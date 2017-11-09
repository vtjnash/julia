# This file is a part of Julia. License is MIT: https://julialang.org/license

module Broadcast

using Base.Cartesian
using Base: linearindices, tail, OneTo, to_shape,
            _msk_end, unsafe_bitgetindex, bitcache_chunks, bitcache_size, dumpbitcache,
            nullable_returntype, null_safe_op, hasvalue, isoperator
import Base: broadcast, broadcast!
export broadcast_getindex, broadcast_setindex!, dotview, @__dot__

const ScalarType = Union{Type{Any}, Type{Nullable}}

## Broadcasting utilities ##
# fallbacks for some special cases
@inline broadcast(f, x::Number...) = f(x...)
@inline broadcast(f, t::NTuple{N,Any}, ts::Vararg{NTuple{N,Any}}) where {N} = map(f, t, ts...)
broadcast!(::typeof(identity), x::Array{T,N}, y::Array{S,N}) where {T,S,N} =
    size(x) == size(y) ? copy!(x, y) : broadcast_c!(identity, Array, Array, x, y)

# special cases for "X .= ..." (broadcast!) assignments
broadcast!(::typeof(identity), X::AbstractArray, x::Number) = fill!(X, x)
broadcast!(f, X::AbstractArray, x::Number...) = (@inbounds for I in eachindex(X); X[I] = f(x...); end; X)

# logic for deciding the resulting container type
_containertype(::Type) = Any
_containertype(::Type{<:Ptr}) = Any
_containertype(::Type{<:Tuple}) = Tuple
_containertype(::Type{<:Ref}) = Array
_containertype(::Type{<:AbstractArray}) = Array
_containertype(::Type{<:Nullable}) = Nullable
containertype(x) = _containertype(typeof(x))
containertype(ct1, ct2) = promote_containertype(containertype(ct1), containertype(ct2))
@inline containertype(ct1, ct2, cts...) = promote_containertype(containertype(ct1), containertype(ct2, cts...))

promote_containertype(::Type{Array}, ::Type{Array}) = Array
promote_containertype(::Type{Array}, ct) = Array
promote_containertype(ct, ::Type{Array}) = Array
promote_containertype(::Type{Tuple}, ::ScalarType) = Tuple
promote_containertype(::ScalarType, ::Type{Tuple}) = Tuple
promote_containertype(::Type{Any}, ::Type{Nullable}) = Nullable
promote_containertype(::Type{Nullable}, ::Type{Any}) = Nullable
promote_containertype(::Type{T}, ::Type{T}) where {T} = T

## Calculate the broadcast indices of the arguments, or error if incompatible
# array inputs
broadcast_indices() = ()
broadcast_indices(A) = broadcast_indices(containertype(A), A)
@inline broadcast_indices(A, B...) = broadcast_shape(broadcast_indices(A), broadcast_indices(B...))
broadcast_indices(::ScalarType, A) = ()
broadcast_indices(::Type{Tuple}, A) = (OneTo(length(A)),)
broadcast_indices(::Type{Array}, A::Ref) = ()
broadcast_indices(::Type{Array}, A) = indices(A)

# shape (i.e., tuple-of-indices) inputs
broadcast_shape(shape::Tuple) = shape
@inline broadcast_shape(shape::Tuple, shape1::Tuple, shapes::Tuple...) = broadcast_shape(_bcs(shape, shape1), shapes...)
# _bcs consolidates two shapes into a single output shape
_bcs(::Tuple{}, ::Tuple{}) = ()
@inline _bcs(::Tuple{}, newshape::Tuple) = (newshape[1], _bcs((), tail(newshape))...)
@inline _bcs(shape::Tuple, ::Tuple{}) = (shape[1], _bcs(tail(shape), ())...)
@inline function _bcs(shape::Tuple, newshape::Tuple)
    return (_bcs1(shape[1], newshape[1]), _bcs(tail(shape), tail(newshape))...)
end
# _bcs1 handles the logic for a single dimension
_bcs1(a::Integer, b::Integer) = a == 1 ? b : (b == 1 ? a : (a == b ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size"))))
_bcs1(a::Integer, b) = a == 1 ? b : (first(b) == 1 && last(b) == a ? b : throw(DimensionMismatch("arrays could not be broadcast to a common size")))
_bcs1(a, b::Integer) = _bcs1(b, a)
_bcs1(a, b) = _bcsm(b, a) ? b : (_bcsm(a, b) ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size")))
# _bcsm tests whether the second index is consistent with the first
_bcsm(a, b) = a == b || length(b) == 1
_bcsm(a, b::Number) = b == 1
_bcsm(a::Number, b::Number) = a == b || b == 1

## Check that all arguments are broadcast compatible with shape
# comparing one input against a shape
check_broadcast_shape(shp) = nothing
check_broadcast_shape(shp, ::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, ::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, Ashp::Tuple) = throw(DimensionMismatch("cannot broadcast array to have fewer dimensions"))
function check_broadcast_shape(shp, Ashp::Tuple)
    _bcsm(shp[1], Ashp[1]) || throw(DimensionMismatch("array could not be broadcast to match destination"))
    check_broadcast_shape(tail(shp), tail(Ashp))
end
check_broadcast_indices(shp, A) = check_broadcast_shape(shp, broadcast_indices(A))
# comparing many inputs
@inline function check_broadcast_indices(shp, A, As...)
    check_broadcast_indices(shp, A)
    check_broadcast_indices(shp, As...)
end

## Indexing manipulations

# newindex(I, keep, Idefault) replaces a CartesianIndex `I` with something that
# is appropriate for a particular broadcast array/scalar. `keep` is a
# NTuple{N,Bool}, where keep[d] == true means that one should preserve
# I[d]; if false, replace it with Idefault[d].
@inline newindex(I::CartesianIndex, keep, Idefault) = CartesianIndex(_newindex(I.I, keep, Idefault))
@inline _newindex(I, keep, Idefault) =
    (ifelse(keep[1], I[1], Idefault[1]), _newindex(tail(I), tail(keep), tail(Idefault))...)
@inline _newindex(I, keep::Tuple{}, Idefault) = ()  # truncate if keep is shorter than I

# newindexer(shape, A) generates `keep` and `Idefault` (for use by
# `newindex` above) for a particular array `A`, given the
# broadcast_indices `shape`
# `keep` is equivalent to map(==, indices(A), shape) (but see #17126)
@inline newindexer(shape, A) = shapeindexer(shape, broadcast_indices(A))
@inline shapeindexer(shape, indsA::Tuple{}) = (), ()
@inline function shapeindexer(shape, indsA::Tuple)
    ind1 = indsA[1]
    keep, Idefault = shapeindexer(tail(shape), tail(indsA))
    (shape[1] == ind1, keep...), (first(ind1), Idefault...)
end

# Equivalent to map(x->newindexer(shape, x), As) (but see #17126)
map_newindexer(shape, ::Tuple{}) = (), ()
@inline function map_newindexer(shape, As)
    A1 = As[1]
    keeps, Idefaults = map_newindexer(shape, tail(As))
    keep, Idefault = newindexer(shape, A1)
    (keep, keeps...), (Idefault, Idefaults...)
end
@inline function map_newindexer(shape, A, Bs)
    keeps, Idefaults = map_newindexer(shape, Bs)
    keep, Idefault = newindexer(shape, A)
    (keep, keeps...), (Idefault, Idefaults...)
end

Base.@propagate_inbounds _broadcast_getindex(A, I) = _broadcast_getindex(containertype(A), A, I)
# `(x,)`, where `x` is a scalar, broadcasts the same way as `[x]` or `x`
Base.@propagate_inbounds _broadcast_getindex(::Type{Tuple}, A::Tuple{Any}, I) = A[1]
Base.@propagate_inbounds _broadcast_getindex(::Type{Array}, A::Ref, I) = A[]
Base.@propagate_inbounds _broadcast_getindex(::ScalarType, A, I) = A
Base.@propagate_inbounds _broadcast_getindex(::Any, A, I) = A[I]

## Broadcasting core
# nargs encodes the number of As arguments (which matches the number
# of keeps). The first two type parameters are to ensure specialization.
@generated function _broadcast!(f, B::AbstractArray, keeps::K, Idefaults::ID, A::AT, Bs::BT, ::Val{N}, iter) where {K,ID,AT,BT,N}
    nargs = N + 1
    quote
        $(Expr(:meta, :inline))
        # destructure the keeps and As tuples
        A_1 = A
        @nexprs $N i->(A_{i+1} = Bs[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        @simd for I in iter
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = _broadcast_getindex(A_i, I_i))
            # call the function and store the result
            result = @ncall $nargs f val
            @inbounds B[I] = result
        end
    end
end

# For BitArray outputs, we cache the result in a "small" Vector{Bool},
# and then copy in chunks into the output
@generated function _broadcast!(f, B::BitArray, keeps::K, Idefaults::ID, A::AT, Bs::BT, ::Val{N}, iter) where {K,ID,AT,BT,N}
    nargs = N + 1
    quote
        $(Expr(:meta, :inline))
        # destructure the keeps and As tuples
        A_1 = A
        @nexprs $N i->(A_{i+1} = Bs[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        C = Vector{Bool}(bitcache_size)
        Bc = B.chunks
        ind = 1
        cind = 1
        @simd for I in iter
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = _broadcast_getindex(A_i, I_i))
            # call the function and store the result
            @inbounds C[ind] = @ncall $nargs f val
            ind += 1
            if ind > bitcache_size
                dumpbitcache(Bc, cind, C)
                cind += bitcache_chunks
                ind = 1
            end
        end
        if ind > 1
            @inbounds C[ind:bitcache_size] = false
            dumpbitcache(Bc, cind, C)
        end
    end
end

"""
    broadcast!(f, dest, As...)

Like [`broadcast`](@ref), but store the result of
`broadcast(f, As...)` in the `dest` array.
Note that `dest` is only used to store the result, and does not supply
arguments to `f` unless it is also listed in the `As`,
as in `broadcast!(f, A, A, B)` to perform `A[:] = broadcast(f, A, B)`.
"""
@inline broadcast!(f, C::AbstractArray, A, Bs::Vararg{Any,N}) where {N} =
    broadcast_c!(f, containertype(C), containertype(A, Bs...), C, A, Bs...)
@inline function broadcast_c!(f, ::Type, ::Type, C, A, Bs::Vararg{Any,N}) where N
    shape = indices(C)
    @boundscheck check_broadcast_indices(shape, A, Bs...)
    keeps, Idefaults = map_newindexer(shape, A, Bs)
    iter = CartesianRange(shape)
    _broadcast!(f, C, keeps, Idefaults, A, Bs, Val(N), iter)
    return C
end

# broadcast with computed element type
@generated function _broadcast!(f, B::AbstractArray, keeps::K, Idefaults::ID, As::AT, ::Val{nargs}, iter, st, count) where {K,ID,AT,nargs}
    quote
        $(Expr(:meta, :noinline))
        # destructure the keeps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        while !done(iter, st)
            I, st = next(iter, st)
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = _broadcast_getindex(A_i, I_i))
            # call the function
            V = @ncall $nargs f val
            S = typeof(V)
            # store the result
            if S <: eltype(B)
                @inbounds B[I] = V
            else
                R = typejoin(eltype(B), S)
                new = similar(B, R)
                for II in Iterators.take(iter, count)
                    new[II] = B[II]
                end
                new[I] = V
                return _broadcast!(f, new, keeps, Idefaults, As, Val(nargs), iter, st, count+1)
            end
            count += 1
        end
        return B
    end
end

# broadcast methods that dispatch on the type found by inference
function broadcast_t(f, ::Type{Any}, shape, iter, As...)
    nargs = length(As)
    keeps, Idefaults = map_newindexer(shape, As)
    st = start(iter)
    I, st = next(iter, st)
    val = f([ _broadcast_getindex(As[i], newindex(I, keeps[i], Idefaults[i])) for i=1:nargs ]...)
    if val isa Bool
        B = similar(BitArray, shape)
    else
        B = similar(Array{typeof(val)}, shape)
    end
    B[I] = val
    return _broadcast!(f, B, keeps, Idefaults, As, Val(nargs), iter, st, 1)
end
@inline function broadcast_t(f, T, shape, iter, A, Bs::Vararg{Any,N}) where N
    C = similar(Array{T}, shape)
    keeps, Idefaults = map_newindexer(shape, A, Bs)
    _broadcast!(f, C, keeps, Idefaults, A, Bs, Val(N), iter)
    return C
end

# default to BitArray for broadcast operations producing Bool, to save 8x space
# in the common case where this is used for logical array indexing; in
# performance-critical cases where Array{Bool} is desired, one can always
# use broadcast! instead.
@inline function broadcast_t(f, ::Type{Bool}, shape, iter, A, Bs::Vararg{Any,N}) where N
    C = similar(BitArray, shape)
    keeps, Idefaults = map_newindexer(shape, A, Bs)
    _broadcast!(f, C, keeps, Idefaults, A, Bs, Val(N), iter)
    return C
end

maptoTuple(f) = Tuple{}
maptoTuple(f, a, b...) = Tuple{f(a), maptoTuple(f, b...).types...}

# An element type satisfying for all A:
# broadcast_getindex(
#     containertype(A),
#     A, broadcast_indices(A)
# )::_broadcast_getindex_eltype(A)
_broadcast_getindex_eltype(A) = _broadcast_getindex_eltype(containertype(A), A)
_broadcast_getindex_eltype(::ScalarType, T::Type) = Type{T}
_broadcast_getindex_eltype(::ScalarType, A) = typeof(A)
_broadcast_getindex_eltype(::Any, A) = eltype(A)  # Tuple, Array, etc.

# An element type satisfying for all A:
# unsafe_get(A)::unsafe_get_eltype(A)
_unsafe_get_eltype(x::Nullable) = eltype(x)
_unsafe_get_eltype(T::Type) = Type{T}
_unsafe_get_eltype(x) = typeof(x)

# Inferred eltype of result of broadcast(f, xs...)
_broadcast_eltype(f, A, As...) =
    Base._return_type(f, maptoTuple(_broadcast_getindex_eltype, A, As...))
_nullable_eltype(f, A, As...) =
    Base._return_type(f, maptoTuple(_unsafe_get_eltype, A, As...))

# broadcast methods that dispatch on the type of the final container
@inline function broadcast_c(f, ::Type{Array}, A, Bs...)
    T = _broadcast_eltype(f, A, Bs...)
    shape = broadcast_indices(A, Bs...)
    iter = CartesianRange(shape)
    if Base._isleaftype(T)
        return broadcast_t(f, T, shape, iter, A, Bs...)
    end
    if isempty(iter)
        return similar(Array{T}, shape)
    end
    return broadcast_t(f, Any, shape, iter, A, Bs...)
end
@inline function broadcast_c(f, ::Type{Nullable}, a...)
    nonnull = all(hasvalue, a)
    S = _nullable_eltype(f, a...)
    if Base._isleaftype(S) && null_safe_op(f, maptoTuple(_unsafe_get_eltype,
                                                         a...).types...)
        Nullable{S}(f(map(unsafe_get, a)...), nonnull)
    else
        if nonnull
            Nullable(f(map(unsafe_get, a)...))
        else
            Nullable{nullable_returntype(S)}()
        end
    end
end
@inline broadcast_c(f, ::Type{Any}, a...) = f(a...)
@inline broadcast_c(f, ::Type{Tuple}, A, Bs...) =
    tuplebroadcast(f, tuplebroadcast_maxtuple(A, Bs...), A, Bs...)
@inline tuplebroadcast(f, ::NTuple{N,Any}, As...) where {N} =
    ntuple(k -> f(tuplebroadcast_getargs(As, k)...), Val(N))
@inline tuplebroadcast(f, ::NTuple{N,Any}, ::Type{T}, As...) where {N,T} =
    ntuple(k -> f(T, tuplebroadcast_getargs(As, k)...), Val(N))
# When the result of broadcast is a tuple it can only come from mixing n-tuples
# of the same length with scalars and 1-tuples. So, in order to have a
# type-stable broadcast, we need to find a tuple of maximum length (except when
# there are only scalars, empty tuples and 1-tuples, in which case the
# returned value will be an empty tuple).
# The following methods compare broadcast arguments pairwise to determine the
# length of the final tuple.
tuplebroadcast_maxtuple(A, B) =
    _tuplebroadcast_maxtuple(containertype(A), containertype(B), A, B)
@inline tuplebroadcast_maxtuple(A, Bs...) =
    tuplebroadcast_maxtuple(A, tuplebroadcast_maxtuple(Bs...))
tuplebroadcast_maxtuple(A::NTuple{N,Any}, ::NTuple{N,Any}...) where {N} = A
# Here we use the containertype trait to easier disambiguate between methods
_tuplebroadcast_maxtuple(::Type{Any}, ::Type{Any}, A, B) = (nothing,)
_tuplebroadcast_maxtuple(::Type{Tuple}, ::Type{Any}, A, B) = A
_tuplebroadcast_maxtuple(::Type{Any}, ::Type{Tuple}, A, B) = B
_tuplebroadcast_maxtuple(::Type{Tuple}, ::Type{Tuple}, A, B::Tuple{Any}) = A
_tuplebroadcast_maxtuple(::Type{Tuple}, ::Type{Tuple}, A::Tuple{Any}, B) = B
_tuplebroadcast_maxtuple(::Type{Tuple}, ::Type{Tuple}, A::Tuple{Any}, ::Tuple{Any}) = A
_tuplebroadcast_maxtuple(::Type{Tuple}, ::Type{Tuple}, A, B) =
    throw(DimensionMismatch("tuples could not be broadcast to a common size"))
tuplebroadcast_getargs(::Tuple{}, k) = ()
@inline tuplebroadcast_getargs(As, k) =
    (_broadcast_getindex(first(As), k), tuplebroadcast_getargs(tail(As), k)...)

"""
    broadcast(f, As...)

Broadcasts the arrays, tuples, `Ref`s, nullables, and/or scalars `As` to a
container of the appropriate type and dimensions. In this context, anything
that is not a subtype of `AbstractArray`, `Ref` (except for `Ptr`s), `Tuple`,
or `Nullable` is considered a scalar. The resulting container is established by
the following rules:

 - If all the arguments are scalars, it returns a scalar.
 - If the arguments are tuples and zero or more scalars, it returns a tuple.
 - If the arguments contain at least one array or `Ref`, it returns an array
   (expanding singleton dimensions), and treats `Ref`s as 0-dimensional arrays,
   and tuples as 1-dimensional arrays.

The following additional rule applies to `Nullable` arguments: If there is at
least one `Nullable`, and all the arguments are scalars or `Nullable`, it
returns a `Nullable` treating `Nullable`s as "containers".

A special syntax exists for broadcasting: `f.(args...)` is equivalent to
`broadcast(f, args...)`, and nested `f.(g.(args...))` calls are fused into a
single broadcast loop.

# Examples
```jldoctest
julia> A = [1, 2, 3, 4, 5]
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
5×2 Array{Int64,2}:
 1   2
 3   4
 5   6
 7   8
 9  10

julia> broadcast(+, A, B)
5×2 Array{Int64,2}:
  2   3
  5   6
  8   9
 11  12
 14  15

julia> parse.(Int, ["1", "2"])
2-element Array{Int64,1}:
 1
 2

julia> abs.((1, -2))
(1, 2)

julia> broadcast(+, 1.0, (0, -2.0))
(1.0, -1.0)

julia> broadcast(+, 1.0, (0, -2.0), Ref(1))
2-element Array{Float64,1}:
 2.0
 0.0

julia> (+).([[0,2], [1,3]], Ref{Vector{Int}}([1,-1]))
2-element Array{Array{Int64,1},1}:
 [1, 1]
 [2, 2]

julia> string.(("one","two","three","four"), ": ", 1:4)
4-element Array{String,1}:
 "one: 1"
 "two: 2"
 "three: 3"
 "four: 4"

julia> Nullable("X") .* "Y"
Nullable{String}("XY")

julia> broadcast(/, 1.0, Nullable(2.0))
Nullable{Float64}(0.5)

julia> (1 + im) ./ Nullable{Int}()
Nullable{Complex{Float64}}()
```
"""
@inline broadcast(f, A, Bs...) = broadcast_c(f, containertype(A, Bs...), A, Bs...)

"""
    broadcast_getindex(A, inds...)

Broadcasts the `inds` arrays to a common size like [`broadcast`](@ref)
and returns an array of the results `A[ks...]`,
where `ks` goes over the positions in the broadcast result `A`.

# Examples
```jldoctest
julia> A = [1, 2, 3, 4, 5]
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
5×2 Array{Int64,2}:
 1   2
 3   4
 5   6
 7   8
 9  10

julia> C = broadcast(+,A,B)
5×2 Array{Int64,2}:
  2   3
  5   6
  8   9
 11  12
 14  15

julia> broadcast_getindex(C,[1,2,10])
3-element Array{Int64,1}:
  2
  5
 15
```
"""
broadcast_getindex(src::AbstractArray, I::AbstractArray...) = broadcast_getindex!(similar(Array{eltype(src)}, broadcast_indices(I...)), src, I...)
@generated function broadcast_getindex!(dest::AbstractArray, src::AbstractArray, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        check_broadcast_indices(indices(dest), $(Isplat...))  # unnecessary if this function is never called directly
        checkbounds(src, $(Isplat...))
        @nexprs $N d->(@nexprs $N k->(Ibcast_d_k = indices(I_k, d) == OneTo(1)))
        @nloops $N i dest d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
            @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
            @inbounds (@nref $N dest i) = (@nref $N src J)
        end
        dest
    end
end

"""
    broadcast_setindex!(A, X, inds...)

Broadcasts the `X` and `inds` arrays to a common size and stores the value from each
position in `X` at the indices in `A` given by the same positions in `inds`.
"""
@generated function broadcast_setindex!(A::AbstractArray, x, I::AbstractArray...)
    N = length(I)
    Isplat = Expr[:(I[$d]) for d = 1:N]
    quote
        @nexprs $N d->(I_d = I[d])
        checkbounds(A, $(Isplat...))
        shape = broadcast_indices($(Isplat...))
        @nextract $N shape d->(length(shape) < d ? OneTo(1) : shape[d])
        @nexprs $N d->(@nexprs $N k->(Ibcast_d_k = indices(I_k, d) == 1:1))
        if !isa(x, AbstractArray)
            xA = convert(eltype(A), x)
            @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
                @nexprs $N k->(@inbounds J_k = @nref $N I_k d->j_d_k)
                @inbounds (@nref $N A J) = xA
            end
        else
            X = x
            @nexprs $N d->(shapelen_d = length(shape_d))
            @ncall $N Base.setindex_shape_check X shapelen
            Xstate = start(X)
            @inbounds @nloops $N i d->shape_d d->(@nexprs $N k->(j_d_k = Ibcast_d_k ? 1 : i_d)) begin
                @nexprs $N k->(J_k = @nref $N I_k d->j_d_k)
                x_el, Xstate = next(X, Xstate)
                (@nref $N A J) = x_el
            end
        end
        A
    end
end

############################################################

# x[...] .= f.(y...) ---> broadcast!(f, dotview(x, ...), y...).
# The dotview function defaults to getindex, but we override it in
# a few cases to get the expected in-place behavior without affecting
# explicit calls to view.   (All of this can go away if slices
# are changed to generate views by default.)

Base.@propagate_inbounds dotview(args...) = Base.maybeview(args...)

############################################################
# The parser turns @. into a call to the __dot__ macro,
# which converts all function calls and assignments into
# broadcasting "dot" calls/assignments:

dottable(x) = false # avoid dotting spliced objects (e.g. view calls inserted by @view)
dottable(x::Symbol) = !isoperator(x) || first(string(x)) != '.' || x == :.. # don't add dots to dot operators
dottable(x::Expr) = x.head != :$
undot(x) = x
function undot(x::Expr)
    if x.head == :.=
        Expr(:(=), x.args...)
    elseif x.head == :block # occurs in for x=..., y=...
        Expr(:block, map(undot, x.args)...)
    else
        x
    end
end
__dot__(x) = x
function __dot__(x::Expr)
    dotargs = map(__dot__, x.args)
    if x.head == :call && dottable(x.args[1])
        Expr(:., dotargs[1], Expr(:tuple, dotargs[2:end]...))
    elseif x.head == :$
        x.args[1]
    elseif x.head == :let # don't add dots to `let x=...` assignments
        Expr(:let, undot(dotargs[1]), dotargs[2])
    elseif x.head == :for # don't add dots to for x=... assignments
        Expr(:for, undot(dotargs[1]), dotargs[2])
    elseif (x.head == :(=) || x.head == :function || x.head == :macro) &&
           Meta.isexpr(x.args[1], :call) # function or macro definition
        Expr(x.head, x.args[1], dotargs[2])
    else
        head = string(x.head)
        if last(head) == '=' && first(head) != '.'
            Expr(Symbol('.',head), dotargs...)
        else
            Expr(x.head, dotargs...)
        end
    end
end
"""
    @. expr

Convert every function call or operator in `expr` into a "dot call"
(e.g. convert `f(x)` to `f.(x)`), and convert every assignment in `expr`
to a "dot assignment" (e.g. convert `+=` to `.+=`).

If you want to *avoid* adding dots for selected function calls in
`expr`, splice those function calls in with `\$`.  For example,
`@. sqrt(abs(\$sort(x)))` is equivalent to `sqrt.(abs.(sort(x)))`
(no dot for `sort`).

(`@.` is equivalent to a call to `@__dot__`.)

# Examples
```jldoctest
julia> x = 1.0:3.0; y = similar(x);

julia> @. y = x + 3 * sin(x)
3-element Array{Float64,1}:
 3.5244129544236893
 4.727892280477045
 3.4233600241796016
```
"""
macro __dot__(x)
    esc(__dot__(x))
end


############################################################

struct TypeTuple{T, Rest}
    head::T    # car
    rest::Rest # cdr
    TypeTuple(x, rest::TypeTuple) where {} = new{Core.Typeof(x), typeof(rest)}(x, rest) # (cons x rest)
    TypeTuple(x, rest::Void) where {} = new{Core.Typeof(x), typeof(rest)}(x, rest) # (cons x nil)
    TypeTuple(x) where {} = new{Core.Typeof(x), Void}(x, nothing) # (list x)
end
# (apply list a)
make_typetuple(a) = TypeTuple(a)
make_typetuple(a, args...) = TypeTuple(a, make_typetuple(args...))
# (map f tt)
Base.map(f, tt::TypeTuple{<:Any, Void}) = (f(tt.head),)
function Base.map(f, tt::TypeTuple)
    return (f(tt.head), map(f, tt.rest)...)
end
Base.all(f, tt::TypeTuple{<:Any, Void}) = f(tt.head)
Base.all(f, tt::TypeTuple) = f(tt.head) && all(f, tt.rest)

mapTypeTuple(f, tt::TypeTuple{<:Any, Void}) = TypeTuple(f(tt.head),)
function mapTypeTuple(f, tt::TypeTuple)
    return TypeTuple(f(tt.head), mapTypeTuple(f, tt.rest))
end
# Base.length(tt::TypeTuple) = length(map(i -> nothing, tt))

typetuple_broadcast_indices(tt::TypeTuple{<:Any, Void}) = broadcast_indices(tt.head)
typetuple_broadcast_indices(tt::TypeTuple) = broadcast_shape(broadcast_indices(tt.head), typetuple_broadcast_indices(tt.rest))

struct Broadcasted{F, A<:TypeTuple, S}
    f::F
    args::A
    shape::S
    Broadcasted{F, A, S}(f::F, args::A, shape::S) where {F, A<:TypeTuple, S} =
        new{F, A, S}(f, args, shape)
end
Broadcasted(f) = inert(f()) # odd, perhaps, but this is how `broadcast` is defined
function Broadcasted(f, args::TypeTuple)
    shape = typetuple_broadcast_indices(args)
    BC = Broadcasted{typeof(f), typeof(args), typeof(shape)}
    return BC(f, args, shape)
end
function Base.show(io::IO, bc::Broadcasted)
    print(io, "Broadcasted(")
    print(io, bc.f)
    args = bc.args
    while args != nothing
        print(io, ", ")
        print(io, args.head)
        args = args.rest
    end
    print(io, ")")
end

_broadcast_getindex_eltype(::ScalarType, bc::Broadcasted) = lazy_broadcast_eltype(bc)
_broadcast_getindex_eltype(::Any, bc::Broadcasted) = lazy_broadcast_eltype(bc)
function lazy_broadcast_eltype(bc::Broadcasted)
    return Base._return_type(bc.f, Tuple{map(_broadcast_getindex_eltype, bc.args)...})
end
_containertype(bc::Type{<:Broadcasted}) = Any
_containertype(bc::Type{<:Broadcasted{F, A} where F}) where {A <: TypeTuple} = lazy_containertype(A)
lazy_containertype(::Type{TypeTuple{A, R}}) where {A, R} = promote_containertype(_containertype(A), lazy_containertype(R))
lazy_containertype(::Type{TypeTuple{A, Void}}) where {A} = _containertype(A)
Base.length(bc::Broadcasted) = length(bc.args.head)
Base.indices(bc::Broadcasted) = bc.shape
Base.@propagate_inbounds function _broadcast_getindex(bc::Broadcasted, I)
    @inline function index_into(a)
        keep, Idefault = newindexer(bc.shape, a)
        i = newindex(I, keep, Idefault)
        return _broadcast_getindex(a, i)
    end
    args = mapTypeTuple(index_into, bc.args)
    return apply_typetuple(bc.f, args)
end

# quasi-support the broken Nullable hack above
_unsafe_get_eltype(x::Broadcasted) = lazy_nullable_eltype(x)
function lazy_nullable_eltype(bc::Broadcasted)
    return Base._return_type(bc.f, Tuple{map(_unsafe_get_eltype, bc.args)...})
end
Base.hasvalue(bc::Broadcasted) = all(hasvalue, bc.args)
Base.@propagate_inbounds function Base.unsafe_get(bc::Broadcasted)
    args = mapTypeTuple(unsafe_get, bc.args)
    return apply_typetuple(bc.f, args)
end


isfused(arg) = true
inert(x) = Ref{typeof(x)}(x)
function make_kwsyntax(f, args...; kwargs...)
    if isempty(args) || !all(isfused, args)
        if isempty(kwargs)
            return inert(broadcast(f, args...))
        else
            return inert(broadcast((as...) -> f(as...; kwargs...), args...))
        end
    else
        args′ = make_typetuple(args...)
        parevalf, passedargstup = capturescalars(f, kwargs, args′)
        if passedargstup === nothing
            return inert(f(args...; kwargs...)) # nothing to broadcast
        else
            return Broadcasted(parevalf, passedargstup)
        end
    end
end
function make(f, args...)
    # optimization when there are syntactically no keywords
    if isempty(args) || !all(isfused, args)
        return inert(broadcast(f, args...))
    else
        args′ = make_typetuple(args...)
        parevalf, passedargstup = capturescalars(f, (), args′)
        if passedargstup === nothing
            return inert(f(args...)) # nothing to broadcast
        else
            return Broadcasted(parevalf, passedargstup)
        end
    end
end

apply_typetuple(f, tt::Void) = f()
apply_typetuple(f, tt::TypeTuple{<:Any, Void}) = f(tt.head)
apply_typetuple(f, tt::TypeTuple{T, TypeTuple{S, Void}} where {T, S}) = f(tt.head, tt.rest.head)
@generated function apply_typetuple(f, tt::TypeTuple)
    # implements f(map(identity, tt)...)
    N = 0
    let tt = tt
        while tt !== Void
            N += 1
            tt = tt.parameters[2]
        end
    end
    return quote
        tt_1 = tt
        @nexprs $N i->(tt_{i+1} = tt_i.rest)
        @nexprs $N i->(a_i = tt_i.head)
        @ncall $N f a
    end
end

# capturescalars takes a function (f) and a tuple of mixed non-scalars and
# broadcast scalar arguments (mixedargs), and returns a function (parevalf, i.e. partially
# evaluated f) and a reduced argument tuple (passedargstup) containing all non-scalars
# vectors/matrices in mixedargs in their orginal order, and such that the result of
# broadcast(parevalf, passedargstup...) is broadcast(f, mixedargs...)
@inline function capturescalars(f, kwargs, mixedargs::TypeTuple)
    let (passedsrcargstup, makeargs) = _capturescalars(mixedargs)
        let f = TypeTuple(f) # capture Typeof(f) in the closure
            if kwargs === ()
                parevalf = (passed...) -> apply_typetuple(f.head, makeargs(passed...))
            else
                parevalf = (passed...) -> apply_typetuple((args...) -> f.head(args...; kwargs...), makeargs(passed...))
            end
            return (parevalf, passedsrcargstup)
        end
    end
end

isscalararg(::Number) = true
isscalararg(::Any) = false

@inline function _capturescalars(::Void)
    return nothing, () -> nothing
end
@inline function _capturescalars(args::TypeTuple)
    let (rest, f) = _capturescalars(args.rest)
        let arg = args.head
            if isscalararg(arg)
                return rest, (tail...) -> TypeTuple(arg, f(tail...)) # add back scalararg after (in makeargs)
            else
                return TypeTuple(arg, rest), (arg, tail...) -> TypeTuple(arg, f(tail...)) # pass-through to broadcast
            end
        end
    end
end
@inline function _capturescalars(args::TypeTuple{<:Any, Void})  # this definition is just an optimization (to bottom out the recursion slightly sooner)
    let arg = args.head
        if isscalararg(arg)
            return nothing, () -> TypeTuple(arg,) # add scalararg
        else
            return TypeTuple(arg,), (head,) -> TypeTuple(head,) # pass-through
        end
    end
end


execute(bc::Ref) = bc[]
execute(bc::Broadcasted) = apply_typetuple((args...) -> broadcast(bc.f, args...), bc.args)

execute!(out, bc::Ref) = broadcast!(identity, out, bc[])
execute!(out, bc::Broadcasted) = apply_typetuple((args...) -> broadcast!(bc.f, out, args...), bc.args)


#isfused(arg::CustomArray) = false
make(f::typeof(+), arg::AbstractRange, inc::Number) = inert(broadcast(+, arg, inc))
make(f::typeof(+), arg::Number, inc::AbstractRange) = inert(broadcast(+, inc, arg))

end # module
