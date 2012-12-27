## dealing with structs types ##

# Struct pointer to integer (and pointer)
convert{T<:Ptr}(::Type{Uint}, s::Struct{T}) = box(Uint, unbox(Struct{T},s))
convert{T<:Ptr,S<:Ptr}(::Type{S}, s::Struct{T}) = box(S, unbox(Struct{T},s))
convert{T<:Integer,R<:Ptr}(::Type{T}, s::Struct{R}) = convert(T,unsigned(s))

# integer to struct pointer
convert{T<:Ptr}(::Type{Struct{T}}, x::Integer) = box(Struct{T},unbox(Uint,uint(x)))
convert{T<:Ptr}(::Type{Struct{T}}, p::Ptr) = box(Struct{T}, unbox(Ptr,p))

# struct pointer to struct pointer
convert{T<:Ptr}(::Type{Struct{T}}, s::Struct{T}) = s
convert{T<:Ptr,R<:Ptr}(::Type{Struct{T}}, s::Struct{R}) = box(Struct{T}, unbox(Struct{R},s))

# object to pointer
convert{T<:Ptr}(::Type{Struct{T}},x::Array) = error("cannot convert "*string(typeof(x))*" to a struct pointer")
convert{T<:Ptr}(::Type{Struct{T}}, x) =
    (isa(typeof(x),CompositeKind) ?
    box(Struct{T}, unbox(Ptr{Any},x)) + 8 :
    error("only CompositeKind's can be converted to Ptr{Struct} types"))

struct{T}(::Type{T}, x::Uint) = convert(Struct{T}, x)
struct{T}(x::T) = convert(Struct{Ptr{T}}, x)

integer{T<:Ptr}(x::Struct{T}) = convert(Uint, x)
unsigned{T<:Ptr}(x::Struct{T}) = convert(Uint, x)

#sizeof{T}(::Type{Struct{T}}) = 0
#sizeof{T}(::Type{Struct{Ptr{T}}}) = 0 
eltype{T}(::Struct{Ptr{T}}) = Struct{T}

## limited struct pointer arithmetic & comparison ##

isequal{T<:Ptr,S<:Ptr}(x::Struct{T}, y::Struct{S}) = uint(x) == uint(y)
-{T<:Ptr,S<:Ptr}(x::Struct{T}, y::Struct{S}) = uint(x) - uint(y)

+{T<:Ptr}(x::Struct{T}, y::Integer) = struct(T, uint(uint(x) + y))
-{T<:Ptr}(x::Struct{T}, y::Integer) = struct(T, uint(uint(x) - y))
+{T<:Ptr}(x::Integer, y::Struct{T}) = y + x


# use the code in ccall.cpp to pass x by-value as a struct
cconvert{T}(::Type{Struct{T}}, x::T) = x
ptr_arg_convert{T<:Ptr}(::Type{Struct{T}}, x) = convert(Struct{T}, x)
