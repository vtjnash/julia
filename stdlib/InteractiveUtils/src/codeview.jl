# This file is a part of Julia. License is MIT: https://julialang.org/license

# displaying type warnings

function warntype_type_printer(io::IO, @nospecialize(ty), used::Bool)
    used || return
    if ty isa Type && (!Base.isdispatchelem(ty) || ty == Core.Box)
        if ty isa Union && Base.is_expected_union(ty)
            Base.emphasize(io, "::$ty", Base.warn_color()) # more mild user notification
        else
            Base.emphasize(io, "::$ty")
        end
    else
        Base.printstyled(io, "::$ty", color=:cyan) # show the "good" type
    end
    nothing
end

"""
    code_warntype([io::IO], f, types)

Prints lowered and type-inferred ASTs for the methods matching the given generic function
and type signature to `io` which defaults to `stdout`. The ASTs are annotated in such a way
as to cause "non-leaf" types to be emphasized (if color is available, displayed in red).
This serves as a warning of potential type instability. Not all non-leaf types are particularly
problematic for performance, so the results need to be used judiciously.
In particular, unions containing either [`missing`](@ref) or [`nothing`](@ref) are displayed in yellow, since
these are often intentional.
See [`@code_warntype`](@ref man-code-warntype) for more information.
"""
function code_warntype(io::IO, @nospecialize(f), @nospecialize(t))
    for (src, rettype) in code_typed(f, t)
        lambda_io::IOContext = io
        if src.slotnames !== nothing
            lambda_io = IOContext(lambda_io, :SOURCE_SLOTNAMES => Base.sourceinfo_slotnames(src))
        end
        print(io, "Body")
        warntype_type_printer(io, rettype, true)
        println(io)
        # TODO: static parameter values
        Base.IRShow.show_ir(lambda_io, src, warntype_type_printer)
    end
    nothing
end
code_warntype(@nospecialize(f), @nospecialize(t); kwargs...) =
    code_warntype(stdout, f, t; kwargs...)

import Base.CodegenParams

# Printing code representations in IR and assembly
function _get_linfo(@nospecialize(f), @nospecialize(t), world::UInt)
    ccall(:jl_is_in_pure_context, Bool, ()) && error("code reflection cannot be used from generated functions")
    if isa(f, Core.Builtin)
        throw(ArgumentError("argument is not a generic function"))
    end
    # get the MethodInstance for the method match
    meth = which(f, t)
    t = to_tuple_type(t)
    tt = signature_type(f, t)
    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), tt, meth.sig)::Core.SimpleVector
    meth = Base.func_for_method_checked(meth, ti)
    linfo = ccall(:jl_specializations_get_linfo, Ref{Core.MethodInstance}, (Any, Any, Any, UInt), meth, ti, env, world)
    return linfo
end

function _get_linfo_native(linfo::Core.MethodInstance, world::UInt, wrapper::Bool, syntax::Symbol)
    if syntax != :att && syntax != :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    str = ccall(:jl_dump_method_asm, Any, (Any, UInt, Cint, Bool, Ptr{UInt8}),
        linfo, world, 0, wrapper, syntax)::String
    return str
end

function _get_linfo_llvm(
        linfo::Core.MethodInstance, world::UInt, wrapper::Bool,
        strip_ir_metadata::Bool, dump_module::Bool,
        optimize::Bool=true, params::CodegenParams=CodegenParams())
    llvmf = ccall(:jl_get_llvmf_defn, Ptr{Cvoid}, (Any, UInt, Bool, Bool, CodegenParams), linfo, world, wrapper, optimize, params)
    llvmf == C_NULL && error("could not compile the specified method")
    str = ccall(:jl_dump_llvm_ir, Any, (Ptr{Cvoid}, Bool, Bool),
        llvmf, strip_ir_metadata, dump_module)::String
    return str
end

function _get_linfo_analysis(
        linfo::Core.MethodInstance, world::UInt,
        syntax::Symbol, params::CodegenParams=CodegenParams())
    if syntax != :att && syntax != :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    llvmf = ccall(:jl_get_llvmf_defn, Ptr{Cvoid}, (Any, UInt, Bool, Bool, CodegenParams), linfo, world, false, false, params)
    llvmf == C_NULL && error("could not compile the specified method")
    analysis = ccall(:jl_dump_llvm_asm, Any, (Ptr{Cvoid}, Ptr{UInt8}, Cint),
        llvmf, syntax, -1)::Core.SimpleVector
    return analysis
end


"""
    code_llvm([io=stdout,], f, types)

Prints the LLVM bitcodes generated for running the method matching the given generic
function and type signature to `io`.

If the `optimize` keyword is unset, the code will be shown before LLVM optimizations.
All metadata and dbg.* calls are removed from the printed bitcode. Set the `raw` keyword for the full IR to true.
To dump the entire module that encapsulates the function, with debug info and metadata, set the `dump_module` keyword to true.
"""
code_llvm(io::IO, @nospecialize(f), @nospecialize(types=Tuple), raw=false, dump_module=false, optimize=true) =
    print(io, _get_linfo_llvm(_get_linfo(f, types, typemax(UInt)), typemax(UInt), false, raw, dump_module, optimize))
code_llvm(@nospecialize(f), @nospecialize(types=Tuple); raw=false, dump_module=false, optimize=true) =
    code_llvm(stdout, f, types, raw, dump_module, optimize)

"""
    code_native([io=stdout,], f, types; syntax = :att)

Prints the native assembly instructions generated for running the method matching the given
generic function and type signature to `io`.
Switch assembly syntax using `syntax` symbol parameter set to `:att` for AT&T syntax or `:intel` for Intel syntax.
"""
code_native(io::IO, @nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol = :att) =
    print(io, _get_linfo_native(_get_linfo(f, types, typemax(UInt)), typemax(UInt), false, syntax))
code_native(@nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol = :att) =
    code_native(stdout, f, types, syntax = syntax)

"""
    code_analysis(f, types; syntax = :att)

Compute all code transforms, modifying the line numbers at most steps to enable fine-grained tracking of the changes.
"""
function code_analysis(@nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol = :att)
    repr(src) = sprint(Base.IRShow.show_ir, src, context = :SOURCE_SLOTNAMES => Base.sourceinfo_slotnames(src))
    world = ccall(:jl_get_world_counter, UInt, ())
    linfo = _get_linfo(f, types, world)
    meth = linfo.def::Method
    params = Core.Compiler.Params(world)
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = Core.Compiler.InferenceResult(linfo)
    frame = Core.Compiler.InferenceState(result, false, params)
    #lowered = repr(frame.src) # this is effectively the same as typed
    @assert Core.Compiler.typeinf(frame)
    typed = repr(frame.src)
    # now rewrite the codelocs / linetable of frame.src so we can track their movements through the optimizer
    let src = frame.src
        nstmts = length(src.code)
        resize!(src.codelocs, nstmts)
        resize!(src.linetable, nstmts)
        for i = 1:nstmts
            src.linetable[i] = Core.LineInfoNode(meth.module, meth.name, :lowered, i, 0)
            src.codelocs[i] = i
        end
    end
    opt = Core.Compiler.OptimizationState(frame)
    Core.Compiler.optimize(opt, result.result)
    ccall(:jl_typeinf_end, Cvoid, ())
    analysis = _get_linfo_analysis(linfo, world, syntax)
    optim = repr(opt.src)
    strs = String[typed, optim, analysis...]
    return strs
end

function collect_analysis(analysis::Vector{String})
    fmts = (:text, :text, :llvm, :llvm, :yaml, :asm, :text)
    lnos = map(text -> countlines(IOBuffer(text)), analysis)
    render = map(analysis, fmts) do text, fmt
        p = open(`pygmentize -l $fmt -f html -O linenos=table`, "r+")
        @async (write(p, text); close(p.in))
        return read(p, String)
    end
    for x in render
       print("""<div class="source">""", x, "</div>")
    end
    println()
    println(lnos)
    return render
end
