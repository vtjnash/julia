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
        linfo::Core.MethodInstance, src::Core.CodeInfo, world::UInt,
        syntax::Symbol, params::CodegenParams=CodegenParams())
    if syntax != :att && syntax != :intel
        throw(ArgumentError("'syntax' must be either :intel or :att"))
    end
    llvmf = ccall(:jl_get_llvmf_defn2, Ptr{Cvoid}, (Any, Any, UInt, Bool, Bool, CodegenParams), linfo, src, world, false, false, params)
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

module IRShowEnhanced
import Base.IRShow: compute_basic_blocks, scan_ssa_use!, block_for_inst,
    print_stmt, should_print_ssa_type, DILineInfoPrinter,
    GotoIfNot, GotoNode, PhiNode
function expr_type_printer(io::IO, @nospecialize(typ), used::Bool)
    print(io, "::", typ)
    nothing
end
function repr_ir(code::Core.CodeInfo)
    buf = IOBuffer()
    io = IOContext(buf, :SOURCE_SLOTNAMES => Base.sourceinfo_slotnames(code))
    used = BitSet()
    stmts = code.code
    types = code.ssavaluetypes
    cfg = compute_basic_blocks(stmts)
    max_bb_idx_size = length(string(length(cfg.blocks)))
    for stmt in stmts
        scan_ssa_use!(push!, used, stmt)
    end
    bb_idx = 1
    if isempty(used)
        maxlength_idx = 0
    else
        maxused = maximum(used)
        maxlength_idx = length(string(maxused))
    end
    emit_lineinfo, line_indent = DILineInfoPrinter(code.linetable)
    nstmts = length(stmts)
    output_starts = zeros(Int, nstmts)
    for idx in 1:nstmts
        output_starts[idx] = position(buf)
        bbrange = cfg.blocks[bb_idx].stmts
        bbrange = bbrange.first:bbrange.last
        # Print line info update
        linestart = idx == first(bbrange) ? "   " : "│  "
        emit_lineinfo(io, linestart, code.codelocs[idx])
        # Compute BB guard rail
        if idx == first(bbrange)
            bb_idx_str = string(bb_idx)
            bb_pad = max_bb_idx_size - length(bb_idx_str)
            bb_type = length(cfg.blocks[bb_idx].preds) <= 1 ? "─" : "┄"
            print(io, bb_idx_str, " ", bb_type, "─"^bb_pad, " ")
        elseif idx == last(bbrange) # print separator
            print(io, "└", "─"^(1 + max_bb_idx_size), " ")
        else
            print(io, "│  ", " "^max_bb_idx_size)
        end
        if idx == last(bbrange)
            bb_idx += 1
        end
        if !isassigned(stmts, idx)
            # This is invalid, but do something useful rather
            # than erroring, to make debugging easier
            print(io, "#UNDEF\n")
            continue
        end
        stmt = stmts[idx]
        # convert statement index to labels, as expected by print_stmt
        if stmt isa Expr
            if stmt.head === :gotoifnot && length(stmt.args) == 2 && stmt.args[2] isa Int
                stmt = GotoIfNot(stmt.args[1], block_for_inst(cfg, stmt.args[2]::Int))
            elseif stmt.head === :enter && length(stmt.args) == 1 && stmt.args[1] isa Int
                stmt = Expr(:enter, block_for_inst(cfg, stmt.args[1]::Int))
            end
        elseif isa(stmt, GotoIfNot)
            stmt = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
        elseif stmt isa GotoNode
            stmt = GotoNode(block_for_inst(cfg, stmt.label))
        elseif stmt isa PhiNode
            e = stmt.edges
            stmt = PhiNode(Any[block_for_inst(cfg, e[i]) for i in 1:length(e)], stmt.values)
        end
        show_type = types isa Vector{Any} && should_print_ssa_type(stmt)
        print_stmt(io, idx, stmt, used, maxlength_idx, true, show_type)
        if types isa Vector{Any} # ignore types for pre-inference code
            if !isassigned(types, idx)
                # This is an error, but can happen if passes don't update their type information
                print(io, "::#UNDEF")
            elseif show_type
                typ = types[idx]
                expr_type_printer(io, typ, idx in used)
            end
        end
        println(io)
    end
    typed = String(take!(buf))
    postprocess_linemap!(typed, output_starts)
    return typed,  output_starts
end
function postprocess_linemap!(text::String, ssamap::Vector{Int})
    # convert position to the corresponding line number for each statement
    io = IOBuffer(text)
    lineno = 1
    lines = replace!(ssamap) do p
        while position(io) < p
            readline(io)
            lineno += 1
        end
        return lineno
    end
    return lines
end
end


"""
    code_analysis(f, types; syntax = :att)

Compute all code transforms, modifying the line numbers at most steps to enable fine-grained tracking of the changes.
"""
function code_analysis(@nospecialize(f), @nospecialize(types=Tuple); syntax::Symbol = :att)
    world = ccall(:jl_get_world_counter, UInt, ())
    linfo = _get_linfo(f, types, world)
    meth = linfo.def::Method
    function rewrite_lineno!(src::Core.CodeInfo, from::Symbol, to::Symbol)
        # rewrites the codelocs / linetable of src so we can track statement movements with good granularity
        # and output a list of the original outer-most line numbers for each statement
        nstmts = length(src.code)
        @assert length(src.codelocs) ==  nstmts
        lines = zeros(Int, nstmts)
        for i = 1:nstmts
            lineidx = src.codelocs[i]
            while lineidx != 0
                info = src.linetable[lineidx]
                if info.inlined_at == 0
                    if info.file == from
                        lines[i] = info.line
                    end
                end
                lineidx = info.inlined_at
            end
        end
        resize!(src.linetable, nstmts)
        for i = 1:nstmts
            src.linetable[i] = Core.LineInfoNode(meth.module, meth.name, to, i, 0)
            src.codelocs[i] = i
        end
        return lines
    end
    params = Core.Compiler.Params(world)
    ccall(:jl_typeinf_begin, Cvoid, ())
    result = Core.Compiler.InferenceResult(linfo)
    frame = Core.Compiler.InferenceState(result, false, params)
    #lowered = IRShowEnhanced.repr_ir(frame.src) # this is effectively the same as typed
    @assert Core.Compiler.typeinf(frame)
    text = get_source_hack(linfo.def::Method)
    typed, typed_map_out = IRShowEnhanced.repr_ir(frame.src) # text, and map from typed:ssa to typed:lineno
    typed_map_in = rewrite_lineno!(frame.src, meth.file, :lowered) # map from typed::ssa to text:lineno
    opt = Core.Compiler.OptimizationState(frame)
    Core.Compiler.optimize(opt, result.result)
    ccall(:jl_typeinf_end, Cvoid, ())
    optim, optim_map_out = IRShowEnhanced.repr_ir(opt.src) # map from optim:ssa to optim:lineno
    optim_map_in = rewrite_lineno!(opt.src, :lowered, :optimized) # map from optim::ssa to typed:ssa
    analysis = _get_linfo_analysis(linfo, opt.src, world, syntax)
    strs = String[text, typed, optim, analysis...]
    rmaps = Vector{Int}[
        re_map(typed_map_in, invert_map(typed_map_out)), # lowered[i] -> text
        re_map(typed_map_out, re_map(optim_map_in, invert_map(optim_map_out))), # optim[i] -> lowered
        Int[], # analysis[1][i] (llvm) -> optim
        Int[], # analysis[2][i] (llvm) -> llvm
        Int[], # analysis[3][i] (mir) -> llvm
        Int[], # analysis[4][i] (asm) -> mir
        Int[], # analysis[5] (remarks)
        ]
    return strs, rmaps
end

function re_map(m::Vector{Int}, is::Vector{Int}) # m[is]
    return map(i -> (i == 0 ? 0 : m[i]), is)
end

function invert_map(m::Vector{Int})
    newmap = zeros(Int, maximum(m))
    for i in 1:length(m)
        newmap[m[i]] = i
    end
    return newmap
end

function get_source_hack(m::Method)
    file = string(m.file)
    repl_history = match(r"^REPL\[(\d+)\]$", file)
    if repl_history === nothing
        text = read(file, String)
    else
        hist = Base.active_repl.interface.modes[1].hist
        hist_idx = hist.start_idx + parse(Int, repl_history[1])
        text = hist.history[hist_idx]
    end
    return text
end

#function filter_source_hack(m::Method, linetable::Vector)
#    file = string(m.file)
#    repl_history = match(r"^REPL\[(\d+)\]$", file)
#    if repl_history === nothing
#        file = Base.find_source_file(file)
#        line1 = m.line
#        line2 = m.line
#        for line in linetable
#            line1 = min(line.line, line1)
#            line2 = max(line.line, line2)
#        end
#        lines = readlines(file)
#        while line1 > 1
#            # the parser gives the wrong answer for the first line number,
#            # rewind to try to guess the right answer by looking for either no indentation or a blank line
#            let line = lines[line1]
#                if isempty(line) || !isspace(line[1])
#                    break
#                end
#            end
#            line1 -= 1
#        end
#        while line1 < length(lines) && all(isspace, lines[line1])
#            # remove any leading empty lines
#            line1 += 1
#        end
#        while line2 < length(lines)
#            # we have no way to ask the parser for the last character, so try to guess it now
#            let line = lines[line2]
#                if isempty(line) || !isspace(line[2])
#                    break
#                end
#            end
#            line2 += 1
#        end
#        while line2 > 1 && all(isspace, lines[line2])
#            # remove any leading empty lines
#            line2 -= 1
#        end
#        lines = join(lines[line1:line2], "\n")
#    else
#        line1 = 1
#        hist = Base.active_repl.interface.modes[1].hist
#        hist_idx = hist.start_idx + parse(Int, repl_history[1])
#        lines = hist.history[hist_idx]
#    end
#    return lines, line1
#end

function collect_analysis(analysis::Vector{String}, rmaps::Vector{Vector{Int}})
    fmts = (:julia, :text, :text, :llvm, :llvm, :yaml, :asm, :yaml)
    @assert length(fmts) == length(analysis)
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
    println(rmaps)
    return render
end
