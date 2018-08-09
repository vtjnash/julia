module ProfileEnhanced
Base.position(io::Base.AbstractPipe) = position(Base.pipe_reader(io))
mutable struct Node
    start::Int
    endof::Int
    level::Vector
    idx::Int
    classes::String
end
const DELETED = Node(0, -1, Any[], 0, "")
function startNode(shadowdom::Vector{Node}, io::IO, level::Vector, idx::Int, classes::String="")
    id = length(shadowdom) + 1
    n = Node(position(io), -1, level, idx, classes)
    push!(shadowdom, n)
    return id
end
function endNode(shadowdom::Vector{Node}, io::IO, id::Int)
    endof = position(io)
    for i in id:length(shadowdom)
        n = shadowdom[i]
        n.endof == -1 || return
        n.endof = endof
    end
end
function escapehtml(io::IO, bytes::AbstractVector{UInt8}, attribute::Bool=true)
    for b in bytes
        if b < 0x20 && (b != 0x09 && b != 0x0a && b != 0x0d) # allow only \t \r \n, otherwise, mangle the control character
            write(io, "&#$b;")
        elseif b == UInt8('&')
            write(io, "&amp;")
        elseif b == UInt8('<')
            write(io, "&lt;")
        elseif b == UInt8('>')
            write(io, "&gt;")
        elseif attribute && b == UInt8('\'')
            write(io, "&apos;") # for html4, use #39
        elseif attribute && b == UInt8('"')
            write(io, "&quot;")
        else
            write(io, b)
        end
    end
end
function postprocess_linemap!(out::AbstractVector{UInt8}, dom::Vector{Node})
    # insert <span> elements to be able to track clicks and map back to the dom element
    # TODO: also translate start and endof to line numbers?
    io = IOBuffer()
    print(io, "<pre>")
    prevstart = 0
    stack = Node[]
    push!(dom, DELETED)
    for (id, node) in enumerate(dom)
        next = node.start
        while !isempty(stack) && (stack[end].endof <= next || id == length(dom))
            let next = pop!(stack).endof
                escapehtml(io, view(out, (prevstart + 1):next), false)
                print(io, "</span>")
                prevstart = next
            end
        end
        if node !== DELETED
            escapehtml(io, view(out, (prevstart + 1):next), false)
            print(io, "<span id=\"n-")
            print(io, id)
            if !isempty(node.classes)
                print(io, "\" class=\"")
                print(io, node.classes)
            end
            print(io, "\">")
            push!(stack, node)
            prevstart = next
        end
    end
    write(io, view(out, (prevstart + 1):length(out)))
    print(io, "</pre>")
    pop!(dom)
    return (String(take!(io)), dom)
end


using Profile
using Profile: StackFrameTree, LineInfoDict, LineInfoFlatDict, StackFrame
const indent = "  ︰  "^10
const indent_z = collect(eachindex(indent))
function print_indent(io::IO, depth::Int)
    depth < 1 && return
    while depth > length(indent_z)
        print(io, indent)
        depth -= length(indent_z)
    end
    print(io, SubString(indent, 1, indent_z[depth]))
end
function tree_format(io::IO, li::StackFrame, depth::Int)
    if li == Profile.UNKNOWN
        print(io, " unknown stackframe")
    else
        if li.line == li.pointer
            print(io, " unknown function (pointer: 0x",
                string(li.pointer, base = 16, pad = 2*sizeof(Ptr{Cvoid})),
                ")")
        else
            show(io, li; full_path = true)
            #fname = string(li.func)
            #if !li.from_c && li.linfo !== nothing
            #    fname = sprint(Profile.show_spec_linfo, li)
            #end
            #print(io, " ", string(li.file), ":",
            #    li.line == -1 ? "?" : string(li.line),
            #    "; ", fname)
        end
    end
    nothing
end

# Print a "branch" starting at a particular level. This gets called recursively.
function tree(io::IO, dom::Vector{Node}, bt::StackFrameTree, depth::Int)
    isempty(bt.down) && return
    # Order the line information
    nexts = collect(values(bt.down))
    lilist = collect(frame.frame for frame in nexts)
    # Generate the string for each line
    # Recurse to the next level
    for i in Profile.liperm(lilist)
        down = nexts[i]
        from_c = down.frame.from_c
        if !from_c
            classes = down.frame.inlined ? "inlined" : ""
            li = down.frame
            count = string(down.count)
            print(io, " "^(5 - length(count)), count)
            print_indent(io, depth)
            print(io, "▻ ")
            id = startNode(dom, io, nexts, i, classes)
            tree_format(io, down.frame, depth)
            endNode(dom, io, id)
            println(io)
            tree(io, dom, down, depth + 1)
        else
            tree(io, dom, down, depth)
        end
    end
end

function tree(io::IO, dom::Vector{Node}, data::Vector{UInt64}, lidict::LineInfoFlatDict)
    combine = true
    if combine
        root = Profile.tree!(StackFrameTree{StackFrame}(), data, lidict, true)
    else
        root = Profile.tree!(StackFrameTree{UInt64}(), data, lidict, true)
    end
    tree(io, dom, root, 1)
    return root.count
end

function tree(io::IO, dom::Vector{Node}, data::Vector{UInt64}, lidict::LineInfoDict)
    newdata, newdict = Profile.flatten(data, lidict)
    return tree(io, dom, newdata, newdict)
end

richprofile() = richprofile(Profile.fetch())
richprofile(data::Vector{<:Unsigned}) = richprofile(data, Profile.getdict(data))
function richprofile(data::Vector{<:Unsigned}, lidict::LineInfoDict = Profile.getdict(data))
    io = IOBuffer()
    dom = Node[]
    maxcount = tree(convert(IOContext, io), dom, data, lidict)
    return (postprocess_linemap!(take!(io), dom)..., maxcount)
end

function collect_profile()
    text, dom, maxcount = richprofile()
    methods = Dict{Core.MethodInstance, Int}()
    heatmap = Vector{Dict{Int, Int}}()
    ref = zeros(Int, length(dom))
    # create a flame graph for rending behind methods
    flame = IOBuffer()
    print(flame, """<div id="profile"><div class="source-title">Profile</div><div class="source-profile"><div class="flame"><pre>\n""")
    for i = 1:length(dom)
        node = dom[i]
        frame = node.level[node.idx]::StackFrameTree
        linfo = frame.frame.linfo
        if linfo === nothing
            idx = 0
        else
            idx = get!(methods, frame.frame.linfo, length(methods) + 1)
            idx > length(heatmap) && push!(heatmap, Dict{Int, Int}())
            def = linfo.def::Method
            m_heatmap = heatmap[idx]
            expected_file = string(def.file)
            if isempty(splitdir(expected_file)[1])
                expected_file = "./" * expected_file
            end
            if frame.frame.file === Symbol(expected_file)
                m_heatmap[frame.frame.line] = get(m_heatmap, frame.frame.line, 0) + frame.count
            end
        end
        ref[i] = idx
        pct = round(Int, 100 * frame.count / maxcount)
        if pct > 0
            print(flame, """<span style="width:""", pct, """%"></span>\n""")
        else
            print(flame, "\n")
        end
    end
    print(flame, """</pre></div>\n""")
    print(flame, text)
    print(flame, """</div></div>\n""")
    # invert the list of unique methods in the graph
    uniques = Vector{Core.MethodInstance}(undef, length(methods))
    for (m, i) in methods
        uniques[i] = m
    end
    return String(take!(flame)), ref, uniques, heatmap
end

end # module ProfileEnhanced
