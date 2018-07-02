# This file is a part of Julia. License is MIT: https://julialang.org/license

include("formatting.jl")

const margin = 2

function term(io::IO, content::Vector)
    first = true
    for md in content
        first ? (first = false) : print(io, "\n\n")
        term(io, md)
    end
end

term(io::IO, md::MD) = term(io, md.content)

function term(io::IO, md::Paragraph)
    pre = ' '^margin
    print(io, pre)
    with_format(:wordwrap => (pre, margin), io) do io
        terminline(io, md.content)
    end
end

function term(io::IO, md::BlockQuote)
    pre = (' '^margin) * '│'
    print(pre)
    with_format(:wordwrap => (pre, 9 - margin), io) do io
        term(io, md.content)
    end
end

function term(io::IO, md::Admonition)
    col = :default
    # If the types below are modified, the page manual/documentation.md must be updated accordingly.
    if lowercase(md.title) == "danger"
        col = Base.error_color()
    elseif lowercase(md.title) == "warning"
        col = Base.warn_color()
    elseif lowercase(md.title) in ("info", "note")
        col = Base.info_color()
    elseif lowercase(md.title) == "tip"
        col = :green
    end
    with_format(col, io; bold=true) do io
        pre = (' '^margin) * "│ "
        print(io, pre)
        print(io, isempty(md.title) ? md.category : md.title)
        print(io, '\n', pre)
        with_format(:wordwrap => (pre, 8 - margin), io) do io
            term(io, md.content)
        end
    end
end

function term(io::IO, f::Footnote)
    pre = (' '^margin) * "│ "
    print(io, pre)
    printstyled(io, "[^$(f.id)]", bold=true)
    print(io, '\n', pre)
    with_format(:wordwrap => (pre, 8 - margin), io) do io
        term(io, f.text)
    end
end

function term(io::IO, md::List)
    pre = 2margin + 2
    post = 2margin
    with_format(:wordwrap => (pre, post), io) do io
        for (i, point) in enumerate(md.items)
            i == 1 || print(io, "\n\n")
            print(io, ' '^2margin, isordered(md) ? "$(i + md.ordered - 1). " : "•  ")
            term(io, point)
        end
    end
end

const _header_underlines = collect("≡=–-⋅ ")
# TODO settle on another option with unicode e.g. "≡=≃–∼⋅" ?

function term(io::IO, md::Header)
    underline = get(_header_underlines, header.level, ' ')
    pre = 1
    post = 4margin - pre
    with_format(:wordwrap => (pre, post), io) do io
        print(io, ' '^margin)
        with_format(:border => ("", "", underline, ""), io) do io # top, right, bottom, and left
            with_format(:bold, io) do io
                terminline(io, md.text)
            end
        end
    end
end

function term(io::IO, md::Code)
    with_format(:cyan, io) do io
        L = lines(md.code)
        first = true
        pre = ' '^margin
        for ln in L
            first ? (first = false) : println(io)
            print(io, pre, l)
        end
    end
end

term(io::IO, br::LineBreak) = nothing # line breaks already printed between subsequent elements

function term(io::IO, br::HorizontalRule)
    with_format(:wordwrap => (margin, margin), io) do io # add padding
        with_format(:border => ("", "", '─', ""), io) do io
            # no text -> boarder will make a full-width line (minus padding)
            println(io)
        end
    end
end

term(io::IO, x) = show(io, MIME"text/plain"(), x)

# Inline Content

terminline_string(io::IO, md) = sprint(terminline, md; context=io)

function terminline(io::IO, content::Vector)
    for md in content
        terminline(io, md)
    end
end

function terminline(io::IO, md::AbstractString)
    print(io, replace(md, r"[\s\t\n]+" => ' '))
end

function terminline(io::IO, md::Bold)
    with_format(terminline, :bold, io, md.text)
end

function terminline(io::IO, md::Italic)
    with_format(terminline, :underline, io, md.text)
end

function terminline(io::IO, md::LineBreak)
    println(io)
end

function terminline(io::IO, md::Image)
    terminline(io, "(Image: $(md.alt))")
end

terminline(io::IO, f::Footnote) = with_format(terminline, :bold, io, "[^$(f.id)]")

function terminline(io::IO, md::Link)
    terminline(io, md.text)
    if !Base.startswith(md.url, "@ref")
        terminline(io, " ($(md.url))")
    end
end

function terminline(io::IO, code::Code)
    printstyled(io, code.code, color=:cyan)
end

terminline(io::IO, x) = show(io, MIME"text/plain"(), x)

# Show in terminal
Base.show(io::IO, ::MIME"text/plain", md::MD) = with_format(term, io, md)
