# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

## unit tests for IOFormatBuffer

let IOF = Base.IOFormatBuffer
    buf = IOContext(PipeBuffer(), :color=>true)
    let io::IOContext = IOF()
        @test_throws ArgumentError("IOFormatBuffer not readable") read(io)
        @test_throws ArgumentError("read failed, IOBuffer is not readable") read(io.io.buf)
        pop!(io.io, push!(io.io, :red))
        pop!(io.io, push!(io.io, :blue))
        write(io, "plain")
        pop!(io.io, push!(io.io, :green))
        write(buf, io.io)
        @test read(buf, String) == "plain"
        write(buf, io.io)
        @test read(buf, String) == ""
    end
    let io::IOContext = IOF()
        let id = push!(io.io, :red)
            push!(io.io, :blue)
            @test bytesavailable(io) == 0
            @test textwidth(io) == 0
            write(io, "redblue")
            pop!(io.io, id)
        end
        write(buf, io.io)
        @test read(buf, String) == "\e[34mredblue\e[31m\e[39m"
        @test bytesavailable(io) == 0
        @test textwidth(io) == 0
        write(buf, io.io)
        @test read(buf, String) == ""
        write(io, "hello")
        write(buf, io.io)
        @test read(buf, String) == "hello"
        let id = push!(io.io, :red)
            push!(io.io, :blue)
            write(io, "abcde")
            @test bytesavailable(io) == 5
            @test textwidth(io) == 5
            push!(io.io, :green)
            write(io, "αβγ")
            pop!(io.io, id)
        end
        @test bytesavailable(io) == 11
        @test textwidth(io) == 8
        truncate(io.io, 1)
        @test bytesavailable(io) == 1
        @test textwidth(io) == 1
        write(io, "1234567890")
        @test bytesavailable(io) == 11
        @test textwidth(io) == 11
        write(buf, io.io)
        @test bytesavailable(io) == 0
        @test textwidth(io) == 0
        @test read(buf, String) == "\e[34ma\e[31m\e[39m1234567890"
    end
    let io::IOContext = IOF()
        write(io, "before")
        let id = push!(io.io, :red)
            push!(io.io, :blue)
            write(io, "redblue")
            pop!(io.io, id)
        end
        write(buf, io.io)
        @test read(buf, String) == "before\e[31m\e[34mredblue\e[31m\e[39m"
    end
    let io::IOContext = IOF()
        write(io, "before\n")
        let id = push!(io.io, :red)
            push!(io.io, :blue)
            write(io, "redblue")
            pop!(io.io, id)
        end
        write(buf, io.io)
        @test read(buf, String) == "before\n\e[34mredblue\e[31m\e[39m"
    end
    let io::IOContext = IOF()
        let id = push!(io.io, :red)
            write(io, "red\n\nder")
        end
        write(buf, io.io)
        @test read(buf, String) == "\e[31mred\e[39m\n\n\e[31mder\e[39m"
    end
    let io::IOContext = IOF()
        write(io, "before")
        let id = push!(io.io, :red)
            write(io, "r")
            let id = push!(io.io, :underline)
                write(io, "u")
                let id = push!(io.io, :underline)
                    write(io, "u")
                    let id = push!(io.io, :normal)
                        write(io, "n")
                        let id = push!(io.io, :red)
                            write(io, "r")
                            let id = push!(io.io, :underline)
                                write(io, "u")
                                let id = push!(io.io, :underline)
                                    write(io, "some\n\ntext")
                                    pop!(io.io, id)
                                end
                                write(io, "u")
                                pop!(io.io, id)
                            end
                            write(io, "r")
                            pop!(io.io, id)
                        end
                        write(io, "n")
                        pop!(io.io, id)
                    end
                    write(io, "u")
                    pop!(io.io, id)
                end
                write(io, "u")
                pop!(io.io, id)
            end
            write(io, "r")
            pop!(io.io, id)
        end
        write(io, "after")
        write(buf, io.io)
        @test read(buf, String) == "before\e[31mr\e[4mu\e[4mu\e[0mn\e[31mr\e[4mu\e[4msome\e[24m\e[39m\n\n\e[31m\e[4mtextu\e[24mr\e[0mn\e[31m\e[4muu\e[24mr\e[39mafter"
    end
    let io::IOContext = IOF()
        # tests for `copy`
        @test io.io::IOF == io.io
        @test copy(io.io)::IOF != io.io
        printstyled(io, "red", color = :red)
        print(io, ", ")
        printstyled(io, "blue", color = :blue)
        for i = 1:2
            buf = IOBuffer()
            write(IOContext(buf, :color => true), copy(io.io))
            @test String(take!(buf)) == "\e[31mred\e[39m, \e[34mblue\e[39m"
            buf = IOBuffer()
            write(IOContext(buf, :color => false), copy(io.io))
            @test String(take!(buf)) == "red, blue"
        end
        buf = IOBuffer()
        write(buf, io.io)
        @test String(take!(buf)) == "red, blue"
        buf = IOBuffer()
        write(buf, io.io)
        @test String(take!(buf)) == ""
        buf = IOBuffer()
        write(IOContext(buf, :color => true), io.io)
        @test String(take!(buf)) == ""
    end
end



## integration and usage tests for printstyled

let buf = IOBuffer()
    printstyled(IOContext(buf, :color=>true), "foo", color=:red)
    @test startswith(String(take!(buf)), Base.text_colors[:red])
end

# Test that `printstyled` accepts non-string values, just as `print` does
let buf_color = IOBuffer()
    args = (3.2, "foo", :testsym)
    printstyled(IOContext(buf_color, :color=>true), args..., color=:red)
    buf_plain = IOBuffer()
    print(buf_plain, args...)
    expected_str = string(Base.text_colors[:red],
                          String(take!(buf_plain)),
                          Base.text_colors[:default])
    @test expected_str == String(take!(buf_color))
end

# Test that `printstyled` on multiline input prints the ANSI codes
# on each line
let buf_color = IOBuffer()
    str = "Two\nlines"
    printstyled(IOContext(buf_color, :color=>true), str; bold=true, color=:red)
    @test String(take!(buf_color)) == "\e[31m\e[1mTwo\e[22m\e[39m\n\e[31m\e[1mlines\e[22m\e[39m"
end

let
    @test haskey(stdout, :color) == false
    @test haskey(stdout, :bar) == false
    @test (:color=>true) ∉ stdout
    @test (:color=>false) ∉ stdout
    @test get(stdout, :color, nothing) === nothing
    @test get(stdout, :bar, nothing) === nothing
    @test_throws KeyError stdout[:color]
    @test_throws KeyError stdout[:bar]
end

let
    global c_18711 = 0
    buf = IOContext(IOBuffer(), :hascontext => true)
    Base.with_format(:red, buf) do buf
        global c_18711
        get(buf, :hascontext, false) && (c_18711 += 1)
    end
    @test c_18711 == 1
end

let buf = IOBuffer()
    buf_color = IOContext(buf, :color => true)
    printstyled(buf_color, "foo", color=:red)
    # Check that we get back to normal text color in the end
    @test String(take!(buf)) == "\e[31mfoo\e[39m"

    # Check that boldness is turned off
    printstyled(buf_color, "foo"; bold=true, color=:red)
    @test String(take!(buf)) == "\e[31m\e[1mfoo\e[22m\e[39m"
end

let buf = IOBuffer()
    Base.with_format(:red, IOContext(buf, :color => true)) do io; print(io, "red",
        Base.text_colors[:underline], " underline")
    Base.with_format(:blue, io) do io; print(io, " blue")
    Base.with_format(:bold, io) do io; print(io, " bold")
    Base.with_format(:green, io) do io; print(io, " green\ngreen")
    end; print(io, " bold",
        Base.text_colors[:underline], " underline")
    end; print(io, " blue")
    end; print(io, " red")
    end
    @test String(take!(buf)) == "\e[31mred\e[4m underline\e[34m blue\e[1m bold\e[32m green\e[22m\e[39m\n\e[32m\e[1mgreen\e[34m bold\e[4m underline\e[22m blue\e[31m red\e[39m"
end
