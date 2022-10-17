# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
using Random
import REPL.LineEdit
using Markdown

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :FakePTYs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FakePTYs.jl"))
import .Main.FakePTYs: with_fake_pty

# For curmod_*
include(joinpath(BASE_TEST_PATH, "testenv.jl"))

include("FakeTerminals.jl")
import .FakeTerminals.FakeTerminal


function kill_timer(delay)
    # Give ourselves a generous timer here, just to prevent
    # this causing e.g. a CI hang when there's something unexpected in the output.
    # This is really messy and leaves the process in an undefined state.
    # the proper and correct way to do this in real code would be to destroy the
    # IO handles: `close(stdout_read); close(stdin_write)`
    test_task = current_task()
    function kill_test(t)
        # **DON'T COPY ME.**
        # The correct way to handle timeouts is to close the handle:
        # e.g. `close(stdout_read); close(stdin_write)`
        test_task.queue === nothing || Base.list_deletefirst!(test_task.queue, test_task)
        schedule(test_task, "hard kill repl test"; error=true)
        print(stderr, "WARNING: attempting hard kill of repl test after exceeding timeout\n")
    end
    return Timer(kill_test, delay)
end

# REPL tests
function fake_repl(@nospecialize(f); options::REPL.Options=REPL.Options(confirm_exit=false))
    # Use pipes so we can easily do blocking reads
    # In the future if we want we can add a test that the right object
    # gets displayed by intercepting the display
    input = Pipe()
    output = Pipe()
    err = Pipe()
    Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(err, reader_supports_async=true, writer_supports_async=true)

    repl = REPL.LineEditREPL(FakeTerminal(input.out, output.in, err.in, options.hascolor), options.hascolor)
    repl.options = options

    hard_kill = kill_timer(900) # Your debugging session starts now. You have 15 minutes. Go.
    f(input.in, output.out, repl)
    t = @async begin
        close(input.in)
        close(output.in)
        close(err.in)
    end
    @test read(err.out, String) == ""
    #display(read(output.out, String))
    Base.wait(t)
    close(hard_kill)
    nothing
end

# Writing ^C to the repl will cause sigint, so let's not die on that
Base.exit_on_sigint(false)

# returns a new stream that has the identical content to `in`, but also "tees"
# the `transform(readavailable(in)::Vector{UInt8})` first to `out`
function tee(f, in::IO)
    copy = Base.BufferStream()
    t = @async try
        while !eof(in)
            l = readavailable(in)
            f(l)
            write(copy, l)
        end
    catch ex
        if !(ex isa Base.IOError && ex.code == Base.UV_EIO)
            rethrow() # ignore EIO on `in` stream
        end
    finally
        # TODO: could we call closewrite to propagate an error, instead of always doing a clean close here?
        closewrite(copy)
    end
    Base.errormonitor(t)
    return copy
end
tee(out::IO, in::IO) = tee(l -> write(out, l), in)


# Non standard output_prefix, tested via `ipython_mode!`
fake_repl() do stdin_write, stdout_read, repl
    repl.interface = REPL.setup_interface(repl)

    backend = REPL.REPLBackend()
    repltask = @async begin
        REPL.run_repl(repl; backend)
    end

    REPL.ipython_mode!(repl, backend)

    stdout_read = tee(stdout_read) do x
        println(repr(String(x)))
        @show stdout_read
    end
    # Some alternative API design options for our `tee` function:
    # stdout_read = tee(stdout_read => stdout, identity)
    # stdout_read = tee(x -> write(stdout, x), stdout_read)
    # stdout_read = tee(stdout_read) do x; write(stdout, x); end

    global c = Base.Event(true)
    function sendrepl2(cmd, txt)
        t = @async try write(stdin_write, "$cmd\n notify($(curmod_prefix)c); \"done\"\n"); catch ex; println(ex); finally; print("written\n"); end
        r = readuntil(stdout_read, txt, keep=true)
        readuntil(stdout_read, "\"done\"\n\n", keep=true)
        wait(c)
        wait(t)
        return r
    end

    s = sendrepl2("\"z\" * \"z\"\n", "\"zz\"")
    @test contains(s, "In [1]")
    @test contains(s, "Out[1]: \"zz\"")

    s = sendrepl2("\"y\" * \"y\"\n", "\"yy\"")
    @test contains(s, "Out[3]: \"yy\"")

    s = sendrepl2("Out[1] * Out[3]\n", "\"zzyy\"")
    @test contains(s, "Out[5]: \"zzyy\"")

    write(stdin_write, '\x04')
    Base.wait(repltask)
end
