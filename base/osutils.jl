# This file is a part of Julia. License is MIT: http://julialang.org/license

const OS_NAME = ccall(:jl_get_OS_NAME, Any, ())

"""
    is_unix([os])

Predicate for testing if the OS provides a Unix-like interface.
See documentation :ref:`Handling Operating System Variation <man-handling-operating-system-variation>`\ .
"""
function is_unix(os::Symbol = OS_NAME)
    if is_windows(os)
        return false
    elseif is_linux(os) || is_bsd(os)
        return true
    else
        throw(ArgumentError("unknown operating system \"$os\""))
    end
end

"""
    is_linux([os])

Predicate for testing if the OS is a derivative of Linux.
See documentation :ref:`Handling Operating System Variation <man-handling-operating-system-variation>`\ .
"""
is_linux(os::Symbol = OS_NAME) = (os == :Linux)

"""
    is_bsd([os])

Predicate for testing if the OS is a derivative of BSD.
See documentation :ref:`Handling Operating System Variation <man-handling-operating-system-variation>`\ .
"""
is_bsd(os::Symbol = OS_NAME) = (os == :FreeBSD || os == :OpenBSD || os == :NetBSD || os == :Darwin)

"""
    is_windows([os])

Predicate for testing if the OS is a derivative of Microsoft Windows NT.
See documentation :ref:`Handling Operating System Variation <man-handling-operating-system-variation>`\ .
"""
is_windows(os::Symbol = OS_NAME) = (os == :Windows)

"""
    is_apple([os])

Predicate for testing if the OS is a derivative of Apple Macintosh OS X or Darwin.
See documentation :ref:`Handling Operating System Variation <man-handling-operating-system-variation>`\ .
"""
is_apple(os::Symbol = OS_NAME) = (os == :Darwin)

"""
    @static

Partially evaluates an expression at parse time.

For example, `@static is_windows() ? foo : bar` will evaluate `is_windows()` and insert either `foo` or `bar` into the expression.
This is useful in cases where a construct would be invalid on other platforms,
such as a ccall to a non-existant functions.
"""
macro static(ex)
    if isa(ex, Expr)
        if ex.head === :if
            cond = eval(current_module(), ex.args[1])
            if cond
                return esc(ex.args[2])
            elseif length(ex.args) == 3
                return esc(ex.args[3])
            else
                return nothing
            end
        end
    end
    throw(ArgumentError("invalid @static macro"))
end

# Windows version macros

if is_windows()
    function windows_version()
        verinfo = ccall(:GetVersion, UInt32, ())
        (Int(verinfo & 0xFF), Int((verinfo >> 8) & 0xFF))
    end
else
    windows_version() = (0, 0)
end
"""
    windows_version()

Returns the version number for the Windows NT Kernel as a (major, minor) pair,
or (0, 0) if this is not running on Windows.
"""
windows_version

const WINDOWS_VISTA_VER = (6, 0)
