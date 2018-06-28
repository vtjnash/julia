# I/O and Network

## General I/O

```@docs
Base.stdout
Base.stderr
Base.stdin
Base.open
Base.IOBuffer
Base.PipeBuffer
Base.take!(::Base.GenericIOBuffer)
Base.fdio
Base.flush
Base.close
Base.write
Base.read
Base.read!
Base.readbytes!
Base.unsafe_read
Base.unsafe_write
Base.position
Base.seek
Base.seekstart
Base.seekend
Base.skip
Base.mark
Base.unmark
Base.reset
Base.ismarked
Base.eof
Base.isreadonly
Base.iswritable
Base.isreadable
Base.isopen
Base.fd
Base.redirect_stdout
Base.redirect_stdout(::Function, ::Any)
Base.redirect_stderr
Base.redirect_stderr(::Function, ::Any)
Base.redirect_stdin
Base.redirect_stdin(::Function, ::Any)
Base.readchomp
Base.truncate
Base.readavailable
Base.bytesavailable
Base.bytesavailable_until_text
```

## Text I/O

These text-based IO methods are useful for emitting and parsing streams of data.
Often, they form the basis for incrementally building or consuming strings.
In this role, the [`IOBuffer`](@ref) and [`IOFormatBuffer`](@ref) types are especially useful,
as are the functions that use them, such as the utility methods [`sprint`](@ref) and [`with_format`](@ref),
and the constructor [`IOBuffer("string")`](@ref `IOBuffer`).

These IO methods are all based on UTF-8, identical to the primary [`String`](@ref) type for Julia.

```@docs
Base.show(::Any)
Base.summary
Base.print
Base.println
Base.Grisu.print_shortest
Base.sprint
Base.repr(::Any)
Base.showerror
Base.dump
Meta.@dump
Base.readline
Base.readuntil
Base.readlines
Base.eachline
Base.countlines
Base.skipchars
Base.truncate_text
Base.bytesavailable_until_text
Base.textwidth(::IO)
Base.displaysize
Base.IOContext
Base.IOContext(::IO, ::Pair)
Base.IOContext(::IO, ::IOContext)
```

## Rich-text (formatted) I/O

Some output contains structural information that may be of particular interest to the user.
While [`IOContext`](@ref) gives the caller some control over the content of the output,
propagation and observance of that contextual information is strictly optional.
By contrast, the [`IOFormatBuffer`](@ref) allows the caller to take strict control over the output.

That control can take the form of adding specific sequences of formatting annotations (such as ANSI or HTML),
or could take the form of truncating and abbreviating and reformatting the output to fit the output device,
or could be some other arbitrary metadata that the callee wanted to reflect.

```@docs
Base.printstyled
Base.with_format
Base.IOFormatBuffer
```

## Multimedia I/O

Just as text output is performed by [`print`](@ref) and user-defined types can indicate their textual
representation by overloading [`show`](@ref), Julia provides a standardized mechanism for rich multimedia
output (such as images, formatted text, or even audio and video), consisting of three parts:

  * A function [`display(x)`](@ref) to request the richest available multimedia display of a Julia object
    `x` (with a plain-text fallback).
  * Overloading [`show`](@ref) allows one to indicate arbitrary multimedia representations (keyed by standard
    MIME types) of user-defined types.
  * Multimedia-capable display backends may be registered by subclassing a generic `AbstractDisplay` type
    and pushing them onto a stack of display backends via [`pushdisplay`](@ref).

The base Julia runtime provides only plain-text display, but richer displays may be enabled by
loading external modules or by using graphical Julia environments (such as the IPython-based IJulia
notebook).

```@docs
Base.Multimedia.display
Base.Multimedia.redisplay
Base.Multimedia.displayable
Base.show(::Any, ::Any, ::Any)
Base.Multimedia.showable
Base.repr(::MIME, ::Any)
```

As mentioned above, one can also define new display backends. For example, a module that can display
PNG images in a window can register this capability with Julia, so that calling [`display(x)`](@ref) on
types with PNG representations will automatically display the image using the module's window.

In order to define a new display backend, one should first create a subtype `D` of the abstract
class `AbstractDisplay`.  Then, for each MIME type (`mime` string) that can be displayed on `D`, one should
define a function `display(d::D, ::MIME"mime", x) = ...` that displays `x` as that MIME type,
usually by calling [`show(io, mime, x)`](@ref) or [`repr(io, mime, x)`](@ref).
A `MethodError` should be thrown if `x` cannot be displayed
as that MIME type; this is automatic if one calls `show` or `repr`. Finally, one should define a function
`display(d::D, x)` that queries [`showable(mime, x)`](@ref) for the `mime` types supported by `D`
and displays the "best" one; a `MethodError` should be thrown if no supported MIME types are found
for `x`.  Similarly, some subtypes may wish to override [`redisplay(d::D, ...)`](@ref Base.Multimedia.redisplay). (Again, one should
`import Base.display` to add new methods to `display`.) The return values of these functions are
up to the implementation (since in some cases it may be useful to return a display "handle" of
some type).  The display functions for `D` can then be called directly, but they can also be invoked
automatically from [`display(x)`](@ref) simply by pushing a new display onto the display-backend stack
with:

```@docs
Base.Multimedia.pushdisplay
Base.Multimedia.popdisplay
Base.Multimedia.TextDisplay
Base.Multimedia.istextmime
```

## Network I/O

Most networking-related code is in the [`Sockets`](@ref) module.

```@docs
Base.ntoh
Base.hton
Base.ltoh
Base.htol
Base.ENDIAN_BOM
```
