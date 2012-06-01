#TODO: fix return types of run, success
#TODO: missing functions: successful, wait
#TODO: stop | and & from mutating inputs
#TODO: broken test case? (`a`|`b`)&((`c`&`d`)|`e`)|`f` -> (`c` & `d` & (`a` | `b`)) | `e` | `f`

typealias PtrSize Int
const UVHandle = Ptr{Void}
const IOStreamHandle = Ptr{Void}
localEventLoop() = ccall(:jl_local_event_loop,Ptr{Void},())
globalEventLoop() = ccall(:jl_global_event_loop,Ptr{Void},())

typealias Executable Union(Vector{ByteString},Function)
typealias Callback Union(Function,Bool)

abstract AsyncStream <: Stream
typealias StreamOrNot Union(Bool,AsyncStream)

abstract AbstractCmd

type Cmd <: AbstractCmd
    exec::Executable
    Cmd(exec::Executable) = new(exec)
end

type OrCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    OrCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

type AndCmds <: AbstractCmd
    a::AbstractCmd
    b::AbstractCmd
    AndCmds(a::AbstractCmd, b::AbstractCmd) = new(a,b)
end

typealias StreamHandle Union(PtrSize,AsyncStream)


abstract AbstractProcess
type Process <: AbstractProcess
    handle::Ptr{Void}
    in::StreamOrNot
    out::StreamOrNot
    err::StreamOrNot
    exit_code::Int32
    term_signal::Int32
    breakEventLoop::Bool
    Process(handle::Ptr{Void},in::StreamOrNot,out::StreamOrNot,err::StreamOrNot)=new(handle,in,out,err,-2,-2,false,false)
    Process(handle::Ptr{Void},in::StreamOrNot,out::StreamOrNot)=Process(handle,in,out,0,false,false)
end

type OrProcesses <: AbstractProcess
    a::AbstractCmd
    b::AbstractCmd
    OrProcesses()=new(null,null)
end

type AndProcesses <: AbstractProcess
    a::AbstractCmd
    b::AbstractCmd
    breakEventLoop::Bool
    AndProcesses()=new(null,null,false)
end

typealias BufOrNot Union(Bool,IOStream)

type NamedPipe <: AsyncStream
    ihandle::Ptr{Void}
    ohandle::Ptr{Void}
    buf::BufOrNot
    closed::Bool
    NamedPipe(ihandle::Ptr{Void},ohandle::Ptr{Void},buf::IOStream) = new(ihandle,ohandle,buf,false)
    NamedPipe(ihandle::Ptr{Void},ohandle::Ptr{Void}) = new(ihandle,ohandle,false,false)
end

type TTY <: AsyncStream
    handle::Ptr{Void}
    handle::Ptr{Void}
    buf::BufOrNot
    closed::Bool
end

convert(T::Type{Ptr{Void}}, s::AsyncStream) = convert(T, s.handle)

make_stdout_stream() = TTY(ccall(:jl_stdout_stream, Ptr{Void}, ()),memio(),false)

function _uv_tty2tty(handle::Ptr{Void})
    TTY(handle,memio(),false)
end

OUTPUT_STREAM = make_stdout_stream()

## SOCKETS ##

abstract Socket <: AsyncStream

type TcpSocket <: Socket
    handle::Ptr{Void}
    buf::BufOrNot
    open::Bool
    TcpSocket(handle::Ptr{Void})=new(handle,false,false)
end

type UdpSocket <: Socket
    handle::Ptr{Void}
    buf::BufOrNot
    open::Bool
    UdpSocket(handle::Ptr{Void})=new(handle,false,false)
end

function _init_buf(stream::AsyncStream)
    if(!isa(stream.buf,IOStream))
        stream.buf=memio()
    end
end

_jl_tcp_init(loop::Ptr{Void}) = ccall(:jl_tcp_init,Ptr{Void},(Ptr{Void},),loop)
_jl_udp_init(loop::Ptr{Void}) = ccall(:jl_udp_init,Ptr{Void},(Ptr{Void},),loop)

abstract IpAddr

type Ip4Addr <: IpAddr
    port::Uint16
    host::Uint32
end

type Ip6Addr <: IpAddr
    port::Uint16
    host::Array{Uint8,1} #this should be fixed at 16 bytes is fixed size arrays are implemented
    flow_info::Uint32
    scope::Uint32
end

_jl_listen(sock::AsyncStream,backlog::Int32,cb::Function) = ccall(:jl_listen,Int32,(Ptr{Void},Int32,Function),sock.handle,backlog,make_callback(cb))

_jl_tcp_bind(sock::TcpSocket,addr::Ip4Addr) = ccall(:jl_tcp_bind,Int32,(Ptr{Void},Uint32,Uint16),sock.handle,hton(addr.port),addr.host)
_jl_tcp_connect(sock::TcpSocket,addr::Ip4Addr) = ccall(:jl_tcp_connect,Int32,(Ptr{Void},Uint32,Uint16,Function),sock.handle,addr.host,hton(addr.port))
_jl_tcp_accept(server::Ptr,client::Ptr) = ccall(:uv_accept,Int32,(Ptr{Void},Ptr{Void}),server,client)
_jl_tcp_accept(server::TcpSocket,client::TcpSocket) = _jl_tcp_accept(server.handle,client.handle)

function open_any_tcp_port(preferred_port::Uint16,cb::Function)
    socket = TcpSocket(_jl_tcp_init(globalEventLoop()));
    if(socket.handle==0)
        error("open_any_tcp_port: could not create socket")
    end
    addr = Ip4Addr(preferred_port,uint32(0)) #bind prefereed port on all adresses
    while _jl_tcp_bind(socket,addr)!=0
        addr.port+=1;
    end
    err = _jl_listen(socket,int32(4),cb)
    if(err!=0)
        show(err)
        error("open_any_tcp_port: could not listen on socket")
    end
    return (addr.port,socket)
end
open_any_tcp_port(preferred_port::Integer,cb::Function)=open_any_tcp_port(uint16(preferred_port),cb)

abstract AsyncWork

type SingleAsyncWork <: AsyncWork
    handle::Ptr{Void}
    SingleAsyncWork(handle::Ptr{Void})=new(handle)
end

type IdleAsyncWork <: AsyncWork
    handle::Ptr{Void}
end

type TimeoutAsyncWork <: AsyncWork
    handle::Ptr{Void}
end

const dummySingleAsync = SingleAsyncWork(C_NULL)

function createSingleAsyncWork(loop::Ptr{Void},cb::Function)
    return SingleAsyncWork(ccall(:jl_make_async,Ptr{Void},(Ptr{Void},Function),loop,cb))
end

function initIdleAsync(loop::Ptr{Void})
    IdleAsyncWork(ccall(:jl_idle_init,Ptr{Void},(Ptr{Void},),int(loop)))
end

function initTimeoutAsync(loop::Ptr{Void})
    TimeoutAsyncWork(ccall(:jl_timer_init,Ptr{Void},(Ptr{Void},),loop))
end

function startTimer(timer::TimeoutAsyncWork,cb::Function,timeout::Int64,repeat::Int64)
    ccall(:jl_timer_start,Int32,(Ptr{Void},Function,Int64,Int64),timer.handle,cb,timeout,repeat)
end

function stopTimer(timer::TimeoutAsyncWork)
    ccall(:jl_timer_stop,Int32,(Ptr{Void},),timer.handle)
end

assignIdleAsyncWork(work::IdleAsyncWork,cb::Function) = ccall(:jl_idle_start,Ptr{Void},(Ptr{Void},Function),work.handle,cb)

function add_idle_cb(loop::Ptr{Void},cb::Function)
    work = initIdleAsyncWork(loop)
    assignIdleAsyncWork(work,cb)
    work
end

function queueAsync(work::SingleAsyncWork)
    ccall(:jl_async_send,Void,(Ptr{Void},),work.handle)
end

# process status #
abstract ProcessStatus
type ProcessNotRun   <: ProcessStatus; end
type ProcessRunning  <: ProcessStatus; end
type ProcessExited   <: ProcessStatus; status::PtrSize; end
type ProcessSignaled <: ProcessStatus; signal::PtrSize; end
type ProcessStopped  <: ProcessStatus; signal::PtrSize; end

process_exited  (s::Process) = (s.exit_code != -2)
process_signaled(s::Process) = (s.term_signal > 0)
process_stopped (s::Process) = 0 #not supported by libuv. Do we need this?

process_exit_status(s::Process) = s.exit_code
process_term_signal(s::Process) = s.term_signal
process_stop_signal(s::Process) = 0 #not supported by libuv. Do we need this?

function process_status(s::PtrSize)
    process_exited  (s) ? ProcessExited  (process_exit_status(s)) :
    process_signaled(s) ? ProcessSignaled(process_term_signal(s)) :
    process_stopped (s) ? ProcessStopped (process_stop_signal(s)) :
    error("process status error")
end

## types

##event loop
function run_event_loop(loop::Ptr{Void})
    ccall(:jl_run_event_loop,Void,(Ptr{Void},),loop)
end
run_event_loop() = run_event_loop(localEventLoop())

function break_one_loop(loop::Ptr{Void})
    ccall(:uv_break_one,Void,(Ptr{Void},),loop)
end

function process_events(loop::Ptr{Void})
    ccall(:jl_process_events,Void,(Ptr{Void},),loop)
end
process_events() = process_events(localEventLoop())

##pipe functions

function make_pipe()
    NamedPipe(ccall(:jl_make_pipe,Ptr{Void},()),ccall(:jl_make_pipe,Ptr{Void},())) #make the pipe and unbuffered stream for now
end

function close(stream::AsyncStream)
    if(!stream.closed)
        ccall(:jl_close_uv,Void,(Ptr{Void},),stream.handle)
        stream.closed=true
    end
end

##stream functions

function start_reading(stream::AsyncStream,cb::Function)
    ccall(:jl_start_reading,Bool,(Ptr{Void},Ptr{Void},Function),stream.handle,stream.buf.ios,cb!=0?cb:C_NULL)
end
start_reading(stream::AsyncStream) = start_reading(stream,0)

function stop_reading(stream::AsyncStream)
    ccall(:jl_stop_reading,Bool,(Ptr{Void},),stream.handle)
end

function readall(stream::AsyncStream)
    start_reading(stream)
    run_event_loop()
    return takebuf_string(stream.buf)
end

show(io, p::Process) = show(io, "Process")


function finish_read(pipe::NamedPipe)
    close(pipe) #handles to UV and ios will be invalid after this point
end

function finish_read(state::(NamedPipe,ByteString))
    finish_read(state...)
end

function end_process(p::Process,h::Ptr{Void},e::Int32, t::Int32)
    p.exit_code=e
    p.term_signal=t
end

_jl_spawn(cmd::Ptr{Uint8}, argv::Ptr{Ptr{Uint8}}, loop::Ptr{Void}, iin::Ptr{Void}, oin::Ptr{Void}, iout::Ptr{Void}, oout::Ptr{Void}, exitcb::Callback, closecb::Callback) =
    ccall(:jl_spawn, PtrSize,
        (Ptr{Uint8}, Ptr{Ptr{Uint8}}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}, Union(Function, Ptr{Void}),  Union(Function, Ptr{Void})),
         cmd,        argv,            loop,      iin,       oin,       iout,      oout,      exitcb==false?C_NULL:exitcb, closecb==false?C_NULL:closecb)

function spawn(cmd::Cmd,in::StreamOrNot,out::StreamOrNot,exitcb::Callback,closecb::Callback,pp::Process)
    loop = localEventLoop()
    if isa(in, NamedPipe)
        iin = in.ihandle
        oin = in.ohandle
    else
        iin = C_NULL
    end
    if isa(out, NamedPipe)
        iout = out.ihandle
        oout = out.ohandle
    else
        iout = C_NULL
        oout = C_NULL
    end
    ptrs = _jl_pre_exec(cmd.exec)
    if exitcb == false
        exitcb = make_callback((args...)->process_exited_chain(pp,args...))
    end
    if closecb == false
        closecb = make_callback((args...)->process_closed_chain(pp))
    end
    pp.handle=_jl_spawn(ptrs[1],convert(Ptr{Ptr{Uint8}},ptrs),loop,iin,oin,iout,oout,exitcb,closecb)
    pp.in=in
    pp.out=out
    pp
end
spawn(cmd::Cmd,in::StreamOrNot,out::StreamOrNot,exitcb::Callback,closecb::Callback) = spawn(cmd,in,out,exitcb,closecb,Process(C_NULL,false,false,false))
spawn(cmd::Cmd,in::StreamOrNot,out::StreamOrNot,exitcb::Callback) = spawn(cmd,in,out,exitcb,false)
spawn(cmd::Cmd,in::StreamOrNot,out::StreamOrNot)=spawn(cmd,in,out,false,false)
spawn(cmd::Cmd,in::StreamOrNot)=spawn(cmd,in,false,false,false)
spawn(cmd::Cmd)=spawn(cmd,false,false,false,false)
function spawn_nostdin(cmd::Cmd,out::StreamOrNot)
    pipe=make_pipe()
    proc=spawn(cmd,pipe,out)
    close(pipe)
    proc
end

#TODO: fix these exit chains
function process_exited_chain(p::Process,h::Ptr,e::Int32,t::Int32)
    p.exit_code=e
    p.term_signal=t
    if p.breakEventLoop
        break_one_loop(localEventLoop())
    end
    true
end
function process_exited_chain(procs::Processes,h::Ptr,e::Int32,t::Int32)
    done=true
    for p::Process in procs.siblings
        if p.handle==h
            p.exit_code=e
            p.term_signal=t
        end
        if p.exit_code==-2
            done=false
        end
    end
    if done&&procs.breakEventLoop
        break_one_loop(localEventLoop())
    end
    done
end

function process_closed_chain(p::Process)
    done = process_exited(p)
    if done && p.out!=0
        close(p.out)
    end
    done
end
function process_closed_chain(procs::Processes)
    done=true
    for p in procs.siblings
        if !process_exited(p)
            done=false
        end
    end
    if done && procs.out!=0
        close(procs.out)
    end
    done
end

function spawn(cmds::OrCmds,in::StreamOrNot,out::StreamOrNot,exitcb::Callback,closecb::Callback)
    procs = OrProcesses()
    n = make_pipe()
    procs.a=spawn(cmds.pipeline,in,n)
    procs.b=spawn(cmds.pipeline,n,out,exitcb,closecb)
    return procs
end
spawn(cmds::OrCmd)=spawn(cmds,false,false)
spawn(cmds::OrCmd,in::StreamOrNot)=spawn(cmds,in,false)
spawn(cmds::OrCmd,in::StreamOrNot,out::StreamOrNot)=spawn(cmds,in,out,false,false)

function spawn(cmds::AndCmds,in::StreamOrNot,out::StreamOrNot,exitcb::Callback,closecb::Callback)
    procs = AndProcesses()
    exitcb2 = make_callback((args...)->(done=process_exited_chain(procs,args...); if (done && exitcb != false) exitcb(args...) end))
    closecb2 = make_callback((args...)->(done=process_closed_chain(procs); if (done && closecb != false) closecb(args...) end))
    procs.a = spawn(cmd.a, in, out, exitcb2, closecb2)
    procs.b = spawn(cmd.b, in, out, exitcb2, closecb2)
    return procs
end
spawn(cmds::AndCmd)=spawn(cmds,false,false)
spawn(cmds::AndCmd,in::StreamOrNot)=spawn(cmds,in,false)
spawn(cmds::AndCmd,in::StreamOrNot,out::StreamOrNot)=spawn(cmds,in,out,false,false)

#returns a pipe to read from the last command in the pipelines
read_from(cmds::AbstractCmd)=read_from(cmds,true)
function read_from(cmds::AbstractCmd,passStdin::Bool)
    out=make_pipe()
    _init_buf(out) #create buffer for reading
    if(passStdin)
        processes=spawn(cmds,false,out)
    else
        dummy=make_pipe()
        processes=spawn(cmds,dummy,out)
        close(dummy)
    end
    ccall(:jl_start_reading,Bool,(Ptr{Void},Ptr{Void},Ptr{Void}),out.ihandle,out.buf.ios,C_NULL)
    (out,processes)
end

function change_readcb(stream::AsyncStream,readcb::Function)
    ccall(:jl_change_readcb,Int16,(Ptr{Void},Function),stream.handle,readcb)
end

function write_to(cmds::AbstractCmd)
    in=make_pipe();
    spawn(cmds,in)
    in
end

_jl_kill(p::Process,signum::Int32) = ccall(:uv_process_kill,Int32,(Ptr{Void},Int32),p.handle,signum)
_jl_kill(p::Process)=_jl_kill(p,int32(9))
kill(p::Process) = _jl_kill(p)
function kill(ps::AbstractProcess)
    #TODO: kill stuff
    ps.breakEventLoop=false
    for p in ps.siblings
        if p.pipeline != false then
            kill(p)
        end
        if(p.exit_code==-2)
            _jl_kill(p)
        end
    end
    if ps.pipeline != false
        kill(ps.pipeline)
    end
end

function readall(cmds::AbstractCmd)
    (out,ps)=read_from(cmds)
    while isa(ps.b, OrProcess)
        ps=ps.b
    end
    ps.breakEventLoop=true
    try
        run_event_loop(localEventLoop())
    catch e
        ps.breakEventLoop=false
        kill(ps)
        throw(e)
    end
    return takebuf_string(out.buf)
end

function _contains_newline(bufptr::Ptr{Void},len::Int32)
    return (ccall(:memchr,Ptr{Uint8},(Ptr{Void},Int32,Uint),bufptr,'\n',len)!=C_NULL)
end


function linebuffer_cb(cb::Function,stream::AsyncStream,handle::Ptr,nread::PtrSize,base::Ptr,buflen::Int32)
    if(!isa(stream.buf,IOStream))
        error("Linebuffering only supported on membuffered ASyncStreams")
    end
    if(nread>0)
        #search for newline
        pd::Ptr{Uint8} = ccall(:memchr,Ptr{Uint8},(Ptr{Uint8},Int32,PtrSize),box(Ptr{Uint8},unbox(Int,base)),'\n',nread)
        if(pd!=C_NULL)
            #newline found - split buffer
            to=memio()
            ccall(:ios_splitbuf,Void,(Ptr{Void},Ptr{Void},Ptr{Uint8}),to.ios,stream.buf.ios,pd)
            cb(stream,takebuf_string(to))
        end
    end
end

##TODO do properly
function success(cmd::Cmd)
    proc = spawn(cmd)
    run_event_loop()
    return (proc.exit_code==0)
end
function success(cmds::AbstractCmd)
    procs = spawn(cmds)
    run_event_loop()
    #TODO: check recursively on all processess launched?
    if (p.exit_code!=0)
        return false
    end
    return true
end
function run(args...)
    ps=spawn(args...)
    #TODO: ?
    while ps.pipeline != false
        ps=ps.pipeline
    end
    ps.breakEventLoop=true
    try
        run_event_loop(localEventLoop())
    catch e
        ps.breakEventLoop=false
        kill(ps)
        throw(e)
    end
end


function exec(thunk::Function)
    try
        thunk()
    catch e
        show(e)
        exit(0xff)
    end
    exit(0)
end

## process status ##

function _jl_pre_exec(args::Vector{ByteString})
    if length(args) < 1
        error("exec: too few words to exec")
    end
    ptrs = Array(Ptr{Uint8}, length(args)+1)
    for i = 1:length(args)
        ptrs[i] = args[i].data
    end
    ptrs[length(args)+1] = C_NULL
    return ptrs
end

## implementation of `cmd` syntax ##

arg_gen(x::String) = ByteString[x]

function arg_gen(head)
    if applicable(start,head)
        vals = ByteString[]
        for x in head
            push(vals,cstring(x))
        end
        return vals
    else
        return ByteString[cstring(head)]
    end
end

function arg_gen(head, tail...)
    head = arg_gen(head)
    tail = arg_gen(tail...)
    vals = ByteString[]
    for h = head, t = tail
        push(vals, cstring(strcat(h,t)))
    end
    vals
end

function cmd_gen(parsed)
    args = ByteString[]
    for arg in parsed
        append!(args, arg_gen(arg...))
    end
    Cmd(args)
end

macro cmd(str)
    :(cmd_gen($_jl_shell_parse(str)))
end

## low-level calls

write(s::AsyncStream, b::ASCIIString) =
    ccall(:jl_puts, Int32, (Ptr{Uint8},Ptr{Void}),b.data,s.handle)

write(s::AsyncStream, b::Uint8) =
    ccall(:jl_putc, Int32, (Uint8, Ptr{Void}), b,s.handle)

write(s::AsyncStream, c::Char) =
    ccall(:jl_pututf8, Int32, (Ptr{Void},Char), s.handle,c)

function write{T}(s::AsyncStream, a::Array{T})
    if isa(T,BitsKind)
        ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint32),s.handle, a, uint(numel(a)*sizeof(T)))
    else
        invoke(write, (Any, Array), s, a)
    end
end

function write(s::AsyncStream, p::Ptr, nb::Integer)
    ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint),s.handle, p, uint(nb))
end

function _write(s::AsyncStream, p::Ptr{Void}, nb::Integer)
    ccall(:jl_write, Uint,(Ptr{Void}, Ptr{Void}, Uint),s.handle,p,uint(nb))
end

(&)(left::AbstractCmd,right::AbstractCmd) = AndCmd(left,right)
(|)(src::AbstractCmd,dest::AbstractCmd) = OrCmd(src,dest)

show(io,cmd::AbstractCmd) = show(io, cmd, false)
#TODO: rewrite show
#function show(io, cmd::Cmd, embedded::Bool)
#    if cmd.pipeline != false && embedded
#        print(io,'(')
#    end
#    if isa(cmd.exec,Vector{ByteString})
#        esc = shell_escape(cmd.exec...)
#        print(io,'`')
#        for c in esc
#            if c == '`'
#                print(io,'\\')
#            end
#            print(io,c)
#        end
#        print(io,'`')
#    else
#        print(io, cmd.exec)
#    end
#    if cmd.pipeline != false
#        print(io," | ")
#        show(io,cmd.pipeline, isa(cmd,Cmds))
#        if embedded
#            print(io,')')
#        end
#    end
#end
#function show(io,cmds::Cmds, embedded::Bool)
#    if cmds.pipeline != false && embedded
#        print(io,'(')
#    end
#    if length(cmds.siblings) > 1 || (embedded || cmds.pipeline != false)
#        print(io,'(')
#    end
#    first = true
#    for cmd = cmds.siblings
#        if !first
#            print(io," & ")
#        end
#        show(io, cmd, true)
#        first = false
#    end
#    if length(cmds.siblings) > 1 || (embedded || cmds.pipeline != false)
#        print(io,')')
#    end
#    if cmds.pipeline != false
#        print(io, " | ")
#        show(io, cmds.pipeline, false)
#        if embedded
#            print(io, ')')
#        end
#    end
#end

_jl_connect_raw(sock::TcpSocket,sockaddr::Ptr{Void},cb::Function) = ccall(:jl_connect_raw,Int32,(Ptr{Void},Ptr{Void},Function),sock.handle,sockaddr,cb)
_jl_getaddrinfo(loop::Ptr,host::ByteString,service::Ptr,cb::Function) = ccall(:jl_getaddrinfo,Int32,(Ptr{Void},Ptr{Uint8},Ptr{Uint8},Function),loop,host,service,cb)
_jl_sockaddr_from_addrinfo(addrinfo::Ptr) = ccall(:jl_sockaddr_from_addrinfo,Ptr{Void},(Ptr,),addrinfo)
_jl_sockaddr_set_port(ptr::Ptr{Void},port::Uint16) = ccall(:jl_sockaddr_set_port,Void,(Ptr{Void},Uint16),ptr,port)
_uv_lasterror(loop::Ptr{Void}) = ccall(:jl_last_errno,Int32,(Ptr{Void},),loop)

function connect_callback(sock::TcpSocket,status::Int32,breakLoop::Bool)
    if(status==-1)
        error("Socket connection failed: ",_uv_lasterror(globalEventLoop()))
    end
    sock.open=true;
    if(breakLoop)
        break_one_loop(globalEventLoop())
    end
end

function getaddrinfo_callback(breakLoop::Bool,sock::TcpSocket,status::Int32,port::Uint16,addrinfo_list::Ptr)
    if(status==-1)
        error("Name lookup failed")
    end
    sockaddr = _jl_sockaddr_from_addrinfo(addrinfo_list) #only use first entry of the list for now
    _jl_sockaddr_set_port(sockaddr,hton(port))
    err = _jl_connect_raw(sock,sockaddr,(req::Ptr,status::Int32)->connect_callback(sock,status,breakLoop))
    if(err != 0)
        error("Failed to connect to host")
    end
end

function readuntil(s::IOStream, delim::Uint8)
    a = ccall(:jl_readuntil, Any, (Ptr{Void}, Uint8), s.ios, delim)
    # TODO: faster versions that avoid this encoding check
    ccall(:jl_array_to_string, Any, (Any,), a)::ByteString
end

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end

readline(s::IOStream) = readuntil(s, uint8('\n'))


function connect_to_host(host::ByteString,port::Uint16)
    sock = TcpSocket(_jl_tcp_init(globalEventLoop()))
    err = _jl_getaddrinfo(globalEventLoop(),host,C_NULL,(addrinfo::Ptr,status::Int32)->getaddrinfo_callback(true,sock,status,port,addrinfo))
    if(err!=0)
        error("Failed to  initilize request to resolve hostname: ",host)
    end
    run_event_loop(globalEventLoop())
    return sock
end
