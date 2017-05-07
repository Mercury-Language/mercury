%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2007, 2011 The University of Melbourne
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%-----------------------------------------------------------------------------%
%
% Module:   tcp
% Main Author:  peter.ross@miscrit.be (based on code written by pma@miscrit.be)
% Stability:    low
%
% An implementation of TCP streams.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module net.tcp.
:- interface.

:- import_module io.
:- import_module stream.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type tcp.
:- type bound_tcp.

:- type tcp.result(T)
    --->    ok(T)
    ;       error(string).

:- type host     == string. % A hostname    ie "localhost"
:- type service  == string. % A service     ie "www"
:- type protocol == string. % A protocol    ie "tcp"
:- type port     == int.    % A portnumber  ie 80 - the webserver

:- pred tcp.connect(host::in, port::in, tcp.result(tcp)::out, io::di, io::uo)
    is det.

:- pred tcp.bind(host::in, port::in, tcp.result(bound_tcp)::out,
    io::di, io::uo) is det.

:- pred tcp.accept(bound_tcp::in, tcp.result(tcp)::out, io::di, io::uo) is det.

:- pred tcp.shutdown(tcp::in, io::di, io::uo) is det.

    % Accesses the stream to see if there is data available, waits for a given
    % period before timing out.  (Use this rather than a failure driven test
    % and loop on connects.)
    %
:- pred tcp.data_available(bound_tcp::in, int::in, int::out, io::di, io::uo)
    is det.

:- func socket_fd(tcp) = int.

    % Sending data to a broken pipe will cause the SIGPIPE signal to be
    % sent to the process.  If SIGPIPE is ignored or blocked then send()
    % fails with EPIPE.  This predicate causes SIGPIPE signals to be
    % ignored.
    %
:- pred tcp.ignore_sigpipe(io::di, io::uo) is det.

    % Restores the SIGPIPE signal handler before the last
    % tcp.ignore_sigpipe() call.
    %
:- pred tcp.unignore_sigpipe(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Stream type class instances
%

:- type tcp.error.

:- instance stream(tcp, io.state).
:- instance error(tcp.error).

:- instance input(tcp, io.state).
:- instance reader(tcp, character, io.state, tcp.error).
:- instance reader(tcp, line, io.state, tcp.error).

:- instance output(tcp, io.state).
:- instance writer(tcp, character, io.state).
:- instance writer(tcp, string, io.state).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type tcp
    --->    tcp(
                name    :: string,
                handle  :: tcp_handle
            ).

:- type bound_tcp
    --->    bound_tcp(
                int,        % socket fd
                c_pointer   % struct sockaddr
            ).

%-----------------------------------------------------------------------------%

tcp.connect(Host, Port, Result, !IO) :-
    handle_connect(Host, Port, Handle, Errno, !IO),
    ( Errno = 0 ->
        Result = ok(tcp(Host, Handle))
    ;
        Result = tcp.error(tcp.error_message(Errno))
    ).

%-----------------------------------------------------------------------------%

tcp.bind(Host, Port, Result, !IO) :-
    handle_bind(Host, Port, Socket, Addr, Errno, !IO),
    ( Errno = 0 ->
        Result = ok(bound_tcp(Socket, Addr))
    ;
        Result = tcp.error(tcp.error_message(Errno))
    ).

%-----------------------------------------------------------------------------%

tcp.accept(bound_tcp(Socket, Addr), Result, !IO) :-
    handle_accept(Socket, Addr, Handle, Errno, !IO),
    ( Errno = 0 ->
        Result = ok(tcp("XXX unknown host", Handle))
    ;
        Result = tcp.error(tcp.error_message(Errno))
    ).

%-----------------------------------------------------------------------------%

tcp.shutdown(tcp(_, Handle), !IO) :-
    handle_shutdown(Handle, !IO).

:- pred handle_shutdown(tcp_handle::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    handle_shutdown(TCP::in, _IO0::di, _IO::uo),
    [may_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    struct linger sockets_linger = { MR_TRUE, 2 };
    ML_tcp *sock;
    int shutdown_erro = 0;

    sock = (ML_tcp *) TCP;

/*  setsockopt(sock->socket, SOL_SOCKET, SO_LINGER,
            &sockets_linger, sizeof(sockets_linger));*/

    errno=0;
    if (close(((int)sock->socket)) == SOCKET_ERROR) {
        ML_throw_tcp_exception((MR_String) ""tcp.shutdown failed (close)"");
    }
").

%-----------------------------------------------------------------------------%

:- type tcp_handle
    --->    socket(c_pointer).

:- pragma foreign_decl("C", "
#ifdef MR_WIN32
    #include ""mercury_windows.h""
    #include <winsock2.h>
    #include <ws2tcpip.h>
    #include <sys/types.h>

    #define  ML_error()       WSAGetLastError()

#else /* !MR_WIN32 */

    #include <errno.h>
    #include <unistd.h>
    #include <netdb.h>

    #include <netinet/in.h>

    #include <sys/types.h>
    #include <sys/socket.h>

    #define  ML_error()       errno

    #define  INVALID_SOCKET   -1
    #define  SOCKET_ERROR     -1
#endif /* !MR_WIN32 */

#define ADDRLEN 16
#define BACKLOG 16
#define FULL    2

#define TCP_BUFSIZE     1024

typedef struct {
    int socket;
    int error;
    size_t  buf_len;
    off_t   buf_pos;
    char    buf[TCP_BUFSIZE];
} ML_tcp;

void ML_tcp_init(void);
").

:- pragma foreign_code("C", "
/*
** We must ensure that the socket DLL is initialiased before use under
** Win32.
*/
void ML_tcp_init(void)
{
  #ifdef MR_WIN32
    static int initialiased = MR_FALSE;

    WORD    wVersionRequested;
    WSADATA wsaData;
    int     err;

    if (!initialiased) {
        wVersionRequested = MAKEWORD( 2, 2 );
        err = WSAStartup(wVersionRequested, &wsaData);

        if ( err != 0 ) {
            MR_fatal_error(""Unable to find a usable winsock.dll\\n"");
        }

        if ( LOBYTE( wsaData.wVersion ) != 2 ||
                HIBYTE( wsaData.wVersion ) != 2 ) {
            WSACleanup();
            MR_fatal_error(""Unable to find a usable winsock.dll\\n"");
        }
        initialiased = MR_TRUE;
    }
  #endif /* MR_WIN32 */
}
").

:- pred handle_connect(string::in, port::in, tcp_handle::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    handle_connect(Host::in, Port::in, TCP::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_tcp              *sock;
    struct hostent      *host;
    struct sockaddr_in  *addr;

    ML_tcp_init();

    sock = MR_GC_NEW(ML_tcp);

    sock->socket = socket(PF_INET, SOCK_STREAM, 0);
    sock->error = 0;
    sock->buf_len = 0;
    sock->buf_pos = 0;

    if (sock->socket == INVALID_SOCKET) {
        sock->error = ML_error();
    } else {
        host = gethostbyname(Host);
        if (host == NULL) {
            sock->error = ML_error();
        } else {
            addr = MR_GC_NEW(struct sockaddr_in);
            MR_memset(addr, 0, sizeof(struct sockaddr_in));
            MR_memcpy(&(addr->sin_addr), host->h_addr_list[0], host->h_length);
            addr->sin_family = host->h_addrtype;
            addr->sin_port = htons(Port);
            /*
            ** MR_memset(&addr, 0, sizeof(addr));
            ** MR_memcpy((char *)&addr.sin_addr, host->h_addr, host->h_length);
            ** addr.sin_family = host->h_addrtype;
            ** addr.sin_port = htons(Port);
            */

            if (connect(sock->socket, (struct sockaddr *)addr, ADDRLEN) ==
                    SOCKET_ERROR)
            {
                sock->error = ML_error();
            }
        }
    }

    Errno = sock->error;
    TCP = (MR_Word) sock;
").

socket_fd(Tcp) = socket_fd_c(Tcp ^ handle).

:- func socket_fd_c(tcp_handle) = int.
:- pragma foreign_proc("C",
    socket_fd_c(Tcp::in) = (FD::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ML_tcp *sock = (ML_tcp *) Tcp;
    FD = sock->socket;
").

%-----------------------------------------------------------------------------%

:- pred handle_bind(string::in, port::in, int::out, c_pointer::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    handle_bind(Host::in, Port::in, Socket::out, Addr::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    struct hostent      *host = NULL;
    struct sockaddr_in  *addr = NULL;

    ML_tcp_init();

    Socket = socket(PF_INET, SOCK_STREAM, 0);
    Errno = 0;

    if (Socket == INVALID_SOCKET) {
        Errno = ML_error();
    } else {
        host = gethostbyname(Host);
        if (host == NULL) {
            Errno = ML_error();
        } else {
            addr = MR_GC_NEW(struct sockaddr_in);
            MR_memset(addr, 0, sizeof(struct sockaddr_in));
            MR_memcpy(&(addr->sin_addr), host->h_addr_list[0], host->h_length);
            addr->sin_family = host->h_addrtype;
            addr->sin_port = htons(Port);

            if (bind(Socket, (struct sockaddr *) addr, ADDRLEN)
                    == SOCKET_ERROR) {
                Errno = ML_error();
            } else {
                if (listen(Socket, BACKLOG) == SOCKET_ERROR) {
                    Errno = ML_error();
                }
            }
        }
    }

    Addr = (MR_Word) addr;
").

:- pred handle_accept(int::in, c_pointer::in, tcp_handle::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    handle_accept(Socket::in, Addr::in, TCP::out, Errno::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_tcp              *sock;
    struct sockaddr     *addr;

    /*
    ** For Winsock the third argument of accept is pointer to a signed
    ** int.  On POSIX, systems, it has type socklen_t which is unsigned.
    */
    #if defined(MR_WIN32)
        int size = sizeof(struct sockaddr_in);
    #else
        socklen_t size = sizeof(struct sockaddr_in);
    #endif

    sock = MR_GC_NEW(ML_tcp);
    addr = (struct sockaddr *) Addr;

    sock->socket = accept(Socket, addr, &size);
    sock->error = 0;
    sock->buf_len = 0;
    sock->buf_pos = 0;

    if (sock->socket == INVALID_SOCKET) {
        sock->error = ML_error();
    }

    TCP = (MR_Word) sock;
    Errno = sock->error;
").

%-----------------------------------------------------------------------------%

:- type tcp.error
    --->    errno(int).

:- instance stream(tcp, io.state) where [
    name(TCP, TCP ^ name, !IO)
].

:- instance error(tcp.error) where [
    (error_message(E) = S :-
        get_error(E, S)
    )
].

:- instance input(tcp, io.state) where [].

:- instance reader(tcp, character, io.state, tcp.error) where [
    (get(T, Result, !IO) :-
        tcp.read_char(T ^ handle, Char, !IO),
        ( Char = -1 ->
            Result = eof
        ; Char = -2 ->
            get_errno(T ^ handle, Errno, !IO),
            Result = error(Errno)
        ;
            Result = ok(char.det_from_int(Char))
        )
    )
].

:- instance reader(tcp, line, io.state, tcp.error) where [
    (get(T, Result, !IO) :-
        tcp.read_line_as_string_2(T ^ handle, ErrCode, String, !IO),
        ( ErrCode = -1 ->
            Result = eof
        ; ErrCode = -2 ->
            get_errno(T ^ handle, Errno, !IO),
            Result = error(Errno)
        ;
            Result = ok(line(String))
        )
    )
].

:- instance output(tcp, io) where [
        % XXX can one flush a socket?
    flush(_, !IO)
].

:- instance writer(tcp, character, io.state) where [
    (put(T, C, !IO) :-
        tcp.write_char(T ^ handle, C, B, !IO),
        ( B = yes,
            true
        ; B = no,
            get_errno(T ^ handle, Errno, !IO),
            get_error(Errno, String),
            error("put(char): " ++ String)
        )
    )
].

:- instance writer(tcp, string, io.state) where [
    (put(T, S, !IO) :-
        tcp.write_string(T ^ handle, S, B, !IO),
        ( B = yes,
            true
        ; B = no,
            get_errno(T ^ handle, Errno, !IO),
            get_error(Errno, String),
            error("put(string): " ++ String)
        )
    )
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    /*
    ** Note: some Mercury code uses the -1 and -2 constants directly.
    */
    #define TCP_EOF     -1
    #define TCP_ERROR   -2

    int TCP_get_char(ML_tcp *sock);
").

:- pragma foreign_code("C", "
    int TCP_get_char(ML_tcp *sock)
    {
        if (sock->buf_pos >= sock->buf_len) {
            /* Refill buffer. */
            int nchars = recv(sock->socket,
                sock->buf, sizeof(sock->buf), 0);
            if (nchars == SOCKET_ERROR) {
                sock->error = ML_error();
                return TCP_ERROR;
            } else if (nchars == 0) {
                return TCP_EOF;
            } else {
                sock->buf_pos = 1;
                sock->buf_len = nchars;
                return sock->buf[0];
            }
        } else {
            return sock->buf[sock->buf_pos++];
        }
    }
").

:- pred tcp.read_char(tcp_handle::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    tcp.read_char(Socket::in, Chr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_tcp *sock = (ML_tcp *) Socket;
    Chr = TCP_get_char(sock);
").

    % This implementation is based on io.read_line_as_string_2.
    %
:- pred tcp.read_line_as_string_2(tcp_handle::in, int::out, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("C",
    tcp.read_line_as_string_2(TCP::in, Res::out, RetString::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#define TCP_IO_READ_LINE_GROW(n) ((n) * 3 / 2)
#define TCP_IO_BYTES_TO_WORDS(n) (((n) + sizeof(MR_Word) - 1) / sizeof(MR_Word))
#define TCP_IO_READ_LINE_START   1024

    ML_tcp *sock = (ML_tcp *) TCP;
    MR_Char initial_read_buffer[TCP_IO_READ_LINE_START];
    MR_Char *read_buffer = initial_read_buffer;
    size_t read_buf_size = TCP_IO_READ_LINE_START;
    size_t i;
    int char_code = '\\0';

    Res = 0;
    for (i = 0; char_code != '\\n'; ) {
        char_code = TCP_get_char(sock);
        if (char_code == TCP_EOF) {
            if (i == 0) {
                Res = -1;
            }
            break;
        }
        if (char_code == TCP_ERROR) {
            Res = -2;
            break;
        }
        read_buffer[i++] = char_code;
        MR_assert(i <= read_buf_size);
        if (i == read_buf_size) {
            /* Grow the read buffer */
            read_buf_size = TCP_IO_READ_LINE_GROW(read_buf_size);
            if (read_buffer == initial_read_buffer) {
                read_buffer = MR_NEW_ARRAY(MR_Char, read_buf_size);
                MR_memcpy(read_buffer, initial_read_buffer,
                    TCP_IO_READ_LINE_START);
            } else {
                read_buffer = MR_RESIZE_ARRAY(read_buffer, MR_Char,
                    read_buf_size);
            }
        }
    }
    if (Res == 0) {
        MR_Word ret_string_word;
        MR_offset_incr_hp_atomic_msg(ret_string_word,
            0, TCP_IO_BYTES_TO_WORDS((i + 1) * sizeof(MR_Char)),
            MR_ALLOC_ID, ""string.string/0"");
        RetString = (MR_String) ret_string_word;
        MR_memcpy(RetString, read_buffer, i * sizeof(MR_Char));
        RetString[i] = '\\0';
    } else {
        /*
        ** We can't just return NULL here, because  otherwise mdb will break
        ** when it tries to print the string.
        */
        RetString = MR_make_string_const("""");
    }
    if (read_buffer != initial_read_buffer) {
        MR_free(read_buffer);
    }
").

:- pred tcp.write_char(tcp_handle::in, char::in, bool::out,
    io::di, io::uo) is det.
:- pragma foreign_proc(c,
    tcp.write_char(Socket::in, Chr::in, Success::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_tcp *sock = (ML_tcp *) Socket;

    if (send(sock->socket, &Chr, 1, 0) == SOCKET_ERROR) {
        sock->error = ML_error();
        Success = MR_FALSE;
    } else {
        Success = MR_TRUE;
    }
").

:- pred tcp.write_string(tcp_handle::in, string::in, bool::out,
    io::di, io::uo) is det.
:- pragma foreign_proc(c,
    tcp.write_string(Socket::in, Str::in, Success::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_tcp *sock = (ML_tcp *) Socket;

    if (send(sock->socket, Str, strlen(Str), 0) == SOCKET_ERROR) {
        sock->error = ML_error();
        Success = MR_NO;
    } else {
        Success = MR_YES;
    }
").

:- pred get_errno(tcp_handle::in, tcp.error::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    get_errno(Socket::in, Errno::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
    ML_tcp *sock = (ML_tcp *) Socket;
    Errno = sock->error;
").

:- pred tcp.get_error(tcp.error::in, string::out) is det.
:- pragma foreign_proc("C",
    tcp.get_error(Errno::in, Msg::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    char errbuf[MR_STRERROR_BUF_SIZE];

    MR_save_transient_hp();
    MR_make_aligned_string_copy(Msg,
        MR_strerror(Errno, errbuf, sizeof(errbuf)));
    MR_restore_transient_hp();
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    tcp.data_available(Socket::in, Wait::in, Int::out, _IO0::di, _IO::uo),
    [promise_pure, tabled_for_io],
"
    ML_tcp *sock = (ML_tcp *) Socket;
    int selres = 0;
    fd_set readfds, writefds, exceptfds;
        struct timeval *sockets__timeout;
        struct timeval sockets__timeout_struct;

        if ( Wait > 0 ) {
           sockets__timeout = &sockets__timeout_struct;
           sockets__timeout->tv_sec = ((int)Wait * 60);
           sockets__timeout->tv_usec = 0;
        } else {
           sockets__timeout = NULL;
        }

        FD_ZERO(&writefds);
        FD_ZERO(&readfds);
        FD_ZERO(&exceptfds);
        FD_SET(sock->socket,&readfds);
        if ( sockets__timeout != NULL ) {
            /* Do a select to see if something is available......... */
            selres = select(0, &readfds, &writefds, &exceptfds,
                sockets__timeout);
            if ( selres == 0 ) {
                Int = -1;
            } else {
                if ( selres == SOCKET_ERROR ) {
                    Int = -2;
                } else {
                    Int = 0;
                };
            };
        } else {
            Int = 0;
        }
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func error_message(int) = string.

:- pragma foreign_proc("C",
    error_message(Errno::in) = (Err::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    char errbuf[MR_STRERROR_BUF_SIZE];

    MR_make_aligned_string_copy(Err,
        MR_strerror(Errno, errbuf, sizeof(errbuf)));
").

:- pred throw_tcp_exception(string::in) is erroneous.
:- pragma foreign_export("C", throw_tcp_exception(in),
    "ML_throw_tcp_exception").

throw_tcp_exception(S) :-
    error(S).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "

    #include <signal.h>

    #if defined(SIGPIPE)
        extern void *TCP__prev_sigpipe_handler;
     #endif
").

:- pragma foreign_code("C", "
    #if defined(SIGPIPE)
        void *TCP__prev_sigpipe_handler = SIG_DFL;
    #endif
").

:- pragma foreign_proc("C",
    tcp.ignore_sigpipe(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#if defined(SIGPIPE)
    TCP__prev_sigpipe_handler = signal(SIGPIPE, SIG_IGN);
#else
    MR_external_fatal_error(""tcp"", ""SIGPIPE not available on this system."")
#endif
").

:- pragma foreign_proc("C",
    tcp.unignore_sigpipe(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#if defined(SIGPIPE)
    signal(SIGPIPE, TCP__prev_sigpipe_handler);
#else
    MR_external_fatal_error(""tcp"", ""SIGPIPE not available on this system."")
#endif
").

%-----------------------------------------------------------------------------%
:- end_module tcp.
%-----------------------------------------------------------------------------%
