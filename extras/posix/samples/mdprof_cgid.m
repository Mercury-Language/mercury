%------------------------------------------------------------------------------%
% Copyright (C) 2007, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: mdprof_cgid.
% Author: wangp.
%
% This is a minimal server that accepts and passes through requests to
% mdprof.  It's useful when you want to view deep profiling data without
% configuring and running a proper web server.
%
%-----------------------------------------------------------------------------%

:- module mdprof_cgid.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bitmap.
:- import_module char.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module posix.
:- import_module posix.dup.
:- import_module posix.read.
:- import_module posix.open.
:- import_module posix.select.
:- import_module posix.socket.

%-----------------------------------------------------------------------------%
%
% Command-line options
%

:- type option
    --->    server_name
    ;       server_port
    ;       mdprof_path
    ;       script_name.

:- type data
    --->    data(
                data_mdprof_path    :: string,
                data_script_name    :: string
            ).

:- pred short_option(char::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_default(option::out, option_data::out) is multi.
:- pred option_defaults(option::out, option_data::out) is nondet.
:- pred show_usage(io::di, io::uo) is det.

short_option('h',               server_name).
short_option('p',               server_port).
short_option('m',               mdprof_path).
short_option('s',               script_name).

long_option("host",             server_name).
long_option("name",             server_name).
long_option("port",             server_port).
long_option("mdprof",           mdprof_path).
long_option("script-name",      script_name).

option_default(server_name,     string("localhost")).
option_default(server_port,     int(8081)).
option_default(mdprof_path,     string("mdprof")).
option_default(script_name,     string("/cgi-bin/mdprof_cgi")).

option_defaults(Option, Default) :-
    option_default(Option, Default),
    semidet_true.

show_usage(!IO) :-
    list.foldl(io.write_string, [
        "Options:\n",
        "    -h, --host HOSTNAME    (default: localhost)\n",
        "    -p, --port PORT        (default: 8081)\n",
        "    -m, --mdprof PATH      (default: mdprof)\n",
        "    -s, --script-name NAME (default: /cgi-bin/mdprof_cgi)\n",
        "\n"
    ], !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args0, !IO),
    OptionOpts = option_ops(short_option, long_option, option_defaults),
    getopt.process_options(OptionOpts, Args0, _Args, OptionsResults),
    (
        OptionsResults = ok(OptTable),
        main_2(OptTable, !IO)
    ;
        OptionsResults = error(OptionError),
        OptionErrorString = option_error_to_string(OptionError),
        io.write_string(OptionErrorString, !IO),
        io.nl(!IO),
        show_usage(!IO),
        io.set_exit_status(1, !IO)
    ).

:- pred main_2(option_table(option)::in, io::di, io::uo) is det.

main_2(OptTable, !IO) :-
    %
    % Handle command-line options.
    %
    getopt.lookup_string_option(OptTable, server_name, ServerName),
    getopt.lookup_int_option(OptTable, server_port, ServerPort),
    getopt.lookup_string_option(OptTable, script_name, ScriptName0),
    ( if string.prefix(ScriptName0, "/") then
        ScriptName = ScriptName0
    else
        ScriptName = "/" ++ ScriptName0
    ),
    getopt.lookup_string_option(OptTable, mdprof_path, MdprofPath),
    %
    % Set CGI environment variables.
    %
    io.set_environment_var("SERVER_NAME", ServerName, !IO),
    io.set_environment_var("SERVER_PORT", from_int(ServerPort), !IO),
    io.set_environment_var("SCRIPT_NAME", ScriptName, !IO),
    %
    % Listen for requests.
    %
    create_listen_conn(ServerPort, ListenFd, !IO),
    io.format("%s started on %s:%d\n",
        [s(prog_name), s(ServerName), i(ServerPort)], !IO),
    Data = data(MdprofPath, ScriptName),
    main_loop(Data, ListenFd, !IO).

:- pred create_listen_conn(int::in, fd::out, io::di, io::uo) is det.

create_listen_conn(ServerPort, Fd, !IO) :-
    socket(inet, stream, protocol(0), SocketResult, !IO),
    (
        SocketResult = ok(Fd)
    ;
        SocketResult = error(SocketError),
        abort_with_error("socket failed", SocketError)
    ),
    bind(Fd, inet(port(ServerPort), inet_addr(0)), BindResult, !IO),
    (
        BindResult = ok
    ;
        BindResult = error(_BindError),
        abort("bind failed (perhaps port " ++ string(ServerPort) ++
            " is already in use?)")
    ),
    listen(Fd, 1, ListenResult, !IO),
    (
        ListenResult = ok
    ;
        ListenResult = error(ListenError),
        abort_with_error("listen failed", ListenError)
    ).

:- pred main_loop(data::in, fd::in, io::di, io::uo) is det.

main_loop(Data, ListenFd, !IO) :-
    await_conn(ListenFd, ConnFd, !IO),
    handle_conn(Data, ConnFd, !IO),
    main_loop(Data, ListenFd, !IO).

:- pred await_conn(fd::in, fd::out, io::di, io::uo) is det.

await_conn(ListenFd, ConnFd, !IO) :-
    wait_input_fd(ListenFd, SelectResult, !IO),
    (
        SelectResult = ok(_)
    ;
        SelectResult = error(SelectError),
        abort_with_error("wait_input_fd failed", SelectError)
    ),
    accept(ListenFd, AcceptResult, !IO),
    (
        AcceptResult = ok(ConnFd)
    ;
        AcceptResult = error(AcceptError),
        abort_with_error("accept failed", AcceptError)
    ).

:- pred wait_input_fd(fd::in, posix.result(int)::out, io::di, io::uo) is det.

wait_input_fd(Fd @ fd(MaxFd), Result, !IO) :-
    new_fdset_ptr(Rd, !IO),
    new_fdset_ptr(Wr, !IO),
    new_fdset_ptr(Ex, !IO),
    fd_set(Fd, Rd, !IO),
    LongTime = timeval(9999, 0),
    select(MaxFd+1, Rd, Wr, Ex, LongTime, Result, !IO).

:- pred handle_conn(data::in, fd::in, io::di, io::uo) is det.

handle_conn(Data, ConnFd, !IO) :-
    TextSize = 1024,
    Text0 = bitmap.init(TextSize * bits_per_int),
    read(ConnFd, TextSize, ReadResult, Text0, Text, !IO),
    (
        ReadResult = ok(Length),
        Request = first_line(Text, Length),
        with_stdout(ConnFd, handle_request(Data, Request), !IO)
    ;
        ReadResult = error(_),
        io.print("read failed\n", !IO)
    ),
    close(ConnFd, _, !IO).

    % with_stdout(OtherFd, P, !IO)
    % Call P, with OtherFd substituted for standard output during
    % the execution of P.
    %
:- pred with_stdout(fd::in, pred(io, io)::in(pred(di, uo) is det),
    io::di, io::uo) is det.

with_stdout(OtherFd, P, !IO) :-
    Stdout = fd(1),
    dup(Stdout, StdoutCopy, !IO),
    dup2(OtherFd, Stdout, _, !IO),
    P(!IO),
    close(Stdout, _, !IO),
    (
        StdoutCopy = ok(Stdout1),
        dup2(Stdout1, Stdout, _, !IO)
    ;
        StdoutCopy = error(_)
    ).

:- pred handle_request(data::in, string::in, io::di, io::uo) is det.

handle_request(Data, Request, !IO) :-
    (if string.words(Request) = [Method, Path, HttpProt] then
        (if http_protocol(HttpProt) then
            (if string.to_upper(Method) = "GET" then
                (if string.prefix(Path, Data ^ data_script_name) then
                    handle_request_2(Data, Path, !IO)
                else
                    write_http_response("404", "Not found", !IO)
                )
            else
                write_http_response("501", "Not implemented", !IO)
            )
        else
            write_http_response("505", "HTTP Version Not Supported", !IO)
        )
    else
        write_http_response("400", "Bad request", !IO)
    ).

:- pred handle_request_2(data::in, string::in, io::di, io::uo) is det.

handle_request_2(Data, Path, !IO) :-
    %
    % Set up the QUERY_STRING CGI environment variable.
    %
    (if string.sub_string_search(Path, "?", QuesIndex) then
        Length = string.length(Path),
        QueryString = string.between(Path, QuesIndex + 1, Length)
    else
        QueryString = ""
    ),
    io.set_environment_var("QUERY_STRING", QueryString, !IO),
    %
    % Write the HTTP header and call the mdprof program to generate
    % the rest of the output.
    %
    io.write_string("HTTP/1.0 200 OK\r\n", !IO),
    MdprofPath = Data ^ data_mdprof_path,
    io.call_system(MdprofPath, CgiResult, !IO),
    (
        CgiResult = ok(_)
    ;
        CgiResult = error(Error),
        io.error_message(Error, ErrorMsg),
        abort("call_system failed: " ++ ErrorMsg)
    ).

:- pred http_protocol(string::in) is semidet.

http_protocol("HTTP/1.0").
http_protocol("HTTP/1.1").

:- pred write_http_response(string::in, string::in, io::di, io::uo)
    is det.

write_http_response(StatusCode, Message, !IO) :-
    list.foldl(io.write_string, [
        "HTTP/1.0 ", StatusCode, " OK\r\n",
        "Content-Type: text/html\r\n\r\n",
        "<html><body><p>", Message, "</p></body></html>\n"
    ], !IO).

%-----------------------------------------------------------------------------%

    % first_line(Text, TextLen) = String
    % Return the first line from the bitmap object as a string,
    % i.e. everything up to the first CR or NL character.
    %
:- func first_line(bitmap, int) = string.

first_line(Text, TextLen) =
    string.from_char_list(first_line_2(Text, 0, TextLen)).

:- func first_line_2(bitmap, int, int) = list(char).

first_line_2(Text, Index, TextLen) = RevChars :-
    ( if Index >= TextLen then
        RevChars = []
    else
        Char = char.det_from_int(Text ^ byte(Index)),
        ( if is_newline(Char) then
            RevChars = []
        else
            RevChars = [Char | first_line_2(Text, Index + 1, TextLen)]
        )
    ).

:- pred is_newline(char::in) is semidet.

is_newline('\n').
is_newline('\r').

%-----------------------------------------------------------------------------%

:- pred abort(string::in) is erroneous.

abort(Msg) :-
    error(prog_name ++ ": " ++ Msg).

:- pred abort_with_error(string::in, posix.error::in) is erroneous.

abort_with_error(Msg, Error) :-
    Str = string.format("%s: %s %s",
        [s(prog_name), s(Msg), s(string(Error))]),
    error(Str).

:- func prog_name = string.

prog_name = "mdprof_cgid".

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
