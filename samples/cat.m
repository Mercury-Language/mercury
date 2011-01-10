%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Simple implementation of the standard unix `cat' filter:
% copy input files (or stdin, if no input files) to stdout.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%-----------------------------------------------------------------------------%

:- module cat.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [],
        cat(!IO)
    ;
        Args = [_ | _],
        cat_file_list(Args, !IO)
    ).
 
:- pred cat_file_list(list(string)::in, io::di, io::uo) is det.

cat_file_list([], !IO).
cat_file_list([File | Files], !IO) :-
    cat_file(File, !IO),
    cat_file_list(Files, !IO).

:- pred cat_file(string::in, io::di, io::uo) is det.

cat_file(File, !IO) :-
    io.open_input(File, Result, !IO),
    (
        Result = ok(Stream),
        cat_stream(Stream, !IO),
        io.close_input(Stream, !IO)
    ;
        Result = error(Error),
        io.progname("cat", Progname, !IO),
        io.error_message(Error, Message),
        io.write_strings([
            Progname, ": ",
            "error opening file `", File, "' for input:\n\t",
            Message, "\n"
        ], !IO)
    ).

:- pred cat_stream(io.input_stream::in, io::di, io::uo) is det.

cat_stream(Stream, !IO) :-
    io.set_input_stream(Stream, _OldStream, !IO),
    cat(!IO).

:- pred cat(io::di, io::uo) is det.

cat(!IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = ok(Line),
        io.write_string(Line, !IO),
        cat(!IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, Message),
        io.input_stream_name(StreamName, !IO),
        io.progname("cat", ProgName, !IO),
        io.write_strings([
            ProgName, ": ",
            "error reading input file `", StreamName, "': \n\t",
            Message, "\n"
        ], !IO)
    ).

%-----------------------------------------------------------------------------%
