%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% A simple sorting program.
%
% It works on text files, considering each line to be a record.
% The entire line is considered to be the sort key.
%
% The algorithm used is simple insertion sort.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%-----------------------------------------------------------------------------%

:- module sort.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        (
            Args = [],
            handle_args(no, InputFile, no, OutputFile, !IO)
        ;
            Args = [Input],
            handle_args(yes(Input), InputFile, no, OutputFile, !IO)
        ;
            Args = [Input, Output],
            handle_args(yes(Input), InputFile, yes(Output), OutputFile, !IO)
        ),
        sort(InputFile, OutputFile, !IO)
    ;
        Args = [_, _, _ | _],
        io.write_string("Usage: sort [Input [Output]]\\n", !IO)
    ).

:- pred handle_args(maybe(string)::in, io.text_input_stream::out,
    maybe(string)::in, io.text_output_stream::out, io::di, io::uo)
    is det.

handle_args(InArg, InFile, OutArg, OutFile, !IO) :-
    (
        InArg = yes(InFilename),
        io.open_input(InFilename, InResult, !IO),
        (
            InResult = ok(InFile)
        ;
            InResult = error(InError),
            io.error_message(InError, InMsg),
            error(InMsg)
        )
    ;
        InArg = no,
        io.stdin_stream(InFile, !IO)
    ),
    (
        OutArg = yes(OutFilename),
        io.open_output(OutFilename, OutResult, !IO),
        (
            OutResult = ok(OutFile)
        ;
            OutResult = error(OutError),
            io.error_message(OutError, OutMsg),
            error(OutMsg)
        )
    ;
        OutArg = no,
        io.stdout_stream(OutFile, !IO)
    ).

:- pred sort(io.text_input_stream::in, io.text_output_stream::in,
    io::di, io::uo) is det.

sort(InFile, OutFile, !IO) :-
    sort_2(InFile, OutFile, [], !IO).

:- pred sort_2(io.text_input_stream::in, io.text_output_stream::in,
    list(string)::in, io::di, io::uo) is det.

sort_2(InFile, OutFile, !.Lines, !IO) :-
    io.read_line_as_string(InFile, Result, !IO),
    (
        Result = error(Error),
        io.error_message(Error, Msg),
        error(Msg)
    ;
        Result = eof,
        output_sorted_lines(OutFile, !.Lines, !IO)
    ;
        Result = ok(Line),
        insert_line(Line, !Lines),
        sort_2(InFile, OutFile, !.Lines, !IO)
    ).

:- pred insert_line(string::in, list(string)::in, list(string)::out) is det.

insert_line(I, [], [I]).
insert_line(I, [H | T], L) :-
    compare(R, I, H),
    (
        R = (<),
        L = [I, H | T]
    ;
        ( R = (=)
        ; R = (>)
        ),
        insert_line(I, T, NT),
        L = [H | NT]
    ).

:- pred output_sorted_lines(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

output_sorted_lines(_, [], !IO).
output_sorted_lines(OutFile, [Line | Lines], !IO) :-
    io.write_string(OutFile, Line, !IO),
    output_sorted_lines(OutFile, Lines, !IO).

%-----------------------------------------------------------------------------%
:- end_module sort.
%-----------------------------------------------------------------------------%
