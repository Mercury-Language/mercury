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
        Args = [],
        handle_args(no, no, !IO),
        sort(!IO)
    ;
        Args = [Input],
        handle_args(yes(Input), no, !IO),
        sort(!IO)
    ;
        Args = [Input, Output], 
        handle_args(yes(Input), yes(Output), !IO),
        sort(!IO)
    ;
        Args = [_, _, _ | _],
        io.write_string("Usage: sort [Input [Output]]\\n", !IO)
    ).

:- pred handle_args(maybe(string)::in, maybe(string)::in, io::di, io::uo)
    is det.

handle_args(InArg, OutArg, !IO) :-
    (
        InArg = yes(InFilename),
        io.see(InFilename, InResult, !IO),
        (
            InResult = ok
        ;
            InResult = error(InError),
            io.error_message(InError, InMsg),
            error(InMsg)
        )
    ;
        InArg = no
    ),
    (
        OutArg = yes(OutFilename),
        io.tell(OutFilename, OutResult, !IO),
        (
            OutResult = ok
        ;
            OutResult = error(OutError),
            io.error_message(OutError, OutMsg), 
            error(OutMsg)
        )
    ;
        OutArg = no
    ).

:- pred sort(io::di, io::uo) is det.

sort(!IO) :-
    sort_2([], !IO).

:- pred sort_2(list(string)::in, io::di, io::uo) is det.

sort_2(Lines0, !IO) :-
    io.read_line_as_string(Result, !IO),
    (
        Result = error(Error),
        io.error_message(Error, Msg),
        error(Msg)
    ;
        Result = eof,
        output_sorted_lines(Lines0, !IO)
    ;
        Result = ok(Line),
        insert(Lines0, Line, Lines1),
        sort_2(Lines1, !IO)
    ).

:- pred insert(list(T)::in, T::in, list(T)::out) is det.

insert([], I, [I]).
insert([H | T], I, L) :-
    compare(R, I, H),
    (
        R = (<),
        L = [I, H | T]
    ;
        ( R = (=)
        ; R = (>)
        ),
        insert(T, I, NT),
        L = [H | NT]
    ).

:- pred output_sorted_lines(list(string)::in, io::di, io::uo) is det.

output_sorted_lines([], !IO).
output_sorted_lines([Line | Lines], !IO) :-
    io.write_string(Line, !IO),
    output_sorted_lines(Lines, !IO).

%-----------------------------------------------------------------------------%
:- end_module sort.
%-----------------------------------------------------------------------------%
