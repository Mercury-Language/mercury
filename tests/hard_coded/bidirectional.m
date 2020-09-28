%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% Tests predicates with multiple modes,
% reordering, and partially instantiated data structures.
%

:- module bidirectional.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    format_list_of_int(read, List, !IO),
    format_list_of_int(write, List, !IO).

:- type rw
    --->    read
    ;       write.

:- mode read == in(bound(read)).
:- mode write == in(bound(write)).

:- pred format_list_of_int(rw, list(int), io, io).
:- mode format_list_of_int(write, in, di, uo) is det.
:- mode format_list_of_int(read, out, di, uo) is det.

format_list_of_int(RW, List, !IO) :-
    format_int(RW, Val, !IO),
    (
        Val = no,
        List = []
    ;
        Val = yes(X),
        List = [X | _]
    ),
    (
        List = []
    ;
        List = [_ | _],
        format_list_of_int(RW, Xs, !IO),
        List = [_ | Xs]
    ).

:- pred format_int(rw, maybe(int), io, io).
:- mode format_int(write, in, di, uo) is det.
:- mode format_int(read, out, di, uo) is det.

format_int(RW, MaybeVal, !IO) :-
    (
        RW = read,
        my_read_line(MaybeLine, !IO),
        ( if
            MaybeLine = ok(Chars),
            string.from_char_list(Chars, Line),
            string.to_int(Line, X)
        then
            MaybeVal = yes(X)
        else
            MaybeVal = no
        )
    ;
        RW = write,
        (
            MaybeVal = yes(X),
            io.write_int(X, !IO),
            io.write_string("\n", !IO)
        ;
            MaybeVal = no,
            io.write_string("\n", !IO)
        )
    ).

:- pred my_read_line(io.result(list(char))::out, io::di, io::uo) is det.

my_read_line(Result, !IO) :-
    io.read_line(Result0, !IO),
    ( if
        Result0 = ok(Line0),
        list.reverse(Line0, ['\n' | LineRev])
    then
        list.reverse(LineRev, Line),
        Result = ok(Line)
    else
        Result = Result0
    ).
