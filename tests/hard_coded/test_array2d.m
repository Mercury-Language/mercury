%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% test_array2d.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Jan 21 12:38:05 EST 2003
%---------------------------------------------------------------------------%

:- module test_array2d.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array2d.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module pprint.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    Empty = array2d([]) `with_type` array2d(int),
    write_array2d("Empty", Empty, !IO),
    io.nl(!IO),

    One = array2d([[1]]),
    write_array2d("One", One, !IO),
    write_array2d_elem("One", One, 0, 0, !IO),
    io.nl(!IO),

    Two = array2d([[1, 0], [0, 2]]),
    write_array2d("Two", Two, !IO),
    write_array2d_elem("Two", Two, 0, 0, !IO),
    write_array2d_elem("Two", Two, 0, 1, !IO),
    write_array2d_elem("Two", Two, 1, 0, !IO),
    write_array2d_elem("Two", Two, 1, 1, !IO),
    io.nl(!IO),

    ( if array2d.is_empty(Empty) then
        io.write_string("Empty is empty\n", !IO)
    else
        io.write_string("Empty is not empty\n", !IO)
    ),
    ( if array2d.is_empty(One) then
        io.write_string("One is empty\n", !IO)
    else
        io.write_string("One is not empty\n", !IO)
    ),
    ( if array2d.is_empty(Two) then
        io.write_string("Two is empty\n", !IO)
    else
        io.write_string("Two is not empty\n", !IO)
    ),
    io.nl(!IO),

    Two_a = Two ^ elem(0, 1) := 3,
    write_array2d("Two_a", Two_a, !IO),
    io.nl(!IO),

    Two_b = Two_a ^ elem(1, 0) := 4,
    write_array2d("Two_b", Two_b, !IO),
    io.nl(!IO),

    Zeroes = array2d.init(3, 3, 0),
    write_array2d("Zeroes", Zeroes, !IO),
    io.nl(!IO),

    write_array2d_elem("Empty",  Empty,   0,  0, !IO),
    write_array2d_elem("Zeroes", Zeroes, -1,  0, !IO),
    write_array2d_elem("Zeroes", Zeroes,  0, -1, !IO),
    write_array2d_elem("Zeroes", Zeroes, -1, -1, !IO),
    write_array2d_elem("Zeroes", Zeroes,  3,  0, !IO),
    write_array2d_elem("Zeroes", Zeroes,  0,  3, !IO),
    write_array2d_elem("Zeroes", Zeroes,  3,  3, !IO),
    io.nl(!IO),

    FourSix = array2d([
        [11, 12, 13, 14, 15, 16],
        [21, 22, 23, 24, 25, 26],
        [31, 32, 33, 34, 35, 36],
        [41, 42, 43, 44, 45, 46]
    ]),
    FourSixLists = lists(FourSix),
    list.foldl(io.write_line, FourSixLists, !IO),

    true.

%---------------------------------------------------------------------------%

:- pred write_array2d(string, array2d(T), io, io).
:- mode write_array2d(in,     array2d_ui, di, uo) is det.

write_array2d(Name, Table, !IO) :-
    io.format("%s =\n%s\n", [s(Name), s(test_array2d.string(Table))], !IO).

%---------------------------------------------------------------------------%

:- pred write_array2d_elem(string, array2d(T), int, int, io, io).
:- mode write_array2d_elem(in,     array2d_ui, in,  in,  di, uo) is cc_multi.

write_array2d_elem(Name, Table, R, C, !IO) :-
    io.format("%s ^ elem(%d, %d) = ", [s(Name), i(R), i(C)], !IO),
    try(
        ( pred(X::out) is det :-
            X = Table ^ elem(R, C)
        ), Result),
    (
        Result = succeeded(Elem),
        io.print(Elem, !IO)
    ;
        Result = exception(_),
        io.print(Result, !IO)
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- func string(array2d(T)) = string.
:- mode string(array2d_ui) = out is det.

string(T) =
    to_string(80, brackets(nest(1, separated(to_doc, line, lists(T))))).

%---------------------------------------------------------------------------%
