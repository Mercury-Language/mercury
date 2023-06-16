%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for a bug in the MLDS backend in rotd-2007-08-28 and before.
% The MLDS code generator was incorrectly inserting assignment statements
% to do casts even when the types on the lhs and rhs were identical.
% This was inhibiting tail call optimisation in code that used arrays.
% (The code marked XXX below is one such example.).
%

:- module big_array_from_list.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    List = 0 .. 600000,
    array_from_list(List, Array),
    NumElems = array.foldl(count, Array, 0),
    io.format("NumElems = %d\n", [i(NumElems)], !IO).

:- pred array_from_list(list(T), array(T)).
:- mode array_from_list(in, array_uo) is det.

array_from_list([], Array) :-
    array.make_empty_array(Array).
array_from_list(List, Array) :-
    List = [Head | Tail],
    list.length(List, Len),
    array.init(Len, Head, Array0),
    array_insert_items(Tail, 1, Array0, Array).

% XXX Tail call optimisation was not being performed on this predicate
% in hl* grades.
:- pred array_insert_items(list(T)::in, int::in,
    array(T)::array_di, array(T)::array_uo) is det.

array_insert_items([], _N, Array, Array).
array_insert_items([Head | Tail], N, Array0, Array) :-
    array.set(N, Head, Array0, Array1),
    N1 = N + 1,
    array_insert_items(Tail, N1, Array1, Array).

:- func count(int, int) = int.

count(_, X) = X + 1.
