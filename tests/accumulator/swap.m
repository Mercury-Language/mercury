%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that the compiler recognises append is assocative if we
% swap the order of the two input arguments.
%

:- module swap.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    io.write_string("rev: ", !IO),
    rev([5, 6, 7], ListA),
    io.write_line(ListA, !IO).

:- pred rev(list(T)::in, list(T)::out) is det.

rev([], []).
rev([H | T], R) :-
    rev(T, R0),
    append(R0, [H], R).
