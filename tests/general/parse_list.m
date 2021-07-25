%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module, originally written by Philip Dart,
% uncovered a (second) bug in the implementation of semidet predicates
% in Mercury version 0.4.
%

:- module parse_list.
:- interface.

:- import_module io.
:- import_module list.
:- import_module std_util.

:- pred main(io::di, io::uo) is det.

:- pred meta_parse_list(pred(Y, Y, X), list(X), Y, Y).
:- mode meta_parse_list(pred(in, out, out) is semidet, out, in, out) is det.

:- implementation.

:- import_module builtin.
:- import_module int.
:- import_module string.

main(!IO) :-
    P = (pred(I::in, O::out, N::out) is semidet :- one_or_two(N, I, O)),
    ( if meta_parse_list(P, [X, Y], [2, 1, 3], _) then
        string.int_to_string(X, SX),
        string.int_to_string(Y, SY),
        io.write_strings(["Success: X = ", SX, "; Y = ", SY, ".\n"], !IO)
    else
        io.write_string("Failure.\n", !IO)
    ).

meta_parse_list(P, L, In, Out) :-
    ( if call(P, In, Out0, E) then
        L = [E | L1],
        meta_parse_list(P, L1, Out0, Out)
    else
        L = [], Out = In
    ).

:- pred one_or_two(int::out, list(int)::in, list(int)::out) is semidet.

one_or_two(1) --> [1].
one_or_two(2) --> [2].
