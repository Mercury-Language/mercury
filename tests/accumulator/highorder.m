%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Highoder functions cannot use accumulator recursion because we
% don't know anything about the assocativity of P.
%

:- module highorder.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    io.write_string("foldr: ", !IO),
    highorder.foldr(minus, [1, 10, 100], 0, ListA),
    io.write_line(ListA, !IO).

:- pred minus(int::in, int::in, int::out) is det.

minus(A, B, C) :-
    C = A - B.

    % highorder.foldr(Pred, List, Start, End) calls Pred with each element
    % of List (working right-to-left) and an accumulator (with the initial
    % value of Start), and returns the final value in End.
    %
:- pred foldr(pred(X, Y, Y)::in(pred(in, in, out) is det),
    list(X)::in, Y::in, Y::out) is det.

foldr(_, [], Acc, Acc).
foldr(P, [H | T], Acc0, Acc) :-
    highorder.foldr(P, T, Acc0, Acc1),
    call(P, H, Acc1, Acc).
