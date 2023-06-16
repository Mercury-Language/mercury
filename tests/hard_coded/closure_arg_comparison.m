%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module closure_arg_comparison.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.

main(!IO) :-
    compare_representation(R, p(q(1)), p(r(1))),
    io.write_line(R, !IO).

:- pred p(pred(int), int).
:- mode p(pred(out) is det, out) is det.

p(P, X) :-
    P(Y),
    X = Y + 1.

:- pred q(int::in, int::out) is det.

q(X, X + 4).

:- pred r(int::in, int::out) is det.

r(X, X + 5).
