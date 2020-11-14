%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for higher-order terms exported using
% inter-module optimization.
% This is also a regression test to check that local modes are put
% in the .opt files.
%

:- module intermod_lambda2.

:- interface.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred sol(pred(T), list(T)).
:- mode sol(pred(out) is det, out) is det.

:- implementation.

:- mode in2 == in.

sol(Generator, List) :-
    Test = ((pred) is semidet),
    TestFunc = ((func) = 1),
    Cons =
        ( pred(Elem::in, L0::in, L::out) is det :-
            intermod_lambda2__cons(Elem, L0, L)
        ),
    t(Test, TestFunc, Generator, Cons, [], List).

:- pred intermod_lambda2__cons(T::in, list(T)::in, list(T)::out) is det.

intermod_lambda2__cons(H, T, [H | T]).

:- pred t((pred), ((func) = int), pred(T), pred(T, T2, T2), T2, T2).
:- mode t((pred) is semidet, ((func) = out is det),
    pred(out) is det, pred(in, in, out) is det, in2, out) is det.

t(_, _, _, _, A, A).

%---------------------------------------------------------------------------%
