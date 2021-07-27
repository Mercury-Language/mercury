%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a cut-down version of a bug reported by Peter Hawkins.
% Mercury rotd 2005-08-02 and before failed this because some of the
% extra unifications required to convert the lambda expression to
% superhomogenous form were put in the wrong spot if the body of the
% lambda expression was impure.  In particular the unifications for the
% output arguments needed to be reordered by mode analysis so that they
% occurred after whatever goal produces their RHS; in the case where the
% body is impure so mode analysis could not do the necessary reordering.

:- module impure_lambda_bug.

:- interface.

:- type unit ---> unit.
:- type alpha ---> alpha.
:- type beta ---> beta.

    % A cut down version of the original bug.
    %
:- pred foo((impure pred(unit, unit))::out((pred(di, uo) is det))) is det.

    % A couple of other tests.
    %
:- pred baz((pred(alpha, beta, beta))::out(pred(out, di, uo) is det)) is det.
:- pred func_foo((impure func(unit) = unit)::out) is det.
:- pred with_unused((impure pred(unit, unit, unit))
    ::out((pred(unused, di, uo) is det))) is det.

:- implementation.

foo(Pred) :-
    Pred =
        ( impure pred(!.A::di, !:A::uo) is det :-
            impure bar(!A)
        ).

:- impure pred bar(unit, unit).
:-        mode bar(in, out) is det.
:-        mode bar(di, uo) is det.

bar(!A) :-
    impure private_builtin.imp.

baz(Pred) :-
    Pred =
        ( pred(R::out, A::di, B::uo) is det :-
            A = B,
            R = alpha
        ).

func_foo(Func) :-
    Func = (impure func(In) = Out :- impure bar(In, Out)).

with_unused(Pred) :-
    Pred =
        ( impure pred(_::unused, !.A::di, !:A::uo) is det :-
            impure bar(!A)
        ).
