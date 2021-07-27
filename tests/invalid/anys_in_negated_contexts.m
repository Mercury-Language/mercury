%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module anys_in_negated_contexts.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    io.print("Hello, World!\n", !IO).

:- pred good_if_then_else(T::ia, int::out) is det.

good_if_then_else(X, Y) :-
    promise_pure
    ( if
        ia(X),
        ig(3)
    then
        Y = 1
    else
        Y = 2
    ).

:- pred bad_if_then_else1(T::ia, int::out) is det.

bad_if_then_else1(X, Y) :-
    ( if
        ia(X),
        ig(3)
    then
        Y = 1
    else
        Y = 2
    ).

:- pred good_negation(T::ia) is semidet.

good_negation(X) :-
    promise_pure (
        not (
            ia(X),
            ig(3)
        )
    ).

:- pred bad_negation1(T::ia) is semidet.

bad_negation1(X) :-
    not (
        ia(X),
        ig(3)
    ).

:- pred pure_pred_mode_specific_clauses(int).
:- mode pure_pred_mode_specific_clauses(in) is semidet.
:- mode pure_pred_mode_specific_clauses(out) is det.
:- pragma promise_pure(pure_pred_mode_specific_clauses/1).

pure_pred_mode_specific_clauses(42::in).
pure_pred_mode_specific_clauses(11::out).

:- impure pred good_lambda(int::in) is semidet.

good_lambda(X) :-
    oa(Y),
    P = (impure pred(Z::in) is semidet :- Y = Z),
    impure P(X).

:- pred bad_lambda(int::in) is semidet.

bad_lambda(X) :-
    oa(Y),
    P = (pred(Z::in) is semidet :- Y = Z),
    P(X).

:- pred ia(T::ia) is semidet.

ia(_) :-
    semidet_succeed.

:- pred ig(T::in) is semidet.

ig(_) :-
    semidet_succeed.

:- pred oa(int::oa) is det.

oa(42).

%---------------------------------------------------------------------------%
