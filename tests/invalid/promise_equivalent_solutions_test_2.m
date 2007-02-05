:- module promise_equivalent_solutions_test_2.
:- interface.

:- pred p1(int::ia) is semidet.

:- pred p2(int::ia, int::ia) is semidet.

:- pred p3(int::ia, int::out) is semidet.

:- implementation.

	% This should pass.
p1(X) :-
    promise_equivalent_solutions [X] (
        q(X)
    ).

	% Compiler should complain about Y being possibly constrained.
p2(X, Y) :-
    promise_equivalent_solutions [X] (
        q(X),
        q(Y)
    ).

	% Compiler should complain about Y being bound.
p3(X, Y) :-
    promise_equivalent_solutions [X] (
        q(X),
        q(Y)
    ).

:- pred q(int::oa) is multi.

q(1).
q(2).
q(3).
