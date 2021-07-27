%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The compiler of 15/9/1999 reported the wrong predicate name in the
% mode error for this test case because polymorphism.m substituted
% private_builtin.builtin_unify_int/2 for builtin.unify/2.
% That optimization is now only done by higher_order.m after all errors
% have been reported.

:- module unify_mode_error.

:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

foo(X, Y) :-
    builtin.unify(X, Y).
