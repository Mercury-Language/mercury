%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module trans_intermod_user_equality_helper_2.

:- interface.

:- type foo
    --->    ctor1(int, int)
    ;       ctor2(int, int)
    where equality is foo_unify.

:- pred foo_unify(foo::in, foo::in) is semidet.

:- implementation.

foo_unify(X, X).
