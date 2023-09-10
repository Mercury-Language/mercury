%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module user_eq_pred_nested_helper_1.

:- interface.

:- type foo.

:- typeclass class(T) where [
    pred p(T::in, T::in) is semidet
].

:- instance class(foo).

:- pred foo_field1(foo::in, int::out) is cc_nondet.

    :- module user_eq_pred_nested_helper_1.user_eq_pred_nested_helper_2.

    :- interface.

    :- type bar
        --->    bar(foo).

    :- end_module user_eq_pred_nested_helper_1.user_eq_pred_nested_helper_2.

:- implementation.

:- type foo
    --->    ctor1(int, int)
    ;       ctor2(int, int)
    where equality is foo_unify.

:- instance class(foo) where [
    pred(p/2) is foo_unify
].

:- pred foo_unify(foo::in, foo::in) is semidet.
foo_unify(X, X).

foo_field1(ctor1(X, _), X).
foo_field1(ctor2(X, _), X).
