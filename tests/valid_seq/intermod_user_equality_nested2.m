%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_user_equality_nested2.

:- interface.

:- type foo.

:- typeclass class(T) where [
    pred p(T::in, T::in) is semidet
].

:- instance class(foo).

:- pred foo_field1(foo::in, int::out) is cc_nondet.

    :- module intermod_user_equality_nested2.sub.

    :- interface.

    :- type bar
        --->    bar(foo).

    :- end_module intermod_user_equality_nested2.sub.

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
