%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exported_unify_helper_1.

:- interface.

:- type foo
    --->    foo1
    ;       foo2
    where equality is unify_foo.

:- implementation.

:- import_module std_util.

:- pred unify_foo(foo::in, foo::in) is semidet.

unify_foo(_, _) :-
    semidet_fail.
