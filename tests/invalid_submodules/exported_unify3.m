%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module exported_unify3.

:- interface.

:- type foo
    --->    foo1
    ;       foo2
    where equality is defined_in_wrong_module.

    :- module exported_unify3.sub.

    :- interface.

    :- type bar
        --->    bar1
        ;       bar2
        where equality is not_exported.

    :- pred defined_in_wrong_module(foo::in, foo::in) is semidet.

    :- end_module exported_unify3.sub.

:- implementation.

:- import_module exported_unify3.sub.

    :- module exported_unify3.sub.

    :- implementation.

    :- import_module std_util.

    defined_in_wrong_module(_, _) :- semidet_fail.

    :- pred not_exported(bar::in, bar::in) is semidet.

    not_exported(_, _) :- semidet_fail.

    :- end_module exported_unify3.sub.
