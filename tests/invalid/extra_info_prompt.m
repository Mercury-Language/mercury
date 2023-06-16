%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2006-05-27 wasn't printing out the recompile with `-E' prompt.

:- module extra_info_prompt.
:- interface.

:- type foo
    --->    foo
    where equality is foo_eq.

:- pred foo_eq(foo::in, foo::in) is semidet.

:- implementation.

foo_eq(_, _) :-
    semidet_true.
