%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The following modules causes rotd-2006-05-27 to abort with the following:
%
% Uncaught Mercury exception:
% Software Error: rtti.m: Unexpected: type_ctor_rep_to_string: dummy type with
% user equality

:- module user_eq_dummy.
:- interface.

:- type foo
    --->    foo
    where equality is foo_eq.

:- type bar
    --->    bar
    where comparison is bar_cmp.

:- pred foo_eq(foo::in, foo::in) is semidet.
:- pred bar_cmp(comparison_result::uo, bar::in, bar::in) is det.

:- implementation.

foo_eq(_, _) :-
    semidet_true.

bar_cmp((=), _, _).
