%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called intermod_user_equality_nested.
%

:- module user_eq_pred_nested.

:- interface.
:- import_module user_eq_pred_nested_helper_1.

:- pred check_foo(foo::in, foo::in) is semidet.

:- implementation.

check_foo(Foo, Bar) :-
    Foo = Bar.
