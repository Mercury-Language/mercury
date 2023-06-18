%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_nested.
:- interface.

:- type foo.

:- implementation.
:- use_module test_nested_helper_1.
:- use_module test_nested_helper_1.test_nested_helper_3.
:- use_module test_nested_helper_1.test_nested_helper_4.
:- use_module test_nested_helper_1.test_nested_helper_5.
% :- use_module test_nested_helper_1.nonexistent_child.

:- use_module test_nested_helper_2.test_nested_helper_6.

:- type foo
    --->    foo(
                test_nested_helper_1.test_nested_helper_3.foo,
                test_nested_helper_1.test_nested_helper_4.foo,
                test_nested_helper_1.test_nested_helper_5.foo,
                test_nested_helper_1.nonexistent_child.foo,
                test_nested_helper_2.child.test_nested_helper_6
            ).
