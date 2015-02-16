%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_nested.
:- interface.

:- type foo.

:- implementation.
:- use_module parent.
:- use_module parent.public_child.
:- use_module parent.private_child.
:- use_module parent.undeclared_child.
% :- use_module parent.nonexistent_child.

:- use_module parent2.child.

:- type foo
    --->    foo(
                parent.public_child.foo,
                parent.private_child.foo,
                parent.undeclared_child.foo,
                parent.nonexistent_child.foo,
                parent2.child.foo
            ).
