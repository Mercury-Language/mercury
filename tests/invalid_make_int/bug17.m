%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug17.
:- interface.

:- func init_foo = foo.

:- implementation.

:- type foo
    --->    foo(int).

init_foo = foo(0).
