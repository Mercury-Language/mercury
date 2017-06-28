%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_type_decl.

:- interface.

:- type foo.

:- implementation.

:- type foo.
:- type bar.
:- type bar.

:- type foo
    --->    foo(int).

:- type bar
    --->    bar(int).
