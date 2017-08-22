%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_eqv2.
:- interface.

:- type foo.

:- type bar
    --->    bar(int).

:- pred call_with_foo(foo::in) is det.

:- implementation.

:- type foo == bar.

call_with_foo(_).
