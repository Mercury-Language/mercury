%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fundeps_unbound_in_ctor.
:- interface.

:- typeclass foo(A, B, C) <= (A -> B) where [].

    % Error: C is unbound.
    %
:- type bar
    --->    some [A, B, C] bar(A) => foo(A, B, C).

    % Error: C is universally quantified.
    %
:- type baz(C)
    --->    some [A, B] baz(A, B, C) => foo(A, B, C).
