%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test for bug #190.
% rotd-2011-03-17 and before were not module qualifying mutable definitions
% attached to the constraint_store attributes of solver types.
% Nor were equivalence types in such definitions being expanded.

:- module bug190.
:- interface.

:- solver type foo.

:- implementation.

:- import_module list.

:- type bar_type == list(int).

:- solver type foo where
    representation is character,
    constraint_store is [
        mutable(foo, list(int), [3], ground, [untrailed]),
        mutable(bar, bar_type, [3], ground, [untrailed])
    ].
