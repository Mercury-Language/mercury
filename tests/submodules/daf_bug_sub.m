%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module daf_bug_parent.daf_bug_sub.
:- interface.

:- type bool_kind
    --->    geq
    ;       lt
    ;       eq
    ;       neq.

:- func child_value = list(outer_public).

:- func child_value2 = univ.

:- implementation.

child_value = [
    outer_public1(inner_public(geq, 561, 42)),
    outer_public2(inner_private(geq, 561, 42))
].

child_value2 = univ([
    outer_private1(inner_public(geq, 561, 42)),
    outer_private2(inner_private(geq, 561, 42))
]).
