%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module daf_bug_parent.
:- interface.

:- include_module daf_bug_sub.
:- import_module daf_bug_parent.daf_bug_sub.

:- import_module list.
:- import_module univ.

:- type outer_public
    --->    outer_public0(list(int))
    ;       outer_public1(inner_public)
    ;       outer_public2(inner_private).

:- type inner_public
        --->    inner_public(bool_kind, int, int).

:- type inner_private.

:- func parent_value = list(outer_public).

:- func parent_value2 = univ.

:- implementation.

:- type outer_private
    --->    outer_private0(list(int))
    ;       outer_private1(inner_public)
    ;       outer_private2(inner_private).

:- type inner_private
    --->    inner_private(bool_kind, int, int).

parent_value = [
    outer_public1(inner_public(geq, 561, 42)),
    outer_public2(inner_private(geq, 561, 42))
].

parent_value2 = univ([
    outer_private1(inner_public(geq, 561, 42)),
    outer_private2(inner_private(geq, 561, 42))
]).
