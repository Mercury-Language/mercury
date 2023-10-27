%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bad_mutable_int.

:- interface.

:- mutable(in_interface, int, 0, ground, [untrailed]).
:- type export_something == int.
:- implementation.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- mutable(bad_attribute, int, 0, ground, [untrailed, bad_attrib]).

:- mutable(poly_type, list(T), [], ground, [untrailed]).

:- mutable(conflicting_trail, int, 0, ground, [untrailed, trailed]).

:- mutable(multiple_foreign, int, 0, ground,
    [untrailed, foreign_name("C", "one"), foreign_name("C", "two")]).

:- mutable(conflicting_thr_local1, int, 0, ground, [thread_local, trailed]).

:- mutable(conflicting_thr_local2, int, 0, ground, [thread_local, constant]).

:- mutable(non_list_attr, int, 0, ground, thread_local).

:- mutable(conflict_trailed_constant, int, 0, ground, [trailed, constant]).

:- mutable(repeated_untrailed, int, 0, ground, [untrailed, untrailed]).

:- mutable(repeated_constant, int, 0, ground,
    [constant,
    constant, constant, % There should be one error for these two repeats ...
    constant]).         % ... and one error for this one repeat.

:- mutable(implicitly_untrailed, int, 0, ground, [thread_local]).
