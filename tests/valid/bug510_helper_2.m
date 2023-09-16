%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug510_helper_2.

:- interface.

:- import_module list.

:- type foo_set.

:- pred init_set(int::in, foo_set::out) is det.

:- pred make_foo_set(int::in, foo_set::out) is det.

:- pred foo_set_to_list(foo_set::in, list(int)::out) is det.

:- implementation.
:- import_module enum.
:- import_module int.
:- import_module sparse_bitset.

:- type foo_set == sparse_bitset(int).

make_foo_set(N, Set) :-
    Set = sparse_bitset.make_singleton_set(N).

foo_set_to_list(Set, List) :-
    sparse_bitset.to_sorted_list(Set, List).

init_set(N, Set) :-
    make_foo_set(N, Set).
