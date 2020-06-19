%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug510a.

:- interface.

:- import_module bug510b.
:- import_module list.

:- type bar_set == foo_set.

:- pred init_bar_set(int::in, bar_set::out) is det.

:- pred make_bar_set(int::in, bar_set::out) is det.

:- pred bar_set_to_list(bar_set::in, list(int)::out) is det.

:- implementation.
:- import_module enum.
:- import_module int.

init_bar_set(N, Set) :-
    make_foo_set(N, Set).

make_bar_set(N, Set) :-
    make_foo_set(N, Set).

bar_set_to_list(Set, List) :-
    foo_set_to_list(Set, List).
