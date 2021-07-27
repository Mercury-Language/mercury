%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a limitation of switch detection and
% common subexpression elimination. The limitation was that when they looked
% for deconstruction unifications, they stopped looking when encountering
% a call, even if the call came from the clause head.
%
% In the test predicate below, they failed to recognize the common
% deconstruction of Pair with - and the different deconstructions of List
% with [] and [ | ], leading determinism analysis to conclude that test may
% have more than one solution, leading to a bogus determinism error.

:- module func_in_head.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pair.
:- import_module set.

main(!IO) :-
    test([1, 2] - "dummy", Set),
    io.write_list(set.to_sorted_list(Set), ", ", io.write_int, !IO).

:- pred test(pair(list(T), U)::in, set(T)::out) is det.

test(Pair, set.init) :-
    Pair = List - _,
    List = [].
test(Pair, Set) :-
    Pair = List - _,
    List = [_ | _],
    set.list_to_set(List, Set).
