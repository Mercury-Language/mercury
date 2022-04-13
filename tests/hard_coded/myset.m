%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module myset.
:- interface.

:- import_module list.
:- import_module io.

:- type set(T).

% convert list to set
:- func set(list(T)) = set(T).

% convert set to list
:- pred set_to_list(set(T)::in, list(T)::out) is cc_multi.
:- func set_to_sorted_list(set(T)) = list(T).

% empty set
:- func {} = set(T).
:- pred is_empty(set(T)::in) is semidet.

% singleton set
:- func {T} = set(T).

% union of two sets
:- func set(T) + set(T) = set(T).
:- mode in + in = out is det.

% union of an element and a set
:- func [T | set(T)] = set(T).
% :- mode [in | in] = out is det.
:- mode [out | out] = in is cc_nondet.

:- pred print_myset_rep(set(T)::in, io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module require.

:- type set(T)
    --->    set_rep(list(T))
    where equality is set_equal.

:- pred set_equal(set(T)::in, set(T)::in) is semidet.
set_equal(Set1, Set2) :-
    set_to_sorted_list(Set1) = set_to_sorted_list(Set2).

set(List) = set_rep(List).

set_to_list(set_rep(List), List).

set_to_sorted_list(Set) = Sorted :-
    promise_equivalent_solutions [Sorted] (
        Set = set_rep(Unsorted),
        list.sort(Unsorted, Sorted)
    ).

{} = set_rep([]).

{X} = set_rep([X]).

is_empty(Set) :-
    promise_equivalent_solutions [Empty] (
        Set = set_rep(List),
        Empty = (if List = [] then yes else no)
    ),
    Empty = yes.

Set1 + Set2 = Union :-
    promise_equivalent_solutions [Union] (
        Set1 = set_rep(List1),
        Set2 = set_rep(List2),
        list.append(List1, List2, UnionList),
        Union = set_rep(UnionList)
    ).

% [Element | set_rep(Rest)] = set_rep([Element | Rest]).
[Element | set_rep(Rest)] = UnionSet :-
    ( if is_empty(UnionSet) then
        fail
    else
        UnionSet = set_rep(UnionList),
        ( if UnionList = [Element1 | Rest1] then
            Element = Element1,
            Rest = Rest1
        else
            error("unexpected non-empty list")
        )
    ).

print_myset_rep(set_rep(List), !IO) :-
    print("set_rep(", !IO),
    print(List, !IO),
    print(")", !IO).
