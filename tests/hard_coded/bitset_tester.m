%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test operations on bitsets by comparing the output with the output
% from an ordinary set.

:- module bitset_tester.

:- interface.

:- import_module enum.
:- import_module list.

:- type bitset_tester(T).

:- type bitset_error(T)
    --->    one_argument(string,
                bitset_tester(T), bitset_tester(T))
    ;       two_arguments(string, bitset_tester(T),
                bitset_tester(T), bitset_tester(T)).

:- func init = bitset_tester(T).
:- func insert(bitset_tester(T), T) = bitset_tester(T) <= enum(T).
:- func insert_list(bitset_tester(T), list(T)) = bitset_tester(T) <= enum(T).
:- func list_to_set(list(T)) = bitset_tester(T) <= enum(T).
:- func sorted_list_to_set(list(T)) = bitset_tester(T) <= enum(T).
:- func delete(bitset_tester(T), T) = bitset_tester(T) <= enum(T).
:- func delete_list(bitset_tester(T), list(T)) = bitset_tester(T) <= enum(T).
:- func remove(bitset_tester(T), T) = bitset_tester(T) <= enum(T).
:- mode remove(in, in) = out is semidet.
:- func remove_list(bitset_tester(T), list(T)) = bitset_tester(T) <= enum(T).
:- mode remove_list(in, in) = out is semidet.

:- func to_sorted_list(bitset_tester(T)) = list(T) <= enum(T).

:- func singleton_set(T) = bitset_tester(T) <= enum(T).

:- func union(bitset_tester(T), bitset_tester(T)) = bitset_tester(T)
    <= enum(T).
:- func intersect(bitset_tester(T), bitset_tester(T)) = bitset_tester(T)
    <= enum(T).
:- func difference(bitset_tester(T), bitset_tester(T)) = bitset_tester(T)
    <= enum(T).

:- pred remove_least(bitset_tester(T), T, bitset_tester(T)) <= enum(T).
:- mode remove_least(in, out, out) is semidet.

:- pred subset(bitset_tester(T), bitset_tester(T)).
:- mode subset(in, in) is semidet.

:- pred superset(bitset_tester(T), bitset_tester(T)).
:- mode superset(in, in) is semidet.

:- func count(bitset_tester(T)) = int <= enum(T).

:- func foldl(func(T, U) = U, bitset_tester(T), U) = U <= enum(T).

:- pred is_empty(bitset_tester(T)).
:- mode is_empty(in) is semidet.

:- pred contains(bitset_tester(T)::in, T::in) is semidet <= enum(T).

:- pred init(bitset_tester(T)::out) is det.
:- pred singleton_set(bitset_tester(T)::out, T::in) is det <= enum(T).

:- pred list_to_set(list(T)::in, bitset_tester(T)::out) is det <= enum(T).
:- pred sorted_list_to_set(list(T)::in,
    bitset_tester(T)::out) is det <= enum(T).
:- pred to_sorted_list(bitset_tester(T)::in,
    list(T)::out) is det <= enum(T).
:- pred insert(bitset_tester(T)::in, T::in,
    bitset_tester(T)::out) is det <= enum(T).
:- pred insert_list(bitset_tester(T)::in,
    list(T)::in, bitset_tester(T)::out) is det <= enum(T).
:- pred delete(bitset_tester(T)::in, T::in,
    bitset_tester(T)::out) is det <= enum(T).
:- pred delete_list(bitset_tester(T)::in, list(T)::in,
    bitset_tester(T)::out) is det <= enum(T).
:- pred union(bitset_tester(T)::in,
    bitset_tester(T)::in, bitset_tester(T)::out) is det <= enum(T).
:- pred intersect(bitset_tester(T)::in,
    bitset_tester(T)::in, bitset_tester(T)::out) is det <= enum(T).
:- pred difference(bitset_tester(T)::in,
    bitset_tester(T)::in, bitset_tester(T)::out) is det <= enum(T).

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module sparse_bitset.
:- import_module string.

:- type bitset_tester(T) == pair(sparse_bitset(T), set.set(T)).

%---------------------------------------------------------------------------%

init = init - init.

singleton_set(A) = make_singleton_set(A) - make_singleton_set(A).

init(init).
is_empty(A - B) :-
    ( if is_empty(A) then EmptyA = yes else EmptyA = no),
    ( if is_empty(B) then EmptyB = yes else EmptyB = no),
    ( if EmptyA = EmptyB then
        EmptyA = yes
    else
        error("empty failed")
    ).
singleton_set(singleton_set(A), A).
insert(A, B, insert(A, B)).
insert_list(A, B, insert_list(A, B)).
delete(A, B, delete(A, B)).
delete_list(A, B, delete_list(A, B)).
list_to_set(A, list_to_set(A)).
to_sorted_list(A, to_sorted_list(A)).
sorted_list_to_set(A, sorted_list_to_set(A)).
union(A, B, union(A, B)).
intersect(A, B, intersect(A, B)).
difference(A, B, difference(A, B)).

%---------------------------------------------------------------------------%

to_sorted_list(A - B) = List :-
    ListA = to_sorted_list(A),
    ListB = set.to_sorted_list(B),
    ( if ListA = ListB then
        List = ListB
    else
        error("to_sorted_list failed")
    ).

%---------------------------------------------------------------------------%

delete(SetA - SetB, Var) =
    check("delete", SetA - SetB, delete(SetA, Var) - set.delete(SetB, Var)).

delete_list(SetA - SetB, List) =
    check("delete_list", SetA - SetB,
        delete_list(SetA, List) - set.delete_list(SetB, List)).

remove(SetA0 - SetB0, Elem) = Result :-
    ( if remove(Elem, SetA0, SetA1) then
        ( if remove(Elem, SetB0, SetB1) then
            SetA = SetA1,
            SetB = SetB1
        else
            error("remove succeeded unexpectedly")
        )
    else if set.remove(Elem, SetB0, _) then
        error("remove failed unexpectedly")
    else
        fail
    ),
    Result = check("remove", SetA0 - SetB0, SetA - SetB).

remove_list(SetA0 - SetB0, List) = Result :-
    ( if remove_list(List, SetA0, SetA1) then
        ( if set.remove_list(List, SetB0, SetB1) then
            SetA = SetA1,
            SetB = SetB1
        else
            error("remove succeeded unexpectedly")
        )
    else if set.remove_list(List, SetB0, _) then
        error("remove failed unexpectedly")
    else
        fail
    ),
    Result = check("remove_list", SetA0 - SetB0, SetA - SetB).

%---------------------------------------------------------------------------%

insert(SetA - SetB, Var) =
    check("insert", SetA - SetB, insert(SetA, Var) - set.insert(SetB, Var)).

%---------------------------------------------------------------------------%

insert_list(SetA - SetB, Vars) =
    check("insert_list", SetA - SetB,
        insert_list(SetA, Vars) - set.insert_list(SetB, Vars)).

%---------------------------------------------------------------------------%

list_to_set(List) =
    check("list_to_set", init - init,
        list_to_set(List) - set.list_to_set(List)).

sorted_list_to_set(List) =
    check("sorted_list_to_set", init - init,
        sorted_list_to_set(List) - set.sorted_list_to_set(List)).

%---------------------------------------------------------------------------%

contains(SetA - SetB, Var) :-
    ( if contains(SetA, Var) then InSetA = yes else InSetA = no),
    ( if set.contains(SetB, Var) then InSetB = yes else InSetB = no),
    ( if InSetA = InSetB then
        InSetA = yes
    else
        error("contains failed")
    ).

%---------------------------------------------------------------------------%

foldl(F, SetA - SetB, Acc0) = Acc :-
    AccA = foldl(F, SetA, Acc0),
    AccB = fold(F, SetB, Acc0),
    ( if AccA = AccB then
        Acc = AccA
    else
        error("bitset_tester: fold failed")
    ).

%---------------------------------------------------------------------------%

count(SetA - SetB) = Count :-
    CountA = count(SetA),
    CountB = count(SetB),
    ( if CountA = CountB then
        Count = CountA
    else
        error("bitset_tester: count failed")
    ).

%---------------------------------------------------------------------------%

subset(SetA1 - SetB1, SetA2 - SetB2) :-
    ( if subset(SetA1, SetA2) then
        ( if subset(SetB1, SetB2) then
            true
        else
            error("bitset_tester: subset succeeded unexpectedly")
        )
    else if subset(SetB1, SetB2) then
        error("bitset_tester: subset failed unexpectedly")
    else
        fail
    ).

superset(SetA1 - SetB1, SetA2 - SetB2) :-
    ( if superset(SetA1, SetA2) then
        ( if superset(SetB1, SetB2) then
            true
        else
            error("bitset_tester: superset succeeded unexpectedly")
        )
    else if superset(SetB1, SetB2) then
        error("bitset_tester: superset failed unexpectedly")
    else
        fail
    ).

%---------------------------------------------------------------------------%

union(SetA1 - SetB1, SetA2 - SetB2) =
    check2("union", SetA1 - SetB1, SetA2 - SetB2,
        union(SetA1, SetA2) - set.union(SetB1, SetB2)).

%---------------------------------------------------------------------------%

intersect(SetA1 - SetB1, SetA2 - SetB2) =
    check2("intersect", SetA1 - SetB1, SetA2 - SetB2,
        intersect(SetA1, SetA2) - set.intersect(SetB1, SetB2)).

%---------------------------------------------------------------------------%

difference(SetA1 - SetB1, SetA2 - SetB2) =
    check2("difference", SetA1 - SetB1, SetA2 - SetB2,
        difference(SetA1, SetA2) - set.difference(SetB1, SetB2)).

%---------------------------------------------------------------------------%

remove_least(SetA0 - SetB0, Least, SetA - SetB) :-
    ( if remove_least(LeastA, SetA0, SetA1) then
        ( if remove_least(LeastB, SetB0, SetB1) then
            ( if LeastA = LeastB then
                SetA = SetA1,
                SetB = SetB1,
                Least = LeastA
            else
                error("remove_least: wrong least element")
            )
        else
            error("remove_least: should be no least value")
        )
    else if remove_least(_, SetB0, _) then
        error("remove_least: failed")
    else
        fail
    ).

%---------------------------------------------------------------------------%

:- func check(string, bitset_tester(T), bitset_tester(T)) = bitset_tester(T)
    <= enum(T).

check(Op, Tester1, Tester) = Tester :-
    Tester1 = BitSet1 - Set1,
    BitSetSet1 =
        sparse_bitset.sorted_list_to_set(set.to_sorted_list(Set1)),
    Tester = BitSet - Set,
    BitSetSet = sparse_bitset.sorted_list_to_set(set.to_sorted_list(Set)),
    ( if BitSetSet1 = BitSet1, BitSet = BitSetSet then
        true
    else
        throw(one_argument(Op, Tester1, Tester))
    ).

:- func check2(string, bitset_tester(T), bitset_tester(T), bitset_tester(T))
    = bitset_tester(T) <= enum(T).

check2(Op, Tester1, Tester2, Tester) = Result :-
    Tester1 = BitSet1 - Set1,
    BitSetSet1 =
        sparse_bitset.sorted_list_to_set(set.to_sorted_list(Set1)),
    Tester2 = BitSet2 - Set2,
    BitSetSet2 = sorted_list_to_set(
        set.to_sorted_list(Set2)),

    Tester = BitSet - Set,
    BitSetSet = sparse_bitset.sorted_list_to_set(set.to_sorted_list(Set)),

    ( if BitSetSet1 = BitSet1, BitSetSet2 = BitSet2, BitSet = BitSetSet then
        Result = Tester
    else
        throw(two_arguments(Op, Tester1, Tester2, Tester))
    ).

%---------------------------------------------------------------------------%
