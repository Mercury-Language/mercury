% Test operations on bitsets by comparing the output with the output
% from an ordinary set.
:- module bitset_tester.

:- interface.

:- import_module enum, list.

:- type bitset_tester(T).

:- type bitset_error(T)
	--->	one_argument(string,
			bitset_tester(T), bitset_tester(T))
	;	two_arguments(string, bitset_tester(T),
			bitset_tester(T), bitset_tester(T)).

:- func init = bitset_tester(T).
:- func insert(bitset_tester(T), T) = bitset_tester(T) <= enum(T).
:- func insert_list(bitset_tester(T), list(T)) =
		bitset_tester(T) <= enum(T).
:- func list_to_set(list(T)) = bitset_tester(T) <= enum(T).
:- func sorted_list_to_set(list(T)) = bitset_tester(T) <= enum(T).
:- func delete(bitset_tester(T), T) = bitset_tester(T) <= enum(T).
:- func delete_list(bitset_tester(T), list(T)) =
			bitset_tester(T) <= enum(T).
:- func remove(bitset_tester(T), T) = bitset_tester(T) <= enum(T).
:- mode remove(in, in) = out is semidet.
:- func remove_list(bitset_tester(T), list(T)) =
			bitset_tester(T) <= enum(T).
:- mode remove_list(in, in) = out is semidet.

:- func to_sorted_list(bitset_tester(T)) = list(T) <= enum(T).

:- func singleton_set(T) = bitset_tester(T) <= enum(T).

:- func union(bitset_tester(T), bitset_tester(T)) =
		bitset_tester(T) <= enum(T).
:- func intersect(bitset_tester(T), bitset_tester(T)) =
		bitset_tester(T) <= enum(T).
:- func difference(bitset_tester(T),
		bitset_tester(T)) = bitset_tester(T) <= enum(T).

:- pred remove_least(bitset_tester(T), T, bitset_tester(T)) <= enum(T).
:- mode remove_least(in, out, out) is semidet.

:- pred subset(bitset_tester(T), bitset_tester(T)).
:- mode subset(in, in) is semidet.

:- pred superset(bitset_tester(T), bitset_tester(T)).
:- mode superset(in, in) is semidet.

:- func count(bitset_tester(T)) = int <= enum(T).

:- func foldl(func(T, U) = U, bitset_tester(T), U) = U <= enum(T).

:- pred empty(bitset_tester(T)).
:- mode empty(in) is semidet.

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

:- import_module bool, exception, list, int, require, set, pair, string.
:- import_module sparse_bitset.

:- type bitset_tester(T) == pair(sparse_bitset(T), set__set(T)).

%-----------------------------------------------------------------------------%

init = init - init.

singleton_set(A) = make_singleton_set(A) - make_singleton_set(A).

init(init).
empty(A - B) :-
	( empty(A) -> EmptyA = yes; EmptyA = no),
	( empty(B) -> EmptyB = yes; EmptyB = no),
	( EmptyA = EmptyB ->
		EmptyA = yes
	;
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

%-----------------------------------------------------------------------------%

to_sorted_list(A - B) = List :-
	ListA = to_sorted_list(A),
	ListB = set__to_sorted_list(B),
	( ListA = ListB ->
		List = ListB
	;
		error("to_sorted_list failed")
	).

%-----------------------------------------------------------------------------%

delete(SetA - SetB, Var) = check("delete", SetA - SetB, delete(SetA, Var) - set__delete(SetB, Var)).

delete_list(SetA - SetB, List) =
	check("delete_list", SetA - SetB,
		delete_list(SetA, List) - set__delete_list(SetB, List)).

remove(SetA0 - SetB0, Elem) = Result :-
	( remove(SetA0, Elem, SetA1) ->
		( remove(SetB0, Elem, SetB1) ->
			SetA = SetA1,
			SetB = SetB1
		;
			error("remove succeeded unexpectedly")
		)
	; set__remove(SetB0, Elem, _) ->
		error("remove failed unexpectedly")
	;
		fail
	),
	Result = check("remove", SetA0 - SetB0, SetA - SetB).

remove_list(SetA0 - SetB0, List) = Result :-
	( remove_list(SetA0, List, SetA1) ->
		( set__remove_list(SetB0, List, SetB1) ->
			SetA = SetA1,
			SetB = SetB1
		;
			error("remove succeeded unexpectedly")
		)
	; set__remove_list(SetB0, List, _) ->
		error("remove failed unexpectedly")
	;
		fail
	),
	Result = check("remove_list", SetA0 - SetB0, SetA - SetB).

%-----------------------------------------------------------------------------%

insert(SetA - SetB, Var) = check("insert", SetA - SetB,
		insert(SetA, Var) - set__insert(SetB, Var)).

%-----------------------------------------------------------------------------%

insert_list(SetA - SetB, Vars) = check("insert_list", SetA - SetB, insert_list(SetA, Vars) - set__insert_list(SetB, Vars)).

%-----------------------------------------------------------------------------%

list_to_set(List) = check("list_to_set", init - init,
			list_to_set(List) - set__list_to_set(List)).

sorted_list_to_set(List) = check("sorted_list_to_set", init - init,
			sorted_list_to_set(List) - set__sorted_list_to_set(List)).

%-----------------------------------------------------------------------------%

contains(SetA - SetB, Var) :-
	( contains(SetA, Var) -> InSetA = yes ; InSetA = no),
	( set__contains(SetB, Var) -> InSetB = yes ; InSetB = no),
	( InSetA = InSetB ->
		InSetA = yes
	;
		error("contains failed")
	).

%-----------------------------------------------------------------------------%

foldl(F, SetA - SetB, Acc0) = Acc :-
	AccA = foldl(F, SetA, Acc0),
	AccB = fold(F, SetB, Acc0),
	( AccA = AccB ->
		Acc = AccA
	;	
		error("bitset_tester: fold failed")
	).

%-----------------------------------------------------------------------------%

count(SetA - SetB) = Count :-
	CountA = count(SetA),
	CountB = count(SetB),
	( CountA = CountB ->
		Count = CountA
	;	
		error("bitset_tester: count failed")
	).

%-----------------------------------------------------------------------------%

subset(SetA1 - SetB1, SetA2 - SetB2) :-
	( subset(SetA1, SetA2) ->
		( subset(SetB1, SetB2) ->
			true
		;	
			error("bitset_tester: subset succeeded unexpectedly")
		)
	; subset(SetB1, SetB2) ->
		error("bitset_tester: subset failed unexpectedly")
	;
		fail
	).

superset(SetA1 - SetB1, SetA2 - SetB2) :-
	( superset(SetA1, SetA2) ->
		( superset(SetB1, SetB2) ->
			true
		;	
			error("bitset_tester: superset succeeded unexpectedly")
		)
	; superset(SetB1, SetB2) ->
		error("bitset_tester: superset failed unexpectedly")
	;
		fail
	).

%-----------------------------------------------------------------------------%

union(SetA1 - SetB1, SetA2 - SetB2) =
		check2("union", SetA1 - SetB1, SetA2 - SetB2,
			union(SetA1, SetA2) - set__union(SetB1, SetB2)).

%-----------------------------------------------------------------------------%

intersect(SetA1 - SetB1, SetA2 - SetB2) =
		check2("intersect", SetA1 - SetB1, SetA2 - SetB2,
			intersect(SetA1, SetA2) - set__intersect(SetB1, SetB2)).

%-----------------------------------------------------------------------------%

difference(SetA1 - SetB1, SetA2 - SetB2) =
		check2("difference", SetA1 - SetB1, SetA2 - SetB2,
			difference(SetA1, SetA2) - set__difference(SetB1, SetB2)).

%-----------------------------------------------------------------------------%

remove_least(SetA0 - SetB0, Least, SetA - SetB) :-
	( remove_least(SetA0, LeastA, SetA1) ->
		( remove_least(SetB0, LeastB, SetB1) ->
			( LeastA = LeastB ->
				SetA = SetA1,
				SetB = SetB1,
				Least = LeastA
			;
				error("remove_least: wrong least element")
			)
		;
			error("remove_least: should be no least value")
		)
	; remove_least(SetB0, _, _) ->
		error("remove_least: failed")
	;
		fail
	).

%-----------------------------------------------------------------------------%

:- func check(string, bitset_tester(T), bitset_tester(T)) =
		bitset_tester(T) <= enum(T).

check(Op, Tester1, Tester) = Tester :-

	Tester1 = BitSet1 - Set1,
	BitSetSet1 =
		sparse_bitset__sorted_list_to_set(set__to_sorted_list(Set1)),
	Tester = BitSet - Set,
	BitSetSet = sparse_bitset__sorted_list_to_set(set__to_sorted_list(Set)),
	( BitSetSet1 = BitSet1, BitSet = BitSetSet ->
		true
	;
		throw(one_argument(Op, Tester1, Tester))
	).

:- func check2(string, bitset_tester(T), bitset_tester(T),
		bitset_tester(T)) = bitset_tester(T) <= enum(T).

check2(Op, Tester1, Tester2, Tester) = Result :-
	Tester1 = BitSet1 - Set1,
	BitSetSet1 =
		sparse_bitset__sorted_list_to_set(set__to_sorted_list(Set1)),
	Tester2 = BitSet2 - Set2,
	BitSetSet2 = sorted_list_to_set(
		set__to_sorted_list(Set2)),

	Tester = BitSet - Set,
	BitSetSet = sparse_bitset__sorted_list_to_set(set__to_sorted_list(Set)),

	( BitSetSet1 = BitSet1, BitSetSet2 = BitSet2, BitSet = BitSetSet ->
		Result = Tester
	;
		throw(two_arguments(Op, Tester1, Tester2, Tester))
	).

%-----------------------------------------------------------------------------%
