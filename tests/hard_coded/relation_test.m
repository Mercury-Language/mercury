:- module relation_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util, list, set, relation.

main -->
	{ relation__from_assoc_list(
		["a" - "b",
		"b" - "c",
		"c" - "d",
		"l1" - "l2",
		"l2" - "l3",
		"l3" - "l1",
		"x" - "x"],
		Rel) },
	{ relation__from_assoc_list(
		["a" - "a1",
		"b" - "b1",
		"c" - "c1",
		"d" - "d1"],
		Rel2) },
	test_rel("Rel", Rel),
	test_rel("Rel2", Rel2),
	{ relation__compose(Rel, Rel2, ComposedRel) },
	print("composition of Rel and Rel2 ="), nl,
	print_rel(ComposedRel), nl.

:- pred test_rel(string::in, relation(T)::in, io::di, io::uo) is det.

test_rel(Name, Rel) -->
	{ relation__dfs(Rel, DfsKeys) },
	{ list__map(relation__lookup_key(Rel), DfsKeys, Dfs) },
	{ relation__tc(Rel, TC_Rel) },
	{ relation__rtc(Rel, RTC_Rel) },
	{ relation__sc(Rel, SC_Rel) },
	{ relation__atsort(Rel, ATSort) },
	print(Name),
	print(" ="), nl, print_rel(Rel), nl,
	print("tc ="), nl, print_rel(TC_Rel), nl,
	print("rtc ="), nl, print_rel(RTC_Rel), nl,
	print("sc ="), nl, print_rel(SC_Rel), nl,
	print("dfs ="), nl, print(Dfs), nl,
	print("atsort ="), nl, list__foldl(print_set, ATSort), nl,
	( { relation__tsort(Rel, TSort) } ->
		print("tsort ="), nl, print(TSort), nl
	;
		print("tsort failed\n")
	),
	( { relation__is_dag(Rel) } ->
		io__write_string("is_dag succeeded\n")
	;
		io__write_string("is_dag failed\n")
	).

:- pred print_set(set(T)::in, io::di, io::uo) is det.

print_set(Set, !IO) :-
	io__write_string("[", !IO),
	io__write_list(set__to_sorted_list(Set), ", ", print, !IO),
	io__write_string("]\n", !IO).

:- pred print_rel(relation(T)::in, io::di, io::uo) is det.

print_rel(Relation) -->
	{ relation__to_assoc_list(Relation, AssocList0) },
	{ list__sort(AssocList0, AssocList) },
	write_list(AssocList, "\n\t", print), nl.

