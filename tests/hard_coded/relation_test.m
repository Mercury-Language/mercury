:- module relation_test.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module std_util, list, relation.

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
	{ relation__tc(Rel, TC_Rel) },
	{ relation__rtc(Rel, RTC_Rel) },
	{ relation__compose(Rel, Rel2, ComposedRel) },
	print("Rel ="), nl, print_rel(Rel), nl,
	print("tc of Rel ="), nl, print_rel(TC_Rel), nl,
	print("rtc of Rel ="), nl, print_rel(RTC_Rel), nl,
	print("Rel2 ="), nl, print_rel(Rel2), nl,
	print("composition of Rel1 and Rel2 ="), nl,
			print_rel(ComposedRel), nl,
	( { relation__is_dag(Rel) } ->
		io__write_string("Error: relation__is_dag(Rel) succeeded\n")
	;
		io__write_string("relation__is_dag(Rel) failed as expected\n")
	),
	( { relation__is_dag(Rel2) } ->
		io__write_string("relation__is_dag(Rel) succeeded\n")
	;
		io__write_string("Error: relation__is_dag(Rel2) failed\n")
	).

:- pred print_rel(relation(T), state, state).
:- mode print_rel(in, di, uo) is det.

print_rel(Relation) -->
	{ relation__to_assoc_list(Relation, AssocList0) },
	{ list__sort(AssocList0, AssocList) },
	write_list(AssocList, "\n", print), nl.

