%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% File: switch_gen.nl
% Main authors: conway, fjh
%
% This module handles the generation of code for switches, which are
% disjunctions that do not require backtracking.  Switches are detected
% in switch_detection.nl.  This is the module that determines what
% sort of indexing to use for each switch and then actually generates the
% code.
%
% Currently only the following forms of indexing are used:
%
%	For switches on atomic data types (int, char, enums),
%	if the cases are not sparse, we use the value of the switch variable
%	to index into a jump table.
%
%	For switches on strings, we lookup the address to jump to in a
%	hash table, using open addressing to resolve hash collisions.
%
%	For all other cases (or if the --smart-indexing option was
%	disabled), we just generate a chain of if-then-elses.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module switch_gen.

:- interface.

:- import_module hlds, code_info.

:- pred switch_gen__generate_switch(category, var, category, list(case),
					code_tree, code_info, code_info).
:- mode switch_gen__generate_switch(in, in, in, in, out, in, out) is det.

%	switch_gen__generate_switch(Det, Var, LocalDet, Cases, Code):
%		Generate code for a switch statement.
%		Det is the determinism of the context.
%		LocalDet is the determinism of the switch itself,
%		ignoring the determism of the goals in the cases.
%		I.e. LocalDet is `det' if the switch covers all cases and
%		`semidet' otherwise.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, string, list, map, tree, std_util, require.
:- import_module llds, code_gen, unify_gen, type_util, code_util.
:- import_module globals, options.

%---------------------------------------------------------------------------%

:- type extended_case ---> case(int, cons_tag, cons_id, hlds__goal).
:- type cases_list == list(extended_case).

:- type switch_category
	--->	atomic_switch
	;	string_switch
	;	complicated_switch.

%---------------------------------------------------------------------------%

	% Choose which method to use to generate the switch.

switch_gen__generate_switch(Det, CaseVar, LocalDet, Cases, Code) -->
	switch_gen__determine_category(CaseVar, SwitchCategory),
	code_info__get_next_label(EndLabel),
	switch_gen__lookup_tags(Cases, CaseVar, TaggedCases0),
	{ list__sort(TaggedCases0, TaggedCases) },
	code_info__get_globals(Globals),
	{ globals__lookup_bool_option(Globals, smart_indexing, Indexing) },
	(
		{ Indexing = yes },
		{ SwitchCategory = atomic_switch },
		switch_gen__is_dense_switch(CaseVar, TaggedCases, LocalDet,
					FirstVal, LastVal, LocalDet1)
	->
		switch_gen__generate_dense_switch(TaggedCases,
				FirstVal, LastVal,
				CaseVar, Det, LocalDet1, EndLabel, Code)
	;
		{ Indexing = yes },
		{ SwitchCategory = string_switch },
		{ list__length(TaggedCases, NumCases) },
		{ NumCases > 5 }	% XXX heuristic - investigate
	->
		switch_gen__generate_string_switch(TaggedCases, CaseVar, Det,
			LocalDet, EndLabel, Code)
	;
		% To generate a switch, first we flush the
		% variable on whose tag we are going to switch, then we
		% generate the cases for the switch.

		code_info__produce_variable(CaseVar, VarCode, _Lval),
		switch_gen__generate_cases(TaggedCases,
				CaseVar, Det, LocalDet, EndLabel, CasesCode),
		{ Code = tree(VarCode, CasesCode) }
	),
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

	% We categorize switches according to whether the value
	% being switched on is an atomic type, a string, or
	% something more complicated.

:- pred switch_gen__determine_category(var, switch_category,
					code_info, code_info).
:- mode switch_gen__determine_category(in, out, in, out) is det.

switch_gen__determine_category(CaseVar, SwitchCategory) -->
	code_info__variable_type(CaseVar, Type),
	code_info__get_module_info(ModuleInfo),
	{ classify_type(Type, ModuleInfo, TypeCategory) },
	{ switch_gen__type_cat_to_switch_cat(TypeCategory, SwitchCategory) }.

:- pred switch_gen__type_cat_to_switch_cat(builtin_type, switch_category).
:- mode switch_gen__type_cat_to_switch_cat(in, out) is det.

switch_gen__type_cat_to_switch_cat(enumtype, atomic_switch).
switch_gen__type_cat_to_switch_cat(inttype,  atomic_switch).
switch_gen__type_cat_to_switch_cat(chartype, atomic_switch).
switch_gen__type_cat_to_switch_cat(strtype,  string_switch).
switch_gen__type_cat_to_switch_cat(usertype(_), complicated_switch).

%---------------------------------------------------------------------------%

:- pred switch_gen__lookup_tags(list(case), var, cases_list,
				code_info, code_info).
:- mode switch_gen__lookup_tags(in, in, out, in, out) is det.

switch_gen__lookup_tags([], _, []) --> [].
switch_gen__lookup_tags([Case | Cases], Var, [TaggedCase | TaggedCases]) -->
	{ Case = case(ConsId, Goal) },
	code_info__cons_id_to_tag(Var, ConsId, Tag),
	{ switch_gen__priority(Tag, Priority) },
	{ TaggedCase = case(Priority, Tag, ConsId, Goal) },
	switch_gen__lookup_tags(Cases, Var, TaggedCases).

%---------------------------------------------------------------------------%

:- pred switch_gen__priority(cons_tag, int).
:- mode switch_gen__priority(in, out) is det.

	% prioritize tag tests - the most efficient ones first.

switch_gen__priority(int_constant(_), 1).
switch_gen__priority(complicated_constant_tag(_, _), 1).
switch_gen__priority(simple_tag(_), 2).
switch_gen__priority(float_constant(_), 3).
switch_gen__priority(complicated_tag(_, _), 4).
switch_gen__priority(string_constant(_), 5).

%---------------------------------------------------------------------------%

	% Should this switch be implemented as a dense jump table?
	% If so, we return the starting and ending values for the table,
	% and whether the switch is still locally det or not
	% (we may convert locally semidet switches into locally det
	% switches by adding extra cases whose body is just `fail'.)

:- pred switch_gen__is_dense_switch(var, cases_list, category,
					int, int, category,
					code_info, code_info).
:- mode switch_gen__is_dense_switch(in, in, in, out, out, out,
					in, out) is semidet.

switch_gen__is_dense_switch(CaseVar, TaggedCases, LocalDet0,
				FirstVal, LastVal, LocalDet) -->
	{
		list__length(TaggedCases, NumCases),
		NumCases > 2,
		TaggedCases = [FirstCase | _],
		FirstCase = case(_, int_constant(FirstCaseVal), _, _),
		list__index1_det(TaggedCases, NumCases, LastCase),
		LastCase = case(_, int_constant(LastCaseVal), _, _),
		Span is LastCaseVal - FirstCaseVal,
		Range is Span + 1,
		Sparsity is Range // NumCases,
		Sparsity =< 10		% At least 1/10th of the table entries
					% should be real cases
	},
	( { LocalDet0 = semideterministic } ->
		% For semidet switches, we normally need to check that
		% the variable is in range before we index into the jump table.
		% However, if the range of the type is sufficiently small,
		% we can make the jump table large enough to hold all
		% of the values for the type.
		code_info__variable_type(CaseVar, Type),
		code_info__get_module_info(ModuleInfo),
		{ classify_type(Type, ModuleInfo, TypeCategory) },
		(
			switch_gen__type_range(TypeCategory, Type, TypeRange),
			{ DetSparsity is TypeRange // NumCases },
			{ DetSparsity =< 10 }
		->
			{ LocalDet = deterministic },
			{ FirstVal = 0 },
			{ LastVal is TypeRange - 1 }
		;
			{ LocalDet = LocalDet0 },
			{ FirstVal = FirstCaseVal },
			{ LastVal = LastCaseVal }
		)
	;
		{ LocalDet = LocalDet0 },
		{ FirstVal = FirstCaseVal },
		{ LastVal = LastCaseVal }
	).

%---------------------------------------------------------------------------%

	% Determine the range of an atomic type.
	% Fail if the type isn't the sort of type that has a range
	% or if the type's range is to big to switch on (e.g. int).
	%
	% XXX the size of `character' is hard-coded here.

:- pred switch_gen__type_range(builtin_type, type, int, code_info, code_info).
:- mode switch_gen__type_range(in, in, out, in, out) is semidet.

switch_gen__type_range(chartype, _, 128) --> [].
switch_gen__type_range(enumtype, Type, TypeRange) -->
	{ type_to_type_id(Type, TypeId0, _) ->
		TypeId = TypeId0
	;
		error("switch_gen__type_range: invalid enum type?")
	},
	code_info__get_module_info(ModuleInfo),
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) },
	{
		TypeDefn = hlds__type_defn(_, _,
			du_type(_, ConsTable, _), _, _)
	->
		map__count(ConsTable, TypeRange)
	;
		error(
		"switch_gen__type_range: enum type is not d.u. type?"
		)
	}.

%---------------------------------------------------------------------------%

	% Generate code for a switch using a dense jump table 

:- pred switch_gen__generate_dense_switch(cases_list, int, int,
					var, category, category, label,
					code_tree,
					code_info, code_info).
:- mode switch_gen__generate_dense_switch(in, in, in, in, in, in, in,
					out, in, out) is det.

switch_gen__generate_dense_switch(Cases, StartVal, EndVal,
				Var, Det, LocalDet, EndLabel, Code) -->
		% Evaluate the variable which we are going to be switching on
	code_info__produce_variable(Var, VarCode, Rval),
		% If the case values start at some number other than 0,
		% then subtract that number to give us a zero-based index
	{ StartVal = 0 ->
		Index = Rval
	;
		Index = binop(-, Rval, const(int_const(StartVal)))
	},
		% If the switch is not locally deterministic, we need to
		% check that the value of the variable lies within the
		% appropriate range
	( { LocalDet = semideterministic } ->
		{ Difference is EndVal - StartVal },
		code_info__generate_test_and_fail(
			binop(<=, unop(cast_to_unsigned, Index),
				const(int_const(Difference))), RangeCheck)
	;
		{ RangeCheck = empty }
	),
		% Now generate the jump table and the cases
	switch_gen__generate_dense_cases(Cases, StartVal, EndVal, Det, EndLabel,
			Labels, CasesCode),
	{ DoJump = node([
		computed_goto(Index, Labels)
			- "switch (using dense jump table)"
	]) },
		% Assemble to code together
	{ Code = tree(tree(VarCode, RangeCheck), tree(DoJump, CasesCode)) }.

:- pred switch_gen__generate_dense_cases(cases_list, int, int,
						category, label,
						list(label), code_tree,
						code_info, code_info).
:- mode switch_gen__generate_dense_cases(in, in, in, in, in, out, out,
						in, out) is det.

switch_gen__generate_dense_cases(Cases0, NextVal, EndVal, Det, EndLabel,
					Labels, Code) -->
	(
		{ NextVal > EndVal }
	->
		{ Code = node([ label(EndLabel) - "End of dense switch" ]) },
		{ Labels = [] }
	;
		code_info__get_next_label(ThisLabel),
		switch_gen__generate_dense_case(Cases0, NextVal, Det,
					Cases1, ThisCode, Comment),
		{ ThisCaseCode = tree(
			node([ label(ThisLabel) - Comment ]),
			tree(	ThisCode,
				node([ goto(label(EndLabel))
				- "branch to end of dense switch" ])
			)
		) },
			% generate the rest of the cases.
		{ NextVal1 is NextVal + 1 },
		switch_gen__generate_dense_cases(Cases1, NextVal1, EndVal, Det,
					EndLabel, Labels1, OtherCasesCode),
		{ Labels = [ThisLabel | Labels1] },
		{ Code = tree(ThisCaseCode, OtherCasesCode) }
	).

:- pred switch_gen__generate_dense_case(cases_list, int, category, 
				cases_list, code_tree, string,
				code_info, code_info).
:- mode switch_gen__generate_dense_case(in, in, in, out, out, out,
				in, out) is det.

switch_gen__generate_dense_case(Cases0, NextVal, Det, Cases, Code, Comment) -->
	(
		{ Cases0 = [Case | Cases1] },
		{ Case = case(_, int_constant(NextVal), _, Goal) }
	->
		% For every case except the last, we need to save the
		% expression cache, etc., and restore them when we've finished
		(
			{ Cases1 = [_|_] }
		->
			{ Comment = "case of dense switch" },
			code_info__grab_code_info(CodeInfo),
			code_gen__generate_forced_goal(Det, Goal, Code),
			code_info__slap_code_info(CodeInfo)
		;
			{ Comment = "last case of dense switch" },
			code_gen__generate_forced_goal(Det, Goal, Code)
		),
		{ Cases = Cases1 }
	;
		% This case didn't occur in the original case list - just
		% generate a `fail' for it.
		{ Comment = "compiler-introduced `fail' case of dense switch" },
		code_info__generate_failure(CodeA),
		code_info__generate_forced_saves(CodeB),
		{ Code = tree(CodeA, CodeB) },
		code_info__remake_with_store_map,
		{ Cases = Cases0 }
	).


%---------------------------------------------------------------------------%

	% For strings, we generate a hash table using open addressing
	% to resolve hash conflicts.

:- pred switch_gen__generate_string_switch(cases_list, var, category,
			category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_string_switch(in, in, in, in, in, out,
			in, out) is det.

switch_gen__generate_string_switch(Cases, Var, Det, _LocalDet, EndLabel, Code)
		-->
	code_info__produce_variable(Var, VarCode, VarRval),
	code_info__get_free_register(SlotR),
	{ SlotReg = reg(SlotR) },
	code_info__get_free_register(StringR),
	{ StringReg = reg(StringR) },
	code_info__get_next_label(LoopLabel),
	code_info__get_next_label(FailLabel),
	code_info__get_next_label(JumpLabel),
	code_info__get_next_label_number(NextSlotsTableLabel),
	code_info__get_next_label_number(StringTableLabel),
	{
		% Determine how big to make the hash table.
		% Currently we round the number of cases up to the nearest power
		% of two, and then double it.  This should hopefully ensure that
		% we don't get too many hash collisions.
		%
		list__length(Cases, NumCases),
		int__log2(NumCases, LogNumCases),
		int__pow(2, LogNumCases, RoundedNumCases),
		TableSize is 2 * RoundedNumCases,
		HashMask is TableSize - 1,

		% Compute the hash table
		%
		switch_gen__hash_cases(Cases, TableSize, HashValsMap),
		map__to_assoc_list(HashValsMap, HashValsList),
		switch_gen__calc_hash_slots(HashValsList, HashValsMap,
					HashSlotsMap)
	},

		% Generate the code etc. for the hash table
		%
	switch_gen__gen_hash_slots(0, TableSize, HashSlotsMap,
				Det, FailLabel, EndLabel,
				Strings, Labels, NextSlots, SlotsCode),

		% Generate code which does the hash table lookup
	{
		NextSlotsTable = create(0, NextSlots, NextSlotsTableLabel),
		StringTable = create(0, Strings, StringTableLabel),
		HashLookupCode = node([
			comment("hashed string switch") -
			  "",
			assign(SlotReg, binop(&, unop(hash_string, VarRval),
						const(int_const(HashMask)))) -
			  "compute the hash value of the input string",
			label(LoopLabel) -
			  "begin hash chain loop",
			assign(StringReg, binop(array_index, StringTable,
							lval(SlotReg))) -
			  "lookup the string for this hash slot",
			if_val(binop(and, lval(StringReg),
				binop(str_eq, lval(StringReg), VarRval)),
					label(JumpLabel)) -
			  "did we find a match?",
			assign(SlotReg, binop(array_index, NextSlotsTable,
							lval(SlotReg))) -
			  "not yet, so get next slot in hash chain",
			if_val(binop(>=, lval(SlotReg), const(int_const(0))),
				label(LoopLabel)) -
			  "keep searching until we reach the end of the chain",
			label(FailLabel) -
			  "no match, so fail"
		])
	},
	code_info__generate_failure(FailCodeA),
	code_info__generate_forced_saves(FailCodeB),
	{ FailCode = tree(FailCodeA, FailCodeB) },
	code_info__remake_with_store_map,
	{
		JumpCode = node([
			label(JumpLabel) -
				"we found a match",
			computed_goto(lval(SlotReg), Labels) -
				"jump to the corresponding code"
		])
	},
		% Collect all the generated code fragments together
	{ Code = tree(tree(VarCode, tree(HashLookupCode, FailCode)),
			tree(JumpCode, SlotsCode))
	}.

:- pred switch_gen__hash_cases(cases_list, int, map(int, cases_list)).
:- mode switch_gen__hash_cases(in, in, out) is det.

switch_gen__hash_cases([], _, Map) :-
	map__init(Map).
switch_gen__hash_cases([Case | Cases], TableSize, Map) :-
	switch_gen__hash_cases(Cases, TableSize, Map0),
	( Case = case(_, string_constant(String0), _, _) ->
		String = String0
	;
		error("switch_gen__hash_cases: non-string case?")
	),
	string__hash(String, HashVal0),
	HashVal is HashVal0 mod TableSize,
	( map__search(Map0, HashVal, CaseList0) ->
		map__set(Map0, HashVal, [Case | CaseList0], Map)
	;
		map__set(Map0, HashVal, [Case], Map)
	).

:- type hash_slot ---> hash_slot(extended_case, int).

:- pred switch_gen__calc_hash_slots(assoc_list(int, cases_list),
					map(int, cases_list),
					map(int, hash_slot)).
:- mode switch_gen__calc_hash_slots(in, in, out) is det.

	% switch_gen__calc_hash_slots(AssocList, HashMap, Map) :-
	%	For each (HashVal - Case) pair in AssocList,
	%	allocate a hash slot in Map for the case, as follows.
	%	If the hash slot corresponding to HashVal is not
	%	already used, then use that one.  Otherwise, find
	%	the next spare slot (making sure that we don't
	%	use slots which can be used for a direct match with
	%	the hash value for one of the other cases), and
	%	use it instead.  Keep track of the hash chains
	%	as we do this.

switch_gen__calc_hash_slots(HashValList, HashMap, Map) :-
	map__init(Map0),
	switch_gen__calc_hash_slots_1(HashValList, HashMap, Map0, 0, Map, _).

:- pred switch_gen__calc_hash_slots_1(assoc_list(int, cases_list),
					map(int, cases_list),
					map(int, hash_slot), int,
					map(int, hash_slot), int).
:- mode switch_gen__calc_hash_slots_1(in, in, in, in, out, out) is det.

switch_gen__calc_hash_slots_1([], _, Map, LastUsed, Map, LastUsed).
switch_gen__calc_hash_slots_1([HashVal-Cases | Rest], HashMap, Map0, LastUsed0,
		Map, LastUsed) :-
	switch_gen__calc_hash_slots_2(Cases, HashVal, HashMap, Map0, LastUsed0,
			Map1, LastUsed1),
	switch_gen__calc_hash_slots_1(Rest, HashMap, Map1, LastUsed1,
			Map, LastUsed).

:- pred switch_gen__calc_hash_slots_2(cases_list, int,
					map(int, cases_list),
					map(int, hash_slot), int,
					map(int, hash_slot), int).
:- mode switch_gen__calc_hash_slots_2(in, in, in, in, in, out, out) is det.

switch_gen__calc_hash_slots_2([], _HashVal, _HashMap, Map, LastUsed,
				Map, LastUsed).
switch_gen__calc_hash_slots_2([Case | Cases], HashVal, HashMap, Map0, LastUsed0,
				Map, LastUsed) :-
	switch_gen__calc_hash_slots_2(Cases, HashVal, HashMap, Map0, LastUsed0,
				Map1, LastUsed1),
	( map__contains(Map1, HashVal) ->
		switch_gen__follow_hash_chain(Map1, HashVal, ChainEnd),
		switch_gen__next_free_hash_slot(Map1, HashMap, LastUsed1, Next),
		map__lookup(Map1, ChainEnd, hash_slot(PrevCase, _)),
		map__set(Map1, ChainEnd, hash_slot(PrevCase, Next), Map2),
		map__set(Map2, Next, hash_slot(Case, -1), Map),
		LastUsed = Next
	;
		map__set(Map1, HashVal, hash_slot(Case, -1), Map),
		LastUsed = LastUsed1
	).

:- pred switch_gen__follow_hash_chain(map(int, hash_slot), int, int).
:- mode switch_gen__follow_hash_chain(in, in, out) is det.

switch_gen__follow_hash_chain(Map, Slot, LastSlot) :-
	map__lookup(Map, Slot, hash_slot(_, NextSlot)),
	(
		NextSlot >= 0,
		map__contains(Map, NextSlot)
	->
		switch_gen__follow_hash_chain(Map, NextSlot, LastSlot)
	;
		LastSlot = Slot
	).

	% next_free_hash_slot(M, H_M, LastUsed, FreeSlot) :-
	%	Find the next available slot FreeSlot in the hash table
	%	which is not already used (contained in M) and which is not
	%	going to be used a primary slot (contained in H_M),
	%	starting at the slot after LastUsed.

:- pred switch_gen__next_free_hash_slot(map(int, hash_slot),
					map(int, cases_list), int, int).
:- mode switch_gen__next_free_hash_slot(in, in, in, out) is det.

switch_gen__next_free_hash_slot(Map, H_Map, LastUsed, FreeSlot) :-
	NextSlot is LastUsed + 1,
	(
		\+ map__contains(Map, NextSlot),
		\+ map__contains(H_Map, NextSlot)
	->
		FreeSlot = NextSlot
	;
		switch_gen__next_free_hash_slot(Map, H_Map, NextSlot, FreeSlot)
	).


:- pred switch_gen__gen_hash_slots(int, int, map(int, hash_slot),
				category, label, label,
				list(maybe(rval)), list(label),
				list(maybe(rval)), code_tree,
				code_info, code_info).
:- mode switch_gen__gen_hash_slots(in, in, in, in, in, in,
				out, out, out, out,
				in, out) is det.

switch_gen__gen_hash_slots(Slot, TableSize, HashSlotMap,
			Det, FailLabel, EndLabel,
			Strings, Labels, NextSlots, Code) -->
	( { Slot = TableSize } ->
		{
			Strings = [],
			Labels = [],
			NextSlots = [],
			Code = node([
				label(EndLabel) - "end of hashed string switch"
			])
		}
	;
		switch_gen__gen_hash_slot(Slot, TableSize, HashSlotMap,
					Det, FailLabel, EndLabel,
					String, Label, NextSlot, SlotCode),
		{ Slot1 is Slot + 1 },
		{ 
			Strings = [String | Strings0],
			Labels = [Label | Labels0],
			NextSlots = [NextSlot | NextSlots0],
			Code = tree(SlotCode, Code0)
		},
		switch_gen__gen_hash_slots(Slot1, TableSize, HashSlotMap,
					Det, FailLabel, EndLabel,
					Strings0, Labels0, NextSlots0, Code0)
	).

:- pred switch_gen__gen_hash_slot(int, int, map(int, hash_slot),
				category, label, label,
				maybe(rval), label, maybe(rval), code_tree,
				code_info, code_info).
:- mode switch_gen__gen_hash_slot(in, in, in, in, in, in,
				out, out, out, out,
				in, out) is det.

switch_gen__gen_hash_slot(Slot, TblSize, HashSlotMap, Det, FailLabel, EndLabel,
			yes(StringRval), Label, yes(NextSlotRval), Code) -->
	(
		{ map__search(HashSlotMap, Slot, hash_slot(Case, Next)) }
	->
		{ NextSlotRval = const(int_const(Next)) },
		{ Case = case(_, ConsTag, _, Goal) },
		{ ConsTag = string_constant(String0) ->
			String = String0
		;
			error("switch_gen__gen_hash_slots: string expected")
		},
		{ StringRval = const(string_const(String)) },
		code_info__get_next_label(Label),
		{ string__append_list(["case \"", String, "\""], Comment) },
		{ LabelCode = node([
			label(Label) - Comment
		]) },
		{ FinishCode = node([
			goto(label(EndLabel)) - "jump to end of switch"
		]) },
		(
			{ switch_gen__this_is_last_case(Slot, TblSize,
				HashSlotMap) }
		->
			code_info__grab_code_info(CodeInfo),
			code_gen__generate_forced_goal(Det, Goal, GoalCode),
			code_info__slap_code_info(CodeInfo)
		;
			code_gen__generate_forced_goal(Det, Goal, GoalCode)
		),
		{ Code = tree(LabelCode, tree(GoalCode, FinishCode)) }
	;
		{ StringRval = const(int_const(0)) },
		{ Label = FailLabel },
		{ NextSlotRval = const(int_const(-2)) },
		{ Code = empty }
	).


:- pred switch_gen__this_is_last_case(int, int, map(int, hash_slot)).
:- mode switch_gen__this_is_last_case(in, in, in) is semidet.

switch_gen__this_is_last_case(Slot, TableSize, Table) :-
	Slot1 is Slot + 1,
	( Slot1 >= TableSize ->
		true
	;
		\+ map__contains(Table, Slot1),
		switch_gen__this_is_last_case(Slot1, TableSize, Table)
	).

%---------------------------------------------------------------------------%

	% Generate a switch as a chain of if-then-elses.
	%
	% To generate a case for a switch we generate
	% code to do a tag-test and fall through to the next case in
	% the event of failure.
	%
	% Each case except the last consists of a tag test, followed by
	% the goal for that case, followed by a branch to the end of
	% the switch. The goal is generated as a "forced" goal which
	% ensures that all variables which are live at the end of the
	% case get stored in their stack slot.  For the last case, if
	% the switch is locally deterministic, then we don't need to
	% generate the tag test, and we never need to generate the
	% branch to the end of the switch.  After the last case, we put
	% the end-of-switch label which other cases branch to after
	% their case goals.

:- pred switch_gen__generate_cases(list(extended_case), var, category,
			category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_cases(in, in, in, in, in, out, in, out) is det.

	% At the end of a locally semidet switch, we fail because we
	% came across a tag which was not covered by one of the cases.
	% It is followed by the end of switch label to which the cases
	% branch.
switch_gen__generate_cases([], _Var, _Det, LocalDet, EndLabel, Code) -->
	( { LocalDet = semideterministic } ->
		code_info__generate_failure(FailCode)
	;
		{ FailCode = empty }
	),
	{ Code = tree(FailCode, node([ label(EndLabel) -
		"End of switch" ])) }.

	% A case consists of a tag-test followed by a 
	% goal and a label for the start of the next case.
switch_gen__generate_cases([case(_, _, Cons, Goal)|Cases], Var, Det, LocalDet, 
					EndLabel, CasesCode) -->
	(
		{ Cases = [_|_] ; LocalDet = semideterministic }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLab),
		code_info__push_failure_cont(known(ElseLab)),
		unify_gen__generate_tag_test(Var, Cons, TestCode),
		code_info__pop_failure_cont,
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ElseLabel = node([
			goto(label(EndLabel)) -
				"skip to the end of the switch",
			label(ElseLab) - "next case"
		]) },
		{ ThisCaseCode = tree(tree(TestCode, ThisCode), ElseLabel) },
		% If there are more cases, the we need to restore
		% the expression cache, etc.
		( { Cases = [_|_] } ->
			code_info__slap_code_info(CodeInfo)
		;
			[]
		)
	;
		code_gen__generate_forced_goal(Det, Goal, ThisCaseCode)
	),
		% generate the rest of the cases.
	switch_gen__generate_cases(Cases, Var, Det, LocalDet, EndLabel,
		CasesCode0),
	{ CasesCode = tree(ThisCaseCode, CasesCode0) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
