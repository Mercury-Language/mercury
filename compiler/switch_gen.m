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
%	For switches on discrmiminated union types, we generate a chain of
%	if-then-elses on the primary tags but then use the secondary tag
%	to index into a jump table if the table is big enough.
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
	;	tag_switch.

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
		{ Indexing = yes },
		{ SwitchCategory = tag_switch },
		{ list__length(TaggedCases, NumCases) },
		{ NumCases > 8 }	% XXX heuristic - investigate
	->
		switch_gen__generate_tag_switch(TaggedCases,
			CaseVar, Det, LocalDet, EndLabel, Code)
	;
		% To generate a switch, first we flush the
		% variable on whose tag we are going to switch, then we
		% generate the cases for the switch.

		switch_gen__generate_all_cases(TaggedCases,
			CaseVar, Det, LocalDet, EndLabel, Code)
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
switch_gen__type_cat_to_switch_cat(usertype(_), tag_switch).

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
switch_gen__priority(pred_constant(_, _), 6).

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
		error("switch_gen__type_range: enum type is not d.u. type?")
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

%---------------------------------------------------------------------------%

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
			code_gen__generate_forced_goal(Det, Goal, GoalCode)
		;
			code_info__grab_code_info(CodeInfo),
			code_gen__generate_forced_goal(Det, Goal, GoalCode),
			code_info__slap_code_info(CodeInfo)
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

% where is the secondary tag for this primary tag value
:- type stag_loc ---> none ; local ; remote.

% map secondary tag values (-1 stands for none) to their goal
:- type tag_goal_map   == map(int, hlds__goal).
:- type tag_goal_list  == assoc_list(int, hlds__goal).

% map primary tag values to the set of their goals
:- type tag_case_map   == map(tag_bits, pair(stag_loc, tag_goal_map)).
:- type tag_case_list  == assoc_list(tag_bits, pair(stag_loc, tag_goal_map)).

% map primary tag values to the number of constructors sharing them
:- type tag_count_map  == map(tag_bits, pair(stag_loc, int)).
:- type tag_count_list == assoc_list(tag_bits, pair(stag_loc, int)).

	% Generate intelligent indexing code for tag based switches.

:- pred switch_gen__generate_tag_switch(list(extended_case), var,
	category, category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_tag_switch(in, in, in, in, in, out, in, out)
	is det.

switch_gen__generate_tag_switch(Cases, Var, Det, LocalDet, EndLabel, Code) -->
	% group the cases based on primary tag value
	% and find out how many constructors share each primary tag value
	switch_gen__get_tag_counts(Var, TagCountMap),
	{ map__to_assoc_list(TagCountMap, TagCountList) },
	{ map__init(TagCaseMap0) },
	{ switch_gen__group_tags(Cases, TagCaseMap0, TagCaseMap) },
	{ switch_gen__order_tags(TagCountList, TagCaseMap, TagCaseList) },

	code_info__get_next_label(FailLabel),
	code_info__produce_variable(Var, VarCode, Rval),
	{ Rval = lval(LvalPrime) ->
		Lval = LvalPrime
	;
		error("Rval is not an lval in switch_gen__generate_tag_switch")
	},
	switch_gen__generate_primary_tag_codes(TagCaseList, Var, Lval,
		Det, LocalDet, EndLabel, FailLabel, TagCountMap, CasesCode),
	% we generate FailCode and EndCode here because the last case within
	% a primary tag may not be the last case overall
	( { LocalDet = deterministic } ->
		{ FailCode = empty }
	;
		code_info__generate_failure(FailCode1),
		{ FailCode = tree(
			node([label(FailLabel) - "switch has failed"]),
			FailCode1) }
	),
	{ EndCode = node([label(EndLabel) - "end of tag switch"]) },
	{ Code = tree(tree(VarCode, CasesCode), tree(FailCode, EndCode)) }.

	% Generate a series of if-then-elses, one for each primary tag value.
	% Jump tables are used only on secondary tags.

:- pred switch_gen__generate_primary_tag_codes(tag_case_list,
	var, lval, category, category, label, label,
	tag_count_map, code_tree, code_info, code_info).
:- mode switch_gen__generate_primary_tag_codes(in, in, in, in, in, in, in,
	in, out, in, out) is det.

switch_gen__generate_primary_tag_codes([], _Var, _Lval, _Det, _LocalDet,
		_EndLabel, _FailLabel, _TagCountMap, empty) -->
	[].
switch_gen__generate_primary_tag_codes([TagGroup | TagGroups], Var, Lval,
		Det, LocalDet, EndLabel, FailLabel, TagCountMap, Code) -->
	{ TagGroup = Primary - (StagLoc - TagGoalMap) },
	{ map__lookup(TagCountMap, Primary, CountInfo) },
	{ CountInfo = StagLoc1 - MaxSecondary },
	{ StagLoc = StagLoc1 ->
		true
	;
		error("secondary tag locations differ in switch_gen__generate_primary_tag_codes")
	},
	(
		{ TagGroups = [_|_] ; LocalDet = semideterministic }
	->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		% XXX may be able to dispense with the tag operation
		{ TestRval = binop(ne, unop(tag, lval(Lval)),
			unop(mktag, const(int_const(Primary)))) },
		{ TestCode = node([if_val(TestRval, label(ElseLabel))
			- "test primary tag only"]) },
		switch_gen__generate_primary_tag_code(TagGoalMap,
			Primary, MaxSecondary, StagLoc, Lval, Det,
			EndLabel, FailLabel, TagCode),
		{ ElseCode = node([
			goto(label(EndLabel)) - "skip to end of tag switch",
			label(ElseLabel) - "handle next primary tag"]) },
		{ ThisTagCode = tree(tree(TestCode, TagCode), ElseCode) },
		( { TagGroups = [_|_] } ->
			code_info__slap_code_info(CodeInfo)
		;
			[]
		)
	;
		switch_gen__generate_primary_tag_code(TagGoalMap,
			Primary, MaxSecondary, StagLoc, Lval, Det,
			EndLabel, FailLabel, ThisTagCode)
	),
	switch_gen__generate_primary_tag_codes(TagGroups,
		Var, Lval, Det, LocalDet, EndLabel, FailLabel,
		TagCountMap, OtherTagsCode),
	{ Code = tree(ThisTagCode, OtherTagsCode) }.

:- pred switch_gen__generate_primary_tag_code(tag_goal_map, tag_bits, int,
	stag_loc, lval, category,
	label, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_primary_tag_code(in, in, in, in, in, in,
	in, in, out, in, out) is det.

switch_gen__generate_primary_tag_code(GoalMap, Primary, MaxSecondary,
		StagLoc, Lval, Det, EndLabel, FailLabel, Code) -->
	{ map__to_assoc_list(GoalMap, GoalList) },
	( { StagLoc = none } ->
		( { GoalList = [-1 - Goal] } ->
			code_gen__generate_forced_goal(Det, Goal, Code)
		;
			{ error("more than one goal for non-shared tag") }
		)
	; { MaxSecondary < 4 } ->
		switch_gen__generate_secondary_tag_tests(GoalList, Lval,
			Primary, StagLoc, Det, EndLabel, FailLabel, Code)
	;
		switch_gen__generate_secondary_tag_codes(GoalList,
			0, MaxSecondary, Det,
			EndLabel, FailLabel, Labels, CasesCode),
		{ StagLoc = remote ->
			Index = lval(field(Primary, Lval, 0))
		;
			Index = unop(unmkbody, lval(Lval))
		},
		{ SwitchCode = node([computed_goto(Index, Labels) -
			"switch on secondary tag"]) },
		{ Code = tree(SwitchCode, CasesCode) }
	).

:- pred switch_gen__generate_secondary_tag_tests(tag_goal_list, lval, tag_bits,
	stag_loc, category, label, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_secondary_tag_tests(in, in, in,
	in, in, in, in, out, in, out) is det.

switch_gen__generate_secondary_tag_tests([], _Lval, _Primary,
		_StagLoc, _Det, _EndLabel, _FailLabel, empty) -->
	[].
switch_gen__generate_secondary_tag_tests([Case0 | Cases0], Lval, Primary,
		StagLoc, Det, EndLabel, FailLabel, Code) -->
	{ Case0 = Secondary - Goal },
	( { Cases0 = [_|_] ; Det = semideterministic } ->
		code_info__grab_code_info(CodeInfo),
		code_info__get_next_label(ElseLabel),
		{ StagLoc = remote ->
			TestCode = node([if_val(binop(ne,
				field(Primary, lval(Lval), 0),
				const(int_const(Secondary))),
				label(ElseLabel)) - "test remote sec tag only"])
		;
			TestCode = node([if_val(binop(ne,
				unop(unmkbody, lval(Lval)),
				const(int_const(Secondary))),
				label(ElseLabel)) - "test local sec tag only"])
		},
		code_gen__generate_forced_goal(Det, Goal, GoalCode),
		{ ElseCode = node([
			goto(label(EndLabel)) - "skip to end of tag switch",
			label(ElseLabel) - "handle next secondary tag"]) },
		{ ThisCode = tree(TestCode, tree(GoalCode, ElseCode)) },
		( { Cases0 = [_|_] } ->
			code_info__slap_code_info(CodeInfo)
		;
			[]
		)
	;
		code_gen__generate_forced_goal(Det, Goal, ThisCode)
	),
	switch_gen__generate_secondary_tag_tests(Cases0, Lval,
		Primary, StagLoc, Det, EndLabel, FailLabel, OtherCode),
	{ Code = tree(ThisCode, OtherCode) }.

:- pred switch_gen__generate_secondary_tag_codes(tag_goal_list, int, int,
	category, label, label, list(label), code_tree, code_info, code_info).
:- mode switch_gen__generate_secondary_tag_codes(in, in, in,
	in, in, in, out, out, in, out) is det.

switch_gen__generate_secondary_tag_codes(CaseList, CurSecondary, MaxSecondary,
		Det, EndLabel, FailLabel, Labels, Code) -->
	( { CurSecondary > MaxSecondary } ->
		{ CaseList = [] ->
			true
		;
			error("caselist not empty when reaching limiting secondary tag")
		},
		{ Labels = [] },
		{ Code = empty }
	;
		{ NextSecondary is CurSecondary + 1 },
		( { CaseList = [CurSecondary - Goal | CaseList1] } ->
			code_info__get_next_label(NewLabel),
			( { CaseList1 = [] } ->
				code_gen__generate_forced_goal(Det, Goal, GoalCode)
			;
				code_info__grab_code_info(CodeInfo),
				code_gen__generate_forced_goal(Det, Goal, GoalCode),
				code_info__slap_code_info(CodeInfo)
			),
			{ LabelCode = node([label(NewLabel) -
				"start of a case in tag switch"]) },
			{ GotoCode = node([goto(label(EndLabel)) -
				"branch to end of tag switch"]) },
			switch_gen__generate_secondary_tag_codes(CaseList1,
				NextSecondary, MaxSecondary, Det,
				EndLabel, FailLabel, OtherLabels, OtherCode),
			{ Labels = [NewLabel | OtherLabels] },
			{ Code = tree(tree(LabelCode, GoalCode),
				tree(GotoCode, OtherCode)) }
		;
			switch_gen__generate_secondary_tag_codes(CaseList,
				NextSecondary, MaxSecondary, Det,
				EndLabel, FailLabel, OtherLabels, Code),
			{ Labels = [FailLabel | OtherLabels] }
		)
	).

:- pred switch_gen__get_tag_counts(var, tag_count_map,
	code_info, code_info).
:- mode switch_gen__get_tag_counts(in, out, in, out) is det.

switch_gen__get_tag_counts(Var, TagCountMap) -->
	code_info__variable_type(Var, Type),
	{ type_to_type_id(Type, TypeIdPrime, _) ->
		TypeId = TypeIdPrime
	;
		error("unknown type in switch_gen__get_tag_counts")
	},
	code_info__get_module_info(ModuleInfo),
	{ module_info_types(ModuleInfo, TypeTable) },
	{ map__lookup(TypeTable, TypeId, TypeDefn) },
	{ TypeDefn = hlds__type_defn(_, _, du_type(_, ConsTable, _), _, _) ->
		map__to_assoc_list(ConsTable, ConsList),
		switch_gen__cons_list_to_tag_list(ConsList, TagList)
	;
		error("non-du type in switch_gen__get_tag_counts")
	},
	{ map__init(TagCountMap0) },
	{ switch_gen__get_tag_counts_2(TagList, TagCountMap0, TagCountMap) }.

:- pred switch_gen__get_tag_counts_2(list(cons_tag),
	tag_count_map, tag_count_map).
:- mode switch_gen__get_tag_counts_2(in, in, out) is det.

switch_gen__get_tag_counts_2([], TagCountMap, TagCountMap).
switch_gen__get_tag_counts_2([ConsTag | TagList], TagCountMap0, TagCountMap) :-
	( ConsTag = simple_tag(Primary) ->
		( map__search(TagCountMap0, Primary, _) ->
			error("simple tag is shared")
		;
			% write('tag count mapping '),
			% write(Primary),
			% write(' to none '),
			% write(-1),
			% nl,
			map__set(TagCountMap0, Primary, none - -1,
				TagCountMap1)
		)
	; ConsTag = complicated_tag(Primary, Secondary) ->
		( map__search(TagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			),
			int__max(Secondary, MaxSoFar, Max),
			% write('tag count mapping '),
			% write(Primary),
			% write(' to remote max '),
			% write(Max),
			% nl,
			map__set(TagCountMap0, Primary, remote - Max,
				TagCountMap1)
		;
			% write('tag count mapping '),
			% write(Primary),
			% write(' to remote start '),
			% write(Secondary),
			% nl,
			map__set(TagCountMap0, Primary, remote - Secondary,
				TagCountMap1)
		)
	; ConsTag = complicated_constant_tag(Primary, Secondary) ->
		( map__search(TagCountMap0, Primary, Target) ->
			Target = TagType - MaxSoFar,
			( TagType = local ->
				true
			;
				error("local tag is shared with non-local")
			),
			int__max(Secondary, MaxSoFar, Max),
			% write('tag count mapping '),
			% write(Primary),
			% write(' to local max '),
			% write(Max),
			% nl,
			map__set(TagCountMap0, Primary, local - Max,
				TagCountMap1)
		;
			% write('tag count mapping '),
			% write(Primary),
			% write(' to local start '),
			% write(Secondary),
			% nl,
			map__set(TagCountMap0, Primary, local - Secondary,
				TagCountMap1)
		)
	;
		error("non-du tag in switch_gen__get_tag_counts_2")
	),
	switch_gen__get_tag_counts_2(TagList, TagCountMap1, TagCountMap).

:- pred switch_gen__cons_list_to_tag_list(assoc_list(cons_id, cons_tag),
	list(cons_tag)).
:- mode switch_gen__cons_list_to_tag_list(in, out) is det.

switch_gen__cons_list_to_tag_list([], []).
switch_gen__cons_list_to_tag_list([_ConsId - ConsTag | ConsList],
		[ConsTag | Tagslist]) :-
	switch_gen__cons_list_to_tag_list(ConsList, Tagslist).

:- pred switch_gen__group_tags(cases_list, tag_case_map, tag_case_map).
:- mode switch_gen__group_tags(in, in, out) is det.

switch_gen__group_tags([], TagCaseMap, TagCaseMap).
switch_gen__group_tags([Case0 | Cases0], TagCaseMap0, TagCaseMap) :-
	Case0 = case(_Priority, Tag, _ConsId, Goal),
	( Tag = simple_tag(Primary) ->
		( map__search(TagCaseMap0, Primary, _Group) ->
			error("simple tag is shared")
		;
			true
		),
		% write('tag case mapping '),
		% write(Primary),
		% write(' to none'),
		% nl,
		map__init(TagGoalMap0),
		map__set(TagGoalMap0, -1, Goal, TagGoalMap),
		map__set(TagCaseMap0, Primary, none - TagGoalMap, TagCaseMap1)
	; Tag = complicated_tag(Primary, Secondary) ->
		( map__search(TagCaseMap0, Primary, Group) ->
			Group = StagLoc - TagGoalMap0,
			( StagLoc = remote ->
				true
			;
				error("remote tag is shared with non-remote")
			)
		;
			map__init(TagGoalMap0)
		),
		% write('tag case mapping '),
		% write(Primary),
		% write(' to remote'),
		% nl,
		map__set(TagGoalMap0, Secondary, Goal, TagGoalMap),
		map__set(TagCaseMap0, Primary, remote - TagGoalMap, TagCaseMap1)
	; Tag = complicated_constant_tag(Primary, Secondary) ->
		( map__search(TagCaseMap0, Primary, Group) ->
			Group = StagLoc - TagGoalMap0,
			( StagLoc = local ->
				true
			;
				error("local tag is shared with non-local")
			)
		;
			map__init(TagGoalMap0)
		),
		% write('tag case mapping '),
		% write(Primary),
		% write(' to local'),
		% nl,
		map__set(TagGoalMap0, Secondary, Goal, TagGoalMap),
		map__set(TagCaseMap0, Primary, local - TagGoalMap, TagCaseMap1)
	;
		error("non-du tag in switch_gen__group_tags")
	),
	switch_gen__group_tags(Cases0, TagCaseMap1, TagCaseMap).

	% Order the most primary tags based on the number of secondary tags
	% associated with them. We use selection sort.
	% Note that it is not an error for a primary tag to have no case list.

:- pred switch_gen__order_tags(tag_count_list, tag_case_map, tag_case_list).
:- mode switch_gen__order_tags(in, in, out) is det.

switch_gen__order_tags(TagCountList0, TagCaseMap0, TagCaseList) :-
	(
		switch_gen__select_frequent_tag(TagCountList0,
			Primary, _, TagCountList1)
	->
		( map__search(TagCaseMap0, Primary, TagCase) ->
			map__delete(TagCaseMap0, Primary, TagCaseMap1),
			switch_gen__order_tags(TagCountList1, TagCaseMap1,
				TagCaseList1),
			TagCaseList = [Primary - TagCase | TagCaseList1]
		;
			switch_gen__order_tags(TagCountList1, TagCaseMap0,
				TagCaseList)
		)
	;
		( map__is_empty(TagCaseMap0) ->
			TagCaseList = []
		;
			error("TagCaseMap0 is not empty in switch_gen__order_tags")
		)
	).

	% Select the most frequently used primary tag based on the number of
	% secondary tags associated with it.

:- pred switch_gen__select_frequent_tag(tag_count_list, tag_bits, int,
	tag_count_list).
:- mode switch_gen__select_frequent_tag(in, out, out, out) is semidet.

switch_gen__select_frequent_tag([TagCount0 | TagCountList1], Primary, Count,
		TagCountList) :-
	TagCount0 = Primary0 - (_ - Count0),
	(
		switch_gen__select_frequent_tag(TagCountList1,
			Primary1, Count1, TagCountList2),
		Count1 > Count0
	->
		Primary = Primary1,
		Count = Count1,
		TagCountList = [TagCount0 | TagCountList2]
	;
		Primary = Primary0,
		Count = Count0,
		TagCountList = TagCountList1
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

:- pred switch_gen__generate_all_cases(list(extended_case), var,
	category, category, label, code_tree, code_info, code_info).
:- mode switch_gen__generate_all_cases(in, in, in, in, in, out, in, out) is det.

switch_gen__generate_all_cases(Cases, Var, Det, LocalDet, EndLabel, Code) -->
	code_info__produce_variable(Var, VarCode, _Rval),
	(
		{ Det = deterministic },
		{ LocalDet = deterministic },
		{ Cases = [Case1, Case2] }
	->
		{ Case1 = case(_, _, Cons1, Goal1) },
		code_info__get_next_label(ElseLab),
		code_info__push_failure_cont(known(ElseLab)),
		unify_gen__generate_tag_test(Var, Cons1, TestCode),
		code_info__pop_failure_cont,
		code_info__grab_code_info(CodeInfo),
		code_gen__generate_forced_goal(Det, Goal1, Case1Code),

		{ Case2 = case(_, _, _Cons2, Goal2) },
		code_info__slap_code_info(CodeInfo),
		code_gen__generate_forced_goal(Det, Goal2, Case2Code),

		{ tree__flatten(TestCode, TestListList) },
		{ list__condense(TestListList, TestList) },
		{ code_util__negate_the_test(TestList, RealTestList) },
		{ Code  = tree(
			tree(
				VarCode,
				tree(
					node(RealTestList),
					Case2Code)),
			tree(
				node([
					goto(label(EndLabel)) -
						"skip to the end of the switch",
					label(ElseLab) - "next case" ]),
				tree(
					Case1Code,
					node([ label(EndLabel) -
						"End of switch" ]))))
		}
	;
		switch_gen__generate_cases(Cases, Var, Det, LocalDet,
			EndLabel, CasesCode),
		{ Code = tree(VarCode, CasesCode) }
	).

:- pred switch_gen__generate_cases(list(extended_case), var,
	category, category, label, code_tree, code_info, code_info).
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
		code_info__get_next_label(ElseLabel),
		code_info__push_failure_cont(known(ElseLabel)),
		unify_gen__generate_tag_test(Var, Cons, TestCode),
		code_info__pop_failure_cont,
		code_gen__generate_forced_goal(Det, Goal, ThisCode),
		{ ElseCode = node([
			goto(label(EndLabel)) -
				"skip to the end of the switch",
			label(ElseLabel) - "next case"
		]) },
		{ ThisCaseCode = tree(tree(TestCode, ThisCode), ElseCode) },
		% If there are more cases, then we need to restore
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
