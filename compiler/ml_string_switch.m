%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: ml_string_switch.m
% author: fjh (adapted from string_switch.m)

% For switches on strings, we generate a hash table using open addressing
% to resolve hash conflicts.

% WARNING: the code here is quite similar to the code in string_switch.m.
% Any changes here may require similar changes there and vice versa.

%-----------------------------------------------------------------------------%

:- module ml_string_switch.

:- interface.

:- import_module prog_data.
:- import_module hlds_data.
:- import_module mlds, ml_code_util, ml_switch_gen.
:- import_module llds. % XXX for code_model.

:- pred ml_string_switch__generate(ml_cases_list::in, prog_var::in,
		code_model::in, can_fail::in, prog_context::in,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_code_gen, builtin_ops, type_util.
:- import_module globals, options.

:- import_module bool, int, string, list, map, std_util, assoc_list, require.

ml_string_switch__generate(Cases, Var, CodeModel, _CanFail, Context,
		MLDS_Decls, MLDS_Statements) -->
	{ MLDS_Context = mlds__make_context(Context) },
	%
	% Compute the value we're going to switch on
	%
	ml_gen_var(Var, VarLval),
	{ VarRval = lval(VarLval) },

	%
	% Generate the following local variable declarations:
	%	int slot;
	%	MR_String str;
	%
	ml_gen_info_new_cond_var(SlotVarSeq),
	{ SlotVarName = string__format("slot_%d", [i(SlotVarSeq)]) },
	{ SlotVarDefn = ml_gen_mlds_var_decl(var(SlotVarName),
		mlds__native_int_type, MLDS_Context) },
	ml_qualify_var(SlotVarName, SlotVarLval),

	ml_gen_info_new_cond_var(StringVarSeq),
	{ StringVarName = string__format("str_%d", [i(StringVarSeq)]) },
	{ StringVarDefn = ml_gen_mlds_var_decl(var(StringVarName),
		ml_string_type, MLDS_Context) },
	ml_qualify_var(StringVarName, StringVarLval),

	%
	% Generate new labels
	%
	ml_gen_new_label(FailLabel),
	ml_gen_new_label(EndLabel),
	{ GotoEndStatement = mlds__statement(
		goto(EndLabel),
		MLDS_Context) },

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
		ml_string_switch__hash_cases(Cases, HashMask, HashValsMap),
		map__to_assoc_list(HashValsMap, HashValsList),
		ml_string_switch__calc_hash_slots(HashValsList, HashValsMap,
			HashSlotsMap)
	},

		% Generate the code for when the hash lookup fails.
		%
	ml_gen_failure(CodeModel, Context, FailStatements),

		% Generate the code etc. for the hash table
		%
	ml_string_switch__gen_hash_slots(0, TableSize, HashSlotsMap, CodeModel,
		Context, FailLabel, EndLabel, Strings, Labels, NextSlots,
		SlotsStatements, SlotsCases),

	%
	% Generate the following local constant declarations:
	%	static const int next_slots_table = { <NextSlots> };
	%	static const MR_String string_table = { <Strings> };
	%
	ml_gen_info_new_const(NextSlotsSeq),
	ml_format_static_const_name("next_slots_table", NextSlotsSeq,
		NextSlotsName),
	{ NextSlotsType = mlds__array_type(mlds__native_int_type) },
	{ NextSlotsDefn = ml_gen_static_const_defn(NextSlotsName,
		NextSlotsType,
		init_array(NextSlots), Context) },
	ml_qualify_var(NextSlotsName, NextSlotsLval),

	ml_gen_info_new_const(StringTableSeq),
	ml_format_static_const_name("string_table", StringTableSeq,
		StringTableName),
	{ StringTableType = mlds__array_type(ml_string_type) },
	{ StringTableDefn = ml_gen_static_const_defn(StringTableName,
		StringTableType, init_array(Strings), Context) },
	ml_qualify_var(StringTableName, StringTableLval),
	
	%
	% Generate code which does the hash table lookup
	% Note that we generate both the switch version and the
	% computed goto version, and then use whichever one is
	% appropriate for the target.
	%

	ml_gen_info_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, prefer_switch, PreferSwitch) },
	(
		{ target_supports_computed_goto(Globals) },
		\+ {
			PreferSwitch = yes,
			target_supports_int_switch(Globals)
		}
	->
		{ UseComputedGoto = yes },
		{
			FoundMatch = mlds__statement(
				block([], [
					mlds__statement(atomic(comment(
						"we found a match")),
						MLDS_Context),
					mlds__statement(atomic(comment(
					    "jump to the corresponding code")),
						MLDS_Context),
					mlds__statement(
					    computed_goto(lval(SlotVarLval),
							Labels),
						MLDS_Context)
				]),
				MLDS_Context)
		}
	;
		{ UseComputedGoto = no },
		{
			FoundMatch = mlds__statement(
				block([], [
					mlds__statement(atomic(comment(
						"we found a match")),
						MLDS_Context),
					mlds__statement(atomic(comment(
					"dispatch to the corresponding code")),
						MLDS_Context),
					mlds__statement(
						switch(mlds__native_int_type,
							lval(SlotVarLval),
							SlotsCases,
							default_is_unreachable),
						MLDS_Context),
					GotoEndStatement
				]),
				MLDS_Context)
		}
	),
	{
		LoopBody = ml_gen_block([], [
			mlds__statement(atomic(comment(
				"lookup the string for this hash slot")),
				MLDS_Context),
			mlds__statement(
				atomic(assign(StringVarLval,
					binop(array_index,
						lval(StringTableLval),
						lval(SlotVarLval)))),
				MLDS_Context),
			mlds__statement(atomic(comment(
				"did we find a match?")),
				MLDS_Context),
			mlds__statement(
				if_then_else(
					binop(and,
						binop(ne,
							lval(StringVarLval),
							const(null(
								ml_string_type))),
						binop(str_eq,
							lval(StringVarLval),
							VarRval)
					),
					FoundMatch,
					no
				),
				MLDS_Context),
			mlds__statement(atomic(comment(
			      "no match yet, so get next slot in hash chain")),
				MLDS_Context),
			mlds__statement(
				atomic(assign(SlotVarLval,
					binop(array_index,
						lval(NextSlotsLval),
						lval(SlotVarLval)))),
				MLDS_Context)
			],
			Context),
		HashLookupStatements = [
			mlds__statement(
				atomic(comment("hashed string switch")),
				MLDS_Context),
			mlds__statement(atomic(comment(
				"compute the hash value of the input string")),
				MLDS_Context),
			mlds__statement(
				atomic(assign(SlotVarLval, binop(&,
					unop(std_unop(hash_string), VarRval),
					const(int_const(HashMask))))),
				MLDS_Context),
			mlds__statement(atomic(comment(
				"hash chain loop")),
				MLDS_Context),
			mlds__statement(
				while(binop(>=, lval(SlotVarLval),
						const(int_const(0))),
					LoopBody,
					yes), % this is a do...while loop
				MLDS_Context)
			],
		FailLabelStatement =
			mlds__statement(
				label(FailLabel),
				MLDS_Context),
		FailComment =
			mlds__statement(
				atomic(comment("no match, so fail")),
				MLDS_Context),
		EndLabelStatement =
			mlds__statement(
				label(EndLabel),
				MLDS_Context),
		EndComment =
			mlds__statement(
				atomic(comment("end of hashed string switch")),
				MLDS_Context)
	},

	%
	% Collect all the generated variable/constant declarations
	% and code fragments together.
	%
	{ MLDS_Decls = [NextSlotsDefn, StringTableDefn,
		SlotVarDefn, StringVarDefn] },
	{ UseComputedGoto = yes ->
		MLDS_Statements =
			HashLookupStatements ++
			[FailLabelStatement, FailComment | FailStatements] ++
			[GotoEndStatement] ++
			SlotsStatements ++
			[EndLabelStatement, EndComment]
	;
		MLDS_Statements = HashLookupStatements ++
			[FailComment | FailStatements] ++
			[EndLabelStatement, EndComment]
	}.

:- pred ml_string_switch__hash_cases(ml_cases_list, int,
		map(int, ml_cases_list)).
:- mode ml_string_switch__hash_cases(in, in, out) is det.

ml_string_switch__hash_cases([], _, Map) :-
	map__init(Map).
ml_string_switch__hash_cases([Case | Cases], HashMask, Map) :-
	ml_string_switch__hash_cases(Cases, HashMask, Map0),
	( Case = case(_, string_constant(String0), _, _) ->
		String = String0
	;
		error("ml_string_switch__hash_cases: non-string case?")
	),
	string__hash(String, HashVal0),
	HashVal is HashVal0 /\ HashMask,
	( map__search(Map0, HashVal, CaseList0) ->
		map__det_update(Map0, HashVal, [Case | CaseList0], Map)
	;
		map__det_insert(Map0, HashVal, [Case], Map)
	).

:- type hash_slot ---> hash_slot(ml_extended_case, int).

:- pred ml_string_switch__calc_hash_slots(assoc_list(int, ml_cases_list),
	map(int, ml_cases_list), map(int, hash_slot)).
:- mode ml_string_switch__calc_hash_slots(in, in, out) is det.

	% ml_string_switch__calc_hash_slots(AssocList, HashMap, Map) :-
	%	For each (HashVal - Case) pair in AssocList,
	%	allocate a hash slot in Map for the case, as follows.
	%	If the hash slot corresponding to HashVal is not
	%	already used, then use that one.  Otherwise, find
	%	the next spare slot (making sure that we don't
	%	use slots which can be used for a direct match with
	%	the hash value for one of the other cases), and
	%	use it instead.  Keep track of the hash chains
	%	as we do this.

ml_string_switch__calc_hash_slots(HashValList, HashMap, Map) :-
	map__init(Map0),
	ml_string_switch__calc_hash_slots_1(HashValList, HashMap, Map0, 0,
		Map, _).

:- pred ml_string_switch__calc_hash_slots_1(assoc_list(int, ml_cases_list),
	map(int, ml_cases_list), map(int, hash_slot), int,
	map(int, hash_slot), int).
:- mode ml_string_switch__calc_hash_slots_1(in, in, in, in, out, out) is det.

ml_string_switch__calc_hash_slots_1([], _, Map, LastUsed, Map, LastUsed).
ml_string_switch__calc_hash_slots_1([HashVal-Cases | Rest], HashMap, Map0,
		LastUsed0, Map, LastUsed) :-
	ml_string_switch__calc_hash_slots_2(Cases, HashVal, HashMap, Map0,
		LastUsed0, Map1, LastUsed1),
	ml_string_switch__calc_hash_slots_1(Rest, HashMap, Map1,
		LastUsed1, Map, LastUsed).

:- pred ml_string_switch__calc_hash_slots_2(ml_cases_list, int,
		map(int, ml_cases_list), map(int, hash_slot), int,
		map(int, hash_slot), int).
:- mode ml_string_switch__calc_hash_slots_2(in, in, in, in, in, out, out) is det.

ml_string_switch__calc_hash_slots_2([], _HashVal, _HashMap, Map, LastUsed,
		Map, LastUsed).
ml_string_switch__calc_hash_slots_2([Case | Cases], HashVal, HashMap, Map0,
		LastUsed0, Map, LastUsed) :-
	ml_string_switch__calc_hash_slots_2(Cases, HashVal, HashMap, Map0,
		LastUsed0, Map1, LastUsed1),
	( map__contains(Map1, HashVal) ->
		ml_string_switch__follow_hash_chain(Map1, HashVal, ChainEnd),
		ml_string_switch__next_free_hash_slot(Map1, HashMap, LastUsed1,
			Next),
		map__lookup(Map1, ChainEnd, hash_slot(PrevCase, _)),
		map__det_update(Map1, ChainEnd, hash_slot(PrevCase, Next),
			Map2),
		map__det_insert(Map2, Next, hash_slot(Case, -1), Map),
		LastUsed = Next
	;
		map__det_insert(Map1, HashVal, hash_slot(Case, -1), Map),
		LastUsed = LastUsed1
	).

:- pred ml_string_switch__follow_hash_chain(map(int, hash_slot), int, int).
:- mode ml_string_switch__follow_hash_chain(in, in, out) is det.

ml_string_switch__follow_hash_chain(Map, Slot, LastSlot) :-
	map__lookup(Map, Slot, hash_slot(_, NextSlot)),
	(
		NextSlot >= 0,
		map__contains(Map, NextSlot)
	->
		ml_string_switch__follow_hash_chain(Map, NextSlot, LastSlot)
	;
		LastSlot = Slot
	).

	% next_free_hash_slot(M, H_M, LastUsed, FreeSlot) :-
	%	Find the next available slot FreeSlot in the hash table
	%	which is not already used (contained in M) and which is not
	%	going to be used a primary slot (contained in H_M),
	%	starting at the slot after LastUsed.

:- pred ml_string_switch__next_free_hash_slot(map(int, hash_slot),
	map(int, ml_cases_list), int, int).
:- mode ml_string_switch__next_free_hash_slot(in, in, in, out) is det.

ml_string_switch__next_free_hash_slot(Map, H_Map, LastUsed, FreeSlot) :-
	NextSlot is LastUsed + 1,
	(
		\+ map__contains(Map, NextSlot),
		\+ map__contains(H_Map, NextSlot)
	->
		FreeSlot = NextSlot
	;
		ml_string_switch__next_free_hash_slot(Map, H_Map, NextSlot,
			FreeSlot)
	).

:- pred ml_string_switch__gen_hash_slots(int::in, int::in,
		map(int, hash_slot)::in, code_model::in, prog_context::in,
		mlds__label::in, mlds__label::in,
		list(mlds__initializer)::out, list(mlds__label)::out,
		list(mlds__initializer)::out, mlds__statements::out,
		list(mlds__switch_case)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_string_switch__gen_hash_slots(Slot, TableSize, HashSlotMap, CodeModel,
		Context, FailLabel, EndLabel, Strings, Labels, NextSlots,
		MLDS_Statements, MLDS_Cases) -->
	{ MLDS_Context = mlds__make_context(Context) },
	( { Slot = TableSize } ->
		{
			Strings = [],
			Labels = [],
			NextSlots = [],
			MLDS_Statements = [],
			MLDS_Cases = []
		}
	;
		ml_string_switch__gen_hash_slot(Slot, HashSlotMap,
			CodeModel, MLDS_Context, FailLabel, EndLabel,
			String, Label, NextSlot, SlotStatements, SlotCases),
		{ Slot1 is Slot + 1 },
		{ 
			Strings = [String | Strings0],
			Labels = [Label | Labels0],
			NextSlots = [NextSlot | NextSlots0],
			MLDS_Statements = SlotStatements ++ MLDS_Statements0,
			MLDS_Cases = SlotCases ++ MLDS_Cases0
		},
		ml_string_switch__gen_hash_slots(Slot1, TableSize, HashSlotMap,
			CodeModel, Context, FailLabel, EndLabel,
			Strings0, Labels0, NextSlots0, MLDS_Statements0,
			MLDS_Cases0)
	).

:- pred ml_string_switch__gen_hash_slot(int::in, map(int, hash_slot)::in,
		code_model::in, mlds__context::in, mlds__label::in,
		mlds__label::in, mlds__initializer::out,
		mlds__label::out, mlds__initializer::out,
		mlds__statements::out, list(mlds__switch_case)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_string_switch__gen_hash_slot(Slot, HashSlotMap, CodeModel, MLDS_Context,
		FailLabel, EndLabel, init_obj(StringRval), Label,
		init_obj(NextSlotRval), MLDS_Statements, MLDS_Cases) -->
	(
		{ map__search(HashSlotMap, Slot, hash_slot(Case, Next)) }
	->
		{ NextSlotRval = const(int_const(Next)) },
		{ Case = case(_, ConsTag, _, Goal) },
		{ ConsTag = string_constant(String0) ->
			String = String0
		;
			error("ml_string_switch__gen_hash_slots: string expected")
		},
		{ StringRval = const(string_const(String)) },
		ml_gen_new_label(Label),
		{ string__append_list(["case """, String, """"],
			CommentString) },
		{ LabelComment = mlds__statement(
			atomic(comment(CommentString)),
			MLDS_Context) },
		{ LabelStatement = mlds__statement(
			label(Label),
			MLDS_Context) },
		ml_gen_goal(CodeModel, Goal, GoalStatement),
		{ JumpComment = mlds__statement(
			atomic(comment("jump to end of switch")),
			MLDS_Context) },
		{ JumpStatement = mlds__statement(
			goto(EndLabel),
			MLDS_Context) },
		{ MLDS_Statements = [LabelComment, LabelStatement,
			GoalStatement, JumpComment, JumpStatement] },
		{ MLDS_Cases = [[match_value(const(int_const(Slot)))] -
			GoalStatement] }
	;
		{ StringRval = const(null(ml_string_type)) },
		{ Label = FailLabel },
		{ NextSlotRval = const(int_const(-2)) },
		{ MLDS_Statements = [] },
		{ MLDS_Cases = [] }
	).

:- func ml_string_type = mlds__type.
ml_string_type = mercury_type(string_type, str_type).
