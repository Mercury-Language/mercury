%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
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
:- import_module hlds_data, switch_util.
:- import_module code_model.
:- import_module mlds, ml_code_util.

:- pred ml_string_switch__generate(cases_list::in, prog_var::in,
		code_model::in, can_fail::in, prog_context::in,
		mlds__defns::out, mlds__statements::out,
		ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ml_code_gen, ml_switch_gen, ml_simplify_switch.
:- import_module builtin_ops, type_util.
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
	{ SlotVarName = mlds__var_name(
		string__format("slot_%d", [i(SlotVarSeq)]), no) },
	{ SlotVarType = mlds__native_int_type },
	{ SlotVarDefn = ml_gen_mlds_var_decl(var(SlotVarName), SlotVarType,
		MLDS_Context) },
	ml_gen_var_lval(SlotVarName, SlotVarType, SlotVarLval),

	ml_gen_info_new_cond_var(StringVarSeq),
	{ StringVarName = mlds__var_name(
		string__format("str_%d", [i(StringVarSeq)]), no) },
	{ StringVarType = ml_string_type },
	{ StringVarDefn = ml_gen_mlds_var_decl(var(StringVarName),
		StringVarType, MLDS_Context) },
	ml_gen_var_lval(StringVarName, StringVarType, StringVarLval),

	%
	% Generate new labels
	%
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
		switch_util__string_hash_cases(Cases, HashMask, HashValsMap),
		map__to_assoc_list(HashValsMap, HashValsList),
		switch_util__calc_hash_slots(HashValsList, HashValsMap,
			HashSlotsMap)
	},

		% Generate the code for when the hash lookup fails.
		%
	ml_gen_failure(CodeModel, Context, FailStatements),

		% Generate the code etc. for the hash table
		%
	ml_string_switch__gen_hash_slots(0, TableSize, HashSlotsMap, CodeModel,
		Context, Strings, NextSlots, SlotsCases),

	%
	% Generate the following local constant declarations:
	%	static const int next_slots_table = { <NextSlots> };
	%	static const MR_String string_table = { <Strings> };
	%
	ml_gen_info_new_const(NextSlotsSeq),
	ml_format_static_const_name("next_slots_table", NextSlotsSeq,
		NextSlotsName),
	{ NextSlotsType = mlds__array_type(SlotVarType) },
	{ NextSlotsDefn = ml_gen_static_const_defn(NextSlotsName,
		NextSlotsType, local, init_array(NextSlots), Context) },
	ml_gen_var_lval(NextSlotsName, NextSlotsType, NextSlotsLval),

	ml_gen_info_new_const(StringTableSeq),
	ml_format_static_const_name("string_table", StringTableSeq,
		StringTableName),
	{ StringTableType = mlds__array_type(StringVarType) },
	{ StringTableDefn = ml_gen_static_const_defn(StringTableName,
		StringTableType, local, init_array(Strings), Context) },
	ml_gen_var_lval(StringTableName, StringTableType ,StringTableLval),
	
	%
	% Generate code which does the hash table lookup.
	%

	{ SwitchStmt0 = switch(SlotVarType, lval(SlotVarLval),
		range(0, TableSize - 1),
		SlotsCases, default_is_unreachable) },
	ml_simplify_switch(SwitchStmt0, MLDS_Context, SwitchStatement),
	{
		FoundMatchCond =
			binop(and,
				binop(ne,
					lval(StringVarLval),
					const(null(StringVarType))),
				binop(str_eq,
					lval(StringVarLval),
					VarRval)
			),
		FoundMatchCode = mlds__statement(
			block([], [
				mlds__statement(atomic(comment(
					"we found a match")),
					MLDS_Context),
				mlds__statement(atomic(comment(
				"dispatch to the corresponding code")),
					MLDS_Context),
				SwitchStatement,
				GotoEndStatement
			]),
			MLDS_Context),
		LoopBody = ml_gen_block([], [
			mlds__statement(atomic(comment(
				"lookup the string for this hash slot")),
				MLDS_Context),
			mlds__statement(
				atomic(assign(StringVarLval,
					binop(array_index(elem_type_string),
						lval(StringTableLval),
						lval(SlotVarLval)))),
				MLDS_Context),
			mlds__statement(atomic(comment(
				"did we find a match?")),
				MLDS_Context),
			mlds__statement(
				if_then_else(FoundMatchCond, FoundMatchCode,
					no),
				MLDS_Context),
			mlds__statement(atomic(comment(
			      "no match yet, so get next slot in hash chain")),
				MLDS_Context),
			mlds__statement(
				atomic(assign(SlotVarLval,
					binop(array_index(elem_type_int),
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
	{ MLDS_Statements = HashLookupStatements ++
			[FailComment | FailStatements] ++
			[EndLabelStatement, EndComment] }.

%-----------------------------------------------------------------------------%

:- pred ml_string_switch__gen_hash_slots(int::in, int::in,
		map(int, hash_slot)::in, code_model::in, prog_context::in,
		list(mlds__initializer)::out, list(mlds__initializer)::out,
		list(mlds__switch_case)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_string_switch__gen_hash_slots(Slot, TableSize, HashSlotMap, CodeModel,
		Context, Strings, NextSlots, MLDS_Cases) -->
	( { Slot = TableSize } ->
		{ Strings = [] },
		{ NextSlots = [] },
		{ MLDS_Cases = [] }
	;
		{ MLDS_Context = mlds__make_context(Context) },
		ml_string_switch__gen_hash_slot(Slot, HashSlotMap,
			CodeModel, MLDS_Context, String, NextSlot, SlotCases),
		ml_string_switch__gen_hash_slots(Slot + 1, TableSize,
			HashSlotMap, CodeModel, Context,
			Strings0, NextSlots0, MLDS_Cases0),
		{ Strings = [String | Strings0] },
		{ NextSlots = [NextSlot | NextSlots0] },
		{ MLDS_Cases = SlotCases ++ MLDS_Cases0 }
	).

:- pred ml_string_switch__gen_hash_slot(int::in, map(int, hash_slot)::in,
		code_model::in, mlds__context::in, mlds__initializer::out,
		mlds__initializer::out, list(mlds__switch_case)::out,
		ml_gen_info::in, ml_gen_info::out) is det.

ml_string_switch__gen_hash_slot(Slot, HashSlotMap, CodeModel, MLDS_Context,
		init_obj(StringRval), init_obj(NextSlotRval), MLDS_Cases) -->
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
		ml_gen_goal(CodeModel, Goal, GoalStatement),

		{ string__append_list(["case """, String, """"],
			CommentString) },
		{ Comment = mlds__statement(
			atomic(comment(CommentString)),
			MLDS_Context) },
		{ CaseStatement = mlds__statement(
			block([], [Comment, GoalStatement]),
			MLDS_Context) },
		{ MLDS_Cases = [[match_value(const(int_const(Slot)))] -
			CaseStatement] }
	;
		{ StringRval = const(null(ml_string_type)) },
		{ NextSlotRval = const(int_const(-2)) },
		{ MLDS_Cases = [] }
	).
