%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% string_switch.m

% For switches on strings, we generate a hash table using open addressing
% to resolve hash conflicts.

% Author: fjh.

%-----------------------------------------------------------------------------%

:- module string_switch.

:- interface.

:- import_module hlds_goal, llds, switch_gen, code_info.
:- import_module term.

:- pred string_switch__generate(cases_list, var, code_model,
	can_fail, store_map, label, code_tree, code_info, code_info).
:- mode string_switch__generate(in, in, in, in, in, in, out, in, out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_data, code_gen, tree.
:- import_module bool, int, string, list, map, std_util, assoc_list, require.

string_switch__generate(Cases, Var, CodeModel, _CanFail, StoreMap,
		EndLabel, Code) -->
	code_info__produce_variable(Var, VarCode, VarRval),
	code_info__acquire_reg(r, SlotReg),
	code_info__acquire_reg(r, StringReg),
	code_info__get_next_label(LoopLabel),
	code_info__get_next_label(FailLabel),
	code_info__get_next_label(JumpLabel),
	code_info__get_next_cell_number(NextSlotsTableNo),
	code_info__get_next_cell_number(StringTableNo),
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
		string_switch__hash_cases(Cases, HashMask, HashValsMap),
		map__to_assoc_list(HashValsMap, HashValsList),
		string_switch__calc_hash_slots(HashValsList, HashValsMap,
			HashSlotsMap)
	},
		% Note that it is safe to release the registers now,
		% even though we haven't yet generated all the code
		% which uses them, because that code will be executed
		% before the code for the cases (which might reuse those
		% registers), and because that code is generated manually
		% (below) so we don't need the reg info to be valid when
		% we generated it.
	code_info__release_reg(SlotReg),
	code_info__release_reg(StringReg),

		% Generate the code for when the hash lookup fails.
		% This must be done before gen_hash_slots, since
		% we want to use the exprn_info corresponding to
		% the start of the switch, not to the end of the last case.
	code_info__generate_failure(FailCode),

		% Generate the code etc. for the hash table
		%
	string_switch__gen_hash_slots(0, TableSize, HashSlotsMap, CodeModel,
		StoreMap, FailLabel, EndLabel, Strings, Labels,
		NextSlots, SlotsCode),

		% Generate code which does the hash table lookup
	{
		NextSlotsTable = create(0, NextSlots, no, NextSlotsTableNo),
		StringTable = create(0, Strings, no, StringTableNo),
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
	{
		JumpCode = node([
			label(JumpLabel) -
				"we found a match",
			computed_goto(lval(SlotReg), Labels) -
				"jump to the corresponding code"
		])
	},
		% Collect all the generated code fragments together
	{ Code =
		tree(VarCode,
		tree(HashLookupCode,
		tree(FailCode,
		tree(JumpCode,
		     SlotsCode))))
	}.

:- pred string_switch__hash_cases(cases_list, int, map(int, cases_list)).
:- mode string_switch__hash_cases(in, in, out) is det.

string_switch__hash_cases([], _, Map) :-
	map__init(Map).
string_switch__hash_cases([Case | Cases], HashMask, Map) :-
	string_switch__hash_cases(Cases, HashMask, Map0),
	( Case = case(_, string_constant(String0), _, _) ->
		String = String0
	;
		error("string_switch__hash_cases: non-string case?")
	),
	string__hash(String, HashVal0),
	HashVal is HashVal0 /\ HashMask,
	( map__search(Map0, HashVal, CaseList0) ->
		map__det_update(Map0, HashVal, [Case | CaseList0], Map)
	;
		map__det_insert(Map0, HashVal, [Case], Map)
	).

:- type hash_slot ---> hash_slot(extended_case, int).

:- pred string_switch__calc_hash_slots(assoc_list(int, cases_list),
	map(int, cases_list), map(int, hash_slot)).
:- mode string_switch__calc_hash_slots(in, in, out) is det.

	% string_switch__calc_hash_slots(AssocList, HashMap, Map) :-
	%	For each (HashVal - Case) pair in AssocList,
	%	allocate a hash slot in Map for the case, as follows.
	%	If the hash slot corresponding to HashVal is not
	%	already used, then use that one.  Otherwise, find
	%	the next spare slot (making sure that we don't
	%	use slots which can be used for a direct match with
	%	the hash value for one of the other cases), and
	%	use it instead.  Keep track of the hash chains
	%	as we do this.

string_switch__calc_hash_slots(HashValList, HashMap, Map) :-
	map__init(Map0),
	string_switch__calc_hash_slots_1(HashValList, HashMap, Map0, 0, Map, _).

:- pred string_switch__calc_hash_slots_1(assoc_list(int, cases_list),
	map(int, cases_list), map(int, hash_slot), int,
	map(int, hash_slot), int).
:- mode string_switch__calc_hash_slots_1(in, in, in, in, out, out) is det.

string_switch__calc_hash_slots_1([], _, Map, LastUsed, Map, LastUsed).
string_switch__calc_hash_slots_1([HashVal-Cases | Rest], HashMap, Map0,
		LastUsed0, Map, LastUsed) :-
	string_switch__calc_hash_slots_2(Cases, HashVal, HashMap, Map0,
		LastUsed0, Map1, LastUsed1),
	string_switch__calc_hash_slots_1(Rest, HashMap, Map1,
		LastUsed1, Map, LastUsed).

:- pred string_switch__calc_hash_slots_2(cases_list, int, map(int, cases_list),
	map(int, hash_slot), int, map(int, hash_slot), int).
:- mode string_switch__calc_hash_slots_2(in, in, in, in, in, out, out) is det.

string_switch__calc_hash_slots_2([], _HashVal, _HashMap, Map, LastUsed,
		Map, LastUsed).
string_switch__calc_hash_slots_2([Case | Cases], HashVal, HashMap, Map0,
		LastUsed0, Map, LastUsed) :-
	string_switch__calc_hash_slots_2(Cases, HashVal, HashMap, Map0,
		LastUsed0, Map1, LastUsed1),
	( map__contains(Map1, HashVal) ->
		string_switch__follow_hash_chain(Map1, HashVal, ChainEnd),
		string_switch__next_free_hash_slot(Map1, HashMap, LastUsed1,
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

:- pred string_switch__follow_hash_chain(map(int, hash_slot), int, int).
:- mode string_switch__follow_hash_chain(in, in, out) is det.

string_switch__follow_hash_chain(Map, Slot, LastSlot) :-
	map__lookup(Map, Slot, hash_slot(_, NextSlot)),
	(
		NextSlot >= 0,
		map__contains(Map, NextSlot)
	->
		string_switch__follow_hash_chain(Map, NextSlot, LastSlot)
	;
		LastSlot = Slot
	).

	% next_free_hash_slot(M, H_M, LastUsed, FreeSlot) :-
	%	Find the next available slot FreeSlot in the hash table
	%	which is not already used (contained in M) and which is not
	%	going to be used a primary slot (contained in H_M),
	%	starting at the slot after LastUsed.

:- pred string_switch__next_free_hash_slot(map(int, hash_slot),
	map(int, cases_list), int, int).
:- mode string_switch__next_free_hash_slot(in, in, in, out) is det.

string_switch__next_free_hash_slot(Map, H_Map, LastUsed, FreeSlot) :-
	NextSlot is LastUsed + 1,
	(
		\+ map__contains(Map, NextSlot),
		\+ map__contains(H_Map, NextSlot)
	->
		FreeSlot = NextSlot
	;
		string_switch__next_free_hash_slot(Map, H_Map, NextSlot, FreeSlot)
	).

:- pred string_switch__gen_hash_slots(int, int, map(int, hash_slot),
	code_model, store_map, label, label, list(maybe(rval)), list(label),
	list(maybe(rval)), code_tree, code_info, code_info).
:- mode string_switch__gen_hash_slots(in, in, in, in, in, in, in,
	out, out, out, out, in, out) is det.

string_switch__gen_hash_slots(Slot, TableSize, HashSlotMap, CodeModel,
		StoreMap, FailLabel, EndLabel, Strings, Labels,
		NextSlots, Code) -->
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
		string_switch__gen_hash_slot(Slot, TableSize, HashSlotMap,
				CodeModel, StoreMap, FailLabel, EndLabel,
				String, Label, NextSlot, SlotCode),
		{ Slot1 is Slot + 1 },
		{ 
			Strings = [String | Strings0],
			Labels = [Label | Labels0],
			NextSlots = [NextSlot | NextSlots0],
			Code = tree(SlotCode, Code0)
		},
		string_switch__gen_hash_slots(Slot1, TableSize, HashSlotMap,
				CodeModel, StoreMap, FailLabel, EndLabel,
				Strings0, Labels0, NextSlots0, Code0)
	).

:- pred string_switch__gen_hash_slot(int, int, map(int, hash_slot),
	code_model, store_map, label, label, maybe(rval), label,
	maybe(rval), code_tree, code_info, code_info).
:- mode string_switch__gen_hash_slot(in, in, in, in, in, in, in,
	out, out, out, out, in, out) is det.

string_switch__gen_hash_slot(Slot, TblSize, HashSlotMap, CodeModel, StoreMap,
		FailLabel, EndLabel, yes(StringRval), Label,
		yes(NextSlotRval), Code) -->
	(
		{ map__search(HashSlotMap, Slot, hash_slot(Case, Next)) }
	->
		{ NextSlotRval = const(int_const(Next)) },
		{ Case = case(_, ConsTag, _, Goal) },
		{ ConsTag = string_constant(String0) ->
			String = String0
		;
			error("string_switch__gen_hash_slots: string expected")
		},
		{ StringRval = const(string_const(String)) },
		code_info__get_next_label(Label),
		{ string__append_list(["case """, String, """"], Comment) },
		{ LabelCode = node([
			label(Label) - Comment
		]) },
		(
			{ string_switch__this_is_last_case(Slot, TblSize,
				HashSlotMap) }
		->
			code_gen__generate_goal(CodeModel, Goal, GoalCode),
			code_info__generate_branch_end(CodeModel, StoreMap,
				SaveCode)
		;
			code_info__grab_code_info(CodeInfo),
			code_gen__generate_goal(CodeModel, Goal, GoalCode),
			code_info__generate_branch_end(CodeModel, StoreMap,
				SaveCode),
			code_info__slap_code_info(CodeInfo)
		),
		{ FinishCode = node([
			goto(label(EndLabel)) - "jump to end of switch"
		]) },
		{ Code =
			tree(LabelCode,
			tree(GoalCode,
			tree(SaveCode,
			     FinishCode)))
		}
	;
		{ StringRval = const(int_const(0)) },
		{ Label = FailLabel },
		{ NextSlotRval = const(int_const(-2)) },
		{ Code = empty }
	).

:- pred string_switch__this_is_last_case(int, int, map(int, hash_slot)).
:- mode string_switch__this_is_last_case(in, in, in) is semidet.

string_switch__this_is_last_case(Slot, TableSize, Table) :-
	Slot1 is Slot + 1,
	( Slot1 >= TableSize ->
		true
	;
		\+ map__contains(Table, Slot1),
		string_switch__this_is_last_case(Slot1, TableSize, Table)
	).

