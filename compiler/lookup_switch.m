%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% lookup_switch.m

% For switches on atomic types in which the cases contain only the construction
% of constants, generate code which just assigns the values of the output
% variables by indexing into an array of values for each output variable.

% For switches that can fail, the generated code does a range check on the
% index, and then does a lookup in a bit-vector to see if there is a value
% for the appropriate case. If there is, then it does a lookup (using the
% field macro) in the array of results. The array is padded with "0"s for
% cases that are not covered. This is fine, since we do the lookup after
% we check the bit-vector for the appropriate case.

% The current implementation works out whether or not it can do a lookup
% switch by generating code for each case and looking to see that no code
% got generated (ie only the exprn cache got modified) and that the output
% variables of the switch are all constants. This is potentially quite in-
% efficient because it does the work of generating code for the cases and
% then may throw it away if a subsequent case generates actual code, or non
% constant outputs.

% A potential improvement would be to make a single array for each switch,
% since putting the values produced for each tag value side-by-side in
% memory will tend to lead to fewer cache misses.

% The number of bits per word is taken from the bits_per_word option which
% uses a flag in the mc script with a value from configuration. This is
% used when generating bit-vectors.

% Author: conway.

%-----------------------------------------------------------------------------%

:- module lookup_switch.

:- interface.

:- import_module hlds_goal, hlds_data, llds, switch_gen, code_info.
:- import_module list, term.

:- type case_consts == list(pair(int, list(rval))).

:- type rval_map == map(var, list(pair(int, rval))).

:- pred lookup_switch__is_lookup_switch(var, cases_list, hlds_goal_info,
		can_fail, int, code_model, int, int, can_fail, can_fail,
		list(var), case_consts, maybe(set(var)),
		code_info, code_info).
:- mode lookup_switch__is_lookup_switch(in, in, in, in, in, in, out, out,
		out, out, out, out, out, in, out) is semidet.

	% Generate code for a switch using a lookup table.

:- pred lookup_switch__generate(var, list(var), case_consts,
		int, int, can_fail, can_fail, maybe(set(var)), store_map,
		code_tree, code_info, code_info).
:- mode lookup_switch__generate(in, in, in, in, in, in, in, in, in,
		out, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set, code_gen, type_util, map, tree, int, std_util, require.
:- import_module dense_switch, bool, assoc_list, globals, options, mode_util.
:- import_module exprn_aux, getopt, prog_data, instmap.

	% Most of this predicate is taken from dense_switch.m

	% We need the code_info structure to generate code for the cases to
	% get the constants (if they exist). We can't throw it away at the
	% end because we may have allocated some new static ground term labels.
lookup_switch__is_lookup_switch(CaseVar, TaggedCases, GoalInfo,
		CanFail0, ReqDensity, CodeModel, FirstVal, LastVal,
		NeedRangeCheck, NeedBitVecTest, OutVars,
			CaseValues, MLiveness) -->
		% Since lookup switches rely on static ground terms to
		% work efficiently, there is no point in using a lookup
		% switch if static-ground-terms are not enabled. Well,
		% actually, it is possible that they might be a win in
		% some circumstances, but it would take a pretty complex
		% heuristic to get it right, so, lets just use a simple
		% one - no static ground terms, no lookup switch.
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ getopt__lookup_bool_option(Options, static_ground_terms, yes) },
	{
		% We want to generate a lookup switch for any switch
		% that is dense enough - we don't care how many cases
		% it has. A memory lookup tends to be cheaper than
		% a branch.
		list__length(TaggedCases, NumCases),
		TaggedCases = [FirstCase | _],
		FirstCase = case(_, int_constant(FirstCaseVal), _, _),
		list__index1_det(TaggedCases, NumCases, LastCase),
		LastCase = case(_, int_constant(LastCaseVal), _, _),
		Span is LastCaseVal - FirstCaseVal,
		Range is Span + 1,
		dense_switch__calc_density(NumCases, Range, Density),
		Density > ReqDensity
	},
	% If there are going to be no gaps in the lookup
	% table then we won't need a bitvector test to see
	% if this switch has a value for this case.
	(
		{ NumCases = Range }
	->
		{ NeedBitVecTest = cannot_fail }
	;
		{ NeedBitVecTest = can_fail }
	),
	( { CanFail0 = can_fail } ->
		% For semidet switches, we normally need to check that
		% the variable is in range before we index into the jump table.
		% However, if the range of the type is sufficiently small,
		% we can make the jump table large enough to hold all
		% of the values for the type.
		code_info__variable_type(CaseVar, Type),
		code_info__get_module_info(ModuleInfo),
		{ classify_type(Type, ModuleInfo, TypeCategory) },
		(
			dense_switch__type_range(TypeCategory, Type, TypeRange),
			{ dense_switch__calc_density(NumCases, TypeRange, DetDensity) },
			{ DetDensity > ReqDensity }
		->
			{ NeedRangeCheck = cannot_fail },
			{ FirstVal = 0 },
			{ LastVal is TypeRange - 1 }
		;
			{ NeedRangeCheck = CanFail0 },
			{ FirstVal = FirstCaseVal },
			{ LastVal = LastCaseVal }
		)
	;
		{ NeedRangeCheck = CanFail0 },
		{ FirstVal = FirstCaseVal },
		{ LastVal = LastCaseVal }
	),
	lookup_switch__figure_out_output_vars(GoalInfo, OutVars),
	lookup_switch__generate_constants(TaggedCases, OutVars, CodeModel,
		CaseValues, MLiveness).

%---------------------------------------------------------------------------%

:- pred lookup_switch__figure_out_output_vars(hlds_goal_info, list(var),
		code_info, code_info).
:- mode lookup_switch__figure_out_output_vars(in, out, in, out) is det.

	% Figure out which variables are bound in the switch.
	% We do this by using the current instmap and the instmap delta in
	% the goal info to work out which variables are [further] bound by
	% the switch.

lookup_switch__figure_out_output_vars(GoalInfo, OutVars) -->
	{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
	(
		{ instmap_delta_is_unreachable(InstMapDelta) }
	->
		{ OutVars = [] }
	;
		code_info__get_instmap(CurrentInstMap),
		code_info__get_module_info(ModuleInfo),
		{ instmap_delta_changed_vars(InstMapDelta, ChangedVars) },
		{ instmap__apply_instmap_delta(CurrentInstMap, InstMapDelta,
			InstMapAfter) },
		{ Lambda = lambda([Var::out] is nondet, (
			% If a variable has a final inst, then it changed
			% instantiatedness during the switch.
			set__member(Var, ChangedVars),
			instmap__lookup_var(CurrentInstMap, Var, Initial),
			instmap__lookup_var(InstMapAfter, Var, Final),
			mode_is_output(ModuleInfo, (Initial -> Final))
		)) },
		{ solutions(Lambda, OutVars) }
	).

%---------------------------------------------------------------------------%

:- pred lookup_switch__generate_constants(cases_list, list(var),
		code_model, case_consts, maybe(set(var)),
		code_info, code_info).
:- mode lookup_switch__generate_constants(in, in, in, out, out,
		in, out) is semidet.

	% To figure out if the outputs are constants, we generate code for
	% the cases, and check to see if each of the output vars is a constant,
	% and that no actual code was generated for the goal.
lookup_switch__generate_constants([], _Vars, _CodeModel, [], no) --> [].
lookup_switch__generate_constants([Case|Cases], Vars, CodeModel,
		[CaseVal|Rest], yes(Liveness)) -->
	{ Case = case(_, int_constant(CaseTag), _, Goal) },
	code_info__grab_code_info(CodeInfo),
	code_gen__generate_goal(CodeModel, Goal, Code),
	code_info__get_forward_live_vars(Liveness),
	{ tree__is_empty(Code) },
	lookup_switch__get_case_rvals(Vars, CaseRvals),
	{ CaseVal = CaseTag - CaseRvals },
	code_info__slap_code_info(CodeInfo),
	lookup_switch__generate_constants(Cases, Vars, CodeModel, Rest, _).

%---------------------------------------------------------------------------%

:- pred lookup_switch__get_case_rvals(list(var), list(rval),
		code_info, code_info).
:- mode lookup_switch__get_case_rvals(in, out, in, out) is semidet.

lookup_switch__get_case_rvals([], []) --> [].
lookup_switch__get_case_rvals([Var|Vars], [Rval|Rvals]) -->
	code_info__produce_variable(Var, Code, Rval),
	{ tree__is_empty(Code) },
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ exprn_aux__init_exprn_opts(Options, ExprnOpts) },
	{ lookup_switch__rval_is_constant(Rval, ExprnOpts) },
	lookup_switch__get_case_rvals(Vars, Rvals).

%---------------------------------------------------------------------------%

	% lookup_switch__rval_is_constant(Rval, ExprnOpts) is
	% true iff Rval is a constant. This depends on the options governing
	% nonlocal gotos, asm labels enabled, and static ground terms, etc.
:- pred lookup_switch__rval_is_constant(rval, exprn_opts).
:- mode lookup_switch__rval_is_constant(in, in) is semidet.

	% Based on code_exprn__rval_is_constant, but differs in
	% that it doesn't happen with respect to the expresion cache.

lookup_switch__rval_is_constant(const(Const), ExprnOpts) :-
	exprn_aux__const_is_constant(Const, ExprnOpts, yes).
lookup_switch__rval_is_constant(unop(_, Exprn), ExprnOpts) :-
	lookup_switch__rval_is_constant(Exprn, ExprnOpts).
lookup_switch__rval_is_constant(binop(_, Exprn0, Exprn1), ExprnOpts) :-
	lookup_switch__rval_is_constant(Exprn0, ExprnOpts),
	lookup_switch__rval_is_constant(Exprn1, ExprnOpts).
lookup_switch__rval_is_constant(mkword(_, Exprn0), ExprnOpts) :-
	lookup_switch__rval_is_constant(Exprn0, ExprnOpts).
lookup_switch__rval_is_constant(create(_, Args, _, _), ExprnOpts) :-
	ExprnOpts = nlg_asm_sgt_ubf(_, _, StaticGroundTerms, _),
	StaticGroundTerms = yes,
	lookup_switch__rvals_are_constant(Args, ExprnOpts).

:- pred lookup_switch__rvals_are_constant(list(maybe(rval)), exprn_opts).
:- mode lookup_switch__rvals_are_constant(in, in) is semidet.

lookup_switch__rvals_are_constant([], _).
lookup_switch__rvals_are_constant([MRval|MRvals], ExprnOpts) :-
	MRval = yes(Rval),
	lookup_switch__rval_is_constant(Rval, ExprnOpts),
	lookup_switch__rvals_are_constant(MRvals, ExprnOpts).

%---------------------------------------------------------------------------%

lookup_switch__generate(Var, OutVars, CaseValues,
		StartVal, EndVal, NeedRangeCheck, NeedBitVecCheck,
		MLiveness, StoreMap, Code) -->
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
	(
		{ NeedRangeCheck = can_fail },
		{ Difference is EndVal - StartVal },
		code_info__fail_if_rval_is_false(
			binop(<=, unop(cast_to_unsigned, Index),
				const(int_const(Difference))), RangeCheck)
	;
		{ NeedRangeCheck = cannot_fail },
		{ RangeCheck = empty }
	),
	(
		{ NeedBitVecCheck = can_fail },
		lookup_switch__generate_bitvec_test(Index, CaseValues,
			StartVal, EndVal, CheckBitVec)
	;
		{ NeedBitVecCheck = cannot_fail },
		{ CheckBitVec = empty }
	),
		% Now generate the terms into which we do the lookups
	lookup_switch__generate_terms(Index, OutVars, CaseValues, StartVal),
		% We keep track of what variables are supposed to be
		% live at the end of cases. We have to do this explicitly
		% because generating a `fail' slot last would yield the
		% wrong liveness.
	(
		{ MLiveness = yes(Liveness) },
		code_info__set_forward_live_vars(Liveness)
	;
		{ MLiveness = no },
		{ error("lookup_switch__generate: no liveness!") }
	),
	code_info__generate_branch_end(model_det, StoreMap, LookupCode),
		% Assemble to code together
	{ Comment = node([comment("lookup switch") - ""]) },
	{ Code =
		tree(Comment,
		tree(VarCode,
		tree(RangeCheck,
		tree(CheckBitVec,
		     LookupCode))))
	}.

%------------------------------------------------------------------------------%

:- pred lookup_switch__generate_bitvec_test(rval, case_consts, int, int,
		code_tree, code_info, code_info).
:- mode lookup_switch__generate_bitvec_test(in, in, in, in, out,
		in, out) is det.

	% The bitvector is an array of words (where we use the first
	% 32 bits of each word). Each bit represents a tag value for
	% the (range checked) input to the lookup switch. The bit is `1'
	% iff we have a case for that tag value.
lookup_switch__generate_bitvec_test(Index, CaseVals, Start, _End,
		CheckCode) -->
	lookup_switch__get_word_bits(WordBits),
	generate_bit_vec(CaseVals, Start, WordBits, BitVec),

	{ UIndex = unop(cast_to_unsigned, Index) },
		%
		% Optimize the single-word case:
		% if all the cases fit into a single word, then
		% the word to use is always that word, and the index
		% specifies which bit.  Otherwise, the high bits
		% of the index specify which word to use and the
		% low bits specify which bit.
		%
	{
		BitVec = create(_, [yes(SingleWord)], _, _)
	->
		Word = SingleWord,
		BitNum = UIndex
	;
		WordNum = binop(/, UIndex, const(int_const(WordBits))),
		Word = lval(field(0, BitVec, WordNum)),
		BitNum = binop(mod, UIndex, const(int_const(WordBits)))
	},
	{ HasBit = binop((&),
			binop((<<), const(int_const(1)), BitNum),
			Word) },
	code_info__fail_if_rval_is_false(HasBit, CheckCode).


:- pred lookup_switch__get_word_bits(int, code_info, code_info).
:- mode lookup_switch__get_word_bits(out, in, out) is det.

	% Prevent cross-compilation errors by making sure that
	% the bitvector uses a number of bits that will fit both
	% on this machine (so that we can correctly generate it),
	% and on the target machine (so that it can be executed
	% correctly)

lookup_switch__get_word_bits(WordBits) -->
	{ int__bits_per_int(HostWordBits) },
	code_info__get_globals(Globals),
	{ globals__get_options(Globals, Options) },
	{ getopt__lookup_int_option(Options, bits_per_word, TargetWordBits) },
	{ int__min(HostWordBits, TargetWordBits, WordBits) }.

:- pred generate_bit_vec(case_consts, int, int, rval, code_info, code_info).
:- mode generate_bit_vec(in, in, in, out, in, out) is det.

	% we generate the bitvector by iterating through the cases
	% marking the bit for each case. (We represent the bitvector
	% here as a map from the word number in the vector to the bits
	% for that word.
generate_bit_vec(CaseVals, Start, WordBits, BitVec) -->
	{ map__init(Empty) },
	{ generate_bit_vec_2(CaseVals, Start, WordBits, Empty, BitMap) },
	{ map__to_assoc_list(BitMap, WordVals) },
	{ generate_bit_vec_args(WordVals, 0, Args) },
	code_info__get_next_cell_number(CellNo),
	{ BitVec = create(0, Args, no, CellNo) }.

:- pred generate_bit_vec_2(case_consts, int, int,
			map(int, int), map(int, int)).
:- mode generate_bit_vec_2(in, in, in, in, out) is det.

generate_bit_vec_2([], _, _, Bits, Bits).
generate_bit_vec_2([Tag-_|Rest], Start, WordBits, Bits0, Bits) :-
	Val is Tag - Start,
	Word is Val // WordBits,
	Offset is Val mod WordBits,
	(
		map__search(Bits0, Word, X0)
	->
		X1 is X0 \/ (1 << Offset)
	;
		X1 is (1 << Offset)
	),
	map__set(Bits0, Word, X1, Bits1),
	generate_bit_vec_2(Rest, Start, WordBits, Bits1, Bits).

:- pred generate_bit_vec_args(list(pair(int)), int, list(maybe(rval))).
:- mode generate_bit_vec_args(in, in, out) is det.

generate_bit_vec_args([], _, []).
generate_bit_vec_args([Word - Bits|Rest], Count, [yes(Rval)|Rvals]) :-
	(
		Count < Word
	->
		WordVal = 0,
		Remainder = [Word - Bits|Rest]
	;
		WordVal = Bits,
		Remainder = Rest
	),
	Rval = const(int_const(WordVal)),
	Count1 is Count + 1,
	generate_bit_vec_args(Remainder, Count1, Rvals).

%------------------------------------------------------------------------------%

:- pred lookup_switch__generate_terms(rval, list(var),
		case_consts, int, code_info, code_info).
:- mode lookup_switch__generate_terms(in, in, in, in, in, out) is det.

	% Add an expression to the expression cache in the code_info
	% structure for each of the output variables of the lookup
	% switch. This is done by creating a `create' term for the
	% array, and caching an expression for the variable to get the
	% Index'th field of that therm.

lookup_switch__generate_terms(Index, OutVars, CaseVals, Start) -->
	{ map__init(Empty) },
	{ rearrange_vals(OutVars, CaseVals, Start, Empty, ValMap) },
	lookup_switch__generate_terms_2(Index, OutVars, ValMap).

:- pred lookup_switch__generate_terms_2(rval, list(var),
		rval_map, code_info, code_info).
:- mode lookup_switch__generate_terms_2(in, in, in, in, out) is det.

lookup_switch__generate_terms_2(_Index, [], _Map) --> [].
lookup_switch__generate_terms_2(Index, [Var|Vars], Map) -->
	{ map__lookup(Map, Var, Vals0) },
	{ list__sort(Vals0, Vals) },
	{ construct_args(Vals, 0, Args) },
	code_info__get_next_cell_number(CellNo),
	{ ArrayTerm = create(0, Args, no, CellNo) },
	{ LookupTerm = lval(field(0, ArrayTerm, Index)) },
	code_info__cache_expression(Var, LookupTerm),
	lookup_switch__generate_terms_2(Index, Vars, Map).

:- pred construct_args(list(pair(int, rval)), int, list(maybe(rval))).
:- mode construct_args(in, in, out) is det.

construct_args([], _, []).
construct_args([Index - Rval|Rest], Count0, [yes(Arg)|Args]) :-
	(
		Count0 < Index
	->
		% If this argument (array element) is a place-holder and
		% will never be referenced, just fill it in with a `0'
		Arg = const(int_const(0)),
		Remainder = [Index - Rval|Rest]
	;
		Arg = Rval,
		Remainder = Rest
	),
	Count1 is Count0 + 1,
	construct_args(Remainder, Count1, Args).

%------------------------------------------------------------------------------%

:- pred rearrange_vals(list(var), case_consts, int, rval_map, rval_map).
:- mode rearrange_vals(in, in, in, in, out) is det.

	% For the purpose of constructing the terms, the case_consts
	% structure is a bit inconvenient, so we rearrange the data
	% into a map from var to list of tag-value pairs.
rearrange_vals(_Vars, [], _Start, Map, Map).
rearrange_vals(Vars, [Tag - Rvals|Rest], Start, Map0, Map) :-
	assoc_list__from_corresponding_lists(Vars, Rvals, Pairs),
	Index is Tag - Start,
	rearrange_vals_2(Pairs, Index, Map0, Map1),
	rearrange_vals(Vars, Rest, Start, Map1, Map).

:- pred rearrange_vals_2(list(pair(var, rval)), int, rval_map, rval_map).
:- mode rearrange_vals_2(in, in, in, out) is det.

rearrange_vals_2([], _, Map, Map).
rearrange_vals_2([Var - Rval|Rest], Tag, Map0, Map) :-
	(
		map__search(Map0, Var, Vals0)
	->
		Vals = [Tag - Rval|Vals0]
	;
		Vals = [Tag - Rval]
	),
	map__set(Map0, Var, Vals, Map1),
	rearrange_vals_2(Rest, Tag, Map1, Map).

%------------------------------------------------------------------------------%
