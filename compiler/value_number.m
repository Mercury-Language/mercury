%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% value_number.m - optimization of straight-line LLDS code.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module value_number.

:- interface.

:- import_module bool, list, io.
:- import_module llds.

	% Find straight-line code sequences and optimize them using
	% value numbering.

:- pred value_number__main(list(instruction), list(instruction),
	io__state, io__state).
:- mode value_number__main(in, out, di, uo) is det.

	% The main value numbering pass introduces references to temporary
	% variables whose values need be preserved only within an extended
	% basic block. The post_main pass looks for references to temporaries
	% and introduces block instructions whenever it sees them. These
	% block instructions go from the first reference to a temporary
	% to the end of its extended basic block.

:- pred value_number__post_main(list(instruction), list(instruction)).
:- mode value_number__post_main(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module set, map, bimap, require, int, string, std_util, assoc_list.

:- import_module vn_type, vn_table, vn_block, vn_order, vn_flush, vn_temploc. 
:- import_module vn_cost, vn_debug, vn_util, vn_verify, opt_debug, labelopt.
:- import_module globals, options, peephole, livemap, code_util, opt_util.

	% We can't find out what variables are used by C code sequences,
	% so we don't optimize any predicates containing them.

value_number__main(Instrs0, Instrs) -->
	{ opt_util__get_prologue(Instrs0, ProcLabel, Comments, Instrs1) },
	{ opt_util__new_label_no(Instrs1, 1000, N0) },
	{ set__init(AllocSet0) },
	{ value_number__prepare_for_vn(Instrs1, ProcLabel,
		no, AllocSet0, AllocSet, N0, N, Instrs2) },
	{ labelopt__build_useset(Instrs2, UseSet) },
	{ livemap__build(Instrs2, Ccode, LiveMap) },
	(
		{ Ccode = no },
		vn_debug__livemap_msg(LiveMap),
		value_number__procedure(Instrs2, LiveMap, UseSet, AllocSet,
			N, Instrs3),
		{ list__append(Comments, Instrs3, Instrs) }
	;
		% Don't perform value numbering if there is a c_code or a 
		% pragma_c in the instructions.
		{ Ccode = yes },
		{ Instrs = Instrs0 }
	).

%-----------------------------------------------------------------------------%

	% Instructions such as if_val(tag(r1) == 1 && field(1, r1, N) = X)
	% pose a problem for value numbering. The field reference will be
	% extracted into a register before the if, and this extraction will
	% cause an unaligned access if done before the tag test. Similar
	% problems can arise even if the code before the && does not contain
	% a tag operator, since this may have been applied earlier.
	%
	% By converting all boolean operations in if_vals into multiple
	% if_vals, we are preventing this from happening. 
	%
	% Value numbering currently combines multiple heap pointer
	% increments into a single heap pointer increment.  If we're
	% using conservative garbage collection, this would create
	% invalid code (unless the collector was compiled with
	% -DALL_INTERIOR_POINTERS, which would be very bad for
	% performance).  Hence, if GC=conservative we must not
	% perform value numbering on a block that contains more
	% than one heap pointer increment.
	%
	% We therefore insert new labels before every occurrence of incr_hp,
	% with the exception of the first one in each basic block. This allows
	% vn_block__divide_into_blocks to break up such blocks into smaller
	% blocks, with each smaller block having at most one incr_hp.

:- pred value_number__prepare_for_vn(list(instruction), proc_label,
	bool, set(label), set(label), int, int, list(instruction)).
:- mode value_number__prepare_for_vn(in, in, in, in, out, in, out, out) is det.

value_number__prepare_for_vn([], _, _, AllocSet, AllocSet, N, N, []).
value_number__prepare_for_vn([Instr0 | Instrs0], ProcLabel,
		SeenAlloc, AllocSet0, AllocSet, N0, N, Instrs) :-
	Instr0 = Uinstr0 - _Comment,
	( Uinstr0 = if_val(Test, TrueAddr) ->
		( Instrs0 = [label(FalseLabelPrime) - _ | _] ->
			FalseLabel = FalseLabelPrime,
			FalseAddr = label(FalseLabel),
			N1 = N0
		;
			FalseLabel = local(ProcLabel, N0),
			FalseAddr = label(FalseLabel),
			N1 is N0 + 1
		),
		value_number__breakup_complex_if(Test, TrueAddr, FalseAddr,
			FalseAddr, ProcLabel, N1, N2, IfInstrs),
		value_number__prepare_for_vn(Instrs0, ProcLabel,
			SeenAlloc, AllocSet0, AllocSet, N2, N, Instrs1),
		( N1 = N0 ->
			list__append(IfInstrs, Instrs1, Instrs)
		;
			LabelInstr = label(FalseLabel) - "vn false label",
			list__append(IfInstrs, [LabelInstr | Instrs1], Instrs)
		)
	; Uinstr0 = label(_) ->
		% If we have seen a label, then the next incr_hp
		% need not have a label placed in front of it.
		value_number__prepare_for_vn(Instrs0, ProcLabel,
			no, AllocSet0, AllocSet, N0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	; Uinstr0 = incr_hp(_, _, _) ->
		( SeenAlloc = yes ->
			N1 is N0 + 1,
			NewLabel = local(ProcLabel, N0),
			set__insert(AllocSet0, NewLabel, AllocSet1),
			value_number__prepare_for_vn(Instrs0, ProcLabel,
				yes, AllocSet1, AllocSet, N1, N, Instrs1),
			LabelInstr = label(NewLabel) - "vn incr divide label",
			Instrs = [LabelInstr, Instr0 | Instrs1]
		;
			value_number__prepare_for_vn(Instrs0, ProcLabel,
				yes, AllocSet0, AllocSet, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		)
	;
		value_number__prepare_for_vn(Instrs0, ProcLabel,
			SeenAlloc, AllocSet0, AllocSet, N0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__breakup_complex_if(rval, code_addr, code_addr, code_addr,
	proc_label, int, int, list(instruction)).
:- mode value_number__breakup_complex_if(in, in, in, in, in, in, out, out)
	is det.

value_number__breakup_complex_if(Test, TrueAddr, FalseAddr, NextAddr,
		ProcLabel, N0, N, Instrs) :-
	( Test = binop(and, Test1, Test2) ->
		NewLabel = local(ProcLabel, N0),
		NewAddr = label(NewLabel),
		N1 is N0 + 1,
		value_number__breakup_complex_if(Test1, NewAddr, FalseAddr,
			NewAddr, ProcLabel, N1, N2, Instrs1),
		value_number__breakup_complex_if(Test2, TrueAddr, FalseAddr,
			NextAddr, ProcLabel, N2, N, Instrs2),
		list__append(Instrs1, [label(NewLabel) - "" | Instrs2], Instrs)
	; Test = binop(or, Test1, Test2) ->
		NewLabel = local(ProcLabel, N0),
		NewAddr = label(NewLabel),
		N1 is N0 + 1,
		value_number__breakup_complex_if(Test1, TrueAddr, NewAddr,
			NewAddr, ProcLabel, N1, N2, Instrs1),
		value_number__breakup_complex_if(Test2, TrueAddr, FalseAddr,
			NextAddr, ProcLabel, N2, N, Instrs2),
		list__append(Instrs1, [label(NewLabel) - "" | Instrs2], Instrs)
	; Test = unop(not, Test1) ->
		value_number__breakup_complex_if(Test1, FalseAddr, TrueAddr,
			NextAddr, ProcLabel, N0, N, Instrs)
	;
		N = N0,
		( NextAddr = FalseAddr ->
			Instrs = [if_val(Test, TrueAddr) - ""]
		; NextAddr = TrueAddr ->
			code_util__neg_rval(Test, NegTest),
			Instrs = [if_val(NegTest, FalseAddr) - ""]
		;
			Instrs = [if_val(Test, TrueAddr) - "",
				goto(FalseAddr) - ""]
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize the code of a procedure.

:- pred value_number__procedure(list(instruction), livemap, set(label),
	set(label), int, list(instruction), io__state, io__state).
:- mode value_number__procedure(in, in, in, in, in, out, di, uo) is det.

value_number__procedure(Instrs0, LiveMap, UseSet, AllocSet, N0, OptInstrs) -->
	{ opt_util__gather_comments(Instrs0, Comments, Instrs1) },
	globals__io_get_gc_method(GC),
	( { GC = conservative } ->
		{ set__union(UseSet, AllocSet, DivideSet) }
	;
		{ DivideSet = UseSet }
	),
	{ vn_block__divide_into_blocks(Instrs1, DivideSet, Blocks) },
	value_number__optimize_blocks(Blocks, LiveMap, N0, OptBlocks0,
		[], RevTuples),
	globals__io_lookup_bool_option(pred_value_number, PredVn),
	( { PredVn = yes } ->
		{ list__reverse(RevTuples, Tuples) },
		value_number__process_parallel_tuples(Tuples, OptBlocks0,
			LiveMap, OptBlocks1),
		{ list__condense([Comments | OptBlocks0], OptInstrs0) },
		{ list__condense([Comments | OptBlocks1], OptInstrs) },
		vn_debug__cost_header_msg("procedure before parallels"),
		vn_cost__block_cost(OptInstrs0, yes, _),
		vn_debug__cost_header_msg("procedure after parallels"),
		vn_cost__block_cost(OptInstrs, yes, _)
	;
		{ OptBlocks1 = OptBlocks0 },
		{ list__condense([Comments | OptBlocks1], OptInstrs) }
	).

:- pred value_number__optimize_blocks(list(list(instruction)), livemap, int,
	list(list(instruction)), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
% :- mode value_number__optimize_blocks(in, in, in, out, di, uo, di, uo) is det.
:- mode value_number__optimize_blocks(in, in, in, out, in, out, di, uo) is det.

value_number__optimize_blocks([], _, _, [], Tuples, Tuples) --> [].
value_number__optimize_blocks([Block0 | Blocks0], LiveMap, LabelNo0,
		[Block | Blocks], RevTuples0, RevTuples) -->
	value_number__optimize_block(Block0, LiveMap, [], LabelNo0, LabelNo1,
		Block, RevTuples0, RevTuples1),
	value_number__optimize_blocks(Blocks0, LiveMap, LabelNo1,
		Blocks, RevTuples1, RevTuples).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__optimize_block(list(instruction), livemap, list(parentry),
	int, int, list(instruction), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
:- mode value_number__optimize_block(in, in, in, in, out, out, in, out, di, uo)
	is det.

value_number__optimize_block(Instrs0, LiveMap, ParEntries, LabelNo0, LabelNo,
		Instrs, RevTuples0, RevTuples) -->
	(
		{ list__reverse(Instrs0, RevInstrs) },
		{ RevInstrs = [LastInstr - _ | _] },
		{ opt_util__can_instr_fall_through(LastInstr, yes) }
	->
		% The block ends with a call to an erroneous procedure
		% and its never to be used return label
		{ Instrs = Instrs0 },
		{ LabelNo = LabelNo0 },
		{ RevTuples = [no | RevTuples0] }
	;
		value_number__optimize_fragment(Instrs0, LiveMap, ParEntries,
			LabelNo0, Tuple, Instrs),
		{ Tuple = tuple(_, _, _, LabelNo, _) },
		{ RevTuples = [yes(Tuple) | RevTuples0] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize a fragment of a block. This may be the entire block,
	% or it may be a part of the block; we optimize parts of blocks if
	% a conflict prevents us from optimizing the whole block together.

:- pred value_number__optimize_fragment(list(instruction), livemap,
	list(parentry), int, vn_ctrl_tuple, list(instruction),
	io__state, io__state).
:- mode value_number__optimize_fragment(in, in, in, in, out, out, di, uo)
	is det.

value_number__optimize_fragment(Instrs0, LiveMap, ParEntries, LabelNo0,
		Tuple, Instrs) -->
	globals__io_get_gc_method(GC),
	(
		{ GC = conservative },
		{ opt_util__count_incr_hp(Instrs0, NumIncrs) },
		{ NumIncrs >= 2 }
	->
		{ error("instruction sequence with several incr_hps in value_number__optimize_fragment") }
		% { Instrs = Instrs0 },
		% { vn_block__build_block_info(Instrs0, LiveMap, ParEntries,	
		% 	LabelNo0, _, _, _, Tuple) }
	;
		value_number__optimize_fragment_2(Instrs0, LiveMap, ParEntries,
			LabelNo0, Tuple, Instrs)
	).

:- pred value_number__optimize_fragment_2(list(instruction), livemap,
	list(parentry), int, vn_ctrl_tuple, list(instruction),
	io__state, io__state).
:- mode value_number__optimize_fragment_2(in, in, in, in, out, out, di, uo)
	is det.

value_number__optimize_fragment_2(Instrs0, LiveMap, ParEntries, LabelNo0,
		Tuple, Instrs) -->
	( { Instrs0 = [Uinstr0Prime - _ | _] } ->
		{ Uinstr0 = Uinstr0Prime },
		vn_debug__fragment_msg(Uinstr0)
	;
		{ error("empty instruction sequence in value_number__optimize_fragment") }
	),
	{ vn_block__build_block_info(Instrs0, LiveMap, ParEntries, LabelNo0,
		VnTables0, Liveset, SeenIncr, Tuple0) },
	{ Tuple0 = tuple(Ctrl, Ctrlmap, Flushmap, LabelNo, _Parmap) },

	{ vn_util__build_uses(Liveset, Ctrlmap, VnTables0, VnTables1) },

	vn_order__order(Liveset, VnTables1, SeenIncr, Ctrl, Ctrlmap,
		Flushmap, Res),
	(
		{ Res = success(VnTables2, Order) },
		{ value_number__max_real_regs(MaxRealRegs) },
		{ value_number__max_real_temps(MaxRealTemps) },
		{ vn_temploc__init_templocs(MaxRealRegs, MaxRealTemps,
			Liveset, VnTables2, Templocs0) },
		vn_flush__nodelist(Order, Ctrlmap, VnTables2, Templocs0,
			Instrs1),

		{ value_number__push_decr_sp_back(Instrs1, Instrs2) },
		{ value_number__push_incr_sp_forw(Instrs2, Instrs3) },
		{ value_number__push_livevals_back(Instrs3, Instrs4) },
		{ value_number__convert_back_modframe(Instrs4, Instrs5) },
		{ bimap__init(TeardownMap) },
		{ peephole__optimize(Instrs5, Instrs6, TeardownMap, no, _) },

		vn_debug__cost_header_msg("original code sequence"),
		vn_cost__block_cost(Instrs0, yes, OrigCost),
		vn_debug__cost_header_msg("new code sequence"),
		vn_cost__block_cost(Instrs6, yes, VnCost),
		vn_debug__cost_msg(OrigCost, VnCost),

		( { VnCost < OrigCost } ->
			{ vn_block__build_block_info(Instrs6, LiveMap,
				ParEntries, LabelNo0, VnTables6, Liveset6,
				SeenIncr6, Tuple6) },
			{ vn_verify__equivalence(Liveset, Liveset6,
				VnTables0, VnTables6, Problem) },
			( { SeenIncr \= SeenIncr6 } ->
				vn_debug__failure_msg(Uinstr0,
					"disagreement on SeenIncr"),
				{ Instrs = Instrs0 },
				{ Tuple = Tuple0 }
			; { Problem = yes(Msg) } ->
				vn_debug__failure_msg(Uinstr0, Msg),
				{ Instrs = Instrs0 },
				{ Tuple = Tuple0 }
			; { vn_verify__tags(Instrs6) } ->
				{ Instrs = Instrs6 },
				{ Tuple = Tuple6 }
			;
				vn_debug__failure_msg(Uinstr0,
					"failure of tag check"),
				{ Instrs = Instrs0 },
				{ Tuple = Tuple0 }
			)
		;
			{ Instrs = Instrs0 },
			{ Tuple = Tuple0 }
		)
	;
		{ Res = failure(MaybeLabel) },
		(
			{ MaybeLabel = yes(RestartLabel) },
			value_number__try_again(Instrs0, [], RestartLabel,
				LiveMap, LabelNo, Instrs)
		;
			{ MaybeLabel = no },
			value_number__last_ditch(Instrs0,
				LiveMap, LabelNo, Instrs)
		),
		{ vn_block__build_block_info(Instrs0, LiveMap, ParEntries,	
			LabelNo0, _, _, _, Tuple) }
	).

%-----------------------------------------------------------------------------%

:- pred value_number__try_again(list(instruction), list(instruction), label,
	livemap, int, list(instruction), io__state, io__state).
:- mode value_number__try_again(in, in, in, in, in, out, di, uo) is det.

value_number__try_again([], RevInstrs, _Label, LiveMap, LabelNo0, Instrs) -->
	{ list__reverse(RevInstrs, Instrs0) },
	value_number__last_ditch(Instrs0, LiveMap, LabelNo0, Instrs).
value_number__try_again([Instr0 | Instrs0], RevInstrs0, RestartLabel, LiveMap,
		LabelNo0, Instrs) -->
	( { Instr0 = label(RestartLabel) - _ } ->
		( { RevInstrs0 = [] } ->
			value_number__last_ditch(Instrs0, LiveMap, LabelNo0,
				Instrs1),
			{ Instrs = [Instr0 | Instrs1] }
		;
			vn_debug__divide_msg(Instr0),
			{ GotoInstr = goto(label(RestartLabel)) - "" },
			{ list__reverse([GotoInstr | RevInstrs0],
				FrontInstrs0) },
			value_number__optimize_fragment(FrontInstrs0, LiveMap,
				[], LabelNo0, _, FrontInstrs),
			value_number__optimize_fragment(Instrs0, LiveMap, [],
				LabelNo0, _, BackInstrs),
			{ list__append(FrontInstrs, [Instr0 | BackInstrs],
				Instrs1) },
			{ bimap__init(TeardownMap) },
			% to get rid of the introduced goto
			{ peephole__optimize(Instrs1, Instrs, TeardownMap,
				no, _) }
		)
	;
		{ RevInstrs1 = [Instr0 | RevInstrs0] },
		value_number__try_again(Instrs0, RevInstrs1, RestartLabel,
			LiveMap, LabelNo0, Instrs)
	).

%-----------------------------------------------------------------------------%

:- pred value_number__last_ditch(list(instruction), livemap, int,
	list(instruction), io__state, io__state).
:- mode value_number__last_ditch(in, in, in, out, di, uo) is det.

value_number__last_ditch([], _, _, []) --> [].
value_number__last_ditch([Instr0 | Instrs0], LiveMap, LabelNo0, Instrs) -->
	(
		{ Instr0 = Uinstr0 - _ },
		{
			Uinstr0 = if_val(_, _)
		;
			Uinstr0 = restore_hp(_)
		;
			Uinstr0 = mark_hp(_)
		}
	->
		vn_debug__restart_msg(Instr0),
		value_number__optimize_fragment(Instrs0, LiveMap, [], LabelNo0,
			_, Instrs1),
		{ Instrs = [Instr0 | Instrs1] }
	;
		value_number__last_ditch(Instrs0, LiveMap, LabelNo0, Instrs1),
		{ Instrs = [Instr0 | Instrs1] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__process_parallel_tuples(list(maybe(vn_ctrl_tuple)),
	list(list(instruction)), livemap, list(list(instruction)),
	io__state, io__state).
:- mode value_number__process_parallel_tuples(in, in, in, out, di, uo) is det.

value_number__process_parallel_tuples(Tuples0, Blocks0, LiveMap, Blocks) -->
	{ list__length(Tuples0, TupleLength) },
	{ list__length(Blocks0, BlockLength) },
	{ TupleLength = BlockLength ->
		true
	;
		error("number of tuples and blocks differ")
	},
	value_number__process_parallel_tuples_2(Blocks0, Tuples0,
		LiveMap, Blocks0, Blocks1, Extras),
	{ value_number__insert_new_blocks(Extras, Blocks1, Blocks) }.

:- pred value_number__insert_new_blocks(assoc_list(label, list(instruction)),
	list(list(instruction)), list(list(instruction))).
:- mode value_number__insert_new_blocks(in, in, out) is det.

value_number__insert_new_blocks([], Blocks, Blocks).
value_number__insert_new_blocks([Label - Extra | Extras], Blocks0, Blocks) :-
	value_number__find_block_by_label(Blocks0, Label, Before, LabelBlock,
		After),
	list__condense([Before, [Extra, LabelBlock], After], Blocks1),
	value_number__insert_new_blocks(Extras, Blocks1, Blocks).

:- pred value_number__process_parallel_tuples_2(list(list(instruction)),
	list(maybe(vn_ctrl_tuple)), livemap,
	list(list(instruction)), list(list(instruction)),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallel_tuples_2(in, in, in, in, out, out,
	di, uo) is det.

value_number__process_parallel_tuples_2([], _, _, _, [], []) --> [].
value_number__process_parallel_tuples_2([Block0 | Blocks0], MaybeTuples0,
		LiveMap, AllBlocks, [Block | Blocks], Extras) -->
	{ MaybeTuples0 = [MaybeTuple0Prime | MaybeTuples1Prime] ->
		MaybeTuple0 = MaybeTuple0Prime,
		MaybeTuples1 = MaybeTuples1Prime
	;
		error("tuples and blocks not in sync")
	},
	(
		{ MaybeTuple0 = yes(Tuple) },
		value_number__process_parallel_tuple(Block0, Tuple,
			LiveMap, AllBlocks, Block, Extras1)
	;
		{ MaybeTuple0 = no },
		{ Block = Block0 },
		{ Extras1 = [] }
	),
	value_number__process_parallel_tuples_2(Blocks0, MaybeTuples1,
		LiveMap, AllBlocks, Blocks, Extras2),
	{ list__append(Extras1, Extras2, Extras) }.

:- pred value_number__process_parallel_tuple(list(instruction), vn_ctrl_tuple,
	livemap, list(list(instruction)), list(instruction),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallel_tuple(in, in, in, in, out, out, di, uo)
	is det.

value_number__process_parallel_tuple(Block0, tuple(_, _, _, _, Parmap),
		LiveMap, AllBlocks, Block, Extras) -->
	{ map__values(Parmap, ParList) },
	( { value_number__all_empty_lists(ParList) } ->
		{ Block = Block0 },
		{ Extras = [] }
	;
		value_number__process_parallel_nodes(ParList, LiveMap,
			Block0, AllBlocks, Block, Extras)
	).

:- pred value_number__all_empty_lists(list(list(T))).
:- mode value_number__all_empty_lists(in) is semidet.

value_number__all_empty_lists([]).
value_number__all_empty_lists([[] | Lists]) :-
	value_number__all_empty_lists(Lists).

:- pred value_number__process_parallel_nodes(list(list(parallel)), livemap,
	list(instruction), list(list(instruction)), list(instruction),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallel_nodes(in, in, in, in, out, out,
	di, uo) is det.

value_number__process_parallel_nodes([], _, Block, _, Block, []) --> [].
value_number__process_parallel_nodes([Par0 | Pars1], LiveMap,
		Block0, AllBlocks, Block, Extras) -->
	{ vn_block__split_at_next_ctrl_instr(Block0, Start, NodeInstr,
		Block1) },
	value_number__process_parallels(Par0, LiveMap,
		NodeInstr, NewNodeInstr, AllBlocks, Extras1),
	value_number__process_parallel_nodes(Pars1, LiveMap,
		Block1, AllBlocks, Block2, Extras2),
	{ list__condense([Start, [NewNodeInstr], Block2], Block) },
	{ list__append(Extras1, Extras2, Extras) }.

:- pred value_number__process_parallels(list(parallel), livemap,
	instruction, instruction, list(list(instruction)),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallels(in, in, in, out, in, out, di, uo)
	is det.

value_number__process_parallels(Pars, LiveMap, Instr0, Instr,
		AllBlocks, Extras) -->
	{ Instr0 = Uinstr0 - Comment },
	( { Pars = [] } ->
		{ Instr = Instr0 },
		{ Extras = []}
	; { Uinstr0 = if_val(Rval, label(Label)) } ->
		( { Pars = [Par] } ->
			( { Par = parallel(Label, _NewLabel, _ParEntries) } ->
				value_number__process_parallel(Par, LiveMap,
					AllBlocks, FinalLabel, Extras),
				{ Instr = if_val(Rval, label(FinalLabel))
					- Comment }
			;
				{ error("wrong label in parallel for if_val") }
			)
		;
			{ error("more than one parallel for if_val") }
		)
	; { Uinstr0 = goto(label(Label)) } ->
		( { Pars = [Par] } ->
			( { Par = parallel(Label, _NewLabel, _ParEntries) } ->
				value_number__process_parallel(Par, LiveMap,
					AllBlocks, FinalLabel, Extras),
				{ Instr = goto(label(FinalLabel)) - Comment }
			;
				{ error("wrong label in parallel for goto") }
			)
		;
			{ error("more than one parallel for goto") }
		)
	; { Uinstr0 = computed_goto(Rval, Labels) } ->
		value_number__process_parallel_list(Pars, Labels, LiveMap,
			AllBlocks, FinalLabels, Extras),
		{ Instr = computed_goto(Rval, FinalLabels) - Comment }
	;
		{ Instr = Instr0 },
		{ Extras = [] }
	).

:- pred value_number__process_parallel_list(list(parallel), list(label),
	livemap, list(list(instruction)), list(label),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode value_number__process_parallel_list(in, in, in, in, out, out, di, uo)
	is det.

value_number__process_parallel_list([], _, _, _, [], []) --> [].
value_number__process_parallel_list([Par | Pars], OldLabels, LiveMap, AllBlocks,
		[Label | Labels], Extras) -->
	{ Par = parallel(OldLabel, _, _) },
	{ OldLabels = [OldLabel | OldLabels1Prime] ->
		OldLabels1 = OldLabels1Prime
	;
		error("wrong label sequence in parallel for computed_goto")
	},
	value_number__process_parallel(Par, LiveMap, AllBlocks, Label, Extras1),
	value_number__process_parallel_list(Pars, OldLabels1, LiveMap,
		AllBlocks, Labels, Extras2),
	{ list__append(Extras1, Extras2, Extras) }.

:- pred value_number__process_parallel(parallel, livemap,
	list(list(instruction)), label, assoc_list(label, list(instruction)),
	io__state, io__state).
:- mode value_number__process_parallel(in, in, in, out, out, di, uo) is det.

value_number__process_parallel(Par, LiveMap, AllBlocks, FinalLabel, Extras) -->
	vn_debug__parallel_msg(Par),
	{ Par = parallel(OldLabel, NewLabel, ParEntries) },
	{ value_number__find_block_by_label(AllBlocks, OldLabel, _, Block, _) },
	value_number__optimize_block(Block, LiveMap, ParEntries, 2000, _,
		NewBlock0, [], _),
	vn_cost__block_cost(Block, no, OrigCost),
	vn_cost__block_cost(NewBlock0, no, ParCost),
	{
		ParCost < OrigCost
	->
		FinalLabel = NewLabel,
		( NewBlock0 = [label(OldLabel) - Comment | Rest0] ->
			opt_util__filter_out_labels(Rest0, Rest),
			NewBlock = [label(NewLabel) - Comment | Rest],
			Extras = [OldLabel - NewBlock]
		;
			error("block starts with wrong label")
		)
	;
		FinalLabel = OldLabel,
		Extras = []
	}.

	% Given a list of blocks and a label, return the blocks before the
	% labelled block, the labelled block itself, and the following blocks.

:- pred value_number__find_block_by_label(list(list(instruction)), label,
	list(list(instruction)), list(instruction), list(list(instruction))).
% :- mode value_number__find_block_by_label(di, in, uo, uo, uo) is det.
:- mode value_number__find_block_by_label(in, in, out, out, out) is det.

value_number__find_block_by_label([], Label, _, _, _) :-
	opt_debug__dump_label(Label, L_str),
	string__append("Cannot find block with label ", L_str, Str),
	error(Str).
value_number__find_block_by_label([Block | Blocks], Label, Before, LabelBlock,
		After) :-
	( Block = [FirstInstr | _] ->
		( FirstInstr = label(Label) - _ ->
			Before = [],
			LabelBlock = Block,
			After = Blocks
		;
			value_number__find_block_by_label(Blocks, Label,
				Before0, LabelBlock, After),
			Before = [Block | Before0]
		)
	;
		error("found empty block")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__convert_back_modframe(list(instruction),
	list(instruction)).
:- mode value_number__convert_back_modframe(in, out) is det.

value_number__convert_back_modframe([], []).
value_number__convert_back_modframe([Instr0 | Instrs0], [Instr | Instrs]) :-
	value_number__convert_back_modframe(Instrs0, Instrs),
	(
		Instr0 = assign(redoip(lval(curfr)),
			const(address_const(Redoip))) - _
	->
		Instr = modframe(Redoip) - "recovered modframe"
	;
		Instr = Instr0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred value_number__push_decr_sp_back(list(instruction), list(instruction)).
% :- mode value_number__push_decr_sp_back(di, uo) is det.
:- mode value_number__push_decr_sp_back(in, out) is det.

value_number__push_decr_sp_back([], []).
value_number__push_decr_sp_back([Instr0 | Instrs0], Instrs) :-
	( Instr0 = decr_sp(N) - _ ->
		value_number__push_decr_sp_back_2(Instrs0, N, Instrs)
	;
		value_number__push_decr_sp_back(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_decr_sp_back_2(list(instruction), int,
	list(instruction)).
% :- mode value_number__push_decr_sp_back_2(di, in, uo) is det.
:- mode value_number__push_decr_sp_back_2(in, in, out) is det.

value_number__push_decr_sp_back_2([], N, [decr_sp(N) - ""]).
value_number__push_decr_sp_back_2([Instr0 | Instrs0], N, Instrs) :-
	Instr0 = Uinstr0 - _,
	value_number__boundary_instr(Uinstr0, Boundary),
	(
		Boundary = yes,
		Instrs = [decr_sp(N) - "", Instr0 | Instrs0],
		opt_util__block_refers_stackvars([Instr0 | Instrs], Ref),
		(
			Ref = yes,
			error("cannot push decr_sp back enough")
		;
			Ref = no
		)
	;
		Boundary = no,
		value_number__push_decr_sp_back_2(Instrs0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_incr_sp_forw(list(instruction), list(instruction)).
% :- mode value_number__push_incr_sp_forw(di, uo) is det.
:- mode value_number__push_incr_sp_forw(in, out) is det.

value_number__push_incr_sp_forw(Instrs0, Instrs) :-
	list__reverse(Instrs0, Instrs1),
	value_number__push_incr_sp_forw_rev(Instrs1, MaybeFrameSize, Instrs2),
	(
		MaybeFrameSize = yes(N),
		value_number__push_save_succip_forw_rev(Instrs2, N, Instrs3)
	;
		MaybeFrameSize = no,
		Instrs3 = Instrs2
	),
	list__reverse(Instrs3, Instrs).

%-----------------------------------------------------------------------------%

:- pred value_number__push_incr_sp_forw_rev(list(instruction), maybe(int),
	list(instruction)).
% :- mode value_number__push_incr_sp_forw_rev(di, out, uo) is det.
:- mode value_number__push_incr_sp_forw_rev(in, out, out) is det.

value_number__push_incr_sp_forw_rev([], no, []).
value_number__push_incr_sp_forw_rev([Instr0 | Instrs0], MaybeFrameSize,
		Instrs) :-
	( Instr0 = incr_sp(N) - _ ->
		value_number__push_incr_sp_forw_rev_2(Instrs0, N, Instrs),
		MaybeFrameSize = yes(N)
	;
		value_number__push_incr_sp_forw_rev(Instrs0, MaybeFrameSize,
			Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_incr_sp_forw_rev_2(list(instruction), int,
	list(instruction)).
% :- mode value_number__push_incr_sp_forw_rev_2(di, in, uo) is det.
:- mode value_number__push_incr_sp_forw_rev_2(in, in, out) is det.

value_number__push_incr_sp_forw_rev_2([], N, [incr_sp(N) - ""]).
value_number__push_incr_sp_forw_rev_2([Instr0 | Instrs0], N, Instrs) :-
	Instr0 = Uinstr0 - _,
	value_number__boundary_instr(Uinstr0, Boundary),
	(
		Boundary = yes,
		Instrs = [incr_sp(N) - "", Instr0 | Instrs0],
		opt_util__block_refers_stackvars([Instr0 | Instrs], Ref),
		(
			Ref = yes,
			error("cannot push incr_sp forward enough")
		;
			Ref = no
		)
	;
		Boundary = no,
		value_number__push_incr_sp_forw_rev_2(Instrs0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred value_number__push_save_succip_forw_rev(list(instruction), int,
	list(instruction)).
% :- mode value_number__push_save_succip_forw_rev(di, in, uo) is det.
:- mode value_number__push_save_succip_forw_rev(in, in, out) is det.

value_number__push_save_succip_forw_rev([], _, []).
value_number__push_save_succip_forw_rev([Instr0 | Instrs0], FrameSize,
		Instrs) :-
	( Instr0 = assign(stackvar(FrameSize), lval(succip)) - _ ->
		value_number__push_save_succip_forw_rev_2(Instrs0, FrameSize,
			Instrs)
	;
		value_number__push_save_succip_forw_rev(Instrs0, FrameSize,
			Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_save_succip_forw_rev_2(list(instruction), int,
	list(instruction)).
% :- mode value_number__push_save_succip_forw_rev_2(di, in, uo) is det.
:- mode value_number__push_save_succip_forw_rev_2(in, in, out) is det.

value_number__push_save_succip_forw_rev_2([], _FrameSize, _) :-
	error("succip save without incr_sp").
value_number__push_save_succip_forw_rev_2([Instr0 | Instrs0], FrameSize,
		Instrs) :-
	Instr0 = Uinstr0 - _,
	( Uinstr0 = incr_sp(FrameSize) ->
		Instrs = [assign(stackvar(FrameSize), lval(succip)) - "",
			Instr0 | Instrs0]
	;
		value_number__push_save_succip_forw_rev_2(Instrs0, FrameSize,
			Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred value_number__push_livevals_back(list(instruction), list(instruction)).
% :- mode value_number__push_livevals_back(di, uo) is det.
:- mode value_number__push_livevals_back(in, out) is det.

value_number__push_livevals_back([], []).
value_number__push_livevals_back([Instr0 | Instrs0], Instrs) :-
	( Instr0 = livevals(Livevals) - _ ->
		value_number__push_livevals_back_2(Instrs0, Livevals, Instrs)
	;
		value_number__push_livevals_back(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred value_number__push_livevals_back_2(list(instruction), lvalset,
	list(instruction)).
% :- mode value_number__push_livevals_back_2(di, in, uo) is det.
:- mode value_number__push_livevals_back_2(in, in, out) is det.

value_number__push_livevals_back_2([], Livevals, [livevals(Livevals) - ""]).
value_number__push_livevals_back_2([Instr0 | Instrs0], Livevals, Instrs) :-
	Instr0 = Uinstr0 - _,
	value_number__boundary_instr(Uinstr0, Boundary),
	opt_util__can_instr_branch_away(Uinstr0, CanBranch),
	( Boundary = yes, CanBranch = yes ->
		Instrs = [livevals(Livevals) - "", Instr0 | Instrs0]
	;
		value_number__push_livevals_back_2(Instrs0, Livevals, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred value_number__boundary_instr(instr, bool).
:- mode value_number__boundary_instr(in, out) is det.

value_number__boundary_instr(comment(_), no).
value_number__boundary_instr(livevals(_), no).
value_number__boundary_instr(block(_, _), no).
value_number__boundary_instr(assign(_,_), no).
value_number__boundary_instr(call(_, _, _, _), yes).
value_number__boundary_instr(call_closure(_, _, _), yes).
value_number__boundary_instr(mkframe(_, _, _), yes).
value_number__boundary_instr(modframe(_), yes).
value_number__boundary_instr(label(_), yes).
value_number__boundary_instr(goto(_), yes).
value_number__boundary_instr(computed_goto(_, _), yes).
value_number__boundary_instr(c_code(_), yes).
value_number__boundary_instr(if_val(_, _), yes).
value_number__boundary_instr(incr_hp(_, _, _), no).
value_number__boundary_instr(mark_hp(_), no).
value_number__boundary_instr(restore_hp(_), no).
value_number__boundary_instr(incr_sp(_), yes).
value_number__boundary_instr(decr_sp(_), yes).
value_number__boundary_instr(pragma_c(_, _, _, _), yes).

%-----------------------------------------------------------------------------%

	% The best values of these two parameters are platform dependent.

:- pred value_number__max_real_regs(int).
:- mode value_number__max_real_regs(out) is det.

value_number__max_real_regs(5).

:- pred value_number__max_real_temps(int).
:- mode value_number__max_real_temps(out) is det.

value_number__max_real_temps(5).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

value_number__post_main(Instrs0, Instrs) :-
	value_number__post_main_2(Instrs0, 0, [], Instrs).

	% N is the number of the highest numbered temp variable seen so far;
	% N = 0 means we haven't seen any temp variables. RevSofar is a
	% reversed list of instructions starting with the first instruction
	% in this block that accesses a temp variable. Invariant: RevSofar
	% is always empty if N = 0.

:- pred value_number__post_main_2(list(instruction), int, list(instruction),
	list(instruction)).
:- mode value_number__post_main_2(in, in, in, out) is det.

value_number__post_main_2([], N, RevSofar, []) :-
	( RevSofar = [_|_] ->
		error("procedure ends with fallthrough")
	; N > 0 ->
		error("procedure ends without closing block")
	;
		true
	).
value_number__post_main_2([Instr0 | Instrs0], N0, RevSofar, Instrs) :-
	Instr0 = Uinstr0 - _Comment0,
	opt_util__count_temps_instr(Uinstr0, N0, N1),
	(
		N1 > 0
	->
		(
			opt_util__can_instr_fall_through(Uinstr0, no)
		->
			list__reverse([Instr0 | RevSofar], BlockInstrs),
			value_number__post_main_2(Instrs0, 0, [], Instrs1),
			Instrs = [block(N1, BlockInstrs) - "" | Instrs1]
		;
			Uinstr0 = label(_)
		->
			list__reverse(RevSofar, BlockInstrs),
			value_number__post_main_2(Instrs0, 0, [], Instrs1),
			Instrs = [block(N1, BlockInstrs) - "", Instr0 | Instrs1]
		;
			value_number__post_main_2(Instrs0,
				N1, [Instr0 | RevSofar], Instrs)
		)
	;
		value_number__post_main_2(Instrs0, 0, [], Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
