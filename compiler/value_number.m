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

:- import_module vn_type, llds, list, io.

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

:- import_module vn_table, vn_block, vn_order, vn_flush.
:- import_module vn_temploc, vn_cost, vn_debug, vn_util.
:- import_module peephole, livemap, opt_util, opt_debug.
:- import_module map, bintree_set, require, int, string, std_util.

	% We can't find out what variables are used by C code sequences,
	% so we don't optimize any predicates containing them.

value_number__main(Instrs0, Instrs) -->
	{ livemap__build(Instrs0, Ccode, Livemap) },
	vn__livemap_msg(Livemap),
	(
		{ Ccode = no },
		vn__procedure(Instrs0, Livemap, Instrs)
	;
		{ Ccode = yes },
		{ Instrs = Instrs0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize the code of a procedure.

:- pred vn__procedure(list(instruction), livemap, list(instruction),
	io__state, io__state).
:- mode vn__procedure(in, in, out, di, uo) is det.

vn__procedure(Instrs0, Livemap, OptInstrs) -->
	{ opt_util__new_label_no(Instrs0, 1000, LabelNo0) },
	{ opt_util__gather_comments(Instrs0, Comments, Instrs1) },
	{ vn__divide_into_blocks(Instrs1, Blocks) },
	vn__optimize_blocks(Blocks, Livemap, LabelNo0, OptBlocks0,
		[], RevTuples),
	{ list__reverse(RevTuples, Tuples) },
	vn__process_parallel_tuples(Tuples, OptBlocks0, Livemap, OptBlocks1),
	{ list__condense([Comments | OptBlocks1], OptInstrs) }.

:- pred vn__optimize_blocks(list(list(instruction)), livemap, int,
	list(list(instruction)), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
:- mode vn__optimize_blocks(in, in, in, out, di, uo, di, uo) is det.

vn__optimize_blocks([], _, _, [], Tuples, Tuples) --> [].
vn__optimize_blocks([Block0 | Blocks0], Livemap, LabelNo0, [Block | Blocks],
		RevTuples0, RevTuples) -->
	vn__optimize_block(Block0, Livemap, [], LabelNo0, LabelNo1, Block,
		RevTuples0, RevTuples1),
	vn__optimize_blocks(Blocks0, Livemap, LabelNo1, Blocks,
		RevTuples1, RevTuples).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__optimize_block(list(instruction), livemap, list(parentry),
	int, int, list(instruction), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
:- mode vn__optimize_block(in, in, in, in, out, out, in, out, di, uo) is det.

vn__optimize_block(Instrs0, Livemap, ParEntries, LabelNo0, LabelNo, Instrs,
		RevTuples0, RevTuples) -->
	(
		{ list__reverse(Instrs0, RevInstrs) },
		{ RevInstrs = [LastInstr - _ | _] },
		{ opt_util__can_instr_fall_through(LastInstr, yes) }
	->
		% The blocks ends with a call to an erroneous procedure
		% and its never to be used return label
		{ Instrs = Instrs0 },
		{ LabelNo = LabelNo0 },
		{ RevTuples = [no | RevTuples0] }
	;
		vn__optimize_fragment(Instrs0, Livemap, ParEntries, LabelNo0,
			Tuple, Instrs),
		{ Tuple = tuple(_, _, _, LabelNo, _) },
		{ RevTuples = [yes(Tuple) | RevTuples0] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize a tail fragment of a block. This may be the entire block,
	% or it may be a part of the block; we optimize parts of blocks if
	% a conflict prevents us from optimizing the whole block together.

:- pred vn__optimize_fragment(list(instruction), livemap, list(parentry),
	int, vn_ctrl_tuple, list(instruction), io__state, io__state).
:- mode vn__optimize_fragment(in, in, in, in, out, out, di, uo) is det.

vn__optimize_fragment(Instrs0, Livemap, ParEntries, LabelNo0, Tuple, Instrs) -->
	% io__write_string("in vn__optimize_fragment starting at\n"),
	% ( { Instrs0 = [Uinstr0 - _ | _] } ->
	% 	output_instruction(Uinstr0),
	% 	io__write_string("\n")
	% ;
	% 	{ error("empty instruction sequence in vn__optimize_fragment") }
	% ),
	{ vn__build_block_info(Instrs0, Livemap, ParEntries, LabelNo0,
		VnTables0, Liveset, SeenIncr, Tuple0) },
	{ Tuple0 = tuple(Ctrl, Ctrlmap, Flushmap, LabelNo, _Parmap) },

	{ vn__build_uses(Liveset, Ctrlmap, VnTables0, VnTables1) },

	vn__order(Liveset, VnTables1, SeenIncr, Ctrl, Ctrlmap, Flushmap,
		Maybe),
	(
		{ Maybe = yes(VnTables2 - Order) },

		{ vn__max_real_regs(MaxRealRegs) },
		{ vn__max_real_temps(MaxRealTemps) },
		{ vn__init_templocs(MaxRealRegs, MaxRealTemps,
			Liveset, VnTables2, Templocs0) },
		vn__flush_nodelist(Order, Ctrlmap, VnTables2, Templocs0,
			Instrs1),

		{ vn__push_decr_sp_back(Instrs1, Instrs2) },
		{ vn__push_incr_sp_forw(Instrs2, Instrs3) },
		{ vn__push_livevals_back(Instrs3, Instrs4) },
		{ vn__convert_back_modframe(Instrs4, Instrs5) },
		{ peephole__main(Instrs5, Instrs6, _) },

		vn__cost_header_msg("original code sequence"),
		vn__block_cost(Instrs0, yes, OrigCost),
		vn__cost_header_msg("new code sequence"),
		vn__block_cost(Instrs6, yes, VnCost),
		vn__cost_msg(OrigCost, VnCost),

		{ VnCost < OrigCost ->
			Instrs = Instrs6,
			vn__build_block_info(Instrs6, Livemap, ParEntries,
				LabelNo0, _, _, _, Tuple)
		;
			Instrs = Instrs0,
			Tuple = Tuple0
		}
	;
		{ Maybe = no },
		vn__try_again(Instrs0, Livemap, LabelNo, Instrs),
		{ vn__build_block_info(Instrs0, Livemap, ParEntries,	
			LabelNo0, _, _, _, Tuple) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__process_parallel_tuples(list(maybe(vn_ctrl_tuple)),
	list(list(instruction)), livemap, list(list(instruction)),
	io__state, io__state).
:- mode vn__process_parallel_tuples(in, in, in, out, di, uo) is det.

vn__process_parallel_tuples(Tuples0, Blocks0, Livemap, Blocks) -->
	{ list__length(Tuples0, TupleLength) },
	{ list__length(Blocks0, BlockLength) },
	{ TupleLength = BlockLength ->
		true
	;
		error("number of tuples and blocks differ")
	},
	vn__process_parallel_tuples_2(Blocks0, Tuples0, Livemap, Blocks0,
		Blocks1, Extras),
	{ vn__insert_new_blocks(Extras, Blocks1, Blocks) }.

:- pred vn__insert_new_blocks(assoc_list(label, list(instruction)),
	list(list(instruction)), list(list(instruction))).
:- mode vn__insert_new_blocks(di, di, uo) is det.

vn__insert_new_blocks([], Blocks, Blocks).
vn__insert_new_blocks([Label - Extra | Extras], Blocks0, Blocks) :-
	vn__find_block_by_label(Blocks0, Label, Before, LabelBlock, After),
	list__condense([Before, [Extra, LabelBlock], After], Blocks1),
	vn__insert_new_blocks(Extras, Blocks1, Blocks).

:- pred vn__process_parallel_tuples_2(list(list(instruction)),
	list(maybe(vn_ctrl_tuple)), livemap, list(list(instruction)),
	list(list(instruction)), assoc_list(label, list(instruction)), io__state, io__state).
:- mode vn__process_parallel_tuples_2(di, in, in, in, uo, out, di, uo) is det.

vn__process_parallel_tuples_2([], _, _, _, [], []) --> [].
vn__process_parallel_tuples_2([Block0 | Blocks0], MaybeTuples0, Livemap,
		AllBlocks, [Block | Blocks], Extras) -->
	{ MaybeTuples0 = [MaybeTuple0Prime | MaybeTuples1Prime] ->
		MaybeTuple0 = MaybeTuple0Prime,
		MaybeTuples1 = MaybeTuples1Prime
	;
		error("tuples and blocks not in sync")
	},
	(
		{ MaybeTuple0 = yes(Tuple) },
		vn__process_parallel_tuple(Block0, Tuple, Livemap,
			AllBlocks, Block, Extras1)
	;
		{ MaybeTuple0 = no },
		{ Block = Block0 },
		{ Extras1 = [] }
	),
	vn__process_parallel_tuples_2(Blocks0, MaybeTuples1, Livemap,
		AllBlocks, Blocks, Extras2),
	{ list__append(Extras1, Extras2, Extras) }.

:- pred vn__process_parallel_tuple(list(instruction), vn_ctrl_tuple,
	livemap, list(list(instruction)), list(instruction),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode vn__process_parallel_tuple(di, in, in, in, uo, out, di, uo) is det.

vn__process_parallel_tuple(Block0, tuple(_, _, _, _, Parmap), Livemap,
		AllBlocks, Block, Extras) -->
	{ map__values(Parmap, ParList) },
	( { vn__all_empty_lists(ParList) } ->
		{ Block = Block0 },
		{ Extras = [] }
	;
		vn__process_parallel_nodes(ParList, Livemap,
			Block0, AllBlocks, Block, Extras)
	).

:- pred vn__all_empty_lists(list(list(T))).
:- mode vn__all_empty_lists(in) is semidet.

vn__all_empty_lists([]).
vn__all_empty_lists([[] | Lists]) :-
	vn__all_empty_lists(Lists).

:- pred vn__process_parallel_nodes(list(list(parallel)), livemap,
	list(instruction), list(list(instruction)), list(instruction),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode vn__process_parallel_nodes(in, in, di, in, uo, out, di, uo) is det.

vn__process_parallel_nodes([], _, Block, _, Block, []) --> [].
vn__process_parallel_nodes([Par0 | Pars1], Livemap,
		Block0, AllBlocks, Block, Extras) -->
	{ vn__split_at_next_ctrl_instr(Block0, Start, NodeInstr, Block1) },
	vn__process_parallels(Par0, Livemap, NodeInstr,
		NewNodeInstr, AllBlocks, Extras1),
	vn__process_parallel_nodes(Pars1, Livemap,
		Block1, AllBlocks, Block2, Extras2),
	{ list__condense([Start, [NewNodeInstr], Block2], Block) },
	{ list__append(Extras1, Extras2, Extras) }.

:- pred vn__process_parallels(list(parallel), livemap, instruction, instruction,
	list(list(instruction)), assoc_list(label, list(instruction)), io__state, io__state).
:- mode vn__process_parallels(in, in, in, out, in, out, di, uo) is det.

vn__process_parallels(Pars, Livemap, Instr0, Instr, AllBlocks, Extras) -->
	{ Instr0 = Uinstr0 - Comment },
	(
		{ Pars = [] }
	->
		{ Instr = Instr0 },
		{ Extras = []}
	;
		{ Uinstr0 = if_val(Rval, label(Label)) }
	->
		( { Pars = [Par] } ->
			( { Par = parallel(Label, NewLabel, ParEntries) } ->
				vn__process_parallel(Par, Livemap, AllBlocks,
					FinalLabel, Extras),
				{ Instr = if_val(Rval, label(FinalLabel))
					- Comment }
			;
				{ error("wrong label in parallel for if_val") }
			)
		;
			{ error("more than one parallel for if_val") }
		)
	;
		{ Uinstr0 = goto(label(Label), Profile) }
	->
		( { Pars = [Par] } ->
			( { Par = parallel(Label, NewLabel, ParEntries) } ->
				vn__process_parallel(Par, Livemap, AllBlocks,
					FinalLabel, Extras),
				{ Instr = goto(label(FinalLabel), Profile) 
					- Comment }
			;
				{ error("wrong label in parallel for goto") }
			)
		;
			{ error("more than one parallel for goto") }
		)
	;
		{ Uinstr0 = computed_goto(Rval, Labels) }
	->
		vn__process_parallel_list(Pars, Labels, Livemap, AllBlocks,
			FinalLabels, Extras),
		{ Instr = computed_goto(Rval, FinalLabels) 
			- Comment }
	;
		{ Instr = Instr0 },
		{ Extras = [] }
	).

:- pred vn__process_parallel_list(list(parallel), list(label),
	livemap, list(list(instruction)), list(label),
	assoc_list(label, list(instruction)), io__state, io__state).
:- mode vn__process_parallel_list(in, in, in, in, out, out, di, uo) is det.

vn__process_parallel_list([], _, _, _, [], []) --> [].
vn__process_parallel_list([Par | Pars], OldLabels, Livemap, AllBlocks,
		[Label | Labels], Extras) -->
	{ Par = parallel(OldLabel, _, _) },
	{ OldLabels = [OldLabel | OldLabels1Prime] ->
		OldLabels1 = OldLabels1Prime
	;
		error("wrong label sequence in parallel for computed_goto")
	},
	vn__process_parallel(Par, Livemap, AllBlocks, Label, Extras1),
	vn__process_parallel_list(Pars, OldLabels1, Livemap,
		AllBlocks, Labels, Extras2),
	{ list__append(Extras1, Extras2, Extras) }.

:- pred vn__process_parallel(parallel, livemap, list(list(instruction)),
	label, assoc_list(label, list(instruction)), io__state, io__state).
:- mode vn__process_parallel(in, in, in, out, out, di, uo) is det.

vn__process_parallel(Par, Livemap, AllBlocks, FinalLabel, Extras) -->
	vn__parallel_msg(Par),
	{ Par = parallel(OldLabel, NewLabel, ParEntries) },
	{ vn__find_block_by_label(AllBlocks, OldLabel, _, Block, _) },
	vn__optimize_block(Block, Livemap, ParEntries, 2000, _,
		NewBlock0, [], _),
	vn__block_cost(Block, no, OrigCost),
	vn__block_cost(NewBlock0, no, ParCost),
	{
		ParCost < OrigCost
	->
		FinalLabel = NewLabel,
		( NewBlock0 = [label(OldLabel) - Comment | Rest] ->
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

:- pred vn__find_block_by_label(list(list(instruction)), label,
	list(list(instruction)), list(instruction), list(list(instruction))).
:- mode vn__find_block_by_label(di, in, uo, uo, uo) is det.

vn__find_block_by_label([], Label, _, _, _) :-
	opt_debug__dump_label(Label, L_str),
	string__append("Cannot find block with label ", L_str, Str),
	error(Str).
vn__find_block_by_label([Block | Blocks], Label, Before, LabelBlock, After) :-
	( Block = [FirstInstr | _] ->
		( FirstInstr = label(Label) - _ ->
			Before = [],
			LabelBlock = Block,
			After = Blocks
		;
			vn__find_block_by_label(Blocks, Label,
				Before0, LabelBlock, After),
			Before = [Block | Before0]
		)
	;
		error("found empty block")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__convert_back_modframe(list(instruction), list(instruction)).
:- mode vn__convert_back_modframe(in, out) is det.

vn__convert_back_modframe([], []).
vn__convert_back_modframe([Instr0 | Instrs0], [Instr | Instrs]) :-
	vn__convert_back_modframe(Instrs0, Instrs),
	(
		Instr0 = assign(redoip(lval(curfr)),
			const(address_const(Redoip))) - _
	->
		Instr = modframe(Redoip) - "recovered modframe"
	;
		Instr = Instr0
	).

:- pred vn__try_again(list(instruction), livemap, int, list(instruction),
	io__state, io__state).
:- mode vn__try_again(in, in, in, out, di, uo) is det.

vn__try_again([], _, _, []) --> [].
vn__try_again([Instr0 | Instrs0], Livemap, LabelNo0, Instrs) -->
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
		vn__order_restart_msg(Instr0),
		vn__optimize_fragment(Instrs0, Livemap, [], LabelNo0,
			_, Instrs1),
		{ Instrs = [Instr0 | Instrs1] }
	;
		vn__try_again(Instrs0, Livemap, LabelNo0, Instrs1),
		{ Instrs = [Instr0 | Instrs1] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__push_decr_sp_back(list(instruction), list(instruction)).
:- mode vn__push_decr_sp_back(di, uo) is det.

vn__push_decr_sp_back([], []).
vn__push_decr_sp_back([Instr0 | Instrs0], Instrs) :-
	( Instr0 = decr_sp(N) - _ ->
		vn__push_decr_sp_back_2(Instrs0, N, Instrs)
	;
		vn__push_decr_sp_back(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn__push_decr_sp_back_2(list(instruction), int, list(instruction)).
:- mode vn__push_decr_sp_back_2(di, in, uo) is det.

vn__push_decr_sp_back_2([], N, [decr_sp(N) - ""]).
vn__push_decr_sp_back_2([Instr0 | Instrs0], N, Instrs) :-
	Instr0 = Uinstr0 - _,
	vn__boundary_instr(Uinstr0, Boundary),
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
		vn__push_decr_sp_back_2(Instrs0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn__push_incr_sp_forw(list(instruction), list(instruction)).
:- mode vn__push_incr_sp_forw(di, uo) is det.

vn__push_incr_sp_forw(Instrs0, Instrs) :-
	list__reverse(Instrs0, Instrs1),
	vn__push_incr_sp_forw_rev(Instrs1, MaybeFrameSize, Instrs2),
	(
		MaybeFrameSize = yes(N),
		vn__push_save_succip_forw_rev(Instrs2, N, Instrs3)
	;
		MaybeFrameSize = no,
		Instrs3 = Instrs2
	),
	list__reverse(Instrs3, Instrs).

%-----------------------------------------------------------------------------%

:- pred vn__push_incr_sp_forw_rev(list(instruction), maybe(int),
	list(instruction)).
:- mode vn__push_incr_sp_forw_rev(di, out, uo) is det.

vn__push_incr_sp_forw_rev([], no, []).
vn__push_incr_sp_forw_rev([Instr0 | Instrs0], MaybeFrameSize, Instrs) :-
	( Instr0 = incr_sp(N) - _ ->
		vn__push_incr_sp_forw_rev_2(Instrs0, N, Instrs),
		MaybeFrameSize = yes(N)
	;
		vn__push_incr_sp_forw_rev(Instrs0, MaybeFrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn__push_incr_sp_forw_rev_2(list(instruction), int, list(instruction)).
:- mode vn__push_incr_sp_forw_rev_2(di, in, uo) is det.

vn__push_incr_sp_forw_rev_2([], N, [incr_sp(N) - ""]).
vn__push_incr_sp_forw_rev_2([Instr0 | Instrs0], N, Instrs) :-
	Instr0 = Uinstr0 - _,
	vn__boundary_instr(Uinstr0, Boundary),
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
		vn__push_incr_sp_forw_rev_2(Instrs0, N, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred vn__push_save_succip_forw_rev(list(instruction), int,
	list(instruction)).
:- mode vn__push_save_succip_forw_rev(di, in, uo) is det.

vn__push_save_succip_forw_rev([], _, []).
vn__push_save_succip_forw_rev([Instr0 | Instrs0], FrameSize, Instrs) :-
	( Instr0 = assign(stackvar(FrameSize), lval(succip)) - _ ->
		vn__push_save_succip_forw_rev_2(Instrs0, FrameSize, Instrs)
	;
		vn__push_save_succip_forw_rev(Instrs0, FrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn__push_save_succip_forw_rev_2(list(instruction), int,
	list(instruction)).
:- mode vn__push_save_succip_forw_rev_2(di, in, uo) is det.

vn__push_save_succip_forw_rev_2([], _FrameSize, _) :-
	error("succip save without incr_sp").
vn__push_save_succip_forw_rev_2([Instr0 | Instrs0], FrameSize, Instrs) :-
	Instr0 = Uinstr0 - _,
	( Uinstr0 = incr_sp(FrameSize) ->
		Instrs = [assign(stackvar(FrameSize), lval(succip)) - "",
			Instr0 | Instrs0]
	;
		vn__push_save_succip_forw_rev_2(Instrs0, FrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred vn__push_livevals_back(list(instruction), list(instruction)).
:- mode vn__push_livevals_back(di, uo) is det.

vn__push_livevals_back([], []).
vn__push_livevals_back([Instr0 | Instrs0], Instrs) :-
	( Instr0 = livevals(Livevals) - _ ->
		vn__push_livevals_back_2(Instrs0, Livevals, Instrs)
	;
		vn__push_livevals_back(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn__push_livevals_back_2(list(instruction), lvalset, list(instruction)).
:- mode vn__push_livevals_back_2(di, in, uo) is det.

vn__push_livevals_back_2([], Livevals, [livevals(Livevals) - ""]).
vn__push_livevals_back_2([Instr0 | Instrs0], Livevals, Instrs) :-
	Instr0 = Uinstr0 - _,
	vn__boundary_instr(Uinstr0, Boundary),
	opt_util__can_instr_branch_away(Uinstr0, CanBranch),
	( Boundary = yes, CanBranch = yes ->
		Instrs = [livevals(Livevals) - "", Instr0 | Instrs0]
	;
		vn__push_livevals_back_2(Instrs0, Livevals, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred vn__boundary_instr(instr, bool).
:- mode vn__boundary_instr(in, out) is det.

vn__boundary_instr(comment(_), no).
vn__boundary_instr(livevals(_), no).
vn__boundary_instr(block(_, _), no).
vn__boundary_instr(assign(_,_), no).
vn__boundary_instr(call(_, _, _, _), yes).
vn__boundary_instr(call_closure(_, _, _), yes).
vn__boundary_instr(mkframe(_, _, _), yes).
vn__boundary_instr(modframe(_), yes).
vn__boundary_instr(label(_), yes).
vn__boundary_instr(goto(_, _), yes).
vn__boundary_instr(computed_goto(_, _), yes).
vn__boundary_instr(c_code(_), yes).
vn__boundary_instr(if_val(_, _), yes).
vn__boundary_instr(incr_hp(_, _, _), no).
vn__boundary_instr(mark_hp(_), no).
vn__boundary_instr(restore_hp(_), no).
vn__boundary_instr(incr_sp(_), yes).
vn__boundary_instr(decr_sp(_), yes).

%-----------------------------------------------------------------------------%

	% The best values of these two parameters are platform dependent.

:- pred vn__max_real_regs(int).
:- mode vn__max_real_regs(out) is det.

vn__max_real_regs(5).

:- pred vn__max_real_temps(int).
:- mode vn__max_real_temps(out) is det.

vn__max_real_temps(5).

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
