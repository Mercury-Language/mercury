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

:- import_module vn_table, vn_livemap, vn_block, vn_order, vn_flush.
:- import_module vn_temploc, vn_cost, vn_debug, vn_util.
:- import_module peephole, opt_util, opt_debug.
:- import_module map, bintree_set, require, int, string, std_util.

	% We can't find out what variables are used by C code sequences,
	% so we don't optimize any predicates containing them.

value_number__main(Instrs0, Instrs) -->
	{ list__reverse(Instrs0, Backinstrs) },
	{ vn__repeat_build_livemap(Backinstrs, Ccode, Livemap) },
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
	vn__optimize_blocks(Blocks, Livemap, LabelNo0, OptBlocks,
		[], RevTuples),
	{ list__reverse(RevTuples, Tuples) },
	{ list__condense([Comments | OptBlocks], OptInstrs) },
	vn__process_parallel_tuples(Tuples).

:- pred vn__optimize_blocks(list(list(instruction)), livemap, int,
	list(list(instruction)), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
:- mode vn__optimize_blocks(in, in, in, out, di, uo, di, uo) is det.

vn__optimize_blocks([], _, _, [], Tuples, Tuples) --> [].
vn__optimize_blocks([Block0 | Blocks0], Livemap, LabelNo0, [Block | Blocks],
		RevTuples0, RevTuples) -->
	vn__optimize_block(Block0, Livemap, LabelNo0, LabelNo1, Block,
		RevTuples0, RevTuples1),
	vn__optimize_blocks(Blocks0, Livemap, LabelNo1, Blocks,
		RevTuples1, RevTuples).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__optimize_block(list(instruction), livemap, int, int,
	list(instruction), list(maybe(vn_ctrl_tuple)),
	list(maybe(vn_ctrl_tuple)), io__state, io__state).
:- mode vn__optimize_block(in, in, in, out, out, in, out, di, uo) is det.

vn__optimize_block(Instrs0, Livemap, LabelNo0, LabelNo, Instrs,
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
		vn__optimize_fragment(Instrs0, Livemap, LabelNo0,
			Tuple, Instrs),
		{ Tuple = tuple(_, _, _, LabelNo, _) },
		{ RevTuples = [yes(Tuple) | RevTuples0] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Optimize a tail fragment of a block. This may be the entire block,
	% or it may be a part of the block; we optimize parts of blocks if
	% a conflict prevents us from optimizing the whole block together.

:- pred vn__optimize_fragment(list(instruction), livemap, int, vn_ctrl_tuple,
	list(instruction), io__state, io__state).
:- mode vn__optimize_fragment(in, in, in, out, out, di, uo) is det.

vn__optimize_fragment(Instrs0, Livemap, LabelNo0, Tuple, Instrs) -->
	{ vn__build_block_info(Instrs0, Livemap, LabelNo0,
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
		vn__block_cost(Instrs0, OrigCost),
		vn__cost_header_msg("new code sequence"),
		vn__block_cost(Instrs6, VnCost),
		vn__cost_msg(OrigCost, VnCost),

		{ VnCost < OrigCost ->
			Instrs = Instrs6,
			Tuple = Tuple0
		;
			Instrs = Instrs0,
			vn__build_block_info(Instrs0, Livemap, LabelNo0,
				_VnTables1, _Liveset1, _SeenIncr1, Tuple)
		}
	;
		{ Maybe = no },
		vn__try_again(Instrs0, Livemap, LabelNo, Instrs),
		{ vn__build_block_info(Instrs0, Livemap, LabelNo0,
			_VnTables2, _Liveset2, _SeenIncr2, Tuple) }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__process_parallel_tuples(list(maybe(vn_ctrl_tuple)),
	io__state, io__state).
:- mode vn__process_parallel_tuples(in, di, uo) is det.

vn__process_parallel_tuples([]) --> [].
vn__process_parallel_tuples([MaybeTuple | MaybeTuples]) -->
	(
		{ MaybeTuple = yes(Tuple) },
		vn__process_parallel_tuple(Tuple)
	;
		{ MaybeTuple = no }
	),
	vn__process_parallel_tuples(MaybeTuples).

:- pred vn__process_parallel_tuple(vn_ctrl_tuple, io__state, io__state).
:- mode vn__process_parallel_tuple(in, di, uo) is det.

vn__process_parallel_tuple(tuple(_, Ctrlmap, _, _, Parmap)) -->
	{ map__values(Ctrlmap, CtrlList) },
	{ map__values(Parmap, ParList) },
	vn__process_parallel_nodes(CtrlList, ParList).

:- pred vn__process_parallel_nodes(list(vn_instr), list(list(parallel)),
	io__state, io__state).
:- mode vn__process_parallel_nodes(in, in, di, uo) is det.

vn__process_parallel_nodes([], _) --> [].
vn__process_parallel_nodes([_VnInstr | VnInstrs], Par) -->
	( { Par = [Parallels | MoreParallels] } ->
		% { opt_debug__dump_vninstr(VnInstr, I_str) },
		% io__write_string("parallels at node "),
		% io__write_string(I_str),
		% io__write_string("\n"),
		vn__process_parallels(Parallels),
		vn__process_parallel_nodes(VnInstrs, MoreParallels)
	;
		{ error("ctrl and par maps not in sync") }
	).

:- pred vn__process_parallels(list(parallel), io__state, io__state).
:- mode vn__process_parallels(in, di, uo) is det.

vn__process_parallels([]) --> [].
vn__process_parallels([Parallel | Parallels]) -->
	vn__process_parallel(Parallel),
	vn__process_parallels(Parallels).

:- pred vn__process_parallel(parallel, io__state, io__state).
:- mode vn__process_parallel(in, di, uo) is det.

vn__process_parallel(parallel(_OldLabel, _NewLabel, ParEntries)) -->
	% { opt_debug__dump_label(OldLabel, O_str) },
	% { opt_debug__dump_label(NewLabel, N_str) },
	% io__write_string("parallel from "),
	% io__write_string(O_str),
	% io__write_string(" to "),
	% io__write_string(N_str),
	% io__write_string("\n"),
	vn__process_parentries(ParEntries).

:- pred vn__process_parentries(list(parentry), io__state, io__state).
:- mode vn__process_parentries(in, di, uo) is det.

vn__process_parentries([]) --> [].
vn__process_parentries([_Lval - _Rvals | ParEntries]) -->
	% { opt_debug__dump_lval(Lval, L_str) },
	% { opt_debug__dump_rvals(Rvals, R_str) },
	% io__write_string(L_str),
	% io__write_string(" -> "),
	% io__write_string(R_str),
	% io__write_string("\n"),
	vn__process_parentries(ParEntries).

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
		vn__optimize_fragment(Instrs0, Livemap, LabelNo0, _, Instrs1),
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
	vn__push_incr_sp_forw_rev(Instrs1, Instrs2),
	list__reverse(Instrs2, Instrs).

%-----------------------------------------------------------------------------%

:- pred vn__push_incr_sp_forw_rev(list(instruction), list(instruction)).
:- mode vn__push_incr_sp_forw_rev(di, uo) is det.

vn__push_incr_sp_forw_rev([], []).
vn__push_incr_sp_forw_rev([Instr0 | Instrs0], Instrs) :-
	( Instr0 = incr_sp(N) - _ ->
		vn__push_incr_sp_forw_2(Instrs0, N, Instrs)
	;
		vn__push_incr_sp_forw_rev(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn__push_incr_sp_forw_2(list(instruction), int, list(instruction)).
:- mode vn__push_incr_sp_forw_2(di, in, uo) is det.

vn__push_incr_sp_forw_2([], N, [incr_sp(N) - ""]).
vn__push_incr_sp_forw_2([Instr0 | Instrs0], N, Instrs) :-
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
		vn__push_incr_sp_forw_2(Instrs0, N, Instrs1),
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
