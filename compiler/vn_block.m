%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_build.m - Divide a procedure into extended basic blocks, building up
%	a description of each basic block and handing them over to be flushed.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_block.

:- interface.

:- import_module list, set, bool.
:- import_module llds, vn_type, livemap.

:- pred vn_block__divide_into_blocks(list(instruction), set(label),
	list(list(instruction))).
:- mode vn_block__divide_into_blocks(in, in, out) is det.

:- pred vn_block__build_block_info(list(instruction), livemap, list(parentry),
	int, vn_tables, vnlvalset, bool, vn_ctrl_tuple).
:- mode vn_block__build_block_info(in, in, in, in, out, out, out, out) is det.

:- pred vn_block__split_at_next_ctrl_instr(list(instruction), 
	list(instruction), instruction, list(instruction)).
:- mode vn_block__split_at_next_ctrl_instr(in, out, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module vn_table, vn_util, vn_cost, opt_util, opt_debug.
:- import_module map, int, string, require, std_util, assoc_list.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

vn_block__divide_into_blocks(Instrs, Divideset, Blocks) :-
	vn_block__divide_into_blocks_2(Instrs, [], [], Divideset, Blocks).

:- pred vn_block__divide_into_blocks_2(list(instruction), list(instruction),
	list(list(instruction)), set(label), list(list(instruction))).
:- mode vn_block__divide_into_blocks_2(in, in, in, in, out) is det.

vn_block__divide_into_blocks_2([], BlockInstrs, PrevBlocks, _, Blocks) :-
	( BlockInstrs = [] ->
		list__reverse(PrevBlocks, Blocks)
	;
		% This happens only if a procedure ends with fallthrough
		% instruction. This can happen only if the procedure ends
		% with a call to an erroneous procedure and the return label
		% of that call, which will never be used.
		list__reverse(BlockInstrs, ThisBlock),
		list__reverse([ThisBlock | PrevBlocks], Blocks)
	).
vn_block__divide_into_blocks_2([Instr0 | Instrs0], BlockInstrs0, PrevBlocks0,
		Divideset, Blocks) :-
	Instr0 = Uinstr0 - _Comment0,
	(
		opt_util__can_instr_fall_through(Uinstr0, no)
	->
		BlockInstrs = [],
		list__reverse([Instr0 | BlockInstrs0], ThisBlock),
		PrevBlocks = [ThisBlock | PrevBlocks0]
	;
		Uinstr0 = label(Label),
		set__member(Label, Divideset)
	->
		BlockInstrs = [Instr0],
		( BlockInstrs0 = [] ->
			PrevBlocks = PrevBlocks0
		;
			BlockInstrs1 = [goto(label(Label)) - "" | BlockInstrs0],
			list__reverse(BlockInstrs1, ThisBlock),
			PrevBlocks = [ThisBlock | PrevBlocks0]
		)
	;
		BlockInstrs = [Instr0 | BlockInstrs0],
		PrevBlocks = PrevBlocks0
	),
	vn_block__divide_into_blocks_2(Instrs0, BlockInstrs, PrevBlocks,
		Divideset, Blocks).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

vn_block__build_block_info(Instrs, Livemap, ParEntries, LabelNo0,
		VnTables, Liveset, SeenIncr, Tuple) :-
	vn_table__init_tables(VnTables0),
	vn_block__build_from_parallel(ParEntries, VnTables0, VnTables1),
	set__init(Liveset0),
	map__init(Ctrlmap0),
	map__init(Flushmap0),
	map__init(Parmap0),
	Tuple0 = tuple(0, Ctrlmap0, Flushmap0, LabelNo0, Parmap0),
	vn_block__handle_instrs(Instrs, Livemap, VnTables1, VnTables,
		Liveset0, Liveset, no, SeenIncr, Tuple0, Tuple).

:- pred vn_block__build_from_parallel(list(parentry), vn_tables, vn_tables).
% :- mode vn_block__build_from_parallel(in, di, uo) is det.
:- mode vn_block__build_from_parallel(in, in, out) is det.

vn_block__build_from_parallel([], VnTables, VnTables).
vn_block__build_from_parallel([Lval - Rvals | ParEntries], VnTables0, VnTables) :-
	vn_block__real_fake_parentries(Rvals, RealRvals, FakeRvals),
	vn_block__build_from_real_rval(Lval, RealRvals, VnTables0, VnTables1),
	vn_block__build_from_fake_rval(Lval, FakeRvals, VnTables1, VnTables2),
	vn_block__build_from_parallel(ParEntries, VnTables2, VnTables).

:- pred vn_block__real_fake_parentries(list(rval), list(rval), list(lval)).
% :- mode vn_block__real_fake_parentries(di, uo, uo) is det.
:- mode vn_block__real_fake_parentries(in, out, out) is det.

vn_block__real_fake_parentries([], [], []).
vn_block__real_fake_parentries([Rval | Rvals], RealRvals, FakeRvals) :-
	vn_block__real_fake_parentries(Rvals, RealRvals0, FakeRvals0),
	( Rval = lval(Lval) ->
		RealRvals = RealRvals0,
		FakeRvals = [Lval | FakeRvals0]
	;
		RealRvals = [Rval | RealRvals0],
		FakeRvals = FakeRvals0
	).

:- pred vn_block__build_from_real_rval(lval, list(rval), vn_tables, vn_tables).
% :- mode vn_block__build_from_real_rval(in, in, di, uo) is det.
:- mode vn_block__build_from_real_rval(in, in, in, out) is det.

vn_block__build_from_real_rval(_Lval, [], VnTables, VnTables).
vn_block__build_from_real_rval(Lval, [Rval | _], VnTables0, VnTables) :-
	vn_util__rval_to_vn(Rval, Vn, VnTables0, VnTables1),
	vn_util__lval_to_vnlval(Lval, Vnlval, VnTables1, VnTables2),
	vn_table__set_desired_value(Vnlval, Vn, VnTables2, VnTables3),
	vn_table__set_current_value(Vnlval, Vn, VnTables3, VnTables).

:- pred vn_block__build_from_fake_rval(lval, list(lval), vn_tables, vn_tables).
% :- mode vn_block__build_from_fake_rval(in, in, di, uo) is det.
:- mode vn_block__build_from_fake_rval(in, in, in, out) is det.

vn_block__build_from_fake_rval(_Lval, [], VnTables, VnTables).
vn_block__build_from_fake_rval(Lval, [Cheap | Cheaps], VnTables0, VnTables) :-
	(
		vn_util__no_access_lval_to_vnlval(Cheap, yes(VnCheap))
	->
		vn_util__lval_to_vn(Lval, OldVn, VnTables0, VnTables1),
		vn_table__set_parallel_value(VnCheap, OldVn, VnTables1, VnTables2)
	;
		VnTables2 = VnTables0
	),
	vn_block__build_from_fake_rval(Lval, Cheaps, VnTables2, VnTables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_block__handle_instrs(list(instruction), livemap,
	vn_tables, vn_tables, vnlvalset, vnlvalset,
	bool, bool, vn_ctrl_tuple,  vn_ctrl_tuple).
% :- mode vn_block__handle_instrs(in, in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode vn_block__handle_instrs(in, in, in, out, in, out, in, out, in, out)
	is det.

vn_block__handle_instrs([], _Livemap, VnTables, VnTables,
		Liveset, Liveset, SeenIncr, SeenIncr, Tuple, Tuple).
vn_block__handle_instrs([Uinstr0 - _ | Instrs], Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr0, SeenIncr, Tuple0, Tuple) :-
	vn_block__handle_instr(Uinstr0, Livemap, VnTables0, VnTables1,
		Liveset0, Liveset1, SeenIncr0, SeenIncr1, Tuple0, Tuple1),
	vn_block__handle_instrs(Instrs, Livemap, VnTables1, VnTables,
		Liveset1, Liveset, SeenIncr1, SeenIncr, Tuple1, Tuple).

	% If you change vn_block__handle_instr, you may also have to change
	% vn_block__is_ctrl_instr.

:- pred vn_block__handle_instr(instr, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, bool, bool, vn_ctrl_tuple, vn_ctrl_tuple).
% :- mode vn_block__handle_instr(in, in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode vn_block__handle_instr(in, in, in, out, in, out, in, out, in, out)
	is det.

vn_block__handle_instr(comment(_),
		_Livemap, VnTables, VnTables,
		Liveset, Liveset, SeenIncr, SeenIncr, Tuple, Tuple).
vn_block__handle_instr(livevals(Livevals),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_livevals(Livevals),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(block(_, _),
		_Livemap, VnTables, VnTables,
		Liveset, Liveset, SeenIncr, SeenIncr, Tuple, Tuple) :-
	error("block should not be found in handle_instr").
vn_block__handle_instr(assign(Lval, Rval),
		_Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple, Tuple) :-
	vn_util__rval_to_vn(Rval, Vn, VnTables0, VnTables1),
	vn_util__lval_to_vnlval(Lval, Vnlval, VnTables1, VnTables2),
	vn_table__set_desired_value(Vnlval, Vn, VnTables2, VnTables),
	vn_util__find_specials(Vnlval, Specials),
	set__insert_list(Liveset0, Specials, Liveset).
vn_block__handle_instr(call(Proc, Return, Info, CallModel),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_call(Proc, Return, Info, CallModel),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(call_closure(Proc, Return, Info),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_call_closure(Proc, Return, Info),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(mkframe(Name, Size, Redoip),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr0, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_mkframe(Name, Size, Redoip),
		Livemap, VnTables0, VnTables1, Liveset0, Liveset1,
			Tuple0, Tuple1),
	vn_block__handle_instr(assign(redoip(lval(maxfr)),
		const(address_const(Redoip))),
		Livemap, VnTables1, VnTables,
		Liveset1, Liveset, SeenIncr0, SeenIncr, Tuple1, Tuple).
vn_block__handle_instr(modframe(Redoip),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr0, SeenIncr, Tuple0, Tuple) :-
	vn_block__handle_instr(assign(redoip(lval(curfr)),
		const(address_const(Redoip))),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr0, SeenIncr, Tuple0, Tuple).
vn_block__handle_instr(label(Label),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_label(Label),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(goto(Target),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_goto(Target),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(computed_goto(Rval, Labels),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_util__rval_to_vn(Rval, Vn, VnTables0, VnTables1),
	vn_block__new_ctrl_node(vn_computed_goto(Vn, Labels),
		Livemap, VnTables1, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(c_code(_),
		_Livemap, VnTables, VnTables,
		Liveset, Liveset, SeenIncr, SeenIncr, Tuple, Tuple) :-
	error("c_code should not be found in handle_instr").
vn_block__handle_instr(if_val(Rval, Target),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_util__rval_to_vn(Rval, Vn, VnTables0, VnTables1),
	vn_block__new_ctrl_node(vn_if_val(Vn, Target),
		Livemap, VnTables1, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(incr_hp(Lval, MaybeTag, Rval),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, _SeenIncr0, SeenIncr, Tuple0, Tuple) :-
	(
		MaybeTag = no,
		StoreInstr = assign(Lval, lval(hp))
	;
		MaybeTag = yes(Tag),
		StoreInstr = assign(Lval, mkword(Tag, lval(hp)))
	),
	IncrInstr = assign(hp, binop((+), lval(hp),
		binop((*), Rval, const(int_const(4))))),
	vn_block__handle_instr(StoreInstr, Livemap, VnTables0, VnTables1,
		Liveset0, Liveset1, yes, SeenIncr1, Tuple0, Tuple1),
	vn_block__handle_instr(IncrInstr, Livemap, VnTables1, VnTables,
		Liveset1, Liveset, SeenIncr1, SeenIncr, Tuple1, Tuple).
vn_block__handle_instr(mark_hp(Lval),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_util__lval_to_vnlval(Lval, Vnlval, VnTables0, VnTables1),
	vn_block__new_ctrl_node(vn_mark_hp(Vnlval),
		Livemap, VnTables1, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(restore_hp(Rval),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_util__rval_to_vn(Rval, Vn, VnTables0, VnTables1),
	vn_block__new_ctrl_node(vn_restore_hp(Vn),
		Livemap, VnTables1, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(incr_sp(N),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_incr_sp(N),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(decr_sp(N),
		Livemap, VnTables0, VnTables,
		Liveset0, Liveset, SeenIncr, SeenIncr, Tuple0, Tuple) :-
	vn_block__new_ctrl_node(vn_decr_sp(N),
		Livemap, VnTables0, VnTables, Liveset0, Liveset, Tuple0, Tuple).
vn_block__handle_instr(pragma_c(_, _, _, _),
		_Livemap, VnTables, VnTables,
		Liveset, Liveset, SeenIncr, SeenIncr, Tuple, Tuple) :-
	error("value numbering not supported for pragma_c").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_block__new_ctrl_node(vn_instr, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, vn_ctrl_tuple, vn_ctrl_tuple).
% :- mode vn_block__new_ctrl_node(in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_block__new_ctrl_node(in, in, in, out, in, out, in, out) is det.

vn_block__new_ctrl_node(Vn_instr, Livemap, VnTables0, VnTables,
		Liveset0, Liveset,
		tuple(Ctrl0, Ctrlmap0, Flushmap0, LabelNo0, Parmap0),
		tuple(Ctrl,  Ctrlmap,  Flushmap,  LabelNo,  Parmap)) :-
	map__init(FlushEntry0),
	(
		Vn_instr = vn_livevals(Livevals),
		set__to_sorted_list(Livevals, Livelist),
		vn_util__convert_to_vnlval_and_insert(Livelist,
			Liveset0, Liveset),
		VnTables = VnTables0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_call(_, _, _, _),
		vn_block__record_at_call(VnTables0, VnTables, Liveset0, Liveset,
			FlushEntry0, FlushEntry),
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_call_closure(_, _, _),
		vn_block__record_at_call(VnTables0, VnTables, Liveset0, Liveset,
			FlushEntry0, FlushEntry),
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_mkframe(_, _, _),
		VnTables = VnTables0,
		Liveset = Liveset0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_label(_),
		VnTables = VnTables0,
		Liveset = Liveset0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_goto(TargetAddr),
		(
			TargetAddr = label(Label),
			map__search(Livemap, Label, _)
		->
			vn_block__record_one_label(Label, Livemap,
				VnTables0, VnTables, Liveset0, Liveset,
				FlushEntry0, FlushEntry,
				LabelNo0, LabelNo, Parallels)
		;
			vn_block__record_at_call(VnTables0, VnTables,
				Liveset0, Liveset, FlushEntry0, FlushEntry),
			LabelNo = LabelNo0,
			Parallels = []
		)
	;
		Vn_instr = vn_computed_goto(_, Labels),
		vn_block__record_several_labels(Labels, Livemap,
			VnTables0, VnTables, Liveset0, Liveset,
			FlushEntry0, FlushEntry, LabelNo0, LabelNo, Parallels)
	;
		Vn_instr = vn_if_val(_, TargetAddr),
		vn_block__new_if_node(TargetAddr, Livemap, Ctrlmap0, Ctrl0,
			VnTables0, VnTables, Liveset0, Liveset,
			FlushEntry0, FlushEntry, LabelNo0, LabelNo, Parallels)
	;
		Vn_instr = vn_mark_hp(Vnlval),
		vn_util__rval_to_vn(lval(hp), Vn, VnTables0, VnTables1),
		vn_table__set_desired_value(Vnlval, Vn, VnTables1, VnTables),
		set__insert(Liveset0, Vnlval, Liveset),
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_restore_hp(_),
		VnTables = VnTables0,
		Liveset = Liveset0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_incr_sp(_),
		VnTables = VnTables0,
		Liveset = Liveset0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		Vn_instr = vn_decr_sp(_),
		VnTables = VnTables0,
		Liveset = Liveset0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	),
	Ctrl is Ctrl0 + 1,
	map__det_insert(Ctrlmap0, Ctrl0, Vn_instr, Ctrlmap),
	map__det_insert(Flushmap0, Ctrl0, FlushEntry, Flushmap),
	map__det_insert(Parmap0, Ctrl0, Parallels, Parmap).

:- pred vn_block__new_if_node(code_addr, livemap, ctrlmap, int,
	vn_tables, vn_tables, vnlvalset, vnlvalset,
	flushmapentry, flushmapentry, int, int, list(parallel)).
:- mode vn_block__new_if_node(in, in, in, in, in, out, in, out, in, out, in,
	out, out) is det.

vn_block__new_if_node(TargetAddr, Livemap, Ctrlmap0, Ctrl0, VnTables0, VnTables,
		Liveset0, Liveset, FlushEntry0, FlushEntry,
		LabelNo0, LabelNo, Parallels) :-
	(
		TargetAddr = label(Label),
		map__search(Livemap, Label, _)
	->
		vn_block__record_one_label(Label, Livemap,
			VnTables0, VnTables1, Liveset0, Liveset1,
			FlushEntry0, FlushEntry1,
			LabelNo0, LabelNoPrime, ParallelsPrime),
		vn_block__record_compulsory_lvals(VnTables1, Liveset1, Liveset2,
			FlushEntry1, FlushEntry2),
		(
			Prev is Ctrl0 - 1,
			map__search(Ctrlmap0, Prev, PrevInstr),
			PrevInstr = vn_livevals(_)
		->
			% middle-rec branch back
			set__to_sorted_list(Liveset0,
				Vnlivelist),
			vn_block__record_livevnlvals(Vnlivelist,
				VnTables1, VnTables,
				Liveset2, Liveset,
				FlushEntry2, FlushEntry),
			LabelNo = LabelNo0,
			Parallels = []
		;
			VnTables = VnTables1,
			Liveset = Liveset2,
			FlushEntry = FlushEntry2,
			LabelNo = LabelNoPrime,
			Parallels = ParallelsPrime
		)
	;
		TargetAddr = do_fail
	->
		VnTables = VnTables0,
		Liveset = Liveset0,
		FlushEntry = FlushEntry0,
		LabelNo = LabelNo0,
		Parallels = []
	;
		TargetAddr = do_redo
	->
		set__to_sorted_list(Liveset0, Vnlivelist0),
		vn_block__filter_redo_livelist(Vnlivelist0, Vnlivelist),
		vn_block__record_livevnlvals(Vnlivelist, VnTables0, VnTables,
			Liveset0, Liveset, FlushEntry0, FlushEntry),
		LabelNo = LabelNo0,
		Parallels = []
	;
		set__to_sorted_list(Liveset0, Vnlivelist),
		vn_block__record_livevnlvals(Vnlivelist, VnTables0, VnTables,
			Liveset0, Liveset, FlushEntry0, FlushEntry),
		LabelNo = LabelNo0,
		Parallels = []
	).

:- pred vn_block__filter_redo_livelist(list(vnlval), list(vnlval)).
% :- mode vn_block__filter_redo_livelist(di, uo) is det.
:- mode vn_block__filter_redo_livelist(in, out) is det.

vn_block__filter_redo_livelist([], []).
vn_block__filter_redo_livelist([Live0 | Lives0], Lives) :-
	vn_block__filter_redo_livelist(Lives0, Lives1),
	( Live0 = vn_hp ->
		Lives = Lives1
	;
		Lives = [Live0 | Lives1]
	).

%-----------------------------------------------------------------------------%

	% Compute the flushmap entry for a call or for a possible branch to
	% a label or a set of labels.

:- pred vn_block__record_at_call(vn_tables, vn_tables, vnlvalset, vnlvalset,
	flushmapentry, flushmapentry).
% :- mode vn_block__record_at_call(di, uo, di, uo, di, uo) is det.
:- mode vn_block__record_at_call(in, out, in, out, in, out) is det.

vn_block__record_at_call(VnTables0, VnTables, Livevals0, Livevals,
		FlushEntry0, FlushEntry) :-
	set__to_sorted_list(Livevals0, Livelist),
	vn_block__record_livevnlvals(Livelist, VnTables0, VnTables,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1),
	vn_block__record_compulsory_lvals(VnTables, Livevals1, Livevals,
		FlushEntry1, FlushEntry).

:- pred vn_block__record_several_labels(list(label), livemap,
	vn_tables, vn_tables, vnlvalset, vnlvalset,
	flushmapentry, flushmapentry, int, int, list(parallel)).
% :- mode vn_block__record_several_labels(in, in, di, uo, di, uo, di, uo, in,
%	out, out) is det.
:- mode vn_block__record_several_labels(in, in, in, out, in, out, in, out, in,
	out, out) is det.

vn_block__record_several_labels(Labels, Livemap, VnTables0, VnTables,
		Livevals0, Livevals, FlushEntry0, FlushEntry,
		LabelNo0, LabelNo, Parallels) :-
	vn_block__record_labels(Labels, Livemap, VnTables0, VnTables,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1,
		LabelNo0, LabelNo, Parallels),
	vn_block__record_compulsory_lvals(VnTables, Livevals1, Livevals,
		FlushEntry1, FlushEntry).

:- pred vn_block__record_one_label(label, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry,
	int, int, list(parallel)).
:- mode vn_block__record_one_label(in, in, in, out, in, out, in, out, in,
	out, out) is det.

vn_block__record_one_label(Label, Livemap, VnTables0, VnTables,
		Livevals0, Livevals, FlushEntry0, FlushEntry,
		LabelNo0, LabelNo, Parallels) :-
	vn_block__record_label(Label, Livemap, VnTables0, VnTables,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1,
		LabelNo0, LabelNo, Parallels),
	vn_block__record_compulsory_lvals(VnTables, Livevals1, Livevals,
		FlushEntry1, FlushEntry).

%-----------------------------------------------------------------------------%

:- pred vn_block__record_labels(list(label), livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry,
	int, int, list(parallel)).
% :- mode vn_block__record_labels(in, in, di, uo, di, uo, di, uo, in, out, out)
%	is det.
:- mode vn_block__record_labels(in, in, in, out, in, out, in, out, in, out, out)
	is det.

vn_block__record_labels([], _, VnTables, VnTables, Livevals, Livevals,
	FlushEntry, FlushEntry, LabelNo, LabelNo, []).
vn_block__record_labels([Label | Labels], Livemap, VnTables0, VnTables,
		Livevals0, Livevals, FlushEntry0, FlushEntry,
		LabelNo0, LabelNo, Parallels) :-
	vn_block__record_label(Label, Livemap, VnTables0, VnTables1,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1,
		LabelNo0, LabelNo1, Parallels0),
	vn_block__record_labels(Labels, Livemap, VnTables1, VnTables,
		Livevals1, Livevals, FlushEntry1, FlushEntry,
		LabelNo1, LabelNo, Parallels1),
	list__append(Parallels0, Parallels1, Parallels).

:- pred vn_block__record_label(label, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry,
	int, int, list(parallel)).
% :- mode vn_block__record_label(in, in, di, uo, di, uo, di, uo, in, out, out)
%	is det.
:- mode vn_block__record_label(in, in, in, out, in, out, in, out, in, out, out)
	is det.

vn_block__record_label(Label, Livemap, VnTables0, VnTables, Livevals0, Livevals,
		FlushEntry0, FlushEntry, LabelNo0, LabelNo, Parallels) :-
	( map__search(Livemap, Label, Liveset) ->
		set__to_sorted_list(Liveset, Livelist),
		vn_block__record_livevals(Livelist, VnTables0, VnTables,
			Livevals0, Livevals, FlushEntry0, FlushEntry,
			ParEntries),
		( ParEntries = [] ->
			LabelNo = LabelNo0,
			Parallels = []
		;
			( Label = local(ProcLabel, _) ->
				LabelNo is LabelNo0 + 1,
				NewLabel = local(ProcLabel, LabelNo),
				Parallels = [parallel(Label, NewLabel,
					ParEntries)]
			;
				LabelNo = LabelNo0,
				Parallels = []
			)
		)
	;
		opt_debug__dump_label(Label, L_str),
		string__append_list(["cannot find label ",
			L_str, " in Livemap"], Str),
		error(Str)
	).

:- pred vn_block__record_livevals(list(lval), vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry, list(parentry)).
% :- mode vn_block__record_livevals(in, di, uo, di, uo, di, uo, out) is det.
:- mode vn_block__record_livevals(in, in, out, in, out, in, out, out) is det.

vn_block__record_livevals([], VnTables, VnTables,
		Livevals, Livevals, FlushEntry, FlushEntry, []).
vn_block__record_livevals([Lval | Livelist], VnTables0, VnTables,
		Livevals0, Livevals, FlushEntry0, FlushEntry, ParEntries) :-
	vn_util__no_access_lval_to_vnlval(Lval, MaybeVnlval),
	(
		MaybeVnlval = yes(Vnlval),
		( vn_table__search_desired_value(Vnlval, VnPrime, VnTables0) ->
			Vn = VnPrime,
			VnTables1 = VnTables0,
			vn_block__find_cheaper_copies(Lval, Vn, VnTables1,
				ParEntries0)
		;
			vn_table__record_first_vnlval(Vnlval, Vn,
				VnTables0, VnTables1),
			ParEntries0 = []
		),
		map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
		set__insert(Livevals0, Vnlval, Livevals1)
	;
		MaybeVnlval = no,
		VnTables1 = VnTables0,
		Livevals1 = Livevals0,
		FlushEntry1 = FlushEntry0,
		ParEntries0 = []
	),
	vn_block__record_livevals(Livelist, VnTables1, VnTables,
		Livevals1, Livevals, FlushEntry1, FlushEntry, ParEntries1),
	list__append(ParEntries0, ParEntries1, ParEntries).

:- pred vn_block__record_livevnlvals(list(vnlval), vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
% :- mode vn_block__record_livevnlvals(in, di, uo, di, uo, di, uo) is det.
:- mode vn_block__record_livevnlvals(in, in, out, in, out, in, out) is det.

vn_block__record_livevnlvals([], VnTables, VnTables,
		Livevals, Livevals, FlushEntry, FlushEntry).
vn_block__record_livevnlvals([Vnlval | Livevnlvallist], VnTables0, VnTables,
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	( vn_table__search_desired_value(Vnlval, VnPrime, VnTables0) ->
		Vn = VnPrime,
		VnTables1 = VnTables0
	;
		vn_table__record_first_vnlval(Vnlval, Vn,
			VnTables0, VnTables1)
	),
	map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
	set__insert(Livevals0, Vnlval, Livevals1),
	vn_block__record_livevnlvals(Livevnlvallist, VnTables1, VnTables,
		Livevals1, Livevals, FlushEntry1, FlushEntry).

%-----------------------------------------------------------------------------%

	% Insert the heap references that have been made so far into the
	% live vnlval set, and record their currently desired value numbers
	% into the given flushmap entry.

:- pred vn_block__record_compulsory_lvals(vn_tables, vnlvalset, vnlvalset,
	flushmapentry, flushmapentry).
% :- mode vn_block__record_compulsory_lvals(in, di, uo, di, uo) is det.
:- mode vn_block__record_compulsory_lvals(in, in, out, in, out) is det.

vn_block__record_compulsory_lvals(VnTables, Livevals0, Livevals,
		FlushEntry0, FlushEntry) :-
	vn_table__get_vnlval_vn_list(VnTables, Lval_vn_list),
	vn_block__record_compulsory_lval_list(Lval_vn_list, Livevals0, Livevals,
		FlushEntry0, FlushEntry).

:- pred vn_block__record_compulsory_lval_list(assoc_list(vnlval, vn),
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
% :- mode vn_block__record_compulsory_lval_list(in, di, uo, di, uo) is det.
:- mode vn_block__record_compulsory_lval_list(in, in, out, in, out) is det.

vn_block__record_compulsory_lval_list([], Livevals, Livevals,
	FlushEntry, FlushEntry).
vn_block__record_compulsory_lval_list([Vnlval - Vn | Lval_vn_list],
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	(
		( Vnlval = vn_field(_, _, _)
		; Vnlval = vn_redoip(_)
		; Vnlval = vn_framevar(_)
		; Vnlval = vn_curfr
		; Vnlval = vn_maxfr
		; Vnlval = vn_succip
		)
	->
		map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
		set__insert(Livevals0, Vnlval, Livevals1)
	;
		FlushEntry1 = FlushEntry0,
		Livevals1 = Livevals0
	),
	vn_block__record_compulsory_lval_list(Lval_vn_list,
		Livevals1, Livevals, FlushEntry1, FlushEntry).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_block__find_cheaper_copies(lval, vn, vn_tables, list(parentry)).
:- mode vn_block__find_cheaper_copies(in, in, in, out) is det.

vn_block__find_cheaper_copies(Lval, Vn, VnTables, ParEntries) :-
	vn_cost__lval_cost(Lval, LvalCost),
	vn_table__lookup_current_locs(Vn, CurVnlvals,
		"vn_block__find_cheaper_copies", VnTables),
	vn_block__find_cheaper_copies_2(CurVnlvals, LvalCost, CheapRvals),
	( CheapRvals = [] ->
		ParEntries = []
	;
		ParEntries = [Lval - CheapRvals]
	).

:- pred vn_block__find_cheaper_copies_2(list(vnlval), int, list(rval)).
:- mode vn_block__find_cheaper_copies_2(in, in, out) is det.

vn_block__find_cheaper_copies_2([], _, []).
vn_block__find_cheaper_copies_2([Vnlval | Vnlvals], OldCost, Rvals) :-
	vn_block__find_cheaper_copies_2(Vnlvals, OldCost, Rvals0),
	vn_util__no_access_vnlval_to_lval(Vnlval, MaybeLval),
	(
		MaybeLval = yes(Lval),
		vn_cost__lval_cost(Lval, LvalCost),
		LvalCost < OldCost
	->
		Rvals = [lval(Lval) | Rvals0]
	;
		Rvals = Rvals0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

vn_block__split_at_next_ctrl_instr([], _, _, _) :-
	error("could not find next ctrl instr").
vn_block__split_at_next_ctrl_instr([Instr0 | Instrs0], Before, Instr, After) :-
	Instr0 = Uinstr0 - _,
	( vn_block__is_ctrl_instr(Uinstr0, yes) ->
		Before = [],
		Instr = Instr0,
		After = Instrs0
	;
		vn_block__split_at_next_ctrl_instr(Instrs0, Before0, Instr,
			After),
		Before = [Instr0 | Before0]
	).

:- pred vn_block__is_ctrl_instr(instr, bool).
:- mode vn_block__is_ctrl_instr(in, out) is det.

vn_block__is_ctrl_instr(comment(_), no).
vn_block__is_ctrl_instr(livevals(_), yes).
vn_block__is_ctrl_instr(block(_, _), no).
vn_block__is_ctrl_instr(assign(_, _), no).
vn_block__is_ctrl_instr(call(_, _, _, _), yes).
vn_block__is_ctrl_instr(call_closure(_, _, _), yes).
vn_block__is_ctrl_instr(mkframe(_, _, _), yes).
vn_block__is_ctrl_instr(modframe(_), no).
vn_block__is_ctrl_instr(label(_), yes).
vn_block__is_ctrl_instr(goto(_), yes).
vn_block__is_ctrl_instr(computed_goto(_, _), yes).
vn_block__is_ctrl_instr(c_code(_), no).
vn_block__is_ctrl_instr(if_val(_, _), yes).
vn_block__is_ctrl_instr(incr_hp(_, _, _), no).
vn_block__is_ctrl_instr(mark_hp(_), yes).
vn_block__is_ctrl_instr(restore_hp(_), yes).
vn_block__is_ctrl_instr(incr_sp(_), yes).
vn_block__is_ctrl_instr(decr_sp(_), yes).
vn_block__is_ctrl_instr(pragma_c(_, _, _, _), no).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
