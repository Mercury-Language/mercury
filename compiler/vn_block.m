%-----------------------------------------------------------------------------%

% Vn_build.nl - Divide a procedure into extended basic blocks, building up
%	a description of each basic block and handing them over to be flushed.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_block.

:- interface.

:- import_module vn_type.
:- import_module llds, list, bintree_set.

	% Optimize instructions, assume we are outside of a block.

:- pred vn__opt_non_block(list(instruction), livemap, list(instruction)).
:- mode vn__opt_non_block(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module vn_order, vn_table, vn_util, opt_debug.
:- import_module map, int, string, require, std_util.

vn__opt_non_block([], _, []).
vn__opt_non_block([Instr0 | Instrs0], Livemap, Instrs) :-
	Instr0 = Uinstr0 - _Comment0,
	(
		Uinstr0 = label(_)
	->
		vn__init_tables(Vn_tables0),
		bintree_set__init(Liveset0),
		map__init(Ctrlmap0),
		map__init(Flushmap0),
		vn__opt_block(Instrs0, Vn_tables0, Livemap, Liveset0,
			no, Ctrlmap0, Flushmap0, 0, [], NewInstrs),
		Instrs = [Instr0 | NewInstrs]
	;
		Uinstr0 = comment(_)
	->
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		% we get here only if label elim is switched off
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

	% Optimize instructions, assuming we are inside of a block.

:- pred vn__opt_block(list(instruction), vn_tables, livemap, vnlvalset, bool,
	ctrlmap, flushmap, int, list(instruction), list(instruction)).
:- mode vn__opt_block(in, in, in, in, in, in, in, in, in, out) is det.

vn__opt_block([], _, _, _, _, _, _, _, _, []) :-
	error("block has no terminator").
vn__opt_block([Instr0 | Instrs0], Vn_tables, Livemap, Livevals, Incrhp,
		Ctrlmap0, Flushmap0, Ctrl0, Prev, Instrs) :-
	vn__handle_instr(Instr0, Vn_tables, Livemap, Livevals, Incrhp,
		Ctrlmap0, Flushmap0, Ctrl0, Prev, Instrs0, Instrs).

%-----------------------------------------------------------------------------%

:- pred vn__handle_instr(instruction, vn_tables, livemap, vnlvalset, bool,
	ctrlmap, flushmap, int, list(instruction),
	list(instruction), list(instruction)).
:- mode vn__handle_instr(in, in, in, in, in, in, in, in, in, in, out) is det.

vn__handle_instr(Instr0, Vn_tables0, Livemap, Livevals0, Incrhp,
		Ctrlmap0, Flushmap0, Ctrl0, Prev, Instrs0, Instrs) :-
	Instr0 = Uinstr0 - Comment,
	%% opt_debug__dump_instr(Uinstr0, I_str),
	%% opt_debug__write("\n\nInstr: "),
	%% opt_debug__write(I_str),
	%% bintree_set__to_sorted_list(Livevals0, Livelist),
	%% opt_debug__dump_vnlvals(Livelist, L_str),
	%% opt_debug__write("\nLivevals: "),
	%% opt_debug__write(L_str),
	(
		Uinstr0 = comment(_),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Livevals0, Incrhp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = livevals(Livevals),
		bintree_set__to_sorted_list(Livevals, List),
		vn__convert_to_vnlval_and_insert(List, Livevals0, Livevals1),
		vn__opt_block(Instrs0, Vn_tables0, Livemap,
			Livevals1, Incrhp,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = block(_, _),
		error("block should not be found in handle_instr")
	;
		Uinstr0 = assign(Lval, Rval),
		vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
		vn__lval_to_vnlval(Lval, Vnlval, Vn_tables1, Vn_tables2),
		vn__set_desired_value(Vnlval, Vn, Vn_tables2, Vn_tables3),
		vn__find_specials(Vnlval, Specials),
		bintree_set__insert_list(Livevals0, Specials, Livevals1),
		( Comment = "incr_hp" ->
			Prev1 = Prev
		;
			Prev1 = [Instr0 | Prev]
		),
		vn__opt_block(Instrs0, Vn_tables3, Livemap,
			Livevals1, Incrhp,
			Ctrlmap0, Flushmap0, Ctrl0, Prev1, Instrs)
	;
		Uinstr0 = call(ProcAddr, ReturnAddr, LiveInfo),
		vn__new_ctrl_node(vn_call(ProcAddr, ReturnAddr, LiveInfo),
			Livemap, Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Vn_tables1,
			Incrhp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = call_closure(ClAddr, ReturnAddr, LiveInfo),
		vn__new_ctrl_node(vn_call_closure(ClAddr, ReturnAddr, LiveInfo),
			Livemap, Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Vn_tables1,
			Incrhp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = mkframe(Name, Size, Redoip),
		vn__new_ctrl_node(vn_mkframe(Name, Size, Redoip), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap,
			Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = modframe(Redoip),
		vn__new_ctrl_node(vn_modframe(Redoip), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap,
			Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = label(Label),
		vn__new_ctrl_node(vn_label(Label), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Vn_tables1,
			Incrhp, Ctrlmap1, Flushmap1, Ctrl1,
			Prev, FlushInstrs),
		vn__opt_non_block([Instr0 | Instrs0], Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = goto(CodeAddr),
		vn__new_ctrl_node(vn_goto(CodeAddr), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Vn_tables1,
			Incrhp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = computed_goto(Rval, Labels),
		vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
		vn__new_ctrl_node(vn_computed_goto(Vn, Labels), Livemap,
			Vn_tables1, Vn_tables2, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__flush_all_nodes(Livevals1, Vn_tables2,
			Incrhp, Ctrlmap1, Flushmap1, Ctrl1,
			[Instr0 | Prev], FlushInstrs),
		vn__opt_non_block(Instrs0, Livemap, Instrs1),
		list__append(FlushInstrs, Instrs1, Instrs)
	;
		Uinstr0 = c_code(_),
		error("c_code in handle_instr")
	;
		Uinstr0 = if_val(Rval, CodeAddr),
		vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
		vn__new_ctrl_node(vn_if_val(Vn, CodeAddr), Livemap,
			Vn_tables1, Vn_tables2, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables2, Livemap,
			Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = incr_hp(Lval, MaybeTag, Rval),
		(
			MaybeTag = no,
			StoreInstr = assign(Lval, lval(hp))
				- "incr_hp"
		;
			MaybeTag = yes(Tag),
			StoreInstr = assign(Lval, mkword(Tag, lval(hp)))
				- "incr_hp"
		),
		IncrInstr = assign(hp, binop((+), lval(hp),
		%	Rval)) - "",
			binop((*), Rval, const(int_const(4)))))
				- "incr_hp",
		Instrs1 = [StoreInstr, IncrInstr | Instrs0],
		vn__opt_block(Instrs1, Vn_tables0, Livemap, Livevals0, yes,
			Ctrlmap0, Flushmap0, Ctrl0, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = mark_hp(Lval),
		vn__lval_to_vnlval(Lval, Vnlval, Vn_tables0, Vn_tables1),
		vn__new_ctrl_node(vn_mark_hp(Vnlval), Livemap,
			Vn_tables1, Vn_tables2, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables2, Livemap, Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = restore_hp(Rval),
		vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
		vn__new_ctrl_node(vn_restore_hp(Vn), Livemap,
			Vn_tables1, Vn_tables2, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables2, Livemap, Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = incr_sp(N),
		vn__new_ctrl_node(vn_incr_sp(N), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap, Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	;
		Uinstr0 = decr_sp(N),
		vn__new_ctrl_node(vn_decr_sp(N), Livemap,
			Vn_tables0, Vn_tables1, Livevals0, Livevals1,
			Ctrlmap0, Ctrlmap1, Flushmap0, Flushmap1, Ctrl0, Ctrl1),
		vn__opt_block(Instrs0, Vn_tables1, Livemap, Livevals1, Incrhp,
			Ctrlmap1, Flushmap1, Ctrl1, [Instr0 | Prev], Instrs)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__new_ctrl_node(vn_instr, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, ctrlmap, ctrlmap, flushmap, flushmap, int, int).
:- mode vn__new_ctrl_node(in, in, di, uo, di, uo, di, uo, di, uo, in, out)
	is det.

:- implementation.

vn__new_ctrl_node(Vn_instr, Livemap, Vn_tables0, Vn_tables, Livevals0, Livevals,
		Ctrlmap0, Ctrlmap, Flushmap0, Flushmap, Ctrl0, Ctrl) :-
	Ctrl is Ctrl0 + 1,
	map__set(Ctrlmap0, Ctrl0, Vn_instr, Ctrlmap),
	map__init(FlushEntry0),
	(
		Vn_instr = vn_call(_, _, _),
		vn__record_at_call(Vn_tables0, Vn_tables, Livevals0, Livevals,
			FlushEntry0, FlushEntry)
	;
		Vn_instr = vn_call_closure(_, _, _),
		vn__record_at_call(Vn_tables0, Vn_tables, Livevals0, Livevals,
			FlushEntry0, FlushEntry)
	;
		Vn_instr = vn_mkframe(_, _, _),
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
	;
		Vn_instr = vn_modframe(_),
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
	;
		Vn_instr = vn_label(Label),
		vn__record_one_label(Label, Livemap, Vn_tables0, Vn_tables,
			Livevals0, Livevals, FlushEntry0, FlushEntry)
	;
		Vn_instr = vn_goto(TargetAddr),
		(
			TargetAddr = label(Label),
			map__search(Livemap, Label, _)
		->
			vn__record_one_label(Label, Livemap,
				Vn_tables0, Vn_tables, Livevals0, Livevals,
				FlushEntry0, FlushEntry)
		;
			vn__record_at_call(Vn_tables0, Vn_tables,
				Livevals0, Livevals, FlushEntry0, FlushEntry)
		)
	;
		Vn_instr = vn_computed_goto(_, Labels),
		vn__record_several_labels(Labels, Livemap,
			Vn_tables0, Vn_tables, Livevals0, Livevals,
			FlushEntry0, FlushEntry)
	;
		Vn_instr = vn_if_val(_, TargetAddr),
		(
			TargetAddr = label(Label),
			map__search(Livemap, Label, _)
		->
			vn__record_one_label(Label, Livemap,
				Vn_tables0, Vn_tables, Livevals0, Livevals,
				FlushEntry0, FlushEntry)
		;
			bintree_set__to_sorted_list(Livevals0, Livelist),
			vn__record_livevnlvals(Livelist, Vn_tables0, Vn_tables,
				Livevals0, Livevals, FlushEntry0, FlushEntry)
		)
	;
		Vn_instr = vn_mark_hp(Vnlval),
		vn__rval_to_vn(lval(hp), Vn, Vn_tables0, Vn_tables1),
		vn__set_desired_value(Vnlval, Vn, Vn_tables1, Vn_tables),
		bintree_set__insert(Livevals0, Vnlval, Livevals),
		FlushEntry = FlushEntry0
	;
		Vn_instr = vn_restore_hp(_Vn),
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
	;
		Vn_instr = vn_incr_sp(_),
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
	;
		Vn_instr = vn_decr_sp(_),
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
	),
	map__set(Flushmap0, Ctrl0, FlushEntry, Flushmap).

%-----------------------------------------------------------------------------%

	% Compute the flushmap entry for a call or for a possible branch to
	% a label or a set of labels.

:- pred vn__record_at_call(vn_tables, vn_tables, vnlvalset, vnlvalset,
	flushmapentry, flushmapentry).
:- mode vn__record_at_call(di, uo, di, uo, di, uo) is det.

vn__record_at_call(Vn_tables0, Vn_tables, Livevals0, Livevals,
		FlushEntry0, FlushEntry) :-
	bintree_set__to_sorted_list(Livevals0, Livelist),
	vn__record_livevnlvals(Livelist, Vn_tables0, Vn_tables,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1),
	vn__record_compulsory_lvals(Vn_tables, Livevals1, Livevals,
		FlushEntry1, FlushEntry).

:- pred vn__record_several_labels(list(label), livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_several_labels(in, in, di, uo, di, uo, di, uo) is det.

vn__record_several_labels(Labels, Livemap, Vn_tables0, Vn_tables,
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	vn__record_labels(Labels, Livemap, Vn_tables0, Vn_tables,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1),
	vn__record_compulsory_lvals(Vn_tables, Livevals1, Livevals,
		FlushEntry1, FlushEntry).

:- pred vn__record_one_label(label, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_one_label(in, in, di, uo, di, uo, di, uo) is det.

vn__record_one_label(Label, Livemap, Vn_tables0, Vn_tables,
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	vn__record_label(Label, Livemap, Vn_tables0, Vn_tables,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1),
	vn__record_compulsory_lvals(Vn_tables, Livevals1, Livevals,
		FlushEntry1, FlushEntry).

%-----------------------------------------------------------------------------%

:- pred vn__record_labels(list(label), livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_labels(in, in, di, uo, di, uo, di, uo) is det.

vn__record_labels([], _, Vn_tables, Vn_tables, Livevals, Livevals,
	FlushEntry, FlushEntry).
vn__record_labels([Label | Labels], Livemap, Vn_tables0, Vn_tables,
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	vn__record_label(Label, Livemap, Vn_tables0, Vn_tables1,
		Livevals0, Livevals1, FlushEntry0, FlushEntry1),
	vn__record_labels(Labels, Livemap, Vn_tables1, Vn_tables,
		Livevals1, Livevals, FlushEntry1, FlushEntry).

:- pred vn__record_label(label, livemap, vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_label(in, in, di, uo, di, uo, di, uo) is det.

vn__record_label(Label, Livemap, Vn_tables0, Vn_tables, Livevals0, Livevals,
		FlushEntry0, FlushEntry) :-
	( map__search(Livemap, Label, Liveset) ->
		bintree_set__to_sorted_list(Liveset, Livelist),
		vn__record_livevals(Livelist, Vn_tables0, Vn_tables,
			Livevals0, Livevals, FlushEntry0, FlushEntry)
	;
		opt_debug__dump_label(Label, L_str),
		string__append_list(["cannot find label ",
			L_str, " in Livemap"], Str),
		error(Str)
	).

:- pred vn__record_livevals(list(lval), vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_livevals(in, di, uo, di, uo, di, uo) is det.

vn__record_livevals([], Vn_tables, Vn_tables,
		Livevals, Livevals, FlushEntry, FlushEntry).
vn__record_livevals([Lval | Livelist], Vn_tables0, Vn_tables,
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	vn__no_heap_lval_to_vnlval(Lval, MaybeVnlval),
	(
		MaybeVnlval = yes(Vnlval),
		( vn__search_desired_value(Vnlval, VnPrime, Vn_tables0) ->
			Vn = VnPrime,
			Vn_tables1 = Vn_tables0
		;
			vn__record_first_vnlval(Vnlval, Vn,
				Vn_tables0, Vn_tables1)
		),
		map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
		bintree_set__insert(Livevals0, Vnlval, Livevals1)
	;
		MaybeVnlval = no,
		Vn_tables1 = Vn_tables0,
		Livevals1 = Livevals0,
		FlushEntry1 = FlushEntry0
	),
	vn__record_livevals(Livelist, Vn_tables1, Vn_tables,
		Livevals1, Livevals, FlushEntry1, FlushEntry).

:- pred vn__record_livevnlvals(list(vnlval), vn_tables, vn_tables,
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_livevnlvals(in, di, uo, di, uo, di, uo) is det.

vn__record_livevnlvals([], Vn_tables, Vn_tables,
		Livevals, Livevals, FlushEntry, FlushEntry).
vn__record_livevnlvals([Vnlval | Livevnlvallist], Vn_tables0, Vn_tables,
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	( vn__search_desired_value(Vnlval, VnPrime, Vn_tables0) ->
		Vn = VnPrime,
		Vn_tables1 = Vn_tables0
	;
		vn__record_first_vnlval(Vnlval, Vn,
			Vn_tables0, Vn_tables1)
	),
	map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
	bintree_set__insert(Livevals0, Vnlval, Livevals1),
	vn__record_livevnlvals(Livevnlvallist, Vn_tables1, Vn_tables,
		Livevals1, Livevals, FlushEntry1, FlushEntry).

%-----------------------------------------------------------------------------%

	% Insert the heap references that have been made so far into the
	% live vnlval set, and record their currently desired value numbers
	% into the given flushmap entry.

:- pred vn__record_compulsory_lvals(vn_tables, vnlvalset, vnlvalset,
	flushmapentry, flushmapentry).
:- mode vn__record_compulsory_lvals(in, di, uo, di, uo) is det.

vn__record_compulsory_lvals(Vn_tables, Livevals0, Livevals,
		FlushEntry0, FlushEntry) :-
	vn__get_vnlval_vn_list(Vn_tables, Lval_vn_list),
	vn__record_compulsory_lval_list(Lval_vn_list, Livevals0, Livevals,
		FlushEntry0, FlushEntry).

:- pred vn__record_compulsory_lval_list(assoc_list(vnlval, vn),
	vnlvalset, vnlvalset, flushmapentry, flushmapentry).
:- mode vn__record_compulsory_lval_list(in, di, uo, di, uo) is det.

vn__record_compulsory_lval_list([], Livevals, Livevals, FlushEntry, FlushEntry).
vn__record_compulsory_lval_list([Vnlval - Vn | Lval_vn_list],
		Livevals0, Livevals, FlushEntry0, FlushEntry) :-
	vn__vnlval_access_vns(Vnlval, Access),
	(
		Access = [_|_],
		map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
		bintree_set__insert(Livevals0, Vnlval, Livevals1)
	;
		Access = [],
		FlushEntry1 = FlushEntry0,
		Livevals1 = Livevals0
	),
	vn__record_compulsory_lval_list(Lval_vn_list,
		Livevals1, Livevals, FlushEntry1, FlushEntry).
