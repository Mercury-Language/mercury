%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_flush.m - flush the nodes of the vn graph in order.

% Author: zs.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module vn_flush.

:- interface.

:- import_module vn_type, vn_table, vn_temploc.
:- import_module llds, list.

	% Flush the given nodes in the given order.

:- pred vn_flush__nodelist(list(vn_node), ctrlmap, vn_tables, templocs,
	vn_params, list(instruction), io__state, io__state).
:- mode vn_flush__nodelist(in, in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, map, int, string, require, std_util.

:- import_module builtin_ops, vn_table, vn_util, vn_debug, opt_debug.

vn_flush__nodelist([], _, _, _, _, []) --> [].
vn_flush__nodelist([Node0 | Nodes0], Ctrlmap, VnTables0, Templocs0, Params,
		Instrs) -->
	( { Node0 = node_origlval(_) } ->
		{ Nodes1 = Nodes0 },
		{ VnTables1 = VnTables0 },
		{ Templocs1 = Templocs0 },
		{ Instrs0 = [] }
	;
		vn_flush__node(Node0, Ctrlmap,
			Nodes0, Nodes1, VnTables0, VnTables1,
			Templocs0, Templocs1, Params,Instrs0)
	),
	vn_flush__nodelist(Nodes1, Ctrlmap, VnTables1, Templocs1, Params,
		Instrs1),
	{ list__append(Instrs0, Instrs1, Instrs) }.

	% Flush the given node.

:- pred vn_flush__node(vn_node, ctrlmap, list(vn_node), list(vn_node),
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction),
	io__state, io__state).
:- mode vn_flush__node(in, in, in, out, in, out, in, out, in, out, di, uo)
	is det.

vn_flush__node(Node, Ctrlmap, Nodes0, Nodes, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) -->
	vn_debug__flush_start_msg(Node),
	(
		{ Node = node_shared(Vn) },
		vn_flush__shared_node(Vn, Nodes0, Nodes, VnTables0, VnTables,
			Templocs0, Templocs, Params, Instrs)
	;
		{ Node = node_lval(Vnlval) },
		vn_flush__lval_node(Vnlval, Ctrlmap, Nodes0, Nodes,
			VnTables0, VnTables, Templocs0, Templocs, Params,
			Instrs)
	;
		{ Node = node_origlval(_Vnlval) },
		{ Nodes = Nodes0 },
		{ VnTables = VnTables0 },
		{ Templocs = Templocs0 },
		{ Instrs = [] }
	;
		{ Node = node_ctrl(N) },
		{ Nodes = Nodes0 },
		{ map__lookup(Ctrlmap, N, VnInstr) },
		{ vn_flush__ctrl_node(VnInstr, N, VnTables0, VnTables,
			Templocs0, Templocs, Params, Instrs) }
	),
	% we should look at all the temporary regs here and call reuse_temploc
	% for the ones that store values that are not live and are not needed
	% any more.
	vn_debug__flush_end_msg(Instrs, VnTables).

%-----------------------------------------------------------------------------%

:- pred vn_flush__lval_node(vnlval, ctrlmap, list(vn_node), list(vn_node),
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction),
	io__state, io__state).
:- mode vn_flush__lval_node(in, in, in, out, in, out, in, out, in, out, di, uo)
	is det.

vn_flush__lval_node(Vnlval, Ctrlmap, Nodes0, Nodes,
		VnTables0, VnTables, Templocs0, Templocs, Params, Instrs) -->
	{ vn_table__lookup_desired_value(Vnlval, DesVn, "vn_flush__lval_node",
		VnTables0) },
	{ vn_table__lookup_current_value(Vnlval, CurVn, "vn_flush__lval_node",
		VnTables0) },
	(
		% Even if a vnlval already has the right value,
		% we must make sure its access path will not be
		% needed again. This requires its storage in a
		% register or temporary if it is ever used again.

		{ CurVn = DesVn },
		{ vn_util__vnlval_access_vns(Vnlval, AccessVns) },
		{ AccessVns = [_|_] },
		{ vn_table__lookup_uses(DesVn, Uses, "vn_flush__lval_node",
			VnTables0) },
		{ vn_util__real_uses(Uses, RealUses, VnTables0) },
		{ RealUses = [_|_] }
	->
		( { RealUses = [src_liveval(UserVnlval)] } ->
			vn_flush__node(node_lval(UserVnlval), Ctrlmap,
				Nodes0, Nodes1, VnTables0, VnTables1,
				Templocs0, Templocs1, Params, Instrs1),
			{ list__delete_all(Nodes1, node_lval(UserVnlval),
				Nodes) }
		;
			% This path should be taken only if some circularities
			% are broken arbitrarily. Otherwise, the shared node
			% should come before the user lval nodes.
			vn_flush__node(node_shared(DesVn), Ctrlmap,
				Nodes0, Nodes, VnTables0, VnTables1,
				Templocs0, Templocs1, Params, Instrs1)
		),
		{ vn_flush__ensure_assignment(Vnlval, DesVn, [],
			VnTables1, VnTables,
			Templocs1, Templocs, Params, Instrs2) },
		{ list__append(Instrs1, Instrs2, Instrs) }
	;
		{ vn_flush__ensure_assignment(Vnlval, DesVn, [],
			VnTables0, VnTables,
			Templocs0, Templocs, Params, Instrs) },
		{ Nodes = Nodes0 }
	).

%-----------------------------------------------------------------------------%

:- pred vn_flush__shared_node(vn, list(vn_node), list(vn_node),
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction),
	io__state, io__state).
:- mode vn_flush__shared_node(in, in, out, in, out, in, out, in, out, di, uo)
	is det.

vn_flush__shared_node(Vn, Nodes0, Nodes, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) -->
	( { vn_table__lookup_uses(Vn, [], "vn_flush__shared_node", VnTables0) } ->
		% earlier nodes must have taken care of this vn
		{ Nodes = Nodes0 },
		{ VnTables = VnTables0 },
		{ Templocs = Templocs0 },
		{ Instrs = [] }
	;
		{ vn_flush__choose_loc_for_shared_vn(Vn, Vnlval, VnTables0,
			Templocs0, Templocs1) },
		( { vn_table__search_desired_value(Vnlval, Vn, VnTables0) } ->
			vn_debug__flush_also_msg(Vnlval),
			{ list__delete_all(Nodes0, node_lval(Vnlval), Nodes) }
		;
			{ Nodes = Nodes0 }
		),
		{ vn_flush__ensure_assignment(Vnlval, Vn, [],
			VnTables0, VnTables, Templocs1, Templocs, Params,
			Instrs) }
	).

%-----------------------------------------------------------------------------%

:- pred vn_flush__ctrl_node(vn_instr, int, vn_tables, vn_tables,
	templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__ctrl_node(in, in, in, out, in, out, in, out) is det.

vn_flush__ctrl_node(Vn_instr, N, VnTables0, VnTables, Templocs0, Templocs,
		Params, Instrs) :-
	(
		Vn_instr = vn_livevals(Livevals),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [livevals(Livevals) - ""]
	;
		Vn_instr = vn_call(ProcAddr, RetAddr, LiveInfo,
			Context, CodeModel),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [call(ProcAddr, RetAddr, LiveInfo,
			Context, CodeModel) - ""]
	;
		Vn_instr = vn_mkframe(NondetFrameInfo, Redoip),
		vn_util__rval_to_vn(const(code_addr_const(Redoip)), AddrVn,
			VnTables0, VnTables1),
		vn_util__lval_to_vnlval(redoip(lval(maxfr)), SlotVnlval,
			VnTables1, VnTables2),
		vn_table__set_current_value(SlotVnlval, AddrVn,
			VnTables2, VnTables),
		Templocs = Templocs0,
		Instrs = [mkframe(NondetFrameInfo, Redoip) - ""]
	;
		Vn_instr = vn_label(Label),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [label(Label) - ""]
	;
		Vn_instr = vn_goto(TargetAddr),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [goto(TargetAddr) - ""]
	;
		Vn_instr = vn_computed_goto(Vn, Labels),
		vn_flush__vn(Vn, [src_ctrl(N)], [], Rval, VnTables0, VnTables,
			Templocs0, Templocs, Params, FlushInstrs),
		Instr = computed_goto(Rval, Labels) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_if_val(Vn, TargetAddr),
		vn_flush__vn(Vn, [src_ctrl(N)], [], Rval, VnTables0, VnTables,
			Templocs0, Templocs, Params, FlushInstrs),
		Instr = if_val(Rval, TargetAddr) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_mark_hp(Vnlval),
		vn_flush__access_path(Vnlval, [src_ctrl(N)], [], Lval,
			VnTables0, VnTables1, Templocs0, Templocs, Params,
			FlushInstrs),
		vn_table__lookup_assigned_vn(vn_origlval(vn_hp), OldhpVn,
			"vn_flush__ctrl_node", VnTables1),
		vn_table__set_current_value(Vnlval, OldhpVn,
			VnTables1, VnTables),
		Instr = mark_hp(Lval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_restore_hp(Vn),
		vn_flush__vn(Vn, [src_ctrl(N)], [], Rval, VnTables0, VnTables1,
			Templocs0, Templocs, Params, FlushInstrs),
		vn_table__set_current_value(vn_hp, Vn, VnTables1, VnTables),
		Instr = restore_hp(Rval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_free_heap(Vn),
		vn_flush__vn(Vn, [src_ctrl(N)], [], Rval, VnTables0, VnTables,
			Templocs0, Templocs, Params, FlushInstrs),
		Instr = free_heap(Rval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_store_ticket(Vnlval),
		vn_flush__access_path(Vnlval, [src_ctrl(N)], [], Lval,
			VnTables0, VnTables1, Templocs0, Templocs, Params,
			FlushInstrs),
		vn_table__lookup_assigned_vn(vn_origlval(Vnlval), OldVn,
			"vn_flush__ctrl_node", VnTables1),
		vn_table__set_current_value(Vnlval, OldVn, VnTables1, VnTables),
		Instr = store_ticket(Lval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_reset_ticket(Vn, Reason),
		vn_flush__vn(Vn, [src_ctrl(N)], [], Rval, VnTables0, VnTables,
			Templocs0, Templocs, Params, FlushInstrs),
		Instr = reset_ticket(Rval, Reason) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_discard_ticket,
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [discard_ticket - ""]
	;
		Vn_instr = vn_mark_ticket_stack(Vnlval),
		vn_flush__access_path(Vnlval, [src_ctrl(N)], [], Lval,
			VnTables0, VnTables1, Templocs0, Templocs, Params,
			FlushInstrs),
		vn_table__lookup_assigned_vn(vn_origlval(Vnlval), OldVn,
			"vn_flush__ctrl_node", VnTables1),
		vn_table__set_current_value(Vnlval, OldVn, VnTables1, VnTables),
		Instr = mark_ticket_stack(Lval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_discard_tickets_to(Vn),
		vn_flush__vn(Vn, [src_ctrl(N)], [], Rval, VnTables0, VnTables,
			Templocs0, Templocs, Params, FlushInstrs),
		Instr = discard_tickets_to(Rval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_incr_sp(Incr, Msg),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [incr_sp(Incr, Msg) - ""]
	;
		Vn_instr = vn_decr_sp(Decr),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [decr_sp(Decr) - ""]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Choose a location for a shared value number that does not have to go
	% anywhere specific right now; we do have to ensure that the chosen
	% location is accessible without access vns.

	% We prefer to choose a register or stack slot that already has the
	% value; failing that, a register or stack slot that would like to
	% have the value and whose contents either don't have to be saved
	% or can be saved with a single instruction.

	% In either case we have to pay attention when we choose non-register
	% destinations. It pays to choose a non-register holder of the value
	% only if there is at most one other user of the value. It pays to
	% choose a non-register user only if there are no other users of the
	% value.

:- pred vn_flush__choose_loc_for_shared_vn(vn, vnlval, vn_tables,
	templocs, templocs).
% :- mode vn_flush__choose_loc_for_shared_vn(in, out, in, di, uo) is det.
:- mode vn_flush__choose_loc_for_shared_vn(in, out, in, in, out) is det.

vn_flush__choose_loc_for_shared_vn(Vn, Chosen, VnTables, Templocs0, Templocs) :-
	(
		vn_table__lookup_current_locs(Vn, CurrentLocs,
			"vn_flush__choose_loc_for_shared_vn", VnTables),
		vn_util__choose_cheapest_loc(CurrentLocs, BestHolder),
		vn_util__vnlval_access_vns(BestHolder, []),
		(
			vn_util__classify_loc_cost(BestHolder, 0)
		;
			vn_table__lookup_uses(Vn, Uses,
				"vn_flush__choose_loc_for_shared_vn", VnTables),
			list__delete_first(Uses, src_liveval(BestHolder),
				NewUses),
			( NewUses = [] ; NewUses = [_] )
		)
	->
		Chosen = BestHolder,
		Templocs = Templocs0
	;
		vn_flush__find_cheap_users(Vn, Users, VnTables),
		vn_util__choose_cheapest_loc(Users, BestUser),
		vn_util__vnlval_access_vns(BestUser, []),
		(
			vn_util__classify_loc_cost(BestUser, 0)
		;
			vn_table__lookup_uses(Vn, Uses,
				"vn_flush__choose_loc_for_shared_vn", VnTables),
			list__delete_first(Uses, src_liveval(BestUser), [])
		)
	->
		Chosen = BestUser,
		Templocs = Templocs0
	;
		vn_flush__choose_temp(Vn, VnTables, Templocs0, Templocs,
			Chosen)
	).

:- pred vn_flush__choose_temp(vn, vn_tables, templocs, templocs, vnlval).
:- mode vn_flush__choose_temp(in, in, in, out, out) is det.

vn_flush__choose_temp(Vn, VnTables, Templocs0, Templocs, Chosen) :-
	vn_table__lookup_defn(Vn, Vnrval, "vn_flush__choose_temp", VnTables),
	vn_type__vnrval_type(Vnrval, Type),
	( Type = float ->
		vn_temploc__next_tempf(Templocs0, Templocs, Chosen)
	;
		vn_temploc__next_tempr(Templocs0, Templocs, Chosen)
	).

%-----------------------------------------------------------------------------%

	% Find a 'user', location that would like to have the given vn.
	% The user should be 'cheap', i.e. either it should hold no value
	% that will ever be used by anybody else, or the value it holds
	% should be assignable to one of its users without needing any
	% more assignments. At the moment we insist that this user be
	% a register. We could allow stack/frame variables as well,
	% but we must not allow fields, or in general any location
	% that needs access vns. (When flushing framevars before assigning
	% to curfr, we don't want to pick anything that references a framevar
	% in any case.)

:- pred vn_flush__find_cheap_users(vn, list(vnlval), vn_tables).
:- mode vn_flush__find_cheap_users(in, out, in) is det.

vn_flush__find_cheap_users(Vn, Vnlvals, VnTables) :-
	( vn_table__search_uses(Vn, Uses, VnTables) ->
		vn_flush__find_cheap_users_2(Uses, Vnlvals, VnTables)
	;
		Vnlvals = []
	).

:- pred vn_flush__find_cheap_users_2(list(vn_src), list(vnlval), vn_tables).
:- mode vn_flush__find_cheap_users_2(in, out, in) is det.

vn_flush__find_cheap_users_2([], [], _VnTables).
vn_flush__find_cheap_users_2([Src | Srcs], Vnlvals, VnTables) :-
	vn_flush__find_cheap_users_2(Srcs, Vnlvals0, VnTables),
	(
		Src = src_liveval(Live)
		% \+ Live = vn_field(_, _, _)
	->
		( vn_table__search_current_value(Live, Vn, VnTables) ->
			vn_table__lookup_uses(Vn, Uses,
				"vn_flush__find_cheap_users_2", VnTables),
			(
				Uses = []
			->
				% Live's current value is not used.
				Vnlvals = [Live | Vnlvals0]
			;
				list__member(UserSrc, Uses),
				(
					UserSrc = src_liveval(User),
					vn_table__search_current_value(User,
						UserVn, VnTables)
				->
					User = vn_reg(_, _),
					vn_table__lookup_uses(UserVn, [],
						"vn_flush__find_cheap_users_2",
						VnTables)
				;
					true
				)
			->
				% Live's current value can be saved to User
				% without any further action.
				Vnlvals = [Live | Vnlvals0]
			;
				Vnlvals = Vnlvals0
			)
		;
			% Live doesn't have a value we know about.
			Vnlvals = [Live | Vnlvals0]
		)
	;
		Vnlvals = Vnlvals0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_flush__ensure_assignment(vnlval, vn, list(lval),
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__ensure_assignment(in, in, in, in, out, in, out, in, out)
	is det.

vn_flush__ensure_assignment(Vnlval, Vn, Forbidden, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) :-
	(
		vn_table__search_current_value(Vnlval, Cur_vn, VnTables0),
		Vn = Cur_vn
	->
		vn_table__del_old_use(Vn, src_liveval(Vnlval),
			VnTables0, VnTables1),
		vn_util__vnlval_access_vns(Vnlval, SubVns),
		vn_table__del_old_uses(SubVns, src_access(Vnlval),
			VnTables1, VnTables),
		Templocs = Templocs0,
		Instrs = []
	;
		vn_flush__generate_assignment(Vnlval, Vn, Forbidden,
			VnTables0, VnTables, Templocs0, Templocs, Params,
			_, Instrs)
	).

:- pred vn_flush__generate_assignment(vnlval, vn, list(lval),
	vn_tables, vn_tables, templocs, templocs, vn_params,
	lval, list(instruction)).
:- mode vn_flush__generate_assignment(in, in, in, in, out, in, out, in,
	out, out) is det.

vn_flush__generate_assignment(Vnlval, Vn, Forbidden0, VnTables0, VnTables,
		Templocs0, Templocs, Params, Lval, Instrs) :-
	( Vnlval = vn_hp ->
		error("vn_hp should never need to be explicitly flushed")
		% It should be done by the first reference to the old value
		% of the heap pointer, which should generate an incr_hp.
	;
		true
	),
	( vn_table__search_current_value(Vnlval, OldVn0, VnTables0) ->
		SaveVn = yes(OldVn0)
	;
		SaveVn = no
	),
	% Only lvals on the heap must have their access path flushed,
	% but they cannot appear on the temploc list, so of the next
	% next two calls, at most one will modify Temploc.
	vn_temploc__no_temploc(Vnlval, Templocs0, Templocs1),
	vn_flush__access_path(Vnlval, [src_access(Vnlval)], Forbidden0, Lval,
		VnTables0, VnTables1, Templocs1, Templocs2, Params,
		AccessInstrs),
	vn_flush__vn(Vn, [src_liveval(Vnlval)], Forbidden0, Rval,
		VnTables1, VnTables2, Templocs2, Templocs3, Params,
		FlushInstrs0),
	% The 'current' value of Vnlval may be changed by flush_vn if it
	% involves a reference to the old value of hp.
	(
		SaveVn = yes(OldVn),
		vn_util__find_lvals_in_rval(Rval, Forbidden1),
		list__append(Forbidden0, Forbidden1, Forbidden),
		vn_flush__maybe_save_prev_value(Vnlval, OldVn, Vn, Forbidden,
			VnTables2, VnTables3, Templocs3, Templocs, Params,
			SaveInstrs)
	;
		SaveVn = no,
		VnTables3 = VnTables2,
		Templocs = Templocs3,
		SaveInstrs = []
	),
	( vn_table__search_current_value(Vnlval, Vn, VnTables3) ->
		% Flush_vn must perform the entire assignment if it involves
		% exactly the actions of an incr_hp operation. Since the
		% incr_hp in FlushInstrs overwrites Lval, we must perform it
		% after Lval's old value has been saved.
		vn_flush__get_incr_hp(FlushInstrs0, Instr, FlushInstrs1),
		VnTables = VnTables3
	;
		vn_table__set_current_value(Vnlval, Vn, VnTables3, VnTables),
		Instr = assign(Lval, Rval) - "vn flush",
		FlushInstrs1 = FlushInstrs0
	),
	list__condense([AccessInstrs, FlushInstrs1, SaveInstrs, [Instr]],
		Instrs).

	% Remove the incr_hp instruction from the list and return it
	% separately.

:- pred vn_flush__get_incr_hp(list(instruction), instruction, list(instruction)).
% :- mode vn_flush__get_incr_hp(di, out, uo) is det.
:- mode vn_flush__get_incr_hp(in, out, out) is det.

vn_flush__get_incr_hp([], _, _) :-
	error("could not find incr_hp").
vn_flush__get_incr_hp([Instr0 | Instrs0], IncrHp, Instrs) :-
	( Instr0 = incr_hp(_, _, _, _) - _ ->
		IncrHp = Instr0,
		Instrs = Instrs0
	;
		vn_flush__get_incr_hp(Instrs0, IncrHp, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_flush__vn(vn, list(vn_src), list(lval), rval, vn_tables, vn_tables,
	templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__vn(in, in, in, out, in, out, in, out, in, out) is det.

vn_flush__vn(Vn, Srcs, Forbidden, Rval, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) :-
	( Srcs = [SrcPrime | _] ->
		Src = SrcPrime
	;
		error("empty source list in flush_vn")
	),
	vn_util__is_const_expr(Vn, IsConst, VnTables0),
	(
		IsConst = yes,
		vn_flush__vn_value(Vn, Srcs, Forbidden, Rval,
			VnTables0, VnTables3, Templocs0, Templocs, Params,
			Instrs)
	;
		IsConst = no,
		vn_table__lookup_current_locs(Vn, Locs, "vn_flush__vn",
			VnTables0),
		vn_table__lookup_uses(Vn, Uses, "vn_flush__vn", VnTables0),
		list__delete_all(Uses, Src, NewUses),
		( vn_util__choose_cheapest_loc(Locs, Loc) ->
			(
				Loc = vn_hp
			->
				% The first reference to the old value of hp.
				vn_flush__old_hp(Srcs, Forbidden, Rval,
					VnTables0, VnTables3,
					Templocs0, Templocs, Params, Instrs)
			;
				NewUses = [_,_|_],
				vn_util__classify_loc_cost(Loc, Cost),
				Cost > 0,
				\+ Src = src_liveval(_)
			->
				vn_flush__choose_temp(Vn, VnTables0,
					Templocs0, Templocs1, Vnlval),
				vn_flush__generate_assignment(Vnlval, Vn,
					Forbidden, VnTables0, VnTables3,
					Templocs1, Templocs, Params,
					Lval, Instrs),
				Rval = lval(Lval)
			;
				vn_flush__access_path(Loc, [src_vn(Vn) | Srcs],
					Forbidden, Lval, VnTables0, VnTables3,
					Templocs0, Templocs, Params, Instrs),
				Rval = lval(Lval)
			)
		;
			% If there are no more uses, it is useless to assign
			% the vn to a location. Otherwise it is useful, but if
			% Src is a livevals, the assignment will be done by
			% the caller.
			(
				NewUses = [_|_],
				\+ Src = src_liveval(_)
			->
				vn_flush__choose_loc_for_shared_vn(Vn, Vnlval,
					VnTables0, Templocs0, Templocs1),
				vn_flush__generate_assignment(Vnlval, Vn,
					Forbidden, VnTables0, VnTables3,
					Templocs1, Templocs, Params,
					Lval, Instrs),
				Rval = lval(Lval)
			;
				vn_flush__vn_value(Vn, Srcs, Forbidden, Rval,
					VnTables0, VnTables3,
					Templocs0, Templocs, Params, Instrs)
			)
		)
	),
	vn_table__del_old_use(Vn, Src, VnTables3, VnTables).

%-----------------------------------------------------------------------------%

:- pred vn_flush__vn_value(vn, list(vn_src), list(lval), rval,
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__vn_value(in, in, in, out, in, out, in, out, in, out) is det.

vn_flush__vn_value(Vn, Srcs, Forbidden, Rval, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) :-
	vn_table__lookup_defn(Vn, Vnrval, "vn_flush__vn_value", VnTables0),
	(
		Vnrval = vn_origlval(Vnlval),
		( Vnlval = vn_hp ->
			error("vn_hp found in flush_vn_value")
			% It should have been caught in flush_vn
		;
			true
		),
		vn_table__lookup_current_locs(Vn, Locs0, "vn_flush__vn_value",
			VnTables0),
		(
			% For code understandability, and for aesthetics,
			% we prefer to take the value from its original home,
			% but only if by doing so we incur no cost penalty.
			vn_table__lookup_current_value(Vnlval, CurVn,
				"vn_flush__vn_value", VnTables0),
			Vn = CurVn
		->
			Locs1 = [Vnlval | Locs0]
		;
			Locs1 = Locs0
		),
		( vn_util__choose_cheapest_loc(Locs1, LocPrime) ->
			Loc = LocPrime
		;
			opt_debug__dump_vnlval(Vnlval, V_str),
			string__append("cannot find copy of an origlval: ",
				V_str, Str),
			error(Str)
		),
		vn_flush__access_path(Loc, [src_vn(Vn) | Srcs], Forbidden, Lval,
			VnTables0, VnTables, Templocs0, Templocs, Params,
			Instrs),
		Rval = lval(Lval)
	;
		Vnrval = vn_mkword(Tag, SubVn1),
		(
			vn_table__lookup_defn(SubVn1, SubVnrval,
				"vn_flush__vn_value", VnTables0),
			SubVnrval = vn_origlval(vn_hp)
		->
			vn_flush__vn(SubVn1, [src_vn(Vn) | Srcs], Forbidden,
				Rval1, VnTables0, VnTables,
				Templocs0, Templocs, Params, Instrs),
			vn_table__lookup_current_locs(Vn, Locs,
				"vn_flush__vn_value", VnTables),
			( Locs = [Loc0 | _] ->
				% see below for an explanation
				vn_flush__access_path(Loc0, [], Forbidden, Lval,
					VnTables, _, Templocs0, _, Params, _),
				Rval = lval(Lval)
			;
				Rval = mkword(Tag, Rval1)
			)
		;
			vn_flush__vn(SubVn1, [src_vn(Vn) | Srcs], Forbidden,
				Rval1, VnTables0, VnTables,
				Templocs0, Templocs, Params, Instrs),
			Rval = mkword(Tag, Rval1)
		)
	;
		Vnrval = vn_const(Const),
		Rval = const(Const),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_create(Tag, MaybeRvals, ArgTypes, StatDyn,
			Label, Msg),
		Reuse = no,
		Rval = create(Tag, MaybeRvals, ArgTypes, StatDyn,
			Label, Msg, Reuse),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_unop(Unop, SubVn1),
		vn_flush__vn(SubVn1, [src_vn(Vn) | Srcs], Forbidden, Rval1,
			VnTables0, VnTables, Templocs0, Templocs, Params,
			Instrs),
		Rval = unop(Unop, Rval1)
	;
		Vnrval = vn_binop(Binop, SubVn1, SubVn2),
		vn_flush__vn(SubVn1, [src_vn(Vn) | Srcs], Forbidden, Rval1,
			VnTables0, VnTables1, Templocs0, Templocs1, Params,
			Instrs1),
		vn_flush__vn(SubVn2, [src_vn(Vn) | Srcs], Forbidden, Rval2,
			VnTables1, VnTables, Templocs1, Templocs, Params,
			Instrs2),
		Rval = binop(Binop, Rval1, Rval2),
		list__append(Instrs1, Instrs2, Instrs)
	;
		Vnrval = vn_stackvar_addr(N),
		Rval = mem_addr(stackvar_ref(N)),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_framevar_addr(N),
		Rval = mem_addr(framevar_ref(N)),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_heap_addr(SubVn1, Tag, Field),
		vn_flush__vn(SubVn1, [src_vn(Vn) | Srcs], Forbidden, Rval1,
			VnTables0, VnTables, Templocs0, Templocs, Params,
			Instrs),
		Rval = mem_addr(heap_ref(Rval1, Tag, Field))
	).

%-----------------------------------------------------------------------------%

:- pred vn_flush__old_hp(list(vn_src), list(lval), rval, vn_tables, vn_tables,
	templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__old_hp(in, in, out, in, out, in, out, in, out) is det.

vn_flush__old_hp(Srcs0, Forbidden0, ReturnRval, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) :-
	% First take care of the "assignment to hp" part of incr_hp.
	vn_table__lookup_desired_value(vn_hp, NewhpVn, "vn_flush__old_hp",
		VnTables0),
	vn_flush__hp_incr(NewhpVn, Srcs0, Forbidden0, MaybeRval,
		VnTables0, VnTables1, Templocs0, Templocs1, Params, IncrInstrs),
	(
		MaybeRval = yes(Rval0),
		vn_type__bytes_per_word(Params, BytesPerWord),
		( Rval0 = const(int_const(I)) ->
			I1 is I // BytesPerWord,
			Rval = const(int_const(I1))
		; Rval0 = binop((*), Rval1, const(int_const(BytesPerWord))) ->
			Rval = Rval1
		;
			Rval = binop((/), Rval0, const(int_const(BytesPerWord)))
		)
	;
		MaybeRval = no,
		error("empty expression for hp increment")
	),
	vn_table__set_current_value(vn_hp, NewhpVn, VnTables1, VnTables2),

	% Find out whether we should tag the old hp, and if so, with what.
	vn_table__lookup_assigned_vn(vn_origlval(vn_hp), OldhpVn,
		"vn_flush__old_hp", VnTables2),
	( Srcs0 = [Src0Prime | Srcs1Prime] ->
		Src0 = Src0Prime,
		Srcs1 = Srcs1Prime
	;
		error("empty src list in vn_flush__old_hp")
	),
	vn_table__del_old_use(OldhpVn, Src0, VnTables2, VnTables3),
	vn_table__lookup_uses(OldhpVn, OldhpUses, "vn_flush__old_hp",
		VnTables3),
	(
		OldhpUses = [],
		Src0 = src_vn(UserVn),
		vn_table__lookup_defn(UserVn, UserVnrval, "vn_flush__old_hp",
			VnTables3),
		UserVnrval = vn_mkword(Tag, OldhpVn)
	->
		MaybeTag = yes(Tag),
		AssignedVn = UserVn,

		% Find out where to put the tagged value.
		(
			Srcs1 = [src_liveval(VnlvalPrime) | _]
		->
			Vnlval = VnlvalPrime,
			% This call is purely to convert Vnlval in Lval.
			% Since this flush will already have been done in
			% generate_assign, we give it a bogus Srcs input and
			% ignore all its other outputs.
			vn_flush__access_path(Vnlval, [], Forbidden0, Lval,
				VnTables3, _, Templocs1, _, Params, _),
			Templocs2 = Templocs1
		; 
			vn_flush__find_cheap_users(UserVn, UserLocs, VnTables3),
			vn_util__choose_cheapest_loc(UserLocs, UserLoc),
			UserLoc = vn_reg(_, _)
		->
			Vnlval = UserLoc,
			vn_util__no_access_vnlval_to_lval(Vnlval, MaybeLval),
			(
				MaybeLval = yes(Lval)
			;
				MaybeLval = no,
				error("register needs access path")
			),
			Templocs2 = Templocs1
		;
			vn_temploc__next_tempr(Templocs1, Templocs2, Vnlval),
			vn_util__no_access_vnlval_to_lval(Vnlval, MaybeLval),
			(
				MaybeLval = yes(Lval)
			;
				MaybeLval = no,
				error("temploc needs access path")
			)
		),
		ReturnRval = const(int_const(42))	% should not be used
	;
		MaybeTag = no,
		AssignedVn = OldhpVn,
		vn_temploc__next_tempr(Templocs1, Templocs2, Vnlval),
		vn_util__no_access_vnlval_to_lval(Vnlval, MaybeLval),
		(
			MaybeLval = yes(Lval)
		;
			MaybeLval = no,
			error("temploc needs access path")
		),
		ReturnRval = lval(Lval)
	),

	% Save the old value if necessary.
	( vn_table__search_current_value(Vnlval, OldVn, VnTables3) ->
		vn_util__find_lvals_in_rval(Rval, Forbidden1),
		list__append(Forbidden0, Forbidden1, Forbidden),
		vn_flush__maybe_save_prev_value(Vnlval, OldVn, AssignedVn,
			Forbidden, VnTables3, VnTables4, Templocs2, Templocs,
			Params, SaveInstrs)
	;
		VnTables4 = VnTables2,
		Templocs = Templocs2,
		SaveInstrs = []
	),

	vn_table__set_current_value(Vnlval, AssignedVn, VnTables4, VnTables),
	Instr = incr_hp(Lval, MaybeTag, Rval, "origin_lost_in_value_number")
		- "",
	list__condense([IncrInstrs, SaveInstrs, [Instr]], Instrs).

%-----------------------------------------------------------------------------%

:- pred vn_flush__hp_incr(vn, list(vn_src), list(lval), maybe(rval),
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__hp_incr(in, in, in, out, in, out, in, out, in, out) is det.

vn_flush__hp_incr(Vn, Srcs, Forbidden, MaybeRval, VnTables0, VnTables,
		Templocs0, Templocs, Params, Instrs) :-
	(
		vn_flush__rec_find_ref_vns(Vn, SubVns, VnTables0),
		vn_flush__free_of_old_hp(SubVns, VnTables0)
	->
		vn_flush__vn(Vn, Srcs, Forbidden, Rval, VnTables0, VnTables,
			Templocs0, Templocs, Params, Instrs),
		MaybeRval = yes(Rval)
	;
		vn_table__lookup_defn(Vn, Vnrval, "vn_flush__hp_incr",
			VnTables0),
		(
			Vnrval = vn_origlval(Vnlval),
			( Vnlval = vn_hp ->
				MaybeRval = no
			;
				error("non-hp origlval in flush_hp_incr")
			),
			VnTables2 = VnTables0,
			Templocs = Templocs0,
			Instrs = []
		;
			Vnrval = vn_mkword(_, _),
			error("mkword in calculation of new hp")
		;
			Vnrval = vn_const(Const),
			( Const = int_const(_) ->
				MaybeRval = yes(const(Const))
			;
				error("non-int const in flush_hp_incr")
			),
			VnTables2 = VnTables0,
			Templocs = Templocs0,
			Instrs = []
		;
			Vnrval = vn_create(_, _, _, _, _, _),
			error("create in calculation of new hp")
		;
			Vnrval = vn_unop(_, _),
			error("unop in calculation of new hp")
		;
			Vnrval = vn_binop(Op, SubVn1, SubVn2),
			vn_flush__hp_incr(SubVn1, [src_vn(Vn) | Srcs],
				Forbidden, MaybeRval1, VnTables0, VnTables1,
				Templocs0, Templocs1, Params, Instrs1),
			vn_flush__hp_incr(SubVn2, [src_vn(Vn) | Srcs],
				Forbidden, MaybeRval2, VnTables1, VnTables2,
				Templocs1, Templocs, Params, Instrs2),
			list__append(Instrs1, Instrs2, Instrs),
			(
				MaybeRval1 = yes(Rval1),
				MaybeRval2 = yes(Rval2),
				MaybeRval = yes(binop(Op, Rval1, Rval2))
			;
				MaybeRval1 = yes(_Rval1),
				MaybeRval2 = no,
				( Op = (+) ->
					MaybeRval = MaybeRval1
				;
					error("non-+ op on hp")
				)
			;
				MaybeRval1 = no,
				MaybeRval2 = yes(_Rval2),
				( Op = (+) ->
					MaybeRval = MaybeRval2
				;
					error("non-+ op on hp")
				)
			;
				MaybeRval1 = no,
				MaybeRval2 = no,
				error("two 'no's in flush_hp_incr")
			)
		;
			Vnrval = vn_stackvar_addr(_),
			error("stackvar_addr in calculation of new hp")
		;
			Vnrval = vn_framevar_addr(_),
			error("framevar_addr in calculation of new hp")
		;
			Vnrval = vn_heap_addr(_, _, _),
			error("heap_addr in calculation of new hp")
		),
		( Srcs = [SrcPrime | _] ->
			Src = SrcPrime
		;
			error("empty source list in flush_vn")
		),
		vn_table__del_old_use(Vn, Src, VnTables2, VnTables)
	).

%-----------------------------------------------------------------------------%

:- pred vn_flush__free_of_old_hp(list(vn), vn_tables).
:- mode vn_flush__free_of_old_hp(in, in) is semidet.

vn_flush__free_of_old_hp([], _VnTables).
vn_flush__free_of_old_hp([Vn | Vns], VnTables) :-
	vn_table__lookup_defn(Vn, Vnrval, "vn_flush__free_of_old_hp", VnTables),
	\+ Vnrval = vn_origlval(vn_hp),
	vn_flush__free_of_old_hp(Vns, VnTables).

:- pred vn_flush__rec_find_ref_vns(vn, list(vn), vn_tables).
:- mode vn_flush__rec_find_ref_vns(in, out, in) is det.

vn_flush__rec_find_ref_vns(Vn, [Vn | DeepVns], VnTables) :-
	vn_table__lookup_defn(Vn, Vnrval, "vn_flush__rec_find_ref_vns",
		VnTables),
	vn_util__find_sub_vns(Vnrval, ImmedVns),
	vn_flush__rec_find_ref_vns_list(ImmedVns, DeepVns, VnTables).

:- pred vn_flush__rec_find_ref_vns_list(list(vn), list(vn), vn_tables).
:- mode vn_flush__rec_find_ref_vns_list(in, out, in) is det.

vn_flush__rec_find_ref_vns_list([], [], _VnTables).
vn_flush__rec_find_ref_vns_list([Vn | Vns], SubVns, VnTables) :-
	vn_flush__rec_find_ref_vns(Vn, SubVns0, VnTables),
	vn_flush__rec_find_ref_vns_list(Vns, SubVns1, VnTables),
	list__append(SubVns0, SubVns1, SubVns).

%-----------------------------------------------------------------------------%

:- pred vn_flush__access_path(vnlval, list(vn_src), list(lval), lval,
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__access_path(in, in, in, out, in, out, in, out, in, out)
	is det.

vn_flush__access_path(Vnlval, Srcs, Forbidden, Lval, VnTables0, VnTables,
		Templocs0, Templocs, Params, AccessInstrs) :-
	(
		Vnlval = vn_reg(Type, Num),
		Lval = reg(Type, Num),
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_temp(Type, Num),
		Lval = temp(Type, Num),
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_stackvar(Slot),
		Lval = stackvar(Slot),
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_framevar(Slot),
		Lval = framevar(Slot),
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_succip,
		Lval = succip,
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_maxfr,
		Lval = maxfr,
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_curfr,
		Lval = curfr,
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_succfr(Vn1),
		vn_flush__vn(Vn1, [src_access(Vnlval) | Srcs], Forbidden, Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, Params, AccessInstrs),
		Lval = succfr(Rval)
	;
		Vnlval = vn_prevfr(Vn1),
		vn_flush__vn(Vn1, [src_access(Vnlval) | Srcs], Forbidden, Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, Params, AccessInstrs),
		Lval = prevfr(Rval)
	;
		Vnlval = vn_redofr(Vn1),
		vn_flush__vn(Vn1, [src_access(Vnlval) | Srcs], Forbidden, Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, Params, AccessInstrs),
		Lval = redofr(Rval)
	;
		Vnlval = vn_redoip(Vn1),
		vn_flush__vn(Vn1, [src_access(Vnlval) | Srcs], Forbidden, Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, Params, AccessInstrs),
		Lval = redoip(Rval)
	;
		Vnlval = vn_succip(Vn1),
		vn_flush__vn(Vn1, [src_access(Vnlval) | Srcs], Forbidden, Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, Params, AccessInstrs),
		Lval = succip(Rval)
	;
		Vnlval = vn_hp,
		Lval = hp,
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_sp,
		Lval = sp,
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_field(Tag, Vn1, Vn2),
		vn_flush__vn(Vn1, [src_access(Vnlval) | Srcs], Forbidden, Rval1,
			VnTables0, VnTables1,
			Templocs0, Templocs1, Params, AccessInstrs1),
		vn_flush__vn(Vn2, [src_access(Vnlval) | Srcs], Forbidden, Rval2,
			VnTables1, VnTables,
			Templocs1, Templocs, Params, AccessInstrs2),
		Lval = field(Tag, Rval1, Rval2),
		list__append(AccessInstrs1, AccessInstrs2, AccessInstrs)
	;
		Vnlval = vn_mem_ref(Vn),
		vn_flush__vn(Vn, [src_access(Vnlval) | Srcs], Forbidden, Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, Params, AccessInstrs),
		Lval = mem_ref(Rval)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% If the vn currently stored in the vnlval is used elsewhere,
	% and if it cannot be recreated blind (or at least not cheaply),
	% then save the value somewhere else. We prefer the somewhere else
	% to be a location where we have to store the value anyway.
	% However, we must not choose a location that is used in the expression
	% being assigned to the vnlval.

	% If we are overwriting a temporary location, it may not have
	% a current value entry in the vn tables.

	% We cannot look up the old contents of the Vnlval here, because
	% it may have been already overwritten in flush_old_hp.

:- pred vn_flush__maybe_save_prev_value(vnlval, vn, vn, list(lval),
	vn_tables, vn_tables, templocs, templocs, vn_params, list(instruction)).
:- mode vn_flush__maybe_save_prev_value(in, in, in, in, in, out, in, out,
	in, out) is det.

vn_flush__maybe_save_prev_value(Vnlval, OldVn, NewVn, Forbidden,
		VnTables0, VnTables, Templocs0, Templocs, Params, Instrs) :-
	(
		vn_table__set_current_value(Vnlval, NewVn, VnTables0,
			VnTablesProbe),
		vn_table__search_uses(OldVn, Uses, VnTablesProbe),
		vn_util__real_uses(Uses, RealUses, VnTablesProbe),
		\+ RealUses = [],
		vn_util__is_const_expr(OldVn, no, VnTables0),
		vn_table__lookup_current_locs(OldVn, Locs0,
			"vn_flush__maybe_save_prev_value", VnTables0),
		list__delete_all(Locs0, Vnlval, Locs),
		vn_flush__no_good_copies(Locs)
	->
		(
			vn_flush__find_cheap_users(OldVn, ReqLocs, VnTables0),
			vn_util__choose_cheapest_loc(ReqLocs, Presumed)
		->
			vn_util__no_access_vnlval_to_lval(Presumed,
				MaybePresumed),
			(
				MaybePresumed = yes(PresumedLval),
				(
					list__member(PresumedLval, Forbidden)
				->
					vn_flush__choose_temp(OldVn,
						VnTables0, Templocs0,
						Templocs1, Chosen)
				;
					RealUses = [_,_|_],
					\+ Presumed = vn_reg(_, _)
				->
					vn_flush__choose_temp(OldVn,
						VnTables0, Templocs0,
						Templocs1, Chosen)
				;
					Chosen = Presumed,
					Templocs1 = Templocs0
				)
			;
				MaybePresumed = no,
				% we cannot use Presumed even if it is not
				% in Forbidden
				vn_flush__choose_temp(OldVn, VnTables0,
					Templocs0, Templocs1, Chosen)
			)
		;
			vn_flush__choose_temp(OldVn, VnTables0,
				Templocs0, Templocs1, Chosen)
		),
		vn_flush__ensure_assignment(Chosen, OldVn, Forbidden,
			VnTables0, VnTables, Templocs1, Templocs, Params,
			Instrs1),
		opt_debug__dump_uses_list(RealUses, Debug),
		Instrs = [comment(Debug) - "" | Instrs1]
	;
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	).

:- pred vn_flush__no_good_copies(list(vnlval)).
:- mode vn_flush__no_good_copies(in) is semidet.

vn_flush__no_good_copies([]).
vn_flush__no_good_copies([Vnlval | Vnlvals]) :-
	Vnlval = vn_field(_, _, _),
	vn_flush__no_good_copies(Vnlvals).
