%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_order.m - find and order the nodes of the vn_table.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_order.

:- interface.

:- import_module llds, vn_type, vn_table.
:- import_module bool, list.

:- type order_result	--->	success(vn_tables, list(vn_node))
			;	failure(maybe(label)).

:- pred vn_order__order(vnlvalset, vn_tables, bool, int, ctrlmap, flushmap,
	order_result, io__state, io__state).
:- mode vn_order__order(in, in, in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module map, set, string, int, require, std_util, assoc_list.

:- import_module atsort, vn_util, vn_debug, opt_util.

%-----------------------------------------------------------------------------%

vn_order__order(Liveset, VnTables0, SeenIncr, Ctrl, Ctrlmap, Flushmap, Result)
		-->
	vn_debug__order_start_msg(Ctrlmap, Flushmap, VnTables0),
	{ vn_order__req_order(Ctrlmap, Flushmap, SeenIncr, VnTables0,
		Problem, MustSuccmap0, MustPredmap0) },
	(
		{ Problem = no },
		{ set__to_sorted_list(Liveset, Livelist) },
		{ vn_order__prod_cons_order(Livelist, VnTables0, VnTables1,
			MustSuccmap0, MustSuccmap1,
			MustPredmap0, MustPredmap1) },
		{ vn_order__vn_ctrl_order(0, Ctrlmap, VnTables1, VnTables2,
			MustSuccmap1, Succmap1,
			MustPredmap1, Predmap1) },
		vn_order__use_before_redef(VnTables2, Liveset,
			Succmap1, Succmap2, Predmap1, Predmap2),
		vn_order__ctrl_vn_order(0, Flushmap,
			MustSuccmap1, MustSuccmap2,
			MustPredmap1, MustPredmap2, Links),
		{ vn_order__add_computed_links(Links,
			Succmap2, Succmap3,
			Predmap2, Predmap3) },

		vn_debug__order_map_msg(MustSuccmap2, MustPredmap2,
			Succmap3, Predmap3),

		{ vn_order__pref_order(Succmap3, PrefOrder) },
		{ atsort(Succmap3, Predmap3, MustSuccmap2, MustPredmap2,
			PrefOrder, Blocks) },
		{ LastCtrl is Ctrl - 1 },
		vn_order__blocks_to_order(Blocks, LastCtrl, VnTables2, Order0),
		{ vn_order__reorder_noops(Order0, VnTables2, Order) },

		vn_debug__order_order_msg(Order),
		{ Result = success(VnTables2, Order) }
	;
		{ Problem = yes(MaybeLabel) },
		{ Result = failure(MaybeLabel) }
	).

%-----------------------------------------------------------------------------%

	% Given the information gathered on the control nodes, check whether
	% any side branch requires a value to be stored in a vnlval other than
	% the final one. If yes, we can't apply (this version of) value
	% numbering. If no, we can apply it, and we compute the compulsory
	% part of the order of nodes, which is that a vnlval that is live
	% at a side branch must be produced before that side branch.

:- pred vn_order__req_order(ctrlmap, flushmap, bool, vn_tables,
	maybe(maybe(label)), relmap(vn_node), relmap(vn_node)).
:- mode vn_order__req_order(in, in, in, in, out, out, out) is det.

vn_order__req_order(Ctrlmap, Flushmap, SeenIncr, VnTables, Problem,
		MustSuccmap, MustPredmap) :-
	map__init(MustSuccmap0),
	map__init(MustPredmap0),
	vn_order__req_order_2(0, Ctrlmap, Flushmap, SeenIncr, VnTables, no,
		Problem, MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap).

:- pred vn_order__req_order_2(int, ctrlmap, flushmap, bool, vn_tables,
	maybe(label), maybe(maybe(label)),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn_order__req_order_2(in, in, in, in, in, in, out, in, out, in, out)
	is det.

vn_order__req_order_2(Ctrl, Ctrlmap, Flushmap, Heapop0, VnTables,
		LastLabel, Problem, MustSuccmap0, MustSuccmap,
		MustPredmap0, MustPredmap) :-
	( map__search(Ctrlmap, Ctrl, VnInstr) ->
		( VnInstr = vn_mark_hp(_) ->
			Heapop1 = yes
		; VnInstr = vn_restore_hp(_) ->
			Heapop1 = yes
		;
			Heapop1 = no
		),
		(
			Heapop0 = yes,
			Heapop1 = yes
		->
			MustSuccmap = MustSuccmap0,
			MustPredmap = MustPredmap0,
			Problem = yes(LastLabel)
		;
			( VnInstr = vn_label(Label) ->
				CurLabel = yes(Label)
			;
				CurLabel = LastLabel
			),
			bool__or(Heapop0, Heapop1, Heapop2),
			map__lookup(Flushmap, Ctrl, FlushEntry),
			( Ctrl > 0 ->
				PrevCtrl is Ctrl - 1,
				vn_order__add_link(node_ctrl(PrevCtrl),
					node_ctrl(Ctrl),
					MustSuccmap0, MustSuccmap1,
					MustPredmap0, MustPredmap1)
			;
				vn_order__add_node(node_ctrl(Ctrl),
					MustSuccmap0, MustSuccmap1,
					MustPredmap0, MustPredmap1)
			),
			(
				map__to_assoc_list(FlushEntry, FlushList),
				vn_order__record_ctrl_deps(FlushList,
					node_ctrl(Ctrl),
					VnTables, MustSuccmap1, MustSuccmap2,
					MustPredmap1, MustPredmap2)
			->
				NextCtrl is Ctrl + 1,
				vn_order__req_order_2(NextCtrl, Ctrlmap,
					Flushmap, Heapop2, VnTables,
					CurLabel, Problem,
					MustSuccmap2, MustSuccmap,
					MustPredmap2, MustPredmap)
			;
				MustSuccmap = MustSuccmap1,
				MustPredmap = MustPredmap1,
				Problem = yes(no)
			)
		)
	;
		MustSuccmap = MustSuccmap0,
		MustPredmap = MustPredmap0,
		Problem = no
	).

:- pred vn_order__record_ctrl_deps(assoc_list(vnlval, vn), vn_node, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__record_ctrl_deps(in, in, in, di, uo, di, uo) is semidet.
:- mode vn_order__record_ctrl_deps(in, in, in, in, out, in, out) is semidet.

vn_order__record_ctrl_deps([], _Sink, _VnTables,
		MustSuccmap, MustSuccmap, MustPredmap, MustPredmap).
vn_order__record_ctrl_deps([Vnlval - Vn | FlushList], Sink, VnTables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap) :-
	vn_table__lookup_desired_value(Vnlval, Des_vn,
		"vn_order__record_ctrl_deps", VnTables),
	Vn = Des_vn,
	vn_order__add_link(node_lval(Vnlval), Sink,
		MustSuccmap0, MustSuccmap1, MustPredmap0, MustPredmap1),
	vn_order__record_ctrl_deps(FlushList, Sink, VnTables,
		MustSuccmap1, MustSuccmap, MustPredmap1, MustPredmap).

%-----------------------------------------------------------------------------%

	% Record the natural producer-consumer relationships
	% induced by the live vnlvals.

:- pred vn_order__prod_cons_order(list(vnlval), vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__prod_cons_order(in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__prod_cons_order(in, in, out, in, out, in, out) is det.

vn_order__prod_cons_order([], VnTables, VnTables,
		Succmap, Succmap, Predmap, Predmap).
vn_order__prod_cons_order([Vnlval | Vnlvals], VnTables0, VnTables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn_table__lookup_desired_value(Vnlval, NewVn,
		"vn_order__prod_cons_order", VnTables0),
	vn_table__lookup_current_value(Vnlval, OldVn,
		"vn_order__prod_cons_order", VnTables0),
	( OldVn = NewVn ->
		VnTables1 = VnTables0,
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	;
		vn_order__find_links(NewVn, node_lval(Vnlval), VnTables0,
			VnTables1, Succmap0, Succmap1, Predmap0, Predmap1)
	),
	vn_order__prod_cons_order(Vnlvals, VnTables1, VnTables,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the requirement of not producing any lvals before a control
	% node except those we absolutely have to, since any such production
	% will be wasted on one control path, and may cause dereferencing of
	% a word whose tag has not yet been checked, which will in general
	% lead to an unaligned memory reference and a core dump.

:- pred vn_order__ctrl_vn_order(int, flushmap,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	list(pair(vn_node)), io__state, io__state).
% :- mode vn_order__ctrl_vn_order(in, in, di, uo, di, uo, out, di, uo) is det.
:- mode vn_order__ctrl_vn_order(in, in, in, out, in, out, out, di, uo) is det.

vn_order__ctrl_vn_order(Ctrl, Flushmap, Succmap0, Succmap, Predmap0, Predmap,
		Links) -->
	{ map__keys(Succmap0, AllNodes) },
	vn_order__ctrl_vn_order_2(Ctrl, Flushmap, AllNodes, [],
		Succmap0, Succmap, Predmap0, Predmap, [], Links).

:- pred vn_order__ctrl_vn_order_2(int, flushmap, list(vn_node), list(vn_node),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	list(pair(vn_node)), list(pair(vn_node)), io__state, io__state).
:- mode vn_order__ctrl_vn_order_2(in, in, in, in, in, out, in, out, in, out,
	di, uo) is det.

vn_order__ctrl_vn_order_2(Ctrl, Flushmap, AllNodes, Prednodes0,
		Succmap0, Succmap, Predmap0, Predmap, Links0, Links) -->
	( { map__search(Flushmap, Ctrl, FlushEntry) } ->
		{ map__keys(FlushEntry, FlushVnlvals) },
		{ vn_order__append_vnlval_nodes(FlushVnlvals, Prednodes0,
			Prednodes1) },
		{ atsort__closure(Prednodes1, Predmap0, Prednodes2) },
		vn_order__record_antideps(AllNodes, Prednodes2, node_ctrl(Ctrl),
			Succmap0, Succmap1, Predmap0, Predmap1, Links0, Links1),
		{ NextCtrl is Ctrl + 1 },
		vn_order__ctrl_vn_order_2(NextCtrl, Flushmap, AllNodes,
			Prednodes2, Succmap1, Succmap, Predmap1, Predmap,
			Links1, Links)
	;
		{ Succmap = Succmap0 },
		{ Predmap = Predmap0 },
		{ Links = Links0 }
	).

:- pred vn_order__append_vnlval_nodes(list(vnlval), list(vn_node),
	list(vn_node)).
% :- mode vn_order__append_vnlval_nodes(in, di, uo) is det.
:- mode vn_order__append_vnlval_nodes(in, in, out) is det.

vn_order__append_vnlval_nodes([], Prednodes, Prednodes).
vn_order__append_vnlval_nodes([Vnlval | Vnlvals], Prednodes0, Prednodes) :-
	Prednodes1 = [node_lval(Vnlval) | Prednodes0],
	vn_order__append_vnlval_nodes(Vnlvals, Prednodes1, Prednodes).

:- pred vn_order__record_antideps(list(vn_node), list(vn_node), vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	list(pair(vn_node)), list(pair(vn_node)), io__state, io__state).
:- mode vn_order__record_antideps(in, in, in, in, out, in, out, in, out, di, uo)
	is det.

vn_order__record_antideps([], _, _, Succmap, Succmap, Predmap, Predmap,
		Links, Links) -->
	[].
vn_order__record_antideps([Node | Nodes], Prednodes, CtrlNode,
		Succmap0, Succmap, Predmap0, Predmap, Links0, Links) -->
	(
		{ Node = node_lval(_) ; Node = node_shared(_) },
		{ \+ list__member(Node, Prednodes) }
	->
		vn_debug__order_antidep_msg(CtrlNode, Node),
		{ vn_order__add_link(CtrlNode, Node,
			Succmap0, Succmap1, Predmap0, Predmap1) },
		{ Links1 = [CtrlNode - Node | Links0] }
	;
		{ Succmap1 = Succmap0 },
		{ Predmap1 = Predmap0 },
		{ Links1 = Links0 }
	),
	vn_order__record_antideps(Nodes, Prednodes, CtrlNode,
		Succmap1, Succmap, Predmap1, Predmap, Links1, Links).

:- pred vn_order__add_computed_links(list(pair(vn_node)),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__add_computed_links(in, di, uo, di, uo) is det.
:- mode vn_order__add_computed_links(in, in, out, in, out) is det.

vn_order__add_computed_links([], Succmap, Succmap, Predmap, Predmap).
vn_order__add_computed_links([From - To | Links],
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn_order__add_link(From, To, Succmap0, Succmap1, Predmap0, Predmap1),
	vn_order__add_computed_links(Links, Succmap1, Succmap,
		Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the desirability of producing the vnlvals involved in the
	% expressions inside control nodes before producing the control nodes
	% themselves. This is stated as a suggestion, not a requirement, since
	% it can be overridden during the flushing process.

:- pred vn_order__vn_ctrl_order(int, ctrlmap, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__vn_ctrl_order(in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__vn_ctrl_order(in, in, in, out, in, out, in, out) is det.

vn_order__vn_ctrl_order(Ctrl, Ctrlmap, VnTables0, VnTables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Ctrlmap, Ctrl, Vn_instr) ->
		(
			Vn_instr = vn_livevals(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_call(_, _, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_mkframe(_, _, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_label(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_goto(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_computed_goto(Vn, _),
			vn_order__find_links(Vn, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_if_val(Vn, _),
			vn_order__find_links(Vn, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_mark_hp(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Vns),
			vn_order__find_all_links(Vns, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_restore_hp(Vn),
			vn_order__find_links(Vn, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_store_ticket(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Vns),
			vn_order__find_all_links(Vns, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_reset_ticket(Vn, _Reason),
			vn_order__find_links(Vn, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_discard_ticket,
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_mark_ticket_stack(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Vns),
			vn_order__find_all_links(Vns, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_discard_tickets_to(Vn),
			vn_order__find_links(Vn, node_ctrl(Ctrl),
				VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_incr_sp(_, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		;
			Vn_instr = vn_decr_sp(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			VnTables1 = VnTables0
		),
		NextCtrl is Ctrl + 1,
		vn_order__vn_ctrl_order(NextCtrl, Ctrlmap, VnTables1, VnTables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		Succmap = Succmap0,
		Predmap = Predmap0,
		VnTables = VnTables0
	).

%-----------------------------------------------------------------------------%

	% Try to make sure that all immediate users of the original value
	% of a vnlval are done before the vnlval is redefined. This avoids
	% an instruction to save the original value somewhere else.

:- pred vn_order__use_before_redef(vn_tables, vnlvalset,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	io__state, io__state).
% :- mode vn_order__use_before_redef(in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__use_before_redef(in, in, in, out, in, out, di, uo) is det.

vn_order__use_before_redef(VnTables, Liveset,
		Succmap0, Succmap, Predmap0, Predmap) -->
	{ map__keys(Predmap0, Sinks) },
	vn_order__use_sinks_before_redef(Sinks, VnTables, Liveset,
		Succmap0, Succmap, Predmap0, Predmap).

:- pred vn_order__use_sinks_before_redef(list(vn_node), vn_tables, vnlvalset,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	io__state, io__state).
:- mode vn_order__use_sinks_before_redef(in, in, in, in, out, in, out, di, uo)
	is det.

vn_order__use_sinks_before_redef([], _, _, Succmap, Succmap, Predmap, Predmap)
		-->
	[].
vn_order__use_sinks_before_redef([Sink | Sinks], VnTables, Liveset,
		Succmap0, Succmap, Predmap0, Predmap) -->
	vn_order__use_sink_before_redef(Sink, VnTables, Liveset,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn_order__use_sinks_before_redef(Sinks, VnTables, Liveset,
		Succmap1, Succmap, Predmap1, Predmap).

:- pred vn_order__use_sink_before_redef(vn_node, vn_tables, vnlvalset,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	io__state, io__state).
:- mode vn_order__use_sink_before_redef(in, in, in, in, out, in, out, di, uo)
	is det.

vn_order__use_sink_before_redef(Sink, VnTables, Liveset,
		Succmap0, Succmap, Predmap0, Predmap) -->
	vn_debug__order_sink_msg(Sink),
	(
		{ Sink = node_lval(Vnlval) },
		{ vn_table__search_current_value(Vnlval, OldVn, VnTables) },
		{ vn_table__search_desired_value(Vnlval, NewVn, VnTables) },
		{ OldVn \= NewVn }
	->
		{ map__search(Succmap0, node_origlval(Vnlval), Users1Prime) ->
			Users1 = Users1Prime
		;
			Users1 = []
		},
		{ vn_table__lookup_uses(OldVn, Uses,
			"vn_order__use_sink_before_redef", VnTables) },
		{ vn_order__find_access_users(Uses, VnTables, Succmap0,
			Users2) },
		{ list__append(Users1, Users2, Users) },

		vn_order__add_users(Users, Sink, Vnlval, VnTables, Liveset,
			Succmap0, Succmap1, Predmap0, Predmap1),
		( { map__search(Succmap1, node_shared(NewVn), _) } ->
			vn_order__add_users(Users, node_shared(NewVn), Vnlval,
				VnTables, Liveset,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			{ Succmap = Succmap1 },
			{ Predmap = Predmap1 }
		)
	;
		{ Succmap = Succmap0 },
		{ Predmap = Predmap0 }
	).

:- pred vn_order__find_access_users(list(vn_src), vn_tables, relmap(vn_node),
	list(vn_node)).
:- mode vn_order__find_access_users(in, in, in, out) is det.

vn_order__find_access_users([], _VnTables, _Succmap, []).
vn_order__find_access_users([Src | Srcs], VnTables, Succmap, Users) :-
	vn_order__find_access_users(Srcs, VnTables, Succmap, Users1),
	(
		Src = src_access(Vnlval),
		Vnlval = vn_field(_, _, _)
	->
		(
			map__search(Succmap, node_origlval(Vnlval), Users2Prime)
		->
			Users2 = Users2Prime
		;
			Users2 = []
		),
		% opt_debug__write(" and "),
		(
			vn_table__lookup_current_value(Vnlval, OldVn,
				"vn_order__find_access_users", VnTables),
			vn_table__lookup_desired_value(Vnlval, NewVn,
				"vn_order__find_access_users", VnTables),
			NewVn \= OldVn,
			map__search(Succmap, node_lval(Vnlval), Users3Prime)
		->
			Users3 = Users3Prime
		;
			Users3 = []
		),
		list__condense([Users1, Users2, Users3], Users)
	;
		Users = Users1
	).

:- pred vn_order__add_users(list(vn_node), vn_node, vnlval, vn_tables,
	vnlvalset, relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node), io__state, io__state).
:- mode vn_order__add_users(in, in, in, in, in, in, out, in, out, di, uo)
	is det.

vn_order__add_users([], _, _, _, _, Succmap, Succmap, Predmap, Predmap) --> [].
vn_order__add_users([User | Users], Sink, Vnlval, VnTables, Liveset,
		Succmap0, Succmap, Predmap0, Predmap) -->
	vn_order__add_user(User, Sink, Vnlval, VnTables, Liveset,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn_order__add_users(Users, Sink, Vnlval, VnTables, Liveset,
		Succmap1, Succmap, Predmap1, Predmap).

:- pred vn_order__add_user(vn_node, vn_node, vnlval, vn_tables, vnlvalset,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	io__state, io__state).
% :- mode vn_order__add_user(in, in, in, in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__add_user(in, in, in, in, in, in, out, in, out, di, uo) is det.

vn_order__add_user(User, Sink, Vnlval, VnTables, Liveset,
		Succmap0, Succmap, Predmap0, Predmap) -->
	(
		{
			User = node_lval(Vnlval)
		;
			User = node_shared(UserVn),
			vn_table__lookup_desired_value(Vnlval, UserVn,
				"vn_order__add_user", VnTables)
		}
	->
		{ Succmap = Succmap0 },
		{ Predmap = Predmap0 }
	;
		vn_debug__order_link_msg(User, Sink, yes),
		{ vn_order__add_link(User, Sink,
			Succmap0, Succmap1, Predmap0, Predmap1) },
		(
			{ User = node_shared(Vn) }
		->
			{ vn_table__get_vnlval_vn_list(VnTables, VnlvalVns) }, 
			vn_order__add_aliases(VnlvalVns, Vn, Sink, Liveset,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			{ Succmap = Succmap1 },
			{ Predmap = Predmap1 }
		)
	).

:- pred vn_order__add_aliases(assoc_list(vnlval, vn), vn, vn_node, vnlvalset,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node),
	io__state, io__state).
% :- mode vn_order__add_aliases(in, in, in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__add_aliases(in, in, in, in, in, out, in, out, di, uo) is det.

vn_order__add_aliases([], _, _, _, Succmap, Succmap, Predmap, Predmap) --> [].
vn_order__add_aliases([Pair | Pairs], Vn, Sink, Liveset,
		Succmap0, Succmap, Predmap0, Predmap) -->
	{ Pair = PairVnlval - PairVn },
	(
		{ PairVn = Vn },
		{ set__member(PairVnlval, Liveset) }
	->
		vn_debug__order_link_msg(node_lval(PairVnlval), Sink, no),
		{ vn_order__add_link(node_lval(PairVnlval), Sink,
			Succmap0, Succmap1, Predmap0, Predmap1) }
	;
		{ Succmap1 = Succmap0 },
		{ Predmap1 = Predmap0 }
	),
	vn_order__add_aliases(Pairs, Vn, Sink, Liveset,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the dependency of the nodes inside the given vn
	% on the given node.

:- pred vn_order__find_links(vn, vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__find_links(in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__find_links(in, in, in, out, in, out, in, out) is det.

vn_order__find_links(Vn, Sink, VnTables0, VnTables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn_table__lookup_uses(Vn, Uses0, "vn_order__find_links", VnTables0),
	vn_table__lookup_defn(Vn, Vnrval, "vn_order__find_links", VnTables0),
	(
		vn_util__is_vn_shared(Vn, Vnrval, Uses0, VnTables0),
		\+ Sink = node_shared(_)
	->
		vn_order__add_link(node_shared(Vn), Sink,
			Succmap0, Succmap1, Predmap0, Predmap1),
		vn_order__find_links(Vn, node_shared(Vn), VnTables0, VnTables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		(
			Vnrval = vn_origlval(Vnlval),
			vn_util__vnlval_access_vns(Vnlval, Access_vns),
			vn_order__find_all_links(Access_vns, Sink,
				VnTables0, VnTables,
				Succmap0, Succmap1, Predmap0, Predmap1),
			vn_order__add_link(node_origlval(Vnlval), Sink,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			Vnrval = vn_mkword(_Tag1, SubVn),
			vn_order__find_links(SubVn, Sink, VnTables0, VnTables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vnrval = vn_const(_Const),
			Succmap = Succmap0,
			Predmap = Predmap0,
			VnTables = VnTables0
		;
			Vnrval = vn_create(_, _, _, _, _),
			Succmap = Succmap0,
			Predmap = Predmap0,
			VnTables = VnTables0
		;
			Vnrval = vn_unop(_Unop, SubVn),
			vn_order__find_links(SubVn, Sink, VnTables0, VnTables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vnrval = vn_binop(_Binop, SubVn1, SubVn2),
			vn_order__find_links(SubVn1, Sink, VnTables0, VnTables1,
				Succmap0, Succmap1, Predmap0, Predmap1),
			vn_order__find_links(SubVn2, Sink, VnTables1, VnTables,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			Vnrval = vn_stackvar_addr(_),
			Succmap = Succmap0,
			Predmap = Predmap0,
			VnTables = VnTables0
		;
			Vnrval = vn_framevar_addr(_),
			Succmap = Succmap0,
			Predmap = Predmap0,
			VnTables = VnTables0
		;
			Vnrval = vn_heap_addr(SubVn, _, _),
			vn_order__find_links(SubVn, Sink, VnTables0, VnTables,
				Succmap0, Succmap, Predmap0, Predmap)
		)
	).

:- pred vn_order__find_all_links(list(vn), vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__find_all_links(in, in, di, uo, di, uo, di, uo) is det.
:- mode vn_order__find_all_links(in, in, in, out, in, out, in, out) is det.

vn_order__find_all_links([], _Sink, VnTables, VnTables,
		Succmap, Succmap, Predmap, Predmap).
vn_order__find_all_links([Vn | Vns], Sink, VnTables0, VnTables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn_order__find_links(Vn, Sink, VnTables0, VnTables1,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn_order__find_all_links(Vns, Sink, VnTables1, VnTables,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Add specified link(s) to succmap/predmap pairs.

:- pred vn_order__add_links(list(vn_node), vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__add_links(in, in, di, uo, di, uo) is det.
:- mode vn_order__add_links(in, in, in, out, in, out) is det.

vn_order__add_links([], _, Succmap, Succmap, Predmap, Predmap).
vn_order__add_links([Source | Sources], Sink, Succmap0, Succmap,
		Predmap0, Predmap) :-
	vn_order__add_link(Source, Sink, Succmap0, Succmap1,
		Predmap0, Predmap1),
	vn_order__add_links(Sources, Sink, Succmap1, Succmap,
		Predmap1, Predmap).

:- pred vn_order__add_link(vn_node, vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__add_link(in, in, di, uo, di, uo) is det.
:- mode vn_order__add_link(in, in, in, out, in, out) is det.

vn_order__add_link(Source, Sink, Succmap0, Succmap, Predmap0, Predmap) :-
	( Source = Sink ->
		Succmap = Succmap0,
		Predmap = Predmap0
	;
		( map__search(Succmap0, Source, Sinks0) ->
			vn_order__insert_if_not_there(Sink, Sinks0, Sinks1),
			map__det_update(Succmap0, Source, Sinks1, Succmap1)
		;
			map__det_insert(Succmap0, Source, [Sink], Succmap1)
		),
		( map__search(Succmap1, Sink, _) ->
			Succmap = Succmap1
		;
			map__det_insert(Succmap1, Sink, [], Succmap)
		),
		( map__search(Predmap0, Sink, Sources0) ->
			vn_order__insert_if_not_there(Source,
				Sources0, Sources1),
			map__det_update(Predmap0, Sink, Sources1, Predmap1)
		;
			map__det_insert(Predmap0, Sink, [Source], Predmap1)
		),
		( map__search(Predmap1, Source, _) ->
			Predmap = Predmap1
		;
			map__det_insert(Predmap1, Source, [], Predmap)
		)
	).

:- pred vn_order__insert_if_not_there(vn_node, list(vn_node), list(vn_node)).
% :- mode vn_order__insert_if_not_there(in, di, uo) is det.
:- mode vn_order__insert_if_not_there(in, in, out) is det.

vn_order__insert_if_not_there(Node, Nodes0, Nodes) :-
	( list__member(Node, Nodes0) ->
		Nodes = Nodes0
	;
		Nodes = [Node | Nodes0]
	).

	% Ensure the presence of the given node without giving any links.

:- pred vn_order__add_node(vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
% :- mode vn_order__add_node(in, di, uo, di, uo) is det.
:- mode vn_order__add_node(in, in, out, in, out) is det.

vn_order__add_node(Node, Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Succmap0, Node, _) ->
		Succmap = Succmap0
	;
		map__det_insert(Succmap0, Node, [], Succmap)
	),
	( map__search(Predmap0, Node, _) ->
		Predmap = Predmap0
	;
		map__det_insert(Predmap0, Node, [], Predmap)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_order__pref_order(relmap(vn_node), list(vn_node)).
:- mode vn_order__pref_order(in, out) is det.

vn_order__pref_order(Map, Order) :-
	map__keys(Map, Nodes),
	vn_order__classify_nodes(Nodes, Origlvals, Ctrls, Shareds, Lvals),
	list__condense([Origlvals, Ctrls, Shareds, Lvals], Order).

:- pred vn_order__classify_nodes(list(vn_node), list(vn_node), list(vn_node),
	list(vn_node), list(vn_node)).
% :- mode vn_order__classify_nodes(di, uo, uo, uo, uo) is det.
:- mode vn_order__classify_nodes(in, out, out, out, out) is det.

vn_order__classify_nodes([], [], [], [], []).
vn_order__classify_nodes([Node | Nodes], Origlvals, Ctrls, Shareds, Lvals) :-
	(
		Node = node_origlval(_),
		vn_order__classify_nodes(Nodes, Origlvals0, Ctrls, Shareds,
			Lvals),
		Origlvals = [Node | Origlvals0]
	;
		Node = node_ctrl(_),
		vn_order__classify_nodes(Nodes, Origlvals, Ctrls0, Shareds,
			Lvals),
		Ctrls = [Node | Ctrls0]
	;
		Node = node_shared(_),
		vn_order__classify_nodes(Nodes, Origlvals, Ctrls, Shareds0,
			Lvals),
		Shareds = [Node | Shareds0]
	;
		Node = node_lval(_),
		vn_order__classify_nodes(Nodes, Origlvals, Ctrls, Shareds,
			Lvals0),
		Lvals = [Node | Lvals0]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for converting the output of the approximate topological
	% sort into the proposed order of evaluation.

	% We try to put registers before stack variables, since this
	% minimizes memory traffic. For example, the sequence
	%	r1 = field(...); stackvar(1) = r1
	% is cheaper than the sequence
	%	stackvar(1) = field(...);  r1 = stackvar(1)
	% and the C compiler may not be able to turn the latter into
	% the former.

:- pred vn_order__blocks_to_order(list(list(vn_node)), int, vn_tables,
	list(vn_node), io__state, io__state).
% :- mode vn_order__blocks_to_order(di, in, in, uo, di, uo) is det.
:- mode vn_order__blocks_to_order(in, in, in, out, di, uo) is det.

vn_order__blocks_to_order(BlockOrder, N, VnTables, Order, IOstate0, IOstate) :-
	vn_order__order_equal_lists(BlockOrder, VnTables, GoodBlockOrder,
		IOstate0, IOstate),
	list__condense(GoodBlockOrder, Order0),
	vn_order__find_last_ctrl(Order0, N, Ctrl, Order1),
	( Ctrl = [_] ->
		true
	; Ctrl = [] ->
		error("last ctrl node does not exist")
	;
		error("last ctrl node exists in multiples")
	),
	list__append(Order1, Ctrl, Order).

:- pred vn_order__find_last_ctrl(list(vn_node), int, list(vn_node),
	list(vn_node)).
% :- mode vn_order__find_last_ctrl(di, in, out, uo) is det.
:- mode vn_order__find_last_ctrl(in, in, out, out) is det.

vn_order__find_last_ctrl([], _, [], []).
vn_order__find_last_ctrl([Node0 | Nodes0], N, Ctrl, Nodes) :-
	vn_order__find_last_ctrl(Nodes0, N, Ctrl1, Nodes1),
	( Node0 = node_ctrl(N) ->
		Ctrl = [Node0 | Ctrl1],
		Nodes = Nodes1
	;
		Ctrl = Ctrl1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn_order__order_equal_lists(list(list(vn_node)), vn_tables,
	list(list(vn_node)), io__state, io__state).
% :- mode vn_order__order_equal_lists(di, in, uo, di, uo) is det.
:- mode vn_order__order_equal_lists(in, in, out, di, uo) is det.

vn_order__order_equal_lists([], _, []) --> [].
vn_order__order_equal_lists([Block | Blocks], VnTables,
		[GoodBlock | GoodBlocks]) -->
	vn_order__order_equals(Block, VnTables, GoodBlock),
	vn_order__order_equal_lists(Blocks, VnTables, GoodBlocks).

:- pred vn_order__order_equals(list(vn_node), vn_tables, list(vn_node),
	io__state, io__state).
% :- mode vn_order__order_equals(di, in, uo, di, uo) is det.
:- mode vn_order__order_equals(in, in, out, di, uo) is det.

vn_order__order_equals(Order0, VnTables, Order) -->
	vn_debug__order_equals_msg("\nold order: ", Order0),
	{ vn_order__find_ctrls(Order0, Ctrls, Order1) },
	{ vn_order__find_noops(Order1, VnTables, Noops, Order2) },
	{ vn_order__find_regs(Order2, RegsSet, Order3) },
	{ set__to_sorted_list(RegsSet, RegsList) },
	{ vn_order__find_stackvars(Order3, StackSet, Order4) },
	{ set__to_sorted_list(StackSet, StackList) },
	{ list__condense([Ctrls, Noops, RegsList, StackList, Order4], Order) },
	vn_debug__order_equals_msg("new order: ", Order).

:- pred vn_order__find_ctrls(list(vn_node), list(vn_node), list(vn_node)).
% :- mode vn_order__find_ctrls(di, out, uo) is det.
:- mode vn_order__find_ctrls(in, out, out) is det.

vn_order__find_ctrls([], [], []).
vn_order__find_ctrls([Node0 | Nodes0], Ctrls, Nodes) :-
	vn_order__find_ctrls(Nodes0, Ctrls1, Nodes1),
	( Node0 = node_ctrl(_) ->
		Ctrls = [Node0 | Ctrls1],
		Nodes = Nodes1
	;
		Ctrls = Ctrls1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn_order__find_noops(list(vn_node), vn_tables, list(vn_node),
	list(vn_node)).
% :- mode vn_order__find_noops(di, in, out, uo) is det.
:- mode vn_order__find_noops(in, in, out, out) is det.

vn_order__find_noops([], _, [], []).
vn_order__find_noops([Node0 | Nodes0], VnTables, Noops, Nodes) :-
	vn_order__find_noops(Nodes0, VnTables, Noops1, Nodes1),
	(
		Node0 = node_lval(Vnlval),
		vn_table__search_desired_value(Vnlval, Vn, VnTables),
		vn_table__search_current_value(Vnlval, Vn, VnTables)
	->
		Noops = [Node0 | Noops1],
		Nodes = Nodes1
	;
		Noops = Noops1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn_order__find_regs(list(vn_node), set(vn_node), list(vn_node)).
% :- mode vn_order__find_regs(di, out, uo) is det.
:- mode vn_order__find_regs(in, out, out) is det.

vn_order__find_regs([], Regs, []) :-
	set__init(Regs).
vn_order__find_regs([Node0 | Nodes0], Regs, Nodes) :-
	vn_order__find_regs(Nodes0, Regs1, Nodes1),
	( Node0 = node_lval(vn_reg(_, _)) ->
		set__insert(Regs1, Node0, Regs),
		Nodes = Nodes1
	;
		Regs = Regs1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn_order__find_stackvars(list(vn_node), set(vn_node), list(vn_node)).
% :- mode vn_order__find_stackvars(di, out, uo) is det.
:- mode vn_order__find_stackvars(in, out, out) is det.

vn_order__find_stackvars([], Stackvars, []) :-
	set__init(Stackvars).
vn_order__find_stackvars([Node0 | Nodes0], Stackvars, Nodes) :-
	vn_order__find_stackvars(Nodes0, Stackvars1, Nodes1),
	(
		Node0 = node_lval(Vnlval),
		( Vnlval = vn_stackvar(_) ; Vnlval = vn_framevar(_) )
	->
		set__insert(Stackvars1, Node0, Stackvars),
		Nodes = Nodes1
	;
		Stackvars = Stackvars1,
		Nodes = [Node0 | Nodes1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% We want to make sure that references to node_lvals that do
	% not actually call for any actions to be taken are at the start
	% of the order, because this way they tie up registers for access
	% for the shortest possible time.

:- pred vn_order__reorder_noops(list(vn_node), vn_tables, list(vn_node)).
% :- mode vn_order__reorder_noops(di, in, uo) is det.
:- mode vn_order__reorder_noops(in, in, out) is det.

vn_order__reorder_noops(Order0, VnTables, Order) :-
	vn_order__reorder_noops_2(Order0, VnTables, Noops, Ops),
	list__append(Noops, Ops, Order).

:- pred vn_order__reorder_noops_2(list(vn_node), vn_tables,
	list(vn_node), list(vn_node)).
% :- mode vn_order__reorder_noops_2(di, in, uo, uo) is det.
:- mode vn_order__reorder_noops_2(in, in, out, out) is det.

vn_order__reorder_noops_2([], _, [], []).
vn_order__reorder_noops_2([Node | Nodes], VnTables, Noops, Ops) :-
	vn_order__reorder_noops_2(Nodes, VnTables, Noops0, Ops0),
	(
		(
			Node = node_origlval(_)
		;
			Node = node_lval(Vnlval),
			vn_table__lookup_desired_value(Vnlval, Vn,
				"vn_order__reorder_noops_2", VnTables),
			vn_table__search_current_value(Vnlval, Vn, VnTables),
			vn_util__vnlval_access_vns(Vnlval, [])
		)
	->
		Ops = Ops0,
		Noops = [Node | Noops0]
	;
		Ops = [Node | Ops0],
		Noops = Noops0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
