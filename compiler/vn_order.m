%-----------------------------------------------------------------------------%

% vn_order.nl - find and order the nodes of the vn_table.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_order.

:- interface.

:- import_module vn_type, vn_table.
:- import_module llds, list.

:- pred vn__flush_all_nodes(vnlvalset, vn_tables, bool, ctrlmap, flushmap, int,
	list(instruction), list(instruction)).
:- mode vn__flush_all_nodes(in, in, in, in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module vn_flush, vn_temploc, vn_util, opt_debug, opt_util.
:- import_module atsort, map, bintree_set, string, int, require, std_util.

%-----------------------------------------------------------------------------%

% uncount all dead lvals
% map from (livelvals U shared_vn U ctrl) to set of (shared_vn U origlvals)
% count only unique paths, i.e. when arriving at a shared node
%  count only the shared vn, not anything else it points to
% build a topological sort based on this order, plus the order that
%  after the appearance of an lval no refs to its origlval be outstanding
%  this includes appearances as the bases of field references
%  plus the order that the livevals at a label must be before a branch
%  to that label

%-----------------------------------------------------------------------------%

vn__flush_all_nodes(Livevals, Vn_tables0, Incrhp,
		Ctrlmap, Flushmap, Ctrl, RevInstrs, Instrs) :-
	list__reverse(RevInstrs, OrigInstrs),
	bintree_set__to_sorted_list(Livevals, Live),
	vn__build_uses(Livevals, Ctrlmap, Vn_tables0, Vn_tables1),
	vn__init_templocs(5, 5, Livevals, Vn_tables0, Templocs0),

	opt_debug__write("\n\n"),
	opt_debug__dump_ctrlmap(Ctrlmap, Ctrl_str),
	opt_debug__write(Ctrl_str),
	opt_debug__dump_flushmap(Flushmap, Flush_str),
	opt_debug__write(Flush_str),
	opt_debug__dump_tables(Vn_tables1, Tables_str),
	opt_debug__write(Tables_str),

	(
		vn__req_order(Ctrlmap, Flushmap, Incrhp, Vn_tables1,
			MustSuccmap, MustPredmap)
	->
		Succmap0 = MustSuccmap,
		Predmap0 = MustPredmap,
		vn__prod_cons_order(Live, Vn_tables1, Vn_tables2,
			Succmap0, Succmap1, Predmap0, Predmap1),
		vn__use_before_redef(Succmap1, Succmap2, Predmap1, Predmap2),
		vn__vn_ctrl_order(0, Ctrlmap, Vn_tables2, Vn_tables3,
			Succmap2, Succmap3, Predmap2, Predmap3),
		vn__ctrl_vn_order(0, Flushmap, [],
			Succmap3, Succmap4, Predmap3, Predmap4),
		vn__pref_order(Succmap4, PrefOrder),

		opt_debug__write("\nMustSuccmap:\n"),
		opt_debug__dump_node_relmap(MustSuccmap, MS_str),
		opt_debug__write(MS_str),
		opt_debug__write("\nMustPredmap:\n"),
		opt_debug__dump_node_relmap(MustPredmap, MP_str),
		opt_debug__write(MP_str),
		opt_debug__write("\nSuccmap:\n"),
		opt_debug__dump_node_relmap(Succmap4, S_str),
		opt_debug__write(S_str),
		opt_debug__write("\nPredmap:\n"),
		opt_debug__dump_node_relmap(Predmap4, P_str),
		opt_debug__write(P_str),

		atsort(Succmap4, Predmap4, MustSuccmap, MustPredmap,
			PrefOrder, Blocks),
		LastCtrl is Ctrl - 1,
		vn__blockorder_to_order(Blocks, LastCtrl, Vn_tables3, Order),

		opt_debug__dump_longnodelist(Order, O_str),
		opt_debug__write("\nOrder:\n"),
		opt_debug__write(O_str),
		opt_debug__write("\n"),

		vn__flush_nodelist(Order, Ctrlmap, Vn_tables3, _Vn_tables,
			Templocs0, Templocs, Instrs0),

		(
			opt_util__has_both_incr_decr_sp(Instrs0),
			opt_util__block_refers_stackvars(Instrs0, no)
		->
			opt_util__remove_both_incr_decr_sp(Instrs0, Instrs2)
		;
			vn__push_decr_sp_back(Instrs0, Instrs1),
			vn__push_incr_sp_forw(Instrs1, Instrs2)
		),

		vn__block_cost(OrigInstrs, OrigCost),
		vn__block_cost(Instrs2, VnCost),

		opt_debug__write("Old cost: "),
		string__int_to_string(OrigCost, OC_str),
		opt_debug__write(OC_str),
		opt_debug__write("\n"),
		opt_debug__write("New cost: "),
		string__int_to_string(VnCost, VC_str),
		opt_debug__write(VC_str),
		opt_debug__write("\n"),

		( VnCost < OrigCost ->
			opt_debug__write("Result: improvement\n"),
			vn__max_temploc(Templocs, Max),
			( Max > 0 ->
				Instrs = [block(Max, Instrs2) - ""]
			;
				Instrs = Instrs2
			)
		;
			opt_debug__write("Result: no improvement\n"),
			Instrs = OrigInstrs
		)
	;
		Instrs = OrigInstrs
	).

%-----------------------------------------------------------------------------%

	% Given the information gathered on the control nodes, check whether
	% any side branch requires a value to be stored in a vnlval other than
	% the final one. If yes, we can't apply (this version of) value
	% numbering. If no, we can apply it, and we compute the compulsory
	% part of the order of nodes, which is that a vnlval that is live
	% at a side branch must be produced before that side branch.

:- pred vn__req_order(ctrlmap, flushmap, bool, vn_tables,
	relmap(vn_node), relmap(vn_node)).
:- mode vn__req_order(in, in, in, in, out, out) is semidet.

vn__req_order(Ctrlmap, Flushmap, Incrhp, Vn_tables, MustSuccmap, MustPredmap) :-
	map__init(MustSuccmap0),
	map__init(MustPredmap0),
	vn__req_order_2(0, Ctrlmap, Flushmap, Incrhp, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap).

:- pred vn__req_order_2(int, ctrlmap, flushmap, bool, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__req_order_2(in, in, in, in, in, di, uo, di, uo) is semidet.

vn__req_order_2(Ctrl, Ctrlmap, Flushmap, Heapop0, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap) :-
	( map__search(Ctrlmap, Ctrl, VnInstr) ->
		( VnInstr = vn_mark_hp(_) ->
			Heapop0 = no,
			Heapop1 = yes
		; VnInstr = vn_restore_hp(_) ->
			Heapop0 = no,
			Heapop1 = yes
		;
			Heapop1 = Heapop0
		),
		map__lookup(Flushmap, Ctrl, FlushEntry),
		( Ctrl > 0 ->
			PrevCtrl is Ctrl - 1,
			vn__add_link(node_ctrl(PrevCtrl), node_ctrl(Ctrl),
				MustSuccmap0, MustSuccmap1,
				MustPredmap0, MustPredmap1)
		;
			vn__add_node(node_ctrl(Ctrl),
				MustSuccmap0, MustSuccmap1,
				MustPredmap0, MustPredmap1)
		),
		map__to_assoc_list(FlushEntry, FlushList),
		vn__record_ctrl_deps(FlushList, node_ctrl(Ctrl), Vn_tables,
			MustSuccmap1, MustSuccmap2, MustPredmap1, MustPredmap2),
		NextCtrl is Ctrl + 1,
		vn__req_order_2(NextCtrl, Ctrlmap, Flushmap, Heapop1, Vn_tables,
			MustSuccmap2, MustSuccmap, MustPredmap2, MustPredmap)
	;
		MustSuccmap = MustSuccmap0,
		MustPredmap = MustPredmap0
	).

:- pred vn__record_ctrl_deps(assoc_list(vnlval, vn), vn_node, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__record_ctrl_deps(in, in, in, di, uo, di, uo) is semidet.

vn__record_ctrl_deps([], _Sink, _Vn_tables,
		MustSuccmap, MustSuccmap, MustPredmap, MustPredmap).
vn__record_ctrl_deps([Vnlval - Vn | FlushList], Sink, Vn_tables,
		MustSuccmap0, MustSuccmap, MustPredmap0, MustPredmap) :-
	vn__lookup_desired_value(Vnlval, Des_vn, Vn_tables),
	Vn = Des_vn,
	vn__add_link(node_lval(Vnlval), Sink,
		MustSuccmap0, MustSuccmap1, MustPredmap0, MustPredmap1),
	vn__record_ctrl_deps(FlushList, Sink, Vn_tables,
		MustSuccmap1, MustSuccmap, MustPredmap1, MustPredmap).

%-----------------------------------------------------------------------------%

	% Record the desirability of producing the vnlvals involved in the
	% expressions inside control nodes before producing the control nodes
	% themselves. This is stated as a suggestion, not a requirement, since
	% it can be overridden during the flushing process.

:- pred vn__vn_ctrl_order(int, ctrlmap, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__vn_ctrl_order(in, in, di, uo, di, uo, di, uo) is det.

vn__vn_ctrl_order(Ctrl, Ctrlmap, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Ctrlmap, Ctrl, Vn_instr) ->
		(
			Vn_instr = vn_call(_, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_call_closure(_, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_mkframe(_, _, _),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_modframe(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_label(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_goto(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_computed_goto(Vn, _),
			vn__find_links(Vn, node_ctrl(Ctrl),
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_if_val(Vn, _),
			vn__find_links(Vn, node_ctrl(Ctrl),
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_mark_hp(Vnlval),
			vn__vnlval_access_vns(Vnlval, Vns),
			vn__find_all_links(Vns, node_ctrl(Ctrl),
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_restore_hp(Vn),
			vn__find_links(Vn, node_ctrl(Ctrl),
				Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1)
		;
			Vn_instr = vn_incr_sp(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		;
			Vn_instr = vn_decr_sp(_),
			Succmap1 = Succmap0,
			Predmap1 = Predmap0,
			Vn_tables1 = Vn_tables0
		),
		NextCtrl is Ctrl + 1,
		vn__vn_ctrl_order(NextCtrl, Ctrlmap, Vn_tables1, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		Succmap = Succmap0,
		Predmap = Predmap0,
		Vn_tables = Vn_tables0
	).

%-----------------------------------------------------------------------------%

	% Record the desirability of not producing any lvals before a control
	% node expect those we absolutely have to, since any such production
	% will be wasted on one control path.

:- pred vn__ctrl_vn_order(int, flushmap, list(vnlval),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__ctrl_vn_order(in, in, in, di, uo, di, uo) is det.

vn__ctrl_vn_order(Ctrl, Flushmap, FlushBefore,
		Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Flushmap, Ctrl, FlushEntry) ->
		map__keys(Succmap0, Nodes),
		map__keys(FlushEntry, FlushVnlvals),
		list__append(FlushBefore, FlushVnlvals, FlushNow),
		vn__record_antideps(Nodes, FlushNow, Ctrl,
			Succmap0, Succmap1, Predmap0, Predmap1),
		NextCtrl is Ctrl + 1,
		vn__ctrl_vn_order(NextCtrl, Flushmap, FlushNow,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		Succmap = Succmap0,
		Predmap = Predmap0
	).

:- pred vn__record_antideps(list(vn_node), list(vnlval), int,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__record_antideps(in, in, in, di, uo, di, uo) is det.

vn__record_antideps([], _, _, Succmap, Succmap, Predmap, Predmap).
vn__record_antideps([Node | Nodelist], FlushVnlvals, Ctrl,
		Succmap0, Succmap, Predmap0, Predmap) :-
	(
		% once upon a time any shared node was also included
		Node = node_lval(Vnlval),
		\+ list__member(Vnlval, FlushVnlvals)
	->
		vn__add_link(node_ctrl(Ctrl), Node,
			Succmap0, Succmap1, Predmap0, Predmap1)
	;
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	),
	vn__record_antideps(Nodelist, FlushVnlvals, Ctrl,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the natural producer-consumer relationships
	% induced by the live vnlvals.

:- pred vn__prod_cons_order(list(vnlval), vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__prod_cons_order(in, di, uo, di, uo, di, uo) is det.

vn__prod_cons_order([], Vn_tables, Vn_tables,
		Succmap, Succmap, Predmap, Predmap).
vn__prod_cons_order([Vnlval | Vnlvals], Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__lookup_desired_value(Vnlval, Vn, Vn_tables0),
	vn__find_links(Vn, node_lval(Vnlval), Vn_tables0, Vn_tables1,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn__prod_cons_order(Vnlvals, Vn_tables1, Vn_tables,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Try to make sure that all immediate users of the original value
	% of a vnlval are done before the vnlval is redefined. This avoids
	% an instruction to save the original value somewhere else.

:- pred vn__use_before_redef(relmap(vn_node), relmap(vn_node),
	relmap(vn_node), relmap(vn_node)).
:- mode vn__use_before_redef(di, uo, di, uo) is det.

vn__use_before_redef(Succmap0, Succmap, Predmap0, Predmap) :-
	map__keys(Predmap0, Sinks),
	vn__use_sinks_before_redef(Sinks, Succmap0, Succmap, Predmap0, Predmap).

:- pred vn__use_sinks_before_redef(list(vn_node),
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__use_sinks_before_redef(in, di, uo, di, uo) is det.

vn__use_sinks_before_redef([], Succmap, Succmap, Predmap, Predmap).
vn__use_sinks_before_redef([Sink | Sinks],
		Succmap0, Succmap, Predmap0, Predmap) :-
	(
		Sink = node_lval(Vnlval),
		map__search(Succmap0, node_origlval(Vnlval), Users)
	->
		vn__add_links(Users, Sink,
			Succmap0, Succmap1, Predmap0, Predmap1)
	;
		Succmap1 = Succmap0,
		Predmap1 = Predmap0
	),
	vn__use_sinks_before_redef(Sinks, Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Record the dependency of the nodes inside the given vn
	% on the given node.

:- pred vn__find_links(vn, vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__find_links(in, in, di, uo, di, uo, di, uo) is det.

vn__find_links(Vn, Sink, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__lookup_uses(Vn, Uses0, Vn_tables0),
	vn__lookup_defn(Vn, Vnrval, Vn_tables0),
	(
		Uses0 = [_,_|_],
		\+ Sink = node_shared(_),
		\+ Vnrval = vn_origlval(vn_hp),
		vn__is_const_expr(Vn, no, Vn_tables0)
	->
		vn__add_link(node_shared(Vn), Sink,
			Succmap0, Succmap1, Predmap0, Predmap1),
		vn__find_links(Vn, node_shared(Vn), Vn_tables0, Vn_tables,
			Succmap1, Succmap, Predmap1, Predmap)
	;
		(
			Vnrval = vn_origlval(Vnlval),
			vn__vnlval_access_vns(Vnlval, Access_vns),
			vn__find_all_links(Access_vns, Sink,
				Vn_tables0, Vn_tables,
				Succmap0, Succmap1, Predmap0, Predmap1),
			vn__add_link(node_origlval(Vnlval), Sink,
				Succmap1, Succmap, Predmap1, Predmap)
		;
			Vnrval = vn_mkword(_Tag1, SubVn),
			vn__find_links(SubVn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vnrval = vn_const(_Const),
			Succmap = Succmap0,
			Predmap = Predmap0,
			Vn_tables = Vn_tables0
		;
			Vnrval = vn_create(_Tag2, _Args, _Label),
			Succmap = Succmap0,
			Predmap = Predmap0,
			Vn_tables = Vn_tables0
		;
			Vnrval = vn_unop(_Unop, SubVn),
			vn__find_links(SubVn, Sink, Vn_tables0, Vn_tables,
				Succmap0, Succmap, Predmap0, Predmap)
		;
			Vnrval = vn_binop(_Binop, SubVn1, SubVn2),
			vn__find_links(SubVn1, Sink, Vn_tables0, Vn_tables1,
				Succmap0, Succmap1, Predmap0, Predmap1),
			vn__find_links(SubVn2, Sink, Vn_tables1, Vn_tables,
				Succmap1, Succmap, Predmap1, Predmap)
		)
	).

:- pred vn__find_all_links(list(vn), vn_node, vn_tables, vn_tables,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__find_all_links(in, in, di, uo, di, uo, di, uo) is det.

vn__find_all_links([], _Sink, Vn_tables, Vn_tables,
		Succmap, Succmap, Predmap, Predmap).
vn__find_all_links([Vn | Vns], Sink, Vn_tables0, Vn_tables,
		Succmap0, Succmap, Predmap0, Predmap) :-
	vn__find_links(Vn, Sink, Vn_tables0, Vn_tables1,
		Succmap0, Succmap1, Predmap0, Predmap1),
	vn__find_all_links(Vns, Sink, Vn_tables1, Vn_tables,
		Succmap1, Succmap, Predmap1, Predmap).

%-----------------------------------------------------------------------------%

	% Add specified link(s) to succmap/predmap pairs.

:- pred vn__add_links(list(vn_node), vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__add_links(in, in, di, uo, di, uo) is det.

vn__add_links([], _, Succmap, Succmap, Predmap, Predmap).
vn__add_links([Source | Sources], Sink, Succmap0, Succmap, Predmap0, Predmap) :-
	vn__add_link(Source, Sink, Succmap0, Succmap1, Predmap0, Predmap1),
	vn__add_links(Sources, Sink, Succmap1, Succmap, Predmap1, Predmap).

:- pred vn__add_link(vn_node, vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__add_link(in, in, di, uo, di, uo) is det.

vn__add_link(Source, Sink, Succmap0, Succmap, Predmap0, Predmap) :-
	( Source = Sink ->
		Succmap = Succmap0,
		Predmap = Predmap0
	;
		( map__search(Succmap0, Source, Sinks0) ->
			vn__insert_if_not_there(Sink, Sinks0, Sinks1),
			map__set(Succmap0, Source, Sinks1, Succmap1)
		;
			map__set(Succmap0, Source, [Sink], Succmap1)
		),
		( map__search(Succmap0, Sink, _) ->
			Succmap = Succmap1
		;
			map__set(Succmap1, Sink, [], Succmap)
		),
		( map__search(Predmap0, Sink, Sources0) ->
			vn__insert_if_not_there(Source, Sources0, Sources1),
			map__set(Predmap0, Sink, Sources1, Predmap1)
		;
			map__set(Predmap0, Sink, [Source], Predmap1)
		),
		( map__search(Predmap0, Source, _) ->
			Predmap = Predmap1
		;
			map__set(Predmap1, Source, [], Predmap)
		)
	).

:- pred vn__insert_if_not_there(vn_node, list(vn_node), list(vn_node)).
:- mode vn__insert_if_not_there(in, di, uo) is det.

vn__insert_if_not_there(Node, Nodes0, Nodes) :-
	( list__member(Node, Nodes0) ->
		Nodes = Nodes0
	;
		Nodes = [Node | Nodes0]
	).

	% Ensure the presence of the given node without giving any links.

:- pred vn__add_node(vn_node,
	relmap(vn_node), relmap(vn_node), relmap(vn_node), relmap(vn_node)).
:- mode vn__add_node(in, di, uo, di, uo) is det.

vn__add_node(Node, Succmap0, Succmap, Predmap0, Predmap) :-
	( map__search(Succmap0, Node, _) ->
		Succmap = Succmap0
	;
		map__set(Succmap0, Node, [], Succmap)
	),
	( map__search(Predmap0, Node, _) ->
		Predmap = Predmap0
	;
		map__set(Predmap0, Node, [], Predmap)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__pref_order(relmap(vn_node), list(vn_node)).
:- mode vn__pref_order(in, out) is det.

vn__pref_order(Map, Order) :-
	map__keys(Map, Nodes),
	vn__classify_nodes(Nodes, Origlvals, Ctrls, Shareds, Lvals),
	list__condense([Origlvals, Ctrls, Shareds, Lvals], Order).

:- pred vn__classify_nodes(list(vn_node), list(vn_node), list(vn_node),
	list(vn_node), list(vn_node)).
:- mode vn__classify_nodes(di, uo, uo, uo, uo) is det.

vn__classify_nodes([], [], [], [], []).
vn__classify_nodes([Node | Nodes], Origlvals, Ctrls, Shareds, Lvals) :-
	(
		Node = node_origlval(_),
		vn__classify_nodes(Nodes, Origlvals0, Ctrls, Shareds, Lvals),
		Origlvals = [Node | Origlvals0]
	;
		Node = node_ctrl(_),
		vn__classify_nodes(Nodes, Origlvals, Ctrls0, Shareds, Lvals),
		Ctrls = [Node | Ctrls0]
	;
		Node = node_shared(_),
		vn__classify_nodes(Nodes, Origlvals, Ctrls, Shareds0, Lvals),
		Shareds = [Node | Shareds0]
	;
		Node = node_lval(_),
		vn__classify_nodes(Nodes, Origlvals, Ctrls, Shareds, Lvals0),
		Lvals = [Node | Lvals0]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for converting the output of the approximate topological
	% sort into the proposed order of evaluation. We must make sure that
	% the condition, if any, comes last.

	% We also try to put registers before stack variables, since this
	% minimizes memory traffic. For example, the sequence
	%	r1 = field(...); stackvar(1) = r1
	% is cheaper than the sequence
	%	stackvar(1) = field(...);  r1 = stackvar(1)
	% and the C compiler may not be able to turn the latter into
	% the former.

:- pred vn__blockorder_to_order(list(list(vn_node)), int, vn_tables,
	list(vn_node)).
:- mode vn__blockorder_to_order(in, in, in, out) is det.

vn__blockorder_to_order(BlockOrder, N, Vn_tables, Order) :-
	vn__order_equal_lists(BlockOrder, Vn_tables, GoodBlockOrder),
	list__condense(GoodBlockOrder, Order0),
	vn__find_last_ctrl(Order0, N, Ctrl, Order1),
	( Ctrl = [_] ->
		true
	; Ctrl = [] ->
		error("last ctrl node does not exist")
	;
		error("last ctrl node exists in multiples")
	),
	list__append(Order1, Ctrl, Order).

:- pred vn__order_equal_lists(list(list(vn_node)), vn_tables,
	list(list(vn_node))).
:- mode vn__order_equal_lists(di, in, uo) is det.

vn__order_equal_lists([], _, []).
vn__order_equal_lists([Block | Blocks], Vn_tables, [GoodBlock | GoodBlocks]) :-
	vn__order_equals(Block, Vn_tables, GoodBlock),
	vn__order_equal_lists(Blocks, Vn_tables, GoodBlocks).

:- pred vn__order_equals(list(vn_node), vn_tables, list(vn_node)).
:- mode vn__order_equals(di, in, uo) is det.

vn__order_equals(Order0, Vn_tables, Order) :-
	vn__find_ctrls(Order0, Ctrls, Order1),
	vn__find_noops(Order1, Vn_tables, Noops, Order2),
	vn__find_regs(Order2, Regs, Order3),
	list__condense([Ctrls, Noops, Regs, Order3], Order).

:- pred vn__find_ctrls(list(vn_node), list(vn_node), list(vn_node)).
:- mode vn__find_ctrls(di, out, uo) is det.

vn__find_ctrls([], [], []).
vn__find_ctrls([Node0 | Nodes0], Ctrls, Nodes) :-
	vn__find_ctrls(Nodes0, Ctrls1, Nodes1),
	( Node0 = node_ctrl(_) ->
		Ctrls = [Node0 | Ctrls1],
		Nodes = Nodes1
	;
		Ctrls = Ctrls1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn__find_noops(list(vn_node), vn_tables, list(vn_node), list(vn_node)).
:- mode vn__find_noops(di, in, out, uo) is det.

vn__find_noops([], _, [], []).
vn__find_noops([Node0 | Nodes0], Vn_tables, Noops, Nodes) :-
	vn__find_noops(Nodes0, Vn_tables, Noops1, Nodes1),
	(
		Node0 = node_lval(Vnlval),
		vn__search_desired_value(Vnlval, Vn, Vn_tables),
		vn__search_current_value(Vnlval, Vn, Vn_tables)
	->
		Noops = [Node0 | Noops1],
		Nodes = Nodes1
	;
		Noops = Noops1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn__find_regs(list(vn_node), list(vn_node), list(vn_node)).
:- mode vn__find_regs(di, out, uo) is det.

vn__find_regs([], [], []).
vn__find_regs([Node0 | Nodes0], Regs, Nodes) :-
	vn__find_regs(Nodes0, Regs1, Nodes1),
	( Node0 = node_lval(vn_reg(_)) ->
		Regs = [Node0 | Regs1],
		Nodes = Nodes1
	;
		Regs = Regs1,
		Nodes = [Node0 | Nodes1]
	).

:- pred vn__find_last_ctrl(list(vn_node), int, list(vn_node), list(vn_node)).
:- mode vn__find_last_ctrl(di, in, out, uo) is det.

vn__find_last_ctrl([], _, [], []).
vn__find_last_ctrl([Node0 | Nodes0], N, Ctrl, Nodes) :-
	vn__find_last_ctrl(Nodes0, N, Ctrl1, Nodes1),
	( Node0 = node_ctrl(N) ->
		Ctrl = [Node0 | Ctrl1],
		Nodes = Nodes1
	;
		Ctrl = Ctrl1,
		Nodes = [Node0 | Nodes1]
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

:- pred vn__boundary_instr(instr, bool).
:- mode vn__boundary_instr(in, out) is det.

vn__boundary_instr(comment(_), no).
vn__boundary_instr(livevals(_), no).
vn__boundary_instr(block(_, _), no).
vn__boundary_instr(assign(_,_), no).
vn__boundary_instr(call(_, _, _), yes).
vn__boundary_instr(call_closure(_, _, _), yes).
vn__boundary_instr(mkframe(_, _, _), yes).
vn__boundary_instr(modframe(_), yes).
vn__boundary_instr(label(_), yes).
vn__boundary_instr(goto(_), yes).
vn__boundary_instr(computed_goto(_, _), yes).
vn__boundary_instr(c_code(_), yes).
vn__boundary_instr(if_val(_, _), yes).
vn__boundary_instr(incr_hp(_, _, _), no).
vn__boundary_instr(mark_hp(_), no).
vn__boundary_instr(restore_hp(_), no).
vn__boundary_instr(incr_sp(_), yes).
vn__boundary_instr(decr_sp(_), yes).
