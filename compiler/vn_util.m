%-----------------------------------------------------------------------------%

% Vn_util.nl - utility predicates for value numbering.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module vn_util.

:- interface.
:- import_module value_number, bintree_set, llds, list, int.

:- implementation.
:- import_module require, std_util, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for the livemap building phase.

:- interface.

	% Set all lvals found in this rval to live.

:- pred vn__make_live(rval, lvalset, lvalset).
:- mode vn__make_live(in, di, uo) is det.

	% Set this lval to dead.

:- pred vn__make_dead(lval, lvalset, lvalset).
:- mode vn__make_dead(in, di, uo) is det.

:- implementation.

vn__make_live(Rval, Livevals0, Livevals) :-
	(
		Rval = lval(Lval),
		vn__make_live_lval(Lval, Livevals0, Livevals)
	;
		Rval = create(_, _, _),
		Livevals = Livevals0
	;
		Rval = mkword(_, Rval1),
		vn__make_live(Rval1, Livevals0, Livevals)
	;
		Rval = const(_),
		Livevals = Livevals0
	;
		Rval = unop(_, Rval1),
		vn__make_live(Rval1, Livevals0, Livevals)
	;
		Rval = binop(_, Rval1, Rval2),
		vn__make_live(Rval1, Livevals0, Livevals1),
		vn__make_live(Rval2, Livevals1, Livevals)
	;
		Rval = var(_),
		error("var rval should not propagate to value_number")
	).

:- pred vn__make_live_all(list(rval), lvalset, lvalset).
:- mode vn__make_live_all(in, di, uo) is det.

vn__make_live_all([], Livevals, Livevals).
vn__make_live_all([Rval | Rvals], Livevals0, Livevals) :-
	vn__make_live(Rval, Livevals0, Livevals1),
	vn__make_live_all(Rvals, Livevals1, Livevals).

:- pred vn__make_live_lval(lval, lvalset, lvalset).
:- mode vn__make_live_lval(in, di, uo) is det.

vn__make_live_lval(Lval, Livevals0, Livevals) :-
	% XXX find_subrvals(Lval, RvalList) is more robust
	( Lval = field(_, Rval1, Rval2) ->
		vn__make_live(Rval1, Livevals0, Livevals1),
		vn__make_live(Rval2, Livevals1, Livevals)
	;
		bintree_set__insert(Livevals0, Lval, Livevals)
	).

vn__make_dead(Lval, Livevals0, Livevals) :-
	bintree_set__delete(Livevals0, Lval, Livevals1),
	vn__lval_access_rval(Lval, Rvals),
	vn__make_live_all(Rvals, Livevals1, Livevals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for the forward phase of vn__handle_instr

:- interface.

	% Reflect the effect of a livevals instr on the set of live vnlvals.

:- pred vn__handle_livevals(bool, lvalset, vnlvalset, vnlvalset).
:- mode vn__handle_livevals(in, in, di, uo) is det.

	% Reflect the action of an assignment in the vn tables.

:- pred vn__handle_assign(lval, rval, vn_tables, vn_tables).
:- mode vn__handle_assign(in, in, di, uo) is det.

:- implementation.

vn__handle_livevals(Terminate, Livevals, Liveset0, Liveset) :-
	( Terminate = yes ->
		true
	;
		error("non-terminating livevals in vn__handle_instr")
	),
	bintree_set__to_sorted_list(Livevals, Livelist),
	vn__convert_to_vnlval_and_insert(Livelist, Liveset0, Liveset).

:- pred vn__convert_to_vnlval_and_insert(list(lval), vnlvalset, vnlvalset).
:- mode vn__convert_to_vnlval_and_insert(in, di, uo) is det.

vn__convert_to_vnlval_and_insert([], Liveset, Liveset).
vn__convert_to_vnlval_and_insert([Lval | Lvals], Liveset0, Liveset) :-
	vn__no_heap_lval_to_vnlval(Lval, MaybeVnlval),
	(
		MaybeVnlval = yes(Vnlval),
		bintree_set__insert(Liveset0, Vnlval, Liveset1)
	;
		MaybeVnlval = no,
		Liveset1 = Liveset0
	),
	vn__convert_to_vnlval_and_insert(Lvals, Liveset1, Liveset).

vn__handle_assign(Lval, Rval, Vn_tables0, Vn_tables) :-
	vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables1),
	vn__lval_to_vnlval(Lval, Vnlval, Vn_tables1, Vn_tables2),
	vn__set_desired_value(Vnlval, Vn, Vn_tables2, Vn_tables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% An abstract data type for temporary locations.

:- interface.

:- type templocs.

	% Initialize the list of temporary locations.

:- pred vn__init_templocs(int, int, bintree_set(lval), templocs).
:- mode vn__init_templocs(in, in, in, out) is det.

	% Get a temporary location.

:- pred vn__next_temploc(templocs, templocs, vnlval).
:- mode vn__next_temploc(di, uo, out) is det.

	% Prevent the use of this location as a temporary.

:- pred vn__no_temploc(vnlval, templocs, templocs).
:- mode vn__no_temploc(in, di, uo) is det.

	% Make a location available for reuse as a temporary location
	% _if_  it is not live. Heap locations are implicitly live; other
	% locations are live if they were recorded as such during init.

:- pred vn__reuse_templocs(list(vnlval), templocs, templocs).
:- mode vn__reuse_templocs(in, di, uo) is det.

	% Give the number of the highest temp variable used.

:- pred vn__max_temploc(templocs, int).
:- mode vn__max_temploc(in, out) is det.

:- implementation.

:- type templocs ---> quad(list(vnlval), list(vnlval), int, int).

vn__init_templocs(MaxTemp, MaxReg, Livevals, Templocs) :-
	vn__get_n_temps(1, MaxTemp, Temps),
	vn__find_free_regs(1, MaxReg, Livevals, Regs),
	NextTemp is MaxTemp + 1,
	list__append(Regs, Temps, Queue),
	bintree_set__to_sorted_list(Livevals, Livelist),
	vn__find_live_non_heap(Livelist, Livevnlvals),
	Templocs = quad(Queue, Livevnlvals, 0, NextTemp).

vn__next_temploc(quad(Queue0, Livevnlvals, MaxUsed0, Next0),
		quad(Queue1, Livevnlvals, MaxUsed1, Next1), Vnlval) :-
	( Queue0 = [Head | Tail] ->
		Vnlval = Head,
		Queue1 = Tail,
		Next1 = Next0
	;
		Vnlval = vn_temp(Next0),
		Queue1 = Queue0,
		Next1 is Next0 + 1
	),
	( Vnlval = vn_temp(N) ->
		int__max(MaxUsed0, N, MaxUsed1)
	;
		MaxUsed1 = MaxUsed0
	).

vn__no_temploc(Vnlval, quad(Queue0, Livevnlvals, MaxUsed, Next),
		quad(Queue, Livevnlvals, MaxUsed, Next)) :-
	list__delete_all(Queue0, Vnlval, Queue).

vn__reuse_templocs([], Templocs, Templocs).
vn__reuse_templocs([Vnlval | Vnlvals], Templocs0, Templocs) :-
	Templocs0 = quad(Queue0, Livevnlvals, MaxUsed, Next),
	( Vnlval = vn_field(_, _, _) ->
		Templocs1 = Templocs0
	; list__member(Vnlval, Livevnlvals) ->
		Templocs1 = Templocs0
	;
		list__append(Queue0, [Vnlval], Queue1),
		Templocs1 = quad(Queue1, Livevnlvals, MaxUsed, Next)
	),
	vn__reuse_templocs(Vnlvals, Templocs1, Templocs).

vn__max_temploc(quad(_Queue0, _Livevnlvals, MaxUsed, _Next), MaxUsed).

	% Return the non-live registers from the first N registers.

:- pred vn__find_free_regs(int, int, bintree_set(lval), list(vnlval)).
:- mode vn__find_free_regs(in, in, in, out) is det.

vn__find_free_regs(N, Max, Livevals, Freeregs) :-
	( N > Max ->
		Freeregs = []
	;
		N1 is N + 1,
		vn__find_free_regs(N1, Max, Livevals, Freeregs0),
		( bintree_set__member(reg(r(N)), Livevals) ->
			Freeregs = Freeregs0
		;
			Freeregs = [vn_reg(r(N)) | Freeregs0]
		)
	).

	% Return a list of the first N temp locations.

:- pred vn__get_n_temps(int, int, list(vnlval)).
:- mode vn__get_n_temps(in, in, out) is det.

vn__get_n_temps(N, Max, Temps) :-
	( N > Max ->
		Temps = []
	;
		N1 is N + 1,
		vn__get_n_temps(N1, Max, Temps0),
		Temps = [vn_temp(N) | Temps0]
	).

:- pred vn__find_live_non_heap(list(lval), list(vnlval)).
:- mode vn__find_live_non_heap(in, out) is det.

vn__find_live_non_heap([], []).
vn__find_live_non_heap([Liveval | Livevals], Livevnlvals) :-
	vn__find_live_non_heap(Livevals, Livevnlvals0),
	(
		Liveval = reg(R),
		Livevnlvals = [vn_reg(R) | Livevnlvals0]
	;
		Liveval = stackvar(N),
		Livevnlvals = [vn_stackvar(N) | Livevnlvals0]
	;
		Liveval = framevar(N),
		Livevnlvals = [vn_framevar(N) | Livevnlvals0]
	;
		Liveval = succip,
		Livevnlvals = [vn_succip | Livevnlvals0]
	;
		Liveval = maxfr,
		Livevnlvals = [vn_maxfr | Livevnlvals0]
	;
		Liveval = curredoip,
		Livevnlvals = [vn_curredoip | Livevnlvals0]
	;
		Liveval = hp,
		Livevnlvals = [vn_hp | Livevnlvals0]
	;
		Liveval = sp,
		Livevnlvals = [vn_sp | Livevnlvals0]
	;
		Liveval = field(_, _, _),
		Livevnlvals = Livevnlvals0
	;
		Liveval = lvar(_),
		error("found lvar in vn__find_live_non_heap")
	;
		Liveval = temp(N),
		Livevnlvals = [vn_temp(N) | Livevnlvals0]
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for the creation of control nodes.

:- interface.

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
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
	;
		Vn_instr = vn_call_closure(_, _, _),
		Vn_tables = Vn_tables0,
		Livevals = Livevals0,
		FlushEntry = FlushEntry0
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
		( TargetAddr = label(Label) ->
			vn__record_one_label(Label, Livemap,
				Vn_tables0, Vn_tables, Livevals0, Livevals,
				FlushEntry0, FlushEntry)
		;
			Vn_tables = Vn_tables0,
			Livevals = Livevals0,
			FlushEntry = FlushEntry0
		)
	;
		Vn_instr = vn_computed_goto(_, Labels),
		vn__record_several_labels(Labels, Livemap,
			Vn_tables0, Vn_tables, Livevals0, Livevals,
			FlushEntry0, FlushEntry)
	;
		Vn_instr = vn_if_val(_, TargetAddr),
		( TargetAddr = label(Label) ->
			vn__record_one_label(Label, Livemap,
				Vn_tables0, Vn_tables, Livevals0, Livevals,
				FlushEntry0, FlushEntry)
		;
			Vn_tables = Vn_tables0,
			Livevals = Livevals0,
			FlushEntry = FlushEntry0
		)
	),
	map__set(Flushmap0, Ctrl0, FlushEntry, Flushmap).

%-----------------------------------------------------------------------------%

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
	map__lookup(Livemap, Label, Liveset),
	bintree_set__to_sorted_list(Liveset, Livelist),
	vn__record_livevals(Livelist, Vn_tables0, Vn_tables,
		Livevals0, Livevals, FlushEntry0, FlushEntry).

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

%-----------------------------------------------------------------------------%

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
	map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
	bintree_set__insert(Livevals0, Vnlval, Livevals1),
	vn__record_compulsory_lval_list(Lval_vn_list,
		Livevals1, Livevals, FlushEntry1, FlushEntry).

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

:- interface.

:- pred vn__blockorder_to_order(list(list(vn_node)), int, list(vn_node)).
:- mode vn__blockorder_to_order(in, in, out) is det.

:- implementation.

vn__blockorder_to_order(BlockOrder, N, Order) :-
	vn__order_equal_lists(BlockOrder, GoodBlockOrder),
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

:- pred vn__order_equal_lists(list(list(vn_node)), list(list(vn_node))).
:- mode vn__order_equal_lists(di, uo) is det.

vn__order_equal_lists([], []).
vn__order_equal_lists([Block | Blocks], [GoodBlock | GoodBlocks]) :-
	vn__order_equals(Block, GoodBlock),
	vn__order_equal_lists(Blocks, GoodBlocks).

:- pred vn__order_equals(list(vn_node), list(vn_node)).
:- mode vn__order_equals(di, uo) is det.

vn__order_equals(Order0, Order) :-
	vn__find_regs(Order0, Regs, Order1),
	list__append(Regs, Order1, Order).

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

:- pred vn__find_sub_vns(vnrval, list(vn)).
:- mode vn__find_sub_vns(in, out) is det.

vn__find_sub_vns(vn_origlval(Vnlval), SubVns) :-
	(Vnlval = vn_field(_, SubVn1, SubVn2) ->
		SubVns = [SubVn1, SubVn2]
	;
		SubVns = []
	).
vn__find_sub_vns(vn_mkword(_, SubVn), [SubVn]).
vn__find_sub_vns(vn_const(_), []).
vn__find_sub_vns(vn_create(_, _, _), []).
vn__find_sub_vns(vn_unop(_, SubVn), [SubVn]).
vn__find_sub_vns(vn_binop(_, SubVn1, SubVn2), [SubVn1, SubVn2]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% XXX

:- interface.

	% Convert an rval into a vnrval and hence into a vn.

:- pred vn__rval_to_vn(rval, vn, vn_tables, vn_tables).
:- mode vn__rval_to_vn(in, out, di, uo) is det.

:- pred vn__lval_to_vnlval(lval, vnlval, vn_tables, vn_tables).
:- mode vn__lval_to_vnlval(in, out, di, uo) is det.

:- pred vn__is_const_expr(vn, vn_tables).
:- mode vn__is_const_expr(in, in) is semidet.

	% Find out what vn, if any, is needed to access a vnlval.

:- pred vn__vnlval_access_vn(vnlval, list(vn)).
:- mode vn__vnlval_access_vn(in, out) is det.

:- implementation.

%-----------------------------------------------------------------------------%

vn__rval_to_vn(Rval, Vn, Vn_tables0, Vn_tables) :-
	(
		Rval = lval(Lval),
		vn__lval_to_vn(Lval, Vn, Vn_tables0, Vn_tables)
	;
		Rval = var(_),
		error("value_number should never get rval: var")
	;
		Rval = create(Tag, Args, Label),
		vn__vnrval_to_vn(vn_create(Tag, Args, Label), Vn,
			Vn_tables0, Vn_tables)
	;
		Rval = mkword(Tag, Rval1),
		vn__rval_to_vn(Rval1, SubVn, Vn_tables0, Vn_tables1),
		vn__vnrval_to_vn(vn_mkword(Tag, SubVn), Vn,
			Vn_tables1, Vn_tables)
	;
		Rval = const(Const),
		vn__vnrval_to_vn(vn_const(Const), Vn,
			Vn_tables0, Vn_tables)
	;
		Rval = unop(Unop, Rval1),
		vn__rval_to_vn(Rval1, SubVn, Vn_tables0, Vn_tables1),
		vn__vnrval_to_vn(vn_unop(Unop, SubVn), Vn,
			Vn_tables1, Vn_tables)
	;
		Rval = binop(Binop, Rval1, Rval2),
		vn__rval_to_vn(Rval1, SubVn1, Vn_tables0, Vn_tables1),
		vn__rval_to_vn(Rval2, SubVn2, Vn_tables1, Vn_tables2),
		vn__vnrval_to_vn(vn_binop(Binop, SubVn1, SubVn2), Vn,
			Vn_tables2, Vn_tables)
	).

:- pred vn__vnrval_to_vn(vnrval, vn, vn_tables, vn_tables).
:- mode vn__vnrval_to_vn(in, out, di, uo) is det.

vn__vnrval_to_vn(Vnrval, Vn, Vn_tables0, Vn_tables) :-
	vn__simplify_vnrval(Vnrval, Vnrval1, Vn_tables0, Vn_tables1),
	( vn__search_assigned_vn(Vnrval1, Vn_prime, Vn_tables1) ->
		Vn = Vn_prime,
		Vn_tables = Vn_tables1
	;
		vn__record_first_vnrval(Vnrval1, Vn, Vn_tables1, Vn_tables)
	).

	% Simplify the vnrval by partially evaluating expressions involving
	% integer constants. To make this simpler, swap the arguments of
	% commutative expressions around to put the constants on the right
	% side.

	% The simplification has to be done on vnrvals and not on rvals
	% even though this complicates the code. The reason is that an
	% expression such as r1 + 4 can be simplified if we know that
	% r1 was defined as r2 + 8.

	% XXX more simplification opportunities exist

:- pred vn__simplify_vnrval(vnrval, vnrval, vn_tables, vn_tables).
:- mode vn__simplify_vnrval(in, out, di, uo) is det.

vn__simplify_vnrval(Vnrval0, Vnrval, Vn_tables0, Vn_tables) :-
	( Vnrval0 = vn_binop((+), Vn1, Vn2) ->
		vn__lookup_defn(Vn1, Vnrval1, Vn_tables0),
		vn__lookup_defn(Vn2, Vnrval2, Vn_tables0),
		(
							% c1+c2 => c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I1 + I2,
			Vnrval = vn_const(int_const(I)),
			Vn_tables = Vn_tables0
		;
							% c1+e21+c22 => e21+c
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn__lookup_defn(Vn22, Vnrval22, Vn_tables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I1 + I22,
			vn__vnrval_to_vn(vn_const(int_const(I)), VnConst,
				Vn_tables0, Vn_tables),
			Vnrval = vn_binop((+), Vn21, VnConst)
		;
							% c1+e2 => e2+c1
			Vnrval1 = vn_const(int_const(I1)),
			Vnrval2 = vn_binop((+), Vn21, Vn22)
		->
			Vnrval = vn_binop((+), Vn2, Vn1),
			Vn_tables = Vn_tables0
		;
							% e11+c12+c2 => e11+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, Vn_tables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_const(int_const(I2))
		->
			I is I12 + I2,
			vn__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, Vn_tables0, Vn_tables),
			Vnrval = vn_binop((+), Vn11, VnConst)
		;
							% e11+c12+e21+c22 =>
							%	e11+e21+c
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, Vn_tables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22),
			vn__lookup_defn(Vn22, Vnrval22, Vn_tables0),
			Vnrval22 = vn_const(int_const(I22))
		->
			I is I12 + I22,
			vn__vnrval_to_vn(vn_binop((+), Vn11, Vn21), VnExpr,
				Vn_tables0, Vn_tables1),
			vn__vnrval_to_vn(vn_const(int_const(I)),
				VnConst, Vn_tables1, Vn_tables),
			Vnrval = vn_binop((+), VnExpr, VnConst)
		;
							% e11+c12+e2 =>
							%	e11+e2+c12
			Vnrval1 = vn_binop((+), Vn11, Vn12),
			vn__lookup_defn(Vn12, Vnrval12, Vn_tables0),
			Vnrval12 = vn_const(int_const(I12)),
			Vnrval2 = vn_binop((+), Vn21, Vn22)
		->
			vn__vnrval_to_vn(vn_binop((+), Vn11, Vn2), VnExpr,
				Vn_tables0, Vn_tables),
			Vnrval = vn_binop((+), VnExpr, Vn12)
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	; Vnrval0 = vn_binop((-), Vn1, Vn2) ->
		vn__lookup_defn(Vn1, Vnrval1, Vn_tables0),
		vn__lookup_defn(Vn2, Vnrval2, Vn_tables0),
		(
							% e1-c2 => e1+c
			Vnrval2 = vn_const(int_const(I2))
		->
			NI2 is 0 - I2,
			vn__vnrval_to_vn(vn_const(int_const(NI2)), VnConst,
				Vn_tables0, Vn_tables1),
			vn__simplify_vnrval(vn_binop((+), Vn1, VnConst), Vnrval,
				Vn_tables1, Vn_tables)
		;
							% c1-e2 => 0-e2+c1
			Vnrval1 = vn_const(int_const(I1))
		->
			vn__vnrval_to_vn(vn_const(int_const(0)), VnConst0,
				Vn_tables0, Vn_tables1),
			vn__vnrval_to_vn(vn_binop((-), VnConst0, Vn2), VnExpr,
				Vn_tables1, Vn_tables2),
			vn__simplify_vnrval(vn_binop((+), VnExpr, Vn1), Vnrval,
				Vn_tables2, Vn_tables)
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	;
		Vnrval = Vnrval0,
		Vn_tables = Vn_tables0
	).

:- pred vn__lval_to_vn(lval, vn, vn_tables, vn_tables).
:- mode vn__lval_to_vn(in, out, di, uo) is det.

vn__lval_to_vn(Lval, Vn, Vn_tables0, Vn_tables) :-
	vn__lval_to_vnlval(Lval, Vnlval, Vn_tables0, Vn_tables1),
	( vn__search_desired_value(Vnlval, Vn_prime, Vn_tables1) ->
		Vn = Vn_prime,
		Vn_tables = Vn_tables1
	;
		vn__record_first_vnlval(Vnlval, Vn, Vn_tables1, Vn_tables)
	).

vn__lval_to_vnlval(Lval, Vnlval, Vn_tables0, Vn_tables) :-
	vn__no_heap_lval_to_vnlval(Lval, MaybeVnlval),
	( MaybeVnlval = yes(VnlvalPrime) ->
		Vnlval = VnlvalPrime,
		Vn_tables = Vn_tables0
	; Lval = field(Tag, Rval1, Rval2) ->
		vn__rval_to_vn(Rval1, Vn1, Vn_tables0, Vn_tables1),
		vn__rval_to_vn(Rval2, Vn2, Vn_tables1, Vn_tables),
		Vnlval = vn_field(Tag, Vn1, Vn2)
	;
		error("unexpected lval in vn__lval_to_vnlval")
	).

:- pred vn__no_heap_lval_to_vnlval(lval, maybe(vnlval)).
:- mode vn__no_heap_lval_to_vnlval(in, out) is det.

% If you to add to this list to fix a determinism error,
% check vn__lval_to_vnlval above as well.

vn__no_heap_lval_to_vnlval(reg(Reg),	yes(vn_reg(Reg))).
vn__no_heap_lval_to_vnlval(stackvar(N),	yes(vn_stackvar(N))).
vn__no_heap_lval_to_vnlval(framevar(N),	yes(vn_framevar(N))).
vn__no_heap_lval_to_vnlval(succip,	yes(vn_succip)).
vn__no_heap_lval_to_vnlval(maxfr,	yes(vn_maxfr)).
vn__no_heap_lval_to_vnlval(curredoip,	yes(vn_curredoip)).
vn__no_heap_lval_to_vnlval(hp,		yes(vn_hp)).
vn__no_heap_lval_to_vnlval(sp,		yes(vn_sp)).
vn__no_heap_lval_to_vnlval(field(_, _, _), no).
vn__no_heap_lval_to_vnlval(temp(N),	yes(vn_temp(N))).
vn__no_heap_lval_to_vnlval(lvar(_Var), _) :-
	error("lvar detected in value_number").

:- pred vn__lval_access_rval(lval, list(rval)).
:- mode vn__lval_access_rval(in, out) is det.

vn__lval_access_rval(reg(_), []).
vn__lval_access_rval(stackvar(_), []).
vn__lval_access_rval(framevar(_), []).
vn__lval_access_rval(succip, []).
vn__lval_access_rval(maxfr, []).
vn__lval_access_rval(curredoip, []).
vn__lval_access_rval(hp, []).
vn__lval_access_rval(sp, []).
vn__lval_access_rval(field(_, Rval1, Rval2), [Rval1, Rval2]).
vn__lval_access_rval(temp(_), []).
vn__lval_access_rval(lvar(_), _) :-
	error("lvar detected in value_number").

vn__vnlval_access_vn(vn_reg(_), []).
vn__vnlval_access_vn(vn_stackvar(_), []).
vn__vnlval_access_vn(vn_framevar(_), []).
vn__vnlval_access_vn(vn_succip, []).
vn__vnlval_access_vn(vn_maxfr, []).
vn__vnlval_access_vn(vn_curredoip, []).
vn__vnlval_access_vn(vn_hp, []).
vn__vnlval_access_vn(vn_sp, []).
vn__vnlval_access_vn(vn_field(_, Vn1, Vn2), [Vn1, Vn2]).
vn__vnlval_access_vn(vn_temp(_), []).

vn__is_const_expr(Vn, Vn_tables) :-
	vn__lookup_defn(Vn, Vnrval, Vn_tables),
	( Vnrval = vn_const(_) ->
		true
	; Vnrval = vn_mkword(_, Vn1) ->
		vn__is_const_expr(Vn1, Vn_tables)
	; Vnrval = vn_unop(_, Vn1) ->
		vn__is_const_expr(Vn1, Vn_tables)
	; Vnrval = vn_binop(_, Vn1, Vn2) ->
		vn__is_const_expr(Vn1, Vn_tables),
		vn__is_const_expr(Vn2, Vn_tables)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule dealing with simple vn_table manipulations.

:- interface.

% The definitions of the types are exported only for debugging.

:- type lval_to_vn_table == map(vnlval, vn).
:- type rval_to_vn_table == map(vnrval, vn).
:- type vn_to_rval_table == map(vn, vnrval).
:- type vn_to_uses_table == map(vn, list(vn_src)).
:- type vn_to_locs_table == map(vn, list(vnlval)).
:- type loc_to_vn_table  == map(vnlval, vn).

:- type vn_tables --->	vn_tables(vn,
				lval_to_vn_table, rval_to_vn_table,
				vn_to_rval_table, vn_to_uses_table,
				vn_to_locs_table, loc_to_vn_table).

:- pred vn__init_tables(vn_tables).
:- mode vn__init_tables(out) is det.

:- pred vn__lookup_desired_value(vnlval, vn, vn_tables).
:- mode vn__lookup_desired_value(in, out, in) is det.

:- pred vn__lookup_assigned_vn(vnrval, vn, vn_tables).
:- mode vn__lookup_assigned_vn(in, out, in) is det.

:- pred vn__lookup_defn(vn, vnrval, vn_tables).
:- mode vn__lookup_defn(in, out, in) is det.

:- pred vn__lookup_uses(vn, list(vn_src), vn_tables).
:- mode vn__lookup_uses(in, out, in) is det.

:- pred vn__lookup_current_locs(vn, list(vnlval), vn_tables).
:- mode vn__lookup_current_locs(in, out, in) is det.

:- pred vn__lookup_current_value(vnlval, vn, vn_tables).
:- mode vn__lookup_current_value(in, out, in) is det.

:- pred vn__search_desired_value(vnlval, vn, vn_tables).
:- mode vn__search_desired_value(in, out, in) is semidet.

:- pred vn__search_assigned_vn(vnrval, vn, vn_tables).
:- mode vn__search_assigned_vn(in, out, in) is semidet.

:- pred vn__search_definition(vn, vnrval, vn_tables).
:- mode vn__search_definition(in, out, in) is semidet.

:- pred vn__search_uses(vn, list(vn_src), vn_tables).
:- mode vn__search_uses(in, out, in) is semidet.

:- pred vn__search_current_locs(vn, list(vnlval), vn_tables).
:- mode vn__search_current_locs(in, out, in) is semidet.

:- pred vn__search_current_value(vnlval, vn, vn_tables).
:- mode vn__search_current_value(in, out, in) is semidet.

:- pred vn__get_vnlval_vn_list(vn_tables, assoc_list(vnlval, vn)).
:- mode vn__get_vnlval_vn_list(in, out) is det.

:- pred vn__add_new_use(vn, vn_src, vn_tables, vn_tables).
:- mode vn__add_new_use(in, in, di, uo) is det.

:- pred vn__del_old_use(vn, vn_src, vn_tables, vn_tables).
:- mode vn__del_old_use(in, in, di, uo) is det.

:- pred vn__set_current_value(vnlval, vn, vn_tables, vn_tables).
:- mode vn__set_current_value(in, in, di, uo) is det.

:- pred vn__set_desired_value(vnlval, vn, vn_tables, vn_tables).
:- mode vn__set_desired_value(in, in, di, uo) is det.

:- pred vn__record_first_vnrval(vnrval, vn, vn_tables, vn_tables).
:- mode vn__record_first_vnrval(in, out, di, uo) is det.

:- pred vn__record_first_vnlval(vnlval, vn, vn_tables, vn_tables).
:- mode vn__record_first_vnlval(in, out, di, uo) is det.

:- implementation.

vn__init_tables(Vn_tables) :-
	map__init(Lval_to_vn_table0),
	map__init(Rval_to_vn_table0),
	map__init(Vn_to_rval_table0),
	map__init(Vn_to_uses_table0),
	map__init(Vn_to_locs_table0),
	map__init(Loc_to_vn_table0),
	Vn_tables = vn_tables(0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__lookup_desired_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Lval_to_vn_table, Vnlval, Vn).

vn__lookup_assigned_vn(Vnrval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Rval_to_vn_table, Vnrval, Vn).

vn__lookup_defn(Vn, Vnrval, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Vn_to_rval_table, Vn, Vnrval).

vn__lookup_uses(Vn, Uses, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Vn_to_uses_table, Vn, Uses).

vn__lookup_current_locs(Vn, Locs, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Vn_to_locs_table, Vn, Locs).

vn__lookup_current_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, Loc_to_vn_table),
	map__lookup(Loc_to_vn_table, Vnlval, Vn).

vn__search_desired_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Lval_to_vn_table, Vnlval, Vn).

vn__search_assigned_vn(Vnrval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Rval_to_vn_table, Vnrval, Vn).

vn__search_definition(Vn, Vnrval, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Vn_to_rval_table, Vn, Vnrval).

vn__search_uses(Vn, Uses, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Vn_to_uses_table, Vn, Uses).

vn__search_current_locs(Vn, Locs, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Vn_to_locs_table, Vn, Locs).

vn__search_current_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_NextVn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, Loc_to_vn_table),
	map__search(Loc_to_vn_table, Vnlval, Vn).

vn__get_vnlval_vn_list(Vn_tables, Lval_vn_list) :-
	Vn_tables = vn_tables(_NextVn,
		Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__to_assoc_list(Lval_to_vn_table, Lval_vn_list).

%-----------------------------------------------------------------------------%

vn__add_new_use(Vn, NewUse, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Uses0),
	( list__member(NewUse, Uses0) ->
		error("new use already known")
	;
		Uses1 = [NewUse | Uses0]
	),
	map__set(Vn_to_uses_table0, Vn, Uses1, Vn_to_uses_table1),
	Vn_tables = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__del_old_use(Vn, OldUse, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Uses0),
	( list__member(OldUse, Uses0) ->
		list__delete_all(Uses0, OldUse, Uses1)
	;
		error("old use not known")
	),
	map__set(Vn_to_uses_table0, Vn, Uses1, Vn_to_uses_table1),
	Vn_tables = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__set_current_value(Vnlval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(NextVn0,
		Lval_to_vn_table0,  Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	% change the forward mapping
	map__lookup(Loc_to_vn_table0, Vnlval, Old_vn),
	map__set(Loc_to_vn_table0, Vnlval, Vn, Loc_to_vn_table1),
	% change the reverse mapping
	map__lookup(Vn_to_locs_table0, Old_vn, Old_locs0),
	list__delete_all(Old_locs0, Vnlval, Old_locs1),
	map__set(Vn_to_locs_table0, Old_vn, Old_locs1, Vn_to_locs_table1),
	Vn_tables = vn_tables(NextVn0,
		Lval_to_vn_table0,  Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table1, Loc_to_vn_table1).

vn__set_desired_value(Vnlval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__set(Lval_to_vn_table0, Vnlval, Vn, Lval_to_vn_table1),
	Vn_tables = vn_tables(NextVn0,
		Lval_to_vn_table1, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__record_first_vnrval(Vnrval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = NextVn0,
	NextVn1 is NextVn0 + 1,
	map__set(Rval_to_vn_table0, Vnrval, Vn, Rval_to_vn_table1),
	map__set(Vn_to_rval_table0, Vn, Vnrval, Vn_to_rval_table1),
	map__set(Vn_to_uses_table0, Vn, [], Vn_to_uses_table1),
	Vn_tables = vn_tables(NextVn1,
		Lval_to_vn_table0, Rval_to_vn_table1,
		Vn_to_rval_table1, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__record_first_vnlval(Vnlval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = NextVn0,
	NextVn is NextVn0 + 1,
	map__set(Lval_to_vn_table0, Vnlval, Vn, Lval_to_vn_table),
	map__set(Rval_to_vn_table0, vn_origlval(Vnlval), Vn, Rval_to_vn_table),
	map__set(Vn_to_rval_table0, Vn, vn_origlval(Vnlval), Vn_to_rval_table),
	map__set(Vn_to_uses_table0, Vn, [], Vn_to_uses_table),
	map__set(Vn_to_locs_table0, Vn, [Vnlval], Vn_to_locs_table),
	map__set(Loc_to_vn_table0,  Vnlval, Vn, Loc_to_vn_table),
	Vn_tables = vn_tables(NextVn,
		Lval_to_vn_table, Rval_to_vn_table,
		Vn_to_rval_table, Vn_to_uses_table,
		Vn_to_locs_table, Loc_to_vn_table).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

:- pred vn__build_counts(vnlvalset, ctrlmap, vn_tables, vn_tables).
:- mode vn__build_counts(in, in, di, uo) is det.

:- implementation.

vn__build_counts(Livevals, Ctrlmap, Vn_tables0, Vn_tables) :-
	vn__build_counts_from_ctrl(0, Ctrlmap, Vn_tables0, Vn_tables1),
	bintree_set__to_sorted_list(Livevals, Livelist),
	vn__build_counts_from_livevals(Livelist, Vn_tables1, Vn_tables).

:- pred vn__build_counts_from_ctrl(int, ctrlmap, vn_tables, vn_tables).
:- mode vn__build_counts_from_ctrl(in, in, di, uo) is det.

vn__build_counts_from_ctrl(Ctrl, Ctrlmap, Vn_tables0, Vn_tables) :-
	( map__search(Ctrlmap, Ctrl, VnInstr) ->
		(
			VnInstr = vn_call(_, _, _),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_call_closure(_, _, _),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_mkframe(_, _, _),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_modframe(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_label(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_goto(_),
			Vn_tables1 = Vn_tables0
		;
			VnInstr = vn_computed_goto(Vn, _),
			vn__record_use(Vn, src_ctrl(Ctrl),
				Vn_tables0, Vn_tables1)
		;
			VnInstr = vn_if_val(Vn, _),
			vn__record_use(Vn, src_ctrl(Ctrl),
				Vn_tables0, Vn_tables1)
		),
		NextCtrl is Ctrl + 1,
		vn__build_counts_from_ctrl(NextCtrl, Ctrlmap,
			Vn_tables1, Vn_tables)
	;
		Vn_tables = Vn_tables0
	).

	% We have to record two kinds of uses. The first is the use of the
	% value number we want to assign to the vnlval. The second is the
	% value numbers needed to access the vnlval at all.

:- pred vn__build_counts_from_livevals(list(vnlval), vn_tables, vn_tables).
:- mode vn__build_counts_from_livevals(in, di, uo) is det.

vn__build_counts_from_livevals([], Vn_tables, Vn_tables).
vn__build_counts_from_livevals([Live | Liveslist], Vn_tables0, Vn_tables) :-
	vn__lookup_desired_value(Live, Vn, Vn_tables0),
	vn__record_use(Vn, src_liveval(Live), Vn_tables0, Vn_tables1),
	vn__vnlval_access_vn(Live, SubVns),
	vn__record_use_list(SubVns, src_liveval(Live),
		Vn_tables1, Vn_tables2),
	vn__build_counts_from_livevals(Liveslist, Vn_tables2, Vn_tables).

:- pred vn__record_use(vn, vn_src, vn_tables, vn_tables).
:- mode vn__record_use(in, in, di, uo) is det.

vn__record_use(Vn, Src, Vn_tables0, Vn_tables) :-
	vn__lookup_uses(Vn, OldUses, Vn_tables0),
	vn__add_new_use(Vn, Src, Vn_tables0, Vn_tables1),
	( OldUses = [] ->
		vn__lookup_defn(Vn, Vnrval, Vn_tables1),
		(
			Vnrval = vn_origlval(Vnlval),
			vn__vnlval_access_vn(Vnlval, SubVns),
			vn__record_use_list(SubVns, src_vn(Vn),
				Vn_tables1, Vn_tables)
		;
			Vnrval = vn_mkword(_, SubVn),
			vn__record_use(SubVn, src_vn(Vn),
				Vn_tables1, Vn_tables)
		;
			Vnrval = vn_const(_),
			Vn_tables = Vn_tables1
		;
			Vnrval = vn_create(_, _, _),
			Vn_tables = Vn_tables1
		;
			Vnrval = vn_unop(_, SubVn),
			vn__record_use(SubVn, src_vn(Vn),
				Vn_tables1, Vn_tables)
		;
			Vnrval = vn_binop(_, SubVn1, SubVn2),
			vn__record_use(SubVn1, src_vn(Vn),
				Vn_tables1, Vn_tables2),
			vn__record_use(SubVn2, src_vn(Vn),
				Vn_tables2, Vn_tables)
		)
	;
		Vn_tables = Vn_tables1
	).

:- pred vn__record_use_list(list(vn), vn_src, vn_tables, vn_tables).
:- mode vn__record_use_list(in, in, di, uo) is det.

vn__record_use_list([], _Src, Vn_tables, Vn_tables).
vn__record_use_list([Vn | Vns], Src, Vn_tables0, Vn_tables) :-
	vn__record_use(Vn, Src, Vn_tables0, Vn_tables1),
	vn__record_use_list(Vns, Src, Vn_tables1, Vn_tables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
