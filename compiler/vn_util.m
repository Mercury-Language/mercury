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
		Rval = var(_),
		error("var rval should not propagate to value_number")
	;
		Rval = create(_, _, _),
		Livevals = Livevals0
	;
		Rval = heap_alloc(Rval1),
		vn__make_live_lval(hp, Livevals0, Livevals1),
		vn__make_live(Rval1, Livevals1, Livevals)
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
	).

:- pred vn__make_live_lval(lval, lvalset, lvalset).
:- mode vn__make_live_lval(in, di, uo) is det.

vn__make_live_lval(Lval, Livevals0, Livevals) :-
	bintree_set__insert(Livevals0, Lval, Livevals1),
	( Lval = field(_, Rval1, Rval2) ->
		vn__make_live(Rval1, Livevals1, Livevals2),
		vn__make_live(Rval2, Livevals2, Livevals)
	;
		Livevals = Livevals1
	).

vn__make_dead(Lval, Livevals0, Livevals) :-
	bintree_set__delete(Livevals0, Lval, Livevals).

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
	( vn__no_heap_lval_to_vnlval(Lval, Vnlval) ->
		bintree_set__insert(Liveset0, Vnlval, Liveset1)
	;
		Liveset1 = Liveset0
	),
	vn__convert_to_vnlval_and_insert(Lvals, Liveset1, Liveset).

vn__handle_assign(Lval, Rval, Vn_tables0, Vn_tables) :-
	vn__use_rval_find_vn(Rval, Vn, Vn_tables0, Vn_tables1),
	vn__lval_to_vnlval(Lval, yes, Vnlval, Vn_tables1, Vn_tables2),
	( vn__search_desired_value(Vnlval, Old_vn, Vn_tables2) ->
		vn__deep_unuse_vn(Old_vn, Vn_tables2, Vn_tables3)
	;
		Vn_tables3 = Vn_tables2
	),
	vn__set_desired_value(Vnlval, Vn, Vn_tables3, Vn_tables).

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

	% Make a location available for reuse as a temporary location.

:- pred vn__reuse_temploc(vnlval, templocs, templocs).
:- mode vn__reuse_temploc(in, di, uo) is det.

	% Give the number of the highest temp variable used.

:- pred vn__max_temploc(templocs, int).
:- mode vn__max_temploc(in, out) is det.

:- implementation.

:- type templocs == pair(list(vnlval), int).

vn__init_templocs(MaxTemp, MaxReg, Livevals, Templocs) :-
	vn__get_n_temps(1, MaxTemp, Temps),
	vn__find_free_regs(1, MaxReg, Livevals, Regs),
	NextTemp is MaxTemp + 1,
	list__append(Regs, Temps, Queue),
	Templocs = Queue - NextTemp.

vn__next_temploc(Queue0 - Next0, Queue1 - Next1, Vnlval) :-
	( Queue0 = [Head | Tail] ->
		Vnlval = Head,
		Queue1 = Tail,
		Next1 = Next0
	;
		Vnlval = vn_temp(Next0),
		Queue1 = Queue0,
		Next1 is Next0 + 1
	).

vn__no_temploc(Vnlval, Queue0 - Next, Queue - Next) :-
	list__delete_all(Queue0, Vnlval, Queue).

vn__reuse_temploc(Vnlval, Queue0 - Next, Queue - Next) :-
	list__append(Queue0, [Vnlval], Queue).

vn__max_temploc(_Queue0 - Next, Max) :-
	Max is Next - 1.

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
	( vn__no_heap_lval_to_vnlval(Lval, Vnlval) ->
		Vn_tables0 = vn_tables(_Next_vn,
			Lval_to_vn_table, _Rval_to_vn_table,
			_Vn_to_rval_table, _Vn_to_uses_table,
			_Vn_to_locs_table, _Loc_to_vn_table),
		( map__search(Lval_to_vn_table, Vnlval, VnPrime) ->
			Vn = VnPrime,
			Vn_tables1 = Vn_tables0
		;
			vn__record_first_vnlval(Vnlval, Vn,
				Vn_tables0, Vn_tables1)
		),
		map__set(FlushEntry0, Vnlval, Vn, FlushEntry1),
		bintree_set__insert(Livevals0, Vnlval, Livevals1)
	;
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
	Vn_tables = vn_tables(_Next_vn,
		Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__to_assoc_list(Lval_to_vn_table, Lval_vn_list),
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

	% Submodule for keeping only live nodes in the vn table.

:- interface.

:- pred vn__keep_live_nodes(list(vnlval), vn_tables, vn_tables).
:- mode vn__keep_live_nodes(in, di, uo) is det.

:- implementation.

vn__keep_live_nodes(Live_lvals, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__keys(Lval_to_vn_table0, All_lvals),
	vn__find_dead_nodes(All_lvals, Live_lvals, Dead_lvals),
	vn__remove_dead_nodes(Dead_lvals, Vn_tables0, Vn_tables).

:- pred vn__find_dead_nodes(list(vnlval), list(vnlval), list(vnlval)).
:- mode vn__find_dead_nodes(in, in, out) is det.

vn__find_dead_nodes([], _, []).
vn__find_dead_nodes([Vnlval | Vnlvals], Live, Dead) :-
	vn__find_dead_nodes(Vnlvals, Live, Dead0),
	(
		( Vnlval = vn_reg(_)
		; Vnlval = vn_stackvar(_)
		; Vnlval = vn_framevar(_)
		)
	->
		( list__member(Vnlval, Live) ->
			Dead = Dead0
		;
			Dead = [Vnlval | Dead0]
		)
	;
		Dead = Dead0
	).

:- pred vn__remove_dead_nodes(list(vnlval), vn_tables, vn_tables).
:- mode vn__remove_dead_nodes(in, di, uo) is det.

vn__remove_dead_nodes([], Vn_tables, Vn_tables).
vn__remove_dead_nodes([Lval | Lvals], Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		Lval_to_vn_table0, _Rval_to_vn_table0,
		_Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	map__lookup(Lval_to_vn_table0, Lval, Vn),
	vn__deep_unuse_vn(Vn, Vn_tables0, Vn_tables1),
	vn__remove_dead_nodes(Lvals, Vn_tables1, Vn_tables).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Submodule for keeping track of use counts.

:- interface.

:- pred vn__use_vn(vn, vn_tables, vn_tables).
:- mode vn__use_vn(in, di, uo) is det.

:- pred vn__deep_unuse_vn(vn, vn_tables, vn_tables).
:- mode vn__deep_unuse_vn(in, di, uo) is det.

:- pred vn__deep_unuse_vns(list(vn), vn_tables, vn_tables).
:- mode vn__deep_unuse_vns(in, di, uo) is det.

:- pred vn__deep_unuse_vn_except(vn, list(vn), vn_tables, vn_tables).
:- mode vn__deep_unuse_vn_except(in, in, di, uo) is det.

:- pred vn__deep_unuse_vns_except(list(vn), list(vn), vn_tables, vn_tables).
:- mode vn__deep_unuse_vns_except(in, in, di, uo) is det.

:- implementation.

vn__use_vn(Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Count0),
	Count1 is Count0 + 1,
	map__set(Vn_to_uses_table0, Vn, Count1, Vn_to_uses_table1),
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__deep_unuse_vn(Vn, Vn_tables0, Vn_tables) :-
	vn__deep_unuse_vn_except(Vn, [], Vn_tables0, Vn_tables).

vn__deep_unuse_vns(Vns, Vn_tables0, Vn_tables) :-
	vn__deep_unuse_vns_except(Vns, [], Vn_tables0, Vn_tables).

vn__deep_unuse_vn_except(Vn, Except, Vn_tables0, Vn_tables) :-
	( list__member(Vn, Except) ->
		Vn_tables = Vn_tables0
	;
		Vn_tables0 = vn_tables(Next_vn0,
			Lval_to_vn_table0, Rval_to_vn_table0,
			Vn_to_rval_table0, Vn_to_uses_table0,
			Vn_to_locs_table0, Loc_to_vn_table0),
		map__lookup(Vn_to_uses_table0, Vn, Uses0),
		Uses is Uses0 - 1,
		map__set(Vn_to_uses_table0, Vn, Uses, Vn_to_uses_table1),
		Vn_tables1 = vn_tables(Next_vn0,
			Lval_to_vn_table0, Rval_to_vn_table0,
			Vn_to_rval_table0, Vn_to_uses_table1,
			Vn_to_locs_table0, Loc_to_vn_table0),
		( Uses > 0 ->
			Vn_tables = Vn_tables1
		;
			map__lookup(Vn_to_rval_table0, Vn, Vnrval),
			(
				Vnrval = vn_origlval(Vnlval),
				(Vnlval = vn_field(_Tag1, Sub_vn1, Sub_vn2) ->
					Sub_vns = [Sub_vn1, Sub_vn2]
				;
					Sub_vns = []
				)
			;
				Vnrval = vn_mkword(_Tag2, Sub_vn),
				Sub_vns = [Sub_vn]
			;
				Vnrval = vn_const(_Const),
				Sub_vns = []
			;
				Vnrval = vn_create(_Tag3, _Args, _Label),
				Sub_vns = []
			;
				Vnrval = vn_heap_alloc(Sub_vn),
				Sub_vns = [Sub_vn]
			;
				Vnrval = vn_unop(_Unop, Sub_vn),
				Sub_vns = [Sub_vn]
			;
				Vnrval = vn_binop(_Binop, Sub_vn1, Sub_vn2),
				Sub_vns = [Sub_vn1, Sub_vn2]
			),
			vn__deep_unuse_vns_except(Sub_vns, Except,
				Vn_tables1, Vn_tables)
		)
	).

vn__deep_unuse_vns_except([], _Except, Vn_tables, Vn_tables).
vn__deep_unuse_vns_except([Vn | Vns], Except, Vn_tables0, Vn_tables) :-
	vn__deep_unuse_vn_except(Vn, Except, Vn_tables0, Vn_tables1),
	vn__deep_unuse_vns_except(Vns, Except, Vn_tables1, Vn_tables).

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

	% Submodule dealing with simple vn_table manipulations.

:- interface.

:- pred vn__init_tables(vn_tables).
:- mode vn__init_tables(out) is det.

:- pred vn__lookup_desired_value(vnlval, vn, vn_tables).
:- mode vn__lookup_desired_value(in, out, in) is det.

:- pred vn__lookup_assigned_vn(vnrval, vn, vn_tables).
:- mode vn__lookup_assigned_vn(in, out, in) is det.

:- pred vn__lookup_definition(vn, vnrval, vn_tables).
:- mode vn__lookup_definition(in, out, in) is det.

:- pred vn__lookup_use_count(vn, int, vn_tables).
:- mode vn__lookup_use_count(in, out, in) is det.

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

:- pred vn__search_use_count(vn, int, vn_tables).
:- mode vn__search_use_count(in, out, in) is semidet.

:- pred vn__search_current_locs(vn, list(vnlval), vn_tables).
:- mode vn__search_current_locs(in, out, in) is semidet.

:- pred vn__search_current_value(vnlval, vn, vn_tables).
:- mode vn__search_current_value(in, out, in) is semidet.

:- pred vn__incr_use_count(vn, int, vn_tables, vn_tables).
:- mode vn__incr_use_count(in, out, di, uo) is det.

:- pred vn__decr_use_count(vn, int, vn_tables, vn_tables).
:- mode vn__decr_use_count(in, out, di, uo) is det.

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
	Vn_tables = vn_tables(_Next_vn,
		Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Lval_to_vn_table, Vnlval, Vn).

vn__lookup_assigned_vn(Vnrval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Rval_to_vn_table, Vnrval, Vn).

vn__lookup_definition(Vn, Vnrval, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Vn_to_rval_table, Vn, Vnrval).

vn__lookup_use_count(Vn, Uses, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Vn_to_uses_table, Vn, Uses).

vn__lookup_current_locs(Vn, Locs, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		Vn_to_locs_table, _Loc_to_vn_table),
	map__lookup(Vn_to_locs_table, Vn, Locs).

vn__lookup_current_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, Loc_to_vn_table),
	map__lookup(Loc_to_vn_table, Vnlval, Vn).

vn__search_desired_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Lval_to_vn_table, Vnlval, Vn).

vn__search_assigned_vn(Vnrval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Rval_to_vn_table, Vnrval, Vn).

vn__search_definition(Vn, Vnrval, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Vn_to_rval_table, Vn, Vnrval).

vn__search_use_count(Vn, Uses, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Vn_to_uses_table, Vn, Uses).

vn__search_current_locs(Vn, Locs, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		Vn_to_locs_table, _Loc_to_vn_table),
	map__search(Vn_to_locs_table, Vn, Locs).

vn__search_current_value(Vnlval, Vn, Vn_tables) :-
	Vn_tables = vn_tables(_Next_vn,
		_Lval_to_vn_table,  _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, Loc_to_vn_table),
	map__search(Loc_to_vn_table, Vnlval, Vn).

vn__incr_use_count(Vn, NewUses, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, OldUses),
	NewUses is OldUses + 1,
	map__set(Vn_to_uses_table0, Vn, NewUses, Vn_to_uses_table1),
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn__decr_use_count(Vn, NewUses, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, OldUses),
	NewUses is OldUses - 1,
	map__set(Vn_to_uses_table0, Vn, NewUses, Vn_to_uses_table1),
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

%-----------------------------------------------------------------------------%

% XXX

:- interface.

	% Convert an rval into a vnrval and hence into a vn,
	% counting it as a new use.

% from handle_instr
:- pred vn__use_rval_find_vn(rval, vn, vn_tables, vn_tables).
:- mode vn__use_rval_find_vn(in, out, di, uo) is det.

% from generate_assignment
:- pred vn__point_vnlval_to_vn(vnlval, vn, vn_tables, vn_tables).
:- mode vn__point_vnlval_to_vn(in, in, di, uo) is det.

% from maybe_save_prev_value
:- pred vn__is_const_expr(vn, vn_tables).
:- mode vn__is_const_expr(in, in) is semidet.

% from maybe_save_prev_value (Use = yes)
:- pred vn__lval_to_vnlval(lval, bool, vnlval, vn_tables, vn_tables).
:- mode vn__lval_to_vnlval(in, in, out, di, uo) is det.

	% Find out what vn, if any, is needed to access a vnlval.

:- pred vn__vnlval_access_vn(vnlval, maybe(vn)).
:- mode vn__vnlval_access_vn(in, out) is det.

:- implementation.

%-----------------------------------------------------------------------------%

vn__use_rval_find_vn(Rval, Vn, Vn_tables0, Vn_tables) :-
	vn__from_rval_find_vn(Rval, yes, Vn, Vn_tables0, Vn_tables).

:- pred vn__from_rval_find_vn(rval, bool, vn, vn_tables, vn_tables).
:- mode vn__from_rval_find_vn(in, in, out, di, uo) is det.

vn__from_rval_find_vn(Rval, Use, Vn, Vn_tables0, Vn_tables) :-
	(
		Rval = lval(Lval),
		vn__from_lval_find_vn(Lval, Use, Vn, Vn_tables0, Vn_tables)
	;
		Rval = var(_),
		error("value_number should never get rval: var")
	;
		Rval = create(Tag, Args, Label),
		vn__from_vnrval_find_vn(vn_create(Tag, Args, Label), Use,
			Vn, Vn_tables0, Vn_tables)
	;
		Rval = heap_alloc(Rval1),
		vn__from_rval_find_vn(Rval1, Use,
			Sub_vn, Vn_tables0, Vn_tables1),
		vn__from_vnrval_find_vn(vn_heap_alloc(Sub_vn), Use,
			Vn, Vn_tables1, Vn_tables)
	;
		Rval = mkword(Tag, Rval1),
		vn__from_rval_find_vn(Rval1, Use,
			Sub_vn, Vn_tables0, Vn_tables1),
		vn__from_vnrval_find_vn(vn_mkword(Tag, Sub_vn), Use,
			Vn, Vn_tables1, Vn_tables)
	;
		Rval = const(Const),
		vn__from_vnrval_find_vn(vn_const(Const), Use,
			Vn, Vn_tables0, Vn_tables)
	;
		Rval = unop(Unop, Rval1),
		vn__from_rval_find_vn(Rval1, Use,
			Sub_vn, Vn_tables0, Vn_tables1),
		vn__from_vnrval_find_vn(vn_unop(Unop, Sub_vn), Use,
			Vn, Vn_tables1, Vn_tables)
	;
		Rval = binop(Binop, Rval1, Rval2),
		vn__from_rval_find_vn(Rval1, Use,
			Sub_vn1, Vn_tables0, Vn_tables1),
		vn__from_rval_find_vn(Rval2, Use,
			Sub_vn2, Vn_tables1, Vn_tables2),
		vn__from_vnrval_find_vn(vn_binop(Binop, Sub_vn1, Sub_vn2), Use,
			Vn, Vn_tables2, Vn_tables)
	).

:- pred vn__from_vnrval_find_vn(vnrval, bool, vn, vn_tables, vn_tables).
:- mode vn__from_vnrval_find_vn(in, in, out, di, uo) is det.

vn__from_vnrval_find_vn(Vnrval, Use, Vn, Vn_tables0, Vn_tables) :-
	vn__simplify_vnrval(Vnrval, Use, Vnrval1, Vn_tables0, Vn_tables1),
	vn__lookup_vnrval(Vnrval1, Use, Vn, Vn_tables1, Vn_tables).

	% Simplify the vnrval by partially evaluating expressions involving
	% integer constants. To make this simpler, swap the arguments of
	% commutative expressions around to put the constants on the right
	% side.
	%
	% The simplification has to be done on vnrvals and not on rvals
	% even though this complicates the code. The reason is that an
	% expression such as r1 + 4 can be simplified if we know that
	% r1 was defined as r2 + 8.
	%
	% This code should decrement the use counts of value numbers that
	% the incoming vnrval refers to that the outgoing vnrval does not,
	% but this is not yet implemented. In any case, we probably don't need
	% accurate use counts on constants.

:- pred vn__simplify_vnrval(vnrval, bool, vnrval, vn_tables, vn_tables).
:- mode vn__simplify_vnrval(in, in, out, di, uo) is det.

vn__simplify_vnrval(Vnrval0, Use, Vnrval, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(_Next_vn0,
		_Lval_to_vn_table0, _Rval_to_vn_table0,
		Vn_to_rval_table0, _Vn_to_uses_table0,
		_Vn_to_locs_table0, _Loc_to_vn_table0),
	( Vnrval0 = vn_binop((+), Vn1, Vn2) ->
		map__lookup(Vn_to_rval_table0, Vn1, Vnrval1),
		map__lookup(Vn_to_rval_table0, Vn2, Vnrval2),
		( Vnrval1 = vn_const(int_const(I1)) ->
			( Vnrval2 = vn_const(int_const(I2)) ->
				I is I1 + I2,
				Vnrval = vn_const(int_const(I)),
				Vn_tables = Vn_tables0
			; Vnrval2 = vn_binop((+), Vn21, Vn22) ->
				map__lookup(Vn_to_rval_table0, Vn22, Vnrval22),
				( Vnrval22 = vn_const(int_const(I22)) ->
					I is I1 + I22,
					vn__deep_unuse_vn(Vn2, Vn_tables0, Vn_tables1),
					vn__from_vnrval_find_vn(
						vn_const(int_const(I)), Use,
						Vn_i, Vn_tables1, Vn_tables),
					Vnrval = vn_binop((+), Vn21, Vn_i)
				;
					Vnrval = vn_binop((+), Vn2, Vn1),
					Vn_tables = Vn_tables0
				)
			;
				Vnrval = vn_binop((+), Vn2, Vn1),
				Vn_tables = Vn_tables0
			)
		; Vnrval1 = vn_binop((+), Vn11, Vn12) ->
			map__lookup(Vn_to_rval_table0, Vn12, Vnrval12),
			( Vnrval12 = vn_const(int_const(I12)) ->
				( Vnrval2 = vn_const(int_const(I2)) ->
					I is I12 + I2,
					vn__deep_unuse_vn(Vn1, Vn_tables0, Vn_tables1),
					vn__from_vnrval_find_vn(
						vn_const(int_const(I)), Use,
						Vn_i, Vn_tables1, Vn_tables),
					Vnrval = vn_binop((+), Vn11, Vn_i)
				; Vnrval2 = vn_binop((+), Vn21, Vn22) ->
					map__lookup(Vn_to_rval_table0, Vn22, Vnrval22),
					( Vnrval22 = vn_const(int_const(I22)) ->
						I is I12 + I22,
						vn__deep_unuse_vn(Vn1, Vn_tables0, Vn_tables1),
						vn__deep_unuse_vn(Vn2, Vn_tables1, Vn_tables2),
						vn__from_vnrval_find_vn(
							vn_binop((+), Vn11, Vn21), Use,
							Vn_e, Vn_tables2, Vn_tables3),
						vn__from_vnrval_find_vn(
							vn_const(int_const(I)), Use,
							Vn_i, Vn_tables3, Vn_tables),
						Vnrval = vn_binop((+), Vn_e, Vn_i)
					;
						vn__deep_unuse_vn(Vn1, Vn_tables0, Vn_tables1),
						vn__from_vnrval_find_vn(
							vn_binop((+), Vn11, Vn2), Use,
							Vn_e, Vn_tables1, Vn_tables),
						Vnrval = vn_binop((+), Vn_e, Vn12)
					)
				;
					Vnrval = Vnrval0,
					Vn_tables = Vn_tables0
				)
			;
				Vnrval = vn_binop((+), Vn2, Vn1),
				Vn_tables = Vn_tables0
			)
		;
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	; Vnrval0 = vn_binop((-), Vn1, Vn2) ->
		map__lookup(Vn_to_rval_table0, Vn2, Vnrval2),
		( Vnrval2 = vn_const(int_const(I2)) ->
			NI2 is 0 - I2,
			vn__from_vnrval_find_vn(vn_const(int_const(NI2)),
				Use, Vn2prime, Vn_tables0, Vn_tables1),
			vn__simplify_vnrval(vn_binop((+), Vn1, Vn2prime),
				Use, Vnrval, Vn_tables1, Vn_tables)
		;
			% XXX more simplification opportunities exist
			Vnrval = Vnrval0,
			Vn_tables = Vn_tables0
		)
	;
		% XXX more simplification opportunities exist
		Vnrval = Vnrval0,
		Vn_tables = Vn_tables0
	).

:- pred vn__lookup_vnrval(vnrval, bool, vn, vn_tables, vn_tables).
:- mode vn__lookup_vnrval(in, in, out, di, uo) is det.

vn__lookup_vnrval(Vnrval, Use, Vn, Vn_tables0, Vn_tables) :-
	( vn__search_assigned_vn(Vnrval, Vn_prime, Vn_tables0) ->
		Vn = Vn_prime,
		( Use = yes ->
			vn__record_vn_use(Vn, Vn_tables0, Vn_tables)
		;
			Vn_tables = Vn_tables0
		)
	;
		( Use = yes ->
			vn__record_first_vnrval(Vnrval, Vn,
				Vn_tables0, Vn_tables)
		;
			error("first time for vn but use is not set")
		)
	).

:- pred vn__from_lval_find_vn(lval, bool, vn, vn_tables, vn_tables).
:- mode vn__from_lval_find_vn(in, in, out, di, uo) is det.

vn__from_lval_find_vn(Lval, Use, Vn, Vn_tables0, Vn_tables) :-
	vn__lval_to_vnlval(Lval, Use, Vnlval, Vn_tables0, Vn_tables1),
	( vn__search_desired_value(Vnlval, Vn_prime, Vn_tables1) ->
		Vn = Vn_prime,
		( Use = yes ->
			vn__use_vn(Vn, Vn_tables1, Vn_tables)
		;
			Vn_tables = Vn_tables1
		)
	;
		( Use = yes ->
			vn__record_first_vnlval(Vnlval, Vn,
				Vn_tables1, Vn_tables)
		;
			error("first time for vn but use is not set")
		)
	).

vn__lval_to_vnlval(Lval, Use, Vnlval, Vn_tables0, Vn_tables) :-
	( vn__no_heap_lval_to_vnlval(Lval, VnlvalPrime) ->
		Vnlval = VnlvalPrime,
		Vn_tables = Vn_tables0
	; Lval = field(Tag, Rval1, Rval2) ->
		vn__from_rval_find_vn(Rval1, Use, Vn1, Vn_tables0, Vn_tables1),
		vn__from_rval_find_vn(Rval2, Use, Vn2, Vn_tables1, Vn_tables),
		Vnlval = vn_field(Tag, Vn1, Vn2)
	;
		error("unexpected lval in vn__lval_to_vnlval")
	).

:- pred vn__no_heap_lval_to_vnlval(lval, vnlval).
:- mode vn__no_heap_lval_to_vnlval(in, out) is semidet.

vn__no_heap_lval_to_vnlval(Lval, Vnlval) :-
	(
		Lval = reg(Reg),
		Vnlval = vn_reg(Reg)
	;
		Lval = succip,
		Vnlval = vn_succip
	;
		Lval = maxfr,
		Vnlval = vn_maxfr
	;
		Lval = curredoip,
		Vnlval = vn_curredoip
	;
		Lval = hp,
		Vnlval = vn_hp
	;
		Lval = sp,
		Vnlval = vn_sp
	;
		Lval = stackvar(Slot),
		Vnlval = vn_stackvar(Slot)
	;
		Lval = framevar(Slot),
		Vnlval = vn_framevar(Slot)
	;
		Lval = temp(No),
		Vnlval = vn_temp(No)
	;
		Lval = field(_, _, _),
		fail
	;
		Lval = lvar(_Var),
		error("lvar detected in value_number")
	).


vn__vnlval_access_vn(vn_reg(_), no).
vn__vnlval_access_vn(vn_stackvar(_), no).
vn__vnlval_access_vn(vn_framevar(_), no).
vn__vnlval_access_vn(vn_succip, no).
vn__vnlval_access_vn(vn_maxfr, no).
vn__vnlval_access_vn(vn_curredoip, no).
vn__vnlval_access_vn(vn_hp, no).
vn__vnlval_access_vn(vn_sp, no).
vn__vnlval_access_vn(vn_field(_, Vn, _), yes(Vn)).
vn__vnlval_access_vn(vn_temp(_), no).

vn__point_vnlval_to_vn(Vnlval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
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
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table0,  Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table1, Loc_to_vn_table1).

vn__is_const_expr(Vn, Vn_tables) :-
	vn__lookup_definition(Vn, Vnrval, Vn_tables),
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

:- pred vn__set_desired_value(vnlval, vn, vn_tables, vn_tables).
:- mode vn__set_desired_value(in, in, di, uo) is det.

vn__set_desired_value(Vnlval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__set(Lval_to_vn_table0, Vnlval, Vn, Lval_to_vn_table1),
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table1, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0).

:- pred vn__record_vn_use(vn, vn_tables, vn_tables).
:- mode vn__record_vn_use(in, di, uo) is det.

vn__record_vn_use(Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	map__lookup(Vn_to_uses_table0, Vn, Usecount),
	Usecount1 is Usecount + 1,
	map__set(Vn_to_uses_table0, Vn, Usecount1, Vn_to_uses_table1),
	Vn_tables = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

:- pred vn__record_first_vnrval(vnrval, vn, vn_tables, vn_tables).
:- mode vn__record_first_vnrval(in, out, di, uo) is det.

vn__record_first_vnrval(Vnrval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = Next_vn0,
	Next_vn1 is Next_vn0 + 1,
	map__set(Rval_to_vn_table0, Vnrval, Vn, Rval_to_vn_table1),
	map__set(Vn_to_rval_table0, Vn, Vnrval, Vn_to_rval_table1),
	map__set(Vn_to_uses_table0, Vn, 1, Vn_to_uses_table1),
	Vn_tables = vn_tables(Next_vn1,
		Lval_to_vn_table0, Rval_to_vn_table1,
		Vn_to_rval_table1, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

:- pred vn__record_first_vnlval(vnlval, vn, vn_tables, vn_tables).
:- mode vn__record_first_vnlval(in, out, di, uo) is det.

vn__record_first_vnlval(Vnlval, Vn, Vn_tables0, Vn_tables) :-
	Vn_tables0 = vn_tables(Next_vn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = Next_vn0,
	Next_vn is Next_vn0 + 1,
	map__set(Lval_to_vn_table0, Vnlval, Vn, Lval_to_vn_table),
	map__set(Rval_to_vn_table0, vn_origlval(Vnlval), Vn, Rval_to_vn_table),
	map__set(Vn_to_rval_table0, Vn, vn_origlval(Vnlval), Vn_to_rval_table),
	map__set(Vn_to_uses_table0, Vn, 1, Vn_to_uses_table),
	map__set(Vn_to_locs_table0, Vn, [Vnlval], Vn_to_locs_table),
	map__set(Loc_to_vn_table0,  Vnlval, Vn, Loc_to_vn_table),
	Vn_tables = vn_tables(Next_vn,
		Lval_to_vn_table, Rval_to_vn_table,
		Vn_to_rval_table, Vn_to_uses_table,
		Vn_to_locs_table, Loc_to_vn_table).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
