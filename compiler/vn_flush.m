%-----------------------------------------------------------------------------%

% Vn_flush.nl - flush the nodes of the vn graph in order.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_flush.

:- interface.

:- import_module vn_type, vn_table, vn_temploc.
:- import_module llds, list.

	% Flush the given nodes in the given order.

:- pred vn__flush_nodelist(list(vn_node), ctrlmap,
	vn_tables, vn_tables, templocs, templocs, list(instruction)).
:- mode vn__flush_nodelist(in, in, di, uo, di, uo, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module vn_table, vn_util, vn_debug, opt_debug.
:- import_module map, int, string, require, std_util.

vn__flush_nodelist([], _Ctrlmap, Vn_tables, Vn_tables, Templocs, Templocs, []).
vn__flush_nodelist([Node0 | Nodes0], Ctrlmap, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	( Node0 = node_origlval(_) ->
		Nodes1 = Nodes0,
		Vn_tables1 = Vn_tables0,
		Templocs1 = Templocs0,
		Instrs0 = []
	;
		vn__flush_node(Node0, Ctrlmap, Nodes0, Nodes1,
			Vn_tables0, Vn_tables1, Templocs0, Templocs1, Instrs0)
	),
	vn__flush_nodelist(Nodes1, Ctrlmap, Vn_tables1, Vn_tables,
		Templocs1, Templocs, Instrs1),
	list__append(Instrs0, Instrs1, Instrs).

	% Flush the given node.

:- pred vn__flush_node(vn_node, ctrlmap, list(vn_node), list(vn_node),
	vn_tables, vn_tables, templocs, templocs, list(instruction)).
:- mode vn__flush_node(in, in, di, uo, di, uo, di, uo, out) is det.

vn__flush_node(Node, Ctrlmap, Nodes0, Nodes, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	vn__flush_start_msg(Node),

	(
		Node = node_shared(Vn),
		vn__choose_best_loc(Vn, Vnlval, Vn_tables0,
			Templocs0, Templocs1),
		( vn__search_desired_value(Vnlval, Vn, Vn_tables0) ->
			vn__flush_also_msg(Vnlval),
			list__delete_all(Nodes0, node_lval(Vnlval), Nodes)
		;
			Nodes = Nodes0
		),
		% vn__vnlval_access_vns(Vnlval, AccessVns),
		% vn__compensate_for_access_vns(AccessVns, Vnlval,
		% 	Vn_tables0, Vn_tables1),
		vn__ensure_assignment(Vnlval, Vn,
			Vn_tables0, Vn_tables, Templocs1, Templocs, Instrs)
	;
		Node = node_lval(Vnlval),
		Nodes = Nodes0,
		vn__lookup_desired_value(Vnlval, Vn, Vn_tables0),
		vn__ensure_assignment(Vnlval, Vn,
			Vn_tables0, Vn_tables, Templocs0, Templocs, Instrs)
	;
		Node = node_origlval(Vnlval),
		Nodes = Nodes0,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Node = node_ctrl(N),
		Nodes = Nodes0,
		map__lookup(Ctrlmap, N, Vn_instr),
		(
			Vn_instr = vn_call(ProcAddr, RetAddr, LiveInfo),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [call(ProcAddr, RetAddr, LiveInfo) - ""]
		;
			Vn_instr = vn_call_closure(ClAddr, RetAddr, LiveInfo),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [call_closure(ClAddr, RetAddr, LiveInfo) - ""]
		;
			Vn_instr = vn_mkframe(Name, Size, Redoip),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [mkframe(Name, Size, Redoip) - ""]
		;
			Vn_instr = vn_modframe(Redoip),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [modframe(Redoip) - ""]
		;
			Vn_instr = vn_label(Label),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [goto(label(Label)) - "eliminated by postopt"]
		;
			Vn_instr = vn_goto(TargetAddr),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [goto(TargetAddr) - ""]
		;
			Vn_instr = vn_computed_goto(Vn, Labels),
			vn__flush_vn(Vn, [src_ctrl(N)], Rval,
				Vn_tables0, Vn_tables,
				Templocs0, Templocs, FlushInstrs),
			Instr = computed_goto(Rval, Labels) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		;
			Vn_instr = vn_if_val(Vn, TargetAddr),
			vn__flush_vn(Vn, [src_ctrl(N)], Rval,
				Vn_tables0, Vn_tables,
				Templocs0, Templocs, FlushInstrs),
			Instr = if_val(Rval, TargetAddr) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		;
			Vn_instr = vn_mark_hp(Vnlval),
			vn__flush_access_path(Vnlval, [src_ctrl(N)], Lval,
				Vn_tables0, Vn_tables1,
				Templocs0, Templocs, FlushInstrs),
			vn__lookup_assigned_vn(vn_origlval(vn_hp), OldhpVn,
				Vn_tables1),
			vn__set_current_value(Vnlval, OldhpVn,
				Vn_tables1, Vn_tables),
			Instr = mark_hp(Lval) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		;
			Vn_instr = vn_restore_hp(Vn),
			vn__flush_vn(Vn, [src_ctrl(N)], Rval,
				Vn_tables0, Vn_tables1,
				Templocs0, Templocs, FlushInstrs),
			vn__set_current_value(vn_hp, Vn,
				Vn_tables1, Vn_tables),
			Instr = restore_hp(Rval) - "",
			list__append(FlushInstrs, [Instr], Instrs)
		;
			Vn_instr = vn_incr_sp(Incr),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [incr_sp(Incr) - ""]
		;
			Vn_instr = vn_decr_sp(Decr),
			Vn_tables = Vn_tables0,
			Templocs = Templocs0,
			Instrs = [decr_sp(Decr) - ""]
		)
	),

	vn__flush_end_msg(Instrs, Vn_tables).

%-----------------------------------------------------------------------------%

	% Choose a location for a shared value number that does not have to go
	% anywhere specific right now. We prefer to choose a register that
	% already has the value. If we cannot, we would like to choose from
	% among those locations that eventually want to have that value number,
	% but this may not be worthwhile if the contents of that location would
	% have to be saved first.

:- pred vn__choose_best_loc(vn, vnlval, vn_tables, templocs, templocs).
:- mode vn__choose_best_loc(in, out, in, di, uo) is det.

vn__choose_best_loc(Vn, Chosen, Vn_tables, Templocs0, Templocs) :-
	(
		vn__lookup_current_locs(Vn, CurrentLocs, Vn_tables),
		CurrentLocs = [_|_]
	->
		vn__choose_cheapest_loc(CurrentLocs, no, no, Presumed),
		(
			\+ Presumed = vn_reg(_),
			vn__lookup_uses(Vn, Uses, Vn_tables),
			list__delete_first(Uses, src_liveval(Presumed),
				NewUses),
			NewUses = [_,_|_]
		->
			vn__choose_best_user(Vn, Chosen, Vn_tables,
				Templocs0, Templocs)
		;
			Chosen = Presumed,
			Templocs = Templocs0
		)
	;
		vn__choose_best_user(Vn, Chosen, Vn_tables, Templocs0, Templocs)
	).

:- pred vn__choose_best_user(vn, vnlval, vn_tables, templocs, templocs).
:- mode vn__choose_best_user(in, out, in, di, uo) is det.

vn__choose_best_user(Vn, Chosen, Vn_tables, Templocs0, Templocs) :-
	(
		vn__find_cheap_users(Vn, Users, Vn_tables),
		Users = [_|_]
	->
		vn__choose_cheapest_loc(Users, no, no, Presumed),
		(
			% assign directly to a non-reg location
			% only if that is the only user of this vn
			\+ Presumed = vn_reg(_),
			vn__lookup_uses(Vn, Uses, Vn_tables),
			list__delete_first(Uses, src_liveval(Presumed),
				NewUses),
			NewUses = [_|_]
		->
			vn__next_temploc(Templocs0, Templocs, Chosen)
		;
			Chosen = Presumed,
			Templocs = Templocs0
		)
	;
		vn__next_temploc(Templocs0, Templocs, Chosen)
	).

	% Find a 'user', location that would like to have the given vn.
	% The user should be 'cheap', i.e. either it should hold no value
	% that will ever be used by anybody else, or the value it holds
	% should be assignable to one of its users without needing any
	% more assignments. At the moment we insist that this user be
	% a register. We could allow stack/frame variables as well,
	% but we must now allow fields, or in general any location
	% that needs access vns.

:- pred vn__find_cheap_users(vn, list(vnlval), vn_tables).
:- mode vn__find_cheap_users(in, out, in) is det.

vn__find_cheap_users(Vn, Vnlvals, Vn_tables) :-
	( vn__search_uses(Vn, Uses, Vn_tables) ->
		vn__find_cheap_users_2(Uses, Vnlvals, Vn_tables)
	;
		Vnlvals = []
	).

:- pred vn__find_cheap_users_2(list(vn_src), list(vnlval), vn_tables).
:- mode vn__find_cheap_users_2(in, out, in) is det.

vn__find_cheap_users_2([], [], _Vn_tables).
vn__find_cheap_users_2([Src | Srcs], Vnlvals, Vn_tables) :-
	vn__find_cheap_users_2(Srcs, Vnlvals0, Vn_tables),
	(
		Src = src_liveval(Live)
		% \+ Live = vn_field(_, _, _)
	->
		( vn__search_current_value(Live, Vn, Vn_tables) ->
			vn__lookup_uses(Vn, Uses, Vn_tables),
			(
				Uses = []
			->
				% Live's current value is not used.
				Vnlvals = [Live | Vnlvals0]
			;
				list__member(UserSrc, Uses),
				(
					UserSrc = src_liveval(User),
					vn__search_current_value(User, UserVn,
						Vn_tables)
				->
					User = vn_reg(_),
					vn__lookup_uses(UserVn, [], Vn_tables)
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

	% Choose the cheapest location from among those already holding
	% the desired vn. Therefore access time is the only consideration.

:- pred vn__choose_cheapest_loc(list(vnlval), maybe(vnlval), maybe(vnlval),
	vnlval).
:- mode vn__choose_cheapest_loc(in, in, in, out) is det.

vn__choose_cheapest_loc([Loc | Locs], Stack0, Heap0, BestLoc) :-
	(
		Loc = vn_reg(_),
		BestLoc = Loc
	;
		Loc = vn_stackvar(_),
		vn__choose_cheapest_loc(Locs, yes(Loc), Heap0, BestLoc)
	;
		Loc = vn_framevar(_),
		vn__choose_cheapest_loc(Locs, yes(Loc), Heap0, BestLoc)
	;
		Loc = vn_succip,
		BestLoc = Loc
	;
		Loc = vn_maxfr,
		BestLoc = Loc
	;
		Loc = vn_curredoip,
		vn__choose_cheapest_loc(Locs, yes(Loc), Heap0, BestLoc)
	;
		Loc = vn_hp,
		BestLoc = Loc
	;
		Loc = vn_sp,
		BestLoc = Loc
	;
		Loc = vn_field(_, _, _),
		vn__choose_cheapest_loc(Locs, Stack0, yes(Loc), BestLoc)
	;
		Loc = vn_temp(_),
		BestLoc = Loc
	).
vn__choose_cheapest_loc([], Stack0, Heap0, BestLoc) :-
	( Stack0 = yes(Stack) ->
		BestLoc = Stack
	; Heap0 = yes(Heap) ->
		BestLoc = Heap
	;
		error("empty locations list in vn__choose_cheapest_loc")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn__ensure_assignment(vnlval, vn, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__ensure_assignment(in, in, di, uo, di, uo, out) is det.

vn__ensure_assignment(Vnlval, Vn, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	(
		vn__search_current_value(Vnlval, Cur_vn, Vn_tables0),
		Vn = Cur_vn
	->
		vn__del_old_use(Vn, src_liveval(Vnlval),
			Vn_tables0, Vn_tables1),
		vn__vnlval_access_vns(Vnlval, SubVns),
		vn__del_old_uses(SubVns, src_access(Vnlval),
			Vn_tables1, Vn_tables),
		Templocs = Templocs0,
		Instrs = []
	;
		vn__generate_assignment(Vnlval, Vn, Vn_tables0, Vn_tables,
			Templocs0, Templocs, _, Instrs)
	).

:- pred vn__generate_assignment(vnlval, vn, vn_tables, vn_tables,
	templocs, templocs, lval, list(instruction)).
:- mode vn__generate_assignment(in, in, di, uo, di, uo, out, out) is det.

vn__generate_assignment(Vnlval, Vn, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Lval, Instrs) :-
	( Vnlval = vn_hp ->
		error("vn_hp should never need to be explicitly flushed")
		% It should be done by the first reference to the old value
		% of the heap pointer, which should generate an incr_hp.
	;
		true
	),
	% Only lvals on the heap must have their access path flushed,
	% but they cannot appear on the temploc list, so of the next
	% next two calls, at most one will modify temploc.
	( vn__search_current_value(Vnlval, OldVn0, Vn_tables0) ->
		SaveVn = yes(OldVn0)
	;
		SaveVn = no
	),
	vn__no_temploc(Vnlval, Templocs0, Templocs1),
	vn__flush_access_path(Vnlval, [src_access(Vnlval)], Lval,
		Vn_tables0, Vn_tables1, Templocs1, Templocs2, AccessInstrs),
	vn__flush_vn(Vn, [src_liveval(Vnlval)], Rval, Vn_tables1, Vn_tables2,
		Templocs2, Templocs3, FlushInstrs0),
	% The 'current' value of Vnlval may be changed by flush_vn if it
	% involves a reference to the old value of hp.
	(
		SaveVn = yes(OldVn),
		vn__find_lvals_in_rval(Rval, ForbiddenLvals),
		vn__maybe_save_prev_value(Vnlval, OldVn, ForbiddenLvals,
			Vn_tables2, Vn_tables3, Templocs3, Templocs, SaveInstrs)
	;
		SaveVn = no,
		Vn_tables3 = Vn_tables2,
		Templocs = Templocs3,
		SaveInstrs = []
	),
	( vn__search_current_value(Vnlval, Vn, Vn_tables3) ->
		% Flush_vn must perform the entire assignment if it involves
		% exactly the actions of an incr_hp operation. Since the
		% incr_hp in FlushInstrs overwrites Lval, we must perform it
		% after Lval's old value has been saved.
		vn__get_incr_hp(FlushInstrs0, Instr, FlushInstrs1),
		Vn_tables = Vn_tables3
	;
		vn__set_current_value(Vnlval, Vn, Vn_tables3, Vn_tables),
		Instr = assign(Lval, Rval) - "vn flush",
		FlushInstrs1 = FlushInstrs0
	),
	list__condense([AccessInstrs, FlushInstrs1, SaveInstrs, [Instr]],
		Instrs).

%-----------------------------------------------------------------------------%

:- pred vn__flush_vn(vn, list(vn_src), rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn(in, in, out, di, uo, di, uo, out) is det.

vn__flush_vn(Vn, Srcs, Rval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	( Srcs = [SrcPrime | _] ->
		Src = SrcPrime
	;
		error("empty source list in flush_vn")
	),
	vn__is_const_expr(Vn, IsConst, Vn_tables0),
	(
		IsConst = yes,
		vn__flush_vn_value(Vn, Srcs, Rval,
			Vn_tables0, Vn_tables3, Templocs0, Templocs, Instrs)
	;
		IsConst = no,
		vn__lookup_current_locs(Vn, Locs, Vn_tables0),
		( Locs = [] ->
			vn__lookup_uses(Vn, Uses, Vn_tables0),
			list__delete_all(Uses, Src, NewUses),
			% If there are no more uses, it is useless to assign
			% the vn to a location. Otherwise it is useful, but if
			% Src is a livevals, the assignment will be done by
			% the caller.
			(
				NewUses = [_|_],
				\+ Src = src_liveval(_)
			->
				vn__choose_best_loc(Vn, Vnlval, Vn_tables0,
					Templocs0, Templocs1),
				vn__generate_assignment(Vnlval, Vn,
					Vn_tables0, Vn_tables3,
					Templocs1, Templocs, Lval, Instrs),
				Rval = lval(Lval)
			;
				vn__flush_vn_value(Vn, Srcs, Rval,
					Vn_tables0, Vn_tables3,
					Templocs0, Templocs, Instrs)
			)
		;
			vn__choose_cheapest_loc(Locs, no, no, Loc),
			( Loc = vn_hp ->
				% The first reference to the old value of hp.
				vn__flush_old_hp(Srcs, Rval,
					Vn_tables0, Vn_tables3,
					Templocs0, Templocs, Instrs)
			;
				vn__flush_access_path(Loc, [src_vn(Vn) | Srcs],
					Lval, Vn_tables0, Vn_tables3,
					Templocs0, Templocs, Instrs),
				Rval = lval(Lval)
			)
		)
	),
	vn__del_old_use(Vn, Src, Vn_tables3, Vn_tables),
	true.
	% vn__lookup_uses(Vn, NewUses, Vn_tables),
	% ( NewUses = [_|_] ->
	% 	NewlyFree = NewlyFree0
	% ;
	% 	vn__lookup_current_locs(Vn, NewlyFree1, Vn_tables0)
	% 	list__append(NewlyFree0, NewlyFree1, NewlyFree)
	% ).

:- pred vn__flush_vn_value(vn, list(vn_src), rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn_value(in, in, out, di, uo, di, uo, out) is det.

vn__flush_vn_value(Vn, Srcs, Rval, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-
	vn__lookup_defn(Vn, Vnrval, Vn_tables0),
	(
		Vnrval = vn_origlval(Vnlval),
		( Vnlval = vn_hp ->
			error("vn_hp found in flush_vn_value")
			% It should have been caught in flush_vn
		;
			true
		),
		vn__lookup_current_locs(Vn, Locs0, Vn_tables0),
		(
			% For code understandability, and for aesthetics,
			% we prefer to take the value from its original home,
			% but only if by doing so we incur no cost penalty.
			vn__lookup_current_value(Vnlval, CurVn, Vn_tables0),
			Vn = CurVn
		->
			Locs1 = [Vnlval | Locs0]
		;
			Locs1 = Locs0
		),
		( Locs1 = [] ->
			opt_debug__dump_vnlval(Vnlval, V_str),
			string__append("cannot find copy of an origlval: ",
				V_str, Str),
			error(Str)
		;
			vn__choose_cheapest_loc(Locs1, no, no, Loc)
		),
		vn__flush_access_path(Loc, [src_vn(Vn) | Srcs], Lval,
			Vn_tables0, Vn_tables, Templocs0, Templocs, Instrs),
		Rval = lval(Lval)
	;
		Vnrval = vn_mkword(Tag, SubVn1),
		(
			vn__lookup_defn(SubVn1, SubVnrval, Vn_tables0),
			SubVnrval = vn_origlval(vn_hp)
		->
			vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
				Vn_tables0, Vn_tables,
				Templocs0, Templocs, Instrs),
			vn__lookup_current_locs(Vn, Locs, Vn_tables),
			( Locs = [Loc0 | _] ->
				% see below for an explanation
				vn__flush_access_path(Loc0, [], Lval,
					Vn_tables, _, Templocs0, _, _),
				Rval = lval(Lval)
			;
				Rval = mkword(Tag, Rval1)
			)
		;
			vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
				Vn_tables0, Vn_tables,
				Templocs0, Templocs, Instrs),
			Rval = mkword(Tag, Rval1)
		)
	;
		Vnrval = vn_const(Const),
		Rval = const(Const),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_create(Tag, MaybeRvals, Label),
		Rval = create(Tag, MaybeRvals, Label),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_unop(Unop, SubVn1),
		vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
			Vn_tables0, Vn_tables, Templocs0, Templocs, Instrs),
		Rval = unop(Unop, Rval1)
	;
		Vnrval = vn_binop(Binop, SubVn1, SubVn2),
		vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
			Vn_tables0, Vn_tables1, Templocs0, Templocs1, Instrs1),
		vn__flush_vn(SubVn2, [src_vn(Vn) | Srcs], Rval2,
			Vn_tables1, Vn_tables, Templocs1, Templocs, Instrs2),
		Rval = binop(Binop, Rval1, Rval2),
		list__append(Instrs1, Instrs2, Instrs)
	).

:- pred vn__flush_old_hp(list(vn_src), rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_old_hp(in, out, di, uo, di, uo, out) is det.

vn__flush_old_hp(Srcs0, ReturnRval, Vn_tables0, Vn_tables, Templocs0, Templocs,
		Instrs) :-

	% First take care of the "assignment to hp" part of incr_hp.
	vn__lookup_desired_value(vn_hp, NewhpVn, Vn_tables0),
	vn__flush_hp_incr(NewhpVn, Srcs0, MaybeRval, Vn_tables0, Vn_tables1,
		Templocs0, Templocs1, IncrInstrs),
	(
		MaybeRval = yes(Rval0),
		( Rval0 = const(int_const(I)) ->
			I1 is I // 4,
			Rval = const(int_const(I1))
		; Rval0 = binop((*), Rval1, const(int_const(4))) ->
			Rval = Rval1
		;
			Rval = binop((/), Rval0, const(int_const(4)))
		)
	;
		MaybeRval = no,
		error("empty expression for hp increment")
	),
	vn__set_current_value(vn_hp, NewhpVn, Vn_tables1, Vn_tables2),

	% Find out whether we should tag the old hp, and if so, with what.
	vn__lookup_assigned_vn(vn_origlval(vn_hp), OldhpVn, Vn_tables2),
	( Srcs0 = [Src0Prime | Srcs1Prime] ->
		Src0 = Src0Prime,
		Srcs1 = Srcs1Prime
	;
		error("empty src list in vn__flush_old_hp")
	),
	vn__del_old_use(OldhpVn, Src0, Vn_tables2, Vn_tables3),
	vn__lookup_uses(OldhpVn, OldhpUses, Vn_tables3),
	(
		OldhpUses = [],
		Src0 = src_vn(UserVn),
		vn__lookup_defn(UserVn, UserVnrval, Vn_tables3),
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
			vn__flush_access_path(Vnlval, [], Lval,
				Vn_tables3, _, Templocs1, _, _),
			Templocs2 = Templocs1
		; 
			vn__find_cheap_users(UserVn, UserLocs, Vn_tables3),
			UserLocs = [_|_],
			vn__choose_cheapest_loc(UserLocs, no, no, UserLoc),
			UserLoc = vn_reg(_)
		->
			Vnlval = UserLoc,
			vn__no_heap_vnlval_to_lval(Vnlval, MaybeLval),
			(
				MaybeLval = yes(Lval)
			;
				MaybeLval = no,
				error("register refers to heap")
			),
			Templocs2 = Templocs1
		;
			vn__next_temploc(Templocs1, Templocs2, Vnlval),
			vn__no_heap_vnlval_to_lval(Vnlval, MaybeLval),
			(
				MaybeLval = yes(Lval)
			;
				MaybeLval = no,
				error("temploc refers to heap")
			)
		),
		ReturnRval = const(int_const(42))	% should not be used
	;
		MaybeTag = no,
		AssignedVn = OldhpVn,
		vn__next_temploc(Templocs1, Templocs2, Vnlval),
		vn__no_heap_vnlval_to_lval(Vnlval, MaybeLval),
		(
			MaybeLval = yes(Lval)
		;
			MaybeLval = no,
			error("temploc refers to heap")
		),
		ReturnRval = lval(Lval)
	),

	% Save the old value if necessary.
	( vn__search_current_value(Vnlval, OldVn, Vn_tables3) ->
		vn__find_lvals_in_rval(Rval, ForbiddenLvals),
		vn__maybe_save_prev_value(Vnlval, OldVn, ForbiddenLvals,
			Vn_tables3, Vn_tables4, Templocs2, Templocs, SaveInstrs)
	;
		Vn_tables4 = Vn_tables2,
		Templocs = Templocs2,
		SaveInstrs = []
	),

	vn__set_current_value(Vnlval, AssignedVn, Vn_tables4, Vn_tables),
	Instr = incr_hp(Lval, MaybeTag, Rval) - "",
	list__condense([IncrInstrs, SaveInstrs, [Instr]], Instrs).

:- pred vn__flush_hp_incr(vn, list(vn_src), maybe(rval), vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_hp_incr(in, in, out, di, uo, di, uo, out) is det.

vn__flush_hp_incr(Vn, Srcs, MaybeRval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, Instrs) :-
	(
		vn__rec_find_ref_vns(Vn, SubVns, Vn_tables0),
		vn__free_of_old_hp(SubVns, Vn_tables0)
	->
		vn__flush_vn(Vn, Srcs, Rval, Vn_tables0, Vn_tables,
			Templocs0, Templocs, Instrs),
		MaybeRval = yes(Rval)
	;
		vn__lookup_defn(Vn, Vnrval, Vn_tables0),
		(
			Vnrval = vn_origlval(Vnlval),
			( Vnlval = vn_hp ->
				MaybeRval = no
			;
				error("non-hp origlval in flush_hp_incr")
			),
			Vn_tables2 = Vn_tables0,
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
			Vn_tables2 = Vn_tables0,
			Templocs = Templocs0,
			Instrs = []
		;
			Vnrval = vn_create(_, _, _),
			error("create in calculation of new hp")
		;
			Vnrval = vn_unop(_, _),
			error("unop in calculation of new hp")
		;
			Vnrval = vn_binop(Op, SubVn1, SubVn2),
			vn__flush_hp_incr(SubVn1, [src_vn(Vn) | Srcs],
				MaybeRval1, Vn_tables0, Vn_tables1,
				Templocs0, Templocs1, Instrs1),
			vn__flush_hp_incr(SubVn2, [src_vn(Vn) | Srcs],
				MaybeRval2, Vn_tables1, Vn_tables2,
				Templocs1, Templocs, Instrs2),
			list__append(Instrs1, Instrs2, Instrs),
			(
				MaybeRval1 = yes(Rval1),
				MaybeRval2 = yes(Rval2),
				MaybeRval = yes(binop(Op, Rval1, Rval2))
			;
				MaybeRval1 = yes(Rval2),
				MaybeRval2 = no,
				( Op = (+) ->
					MaybeRval = MaybeRval1
				;
					error("non-+ op on hp")
				)
			;
				MaybeRval1 = no,
				MaybeRval2 = yes(Rval2),
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
		),
		( Srcs = [SrcPrime | _] ->
			Src = SrcPrime
		;
			error("empty source list in flush_vn")
		),
		vn__del_old_use(Vn, Src, Vn_tables2, Vn_tables)
	).

:- pred vn__free_of_old_hp(list(vn), vn_tables).
:- mode vn__free_of_old_hp(in, in) is semidet.

vn__free_of_old_hp([], _Vn_tables).
vn__free_of_old_hp([Vn | Vns], Vn_tables) :-
	vn__lookup_defn(Vn, Vnrval, Vn_tables),
	\+ Vnrval = vn_origlval(vn_hp),
	vn__free_of_old_hp(Vns, Vn_tables).

:- pred vn__rec_find_ref_vns(vn, list(vn), vn_tables).
:- mode vn__rec_find_ref_vns(in, out, in) is det.

vn__rec_find_ref_vns(Vn, [Vn | DeepVns], Vn_tables) :-
	vn__lookup_defn(Vn, Vnrval, Vn_tables),
	vn__find_sub_vns(Vnrval, ImmedVns),
	vn__rec_find_ref_vns_list(ImmedVns, DeepVns, Vn_tables).

:- pred vn__rec_find_ref_vns_list(list(vn), list(vn), vn_tables).
:- mode vn__rec_find_ref_vns_list(in, out, in) is det.

vn__rec_find_ref_vns_list([], [], _Vn_tables).
vn__rec_find_ref_vns_list([Vn | Vns], SubVns, Vn_tables) :-
	vn__rec_find_ref_vns(Vn, SubVns0, Vn_tables),
	vn__rec_find_ref_vns_list(Vns, SubVns1, Vn_tables),
	list__append(SubVns0, SubVns1, SubVns).

%-----------------------------------------------------------------------------%

:- pred vn__flush_access_path(vnlval, list(vn_src), lval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_access_path(in, in, out, di, uo, di, uo, out) is det.

vn__flush_access_path(Vnlval, Srcs, Lval, Vn_tables0, Vn_tables,
		Templocs0, Templocs, AccessInstrs) :-
	(
		Vnlval = vn_reg(Reg),
		Lval = reg(Reg),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_stackvar(Slot),
		Lval = stackvar(Slot),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_framevar(Slot),
		Lval = framevar(Slot),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_succip,
		Lval = succip,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_maxfr,
		Lval = maxfr,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_curredoip,
		Lval = curredoip,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_hp,
		Lval = hp,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_sp,
		Lval = sp,
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	;
		Vnlval = vn_field(Tag, Vn1, Vn2),
		vn__flush_vn(Vn1, [src_access(Vnlval) | Srcs], Rval1,
			Vn_tables0, Vn_tables1,
			Templocs0, Templocs1, AccessInstrs1),
		vn__flush_vn(Vn2, [src_access(Vnlval) | Srcs], Rval2,
			Vn_tables1, Vn_tables,
			Templocs1, Templocs, AccessInstrs2),
		Lval = field(Tag, Rval1, Rval2),
		list__append(AccessInstrs1, AccessInstrs2, AccessInstrs)
	;
		Vnlval = vn_temp(Num),
		Lval = temp(Num),
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		AccessInstrs = []
	).

	% If the vn currently stored in the vnlval is used elsewhere,
	% and if it cannot be recreated blind (or at least not cheaply),
	% then save the value somewhere else. We prefer the somewhere else
	% to be a location where we have to store tha value anyway.
	% However, we must not choose a location that is used in the expression
	% being assigned to the vnlval.

	% If we are overwriting a temporary location, it may not have
	% a current value entry in the vn tables.

	% We cannot look up the old contents of the Vnlval here, because
	% it may have been already overwritten in flush_old_hp.

:- pred vn__maybe_save_prev_value(vnlval, vn, list(lval),
	vn_tables, vn_tables, templocs, templocs, list(instruction)).
:- mode vn__maybe_save_prev_value(in, in, in, di, uo, di, uo, out) is det.

vn__maybe_save_prev_value(Vnlval, Vn, ForbiddenLvals,
		Vn_tables0, Vn_tables, Templocs0, Templocs, Instrs) :-
	(
		vn__search_uses(Vn, Uses, Vn_tables0),
		Uses = [_|_],
		vn__is_const_expr(Vn, no, Vn_tables0),
		vn__lookup_current_locs(Vn, Locs0, Vn_tables0),
		list__delete_all(Locs0, Vnlval, Locs),
		vn__no_good_copies(Locs)
	->
		(
			vn__find_cheap_users(Vn, ReqLocs, Vn_tables0),
			ReqLocs = [_|_]
		->
			vn__choose_cheapest_loc(ReqLocs, no, no, Presumed),
			vn__no_heap_vnlval_to_lval(Presumed, MaybePresumed),
			(
				MaybePresumed = yes(PresumedLval),
				( list__member(PresumedLval, ForbiddenLvals) ->
					vn__next_temploc(Templocs0, Templocs1,
						Chosen)
				; Uses = [_,_|_], \+ Presumed = vn_reg(_) ->
					vn__next_temploc(Templocs0, Templocs1,
						Chosen)
				;
					Chosen = Presumed,
					Templocs1 = Templocs0
				)
			;
				MaybePresumed = no,
				% we cannot use Presumed even if it is not
				% in ForbiddenLvals
				vn__next_temploc(Templocs0, Templocs1,
					Chosen)
			)
		;
			vn__next_temploc(Templocs0, Templocs1, Chosen)
		),
		vn__ensure_assignment(Chosen, Vn,
			Vn_tables0, Vn_tables, Templocs1, Templocs, Instrs)
	;
		Vn_tables = Vn_tables0,
		Templocs = Templocs0,
		Instrs = []
	).

:- pred vn__no_good_copies(list(vnlval)).
:- mode vn__no_good_copies(in) is semidet.

vn__no_good_copies([]).
vn__no_good_copies([Vnlval | Vnlvals]) :-
	Vnlval = vn_field(_, _, _),
	vn__no_good_copies(Vnlvals).

%-----------------------------------------------------------------------------%

:- pred vn__get_incr_hp(list(instruction), instruction, list(instruction)).
:- mode vn__get_incr_hp(di, out, uo) is det.

vn__get_incr_hp([], _, _) :-
	error("could not find incr_hp").
vn__get_incr_hp([Instr0 | Instrs0], IncrHp, Instrs) :-
	( Instr0 = incr_hp(_, _, _) - _ ->
		IncrHp = Instr0,
		Instrs = Instrs0
	;
		vn__get_incr_hp(Instrs0, IncrHp, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).
