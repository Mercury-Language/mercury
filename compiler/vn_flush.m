%-----------------------------------------------------------------------------%

% Vn_flush.nl - flush the nodes of the vn graph in order.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_flush.

:- interface.

:- import_module vn_type, vn_table, vn_temploc.
:- import_module llds, list.

	% Flush the given nodes in the given order.

:- pred vn__flush_nodelist(list(vn_node), ctrlmap, vn_tables, templocs,
	list(instruction), io__state, io__state).
:- mode vn__flush_nodelist(in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module vn_table, vn_util, vn_debug, opt_debug.
:- import_module map, int, string, require, std_util.

vn__flush_nodelist([], _, _, _, []) --> [].
vn__flush_nodelist([Node0 | Nodes0], Ctrlmap, VnTables0, Templocs0, Instrs) -->
	( { Node0 = node_origlval(_) } ->
		{ Nodes1 = Nodes0 },
		{ VnTables1 = VnTables0 },
		{ Templocs1 = Templocs0 },
		{ Instrs0 = [] }
	;
		vn__flush_node(Node0, Ctrlmap, Nodes0, Nodes1,
			VnTables0, VnTables1, Templocs0, Templocs1, Instrs0)
	),
	vn__flush_nodelist(Nodes1, Ctrlmap, VnTables1, Templocs1, Instrs1),
	{ list__append(Instrs0, Instrs1, Instrs) }.

	% Flush the given node.

:- pred vn__flush_node(vn_node, ctrlmap, list(vn_node), list(vn_node),
	vn_tables, vn_tables, templocs, templocs, list(instruction),
	io__state, io__state).
:- mode vn__flush_node(in, in, di, uo, di, uo, di, uo, out, di, uo) is det.

vn__flush_node(Node, Ctrlmap, Nodes0, Nodes, VnTables0, VnTables,
		Templocs0, Templocs, Instrs) -->
	vn__flush_start_msg(Node),
	(
		{ Node = node_shared(Vn) },
		{ vn__choose_best_loc(Vn, Vnlval, VnTables0,
			Templocs0, Templocs1) },
		( { vn__search_desired_value(Vnlval, Vn, VnTables0) } ->
			vn__flush_also_msg(Vnlval),
			{ list__delete_all(Nodes0, node_lval(Vnlval), Nodes) }
		;
			{ Nodes = Nodes0 }
		),
		% vn__vnlval_access_vns(Vnlval, AccessVns),
		% vn__compensate_for_access_vns(AccessVns, Vnlval,
		% 	VnTables0, VnTables1),
		{ vn__ensure_assignment(Vnlval, Vn,
			VnTables0, VnTables, Templocs1, Templocs, Instrs) }
	;
		{ Node = node_lval(Vnlval) },
		{ Nodes = Nodes0 },
		{ vn__lookup_desired_value(Vnlval, Vn, VnTables0) },
		{ vn__ensure_assignment(Vnlval, Vn,
			VnTables0, VnTables, Templocs0, Templocs, Instrs) }
	;
		{ Node = node_origlval(Vnlval) },
		{ Nodes = Nodes0 },
		{ VnTables = VnTables0 },
		{ Templocs = Templocs0 },
		{ Instrs = [] }
	;
		{ Node = node_ctrl(N) },
		{ Nodes = Nodes0 },
		{ map__lookup(Ctrlmap, N, VnInstr) },
		{ vn__flush_ctrl_node(VnInstr, N,
			VnTables0, VnTables, Templocs0, Templocs, Instrs) }
	),
	vn__flush_end_msg(Instrs, VnTables).

:- pred vn__flush_ctrl_node(vn_instr, int, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_ctrl_node(in, in, di, uo, di, uo, out) is det.

vn__flush_ctrl_node(Vn_instr, N, VnTables0, VnTables, Templocs0, Templocs,
		Instrs) :-
	(
		Vn_instr = vn_livevals(Livevals),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [livevals(Livevals) - ""]
	;
		Vn_instr = vn_call(ProcAddr, RetAddr, CallerAddr, LiveInfo),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [call(ProcAddr, RetAddr, CallerAddr, LiveInfo) - ""]
	;
		Vn_instr = vn_call_closure(ClAddr, RetAddr, LiveInfo),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [call_closure(ClAddr, RetAddr, LiveInfo) - ""]
	;
		Vn_instr = vn_mkframe(Name, Size, Redoip),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [mkframe(Name, Size, Redoip) - ""]
	;
		Vn_instr = vn_modframe(Redoip),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [modframe(Redoip) - ""]
	;
		Vn_instr = vn_label(Label),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [label(Label) - ""]
	;
		Vn_instr = vn_goto(TargetAddr, CallerAddr),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [goto(TargetAddr, CallerAddr) - ""]
	;
		Vn_instr = vn_computed_goto(Vn, Labels),
		vn__flush_vn(Vn, [src_ctrl(N)], Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, FlushInstrs),
		Instr = computed_goto(Rval, Labels) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_if_val(Vn, TargetAddr),
		vn__flush_vn(Vn, [src_ctrl(N)], Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, FlushInstrs),
		Instr = if_val(Rval, TargetAddr) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_mark_hp(Vnlval),
		vn__flush_access_path(Vnlval, [src_ctrl(N)], Lval,
			VnTables0, VnTables1,
			Templocs0, Templocs, FlushInstrs),
		vn__lookup_assigned_vn(vn_origlval(vn_hp), OldhpVn,
			VnTables1),
		vn__set_current_value(Vnlval, OldhpVn,
			VnTables1, VnTables),
		Instr = mark_hp(Lval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_restore_hp(Vn),
		vn__flush_vn(Vn, [src_ctrl(N)], Rval,
			VnTables0, VnTables1,
			Templocs0, Templocs, FlushInstrs),
		vn__set_current_value(vn_hp, Vn,
			VnTables1, VnTables),
		Instr = restore_hp(Rval) - "",
		list__append(FlushInstrs, [Instr], Instrs)
	;
		Vn_instr = vn_incr_sp(Incr),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [incr_sp(Incr) - ""]
	;
		Vn_instr = vn_decr_sp(Decr),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = [decr_sp(Decr) - ""]
	).

%-----------------------------------------------------------------------------%

	% Choose a location for a shared value number that does not have to go
	% anywhere specific right now. We prefer to choose a register that
	% already has the value. If we cannot, we would like to choose from
	% among those locations that eventually want to have that value number,
	% but this may not be worthwhile if the contents of that location would
	% have to be saved first.

:- pred vn__choose_best_loc(vn, vnlval, vn_tables, templocs, templocs).
:- mode vn__choose_best_loc(in, out, in, di, uo) is det.

vn__choose_best_loc(Vn, Chosen, VnTables, Templocs0, Templocs) :-
	(
		vn__lookup_current_locs(Vn, CurrentLocs, VnTables),
		CurrentLocs = [_|_]
	->
		vn__choose_cheapest_loc(CurrentLocs, no, no, Presumed),
		(
			\+ Presumed = vn_reg(_),
			vn__lookup_uses(Vn, Uses, VnTables),
			list__delete_first(Uses, src_liveval(Presumed),
				NewUses),
			NewUses = [_,_|_]
		->
			vn__choose_best_user(Vn, Chosen, VnTables,
				Templocs0, Templocs)
		;
			Chosen = Presumed,
			Templocs = Templocs0
		)
	;
		vn__choose_best_user(Vn, Chosen, VnTables, Templocs0, Templocs)
	).

:- pred vn__choose_best_user(vn, vnlval, vn_tables, templocs, templocs).
:- mode vn__choose_best_user(in, out, in, di, uo) is det.

vn__choose_best_user(Vn, Chosen, VnTables, Templocs0, Templocs) :-
	(
		vn__find_cheap_users(Vn, Users, VnTables),
		Users = [_|_]
	->
		vn__choose_cheapest_loc(Users, no, no, Presumed),
		(
			% assign directly to a non-reg location
			% only if that is the only user of this vn
			\+ Presumed = vn_reg(_),
			vn__lookup_uses(Vn, Uses, VnTables),
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

vn__find_cheap_users(Vn, Vnlvals, VnTables) :-
	( vn__search_uses(Vn, Uses, VnTables) ->
		vn__find_cheap_users_2(Uses, Vnlvals, VnTables)
	;
		Vnlvals = []
	).

:- pred vn__find_cheap_users_2(list(vn_src), list(vnlval), vn_tables).
:- mode vn__find_cheap_users_2(in, out, in) is det.

vn__find_cheap_users_2([], [], _VnTables).
vn__find_cheap_users_2([Src | Srcs], Vnlvals, VnTables) :-
	vn__find_cheap_users_2(Srcs, Vnlvals0, VnTables),
	(
		Src = src_liveval(Live)
		% \+ Live = vn_field(_, _, _)
	->
		( vn__search_current_value(Live, Vn, VnTables) ->
			vn__lookup_uses(Vn, Uses, VnTables),
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
						VnTables)
				->
					User = vn_reg(_),
					vn__lookup_uses(UserVn, [], VnTables)
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
		Loc = vn_curfr,
		BestLoc = Loc
	;
		Loc = vn_redoip(_),
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

vn__ensure_assignment(Vnlval, Vn, VnTables0, VnTables,
		Templocs0, Templocs, Instrs) :-
	(
		vn__search_current_value(Vnlval, Cur_vn, VnTables0),
		Vn = Cur_vn
	->
		vn__del_old_use(Vn, src_liveval(Vnlval),
			VnTables0, VnTables1),
		vn__vnlval_access_vns(Vnlval, SubVns),
		vn__del_old_uses(SubVns, src_access(Vnlval),
			VnTables1, VnTables),
		Templocs = Templocs0,
		Instrs = []
	;
		vn__generate_assignment(Vnlval, Vn, VnTables0, VnTables,
			Templocs0, Templocs, _, Instrs)
	).

:- pred vn__generate_assignment(vnlval, vn, vn_tables, vn_tables,
	templocs, templocs, lval, list(instruction)).
:- mode vn__generate_assignment(in, in, di, uo, di, uo, out, out) is det.

vn__generate_assignment(Vnlval, Vn, VnTables0, VnTables,
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
	( vn__search_current_value(Vnlval, OldVn0, VnTables0) ->
		SaveVn = yes(OldVn0)
	;
		SaveVn = no
	),
	vn__no_temploc(Vnlval, Templocs0, Templocs1),
	vn__flush_access_path(Vnlval, [src_access(Vnlval)], Lval,
		VnTables0, VnTables1, Templocs1, Templocs2, AccessInstrs),
	vn__flush_vn(Vn, [src_liveval(Vnlval)], Rval, VnTables1, VnTables2,
		Templocs2, Templocs3, FlushInstrs0),
	% The 'current' value of Vnlval may be changed by flush_vn if it
	% involves a reference to the old value of hp.
	(
		SaveVn = yes(OldVn),
		vn__find_lvals_in_rval(Rval, ForbiddenLvals),
		vn__maybe_save_prev_value(Vnlval, OldVn, ForbiddenLvals,
			VnTables2, VnTables3, Templocs3, Templocs, SaveInstrs)
	;
		SaveVn = no,
		VnTables3 = VnTables2,
		Templocs = Templocs3,
		SaveInstrs = []
	),
	( vn__search_current_value(Vnlval, Vn, VnTables3) ->
		% Flush_vn must perform the entire assignment if it involves
		% exactly the actions of an incr_hp operation. Since the
		% incr_hp in FlushInstrs overwrites Lval, we must perform it
		% after Lval's old value has been saved.
		vn__get_incr_hp(FlushInstrs0, Instr, FlushInstrs1),
		VnTables = VnTables3
	;
		vn__set_current_value(Vnlval, Vn, VnTables3, VnTables),
		Instr = assign(Lval, Rval) - "vn flush",
		FlushInstrs1 = FlushInstrs0
	),
	list__condense([AccessInstrs, FlushInstrs1, SaveInstrs, [Instr]],
		Instrs).

%-----------------------------------------------------------------------------%

:- pred vn__flush_vn(vn, list(vn_src), rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn(in, in, out, di, uo, di, uo, out) is det.

vn__flush_vn(Vn, Srcs, Rval, VnTables0, VnTables,
		Templocs0, Templocs, Instrs) :-
	( Srcs = [SrcPrime | _] ->
		Src = SrcPrime
	;
		error("empty source list in flush_vn")
	),
	vn__is_const_expr(Vn, IsConst, VnTables0),
	(
		IsConst = yes,
		vn__flush_vn_value(Vn, Srcs, Rval,
			VnTables0, VnTables3, Templocs0, Templocs, Instrs)
	;
		IsConst = no,
		vn__lookup_current_locs(Vn, Locs, VnTables0),
		vn__lookup_uses(Vn, Uses, VnTables0),
		list__delete_all(Uses, Src, NewUses),
		( Locs = [] ->
			% If there are no more uses, it is useless to assign
			% the vn to a location. Otherwise it is useful, but if
			% Src is a livevals, the assignment will be done by
			% the caller.
			(
				NewUses = [_|_],
				\+ Src = src_liveval(_)
			->
				vn__choose_best_loc(Vn, Vnlval, VnTables0,
					Templocs0, Templocs1),
				vn__generate_assignment(Vnlval, Vn,
					VnTables0, VnTables3,
					Templocs1, Templocs, Lval, Instrs),
				Rval = lval(Lval)
			;
				vn__flush_vn_value(Vn, Srcs, Rval,
					VnTables0, VnTables3,
					Templocs0, Templocs, Instrs)
			)
		;
			vn__choose_cheapest_loc(Locs, no, no, Loc),
			(
				Loc = vn_hp
			->
				% The first reference to the old value of hp.
				vn__flush_old_hp(Srcs, Rval,
					VnTables0, VnTables3,
					Templocs0, Templocs, Instrs)
			;
				NewUses = [_,_|_],
				\+ ( Loc = vn_reg(_) ; Loc = vn_temp(_) ),
				\+ Src = src_liveval(_)
			->
				vn__next_temploc(Templocs0, Templocs1, Vnlval),
				vn__generate_assignment(Vnlval, Vn,
					VnTables0, VnTables3,
					Templocs1, Templocs, Lval, Instrs),
				Rval = lval(Lval)
			;
				vn__flush_access_path(Loc, [src_vn(Vn) | Srcs],
					Lval, VnTables0, VnTables3,
					Templocs0, Templocs, Instrs),
				Rval = lval(Lval)
			)
		)
	),
	vn__del_old_use(Vn, Src, VnTables3, VnTables),
	true.
	% vn__lookup_uses(Vn, NewUses, VnTables),
	% ( NewUses = [_|_] ->
	% 	NewlyFree = NewlyFree0
	% ;
	% 	vn__lookup_current_locs(Vn, NewlyFree1, VnTables0)
	% 	list__append(NewlyFree0, NewlyFree1, NewlyFree)
	% ).

:- pred vn__flush_vn_value(vn, list(vn_src), rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_vn_value(in, in, out, di, uo, di, uo, out) is det.

vn__flush_vn_value(Vn, Srcs, Rval, VnTables0, VnTables, Templocs0, Templocs,
		Instrs) :-
	vn__lookup_defn(Vn, Vnrval, VnTables0),
	(
		Vnrval = vn_origlval(Vnlval),
		( Vnlval = vn_hp ->
			error("vn_hp found in flush_vn_value")
			% It should have been caught in flush_vn
		;
			true
		),
		vn__lookup_current_locs(Vn, Locs0, VnTables0),
		(
			% For code understandability, and for aesthetics,
			% we prefer to take the value from its original home,
			% but only if by doing so we incur no cost penalty.
			vn__lookup_current_value(Vnlval, CurVn, VnTables0),
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
			VnTables0, VnTables, Templocs0, Templocs, Instrs),
		Rval = lval(Lval)
	;
		Vnrval = vn_mkword(Tag, SubVn1),
		(
			vn__lookup_defn(SubVn1, SubVnrval, VnTables0),
			SubVnrval = vn_origlval(vn_hp)
		->
			vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
				VnTables0, VnTables,
				Templocs0, Templocs, Instrs),
			vn__lookup_current_locs(Vn, Locs, VnTables),
			( Locs = [Loc0 | _] ->
				% see below for an explanation
				vn__flush_access_path(Loc0, [], Lval,
					VnTables, _, Templocs0, _, _),
				Rval = lval(Lval)
			;
				Rval = mkword(Tag, Rval1)
			)
		;
			vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
				VnTables0, VnTables,
				Templocs0, Templocs, Instrs),
			Rval = mkword(Tag, Rval1)
		)
	;
		Vnrval = vn_const(Const),
		Rval = const(Const),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_create(Tag, MaybeRvals, Label),
		Rval = create(Tag, MaybeRvals, Label),
		VnTables = VnTables0,
		Templocs = Templocs0,
		Instrs = []
	;
		Vnrval = vn_unop(Unop, SubVn1),
		vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
			VnTables0, VnTables, Templocs0, Templocs, Instrs),
		Rval = unop(Unop, Rval1)
	;
		Vnrval = vn_binop(Binop, SubVn1, SubVn2),
		vn__flush_vn(SubVn1, [src_vn(Vn) | Srcs], Rval1,
			VnTables0, VnTables1, Templocs0, Templocs1, Instrs1),
		vn__flush_vn(SubVn2, [src_vn(Vn) | Srcs], Rval2,
			VnTables1, VnTables, Templocs1, Templocs, Instrs2),
		Rval = binop(Binop, Rval1, Rval2),
		list__append(Instrs1, Instrs2, Instrs)
	).

:- pred vn__flush_old_hp(list(vn_src), rval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_old_hp(in, out, di, uo, di, uo, out) is det.

vn__flush_old_hp(Srcs0, ReturnRval, VnTables0, VnTables, Templocs0, Templocs,
		Instrs) :-

	% First take care of the "assignment to hp" part of incr_hp.
	vn__lookup_desired_value(vn_hp, NewhpVn, VnTables0),
	vn__flush_hp_incr(NewhpVn, Srcs0, MaybeRval, VnTables0, VnTables1,
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
	vn__set_current_value(vn_hp, NewhpVn, VnTables1, VnTables2),

	% Find out whether we should tag the old hp, and if so, with what.
	vn__lookup_assigned_vn(vn_origlval(vn_hp), OldhpVn, VnTables2),
	( Srcs0 = [Src0Prime | Srcs1Prime] ->
		Src0 = Src0Prime,
		Srcs1 = Srcs1Prime
	;
		error("empty src list in vn__flush_old_hp")
	),
	vn__del_old_use(OldhpVn, Src0, VnTables2, VnTables3),
	vn__lookup_uses(OldhpVn, OldhpUses, VnTables3),
	(
		OldhpUses = [],
		Src0 = src_vn(UserVn),
		vn__lookup_defn(UserVn, UserVnrval, VnTables3),
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
				VnTables3, _, Templocs1, _, _),
			Templocs2 = Templocs1
		; 
			vn__find_cheap_users(UserVn, UserLocs, VnTables3),
			UserLocs = [_|_],
			vn__choose_cheapest_loc(UserLocs, no, no, UserLoc),
			UserLoc = vn_reg(_)
		->
			Vnlval = UserLoc,
			vn__no_access_vnlval_to_lval(Vnlval, MaybeLval),
			(
				MaybeLval = yes(Lval)
			;
				MaybeLval = no,
				error("register needs access path")
			),
			Templocs2 = Templocs1
		;
			vn__next_temploc(Templocs1, Templocs2, Vnlval),
			vn__no_access_vnlval_to_lval(Vnlval, MaybeLval),
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
		vn__next_temploc(Templocs1, Templocs2, Vnlval),
		vn__no_access_vnlval_to_lval(Vnlval, MaybeLval),
		(
			MaybeLval = yes(Lval)
		;
			MaybeLval = no,
			error("temploc needs access path")
		),
		ReturnRval = lval(Lval)
	),

	% Save the old value if necessary.
	( vn__search_current_value(Vnlval, OldVn, VnTables3) ->
		vn__find_lvals_in_rval(Rval, ForbiddenLvals),
		vn__maybe_save_prev_value(Vnlval, OldVn, ForbiddenLvals,
			VnTables3, VnTables4, Templocs2, Templocs, SaveInstrs)
	;
		VnTables4 = VnTables2,
		Templocs = Templocs2,
		SaveInstrs = []
	),

	vn__set_current_value(Vnlval, AssignedVn, VnTables4, VnTables),
	Instr = incr_hp(Lval, MaybeTag, Rval) - "",
	list__condense([IncrInstrs, SaveInstrs, [Instr]], Instrs).

:- pred vn__flush_hp_incr(vn, list(vn_src), maybe(rval), vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_hp_incr(in, in, out, di, uo, di, uo, out) is det.

vn__flush_hp_incr(Vn, Srcs, MaybeRval, VnTables0, VnTables,
		Templocs0, Templocs, Instrs) :-
	(
		vn__rec_find_ref_vns(Vn, SubVns, VnTables0),
		vn__free_of_old_hp(SubVns, VnTables0)
	->
		vn__flush_vn(Vn, Srcs, Rval, VnTables0, VnTables,
			Templocs0, Templocs, Instrs),
		MaybeRval = yes(Rval)
	;
		vn__lookup_defn(Vn, Vnrval, VnTables0),
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
			Vnrval = vn_create(_, _, _),
			error("create in calculation of new hp")
		;
			Vnrval = vn_unop(_, _),
			error("unop in calculation of new hp")
		;
			Vnrval = vn_binop(Op, SubVn1, SubVn2),
			vn__flush_hp_incr(SubVn1, [src_vn(Vn) | Srcs],
				MaybeRval1, VnTables0, VnTables1,
				Templocs0, Templocs1, Instrs1),
			vn__flush_hp_incr(SubVn2, [src_vn(Vn) | Srcs],
				MaybeRval2, VnTables1, VnTables2,
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
		vn__del_old_use(Vn, Src, VnTables2, VnTables)
	).

:- pred vn__free_of_old_hp(list(vn), vn_tables).
:- mode vn__free_of_old_hp(in, in) is semidet.

vn__free_of_old_hp([], _VnTables).
vn__free_of_old_hp([Vn | Vns], VnTables) :-
	vn__lookup_defn(Vn, Vnrval, VnTables),
	\+ Vnrval = vn_origlval(vn_hp),
	vn__free_of_old_hp(Vns, VnTables).

:- pred vn__rec_find_ref_vns(vn, list(vn), vn_tables).
:- mode vn__rec_find_ref_vns(in, out, in) is det.

vn__rec_find_ref_vns(Vn, [Vn | DeepVns], VnTables) :-
	vn__lookup_defn(Vn, Vnrval, VnTables),
	vn__find_sub_vns(Vnrval, ImmedVns),
	vn__rec_find_ref_vns_list(ImmedVns, DeepVns, VnTables).

:- pred vn__rec_find_ref_vns_list(list(vn), list(vn), vn_tables).
:- mode vn__rec_find_ref_vns_list(in, out, in) is det.

vn__rec_find_ref_vns_list([], [], _VnTables).
vn__rec_find_ref_vns_list([Vn | Vns], SubVns, VnTables) :-
	vn__rec_find_ref_vns(Vn, SubVns0, VnTables),
	vn__rec_find_ref_vns_list(Vns, SubVns1, VnTables),
	list__append(SubVns0, SubVns1, SubVns).

%-----------------------------------------------------------------------------%

:- pred vn__flush_access_path(vnlval, list(vn_src), lval, vn_tables, vn_tables,
	templocs, templocs, list(instruction)).
:- mode vn__flush_access_path(in, in, out, di, uo, di, uo, out) is det.

vn__flush_access_path(Vnlval, Srcs, Lval, VnTables0, VnTables,
		Templocs0, Templocs, AccessInstrs) :-
	(
		Vnlval = vn_reg(Reg),
		Lval = reg(Reg),
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
		Vnlval = vn_redoip(Vn1),
		vn__flush_vn(Vn1, [src_access(Vnlval) | Srcs], Rval,
			VnTables0, VnTables,
			Templocs0, Templocs, AccessInstrs),
		Lval = redoip(Rval)
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
		vn__flush_vn(Vn1, [src_access(Vnlval) | Srcs], Rval1,
			VnTables0, VnTables1,
			Templocs0, Templocs1, AccessInstrs1),
		vn__flush_vn(Vn2, [src_access(Vnlval) | Srcs], Rval2,
			VnTables1, VnTables,
			Templocs1, Templocs, AccessInstrs2),
		Lval = field(Tag, Rval1, Rval2),
		list__append(AccessInstrs1, AccessInstrs2, AccessInstrs)
	;
		Vnlval = vn_temp(Num),
		Lval = temp(Num),
		VnTables = VnTables0,
		Templocs = Templocs0,
		AccessInstrs = []
	).

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

:- pred vn__maybe_save_prev_value(vnlval, vn, list(lval),
	vn_tables, vn_tables, templocs, templocs, list(instruction)).
:- mode vn__maybe_save_prev_value(in, in, in, di, uo, di, uo, out) is det.

vn__maybe_save_prev_value(Vnlval, Vn, ForbiddenLvals,
		VnTables0, VnTables, Templocs0, Templocs, Instrs) :-
	(
		vn__search_uses(Vn, Uses, VnTables0),
		Uses = [_|_],
		vn__is_const_expr(Vn, no, VnTables0),
		vn__lookup_current_locs(Vn, Locs0, VnTables0),
		list__delete_all(Locs0, Vnlval, Locs),
		vn__no_good_copies(Locs)
	->
		(
			vn__find_cheap_users(Vn, ReqLocs, VnTables0),
			ReqLocs = [_|_]
		->
			vn__choose_cheapest_loc(ReqLocs, no, no, Presumed),
			vn__no_access_vnlval_to_lval(Presumed, MaybePresumed),
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
			VnTables0, VnTables, Templocs1, Templocs, Instrs)
	;
		VnTables = VnTables0,
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
