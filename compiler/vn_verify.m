%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% value_number.m - sanity checks for value numbering.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_verify.

:- interface.

:- import_module vn_type, vn_table, llds.
:- import_module list, bool.

:- pred vn_verify__ok(list(instruction), instr, bool, bool,
	vnlvalset, vnlvalset, vn_tables, vn_tables, bool, io__state, io__state).
:- mode vn_verify__ok(in, in, in, in, in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module builtin_ops, opt_debug, vn_debug, vn_util.
:- import_module map, set, string, std_util, require.

vn_verify__ok(Instrs, Uinstr0, SeenIncr0, SeenIncr, Liveset0, Liveset,
		VnTables0, VnTables, OK) -->
	(
		{ SeenIncr0 \= SeenIncr }
	->
		vn_debug__failure_msg(Uinstr0, "disagreement on SeenIncr"),
		{ OK = no }
	;
		{ vn_verify__equivalence(Liveset0, Liveset,
			VnTables0, VnTables, Problem) },
		{ Problem = yes(Msg) }
	->
		vn_debug__failure_msg(Uinstr0, Msg),
		{ OK = no }
	;
		{ vn_verify__tags(Instrs) }
	->
		{ OK = yes }
	;
		vn_debug__failure_msg(Uinstr0, "failure of tag check"),
		{ OK = no }
	).

:- pred vn_verify__equivalence(vnlvalset, vnlvalset, vn_tables, vn_tables,
	maybe(string)).
:- mode vn_verify__equivalence(in, in, in, in, out) is det.

vn_verify__equivalence(Liveset0, Liveset7, VnTables0, VnTables7,
		Problem) :-
	set__to_sorted_list(Liveset0, Livevals0),
	set__to_sorted_list(Liveset7, Livevals7),
	map__init(InitVerifyMap0),
	vn_verify__make_verify_map(Livevals0, VnTables0, InitVerifyMap0,
		VerifyMap0, Problem0),
	map__init(InitVerifyMap7),
	vn_verify__make_verify_map(Livevals7, VnTables7, InitVerifyMap7,
		VerifyMap7, Problem7),
	( Problem0 = yes(_) ->
		Problem = Problem0
	; Problem7 = yes(_) ->
		Problem = Problem7
	;
		map__keys(VerifyMap0, Keys0),
		map__keys(VerifyMap7, Keys7),
		list__append(Keys0, Keys7, KeysDups),
		list__remove_dups(KeysDups, Keys),
		vn_verify__correspondence(Keys, VerifyMap0, VerifyMap7, Problem)
	).

:- pred vn_verify__correspondence(list(lval), map(lval, rval),
	map(lval, rval), maybe(string)).
:- mode vn_verify__correspondence(in, in, in, out) is det.

vn_verify__correspondence([], _, _, no).
vn_verify__correspondence([Lval | Lvals], VerifyMap0, VerifyMap7,
		Problem) :-
	(
		map__search(VerifyMap0, Lval, Rval0),
		map__search(VerifyMap7, Lval, Rval7)
	->
		( Rval0 = Rval7 ->
			vn_verify__correspondence(Lvals,
				VerifyMap0, VerifyMap7, Problem)
		;
			opt_debug__dump_lval(Lval, Lstr),
			string__append("disagreement on value of ", Lstr, Msg),
			Problem = yes(Msg)
		)
	;
		% We want to allow the optimization of blocks in which
		% the new version of the code does not have any mention at all
		% of some lval that appeared in dead code.
		\+ map__search(VerifyMap7, Lval, _)
	->
		vn_verify__correspondence(Lvals,
			VerifyMap0, VerifyMap7, Problem)
	;
		opt_debug__dump_lval(Lval, Lstr),
		string__append("cannot find value of ", Lstr, Msg),
		Problem = yes(Msg)
	).

:- pred vn_verify__make_verify_map(list(vnlval), vn_tables,
	map(lval, rval), map(lval, rval), maybe(string)).
% :- mode vn_verify__make_verify_map(in, in, di, uo, out) is det.
:- mode vn_verify__make_verify_map(in, in, in, out, out) is det.

vn_verify__make_verify_map(LiveVnlvals, VnTables,
		VerifyMap0, VerifyMap, Problem) :-
	vn_table__get_all_vnrvals(Vnrvals, VnTables),
	vn_verify__make_verify_map_specials(Vnrvals, LiveVnlvals,
		Vnlvals),
	vn_verify__make_verify_map_2(Vnlvals, VnTables,
		VerifyMap0, VerifyMap, Problem).

:- pred vn_verify__make_verify_map_2(list(vnlval), vn_tables,
	map(lval, rval), map(lval, rval), maybe(string)).
% :- mode vn_verify__make_verify_map_2(in, in, di, uo, out) is det.
:- mode vn_verify__make_verify_map_2(in, in, in, out, out) is det.

vn_verify__make_verify_map_2([], _, VerifyMap, VerifyMap, no).
vn_verify__make_verify_map_2([Vnlval | Vnlvals], VnTables,
		VerifyMap0, VerifyMap, Problem) :-
	vn_verify__lval(Vnlval, VnTables, Lval),
	( vn_table__search_desired_value(Vnlval, DesVn, VnTables) ->
		vn_verify__value(DesVn, VnTables, Rval),
		map__set(VerifyMap0, Lval, Rval, VerifyMap1),
		vn_verify__make_verify_map_2(Vnlvals, VnTables,
			VerifyMap1, VerifyMap, Problem)
	;
		% opt_debug__dump_vnlval(Vnlval, Lstr),
		% string__append("cannot find desired value of ", Lstr, Msg),
		% Problem = yes(Msg),
		% VerifyMap = VerifyMap0	% should be ignored
		vn_verify__make_verify_map_2(Vnlvals, VnTables,
			VerifyMap0, VerifyMap, Problem)
	).

:- pred vn_verify__make_verify_map_specials(list(vnrval),
	list(vnlval), list(vnlval)).
% :- mode vn_verify__make_verify_map_specials(in, di, uo) is det.
:- mode vn_verify__make_verify_map_specials(in, in, out) is det.

vn_verify__make_verify_map_specials([], Vnlvals, Vnlvals).
vn_verify__make_verify_map_specials([Vnrval | Vnrvals],
		Vnlvals0, Vnlvals) :-
	(
		Vnrval = vn_origlval(Vnlval),
		vn_util__find_specials(Vnlval, Specials)
	->
		list__append(Vnlvals0, Specials, Vnlvals1)
	;
		Vnlvals1 = Vnlvals0
	),
	vn_verify__make_verify_map_specials(Vnrvals, Vnlvals1, Vnlvals).

:- pred vn_verify__lval(vnlval, vn_tables, lval).
:- mode vn_verify__lval(in, in, out) is det.

vn_verify__lval(Vnlval, VnTables, Lval) :-
	vn_util__vnlval_access_vns(Vnlval, AccessVns),
	vn_verify__values(AccessVns, VnTables, AccessRvals),
	( vn_verify__subst_access_vns(Vnlval, AccessRvals, LvalPrime) ->
		Lval = LvalPrime
	;
		error("cannot substitute access vns in vn_verify__lval")
	).

:- pred vn_verify__values(list(vn), vn_tables, list(rval)).
:- mode vn_verify__values(in, in, out) is det.

vn_verify__values([], _VnTables, []).
vn_verify__values([Vn | Vns], VnTables, [Rval | Rvals]) :-
	vn_verify__value(Vn, VnTables, Rval),
	vn_verify__values(Vns, VnTables, Rvals).

:- pred vn_verify__value(vn, vn_tables, rval).
:- mode vn_verify__value(in, in, out) is det.

vn_verify__value(Vn, VnTables, Rval) :-
	vn_table__lookup_defn(Vn, Vnrval, "vn_verify__value",
		VnTables),
	vn_util__find_sub_vns(Vnrval, SubVns),
	vn_verify__values(SubVns, VnTables, SubRvals),
	(
		vn_verify__subst_sub_vns(Vnrval, SubRvals, VnTables,
			RvalPrime)
	->
		Rval = RvalPrime
	;
		error("cannot substitute sub vns in vn_verify__value")
	).

:- pred vn_verify__subst_access_vns(vnlval, list(rval), lval).
:- mode vn_verify__subst_access_vns(in, in, out) is semidet.

vn_verify__subst_access_vns(vn_reg(T, N), [], reg(T, N)).
vn_verify__subst_access_vns(vn_stackvar(N), [], stackvar(N)).
vn_verify__subst_access_vns(vn_framevar(N), [], framevar(N)).
vn_verify__subst_access_vns(vn_succip, [], succip).
vn_verify__subst_access_vns(vn_maxfr, [], maxfr).
vn_verify__subst_access_vns(vn_curfr, [], curfr).
vn_verify__subst_access_vns(vn_prevfr(_), [R], prevfr(R)).
vn_verify__subst_access_vns(vn_redoip(_), [R], redoip(R)).
vn_verify__subst_access_vns(vn_redofr(_), [R], redofr(R)).
vn_verify__subst_access_vns(vn_succip(_), [R], succip(R)).
vn_verify__subst_access_vns(vn_succfr(_), [R], succfr(R)).
vn_verify__subst_access_vns(vn_hp, [], hp).
vn_verify__subst_access_vns(vn_sp, [], sp).
vn_verify__subst_access_vns(vn_field(T, _, _), [R1, R2], field(T, R1, R2)).
vn_verify__subst_access_vns(vn_temp(T, N), [], temp(T, N)).

:- pred vn_verify__subst_sub_vns(vnrval, list(rval), vn_tables, rval).
:- mode vn_verify__subst_sub_vns(in, in, in, out) is semidet.

vn_verify__subst_sub_vns(vn_origlval(Vnlval), _, VnTables, lval(Lval)) :-
	vn_verify__lval(Vnlval, VnTables, Lval).
vn_verify__subst_sub_vns(vn_mkword(Tag, _), [R], _, mkword(Tag, R)).
vn_verify__subst_sub_vns(vn_const(Const), [], _, const(Const)).
vn_verify__subst_sub_vns(vn_create(T,A,AT,U,L,M), [], _,
		create(T,A,AT,U,L,M, Reuse)) :-
	Reuse = no.
vn_verify__subst_sub_vns(vn_unop(Op, _), [R], _, unop(Op, R)).
vn_verify__subst_sub_vns(vn_binop(Op, _, _), [R1, R2], _, binop(Op, R1, R2)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred vn_verify__tags(list(instruction)).
:- mode vn_verify__tags(in) is semidet.

vn_verify__tags(Instrs) :-
	list__reverse(Instrs, RevInstrs),
	set__init(NoDeref),
	set__init(Tested),
	vn_verify__tags_2(RevInstrs, NoDeref, Tested).

:- pred vn_verify__tags_2(list(instruction), set(rval), set(lval)).
:- mode vn_verify__tags_2(in, in, in) is semidet.

vn_verify__tags_2([], _, _).
vn_verify__tags_2([Instr0 - _| RevInstrs], NoDeref0, Tested0) :-
	vn_verify__tags_instr(Instr0, NoDeref0, NoDeref1, Tested0, Tested1),
	vn_verify__tags_2(RevInstrs, NoDeref1, Tested1).

:- pred vn_verify__tags_instr(instr, set(rval), set(rval),
	set(lval), set(lval)).
% :- mode vn_verify__tags_instr(in, di, uo, di, uo) is semidet.
:- mode vn_verify__tags_instr(in, in, out, in, out) is semidet.

vn_verify__tags_instr(Instr, NoDeref0, NoDeref, Tested0, Tested) :-
	(
		Instr = comment(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = livevals(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = block(_, _, _),
		error("found block in vn_verify__tags_instr")
	;
		Instr = assign(Lval, Rval),
		vn_verify__tags_lval(Lval, NoDeref0),
		(
			set__member(lval(Lval), NoDeref0),
			vn_verify__tags_is_base(Rval, Base)
		->
			set__insert(NoDeref0, Base, NoDeref1)
		;
			NoDeref1 = NoDeref0
		),
		(
			set__member(Lval, Tested0)
		->
			vn_verify__tags_cond(lval(Lval), NoDeref1, NoDeref2,
				Tested0, Tested)
		;
			NoDeref2 = NoDeref1,
			Tested = Tested0
		),
		% Any appearance of Lval on the right hand side of the
		% assignment and in previous statements refers to its old value;
		% the new value is the one that should not be tested.
		set__delete(NoDeref2, lval(Lval), NoDeref),
		vn_verify__tags_rval(Rval, NoDeref)
	;
		Instr = call(_, _, _, _, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = mkframe(_, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = label(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = goto(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = computed_goto(_, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = c_code(_),
		error("found c_code in vn_verify__tags_instr")
	;
		Instr = if_val(Rval, _),
		vn_verify__tags_cond(Rval, NoDeref0, NoDeref,
			Tested0, Tested)
	;
		Instr = incr_hp(Lval, _, Rval, _),
		vn_verify__tags_lval(Lval, NoDeref0),
		vn_verify__tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = mark_hp(Lval),
		vn_verify__tags_lval(Lval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = restore_hp(Rval),
		vn_verify__tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = free_heap(Rval),
		vn_verify__tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = store_ticket(Lval),
		vn_verify__tags_lval(Lval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = reset_ticket(Rval, _Reason),
		vn_verify__tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = discard_ticket,
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = prune_ticket,
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = mark_ticket_stack(Lval),
		vn_verify__tags_lval(Lval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = prune_tickets_to(Rval),
		vn_verify__tags_rval(Rval, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = incr_sp(_, _),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = decr_sp(_),
		NoDeref = NoDeref0,
		Tested = Tested0
	;
		Instr = pragma_c(_, _, _, _, _, _, _),
		error("found c_code in vn_verify__tags_instr")
	).

:- pred vn_verify__tags_lval(lval, set(rval)).
:- mode vn_verify__tags_lval(in, in) is semidet.

vn_verify__tags_lval(reg(_, _), _).
vn_verify__tags_lval(stackvar(_), _).
vn_verify__tags_lval(framevar(_), _).
vn_verify__tags_lval(succip, _).
vn_verify__tags_lval(maxfr, _).
vn_verify__tags_lval(curfr, _).
vn_verify__tags_lval(prevfr(Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_lval(redoip(Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_lval(redofr(Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_lval(succip(Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_lval(succfr(Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_lval(hp, _).
vn_verify__tags_lval(sp, _).
vn_verify__tags_lval(field(_, Rval1, Rval2), NoDeref) :-
	\+ set__member(Rval1, NoDeref),
	vn_verify__tags_rval(Rval1, NoDeref),
	vn_verify__tags_rval(Rval2, NoDeref).
vn_verify__tags_lval(lvar(_), _) :-
	error("found lvar in vn_verify__tags_lval").
vn_verify__tags_lval(temp(_, _), _).

:- pred vn_verify__tags_rval(rval, set(rval)).
:- mode vn_verify__tags_rval(in, in) is semidet.

vn_verify__tags_rval(lval(Lval), NoDeref) :-
	vn_verify__tags_lval(Lval, NoDeref).
vn_verify__tags_rval(var(_), _) :-
	error("found var in vn_verify__tags_rval").
vn_verify__tags_rval(create(_, _, _, _, _, _, _), _).
vn_verify__tags_rval(mkword(_, Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_rval(const(_), _).
vn_verify__tags_rval(unop(_, Rval), NoDeref) :-
	vn_verify__tags_rval(Rval, NoDeref).
vn_verify__tags_rval(binop(_, Rval1, Rval2), NoDeref) :-
	vn_verify__tags_rval(Rval1, NoDeref),
	vn_verify__tags_rval(Rval2, NoDeref).

	% If the tag of an rval is tested in the condition of an if_val,
	% that rval should not be dereferenced in previous statements.
	% We therefore put this rval into NoDeref.

	% If an rval is tested in the condition of an if_val, and that
	% rval is defined as the tag of another rval, the second rval
	% should not be dereferenced in previous statements. We put the
	% first rval (all we can see here) into NoDeref; the rest has
	% to be done when handling assignments.

	% If the condition is simply the value of an lval, then we put
	% this lval into Tested. When processing an assignment to an lval
	% that is in tested, we process the right hand side of the assignment
	% as if it were the condition of an if_val (which in a way it is).

:- pred vn_verify__tags_cond(rval, set(rval), set(rval), set(lval), set(lval)).
% :- mode vn_verify__tags_cond(in, di, uo, di, uo) is semidet.
:- mode vn_verify__tags_cond(in, in, out, in, out) is semidet.

vn_verify__tags_cond(Cond, NoDeref0, NoDeref, Tested0, Tested) :-
	(
		Cond = binop(Binop, Rval1, Rval2),
		( Binop = eq ; Binop = (<) ; Binop = (>)
		; Binop = ne ; Binop = (<=) ; Binop = (>=) )
	->
		% ( Binop = eq ; Binop = ne ),
		( vn_verify__tags_is_base(Rval1, Base1) ->
			set__insert(NoDeref0, Base1, NoDeref1)
		;
			vn_verify__tags_rval(Rval1, NoDeref0),
			NoDeref1 = NoDeref0
		),
		( vn_verify__tags_is_base(Rval2, Base2) ->
			set__insert(NoDeref1, Base2, NoDeref)
		;
			vn_verify__tags_rval(Rval2, NoDeref1),
			NoDeref = NoDeref1
		),
		Tested = Tested0
	;
		Cond = binop(Binop, Rval1, Rval2),
		( Binop = (and) ; Binop = (or) )
	->
		vn_verify__tags_cond(Rval2,
			NoDeref0, NoDeref1, Tested0, Tested1),
		vn_verify__tags_cond(Rval1,
			NoDeref1, NoDeref, Tested1, Tested)
	;
		Cond = unop(Unop, Rval),
		Unop = (not)
	->
		vn_verify__tags_cond(Rval, NoDeref0, NoDeref,
			Tested0, Tested)
	;
		Cond = lval(Lval)
	->
		vn_verify__tags_rval(Cond, NoDeref0),
		NoDeref = NoDeref0,
		set__insert(Tested0, Lval, Tested)
	;
		vn_verify__tags_rval(Cond, NoDeref0),
		NoDeref = NoDeref0,
		Tested = Tested0
	).

:- pred vn_verify__tags_is_base(rval, rval).
:- mode vn_verify__tags_is_base(in, out) is semidet.

vn_verify__tags_is_base(Rval, Base) :-
	(
		Rval = unop(tag, Base)
	;
		Rval = lval(_),
		Base = Rval
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
