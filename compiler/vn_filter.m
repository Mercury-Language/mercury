%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_filter.m - filter out redundant temp assignments
% introduced by value numbering.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module vn_filter.

:- interface.

:- import_module llds.
:- import_module list.

:- pred vn_filter__block(list(instruction), list(instruction)).
:- mode vn_filter__block(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util.
:- import_module require, std_util.

vn_filter__block([], []).
vn_filter__block([Instr0 | Instrs0], Instrs) :-
	(
		Instr0 = Uinstr0 - _,
		Uinstr0 = assign(Temp, Defn),
		Temp = temp(_),
		opt_util__lvals_in_rval(Defn, Deps),
		vn_filter__can_substitute(Instrs0, Temp, Defn, Deps,
			Instrs1)
	->
		vn_filter__block(Instrs1, Instrs)
	;
		vn_filter__block(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

:- pred vn_filter__can_substitute(list(instruction), lval, rval, list(lval),
	list(instruction)).
:- mode vn_filter__can_substitute(in, in, in, in, out) is semidet.

vn_filter__can_substitute([], _, _, _, []).
vn_filter__can_substitute([Instr0 | Instrs0], Temp, Defn, Deps, Instrs) :-
	Instr0 = Uinstr0 - Comment,
	(
		vn_filter__user_instr(Uinstr0, yes(Rval)),
		opt_util__lvals_in_rval(Rval, Lvals),
		list__delete_first(Lvals, Temp, OtherLvals)
	->
		% We don't want to perform the subsitution
		% if Temp appears in the instruction more than once.
		\+ list__member(Temp, OtherLvals),
		\+ (
			vn_filter__defining_instr(Uinstr0, yes(Lval)),
			opt_util__lvals_in_lval(Lval, AccessLvals),
			list__member(Temp, AccessLvals)
		),
		vn_filter__replace_in_user_instr(Uinstr0, Temp, Defn, Uinstr1),
		Instrs = [Uinstr1 - Comment | Instrs0],
		vn_filter__instrs_free_of_lval(Instrs, Temp)
	;
		vn_filter__defining_instr(Uinstr0, yes(Lval))
	->
		(
			Lval = Temp
		->
			Instrs = [Instr0 | Instrs0]
		;
			list__member(Lval, Deps)
		->
			fail
		;
			opt_util__lvals_in_lval(Lval, AccessLvals),
			list__delete_first(AccessLvals, Temp, OtherAccessLvals)
		->
			\+ list__member(Temp, OtherAccessLvals),
			vn_filter__replace_in_defining_instr(Uinstr0,
				Temp, Defn, Uinstr1),
			Instrs = [Uinstr1 - Comment | Instrs0],
			vn_filter__instrs_free_of_lval(Instrs, Temp)
		;
			vn_filter__can_substitute(Instrs0, Temp, Defn, Deps,
				Instrs1),
			Instrs = [Instr0 | Instrs1]
		)
	;
		vn_filter__can_substitute(Instrs0, Temp, Defn, Deps, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

:- pred vn_filter__user_instr(instr, maybe(rval)).
:- mode vn_filter__user_instr(in, out) is det.

vn_filter__user_instr(comment(_), no).
vn_filter__user_instr(livevals(_), no).
vn_filter__user_instr(block(_, _), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__user_instr(assign(_, Rval), yes(Rval)).
vn_filter__user_instr(call(_, _, _, _), no).
vn_filter__user_instr(mkframe(_, _, _), no).
vn_filter__user_instr(modframe(_), no).
vn_filter__user_instr(label(_), no).
vn_filter__user_instr(goto(_), no).
vn_filter__user_instr(computed_goto(Rval, _), yes(Rval)).
vn_filter__user_instr(if_val(Rval, _), yes(Rval)).
vn_filter__user_instr(c_code(_), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__user_instr(incr_hp(_, _, Rval), yes(Rval)).
vn_filter__user_instr(mark_hp(_), no).
vn_filter__user_instr(restore_hp(Rval), yes(Rval)).
vn_filter__user_instr(store_ticket(_), no).
vn_filter__user_instr(restore_ticket(Rval), yes(Rval)).
vn_filter__user_instr(discard_ticket, no).
vn_filter__user_instr(incr_sp(_), no).
vn_filter__user_instr(decr_sp(_), no).
vn_filter__user_instr(pragma_c(_, _, _, _), _):-
	error("inappropriate instruction in vn__filter").

:- pred vn_filter__replace_in_user_instr(instr, lval, rval, instr).
:- mode vn_filter__replace_in_user_instr(in, in, in, out) is det.

vn_filter__replace_in_user_instr(comment(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(livevals(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(block(_, _), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_user_instr(assign(Lval, Rval0), Temp, Defn,
		assign(Lval, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(call(_, _, _, _), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(mkframe(_, _, _), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(modframe(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(label(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(goto(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(computed_goto(Rval0, Labels), Temp, Defn,
		computed_goto(Rval, Labels)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(if_val(Rval0, Label), Temp, Defn,
		if_val(Rval, Label)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(c_code(_), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_user_instr(incr_hp(Lval, Tag, Rval0), Temp, Defn,
		incr_hp(Lval, Tag, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(mark_hp(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(restore_hp(Rval0), Temp, Defn,
		restore_hp(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(store_ticket(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(restore_ticket(Rval0), Temp, Defn,
		restore_ticket(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(discard_ticket, _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(incr_sp(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(decr_sp(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(pragma_c(_, _, _, _), _, _, _):-
	error("inappropriate instruction in vn__filter").

:- pred vn_filter__defining_instr(instr, maybe(lval)).
:- mode vn_filter__defining_instr(in, out) is det.

vn_filter__defining_instr(comment(_), no).
vn_filter__defining_instr(livevals(_), no).
vn_filter__defining_instr(block(_, _), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__defining_instr(assign(Lval, _), yes(Lval)).
vn_filter__defining_instr(call(_, _, _, _), no).
vn_filter__defining_instr(mkframe(_, _, _), no).
vn_filter__defining_instr(modframe(_), no).
vn_filter__defining_instr(label(_), no).
vn_filter__defining_instr(goto(_), no).
vn_filter__defining_instr(computed_goto(_, _), no).
vn_filter__defining_instr(if_val(_, _), no).
vn_filter__defining_instr(c_code(_), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__defining_instr(incr_hp(Lval, _, _), yes(Lval)).
vn_filter__defining_instr(mark_hp(Lval), yes(Lval)).
vn_filter__defining_instr(restore_hp(_), no).
vn_filter__defining_instr(store_ticket(Lval), yes(Lval)).
vn_filter__defining_instr(restore_ticket(_), no).
vn_filter__defining_instr(discard_ticket, no).
vn_filter__defining_instr(incr_sp(_), no).
vn_filter__defining_instr(decr_sp(_), no).
vn_filter__defining_instr(pragma_c(_, _, _, _), _):-
	error("inappropriate instruction in vn__filter").

:- pred vn_filter__replace_in_defining_instr(instr, lval, rval, instr).
:- mode vn_filter__replace_in_defining_instr(in, in, in, out) is det.

vn_filter__replace_in_defining_instr(comment(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(livevals(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(block(_, _), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_defining_instr(assign(Lval0, Rval), Temp, Defn,
		assign(Lval, Rval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(call(_, _, _, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(mkframe(_, _, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(modframe(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(label(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(goto(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(computed_goto(_, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(if_val(_, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(c_code(_), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_defining_instr(incr_hp(Lval0, Tag, Rval), Temp, Defn,
		incr_hp(Lval, Tag, Rval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(mark_hp(Lval0), Temp, Defn,
		mark_hp(Lval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(restore_hp(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(store_ticket(Lval0), Temp, Defn,
		store_ticket(Lval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(restore_ticket(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(discard_ticket, _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(incr_sp(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(decr_sp(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(pragma_c(_, _, _, _), _, _, _):-
	error("inappropriate instruction in vn__filter").

:- pred vn_filter__replace_in_lval(lval, lval, rval, lval).
:- mode vn_filter__replace_in_lval(in, in, in, out) is det.

vn_filter__replace_in_lval(reg(R), _, _, reg(R)).
vn_filter__replace_in_lval(stackvar(N), _, _, stackvar(N)).
vn_filter__replace_in_lval(framevar(N), _, _, framevar(N)).
vn_filter__replace_in_lval(succip, _, _, succip).
vn_filter__replace_in_lval(maxfr, _, _, maxfr).
vn_filter__replace_in_lval(curfr, _, _, curfr).
vn_filter__replace_in_lval(succip(Rval0), Temp, Defn, succip(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_lval(redoip(Rval0), Temp, Defn, redoip(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_lval(succfr(Rval0), Temp, Defn, succfr(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_lval(prevfr(Rval0), Temp, Defn, prevfr(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_lval(hp, _, _, hp).
vn_filter__replace_in_lval(sp, _, _, sp).
vn_filter__replace_in_lval(field(Tag, Rval1, Rval2), Temp, Defn,
		field(Tag, Rval3, Rval4)) :-
	vn_filter__replace_in_rval(Rval1, Temp, Defn, Rval3),
	vn_filter__replace_in_rval(Rval2, Temp, Defn, Rval4).
vn_filter__replace_in_lval(lvar(_), _, _, _) :-
	error("found lvar in vn_filter__replace_in_lval").
vn_filter__replace_in_lval(temp(N), _, _, temp(N)).

:- pred vn_filter__replace_in_rval(rval, lval, rval, rval).
:- mode vn_filter__replace_in_rval(in, in, in, out) is det.

vn_filter__replace_in_rval(lval(Lval0), Temp, Defn, Rval) :-
	( Lval0 = Temp ->
		Rval = Defn
	;
		vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval),
		Rval = lval(Lval)
	).
vn_filter__replace_in_rval(var(_), _, _, _) :-
	error("found var in vn_filter__replace_in_rval").
vn_filter__replace_in_rval(create(Tag, Args, Label), _, _,
		create(Tag, Args, Label)).
vn_filter__replace_in_rval(mkword(Tag, Rval0), Temp, Defn, mkword(Tag, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_rval(const(Const), _, _, const(Const)).
vn_filter__replace_in_rval(unop(Unop, Rval0), Temp, Defn, unop(Unop, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_rval(binop(Binop, Rval1, Rval2), Temp, Defn,
		binop(Binop, Rval3, Rval4)) :-
	vn_filter__replace_in_rval(Rval1, Temp, Defn, Rval3),
	vn_filter__replace_in_rval(Rval2, Temp, Defn, Rval4).

:- pred vn_filter__instrs_free_of_lval(list(instruction), lval).
:- mode vn_filter__instrs_free_of_lval(in, in) is semidet.

vn_filter__instrs_free_of_lval([], _).
vn_filter__instrs_free_of_lval([Uinstr - _ | Instrs], Temp) :-
	( vn_filter__user_instr(Uinstr, yes(Rval)) ->
		opt_util__rval_free_of_lval(Rval, Temp)
	;
		true
	),
	( vn_filter__defining_instr(Uinstr, yes(Lval)) ->
		Lval \= Temp,
		opt_util__lval_access_rvals(Lval, Rvals),
		opt_util__rvals_free_of_lval(Rvals, Temp)
	;
		true
	),
	vn_filter__instrs_free_of_lval(Instrs, Temp).
