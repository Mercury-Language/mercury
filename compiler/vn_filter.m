%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
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

:- import_module code_util, opt_util.
:- import_module require, std_util.

	% Look for assignments to temp variables. If possible and profitable,
	% eliminate the assignment by substituting the value being assigned
	% for the later occurrences of the temp variable.

vn_filter__block([], []).
vn_filter__block([Instr0 | Instrs0], Instrs) :-
	(
		Instr0 = Uinstr0 - _,
		Uinstr0 = assign(Temp, Defn),
		Temp = temp(_, _),
		code_util__lvals_in_rval(Defn, Deps),
		vn_filter__can_substitute(Instrs0, Temp, Defn, Deps,
			Instrs1)
	->
		vn_filter__block(Instrs1, Instrs)
	;
		vn_filter__block(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

	% Check whether eliminating the assignment of Defn to Temp is
	% possible and profitable.
	%
	% Eliminating the assignment would require substituting Defn for
	% lval(Temp) in later instructions, up to the next definition of Temp.
	%
	% We cannot eliminate the assignment if a later reference that we
	% must substitute occurs after some code that has modified the value
	% of any location that occurs in Defn. (E.g. we cannot replace the
	% sequence t1 = r1; r1 = ...; ...t1... with r1 = ..., ...r1...)
	%
	% If there are two or more references to the value of Temp, then
	% it is not profitable to eliminate the assignment to it.

:- pred vn_filter__can_substitute(list(instruction), lval, rval, list(lval),
	list(instruction)).
:- mode vn_filter__can_substitute(in, in, in, in, out) is semidet.

vn_filter__can_substitute([], _, _, _, []).
vn_filter__can_substitute([Instr0 | Instrs0], Temp, Defn, Deps, Instrs) :-
	Instr0 = Uinstr0 - Comment,
	(
		vn_filter__user_instr(Uinstr0, yes(Rval)),
		code_util__lvals_in_rval(Rval, Lvals),
		list__delete_first(Lvals, Temp, OtherLvals)
	->
		% We don't want to perform the subsitution
		% if Temp appears in the instruction more than once.
		\+ list__member(Temp, OtherLvals),
		\+ (
			vn_filter__defining_instr(Uinstr0, yes(Lval)),
			code_util__lvals_in_lval(Lval, AccessLvals),
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
			code_util__lvals_in_lval(Lval, AccessLvals),
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

	% Check whether this instruction can possibly use the value
	% stored in a temp variable. If it can, it must refer to the
	% value via an rval; return that rval.
	%
	% Note that some of the instructions below can refer to the values of
	% variables such as r1, yet we consider that they cannot refer to the
	% values of temp variables. This is because value numbering does not
	% attempt to optimize them, and value numbering is the only part of
	% the compiler that can introduce temp variables.

:- pred vn_filter__user_instr(instr, maybe(rval)).
:- mode vn_filter__user_instr(in, out) is det.

vn_filter__user_instr(comment(_), no).
vn_filter__user_instr(livevals(_), no).
vn_filter__user_instr(block(_, _, _), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__user_instr(assign(_, Rval), yes(Rval)).
vn_filter__user_instr(call(_, _, _, _), no).
vn_filter__user_instr(mkframe(_, _), no).
vn_filter__user_instr(modframe(_), no).
vn_filter__user_instr(label(_), no).
vn_filter__user_instr(goto(_), no).
vn_filter__user_instr(computed_goto(Rval, _), yes(Rval)).
vn_filter__user_instr(if_val(Rval, _), yes(Rval)).
vn_filter__user_instr(c_code(_), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__user_instr(incr_hp(_, _, Rval, _), yes(Rval)).
vn_filter__user_instr(mark_hp(_), no).
vn_filter__user_instr(restore_hp(Rval), yes(Rval)).
vn_filter__user_instr(store_ticket(_), no).
vn_filter__user_instr(reset_ticket(Rval, _Reason), yes(Rval)).
vn_filter__user_instr(discard_ticket, no).
vn_filter__user_instr(mark_ticket_stack(_), no).
vn_filter__user_instr(discard_tickets_to(Rval), yes(Rval)).
vn_filter__user_instr(incr_sp(_, _), no).
vn_filter__user_instr(decr_sp(_), no).
vn_filter__user_instr(pragma_c(_, _, _, _, _), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__user_instr(init_sync_term(_, _), _):-
	error("init_sync_term instruction in vn__filter").
vn_filter__user_instr(fork(_, _, _), _):-
	error("fork instruction in vn__filter").
vn_filter__user_instr(join_and_terminate(_), _):-
	error("join_and_terminate instruction in vn__filter").
vn_filter__user_instr(join_and_continue(_, _), _):-
	error("join_and_continue instruction in vn__filter").

	% vn_filter__replace_in_user_instr(Instr0, Old, New, Instr):
	% Given that Instr0 refers to the values of some locations,
	% replace all occurrences of lval(Old) with New in those uses,
	% returning the result as Instr.

:- pred vn_filter__replace_in_user_instr(instr, lval, rval, instr).
:- mode vn_filter__replace_in_user_instr(in, in, in, out) is det.

vn_filter__replace_in_user_instr(comment(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(livevals(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(block(_, _, _), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_user_instr(assign(Lval, Rval0), Temp, Defn,
		assign(Lval, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(call(_, _, _, _), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(mkframe(_, _), _, _, _) :-
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
vn_filter__replace_in_user_instr(incr_hp(Lval, Tag, Rval0, Msg), Temp, Defn,
		incr_hp(Lval, Tag, Rval, Msg)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(mark_hp(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(restore_hp(Rval0), Temp, Defn,
		restore_hp(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(store_ticket(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(reset_ticket(Rval0, Reason), Temp, Defn,
		reset_ticket(Rval, Reason)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(discard_ticket, _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(mark_ticket_stack(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(discard_tickets_to(Rval0), Temp, Defn,
		discard_tickets_to(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_user_instr(incr_sp(_, _), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(decr_sp(_), _, _, _) :-
	error("non-user instruction in vn_filter__replace_in_user_instr").
vn_filter__replace_in_user_instr(pragma_c(_, _, _, _, _), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_user_instr(init_sync_term(_, _), _, _, _):-
	error("init_sync_term instruction in vn__filter").
vn_filter__replace_in_user_instr(fork(_, _, _), _, _, _):-
	error("fork instruction in vn__filter").
vn_filter__replace_in_user_instr(join_and_terminate(_), _, _, _):-
	error("join_and_terminate instruction in vn__filter").
vn_filter__replace_in_user_instr(join_and_continue(_, _), _, _, _):-
	error("join_and_continue instruction in vn__filter").

	% Check whether this instruction defines the value of any lval.

:- pred vn_filter__defining_instr(instr, maybe(lval)).
:- mode vn_filter__defining_instr(in, out) is det.

vn_filter__defining_instr(comment(_), no).
vn_filter__defining_instr(livevals(_), no).
vn_filter__defining_instr(block(_, _, _), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__defining_instr(assign(Lval, _), yes(Lval)).
vn_filter__defining_instr(call(_, _, _, _), no).
vn_filter__defining_instr(mkframe(_, _), no).
vn_filter__defining_instr(modframe(_), no).
vn_filter__defining_instr(label(_), no).
vn_filter__defining_instr(goto(_), no).
vn_filter__defining_instr(computed_goto(_, _), no).
vn_filter__defining_instr(if_val(_, _), no).
vn_filter__defining_instr(c_code(_), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__defining_instr(incr_hp(Lval, _, _, _), yes(Lval)).
vn_filter__defining_instr(mark_hp(Lval), yes(Lval)).
vn_filter__defining_instr(restore_hp(_), no).
vn_filter__defining_instr(store_ticket(Lval), yes(Lval)).
vn_filter__defining_instr(reset_ticket(_, _), no).
vn_filter__defining_instr(discard_ticket, no).
vn_filter__defining_instr(mark_ticket_stack(Lval), yes(Lval)).
vn_filter__defining_instr(discard_tickets_to(_), no).
vn_filter__defining_instr(incr_sp(_, _), no).
vn_filter__defining_instr(decr_sp(_), no).
vn_filter__defining_instr(pragma_c(_, _, _, _, _), _):-
	error("inappropriate instruction in vn__filter").
vn_filter__defining_instr(init_sync_term(_, _), _):-
	error("init_sync_term instruction in vn__filter").
vn_filter__defining_instr(fork(_, _, _), _):-
	error("fork instruction in vn__filter").
vn_filter__defining_instr(join_and_terminate(_), _):-
	error("join_and_terminate instruction in vn__filter").
vn_filter__defining_instr(join_and_continue(_, _), _):-
	error("join_and_continue instruction in vn__filter").

	% vn_filter__replace_in_defining_instr(Instr0, Old, New, Instr):
	% Given that Instr0 defines the value of a location,
	% replace all occurrences of lval(Old) with New in the access path
	% of that location, returning the result as Instr.

:- pred vn_filter__replace_in_defining_instr(instr, lval, rval, instr).
:- mode vn_filter__replace_in_defining_instr(in, in, in, out) is det.

vn_filter__replace_in_defining_instr(comment(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(livevals(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(block(_, _, _), _, _, _):-
	error("inappropriate instruction in vn__filter").
vn_filter__replace_in_defining_instr(assign(Lval0, Rval), Temp, Defn,
		assign(Lval, Rval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(call(_, _, _, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(mkframe(_, _), _, _, _) :-
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
vn_filter__replace_in_defining_instr(incr_hp(Lval0, Tag, Rval, Msg), Temp, Defn,
		incr_hp(Lval, Tag, Rval, Msg)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(mark_hp(Lval0), Temp, Defn,
		mark_hp(Lval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(restore_hp(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(store_ticket(Lval0), Temp, Defn,
		store_ticket(Lval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(reset_ticket(_, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(discard_ticket, _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(mark_ticket_stack(Lval0), Temp, Defn,
		mark_ticket_stack(Lval)) :-
	vn_filter__replace_in_lval(Lval0, Temp, Defn, Lval).
vn_filter__replace_in_defining_instr(discard_tickets_to(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(incr_sp(_, _), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(decr_sp(_), _, _, _) :-
	error("non-def instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(init_sync_term(_, _), _, _, _):-
	error("init_sync_term instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(fork(_, _, _), _, _, _):-
	error("fork instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(join_and_terminate(_), _, _, _):-
	error("join_and_terminate instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(join_and_continue(_, _), _, _, _):-
	error("join_and_continue instruction in vn_filter__replace_in_defining_instr").
vn_filter__replace_in_defining_instr(pragma_c(_, _, _, _, _), _, _, _):-
	error("inappropriate instruction in vn__filter").

	% vn_filter__replace_in_lval(Lval0, Old, New, Lval):
	% Replace all occurrences of Old with New in Lval0,
	% returning the result as Lval.

:- pred vn_filter__replace_in_lval(lval, lval, rval, lval).
:- mode vn_filter__replace_in_lval(in, in, in, out) is det.

vn_filter__replace_in_lval(reg(T, N), _, _, reg(T, N)).
vn_filter__replace_in_lval(stackvar(N), _, _, stackvar(N)).
vn_filter__replace_in_lval(framevar(N), _, _, framevar(N)).
vn_filter__replace_in_lval(succip, _, _, succip).
vn_filter__replace_in_lval(maxfr, _, _, maxfr).
vn_filter__replace_in_lval(curfr, _, _, curfr).
vn_filter__replace_in_lval(succip(Rval0), Temp, Defn, succip(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_lval(redoip(Rval0), Temp, Defn, redoip(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_lval(redofr(Rval0), Temp, Defn, redofr(Rval)) :-
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
vn_filter__replace_in_lval(temp(T, N), _, _, temp(T, N)).
vn_filter__replace_in_lval(mem_ref(Rval0), Temp, Defn, mem_ref(Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).

	% vn_filter__replace_in_rval(Rval0, Old, New, Rval):
	% Replace all occurrences of lval(Old) with New in Rval0,
	% returning the result as Rval.

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
vn_filter__replace_in_rval(create(Tag, Args, Unique, Label, Msg), _, _,
		create(Tag, Args, Unique, Label, Msg)).
vn_filter__replace_in_rval(mkword(Tag, Rval0), Temp, Defn, mkword(Tag, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_rval(const(Const), _, _, const(Const)).
vn_filter__replace_in_rval(unop(Unop, Rval0), Temp, Defn, unop(Unop, Rval)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).
vn_filter__replace_in_rval(binop(Binop, Rval1, Rval2), Temp, Defn,
		binop(Binop, Rval3, Rval4)) :-
	vn_filter__replace_in_rval(Rval1, Temp, Defn, Rval3),
	vn_filter__replace_in_rval(Rval2, Temp, Defn, Rval4).
vn_filter__replace_in_rval(mem_addr(MemRef0), Temp, Defn, mem_addr(MemRef)) :-
	vn_filter__replace_in_mem_ref(MemRef0, Temp, Defn, MemRef).

	% vn_filter__replace_in_mem_ref(Ref0, Old, New, Ref):
	% Replace all occurrences of lval(Old) with New in Ref0,
	% returning the result as Ref.

:- pred vn_filter__replace_in_mem_ref(mem_ref, lval, rval, mem_ref).
:- mode vn_filter__replace_in_mem_ref(in, in, in, out) is det.

vn_filter__replace_in_mem_ref(stackvar_ref(N), _, _, stackvar_ref(N)).
vn_filter__replace_in_mem_ref(framevar_ref(N), _, _, framevar_ref(N)).
vn_filter__replace_in_mem_ref(heap_ref(Rval0, Tag, N), Temp, Defn,
		heap_ref(Rval, Tag, N)) :-
	vn_filter__replace_in_rval(Rval0, Temp, Defn, Rval).

	% Succeed if the given list of instructions does not mention
	% the given lval.

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
