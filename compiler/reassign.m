%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: reassign.m
%
% Author: zs.
%
% This module implements an LLDS->LLDS transformation that optimizes
% away assignments to locations that already hold the assigned value.
% It operates entirely within extended basic blocks.
%
% It is intended for instruction sequences such as the following extract
% from tree234__search:
%
%	MR_r1 = MR_stackvar(3);
%	MR_r2 = MR_stackvar(4);
%	MR_r3 = MR_const_field(MR_mktag(1), MR_stackvar(1), (MR_Integer) 2);
%	MR_r4 = MR_stackvar(2);
%	MR_succip = (MR_Code *) MR_stackvar(5);
%	if ((MR_tag(MR_r3) != MR_mktag((MR_Integer) 1))) {
%		MR_GOTO_LABEL(mercury__x3__search_3_0_i1);
%	}
%	MR_stackvar(1) = MR_r3;
%	MR_stackvar(2) = MR_r4;
%	MR_stackvar(3) = MR_r1;
%	MR_stackvar(4) = MR_r2;
%	MR_r2 = MR_r4;
%	MR_r3 = MR_const_field(MR_mktag(1), MR_r3, (MR_Integer) 0);
%	MR_call_localret(...)
%
% The code before the if statement is part of the procedure epilogue; the code
% after it is the code from the initial part of the procedure that fulljump
% optimization replaces the self-tail-call with.
%
% The objective of this module is to remove assignments such as the assignments
% to stackvars 2, 3 and 4 above, in which the register assigned to the stackvar
% comes from the same stackvar in the first place.
%
% In general, for every assignment TargetLval = SourceRval, we record that
% TargetLval now contains SourceRval; if SourceRval is of the form
% lval(SourceLval), we also record that SourceLval now contains
% lval(TargetLval). Later on, if we find an assignment that assigns
% to an lval a value that it already holds, we remove the assignment.
% The removed assignment will either be a copy of the original assignment
% TargetLval = SourceRval, or its converse, SourceLval = lval(TargetLval).
% The mechanism that enables us to do this is a map that maps lvals
% (e.g. TargetLval) to its known contents (e.g. SourceRval).
%
% Of course, if any of the lvals occurring on the right hand side of an
% assignment change, we cannot remove a later copy of that assignment or
% of its converse. For example, we cannot remove the final assignment in
% the following code.
%
%	MR_r3 = MR_stackvar(1);
%	...
%	MR_stackvar(1) = MR_r2;
%	...
%	MR_r3 = MR_stackvar(1);
%
% We handle this by keeping track of which lvals an entry in the known contents
% map depends on. If one of these lvals is updated, we invalidate the dependent
% entries in the known contents map (i.e. we delete them).
%
% The lvals on which TargetLval depends include any lvals occurring inside it.
% We cannot optimize away the second assignment to the field below because
% even though the two field references are the same syntactically, they refer
% to different memory locations due to the update of MR_r5 between them.
%
%	MR_field(MR_mktag(1), MR_r5, 1) = r2;
%	...
%	MR_incr_hp(MR_r5, 4);
%	...
%	MR_field(MR_mktag(1), MR_r5, 1) = r2;
%
%
% The lvals on which TargetLval depends need not include TargetLval itself,
% since an assignment to TargetLval will in any case override the previous
% entry for TargetLval in the known contents map. This takes care of code
% sequences such as:
%
%	MR_r3 = MR_stackvar(1);
%	...
%	MR_r3 = MR_r2;
%	...
%	MR_r3 = MR_stackvar(1);
%
% The optimization makes conservative assumptions in several places, meaning
% it clobbers entries in the known contents map whenever an instruction *could*
% affect the entry, even if it in fact doesn't. For example, we clobber the
% known contents map at calls, labels and ticket resets.

%-----------------------------------------------------------------------------%

:- module ll_backend__reassign.

:- interface.

:- import_module ll_backend__llds.
:- import_module list.

:- pred remove_reassign(list(instruction)::in, list(instruction)::out) is det.

:- implementation.

:- import_module ll_backend__code_util.
:- import_module std_util, set, map, require.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type known_contents	==	map(lval, rval).
:- type dependent_lval_map	==	map(lval, set(lval)).

remove_reassign(Instrs0, Instrs) :-
	remove_reassign_loop(Instrs0, map__init, map__init, [], RevInstrs),
	list__reverse(RevInstrs, Instrs).

:- pred remove_reassign_loop(list(instruction)::in, known_contents::in,
	dependent_lval_map::in, list(instruction)::in, list(instruction)::out)
	is det.

remove_reassign_loop([], _, _, RevInstrs, RevInstrs).
remove_reassign_loop([Instr0 | Instrs0], KnownContentsMap0, DepLvalMap0,
		RevInstrs0, RevInstrs) :-
	Instr0 = Uinstr0 - _,
	(
		Uinstr0 = comment(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = livevals(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = block(_, _, _),
		error("remove_reassign_loop: block")
	;
		Uinstr0 = assign(Target, Source),
		(
			map__search(KnownContentsMap0, Target, KnownContents),
			KnownContents = Source
		->
				% By not including Instr0 in RevInstrs1,
				% we are deleting Instr0.
			RevInstrs1 = RevInstrs0,
			KnownContentsMap = KnownContentsMap0,
			DepLvalMap = DepLvalMap0
		;
			RevInstrs1 = [Instr0 | RevInstrs0],
			clobber_dependents(Target,
				KnownContentsMap0, KnownContentsMap1,
				DepLvalMap0, DepLvalMap1),
			(
				% For Targets of the following form, the code
				% generator ensures that the storage location
				% referred to by Target can only be updated
				% through the Target lval, and not through
				% some other lval, unless one uses mem_addr to
				% explicitly create an alias and mem_ref to
				% access the memory location via that alias.

				no_implicit_alias_target(Target)
			->
				record_known(Target, Source,
					KnownContentsMap1, KnownContentsMap,
					DepLvalMap1, DepLvalMap)
			;
				KnownContentsMap = KnownContentsMap1,
				DepLvalMap = DepLvalMap1
			)
		)
	;
		Uinstr0 = call(_, _, _, _, _, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The call may clobber any lval.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = mkframe(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = label(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% We don't know what is stored where at the
			% instructions that jump here.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = goto(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The value of KnownContentsMap doesn't really matter
			% since the next instruction (which must be a label) 
			% will reset it to empty anyway.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = computed_goto(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The value of KnownContentsMap doesn't really matter
			% since the next instruction (which must be a label) 
			% will reset it to empty anyway.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = c_code(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The C code may clobber any lval.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = if_val(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = incr_hp(Target, _, _, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
		clobber_dependents(Target,
			KnownContentsMap0, KnownContentsMap1,
			DepLvalMap0, DepLvalMap1),
		clobber_dependents(hp, KnownContentsMap1, KnownContentsMap,
			DepLvalMap1, DepLvalMap)
	;
		Uinstr0 = mark_hp(Target),
		RevInstrs1 = [Instr0 | RevInstrs0],
		clobber_dependents(Target, KnownContentsMap0, KnownContentsMap,
			DepLvalMap0, DepLvalMap)
	;
		Uinstr0 = restore_hp(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
		clobber_dependents(hp, KnownContentsMap0, KnownContentsMap,
			DepLvalMap0, DepLvalMap)
	;
		Uinstr0 = free_heap(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% There is no need to update KnownContentsMap since
			% later code should never refer to the freed cell.
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = store_ticket(Target),
		RevInstrs1 = [Instr0 | RevInstrs0],
		clobber_dependents(Target, KnownContentsMap0, KnownContentsMap,
			DepLvalMap0, DepLvalMap)
	;
		Uinstr0 = reset_ticket(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The reset operation may modify any lval.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = prune_ticket,
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = discard_ticket,
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = mark_ticket_stack(Target),
		RevInstrs1 = [Instr0 | RevInstrs0],
		clobber_dependents(Target, KnownContentsMap0, KnownContentsMap,
			DepLvalMap0, DepLvalMap)
	;
		Uinstr0 = prune_tickets_to(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
%	;
%		Uinstr0 = discard_tickets_to(_),
%		RevInstrs1 = [Instr0 | RevInstrs0],
%		KnownContentsMap = KnownContentsMap0,
%		DepLvalMap = DepLvalMap0
	;
		Uinstr0 = incr_sp(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% All stackvars now refer to new locations.
			% Rather than delete only stackvars from
			% KnownContentsMap0, we delete everything.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = decr_sp(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% All stackvars now refer to new locations.
			% Rather than delete only stackvars from
			% KnownContentsMap0, we delete everything.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = pragma_c(_, _, _, _, _, _, _, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The C code may clobber any lval.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = init_sync_term(Target, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
		clobber_dependents(Target, KnownContentsMap0, KnownContentsMap,
			DepLvalMap0, DepLvalMap)
	;
		Uinstr0 = fork(_, _, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% Both the parent and the child thread jump to labels
			% specified by the fork instruction, so the value of
			% KnownContentsMap doesn't really matter since the
			% next instruction (which must be a label) will
			% reset it to empty anyway.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = join_and_terminate(_),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% The value of KnownContentsMap doesn't really matter
			% since this instruction terminates the execution of
			% this thread.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		Uinstr0 = join_and_continue(_, _),
		RevInstrs1 = [Instr0 | RevInstrs0],
			% Other threads may modify any lval.
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	),
	remove_reassign_loop(Instrs0, KnownContentsMap, DepLvalMap,
		RevInstrs1, RevInstrs).

% Succeed iff the lval cannot have an alias created for it without the use of
% a mem_ref lval or an instruction with embedded C code, both of which cause
% us to clobber the known contents map.

:- pred no_implicit_alias_target(lval::in) is semidet.

no_implicit_alias_target(reg(_, _)).
no_implicit_alias_target(stackvar(_)).
no_implicit_alias_target(framevar(_)).

:- pred clobber_dependents(lval::in, known_contents::in, known_contents::out,
	dependent_lval_map::in, dependent_lval_map::out) is det.

clobber_dependents(Target, KnownContentsMap0, KnownContentsMap,
		DepLvalMap0, DepLvalMap) :-
	( map__search(DepLvalMap0, Target, DepLvals) ->
		set__fold(clobber_dependent, DepLvals,
			KnownContentsMap0, KnownContentsMap1),
		map__delete(DepLvalMap0, Target, DepLvalMap1)
	;
		KnownContentsMap1 = KnownContentsMap0,
		DepLvalMap1 = DepLvalMap0
	),
		% LLDS code can refer to arbitrary locations on the stack
		% or in the heap with mem_ref lvals. Since we don't keep track
		% of which locations have their addresses taken, on any
		% assignment through a mem_ref lval we throw way the known
		% contents map. This is a conservative approximation of the
		% desired behaviour, which would invalidate only the entries
		% of lvals that may be referred to via this mem_ref.
	code_util__lvals_in_rval(lval(Target), SubLvals),
	(
		list__member(SubLval, SubLvals),
		SubLval = mem_ref(_)
	->
		KnownContentsMap = map__init,
		DepLvalMap = map__init
	;
		KnownContentsMap = KnownContentsMap1,
		DepLvalMap = DepLvalMap1
	).

:- pred clobber_dependent(lval::in, known_contents::in, known_contents::out)
	is det.

clobber_dependent(Dependent, KnownContentsMap0, KnownContentsMap) :-
	map__delete(KnownContentsMap0, Dependent, KnownContentsMap).

:- pred record_known(lval::in, rval::in,
	known_contents::in, known_contents::out,
	dependent_lval_map::in, dependent_lval_map::out) is det.

record_known(TargetLval, SourceRval, KnownContentsMap0, KnownContentsMap,
		DepLvalMap0, DepLvalMap) :-
	code_util__lvals_in_rval(SourceRval, SourceSubLvals),
	( list__member(TargetLval, SourceSubLvals) ->
			% The act of assigning to TargetLval has modified
			% the value of SourceRval, so we can't eliminate
			% any copy of this assignment or its converse.
		KnownContentsMap = KnownContentsMap0,
		DepLvalMap = DepLvalMap0
	;
		record_known_lval_rval(TargetLval, SourceRval,
			KnownContentsMap0, KnownContentsMap1,
			DepLvalMap0, DepLvalMap1),
		( SourceRval = lval(SourceLval) ->
			record_known_lval_rval(SourceLval, lval(TargetLval),
				KnownContentsMap1, KnownContentsMap,
				DepLvalMap1, DepLvalMap)
		;
			KnownContentsMap = KnownContentsMap1,
			DepLvalMap = DepLvalMap1
		)
	).

:- pred record_known_lval_rval(lval::in, rval::in,
	known_contents::in, known_contents::out,
	dependent_lval_map::in, dependent_lval_map::out) is det.

record_known_lval_rval(TargetLval, SourceRval,
		KnownContentsMap0, KnownContentsMap,
		DepLvalMap0, DepLvalMap) :-
	( map__search(KnownContentsMap0, TargetLval, OldRval) ->
		% TargetLval no longer depends on the lvals in OldRval;
		% it depends on the lvals in SourceRval instead. If any lvals
		% occur in both, we delete TargetLval from their entries here
		% and will add it back in a few lines later on.
		%
		% TargetLval still depends on the lvals inside it.
		code_util__lvals_in_rval(OldRval, OldSubLvals),
		list__foldl(make_not_dependent(TargetLval), OldSubLvals,
			DepLvalMap0, DepLvalMap1)
	;
		DepLvalMap1 = DepLvalMap0
	),
	code_util__lvals_in_lval(TargetLval, TargetSubLvals),
	code_util__lvals_in_rval(SourceRval, SourceSubLvals),
	list__append(TargetSubLvals, SourceSubLvals, AllSubLvals),
	list__foldl(make_dependent(TargetLval), AllSubLvals,
		DepLvalMap1, DepLvalMap),
	map__set(KnownContentsMap0, TargetLval, SourceRval,
		KnownContentsMap).

:- pred make_not_dependent(lval::in, lval::in,
	dependent_lval_map::in, dependent_lval_map::out) is det.

make_not_dependent(Target, SubLval, DepLvalMap0, DepLvalMap) :-
	( map__search(DepLvalMap0, SubLval, DepLvals0) ->
		set__delete(DepLvals0, Target, DepLvals),
		map__det_update(DepLvalMap0, SubLval, DepLvals, DepLvalMap)
	;
		DepLvalMap = DepLvalMap0
	).

:- pred make_dependent(lval::in, lval::in,
	dependent_lval_map::in, dependent_lval_map::out) is det.

make_dependent(Target, SubLval, DepLvalMap0, DepLvalMap) :-
	( map__search(DepLvalMap0, SubLval, DepLvals0) ->
		set__insert(DepLvals0, Target, DepLvals),
		map__det_update(DepLvalMap0, SubLval, DepLvals, DepLvalMap)
	;
		DepLvals = set__make_singleton_set(Target),
		map__det_insert(DepLvalMap0, SubLval, DepLvals, DepLvalMap)
	).
