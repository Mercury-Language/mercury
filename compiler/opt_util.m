%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Utilities for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module opt_util.

:- interface.
:- import_module bool, list, std_util.
:- import_module llds.

:- type instrmap == map(label, instruction).
:- type lvalmap == map(label, maybe(instruction)).
:- type tailmap == map(label, list(instruction)).
:- type succmap == map(label, bool).

:- pred opt_util__get_prologue(list(instruction), proc_label, instruction,
	list(instruction), list(instruction)).
:- mode opt_util__get_prologue(in, out, out, out, out) is det.

:- pred opt_util__gather_comments(list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__gather_comments(in, out, out) is det.

:- pred opt_util__gather_comments_livevals(list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__gather_comments_livevals(in, out, out) is det.

:- pred opt_util__skip_comments(list(instruction), list(instruction)).
% :- mode opt_util__skip_comments(di, uo) is det.
:- mode opt_util__skip_comments(in, out) is det.

:- pred opt_util__skip_comments_livevals(list(instruction), list(instruction)).
:- mode opt_util__skip_comments_livevals(in, out) is det.

:- pred opt_util__skip_comments_labels(list(instruction), list(instruction)).
:- mode opt_util__skip_comments_labels(in, out) is det.

:- pred opt_util__skip_comments_livevals_labels(list(instruction),
	list(instruction)).
:- mode opt_util__skip_comments_livevals_labels(in, out) is det.

	% Find the next modframe if it is guaranteed to be reached from here

:- pred opt_util__next_modframe(list(instruction), list(instruction),
	code_addr, list(instruction), list(instruction)).
:- mode opt_util__next_modframe(in, in, out, out, out) is semidet.

	% See if these instructions touch nondet stack controls.

:- pred opt_util__touches_nondet_ctrl(list(instruction), bool).
:- mode opt_util__touches_nondet_ctrl(in, out) is det.

	% Find the instructions up to and including
	% the next one that cannot fall through

:- pred opt_util__find_no_fallthrough(list(instruction), list(instruction)).
:- mode opt_util__find_no_fallthrough(in, out) is det.

	% Find the first label in the instruction stream.

:- pred opt_util__find_first_label(list(instruction), label).
:- mode opt_util__find_first_label(in, out) is det.

	% Skip to the next label, returning the code before the label,
	% and the label together with the code after the label.

:- pred opt_util__skip_to_next_label(list(instruction),
	list(instruction), list(instruction)).
% :- mode opt_util__skip_to_next_label(di, uo, uo) is det.
:- mode opt_util__skip_to_next_label(in, out, out) is det.

	% Check whether the named label follows without any intervening code.
	% If yes, return the instructions after the label.

:- pred opt_util__is_this_label_next(label, list(instruction),
	list(instruction)).
:- mode opt_util__is_this_label_next(in, in, out) is semidet.

 	% Is a proceed instruction (i.e. a goto(succip) instruction)
 	% next in the instruction list, possibly preceded by a restoration
 	% of succip and a det stack frame removal? If yes, return the
 	% instructions up to the proceed.

:- pred opt_util__is_proceed_next(list(instruction), list(instruction)).
:- mode opt_util__is_proceed_next(in, out) is semidet.

 	% Is a proceed instruction (i.e. a goto(succip) instruction)
 	% next in the instruction list, possibly preceded by an assignment
	% to r1, a restoration of succip and a det stack frame removal?
	% If yes, return the instructions up to the proceed.

:- pred opt_util__is_sdproceed_next(list(instruction), list(instruction)).
:- mode opt_util__is_sdproceed_next(in, out) is semidet.

	% Same as the previous predicate, but also return whether it is
	% a success or a fail.

:- pred opt_util__is_sdproceed_next_sf(list(instruction), list(instruction),
	bool).
:- mode opt_util__is_sdproceed_next_sf(in, out, out) is semidet.

 	% Is a succeed instruction (i.e. a goto(do_succeed(_)) instruction)
 	% next in the instruction list? If yes, return the instructions
	% up to and including the succeed.

:- pred opt_util__is_succeed_next(list(instruction), list(instruction)).
:- mode opt_util__is_succeed_next(in, out) is semidet.

 	% Is the following code a test of r1, followed in both continuations
	% by a semidet proceed? Is the code in both continuations the same,
	% modulo livevals annotations and the value assigned to r1? Is TRUE
	% assigned to r1 in the success continuation and FALSE in the failure
	% continuation? If the answer is yes to all these questions, return
	% the code shared by the two continuations.

:- pred opt_util__is_forkproceed_next(list(instruction), tailmap,
	list(instruction)).
:- mode opt_util__is_forkproceed_next(in, in, out) is semidet.

	% Remove the assignment to r1 from the list returned by
	% opt_util__is_sdproceed_next.

:- pred opt_util__filter_out_r1(list(instruction), maybe(rval_const),
	list(instruction)).
:- mode opt_util__filter_out_r1(in, out, out) is det.

 	% Does the following code consist of straighline instructions
	% that do not modify nondet frame linkages, plus possibly
	% if_val(..., dofail), and then a succeed?
	% If yes, then return all the instructions up to the succeed,
	% and all the following instructions.

:- pred opt_util__straight_alternative(list(instruction), list(instruction),
	list(instruction)).
:- mode opt_util__straight_alternative(in, out, out) is semidet.

	% Find and return the initial sequence of instructions that do not
	% refer to stackvars and do not branch.

:- pred opt_util__no_stack_straight_line(list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__no_stack_straight_line(in, out, out) is det.

	% Remove the labels from a block of code for jumpopt.

:- pred opt_util__filter_out_labels(list(instruction), list(instruction)).
:- mode opt_util__filter_out_labels(in, out) is det.

	% Remove any livevals instructions that do not precede an instruction
	% that needs one.

:- pred opt_util__filter_out_bad_livevals(list(instruction), list(instruction)).
:- mode opt_util__filter_out_bad_livevals(in, out) is det.

	% Remove the livevals instruction from the list returned by
	% opt_util__is_proceed_next.

:- pred opt_util__filter_out_livevals(list(instruction), list(instruction)).
:- mode opt_util__filter_out_livevals(in, out) is det.

	% Get just the livevals instructions from a list of instructions.

:- pred opt_util__filter_in_livevals(list(instruction), list(instruction)).
:- mode opt_util__filter_in_livevals(in, out) is det.

	% See if the condition of an if-then-else is constant,
	% and if yes, whether the branch will be taken or not.

:- pred opt_util__is_const_condition(rval, bool).
:- mode opt_util__is_const_condition(in, out) is semidet.

	% Check whether an instruction can possibly branch away.

:- pred opt_util__can_instr_branch_away(instr, bool).
:- mode opt_util__can_instr_branch_away(in, out) is det.

	% Check whether an instruction can possibly fall through
	% to the next instruction without using its label.

:- pred opt_util__can_instr_fall_through(instr, bool).
:- mode opt_util__can_instr_fall_through(in, out) is det.

	% Check whether a code_addr, when the target of a goto, represents
	% either a call or a proceed/succeed; if so, it is the end of an
	% extended basic block and needs a livevals in front of it.

:- pred opt_util__livevals_addr(code_addr, bool).
:- mode opt_util__livevals_addr(in, out) is det.

	% Determine all the labels and code addresses which are referenced
	% by an instruction. The code addresses that are labels are returned
	% in both output arguments.

:- pred opt_util__instr_labels(instr, list(label), list(code_addr)).
:- mode opt_util__instr_labels(in, out, out) is det.

	% Determine all the labels and code addresses which are referenced
	% by a list of instructions.

:- pred opt_util__instr_list_labels(list(instruction),
	list(label), list(code_addr)).
:- mode opt_util__instr_list_labels(in, out, out) is det.

	% Find a label number that does not occur in the instruction list,
	% starting the search at a given number.

:- pred opt_util__new_label_no(list(instruction), int, int).
:- mode opt_util__new_label_no(in, in, out) is det.

	% Find the maximum temp variable number used.

:- pred opt_util__count_temps_instr_list(list(instruction), int, int, int, int).
:- mode opt_util__count_temps_instr_list(in, in, out, in, out) is det.

:- pred opt_util__count_temps_instr(instr, int, int, int, int).
:- mode opt_util__count_temps_instr(in, in, out, in, out) is det.

	% See whether an lval references any stackvars.

:- pred opt_util__lval_refers_stackvars(lval, bool).
:- mode opt_util__lval_refers_stackvars(in, out) is det.

	% See whether an rval references any stackvars.

:- pred opt_util__rval_refers_stackvars(rval, bool).
:- mode opt_util__rval_refers_stackvars(in, out) is det.

	% See whether a list of maybe rvals references any stackvars.

:- pred opt_util__rvals_refer_stackvars(list(maybe(rval)), bool).
:- mode opt_util__rvals_refer_stackvars(in, out) is det.

	% See whether instructions until the next decr_sp (if any) refer to
	% any stackvars or branch away. If not, return the instructions up to
	% the decr_sp. A restoration of succip from the bottom stack slot
	% is allowed; this instruction is not returned in the output.
	% The same thing applies to assignments to detstackvars; these are
	% not useful if we throw away the stack frame.

:- pred opt_util__no_stackvars_til_decr_sp(list(instruction), int,
	list(instruction), list(instruction)).
% :- mode opt_util__no_stackvars_til_decr_sp(di, in, uo, uo) is semidet.
:- mode opt_util__no_stackvars_til_decr_sp(in, in, out, out) is semidet.

	% See whether a list of instructions references any stackvars.

:- pred opt_util__block_refers_stackvars(list(instruction), bool).
:- mode opt_util__block_refers_stackvars(in, out) is det.

	% Format a label for verbose messages during compilation

:- pred opt_util__format_label(label, string).
:- mode opt_util__format_label(in, out) is det.

	% Find out if an instruction sequence has both incr_sp and decr_sp.

:- pred opt_util__has_both_incr_decr_sp(list(instruction)).
:- mode opt_util__has_both_incr_decr_sp(in) is semidet.

	% Find out what rvals, if any, are needed to access an lval.

:- pred opt_util__lval_access_rvals(lval, list(rval)).
:- mode opt_util__lval_access_rvals(in, out) is det.

	% See whether an rval is free of references to a given lval.

:- pred opt_util__rval_free_of_lval(rval, lval).
:- mode opt_util__rval_free_of_lval(in, in) is semidet.

	% See whether a list of rvals is free of references to a given lval.

:- pred opt_util__rvals_free_of_lval(list(rval), lval).
:- mode opt_util__rvals_free_of_lval(in, in) is semidet.

	% Return the set of lvals referenced in an rval.

:- pred opt_util__lvals_in_rval(rval, list(lval)).
:- mode opt_util__lvals_in_rval(in, out) is det.

	% Return the set of lvals referenced in an lval.

:- pred opt_util__lvals_in_lval(lval, list(lval)).
:- mode opt_util__lvals_in_lval(in, out) is det.

	% Count the number of hp increments in a block of code.

:- pred opt_util__count_incr_hp(list(instruction), int).
:- mode opt_util__count_incr_hp(in, out) is det.

	% Whenever the input list of instructions contains two livevals
	% pseudo-ops without an intervening no-fall-through instruction,
	% ensure that the first of these registers as live every lval
	% that is live in the second, except those that are assigned to
	% by intervening instructions. This makes the shadowing of the
	% second livevals by the first benign.

:- pred opt_util__propagate_livevals(list(instruction), list(instruction)).
:- mode opt_util__propagate_livevals(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exprn_aux, llds_out, hlds_pred.
:- import_module int, string, set, map, require.

opt_util__get_prologue(Instrs0, ProcLabel, LabelInstr, Comments, Instrs) :-
	opt_util__gather_comments(Instrs0, Comments1, Instrs1),
	(
		Instrs1 = [Instr1 | Instrs2],
		Instr1 = label(FirstLabel) - _
	->
		LabelInstr = Instr1,
		( FirstLabel = exported(ProcLabelPrime) ->
			ProcLabel = ProcLabelPrime
		; FirstLabel = local(ProcLabelPrime) ->
			ProcLabel = ProcLabelPrime
		;
			error("procedure begins with bad label type")
		),
		opt_util__gather_comments(Instrs2, Comments2, Instrs),
		list__append(Comments1, Comments2, Comments)
	;
		error("procedure does not begin with label")
	).

opt_util__gather_comments(Instrs0, Comments, Instrs) :-
	(
		Instrs0 = [Instr0 | Instrs1],
		Instr0 = comment(_) - _
	->
		opt_util__gather_comments(Instrs1, Comments0, Instrs),
		Comments = [Instr0 | Comments0]
	;
		Instrs = Instrs0,
		Comments = []
	).

opt_util__gather_comments_livevals(Instrs0, Comments, Instrs) :-
	(
		Instrs0 = [Instr0 | Instrs1],
		( Instr0 = comment(_) - _ ; Instr0 = livevals(_) - _ )
	->
		opt_util__gather_comments_livevals(Instrs1, Comments0, Instrs),
		Comments = [Instr0 | Comments0]
	;
		Instrs = Instrs0,
		Comments = []
	).

	% Given a list of instructions, skip past any comment instructions
	% at the start and return the remaining instructions.
	% We do this because comment instructions get in the way of
	% peephole optimization.

opt_util__skip_comments(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		opt_util__skip_comments(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

opt_util__skip_comments_livevals(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		opt_util__skip_comments(Instrs1, Instrs)
	; Instrs0 = [livevals(_) - _ | Instrs1] ->
		opt_util__skip_comments_livevals(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

opt_util__skip_comments_labels(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		opt_util__skip_comments_labels(Instrs1, Instrs)
	; Instrs0 = [label(_) - _ | Instrs1] ->
		opt_util__skip_comments_labels(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

opt_util__skip_comments_livevals_labels(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		opt_util__skip_comments_livevals_labels(Instrs1, Instrs)
	; Instrs0 = [livevals(_) - _ | Instrs1] ->
		opt_util__skip_comments_livevals_labels(Instrs1, Instrs)
	; Instrs0 = [label(_) - _ | Instrs1] ->
		opt_util__skip_comments_livevals_labels(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

opt_util__next_modframe([Instr | Instrs], RevSkip, Redoip, Skip, Rest) :-
	Instr = Uinstr - _Comment,
	(
		Uinstr = modframe(Redoip0)
	->
		Redoip = Redoip0,
		list__reverse(RevSkip, Skip),
		Rest = Instrs
	;
		Uinstr = assign(redoip(lval(Fr)),
			const(code_addr_const(Redoip0))),
		( Fr = maxfr ; Fr = curfr )
	->
		Redoip = Redoip0,
		list__reverse(RevSkip, Skip),
		Rest = Instrs
	;
		Uinstr = mkframe(_, _, _)
	->
		fail
	;
		opt_util__can_instr_branch_away(Uinstr, Canbranchaway),
		( Canbranchaway = no ->
			opt_util__next_modframe(Instrs, [Instr | RevSkip],
				Redoip, Skip, Rest)
		;
			fail
		)
	).

opt_util__find_no_fallthrough([], []).
opt_util__find_no_fallthrough([Instr0 | Instrs0], Instrs) :-
	(
		Instr0 = Uinstr0 - _,
		opt_util__can_instr_fall_through(Uinstr0, no)
	->
		Instrs = [Instr0]
	;
		opt_util__find_no_fallthrough(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

opt_util__find_first_label([], _) :-
	error("cannot find first label").
opt_util__find_first_label([Instr0 | Instrs0], Label) :-
	( Instr0 = label(LabelPrime) - _ ->
		Label = LabelPrime
	;
		opt_util__find_first_label(Instrs0, Label)
	).

opt_util__skip_to_next_label([], [], []).
opt_util__skip_to_next_label([Instr0 | Instrs0], Before, Remain) :-
	( Instr0 = label(_) - _ ->
		Before = [],
		Remain = [Instr0 | Instrs0]
	;
		opt_util__skip_to_next_label(Instrs0, Before1, Remain),
		Before = [Instr0 | Before1]
	).

opt_util__is_this_label_next(Label, [Instr | Moreinstr], Remainder) :-
	Instr = Uinstr - _Comment,
	( Uinstr = comment(_) ->
		opt_util__is_this_label_next(Label, Moreinstr, Remainder)
	; Uinstr = livevals(_) ->
		% this is questionable
		opt_util__is_this_label_next(Label, Moreinstr, Remainder)
	; Uinstr = label(NextLabel) ->
		( Label = NextLabel ->
			Remainder = Moreinstr
		;
			opt_util__is_this_label_next(Label, Moreinstr,
				Remainder)
		)
	;
		fail
	).

opt_util__is_proceed_next(Instrs0, InstrsBetween) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	( Instr1 = assign(succip, lval(stackvar(_))) - _ ->
		Instr1use = Instr1,
		opt_util__skip_comments_labels(Instrs2, Instrs3)
	;
		Instr1use = comment("no succip restoration") - "",
		Instrs3 = Instrs1
	),
	Instrs3 = [Instr3 | Instrs4],
	( Instr3 = decr_sp(_) - _ ->
		Instr3use = Instr3,
		opt_util__skip_comments_labels(Instrs4, Instrs5)
	;
		Instr3use = comment("no sp restoration") - "",
		Instrs5 = Instrs3
	),
	Instrs5 = [Instr5 | Instrs6],
	Instr5 = livevals(_) - _,
	opt_util__skip_comments_labels(Instrs6, Instrs7),
	Instrs7 = [Instr7 | _],
	Instr7 = goto(succip) - _,
	InstrsBetween = [Instr1use, Instr3use, Instr5].

opt_util__is_sdproceed_next(Instrs0, InstrsBetween) :-
	opt_util__is_sdproceed_next_sf(Instrs0, InstrsBetween, _).

opt_util__is_sdproceed_next_sf(Instrs0, InstrsBetween, Success) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	( Instr1 = assign(succip, lval(stackvar(_))) - _ ->
		Instr1use = Instr1,
		opt_util__skip_comments_labels(Instrs2, Instrs3)
	;
		Instr1use = comment("no succip restoration") - "",
		Instrs3 = Instrs1
	),
	Instrs3 = [Instr3 | Instrs4],
	( Instr3 = decr_sp(_) - _ ->
		Instr3use = Instr3,
		opt_util__skip_comments_labels(Instrs4, Instrs5)
	;
		Instr3use = comment("no sp restoration") - "",
		Instrs5 = Instrs3
	),
	Instrs5 = [Instr5 | Instrs6],
	Instr5 = assign(reg(r, 1), const(R1val)) - _,
	(
		R1val = true,
		Success = yes
	;
		R1val = false,
		Success = no
	),
	opt_util__skip_comments_labels(Instrs6, Instrs7),
	Instrs7 = [Instr7 | Instrs8],
	Instr7 = livevals(_) - _,
	opt_util__skip_comments_labels(Instrs8, Instrs9),
	Instrs9 = [Instr9 | _],
	Instr9 = goto(succip) - _,
	InstrsBetween = [Instr1use, Instr3use, Instr5, Instr7].

opt_util__is_succeed_next(Instrs0, InstrsBetweenIncl) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = livevals(_) - _,
	opt_util__skip_comments_labels(Instrs2, Instrs3),
	Instrs3 = [Instr3 | _],
	Instr3 = goto(do_succeed(_)) - _,
	InstrsBetweenIncl = [Instr1, Instr3].

opt_util__is_forkproceed_next(Instrs0, Sdprocmap, Between) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	( Instr1 = if_val(lval(reg(r, 1)), label(JumpLabel)) - _ ->
		map__search(Sdprocmap, JumpLabel, BetweenJump),
		opt_util__is_sdproceed_next(Instrs2, BetweenFall),
		opt_util__filter_out_r1(BetweenJump, yes(true), BetweenTrue0),
		opt_util__filter_out_livevals(BetweenTrue0, Between),
		opt_util__filter_out_r1(BetweenFall, yes(false), BetweenFalse0),
		opt_util__filter_out_livevals(BetweenFalse0, Between)
	; Instr1 = if_val(unop(not, lval(reg(r, 1))), label(JumpLabel)) - _ ->
		map__search(Sdprocmap, JumpLabel, BetweenJump),
		opt_util__is_sdproceed_next(Instrs2, BetweenFall),
		opt_util__filter_out_r1(BetweenJump, yes(false), BetweenFalse0),
		opt_util__filter_out_livevals(BetweenFalse0, Between),
		opt_util__filter_out_r1(BetweenFall, yes(true), BetweenTrue0),
		opt_util__filter_out_livevals(BetweenTrue0, Between)
	;
		fail
	).

opt_util__filter_out_r1([], no, []).
opt_util__filter_out_r1([Instr0 | Instrs0], Success, Instrs) :-
	opt_util__filter_out_r1(Instrs0, Success0, Instrs1),
	( Instr0 = assign(reg(r, 1), const(Success1)) - _ ->
		Instrs = Instrs1,
		Success = yes(Success1)
	;
		Instrs = [Instr0 | Instrs1],
		Success = Success0
	).

opt_util__straight_alternative(Instrs0, Between, After) :-
	opt_util__straight_alternative_2(Instrs0, [], BetweenRev, After),
	list__reverse(BetweenRev, Between).

:- pred opt_util__straight_alternative_2(list(instruction), list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__straight_alternative_2(in, in, out, out) is semidet.

opt_util__straight_alternative_2([Instr0 | Instrs0], Between0, Between,
		After) :-
	Instr0 = Uinstr0 - _,
	(
		(
			opt_util__can_instr_branch_away(Uinstr0, no),
			opt_util__touches_nondet_ctrl_instr(Uinstr0, no)
		;
			Uinstr0 = if_val(_, CodeAddr),
			( CodeAddr = do_fail ; CodeAddr = do_redo )
		)
	->
		opt_util__straight_alternative_2(Instrs0, [Instr0 | Between0],
			Between, After)
	;
		Uinstr0 = goto(do_succeed(no))
	->
		Between = Between0,
		After = Instrs0
	;
		fail
	).

opt_util__no_stack_straight_line(Instrs0, Shuffle, Instrs) :-
	opt_util__no_stack_straight_line_2(Instrs0, [], RevShuffle, Instrs),
	list__reverse(RevShuffle, Shuffle).

:- pred opt_util__no_stack_straight_line_2(list(instruction),
	list(instruction), list(instruction), list(instruction)).
:- mode opt_util__no_stack_straight_line_2(in, in, out, out) is det.

opt_util__no_stack_straight_line_2([], After, After, []).
opt_util__no_stack_straight_line_2([Instr0 | Instrs0], After0, After, Instrs) :-
	Instr0 = Uinstr - _,
	(
		(
			Uinstr = comment(_)
		;
			Uinstr = livevals(_)
		;
			Uinstr = assign(Lval, Rval),
			opt_util__lval_refers_stackvars(Lval, no),
			opt_util__rval_refers_stackvars(Rval, no)
		)
	->
		After1 = [Instr0 | After0],
		opt_util__no_stack_straight_line_2(Instrs0, After1, After, Instrs)
	;
		After = After0,
		Instrs = [Instr0 | Instrs0]
	).

opt_util__lval_refers_stackvars(reg(_, _), no).
opt_util__lval_refers_stackvars(stackvar(_), yes).
opt_util__lval_refers_stackvars(framevar(_), yes).
opt_util__lval_refers_stackvars(succip, no).
opt_util__lval_refers_stackvars(maxfr, no).
opt_util__lval_refers_stackvars(curfr, no).
opt_util__lval_refers_stackvars(succfr(Rval), Refers) :-
	%% I'm not 100% sure of what we should do here, so
	%% to be safe, just abort. I don't think this code
	%% is used anyway. -fjh.
	% error("found succfr in lval_refers_stackvars").
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(prevfr(Rval), Refers) :-
	%% I'm not 100% sure of what we should do here, so
	%% to be safe, just abort. I don't think this code
	%% is used anyway. -fjh.
	%error("found prevfr in lval_refers_stackvars").
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(redoip(Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(succip(Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(hp, no).
opt_util__lval_refers_stackvars(sp, no).
opt_util__lval_refers_stackvars(field(_, Rval, FieldNum), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers1),
	opt_util__rval_refers_stackvars(FieldNum, Refers2),
	bool__or(Refers1, Refers2, Refers).
opt_util__lval_refers_stackvars(lvar(_), _) :-
	error("found lvar in lval_refers_stackvars").
opt_util__lval_refers_stackvars(temp(_, _), no).
opt_util__lval_refers_stackvars(mem_ref(Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).

:- pred opt_util__mem_ref_refers_stackvars(mem_ref, bool).
:- mode opt_util__mem_ref_refers_stackvars(in, out) is det.

opt_util__mem_ref_refers_stackvars(stackvar_ref(_), yes).
opt_util__mem_ref_refers_stackvars(framevar_ref(_), yes).
opt_util__mem_ref_refers_stackvars(heap_ref(Rval, _, _), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).

opt_util__rval_refers_stackvars(lval(Lval), Refers) :-
	opt_util__lval_refers_stackvars(Lval, Refers).
opt_util__rval_refers_stackvars(var(_), _) :-
	error("found var in rval_refers_stackvars").
opt_util__rval_refers_stackvars(create(_, Rvals, _, _, _), Refers) :-
	opt_util__rvals_refer_stackvars(Rvals, Refers).
opt_util__rval_refers_stackvars(mkword(_, Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__rval_refers_stackvars(const(_), no).
opt_util__rval_refers_stackvars(unop(_, Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__rval_refers_stackvars(binop(_, Rval1, Rval2), Refers) :-
	opt_util__rval_refers_stackvars(Rval1, Refers1),
	opt_util__rval_refers_stackvars(Rval2, Refers2),
	bool__or(Refers1, Refers2, Refers).
opt_util__rval_refers_stackvars(mem_addr(MemRef), Refers) :-
	opt_util__mem_ref_refers_stackvars(MemRef, Refers).

opt_util__rvals_refer_stackvars([], no).
opt_util__rvals_refer_stackvars([MaybeRval | Tail], Refers) :-
	(
		(
			MaybeRval = no
		;
			MaybeRval = yes(Rval),
			opt_util__rval_refers_stackvars(Rval, no)
		)
	->
		opt_util__rvals_refer_stackvars(Tail, Refers)
	;
		Refers = yes
	).

opt_util__no_stackvars_til_decr_sp([Instr0 | Instrs0], FrameSize,
		Between, Remain) :-
	Instr0 = Uinstr0 - _,
	(
		Uinstr0 = comment(_),
		opt_util__no_stackvars_til_decr_sp(Instrs0, FrameSize,
			Between0, Remain),
		Between = [Instr0 | Between0]
	;
		Uinstr0 = livevals(_),
		opt_util__no_stackvars_til_decr_sp(Instrs0, FrameSize,
			Between0, Remain),
		Between = [Instr0 | Between0]
	;
		Uinstr0 = assign(Lval, Rval),
		(
			Lval = stackvar(_),
			opt_util__rval_refers_stackvars(Rval, no)
		->
			opt_util__no_stackvars_til_decr_sp(Instrs0, FrameSize,
				Between, Remain)
		;
			Lval = succip,
			Rval = lval(stackvar(FrameSize)),
			opt_util__skip_comments(Instrs0, Instrs1),
			Instrs1 = [decr_sp(FrameSize) - _ | Instrs2]
		->
			Between = [],
			Remain = Instrs2
		;
			opt_util__lval_refers_stackvars(Lval, no),
			opt_util__rval_refers_stackvars(Rval, no),
			opt_util__no_stackvars_til_decr_sp(Instrs0, FrameSize,
				Between0, Remain),
			Between = [Instr0 | Between0]
		)
	;
		Uinstr0 = incr_hp(Lval, _, Rval, _),
		opt_util__lval_refers_stackvars(Lval, no),
		opt_util__rval_refers_stackvars(Rval, no),
		opt_util__no_stackvars_til_decr_sp(Instrs0, FrameSize,
			Between0, Remain),
		Between = [Instr0 | Between0]
	;
		Uinstr0 = decr_sp(FrameSize),
		Between = [],
		Remain = Instrs0
	).

opt_util__block_refers_stackvars([], no).
opt_util__block_refers_stackvars([Uinstr0 - _ | Instrs0], Need) :-
	(
		Uinstr0 = comment(_),
		opt_util__block_refers_stackvars(Instrs0, Need)
	;
		Uinstr0 = livevals(_),
		opt_util__block_refers_stackvars(Instrs0, Need)
	;
		Uinstr0 = block(_, _, BlockInstrs),
		opt_util__block_refers_stackvars(BlockInstrs, Need)
	;
		Uinstr0 = assign(Lval, Rval),
		opt_util__lval_refers_stackvars(Lval, Use1),
		opt_util__rval_refers_stackvars(Rval, Use2),
		bool__or(Use1, Use2, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = call(_, _, _, _),
		Need = no
	;
		Uinstr0 = mkframe(_, _, _),
		Need = no
	;
		Uinstr0 = modframe(_),
		Need = no
	;
		Uinstr0 = label(_),
		Need = no
	;
		Uinstr0 = goto(_),
		Need = no
	;
		Uinstr0 = computed_goto(Rval, _),
		opt_util__rval_refers_stackvars(Rval, Use),
		( Use = yes ->
			Need = yes
		;
			Need = no
		)
	;
		Uinstr0 = c_code(_),
		Need = no
	;
		Uinstr0 = if_val(Rval, _),
		opt_util__rval_refers_stackvars(Rval, Use),
		( Use = yes ->
			Need = yes
		;
			Need = no
		)
	;
		Uinstr0 = incr_hp(Lval, _, Rval, _),
		opt_util__lval_refers_stackvars(Lval, Use1),
		opt_util__rval_refers_stackvars(Rval, Use2),
		bool__or(Use1, Use2, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = mark_hp(Lval),
		opt_util__lval_refers_stackvars(Lval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = restore_hp(Rval),
		opt_util__rval_refers_stackvars(Rval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = store_ticket(Lval),
		opt_util__lval_refers_stackvars(Lval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = reset_ticket(Rval, _Reason),
		opt_util__rval_refers_stackvars(Rval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = discard_ticket,
		opt_util__block_refers_stackvars(Instrs0, Need)
	;
		Uinstr0 = mark_ticket_stack(Lval),
		opt_util__lval_refers_stackvars(Lval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = discard_tickets_to(Rval),
		opt_util__rval_refers_stackvars(Rval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		% handled specially
		Uinstr0 = incr_sp(_, _),
		Need = no
	;
		% handled specially
		Uinstr0 = decr_sp(_),
		Need = no
	;
		Uinstr0 = pragma_c(_, _, _, _, _),
		Need = no
	).

opt_util__filter_out_labels([], []).
opt_util__filter_out_labels([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_out_labels(Instrs0, Instrs1),
	( Instr0 = label(_) - _ ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).

opt_util__filter_out_bad_livevals([], []).
opt_util__filter_out_bad_livevals([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_out_bad_livevals(Instrs0, Instrs1),
	(
		Instr0 = livevals(_) - _,
		opt_util__skip_comments(Instrs1, Instrs2),
		Instrs2 = [Uinstr2 - _ | _],
		opt_util__can_use_livevals(Uinstr2, no)
	->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).

opt_util__filter_out_livevals([], []).
opt_util__filter_out_livevals([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_out_livevals(Instrs0, Instrs1),
	( Instr0 = livevals(_) - _ ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).

opt_util__filter_in_livevals([], []).
opt_util__filter_in_livevals([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_in_livevals(Instrs0, Instrs1),
	( Instr0 = livevals(_) - _ ->
		Instrs = [Instr0 | Instrs1]
	;
		Instrs = Instrs1
	).

	% We recognize only a subset of all constant conditions.
	% The time to extend this predicate is when the rest of the compiler
	% generates more complicated constant conditions.

opt_util__is_const_condition(const(Const), Taken) :-
	( Const = true ->
		Taken = yes
	; Const = false ->
		Taken = no
	;
		error("non-boolean constant as if-then-else condition")
	).
opt_util__is_const_condition(unop(Op, Rval1), Taken) :-
	Op = (not),
	opt_util__is_const_condition(Rval1, Taken1),
	bool__not(Taken1, Taken).
opt_util__is_const_condition(binop(Op, Rval1, Rval2), Taken) :-
	Op = eq,
	Rval1 = Rval2,
	Taken = yes.

opt_util__new_label_no([], N, N).
opt_util__new_label_no([Instr0 | Instrs0], N0, N) :-
	(
		Instr0 = label(local(_, K)) - _,
		K >= N0
	->
		N1 is K + 1
	;
		N1 = N0
	),
	opt_util__new_label_no(Instrs0, N1, N).

opt_util__can_instr_branch_away(comment(_), no).
opt_util__can_instr_branch_away(livevals(_), no).
opt_util__can_instr_branch_away(block(_, _, _), yes).
opt_util__can_instr_branch_away(assign(_, _), no).
opt_util__can_instr_branch_away(call(_, _, _, _), yes).
opt_util__can_instr_branch_away(mkframe(_, _, _), no).
opt_util__can_instr_branch_away(modframe(_), no).
opt_util__can_instr_branch_away(label(_), no).
opt_util__can_instr_branch_away(goto(_), yes).
opt_util__can_instr_branch_away(computed_goto(_, _), yes).
opt_util__can_instr_branch_away(c_code(_), no).
opt_util__can_instr_branch_away(if_val(_, _), yes).
opt_util__can_instr_branch_away(incr_hp(_, _, _, _), no).
opt_util__can_instr_branch_away(mark_hp(_), no).
opt_util__can_instr_branch_away(restore_hp(_), no).
opt_util__can_instr_branch_away(store_ticket(_), no).
opt_util__can_instr_branch_away(reset_ticket(_, _), no).
opt_util__can_instr_branch_away(discard_ticket, no).
opt_util__can_instr_branch_away(mark_ticket_stack(_), no).
opt_util__can_instr_branch_away(discard_tickets_to(_), no).
opt_util__can_instr_branch_away(incr_sp(_, _), no).
opt_util__can_instr_branch_away(decr_sp(_), no).
opt_util__can_instr_branch_away(pragma_c(_, _, _, _, _), no).

opt_util__can_instr_fall_through(comment(_), yes).
opt_util__can_instr_fall_through(livevals(_), yes).
opt_util__can_instr_fall_through(block(_, _, Instrs), FallThrough) :-
	opt_util__can_block_fall_through(Instrs, FallThrough).
opt_util__can_instr_fall_through(assign(_, _), yes).
opt_util__can_instr_fall_through(call(_, _, _, _), no).
opt_util__can_instr_fall_through(mkframe(_, _, _), yes).
opt_util__can_instr_fall_through(modframe(_), yes).
opt_util__can_instr_fall_through(label(_), yes).
opt_util__can_instr_fall_through(goto(_), no).
opt_util__can_instr_fall_through(computed_goto(_, _), no).
opt_util__can_instr_fall_through(c_code(_), yes).
opt_util__can_instr_fall_through(if_val(_, _), yes).
opt_util__can_instr_fall_through(incr_hp(_, _, _, _), yes).
opt_util__can_instr_fall_through(mark_hp(_), yes).
opt_util__can_instr_fall_through(restore_hp(_), yes).
opt_util__can_instr_fall_through(store_ticket(_), yes).
opt_util__can_instr_fall_through(reset_ticket(_, _), yes).
opt_util__can_instr_fall_through(discard_ticket, yes).
opt_util__can_instr_fall_through(mark_ticket_stack(_), yes).
opt_util__can_instr_fall_through(discard_tickets_to(_), yes).
opt_util__can_instr_fall_through(incr_sp(_, _), yes).
opt_util__can_instr_fall_through(decr_sp(_), yes).
opt_util__can_instr_fall_through(pragma_c(_, _, _, _, _), yes).

	% Check whether an instruction sequence can possibly fall through
	% to the next instruction without using its label.

:- pred opt_util__can_block_fall_through(list(instruction), bool).
:- mode opt_util__can_block_fall_through(in, out) is det.

opt_util__can_block_fall_through([], yes).
opt_util__can_block_fall_through([Instr - _ | Instrs], FallThrough) :-
	( opt_util__can_instr_fall_through(Instr, no) ->
		FallThrough = no
	;
		opt_util__can_block_fall_through(Instrs, FallThrough)
	).

:- pred opt_util__can_use_livevals(instr, bool).
:- mode opt_util__can_use_livevals(in, out) is det.

opt_util__can_use_livevals(comment(_), no).
opt_util__can_use_livevals(livevals(_), no).
opt_util__can_use_livevals(block(_, _, _), no).
opt_util__can_use_livevals(assign(_, _), no).
opt_util__can_use_livevals(call(_, _, _, _), yes).
opt_util__can_use_livevals(mkframe(_, _, _), no).
opt_util__can_use_livevals(modframe(_), no).
opt_util__can_use_livevals(label(_), no).
opt_util__can_use_livevals(goto(_), yes).
opt_util__can_use_livevals(computed_goto(_, _), no).
opt_util__can_use_livevals(c_code(_), no).
opt_util__can_use_livevals(if_val(_, _), yes).
opt_util__can_use_livevals(incr_hp(_, _, _, _), no).
opt_util__can_use_livevals(mark_hp(_), no).
opt_util__can_use_livevals(restore_hp(_), no).
opt_util__can_use_livevals(store_ticket(_), no).
opt_util__can_use_livevals(reset_ticket(_, _), no).
opt_util__can_use_livevals(discard_ticket, no).
opt_util__can_use_livevals(mark_ticket_stack(_), no).
opt_util__can_use_livevals(discard_tickets_to(_), no).
opt_util__can_use_livevals(incr_sp(_, _), no).
opt_util__can_use_livevals(decr_sp(_), no).
opt_util__can_use_livevals(pragma_c(_, _, _, _, _), no).

% determine all the labels and code_addresses that are referenced by Instr

opt_util__instr_labels(Instr, Labels, CodeAddrs) :-
	opt_util__instr_labels_2(Instr, Labels0, CodeAddrs1),
	opt_util__instr_rvals_and_lvals(Instr, Rvals, Lvals),
	exprn_aux__rval_list_addrs(Rvals, CodeAddrs2, _),
	exprn_aux__lval_list_addrs(Lvals, CodeAddrs3, _),
	list__append(CodeAddrs1, CodeAddrs2, CodeAddrs12),
	list__append(CodeAddrs12, CodeAddrs3, CodeAddrs),
	opt_util__find_label_code_addrs(CodeAddrs, Labels0, Labels).

:- pred opt_util__find_label_code_addrs(list(code_addr),
	list(label), list(label)). 
:- mode opt_util__find_label_code_addrs(in, in, out) is det.

	% Find out which code addresses are also labels.

opt_util__find_label_code_addrs([], Labels, Labels).
opt_util__find_label_code_addrs([CodeAddr | Rest], Labels0, Labels) :-
	( CodeAddr = label(Label) ->
		Labels1 = [Label | Labels0]
	;
		Labels1 = Labels0
	),
	opt_util__find_label_code_addrs(Rest, Labels1, Labels).

:- pred opt_util__instr_labels_2(instr, list(label), list(code_addr)).
:- mode opt_util__instr_labels_2(in, out, out) is det.

% determine all the labels and code_addresses that are directly
% referenced by an instruction (not counting ones referenced indirectly
% via rvals or lvals)

opt_util__instr_labels_2(comment(_), [], []).
opt_util__instr_labels_2(livevals(_), [], []).
opt_util__instr_labels_2(block(_, _, Instrs), Labels, CodeAddrs) :-
	opt_util__instr_list_labels(Instrs, Labels, CodeAddrs).
opt_util__instr_labels_2(assign(_,_), [], []).
opt_util__instr_labels_2(call(Target, Ret, _, _), [], [Target, Ret]).
opt_util__instr_labels_2(mkframe(_, _, Addr), [], [Addr]).
opt_util__instr_labels_2(modframe(Addr), [], [Addr]).
opt_util__instr_labels_2(label(_), [], []).
opt_util__instr_labels_2(goto(Addr), [], [Addr]).
opt_util__instr_labels_2(computed_goto(_, Labels), Labels, []).
opt_util__instr_labels_2(c_code(_), [], []).
opt_util__instr_labels_2(if_val(_, Addr), [], [Addr]).
opt_util__instr_labels_2(incr_hp(_, _, _, _), [], []).
opt_util__instr_labels_2(mark_hp(_), [], []).
opt_util__instr_labels_2(restore_hp(_), [], []).
opt_util__instr_labels_2(store_ticket(_), [], []).
opt_util__instr_labels_2(reset_ticket(_, _), [], []).
opt_util__instr_labels_2(discard_ticket, [], []).
opt_util__instr_labels_2(mark_ticket_stack(_), [], []).
opt_util__instr_labels_2(discard_tickets_to(_), [], []).
opt_util__instr_labels_2(incr_sp(_, _), [], []).
opt_util__instr_labels_2(decr_sp(_), [], []).
opt_util__instr_labels_2(pragma_c(_, _, _, _, _), [], []).

:- pred opt_util__instr_rvals_and_lvals(instr, list(rval), list(lval)).
:- mode opt_util__instr_rvals_and_lvals(in, out, out) is det.

% determine all the rvals and lvals referenced by an instruction

opt_util__instr_rvals_and_lvals(comment(_), [], []).
opt_util__instr_rvals_and_lvals(livevals(_), [], []).
opt_util__instr_rvals_and_lvals(block(_, _, Instrs), Labels, CodeAddrs) :-
	opt_util__instr_list_rvals_and_lvals(Instrs, Labels, CodeAddrs).
opt_util__instr_rvals_and_lvals(assign(Lval,Rval), [Rval], [Lval]).
opt_util__instr_rvals_and_lvals(call(_, _, _, _), [], []).
opt_util__instr_rvals_and_lvals(mkframe(_, _, _), [], []).
opt_util__instr_rvals_and_lvals(modframe(_), [], []).
opt_util__instr_rvals_and_lvals(label(_), [], []).
opt_util__instr_rvals_and_lvals(goto(_), [], []).
opt_util__instr_rvals_and_lvals(computed_goto(Rval, _), [Rval], []).
opt_util__instr_rvals_and_lvals(c_code(_), [], []).
opt_util__instr_rvals_and_lvals(if_val(Rval, _), [Rval], []).
opt_util__instr_rvals_and_lvals(incr_hp(Lval, _, Rval, _), [Rval], [Lval]).
opt_util__instr_rvals_and_lvals(mark_hp(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(restore_hp(Rval), [Rval], []).
opt_util__instr_rvals_and_lvals(store_ticket(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(reset_ticket(Rval, _Reason), [Rval], []).
opt_util__instr_rvals_and_lvals(discard_ticket, [], []).
opt_util__instr_rvals_and_lvals(mark_ticket_stack(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(discard_tickets_to(Rval), [Rval], []).
opt_util__instr_rvals_and_lvals(incr_sp(_, _), [], []).
opt_util__instr_rvals_and_lvals(decr_sp(_), [], []).
opt_util__instr_rvals_and_lvals(pragma_c(_, In, _, Out, _), Rvals, Lvals) :-
	pragma_c_inputs_get_rvals(In, Rvals),
	pragma_c_outputs_get_lvals(Out, Lvals).

	% extract the rvals from the pragma_c_input
:- pred pragma_c_inputs_get_rvals(list(pragma_c_input), list(rval)).
:- mode pragma_c_inputs_get_rvals(in, out) is det.

pragma_c_inputs_get_rvals([], []).
pragma_c_inputs_get_rvals([I|Inputs], [R|Rvals]) :-
	I = pragma_c_input(_Name, _Type, R),
	pragma_c_inputs_get_rvals(Inputs, Rvals).

	% extract the lvals from the pragma_c_output
:- pred pragma_c_outputs_get_lvals(list(pragma_c_output), list(lval)).
:- mode pragma_c_outputs_get_lvals(in, out) is det.

pragma_c_outputs_get_lvals([], []).
pragma_c_outputs_get_lvals([O|Outputs], [L|Lvals]) :-
	O = pragma_c_output(L, _Type, _Name),
	pragma_c_outputs_get_lvals(Outputs, Lvals).

% determine all the rvals and lvals referenced by a list of instructions

:- pred opt_util__instr_list_rvals_and_lvals(list(pair(instr, string)),
						list(rval), list(lval)).
:- mode opt_util__instr_list_rvals_and_lvals(in, out, out) is det.

opt_util__instr_list_rvals_and_lvals([], [], []).
opt_util__instr_list_rvals_and_lvals([Instr - _|Instrs], Rvals, Lvals) :-
	opt_util__instr_rvals_and_lvals(Instr, Rvals0, Lvals0),
	opt_util__instr_list_rvals_and_lvals(Instrs, Rvals1, Lvals1),
	list__append(Rvals0, Rvals1, Rvals),
	list__append(Lvals0, Lvals1, Lvals).

opt_util__instr_list_labels([], [], []).
opt_util__instr_list_labels([Uinstr - _ | Instrs], Labels, CodeAddrs) :-
	opt_util__instr_labels(Uinstr, Labels0, CodeAddrs0),
	opt_util__instr_list_labels(Instrs, Labels1, CodeAddrs1),
	list__append(Labels0, Labels1, Labels),
	list__append(CodeAddrs0, CodeAddrs1, CodeAddrs).

opt_util__livevals_addr(label(Label), Result) :-
	( Label = local(_, _) ->
		Result = no
	;	
		Result = yes
	).
opt_util__livevals_addr(imported(_), yes).
opt_util__livevals_addr(succip, yes).
opt_util__livevals_addr(do_succeed(_), yes).
opt_util__livevals_addr(do_redo, no).
opt_util__livevals_addr(do_fail, no).
opt_util__livevals_addr(do_det_closure, yes).
opt_util__livevals_addr(do_semidet_closure, yes).
opt_util__livevals_addr(do_nondet_closure, yes).
opt_util__livevals_addr(do_not_reached, no).

opt_util__count_temps_instr_list([], R, R, F, F).
opt_util__count_temps_instr_list([Uinstr - _Comment | Instrs], R0, R, F0, F) :-
	opt_util__count_temps_instr(Uinstr, R0, R1, F0, F1),
	opt_util__count_temps_instr_list(Instrs, R1, R, F1, F).

opt_util__count_temps_instr(comment(_), R, R, F, F).
opt_util__count_temps_instr(livevals(_), R, R, F, F).
opt_util__count_temps_instr(block(_, _, _), R, R, F, F).
opt_util__count_temps_instr(assign(Lval, Rval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R1, F0, F1),
	opt_util__count_temps_rval(Rval, R1, R, F1, F).
opt_util__count_temps_instr(call(_, _, _, _), R, R, F, F).
opt_util__count_temps_instr(mkframe(_, _, _), R, R, F, F).
opt_util__count_temps_instr(modframe(_), R, R, F, F).
opt_util__count_temps_instr(label(_), R, R, F, F).
opt_util__count_temps_instr(goto(_), R, R, F, F).
opt_util__count_temps_instr(computed_goto(Rval, _), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(if_val(Rval, _), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(c_code(_), R, R, F, F).
opt_util__count_temps_instr(incr_hp(Lval, _, Rval, _), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R1, F0, F1),
	opt_util__count_temps_rval(Rval, R1, R, F1, F).
opt_util__count_temps_instr(mark_hp(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(restore_hp(Rval), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(store_ticket(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(reset_ticket(Rval, _Reason), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(discard_ticket, R, R, F, F).
opt_util__count_temps_instr(mark_ticket_stack(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(discard_tickets_to(Rval), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(incr_sp(_, _), R, R, F, F).
opt_util__count_temps_instr(decr_sp(_), R, R, F, F).
opt_util__count_temps_instr(pragma_c(_, _, _, _, _), R, R, F, F).

:- pred opt_util__count_temps_lval(lval, int, int, int, int).
:- mode opt_util__count_temps_lval(in, in, out, in, out) is det.

opt_util__count_temps_lval(Lval, R0, R, F0, F) :-
	( Lval = temp(Type, N) ->
		(
			Type = r,
			int__max(R0, N, R),
			F = F0
		;
			Type = f,
			int__max(F0, N, F),
			R = R0
		)
	; Lval = field(_, Rval, FieldNum) ->
		opt_util__count_temps_rval(Rval, R0, R1, F0, F1),
		opt_util__count_temps_rval(FieldNum, R1, R, F1, F)
	;
		R = R0,
		F = F0
	).

:- pred opt_util__count_temps_rval(rval, int, int, int, int).
:- mode opt_util__count_temps_rval(in, in, out, in, out) is det.

% XXX assume that we don't generate code
% that uses a temp var without defining it.
opt_util__count_temps_rval(_, R, R, F, F).

opt_util__format_label(local(ProcLabel, _), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).
opt_util__format_label(c_local(ProcLabel), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).
opt_util__format_label(local(ProcLabel), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).
opt_util__format_label(exported(ProcLabel), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).

:- pred opt_util__format_proclabel(proc_label, string).
:- mode opt_util__format_proclabel(in, out) is det.

opt_util__format_proclabel(proc(_Module, _PredOrFunc, _, Name, Arity, ProcId),
		Str) :-
	string__int_to_string(Arity, ArityStr),
	proc_id_to_int(ProcId, Mode),
	string__int_to_string(Mode, ModeStr),
	string__append_list([Name, "/", ArityStr, " mode ", ModeStr], Str).
opt_util__format_proclabel(special_proc(_Module, Pred, _, Type, Arity, ProcId),
		Str) :-
	string__int_to_string(Arity, ArityStr),
	proc_id_to_int(ProcId, Mode),
	string__int_to_string(Mode, ModeStr),
	string__append_list(
		[Pred, "_", Type, "/", ArityStr, " mode ", ModeStr], Str).

opt_util__has_both_incr_decr_sp(Instrs) :-
	opt_util__has_both_incr_decr_sp_2(Instrs, no, yes, no, yes).

:- pred opt_util__has_both_incr_decr_sp_2(list(instruction),
	bool, bool, bool, bool).
:- mode opt_util__has_both_incr_decr_sp_2(in, in, out, in, out) is det.

opt_util__has_both_incr_decr_sp_2([], HasIncr, HasIncr, HasDecr, HasDecr).
opt_util__has_both_incr_decr_sp_2([Uinstr - _ | Instrs],
		HasIncr0, HasIncr, HasDecr0, HasDecr) :-
	( Uinstr = incr_sp(_, _) ->
		HasIncr1 = yes
	;
		HasIncr1 = HasIncr0
	),
	( Uinstr = decr_sp(_) ->
		HasDecr1 = yes
	;
		HasDecr1 = HasDecr0
	),
	opt_util__has_both_incr_decr_sp_2(Instrs,
		HasIncr1, HasIncr, HasDecr1, HasDecr).

opt_util__touches_nondet_ctrl([], no).
opt_util__touches_nondet_ctrl([Uinstr - _ | Instrs], Touch) :-
	opt_util__touches_nondet_ctrl_instr(Uinstr, Touch0),
	(
		Touch0 = yes,
		Touch = yes
	;
		Touch0 = no,
		opt_util__touches_nondet_ctrl(Instrs, Touch)
	).

:- pred opt_util__touches_nondet_ctrl_instr(instr, bool).
:- mode opt_util__touches_nondet_ctrl_instr(in, out) is det.

opt_util__touches_nondet_ctrl_instr(Uinstr, Touch) :-
	( Uinstr = assign(Lval, Rval) ->
		opt_util__touches_nondet_ctrl_lval(Lval, TouchLval),
		opt_util__touches_nondet_ctrl_rval(Rval, TouchRval),
		bool__or(TouchLval, TouchRval, Touch)
	; Uinstr = incr_hp(Lval, _, Rval, _) ->
		opt_util__touches_nondet_ctrl_lval(Lval, TouchLval),
		opt_util__touches_nondet_ctrl_rval(Rval, TouchRval),
		bool__or(TouchLval, TouchRval, Touch)
	; Uinstr = mark_hp(Lval) ->
		opt_util__touches_nondet_ctrl_lval(Lval, Touch)
	; Uinstr = restore_hp(Rval) ->
		opt_util__touches_nondet_ctrl_rval(Rval, Touch)
	;
		Touch = yes
	).

:- pred opt_util__touches_nondet_ctrl_lval(lval, bool).
:- mode opt_util__touches_nondet_ctrl_lval(in, out) is det.

opt_util__touches_nondet_ctrl_lval(reg(_, _), no).
opt_util__touches_nondet_ctrl_lval(stackvar(_), no).
opt_util__touches_nondet_ctrl_lval(framevar(_), no).
opt_util__touches_nondet_ctrl_lval(succip, no).
opt_util__touches_nondet_ctrl_lval(maxfr, yes).
opt_util__touches_nondet_ctrl_lval(curfr, yes).
opt_util__touches_nondet_ctrl_lval(succfr(_), yes).
opt_util__touches_nondet_ctrl_lval(prevfr(_), yes).
opt_util__touches_nondet_ctrl_lval(redoip(_), yes).
opt_util__touches_nondet_ctrl_lval(succip(_), yes).
opt_util__touches_nondet_ctrl_lval(hp, no).
opt_util__touches_nondet_ctrl_lval(sp, no).
opt_util__touches_nondet_ctrl_lval(field(_, Rval1, Rval2), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval1, Touch1),
	opt_util__touches_nondet_ctrl_rval(Rval2, Touch2),
	bool__or(Touch1, Touch2, Touch).
opt_util__touches_nondet_ctrl_lval(lvar(_), no).
opt_util__touches_nondet_ctrl_lval(temp(_, _), no).
opt_util__touches_nondet_ctrl_lval(mem_ref(Rval), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval, Touch).

:- pred opt_util__touches_nondet_ctrl_rval(rval, bool).
:- mode opt_util__touches_nondet_ctrl_rval(in, out) is det.

opt_util__touches_nondet_ctrl_rval(lval(Lval), Touch) :-
	opt_util__touches_nondet_ctrl_lval(Lval, Touch).
opt_util__touches_nondet_ctrl_rval(var(_), no).
opt_util__touches_nondet_ctrl_rval(create(_, _, _, _, _), no).
opt_util__touches_nondet_ctrl_rval(mkword(_, Rval), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval, Touch).
opt_util__touches_nondet_ctrl_rval(const(_), no).
opt_util__touches_nondet_ctrl_rval(unop(_, Rval), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval, Touch).
opt_util__touches_nondet_ctrl_rval(binop(_, Rval1, Rval2), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval1, Touch1),
	opt_util__touches_nondet_ctrl_rval(Rval2, Touch2),
	bool__or(Touch1, Touch2, Touch).
opt_util__touches_nondet_ctrl_rval(mem_addr(MemRef), Touch) :-
	opt_util__touches_nondet_ctrl_mem_ref(MemRef, Touch).

:- pred opt_util__touches_nondet_ctrl_mem_ref(mem_ref, bool).
:- mode opt_util__touches_nondet_ctrl_mem_ref(in, out) is det.

opt_util__touches_nondet_ctrl_mem_ref(stackvar_ref(_), no).
opt_util__touches_nondet_ctrl_mem_ref(framevar_ref(_), no).
opt_util__touches_nondet_ctrl_mem_ref(heap_ref(Rval, _, _), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval, Touch).

%-----------------------------------------------------------------------------%

opt_util__lval_access_rvals(reg(_, _), []).
opt_util__lval_access_rvals(stackvar(_), []).
opt_util__lval_access_rvals(framevar(_), []).
opt_util__lval_access_rvals(succip, []).
opt_util__lval_access_rvals(maxfr, []).
opt_util__lval_access_rvals(curfr, []).
opt_util__lval_access_rvals(redoip(Rval), [Rval]).
opt_util__lval_access_rvals(succip(Rval), [Rval]).
opt_util__lval_access_rvals(prevfr(Rval), [Rval]).
opt_util__lval_access_rvals(succfr(Rval), [Rval]).
opt_util__lval_access_rvals(hp, []).
opt_util__lval_access_rvals(sp, []).
opt_util__lval_access_rvals(field(_, Rval1, Rval2), [Rval1, Rval2]).
opt_util__lval_access_rvals(temp(_, _), []).
opt_util__lval_access_rvals(lvar(_), _) :-
	error("lvar detected in opt_util__lval_access_rvals").
opt_util__lval_access_rvals(mem_ref(Rval), [Rval]).

%-----------------------------------------------------------------------------%

opt_util__rvals_free_of_lval([], _).
opt_util__rvals_free_of_lval([Rval | Rvals], Forbidden) :-
	opt_util__rval_free_of_lval(Rval, Forbidden),
	opt_util__rvals_free_of_lval(Rvals, Forbidden).

opt_util__rval_free_of_lval(lval(Lval), Forbidden) :-
	Lval \= Forbidden,
	opt_util__lval_access_rvals(Lval, Rvals),
	opt_util__rvals_free_of_lval(Rvals, Forbidden).
opt_util__rval_free_of_lval(var(_), _) :-
	error("found var in opt_util__rval_free_of_lval").
opt_util__rval_free_of_lval(create(_, _, _, _, _), _).
opt_util__rval_free_of_lval(mkword(_, Rval), Forbidden) :-
	opt_util__rval_free_of_lval(Rval, Forbidden).
opt_util__rval_free_of_lval(const(_), _).
opt_util__rval_free_of_lval(unop(_, Rval), Forbidden) :-
	opt_util__rval_free_of_lval(Rval, Forbidden).
opt_util__rval_free_of_lval(binop(_, Rval1, Rval2), Forbidden) :-
	opt_util__rval_free_of_lval(Rval1, Forbidden),
	opt_util__rval_free_of_lval(Rval2, Forbidden).

%-----------------------------------------------------------------------------%

opt_util__lvals_in_lval(reg(_, _), []).
opt_util__lvals_in_lval(stackvar(_), []).
opt_util__lvals_in_lval(framevar(_), []).
opt_util__lvals_in_lval(succip, []).
opt_util__lvals_in_lval(maxfr, []).
opt_util__lvals_in_lval(curfr, []).
opt_util__lvals_in_lval(succip(Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).
opt_util__lvals_in_lval(redoip(Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).
opt_util__lvals_in_lval(succfr(Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).
opt_util__lvals_in_lval(prevfr(Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).
opt_util__lvals_in_lval(hp, []).
opt_util__lvals_in_lval(sp, []).
opt_util__lvals_in_lval(field(_, Rval1, Rval2), Lvals) :-
	opt_util__lvals_in_rval(Rval1, Lvals1),
	opt_util__lvals_in_rval(Rval2, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).
opt_util__lvals_in_lval(lvar(_), []).
opt_util__lvals_in_lval(temp(_, _), []).
opt_util__lvals_in_lval(mem_ref(Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).

opt_util__lvals_in_rval(lval(Lval), [Lval | Lvals]) :-
	opt_util__lvals_in_lval(Lval, Lvals).
opt_util__lvals_in_rval(var(_), _) :-
	error("found var in opt_util__lvals_in_rval").
opt_util__lvals_in_rval(create(_, _, _, _, _), []).
opt_util__lvals_in_rval(mkword(_, Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).
opt_util__lvals_in_rval(const(_), []).
opt_util__lvals_in_rval(unop(_, Rval), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).
opt_util__lvals_in_rval(binop(_, Rval1, Rval2), Lvals) :-
	opt_util__lvals_in_rval(Rval1, Lvals1),
	opt_util__lvals_in_rval(Rval2, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).
opt_util__lvals_in_rval(mem_addr(MemRef), Lvals) :-
	opt_util__lvals_in_mem_ref(MemRef, Lvals).

	% XXX
:- pred opt_util__lvals_in_mem_ref(mem_ref, list(lval)).
:- mode opt_util__lvals_in_mem_ref(in, out) is det.

opt_util__lvals_in_mem_ref(stackvar_ref(_), []).
opt_util__lvals_in_mem_ref(framevar_ref(_), []).
opt_util__lvals_in_mem_ref(heap_ref(Rval, _, _), Lvals) :-
	opt_util__lvals_in_rval(Rval, Lvals).

%-----------------------------------------------------------------------------%

opt_util__count_incr_hp(Instrs, N) :-
	opt_util__count_incr_hp_2(Instrs, 0, N).

:- pred opt_util__count_incr_hp_2(list(instruction), int, int).
:- mode opt_util__count_incr_hp_2(in, in, out) is det.

opt_util__count_incr_hp_2([], N, N).
opt_util__count_incr_hp_2([Uinstr0 - _ | Instrs], N0, N) :-
	( Uinstr0 = incr_hp(_, _, _, _) ->
		N1 is N0 + 1
	;
		N1 = N0
	),
	opt_util__count_incr_hp_2(Instrs, N1, N).

%-----------------------------------------------------------------------------%

opt_util__propagate_livevals(Instrs0, Instrs) :-
	list__reverse(Instrs0, RevInstrs0),
	set__init(Livevals),
	opt_util__propagate_livevals_2(RevInstrs0, Livevals, RevInstrs),
	list__reverse(RevInstrs, Instrs).

:- pred opt_util__propagate_livevals_2(list(instruction), set(lval),
	list(instruction)).
:- mode opt_util__propagate_livevals_2(in, in, out) is det.

opt_util__propagate_livevals_2([], _, []).
opt_util__propagate_livevals_2([Instr0 | Instrs0], Livevals0,
		[Instr | Instrs]) :-
	Instr0 = Uinstr0 - Comment,
	( Uinstr0 = livevals(ThisLivevals) ->
		set__union(Livevals0, ThisLivevals, Livevals),
		Instr = livevals(Livevals) - Comment
	;
		Instr = Instr0,
		( Uinstr0 = assign(Lval, _) ->
			set__delete(Livevals0, Lval, Livevals)
		; opt_util__can_instr_fall_through(Uinstr0, no) ->
			set__init(Livevals)
		;
			Livevals = Livevals0
		)
	),
	opt_util__propagate_livevals_2(Instrs0, Livevals, Instrs).

%-----------------------------------------------------------------------------%
