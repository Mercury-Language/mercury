%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Utilities for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__opt_util.

:- interface.
:- import_module bool, map, list, std_util.
:- import_module ll_backend__llds.

:- type instrmap == map(label, instruction).
:- type lvalmap == map(label, maybe(instruction)).
:- type tailmap == map(label, list(instruction)).
:- type succmap == map(label, bool).

:- pred opt_util__get_prologue(list(instruction), instruction,
	list(instruction), list(instruction)).
:- mode opt_util__get_prologue(in, out, out, out) is det.

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

	% Find the next assignment to the redoip of the frame whose address
	% is given by the base addresses in the second argument, provided
	% it is guaranteed to be reached from here.

:- pred opt_util__next_assign_to_redoip(list(instruction), list(lval),
	list(instruction), code_addr, list(instruction), list(instruction)).
:- mode opt_util__next_assign_to_redoip(in, in, in, out, out, out) is semidet.

	% See if these instructions touch nondet stack controls, i.e.
	% the virtual machine registers that point to the nondet stack
	% (curfr and maxfr) and the fixed slots in nondet stack frames.

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
	% modulo livevals annotations and the value assigned to r1? Is MR_TRUE
	% assigned to r1 in the success continuation and MR_FALSE in the failure
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

	% Given an instruction, find the set of labels to which it can cause
	% control to transfer. In the case of calls, this includes transfer
	% via return from the called procedure.

:- pred opt_util__possible_targets(instr, list(label)).
:- mode opt_util__possible_targets(in, out) is det.

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

	% Replace references to one set of local labels with references
	% to another set, in one instruction or a list of instructions.
	% Control references (those that can cause a transfer of control
	% from the instruction they occur in to the replaced label, either
	% directly or via return from a called procedure) are always replaced;
	% references that treat the label as data are replaced iff the third
	% argument is set to "yes".

:- pred opt_util__replace_labels_instr(instr::in, map(label, label)::in,
	bool::in, instr::out) is det.

:- pred opt_util__replace_labels_instruction(instruction::in,
	map(label, label)::in, bool::in, instruction::out) is det.

:- pred opt_util__replace_labels_instruction_list(list(instruction)::in,
	map(label, label)::in, bool::in, list(instruction)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops, ll_backend__exprn_aux.
:- import_module ll_backend__llds_out, hlds__hlds_pred.
:- import_module int, string, set, require.

opt_util__get_prologue(Instrs0, LabelInstr, Comments, Instrs) :-
	opt_util__gather_comments(Instrs0, Comments1, Instrs1),
	(
		Instrs1 = [Instr1 | Instrs2],
		Instr1 = label(_) - _
	->
		LabelInstr = Instr1,
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

opt_util__next_assign_to_redoip([Instr | Instrs], AllowedBases,
		RevSkip, Redoip, Skip, Rest) :-
	Instr = Uinstr - _Comment,
	(
		Uinstr = assign(redoip(lval(Fr)),
			const(code_addr_const(Redoip0))),
		list__member(Fr, AllowedBases)
	->
		Redoip = Redoip0,
		list__reverse(RevSkip, Skip),
		Rest = Instrs
	;
		Uinstr = mkframe(_, _)
	->
		fail
	;
		opt_util__can_instr_branch_away(Uinstr, Canbranchaway),
		( Canbranchaway = no ->
			opt_util__next_assign_to_redoip(Instrs, AllowedBases,
				[Instr | RevSkip], Redoip, Skip, Rest)
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
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(prevfr(Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(redofr(Rval), Refers) :-
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
opt_util__rval_refers_stackvars(create(_, Rvals, _, _, _, _, _), Refers) :-
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
		Uinstr0 = call(_, _, _, _, _, _),
		Need = no
	;
		Uinstr0 = mkframe(_, _),
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
		Uinstr0 = c_code(_, _),
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
		Uinstr0 = free_heap(Rval),
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
		Uinstr0 = prune_ticket,
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
		Uinstr0 = prune_tickets_to(Rval),
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
		Uinstr0 = pragma_c(_, _, _, _, _, _, _, _),
		Need = no
	;
		Uinstr0 = init_sync_term(Lval, _),
		opt_util__lval_refers_stackvars(Lval, Need)
	;
		Uinstr0 = fork(_, _, _),
		Need = no
	;
		Uinstr0 = join_and_terminate(Lval),
		opt_util__lval_refers_stackvars(Lval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
	;
		Uinstr0 = join_and_continue(Lval, _),
		opt_util__lval_refers_stackvars(Lval, Use),
		( Use = yes ->
			Need = yes
		;
			opt_util__block_refers_stackvars(Instrs0, Need)
		)
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

opt_util__can_instr_branch_away(comment(_), no).
opt_util__can_instr_branch_away(livevals(_), no).
opt_util__can_instr_branch_away(block(_, _, _), yes).
opt_util__can_instr_branch_away(assign(_, _), no).
opt_util__can_instr_branch_away(call(_, _, _, _, _, _), yes).
opt_util__can_instr_branch_away(mkframe(_, _), no).
opt_util__can_instr_branch_away(label(_), no).
opt_util__can_instr_branch_away(goto(_), yes).
opt_util__can_instr_branch_away(computed_goto(_, _), yes).
opt_util__can_instr_branch_away(c_code(_, _), no).
opt_util__can_instr_branch_away(if_val(_, _), yes).
opt_util__can_instr_branch_away(incr_hp(_, _, _, _), no).
opt_util__can_instr_branch_away(mark_hp(_), no).
opt_util__can_instr_branch_away(restore_hp(_), no).
opt_util__can_instr_branch_away(free_heap(_), no).
opt_util__can_instr_branch_away(store_ticket(_), no).
opt_util__can_instr_branch_away(reset_ticket(_, _), no).
opt_util__can_instr_branch_away(discard_ticket, no).
opt_util__can_instr_branch_away(prune_ticket, no).
opt_util__can_instr_branch_away(mark_ticket_stack(_), no).
opt_util__can_instr_branch_away(prune_tickets_to(_), no).
opt_util__can_instr_branch_away(incr_sp(_, _), no).
opt_util__can_instr_branch_away(decr_sp(_), no).
opt_util__can_instr_branch_away(init_sync_term(_, _), no).
opt_util__can_instr_branch_away(fork(_, _, _), yes).
opt_util__can_instr_branch_away(join_and_terminate(_), no).
opt_util__can_instr_branch_away(join_and_continue(_, _), yes).
opt_util__can_instr_branch_away(pragma_c(_, Comps, _, _, _, _, _, _),
		BranchAway) :-
	opt_util__can_components_branch_away(Comps, BranchAway).

:- pred opt_util__can_components_branch_away(list(pragma_c_component), bool).
:- mode opt_util__can_components_branch_away(in, out) is det.

opt_util__can_components_branch_away([], no).
opt_util__can_components_branch_away([Component | Components], BranchAway) :-
	opt_util__can_component_branch_away(Component, BranchAway1),
	( BranchAway1 = yes ->
		BranchAway = yes
	;
		opt_util__can_components_branch_away(Components, BranchAway)
	).

:- pred opt_util__can_component_branch_away(pragma_c_component, bool).
:- mode opt_util__can_component_branch_away(in, out) is det.

	% The input and output components get expanded to straight line code.
	% Some of the raw_code components we generate for nondet pragma C codes
	% invoke succeed(), which definitely does branch away.
	% Also the raw_code components for semidet pragma C codes can
	% branch to a label on failure.
	% User-written C code cannot branch away because users do not know
	% how to do that. (They can call other functions, but those functions
	% will return, so control will still go to the instruction following
	% this one. We the developers could write C code that branched away,
	% but we are careful to preserve a declarative interface, and that
	% is incompatible with branching away.)

opt_util__can_component_branch_away(pragma_c_inputs(_), no).
opt_util__can_component_branch_away(pragma_c_outputs(_), no).
opt_util__can_component_branch_away(pragma_c_raw_code(Code, _),
		CanBranchAway) :-
	( Code = "" -> CanBranchAway = no ; CanBranchAway = yes ).
opt_util__can_component_branch_away(pragma_c_user_code(_, _), no).
opt_util__can_component_branch_away(pragma_c_fail_to(_), yes).
opt_util__can_component_branch_away(pragma_c_noop, no).

opt_util__can_instr_fall_through(comment(_), yes).
opt_util__can_instr_fall_through(livevals(_), yes).
opt_util__can_instr_fall_through(block(_, _, Instrs), FallThrough) :-
	opt_util__can_block_fall_through(Instrs, FallThrough).
opt_util__can_instr_fall_through(assign(_, _), yes).
opt_util__can_instr_fall_through(call(_, _, _, _, _, _), no).
opt_util__can_instr_fall_through(mkframe(_, _), yes).
opt_util__can_instr_fall_through(label(_), yes).
opt_util__can_instr_fall_through(goto(_), no).
opt_util__can_instr_fall_through(computed_goto(_, _), no).
opt_util__can_instr_fall_through(c_code(_, _), yes).
opt_util__can_instr_fall_through(if_val(_, _), yes).
opt_util__can_instr_fall_through(incr_hp(_, _, _, _), yes).
opt_util__can_instr_fall_through(mark_hp(_), yes).
opt_util__can_instr_fall_through(restore_hp(_), yes).
opt_util__can_instr_fall_through(free_heap(_), yes).
opt_util__can_instr_fall_through(store_ticket(_), yes).
opt_util__can_instr_fall_through(reset_ticket(_, _), yes).
opt_util__can_instr_fall_through(discard_ticket, yes).
opt_util__can_instr_fall_through(prune_ticket, yes).
opt_util__can_instr_fall_through(mark_ticket_stack(_), yes).
opt_util__can_instr_fall_through(prune_tickets_to(_), yes).
opt_util__can_instr_fall_through(incr_sp(_, _), yes).
opt_util__can_instr_fall_through(decr_sp(_), yes).
opt_util__can_instr_fall_through(init_sync_term(_, _), yes).
opt_util__can_instr_fall_through(fork(_, _, _), no).
opt_util__can_instr_fall_through(join_and_terminate(_), no).
opt_util__can_instr_fall_through(join_and_continue(_, _), no).
opt_util__can_instr_fall_through(pragma_c(_, _, _, _, _, _, _, _), yes).

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
opt_util__can_use_livevals(call(_, _, _, _, _, _), yes).
opt_util__can_use_livevals(mkframe(_, _), no).
opt_util__can_use_livevals(label(_), no).
opt_util__can_use_livevals(goto(_), yes).
opt_util__can_use_livevals(computed_goto(_, _), no).
opt_util__can_use_livevals(c_code(_, _), no).
opt_util__can_use_livevals(if_val(_, _), yes).
opt_util__can_use_livevals(incr_hp(_, _, _, _), no).
opt_util__can_use_livevals(mark_hp(_), no).
opt_util__can_use_livevals(restore_hp(_), no).
opt_util__can_use_livevals(free_heap(_), no).
opt_util__can_use_livevals(store_ticket(_), no).
opt_util__can_use_livevals(reset_ticket(_, _), no).
opt_util__can_use_livevals(discard_ticket, no).
opt_util__can_use_livevals(prune_ticket, no).
opt_util__can_use_livevals(mark_ticket_stack(_), no).
opt_util__can_use_livevals(prune_tickets_to(_), no).
opt_util__can_use_livevals(incr_sp(_, _), no).
opt_util__can_use_livevals(decr_sp(_), no).
opt_util__can_use_livevals(init_sync_term(_, _), no).
opt_util__can_use_livevals(fork(_, _, _), no).
opt_util__can_use_livevals(join_and_terminate(_), no).
opt_util__can_use_livevals(join_and_continue(_, _), no).
opt_util__can_use_livevals(pragma_c(_, _, _, _, _, _, _, _), no).

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
opt_util__instr_labels_2(call(Target, Ret, _, _, _, _), [], [Target, Ret]).
opt_util__instr_labels_2(mkframe(_, Addr), [], [Addr]).
opt_util__instr_labels_2(label(_), [], []).
opt_util__instr_labels_2(goto(Addr), [], [Addr]).
opt_util__instr_labels_2(computed_goto(_, Labels), Labels, []).
opt_util__instr_labels_2(c_code(_, _), [], []).
opt_util__instr_labels_2(if_val(_, Addr), [], [Addr]).
opt_util__instr_labels_2(incr_hp(_, _, _, _), [], []).
opt_util__instr_labels_2(mark_hp(_), [], []).
opt_util__instr_labels_2(restore_hp(_), [], []).
opt_util__instr_labels_2(free_heap(_), [], []).
opt_util__instr_labels_2(store_ticket(_), [], []).
opt_util__instr_labels_2(reset_ticket(_, _), [], []).
opt_util__instr_labels_2(discard_ticket, [], []).
opt_util__instr_labels_2(prune_ticket, [], []).
opt_util__instr_labels_2(mark_ticket_stack(_), [], []).
opt_util__instr_labels_2(prune_tickets_to(_), [], []).
opt_util__instr_labels_2(incr_sp(_, _), [], []).
opt_util__instr_labels_2(decr_sp(_), [], []).
opt_util__instr_labels_2(init_sync_term(_, _), [], []).
opt_util__instr_labels_2(fork(Child, Parent, _), [Child, Parent], []).
opt_util__instr_labels_2(join_and_terminate(_), [], []).
opt_util__instr_labels_2(join_and_continue(_, Label), [Label], []).
opt_util__instr_labels_2(pragma_c(_, _, _, MaybeFixLabel, MaybeLayoutLabel,
		MaybeOnlyLayoutLabel, MaybeSubLabel, _), Labels, []) :-
	opt_util__pragma_c_labels(MaybeFixLabel, MaybeLayoutLabel,
		MaybeOnlyLayoutLabel, MaybeSubLabel, Labels).

opt_util__possible_targets(comment(_), []).
opt_util__possible_targets(livevals(_), []).
opt_util__possible_targets(block(_, _, _), _) :-
	error("block in possible_targets").
opt_util__possible_targets(assign(_, _), []).
opt_util__possible_targets(call(_, ReturnAddr, _, _, _, _), Labels) :-
	( ReturnAddr = label(Label) ->
		Labels = [Label]
	;
		Labels = []
	).
opt_util__possible_targets(mkframe(_, _), []).
opt_util__possible_targets(label(_), []).
opt_util__possible_targets(goto(CodeAddr), Targets) :-
	( CodeAddr = label(Label) ->
		Targets = [Label]
	;
		Targets = []
	).
opt_util__possible_targets(computed_goto(_, Targets), Targets).
opt_util__possible_targets(c_code(_, _), []).
opt_util__possible_targets(if_val(_, CodeAddr), Targets) :-
	( CodeAddr = label(Label) ->
		Targets = [Label]
	;
		Targets = []
	).
opt_util__possible_targets(incr_hp(_, _, _, _), []).
opt_util__possible_targets(mark_hp(_), []).
opt_util__possible_targets(restore_hp(_), []).
opt_util__possible_targets(free_heap(_), []).
opt_util__possible_targets(store_ticket(_), []).
opt_util__possible_targets(reset_ticket(_, _), []).
opt_util__possible_targets(discard_ticket, []).
opt_util__possible_targets(prune_ticket, []).
opt_util__possible_targets(mark_ticket_stack(_), []).
opt_util__possible_targets(prune_tickets_to(_), []).
opt_util__possible_targets(incr_sp(_, _), []).
opt_util__possible_targets(decr_sp(_), []).
opt_util__possible_targets(init_sync_term(_, _), []).
opt_util__possible_targets(fork(Child, Parent, _), [Child, Parent]).
opt_util__possible_targets(join_and_terminate(_), []).
opt_util__possible_targets(join_and_continue(_, L), [L]).
opt_util__possible_targets(pragma_c(_, _, _, MaybeFixedLabel, MaybeLayoutLabel,
		_, MaybeSubLabel, _), Labels) :-
	opt_util__pragma_c_labels(MaybeFixedLabel, MaybeLayoutLabel,
		no, MaybeSubLabel, Labels).

:- pred opt_util__pragma_c_labels(maybe(label), maybe(label), maybe(label),
	maybe(label), list(label)).
:- mode opt_util__pragma_c_labels(in, in, in, in, out) is det.

opt_util__pragma_c_labels(MaybeFixedLabel, MaybeLayoutLabel,
		MaybeOnlyLayoutLabel, MaybeSubLabel, Labels) :-
	( MaybeFixedLabel = yes(FixedLabel) ->
		Labels0 = [FixedLabel]
	;
		Labels0 = []
	),
	( MaybeLayoutLabel = yes(LayoutLabel) ->
		Labels1 = [LayoutLabel | Labels0]
	;
		Labels1 = Labels0
	),
	( MaybeOnlyLayoutLabel = yes(OnlyLayoutLabel) ->
		Labels2 = [OnlyLayoutLabel | Labels1]
	;
		Labels2 = Labels1
	),
	( MaybeSubLabel = yes(SubLabel) ->
		Labels = [SubLabel | Labels2]
	;
		Labels = Labels2
	).

:- pred opt_util__instr_rvals_and_lvals(instr, list(rval), list(lval)).
:- mode opt_util__instr_rvals_and_lvals(in, out, out) is det.

% determine all the rvals and lvals referenced by an instruction

opt_util__instr_rvals_and_lvals(comment(_), [], []).
opt_util__instr_rvals_and_lvals(livevals(_), [], []).
opt_util__instr_rvals_and_lvals(block(_, _, Instrs), Labels, CodeAddrs) :-
	opt_util__instr_list_rvals_and_lvals(Instrs, Labels, CodeAddrs).
opt_util__instr_rvals_and_lvals(assign(Lval,Rval), [Rval], [Lval]).
opt_util__instr_rvals_and_lvals(call(_, _, _, _, _, _), [], []).
opt_util__instr_rvals_and_lvals(mkframe(_, _), [], []).
opt_util__instr_rvals_and_lvals(label(_), [], []).
opt_util__instr_rvals_and_lvals(goto(_), [], []).
opt_util__instr_rvals_and_lvals(computed_goto(Rval, _), [Rval], []).
opt_util__instr_rvals_and_lvals(c_code(_, _), [], []).
opt_util__instr_rvals_and_lvals(if_val(Rval, _), [Rval], []).
opt_util__instr_rvals_and_lvals(incr_hp(Lval, _, Rval, _), [Rval], [Lval]).
opt_util__instr_rvals_and_lvals(mark_hp(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(restore_hp(Rval), [Rval], []).
opt_util__instr_rvals_and_lvals(free_heap(Rval), [Rval], []).
opt_util__instr_rvals_and_lvals(store_ticket(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(reset_ticket(Rval, _Reason), [Rval], []).
opt_util__instr_rvals_and_lvals(discard_ticket, [], []).
opt_util__instr_rvals_and_lvals(prune_ticket, [], []).
opt_util__instr_rvals_and_lvals(mark_ticket_stack(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(prune_tickets_to(Rval), [Rval], []).
opt_util__instr_rvals_and_lvals(incr_sp(_, _), [], []).
opt_util__instr_rvals_and_lvals(decr_sp(_), [], []).
opt_util__instr_rvals_and_lvals(init_sync_term(Lval, _), [], [Lval]).
opt_util__instr_rvals_and_lvals(fork(_, _, _), [], []).
opt_util__instr_rvals_and_lvals(join_and_terminate(Lval), [], [Lval]).
opt_util__instr_rvals_and_lvals(join_and_continue(Lval, _), [], [Lval]).
opt_util__instr_rvals_and_lvals(pragma_c(_, Cs, _, _, _, _, _, _),
		Rvals, Lvals) :-
	pragma_c_components_get_rvals_and_lvals(Cs, Rvals, Lvals).

	% extract the rvals and lvals from the pragma_c_components
:- pred pragma_c_components_get_rvals_and_lvals(list(pragma_c_component),
	list(rval), list(lval)).
:- mode pragma_c_components_get_rvals_and_lvals(in, out, out) is det.

pragma_c_components_get_rvals_and_lvals([], [], []).
pragma_c_components_get_rvals_and_lvals([Comp | Comps], Rvals, Lvals) :-
	pragma_c_components_get_rvals_and_lvals(Comps, Rvals1, Lvals1),
	pragma_c_component_get_rvals_and_lvals(Comp,
		Rvals1, Rvals, Lvals1, Lvals).

	% extract the rvals and lvals from the pragma_c_component
	% and add them to the list.
:- pred pragma_c_component_get_rvals_and_lvals(pragma_c_component,
	list(rval), list(rval), list(lval), list(lval)).
:- mode pragma_c_component_get_rvals_and_lvals(in, in, out, in, out) is det.

pragma_c_component_get_rvals_and_lvals(pragma_c_inputs(Inputs),
		Rvals0, Rvals, Lvals, Lvals) :-
	pragma_c_inputs_get_rvals(Inputs, Rvals1),
	list__append(Rvals1, Rvals0, Rvals).
pragma_c_component_get_rvals_and_lvals(pragma_c_outputs(Outputs),
		Rvals, Rvals, Lvals0, Lvals) :-
	pragma_c_outputs_get_lvals(Outputs, Lvals1),
	list__append(Lvals1, Lvals0, Lvals).
pragma_c_component_get_rvals_and_lvals(pragma_c_user_code(_, _),
		Rvals, Rvals, Lvals, Lvals).
pragma_c_component_get_rvals_and_lvals(pragma_c_raw_code(_, _),
		Rvals, Rvals, Lvals, Lvals).
pragma_c_component_get_rvals_and_lvals(pragma_c_fail_to(_),
		Rvals, Rvals, Lvals, Lvals).
pragma_c_component_get_rvals_and_lvals(pragma_c_noop,
		Rvals, Rvals, Lvals, Lvals).

	% extract the rvals from the pragma_c_input
:- pred pragma_c_inputs_get_rvals(list(pragma_c_input), list(rval)).
:- mode pragma_c_inputs_get_rvals(in, out) is det.

pragma_c_inputs_get_rvals([], []).
pragma_c_inputs_get_rvals([I|Inputs], [R|Rvals]) :-
	I = pragma_c_input(_Name, _Type, R, _),
	pragma_c_inputs_get_rvals(Inputs, Rvals).

	% extract the lvals from the pragma_c_output
:- pred pragma_c_outputs_get_lvals(list(pragma_c_output), list(lval)).
:- mode pragma_c_outputs_get_lvals(in, out) is det.

pragma_c_outputs_get_lvals([], []).
pragma_c_outputs_get_lvals([O|Outputs], [L|Lvals]) :-
	O = pragma_c_output(L, _Type, _Name, _),
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
opt_util__livevals_addr(do_trace_redo_fail_shallow, no).
opt_util__livevals_addr(do_trace_redo_fail_deep, no).
opt_util__livevals_addr(do_call_closure, yes).
opt_util__livevals_addr(do_call_class_method, yes).
opt_util__livevals_addr(do_det_aditi_call, yes).
opt_util__livevals_addr(do_semidet_aditi_call, yes).
opt_util__livevals_addr(do_nondet_aditi_call, yes).
opt_util__livevals_addr(do_aditi_insert, yes).
opt_util__livevals_addr(do_aditi_delete, yes).
opt_util__livevals_addr(do_aditi_bulk_insert, yes).
opt_util__livevals_addr(do_aditi_bulk_delete, yes).
opt_util__livevals_addr(do_aditi_bulk_modify, yes).
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
opt_util__count_temps_instr(call(_, _, _, _, _, _), R, R, F, F).
opt_util__count_temps_instr(mkframe(_, _), R, R, F, F).
opt_util__count_temps_instr(label(_), R, R, F, F).
opt_util__count_temps_instr(goto(_), R, R, F, F).
opt_util__count_temps_instr(computed_goto(Rval, _), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(if_val(Rval, _), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(c_code(_, _), R, R, F, F).
opt_util__count_temps_instr(incr_hp(Lval, _, Rval, _), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R1, F0, F1),
	opt_util__count_temps_rval(Rval, R1, R, F1, F).
opt_util__count_temps_instr(mark_hp(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(restore_hp(Rval), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(free_heap(Rval), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(store_ticket(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(reset_ticket(Rval, _Reason), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(discard_ticket, R, R, F, F).
opt_util__count_temps_instr(prune_ticket, R, R, F, F).
opt_util__count_temps_instr(mark_ticket_stack(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(prune_tickets_to(Rval), R0, R, F0, F) :-
	opt_util__count_temps_rval(Rval, R0, R, F0, F).
opt_util__count_temps_instr(incr_sp(_, _), R, R, F, F).
opt_util__count_temps_instr(decr_sp(_), R, R, F, F).
opt_util__count_temps_instr(init_sync_term(Lval, _), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(fork(_, _, _), R, R, F, F).
opt_util__count_temps_instr(join_and_terminate(Lval), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(join_and_continue(Lval, _), R0, R, F0, F) :-
	opt_util__count_temps_lval(Lval, R0, R, F0, F).
opt_util__count_temps_instr(pragma_c(_, _, _, _, _, _, _, _), R, R, F, F).

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

opt_util__format_label(local(_, ProcLabel), Str) :-
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
	; Uinstr = pragma_c(_, Components, _, _, _, _, _, _) ->
		opt_util__touches_nondet_ctrl_components(Components, Touch)
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
opt_util__touches_nondet_ctrl_lval(redofr(_), yes).
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
opt_util__touches_nondet_ctrl_rval(create(_, _, _, _, _, _, _), no).
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

:- pred opt_util__touches_nondet_ctrl_components(list(pragma_c_component),
	bool).
:- mode opt_util__touches_nondet_ctrl_components(in, out) is det.

opt_util__touches_nondet_ctrl_components([], no).
opt_util__touches_nondet_ctrl_components([C | Cs], Touch) :-
	opt_util__touches_nondet_ctrl_component(C, Touch1),
	opt_util__touches_nondet_ctrl_components(Cs, Touch2),
	bool__or(Touch1, Touch2, Touch).

:- pred opt_util__touches_nondet_ctrl_component(pragma_c_component, bool).
:- mode opt_util__touches_nondet_ctrl_component(in, out) is det.

	% The inputs and outputs components get emitted as simple
	% straight-line code that do not refer to control slots.
	% The compiler does not generate raw_code that refers to control slots.
	% User code shouldn't either, but until we have prohibited the
	% use of ordinary pragma C codes for model_non procedures,
	% some user code will need to ignore this restriction.

opt_util__touches_nondet_ctrl_component(pragma_c_inputs(_), no).
opt_util__touches_nondet_ctrl_component(pragma_c_outputs(_), no).
opt_util__touches_nondet_ctrl_component(pragma_c_raw_code(_, _), no).
opt_util__touches_nondet_ctrl_component(pragma_c_user_code(_, _), yes).
opt_util__touches_nondet_ctrl_component(pragma_c_fail_to(_), no).
opt_util__touches_nondet_ctrl_component(pragma_c_noop, no).

%-----------------------------------------------------------------------------%

opt_util__lval_access_rvals(reg(_, _), []).
opt_util__lval_access_rvals(stackvar(_), []).
opt_util__lval_access_rvals(framevar(_), []).
opt_util__lval_access_rvals(succip, []).
opt_util__lval_access_rvals(maxfr, []).
opt_util__lval_access_rvals(curfr, []).
opt_util__lval_access_rvals(redoip(Rval), [Rval]).
opt_util__lval_access_rvals(succip(Rval), [Rval]).
opt_util__lval_access_rvals(redofr(Rval), [Rval]).
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
opt_util__rval_free_of_lval(create(_, _, _, _, _, _, _), _).
opt_util__rval_free_of_lval(mkword(_, Rval), Forbidden) :-
	opt_util__rval_free_of_lval(Rval, Forbidden).
opt_util__rval_free_of_lval(const(_), _).
opt_util__rval_free_of_lval(unop(_, Rval), Forbidden) :-
	opt_util__rval_free_of_lval(Rval, Forbidden).
opt_util__rval_free_of_lval(binop(_, Rval1, Rval2), Forbidden) :-
	opt_util__rval_free_of_lval(Rval1, Forbidden),
	opt_util__rval_free_of_lval(Rval2, Forbidden).

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

	% The code in this section is concerned with replacing all references
	% to one given label with a reference to another given label.

opt_util__replace_labels_instruction_list([], _, _, []).
opt_util__replace_labels_instruction_list([Instr0 | Instrs0], ReplMap,
		ReplData, [Instr | Instrs]) :-
	opt_util__replace_labels_instruction(Instr0, ReplMap, ReplData, Instr),
	opt_util__replace_labels_instruction_list(Instrs0, ReplMap, ReplData,
		Instrs).

opt_util__replace_labels_instruction(Instr0 - Comment, ReplMap, ReplData,
		Instr - Comment) :-
	opt_util__replace_labels_instr(Instr0, ReplMap, ReplData, Instr).

opt_util__replace_labels_instr(comment(Comment), _, _, comment(Comment)).
opt_util__replace_labels_instr(livevals(Livevals), _, _, livevals(Livevals)).
opt_util__replace_labels_instr(block(R, F, Instrs0), ReplMap, ReplData,
		block(R, F, Instrs)) :-
	opt_util__replace_labels_instruction_list(Instrs0, ReplMap, ReplData,
		Instrs).
opt_util__replace_labels_instr(assign(Lval0, Rval0), ReplMap, ReplData,
		assign(Lval, Rval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_lval(Lval0, ReplMap, Lval),
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Lval = Lval0,
		Rval = Rval0
	).
opt_util__replace_labels_instr(call(Target, Return0, LiveInfo, CXT, GP, CM),
		ReplMap, _, call(Target, Return, LiveInfo, CXT, GP, CM)) :-
	opt_util__replace_labels_code_addr(Return0, ReplMap, Return).
opt_util__replace_labels_instr(mkframe(NondetFrameInfo, Redoip0), ReplMap,
		ReplData, mkframe(NondetFrameInfo, Redoip)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_code_addr(Redoip0, ReplMap, Redoip)
	;
		ReplData = no,
		Redoip = Redoip0
	).
opt_util__replace_labels_instr(label(Label), ReplMap, _, label(Label)) :-
	( map__search(ReplMap, Label, _) ->
		% The reason why we are replacing references to this label
		% is that it is being eliminated, and in fact should have been
		% already eliminated by the time opt_util__replace_labels_instr
		% is called.
		error("eliminated label in opt_util__replace_labels_instr")
	;
		true
	).
opt_util__replace_labels_instr(goto(Target0), ReplMap, _, goto(Target)) :-
	opt_util__replace_labels_code_addr(Target0, ReplMap, Target).
opt_util__replace_labels_instr(computed_goto(Rval0, Labels0), ReplMap,
		ReplData, computed_goto(Rval, Labels)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Rval = Rval0
	),
	opt_util__replace_labels_label_list(Labels0, ReplMap, Labels).
opt_util__replace_labels_instr(c_code(Code, Lvals), _, _, c_code(Code, Lvals)).
opt_util__replace_labels_instr(if_val(Rval0, Target0), ReplMap, ReplData,
		if_val(Rval, Target)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Rval = Rval0
	),
	opt_util__replace_labels_code_addr(Target0, ReplMap, Target).
opt_util__replace_labels_instr(incr_hp(Lval0, MaybeTag, Rval0, Msg), ReplMap,
		ReplData, incr_hp(Lval, MaybeTag, Rval, Msg)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_lval(Lval0, ReplMap, Lval),
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Lval = Lval0,
		Rval = Rval0
	).
opt_util__replace_labels_instr(mark_hp(Lval0), ReplMap, ReplData,
		mark_hp(Lval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_lval(Lval0, ReplMap, Lval)
	;
		ReplData = no,
		Lval = Lval0
	).
opt_util__replace_labels_instr(restore_hp(Rval0), ReplMap, ReplData,
		restore_hp(Rval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Rval = Rval0
	).
opt_util__replace_labels_instr(free_heap(Rval0), ReplMap, ReplData,
		free_heap(Rval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Rval = Rval0
	).
opt_util__replace_labels_instr(store_ticket(Lval0), ReplMap, ReplData,
		store_ticket(Lval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_lval(Lval0, ReplMap, Lval)
	;
		ReplData = no,
		Lval = Lval0
	).
opt_util__replace_labels_instr(reset_ticket(Rval0, Reason), ReplMap, ReplData,
		reset_ticket(Rval, Reason)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Rval = Rval0
	).
opt_util__replace_labels_instr(discard_ticket, _, _, discard_ticket).
opt_util__replace_labels_instr(prune_ticket, _, _, prune_ticket).
opt_util__replace_labels_instr(mark_ticket_stack(Lval0), ReplMap, ReplData,
		mark_ticket_stack(Lval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_lval(Lval0, ReplMap, Lval)
	;
		ReplData = no,
		Lval = Lval0
	).
opt_util__replace_labels_instr(prune_tickets_to(Rval0), ReplMap, ReplData,
		prune_tickets_to(Rval)) :-
	(
		ReplData = yes,
		opt_util__replace_labels_rval(Rval0, ReplMap, Rval)
	;
		ReplData = no,
		Rval = Rval0
	).
opt_util__replace_labels_instr(incr_sp(Size, Msg), _, _, incr_sp(Size, Msg)).
opt_util__replace_labels_instr(decr_sp(Size), _, _, decr_sp(Size)).
opt_util__replace_labels_instr(init_sync_term(T, N), _, _,
		init_sync_term(T, N)).
opt_util__replace_labels_instr(fork(Child0, Parent0, SlotCount), Replmap, _,
		fork(Child, Parent, SlotCount)) :-
	opt_util__replace_labels_label(Child0, Replmap, Child),
	opt_util__replace_labels_label(Parent0, Replmap, Parent).
opt_util__replace_labels_instr(join_and_terminate(Lval0), Replmap, _,
		join_and_terminate(Lval)) :-
	opt_util__replace_labels_lval(Lval0, Replmap, Lval).
opt_util__replace_labels_instr(join_and_continue(Lval0, Label0),
		Replmap, _, join_and_continue(Lval, Label)) :-
	opt_util__replace_labels_label(Label0, Replmap, Label),
	opt_util__replace_labels_lval(Lval0, Replmap, Lval).
opt_util__replace_labels_instr(pragma_c(A, Comps0, C, MaybeFix, MaybeLayout,
		MaybeOnlyLayout, MaybeSub0, F), ReplMap, _,
		pragma_c(A, Comps, C, MaybeFix, MaybeLayout, MaybeOnlyLayout,
			MaybeSub, F)) :-
	(
		MaybeFix = no
	;
		MaybeFix = yes(FixLabel0),
		opt_util__replace_labels_label(FixLabel0, ReplMap, FixLabel),
			% We cannot replace the label in the C code string
			% itself.
		require(unify(FixLabel0, FixLabel),
			"trying to replace Mercury label in C code")
	),
	(
		MaybeLayout = no
	;
		MaybeLayout = yes(LayoutLabel0),
		opt_util__replace_labels_label(LayoutLabel0, ReplMap,
			LayoutLabel),
			% We cannot replace the label that has a layout
			% structure.
		require(unify(LayoutLabel0, LayoutLabel),
			"trying to replace Mercury label with layout")
	),
	(
		MaybeOnlyLayout = no
	;
		MaybeOnlyLayout = yes(OnlyLayoutLabel0),
		opt_util__replace_labels_label(OnlyLayoutLabel0, ReplMap,
			OnlyLayoutLabel),
			% We cannot replace the label that has a layout
			% structure.
		require(unify(OnlyLayoutLabel0, OnlyLayoutLabel),
			"trying to replace Mercury label with layout")
	),
	(
		MaybeSub0 = no,
		MaybeSub = no,
		Comps = Comps0
	;
		MaybeSub0 = yes(SubLabel0),
		opt_util__replace_labels_label(SubLabel0, ReplMap, SubLabel),
		MaybeSub = yes(SubLabel),
		opt_util__replace_labels_comps(Comps0, ReplMap, Comps)
	).

:- pred opt_util__replace_labels_comps(list(pragma_c_component),
	map(label, label), list(pragma_c_component)).
:- mode opt_util__replace_labels_comps(in, in, out) is det.

opt_util__replace_labels_comps([], _, []).
opt_util__replace_labels_comps([Comp0 | Comps0], ReplMap, [Comp | Comps]) :-
	opt_util__replace_labels_comp(Comp0, ReplMap, Comp),
	opt_util__replace_labels_comps(Comps0, ReplMap, Comps).

:- pred opt_util__replace_labels_comp(pragma_c_component,
	map(label, label), pragma_c_component).
:- mode opt_util__replace_labels_comp(in, in, out) is det.

opt_util__replace_labels_comp(pragma_c_inputs(A), _, pragma_c_inputs(A)).
opt_util__replace_labels_comp(pragma_c_outputs(A), _, pragma_c_outputs(A)).
opt_util__replace_labels_comp(pragma_c_user_code(A, B), _,
		pragma_c_user_code(A, B)).
opt_util__replace_labels_comp(pragma_c_raw_code(A, B), _,
		pragma_c_raw_code(A, B)).
opt_util__replace_labels_comp(pragma_c_fail_to(Label0), ReplMap,
		pragma_c_fail_to(Label)) :-
	opt_util__replace_labels_label(Label0, ReplMap, Label).
opt_util__replace_labels_comp(pragma_c_noop, _, pragma_c_noop).

:- pred opt_util__replace_labels_lval(lval, map(label, label), lval).
:- mode opt_util__replace_labels_lval(in, in, out) is det.

opt_util__replace_labels_lval(reg(RegType, RegNum), _, reg(RegType, RegNum)).
opt_util__replace_labels_lval(stackvar(N), _, stackvar(N)).
opt_util__replace_labels_lval(framevar(N), _, framevar(N)).
opt_util__replace_labels_lval(succip, _, succip).
opt_util__replace_labels_lval(maxfr, _, maxfr).
opt_util__replace_labels_lval(curfr, _, curfr).
opt_util__replace_labels_lval(succip(Rval0), ReplMap, succip(Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_lval(redoip(Rval0), ReplMap, redoip(Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_lval(redofr(Rval0), ReplMap, redofr(Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_lval(succfr(Rval0), ReplMap, succfr(Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_lval(prevfr(Rval0), ReplMap, prevfr(Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_lval(hp, _, hp).
opt_util__replace_labels_lval(sp, _, sp).
opt_util__replace_labels_lval(field(Tag, Base0, Offset0), ReplMap,
		field(Tag, Base, Offset)) :-
	opt_util__replace_labels_rval(Base0, ReplMap, Base),
	opt_util__replace_labels_rval(Offset0, ReplMap, Offset).
opt_util__replace_labels_lval(lvar(Var), _, lvar(Var)).
opt_util__replace_labels_lval(temp(Type, Num), _, temp(Type, Num)).
opt_util__replace_labels_lval(mem_ref(Rval0), ReplMap, mem_ref(Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).

:- pred opt_util__replace_labels_rval(rval::in, map(label, label)::in,
	rval::out) is det.

opt_util__replace_labels_rval(lval(Lval0), ReplMap, lval(Lval)) :-
	opt_util__replace_labels_lval(Lval0, ReplMap, Lval).
opt_util__replace_labels_rval(var(Var), _, var(Var)).
opt_util__replace_labels_rval(
		create(Tag, Rvals, ArgTypes, StatDyn, N, Msg, Reuse), _,
		create(Tag, Rvals, ArgTypes, StatDyn, N, Msg, Reuse)).
opt_util__replace_labels_rval(mkword(Tag, Rval0), ReplMap, mkword(Tag, Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_rval(const(Const0), ReplMap, const(Const)) :-
	opt_util__replace_labels_rval_const(Const0, ReplMap, Const).
opt_util__replace_labels_rval(unop(Op, Rval0), ReplMap, unop(Op, Rval)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).
opt_util__replace_labels_rval(binop(Op, LRval0, RRval0), ReplMap,
		binop(Op, LRval, RRval)) :-
	opt_util__replace_labels_rval(LRval0, ReplMap, LRval),
	opt_util__replace_labels_rval(RRval0, ReplMap, RRval).
opt_util__replace_labels_rval(mem_addr(MemRef0), ReplMap, mem_addr(MemRef)) :-
	opt_util__replace_labels_mem_ref(MemRef0, ReplMap, MemRef).

:- pred opt_util__replace_labels_mem_ref(mem_ref::in, map(label, label)::in,
	mem_ref::out) is det.

opt_util__replace_labels_mem_ref(stackvar_ref(N), _, stackvar_ref(N)).
opt_util__replace_labels_mem_ref(framevar_ref(N), _, framevar_ref(N)).
opt_util__replace_labels_mem_ref(heap_ref(Rval0, Tag, N), ReplMap,
		heap_ref(Rval, Tag, N)) :-
	opt_util__replace_labels_rval(Rval0, ReplMap, Rval).

:- pred opt_util__replace_labels_rval_const(rval_const::in,
	map(label, label)::in, rval_const::out) is det.

opt_util__replace_labels_rval_const(true, _, true).
opt_util__replace_labels_rval_const(false, _, false).
opt_util__replace_labels_rval_const(int_const(N), _, int_const(N)).
opt_util__replace_labels_rval_const(float_const(N), _, float_const(N)).
opt_util__replace_labels_rval_const(string_const(S), _, string_const(S)).
opt_util__replace_labels_rval_const(multi_string_const(L, S), _,
	multi_string_const(L, S)).
opt_util__replace_labels_rval_const(code_addr_const(Addr0), ReplMap,
		code_addr_const(Addr)) :-
	opt_util__replace_labels_code_addr(Addr0, ReplMap, Addr).
opt_util__replace_labels_rval_const(data_addr_const(DataAddr), _,
		data_addr_const(DataAddr)).
opt_util__replace_labels_rval_const(label_entry(Label), _, label_entry(Label)).

:- pred opt_util__replace_labels_code_addr(code_addr::in, map(label, label)::in,
	code_addr::out) is det.

opt_util__replace_labels_code_addr(label(Label0), ReplMap, label(Label)) :-
	opt_util__replace_labels_label(Label0, ReplMap, Label).
opt_util__replace_labels_code_addr(imported(Proc), _, imported(Proc)).
opt_util__replace_labels_code_addr(succip, _, succip).
opt_util__replace_labels_code_addr(do_succeed(Last), _, do_succeed(Last)).
opt_util__replace_labels_code_addr(do_redo, _, do_redo).
opt_util__replace_labels_code_addr(do_fail, _, do_fail).
opt_util__replace_labels_code_addr(do_trace_redo_fail_shallow, _,
	do_trace_redo_fail_shallow).
opt_util__replace_labels_code_addr(do_trace_redo_fail_deep, _,
	do_trace_redo_fail_deep).
opt_util__replace_labels_code_addr(do_call_closure, _, do_call_closure).
opt_util__replace_labels_code_addr(do_call_class_method, _,
	do_call_class_method).
opt_util__replace_labels_code_addr(do_det_aditi_call, _, do_det_aditi_call).
opt_util__replace_labels_code_addr(do_semidet_aditi_call, _,
		do_semidet_aditi_call).
opt_util__replace_labels_code_addr(do_nondet_aditi_call, _,
		do_nondet_aditi_call).
opt_util__replace_labels_code_addr(do_aditi_insert, _, do_aditi_insert).
opt_util__replace_labels_code_addr(do_aditi_delete, _, do_aditi_delete).
opt_util__replace_labels_code_addr(do_aditi_bulk_insert, _,
		do_aditi_bulk_insert).
opt_util__replace_labels_code_addr(do_aditi_bulk_delete, _,
		do_aditi_bulk_delete).
opt_util__replace_labels_code_addr(do_aditi_bulk_modify, _,
		do_aditi_bulk_modify).
opt_util__replace_labels_code_addr(do_not_reached, _, do_not_reached).

:- pred opt_util__replace_labels_label_list(list(label)::in,
	map(label, label)::in, list(label)::out) is det.

opt_util__replace_labels_label_list([], _ReplMap, []).
opt_util__replace_labels_label_list([Label0 | Labels0], ReplMap,
		[Label | Labels]) :-
	opt_util__replace_labels_label(Label0, ReplMap, Label),
	opt_util__replace_labels_label_list(Labels0, ReplMap, Labels).

:- pred opt_util__replace_labels_label(label::in, map(label, label)::in,
	label::out) is det.

opt_util__replace_labels_label(Label0, ReplMap, Label) :-
	( map__search(ReplMap, Label0, NewLabel) ->
		Label = NewLabel
	;
		Label = Label0
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
