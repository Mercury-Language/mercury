%-----------------------------------------------------------------------------%

% Utilities for LLDS to LLDS peephole optimization.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module opt_util.

:- interface.
:- import_module llds, list, int, string, std_util.

:- type instrmap == map(label, instruction).
:- type lvalmap == map(label, maybe(instruction)).
:- type tailmap == map(label, list(instruction)).
:- type succmap == map(label, bool).

:- pred opt_util__gather_comments(list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__gather_comments(in, out, out) is det.

:- pred opt_util__gather_comments_livevals(list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__gather_comments_livevals(in, out, out) is det.

:- pred opt_util__skip_comments(list(instruction), list(instruction)).
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

	% Find the first label in the instruction stream.

:- pred opt_util__find_first_label(list(instruction), label).
:- mode opt_util__find_first_label(in, out) is det.

	% Check whether the named label follows without any intervening code.
	% If yes, return the instructions after the label.

	% Skip to the next label, returning the code before the label,
	% and the label together with the code after the label.

:- pred opt_util__skip_to_next_label(list(instruction),
	list(instruction), list(instruction)).
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
	% by a semidet proceed with the same value of r1?

:- pred opt_util__is_forkproceed_next(list(instruction), map(label, bool),
	list(instruction)).
:- mode opt_util__is_forkproceed_next(in, in, out) is semidet.

 	% Does the following code consist of straighline instructions
	% except possibly if_val(..., dofail), and then a succeed?
	% If yes, then return all the instructions up to the succeed,
	% and all the following instructions.

:- pred opt_util__straight_alternative(list(instruction), list(instruction),
	list(instruction)).
:- mode opt_util__straight_alternative(in, out, out) is semidet.

	% Remove the assignment to r1 from the list returned by
	% opt_util__is_sdproceed_next.

:- pred opt_util__filter_out_r1(list(instruction), list(instruction)).
:- mode opt_util__filter_out_r1(in, out) is det.

	% Remove the livevals instruction from the list returned by
	% opt_util__is_proceed_next.

:- pred opt_util__filter_out_livevals(list(instruction), list(instruction)).
:- mode opt_util__filter_out_livevals(in, out) is det.

	% Get just the livevals instructions from a list of instructions.

:- pred opt_util__filter_in_livevals(list(instruction), list(instruction)).
:- mode opt_util__filter_in_livevals(in, out) is det.

	% See if an instruction sequence contains incr_sp, and if yes,
	% what is the increment.

% :- pred opt_util__has_incr_sp(list(instruction), int).
% :- mode opt_util__has_incr_sp(in, out) is semidet.

	% See if an instruction sequence contains decr_sp, and if yes,
	% what is the decrement.

% :- pred opt_util__has_decr_sp(list(instruction), int).
% :- mode opt_util__has_decr_sp(in, out) is semidet.

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
	% by an instruction.

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

:- pred opt_util__count_temps_instr_list(list(instruction), int, int).
:- mode opt_util__count_temps_instr_list(in, in, out) is det.

:- pred opt_util__count_temps_instr(instr, int, int).
:- mode opt_util__count_temps_instr(in, in, out) is det.

	% See whether an lval references any stackvars.

:- pred opt_util__lval_refers_stackvars(lval, bool).
:- mode opt_util__lval_refers_stackvars(in, out) is det.

	% See whether an rval references any stackvars.

:- pred opt_util__rval_refers_stackvars(rval, bool).
:- mode opt_util__rval_refers_stackvars(in, out) is det.

	% See whether a list of maybe rvals references any stackvars.

:- pred opt_util__rvals_refer_stackvars(list(maybe(rval)), bool).
:- mode opt_util__rvals_refer_stackvars(in, out) is det.

	% See whether a list of instructions references any stackvars.

:- pred opt_util__block_refers_stackvars(list(instruction), bool).
:- mode opt_util__block_refers_stackvars(in, out) is det.

	% Format a label for verbose messages during compilation

:- pred opt_util__format_label(label, string).
:- mode opt_util__format_label(in, out) is det.

	% Find out if an instruction sequence has both incr_sp and decr_sp.

:- pred opt_util__has_both_incr_decr_sp(list(instruction)).
:- mode opt_util__has_both_incr_decr_sp(in) is semidet.

	% Remove all incr_sp and decr_sp from an instruction sequence.

:- pred opt_util__remove_both_incr_decr_sp(list(instruction),
	list(instruction)).
:- mode opt_util__remove_both_incr_decr_sp(di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, require.

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
	( Uinstr = modframe(Redoip0) ->
		Redoip = Redoip0,
		list__reverse(RevSkip, Skip),
		Rest = Instrs
	; Uinstr = mkframe(_, _, _) ->
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
	Instr7 = goto(succip, _) - _,
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
	Instr5 = assign(reg(r(1)), const(R1val)) - _,
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
	Instr9 = goto(succip, _) - _,
	InstrsBetween = [Instr1use, Instr3use, Instr5, Instr7].

opt_util__is_succeed_next(Instrs0, InstrsBetweenIncl) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = livevals(_) - _,
	opt_util__skip_comments_labels(Instrs2, Instrs3),
	Instrs3 = [Instr3 | _],
	Instr3 = goto(do_succeed(_), _) - _,
	InstrsBetweenIncl = [Instr1, Instr3].

	% When we return Between, we are implicitly assuming that
	% the other continuation' instruction sequence is the same
	% expect for the value assigned to r1. If this isn't true,
	% then we are up shit creek anyway.

opt_util__is_forkproceed_next(Instrs0, Succmap, Between) :-
	opt_util__skip_comments_labels(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	( Instr1 = if_val(lval(reg(r(1))), label(BranchLabel)) - _ ->
		map__search(Succmap, BranchLabel, BranchSuccess),
		BranchSuccess = yes,
		opt_util__is_sdproceed_next_sf(Instrs2, Between, FallSuccess),
		FallSuccess = no
	; Instr1 = if_val(unop(not, lval(reg(r(1)))), label(BranchLabel)) - _ ->
		map__search(Succmap, BranchLabel, BranchSuccess),
		BranchSuccess = no,
		opt_util__is_sdproceed_next_sf(Instrs2, Between, FallSuccess),
		FallSuccess = yes
	;
		fail
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
			opt_util__can_instr_branch_away(Uinstr0, no)
		;
			Uinstr0 = if_val(_, CodeAddr),
			( CodeAddr = do_fail ; CodeAddr = do_redo )
		)
	->
		opt_util__straight_alternative_2(Instrs0, [Instr0 | Between0],
			Between, After)
	;
		Uinstr0 = goto(do_succeed(no), do_succeed(no))
	->
		Between = Between0,
		After = Instrs0
	;
		fail
	).

:- pred opt_util__no_stack_straight_line(list(instruction),
	list(instruction), list(instruction)).
:- mode opt_util__no_stack_straight_line(in, out, out) is det.

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

opt_util__lval_refers_stackvars(reg(_), no).
opt_util__lval_refers_stackvars(stackvar(_), yes).
opt_util__lval_refers_stackvars(framevar(_), _) :-
	error("found framevar in lval_refers_stackvars").
opt_util__lval_refers_stackvars(succip, no).
opt_util__lval_refers_stackvars(maxfr, no).
opt_util__lval_refers_stackvars(curfr, no).
opt_util__lval_refers_stackvars(redoip(Rval), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers).
opt_util__lval_refers_stackvars(hp, no).
opt_util__lval_refers_stackvars(sp, no).
opt_util__lval_refers_stackvars(field(_, Rval, FieldNum), Refers) :-
	opt_util__rval_refers_stackvars(Rval, Refers1),
	opt_util__rval_refers_stackvars(FieldNum, Refers2),
	bool__or(Refers1, Refers2, Refers).
opt_util__lval_refers_stackvars(lvar(_), _) :-
	error("found lvar in lval_refers_stackvars").
opt_util__lval_refers_stackvars(temp(_), no).

opt_util__rval_refers_stackvars(lval(Lval), Refers) :-
	opt_util__lval_refers_stackvars(Lval, Refers).
opt_util__rval_refers_stackvars(var(_), _) :-
	error("found var in rval_refers_stackvars").
opt_util__rval_refers_stackvars(create(_, Rvals, _), Refers) :-
	opt_util__rvals_refer_stackvars(Rvals, Refers).
opt_util__rval_refers_stackvars(mkword(_, Baserval), Refers) :-
	opt_util__rval_refers_stackvars(Baserval, Refers).
opt_util__rval_refers_stackvars(const(_), no).
opt_util__rval_refers_stackvars(unop(_, Baserval), Refers) :-
	opt_util__rval_refers_stackvars(Baserval, Refers).
opt_util__rval_refers_stackvars(binop(_, Baserval1, Baserval2), Refers) :-
	opt_util__rval_refers_stackvars(Baserval1, Refers1),
	opt_util__rval_refers_stackvars(Baserval2, Refers2),
	bool__or(Refers1, Refers2, Refers).

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

opt_util__block_refers_stackvars([], no).
opt_util__block_refers_stackvars([Uinstr0 - _ | Instrs0], Need) :-
	(
		Uinstr0 = comment(_),
		opt_util__block_refers_stackvars(Instrs0, Need)
	;
		Uinstr0 = livevals(_),
		opt_util__block_refers_stackvars(Instrs0, Need)
	;
		Uinstr0 = block(_, BlockInstrs),
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
		Uinstr0 = call_closure(_, _, _),
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
		Uinstr0 = goto(_, _),
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
		Uinstr0 = incr_hp(Lval, _, Rval),
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
		% handled specially
		Uinstr0 = incr_sp(_),
		Need = no
	;
		% handled specially
		Uinstr0 = decr_sp(_),
		Need = no
	).

opt_util__filter_out_r1([], []).
opt_util__filter_out_r1([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_out_r1(Instrs0, Instrs1),
	( Instr0 = assign(reg(r(1)), const(_)) - _ ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).

opt_util__filter_out_livevals([], []).
opt_util__filter_out_livevals([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_out_livevals(Instrs0, Instrs1),
	( Instr0 = livevals(_) - _Comment ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).

opt_util__filter_in_livevals([], []).
opt_util__filter_in_livevals([Instr0 | Instrs0], Instrs) :-
	opt_util__filter_in_livevals(Instrs0, Instrs1),
	( Instr0 = livevals(_) - _Comment ->
		Instrs = [Instr0 | Instrs1]
	;
		Instrs = Instrs1
	).

% opt_util__has_incr_sp([Instr0 | Instrs0], Inc) :-
% 	( Instr0 = incr_sp(N) - _Comment ->
% 		Inc = N
% 	;
% 		opt_util__has_incr_sp(Instrs0, Inc)
% 	).

% opt_util__has_decr_sp([Instr0 | Instrs0], Dec) :-
% 	( Instr0 = decr_sp(N) - _Comment ->
% 		Dec = N
% 	;
% 		opt_util__has_decr_sp(Instrs0, Dec)
% 	).

opt_util__new_label_no([], N, N).
opt_util__new_label_no([Instr0 | Instrs0], N0, N) :-
	( Instr0 = label(local(_, K)) - _Comment ->
		( K < N0 ->
			N1 = N0
		;
			N1 is K + 1
		)
	;
		N1 = N0
	),
	opt_util__new_label_no(Instrs0, N1, N).

opt_util__can_instr_branch_away(comment(_), no).
opt_util__can_instr_branch_away(livevals(_), no).
opt_util__can_instr_branch_away(block(_, _), yes).
opt_util__can_instr_branch_away(assign(_, _), no).
opt_util__can_instr_branch_away(call(_, _, _, _), yes).
opt_util__can_instr_branch_away(call_closure(_, _, _), yes).
opt_util__can_instr_branch_away(mkframe(_, _, _), no).
opt_util__can_instr_branch_away(modframe(_), no).
opt_util__can_instr_branch_away(label(_), no).
opt_util__can_instr_branch_away(goto(_, _), yes).
opt_util__can_instr_branch_away(computed_goto(_, _), yes).
opt_util__can_instr_branch_away(c_code(_), no).
opt_util__can_instr_branch_away(if_val(_, _), yes).
opt_util__can_instr_branch_away(incr_hp(_, _, _), no).
opt_util__can_instr_branch_away(mark_hp(_), no).
opt_util__can_instr_branch_away(restore_hp(_), no).
opt_util__can_instr_branch_away(incr_sp(_), no).
opt_util__can_instr_branch_away(decr_sp(_), no).

opt_util__can_instr_fall_through(comment(_), yes).
opt_util__can_instr_fall_through(livevals(_), yes).
opt_util__can_instr_fall_through(block(_, _), yes).
opt_util__can_instr_fall_through(assign(_, _), yes).
opt_util__can_instr_fall_through(call(_, _, _, _), no).
opt_util__can_instr_fall_through(call_closure(_, _, _), no).
opt_util__can_instr_fall_through(mkframe(_, _, _), yes).
opt_util__can_instr_fall_through(modframe(_), yes).
opt_util__can_instr_fall_through(label(_), yes).
opt_util__can_instr_fall_through(goto(_, _), no).
opt_util__can_instr_fall_through(computed_goto(_, _), no).
opt_util__can_instr_fall_through(c_code(_), yes).
opt_util__can_instr_fall_through(if_val(_, _), yes).
opt_util__can_instr_fall_through(incr_hp(_, _, _), yes).
opt_util__can_instr_fall_through(mark_hp(_), yes).
opt_util__can_instr_fall_through(restore_hp(_), yes).
opt_util__can_instr_fall_through(incr_sp(_), yes).
opt_util__can_instr_fall_through(decr_sp(_), yes).

opt_util__instr_labels(comment(_), [], []).
opt_util__instr_labels(livevals(_), [], []).
opt_util__instr_labels(block(_, Instrs), Labels, CodeAddrs) :-
	opt_util__instr_list_labels(Instrs, Labels, CodeAddrs).
opt_util__instr_labels(assign(_,_), [], []).
opt_util__instr_labels(call(Target, Ret, _, _), [], [Target, Ret]).
opt_util__instr_labels(call_closure(_, Ret, _), [], [Ret]).
opt_util__instr_labels(mkframe(_, _, Addr), [], [Addr]).
opt_util__instr_labels(modframe(Addr), [], [Addr]).
opt_util__instr_labels(label(_), [], []).
opt_util__instr_labels(goto(Addr, _), [], [Addr]).
opt_util__instr_labels(computed_goto(_, Labels), Labels, []).
opt_util__instr_labels(c_code(_), [], []).
opt_util__instr_labels(if_val(_, Addr), [], [Addr]).
opt_util__instr_labels(incr_hp(_, _, _), [], []).
opt_util__instr_labels(mark_hp(_), [], []).
opt_util__instr_labels(restore_hp(_), [], []).
opt_util__instr_labels(incr_sp(_), [], []).
opt_util__instr_labels(decr_sp(_), [], []).

opt_util__instr_list_labels([], [], []).
opt_util__instr_list_labels([Uinstr - _ | Instrs], Labels, CodeAddrs) :-
	opt_util__instr_labels(Uinstr, Labels0, CodeAddrs0),
	opt_util__instr_list_labels(Instrs, Labels1, CodeAddrs1),
	list__append(Labels0, Labels1, Labels),
	list__append(CodeAddrs0, CodeAddrs1, CodeAddrs).

opt_util__livevals_addr(label(Label), Result) :-
	( Label = local(_,_) ->
		Result = no
	;	
		Result = yes
	).
opt_util__livevals_addr(imported(_), yes).
opt_util__livevals_addr(succip, yes).
opt_util__livevals_addr(do_succeed(_), yes).
opt_util__livevals_addr(do_redo, no).
opt_util__livevals_addr(do_fail, no).

opt_util__count_temps_instr_list([], N, N).
opt_util__count_temps_instr_list([Uinstr - _Comment | Instrs], N0, N) :-
	opt_util__count_temps_instr(Uinstr, N0, N1),
	opt_util__count_temps_instr_list(Instrs, N1, N).

opt_util__count_temps_instr(comment(_), N, N).
opt_util__count_temps_instr(livevals(_), N, N).
opt_util__count_temps_instr(block(_, _), N, N).
opt_util__count_temps_instr(assign(Lval, Rval), N0, N) :-
	opt_util__count_temps_lval(Lval, N0, N1),
	opt_util__count_temps_rval(Rval, N1, N).
opt_util__count_temps_instr(call(_, _, _, _), N, N).
opt_util__count_temps_instr(call_closure(_, _, _), N, N).
opt_util__count_temps_instr(mkframe(_, _, _), N, N).
opt_util__count_temps_instr(modframe(_), N, N).
opt_util__count_temps_instr(label(_), N, N).
opt_util__count_temps_instr(goto(_, _), N, N).
opt_util__count_temps_instr(computed_goto(Rval, _), N0, N) :-
	opt_util__count_temps_rval(Rval, N0, N).
opt_util__count_temps_instr(if_val(Rval, _), N0, N) :-
	opt_util__count_temps_rval(Rval, N0, N).
opt_util__count_temps_instr(c_code(_), N, N).
opt_util__count_temps_instr(incr_hp(Lval, _, Rval), N0, N) :-
	opt_util__count_temps_lval(Lval, N0, N1),
	opt_util__count_temps_rval(Rval, N1, N).
opt_util__count_temps_instr(mark_hp(Lval), N0, N) :-
	opt_util__count_temps_lval(Lval, N0, N).
opt_util__count_temps_instr(restore_hp(Rval), N0, N) :-
	opt_util__count_temps_rval(Rval, N0, N).
opt_util__count_temps_instr(incr_sp(_), N, N).
opt_util__count_temps_instr(decr_sp(_), N, N).

:- pred opt_util__count_temps_lval(lval, int, int).
:- mode opt_util__count_temps_lval(in, in, out) is det.

opt_util__count_temps_lval(Lval, N0, N) :-
	( Lval = temp(T) ->
		int__max(N0, T, N)
	; Lval = field(_, Rval, FieldNum) ->
		opt_util__count_temps_rval(Rval, N0, N1),
		opt_util__count_temps_rval(FieldNum, N1, N)
	;
		N = N0
	).

:- pred opt_util__count_temps_rval(rval, int, int).
:- mode opt_util__count_temps_rval(in, in, out) is det.

% XXX assume that we don't generate code
% that uses a temp var without defining it.
opt_util__count_temps_rval(_, N, N).

opt_util__format_label(local(ProcLabel), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).
opt_util__format_label(local(ProcLabel, _), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).
opt_util__format_label(exported(ProcLabel), Str) :-
	opt_util__format_proclabel(ProcLabel, Str).

:- pred opt_util__format_proclabel(proc_label, string).
:- mode opt_util__format_proclabel(in, out) is det.

opt_util__format_proclabel(proc(_Module, Pred, Arity, Mode), Str) :-
	string__int_to_string(Arity, ArityStr),
	string__int_to_string(Mode, ModeStr),
	string__append_list([Pred, "/", ArityStr, " mode ", ModeStr], Str).
opt_util__format_proclabel(special_proc(_Module, Pred, Type, Arity, Mode),
		Str) :-
	string__int_to_string(Arity, ArityStr),
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
	( Uinstr = incr_sp(_) ->
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

opt_util__remove_both_incr_decr_sp([], []).
opt_util__remove_both_incr_decr_sp([Instr0 | Instrs0], Instrs) :-
	opt_util__remove_both_incr_decr_sp(Instrs0, Instrs1),
	( Uinstr = incr_sp(_) - _ ->
		Instrs = Instrs1
	; Uinstr = decr_sp(_) - _ ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).

opt_util__touches_nondet_ctrl([], no).
opt_util__touches_nondet_ctrl([Uinstr - _ | Instrs], Touch) :-
	( Uinstr = assign(Lval, Rval) ->
		opt_util__touches_nondet_ctrl_lval(Lval, TouchLval),
		opt_util__touches_nondet_ctrl_rval(Rval, TouchRval),
		bool__or(TouchLval, TouchRval, Touch0)
	; Uinstr = incr_hp(Lval, _, Rval) ->
		opt_util__touches_nondet_ctrl_lval(Lval, TouchLval),
		opt_util__touches_nondet_ctrl_rval(Rval, TouchRval),
		bool__or(TouchLval, TouchRval, Touch0)
	; Uinstr = mark_hp(Lval) ->
		opt_util__touches_nondet_ctrl_lval(Lval, Touch0)
	; Uinstr = restore_hp(Rval) ->
		opt_util__touches_nondet_ctrl_rval(Rval, Touch0)
	;
		Touch0 = yes
	),
	(
		Touch0 = yes,
		Touch = yes
	;
		Touch0 = no,
		opt_util__touches_nondet_ctrl(Instrs, Touch)
	).

:- pred opt_util__touches_nondet_ctrl_lval(lval, bool).
:- mode opt_util__touches_nondet_ctrl_lval(in, out) is det.

opt_util__touches_nondet_ctrl_lval(reg(_), no).
opt_util__touches_nondet_ctrl_lval(stackvar(_), no).
opt_util__touches_nondet_ctrl_lval(framevar(_), no).
opt_util__touches_nondet_ctrl_lval(succip, no).
opt_util__touches_nondet_ctrl_lval(maxfr, yes).
opt_util__touches_nondet_ctrl_lval(curfr, yes).
opt_util__touches_nondet_ctrl_lval(redoip(_), yes).
opt_util__touches_nondet_ctrl_lval(hp, no).
opt_util__touches_nondet_ctrl_lval(sp, no).
opt_util__touches_nondet_ctrl_lval(field(_, Rval1, Rval2), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval1, Touch1),
	opt_util__touches_nondet_ctrl_rval(Rval2, Touch2),
	bool__or(Touch1, Touch2, Touch).
opt_util__touches_nondet_ctrl_lval(lvar(_), no).
opt_util__touches_nondet_ctrl_lval(temp(_), no).

:- pred opt_util__touches_nondet_ctrl_rval(rval, bool).
:- mode opt_util__touches_nondet_ctrl_rval(in, out) is det.

opt_util__touches_nondet_ctrl_rval(lval(Lval), Touch) :-
	opt_util__touches_nondet_ctrl_lval(Lval, Touch).
opt_util__touches_nondet_ctrl_rval(var(_), no).
opt_util__touches_nondet_ctrl_rval(create(_, _, _), no).
opt_util__touches_nondet_ctrl_rval(mkword(_, Rval), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval, Touch).
opt_util__touches_nondet_ctrl_rval(const(_), no).
opt_util__touches_nondet_ctrl_rval(unop(_, Rval), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval, Touch).
opt_util__touches_nondet_ctrl_rval(binop(_, Rval1, Rval2), Touch) :-
	opt_util__touches_nondet_ctrl_rval(Rval1, Touch1),
	opt_util__touches_nondet_ctrl_rval(Rval2, Touch2),
	bool__or(Touch1, Touch2, Touch).

%-----------------------------------------------------------------------------%
