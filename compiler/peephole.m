%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% peephole.m - local LLDS to LLDS optimizations based on pattern-matching.

% Authors: fjh and zs.

%-----------------------------------------------------------------------------%

:- module peephole.

:- interface.

:- import_module bool, list.
:- import_module llds.

:- pred peephole__optimize(list(instruction), list(instruction), bool).
:- mode peephole__optimize(in, out, out) is det.

:- implementation.

:- import_module map, string, std_util.
:- import_module code_util, opt_util, opt_debug.

	% We zip down to the end of the instruction list, and start attempting
	% to optimize instruction sequences. As long as we can continue
	% optimizing the instruction sequence, we keep doing so;
	% when we find a sequence we can't optimize, we back up and try
	% to optimize the sequence starting with the previous instruction.

peephole__optimize([], [], no).
peephole__optimize([Instr0 - Comment | Instrs0], Instrs, Mod) :-
	peephole__optimize(Instrs0, Instrs1, Mod0),
	peephole__opt_instr(Instr0, Comment, Instrs1, Instrs, Mod1),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

	% Try to optimize the beginning of the given instruction sequence.
	% If successful, try it again.

:- pred peephole__opt_instr(instr, string,
	list(instruction), list(instruction), bool).
:- mode peephole__opt_instr(in, in, in, out, out) is det.

peephole__opt_instr(Instr0, Comment0, Instrs0, Instrs, Mod) :-
	(
		opt_util__skip_comments(Instrs0, Instrs1),
		peephole__match(Instr0, Comment0, Instrs1, Instrs2)
	->
		( Instrs2 = [Instr2 - Comment2 | Instrs3] ->
			peephole__opt_instr(Instr2, Comment2, Instrs3, Instrs,
				_)
		;
			Instrs = Instrs2
		),
		Mod = yes
	;
		Instrs = [Instr0 - Comment0 | Instrs0],
		Mod = no
	).

%-----------------------------------------------------------------------------%

	% Look for code patterns that can be optimized, and optimize them.

:- pred peephole__match(instr, string, list(instruction), list(instruction)).
:- mode peephole__match(in, in, in, out) is semidet.

	% A `goto' can be deleted if the target of the jump is the very
	% next instruction:
	%
	%	goto next;
	%	<comments, labels>	=>	<comments, labels>
	%     next:			      next:

peephole__match(goto(label(Label)), _, Instrs0, Instrs) :-
	opt_util__is_this_label_next(Label, Instrs0, _),
	Instrs = Instrs0.

	% A `computed_goto' with all branches pointing to the same 
	% label can be replaced with an unconditional goto. 

peephole__match(computed_goto(_, Labels), Comment, Instrs0, Instrs) :-
	list__all_same(Labels),
	Labels = [Target|_],
	Instrs = [goto(label(Target)) - Comment | Instrs0].

	% A conditional branch whose condition is constant
	% can be either elimininated or replaced by an unconditional goto.
	%
	% A conditional branch over a branch can be replaced
	% by an inverse conditional branch:
	%
	%	if (x) goto skip;		if (!x) goto somewhere
	%	<comments>			omit <comments>
	%	goto somewhere;		=>	<comments, labels>
	%	<comments, labels>	      skip:
	%     skip:
	%
	% A conditional branch to the very next instruction
	% can be deleted:
	%
	%	if (x) goto next;	=>	<comments, labels>
	%	<comments, labels>	      next:
	%     next:

peephole__match(if_val(Rval, label(Target)), Comment, Instrs0, Instrs) :-
	( opt_util__is_const_condition(Rval, Taken) ->
		(
			Taken = yes,
			Instrs = [goto(label(Target)) - Comment | Instrs0]
		;
			Taken = no,
			Instrs = Instrs0
		)
	;
		opt_util__skip_comments(Instrs0, Instrs1),
		( Instrs1 = [goto(Somewhere) - C2 | Instrs2] ->
			opt_util__is_this_label_next(Target, Instrs2, _),
			code_util__neg_rval(Rval, NotRval),
			Instrs = [if_val(NotRval, Somewhere) - C2 | Instrs2]
		;
			opt_util__is_this_label_next(Target, Instrs1, _),
			Instrs = Instrs0
		)
	).

	% If a `mkframe' is followed by a `modframe', with the instructions
	% in between containing only straight-line code, we can delete the
	% `modframe' and instead just set the redoip directly in the `mkframe'.
	% This should also be done if the modframe appears instead as an
	% assignment to the redoip of curfr or maxfr.
	%
	%	mkframe(D, S, _)	=>	mkframe(D, S, Redoip)
	%	<straightline instrs>		<straightline instrs>
	%	modframe(Redoip)
	%
	% If a `mkframe' is followed by a test that can fail, we try to
	% swap the two instructions to avoid doing the mkframe unnecessarily.
	%
	%	mkframe(D, S, dofail)	=>	if_val(test, redo)
	%	if_val(test, redo/fail)		mkframe(D, S, dofail)
	%
	%	mkframe(D, S, label)	=>	if_val(test, redo)
	%	if_val(test, fail)		mkframe(D, S, label)
	%
	%	mkframe(D, S, label)	=>	mkframe(D, S, label)
	%	if_val(test, redo)		if_val(test, label)
	%
	% These two patterns are mutually exclusive because if_val is not
	% straigh-line code.

peephole__match(mkframe(Name, Slots, Redoip1), Comment, Instrs0, Instrs) :-
	(
		opt_util__next_modframe(Instrs0, [], Redoip2, Skipped, Rest),
		opt_util__touches_nondet_ctrl(Skipped, no)
	->
		list__append(Skipped, Rest, Instrs1),
		Instrs = [mkframe(Name, Slots, Redoip2) - Comment | Instrs1]
	;
		opt_util__skip_comments_livevals(Instrs0, Instrs1),
		Instrs1 = [Instr1 | Instrs2],
		Instr1 = if_val(Test, Target) - Comment2,
		(
			Redoip1 = do_fail,
			( Target = do_redo ; Target = do_fail)
		->
			Instrs = [
				if_val(Test, do_redo) - Comment2,
				mkframe(Name, Slots, do_fail) - Comment
				| Instrs2
			]
		;
			Redoip1 = label(_)
		->
			(
				Target = do_fail
			->
				Instrs = [
					if_val(Test, do_redo) - Comment2,
					mkframe(Name, Slots, Redoip1) - Comment
					| Instrs2
				]
			;
				Target = do_redo
			->
				Instrs = [
					mkframe(Name, Slots, Redoip1) - Comment,
					if_val(Test, Redoip1) - Comment2
					| Instrs2
				]
			;
				fail
			)
		;
			fail
		)
	).

	% If a `store_ticket' is followed by a `restore_ticket',
	% we can delete the `restore_ticket'.
	%
	%	store_ticket(Lval)	=>	store_ticket(Lval)
	%	restore_ticket(Lval)

peephole__match(store_ticket(Lval), Comment, Instrs0, Instrs) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [restore_ticket(lval(Lval)) - _Comment2 | Instrs2],
	Instrs = [store_ticket(Lval) - Comment | Instrs2].

	% If a `modframe' is followed by another, with the instructions
	% in between containing only straight-line code, we can delete
	% one of the modframes:
	%
	%	modframe(Redoip1)	=>	modframe(Redoip2)
	%	<straightline instrs>		<straightline instrs>
	%	modframe(Redoip2)

	% If a modframe(do_fail) is followed by straight-line instructions
	% except possibly for if_val with do_fail or do_redo as target,
	% until a goto to do_succeed(no), and if the nondet stack linkages
	% are not touched by the straight-line instructions, then we can
	% discard the nondet stack frame early.

peephole__match(modframe(Redoip), Comment, Instrs0, Instrs) :-
	(
		opt_util__next_modframe(Instrs0, [], Redoip2, Skipped, Rest),
		opt_util__touches_nondet_ctrl(Skipped, no)
	->
		list__append(Skipped, Rest, Instrs1),
		Instrs = [modframe(Redoip2) - Comment | Instrs1]
	;
		Redoip = do_fail,
		opt_util__straight_alternative(Instrs0, Between, After),
		opt_util__touches_nondet_ctrl(Between, no)
	->
		list__condense([Between,
			[goto(do_succeed(yes)) - "early discard"], After],
				Instrs)
	;
		fail
	).

	% If a decr_sp follows an incr_sp of the same amount, with the code
	% in between not referencing the stack, except possibly for a
	% restoration of succip, then the two cancel out. Assignments to
	% stack slots are allowed and are thrown away.
	%
	%	incr_sp N
	%	<...>		=>	<...>
	%	decr_sp N
	%
	%	incr_sp N
	%	<...>		=>	<...>
	%	succip = detstackvar(N)
	%	decr_sp N

	% The following transformation is sometimes useful because of the
	% limitations of frameopt:
	%
	%	incr_sp N
	%	goto L2
	%     L1:		=>    L1:
	%	incr_sp N		incr_sp N
	%     L2:		      L2:

peephole__match(incr_sp(N, Msg), _, Instrs0, Instrs) :-
	(
		opt_util__no_stackvars_til_decr_sp(Instrs0, N, Between, Remain)
	->
		list__append(Between, Remain, Instrs)
	;
		Instrs0 = [Instr0, Instr1, Instr2, Instr3 | Instrs3],
		Instr0 = goto(label(L2)) - _,
		Instr1 = label(_) - _,
		Instr2 = incr_sp(N, Msg) - _,
		Instr3 = label(L2) - _
	->
		Instrs = [Instr1, Instr2, Instr3 | Instrs3]
	;
		fail
	).

	% If an incr_sp follows a decr_sp of the same amount, then the two
	% cancel out. The code in-between may assign to stack vars as long
	% as it doesn't use stack vars; useless assignments to stack vars
	% are removed. Any if_vals in the interval must go to a label that
	% has a parallel that expects a stack frame.
	%
	%	decr_sp N
	%	<no uses of stackvars>
	%	<if_vals only with teardown equivalents>
	%	incr_sp N

peephole__match(decr_sp(N), _, Instrs0, Instrs) :-
	peephole__decr(N, Instrs0, Instrs).

%-----------------------------------------------------------------------------%

:- pred peephole__decr(int, list(instruction), list(instruction)).
:- mode peephole__decr(in, in, out) is semidet.

peephole__decr(N, Instrs0, Instrs) :-
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = Uinstr1 - _,
	(
		Uinstr1 = incr_sp(N, _),
		Instrs = Instrs2
	;
		Uinstr1 = assign(Lval, Rval),
		opt_util__lval_refers_stackvars(Lval, no),
		opt_util__rval_refers_stackvars(Rval, no),
		peephole__decr(N, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	;
		Uinstr1 = incr_hp(Lval, _Tag, Rval),
		opt_util__lval_refers_stackvars(Lval, no),
		opt_util__rval_refers_stackvars(Rval, no),
		peephole__decr(N, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	;
		Uinstr1 = comment(_),
		peephole__decr(N, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	;
		Uinstr1 = livevals(_),
		peephole__decr(N, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	).

%-----------------------------------------------------------------------------%
