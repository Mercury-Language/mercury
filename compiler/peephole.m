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

:- import_module list, llds, map, bimap.

:- pred peephole__main(list(instruction), list(instruction),
	bimap(label, label), bool).
:- mode peephole__main(in, out, in, out) is det.

:- implementation.

:- import_module code_util, opt_util, opt_debug, map, string, std_util.

peephole__main(Instrs0, Instrs, TeardownMap, Mod) :-
	map__init(Procmap0),
	map__init(Succmap0),
	peephole__build_procmaps(Instrs0, Procmap0, Procmap, Succmap0, Succmap),
	map__init(Forkmap0),
	peephole__build_forkmap(Instrs0, Succmap, Forkmap0, Forkmap),
	peephole__optimize(Instrs0, Instrs, Procmap, Forkmap, TeardownMap, Mod).

	% Build two maps, one for deterministic proceeds and one for
	% semideterministic proceeds. If a label is followed by code
	% that falls into one of these categories, it is entered into
	% the relevant map, with the target being the following code,
	% and for semideterministic proceeds, a success/failure indication.

:- pred peephole__build_procmaps(list(instruction), tailmap, tailmap,
	succmap, succmap).
% :- mode peephole__build_procmaps(in, di, uo, di, uo) is det.
:- mode peephole__build_procmaps(in, in, out, in, out) is det.

peephole__build_procmaps([], Procmap, Procmap, Succmap, Succmap).
peephole__build_procmaps([Instr - _Comment|Instrs], Procmap0, Procmap,
		Succmap0, Succmap) :-
	( Instr = label(Label) ->
		( opt_util__is_proceed_next(Instrs, Between) ->
			map__set(Procmap0, Label, Between, Procmap1),
			Succmap1 = Succmap0
		; opt_util__is_sdproceed_next_sf(Instrs, _, Success) ->
			map__set(Succmap0, Label, Success, Succmap1),
			Procmap1 = Procmap0
		;
			Procmap1 = Procmap0,
			Succmap1 = Succmap0
		)
	;
		Procmap1 = Procmap0,
		Succmap1 = Succmap0
	),
	peephole__build_procmaps(Instrs, Procmap1, Procmap,
		Succmap1, Succmap).

	% Find labels followed by a test of r1 where both paths set r1 to
	% its original value and proceed.

:- pred peephole__build_forkmap(list(instruction), succmap,
	tailmap, tailmap).
% :- mode peephole__build_forkmap(in, in, di, uo) is det.
:- mode peephole__build_forkmap(in, in, in, out) is det.

peephole__build_forkmap([], _Succmap, Forkmap, Forkmap).
peephole__build_forkmap([Instr - _Comment|Instrs], Succmap,
		Forkmap0, Forkmap) :-
	(
		Instr = label(Label),
		opt_util__is_forkproceed_next(Instrs, Succmap, Between)
	->
		map__set(Forkmap0, Label, Between, Forkmap1)
	;
		Forkmap1 = Forkmap0
	),
	peephole__build_forkmap(Instrs, Succmap, Forkmap1, Forkmap).

	% We zip down to the end of the instruction list, and start attempting
	% to optimize instruction sequences.  As long as we can continue
	% optimizing the instruction sequence, we keep doing so;
	% when we find a sequence we can't optimize, we back up try
	% to optimize the sequence starting with the previous instruction.

:- pred peephole__optimize(list(instruction), list(instruction),
	tailmap, tailmap, bimap(label, label), bool).
:- mode peephole__optimize(in, out, in, in, in, out) is det.

peephole__optimize([], [], _, _, _, no).
peephole__optimize([Instr0 - Comment | Instructions0], Instructions,
		Procmap, Forkmap, TeardownMap, Mod) :-
	peephole__optimize(Instructions0, Instructions1,
		Procmap, Forkmap, TeardownMap, Mod0),
	peephole__opt_instr(Instr0, Comment, Procmap, Forkmap, TeardownMap,
		Instructions1, Instructions, Mod1),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

	% Try to optimize the beginning of the given instruction sequence.
	% If successful, try it again.

:- pred peephole__opt_instr(instr, string, tailmap, tailmap,
	bimap(label, label), list(instruction), list(instruction), bool).
:- mode peephole__opt_instr(in, in, in, in, in, in, out, out) is det.

peephole__opt_instr(Instr0, Comment0, Procmap, Forkmap, TeardownMap,
		Instructions0, Instructions, Mod) :-
	(
		opt_util__skip_comments(Instructions0, Instructions1),
		peephole__match(Instr0, Comment0, Procmap, Forkmap, TeardownMap,
			Instructions1, Instructions2)
	->
		( Instructions2 = [Instr2 - Comment2 | Instructions3] ->
			peephole__opt_instr(Instr2, Comment2, Procmap, Forkmap,
				TeardownMap, Instructions3, Instructions, _)
		;
			Instructions = Instructions2
		),
		Mod = yes
	;
		Instructions = [Instr0 - Comment0 | Instructions0],
		Mod = no
	).

	% Look for code patterns that can be optimized, and optimize them.

:- pred peephole__match(instr, string, tailmap, tailmap, bimap(label, label),
	list(instruction), list(instruction)).
:- mode peephole__match(in, in, in, in, in, in, out) is semidet.

% peephole__match(block(N, Block0), Comment, Procmap, Forkmap, TeardownMap,
% 		Instrs0, Instrs) :-
% 	peephole__optimize(Block0, Block, Procmap, Forkmap, TeardownMap, _),
% 	Instrs = [block(N, Block) - Comment | Instrs0].

	% A `call' followed by a `proceed' can be replaced with a `tailcall'.
	%
	%					succip = ...
	%					decr_sp(N)
	%	livevals(T1, L1)		livevals(T1, L1)
	%	call(Foo, &&ret);		tailcall(Foo)
	%       <comments, labels>		<comments, labels>
	%	...				...
	%     ret:			=>    ret:
	%       <comments, labels>		<comments, labels>
	%	succip = ...			succip = ...
	%       <comments, labels>		<comments, labels>
	%	decr_sp(N)			decr_sp(N)
	%       <comments, labels>		<comments, labels>
	%	livevals(T2, L2)		livevals(T2, L2)
	%       <comments, labels>		<comments, labels>
	%	proceed				proceed
	%
	% Note that we can't delete the return label and the following
	% code, since the label might be branched to from elsewhere.
	% If it isn't, label elimination will get rid of it later.
	%
	% I have some doubt about the validity of using L1 unchanged.

	% A `call' followed by a `proceed' can be replaced with a `tailcall'.
	%
	%					succip = ...
	%					decr_sp(N)
	%	livevals(T1, L1)		livevals(T1, L1)
	%	call(Foo, &&ret);		tailcall(Foo)
	%       <comments, labels>		<comments, labels>
	%	...				...
	%     ret:			=>    ret:
	%       if_val(not(r1), &&fail)		if_val(not(r1), &&fail)
	%       <comments, labels>		<comments, labels>
	%	succip = ...			succip = ...
	%       <comments, labels>		<comments, labels>
	%	decr_sp(N)			decr_sp(N)
	%       <comments, labels>		<comments, labels>
	%	r1 = TRUE			r1 = TRUE
	%       <comments, labels>		<comments, labels>
	%	livevals(T2, L2)		livevals(T2, L2)
	%       <comments, labels>		<comments, labels>
	%	proceed				proceed
	%       <comments, labels>		<comments, labels>
	%     fail:			      fail:
	%	succip = ...			succip = ...
	%       <comments, labels>		<comments, labels>
	%	decr_sp(N)			decr_sp(N)
	%       <comments, labels>		<comments, labels>
	%	r1 = FALSE			r1 = FALSE
	%       <comments, labels>		<comments, labels>
	%	livevals(T2, L2)		livevals(T2, L2)
	%       <comments, labels>		<comments, labels>
	%	proceed				proceed

peephole__match(livevals(Livevals), Comment, Procmap, Forkmap, _,
		Instrs0, Instrs) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [call(CodeAddress, label(ContLabel), Caller, _LiveVals)
		- Comment2 | _],
	( map__search(Procmap, ContLabel, Between0) ->
		opt_util__filter_out_livevals(Between0, Between1),
		string__append(Comment2, " (redirected return)", Redirect),
		list__append(Between1,
			[livevals(Livevals) - Comment,
			goto(CodeAddress, Caller) - Redirect | Instrs0], Instrs)
	; map__search(Forkmap, ContLabel, Between0) ->
		opt_util__filter_out_livevals(Between0, Between1),
		opt_util__filter_out_r1(Between1, Between2),
		string__append(Comment2, " (redirected return)", Redirect),
		list__append(Between2,
			[livevals(Livevals) - Comment,
			goto(CodeAddress, Caller) - Redirect | Instrs0], Instrs)
	;
		fail
	).

	% A `goto' can be deleted if the target of the jump is the very
	% next instruction:
	%
	%	goto next;
	%	<comments, labels>	=>	<comments, labels>
	%     next:			      next:

peephole__match(goto(label(Label), _Caller), _, _, _, _, Instrs0, Instrs) :-
	opt_util__is_this_label_next(Label, Instrs0, _),
	Instrs = Instrs0.

	% A conditional branch whose condition is constant
	% can either be replaced by an unconditional goto
	% or elimininated
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

peephole__match(if_val(Rval, label(Target)), Comment, _, _, _,
		Instrs0, Instrs) :-
	( opt_util__is_const_condition(Rval, Taken) ->
		(
			Taken = yes,
			% XXX must be fixed before profiling is finished
			Instrs = [goto(label(Target), label(Target)) - Comment
				| Instrs0]
		;
			Taken = no,
			Instrs = Instrs0
		)
	;
		opt_util__skip_comments_livevals(Instrs0, Instrs1),
		( Instrs1 = [goto(Somewhere, _) - C2 | Instrs2] ->
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

peephole__match(mkframe(Descr, Slots, Redoip1), Comment, _, _, _,
		Instrs0, Instrs) :-
	(
		opt_util__next_modframe(Instrs0, [], Redoip2, Skipped, Rest),
		opt_util__touches_nondet_ctrl(Skipped, no)
	->
		list__append(Skipped, Rest, Instrs1),
		Instrs = [mkframe(Descr, Slots, Redoip2) - Comment | Instrs1]
	;
		opt_util__skip_comments_livevals(Instrs0, Instrs1),
		Instrs1 = [Instr1 | Instrs2],
		Instr1 = if_val(Test, Target) - Comment2,
		(
			Redoip1 = do_fail,
			( Target = do_redo ; Target = do_fail)
		->
			Instrs = [if_val(Test, do_redo) - Comment2,
				mkframe(Descr, Slots, do_fail) - Comment | Instrs2]
		;
			Redoip1 = label(_)
		->
			(
				Target = do_fail
			->
				Instrs = [if_val(Test, do_redo) - Comment2,
					mkframe(Descr, Slots, Redoip1) - Comment | Instrs2]
			;
				Target = do_redo
			->
				Instrs = [mkframe(Descr, Slots, Redoip1) - Comment,
					if_val(Test, Redoip1) - Comment2 | Instrs2]
			;
				fail
			)
		;
			fail
		)
	).

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

peephole__match(modframe(Redoip), Comment, _, _, _, Instrs0, Instrs) :-
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
			% XXX must be fixed before profiling is finished
			[goto(do_succeed(yes), do_succeed(yes)) -
			"early discard"], After], Instrs)
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

peephole__match(incr_sp(N), _, _, _, _, Instrs0, Instrs) :-
	opt_util__no_stackvars_til_decr_sp(Instrs0, N, Between, Remain),
	list__append(Between, Remain, Instrs).

	% If an incr_sp follows a decr_sp of the same amount, then the two
	% cancel out. The optimization that creates these opportunities
	% (full jumpopt) should not put any instructions between the incr_sp
	% and the decr_sp, so looking for such would be redundant.
	%
	%	decr_sp N 	=>	<...>
	%	incr_sp N

peephole__match(decr_sp(N), _, _, _, _TeardownMap, Instrs0, Instrs) :-
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
	Instrs1 = [incr_sp(N) - _ | Instrs].
% XXX should be restored when proven OK
% peephole__match(decr_sp(N), _, _, _, TeardownMap, Instrs0, Instrs) :-
% 	peephole_decr(N, TeardownMap, Instrs0, Instrs).

peephole__match(assign(Lval, Rval), Comment, _Procmap, _Forkmap, _TeardownMap,
		Instrs0, Instrs) :-
	Lval = succip,
	Rval = lval(stackvar(N)),
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = decr_sp(N) - _,
	opt_util__skip_comments_livevals(Instrs2, Instrs3),
	Instrs3 = [Instr3 | Instrs4],
	Instr3 = assign(stackvar(0), lval(succip)) - _,
	Instr0 = assign(Lval, Rval) - Comment,
	Instrs = [Instr0, Instr1 | Instrs4].
% XXX should be restored when proven OK
%	peephole__optimize([Instr0, Instr1 | Instrs4], Instrs,
%		Procmap, Forkmap, TeardownMap, _).

:- pred peephole_decr(int, bimap(label, label),
	list(instruction), list(instruction)).
:- mode peephole_decr(in, in, in, out) is semidet.

peephole_decr(N, TeardownMap, Instrs0, Instrs) :-
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = Uinstr1 - Comment1,
	(
		Uinstr1 = incr_sp(N),
		Instrs = Instrs2
	;
		Uinstr1 = if_val(Cond, Addr),
		Addr = label(Label),
		bimap__search(TeardownMap, OrigLabel, Label),
		peephole_decr(N, TeardownMap, Instrs2, TailInstrs),
		string__append(Comment1, " (original)", NewComment),
		NewInstr = if_val(Cond, label(OrigLabel)) - NewComment,
		Instrs = [NewInstr | TailInstrs]
	;
		Uinstr1 = assign(Lval, Rval),
		opt_util__lval_refers_stackvars(Lval, no),
		opt_util__rval_refers_stackvars(Rval, no),
		peephole_decr(N, TeardownMap, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	;
		Uinstr1 = incr_hp(Lval, _Tag, Rval),
		opt_util__lval_refers_stackvars(Lval, no),
		opt_util__rval_refers_stackvars(Rval, no),
		peephole_decr(N, TeardownMap, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	;
		Uinstr1 = comment(_),
		peephole_decr(N, TeardownMap, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	;
		Uinstr1 = livevals(_),
		peephole_decr(N, TeardownMap, Instrs2, TailInstrs),
		Instrs = [Instr1 | TailInstrs]
	).
