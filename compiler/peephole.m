%-----------------------------------------------------------------------------%

% peephole.nl - local LLDS to LLDS optimizations based on pattern-matching.

% Authors: fjh and zs.

%-----------------------------------------------------------------------------%

:- module peephole.

:- interface.

:- import_module list, llds.

:- pred peephole__main(list(instruction), list(instruction), bool).
:- mode peephole__main(in, out, out) is det.

:- implementation.

:- import_module code_util, opt_util, map, string, std_util.

peephole__main(Instrs0, Instrs, Mod) :-
	map__init(Procmap0),
	map__init(Succmap0),
	peephole__build_procmaps(Instrs0, Procmap0, Procmap,
		Succmap0, Succmap),
	map__init(Forkmap0),
	peephole__build_forkmap(Instrs0, Succmap, Forkmap0, Forkmap),
	peephole__optimize(Instrs0, Instrs, Procmap, Forkmap, Mod).

	% Build two maps, one for deterministic proceeds and one for
	% semideterministic proceeds. If a label is followed by code
	% that falls into one of these categories, it is entered into
	% the relevant map, with the target being the following code,
	% and for semideterministic proceeds, a success/failure indication.

:- pred peephole__build_procmaps(list(instruction), tailmap, tailmap,
	succmap, succmap).
:- mode peephole__build_procmaps(in, di, uo, di, uo) is det.

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
:- mode peephole__build_forkmap(in, in, di, uo) is det.

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
	tailmap, tailmap, bool).
:- mode peephole__optimize(in, out, in, in, out) is det.

peephole__optimize([], [], _, _, no).
peephole__optimize([Instr0 - Comment|Instructions0], Instructions,
		Procmap, Forkmap, Mod) :-
	peephole__optimize(Instructions0, Instructions1,
		Procmap, Forkmap, Mod0),
	peephole__opt_instr(Instr0, Comment, Procmap, Forkmap,
		Instructions1, Instructions, Mod1),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

	% Try to optimize the beginning of the given instruction sequence.
	% If successful, try it again.

:- pred peephole__opt_instr(instr, string, tailmap, tailmap,
	list(instruction), list(instruction), bool).
:- mode peephole__opt_instr(in, in, in, in, in, out, out) is det.

peephole__opt_instr(Instr0, Comment0, Procmap, Forkmap,
		Instructions0, Instructions, Mod) :-
	(
		opt_util__skip_comments(Instructions0, Instructions1),
		peephole__match(Instr0, Comment0, Procmap, Forkmap,
			Instructions1, Instructions2)
	->
		( Instructions2 = [Instr2 - Comment2 | Instructions3] ->
			peephole__opt_instr(Instr2, Comment2, Procmap, Forkmap,
				Instructions3, Instructions, _)
		;
			Instructions = Instructions2
		),
		Mod = yes
	;
		Instructions = [Instr0 - Comment0 | Instructions0],
		Mod = no
	).

	% Look for code patterns that can be optimized, and optimize them.

:- pred peephole__match(instr, string, tailmap, tailmap,
	list(instruction), list(instruction)).
:- mode peephole__match(in, in, in, in, in, out) is semidet.

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

peephole__match(livevals(Livevals), Comment, Procmap, Forkmap,
		Instrs0, Instrs) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [call(CodeAddress, label(ContLabel), _Caller, _LiveVals)
							- Comment2 | _],
	( map__search(Procmap, ContLabel, Between0) ->
		opt_util__filter_out_livevals(Between0, Between1),
		string__append(Comment2, " (redirected return)", Redirect),
		list__append(Between1,
			[livevals(Livevals) - Comment,
			goto(CodeAddress) - Redirect | Instrs0], Instrs)
	; map__search(Forkmap, ContLabel, Between0) ->
		opt_util__filter_out_livevals(Between0, Between1),
		opt_util__filter_out_r1(Between1, Between2),
		string__append(Comment2, " (redirected return)", Redirect),
		list__append(Between2,
			[livevals(Livevals) - Comment,
			goto(CodeAddress) - Redirect | Instrs0], Instrs)
	;
		fail
	).

	% A `goto' can be deleted if the target of the jump is the very
	% next instruction:
	%
	%	goto next;
	%	<comments, labels>	=>	<comments, labels>
	%     next:			      next:

peephole__match(goto(label(Label)), _Comment, _Procmap, _Forkmap,
		Instrs0, Instrs) :-
	opt_util__is_this_label_next(Label, Instrs0, _),
	Instrs = Instrs0.

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

peephole__match(if_val(Rval, label(Target)), _C1, _Procmap, _Forkmap,
		Instrs0, Instrs) :-
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
	( Instrs1 = [goto(Somewhere) - C2 | Instrs2] ->
		opt_util__is_this_label_next(Target, Instrs2, _),
		code_util__neg_rval(Rval, NotRval),
		Instrs = [if_val(NotRval, Somewhere) - C2 | Instrs2]
	;
		opt_util__is_this_label_next(Target, Instrs1, _),
		Instrs = Instrs0
	).

	% If a `mkframe' is followed by a `modframe', with the instructions
	% in between containing only straight-line code, we can delete the
	% `modframe' and instead just set the redoip directly in the `mkframe'.
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

peephole__match(mkframe(Descr, Slots, Redoip1), Comment,
		_Procmap, _Forkmap, Instrs0, Instrs) :-
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

peephole__match(modframe(_), Comment,
		_Procmap, _Forkmap, Instrs0, Instrs) :-
	opt_util__next_modframe(Instrs0, [], Redoip2, Skipped, Rest),
	opt_util__touches_nondet_ctrl(Skipped, no),
	list__append(Skipped, Rest, Instrs1),
	Instrs = [modframe(Redoip2) - Comment | Instrs1].

	% If a decr_sp immediately follows an incr_sp of the same amount,
	% the two cancel out.
	%
	%	incr_sp N
	%	decr_sp N	=>	(nothing)

peephole__match(incr_sp(N), _Comment, _Procmap, _Forkmap, Instrs0, Instrs) :-
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
	Instrs1 = [decr_sp(N) - _Comment2 | Instrs].
