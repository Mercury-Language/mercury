%-----------------------------------------------------------------------------%

% Peephole.nl - LLDS to LLDS peephole optimization.

% Main authors: zs, fjh.

%-----------------------------------------------------------------------------%

:- module peephole.
:- interface.
:- import_module map, llds, options.

:- pred peephole__optimize(option_table, c_file, c_file).
:- mode peephole__optimize(in, in, out) is det.

% the types are exported only for debugging.

:- type instmap == map(label, instruction).
:- type tailmap == map(label, list(instruction)).
:- type redoipmap == map(label, list(maybe(code_addr))).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module value_number, opt_debug, opt_util, code_util, map.
:- import_module bintree_set, string, list, require, int, std_util.

%-----------------------------------------------------------------------------%

	% Boring LLDS traversal code.

peephole__optimize(Options, c_file(Name, Modules0), c_file(Name, Modules)) :-
	peephole__opt_module_list(Options, Modules0, Modules).

:- pred peephole__opt_module_list(option_table, list(c_module), list(c_module)).
:- mode peephole__opt_module_list(in, in, out) is det.

peephole__opt_module_list(_Options, [], []).
peephole__opt_module_list(Options, [M0|Ms0], [M|Ms]) :-
	peephole__opt_module(Options, M0, M),
	peephole__opt_module_list(Options, Ms0, Ms).

:- pred peephole__opt_module(option_table, c_module, c_module).
:- mode peephole__opt_module(in, in, out) is det.

peephole__opt_module(Options, c_module(Name, Procs0), c_module(Name, Procs)) :-
	peephole__opt_proc_list(Options, Procs0, Procs).

:- pred peephole__opt_proc_list(option_table,
				list(c_procedure), list(c_procedure)).
:- mode peephole__opt_proc_list(in, in, out) is det.

peephole__opt_proc_list(_Options, [], []).
peephole__opt_proc_list(Options, [P0|Ps0], [P|Ps]) :-
	peephole__opt_proc(Options, P0, P),
	peephole__opt_proc_list(Options, Ps0, Ps).

	% We short-circuit jump sequences before normal peepholing
	% to create more opportunities for use of the tailcall macro.

:- pred peephole__opt_proc(option_table, c_procedure, c_procedure).
:- mode peephole__opt_proc(in, in, out) is det.

peephole__opt_proc(Options, c_procedure(Name, Arity, Mode, Instructions0),
		   c_procedure(Name, Arity, Mode, Instructions)) :-
	peephole__repeat_opts(Options, 0, no, Instructions0, Instructions1),
	peephole__nonrepeat_opts(Options, Instructions1, Instructions).

:- pred peephole__repeat_opts(option_table, int, bool, list(instruction),
	list(instruction)).
:- mode peephole__repeat_opts(in, in, in, in, out) is det.

peephole__repeat_opts(Options, Iter, DoneValueNumber,
		Instructions0, Instructions) :-
	options__lookup_int_option(Options, peephole_repeat, Repeat),
	( Iter < Repeat ->
		options__lookup_bool_option(Options, peephole_jump_opt, Jumpopt),
		( Jumpopt = yes ->
			% write('short circuit'), nl,
			peephole__short_circuit(Instructions0, Instructions1, Mod1)
		;
			Instructions1 = Instructions0,
			Mod1 = no
		),
		options__lookup_bool_option(Options, peephole_local, Local),
		( Local = yes ->
			% write('local'), nl,
			peephole__local_opt(Instructions1, Instructions2, Mod2)
		;
			Instructions2 = Instructions1,
			Mod2 = no
		),
		options__lookup_bool_option(Options, peephole_label_elim, LabelElim),
		( LabelElim = yes ->
			% write('label elim'), nl,
			peephole__label_elim(Instructions2, Instructions3, Mod3)
		;
			Instructions3 = Instructions2,
			Mod3 = no
		),
		options__lookup_bool_option(Options, peephole_opt_redoip, OptRedoip),
		( OptRedoip = yes ->
			% write('redoip'), nl,
			peephole__opt_redoip(Instructions3, Instructions4, Mod4)
		;
			Instructions4 = Instructions3,
			Mod4 = no
		),
		options__lookup_bool_option(Options, peephole_value_number, ValueNumber),
		( ValueNumber = yes, DoneValueNumber = no ->
			% write('value number'), nl,
			value_number__optimize(Instructions4, Instructions5)
		;
			Instructions5 = Instructions4
		),
		% value_number does not open up possibilities for other opts
		% and is not profitable to repeat again
		( Mod1 = no, Mod2 = no, Mod3 = no, Mod4 = no ->
			Instructions = Instructions5
		;
			Iter1 is Iter + 1,
			peephole__repeat_opts(Options, Iter1, ValueNumber,
				Instructions5, Instructions)
		)
	;
		Instructions = Instructions0
	).

:- pred peephole__nonrepeat_opts(option_table,
	list(instruction), list(instruction)).
:- mode peephole__nonrepeat_opts(in, in, out) is det.

peephole__nonrepeat_opts(Options, Instructions0, Instructions) :-
	options__lookup_bool_option(Options, peephole_frame_opt, FrameOpt),
	( FrameOpt = yes ->
		peephole__frame_opt(Instructions0, Instructions, _Mod)
	;
		Instructions = Instructions0
	).

%-----------------------------------------------------------------------------%

	% Build up a table showing the first instruction following each label.
	% Then traverse the instruction list, short-circuiting jump sequences.

:- pred peephole__short_circuit(list(instruction), list(instruction), bool).
:- mode peephole__short_circuit(in, out, out) is det.

peephole__short_circuit(Instrs0, Instrs, Mod) :-
	map__init(Instmap0),
	map__init(Procmap0),
	map__init(Sdprocmap0),
	map__init(Succmap0),
	peephole__jumpopt_build_maps(Instrs0, Instmap0, Instmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap),
	% opt_debug__print_instmap(Instmap),
	% opt_debug__print_tailmap(Procmap),
	% opt_debug__print_tailmap(Sdprocmap),
	% opt_debug__print_tailmap(Succmap),
	peephole__jumpopt_instr_list(Instrs0, comment(""),
		Instmap, Procmap, Sdprocmap, Succmap, Instrs, Mod).

:- pred peephole__jumpopt_build_maps(list(instruction), instmap, instmap,
	tailmap, tailmap, tailmap, tailmap, tailmap, tailmap).
:- mode peephole__jumpopt_build_maps(in, di, uo, di, uo, di, uo, di, uo) is det.

peephole__jumpopt_build_maps([], Instmap, Instmap, Procmap, Procmap,
	Sdprocmap, Sdprocmap, Succmap, Succmap).
peephole__jumpopt_build_maps([Instr - _Comment|Instrs], Instmap0, Instmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap) :-
	( Instr = label(Label) ->
		opt_util__skip_comments_livevals(Instrs, Instrs1),
		( Instrs1 = [Nextinstr | _] ->
			map__set(Instmap0, Label, Nextinstr, Instmap1)
		;
			Instmap1 = Instmap0
		),
		( opt_util__is_proceed_next(Instrs, Between1) ->
			map__set(Procmap0, Label, Between1, Procmap1)
		;
			Procmap1 = Procmap0
		),
		( opt_util__is_sdproceed_next(Instrs, Between2) ->
			map__set(Sdprocmap0, Label, Between2, Sdprocmap1)
		;
			Sdprocmap1 = Sdprocmap0
		),
		( opt_util__is_succeed_next(Instrs, Between3) ->
			map__set(Succmap0, Label, Between3, Succmap1)
		;
			Succmap1 = Succmap0
		)
	;
		Instmap1 = Instmap0,
		Procmap1 = Procmap0,
		Sdprocmap1 = Sdprocmap0,
		Succmap1 = Succmap0
	),
	peephole__jumpopt_build_maps(Instrs, Instmap1, Instmap,
		Procmap1, Procmap, Sdprocmap1, Sdprocmap, Succmap1, Succmap).

:- pred peephole__jumpopt_instr_list(list(instruction), instr,
	instmap, tailmap, tailmap, tailmap, list(instruction), bool).
:- mode peephole__jumpopt_instr_list(in, in, in, in, in, in, out, out) is det.

peephole__jumpopt_instr_list([], _Previnstr,
		_Instmap, _Procmap, _Sdprocmap, _Succmap, [], no).
peephole__jumpopt_instr_list([Instr0|Moreinstrs0], Previnstr,
		Instmap, Procmap, Sdprocmap, Succmap, Instrs, Mod) :-
	Instr0 = Uinstr0 - Comment0,
	string__append(Comment0, " (redirected return)", Redirect),
	(
		Uinstr0 = call(Proc, label(Retlabel)),
		map__search(Instmap, Retlabel, Retinstr)
	->
		peephole__jumpopt_final_dest(Retlabel, Retinstr,
			Instmap, Destlabel, Destinstr),
		( Retlabel = Destlabel ->
			Newinstrs = [Instr0],
			Mod0 = no
		;
			Newinstrs = [call(Proc, label(Destlabel)) - Redirect],
			Mod0 = yes
		)
	; Uinstr0 = goto(label(Targetlabel)) ->
		( Moreinstrs0 = [label(Targetlabel) - _|_] ->
			% eliminating the goto (by the local peephole pass)
			% is better than shortcircuiting it here
			Newinstrs = [Instr0],
			Mod0 = no
		; Previnstr = if_val(_, label(Iftargetlabel)),
		  Moreinstrs0 = [label(Iftargetlabel) - _|_] ->
			% eliminating the goto (by the local peephole pass)
			% is better than shortcircuiting it here
			% PROVIDED the test will succeed most of the time
			% we could use profiling feedback here XXX
			Newinstrs = [Instr0],
			Mod0 = no
		; map__search(Procmap, Targetlabel, Between) ->
			list__append(Between, [goto(succip) - "shortcircuit"],
				Newinstrs),
			Mod0 = yes
		; map__search(Sdprocmap, Targetlabel, Between) ->
			list__append(Between, [goto(succip) - "shortcircuit"],
				Newinstrs),
			Mod0 = yes
		; map__search(Succmap, Targetlabel, Between) ->
			list__append(Between, [goto(do_succeed) - "shortcircuit"],
				Newinstrs),
			Mod0 = yes
		; map__search(Instmap, Targetlabel, Targetinstr) ->
			peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
				Instmap, Destlabel, Destinstr),
			Destinstr = Udestinstr - _Destcomment,
			string__append("shortcircuited jump: ",
				Comment0, Shorted),
			opt_util__can_instr_fall_through(Udestinstr,
				Canfallthrough),
			( Canfallthrough = no ->
				Newinstrs = [Udestinstr - Shorted],
				Mod0 = yes
			;
				( Targetlabel = Destlabel ->
					Newinstrs = [Instr0],
					Mod0 = no
				;
					Newinstrs = [goto(label(Destlabel))
							- Shorted],
					Mod0 = yes
				)
			)
		;
			error("target label not in instmap")
			% Newinstrs = [Instr0],
			% Mod0 = no
		)
	; Uinstr0 = computed_goto(Index, LabelList0) ->
		peephole__short_labels(LabelList0, Instmap, LabelList, Mod0),
		( Mod0 = yes ->
			string__append(Comment0, " (some shortciruits)",
				Shorted),
			Newinstrs = [computed_goto(Index, LabelList) - Shorted]
		;
			Newinstrs = [Instr0]
		)
	;
		Newinstrs = [Instr0],
		Mod0 = no
	),
	( ( Uinstr0 = comment(_) ; Uinstr0 = livevals(_, _) ) ->
		Newprevinstr = Previnstr
	;
		Newprevinstr = Uinstr0
	),
	peephole__jumpopt_instr_list(Moreinstrs0, Newprevinstr,
		Instmap, Procmap, Sdprocmap, Succmap, Moreinstrs, Mod1),
	list__append(Newinstrs, Moreinstrs, Instrs),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred peephole__short_labels(list(label), instmap, list(label), bool).
:- mode peephole__short_labels(in, in, out, out) is det.

% XXX these uses of the Mod argument should be replaced by accumulator passing

peephole__short_labels([], _Instmap, [], no).
peephole__short_labels([Label0 | Labels0], Instmap, [Label | Labels], Mod) :-
	peephole__short_label(Label0, Instmap, Label, Mod1),
	peephole__short_labels(Labels0, Instmap, Labels, Mod2),
	( Mod1 = no, Mod2 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred peephole__short_label(label, instmap, label, bool).
:- mode peephole__short_label(in, in, out, out) is det.

peephole__short_label(Label0, Instmap, Label, Mod) :-
	( map__search(Instmap, Label0, Instr0) ->
		peephole__jumpopt_final_dest(Label0, Instr0, Instmap,
			Label, _Instr),
		( Label = Label0 ->
			Mod = no
		;
			Mod = yes
		)
	;
		error("target label not in instmap")
	).

:- pred peephole__jumpopt_final_dest(label, instruction, instmap,
	label, instruction).
:- mode peephole__jumpopt_final_dest(in, in, in, out, out) is det.

	% Currently we don't check for infinite loops.  This is OK at
	% the moment since the compiler never generates code containing
	% infinite loops, but it may cause problems in the future.

peephole__jumpopt_final_dest(Srclabel, Srcinstr, Instmap,
		Destlabel, Destinstr) :-
	(
		Srcinstr = goto(label(Targetlabel)) - Comment,
		map__search(Instmap, Targetlabel, Targetinstr)
	->
		% write('goto short-circuit from '),
		% write(Srclabel),
		% write(' to '),
		% write(Targetlabel),
		% nl,
		peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
			Instmap, Destlabel, Destinstr)
	;
		Srcinstr = label(Targetlabel) - Comment,
		map__search(Instmap, Targetlabel, Targetinstr)
	->
		% write('fallthrough short-circuit from '),
		% write(Srclabel),
		% write(' to '),
		% write(Targetlabel),
		% nl,
		peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
			Instmap, Destlabel, Destinstr)
	;
		Destlabel = Srclabel,
		Destinstr = Srcinstr
	).

%-----------------------------------------------------------------------------%

	% We zip down to the end of the instruction list, and start attempting
	% to optimize instruction sequences.  As long as we can continue
	% optimizing the instruction sequence, we keep doing so;
	% when we find a sequence we can't optimize, we back up try
	% so optimize the sequence starting with the previous instruction.

:- pred peephole__local_opt(list(instruction), list(instruction), bool).
:- mode peephole__local_opt(in, out, out) is det.

peephole__local_opt(Instrs0, Instrs, Mod) :-
	map__init(Procmap0),
	peephole__local_build_maps(Instrs0, Procmap0, Procmap),
	peephole__local_opt_2(Instrs0, Instrs, Procmap, Mod).

:- pred peephole__local_build_maps(list(instruction), tailmap, tailmap).
:- mode peephole__local_build_maps(in, di, uo) is det.

peephole__local_build_maps([], Procmap, Procmap).
peephole__local_build_maps([Instr - _Comment|Instrs], Procmap0, Procmap) :-
	( Instr = label(Label) ->
		( opt_util__is_proceed_next(Instrs, Between) ->
			map__set(Procmap0, Label, Between, Procmap1)
		;
			Procmap1 = Procmap0
		)
	;
		Procmap1 = Procmap0
	),
	peephole__local_build_maps(Instrs, Procmap1, Procmap).

:- pred peephole__local_opt_2(list(instruction), list(instruction),
	tailmap, bool).
:- mode peephole__local_opt_2(in, out, in, out) is det.

peephole__local_opt_2([], [], _, no).
peephole__local_opt_2([Instr0 - Comment|Instructions0], Instructions,
		Procmap, Mod) :-
	peephole__local_opt_2(Instructions0, Instructions1, Procmap, Mod0),
	peephole__opt_instr(Instr0, Comment, Procmap,
		Instructions1, Instructions, Mod1),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred peephole__opt_instr(instr, string, tailmap,
		list(instruction), list(instruction), bool).
:- mode peephole__opt_instr(in, in, in, in, out, out) is det.

peephole__opt_instr(Instr0, Comment0, Procmap, Instructions0, Instructions,
		Mod) :-
	(
		opt_util__skip_comments(Instructions0, Instructions1),
		peephole__opt_instr_2(Instr0, Comment0, Procmap, Instructions1,
			Instructions2)
	->
		( Instructions2 = [Instr2 - Comment2 | Instructions3] ->
			peephole__opt_instr(Instr2, Comment2, Procmap,
				Instructions3, Instructions, _)
		;
			Instructions = Instructions2
		),
		Mod = yes
	;
		Instructions = [Instr0 - Comment0 | Instructions0],
		Mod = no
	).

:- pred peephole__opt_instr_2(instr, string, tailmap,
		list(instruction), list(instruction)).
:- mode peephole__opt_instr_2(in, in, in, in, out) is semidet.

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

peephole__opt_instr_2(livevals(yes, Livevals), Comment, Procmap,
		Instrs0, Instrs) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [call(CodeAddress, label(ContLabel)) - Comment2 | _],
	map__search(Procmap, ContLabel, Instrs_to_proceed),
	opt_util__filter_out_livevals(Instrs_to_proceed, Instrs_to_insert),
	string__append(Comment, " (redirected return)", Redirect),
	list__append(Instrs_to_insert,
		[livevals(yes, Livevals) - Comment2,
		goto(CodeAddress) - Redirect | Instrs0], Instrs).

	% a `goto' can be deleted if the target of the jump is the very
	% next instruction.
	%
	%	goto next;	=>	  <comments, labels>
	%	<comments, labels>	next:
	%     next:
	%
	% dead code after a `goto' is deleted in label-elim.

peephole__opt_instr_2(goto(label(Label)), _Comment, _Procmap,
		Instrs0, Instrs) :-
	opt_util__is_this_label_next(Label, Instrs0, _),
	Instrs = Instrs0.

	% a conditional branch over a branch can be replaced
	% by an inverse conditional branch
	%
	%	if (x) goto skip;		if (!x) goto somewhere
	%	<comments>			omit <comments>
	%	goto somewhere;		=>	<comments, labels>
	%	<comments, labels>	      skip:
	%     skip:
	%
	% a conditional branch to the very next instruction
	% can be deleted
	%	if (x) goto next;	=>	<comments, labels>
	%	<comments, labels>	      next:
	%     next:

peephole__opt_instr_2(if_val(Rval, label(Target)), _C1, _Procmap,
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

	% if a `mkframe' is followed by a `modframe', with the instructions
	% in between containing only straight-line code, we can delete the
	% `modframe' and instead just set the redoip directly in the `mkframe'.
	%
	%	mkframe(D, S, _)	=>	mkframe(D, S, Redoip)
	%	<straightline instrs>		<straightline instrs>
	%	modframe(Redoip)
	%
	% if a `mkframe' is followed by a test that can fail, we try to
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
	% these two patterns are mutually exclusive because if_val is not
	% straigh-line code.

peephole__opt_instr_2(mkframe(Descr, Slots, Redoip), Comment, _Procmap,
		Instrs0, Instrs) :-
	( opt_util__next_modframe(Instrs0, [], Newredoip, Skipped, Rest) ->
		list__append(Skipped, Rest, Instrs1),
		Instrs = [mkframe(Descr, Slots, Newredoip) - Comment | Instrs1]
	;
		opt_util__skip_comments_livevals(Instrs0, Instrs1),
		Instrs1 = [Instr1 | Instrs2],
		Instr1 = if_val(Test, Target) - Comment2,
		(
			Redoip = do_fail,
			( Target = do_redo ; Target = do_fail)
		->
			Instrs = [if_val(Test, do_redo) - Comment2,
				mkframe(Descr, Slots, do_fail) - Comment | Instrs2]
		;
			Redoip = label(_)
		->
			(
				Target = do_fail
			->
				Instrs = [if_val(Test, do_redo) - Comment2,
					mkframe(Descr, Slots, Redoip) - Comment | Instrs2]
			;
				Target = do_redo
			->
				Instrs = [mkframe(Descr, Slots, Redoip) - Comment,
					if_val(Test, Redoip) - Comment2 | Instrs2]
			;
				fail
			)
		;
			fail
		)
	).

%-----------------------------------------------------------------------------%

	% Build up a table showing which labels are branched to.
	% Then traverse the instruction list removing unnecessary labels.
	% If the instruction before the label branches away, we also
	% remove the instruction block following the label.

:- type usemap == bintree_set(label).

:- pred peephole__label_elim(list(instruction), list(instruction), bool).
:- mode peephole__label_elim(in, out, out) is det.

peephole__label_elim(Instructions0, Instructions, Mod) :-
	bintree_set__init(Usemap0),
	peephole__label_elim_build_usemap(Instructions0, Usemap0, Usemap),
	peephole__label_elim_instr_list(Instructions0, Usemap,
		Instructions, Mod).

:- pred peephole__label_elim_build_usemap(list(instruction), usemap, usemap).
:- mode peephole__label_elim_build_usemap(in, di, uo) is det.

peephole__label_elim_build_usemap([], Usemap, Usemap).
peephole__label_elim_build_usemap([Instr | Instructions], Usemap0, Usemap) :-
	Instr = Uinstr - _Comment,
	% format("looking at instr ~w", [Instr]), nl,
	opt_util__instr_labels(Uinstr, Labels, CodeAddresses),
	peephole__label_list_build_usemap(Labels, Usemap0, Usemap1),
	peephole__code_addr_list_build_usemap(CodeAddresses, Usemap1, Usemap2),
	peephole__label_elim_build_usemap(Instructions, Usemap2, Usemap).

:- pred peephole__code_addr_list_build_usemap(list(code_addr), usemap, usemap).
:- mode peephole__code_addr_list_build_usemap(in, di, uo) is det.

peephole__code_addr_list_build_usemap([], Usemap, Usemap).
peephole__code_addr_list_build_usemap([Code_addr | Rest], Usemap0, Usemap) :-
	( Code_addr = label(Label) ->
		bintree_set__insert(Usemap0, Label, Usemap1)
	;
		Usemap1 = Usemap0
	),
	peephole__code_addr_list_build_usemap(Rest, Usemap1, Usemap).

:- pred peephole__label_list_build_usemap(list(label), usemap, usemap).
:- mode peephole__label_list_build_usemap(in, di, uo) is det.

peephole__label_list_build_usemap([], Usemap, Usemap).
peephole__label_list_build_usemap([Label | Labels], Usemap0, Usemap) :-
	bintree_set__insert(Usemap0, Label, Usemap1),
	peephole__label_list_build_usemap(Labels, Usemap1, Usemap).

:- pred peephole__label_elim_instr_list(list(instruction),
	usemap, list(instruction), bool).
:- mode peephole__label_elim_instr_list(in, in, out, out) is det.

peephole__label_elim_instr_list(Instrs0, Usemap, Instrs, Mod) :-
	peephole__label_elim_instr_list(Instrs0, yes, Usemap, Instrs, Mod).

:- pred peephole__label_elim_instr_list(list(instruction),
	bool, usemap, list(instruction), bool).
:- mode peephole__label_elim_instr_list(in, in, in, out, out) is det.

peephole__label_elim_instr_list([], _Fallthrough, _Usemap, [], no).
peephole__label_elim_instr_list([Instr0 | Moreinstrs0],
		Fallthrough, Usemap, [Instr | Moreinstrs], Mod) :-
	( Instr0 = label(Label) - Comment ->
		(
		    (   Label = exported(_)
		    ;	Label = local(_)
		    ;   bintree_set__is_member(Label, Usemap)
		    )
		->
			Instr = Instr0,
			Fallthrough1 = yes,
			Mod0 = no
		;
			peephole__eliminate(Instr0, yes(Fallthrough), Instr,
				Mod0),
			Fallthrough1 = Fallthrough
		)
	;
		( Fallthrough = yes ->
			Instr = Instr0,
			Mod0 = no
		;
			peephole__eliminate(Instr0, no, Instr, Mod0)
		),
		Instr0 = Uinstr0 - Comment,
		opt_util__can_instr_fall_through(Uinstr0, Canfallthrough),
		( Canfallthrough = yes ->
			Fallthrough1 = Fallthrough
		;
			Fallthrough1 = no
		)
	),
	peephole__label_elim_instr_list(Moreinstrs0, Fallthrough1, Usemap,
				Moreinstrs, Mod1),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred peephole__eliminate(instruction, maybe(bool), instruction, bool).
:- mode peephole__eliminate(in, in, out, out) is det.

peephole__eliminate(Uinstr0 - Comment0, Label, Uinstr - Comment, Mod) :-
	( Uinstr0 = comment(_) ->
		Comment = Comment0,
		Uinstr = Uinstr0,
		Mod = no
	;
		(
			Label = yes(Follow)
		->
			(
				Follow = yes
			->
				Uinstr = comment("eliminated label only")
			;
				% Follow = no,
				Uinstr = comment("eliminated label and block")
			)
		;
			% Label = no,
			Uinstr = comment("eliminated instruction")
		),
		Comment = Comment0,
		Mod = yes
	).

%-----------------------------------------------------------------------------%

	% Turn goto(do_redo) into goto(label(...)) or goto(do_fail).
	% After this optimize away redundant modframes.

:- pred peephole__opt_redoip(list(instruction), list(instruction), bool).
:- mode peephole__opt_redoip(in, out, out) is det.

peephole__opt_redoip(Instrs0, Instrs, Mod) :-
	map__init(Redoipmap0),
	peephole__build_redoip_map(Instrs0, no, Redoipmap0, Redoipmap),
	% opt_debug__print_redoipmap(Redoipmap),
	peephole__do_redoip_opt(Instrs0, Instrs, no, Redoipmap, no, Mod).

:- pred peephole__build_redoip_map(list(instruction), maybe(code_addr),
	redoipmap, redoipmap).
:- mode peephole__build_redoip_map(in, in, di, uo) is det.

peephole__build_redoip_map([], _, Redoipmap, Redoipmap).
peephole__build_redoip_map([Instr | Instrs], Curredoip,
		Redoipmap0, Redoipmap) :-
	Instr = Uinstr - _,
	( Uinstr = mkframe(_, _, Newredoip) ->
		Newcurredoip = yes(Newredoip),
		Redoipmap1 = Redoipmap0
	; Uinstr = modframe(Newredoip) ->
		Newcurredoip = yes(Newredoip),
		Redoipmap1 = Redoipmap0
	; Uinstr = label(Label) ->
		Newcurredoip = Curredoip,
		peephole__add_to_redoip_map(Redoipmap0, Label, Curredoip,
			Redoipmap1)
	; Uinstr = goto(label(Label)) ->
		Newcurredoip = Curredoip,
		peephole__add_to_redoip_map(Redoipmap0, Label, Curredoip,
			Redoipmap1)
	; Uinstr = computed_goto(_, Labels) ->
		Newcurredoip = Curredoip,
		peephole__add_list_to_redoip_map(Redoipmap0, Labels, Curredoip,
			Redoipmap1)
	; Uinstr = if_val(_, label(Label)) ->
		Newcurredoip = Curredoip,
		peephole__add_to_redoip_map(Redoipmap0, Label, Curredoip,
			Redoipmap1)
	; Uinstr = call(_, label(Label)) ->
		Newcurredoip = Curredoip,
		peephole__add_to_redoip_map(Redoipmap0, Label, Curredoip,
			Redoipmap1)
	;
		Newcurredoip = Curredoip,
		Redoipmap1 = Redoipmap0
	),
	peephole__build_redoip_map(Instrs, Newcurredoip, Redoipmap1, Redoipmap).

:- pred peephole__add_to_redoip_map(redoipmap, label,
	maybe(code_addr), redoipmap).
:- mode peephole__add_to_redoip_map(di, in, in, uo) is det.

peephole__add_to_redoip_map(Redoipmap0, Label, Maybe_redoip, Redoipmap) :-
	( map__search(Redoipmap0, Label, Curlist) ->
		( list__member(Maybe_redoip, Curlist) ->
			Redoipmap = Redoipmap0
		;
			map__set(Redoipmap0, Label, [Maybe_redoip | Curlist],
				Redoipmap)
		)
	;
		map__set(Redoipmap0, Label, [Maybe_redoip], Redoipmap)
	).

:- pred peephole__add_list_to_redoip_map(redoipmap, list(label),
	maybe(code_addr), redoipmap).
:- mode peephole__add_list_to_redoip_map(di, in, in, uo) is det.

peephole__add_list_to_redoip_map(Redoipmap, [], _Maybe_redoip, Redoipmap).
peephole__add_list_to_redoip_map(Redoipmap0, [Label | Labels], Maybe_redoip,
		Redoipmap) :-
	peephole__add_to_redoip_map(Redoipmap0, Label, Maybe_redoip,
		Redoipmap1),
	peephole__add_list_to_redoip_map(Redoipmap1, Labels, Maybe_redoip,
		Redoipmap).

:- pred peephole__do_redoip_opt(list(instruction), list(instruction),
	maybe(code_addr), redoipmap, bool, bool).
:- mode peephole__do_redoip_opt(di, uo, in, in, in, out) is det.

peephole__do_redoip_opt([], [], _Curredoip, _Redoipmap, Mod, Mod).
peephole__do_redoip_opt([Instr0 | Instrs0], [Instr | Instrs],
		Curredoip, Redoipmap, Mod0, Mod) :-
	Instr0 = Uinstr0 - Comment,
	( Uinstr0 = mkframe(_, _, Newredoip) ->
		Newcurredoip = yes(Newredoip),
		Instr = Instr0,
		Mod1 = Mod0
	; Uinstr0 = modframe(Newredoip) ->
		Newcurredoip = yes(Newredoip),
		Instr = Instr0,
		Mod1 = Mod0
	; Uinstr0 = label(Label) ->
		( map__search(Redoipmap, Label, Redoips) ->
			( Redoips = [yes(label(Realredoip))] ->
				Newcurredoip = yes(label(Realredoip))
			;
				Newcurredoip = no
			)
		;
			Newcurredoip = no
		),
		Instr = Instr0,
		Mod1 = Mod0
	; Uinstr0 = call(Proc, Target) ->
		peephole__replace_curredoip(Target, Curredoip, Newtarget,
			Mod0, Mod1),
		Instr = call(Proc, Newtarget) - Comment,
		Newcurredoip = Curredoip
	; Uinstr0 = goto(Target) ->
		peephole__replace_curredoip(Target, Curredoip, Newtarget,
			Mod0, Mod1),
		Instr = goto(Newtarget) - Comment,
		Newcurredoip = Curredoip
	; Uinstr0 = if_val(Test, Target) ->
		peephole__replace_curredoip(Target, Curredoip, Newtarget,
			Mod0, Mod1),
		Instr = if_val(Test, Newtarget) - Comment,
		Newcurredoip = Curredoip
	;
		Instr = Instr0,
		Newcurredoip = Curredoip,
		Mod1 = Mod0
	),
	peephole__do_redoip_opt(Instrs0, Instrs, Newcurredoip, Redoipmap,
		Mod1, Mod).

:- pred peephole__replace_curredoip(code_addr, maybe(code_addr), code_addr,
	bool, bool).
:- mode peephole__replace_curredoip(in, in, out, in, out) is det.

peephole__replace_curredoip(Target, Curredoip, Newtarget, Mod0, Mod) :-
	(
		Target = do_redo,
		Curredoip = yes(Redoip)		% could be label or do_fail
	->
		Newtarget = Redoip,
		Mod = yes
	;
		Newtarget = Target,
		Mod = Mod0
	).

%-----------------------------------------------------------------------------%

	% Turn the code into a list of order-independent <label, block> pairs.
	% For each block find out whether it needs a stack frame. If not,
	% try to delay the construction until after the jump to that block,
	% and remove the frame building and removing code.

:- pred peephole__frame_opt(list(instruction), list(instruction), bool).
:- mode peephole__frame_opt(in, out, out) is det.

peephole__frame_opt(Instrs0, Instrs, Mod) :-
	opt_util__gather_comments(Instrs0, Comments1, Instrs1),
	(
		Instrs1 = [Instr1prime | Instrs2prime],
		Instr1prime = label(Firstlabel) - _
	->
		Instr1 = Instr1prime,
		Instrs2 = Instrs2prime,
		( Firstlabel = exported(ProclabelPrime) ->
			Proclabel = ProclabelPrime
		; Firstlabel = local(ProclabelPrime) ->
			Proclabel = ProclabelPrime
		;
			error("procedure begins with bad label type")
		)
	;
		error("procedure does not begin with label")
	),
	opt_util__gather_comments(Instrs2, Comments2, Instrs3),
	(
		opt_util__first_base_case(Instrs3, SetupSp, _SetupSuccip,
			Test, After, Teardown, Follow),
		Test = [if_val(Cond, Addr) - _]
	->
		opt_util__new_label_no(Instrs2, 1, Label_no),
		opt_util__filter_in_livevals(Teardown, Livevals),
		Label = local(Proclabel, Label_no),
		% The reason why we keep the saving of succip before the test
		% is to occupy the delay slot of the branch. Without this,
		% the transformation loses performance.
		%
		% This pass is after the value numbering pass because this
		% access to a det stack slot before incr_sp violates the
		% assumptions made by value_number.
		list__condense([
			Comments1,
			[Instr1],
			Comments2,
			[assign(stackvar(0), lval(succip)) - "new setup"],
			[if_val(Cond, label(Label)) - "promoted test"],
			After,
			Livevals,
			[goto(succip) - "proceed without teardown"],
			[label(Label) - "new stack building code"],
			SetupSp,
			[comment("this goto should be optimized away") - ""],
			[goto(Addr) - "clauses after first base clause"],
			Follow
		], Instrs),
		Mod = yes
	;
		Instrs = Instrs0,
		Mod = no
	).

:- end_module peephole.

%-----------------------------------------------------------------------------%
