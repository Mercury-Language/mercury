%-----------------------------------------------------------------------------%

% Peephole.nl - LLDS to LLDS peephole optimization.

% Main author: fjh.
% Jump to jump optimizations by zs.

% XXX jump optimization must be revisited when we start using unilabels.

%-----------------------------------------------------------------------------%

:- module peephole.		
:- interface.
:- import_module llds, options.

:- pred peephole__optimize(option_table, c_file, c_file).
:- mode peephole__optimize(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, string, list, require, std_util.

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

peephole__opt_module(Options, c_module(Name,Procs0), c_module(Name,Procs)) :-
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

peephole__opt_proc(Options, c_procedure(Name,Arity,Mode,Instructions0),
		   c_procedure(Name,Arity,Mode,Instructions)) :-
	( options__lookup_bool_option(Options, peephole_jump_opt, yes) ->
		peephole__jumpopt_instr_list(Instructions0, Instructions1)
	;
		Instructions1 = Instructions0
	),
	( options__lookup_bool_option(Options, peephole_local, yes) ->
		peephole__opt_instr_list(Instructions1, Instructions)
	;
		Instructions = Instructions1
	).

%-----------------------------------------------------------------------------%

	% Build up a table showing the first instruction following each label.
	% Then traverse the instruction list, short-circuiting jump sequences.

:- type jumpmap == map(label, instruction).

:- pred peephole__jumpopt_instr_list(list(instruction), list(instruction)).
:- mode peephole__jumpopt_instr_list(in, out) is det.

peephole__jumpopt_instr_list(Instructions0, Instructions) :-
	map__init(Jumpmap0),
	peephole__jumpopt_build_jumpmap(Instructions0, Jumpmap0, Jumpmap),
	peephole__jumpopt_instr_list(Instructions0, Jumpmap, Instructions).

:- pred peephole__jumpopt_build_jumpmap(list(instruction), jumpmap, jumpmap).
:- mode peephole__jumpopt_build_jumpmap(in, di, uo) is det.

peephole__jumpopt_build_jumpmap([], Jumpmap, Jumpmap).
peephole__jumpopt_build_jumpmap([Instr - _Comment|Instructions],
		Jumpmap0, Jumpmap) :-
	( Instr = label(Label) ->
		peephole__jumpopt_next_instr(Instructions, Next),
		(
			Next = yes(Nextinstr),
			map__set(Jumpmap0, Label, Nextinstr, Jumpmap1)
		;
			Next = no,
			Jumpmap1 = Jumpmap0
		)
	;
		Jumpmap1 = Jumpmap0
	),
	peephole__jumpopt_build_jumpmap(Instructions, Jumpmap1, Jumpmap).

:- pred peephole__jumpopt_next_instr(list(instruction), maybe(instruction)).
:- mode peephole__jumpopt_next_instr(in, out) is det.

peephole__jumpopt_next_instr([], no).
peephole__jumpopt_next_instr([Instr|Moreinstr], Next) :-
	Instr = Uinstr - _Comment,
	( Uinstr = label(_) ->
		peephole__jumpopt_next_instr(Moreinstr, Next)
	; Uinstr = comment(_) ->
		peephole__jumpopt_next_instr(Moreinstr, Next)
	;
		Next = yes(Instr)
	).

:- pred peephole__jumpopt_instr_list(list(instruction),
	map(label, instruction), list(instruction)).
:- mode peephole__jumpopt_instr_list(in, in, out) is det.

peephole__jumpopt_instr_list([], _Jumpmap, []).
peephole__jumpopt_instr_list([Instr0|Moreinstr0], Jumpmap,
		[Instr|Moreinstr]) :-
	Instr0 = Uinstr0 - Comment0,
	string__append(Comment0, " (redirected return)", Redirect),
	( Uinstr0 = call(Proc, Retlabel) ->
		map__lookup(Jumpmap, Retlabel, Retinstr),
		peephole__jumpopt_final_dest(Retlabel, Retinstr,
			Jumpmap, Destlabel, Destinstr),
		( Retlabel = Destlabel ->
			Instr = Instr0
		;
			Instr = call(Proc, Destlabel) - Redirect
		)
	; Uinstr0 = entrycall(Proc, Retlabel) ->
		map__lookup(Jumpmap, Retlabel, Retinstr),
		peephole__jumpopt_final_dest(Retlabel, Retinstr,
			Jumpmap, Destlabel, Destinstr),
		( Retlabel = Destlabel ->
			Instr = Instr0
		;
			Instr = entrycall(Proc, Destlabel) - Redirect
		)
	; Uinstr0 = unicall(Unilabel, Retlabel) ->
		map__lookup(Jumpmap, Retlabel, Retinstr),
		peephole__jumpopt_final_dest(Retlabel, Retinstr,
			Jumpmap, Destlabel, Destinstr),
		( Retlabel = Destlabel ->
			Instr = Instr0
		;
			Instr = unicall(Unilabel, Destlabel) - Redirect
		)
	; Uinstr0 = goto(Targetlabel) ->
		% eliminating the goto (by a later pass)
		% is better than shortcircuiting it here
		( Moreinstr0 = [label(Targetlabel) - _|_] ->
			Instr = Instr0
		;
			map__lookup(Jumpmap, Targetlabel, Targetinstr),
			peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
				Jumpmap, Destlabel, Destinstr),
			Destinstr = Udestinstr - _Destcomment,
			string__append("shortcircuited jump: ",
				Comment0, Shorted),
			( peephole__indirect_jump(Udestinstr) ->
				Instr = Udestinstr - Shorted
			;
				( Targetlabel = Destlabel ->
					Instr = goto(Destlabel) - Comment0
				;
					Instr = goto(Destlabel) - Shorted
				)
			)
		)
	;
		Instr = Instr0
	),
	peephole__jumpopt_instr_list(Moreinstr0, Jumpmap, Moreinstr).

:- pred peephole__indirect_jump(instr).
:- mode peephole__indirect_jump(in) is semidet.

peephole__indirect_jump(call(_, _)).
peephole__indirect_jump(entrycall(_, _)).
peephole__indirect_jump(unicall(_, _)).
peephole__indirect_jump(fail).
peephole__indirect_jump(redo).
peephole__indirect_jump(succeed).
peephole__indirect_jump(proceed).

:- pred peephole__jumpopt_final_dest(label, instruction, jumpmap,
	label, instruction).
:- mode peephole__jumpopt_final_dest(in, in, in, out, out) is det.

	% Currently we don't check for infinite loops.  This is OK at
	% the moment since the compiler never generates code containing
	% infinite loops, but it may cause problems in the future.

peephole__jumpopt_final_dest(Srclabel, Srcinstr, Jumpmap,
		Destlabel, Destinstr) :-
	(
		Srcinstr = goto(Targetlabel) - _Comment,
		map__search(Jumpmap, Targetlabel, Targetinstr)
	->
		peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
			Jumpmap, Destlabel, Destinstr)
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

:- pred peephole__opt_instr_list(list(instruction), list(instruction)).
:- mode peephole__opt_instr_list(in, out) is det.

peephole__opt_instr_list([], []).
peephole__opt_instr_list([Instr0 - Comment|Instructions0], Instructions) :-
	peephole__opt_instr_list(Instructions0, Instructions1),
	peephole__opt_instr(Instr0, Comment, Instructions1, Instructions).

:- pred peephole__opt_instr(instr, string, list(instruction),
				list(instruction)).
:- mode peephole__opt_instr(in, in, in, out) is det.

peephole__opt_instr(Instr0, Comment0, Instructions0, Instructions) :-
	(
		peephole__skip_comments(Instructions0, Instructions1),
		peephole__opt_instr_2(Instr0, Comment0, Instructions1,
			Instructions2)
	->
		( Instructions2 = [Instr2 - Comment2 | Instructions3] ->
			peephole__opt_instr(Instr2, Comment2, Instructions3,
				Instructions)
		;
			Instructions = Instructions2
		)
	;
		Instructions = [Instr0 - Comment0 | Instructions0]
	).

%-----------------------------------------------------------------------------%

	% Given a list of instructions, skip past any comment instructions
	% at the start and return the remaining instructions.
	% We do this because comment instructions get in the way of
	% peephole optimization.

:- pred peephole__skip_comments(list(instruction), list(instruction)).
:- mode peephole__skip_comments(in, out) is det.

peephole__skip_comments(Instrs0, Instrs) :-
	( Instrs0 = [comment(_) - _ | Instrs1] ->
		peephole__skip_comments(Instrs1, Instrs)
	;
		Instrs = Instrs0
	).

%-----------------------------------------------------------------------------%

:- pred peephole__opt_instr_2(instr, string, list(instruction),
				list(instruction)).
:- mode peephole__opt_instr_2(in, in, in, out) is semidet.

	% A `call' followed by a `proceed' can be replaced with a `tailcall'.
	%
	%	call(Foo, &&ret);		tailcall(Foo)
	%     ret:			=>    ret:
	%	proceed				proceed
	%	
	% Note that we can't delete the `ret: proceed', since `ret'
	% might be branched to from elsewhere.

peephole__opt_instr_2(call(CodeAddress, ContLabel), Comment, Instrs0, Instrs) :-
	Instrs0 = [label(ContLabel) - _, proceed - _ | _],
	Instrs = [tailcall(CodeAddress) - Comment | Instrs0 ].

	% if a `mkframe' is immediately followed by a `modframe', we
	% can delete the `modframe' and instead just set the redoip
	% directly in the `mkframe'.
	%
	%	mkframe(D, S, _)	=>	mkframe(D, S, Redoip)
	%	modframe(Redoip)
	%
	% XXX this is usually not effective since the two instrs are
	% usually separated by some arg saves.

peephole__opt_instr_2(mkframe(Descr, Slots, _), Comment, Instrs0, Instrs) :-
	Instrs0 = [modframe(Redoip) - _ | Instrs1],
	Instrs = [mkframe(Descr, Slots, Redoip) - Comment | Instrs1].

	% a `goto' can be deleted if the target of the jump is the very
	% next instruction.
	%
	%	goto next;	=>	next:
	%     next:
	%
	% anything after a `goto' except a label is dead code and can be
	% deleted.
	%
	%	goto label1;	=>	goto label1;
	%	...		      label2:
	%     label2:

peephole__opt_instr_2(goto(Label), Comment, Instrs0, Instrs) :-
	Instrs0 = [Instr0 - _ | Instrs1],
	( Instr0 = label(Label1) ->
		( Label = Label1 ->
			Instrs = Instrs0	% delete the goto
		;
			fail
		)
	;
		Instrs = [goto(Label) - Comment | Instrs1]
	).

	% a conditional branch over a branch can be replaced
	% by an inverse conditional branch
	%
	%	if (x) goto skip;		if (!x) goto somewhere
	%	goto somewhere;		=>    skip:
	%     skip:
	%
	% a conditional branch to the very next instruction
	% can be deleted
	%	if (x) goto next;	=>    next:
	%     next:

peephole__opt_instr_2(if_val(Rval, Target), _C1, Instrs0, Instrs) :-
	( Instrs0 = [goto(Somewhere) - C2, label(Target) - C3 | Instrs1] ->
		peephole__neg_rval(Rval, NotRval),
		Instrs = [if_val(NotRval, Somewhere) - C2, label(Target) - C3
				| Instrs1]
	; Instrs0 = [label(Target) - _ | _] ->
		Instrs = Instrs0
	;
		fail
	).

	% A conditional branch around a redo or fail can be replaced
	% by an inverse conditional redo or fail
	%
	%	if (x) goto skip;		if (!x) redo;
	%	redo;			=>    skip:
	%     skip:
	%
	% This would require a change to the type of the second arg of if_val.
	% Is it worth it?  The two fragments generate the same assembly.

%-----------------------------------------------------------------------------%

:- pred peephole__neg_rval(rval, rval).
:- mode peephole__neg_rval(in, out) is det.

peephole__neg_rval(Rval, NegRval) :-
	( Rval = not(NegRval0) ->
		NegRval = NegRval0
	;	
		NegRval = not(Rval)
	).

%-----------------------------------------------------------------------------%

:- end_module peephole.

%-----------------------------------------------------------------------------%
