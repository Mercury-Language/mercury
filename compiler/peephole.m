%-----------------------------------------------------------------------------%

% Peephole.nl - LLDS to LLDS peephole optimization.

% Main author: fjh.
% Jump to jump optimizations and label elimination by zs.

% XXX jump optimization and label elimination must be revisited
% when we start using unilabels.

%-----------------------------------------------------------------------------%

:- module peephole.		
:- interface.
:- import_module llds, options.

:- pred peephole__optimize(option_table, c_file, c_file).
:- mode peephole__optimize(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module value_number, code_util, map, bintree_set.
:- import_module string, list, require, std_util.

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
	options__lookup_bool_option(Options, peephole_jump_opt, Jumpopt),
	( Jumpopt = yes ->
		peephole__jumpopt_instr_list(Instructions0, Instructions1)
	;
		Instructions1 = Instructions0
	),
	options__lookup_bool_option(Options, peephole_local, Local),
	( Local = yes ->
		peephole__opt_instr_list(Instructions1, Instructions2)
	;
		Instructions2 = Instructions1
	),
	options__lookup_bool_option(Options, peephole_label_elim, LabelElim),
	( LabelElim = yes ->
		peephole__label_elim(Instructions2, Instructions3)
	;
		Instructions3 = Instructions2
	),
	options__lookup_bool_option(Options, peephole_value_number, ValueNumber),
	( ValueNumber = yes ->
		value_number__optimize(Instructions3, Instructions)
	;
		Instructions = Instructions3
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
		peephole__skip_comments(Instructions, Instructions1),
		(
			Instructions1 = [Nextinstr | _],
			% write('label '),
			% write(Label),
			% write(' maps to '),
			% write(Nextinstr),
			% nl,
			map__set(Jumpmap0, Label, Nextinstr, Jumpmap1)
		;
			Instructions1 = [],
			Jumpmap1 = Jumpmap0
		)
	;
		Jumpmap1 = Jumpmap0
	),
	peephole__jumpopt_build_jumpmap(Instructions, Jumpmap1, Jumpmap).

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
		( Moreinstr0 = [label(Targetlabel) - _|_] ->
			% eliminating the goto (by the local peephole pass)
			% is better than shortcircuiting it here
			Instr = Instr0
		;
			map__lookup(Jumpmap, Targetlabel, Targetinstr),
			peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
				Jumpmap, Destlabel, Destinstr),
			Destinstr = Udestinstr - _Destcomment,
			string__append("shortcircuited jump: ",
				Comment0, Shorted),
			code_util__can_instr_fall_through(Udestinstr, Canfallthrough),
			( Canfallthrough = no ->
				Instr = Udestinstr - Shorted
			;
				( Targetlabel = Destlabel ->
					Instr = Instr0
				;
					Instr = goto(Destlabel) - Shorted
				)
			)
		)
	;
		Instr = Instr0
	),
	peephole__jumpopt_instr_list(Moreinstr0, Jumpmap, Moreinstr).

:- pred peephole__jumpopt_final_dest(label, instruction, jumpmap,
	label, instruction).
:- mode peephole__jumpopt_final_dest(in, in, in, out, out) is det.

	% Currently we don't check for infinite loops.  This is OK at
	% the moment since the compiler never generates code containing
	% infinite loops, but it may cause problems in the future.

peephole__jumpopt_final_dest(Srclabel, Srcinstr, Jumpmap,
		Destlabel, Destinstr) :-
	(
		Srcinstr = goto(Targetlabel) - Comment,
		map__search(Jumpmap, Targetlabel, Targetinstr)
	->
		% write('goto short-circuit from '),
		% write(Srclabel),
		% write(' to '),
		% write(Targetlabel),
		% nl,
		peephole__jumpopt_final_dest(Targetlabel, Targetinstr,
			Jumpmap, Destlabel, Destinstr)
	;
		Srcinstr = label(Targetlabel) - Comment,
		map__search(Jumpmap, Targetlabel, Targetinstr)
	->
		% write('fallthrough short-circuit from '),
		% write(Srclabel),
		% write(' to '),
		% write(Targetlabel),
		% nl,
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
	% might be branched to from elsewhere. If it isn't, label
	% elimination will get rid of it later.

peephole__opt_instr_2(call(CodeAddress, ContLabel), Comment, Instrs0, Instrs) :-
	Instrs0 = [label(ContLabel) - _, proceed - _ | _],
	Instrs = [tailcall(CodeAddress) - Comment | Instrs0 ].

	% if a `mkframe' is followed by a `modframe', with the instructions
	% in between containing only straight-line code, we can delete the
	% `modframe' and instead just set the redoip directly in the `mkframe'.
	%
	%	mkframe(D, S, _)	=>	mkframe(D, S, Redoip)
	%	<straightline instrs>		<straightline instrs>
	%	modframe(Redoip)

peephole__opt_instr_2(mkframe(Descr, Slots, _), Comment, Instrs0, Instrs) :-
	peephole__next_modframe(Instrs0, [], Redoip, Skipped, Rest),
	list__append(Skipped, Rest, Instrs1),
	Instrs = [mkframe(Descr, Slots, Redoip) - Comment | Instrs1].

	% a `goto' can be deleted if the target of the jump is the very
	% next instruction.
	%
	%	goto next;	=>	  <comments, labels>
	%	<comments, labels>	next:
	%     next:
	%
	% dead code after a `goto' is deleted in label-elim.

peephole__opt_instr_2(goto(Label), _Comment, Instrs0, Instrs) :-
	peephole__is_this_label_next(Label, Instrs0),
	Instrs = Instrs0.

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
		code_util__neg_rval(Rval, NotRval),
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
	% 
	% The two fragments generate the same assembly on our current machines.
	% The transformation may be worthwhile on machines with conditionally
	% executed instructions (Alpha?).

%-----------------------------------------------------------------------------%

	% Build up a table showing which labels are branched to.
	% Then traverse the instruction list removing unnecessary labels.
	% If the instruction before the label branches away, we also
	% remove the instruction block following the label.

:- type usemap == bintree_set(label).

:- pred peephole__label_elim(list(instruction), list(instruction)).
:- mode peephole__label_elim(in, out) is det.

peephole__label_elim(Instructions0, Instructions) :-
	bintree_set__init(Usemap0),
	peephole__label_elim_build_usemap(Instructions0, Usemap0, Usemap),
	peephole__label_elim_instr_list(Instructions0, Usemap, Instructions).

:- pred peephole__label_elim_build_usemap(list(instruction), usemap, usemap).
:- mode peephole__label_elim_build_usemap(in, di, uo) is det.

peephole__label_elim_build_usemap([], Usemap, Usemap).
peephole__label_elim_build_usemap([Instr - _Comment|Instructions],
		Usemap0, Usemap) :-
	( Instr = call(Code_addr, Label) ->
		bintree_set__insert(Usemap0, Label, Usemap1),
		peephole__code_addr_build_usemap(Code_addr, Usemap1, Usemap2)
	; Instr = entrycall(Code_addr, Label) ->
		bintree_set__insert(Usemap0, Label, Usemap1),
		peephole__code_addr_build_usemap(Code_addr, Usemap1, Usemap2)
	; Instr = unicall(_, Label) ->
		bintree_set__insert(Usemap0, Label, Usemap2)
	; Instr = tailcall(local(Label)) ->
		bintree_set__insert(Usemap0, Label, Usemap2)
	; Instr = mkframe(_, _, yes(Label)) ->
		bintree_set__insert(Usemap0, Label, Usemap2)
	; Instr = modframe(yes(Label)) ->
		bintree_set__insert(Usemap0, Label, Usemap2)
	; Instr = goto(Label) ->
		bintree_set__insert(Usemap0, Label, Usemap2)
	; Instr = if_val(_, Label) ->
		bintree_set__insert(Usemap0, Label, Usemap2)
	;
		Usemap2 = Usemap0
	),
	peephole__label_elim_build_usemap(Instructions, Usemap2, Usemap).

:- pred peephole__code_addr_build_usemap(code_addr, usemap, usemap).
:- mode peephole__code_addr_build_usemap(in, di, uo) is det.

peephole__code_addr_build_usemap(Code_addr, Usemap0, Usemap) :-
	( Code_addr = local(Label) ->
		bintree_set__insert(Usemap0, Label, Usemap)
	;
		Usemap = Usemap0
	).

:- pred peephole__label_elim_instr_list(list(instruction),
	usemap, list(instruction)).
:- mode peephole__label_elim_instr_list(in, in, out) is det.

peephole__label_elim_instr_list(Instrs0, Usemap, Instrs) :-
	peephole__label_elim_instr_list(Instrs0, yes, Usemap, Instrs).

:- pred peephole__label_elim_instr_list(list(instruction),
	bool, usemap, list(instruction)).
:- mode peephole__label_elim_instr_list(in, in, in, out) is det.

peephole__label_elim_instr_list([], _Fallthrough, _Usemap, []).
peephole__label_elim_instr_list([Instr0 | Moreinstrs0],
		Fallthrough, Usemap, [Instr | Moreinstrs]) :-
	( Instr0 = label(Label) - Comment ->
		(
		    (   Label = entrylabel(_, _, _, _)
		    ;   bintree_set__is_member(Label, Usemap)
		    )
		->
			Instr = Instr0,
			Fallthrough1 = yes
		; 
			peephole__eliminate(Instr0, yes(Fallthrough), Instr),
			Fallthrough1 = Fallthrough
		)
	;
		( Fallthrough = yes ->
			Instr = Instr0
		;
			peephole__eliminate(Instr0, no, Instr)
		),
		Instr0 = Uinstr0 - Comment,
		code_util__can_instr_fall_through(Uinstr0, Canfallthrough),
		( Canfallthrough = yes ->
			Fallthrough1 = Fallthrough
		;
			Fallthrough1 = no
		)
	),
	peephole__label_elim_instr_list(Moreinstrs0, Fallthrough1, Usemap,
				Moreinstrs).

:- pred peephole__eliminate(instruction, maybe(bool), instruction).
:- mode peephole__eliminate(in, in, out) is det.

peephole__eliminate(Uinstr0 - Comment0, Label, Uinstr - Comment) :-
	( Uinstr0 = comment(_) ->
		Comment = Comment0,
		Uinstr = Uinstr0
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
		Comment = Comment0
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

	% Find the next modframe if it is guaranteed to be reached from here

:- pred peephole__next_modframe(list(instruction), list(instruction),
	maybe(label), list(instruction), list(instruction)).
:- mode peephole__next_modframe(in, in, out, out, out) is semidet.

peephole__next_modframe([Instr | Instrs], RevSkip, Redoip, Skip, Rest) :-
	Instr = Uinstr - _Comment,
	( Uinstr = modframe(Redoip0) ->
		Redoip = Redoip0,
		list__reverse(RevSkip, Skip),
		Rest = Instrs
	; Uinstr = mkframe(_, _, _) ->
		fail
	;
		code_util__can_instr_branch_away(Uinstr, Canbranchaway),
		( Canbranchaway = no ->
			peephole__next_modframe(Instrs, [Instr | RevSkip],
				Redoip, Skip, Rest)
		;
			fail
		)
	).

	% Check whether the named label follows without any intervening code

:- pred peephole__is_this_label_next(label, list(instruction)).
:- mode peephole__is_this_label_next(in, in) is semidet.

peephole__is_this_label_next(Label, [Instr | Moreinstr]) :-
	Instr = Uinstr - _Comment,
	( Uinstr = comment(_) ->
		peephole__is_this_label_next(Label, Moreinstr)
	; Uinstr = label(NextLabel) ->
		( Label = NextLabel ->
			true
		;
			peephole__is_this_label_next(Label, Moreinstr)
		)
	;
		fail
	).

:- end_module peephole.

%-----------------------------------------------------------------------------%
