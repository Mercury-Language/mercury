%-----------------------------------------------------------------------------%

% jumpopt.nl - optimize jumps to jumps.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module jumpopt.

:- interface.

:- import_module list, llds.

:- pred jumpopt__main(list(instruction), list(instruction), bool).
:- mode jumpopt__main(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util, std_util, map, string, require.

	% Build up a table showing the first instruction following each label.
	% Then traverse the instruction list, short-circuiting jump sequences.

jumpopt__main(Instrs0, Instrs, Mod) :-
	map__init(Instmap0),
	map__init(Procmap0),
	map__init(Sdprocmap0),
	map__init(Succmap0),
	jumpopt__build_maps(Instrs0, Instmap0, Instmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap),
	% opt_debug__print_instmap(Instmap),
	% opt_debug__print_tailmap(Procmap),
	% opt_debug__print_tailmap(Sdprocmap),
	% opt_debug__print_tailmap(Succmap),
	jumpopt__instr_list(Instrs0, comment(""),
		Instmap, Procmap, Sdprocmap, Succmap, Instrs, Mod).

:- pred jumpopt__build_maps(list(instruction), instmap, instmap,
	tailmap, tailmap, tailmap, tailmap, tailmap, tailmap).
:- mode jumpopt__build_maps(in, di, uo, di, uo, di, uo, di, uo) is det.

jumpopt__build_maps([], Instmap, Instmap, Procmap, Procmap,
	Sdprocmap, Sdprocmap, Succmap, Succmap).
jumpopt__build_maps([Instr - _Comment|Instrs], Instmap0, Instmap,
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
	jumpopt__build_maps(Instrs, Instmap1, Instmap,
		Procmap1, Procmap, Sdprocmap1, Sdprocmap, Succmap1, Succmap).

:- pred jumpopt__instr_list(list(instruction), instr,
	instmap, tailmap, tailmap, tailmap, list(instruction), bool).
:- mode jumpopt__instr_list(in, in, in, in, in, in, out, out) is det.

jumpopt__instr_list([], _Previnstr,
		_Instmap, _Procmap, _Sdprocmap, _Succmap, [], no).
jumpopt__instr_list([Instr0|Moreinstrs0], Previnstr,
		Instmap, Procmap, Sdprocmap, Succmap, Instrs, Mod) :-
	Instr0 = Uinstr0 - Comment0,
	string__append(Comment0, " (redirected return)", Redirect),
	(
		Uinstr0 = call(Proc, label(Retlabel)),
		map__search(Instmap, Retlabel, Retinstr)
	->
		jumpopt__final_dest(Retlabel, Retinstr,
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
			jumpopt__final_dest(Targetlabel, Targetinstr,
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
			% error("target label not in instmap")
			Newinstrs = [Instr0],
			Mod0 = no
		)
	; Uinstr0 = computed_goto(Index, LabelList0) ->
		jumpopt__short_labels(LabelList0, Instmap, LabelList, Mod0),
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
	jumpopt__instr_list(Moreinstrs0, Newprevinstr,
		Instmap, Procmap, Sdprocmap, Succmap, Moreinstrs, Mod1),
	list__append(Newinstrs, Moreinstrs, Instrs),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred jumpopt__short_labels(list(label), instmap, list(label), bool).
:- mode jumpopt__short_labels(in, in, out, out) is det.

% XXX these uses of the Mod argument should be replaced by accumulator passing

jumpopt__short_labels([], _Instmap, [], no).
jumpopt__short_labels([Label0 | Labels0], Instmap, [Label | Labels], Mod) :-
	jumpopt__short_label(Label0, Instmap, Label, Mod1),
	jumpopt__short_labels(Labels0, Instmap, Labels, Mod2),
	( Mod1 = no, Mod2 = no ->
		Mod = no
	;
		Mod = yes
	).

:- pred jumpopt__short_label(label, instmap, label, bool).
:- mode jumpopt__short_label(in, in, out, out) is det.

jumpopt__short_label(Label0, Instmap, Label, Mod) :-
	( map__search(Instmap, Label0, Instr0) ->
		jumpopt__final_dest(Label0, Instr0, Instmap,
			Label, _Instr),
		( Label = Label0 ->
			Mod = no
		;
			Mod = yes
		)
	;
		error("target label not in instmap")
	).

:- pred jumpopt__final_dest(label, instruction, instmap,
	label, instruction).
:- mode jumpopt__final_dest(in, in, in, out, out) is det.

	% Currently we don't check for infinite loops.  This is OK at
	% the moment since the compiler never generates code containing
	% infinite loops, but it may cause problems in the future.

jumpopt__final_dest(Srclabel, Srcinstr, Instmap,
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
		jumpopt__final_dest(Targetlabel, Targetinstr,
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
		jumpopt__final_dest(Targetlabel, Targetinstr,
			Instmap, Destlabel, Destinstr)
	;
		Destlabel = Srclabel,
		Destinstr = Srcinstr
	).
