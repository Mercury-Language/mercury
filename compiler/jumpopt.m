%-----------------------------------------------------------------------------%

% jumpopt.nl - optimize jumps to jumps.

% Author: zs.

%-----------------------------------------------------------------------------%

:- module jumpopt.

:- interface.

:- import_module list, llds.

	% Build up a table showing the first instruction following each label.
	% Then traverse the instruction list, short-circuiting jump sequences.

:- pred jumpopt__main(list(instruction), list(instruction), bool).
:- mode jumpopt__main(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util, std_util, map, string, require.

jumpopt__main(Instrs0, Instrs, Mod) :-
	map__init(Instrmap0),
	map__init(Procmap0),
	map__init(Sdprocmap0),
	map__init(Succmap0),
	jumpopt__build_maps(Instrs0, Instrmap0, Instrmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap),
	% opt_debug__print_instrmap(Instrmap),
	% opt_debug__print_tailmap(Procmap),
	% opt_debug__print_tailmap(Sdprocmap),
	% opt_debug__print_tailmap(Succmap),
	jumpopt__instr_list(Instrs0, comment(""),
		Instrmap, Procmap, Sdprocmap, Succmap, Instrs, Mod).

%-----------------------------------------------------------------------------%

	% Build up three tables mapping labels to instruction sequences.
	% A label has an entry in a table if it is followed by a deterministic,
	% semideterministic or nondeterministic proceed/succeed; the map target
	% gives the code sequence between the label and the proceed/succeed.

:- pred jumpopt__build_maps(list(instruction), instrmap, instrmap,
	tailmap, tailmap, tailmap, tailmap, tailmap, tailmap).
:- mode jumpopt__build_maps(in, di, uo, di, uo, di, uo, di, uo) is det.

jumpopt__build_maps([], Instrmap, Instrmap, Procmap, Procmap,
	Sdprocmap, Sdprocmap, Succmap, Succmap).
jumpopt__build_maps([Instr - _Comment|Instrs], Instrmap0, Instrmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap) :-
	( Instr = label(Label) ->
		opt_util__skip_comments_livevals(Instrs, Instrs1),
		( Instrs1 = [Nextinstr | _] ->
			map__set(Instrmap0, Label, Nextinstr, Instrmap1)
		;
			Instrmap1 = Instrmap0
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
		Instrmap1 = Instrmap0,
		Procmap1 = Procmap0,
		Sdprocmap1 = Sdprocmap0,
		Succmap1 = Succmap0
	),
	jumpopt__build_maps(Instrs, Instrmap1, Instrmap,
		Procmap1, Procmap, Sdprocmap1, Sdprocmap, Succmap1, Succmap).

%-----------------------------------------------------------------------------%

	% Optimize the given instruction list by eliminating unnecessary
	% jumps.
	%
	% We handle calls by trying to short-circuit the return address.
	%
	% We handle gotos by first trying to eliminate them. If this fails,
	% we check whether their target label begins a proceed/succeed
	% sequence; if it does, we replace the label by that sequence.
	% If this fails as well, we check whether the instruction at the
	% ultimate target label can fall through. If it cannot (e.g. call),
	% we replace the goto with this instruction.
	%
	% We handle computed gotos by attempting to short-circuit all the
	% labels in the label list.

:- pred jumpopt__instr_list(list(instruction), instr,
	instrmap, tailmap, tailmap, tailmap, list(instruction), bool).
:- mode jumpopt__instr_list(in, in, in, in, in, in, out, out) is det.

jumpopt__instr_list([], _Previnstr,
		_Instrmap, _Procmap, _Sdprocmap, _Succmap, [], no).
jumpopt__instr_list([Instr0 | Instrs0], Previnstr,
		Instrmap, Procmap, Sdprocmap, Succmap, Instrs, Mod) :-
	Instr0 = Uinstr0 - Comment0,
	string__append(Comment0, " (redirected return)", Redirect),
	(
		Uinstr0 = call(Proc, label(Retlabel), Caller, LiveVals),
		map__search(Instrmap, Retlabel, Retinstr)
	->
		jumpopt__final_dest(Retlabel, Retinstr, Instrmap,
			Destlabel, Destinstr),
		( Retlabel = Destlabel ->
			Newinstrs = [Instr0],
			Mod0 = no
		;
			Newinstrs = [call(Proc, label(Destlabel), Caller,
				LiveVals) - Redirect],
			Mod0 = yes
		)
	;
		Uinstr0 = goto(label(TargetLabel))
	->
		(
			opt_util__is_this_label_next(TargetLabel, Instrs0, _)
		->
			% Eliminating is better than shortcircuiting.
			Newinstrs = [],
			Mod0 = yes
		;
			Previnstr = if_val(_, label(IfTargetLabel)),
			opt_util__is_this_label_next(IfTargetLabel, Instrs0, _)
		->
			% Eliminating the goto (by the local peephole pass)
			% is better than shortcircuiting it here,
			% PROVIDED the test will succeed most of the time;
			% we could use profiling feedback on this.
			% We cannot eliminate the instruction here because
			% that would require altering the if_val instruction.
			Newinstrs = [Instr0],
			Mod0 = no
		;
			map__search(Procmap, TargetLabel, Between)
		->
			list__append(Between, [goto(succip) - "shortcircuit"],
				Newinstrs),
			Mod0 = yes
		;
			map__search(Sdprocmap, TargetLabel, Between)
		->
			list__append(Between, [goto(succip) - "shortcircuit"],
				Newinstrs),
			Mod0 = yes
		;
			map__search(Succmap, TargetLabel, Between)
		->
			list__append(Between, [goto(do_succeed) - "shortcircuit"],
				Newinstrs),
			Mod0 = yes
		;
			map__search(Instrmap, TargetLabel, TargetInstr)
		->
			jumpopt__final_dest(TargetLabel, TargetInstr,
				Instrmap, Destlabel, Destinstr),
			Destinstr = Udestinstr - _Destcomment,
			string__append("shortcircuited jump: ",
				Comment0, Shorted),
			opt_util__can_instr_fall_through(Udestinstr,
				Canfallthrough),
			( Canfallthrough = no ->
				Newinstrs = [Udestinstr - Shorted],
				Mod0 = yes
			;
				( TargetLabel = Destlabel ->
					Newinstrs = [Instr0],
					Mod0 = no
				;
					Newinstrs = [goto(label(Destlabel))
							- Shorted],
					Mod0 = yes
				)
			)
		;
			% error("target label not in instrmap")
			Newinstrs = [Instr0],
			Mod0 = no
		)
	; Uinstr0 = computed_goto(Index, LabelList0) ->
		jumpopt__short_labels(LabelList0, Instrmap, LabelList, Mod0),
		( Mod0 = yes ->
			string__append(Comment0, " (some shortcircuits)",
				Shorted),
			Newinstrs = [computed_goto(Index, LabelList) - Shorted]
		;
			Newinstrs = [Instr0]
		)
	;
		Newinstrs = [Instr0],
		Mod0 = no
	),
	( ( Uinstr0 = comment(_) ; Uinstr0 = livevals(_) ) ->
		Newprevinstr = Previnstr
	;
		Newprevinstr = Uinstr0
	),
	jumpopt__instr_list(Instrs0, Newprevinstr,
		Instrmap, Procmap, Sdprocmap, Succmap, Instrs1, Mod1),
	list__append(Newinstrs, Instrs1, Instrs),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

%-----------------------------------------------------------------------------%

	% Short-circuit the given label by following any gotos at the
	% labelled instruction or by falling through consecutive labels.

:- pred jumpopt__short_label(label, instrmap, label, bool).
:- mode jumpopt__short_label(in, in, out, out) is det.

jumpopt__short_label(Label0, Instrmap, Label, Mod) :-
	map__lookup(Instrmap, Label0, Instr0),
	jumpopt__final_dest(Label0, Instr0, Instrmap, Label, _Instr),
	( Label = Label0 ->
		Mod = no
	;
		Mod = yes
	).

:- pred jumpopt__short_labels(list(label), instrmap, list(label), bool).
:- mode jumpopt__short_labels(in, in, out, out) is det.

% XXX these uses of the Mod argument should be replaced by accumulator passing

jumpopt__short_labels([], _Instrmap, [], no).
jumpopt__short_labels([Label0 | Labels0], Instrmap, [Label | Labels], Mod) :-
	jumpopt__short_label(Label0, Instrmap, Label, Mod1),
	jumpopt__short_labels(Labels0, Instrmap, Labels, Mod2),
	( Mod1 = no, Mod2 = no ->
		Mod = no
	;
		Mod = yes
	).

%-----------------------------------------------------------------------------%

	% Find the final destination of a given instruction at a given label.
	% We follow gotos as well as consecutive labels.

	% Currently we don't check for infinite loops.  This is OK at
	% the moment since the compiler never generates code containing
	% infinite loops, but it may cause problems in the future.

:- pred jumpopt__final_dest(label, instruction, instrmap, label, instruction).
:- mode jumpopt__final_dest(in, in, in, out, out) is det.

jumpopt__final_dest(SrcLabel, SrcInstr, Instrmap, DestLabel, DestInstr) :-
	(
		SrcInstr = SrcUinstr - _Comment,
		(
			SrcUinstr = goto(label(TargetLabel))
		;
			SrcUinstr = label(TargetLabel)
		),
		map__search(Instrmap, TargetLabel, TargetInstr)
	->
		jumpopt__final_dest(TargetLabel, TargetInstr,
			Instrmap, DestLabel, DestInstr)
	;
		DestLabel = SrcLabel,
		DestInstr = SrcInstr
	).
