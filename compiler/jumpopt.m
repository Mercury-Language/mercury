%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% jumpopt.m - optimize jumps to jumps.
%
% Author: zs.

%-----------------------------------------------------------------------------%

:- module jumpopt.

:- interface.

:- import_module llds.
:- import_module list, bool.

:- pred jumpopt_main(list(instruction), bool, bool, list(instruction), bool).
:- mode jumpopt_main(in, in, in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module code_util, opt_util.
:- import_module std_util, map, string, require.

% We first build up a bunch of tables giving information about labels.
% We then traverse the instruction list, using the information in the
% tables to short-circuit jumps.
%
% Instrmap:	Maps each label to the next real (non-comment, non-livevals)
%		instruction after that label.
% Lvalmap:	Maps each label to yes(Livevals) if the label is followed
%		by a livevals instruction, and to no otherwise.
% Blockmap:	Maps each label to the block following that label.
%		This includes all instructions up to the first one that
%		cannot fall through.
% Procmap:	Maps each label that begins a det epilog to the epilog.
% Succmap:	Maps each label that begins a nondet epilog to the epilog.
% Sdprocmap:	Maps each label that begins a semidet epilog to the epilog.
%		This can be the success epilog or the failure epilog.
% Forkmap:	Maps each label that begins a full semidet epilog (code to
%		test r1, and to execute the the success or failure epilog
%		depending on the result) to the epilog.
%
% If we are not doing full jump optimization, Blockmap will be empty.
% Even with full jump optimization, Blockmap will not contain the initial
% block of the procedure unless Recjump is set. The intention is that
% Recjump will not be set until optimizations such as frameopt and value
% numbering, which can do a better job of optimizing this block, have
% been applied.

jumpopt_main(Instrs0, Blockopt, Recjump, Instrs, Mod) :-
	map__init(Instrmap0),
	map__init(Lvalmap0),
	map__init(Procmap0),
	map__init(Sdprocmap0),
	map__init(Succmap0),
	map__init(Blockmap0),
	jumpopt__build_maps(Instrs0, Blockopt, Recjump, Instrmap0, Instrmap,
		Blockmap0, Blockmap, Lvalmap0, Lvalmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap),
	map__init(Forkmap0),
	jumpopt__build_forkmap(Instrs0, Sdprocmap, Forkmap0, Forkmap),
	jumpopt__instr_list(Instrs0, comment(""), Instrmap, Blockmap, Lvalmap,
		Procmap, Sdprocmap, Forkmap, Succmap, Instrs1, Mod),
	opt_util__filter_out_bad_livevals(Instrs1, Instrs).

%-----------------------------------------------------------------------------%

:- pred jumpopt__build_maps(list(instruction), bool, bool,
	instrmap, instrmap, tailmap, tailmap, lvalmap, lvalmap,
	tailmap, tailmap, tailmap, tailmap, tailmap, tailmap).
% :- mode jumpopt__build_maps(in, in, in, di, uo, di, uo, di, uo, di, uo,
%	di, uo, di, uo) is det.
:- mode jumpopt__build_maps(in, in, in, in, out, in, out, in, out, in, out,
	in, out, in, out) is det.

jumpopt__build_maps([], _, _,
		Instrmap, Instrmap, Blockmap, Blockmap, Lvalmap, Lvalmap,
		Procmap, Procmap, Sdprocmap, Sdprocmap, Succmap, Succmap).
jumpopt__build_maps([Instr0 | Instrs0], Blockopt, Recjump, Instrmap0, Instrmap,
		Blockmap0, Blockmap, Lvalmap0, Lvalmap,
		Procmap0, Procmap, Sdprocmap0, Sdprocmap, Succmap0, Succmap) :-
	Instr0 = Uinstr0 - _,
	( Uinstr0 = label(Label) ->
		opt_util__skip_comments(Instrs0, Instrs1),
		( Instrs1 = [Instr1 | _], Instr1 = livevals(_) - _ ->
			map__det_insert(Lvalmap0, Label, yes(Instr1), Lvalmap1)
		;
			map__det_insert(Lvalmap0, Label, no, Lvalmap1)
		),
		opt_util__skip_comments_livevals(Instrs1, Instrs2),
		( Instrs2 = [Instr2 | _] ->
			map__det_insert(Instrmap0, Label, Instr2, Instrmap1)
		;
			Instrmap1 = Instrmap0
		),
		( opt_util__is_proceed_next(Instrs1, Between1) ->
			map__det_insert(Procmap0, Label, Between1, Procmap1)
		;
			Procmap1 = Procmap0
		),
		( opt_util__is_sdproceed_next(Instrs1, Between2) ->
			map__det_insert(Sdprocmap0, Label, Between2, Sdprocmap1)
		;
			Sdprocmap1 = Sdprocmap0
		),
		( opt_util__is_succeed_next(Instrs1, Between3) ->
			map__det_insert(Succmap0, Label, Between3, Succmap1)
		;
			Succmap1 = Succmap0
		),
		% put the start of the procedure into Blockmap
		% only after frameopt and value_number have had a shot at it
		( Blockopt = yes, ( Label = local(_, _) ; Recjump = yes ) ->
			opt_util__find_no_fallthrough(Instrs1, Block),
			map__det_insert(Blockmap0, Label, Block, Blockmap1)
		;
			Blockmap1 = Blockmap0
		)
	;
		Instrmap1 = Instrmap0,
		Blockmap1 = Blockmap0,
		Lvalmap1 = Lvalmap0,
		Procmap1 = Procmap0,
		Sdprocmap1 = Sdprocmap0,
		Succmap1 = Succmap0
	),
	jumpopt__build_maps(Instrs0, Blockopt, Recjump, Instrmap1, Instrmap,
		Blockmap1, Blockmap, Lvalmap1, Lvalmap,
		Procmap1, Procmap, Sdprocmap1, Sdprocmap, Succmap1, Succmap).

	% Find labels followed by a test of r1 where both paths set r1 to
	% its original value and proceed.

:- pred jumpopt__build_forkmap(list(instruction), tailmap, tailmap, tailmap).
% :- mode jumpopt__build_forkmap(in, in, di, uo) is det.
:- mode jumpopt__build_forkmap(in, in, in, out) is det.

jumpopt__build_forkmap([], _Sdprocmap, Forkmap, Forkmap).
jumpopt__build_forkmap([Instr - _Comment|Instrs], Sdprocmap,
		Forkmap0, Forkmap) :-
	(
		Instr = label(Label),
		opt_util__is_forkproceed_next(Instrs, Sdprocmap, Between)
	->
		map__det_insert(Forkmap0, Label, Between, Forkmap1)
	;
		Forkmap1 = Forkmap0
	),
	jumpopt__build_forkmap(Instrs, Sdprocmap, Forkmap1, Forkmap).

%-----------------------------------------------------------------------------%

	% Optimize the given instruction list by eliminating unnecessary
	% jumps.
	%
	% We handle calls by attempting to turn them into tailcalls. If this
	% fails, we try to short-circuit the return address.
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
	%
	% We handle if-vals by trying to turn them into the assignment
	% of a boolean value to r1, or by short-circuiting the target label.
	% We also try to eliminate a goto following an if-val, if we can
	% do so by negating the condition and possibly also deleting a label
	% between the if-val and the goto.

:- pred jumpopt__instr_list(list(instruction), instr, instrmap, tailmap,
	lvalmap, tailmap, tailmap, tailmap, tailmap, list(instruction), bool).
:- mode jumpopt__instr_list(in, in, in, in, in, in, in, in, in, out, out)
	is det.

jumpopt__instr_list([], _PrevInstr, _Instrmap, _Blockmap,
		_Lvalmap, _Procmap, _Sdprocmap, _Forkmap, _Succmap, [], no).
jumpopt__instr_list([Instr0 | Instrs0], PrevInstr, Instrmap, Blockmap,
		Lvalmap, Procmap, Sdprocmap, Forkmap, Succmap, Instrs, Mod) :-
	Instr0 = Uinstr0 - Comment0,
	string__append(Comment0, " (redirected return)", Redirect),
	(
		Uinstr0 = call(Proc, label(RetLabel), GC, CallModel)
	->
		(
			% Look for det style tailcalls. We look for this
			% even if the call is semidet because one of the
			% optimizations below turns a pair of semidet epilogs
			% into a det epilog.
			( CallModel = det ; CallModel = semidet ),
			map__search(Procmap, RetLabel, Between0),
			PrevInstr = livevals(Livevals) 
		->
			opt_util__filter_out_livevals(Between0, Between1),
			list__append(Between1, [livevals(Livevals) - "",
				goto(Proc) - Redirect], NewInstrs),
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Look for semidet style tailcalls.
			CallModel = semidet,
			map__search(Forkmap, RetLabel, Between),
			PrevInstr = livevals(Livevals) 
		->
			list__append(Between, [livevals(Livevals) - "",
				goto(Proc) - Redirect], NewInstrs),
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Look for nondet style tailcalls.
			CallModel = nondet(yes),
			map__search(Succmap, RetLabel, BetweenIncl),
			BetweenIncl = [livevals(_) - _, goto(_) - _],
			PrevInstr = livevals(Livevals) 
		->
			NewInstrs = [
				assign(maxfr, lval(prevfr(lval(curfr))))
					- "discard this frame",
				assign(succip, lval(succip(lval(curfr))))
					- "setup PC on return from tailcall",
				assign(curfr, lval(succfr(lval(curfr))))
					- "setup curfr on return from tailcall",
				livevals(Livevals) - "",
				goto(Proc) - Redirect
			],
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Short circuit the return label if possible.
			map__search(Instrmap, RetLabel, RetInstr)
		->
			jumpopt__final_dest(RetLabel, RetInstr, Instrmap,
				DestLabel, _DestInstr),
			( RetLabel = DestLabel ->
				NewInstrs = [Instr0],
				RemainInstrs = Instrs0,
				Mod0 = no
			;
				NewInstrs = [call(Proc, label(DestLabel),
					GC, CallModel) - Redirect],
				RemainInstrs = Instrs0,
				Mod0 = yes
			)
		;
			NewInstrs = [Instr0],
			RemainInstrs = Instrs0,
			Mod0 = no
		)
	;
		Uinstr0 = goto(label(TargetLabel))
	->
		(
			% Eliminate the goto if possible.
			opt_util__is_this_label_next(TargetLabel, Instrs0, _)
		->
			NewInstrs = [],
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			PrevInstr = if_val(_, label(IfTargetLabel)),
			opt_util__is_this_label_next(IfTargetLabel, Instrs0, _)
		->
			% Eliminating the goto (by the local peephole pass)
			% is better than shortcircuiting it here,
			% PROVIDED the test will succeed most of the time;
			% we could use profiling feedback on this.
			% We cannot eliminate the instruction here because
			% that would require altering the if_val instruction.
			NewInstrs = [Instr0],
			RemainInstrs = Instrs0,
			Mod0 = no
		;
			% Replace a jump to a det epilog with the epilog.
			map__search(Procmap, TargetLabel, Between0)
		->
			jumpopt__adjust_livevals(PrevInstr, Between0, Between),
			list__append(Between, [goto(succip) - "shortcircuit"],
				NewInstrs),
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Replace a jump to a semidet epilog with the epilog.
			map__search(Sdprocmap, TargetLabel, Between0)
		->
			jumpopt__adjust_livevals(PrevInstr, Between0, Between),
			list__append(Between, [goto(succip) - "shortcircuit"],
				NewInstrs),
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Replace a jump to a nondet epilog with the epilog.
			map__search(Succmap, TargetLabel, BetweenIncl0)
		->
			jumpopt__adjust_livevals(PrevInstr, BetweenIncl0,
				NewInstrs),
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Replace a jump to a non-epilog block with the
			% block itself. These jumps are treated separately
			% from jumps to epilog blocks, for two reasons.
			% First, epilog blocks are always short, so we
			% always want to replace jumps to them, whereas
			% other blocks may be long, so we want to replace
			% jumps to them only if the fulljumps option
			% was given (if it wasn't, Blockmap will be empty).
			% Second, non-epilog blocks may contain branches to
			% other labels in this procedure, and we want to
			% make sure that these are short-circuited.
			% This short-circuiting is necessary because
			% another optimization below eliminates labels,
			% which is correct only if jumps to those labels
			% are short-circuited everywhere.
			map__search(Instrmap, TargetLabel, TargetInstr),
			jumpopt__final_dest(TargetLabel, TargetInstr,
				Instrmap, DestLabel, _DestInstr),
			map__search(Blockmap, DestLabel, Block)
		->
			opt_util__filter_out_labels(Block, FilteredBlock),
			jumpopt__adjust_livevals(PrevInstr, FilteredBlock,
				AdjustedBlock),
			% Block may end with a goto to DestLabel. We avoid
			% infinite expansion in such cases by removing
			% DestLabel from Blockmap, though only while
			% processing AdjustedBlock.
			map__delete(Blockmap, DestLabel, CrippledBlockmap),
			jumpopt__instr_list(AdjustedBlock, comment(""),
				Instrmap, CrippledBlockmap, Lvalmap, Procmap,
				Sdprocmap, Forkmap, Succmap, NewInstrs, _),
			RemainInstrs = Instrs0,
			Mod0 = yes
		;
			% Short-circuit the goto.
			map__search(Instrmap, TargetLabel, TargetInstr)
		->
			jumpopt__final_dest(TargetLabel, TargetInstr,
				Instrmap, DestLabel, DestInstr),
			DestInstr = UdestInstr - _Destcomment,
			string__append("shortcircuited jump: ",
				Comment0, Shorted),
			opt_util__can_instr_fall_through(UdestInstr,
				Canfallthrough),
			( Canfallthrough = no ->
				NewInstrs0 = [UdestInstr - Shorted],
				RemainInstrs = Instrs0,
				Mod0 = yes
			;
				( TargetLabel = DestLabel ->
					NewInstrs0 = [Instr0],
					RemainInstrs = Instrs0,
					Mod0 = no
				;
					NewInstrs0 = [goto(label(DestLabel))
						- Shorted],
					RemainInstrs = Instrs0,
					Mod0 = yes
				)
			),
			( map__search(Lvalmap, DestLabel, yes(Lvalinstr)) ->
				jumpopt__adjust_livevals(PrevInstr,
					[Lvalinstr | NewInstrs0], NewInstrs)
			;
				NewInstrs = NewInstrs0
			)
		;
			NewInstrs = [Instr0],
			RemainInstrs = Instrs0,
			Mod0 = no
		)
	; Uinstr0 = computed_goto(Index, LabelList0) ->
		% Short-circuit all the destination labels.
		jumpopt__short_labels(LabelList0, Instrmap, LabelList, Mod0),
		RemainInstrs = Instrs0,
		( Mod0 = yes ->
			string__append(Comment0, " (some shortcircuits)",
				Shorted),
			NewInstrs = [computed_goto(Index, LabelList) - Shorted]
		;
			NewInstrs = [Instr0]
		)
	; Uinstr0 = if_val(Cond, label(TargetLabel)) ->
		(
			% Attempt to transform code such as
			%
			%	if (Cond) L2
			% L1:
			%	goto L3
			% L2:	...
			%
			% into
			%
			%	if (! Cond) L3
			% L2:	...
			%
			% The label L1 may be present or not. If it is present,
			% we are eliminating it, which is possible only because
			% we can short-circuit jumps to it (make them jump
			% directly to L3). This may not be possible if L3 is
			% a non-label code address; e.g. we cannot jump to
			% non-label code addresses from computed gotos.

			opt_util__skip_comments(Instrs0, Instrs1),
			Instrs1 = [Instr1 | Instrs2],
			( Instr1 = label(_) - _ ->
				opt_util__skip_comments(Instrs2, Instrs3),
				Instrs3 = [GotoInstr | AfterGoto],
				HaveLabel = yes
			;
				Instr1 = GotoInstr,
				AfterGoto = Instrs2,
				HaveLabel = no
			),
			GotoInstr = goto(GotoTarget) - GotoComment,
			( HaveLabel = no ; GotoTarget = label(_) ),
			opt_util__skip_comments(AfterGoto, AfterGotoComments),
			AfterGotoComments = [LabelInstr | _],
			LabelInstr = label(TargetLabel) - _
		->
			code_util__neg_rval(Cond, NotCond),
			NewInstr = if_val(NotCond, GotoTarget) - GotoComment,
			NewInstrs = [],
			% The transformed code may fit the pattern again,
			% so make sure that we look for the pattern again
			% by giving all of the transformed instructions to
			% the recursive call. We can't go into an infinite
			% loop because each application of the transformation
			% strictly reduces the size of the code.
			RemainInstrs = [NewInstr | AfterGoto],
			Mod0 = yes
		;
			map__search(Instrmap, TargetLabel, TargetInstr)
		->
			jumpopt__final_dest(TargetLabel, TargetInstr,
				Instrmap, DestLabel, _DestInstr),
			(
				% Attempt to transform code such as
				%
				%	if (Cond) L1
				%	r1 = TRUE
				% 	<epilog>
				%	...
				% L1:
				%	r1 = FALSE
				%	<epilog>
				%
				% into
				%
				%	r1 = Cond
				%	<epilog>
				%

				opt_util__is_sdproceed_next(Instrs0, BetweenFT),
				map__search(Blockmap, DestLabel, Block),
				opt_util__is_sdproceed_next(Block, BetweenBR),
				opt_util__filter_out_r1(BetweenFT,
					yes(SuccessFT), Between),
				opt_util__filter_out_r1(BetweenBR,
					yes(SuccessBR), Between),
				(
					SuccessFT = true,
					SuccessBR = false,
					code_util__neg_rval(Cond, NewCond)
				;
					SuccessFT = false,
					SuccessBR = true,
					NewCond = Cond
				),
				\+ needs_workaround(reg(r, 1), NewCond)
			->
				( NewCond = lval(reg(r, 1)) ->
					NewAssign = comment("r1 = old r1") - ""
				;
					NewAssign = assign(reg(r, 1), NewCond) -
						"shortcircuit bool computation"
				),
				Proceed = goto(succip) - "shortcircuit",
				list__append([NewAssign | Between], [Proceed],
					NewInstrs),
				RemainInstrs = Instrs0,
				Mod0 = yes
			;
				% Try to short-circuit the destination.

				TargetLabel \= DestLabel
			->
				string__append("shortcircuited jump: ",
					Comment0, Shorted),
				NewInstrs = [if_val(Cond, label(DestLabel))
					- Shorted],
				RemainInstrs = Instrs0,
				Mod0 = yes
			;
				NewInstrs = [Instr0],
				RemainInstrs = Instrs0,
				Mod0 = no
			)
		;
			NewInstrs = [Instr0],
			RemainInstrs = Instrs0,
			Mod0 = no
		)
	;
		NewInstrs = [Instr0],
		RemainInstrs = Instrs0,
		Mod0 = no
	),
	( ( Uinstr0 = comment(_) ; NewInstrs = [] ) ->
		NewPrevInstr = PrevInstr
	;
		NewPrevInstr = Uinstr0
	),
	jumpopt__instr_list(RemainInstrs, NewPrevInstr, Instrmap, Blockmap,
		Lvalmap, Procmap, Sdprocmap, Forkmap, Succmap, Instrs9, Mod1),
	list__append(NewInstrs, Instrs9, Instrs),
	( Mod0 = no, Mod1 = no ->
		Mod = no
	;
		Mod = yes
	).

% We avoid generating statements that redefine the value of a location
% by comparing its old contents for non-equality with zero.
%
% The reason is that code such as r1 = !r1 causes gcc 2.7 on SPARCs to
% abort with an internal error.
%
% Apparently this is the only place where the Mercury compiler generates
% assignments like that, otherwise we might need a more general work-around
% that worked for code generated by other parts of the compiler as well.
%
% (It is likely that the problem would occur if bool_not was ever inlined
% into a procedure where the value being complemented was already known to
% be false.)

:- pred needs_workaround(lval, rval).
:- mode needs_workaround(in, in) is semidet.

needs_workaround(Lval, Cond) :-
	(
		Cond = unop(not, lval(Lval))
	;
		Cond = binop(Op, Left, Right),
		( Op = eq ; Op = ne ),
		(
			Right = lval(Lval),
			( Left = const(int_const(0))
			; Left = mkword(0, unop(mkbody, const(int_const(0))))
			)
		;
			Left = lval(Lval),
			( Right = const(int_const(0))
			; Right = mkword(0, unop(mkbody, const(int_const(0))))
			)
		)
	).

:- pred jumpopt__adjust_livevals(instr, list(instruction), list(instruction)).
% :- mode jumpopt__adjust_livevals(in, di, uo) is det.
:- mode jumpopt__adjust_livevals(in, in, out) is det.

jumpopt__adjust_livevals(PrevInstr, Instrs0, Instrs) :-
	(
		PrevInstr = livevals(PrevLivevals),
		opt_util__skip_comments(Instrs0, Instrs1),
		Instrs1 = [livevals(BetweenLivevals) - _ | Instrs2]
	->
		( BetweenLivevals = PrevLivevals ->
			Instrs = Instrs2
		;
			error("betweenLivevals and prevLivevals differ in jumpopt")
		)
	;
		Instrs = Instrs0
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

:- pred jumpopt__final_dest(label, instruction, instrmap, label, instruction).
:- mode jumpopt__final_dest(in, in, in, out, out) is det.

:- pred jumpopt__final_dest_2(label, instruction, instrmap, list(label),
	label, instruction).
:- mode jumpopt__final_dest_2(in, in, in, in, out, out) is det.

jumpopt__final_dest(SrcLabel, SrcInstr, Instrmap, DestLabel, DestInstr) :-
	jumpopt__final_dest_2(SrcLabel, SrcInstr, Instrmap, [],
		DestLabel, DestInstr).

jumpopt__final_dest_2(SrcLabel, SrcInstr, Instrmap, LabelsSofar,
		DestLabel, DestInstr) :-
	(
		SrcInstr = SrcUinstr - _Comment,
		(
			SrcUinstr = goto(label(TargetLabel))
		;
			SrcUinstr = label(TargetLabel)
		),
		map__search(Instrmap, TargetLabel, TargetInstr),
		\+ list__member(SrcLabel, LabelsSofar)
	->
		jumpopt__final_dest_2(TargetLabel, TargetInstr, Instrmap,
			[SrcLabel | LabelsSofar], DestLabel, DestInstr)
	;
		DestLabel = SrcLabel,
		DestInstr = SrcInstr
	).

%-----------------------------------------------------------------------------%
