%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Frameopt.m - optimization of detstack frame manipulation code.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module frameopt.

:- interface.
:- import_module bool, list, map, bimap.
:- import_module llds.

	% Delay the construction of det stack frames as long as possible,
	% in order to avoid the construction in as many cases as possible.

:- pred frameopt__main(list(instruction), list(instruction),
	bool, bimap(label, label), bool).
:- mode frameopt__main(in, out, in, out, out) is det.

	% Find out if succip is ever restored.

:- pred frameopt__is_succip_restored(list(instruction)).
:- mode frameopt__is_succip_restored(in) is semidet.

	% Remove any saves of the succip. Should be called only if succip
	% is never restored. Note that due to fulljump optimization, there
	% may be several copies of the save.

:- pred frameopt__dont_save_succip(list(instruction), list(instruction)).
:- mode frameopt__dont_save_succip(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module peephole, livemap, opt_util, code_util, opt_debug.
:- import_module map, set, int, string, require, std_util, assoc_list.

	% The first part of this code steps over the procedure prolog.
	% The second part starts by checking for the start of a detstack-using
	% procedure (the only kind we optimize here). It then initializes the
	% two main data structures of this optimization, the sets containing
	% the labels on arriving at which (a) a stack frame has been set up
	% and (b) the succip has been saved. The former implies the latter
	% but not vice versa. For labelled straight-line code sequences that
	% tear down the stack frame we create parallels without stack teardown
	% code. We do this in the hope that code branching to this label that
	% would otherwise have to set up a stack frame just so that it can be
	% destroyed can now branch to the parallel code sequence instead.

	% We return the map between nonteardown/teardown labels for use in
	% peepholing. Caution: this information is valid only until the next
	% invocation of labelopt.

	% At the end, after another round of the other optimizations (including
	% peepholes that may eliminate decr_sp/incr_sp pairs) we test whether
	% the procedure ever restores succip; if not, we delete any speculative
	% saves we introduced.

frameopt__main(Instrs0, Instrs, FillDelaySlot, TeardownMap, Mod) :-
	opt_util__get_prologue(Instrs0, ProcLabel, Comments, Instrs2),
	(
		Instrs2 = [LabelInstr | Instrs3],
		frameopt__detstack_setup(Instrs3, FrameSize, Body0),
		livemap__build(Instrs0, no, Livemap)
	->
		set__init(FrameSet0),
		set__init(SuccipSet0),
		frameopt__repeat_build_sets(Body0, FrameSize, Livemap,
			FillDelaySlot, FrameSet0, FrameSet,
			SuccipSet0, SuccipSet),
		opt_util__new_label_no(Instrs0, 1000, N0),
		bimap__init(TeardownMap0),
		frameopt__dup_teardown_labels(Body0, FrameSize, FrameSet,
			TeardownMap0, TeardownMap, ProcLabel, N0, N1, Extra),
		map__init(InsertMap0),
		frameopt__doit(Body0, FrameSize, [], no, no,
			FrameSet, SuccipSet, Livemap, TeardownMap,
			InsertMap0, InsertMap, ProcLabel, FillDelaySlot,
			N1, _, Body1),
		frameopt__insert_late_setups(Body1, InsertMap, comment(""),
			Body2),
		list__append(Body2, Extra, Body3),
		list__append(Comments, [LabelInstr | Body3], Instrs4),
		( Instrs4 = Instrs0 ->
			Instrs = Instrs0,
			Mod = no
		;
			% optional debugging code
			% bimap__to_assoc_list(TeardownMap, LabelPairList),
			% opt_debug__dump_label_pairs(LabelPairList, DebugMsg),
			% DebugInstr = comment(DebugMsg) - "",
			% Instrs = [DebugInstr | Instrs4],
			Instrs = Instrs4,
			Mod = yes
		)
	;
		Instrs = Instrs0,
		bimap__init(TeardownMap),
		Mod = no
	).

%-----------------------------------------------------------------------------%

	% For each label, find out whether succip has been saved and whether
	% a det stack frame has been set up by the time control arrives at the
	% label. If any path to a label answers yes to either question, the
	% other paths must also take the same action before arriving at the
	% label. Since this action may influence the answers at other labels
	% reachable from this new action, we must repeat the basic step until
	% we get a fixpoint.

:- pred frameopt__repeat_build_sets(list(instruction), int, livemap, bool,
	set(label), set(label), set(label), set(label)).
:- mode frameopt__repeat_build_sets(in, in, in, in, in, out, in, out) is det.

frameopt__repeat_build_sets(Instrs0, FrameSize, Livemap, FDS,
		FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS, [], no, no,
		FrameSet0, FrameSet1, SuccipSet0, SuccipSet1),
	(
		set__equal(FrameSet0, FrameSet1),
		set__equal(SuccipSet0, SuccipSet1)
	->
		FrameSet = FrameSet1,
		SuccipSet = SuccipSet1
	;
		frameopt__repeat_build_sets(Instrs0, FrameSize, Livemap,
			FDS, FrameSet1, FrameSet, SuccipSet1, SuccipSet)
	).

	% For each label, find out whether succip has been saved and whether
	% a det stack frame has been set up by the time control arrives at the
	% label.

	% It is CRITICAL that the information gathered by build_sets
	% be based on exactly the set of optimizations applied by doit.

:- pred frameopt__build_sets(list(instruction), int, livemap, bool,
	list(instruction), bool, bool, set(label), set(label),
	set(label), set(label)).
:- mode frameopt__build_sets(in, in, in, in, in, in, in, in, out, in, out)
	is det.

frameopt__build_sets([], _, _, _, _, _, _,
		FrameSet, FrameSet, SuccipSet, SuccipSet).
frameopt__build_sets([Instr0 | Instrs0], FrameSize, Livemap, FDS,
		PrevInstrs, SetupFrame0, SetupSuccip0,
		FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	(
		frameopt__detstack_teardown([Instr0 | Instrs0],
			FrameSize, _Tail, _Teardown, _Goto, After)
	->
		frameopt__build_sets(After, FrameSize, Livemap, FDS, [], no, no,
			FrameSet0, FrameSet, SuccipSet0, SuccipSet)
	;
		Instr0 = Uinstr0 - _,
		(
			Uinstr0 = comment(_),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, PrevInstrs, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = livevals(_),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, PrevInstrs, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			% We assume that blocks always end with an instruction
			% that cannot fall through. At the moment only value
			% numbering creates blocks, and it establishes this
			% invariant.
			Uinstr0 = block(_, BlockInstrs),
			frameopt__build_sets(BlockInstrs, FrameSize, Livemap,
				FDS, PrevInstrs, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet1, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [Instr0], no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = assign(Lval, Rval),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Rval, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = call(_, ReturnAddr, _, _),
			frameopt__targeting_code_addr(ReturnAddr,
				yes, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(ReturnAddr,
				yes, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = mkframe(_, _, Target),
			frameopt__targeting_code_addr(Target,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(Target,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [Instr0], SetupFrame0, SetupSuccip0,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = modframe(Target),
			frameopt__targeting_code_addr(Target,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(Target,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [Instr0], SetupFrame0, SetupSuccip0,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = label(Label),
			frameopt__setup_label_use(Label,
				SetupFrame0, SetupFrame1,
				FrameSet0, FrameSet1),
			frameopt__setup_label_use(Label,
				SetupSuccip0, SetupSuccip1,
				SuccipSet0, SuccipSet1),
			frameopt__setup_liveval_use(Label, Livemap,
				FrameSet1, FrameSet2),
			frameopt__setup_liveval_use(Label, Livemap,
				SuccipSet1, SuccipSet2),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], SetupFrame1, SetupSuccip1,
				FrameSet2, FrameSet, SuccipSet2, SuccipSet)
		;
			Uinstr0 = goto(Target),
			frameopt__targeting_code_addr(Target,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(Target,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = computed_goto(_, Labels),
			frameopt__targeting_labels(Labels,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_labels(Labels,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = c_code(_),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = if_val(Rval, Target),
			frameopt__setup_if(Rval, Target, Instrs0, FrameSize,
				Livemap, FDS, PrevInstrs,
				SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = incr_hp(Lval, _, Size),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Size, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = mark_hp(Lval),
			opt_util__lval_refers_stackvars(Lval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = restore_hp(Rval),
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)	
		;
			Uinstr0 = store_ticket(Lval),
			opt_util__lval_refers_stackvars(Lval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = restore_ticket(Rval),
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap, FDS,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = discard_ticket,
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = incr_sp(_),
			error("incr_sp in frameopt__build_sets")
		;
			Uinstr0 = decr_sp(N),
			(
				opt_util__skip_comments(Instrs0, Instrs1),
				Instrs1 = [incr_sp(N) - _ | Instrs2]
			->
				% This can happen when jumpopt copies the
				% procedure prolog from a tailcall.
				frameopt__build_sets(Instrs2, FrameSize,
					Livemap, FDS, PrevInstrs,
					SetupFrame0, SetupSuccip0,
					FrameSet0, FrameSet,
					SuccipSet0, SuccipSet)
			;
				error("decr_sp in frameopt__build_sets")
			)
		;
			Uinstr0 = pragma_c(_, _, _, _),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FDS, [], SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	).

%-----------------------------------------------------------------------------%

	% The decisions taken here must be paralleled in the actions
	% taken in generate_if.

:- pred frameopt__setup_if(rval, code_addr, list(instruction), int, livemap,
	bool, list(instruction), bool, bool, set(label), set(label),
	set(label), set(label)).
:- mode frameopt__setup_if(in, in, in, in, in, in, in, in, in, in, out, in, out)
	is det.

frameopt__setup_if(Rval, Target, Instrs0, FrameSize, Livemap, FillDelaySlot,
		PrevInstrs, SetupFrame0, SetupSuccip0,
		FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	(
		Target = label(Label),
		opt_util__rval_refers_stackvars(Rval, Use),
		(
			SetupFrame0 = yes,
			Use = no,
			set__is_member(Label, FrameSet0, no),
			frameopt__detstack_teardown(Instrs0,
				FrameSize, _Tail, _Teardown, _Goto, After)
		->
			% If we get here, then generate_if will move the
			% stack teardown code before the if, since the stack
			% frame is not needed in either continuation.
			frameopt__build_sets(After, FrameSize, Livemap,
				FillDelaySlot, [], no, no,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			SetupFrame0 = no,
			Use = no,
			set__is_member(Label, FrameSet0, yes),
			opt_util__block_refers_stackvars(Instrs0, yes)
		->
			% If we get here, then we will need a stack frame
			% soon after the if in both continuations, so it is
			% better to set up the frame before the if.
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], yes, yes,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1a),
			% If the if_val is not preceded by other code that
			% is independent of the condition, attempt to fill in
			% the delay slot another way. We want to use the
			% instruction following the if_val if possible,
			% but if it isn't, we save the succip there
			% if it hasn't been saved earlier. We do this
			% even if the succip is not needed at the target,
			% since we now gather only "have" information,
			% not "need" information.
			(
				FillDelaySlot = yes,
				\+ frameopt__prev_instrs_fill_slot(Rval,
					PrevInstrs),
				frameopt__delay_slot(Instrs0, Rval, Label,
					Livemap, _DelaySlot, Instrs1Prime)
			->
				SetupSuccip1 = SetupSuccip1a,
				Instrs1 = Instrs1Prime
			;
				FillDelaySlot = yes,
				\+ frameopt__prev_instrs_fill_slot(Rval,
					PrevInstrs),
				SetupSuccip1a = no
			->
				SetupSuccip1 = yes,
				Instrs1 = Instrs0
			;
				SetupSuccip1 = SetupSuccip1a,
				Instrs1 = Instrs0
			),
			frameopt__targeting_label(Label,
				SetupFrame1, FrameSet0, FrameSet1),
			frameopt__targeting_label(Label,
				SetupSuccip1, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs1, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		)
	;
		Target = imported(_),
		error("imported label in frameopt__setup_if")
	;
		Target = succip,
		( SetupFrame0 = yes ->
			error("proceed without teardown in frameopt__setup_if")
		;
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	;
		Target = do_succeed(_),
		error("succeed in frameopt__setup_if")
	;
		Target = do_redo,
		( SetupFrame0 = no ->
			error("redo without stack frame in frameopt__setup_if")
		;
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	;
		Target = do_fail,
		( SetupFrame0 = no ->
			error("fail without stack frame in frameopt__setup_if")
		;
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	;
		Target = do_det_closure,
		( SetupFrame0 = yes ->
			error("det_closure without teardown in frameopt__setup_if")
		;
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	;
		Target = do_semidet_closure,
		( SetupFrame0 = yes ->
			error("det_closure without teardown in frameopt__setup_if")
		;
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	;
		Target = do_nondet_closure,
		( SetupFrame0 = yes ->
			error("det_closure without teardown in frameopt__setup_if")
		;
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize, Livemap,
				FillDelaySlot, [], SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	).

% The delay slot instr should be a single machine instruction. It should not
% refer to fields, since the if may test the tag of the base pointer, and
% it should not refer to stack vars since we may not have a stack yet.

:- pred frameopt__delay_slot(list(instruction), rval, label, livemap,
	instruction, list(instruction)).
% :- mode frameopt__delay_slot(di, in, in, in, uo, uo) is semidet.
:- mode frameopt__delay_slot(in, in, in, in, out, out) is semidet.

frameopt__delay_slot(Instrs0, Cond, Label, Livemap, DelaySlot, Instrs) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [DelaySlot | Instrs],
	DelaySlot = assign(Lval, Rval) - _,
	Lval = reg(_),
	(
		Rval = lval(reg(_))
	;
		Rval = binop(Binop, lval(reg(_)), lval(reg(_))),
		frameopt__single_instruction_binop(Binop, yes)
	;
		Rval = unop(Unop, lval(reg(_))),
		frameopt__single_instruction_unop(Unop, yes)
	),
	map__lookup(Livemap, Label, Liveset),
	\+ set__member(Lval, Liveset),
	opt_util__rval_free_of_lval(Cond, Lval).

:- pred frameopt__prev_instrs_fill_slot(rval, list(instruction)).
:- mode frameopt__prev_instrs_fill_slot(in, in) is semidet.

frameopt__prev_instrs_fill_slot(TestRval, PrevInstrs) :-
	opt_util__lvals_in_rval(TestRval, TestLvals),
	frameopt__prev_instrs_fill_slot_2(TestLvals, PrevInstrs).

:- pred frameopt__prev_instrs_fill_slot_2(list(lval), list(instruction)).
:- mode frameopt__prev_instrs_fill_slot_2(in, in) is semidet.

frameopt__prev_instrs_fill_slot_2(UsedLvals0, [Instr0 | Instrs]) :-
	Instr0 = Uinstr0 - _,
	(
		Uinstr0 = assign(Lval, Rval),
		( list__member(Lval, UsedLvals0) ->
			opt_util__lvals_in_rval(Rval, AssignLvals),
			list__append(UsedLvals0, AssignLvals, UsedLvals1),
			frameopt__prev_instrs_fill_slot_2(UsedLvals1, Instrs)
		;
			true
		)
	;
		Uinstr0 = mkframe(_, _, _)
	;
		Uinstr0 = modframe(_)
	;
		Uinstr0 = incr_sp(_)
	;
		Uinstr0 = decr_sp(_)
	).	

%-----------------------------------------------------------------------------%

:- pred frameopt__setup_label_use(label, bool, bool, set(label), set(label)).
:- mode frameopt__setup_label_use(in, in, out, in, out) is det.

frameopt__setup_label_use(Label, Setup0, Setup1, Set0, Set1) :-
	( Setup0 = yes ->
		Setup1 = Setup0,
		( set__member(Label, Set0) ->
			Set1 = Set0
		;
			set__insert(Set0, Label, Set1)
		)
	;
		Set1 = Set0,
		( set__member(Label, Set0) ->
			Setup1 = yes
		;
			Setup1 = no
		)
	).

:- pred frameopt__setup_use(bool, bool, bool, bool, bool).
:- mode frameopt__setup_use(in, in, out, in, out) is det.

frameopt__setup_use(Use,
		SetupFrame0, SetupFrame, SetupSuccip0, SetupSuccip) :-
	( Use = yes ->
		SetupFrame  = yes,
		SetupSuccip = yes
	;
		SetupFrame  = SetupFrame0,
		SetupSuccip = SetupSuccip0
	).

:- pred frameopt__targeting_code_addr(code_addr, bool, set(label), set(label)).
:- mode frameopt__targeting_code_addr(in, in, in, out) is det.

frameopt__targeting_code_addr(CodeAddr, Setup, Set0, Set1) :-
	( CodeAddr = label(Label) ->
		frameopt__targeting_label(Label, Setup, Set0, Set1)
	;
		Set1 = Set0
	).

:- pred frameopt__targeting_label(label, bool, set(label), set(label)).
:- mode frameopt__targeting_label(in, in, in, out) is det.

frameopt__targeting_label(Label, Setup, Set0, Set1) :-
	( Setup = yes ->
		( set__member(Label, Set0) ->
			Set1 = Set0
		;
			set__insert(Set0, Label, Set1)
		)
	;
		Set1 = Set0
	).

:- pred frameopt__targeting_labels(list(label), bool, set(label), set(label)).
:- mode frameopt__targeting_labels(in, in, in, out) is det.

frameopt__targeting_labels(Labels, Setup, Set0, Set1) :-
	( Setup = yes ->
		set__insert_list(Set0, Labels, Set1)
	;
		Set1 = Set0
	).

:- pred frameopt__setup_liveval_use(label, livemap, set(label), set(label)).
% :- mode frameopt__setup_liveval_use(in, in, di, uo) is det.
:- mode frameopt__setup_liveval_use(in, in, in, out) is det.

frameopt__setup_liveval_use(Label, Livemap, Set0, Set) :-
	(
		\+ set__member(Label, Set0),
		map__lookup(Livemap, Label, Livevals),
		set__member(Live, Livevals),
		Live = stackvar(_)
	->
		set__insert(Set0, Label, Set)
	;
		Set = Set0
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__dup_teardown_labels(list(instruction), int, set(label),
	bimap(label, label), bimap(label, label),
	proc_label, int, int, list(instruction)).
:- mode frameopt__dup_teardown_labels(in, in, in, in, out, in, in, out, out)
	is det.

frameopt__dup_teardown_labels([], _, _, TeardownMap, TeardownMap, _, N, N, []).
frameopt__dup_teardown_labels([Instr0 | Instrs0], FrameSize, FrameSet,
		TeardownMap0, TeardownMap, ProcLabel, N0, N, Extra) :-
	(
		Instr0 = label(Label) - _,
		frameopt__detstack_teardown(Instrs0,
			FrameSize, Tail, Teardown, Goto, After)
	->
		N1 is N0 + 1,
		NewLabel = local(ProcLabel, N0),
		NewLabelInstr = label(NewLabel) - "non-teardown parallel label",

		% If Label is in FrameSet, the code following it will not be
		% changed by the rest of frameopt, and we create a parallel
		% code fragment that does not have the teardown action.

		% If Label is not in FrameSet, the code following it WILL be
		% changed by the rest of frameopt to remove the teardown code.
		% The parallel code fragment we create SHOULD have the teardown
		% action. We enter this into TeardownMap in reverse.

		( set__member(Label, FrameSet) ->
			list__condense([[NewLabelInstr], Tail, Goto], Extra1),
			bimap__set(TeardownMap0, Label, NewLabel, TeardownMap1)
		;
			list__condense([[NewLabelInstr], Tail, Teardown, Goto],
				Extra1),
			bimap__set(TeardownMap0, NewLabel, Label, TeardownMap1)
		),
		frameopt__dup_teardown_labels(After, FrameSize, FrameSet,
			TeardownMap1, TeardownMap, ProcLabel, N1, N, Extra2),
		list__append(Extra1, Extra2, Extra)
	;
		frameopt__dup_teardown_labels(Instrs0, FrameSize, FrameSet,
			TeardownMap0, TeardownMap, ProcLabel, N0, N, Extra)
	).

%-----------------------------------------------------------------------------%

:- type insertmap ==	map(label, map(list(instruction), label)).

:- pred frameopt__doit(list(instruction), int, list(instruction), bool, bool,
	set(label), set(label), livemap, bimap(label, label),
	insertmap, insertmap, proc_label, bool, int, int, list(instruction)).
% :- mode frameopt__doit(in, in, in, in, in, in, in, in, in, di, uo,
% 	in, in, out, out) is det.
:- mode frameopt__doit(in, in, in, in, in, in, in, in, in, in, out,
	in, in, in, out, out) is det.

frameopt__doit([], _, _, _, _, _, _, _, _, InsMap, InsMap, _, _, N, N, []).
frameopt__doit([Instr0 | Instrs0], FrameSize, PrevInstrs,
		SetupFrame0, SetupSuccip0, FrameSet, SuccipSet, Livemap,
		TeardownMap, InsertMap0, InsertMap, ProcLabel,
		FDS, N0, N, Instrs) :-
	(
		frameopt__detstack_teardown([Instr0 | Instrs0],
			FrameSize, Tail, Teardown, Goto, After)
	->
		frameopt__doit(After, FrameSize, [], no, no,
			FrameSet, SuccipSet, Livemap, TeardownMap,
			InsertMap0, InsertMap, ProcLabel, FDS, N0, N, Instrs1),
		( SetupFrame0 = yes ->
			list__condense([Tail, Teardown, Goto, Instrs1], Instrs)
		;
			Comment = [comment("teardown omitted here") - ""],
			list__condense([Tail, Comment, Goto, Instrs1], Instrs)
		)
	;
		Instr0 = Uinstr0 - Comment,
		(
			Uinstr0 = comment(_),
			frameopt__doit(Instrs0, FrameSize,
				PrevInstrs, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = livevals(_),
			frameopt__doit(Instrs0, FrameSize,
				PrevInstrs, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = block(Temps, BlockInstrs),
			frameopt__doit(BlockInstrs, FrameSize,
				PrevInstrs, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap1, ProcLabel,
				FDS, N0, N1, Instrs1),
			frameopt__doit(Instrs0, FrameSize, [Instr0], no, no,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap1, InsertMap, ProcLabel,
				FDS, N1, N, Instrs2),
			Instrs = [block(Temps, Instrs1) - Comment | Instrs2]
		;
			Uinstr0 = assign(Lval, Rval),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Rval, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = call(_, _, _, _),
			frameopt__generate_setup(SetupFrame0, yes,
				SetupSuccip0, yes, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize, [], no, no,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = mkframe(_, _, _),
			error("mkframe in frameopt__doit")
		;
			Uinstr0 = modframe(_),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0], SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = label(Label),
			set__is_member(Label, FrameSet, SetupFrame1),
			set__is_member(Label, SuccipSet, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = goto(TargetAddr),
			( TargetAddr = label(Label) ->
				set__is_member(Label, FrameSet, SetupFrame1),
				set__is_member(Label, SuccipSet, SetupSuccip1),
				frameopt__generate_setup(SetupFrame0,
					SetupFrame1,
					SetupSuccip0, SetupSuccip1,
					FrameSize, SetupCode)
			;
				SetupCode = []
			),
			frameopt__doit(Instrs0, FrameSize, [], no, no,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = computed_goto(Rval, Labels),
			frameopt__generate_labels(Labels,
				SetupFrame0, SetupSuccip0, FrameSize,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N1, NewLabels, SetupCodes),
			frameopt__doit(Instrs0, FrameSize, [], no, no,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N1, N, Instrs1),
			list__condense([[computed_goto(Rval, NewLabels)
				- Comment], SetupCodes, Instrs1], Instrs)
		;
			Uinstr0 = c_code(_),
			frameopt__doit(Instrs0, FrameSize,
				[], SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = if_val(Rval, CodeAddr),
			frameopt__generate_if(Rval, CodeAddr, Comment, Instrs0,
				FrameSize, PrevInstrs,
				SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs)
		;
			Uinstr0 = incr_hp(Lval, _, Size),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Size, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = mark_hp(Lval),
			opt_util__lval_refers_stackvars(Lval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = restore_hp(Rval),
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = store_ticket(Lval),
			opt_util__lval_refers_stackvars(Lval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = restore_ticket(Rval),
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1,
				FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				[Instr0|PrevInstrs], SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = discard_ticket,
			frameopt__doit(Instrs0, FrameSize,
				[], SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = incr_sp(_),
			error("incr_sp in frameopt__doit")
		;
			Uinstr0 = decr_sp(_),
			error("decr_sp in frameopt__doit")
		;
			Uinstr0 = pragma_c(_, _, _, _),
			frameopt__doit(Instrs0, FrameSize,
				[], SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, Livemap, TeardownMap,
				InsertMap0, InsertMap, ProcLabel,
				FDS, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		)
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__generate_if(rval, code_addr, string, list(instruction), int,
	list(instruction), bool, bool, set(label), set(label),
	livemap, bimap(label, label), insertmap, insertmap,
	proc_label, bool, int, int, list(instruction)).
% :- mode frameopt__generate_if(in, in, in, in, in, in, in, in, in, in, in, in,
% 	di, uo, in, in, out, out) is det.
:- mode frameopt__generate_if(in, in, in, in, in, in, in, in, in, in, in, in,
	in, out, in, in, in, out, out) is det.

frameopt__generate_if(Rval, CodeAddr, Comment, Instrs0, FrameSize,
		PrevInstrs, SetupFrame0, SetupSuccip0, FrameSet, SuccipSet,
		Livemap, TeardownMap, InsertMap0, InsertMap, ProcLabel,
		FillDelaySlot, N0, N, Instrs) :-
	Instr0 = if_val(Rval, CodeAddr) - Comment,
	opt_util__rval_refers_stackvars(Rval, Use),
	(
		% If we have a frame that is not required in either of the two
		% continuations, remove it before the if_val instruction.
		SetupFrame0 = yes,
		Use = no,
		CodeAddr = label(Label),
		frameopt__label_without_frame(Label,
			FrameSet, TeardownMap, Label1),
		frameopt__detstack_teardown(Instrs0,
			FrameSize, Tail, Teardown, Goto, After)
	->
		( Label1 = Label ->
			Instr1 = Instr0
		;
			string__append(Comment, " (teardown redirect)",
				Comment1),
			Instr1 = if_val(Rval, label(Label1)) - Comment1
		),
		frameopt__doit(After, FrameSize, [], no, no,
			FrameSet, SuccipSet, Livemap, TeardownMap,
			InsertMap0, InsertMap, ProcLabel, FillDelaySlot,
			N0, N, Instrs1),
		list__condense([Teardown, [Instr1], Tail, Goto, Instrs1],
			Instrs)
	;
		% If both continuations require a stack frame, set it up
		% before the if_val instruction.
		SetupFrame0 = no,
		Use = no,
		CodeAddr = label(Label),
		\+ frameopt__label_without_frame(Label,
			FrameSet, TeardownMap, _),
		opt_util__block_refers_stackvars(Instrs0, yes),
		\+ frameopt__detstack_teardown(Instrs0, FrameSize, _, _, _, _)
	->
		frameopt__generate_setup(SetupFrame0, yes,
			SetupSuccip0, yes, FrameSize, SetupCode),
		frameopt__doit(Instrs0, FrameSize, [], yes, yes,
			FrameSet, SuccipSet, Livemap, TeardownMap,
			InsertMap0, InsertMap, ProcLabel,
			FillDelaySlot, N0, N, Instrs1),
		list__condense([SetupCode, [Instr0], Instrs1], Instrs)
	;
		% set up a frame if needed for the condition
		frameopt__setup_use(Use,
			SetupFrame0, SetupFrame1,
			SetupSuccip0, SetupSuccip1a),
		(
			FillDelaySlot = yes,
			\+ frameopt__prev_instrs_fill_slot(Rval,
				PrevInstrs),
			CodeAddr = label(TargetLabel),
			frameopt__delay_slot(Instrs0, Rval, TargetLabel,
				Livemap, DelaySlotInstr, Instrs1Prime)
		->
			DelayCode = [DelaySlotInstr],
			SetupSuccip1 = SetupSuccip1a,
			Instrs1 = Instrs1Prime
		;
			FillDelaySlot = yes,
			\+ frameopt__prev_instrs_fill_slot(Rval,
				PrevInstrs),
			SetupSuccip1a = no
		->
			DelayCode = [],
			SetupSuccip1 = yes,
			Instrs1 = Instrs0
		;
			DelayCode = [],
			SetupSuccip1 = SetupSuccip1a,
			Instrs1 = Instrs0
		),
		frameopt__generate_setup(SetupFrame0, SetupFrame1,
			SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
		( SetupCode = [] ->
			StartCode = DelayCode,
			Instrs2 = Instrs1
		;
			StartCode = SetupCode,
			Instrs2 = Instrs0
		),
		(
			% see if we can avoid setting up a frame
			% for the target label
			CodeAddr = label(Label),
			SetupFrame1 = no,
			bimap__search(TeardownMap, Label, Label1)
		->
			string__append(Comment, " (teardown redirect)",
				Comment1),
			N1 = N0,
			IfCode = [
				if_val(Rval, label(Label1)) - Comment1
			],
			InsertMap1 = InsertMap0
		;
			CodeAddr = label(Label)
		->
			% set up a frame if needed for the target label
			set__is_member(Label, FrameSet, SetupFrameSide),
			set__is_member(Label, SuccipSet, SetupSuccipSide),
			frameopt__generate_setup(SetupFrame1, SetupFrameSide,
				SetupSuccip1, SetupSuccipSide, FrameSize, ExtraCode),
			( ExtraCode = [] ->
				N1 = N0,
				IfCode = [Instr0],
				InsertMap1 = InsertMap0
			;
				( map__search(InsertMap0, Label, Insert0) ->
					( map__search(Insert0, ExtraCode, OldLabel) ->
						N1 = N0,
						NewLabel = OldLabel,
						InsertMap1 = InsertMap0
					;
						N1 is N0 + 1,
						NewLabel = local(ProcLabel, N0),
						map__det_insert(Insert0, ExtraCode, NewLabel, Insert),
						map__set(InsertMap0, Label, Insert, InsertMap1)
					)
				;
					N1 is N0 + 1,
					NewLabel = local(ProcLabel, N0),
					map__init(Insert0),
					map__det_insert(Insert0, ExtraCode, NewLabel, Insert),
					map__set(InsertMap0, Label, Insert, InsertMap1)
				),
				IfCode = [
					if_val(Rval, label(NewLabel))
					- "jump to setup"
				]
			)
		;
			N1 = N0,
			IfCode = [Instr0],
			InsertMap1 = InsertMap0
		),
		% Peek ahead to see if the following block requires a frame.
		% If it does, put the setup code immediately after the if.
		% This will be faster because the sp won't be assigned to
		% just before it is referenced by a detstackvar.
		(
			opt_util__block_refers_stackvars(Instrs2, yes),
			\+ frameopt__detstack_teardown(Instrs2, FrameSize,
				_, _, _, _)
		->
			SetupFrame2 = yes,
			SetupSuccip2 = yes,
			frameopt__generate_setup(SetupFrame1, SetupFrame2,
				SetupSuccip1, SetupSuccip2, FrameSize, PostSetupCode)
		;
			SetupFrame2 = SetupFrame1,
			SetupSuccip2 = SetupSuccip1,
			PostSetupCode = []
		),
		frameopt__doit(Instrs2, FrameSize,
			[], SetupFrame2, SetupSuccip2,
			FrameSet, SuccipSet, Livemap, TeardownMap,
			InsertMap1, InsertMap, ProcLabel,
			FillDelaySlot, N1, N, Instrs3),
		list__condense([StartCode, IfCode, PostSetupCode, Instrs3],
			Instrs)
	).

	% For a given label, return a label (the same or another) that
	% does the same things and does not need or want a frame on arrival.

:- pred frameopt__label_without_frame(label, set(label), bimap(label, label),
	label).
:- mode frameopt__label_without_frame(in, in, in, out) is semidet.

frameopt__label_without_frame(Label0, FrameSet, TeardownMap, Label) :-
	( set__member(Label0, FrameSet) ->
		bimap__search(TeardownMap, Label0, Label)
	;
		Label = Label0
	).

	% This predicate is given the current state of the stack frame
	% and the desired state, and returns the instructions needed to
	% transform the former into the latter.

:- pred frameopt__generate_setup(bool, bool, bool, bool, int,
	list(instruction)).
:- mode frameopt__generate_setup(in, in, in, in, in, out) is det.

frameopt__generate_setup(SetupFrame0, SetupFrame, SetupSuccip0, SetupSuccip,
		FrameSize, SetupCode) :-
	( SetupFrame = yes, SetupSuccip = no ->
		error("requirement for frame without succip in generate_setup")
	;
		true
	),
	( SetupFrame0 = yes, SetupSuccip0 = no ->
		error("existing frame without succip in generate_setup")
	;
		true
	),
	( SetupFrame = yes ->
		( SetupFrame0 = yes ->
			SetupCode = []
		; SetupSuccip0 = yes ->
			SetupCode = [
				incr_sp(FrameSize)
					- "late setup after succip"
			]
		;
			SetupCode = [
				incr_sp(FrameSize)
					- "late setup",
				assign(stackvar(FrameSize), lval(succip))
					- "late save"
			]
		)
	; SetupSuccip = yes ->
		( SetupSuccip0 = yes ->
			SetupCode = []
		;
			SetupCode = [
				assign(stackvar(0), lval(succip))
					- "late save"
			]
		)
	;
		SetupCode = []
	).

	% Rewrite the label list of a computed goto. If there is no
	% stack frame at the point of the goto, then either redirect
	% any stack-needing labels to their teardown equivalents,
	% or if that cannot be done, generate code to do the setup,
	% and branch to the real label through this code.

:- pred frameopt__generate_labels(list(label), bool, bool, int,
	set(label), set(label), bimap(label, label),
	proc_label, int, int, list(label), list(instruction)).
:- mode frameopt__generate_labels(in, in, in, in, in, in, in,
	in, in, out, out, out) is det.

frameopt__generate_labels([], _, _, _, _, _, _, _, N, N, [], []).
frameopt__generate_labels([Label | Labels], SetupFrame0, SetupSuccip0,
		FrameSize, FrameSet, SuccipSet, TeardownMap,
		ProcLabel, N0, N, [NewLabel | NewLabels], SetupCodes) :-
	frameopt__generate_labels(Labels, SetupFrame0, SetupSuccip0,
		FrameSize, FrameSet, SuccipSet, TeardownMap,
		ProcLabel, N0, N1, NewLabels, SetupCodes1),
	(
		SetupFrame0 = no,
		bimap__search(TeardownMap, Label, TeardownLabel)
	->
		N = N1,
		NewLabel = TeardownLabel,
		SetupCodes = SetupCodes1
	;
		set__is_member(Label, FrameSet, SetupFrame1),
		set__is_member(Label, SuccipSet, SetupSuccip1),
		frameopt__generate_setup(SetupFrame0, SetupFrame1,
			SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
		( SetupCode = [] ->
			N = N1,
			NewLabel = Label,
			SetupCodes = SetupCodes1
		;
			N is N1 + 1,
			NewLabel = local(ProcLabel, N1),
			LabelCode = [
				label(NewLabel)
					- "setup bridging label"
			],
			GotoCode = [
				goto(label(Label)) - "cross the bridge"
			],
			list__condense([LabelCode, SetupCode, GotoCode, SetupCodes1],
				SetupCodes)
		)
	).

%-----------------------------------------------------------------------------%

	% If the following code a setup of a det stack frame? If yes, return
	% the size of the frame and the remaining instructions.

:- pred frameopt__detstack_setup(list(instruction), int, list(instruction)).
:- mode frameopt__detstack_setup(in, out, out) is semidet.

frameopt__detstack_setup(Instrs0, FrameSize, Instrs) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = incr_sp(FrameSize) - _,
	frameopt__detstack_setup_2(Instrs2, FrameSize, Instrs).

:- pred frameopt__detstack_setup_2(list(instruction), int, list(instruction)).
:- mode frameopt__detstack_setup_2(in, in, out) is semidet.

frameopt__detstack_setup_2([Instr0 | Instrs0], FrameSize, Instrs) :-
	( Instr0 = assign(stackvar(FrameSize), lval(succip)) - _ ->
		Instrs = Instrs0
	; Instr0 = assign(_, _) - _ ->
		frameopt__detstack_setup_2(Instrs0, FrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	; Instr0 = comment(_) - _ ->
		frameopt__detstack_setup_2(Instrs0, FrameSize, Instrs1),
		Instrs = [Instr0 | Instrs1]
	;
		fail
	).

	% Is the following code a teardown of a det stack frame, including
	% possibly a semidet assignment to r1 and a proceed or tailcall?
	% Return the teardown instructions, the non-stack instructions
	% (possible assignment to r1 etc), the branch away and the instructions
	% remaining after that.

	% We are looking for the teardown components in any order, since
	% value numbering may change the original order.

:- pred frameopt__detstack_teardown(list(instruction), int, list(instruction),
	list(instruction), list(instruction), list(instruction)).
:- mode frameopt__detstack_teardown(in, in, out, out, out, out) is semidet.

frameopt__detstack_teardown(Instrs0, FrameSize, Tail, Teardown, Goto, Remain) :-
	frameopt__detstack_teardown_2(Instrs0, FrameSize, [], [], [], [],
		Tail, Teardown, Goto, Remain).

:- pred frameopt__detstack_teardown_2(list(instruction), int,
	list(instruction), list(instruction), list(instruction),
	list(instruction), list(instruction), list(instruction),
	list(instruction), list(instruction)).
:- mode frameopt__detstack_teardown_2(in, in, in, in, in, in,
	out, out, out, out) is semidet.

frameopt__detstack_teardown_2(Instrs0, FrameSize,
		SeenSuccip0, SeenDecrsp0, SeenExtra0, SeenLivevals0,
		Tail, Teardown, Goto, Remain) :-
	opt_util__skip_comments(Instrs0, Instrs1),
	Instrs1 = [Instr1 | Instrs2],
	Instr1 = Uinstr1 - _,
	(
		Uinstr1 = assign(Lval, Rval),
		(
			Lval = succip,
			Rval = lval(stackvar(FrameSize))
		->
			SeenSuccip0 = [],
			SeenDecrsp0 = [],
			SeenSuccip1 = [Instr1],
			frameopt__detstack_teardown_2(Instrs2, FrameSize,
				SeenSuccip1, SeenDecrsp0, SeenExtra0,
				SeenLivevals0, Tail, Teardown, Goto, Remain)
		;
			opt_util__lval_refers_stackvars(Lval, no),
			opt_util__rval_refers_stackvars(Rval, no),
			list__append(SeenExtra0, [Instr1], SeenExtra1),
			frameopt__detstack_teardown_2(Instrs2, FrameSize,
				SeenSuccip0, SeenDecrsp0, SeenExtra1,
				SeenLivevals0, Tail, Teardown, Goto, Remain)
		)
	;
		Uinstr1 = decr_sp(FrameSize),
		SeenDecrsp0 = [],
		SeenDecrsp1 = [Instr1],
		frameopt__detstack_teardown_2(Instrs2, FrameSize,
			SeenSuccip0, SeenDecrsp1, SeenExtra0, SeenLivevals0,
			Tail, Teardown, Goto, Remain)
	;
		Uinstr1 = livevals(_),
		SeenLivevals0 = [],
		SeenLivevals1 = [Instr1],
		frameopt__detstack_teardown_2(Instrs2, FrameSize,
			SeenSuccip0, SeenDecrsp0, SeenExtra0, SeenLivevals1,
			Tail, Teardown, Goto, Remain)
	;
		Uinstr1 = goto(_),
		SeenDecrsp0 = [_],
		list__append(SeenSuccip0, SeenDecrsp0, Teardown),
		Tail = SeenExtra0,
		list__append(SeenLivevals0, [Instr1], Goto),
		Remain = Instrs2
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__insert_late_setups(list(instruction), insertmap, instr,
	list(instruction)).
% :- mode frameopt__insert_late_setups(di, in, in, uo) is det.
:- mode frameopt__insert_late_setups(in, in, in, out) is det.

frameopt__insert_late_setups([], _, _, []).
frameopt__insert_late_setups([Instr0 | Instrs0], InsertMap, Prev, Instrs) :-
	Instr0 = Uinstr0 - _Comment0,
	frameopt__insert_late_setups(Instrs0, InsertMap, Uinstr0, Instrs1),
	(
		Uinstr0 = label(Label),
		map__search(InsertMap, Label, Insert)
	->
		opt_util__can_instr_fall_through(Prev, FallThrough),
		(
			FallThrough = yes,
			Guard = [goto(label(Label)) - "jump around setup"]
		;
			FallThrough = no,
			Guard = []
		),
		map__to_assoc_list(Insert, InsertList),
		frameopt__insert_late_setups_list(InsertList, Label, SetupCode),
		list__condense([Guard, SetupCode, [Instr0], Instrs1],  Instrs)
	;
		Instrs = [Instr0 | Instrs1]
	).

:- pred frameopt__insert_late_setups_list(assoc_list(list(instruction), label),
	label, list(instruction)).
:- mode frameopt__insert_late_setups_list(in, in, out) is det.

frameopt__insert_late_setups_list([], _, []).
frameopt__insert_late_setups_list([SetupCode0 - Label | Inserts], OrigLabel,
		SetupCode) :-
	frameopt__insert_late_setups_list(Inserts, OrigLabel, SetupCode1),
	( SetupCode1 = [] ->
		JumpAround = []
	;
		JumpAround = [goto(label(OrigLabel)) - "jump around next setup"]
	),
	LabelInstr = label(Label) - "label for late setup code",
	list__condense([[LabelInstr | SetupCode0], JumpAround, SetupCode1],
		SetupCode).

%-----------------------------------------------------------------------------%

:- pred frameopt__single_instruction_unop(unary_op, bool).
:- mode frameopt__single_instruction_unop(in, out) is det.

frameopt__single_instruction_unop(mktag, yes).
frameopt__single_instruction_unop(tag, yes).
frameopt__single_instruction_unop(unmktag, yes).
frameopt__single_instruction_unop(mkbody, yes).
frameopt__single_instruction_unop(body, yes).
frameopt__single_instruction_unop(unmkbody, yes).
frameopt__single_instruction_unop(cast_to_unsigned, yes).
frameopt__single_instruction_unop(hash_string, no).
frameopt__single_instruction_unop(bitwise_complement, yes).
frameopt__single_instruction_unop((not), no).

:- pred frameopt__single_instruction_binop(binary_op, bool).
:- mode frameopt__single_instruction_binop(in, out) is det.

frameopt__single_instruction_binop((+), yes).
frameopt__single_instruction_binop((-), yes).
frameopt__single_instruction_binop((*), no).
frameopt__single_instruction_binop((/), no).
frameopt__single_instruction_binop((mod), no).
frameopt__single_instruction_binop((<<), yes).
frameopt__single_instruction_binop((>>), yes).
frameopt__single_instruction_binop((&), yes).
frameopt__single_instruction_binop(('|'), yes).
frameopt__single_instruction_binop((^), yes).
frameopt__single_instruction_binop((and), no).
frameopt__single_instruction_binop((or), no).
frameopt__single_instruction_binop(eq, no).
frameopt__single_instruction_binop(ne, no).
frameopt__single_instruction_binop(array_index, yes).
frameopt__single_instruction_binop(str_eq, no).
frameopt__single_instruction_binop(str_ne, no).
frameopt__single_instruction_binop(str_lt, no).
frameopt__single_instruction_binop(str_gt, no).
frameopt__single_instruction_binop(str_le, no).
frameopt__single_instruction_binop(str_ge, no).
frameopt__single_instruction_binop((<), no).
frameopt__single_instruction_binop((>), no).
frameopt__single_instruction_binop((<=), no).
frameopt__single_instruction_binop((>=), no).
frameopt__single_instruction_binop(float_plus, no).
frameopt__single_instruction_binop(float_minus, no).
frameopt__single_instruction_binop(float_times, no).
frameopt__single_instruction_binop(float_divide, no).
frameopt__single_instruction_binop(float_eq, no).
frameopt__single_instruction_binop(float_ne, no).
frameopt__single_instruction_binop(float_lt, no).
frameopt__single_instruction_binop(float_gt, no).
frameopt__single_instruction_binop(float_le, no).
frameopt__single_instruction_binop(float_ge, no).

%-----------------------------------------------------------------------------%

frameopt__is_succip_restored([Uinstr - _Comment | Instrs]) :-
	(
		Uinstr = assign(succip, lval(stackvar(_)))
	;
		Uinstr = block(_, BlockInstrs),
		frameopt__is_succip_restored(BlockInstrs)
	;
		frameopt__is_succip_restored(Instrs)
	).

frameopt__dont_save_succip([], []).
frameopt__dont_save_succip([Instr0 | Instrs0], Instrs) :-
	frameopt__dont_save_succip(Instrs0, Instrs1),
	Instr0 = Uinstr - _Comment,
	( Uinstr = assign(stackvar(_), lval(succip)) ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).
