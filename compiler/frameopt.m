%-----------------------------------------------------------------------------%

% Frameopt.nl - optimization of detstack frame manipulation code.

% Main author: zs.

%-----------------------------------------------------------------------------%

:- module frameopt.

:- interface.
:- import_module llds, list.

	% Delay the construction of det stack frames as long as possible,
	% in order to avoid the construction in as many cases as possible.

:- pred frameopt__main(list(instruction), list(instruction), bool).
:- mode frameopt__main(in, out, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module opt_util, code_util, map, set, int, string, require, std_util.

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
	% At the end, we test whether the procedure ever restores succip;
	% if not, we delete any speculative saves we introduced.

frameopt__main(Instrs0, Instrs, Mod) :-
	opt_util__gather_comments(Instrs0, Comment1, Instrs1),
	(
		Instrs1 = [Instr1prime | Instrs2prime],
		Instr1prime = label(FirstLabelPrime) - _
	->
		Instr1 = Instr1prime,
		Instrs2 = Instrs2prime,
		FirstLabel = FirstLabelPrime,
		( FirstLabel = exported(ProcLabelPrime) ->
			ProcLabel = ProcLabelPrime
		; FirstLabel = local(ProcLabelPrime) ->
			ProcLabel = ProcLabelPrime
		;
			error("procedure begins with bad label type")
		)
	;
		error("procedure does not begin with label")
	),
	opt_util__gather_comments(Instrs2, Comment2, Instrs3),
	(
		frameopt__detstack_setup(Instrs3, FrameSize, Body0)
	->
		set__init(FrameSet0),
		set__init(SuccipSet0),
		frameopt__repeat_build_sets(Body0, FrameSize,
			FrameSet0, FrameSet, SuccipSet0, SuccipSet),
		opt_util__new_label_no(Instrs0, 1000, N0),
		map__init(TeardownMap0),
		frameopt__dup_teardown_labels(Body0, FrameSize,
			TeardownMap0, TeardownMap, ProcLabel, N0, N1, Extra),
		frameopt__doit(Body0, FrameSize, yes, no, no,
			FrameSet, SuccipSet, TeardownMap,
			ProcLabel, N1, _, Body1),
		list__append(Body1, Extra, Body2),
		list__condense([Comment1, [Instr1], Comment2, Body2], Instrs4),
		( Instrs4 = Instrs0 ->
			Instrs = Instrs0,
			Mod = no
		; frameopt__is_succip_restored(Instrs4) ->
			Instrs = Instrs4,
			Mod = yes
		;
			frameopt__dont_save_succip(Instrs4, Instrs),
			Mod = yes
		)
	;
		Instrs = Instrs0,
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

:- pred frameopt__repeat_build_sets(list(instruction), int,
	set(label), set(label), set(label), set(label)).
:- mode frameopt__repeat_build_sets(in, in, in, out, in, out) is det.

frameopt__repeat_build_sets(Instrs0, FrameSize,
		FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	frameopt__build_sets(Instrs0, FrameSize, yes, no, no,
		FrameSet0, FrameSet1, SuccipSet0, SuccipSet1),
	(
		set__equal(FrameSet0, FrameSet1),
		set__equal(SuccipSet0, SuccipSet1)
	->
		FrameSet = FrameSet1,
		SuccipSet = SuccipSet1
	;
		frameopt__repeat_build_sets(Instrs0, FrameSize,
			FrameSet1, FrameSet, SuccipSet1, SuccipSet)
	).

	% For each label, find out whether succip has been saved and whether
	% a det stack frame has been set up by the time control arrives at the
	% label.

	% It is CRITICAL that the information gathered by build_sets
	% be based on exactly the set of optimizations applied by doit.

:- pred frameopt__build_sets(list(instruction), int, bool, bool, bool,
	set(label), set(label), set(label), set(label)).
:- mode frameopt__build_sets(in, in, in, in, in, in, out, in, out) is det.

frameopt__build_sets([], _, _, _, _, FrameSet, FrameSet, SuccipSet, SuccipSet).
frameopt__build_sets([Instr0 | Instrs0], FrameSize, First, SetupFrame0,
		SetupSuccip0, FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	% write(Instr0),
	% nl,
	(
		frameopt__detstack_teardown([Instr0 | Instrs0],
			FrameSize, _Tail, _Teardown, _Goto, After)
	->
		frameopt__build_sets(After, FrameSize, yes, no, no,
			FrameSet0, FrameSet, SuccipSet0, SuccipSet)
	;
		Instr0 = Uinstr0 - _,
		(
			Uinstr0 = comment(_),
			frameopt__build_sets(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = livevals(_),
			frameopt__build_sets(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			% We assume that blocks always end with an instruction
			% that cannot fall through. At the moment only value
			% numbering creates blocks, and it establishes this
			% invariant.
			Uinstr0 = block(_, BlockInstrs),
			frameopt__build_sets(BlockInstrs, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet1, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, yes, no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = assign(Lval, Rval),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Rval, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = call(_, ReturnAddr, _, _),
			frameopt__targeting_code_addr(ReturnAddr,
				yes, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(ReturnAddr,
				yes, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, yes, no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = call_closure(_, ReturnAddr, _),
			frameopt__targeting_code_addr(ReturnAddr,
				yes, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(ReturnAddr,
				yes, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, yes, no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = mkframe(_, _, _),
			error("mkframe in frameopt__build_sets")
		;
			Uinstr0 = modframe(Target),
			frameopt__targeting_code_addr(Target,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(Target,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = label(Label),
			frameopt__setup_label_use(Label,
				SetupFrame0, SetupFrame1,
				FrameSet0, FrameSet1),
			frameopt__setup_label_use(Label,
				SetupSuccip0, SetupSuccip1,
				SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize,
				First, SetupFrame1, SetupSuccip1,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = goto(Target),
			frameopt__targeting_code_addr(Target,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_code_addr(Target,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, yes, no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = computed_goto(_, Labels),
			frameopt__targeting_labels(Labels,
				SetupFrame0, FrameSet0, FrameSet1),
			frameopt__targeting_labels(Labels,
				SetupSuccip0, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize, yes, no, no,
				FrameSet1, FrameSet, SuccipSet1, SuccipSet)
		;
			Uinstr0 = c_code(_),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = if_val(Rval, Target),
			frameopt__setup_if(Rval, Target, Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = incr_hp(Lval, _, Size),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Size, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = mark_hp(Lval),
			opt_util__lval_refers_stackvars(Lval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = restore_hp(Rval),
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			Uinstr0 = incr_sp(_),
			error("incr_sp in frameopt__build_sets")
		;
			Uinstr0 = decr_sp(_),
			error("decr_sp in frameopt__build_sets")
		)
	).

%-----------------------------------------------------------------------------%

	% The decisions taken here must be paralleled in the actions
	% taken in generate_if.

:- pred frameopt__setup_if(rval, code_addr, list(instruction), int,
	bool, bool, bool, set(label), set(label), set(label), set(label)).
:- mode frameopt__setup_if(in, in, in, in, in, in, in, in, out, in, out) is det.

frameopt__setup_if(Rval, Target, Instrs0, FrameSize,
		First, SetupFrame0, SetupSuccip0,
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
			% If we get here, then generate_if will be move the
			% stack teardown code before the if, since the stack
			% frame is not needed in either continuation.
			frameopt__build_sets(After, FrameSize, yes, no, no,
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
			frameopt__build_sets(Instrs0, FrameSize, no, yes, yes,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1a),
			% As a speculation, save the succip if doing so costs
			% only a delay slot whose cost is incurred anyway.
			% The condition should observe whether succip is
			% needed at Target. However, we now gather only
			% "have" information, not "need" information.
			( SetupSuccip1a = no, First = yes ->
				SetupSuccip1 = yes
			;
				SetupSuccip1 = SetupSuccip1a
			),
			frameopt__targeting_label(Label,
				SetupFrame1, FrameSet0, FrameSet1),
			frameopt__targeting_label(Label,
				SetupSuccip1, SuccipSet0, SuccipSet1),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
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
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	;
		Target = do_succeed,
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
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
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
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	).

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

%-----------------------------------------------------------------------------%

:- pred frameopt__dup_teardown_labels(list(instruction),
	int, map(label, label), map(label, label),
	proc_label, int, int, list(instruction)).
:- mode frameopt__dup_teardown_labels(in, in, in, out, in, in, out, out) is det.

frameopt__dup_teardown_labels([], _, TeardownMap, TeardownMap, _, N, N, []).
frameopt__dup_teardown_labels([Instr0 | Instrs0], FrameSize,
		TeardownMap0, TeardownMap, ProcLabel, N0, N, Extra) :-
	(
		Instr0 = label(Label) - _,
		frameopt__detstack_teardown(Instrs0,
			FrameSize, Tail, _Teardown, Goto, After)
	->
		N1 is N0 + 1,
		NewLabel = local(ProcLabel, N0),
		NewLabelInstr = label(NewLabel) - "non-teardown parallel label",
		list__condense([[NewLabelInstr], Tail, [Goto]], Extra1),
		map__set(TeardownMap0, Label, NewLabel, TeardownMap1),
		frameopt__dup_teardown_labels(After, FrameSize,
			TeardownMap1, TeardownMap, ProcLabel, N1, N, Extra2),
		list__append(Extra1, Extra2, Extra)
	;
		frameopt__dup_teardown_labels(Instrs0, FrameSize,
			TeardownMap0, TeardownMap, ProcLabel, N0, N, Extra)
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__doit(list(instruction), int, bool, bool, bool,
	set(label), set(label), map(label, label),
	proc_label, int, int, list(instruction)).
:- mode frameopt__doit(in, in, in, in, in, in, in, in, in, in, out, out) is det.

frameopt__doit([], _, _, _, _, _, _, _, _, N, N, []).
frameopt__doit([Instr0 | Instrs0], FrameSize, First, SetupFrame0, SetupSuccip0,
		FrameSet, SuccipSet, TeardownMap, ProcLabel, N0, N, Instrs) :-
	(
		frameopt__detstack_teardown([Instr0 | Instrs0],
			FrameSize, Tail, Teardown, Goto, After)
	->
		frameopt__doit(After, FrameSize, yes, no, no,
			FrameSet, SuccipSet, TeardownMap,
			ProcLabel, N0, N, Instrs1),
		( SetupFrame0 = yes ->
			list__condense([Tail, Teardown, [Goto], Instrs1],
				Instrs)
		;
			list__condense([Tail, [Goto], Instrs1], Instrs)
		)
	;
		Instr0 = Uinstr0 - Comment,
		(
			Uinstr0 = comment(_),
			frameopt__doit(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = livevals(_),
			frameopt__doit(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = block(Temps, BlockInstrs),
			frameopt__doit(BlockInstrs, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N1, Instrs1),
			frameopt__doit(Instrs0, FrameSize, yes, no, no,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N1, N, Instrs2),
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
				SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = call(_, _, _, _),
			frameopt__generate_setup(SetupFrame0, yes,
				SetupSuccip0, yes, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize, yes, no, no,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = call_closure(_, _, _),
			frameopt__generate_setup(SetupFrame0, yes,
				SetupSuccip0, yes, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize, yes, no, no,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = mkframe(_, _, _),
			error("mkframe in frameopt__doit")
		;
			Uinstr0 = modframe(_),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = label(Label),
			set__is_member(Label, FrameSet, SetupFrame1),
			set__is_member(Label, SuccipSet, SetupSuccip1),
			( First = no ->
				frameopt__generate_setup(SetupFrame0, SetupFrame1,
					SetupSuccip0, SetupSuccip1, FrameSize, SetupCode)
			;
				SetupCode = []
			),
			frameopt__doit(Instrs0, FrameSize,
				First, SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = goto(TargetAddr),
			( TargetAddr = label(Label) ->
				set__is_member(Label, FrameSet, SetupFrame1),
				set__is_member(Label, SuccipSet, SetupSuccip1),
				frameopt__generate_setup(SetupFrame0, SetupFrame1,
					SetupSuccip0, SetupSuccip1, FrameSize, SetupCode)
			;
				SetupCode = []
			),
			frameopt__doit(Instrs0, FrameSize, yes, no, no,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = computed_goto(Rval, Labels),
			frameopt__generate_labels(Labels, SetupFrame0, SetupSuccip0,
				FrameSize, FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N1, NewLabels, SetupCodes),
			frameopt__doit(Instrs0, FrameSize, yes, no, no,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N1, N, Instrs1),
			list__condense([[computed_goto(Rval, NewLabels)
				- Comment], SetupCodes, Instrs1], Instrs)
		;
			Uinstr0 = c_code(_),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			Uinstr0 = if_val(Rval, CodeAddr),
			frameopt__generate_if(Rval, CodeAddr, Comment, Instrs0,
				FrameSize, First, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap, ProcLabel,
				N0, N, Instrs)
		;
			Uinstr0 = incr_hp(Lval, _, Size),
			opt_util__lval_refers_stackvars(Lval, Use1),
			opt_util__rval_refers_stackvars(Size, Use2),
			bool__or(Use1, Use2, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = mark_hp(Lval),
			opt_util__lval_refers_stackvars(Lval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = restore_hp(Rval),
			opt_util__rval_refers_stackvars(Rval, Use),
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			list__append(SetupCode, [Instr0 | Instrs1], Instrs)
		;
			Uinstr0 = incr_sp(_),
			error("incr_sp in frameopt__doit")
		;
			Uinstr0 = decr_sp(_),
			error("decr_sp in frameopt__doit")
		)
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__generate_if(rval, code_addr, string, list(instruction), int,
	bool, bool, bool, set(label), set(label), map(label, label),
	proc_label, int, int, list(instruction)).
:- mode frameopt__generate_if(in, in, in, in, in, in, in, in, in, in, in, in,
	in, out, out) is det.

frameopt__generate_if(Rval, CodeAddr, Comment, Instrs0, FrameSize,
		First, SetupFrame0, SetupSuccip0, FrameSet, SuccipSet,
		TeardownMap, ProcLabel, N0, N, Instrs) :-
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
		frameopt__doit(After, FrameSize, yes, no, no,
			FrameSet, SuccipSet, TeardownMap,
			ProcLabel, N0, N, Instrs1),
		list__condense([Teardown, [Instr1], Tail, [Goto], Instrs1],
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
		frameopt__doit(Instrs0, FrameSize, no, yes, yes,
			FrameSet, SuccipSet, TeardownMap,
			ProcLabel, N0, N, Instrs1),
		list__condense([SetupCode, [Instr0], Instrs1], Instrs)
	;
		% set up a frame if needed for the condition
		frameopt__setup_use(Use,
			SetupFrame0, SetupFrame1,
			SetupSuccip0, SetupSuccip1a),
		( SetupSuccip1a = no, First = yes ->
			SetupSuccip1 = yes
		;
			SetupSuccip1 = SetupSuccip1a
		),
		frameopt__generate_setup(SetupFrame0, SetupFrame1,
			SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
		(
			% see if we can avoid setting up a frame
			% for the target label
			CodeAddr = label(Label),
			SetupFrame1 = no,
			map__search(TeardownMap, Label, Label1)
		->
			string__append(Comment, " (teardown redirect)",
				Comment1),
			N1 = N0,
			IfCode = [
				if_val(Rval, label(Label1)) - Comment1
			]
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
				IfCode = [Instr0]
			;
				N1 is N0 + 1,
				NewLabel = local(ProcLabel, N0),
				code_util__neg_rval(Rval, Neg),
				list__condense([
					[
						if_val(Neg, label(NewLabel))
							- "jump around setup"
					],
					ExtraCode,
					[
						goto(label(Label))
							- "branch after setup",
						label(NewLabel)
							- "fallthrough"
					]
				], IfCode)
			)
		;
			N1 = N0,
			IfCode = [Instr0]
		),
		% Peek ahead to see if the following block requires a frame.
		% If it does, put the setup code immediately after the if.
		% This will be faster because the sp won't be assigned to
		% just before it is referenced by a detstackvar.
		(
			opt_util__block_refers_stackvars(Instrs0, yes),
			\+ frameopt__detstack_teardown(Instrs0, FrameSize,
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
		frameopt__doit(Instrs0, FrameSize,
			no, SetupFrame2, SetupSuccip2,
			FrameSet, SuccipSet, TeardownMap,
			ProcLabel, N1, N, Instrs1),
		list__condense([SetupCode, IfCode, PostSetupCode, Instrs1],
			Instrs)
	).

	% For a given label, return a label (the same or another) that
	% does the same things and does not need or want a frame on arrival.

:- pred frameopt__label_without_frame(label, set(label),
	map(label, label), label).
:- mode frameopt__label_without_frame(in, in, in, out) is semidet.

frameopt__label_without_frame(Label0, FrameSet, TeardownMap, Label) :-
	( set__member(Label0, FrameSet) ->
		map__search(TeardownMap, Label0, Label)
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
	set(label), set(label), map(label, label),
	proc_label, int, int, list(label), list(instruction)).
:- mode frameopt__generate_labels(in, in, in, in, in, in, in,
	in, in, out, out, out) is det.

frameopt__generate_labels([], _, _, _, _, _, _, _, N, N, [], []).
frameopt__generate_labels([Label | Labels], SetupFrame0, SetupSuccip0,
		FrameSize, FrameSet, SuccipSet, TeardownMap, ProcLabel, N0, N,
		[NewLabel | NewLabels], SetupCodes) :-
	frameopt__generate_labels(Labels, SetupFrame0, SetupSuccip0,
		FrameSize, FrameSet, SuccipSet,
		TeardownMap, ProcLabel, N0, N1, NewLabels, SetupCodes1),
	(
		SetupFrame0 = no,
		map__search(TeardownMap, Label, TeardownLabel)
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
				goto(label(Label))
					- "cross the bridge"
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
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
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

:- pred frameopt__detstack_teardown(list(instruction), int,
	list(instruction), list(instruction), instruction, list(instruction)).
:- mode frameopt__detstack_teardown(in, in, out, out, out, out) is semidet.

frameopt__detstack_teardown(Instrs0, FrameSize, Tail, Teardown, Goto, Remain) :-
	frameopt__detstack_teardown_2(Instrs0, FrameSize, [], [], [],
		Tail, Teardown, Goto, Remain).

:- pred frameopt__detstack_teardown_2(list(instruction), int,
	list(instruction), list(instruction), list(instruction),
	list(instruction), list(instruction), instruction, list(instruction)).
:- mode frameopt__detstack_teardown_2(in, in, in, in, in, out, out, out, out)
	is semidet.

frameopt__detstack_teardown_2(Instrs0, FrameSize,
		SeenSuccip0, SeenDecrsp0, SeenExtra0,
		Tail, Teardown, Goto, Remain) :-
	opt_util__skip_comments_livevals(Instrs0, Instrs1),
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
				Tail, Teardown, Goto, Remain)
		;
			opt_util__lval_refers_stackvars(Lval, no),
			opt_util__rval_refers_stackvars(Rval, no),
			list__append(SeenExtra0, [Instr1], SeenExtra1),
			frameopt__detstack_teardown_2(Instrs2, FrameSize,
				SeenSuccip0, SeenDecrsp0, SeenExtra1,
				Tail, Teardown, Goto, Remain)
		)
	;
		Uinstr1 = decr_sp(FrameSize),
		SeenDecrsp0 = [],
		SeenDecrsp1 = [Instr1],
		frameopt__detstack_teardown_2(Instrs2, FrameSize,
			SeenSuccip0, SeenDecrsp1, SeenExtra0,
			Tail, Teardown, Goto, Remain)
	;
		Uinstr1 = goto(_),
		SeenDecrsp0 = [_],
		list__append(SeenSuccip0, SeenDecrsp0, Teardown),
		Tail = SeenExtra0,
		Goto = Instr1,
		Remain = Instrs2
	).

%-----------------------------------------------------------------------------%

	% Find out if succip is ever restored.

:- pred frameopt__is_succip_restored(list(instruction)).
:- mode frameopt__is_succip_restored(in) is semidet.

frameopt__is_succip_restored([Uinstr - _Comment | Instrs]) :-
	(
		Uinstr = assign(succip, lval(stackvar(_)))
	;
		frameopt__is_succip_restored(Instrs)
	).

	% Remove any saves of the succip. Should be called only if succip
	% is never restored.

:- pred frameopt__dont_save_succip(list(instruction), list(instruction)).
:- mode frameopt__dont_save_succip(in, out) is det.

frameopt__dont_save_succip([], []).
frameopt__dont_save_succip([Instr0 | Instrs0], Instrs) :-
	frameopt__dont_save_succip(Instrs0, Instrs1),
	Instr0 = Uinstr - _Comment,
	( Uinstr = assign(stackvar(_), lval(succip)) ->
		Instrs = Instrs1
	;
		Instrs = [Instr0 | Instrs1]
	).
