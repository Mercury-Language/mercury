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
		opt_util__detstack_setup(Instrs3, FrameSize, Body0)
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

:- pred frameopt__build_sets(list(instruction), int, bool, bool, bool,
	set(label), set(label), set(label), set(label)).
:- mode frameopt__build_sets(in, in, in, in, in, in, out, in, out) is det.

frameopt__build_sets([], _, _, _, _, FrameSet, FrameSet, SuccipSet, SuccipSet).
frameopt__build_sets([Instr0 | Instrs0], FrameSize, First, SetupFrame0,
		SetupSuccip0, FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	(
		opt_util__detstack_teardown([Instr0 | Instrs0],
			FrameSize, _Teardown, _Tail, After)
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
			Uinstr0 = livevals(_, _),
			frameopt__build_sets(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			% we assume that blocks always end with an instruction
			% that cannot fall through
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
			Uinstr0 = call(_, ReturnAddr),
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
			Uinstr0 = modframe(_),
			% the redoip of another frame is being hijacked
			frameopt__build_sets(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
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
			Uinstr0 = incr_sp(_),
			error("incr_sp in frameopt__build_sets")
		;
			Uinstr0 = decr_sp(_),
			error("decr_sp in frameopt__build_sets")
		;
			% XXX possible interaction with garbage collection
			Uinstr0 = incr_hp(_),
			frameopt__build_sets(Instrs0, FrameSize,
				no, SetupFrame0, SetupSuccip0,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		)
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__setup_if(rval, code_addr, list(instruction), int,
	bool, bool, bool, set(label), set(label), set(label), set(label)).
:- mode frameopt__setup_if(in, in, in, in, in, in, in, in, out, in, out) is det.

frameopt__setup_if(Rval, Target, Instrs0, FrameSize,
		First, SetupFrame0, SetupSuccip0,
		FrameSet0, FrameSet, SuccipSet0, SuccipSet) :-
	( Target = label(Label) ->
		opt_util__rval_refers_stackvars(Rval, Use),
		(
			SetupFrame0 = yes,
			Use = no,
			set__is_member(Label, FrameSet0, IsMember),
			IsMember = no,
			opt_util__detstack_teardown(Instrs0,
				FrameSize, _Teardown, _Tail, After)
		->
			frameopt__build_sets(After, FrameSize, yes, no, no,
				FrameSet0, FrameSet, SuccipSet0, SuccipSet)
		;
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1a),
			% XXX this condition should observe
			% whether succip is needed at Target
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
		error("non-label target in frameopt__setup_if")
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
		error("non-label code_addr in frameopt")
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
		opt_util__detstack_teardown(Instrs0,
			FrameSize, _Teardown, Tail, After)
	->
		N1 is N0 + 1,
		NewLabel = local(ProcLabel, N0),
		Extra1 = [
			label(NewLabel) - "non-teardown parallel label"
			| Tail
		],
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
		opt_util__detstack_teardown([Instr0 | Instrs0],
			FrameSize, Teardown, Tail, After)
	->
		frameopt__doit(After, FrameSize, yes, no, no,
			FrameSet, SuccipSet, TeardownMap,
			ProcLabel, N0, N, Instrs1),
		( SetupFrame0 = yes ->
			list__condense([Teardown, Tail, Instrs1], Instrs)
		;
			list__append(Tail, Instrs1, Instrs)
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
			Uinstr0 = livevals(_, _),
			frameopt__doit(Instrs0, FrameSize,
				First, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		;
			% we assume that blocks always end with an instruction
			% that cannot fall through
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
			Uinstr0 = call(_, _),
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
				set__is_member(Label, SuccipSet, SetupSuccip1)
			;
				error("non-label target in frameopt__doit")
			),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
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
			Uinstr0 = if_val(_, _),
			frameopt__generate_if(Instr0, Instrs0, FrameSize, First,
				SetupFrame0, SetupSuccip0, FrameSet, SuccipSet,
				TeardownMap, ProcLabel, N0, N, Instrs)
		;
			Uinstr0 = incr_sp(_),
			error("incr_sp in frameopt__doit")
		;
			Uinstr0 = decr_sp(_),
			error("decr_sp in frameopt__doit")
		;
			% XXX possible interaction with garbage collection
			Uinstr0 = incr_hp(_),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame0, SetupSuccip0,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			Instrs = [Instr0 | Instrs1]
		)
	).

%-----------------------------------------------------------------------------%

:- pred frameopt__label_without_frame(label, set(label),
	map(label, label), label).
:- mode frameopt__label_without_frame(in, in, in, out) is semidet.

frameopt__label_without_frame(Label0, FrameSet, TeardownMap, Label) :-
	( set__member(Label0, FrameSet) ->
		map__search(TeardownMap, Label0, Label)
	;
		Label = Label0
	).

:- pred frameopt__generate_if(instruction, list(instruction), int,
	bool, bool, bool, set(label), set(label), map(label, label),
	proc_label, int, int, list(instruction)).
:- mode frameopt__generate_if(in, in, in, in, in, in, in, in, in, in,
	in, out, out) is det.

frameopt__generate_if(Instr0, Instrs0, FrameSize, First, SetupFrame0,
		SetupSuccip0, FrameSet, SuccipSet, TeardownMap,
		ProcLabel, N0, N, Instrs) :-
	( Instr0 = if_val(Rval, label(Label)) - Comment ->
		opt_util__rval_refers_stackvars(Rval, Use),
		(
			% if we have a frame that is not required
			% in either continuation, remove it before
			% the if_val instruction.
			SetupFrame0 = yes,
			Use = no,
			frameopt__label_without_frame(Label,
				FrameSet, TeardownMap, Label1),
			opt_util__detstack_teardown(Instrs0,
				FrameSize, Teardown, Tail, After)
		->
			frameopt__doit(After, FrameSize, yes, no, no,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N0, N, Instrs1),
			( Label1 = Label ->
				Instr1 = Instr0
			;
				string__append(Comment, " (teardown redirect)",
					Comment1),
				Instr1 = if_val(Rval, label(Label1)) - Comment1
			),
			list__condense([Teardown, [Instr1], Tail, Instrs1],
				Instrs)
		;
			% set up a frame if needed for the condition
			frameopt__setup_use(Use,
				SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1a),
			% XXX this condition should observe
			% whether succip is needed at Label
			( SetupSuccip1a = no, First = yes ->
				SetupSuccip1 = yes
			;
				SetupSuccip1 = SetupSuccip1a
			),
			frameopt__generate_setup(SetupFrame0, SetupFrame1,
				SetupSuccip0, SetupSuccip1, FrameSize, SetupCode),
			(
				% see if can avoid setting up a frame
				% for the target label
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
				% set up a frame if needed for the target label
				set__is_member(Label, FrameSet, SetupFrame2),
				set__is_member(Label, SuccipSet, SetupSuccip2),
				frameopt__generate_setup(SetupFrame1, SetupFrame2,
					SetupSuccip1, SetupSuccip2, FrameSize, ExtraCode),
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
			),
			frameopt__doit(Instrs0, FrameSize,
				no, SetupFrame1, SetupSuccip1,
				FrameSet, SuccipSet, TeardownMap,
				ProcLabel, N1, N, Instrs1),
			list__condense([SetupCode, IfCode, Instrs1], Instrs)
		)
	;
		error("instruction other than if_val in frameopt__setup_if")
	).

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
