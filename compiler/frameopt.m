%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: zs.
%
% This module performs two kinds of transformation to optimize code
% that manipulates detstack frames.
%
% The first and more common transformation transforms code such as
%
%	proc_entry:
%		incr_sp(N)
%		detstackvar(N) = succip
%		if (cond) goto L1
%		...
%	L1:	finalization
%		succip = detstackvar(N)
%		incr_sp(N)
%		proceed
%
% into
%
%	proc_entry:
%		if (cond) goto L1
%		incr_sp(N)
%		detstackvar(N) = succip
%		...
%		finalization
%		succip = detstackvar(N)
%		incr_sp(N)
%		proceed
%	L1:	finalization
%		proceed
%
% The advantage is that we don't set up the stack frame unless we need it.
%
% The actual optimization is more complex than this, because we want to delay
% the construction of the stack frame across more than one jump, if this is
% possible.
%
% The second transformation transforms code such as
%
%	proc_entry:
%		incr_sp(N)
%		detstackvar(N) = succip
%		...
%		finalization
%		succip = detstackvar(N)
%		incr_sp(N)
%		goto proc_entry
%
% into
%
%	proc_entry:
%		incr_sp(N)
%		detstackvar(N) = succip
%	L1:
%		...
%		finalization
%		goto L1
%
% The advantage is that we don't destroy the stack frame just to set it up
% again.
%
% Since the second transformation is a bigger win, we prefer to use it
% whenever both transformations are possible.

%-----------------------------------------------------------------------------%

:- module frameopt.

:- interface.

:- import_module llds.
:- import_module bool, list.

	% Delay the construction of det stack frames as long as possible,
	% in order to avoid the construction in as many cases as possible.
	%
	% The first bool output says whether we performed any modifications.
	% If yes, then we also introduced some extra labels that should be
	% deleted. The second says whether we introduced any jumps that
	% can be profitably be short-circuited.

:- pred frameopt__main(list(instruction)::in, list(instruction)::out,
	bool::out, bool::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module livemap, opt_util, code_util, opt_debug.
:- import_module int, string, require, std_util, assoc_list, set, map, queue.

frameopt__main(Instrs0, Instrs, Mod, Jumps) :-
	opt_util__get_prologue(Instrs0, ProcLabel, LabelInstr,
		Comments0, Instrs1),
	(
		frameopt__detstack_setup(Instrs1, FrameSize, Msg, _, _, _)
	->
		opt_util__new_label_no(Instrs0, 1000, N0),
		map__init(BlockMap0),
		divide_into_basic_blocks([LabelInstr | Instrs1], ProcLabel, N0,
			BasicInstrs, N1),
		build_block_map(BasicInstrs, FrameSize, LabelSeq0,
			BlockMap0, BlockMap1, ProcLabel, N1, N2),
		analyze_block_map(LabelSeq0, BlockMap1, BlockMap2, KeepFrame),
		(
			KeepFrame = yes(FirstLabel - SecondLabel),
			keep_frame(LabelSeq0, BlockMap2,
				FirstLabel, SecondLabel, BlockMap),
			LabelSeq = LabelSeq0,
			NewComment = comment("keeping stack frame") - "",
			list__append(Comments0, [NewComment], Comments),
			flatten_block_seq(LabelSeq, BlockMap, BodyInstrs),
			list__append(Comments, BodyInstrs, Instrs),
			Mod = yes,
			Jumps = yes
		;
			KeepFrame = no,
			( can_delay_frame(LabelSeq0, BlockMap2, yes) ->
				delay_frame(LabelSeq0, BlockMap2,
					FrameSize, Msg,
					ProcLabel, N2, LabelSeq, BlockMap),
				NewComment = comment("delaying stack frame")
					- "",
				list__append(Comments0, [NewComment], Comments),
				flatten_block_seq(LabelSeq, BlockMap,
					BodyInstrs),
				list__append(Comments, BodyInstrs, Instrs),
				Mod = yes,
				Jumps = no
			;
				Instrs = Instrs0,
				Mod = no,
				Jumps = no
			)
		)
	;
		Instrs = Instrs0,
		Mod = no,
		Jumps = no
	).

:- pred flatten_block_seq(list(label)::in, block_map::in,
	list(instruction)::out) is det.

flatten_block_seq([], _, []).
flatten_block_seq([Label | Labels], BlockMap, Instrs) :-
	flatten_block_seq(Labels, BlockMap, RestInstrs),
	map__lookup(BlockMap, Label, BlockInfo),
	BlockInfo = block_info(_, BlockInstrs, _, _, _),
	list__append(BlockInstrs, RestInstrs, Instrs).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type block_map	==	map(label, block_info).

:- type block_info	--->	block_info(
					label,
						% The label of the first instr.
					list(instruction),
						% The code of the block.
					list(label),
						% The labels we can jump to
						% (not falling through).
					maybe(label),
						% The label we fall through to
						% (if there is one).
					block_type
				).

:- type block_type	--->	setup		% This is a block containing
						% only setup instructions.
			;	ordinary(bool)	% This block does not contain
						% setup or teardown. The bool
						% says whether the code in the
						% block needs a stack frame.
			;	teardown(list(instruction)).
						% This block contains stack
						% teardown and goto code;
						% the arg gives just the goto
						% code (which should have a
						% label instruction put before
						% it if used).

%-----------------------------------------------------------------------------%

	% Add labels to the given instruction sequence so that
	% every basic block has labels around it.

:- pred divide_into_basic_blocks(list(instruction)::in, proc_label::in, int::in,
	list(instruction)::out, int::out) is det.

divide_into_basic_blocks([], _, N, [], N).
	% Control can fall of the end of a procedure if that procedure
	% ends with a call to another procedure that cannot succeed.
	% This is the only situation in which the base case can be reached.
divide_into_basic_blocks([Instr0 | Instrs0], ProcLabel, N0, Instrs, N) :-
	Instr0 = Uinstr0 - _Comment,
	( opt_util__can_instr_branch_away(Uinstr0, yes) ->
		(
			Instrs0 = [Instr1 | _],
			( Instr1 = label(_) - _ ->
				divide_into_basic_blocks(Instrs0, ProcLabel,
					N0, Instrs1, N),
				Instrs = [Instr0 | Instrs1]
			;
				N1 is N0 + 1,
				NewLabel = local(ProcLabel, N0),
				NewInstr = label(NewLabel) - "",
				divide_into_basic_blocks(Instrs0, ProcLabel,
					N1, Instrs1, N),
				Instrs = [Instr0, NewInstr | Instrs1]
			)
		;
			Instrs0 = [],
			Instrs = [Instr0],
			N = N0
		)
	;
		divide_into_basic_blocks(Instrs0, ProcLabel, N0, Instrs1, N),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

	% Given an instruction list in which labels mark the start of every
	% basic block, divide it up into basic blocks of one of three types:
	%
	% - setup blocks, blocks that contain only stack setup instructions
	%   (incr_sp and assignment of succip to the bottom stack slot);
	%
	% - ordinary blocks that contain neither a stack setup nor a
	%   stack teardown;
	%
	% - teardown blocks that remove an existing stack frame.
	%
	% For such each block, create a block_info structure that gives the
	% label starting the block, the instructions in the block, and its
	% type. Two of the fields of the block_info structure are filled in
	% with dummy values; they will be filled in for real later.
	%
	% Put these block_info structures into a table indexed by the label,
	% and return the sequence of labels of the blocks in their original
	% order.

:- pred build_block_map(list(instruction)::in, int::in, list(label)::out,
	block_map::in, block_map::out, proc_label::in, int::in, int::out)
	is det.

build_block_map([], _, [], BlockMap, BlockMap, _, N, N).
build_block_map([Instr0 | Instrs0], FrameSize, LabelSeq, BlockMap0, BlockMap,
		ProcLabel, N0, N) :-
	( Instr0 = label(Label) - _ ->
		( 
			frameopt__detstack_setup(Instrs0, _, _, Setup,
				Others, Remain)
		->
			% Create a block with just the Setup instructions
			% in it.

			BlockInfo = block_info(Label, [Instr0 | Setup],
				[], no, setup),
			list__append(Others, Remain, Instrs1),
			(
				Instrs1 = [Instr1 | _],
				Instr1 = label(_) - _
			->
				N1 = N0,
				Instrs2 = Instrs1
			;
				N1 is N0 + 1,
				NewLabel = local(ProcLabel, N0),
				NewInstr = label(NewLabel) - "",
				Instrs2 = [NewInstr | Instrs1]
			),
			build_block_map(Instrs2, FrameSize, LabelSeq0,
				BlockMap0, BlockMap1, ProcLabel, N1, N),
			map__det_insert(BlockMap1, Label, BlockInfo, BlockMap),
			LabelSeq = [Label | LabelSeq0]
		;
			frameopt__detstack_teardown(Instrs0, FrameSize,
				Tail, Teardown, Goto, Remain)
		->
			( Tail = [] ->
				MaybeTailInfo = no,
				N1 = N0,
				list__append([Instr0 | Teardown], Goto,
					TeardownBlock),
				TeardownLabel = Label,
				TeardownInfo = block_info(TeardownLabel,
					TeardownBlock, [], no,
					teardown(Goto))
			;
				block_needs_frame(Tail, Needs),
				TailInfo = block_info(Label, [Instr0 | Tail],
					[], no, ordinary(Needs)),
				MaybeTailInfo = yes(TailInfo - Label),
				N1 is N0 + 1,
				NewLabel = local(ProcLabel, N0),
				NewInstr = label(NewLabel) - "",
				list__append([NewInstr | Teardown], Goto,
					TeardownBlock),
				TeardownLabel = NewLabel,
				TeardownInfo = block_info(TeardownLabel,
					TeardownBlock, [], no,
					teardown(Goto))
			),
			build_block_map(Remain, FrameSize, LabelSeq0,
				BlockMap0, BlockMap1, ProcLabel, N1, N),
			(
				MaybeTailInfo = no,
				map__det_insert(BlockMap1, TeardownLabel,
					TeardownInfo, BlockMap),
				LabelSeq = [TeardownLabel | LabelSeq0]
			;
				MaybeTailInfo = yes(TailInfo2 - TailLabel2),
				map__det_insert(BlockMap1, TeardownLabel,
					TeardownInfo, BlockMap2),
				map__det_insert(BlockMap2, TailLabel2,
					TailInfo2, BlockMap),
				LabelSeq = [TailLabel2, TeardownLabel
					| LabelSeq0]
			)
		;
			opt_util__skip_to_next_label(Instrs0, Block, Instrs1),
			block_needs_frame(Block, Needs),
			BlockInfo = block_info(Label, [Instr0 | Block],
				[], no, ordinary(Needs)),
			build_block_map(Instrs1, FrameSize, LabelSeq0,
				BlockMap0, BlockMap1, ProcLabel, N0, N),
			map__det_insert(BlockMap1, Label, BlockInfo, BlockMap),
			LabelSeq = [Label | LabelSeq0]
		)
	;
		error("block does not start with label")
	).

%-----------------------------------------------------------------------------%

	% Does the given code start with a setup of a det stack frame? If yes,
	% return the size of the frame and three instruction sequences,
	% Setup, Others and Remain. Setup is the instruction sequence
	% that sets up the det stack frame, Others is a sequence of
	% non-interfering instructions that were interspersed with Setup
	% but can be moved after Setup, and Remain is all remaining
	% instructions.

:- pred frameopt__detstack_setup(list(instruction)::in, int::out, string::out,
	list(instruction)::out, list(instruction)::out, list(instruction)::out)
	is semidet.

frameopt__detstack_setup(Instrs0, FrameSize, Msg, Setup, Others, Remain) :-
	opt_util__gather_comments(Instrs0, Others0, Instrs1),
	Instrs1 = [SetupInstr1 | Instrs2],
	SetupInstr1 = incr_sp(FrameSize, Msg) - _,
	frameopt__detstack_setup_2(Instrs2, FrameSize, SetupInstr2,
		Others0, Others, Remain),
	Setup = [SetupInstr1, SetupInstr2].

:- pred frameopt__detstack_setup_2(list(instruction)::in, int::in,
	instruction::out, list(instruction)::in, list(instruction)::out,
	list(instruction)::out) is semidet.

frameopt__detstack_setup_2([Instr0 | Instrs0], FrameSize, Setup,
		Others0, Others, Remain) :-
	( Instr0 = assign(Lval, Rval) - _ ->
		(
			Lval = stackvar(FrameSize),
			Rval = lval(succip)
		->
			Others = Others0,
			Setup = Instr0,
			Remain = Instrs0
		;
			Lval \= succip,
			Lval \= stackvar(FrameSize)
		->
			list__append(Others0, [Instr0], Others1),
			frameopt__detstack_setup_2(Instrs0, FrameSize, Setup,
				Others1, Others, Remain)
		;
			fail
		)
	; Instr0 = comment(_) - _ ->
		list__append(Others0, [Instr0], Others1),
		frameopt__detstack_setup_2(Instrs0, FrameSize, Setup,
			Others1, Others, Remain)
	;
		fail
	).

%-----------------------------------------------------------------------------%

	% Does the following block contain a teardown of a det stack frame,
	% and a proceed or tailcall?
	% Return the teardown instructions and the branch away, together with
	% the instructions that either are before the teardown or can be
	% reordered to be before the teardown, and the instructions remaining
	% after all that.

	% We are looking for the teardown components in any order, since
	% value numbering may change the original order. This is also the
	% reason why we allow teardown instructions to be interleaved with
	% instructions that do not access the stack.

:- pred frameopt__detstack_teardown(list(instruction)::in, int::in,
	list(instruction)::out, list(instruction)::out,
	list(instruction)::out, list(instruction)::out) is semidet.

frameopt__detstack_teardown([Instr0 | Instrs0], FrameSize,
		Tail, Teardown, Goto, Remain) :-
	(
		Instr0 = label(_) - _
	->
		fail
	;
		frameopt__detstack_teardown_2([Instr0 | Instrs0], FrameSize,
			[], [], [], [],
			TailPrime, TeardownPrime, GotoPrime, RemainPrime)
	->
		Tail = TailPrime,
		Teardown = TeardownPrime,
		Goto = GotoPrime,
		Remain = RemainPrime
	;
		frameopt__detstack_teardown(Instrs0, FrameSize,
			Tail1, Teardown, Goto, Remain),
		Tail = [Instr0 | Tail1]
	).

:- pred frameopt__detstack_teardown_2(list(instruction)::in, int::in,
	list(instruction)::in, list(instruction)::in, list(instruction)::in,
	list(instruction)::in, list(instruction)::out, list(instruction)::out,
	list(instruction)::out, list(instruction)::out) is semidet.

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

	% Does an ordinary block with the given content need a stack frame?

:- pred block_needs_frame(list(instruction)::in, bool::out) is det.

block_needs_frame(Instrs, NeedsFrame) :-
	opt_util__block_refers_stackvars(Instrs, ReferStackVars),
	( ReferStackVars = yes ->
		NeedsFrame = yes
	;
		(
			list__member(Instr, Instrs),
			Instr = Uinstr - _,
			(
				Uinstr = call(_, _, _, _)
			;
				Uinstr = mkframe(_, _, _)
			;
				Uinstr = c_code(_)
			;
				Uinstr = pragma_c(_, _, _, _, _)
			)
		->
			NeedsFrame = yes
		;
			NeedsFrame = no
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% For each block in the given sequence, whose block_info structures
	% in the given block map have been partially filled in, fill in the
	% remaining two fields. These two fields give the labels the block
	% can branch to on the side (this includes return addresses for
	% calls), and the label if any to which it falls through.
	%
	% Also decide whether the optimization we want to apply is keeping
	% the stack frame for recursive tail calls once it has been set up
	% by the initial entry to the procedure, or delaying the creation of
	% the stack frame as long as possible. We want to do the former
	% whenever we find at least one teardown block that branches back
	% to the beginning of the procedure; in such cases we return the
	% the label starting the procedure and the label that should replace
	% it in tailcalls that avoid the stack teardown, which is the label
	% immediately after the initial stack setup block.

:- pred analyze_block_map(list(label)::in, block_map::in,
	block_map::out, maybe(pair(label))::out) is det.

analyze_block_map(LabelSeq, BlockMap0, BlockMap, KeepFrameData) :-
	(
		LabelSeq = [FirstLabel, SecondLabel | _],
		map__search(BlockMap0, FirstLabel, FirstBlockInfo),
		FirstBlockInfo = block_info(FirstLabel, _, _, _, setup)
	->
		analyze_block_map_2(LabelSeq, BlockMap0, FirstLabel,
			BlockMap, no, KeepFrame),
		( KeepFrame = yes ->
			KeepFrameData = yes(FirstLabel - SecondLabel)
		;
			KeepFrameData = no
		)
	;
		error("bad data in analyze_block_map")
	).

:- pred analyze_block_map_2(list(label)::in, block_map::in, label::in,
	block_map::out, bool::in, bool::out) is det.

analyze_block_map_2([], BlockMap, _, BlockMap, KeepFrame, KeepFrame).
analyze_block_map_2([Label | Labels], BlockMap0, FirstLabel, BlockMap,
		KeepFrame0, KeepFrame) :-
	map__lookup(BlockMap0, Label, BlockInfo0),
	BlockInfo0 = block_info(BlockLabel, BlockInstrs, _, _, Type),
	(
		Label = BlockLabel,	% sanity check
		list__last(BlockInstrs, LastInstr)
	->
		LastInstr = LastUinstr - _,
		possible_targets(LastUinstr, SideLabels),
		(
			opt_util__can_instr_fall_through(LastUinstr, yes),
			Labels = [NextLabel | _]
		->
			MaybeFallThrough = yes(NextLabel)
		;
			MaybeFallThrough = no
		),
		(
			LastUinstr = goto(label(GotoLabel)),
			same_label_ref(FirstLabel, GotoLabel)
		->
			KeepFrame1 = yes
		;
			KeepFrame1 = KeepFrame0
		)
	;
		error("bad data in analyze_block_map_2")
	),
	BlockInfo = block_info(BlockLabel, BlockInstrs, SideLabels,
		MaybeFallThrough, Type),
	map__det_update(BlockMap0, Label, BlockInfo, BlockMap1),
	analyze_block_map_2(Labels, BlockMap1, FirstLabel, BlockMap,
		KeepFrame1, KeepFrame).

	% The form of a label used in a tailcall may be different from
	% the form used in the initial label. The initial label may be
	% exported from the Mercury module or possibly from the C module,
	% while the label used in the tailcall may use a more local form
	% for better performance.
	%
	% This predicate tests whether the second label (from the tailcall)
	% is a proper reference to the first label (from the initial label
	% instruction).

:- pred same_label_ref(label::in, label::in) is semidet.

same_label_ref(exported(ProcLabel), exported(ProcLabel)).
same_label_ref(exported(ProcLabel), c_local(ProcLabel)).
same_label_ref(exported(ProcLabel), local(ProcLabel)).
same_label_ref(local(ProcLabel), c_local(ProcLabel)).
same_label_ref(local(ProcLabel), local(ProcLabel)).
same_label_ref(c_local(ProcLabel), c_local(ProcLabel)).

	% Given an instruction, find the set of labels to which it can cause
	% control to transfer. In the case of calls, this includes transfer
	% via return from the called procedure.

:- pred possible_targets(instr::in, list(label)::out) is det.

possible_targets(comment(_), []).
possible_targets(livevals(_), []).
possible_targets(block(_, _, _), _) :-
	error("block in possible_targets").
possible_targets(assign(_, _), []).
possible_targets(call(_, ReturnAddr, _, _), Labels) :-
	( ReturnAddr = label(Label) ->
		Labels = [Label]
	;
		Labels = []
	).
possible_targets(mkframe(_, _, _), []).
possible_targets(modframe(_), []).
possible_targets(label(_), []).
possible_targets(goto(CodeAddr), Targets) :-
	( CodeAddr = label(Label) ->
		Targets = [Label]
	;
		Targets = []
	).
possible_targets(computed_goto(_, Targets), Targets).
possible_targets(c_code(_), []).
possible_targets(if_val(_, CodeAddr), Targets) :-
	( CodeAddr = label(Label) ->
		Targets = [Label]
	;
		Targets = []
	).
possible_targets(incr_hp(_, _, _), []).
possible_targets(mark_hp(_), []).
possible_targets(restore_hp(_), []).
possible_targets(store_ticket(_), []).
possible_targets(restore_ticket(_), []).
possible_targets(discard_ticket, []).
possible_targets(incr_sp(_, _), []).
possible_targets(decr_sp(_), []).
possible_targets(pragma_c(_, _, _, _, _), []).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Transform the given block sequence to effect the optimization
	% of keeping the stack frame for recursive tail calls once it has
	% been set up by the initial entry to the procedure.
	% The two label arguments are the label starting the procedure
	% (a form of which appears in existing tailcalls) and the label that
	% should replace it in tailcalls that avoid the stack teardown.

:- pred keep_frame(list(label)::in, block_map::in, label::in, label::in,
	block_map::out) is det.

keep_frame([], BlockMap, _FirstLabel, _SecondLabel, BlockMap).
keep_frame([Label | Labels], BlockMap0, FirstLabel, SecondLabel, BlockMap) :-
	map__lookup(BlockMap0, Label, BlockInfo0),
	(
		BlockInfo0 = block_info(Label, Instrs0, [_], no,
			teardown(Instrs1)),
		pick_last(Instrs0, _NonLastInstrs, LastInstr),
		LastInstr = goto(label(GotoLabel)) - Comment,
		same_label_ref(FirstLabel, GotoLabel)
	->
		(
			Instrs0 = [Instr0 | _],
			Instr0 = label(_) - _
		->
			NewLabelInstr = Instr0
		;
			error("block does not begin with label")
		),
		pick_last(Instrs1, NonLastInstrs, _LastInstr),
		string__append(Comment, " (keeping frame)", NewComment),
		NewGoto = goto(label(SecondLabel)) - NewComment,
		list__append([NewLabelInstr | NonLastInstrs], [NewGoto],
			Instrs),
		BlockInfo = block_info(Label, Instrs, [SecondLabel], no,
			ordinary(yes)),
		map__det_update(BlockMap0, Label, BlockInfo, BlockMap1)
	;
		BlockMap1 = BlockMap0
	),
	keep_frame(Labels, BlockMap1, FirstLabel, SecondLabel, BlockMap).

:- pred pick_last(list(T)::in, list(T)::out, T::out) is det.

pick_last([], _, _) :-
	error("empty list in pick_last").
pick_last([First | Rest], NonLast, Last) :-
	( Rest = [] ->
		NonLast = [],
		Last = First
	;
		pick_last(Rest, NonLast0, Last),
		NonLast = [First | NonLast0]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Check that we can use the delay_frame transformation. This requires
	% that only the first block is of the setup type, and that the
	% second block is of the ordinary type. Since the transformation
	% is a null operation if the second block needs a stack frame,
	% we lie a bit and say that the transformation is not applicable
	% in such cases.

:- pred can_delay_frame(list(label)::in, block_map::in, bool::in) is semidet.

can_delay_frame([], _, _).
can_delay_frame([Label | Labels], BlockMap, First) :-
	map__lookup(BlockMap, Label, BlockInfo),
	BlockInfo = block_info(_, _, _, MaybeFallThrough, BlockType),
	( BlockType = setup ->
		First = yes,
		MaybeFallThrough = yes(FallThrough),
		map__lookup(BlockMap, FallThrough, FallThroughBlockInfo),
		FallThroughBlockInfo = block_info(_, _, _, _, FallThroughType),
		FallThroughType = ordinary(no)
	;
		can_delay_frame(Labels, BlockMap, no)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The data structures used in the delaying optimizations:

	% map__search(RevMap, Label, SideLabels) should be true
	% if the block started by Label can be reached via jump
	% (i.e. not fallthrough) from the labels in SideLabels.

:- type rev_map	==	map(label, list(label)).

	% map__search(ParMap, Label, ParallelLabel) should be true if
	% Label starts a teardown block and ParallelLabel starts its parallel
	% (i.e. a copy without the stack teardown code and therefore an
	% ordinary block).

:- type par_map	==	map(label, label).

	% set__member(Label, FallIntoParallel) should be true if
	% Label starts a teardown block and the immediately previous block
	% does not have a stack frame and falls through into the teardown
	% block.
	%
	% If it is true, then we will put the new ParallelLabel block
	% immediately before the Label block; if it is false, we will put it
	% immediately after. (Both teardown blocks and their parallels
	% end with a goto, so the first block cannot fall through to the
	% second, nor can the presence of the second block prevent any
	% fallthrough from the first block to some other block.)

%-----------------------------------------------------------------------------%

	% The optimization of delaying the creation of stack frames as long
	% as possible is in three main phases:
	%
	% -	The first phase finds out which ordinary blocks need a
	%	a stack frame. This naturally includes blocks that access
	%	stackvars, and blocks that perform calls. It also includes
	%	blocks that jump to ordinary blocks that need a frame,
	%	or which are jumped to or fallen through to from ordinary
	%	blocks that need a stack frame. It does not include blocks
	%	that fall through to ordinary blocks that need a frame,
	%	since the frame setup code can be interposed between the
	%	two blocks.
	%	
	% -	The second phase gets rid of the frame setup code in the
	%	initial setup block, but its main task is to transform
	%	ordinary blocks that do not need a frame. Such blocks cannot
	%	directly jump to ordinary blocks that need a frame (if they
	%	could, they would have been marked as needing a frame too),
	%	but they can jump to teardown blocks that also assume the
	%	presence of a frame. Therefore the last instruction in such
	%	blocks, the only one that can jump out of the block, must be
	%	modified to jump to non-teardown parallels to these teardown
	%	blocks. These parallel blocks will be created in the third
	%	phase, but to make the substitution possible we allocate labels
	%	for them in this phase.
	%
	%	We must also correctly process fallthrough from such blocks.
	%	If the block can fall through to a teardown block, we mark
	%	the teardown block so that its parallel will be put before it.
	%	If the block can fall through to an ordinary block that needs
	%	a frame, then we put stack frame setup code between the two
	%	blocks.
	%
	% -	The third phase creates non-teardown parallels to the teardown
	%	blocks that need them, and puts them in their correct place,
	%	either just before or just after the original block.

:- pred delay_frame(list(label)::in, block_map::in, int::in, string::in,
	proc_label::in, int::in, list(label)::out, block_map::out) is det.

delay_frame(LabelSeq0, BlockMap0, FrameSize, Msg, ProcLabel, N0,
		LabelSeq, BlockMap) :-
	map__init(RevMap0),
	queue__init(Queue0),
	delay_frame_init(LabelSeq0, BlockMap0, RevMap0, RevMap,
		Queue0, Queue1),
	set__init(FramedLabels0),
	propagate_framed_labels(Queue1, BlockMap0, RevMap,
		FramedLabels0, FramedLabels),
	map__init(ParMap0),
	set__init(FallIntoParallel0),
	process_frame_delay(LabelSeq0, BlockMap0, ParMap0, FallIntoParallel0,
		FramedLabels, FrameSize, Msg, ProcLabel, N0,
		LabelSeq1, BlockMap1, ParMap, FallIntoParallel),
	create_parallels(LabelSeq1, BlockMap1, ParMap, FallIntoParallel,
		LabelSeq, BlockMap).

%-----------------------------------------------------------------------------%

	% Initialize two data structures for the delaying operation.
	% The first is a map showing the predecessors of each block,
	% i.e. the set of blocks that can jump to each other block.
	% The second is a queue of ordinary blocks that need a stack frame.

:- pred delay_frame_init(list(label)::in, block_map::in,
	rev_map::in, rev_map::out, queue(label)::in, queue(label)::out) is det.

delay_frame_init([], _, RevMap, RevMap, Queue, Queue).
delay_frame_init([Label | Labels], BlockMap, RevMap0, RevMap, Queue0, Queue) :-
	map__lookup(BlockMap, Label, BlockInfo),
	BlockInfo = block_info(_, _, SideLabels, _, BlockType),
	(
		BlockType = setup,
		Queue1 = Queue0
	;
		BlockType = ordinary(NeedsFrame),
		(
			NeedsFrame = no,
			Queue1 = Queue0
		;
			NeedsFrame = yes,
			queue__put(Queue0, Label, Queue1)
		)
	;
		BlockType = teardown(_),
		Queue1 = Queue0
	),
	rev_map_side_labels(SideLabels, Label, RevMap0, RevMap1),
	delay_frame_init(Labels, BlockMap, RevMap1, RevMap, Queue1, Queue).

:- pred rev_map_side_labels(list(label)::in, label::in,
	rev_map::in, rev_map::out) is det.

rev_map_side_labels([], _Label, RevMap, RevMap).
rev_map_side_labels([Label | Labels], SourceLabel, RevMap0, RevMap) :-
	( map__search(RevMap0, Label, OtherSources0) ->
		OtherSources = [SourceLabel | OtherSources0],
		map__det_update(RevMap0, Label, OtherSources, RevMap1)
	;
		OtherSources = [SourceLabel],
		map__det_insert(RevMap0, Label, OtherSources, RevMap1)
	),
	rev_map_side_labels(Labels, SourceLabel, RevMap1, RevMap).

%-----------------------------------------------------------------------------%

	% Given FramedLabels0, a set of labels representing ordinary blocks
	% that must have a stack frame, propagate the requirement for a stack
	% frame to labels representing other ordinary blocks that
	%
	% - are reachable from a block in FramedLabels0, either by jump or
	%   by falling through, or
	%
	% - can perform a jump to a block in FramedLabels0.
	%
	% The requirement is not propagated to blocks that can fall through
	% to a block in FramedLabels0, since on such paths stack frame setup
	% code can be inserted between the two blocks.

:- pred propagate_framed_labels(queue(label)::in, block_map::in, rev_map::in,
	set(label)::in, set(label)::out) is det.

propagate_framed_labels(Queue0, BlockMap, RevMap,
		FramedLabels0, FramedLabels) :-
	( queue__get(Queue0, Label, Queue1) ->
		(
			map__lookup(BlockMap, Label, BlockInfo),
			BlockInfo = block_info(_, _, SideLabels,
				MaybeFallThrough, BlockType),
			BlockType = ordinary(_),
			\+ set__member(Label, FramedLabels0)
		->
			set__insert(FramedLabels0, Label, FramedLabels1),
			(
				MaybeFallThrough = no,
				ReachableLabels = SideLabels
			;
				MaybeFallThrough = yes(FallThrough),
				ReachableLabels = [FallThrough | SideLabels]
			),
			queue__put_list(Queue1, ReachableLabels, Queue2),
			( map__search(RevMap, Label, Sources) ->
				queue__put_list(Queue2, Sources, Queue3)
			;
				Queue3 = Queue2
			),
			propagate_framed_labels(Queue3, BlockMap, RevMap,
				FramedLabels1, FramedLabels)
		;
			propagate_framed_labels(Queue1, BlockMap, RevMap,
				FramedLabels0, FramedLabels)
		)
	;
		FramedLabels = FramedLabels0
	).

%-----------------------------------------------------------------------------%

	% The predicates process_frame_delay and transform_ordinary_block
	% implement the second phase of delay_frame. For documentation,
	% see the comment at the top of delay_frame.

:- pred process_frame_delay(list(label)::in, block_map::in,
	par_map::in, set(label)::in,
	set(label)::in, int::in, string::in, proc_label::in, int::in,
	list(label)::out, block_map::out, par_map::out, set(label)::out) is det.

process_frame_delay([], BlockMap, ParMap, FallIntoParallel, _, _, _, _, _,
		[], BlockMap, ParMap, FallIntoParallel).
process_frame_delay([Label0 | Labels0], BlockMap0, ParMap0, FallIntoParallel0,
		FramedLabels, FrameSize, Msg, ProcLabel, N0,
		Labels, BlockMap, ParMap, FallIntoParallel) :-
	map__lookup(BlockMap0, Label0, BlockInfo0),
	BlockInfo0 = block_info(Label0Copy, Instrs0, SideLabels0,
		MaybeFallThrough0, Type),
	( Label0 = Label0Copy ->
		true
	;
		error("label in block_info is not copy")
	),
	(
		Type = setup,
		(
			MaybeFallThrough0 = yes(FallThrough)
		;
			MaybeFallThrough0 = no,
			error("no fallthrough for setup block")
		),
		( SideLabels0 = [] ->
			true
		;
			error("nonempty side labels for setup block")
		),
		(
			Instrs0 = [LabelInstrPrime | _],
			LabelInstrPrime = label(_) - _
		->
			LabelInstr = LabelInstrPrime
		;
			error("setup block does not begin with label")
		),
		( set__member(FallThrough, FramedLabels) ->
			% we can't delay the frame setup,
			% so return everything unchanged
			Labels = [Label0 | Labels0],
			BlockMap = BlockMap0,
			ParMap = ParMap0,
			FallIntoParallel = FallIntoParallel0
		;
			BlockInfo = block_info(Label0, [LabelInstr],
				SideLabels0, MaybeFallThrough0, ordinary(no)),
			map__det_update(BlockMap0, Label0, BlockInfo,
				BlockMap1),
			process_frame_delay(Labels0, BlockMap1,
				ParMap0, FallIntoParallel0, FramedLabels,
				FrameSize, Msg, ProcLabel, N0,
				Labels1, BlockMap, ParMap, FallIntoParallel),
			Labels = [Label0 | Labels1]
		)
	;
		Type = ordinary(_),
		( set__member(Label0, FramedLabels) ->
			% Every block reachable from this block, whether via
			% jump or fallthrough, will be an ordinary block also
			% in FramedLabels, or will be a teardown block.
			% We already have a stack frame, and all our
			% successors expect one, so we need not do anything.
			process_frame_delay(Labels0, BlockMap0,
				ParMap0, FallIntoParallel0, FramedLabels,
				FrameSize, Msg, ProcLabel, N0,
				Labels1, BlockMap, ParMap, FallIntoParallel),
			Labels = [Label0 | Labels1]
		;
			% Every block reachable from this block, whether via
			% jump or fallthrough, will be an ordinary block also
			% not in FramedLabels, or will be a teardown block.
			% The ordinary blocks are OK, since we don't have a
			% stack frame and they don't expect one exither, but
			% the teardown blocks are a different matter; we must
			% make sure that we reach their non-teardown parallels
			% instead.
			transform_ordinary_block(Label0, Labels0, BlockInfo0,
				BlockMap0, ParMap0, FallIntoParallel0,
				FramedLabels, FrameSize, Msg, ProcLabel, N0,
				Labels, BlockMap, ParMap, FallIntoParallel)
		)
	;
		Type = teardown(_),
		process_frame_delay(Labels0, BlockMap0,
			ParMap0, FallIntoParallel0, FramedLabels,
			FrameSize, Msg, ProcLabel, N0,
			Labels1, BlockMap, ParMap, FallIntoParallel),
		Labels = [Label0 | Labels1]
	).

:- pred transform_ordinary_block(label::in, list(label)::in, block_info::in,
	block_map::in, par_map::in, set(label)::in,
	set(label)::in, int::in, string::in, proc_label::in, int::in,
	list(label)::out, block_map::out, par_map::out, set(label)::out) is det.

transform_ordinary_block(Label0, Labels0, BlockInfo0, BlockMap0, ParMap0,
		FallIntoParallel0, FramedLabels, FrameSize, Msg,
		ProcLabel, N0, Labels, BlockMap, ParMap, FallIntoParallel) :-
	BlockInfo0 = block_info(_, Instrs0, SideLabels0,
		MaybeFallThrough0, Type),
	mark_parallels_for_teardown(SideLabels0, SideLabels,
		LabelMap, BlockMap0, ProcLabel, N0, N1, ParMap0, ParMap1),
	pick_last(Instrs0, PrevInstrs, LastInstr0),
	substitute_labels(LastInstr0, LabelMap, LastInstr),
	list__append(PrevInstrs, [LastInstr], Instrs),
	(
		MaybeFallThrough0 = yes(FallThrough),
		map__lookup(BlockMap0, FallThrough, FallThroughInfo),
		FallThroughInfo = block_info(_, _, _, _, FallThroughType),
		(
			FallThroughType = setup,
			error("ordinary block falls through to setup")
		;
			FallThroughType = ordinary(_),
			FallIntoParallel1 = FallIntoParallel0,
			ParMap2 = ParMap1,
			( set__member(FallThrough, FramedLabels) ->
				% We fall through from a block without a
				% stack frame to a block which needs a
				% stack frame, so we must create one.

				NewLabel = local(ProcLabel, N1),
				MaybeFallThrough = yes(NewLabel),
				MaybeNewLabel = yes(NewLabel),
				N2 is N1 + 1,
				SetupCode = [
					label(NewLabel)
						- "late setup label",
					incr_sp(FrameSize, Msg)
						- "late setup",
					assign(stackvar(FrameSize), 
						lval(succip))
						- "late save"
				],
				SetupBlock = block_info(NewLabel, SetupCode,
					[], MaybeFallThrough0, setup),
				map__det_insert(BlockMap0, NewLabel,
					SetupBlock, BlockMap1)
			;
				MaybeFallThrough = yes(FallThrough),
				BlockMap1 = BlockMap0,
				MaybeNewLabel = no,
				N2 = N1
			)
		;
			FallThroughType = teardown(_),
			MaybeFallThrough = yes(FallThrough),
			BlockMap1 = BlockMap0,
			set__insert(FallIntoParallel0,
				FallThrough, FallIntoParallel1),
			MaybeNewLabel = no,
			mark_parallel(FallThrough, _, ProcLabel, N1, N2,
				ParMap1, ParMap2)
		)
	;
		MaybeFallThrough0 = no,
		MaybeFallThrough = no,
		BlockMap1 = BlockMap0,
		FallIntoParallel1 = FallIntoParallel0,
		ParMap2 = ParMap1,
		MaybeNewLabel = no,
		N2 = N1
	),
	BlockInfo = block_info(Label0, Instrs, SideLabels,
		MaybeFallThrough, Type),
	map__set(BlockMap1, Label0, BlockInfo, BlockMap2),
	process_frame_delay(Labels0, BlockMap2, ParMap2, FallIntoParallel1,
		FramedLabels, FrameSize, Msg, ProcLabel, N2,
		Labels1, BlockMap, ParMap, FallIntoParallel),
	( MaybeNewLabel = yes(NewLabel2) ->
		Labels = [Label0, NewLabel2 | Labels1]
	;
		Labels = [Label0 | Labels1]
	).

%-----------------------------------------------------------------------------%

	% The input is a list of labels that are jumped to from a frame
	% which has no stack frame. Therefore if some of those labels start
	% teardown blocks, we ensure that those blocks have non-teardown
	% parallels, allocating labels for them if they haven't been allocated
	% already. We return both the updated list of labels and the
	% substitution (represented as an association list) that will have
	% to applied to the jumpting instruction.

:- pred mark_parallels_for_teardown(list(label)::in, list(label)::out,
	assoc_list(label)::out, block_map::in,
	proc_label::in, int::in, int::out, par_map::in, par_map::out) is det.

mark_parallels_for_teardown([], [], [], _, _, N, N, ParMap, ParMap).
mark_parallels_for_teardown([Label0 | Labels0], [Label | Labels],
		[Label0 - Label | LabelMap], BlockMap,
		ProcLabel, N0, N, ParMap0, ParMap) :-
	map__lookup(BlockMap, Label0, BlockInfo),
	BlockInfo = block_info(_, _, _, _, Type),
	(
		Type = setup,
		error("reached setup via jump from ordinary block")
	;
		Type = ordinary(_),
		Label = Label0,
		N1 = N0,
		ParMap1 = ParMap0
	;
		Type = teardown(_),
		mark_parallel(Label0, Label, ProcLabel, N0, N1,
			ParMap0, ParMap1)
	),
	mark_parallels_for_teardown(Labels0, Labels, LabelMap,
		BlockMap, ProcLabel, N1, N, ParMap1, ParMap).

	% Given the label of a teardown block, allocate a label for its
	% non-teardown parallel if it doesn't already have one.

:- pred mark_parallel(label::in, label::out,
	proc_label::in, int::in, int::out, par_map::in, par_map::out) is det.

mark_parallel(Label0, Label, ProcLabel, N0, N, ParMap0, ParMap) :-
	( map__search(ParMap0, Label0, OldParallel) ->
		Label = OldParallel,
		N = N0,
		ParMap = ParMap0
	;
		NewParallel = local(ProcLabel, N0),
		Label = NewParallel,
		N is N0 + 1,
		map__det_insert(ParMap0, Label0, NewParallel, ParMap)
	).

%-----------------------------------------------------------------------------%

	% Given an instruction, substitute the labels in it
	% according to the given association list.
	% The substitution is not performed for instruction components
	% which treat the label merely as data.

:- pred substitute_labels(instruction::in, assoc_list(label)::in,
	instruction::out) is det.

substitute_labels(Uinstr0 - Comment, LabelMap, Uinstr - Comment) :-
	substitute_labels_instr(Uinstr0, LabelMap, Uinstr).

:- pred substitute_labels_instr(instr::in, assoc_list(label)::in,
	instr::out) is det.

substitute_labels_instr(comment(Comment), _, comment(Comment)).
substitute_labels_instr(livevals(LiveVals), _, livevals(LiveVals)).
substitute_labels_instr(block(_, _, _), _, _) :-
	error("block in substitute_labels_instr").
substitute_labels_instr(assign(Lval, Rval), _, assign(Lval, Rval)).
substitute_labels_instr(call(Target, ReturnAddr0, LiveInfo, Model), LabelMap,
		call(Target, ReturnAddr, LiveInfo, Model)) :-
	(
		ReturnAddr0 = label(Label0),
		assoc_list__search(LabelMap, Label0, Label)
	->
		ReturnAddr = label(Label)
	;
		ReturnAddr = ReturnAddr0
	).
substitute_labels_instr(mkframe(Name, Size, Redoip), _,
		mkframe(Name, Size, Redoip)).
substitute_labels_instr(modframe(Redoip), _, modframe(Redoip)).
substitute_labels_instr(label(_), _, _) :-
	error("label in substitute_labels_instr").
substitute_labels_instr(goto(CodeAddr0), LabelMap, goto(CodeAddr)) :-
	(
		CodeAddr0 = label(Label0),
		assoc_list__search(LabelMap, Label0, Label)
	->
		CodeAddr = label(Label)
	;
		CodeAddr = CodeAddr0
	).
substitute_labels_instr(computed_goto(Rval, Targets0), LabelMap,
		computed_goto(Rval, Targets)) :-
	substitute_labels_list(Targets0, LabelMap, Targets).
substitute_labels_instr(c_code(Code), _, c_code(Code)).
substitute_labels_instr(if_val(Rval, CodeAddr0), LabelMap,
		if_val(Rval, CodeAddr)) :-
	(
		CodeAddr0 = label(Label0),
		assoc_list__search(LabelMap, Label0, Label)
	->
		CodeAddr = label(Label)
	;
		CodeAddr = CodeAddr0
	).
substitute_labels_instr(incr_hp(Lval, Tag, Rval), _,
		incr_hp(Lval, Tag, Rval)).
substitute_labels_instr(mark_hp(Lval), _, mark_hp(Lval)).
substitute_labels_instr(restore_hp(Rval), _, restore_hp(Rval)).
substitute_labels_instr(store_ticket(Lval), _, store_ticket(Lval)).
substitute_labels_instr(restore_ticket(Rval), _, restore_ticket(Rval)).
substitute_labels_instr(discard_ticket, _, discard_ticket).
substitute_labels_instr(incr_sp(Size, Name), _, incr_sp(Size, Name)).
substitute_labels_instr(decr_sp(Size), _, decr_sp(Size)).
substitute_labels_instr(pragma_c(Decl, In, Code, Out, Context), _,
		pragma_c(Decl, In, Code, Out, Context)).

:- pred substitute_labels_list(list(label)::in, assoc_list(label)::in,
	list(label)::out) is det.

substitute_labels_list([], _, []).
substitute_labels_list([Label0 | Labels0], LabelMap, [Label | Labels]) :-
	( assoc_list__search(LabelMap, Label0, Label1) ->
		Label = Label1
	;
		Label = Label0
	),
	substitute_labels_list(Labels0, LabelMap, Labels).

%-----------------------------------------------------------------------------%

	% The third phase of the delay_frame optimization, creating
	% the non-teardown parallel blocks.

:- pred create_parallels(list(label)::in, block_map::in, par_map::in,
	set(label)::in, list(label)::out, block_map::out) is det.

create_parallels([], BlockMap, _, _, [], BlockMap).
create_parallels([Label0 | Labels0], BlockMap0, ParMap, FallIntoParallel,
		Labels, BlockMap) :-
	create_parallels(Labels0, BlockMap0, ParMap, FallIntoParallel,
		Labels1, BlockMap1),
	( map__search(ParMap, Label0, ParallelLabel) ->
		map__lookup(BlockMap1, Label0, BlockInfo0),
		BlockInfo0 = block_info(Label0Copy, _,
			SideLabels, MaybeFallThrough, Type),
		( Label0 = Label0Copy ->
			true
		;
			error("label in block_info is not copy")
		),
		( MaybeFallThrough = no ->
			true
		;
			error("block with parallel has fall through")
		),
		( Type = teardown(Replacement0) ->
			LabelInstr = label(ParallelLabel)
				- "non-teardown parallel",
			Replacement = [LabelInstr | Replacement0],
			NewBlockInfo = block_info(ParallelLabel, Replacement,
				SideLabels, no, ordinary(no)),
			map__det_insert(BlockMap1, ParallelLabel,
				NewBlockInfo, BlockMap),
			( set__member(Label0, FallIntoParallel) ->
				Labels = [ParallelLabel, Label0 | Labels1]
			;
				Labels = [Label0, ParallelLabel | Labels1]
			)
		;
			error("block with parallel is not teardown")
		)
	;
		Labels = [Label0 | Labels1],
		BlockMap = BlockMap1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
