%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001,2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: frameopt.m.
% Main author: zs.

% This module performs two kinds of transformation to optimize code that
% manipulates detstack frames.
%
% The first and more common transformation transforms code such as
%
%   proc_entry:
%       incr_sp(N)
%       stackvar(N) = succip
%       if (cond) goto L1
%       ...
%   L1: finalization
%       succip = stackvar(N)
%       decr_sp(N)
%       proceed
%
% into
%
%   proc_entry:
%       if (cond) goto L1
%       incr_sp(N)
%       stackvar(N) = succip
%       ...
%       finalization
%       succip = stackvar(N)
%       decr_sp(N)
%       proceed
%   L1: finalization
%       proceed
%
% The advantage is that we don't set up the stack frame unless we need it.
%
% The actual optimization is more complex than this, because we want to delay
% the construction of the stack frame across more than one jump, if this is
% possible.
%
% The second transformation transforms code such as
%
%   proc_entry:
%       incr_sp(N)
%       stackvar(N) = succip
%       ...
%       finalization
%       succip = stackvar(N)
%       decr_sp(N)
%       goto proc_entry
%
% into
%
%   proc_entry:
%       incr_sp(N)
%       stackvar(N) = succip
%   L1:
%       ...
%       succip = stackvar(N)
%       finalization
%       goto L1
%
% The advantage is that we don't destroy the stack frame just to set it up
% again. The restore of succip can be omitted if the procedure makes no calls,
% since in that case succip must still contain the value it had when this
% procedure was called.
%
% Since the second transformation is a bigger win, we prefer to use it
% whenever both transformations are possible.
%
% For predicates that live on the nondet stack, we attempt to perform
% only the second transformation.
%
% NOTE: This module cannot handle code sequences of the form
%
%   label1:
%   label2:
%
% Jump optimization converts any gotos to label1 to gotos to label2, making
% label1 unused, and label optimization removes unused labels. (This works
% for any number of labels in a sequence, not just two.) Therefore the
% handle_options module turns on the jump and label optimizations whenever
% frame optimization is turned on.

%-----------------------------------------------------------------------------%

:- module ll_backend__frameopt.
:- interface.

:- import_module libs.globals.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % frameopt_main(ProcLabel, !LabelCounter, !Instrs, Globals, AnyChange,
    %   NewJumps):
    %
    % Attempt to update !Instrs using the one of the transformations
    % described above for procedures that live on the det stack.
    %
    % ProcLabel should be the ProcLabel of the procedure whose body !.Instrs
    % implements, and !.LabelCounter that procedure's label counter.
    % If frameopt_main allocates any labels, !:LabelCounter will reflect this.
    %
    % AnyChange says whether we performed any modifications.
    % If yes, then we also introduced some extra labels that should be
    % deleted and probably some jumps that could be profitably be
    % short-circuited.
    %
:- pred frameopt_main(proc_label::in, counter::in, counter::out,
    list(instruction)::in, list(instruction)::out, globals::in, bool::out)
    is det.

    % frameopt_nondet(ProcLabel, LayoutLabels, MayAlterRtti, !LabelCounter,
    %   !Instrs, AnyChange):
    %
    % Attempt to update !Instrs using the one of the transformations
    % described above for procedures that live on the nondet stack.
    %
    % ProcLabel should be the ProcLabel of the procedure whose body
    % !.Instrs implements, and !.LabelCounter that procedure's label
    % counter. If frameopt_main allocates any labels, !:LabelCounter
    % will reflect this.
    %
    % LayoutLabels should be the set of labels in the procedure with layout
    % structures, while MayAlterRtti should say whether we are allowed to
    % perform optimizations that may interfere with RTTI.
    %
    % AnyChange says whether we performed any modifications.
    % If yes, then we also introduced some extra labels that should be
    % deleted, and we introduced some jumps that may be profitably
    % short-circuited.
    %
:- pred frameopt_nondet(proc_label::in, set(label)::in, may_alter_rtti::in,
    counter::in, counter::out,
    list(instruction)::in, list(instruction)::out, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.livemap.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.opt_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module queue.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module svqueue.
:- import_module svset.

%-----------------------------------------------------------------------------%

frameopt_main(ProcLabel, !C, Instrs0, Instrs, Globals, Mod) :-
    opt_util__get_prologue(Instrs0, LabelInstr, Comments0, Instrs1),
    ( frameopt__detstack_setup(Instrs1, FrameSize, Msg, _, _, _) ->
    	some [!BlockMap] (
            map__init(!:BlockMap),
            divide_into_basic_blocks([LabelInstr | Instrs1], ProcLabel,
                BasicInstrs, !C),
            build_frame_block_map(BasicInstrs, FrameSize, LabelSeq0, no, no,
                ProcLabel, !BlockMap, map__init, PredMap, !C),
            analyze_block_map(LabelSeq0, !BlockMap, KeepFrame),
            (
                KeepFrame = yes(FirstLabel - SecondLabel),
                CanClobberSuccip = can_clobber_succip(LabelSeq0, !.BlockMap),
                keep_frame_transform(LabelSeq0, FirstLabel, SecondLabel,
                    CanClobberSuccip, !BlockMap),
                LabelSeq = LabelSeq0,
                NewComment = comment("keeping stack frame") - "",
                list__append(Comments0, [NewComment], Comments),
                flatten_block_seq(LabelSeq, !.BlockMap, BodyInstrs),
                list__append(Comments, BodyInstrs, Instrs),
                Mod = yes
            ;
                KeepFrame = no,
                (
                    can_delay_frame(LabelSeq0, !.BlockMap),
                    delay_frame_transform(LabelSeq0, LabelSeq, FrameSize, Msg,
                        ProcLabel, PredMap, !C, !BlockMap, Globals,
                        NewComments, CanTransform),
                    CanTransform = can_transform
                ->
                    Comments = Comments0 ++ NewComments,
                    flatten_block_seq(LabelSeq, !.BlockMap, BodyInstrs),
                    list__append(Comments, BodyInstrs, Instrs),
                    Mod = yes
                ;
                    Instrs = Instrs0,
                    Mod = no
                )
            )
        )
    ;
        Instrs = Instrs0,
        Mod = no
    ).

:- pred flatten_block_seq(list(label)::in, frame_block_map::in,
    list(instruction)::out) is det.

flatten_block_seq([], _, []).
flatten_block_seq([Label | Labels], BlockMap, Instrs) :-
    flatten_block_seq(Labels, BlockMap, RestInstrs),
    map__lookup(BlockMap, Label, BlockInfo),
    BlockInstrs = BlockInfo ^ fb_instrs,
    (
        list__split_last(BlockInstrs, MostInstrs, LastInstr),
        Labels = [NextLabel | _],
        LastInstr = goto(label(NextLabel)) - _
    ->
        % Optimize away the redundant goto, which we probably introduced.
        % The next invocation of jumpopt would also do this, but doing it here
        % is cheaper and may let us reach a fixpoint in the optimization
        % sequence earlier.
        Instrs = MostInstrs ++ RestInstrs
    ;
        Instrs = BlockInstrs ++ RestInstrs
    ).

%-----------------------------------------------------------------------------%

frameopt_nondet(ProcLabel, LayoutLabels, MayAlterRtti, !C, Instrs0, Instrs,
        Mod) :-
    opt_util__get_prologue(Instrs0, LabelInstr, Comments0, Instrs1),
    (
        MayAlterRtti = may_alter_rtti,
        frameopt__nondetstack_setup(Instrs1, FrameInfo, Redoip,
            MkframeInstr, Remain),
        MkframeInstr = MkframeUinstr - MkframeComment,
        find_succeed_labels(Instrs1, map__init, SuccMap),
        counter__allocate(KeepFrameLabelNum, !C),
        KeepFrameLabel = internal(KeepFrameLabelNum, ProcLabel),
        keep_nondet_frame(Remain, Instrs2, ProcLabel, KeepFrameLabel,
            MkframeUinstr, SuccMap, LayoutLabels, no, yes)
    ->
        list__condense([[LabelInstr], Comments0,
            [mkframe(FrameInfo, no) - MkframeComment,
            label(KeepFrameLabel) - "tail recursion target",
            assign(redoip(lval(curfr)), const(code_addr_const(Redoip))) - ""],
            Instrs2], Instrs),
        Mod = yes
    ;
        Instrs = Instrs0,
        Mod = no
    ).

:- pred frameopt__nondetstack_setup(list(instruction)::in,
    nondet_frame_info::out, code_addr::out,
    instruction::out, list(instruction)::out) is semidet.

frameopt__nondetstack_setup(Instrs0, FrameInfo, Redoip, MkframeInstr,
        Remain) :-
    Instrs0 = [MkframeInstr | Remain],
    MkframeInstr = mkframe(FrameInfo, yes(Redoip)) - _,
    FrameInfo = ordinary_frame(_, _, _).

:- pred find_succeed_labels(list(instruction)::in, tailmap::in, tailmap::out)
    is det.

find_succeed_labels([], !SuccMap).
find_succeed_labels([Instr | Instrs], !SuccMap) :-
    Instr = Uinstr - _,
    (
        Uinstr = label(Label),
        opt_util__skip_comments(Instrs, TailInstrs),
        opt_util__is_succeed_next(TailInstrs, Between)
    ->
        svmap__det_insert(Label, Between, !SuccMap)
    ;
        true
    ),
    find_succeed_labels(Instrs, !SuccMap).

:- pred keep_nondet_frame(list(instruction)::in, list(instruction)::out,
    proc_label::in, label::in, instr::in, tailmap::in, set(label)::in,
    bool::in, bool::out) is det.

keep_nondet_frame([], [], _, _, _, _, _, !Changed).
keep_nondet_frame([Instr0 | Instrs0], Instrs, ProcLabel, KeepFrameLabel,
        PrevInstr, SuccMap, LayoutLabels, !Changed) :-
    Instr0 = Uinstr0 - Comment,
    (
        % Look for nondet style tailcalls which do not need
        % a runtime check.
        Uinstr0 = call(label(entry(_, ProcLabel)), label(RetLabel),
            _, _, _, CallModel),
        CallModel = nondet(unchecked_tail_call),
        map__search(SuccMap, RetLabel, BetweenIncl),
        BetweenIncl = [livevals(_) - _, goto(_) - _],
        PrevInstr = livevals(Livevals),
        not set__member(RetLabel, LayoutLabels)
    ->
        keep_nondet_frame(Instrs0, Instrs1, ProcLabel, KeepFrameLabel,
            Uinstr0, SuccMap, LayoutLabels, !.Changed, _),
        !:Changed = yes,
        NewComment = Comment ++ " (nondet tailcall)",
        Instrs = [
            livevals(Livevals) - "",
            goto(label(KeepFrameLabel)) - NewComment
            | Instrs1
        ]
    ;
        keep_nondet_frame(Instrs0, Instrs1, ProcLabel, KeepFrameLabel,
            Uinstr0, SuccMap, LayoutLabels, !Changed),
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type frame_block_map   ==  map(label, frame_block_info).

:- type frame_block_info
    --->    frame_block_info(
                fb_label        :: label,
                                % The label of the first instr.

                fb_instrs       :: list(instruction),
                                % The code of the block.

                fb_fallen_into  :: maybe(label),
                                % Does the previous block (if any)
                                % fall through to this block, and if yes,
                                % what is its label?

                fb_jump_dests   :: list(label),
                                % The labels we can jump to
                                % (not falling through).

                fb_fall_dest    :: maybe(label),
                                % The label we fall through to
                                % (if there is one).

                fb_type         :: block_type
            ).

:- type block_type
    --->    setup               % This is a block containing
                                % only setup instructions.

    ;       ordinary(block_needs_frame)
                                % This block does not contain setup or
                                % teardown. The arg says whether the code
                                % in the block needs a stack frame.

    ;       teardown(           % This block contains stack teardown
                                % and goto code.
                list(instruction),
                                % The instr that restores succip (if any),
                list(instruction),
                                % The livevals instr before the goto (if any),
                instruction
                                % The goto instr.
            ).

:- type block_needs_frame
    --->    block_needs_frame
    ;       block_doesnt_need_frame.

%-----------------------------------------------------------------------------%

    % Add labels to the given instruction sequence so that
    % every basic block has labels around it.
    %
:- pred divide_into_basic_blocks(list(instruction)::in, proc_label::in,
    list(instruction)::out, counter::in, counter::out) is det.

divide_into_basic_blocks([], _, [], !C).
    % Control can fall off the end of a procedure if that procedure
    % ends with a call to another procedure that cannot succeed.
    % This is the only situation in which the base case can be reached.
divide_into_basic_blocks([Instr0 | Instrs0], ProcLabel, Instrs, !C) :-
    Instr0 = Uinstr0 - _Comment,
    ( opt_util__can_instr_branch_away(Uinstr0, yes) ->
        (
            Instrs0 = [Instr1 | _],
            ( Instr1 = label(_) - _ ->
                divide_into_basic_blocks(Instrs0, ProcLabel, Instrs1, !C),
                Instrs = [Instr0 | Instrs1]
            ;
                counter__allocate(N, !C),
                NewLabel = internal(N, ProcLabel),
                NewInstr = label(NewLabel) - "",
                divide_into_basic_blocks(Instrs0, ProcLabel, Instrs1, !C),
                Instrs = [Instr0, NewInstr | Instrs1]
            )
        ;
            Instrs0 = [],
            Instrs = [Instr0]
        )
    ;
        divide_into_basic_blocks(Instrs0, ProcLabel, Instrs1, !C),
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
    % For such each block, create a frame_block_info structure that gives the
    % label starting the block, the instructions in the block, and its
    % type. Two of the fields of the frame_block_info structure are filled in
    % with dummy values; they will be filled in for real later.
    %
    % Put these frame_block_info structures into a table indexed by the label,
    % and return the sequence of labels of the blocks in their original
    % order.
    %
:- pred build_frame_block_map(list(instruction)::in, int::in, list(label)::out,
    maybe(label)::in, maybe(label)::in, proc_label::in,
    frame_block_map::in, frame_block_map::out,
    pred_map::in, pred_map::out, counter::in, counter::out) is det.

build_frame_block_map([], _, [], _, _, _, !BlockMap, !PredMap, !C).
build_frame_block_map([Instr0 | Instrs0], FrameSize, LabelSeq,
        MaybePrevLabel, FallInto, ProcLabel, !BlockMap, !PredMap, !C) :-
    ( Instr0 = label(Label) - _ ->
        (
            MaybePrevLabel = yes(PrevLabel),
            svmap__det_insert(Label, PrevLabel, !PredMap)
        ;
            MaybePrevLabel = no
        ),
        (
            frameopt__detstack_setup(Instrs0, _, _, Setup, Others, Remain)
        ->
            % Create a block with just the Setup instructions in it.

            BlockInfo = frame_block_info(Label, [Instr0 | Setup], FallInto,
                [], no, setup),
            list__append(Others, Remain, Instrs1),
            (
                Instrs1 = [Instr1 | _],
                Instr1 = label(_) - _
            ->
                Instrs2 = Instrs1
            ;
                counter__allocate(N, !C),
                NewLabel = internal(N, ProcLabel),
                NewInstr = label(NewLabel) - "",
                Instrs2 = [NewInstr | Instrs1]
            ),
            build_frame_block_map(Instrs2, FrameSize, LabelSeq0, yes(Label),
                yes(Label), ProcLabel, !BlockMap, !PredMap, !C),
            svmap__det_insert(Label, BlockInfo, !BlockMap),
            LabelSeq = [Label | LabelSeq0]
        ;
            frameopt__detstack_teardown(Instrs0, FrameSize, Extra,
                SuccipRestore, Decrsp, Livevals, Goto, Remain)
        ->
            Teardown = SuccipRestore ++ Decrsp ++ Livevals ++ [Goto],
            (
                Extra = [],
                MaybeExtraInfo = no,
                LabelledBlock = [Instr0 | Teardown],
                TeardownLabel = Label,
                TeardownInfo = frame_block_info(TeardownLabel, LabelledBlock,
                    FallInto, [], no, teardown(SuccipRestore, Livevals, Goto)),
                NextPrevLabel = Label
            ;
                Extra = [_ | _],
                block_needs_frame(Extra, NeedsFrame),
                ExtraInfo = frame_block_info(Label, [Instr0 | Extra],
                    FallInto, [], no, ordinary(NeedsFrame)),
                MaybeExtraInfo = yes(ExtraInfo - Label),
                counter__allocate(N, !C),
                NewLabel = internal(N, ProcLabel),
                NewInstr = label(NewLabel) - "",
                LabelledBlock = [NewInstr | Teardown],
                TeardownLabel = NewLabel,
                TeardownInfo = frame_block_info(TeardownLabel, LabelledBlock,
                    yes(Label), [], no,
                    teardown(SuccipRestore, Livevals, Goto)),
                svmap__det_insert(NewLabel, Label, !PredMap),
                NextPrevLabel = TeardownLabel
            ),
            build_frame_block_map(Remain, FrameSize, LabelSeq0,
                yes(NextPrevLabel), no, ProcLabel, !BlockMap, !PredMap, !C),
            (
                MaybeExtraInfo = no,
                svmap__det_insert(TeardownLabel, TeardownInfo, !BlockMap),
                LabelSeq = [TeardownLabel | LabelSeq0]
            ;
                MaybeExtraInfo = yes(ExtraInfo2 - ExtraLabel2),
                svmap__det_insert(TeardownLabel, TeardownInfo, !BlockMap),
                svmap__det_insert(ExtraLabel2, ExtraInfo2, !BlockMap),
                LabelSeq = [ExtraLabel2, TeardownLabel | LabelSeq0]
            )
        ;
            opt_util__skip_to_next_label(Instrs0, Block, Instrs1),
            block_needs_frame(Block, NeedsFrame),
            BlockInstrs = [Instr0 | Block],
            BlockInfo = frame_block_info(Label, BlockInstrs, FallInto,
                [], no, ordinary(NeedsFrame)),
            ( list__last(BlockInstrs, LastBlockInstr) ->
                LastBlockInstr = LastBlockUinstr - _,
                opt_util__can_instr_fall_through(LastBlockUinstr,
                    NextFallIntoBool),
                (
                    NextFallIntoBool = yes,
                    NextFallInto = yes(Label)
                ;
                    NextFallIntoBool = no,
                    NextFallInto = no
                )
            ;
                NextFallInto = yes(Label)
            ),
            build_frame_block_map(Instrs1, FrameSize, LabelSeq0,
                yes(Label), NextFallInto, ProcLabel, !BlockMap, !PredMap, !C),
            svmap__det_insert(Label, BlockInfo, !BlockMap),
            LabelSeq = [Label | LabelSeq0]
        )
    ;
        unexpected(this_file,
            "build_frame_block_map; block does not start with label")
    ).

%-----------------------------------------------------------------------------%

    % Does the given code start with a setup of a det stack frame? If yes,
    % return the size of the frame and three instruction sequences,
    % Setup, Others and Remain. Setup is the instruction sequence
    % that sets up the det stack frame, Others is a sequence of
    % non-interfering instructions that were interspersed with Setup
    % but can be moved after Setup, and Remain is all remaining
    % instructions.
    %
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

frameopt__detstack_setup_2([Instr0 | Instrs0], FrameSize, Setup, !Others,
        Remain) :-
    ( Instr0 = assign(Lval, Rval) - _ ->
        (
            Lval = stackvar(FrameSize),
            Rval = lval(succip)
        ->
            Setup = Instr0,
            Remain = Instrs0
        ;
            Lval \= succip,
            Lval \= stackvar(FrameSize)
        ->
            !:Others = !.Others ++ [Instr0],
            frameopt__detstack_setup_2(Instrs0, FrameSize, Setup, !Others,
                Remain)
        ;
            fail
        )
    ; Instr0 = comment(_) - _ ->
        !:Others = !.Others ++ [Instr0],
        frameopt__detstack_setup_2(Instrs0, FrameSize, Setup, !Others, Remain)
    ;
        fail
    ).

%-----------------------------------------------------------------------------%

    % Does the following block contain a teardown of a det stack frame,
    % and a proceed or tailcall? If yes, we return
    %
    % - the instruction that restores succip as Succip
    % - the decr_sp instruction as Decrsp
    % - the livevals instruction as Livevals
    % - the goto instruction as Goto
    %
    % The first three can appear in any order or may be missing, due to
    % value numbering. This is also why we allow the teardown instructions
    % to be interleaved with instructions that do not access the stack;
    % any such instructions are returned as Extra. Remain is all the
    % instructions after the teardown.
    %
:- pred frameopt__detstack_teardown(list(instruction)::in, int::in,
    list(instruction)::out, list(instruction)::out,
    list(instruction)::out, list(instruction)::out,
    instruction::out, list(instruction)::out) is semidet.

frameopt__detstack_teardown([Instr0 | Instrs0], FrameSize,
        Extra, Succip, Decrsp, Livevals, Goto, Remain) :-
    (
        Instr0 = label(_) - _
    ->
        fail
    ;
        frameopt__detstack_teardown_2([Instr0 | Instrs0], FrameSize,
            [], ExtraPrime, [], SuccipPrime, [], DecrspPrime,
            [], LivevalsPrime, GotoPrime, RemainPrime)
    ->
        Extra = ExtraPrime,
        Succip = SuccipPrime,
        Decrsp = DecrspPrime,
        Livevals = LivevalsPrime,
        Goto = GotoPrime,
        Remain = RemainPrime
    ;
        frameopt__detstack_teardown(Instrs0, FrameSize, Extra1, Succip,
            Decrsp, Livevals, Goto, Remain),
        Extra = [Instr0 | Extra1]
    ).

:- pred frameopt__detstack_teardown_2(list(instruction)::in, int::in,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    instruction::out, list(instruction)::out) is semidet.

frameopt__detstack_teardown_2(Instrs0, FrameSize,
        !Extra, !Succip, !Decrsp, !Livevals, Goto, Remain) :-
    opt_util__skip_comments(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    Instr1 = Uinstr1 - _,
    (
        Uinstr1 = assign(Lval, Rval),
        (
            Lval = succip,
            Rval = lval(stackvar(FrameSize))
        ->
            !.Succip = [],
            !.Decrsp = [],
            !:Succip = [Instr1],
            frameopt__detstack_teardown_2(Instrs2, FrameSize, !Extra, !Succip,
                !Decrsp, !Livevals, Goto, Remain)
        ;
            opt_util__lval_refers_stackvars(Lval, no),
            opt_util__rval_refers_stackvars(Rval, no),
            list__append(!.Extra, [Instr1], !:Extra),
            frameopt__detstack_teardown_2(Instrs2, FrameSize, !Extra, !Succip,
                !Decrsp, !Livevals, Goto, Remain)
        )
    ;
        Uinstr1 = decr_sp(FrameSize),
        !.Decrsp = [],
        !:Decrsp = [Instr1],
        frameopt__detstack_teardown_2(Instrs2, FrameSize, !Extra, !Succip,
            !Decrsp, !Livevals, Goto, Remain)
    ;
        Uinstr1 = livevals(_),
        !.Livevals = [],
        !:Livevals = [Instr1],
        frameopt__detstack_teardown_2(Instrs2, FrameSize, !Extra, !Succip,
            !Decrsp, !Livevals, Goto, Remain)
    ;
        Uinstr1 = goto(_),
        !.Decrsp = [_],
        Goto = Instr1,
        Remain = Instrs2
    ).

%-----------------------------------------------------------------------------%

    % Does an ordinary block with the given content need a stack frame?
    %
:- pred block_needs_frame(list(instruction)::in, block_needs_frame::out) is det.

block_needs_frame(Instrs, NeedsFrame) :-
    opt_util__block_refers_stackvars(Instrs, ReferStackVars),
    (
        ReferStackVars = yes,
        NeedsFrame = block_needs_frame
    ;
        ReferStackVars = no,
        (
            list__member(Instr, Instrs),
            Instr = Uinstr - _,
            (
                Uinstr = call(_, _, _, _, _, _)
            ;
                Uinstr = mkframe(_, _)
            ;
                Uinstr = c_code(_, _)
            ;
                Uinstr = pragma_c(_, _, MayCallMercury, _, MaybeLayout,
                    MaybeOnlyLayout, _, NeedStack, _),
                (
                    MayCallMercury = may_call_mercury
                ;
                    MaybeLayout = yes(_)
                ;
                    MaybeOnlyLayout = yes(_)
                ;
                    NeedStack = yes
                )
            )
        ->
            NeedsFrame = block_needs_frame
        ;
            NeedsFrame = block_doesnt_need_frame
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % For each block in the given sequence, whose frame_block_info structures
    % in the given block map have been partially filled in, fill in the
    % remaining two fields. These two fields give the labels the block
    % can branch to on the side (this includes return addresses for calls),
    % and the label if any to which it falls through.
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
    %
:- pred analyze_block_map(list(label)::in,
    frame_block_map::in, frame_block_map::out, maybe(pair(label))::out) is det.

analyze_block_map(LabelSeq, !BlockMap, KeepFrameData) :-
    (
        LabelSeq = [FirstLabel, SecondLabel | _],
        map__search(!.BlockMap, FirstLabel, FirstBlockInfo),
        FirstBlockInfo = frame_block_info(FirstLabel, _, _, _, _, setup)
    ->
        analyze_block_map_2(LabelSeq, FirstLabel, !BlockMap,
            block_doesnt_need_frame, AnyBlockNeedsFrame, no, JumpToStart),
        % We want to apply the transformation to keep the stack frame only if
        % (a) some block actually needs the stack frame, and (b) there is at
        % least one block that jumps back to the start of the procedure.
        (
            AnyBlockNeedsFrame = block_needs_frame,
            JumpToStart = yes
        ->
            KeepFrameData = yes(FirstLabel - SecondLabel)
        ;
            KeepFrameData = no
        )
    ;
        unexpected(this_file, "analyze_block_map: bad data")
    ).

:- pred analyze_block_map_2(list(label)::in, label::in,
    frame_block_map::in, frame_block_map::out,
    block_needs_frame::in, block_needs_frame::out, bool::in, bool::out) is det.

analyze_block_map_2([], _, !BlockMap, !AnyBlockNeedsFrame, !KeepFrame).
analyze_block_map_2([Label | Labels], FirstLabel, !BlockMap,
        !AnyBlockNeedsFrame, !JumpToStart) :-
    map__lookup(!.BlockMap, Label, BlockInfo0),
    BlockInfo0 = frame_block_info(BlockLabel, BlockInstrs, FallInto,
        _, _, Type),
    ( Type = ordinary(block_needs_frame) ->
        !:AnyBlockNeedsFrame = block_needs_frame
    ;
        true
    ),
    (
        Label = BlockLabel, % sanity check
        list__last(BlockInstrs, LastInstr)
    ->
        LastInstr = LastUinstr - _,
        possible_targets(LastUinstr, SideLabels, _SideCodeAddrs),
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
            matching_label_ref(FirstLabel, GotoLabel)
        ->
            !:JumpToStart = yes
        ;
            true
        )
    ;
        unexpected(this_file, "analyze_block_map_2: mismatch or no last instr")
    ),
    BlockInfo = frame_block_info(BlockLabel, BlockInstrs, FallInto,
        SideLabels, MaybeFallThrough, Type),
    svmap__det_update(Label, BlockInfo, !BlockMap),
    analyze_block_map_2(Labels, FirstLabel, !BlockMap, !AnyBlockNeedsFrame,
        !JumpToStart).

    % The form of a label used in a tailcall may be different from
    % the form used in the initial label. The initial label may be
    % exported from the Mercury module or possibly from the C module,
    % while the label used in the tailcall may use a more local form
    % for better performance.
    %
    % This predicate tests whether the second label (from the tailcall)
    % is a proper reference to the first label (from the initial label
    % instruction).
    %
:- pred matching_label_ref(label::in, label::in) is semidet.

matching_label_ref(entry(FirstLabelType, ProcLabel),
        entry(GotoLabelType, ProcLabel)) :-
    matching_entry_type(FirstLabelType, GotoLabelType).

:- pred matching_entry_type(entry_label_type::in, entry_label_type::in)
    is semidet.

matching_entry_type(exported, exported).
matching_entry_type(exported, c_local).
matching_entry_type(exported, local).
matching_entry_type(local, c_local).
matching_entry_type(local, local).
matching_entry_type(c_local, c_local).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func can_clobber_succip(list(label), frame_block_map) = bool.

can_clobber_succip([], _BlockMap) = no.
can_clobber_succip([Label | Labels], BlockMap) = CanClobberSuccip :-
    map__lookup(BlockMap, Label, BlockInfo),
    Instrs = BlockInfo ^ fb_instrs,
    (
        list__member(Instr, Instrs),
        Instr = Uinstr - _,
        (
            Uinstr = call(_, _, _, _, _, _)
        ;
            % Only may_call_mercury pragma_c's can clobber succip.
            Uinstr = pragma_c(_, _, may_call_mercury,
                _, _, _, _, _, _)
        )
    ->
        CanClobberSuccip = yes
    ;
        CanClobberSuccip = can_clobber_succip(Labels, BlockMap)
    ).

%-----------------------------------------------------------------------------%

    % Transform the given block sequence to effect the optimization
    % of keeping the stack frame for recursive tail calls once it has
    % been set up by the initial entry to the procedure.
    % The two label arguments are the label starting the procedure
    % (a form of which appears in existing tailcalls) and the label that
    % should replace it in tailcalls that avoid the stack teardown.
    %
:- pred keep_frame_transform(list(label)::in, label::in, label::in, bool::in,
    frame_block_map::in, frame_block_map::out) is det.

keep_frame_transform([], _, _, _, !BlockMap).
keep_frame_transform([Label | Labels], FirstLabel, SecondLabel,
        CanClobberSuccip, !BlockMap) :-
    map__lookup(!.BlockMap, Label, BlockInfo0),
    (
        BlockInfo0 = frame_block_info(Label, OrigInstrs, FallInto, [_], no,
            teardown(Succip, Livevals, Goto)),
        Goto = goto(label(GotoLabel)) - Comment,
        matching_label_ref(FirstLabel, GotoLabel)
    ->
        (
            OrigInstrs = [OrigInstr0 | _],
            OrigInstr0 = label(_) - _
        ->
            OrigLabelInstr = OrigInstr0
        ;
            unexpected(this_file,
                "keep_frame_transform: block does not begin with label")
        ),
        string__append(Comment, " (keeping frame)", NewComment),
        NewGoto = goto(label(SecondLabel)) - NewComment,
        list__append(Livevals, [NewGoto], LivevalsGoto),
        (
            CanClobberSuccip = yes,
            list__append(Succip, LivevalsGoto, BackInstrs)
        ;
            CanClobberSuccip = no,
            BackInstrs = LivevalsGoto
        ),
        Instrs = [OrigLabelInstr | BackInstrs],
        BlockInfo = frame_block_info(Label, Instrs, FallInto,
            [SecondLabel], no, ordinary(block_needs_frame)),
        map__det_update(!.BlockMap, Label, BlockInfo, !:BlockMap)
    ;
        true
    ),
    keep_frame_transform(Labels, FirstLabel, SecondLabel, CanClobberSuccip,
        !BlockMap).

% list__split_last_det
:- pred pick_last(list(T)::in, list(T)::out, T::out) is det.

pick_last([], _, _) :-
    unexpected(this_file, "empty list in pick_last").
pick_last([First | Rest], NonLast, Last) :-
    (
        Rest = [],
        NonLast = [],
        Last = First
    ;
        Rest = [_ | _],
        pick_last(Rest, NonLast0, Last),
        NonLast = [First | NonLast0]
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Check that we can use the delay_frame transformation. This requires
    % that only the first block is of the setup type.
    %
:- pred can_delay_frame(list(label)::in, frame_block_map::in) is semidet.

can_delay_frame([], _).
can_delay_frame([Label | _Labels], BlockMap) :-
    map__lookup(BlockMap, Label, BlockInfo),
    BlockInfo ^ fb_type = setup.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % The data structures used in the delaying optimizations.

    % map__search(RevMap, Label, SideLabels) should be true if the block
    % started by Label can be reached via jump or fallthrough from the labels
    % in SideLabels.
    %
:- type rev_map ==  map(label, list(label)).

    % Given the label L starting a block, map__search(PredMap, L, PrevL)
    % is true if PrevL starts the block immediately before L.
    % If L is the first block in the sequence (which should be a setup block),
    % map__search(PredMap, L, _) fails.
:- type pred_map == map(label, label).

    % map__search(SetupParMap, L, SetupL) should be true if L starts
    % an ordinary block that needs a stack frame that can be jumped to by
    % blocks that do not have a stack frame. In this case, SetupL will be the
    % label starting a new block that sets up the stack frame before handing
    % control to L. We put SetupL's block immediately before L's block.
    % If L cannot be fallen into, this is fine. If it can, we put a goto
    % around SetupL's block after the previous block. In most cases,
    % full jump optimization to duplicate code as necessary to optimize away
    % the goto.
    %
:- type setup_par_map ---> setup_par_map(map(label, label)).

    % map__search(TeardownParMap, L, ParallelL should be true if L starts
    % a teardown block and ParallelL starts a copy of L's block from which
    % the instructions to tear down the stack frame have been deleted.
    % If the block immediately before L does not have a stack frame,
    % we put ParallelL before L. If it does, we put ParallelL after L.
    % since neither L nor ParallelL can fall through, we don't need any gotos
    % to jump around blocks.
    %
:- type teardown_par_map ---> teardown_par_map(map(label, label)).

%-----------------------------------------------------------------------------%

:- type can_transform
    --->    can_transform
    ;       cannot_transform.

    % XXX needs updating

    % The optimization of delaying the creation of stack frames as long
    % as possible is in three main phases:
    %
    % - The first phase finds out which ordinary blocks need a stack frame.
    %   This naturally includes blocks that access stackvars, and blocks
    %   that perform calls. It also includes blocks which are jumped to
    %   or fallen through to from ordinary blocks that need a stack frame;
    %   this is done by propagate_frame_requirement_to_successors.   
    %   A predecessor of a block that needs a frame need not have a frame
    %   itself, since we can interpose the frame setup code on the jump or
    %   fallthrough between them. However, if all of a block's successors
    %   need a frame, then interposing the setup code on *all* those jumps
    %   and/or fallthroughs is a space cost, and delaying the setup doesn't
    %   gain any time, so we may as well say that the predecessor itself needs
    %   a frame. This is done by propagate_frame_requirement_to_predecessors.
    %   If a propagation step says that a setup block needs a stack frame,
    %   that implies that all blocks need a frame, and thus we cannot avoid
    %   setting up a stack frame on any path. In such cases, this predicate
    %   wil return CanTransform = cannot_transform, which tells our caller
    %   to leave the procedure's code unmodified.
    %
    % - The second phase gets rid of the frame setup code in the initial
    %   setup block, but its main task is to transform ordinary blocks that
    %   do not need a frame. Every block that is a successor of such a block,
    %   whether via jump or fallthrough, will be an ordinary block or a
    %   teardown block.
    %
    %   - If the successor is an ordinary block that doesn't need a frame,
    %     the transfer of control remains as before.
    %
    %   - If the successor is an ordinary block B that does need a stack frame,
    %     we need to insert the frame setup code at the transfer between the
    %     two blocks. In phase 2, we note the need for this insertion by
    %     allocating a new label SetupB, which will start a new block that will
    %     create the stack frame and then fall through to B. In the second
    %     phase, we alter the transfer of control to goto to SetupB instead
    %     of B; the creation of SetupB's block is left to the third phase.
    %     The correspondence between B and SetupB is recorded in SetupParMap.
    %
    %   - If the successor is a teardown block B, then we modify the transfer
    %     of control to jump a new label ParallelB, whose block has the same
    %     code as B's block, except for the deletion of the instructions that
    %     tear down the (nonexistent along this path) stack frame. The
    %     correspondence between B and ParallelB is recorded in TeardownParMap.
    %
    % - The third phase has the job of adding the pieces of code whose
    %   existence is assumed by the modified code output by the second stage.
    %
    %   For every frame-needing ordinary block that can be jumped to from
    %   someplace that does not have a stack frame (i.e. for every B in
    %   SetupParMap), we insert before it the SetupB label followed by
    %   the code to set up the stack frame SetupB's predecessor may need to
    %   jump around SetupB's block if previously it fell through to B.)
    %
    %   For every teardown block that can jumped to from someplace that does
    %   not have a stack frame (i.e. for every B in TeardownParMap), we create
    %   a new block that is a clone of B with the stack teardown deleted.
    %   Whether we put B or ParallelB first depends on whether the immediately
    %   previous block has a stack frame or not.
    %
:- pred delay_frame_transform(list(label)::in, list(label)::out, int::in,
    string::in, proc_label::in, pred_map::in, counter::in, counter::out,
    frame_block_map::in, frame_block_map::out, globals::in,
    list(instruction)::out, can_transform::out) is det.

delay_frame_transform(!LabelSeq, FrameSize, Msg, ProcLabel, PredMap, !C,
        !BlockMap, Globals, NewComments, CanTransform) :-
    some [!OrdNeedsFrame, !CanTransform, !PropagationStepsLeft] (
        !:OrdNeedsFrame = map__init,
        !:CanTransform = can_transform,
        !:PropagationStepsLeft = max_propagation_steps,
        delay_frame_init(!.LabelSeq, !.BlockMap, map__init, RevMap,
            queue__init, SuccQueue, !OrdNeedsFrame),
        propagate_frame_requirement_to_successors(SuccQueue, !.BlockMap,
            !OrdNeedsFrame, set__init, !PropagationStepsLeft, !CanTransform),
        map__to_assoc_list(!.OrdNeedsFrame, OrdNeedsFrameList),
        list__filter_map(key_block_needs_frame, OrdNeedsFrameList, Frontier),
        queue__list_to_queue(Frontier, PredQueue),
        propagate_frame_requirement_to_predecessors(PredQueue, !.BlockMap,
            RevMap, !OrdNeedsFrame, !.PropagationStepsLeft, _, !CanTransform),
        (
            !.CanTransform = cannot_transform,
            % The delay frame optimization is not applicable; our caller will
            % ignore all the other output arguments.
            NewComments = []
        ;
            !.CanTransform = can_transform,
            globals__lookup_bool_option(Globals, frameopt_comments,
                FrameoptComments),
            (
                FrameoptComments = no,
                NewComments = []
            ;
                FrameoptComments = yes,
                FirstComment = comment("delaying stack frame") - "",
                list__map(describe_block(!.BlockMap, !.OrdNeedsFrame,
                    PredMap, ProcLabel), !.LabelSeq, BlockComments),
                NewComments = [FirstComment | BlockComments]
            ),
            process_frame_delay(!.LabelSeq, !.OrdNeedsFrame, ProcLabel, !C,
                !BlockMap, setup_par_map(map__init), SetupParMap,
                teardown_par_map(map__init), TeardownParMap),
            create_parallels(!LabelSeq, FrameSize, Msg, ProcLabel, !C,
                !.OrdNeedsFrame, SetupParMap, TeardownParMap, PredMap,
                !BlockMap)
        ),
        CanTransform = !.CanTransform
    ).

    % We want to stop the transformation if we need more than this many
    % propagation steps. For such large predicates (write_ordinary_term
    % in , any performance benefit
    % of frameopt is unlikely to be noticeable.
    %
:- func max_propagation_steps = int.

max_propagation_steps = 10000.

:- pred key_block_needs_frame(pair(label, block_needs_frame)::in, label::out)
    is semidet.

key_block_needs_frame(Label - block_needs_frame, Label).

%-----------------------------------------------------------------------------%

    % Maps the label of each ordinary block to a bool that says whether
    % the block needs a stack frame or not.
    %
:- type ord_needs_frame == map(label, block_needs_frame).

    % Initialize the data structures for the delaying operation.
    % The first is a map showing the predecessors of each block,
    % i.e. the set of blocks that can jump to or fall through each other block.
    % The second is a queue of ordinary blocks that need a stack frame.
    % The third says, for each ordinary block, whether it needs a stack frame.
    %
    % This predicate implements the first part of the first phase of
    % delay_frame_transform.
    %
:- pred delay_frame_init(list(label)::in, frame_block_map::in,
    rev_map::in, rev_map::out, queue(label)::in, queue(label)::out,
    ord_needs_frame::in, ord_needs_frame::out) is det.

delay_frame_init([], _, !RevMap, !Queue, !OrdNeedsFrame).
delay_frame_init([Label | Labels], BlockMap, !RevMap, !Queue,
        !OrdNeedsFrame) :-
    map__lookup(BlockMap, Label, BlockInfo),
    BlockType = BlockInfo ^ fb_type,
    (
        BlockType = setup
    ;
        BlockType = ordinary(NeedsFrame),
        svmap__det_insert(Label, NeedsFrame, !OrdNeedsFrame),
        (
            NeedsFrame = block_doesnt_need_frame
        ;
            NeedsFrame = block_needs_frame,
            svqueue__put(Label, !Queue)
        )
    ;
        BlockType = teardown(_, _, _)
    ),
    rev_map_side_labels(successors(BlockInfo), Label, !RevMap),
    delay_frame_init(Labels, BlockMap, !RevMap, !Queue, !OrdNeedsFrame).

:- pred rev_map_side_labels(list(label)::in, label::in,
    rev_map::in, rev_map::out) is det.

rev_map_side_labels([], _Label, !RevMap).
rev_map_side_labels([Label | Labels], SourceLabel, !RevMap) :-
    ( map__search(!.RevMap, Label, OtherSources0) ->
        OtherSources = [SourceLabel | OtherSources0],
        svmap__det_update(Label, OtherSources, !RevMap)
    ;
        OtherSources = [SourceLabel],
        svmap__det_insert(Label, OtherSources, !RevMap)
    ),
    rev_map_side_labels(Labels, SourceLabel, !RevMap).

%-----------------------------------------------------------------------------%

    % Given a queue of labels representing ordinary blocks that must have
    % a stack frame, propagate the requirement for a stack frame to all
    % other ordinary blocks that are their successors.
    %
    % This predicate implements the second part of the first phase of
    % delay_frame_transform.
    %
:- pred propagate_frame_requirement_to_successors(queue(label)::in,
    frame_block_map::in, ord_needs_frame::in, ord_needs_frame::out,
    set(label)::in, int::in, int::out, can_transform::in, can_transform::out)
    is det.

propagate_frame_requirement_to_successors(!.Queue, BlockMap, !OrdNeedsFrame,
        !.AlreadyProcessed, !PropagationStepsLeft, !CanTransform) :-
    ( !.CanTransform = cannot_transform ->
        true
    ; !.PropagationStepsLeft < 0 ->
        !:CanTransform = cannot_transform
    ; svqueue__get(Label, !Queue) ->
        !:PropagationStepsLeft = !.PropagationStepsLeft - 1,
        svset__insert(Label, !AlreadyProcessed),
        map__lookup(BlockMap, Label, BlockInfo),
        BlockType = BlockInfo ^ fb_type,
        (
            BlockType = ordinary(_),
            svmap__det_update(Label, block_needs_frame, !OrdNeedsFrame),
            % Putting an already processed label into the queue could
            % lead to an infinite loop. However, we cannot decide whether
            % a label has been processed by checking whether !.OrdNeedsFrame
            % maps Label to yes, since !.OrdNeedsFrame doesn't mention setup
            % frames, and we want to set !:CanTransform to no if any successor
            % is a setup frame. We cannot assume that successors not in
            % !.OrdNeedsFrame should set !:CanTransform to no either, since
            % we don't want to do that for teardown frames.
            list__filter(set__contains(!.AlreadyProcessed),
                successors(BlockInfo), _, UnprocessedSuccessors),
            svqueue__put_list(UnprocessedSuccessors, !Queue)
        ;
            BlockType = setup,
            !:CanTransform = cannot_transform
        ;
            BlockType = teardown(_, _, _)
            % Teardown frames never *need* stack frames.
        ),
        propagate_frame_requirement_to_successors(!.Queue, BlockMap,
            !OrdNeedsFrame, !.AlreadyProcessed, !PropagationStepsLeft,
            !CanTransform)
    ;
        true
    ).

    % This predicate implements the third part of the first phase of
    % delay_frame_transform; see the documentation there.
    %
:- pred propagate_frame_requirement_to_predecessors(queue(label)::in,
    frame_block_map::in, rev_map::in,
    ord_needs_frame::in, ord_needs_frame::out, int::in, int::out,
    can_transform::in, can_transform::out) is det.

propagate_frame_requirement_to_predecessors(!.Queue, BlockMap, RevMap,
        !OrdNeedsFrame, !PropagationStepsLeft, !CanTransform) :-
    ( !.CanTransform = cannot_transform ->
        true
    ; !.PropagationStepsLeft < 0 ->
        !:CanTransform = cannot_transform
    ; svqueue__get(Label, !Queue) ->
        !:PropagationStepsLeft = !.PropagationStepsLeft - 1,
        ( map__search(RevMap, Label, PredecessorsPrime) ->
            Predecessors = PredecessorsPrime
        ;
            % We get here if Label cannot be reached by a fallthrough or an
            % explicit jump, but only by backtracking. In that case, the code
            % that sets up the resumption point saves the address of Label on
            % the stack, and thus is already known to need a stack frame.
            Predecessors = [],
            svmap__det_update(Label, block_needs_frame, !OrdNeedsFrame)
        ),
        list__filter(all_successors_need_frame(BlockMap, !.OrdNeedsFrame),
            Predecessors, NowNeedFrameLabels),
        list__foldl2(record_frame_need(BlockMap), NowNeedFrameLabels,
            !OrdNeedsFrame, !CanTransform),
        svqueue__put_list(NowNeedFrameLabels, !Queue),
        propagate_frame_requirement_to_predecessors(!.Queue, BlockMap,
            RevMap, !OrdNeedsFrame, !PropagationStepsLeft, !CanTransform)
    ;
        true
    ).

:- pred record_frame_need(frame_block_map::in, label::in,
    ord_needs_frame::in, ord_needs_frame::out,
    can_transform::in, can_transform::out) is det.

record_frame_need(BlockMap, Label, !OrdNeedsFrame, !CanTransform) :-
    map__lookup(BlockMap, Label, BlockInfo),
    BlockType = BlockInfo ^ fb_type,
    (
        BlockType = setup,
        !:CanTransform = cannot_transform
    ;
        BlockType = ordinary(_),
        svmap__det_update(Label, block_needs_frame, !OrdNeedsFrame)
    ;
        BlockType = teardown(_, _, _),
        unexpected(this_file, "record_frame_need: teardown")
    ).

:- pred all_successors_need_frame(frame_block_map::in, ord_needs_frame::in,
    label::in) is semidet.

all_successors_need_frame(BlockMap, OrdNeedsFrame, Label) :-
    map__lookup(BlockMap, Label, BlockInfo),
    Successors = successors(BlockInfo),
    list__filter(label_needs_frame(OrdNeedsFrame), Successors,
        _NeedFrameSuccessors, NoNeedFrameSuccessors),
    NoNeedFrameSuccessors = [].

:- pred label_needs_frame(ord_needs_frame::in, label::in) is semidet.

label_needs_frame(OrdNeedsFrame, Label) :-
    ( map__search(OrdNeedsFrame, Label, NeedsFrame) ->
        NeedsFrame = block_needs_frame
    ;
        % If the map__search fails, Label is not an ordinary frame.
        % Setup blocks and teardown blocks don't need frames.
        fail
    ).

    % Returns the set of successors of the given block as a list
    % (which may contain duplicates).
    %
:- func successors(frame_block_info) = list(label).

successors(BlockInfo) = Successors :-
    SideLabels = BlockInfo ^ fb_jump_dests,
    MaybeFallThrough = BlockInfo ^ fb_fall_dest,
    (
        MaybeFallThrough = no,
        Successors = SideLabels
    ;
        MaybeFallThrough = yes(FallThrough),
        Successors = [FallThrough | SideLabels]
    ).

%-----------------------------------------------------------------------------%

    % The predicates process_frame_delay and transform_ordinary_block
    % implement the second phase of delay_frame_transform. For documentation,
    % see the comment at the top of delay_frame_transform.
    %
:- pred process_frame_delay(list(label)::in, ord_needs_frame::in,
    proc_label::in, counter::in, counter::out,
    frame_block_map::in, frame_block_map::out,
    setup_par_map::in, setup_par_map::out,
    teardown_par_map::in, teardown_par_map::out) is det.

process_frame_delay([], _, _, !C, !BlockMap,
        !SetupParMap, !TeardownParMap).
process_frame_delay([Label0 | Labels0], OrdNeedsFrame, ProcLabel, !C,
        !BlockMap, !SetupParMap, !TeardownParMap) :-
    map__lookup(!.BlockMap, Label0, BlockInfo0),
    BlockInfo0 = frame_block_info(Label0Copy, Instrs0, FallInto, SideLabels0,
        MaybeFallThrough0, Type),
    expect(unify(Label0, Label0Copy), this_file,
        "process_frame_delay: label in frame_block_info is not copy"),
    (
        Type = setup,
        (
            MaybeFallThrough0 = yes(_FallThrough)
        ;
            MaybeFallThrough0 = no,
            unexpected(this_file,
                "process_frame_delay: no fallthrough for setup block")
        ),
        (
            SideLabels0 = []
        ;
            SideLabels0 = [_ | _],
            unexpected(this_file,
                "process_frame_delay: nonempty side labels for setup block")
        ),
        (
            Instrs0 = [LabelInstrPrime | _],
            LabelInstrPrime = label(_) - _
        ->
            LabelInstr = LabelInstrPrime
        ;
            unexpected(this_file,
                "process_frame_delay: setup block does not begin with label")
        ),
        BlockInfo = frame_block_info(Label0, [LabelInstr], FallInto,
            SideLabels0, MaybeFallThrough0, ordinary(block_doesnt_need_frame)),
        svmap__det_update(Label0, BlockInfo, !BlockMap),
        process_frame_delay(Labels0, OrdNeedsFrame,
            ProcLabel, !C, !BlockMap, !SetupParMap, !TeardownParMap)
    ;
        Type = ordinary(_),
        map__lookup(OrdNeedsFrame, Label0, NeedsFrame),
        (
            NeedsFrame = block_needs_frame,
            % Every block reachable from this block, whether via jump or
            % fallthrough, will be an ordinary block also mapped to `yes'
            % by OrdNeedsFrame, or will be a teardown block. We already have
            % a stack frame, and all our successors expect one, so we need not
            % do anything.
            process_frame_delay(Labels0, OrdNeedsFrame, ProcLabel, !C,
                !BlockMap, !SetupParMap, !TeardownParMap)
        ;
            NeedsFrame = block_doesnt_need_frame,
            transform_nostack_ordinary_block(Label0, Labels0, BlockInfo0,
                OrdNeedsFrame, ProcLabel, !C, !BlockMap,
                !SetupParMap, !TeardownParMap)
        )
    ;
        Type = teardown(_, _, _),
        process_frame_delay(Labels0, OrdNeedsFrame, ProcLabel, !C,
            !BlockMap, !SetupParMap, !TeardownParMap)
    ).

    % Transform an ordinary block that doesn't have a stack frame.
    % Every block that is a successor of this block, whether via jump or
    % fallthrough, will be an ordinary block or a teardown block.
    %
    % - If it is an ordinary block that doesn't need a frame, we need not
    %   do anything.
    %
    % - If it is an ordinary block B that does need a stack frame, we need to
    %   insert the frame setup code at the transfer between the two blocks.
    %   The label S of the block that contains the setup code and then goes
    %   to block B will be given by map__lookup(!.SetupParMap, B, S).
    %   Here, we just allocate the label S; the block will be created later.
    %
    % - If it is teardown block B, then we need to jump to a variant of B
    %   that does no teardown, since there is no stack frame to tear down.
    %   The label S of the variant block will be given by
    %   map__lookup(!.TeardownParMap, B, S). Here, we just allocate
    %   the label S; the block will be created later.
    %
:- pred transform_nostack_ordinary_block(label::in, list(label)::in,
    frame_block_info::in, ord_needs_frame::in,
    proc_label::in, counter::in, counter::out,
    frame_block_map::in, frame_block_map::out,
    setup_par_map::in, setup_par_map::out,
    teardown_par_map::in, teardown_par_map::out) is det.

transform_nostack_ordinary_block(Label0, Labels0, BlockInfo0, OrdNeedsFrame,
        ProcLabel, !C, !BlockMap, !SetupParMap, !TeardownParMap) :-
    BlockInfo0 = frame_block_info(_, Instrs0, FallInto,
        SideLabels0, MaybeFallThrough0, Type),
    mark_parallels_for_nostack_successors(SideLabels0, SideLabels,
        SideAssocLabelMap, OrdNeedsFrame, !.BlockMap, ProcLabel, !C,
        !SetupParMap, !TeardownParMap),
    (
        MaybeFallThrough0 = yes(FallThroughLabel0),
        mark_parallel_for_nostack_successor(FallThroughLabel0,
            FallThroughLabel, OrdNeedsFrame, !.BlockMap, ProcLabel, !C,
            !SetupParMap, !TeardownParMap),
        MaybeFallThrough = yes(FallThroughLabel),
        expect(no_disagreement(SideAssocLabelMap,
            FallThroughLabel0, FallThroughLabel), this_file,
            "transform_nostack_ordinary_block: disagreement"),
        AssocLabelMap = [FallThroughLabel0 - FallThroughLabel
            | SideAssocLabelMap],
        ( FallThroughLabel = FallThroughLabel0 ->
            RedirectFallThrough = []
        ;
            RedirectFallThrough = [goto(label(FallThroughLabel))
                - "redirect fallthrough"]
            % We can expect this jump to be optimized away in most cases.
        )
    ;
        MaybeFallThrough0 = no,
        MaybeFallThrough = no,
        AssocLabelMap = SideAssocLabelMap,
        RedirectFallThrough = []
    ),
    pick_last(Instrs0, PrevInstrs, LastInstr0),
    map__from_assoc_list(AssocLabelMap, LabelMap),
    opt_util__replace_labels_instruction(LastInstr0, LabelMap, no, LastInstr),
    Instrs = PrevInstrs ++ [LastInstr | RedirectFallThrough],
    BlockInfo = frame_block_info(Label0, Instrs, FallInto,
        SideLabels, MaybeFallThrough, Type),
    map__set(!.BlockMap, Label0, BlockInfo, !:BlockMap),
    process_frame_delay(Labels0, OrdNeedsFrame, ProcLabel, !C, !BlockMap,
        !SetupParMap, !TeardownParMap).

:- pred no_disagreement(assoc_list(label, label)::in, label::in, label::in)
    is semidet.

no_disagreement([], _, _).
no_disagreement([K - V | KVs], Key, Value) :-
    ( K = Key => V = Value ),
    no_disagreement(KVs, Key, Value).

%-----------------------------------------------------------------------------%

    % Invokes mark_parallel_for_nostack_successor on each input label,
    % and returns both the updated list of labels and the substitution
    % (represented as an association list) that will have to applied
    % to the jumping instruction.
    %
:- pred mark_parallels_for_nostack_successors(list(label)::in,
    list(label)::out, assoc_list(label)::out, ord_needs_frame::in,
    frame_block_map::in, proc_label::in, counter::in, counter::out,
    setup_par_map::in, setup_par_map::out,
    teardown_par_map::in, teardown_par_map::out) is det.

mark_parallels_for_nostack_successors([], [], [], _, _, _, !C,
        !SetupParMap, !TeardownParMap).
mark_parallels_for_nostack_successors([Label0 | Labels0], [Label | Labels],
        [Label0 - Label | LabelMap], OrdNeedsFrame, BlockMap, ProcLabel, !C,
        !SetupParMap, !TeardownParMap) :-
    mark_parallel_for_nostack_successor(Label0, Label,
        OrdNeedsFrame, BlockMap, ProcLabel, !C, !SetupParMap, !TeardownParMap),
    mark_parallels_for_nostack_successors(Labels0, Labels, LabelMap,
        OrdNeedsFrame, BlockMap, ProcLabel, !C, !SetupParMap, !TeardownParMap).

    % Label0 is a label that is a successor of a block which has no stack
    % frame.
    %
    % If Label0 starts a teardown block, we ensure that it has a non-teardown
    % parallel Label.
    %
    % If Label0 starts an ordinary block that needs a stack frame, we ensure
    % that it has a parallel Label that allocates a stack frame before handing
    % control to Label0.
    %
:- pred mark_parallel_for_nostack_successor(label::in, label::out,
    ord_needs_frame::in, frame_block_map::in, proc_label::in,
    counter::in, counter::out, setup_par_map::in, setup_par_map::out,
    teardown_par_map::in, teardown_par_map::out) is det.

mark_parallel_for_nostack_successor(Label0, Label, OrdNeedsFrame, BlockMap,
        ProcLabel, !C, !SetupParMap, !TeardownParMap) :-
    map__lookup(BlockMap, Label0, BlockInfo),
    Type = BlockInfo ^ fb_type,
    (
        Type = setup,
        unexpected(this_file, "mark_parallels_for_nostack_jump: " ++
            "reached setup via jump from ordinary block")
    ;
        Type = ordinary(_),
        map__lookup(OrdNeedsFrame, Label0, NeedsFrame),
        (
            NeedsFrame = block_needs_frame,
            ensure_setup_parallel(Label0, Label, ProcLabel, !C, !SetupParMap)
        ;
            NeedsFrame = block_doesnt_need_frame,
            Label = Label0
        )
    ;
        Type = teardown(_, _, _),
        ensure_teardown_parallel(Label0, Label, ProcLabel, !C, !TeardownParMap)
    ).

%-----------------------------------------------------------------------------%

    % The third phase of the delay_frame_transform optimization, creating
    %
    % - the setup code of ordinary blocks that need frames but (some of)
    %   whose predecessors don't have one, and
    %
    % - the parallels of teardown blocks that can assume there is no frame to
    %   tear down.
    %
:- pred create_parallels(list(label)::in, list(label)::out, int::in,
    string::in, proc_label::in, counter::in, counter::out, ord_needs_frame::in,
    setup_par_map::in, teardown_par_map::in, pred_map::in,
    frame_block_map::in, frame_block_map::out) is det.

create_parallels([], [], _, _, _, !C, _, _, _, _, !BlockMap).
create_parallels([Label0 | Labels0], Labels, FrameSize, Msg, ProcLabel, !C,
        OrdNeedsFrame, SetupParMap, TeardownParMap, PredMap, !BlockMap) :-
    create_parallels(Labels0, Labels1, FrameSize, Msg, ProcLabel, !C,
        OrdNeedsFrame, SetupParMap, TeardownParMap, PredMap, !BlockMap),
    map__lookup(!.BlockMap, Label0, BlockInfo0),
    BlockInfo0 = frame_block_info(Label0Copy, _, FallInto,
        SideLabels, MaybeFallThrough, Type),
    expect(unify(Label0, Label0Copy), this_file,
        "create_parallels: label in frame_block_info is not copy"),
    ( search_teardown_par_map(TeardownParMap, Label0, ParallelLabel) ->
        expect(unify(MaybeFallThrough, no), this_file,
            "create_parallels: teardown block with parallel has fall through"),
        (
            SideLabels = [],
            Comments = []
        ;
            SideLabels = [_ | _],
            % This can happen if fulljump optimization has redirected the
            % return.
            Comments = [comment("teardown side labels "
                ++ dump_labels(ProcLabel, SideLabels)) - ""]
        ),
        PrevNeedsFrame = prev_block_needs_frame(OrdNeedsFrame, BlockInfo0),
        ( Type = teardown(_, Livevals, Goto) ->
            LabelInstr = label(ParallelLabel) - "non-teardown parallel",
            ReplacementCode = [LabelInstr] ++ Comments ++ Livevals ++ [Goto],
            (
                PrevNeedsFrame = block_doesnt_need_frame,
                Labels = [ParallelLabel, Label0 | Labels1],
                BlockInfo = BlockInfo0 ^ fb_fallen_into := no,
                svmap__det_update(Label0, BlockInfo, !BlockMap),
                ParallelBlockFallInto = FallInto
            ;
                PrevNeedsFrame = block_needs_frame,
                Labels = [Label0, ParallelLabel | Labels1],
                ParallelBlockFallInto = no
            ),
            ParallelBlockInfo = frame_block_info(ParallelLabel,
                ReplacementCode, ParallelBlockFallInto, SideLabels,
                no, ordinary(block_doesnt_need_frame)),
            svmap__det_insert(ParallelLabel, ParallelBlockInfo, !BlockMap)
        ;
            unexpected(this_file,
                "block in teardown_par_map is not teardown")
        )
    ; search_setup_par_map(SetupParMap, Label0, SetupLabel) ->
        expect(is_ordinary(Type), this_file,
            "create_parallels: block in setup map is not ordinary"),
        PrevNeedsFrame = prev_block_needs_frame(OrdNeedsFrame, BlockInfo0),
        (
            PrevNeedsFrame = block_needs_frame,
            counter__allocate(N, !C),
            JumpAroundLabel = internal(N, ProcLabel),
            % By not including a label instruction at the start of
            % JumpAroundCode, we are breaking an invariant of frame_block_maps.
            % However, we don't execute any code during or after
            % create_parallels that depends on that invariant, and not
            % including the label saves memory and reduces the amount of work
            % labelopt has to do. (The label *would* be optimized away, since
            % it can't be referred to from anywhere.)
            JumpAroundCode = [goto(label(Label0)) - "jump around setup"],
            Labels = [JumpAroundLabel, SetupLabel, Label0 | Labels1],
            JumpAroundBlockInfo = frame_block_info(JumpAroundLabel,
                JumpAroundCode, no, [Label0], FallInto,
                ordinary(block_needs_frame)),
            svmap__det_insert(JumpAroundLabel, JumpAroundBlockInfo, !BlockMap),
            SetupFallInto = yes(JumpAroundLabel),
            BlockInfo = BlockInfo0 ^ fb_fallen_into := yes(SetupLabel),
            svmap__det_update(Label0, BlockInfo, !BlockMap)
        ;
            PrevNeedsFrame = block_doesnt_need_frame,
            Labels = [SetupLabel, Label0 | Labels1],
            SetupFallInto = no
        ),
        SetupCode = [
            label(SetupLabel) - "late setup label",
            incr_sp(FrameSize, Msg) - "late setup",
            assign(stackvar(FrameSize), lval(succip)) - "late save"
        ],
        SetupBlockInfo = frame_block_info(SetupLabel, SetupCode,
            SetupFallInto, [], yes(Label0), setup),
        svmap__det_insert(SetupLabel, SetupBlockInfo, !BlockMap)
    ;
        Labels = [Label0 | Labels1]
    ).

:- func prev_block_needs_frame(ord_needs_frame, frame_block_info) =
    block_needs_frame.

prev_block_needs_frame(OrdNeedsFrame, BlockInfo) = PrevNeedsFrame :-
    MaybeFallIntoFrom = BlockInfo ^ fb_fallen_into,
    (
        MaybeFallIntoFrom = yes(FallIntoFrom),
        ( map__search(OrdNeedsFrame, FallIntoFrom, NeedsFrame) ->
            % FallIntoFrom is an ordinary block that can fall through
            % to this block.
            PrevNeedsFrame = NeedsFrame
        ;
            % FallIntoFrom is a setup block; teardown blocks cannot fall
            % through. Setup blocks don't need frames.
            PrevNeedsFrame = block_doesnt_need_frame
        )
    ;
        MaybeFallIntoFrom = no,
        % The previous block doesn't care whether the following block
        % has a frame or not.
        PrevNeedsFrame = block_doesnt_need_frame
    ).

:- pred is_ordinary(block_type::in) is semidet.

is_ordinary(ordinary(_)).

%-----------------------------------------------------------------------------%

    % Given the label of a block, allocate a label for its parallel
    % in the given setup map if it doesn't already have one.
    %
:- pred ensure_setup_parallel(label::in, label::out, proc_label::in,
    counter::in, counter::out, setup_par_map::in, setup_par_map::out) is det.

ensure_setup_parallel(Label, ParallelLabel, ProcLabel, !C, !SetupParMap) :-
    !.SetupParMap = setup_par_map(ParMap0),
    ( map__search(ParMap0, Label, OldParallel) ->
        ParallelLabel = OldParallel
    ;
        counter__allocate(N, !C),
        NewParallel = internal(N, ProcLabel),
        ParallelLabel = NewParallel,
        map__det_insert(ParMap0, Label, NewParallel, ParMap),
        !:SetupParMap = setup_par_map(ParMap)
    ).

    % Given the label of a block, allocate a label for its parallel
    % in the given teardown map if it doesn't already have one.
    %
:- pred ensure_teardown_parallel(label::in, label::out, proc_label::in,
    counter::in, counter::out, teardown_par_map::in, teardown_par_map::out)
    is det.

ensure_teardown_parallel(Label, ParallelLabel, ProcLabel, !C,
        !TeardownParMap) :-
    !.TeardownParMap = teardown_par_map(ParMap0),
    ( map__search(ParMap0, Label, OldParallel) ->
        ParallelLabel = OldParallel
    ;
        counter__allocate(N, !C),
        NewParallel = internal(N, ProcLabel),
        ParallelLabel = NewParallel,
        map__det_insert(ParMap0, Label, NewParallel, ParMap),
        !:TeardownParMap = teardown_par_map(ParMap)
    ).

%-----------------------------------------------------------------------------%

    % This predicate generates a human-readable description of a block
    % as a comment instruction. This can make it much easier to debug
    % frameopt.
    %
:- pred describe_block(frame_block_map::in, ord_needs_frame::in, pred_map::in,
    proc_label::in, label::in, instruction::out) is det.

describe_block(BlockMap, OrdNeedsFrame, PredMap, ProcLabel, Label, Instr) :-
    map__lookup(BlockMap, Label, BlockInfo),
    BlockInfo = frame_block_info(BlockLabel, BlockInstrs, FallInto,
        SideLabels, MaybeFallThrough, Type),
    expect(unify(Label, BlockLabel), this_file,
        "describe_block: label mismatch"),
    LabelStr = dump_label(ProcLabel, Label),
    BlockInstrsStr = dump_fullinstrs(ProcLabel, yes, BlockInstrs),
    Heading = "\nBLOCK " ++ LabelStr ++ "\n\n",
    ( map__search(PredMap, Label, PredLabel) ->
        PredStr = "previous label " ++ dump_label(ProcLabel, PredLabel) ++ "\n"
    ;
        PredStr = "no previous label\n"
    ),
    (
        FallInto = yes(FallIntoFromLabel),
        FallIntoStr = "fallen into from "
            ++ dump_label(ProcLabel, FallIntoFromLabel) ++ "\n"
    ;
        FallInto = no,
        FallIntoStr = "not fallen into\n"
    ),
    (
        SideLabels = [],
        SideStr = "no side labels\n"
    ;
        SideLabels = [_ | _],
        SideStr = "side labels " ++ dump_labels(ProcLabel, SideLabels) ++ "\n"
    ),
    (
        MaybeFallThrough = yes(FallThroughLabel),
        FallThroughStr = "falls through to "
            ++ dump_label(ProcLabel, FallThroughLabel) ++ "\n"
    ;
        MaybeFallThrough = no,
        FallThroughStr = "does not fall through\n"
    ),
    (
        Type = setup,
        expect(unify(SideLabels, []), this_file,
            "describe_block: setup, SideLabels=[_ | _]"),
        expect(is_yes(MaybeFallThrough), this_file,
            "describe_block: setup, MaybeFallThrough=no"),
        TypeStr = "setup\n",
        OrdNeedsFrameStr = ""
    ;
        Type = ordinary(UsesFrame),
        (
            UsesFrame = block_needs_frame,
            TypeStr = "ordinary; uses frame, "
        ;
            UsesFrame = block_doesnt_need_frame,
            TypeStr = "ordinary; does not use frame, "
        ),
        map__lookup(OrdNeedsFrame, Label, NeedsFrame),
        (
            NeedsFrame = block_doesnt_need_frame,
            expect(unify(UsesFrame, block_doesnt_need_frame), this_file,
                "describe_block: "
                ++ "NeedsFrame=block_doesnt_need_frame, "
                ++ "UsesFrame=block_needs_frame"),
            OrdNeedsFrameStr = "does not need frame\n"
        ;
            NeedsFrame = block_needs_frame,
            OrdNeedsFrameStr = "does need frame\n"
        )
    ;
        Type = teardown(RestoreSuccip, Livevals, Goto),
        expect(unify(MaybeFallThrough, no), this_file,
            "describe_block: teardown, MaybeFallThrough=yes(_)"),
        TypeStr = "teardown\n"
            ++ "restore:  "
            ++ dump_fullinstrs(ProcLabel, yes, RestoreSuccip)
            ++ "livevals: "
            ++ dump_fullinstrs(ProcLabel, yes, Livevals)
            ++ "goto:     "
            ++ dump_fullinstr(ProcLabel, yes, Goto),
        OrdNeedsFrameStr = ""
    ),
    Comment = Heading ++ PredStr ++ FallIntoStr ++ SideStr ++ FallThroughStr
        ++ TypeStr ++ OrdNeedsFrameStr ++ "CODE:\n" ++ BlockInstrsStr,
    Instr = comment(Comment) - "".

:- pred is_yes(maybe(T)::in) is semidet.

is_yes(yes(_)).

%-----------------------------------------------------------------------------%

:- pred search_setup_par_map(setup_par_map::in, label::in, label::out)
    is semidet.

search_setup_par_map(setup_par_map(ParMap), Label, ParallelLabel) :-
    map__search(ParMap, Label, ParallelLabel).

:- pred search_teardown_par_map(teardown_par_map::in, label::in, label::out)
    is semidet.

search_teardown_par_map(teardown_par_map(ParMap), Label, ParallelLabel) :-
    map__search(ParMap, Label, ParallelLabel).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "frameopt.m".

%-----------------------------------------------------------------------------%
:- end_module frameopt.
%-----------------------------------------------------------------------------%
