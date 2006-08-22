%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001,2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: frameopt.m.
% Main author: zs.
%
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
% 
%-----------------------------------------------------------------------------%

:- module ll_backend.frameopt.
:- interface.

:- import_module libs.globals.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % frameopt_main_det_stack(ProcLabel, !LabelCounter, !Instrs, Globals,
    %   AnyChange):
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
:- pred frameopt_main_det_stack(proc_label::in, counter::in, counter::out,
    list(instruction)::in, list(instruction)::out, globals::in, bool::out)
    is det.

    % frameopt_main_nondet_stack(ProcLabel, !LabelCounter, !Instrs, Globals,
    %   AnyChange):
    %
    % The equivalent of frameopt_main_det_stack for procedures that live on
    % the nondet stack, but attempting only the transformation that delays
    % the creation of stack frames.
    %
:- pred frameopt_main_nondet_stack(proc_label::in, counter::in, counter::out,
    list(instruction)::in, list(instruction)::out, globals::in, bool::out)
    is det.

    % frameopt_keep_nondet_frame(ProcLabel, LayoutLabels,
    %   !LabelCounter, !Instrs, AnyChange):
    %
    % The equivalent of frameopt_main_det_stack for procedures that live on
    % the nondet stack, but attempting only the transformation that keeps
    % existing stack frames at tail calls. This should be called before
    % other LLDS optimizations on the procedure body.
    %
    % LayoutLabels should be the set of labels in the procedure with layout
    % structures, while MayAlterRtti should say whether we are allowed to
    % perform optimizations that may interfere with RTTI.
    %
:- pred frameopt_keep_nondet_frame(proc_label::in, set(label)::in,
    counter::in, counter::out, list(instruction)::in, list(instruction)::out,
    bool::out) is det.

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
:- import_module maybe.
:- import_module pair.
:- import_module queue.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svqueue.
:- import_module svset.

%-----------------------------------------------------------------------------%

frameopt_main_det_stack(ProcLabel, !C, Instrs0, Instrs, Globals, Mod) :-
    opt_util.get_prologue(Instrs0, LabelInstr, Comments0, Instrs1),
    ( detect_det_entry(Instrs1, _, _, EntryInfo) ->
        some [!BlockMap] (
            map.init(!:BlockMap),
            divide_into_basic_blocks([LabelInstr | Instrs1], ProcLabel,
                BasicInstrs, !C),
            build_frame_block_map(BasicInstrs, EntryInfo, LabelSeq0, no, no,
                ProcLabel, !BlockMap, map.init, PredMap, !C,
                map.init, PreExitDummyLabelMap),
            analyze_block_map(LabelSeq0, PreExitDummyLabelMap, !BlockMap,
                KeepFrame),
            (
                KeepFrame = yes(FirstLabel - SecondLabel),
                CanClobberSuccip = can_clobber_succip(LabelSeq0, !.BlockMap),
                keep_frame_transform(LabelSeq0, FirstLabel, SecondLabel,
                    CanClobberSuccip, !BlockMap),
                LabelSeq = LabelSeq0,
                NewComment = comment("keeping stack frame") - "",
                Comments = Comments0 ++ [NewComment],
                flatten_block_seq(LabelSeq, !.BlockMap, BodyInstrs),
                Instrs = Comments ++ BodyInstrs,
                Mod = yes
            ;
                KeepFrame = no,
                ( can_delay_frame(LabelSeq0, !.BlockMap) ->
                    delay_frame_transform(LabelSeq0, LabelSeq, EntryInfo,
                        ProcLabel, PredMap, !C, !BlockMap, Globals,
                        TransformComments, DescComments, CanTransform),
                    (
                        CanTransform = can_transform,
                        Comments = Comments0 ++ TransformComments
                            ++ DescComments,
                        flatten_block_seq(LabelSeq, !.BlockMap, BodyInstrs),
                        Instrs = Comments ++ BodyInstrs,
                        Mod = yes
                    ;
                        CanTransform = cannot_transform,
                        maybe_add_comments(Globals, DescComments,
                            Instrs0, Instrs, Mod)
                    )
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

frameopt_main_nondet_stack(ProcLabel, !C, Instrs0, Instrs, Globals, Mod) :-
    opt_util.get_prologue(Instrs0, LabelInstr, Comments0, Instrs1),
    ( detect_nondet_entry(Instrs1, _, _, EntryInfo) ->
        some [!BlockMap] (
            map.init(!:BlockMap),
            divide_into_basic_blocks([LabelInstr | Instrs1], ProcLabel,
                BasicInstrs, !C),
            build_frame_block_map(BasicInstrs, EntryInfo, LabelSeq0, no, no,
                ProcLabel, !BlockMap, map.init, PredMap, !C,
                map.init, PreExitDummyLabelMap),
            analyze_block_map(LabelSeq0, PreExitDummyLabelMap, !BlockMap,
                _KeepFrame),
            ( can_delay_frame(LabelSeq0, !.BlockMap) ->
                delay_frame_transform(LabelSeq0, LabelSeq, EntryInfo,
                    ProcLabel, PredMap, !C, !BlockMap, Globals,
                    TransformComments, DescComments, CanTransform),
                (
                    CanTransform = can_transform,
                    Comments = Comments0 ++ TransformComments ++ DescComments,
                    flatten_block_seq(LabelSeq, !.BlockMap, BodyInstrs),
                    Instrs = Comments ++ BodyInstrs,
                    Mod = yes
                ;
                    CanTransform = cannot_transform,
                    maybe_add_comments(Globals, DescComments,
                        Instrs0, Instrs, Mod)
                )
            ;
                Instrs = Instrs0,
                Mod = no
            )
        )
    ;
        Instrs = Instrs0,
        Mod = no
    ).

:- pred maybe_add_comments(globals::in, list(instruction)::in,
    list(instruction)::in, list(instruction)::out, bool::out) is det.

maybe_add_comments(Globals, DescComments, Instrs0, Instrs, Mod) :-
    globals.lookup_bool_option(Globals, frameopt_comments, FrameoptComments),
    (
        FrameoptComments = no,
        Instrs = Instrs0,
        Mod = no
    ;
        FrameoptComments = yes,
        Instrs =
            [comment("could not delay frame creation") - ""]
            ++ DescComments ++ Instrs0,
        Mod = yes
    ).

%-----------------------------------------------------------------------------%

frameopt_keep_nondet_frame(ProcLabel, LayoutLabels, !C, Instrs0, Instrs,
        Mod) :-
    opt_util.get_prologue(Instrs0, LabelInstr, Comments0, Instrs1),
    (
        nondetstack_setup(Instrs1, FrameInfo, Redoip, MkframeInstr, Remain),
        MkframeInstr = MkframeUinstr - MkframeComment,
        find_succeed_labels(Instrs1, map.init, SuccMap),
        counter.allocate(KeepFrameLabelNum, !C),
        KeepFrameLabel = internal(KeepFrameLabelNum, ProcLabel),
        keep_nondet_frame(Remain, Instrs2, ProcLabel, KeepFrameLabel,
            MkframeUinstr, SuccMap, LayoutLabels, no, yes)
    ->
        list.condense([[LabelInstr], Comments0,
            [mkframe(FrameInfo, no) - MkframeComment,
            label(KeepFrameLabel) - "tail recursion target",
            assign(redoip_slot(lval(curfr)),
                const(llconst_code_addr(Redoip))) - ""],
            Instrs2], Instrs),
        Mod = yes
    ;
        Instrs = Instrs0,
        Mod = no
    ).

:- pred nondetstack_setup(list(instruction)::in, nondet_frame_info::out,
    code_addr::out, instruction::out, list(instruction)::out) is semidet.

nondetstack_setup(Instrs0, FrameInfo, Redoip, MkframeInstr, Remain) :-
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
        opt_util.skip_comments(Instrs, TailInstrs),
        opt_util.is_succeed_next(TailInstrs, Between)
    ->
        svmap.det_insert(Label, Between, !SuccMap)
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
        Uinstr0 = llcall(label(entry(_, ProcLabel)), label(RetLabel),
            _, _, _, CallModel),
        CallModel = call_model_nondet(unchecked_tail_call),
        map.search(SuccMap, RetLabel, BetweenIncl),
        BetweenIncl = [livevals(_) - _, goto(_) - _],
        PrevInstr = livevals(Livevals),
        not set.member(RetLabel, LayoutLabels)
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

:- type frame_block_map(En, Ex)   ==  map(label, frame_block_info(En, Ex)).

:- type det_frame_block_map == frame_block_map(det_entry_info, det_exit_info).

:- type frame_block_info(En, Ex)
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

                fb_type         :: block_type(En, Ex)
            ).

:- type block_type(EntryInfo, ExitInfo)
    --->    entry_block(EntryInfo)
    ;       ordinary_block(block_needs_frame(needs_frame_reason), maybe_dummy)
    ;       exit_block(ExitInfo).

:- type maybe_dummy
    --->    is_not_dummy
    ;       is_post_entry_dummy
    ;       is_pre_exit_dummy.

:- type block_needs_frame(T)
    --->    block_needs_frame(T)
    ;       block_doesnt_need_frame.

:- type needs_frame_reasons == set(needs_frame_reason).

:- type needs_frame_reason
    --->    code_needs_frame(label)
            % The code of the block of this label needs a frame.

    ;       keep_frame

    ;       redoip_label

    ;       jump_around(label, needs_frame_reasons)

    ;       frontier(label, needs_frame_reasons)

    ;       succ_propagated(label, needs_frame_reason)
            % The reason given by the second arg is propagated to its
            % successors, including the block of the first argument.

    ;       pred_propagated(label, needs_frame_reason).
            % The reason given by the second arg is propagated to its
            % predecessors, including the block of the first argument.

:- type det_entry_info
    --->    det_entry(
                string,         % The msg of the incr_sp instruction.
                int             % The frame size.
            ).

:- type det_exit_info
    --->    det_exit(           % This block contains det stack teardown
                                % and goto code.
                list(instruction),
                                % The instr that restores succip (if any).
                list(instruction),
                                % The livevals instr before the goto (if any).
                instruction
                                % The goto instr.
            ).

:- type nondet_entry_info
    --->    nondet_entry(
                string,         % The msg of the mkframe instruction.
                int,            % The frame size.
                code_addr       % The initial redoip.
            ).

:- type nondet_exit_info
    --->    nondet_plain_exit(  % This block contains nondet stack exit code
                                % that doesn't throw away the stack frame.
                list(instruction),
                                % The livevals instr before the goto (if any).
                instruction
                                % The goto(do_succeed) instr.
            )
    ;       nondet_teardown_exit(
                                % This block contains nondet stack exit code
                                % that *does* throw away the stack frame.
                instruction,    % The restore of the succip.
                instruction,    % The restore of the maxfr.
                instruction,    % The restore of the curfr.
                list(instruction),
                                % The livevals instr before the goto (if any).
                instruction
                                % The goto(entry(_, _)) instr.
            ).

:- typeclass block_entry_exit(En, Ex) <= ((En -> Ex), (Ex -> En)) where [
    pred detect_entry(list(instruction)::in, list(instruction)::out,
        list(instruction)::out, En::out) is semidet,
    pred detect_exit(list(instruction)::in, En::in, list(instruction)::out,
        list(instruction)::out, list(instruction)::out, Ex::out) is semidet,
    func late_setup_code(En) = list(instruction),
    func non_teardown_exit_code(Ex) = list(instruction),
    func describe_entry(En) = string,
    func describe_exit(proc_label, Ex) = string
].

:- instance block_entry_exit(det_entry_info, det_exit_info) where [
    pred(detect_entry/4) is detect_det_entry,
    pred(detect_exit/6)  is detect_det_exit,
    func(late_setup_code/1) is det_late_setup,
    func(non_teardown_exit_code/1) is det_non_teardown_exit_code,
    func(describe_entry/1) is describe_det_entry,
    func(describe_exit/2)  is describe_det_exit
].

:- instance block_entry_exit(nondet_entry_info, nondet_exit_info) where [
    pred(detect_entry/4) is detect_nondet_entry,
    pred(detect_exit/6)  is detect_nondet_exit,
    func(late_setup_code/1) is nondet_late_setup,
    func(non_teardown_exit_code/1) is nondet_non_teardown_exit_code,
    func(describe_entry/1) is describe_nondet_entry,
    func(describe_exit/2)  is describe_nondet_exit
].

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
    ( opt_util.can_instr_branch_away(Uinstr0, yes) ->
        (
            Instrs0 = [Instr1 | _],
            ( Instr1 = label(_) - _ ->
                divide_into_basic_blocks(Instrs0, ProcLabel, Instrs1, !C),
                Instrs = [Instr0 | Instrs1]
            ;
                counter.allocate(N, !C),
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

:- pred flatten_block_seq(list(label)::in, frame_block_map(En, Ex)::in,
    list(instruction)::out) is det.

flatten_block_seq([], _, []).
flatten_block_seq([Label | Labels], BlockMap, Instrs) :-
    flatten_block_seq(Labels, BlockMap, RestInstrs),
    map.lookup(BlockMap, Label, BlockInfo),
    BlockInstrs = BlockInfo ^ fb_instrs,
    (
        list.split_last(BlockInstrs, MostInstrs, LastInstr),
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

    % This type is explained in the big comment in the body of
    % build_frame_block_map below.
:- type pre_exit_dummy_label_map == map(label, label).

    % Given an instruction list in which labels mark the start of every
    % basic block, divide it up into basic blocks of one of three types:
    %
    % - entry blocks, blocks that contain only stack setup instructions
    %   (incr_sp and assignment of succip to the bottom stack slot);
    %
    % - ordinary blocks that contain neither a stack setup nor a
    %   stack teardown;
    %
    % - exit blocks that remove an existing stack frame.
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
:- pred build_frame_block_map(list(instruction)::in, En::in,
    list(label)::out, maybe(label)::in, maybe(label)::in, proc_label::in,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out,
    pred_map::in, pred_map::out, counter::in, counter::out,
    pre_exit_dummy_label_map::in, pre_exit_dummy_label_map::out) is det
    <= block_entry_exit(En, Ex).

build_frame_block_map([], _, [], _, _, _, !BlockMap, !PredMap, !C,
        !PreExitDummyLabelMap).
build_frame_block_map([Instr0 | Instrs0], EntryInfo, LabelSeq,
        MaybePrevLabel, FallInto, ProcLabel, !BlockMap, !PredMap, !C,
        !PreExitDummyLabelMap) :-
    ( Instr0 = label(Label) - _ ->
        (
            MaybePrevLabel = yes(PrevLabel),
            svmap.det_insert(Label, PrevLabel, !PredMap)
        ;
            MaybePrevLabel = no
        ),
        (
            detect_entry(Instrs0, EntryInstrs, Instrs1, EntryInfo)
        ->
            % Create a block with just the entry instructions in it,
            % followed by an empty block. The reason why we need the empty
            % block is that process_frame_delay below doesn't handle any
            % transition from entry blocks directly to exit blocks.
            % Fixing that would be complicated; this fix is simpler.
            %
            % We would like to put EmptyLabel *after* FallThroughLabel
            % to make the next invocation of labelopt eliminate EmptyLabel
            % rather than FallThroughLabel, but doing that would require
            % updating LabelSeq and BlockMap.

            counter.allocate(EmptyN, !C),
            EmptyLabel = internal(EmptyN, ProcLabel),

            % The fb_jump_dests and fb_fall_dest fields are only dummies.
            FallThroughToEmptyInstr = goto(label(EmptyLabel)) - "fall through",
            BlockInfo = frame_block_info(Label,
                [Instr0 | EntryInstrs] ++ [FallThroughToEmptyInstr],
                FallInto, [], no, entry_block(EntryInfo)),

            % Ensure that the left over, non-entry part of the original block
            % starts with a label, and that the empty block ends with a goto
            % that falls through to this label.
            (
                Instrs1 = [Instr1 | _],
                Instr1 = label(NextLabelPrime) - _
            ->
                NextLabel = NextLabelPrime,
                Instrs2 = Instrs1
            ;
                counter.allocate(N, !C),
                NextLabel = internal(N, ProcLabel),
                NextLabelInstr = label(NextLabel) - "",
                Instrs2 = [NextLabelInstr | Instrs1]
            ),

            EmptyLabelInstr = label(EmptyLabel) - "",
            FallThroughFromEmptyInstr =
                goto(label(NextLabel)) - "fall through",
            % The fb_jump_dests and fb_fall_dest fields are only dummies.
            EmptyBlockType = ordinary_block(block_doesnt_need_frame,
                is_post_entry_dummy),
            EmptyBlockInfo = frame_block_info(EmptyLabel,
                [EmptyLabelInstr, FallThroughFromEmptyInstr],
                yes(Label), [], no, EmptyBlockType),

            build_frame_block_map(Instrs2, EntryInfo, LabelSeq0,
                yes(EmptyLabel), yes(EmptyLabel), ProcLabel, !BlockMap,
                !PredMap, !C, !PreExitDummyLabelMap),
            svmap.det_insert(Label, BlockInfo, !BlockMap),
            svmap.det_insert(EmptyLabel, EmptyBlockInfo, !BlockMap),
            LabelSeq = [Label, EmptyLabel | LabelSeq0]
        ;
            detect_exit(Instrs0, EntryInfo, Extra, ExitInstrs,
                Remain, ExitInfo)
        ->
            % We always insert an ordinary block before the exit block,
            % because doing otherwise could lead to a violation of our
            % invariant that exit blocks never *need* a stack frame
            % in cases where the redoip of a nondet frame is assigned
            % the label of the exit block. By inserting a dummy block
            % before the exit block if necessary, we ensure that the redoip
            % points not to the exit block but to the ordinary block
            % preceding it.
            %
            % However, having other blocks jump to a pre_exit dummy block
            % instead of the exit block loses opportunities for optimization.
            % This is because we may (and typically will) fall into the
            % pre_exit dummy block from an ordinary block that needs a stack
            % frame, and this requirement is propagated to the pre_exit dummy
            % block by propagate_frame_requirement_to_successors, which will
            % cause propagate_frame_requirement_to_predecessors to propagate
            % that requirement to blocks that don't actually need a stack
            % frame. This is why in the is_pre_exit_dummy case below we record
            % the mapping from the label of the pre_exit dummy block to the
            % label of the exit block, so that we can alter instructions
            % that jump to the first label to proceed to the second. Since
            % there is no code between those two labels (except a dummy goto
            % instruction that implements the fallthrough), this is safe.
            % To avoid violating the invariant mentioned at the top of this
            % comment, we don't substitute labels used as code addresses
            % in assignments to redoip slots.
            counter.allocate(N, !C),
            ExitLabel = internal(N, ProcLabel),

            compute_block_needs_frame(Label, Extra, NeedsFrame),
            FallThroughInstr = goto(label(ExitLabel)) - "fall through",
            % The fb_jump_dests and fb_fall_dest fields are only dummies.
            (
                Extra = [],
                expect(unify(NeedsFrame, block_doesnt_need_frame), this_file,
                    "build_frame_block_map: [] needs frame"),
                svmap.det_insert(Label, ExitLabel, !PreExitDummyLabelMap),
                ExtraBlockType = ordinary_block(NeedsFrame, is_pre_exit_dummy)
            ;
                Extra = [_ | _],
                ExtraBlockType = ordinary_block(NeedsFrame, is_not_dummy)
            ),
            ExtraInstrs = [Instr0 | Extra] ++ [FallThroughInstr],
            ExtraInfo = frame_block_info(Label, ExtraInstrs, FallInto,
                [], no, ExtraBlockType),

            ExitLabelInstr = label(ExitLabel) - "",
            LabelledBlock = [ExitLabelInstr | ExitInstrs],
            % The fb_jump_dests and fb_fall_dest fields are only dummies.
            ExitBlockInfo = frame_block_info(ExitLabel, LabelledBlock,
                yes(Label), [], no, exit_block(ExitInfo)),
            svmap.det_insert(ExitLabel, Label, !PredMap),

            build_frame_block_map(Remain, EntryInfo, LabelSeq0, yes(ExitLabel),
                no, ProcLabel, !BlockMap, !PredMap, !C, !PreExitDummyLabelMap),

            svmap.det_insert(ExitLabel, ExitBlockInfo, !BlockMap),
            svmap.det_insert(Label, ExtraInfo, !BlockMap),
            LabelSeq = [Label, ExitLabel | LabelSeq0]
        ;
            opt_util.skip_to_next_label(Instrs0, Block, Instrs1),
            compute_block_needs_frame(Label, Block, NeedsFrame),
            BlockInstrs = [Instr0 | Block],
            % The fb_jump_dests and fb_fall_dest fields are only dummies.
            BlockInfo = frame_block_info(Label, BlockInstrs, FallInto,
                [], no, ordinary_block(NeedsFrame, is_not_dummy)),
            ( list.last(BlockInstrs, LastBlockInstr) ->
                LastBlockInstr = LastBlockUinstr - _,
                opt_util.can_instr_fall_through(LastBlockUinstr,
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
            build_frame_block_map(Instrs1, EntryInfo, LabelSeq0, yes(Label),
                NextFallInto, ProcLabel, !BlockMap, !PredMap, !C,
                !PreExitDummyLabelMap),
            svmap.det_insert(Label, BlockInfo, !BlockMap),
            LabelSeq = [Label | LabelSeq0]
        )
    ;
        unexpected(this_file,
            "build_frame_block_map; block does not start with label")
    ).

%-----------------------------------------------------------------------------%

    % Does the given code start with a setup of a det stack frame? If yes,
    % return the size of the frame and two instruction sequences,
    % Setup and Others ++ Remain. Setup is the instruction sequence
    % that sets up the det stack frame, Others is a sequence of
    % non-interfering instructions that were interspersed with Setup
    % but can be moved after Setup, and Remain is all remaining instructions.
    %
:- pred detect_det_entry(list(instruction)::in, list(instruction)::out,
    list(instruction)::out, det_entry_info::out) is semidet.

detect_det_entry(Instrs0, Setup, Others ++ Remain, EntryInfo) :-
    opt_util.gather_comments(Instrs0, Others0, Instrs1),
    Instrs1 = [SetupInstr1 | Instrs2],
    SetupInstr1 = incr_sp(FrameSize, Msg) - _,
    detstack_setup(Instrs2, FrameSize, SetupInstr2, Others0, Others, Remain),
    Setup = [SetupInstr1, SetupInstr2],
    EntryInfo = det_entry(Msg, FrameSize).

:- pred detstack_setup(list(instruction)::in, int::in, instruction::out,
    list(instruction)::in, list(instruction)::out, list(instruction)::out)
    is semidet.

detstack_setup([Instr0 | Instrs0], FrameSize, Setup, !Others, Remain) :-
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
            detstack_setup(Instrs0, FrameSize, Setup, !Others, Remain)
        ;
            fail
        )
    ; Instr0 = comment(_) - _ ->
        !:Others = !.Others ++ [Instr0],
        detstack_setup(Instrs0, FrameSize, Setup, !Others, Remain)
    ;
        fail
    ).

:- pred detect_nondet_entry(list(instruction)::in, list(instruction)::out,
    list(instruction)::out, nondet_entry_info::out) is semidet.

detect_nondet_entry(Instrs0, [MkframeInstr], Remain, EntryInfo) :-
    Instrs0 = [MkframeInstr | Remain],
    MkframeInstr = mkframe(FrameInfo, MaybeRedoip) - _,
    % We could allow MaybeRedoip to be `no', and search for the instruction
    % that sets the redoip of the new frame. That is left for future work.
    MaybeRedoip = yes(Redoip),
    % If the initial mkframe sets the redoip to something other than do_fail,
    % then even this entry block needs a stack frame, so frameopt cannot do
    % anything.
    Redoip = do_fail,
    FrameInfo = ordinary_frame(Msg, Size, no),
    EntryInfo = nondet_entry(Msg, Size, Redoip).

%-----------------------------------------------------------------------------%

:- pred detect_det_exit(list(instruction)::in, det_entry_info::in,
    list(instruction)::out, list(instruction)::out, list(instruction)::out,
    det_exit_info::out) is semidet.

detect_det_exit(Instrs0, EntryInfo, Extra, ExitInstrs, Remain, ExitInfo) :-
    EntryInfo = det_entry(_Msg, FrameSize),
    detstack_teardown(Instrs0, FrameSize, Extra, SuccipRestore, Decrsp,
        Livevals, Goto, Remain),
    ExitInstrs = SuccipRestore ++ Decrsp ++ Livevals ++ [Goto],
    ExitInfo = det_exit(SuccipRestore, Livevals, Goto).

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
:- pred detstack_teardown(list(instruction)::in, int::in,
    list(instruction)::out, list(instruction)::out,
    list(instruction)::out, list(instruction)::out,
    instruction::out, list(instruction)::out) is semidet.

detstack_teardown([Instr0 | Instrs0], FrameSize, Extra, SuccipRestore, Decrsp,
        Livevals, Goto, Remain) :-
    (
        Instr0 = label(_) - _
    ->
        fail
    ;
        detstack_teardown_2([Instr0 | Instrs0], FrameSize,
            [], ExtraPrime, [], SuccipRestorePrime, [], DecrspPrime,
            [], LivevalsPrime, GotoPrime, RemainPrime)
    ->
        Extra = ExtraPrime,
        SuccipRestore = SuccipRestorePrime,
        Decrsp = DecrspPrime,
        Livevals = LivevalsPrime,
        Goto = GotoPrime,
        Remain = RemainPrime
    ;
        detstack_teardown(Instrs0, FrameSize, Extra1, SuccipRestore, Decrsp,
            Livevals, Goto, Remain),
        Extra = [Instr0 | Extra1]
    ).

:- pred detstack_teardown_2(list(instruction)::in, int::in,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    instruction::out, list(instruction)::out) is semidet.

detstack_teardown_2(Instrs0, FrameSize, !Extra, !SuccipRestore, !Decrsp,
        !Livevals, Goto, Remain) :-
    opt_util.skip_comments(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    Instr1 = Uinstr1 - _,
    % XXX allow other instruction types in Extras, e.g. incr_hp
    (
        Uinstr1 = assign(Lval, Rval),
        (
            Lval = succip,
            Rval = lval(stackvar(FrameSize))
        ->
            !.SuccipRestore = [],
            !.Decrsp = [],
            !:SuccipRestore = [Instr1],
            detstack_teardown_2(Instrs2, FrameSize, !Extra, !SuccipRestore,
                !Decrsp, !Livevals, Goto, Remain)
        ;
            opt_util.lval_refers_stackvars(Lval) = no,
            opt_util.rval_refers_stackvars(Rval) = no,
            !:Extra = !.Extra ++ [Instr1],
            detstack_teardown_2(Instrs2, FrameSize, !Extra, !SuccipRestore,
                !Decrsp, !Livevals, Goto, Remain)
        )
    ;
        Uinstr1 = decr_sp(FrameSize),
        !.Decrsp = [],
        !:Decrsp = [Instr1],
        detstack_teardown_2(Instrs2, FrameSize, !Extra, !SuccipRestore,
            !Decrsp, !Livevals, Goto, Remain)
    ;
        Uinstr1 = livevals(_),
        !.Livevals = [],
        !:Livevals = [Instr1],
        detstack_teardown_2(Instrs2, FrameSize, !Extra, !SuccipRestore,
            !Decrsp, !Livevals, Goto, Remain)
    ;
        Uinstr1 = goto(_),
        !.Decrsp = [_],
        Goto = Instr1,
        Remain = Instrs2
    ).

:- pred detect_nondet_exit(list(instruction)::in, nondet_entry_info::in,
    list(instruction)::out, list(instruction)::out, list(instruction)::out,
    nondet_exit_info::out) is semidet.

detect_nondet_exit(Instrs0, _EntryInfo, Extra, ExitInstrs, Remain, ExitInfo) :-
    nondetstack_teardown(Instrs0, Extra, SuccipRestore, Maxfr, Curfr,
        Livevals, Goto, GotoTarget, Remain),
    ExitInstrs = SuccipRestore ++ Maxfr ++ Curfr ++ Livevals ++ [Goto],
    (
        Curfr = [],
        % MR_succeed refers to the current stack frame, so it is valid
        % only if we haven't thrown away the stack frame yet by resetting
        % curfr.
        Maxfr = [],
        SuccipRestore = [],
        GotoTarget = do_succeed(_),
        ExitInfo = nondet_plain_exit(Livevals, Goto)
    ;
        Curfr = [CurfrInstr],
        % If we *have* thrown away the current stack frame, we can exit
        % only via tailcall, and in that case we ought to have also reset
        % maxfr and succip.
        Maxfr = [MaxfrInstr],
        SuccipRestore = [SuccipRestoreInstr],
        ( GotoTarget = label(entry(_, _))
        ; GotoTarget = imported(_)
        ),
        ExitInfo = nondet_teardown_exit(SuccipRestoreInstr,
            MaxfrInstr, CurfrInstr, Livevals, Goto)
    ).

    % Does the following block contain a succeed from a nondet stack frame?
    % If yes, we return
    %
    % - the livevals instruction (if any) as Livevals
    % - the goto instruction as Goto
    %
    % These may be preceded by instructions that do not access the stack;
    % any such instructions are returned as Extra. Remain is all the
    % instructions after the succeed.
    %
:- pred nondetstack_teardown(list(instruction)::in, list(instruction)::out,
    list(instruction)::out, list(instruction)::out,
    list(instruction)::out, list(instruction)::out,
    instruction::out, code_addr::out, list(instruction)::out) is semidet.

nondetstack_teardown([Instr0 | Instrs0], Extra, SuccipRestore, Maxfr, Curfr,
        Livevals, Goto, GotoTarget, Remain) :-
    (
        Instr0 = label(_) - _
    ->
        fail
    ;
        nondetstack_teardown_2([Instr0 | Instrs0], [], ExtraPrime,
            [], SuccipRestorePrime, [], MaxfrPrime, [], CurfrPrime,
            [], LivevalsPrime, GotoPrime, GotoTargetPrime, RemainPrime)
    ->
        Extra = ExtraPrime,
        SuccipRestore = SuccipRestorePrime,
        Maxfr = MaxfrPrime,
        Curfr = CurfrPrime,
        Livevals = LivevalsPrime,
        Goto = GotoPrime,
        GotoTarget = GotoTargetPrime,
        Remain = RemainPrime
    ;
        nondetstack_teardown(Instrs0, Extra1, SuccipRestore, Maxfr, Curfr,
            Livevals, Goto, GotoTarget, Remain),
        Extra = [Instr0 | Extra1]
    ).

:- pred nondetstack_teardown_2(list(instruction)::in,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    list(instruction)::in, list(instruction)::out,
    instruction::out, code_addr::out, list(instruction)::out) is semidet.

nondetstack_teardown_2(Instrs0, !Extra, !SuccipRestore, !Maxfr, !Curfr,
        !Livevals, Goto, GotoTarget, Remain) :-
    opt_util.skip_comments(Instrs0, Instrs1),
    Instrs1 = [Instr1 | Instrs2],
    Instr1 = Uinstr1 - _,
    % XXX allow other instruction types in Extras, e.g. incr_hp
    (
        Uinstr1 = assign(Lval, Rval),
        (
            Lval = succip,
            Rval = lval(succip_slot(lval(curfr))),
            !.SuccipRestore = [],
            % The restore instruction is valid only if curfr hasn't been
            % modified yet.
            !.Curfr = []
        ->
            !:SuccipRestore = [Instr1]
        ;
            Lval = maxfr,
            Rval = lval(prevfr_slot(lval(curfr))),
            !.Maxfr = [],
            % The restore instruction is valid only if curfr hasn't been
            % modified yet.
            !.Curfr = []
        ->
            !:Maxfr = [Instr1]
        ;
            Lval = curfr,
            Rval = lval(succfr_slot(lval(curfr))),
            !.Curfr = []
        ->
            !:Curfr = [Instr1]
        ;
            opt_util.lval_refers_stackvars(Lval) = no,
            opt_util.rval_refers_stackvars(Rval) = no
        ->
            !:Extra = !.Extra ++ [Instr1]
        ;
            fail
        ),
        nondetstack_teardown_2(Instrs2, !Extra, !SuccipRestore, !Maxfr, !Curfr,
            !Livevals, Goto, GotoTarget, Remain)
    ;
        Uinstr1 = livevals(_),
        !.Livevals = [],
        !:Livevals = [Instr1],
        nondetstack_teardown_2(Instrs2, !Extra, !SuccipRestore, !Maxfr, !Curfr,
            !Livevals, Goto, GotoTarget, Remain)
    ;
        Uinstr1 = goto(GotoTarget),
        Goto = Instr1,
        Remain = Instrs2
    ).

%-----------------------------------------------------------------------------%

    % Does an ordinary block with the given content need a stack frame?
    %
:- pred compute_block_needs_frame(label::in, list(instruction)::in,
    block_needs_frame(needs_frame_reason)::out) is det.

compute_block_needs_frame(Label, Instrs, NeedsFrame) :-
    opt_util.block_refers_to_stack(Instrs) = ReferStackVars,
    (
        ReferStackVars = yes,
        NeedsFrame = block_needs_frame(code_needs_frame(Label))
    ;
        ReferStackVars = no,
        (
            list.member(Instr, Instrs),
            Instr = Uinstr - _,
            (
                Uinstr = llcall(_, _, _, _, _, _)
            ;
                Uinstr = mkframe(_, _)
            ;
                Uinstr = arbitrary_c_code(_, _)
            ;
                Uinstr = pragma_c(_, _, MayCallMercury, _, MaybeLayout,
                    MaybeOnlyLayout, _, NeedStack, _),
                (
                    MayCallMercury = proc_may_call_mercury
                ;
                    MaybeLayout = yes(_)
                ;
                    MaybeOnlyLayout = yes(_)
                ;
                    NeedStack = yes
                )
            ;
                % If we assign to the succip register, then we need the saved
                % copy of the original return address in the stack frame.
                Uinstr = assign(succip, _)
            )
        ->
            NeedsFrame = block_needs_frame(code_needs_frame(Label))
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
    % whenever we find at least one exit block that branches back
    % to the beginning of the procedure; in such cases we return the
    % the label starting the procedure and the label that should replace
    % it in tailcalls that avoid the stack teardown, which is the label
    % immediately after the initial stack setup block.
    %
:- pred analyze_block_map(list(label)::in, pre_exit_dummy_label_map::in,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out,
    maybe(pair(label))::out) is det <= block_entry_exit(En, Ex).

analyze_block_map(LabelSeq, PreExitDummyLabelMap, !BlockMap, KeepFrameData) :-
    (
        LabelSeq = [FirstLabel, SecondLabel | _],
        map.search(!.BlockMap, FirstLabel, FirstBlockInfo),
        FirstBlockInfo = frame_block_info(FirstLabel, _, _, _, _, BlockType),
        BlockType = entry_block(_)
    ->
        ProcLabel = get_proc_label(FirstLabel),
        analyze_block_map_2(LabelSeq, FirstLabel, ProcLabel,
            PreExitDummyLabelMap, !BlockMap, no, AnyBlockNeedsFrame,
            no, JumpToStart),
        % We want to apply the transformation to keep the stack frame only if
        % (a) some block actually needs the stack frame, and (b) there is at
        % least one block that jumps back to the start of the procedure.
        (
            AnyBlockNeedsFrame = yes,
            JumpToStart = yes
        ->
            KeepFrameData = yes(FirstLabel - SecondLabel)
        ;
            KeepFrameData = no
        )
    ;
        unexpected(this_file, "analyze_block_map: bad data")
    ).

:- pred analyze_block_map_2(list(label)::in, label::in, proc_label::in,
    pre_exit_dummy_label_map::in,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out,
    bool::in, bool::out, bool::in, bool::out) is det
    <= block_entry_exit(En, Ex).

analyze_block_map_2([], _, _, _, !BlockMap, !AnyBlockNeedsFrame, !KeepFrame).
analyze_block_map_2([Label | Labels], FirstLabel, ProcLabel,
        PreExitDummyLabelMap, !BlockMap, !AnyBlockNeedsFrame, !JumpToStart) :-
    analyze_block(Label, Labels, FirstLabel, ProcLabel, PreExitDummyLabelMap,
        !BlockMap, !AnyBlockNeedsFrame, !JumpToStart),
    analyze_block_map_2(Labels, FirstLabel, ProcLabel, PreExitDummyLabelMap,
        !BlockMap, !AnyBlockNeedsFrame, !JumpToStart).

:- pred analyze_block(label::in, list(label)::in, label::in, proc_label::in,
    pre_exit_dummy_label_map::in,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out,
    bool::in, bool::out, bool::in, bool::out) is det
    <= block_entry_exit(En, Ex).

analyze_block(Label, FollowingLabels, FirstLabel, ProcLabel,
        PreExitDummyLabelMap, !BlockMap, !AnyBlockNeedsFrame, !JumpToStart) :-
    map.lookup(!.BlockMap, Label, BlockInfo0),
    BlockInfo0 = frame_block_info(BlockLabel, BlockInstrs0, FallInto,
        _, _, Type),
    ( Type = ordinary_block(block_needs_frame(_), _) ->
        !:AnyBlockNeedsFrame = yes
    ;
        true
    ),
    (
        Label = BlockLabel, % sanity check
        list.split_last_det(BlockInstrs0, AllButLastInstrs, LastInstr0)
    ->
        LastInstr0 = LastUinstr0 - Comment,
        ( LastUinstr0 = goto(GotoTarget0) ->
            replace_labels_code_addr(GotoTarget0, PreExitDummyLabelMap,
                GotoTarget),
            LastUinstr = goto(GotoTarget),
            LastInstr = LastUinstr - Comment,
            BlockInstrs = AllButLastInstrs ++ [LastInstr]
        ; LastUinstr0 = if_val(Rval, GotoTarget0) ->
            replace_labels_code_addr(GotoTarget0, PreExitDummyLabelMap,
                GotoTarget),
            LastUinstr = if_val(Rval, GotoTarget),
            LastInstr = LastUinstr - Comment,
            BlockInstrs = AllButLastInstrs ++ [LastInstr]
        ; LastUinstr0 = computed_goto(Rval, GotoTargets0) ->
            replace_labels_label_list(GotoTargets0, PreExitDummyLabelMap,
                GotoTargets),
            LastUinstr = computed_goto(Rval, GotoTargets),
            LastInstr = LastUinstr - Comment,
            BlockInstrs = AllButLastInstrs ++ [LastInstr]
        ; LastUinstr0 = pragma_c(D, Comps0, MC, FNL, FL, FOL, NF0, S, MD) ->
            (
                NF0 = no,
                NF = no,
                Comps0 = Comps
            ;
                NF0 = yes(NFLabel0),
                replace_labels_label(NFLabel0, PreExitDummyLabelMap, NFLabel),
                NF = yes(NFLabel),
                replace_labels_comps(Comps0, PreExitDummyLabelMap, Comps)
            ),
            LastUinstr = pragma_c(D, Comps, MC, FNL, FL, FOL, NF, S, MD),
            LastInstr = LastUinstr - Comment,
            BlockInstrs = AllButLastInstrs ++ [LastInstr]
        ;
            LastUinstr = LastUinstr0,
            BlockInstrs = BlockInstrs0
        ),
        possible_targets(LastUinstr, SideLabels0, _SideCodeAddrs),
        list.filter(local_label(ProcLabel), SideLabels0, SideLabels),
        (
            opt_util.can_instr_fall_through(LastUinstr, yes),
            FollowingLabels = [NextLabel | _]
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
    svmap.det_update(Label, BlockInfo, !BlockMap),
    find_redoip_labels(BlockInstrs, ProcLabel, [], RedoipLabels),
    list.foldl(mark_redoip_label, RedoipLabels, !BlockMap).

:- pred local_label(proc_label::in, label::in) is semidet.

local_label(ProcLabel, entry(_, ProcLabel)).
local_label(ProcLabel, internal(_, ProcLabel)).

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

matching_label_ref(FirstLabel, GotoLabel) :-
    FirstLabel = entry(FirstLabelType, ProcLabel),
    GotoLabel = entry(GotoLabelType, ProcLabel),
    matching_entry_type(FirstLabelType, GotoLabelType).

:- pred matching_entry_type(entry_label_type::in, entry_label_type::in)
    is semidet.

matching_entry_type(entry_label_exported, entry_label_exported).
matching_entry_type(entry_label_exported, entry_label_c_local).
matching_entry_type(entry_label_exported, entry_label_local).
matching_entry_type(entry_label_local, entry_label_c_local).
matching_entry_type(entry_label_local, entry_label_local).
matching_entry_type(entry_label_c_local, entry_label_c_local).

:- pred find_redoip_labels(list(instruction)::in, proc_label::in,
    list(label)::in, list(label)::out) is det.

find_redoip_labels([], _, !RedoipLabels).
find_redoip_labels([Instr | Instrs], ProcLabel, !RedoipLabels) :-
    Instr = Uinstr - _,
    (
        Uinstr = assign(redoip_slot(_),
            const(llconst_code_addr(label(Label)))),
        get_proc_label(Label) = ProcLabel
    ->
        !:RedoipLabels = [Label | !.RedoipLabels]
    ;
        true
    ),
    find_redoip_labels(Instrs, ProcLabel, !RedoipLabels).

:- pred mark_redoip_label(label::in,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out) is det.

mark_redoip_label(Label, !BlockMap) :-
    map.lookup(!.BlockMap, Label, BlockInfo0),
    BlockType0 = BlockInfo0 ^ fb_type,
    (
        BlockType0 = entry_block(_),
        unexpected(this_file, "mark_redoip_label: entry_block")
    ;
        BlockType0 = ordinary_block(_, MaybeDummy),
        Reason = redoip_label,
        BlockType = ordinary_block(block_needs_frame(Reason), MaybeDummy),
        BlockInfo = BlockInfo0 ^ fb_type := BlockType,
        svmap.det_update(Label, BlockInfo, !BlockMap)
    ;
        BlockType0 = exit_block(_),
        unexpected(this_file, "mark_redoip_label: exit_block")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func can_clobber_succip(list(label), frame_block_map(_, _)) = bool.

can_clobber_succip([], _BlockMap) = no.
can_clobber_succip([Label | Labels], BlockMap) = CanClobberSuccip :-
    map.lookup(BlockMap, Label, BlockInfo),
    Instrs = BlockInfo ^ fb_instrs,
    (
        list.member(Instr, Instrs),
        Instr = Uinstr - _,
        (
            Uinstr = llcall(_, _, _, _, _, _)
        ;
            % Only may_call_mercury pragma_c's can clobber succip.
            Uinstr = pragma_c(_, _, proc_may_call_mercury, _, _, _, _, _, _)
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
    det_frame_block_map::in, det_frame_block_map::out) is det.

keep_frame_transform([], _, _, _, !BlockMap).
keep_frame_transform([Label | Labels], FirstLabel, SecondLabel,
        CanClobberSuccip, !BlockMap) :-
    map.lookup(!.BlockMap, Label, BlockInfo0),
    (
        BlockInfo0 = frame_block_info(Label, OrigInstrs, FallInto, [_], no,
            exit_block(det_exit(Succip, Livevals, Goto))),
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
        string.append(Comment, " (keeping frame)", NewComment),
        NewGoto = goto(label(SecondLabel)) - NewComment,
        LivevalsGoto = Livevals ++ [NewGoto],
        (
            CanClobberSuccip = yes,
            BackInstrs = Succip ++ LivevalsGoto
        ;
            CanClobberSuccip = no,
            BackInstrs = LivevalsGoto
        ),
        Instrs = [OrigLabelInstr | BackInstrs],
        Reason = keep_frame,
        BlockType = ordinary_block(block_needs_frame(Reason), is_not_dummy),
        BlockInfo = frame_block_info(Label, Instrs, FallInto, [SecondLabel],
            no, BlockType),
        map.det_update(!.BlockMap, Label, BlockInfo, !:BlockMap)
    ;
        true
    ),
    keep_frame_transform(Labels, FirstLabel, SecondLabel, CanClobberSuccip,
        !BlockMap).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Check that we can use the delay_frame transformation. This requires
    % that only the first block is of the setup type.
    %
:- pred can_delay_frame(list(label)::in, frame_block_map(En, Ex)::in)
    is semidet.

can_delay_frame([], _).
can_delay_frame([Label | _Labels], BlockMap) :-
    map.lookup(BlockMap, Label, BlockInfo),
    BlockInfo ^ fb_type = entry_block(_).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % The data structures used in the delaying optimizations.

    % map.search(RevMap, Label, SideLabels) should be true if the block
    % started by Label can be reached via jump or fallthrough from the labels
    % in SideLabels.
    %
:- type rev_map ==  map(label, list(label)).

    % Given the label L starting a block, map.search(PredMap, L, PrevL)
    % is true if PrevL starts the block immediately before L.
    % If L is the first block in the sequence (which should be a setup block),
    % map.search(PredMap, L, _) fails.
:- type pred_map == map(label, label).

    % map.search(SetupParMap, L, SetupL) should be true if L starts
    % an ordinary block that needs a stack frame that can be jumped to by
    % blocks that do not have a stack frame. In this case, SetupL will be the
    % label starting a new block that sets up the stack frame before handing
    % control to L. We put SetupL's block immediately before L's block.
    % If L cannot be fallen into, this is fine. If it can, we put a goto
    % around SetupL's block after the previous block. In most cases,
    % full jump optimization to duplicate code as necessary to optimize away
    % the goto.
    %
:- type setup_par_map
    --->    setup_par_map(map(label, label)).

    % map.search(ExitParMap, L, ParallelL should be true if L starts
    % an exit block and ParallelL starts a copy of L's block from which
    % the instructions to tear down the stack frame have been deleted.
    % If the block immediately before L does not have a stack frame,
    % we put ParallelL before L. If it does, we put ParallelL after L.
    % since neither L nor ParallelL can fall through, we don't need any gotos
    % to jump around blocks.
    %
:- type exit_par_map
    --->    exit_par_map(map(label, label)).

%-----------------------------------------------------------------------------%

:- type can_transform
    --->    can_transform
    ;       cannot_transform.

    % XXX This documentation needs updating.

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
    %   whether via jump or fallthrough, will be an ordinary block or an
    %   exit block.
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
    %   - If the successor is an exit block B, then we modify the transfer
    %     of control to jump a new label ParallelB, whose block has the same
    %     code as B's block, except for the deletion of the instructions that
    %     tear down the (nonexistent along this path) stack frame. The
    %     correspondence between B and ParallelB is recorded in ExitParMap.
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
    %   not have a stack frame (i.e. for every B in ExitParMap), we create
    %   a new block that is a clone of B with the stack teardown deleted.
    %   Whether we put B or ParallelB first depends on whether the immediately
    %   previous block has a stack frame or not.
    %
:- pred delay_frame_transform(list(label)::in, list(label)::out,
    En::in, proc_label::in, pred_map::in, counter::in, counter::out,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out, globals::in,
    list(instruction)::out, list(instruction)::out, can_transform::out) is det
    <= block_entry_exit(En, Ex).

delay_frame_transform(!LabelSeq, EntryInfo, ProcLabel, PredMap, !C,
        !BlockMap, Globals, TransformComments, DescComments, CanTransform) :-
    some [!OrdNeedsFrame, !CanTransform, !PropagationStepsLeft] (
        !:OrdNeedsFrame = map.init,
        !:CanTransform = can_transform,
        !:PropagationStepsLeft = max_propagation_steps,
        delay_frame_init(!.LabelSeq, !.BlockMap, map.init, RevMap,
            queue.init, SuccQueue, !OrdNeedsFrame),
        propagate_frame_requirement_to_successors(SuccQueue, !.BlockMap,
            !OrdNeedsFrame, set.init, !PropagationStepsLeft, !CanTransform),
        map.to_assoc_list(!.OrdNeedsFrame, OrdNeedsFrameList),
        list.filter_map(key_block_needs_frame, OrdNeedsFrameList, Frontier),
        queue.list_to_queue(Frontier, PredQueue),
        propagate_frame_requirement_to_predecessors(PredQueue, !.BlockMap,
            RevMap, !OrdNeedsFrame, !.PropagationStepsLeft, _, !CanTransform),
        (
            !.CanTransform = cannot_transform
            % The delay frame optimization is not applicable; our caller will
            % ignore all the other output arguments.
        ;
            !.CanTransform = can_transform,
            process_frame_delay(!.LabelSeq, !.OrdNeedsFrame, ProcLabel, !C,
                !BlockMap, setup_par_map(map.init), SetupParMap,
                exit_par_map(map.init), ExitParMap),
            create_parallels(!LabelSeq, EntryInfo, ProcLabel, !C,
                !.OrdNeedsFrame, SetupParMap, ExitParMap, PredMap, !BlockMap)
        ),
        globals.lookup_bool_option(Globals, frameopt_comments,
            FrameoptComments),
        (
            FrameoptComments = no,
            TransformComments = [],
            DescComments = []
        ;
            FrameoptComments = yes,
            TransformComments = [comment("delaying stack frame") - ""],
            list.map(describe_block(!.BlockMap, !.OrdNeedsFrame,
                PredMap, ProcLabel), !.LabelSeq, DescComments)
        ),
        CanTransform = !.CanTransform
    ).

    % We want to stop the transformation if we need more than this many
    % propagation steps. For such large predicates (e.g. write_ordinary_term)
    % any performance benefit from frameopt is unlikely to be noticeable.
    %
:- func max_propagation_steps = int.

max_propagation_steps = 10000.

:- pred key_block_needs_frame(
    pair(label, block_needs_frame(needs_frame_reasons))::in,
    pair(needs_frame_reason, label)::out) is semidet.

key_block_needs_frame(Label - block_needs_frame(Reasons),
    frontier(Label, Reasons) - Label).

%-----------------------------------------------------------------------------%

    % Maps the label of each ordinary block to a bool that says whether
    % the block needs a stack frame or not.
    %
:- type ord_needs_frame == map(label, block_needs_frame(needs_frame_reasons)).

:- type prop_queue == queue(pair(needs_frame_reason, label)).

    % Initialize the data structures for the delaying operation.
    % The first is a map showing the predecessors of each block,
    % i.e. the set of blocks that can jump to or fall through each other block.
    % The second is a queue of ordinary blocks that need a stack frame.
    % The third says, for each ordinary block, whether it needs a stack frame.
    %
    % This predicate implements the first part of the first phase of
    % delay_frame_transform.
    %
:- pred delay_frame_init(list(label)::in, frame_block_map(En, Ex)::in,
    rev_map::in, rev_map::out, prop_queue::in, prop_queue::out,
    ord_needs_frame::in, ord_needs_frame::out) is det.

delay_frame_init([], _, !RevMap, !Queue, !OrdNeedsFrame).
delay_frame_init([Label | Labels], BlockMap, !RevMap, !Queue,
        !OrdNeedsFrame) :-
    map.lookup(BlockMap, Label, BlockInfo),
    BlockType = BlockInfo ^ fb_type,
    (
        BlockType = entry_block(_)
    ;
        BlockType = ordinary_block(NeedsFrame, _),
        (
            NeedsFrame = block_doesnt_need_frame,
            svmap.det_insert(Label, block_doesnt_need_frame, !OrdNeedsFrame)
        ;
            NeedsFrame = block_needs_frame(Reason),
            Reasons = make_singleton_set(Reason),
            svmap.det_insert(Label, block_needs_frame(Reasons),
                !OrdNeedsFrame),
            svqueue.put(Reason - Label, !Queue)
        )
    ;
        BlockType = exit_block(_)
    ),
    rev_map_side_labels(successors(BlockInfo), Label, !RevMap),
    delay_frame_init(Labels, BlockMap, !RevMap, !Queue, !OrdNeedsFrame).

:- pred rev_map_side_labels(list(label)::in, label::in,
    rev_map::in, rev_map::out) is det.

rev_map_side_labels([], _Label, !RevMap).
rev_map_side_labels([Label | Labels], SourceLabel, !RevMap) :-
    ( map.search(!.RevMap, Label, OtherSources0) ->
        OtherSources = [SourceLabel | OtherSources0],
        svmap.det_update(Label, OtherSources, !RevMap)
    ;
        OtherSources = [SourceLabel],
        svmap.det_insert(Label, OtherSources, !RevMap)
    ),
    rev_map_side_labels(Labels, SourceLabel, !RevMap).

%-----------------------------------------------------------------------------%

:- pred ord_needs_frame(label::in, needs_frame_reason::in,
    ord_needs_frame::in, ord_needs_frame::out) is det.

ord_needs_frame(Label, CurReason, !OrdNeedsFrame) :-
    map.lookup(!.OrdNeedsFrame, Label, NeedsFrame0),
    (
        NeedsFrame0 = block_doesnt_need_frame,
        Reasons = make_singleton_set(CurReason),
        svmap.det_update(Label, block_needs_frame(Reasons), !OrdNeedsFrame)
    ;
        NeedsFrame0 = block_needs_frame(Reasons0),
        set.insert(Reasons0, CurReason, Reasons),
        svmap.det_update(Label, block_needs_frame(Reasons), !OrdNeedsFrame)
    ).

    % Given a queue of labels representing ordinary blocks that must have
    % a stack frame, propagate the requirement for a stack frame to all
    % other ordinary blocks that are their successors.
    %
    % This predicate implements the second part of the first phase of
    % delay_frame_transform.
    %
:- pred propagate_frame_requirement_to_successors(prop_queue::in,
    frame_block_map(En, Ex)::in, ord_needs_frame::in, ord_needs_frame::out,
    set(label)::in, int::in, int::out, can_transform::in, can_transform::out)
    is det.

propagate_frame_requirement_to_successors(!.Queue, BlockMap, !OrdNeedsFrame,
        !.AlreadyProcessed, !PropagationStepsLeft, !CanTransform) :-
    ( !.CanTransform = cannot_transform ->
        true
    ; !.PropagationStepsLeft < 0 ->
        !:CanTransform = cannot_transform
    ; svqueue.get(Reason - Label, !Queue) ->
        !:PropagationStepsLeft = !.PropagationStepsLeft - 1,
        svset.insert(Label, !AlreadyProcessed),
        map.lookup(BlockMap, Label, BlockInfo),
        BlockType = BlockInfo ^ fb_type,
        (
            BlockType = ordinary_block(_, _MaybeDummy),
            ord_needs_frame(Label, Reason, !OrdNeedsFrame),
            % Putting an already processed label into the queue could
            % lead to an infinite loop. However, we cannot decide whether
            % a label has been processed by checking whether
            % !.OrdNeedsFrame maps Label to yes, since !.OrdNeedsFrame
            % doesn't mention setup frames, and we want to set
            % !:CanTransform to no if any successor is a setup frame.
            % We cannot assume that successors not in !.OrdNeedsFrame
            % should set !:CanTransform to no either, since we don't want
            % to do that for exit frames.
            list.filter(set.contains(!.AlreadyProcessed),
                successors(BlockInfo), _, UnprocessedSuccessors),
            list.map(pair_with(succ_propagated(Label, Reason)),
                UnprocessedSuccessors, PairedUnprocessedSuccessors),
            svqueue.put_list(PairedUnprocessedSuccessors, !Queue)
        ;
            BlockType = entry_block(_),
            !:CanTransform = cannot_transform
        ;
            BlockType = exit_block(_)
            % Exit blocks never *need* stack frames.
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
:- pred propagate_frame_requirement_to_predecessors(prop_queue::in,
    frame_block_map(En, Ex)::in, rev_map::in,
    ord_needs_frame::in, ord_needs_frame::out, int::in, int::out,
    can_transform::in, can_transform::out) is det.

propagate_frame_requirement_to_predecessors(!.Queue, BlockMap, RevMap,
        !OrdNeedsFrame, !PropagationStepsLeft, !CanTransform) :-
    ( !.CanTransform = cannot_transform ->
        true
    ; !.PropagationStepsLeft < 0 ->
        !:CanTransform = cannot_transform
    ; svqueue.get(Reason - Label, !Queue) ->
        !:PropagationStepsLeft = !.PropagationStepsLeft - 1,
        ( map.search(RevMap, Label, PredecessorsPrime) ->
            Predecessors = PredecessorsPrime
        ;
            % We get here if Label cannot be reached by a fallthrough or an
            % explicit jump, but only by backtracking. In that case, the code
            % that sets up the resumption point saves the address of Label on
            % the stack, and thus is already known to need a stack frame.
            Predecessors = [],
            ord_needs_frame(Label, Reason, !OrdNeedsFrame)
        ),
        list.filter(all_successors_need_frame(BlockMap, !.OrdNeedsFrame),
            Predecessors, NowNeedFrameLabels),
        list.foldl2(record_frame_need(BlockMap, Reason), NowNeedFrameLabels,
            !OrdNeedsFrame, !CanTransform),
        % XXX map.lookup(BlockMap, Label, BlockInfo),
        % XXX Successors = successors(BlockInfo),
        list.map(pair_with(pred_propagated(Label, Reason)), NowNeedFrameLabels,
            PairedNowNeedFrameLabels),
        svqueue.put_list(PairedNowNeedFrameLabels, !Queue),
        propagate_frame_requirement_to_predecessors(!.Queue, BlockMap,
            RevMap, !OrdNeedsFrame, !PropagationStepsLeft, !CanTransform)
    ;
        true
    ).

:- pred record_frame_need(frame_block_map(En, Ex)::in, needs_frame_reason::in,
    label::in, ord_needs_frame::in, ord_needs_frame::out,
    can_transform::in, can_transform::out) is det.

record_frame_need(BlockMap, Reason, Label, !OrdNeedsFrame, !CanTransform) :-
    map.lookup(BlockMap, Label, BlockInfo),
    BlockType = BlockInfo ^ fb_type,
    (
        BlockType = entry_block(_),
        !:CanTransform = cannot_transform
    ;
        BlockType = ordinary_block(_, _),
        ord_needs_frame(Label, Reason, !OrdNeedsFrame)
    ;
        BlockType = exit_block(_),
        unexpected(this_file, "record_frame_need: exit_block")
    ).

:- pred all_successors_need_frame(frame_block_map(En, Ex)::in,
    ord_needs_frame::in, label::in) is semidet.

all_successors_need_frame(BlockMap, OrdNeedsFrame, Label) :-
    map.lookup(BlockMap, Label, BlockInfo),
    Successors = successors(BlockInfo),
    list.filter(label_needs_frame(OrdNeedsFrame), Successors,
        _NeedFrameSuccessors, NoNeedFrameSuccessors),
    NoNeedFrameSuccessors = [].

:- pred label_needs_frame(ord_needs_frame::in, label::in) is semidet.

label_needs_frame(OrdNeedsFrame, Label) :-
    ( map.search(OrdNeedsFrame, Label, NeedsFrame) ->
        NeedsFrame = block_needs_frame(_)
    ;
        % If the map.search fails, Label is not an ordinary frame.
        % Entry blocks and exit blocks don't need frames.
        fail
    ).

    % Returns the set of successors of the given block as a list
    % (which may contain duplicates).
    %
:- func successors(frame_block_info(En, Ex)) = list(label).

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
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out,
    setup_par_map::in, setup_par_map::out, exit_par_map::in, exit_par_map::out)
    is det <= block_entry_exit(En, Ex).

process_frame_delay([], _, _, !C, !BlockMap, !SetupParMap, !ExitParMap).
process_frame_delay([Label0 | Labels0], OrdNeedsFrame, ProcLabel, !C,
        !BlockMap, !SetupParMap, !ExitParMap) :-
    map.lookup(!.BlockMap, Label0, BlockInfo0),
    BlockInfo0 = frame_block_info(Label0Copy, Instrs0, FallInto, SideLabels0,
        MaybeFallThrough0, Type),
    expect(unify(Label0, Label0Copy), this_file,
        "process_frame_delay: label in frame_block_info is not copy"),
    (
        Type = entry_block(_),
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
            SideLabels0, MaybeFallThrough0,
            ordinary_block(block_doesnt_need_frame, is_not_dummy)),
        svmap.det_update(Label0, BlockInfo, !BlockMap),
        process_frame_delay(Labels0, OrdNeedsFrame,
            ProcLabel, !C, !BlockMap, !SetupParMap, !ExitParMap)
    ;
        Type = ordinary_block(_, _),
        map.lookup(OrdNeedsFrame, Label0, NeedsFrame),
        (
            NeedsFrame = block_needs_frame(_),
            % Every block reachable from this block, whether via jump or
            % fallthrough, will be an ordinary block also mapped to `yes'
            % by OrdNeedsFrame, or will be an exit block, or will be a pre-exit
            % dummy block. We already have a stack frame, and all our
            % successors expect one, so we need not do anything.
            process_frame_delay(Labels0, OrdNeedsFrame, ProcLabel, !C,
                !BlockMap, !SetupParMap, !ExitParMap)
        ;
            NeedsFrame = block_doesnt_need_frame,
            transform_nostack_ordinary_block(Label0, Labels0, BlockInfo0,
                OrdNeedsFrame, ProcLabel, !C, !BlockMap,
                !SetupParMap, !ExitParMap)
        )
    ;
        Type = exit_block(_),
        process_frame_delay(Labels0, OrdNeedsFrame, ProcLabel, !C,
            !BlockMap, !SetupParMap, !ExitParMap)
    ).

    % Transform an ordinary block that doesn't have a stack frame.
    % Every block that is a successor of this block, whether via jump or
    % fallthrough, will be an ordinary block or an exit block.
    %
    % - If it is an ordinary block that doesn't need a frame, we need not
    %   do anything.
    %
    % - If it is an ordinary block B that does need a stack frame, we need to
    %   insert the frame setup code at the transfer between the two blocks.
    %   The label S of the block that contains the setup code and then goes
    %   to block B will be given by map.lookup(!.SetupParMap, B, S).
    %   Here, we just allocate the label S; the block will be created later.
    %
    % - If it is exit block B, then we need to jump to a variant of B
    %   that does no teardown, since there is no stack frame to tear down.
    %   The label S of the variant block will be given by
    %   map.lookup(!.ExitParMap, B, S). Here, we just allocate
    %   the label S; the block will be created later.
    %
:- pred transform_nostack_ordinary_block(label::in, list(label)::in,
    frame_block_info(En, Ex)::in, ord_needs_frame::in,
    proc_label::in, counter::in, counter::out,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out,
    setup_par_map::in, setup_par_map::out, exit_par_map::in, exit_par_map::out)
    is det <= block_entry_exit(En, Ex).

transform_nostack_ordinary_block(Label0, Labels0, BlockInfo0, OrdNeedsFrame,
        ProcLabel, !C, !BlockMap, !SetupParMap, !ExitParMap) :-
    BlockInfo0 = frame_block_info(_, Instrs0, FallInto,
        SideLabels0, MaybeFallThrough0, Type),
    mark_parallels_for_nostack_successors(SideLabels0, SideLabels,
        SideAssocLabelMap, OrdNeedsFrame, !.BlockMap, ProcLabel, !C,
        !SetupParMap, !ExitParMap),
    (
        MaybeFallThrough0 = yes(FallThroughLabel0),
        mark_parallel_for_nostack_successor(FallThroughLabel0,
            FallThroughLabel, OrdNeedsFrame, !.BlockMap, ProcLabel, !C,
            !SetupParMap, !ExitParMap),
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
    list.split_last_det(Instrs0, PrevInstrs, LastInstr0),
    map.from_assoc_list(AssocLabelMap, LabelMap),
    opt_util.replace_labels_instruction(LastInstr0, LabelMap, no, LastInstr),
    Instrs = PrevInstrs ++ [LastInstr | RedirectFallThrough],
    BlockInfo = frame_block_info(Label0, Instrs, FallInto,
        SideLabels, MaybeFallThrough, Type),
    map.set(!.BlockMap, Label0, BlockInfo, !:BlockMap),
    process_frame_delay(Labels0, OrdNeedsFrame, ProcLabel, !C, !BlockMap,
        !SetupParMap, !ExitParMap).

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
    frame_block_map(En, Ex)::in, proc_label::in, counter::in, counter::out,
    setup_par_map::in, setup_par_map::out,
    exit_par_map::in, exit_par_map::out) is det.

mark_parallels_for_nostack_successors([], [], [], _, _, _, !C,
        !SetupParMap, !ExitParMap).
mark_parallels_for_nostack_successors([Label0 | Labels0], [Label | Labels],
        [Label0 - Label | LabelMap], OrdNeedsFrame, BlockMap, ProcLabel, !C,
        !SetupParMap, !ExitParMap) :-
    mark_parallel_for_nostack_successor(Label0, Label,
        OrdNeedsFrame, BlockMap, ProcLabel, !C, !SetupParMap, !ExitParMap),
    mark_parallels_for_nostack_successors(Labels0, Labels, LabelMap,
        OrdNeedsFrame, BlockMap, ProcLabel, !C, !SetupParMap, !ExitParMap).

    % Label0 is a label that is a successor of a block which has no stack
    % frame.
    %
    % If Label0 starts an exit block, we ensure that it has a non-teardown
    % parallel Label.
    %
    % If Label0 starts an ordinary block that needs a stack frame, we ensure
    % that it has a parallel Label that allocates a stack frame before handing
    % control to Label0.
    %
:- pred mark_parallel_for_nostack_successor(label::in, label::out,
    ord_needs_frame::in, frame_block_map(En, Ex)::in, proc_label::in,
    counter::in, counter::out, setup_par_map::in, setup_par_map::out,
    exit_par_map::in, exit_par_map::out) is det.

mark_parallel_for_nostack_successor(Label0, Label, OrdNeedsFrame, BlockMap,
        ProcLabel, !C, !SetupParMap, !ExitParMap) :-
    map.lookup(BlockMap, Label0, BlockInfo),
    Type = BlockInfo ^ fb_type,
    (
        Type = entry_block(_),
        unexpected(this_file, "mark_parallels_for_nostack_jump: " ++
            "reached setup via jump from ordinary block")
    ;
        Type = ordinary_block(_, _),
        map.lookup(OrdNeedsFrame, Label0, NeedsFrame),
        (
            NeedsFrame = block_needs_frame(_),
            ensure_setup_parallel(Label0, Label, ProcLabel, !C, !SetupParMap)
        ;
            NeedsFrame = block_doesnt_need_frame,
            Label = Label0
        )
    ;
        Type = exit_block(_),
        ensure_exit_parallel(Label0, Label, ProcLabel, !C, !ExitParMap)
    ).

%-----------------------------------------------------------------------------%

    % The third phase of the delay_frame_transform optimization, creating
    %
    % - the setup code of ordinary blocks that need frames but (some of)
    %   whose predecessors don't have one, and
    %
    % - the parallels of exit blocks that can assume there is no frame to
    %   tear down.
    %
:- pred create_parallels(list(label)::in, list(label)::out, En::in,
    proc_label::in, counter::in, counter::out, ord_needs_frame::in,
    setup_par_map::in, exit_par_map::in, pred_map::in,
    frame_block_map(En, Ex)::in, frame_block_map(En, Ex)::out) is det
    <= block_entry_exit(En, Ex).

create_parallels([], [], _, _, !C, _, _, _, _, !BlockMap).
create_parallels([Label0 | Labels0], Labels, EntryInfo, ProcLabel, !C,
        OrdNeedsFrame, SetupParMap, ExitParMap, PredMap, !BlockMap) :-
    create_parallels(Labels0, Labels1, EntryInfo, ProcLabel, !C,
        OrdNeedsFrame, SetupParMap, ExitParMap, PredMap, !BlockMap),
    map.lookup(!.BlockMap, Label0, BlockInfo0),
    BlockInfo0 = frame_block_info(Label0Copy, _, FallInto,
        SideLabels, MaybeFallThrough, Type),
    expect(unify(Label0, Label0Copy), this_file,
        "create_parallels: label in frame_block_info is not copy"),
    ( search_exit_par_map(ExitParMap, Label0, ParallelLabel) ->
        expect(unify(MaybeFallThrough, no), this_file,
            "create_parallels: exit block with parallel has fall through"),
        (
            SideLabels = [],
            Comments = []
        ;
            SideLabels = [_ | _],
            % This can happen if fulljump optimization has redirected the
            % return.
            Comments = [comment("exit side labels "
                ++ dump_labels(ProcLabel, SideLabels)) - ""]
        ),
        PrevNeedsFrame = prev_block_needs_frame(OrdNeedsFrame, BlockInfo0),
        ( Type = exit_block(ExitInfo) ->
            LabelInstr = label(ParallelLabel) - "non-teardown parallel",
            ReplacementCode = [LabelInstr] ++ Comments ++
                non_teardown_exit_code(ExitInfo),
            (
                PrevNeedsFrame = block_doesnt_need_frame,
                Labels = [ParallelLabel, Label0 | Labels1],
                BlockInfo = BlockInfo0 ^ fb_fallen_into := no,
                svmap.det_update(Label0, BlockInfo, !BlockMap),
                ParallelBlockFallInto = FallInto
            ;
                PrevNeedsFrame = block_needs_frame(_),
                Labels = [Label0, ParallelLabel | Labels1],
                ParallelBlockFallInto = no
            ),
            ParallelBlockInfo = frame_block_info(ParallelLabel,
                ReplacementCode, ParallelBlockFallInto, SideLabels,
                no, ordinary_block(block_doesnt_need_frame, is_not_dummy)),
            svmap.det_insert(ParallelLabel, ParallelBlockInfo, !BlockMap)
        ;
            unexpected(this_file, "block in exit_par_map is not exit")
        )
    ; search_setup_par_map(SetupParMap, Label0, SetupLabel) ->
        expect(is_ordinary_block(Type), this_file,
            "create_parallels: block in setup map is not ordinary"),
        PrevNeedsFrame = prev_block_needs_frame(OrdNeedsFrame, BlockInfo0),
        (
            PrevNeedsFrame = block_needs_frame(Reasons),
            counter.allocate(N, !C),
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
            JumpAroundReason = jump_around(Label0, Reasons),
            JumpAroundBlockInfo = frame_block_info(JumpAroundLabel,
                JumpAroundCode, no, [Label0], FallInto,
                ordinary_block(block_needs_frame(JumpAroundReason),
                    is_not_dummy)),
            svmap.det_insert(JumpAroundLabel, JumpAroundBlockInfo, !BlockMap),
            SetupFallInto = yes(JumpAroundLabel),
            BlockInfo = BlockInfo0 ^ fb_fallen_into := yes(SetupLabel),
            svmap.det_update(Label0, BlockInfo, !BlockMap)
        ;
            PrevNeedsFrame = block_doesnt_need_frame,
            Labels = [SetupLabel, Label0 | Labels1],
            SetupFallInto = no
        ),
        SetupCode = [label(SetupLabel) - "late setup label"]
            ++ late_setup_code(EntryInfo),
        SetupBlockInfo = frame_block_info(SetupLabel, SetupCode,
            SetupFallInto, [], yes(Label0), entry_block(EntryInfo)),
        svmap.det_insert(SetupLabel, SetupBlockInfo, !BlockMap)
    ;
        Labels = [Label0 | Labels1]
    ).

:- func prev_block_needs_frame(ord_needs_frame, frame_block_info(En, Ex)) =
    block_needs_frame(needs_frame_reasons).

prev_block_needs_frame(OrdNeedsFrame, BlockInfo) = PrevNeedsFrame :-
    MaybeFallIntoFrom = BlockInfo ^ fb_fallen_into,
    (
        MaybeFallIntoFrom = yes(FallIntoFrom),
        ( map.search(OrdNeedsFrame, FallIntoFrom, NeedsFrame) ->
            % FallIntoFrom is an ordinary block that can fall through
            % to this block.
            PrevNeedsFrame = NeedsFrame
        ;
            % FallIntoFrom is a setup block; exit blocks cannot fall
            % through. Entry blocks don't need frames.
            PrevNeedsFrame = block_doesnt_need_frame
        )
    ;
        MaybeFallIntoFrom = no,
        % The previous block doesn't care whether the following block
        % has a frame or not.
        PrevNeedsFrame = block_doesnt_need_frame
    ).

:- pred is_ordinary_block(block_type(En, Ex)::in) is semidet.

is_ordinary_block(ordinary_block(_, _)).

%-----------------------------------------------------------------------------%

:- func det_late_setup(det_entry_info) = list(instruction).

det_late_setup(det_entry(Msg, FrameSize)) =
    [incr_sp(FrameSize, Msg) - "late setup",
    assign(stackvar(FrameSize), lval(succip)) - "late save"].

:- func det_non_teardown_exit_code(det_exit_info) = list(instruction).

det_non_teardown_exit_code(det_exit(_, Livevals, Goto)) = Livevals ++ [Goto].

:- func nondet_late_setup(nondet_entry_info) = list(instruction).

nondet_late_setup(nondet_entry(Msg, FrameSize, Redoip)) =
    [mkframe(ordinary_frame(Msg, FrameSize, no), yes(Redoip)) - "late setup"].

:- func nondet_non_teardown_exit_code(nondet_exit_info) = list(instruction).

nondet_non_teardown_exit_code(nondet_plain_exit(Livevals, _Goto)) =
    Livevals ++ [goto(succip) - ""].
nondet_non_teardown_exit_code(nondet_teardown_exit(_, _, _, Livevals, Goto)) =
    Livevals ++ [Goto].

%-----------------------------------------------------------------------------%

    % Given the label of a block, allocate a label for its parallel
    % in the given setup map if it doesn't already have one.
    %
:- pred ensure_setup_parallel(label::in, label::out, proc_label::in,
    counter::in, counter::out, setup_par_map::in, setup_par_map::out) is det.

ensure_setup_parallel(Label, ParallelLabel, ProcLabel, !C, !SetupParMap) :-
    !.SetupParMap = setup_par_map(ParMap0),
    ( map.search(ParMap0, Label, OldParallel) ->
        ParallelLabel = OldParallel
    ;
        counter.allocate(N, !C),
        NewParallel = internal(N, ProcLabel),
        ParallelLabel = NewParallel,
        map.det_insert(ParMap0, Label, NewParallel, ParMap),
        !:SetupParMap = setup_par_map(ParMap)
    ).

    % Given the label of a block, allocate a label for its parallel
    % in the given exit map if it doesn't already have one.
    %
:- pred ensure_exit_parallel(label::in, label::out, proc_label::in,
    counter::in, counter::out, exit_par_map::in, exit_par_map::out) is det.

ensure_exit_parallel(Label, ParallelLabel, ProcLabel, !C, !ExitParMap) :-
    !.ExitParMap = exit_par_map(ParMap0),
    ( map.search(ParMap0, Label, OldParallel) ->
        ParallelLabel = OldParallel
    ;
        counter.allocate(N, !C),
        NewParallel = internal(N, ProcLabel),
        ParallelLabel = NewParallel,
        map.det_insert(ParMap0, Label, NewParallel, ParMap),
        !:ExitParMap = exit_par_map(ParMap)
    ).

%-----------------------------------------------------------------------------%

    % This predicate generates a human-readable description of a block
    % as a comment instruction. This can make it much easier to debug
    % frameopt.
    %
:- pred describe_block(frame_block_map(En, Ex)::in, ord_needs_frame::in,
    pred_map::in, proc_label::in, label::in, instruction::out) is det
    <= block_entry_exit(En, Ex).

describe_block(BlockMap, OrdNeedsFrame, PredMap, ProcLabel, Label, Instr) :-
    map.lookup(BlockMap, Label, BlockInfo),
    BlockInfo = frame_block_info(BlockLabel, BlockInstrs, FallInto,
        SideLabels, MaybeFallThrough, Type),
    expect(unify(Label, BlockLabel), this_file,
        "describe_block: label mismatch"),
    LabelStr = dump_label(ProcLabel, Label),
    BlockInstrsStr = dump_fullinstrs(ProcLabel, yes, BlockInstrs),
    Heading = "\nBLOCK " ++ LabelStr ++ "\n\n",
    ( map.search(PredMap, Label, PredLabel) ->
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
        Type = entry_block(Entry),
        TypeStr = "entry_block\n" ++ describe_entry(Entry)
    ;
        Type = ordinary_block(UsesFrame, MaybeDummy),
        (
            MaybeDummy = is_not_dummy,
            TypeStr0 = "ordinary_block; "
        ;
            MaybeDummy = is_post_entry_dummy,
            TypeStr0 = "ordinary_block (post_entry_dummy); "
        ;
            MaybeDummy = is_pre_exit_dummy,
            TypeStr0 = "ordinary_block (pre_exit_dummy); "
        ),
        (
            UsesFrame = block_needs_frame(UsesReason),
            TypeStr1 = TypeStr0 ++ "uses frame",
            ( UsesReason = code_needs_frame(Label) ->
                TypeStr2 = TypeStr1 ++ "\n"
            ;
                TypeStr2 = TypeStr1 ++ " " ++
                    describe_reason(ProcLabel, UsesReason) ++ "\n"
            )
        ;
            UsesFrame = block_doesnt_need_frame,
            TypeStr2 = TypeStr0 ++ "does not use frame\n"
        ),
        ( map.search(OrdNeedsFrame, Label, NeedsFrame) ->
            (
                NeedsFrame = block_doesnt_need_frame,
                expect(unify(UsesFrame, block_doesnt_need_frame), this_file,
                    "describe_block: "
                    ++ "NeedsFrame=block_doesnt_need_frame, "
                    ++ "UsesFrame=block_needs_frame"),
                TypeStr = TypeStr2 ++ "does not need frame\n"
            ;
                NeedsFrame = block_needs_frame(NeedsReasonSet),
                set.to_sorted_list(NeedsReasonSet, NeedsReasons),
                ReasonsStr = describe_top_reasons(ProcLabel, NeedsReasons),
                TypeStr = TypeStr2 ++ "does need frame\n" ++ ReasonsStr
            )
        ;
            % We can get here if delay_frame_transform fails.
            TypeStr = TypeStr2 ++ "(unknown whether it does need frame)\n"
        )
    ;
        Type = exit_block(Exit),
        expect(unify(MaybeFallThrough, no), this_file,
            "describe_block: exit_block, MaybeFallThrough=yes(_)"),
        TypeStr = "exit_block\n" ++ describe_exit(ProcLabel, Exit)
    ),
    Comment = Heading ++ PredStr ++ FallIntoStr ++ SideStr ++ FallThroughStr
        ++ TypeStr ++ "CODE:\n" ++ BlockInstrsStr,
    Instr = comment(Comment) - "".

:- func describe_det_entry(det_entry_info) = string.

describe_det_entry(det_entry(Msg, Size)) =
    "msg: " ++ Msg ++ ", size: " ++ int_to_string(Size) ++ "\n".

:- func describe_det_exit(proc_label, det_exit_info) = string.

describe_det_exit(ProcLabel, det_exit(RestoreSuccip, Livevals, Goto)) =
    "restore:  "
    ++ dump_fullinstrs(ProcLabel, yes, RestoreSuccip)
    ++ "livevals: "
    ++ dump_fullinstrs(ProcLabel, yes, Livevals)
    ++ "goto:     "
    ++ dump_fullinstr(ProcLabel, yes, Goto).

:- func describe_nondet_entry(nondet_entry_info) = string.

describe_nondet_entry(nondet_entry(Msg, Size, Redoip)) =
    "msg: "
    ++ Msg
    ++ ", size: "
    ++ int_to_string(Size)
    ++ ", redoip: "
    ++ dump_code_addr(Redoip)
    ++ "\n".

:- func describe_nondet_exit(proc_label, nondet_exit_info) = string.

describe_nondet_exit(ProcLabel, nondet_plain_exit(Livevals, Goto)) =
    "livevals: "
    ++ dump_fullinstrs(ProcLabel, yes, Livevals)
    ++ "goto:     "
    ++ dump_fullinstr(ProcLabel, yes, Goto).
describe_nondet_exit(ProcLabel, nondet_teardown_exit(Succip, Maxfr, Curfr,
        Livevals, Goto)) =
    "succip: "
    ++ dump_fullinstr(ProcLabel, yes, Succip)
    ++ "maxfr: "
    ++ dump_fullinstr(ProcLabel, yes, Maxfr)
    ++ "curfr: "
    ++ dump_fullinstr(ProcLabel, yes, Curfr)
    ++ "livevals: "
    ++ dump_fullinstrs(ProcLabel, yes, Livevals)
    ++ "goto:     "
    ++ dump_fullinstr(ProcLabel, yes, Goto).

:- func describe_top_reasons(proc_label, list(needs_frame_reason)) = string.

describe_top_reasons(_ProcLabel, []) = "".
describe_top_reasons(ProcLabel, [Reason | Reasons]) =
    describe_reason(ProcLabel, Reason) ++ "\n" ++
    describe_top_reasons(ProcLabel, Reasons).

:- func describe_reason(proc_label, needs_frame_reason) = string.

describe_reason(ProcLabel, code_needs_frame(Label)) =
    "code " ++ dump_label(ProcLabel, Label).
describe_reason(_ProcLabel, keep_frame) = "keep_frame".
describe_reason(_ProcLabel, redoip_label) = "redoip_label".
describe_reason(ProcLabel, frontier(Label, Reasons)) =
    "frontier(" ++ dump_label(ProcLabel, Label) ++ ", {"
        ++ describe_reasons(ProcLabel, to_sorted_list(Reasons)) ++ "})".
describe_reason(ProcLabel, jump_around(Label, Reasons)) =
    "jump_around(" ++ dump_label(ProcLabel, Label) ++ ", {"
        ++ describe_reasons(ProcLabel, to_sorted_list(Reasons)) ++ "})".
describe_reason(ProcLabel, succ_propagated(Label, Reason)) =
    "successor(" ++ dump_label(ProcLabel, Label) ++ ", "
        ++ describe_reason(ProcLabel, Reason) ++ ")".
describe_reason(ProcLabel, pred_propagated(Label, Reason)) =
    "predecessor(" ++ dump_label(ProcLabel, Label) ++ ", "
        ++ describe_reason(ProcLabel, Reason) ++ ")".

:- func describe_reasons(proc_label, list(needs_frame_reason)) = string.

describe_reasons(_, []) = "".
describe_reasons(ProcLabel, [Reason]) = describe_reason(ProcLabel, Reason).
describe_reasons(ProcLabel, [Reason1, Reason2 | Reasons]) =
    describe_reason(ProcLabel, Reason1) ++ ", " ++
    describe_reasons(ProcLabel, [Reason2 | Reasons]).

%-----------------------------------------------------------------------------%

:- pred search_setup_par_map(setup_par_map::in, label::in, label::out)
    is semidet.

search_setup_par_map(setup_par_map(ParMap), Label, ParallelLabel) :-
    map.search(ParMap, Label, ParallelLabel).

:- pred search_exit_par_map(exit_par_map::in, label::in, label::out)
    is semidet.

search_exit_par_map(exit_par_map(ParMap), Label, ParallelLabel) :-
    map.search(ParMap, Label, ParallelLabel).

%-----------------------------------------------------------------------------%

:- pred is_yes(maybe(T)::in) is semidet.

is_yes(yes(_)).

:- pred pair_with(T::in, U::in, pair(T, U)::out) is det.

pair_with(T, U, T - U).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "frameopt.m".

%-----------------------------------------------------------------------------%
:- end_module frameopt.
%-----------------------------------------------------------------------------%
