%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% jumpopt.m - optimize jumps to jumps.
%
% Author: zs.

%-----------------------------------------------------------------------------%

:- module ll_backend__jumpopt.

:- interface.

:- import_module ll_backend__llds.
:- import_module mdbcomp__prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module set.

    % jumpopt_main(LayoutLabels, MayAlterRtti, ProcLabel, Fulljumpopt,
    %   Recjump, PessimizeTailCalls, CheckedNondetTailCall, !LabelCounter,
    %   !Instrs, Mod):
    %
    % Take an instruction list and optimize jumps. This includes the jumps
    % implicit in procedure returns.
    %
    % LayoutLabels gives the set of labels that have layout structures.
    % This module will not optimize jumps to labels in this set, since
    % this may interfere with the RTTI recorded for these labels.
    % MayAlterRtti says whether we are allowed to perform optimizations
    % that may interfere with RTTI.
    %
    % Fulljumpopt should be the value of the --optimize-fulljumps option.
    %
    % Recjump should be an indication of whether this is the final
    % application of this optimization.
    %
    % PessimizeTailCalls should be the value of the --pessimize-tailcalls
    % option.
    %
    % CheckedNondetTailCall should be the value of the
    % --checked-nondet-tailcalls option.
    %
    % Mod will say whether the instruction sequence was modified
    % by the optimization.
    %
:- pred jumpopt_main(set(label)::in, may_alter_rtti::in, proc_label::in,
    bool::in, bool::in, bool::in, bool::in, counter::in, counter::out,
    list(instruction)::in, list(instruction)::out, bool::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module ll_backend__code_util.
:- import_module ll_backend__opt_util.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.

% We first build up a bunch of tables giving information about labels.
% We then traverse the instruction list, using the information in the
% tables to short-circuit jumps.
%
% Instrmap:     Maps each label to the next real (non-comment, non-livevals)
%               instruction after that label.
% Lvalmap:      Maps each label to yes(Livevals) if the label is followed
%               by a livevals instruction, and to no otherwise.
% Blockmap:     Maps each label to the block following that label.
%               This includes all instructions up to the first one that
%               cannot fall through.
% Procmap:      Maps each label that begins a det epilog to the epilog.
% Succmap:      Maps each label that begins a nondet epilog to the epilog.
% Sdprocmap:    Maps each label that begins a semidet epilog to the epilog.
%               This can be the success epilog or the failure epilog.
% Forkmap:      Maps each label that begins a full semidet epilog (code to
%               test r1, and to execute the the success or failure epilog
%               depending on the result) to the epilog.
%
% Blockmap will not contain the initial block of the procedure unless
% Recjump is set. The intention is that Recjump will not be set until
% frameopt, which can do a better job of optimizing this block, have
% been applied.

jumpopt_main(LayoutLabels, MayAlterRtti, ProcLabel, Fulljumpopt, Recjump,
        PessimizeTailCalls, CheckedNondetTailCall, !C, !Instrs, Mod) :-
    some [!Instrmap, !Blockmap, !Lvalmap, !Procmap, !Sdprocmap, !Succmap,
            !Forkmap] (
        Instrs0 = !.Instrs,
        map.init(!:Instrmap),
        map.init(!:Blockmap),
        map.init(!:Lvalmap),
        map.init(!:Procmap),
        map.init(!:Sdprocmap),
        map.init(!:Succmap),
        jumpopt__build_maps(!.Instrs, Recjump, !Instrmap, !Blockmap, !Lvalmap,
            !Procmap, !Sdprocmap, !Succmap),
        jumpopt__build_forkmap(!.Instrs, !.Sdprocmap, map.init, !:Forkmap),
        (
            PessimizeTailCalls = no
        ;
            PessimizeTailCalls = yes,
            !:Procmap = map.init,
            !:Sdprocmap = map.init,
            !:Succmap = map.init,
            !:Forkmap = map.init
        ),
        (
            CheckedNondetTailCall = yes,
            CheckedNondetTailCallInfo0 = yes(ProcLabel - !.C),
            jumpopt__instr_list(!.Instrs, comment(""), !.Instrmap, !.Blockmap,
                !.Lvalmap, !.Procmap, !.Sdprocmap, !.Forkmap, !.Succmap,
                LayoutLabels, Fulljumpopt, MayAlterRtti,
                CheckedNondetTailCallInfo0, CheckedNondetTailCallInfo,
                !:Instrs),
            (
                CheckedNondetTailCallInfo = yes(_ - !:C)
            ;
                CheckedNondetTailCallInfo = no,
                error("jumpopt_main: lost the next label number")
            )
        ;
            CheckedNondetTailCall = no,
            CheckedNondetTailCallInfo0 = no,
            jumpopt__instr_list(!.Instrs, comment(""), !.Instrmap, !.Blockmap,
                !.Lvalmap, !.Procmap, !.Sdprocmap, !.Forkmap, !.Succmap,
                LayoutLabels, Fulljumpopt, MayAlterRtti,
                CheckedNondetTailCallInfo0, _, !:Instrs)
        ),
        opt_util__filter_out_bad_livevals(!Instrs),
        ( !.Instrs = Instrs0 ->
            Mod = no
        ;
            Mod = yes
        )
    ).

%-----------------------------------------------------------------------------%

:- pred jumpopt__build_maps(list(instruction)::in, bool::in,
    instrmap::in, instrmap::out, tailmap::in, tailmap::out,
    lvalmap::in, lvalmap::out, tailmap::in, tailmap::out,
    tailmap::in, tailmap::out, tailmap::in, tailmap::out) is det.

jumpopt__build_maps([], _, !Instrmap, !Blockmap,
        !Lvalmap, !Procmap, !Sdprocmap, !Succmap).
jumpopt__build_maps([Instr0 | Instrs0], Recjump, !Instrmap, !Blockmap,
        !Lvalmap, !Procmap, !Sdprocmap, !Succmap) :-
    Instr0 = Uinstr0 - _,
    ( Uinstr0 = label(Label) ->
        opt_util__skip_comments(Instrs0, Instrs1),
        ( Instrs1 = [Instr1 | _], Instr1 = livevals(_) - _ ->
            map__det_insert(!.Lvalmap, Label, yes(Instr1), !:Lvalmap)
        ;
            map__det_insert(!.Lvalmap, Label, no, !:Lvalmap)
        ),
        opt_util__skip_comments_livevals(Instrs1, Instrs2),
        ( Instrs2 = [Instr2 | _] ->
            map__det_insert(!.Instrmap, Label, Instr2, !:Instrmap)
        ;
            true
        ),
        ( opt_util__is_proceed_next(Instrs1, Between1) ->
            map__det_insert(!.Procmap, Label, Between1, !:Procmap)
        ;
            true
        ),
        ( opt_util__is_sdproceed_next(Instrs1, Between2) ->
            map__det_insert(!.Sdprocmap, Label, Between2, !:Sdprocmap)
        ;
            true
        ),
        ( opt_util__is_succeed_next(Instrs1, Between3) ->
            map__det_insert(!.Succmap, Label, Between3, !:Succmap)
        ;
            true
        ),
        % Put the start of the procedure into Blockmap only after
        % frameopt has had a shot at it.
        (
            ( Label = internal(_, _)
            ; Recjump = yes
            )
        ->
            opt_util__find_no_fallthrough(Instrs1, Block),
            map__det_insert(!.Blockmap, Label, Block, !:Blockmap)
        ;
            true
        )
    ;
        true
    ),
    jumpopt__build_maps(Instrs0, Recjump, !Instrmap, !Blockmap, !Lvalmap,
        !Procmap, !Sdprocmap, !Succmap).

    % Find labels followed by a test of r1 where both paths set r1 to
    % its original value and proceed.
    %
:- pred jumpopt__build_forkmap(list(instruction)::in, tailmap::in,
    tailmap::in, tailmap::out) is det.

jumpopt__build_forkmap([], _Sdprocmap, !Forkmap).
jumpopt__build_forkmap([Instr - _Comment|Instrs], Sdprocmap, !Forkmap) :-
    (
        Instr = label(Label),
        opt_util__is_forkproceed_next(Instrs, Sdprocmap, Between)
    ->
        map__det_insert(!.Forkmap, Label, Between, !:Forkmap)
    ;
        true
    ),
    jumpopt__build_forkmap(Instrs, Sdprocmap, !Forkmap).

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
    %
:- pred jumpopt__instr_list(list(instruction)::in, instr::in, instrmap::in,
    tailmap::in, lvalmap::in, tailmap::in, tailmap::in, tailmap::in,
    tailmap::in, set(label)::in, bool::in, may_alter_rtti::in,
    maybe(pair(proc_label, counter))::in,
    maybe(pair(proc_label, counter))::out, list(instruction)::out) is det.

jumpopt__instr_list([], _PrevInstr, _Instrmap, _Blockmap, _Lvalmap,
        _Procmap, _Sdprocmap, _Forkmap, _Succmap, _LayoutLabels,
        _Fulljumpopt, _MayAlterRtti, !CheckedNondetTailCallInfo, []).
jumpopt__instr_list([Instr0 | Instrs0], PrevInstr, Instrmap, Blockmap,
        Lvalmap, Procmap, Sdprocmap, Forkmap, Succmap, LayoutLabels,
        Fulljumpopt, MayAlterRtti, !CheckedNondetTailCallInfo, Instrs) :-
    Instr0 = Uinstr0 - Comment0,
    % We do a switch on the instruction type to ensure that we short circuit
    % all the labels that are in Instrmap but not in LayoutLabels in *all*
    % instructions in which they occur. This means we must fully search
    % every part of every instruction that may possibly hold a label.
    % In theory, this means every lval and every rval, but in practice we
    % know that rvals representing e.g. the tags of fields cannot contain
    % labels.
    (
        Uinstr0 = call(Proc, RetAddr, LiveInfos, Context, GoalPath, CallModel),
        ( RetAddr = label(RetLabel) ->
            (
                % Look for det style tailcalls. We look for this
                % even if the call is semidet because one of the
                % optimizations below turns a pair of semidet epilogs
                % into a det epilog.
                ( CallModel = det ; CallModel = semidet ),
                map__search(Procmap, RetLabel, Between0),
                PrevInstr = livevals(Livevals),
                MayAlterRtti = may_alter_rtti,
                not set__member(RetLabel, LayoutLabels)
            ->
                opt_util__filter_out_livevals(Between0, Between1),
                list__append(Between1, [livevals(Livevals) - "",
                    goto(Proc) - redirect_comment(Comment0)], NewInstrs),
                RemainInstrs = Instrs0
            ;
                % Look for semidet style tailcalls.
                CallModel = semidet,
                map__search(Forkmap, RetLabel, Between),
                PrevInstr = livevals(Livevals),
                MayAlterRtti = may_alter_rtti,
                not set__member(RetLabel, LayoutLabels)
            ->
                list__append(Between, [livevals(Livevals) - "",
                    goto(Proc) - redirect_comment(Comment0)], NewInstrs),
                RemainInstrs = Instrs0
            ;
                % Look for nondet style tailcalls which do not need
                % a runtime check.
                CallModel = nondet(unchecked_tail_call),
                map__search(Succmap, RetLabel, BetweenIncl),
                BetweenIncl = [livevals(_) - _, goto(_) - _],
                PrevInstr = livevals(Livevals),
                MayAlterRtti = may_alter_rtti,
                not set__member(RetLabel, LayoutLabels)
            ->
                NewInstrs = [
                    assign(maxfr, lval(prevfr(lval(curfr))))
                        - "discard this frame",
                    assign(succip, lval(succip(lval(curfr))))
                        - "setup PC on return from tailcall",
                    assign(curfr, lval(succfr(lval(curfr))))
                        - "setup curfr on return from tailcall",
                    livevals(Livevals) - "",
                    goto(Proc) - redirect_comment(Comment0)
                ],
                RemainInstrs = Instrs0
            ;
                % Look for nondet style tailcalls which do need
                % a runtime check.
                CallModel = nondet(checked_tail_call),
                !.CheckedNondetTailCallInfo = yes(ProcLabel - Counter0),
                map__search(Succmap, RetLabel, BetweenIncl),
                BetweenIncl = [livevals(_) - _, goto(_) - _],
                PrevInstr = livevals(Livevals),
                MayAlterRtti = may_alter_rtti,
                not set__member(RetLabel, LayoutLabels)
            ->
                counter__allocate(LabelNum, Counter0, Counter1),
                NewLabel = internal(LabelNum, ProcLabel),
                NewInstrs = [
                    if_val(binop(ne, lval(curfr), lval(maxfr)),
                        label(NewLabel))
                        - "branch around if cannot tail call",
                    assign(maxfr, lval(prevfr(lval(curfr))))
                        - "discard this frame",
                    assign(succip, lval(succip(lval(curfr))))
                        - "setup PC on return from tailcall",
                    assign(curfr, lval(succfr(lval(curfr))))
                        - "setup curfr on return from tailcall",
                    livevals(Livevals) - "",
                    goto(Proc) - redirect_comment(Comment0),
                    label(NewLabel) - "non tail call",
                    Instr0
                ],
                RemainInstrs = Instrs0,
                !:CheckedNondetTailCallInfo = yes(ProcLabel - Counter1)
            ;
                % Short circuit the return label if possible.
                map__search(Instrmap, RetLabel, RetInstr),
                MayAlterRtti = may_alter_rtti,
                not set__member(RetLabel, LayoutLabels)
            ->
                jumpopt__final_dest(Instrmap, RetLabel, DestLabel,
                    RetInstr, _DestInstr),
                ( RetLabel = DestLabel ->
                    NewInstrs = [Instr0],
                    RemainInstrs = Instrs0
                ;
                    NewInstrs = [call(Proc, label(DestLabel), LiveInfos,
                        Context, GoalPath, CallModel)
                        - redirect_comment(Comment0)],
                    RemainInstrs = Instrs0
                )
            ;
                NewInstrs = [Instr0],
                RemainInstrs = Instrs0
            )
        ;
            NewInstrs = [Instr0],
            RemainInstrs = Instrs0
        )
    ;
        Uinstr0 = goto(TargetAddr),
        ( TargetAddr = label(TargetLabel) ->
            (
                % Eliminate the goto if possible.
                opt_util__is_this_label_next(TargetLabel, Instrs0, _)
            ->
                NewInstrs = [],
                RemainInstrs = Instrs0
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
                RemainInstrs = Instrs0
            ;
                % Replace a jump to a det epilog with the epilog.
                map__search(Procmap, TargetLabel, Between0)
            ->
                jumpopt__adjust_livevals(PrevInstr, Between0, Between),
                NewInstrs = Between ++ [goto(succip) - "shortcircuit"],
                RemainInstrs = Instrs0
            ;
                % Replace a jump to a semidet epilog with the epilog.
                map__search(Sdprocmap, TargetLabel, Between0)
            ->
                jumpopt__adjust_livevals(PrevInstr, Between0, Between),
                NewInstrs = Between ++ [goto(succip) - "shortcircuit"],
                RemainInstrs = Instrs0
            ;
                % Replace a jump to a nondet epilog with the epilog.
                map__search(Succmap, TargetLabel, BetweenIncl0)
            ->
                jumpopt__adjust_livevals(PrevInstr, BetweenIncl0, NewInstrs),
                RemainInstrs = Instrs0
            ;
                % Replace a jump to a non-epilog block with the
                % block itself. These jumps are treated separately
                % from jumps to epilog blocks, for two reasons.
                % First, epilog blocks are always short, so we
                % always want to replace jumps to them, whereas
                % other blocks may be long, so we want to replace
                % jumps to them only if the fulljumps option
                % was given. Second, non-epilog blocks may contain
                % branches to other labels in this procedure, and
                % we want to make sure that these are short-circuited.
                % This short-circuiting is necessary because
                % another optimization below eliminates labels,
                % which is correct only if jumps to those labels
                % are short-circuited everywhere.
                Fulljumpopt = yes,
                map__search(Instrmap, TargetLabel, TargetInstr),
                jumpopt__final_dest(Instrmap, TargetLabel, DestLabel,
                    TargetInstr, _DestInstr),
                map__search(Blockmap, DestLabel, Block),
                block_may_be_duplicated(Block) = yes
            ->
                opt_util__filter_out_labels(Block, FilteredBlock),
                jumpopt__adjust_livevals(PrevInstr, FilteredBlock,
                    AdjustedBlock),
                % Block may end with a goto to DestLabel. We avoid
                % infinite expansion in such cases by removing
                % DestLabel from Blockmap, though only while
                % processing AdjustedBlock.
                map__delete(Blockmap, DestLabel, CrippledBlockmap),
                jumpopt__instr_list(AdjustedBlock, comment(""), Instrmap,
                    CrippledBlockmap, Lvalmap, Procmap, Sdprocmap, Forkmap,
                    Succmap, LayoutLabels, Fulljumpopt, MayAlterRtti,
                    !CheckedNondetTailCallInfo, NewInstrs),
                RemainInstrs = Instrs0
            ;
                % Short-circuit the goto.
                map__search(Instrmap, TargetLabel, TargetInstr)
            ->
                jumpopt__final_dest(Instrmap, TargetLabel, DestLabel,
                    TargetInstr, DestInstr),
                DestInstr = UdestInstr - _Destcomment,
                Shorted = "shortcircuited jump: " ++ Comment0,
                opt_util__can_instr_fall_through(UdestInstr, Canfallthrough),
                (
                    Canfallthrough = no,
                    NewInstrs0 = [UdestInstr - Shorted],
                    RemainInstrs = Instrs0
                ;
                    Canfallthrough = yes,
                    ( TargetLabel = DestLabel ->
                        NewInstrs0 = [Instr0],
                        RemainInstrs = Instrs0
                    ;
                        NewInstrs0 = [goto(label(DestLabel)) - Shorted],
                        RemainInstrs = Instrs0
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
                RemainInstrs = Instrs0
            )
        ;
            NewInstrs = [Instr0],
            RemainInstrs = Instrs0
        )
    ;
        Uinstr0 = computed_goto(Index, LabelList0),
        % Short-circuit all the destination labels.
        jumpopt__short_labels(Instrmap, LabelList0, LabelList),
        RemainInstrs = Instrs0,
        ( LabelList = LabelList0 ->
            NewInstrs = [Instr0]
        ;
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs = [computed_goto(Index, LabelList) - Shorted]
        )
    ;
        Uinstr0 = if_val(Cond, TargetAddr),
        ( TargetAddr = label(TargetLabel) ->
            (
                % Attempt to transform code such as
                %
                %   if (Cond) L2
                % L1:
                %   goto L3
                % L2:   ...
                %
                % into
                %
                %   if (! Cond) L3
                % L2:   ...
                %
                % The label L1 may be present or not. If it is present,
                % we are eliminating it, which is possible only because
                % we short-circuit all jumps to it (make them jump
                % directly to L3). This may not be possible if L3 is
                % a non-label code address; e.g. we cannot jump to
                % non-label code addresses from computed gotos.
                opt_util__skip_comments(Instrs0, Instrs1),
                Instrs1 = [Instr1 | Instrs2],
                ( Instr1 = label(ElimLabel) - _ ->
                    not set__member(ElimLabel, LayoutLabels),
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
                RemainInstrs = [NewInstr | AfterGoto]
            ;
                % Attempt to transform code such as
                %
                %   if (Cond) L1
                %   goto L2
                %
                % into
                %
                %   if (! Cond) L2
                %   <code at L1>
                %
                % when we know the code at L1 and don't know the code
                % at L2. Here, we just generate
                %
                %   if (! Cond) L2
                %   goto L1
                %
                % and get the code processed again starting after the
                % if_val, to get the recursive call to replace the goto
                % to L1 with the code at L1.
                Fulljumpopt = yes,
                map__search(Blockmap, TargetLabel, _TargetBlock),
                opt_util__skip_comments(Instrs0, Instrs1),
                Instrs1 = [GotoInstr | AfterGoto],
                GotoInstr = goto(GotoAddr) - GotoComment,
                \+ (
                    GotoAddr = label(GotoLabel),
                    map__search(Blockmap, GotoLabel, _)
                )
            ->
                code_util__neg_rval(Cond, NotCond),
                NewIfInstr = if_val(NotCond, GotoAddr) - GotoComment,
                NewInstrs = [NewIfInstr],
                NewGotoComment = Comment0 ++ " (switched)",
                NewGotoInstr = goto(label(TargetLabel)) - NewGotoComment,
                RemainInstrs = [NewGotoInstr | AfterGoto]
            ;
                map__search(Instrmap, TargetLabel, TargetInstr)
            ->
                jumpopt__final_dest(Instrmap, TargetLabel, DestLabel,
                    TargetInstr, _DestInstr),
                (
                    % Attempt to transform code such as
                    %
                    %   if (Cond) L1
                    %   r1 = MR_TRUE
                    %   <epilog>
                    %   ...
                    % L1:
                    %   r1 = MR_FALSE
                    %   <epilog>
                    %
                    % into
                    %
                    %   r1 = Cond
                    %   <epilog>
                    %
                    opt_util__is_sdproceed_next(Instrs0, BetweenFT),
                    map__search(Blockmap, DestLabel, Block),
                    opt_util__is_sdproceed_next(Block, BetweenBR),
                    opt_util__filter_out_r1(BetweenFT, yes(SuccessFT),
                        Between),
                    opt_util__filter_out_r1(BetweenBR, yes(SuccessBR),
                        Between),
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
                    list__append([NewAssign | Between], [Proceed], NewInstrs),
                    RemainInstrs = Instrs0
                ;
                    % Try to short-circuit the destination.
                    TargetLabel \= DestLabel
                ->
                    Shorted = "shortcircuited jump: " ++ Comment0,
                    NewInstrs = [if_val(Cond, label(DestLabel)) - Shorted],
                    RemainInstrs = Instrs0
                ;
                    NewInstrs = [Instr0],
                    RemainInstrs = Instrs0
                )
            ;
                NewInstrs = [Instr0],
                RemainInstrs = Instrs0
            )
        ;
            NewInstrs = [Instr0],
            RemainInstrs = Instrs0
        )
    ;
        Uinstr0 = assign(Lval, Rval0),
        % Any labels mentioned in Rval0 should be short-circuited.
        jumpopt__short_labels_rval(Instrmap, Rval0, Rval),
        RemainInstrs = Instrs0,
        ( Rval = Rval0 ->
            NewInstrs = [Instr0]
        ;
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs = [assign(Lval, Rval) - Shorted]
        )
    ;
        Uinstr0 = mkframe(FrameInfo, Redoip),
        ( Redoip = yes(label(Label0)) ->
            jumpopt__short_label(Instrmap, Label0, Label),
            RemainInstrs = Instrs0,
            ( Label = Label0 ->
                NewInstrs = [Instr0]
            ;
                Shorted = Comment0 ++ " (some shortcircuits)",
                NewInstrs = [mkframe(FrameInfo, yes(label(Label))) - Shorted]
            )
        ;
            NewInstrs = [Instr0],
            RemainInstrs = Instrs0
        )
    ;
		Uinstr0 = pragma_c(Decls, Components0, MayCallMercury,
			MaybeFixNoLayout, MaybeFixLayout, MaybeFixOnlyLayout,
			MaybeNoFix0, StackSlotRef, MaybeDup),
        some [!Redirect] (
            list__map_foldl(short_pragma_component(Instrmap),
                Components0, Components, no, !:Redirect),
            (
                MaybeNoFix0 = yes(NoFix),
                short_label(Instrmap, NoFix, NoFixDest),
                MaybeNoFix = yes(NoFixDest),
                !:Redirect = yes
            ;
                MaybeNoFix0 = no,
                MaybeNoFix = no
            ),
% These sanity checks are too strong, because we don't prohibit labels
% appearing these slots of pragma_c instructions from appearing in Instrmap;
% we only prohibit the use of those entries in Instrmap to optimize away these
% labels.
%
%           (
%               MaybeFixNoLayout = yes(FixNoLayout),
%               short_label(Instrmap, FixNoLayout, FixNoLayoutDest),
%               FixNoLayoutDest \= FixNoLayout
%           ->
%               error("jumpopt__instr_list: pragma_c fix_no_layout")
%           ;
%               true
%           ),
%           (
%               MaybeFixLayout = yes(FixLayout),
%               short_label(Instrmap, FixLayout, FixLayoutDest),
%               FixLayoutDest \= FixLayout
%           ->
%               error("jumpopt__instr_list: pragma_c fix_layout")
%           ;
%               true
%           ),
%           (
%               MaybeFixOnlyLayout = yes(FixOnlyLayout),
%               short_label(Instrmap, FixOnlyLayout, FixOnlyLayoutDest),
%               FixOnlyLayoutDest \= FixOnlyLayout
%           ->
%               error("jumpopt__instr_list: pragma_c fix_only_layout")
%           ;
%               true
%           ),
            (
                !.Redirect = no,
                Instr = Instr0
            ;
                !.Redirect = yes,
                Comment = Comment0 ++ " (some redirects)",
                Uinstr = pragma_c(Decls, Components, MayCallMercury,
                    MaybeFixNoLayout, MaybeFixLayout, MaybeFixOnlyLayout,
                    MaybeNoFix, StackSlotRef, MaybeDup),
                Instr = Uinstr - Comment
            )
        ),
        NewInstrs = [Instr],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = c_code(_, _),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = comment(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = livevals(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = block(_, _, _),
        % These are supposed to be introduced only after jumpopt is run
        % for the last time.
        error("jumpopt__instr_list: block")
    ;
        Uinstr0 = label(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = save_maxfr(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = restore_maxfr(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = incr_sp(_, _),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = decr_sp(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = decr_sp_and_return(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = store_ticket(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = reset_ticket(_, _),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = discard_ticket,
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = prune_ticket,
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = prune_tickets_to(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = mark_ticket_stack(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = mark_hp(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = free_heap(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = incr_hp(_, _, _, _, _),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = restore_hp(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = fork(Child0, Parent0, NumSlots),
        short_label(Instrmap, Child0, Child),
        short_label(Instrmap, Parent0, Parent),
        (
            Child = Child0,
            Parent = Parent0
        ->
            Instr = Instr0
        ;
            Uinstr = fork(Child, Parent, NumSlots),
            Comment = Comment0 ++ " (redirect)",
            Instr = Uinstr - Comment
        ),
        NewInstrs = [Instr],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = init_sync_term(_, _),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = join_and_continue(SyncTerm, Label0),
        short_label(Instrmap, Label0, Label),
        ( Label = Label0 ->
            Instr = Instr0
        ;
            Uinstr = join_and_continue(SyncTerm, Label),
            Comment = Comment0 ++ " (redirect)",
            Instr = Uinstr - Comment
        ),
        NewInstrs = [Instr],
        RemainInstrs = Instrs0
    ;
        Uinstr0 = join_and_terminate(_),
        NewInstrs = [Instr0],
        RemainInstrs = Instrs0
    ),
    ( ( Uinstr0 = comment(_) ; NewInstrs = [] ) ->
        NewPrevInstr = PrevInstr
    ;
        NewPrevInstr = Uinstr0
    ),
    jumpopt__instr_list(RemainInstrs, NewPrevInstr, Instrmap, Blockmap,
        Lvalmap, Procmap, Sdprocmap, Forkmap, Succmap, LayoutLabels,
        Fulljumpopt, MayAlterRtti, !CheckedNondetTailCallInfo, Instrs9),
    list__append(NewInstrs, Instrs9, Instrs).

:- func block_may_be_duplicated(list(instruction)) = bool.

block_may_be_duplicated([]) =  yes.
block_may_be_duplicated([Uinstr - _ | Instrs]) = BlockMayBeDuplicated :-
    InstrMayBeDuplicated = instr_may_be_duplicated(Uinstr),
    (
        InstrMayBeDuplicated = no,
        BlockMayBeDuplicated = no
    ;
        InstrMayBeDuplicated = yes,
        BlockMayBeDuplicated = block_may_be_duplicated(Instrs)
    ).

:- func instr_may_be_duplicated(instr) = bool.

instr_may_be_duplicated(Instr) = InstrMayBeDuplicated :-
    ( Instr ^ pragma_c_fix_onlylayout = yes(_) ->
        % This instruction is a trace event. Duplicating it would
        % increase code size, and may cost more in locality than
        % the benefit represented by the elimination of the jump.
        % When debugging is enabled, size is in any case more important
        % than the last bit of speed.
        InstrMayBeDuplicated = no
    ; Instr ^ pragma_c_maybe_dupl = no ->
        InstrMayBeDuplicated = no
    ;
        InstrMayBeDuplicated = yes
    ).

:- func redirect_comment(string) = string.

redirect_comment(Comment0) = string__append(Comment0, " (redirected return)").

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

:- pred jumpopt__adjust_livevals(instr::in, list(instruction)::in,
    list(instruction)::out) is det.

jumpopt__adjust_livevals(PrevInstr, Instrs0, Instrs) :-
    (
        PrevInstr = livevals(PrevLivevals),
        opt_util__skip_comments(Instrs0, Instrs1),
        Instrs1 = [livevals(BetweenLivevals) - _ | Instrs2]
    ->
        ( BetweenLivevals = PrevLivevals ->
            Instrs = Instrs2
        ;
            error("BetweenLivevals and PrevLivevals differ in jumpopt")
        )
    ;
        Instrs = Instrs0
    ).

%-----------------------------------------------------------------------------%

    % Short-circuit the given label by following any gotos at the
    % labelled instruction or by falling through consecutive labels.
    %
:- pred jumpopt__short_label(instrmap::in, label::in, label::out) is det.

jumpopt__short_label(Instrmap, Label0, Label) :-
    ( map__search(Instrmap, Label0, Instr0) ->
        jumpopt__final_dest(Instrmap, Label0, Label, Instr0, _Instr)
    ;
        Label = Label0
    ).

:- pred jumpopt__short_labels(instrmap::in, list(label)::in, list(label)::out)
    is det.

jumpopt__short_labels(_Instrmap, [], []).
jumpopt__short_labels(Instrmap, [Label0 | Labels0], [Label | Labels]) :-
    jumpopt__short_label(Instrmap, Label0, Label),
    jumpopt__short_labels(Instrmap, Labels0, Labels).

%-----------------------------------------------------------------------------%

    % Find the final destination of a given instruction at a given label.
    % We follow gotos as well as consecutive labels.
    %
:- pred jumpopt__final_dest(instrmap::in, label::in, label::out,
    instruction::in, instruction::out) is det.

jumpopt__final_dest(Instrmap, SrcLabel, DestLabel, SrcInstr, DestInstr) :-
    jumpopt__final_dest_2(Instrmap, [], SrcLabel, DestLabel,
        SrcInstr, DestInstr).

:- pred jumpopt__final_dest_2(instrmap::in, list(label)::in,
    label::in, label::out, instruction::in, instruction::out) is det.

jumpopt__final_dest_2(Instrmap, LabelsSofar, SrcLabel, DestLabel,
        SrcInstr, DestInstr) :-
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
        jumpopt__final_dest_2(Instrmap, [SrcLabel | LabelsSofar],
            TargetLabel, DestLabel, TargetInstr, DestInstr)
    ;
        DestLabel = SrcLabel,
        DestInstr = SrcInstr
    ).

%-----------------------------------------------------------------------------%

:- pred jumpopt__short_labels_rval(instrmap::in, rval::in, rval::out) is det.

jumpopt__short_labels_rval(Instrmap, lval(Lval0), lval(Lval)) :-
    jumpopt__short_labels_lval(Instrmap, Lval0, Lval).
jumpopt__short_labels_rval(_, var(_), _) :-
    error("var rval in jumpopt__short_labels_rval").
jumpopt__short_labels_rval(Instrmap, mkword(Tag, Rval0), mkword(Tag, Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_rval(Instrmap, const(Const0), const(Const)) :-
    jumpopt__short_labels_const(Instrmap, Const0, Const).
jumpopt__short_labels_rval(Instrmap, unop(Op, Rval0), unop(Op, Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_rval(Instrmap, binop(Op, LRval0, RRval0),
        binop(Op, LRval, RRval)) :-
    jumpopt__short_labels_rval(Instrmap, LRval0, LRval),
    jumpopt__short_labels_rval(Instrmap, RRval0, RRval).
jumpopt__short_labels_rval(_, mem_addr(MemRef), mem_addr(MemRef)).

:- pred jumpopt__short_labels_const(instrmap::in,
    rval_const::in, rval_const::out) is det.

jumpopt__short_labels_const(_, true, true).
jumpopt__short_labels_const(_, false, false).
jumpopt__short_labels_const(_, int_const(I), int_const(I)).
jumpopt__short_labels_const(_, float_const(F), float_const(F)).
jumpopt__short_labels_const(_, string_const(S), string_const(S)).
jumpopt__short_labels_const(_, multi_string_const(L, S),
        multi_string_const(L, S)).
jumpopt__short_labels_const(Instrmap, code_addr_const(CodeAddr0),
        code_addr_const(CodeAddr)) :-
    ( CodeAddr0 = label(Label0) ->
        jumpopt__short_label(Instrmap, Label0, Label),
        CodeAddr = label(Label)
    ;
        CodeAddr = CodeAddr0
    ).
jumpopt__short_labels_const(_, data_addr_const(D, O), data_addr_const(D, O)).

:- pred jumpopt__short_labels_maybe_rvals(instrmap::in, list(maybe(rval))::in,
    list(maybe(rval))::out) is det.

jumpopt__short_labels_maybe_rvals(_, [], []).
jumpopt__short_labels_maybe_rvals(Instrmap, [MaybeRval0 | MaybeRvals0],
        [MaybeRval | MaybeRvals]) :-
    jumpopt__short_labels_maybe_rval(Instrmap, MaybeRval0, MaybeRval),
    jumpopt__short_labels_maybe_rvals(Instrmap, MaybeRvals0, MaybeRvals).

:- pred jumpopt__short_labels_maybe_rval(instrmap::in,
    maybe(rval)::in, maybe(rval)::out) is det.

jumpopt__short_labels_maybe_rval(Instrmap, MaybeRval0, MaybeRval) :-
    (
        MaybeRval0 = no,
        MaybeRval = no
    ;
        MaybeRval0 = yes(Rval0),
        jumpopt__short_labels_rval(Instrmap, Rval0, Rval),
        MaybeRval = yes(Rval)
    ).

:- pred jumpopt__short_labels_lval(instrmap::in, lval::in, lval::out) is det.

jumpopt__short_labels_lval(_, reg(T, N), reg(T, N)).
jumpopt__short_labels_lval(_, succip, succip).
jumpopt__short_labels_lval(_, maxfr, maxfr).
jumpopt__short_labels_lval(_, curfr, curfr).
jumpopt__short_labels_lval(_, hp, hp).
jumpopt__short_labels_lval(_, sp, sp).
jumpopt__short_labels_lval(_, temp(T, N), temp(T, N)).
jumpopt__short_labels_lval(_, stackvar(N), stackvar(N)).
jumpopt__short_labels_lval(_, framevar(N), framevar(N)).
jumpopt__short_labels_lval(Instrmap, succip(Rval0), succip(Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_lval(Instrmap, redoip(Rval0), redoip(Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_lval(Instrmap, redofr(Rval0), redofr(Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_lval(Instrmap, succfr(Rval0), succfr(Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_lval(Instrmap, prevfr(Rval0), prevfr(Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_lval(Instrmap, field(Tag, Rval0, Field0),
        field(Tag, Rval, Field)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval),
    jumpopt__short_labels_rval(Instrmap, Field0, Field).
jumpopt__short_labels_lval(Instrmap, mem_ref(Rval0), mem_ref(Rval)) :-
    jumpopt__short_labels_rval(Instrmap, Rval0, Rval).
jumpopt__short_labels_lval(_, lvar(_), _) :-
    error("lvar lval in jumpopt__short_labels_lval").

:- pred short_pragma_component(instrmap::in,
    pragma_c_component::in, pragma_c_component::out,
    bool::in, bool::out) is det.

short_pragma_component(Instrmap, !Component, !Redirect) :-
    (
        !.Component = pragma_c_inputs(_)
    ;
        !.Component = pragma_c_outputs(_)
    ;
        !.Component = pragma_c_user_code(_, _)
    ;
        !.Component = pragma_c_raw_code(_, _, _)
    ;
        !.Component = pragma_c_fail_to(Label0),
        short_label(Instrmap, Label0, Label),
        !:Component = pragma_c_fail_to(Label),
        ( Label = Label0 ->
            true
        ;
            !:Redirect = yes
        )
    ;
        !.Component = pragma_c_noop
    ).

%-----------------------------------------------------------------------------%
