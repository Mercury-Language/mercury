%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: jumpopt.m.
% Author: zs.
%
% This module contains code that optimizes jumps to jumps.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.jumpopt.
:- interface.

:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % optimize_jumps_in_proc(LayoutLabels, MayAlterRtti, ProcLabel,
    %   Fulljumpopt, Recjump, PessimizeTailCalls, CheckedNondetTailCall,
    %   !LabelCounter, !Instrs, Mod):
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
:- pred optimize_jumps_in_proc(set(label)::in, may_alter_rtti::in,
    proc_label::in, bool::in, bool::in, bool::in, bool::in,
    counter::in, counter::out, list(instruction)::in, list(instruction)::out,
    bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module ll_backend.code_util.
:- import_module ll_backend.opt_util.
:- import_module parse_tree.prog_data.

:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

% We first build up a bunch of tables giving information about labels.
% We then traverse the instruction list, using the information in the
% tables to short-circuit jumps.
%
% InstrMap:     Maps each label to the next real (non-comment, non-livevals)
%               instruction after that label.
% LvalMap:      Maps each label to yes(Livevals) if the label is followed
%               by a livevals instruction, and to no otherwise.
% BlockMap:     Maps each label to the block following that label.
%               This includes all instructions up to the first one that
%               cannot fall through.
% ProcMap:      Maps each label that begins a det epilog to the epilog.
% SuccMap:      Maps each label that begins a nondet epilog to the epilog.
% SdprocMap:    Maps each label that begins a semidet epilog to the epilog.
%               This can be the success epilog or the failure epilog.
% ForkMap:      Maps each label that begins a full semidet epilog (code to
%               test r1, and to execute the the success or failure epilog
%               depending on the result) to the epilog.
%
% BlockMap will not contain the initial block of the procedure unless
% Recjump is set. The intention is that Recjump will not be set until
% frameopt, which can do a better job of optimizing this block, have
% been applied.

optimize_jumps_in_proc(LayoutLabels, MayAlterRtti, ProcLabel, Fulljumpopt,
        Recjump, PessimizeTailCalls, CheckedNondetTailCall, !C, !Instrs,
        Mod) :-
    some [!InstrMap, !BlockMap, !LvalMap, !ProcMap, !SdprocMap, !SuccMap,
        !ForkMap]
    (
        Instrs0 = !.Instrs,
        map.init(!:InstrMap),
        map.init(!:BlockMap),
        map.init(!:LvalMap),
        map.init(!:ProcMap),
        map.init(!:SdprocMap),
        map.init(!:SuccMap),
        jump_opt_build_maps(!.Instrs, Recjump, !InstrMap, !BlockMap, !LvalMap,
            !ProcMap, !SdprocMap, !SuccMap),
        jump_opt_build_forkmap(!.Instrs, !.SdprocMap, map.init, !:ForkMap),
        (
            PessimizeTailCalls = no
        ;
            PessimizeTailCalls = yes,
            !:ProcMap = map.init,
            !:SdprocMap = map.init,
            !:SuccMap = map.init,
            !:ForkMap = map.init
        ),
        JumpOptInfo = jump_opt_info(!.InstrMap, !.BlockMap, !.LvalMap,
            !.ProcMap, !.SdprocMap, !.ForkMap, !.SuccMap, LayoutLabels,
            Fulljumpopt, MayAlterRtti),
        (
            CheckedNondetTailCall = yes,
            CheckedNondetTailCallInfo0 = yes(ProcLabel - !.C),
            jump_opt_instr_list(!.Instrs, comment(""), JumpOptInfo,
                CheckedNondetTailCallInfo0, CheckedNondetTailCallInfo,
                [], RevInstrs),
            (
                CheckedNondetTailCallInfo = yes(_ - !:C)
            ;
                CheckedNondetTailCallInfo = no,
                unexpected(this_file,
                    "jumpopt_main: lost the next label number")
            )
        ;
            CheckedNondetTailCall = no,
            CheckedNondetTailCallInfo0 = no,
            jump_opt_instr_list(!.Instrs, comment(""), JumpOptInfo,
                CheckedNondetTailCallInfo0, _, [], RevInstrs)
        ),
        list.reverse(RevInstrs, !:Instrs),
        opt_util.filter_out_bad_livevals(!Instrs),
        ( !.Instrs = Instrs0 ->
            Mod = no
        ;
            Mod = yes
        )
    ).

%-----------------------------------------------------------------------------%

:- pred jump_opt_build_maps(list(instruction)::in, bool::in,
    instrmap::in, instrmap::out, tailmap::in, tailmap::out,
    lvalmap::in, lvalmap::out, tailmap::in, tailmap::out,
    tailmap::in, tailmap::out, tailmap::in, tailmap::out) is det.

jump_opt_build_maps([], _, !InstrMap, !BlockMap,
        !LvalMap, !ProcMap, !SdprocMap, !SuccMap).
jump_opt_build_maps([Instr0 | Instrs0], Recjump, !InstrMap, !BlockMap,
        !LvalMap, !ProcMap, !SdprocMap, !SuccMap) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    ( Uinstr0 = label(Label) ->
        opt_util.skip_comments(Instrs0, Instrs1),
        ( Instrs1 = [Instr1 | _], Instr1 = llds_instr(livevals(_), _) ->
            map.det_insert(Label, yes(Instr1), !LvalMap)
        ;
            map.det_insert(Label, no, !LvalMap)
        ),
        opt_util.skip_comments_livevals(Instrs1, Instrs2),
        ( Instrs2 = [Instr2 | _] ->
            map.det_insert(Label, Instr2, !InstrMap)
        ;
            true
        ),
        ( opt_util.is_proceed_next(Instrs1, Between1) ->
            map.det_insert(Label, Between1, !ProcMap)
        ;
            true
        ),
        ( opt_util.is_sdproceed_next(Instrs1, Between2) ->
            map.det_insert(Label, Between2, !SdprocMap)
        ;
            true
        ),
        ( opt_util.is_succeed_next(Instrs1, Between3) ->
            map.det_insert(Label, Between3, !SuccMap)
        ;
            true
        ),
        % Put the start of the procedure into Blockmap only after
        % frameopt has had a shot at it.
        (
            (
                Label = internal_label(_, _),
                % We put entry labels into !BlockMap only if the comment
                % does NOT end with "nofulljump".
                not string.suffix(Comment0, "nofulljump")
            ;
                Label = entry_label(_, _),
                % We put entry labels into !BlockMap only if Recjump = yes.
                Recjump = yes
            )
        ->
            opt_util.find_no_fallthrough(Instrs1, Block),
            map.det_insert(Label, Block, !BlockMap)
        ;
            true
        )
    ;
        true
    ),
    jump_opt_build_maps(Instrs0, Recjump, !InstrMap, !BlockMap, !LvalMap,
        !ProcMap, !SdprocMap, !SuccMap).

    % Find labels followed by a test of r1 where both paths set r1 to
    % its original value and proceed.
    %
:- pred jump_opt_build_forkmap(list(instruction)::in, tailmap::in,
    tailmap::in, tailmap::out) is det.

jump_opt_build_forkmap([], _SdprocMap, !ForkMap).
jump_opt_build_forkmap([llds_instr(Uinstr, _Comment) | Instrs], SdprocMap,
        !ForkMap) :-
    (
        Uinstr = label(Label),
        opt_util.is_forkproceed_next(Instrs, SdprocMap, Between)
    ->
        map.det_insert(Label, Between, !ForkMap)
    ;
        true
    ),
    jump_opt_build_forkmap(Instrs, SdprocMap, !ForkMap).

%-----------------------------------------------------------------------------%

:- type jump_opt_info
    --->    jump_opt_info(
                joi_instr_map       :: instrmap,
                joi_block_map       :: tailmap,
                joi_lval_map        :: lvalmap,
                joi_proc_map        :: tailmap,
                joi_sdproc_map      :: tailmap,
                joi_fork_map        :: tailmap,
                joi_succ_map        :: tailmap,
                joi_layout_labels   :: set(label),
                joi_full_jump_opt   :: bool,
                joi_may_alter_rtti  :: may_alter_rtti
            ).

:- type new_remain
    --->    specified(
                new_instructions        :: list(instruction),
                remaining_instructions  :: list(instruction)
            )
    ;       usual_case.
            % The list of new instructions contains just Instr0, and
            % the list of remaining instructions, on which to recurse,
            % Instrs0.

    % Optimize the given instruction list by eliminating unnecessary jumps.
    %
    % We handle calls by attempting to turn them into tailcalls. If this fails,
    % we try to short-circuit the return address.
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
    % We build up the generated instruction list in reverse order, because
    % building it in right order would make instr_list not tail recursive,
    % and thus unable to handle very long instruction lists.
    %
:- pred jump_opt_instr_list(list(instruction)::in, instr::in,
    jump_opt_info::in,
    maybe(pair(proc_label, counter))::in,
    maybe(pair(proc_label, counter))::out,
    list(instruction)::in, list(instruction)::out) is det.

jump_opt_instr_list([], _PrevInstr, _, !CheckedNondetTailCallInfo, !RevInstrs).
jump_opt_instr_list([Instr0 | Instrs0], PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, !RevInstrs) :-
    Instr0 = llds_instr(Uinstr0, Comment0),
    % We do a switch on the instruction type to ensure that we short circuit
    % all the labels that are in InstrMap but not in LayoutLabels in *all*
    % instructions in which they occur. This means we must fully search
    % every part of every instruction that may possibly hold a label.
    % In theory, this means every lval and every rval, but in practice we
    % know that rvals representing e.g. the tags of fields cannot contain
    % labels.
    (
        Uinstr0 = llcall(_, _, _, _, _, _),
        jump_opt_llcall(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
            !CheckedNondetTailCallInfo, NewRemain)
    ;
        Uinstr0 = goto(_),
        jump_opt_goto(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
            !CheckedNondetTailCallInfo, NewRemain)
    ;
        Uinstr0 = computed_goto(Index, MaybeTargets0),
        InstrMap = JumpOptInfo ^ joi_instr_map,
        % Short-circuit all the destination labels.
        short_maybe_labels(InstrMap, MaybeTargets0, MaybeTargets),
        ( MaybeTargets = MaybeTargets0 ->
            NewRemain = usual_case
        ;
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs =
                [llds_instr(computed_goto(Index, MaybeTargets), Shorted)],
            NewRemain = specified(NewInstrs, Instrs0)
        )
    ;
        Uinstr0 = if_val(_, _),
        jump_opt_if_val(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
            !CheckedNondetTailCallInfo, NewRemain)
    ;
        Uinstr0 = assign(Lval, Rval0),
        % Any labels mentioned in Rval0 should be short-circuited.
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_labels_rval(InstrMap, Rval0, Rval),
        ( Rval = Rval0 ->
            NewRemain = usual_case
        ;
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs = [llds_instr(assign(Lval, Rval), Shorted)],
            NewRemain = specified(NewInstrs, Instrs0)
        )
    ;
        Uinstr0 = keep_assign(Lval, Rval0),
        % Any labels mentioned in Rval0 should be short-circuited.
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_labels_rval(InstrMap, Rval0, Rval),
        ( Rval = Rval0 ->
            NewRemain = usual_case
        ;
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs = [llds_instr(keep_assign(Lval, Rval), Shorted)],
            NewRemain = specified(NewInstrs, Instrs0)
        )
    ;
        Uinstr0 = mkframe(FrameInfo, Redoip),
        ( Redoip = yes(code_label(Label0)) ->
            InstrMap = JumpOptInfo ^ joi_instr_map,
            short_label(InstrMap, Label0, Label),
            ( Label = Label0 ->
                NewRemain = usual_case
            ;
                Shorted = Comment0 ++ " (some shortcircuits)",
                NewInstrs =
                    [llds_instr(mkframe(FrameInfo, yes(code_label(Label))),
                        Shorted)],
                NewRemain = specified(NewInstrs, Instrs0)
            )
        ;
            NewRemain = usual_case
        )
    ;
        Uinstr0 = foreign_proc_code(_, _, _, _, _, _, _, _, _, _),
        jump_opt_foreign_proc_code(Uinstr0, Comment0, Instrs0, PrevInstr,
            JumpOptInfo, !CheckedNondetTailCallInfo, NewRemain)
    ;
        Uinstr0 = block(_, _, _),
        % These are supposed to be introduced only after jumpopt is run
        % for the last time.
        unexpected(this_file, "jump_opt_instr_list: block")
    ;
        Uinstr0 = fork_new_child(SyncTerm, Child0),
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_label(InstrMap, Child0, Child),
        ( Child = Child0 ->
            NewRemain = usual_case
        ;
            Uinstr = fork_new_child(SyncTerm, Child),
            Comment = Comment0 ++ " (redirect)",
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = specified([Instr], Instrs0)
        )
    ;
        Uinstr0 = join_and_continue(SyncTerm, Label0),
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_label(InstrMap, Label0, Label),
        ( Label = Label0 ->
            NewRemain = usual_case
        ;
            Uinstr = join_and_continue(SyncTerm, Label),
            Comment = Comment0 ++ " (redirect)",
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = specified([Instr], Instrs0)
        )
    ;
        ( Uinstr0 = arbitrary_c_code(_, _, _)
        ; Uinstr0 = comment(_)
        ; Uinstr0 = livevals(_)
        ; Uinstr0 = label(_)
        ; Uinstr0 = save_maxfr(_)
        ; Uinstr0 = restore_maxfr(_)
        ; Uinstr0 = incr_sp(_, _, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ; Uinstr0 = push_region_frame(_, _)
        ; Uinstr0 = region_fill_frame(_, _, _, _, _)
        ; Uinstr0 = region_set_fixed_slot(_, _, _)
        ; Uinstr0 = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr0 = store_ticket(_)
        ; Uinstr0 = reset_ticket(_, _)
        ; Uinstr0 = discard_ticket
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = prune_tickets_to(_)
        ; Uinstr0 = mark_ticket_stack(_)
        ; Uinstr0 = mark_hp(_)
        ; Uinstr0 = free_heap(_)
        ; Uinstr0 = incr_hp(_, _, _, _, _, _, _, _)
        ; Uinstr0 = restore_hp(_)
        ; Uinstr0 = init_sync_term(_, _, _)
        ),
        NewRemain = usual_case
    ),
    (
        NewRemain = usual_case,
        ReplacementInstrsEmpty = no,
        RecurseInstrs = Instrs0,
        !:RevInstrs = [Instr0 | !.RevInstrs]
    ;
        NewRemain = specified(ReplacementInstrs, RecurseInstrs),
        % ReplacementInstrs are in the right order, but they will be reversed
        % by our caller. We therefore reverse them here, which allows that
        % final reverse to put them in the right order.
        !:RevInstrs = list.reverse(ReplacementInstrs) ++ !.RevInstrs,
        (
            ReplacementInstrs = [],
            ReplacementInstrsEmpty = yes
        ;
            ReplacementInstrs = [_ | _],
            ReplacementInstrsEmpty = no
        )
    ),
    (
        ( Uinstr0 = comment(_)
        ; ReplacementInstrsEmpty = yes
        )
    ->
        NewPrevInstr = PrevInstr
    ;
        NewPrevInstr = Uinstr0
    ),
    jump_opt_instr_list(RecurseInstrs, NewPrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, !RevInstrs).

:- pred jump_opt_llcall(instr::in(instr_llcall), string::in,
    list(instruction)::in, instr::in, jump_opt_info::in,
    maybe(pair(proc_label, counter))::in,
    maybe(pair(proc_label, counter))::out,
    new_remain::out) is det.

jump_opt_llcall(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = llcall(Proc, RetAddr, LiveInfos, Context, GoalPath, CallModel),
    ( RetAddr = code_label(RetLabel) ->
        (
            (
                JumpOptInfo ^ joi_may_alter_rtti = must_not_alter_rtti
            ;
                LayoutLabels = JumpOptInfo ^ joi_layout_labels,
                set.member(RetLabel, LayoutLabels)
            )
        ->
            % We cannot optimize the call. Test for this once, here, instead
            % of at the end of each of the following conditions.
            NewRemain = usual_case
        ;
            % Look for det style tailcalls. We look for this even if
            % the call is semidet, because one of the optimizations below
            % turns a pair of semidet epilogs into a det epilog.
            ( CallModel = call_model_det
            ; CallModel = call_model_semidet
            ),
            ProcMap = JumpOptInfo ^ joi_proc_map,
            map.search(ProcMap, RetLabel, Between0),
            PrevInstr = livevals(Livevals)
        ->
            opt_util.filter_out_livevals(Between0, Between1),
            NewInstrs = Between1 ++
                [llds_instr(livevals(Livevals), ""),
                llds_instr(goto(Proc), redirect_comment(Comment0))],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Look for semidet style tailcalls.
            CallModel = call_model_semidet,
            ForkMap = JumpOptInfo ^ joi_fork_map,
            map.search(ForkMap, RetLabel, Between),
            PrevInstr = livevals(Livevals)
        ->
            NewInstrs = Between ++
                [llds_instr(livevals(Livevals), ""),
                llds_instr(goto(Proc), redirect_comment(Comment0))],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Look for nondet style tailcalls which do not need
            % a runtime check.
            CallModel = call_model_nondet(unchecked_tail_call),
            SuccMap = JumpOptInfo ^ joi_succ_map,
            map.search(SuccMap, RetLabel, BetweenIncl),
            BetweenIncl = [llds_instr(livevals(_), _), llds_instr(goto(_), _)],
            PrevInstr = livevals(Livevals)
        ->
            NewInstrs = [
                llds_instr(assign(maxfr, lval(prevfr_slot(lval(curfr)))),
                    "discard this frame"),
                llds_instr(assign(succip, lval(succip_slot(lval(curfr)))),
                    "setup PC on return from tailcall"),
                llds_instr(assign(curfr, lval(succfr_slot(lval(curfr)))),
                    "setup curfr on return from tailcall"),
                llds_instr(livevals(Livevals), ""),
                llds_instr(goto(Proc), redirect_comment(Comment0))
            ],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Look for nondet style tailcalls which do need
            % a runtime check.
            CallModel = call_model_nondet(checked_tail_call),
            !.CheckedNondetTailCallInfo = yes(ProcLabel - Counter0),
            SuccMap = JumpOptInfo ^ joi_succ_map,
            map.search(SuccMap, RetLabel, BetweenIncl),
            BetweenIncl = [llds_instr(livevals(_), _), llds_instr(goto(_), _)],
            PrevInstr = livevals(Livevals)
        ->
            counter.allocate(LabelNum, Counter0, Counter1),
            NewLabel = internal_label(LabelNum, ProcLabel),
            NewInstrs = [
                llds_instr(if_val(binop(ne, lval(curfr), lval(maxfr)),
                    code_label(NewLabel)),
                    "branch around if cannot tail call"),
                llds_instr(assign(maxfr, lval(prevfr_slot(lval(curfr)))),
                    "discard this frame"),
                llds_instr(assign(succip, lval(succip_slot(lval(curfr)))),
                    "setup PC on return from tailcall"),
                llds_instr(assign(curfr, lval(succfr_slot(lval(curfr)))),
                    "setup curfr on return from tailcall"),
                llds_instr(livevals(Livevals), ""),
                llds_instr(goto(Proc), redirect_comment(Comment0)),
                llds_instr(label(NewLabel), "non tail call"),
                llds_instr(livevals(Livevals), ""),
                llds_instr(Uinstr0, Comment0)
            ],
            NewRemain = specified(NewInstrs, Instrs0),
            !:CheckedNondetTailCallInfo = yes(ProcLabel - Counter1)
        ;
            % Short circuit the return label if possible.
            InstrMap = JumpOptInfo ^ joi_instr_map,
            map.search(InstrMap, RetLabel, RetInstr)
        ->
            final_dest(InstrMap, RetLabel, DestLabel, RetInstr, _DestInstr),
            ( RetLabel = DestLabel ->
                NewInstrs = [llds_instr(Uinstr0, Comment0)]
            ;
                NewInstrs = [llds_instr(llcall(Proc, code_label(DestLabel),
                    LiveInfos, Context, GoalPath, CallModel),
                    redirect_comment(Comment0))]
            ),
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            NewRemain = usual_case
        )
    ;
        NewRemain = usual_case
    ).

:- pred jump_opt_goto(instr::in(instr_goto), string::in,
    list(instruction)::in, instr::in, jump_opt_info::in,
    maybe(pair(proc_label, counter))::in,
    maybe(pair(proc_label, counter))::out,
    new_remain::out) is det.

jump_opt_goto(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = goto(TargetAddr),
    ( TargetAddr = code_label(TargetLabel) ->
        (
            % Eliminate the goto if possible.
            opt_util.is_this_label_next(TargetLabel, Instrs0, _)
        ->
            NewInstrs = [],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            PrevInstr = if_val(_, code_label(IfTargetLabel)),
            opt_util.is_this_label_next(IfTargetLabel, Instrs0, _)
        ->
            % Eliminating the goto (by the local peephole pass)
            % is better than shortcircuiting it here,
            % PROVIDED the test will succeed most of the time;
            % we could use profiling feedback on this.
            % We cannot eliminate the instruction here because
            % that would require altering the if_val instruction.
            NewInstrs = [llds_instr(Uinstr0, Comment0)],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Replace a jump to a det epilog with the epilog.
            ProcMap = JumpOptInfo ^ joi_proc_map,
            map.search(ProcMap, TargetLabel, Between0)
        ->
            adjust_livevals(PrevInstr, Between0, Between),
            NewInstrs = Between ++
                [llds_instr(goto(code_succip), "shortcircuit")],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Replace a jump to a semidet epilog with the epilog.
            SdprocMap = JumpOptInfo ^ joi_sdproc_map,
            map.search(SdprocMap, TargetLabel, Between0)
        ->
            adjust_livevals(PrevInstr, Between0, Between),
            NewInstrs = Between ++
                [llds_instr(goto(code_succip), "shortcircuit")],
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Replace a jump to a nondet epilog with the epilog.
            SuccMap = JumpOptInfo ^ joi_succ_map,
            map.search(SuccMap, TargetLabel, BetweenIncl0)
        ->
            adjust_livevals(PrevInstr, BetweenIncl0, NewInstrs),
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            % Replace a jump to a non-epilog block with the block itself.
            % These jumps are treated separately from jumps to epilog blocks,
            % for two reasons. First, epilog blocks are always short, so we
            % always want to replace jumps to them, whereas other blocks
            % may be long, so we want to replace jumps to them only if the
            % fulljumps option was given. Second, non-epilog blocks may contain
            % branches to other labels in this procedure, and we want to
            % make sure that these are short-circuited. This short-circuiting
            % is necessary because another optimization below eliminates
            % labels, which is correct only if jumps to those labels are
            % short-circuited everywhere.
            JumpOptInfo ^ joi_full_jump_opt = yes,
            InstrMap = JumpOptInfo ^ joi_instr_map,
            map.search(InstrMap, TargetLabel, TargetInstr),
            final_dest(InstrMap, TargetLabel, DestLabel,
                TargetInstr, _DestInstr),
            BlockMap = JumpOptInfo ^ joi_block_map,
            map.search(BlockMap, DestLabel, Block),
            block_may_be_duplicated(Block) = yes
        ->
            opt_util.filter_out_labels(Block, FilteredBlock),
            adjust_livevals(PrevInstr, FilteredBlock, AdjustedBlock),
            % Block may end with a goto to DestLabel. We avoid infinite
            % expansion in such cases by removing DestLabel from BlockMap,
            % though only while processing AdjustedBlock.
            map.delete(DestLabel, BlockMap, CrippledBlockMap),
            CrippledJumpOptInfo = JumpOptInfo ^ joi_block_map :=
                CrippledBlockMap,
            jump_opt_instr_list(AdjustedBlock, comment(""),
                CrippledJumpOptInfo, !CheckedNondetTailCallInfo,
                [], RevNewInstrs),
            NewRemain = specified(list.reverse(RevNewInstrs), Instrs0)
        ;
            % Short-circuit the goto.
            InstrMap = JumpOptInfo ^ joi_instr_map,
            map.search(InstrMap, TargetLabel, TargetInstr)
        ->
            final_dest(InstrMap, TargetLabel, DestLabel,
                TargetInstr, DestInstr),
            DestInstr = llds_instr(UdestInstr, _Destcomment),
            Shorted = "shortcircuited jump: " ++ Comment0,
            opt_util.can_instr_fall_through(UdestInstr) = Canfallthrough,
            (
                Canfallthrough = no,
                NewInstrs0 = [llds_instr(UdestInstr, Shorted)]
            ;
                Canfallthrough = yes,
                ( TargetLabel = DestLabel ->
                    NewInstrs0 = [llds_instr(Uinstr0, Comment0)]
                ;
                    NewInstrs0 =
                        [llds_instr(goto(code_label(DestLabel)), Shorted)]
                )
            ),
            LvalMap = JumpOptInfo ^ joi_lval_map,
            ( map.search(LvalMap, DestLabel, yes(Lvalinstr)) ->
                adjust_livevals(PrevInstr, [Lvalinstr | NewInstrs0], NewInstrs)
            ;
                NewInstrs = NewInstrs0
            ),
            NewRemain = specified(NewInstrs, Instrs0)
        ;
            NewRemain = usual_case
        )
    ;
        NewRemain = usual_case
    ).

:- pred jump_opt_if_val(instr::in(instr_if_val), string::in,
    list(instruction)::in, instr::in, jump_opt_info::in,
    maybe(pair(proc_label, counter))::in,
    maybe(pair(proc_label, counter))::out,
    new_remain::out) is det.

jump_opt_if_val(Uinstr0, Comment0, Instrs0, _PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = if_val(Cond, TargetAddr),
    ( TargetAddr = code_label(TargetLabel) ->
        JumpOptInfo = jump_opt_info(InstrMap, BlockMap, _LvalMap,
            _ProcMap, _SdprocMap, _ForkMap, _SuccMap, LayoutLabels,
            Fulljumpopt, _MayAlterRtti),
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
            opt_util.skip_comments(Instrs0, Instrs1),
            Instrs1 = [Instr1 | Instrs2],
            ( Instr1 = llds_instr(label(ElimLabel), _) ->
                not set.member(ElimLabel, LayoutLabels),
                opt_util.skip_comments(Instrs2, Instrs3),
                Instrs3 = [GotoInstr | AfterGoto],
                HaveLabel = yes
            ;
                Instr1 = GotoInstr,
                AfterGoto = Instrs2,
                HaveLabel = no
            ),
            GotoInstr = llds_instr(goto(GotoTarget), GotoComment),
            ( HaveLabel = no ; GotoTarget = code_label(_) ),
            opt_util.skip_comments(AfterGoto, AfterGotoComments),
            AfterGotoComments = [LabelInstr | _],
            LabelInstr = llds_instr(label(TargetLabel), _)
        ->
            code_util.neg_rval(Cond, NotCond),
            NewInstr = llds_instr(if_val(NotCond, GotoTarget), GotoComment),
            NewInstrs = [],
            % The transformed code may fit the pattern again,
            % so make sure that we look for the pattern again
            % by giving all of the transformed instructions to
            % the recursive call. We can't go into an infinite
            % loop because each application of the transformation
            % strictly reduces the size of the code.
            RemainInstrs = [NewInstr | AfterGoto],
            NewRemain = specified(NewInstrs, RemainInstrs)
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
            map.search(BlockMap, TargetLabel, _TargetBlock),
            opt_util.skip_comments(Instrs0, Instrs1),
            Instrs1 = [GotoInstr | AfterGoto],
            GotoInstr = llds_instr(goto(GotoAddr), GotoComment),
            \+ (
                GotoAddr = code_label(GotoLabel),
                map.search(BlockMap, GotoLabel, _)
            )
        ->
            code_util.neg_rval(Cond, NotCond),
            NewIfInstr =
                llds_instr(if_val(NotCond, GotoAddr), GotoComment),
            NewInstrs = [NewIfInstr],
            NewGotoComment = Comment0 ++ " (switched)",
            NewGotoInstr =
                llds_instr(goto(code_label(TargetLabel)), NewGotoComment),
            RemainInstrs = [NewGotoInstr | AfterGoto],
            NewRemain = specified(NewInstrs, RemainInstrs)
        ;
            map.search(InstrMap, TargetLabel, TargetInstr)
        ->
            final_dest(InstrMap, TargetLabel, DestLabel,
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
                opt_util.is_sdproceed_next(Instrs0, BetweenFT),
                map.search(BlockMap, DestLabel, Block),
                opt_util.is_sdproceed_next(Block, BetweenBR),
                opt_util.filter_out_r1(BetweenFT, yes(SuccessFT), Between),
                opt_util.filter_out_r1(BetweenBR, yes(SuccessBR), Between),
                (
                    SuccessFT = llconst_true,
                    SuccessBR = llconst_false,
                    code_util.neg_rval(Cond, NewCond)
                ;
                    SuccessFT = llconst_false,
                    SuccessBR = llconst_true,
                    NewCond = Cond
                ),
                \+ needs_workaround(reg(reg_r, 1), NewCond)
            ->
                ( NewCond = lval(reg(reg_r, 1)) ->
                    NewAssign = llds_instr(comment("r1 = old r1"), "")
                ;
                    NewAssign = llds_instr(assign(reg(reg_r, 1), NewCond),
                        "shortcircuit bool computation")
                ),
                Proceed = llds_instr(goto(code_succip), "shortcircuit"),
                NewInstrs = [NewAssign | Between] ++ [Proceed],
                NewRemain = specified(NewInstrs, Instrs0)
            ;
                % Try to short-circuit the destination.
                TargetLabel \= DestLabel
            ->
                Shorted = "shortcircuited jump: " ++ Comment0,
                NewInstrs = [
                    llds_instr(if_val(Cond, code_label(DestLabel)), Shorted)
                ],
                NewRemain = specified(NewInstrs, Instrs0)
            ;
                NewRemain = usual_case
            )
        ;
            NewRemain = usual_case
        )
    ;
        NewRemain = usual_case
    ).

:- pred jump_opt_foreign_proc_code(instr::in(instr_foreign_proc_code),
    string::in, list(instruction)::in, instr::in, jump_opt_info::in,
    maybe(pair(proc_label, counter))::in,
    maybe(pair(proc_label, counter))::out,
    new_remain::out) is det.

jump_opt_foreign_proc_code(Uinstr0, Comment0, Instrs0, _PrevInstr,
        JumpOptInfo, !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = foreign_proc_code(Decls, Components0, MayCallMercury,
        MaybeFixNoLayout, MaybeFixLayout, MaybeFixOnlyLayout,
        MaybeNoFix0, MaybeDefLabel, StackSlotRef, MaybeDup),
    some [!Redirect] (
        InstrMap = JumpOptInfo ^ joi_instr_map,
        list.map_foldl(short_foreign_proc_component(InstrMap),
            Components0, Components, no, !:Redirect),
        (
            MaybeNoFix0 = yes(NoFix),
            short_label(InstrMap, NoFix, NoFixDest),
            MaybeNoFix = yes(NoFixDest),
            !:Redirect = yes
        ;
            MaybeNoFix0 = no,
            MaybeNoFix = no
        ),
% These sanity checks are too strong, because we don't prohibit labels
% appearing these slots of foreign_proc_code instructions from appearing
% in InstrMap; we only prohibit the use of those entries in InstrMap
% to optimize away these labels.
%
%       (
%           MaybeFixNoLayout = yes(FixNoLayout),
%           short_label(InstrMap, FixNoLayout, FixNoLayoutDest),
%           FixNoLayoutDest \= FixNoLayout
%       ->
%           error("jump_opt_instr_list: foreign_proc_code fix_no_layout")
%       ;
%           true
%       ),
%       (
%           MaybeFixLayout = yes(FixLayout),
%           short_label(InstrMap, FixLayout, FixLayoutDest),
%           FixLayoutDest \= FixLayout
%       ->
%           error("jump_opt_instr_list: foreign_proc_code fix_layout")
%       ;
%           true
%       ),
%       (
%           MaybeFixOnlyLayout = yes(FixOnlyLayout),
%           short_label(InstrMap, FixOnlyLayout, FixOnlyLayoutDest),
%           FixOnlyLayoutDest \= FixOnlyLayout
%       ->
%           error("jump_opt_instr_list: foreign_proc_code fix_only_layout")
%       ;
%           true
%       ),
        (
            !.Redirect = no,
            NewRemain = usual_case
        ;
            !.Redirect = yes,
            Comment = Comment0 ++ " (some redirects)",
            Uinstr = foreign_proc_code(Decls, Components, MayCallMercury,
                MaybeFixNoLayout, MaybeFixLayout, MaybeFixOnlyLayout,
                MaybeNoFix, MaybeDefLabel, StackSlotRef, MaybeDup),
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = specified([Instr], Instrs0)
        )
    ).

:- func block_may_be_duplicated(list(instruction)) = bool.

block_may_be_duplicated([]) =  yes.
block_may_be_duplicated([Instr | Instrs]) = BlockMayBeDuplicated :-
    Instr = llds_instr(Uinstr, _),
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
    ( Instr ^ fproc_fix_onlylayout = yes(_) ->
        % This instruction is a trace event. Duplicating it would
        % increase code size, and may cost more in locality than
        % the benefit represented by the elimination of the jump.
        % When debugging is enabled, size is in any case more important
        % than the last bit of speed.
        InstrMayBeDuplicated = no
    ; Instr ^ fproc_maybe_dupl = proc_may_not_duplicate ->
        InstrMayBeDuplicated = no
    ;
        InstrMayBeDuplicated = yes
    ).

:- func redirect_comment(string) = string.

redirect_comment(Comment) = Comment ++ " (redirected return)".

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
    %
:- pred needs_workaround(lval::in, rval::in) is semidet.

needs_workaround(Lval, Cond) :-
    (
        Cond = unop(logical_not, lval(Lval))
    ;
        Cond = binop(Op, Left, Right),
        ( Op = eq ; Op = ne ),
        (
            Right = lval(Lval),
            ( Left = const(llconst_int(0))
            ; Left = mkword(0, unop(mkbody, const(llconst_int(0))))
            )
        ;
            Left = lval(Lval),
            ( Right = const(llconst_int(0))
            ; Right = mkword(0, unop(mkbody, const(llconst_int(0))))
            )
        )
    ).

:- pred adjust_livevals(instr::in, list(instruction)::in,
    list(instruction)::out) is det.

adjust_livevals(PrevInstr, Instrs0, Instrs) :-
    (
        PrevInstr = livevals(PrevLivevals),
        opt_util.skip_comments(Instrs0, Instrs1),
        Instrs1 = [llds_instr(livevals(BetweenLivevals), _) | Instrs2]
    ->
        ( BetweenLivevals = PrevLivevals ->
            Instrs = Instrs2
        ;
            unexpected(this_file,
                "adjust_livevals: BetweenLivevals and PrevLivevals differ")
        )
    ;
        Instrs = Instrs0
    ).

%-----------------------------------------------------------------------------%

    % Short-circuit the given label by following any gotos at the
    % labelled instruction or by falling through consecutive labels.
    %
:- pred short_label(instrmap::in, label::in, label::out) is det.

short_label(InstrMap, Label0, Label) :-
    ( map.search(InstrMap, Label0, Instr0) ->
        final_dest(InstrMap, Label0, Label, Instr0, _Instr)
    ;
        Label = Label0
    ).

:- pred short_maybe_labels(instrmap::in,
    list(maybe(label))::in, list(maybe(label))::out) is det.

short_maybe_labels(_InstrMap, [], []).
short_maybe_labels(InstrMap, [MaybeLabel0 | MaybeLabels0],
        [MaybeLabel | MaybeLabels]) :-
    (
        MaybeLabel0 = yes(Label0),
        short_label(InstrMap, Label0, Label),
        MaybeLabel = yes(Label)
    ;
        MaybeLabel0 = no,
        MaybeLabel = no
    ),
    short_maybe_labels(InstrMap, MaybeLabels0, MaybeLabels).

%-----------------------------------------------------------------------------%

    % Find the final destination of a given instruction at a given label.
    % We follow gotos as well as consecutive labels.
    %
:- pred final_dest(instrmap::in, label::in, label::out, instruction::in,
    instruction::out) is det.

final_dest(InstrMap, SrcLabel, DestLabel, SrcInstr, DestInstr) :-
    final_dest_2(InstrMap, [], SrcLabel, DestLabel, SrcInstr, DestInstr).

:- pred final_dest_2(instrmap::in, list(label)::in,
    label::in, label::out, instruction::in, instruction::out) is det.

final_dest_2(InstrMap, LabelsSofar, SrcLabel, DestLabel,
        SrcInstr, DestInstr) :-
    (
        SrcInstr = llds_instr(SrcUinstr, _Comment),
        (
            SrcUinstr = goto(code_label(TargetLabel))
        ;
            SrcUinstr = label(TargetLabel)
        ),
        map.search(InstrMap, TargetLabel, TargetInstr),
        \+ list.member(SrcLabel, LabelsSofar)
    ->
        final_dest_2(InstrMap, [SrcLabel | LabelsSofar],
            TargetLabel, DestLabel, TargetInstr, DestInstr)
    ;
        DestLabel = SrcLabel,
        DestInstr = SrcInstr
    ).

%-----------------------------------------------------------------------------%

:- pred short_labels_rval(instrmap::in, rval::in, rval::out) is det.

short_labels_rval(InstrMap, lval(Lval0), lval(Lval)) :-
    short_labels_lval(InstrMap, Lval0, Lval).
short_labels_rval(_, var(_), _) :-
    unexpected(this_file, "var rval in short_labels_rval").
short_labels_rval(InstrMap, mkword(Tag, Rval0), mkword(Tag, Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_rval(InstrMap, const(Const0), const(Const)) :-
    short_labels_const(InstrMap, Const0, Const).
short_labels_rval(InstrMap, unop(Op, Rval0), unop(Op, Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_rval(InstrMap, binop(Op, LRval0, RRval0),
        binop(Op, LRval, RRval)) :-
    short_labels_rval(InstrMap, LRval0, LRval),
    short_labels_rval(InstrMap, RRval0, RRval).
short_labels_rval(_, mem_addr(MemRef), mem_addr(MemRef)).

:- pred short_labels_const(instrmap::in,
    rval_const::in, rval_const::out) is det.

short_labels_const(_, llconst_true, llconst_true).
short_labels_const(_, llconst_false, llconst_false).
short_labels_const(_, llconst_int(I), llconst_int(I)).
short_labels_const(_, llconst_foreign(V, T), llconst_foreign(V, T)).
short_labels_const(_, llconst_float(F), llconst_float(F)).
short_labels_const(_, llconst_string(S), llconst_string(S)).
short_labels_const(_, llconst_multi_string(S), llconst_multi_string(S)).
short_labels_const(InstrMap, llconst_code_addr(CodeAddr0),
        llconst_code_addr(CodeAddr)) :-
    ( CodeAddr0 = code_label(Label0) ->
        short_label(InstrMap, Label0, Label),
        CodeAddr = code_label(Label)
    ;
        CodeAddr = CodeAddr0
    ).
short_labels_const(_, llconst_data_addr(D, O),
        llconst_data_addr(D, O)).

:- pred short_labels_maybe_rvals(instrmap::in, list(maybe(rval))::in,
    list(maybe(rval))::out) is det.

short_labels_maybe_rvals(_, [], []).
short_labels_maybe_rvals(InstrMap, [MaybeRval0 | MaybeRvals0],
        [MaybeRval | MaybeRvals]) :-
    short_labels_maybe_rval(InstrMap, MaybeRval0, MaybeRval),
    short_labels_maybe_rvals(InstrMap, MaybeRvals0, MaybeRvals).

:- pred short_labels_maybe_rval(instrmap::in,
    maybe(rval)::in, maybe(rval)::out) is det.

short_labels_maybe_rval(InstrMap, MaybeRval0, MaybeRval) :-
    (
        MaybeRval0 = no,
        MaybeRval = no
    ;
        MaybeRval0 = yes(Rval0),
        short_labels_rval(InstrMap, Rval0, Rval),
        MaybeRval = yes(Rval)
    ).

:- pred short_labels_lval(instrmap::in, lval::in, lval::out) is det.

short_labels_lval(_, reg(T, N), reg(T, N)).
short_labels_lval(_, succip, succip).
short_labels_lval(_, maxfr, maxfr).
short_labels_lval(_, curfr, curfr).
short_labels_lval(_, hp, hp).
short_labels_lval(_, sp, sp).
short_labels_lval(_, parent_sp, parent_sp).
short_labels_lval(_, temp(T, N), temp(T, N)).
short_labels_lval(_, stackvar(N), stackvar(N)).
short_labels_lval(_, parent_stackvar(N), parent_stackvar(N)).
short_labels_lval(_, framevar(N), framevar(N)).
short_labels_lval(_, global_var_ref(Var), global_var_ref(Var)).
short_labels_lval(InstrMap, succip_slot(Rval0), succip_slot(Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_lval(InstrMap, redoip_slot(Rval0), redoip_slot(Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_lval(InstrMap, redofr_slot(Rval0), redofr_slot(Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_lval(InstrMap, succfr_slot(Rval0), succfr_slot(Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_lval(InstrMap, prevfr_slot(Rval0), prevfr_slot(Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_lval(InstrMap, field(Tag, Rval0, Field0),
        field(Tag, Rval, Field)) :-
    short_labels_rval(InstrMap, Rval0, Rval),
    short_labels_rval(InstrMap, Field0, Field).
short_labels_lval(InstrMap, mem_ref(Rval0), mem_ref(Rval)) :-
    short_labels_rval(InstrMap, Rval0, Rval).
short_labels_lval(_, lvar(_), _) :-
    unexpected(this_file, "lvar lval in short_labels_lval").

:- pred short_foreign_proc_component(instrmap::in,
    foreign_proc_component::in, foreign_proc_component::out,
    bool::in, bool::out) is det.

short_foreign_proc_component(InstrMap, !Component, !Redirect) :-
    (
        !.Component = foreign_proc_inputs(_)
    ;
        !.Component = foreign_proc_outputs(_)
    ;
        !.Component = foreign_proc_user_code(_, _, _)
    ;
        !.Component = foreign_proc_raw_code(_, _, _, _)
    ;
        !.Component = foreign_proc_fail_to(Label0),
        short_label(InstrMap, Label0, Label),
        !:Component = foreign_proc_fail_to(Label),
        ( Label = Label0 ->
            true
        ;
            !:Redirect = yes
        )
    ;
        !.Component = foreign_proc_alloc_id(_)
    ;
        !.Component = foreign_proc_noop
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "jumpopt.m".

%-----------------------------------------------------------------------------%
:- end_module jumpopt.
%-----------------------------------------------------------------------------%
