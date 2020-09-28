%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2007, 2009-2012 The University of Melbourne.
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

:- import_module libs.
:- import_module libs.optimization_options.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module set_tree234.

%-----------------------------------------------------------------------------%

    % optimize_jumps_in_proc(LayoutLabels, MayAlterRtti, ProcLabel,
    %   Fulljumpopt, Recjump, PessimizeTailCalls, CheckedNondetTailCall,
    %   !LabelNumCounter, !Instrs, Mod):
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
:- pred optimize_jumps_in_proc(set_tree234(label)::in, may_alter_rtti::in,
    proc_label::in, maybe_opt_fulljumps::in, bool::in,
    maybe_pessimize_tailcalls::in, maybe_opt_checked_nondet_tailcalls::in,
    counter::in, counter::out, list(instruction)::in, list(instruction)::out,
    bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_util.
:- import_module ll_backend.opt_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module map.
:- import_module maybe.
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
%               test r1, and to execute the success or failure epilog
%               depending on the result) to the epilog.
%
% BlockMap will not contain the initial block of the procedure unless
% Recjump is set. The intention is that Recjump will not be set until
% frameopt, which can do a better job of optimizing this block, have
% been applied.

optimize_jumps_in_proc(LayoutLabels, MayAlterRtti, ProcLabel, Fulljumpopt,
        Recjump, PessimizeTailCalls, CheckedNondetTailCall, !LabelNumCounter,
        !Instrs, Mod) :-
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
            PessimizeTailCalls = do_not_pessimize_tailcalls
        ;
            PessimizeTailCalls = pessimize_tailcalls,
            !:ProcMap = map.init,
            !:SdprocMap = map.init,
            !:SuccMap = map.init,
            !:ForkMap = map.init
        ),
        JumpOptInfo = jump_opt_info(!.InstrMap, !.BlockMap, !.LvalMap,
            !.ProcMap, !.SdprocMap, !.ForkMap, !.SuccMap, LayoutLabels,
            Fulljumpopt, MayAlterRtti),
        (
            CheckedNondetTailCall = opt_checked_nondet_tailcalls,
            CheckedNondetTailCallInfo0 =
                check_nondet_tailcalls(ProcLabel, !.LabelNumCounter),
            jump_opt_instr_list(!.Instrs, comment(""), JumpOptInfo,
                CheckedNondetTailCallInfo0, CheckedNondetTailCallInfo,
                [], RevInstrs),
            (
                CheckedNondetTailCallInfo =
                    check_nondet_tailcalls(_, !:LabelNumCounter)
            ;
                CheckedNondetTailCallInfo = dont_check_nondet_tailcalls,
                unexpected($pred, "lost the next label number")
            )
        ;
            CheckedNondetTailCall = do_not_opt_checked_nondet_tailcalls,
            CheckedNondetTailCallInfo0 = dont_check_nondet_tailcalls,
            jump_opt_instr_list(!.Instrs, comment(""), JumpOptInfo,
                CheckedNondetTailCallInfo0, _, [], RevInstrs)
        ),
        list.reverse(RevInstrs, !:Instrs),
        opt_util.filter_out_bad_livevals(!Instrs),
        ( if !.Instrs = Instrs0 then
            Mod = no
        else
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
    ( if Uinstr0 = label(Label) then
        opt_util.skip_comments(Instrs0, Instrs1),
        ( if Instrs1 = [Instr1 | _], Instr1 = llds_instr(livevals(_), _) then
            map.det_insert(Label, yes(Instr1), !LvalMap)
        else
            map.det_insert(Label, no, !LvalMap)
        ),
        opt_util.skip_comments_livevals(Instrs1, Instrs2),
        ( if Instrs2 = [Instr2 | _] then
            map.det_insert(Label, Instr2, !InstrMap)
        else
            true
        ),
        ( if opt_util.is_proceed_next(Instrs1, Between1) then
            map.det_insert(Label, Between1, !ProcMap)
        else
            true
        ),
        ( if opt_util.is_sdproceed_next(Instrs1, Between2) then
            map.det_insert(Label, Between2, !SdprocMap)
        else
            true
        ),
        ( if opt_util.is_succeed_next(Instrs1, Between3) then
            map.det_insert(Label, Between3, !SuccMap)
        else
            true
        ),
        % Put the start of the procedure into Blockmap only after
        % frameopt has had a shot at it.
        ( if
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
        then
            opt_util.find_no_fallthrough(Instrs1, Block),
            map.det_insert(Label, Block, !BlockMap)
        else
            true
        )
    else
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
    ( if
        Uinstr = label(Label),
        opt_util.is_forkproceed_next(Instrs, SdprocMap, Between)
    then
        map.det_insert(Label, Between, !ForkMap)
    else
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
                joi_layout_labels   :: set_tree234(label),
                joi_full_jump_opt   :: maybe_opt_fulljumps,
                joi_may_alter_rtti  :: may_alter_rtti
            ).

:- type new_remain
    --->    nr_specified(
                new_instructions        :: list(instruction),
                remaining_instructions  :: list(instruction)
            )
    ;       nr_usual_case.
            % The list of new instructions contains just Instr0, and
            % the list of remaining instructions, on which to recurse,
            % Instrs0.

:- type maybe_check_nondet_tailcalls
    --->    dont_check_nondet_tailcalls
    ;       check_nondet_tailcalls(proc_label, counter).

    % Optimize the given instruction list by eliminating unnecessary jumps.
    %
    % We handle calls by attempting to turn them into tailcalls. If this fails,
    % we try to short-circuit the return address.
    %
    % We handle gotos by first trying to eliminate them. If this fails,
    % we check whether their target label begins a proceed/succeed sequence;
    % if it does, we replace the label by that sequence. If this fails as well,
    % we check whether the instruction at the ultimate target label can
    % fall through. If it cannot (e.g. call), we replace the goto with
    % this instruction.
    %
    % We handle computed gotos by attempting to short-circuit all the labels
    % in the label list.
    %
    % We handle if-vals by trying to turn them into the assignment
    % of a boolean value to r1, or by short-circuiting the target label.
    % We also try to eliminate a goto following an if-val, if we can do so
    % by negating the condition and possibly also deleting a label between
    % the if-val and the goto.
    %
    % We build up the generated instruction list in reverse order, because
    % building it in right order would make instr_list not tail recursive,
    % and thus unable to handle very long instruction lists.
    %
:- pred jump_opt_instr_list(list(instruction)::in, instr::in,
    jump_opt_info::in,
    maybe_check_nondet_tailcalls::in, maybe_check_nondet_tailcalls::out,
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
        short_circuit_maybe_labels(InstrMap, MaybeTargets0, MaybeTargets),
        ( if MaybeTargets = MaybeTargets0 then
            NewRemain = nr_usual_case
        else
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs =
                [llds_instr(computed_goto(Index, MaybeTargets), Shorted)],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        )
    ;
        Uinstr0 = if_val(_, _),
        jump_opt_if_val(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
            !CheckedNondetTailCallInfo, NewRemain)
    ;
        Uinstr0 = assign(Lval, Rval0),
        % Any labels mentioned in Rval0 should be short-circuited.
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        ( if Rval = Rval0 then
            NewRemain = nr_usual_case
        else
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs = [llds_instr(assign(Lval, Rval), Shorted)],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        )
    ;
        Uinstr0 = keep_assign(Lval, Rval0),
        % Any labels mentioned in Rval0 should be short-circuited.
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        ( if Rval = Rval0 then
            NewRemain = nr_usual_case
        else
            Shorted = Comment0 ++ " (some shortcircuits)",
            NewInstrs = [llds_instr(keep_assign(Lval, Rval), Shorted)],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        )
    ;
        Uinstr0 = mkframe(FrameInfo, Redoip),
        ( if Redoip = yes(code_label(Label0)) then
            InstrMap = JumpOptInfo ^ joi_instr_map,
            short_circuit_label(InstrMap, Label0, Label),
            ( if Label = Label0 then
                NewRemain = nr_usual_case
            else
                Shorted = Comment0 ++ " (some shortcircuits)",
                NewInstrs =
                    [llds_instr(mkframe(FrameInfo, yes(code_label(Label))),
                        Shorted)],
                NewRemain = nr_specified(NewInstrs, Instrs0)
            )
        else
            NewRemain = nr_usual_case
        )
    ;
        Uinstr0 = foreign_proc_code(_, _, _, _, _, _, _, _, _, _),
        jump_opt_foreign_proc_code(Uinstr0, Comment0, Instrs0, PrevInstr,
            JumpOptInfo, !CheckedNondetTailCallInfo, NewRemain)
    ;
        Uinstr0 = block(_, _, _),
        % These are supposed to be introduced only after jumpopt is run
        % for the last time.
        unexpected($pred, "block")
    ;
        Uinstr0 = fork_new_child(SyncTerm, Child0),
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_circuit_label(InstrMap, Child0, Child),
        ( if Child = Child0 then
            NewRemain = nr_usual_case
        else
            Uinstr = fork_new_child(SyncTerm, Child),
            Comment = Comment0 ++ " (redirect)",
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = nr_specified([Instr], Instrs0)
        )
    ;
        Uinstr0 = join_and_continue(SyncTerm, Label0),
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_circuit_label(InstrMap, Label0, Label),
        ( if Label = Label0 then
            NewRemain = nr_usual_case
        else
            Uinstr = join_and_continue(SyncTerm, Label),
            Comment = Comment0 ++ " (redirect)",
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = nr_specified([Instr], Instrs0)
        )
    ;
        Uinstr0 = lc_wait_free_slot(_, _, _),
        % The label in the third argument should not be referred to
        % from any code in the procedure's LLDS instruction sequence,
        % so there is no way for it to be short circuited.
        NewRemain = nr_usual_case
    ;
        Uinstr0 = lc_spawn_off(LCRval, LCSRval, Child0),
        InstrMap = JumpOptInfo ^ joi_instr_map,
        short_circuit_label(InstrMap, Child0, Child),
        ( if Child = Child0 then
            NewRemain = nr_usual_case
        else
            Uinstr = lc_spawn_off(LCRval, LCSRval, Child),
            Comment = Comment0 ++ " (redirect)",
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = nr_specified([Instr], Instrs0)
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
        ; Uinstr0 = lc_create_loop_control(_, _)
        ; Uinstr0 = lc_join_and_terminate(_, _)
        ),
        NewRemain = nr_usual_case
    ),
    (
        NewRemain = nr_usual_case,
        ReplacementInstrsEmpty = no,
        RecurseInstrs = Instrs0,
        !:RevInstrs = [Instr0 | !.RevInstrs]
    ;
        NewRemain = nr_specified(ReplacementInstrs, RecurseInstrs),
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
    ( if
        ( Uinstr0 = comment(_)
        ; ReplacementInstrsEmpty = yes
        )
    then
        NewPrevInstr = PrevInstr
    else
        NewPrevInstr = Uinstr0
    ),
    jump_opt_instr_list(RecurseInstrs, NewPrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, !RevInstrs).

:- pred jump_opt_llcall(instr::in(instr_llcall), string::in,
    list(instruction)::in, instr::in, jump_opt_info::in,
    maybe_check_nondet_tailcalls::in, maybe_check_nondet_tailcalls::out,
    new_remain::out) is det.

jump_opt_llcall(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = llcall(Proc, RetAddr, LiveInfos, Context, GoalPath, CallModel),
    ( if RetAddr = code_label(RetLabel) then
        ( if
            (
                JumpOptInfo ^ joi_may_alter_rtti = must_not_alter_rtti
            ;
                LayoutLabels = JumpOptInfo ^ joi_layout_labels,
                set_tree234.member(RetLabel, LayoutLabels)
            )
        then
            % We cannot optimize the call. Test for this once, here, instead
            % of at the end of each of the following conditions.
            NewRemain = nr_usual_case
        else if
            % Look for det style tailcalls. We look for this even if
            % the call is semidet, because one of the optimizations below
            % turns a pair of semidet epilogs into a det epilog.
            ( CallModel = call_model_det(allow_lco)
            ; CallModel = call_model_semidet(allow_lco)
            ),
            ProcMap = JumpOptInfo ^ joi_proc_map,
            map.search(ProcMap, RetLabel, Between0),
            PrevInstr = livevals(Livevals)
        then
            opt_util.filter_out_livevals(Between0, Between1),
            NewInstrs = Between1 ++
                [llds_instr(livevals(Livevals), ""),
                llds_instr(goto(Proc), redirect_comment(Comment0))],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            % Look for semidet style tailcalls.
            CallModel = call_model_semidet(allow_lco),
            ForkMap = JumpOptInfo ^ joi_fork_map,
            map.search(ForkMap, RetLabel, Between),
            PrevInstr = livevals(Livevals)
        then
            NewInstrs = Between ++
                [llds_instr(livevals(Livevals), ""),
                llds_instr(goto(Proc), redirect_comment(Comment0))],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            % Look for nondet style tailcalls which do not need
            % a runtime check.
            CallModel = call_model_nondet(unchecked_tail_call),
            SuccMap = JumpOptInfo ^ joi_succ_map,
            map.search(SuccMap, RetLabel, BetweenIncl),
            BetweenIncl = [llds_instr(livevals(_), _), llds_instr(goto(_), _)],
            PrevInstr = livevals(Livevals)
        then
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
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            % Look for nondet style tailcalls which do need
            % a runtime check.
            CallModel = call_model_nondet(checked_tail_call),
            !.CheckedNondetTailCallInfo =
                check_nondet_tailcalls(ProcLabel, LabelNumCounter0),
            SuccMap = JumpOptInfo ^ joi_succ_map,
            map.search(SuccMap, RetLabel, BetweenIncl),
            BetweenIncl = [llds_instr(livevals(_), _), llds_instr(goto(_), _)],
            PrevInstr = livevals(Livevals)
        then
            counter.allocate(LabelNum, LabelNumCounter0, LabelNumCounter1),
            NewLabel = internal_label(LabelNum, ProcLabel),
            NewInstrs = [
                llds_instr(if_val(binop(
                    ne(int_type_int), lval(curfr), lval(maxfr)),
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
            NewRemain = nr_specified(NewInstrs, Instrs0),
            !:CheckedNondetTailCallInfo =
                check_nondet_tailcalls(ProcLabel, LabelNumCounter1)
        else if
            % Short circuit the return label if possible.
            InstrMap = JumpOptInfo ^ joi_instr_map,
            map.search(InstrMap, RetLabel, RetInstr)
        then
            final_dest(InstrMap, RetLabel, DestLabel, RetInstr, _DestInstr),
            ( if RetLabel = DestLabel then
                NewInstrs = [llds_instr(Uinstr0, Comment0)]
            else
                NewInstrs = [llds_instr(llcall(Proc, code_label(DestLabel),
                    LiveInfos, Context, GoalPath, CallModel),
                    redirect_comment(Comment0))]
            ),
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else
            NewRemain = nr_usual_case
        )
    else
        NewRemain = nr_usual_case
    ).

:- pred jump_opt_goto(instr::in(instr_goto), string::in,
    list(instruction)::in, instr::in, jump_opt_info::in,
    maybe_check_nondet_tailcalls::in, maybe_check_nondet_tailcalls::out,
    new_remain::out) is det.

jump_opt_goto(Uinstr0, Comment0, Instrs0, PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = goto(TargetAddr),
    ( if TargetAddr = code_label(TargetLabel) then
        ( if
            % Eliminate the goto if possible.
            opt_util.is_this_label_next(TargetLabel, Instrs0, _)
        then
            NewInstrs = [],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            PrevInstr = if_val(_, code_label(IfTargetLabel)),
            opt_util.is_this_label_next(IfTargetLabel, Instrs0, _)
        then
            % Eliminating the goto (by the local peephole pass)
            % is better than shortcircuiting it here,
            % PROVIDED the test will succeed most of the time;
            % we could use profiling feedback on this.
            % We cannot eliminate the instruction here because
            % that would require altering the if_val instruction.
            NewInstrs = [llds_instr(Uinstr0, Comment0)],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            % Replace a jump to a det epilog with the epilog.
            ProcMap = JumpOptInfo ^ joi_proc_map,
            map.search(ProcMap, TargetLabel, Between0)
        then
            adjust_livevals(PrevInstr, Between0, Between),
            NewInstrs = Between ++
                [llds_instr(goto(code_succip), "shortcircuit")],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            % Replace a jump to a semidet epilog with the epilog.
            SdprocMap = JumpOptInfo ^ joi_sdproc_map,
            map.search(SdprocMap, TargetLabel, Between0)
        then
            adjust_livevals(PrevInstr, Between0, Between),
            NewInstrs = Between ++
                [llds_instr(goto(code_succip), "shortcircuit")],
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
            % Replace a jump to a nondet epilog with the epilog.
            SuccMap = JumpOptInfo ^ joi_succ_map,
            map.search(SuccMap, TargetLabel, BetweenIncl0)
        then
            adjust_livevals(PrevInstr, BetweenIncl0, NewInstrs),
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else if
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
            JumpOptInfo ^ joi_full_jump_opt = opt_fulljumps,
            InstrMap = JumpOptInfo ^ joi_instr_map,
            map.search(InstrMap, TargetLabel, TargetInstr),
            final_dest(InstrMap, TargetLabel, DestLabel,
                TargetInstr, _DestInstr),
            BlockMap = JumpOptInfo ^ joi_block_map,
            map.search(BlockMap, DestLabel, Block),
            block_may_be_duplicated(Block) = yes
        then
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
            NewRemain = nr_specified(list.reverse(RevNewInstrs), Instrs0)
        else if
            % Short-circuit the goto.
            InstrMap = JumpOptInfo ^ joi_instr_map,
            map.search(InstrMap, TargetLabel, TargetInstr)
        then
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
                ( if TargetLabel = DestLabel then
                    NewInstrs0 = [llds_instr(Uinstr0, Comment0)]
                else
                    NewInstrs0 =
                        [llds_instr(goto(code_label(DestLabel)), Shorted)]
                )
            ),
            LvalMap = JumpOptInfo ^ joi_lval_map,
            ( if map.search(LvalMap, DestLabel, yes(Lvalinstr)) then
                adjust_livevals(PrevInstr, [Lvalinstr | NewInstrs0], NewInstrs)
            else
                NewInstrs = NewInstrs0
            ),
            NewRemain = nr_specified(NewInstrs, Instrs0)
        else
            NewRemain = nr_usual_case
        )
    else
        NewRemain = nr_usual_case
    ).

:- pred jump_opt_if_val(instr::in(instr_if_val), string::in,
    list(instruction)::in, instr::in, jump_opt_info::in,
    maybe_check_nondet_tailcalls::in, maybe_check_nondet_tailcalls::out,
    new_remain::out) is det.

jump_opt_if_val(Uinstr0, Comment0, Instrs0, _PrevInstr, JumpOptInfo,
        !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = if_val(Cond, TargetAddr),
    ( if TargetAddr = code_label(TargetLabel) then
        JumpOptInfo = jump_opt_info(InstrMap, BlockMap, _LvalMap,
            _ProcMap, _SdprocMap, _ForkMap, _SuccMap, LayoutLabels,
            Fulljumpopt, _MayAlterRtti),
        ( if
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
            % The label L1 may be present or not. If it is present, we are
            % eliminating it, which is possible only because we short-circuit
            % all jumps to it (make them jump directly to L3). This may not be
            % possible if L3 is a non-label code address; e.g. we cannot jump
            % to non-label code addresses from computed gotos.
            opt_util.skip_comments(Instrs0, Instrs1),
            Instrs1 = [Instr1 | Instrs2],
            ( if Instr1 = llds_instr(label(ElimLabel), _) then
                not set_tree234.member(ElimLabel, LayoutLabels),
                opt_util.skip_comments(Instrs2, Instrs3),
                Instrs3 = [GotoInstr | AfterGoto],
                HaveLabel = yes
            else
                Instr1 = GotoInstr,
                AfterGoto = Instrs2,
                HaveLabel = no
            ),
            GotoInstr = llds_instr(goto(GotoTarget), GotoComment),
            ( HaveLabel = no ; GotoTarget = code_label(_) ),
            opt_util.skip_comments(AfterGoto, AfterGotoComments),
            AfterGotoComments = [LabelInstr | _],
            LabelInstr = llds_instr(label(TargetLabel), _)
        then
            code_util.neg_rval(Cond, NotCond),
            NewInstr = llds_instr(if_val(NotCond, GotoTarget), GotoComment),
            NewInstrs = [],
            % The transformed code may fit the pattern again, so make sure that
            % we look for the pattern again by giving all of the transformed
            % instructions to the recursive call. We can't go into an infinite
            % loop because each application of the transformation strictly
            % reduces the size of the code.
            RemainInstrs = [NewInstr | AfterGoto],
            NewRemain = nr_specified(NewInstrs, RemainInstrs)
        else if
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
            % and get the code processed again starting after the if_val,
            % to get the recursive call to replace the goto to L1
            % with the code at L1.
            Fulljumpopt = opt_fulljumps,
            map.search(BlockMap, TargetLabel, _TargetBlock),
            opt_util.skip_comments(Instrs0, Instrs1),
            Instrs1 = [GotoInstr | AfterGoto],
            GotoInstr = llds_instr(goto(GotoAddr), GotoComment),
            not (
                GotoAddr = code_label(GotoLabel),
                map.search(BlockMap, GotoLabel, _)
            )
        then
            code_util.neg_rval(Cond, NotCond),
            NewIfInstr = llds_instr(if_val(NotCond, GotoAddr), GotoComment),
            NewInstrs = [NewIfInstr],
            NewGotoComment = Comment0 ++ " (switched)",
            NewGotoInstr =
                llds_instr(goto(code_label(TargetLabel)), NewGotoComment),
            RemainInstrs = [NewGotoInstr | AfterGoto],
            NewRemain = nr_specified(NewInstrs, RemainInstrs)
        else if
            map.search(InstrMap, TargetLabel, TargetInstr)
        then
            final_dest(InstrMap, TargetLabel, DestLabel,
                TargetInstr, _DestInstr),
            ( if
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
                not needs_workaround(reg(reg_r, 1), NewCond)
            then
                ( if NewCond = lval(reg(reg_r, 1)) then
                    NewAssign = llds_instr(comment("r1 = old r1"), "")
                else
                    NewAssign = llds_instr(assign(reg(reg_r, 1), NewCond),
                        "shortcircuit bool computation")
                ),
                Proceed = llds_instr(goto(code_succip), "shortcircuit"),
                NewInstrs = [NewAssign | Between] ++ [Proceed],
                NewRemain = nr_specified(NewInstrs, Instrs0)
            else if
                % Try to short-circuit the destination.
                TargetLabel \= DestLabel
            then
                Shorted = "shortcircuited jump: " ++ Comment0,
                NewInstrs = [
                    llds_instr(if_val(Cond, code_label(DestLabel)), Shorted)
                ],
                NewRemain = nr_specified(NewInstrs, Instrs0)
            else
                NewRemain = nr_usual_case
            )
        else
            NewRemain = nr_usual_case
        )
    else
        NewRemain = nr_usual_case
    ).

:- pred jump_opt_foreign_proc_code(instr::in(instr_foreign_proc_code),
    string::in, list(instruction)::in, instr::in, jump_opt_info::in,
    maybe_check_nondet_tailcalls::in, maybe_check_nondet_tailcalls::out,
    new_remain::out) is det.

jump_opt_foreign_proc_code(Uinstr0, Comment0, Instrs0, _PrevInstr,
        JumpOptInfo, !CheckedNondetTailCallInfo, NewRemain) :-
    Uinstr0 = foreign_proc_code(Decls, Components0, MayCallMercury,
        MaybeFixNoLayout, MaybeFixLayout, MaybeFixOnlyLayout,
        MaybeNoFix0, MaybeDefLabel, StackSlotRef, MaybeDup),
    some [!Redirect] (
        InstrMap = JumpOptInfo ^ joi_instr_map,
        list.map_foldl(short_circuit_foreign_proc_component(InstrMap),
            Components0, Components, no, !:Redirect),
        (
            MaybeNoFix0 = yes(NoFix),
            short_circuit_label(InstrMap, NoFix, NoFixDest),
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
%       ( if
%           MaybeFixNoLayout = yes(FixNoLayout),
%           short_circuit_label(InstrMap, FixNoLayout, FixNoLayoutDest),
%           FixNoLayoutDest \= FixNoLayout
%       then
%           error("jump_opt_instr_list: foreign_proc_code fix_no_layout")
%       else
%           true
%       ),
%       ( if
%           MaybeFixLayout = yes(FixLayout),
%           short_circuit_label(InstrMap, FixLayout, FixLayoutDest),
%           FixLayoutDest \= FixLayout
%       then
%           error("jump_opt_instr_list: foreign_proc_code fix_layout")
%       else
%           true
%       ),
%       ( if
%           MaybeFixOnlyLayout = yes(FixOnlyLayout),
%           short_circuit_label(InstrMap, FixOnlyLayout, FixOnlyLayoutDest),
%           FixOnlyLayoutDest \= FixOnlyLayout
%       then
%           error("jump_opt_instr_list: foreign_proc_code fix_only_layout")
%       else
%           true
%       ),
        (
            !.Redirect = no,
            NewRemain = nr_usual_case
        ;
            !.Redirect = yes,
            Comment = Comment0 ++ " (some redirects)",
            Uinstr = foreign_proc_code(Decls, Components, MayCallMercury,
                MaybeFixNoLayout, MaybeFixLayout, MaybeFixOnlyLayout,
                MaybeNoFix, MaybeDefLabel, StackSlotRef, MaybeDup),
            Instr = llds_instr(Uinstr, Comment),
            NewRemain = nr_specified([Instr], Instrs0)
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
    ( if Instr ^ fproc_fix_onlylayout = yes(_) then
        % This instruction is a trace event. Duplicating it would
        % increase code size, and may cost more in locality than
        % the benefit represented by the elimination of the jump.
        % When debugging is enabled, size is in any case more important
        % than the last bit of speed.
        InstrMayBeDuplicated = no
    else if Instr ^ fproc_maybe_dupl = proc_may_not_duplicate then
        InstrMayBeDuplicated = no
    else
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
        ( Op = eq(_) ; Op = ne(_) ),
        (
            Right = lval(Lval),
            ( Left = const(llconst_int(0))
            ; Left = mkword(ptag(0u8), unop(mkbody, const(llconst_int(0))))
            )
        ;
            Left = lval(Lval),
            ( Right = const(llconst_int(0))
            ; Right = mkword(ptag(0u8), unop(mkbody, const(llconst_int(0))))
            )
        )
    ).

:- pred adjust_livevals(instr::in, list(instruction)::in,
    list(instruction)::out) is det.

adjust_livevals(PrevInstr, Instrs0, Instrs) :-
    ( if
        PrevInstr = livevals(PrevLivevals),
        opt_util.skip_comments(Instrs0, Instrs1),
        Instrs1 = [llds_instr(livevals(BetweenLivevals), _) | Instrs2]
    then
        ( if BetweenLivevals = PrevLivevals then
            Instrs = Instrs2
        else
            unexpected($pred, "BetweenLivevals and PrevLivevals differ")
        )
    else
        Instrs = Instrs0
    ).

%-----------------------------------------------------------------------------%

    % Short-circuit the given label by following any gotos at the
    % labelled instruction or by falling through consecutive labels.
    %
:- pred short_circuit_label(instrmap::in, label::in, label::out) is det.

short_circuit_label(InstrMap, Label0, Label) :-
    ( if map.search(InstrMap, Label0, Instr0) then
        final_dest(InstrMap, Label0, Label, Instr0, _Instr)
    else
        Label = Label0
    ).

    % Find the final destination of a given instruction at a given label.
    % We follow gotos as well as consecutive labels.
    %
:- pred final_dest(instrmap::in, label::in, label::out, instruction::in,
    instruction::out) is det.

final_dest(InstrMap, SrcLabel, DestLabel, SrcInstr, DestInstr) :-
    final_dest_loop(InstrMap, [], SrcLabel, DestLabel, SrcInstr, DestInstr).

:- pred final_dest_loop(instrmap::in, list(label)::in,
    label::in, label::out, instruction::in, instruction::out) is det.

final_dest_loop(InstrMap, LabelsSofar, SrcLabel, DestLabel,
        SrcInstr, DestInstr) :-
    ( if
        SrcInstr = llds_instr(SrcUinstr, _Comment),
        (
            SrcUinstr = goto(code_label(TargetLabel))
        ;
            SrcUinstr = label(TargetLabel)
        ),
        map.search(InstrMap, TargetLabel, TargetInstr),
        not list.member(SrcLabel, LabelsSofar)
    then
        final_dest_loop(InstrMap, [SrcLabel | LabelsSofar],
            TargetLabel, DestLabel, TargetInstr, DestInstr)
    else
        DestLabel = SrcLabel,
        DestInstr = SrcInstr
    ).

%-----------------------------------------------------------------------------%

:- pred short_circuit_maybe_labels(instrmap::in,
    list(maybe(label))::in, list(maybe(label))::out) is det.

short_circuit_maybe_labels(_InstrMap, [], []).
short_circuit_maybe_labels(InstrMap, [MaybeLabel0 | MaybeLabels0],
        [MaybeLabel | MaybeLabels]) :-
    (
        MaybeLabel0 = yes(Label0),
        short_circuit_label(InstrMap, Label0, Label),
        MaybeLabel = yes(Label)
    ;
        MaybeLabel0 = no,
        MaybeLabel = no
    ),
    short_circuit_maybe_labels(InstrMap, MaybeLabels0, MaybeLabels).

%-----------------------------------------------------------------------------%

:- pred short_circuit_labels_rval(instrmap::in, rval::in, rval::out) is det.

short_circuit_labels_rval(InstrMap, Rval0, Rval) :-
    (
        Rval0 = lval(Lval0),
        short_circuit_labels_lval(InstrMap, Lval0, Lval),
        Rval = lval(Lval)
    ;
        Rval0 = var(_),
        unexpected($pred, "var")
    ;
        Rval0 = mkword(Tag, SubRval0),
        short_circuit_labels_rval(InstrMap, SubRval0, SubRval),
        Rval = mkword(Tag, SubRval)
    ;
        Rval0 = const(Const0),
        short_circuit_labels_const(InstrMap, Const0, Const),
        Rval = const(Const)
    ;
        Rval0 = cast(Type, SubRval0),
        short_circuit_labels_rval(InstrMap, SubRval0, SubRval),
        Rval = cast(Type, SubRval)
    ;
        Rval0 = unop(UnOp, SubRval0),
        short_circuit_labels_rval(InstrMap, SubRval0, SubRval),
        Rval = unop(UnOp, SubRval)
    ;
        Rval0 = binop(BinOp, LRval0, RRval0),
        short_circuit_labels_rval(InstrMap, LRval0, LRval),
        short_circuit_labels_rval(InstrMap, RRval0, RRval),
        Rval = binop(BinOp, LRval, RRval)
    ;
        ( Rval0 = mkword_hole(_)
        ; Rval0 = mem_addr(_)
        ),
        Rval = Rval0
    ).

:- pred short_circuit_labels_const(instrmap::in,
    rval_const::in, rval_const::out) is det.

short_circuit_labels_const(InstrMap, RvalConst0, RvalConst) :-
    (
        ( RvalConst0 = llconst_true
        ; RvalConst0 = llconst_false
        ; RvalConst0 = llconst_int(_I)
        ; RvalConst0 = llconst_uint(_U)
        ; RvalConst0 = llconst_int8(_I8)
        ; RvalConst0 = llconst_uint8(_U8)
        ; RvalConst0 = llconst_int16(_I16)
        ; RvalConst0 = llconst_uint16(_U16)
        ; RvalConst0 = llconst_int32(_I32)
        ; RvalConst0 = llconst_uint32(_U32)
        ; RvalConst0 = llconst_int64(_I64)
        ; RvalConst0 = llconst_uint64(_U64)
        ; RvalConst0 = llconst_foreign(_V, _T)
        ; RvalConst0 = llconst_float(_F)
        ; RvalConst0 = llconst_string(_S)
        ; RvalConst0 = llconst_multi_string(_S)
        ; RvalConst0 = llconst_data_addr(_D, _O)
        ),
        RvalConst = RvalConst0
    ;
        RvalConst0 = llconst_code_addr(CodeAddr0),
        ( if CodeAddr0 = code_label(Label0) then
            short_circuit_label(InstrMap, Label0, Label),
            CodeAddr = code_label(Label)
        else
            CodeAddr = CodeAddr0
        ),
        RvalConst = llconst_code_addr(CodeAddr)
    ).

% Not currently needed.
%
% :- pred short_circuit_labels_maybe_rvals(instrmap::in, list(maybe(rval))::in,
%     list(maybe(rval))::out) is det.
%
% short_circuit_labels_maybe_rvals(_, [], []).
% short_circuit_labels_maybe_rvals(InstrMap, [MaybeRval0 | MaybeRvals0],
%         [MaybeRval | MaybeRvals]) :-
%     short_circuit_labels_maybe_rval(InstrMap, MaybeRval0, MaybeRval),
%     short_circuit_labels_maybe_rvals(InstrMap, MaybeRvals0, MaybeRvals).
% 
% :- pred short_circuit_labels_maybe_rval(instrmap::in,
%     maybe(rval)::in, maybe(rval)::out) is det.
% 
% short_circuit_labels_maybe_rval(InstrMap, MaybeRval0, MaybeRval) :-
%     (
%         MaybeRval0 = no,
%         MaybeRval = no
%     ;
%         MaybeRval0 = yes(Rval0),
%         short_circuit_labels_rval(InstrMap, Rval0, Rval),
%         MaybeRval = yes(Rval)
%     ).

:- pred short_circuit_labels_lval(instrmap::in, lval::in, lval::out) is det.

short_circuit_labels_lval(InstrMap, Lval0, Lval) :-
    (
        ( Lval0 = reg(_T, _N)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = temp(_T, _N)
        ; Lval0 = stackvar(_N)
        ; Lval0 = parent_stackvar(_N)
        ; Lval0 = framevar(_N)
        ; Lval0 = double_stackvar(_Type, _N)
        ; Lval0 = global_var_ref(_Var)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, Rval0, Field0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        short_circuit_labels_rval(InstrMap, Field0, Field),
        Lval = field(Tag, Rval, Field)
    ;
        Lval0 = mem_ref(Rval0),
        short_circuit_labels_rval(InstrMap, Rval0, Rval),
        Lval = mem_ref(Rval)
    ;
        Lval0 = lvar(_),
        unexpected($pred, "lvar")
    ).

:- pred short_circuit_foreign_proc_component(instrmap::in,
    foreign_proc_component::in, foreign_proc_component::out,
    bool::in, bool::out) is det.

short_circuit_foreign_proc_component(InstrMap, !Component, !Redirect) :-
    (
        !.Component = foreign_proc_fail_to(Label0),
        short_circuit_label(InstrMap, Label0, Label),
        !:Component = foreign_proc_fail_to(Label),
        ( if Label = Label0 then
            true
        else
            !:Redirect = yes
        )
    ;
        ( !.Component = foreign_proc_inputs(_)
        ; !.Component = foreign_proc_outputs(_)
        ; !.Component = foreign_proc_user_code(_, _, _)
        ; !.Component = foreign_proc_raw_code(_, _, _, _)
        ; !.Component = foreign_proc_alloc_id(_)
        ; !.Component = foreign_proc_noop
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.jumpopt.
%-----------------------------------------------------------------------------%
