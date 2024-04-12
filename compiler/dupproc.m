%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2013, 2015-2019, 2022, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: dupproc.m.
% Author: zs.
%
% The job of this module is to eliminate duplicate procedure bodies. These
% can arise when a predicate has several modes that differ only in details that
% don't matter for code generation, such as in/out vs di/uo, or (in some cases)
% in/out, vs any/any.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.dupproc.
:- interface.

:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

:- pred eliminate_duplicate_procs(assoc_list(proc_label, c_procedure)::in,
    list(c_procedure)::out,
    map(proc_label, proc_label)::in, map(proc_label, proc_label)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend.opt_util.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

eliminate_duplicate_procs(IdProcs, Procs, !DupProcMap) :-
    (
        IdProcs = [],
        Procs = []
    ;
        IdProcs = [_Id - Proc],
        Procs = [Proc]
    ;
        IdProcs = [Id1 - Proc1 | IdProcsTail],
        IdProcsTail = [_ | _],
        apply_dup_proc_map_in_proc(Proc1, StdProc1, !.DupProcMap),
        eliminate_dup_procs([Id1 - StdProc1], IdProcsTail, FinalIdProcsTail,
            !DupProcMap),
        assoc_list.values(FinalIdProcsTail, FinalProcsTail),
        Procs = [Proc1 | FinalProcsTail]
    ).

:- pred eliminate_dup_procs(assoc_list(proc_label, c_procedure)::in,
    assoc_list(proc_label, c_procedure)::in,
    assoc_list(proc_label, c_procedure)::out,
    map(proc_label, proc_label)::in, map(proc_label, proc_label)::out) is det.

eliminate_dup_procs(_ModelStdProcs, [], [], !DupProcMap).
eliminate_dup_procs(ModelStdProcs0, [Id - Proc0 | IdProcs0],
        [Id - Proc | IdProcs], !DupProcMap) :-
    ( if
        Proc0 ^ cproc_may_alter_rtti = may_alter_rtti,
        find_matching_model_proc(ModelStdProcs0, Id, Proc0, !.DupProcMap,
            MatchingId),
        maybe_redirect_proc(Proc0, MatchingId, MaybeProc),
        MaybeProc = yes(ProcPrime)
    then
        Proc = ProcPrime,
        map.det_insert(Id, MatchingId, !DupProcMap),
        ModelStdProcs = ModelStdProcs0
    else
        Proc = Proc0,
        apply_dup_proc_map_in_proc(Proc0, StdProc0, !.DupProcMap),
        % Since the number of procedures per predicate is tiny,
        % the quadratic behavior here is not a problem.
        list.append(ModelStdProcs0, [Id - StdProc0], ModelStdProcs)
    ),
    eliminate_dup_procs(ModelStdProcs, IdProcs0, IdProcs, !DupProcMap).

:- pred find_matching_model_proc(assoc_list(proc_label, c_procedure)::in,
    proc_label::in, c_procedure::in, map(proc_label, proc_label)::in,
    proc_label::out) is semidet.

find_matching_model_proc([ModelId - ModelStdProc | ModelIdProcs], Id, Proc,
        DupProcMap, MatchingId) :-
    map.det_insert(Id, ModelId, DupProcMap, AugDupProcMap),
    apply_dup_proc_map_in_proc(Proc, StdProc, AugDupProcMap),
    StdInstrs = StdProc ^ cproc_code,
    ModelStdInstrs = ModelStdProc ^ cproc_code,
    ( if StdInstrs = ModelStdInstrs then
        MatchingId = ModelId
    else
        find_matching_model_proc(ModelIdProcs, Id, Proc, DupProcMap,
            MatchingId)
    ).

    % If the body of the matching procedures is significant enough,
    % redirect the current procedure body to jump to the start of the
    % model procedure's body instead. On the other hand, if the body
    % is just a few instructions with no jumps, then this redirection
    % jump would be a greater time cost than the space cost of duplicating
    % the procedure body, so in that case we avoid the transformation
    % by not returning the new body.
    %
:- pred maybe_redirect_proc(c_procedure::in, proc_label::in,
    maybe(c_procedure)::out) is det.

maybe_redirect_proc(Proc0, TargetProcLabel, MaybeProc) :-
    Instrs0 = Proc0 ^ cproc_code,
    get_prologue(Instrs0, LabelInstr, _Comments, LaterInstrs),
    TargetLabel = entry_label(entry_label_local, TargetProcLabel),
    Redirect = llds_instr(goto(code_label(TargetLabel)),
        "Redirect to procedure with identical body"),
    list.filter(disallowed_instr, LaterInstrs, DisallowedInstrs),
    list.length(LaterInstrs, NumLaterInstrs),
    ( if
        DisallowedInstrs = [],
        % The threshold here is a guess. I don't think the precise value
        % has much effect, so I don't think it is worth making it configurable.
        NumLaterInstrs < 6
    then
        MaybeProc = no
    else
        Instrs = [LabelInstr, Redirect],
        Proc = Proc0 ^ cproc_code := Instrs,
        MaybeProc = yes(Proc)
    ).

:- pred disallowed_instr(instruction::in) is semidet.

disallowed_instr(llds_instr(Instr, _)) :-
    (
        Instr = if_val(_, _)
    ;
        Instr = llcall(_, _, _, _, _, _)
    ).

%-----------------------------------------------------------------------------%

:- pred apply_dup_proc_map_in_proc(c_procedure::in, c_procedure::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_proc(CProc, StdCProc, DupProcMap) :-
    Instrs = CProc ^ cproc_code,
    apply_dup_proc_map_in_instrs(Instrs, StdInstrs, DupProcMap),
    StdCProc = CProc ^ cproc_code := StdInstrs.

:- pred apply_dup_proc_map_in_instrs(
    list(instruction)::in, list(instruction)::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_instrs([], [], _).
apply_dup_proc_map_in_instrs([llds_instr(Instr, _) | Instrs],
        [llds_instr(StdInstr, "") | StdInstrs], DupProcMap) :-
    apply_dup_proc_map_in_instr(Instr, StdInstr, DupProcMap),
    apply_dup_proc_map_in_instrs(Instrs, StdInstrs, DupProcMap).

:- pred apply_dup_proc_map_in_instr(instr::in, instr::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_instr(Instr, StdInstr, DupProcMap) :-
    (
        Instr = block(NumIntTemps, NumFloatTemps, Instrs),
        apply_dup_proc_map_in_instrs(Instrs, StdInstrs, DupProcMap),
        StdInstr = block(NumIntTemps, NumFloatTemps, StdInstrs)
    ;
        Instr = assign(Lval, Rval),
        apply_dup_proc_map_in_rval(Rval, StdRval, DupProcMap),
        StdInstr = assign(Lval, StdRval)
    ;
        Instr = keep_assign(Lval, Rval),
        apply_dup_proc_map_in_rval(Rval, StdRval, DupProcMap),
        StdInstr = keep_assign(Lval, StdRval)
    ;
        Instr = llcall(Target, Cont, LiveInfo, Context, GoalPath, Model),
        apply_dup_proc_map_in_code_addr(Target, StdTarget, DupProcMap),
        apply_dup_proc_map_in_code_addr(Cont, StdCont, DupProcMap),
        StdInstr = llcall(StdTarget, StdCont, LiveInfo, Context, GoalPath,
            Model)
    ;
        Instr = mkframe(FrameInfo, MaybeCodeAddr),
        (
            FrameInfo = temp_frame(_),
            StdFrameInfo = FrameInfo
        ;
            FrameInfo = ordinary_frame(_, NumSlots),
            StdFrameInfo = ordinary_frame("", NumSlots)
        ),
        apply_dup_proc_map_in_maybe_code_addr(MaybeCodeAddr, MaybeStdCodeAddr,
            DupProcMap),
        StdInstr = mkframe(StdFrameInfo, MaybeStdCodeAddr)
    ;
        Instr = label(Label),
        apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap),
        StdInstr = label(StdLabel)
    ;
        Instr = goto(Target),
        apply_dup_proc_map_in_code_addr(Target, StdTarget, DupProcMap),
        StdInstr = goto(StdTarget)
    ;
        Instr = computed_goto(Rval, Targets),
        apply_dup_proc_map_in_maybe_labels(Targets, StdTargets, DupProcMap),
        StdInstr = computed_goto(Rval, StdTargets)
    ;
        Instr = if_val(Rval, Target),
        apply_dup_proc_map_in_rval(Rval, StdRval, DupProcMap),
        apply_dup_proc_map_in_code_addr(Target, StdTarget, DupProcMap),
        StdInstr = if_val(StdRval, StdTarget)
    ;
        Instr = incr_sp(NumSlots, _, Kind),
        StdInstr = incr_sp(NumSlots, "", Kind)
    ;
        Instr = fork_new_child(Lval, Child),
        apply_dup_proc_map_in_label(Child, StdChild, DupProcMap),
        StdInstr = fork_new_child(Lval, StdChild)
    ;
        Instr = join_and_continue(Lval, Label),
        apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap),
        StdInstr = join_and_continue(Lval, StdLabel)
    ;
        % The labels occurring in foreign_proc_code instructions
        % cannot be substituted.
        Instr = foreign_proc_code(_, _, _, _, _, _, _, _, _, _),
        StdInstr = Instr
    ;
        Instr = lc_wait_free_slot(Rval, Lval, Label),
        apply_dup_proc_map_in_rval(Rval, StdRval, DupProcMap),
        apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap),
        StdInstr = lc_wait_free_slot(StdRval, Lval, StdLabel)
    ;
        Instr = lc_spawn_off(LCRval, LCSRval, Label),
        apply_dup_proc_map_in_rval(LCRval, StdLCRval, DupProcMap),
        apply_dup_proc_map_in_rval(LCSRval, StdLCSRval, DupProcMap),
        apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap),
        StdInstr = lc_spawn_off(StdLCRval, StdLCSRval, StdLabel)
    ;
        Instr = lc_join_and_terminate(LCRval, LCSRval),
        apply_dup_proc_map_in_rval(LCRval, StdLCRval, DupProcMap),
        apply_dup_proc_map_in_rval(LCSRval, StdLCSRval, DupProcMap),
        StdInstr = lc_join_and_terminate(StdLCRval, StdLCSRval)
    ;
        % These instructions have no labels inside them, or anything else
        % that can be standardized.
        ( Instr = comment(_)
        ; Instr = livevals(_)
        ; Instr = arbitrary_c_code(_, _, _)
        ; Instr = save_maxfr(_)
        ; Instr = restore_maxfr(_)
        ; Instr = incr_hp(_, _, _, _, _, _, _, _)
        ; Instr = mark_hp(_)
        ; Instr = restore_hp(_)
        ; Instr = free_heap(_)
        ; Instr = push_region_frame(_, _)
        ; Instr = region_fill_frame(_, _, _, _, _)
        ; Instr = region_set_fixed_slot(_, _, _)
        ; Instr = use_and_maybe_pop_region_frame(_, _)
        ; Instr = store_ticket(_)
        ; Instr = reset_ticket(_, _)
        ; Instr = discard_ticket
        ; Instr = prune_ticket
        ; Instr = mark_ticket_stack(_)
        ; Instr = prune_tickets_to(_)
        ; Instr = decr_sp(_)
        ; Instr = decr_sp_and_return(_)
        ; Instr = init_sync_term(_, _, _)
        ; Instr = lc_create_loop_control(_, _)
        ),
        StdInstr = Instr
    ).

    % Compute the standard form of a proc_label.
    %
:- pred apply_dup_proc_map_in_proc_label(proc_label::in, proc_label::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_proc_label(ProcLabel, StdProcLabel, DupProcMap) :-
    ( if map.search(DupProcMap, ProcLabel, FoundProcLabel) then
        StdProcLabel = FoundProcLabel
    else
        StdProcLabel = ProcLabel
    ).

    % Compute the standard form of a label.
    %
:- pred apply_dup_proc_map_in_label(label::in, label::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap) :-
    (
        Label = internal_label(Num, ProcLabel),
        apply_dup_proc_map_in_proc_label(ProcLabel, StdProcLabel, DupProcMap),
        StdLabel = internal_label(Num, StdProcLabel)
    ;
        Label = entry_label(Type, ProcLabel),
        apply_dup_proc_map_in_proc_label(ProcLabel, StdProcLabel, DupProcMap),
        StdLabel = entry_label(Type, StdProcLabel)
    ).

    % Compute the standard form of a list(label).
    %
:- pred apply_dup_proc_map_in_maybe_labels(list(maybe(label))::in,
    list(maybe(label))::out, map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_maybe_labels([], [], _DupProcMap).
apply_dup_proc_map_in_maybe_labels([MaybeLabel | MaybeLabels],
        [StdMaybeLabel | StdMaybeLabels], DupProcMap) :-
    (
        MaybeLabel = yes(Label),
        apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap),
        StdMaybeLabel = yes(StdLabel)
    ;
        MaybeLabel = no,
        StdMaybeLabel = no
    ),
    apply_dup_proc_map_in_maybe_labels(MaybeLabels, StdMaybeLabels,
        DupProcMap).

    % Compute the standard form of a code_addr.
    %
:- pred apply_dup_proc_map_in_code_addr(code_addr::in, code_addr::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_code_addr(CodeAddr, StdCodeAddr, DupProcMap) :-
    (
        CodeAddr = code_label(Label),
        apply_dup_proc_map_in_label(Label, StdLabel, DupProcMap),
        StdCodeAddr = code_label(StdLabel)
    ;
        CodeAddr = code_imported_proc(ProcLabel),
        apply_dup_proc_map_in_proc_label(ProcLabel, StdProcLabel, DupProcMap),
        StdCodeAddr = code_imported_proc(StdProcLabel)
    ;
        ( CodeAddr = code_succip
        ; CodeAddr = do_succeed(_)
        ; CodeAddr = do_redo
        ; CodeAddr = do_fail
        ; CodeAddr = do_trace_redo_fail_shallow
        ; CodeAddr = do_trace_redo_fail_deep
        ; CodeAddr = do_call_closure(_)
        ; CodeAddr = do_call_class_method(_)
        ; CodeAddr = do_not_reached
        ),
        StdCodeAddr = CodeAddr
    ).

    % Compute the standard form of a maybe(code_addr).
    %
:- pred apply_dup_proc_map_in_maybe_code_addr(maybe(code_addr)::in,
    maybe(code_addr)::out, map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_maybe_code_addr(MaybeCodeAddr, MaybeStdCodeAddr,
        DupProcMap) :-
    (
        MaybeCodeAddr = no,
        MaybeStdCodeAddr = no
    ;
        MaybeCodeAddr = yes(CodeAddr),
        apply_dup_proc_map_in_code_addr(CodeAddr, StdCodeAddr, DupProcMap),
        MaybeStdCodeAddr = yes(StdCodeAddr)
    ).

    % Compute the standard form of an rval.
    %
:- pred apply_dup_proc_map_in_rval(rval::in, rval::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_rval(Rval, StdRval, DupProcMap) :-
    (
        ( Rval = lval(_)
        ; Rval = mkword(_, _)
        ; Rval = mkword_hole(_)
        ; Rval = mem_addr(_)
        ),
        StdRval = Rval
    ;
        Rval = var(_),
        unexpected($pred, "var")
    ;
        Rval = const(Const),
        apply_dup_proc_map_in_rval_const(Const, StdConst, DupProcMap),
        StdRval = const(StdConst)
    ;
        Rval = cast(Type, RvalL),
        apply_dup_proc_map_in_rval(RvalL, StdRvalL, DupProcMap),
        StdRval = cast(Type, StdRvalL)
    ;
        Rval = unop(Unop, RvalL),
        apply_dup_proc_map_in_rval(RvalL, StdRvalL, DupProcMap),
        StdRval = unop(Unop, StdRvalL)
    ;
        Rval = binop(Binop, RvalL, RvalR),
        apply_dup_proc_map_in_rval(RvalL, StdRvalL, DupProcMap),
        apply_dup_proc_map_in_rval(RvalR, StdRvalR, DupProcMap),
        StdRval = binop(Binop, StdRvalL, StdRvalR)
    ).

    % Compute the standard form of an rval constant.
    %
:- pred apply_dup_proc_map_in_rval_const(rval_const::in, rval_const::out,
    map(proc_label, proc_label)::in) is det.

apply_dup_proc_map_in_rval_const(Const, StdConst, DupProcMap) :-
    (
        ( Const = llconst_true
        ; Const = llconst_false
        ; Const = llconst_int(_)
        ; Const = llconst_uint(_)
        ; Const = llconst_int8(_)
        ; Const = llconst_uint8(_)
        ; Const = llconst_int16(_)
        ; Const = llconst_uint16(_)
        ; Const = llconst_int32(_)
        ; Const = llconst_uint32(_)
        ; Const = llconst_int64(_)
        ; Const = llconst_uint64(_)
        ; Const = llconst_foreign(_, _)
        ; Const = llconst_float(_)
        ; Const = llconst_string(_)
        ; Const = llconst_multi_string(_)
        ; Const = llconst_data_addr(_)
        ; Const = llconst_data_addr_word_offset(_, _)
        ),
        StdConst = Const
    ;
        Const = llconst_code_addr(CodeAddr),
        apply_dup_proc_map_in_code_addr(CodeAddr, StdCodeAddr, DupProcMap),
        StdConst = llconst_code_addr(StdCodeAddr)
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.dupproc.
%-----------------------------------------------------------------------------%
