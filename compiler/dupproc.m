%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
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

:- module ll_backend__dupproc.

:- interface.

% :- import_module hlds__hlds_pred.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

:- pred eliminate_duplicate_procs(assoc_list(proc_label, c_procedure)::in,
    list(c_procedure)::out,
    map(proc_label, proc_label)::in, map(proc_label, proc_label)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend.opt_util.

:- import_module int.
:- import_module require.
:- import_module std_util.

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
        standardize_proc(Proc1, StdProc1, !.DupProcMap),
        eliminate_dup_procs([Id1 - StdProc1], IdProcsTail, FinalIdProcsTail,
            !DupProcMap),
        assoc_list__values(FinalIdProcsTail, FinalProcsTail),
        Procs = [Proc1 | FinalProcsTail]
    ).

:- pred eliminate_dup_procs(assoc_list(proc_label, c_procedure)::in,
    assoc_list(proc_label, c_procedure)::in,
    assoc_list(proc_label, c_procedure)::out,
    map(proc_label, proc_label)::in, map(proc_label, proc_label)::out) is det.

eliminate_dup_procs(_ModelStdProcs, [], [], !DupProcMap).
eliminate_dup_procs(ModelStdProcs0, [Id - Proc0 | IdProcs0],
        [Id - Proc | IdProcs], !DupProcMap) :-
    (
        Proc0 ^ cproc_may_alter_rtti = may_alter_rtti,
        find_matching_model_proc(ModelStdProcs0, Id, Proc0, !.DupProcMap,
            MatchingId),
        maybe_redirect_proc(Proc0, MatchingId, MaybeProc),
        MaybeProc = yes(ProcPrime)
    ->
        Proc = ProcPrime,
        map__det_insert(!.DupProcMap, Id, MatchingId, !:DupProcMap),
        ModelStdProcs = ModelStdProcs0
    ;
        Proc = Proc0,
        standardize_proc(Proc0, StdProc0, !.DupProcMap),
        % Since the number of procedures per predicate is tiny,
        % the quadratic behavior here is not a problem.
        list__append(ModelStdProcs0, [Id - StdProc0], ModelStdProcs)
    ),
    eliminate_dup_procs(ModelStdProcs, IdProcs0, IdProcs, !DupProcMap).

:- pred find_matching_model_proc(assoc_list(proc_label, c_procedure)::in,
    proc_label::in, c_procedure::in, map(proc_label, proc_label)::in,
    proc_label::out) is semidet.

find_matching_model_proc([ModelId - ModelStdProc | ModelIdProcs], Id, Proc,
        DupProcMap, MatchingId) :-
    map__det_insert(DupProcMap, Id, ModelId, AugDupProcMap),
    standardize_proc(Proc, StdProc, AugDupProcMap),
    StdInstrs = StdProc ^ cproc_code,
    ModelStdInstrs = ModelStdProc ^ cproc_code,
    ( StdInstrs = ModelStdInstrs ->
        MatchingId = ModelId
    ;
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

maybe_redirect_proc(Proc0, Target, MaybeProc) :-
    Instrs0 = Proc0 ^ cproc_code,
    get_prologue(Instrs0, LabelInstr, _Comments, LaterInstrs),
    Redirect = goto(label(entry(local, Target))) -
        "Redirect to procedure with identical body",
    list__filter(disallowed_instr, LaterInstrs, DisallowedInstrs),
    list__length(LaterInstrs, NumLaterInstrs),
    (
        DisallowedInstrs = [],
        % The threshold here is a guess. I don't think the precise value
        % has much effect, so I don't think it is worth making it configurable.
        NumLaterInstrs < 6
    ->
        MaybeProc = no
    ;
        Instrs = [LabelInstr, Redirect],
        Proc = Proc0 ^ cproc_code := Instrs,
        MaybeProc = yes(Proc)
    ).

:- pred disallowed_instr(instruction::in) is semidet.

disallowed_instr(Instr - _) :-
    (
        Instr = if_val(_, _)
    ;
        Instr = call(_, _, _, _, _, _)
    ).

%-----------------------------------------------------------------------------%

:- pred standardize_proc(c_procedure::in, c_procedure::out,
    map(proc_label, proc_label)::in) is det.

standardize_proc(CProc, StdCProc, DupProcMap) :-
    Instrs = CProc ^ cproc_code,
    standardize_instrs(Instrs, StdInstrs, DupProcMap),
    StdCProc = CProc ^ cproc_code := StdInstrs.

:- pred standardize_instrs(list(instruction)::in, list(instruction)::out,
    map(proc_label, proc_label)::in) is det.

standardize_instrs([], [], _).
standardize_instrs([Instr - _ | Instrs], [StdInstr - "" | StdInstrs],
        DupProcMap) :-
    standardize_instr(Instr, StdInstr, DupProcMap),
    standardize_instrs(Instrs, StdInstrs, DupProcMap).

:- pred standardize_instr(instr::in, instr::out,
    map(proc_label, proc_label)::in) is det.

standardize_instr(Instr, StdInstr, DupProcMap) :-
    (
        Instr = comment(_),
        StdInstr = comment("")
    ;
        Instr = livevals(_),
        StdInstr = Instr
    ;
        Instr = block(NumIntTemps, NumFloatTemps, Instrs),
        standardize_instrs(Instrs, StdInstrs, DupProcMap),
        StdInstr = block(NumIntTemps, NumFloatTemps, StdInstrs)
    ;
        Instr = assign(Lval, Rval),
        standardize_rval(Rval, StdRval, DupProcMap),
        StdInstr = assign(Lval, StdRval)
    ;
        Instr = call(Target, Cont, LiveInfo, Context, GoalPath, Model),
        standardize_code_addr(Target, StdTarget, DupProcMap),
        standardize_code_addr(Cont, StdCont, DupProcMap),
        StdInstr = call(StdTarget, StdCont, LiveInfo, Context, GoalPath, Model)
    ;
        Instr = mkframe(FrameInfo, MaybeCodeAddr),
        (
            FrameInfo = temp_frame(_),
            StdFrameInfo = FrameInfo
        ;
            FrameInfo = ordinary_frame(_, NumSlots, MaybePragma),
            StdFrameInfo = ordinary_frame("", NumSlots, MaybePragma)
        ),
        standardize_maybe_code_addr(MaybeCodeAddr, MaybeStdCodeAddr,
            DupProcMap),
        StdInstr = mkframe(StdFrameInfo, MaybeStdCodeAddr)
    ;
        Instr = label(Label),
        standardize_label(Label, StdLabel, DupProcMap),
        StdInstr = label(StdLabel)
    ;
        Instr = goto(Target),
        standardize_code_addr(Target, StdTarget, DupProcMap),
        StdInstr = goto(StdTarget)
    ;
        Instr = computed_goto(Rval, Targets),
        standardize_labels(Targets, StdTargets, DupProcMap),
        StdInstr = computed_goto(Rval, StdTargets)
    ;
        Instr = c_code(_, _),
        StdInstr = Instr
    ;
        Instr = save_maxfr(_),
        StdInstr = Instr
    ;
        Instr = restore_maxfr(_),
        StdInstr = Instr
    ;
        Instr = if_val(Rval, Target),
        standardize_rval(Rval, StdRval, DupProcMap),
        standardize_code_addr(Target, StdTarget, DupProcMap),
        StdInstr = if_val(StdRval, StdTarget)
    ;
        Instr = incr_hp(_, _, _, _, _),
        StdInstr = Instr
    ;
        Instr = mark_hp(_),
        StdInstr = Instr
    ;
        Instr = restore_hp(_),
        StdInstr = Instr
    ;
        Instr = free_heap(_),
        StdInstr = Instr
    ;
        Instr = store_ticket(_),
        StdInstr = Instr
    ;
        Instr = reset_ticket(_, _),
        StdInstr = Instr
    ;
        Instr = discard_ticket,
        StdInstr = Instr
    ;
        Instr = prune_ticket,
        StdInstr = Instr
    ;
        Instr = mark_ticket_stack(_),
        StdInstr = Instr
    ;
        Instr = prune_tickets_to(_),
        StdInstr = Instr
    ;
        Instr = incr_sp(NumSlots, _),
        StdInstr = incr_sp(NumSlots, "")
    ;
        Instr = decr_sp(_),
        StdInstr = Instr
    ;
        Instr = decr_sp_and_return(_),
        StdInstr = Instr
    ;
        Instr = fork(Child, Parent, NumSlots),
        standardize_label(Child, StdChild, DupProcMap),
        standardize_label(Parent, StdParent, DupProcMap),
        StdInstr = fork(StdChild, StdParent, NumSlots)
    ;
        Instr = init_sync_term(_, _),
        StdInstr = Instr
    ;
        Instr = join_and_terminate(_),
        StdInstr = Instr
    ;
        Instr = join_and_continue(Lval, Label),
        standardize_label(Label, StdLabel, DupProcMap),
        StdInstr = join_and_continue(Lval, StdLabel)
    ;
        Instr = pragma_c(_, _, _, _, _, _, _, _, _),
        % The labels occurring in pragma_c instructions cannot be substituted.
        StdInstr = Instr
    ).

    % Compute the standard form of a proc_label.
    %
:- pred standardize_proc_label(proc_label::in, proc_label::out,
    map(proc_label, proc_label)::in) is det.

standardize_proc_label(ProcLabel, StdProcLabel, DupProcMap) :-
    ( map__search(DupProcMap, ProcLabel, FoundProcLabel) ->
        StdProcLabel = FoundProcLabel
    ;
        StdProcLabel = ProcLabel
    ).

    % Compute the standard form of a label.
    %
:- pred standardize_label(label::in, label::out,
    map(proc_label, proc_label)::in) is det.

standardize_label(Label, StdLabel, DupProcMap) :-
    (
        Label = internal(Num, ProcLabel),
        standardize_proc_label(ProcLabel, StdProcLabel, DupProcMap),
        StdLabel = internal(Num, StdProcLabel)
    ;
        Label = entry(Type, ProcLabel),
        standardize_proc_label(ProcLabel, StdProcLabel, DupProcMap),
        StdLabel = entry(Type, StdProcLabel)
    ).

    % Compute the standard form of a list(label).
    %
:- pred standardize_labels(list(label)::in, list(label)::out,
    map(proc_label, proc_label)::in) is det.

standardize_labels([], [], _DupProcMap).
standardize_labels([Label | Labels], [StdLabel | StdLabels], DupProcMap) :-
    standardize_label(Label, StdLabel, DupProcMap),
    standardize_labels(Labels, StdLabels, DupProcMap).

    % Compute the standard form of a code_addr.
    %
:- pred standardize_code_addr(code_addr::in, code_addr::out,
    map(proc_label, proc_label)::in) is det.

standardize_code_addr(CodeAddr, StdCodeAddr, DupProcMap) :-
    (
        CodeAddr = label(Label),
        standardize_label(Label, StdLabel, DupProcMap),
        StdCodeAddr = label(StdLabel)
    ;
        CodeAddr = imported(ProcLabel),
        standardize_proc_label(ProcLabel, StdProcLabel, DupProcMap),
        StdCodeAddr = imported(StdProcLabel)
    ;
        CodeAddr = succip,
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_succeed(_),
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_redo,
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_fail,
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_trace_redo_fail_shallow,
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_trace_redo_fail_deep,
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_call_closure(_),
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_call_class_method(_),
        StdCodeAddr = CodeAddr
    ;
        CodeAddr = do_not_reached,
        StdCodeAddr = CodeAddr
    ).

    % Compute the standard form of a maybe(code_addr).
    %
:- pred standardize_maybe_code_addr(maybe(code_addr)::in,
    maybe(code_addr)::out, map(proc_label, proc_label)::in) is det.

standardize_maybe_code_addr(MaybeCodeAddr, MaybeStdCodeAddr, DupProcMap) :-
    (
        MaybeCodeAddr = no,
        MaybeStdCodeAddr = no
    ;
        MaybeCodeAddr = yes(CodeAddr),
        standardize_code_addr(CodeAddr, StdCodeAddr, DupProcMap),
        MaybeStdCodeAddr = yes(StdCodeAddr)
    ).

    % Compute the standard form of a list(code_addr).
    %
:- pred standardize_code_addrs(list(code_addr)::in, list(code_addr)::out,
    map(proc_label, proc_label)::in) is det.

standardize_code_addrs([], [], _DupProcMap).
standardize_code_addrs([CodeAddr | CodeAddrs], [StdCodeAddr | StdCodeAddrs],
        DupProcMap) :-
    standardize_code_addr(CodeAddr, StdCodeAddr, DupProcMap),
    standardize_code_addrs(CodeAddrs, StdCodeAddrs, DupProcMap).

    % Compute the standard form of an rval.
    %
:- pred standardize_rval(rval::in, rval::out, map(proc_label, proc_label)::in)
    is det.

standardize_rval(Rval, StdRval, DupProcMap) :-
    (
        Rval = lval(_),
        StdRval = Rval
    ;
        Rval = var(_),
        error("var in standardize_rval")
    ;
        Rval = mkword(_, _),
        StdRval = Rval
    ;
        Rval = const(Const),
        standardize_rval_const(Const, StdConst, DupProcMap),
        StdRval = const(StdConst)
    ;
        Rval = unop(Unop, RvalL),
        standardize_rval(RvalL, StdRvalL, DupProcMap),
        StdRval = unop(Unop, StdRvalL)
    ;
        Rval = binop(Binop, RvalL, RvalR),
        standardize_rval(RvalL, StdRvalL, DupProcMap),
        standardize_rval(RvalR, StdRvalR, DupProcMap),
        StdRval = binop(Binop, StdRvalL, StdRvalR)
    ;
        Rval = mem_addr(_),
        StdRval = Rval
    ).

    % Compute the standard form of an rval constant.
    %
:- pred standardize_rval_const(rval_const::in, rval_const::out,
    map(proc_label, proc_label)::in) is det.

standardize_rval_const(Const, StdConst, DupProcMap) :-
    (
        Const = true,
        StdConst = Const
    ;
        Const = false,
        StdConst = Const
    ;
        Const = int_const(_),
        StdConst = Const
    ;
        Const = float_const(_),
        StdConst = Const
    ;
        Const = string_const(_),
        StdConst = Const
    ;
        Const = multi_string_const(_, _),
        StdConst = Const
    ;
        Const = code_addr_const(CodeAddr),
        standardize_code_addr(CodeAddr, StdCodeAddr, DupProcMap),
        StdConst = code_addr_const(StdCodeAddr)
    ;
        Const = data_addr_const(_, _),
        StdConst = Const
    ).
