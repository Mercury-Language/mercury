%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unify_gen.m.
%
% This module generates code that creates closures.
%
%---------------------------------------------------------------------------%

:- module ll_backend.closure_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

    % This predicate constructs or extends a closure.
    % The structure of closures is defined in runtime/mercury_ho_call.h.
    %
:- pred construct_closure(pred_id::in, proc_id::in, lambda_eval_method::in,
    prog_var::in, list(prog_var)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.stack_layout.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

construct_closure(PredId, ProcId, EvalMethod, Var, Args, GoalInfo, Code,
        !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),

    % We handle currying of a higher-order pred variable as a special case.
    % We recognize
    %
    %   P = l(P0, X, Y, Z)
    %
    % where
    %
    %   l(P0, A, B, C, ...) :- P0(A, B, C, ...).    % higher-order call
    %
    % as a special case, and generate special code to construct the
    % new closure P from the old closure P0 by appending the args X, Y, Z.
    % The advantage of this optimization is that when P is called, we
    % will only need to do one indirect call rather than two.
    % Its disadvantage is that the cost of creating the closure P is greater.
    % Whether this is a net win depend on the number of times P is called.
    %
    % The pattern that this optimization looks for happens rarely at the
    % moment. The reason is that although we allow the creation of closures
    % with a simple syntax (e.g. P0 = append4([1])), we don't allow their
    % extension with a similarly simple syntax (e.g. P = call(P0, [2])).
    % In fact, typecheck.m contains code to detect such constructs, because
    % it does not have code to typecheck them (you get a message about call/2
    % should be used as a goal, not an expression).

    proc_info_get_goal(ProcInfo, ProcInfoGoal),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_headvars(ProcInfo, ProcHeadVars),
    ( if
        EvalMethod = lambda_normal,
        Args = [CallPred | CallArgs],
        ProcHeadVars = [ProcPred | ProcArgs],
        ProcInfoGoal = hlds_goal(generic_call(higher_order(ProcPred, _, _, _),
            ProcArgs, _, _, CallDeterminism), _GoalInfo),
        determinism_to_code_model(CallDeterminism, CallCodeModel),
        % Check that the code models are compatible. Note that det is not
        % compatible with semidet, and semidet is not compatible with nondet,
        % since the arguments go in different registers.
        % But det is compatible with nondet.
        (
            CodeModel = CallCodeModel
        ;
            CodeModel = model_non,
            CallCodeModel = model_det
        ),
        % This optimization distorts deep profiles, so don't perform it
        % in deep profiling grades.
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, profile_deep, Deep),
        Deep = no,
        % XXX If float registers are used, float register arguments are placed
        % after regular register arguments in the hidden arguments vector.
        % Generate_new_closure_from_old does not (yet) handle that layout.
        globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
        UseFloatRegs = no
    then
        (
            CallArgs = [],
            % If there are no new arguments, we can just use the old closure.
            assign_var_to_var(Var, CallPred, !CLD),
            Code = empty
        ;
            CallArgs = [_ | _],
            generate_new_closure_from_old(Var, CallPred, CallArgs, GoalInfo,
                Code, !CI, !CLD)
        )
    else
        generate_closure_from_scratch(ModuleInfo, PredId, ProcId,
            PredInfo, ProcInfo, Var, Args, GoalInfo, Code, !CI, !CLD)
    ).

:- pred generate_new_closure_from_old(prog_var::in,
    prog_var::in, list(prog_var)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_new_closure_from_old(Var, CallPred, CallArgs, GoalInfo, Code,
        !CI, !CLD) :-
    get_next_label(LoopStart, !CI),
    get_next_label(LoopTest, !CI),
    acquire_reg(reg_r, LoopCounter, !CLD),
    acquire_reg(reg_r, NumOldArgs, !CLD),
    acquire_reg(reg_r, NewClosure, !CLD),
    Zero = const(llconst_int(0)),
    One = const(llconst_int(1)),
    Two = const(llconst_int(2)),
    Three = const(llconst_int(3)),
    list.length(CallArgs, NumNewArgs),
    NumNewArgs_Rval = const(llconst_int(NumNewArgs)),
    NumNewArgsPlusThree = NumNewArgs + 3,
    NumNewArgsPlusThree_Rval = const(llconst_int(NumNewArgsPlusThree)),
    produce_variable(CallPred, OldClosureCode, OldClosure, !CLD),
    Context = goal_info_get_context(GoalInfo),
    maybe_add_alloc_site_info(Context, "closure", NumNewArgsPlusThree,
        MaybeAllocId, !CI),
    % The new closure contains a pointer to the old closure.
    NewClosureMayUseAtomic = may_not_use_atomic_alloc,
    NewClosureCode = from_list([
        llds_instr(comment("build new closure from old closure"), ""),
        llds_instr(
            assign(NumOldArgs, lval(field(yes(ptag(0u8)), OldClosure, Two))),
            "get number of arguments"),
        llds_instr(incr_hp(NewClosure, no, no,
            binop(int_add(int_type_int), lval(NumOldArgs),
                NumNewArgsPlusThree_Rval),
            MaybeAllocId, NewClosureMayUseAtomic, no, no_llds_reuse),
            "allocate new closure"),
        llds_instr(assign(field(yes(ptag(0u8)), lval(NewClosure), Zero),
            lval(field(yes(ptag(0u8)), OldClosure, Zero))),
            "set closure layout structure"),
        llds_instr(assign(field(yes(ptag(0u8)), lval(NewClosure), One),
            lval(field(yes(ptag(0u8)), OldClosure, One))),
            "set closure code pointer"),
        llds_instr(assign(field(yes(ptag(0u8)), lval(NewClosure), Two),
            binop(int_add(int_type_int), lval(NumOldArgs),
                NumNewArgs_Rval)),
            "set new number of arguments"),
        llds_instr(
            assign(NumOldArgs,
                binop(int_add(int_type_int), lval(NumOldArgs), Three)),
            "set up loop limit"),
        llds_instr(assign(LoopCounter, Three),
            "initialize loop counter"),
        % It is possible for the number of hidden arguments to be zero,
        % in which case the body of this loop should not be executed at all.
        % This is why we jump to the loop condition test.
        llds_instr(goto(code_label(LoopTest)),
            "enter the copy loop at the conceptual top"),
        llds_instr(label(LoopStart),
            "start of loop, nofulljump"),
        llds_instr(
            assign(field(yes(ptag(0u8)), lval(NewClosure), lval(LoopCounter)),
                lval(field(yes(ptag(0u8)), OldClosure, lval(LoopCounter)))),
            "copy old hidden argument"),
        llds_instr(
            assign(LoopCounter,
                binop(int_add(int_type_int), lval(LoopCounter), One)),
            "increment loop counter"),
        llds_instr(label(LoopTest),
            "do we have more old arguments to copy? nofulljump"),
        llds_instr(
            if_val(binop(int_lt(int_type_int),
                lval(LoopCounter), lval(NumOldArgs)),
                code_label(LoopStart)),
            "repeat the loop?")
    ]),
    generate_extra_closure_args(CallArgs, LoopCounter, NewClosure,
        ExtraArgsCode, !.CI, !CLD),
    release_reg(LoopCounter, !CLD),
    release_reg(NumOldArgs, !CLD),
    release_reg(NewClosure, !CLD),
    assign_lval_to_var(Var, NewClosure, AssignCode, !.CI, !CLD),
    Code = OldClosureCode ++ NewClosureCode ++ ExtraArgsCode ++ AssignCode.

:- pred generate_closure_from_scratch(module_info::in,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in,
    prog_var::in, list(prog_var)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_closure_from_scratch(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        Var, Args, GoalInfo, Code, !CI, !CLD) :-
    CodeAddr = make_proc_entry_label(!.CI, ModuleInfo, PredId, ProcId,
        for_closure),
    ProcLabel = extract_proc_label_from_code_addr(CodeAddr),
    CodeAddrRval = const(llconst_code_addr(CodeAddr)),
    continuation_info.generate_closure_layout( ModuleInfo, PredId, ProcId,
        ClosureInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    Context = goal_info_get_context(GoalInfo),
    FileName = term_context.context_file(Context),
    LineNumber = term_context.context_line(Context),
    goal_id(GoalIdNum) = goal_info_get_goal_id(GoalInfo),
    GoalIdStr = string.int_to_string(GoalIdNum),
    get_proc_label(!.CI, CallerProcLabel),
    get_next_closure_seq_no(SeqNo, !CI),
    get_static_cell_info(!.CI, StaticCellInfo0),
    hlds.hlds_pred.pred_info_get_origin(PredInfo, PredOrigin),
    stack_layout.construct_closure_layout(CallerProcLabel, SeqNo, ClosureInfo,
        ProcLabel, ModuleName, FileName, LineNumber, PredOrigin, GoalIdStr,
        StaticCellInfo0, StaticCellInfo, ClosureLayoutTypedRvals, Data),
    set_static_cell_info(StaticCellInfo, !CI),
    add_closure_layout(Data, !CI),
    % For now, closures always have zero size, and the size slot
    % is never looked at.
    add_scalar_static_cell(ClosureLayoutTypedRvals, ClosureDataAddr, !CI),
    ClosureLayoutRval = const(llconst_data_addr(ClosureDataAddr, no)),
    proc_info_arg_info(ProcInfo, ArgInfo),
    get_var_table(!.CI, VarTable),
    get_may_use_atomic_alloc(!.CI, MayUseAtomic0),
    generate_pred_args(!.CI, VarTable, Args, ArgInfo, ArgsR, ArgsF,
        MayUseAtomic0, MayUseAtomic),
    list.length(ArgsR, NumArgsR),
    list.length(ArgsF, NumArgsF),
    NumArgsRF = encode_num_generic_call_vars(NumArgsR, NumArgsF),
    list.append(ArgsR, ArgsF, ArgsRF),
    CellArgs = [
        cell_arg_full_word(ClosureLayoutRval, complete),
        cell_arg_full_word(CodeAddrRval, complete),
        cell_arg_full_word(const(llconst_int(NumArgsRF)), complete)
        | ArgsRF
    ],
    % XXX construct_dynamically is just a dummy value. We just want
    % something which is not construct_in_region(_).
    HowToConstruct = construct_dynamically,
    MaybeSize = no,
    maybe_add_alloc_site_info(Context, "closure", length(CellArgs),
        MaybeAllocId, !CI),
    assign_cell_to_var(Var, no, ptag(0u8), CellArgs, HowToConstruct,
        MaybeSize, MaybeAllocId, MayUseAtomic, Code, !CI, !CLD).

:- pred generate_extra_closure_args(list(prog_var)::in, lval::in,
    lval::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_extra_closure_args([], _, _, empty, _CI, !CLD).
generate_extra_closure_args([Var | Vars], LoopCounter, NewClosure, Code,
        CI, !CLD) :-
    FieldLval = field(yes(ptag(0u8)), lval(NewClosure), lval(LoopCounter)),
    get_var_table(CI, VarTable),
    lookup_var_entry(VarTable, Var, Entry),
    IsDummy = Entry ^ vte_is_dummy,
    (
        IsDummy = is_dummy_type,
        ProduceAssignCode = singleton(
            llds_instr(assign(FieldLval, const(llconst_int(0))),
                "set new argument field (dummy type)")
        )
    ;
        IsDummy = is_not_dummy_type,
        produce_variable(Var, ProduceCode, Value, !CLD),
        AssignCode = singleton(
            llds_instr(assign(FieldLval, Value),
                "set new argument field")
        ),
        ProduceAssignCode = ProduceCode ++ AssignCode
    ),
    IncrCode = singleton(
        llds_instr(assign(LoopCounter,
            binop(int_add(int_type_int), lval(LoopCounter),
                const(llconst_int(1)))),
            "increment argument counter")
    ),
    VarCode = ProduceAssignCode ++ IncrCode,
    generate_extra_closure_args(Vars, LoopCounter, NewClosure, VarsCode,
        CI, !CLD),
    Code = VarCode ++ VarsCode.

:- pred generate_pred_args(code_info::in, var_table::in, list(prog_var)::in,
    list(arg_info)::in, list(cell_arg)::out, list(cell_arg)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

generate_pred_args(_, _, [], _, [], [], !MayUseAtomic).
generate_pred_args(_, _, [_ | _], [], _, _, !MayUseAtomic) :-
    unexpected($pred, "insufficient args").
generate_pred_args(CI, VarTable, [Var | Vars], [ArgInfo | ArgInfos],
        ArgsR, ArgsF, !MayUseAtomic) :-
    ArgInfo = arg_info(reg(RegType, _), ArgMode),
    lookup_var_entry(VarTable, Var, Entry),
    Entry = vte(_, Type, IsDummy),
    (
        ArgMode = top_in,
        (
            IsDummy = is_dummy_type,
            Rval = const(llconst_int(0))
        ;
            IsDummy = is_not_dummy_type,
            Rval = var(Var)
        ),
        CellArg = cell_arg_full_word(Rval, complete)
    ;
        ( ArgMode = top_out
        ; ArgMode = top_unused
        ),
        % XXX PACK_64
        % This will become incorrect when we start allowing
        % unpacked int64s and uint64s on 32 bit machines.
        CellArg = cell_arg_skip_one_word
    ),
    get_module_info(CI, ModuleInfo),
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    generate_pred_args(CI, VarTable, Vars, ArgInfos, ArgsR0, ArgsF0,
        !MayUseAtomic),
    (
        RegType = reg_r,
        ArgsR = [CellArg | ArgsR0],
        ArgsF = ArgsF0
    ;
        RegType = reg_f,
        ArgsR = ArgsR0,
        ArgsF = [CellArg | ArgsF0]
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.closure_gen.
%---------------------------------------------------------------------------%
