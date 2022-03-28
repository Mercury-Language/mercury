%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: middle_rec.m.
% Main authors: zs, conway.
%
% Code generation - do middle recursion optimization.
%
%---------------------------------------------------------------------------%

:- module ll_backend.middle_rec.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.

:- pred match_and_generate(hlds_goal::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module hlds.var_table.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.code_util.
:- import_module ll_backend.opt_util.
:- import_module ll_backend.proc_gen.
:- import_module ll_backend.unify_gen_test.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%

match_and_generate(Goal, Instrs, !CI, !CLD) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    GoalExpr = switch(Var, cannot_fail, [Case1, Case2]),
    Case1 = case(ConsId1, [], Goal1),
    Case2 = case(ConsId2, [], Goal2),
    ( if
        contains_only_builtins(Goal1) = yes,
        contains_simple_recursive_call(Goal2, !.CI)
    then
        middle_rec_generate_switch(Var, ConsId1, Goal1, Goal2,
            GoalInfo, Instrs, !CI, !CLD)
    else if
        contains_only_builtins(Goal2) = yes,
        contains_simple_recursive_call(Goal1, !.CI)
    then
        middle_rec_generate_switch(Var, ConsId2, Goal2, Goal1,
            GoalInfo, Instrs, !CI, !CLD)
    else
        fail
    ).

%---------------------------------------------------------------------------%

    % contains_simple_recursive_call(G, CI, Last, ContainsTakeAddr)
    % succeeds if G is a conjunction of goals, exactly one of which is a
    % recursive call (CI says what the current procedure is), there are no
    % other goals that cause control to leave this procedure, and there are
    % no unifications that take the addresses of fields.
    %
:- pred contains_simple_recursive_call(hlds_goal::in, code_info::in)
    is semidet.

contains_simple_recursive_call(hlds_goal(GoalExpr, _), CodeInfo) :-
    GoalExpr = conj(plain_conj, Goals),
    contains_simple_recursive_call_conj(Goals, CodeInfo).

:- pred contains_simple_recursive_call_conj(list(hlds_goal)::in, code_info::in)
    is semidet.

contains_simple_recursive_call_conj([Goal | Goals], CodeInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    OnlyBuiltinsGoalExpr = contains_only_builtins_expr(GoalExpr),
    (
        OnlyBuiltinsGoalExpr = yes,
        contains_simple_recursive_call_conj(Goals, CodeInfo)
    ;
        OnlyBuiltinsGoalExpr = no,
        is_recursive_call(GoalExpr, CodeInfo),
        contains_only_builtins_list(Goals) = yes
    ).

:- pred is_recursive_call(hlds_goal_expr::in, code_info::in) is semidet.

is_recursive_call(Goal, CodeInfo) :-
    Goal = plain_call(CallPredId, CallProcId, _, BuiltinState, _, _),
    BuiltinState = not_builtin,
    get_pred_id(CodeInfo, PredId),
    PredId = CallPredId,
    get_proc_id(CodeInfo, ProcId),
    ProcId = CallProcId.

    % contains_only_builtins(G) returns `yes' if G is a leaf procedure,
    % i.e. control does not leave G to call another procedure, even
    % if that procedure is a complicated unification. It also does not contain
    % unifications that take the addresses of fields.
    %
:- func contains_only_builtins(hlds_goal) = bool.

contains_only_builtins(hlds_goal(GoalExpr, _)) =
    contains_only_builtins_expr(GoalExpr).

:- func contains_only_builtins_expr(hlds_goal_expr) = bool.

contains_only_builtins_expr(GoalExpr) = OnlyBuiltins :-
    (
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj,
            OnlyBuiltins = contains_only_builtins_list(Goals)
        ;
            ConjType = parallel_conj,
            OnlyBuiltins = no
        )
    ;
        GoalExpr = disj(Goals),
        OnlyBuiltins = contains_only_builtins_list(Goals)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        OnlyBuiltins = contains_only_builtins_cases(Cases)
    ;
        GoalExpr = negation(SubGoal),
        OnlyBuiltins = contains_only_builtins(SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            OnlyBuiltins = yes
        else
            OnlyBuiltins = contains_only_builtins(SubGoal)
        )
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        ( if
            contains_only_builtins(Cond) = yes,
            contains_only_builtins(Then) = yes,
            contains_only_builtins(Else) = yes
        then
            OnlyBuiltins = yes
        else
            OnlyBuiltins = no
        )
    ;
        GoalExpr = plain_call(_, _, _, BuiltinState, _, _),
        (
            BuiltinState = inline_builtin,
            OnlyBuiltins = yes
        ;
            BuiltinState = not_builtin,
            OnlyBuiltins = no
        )
    ;
        GoalExpr = unify(_, _, _, Uni, _),
        % Complicated unifies are _non_builtin_
        (
            Uni = assign(_, _),
            OnlyBuiltins = yes
        ;
            Uni = simple_test(_, _),
            OnlyBuiltins = yes
        ;
            Uni = construct(_, _, _, _, _, _, SubInfo),
            (
                SubInfo = no_construct_sub_info,
                OnlyBuiltins = yes
            ;
                SubInfo = construct_sub_info(TakeAddressFields, _),
                (
                    TakeAddressFields = no,
                    OnlyBuiltins = yes
                ;
                    TakeAddressFields = yes(_),
                    OnlyBuiltins = no
                )
            )
        ;
            Uni = deconstruct(_, _, _, _, _, _),
            OnlyBuiltins = yes
        ;
            Uni = complicated_unify(_, _, _),
            OnlyBuiltins = no
        )
    ;
        ( GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ),
        OnlyBuiltins = no
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- func contains_only_builtins_cases(list(case)) = bool.

contains_only_builtins_cases([]) = yes.
contains_only_builtins_cases([case(_, _, Goal) | Cases]) = OnlyBuiltins :-
    OnlyBuiltinsGoal = contains_only_builtins(Goal),
    (
        OnlyBuiltinsGoal = yes,
        OnlyBuiltins = contains_only_builtins_cases(Cases)
    ;
        OnlyBuiltinsGoal = no,
        OnlyBuiltins = no
    ).

:- func contains_only_builtins_list(list(hlds_goal)) = bool.

contains_only_builtins_list([]) = yes.
contains_only_builtins_list([Goal | Goals]) = OnlyBuiltins :-
    OnlyBuiltinsGoal = contains_only_builtins(Goal),
    (
        OnlyBuiltinsGoal = yes,
        OnlyBuiltins = contains_only_builtins_list(Goals)
    ;
        OnlyBuiltinsGoal = no,
        OnlyBuiltins = no
    ).

%---------------------------------------------------------------------------%

:- pred middle_rec_generate_switch(prog_var::in, cons_id::in,
    hlds_goal::in, hlds_goal::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out)
    is semidet.

middle_rec_generate_switch(Var, BaseConsId, Base, Recursive, SwitchGoalInfo,
        Code, !CI, !CLD) :-
    get_stack_slots(!.CI, StackSlots),
    get_var_table(!.CI, VarTable),
    SlotsComment = explain_stack_slots(VarTable, StackSlots),
    get_module_info(!.CI, ModuleInfo),
    get_pred_id(!.CI, PredId),
    get_proc_id(!.CI, ProcId),
    EntryLabel = make_local_entry_label(ModuleInfo, PredId, ProcId,
        for_from_everywhere),

    pre_goal_update(SwitchGoalInfo, has_subgoals, !CLD),
    produce_variable(Var, VarCode, VarRval, !CLD),
    lookup_var_entry(VarTable, Var, VarEntry),
    VarName = var_entry_name(Var, VarEntry),
    VarType = VarEntry ^ vte_type,
    CheaperTagTest = lookup_cheaper_tag_test(!.CI, VarType),
    generate_test_var_has_cons_id(VarRval, VarName, BaseConsId,
        CheaperTagTest, branch_on_success, BaseLabel, TestCode, !CI),
    EntryTestInstrs = cord.list(VarCode ++ TestCode),

    goal_info_get_store_map(SwitchGoalInfo, StoreMap),
    remember_position(!.CLD, BranchStart),
    generate_goal(model_det, Base, BaseGoalCode, !CI, !CLD),
    generate_branch_end(StoreMap, no, MaybeEnd1, BaseSaveCode, !.CLD),
    reset_to_position(BranchStart, !.CI, !:CLD),
    generate_goal(model_det, Recursive, RecGoalCode, !CI, !CLD),
    generate_branch_end(StoreMap, MaybeEnd1, MaybeEnd, RecSaveCode, !.CLD),

    after_all_branches(StoreMap, MaybeEnd, !.CI, !:CLD),
    post_goal_update(SwitchGoalInfo, !.CI, !CLD),

    ArgModes = get_arginfo(!.CI),
    HeadVars = get_headvars(!.CI),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, Args),
    setup_return(Args, LiveArgs, EpilogCode, !CLD),

    BaseCode = BaseGoalCode ++ BaseSaveCode ++ EpilogCode,
    RecCode = RecGoalCode ++ RecSaveCode ++ EpilogCode,
    LiveValCode = singleton(
        llds_instr(livevals(LiveArgs), "")
    ),

    BaseInstrs = cord.list(BaseCode),
    RecInstrs = cord.list(RecCode),

    % In the code we generate, the base instruction sequence is executed
    % in situations where this procedure has no stack frame. If this
    % sequence refers to the stack frame, it will be to some other procedure's
    % variables, which is obviously incorrect.
    opt_util.block_refers_to_stack(BaseInstrs) = no,

    AvoidInstrs = BaseInstrs ++ RecInstrs,
    find_unused_register(AvoidInstrs, AuxReg),

    split_rec_code(RecInstrs, BeforeInstrs0, AfterInstrs),
    add_counter_to_livevals(BeforeInstrs0, AuxReg, BeforeInstrs),

    get_next_label(Loop1Label, !CI),
    get_next_label(Loop2Label, !CI),
    get_total_stackslot_count(!.CI, FrameSize0),
    FrameSize = round_det_stack_frame_size(!.CI, FrameSize0),

    generate_downloop_test(EntryTestInstrs, Loop1Label, Loop1Test),

    ( if FrameSize = 0 then
        MaybeIncrSp = empty,
        MaybeDecrSp = empty,
        InitAuxReg = singleton(
            llds_instr(assign(AuxReg, const(llconst_int(0))),
                "initialize counter register")
        ),
        IncrAuxReg = singleton(
            llds_instr(
                assign(AuxReg,
                    binop(int_add(int_type_int), lval(AuxReg),
                        const(llconst_int(1)))),
                "increment loop counter")
        ),
        DecrAuxReg = singleton(
            llds_instr(
                assign(AuxReg,
                    binop(int_sub(int_type_int), lval(AuxReg),
                        const(llconst_int(1)))),
                "decrement loop counter")
        ),
        TestAuxReg = singleton(
            llds_instr(
                if_val(binop(
                    int_gt(int_type_int), lval(AuxReg), const(llconst_int(0))),
                    code_label(Loop2Label)),
                "test on upward loop")
        )
    else
        PushMsg = proc_gen.push_msg(ModuleInfo, PredId, ProcId),
        MaybeIncrSp = singleton(
            llds_instr(incr_sp(FrameSize, PushMsg, stack_incr_nonleaf), "")
        ),
        MaybeDecrSp = singleton(
            llds_instr(decr_sp(FrameSize), "")
        ),
        InitAuxReg =  singleton(
            llds_instr(assign(AuxReg, lval(sp)), "initialize counter register")
        ),
        IncrAuxReg = empty,
        DecrAuxReg = empty,
        TestAuxReg = singleton(
            llds_instr(if_val(binop(
                int_gt(int_type_int), lval(sp), lval(AuxReg)),
                code_label(Loop2Label)),
                "test on upward loop")
        )
    ),

    % Even though the recursive call is followed by some goals in the HLDS,
    % these goals may generate no LLDS code, so it is in fact possible for
    % AfterInstrs to be empty. There is no point in testing BeforeInstrs
    % for empty, since if it is, the code is an infinite loop anyway.

    (
        AfterInstrs = [],
        Code =
            from_list([
                llds_instr(label(EntryLabel), "Procedure entry point"),
                llds_instr(comment(SlotsComment), "")
            ]) ++
            from_list(EntryTestInstrs) ++
            singleton(
                llds_instr(label(Loop1Label), "start of the down loop")
            ) ++
            from_list(BeforeInstrs) ++
            from_list(Loop1Test) ++
            singleton(
                llds_instr(label(BaseLabel), "start of base case")
            ) ++
            from_list(BaseInstrs) ++
            LiveValCode ++
            singleton(
                llds_instr(goto(code_succip), "exit from base case")
            )
    ;
        AfterInstrs = [_ | _],
        % The instruction list we are constructing has two copies of BaseList.
        % If this list of instructions defines any labels, we must either not
        % apply this version of the optimization, or we must consistently
        % substitute the labels (which will be referred to only from within the
        % BaseList instructions themselves). We choose the former course.
        find_labels(BaseInstrs, BaseLabels),
        BaseLabels = [],
        Code =
            from_list([
                llds_instr(label(EntryLabel), "Procedure entry point"),
                llds_instr(comment(SlotsComment), "")
            ]) ++
            from_list(EntryTestInstrs) ++
            InitAuxReg ++
            singleton(
                llds_instr(label(Loop1Label), "start of the down loop")
            ) ++
            MaybeIncrSp ++
            IncrAuxReg ++
            from_list(BeforeInstrs) ++
            from_list(Loop1Test) ++
            from_list(BaseInstrs) ++
            singleton(
                llds_instr(label(Loop2Label), "")
            ) ++
            from_list(AfterInstrs) ++
            MaybeDecrSp ++
            DecrAuxReg ++
            TestAuxReg ++
            LiveValCode ++
            from_list([
                llds_instr(goto(code_succip), "exit from recursive case"),
                llds_instr(label(BaseLabel), "start of base case")
            ]) ++
            from_list(BaseInstrs) ++
            LiveValCode ++
            singleton(
                llds_instr(goto(code_succip), "exit from base case")
            )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred generate_downloop_test(list(instruction)::in, label::in,
    list(instruction)::out) is det.

generate_downloop_test([], _, _) :-
    unexpected($pred, "empty list").
generate_downloop_test([Instr0 | Instrs0], Target, Instrs) :-
    ( if Instr0 = llds_instr(if_val(Test, _OldTarget), _Comment) then
        (
            Instrs0 = []
        ;
            Instrs0 = [_ | _],
            unexpected($pred, "if_val followed by other instructions")
        ),
        code_util.neg_rval(Test, NewTest),
        Instrs = [
            llds_instr(if_val(NewTest, code_label(Target)),
                "test on downward loop")
        ]
    else
        generate_downloop_test(Instrs0, Target, Instrs1),
        Instrs = [Instr0 | Instrs1]
    ).

%---------------------------------------------------------------------------%

:- pred split_rec_code(list(instruction)::in,
    list(instruction)::out, list(instruction)::out) is det.

split_rec_code([], _, _) :-
    unexpected($pred, "did not find call").
split_rec_code([Instr0 | Instrs1], Before, After) :-
    ( if Instr0 = llds_instr(llcall(_, _, _, _, _, _), _) then
        ( if
            opt_util.skip_comments(Instrs1, Instrs2),
            Instrs2 = [Instr2 | Instrs3],
            Instr2 = llds_instr(label(_), _)
        then
            Before = [],
            After = Instrs3
        else
            unexpected($pred, "call not followed by label")
        )
    else
        split_rec_code(Instrs1, Before1, After),
        Before = [Instr0 | Before1]
    ).

%---------------------------------------------------------------------------%

:- pred add_counter_to_livevals(list(instruction)::in, lval::in,
    list(instruction)::out) is det.

add_counter_to_livevals([], _Lval, []).
add_counter_to_livevals([Instr0 | Instrs0], Lval, [Instr | Instrs]) :-
    ( if Instr0 = llds_instr(livevals(Lives0), Comment) then
        set.insert(Lval, Lives0, Lives),
        Instr = llds_instr(livevals(Lives), Comment)
    else
        Instr = Instr0
    ),
    add_counter_to_livevals(Instrs0, Lval, Instrs).

%---------------------------------------------------------------------------%

:- pred find_unused_register(list(instruction)::in, lval::out)
    is det.

find_unused_register(Instrs, UnusedReg) :-
    set.init(Used0),
    find_used_registers(Instrs, Used0, Used1),
    set.to_sorted_list(Used1, UsedList),
    find_unused_register_acc(UsedList, 1, UnusedReg).

:- pred find_unused_register_acc(list(int)::in, int::in, lval::out) is det.

find_unused_register_acc([], N, reg(reg_r, N)).
find_unused_register_acc([H | T], N, Reg) :-
    ( if N < H then
        Reg = reg(reg_r, N)
    else
        N1 = N + 1,
        find_unused_register_acc(T, N1, Reg)
    ).

:- pred find_used_registers(list(instruction)::in,
    set(int)::in, set(int)::out) is det.

find_used_registers([], !Used).
find_used_registers([llds_instr(Uinstr, _) | Instrs], !Used) :-
    find_used_registers_instr(Uinstr, !Used),
    find_used_registers(Instrs, !Used).

:- pred find_used_registers_instr(instr::in,
    set(int)::in, set(int)::out) is det.

find_used_registers_instr(Uinstr, !Used) :-
    (
        ( Uinstr = comment(_)
        ; Uinstr = llcall(_, _, _, _, _, _)
        ; Uinstr = mkframe(_, _)
        ; Uinstr = label(_)
        ; Uinstr = goto(_)
        ; Uinstr = arbitrary_c_code(_, _, _)
        ; Uinstr = push_region_frame(_Id, _EmbeddedStackFrame)
        ; Uinstr = use_and_maybe_pop_region_frame(_UseOp, _EmbeddedStackFrame)
        ; Uinstr = discard_ticket
        ; Uinstr = prune_ticket
        ; Uinstr = incr_sp(_, _, _)
        ; Uinstr = decr_sp(_)
        ; Uinstr = decr_sp_and_return(_)
        )
    ;
        Uinstr = livevals(LvalSet),
        set.to_sorted_list(LvalSet, LvalList),
        find_used_registers_lvals(LvalList, !Used)
    ;
        Uinstr = block(_, _, Instrs),
        find_used_registers(Instrs, !Used)
    ;
        ( Uinstr = assign(Lval, Rval)
        ; Uinstr = keep_assign(Lval, Rval)
        ),
        find_used_registers_lval(Lval, !Used),
        find_used_registers_rval(Rval, !Used)
    ;
        Uinstr = incr_hp(Lval, _, _, Rval, _, _, MaybeRegionRval, MaybeReuse),
        find_used_registers_lval(Lval, !Used),
        find_used_registers_rval(Rval, !Used),
        (
            MaybeRegionRval = yes(RegionRval),
            find_used_registers_rval(RegionRval, !Used)
        ;
            MaybeRegionRval = no
        ),
        (
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            find_used_registers_rval(ReuseRval, !Used),
            (
                MaybeFlagLval = yes(FlagLval),
                find_used_registers_lval(FlagLval, !Used)
            ;
                MaybeFlagLval = no
            )
        ;
            MaybeReuse = no_llds_reuse
        )
    ;
        Uinstr = region_fill_frame(_FillOp, _EmbeddedStackFrame,
            IdRval, NumLval, AddrLval),
        find_used_registers_rval(IdRval, !Used),
        find_used_registers_lval(NumLval, !Used),
        find_used_registers_lval(AddrLval, !Used)
    ;
        Uinstr = region_set_fixed_slot(_SetOp, _EmbeddedStackFrame, ValueRval),
        find_used_registers_rval(ValueRval, !Used)
    ;
        Uinstr = foreign_proc_code(_, Components, _, _, _, _, _, _, _, _),
        find_used_registers_components(Components, !Used)
    ;
        ( Uinstr = computed_goto(Rval, _)
        ; Uinstr = if_val(Rval, _)
        ; Uinstr = restore_hp(Rval)
        ; Uinstr = free_heap(Rval)
        ; Uinstr = reset_ticket(Rval, _Rsn)
        ; Uinstr = prune_tickets_to(Rval)
        ),
        find_used_registers_rval(Rval, !Used)
    ;
        ( Uinstr = save_maxfr(Lval)
        ; Uinstr = restore_maxfr(Lval)
        ; Uinstr = mark_hp(Lval)
        ; Uinstr = store_ticket(Lval)
        ; Uinstr = mark_ticket_stack(Lval)
        ; Uinstr = init_sync_term(Lval, _, _)
        ; Uinstr = fork_new_child(Lval, _)
        ; Uinstr = join_and_continue(Lval, _)
        ),
        find_used_registers_lval(Lval, !Used)
    ;
        Uinstr = lc_create_loop_control(_, LCLval),
        find_used_registers_lval(LCLval, !Used)
    ;
        Uinstr = lc_wait_free_slot(LCRval, LCSLval, _),
        find_used_registers_rval(LCRval, !Used),
        find_used_registers_lval(LCSLval, !Used)
    ;
        Uinstr = lc_spawn_off(LCRval, LCSRval, _),
        find_used_registers_rval(LCRval, !Used),
        find_used_registers_rval(LCSRval, !Used)
    ;
        Uinstr = lc_join_and_terminate(LCRval, LCSRval),
        find_used_registers_rval(LCRval, !Used),
        find_used_registers_rval(LCSRval, !Used)
    ).

:- pred find_used_registers_components(
    list(foreign_proc_component)::in,
    set(int)::in, set(int)::out) is det.

find_used_registers_components([], !Used).
find_used_registers_components([Comp | Comps], !Used) :-
    find_used_registers_component(Comp, !Used),
    find_used_registers_components(Comps, !Used).

:- pred find_used_registers_component(foreign_proc_component::in,
    set(int)::in, set(int)::out) is det.

find_used_registers_component(foreign_proc_inputs(In), !Used) :-
    insert_foreign_proc_input_registers(In, !Used).
find_used_registers_component(foreign_proc_outputs(Out), !Used) :-
    insert_foreign_proc_output_registers(Out, !Used).
find_used_registers_component(foreign_proc_user_code(_, _, _), !Used).
find_used_registers_component(foreign_proc_raw_code(_, _, _, _), !Used).
find_used_registers_component(foreign_proc_fail_to(_), !Used).
find_used_registers_component(foreign_proc_alloc_id(_), !Used).
find_used_registers_component(foreign_proc_noop, !Used).

:- pred find_used_registers_lvals(list(lval)::in,
    set(int)::in, set(int)::out) is det.

find_used_registers_lvals([], !Used).
find_used_registers_lvals([Lval | Lvals], !Used) :-
    find_used_registers_lval(Lval, !Used),
    find_used_registers_lvals(Lvals, !Used).

:- pred find_used_registers_lval(lval::in,
    set(int)::in, set(int)::out) is det.

find_used_registers_lval(Lval, !Used) :-
    ( if Lval = reg(reg_r, N) then
        copy(N, N1),
        set.insert(N1, !Used)
    else if Lval = field(_, Rval, FieldNum) then
        find_used_registers_rval(Rval, !Used),
        find_used_registers_rval(FieldNum, !Used)
    else if Lval = lvar(_) then
        unexpected($pred, "lvar")
    else
        true
    ).

:- pred find_used_registers_rval(rval::in, set(int)::in, set(int)::out) is det.

find_used_registers_rval(Rval, !Used) :-
    (
        Rval = lval(Lval),
        find_used_registers_lval(Lval, !Used)
    ;
        Rval = var(_),
        unexpected($pred, "var")
    ;
        Rval = mkword_hole(_)
    ;
        Rval = const(_)
    ;
        ( Rval = mkword(_, SubRval)
        ; Rval = cast(_, SubRval)
        ; Rval = unop(_, SubRval)
        ),
        find_used_registers_rval(SubRval, !Used)
    ;
        Rval = binop(_, SubRvalA, SubRvalB),
        find_used_registers_rval(SubRvalA, !Used),
        find_used_registers_rval(SubRvalB, !Used)
    ;
        Rval = mem_addr(MemRef),
        find_used_registers_mem_ref(MemRef, !Used)
    ).

:- pred find_used_registers_mem_ref(mem_ref::in,
    set(int)::in, set(int)::out) is det.

find_used_registers_mem_ref(stackvar_ref(Rval), !Used) :-
    find_used_registers_rval(Rval, !Used).
find_used_registers_mem_ref(framevar_ref(Rval), !Used) :-
    find_used_registers_rval(Rval, !Used).
find_used_registers_mem_ref(heap_ref(Rval1, _, Rval2), !Used) :-
    find_used_registers_rval(Rval1, !Used),
    find_used_registers_rval(Rval2, !Used).

:- pred insert_foreign_proc_input_registers(list(foreign_proc_input)::in,
    set(int)::in, set(int)::out) is det.

insert_foreign_proc_input_registers([], !Used).
insert_foreign_proc_input_registers([Input | Inputs], !Used) :-
    Input = foreign_proc_input(_, _, _, _, Rval, _, _),
    find_used_registers_rval(Rval, !Used),
    insert_foreign_proc_input_registers(Inputs, !Used).

:- pred insert_foreign_proc_output_registers(list(foreign_proc_output)::in,
    set(int)::in, set(int)::out) is det.

insert_foreign_proc_output_registers([], !Used).
insert_foreign_proc_output_registers([Output | Outputs], !Used) :-
    Output = foreign_proc_output(Lval, _, _, _, _, _, _),
    find_used_registers_lval(Lval, !Used),
    insert_foreign_proc_output_registers(Outputs, !Used).

%---------------------------------------------------------------------------%

    % Find all the labels defined in an instruction sequence.
    %
:- pred find_labels(list(instruction)::in, list(label)::out) is det.

find_labels(Instrs, Labels) :-
    find_labels_acc(Instrs, [], Labels).

:- pred find_labels_acc(list(instruction)::in,
    list(label)::in, list(label)::out) is det.

find_labels_acc([], !Labels).
find_labels_acc([Instr | Instrs], !Labels) :-
    Instr = llds_instr(Uinstr, _),
    ( if Uinstr = label(Label) then
        !:Labels = [Label | !.Labels]
    else if Uinstr = block(_, _, Block) then
        find_labels_acc(Block, !Labels)
    else
        true
    ),
    find_labels_acc(Instrs, !Labels).

%---------------------------------------------------------------------------%
:- end_module ll_backend.middle_rec.
%---------------------------------------------------------------------------%

