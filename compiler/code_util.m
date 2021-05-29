%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: code_util.m.
%
% Various utilities routines for code generation and recognition of builtins.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.code_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.
:- import_module libs.optimization_options.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type for_from_where
    --->    for_from_everywhere
    ;       for_from_proc(maybe_use_just_one_c_func, pred_id, proc_id).

    % Create a code address which holds the address of the specified procedure.
    % The `for_from_where' argument should be `for_from_everywhere'
    % if the caller wants the returned address to be valid from everywhere
    % in the program. If being valid from within a specified procedure
    % (the current procedure) is enough, this argument should be
    % `for_from_proc' wrapped around the value of the use_just_one_c_func
    % option and the current procedure id. Using an address that is
    % only valid from within the current procedure may make jumps faster.
    %
    % Note that we cannot take advantage of situations in which the caller
    % and the callee are in the same C module even though use_just_one_c_func
    % is not set, because what procedures end up in the same C module
    % is decided only long after the LLDS has been constructed.
    %
:- func make_entry_label(module_info, pred_id, proc_id, for_from_where)
    = code_addr.

:- func make_entry_label_from_rtti(rtti_proc_label, for_from_where)
    = code_addr.

    % Create a label which holds the address of the specified procedure,
    % which must be defined in the current module (procedures that are
    % imported from other modules have representations only as code_addrs,
    % not as labels, since their address is not known at C compilation time).
    % The fourth argument has the same meaning as for make_entry_label.
    %
:- func make_local_entry_label(module_info, pred_id, proc_id, for_from_where)
    = label.

    % Create a label internal to a Mercury procedure.
    %
:- func make_internal_label(module_info, pred_id, proc_id, int) = label.

:- func extract_proc_label_from_code_addr(code_addr) = proc_label.

:- pred arg_loc_to_register(arg_loc::in, lval::out) is det.

:- pred max_mentioned_regs(list(lval)::in, int::out, int::out) is det.
:- pred max_mentioned_abs_regs(list(abs_locn)::in, int::out, int::out) is det.

:- pred goal_may_alloc_temp_frame(hlds_goal::in, bool::out) is det.

    % Negate a condition.
    % This is used mostly just to make the generated code more readable.
    %
:- pred neg_rval(rval::in, rval::out) is det.

:- pred negate_the_test(list(instruction)::in, list(instruction)::out) is det.

    % These predicates return the set of lvals referenced in an rval
    % and an lval respectively. Lvals referenced indirectly through
    % lvals of the form var(_) are not counted.
    %
:- func lvals_in_lvals(list(lval)) = list(lval).
:- func lvals_in_rval(rval) = list(lval).
:- func lvals_in_lval(lval) = list(lval).

    % Given a procedure that already has its arg_info field filled in,
    % return a list giving its input variables and their initial locations.
    %
:- pred build_input_arg_list(proc_info::in, assoc_list(prog_var, lval)::out)
    is det.

    % Encode the number of regular register and float register arguments
    % into a single word. This representation is in both the MR_Closure
    % num_hidden_args_rf field, and for the input to do_call_closure et al.
    %
:- func encode_num_generic_call_vars(int, int) = int.

:- func size_of_cell_args(list(cell_arg)) = int.

    % Determine all the rvals and lvals referenced by an instruction.
    %
:- pred instr_rvals_and_lvals(instr::in, set(rval)::out, set(lval)::out)
    is det.

:- pred instrs_rvals_and_lvals(list(instruction)::in, set(rval)::out,
    set(lval)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module hlds.code_model.

:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

make_entry_label(ModuleInfo, PredId, ProcId, ForFromWhere) = ProcAddr :-
    RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    ProcAddr = make_entry_label_from_rtti(RttiProcLabel, ForFromWhere).

make_entry_label_from_rtti(RttiProcLabel, ForFromWhere) = ProcAddr :-
    ProcIsImported = RttiProcLabel ^ rpl_proc_is_imported,
    (
        ProcIsImported = yes,
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        ProcAddr = code_imported_proc(ProcLabel)
    ;
        ProcIsImported = no,
        Label = make_local_entry_label_from_rtti(RttiProcLabel, ForFromWhere),
        ProcAddr = code_label(Label)
    ).

make_local_entry_label(ModuleInfo, PredId, ProcId, ForFromWhere) = Label :-
    RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
    Label = make_local_entry_label_from_rtti(RttiProcLabel, ForFromWhere).

:- func make_local_entry_label_from_rtti(rtti_proc_label, for_from_where)
    = label.

make_local_entry_label_from_rtti(RttiProcLabel, ForFromWhere) = Label :-
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    (
        ForFromWhere = for_from_everywhere,
        % If we want to define the label or use it to put it into a data
        % structure, a label that is usable only within the current C module
        % won't do.
        ProcIsExported = RttiProcLabel ^ rpl_proc_is_exported,
        (
            ProcIsExported = yes,
            EntryType = entry_label_exported
        ;
            ProcIsExported = no,
            EntryType = entry_label_local
        ),
        Label = entry_label(EntryType, ProcLabel)
    ;
        ForFromWhere = for_from_proc(MaybeUseJustOneFunc,
            CurPredId, CurProcId),
        Label = choose_local_label_type(MaybeUseJustOneFunc,
            CurPredId, CurProcId,
            RttiProcLabel ^ rpl_pred_id, RttiProcLabel ^ rpl_proc_id,
            ProcLabel)
    ).

:- func choose_local_label_type(maybe_use_just_one_c_func,
    pred_id, proc_id, pred_id, proc_id, proc_label) = label.

choose_local_label_type(MaybeUseJustOneFunc, CurPredId, CurProcId,
        PredId, ProcId, ProcLabel) = Label :-
    ( if
        % If we want to branch to the label now, we prefer a form that is
        % usable only within the current C module, since it is likely to be
        % faster.
        (
            MaybeUseJustOneFunc = use_just_one_c_func
        ;
            PredId = CurPredId,
            ProcId = CurProcId
        )
    then
        EntryType = entry_label_c_local
    else
        EntryType = entry_label_local
    ),
    Label = entry_label(EntryType, ProcLabel).

%-----------------------------------------------------------------------------%

make_internal_label(ModuleInfo, PredId, ProcId, LabelNum) = Label :-
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    Label = internal_label(LabelNum, ProcLabel).

extract_proc_label_from_code_addr(CodeAddr) = ProcLabel :-
    ( if CodeAddr = code_label(Label) then
        ProcLabel = get_proc_label(Label)
    else if CodeAddr = code_imported_proc(ProcLabelPrime) then
        ProcLabel = ProcLabelPrime
    else
        unexpected($pred, "failed")
    ).

%-----------------------------------------------------------------------------%

arg_loc_to_register(reg(RegType, N), reg(RegType, N)).

%-----------------------------------------------------------------------------%

max_mentioned_regs(Lvals, MaxRegR, MaxRegF) :-
    max_mentioned_reg_2(Lvals, 0, MaxRegR, 0, MaxRegF).

:- pred max_mentioned_reg_2(list(lval)::in, int::in, int::out,
    int::in, int::out) is det.

max_mentioned_reg_2([], !MaxRegR, !MaxRegF).
max_mentioned_reg_2([Lval | Lvals], !MaxRegR, !MaxRegF) :-
    ( if Lval = reg(RegType, N) then
        (
            RegType = reg_r,
            int.max(N, !MaxRegR)
        ;
            RegType = reg_f,
            int.max(N, !MaxRegF)
        )
    else
        true
    ),
    max_mentioned_reg_2(Lvals, !MaxRegR, !MaxRegF).

max_mentioned_abs_regs(Lvals, MaxRegR, MaxRegF) :-
    max_mentioned_abs_reg_2(Lvals, 0, MaxRegR, 0, MaxRegF).

:- pred max_mentioned_abs_reg_2(list(abs_locn)::in,
    int::in, int::out, int::in, int::out) is det.

max_mentioned_abs_reg_2([], !MaxRegR, !MaxRegF).
max_mentioned_abs_reg_2([Lval | Lvals], !MaxRegR, !MaxRegF) :-
    ( if Lval = abs_reg(RegType, N) then
        (
            RegType = reg_r,
            int.max(N, !MaxRegR)
        ;
            RegType = reg_f,
            int.max(N, !MaxRegF)
        )
    else
        true
    ),
    max_mentioned_abs_reg_2(Lvals, !MaxRegR, !MaxRegF).

%-----------------------------------------------------------------------------%

goal_may_alloc_temp_frame(hlds_goal(GoalExpr, _GoalInfo), May) :-
    goal_expr_may_alloc_temp_frame(GoalExpr, May).

:- pred goal_expr_may_alloc_temp_frame(hlds_goal_expr::in, bool::out) is det.

goal_expr_may_alloc_temp_frame(GoalExpr, May) :-
    (
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ),
        May = no
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        % We cannot safely say that a foreign code fragment does not allocate
        % temporary nondet frames without knowing all the #defined macros
        % that expand to mktempframe and variants thereof. The performance
        % impact of being too conservative is probably not too bad.
        May = yes
    ;
        GoalExpr = scope(_, SubGoal),
        SubGoal = hlds_goal(_, SubGoalInfo),
        SubCodeModel = goal_info_get_code_model(SubGoalInfo),
        (
            SubCodeModel = model_non,
            May = yes
        ;
            ( SubCodeModel = model_det
            ; SubCodeModel = model_semi
            ),
            goal_may_alloc_temp_frame(SubGoal, May)
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_may_alloc_temp_frame(SubGoal, May)
    ;
        ( GoalExpr = conj(_ConjType, SubGoals)
        ; GoalExpr = disj(SubGoals)
        ),
        goal_list_may_alloc_temp_frame(SubGoals, May)
    ;
        GoalExpr = switch(_Var, _Det, Cases),
        cases_may_alloc_temp_frame(Cases, May)
    ;
        GoalExpr = if_then_else(_Vars, C, T, E),
        ( if goal_may_alloc_temp_frame(C, yes) then
            May = yes
        else if goal_may_alloc_temp_frame(T, yes) then
            May = yes
        else
            goal_may_alloc_temp_frame(E, May)
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred goal_list_may_alloc_temp_frame(list(hlds_goal)::in, bool::out) is det.

goal_list_may_alloc_temp_frame([], no).
goal_list_may_alloc_temp_frame([Goal | Goals], May) :-
    ( if goal_may_alloc_temp_frame(Goal, yes) then
        May = yes
    else
        goal_list_may_alloc_temp_frame(Goals, May)
    ).

:- pred cases_may_alloc_temp_frame(list(case)::in, bool::out) is det.

cases_may_alloc_temp_frame([], no).
cases_may_alloc_temp_frame([case(_, _, Goal) | Cases], May) :-
    ( if goal_may_alloc_temp_frame(Goal, yes) then
        May = yes
    else
        cases_may_alloc_temp_frame(Cases, May)
    ).

%-----------------------------------------------------------------------------%

neg_rval(Rval, NegRval) :-
    ( if natural_neg_rval(Rval, NegRval0) then
        NegRval = NegRval0
    else
        NegRval = unop(logical_not, Rval)
    ).

:- pred natural_neg_rval(rval::in, rval::out) is semidet.

natural_neg_rval(const(Const), const(NegConst)) :-
    (
        Const = llconst_true,
        NegConst = llconst_false
    ;
        Const = llconst_false,
        NegConst = llconst_true
    ).
natural_neg_rval(unop(logical_not, Rval), Rval).
natural_neg_rval(binop(Op, X, Y), binop(NegOp, X, Y)) :-
    neg_op(Op, NegOp).

:- pred neg_op(binary_op::in, binary_op::out) is semidet.

neg_op(eq(T), ne(T)).
neg_op(ne(T), eq(T)).
neg_op(int_lt(T), int_ge(T)).
neg_op(int_le(T), int_gt(T)).
neg_op(int_gt(T), int_le(T)).
neg_op(int_ge(T), int_lt(T)).
neg_op(str_eq, str_ne).
neg_op(str_ne, str_eq).
neg_op(str_lt, str_ge).
neg_op(str_le, str_gt).
neg_op(str_gt, str_le).
neg_op(str_ge, str_lt).
neg_op(float_eq, float_ne).
neg_op(float_ne, float_eq).
neg_op(float_lt, float_ge).
neg_op(float_le, float_gt).
neg_op(float_gt, float_le).
neg_op(float_ge, float_lt).

negate_the_test([], _) :-
    unexpected($pred, "empty list").
negate_the_test([Instr0 | Instrs0], Instrs) :-
    ( if Instr0 = llds_instr(if_val(Test, Target), Comment) then
        neg_rval(Test, NewTest),
        Instrs = [llds_instr(if_val(NewTest, Target), Comment)]
    else
        negate_the_test(Instrs0, Instrs1),
        Instrs = [Instr0 | Instrs1]
    ).

%-----------------------------------------------------------------------------%

lvals_in_lvals([]) = [].
lvals_in_lvals([First | Rest]) = FirstLvals ++ RestLvals :-
    FirstLvals = lvals_in_lval(First),
    RestLvals = lvals_in_lvals(Rest).

lvals_in_rval(lval(Lval)) = [Lval | lvals_in_lval(Lval)].
lvals_in_rval(var(_)) = [].
lvals_in_rval(mkword(_, Rval)) = lvals_in_rval(Rval).
lvals_in_rval(mkword_hole(_)) = [].
lvals_in_rval(const(_)) = [].
lvals_in_rval(cast(_, Rval)) = lvals_in_rval(Rval).
lvals_in_rval(unop(_, Rval)) = lvals_in_rval(Rval).
lvals_in_rval(binop(_, Rval1, Rval2)) =
    lvals_in_rval(Rval1) ++ lvals_in_rval(Rval2).
lvals_in_rval(mem_addr(MemRef)) = lvals_in_mem_ref(MemRef).

lvals_in_lval(reg(_, _)) = [].
lvals_in_lval(stackvar(_)) = [].
lvals_in_lval(parent_stackvar(_)) = [].
lvals_in_lval(framevar(_)) = [].
lvals_in_lval(double_stackvar(_, _)) = [].
lvals_in_lval(succip) = [].
lvals_in_lval(maxfr) = [].
lvals_in_lval(curfr) = [].
lvals_in_lval(succip_slot(Rval)) = lvals_in_rval(Rval).
lvals_in_lval(redofr_slot(Rval)) = lvals_in_rval(Rval).
lvals_in_lval(redoip_slot(Rval)) = lvals_in_rval(Rval).
lvals_in_lval(succfr_slot(Rval)) = lvals_in_rval(Rval).
lvals_in_lval(prevfr_slot(Rval)) = lvals_in_rval(Rval).
lvals_in_lval(hp) = [].
lvals_in_lval(sp) = [].
lvals_in_lval(parent_sp) = [].
lvals_in_lval(field(_, Rval1, Rval2)) =
    lvals_in_rval(Rval1) ++ lvals_in_rval(Rval2).
lvals_in_lval(lvar(_)) = [].
lvals_in_lval(temp(_, _)) = [].
lvals_in_lval(mem_ref(Rval)) = lvals_in_rval(Rval).
lvals_in_lval(global_var_ref(_)) = [].

:- func lvals_in_mem_ref(mem_ref) = list(lval).

lvals_in_mem_ref(stackvar_ref(Rval)) = lvals_in_rval(Rval).
lvals_in_mem_ref(framevar_ref(Rval)) = lvals_in_rval(Rval).
lvals_in_mem_ref(heap_ref(Rval1, _, Rval2)) =
    lvals_in_rval(Rval1) ++ lvals_in_rval(Rval2).

%-----------------------------------------------------------------------------%

build_input_arg_list(ProcInfo, VarLvals) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_arg_info(ProcInfo, ArgInfos),
    assoc_list.from_corresponding_lists(HeadVars, ArgInfos, VarArgInfos),
    build_input_arg_list_2(VarArgInfos, VarLvals).

:- pred build_input_arg_list_2(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, lval)::out) is det.

build_input_arg_list_2([], []).
build_input_arg_list_2([V - Arg | Rest0], VarArgs) :-
    Arg = arg_info(Loc, Mode),
    (
        Mode = top_in,
        arg_loc_to_register(Loc, Reg),
        VarArgs = [V - Reg | VarArgs0]
    ;
        ( Mode = top_out
        ; Mode = top_unused
        ),
        VarArgs = VarArgs0
    ),
    build_input_arg_list_2(Rest0, VarArgs0).

%-----------------------------------------------------------------------------%

encode_num_generic_call_vars(NumR, NumF) = (NumR \/ (NumF << 16)).

%-----------------------------------------------------------------------------%

size_of_cell_args(CellArgs) = Size :-
    size_of_cell_args_acc(CellArgs, 0, Size).

:- pred size_of_cell_args_acc(list(cell_arg)::in, int::in, int::out) is det.

size_of_cell_args_acc([], !Size).
size_of_cell_args_acc([CellArg | CellArgs], !Size) :-
    (
        ( CellArg = cell_arg_full_word(_, _)
        ; CellArg = cell_arg_skip_one_word
        ; CellArg = cell_arg_take_addr_one_word(_, _)
        ),
        CellSize = 1
    ;
        ( CellArg = cell_arg_double_word(_)
        ; CellArg = cell_arg_skip_two_words
        ; CellArg = cell_arg_take_addr_two_words(_, _)
        ),
        CellSize = 2
    ),
    !:Size = !.Size + CellSize,
    size_of_cell_args_acc(CellArgs, !Size).

%-----------------------------------------------------------------------------%

instr_rvals_and_lvals(comment(_), set.init, set.init).
instr_rvals_and_lvals(livevals(_), set.init, set.init).
instr_rvals_and_lvals(block(_, _, Instrs), Rvals, Lvals) :-
    instrs_rvals_and_lvals(Instrs, Rvals, Lvals).
instr_rvals_and_lvals(assign(Lval,Rval), make_singleton_set(Rval),
    make_singleton_set(Lval)).
instr_rvals_and_lvals(keep_assign(Lval,Rval), make_singleton_set(Rval),
    make_singleton_set(Lval)).
instr_rvals_and_lvals(llcall(_, _, _, _, _, _), set.init, set.init).
instr_rvals_and_lvals(mkframe(_, _), set.init, set.init).
instr_rvals_and_lvals(label(_), set.init, set.init).
instr_rvals_and_lvals(goto(_), set.init, set.init).
instr_rvals_and_lvals(computed_goto(Rval, _), make_singleton_set(Rval),
    set.init).
instr_rvals_and_lvals(arbitrary_c_code(_, _, _), set.init, set.init).
instr_rvals_and_lvals(if_val(Rval, _), make_singleton_set(Rval), set.init).
instr_rvals_and_lvals(save_maxfr(Lval), set.init, make_singleton_set(Lval)).
instr_rvals_and_lvals(restore_maxfr(Lval), set.init, make_singleton_set(Lval)).
instr_rvals_and_lvals(incr_hp(Lval, _, _, SizeRval, _, _, MaybeRegionRval,
        MaybeReuse), Rvals, Lvals) :-
    some [!Rvals, !Lvals] (
        !:Rvals = make_singleton_set(SizeRval),
        !:Lvals = make_singleton_set(Lval),
        (
            MaybeRegionRval = yes(RegionRval),
            set.insert(RegionRval, !Rvals)
        ;
            MaybeRegionRval = no
        ),
        (
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            set.insert(ReuseRval, !Rvals),
            (
                MaybeFlagLval = yes(FlagLval),
                set.insert(FlagLval, !Lvals)
            ;
                MaybeFlagLval = no
            )
        ;
            MaybeReuse = no_llds_reuse
        ),
        Rvals = !.Rvals,
        Lvals = !.Lvals
    ).
instr_rvals_and_lvals(mark_hp(Lval), set.init, make_singleton_set(Lval)).
instr_rvals_and_lvals(restore_hp(Rval), make_singleton_set(Rval), set.init).
instr_rvals_and_lvals(free_heap(Rval), make_singleton_set(Rval), set.init).
    % The region instructions implicitly specify some stackvars or framevars,
    % but they cannot reference lvals or rvals that involve code addresses or
    % labels, and that is the motivation of the reason this code was originally
    % written.
    % More recently code generation for loop_control scopes uses this
    % predicate, but it is not likly to be used with rbmm.
instr_rvals_and_lvals(push_region_frame(_, _), set.init, set.init).
instr_rvals_and_lvals(region_fill_frame(_, _, IdRval, NumLval, AddrLval),
    make_singleton_set(IdRval), list_to_set([NumLval, AddrLval])).
instr_rvals_and_lvals(region_set_fixed_slot(_, _, ValueRval),
    make_singleton_set(ValueRval), set.init).
instr_rvals_and_lvals(use_and_maybe_pop_region_frame(_, _), set.init,
    set.init).
instr_rvals_and_lvals(store_ticket(Lval), set.init, make_singleton_set(Lval)).
instr_rvals_and_lvals(reset_ticket(Rval, _Reason), make_singleton_set(Rval),
    set.init).
instr_rvals_and_lvals(discard_ticket, set.init, set.init).
instr_rvals_and_lvals(prune_ticket, set.init, set.init).
instr_rvals_and_lvals(mark_ticket_stack(Lval), set.init,
    make_singleton_set(Lval)).
instr_rvals_and_lvals(prune_tickets_to(Rval), make_singleton_set(Rval),
    set.init).
instr_rvals_and_lvals(incr_sp(_, _, _), set.init, set.init).
instr_rvals_and_lvals(decr_sp(_), set.init, set.init).
instr_rvals_and_lvals(decr_sp_and_return(_), set.init, set.init).
instr_rvals_and_lvals(foreign_proc_code(_, Cs, _, _, _, _, _, _, _, _),
        list_to_set(Rvals), list_to_set(Lvals)) :-
    foreign_proc_components_get_rvals_and_lvals(Cs, Rvals, Lvals).
instr_rvals_and_lvals(init_sync_term(Lval, _, _), set.init,
    make_singleton_set(Lval)).
instr_rvals_and_lvals(fork_new_child(Lval, _), set.init,
    make_singleton_set(Lval)).
instr_rvals_and_lvals(join_and_continue(Lval, _), set.init,
    make_singleton_set(Lval)).
instr_rvals_and_lvals(lc_create_loop_control(_, Lval), set.init,
    make_singleton_set(Lval)).
instr_rvals_and_lvals(lc_wait_free_slot(Rval, Lval, _),
    make_singleton_set(Rval), make_singleton_set(Lval)).
instr_rvals_and_lvals(lc_spawn_off(LCRval, LCSRval, _),
    list_to_set([LCRval, LCSRval]), set.init).
instr_rvals_and_lvals(lc_join_and_terminate(LCRval, LCSRval),
    list_to_set([LCRval, LCSRval]), set.init).

    % Determine all the rvals and lvals referenced by a list of instructions.
    %
instrs_rvals_and_lvals(Instrs, Rvals, Lvals) :-
    foldl2(instrs_rvals_and_lvals_acc, Instrs, set.init, Rvals,
        set.init, Lvals).

:- pred instrs_rvals_and_lvals_acc(instruction::in,
    set(rval)::in, set(rval)::out, set(lval)::in, set(lval)::out) is det.

instrs_rvals_and_lvals_acc(llds_instr(Uinstr, _), !Rvals, !Lvals) :-
    instr_rvals_and_lvals(Uinstr, NewRvals, NewLvals),
    % The accumulator is the first argument since that suits the performance
    % charicteristics of set.union.
    set.union(!.Rvals, NewRvals, !:Rvals),
    set.union(!.Lvals, NewLvals, !:Lvals).

    % Extract the rvals and lvals from the foreign_proc_components.
    %
:- pred foreign_proc_components_get_rvals_and_lvals(
    list(foreign_proc_component)::in,
    list(rval)::out, list(lval)::out) is det.

foreign_proc_components_get_rvals_and_lvals([], [], []).
foreign_proc_components_get_rvals_and_lvals([Comp | Comps],
        !:Rvals, !:Lvals) :-
    foreign_proc_components_get_rvals_and_lvals(Comps, !:Rvals, !:Lvals),
    foreign_proc_component_get_rvals_and_lvals(Comp, !Rvals, !Lvals).

    % Extract the rvals and lvals from the foreign_proc_component
    % and add them to the list.
    %
:- pred foreign_proc_component_get_rvals_and_lvals(foreign_proc_component::in,
    list(rval)::in, list(rval)::out, list(lval)::in, list(lval)::out) is det.

foreign_proc_component_get_rvals_and_lvals(foreign_proc_inputs(Inputs),
        !Rvals, !Lvals) :-
    NewRvals = foreign_proc_inputs_get_rvals(Inputs),
    list.append(NewRvals, !Rvals).
foreign_proc_component_get_rvals_and_lvals(foreign_proc_outputs(Outputs),
        !Rvals, !Lvals) :-
    NewLvals = foreign_proc_outputs_get_lvals(Outputs),
    list.append(NewLvals, !Lvals).
foreign_proc_component_get_rvals_and_lvals(foreign_proc_user_code(_, _, _),
        !Rvals, !Lvals).
foreign_proc_component_get_rvals_and_lvals(foreign_proc_raw_code(_, _, _, _),
        !Rvals, !Lvals).
foreign_proc_component_get_rvals_and_lvals(foreign_proc_fail_to(_),
        !Rvals, !Lvals).
foreign_proc_component_get_rvals_and_lvals(foreign_proc_alloc_id(_),
        !Rvals, !Lvals).
foreign_proc_component_get_rvals_and_lvals(foreign_proc_noop,
        !Rvals, !Lvals).

    % Extract the rvals from the foreign_proc_input.
    %
:- func foreign_proc_inputs_get_rvals(list(foreign_proc_input)) = list(rval).

foreign_proc_inputs_get_rvals([]) = [].
foreign_proc_inputs_get_rvals([Input | Inputs]) = [Rval | Rvals] :-
    Input = foreign_proc_input(_Name, _VarType, _IsDummy, _OrigType, Rval,
        _, _),
    Rvals = foreign_proc_inputs_get_rvals(Inputs).

    % Extract the lvals from the foreign_proc_output.
    %
:- func foreign_proc_outputs_get_lvals(list(foreign_proc_output)) = list(lval).

foreign_proc_outputs_get_lvals([]) = [].
foreign_proc_outputs_get_lvals([Output | Outputs]) = [Lval | Lvals] :-
    Output = foreign_proc_output(Lval, _VarType, _IsDummy, _OrigType,
        _Name, _, _),
    Lvals = foreign_proc_outputs_get_lvals(Outputs).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.code_util.
%-----------------------------------------------------------------------------%
