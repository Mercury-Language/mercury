%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod_decide.m.
% Main author: stayl (the original intermod.m).
%
% This module contains code to decide what entities we want to put into
% .opt files.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod_decide.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.intermod_info.

    % Decide what to output to a module's .opt file.
    %
:- pred decide_what_to_opt_export(module_info::in, intermod_info::out) is det.

:- pred should_opt_export_type_defn(module_name::in, type_ctor::in,
    hlds_type_defn::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_promise.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.
:- import_module transform_hlds.inlining.
:- import_module transform_hlds.intermod_status.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type intermod_params
    --->    intermod_params(
                ip_maybe_process_local_preds    :: maybe_process_local_preds,
                ip_maybe_collect_types          :: maybe_collect_types,
                ip_maybe_deforest               :: maybe_deforest,
                ip_inline_simple_threshold      :: int,
                ip_higher_order_size_limit      :: int
            ).

:- type maybe_collect_types
    --->    do_not_collect_types
    ;       do_collect_types.

:- type maybe_process_local_preds
    --->    do_not_process_local_preds
    ;       do_process_local_preds.

decide_what_to_opt_export(ModuleInfo, !:IntermodInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    InlineSimpleThreshold = OptTuple ^ ot_intermod_inline_simple_threshold,
    HigherOrderSizeLimit = OptTuple ^ ot_higher_order_size_limit,
    Deforest = OptTuple ^ ot_deforest,

    module_info_get_valid_pred_ids(ModuleInfo, RealPredIds),
    module_info_get_assertion_table(ModuleInfo, AssertionTable),
    assertion_table_pred_ids(AssertionTable, AssertPredIds),
    PredIds = AssertPredIds ++ RealPredIds,

    Params = intermod_params(do_not_process_local_preds, do_collect_types,
        Deforest, InlineSimpleThreshold, HigherOrderSizeLimit),

    init_intermod_info(ModuleInfo, !:IntermodInfo),
    gather_opt_export_preds(Params, PredIds, !IntermodInfo),
    gather_opt_export_instances(!IntermodInfo),
    gather_opt_export_types(!IntermodInfo).

%---------------------------------------------------------------------------%

:- pred gather_opt_export_preds(intermod_params::in, list(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_preds(Params0, AllPredIds, !IntermodInfo) :-
    % First gather exported preds.
    gather_opt_export_preds_in_list(Params0, AllPredIds, !IntermodInfo),

    % Then gather preds used by exported preds (recursively).
    Params = Params0 ^ ip_maybe_process_local_preds := do_process_local_preds,
    set.init(ExtraExportedPreds0),
    gather_opt_export_preds_fixpoint(Params, ExtraExportedPreds0,
        !IntermodInfo).

:- pred gather_opt_export_preds_fixpoint(intermod_params::in, set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_preds_fixpoint(Params, ExtraExportedPreds0, !IntermodInfo) :-
    intermod_info_get_pred_decls(!.IntermodInfo, ExtraExportedPreds),
    NewlyExportedPreds = set.to_sorted_list(
        set.difference(ExtraExportedPreds, ExtraExportedPreds0)),
    (
        NewlyExportedPreds = []
    ;
        NewlyExportedPreds = [_ | _],
        gather_opt_export_preds_in_list(Params, NewlyExportedPreds,
            !IntermodInfo),
        gather_opt_export_preds_fixpoint(Params, ExtraExportedPreds,
            !IntermodInfo)
    ).

:- pred gather_opt_export_preds_in_list(intermod_params::in, list(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_preds_in_list(_, [], !IntermodInfo).
gather_opt_export_preds_in_list(Params, [PredId | PredIds], !IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    ( if
        clauses_info_get_explicit_vartypes(ClausesInfo, ExplicitVarTypes),
        vartypes_is_empty(ExplicitVarTypes),
        should_opt_export_pred(ModuleInfo, PredId, PredInfo,
            Params, TypeSpecForcePreds)
    then
        SavedIntermodInfo = !.IntermodInfo,
        % Write a declaration to the `.opt' file for
        % `exported_to_submodules' predicates.
        intermod_add_pred(PredId, MayOptExportPred0, !IntermodInfo),
        clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
        (
            MayOptExportPred0 = may_opt_export_pred,
            get_clause_list_for_replacement(ClausesRep, Clauses),
            gather_entities_to_opt_export_in_clauses(Clauses,
                MayOptExportPred, !IntermodInfo)
        ;
            MayOptExportPred0 = may_not_opt_export_pred,
            MayOptExportPred = may_not_opt_export_pred
        ),
        (
            MayOptExportPred = may_opt_export_pred,
            ( if pred_info_defn_has_foreign_proc(PredInfo) then
                % The foreign code of this predicate may refer to entities
                % in the foreign language that are defined in a foreign module
                % that is imported by a foreign_import_module declaration.
                intermod_info_set_need_foreign_import_modules(!IntermodInfo)
            else
                true
            ),
            intermod_info_get_pred_defns(!.IntermodInfo, PredDefns0),
            set.insert(PredId, PredDefns0, PredDefns),
            intermod_info_set_pred_defns(PredDefns, !IntermodInfo)
        ;
            MayOptExportPred = may_not_opt_export_pred,
            % Remove any items added for the clauses for this predicate.
            !:IntermodInfo = SavedIntermodInfo
        )
    else
        true
    ),
    gather_opt_export_preds_in_list(Params, PredIds, !IntermodInfo).

:- pred should_opt_export_pred(module_info::in, pred_id::in, pred_info::in,
    intermod_params::in, set(pred_id)::in) is semidet.

should_opt_export_pred(ModuleInfo, PredId, PredInfo,
        Params, TypeSpecForcePreds) :-
    ProcessLocalPreds = Params ^ ip_maybe_process_local_preds,
    (
        ProcessLocalPreds = do_not_process_local_preds,
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_exported_to_submodules(PredInfo)
        )
    ;
        ProcessLocalPreds = do_process_local_preds,
        pred_info_get_status(PredInfo, pred_status(status_local))
    ),
    (
        % Allow all promises to be opt-exported.
        % (may_opt_export_pred should succeed for all promises.)
        pred_info_is_promise(PredInfo, _)
    ;
        may_opt_export_pred(PredId, PredInfo, TypeSpecForcePreds),
        opt_exporting_pred_is_likely_worthwhile(Params, ModuleInfo,
            PredId, PredInfo)
    ).

:- pred opt_exporting_pred_is_likely_worthwhile(intermod_params::in,
    module_info::in, pred_id::in, pred_info::in) is semidet.

opt_exporting_pred_is_likely_worthwhile(Params, ModuleInfo,
        PredId, PredInfo) :-
    pred_info_get_clauses_info(PredInfo, ClauseInfo),
    clauses_info_get_clauses_rep(ClauseInfo, ClausesRep, _ItemNumbers),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),
    % At this point, the goal size includes some dummy unifications
    % HeadVar1 = X, HeadVar2 = Y, etc. which will be optimized away
    % later. To account for this, we add the arity to the size thresholds.
    pred_info_get_orig_arity(PredInfo, pred_form_arity(Arity)),
    (
        inlining.is_simple_clause_list(Clauses,
            Params ^ ip_inline_simple_threshold + Arity)
    ;
        pred_info_requested_inlining(PredInfo)
    ;
        % Mutable access preds should always be included in .opt files.
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_mutable_access_pred)
    ;
        pred_has_a_higher_order_input_arg(ModuleInfo, PredInfo),
        clause_list_size(Clauses, GoalSize),
        GoalSize =< Params ^ ip_higher_order_size_limit + Arity
    ;
        Params ^ ip_maybe_deforest = deforest,
        % Double the inline-threshold since goals we want to deforest
        % will have at least two disjuncts. This allows one simple goal
        % in each disjunct. The disjunction adds one to the goal size,
        % hence the `+1'.
        DeforestThreshold = (Params ^ ip_inline_simple_threshold * 2) + 1,
        inlining.is_simple_clause_list(Clauses, DeforestThreshold + Arity),
        clause_list_is_deforestable(PredId, Clauses)
    ).

:- pred may_opt_export_pred(pred_id::in, pred_info::in, set(pred_id)::in)
    is semidet.

may_opt_export_pred(PredId, PredInfo, TypeSpecForcePreds) :-
    % Predicates with `class_method' markers contain class_method_call
    % goals which cannot be written to `.opt' files (they cannot be read
    % back in). They will be recreated in the importing module.
    pred_info_get_markers(PredInfo, Markers),
    not check_marker(Markers, marker_class_method),
    not check_marker(Markers, marker_class_instance_method),

    % Don't write stub clauses to `.opt' files.
    not check_marker(Markers, marker_stub),

    % Don't export builtins, since they will be recreated in the
    % importing module anyway.
    not is_unify_index_or_compare_pred(PredInfo),
    not pred_info_is_builtin(PredInfo),

    % These will be recreated in the importing module.
    not set.member(PredId, TypeSpecForcePreds),

    % Don't export non-inlinable predicates.
    not check_marker(Markers, marker_user_marked_no_inline),
    not check_marker(Markers, marker_mmc_marked_no_inline),

    % Don't export tabled predicates, since they are not inlinable.
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.values(ProcTable, ProcInfos),
    list.all_true(proc_eval_method_is_normal, ProcInfos).

:- pred proc_eval_method_is_normal(proc_info::in) is semidet.

proc_eval_method_is_normal(ProcInfo) :-
    proc_info_get_eval_method(ProcInfo, eval_normal).

:- pred gather_entities_to_opt_export_in_clauses(list(clause)::in,
    may_opt_export_pred::out, intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_clauses([], may_opt_export_pred,
        !IntermodInfo).
gather_entities_to_opt_export_in_clauses([Clause | Clauses], MayOptExportPred,
        !IntermodInfo) :-
    gather_entities_to_opt_export_in_goal(Clause ^ clause_body,
        MayOptExportPred1, !IntermodInfo),
    (
        MayOptExportPred1 = may_opt_export_pred,
        gather_entities_to_opt_export_in_clauses(Clauses,
            MayOptExportPred, !IntermodInfo)
    ;
        MayOptExportPred1 = may_not_opt_export_pred,
        MayOptExportPred = may_not_opt_export_pred
    ).

:- pred pred_has_a_higher_order_input_arg(module_info::in, pred_info::in)
    is semidet.

pred_has_a_higher_order_input_arg(ModuleInfo, PredInfo) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.values(ProcTable, ProcInfos),
    list.find_first_match(proc_has_a_higher_order_input_arg(ModuleInfo),
        ProcInfos, _FirstProcInfoWithHoInput).

:- pred proc_has_a_higher_order_input_arg(module_info::in, proc_info::in)
    is semidet.

proc_has_a_higher_order_input_arg(ModuleInfo, ProcInfo) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_var_table(ProcInfo, VarTable),
    some_input_arg_is_higher_order(ModuleInfo, VarTable, HeadVars, ArgModes).

:- pred some_input_arg_is_higher_order(module_info::in, var_table::in,
    list(prog_var)::in, list(mer_mode)::in) is semidet.

some_input_arg_is_higher_order(ModuleInfo, VarTable,
        [HeadVar | HeadVars], [ArgMode | ArgModes]) :-
    ( if
        mode_is_input(ModuleInfo, ArgMode),
        lookup_var_type(VarTable, HeadVar, Type),
        classify_type(ModuleInfo, Type) = ctor_cat_higher_order
    then
        true
    else
        some_input_arg_is_higher_order(ModuleInfo, VarTable,
            HeadVars, ArgModes)
    ).

    % Rough guess: a goal is deforestable if it contains a single
    % top-level branched goal and is recursive.
    %
:- pred clause_list_is_deforestable(pred_id::in, list(clause)::in) is semidet.

clause_list_is_deforestable(PredId, Clauses)  :-
    some [Clause1] (
        list.member(Clause1, Clauses),
        Goal1 = Clause1 ^ clause_body,
        goal_calls_pred_id(Goal1, PredId)
    ),
    (
        Clauses = [_, _ | _]
    ;
        Clauses = [Clause2],
        Goal2 = Clause2 ^ clause_body,
        goal_to_conj_list(Goal2, GoalList),
        goal_contains_one_branched_goal(GoalList)
    ).

:- pred goal_contains_one_branched_goal(list(hlds_goal)::in) is semidet.

goal_contains_one_branched_goal(GoalList) :-
    goal_contains_one_branched_goal_acc(GoalList, no).

:- pred goal_contains_one_branched_goal_acc(list(hlds_goal)::in, bool::in)
    is semidet.

goal_contains_one_branched_goal_acc([], yes).
goal_contains_one_branched_goal_acc([Goal | Goals], FoundBranch0) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        goal_is_branched(GoalExpr),
        FoundBranch0 = no,
        FoundBranch = yes
    ;
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals,
        FoundBranch = FoundBranch0
    ),
    goal_contains_one_branched_goal_acc(Goals, FoundBranch).

    % Go over the goal of an exported proc looking for proc decls, types,
    % insts and modes that we need to write to the optfile.
    %
:- pred gather_entities_to_opt_export_in_goal(hlds_goal::in,
    may_opt_export_pred::out,
    intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_goal(Goal, MayOptExportPred, !IntermodInfo) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    gather_entities_to_opt_export_in_goal_expr(GoalExpr, MayOptExportPred,
        !IntermodInfo).

:- pred gather_entities_to_opt_export_in_goal_expr(hlds_goal_expr::in,
    may_opt_export_pred::out, intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_goal_expr(GoalExpr, MayOptExportPred,
        !IntermodInfo) :-
    (
        GoalExpr = unify(_LHSVar, RHS, _Mode, _Kind, _UnifyContext),
        % Export declarations for preds used in higher order pred constants
        % or function calls.
        gather_entities_to_opt_export_in_unify_rhs(RHS, MayOptExportPred,
            !IntermodInfo)
    ;
        GoalExpr = plain_call(PredId, _, _, _, _, _),
        % Ensure that the called predicate will be exported.
        intermod_add_pred(PredId, MayOptExportPred, !IntermodInfo)
    ;
        GoalExpr = generic_call(CallType, _, _, _, _),
        (
            CallType = higher_order(_, _, _, _),
            MayOptExportPred = may_opt_export_pred
        ;
            CallType = class_method(_, _, _, _),
            MayOptExportPred = may_not_opt_export_pred
        ;
            CallType = event_call(_),
            MayOptExportPred = may_not_opt_export_pred
        ;
            CallType = cast(CastType),
            (
                ( CastType = unsafe_type_cast
                ; CastType = unsafe_type_inst_cast
                ; CastType = equiv_type_cast
                ; CastType = exists_cast
                ),
                MayOptExportPred = may_not_opt_export_pred
            ;
                CastType = subtype_coerce,
                MayOptExportPred = may_opt_export_pred
            )
        )
    ;
        GoalExpr = call_foreign_proc(Attrs, _, _, _, _, _, _),
        % Inlineable exported pragma_foreign_code goals cannot use any
        % non-exported types, so we just write out the clauses.
        MaybeMayDuplicate = get_may_duplicate(Attrs),
        MaybeMayExportBody = get_may_export_body(Attrs),
        ( if
            ( MaybeMayDuplicate = yes(proc_may_not_duplicate)
            ; MaybeMayExportBody = yes(proc_may_not_export_body)
            )
        then
            MayOptExportPred = may_not_opt_export_pred
        else
            MayOptExportPred = may_opt_export_pred
        )
    ;
        GoalExpr = conj(_ConjType, Goals),
        gather_entities_to_opt_export_in_goals(Goals, MayOptExportPred,
            !IntermodInfo)
    ;
        GoalExpr = disj(Goals),
        gather_entities_to_opt_export_in_goals(Goals, MayOptExportPred,
            !IntermodInfo)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        gather_entities_to_opt_export_in_cases(Cases, MayOptExportPred,
            !IntermodInfo)
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        gather_entities_to_opt_export_in_goal(Cond, MayOptExportPredCond,
            !IntermodInfo),
        gather_entities_to_opt_export_in_goal(Then, MayOptExportPredThen,
            !IntermodInfo),
        gather_entities_to_opt_export_in_goal(Else, MayOptExportPredElse,
            !IntermodInfo),
        ( if
            MayOptExportPredCond = may_opt_export_pred,
            MayOptExportPredThen = may_opt_export_pred,
            MayOptExportPredElse = may_opt_export_pred
        then
            MayOptExportPred = may_opt_export_pred
        else
            MayOptExportPred = may_not_opt_export_pred
        )
    ;
        GoalExpr = negation(SubGoal),
        gather_entities_to_opt_export_in_goal(SubGoal, MayOptExportPred,
            !IntermodInfo)
    ;
        GoalExpr = scope(_Reason, SubGoal),
        % Mode analysis hasn't been run yet, so we don't know yet whether
        % from_ground_term_construct scopes actually satisfy their invariants,
        % specifically the invariant that say they contain no calls or
        % higher-order constants. We therefore cannot special-case them here.
        %
        % XXX Actually it wouldn't be hard to arrange to get this code to run
        % *after* mode analysis.
        gather_entities_to_opt_export_in_goal(SubGoal, MayOptExportPred,
            !IntermodInfo)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            gather_entities_to_opt_export_in_goal(MainGoal,
                MayOptExportPredMain, !IntermodInfo),
            gather_entities_to_opt_export_in_goals(OrElseGoals,
                MayOptExportPredOrElse, !IntermodInfo),
            ( if
                MayOptExportPredMain = may_opt_export_pred,
                MayOptExportPredOrElse = may_opt_export_pred
            then
                MayOptExportPred = may_opt_export_pred
            else
                MayOptExportPred = may_not_opt_export_pred
            )
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, _SubGoal),
            % hlds_out_goal.m does not write out `try' goals properly.
            MayOptExportPred = may_not_opt_export_pred
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

:- pred gather_entities_to_opt_export_in_goals(list(hlds_goal)::in,
    may_opt_export_pred::out,
    intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_goals([], may_opt_export_pred, !IntermodInfo).
gather_entities_to_opt_export_in_goals([Goal | Goals], !:MayOptExportPred,
        !IntermodInfo) :-
    gather_entities_to_opt_export_in_goal(Goal, !:MayOptExportPred,
        !IntermodInfo),
    (
        !.MayOptExportPred = may_opt_export_pred,
        gather_entities_to_opt_export_in_goals(Goals, !:MayOptExportPred,
            !IntermodInfo)
    ;
        !.MayOptExportPred = may_not_opt_export_pred
    ).

:- pred gather_entities_to_opt_export_in_cases(list(case)::in,
    may_opt_export_pred::out,
    intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_cases([], may_opt_export_pred, !IntermodInfo).
gather_entities_to_opt_export_in_cases([Case | Cases], !:MayOptExportPred,
        !IntermodInfo) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    gather_entities_to_opt_export_in_goal(Goal, !:MayOptExportPred,
        !IntermodInfo),
    (
        !.MayOptExportPred = may_opt_export_pred,
        gather_entities_to_opt_export_in_cases(Cases, !:MayOptExportPred,
            !IntermodInfo)
    ;
        !.MayOptExportPred = may_not_opt_export_pred
    ).

%---------------------------------------------------------------------------%

:- type may_opt_export_pred
    --->    may_not_opt_export_pred
    ;       may_opt_export_pred.

    % intermod_add_pred/4 tries to do what ever is necessary to ensure that the
    % specified predicate will be exported, so that it can be called from
    % clauses in the `.opt' file. If it can't, then it returns
    % MayOptExportPred = may_not_opt_export_pred,
    % which will prevent the caller from being included in the `.opt' file.
    %
    % If a proc called within an exported proc is local, we need to add
    % a declaration for the called proc to the .opt file. If a proc called
    % within an exported proc is from a different module, we need to include
    % an `:- import_module' declaration to import that module in the `.opt'
    % file.
    %
:- pred intermod_add_pred(pred_id::in, may_opt_export_pred::out,
    intermod_info::in, intermod_info::out) is det.

intermod_add_pred(PredId, MayOptExportPred, !IntermodInfo) :-
    ( if PredId = invalid_pred_id then
        % This will happen for type class instance methods defined using
        % the clause syntax. Currently we cannot handle intermodule
        % optimization of those.
        MayOptExportPred = may_not_opt_export_pred
    else
        intermod_do_add_pred(PredId, MayOptExportPred, !IntermodInfo)
    ).

:- pred intermod_do_add_pred(pred_id::in, may_opt_export_pred::out,
    intermod_info::in, intermod_info::out) is det.

intermod_do_add_pred(PredId, MayOptExportPred, !IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    pred_info_get_markers(PredInfo, Markers),
    ( if
        % Calling compiler-generated procedures is fine; we don't need
        % to output declarations for them to the `.opt' file, since they
        % will be recreated every time anyway. We don't want declarations
        % for predicates representing promises either.

        ( is_unify_index_or_compare_pred(PredInfo)
        ; pred_info_is_promise(PredInfo, _)
        )
    then
        MayOptExportPred = may_opt_export_pred
    else if
        % Don't write the caller to the `.opt' file if it calls a pred
        % without mode or determinism decls, because then we would need
        % to include the mode decls for the callee in the `.opt' file and
        % (since writing the `.opt' file happens before mode inference)
        % we can't do that because we don't know what the modes are.
        %
        % XXX This prevents intermodule optimizations in such cases,
        % which is a pity.
        %
        % XXX Actually it wouldn't be hard to arrange to get this code to run
        % *after* mode analysis, so this restriction is likely to be
        % unnecessary.
        (
            check_marker(Markers, marker_infer_modes)
        ;
            pred_info_get_proc_table(PredInfo, Procs),
            ProcIds = pred_info_all_procids(PredInfo),
            list.member(ProcId, ProcIds),
            map.lookup(Procs, ProcId, ProcInfo),
            proc_info_get_declared_determinism(ProcInfo, no)
        )
    then
        MayOptExportPred = may_not_opt_export_pred
    else if
        % Goals which call impure predicates cannot be written due to
        % limitations in mode analysis. The problem is that only head
        % unifications are allowed to be reordered with impure goals.
        % For example,
        %
        %   p(A::in, B::in, C::out) :- impure foo(A, B, C).
        %
        % becomes
        %
        %   p(HeadVar1, HeadVar2, HeadVar3) :-
        %       A = HeadVar1, B = HeadVar2, C = HeadVar3,
        %       impure foo(A, B, C).
        %
        % In the clauses written to `.opt' files, the head unifications
        % are already expanded, and are expanded again when the `.opt' file
        % is read in. The `C = HeadVar3' unification cannot be reordered
        % with the impure goal, resulting in a mode error. Fixing this
        % in mode analysis would be tricky.
        % See tests/valid/impure_intermod.m.
        %
        % NOTE: the above restriction applies to user predicates.
        % For compiler generated mutable access predicates, we can ensure
        % that reordering is not necessary by construction, so it is safe
        % to include them in .opt files.

        pred_info_get_purity(PredInfo, purity_impure),
        not check_marker(Markers, marker_mutable_access_pred)
    then
        MayOptExportPred = may_not_opt_export_pred
    else if
        % If a pred whose code we are going to put in the .opt file calls
        % a predicate which is exported, then we do not need to do anything
        % special.

        (
            PredStatus = pred_status(status_exported)
        ;
            PredStatus = pred_status(status_external(OldExternalStatus)),
            old_status_is_exported(OldExternalStatus) = yes
        )
    then
        MayOptExportPred = may_opt_export_pred
    else if
        % Declarations for class methods will be recreated from the class
        % declaration in the `.opt' file. Declarations for local classes
        % are always written to the `.opt' file.

        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, marker_class_method)
    then
        MayOptExportPred = may_opt_export_pred
    else if
        % If a pred whose code we are going to put in the `.opt' file calls
        % a predicate which is local to that module, then we need to put
        % the declaration for the called predicate in the `.opt' file.

        pred_status_to_write(PredStatus) = yes
    then
        MayOptExportPred = may_opt_export_pred,
        intermod_info_get_pred_decls(!.IntermodInfo, PredDecls0),
        set.insert(PredId, PredDecls0, PredDecls),
        intermod_info_set_pred_decls(PredDecls, !IntermodInfo)
    else if
        ( PredStatus = pred_status(status_imported(_))
        ; PredStatus = pred_status(status_opt_imported)
        )
    then
        % Imported pred - add import for module.

        MayOptExportPred = may_opt_export_pred,
        PredModule = pred_info_module(PredInfo),
        intermod_info_get_use_modules(!.IntermodInfo, Modules0),
        set.insert(PredModule, Modules0, Modules),
        intermod_info_set_use_modules(Modules, !IntermodInfo)
    else
        unexpected($pred, "unexpected status")
    ).

    % Resolve overloading and module qualify everything in a unify_rhs.
    % Fully module-qualify the right-hand-side of a unification.
    % For function calls and higher-order terms, call intermod_add_pred
    % so that the predicate or function will be exported if necessary.
    %
:- pred gather_entities_to_opt_export_in_unify_rhs(unify_rhs::in,
    may_opt_export_pred::out,
    intermod_info::in, intermod_info::out) is det.

gather_entities_to_opt_export_in_unify_rhs(RHS, MayOptExportPred,
        !IntermodInfo) :-
    (
        RHS = rhs_var(_),
        MayOptExportPred = may_opt_export_pred
    ;
        RHS = rhs_lambda_goal(_Purity, _HOGroundness, _PorF, _EvalMethod,
            _NonLocals, _ArgVarsModes, _Detism, Goal),
        gather_entities_to_opt_export_in_goal(Goal, MayOptExportPred,
            !IntermodInfo)
    ;
        RHS = rhs_functor(Functor, _Exist, _Vars),
        % Is this a higher-order predicate or higher-order function term?
        ( if Functor = closure_cons(ShroudedPredProcId, _) then
            % Yes, the unification creates a higher-order term.
            % Make sure that the predicate/function is exported.
            proc(PredId, _) = unshroud_pred_proc_id(ShroudedPredProcId),
            intermod_add_pred(PredId, MayOptExportPred, !IntermodInfo)
        else
            % It is an ordinary constructor, or a constant of a builtin type,
            % so just leave it alone.
            %
            % Function calls and higher-order function applications
            % are transformed into ordinary calls and higher-order calls
            % by post_typecheck.m, so they cannot occur here.
            MayOptExportPred = may_opt_export_pred
        )
    ).

%---------------------------------------------------------------------------%

:- pred gather_opt_export_instances(intermod_info::in, intermod_info::out)
    is det.

gather_opt_export_instances(!IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_get_instance_table(ModuleInfo, Instances),
    map.foldl(gather_opt_export_instances_in_class(ModuleInfo), Instances,
        !IntermodInfo).

:- pred gather_opt_export_instances_in_class(module_info::in,
    class_id::in, list(hlds_instance_defn)::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_instances_in_class(ModuleInfo, ClassId, InstanceDefns,
        !IntermodInfo) :-
    list.foldl(
        gather_opt_export_instance_in_instance_defn(ModuleInfo, ClassId),
        InstanceDefns, !IntermodInfo).

:- pred gather_opt_export_instance_in_instance_defn(module_info::in,
    class_id::in, hlds_instance_defn::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_instance_in_instance_defn(ModuleInfo, ClassId, InstanceDefn,
        !IntermodInfo) :-
    InstanceDefn = hlds_instance_defn(ModuleName, InstanceStatus,
        TVarSet, OriginalTypes, Types,
        InstanceConstraints, MaybeSubsumedContext, Proofs,
        InstanceBody0, MaybeMethodInfos, Context),
    DefinedThisModule = instance_status_defined_in_this_module(InstanceStatus),
    (
        DefinedThisModule = yes,
        % The bodies are always stripped from instance declarations
        % before writing them to *.int* files, so the full instance
        % declaration should be written to the .opt file even for
        % exported instances, if this is possible.
        SavedIntermodInfo = !.IntermodInfo,
        (
            InstanceBody0 = instance_body_concrete(Methods0),
            (
                MaybeMethodInfos = yes(MethodInfos)
            ;
                MaybeMethodInfos = no,
                unexpected($pred, "method infos not filled in")
            ),
            AddMethodInfoToMap =
                ( pred(MI::in, Map0::in, Map::out) is det :-
                    MethodName = MI ^ method_pred_name,
                    proc(PredId, _) = MI ^ method_orig_proc,
                    ( if map.insert(MethodName, PredId,  Map0, Map1) then
                        Map = Map1
                    else
                        Map = Map0
                    )
                ),
            list.foldl(AddMethodInfoToMap, MethodInfos, map.init,
                MethodNameToPredIdMap),
            list.map_foldl(
                intermod_qualify_instance_method(ModuleInfo,
                    MethodNameToPredIdMap),
                Methods0, Methods, [], PredIds),
            list.map_foldl(intermod_add_pred, PredIds, MethodMayOptExportPreds,
                !IntermodInfo),
            ( if
                list.all_true(unify(may_opt_export_pred),
                    MethodMayOptExportPreds)
            then
                InstanceBody = instance_body_concrete(Methods)
            else
                % Write an abstract instance declaration if any of the methods
                % cannot be written to the `.opt' file for any reason.
                InstanceBody = instance_body_abstract,

                % Do not write declarations for any of the methods if one
                % cannot be written.
                !:IntermodInfo = SavedIntermodInfo
            )
        ;
            InstanceBody0 = instance_body_abstract,
            InstanceBody = InstanceBody0
        ),
        ( if
            % Don't write an abstract instance declaration
            % if the declaration is already in the `.int' file.
            (
                InstanceBody = instance_body_abstract
            =>
                instance_status_is_exported(InstanceStatus) = no
            )
        then
            InstanceDefnToWrite = hlds_instance_defn(ModuleName,
                InstanceStatus, TVarSet, OriginalTypes, Types,
                InstanceConstraints, MaybeSubsumedContext, Proofs,
                InstanceBody, MaybeMethodInfos, Context),
            intermod_info_get_instances(!.IntermodInfo, Instances0),
            Instances = [ClassId - InstanceDefnToWrite | Instances0],
            intermod_info_set_instances(Instances, !IntermodInfo)
        else
            true
        )
    ;
        DefinedThisModule = no
    ).

    % Resolve overloading of instance methods before writing them
    % to the `.opt' file.
    %
:- pred intermod_qualify_instance_method(module_info::in,
    map(pred_pf_name_arity, pred_id)::in,
    instance_method::in, instance_method::out,
    list(pred_id)::in, list(pred_id)::out) is det.

intermod_qualify_instance_method(ModuleInfo, MethodNameToPredIdMap,
        InstanceMethod0, InstanceMethod, PredIds0, PredIds) :-
    InstanceMethod0 = instance_method(MethodName, InstanceMethodDefn0,
        MethodContext),
    MethodName =
        pred_pf_name_arity(PredOrFunc, _MethodSymName, MethodUserArity),
    map.lookup(MethodNameToPredIdMap, MethodName, MethodPredId),
    module_info_pred_info(ModuleInfo, MethodPredId, MethodPredInfo),
    pred_info_get_arg_types(MethodPredInfo, MethodTVarSet,
        MethodExistQTVars, MethodArgTypes),
    pred_info_get_external_type_params(MethodPredInfo,
        MethodExternalTypeParams),
    (
        InstanceMethodDefn0 = instance_proc_def_name(InstanceMethodName0),
        PredOrFunc = pf_function,
        ( if
            find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
                MethodUserArity, MethodTVarSet, MethodExistQTVars,
                MethodArgTypes, MethodExternalTypeParams,
                MethodContext, MaybePredId, InstanceMethodName)
        then
            (
                MaybePredId = yes(PredId),
                PredIds = [PredId | PredIds0]
            ;
                MaybePredId = no,
                PredIds = PredIds0
            ),
            InstanceMethodDefn = instance_proc_def_name(InstanceMethodName)
        else
            % This will force intermod_add_pred to return
            % MayOptExportPred = may_not_opt_export_pred.
            PredId = invalid_pred_id,
            PredIds = [PredId | PredIds0],

            % We can just leave the method definition unchanged.
            InstanceMethodDefn = InstanceMethodDefn0
        )
    ;
        InstanceMethodDefn0 = instance_proc_def_name(InstanceMethodName0),
        PredOrFunc = pf_predicate,
        init_markers(Markers),
        resolve_pred_overloading(ModuleInfo, Markers, MethodTVarSet,
            MethodExistQTVars, MethodArgTypes,
            MethodExternalTypeParams, MethodContext,
            InstanceMethodName0, InstanceMethodName, PredId, _ResolveSpecs),
        % Any errors in _ResolveSpecs will be reported when a later compiler
        % invocation attempts to generate target language code for this module.
        PredIds = [PredId | PredIds0],
        InstanceMethodDefn = instance_proc_def_name(InstanceMethodName)
    ;
        InstanceMethodDefn0 = instance_proc_def_clauses(_ItemList),
        % XXX For methods defined using this syntax it is a little tricky
        % to write out the .opt files, so for now I've just disabled
        % intermodule optimization for type class instance declarations
        % using the new syntax.
        %
        % This will force intermod_add_pred to return
        % MayOptExportPred = may_not_opt_export_pred.
        PredId = invalid_pred_id,
        PredIds = [PredId | PredIds0],
        % We can just leave the method definition unchanged.
        InstanceMethodDefn = InstanceMethodDefn0
    ),
    InstanceMethod = instance_method(MethodName, InstanceMethodDefn,
        MethodContext).

    % A `func(x/n) is y' method implementation can match an ordinary function,
    % a field access function or a constructor. For now, if there are multiple
    % possible matches, we don't write the instance method.
    %
:- pred find_func_matching_instance_method(module_info::in, sym_name::in,
    user_arity::in, tvarset::in, existq_tvars::in, list(mer_type)::in,
    external_type_params::in, prog_context::in, maybe(pred_id)::out,
    sym_name::out) is semidet.

find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
        MethodUserArity, MethodCallTVarSet, MethodCallExistQTVars,
        MethodCallArgTypes, MethodCallExternalTypeParams, MethodContext,
        MaybePredId, InstanceMethodName) :-
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    MethodUserArity = user_arity(MethodUserArityInt),
    ( if
        % XXX ARITY is_field_access_function_name can take user_arity
        % XXX ARITY is_field_access_function_name can return FieldDefns
        is_field_access_function_name(ModuleInfo, InstanceMethodName0,
            MethodUserArityInt, _, FieldName),
        map.search(CtorFieldTable, FieldName, FieldDefns)
    then
        TypeCtors0 = list.map(
            ( func(FieldDefn) = TypeCtor :-
                FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, _, _)
            ), FieldDefns)
    else
        TypeCtors0 = []
    ),
    module_info_get_cons_table(ModuleInfo, Ctors),
    ( if
        ConsId = cons(InstanceMethodName0, MethodUserArityInt,
            cons_id_dummy_type_ctor),
        search_cons_table(Ctors, ConsId, MatchingConstructors)
    then
        TypeCtors1 = list.map(
            ( func(ConsDefn) = TypeCtor :-
                ConsDefn ^ cons_type_ctor = TypeCtor
            ), MatchingConstructors)
    else
        TypeCtors1 = []
    ),
    TypeCtors = TypeCtors0 ++ TypeCtors1,

    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    predicate_table_lookup_func_sym_arity(PredicateTable,
        may_be_partially_qualified, InstanceMethodName0, MethodUserArity,
        PredIds),
    ( if
        PredIds = [_ | _],
        find_matching_pred_id(ModuleInfo, PredIds, MethodCallTVarSet,
            MethodCallExistQTVars, MethodCallArgTypes,
            MethodCallExternalTypeParams, no, MethodContext,
            PredId, InstanceMethodFuncName, _ResolveSpecs)
        % Any errors in _ResolveSpecs will be reported when a later compiler
        % invocation attempts to generate target language code for this module.
    then
        TypeCtors = [],
        MaybePredId = yes(PredId),
        InstanceMethodName = InstanceMethodFuncName
    else
        TypeCtors = [TheTypeCtor],
        MaybePredId = no,
        TheTypeCtor = type_ctor(TypeCtorSymName, _),
        (
            TypeCtorSymName = qualified(TypeModule, _),
            UnqualMethodName = unqualify_name(InstanceMethodName0),
            InstanceMethodName = qualified(TypeModule, UnqualMethodName)
        ;
            TypeCtorSymName = unqualified(_),
            unexpected($pred, "unqualified type_ctor in " ++
                "hlds_cons_defn or hlds_ctor_field_defn")
        )
    ).

%---------------------------------------------------------------------------%

:- pred gather_opt_export_types(intermod_info::in, intermod_info::out) is det.

gather_opt_export_types(!IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_get_type_table(ModuleInfo, TypeTable),
    foldl_over_type_ctor_defns(gather_opt_export_types_in_type_defn, TypeTable,
        !IntermodInfo).

:- pred gather_opt_export_types_in_type_defn(type_ctor::in, hlds_type_defn::in,
    intermod_info::in, intermod_info::out) is det.

gather_opt_export_types_in_type_defn(TypeCtor, TypeDefn0, !IntermodInfo) :-
    intermod_info_get_module_info(!.IntermodInfo, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    ( if should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn0) then
        hlds_data.get_type_defn_body(TypeDefn0, TypeBody0),
        (
            TypeBody0 = hlds_du_type(TypeBodyDu0),
            TypeBodyDu0 = type_body_du(Ctors, MaybeSuperType, MaybeUserEqComp0,
                MaybeRepn, MaybeForeign0),
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_target(Globals, Target),

            % Note that we don't resolve overloading for the definitions
            % which won't be used on this back-end, because their unification
            % and comparison predicates have not been typechecked. They are
            % only written to the `.opt' it can be handy when building
            % against a workspace for the other definitions to be present
            % (e.g. when testing compiling a module to IL when the workspace
            % was compiled to C).
            % XXX The above sentence doesn't make sense, and never did
            % (even in the first CVS version in which it appears).

            ( if
                MaybeForeign0 = yes(ForeignTypeBody0),
                have_foreign_type_for_backend(Target, ForeignTypeBody0, yes)
            then
                % The foreign type may be defined in one of the foreign
                % modules we import.
                intermod_info_set_need_foreign_import_modules(!IntermodInfo),
                resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
                    ForeignTypeBody0, ForeignTypeBody, !IntermodInfo),
                MaybeForeign = yes(ForeignTypeBody),
                MaybeUserEqComp = MaybeUserEqComp0
            else
                resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
                    MaybeUserEqComp0, MaybeUserEqComp, !IntermodInfo),
                MaybeForeign = MaybeForeign0
            ),
            TypeBodyDu = type_body_du(Ctors, MaybeSuperType, MaybeUserEqComp,
                MaybeRepn, MaybeForeign),
            TypeBody = hlds_du_type(TypeBodyDu),
            hlds_data.set_type_defn_body(TypeBody, TypeDefn0, TypeDefn)
        ;
            TypeBody0 = hlds_foreign_type(ForeignTypeBody0),
            % The foreign type may be defined in one of the foreign
            % modules we import.
            intermod_info_set_need_foreign_import_modules(!IntermodInfo),
            resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
                ForeignTypeBody0, ForeignTypeBody, !IntermodInfo),
            TypeBody = hlds_foreign_type(ForeignTypeBody),
            hlds_data.set_type_defn_body(TypeBody, TypeDefn0, TypeDefn)
        ;
            ( TypeBody0 = hlds_eqv_type(_)
            ; TypeBody0 = hlds_solver_type(_)
            ; TypeBody0 = hlds_abstract_type(_)
            ),
            TypeDefn = TypeDefn0
        ),
        intermod_info_get_types(!.IntermodInfo, Types0),
        intermod_info_set_types([TypeCtor - TypeDefn | Types0], !IntermodInfo)
    else
        true
    ).

:- pred resolve_foreign_type_body_overloading(module_info::in,
    type_ctor::in, foreign_type_body::in, foreign_type_body::out,
    intermod_info::in, intermod_info::out) is det.

resolve_foreign_type_body_overloading(ModuleInfo, TypeCtor,
        ForeignTypeBody0, ForeignTypeBody, !IntermodInfo) :-
    ForeignTypeBody0 = foreign_type_body(MaybeC0, MaybeJava0, MaybeCSharp0),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),

    % Note that we don't resolve overloading for the foreign definitions
    % which won't be used on this back-end, because their unification and
    % comparison predicates have not been typechecked. They are only written
    % to the `.opt' it can be handy when building against a workspace
    % for the other definitions to be present (e.g. when testing compiling
    % a module to IL when the workspace was compiled to C).

    (
        Target = target_c,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeC0, MaybeC, !IntermodInfo)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ),
        MaybeC = MaybeC0
    ),
    (
        Target = target_csharp,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeCSharp0, MaybeCSharp, !IntermodInfo)
    ;
        ( Target = target_c
        ; Target = target_java
        ),
        MaybeCSharp = MaybeCSharp0
    ),
    (
        Target = target_java,
        resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
            MaybeJava0, MaybeJava, !IntermodInfo)
    ;
        ( Target = target_c
        ; Target = target_csharp
        ),
        MaybeJava = MaybeJava0
    ),
    ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCSharp).

:- pred resolve_foreign_type_body_overloading_2(module_info::in, type_ctor::in,
    foreign_type_lang_body(T)::in, foreign_type_lang_body(T)::out,
    intermod_info::in, intermod_info::out) is det.

resolve_foreign_type_body_overloading_2(ModuleInfo, TypeCtor,
        MaybeForeignTypeLangData0, MaybeForeignTypeLangData, !IntermodInfo) :-
    (
        MaybeForeignTypeLangData0 = no,
        MaybeForeignTypeLangData = no
    ;
        MaybeForeignTypeLangData0 =
            yes(type_details_foreign(Body, MaybeUserEqComp0, Assertions)),
        resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
            MaybeUserEqComp0, MaybeUserEqComp, !IntermodInfo),
        MaybeForeignTypeLangData =
            yes(type_details_foreign(Body, MaybeUserEqComp, Assertions))
    ).

:- pred resolve_unify_compare_overloading(module_info::in,
    type_ctor::in, maybe_canonical::in, maybe_canonical::out,
    intermod_info::in, intermod_info::out) is det.

resolve_unify_compare_overloading(ModuleInfo, TypeCtor,
        MaybeCanonical0, MaybeCanonical, !IntermodInfo) :-
    (
        MaybeCanonical0 = canon,
        MaybeCanonical = MaybeCanonical0
    ;
        MaybeCanonical0 = noncanon(NonCanonical0),
        (
            ( NonCanonical0 = noncanon_abstract(_IsSolverType)
            ; NonCanonical0 = noncanon_subtype
            ),
            MaybeCanonical = MaybeCanonical0
        ;
            NonCanonical0 = noncanon_uni_cmp(Uni0, Cmp0),
            resolve_user_special_pred_overloading(ModuleInfo,
                spec_pred_unify, TypeCtor, Uni0, Uni, !IntermodInfo),
            resolve_user_special_pred_overloading(ModuleInfo,
                spec_pred_compare, TypeCtor, Cmp0, Cmp, !IntermodInfo),
            NonCanonical = noncanon_uni_cmp(Uni, Cmp),
            MaybeCanonical = noncanon(NonCanonical)
        ;
            NonCanonical0 = noncanon_uni_only(Uni0),
            resolve_user_special_pred_overloading(ModuleInfo,
                spec_pred_unify, TypeCtor, Uni0, Uni, !IntermodInfo),
            NonCanonical = noncanon_uni_only(Uni),
            MaybeCanonical = noncanon(NonCanonical)
        ;
            NonCanonical0 = noncanon_cmp_only(Cmp0),
            resolve_user_special_pred_overloading(ModuleInfo,
                spec_pred_compare, TypeCtor, Cmp0, Cmp, !IntermodInfo),
            NonCanonical = noncanon_cmp_only(Cmp),
            MaybeCanonical = noncanon(NonCanonical)
        )
    ).

:- pred resolve_user_special_pred_overloading(module_info::in,
    special_pred_id::in, type_ctor::in, sym_name::in, sym_name::out,
    intermod_info::in, intermod_info::out) is det.

resolve_user_special_pred_overloading(ModuleInfo, SpecialId,
        TypeCtor, Pred0, Pred, !IntermodInfo) :-
    module_info_get_special_pred_maps(ModuleInfo, SpecialPredMaps),
    lookup_special_pred_maps(SpecialPredMaps, SpecialId, TypeCtor,
        SpecialPredId),
    module_info_pred_info(ModuleInfo, SpecialPredId, SpecialPredInfo),
    pred_info_get_arg_types(SpecialPredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_external_type_params(SpecialPredInfo, ExternalTypeParams),
    init_markers(Markers0),
    add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
    pred_info_get_context(SpecialPredInfo, Context),
    resolve_pred_overloading(ModuleInfo, Markers, TVarSet, ExistQVars,
        ArgTypes, ExternalTypeParams, Context, Pred0, Pred, UserEqPredId,
        _ResolveSpecs),
    % Any errors in _ResolveSpecs will be reported when a later compiler
    % invocation attempts to generate target language code for this module.
    intermod_add_pred(UserEqPredId, _, !IntermodInfo).

%---------------------------------------------------------------------------%

should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    TypeCtor = type_ctor(Name, _Arity),
    Name = qualified(ModuleName, _),
    type_status_to_write(TypeStatus) = yes.

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod_decide.
%---------------------------------------------------------------------------%
