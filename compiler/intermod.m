%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: intermod.m.
% Main author: stayl (the original intermod.m).
%
% This module writes out the first half of .opt files, which we use
% to implement inter-module optimization. The second half is written out
% by intermod_analysis.m.
%
% The first half of the .opt file includes:
%   - The clauses for exported preds that can be inlined.
%   - The clauses for exported preds that have higher-order pred arguments.
%   - The pred/mode declarations for local predicates that the
%     above clauses use.
%   - pragma declarations for the exported preds.
%   - Non-exported types, insts and modes used by the above.
%   - Pragma foreign_enum, or foreign_type declarations for
%     any types output due to the line above.
%   - :- import_module declarations to import stuff used by the above.
%   - pragma foreign_import_module declarations if any pragma foreign_proc
%     preds are written.
% All these items should be module qualified.
%
% Note that predicates which call predicates that do not have mode or
% determinism declarations do not have clauses exported, since this would
% require running mode analysis and determinism analysis before writing the
% .opt file, significantly increasing compile time for a very small gain.
%
% This module also contains predicates to adjust the import status
% of local predicates which are exported for intermodule optimization.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_item.

:- import_module io.

%---------------------------------------------------------------------------%

    % A value of this type specifies the set of entities we opt-export
    % from a module.
    %
:- type intermod_info.

    % Open the file "<module-name>.opt.tmp", and write out the declarations
    % and clauses for intermodule optimization.
    %
    % Although this predicate creates the .opt.tmp file, it does not
    % necessarily create it in its final form. Later compiler passes
    % may append to this file using the append_analysis_pragmas_to_opt_file
    % predicate in intermod_analysis.m.
    % XXX This is not an elegant arrangement.
    %
    % Update_interface and touch_interface_datestamp are called from
    % mercury_compile_front_end.m, since they must be called after
    % the last time anything is appended to the .opt.tmp file.
    %
:- pred write_initial_opt_file(io.output_stream::in, module_info::in,
    intermod_info::out, parse_tree_plain_opt::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Find out which predicates would be opt-exported, and mark them
    % accordingly. (See the comment on do_maybe_opt_export_entities
    % for why we do this.)
    %
:- pred maybe_opt_export_entities(module_info::in, module_info::out) is det.

    % Change the status of the entities (predicates, types, insts, modes,
    % classes and instances) listed as opt-exported in the given intermod_info
    % to opt-exported. This affects how the rest of the compiler treats
    % these entities. For example, the entry labels at the starts of
    % the C code fragments we generate for an opt-exported local predicate
    % needs to be exported from the .c file, and opt-exported procedures
    % should not be touched by dead proc elimination.
    %
    % The reason why we have a separate pass for this, instead of changing
    % the status of an item to reflect the fact that it is opt-exported
    % at the same time as we decide to opt-export it, is that the decision
    % to opt-export e.g. a procedure takes place inside invocations of
    % mmc --make-opt-int, but we also need the same status updates
    % in invocations of mmc that generate target language code.
    %
:- pred maybe_opt_export_listed_entities(intermod_info::in,
    module_info::in, module_info::out) is det.

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
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_promise.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.item_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.
:- import_module transform_hlds.inlining.
:- import_module transform_hlds.intermod_order_pred_info.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module one_or_more.
:- import_module one_or_more_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
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

%---------------------------------------------------------------------------%

write_initial_opt_file(TmpOptStream, ModuleInfo, IntermodInfo,
        ParseTreePlainOpt, !IO) :-
    decide_what_to_opt_export(ModuleInfo, IntermodInfo),
    write_opt_file_initial(TmpOptStream, IntermodInfo, ParseTreePlainOpt, !IO).

%---------------------------------------------------------------------------%
%
% Predicates to gather items to output to .opt file.
%

:- pred decide_what_to_opt_export(module_info::in, intermod_info::out) is det.

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
    Arity = pred_info_orig_arity(PredInfo),
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
    proc_info_get_var_table(ModuleInfo, ProcInfo, VarTable),
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
    goal_contains_one_branched_goal(GoalList, no).

:- pred goal_contains_one_branched_goal(list(hlds_goal)::in, bool::in)
    is semidet.

goal_contains_one_branched_goal([], yes).
goal_contains_one_branched_goal([Goal | Goals], FoundBranch0) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        goal_is_branched(GoalExpr),
        FoundBranch0 = no,
        FoundBranch = yes
    ;
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals,
        FoundBranch = FoundBranch0
    ),
    goal_contains_one_branched_goal(Goals, FoundBranch).

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
    InstanceDefn = hlds_instance_defn(ModuleName, Types, OriginalTypes,
        InstanceStatus, Context, MaybeSubsumedContext, InstanceConstraints,
        InstanceBody0, MaybePredProcIds, TVarSet, Proofs),
    DefinedThisModule = instance_status_defined_in_this_module(InstanceStatus),
    (
        DefinedThisModule = yes,
        % The bodies are always stripped from instance declarations
        % before writing them to `int' files, so the full instance
        % declaration should be written even for exported instances.
        SavedIntermodInfo = !.IntermodInfo,
        (
            InstanceBody0 = instance_body_concrete(Methods0),
            (
                MaybePredProcIds = yes(ClassProcs),
                ClassPreds0 =
                    list.map(pred_proc_id_project_pred_id, ClassProcs),

                % The interface is sorted on pred_id.
                list.remove_adjacent_dups(ClassPreds0, ClassPreds),
                assoc_list.from_corresponding_lists(ClassPreds, Methods0,
                    MethodAL)
            ;
                MaybePredProcIds = no,
                unexpected($pred, "method pred_proc_ids not filled in")
            ),
            list.map_foldl(qualify_instance_method(ModuleInfo),
                MethodAL, Methods, [], PredIds),
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
                Types, OriginalTypes, InstanceStatus,
                Context, MaybeSubsumedContext, InstanceConstraints,
                InstanceBody, MaybePredProcIds, TVarSet, Proofs),
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
:- pred qualify_instance_method(module_info::in,
    pair(pred_id, instance_method)::in, instance_method::out,
    list(pred_id)::in, list(pred_id)::out) is det.

qualify_instance_method(ModuleInfo, MethodCallPredId - InstanceMethod0,
        InstanceMethod, PredIds0, PredIds) :-
    module_info_pred_info(ModuleInfo, MethodCallPredId, MethodCallPredInfo),
    pred_info_get_arg_types(MethodCallPredInfo, MethodCallTVarSet,
        MethodCallExistQTVars, MethodCallArgTypes),
    pred_info_get_external_type_params(MethodCallPredInfo,
        MethodCallExternalTypeParams),
    InstanceMethod0 = instance_method(PredOrFunc, MethodSymName,
        MethodUserArity, InstanceMethodDefn0, MethodContext),
    (
        InstanceMethodDefn0 = instance_proc_def_name(InstanceMethodName0),
        PredOrFunc = pf_function,
        ( if
            find_func_matching_instance_method(ModuleInfo, InstanceMethodName0,
                MethodUserArity, MethodCallTVarSet, MethodCallExistQTVars,
                MethodCallArgTypes, MethodCallExternalTypeParams,
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
        resolve_pred_overloading(ModuleInfo, Markers, MethodCallTVarSet,
            MethodCallExistQTVars, MethodCallArgTypes,
            MethodCallExternalTypeParams, MethodContext,
            InstanceMethodName0, InstanceMethodName, PredId),
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
    InstanceMethod = instance_method(PredOrFunc, MethodSymName,
        MethodUserArity, InstanceMethodDefn, MethodContext).

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
            PredId, InstanceMethodFuncName)
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
        ArgTypes, ExternalTypeParams, Context, Pred0, Pred, UserEqPredId),
    intermod_add_pred(UserEqPredId, _, !IntermodInfo).

:- pred should_opt_export_type_defn(module_name::in, type_ctor::in,
    hlds_type_defn::in) is semidet.

should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    TypeCtor = type_ctor(Name, _Arity),
    Name = qualified(ModuleName, _),
    type_status_to_write(TypeStatus) = yes.

%---------------------------------------------------------------------------%

    % Output module imports, types, modes, insts and predicates.
    %
:- pred write_opt_file_initial(io.text_output_stream::in,
    intermod_info::in, parse_tree_plain_opt::out, io::di, io::uo) is det.

write_opt_file_initial(Stream, IntermodInfo, ParseTreePlainOpt, !IO) :-
    intermod_info_get_module_info(IntermodInfo, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    io.format(Stream, ":- module %s.\n", [s(ModuleNameStr)], !IO),

    intermod_info_get_pred_decls(IntermodInfo, PredDecls),
    intermod_info_get_pred_defns(IntermodInfo, PredDefns),
    intermod_info_get_instances(IntermodInfo, Instances),
    ( if
        % If none of these item types need writing, nothing else
        % needs to be written.

        set.is_empty(PredDecls),
        set.is_empty(PredDefns),
        Instances = [],
        module_info_get_type_table(ModuleInfo, TypeTable),
        get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
        some_type_needs_to_be_written(TypeCtorsDefns, no)
    then
        ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, term.context_init,
            map.init, set.init, [], [], [], [], [], [], [], [], [], [], [], [],
            [], [], [], [], [], [], [], [], [])
    else
        write_opt_file_initial_body(Stream, IntermodInfo, ParseTreePlainOpt,
            !IO)
    ).

:- pred some_type_needs_to_be_written(
    assoc_list(type_ctor, hlds_type_defn)::in, bool::out) is det.

some_type_needs_to_be_written([], no).
some_type_needs_to_be_written([_ - TypeDefn | TypeCtorDefns], NeedWrite) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    ( if
        ( TypeStatus = type_status(status_abstract_exported)
        ; TypeStatus = type_status(status_exported_to_submodules)
        )
    then
        NeedWrite = yes
    else
        some_type_needs_to_be_written(TypeCtorDefns, NeedWrite)
    ).

:- pred write_opt_file_initial_body(io.text_output_stream::in,
    intermod_info::in, parse_tree_plain_opt::out, io::di, io::uo) is det.

write_opt_file_initial_body(Stream, IntermodInfo, ParseTreePlainOpt, !IO) :-
    IntermodInfo = intermod_info(ModuleInfo, _,
        WriteDeclPredIdSet, WriteDefnPredIdSet,
        InstanceDefns, Types, NeedFIMs),
    set.to_sorted_list(WriteDeclPredIdSet, WriteDeclPredIds),
    set.to_sorted_list(WriteDefnPredIdSet, WriteDefnPredIds),

    module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
    % XXX CLEANUP We could and should reduce AvailModules to the set of modules
    % that are *actually needed* by the items being written.
    % XXX CLEANUP And even if builtin.m and/or private_builtin.m is needed
    % by an item, we *still* shouldn't include them, since the importing
    % module will import and use them respectively anyway.
    map.keys(AvailModuleMap, UsedModuleNames),
    AddToUseMap =
        ( pred(MN::in, UM0::in, UM::out) is det :-
            % We don't have a context for any use_module declaration
            % of this module (since it may have a import_module declaration
            % instead), which is why we specify a dummy context.
            % However, these contexts are used only when the .opt file
            % is read in, not when it is being generated.
            one_or_more_map.add(MN, term.dummy_context_init, UM0, UM)
        ),
    list.foldl(AddToUseMap, UsedModuleNames, one_or_more_map.init, UseMap),

    (
        NeedFIMs = do_need_foreign_import_modules,
        module_info_get_c_j_cs_fims(ModuleInfo, CJCsFIMs),
        FIMSpecsSet = get_all_fim_specs(CJCsFIMs),
        FIMSpecs = set.to_sorted_list(FIMSpecsSet)
    ;
        NeedFIMs = do_not_need_foreign_import_modules,
        set.init(FIMSpecsSet),
        FIMSpecs = []
    ),

    module_info_get_globals(ModuleInfo, Globals),
    OutInfo0 = init_hlds_out_info(Globals, output_mercury),

    % We don't want to write line numbers from the source file to .opt files,
    % because that causes spurious changes to the .opt files
    % when you make trivial changes (e.g. add comments) to the source files.
    MercInfo0 = OutInfo0 ^ hoi_merc_out_info,
    MercInfo = merc_out_info_disable_line_numbers(MercInfo0),
    OutInfo = OutInfo0 ^ hoi_merc_out_info := MercInfo,
    % Disable verbose dumping of clauses.
    OutInfoForPreds = OutInfo ^ hoi_dump_hlds_options := "",

    intermod_gather_types(Types, TypeDefns, ForeignEnums),
    intermod_gather_insts(ModuleInfo, InstDefns),
    intermod_gather_modes(ModuleInfo, ModeDefns),
    intermod_gather_classes(ModuleInfo, TypeClasses),
    intermod_gather_instances(InstanceDefns, Instances),

    list.foldl(mercury_output_module_decl(Stream, "use_module"),
        UsedModuleNames, !IO),
    maybe_write_block_start_blank_line(Stream, FIMSpecs, !IO),
    list.foldl(mercury_output_fim_spec(Stream), FIMSpecs, !IO),
    maybe_write_block_start_blank_line(Stream, TypeDefns, !IO),
    list.foldl(mercury_output_item_type_defn(MercInfo, Stream),
        TypeDefns, !IO),
    maybe_write_block_start_blank_line(Stream, ForeignEnums, !IO),
    list.foldl(mercury_format_item_foreign_enum(MercInfo, Stream),
        ForeignEnums, !IO),
    maybe_write_block_start_blank_line(Stream, InstDefns, !IO),
    list.foldl(mercury_output_item_inst_defn(MercInfo, Stream),
        InstDefns, !IO),
    maybe_write_block_start_blank_line(Stream, ModeDefns, !IO),
    list.foldl(mercury_output_item_mode_defn(MercInfo, Stream),
        ModeDefns, !IO),
    maybe_write_block_start_blank_line(Stream, TypeClasses, !IO),
    list.foldl(mercury_output_item_typeclass(MercInfo, Stream),
        TypeClasses, !IO),
    maybe_write_block_start_blank_line(Stream, Instances, !IO),
    list.foldl(mercury_output_item_instance(MercInfo, Stream),
        Instances, !IO),

    generate_order_pred_infos(ModuleInfo, WriteDeclPredIds,
        DeclOrderPredInfos),
    generate_order_pred_infos(ModuleInfo, WriteDefnPredIds,
        DefnOrderPredInfos),
    PredMarkerPragmasCord0 = cord.init,
    (
        DeclOrderPredInfos = [],
        PredDecls = [],
        ModeDecls = [],
        PredMarkerPragmasCord1 = PredMarkerPragmasCord0,
        TypeSpecPragmas = []
    ;
        DeclOrderPredInfos = [_ | _],
        io.nl(Stream, !IO),
        intermod_write_pred_decls(MercInfo, Stream, ModuleInfo,
            DeclOrderPredInfos,
            cord.init, PredDeclsCord,
            cord.init, ModeDeclsCord,
            PredMarkerPragmasCord0, PredMarkerPragmasCord1,
            cord.init, TypeSpecPragmasCord, !IO),
        PredDecls = cord.list(PredDeclsCord),
        ModeDecls = cord.list(ModeDeclsCord),
        TypeSpecPragmas = list.map(wrap_dummy_pragma_item,
            cord.list(TypeSpecPragmasCord))
    ),
    % Each of these writes a newline at the start.
    intermod_write_pred_defns(OutInfoForPreds, Stream, ModuleInfo,
        DefnOrderPredInfos, PredMarkerPragmasCord1, PredMarkerPragmasCord,
        !IO),
    PredMarkerPragmas = list.map(wrap_dummy_pragma_item,
        cord.list(PredMarkerPragmasCord)),
    Clauses = [],
    ForeignProcs = [],
    % XXX CLEANUP This *may* be a lie, in that some of the predicates we have
    % written out above *may* have goal_type_promise. However, until
    % we switch over completely to creating .opt files purely by building up
    % and then writing out a parse_tree_plain_opt, this shouldn't matter.
    Promises = [],

    module_info_get_name(ModuleInfo, ModuleName),
    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, term.context_init,
        UseMap, FIMSpecsSet, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        PredMarkerPragmas, TypeSpecPragmas, [], [], [], [], [], [], [], []).

:- type maybe_first
    --->    is_not_first
    ;       is_first.

%---------------------------------------------------------------------------%

:- pred intermod_gather_types(assoc_list(type_ctor, hlds_type_defn)::in,
    list(item_type_defn_info)::out, list(item_foreign_enum_info)::out) is det.

intermod_gather_types(Types, TypeDefns, ForeignEnums) :-
    list.sort(Types, SortedTypes),
    list.foldl2(intermod_gather_type, SortedTypes,
        cord.init, TypeDefnsCord, cord.init, ForeignEnumsCord),
    TypeDefns = cord.list(TypeDefnsCord),
    ForeignEnums = cord.list(ForeignEnumsCord).

:- pred intermod_gather_type(pair(type_ctor, hlds_type_defn)::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out,
    cord(item_foreign_enum_info)::in, cord(item_foreign_enum_info)::out)
    is det.

intermod_gather_type(TypeCtor - TypeDefn,
        !TypeDefnsCord, !ForeignEnumsCord) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_body(TypeDefn, Body),
    hlds_data.get_type_defn_context(TypeDefn, Context),
    TypeCtor = type_ctor(TypeSymName, _Arity),
    (
        Body = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(Ctors, MaybeSubType, MaybeCanon,
            MaybeRepnA, MaybeForeignTypeBody),
        (
            MaybeRepnA = no,
            unexpected($pred, "MaybeRepnA = no")
        ;
            MaybeRepnA = yes(RepnA),
            MaybeDirectArgCtors = RepnA ^ dur_direct_arg_ctors
        ),
        (
            MaybeSubType = subtype_of(SuperType),
            % TypeCtor may be noncanonical, and MaybeDirectArgCtors may be
            % nonempty, but any reader of the .opt file has to find out
            % both those facts from the base type of this subtype.
            DetailsSub = type_details_sub(SuperType, Ctors),
            TypeBody = parse_tree_sub_type(DetailsSub)
        ;
            MaybeSubType = not_a_subtype,
            % XXX TYPE_REPN We should output information about any direct args
            % as a separate type_repn item.
            DetailsDu = type_details_du(Ctors, MaybeCanon,
                MaybeDirectArgCtors),
            TypeBody = parse_tree_du_type(DetailsDu)
        )
    ;
        Body = hlds_eqv_type(EqvType),
        TypeBody = parse_tree_eqv_type(type_details_eqv(EqvType)),
        MaybeForeignTypeBody = no
    ;
        Body = hlds_abstract_type(Details),
        TypeBody = parse_tree_abstract_type(Details),
        MaybeForeignTypeBody = no
    ;
        Body = hlds_foreign_type(ForeignTypeBody0),
        TypeBody = parse_tree_abstract_type(abstract_type_general),
        MaybeForeignTypeBody = yes(ForeignTypeBody0)
    ;
        Body = hlds_solver_type(DetailsSolver),
        TypeBody = parse_tree_solver_type(DetailsSolver),
        MaybeForeignTypeBody = no
    ),
    MainItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams, TypeBody,
        TVarSet, Context, item_no_seq_num),
    cord.snoc(MainItemTypeDefn, !TypeDefnsCord),
    (
        MaybeForeignTypeBody = no
    ;
        MaybeForeignTypeBody = yes(ForeignTypeBody),
        ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCsharp),
        maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet,
            Context, (func(FT) = c(FT)), MaybeC, !TypeDefnsCord),
        maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet,
            Context, (func(FT) = java(FT)), MaybeJava, !TypeDefnsCord),
        maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet,
            Context, (func(FT) = csharp(FT)), MaybeCsharp, !TypeDefnsCord)
    ),
    ( if
        Body = hlds_du_type(type_body_du(_, _, _, MaybeRepnB, _)),
        MaybeRepnB = yes(RepnB),
        RepnB = du_type_repn(CtorRepns, _, _, DuTypeKind, _),
        DuTypeKind = du_type_kind_foreign_enum(Lang)
    then
        % XXX TYPE_REPN This code puts into the .opt file the foreign enum
        % specification for this type_ctor ONLY for the foreign language
        % used by the current target platform. We cannot fix this until
        % we preserve the same information for all the other foreign languages
        % as well.
        list.foldl(gather_foreign_enum_value_pair, CtorRepns,
            [], RevForeignEnumVals),
        list.reverse(RevForeignEnumVals, ForeignEnumVals),
        (
            ForeignEnumVals = []
            % This can only happen if the type has no function symbols.
            % which should have been detected and reported by now.
        ;
            ForeignEnumVals = [HeadForeignEnumVal | TailForeignEnumVals],
            OoMForeignEnumVals =
                one_or_more(HeadForeignEnumVal, TailForeignEnumVals),
            ForeignEnum = item_foreign_enum_info(Lang, TypeCtor,
                OoMForeignEnumVals, Context, item_no_seq_num),
            cord.snoc(ForeignEnum, !ForeignEnumsCord)
        )
    else
        true
    ).

:- pred maybe_acc_foreign_type_defn_info(sym_name::in, list(type_param)::in,
    tvarset::in, prog_context::in,
    (func(T) = generic_language_foreign_type)::in,
    maybe(type_details_foreign(T))::in,
    cord(item_type_defn_info)::in, cord(item_type_defn_info)::out) is det.

maybe_acc_foreign_type_defn_info(TypeSymName, TypeParams, TVarSet, Context,
        MakeGeneric, MaybeDetails, !TypeDefnsCord) :-
    (
        MaybeDetails = no
    ;
        MaybeDetails = yes(Details),
        Details = type_details_foreign(LangForeignType, MaybeUserEqComp,
            Assertions),
        DetailsForeign = type_details_foreign(MakeGeneric(LangForeignType),
            MaybeUserEqComp, Assertions),
        ItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams,
            parse_tree_foreign_type(DetailsForeign),
            TVarSet, Context, item_no_seq_num),
        cord.snoc(ItemTypeDefn, !TypeDefnsCord)
    ).

:- pred gather_foreign_enum_value_pair(constructor_repn::in,
    assoc_list(sym_name, string)::in, assoc_list(sym_name, string)::out)
    is det.

gather_foreign_enum_value_pair(CtorRepn, !RevValues) :-
    CtorRepn = ctor_repn(_, _, SymName, Tag, _, Arity, _),
    expect(unify(Arity, 0), $pred, "Arity != 0"),
    ( if Tag = foreign_tag(_ForeignLang, ForeignTag) then
        !:RevValues = [SymName - ForeignTag | !.RevValues]
    else
        unexpected($pred, "expected foreign tag")
    ).

%---------------------------------------------------------------------------%

:- pred intermod_gather_insts(module_info::in,
    list(item_inst_defn_info)::out) is det.

intermod_gather_insts(ModuleInfo, InstDefns) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_inst_table(ModuleInfo, Insts),
    inst_table_get_user_insts(Insts, UserInstMap),
    map.foldl(intermod_gather_inst(ModuleName), UserInstMap,
        cord.init, InstDefnsCord),
    InstDefns = cord.list(InstDefnsCord).

:- pred intermod_gather_inst(module_name::in,
    inst_ctor::in, hlds_inst_defn::in,
    cord(item_inst_defn_info)::in, cord(item_inst_defn_info)::out) is det.

intermod_gather_inst(ModuleName, InstCtor, InstDefn, !InstDefnsCord) :-
    InstCtor = inst_ctor(SymName, _Arity),
    InstDefn = hlds_inst_defn(Varset, Args, Inst, IFTC, Context, InstStatus),
    ( if
        SymName = qualified(ModuleName, _),
        inst_status_to_write(InstStatus) = yes
    then
        (
            IFTC = iftc_applicable_declared(ForTypeCtor),
            MaybeForTypeCtor = yes(ForTypeCtor)
        ;
            ( IFTC = iftc_applicable_known(_)
            ; IFTC = iftc_applicable_not_known
            ; IFTC = iftc_applicable_error
            ; IFTC = iftc_not_applicable
            ),
            MaybeForTypeCtor = no
        ),
        ItemInstDefn = item_inst_defn_info(SymName, Args, MaybeForTypeCtor,
            nonabstract_inst_defn(Inst), Varset, Context, item_no_seq_num),
        cord.snoc(ItemInstDefn, !InstDefnsCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred intermod_gather_modes(module_info::in,
    list(item_mode_defn_info)::out) is det.

intermod_gather_modes(ModuleInfo, ModeDefns) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefnMap),
    map.foldl(intermod_gather_mode(ModuleName), ModeDefnMap,
        cord.init, ModeDefnsCord),
    ModeDefns = cord.list(ModeDefnsCord).

:- pred intermod_gather_mode(module_name::in,
    mode_ctor::in, hlds_mode_defn::in,
    cord(item_mode_defn_info)::in, cord(item_mode_defn_info)::out) is det.

intermod_gather_mode(ModuleName, ModeCtor, ModeDefn, !ModeDefnsCord) :-
    ModeCtor = mode_ctor(SymName, _Arity),
    ModeDefn = hlds_mode_defn(Varset, Args, hlds_mode_body(Mode), Context,
        ModeStatus),
    ( if
        SymName = qualified(ModuleName, _),
        mode_status_to_write(ModeStatus) = yes
    then
        MaybeAbstractModeDefn = nonabstract_mode_defn(eqv_mode(Mode)),
        ItemModeDefn = item_mode_defn_info(SymName, Args,
            MaybeAbstractModeDefn, Varset, Context, item_no_seq_num),
        cord.snoc(ItemModeDefn, !ModeDefnsCord)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred intermod_gather_classes(module_info::in,
    list(item_typeclass_info)::out) is det.

intermod_gather_classes(ModuleInfo, TypeClasses) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_info_get_class_table(ModuleInfo, ClassDefnMap),
    map.foldl(intermod_gather_class(ModuleName), ClassDefnMap,
        cord.init, TypeClassesCord),
    TypeClasses = cord.list(TypeClassesCord).

:- pred intermod_gather_class(module_name::in,
    class_id::in, hlds_class_defn::in,
    cord(item_typeclass_info)::in, cord(item_typeclass_info)::out) is det.

intermod_gather_class(ModuleName, ClassId, ClassDefn, !TypeClassesCord) :-
    ClassDefn = hlds_class_defn(TypeClassStatus, Constraints, HLDSFunDeps,
        _Ancestors, TVars, _Kinds, InstanceBody, _MaybeMethodPPIds, TVarSet,
        Context, _HasBadDefn),
    ClassId = class_id(QualifiedClassName, _),
    ( if
        QualifiedClassName = qualified(ModuleName, _),
        typeclass_status_to_write(TypeClassStatus) = yes
    then
        FunDeps = list.map(unmake_hlds_class_fundep(TVars), HLDSFunDeps),
        ItemTypeClass = item_typeclass_info(QualifiedClassName, TVars,
            Constraints, FunDeps, InstanceBody, TVarSet,
            Context, item_no_seq_num),
        cord.snoc(ItemTypeClass, !TypeClassesCord)
    else
        true
    ).

:- func unmake_hlds_class_fundep(list(tvar), hlds_class_fundep) = prog_fundep.

unmake_hlds_class_fundep(TVars, HLDSFunDep) = ParseTreeFunDep :-
    HLDSFunDep = fundep(DomainArgPosns, RangeArgPosns),
    DomainTVars = unmake_hlds_class_fundep_arg_posns(TVars, DomainArgPosns),
    RangeTVars = unmake_hlds_class_fundep_arg_posns(TVars, RangeArgPosns),
    ParseTreeFunDep = fundep(DomainTVars, RangeTVars).

:- func unmake_hlds_class_fundep_arg_posns(list(tvar), set(hlds_class_argpos))
    = list(tvar).

unmake_hlds_class_fundep_arg_posns(TVars, ArgPosns) = ArgTVars :-
    ArgTVarsSet = set.map(list.det_index1(TVars), ArgPosns),
    set.to_sorted_list(ArgTVarsSet, ArgTVars).

%---------------------------------------------------------------------------%

:- pred intermod_gather_instances(assoc_list(class_id, hlds_instance_defn)::in,
    list(item_instance_info)::out) is det.

intermod_gather_instances(InstanceDefns, Instances) :-
    list.sort(InstanceDefns, SortedInstanceDefns),
    list.foldl(intermod_gather_instance, SortedInstanceDefns,
        cord.init, InstancesCord),
    Instances = cord.list(InstancesCord).

:- pred intermod_gather_instance(pair(class_id, hlds_instance_defn)::in,
    cord(item_instance_info)::in, cord(item_instance_info)::out) is det.

intermod_gather_instance(ClassId - InstanceDefn, !InstancesCord) :-
    InstanceDefn = hlds_instance_defn(ModuleName, Types, OriginalTypes, _,
        Context, _, Constraints, Body, _, TVarSet, _),
    ClassId = class_id(ClassName, _),
    ItemInstance = item_instance_info(ClassName, Types, OriginalTypes,
        Constraints, Body, TVarSet, ModuleName, Context, item_no_seq_num),
    cord.snoc(ItemInstance, !InstancesCord).

%---------------------------------------------------------------------------%

    % We need to write all the declarations for local predicates so
    % the procedure labels for the C code are calculated correctly.
    %
:- pred intermod_write_pred_decls(merc_out_info::in, io.text_output_stream::in,
    module_info::in, list(order_pred_info)::in,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(pragma_info_pred_marker)::in, cord(pragma_info_pred_marker)::out,
    cord(pragma_info_type_spec)::in, cord(pragma_info_type_spec)::out,
    io::di, io::uo) is det.

intermod_write_pred_decls(_, _, _, [],
        !PredDeclsCord, !ModeDeclsCord, !PredMarkerPragmasCord,
        !TypeSpecPragmasCord, !IO).
intermod_write_pred_decls(MercInfo, Stream, ModuleInfo,
        [OrderPredInfo | OrderPredInfos],
        !PredDeclsCord, !ModeDeclsCord, !PredMarkerPragmasCord,
        !TypeSpecPragmasCord, !IO) :-
    intermod_write_pred_decl(MercInfo, Stream, ModuleInfo, OrderPredInfo,
        !PredDeclsCord, !ModeDeclsCord, !PredMarkerPragmasCord,
        !TypeSpecPragmasCord, !IO),
    intermod_write_pred_decls(MercInfo, Stream, ModuleInfo, OrderPredInfos,
        !PredDeclsCord, !ModeDeclsCord, !PredMarkerPragmasCord,
        !TypeSpecPragmasCord, !IO).

:- pred intermod_write_pred_decl(merc_out_info::in, io.text_output_stream::in,
    module_info::in, order_pred_info::in,
    cord(item_pred_decl_info)::in, cord(item_pred_decl_info)::out,
    cord(item_mode_decl_info)::in, cord(item_mode_decl_info)::out,
    cord(pragma_info_pred_marker)::in, cord(pragma_info_pred_marker)::out,
    cord(pragma_info_type_spec)::in, cord(pragma_info_type_spec)::out,
    io::di, io::uo) is det.

intermod_write_pred_decl(MercInfo, Stream, ModuleInfo, OrderPredInfo,
        !PredDeclsCord, !ModeDeclsCord, !PredMarkerPragmasCord,
        !TypeSpecPragmasCord, !IO) :-
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    pred_info_get_arg_types(PredInfo, TVarSet, ExistQVars, ArgTypes),
    pred_info_get_purity(PredInfo, Purity),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_context(PredInfo, Context),
    PredSymName = qualified(ModuleName, PredName),
    TypesAndNoModes = list.map((func(T) = type_only(T)), ArgTypes),
    MaybeWithType = maybe.no,
    MaybeWithInst = maybe.no,
    MaybeDetism = maybe.no,     % We are NOT declaring the mode.
    varset.init(InstVarSet),
    % Origin is a dummy, which is OK because the origin is never printed.
    % If that ever changes, we would have to reverse the transform done
    % by record_pred_origin in add_pred.m.
    Origin = item_origin_user,
    PredDecl = item_pred_decl_info(PredSymName, PredOrFunc,
        TypesAndNoModes, MaybeWithType, MaybeWithInst, MaybeDetism, Origin,
        TVarSet, InstVarSet, ExistQVars, Purity, ClassContext,
        Context, item_no_seq_num),
    % NOTE: The names of type variables in type_spec pragmas must match
    % *exactly* the names of the corresponding type variables in the 
    % predicate declaration to which they apply. This is why one variable,
    % VarNamePrint, controls both.
    %
    % If a predicate is defined by a foreign_proc, then its declaration
    % *must* be printed with print_name_only, because that is the only way
    % that any reference to the type_info variable in the foreign code
    % in the body of the foreign_proc will match the declared name of the
    % type variable that it is for.
    %
    % We used to print the predicate declarations with print_name_only
    % for such predicates (predicates defined by foreign_procs) and with
    % print_name_and_num for all other predicates. (That included predicates
    % representing promises.) However, the predicates whose declarations
    % we are writing out have not been through any transformation that
    % would have either (a) changed the names of any existing type variables,
    % or (b) introduced any new type variables, so the mapping between
    % type variable numbers and names should be the same now as when the
    % the predicate declaration was first parsed. And at that time, two
    % type variable occurrences with the same name obviously referred to the
    % same type variable, so the numeric suffix added by print_name_and_num
    % was obviously not needed.
    VarNamePrint = print_name_only,
    mercury_output_item_pred_decl(output_mercury, VarNamePrint, Stream,
        PredDecl, !IO),
    pred_info_get_proc_table(PredInfo, ProcMap),
    % Make sure the mode declarations go out in the same order they came in,
    % so that the all the modes get the same proc_id in the importing modules.
    % SortedProcPairs will be sorted on proc_ids. (map.values is not
    % *documented* to return a list sorted by keys.)
    map.to_sorted_assoc_list(ProcMap, SortedProcPairs),
    intermod_gather_pred_valid_modes(PredOrFunc, PredSymName,
        SortedProcPairs, ModeDecls),
    intermod_gather_pred_marker_pragmas(PredInfo, PredMarkerPragmas),
    intermod_gather_pred_type_spec_pragmas(ModuleInfo, PredId,
        TypeSpecPragmas),

    list.foldl(mercury_output_item_mode_decl(MercInfo, Stream),
        ModeDecls, !IO),
    list.foldl(mercury_output_item_pred_marker(Stream),
        PredMarkerPragmas, !IO),
    Lang = output_mercury,
    list.foldl(mercury_output_pragma_type_spec(Stream, VarNamePrint, Lang),
        TypeSpecPragmas, !IO),

    cord.snoc(PredDecl, !PredDeclsCord),
    !:ModeDeclsCord = !.ModeDeclsCord ++ cord.from_list(ModeDecls),
    !:PredMarkerPragmasCord =
        !.PredMarkerPragmasCord ++ cord.from_list(PredMarkerPragmas),
    !:TypeSpecPragmasCord =
        !.TypeSpecPragmasCord ++ cord.from_list(TypeSpecPragmas).

:- pred intermod_gather_pred_valid_modes(pred_or_func::in, sym_name::in,
    assoc_list(proc_id, proc_info)::in, list(item_mode_decl_info)::out) is det.

intermod_gather_pred_valid_modes(_, _, [], []).
intermod_gather_pred_valid_modes(PredOrFunc, PredSymName,
        [ProcIdInfo | ProcIdInfos], ModeDecls) :-
    intermod_gather_pred_valid_modes(PredOrFunc, PredSymName,
        ProcIdInfos, TailModeDecls),
    ProcIdInfo = _ProcId - ProcInfo,
    ( if proc_info_is_valid_mode(ProcInfo) then
        proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
        proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
        ( if
            MaybeArgModes = yes(ArgModesPrime),
            MaybeDetism = yes(DetismPrime)
        then
            ArgModes = ArgModesPrime,
            Detism = DetismPrime
        else
            unexpected($pred, "attempt to write undeclared mode")
        ),
        MaybeWithInst = maybe.no,
        varset.init(InstVarset),
        HeadModeDecl = item_mode_decl_info(PredSymName, yes(PredOrFunc),
            ArgModes, MaybeWithInst, yes(Detism), InstVarset,
            term.dummy_context_init, item_no_seq_num),
        ModeDecls = [HeadModeDecl | TailModeDecls]
    else
        ModeDecls = TailModeDecls
    ).

:- pred intermod_gather_pred_marker_pragmas(pred_info::in,
    list(pragma_info_pred_marker)::out) is det.

intermod_gather_pred_marker_pragmas(PredInfo, PredMarkerPragmas) :-
    ModuleName = pred_info_module(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredSymName = qualified(ModuleName, PredName),
    PredFormArity = pred_info_orig_arity(PredInfo),
    user_arity_pred_form_arity(PredOrFunc, UserArity,
        pred_form_arity(PredFormArity)),
    pred_info_get_markers(PredInfo, Markers),
    markers_to_marker_list(Markers, MarkerList),
    intermod_gather_pred_marker_pragmas_loop(PredOrFunc,
        PredSymName, UserArity, MarkerList, [], RevPredMarkerPragmas),
    list.reverse(RevPredMarkerPragmas, PredMarkerPragmas).

:- pred intermod_gather_pred_marker_pragmas_loop(pred_or_func::in,
    sym_name::in, user_arity::in, list(pred_marker)::in,
    list(pragma_info_pred_marker)::in, list(pragma_info_pred_marker)::out)
    is det.

intermod_gather_pred_marker_pragmas_loop(_, _, _, [], !RevPredMarkerPragmas).
intermod_gather_pred_marker_pragmas_loop(PredOrFunc, PredSymName, UserArity,
        [Marker | Markers], !RevPredMarkerPragmas) :-
    (
        % We do not output these markers.
        ( Marker = marker_stub
        ; Marker = marker_builtin_stub
        ; Marker = marker_no_pred_decl
        ; Marker = marker_no_detism_warning
        ; Marker = marker_heuristic_inline
        ; Marker = marker_consider_used
        ; Marker = marker_calls_are_fully_qualified
        ; Marker = marker_mutable_access_pred
        ; Marker = marker_has_require_scope
        ; Marker = marker_has_incomplete_switch
        ; Marker = marker_has_format_call
        ; Marker = marker_has_rhs_lambda
        ; Marker = marker_fact_table_semantic_errors

        % Since the inferred declarations are output, these don't need
        % to be done in the importing module.
        ; Marker = marker_infer_type
        ; Marker = marker_infer_modes

        % Purity is output as part of the pred/func decl.
        ; Marker = marker_is_impure
        ; Marker = marker_is_semipure

        % There is no pragma required for generated class methods.
        ; Marker = marker_class_method
        ; Marker = marker_class_instance_method
        ; Marker = marker_named_class_instance_method

        % Termination should only be checked in the defining module.
        ; Marker = marker_check_termination
        )
    ;
        % We do output these markers.
        (
            Marker = marker_user_marked_inline,
            PragmaKind = pmpk_inline
        ;
            Marker = marker_user_marked_no_inline,
            PragmaKind = pmpk_noinline
        ;
            Marker = marker_promised_pure,
            PragmaKind = pmpk_promise_pure
        ;
            Marker = marker_promised_semipure,
            PragmaKind = pmpk_promise_semipure
        ;
            Marker = marker_promised_equivalent_clauses,
            PragmaKind = pmpk_promise_eqv_clauses
        ;
            Marker = marker_terminates,
            PragmaKind = pmpk_terminates
        ;
            Marker = marker_does_not_terminate,
            PragmaKind = pmpk_does_not_terminate
        ;
            Marker = marker_mode_check_clauses,
            PragmaKind = pmpk_mode_check_clauses
        ),
        PredSpec = pred_pf_name_arity(PredOrFunc, PredSymName, UserArity),
        PredMarkerInfo = pragma_info_pred_marker(PredSpec, PragmaKind),
        !:RevPredMarkerPragmas = [PredMarkerInfo | !.RevPredMarkerPragmas]
    ),
    intermod_gather_pred_marker_pragmas_loop(PredOrFunc, PredSymName,
        UserArity, Markers, !RevPredMarkerPragmas).

:- pred intermod_gather_pred_type_spec_pragmas(module_info::in, pred_id::in,
    list(pragma_info_type_spec)::out) is det.

intermod_gather_pred_type_spec_pragmas(ModuleInfo, PredId, TypeSpecPragmas) :-
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    PragmaMap = TypeSpecInfo ^ pragma_map,
    ( if multi_map.search(PragmaMap, PredId, TypeSpecPragmasPrime) then
        TypeSpecPragmas = TypeSpecPragmasPrime
    else
        TypeSpecPragmas = []
    ).

%---------------------------------------------------------------------------%

:- pred intermod_write_pred_defns(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, list(order_pred_info)::in,
    cord(pragma_info_pred_marker)::in, cord(pragma_info_pred_marker)::out,
    io::di, io::uo) is det.

intermod_write_pred_defns(_, _, _, [], !PredMarkerPragmas, !IO).
intermod_write_pred_defns(OutInfo, Stream, ModuleInfo,
        [OrderPredInfo | OrderPredInfos], !PredMarkerPragmas, !IO) :-
    intermod_write_pred_defn(OutInfo, Stream, ModuleInfo, OrderPredInfo,
        !PredMarkerPragmas, !IO),
    intermod_write_pred_defns(OutInfo, Stream, ModuleInfo, OrderPredInfos,
        !PredMarkerPragmas, !IO).

:- pred intermod_write_pred_defn(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, order_pred_info::in,
    cord(pragma_info_pred_marker)::in, cord(pragma_info_pred_marker)::out,
    io::di, io::uo) is det.

intermod_write_pred_defn(OutInfo, Stream, ModuleInfo, OrderPredInfo,
        !PredMarkerPragmas, !IO) :-
    io.nl(Stream, !IO),
    OrderPredInfo = order_pred_info(PredName, _PredArity, PredOrFunc,
        PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredSymName = qualified(ModuleName, PredName),
    intermod_gather_pred_marker_pragmas(PredInfo, PredMarkerPragmas),
    list.foldl(mercury_output_item_pred_marker(Stream),
        PredMarkerPragmas, !IO),
    !:PredMarkerPragmas =
        !.PredMarkerPragmas ++ cord.from_list(PredMarkerPragmas),
    % The type specialization pragmas for exported preds should
    % already be in the interface file.
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    clauses_info_get_varset(ClausesInfo, VarSet),
    clauses_info_get_headvar_list(ClausesInfo, HeadVars),
    clauses_info_get_clauses_rep(ClausesInfo, ClausesRep, _ItemNumbers),
    clauses_info_get_vartypes(ClausesInfo, VarTypes),
    get_clause_list_maybe_repeated(ClausesRep, Clauses),

    pred_info_get_goal_type(PredInfo, GoalType),
    (
        GoalType = goal_for_promise(PromiseType),
        (
            Clauses = [Clause],
            write_promise(OutInfo, Stream, ModuleInfo, VarSet,
                PromiseType, HeadVars, Clause, !IO)
        ;
            ( Clauses = []
            ; Clauses = [_, _ | _]
            ),
            unexpected($pred, "assertion not a single clause.")
        )
    ;
        GoalType = goal_not_for_promise(_),
        pred_info_get_typevarset(PredInfo, TypeVarset),
        TypeQual = varset_vartypes(TypeVarset, VarTypes),
        list.foldl(
            intermod_write_clause(OutInfo, Stream, ModuleInfo, PredId,
                PredSymName, PredOrFunc, VarSet, TypeQual, HeadVars),
            Clauses, !IO)
    ).

:- pred write_promise(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, prog_varset::in, promise_type::in, list(prog_var)::in,
    clause::in, io::di, io::uo) is det.

write_promise(Info, Stream, ModuleInfo, VarSet, PromiseType, HeadVars,
        Clause, !IO) :-
    % Please *either* keep this code in sync with mercury_output_item_promise
    % in parse_tree_out.m, *or* rewrite it to forward the work to that
    % predicate.
    HeadVarStrs = list.map(varset.lookup_name(VarSet), HeadVars),
    HeadVarsStr = string.join_list(", ", HeadVarStrs),
    % Print initial formatting differently for assertions.
    (
        PromiseType = promise_type_true,
        io.format(Stream, ":- promise all [%s] (\n", [s(HeadVarsStr)], !IO)
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        io.format(Stream, ":- all [%s] %s\n(\n",
            [s(HeadVarsStr), s(promise_to_string(PromiseType))], !IO)
    ),
    Goal = Clause ^ clause_body,
    do_write_goal(Info, Stream, ModuleInfo, vns_varset(VarSet),
        no_varset_vartypes, print_name_only, 1, "\n).\n", Goal, !IO).

:- pred intermod_write_clause(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, pred_id::in, sym_name::in, pred_or_func::in,
    prog_varset::in, maybe_vartypes::in, list(prog_var)::in, clause::in,
    io::di, io::uo) is det.

intermod_write_clause(OutInfo, Stream, ModuleInfo, PredId, SymName, PredOrFunc,
        VarSet, TypeQual, HeadVars, Clause0, !IO) :-
    Clause0 = clause(ApplicableProcIds, Goal, ImplLang, _, _),
    (
        ImplLang = impl_lang_mercury,
        strip_headvar_unifications(HeadVars, Clause0, ClauseHeadVars, Clause),
        % Variable numbers need to be used for the case where the added
        % arguments for a DCG pred expression are named the same
        % as variables in the enclosing clause.
        %
        % We don't need the actual names, and including them in the .opt file
        % would lead to unnecessary recompilations when the *only* changes
        % in a .opt file are changes in variable variables.
        %
        % We could standardize the variables in the clause before printing
        % it out, numbering them e.g. in the order of their appearance,
        % so that changes in variable *numbers* don't cause recompilations
        % either. However, the variable numbers *are* initially allocated
        % in such an order, both by the code that reads in terms and the
        % code that converts parse tree goals into HLDS goals, so this is
        % not likely to be necessary, while its cost may be non-negligible.
        write_clause(OutInfo, Stream, output_mercury, ModuleInfo,
            PredId, PredOrFunc, varset.init, TypeQual, print_name_and_num,
            write_declared_modes, 1, ClauseHeadVars, Clause, !IO)
    ;
        ImplLang = impl_lang_foreign(_),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        ( if
            (
                % Pull the foreign code out of the goal.
                Goal = hlds_goal(conj(plain_conj, Goals), _),
                list.filter(
                    ( pred(G::in) is semidet :-
                        G = hlds_goal(GE, _),
                        GE = call_foreign_proc(_, _, _, _, _, _, _)
                    ), Goals, [ForeignCodeGoal]),
                ForeignCodeGoal = hlds_goal(ForeignCodeGoalExpr, _),
                ForeignCodeGoalExpr = call_foreign_proc(Attributes, _, _,
                    Args, _ExtraArgs, _MaybeTraceRuntimeCond, PragmaCode)
            ;
                Goal = hlds_goal(GoalExpr, _),
                GoalExpr = call_foreign_proc(Attributes, _, _,
                    Args, _ExtraArgs, _MaybeTraceRuntimeCond, PragmaCode)
            )
        then
            (
                ApplicableProcIds = all_modes,
                unexpected($pred, "all_modes foreign_proc")
            ;
                ApplicableProcIds = selected_modes(ProcIds),
                list.foldl(
                    intermod_write_foreign_clause(Stream, Procs, PredOrFunc,
                        PragmaCode, Attributes, Args, VarSet, SymName),
                    ProcIds, !IO)
            ;
                ( ApplicableProcIds = unify_in_in_modes
                ; ApplicableProcIds = unify_non_in_in_modes
                ),
                unexpected($pred, "unify modes foreign_proc")
            )
        else
            unexpected($pred, "did not find foreign_proc")
        )
    ).

    % Strip the `Headvar.n = Term' unifications from each clause,
    % except if the `Term' is a lambda expression.
    %
    % At least two problems occur if this is not done:
    %
    % - in some cases where nested unique modes were accepted by mode analysis,
    %   the extra aliasing added by the extra level of headvar unifications
    %   caused mode analysis to report an error (ground expected unique),
    %   when analysing the clauses read in from `.opt' files.
    %
    % - only HeadVar unifications may be reordered with impure goals,
    %   so a mode error results for the second level of headvar unifications
    %   added when the clauses are read in again from the `.opt' file.
    %   Clauses containing impure goals are not written to the `.opt' file
    %   for this reason.
    %
:- pred strip_headvar_unifications(list(prog_var)::in,
    clause::in, list(prog_term)::out, clause::out) is det.

strip_headvar_unifications(HeadVars, Clause0, HeadTerms, Clause) :-
    Goal0 = Clause0 ^ clause_body,
    Goal0 = hlds_goal(_, GoalInfo0),
    goal_to_conj_list(Goal0, Goals0),
    map.init(HeadVarMap0),
    ( if
        strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
            [], Goals, HeadVarMap0, HeadVarMap)
    then
        list.map(
            ( pred(HeadVar0::in, HeadTerm::out) is det :-
                ( if map.search(HeadVarMap, HeadVar0, HeadTerm0) then
                    HeadTerm = HeadTerm0
                else
                    Context = Clause0 ^ clause_context,
                    HeadTerm = term.variable(HeadVar0, Context)
                )
            ), HeadVars, HeadTerms),
        conj_list_to_goal(Goals, GoalInfo0, Goal),
        Clause = Clause0 ^ clause_body := Goal
    else
        term.var_list_to_term_list(HeadVars, HeadTerms),
        Clause = Clause0
    ).

:- pred strip_headvar_unifications_from_goal_list(list(hlds_goal)::in,
    list(prog_var)::in, list(hlds_goal)::in, list(hlds_goal)::out,
    map(prog_var, prog_term)::in,
    map(prog_var, prog_term)::out) is semidet.

strip_headvar_unifications_from_goal_list([], _, RevGoals, Goals,
        !HeadVarMap) :-
    list.reverse(RevGoals, Goals).
strip_headvar_unifications_from_goal_list([Goal | Goals0], HeadVars,
        RevGoals0, Goals, !HeadVarMap) :-
    ( if
        Goal = hlds_goal(unify(LHSVar, RHS, _, _, _), _),
        list.member(LHSVar, HeadVars),
        term.context_init(Context),
        (
            RHS = rhs_var(RHSVar),
            RHSTerm = term.variable(RHSVar, Context)
        ;
            RHS = rhs_functor(ConsId, _, Args),
            require_complete_switch [ConsId]
            (
                ConsId = some_int_const(IntConst),
                RHSTerm = int_const_to_decimal_term(IntConst, Context)
            ;
                ConsId = float_const(Float),
                RHSTerm = term.functor(term.float(Float), [], Context)
            ;
                ConsId = char_const(Char),
                RHSTerm = term.functor(term.atom(string.from_char(Char)),
                    [], Context)
            ;
                ConsId = string_const(String),
                RHSTerm = term.functor(term.string(String), [], Context)
            ;
                ConsId = cons(SymName, _, _),
                term.var_list_to_term_list(Args, ArgTerms),
                construct_qualified_term(SymName, ArgTerms, RHSTerm)
            ;
                ( ConsId = base_typeclass_info_const(_, _, _, _)
                ; ConsId = closure_cons(_, _)
                ; ConsId = deep_profiling_proc_layout(_)
                ; ConsId = ground_term_const(_, _)
                ; ConsId = tabling_info_const(_)
                ; ConsId = impl_defined_const(_)
                ; ConsId = table_io_entry_desc(_)
                ; ConsId = tuple_cons(_)
                ; ConsId = type_ctor_info_const(_, _, _)
                ; ConsId = type_info_cell_constructor(_)
                ; ConsId = typeclass_info_cell_constructor
                ; ConsId = type_info_const(_)
                ; ConsId = typeclass_info_const(_)
                ),
                fail
            )
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _),
            fail
        )
    then
        % Don't strip the headvar unifications if one of the headvars
        % appears twice. This should probably never happen.
        map.insert(LHSVar, RHSTerm, !HeadVarMap),
        RevGoals1 = RevGoals0
    else
        RevGoals1 = [Goal | RevGoals0]
    ),
    strip_headvar_unifications_from_goal_list(Goals0, HeadVars,
        RevGoals1, Goals, !HeadVarMap).

:- pred intermod_write_foreign_clause(io.text_output_stream::in,
    proc_table::in, pred_or_func::in,
    pragma_foreign_proc_impl::in, pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, prog_varset::in, sym_name::in, proc_id::in,
    io::di, io::uo) is det.

intermod_write_foreign_clause(Stream, Procs, PredOrFunc, PragmaImpl,
        Attributes, Args, ProgVarset0, SymName, ProcId, !IO) :-
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes),
        get_pragma_foreign_code_vars(Args, ArgModes,
            ProgVarset0, ProgVarset, PragmaVars),
        proc_info_get_inst_varset(ProcInfo, InstVarset),
        FPInfo = pragma_info_foreign_proc(Attributes, SymName,
            PredOrFunc, PragmaVars, ProgVarset, InstVarset, PragmaImpl),
        mercury_output_pragma_foreign_proc(Stream, output_mercury, FPInfo, !IO)
    ;
        MaybeArgModes = no,
        unexpected($pred, "no mode declaration")
    ).

:- pred get_pragma_foreign_code_vars(list(foreign_arg)::in, list(mer_mode)::in,
    prog_varset::in, prog_varset::out, list(pragma_var)::out) is det.

get_pragma_foreign_code_vars(Args, Modes, !VarSet, PragmaVars) :-
    (
        Args = [Arg | ArgsTail],
        Modes = [Mode | ModesTail],
        Arg = foreign_arg(Var, MaybeNameAndMode, _, _),
        (
            MaybeNameAndMode = no,
            Name = "_"
        ;
            MaybeNameAndMode = yes(foreign_arg_name_mode(Name, _Mode2))
        ),
        PragmaVar = pragma_var(Var, Name, Mode, bp_native_if_possible),
        varset.name_var(Var, Name, !VarSet),
        get_pragma_foreign_code_vars(ArgsTail, ModesTail, !VarSet,
            PragmaVarsTail),
        PragmaVars = [PragmaVar | PragmaVarsTail]
    ;
        Args = [],
        Modes = [],
        PragmaVars = []
    ;
        Args = [],
        Modes = [_ | _],
        unexpected($pred, "list length mismatch")
    ;
        Args = [_ | _],
        Modes = [],
        unexpected($pred, "list length mismatch")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Should a declaration with the given status be written to the `.opt' file.
    %
:- func type_status_to_write(type_status) = bool.
:- func inst_status_to_write(inst_status) = bool.
:- func mode_status_to_write(mode_status) = bool.
:- func typeclass_status_to_write(typeclass_status) = bool.
:- func instance_status_to_write(instance_status) = bool.
:- func pred_status_to_write(pred_status) = bool.

type_status_to_write(type_status(OldStatus)) =
    old_status_to_write(OldStatus).
inst_status_to_write(inst_status(InstModeStatus)) = ToWrite :-
    ToWrite = instmode_status_to_write(InstModeStatus).
mode_status_to_write(mode_status(InstModeStatus)) = ToWrite :-
    ToWrite = instmode_status_to_write(InstModeStatus).
typeclass_status_to_write(typeclass_status(OldStatus)) =
    old_status_to_write(OldStatus).
instance_status_to_write(instance_status(OldStatus)) =
    old_status_to_write(OldStatus).
pred_status_to_write(pred_status(OldStatus)) =
    old_status_to_write(OldStatus).

:- func instmode_status_to_write(new_instmode_status) = bool.

instmode_status_to_write(InstModeStatus) = ToWrite :-
    (
        InstModeStatus = instmode_defined_in_this_module(InstModeExport),
        (
            InstModeExport = instmode_export_anywhere,
            ToWrite = no
        ;
            ( InstModeExport = instmode_export_only_submodules
            ; InstModeExport = instmode_export_nowhere
            ),
            ToWrite = yes
        )
    ;
        InstModeStatus = instmode_defined_in_other_module(_),
        ToWrite = no
    ).

:- func old_status_to_write(old_import_status) = bool.

old_status_to_write(status_imported(_)) = no.
old_status_to_write(status_abstract_imported) = no.
old_status_to_write(status_pseudo_imported) = no.
old_status_to_write(status_opt_imported) = no.
old_status_to_write(status_exported) = no.
old_status_to_write(status_opt_exported) = yes.
old_status_to_write(status_abstract_exported) = yes.
old_status_to_write(status_pseudo_exported) = no.
old_status_to_write(status_exported_to_submodules) = yes.
old_status_to_write(status_local) = yes.
old_status_to_write(status_external(Status)) =
    bool.not(old_status_is_exported(Status)).

%---------------------------------------------------------------------------%

:- type maybe_need_foreign_import_modules
    --->    do_not_need_foreign_import_modules
    ;       do_need_foreign_import_modules.

    % A collection of stuff to go in the .opt file.
    %
:- type intermod_info
    --->    intermod_info(
                % The initial ModuleInfo. Readonly.
                im_module_info          :: module_info,

                % The modules that the .opt file will need to use.
                im_use_modules          :: set(module_name),

                % The ids of the predicates (and functions) whose type and mode
                % declarations we want to put into the .opt file.
                im_pred_decls           :: set(pred_id),

                % The ids of the predicates (and functions) whose definitions
                % (i.e. clauses, foreign_procs or promises) we want to put
                % into the .opt file.
                im_pred_defns           :: set(pred_id),

                % The instance definitions we want to put into the .opt file.
                im_instance_defns       :: assoc_list(class_id,
                                            hlds_instance_defn),

                % The type definitions we want to put into the .opt file.
                im_type_defns           :: assoc_list(type_ctor,
                                            hlds_type_defn),

                % Is there anything we want to put into the .opt file
                % that may refer to foreign language entities that may need
                % access to foreign_import_modules to resolve?
                %
                % If no, we don't need to include any of the
                % foreign_import_modules declarations in the module
                % in the .opt file.
                %
                % If yes, we need to include all of them in the .opt file,
                % since we have no info about which fim defines what.
                im_need_foreign_imports :: maybe_need_foreign_import_modules
            ).

:- pred init_intermod_info(module_info::in, intermod_info::out) is det.

init_intermod_info(ModuleInfo, IntermodInfo) :-
    set.init(Modules),
    set.init(PredDecls),
    set.init(PredDefns),
    InstanceDefns = [],
    TypeDefns = [],
    IntermodInfo = intermod_info(ModuleInfo, Modules, PredDecls, PredDefns,
        InstanceDefns, TypeDefns, do_not_need_foreign_import_modules).

:- pred intermod_info_get_module_info(intermod_info::in, module_info::out)
    is det.
:- pred intermod_info_get_use_modules(intermod_info::in, set(module_name)::out)
    is det.
:- pred intermod_info_get_pred_decls(intermod_info::in, set(pred_id)::out)
    is det.
:- pred intermod_info_get_pred_defns(intermod_info::in, set(pred_id)::out)
    is det.
:- pred intermod_info_get_instances(intermod_info::in,
    assoc_list(class_id, hlds_instance_defn)::out) is det.
:- pred intermod_info_get_types(intermod_info::in,
    assoc_list(type_ctor, hlds_type_defn)::out) is det.

:- pred intermod_info_set_use_modules(set(module_name)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_pred_decls(set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_pred_defns(set(pred_id)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_instances(
    assoc_list(class_id, hlds_instance_defn)::in,
    intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_types(assoc_list(type_ctor, hlds_type_defn)::in,
    intermod_info::in, intermod_info::out) is det.
%:- pred intermod_info_set_insts(set(inst_ctor)::in,
%   intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_need_foreign_import_modules(intermod_info::in,
    intermod_info::out) is det.

intermod_info_get_module_info(IntermodInfo, X) :-
    X = IntermodInfo ^ im_module_info.
intermod_info_get_use_modules(IntermodInfo, X) :-
    X = IntermodInfo ^ im_use_modules.
intermod_info_get_pred_decls(IntermodInfo, X) :-
    X = IntermodInfo ^ im_pred_decls.
intermod_info_get_pred_defns(IntermodInfo, X) :-
    X = IntermodInfo ^ im_pred_defns.
intermod_info_get_instances(IntermodInfo, X) :-
    X = IntermodInfo ^ im_instance_defns.
intermod_info_get_types(IntermodInfo, X) :-
    X = IntermodInfo ^ im_type_defns.

intermod_info_set_use_modules(X, !IntermodInfo) :-
    !IntermodInfo ^ im_use_modules := X.
intermod_info_set_pred_decls(X, !IntermodInfo) :-
    !IntermodInfo ^ im_pred_decls := X.
intermod_info_set_pred_defns(X, !IntermodInfo) :-
    !IntermodInfo ^ im_pred_defns := X.
intermod_info_set_instances(X, !IntermodInfo) :-
    !IntermodInfo ^ im_instance_defns := X.
intermod_info_set_types(X, !IntermodInfo) :-
    !IntermodInfo ^ im_type_defns := X.
intermod_info_set_need_foreign_import_modules(!IntermodInfo) :-
    !IntermodInfo ^ im_need_foreign_imports := do_need_foreign_import_modules.

%---------------------------------------------------------------------------%

maybe_opt_export_entities(!ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    trace [io(!IO)] (
        get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
        maybe_write_string(ProgressStream, VeryVerbose,
            "% Adjusting import status of predicates in the `.opt' file...",
            !IO)
    ),
    decide_what_to_opt_export(!.ModuleInfo, IntermodInfo),
    maybe_opt_export_listed_entities(IntermodInfo, !ModuleInfo),
    trace [io(!IO)] (
        get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
        maybe_write_string(ProgressStream, VeryVerbose, " done\n", !IO)
    ).

maybe_opt_export_listed_entities(IntermodInfo, !ModuleInfo) :-
    % XXX This would be clearer as well as faster if we gathered up
    % the pred_ids of all the predicates that we found we need to opt_export
    % while processing type, typeclass and instance definitions,
    % and then opt_exported them all at once.
    intermod_info_get_pred_decls(IntermodInfo, PredDeclsSet),
    set.to_sorted_list(PredDeclsSet, PredDecls),
    opt_export_preds(PredDecls, !ModuleInfo),
    maybe_opt_export_types(!ModuleInfo),
    maybe_opt_export_classes(!ModuleInfo),
    maybe_opt_export_instances(!ModuleInfo).

%---------------------%

:- pred maybe_opt_export_types(module_info::in, module_info::out) is det.

maybe_opt_export_types(!ModuleInfo) :-
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    map_foldl_over_type_ctor_defns(maybe_opt_export_type_defn,
        TypeTable0, TypeTable, !ModuleInfo),
    module_info_set_type_table(TypeTable, !ModuleInfo).

:- pred maybe_opt_export_type_defn(type_ctor::in,
    hlds_type_defn::in, hlds_type_defn::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_type_defn(TypeCtor, TypeDefn0, TypeDefn, !ModuleInfo) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ( if should_opt_export_type_defn(ModuleName, TypeCtor, TypeDefn0) then
        hlds_data.set_type_defn_status(type_status(status_exported),
            TypeDefn0, TypeDefn),
        adjust_status_of_special_preds(TypeCtor, !ModuleInfo)
    else
        TypeDefn = TypeDefn0
    ).

:- pred adjust_status_of_special_preds((type_ctor)::in,
    module_info::in, module_info::out) is det.

adjust_status_of_special_preds(TypeCtor, ModuleInfo0, ModuleInfo) :-
    special_pred_list(SpecialPredList),
    module_info_get_special_pred_maps(ModuleInfo0, SpecPredMaps),
    list.filter_map(
        ( pred(SpecPredId::in, PredId::out) is semidet :-
            search_special_pred_maps(SpecPredMaps, SpecPredId, TypeCtor,
                PredId)
        ), SpecialPredList, PredIds),
    opt_export_preds(PredIds, ModuleInfo0, ModuleInfo).

%---------------------%

:- pred maybe_opt_export_classes(module_info::in, module_info::out) is det.

maybe_opt_export_classes(!ModuleInfo) :-
    module_info_get_class_table(!.ModuleInfo, Classes0),
    map.to_assoc_list(Classes0, ClassAL0),
    list.map_foldl(maybe_opt_export_class_defn, ClassAL0, ClassAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(ClassAL, Classes),
    module_info_set_class_table(Classes, !ModuleInfo).

:- pred maybe_opt_export_class_defn(pair(class_id, hlds_class_defn)::in,
    pair(class_id, hlds_class_defn)::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_class_defn(ClassId - ClassDefn0, ClassId - ClassDefn,
        !ModuleInfo) :-
    ToWrite = typeclass_status_to_write(ClassDefn0 ^ classdefn_status),
    (
        ToWrite = yes,
        ClassDefn = ClassDefn0 ^ classdefn_status :=
            typeclass_status(status_exported),
        class_procs_to_pred_ids(ClassDefn ^ classdefn_method_ppids, PredIds),
        opt_export_preds(PredIds, !ModuleInfo)
    ;
        ToWrite = no,
        ClassDefn = ClassDefn0
    ).

:- pred class_procs_to_pred_ids(list(pred_proc_id)::in, list(pred_id)::out)
    is det.

class_procs_to_pred_ids(ClassProcs, PredIds) :-
    PredIds0 = list.map(pred_proc_id_project_pred_id, ClassProcs),
    list.sort_and_remove_dups(PredIds0, PredIds).

%---------------------%

:- pred maybe_opt_export_instances(module_info::in, module_info::out) is det.

maybe_opt_export_instances(!ModuleInfo) :-
    module_info_get_instance_table(!.ModuleInfo, Instances0),
    map.to_assoc_list(Instances0, InstanceAL0),
    list.map_foldl(maybe_opt_export_class_instances, InstanceAL0, InstanceAL,
        !ModuleInfo),
    map.from_sorted_assoc_list(InstanceAL, Instances),
    module_info_set_instance_table(Instances, !ModuleInfo).

:- pred maybe_opt_export_class_instances(
    pair(class_id, list(hlds_instance_defn))::in,
    pair(class_id, list(hlds_instance_defn))::out,
    module_info::in, module_info::out) is det.

maybe_opt_export_class_instances(ClassId - InstanceList0,
        ClassId - InstanceList, !ModuleInfo) :-
    list.map_foldl(maybe_opt_export_instance_defn, InstanceList0, InstanceList,
        !ModuleInfo).

:- pred maybe_opt_export_instance_defn(hlds_instance_defn::in,
    hlds_instance_defn::out, module_info::in, module_info::out) is det.

maybe_opt_export_instance_defn(Instance0, Instance, !ModuleInfo) :-
    Instance0 = hlds_instance_defn(InstanceModule, Types, OriginalTypes,
        InstanceStatus0, Context, MaybeSubsumedContext, Constraints,
        Body, MaybeMethodPPIds, TVarSet, ConstraintProofs),
    ToWrite = instance_status_to_write(InstanceStatus0),
    (
        ToWrite = yes,
        InstanceStatus = instance_status(status_exported),
        Instance = hlds_instance_defn(InstanceModule, Types, OriginalTypes,
            InstanceStatus, Context, MaybeSubsumedContext, Constraints,
            Body, MaybeMethodPPIds, TVarSet, ConstraintProofs),
        (
            MaybeMethodPPIds = yes(MethodPPIds),
            class_procs_to_pred_ids(MethodPPIds, PredIds),
            opt_export_preds(PredIds, !ModuleInfo)
        ;
            % This can happen if an instance has multiple
            % declarations, one of which is abstract.
            MaybeMethodPPIds = no
        )
    ;
        ToWrite = no,
        Instance = Instance0
    ).

%---------------------%

:- pred opt_export_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

opt_export_preds(PredIds, !ModuleInfo) :-
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    opt_export_preds_in_pred_id_table(PredIds, PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

:- pred opt_export_preds_in_pred_id_table(list(pred_id)::in,
    pred_id_table::in, pred_id_table::out) is det.

opt_export_preds_in_pred_id_table([], !PredIdTable).
opt_export_preds_in_pred_id_table([PredId | PredIds], !PredIdTable) :-
    map.lookup(!.PredIdTable, PredId, PredInfo0),
    pred_info_get_status(PredInfo0, PredStatus0),
    ToWrite = pred_status_to_write(PredStatus0),
    (
        ToWrite = yes,
        ( if
            pred_info_get_origin(PredInfo0, Origin),
            Origin = origin_special_pred(spec_pred_unify, _)
        then
            PredStatus = pred_status(status_pseudo_exported)
        else if
            PredStatus0 = pred_status(status_external(_))
        then
            PredStatus = pred_status(status_external(status_opt_exported))
        else
            PredStatus = pred_status(status_opt_exported)
        ),
        pred_info_set_status(PredStatus, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredIdTable)
    ;
        ToWrite = no
    ),
    opt_export_preds_in_pred_id_table(PredIds, !PredIdTable).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod.
%---------------------------------------------------------------------------%
