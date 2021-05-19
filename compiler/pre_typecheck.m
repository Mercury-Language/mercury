%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pre_typecheck.m.
%
% Prepare the clauses in the HLDS for typechecking. This involves three steps.
%
% - We invoke goal_path.m to fill in the goal_id slots of goals.
%   We record goal_ids in constraint_ids, which allows us later to map
%   constraints back to the goal being constrained.
%
% - We supply field access functions with their default clauses, if the
%   user has not explicitly provided clauses for them. This pass is the
%   compiler's first chance to decide that there won't be any user-given
%   clauses coming, and that therefore the default clauses should be
%   the actual clauses.
%
% - The parser puts clauses into superhomogeneous form by filling the heads
%   of clauses with compiler-generated variables with compiler-generated names
%   such as HeadVar__1. We invoke headvar_names.m to give these variables
%   their user-provided names, provided the clauses agree on the names.
%   This is important for making any type error messages about these variables
%   easier to understand.
%
%---------------------------------------------------------------------------%

:- module check_hlds.pre_typecheck.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- pred prepare_for_typecheck_module(module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.headvar_names.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_goal.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set_tree234.
:- import_module term.

%---------------------------------------------------------------------------%

prepare_for_typecheck_module(!ModuleInfo) :-
    module_info_get_valid_pred_id_set(!.ModuleInfo, OrigValidPredIdSet),
    module_info_get_preds(!.ModuleInfo, PredMap0),
    map.to_sorted_assoc_list(PredMap0, PredIdsInfos0),

    prepare_for_typecheck(!.ModuleInfo, OrigValidPredIdSet,
        PredIdsInfos0, PredIdsInfos),

    map.from_sorted_assoc_list(PredIdsInfos, PredMap),
    module_info_set_preds(PredMap, !ModuleInfo).

:- pred prepare_for_typecheck(module_info::in, set_tree234(pred_id)::in,
    assoc_list(pred_id, pred_info)::in, assoc_list(pred_id, pred_info)::out)
    is det.

prepare_for_typecheck(_, _, [], []).
prepare_for_typecheck(ModuleInfo, ValidPredIdSet,
        [PredIdInfo0 | PredIdsInfos0], [PredIdInfo | PredIdsInfos]) :-
    some [!PredInfo] (
        PredIdInfo0 = PredId - !:PredInfo,
        ( if set_tree234.contains(ValidPredIdSet, PredId) then
            % Goal ids are used to identify typeclass constraints.
            pred_info_get_clauses_info(!.PredInfo, GoalIdClausesInfo0),
            fill_goal_id_slots_in_clauses(ModuleInfo, _ContainingGoalMap,
                GoalIdClausesInfo0, GoalIdClausesInfo),
            pred_info_set_clauses_info(GoalIdClausesInfo, !PredInfo),

            maybe_add_field_access_function_clause(ModuleInfo, !PredInfo),

            module_info_get_globals(ModuleInfo, Globals),
            maybe_improve_headvar_names(Globals, !PredInfo),
            PredIdInfo = PredId - !.PredInfo
        else
            PredIdInfo = PredIdInfo0
        )
    ),
    prepare_for_typecheck(ModuleInfo, ValidPredIdSet,
        PredIdsInfos0, PredIdsInfos).

%---------------------------------------------------------------------------%

    % For a field access function for which the user has supplied
    % a declaration but no clauses, add a clause
    % 'foo :='(X, Y) = 'foo :='(X, Y).
    % As for the default clauses added for builtins, this is not a recursive
    % call -- post_typecheck.m will expand the body into unifications.
    %
:- pred maybe_add_field_access_function_clause(module_info::in,
    pred_info::in, pred_info::out) is det.

maybe_add_field_access_function_clause(ModuleInfo, !PredInfo) :-
    pred_info_get_status(!.PredInfo, PredStatus),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo0),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers0),
    ( if
        pred_info_is_field_access_function(ModuleInfo, !.PredInfo),
        clause_list_is_empty(ClausesRep0) = yes,
        pred_status_defined_in_this_module(PredStatus) = yes
    then
        clauses_info_get_headvars(ClausesInfo0, HeadVars),
        proc_arg_vector_to_func_args(HeadVars, FuncArgs, FuncRetVal),
        pred_info_get_context(!.PredInfo, Context),
        FuncModule = pred_info_module(!.PredInfo),
        FuncName = pred_info_name(!.PredInfo),
        PredArity = pred_info_orig_arity(!.PredInfo),
        adjust_func_arity(pf_function, FuncArity, PredArity),
        FuncSymName = qualified(FuncModule, FuncName),
        FuncConsId = cons(FuncSymName, FuncArity, cons_id_dummy_type_ctor),
        FuncRHS = rhs_functor(FuncConsId, is_not_exist_constr, FuncArgs),
        create_pure_atomic_complicated_unification(FuncRetVal,
            FuncRHS, Context, umc_explicit, [], Goal0),
        Goal0 = hlds_goal(GoalExpr, GoalInfo0),
        NonLocals = set_of_var.list_to_set(proc_arg_vector_to_list(HeadVars)),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Clause = clause(all_modes, Goal, impl_lang_mercury, Context, []),
        set_clause_list([Clause], ClausesRep),
        ItemNumbers = init_clause_item_numbers_comp_gen,
        clauses_info_set_clauses_rep(ClausesRep, ItemNumbers,
            ClausesInfo0, ClausesInfo),
        pred_info_update_goal_type(np_goal_type_clause, !PredInfo),
        pred_info_set_clauses_info(ClausesInfo, !PredInfo),
        pred_info_get_markers(!.PredInfo, Markers0),
        add_marker(marker_calls_are_fully_qualified, Markers0, Markers),
        pred_info_set_markers(Markers, !PredInfo)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.pre_typecheck.
%---------------------------------------------------------------------------%
