%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2000,2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Utility predicates used in two or more of the modules concerned with
% determinism: switch_detection, cse_detection, det_analysis, det_report
% and simplify.
%
% Main authors: fjh and zs.
%
%-----------------------------------------------------------------------------%

:- module check_hlds__det_util.

:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module libs__globals.
:- import_module parse_tree__prog_data.

:- import_module bool.
:- import_module list.
:- import_module set.

:- type maybe_changed   --->    changed ; unchanged.

:- type det_info.

    % Given a list of cases, and a list of the possible cons_ids
    % that the switch variable could be bound to, select out only
    % those cases whose cons_id occurs in the list of cases
    % We assume that the list of cases and the list of cons_ids
    % are sorted, so that we can do this using a simple sorted merge.
    %
:- pred delete_unreachable_cases(list(case)::in, list(cons_id)::in,
    list(case)::out) is det.

    % Update the current substitution to account for the effects
    % of the given unification.
    %
:- pred interpret_unify(prog_var::in, unify_rhs::in,
    prog_substitution::in, prog_substitution::out) is semidet.

    % Look up the determinism of a procedure.
    %
:- pred det_lookup_detism(det_info::in, pred_id::in, proc_id::in,
    determinism::out) is det.

:- pred det_get_proc_info(det_info::in, proc_info::out) is det.

:- pred det_lookup_var_type(module_info::in, proc_info::in, prog_var::in,
    hlds_type_defn::out) is semidet.

:- pred det_no_output_vars(set(prog_var)::in, instmap::in, instmap_delta::in,
    det_info::in) is semidet.

:- pred det_info_init(module_info::in, vartypes::in, pred_id::in, proc_id::in,
    globals::in, det_info::out) is det.

:- pred det_info_get_module_info(det_info::in, module_info::out) is det.
:- pred det_info_get_pred_id(det_info::in, pred_id::out) is det.
:- pred det_info_get_proc_id(det_info::in, proc_id::out) is det.
:- pred det_info_get_reorder_conj(det_info::in, bool::out) is det.
:- pred det_info_get_reorder_disj(det_info::in, bool::out) is det.
:- pred det_info_get_fully_strict(det_info::in, bool::out) is det.
:- pred det_info_get_vartypes(det_info::in, vartypes::out) is det.

:- pred det_info_set_module_info(det_info::in, module_info::in, det_info::out)
    is det.
:- pred det_info_set_vartypes(det_info::in, vartypes::in, det_info::out)
    is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__type_util.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module term.

delete_unreachable_cases([], _, []).
delete_unreachable_cases([_ | _], [], []).
delete_unreachable_cases([Case | Cases0], [ConsId | ConsIds], Cases) :-
    Case = case(CaseConsId, _DisjList),
    ( CaseConsId = ConsId ->
        Cases = [Case | Cases1],
        delete_unreachable_cases(Cases0, ConsIds, Cases1)
    ; compare(<, CaseConsId, ConsId) ->
        delete_unreachable_cases(Cases0, [ConsId | ConsIds], Cases)
    ;
        delete_unreachable_cases([Case | Cases0], ConsIds, Cases)
    ).

interpret_unify(X, var(Y), !Subst) :-
    term__unify(term__variable(X), term__variable(Y), !Subst).
interpret_unify(X, functor(ConsId, _, ArgVars), !Subst) :-
    term__var_list_to_term_list(ArgVars, ArgTerms),
    cons_id_and_args_to_term(ConsId, ArgTerms, RhsTerm),
    term__unify(term__variable(X), RhsTerm, !Subst).
interpret_unify(_X, lambda_goal(_, _, _, _, _, _, _, _, _), !Subst).
    % For ease of implementation we just ignore unifications with lambda terms.
    % This is a safe approximation, it just prevents us from optimizing them
    % as well as we would like.

det_lookup_detism(DetInfo, PredId, ModeId, Detism) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    module_info_preds(ModuleInfo, PredTable),
    map__lookup(PredTable, PredId, PredInfo),
    pred_info_procedures(PredInfo, ProcTable),
    map__lookup(ProcTable, ModeId, ProcInfo),
    proc_info_interface_determinism(ProcInfo, Detism).

det_get_proc_info(DetInfo, ProcInfo) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    det_info_get_pred_id(DetInfo, PredId),
    det_info_get_proc_id(DetInfo, ProcId),
    module_info_preds(ModuleInfo, PredTable),
    map__lookup(PredTable, PredId, PredInfo),
    pred_info_procedures(PredInfo, ProcTable),
    map__lookup(ProcTable, ProcId, ProcInfo).

det_lookup_var_type(ModuleInfo, ProcInfo, Var, TypeDefn) :-
    proc_info_vartypes(ProcInfo, VarTypes),
    map__lookup(VarTypes, Var, Type),
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        module_info_types(ModuleInfo, TypeTable),
        map__search(TypeTable, TypeCtor, TypeDefn)
    ;
        unexpected(this_file, "det_lookup_var_type")
    ).

det_no_output_vars(Vars, InstMap, InstMapDelta, DetInfo) :-
    det_info_get_module_info(DetInfo, ModuleInfo),
    instmap__no_output_vars(InstMap, InstMapDelta, Vars,
        DetInfo ^ vartypes, ModuleInfo).

%-----------------------------------------------------------------------------%

:- type det_info
    --->    det_info(
                module_info     :: module_info,
                vartypes        :: vartypes,
                pred_id         :: pred_id,     % the id of the proc
                proc_id         :: proc_id,     % currently processed
                reorder_conj    :: bool,        % --reorder-conj
                reorder_disj    :: bool,        % --reorder-disj
                fully_strict    :: bool         % --fully-strict
            ).

det_info_init(ModuleInfo, VarTypes, PredId, ProcId, Globals, DetInfo) :-
    globals__lookup_bool_option(Globals, reorder_conj, ReorderConj),
    globals__lookup_bool_option(Globals, reorder_disj, ReorderDisj),
    globals__lookup_bool_option(Globals, fully_strict, FullyStrict),
    DetInfo = det_info(ModuleInfo, VarTypes, PredId, ProcId,
        ReorderConj, ReorderDisj, FullyStrict).

det_info_get_module_info(DI, DI ^ module_info).
det_info_get_pred_id(DI, DI ^ pred_id).
det_info_get_proc_id(DI, DI ^ proc_id).
det_info_get_reorder_conj(DI, DI ^ reorder_conj).
det_info_get_reorder_disj(DI, DI ^ reorder_disj).
det_info_get_fully_strict(DI, DI ^ fully_strict).
det_info_get_vartypes(DI, DI ^ vartypes).

det_info_set_module_info(DI, ModuleInfo, DI ^ module_info := ModuleInfo).
det_info_set_vartypes(DI, VarTypes, DI ^ vartypes := VarTypes).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "det_util.m".

%-----------------------------------------------------------------------------%
