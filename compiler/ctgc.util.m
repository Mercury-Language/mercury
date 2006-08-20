%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.util.m.
% Main author: nancy.
%
% Utility operations for the CTGC-system.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.util.
:- interface.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Check if some of the predicates are "special" predicates (as in
    % "special_pred_map" known from module_info) or not defined in the
    % current module, as these predicates are not analysed by the CTGC
    % system.
    %
:- pred preds_requiring_no_analysis(module_info::in, list(pred_proc_id)::in)
    is semidet.

:- pred pred_requires_no_analysis(module_info::in, pred_id::in) is semidet.
:- pred pred_requires_analysis(module_info::in, pred_id::in) is semidet.

    % Given the pred_proc_id of a procedure call and its actual arguments,
    % determine the variable renaming to rename anything which is defined
    % in terms of the formal arguments of the called procedure to the context
    % of the actual arguments.
    %
:- func get_variable_renaming(module_info, pred_proc_id, prog_vars) =
    prog_var_renaming.

    % Same as above, but then in the context of the types of the called
    % procedures.
    %
:- func get_type_substitution(module_info, pred_proc_id,
    list(mer_type), tvarset) = tsubst.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module string.

pred_requires_no_analysis(ModuleInfo, PredId) :-
    module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
    map.values(SpecialPredMap, SpecialPreds),
    (
        list.member(PredId, SpecialPreds)
    ;
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_import_status(PredInfo, Status),
        status_defined_in_this_module(Status) = no
    ).

pred_requires_analysis(ModuleInfo, PredId) :-
    \+ pred_requires_no_analysis(ModuleInfo, PredId).

:- func get_pred_id(pred_proc_id) = pred_id.
get_pred_id(proc(PredId, _)) = PredId.

preds_requiring_no_analysis(ModuleInfo, PPIds) :-
    list.takewhile(pred_requires_analysis(ModuleInfo),
        list.map(get_pred_id, PPIds), _RequiresAnalysis, RequiresNoAnalysis),
    RequiresNoAnalysis = [_|_].

:- pred not_defined_in_this_module(module_info::in, pred_proc_id::in)
    is semidet.

not_defined_in_this_module(ModuleInfo, proc(PredId, _)):-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, Status),
    status_defined_in_this_module(Status) = no.

get_variable_renaming(ModuleInfo, PPId, ActualArgs) = VariableRenaming :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),

    % head variables.
    proc_info_get_headvars(ProcInfo, FormalVars),
    map.from_corresponding_lists(FormalVars, ActualArgs, VariableRenaming).

get_type_substitution(ModuleInfo, PPId, ActualTypes, _TVarSet) =
        TypeSubstitution :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _ProcInfo),

    % types of the head variables.
    pred_info_get_arg_types(PredInfo, FormalTypes),

    type_list_subsumes_det(FormalTypes, ActualTypes, TypeSubstitution).

%-----------------------------------------------------------------------------%

:- func this_file = string.
this_file = "ctgc.util.m".

:- end_module transform_hlds.ctgc.util.
