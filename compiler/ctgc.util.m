%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2008, 2010-2012 The University of Melbourne.
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

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Check if some of the predicates are "special" predicates (as in
    % "special_pred_map" known from module_info) or not defined in the
    % current module, as these predicates are not analysed by the CTGC system.
    %
:- pred some_preds_require_no_analysis(module_info::in,
    set(pred_proc_id)::in) is semidet.

:- pred pred_requires_no_analysis(module_info::in, pred_id::in) is semidet.
:- pred pred_requires_analysis(module_info::in, pred_id::in) is semidet.

    % Given the pred_proc_id of a procedure call and its actual arguments,
    % determine the variable renaming to rename anything which is defined
    % in terms of the formal arguments of the called procedure to the context
    % of the actual arguments.
    %
:- func get_variable_renaming(module_info, pred_proc_id, prog_vars) =
    prog_var_renaming.

    % get_type_substitution(ModuleInfo, PPId, ActualTypes,
    %   CallerTypeVarSet, CallerExternalTypeParams) = TypeSubst
    %
    % Work out a type substitution to map the callee's argument types into the
    % caller's.
    %
:- func get_type_substitution(module_info, pred_proc_id, list(mer_type),
    tvarset, external_type_params) = tsubst.

    % var_needs_sharing_analysis(ModuleInfo, ProcInfo, Var).
    %
    % Succeed iff Var is of a type for which we need to consider structure
    % sharing.
    %
:- pred var_needs_sharing_analysis(module_info::in, proc_info::in,
    prog_var::in) is semidet.

    % Succeed iff the type is one for which we need to consider structure
    % sharing.
    %
:- pred type_needs_sharing_analysis(module_info::in, mer_type::in) is semidet.

    % Succeed iff values of the given type may have a top-level cell
    % that could be reused.
    %
:- pred top_cell_may_be_reusable(module_info::in, mer_type::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module bool.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

pred_requires_no_analysis(ModuleInfo, PredId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    % We handle `:- pragma external_{pred/func}' predicates and functions
    % later. In that sense, they do *not* require that we don't analyse them.
    PredStatus = pred_status(status_imported(_)).

pred_requires_analysis(ModuleInfo, PredId) :-
    not pred_requires_no_analysis(ModuleInfo, PredId).

some_preds_require_no_analysis(ModuleInfo, PPIds) :-
    set.member(proc(PredId, _), PPIds),
    pred_requires_no_analysis(ModuleInfo, PredId).

get_variable_renaming(ModuleInfo, PPId, ActualArgs) = VariableRenaming :-
    module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
    proc_info_get_headvars(ProcInfo, FormalVars),
    map.from_corresponding_lists(FormalVars, ActualArgs, VariableRenaming).

get_type_substitution(ModuleInfo, PPId, ActualTypes, CallerTypeVarSet,
        CallerExternalTypeParams) = TypeSubst :-
    PPId = proc(PredId, _),
    module_info_pred_info(ModuleInfo, PredId, CalleePredInfo),
    pred_info_get_typevarset(CalleePredInfo, CalleeTypeVarSet),
    pred_info_get_arg_types(CalleePredInfo, CalleeArgTypes0),
    pred_info_get_exist_quant_tvars(CalleePredInfo, CalleeExistQVars),

    % Rename apart the type variables.  We don't care about the merged
    % typevarset.
    tvarset_merge_renaming(CallerTypeVarSet, CalleeTypeVarSet, _TypeVarSet,
        CalleeTypeVarRenaming),
    apply_variable_renaming_to_type_list(CalleeTypeVarRenaming,
        CalleeArgTypes0, CalleeArgTypes),

    (
        CalleeExistQVars = [],
        ( if type_list_subsumes(CalleeArgTypes, ActualTypes, TypeSubst0) then
            TypeSubst1 = TypeSubst0
        else
            % See comment in inlining.get_type_substitution.
            TypeSubst1 = map.init
        )
    ;
        CalleeExistQVars = [_ | _],
        % XXX from inlining.m:
        % "For calls to existentially type preds, we may need to bind
        % type variables in the caller, not just those in the callee."
        % We don't do that (yet?).
        ( if
            map.init(TypeSubstPrime),
            type_unify_list(CalleeArgTypes, ActualTypes,
                CallerExternalTypeParams, TypeSubstPrime, TypeSubst0)
        then
            TypeSubst1 = TypeSubst0
        else
            unexpected($pred, "type unification failed")
        )
    ),

    % TypeSubst1 is a substitition for the merged typevarset.  We apply the
    % reverse of CalleeTypeVarRenaming to get TypeSubst, a substitition for
    % the callee typevarset.
    % XXX preferably, we wouldn't need to do this reverse renaming
    map.keys(CalleeTypeVarRenaming, CalleeTypeVarRenamingKeys),
    map.values(CalleeTypeVarRenaming, CalleeTypeVarRenamingValues),
    map.from_corresponding_lists(CalleeTypeVarRenamingValues,
        CalleeTypeVarRenamingKeys, RevCalleeTypeVarRenaming),
    map.foldl(reverse_renaming(RevCalleeTypeVarRenaming), TypeSubst1,
        map.init, TypeSubst).

:- pred reverse_renaming(tvar_renaming::in, tvar::in, mer_type::in,
    tsubst::in, tsubst::out) is det.

reverse_renaming(RevSubst, K0, V0, !Acc) :-
    apply_variable_renaming_to_tvar(RevSubst, K0, K),
    apply_variable_renaming_to_type(RevSubst, V0, V),
    map.det_insert(K, V, !Acc).

%-----------------------------------------------------------------------------%

var_needs_sharing_analysis(ModuleInfo, ProcInfo, Var) :-
    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    type_needs_sharing_analysis(ModuleInfo, Type).

type_needs_sharing_analysis(ModuleInfo, Type) :-
    TypeCat = classify_type(ModuleInfo, Type),
    type_category_needs_sharing_analysis(TypeCat) = yes.

:- func type_category_needs_sharing_analysis(type_ctor_category) = bool.

type_category_needs_sharing_analysis(CtorCat) = NeedsSharingAnalysis :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ),
        NeedsSharingAnalysis = no
    ;
        ( CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        NeedsSharingAnalysis = yes
    ).

top_cell_may_be_reusable(ModuleInfo, Type) :-
    TypeCat = classify_type(ModuleInfo, Type),
    type_category_top_cell_may_be_reusable(TypeCat) = yes.

:- func type_category_top_cell_may_be_reusable(type_ctor_category) = bool.

type_category_top_cell_may_be_reusable(CtorCat) = Reusable :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ),
        Reusable = no
    ;
        ( CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        Reusable = yes
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.util.
%-----------------------------------------------------------------------------%
