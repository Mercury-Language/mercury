%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: qual_info.m.
% Main author: fjh.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.qual_info.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.vartypes.
:- import_module recompilation.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type qual_info.

:- pred init_qual_info(mq_info::in, type_eqv_map::in, qual_info::out) is det.

    % Update the qual_info when processing a new clause.
    %
:- pred update_qual_info(tvar_name_map::in, tvarset::in, vartypes::in,
    maybe_opt_imported::in, qual_info::in, qual_info::out) is det.

:- pred qual_info_get_tvarset(qual_info::in, tvarset::out) is det.
:- pred qual_info_get_explicit_var_types(qual_info::in, vartypes::out) is det.
:- pred qual_info_get_mq_info(qual_info::in, mq_info::out) is det.
:- pred qual_info_get_maybe_opt_imported(qual_info::in,
    maybe_opt_imported::out) is det.
:- pred qual_info_get_found_syntax_error(qual_info::in, bool::out) is det.
:- pred qual_info_get_found_trace_goal(qual_info::in, bool::out) is det.

:- pred qual_info_set_explicit_var_types(vartypes::in,
    qual_info::in, qual_info::out) is det.
:- pred qual_info_set_mq_info(mq_info::in,
    qual_info::in, qual_info::out) is det.
:- pred qual_info_set_found_syntax_error(bool::in,
    qual_info::in, qual_info::out) is det.
:- pred qual_info_set_found_trace_goal(bool::in,
    qual_info::in, qual_info::out) is det.

:- pred apply_to_recompilation_info(
    pred(recompilation_info, recompilation_info)::in(pred(in, out) is det),
    qual_info::in, qual_info::out) is det.

    % Move the recompilation_info from the qual_info to the module_info
    % after make_hlds is finished with it and the qual_info is dead.
    %
:- pred set_module_recompilation_info(qual_info::in,
    module_info::in, module_info::out) is det.

    % Process an explicit type qualification.
    %
:- pred process_type_qualification(prog_var::in, mer_type::in, tvarset::in,
    prog_context::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred make_atomic_unification(prog_var::in, unify_rhs::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in, hlds_goal::out,
    qual_info::in, qual_info::out) is det.

    % As above, except with default purity pure.
    %
:- pred make_atomic_unification(prog_var::in, unify_rhs::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, hlds_goal::out,
    qual_info::in, qual_info::out) is det.

:- pred record_called_pred_or_func(pred_or_func::in, sym_name::in,
    user_arity::in, qual_info::in, qual_info::out) is det.

:- pred construct_and_record_pred_or_func_call(pred_id::in, pred_or_func::in,
    sym_name::in, list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
    qual_info::in, qual_info::out) is det.

:- pred construct_pred_or_func_call(pred_id::in, pred_or_func::in,
    sym_name::in, list(prog_var)::in, hlds_goal_info::in, hlds_goal::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.make_goal.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Information used to process explicit type qualifications.
    %
:- type qual_info
    --->    qual_info(
                % Used to expand equivalence types.
                qual_type_eqv_map       :: type_eqv_map,

                % All type variables for predicate.
                qual_tvarset            :: tvarset,

                % Map from clause type variable to actual type variable
                % in tvarset.
                qual_tvar_renaming      :: tvar_renaming,

                % Type variables in tvarset occurring in the predicate's
                % argument types indexed by name.
                qual_tvar_name_map      :: tvar_name_map,

                qual_explicit_vartypes  :: vartypes,

                % Module qualification info.
                qual_mq_info            :: mq_info,

                qual_maybe_opt_imported :: maybe_opt_imported,

                % Was there a syntax error in the clause?
                qual_found_syntax_error :: bool,

                % Was there a trace goal in the clause?
                qual_found_trace_goal   :: bool
            ).

init_qual_info(MQInfo, TypeEqvMap, QualInfo) :-
    varset.init(TVarSet),
    map.init(Renaming),
    map.init(Index),
    init_vartypes(VarTypes),
    FoundSyntaxError = no,
    FoundTraceGoal = no,
    QualInfo = qual_info(TypeEqvMap, TVarSet, Renaming, Index, VarTypes,
        MQInfo, is_not_opt_imported, FoundSyntaxError, FoundTraceGoal).

update_qual_info(TVarNameMap, TVarSet, VarTypes, MaybeOptImported,
        !QualInfo) :-
    !.QualInfo = qual_info(TypeEqvMap, _TVarSet0, _Renaming0, _TVarNameMap0,
        _VarTypes0, MQInfo, _MaybeOptImported, _FoundError, _FoundTraceGoal),
    % The renaming for one clause is useless in the others.
    map.init(Renaming),
    !:QualInfo = qual_info(TypeEqvMap, TVarSet, Renaming, TVarNameMap,
        VarTypes, MQInfo, MaybeOptImported, no, no).

qual_info_get_tvarset(Info, X) :-
    X = Info ^ qual_tvarset.
qual_info_get_explicit_var_types(Info, X) :-
    X = Info ^ qual_explicit_vartypes.
qual_info_get_mq_info(Info, X) :-
    X = Info ^ qual_mq_info.
qual_info_get_maybe_opt_imported(Info, X) :-
    X = Info ^ qual_maybe_opt_imported.
qual_info_get_found_syntax_error(Info, X) :-
    X = Info ^ qual_found_syntax_error.
qual_info_get_found_trace_goal(Info, X) :-
    X = Info ^ qual_found_trace_goal.

qual_info_set_explicit_var_types(X, !Info) :-
    !Info ^ qual_explicit_vartypes := X.
qual_info_set_mq_info(X, !Info) :-
    !Info ^ qual_mq_info := X.
qual_info_set_found_syntax_error(X, !Info) :-
    !Info ^ qual_found_syntax_error := X.
qual_info_set_found_trace_goal(X, !Info) :-
    !Info ^ qual_found_trace_goal := X.

%-----------------------------------------------------------------------------%

apply_to_recompilation_info(Pred, !QualInfo) :-
    qual_info_get_mq_info(!.QualInfo, MQInfo0),
    mq_info_get_recompilation_info(MQInfo0, MaybeRecompInfo0),
    (
        MaybeRecompInfo0 = yes(RecompInfo0),
        Pred(RecompInfo0, RecompInfo),
        mq_info_set_recompilation_info(yes(RecompInfo), MQInfo0, MQInfo),
        qual_info_set_mq_info(MQInfo, !QualInfo)
    ;
        MaybeRecompInfo0 = no
    ).

set_module_recompilation_info(QualInfo, !ModuleInfo) :-
    qual_info_get_mq_info(QualInfo, MQInfo),
    mq_info_get_recompilation_info(MQInfo, RecompInfo),
    module_info_set_maybe_recompilation_info(RecompInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

process_type_qualification(Var, Type0, VarSet, Context, !ModuleInfo,
        !QualInfo, !Specs) :-
    !.QualInfo = qual_info(TypeEqvMap, TVarSet0, TVarRenaming0,
        TVarNameMap0, VarTypes0, MQInfo0, MaybeOptImported,
        FoundSyntaxError, FoundTraceGoal),
    (
        MaybeOptImported = is_opt_imported,
        % Types in `.opt' files should already be fully module qualified.
        Type1 = Type0,
        MQInfo = MQInfo0
    ;
        MaybeOptImported = is_not_opt_imported,
        % Type qualifications cannot appear in the interface of a module.
        qualify_type_qualification(mq_not_used_in_interface, Context,
            Type0, Type1, MQInfo0, MQInfo, !Specs)
    ),

    % Find any new type variables introduced by this type, and
    % add them to the var-name index and the variable renaming.
    type_vars_in_type(Type1, TVars),
    get_new_tvars(TVars, VarSet, TVarSet0, TVarSet1,
        TVarNameMap0, TVarNameMap, TVarRenaming0, TVarRenaming),

    % Apply the updated renaming to convert type variables in
    % the clause to type variables in the tvarset.
    apply_variable_renaming_to_type(TVarRenaming, Type1, Type2),

    % Expand equivalence types.
    % We don't need to record the expanded types for smart recompilation
    % because at the moment no recompilation.item_id can depend on a
    % clause item.
    ExpandInfo0 = no_eqv_expand_info,
    equiv_type.replace_in_type(TypeEqvMap, Type2, Type, _, TVarSet1, TVarSet,
        ExpandInfo0, _),
    update_var_types(Var, Type, Context, VarTypes0, VarTypes, !Specs),
    !:QualInfo = qual_info(TypeEqvMap, TVarSet, TVarRenaming,
        TVarNameMap, VarTypes, MQInfo, MaybeOptImported,
        FoundSyntaxError, FoundTraceGoal).

:- pred update_var_types(prog_var::in, mer_type::in, prog_context::in,
    vartypes::in, vartypes::out,
    list(error_spec)::in, list(error_spec)::out) is det.

update_var_types(Var, Type, Context, !VarTypes, !Specs) :-
    ( if search_var_type(!.VarTypes, Var, Type0) then
        ( if Type = Type0 then
            true
        else
            Pieces = [words("Error: explicit type qualification"),
                words("does not match prior qualification."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        add_var_type(Var, Type, !VarTypes)
    ).

%-----------------------------------------------------------------------------%

make_atomic_unification(Var, RHS, Context, MainContext, SubContext, Purity,
        Goal, !QualInfo) :-
    (
        RHS = rhs_var(_)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _)
    ;
        RHS = rhs_functor(ConsId, _, _),
        record_used_functor(ConsId, !QualInfo)
    ),
    create_atomic_complicated_unification(Var, RHS, Context,
        MainContext, SubContext, Purity, Goal).

make_atomic_unification(Var, RHS, Context, MainContext, SubContext,
        Goal, !QualInfo) :-
    make_atomic_unification(Var, RHS, Context, MainContext, SubContext,
        purity_pure, Goal, !QualInfo).

record_called_pred_or_func(PredOrFunc, SymName, UserArity, !QualInfo) :-
    UserArity = user_arity(UserArityInt),
    Id = recomp_item_name(SymName, UserArityInt),
    ( PredOrFunc = pf_predicate, UsedItemType = used_predicate
    ; PredOrFunc = pf_function,  UsedItemType = used_function
    ),
    apply_to_recompilation_info(
        recompilation.record_used_item(UsedItemType, Id, Id), !QualInfo).

:- pred record_used_functor(cons_id::in, qual_info::in, qual_info::out) is det.

record_used_functor(ConsId, !QualInfo) :-
    ( if ConsId = cons(SymName, Arity, _) then
        Id = recomp_item_name(SymName, Arity),
        apply_to_recompilation_info(record_used_item(used_functor, Id, Id),
            !QualInfo)
    else
        true
    ).

%-----------------------------------------------------------------------------%

construct_and_record_pred_or_func_call(PredId, PredOrFunc, SymName, ArgVars,
        GoalInfo, Goal, !QualInfo) :-
    construct_pred_or_func_call(PredId, PredOrFunc, SymName, ArgVars,
        GoalInfo, Goal),
    PredFormArity = arg_list_arity(ArgVars),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    record_called_pred_or_func(PredOrFunc, SymName, UserArity, !QualInfo).

construct_pred_or_func_call(PredId, PredOrFunc, SymName, ArgVars,
        GoalInfo, Goal) :-
    (
        PredOrFunc = pf_predicate,
        GoalExpr = plain_call(PredId, invalid_proc_id, ArgVars, not_builtin,
            no, SymName),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(ArgVars, FuncArgVars, RetArgVar),
        list.length(FuncArgVars, Arity),
        TypeCtor = cons_id_dummy_type_ctor,
        ConsId = cons(SymName, Arity, TypeCtor),
        Context = goal_info_get_context(GoalInfo),
        RHS = rhs_functor(ConsId, is_not_exist_constr, FuncArgVars),
        create_pure_atomic_complicated_unification(RetArgVar, RHS,
            Context, umc_explicit, [], hlds_goal(GoalExpr, _)),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.qual_info.
%-----------------------------------------------------------------------------%
