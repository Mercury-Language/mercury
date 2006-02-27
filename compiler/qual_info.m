%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: qual_info.m.
% Main author: fjh.

%-----------------------------------------------------------------------------%

:- module hlds__make_hlds__qual_info.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module recompilation.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type qual_info.

:- pred init_qual_info(mq_info::in, eqv_map::in, qual_info::out) is det.

    % Update the qual_info when processing a new clause.
:- pred update_qual_info(tvar_name_map::in, tvarset::in,
    vartypes::in, import_status::in,
    qual_info::in, qual_info::out) is det.

:- pred qual_info_get_tvarset(qual_info::in, tvarset::out) is det.
:- pred qual_info_get_var_types(qual_info::in, vartypes::out) is det.
:- pred qual_info_get_mq_info(qual_info::in, mq_info::out) is det.
:- pred qual_info_get_import_status(qual_info::in, import_status::out) is det.
:- pred qual_info_get_found_syntax_error(qual_info::in, bool::out) is det.

:- pred qual_info_set_mq_info(mq_info::in, qual_info::in, qual_info::out)
    is det.
:- pred qual_info_set_var_types(vartypes::in, qual_info::in, qual_info::out)
    is det.
:- pred qual_info_set_found_syntax_error(bool::in,
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
    qual_info::in, qual_info::out, io::di, io::uo) is det.

:- pred make_atomic_unification(prog_var::in, unify_rhs::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, purity::in, hlds_goal::out,
    qual_info::in, qual_info::out) is det.

    % As above, except with default purity pure.
    %
:- pred make_atomic_unification(prog_var::in, unify_rhs::in, prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, hlds_goal::out,
    qual_info::in, qual_info::out) is det.

:- pred record_called_pred_or_func(pred_or_func::in, sym_name::in, arity::in,
    qual_info::in, qual_info::out) is det.

:- pred construct_pred_or_func_call(pred_id::in, pred_or_func::in,
    sym_name::in, list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
    qual_info::in, qual_info::out) is det.

:- pred do_construct_pred_or_func_call(pred_id::in, pred_or_func::in,
    sym_name::in, list(prog_var)::in, hlds_goal_info::in, hlds_goal::out)
    is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module std_util.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Information used to process explicit type qualifications.
    %
:- type qual_info
    --->    qual_info(
                eqv_map             :: eqv_map,
                                    % Used to expand equivalence types.

                tvarset             :: tvarset,
                                    % All type variables for predicate.

                tvar_renaming       :: tvar_renaming,
                                    % Map from clause type variable to
                                    % actual type variable in tvarset.

                tvar_name_map       :: tvar_name_map,
                                    % Type variables in tvarset occurring
                                    % in the predicate's argument types
                                    % indexed by name.

                vartypes            :: vartypes,

                mq_info             :: mq_info,
                                    % Module qualification info.

                import_status       :: import_status,

                found_syntax_error  :: bool
                                    % Was there a syntax error in an Aditi
                                    % update.
            ).

init_qual_info(MQInfo0, EqvMap, QualInfo) :-
    mq_info_set_need_qual_flag(may_be_unqualified, MQInfo0, MQInfo),
    varset__init(TVarSet),
    map__init(Renaming),
    map__init(Index),
    map__init(VarTypes),
    FoundSyntaxError = no,
    QualInfo = qual_info(EqvMap, TVarSet, Renaming, Index, VarTypes,
        MQInfo, local, FoundSyntaxError).

update_qual_info(TVarNameMap, TVarSet, VarTypes, Status, !QualInfo) :-
    !.QualInfo = qual_info(EqvMap, _TVarSet0, _Renaming0, _TVarNameMap0,
        _VarTypes0, MQInfo, _Status, _FoundError),
    % The renaming for one clause is useless in the others.
    map__init(Renaming),
    !:QualInfo = qual_info(EqvMap, TVarSet, Renaming, TVarNameMap,
        VarTypes, MQInfo, Status, no).

qual_info_get_tvarset(Info, Info ^ tvarset).
qual_info_get_var_types(Info, Info ^ vartypes).
qual_info_get_mq_info(Info, Info ^ mq_info).
qual_info_get_import_status(Info, Info ^ import_status).
qual_info_get_found_syntax_error(Info, Info ^ found_syntax_error).

qual_info_set_mq_info(MQInfo, Info, Info ^ mq_info := MQInfo).
qual_info_set_var_types(VarTypes, Info, Info ^ vartypes := VarTypes).
qual_info_set_found_syntax_error(FoundError, Info,
    Info ^ found_syntax_error := FoundError).

apply_to_recompilation_info(Pred, !QualInfo) :-
    MQInfo0 = !.QualInfo ^ mq_info,
    mq_info_get_recompilation_info(MQInfo0, MaybeRecompInfo0),
    (
        MaybeRecompInfo0 = yes(RecompInfo0),
        Pred(RecompInfo0, RecompInfo),
        mq_info_set_recompilation_info(yes(RecompInfo), MQInfo0, MQInfo),
        !:QualInfo = !.QualInfo ^ mq_info := MQInfo
    ;
        MaybeRecompInfo0 = no
    ).

set_module_recompilation_info(QualInfo, !ModuleInfo) :-
    mq_info_get_recompilation_info(QualInfo ^ mq_info, RecompInfo),
    module_info_set_maybe_recompilation_info(RecompInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

process_type_qualification(Var, Type0, VarSet, Context, !ModuleInfo,
        !QualInfo, !IO) :-
    !.QualInfo = qual_info(EqvMap, TVarSet0, TVarRenaming0,
        TVarNameMap0, VarTypes0, MQInfo0, Status, FoundError),
    ( Status = opt_imported ->
        % Types in `.opt' files should already be fully module qualified.
        Type1 = Type0,
        MQInfo = MQInfo0
    ;
        module_qual__qualify_type_qualification(Type0, Type1,
            Context, MQInfo0, MQInfo, !IO)
    ),

    % Find any new type variables introduced by this type, and
    % add them to the var-name index and the variable renaming.
    prog_type__vars(Type1, TVars),
    get_new_tvars(TVars, VarSet, TVarSet0, TVarSet1,
        TVarNameMap0, TVarNameMap, TVarRenaming0, TVarRenaming),

    % Apply the updated renaming to convert type variables in
    % the clause to type variables in the tvarset.
    apply_variable_renaming_to_type(TVarRenaming, Type1, Type2),

    % Expand equivalence types.
    % We don't need to record the expanded types for smart recompilation
    % because at the moment no recompilation.item_id can depend on a
    % clause item.
    RecordExpanded = no,
    equiv_type__replace_in_type(EqvMap, Type2, Type, _, TVarSet1, TVarSet,
        RecordExpanded, _),
    update_var_types(Var, Type, Context, VarTypes0, VarTypes, !IO),
    !:QualInfo = qual_info(EqvMap, TVarSet, TVarRenaming,
        TVarNameMap, VarTypes, MQInfo, Status, FoundError).

:- pred update_var_types(prog_var::in, mer_type::in, prog_context::in,
    vartypes::in, vartypes::out, io::di, io::uo) is det.

update_var_types(Var, Type, Context, !VarTypes, !IO) :-
    ( map.search(!.VarTypes, Var, Type0) ->
        ( Type = Type0 ->
            true
        ;
            ErrMsg = [
                words("Error: explicit type qualification does"),
                words("not match prior qualification.")
            ],
            write_error_pieces(Context, 0, ErrMsg, !IO),
            io__set_exit_status(1, !IO)
        )
    ;
        svmap.det_insert(Var, Type, !VarTypes)
    ).

%-----------------------------------------------------------------------------%

make_atomic_unification(Var, Rhs, Context, MainContext, SubContext,
        Goal, !QualInfo) :-
    make_atomic_unification(Var, Rhs, Context, MainContext, SubContext,
            purity_pure, Goal, !QualInfo).

make_atomic_unification(Var, Rhs, Context, MainContext, SubContext, Purity,
        Goal, !QualInfo) :-
    (
        Rhs = var(_)
    ;
        Rhs = lambda_goal(_, _, _, _, _, _, _, _)
    ;
        Rhs = functor(ConsId, _, _),
        record_used_functor(ConsId, !QualInfo)
    ),
    create_atomic_complicated_unification(Var, Rhs, Context,
        MainContext, SubContext, Purity, Goal).

record_called_pred_or_func(PredOrFunc, SymName, Arity, !QualInfo) :-
    Id = SymName - Arity,
    apply_to_recompilation_info(recompilation__record_used_item(
        pred_or_func_to_item_type(PredOrFunc), Id, Id), !QualInfo).

:- pred record_used_functor(cons_id::in, qual_info::in, qual_info::out) is det.

record_used_functor(ConsId, !QualInfo) :-
    ( ConsId = cons(SymName, Arity) ->
        Id = SymName - Arity,
        apply_to_recompilation_info(record_used_item(functor_item, Id, Id),
            !QualInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args, GoalInfo, Goal,
        !QualInfo) :-
    do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
        GoalInfo, Goal),
    list__length(Args, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    record_called_pred_or_func(PredOrFunc, SymName, OrigArity, !QualInfo).

do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
        GoalInfo, Goal) :-
    (
        PredOrFunc = predicate,
        Goal = call(PredId, invalid_proc_id, Args, not_builtin, no, SymName)
            - GoalInfo
    ;
        PredOrFunc = function,
        pred_args_to_func_args(Args, FuncArgs, RetArg),
        list__length(FuncArgs, Arity),
        ConsId = cons(SymName, Arity),
        goal_info_get_context(GoalInfo, Context),
        create_atomic_complicated_unification(RetArg,
            functor(ConsId, no, FuncArgs), Context, explicit, [],
            GoalExpr - _),
        Goal = GoalExpr - GoalInfo
    ).

%-----------------------------------------------------------------------------%
:- end_module qual_info.
%-----------------------------------------------------------------------------%
