%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2008-2012 The University of Melbourne.
% Copyright (C) 2012, 2014-2016, 2018-2020, 2022, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: field_access.
%
% This submodule of make_hlds handles the declarations of fields
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.field_access.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.make_hlds.state_var.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type field_list == assoc_list(sym_name, list(prog_term)).

    % Expand a field update goal into a list of goals which each get or set
    % one level of the structure.
    %
    % A field update goal:
    %   Term = Term0 ^ module_info ^ ctors :=  Ctors
    % is expanded into
    %   V_1 = Term0 ^ module_info,
    %   V_3 = V_2 ^ ctors := Ctors,
    %   Term = Term0 ^ module_info := V_3.
    %
:- pred expand_set_field_function_call(prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out,
    pair(cons_id, list(unify_sub_context))::out, hlds_goal::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % Expand a field extraction goal into a list of goals which each get one
    % level of the structure.
    %
    % A field extraction goal:
    %   := (ModuleName, ^ module_info ^ sub_info ^ module_name,
    %       DCG_in, DCG_out).
    % is expanded into
    %   DCG_out = DCG_in,
    %   V_1 = DCG_out ^ module_info
    %   V_2 = V_1 ^ sub_info,
    %   ModuleName = V_2 ^ module_name.
    %
:- pred expand_dcg_field_extraction_goal(prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out,
    pair(cons_id, list(unify_sub_context))::out, hlds_goal::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

    % Expand a field extraction function call into a list of goals which
    % each get one level of the structure.
    %
    % A field extraction goal:
    %   ModuleName = Info ^ module_info ^ sub_info ^ module_name
    % is expanded into
    %   V_1 = Info ^ module_info,
    %   V_2 = V_1 ^ sub_info,
    %   ModuleName = V_2 ^ module_name.
    %
:- pred expand_get_field_function_call(prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, field_list::in,
    prog_var::in, prog_var::in, purity::in, cons_id::out,
    pair(cons_id, list(unify_sub_context))::out, hlds_goal::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

:- pred parse_field_list(prog_term::in, prog_varset::in,
    list(format_piece)::in, maybe1(field_list)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module hlds.make_hlds.superhomogeneous_util.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.

:- import_module int.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

expand_set_field_function_call(Context, MainContext, SubContext0, FieldNames,
        FieldValueVar, TermInputVar, TermOutputVar, Functor, FieldSubContext,
        Goal, !SVarState, !UrInfo) :-
    expand_set_field_function_call_2(Context, MainContext, SubContext0,
        FieldNames, FieldValueVar, TermInputVar, TermOutputVar, Functor,
        FieldSubContext, Goals, !SVarState, !UrInfo),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_set_field_function_call_2(prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out,
    pair(cons_id, list(unify_sub_context))::out, list(hlds_goal)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

expand_set_field_function_call_2(_, _, _, [], _, _, _, _, _, _,
        !SVarState, !UrInfo) :-
    unexpected($pred, "empty list of field names").
expand_set_field_function_call_2(Context, MainContext, SubContext0,
        [FieldName - FieldArgs | FieldNames], FieldValueVar,
        TermInputVar, TermOutputVar, Functor, FieldSubContext, Goals,
        !SVarState, !UrInfo) :-
    make_fresh_arg_vars_subst_svars(FieldArgs, FieldArgVars, FieldArgVarsArgs,
        !SVarState, !UrInfo),
    (
        FieldNames = [_ | _],
        create_new_unravel_var(SubTermInputVar, !UrInfo),
        create_new_unravel_var(SubTermOutputVar, !UrInfo),
        SetArgs = FieldArgVars ++ [TermInputVar, SubTermOutputVar],
        GetArgs = FieldArgVars ++ [TermInputVar],

        QualInfo0 = !.UrInfo ^ ui_qual_info,
        construct_field_access_function_call(set, Context,
            MainContext, SubContext0, FieldName, TermOutputVar,
            SetArgs, purity_pure, Functor, UpdateGoal, QualInfo0, QualInfo1),
        % Extract the field containing the field to update.
        construct_field_access_function_call(get, Context,
            MainContext, SubContext0, FieldName, SubTermInputVar,
            GetArgs, purity_pure, _, GetSubFieldGoal, QualInfo1, QualInfo),
        !UrInfo ^ ui_qual_info := QualInfo,

        % Recursively update the field.
        SubTermInputArgNumber = 2 + list.length(FieldArgs),
        TermInputContext = unify_sub_context(Functor, SubTermInputArgNumber),
        SubContext = [TermInputContext | SubContext0],
        expand_set_field_function_call_2(Context, MainContext,
            SubContext, FieldNames, FieldValueVar, SubTermInputVar,
            SubTermOutputVar, _, FieldSubContext, Goals0, !SVarState, !UrInfo),

        Goals1 = [GetSubFieldGoal | Goals0] ++ [UpdateGoal]
    ;
        FieldNames = [],
        SetArgs = FieldArgVars ++ [TermInputVar, FieldValueVar],
        QualInfo0 = !.UrInfo ^ ui_qual_info,
        construct_field_access_function_call(set, Context,
            MainContext, SubContext0, FieldName, TermOutputVar,
            SetArgs, purity_pure, Functor, Goal, QualInfo0, QualInfo),
        !UrInfo ^ ui_qual_info := QualInfo,
        FieldSubContext = Functor - SubContext0,
        Goals1 = [Goal]
    ),
    ArgContext = ac_functor(Functor, MainContext, SubContext0),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals1, GoalInfo, Conj0),
    insert_arg_unifications(FieldArgVarsArgs, Context, ArgContext,
        Conj0, Conj, !SVarState, !UrInfo),
    svar_goal_to_conj_list(Conj, Goals, !UrInfo).

expand_dcg_field_extraction_goal(Context, MainContext, SubContext, FieldNames,
        FieldValueVar, TermInputVar, TermOutputVar, Functor, FieldSubContext,
        Goal, !SVarState, !UrInfo) :-
    % Unify the DCG input and output variables.
    QualInfo0 = !.UrInfo ^ ui_qual_info,
    make_atomic_unification(TermOutputVar, rhs_var(TermInputVar), Context,
        MainContext, SubContext, UnifyDCG, QualInfo0, QualInfo),
    !UrInfo ^ ui_qual_info := QualInfo,

    % Process the access function as a get function on the output DCG variable.
    expand_get_field_function_call_2(Context, MainContext, SubContext,
        FieldNames, FieldValueVar, TermOutputVar, purity_pure,
        Functor, FieldSubContext, Goals1, !SVarState, !UrInfo),
    Goals = [UnifyDCG | Goals1],
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

expand_get_field_function_call(Context, MainContext, SubContext0, FieldNames,
        FieldValueVar, TermInputVar, Purity, Functor, FieldSubContext,
        Goal, !SVarState, !UrInfo) :-
    expand_get_field_function_call_2(Context, MainContext, SubContext0,
        FieldNames, FieldValueVar, TermInputVar, Purity, Functor,
        FieldSubContext, Goals, !SVarState, !UrInfo),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_get_field_function_call_2(prog_context::in,
    unify_main_context::in, list(unify_sub_context)::in, field_list::in,
    prog_var::in, prog_var::in, purity::in, cons_id::out,
    pair(cons_id, list(unify_sub_context))::out, list(hlds_goal)::out,
    svar_state::in, svar_state::out,
    unravel_info::in, unravel_info::out) is det.

expand_get_field_function_call_2(_, _, _, [], _, _, _, _, _, _,
        !SVarState, !UrInfo) :-
    unexpected($pred, "empty list of field names").
expand_get_field_function_call_2(Context, MainContext, SubContext0,
        [FieldName - FieldArgs | FieldNames], FieldValueVar, TermInputVar,
        Purity, Functor, FieldSubContext, Goals, !SVarState, !UrInfo) :-
    make_fresh_arg_vars_subst_svars(FieldArgs, FieldArgVars, FieldArgVarsArgs,
        !SVarState, !UrInfo),
    GetArgVars = FieldArgVars ++ [TermInputVar],
    (
        FieldNames = [_ | _],
        create_new_unravel_var(SubTermInputVar, !UrInfo),
        QualInfo0 = !.UrInfo ^ ui_qual_info,
        construct_field_access_function_call(get, Context, MainContext,
            SubContext0, FieldName, SubTermInputVar, GetArgVars, Purity,
            Functor, Goal, QualInfo0, QualInfo),
        !UrInfo ^ ui_qual_info := QualInfo,

        % Recursively extract until we run out of field names.
        TermInputArgNumber = 1 + list.length(FieldArgVars),
        TermInputContext = unify_sub_context(Functor, TermInputArgNumber),
        SubContext = [TermInputContext | SubContext0],
        expand_get_field_function_call_2(Context, MainContext,
            SubContext, FieldNames, FieldValueVar, SubTermInputVar, Purity,
            _, FieldSubContext, Goals1, !SVarState, !UrInfo),
        Goals2 = [Goal | Goals1]
    ;
        FieldNames = [],
        FieldSubContext = Functor - SubContext0,
        QualInfo0 = !.UrInfo ^ ui_qual_info,
        construct_field_access_function_call(get, Context,
            MainContext, SubContext0, FieldName, FieldValueVar,
            GetArgVars, Purity, Functor, Goal, QualInfo0, QualInfo),
        !UrInfo ^ ui_qual_info := QualInfo,
        Goals2 = [Goal]
    ),
    ArgContext = ac_functor(Functor, MainContext, SubContext0),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals2, GoalInfo, Conj0),
    insert_arg_unifications(FieldArgVarsArgs, Context, ArgContext,
        Conj0, Conj, !SVarState, !UrInfo),
    svar_goal_to_conj_list(Conj, Goals, !UrInfo).

:- pred construct_field_access_function_call(field_access_type::in,
    prog_context::in, unify_main_context::in, list(unify_sub_context)::in,
    sym_name::in, prog_var::in, list(prog_var)::in, purity::in,
    cons_id::out, hlds_goal::out, qual_info::in, qual_info::out) is det.

construct_field_access_function_call(AccessType, Context,
        MainContext, SubContext, FieldName, RetArg, Args, Purity, Functor,
        Goal, !QualInfo) :-
    field_access_function_name(AccessType, FieldName, FuncName),
    list.length(Args, Arity),
    Functor = du_data_ctor(du_ctor(FuncName, Arity, cons_id_dummy_type_ctor)),
    make_atomic_unification(RetArg,
        rhs_functor(Functor, is_not_exist_constr, Args),
        Context, MainContext, SubContext, Purity, Goal, !QualInfo).

parse_field_list(Term, VarSet, ContextPieces, MaybeFieldNames) :-
    ( if
        Term = term.functor(term.atom("^"),
            [FieldNameTerm, OtherFieldNamesTerm], _)
    then
        ( if try_parse_sym_name_and_args(FieldNameTerm, FieldName, Args) then
            parse_field_list(OtherFieldNamesTerm, VarSet, ContextPieces,
                MaybeFieldNamesTail),
            (
                MaybeFieldNamesTail = error1(_),
                MaybeFieldNames = MaybeFieldNamesTail
            ;
                MaybeFieldNamesTail = ok1(FieldNamesTail),
                MaybeFieldNames = ok1([FieldName - Args | FieldNamesTail])
            )
        else
            Spec = make_field_list_error(VarSet,
                get_term_context(FieldNameTerm), Term, ContextPieces),
            MaybeFieldNames = error1([Spec])
        )
    else
        ( if try_parse_sym_name_and_args(Term, FieldName, Args) then
            MaybeFieldNames = ok1([FieldName - Args])
        else
            Spec = make_field_list_error(VarSet, get_term_context(Term), Term,
                ContextPieces),
            MaybeFieldNames = error1([Spec])
        )
    ).

:- func make_field_list_error(prog_varset, term.context, prog_term,
    list(format_piece)) = error_spec.

make_field_list_error(VarSet, Context, Term, ContextPieces) = Spec :-
    TermStr = mercury_term_to_string_vs(VarSet, print_name_only, Term),
    Pieces = ContextPieces ++
        [lower_case_next_if_not_first, words("Error: expected a")] ++
        color_as_correct([words("field name,")]) ++
        [words("got")] ++
        color_as_incorrect([quote(TermStr), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.field_access.
%-----------------------------------------------------------------------------%
