%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This submodule of make_hlds handles the declarations of fields

:- module hlds__make_hlds__field_access.
:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__make_hlds__qual_info.
:- import_module hlds__make_hlds__state_var.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_io_util.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module std_util.

:- type field_list == assoc_list(ctor_field_name, list(prog_term)).

    % Expand a field update goal into a list of goals which
    % each get or set one level of the structure.
    %
    % A field update goal:
    %   Term = Term0 ^ module_info ^ ctors :=  Ctors
    % is expanded into
    %   V_1 = Term0 ^ module_info,
    %   V_3 = V_2 ^ ctors := Ctors,
    %   Term = Term0 ^ module_info := V_3.
    %
:- pred expand_set_field_function_call(prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in,
    prog_varset::in, prog_varset::out, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

    % Expand a field extraction goal into a list of goals which
    % each get one level of the structure.
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
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in,
    prog_varset::in, prog_varset::out, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

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
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_varset::in, prog_varset::out,
    cons_id::out, pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

:- pred parse_field_list(prog_term::in,
    maybe1(field_list, prog_var_type)::out) is det.

:- implementation.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_pred.
:- import_module hlds__make_hlds__superhomogeneous.
:- import_module parse_tree__prog_io.

:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module term.
:- import_module varset.

expand_set_field_function_call(Context, MainContext, SubContext0, FieldNames,
        FieldValueVar, TermInputVar, TermOutputVar, !VarSet, Functor,
        FieldSubContext, Goal, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    expand_set_field_function_call_2(Context, MainContext,
        SubContext0, FieldNames, FieldValueVar, TermInputVar,
        TermOutputVar, !VarSet, Functor, FieldSubContext, Goals,
        !ModuleInfo, !QualInfo, !SInfo, !IO),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_set_field_function_call_2(prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in,
    prog_varset::in, prog_varset::out, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, list(hlds_goal)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

expand_set_field_function_call_2(_, _, _, [], _, _, _, !VarSet, _, _, _,
        !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    error("expand_set_field_function_call_2: empty list of field names").
expand_set_field_function_call_2(Context, MainContext, SubContext0,
        [FieldName - FieldArgs | FieldNames], FieldValueVar,
        TermInputVar, TermOutputVar, !VarSet, Functor,
        FieldSubContext, Goals, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    make_fresh_arg_vars(FieldArgs, FieldArgVars, !VarSet, !SInfo, !IO),
    (
        FieldNames = [_ | _],
        varset__new_var(!.VarSet, SubTermInputVar, !:VarSet),
        varset__new_var(!.VarSet, SubTermOutputVar, !:VarSet),
        SetArgs = list__append(FieldArgVars,
            [TermInputVar, SubTermOutputVar]),
        construct_field_access_function_call(set, Context,
            MainContext, SubContext0, FieldName, TermOutputVar,
            SetArgs, Functor, UpdateGoal, !QualInfo),

        % Extract the field containing the field to update.
        construct_field_access_function_call(get, Context,
            MainContext, SubContext0, FieldName, SubTermInputVar,
            list__append(FieldArgVars, [TermInputVar]), _,
            GetSubFieldGoal, !QualInfo),

        % Recursively update the field.
        SubTermInputArgNumber = 2 + list__length(FieldArgs),
        TermInputContext = Functor - SubTermInputArgNumber,
        SubContext = [TermInputContext | SubContext0],
        expand_set_field_function_call_2(Context, MainContext,
            SubContext, FieldNames, FieldValueVar, SubTermInputVar,
            SubTermOutputVar, !VarSet, _, FieldSubContext, Goals0,
            !ModuleInfo, !QualInfo, !SInfo, !IO),

        list__append([GetSubFieldGoal | Goals0], [UpdateGoal], Goals1)
    ;
        FieldNames = [],
        SetArgs = list__append(FieldArgVars, [TermInputVar, FieldValueVar]),
        construct_field_access_function_call(set, Context,
            MainContext, SubContext0, FieldName, TermOutputVar,
            SetArgs, Functor, Goal, !QualInfo),
        FieldSubContext = Functor - SubContext0,
        Goals1 = [Goal]

    ),
    ArgContext = functor(Functor, MainContext, SubContext0),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals1, GoalInfo, Conj0),
    insert_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
        Conj0, Conj, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
    goal_to_conj_list(Conj, Goals).

expand_dcg_field_extraction_goal(Context, MainContext, SubContext, FieldNames,
        FieldValueVar, TermInputVar, TermOutputVar, !VarSet, Functor,
        FieldSubContext, Goal, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    % Unify the DCG input and output variables.
    make_atomic_unification(TermOutputVar, var(TermInputVar), Context,
        MainContext, SubContext, UnifyDCG, !QualInfo),

    % Process the access function as a get function on the output DCG variable.
    expand_get_field_function_call_2(Context, MainContext, SubContext,
        FieldNames, FieldValueVar, TermOutputVar, !VarSet,
        Functor, FieldSubContext, Goals1, !ModuleInfo, !QualInfo, !SInfo, !IO),
    Goals = [UnifyDCG | Goals1],
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

expand_get_field_function_call(Context, MainContext, SubContext0,
        FieldNames, FieldValueVar, TermInputVar, !VarSet,
        Functor, FieldSubContext, Goal, !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    expand_get_field_function_call_2(Context, MainContext, SubContext0,
        FieldNames, FieldValueVar, TermInputVar, !VarSet,
        Functor, FieldSubContext, Goals, !ModuleInfo, !QualInfo, !SInfo, !IO),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_get_field_function_call_2(prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_varset::in, prog_varset::out,
    cons_id::out, pair(cons_id, unify_sub_contexts)::out, list(hlds_goal)::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out, io::di, io::uo) is det.

expand_get_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _,
        !ModuleInfo, !QualInfo, !Sinfo, !IO) :-
    error("expand_get_field_function_call_2: empty list of field names").
expand_get_field_function_call_2(Context, MainContext, SubContext0,
        [FieldName - FieldArgs | FieldNames], FieldValueVar,
        TermInputVar, !VarSet, Functor, FieldSubContext, Goals,
        !ModuleInfo, !QualInfo, !SInfo, !IO) :-
    make_fresh_arg_vars(FieldArgs, FieldArgVars, !VarSet, !SInfo, !IO),
    GetArgVars = list__append(FieldArgVars, [TermInputVar]),
    (
        FieldNames = [_ | _],
        varset__new_var(!.VarSet, SubTermInputVar, !:VarSet),
        construct_field_access_function_call(get, Context, MainContext,
            SubContext0, FieldName, SubTermInputVar, GetArgVars, Functor, Goal,
            !QualInfo),

        % recursively extract until we run out of field names
        TermInputArgNumber = 1 + list__length(FieldArgVars),
        TermInputContext = Functor - TermInputArgNumber,
        SubContext = [TermInputContext | SubContext0],
        expand_get_field_function_call_2(Context, MainContext,
            SubContext, FieldNames, FieldValueVar, SubTermInputVar,
            !VarSet, _, FieldSubContext, Goals1, !ModuleInfo, !QualInfo, !SInfo,
            !IO),
        Goals2 = [Goal | Goals1]
    ;
        FieldNames = [],
        FieldSubContext = Functor - SubContext0,
        construct_field_access_function_call(get, Context,
            MainContext, SubContext0, FieldName, FieldValueVar,
            GetArgVars, Functor, Goal, !QualInfo),
        Goals2 = [Goal]
    ),
    ArgContext = functor(Functor, MainContext, SubContext0),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals2, GoalInfo, Conj0),
    insert_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
        Conj0, Conj, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !IO),
    goal_to_conj_list(Conj, Goals).

:- pred construct_field_access_function_call(field_access_type::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    ctor_field_name::in, prog_var::in, list(prog_var)::in, cons_id::out,
    hlds_goal::out, qual_info::in, qual_info::out) is det.

construct_field_access_function_call(AccessType, Context, MainContext,
        SubContext, FieldName, RetArg, Args, Functor, Goal, !QualInfo) :-
    field_access_function_name(AccessType, FieldName, FuncName),
    list__length(Args, Arity),
    Functor = cons(FuncName, Arity),
    make_atomic_unification(RetArg, functor(Functor, no, Args),
        Context, MainContext, SubContext, Goal, !QualInfo).

parse_field_list(Term, MaybeFieldNames) :-
    (
        Term = term__functor(term__atom("^"),
            [FieldNameTerm, OtherFieldNamesTerm], _)
    ->
        (
            parse_qualified_term(FieldNameTerm, FieldNameTerm,
                "field name", Result),
            Result = ok(FieldName, Args)
        ->
            parse_field_list(OtherFieldNamesTerm,
                MaybeFieldNames1),
            (
                MaybeFieldNames1 = error(_, _),
                MaybeFieldNames = MaybeFieldNames1
            ;
                MaybeFieldNames1 = ok(FieldNames1),
                MaybeFieldNames =
                    ok([FieldName - Args | FieldNames1])
            )
        ;
            MaybeFieldNames = error("expected field name",
                FieldNameTerm)
        )
    ;
        (
            parse_qualified_term(Term, Term, "field name", Result),
            Result = ok(FieldName, Args)
        ->
            MaybeFieldNames = ok([FieldName - Args])
        ;
            MaybeFieldNames = error("expected field name", Term)
        )
    ).
