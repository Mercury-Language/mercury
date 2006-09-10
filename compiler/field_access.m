%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
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
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_util.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- type field_list == assoc_list(ctor_field_name, list(prog_term)).

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
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

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
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

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
    prog_var::in, prog_var::in, purity::in, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out,
    hlds_goal::out, num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred parse_field_list(prog_term::in,
    maybe1(field_list, prog_var_type)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.superhomogeneous.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_io.

:- import_module bool.
:- import_module int.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

expand_set_field_function_call(Context, MainContext, SubContext0, FieldNames,
        FieldValueVar, TermInputVar, TermOutputVar, Functor, FieldSubContext,
        Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs) :-
    expand_set_field_function_call_2(Context, MainContext, SubContext0,
        FieldNames, FieldValueVar, TermInputVar, TermOutputVar, Functor,
        FieldSubContext, Goals, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_set_field_function_call_2(prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, prog_var::in, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, list(hlds_goal)::out,
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_set_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs) :-
    unexpected(this_file,
        "expand_set_field_function_call_2: empty list of field names").
expand_set_field_function_call_2(Context, MainContext, SubContext0,
        [FieldName - FieldArgs | FieldNames], FieldValueVar,
        TermInputVar, TermOutputVar, Functor, FieldSubContext, Goals, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs) :-
    make_fresh_arg_vars(FieldArgs, FieldArgVars, !VarSet, !SInfo, !Specs),
    (
        FieldNames = [_ | _],
        varset.new_var(!.VarSet, SubTermInputVar, !:VarSet),
        varset.new_var(!.VarSet, SubTermOutputVar, !:VarSet),
        SetArgs = FieldArgVars ++ [TermInputVar, SubTermOutputVar],
        construct_field_access_function_call(set, Context,
            MainContext, SubContext0, FieldName, TermOutputVar,
            SetArgs, purity_pure, Functor, UpdateGoal, !QualInfo),
        UpdateAdded = 1,

        % Extract the field containing the field to update.
        construct_field_access_function_call(get, Context,
            MainContext, SubContext0, FieldName, SubTermInputVar,
            list.append(FieldArgVars, [TermInputVar]), purity_pure, _,
            GetSubFieldGoal, !QualInfo),
        GetSubFieldAdded = 1,

        % Recursively update the field.
        SubTermInputArgNumber = 2 + list.length(FieldArgs),
        TermInputContext = Functor - SubTermInputArgNumber,
        SubContext = [TermInputContext | SubContext0],
        expand_set_field_function_call_2(Context, MainContext,
            SubContext, FieldNames, FieldValueVar, SubTermInputVar,
            SubTermOutputVar, _, FieldSubContext, Goals0, SetAdded,
            !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs),

        FieldAdded = GetSubFieldAdded + SetAdded + UpdateAdded,
        Goals1 = [GetSubFieldGoal | Goals0] ++ [UpdateGoal]
    ;
        FieldNames = [],
        SetArgs = FieldArgVars ++ [TermInputVar, FieldValueVar],
        construct_field_access_function_call(set, Context,
            MainContext, SubContext0, FieldName, TermOutputVar,
            SetArgs, purity_pure, Functor, Goal, !QualInfo),
        FieldAdded = 1,
        FieldSubContext = Functor - SubContext0,
        Goals1 = [Goal]
    ),
    ArgContext = ac_functor(Functor, MainContext, SubContext0),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals1, GoalInfo, Conj0),
    insert_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
        Conj0, Conj, ArgAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !Specs),
    NumAdded = FieldAdded + ArgAdded,
    goal_to_conj_list(Conj, Goals).

expand_dcg_field_extraction_goal(Context, MainContext, SubContext, FieldNames,
        FieldValueVar, TermInputVar, TermOutputVar, Functor, FieldSubContext,
        Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs) :-
    % Unify the DCG input and output variables.
    make_atomic_unification(TermOutputVar, rhs_var(TermInputVar), Context,
        MainContext, SubContext, UnifyDCG, !QualInfo),
    UnifyAdded = 1,

    % Process the access function as a get function on the output DCG variable.
    expand_get_field_function_call_2(Context, MainContext, SubContext,
        FieldNames, FieldValueVar, TermOutputVar, purity_pure,
        Functor, FieldSubContext, Goals1, GetAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs),
    NumAdded = UnifyAdded + GetAdded,
    Goals = [UnifyDCG | Goals1],
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

expand_get_field_function_call(Context, MainContext, SubContext0, FieldNames,
        FieldValueVar, TermInputVar, Purity, Functor, FieldSubContext,
        Goal, NumAdded, !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs) :-
    expand_get_field_function_call_2(Context, MainContext, SubContext0,
        FieldNames, FieldValueVar, TermInputVar, Purity, Functor,
        FieldSubContext, Goals, NumAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !Specs),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_get_field_function_call_2(prog_context::in,
    unify_main_context::in, unify_sub_contexts::in, field_list::in,
    prog_var::in, prog_var::in, purity::in, cons_id::out,
    pair(cons_id, unify_sub_contexts)::out, list(hlds_goal)::out,
    num_added_goals::out, prog_varset::in, prog_varset::out,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    svar_info::in, svar_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

expand_get_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _,
        !VarSet, !ModuleInfo, !QualInfo, !Sinfo, !Specs) :-
    unexpected(this_file,
        "expand_get_field_function_call_2: empty list of field names").
expand_get_field_function_call_2(Context, MainContext, SubContext0,
        [FieldName - FieldArgs | FieldNames], FieldValueVar, TermInputVar,
        Purity, Functor, FieldSubContext, Goals, NumAdded,
        !VarSet, !ModuleInfo, !QualInfo, !SInfo, !Specs) :-
    make_fresh_arg_vars(FieldArgs, FieldArgVars, !VarSet, !SInfo, !Specs),
    GetArgVars = FieldArgVars ++ [TermInputVar],
    (
        FieldNames = [_ | _],
        varset.new_var(!.VarSet, SubTermInputVar, !:VarSet),
        construct_field_access_function_call(get, Context, MainContext,
            SubContext0, FieldName, SubTermInputVar, GetArgVars, Purity,
            Functor, Goal, !QualInfo),
        CallAdded = 1,

        % Recursively extract until we run out of field names.
        TermInputArgNumber = 1 + list.length(FieldArgVars),
        TermInputContext = Functor - TermInputArgNumber,
        SubContext = [TermInputContext | SubContext0],
        expand_get_field_function_call_2(Context, MainContext,
            SubContext, FieldNames, FieldValueVar, SubTermInputVar, Purity,
            _, FieldSubContext, Goals1, ExtractAdded, !VarSet, !ModuleInfo,
            !QualInfo, !SInfo, !Specs),
        Goals2 = [Goal | Goals1],
        FieldAdded = CallAdded + ExtractAdded
    ;
        FieldNames = [],
        FieldSubContext = Functor - SubContext0,
        construct_field_access_function_call(get, Context,
            MainContext, SubContext0, FieldName, FieldValueVar,
            GetArgVars, Purity, Functor, Goal, !QualInfo),
        Goals2 = [Goal],
        FieldAdded = 1
    ),
    ArgContext = ac_functor(Functor, MainContext, SubContext0),
    goal_info_init(Context, GoalInfo),
    conj_list_to_goal(Goals2, GoalInfo, Conj0),
    insert_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
        Conj0, Conj, ArgAdded, !VarSet, !ModuleInfo, !QualInfo,
        !SInfo, !Specs),
    NumAdded = FieldAdded + ArgAdded,
    goal_to_conj_list(Conj, Goals).

:- pred construct_field_access_function_call(field_access_type::in,
    prog_context::in, unify_main_context::in, unify_sub_contexts::in,
    ctor_field_name::in, prog_var::in, list(prog_var)::in, purity::in,
    cons_id::out, hlds_goal::out, qual_info::in, qual_info::out) is det.

construct_field_access_function_call(AccessType, Context, MainContext,
        SubContext, FieldName, RetArg, Args, Purity, Functor, Goal,
        !QualInfo) :-
    field_access_function_name(AccessType, FieldName, FuncName),
    list.length(Args, Arity),
    Functor = cons(FuncName, Arity),
    make_atomic_unification(RetArg, rhs_functor(Functor, no, Args),
        Context, MainContext, SubContext, Purity, Goal, !QualInfo).

parse_field_list(Term, MaybeFieldNames) :-
    (
        Term = term.functor(term.atom("^"),
            [FieldNameTerm, OtherFieldNamesTerm], _)
    ->
        (
            parse_qualified_term(FieldNameTerm, FieldNameTerm,
                "field name", Result),
            Result = ok2(FieldName, Args)
        ->
            parse_field_list(OtherFieldNamesTerm, MaybeFieldNames1),
            (
                MaybeFieldNames1 = error1(_),
                MaybeFieldNames = MaybeFieldNames1
            ;
                MaybeFieldNames1 = ok1(FieldNames1),
                MaybeFieldNames = ok1([FieldName - Args | FieldNames1])
            )
        ;
            MaybeFieldNames = error1(["expected field name" - FieldNameTerm])
        )
    ;
        (
            parse_qualified_term(Term, Term, "field name", Result),
            Result = ok2(FieldName, Args)
        ->
            MaybeFieldNames = ok1([FieldName - Args])
        ;
            MaybeFieldNames = error1(["expected field name" - Term])
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "field_access.m".

%-----------------------------------------------------------------------------%
:- end_module field_access.
%-----------------------------------------------------------------------------%
