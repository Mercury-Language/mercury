%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: lambda.m
% main author: fjh

% This module is a pass over the HLDS to deal with lambda expressions.
%
% Lambda expressions are converted into separate predicates, so for
% example we translate
%
%   :- pred p(int::in) is det.
%   p(X) :-
%       V__1 = (pred(Y::out) is nondet :- q(Y, X)),
%       solutions(V__1, List),
%       ...
%   :- pred q(int::out, int::in) is nondet.
%
% into
%
%   p(X) :-
%       V__1 = '__LambdaGoal__1'(X)
%       solutions(V__1, List),
%       ...
%
%   :- pred '__LambdaGoal__1'(int::in, int::out) is nondet.
%   '__LambdaGoal__1'(X, Y) :- q(Y, X).
%
%
% Note that the mode checker requires that a lambda expression
% not bind any of the non-local variables such as `X' in the above
% example.
%
% Similarly, a lambda expression may not bind any of the type_infos for
% those variables; that is, none of the non-local variables
% should be existentially typed (from the perspective of the lambda goal).
% Now that we run the polymorphism.m pass before mode checking, this is
% also checked by mode analysis.
%
% It might be OK to allow the parameters of the lambda goal to be
% existentially typed, but currently that is not supported.
% One difficulty is that it's hard to determine here which type variables
% should be existentially quantified.  The information is readily
% available during type inference, and really type inference should save
% that information in a field in the lambda_goal struct, but currently it
% doesn't; it saves the head_type_params field in the pred_info, which
% tells us which type variables where produced by the body, but for
% any given lambda goal we don't know whether the type variable was
% produced by something outside the lambda goal or by something inside
% the lambda goal (only in the latter case should it be existentially
% quantified).
% The other difficulty is that taking the address of a predicate with an
% existential type would require second-order polymorphism:  for a predicate
% declared as `:- some [T] pred p(int, T)', the expression `p' must have
% type `some [T] pred(int, T)', which is quite a different thing to saying
% that there is some type `T' for which `p' has type `pred(int, T)' --
% we don't know what `T' is until the predicate is called, and it might
% be different for each call.
% Currently we don't support second-order polymorphism, so we
% don't support existentially typed lambda expressions either.
%

%-----------------------------------------------------------------------------%

:- module transform_hlds__lambda.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- pred process_module(module_info::in, module_info::out) is det.

:- pred process_pred(pred_id::in, module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Parse tree modules
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

% HLDS modules
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__code_model.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module hlds__quantification.

% Misc
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.

% Standard library modules
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

:- type lambda_info
    --->    lambda_info(
                prog_varset,            % from the proc_info
                map(prog_var, type),    % from the proc_info
                prog_constraints,       % from the pred_info
                tvarset,                % from the proc_info
                inst_varset,            % from the proc_info
                rtti_varmaps,           % from the proc_info
                pred_markers,           % from the pred_info
                pred_or_func,
                string,                 % pred/func name
                aditi_owner,
                module_info,
                bool                    % true iff we need to recompute
                                        % the nonlocals
            ).

%-----------------------------------------------------------------------------%

    % This whole section just traverses the module structure.

process_module(!ModuleInfo) :-
    module_info_predids(!.ModuleInfo, PredIds),
    list__foldl(process_pred, PredIds, !ModuleInfo),
    % Need update the dependency graph to include the lambda predicates.
    module_info_clobber_dependency_info(!ModuleInfo).

process_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    list__foldl(process_proc(PredId), ProcIds, !ModuleInfo).

:- pred process_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

process_proc(PredId, ProcId, !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    map__lookup(PredTable0, PredId, PredInfo0),
    pred_info_procedures(PredInfo0, ProcTable0),
    map__lookup(ProcTable0, ProcId, ProcInfo0),

    process_proc_2(ProcInfo0, ProcInfo, PredInfo0, PredInfo1, !ModuleInfo),

    pred_info_procedures(PredInfo1, ProcTable1),
    map__det_update(ProcTable1, ProcId, ProcInfo, ProcTable),
    pred_info_set_procedures(ProcTable, PredInfo1, PredInfo),
    module_info_preds(!.ModuleInfo, PredTable1),
    map__det_update(PredTable1, PredId, PredInfo, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

:- pred process_proc_2(proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, module_info::in, module_info::out) is det.

process_proc_2(!ProcInfo, !PredInfo, !ModuleInfo) :-
    % grab the appropriate fields from the pred_info and proc_info
    PredName = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    pred_info_typevarset(!.PredInfo, TypeVarSet0),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_class_context(!.PredInfo, Constraints0),
    pred_info_get_aditi_owner(!.PredInfo, Owner),
    proc_info_headvars(!.ProcInfo, HeadVars),
    proc_info_varset(!.ProcInfo, VarSet0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    proc_info_goal(!.ProcInfo, Goal0),
    proc_info_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    proc_info_inst_varset(!.ProcInfo, InstVarSet0),
    MustRecomputeNonLocals0 = no,

    % Process the goal.
    Info0 = lambda_info(VarSet0, VarTypes0, Constraints0, TypeVarSet0,
        InstVarSet0, RttiVarMaps0, Markers, PredOrFunc,
        PredName, Owner, !.ModuleInfo, MustRecomputeNonLocals0),
    process_goal(Goal0, Goal1, Info0, Info1),
    Info1 = lambda_info(VarSet1, VarTypes1, Constraints, TypeVarSet,
        _, RttiVarMaps, _, _, _, _, !:ModuleInfo,
        MustRecomputeNonLocals),

    % Check if we need to requantify.
    (
        MustRecomputeNonLocals = yes,
        implicitly_quantify_clause_body(HeadVars, _Warnings,
            Goal1, Goal, VarSet1, VarSet, VarTypes1, VarTypes)
    ;
        MustRecomputeNonLocals = no,
        Goal = Goal1,
        VarSet = VarSet1,
        VarTypes = VarTypes1
    ),

    % Set the new values of the fields in proc_info and pred_info.
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo),
    pred_info_set_typevarset(TypeVarSet, !PredInfo),
    pred_info_set_class_context(Constraints, !PredInfo).

    % The job of process_goal is to traverse the goal, processing each
    % unification with process_unify_goal.
    %
:- pred process_goal(hlds_goal::in, hlds_goal::out,
    lambda_info::in, lambda_info::out) is det.

process_goal(GoalExpr0 - GoalInfo, GoalExpr - GoalInfo, !Info) :-
    (
        GoalExpr0 = unify(XVar, Y, Mode, Unification, Context),
        process_unify_goal(XVar, Y, Mode, Unification, Context,
            GoalExpr, !Info)
    ;
        GoalExpr0 = conj(Goals0),
        process_goal_list(Goals0, Goals, !Info),
        GoalExpr = conj(Goals)
    ;
        GoalExpr0 = par_conj(Goals0),
        process_goal_list(Goals0, Goals, !Info),
        GoalExpr = par_conj(Goals)
    ;
        GoalExpr0 = disj(Goals0),
        process_goal_list(Goals0, Goals, !Info),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = not(Goal0),
        process_goal(Goal0, Goal, !Info),
        GoalExpr = not(Goal)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        process_cases(Cases0, Cases, !Info),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = scope(Reason, Goal0),
        process_goal(Goal0, Goal, !Info),
        GoalExpr = scope(Reason, Goal)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        process_goal(Cond0, Cond, !Info),
        process_goal(Then0, Then, !Info),
        process_goal(Else0, Else, !Info),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = foreign_proc(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "process_goal_2: unexpected shorthand")
    ).

:- pred process_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
    lambda_info::in, lambda_info::out) is det.

process_goal_list([], [], !Info).
process_goal_list([Goal0 | Goals0], [Goal | Goals], !Info) :-
    process_goal(Goal0, Goal, !Info),
    process_goal_list(Goals0, Goals, !Info).

:- pred process_cases(list(case)::in, list(case)::out,
    lambda_info::in, lambda_info::out) is det.

process_cases([], [], !Info).
process_cases([case(ConsId, Goal0) | Cases0], [case(ConsId, Goal) | Cases],
        !Info) :-
    process_goal(Goal0, Goal, !Info),
    process_cases(Cases0, Cases, !Info).

:- pred process_unify_goal(prog_var::in, unify_rhs::in, unify_mode::in,
    unification::in, unify_context::in, hlds_goal_expr::out,
    lambda_info::in, lambda_info::out) is det.

process_unify_goal(XVar, Y0, Mode, Unification0, Context, GoalExpr, !Info) :-
    (
        Y0 = lambda_goal(Purity, PredOrFunc, EvalMethod, _,
            NonLocalVars, Vars, Modes, Det, LambdaGoal0)
    ->
        % First, process the lambda goal recursively, in case it contains
        % some nested lambda expressions.
        process_goal(LambdaGoal0, LambdaGoal, !Info),

        % Then, convert the lambda expression into a new predicate.
        process_lambda(Purity, PredOrFunc, EvalMethod, Vars, Modes, Det,
            NonLocalVars, LambdaGoal, Unification0, Y, Unification, !Info),
        GoalExpr = unify(XVar, Y, Mode, Unification, Context)
    ;
        % Ordinary unifications are left unchanged.
        GoalExpr = unify(XVar, Y0, Mode, Unification0, Context)
    ).

:- pred process_lambda(purity::in, pred_or_func::in, lambda_eval_method::in,
    list(prog_var)::in, list(mode)::in, determinism::in, list(prog_var)::in,
    hlds_goal::in, unification::in, unify_rhs::out, unification::out,
    lambda_info::in, lambda_info::out) is det.

process_lambda(Purity, PredOrFunc, EvalMethod, Vars, Modes, Detism,
        OrigNonLocals0, LambdaGoal, Unification0, Functor,
        Unification, LambdaInfo0, LambdaInfo) :-
    LambdaInfo0 = lambda_info(VarSet, VarTypes, _PredConstraints, TVarSet,
        InstVarSet, RttiVarMaps, Markers, POF, OrigPredName,
        Owner, ModuleInfo0, MustRecomputeNonLocals0),

    % Calculate the constraints which apply to this lambda expression.
    % Note currently we only allow lambda expressions to have universally
    % quantified constraints.
    rtti_varmaps_reusable_constraints(RttiVarMaps, AllConstraints),
    map__apply_to_list(Vars, VarTypes, LambdaVarTypes),
    list__map(prog_type__vars, LambdaVarTypes, LambdaTypeVarsList),
    list__condense(LambdaTypeVarsList, LambdaTypeVars),
    list__filter(constraint_contains_vars(LambdaTypeVars),
        AllConstraints, UnivConstraints),
    Constraints = constraints(UnivConstraints, []),

    % Existentially typed lambda expressions are not yet supported
    % (see the documentation at top of this file).
    ExistQVars = [],
    LambdaGoal = _ - LambdaGoalInfo,
    goal_info_get_nonlocals(LambdaGoalInfo, LambdaGoalNonLocals),
    set__insert_list(LambdaGoalNonLocals, Vars, LambdaNonLocals),
    goal_util__extra_nonlocal_typeinfos(RttiVarMaps, VarTypes, ExistQVars,
        LambdaNonLocals, ExtraTypeInfos),
    OrigVars = OrigNonLocals0,

    ( Unification0 = construct(Var0, _, _, UniModes0, _, _, _) ->
        Var = Var0,
        UniModes1 = UniModes0
    ;
        unexpected(this_file, "transform_lambda: weird unification")
    ),

    set__delete_list(LambdaGoalNonLocals, Vars, NonLocals1),

    % We need all the typeinfos, including the ones that are not used,
    % for the layout structure describing the closure.
    NewTypeInfos = ExtraTypeInfos `set__difference` NonLocals1,
    NonLocals = NonLocals1 `set__union` NewTypeInfos,

    % If we added variables to the nonlocals of the lambda goal, then
    % we need to recompute the nonlocals for the procedure that contains it.
    ( \+ set__empty(NewTypeInfos) ->
        MustRecomputeNonLocals = yes
    ;
        MustRecomputeNonLocals = MustRecomputeNonLocals0
    ),

    set__to_sorted_list(NonLocals, ArgVars1),

    (
        % Optimize a special case: replace
        %   `(pred(Y1, Y2, ...) is Detism :-
        %       p(X1, X2, ..., Y1, Y2, ...))'
        % where `p' has determinism `Detism' with
        %   `p(X1, X2, ...)'
        %
        % This optimization is only valid if the modes of the Xi are input,
        % since only input arguments can be curried. It's also only valid
        % if all the inputs in the Yi precede the outputs. It's also not valid
        % if any of the Xi are in the Yi.

        LambdaGoal = call(PredId0, ProcId0, CallVars, _, _, _) - _,
        module_info_pred_proc_info(ModuleInfo0, PredId0, ProcId0,
            Call_PredInfo, Call_ProcInfo),
        (
            EvalMethod = (aditi_bottom_up),
            pred_info_get_markers(Call_PredInfo, Call_Markers),
            check_marker(Call_Markers, aditi)
        ;
            EvalMethod = normal
        ),
        list__remove_suffix(CallVars, Vars, InitialVars),

        % check that none of the variables that we're trying to
        % use as curried arguments are lambda-bound variables
        \+ (
            list__member(InitialVar, InitialVars),
            list__member(InitialVar, Vars)
        ),

        % Check that the code models are compatible. Note that det is not
        % compatible with semidet, and semidet is not compatible with nondet,
        % since the calling conventions are different. If we're using the LLDS
        % back-end (i.e. not --high-level-code), det is compatible with nondet.
        % If we're using the MLDS back-end, then predicates and functions have
        % different calling conventions.
        proc_info_interface_code_model(Call_ProcInfo, Call_CodeModel),
        determinism_to_code_model(Detism, CodeModel),
        module_info_get_globals(ModuleInfo0, Globals),
        globals__lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        (
            HighLevelCode = no,
            (
                CodeModel = Call_CodeModel
            ;
                CodeModel = model_non,
                Call_CodeModel = model_det
            )
        ;
            HighLevelCode = yes,
            Call_PredOrFunc = pred_info_is_pred_or_func(Call_PredInfo),
            PredOrFunc = Call_PredOrFunc,
            CodeModel = Call_CodeModel
        ),

        % Check that the curried arguments are all input.
        proc_info_argmodes(Call_ProcInfo, Call_ArgModes),
        list__length(InitialVars, NumInitialVars),
        list__take(NumInitialVars, Call_ArgModes, CurriedArgModes),
        (
            list__member(Mode, CurriedArgModes)
        =>
            mode_is_input(ModuleInfo0, Mode)
        )
    ->
        ArgVars = InitialVars,
        PredId = PredId0,
        ProcId = ProcId0,
        mode_util__modes_to_uni_modes(ModuleInfo0,
            CurriedArgModes, CurriedArgModes, UniModes),
        % We must mark the procedure as having had its address taken.
        proc_info_set_address_taken(address_is_taken,
            Call_ProcInfo, Call_NewProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId,
            Call_PredInfo, Call_NewProcInfo,
            ModuleInfo0, ModuleInfo)
    ;
        % Prepare to create a new predicate for the lambda expression:
        % work out the arguments, module name, predicate name, arity,
        % arg types, determinism, context, status, etc. for the new predicate.

        ArgVars = put_typeinfo_vars_first(ArgVars1, VarTypes),
        list__append(ArgVars, Vars, AllArgVars),

        module_info_get_name(ModuleInfo0, ModuleName),
        goal_info_get_context(LambdaGoalInfo, OrigContext),
        term__context_file(OrigContext, OrigFile),
        term__context_line(OrigContext, OrigLine),
        module_info_next_lambda_count(OrigContext, LambdaCount,
            ModuleInfo0, ModuleInfo1),
        make_pred_name_with_context(ModuleName, "IntroducedFrom",
            PredOrFunc, OrigPredName, OrigLine, LambdaCount, PredName),
        goal_info_get_context(LambdaGoalInfo, LambdaContext),
        % The TVarSet is a superset of what it really ought be,
        % but that shouldn't matter.
        % Existentially typed lambda expressions are not
        % yet supported (see the documentation at top of this file)
        ExistQVars = [],
        uni_modes_to_modes(UniModes1, OrigArgModes),

        % We have to jump through hoops to work out the mode of the lambda
        % predicate. For introduced type_info arguments, we use the mode "in".
        % For the original non-local vars, we use the modes from `UniModes1'.
        % For the lambda var arguments at the end, we use the mode in the
        % lambda expression.

        list__length(ArgVars, NumArgVars),
        in_mode(In),
        list__duplicate(NumArgVars, In, InModes),
        map__from_corresponding_lists(ArgVars, InModes, ArgModesMap),

        map__from_corresponding_lists(OrigVars, OrigArgModes, OrigArgModesMap),
        map__overlay(ArgModesMap, OrigArgModesMap, ArgModesMap1),
        map__apply_to_list(ArgVars, ArgModesMap1, ArgModes1),

        % Recompute the uni_modes.
        mode_util__modes_to_uni_modes(ModuleInfo1, ArgModes1, ArgModes1,
            UniModes),

        list__append(ArgModes1, Modes, AllArgModes),
        map__apply_to_list(AllArgVars, VarTypes, ArgTypes),

        purity_to_markers(Purity, LambdaMarkers0),
        (
            % Pass through the aditi markers for aggregate query closures.
            % XXX we should differentiate between normal top-down closures
            % and aggregate query closures, possibly by using a different type
            % for aggregate queries. Currently all nondet lambda expressions
            % within Aditi predicates are treated as aggregate inputs.
            % EvalMethod = (aditi_bottom_up),
            determinism_components(Detism, _, at_most_many),
            check_marker(Markers, aditi)
        ->
            markers_to_marker_list(Markers, MarkerList0),
            list__filter(
                (pred(Marker::in) is semidet :-
                    % Pass through only Aditi markers. Don't pass through
                    % `context' markers, since they are useless for
                    % non-recursive predicates such as the created predicate.
                    ( Marker = aditi
                    ; Marker = dnf
                    ; Marker = psn
                    ; Marker = naive
                    ; Marker = supp_magic
                    ; Marker = aditi_memo
                    ; Marker = aditi_no_memo
                    )),
                MarkerList0, MarkerList),
            list__foldl(add_marker, MarkerList, LambdaMarkers0, LambdaMarkers)
        ;
            EvalMethod = (aditi_bottom_up)
        ->
            add_marker(aditi, LambdaMarkers0, LambdaMarkers)
        ;
            LambdaMarkers = LambdaMarkers0
        ),

        % Now construct the proc_info and pred_info for the new single-mode
        % predicate, using the information computed above.
        proc_info_create(LambdaContext, VarSet, VarTypes,
            AllArgVars, InstVarSet, AllArgModes, Detism,
            LambdaGoal, RttiVarMaps, address_is_taken, ProcInfo0),

        % The debugger ignores unnamed variables.
        ensure_all_headvars_are_named(ProcInfo0, ProcInfo1),

        % If we previously already needed to recompute the nonlocals,
        % then we'd better to that recomputation for the procedure
        % that we just created.
        (
            MustRecomputeNonLocals0 = yes,
            requantify_proc(ProcInfo1, ProcInfo)
        ;
            MustRecomputeNonLocals0 = no,
            ProcInfo = ProcInfo1
        ),
        set__init(Assertions),
        pred_info_create(ModuleName, PredName, PredOrFunc, LambdaContext,
            lambda(OrigFile, OrigLine, LambdaCount), local, LambdaMarkers,
            ArgTypes, TVarSet, ExistQVars, Constraints, Assertions, Owner,
            ProcInfo, ProcId, PredInfo),

        % Save the new predicate in the predicate table.
        module_info_get_predicate_table(ModuleInfo1, PredicateTable0),
        predicate_table_insert(PredInfo, PredId,
            PredicateTable0, PredicateTable),
        module_info_set_predicate_table(PredicateTable,
            ModuleInfo1, ModuleInfo)
    ),
    ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
    ConsId = pred_const(ShroudedPredProcId, EvalMethod),
    Functor = functor(ConsId, no, ArgVars),

    Unification = construct(Var, ConsId, ArgVars, UniModes,
        construct_dynamically, cell_is_unique, no_construct_sub_info),
    LambdaInfo = lambda_info(VarSet, VarTypes, Constraints, TVarSet,
        InstVarSet, RttiVarMaps, Markers, POF, OrigPredName, Owner,
        ModuleInfo, MustRecomputeNonLocals).

:- pred constraint_contains_vars(list(tvar)::in, prog_constraint::in)
    is semidet.

constraint_contains_vars(LambdaVars, ClassConstraint) :-
    ClassConstraint = constraint(_, ConstraintTypes),
    list__map(prog_type__vars, ConstraintTypes, ConstraintVarsList),
    list__condense(ConstraintVarsList, ConstraintVars),
    % Probably not the most efficient way of doing it, but I wouldn't think
    % that it matters.
    set__list_to_set(LambdaVars, LambdaVarsSet),
    set__list_to_set(ConstraintVars, ConstraintVarsSet),
    set__subset(ConstraintVarsSet, LambdaVarsSet).

    % This predicate works out the modes of the original non-local variables
    % of a lambda expression based on the list of uni_mode in the unify_info
    % for the lambda unification.
    %
:- pred uni_modes_to_modes(list(uni_mode)::in, list(mode)::out) is det.

uni_modes_to_modes([], []).
uni_modes_to_modes([UniMode | UniModes], [Mode | Modes]) :-
    UniMode = ((_Initial0 - Initial1) -> (_Final0 - _Final1)),
    Mode = (Initial1 -> Initial1),
    uni_modes_to_modes(UniModes, Modes).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lambda.m".

%---------------------------------------------------------------------------%
