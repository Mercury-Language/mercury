%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012, 2014 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
% File: polymorphism_clause.m.
% Main authors: fjh and zs.
%
% This module handles the part of the polymorphism transformation
% that involves transforming the bodies of clauses. That transformation
% is described by the comment at the top of polymorphism.m.
%---------------------------------------------------------------------------%

:- module check_hlds.polymorphism_goal.
:- interface.

:- import_module check_hlds.polymorphism_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- pred polymorphism_process_goal(hlds_goal::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

    % Add the type_info variables for a complicated unification to
    % the appropriate fields in the unification and the goal_info.
    %
    % Exported for modecheck_unify.m.
    %
:- pred unification_typeinfos_rtti_varmaps(mer_type::in, rtti_varmaps::in,
    unification::in, unification::out, hlds_goal_info::in, hlds_goal_info::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.polymorphism_lambda.
:- import_module check_hlds.polymorphism_type_class_info.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.from_ground_term_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.quantification.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

polymorphism_process_goal(Goal0, Goal, !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(LHSVar, RHS, Mode, Unification, UnifyContext),
        polymorphism_process_unify(LHSVar, RHS, Mode, Unification,
            UnifyContext, GoalInfo0, Goal, !Info)
    ;
        GoalExpr0 = plain_call(PredId, _, ArgVars0, _, _, _),
        polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
            ExtraVars, ExtraGoals, !Info),
        ArgVars = ExtraVars ++ ArgVars0,
        CallExpr = GoalExpr0 ^ call_args := ArgVars,
        Call = hlds_goal(CallExpr, GoalInfo),
        GoalList = ExtraGoals ++ [Call],
        conj_list_to_goal(GoalList, GoalInfo0, Goal)
    ;
        GoalExpr0 = call_foreign_proc(_, PredId, _, _, _, _, _),
        poly_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),
        ( if no_type_info_builtin(PredModule, PredName, PredArity) then
            Goal = Goal0
        else
            polymorphism_process_foreign_proc(PredInfo, GoalExpr0, GoalInfo0,
                Goal, !Info)
        )
    ;
        % We don't need to add type_infos for higher order calls, since the
        % type_infos are added when the closures are constructed, not when
        % they are called.
        GoalExpr0 = generic_call(_, _, _, _, _),
        Goal = Goal0
    ;
        % The rest of the cases just process goals recursively.
        (
            GoalExpr0 = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                polymorphism_process_plain_conj(Goals0, Goals, !Info)
            ;
                ConjType = parallel_conj,
                get_cache_maps_snapshot("parconj", InitialSnapshot, !Info),
                polymorphism_process_par_conj(Goals0, Goals, InitialSnapshot,
                    !Info)
                % Unlike with disjunctions, we do not have to reset to
                % InitialSnapshot.
                %
                % The reason why we take InitialSnapshot is to prevent
                % each conjunct from depending on e.g. type_info vars
                % generated by earlier conjuncts. However, the code
                % following the parallel conjunction as a whole
                % *may* rely on the changes to the varmaps made by the
                % last conjunct. (In fact, it could rely on the changes
                % made by any conjunct, but we throw away the changes
                % made by all conjuncts except the last.
            ),
            GoalExpr = conj(ConjType, Goals)
        ;
            GoalExpr0 = disj(Goals0),
            get_cache_maps_snapshot("disj", InitialSnapshot, !Info),
            polymorphism_process_disj(Goals0, Goals, InitialSnapshot, !Info),
            set_cache_maps_snapshot("after disj", InitialSnapshot, !Info),
            GoalExpr = disj(Goals)
        ;
            GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
            get_cache_maps_snapshot("ite", InitialSnapshot, !Info),
            polymorphism_process_goal(Cond0, Cond, !Info),
            % If we allowed a type_info created inside Cond to be reused
            % in Then, then we are adding an output variable to Cond.
            % If Cond scope had no outputs to begin with, this would change
            % its determinism.
            set_cache_maps_snapshot("before then", InitialSnapshot, !Info),
            polymorphism_process_goal(Then0, Then, !Info),
            set_cache_maps_snapshot("before else", InitialSnapshot, !Info),
            polymorphism_process_goal(Else0, Else, !Info),
            set_cache_maps_snapshot("after ite", InitialSnapshot, !Info),
            GoalExpr = if_then_else(Vars, Cond, Then, Else)
        ;
            GoalExpr0 = negation(SubGoal0),
            get_cache_maps_snapshot("neg", InitialSnapshot, !Info),
            polymorphism_process_goal(SubGoal0, SubGoal, !Info),
            set_cache_maps_snapshot("after neg", InitialSnapshot, !Info),
            GoalExpr = negation(SubGoal)
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            get_cache_maps_snapshot("switch", InitialSnapshot, !Info),
            polymorphism_process_cases(Cases0, Cases, InitialSnapshot, !Info),
            set_cache_maps_snapshot("after switch", InitialSnapshot, !Info),
            GoalExpr = switch(Var, CanFail, Cases)
        ;
            GoalExpr0 = scope(Reason0, SubGoal0),
            (
                Reason0 = from_ground_term(TermVar, Kind),
                (
                    Kind = from_ground_term_initial,
                    polymorphism_process_from_ground_term_initial(TermVar,
                        GoalInfo0, SubGoal0, GoalExpr, !Info)
                ;
                    ( Kind = from_ground_term_construct
                    ; Kind = from_ground_term_deconstruct
                    ; Kind = from_ground_term_other
                    ),
                    polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                    GoalExpr = scope(Reason0, SubGoal)
                )
            ;
                Reason0 = promise_solutions(_, _),
                % polymorphism_process_goal may cause SubGoal to bind
                % variables (such as PolyConst variables) that SubGoal0 does
                % not bind. If we allowed such variables to be reused outside
                % the scope, that would change the set of variables that the
                % promise would have to cover. We cannot expect and do not want
                % user level programmers making promises about variables added
                % by the compiler.
                get_cache_maps_snapshot("promise_solns", InitialSnapshot,
                    !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after promise_solns", InitialSnapshot,
                    !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                ( Reason0 = disable_warnings(_, _)
                ; Reason0 = promise_purity(_)
                ; Reason0 = require_detism(_)
                ; Reason0 = require_complete_switch(_)
                ; Reason0 = require_switch_arms_detism(_, _)
                ; Reason0 = commit(_)
                ; Reason0 = barrier(_)
                ; Reason0 = loop_control(_, _, _)
                ),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                Reason0 = exist_quant(_),
                % If we allowed a type_info created inside SubGoal to be reused
                % outside GoalExpr, then we are adding an output variable to
                % the scope. If the scope had no outputs to begin with, this
                % would change the determinism of the scope.
                %
                % However, using a type_info from before the scope in SubGoal
                % is perfectly ok.
                get_cache_maps_snapshot("exists", InitialSnapshot, !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after exists", InitialSnapshot,
                    !Info),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                Reason0 = trace_goal(_, _, _, _, _),
                % Trace goal scopes will be deleted after semantic analysis
                % if their compile-time condition turns out to be false.
                % If we let later code use type_infos introduced inside the
                % scope, the deletion of the scope would leave those variables
                % undefined.
                %
                % We *could* evaluate the compile-time condition here to know
                % whether the deletion will happen or not, but doing so would
                % require breaching the separation between compiler passes.
                get_cache_maps_snapshot("trace", InitialSnapshot, !Info),
                polymorphism_process_goal(SubGoal0, SubGoal, !Info),
                set_cache_maps_snapshot("after trace", InitialSnapshot, !Info),
                GoalExpr = scope(Reason0, SubGoal)
            )
        ),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal0, OrElseGoals0, OrElseInners),
            get_cache_maps_snapshot("atomic", InitialSnapshot, !Info),
            polymorphism_process_goal(MainGoal0, MainGoal, !Info),
            polymorphism_process_disj(OrElseGoals0, OrElseGoals,
                InitialSnapshot, !Info),
            set_cache_maps_snapshot("after atomic", InitialSnapshot, !Info),
            ShortHand = atomic_goal(GoalType, Outer, Inner, Vars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            % We don't let the code inside and outside the try goal share
            % type_info variables for the same reason as with lambda
            % expressions; because those pieces of code will end up
            % in different procedures. However, for try goals, this is true
            % even for the first and second conjuncts.
            get_cache_maps_snapshot("try", InitialSnapshot, !Info),
            ( if
                SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo),
                SubGoalExpr0 = conj(plain_conj, Conjuncts0),
                Conjuncts0 = [ConjunctA0, ConjunctB0]
            then
                empty_cache_maps(!Info),
                polymorphism_process_goal(ConjunctA0, ConjunctA, !Info),
                empty_cache_maps(!Info),
                polymorphism_process_goal(ConjunctB0, ConjunctB, !Info),

                Conjuncts = [ConjunctA, ConjunctB],
                SubGoalExpr = conj(plain_conj, Conjuncts),
                SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo)
            else
                unexpected($pred, "malformed try goal")
            ),
            set_cache_maps_snapshot("after try", InitialSnapshot, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        ),
        GoalExpr = shorthand(ShortHand),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ).

%---------------------------------------------------------------------------%
%
% Process unifications.
%

:- pred polymorphism_process_unify(prog_var::in, unify_rhs::in,
    unify_mode::in, unification::in, unify_context::in, hlds_goal_info::in,
    hlds_goal::out, poly_info::in, poly_info::out) is det.

polymorphism_process_unify(LHSVar, RHS0, Mode, Unification0, UnifyContext,
        GoalInfo0, Goal, !Info) :-
    (
        RHS0 = rhs_var(_RHSVar),
        % Var-var unifications (simple_test, assign, or complicated_unify)
        % are basically left unchanged. Complicated unifications will
        % eventually get converted into calls, but that is done later on,
        % by simplify.m, not now. At this point we just need to figure out
        % which type_info/typeclass_info variables the unification might need,
        % and insert them in the nonlocals. We have to do that for all var-var
        % unifications, because at this point we haven't done mode analysis so
        % we don't know which ones will become complicated_unifies.
        % Note that we also store the type_info/typeclass_info variables
        % in a field in the unification, which quantification.m uses when
        % requantifying things.
        poly_info_get_var_table(!.Info, VarTable),
        lookup_var_type(VarTable, LHSVar, Type),
        unification_typeinfos(Type, Unification0, Unification,
            GoalInfo0, GoalInfo, _Changed, !Info),
        Goal = hlds_goal(unify(LHSVar, RHS0, Mode, Unification, UnifyContext),
            GoalInfo)
    ;
        RHS0 = rhs_functor(ConsId, _, Args),
        polymorphism_process_unify_functor(LHSVar, ConsId, Args, Mode,
            Unification0, UnifyContext, GoalInfo0, Goal, _Changed, !Info)
    ;
        RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals0, ArgVarsModes, Det, LambdaGoal0),
        % For lambda expressions, we must recursively traverse the lambda goal.
        % Any type_info variables needed by the lambda goal are created in the
        % lambda goal (not imported from the outside), and any type_info
        % variables created by the lambda goal are not available outside.
        % This is because, after lambda expansion, the code inside and outside
        % the lambda goal will end up in different procedures.
        get_cache_maps_snapshot("lambda", InitialSnapshot, !Info),
        empty_cache_maps(!Info),
        polymorphism_process_goal(LambdaGoal0, LambdaGoal1, !Info),
        set_cache_maps_snapshot("after lambda", InitialSnapshot, !Info),

        assoc_list.keys(ArgVarsModes, ArgVars),
        % Currently we don't allow lambda goals to be existentially typed.
        ExistQVars = [],
        requantify_lambda_goal(LambdaNonLocals0, ArgVars,
            ExistQVars, LambdaGoal1, LambdaGoal,
            LambdaTiTciVars, PossibleNonLocalTiTciVars, !Info),
        LambdaNonLocals1 =
            set_of_var.to_sorted_list(LambdaTiTciVars) ++
            set_of_var.to_sorted_list(PossibleNonLocalTiTciVars) ++
            LambdaNonLocals0,
        list.sort_and_remove_dups(LambdaNonLocals1, LambdaNonLocals),
        RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals, ArgVarsModes, Det, LambdaGoal),
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.union(PossibleNonLocalTiTciVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),

        % Complicated (in-in) argument unifications are impossible for lambda
        % expressions, so we don't need to worry about adding the type_infos
        % that would be required for such unifications.
        GoalExpr = unify(LHSVar, RHS, Mode, Unification0, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%---------------------%

:- pred unification_typeinfos(mer_type::in,
    unification::in, unification::out, hlds_goal_info::in, hlds_goal_info::out,
    bool::out, poly_info::in, poly_info::out) is det.

unification_typeinfos(Type, !Unification, !GoalInfo, Changed, !Info) :-
    % Compute the type_info/type_class_info variables that would be used
    % if this unification ends up being a complicated_unify.
    type_vars_in_type(Type, TypeVars),
    (
        TypeVars = [],
        Changed = no
    ;
        TypeVars = [_ | _],
        list.map_foldl(poly_get_type_info_locn, TypeVars, TypeInfoLocns,
            !Info),
        add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo),
        Changed = yes
    ).

unification_typeinfos_rtti_varmaps(Type, RttiVarMaps,
        !Unification, !GoalInfo) :-
    % This variant is for use by modecheck_unify.m. During mode checking,
    % all the type_infos should appear in the type_info_varmap.

    % Compute the type_info/type_class_info variables that would be used
    % if this unification ends up being a complicated_unify.
    type_vars_in_type(Type, TypeVars),
    list.map(rtti_lookup_type_info_locn(RttiVarMaps), TypeVars, TypeInfoLocns),
    add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo).

:- pred add_unification_typeinfos(list(type_info_locn)::in,
    unification::in, unification::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

add_unification_typeinfos(TypeInfoLocns, !Unification, !GoalInfo) :-
    list.map(type_info_locn_var, TypeInfoLocns, TypeInfoVars0),
    list.remove_dups(TypeInfoVars0, TypeInfoVars),

    % Insert the TypeInfoVars into the nonlocals field of the goal_info
    % for the unification goal.
    NonLocals0 = goal_info_get_nonlocals(!.GoalInfo),
    set_of_var.insert_list(TypeInfoVars, NonLocals0, NonLocals),
    goal_info_set_nonlocals(NonLocals, !GoalInfo),

    % Also save those type_info vars into a field in the complicated_unify,
    % so that quantification.m can recompute variable scopes properly.
    % This field is also used by modecheck_unify.m -- for complicated
    % unifications, it checks that all these variables are ground.
    (
        !.Unification = complicated_unify(Modes, CanFail, _),
        !:Unification = complicated_unify(Modes, CanFail, TypeInfoVars)
    ;
        % This can happen if an earlier stage of compilation has already
        % determined that this unification is particular kind of unification.
        % In that case, the type_info vars won't be needed.
        ( !.Unification = construct(_, _, _, _, _, _, _)
        ; !.Unification = deconstruct(_, _, _, _, _, _)
        ; !.Unification = assign(_, _)
        ; !.Unification = simple_test(_, _)
        )
    ).

%---------------------%

:- pred polymorphism_process_unify_functor(prog_var::in, cons_id::in,
    list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
    hlds_goal_info::in, hlds_goal::out, bool::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_unify_functor(X0, ConsId0, ArgVars0, Mode0, Unification0,
        UnifyContext, GoalInfo0, Goal, Changed, !Info) :-
    poly_info_get_module_info(!.Info, ModuleInfo0),
    poly_info_get_var_table(!.Info, VarTable0),
    lookup_var_type(VarTable0, X0, TypeOfX),
    list.length(ArgVars0, Arity),

    % We replace any unifications with higher order pred constants
    % by lambda expressions. For example, we replace
    %
    %   X = list.append(Y)     % Y::in, X::out
    %
    % with
    %
    %   X = (
    %       pred(A1::in, A2::out) is ... :- list.append(Y, A1, A2)
    %   )
    %
    % We do this because it makes two things easier. First, mode analysis
    % needs to check that the lambda goal doesn't bind any nonlocal variables
    % (e.g. `Y' in above example). This would require a bit of moderately
    % tricky special case code if we didn't expand them here. Second, this pass
    % (polymorphism.m) is a lot easier if we don't have to handle higher order
    % constants. If it turns out that the predicate was nonpolymorphic,
    % lambda.m will turn the lambda expression back into a higher order
    % constant again.
    %
    % Note that this transformation is also done by modecheck_unify.m, in case
    % we are rerunning mode analysis after lambda.m has already been run;
    % any changes to the code here will also need to be duplicated there.

    ( if
        % Check if variable has a higher order type.
        ConsId0 = closure_cons(ShroudedPredProcId, _),
        proc(PredId, ProcId0) = unshroud_pred_proc_id(ShroudedPredProcId),
        type_is_higher_order_details(TypeOfX, Purity, _PredOrFunc, EvalMethod,
            CalleeArgTypes)
    then
        % An `invalid_proc_id' means the predicate is multi-moded. We can't
        % pick the right mode yet. Perform the rest of the transformation with
        % any mode (the first) but mark the goal with a feature so that mode
        % checking knows to fix it up later.
        ( if ProcId0 = invalid_proc_id then
            module_info_pred_info(ModuleInfo0, PredId, PredInfo),
            ProcIds = pred_info_valid_procids(PredInfo),
            (
                ProcIds = [ProcId | _],
                goal_info_add_feature(feature_lambda_undetermined_mode,
                    GoalInfo0, GoalInfo1)
            ;
                ProcIds = [],
                unexpected($pred, "no modes")
            )
        else
            ProcId = ProcId0,
            GoalInfo1 = GoalInfo0
        ),
        % Convert the higher order pred term to a lambda goal.
        Context = goal_info_get_context(GoalInfo0),
        convert_pred_to_lambda_goal(ModuleInfo0, Purity, EvalMethod, X0,
            PredId, ProcId, ArgVars0, CalleeArgTypes, UnifyContext,
            GoalInfo1, Context, MaybeRHS0, VarTable0, VarTable),
        poly_info_set_var_table(VarTable, !Info),
        (
            MaybeRHS0 = ok1(RHS0),
            % Process the unification in its new form.
            polymorphism_process_unify(X0, RHS0, Mode0, Unification0,
                UnifyContext, GoalInfo1, Goal, !Info)
        ;
            MaybeRHS0 = error1(Specs),
            poly_info_get_errors(!.Info, Specs0),
            poly_info_set_errors(Specs ++ Specs0, !Info),
            % It doesn't matter what Goal we return, since it won't be used.
            RHS = rhs_functor(some_int_const(int_const(42)),
                is_not_exist_constr, []),
            polymorphism_process_unify(X0, RHS, Mode0, Unification0,
                UnifyContext, GoalInfo1, Goal, !Info)
        ),
        Changed = yes
    else if
        % Is this a construction or deconstruction of an existentially
        % typed data type?
        %
        % Check whether the functor had a "new " prefix.
        % If so, assume it is a construction, and strip off the prefix.
        % Otherwise, assume it is a deconstruction.
        ConsId0 = cons(Functor0, Arity, ConsTypeCtor),
        ( if remove_new_prefix(Functor0, OrigFunctor) then
            ConsId = cons(OrigFunctor, Arity, ConsTypeCtor),
            IsExistConstr = is_exist_constr
        else
            ConsId = ConsId0,
            IsExistConstr = is_not_exist_constr
        ),

        % Check whether the functor (with the "new " prefix removed)
        % is an existentially typed functor.
        type_util.get_existq_cons_defn(ModuleInfo0, TypeOfX, ConsId, ConsDefn)
    then
        % Add extra arguments to the unification for the
        % type_info and/or type_class_info variables.
        lookup_var_types(VarTable0, ArgVars0, ActualArgTypes),
        polymorphism_process_existq_unify_functor(ConsDefn,
            IsExistConstr, ActualArgTypes, TypeOfX, GoalInfo0,
            ExtraVars, ExtraGoals, !Info),
        ArgVars = ExtraVars ++ ArgVars0,
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ExtraVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo1),

        % Some of the argument unifications may be complicated unifications,
        % which may need type_infos.
        unification_typeinfos(TypeOfX, Unification0, Unification,
            GoalInfo1, GoalInfo, _Changed, !Info),

        UnifyExpr = unify(X0, rhs_functor(ConsId, IsExistConstr, ArgVars),
            Mode0, Unification, UnifyContext),
        Unify = hlds_goal(UnifyExpr, GoalInfo),
        GoalList = ExtraGoals ++ [Unify],
        conj_list_to_goal(GoalList, GoalInfo0, Goal),
        Changed = yes
    else
        % We leave construction/deconstruction unifications alone.
        % Some of the argument unifications may be complicated unifications,
        % which may need type_infos.
        % XXX Return original Goal0 if Changed = no.
        unification_typeinfos(TypeOfX, Unification0, Unification,
            GoalInfo0, GoalInfo, Changed, !Info),
        RHS = rhs_functor(ConsId0, is_not_exist_constr, ArgVars0),
        GoalExpr = unify(X0, RHS, Mode0, Unification, UnifyContext),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

%---------------------%

    % Compute the extra arguments that we need to add to a unification with
    % an existentially quantified data constructor.
    %
:- pred polymorphism_process_existq_unify_functor(ctor_defn::in,
    is_exist_constr::in, list(mer_type)::in, mer_type::in,
    hlds_goal_info::in, list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_existq_unify_functor(CtorDefn, IsExistConstr,
        ActualArgTypes, ActualRetType, GoalInfo,
        ExtraVars, ExtraGoals, !Info) :-
    CtorDefn = ctor_defn(CtorTypeVarSet, CtorKindMap,
        CtorMaybeExistConstraints, CtorArgTypes, CtorRetType),

    % Rename apart the type variables in the constructor definition.
    poly_info_get_typevarset(!.Info, TypeVarSet0),
    tvarset_merge_renaming(TypeVarSet0, CtorTypeVarSet, TypeVarSet,
        CtorToParentRenaming),
    (
        CtorMaybeExistConstraints = exist_constraints(CtorExistConstraints),
        % XXX Could we use _Ctor{Unc,C}onstrainedExistQVars to avoid
        % some of the work below?
        CtorExistConstraints = cons_exist_constraints(CtorExistQVars,
            CtorExistentialConstraints,
            _CtorUnconstrainedExistQVars, _CtorConstrainedExistQVars),
        apply_variable_renaming_to_tvar_list(CtorToParentRenaming,
            CtorExistQVars, ParentExistQVars),
        apply_variable_renaming_to_prog_constraint_list(CtorToParentRenaming,
            CtorExistentialConstraints, ParentExistentialConstraints),
        list.length(ParentExistentialConstraints, NumExistentialConstraints),

        % Compute the set of _unconstrained_ existentially quantified type
        % variables, and then apply the type bindings to those type variables
        % to figure out what types they are bound to.
        constraint_list_get_tvars(ParentExistentialConstraints,
            ParentExistConstrainedTVars),
        list.delete_elems(ParentExistQVars, ParentExistConstrainedTVars,
            ParentUnconstrainedExistQVars),
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedExistQVars, ActualExistentialTypes)
    ;
        CtorMaybeExistConstraints = no_exist_constraints,
        NumExistentialConstraints = 0,
        ActualExistentialTypes = []
    ),
    apply_variable_renaming_to_tvar_kind_map(CtorToParentRenaming,
        CtorKindMap, ParentKindMap),
    apply_variable_renaming_to_type_list(CtorToParentRenaming,
        CtorArgTypes, ParentArgTypes),
    apply_variable_renaming_to_type(CtorToParentRenaming, CtorRetType,
        ParentRetType),
    poly_info_set_typevarset(TypeVarSet, !Info),

    % Compute the type bindings resulting from the functor's actual argument
    % and return types. These are the ones that might bind the ExistQVars.
    type_list_subsumes_det([ParentRetType | ParentArgTypes],
        [ActualRetType | ActualArgTypes], ParentToActualTypeSubst),

    % Create type_class_info variables for the type class constraints.
    poly_info_get_constraint_map(!.Info, ConstraintMap),
    GoalId = goal_info_get_goal_id(GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    (
        IsExistConstr = is_exist_constr,
        % Assume it is a construction.
        lookup_hlds_constraint_list(ConstraintMap, unproven, GoalId,
            NumExistentialConstraints, ActualExistentialConstraints),
        make_typeclass_info_vars(ActualExistentialConstraints, [], Context,
            ExtraTypeClassVarsMCAs, ExtraTypeClassGoals, !Info),
        assoc_list.keys(ExtraTypeClassVarsMCAs, ExtraTypeClassVars)
    ;
        IsExistConstr = is_not_exist_constr,
        % Assume it is a deconstruction.
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumExistentialConstraints, ActualExistentialConstraints),
        make_existq_typeclass_info_vars(ActualExistentialConstraints, Context,
            ExtraTypeClassVars, ExtraTypeClassGoals, !Info)
    ),

    % Create type_info variables for the _unconstrained_ existentially
    % quantified type variables.
    polymorphism_do_make_type_info_vars(ActualExistentialTypes, Context,
        ExtraTypeInfoVarsMCAs, ExtraTypeInfoGoals, !Info),
    assoc_list.keys(ExtraTypeInfoVarsMCAs, ExtraTypeInfoVars),

    % The type_class_info variables go AFTER the type_info variables
    % (for consistency with the order for argument passing,
    % and because the RTTI support in the runtime system relies on it)

    ExtraGoals = ExtraTypeInfoGoals ++ ExtraTypeClassGoals,
    ExtraVars = ExtraTypeInfoVars ++ ExtraTypeClassVars.

%---------------------%

    % If the lambda goal we are processing is polymorphically typed, we may
    % need to fix up the quantification (nonlocal variables) so that it
    % includes the type_info variables and typeclass_info variables for
    % any polymorphically typed variables in the nonlocals set or in the
    % arguments (either the lambda vars or the implicit curried argument
    % variables). We return this set of LambdaTiTciVars. Including typeinfos
    % for arguments which are not in the nonlocals set of the lambda goal,
    % i.e. unused arguments, is necessary only if typeinfo_liveness is set,
    % but we do it always, since we don't have the options available here,
    % and the since cost is pretty minimal.
    %
    % We also need to include in the nonlocals set of the lambda expression
    % any type_info and/or typeclass_info variables we have added to the
    % goal inside the lambda. In rare cases such as tests/valid/gh98.m,
    % a typeclass_info that we inserted into the goal inside the lambda
    % is defined outside the lambda *without* being in LambdaTiTciVars.
    % We therefore return all type_info/typeclass_info variables that occur
    % in the transformed lambda goal in AllTiTciGoalVars, for our caller
    % to likewise include in the nonlocals set of the lambda goal.
    %
:- pred requantify_lambda_goal(list(prog_var)::in,
    list(prog_var)::in, existq_tvars::in, hlds_goal::in, hlds_goal::out,
    set_of_progvar::out, set_of_progvar::out,
    poly_info::in, poly_info::out) is det.

requantify_lambda_goal(LambdaNonLocals0, ArgVars, ExistQVars, !Goal,
        LambdaTiTciVars, AllTiTciGoalVars, !Info) :-
    poly_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    ( if rtti_varmaps_no_tvars(RttiVarMaps0) then
        set_of_var.init(LambdaTiTciVars),
        set_of_var.init(AllTiTciGoalVars)
    else
        poly_info_get_var_table(!.Info, VarTable0),
        !.Goal = hlds_goal(_, GoalInfo0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(LambdaNonLocals0, NonLocals, BothNonLocals),
        set_of_var.insert_list(ArgVars, BothNonLocals, NonLocalsWithArgVars),
        goal_util.extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps0,
            VarTable0, ExistQVars, NonLocalsWithArgVars, LambdaTiTciVars),

        goal_vars(!.Goal, GoalVars),
        IsTiOrTci =
            ( pred(Var::in) is semidet :-
                lookup_var_type(VarTable0, Var, VarType),
                ( VarType = type_info_type
                ; VarType = typeclass_info_type
                )
            ),
        set_of_var.filter(IsTiOrTci, GoalVars, AllTiTciGoalVars),
        % Our caller will include AllTiTciGoalVars in the nonlocals set
        % of the rhs_lambda_goal, since some of them may actually come
        % from code outside the lambda. However, since some (or even all)
        % of the variables in this set may actually be local, we insist
        % on the *whole* procedure body, not just the code of this lambda,
        % being requantified once its polymorphism transformation has been
        % completed. (This works both for variables in AllTiTciGoalVars
        % that were added by polymorphism, and those which were there before.)
        %
        % Note that all the variables added by the polymorphism transformation
        % of !Goal that may be nonlocal are type_infos and typeclass_infos.
        % The polymorphism transformation can also add integer variables
        % for use in e.g. getting a type_info out of a particular slot
        % of a typeclass_info, but we set the set of cached int vars
        % in the poly_info to empty at the start of transformation of !Goal,
        % and throw away the updated caches at its end, so any integer vars
        % created by the transformation inside !:Goal will be local to !:Goal.
        poly_info_set_must_requantify(!Info),
        set_of_var.union(NonLocalsWithArgVars, AllTiTciGoalVars,
            PossibleOutsideVars),
        implicitly_quantify_goal_general(ord_nl_maybe_lambda,
            PossibleOutsideVars, _Warnings, !Goal,
            VarTable0, VarTable, RttiVarMaps0, RttiVarMaps),
        poly_info_set_var_table_rtti(VarTable, RttiVarMaps, !Info)
    ).

%---------------------------------------------------------------------------%
%
% Process plain calls.
%

    % XXX document me
    %
    % XXX the following code ought to be rewritten to handle
    % existential/universal type_infos and type_class_infos
    % in a more consistent manner.
    %
:- pred polymorphism_process_call(pred_id::in, list(prog_var)::in,
    hlds_goal_info::in, hlds_goal_info::out,
    list(prog_var)::out, list(hlds_goal)::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
        ExtraVars, ExtraGoals, !Info) :-
    poly_info_get_var_table(!.Info, VarTable),
    poly_info_get_typevarset(!.Info, TypeVarSet0),
    poly_info_get_module_info(!.Info, ModuleInfo),

    % The order of the added variables is important, and must match the
    % order specified at the top of this file.
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
        PredArgTypes),
    pred_info_get_tvar_kind_map(PredInfo, PredKindMap),
    pred_info_get_class_context(PredInfo, PredClassContext),

    % VarTable, TypeVarSet* etc come from the caller.
    % PredTypeVarSet, PredArgTypes, PredExistQVars, etc come
    % directly from the callee.
    % ParentArgTypes, ParentExistQVars etc come from a version
    % of the callee that has been renamed apart from the caller.
    %
    % The difference between e.g. PredArgTypes and ParentArgTypes is the
    % application of PredToParentTypeRenaming, which maps the type variables
    % in the callee to new type variables in the caller. Adding the new type
    % variables to TypeVarSet0 yields TypeVarSet.
    ( if varset.is_empty(PredTypeVarSet) then
        % Optimize a common case.
        map.init(PredToParentTypeRenaming),
        TypeVarSet = TypeVarSet0,
        ParentArgTypes = PredArgTypes,
        ParentKindMap = PredKindMap,
        ParentTVars = [],
        ParentExistQVars = []
    else
        % This merge might be a performance bottleneck?
        tvarset_merge_renaming(TypeVarSet0, PredTypeVarSet, TypeVarSet,
            PredToParentTypeRenaming),
        apply_variable_renaming_to_type_list(PredToParentTypeRenaming,
            PredArgTypes, ParentArgTypes),
        type_vars_in_types(ParentArgTypes, ParentTVars),
        apply_variable_renaming_to_tvar_kind_map(PredToParentTypeRenaming,
            PredKindMap, ParentKindMap),
        apply_variable_renaming_to_tvar_list(PredToParentTypeRenaming,
            PredExistQVars, ParentExistQVars)
    ),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    ( if
        (
            % Optimize for the common case of nonpolymorphic call
            % with no constraints.
            ParentTVars = [],
            PredClassContext = constraints([], [])
        ;
            % Some builtins don't need or want the type_info.
            no_type_info_builtin(PredModule, PredName, PredArity)
        )
    then
        GoalInfo = GoalInfo0,
        ExtraGoals = [],
        ExtraVars = []
    else
        poly_info_set_typevarset(TypeVarSet, !Info),

        % Compute which "parent" type variables are constrained
        % by the type class constraints.
        apply_variable_renaming_to_prog_constraints(PredToParentTypeRenaming,
            PredClassContext, ParentClassContext),
        ParentClassContext = constraints(ParentUnivConstraints,
            ParentExistConstraints),
        constraint_list_get_tvars(ParentUnivConstraints,
            ParentUnivConstrainedTVars),
        constraint_list_get_tvars(ParentExistConstraints,
            ParentExistConstrainedTVars),

        % Calculate the set of unconstrained type vars. Split these into
        % existential and universal type vars.
        list.remove_dups(ParentTVars, ParentUnconstrainedTVars0),
        list.delete_elems(ParentUnconstrainedTVars0,
            ParentUnivConstrainedTVars, ParentUnconstrainedTVars1),
        list.delete_elems(ParentUnconstrainedTVars1,
            ParentExistConstrainedTVars, ParentUnconstrainedTVars),
        list.delete_elems(ParentUnconstrainedTVars, ParentExistQVars,
            ParentUnconstrainedUnivTVars),
        list.delete_elems(ParentUnconstrainedTVars,
            ParentUnconstrainedUnivTVars, ParentUnconstrainedExistTVars),

        % Calculate the "parent to actual" binding.
        lookup_var_types(VarTable, ArgVars0, ActualArgTypes),
        type_list_subsumes_det(ParentArgTypes, ActualArgTypes,
            ParentToActualTypeSubst),

        % Make the universally quantified typeclass_infos for the call.
        poly_info_get_constraint_map(!.Info, ConstraintMap),
        GoalId = goal_info_get_goal_id(GoalInfo0),
        list.length(ParentUnivConstraints, NumUnivConstraints),
        lookup_hlds_constraint_list(ConstraintMap, unproven, GoalId,
            NumUnivConstraints, ActualUnivConstraints),
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentExistQVars, ActualExistQVarTypes),
        ( if
            prog_type.type_list_to_var_list(ActualExistQVarTypes,
                ActualExistQVars0)
        then
            ActualExistQVars = ActualExistQVars0
        else
            unexpected($pred, "existq_tvar bound")
        ),
        Context = goal_info_get_context(GoalInfo0),
        make_typeclass_info_vars(ActualUnivConstraints, ActualExistQVars,
            Context, ExtraUnivClassVarsMCAs, ExtraUnivClassGoals, !Info),
        assoc_list.keys(ExtraUnivClassVarsMCAs, ExtraUnivClassVars),

        % Make variables to hold any existentially quantified typeclass_infos
        % in the call, insert them into the typeclass_info map.
        list.length(ParentExistConstraints, NumExistConstraints),
        lookup_hlds_constraint_list(ConstraintMap, assumed, GoalId,
            NumExistConstraints, ActualExistConstraints),
        make_existq_typeclass_info_vars(ActualExistConstraints, Context,
            ExtraExistClassVars, ExtraExistClassGoals, !Info),

        % Make variables to hold typeinfos for unconstrained universal type
        % vars.
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedUnivTVars, ActualUnconstrainedUnivTypes),
        polymorphism_do_make_type_info_vars(ActualUnconstrainedUnivTypes,
            Context, ExtraUnivTypeInfoVarsMCAs,
            ExtraUnivTypeInfoGoals, !Info),
        assoc_list.keys(ExtraUnivTypeInfoVarsMCAs,
            ExtraUnivTypeInfoVars),

        % Make variables to hold typeinfos for unconstrained existential type
        % vars.
        apply_rec_subst_to_tvar_list(ParentKindMap, ParentToActualTypeSubst,
            ParentUnconstrainedExistTVars, ActualUnconstrainedExistTypes),
        polymorphism_do_make_type_info_vars(ActualUnconstrainedExistTypes,
            Context, ExtraExistTypeInfoVarsMCAs,
            ExtraExistTypeInfoGoals, !Info),
        assoc_list.keys(ExtraExistTypeInfoVarsMCAs,
            ExtraExistTypeInfoVars),

        % Add up the extra vars and goals.
        ExtraGoals = ExtraUnivClassGoals ++ ExtraExistClassGoals
            ++ ExtraUnivTypeInfoGoals ++ ExtraExistTypeInfoGoals,
        ExtraVars = ExtraUnivTypeInfoVars ++ ExtraExistTypeInfoVars
            ++ ExtraUnivClassVars ++ ExtraExistClassVars,

        % Update the nonlocals.
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        set_of_var.insert_list(ExtraVars, NonLocals0, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo)
    ).

%---------------------%

    % Add the type_info variables for a new call goal. This predicate assumes
    % that process_module has already been run so the called pred has already
    % been processed.
    %
    % XXX This predicate does not yet handle calls whose arguments include
    % existentially quantified types or type class constraints.
    %
    % XXX This predicate is unused.
    %
:- pred polymorphism_process_new_call(pred_info::in, proc_info::in,
    pred_id::in, proc_id::in, list(prog_var)::in, builtin_state::in,
    maybe(call_unify_context)::in, sym_name::in, hlds_goal_info::in,
    hlds_goal::out, poly_info::in, poly_info::out) is det.
:- pragma consider_used(pred(polymorphism_process_new_call/12)).

polymorphism_process_new_call(CalleePredInfo, CalleeProcInfo, PredId, ProcId,
        CallArgs0, BuiltinState, MaybeCallUnifyContext, SymName,
        GoalInfo0, Goal, !Info) :-
    poly_info_get_typevarset(!.Info, TVarSet0),
    poly_info_get_var_table(!.Info, VarTable0),
    lookup_var_types(VarTable0, CallArgs0, ActualArgTypes0),
    pred_info_get_arg_types(CalleePredInfo, PredTVarSet, _PredExistQVars,
        PredArgTypes),
    proc_info_get_headvars(CalleeProcInfo, CalleeHeadVars),
    proc_info_get_rtti_varmaps(CalleeProcInfo, CalleeRttiVarMaps),

    % Work out how many type_info args we need to prepend.
    NCallArgs0 = list.length(ActualArgTypes0),
    NPredArgs  = list.length(PredArgTypes),
    NExtraArgs = NPredArgs - NCallArgs0,
    ( if
        list.drop(NExtraArgs, PredArgTypes, OrigPredArgTypes0),
        list.take(NExtraArgs, CalleeHeadVars, CalleeExtraHeadVars0)
    then
        OrigPredArgTypes = OrigPredArgTypes0,
        CalleeExtraHeadVars = CalleeExtraHeadVars0
    else
        unexpected($pred, "extra args not found")
    ),

    % Work out the bindings of type variables in the call.
    tvarset_merge_renaming(TVarSet0, PredTVarSet, TVarSet,
        PredToParentRenaming),
    apply_variable_renaming_to_type_list(PredToParentRenaming,
        OrigPredArgTypes, OrigParentArgTypes),
    type_list_subsumes_det(OrigParentArgTypes, ActualArgTypes0,
        ParentToActualTSubst),
    poly_info_set_typevarset(TVarSet, !Info),

    % Look up the type variables that the type_infos in the caller are for,
    % and apply the type bindings to calculate the types that the caller
    % should pass type_infos for.
    GetTypeInfoTypes =
        ( pred(ProgVar::in, TypeInfoType::out) is det :-
            rtti_varmaps_var_info(CalleeRttiVarMaps, ProgVar, VarInfo),
            (
                VarInfo = type_info_var(TypeInfoType)
            ;
                VarInfo = typeclass_info_var(_),
                unexpected($pred,
                    "unsupported: constraints on initialisation preds")
            ;
                VarInfo = non_rtti_var,
                unexpected($pred,
                    "missing rtti_var_info for initialisation pred")
            )
        ),
    list.map(GetTypeInfoTypes, CalleeExtraHeadVars, PredTypeInfoTypes),
    apply_variable_renaming_to_type_list(PredToParentRenaming,
        PredTypeInfoTypes, ParentTypeInfoTypes),
    apply_rec_subst_to_type_list(ParentToActualTSubst, ParentTypeInfoTypes,
        ActualTypeInfoTypes),

    % Construct goals to make the required type_infos.
    Ctxt = term_context.dummy_context,
    polymorphism_do_make_type_info_vars(ActualTypeInfoTypes, Ctxt,
        ExtraArgsConstArgs, ExtraGoals, !Info),
    assoc_list.keys(ExtraArgsConstArgs, ExtraArgs),
    CallArgs = ExtraArgs ++ CallArgs0,
    NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
    NonLocals1 = set_of_var.list_to_set(ExtraArgs),
    set_of_var.union(NonLocals0, NonLocals1, NonLocals),
    goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
    CallGoalExpr = plain_call(PredId, ProcId, CallArgs, BuiltinState,
        MaybeCallUnifyContext, SymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    conj_list_to_goal(ExtraGoals ++ [CallGoal], GoalInfo, Goal).

%---------------------------------------------------------------------------%
%
% Process foreign proc calls.
%

:- pred polymorphism_process_foreign_proc(pred_info::in,
    hlds_goal_expr::in(bound(call_foreign_proc(ground,ground,ground,ground,
    ground,ground,ground))), hlds_goal_info::in, hlds_goal::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_foreign_proc(PredInfo, GoalExpr0, GoalInfo0,
        Goal, !Info) :-
    % Insert the type_info vars into the argname map, so that the foreign_proc
    % can refer to the type_info variable for type T as `TypeInfo_for_T'.
    GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId,
        Args0, ProcExtraArgs, MaybeTraceRuntimeCond, Impl),
    ArgVars0 = list.map(foreign_arg_var, Args0),
    polymorphism_process_call(PredId, ArgVars0, GoalInfo0, GoalInfo,
        ExtraVars, ExtraGoals, !Info),
    polymorphism_process_foreign_proc_args(PredInfo, Impl,
        ExtraVars, ExtraArgs),
    Args = ExtraArgs ++ Args0,

    % Plug it all back together.
    CallExpr = call_foreign_proc(Attributes, PredId, ProcId,
        Args, ProcExtraArgs, MaybeTraceRuntimeCond, Impl),
    Call = hlds_goal(CallExpr, GoalInfo),
    GoalList = ExtraGoals ++ [Call],
    conj_list_to_goal(GoalList, GoalInfo0, Goal).

:- pred polymorphism_process_foreign_proc_args(pred_info::in,
    pragma_foreign_proc_impl::in, list(prog_var)::in, list(foreign_arg)::out)
    is det.

polymorphism_process_foreign_proc_args(PredInfo, Impl, Vars, Args) :-
    pred_info_get_arg_types(PredInfo, PredTypeVarSet, ExistQVars,
        PredArgTypes),

    % Find out which variables are constrained (so that we don't add
    % type_infos for them).
    pred_info_get_class_context(PredInfo, constraints(UnivCs, ExistCs)),
    UnivVars0 = list.map(get_constrained_vars, UnivCs),
    list.condense(UnivVars0, UnivConstrainedVars),
    ExistVars0 = list.map(get_constrained_vars, ExistCs),
    list.condense(ExistVars0, ExistConstrainedVars),

    type_vars_in_types(PredArgTypes, PredTypeVars0),
    list.remove_dups(PredTypeVars0, PredTypeVars1),
    list.delete_elems(PredTypeVars1, UnivConstrainedVars, PredTypeVars2),
    list.delete_elems(PredTypeVars2, ExistConstrainedVars, PredTypeVars),

    % The argument order is described at the top of polymorphism.m.

    in_mode(In),
    out_mode(Out),

    list.map(foreign_proc_add_typeclass_info(Out, Impl, PredTypeVarSet),
        ExistCs, ExistTypeClassArgInfos),
    list.map(foreign_proc_add_typeclass_info(In, Impl, PredTypeVarSet),
        UnivCs, UnivTypeClassArgInfos),
    TypeClassArgInfos = UnivTypeClassArgInfos ++ ExistTypeClassArgInfos,

    list.filter(list.contains(ExistQVars), PredTypeVars,
        ExistUnconstrainedVars, UnivUnconstrainedVars),

    list.map_foldl(foreign_proc_add_typeinfo("Out", Out, Impl, PredTypeVarSet),
        ExistUnconstrainedVars, ExistTypeArgInfos, 1, _),
    list.map_foldl(foreign_proc_add_typeinfo("In", In, Impl, PredTypeVarSet),
        UnivUnconstrainedVars, UnivTypeArgInfos, 1, _),
    TypeInfoArgInfos = UnivTypeArgInfos ++ ExistTypeArgInfos,

    ArgInfos = TypeInfoArgInfos ++ TypeClassArgInfos,

    % Insert type_info/typeclass_info types for all the inserted
    % type_info/typeclass_info vars into the argument type list.
    TypeInfoTypes = list.map((func(_) = type_info_type), PredTypeVars),
    TypeClassInfoType = typeclass_info_type,
    list.length(UnivCs, NumUnivCs),
    list.length(ExistCs, NumExistCs),
    list.duplicate(NumUnivCs + NumExistCs, TypeClassInfoType,
        TypeClassInfoTypes),
    OrigArgTypes = TypeInfoTypes ++ TypeClassInfoTypes,

    make_foreign_args(Vars, ArgInfos, OrigArgTypes, Args).

:- func get_constrained_vars(prog_constraint) = list(tvar).

get_constrained_vars(Constraint) = CVars :-
    Constraint = constraint(_, CTypes),
    type_vars_in_types(CTypes, CVars).

:- pred foreign_proc_add_typeclass_info(mer_mode::in,
    pragma_foreign_proc_impl::in, tvarset::in, prog_constraint::in,
    foreign_arg_name_mode_box::out) is det.

foreign_proc_add_typeclass_info(Mode, Impl, TypeVarSet, Constraint,
        MaybeArgNameBox) :-
    Constraint = constraint(SymName, Types),
    Name = sym_name_to_string_sep(SymName, "__"),
    type_vars_in_types(Types, TypeVars),
    TypeVarNames = list.map(underscore_and_tvar_name(TypeVarSet), TypeVars),
    string.append_list(["TypeClassInfo_for_", Name | TypeVarNames],
        ConstraintVarName),
    % If the variable name corresponding to the typeclass_info isn't mentioned
    % in the C code fragment, don't pass the variable to the C code at all.
    ( if foreign_proc_uses_variable(Impl, ConstraintVarName) then
        MaybeArgName = yes(foreign_arg_name_mode(ConstraintVarName, Mode))
    else
        MaybeArgName = no
    ),
    MaybeArgNameBox =
        foreign_arg_name_mode_box(MaybeArgName, bp_native_if_possible).

:- pred foreign_proc_add_typeinfo(string::in, mer_mode::in,
    pragma_foreign_proc_impl::in, tvarset::in,
    tvar::in, foreign_arg_name_mode_box::out, int::in, int::out) is det.

foreign_proc_add_typeinfo(InOut, Mode, Impl, TypeVarSet, TVar, MaybeArgNameBox,
        !N) :-
    ( if varset.search_name(TypeVarSet, TVar, TypeVarName) then
        OldCVarName = "TypeInfo_for_" ++ TypeVarName,
        NewCVarName = "TypeInfo_" ++ InOut ++ "_" ++ string.int_to_string(!.N),
        % If the variable name corresponding to the type_info isn't mentioned
        % in the C code fragment, don't pass the variable to the C code at all.
        ( if
            ( foreign_proc_uses_variable(Impl, OldCVarName)
            ; foreign_proc_uses_variable(Impl, NewCVarName)
            )
        then
            MaybeArgName = yes(foreign_arg_name_mode(OldCVarName, Mode))
        else
            MaybeArgName = no
        )
    else
        MaybeArgName = no
    ),
    MaybeArgNameBox =
        foreign_arg_name_mode_box(MaybeArgName, bp_native_if_possible),
    !:N = !.N + 1.

%---------------------------------------------------------------------------%
%
% Process scopes.
%

:- pred polymorphism_process_from_ground_term_initial(prog_var::in,
    hlds_goal_info::in, hlds_goal::in, hlds_goal_expr::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_from_ground_term_initial(TermVar, GoalInfo0, SubGoal0,
        GoalExpr, !Info) :-
    SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo0),
    ( if SubGoalExpr0 = conj(plain_conj, SubGoals0Prime) then
        SubGoals0 = SubGoals0Prime
    else
        unexpected($pred, "from_ground_term_initial goal is not plain conj")
    ),
    polymorphism_process_fgti_goals(SubGoals0,
        [], ConstructOrderMarkedSubGoals,
        fgt_invariants_kept, InvariantsStatus, !Info),
    (
        InvariantsStatus = fgt_invariants_kept,
        Reason = from_ground_term(TermVar, from_ground_term_initial),
        GoalExpr = scope(Reason, SubGoal0)
    ;
        InvariantsStatus = fgt_invariants_broken,
        introduce_partial_fgt_scopes(GoalInfo0, SubGoalInfo0,
            ConstructOrderMarkedSubGoals, deconstruct_top_down, SubGoal),
        % Delete the scope wrapper around SubGoal0.
        SubGoal = hlds_goal(GoalExpr, _)
    ).

:- pred polymorphism_process_fgti_goals(list(hlds_goal)::in,
    list(fgt_marked_goal)::in, list(fgt_marked_goal)::out,
    fgt_invariants_status::in, fgt_invariants_status::out,
    poly_info::in, poly_info::out) is det.

polymorphism_process_fgti_goals([], !ConstructOrderMarkedGoals,
        !InvariantsStatus, !Info).
polymorphism_process_fgti_goals([Goal0 | Goals0], !ConstructOrderMarkedGoals,
        !InvariantsStatus, !Info) :-
    % This is used only if polymorphism_fgt_sanity_tests is enabled.
    OldInfo = !.Info,
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = unify(LHSVarPrime, RHS, ModePrime, UnificationPrime,
            UnifyContextPrime),
        RHS = rhs_functor(ConsIdPrime, _, RHSVarsPrime)
    then
        LHSVar = LHSVarPrime,
        Mode = ModePrime,
        Unification = UnificationPrime,
        UnifyContext = UnifyContextPrime,
        ConsId = ConsIdPrime,
        RHSVars = RHSVarsPrime
    else
        unexpected($pred,
            "from_ground_term_initial conjunct is not functor unify")
    ),
    polymorphism_process_unify_functor(LHSVar, ConsId, RHSVars, Mode,
        Unification, UnifyContext, GoalInfo0, Goal, Changed, !Info),
    (
        Changed = no,
        trace [compiletime(flag("polymorphism_fgt_sanity_tests"))] (
            poly_info_get_var_table(OldInfo, VarTableBefore),
            MaxVarBefore = var_table_count(VarTableBefore),
            poly_info_get_num_reuses(OldInfo, NumReusesBefore),

            poly_info_get_var_table(!.Info, VarTableAfter),
            MaxVarAfter = var_table_count(VarTableAfter),
            poly_info_get_num_reuses(!.Info, NumReusesAfter),

            expect(unify(MaxVarBefore, MaxVarAfter), $pred,
                "MaxVarBefore != MaxVarAfter"),
            expect(unify(NumReusesBefore, NumReusesAfter), $pred,
                "NumReusesBefore != NumReusesAfter"),
            expect(unify(Goal0, Goal), $pred,
                "Goal0 != Goal")
        ),
        MarkedGoal = fgt_kept_goal(Goal0, LHSVar, RHSVars)
    ;
        Changed = yes,
        MarkedGoal = fgt_broken_goal(Goal, LHSVar, RHSVars),
        !:InvariantsStatus = fgt_invariants_broken
    ),
    !:ConstructOrderMarkedGoals = [MarkedGoal | !.ConstructOrderMarkedGoals],
    polymorphism_process_fgti_goals(Goals0, !ConstructOrderMarkedGoals,
        !InvariantsStatus, !Info).

:- func underscore_and_tvar_name(tvarset, tvar) = string.

underscore_and_tvar_name(TypeVarSet, TVar) = TVarName :-
    varset.lookup_name(TypeVarSet, TVar, TVarName0),
    TVarName = "_" ++ TVarName0.

%---------------------------------------------------------------------------%
%
% Process compound goals.
%

:- pred polymorphism_process_plain_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, poly_info::in, poly_info::out) is det.

polymorphism_process_plain_conj([], [], !Info).
polymorphism_process_plain_conj([Goal0 | Goals0], [Goal | Goals], !Info) :-
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_plain_conj(Goals0, Goals, !Info).

:- pred polymorphism_process_par_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, cache_maps::in, poly_info::in, poly_info::out)
    is det.

polymorphism_process_par_conj([], [], _, !Info).
polymorphism_process_par_conj([Goal0 | Goals0], [Goal | Goals],
        InitialSnapshot, !Info) :-
    % Any variable that a later parallel conjunct reuses from an earlier
    % parallel conjunct (a) will definitely require synchronization, whose
    % cost will be greater than the cost of building a typeinfo from scratch,
    % and (b) may drastically reduce the available parallelism, if the earlier
    % conjunct produces the variable late but the later conjunct requires it
    % early.
    set_cache_maps_snapshot("par conjunct", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_par_conj(Goals0, Goals, InitialSnapshot, !Info).

:- pred polymorphism_process_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    cache_maps::in, poly_info::in, poly_info::out) is det.

polymorphism_process_disj([], [], _, !Info).
polymorphism_process_disj([Goal0 | Goals0], [Goal | Goals], InitialSnapshot,
        !Info) :-
    set_cache_maps_snapshot("disjunct", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    polymorphism_process_disj(Goals0, Goals, InitialSnapshot, !Info).

:- pred polymorphism_process_cases(list(case)::in, list(case)::out,
    cache_maps::in, poly_info::in, poly_info::out) is det.

polymorphism_process_cases([], [], _, !Info).
polymorphism_process_cases([Case0 | Cases0], [Case | Cases], InitialSnapshot,
        !Info) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_cache_maps_snapshot("case", InitialSnapshot, !Info),
    polymorphism_process_goal(Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    polymorphism_process_cases(Cases0, Cases, InitialSnapshot, !Info).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- end_module check_hlds.polymorphism_goal.
%---------------------------------------------------------------------------%
