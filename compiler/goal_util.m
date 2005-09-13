%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: conway.
%
% This module provides various utility procedures for manipulating HLDS goals,
% e.g. some functionality for renaming variables in goals.

%-----------------------------------------------------------------------------%

:- module hlds__goal_util.
:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module term.

    % Given a goal and an initial instmap, compute the final instmap that
    % results from the initial instmap after execution of the goal.
    %
:- pred update_instmap(hlds_goal::in, instmap::in, instmap::out) is det.

:- type prog_var_renaming == map(prog_var, prog_var).

    % create_renaming(OutputVars, InstMapDelta, !VarTypes, !VarSet,
    %   UnifyGoals, NewVars, Renaming):
    %
    % This predicate is intended for use in program transformations
    % that need to wrap up semidet goals, replacing Goal with
    % ( Goal' -> UnifyGoals, ... ; ...), where Goal' has its output
    % variables (OutputVars) replaced with new variables (NewVars),
    % with the mapping from OutputVars to NewVars being Renaming.
    % VarTypes and Varset are updated for the new variables. The final
    % insts of NewVar are taken from the insts of the corresponding
    % OutputVar in InstMapDelta (the initial inst is free).
    %
:- pred create_renaming(list(prog_var)::in, instmap_delta::in,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    list(hlds_goal)::out, list(prog_var)::out,
    prog_var_renaming::out) is det.

% The predicates rename_var* take a structure and a mapping from var -> var
% and apply that translation. If a var in the input structure does not
% occur as a key in the mapping, then the variable is left unsubstituted.

    % goal_util__rename_vars_in_goals(GoalList, MustRename, Substitution,
    %   NewGoalList).
:- pred goal_util__rename_vars_in_goals(bool::in, prog_var_renaming::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

:- pred goal_util__rename_vars_in_goal(prog_var_renaming::in,
    hlds_goal::in, hlds_goal::out) is det.

:- pred goal_util__must_rename_vars_in_goal(prog_var_renaming::in,
    hlds_goal::in, hlds_goal::out) is det.

:- pred goal_util__rename_vars_in_var_set(bool::in, prog_var_renaming::in,
    set(prog_var)::in, set(prog_var)::out) is det.

:- pred goal_util__rename_var_list(bool::in, map(var(T), var(T))::in,
    list(var(T))::in, list(var(T))::out) is det.

:- pred goal_util__rename_var(bool::in, map(var(V), var(V))::in,
    var(V)::in, var(V)::out) is det.

    % goal_util__create_variables(OldVariables, OldVarset, InitialVarTypes,
    %   InitialSubstitution, OldVarTypes, OldVarNames,  NewVarset,
    %   NewVarTypes, NewSubstitution):
    %
    % goal_util__create_variables takes a list of variables, a varset, and map
    % from vars to types and an initial substitution, and creates new instances
    % of each of the source variables in the substitution, adding each new
    % variable to the varset and the var types map. The name and type of each
    % new variable is found by looking up in the type map given in the 5th
    % argument - the last input. (This interface will not easily admit
    % uniqueness in the type map for this reason - such is the sacrifice
    % for generality.)
    %
:- pred goal_util__create_variables(list(prog_var)::in, prog_varset::in,
    map(prog_var, type)::in, prog_varset::in, prog_varset::out,
    map(prog_var, type)::in, map(prog_var, type)::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

    % Return all the variables in the goal.
    % Unlike quantification.goal_vars, this predicate returns
    % even the explicitly quantified variables.
    %
:- pred goal_util__goal_vars(hlds_goal::in, set(prog_var)::out) is det.

    % Return all the variables in the list of goals.
    % Unlike quantification.goal_vars, this predicate returns
    % even the explicitly quantified variables.
    %
:- pred goal_util__goals_goal_vars(list(hlds_goal)::in, set(prog_var)::in,
    set(prog_var)::out) is det.

    % Return all the variables in a generic call.
    %
:- pred goal_util__generic_call_vars(generic_call::in, list(prog_var)::out)
    is det.

    % Attach the given goal features to the given goal and all its subgoals.
    %
:- pred goal_util__attach_features_to_all_goals(list(goal_feature)::in,
    hlds_goal::in, hlds_goal::out) is det.

    % goal_util__extra_nonlocal_typeinfos(TypeInfoMap, TypeClassInfoMap,
    %   VarTypes, ExistQVars, NonLocals, NonLocalTypeInfos):
    %
    % Compute which type-info and type-class-info variables may need to be
    % non-local to a goal.
    %
    % A type-info variable may be non-local to a goal if any of the ordinary
    % non-local variables for that goal are polymorphically typed with a type
    % that depends on that type-info variable, or if the type-info is for an
    % existentially quantified type variable.
    %
    % In addition, a typeclass-info may be non-local to a goal if any of the
    % non-local variables for that goal are polymorphically typed and are
    % constrained by the typeclass constraints for that typeclass-info
    % variable, or if the the type-class-info is for an existential constraint,
    % i.e. a constraint which contrains an existentially quantified type
    % variable.
    %
:- pred goal_util__extra_nonlocal_typeinfos(rtti_varmaps::in,
    map(prog_var, type)::in, existq_tvars::in,
    set(prog_var)::in, set(prog_var)::out) is det.

    % See whether the goal is a branched structure.
    %
:- pred goal_util__goal_is_branched(hlds_goal_expr::in) is semidet.

    % Return an indication of the size of the goal.
    %
:- pred goal_size(hlds_goal::in, int::out) is det.

    % Return an indication of the size of the list of goals.
    %
:- pred goals_size(list(hlds_goal)::in, int::out) is det.

    % Return an indication of the size of the list of clauses.
    %
:- pred clause_list_size(list(clause)::in, int::out) is det.

    % Test whether the goal calls the given procedure.
    %
:- pred goal_calls(hlds_goal, pred_proc_id).
:- mode goal_calls(in, in) is semidet.
:- mode goal_calls(in, out) is nondet.

    % Test whether the goal calls the given predicate.
    % This is useful before mode analysis when the proc_ids
    % have not been determined.
    %
:- pred goal_calls_pred_id(hlds_goal, pred_id).
:- mode goal_calls_pred_id(in, in) is semidet.
:- mode goal_calls_pred_id(in, out) is nondet.

    % Test whether the goal contains a reconstruction
    % (a construction where the `cell_to_reuse' field is `yes(_)').
    %
:- pred goal_contains_reconstruction(hlds_goal::in) is semidet.

    % goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
    % i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
    % or indirect sub-goal.
    %
:- pred goal_contains_goal(hlds_goal::in, hlds_goal::out) is multi.

    % direct_subgoal(Goal, DirectSubGoal) is true iff DirectSubGoal is
    % a direct sub-goal of Goal.
    %
:- pred direct_subgoal(hlds_goal_expr::in, hlds_goal::out) is nondet.

    % Returns all the predids that are used within a goal.
    %
:- pred predids_from_goal(hlds_goal::in, list(pred_id)::out) is det.

    % Returns all the predids that are used in a list of goals.
    %
:- pred predids_from_goals(list(hlds_goal)::in, list(pred_id)::out) is det.

%-----------------------------------------------------------------------------%

    % Convert a switch back into a disjunction. This is needed
    % for the magic set transformation.
    % This aborts if any of the constructors are existentially typed.
    %
:- pred goal_util__switch_to_disjunction(prog_var::in, list(case)::in,
    instmap::in, list(hlds_goal)::out, prog_varset::in, prog_varset::out,
    map(prog_var, type)::in, map(prog_var, type)::out,
    module_info::in, module_info::out) is det.

    % Convert a case into a conjunction by adding a tag test
    % (deconstruction unification) to the case goal.
    % This aborts if the constructor is existentially typed.
    %
:- pred goal_util__case_to_disjunct(prog_var::in, cons_id::in, hlds_goal::in,
    instmap::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    map(prog_var, type)::in, map(prog_var, type)::out,
    module_info::in, module_info::out) is det.

    % Transform an if-then-else into ( Cond, Then ; \+ Cond, Else ),
    % since magic.m and rl_gen.m don't handle if-then-elses.
    %
:- pred goal_util__if_then_else_to_disjunction(hlds_goal::in, hlds_goal::in,
    hlds_goal::in, hlds_goal_info::in, hlds_goal_expr::out) is det.

%-----------------------------------------------------------------------------%

    % goal_util__can_reorder_goals(ModuleInfo, FullyStrict, Goal1, Goal2).
    %
    % Goals can be reordered if
    % - the goals are independent
    % - the goals are not impure
    % - any possible change in termination behaviour is allowed
    %   according to the semantics options.
    %
:- pred goal_util__can_reorder_goals(module_info::in, vartypes::in, bool::in,
    instmap::in, hlds_goal::in, instmap::in, hlds_goal::in) is semidet.

    % goal_util__reordering_maintains_termination(ModuleInfo,
    %    FullyStrict, Goal1, Goal2)
    %
    % Succeeds if any possible change in termination behaviour from
    % reordering the goals is allowed according to the semantics options.
    % The information computed by termination analysis is used when
    % making this decision.
    %
:- pred goal_util__reordering_maintains_termination(module_info::in, bool::in,
    hlds_goal::in, hlds_goal::in) is semidet.

    % generate_simple_call(ModuleName, ProcName, PredOrFunc, ModeNo,
    %   Detism, Args, Features, InstMapDelta, ModuleInfo, Context,
    %   CallGoal):
    %
    % Generate a call to a builtin procedure (e.g. from the private_builtin
    % or table_builtin module). This is used by HLDS->HLDS transformation
    % passes that introduce calls to builtin procedures.
    %
    % If ModeNo = only_mode then the predicate must have exactly one
    % procedure (an error is raised if this is not the case.)
    %
    % If ModeNo = mode_no(N) then the Nth procedure is used, counting
    % from 0.
    %
:- pred goal_util__generate_simple_call(module_name::in, string::in,
    pred_or_func::in, mode_no::in, determinism::in, list(prog_var)::in,
    list(goal_feature)::in, assoc_list(prog_var, inst)::in,
    module_info::in, term__context::in, hlds_goal::out) is det.

    % generate_foreign_proc(ModuleName, ProcName, PredOrFunc,
    %   ModeNo, Detism, Attributes, Args, ExtraArgs, PrefixCode, Code,
    %   SuffixCode, Features, InstMapDelta, ModuleInfo, Context,
    %   CallGoal):
    %
    % generate_foreign_proc is similar to generate_simple_call,
    % but also assumes that the called predicate is defined via a
    % foreign_proc, that the foreign_proc's arguments are as given in
    % Args, its attributes are Attributes, and its code is Code.
    % As well as returning a foreign_code instead of a call, effectively
    % inlining the call, generate_foreign_proc also puts PrefixCode
    % before Code, SuffixCode after Code, and passes ExtraArgs as well
    % as Args.
    %
:- pred goal_util__generate_foreign_proc(module_name::in, string::in,
    pred_or_func::in, mode_no::in, determinism::in,
    pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, list(foreign_arg)::in, string::in, string::in,
    string::in, list(goal_feature)::in, assoc_list(prog_var, inst)::in,
    module_info::in, term__context::in, hlds_goal::out) is det.

    % Generate a cast goal.  The input and output insts are just ground.
    %
:- pred goal_util__generate_cast(cast_type::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal::out) is det.

    % This version takes input and output inst arguments, which may
    % be necessary when casting, say, solver type values with inst
    % any, or casting between enumeration types and ints.
    %
:- pred goal_util__generate_cast(cast_type::in, prog_var::in, prog_var::in,
    (inst)::in, (inst)::in, prog_context::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__det_analysis.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_form.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_llds.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_type.
:- import_module parse_tree__prog_util.

:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module varset.

%-----------------------------------------------------------------------------%

update_instmap(_Goal0 - GoalInfo0, !InstMap) :-
    goal_info_get_instmap_delta(GoalInfo0, DeltaInstMap),
    instmap__apply_instmap_delta(!.InstMap, DeltaInstMap, !:InstMap).

%-----------------------------------------------------------------------------%

create_renaming(OrigVars, InstMapDelta, !VarTypes, !VarSet, Unifies, NewVars,
        Renaming) :-
    create_renaming_2(OrigVars, InstMapDelta, !VarTypes, !VarSet,
        [], RevUnifies, [], RevNewVars, map__init, Renaming),
    list__reverse(RevNewVars, NewVars),
    list__reverse(RevUnifies, Unifies).

:- pred create_renaming_2(list(prog_var)::in, instmap_delta::in,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(prog_var)::in, list(prog_var)::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

create_renaming_2([], _, !VarTypes, !VarSet, !RevUnifies, !RevNewVars,
        !Renaming).
create_renaming_2([OrigVar | OrigVars], InstMapDelta, !VarTypes, !VarSet,
        !RevUnifies, !RevNewVars, !Renaming) :-
    varset__new_var(!.VarSet, NewVar, !:VarSet),
    map__lookup(!.VarTypes, OrigVar, Type),
    svmap__det_insert(NewVar, Type, !VarTypes),
    ( instmap_delta_search_var(InstMapDelta, OrigVar, DeltaInst) ->
        NewInst = DeltaInst
    ;
        unexpected(this_file, "create_renaming_2: cannot get new inst")
    ),
    Mode = ((NewInst -> NewInst) - (free -> NewInst)),
    UnifyInfo = assign(OrigVar, NewVar),
    UnifyContext = unify_context(explicit, []),
    GoalExpr = unify(OrigVar, var(NewVar), Mode, UnifyInfo, UnifyContext),
    set__list_to_set([OrigVar, NewVar], NonLocals),
    instmap_delta_from_assoc_list([OrigVar - NewInst], UnifyInstMapDelta),
    goal_info_init(NonLocals, UnifyInstMapDelta, det, pure,
        term__context_init, GoalInfo),
    Goal = GoalExpr - GoalInfo,
    !:RevUnifies = [Goal | !.RevUnifies],
    svmap__det_insert(OrigVar, NewVar, !Renaming),
    !:RevNewVars = [NewVar | !.RevNewVars],
    create_renaming_2(OrigVars, InstMapDelta, !VarTypes, !VarSet,
        !RevUnifies, !RevNewVars, !Renaming).

%-----------------------------------------------------------------------------%

goal_util__create_variables([], _OldVarNames, _OldVarTypes,
        !Varset, !VarTypes, !Subn).
goal_util__create_variables([V | Vs], OldVarNames, OldVarTypes,
        !Varset, !VarTypes, !Subn) :-
    ( map__contains(!.Subn, V) ->
        true
    ;
        varset__new_var(!.Varset, NV, !:Varset),
        ( varset__search_name(OldVarNames, V, Name) ->
            varset__name_var(!.Varset, NV, Name, !:Varset)
        ;
            true
        ),
        svmap__det_insert(V, NV, !Subn),
        ( map__search(OldVarTypes, V, VT) ->
            svmap__set(NV, VT, !VarTypes)
        ;
            true
        )
    ),
    goal_util__create_variables(Vs, OldVarNames, OldVarTypes,
        !Varset, !VarTypes, !Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__init_subn(assoc_list(prog_var, prog_var)::in,
    prog_var_renaming::in, prog_var_renaming::out) is det.

goal_util__init_subn([], !Subn).
goal_util__init_subn([A - H | Vs], !Subn) :-
    svmap__set(H, A, !Subn),
    goal_util__init_subn(Vs, !Subn).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_pair_list(bool::in, prog_var_renaming::in,
    assoc_list(prog_var, T)::in, list(pair(prog_var, T))::out) is det.

goal_util__rename_var_pair_list(_Must, _Subn, [], []).
goal_util__rename_var_pair_list(Must, Subn, [V - D | VDs], [N - D | NDs]) :-
    goal_util__rename_var(Must, Subn, V, N),
    goal_util__rename_var_pair_list(Must, Subn, VDs, NDs).

goal_util__rename_var_list(_Must, _Subn, [], []).
goal_util__rename_var_list(Must, Subn, [V | Vs], [N | Ns]) :-
    goal_util__rename_var(Must, Subn, V, N),
    goal_util__rename_var_list(Must, Subn, Vs, Ns).

goal_util__rename_var(Must, Subn, V, N) :-
    ( map__search(Subn, V, N0) ->
        N = N0
    ;
        (
            Must = no,
            N = V
        ;
            Must = yes,
            term__var_to_int(V, VInt),
            string__format("goal_util__rename_var: " ++
                "no substitute for var %i", [i(VInt)], Msg),
            error(Msg)
        )
    ).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_goal(Subn, Goal0, Goal) :-
    goal_util__rename_vars_in_goal(no, Subn, Goal0, Goal).

goal_util__must_rename_vars_in_goal(Subn, Goal0, Goal) :-
    goal_util__rename_vars_in_goal(yes, Subn, Goal0, Goal).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_goals(_, _, [], []).
goal_util__rename_vars_in_goals(Must, Subn, [Goal0 | Goals0], [Goal | Goals]) :-
    goal_util__rename_vars_in_goal(Must, Subn, Goal0, Goal),
    goal_util__rename_vars_in_goals(Must, Subn, Goals0, Goals).

:- pred goal_util__rename_vars_in_goal(bool::in, prog_var_renaming::in,
    hlds_goal::in, hlds_goal::out) is det.

goal_util__rename_vars_in_goal(Must, Subn,
        Goal0 - GoalInfo0, Goal - GoalInfo) :-
    goal_util__rename_vars_in_goal_expr(Must, Subn, Goal0, Goal),
    goal_util__rename_vars_in_goal_info(Must, Subn, GoalInfo0, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_goal_expr(bool::in, prog_var_renaming::in,
    hlds_goal_expr::in, hlds_goal_expr::out) is det.

goal_util__rename_vars_in_goal_expr(Must, Subn, conj(Goals0), conj(Goals)) :-
    goal_util__rename_vars_in_goals(Must, Subn, Goals0, Goals).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        par_conj(Goals0), par_conj(Goals)) :-
    goal_util__rename_vars_in_goals(Must, Subn, Goals0, Goals).

goal_util__rename_vars_in_goal_expr(Must, Subn, disj(Goals0), disj(Goals)) :-
    goal_util__rename_vars_in_goals(Must, Subn, Goals0, Goals).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        switch(Var0, Det, Cases0), switch(Var, Det, Cases)) :-
    goal_util__rename_var(Must, Subn, Var0, Var),
    goal_util__rename_vars_in_cases(Must, Subn, Cases0, Cases).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        if_then_else(Vars0, Cond0, Then0, Else0),
        if_then_else(Vars, Cond, Then, Else)) :-
    goal_util__rename_var_list(Must, Subn, Vars0, Vars),
    goal_util__rename_vars_in_goal(Must, Subn, Cond0, Cond),
    goal_util__rename_vars_in_goal(Must, Subn, Then0, Then),
    goal_util__rename_vars_in_goal(Must, Subn, Else0, Else).

goal_util__rename_vars_in_goal_expr(Must, Subn, not(Goal0), not(Goal)) :-
    goal_util__rename_vars_in_goal(Must, Subn, Goal0, Goal).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        scope(Reason0, Goal0), scope(Reason, Goal)) :-
    (
        Reason0 = exist_quant(Vars0),
        goal_util__rename_var_list(Must, Subn, Vars0, Vars),
        Reason = exist_quant(Vars)
    ;
        Reason0 = promise_purity(_, _),
        Reason = Reason0
    ;
        Reason0 = promise_equivalent_solutions(Vars0),
        goal_util__rename_var_list(Must, Subn, Vars0, Vars),
        Reason = promise_equivalent_solutions(Vars)
    ;
        Reason0 = barrier(_),
        Reason = Reason0
    ;
        Reason0 = commit(_),
        Reason = Reason0
    ;
        Reason0 = from_ground_term(Var0),
        goal_util__rename_var(Must, Subn, Var0, Var),
        Reason = from_ground_term(Var)
    ),
    goal_util__rename_vars_in_goal(Must, Subn, Goal0, Goal).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        generic_call(GenericCall0, Args0, Modes, Det),
        generic_call(GenericCall, Args, Modes, Det)) :-
    goal_util__rename_generic_call(Must, Subn, GenericCall0, GenericCall),
    goal_util__rename_var_list(Must, Subn, Args0, Args).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        call(PredId, ProcId, Args0, Builtin, Context, Sym),
        call(PredId, ProcId, Args, Builtin, Context, Sym)) :-
    goal_util__rename_var_list(Must, Subn, Args0, Args).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        unify(LHS0, RHS0, Mode, Unify0, Context),
        unify(LHS, RHS, Mode, Unify, Context)) :-
    goal_util__rename_var(Must, Subn, LHS0, LHS),
    goal_util__rename_unify_rhs(Must, Subn, RHS0, RHS),
    goal_util__rename_unify(Must, Subn, Unify0, Unify).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        foreign_proc(A, B, C, Args0, Extra0, F),
        foreign_proc(A, B, C, Args, Extra, F)) :-
    goal_util__rename_arg_list(Must, Subn, Args0, Args),
    goal_util__rename_arg_list(Must, Subn, Extra0, Extra).

goal_util__rename_vars_in_goal_expr(Must, Subn,
        shorthand(ShorthandGoal0), shorthand(ShrothandGoal)) :-
    goal_util__rename_vars_in_shorthand(Must, Subn,
        ShorthandGoal0, ShrothandGoal).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_shorthand(bool::in, prog_var_renaming::in,
    shorthand_goal_expr::in, shorthand_goal_expr::out) is det.

goal_util__rename_vars_in_shorthand(Must, Subn,
        bi_implication(LHS0, RHS0), bi_implication(LHS, RHS)) :-
    goal_util__rename_vars_in_goal(Must, Subn, LHS0, LHS),
    goal_util__rename_vars_in_goal(Must, Subn, RHS0, RHS).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_arg_list(bool::in, prog_var_renaming::in,
    list(foreign_arg)::in, list(foreign_arg)::out) is det.

goal_util__rename_arg_list(_Must, _Subn, [], []).
goal_util__rename_arg_list(Must, Subn, [Arg0 | Args0], [Arg | Args]) :-
    goal_util__rename_arg(Must, Subn, Arg0, Arg),
    goal_util__rename_arg_list(Must, Subn, Args0, Args).

:- pred goal_util__rename_arg(bool::in, prog_var_renaming::in,
    foreign_arg::in, foreign_arg::out) is det.

goal_util__rename_arg(Must, Subn,
        foreign_arg(Var0, B, C), foreign_arg(Var, B, C)) :-
    goal_util__rename_var(Must, Subn, Var0, Var).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_cases(bool::in, prog_var_renaming::in,
    list(case)::in, list(case)::out) is det.

goal_util__rename_vars_in_cases(_Must, _Subn, [], []).
goal_util__rename_vars_in_cases(Must, Subn,
        [case(Cons, G0) | Gs0], [case(Cons, G) | Gs]) :-
    goal_util__rename_vars_in_goal(Must, Subn, G0, G),
    goal_util__rename_vars_in_cases(Must, Subn, Gs0, Gs).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_unify_rhs(bool::in, prog_var_renaming::in,
    unify_rhs::in, unify_rhs::out) is det.

goal_util__rename_unify_rhs(Must, Subn, var(Var0), var(Var)) :-
    goal_util__rename_var(Must, Subn, Var0, Var).
goal_util__rename_unify_rhs(Must, Subn,
        functor(Functor, E, ArgVars0), functor(Functor, E, ArgVars)) :-
    goal_util__rename_var_list(Must, Subn, ArgVars0, ArgVars).
goal_util__rename_unify_rhs(Must, Subn,
        lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
            NonLocals0, Vars0, Modes, Det, Goal0),
        lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
            NonLocals, Vars, Modes, Det, Goal)) :-
    goal_util__rename_var_list(Must, Subn, NonLocals0, NonLocals),
    goal_util__rename_var_list(Must, Subn, Vars0, Vars),
    goal_util__rename_vars_in_goal(Must, Subn, Goal0, Goal).

:- pred goal_util__rename_unify(bool::in, prog_var_renaming::in,
    unification::in, unification::out) is det.

goal_util__rename_unify(Must, Subn,
        construct(Var0, ConsId, Vars0, Modes, How0, Uniq, SubInfo0),
        construct(Var, ConsId, Vars, Modes, How, Uniq, SubInfo)) :-
    goal_util__rename_var(Must, Subn, Var0, Var),
    goal_util__rename_var_list(Must, Subn, Vars0, Vars),
    (
        How0 = reuse_cell(cell_to_reuse(ReuseVar0, B, C)),
        goal_util__rename_var(Must, Subn, ReuseVar0, ReuseVar),
        How = reuse_cell(cell_to_reuse(ReuseVar, B, C))
    ;
        How0 = construct_dynamically,
        How = How0
    ;
        How0 = construct_statically(_),
        How = How0
    ),
    (
        SubInfo0 = construct_sub_info(MTA, MaybeSize0),
        (
            MaybeSize0 = no,
            MaybeSize = no
        ;
            MaybeSize0 = yes(Size0),
            (
                Size0 = known_size(_),
                Size = Size0
            ;
                Size0 = dynamic_size(SizeVar0),
                goal_util__rename_var(Must, Subn, SizeVar0, SizeVar),
                Size = dynamic_size(SizeVar)
            ),
            MaybeSize = yes(Size)
        ),
        SubInfo = construct_sub_info(MTA, MaybeSize)
    ;
        SubInfo0 = no_construct_sub_info,
        SubInfo = no_construct_sub_info
    ).
goal_util__rename_unify(Must, Subn,
        deconstruct(Var0, ConsId, Vars0, Modes, Cat, CanCGC),
        deconstruct(Var, ConsId, Vars, Modes, Cat, CanCGC)) :-
    goal_util__rename_var(Must, Subn, Var0, Var),
    goal_util__rename_var_list(Must, Subn, Vars0, Vars).
goal_util__rename_unify(Must, Subn, assign(L0, R0), assign(L, R)) :-
    goal_util__rename_var(Must, Subn, L0, L),
    goal_util__rename_var(Must, Subn, R0, R).
goal_util__rename_unify(Must, Subn, simple_test(L0, R0), simple_test(L, R)) :-
    goal_util__rename_var(Must, Subn, L0, L),
    goal_util__rename_var(Must, Subn, R0, R).
goal_util__rename_unify(_Must, _Subn,
        complicated_unify(Modes, Cat, TypeInfoVars),
        complicated_unify(Modes, Cat, TypeInfoVars)).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_generic_call(bool::in, prog_var_renaming::in,
    generic_call::in, generic_call::out) is det.

goal_util__rename_generic_call(Must, Subn,
        higher_order(Var0, Purity, PredOrFunc, Arity),
        higher_order(Var, Purity, PredOrFunc, Arity)) :-
    goal_util__rename_var(Must, Subn, Var0, Var).
goal_util__rename_generic_call(Must, Subn,
        class_method(Var0, Method, ClassId, MethodId),
        class_method(Var, Method, ClassId, MethodId)) :-
    goal_util__rename_var(Must, Subn, Var0, Var).
goal_util__rename_generic_call(_, _, cast(CastType), cast(CastType)).
goal_util__rename_generic_call(_Must, _Subn,
        aditi_builtin(Builtin, PredCallId),
        aditi_builtin(Builtin, PredCallId)).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_var_maps(bool::in, prog_var_renaming::in,
    map(prog_var, T)::in, map(prog_var, T)::out) is det.

goal_util__rename_var_maps(Must, Subn, Map0, Map) :-
    map__to_assoc_list(Map0, AssocList0),
    goal_util__rename_var_maps_2(Must, Subn, AssocList0, AssocList),
    map__from_assoc_list(AssocList, Map).

:- pred goal_util__rename_var_maps_2(bool::in, map(var(V), var(V))::in,
    assoc_list(var(V), T)::in, assoc_list(var(V), T)::out) is det.

goal_util__rename_var_maps_2(_Must, _Subn, [], []).
goal_util__rename_var_maps_2(Must, Subn, [V - L | Vs], [N - L | Ns]) :-
    goal_util__rename_var(Must, Subn, V, N),
    goal_util__rename_var_maps_2(Must, Subn, Vs, Ns).

%-----------------------------------------------------------------------------%

:- pred goal_util__rename_vars_in_goal_info(bool::in, prog_var_renaming::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

goal_util__rename_vars_in_goal_info(Must, Subn, !GoalInfo) :-
    goal_info_get_nonlocals(!.GoalInfo, NonLocals0),
    goal_util__rename_vars_in_var_set(Must, Subn, NonLocals0, NonLocals),
    goal_info_set_nonlocals(NonLocals, !GoalInfo),

    goal_info_get_instmap_delta(!.GoalInfo, InstMap0),
    instmap_delta_apply_sub(Must, Subn, InstMap0, InstMap),
    goal_info_set_instmap_delta(InstMap, !GoalInfo),

    goal_info_get_code_gen_info(!.GoalInfo, CodeGenInfo0),
    goal_util__rename_vars_in_code_gen_info(Must, Subn,
        CodeGenInfo0, CodeGenInfo),
    goal_info_set_code_gen_info(CodeGenInfo, !GoalInfo).

%-----------------------------------------------------------------------------%

goal_util__rename_vars_in_var_set(Must, Subn, Vars0, Vars) :-
    set__to_sorted_list(Vars0, VarsList0),
    goal_util__rename_var_list(Must, Subn, VarsList0, VarsList),
    set__list_to_set(VarsList, Vars).

:- pred goal_util__rename_vars_in_code_gen_info(bool::in,
    prog_var_renaming::in,
    hlds_goal_code_gen_info::in, hlds_goal_code_gen_info::out) is det.

goal_util__rename_vars_in_code_gen_info(Must, Subn,
        CodeGenInfo0, CodeGenInfo) :-
    (
        CodeGenInfo0 = no_code_gen_info,
        CodeGenInfo = no_code_gen_info
    ;
        CodeGenInfo0 = llds_code_gen_info(LldsInfo0),
        rename_vars_in_llds_code_gen_info(Must, Subn, LldsInfo0, LldsInfo),
        CodeGenInfo = llds_code_gen_info(LldsInfo)
    ).

%-----------------------------------------------------------------------------%

goal_util__goal_vars(Goal - _GoalInfo, Set) :-
    goal_util__goal_vars_2(Goal, set__init, Set).

:- pred goal_util__goal_vars_2(hlds_goal_expr::in,
    set(prog_var)::in, set(prog_var)::out) is det.

goal_util__goal_vars_2(unify(Var, RHS, _, Unif, _), !Set) :-
    set__insert(!.Set, Var, !:Set),
    ( Unif = construct(_, _, _, _, CellToReuse, _, _) ->
        ( CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) ->
            set__insert(!.Set, Var, !:Set)
        ;
            true
        )
    ;
        true
    ),
    goal_util__rhs_goal_vars(RHS, !Set).

goal_util__goal_vars_2(generic_call(GenericCall, ArgVars, _, _), !Set) :-
    goal_util__generic_call_vars(GenericCall, Vars0),
    set__insert_list(!.Set, Vars0, !:Set),
    set__insert_list(!.Set, ArgVars, !:Set).

goal_util__goal_vars_2(call(_, _, ArgVars, _, _, _), !Set) :-
    set__insert_list(!.Set, ArgVars, !:Set).

goal_util__goal_vars_2(conj(Goals), !Set) :-
    goal_util__goals_goal_vars(Goals, !Set).

goal_util__goal_vars_2(par_conj(Goals), !Set) :-
    goal_util__goals_goal_vars(Goals, !Set).

goal_util__goal_vars_2(disj(Goals), !Set) :-
    goal_util__goals_goal_vars(Goals, !Set).

goal_util__goal_vars_2(switch(Var, _Det, Cases), !Set) :-
    set__insert(!.Set, Var, !:Set),
    goal_util__cases_goal_vars(Cases, !Set).

goal_util__goal_vars_2(scope(Reason, Goal - _), !Set) :-
    (
        Reason = exist_quant(Vars),
        set__insert_list(!.Set, Vars, !:Set)
    ;
        Reason = promise_purity(_, _)
    ;
        Reason = promise_equivalent_solutions(Vars),
        set__insert_list(!.Set, Vars, !:Set)
    ;
        Reason = barrier(_)
    ;
        Reason = commit(_)
    ;
        Reason = from_ground_term(Var),
        set__insert(!.Set, Var, !:Set)
    ),
    goal_util__goal_vars_2(Goal, !Set).

goal_util__goal_vars_2(not(Goal - _GoalInfo), !Set) :-
    goal_util__goal_vars_2(Goal, !Set).

goal_util__goal_vars_2(if_then_else(Vars, A - _, B - _, C - _), !Set) :-
    set__insert_list(!.Set, Vars, !:Set),
    goal_util__goal_vars_2(A, !Set),
    goal_util__goal_vars_2(B, !Set),
    goal_util__goal_vars_2(C, !Set).

goal_util__goal_vars_2(foreign_proc(_, _, _, Args, ExtraArgs, _), !Set) :-
    ArgVars = list__map(foreign_arg_var, Args),
    ExtraVars = list__map(foreign_arg_var, ExtraArgs),
    set__insert_list(!.Set, list__append(ArgVars, ExtraVars), !:Set).

goal_util__goal_vars_2(shorthand(ShorthandGoal), !Set) :-
    goal_util__goal_vars_2_shorthand(ShorthandGoal, !Set).

:- pred goal_util__goal_vars_2_shorthand(shorthand_goal_expr::in,
    set(prog_var)::in, set(prog_var)::out) is det.

goal_util__goal_vars_2_shorthand(bi_implication(LHS - _, RHS - _), !Set) :-
    goal_util__goal_vars_2(LHS, !Set),
    goal_util__goal_vars_2(RHS, !Set).

goal_util__goals_goal_vars([], !Set).
goal_util__goals_goal_vars([Goal - _ | Goals], !Set) :-
    goal_util__goal_vars_2(Goal, !Set),
    goal_util__goals_goal_vars(Goals, !Set).

:- pred goal_util__cases_goal_vars(list(case)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

goal_util__cases_goal_vars([], !Set).
goal_util__cases_goal_vars([case(_, Goal - _) | Cases], !Set) :-
    goal_util__goal_vars_2(Goal, !Set),
    goal_util__cases_goal_vars(Cases, !Set).

:- pred goal_util__rhs_goal_vars(unify_rhs::in,
    set(prog_var)::in, set(prog_var)::out) is det.

goal_util__rhs_goal_vars(RHS, !Set) :-
    RHS = var(X),
    set__insert(!.Set, X, !:Set).
goal_util__rhs_goal_vars(RHS, !Set) :-
    RHS = functor(_Functor, _, ArgVars),
    set__insert_list(!.Set, ArgVars, !:Set).
goal_util__rhs_goal_vars(RHS, !Set) :-
    RHS = lambda_goal(_, _, _, _, NonLocals, LambdaVars, _, _, Goal - _),
    set__insert_list(!.Set, NonLocals, !:Set),
    set__insert_list(!.Set, LambdaVars, !:Set),
    goal_util__goal_vars_2(Goal, !Set).

goal_util__generic_call_vars(higher_order(Var, _, _, _), [Var]).
goal_util__generic_call_vars(class_method(Var, _, _, _), [Var]).
goal_util__generic_call_vars(cast(_), []).
goal_util__generic_call_vars(aditi_builtin(_, _), []).

%-----------------------------------------------------------------------------%

attach_features_to_all_goals(Features, Goal0, Goal) :-
    Goal0 = GoalExpr0 - GoalInfo0,
    attach_features_goal_expr(Features, GoalExpr0, GoalExpr),
    list__foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = GoalExpr - GoalInfo.

:- pred attach_features_to_case(list(goal_feature)::in,
    case::in, case::out) is det.

attach_features_to_case(Features, case(ConsId, Goal0), case(ConsId, Goal)) :-
    attach_features_to_all_goals(Features, Goal0, Goal).

:- pred attach_features_goal_expr(list(goal_feature)::in,
    hlds_goal_expr::in, hlds_goal_expr::out) is det.

attach_features_goal_expr(Features, GoalExpr0, GoalExpr) :-
    (
        GoalExpr0 = conj(Goals0),
        list__map(attach_features_to_all_goals(Features),
            Goals0, Goals),
        GoalExpr = conj(Goals)
    ;
        GoalExpr0 = par_conj(Goals0),
        list__map(attach_features_to_all_goals(Features),
            Goals0, Goals),
        GoalExpr = par_conj(Goals)
    ;
        GoalExpr0 = disj(Goals0),
        list__map(attach_features_to_all_goals(Features),
            Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list__map(attach_features_to_case(Features), Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        attach_features_to_all_goals(Features, Cond0, Cond),
        attach_features_to_all_goals(Features, Then0, Then),
        attach_features_to_all_goals(Features, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = not(Goal0),
        attach_features_to_all_goals(Features, Goal0, Goal),
        GoalExpr = not(Goal)
    ;
        GoalExpr0 = scope(Reason, Goal0),
        attach_features_to_all_goals(Features, Goal0, Goal),
        GoalExpr = scope(Reason, Goal)
    ;
        GoalExpr0 = call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = foreign_proc(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        GoalExpr = GoalExpr0
    ).

%-----------------------------------------------------------------------------%

goal_util__extra_nonlocal_typeinfos(RttiVarMaps, VarTypes, ExistQVars,
        NonLocals, NonLocalTypeInfos) :-

        % Find all non-local type vars.  That is, type vars that are
        % existentially quantified or type vars that appear in the
        % type of a non-local prog_var.
        %
    set__to_sorted_list(NonLocals, NonLocalsList),
    map__apply_to_list(NonLocalsList, VarTypes, NonLocalsTypes),
    prog_type__vars_list(NonLocalsTypes, NonLocalTypeVarsList0),
    list__append(ExistQVars, NonLocalTypeVarsList0, NonLocalTypeVarsList),
    set__list_to_set(NonLocalTypeVarsList, NonLocalTypeVars),

        % Find all the type_infos that are non-local, that is,
        % type_infos for type vars that are non-local in the above
        % sense.
        %
    TypeVarToProgVar = (func(TypeVar) = ProgVar :-
            rtti_lookup_type_info_locn(RttiVarMaps, TypeVar, Locn),
            type_info_locn_var(Locn, ProgVar)
        ),
    NonLocalTypeInfoVars = set__map(TypeVarToProgVar, NonLocalTypeVars),

        % Find all the typeclass_infos that are non-local.  These
        % include all typeclass_infos that constrain a type variable
        % that is non-local in the above sense.
        %
    solutions_set((pred(Var::out) is nondet :-
            % Search through all arguments of all constraints
            % that the goal could have used.
            rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
            list__member(Constraint, Constraints),
            Constraint = constraint(_Name, ArgTypes),
            type_list_contains_var(ArgTypes, TypeVar),
            set__member(TypeVar, NonLocalTypeVars),

            % We found a constraint that is non-local.  Include
            % the variable holding its typeclass_info.
            rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var)
        ), NonLocalTypeClassInfoVars),
    NonLocalTypeInfos = set__union(NonLocalTypeInfoVars,
        NonLocalTypeClassInfoVars).

%-----------------------------------------------------------------------------%

goal_util__goal_is_branched(if_then_else(_, _, _, _)).
goal_util__goal_is_branched(switch(_, _, _)).
goal_util__goal_is_branched(disj(_)).

%-----------------------------------------------------------------------------%

goal_size(GoalExpr - _, Size) :-
    goal_expr_size(GoalExpr, Size).

goals_size([], 0).
goals_size([Goal | Goals], Size) :-
    goal_size(Goal, Size1),
    goals_size(Goals, Size2),
    Size = Size1 + Size2.

clause_list_size(Clauses, GoalSize) :-
    list__foldl(clause_size_increment, Clauses, 0, GoalSize0),
    ( Clauses = [_] ->
        GoalSize = GoalSize0
    ;
        % Add one for the disjunction.
        GoalSize = GoalSize0 + 1
    ).

:- pred clause_size_increment(clause::in, int::in, int::out) is det.

clause_size_increment(Clause, Size0, Size) :-
    Clause = clause(_, ClauseGoal, _, _),
    goal_size(ClauseGoal, ClauseSize),
    Size = Size0 + ClauseSize.

:- pred cases_size(list(case)::in, int::out) is det.

cases_size([], 0).
cases_size([case(_, Goal) | Cases], Size) :-
    goal_size(Goal, Size1),
    cases_size(Cases, Size2),
    Size = Size1 + Size2.

:- pred goal_expr_size(hlds_goal_expr::in, int::out) is det.

goal_expr_size(conj(Goals), Size) :-
    goals_size(Goals, Size).
goal_expr_size(par_conj(Goals), Size) :-
    goals_size(Goals, Size1),
    Size = Size1 + 1.
goal_expr_size(disj(Goals), Size) :-
    goals_size(Goals, Size1),
    Size = Size1 + 1.
goal_expr_size(switch(_, _, Goals), Size) :-
    cases_size(Goals, Size1),
    Size = Size1 + 1.
goal_expr_size(if_then_else(_, Cond, Then, Else), Size) :-
    goal_size(Cond, Size1),
    goal_size(Then, Size2),
    goal_size(Else, Size3),
    Size = Size1 + Size2 + Size3 + 1.
goal_expr_size(not(Goal), Size) :-
    goal_size(Goal, Size1),
    Size = Size1 + 1.
goal_expr_size(scope(_, Goal), Size) :-
    goal_size(Goal, Size1),
    Size = Size1 + 1.
goal_expr_size(call(_, _, _, _, _, _), 1).
goal_expr_size(generic_call(_, _, _, _), 1).
goal_expr_size(unify(_, _, _, _, _), 1).
goal_expr_size(foreign_proc(_, _, _, _, _, _), 1).
goal_expr_size(shorthand(ShorthandGoal), Size) :-
    goal_expr_size_shorthand(ShorthandGoal, Size).

:- pred goal_expr_size_shorthand(shorthand_goal_expr::in, int::out) is det.

goal_expr_size_shorthand(bi_implication(LHS, RHS), Size) :-
    goal_size(LHS, Size1),
    goal_size(RHS, Size2),
    Size = Size1 + Size2 + 1.

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls as
%   goal_calls(Goal, proc(PredId, ProcId)) :-
%       goal_contains_subgoal(Goal, call(PredId, ProcId, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls(GoalExpr - _, PredProcId) :-
    goal_expr_calls(GoalExpr, PredProcId).

:- pred goals_calls(list(hlds_goal), pred_proc_id).
:- mode goals_calls(in, in) is semidet.
:- mode goals_calls(in, out) is nondet.

goals_calls([Goal | Goals], PredProcId) :-
    (
        goal_calls(Goal, PredProcId)
    ;
        goals_calls(Goals, PredProcId)
    ).

:- pred cases_calls(list(case), pred_proc_id).
:- mode cases_calls(in, in) is semidet.
:- mode cases_calls(in, out) is nondet.

cases_calls([case(_, Goal) | Cases], PredProcId) :-
    (
        goal_calls(Goal, PredProcId)
    ;
        cases_calls(Cases, PredProcId)
    ).

:- pred goal_expr_calls(hlds_goal_expr, pred_proc_id).
:- mode goal_expr_calls(in, in) is semidet.
:- mode goal_expr_calls(in, out) is nondet.

goal_expr_calls(conj(Goals), PredProcId) :-
    goals_calls(Goals, PredProcId).
goal_expr_calls(par_conj(Goals), PredProcId) :-
    goals_calls(Goals, PredProcId).
goal_expr_calls(disj(Goals), PredProcId) :-
    goals_calls(Goals, PredProcId).
goal_expr_calls(switch(_, _, Goals), PredProcId) :-
    cases_calls(Goals, PredProcId).
goal_expr_calls(if_then_else(_, Cond, Then, Else), PredProcId) :-
    (
        goal_calls(Cond, PredProcId)
    ;
        goal_calls(Then, PredProcId)
    ;
        goal_calls(Else, PredProcId)
    ).
goal_expr_calls(not(Goal), PredProcId) :-
    goal_calls(Goal, PredProcId).
goal_expr_calls(scope(_, Goal), PredProcId) :-
    goal_calls(Goal, PredProcId).
goal_expr_calls(call(PredId, ProcId, _, _, _, _), proc(PredId, ProcId)).

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls_pred_id as
%   goal_calls_pred_id(Goal, PredId) :-
%       goal_contains_subgoal(Goal, call(PredId, _, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls_pred_id(GoalExpr - _, PredId) :-
    goal_expr_calls_pred_id(GoalExpr, PredId).

:- pred goals_calls_pred_id(list(hlds_goal), pred_id).
:- mode goals_calls_pred_id(in, in) is semidet.
:- mode goals_calls_pred_id(in, out) is nondet.

goals_calls_pred_id([Goal | Goals], PredId) :-
    (
        goal_calls_pred_id(Goal, PredId)
    ;
        goals_calls_pred_id(Goals, PredId)
    ).

:- pred cases_calls_pred_id(list(case), pred_id).
:- mode cases_calls_pred_id(in, in) is semidet.
:- mode cases_calls_pred_id(in, out) is nondet.

cases_calls_pred_id([case(_, Goal) | Cases], PredId) :-
    (
        goal_calls_pred_id(Goal, PredId)
    ;
        cases_calls_pred_id(Cases, PredId)
    ).

:- pred goal_expr_calls_pred_id(hlds_goal_expr, pred_id).
:- mode goal_expr_calls_pred_id(in, in) is semidet.
:- mode goal_expr_calls_pred_id(in, out) is nondet.

goal_expr_calls_pred_id(conj(Goals), PredId) :-
    goals_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(par_conj(Goals), PredId) :-
    goals_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(disj(Goals), PredId) :-
    goals_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(switch(_, _, Goals), PredId) :-
    cases_calls_pred_id(Goals, PredId).
goal_expr_calls_pred_id(if_then_else(_, Cond, Then, Else), PredId) :-
    (
        goal_calls_pred_id(Cond, PredId)
    ;
        goal_calls_pred_id(Then, PredId)
    ;
        goal_calls_pred_id(Else, PredId)
    ).
goal_expr_calls_pred_id(not(Goal), PredId) :-
    goal_calls_pred_id(Goal, PredId).
goal_expr_calls_pred_id(scope(_, Goal), PredId) :-
    goal_calls_pred_id(Goal, PredId).
goal_expr_calls_pred_id(call(PredId, _, _, _, _, _), PredId).

%-----------------------------------------------------------------------------%

goal_contains_reconstruction(_Goal - _) :-
    % This will only succeed on the alias branch with structure reuse.
    semidet_fail.
    %goal_expr_contains_reconstruction(Goal).

:- pred goal_expr_contains_reconstruction(hlds_goal_expr::in) is semidet.

goal_expr_contains_reconstruction(conj(Goals)) :-
    goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(disj(Goals)) :-
    goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(par_conj(Goals)) :-
    goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(switch(_, _, Cases)) :-
    list__member(Case, Cases),
    Case = case(_, Goal),
    goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(if_then_else(_, Cond, Then, Else)) :-
    goals_contain_reconstruction([Cond, Then, Else]).
goal_expr_contains_reconstruction(not(Goal)) :-
    goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(scope(_, Goal)) :-
    goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(unify(_, _, _, Unify, _)) :-
    Unify = construct(_, _, _, _, HowToConstruct, _, _),
    HowToConstruct = reuse_cell(_).

:- pred goals_contain_reconstruction(list(hlds_goal)::in) is semidet.

goals_contain_reconstruction(Goals) :-
    list__member(Goal, Goals),
    goal_contains_reconstruction(Goal).

%-----------------------------------------------------------------------------%

    % goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
    % i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
    % or indirect sub-goal.
    %
goal_contains_goal(Goal, Goal).
goal_contains_goal(Goal - _, SubGoal) :-
    direct_subgoal(Goal, DirectSubGoal),
    goal_contains_goal(DirectSubGoal, SubGoal).

    % direct_subgoal(Goal, SubGoal) is true iff SubGoal is
    % a direct sub-goal of Goal.
    %
direct_subgoal(scope(_, Goal), Goal).
direct_subgoal(not(Goal), Goal).
direct_subgoal(if_then_else(_, If, Then, Else), Goal) :-
    ( Goal = If
    ; Goal = Then
    ; Goal = Else
    ).
direct_subgoal(conj(ConjList), Goal) :-
    list__member(Goal, ConjList).
direct_subgoal(par_conj(ConjList), Goal) :-
    list__member(Goal, ConjList).
direct_subgoal(disj(DisjList), Goal) :-
    list__member(Goal, DisjList).
direct_subgoal(switch(_, _, CaseList), Goal) :-
    list__member(Case, CaseList),
    Case = case(_, Goal).

%-----------------------------------------------------------------------------%

goal_util__switch_to_disjunction(_, [], _, [],
        !VarSet, !VarTypes, !ModuleInfo).
goal_util__switch_to_disjunction(Var, [case(ConsId, Goal0) | Cases], InstMap,
        [Goal | Goals], !VarSet, !VarTypes, !ModuleInfo) :-
    goal_util__case_to_disjunct(Var, ConsId, Goal0, InstMap, Goal,
        !VarSet, !VarTypes, !ModuleInfo),
    goal_util__switch_to_disjunction(Var, Cases, InstMap, Goals,
        !VarSet, !VarTypes, !ModuleInfo).

goal_util__case_to_disjunct(Var, ConsId, CaseGoal, InstMap, Disjunct,
        !VarSet, !VarTypes, !ModuleInfo) :-
    ConsArity = cons_id_arity(ConsId),
    varset__new_vars(!.VarSet, ConsArity, ArgVars, !:VarSet),
    map__lookup(!.VarTypes, Var, VarType),
    type_util__get_cons_id_arg_types(!.ModuleInfo,
        VarType, ConsId, ArgTypes),
    svmap__det_insert_from_corresponding_lists(ArgVars, ArgTypes, !VarTypes),
    instmap__lookup_var(InstMap, Var, Inst0),
    (
        inst_expand(!.ModuleInfo, Inst0, Inst1),
        get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
    ->
        ArgInsts = ArgInsts1
    ;
        error("goal_util__case_to_disjunct - get_arg_insts failed")
    ),
    InstToUniMode = (pred(ArgInst::in, ArgUniMode::out) is det :-
            ArgUniMode = ((ArgInst - free) -> (ArgInst - ArgInst))
        ),
    list__map(InstToUniMode, ArgInsts, UniModes),
    UniMode = (Inst0 -> Inst0) - (Inst0 -> Inst0),
    UnifyContext = unify_context(explicit, []),
    Unification = deconstruct(Var, ConsId, ArgVars, UniModes,
        can_fail, cannot_cgc),
    ExtraGoal = unify(Var, functor(ConsId, no, ArgVars),
        UniMode, Unification, UnifyContext),
    set__singleton_set(NonLocals, Var),
    instmap_delta_init_reachable(ExtraInstMapDelta0),
    instmap_delta_bind_var_to_functor(Var, VarType, ConsId, InstMap,
        ExtraInstMapDelta0, ExtraInstMapDelta, !ModuleInfo),
    goal_info_init(NonLocals, ExtraInstMapDelta,
        semidet, pure, ExtraGoalInfo),

    % Conjoin the test and the rest of the case.
    goal_to_conj_list(CaseGoal, CaseGoalConj),
    GoalList = [ExtraGoal - ExtraGoalInfo | CaseGoalConj],

    % Work out the nonlocals, instmap_delta and determinism
    % of the entire conjunction.
    CaseGoal = _ - CaseGoalInfo,
    goal_info_get_nonlocals(CaseGoalInfo, CaseNonLocals0),
    set__insert(CaseNonLocals0, Var, CaseNonLocals),
    goal_info_get_instmap_delta(CaseGoalInfo, CaseInstMapDelta),
    instmap_delta_apply_instmap_delta(ExtraInstMapDelta, CaseInstMapDelta,
        test_size, InstMapDelta),
    goal_info_get_determinism(CaseGoalInfo, CaseDetism0),
    det_conjunction_detism(semidet, CaseDetism0, Detism),
    infer_goal_info_purity(CaseGoalInfo, CasePurity),
    goal_info_init(CaseNonLocals, InstMapDelta,
        Detism, CasePurity, CombinedGoalInfo),
    Disjunct = conj(GoalList) - CombinedGoalInfo.

%-----------------------------------------------------------------------------%

goal_util__if_then_else_to_disjunction(Cond0, Then, Else, GoalInfo, Goal) :-
    goal_util__compute_disjunct_goal_info(Cond0, Then,
        GoalInfo, CondThenInfo),
    conj_list_to_goal([Cond0, Then], CondThenInfo, CondThen),

    Cond0 = _ - CondInfo0,
    goal_info_get_determinism(CondInfo0, CondDetism0),

    determinism_components(CondDetism0, CondCanFail0, CondMaxSoln0),

    % Add a commit inside the negation of the condition in the else branch
    % if the condition can succeed more than once.
    ( CondMaxSoln0 = at_most_many ->
        CondMaxSoln = at_most_one,
        determinism_components(CondDetism, CondCanFail0, CondMaxSoln),
        goal_info_set_determinism(CondDetism, CondInfo0, CondInfo),
        Cond = scope(commit(dont_force_pruning), Cond0) - CondInfo
    ;
        CondDetism = CondDetism0,
        CondInfo = CondInfo0,
        Cond = Cond0
    ),

    det_negation_det(CondDetism, MaybeNegCondDet),
    ( MaybeNegCondDet = yes(NegCondDet1) ->
        NegCondDet = NegCondDet1
    ;
        error("goal_util__if_then_else_to_disjunction: " ++
            "inappropriate determinism in a negation.")
    ),
    determinism_components(NegCondDet, _, NegCondMaxSoln),
    ( NegCondMaxSoln = at_most_zero ->
        instmap_delta_init_unreachable(NegCondDelta)
    ;
        instmap_delta_init_reachable(NegCondDelta)
    ),
    goal_info_get_nonlocals(CondInfo, CondNonLocals),
    infer_goal_info_purity(CondInfo, CondPurity),
    goal_info_init(CondNonLocals, NegCondDelta, NegCondDet, CondPurity,
        NegCondInfo),

    goal_util__compute_disjunct_goal_info(not(Cond) - NegCondInfo, Else,
        GoalInfo, NegCondElseInfo),
    conj_list_to_goal([not(Cond) - NegCondInfo, Else],
        NegCondElseInfo, NegCondElse),
    Goal = disj([CondThen, NegCondElse]).

    % Compute a hlds_goal_info for a pair of conjoined goals.
:- pred goal_util__compute_disjunct_goal_info(hlds_goal::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

goal_util__compute_disjunct_goal_info(Goal1, Goal2, GoalInfo, CombinedInfo) :-
    Goal1 = _ - GoalInfo1,
    Goal2 = _ - GoalInfo2,

    goal_info_get_nonlocals(GoalInfo1, NonLocals1),
    goal_info_get_nonlocals(GoalInfo2, NonLocals2),
    goal_info_get_nonlocals(GoalInfo, OuterNonLocals),
    set__union(NonLocals1, NonLocals2, CombinedNonLocals0),
    set__intersect(CombinedNonLocals0, OuterNonLocals, CombinedNonLocals),

    goal_info_get_instmap_delta(GoalInfo1, Delta1),
    goal_info_get_instmap_delta(GoalInfo2, Delta2),
    instmap_delta_apply_instmap_delta(Delta1, Delta2, test_size,
        CombinedDelta0),
    instmap_delta_restrict(OuterNonLocals, CombinedDelta0, CombinedDelta),

    goal_info_get_determinism(GoalInfo1, Detism1),
    goal_info_get_determinism(GoalInfo2, Detism2),
    det_conjunction_detism(Detism1, Detism2, CombinedDetism),

    goal_list_purity([Goal1, Goal2], CombinedPurity),

    goal_info_init(CombinedNonLocals, CombinedDelta,
        CombinedDetism, CombinedPurity, CombinedInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

goal_util__can_reorder_goals(ModuleInfo, VarTypes, FullyStrict,
        InstmapBeforeEarlierGoal, EarlierGoal, InstmapBeforeLaterGoal,
        LaterGoal) :-

    EarlierGoal = _ - EarlierGoalInfo,
    LaterGoal = _ - LaterGoalInfo,

        % Impure goals cannot be reordered.
    \+ goal_info_is_impure(EarlierGoalInfo),
    \+ goal_info_is_impure(LaterGoalInfo),

    goal_util__reordering_maintains_termination(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal),

    %
    % Don't reorder the goals if the later goal depends
    % on the outputs of the current goal.
    %
    \+ goal_depends_on_earlier_goal(LaterGoal, EarlierGoal,
        InstmapBeforeEarlierGoal, VarTypes, ModuleInfo),

    %
    % Don't reorder the goals if the later goal changes the
    % instantiatedness of any of the non-locals of the earlier
    % goal. This is necessary if the later goal clobbers any
    % of the non-locals of the earlier goal, and avoids rerunning
    % full mode analysis in other cases.
    %
    \+ goal_depends_on_earlier_goal(EarlierGoal, LaterGoal,
        InstmapBeforeLaterGoal, VarTypes, ModuleInfo).

goal_util__reordering_maintains_termination(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal) :-
    EarlierGoal = _ - EarlierGoalInfo,
    LaterGoal = _ - LaterGoalInfo,

    goal_info_get_determinism(EarlierGoalInfo, EarlierDetism),
    determinism_components(EarlierDetism, EarlierCanFail, _),
    goal_info_get_determinism(LaterGoalInfo, LaterDetism),
    determinism_components(LaterDetism, LaterCanFail, _),

        % If --fully-strict was specified, don't convert
        % (can_loop, can_fail) into (can_fail, can_loop).
    (
        FullyStrict = yes,
        \+ goal_cannot_loop_or_throw(EarlierGoal)
    ->
        LaterCanFail = cannot_fail
    ;
        true
    ),
        % Don't convert (can_fail, can_loop) into
        % (can_loop, can_fail), since this could worsen
        % the termination properties of the program.
    ( EarlierCanFail = can_fail ->
        goal_cannot_loop_or_throw(ModuleInfo, LaterGoal)
    ;
        true
    ).

    %
    % If the earlier goal changes the instantiatedness of a variable
    % that is used in the later goal, then the later goal depends on
    % the earlier goal.
    %
    % This code does work on the alias branch.
    %
:- pred goal_depends_on_earlier_goal(hlds_goal::in, hlds_goal::in, instmap::in,
    vartypes::in, module_info::in) is semidet.

goal_depends_on_earlier_goal(_ - LaterGoalInfo, _ - EarlierGoalInfo,
        InstMapBeforeEarlierGoal, VarTypes, ModuleInfo) :-
    goal_info_get_instmap_delta(EarlierGoalInfo, EarlierInstMapDelta),
    instmap__apply_instmap_delta(InstMapBeforeEarlierGoal,
        EarlierInstMapDelta, InstMapAfterEarlierGoal),

    instmap_changed_vars(InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal,
        VarTypes, ModuleInfo, EarlierChangedVars),

    goal_info_get_nonlocals(LaterGoalInfo, LaterGoalNonLocals),
    set__intersect(EarlierChangedVars, LaterGoalNonLocals, Intersection),
    not set__empty(Intersection).

%-----------------------------------------------------------------------------%

goal_util__generate_simple_call(ModuleName, ProcName, PredOrFunc, ModeNo,
        Detism, Args, Features, InstMap, ModuleInfo, Context, Goal) :-
    list__length(Args, Arity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, Arity, ModeNo, PredId, ProcId),

    % builtin_state only uses this to work out whether
    % this is the "recursive" clause generated for the compiler
    % for each builtin, so an invalid pred_id won't cause problems.
    InvalidPredId = invalid_pred_id,
    BuiltinState = builtin_state(ModuleInfo, InvalidPredId, PredId, ProcId),

    GoalExpr = call(PredId, ProcId, Args, BuiltinState, no,
        qualified(ModuleName, ProcName)),
    set__init(NonLocals0),
    set__insert_list(NonLocals0, Args, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    ( NumSolns = at_most_zero ->
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        instmap_delta_from_assoc_list(InstMap, InstMapDelta)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, Purity),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list__foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = GoalExpr - GoalInfo.

goal_util__generate_foreign_proc(ModuleName, ProcName, PredOrFunc, ModeNo,
        Detism, Attributes, Args, ExtraArgs, PrefixCode, Code,
        SuffixCode, Features, InstMap, ModuleInfo, Context, Goal) :-
    list__length(Args, Arity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, Arity, ModeNo, PredId, ProcId),

    AllCode = PrefixCode ++ Code ++ SuffixCode,
    GoalExpr = foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        ordinary(AllCode, no)),
    ArgVars = list__map(foreign_arg_var, Args),
    ExtraArgVars = list__map(foreign_arg_var, ExtraArgs),
    Vars = ArgVars ++ ExtraArgVars,
    set__list_to_set(Vars, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    ( NumSolns = at_most_zero ->
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        instmap_delta_from_assoc_list(InstMap, InstMapDelta)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, Purity),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list__foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = GoalExpr - GoalInfo.

goal_util__generate_cast(CastType, InArg, OutArg, Context, Goal) :-
    Ground = ground_inst,
    goal_util__generate_cast(CastType, InArg, OutArg, Ground, Ground,
        Context, Goal).

goal_util__generate_cast(CastType, InArg, OutArg, InInst, OutInst, Context,
        Goal) :-
    set__list_to_set([InArg, OutArg], NonLocals),
    instmap_delta_from_assoc_list([OutArg - OutInst], InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, det, pure, Context, GoalInfo),
    Goal = generic_call(cast(CastType), [InArg, OutArg],
        [in_mode(InInst), out_mode(OutInst)], det) - GoalInfo.

%-----------------------------------------------------------------------------%

predids_from_goals(Goals, PredIds) :-
    (
        Goals = [],
        PredIds = []
    ;
        Goals = [Goal | Rest],
        predids_from_goal(Goal, PredIds0),
        predids_from_goals(Rest, PredIds1),
        PredIds = PredIds0 ++ PredIds1
    ).

predids_from_goal(Goal, PredIds) :-
        % Explicit lambda expression needed since
        % goal_calls_pred_id has multiple modes.
    P = (pred(PredId::out) is nondet :- goal_calls_pred_id(Goal, PredId)),
    solutions(P, PredIds).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "goal_util.m".

%-----------------------------------------------------------------------------%
