%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: goal_util.m.
% Main author: conway.
%
% This module provides various utility procedures for manipulating HLDS goals,
% e.g. some functionality for renaming variables in goals.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.goal_util.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

    % Given a goal and an initial instmap, compute the final instmap that
    % results from the initial instmap after execution of the goal.
    %
:- pred update_instmap(hlds_goal::in, instmap::in, instmap::out) is det.

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
:- pred create_renaming(prog_vars::in, instmap_delta::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::out, prog_vars::out, prog_var_renaming::out) is det.

    % create_variables(OldVariables, OldVarNames, OldVarTypes,
    %   !Varset, !VarTypes, !Subn):
    %
    % create_variables takes a list of variables, a varset, and map
    % from vars to types and an initial substitution, and creates new instances
    % of each of the source variables in the substitution, adding each new
    % variable to the varset and the var types map. The name and type of each
    % new variable is found by looking up in the type map given in the 5th
    % argument - the last input. (This interface will not easily admit
    % uniqueness in the type map for this reason - such is the sacrifice
    % for generality.)
    %
:- pred create_variables(prog_vars::in, prog_varset::in, vartypes::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

    % Return all the variables in the goal.
    % Unlike quantification.goal_vars, this predicate returns
    % even the explicitly quantified variables.
    %
:- pred goal_vars(hlds_goal::in, set(prog_var)::out) is det.

    % Return all the variables in the list of goals.
    % Unlike quantification.goal_vars, this predicate returns
    % even the explicitly quantified variables.
    %
:- pred goals_goal_vars(hlds_goals::in, set(prog_var)::in,
    set(prog_var)::out) is det.

    % Return all the variables in a generic call.
    %
:- pred generic_call_vars(generic_call::in, prog_vars::out) is det.

    % Attach the given goal features to the given goal and all its subgoals.
    %
:- pred attach_features_to_all_goals(list(goal_feature)::in,
    hlds_goal::in, hlds_goal::out) is det.

    % extra_nonlocal_typeinfos(TypeInfoMap, TypeClassInfoMap,
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
:- pred extra_nonlocal_typeinfos(rtti_varmaps::in, vartypes::in,
    existq_tvars::in, set(prog_var)::in, set(prog_var)::out) is det.

:- type is_leaf
    --->    is_leaf
    ;       is_not_leaf.

    % See whether the given procedure body is that of a leaf procedure.
    %
:- func proc_body_is_leaf(hlds_goal) = is_leaf.

    % See whether the goal is a branched structure.
    %
:- pred goal_is_branched(hlds_goal_expr::in) is semidet.

    % Return an indication of the size of the goal.
    %
:- pred goal_size(hlds_goal::in, int::out) is det.

    % Return an indication of the size of the list of goals.
    %
:- pred goals_size(hlds_goals::in, int::out) is det.

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

    % goal_calls_proc_in_list(Goal, PredProcIds):
    %
    % Returns the subset of PredProcIds that are called from somewhere inside
    % Goal via plain_call.
    %
:- func goal_calls_proc_in_list(hlds_goal, list(pred_proc_id))
    = list(pred_proc_id).

    % goal_list_calls_proc_in_list(Goal, PredProcIds):
    %
    % Returns the subset of PredProcIds that are called from somewhere inside
    % Goals via plain_call.
    %
:- func goal_list_calls_proc_in_list(list(hlds_goal), list(pred_proc_id))
    = list(pred_proc_id).

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
:- pred predids_from_goals(hlds_goals::in, list(pred_id)::out) is det.

    % Returns all the procedures that are used within a goal.
    %
:- pred pred_proc_ids_from_goal(hlds_goal::in, list(pred_proc_id)::out) is det.

%-----------------------------------------------------------------------------%

    % Convert a switch back into a disjunction. This is needed
    % for the magic set transformation.
    % This aborts if any of the constructors are existentially typed.
    %
:- pred switch_to_disjunction(prog_var::in, list(case)::in,
    instmap::in, hlds_goals::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, module_info::in, module_info::out) is det.

    % Convert a case into a conjunction by adding a tag test
    % (deconstruction unification) to the case goal.
    % This aborts if the constructor is existentially typed.
    %
:- pred case_to_disjunct(prog_var::in, cons_id::in, hlds_goal::in,
    instmap::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, module_info::in, module_info::out) is det.

    % Transform an if-then-else into ( Cond, Then ; \+ Cond, Else ),
    % since magic.m and rl_gen.m don't handle if-then-elses.
    %
:- pred if_then_else_to_disjunction(hlds_goal::in, hlds_goal::in,
    hlds_goal::in, hlds_goal_info::in, hlds_goal_expr::out) is det.

%-----------------------------------------------------------------------------%

    % Flatten a list of goals of a conjunction.
    %
:- pred flatten_conj(hlds_goals::in, hlds_goals::out) is det.

    % Create a conjunction of the specified type using the specified two goals.
    % This fills in the hlds_goal_info.
    %
:- pred create_conj(hlds_goal::in, hlds_goal::in, conj_type::in, 
    hlds_goal::out) is det.

    % can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
    %   InstmapBeforeGoal1, Goal1, InstmapBeforeGoal2, Goal2).
    %
    % Goals can be reordered if
    % - the goals are independent
    % - the goals are not impure
    % - any possible change in termination behaviour is allowed according
    %   to the semantics options.
    %
    % NOTE: this version is deprecated; new code should use the following
    %       version because it supports the intermodule-analysis framework.
    %
:- pred can_reorder_goals_old(module_info::in, vartypes::in, bool::in,
    instmap::in, hlds_goal::in, instmap::in, hlds_goal::in) is semidet.

    % can_reorder_goals(VarTypes, FullyStrict, InstmapBeforeGoal1, Goal1,
    %   InstmapBeforeGoal2, Goal2, Result, !ModuleInfo, !IO).
    %
    % Result is `yes' if the goals can be reordered; no otherwise.
    %
    % Goals can be reordered if
    % - the goals are independent
    % - the goals are not impure
    % - any possible change in termination behaviour is allowed according
    %   to the semantics options.
    %
    % NOTE: new code should use this version as it supports the
    %       intermodule-analysis framework.
    %
:- pred can_reorder_goals(vartypes::in, bool::in, instmap::in,
    hlds_goal::in, instmap::in, hlds_goal::in, bool::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

    % reordering_maintains_termination_old(ModuleInfo, FullyStrict,
    %   Goal1, Goal2).
    %
    % Succeeds if any possible change in termination behaviour from reordering
    % the goals is allowed according to the semantics options.
    % The information computed by termination and exception analyses is used
    % when making this decision.
    %
    % NOTE: this version is deprecated; new code should use the following
    %       version because it supports the intermodule-analysis framework.
    %
:- pred reordering_maintains_termination_old(module_info::in, bool::in,
    hlds_goal::in, hlds_goal::in) is semidet.

    % reordering_maintains_termination(FullyStrict, Goal1, Goal2, Result,
    %   !ModuleInfo, !IO).
    %
    % Result is `yes' if any possible change in termination behaviour from
    % reordering the goals is allowed according to the semantics options.
    % The information computed by termination and exception analyses is used
    % when making this decision.
    %
    % NOTE: new code should use this version as it supports the
    %       intermodule-analysis framework.
    %
:- pred reordering_maintains_termination(bool::in, hlds_goal::in,
    hlds_goal::in, bool::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

    % generate_simple_call(ModuleName, ProcName, PredOrFunc, ModeNo, Detism,
    %   Purity, Args, Features, InstMapDelta, ModuleInfo, Context, CallGoal):
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
:- pred generate_simple_call(module_name::in, string::in,
    pred_or_func::in, mode_no::in, determinism::in, purity::in, prog_vars::in,
    list(goal_feature)::in, assoc_list(prog_var, mer_inst)::in,
    module_info::in, term.context::in, hlds_goal::out) is det.

    % generate_foreign_proc(ModuleName, ProcName, PredOrFunc, ModeNo, Detism,
    %   Purity, Attributes, Args, ExtraArgs, MaybeTraceRuntimeCond, Code,
    %   Features, InstMapDelta, ModuleInfo, Context, CallGoal):
    %
    % generate_foreign_proc is similar to generate_simple_call,
    % but also assumes that the called predicate is defined via a
    % foreign_proc, that the foreign_proc's arguments are as given in
    % Args, its attributes are Attributes, and its code is Code.
    % As well as returning a foreign_code instead of a call, effectively
    % inlining the call, generate_foreign_proc also and passes ExtraArgs
    % as well as Args.
    %
:- pred generate_foreign_proc(module_name::in, string::in, pred_or_func::in,
    mode_no::in, determinism::in, purity::in,
    pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, list(foreign_arg)::in,
    maybe(trace_expr(trace_runtime))::in, string::in,
    list(goal_feature)::in, assoc_list(prog_var, mer_inst)::in,
    module_info::in, term.context::in, hlds_goal::out) is det.

    % Generate a cast goal.  The input and output insts are just ground.
    %
:- pred generate_cast(cast_kind::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal::out) is det.

    % This version takes input and output inst arguments, which may
    % be necessary when casting, say, solver type values with inst
    % any, or casting between enumeration types and ints.
    %
:- pred generate_cast_with_insts(cast_kind::in, prog_var::in, prog_var::in,
    mer_inst::in, mer_inst::in, prog_context::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%

:- pred foreign_code_uses_variable(pragma_foreign_code_impl::in, string::in)
    is semidet.

%-----------------------------------------------------------------------------%

:- func maybe_strip_equality_pretest(hlds_goal) = hlds_goal.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_llds.
:- import_module libs.compiler_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module solutions.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module svvarset.
:- import_module varset.

%-----------------------------------------------------------------------------%

update_instmap(hlds_goal(_GoalExpr0, GoalInfo0), !InstMap) :-
    DeltaInstMap = goal_info_get_instmap_delta(GoalInfo0),
    instmap.apply_instmap_delta(!.InstMap, DeltaInstMap, !:InstMap).

%-----------------------------------------------------------------------------%

create_renaming(OrigVars, InstMapDelta, !VarSet, !VarTypes, Unifies, NewVars,
        Renaming) :-
    create_renaming_2(OrigVars, InstMapDelta, !VarSet, !VarTypes,
        [], RevUnifies, [], RevNewVars, map.init, Renaming),
    list.reverse(RevNewVars, NewVars),
    list.reverse(RevUnifies, Unifies).

:- pred create_renaming_2(prog_vars::in, instmap_delta::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goals::in, hlds_goals::out, prog_vars::in, prog_vars::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

create_renaming_2([], _, !VarSet, !VarTypes, !RevUnifies, !RevNewVars,
        !Renaming).
create_renaming_2([OrigVar | OrigVars], InstMapDelta, !VarSet, !VarTypes,
        !RevUnifies, !RevNewVars, !Renaming) :-
    svvarset.new_var(NewVar, !VarSet),
    map.lookup(!.VarTypes, OrigVar, Type),
    svmap.det_insert(NewVar, Type, !VarTypes),
    ( instmap_delta_search_var(InstMapDelta, OrigVar, DeltaInst) ->
        NewInst = DeltaInst
    ;
        unexpected(this_file, "create_renaming_2: cannot get new inst")
    ),
    Mode = ((NewInst -> NewInst) - (free -> NewInst)),
    UnifyInfo = assign(OrigVar, NewVar),
    UnifyContext = unify_context(umc_explicit, []),
    GoalExpr = unify(OrigVar, rhs_var(NewVar), Mode, UnifyInfo, UnifyContext),
    set.list_to_set([OrigVar, NewVar], NonLocals),
    instmap_delta_from_assoc_list([OrigVar - NewInst], UnifyInstMapDelta),
    goal_info_init(NonLocals, UnifyInstMapDelta, detism_det, purity_pure,
        term.context_init, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    !:RevUnifies = [Goal | !.RevUnifies],
    svmap.det_insert(OrigVar, NewVar, !Renaming),
    !:RevNewVars = [NewVar | !.RevNewVars],
    create_renaming_2(OrigVars, InstMapDelta, !VarSet, !VarTypes,
        !RevUnifies, !RevNewVars, !Renaming).

%-----------------------------------------------------------------------------%

create_variables([], _OldVarNames, _OldVarTypes, !Varset, !VarTypes, !Subn).
create_variables([V | Vs], OldVarNames, OldVarTypes, !Varset, !VarTypes,
        !Subn) :-
    ( map.contains(!.Subn, V) ->
        true
    ;
        svvarset.new_var(NV, !Varset),
        ( varset.search_name(OldVarNames, V, Name) ->
            svvarset.name_var(NV, Name, !Varset)
        ;
            true
        ),
        svmap.det_insert(V, NV, !Subn),
        ( map.search(OldVarTypes, V, VT) ->
            svmap.set(NV, VT, !VarTypes)
        ;
            true
        )
    ),
    create_variables(Vs, OldVarNames, OldVarTypes, !Varset, !VarTypes, !Subn).

%-----------------------------------------------------------------------------%

goal_vars(hlds_goal(GoalExpr, _GoalInfo), Set) :-
    goal_vars_2(GoalExpr, set.init, Set).

:- pred goal_vars_2(hlds_goal_expr::in,
    set(prog_var)::in, set(prog_var)::out) is det.

goal_vars_2(unify(Var, RHS, _, Unif, _), !Set) :-
    svset.insert(Var, !Set),
    (
        Unif = construct(_, _, _, _, CellToReuse, _, _),
        ( CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) ->
            svset.insert(Var, !Set)
        ;
            true
        )
    ;
        Unif = deconstruct(_, _, _, _, _, _)
    ;
        Unif = assign(_, _)
    ;
        Unif = simple_test(_, _)
    ;
        Unif = complicated_unify(_, _, _)
    ),
    rhs_goal_vars(RHS, !Set).

goal_vars_2(generic_call(GenericCall, ArgVars, _, _), !Set) :-
    generic_call_vars(GenericCall, Vars0),
    svset.insert_list(Vars0, !Set),
    svset.insert_list(ArgVars, !Set).

goal_vars_2(plain_call(_, _, ArgVars, _, _, _), !Set) :-
    svset.insert_list(ArgVars, !Set).

goal_vars_2(conj(_, Goals), !Set) :-
    goals_goal_vars(Goals, !Set).

goal_vars_2(disj(Goals), !Set) :-
    goals_goal_vars(Goals, !Set).

goal_vars_2(switch(Var, _Det, Cases), !Set) :-
    svset.insert(Var, !Set),
    cases_goal_vars(Cases, !Set).

goal_vars_2(scope(Reason, hlds_goal(GoalExpr, _)), !Set) :-
    (
        Reason = exist_quant(Vars),
        svset.insert_list(Vars, !Set)
    ;
        Reason = promise_purity(_, _)
    ;
        Reason = promise_solutions(Vars, _),
        svset.insert_list(Vars, !Set)
    ;
        Reason = barrier(_)
    ;
        Reason = commit(_)
    ;
        Reason = from_ground_term(Var),
        set.insert(!.Set, Var, !:Set)
    ;
        Reason = trace_goal(_, _, _, _, _)
    ),
    goal_vars_2(GoalExpr, !Set).

goal_vars_2(negation(hlds_goal(GoalExpr, _GoalInfo)), !Set) :-
    goal_vars_2(GoalExpr, !Set).

goal_vars_2(if_then_else(Vars, Cond, Then, Else), !Set) :-
    set.insert_list(!.Set, Vars, !:Set),
    goal_vars_2(Cond ^ hlds_goal_expr, !Set),
    goal_vars_2(Then ^ hlds_goal_expr, !Set),
    goal_vars_2(Else ^ hlds_goal_expr, !Set).

goal_vars_2(call_foreign_proc(_, _, _, Args, ExtraArgs, _, _), !Set) :-
    ArgVars = list.map(foreign_arg_var, Args),
    ExtraVars = list.map(foreign_arg_var, ExtraArgs),
    svset.insert_list(list.append(ArgVars, ExtraVars), !Set).

goal_vars_2(shorthand(ShorthandGoal), !Set) :-
    goal_vars_2_shorthand(ShorthandGoal, !Set).

:- pred goal_vars_2_shorthand(shorthand_goal_expr::in,
    set(prog_var)::in, set(prog_var)::out) is det.

goal_vars_2_shorthand(bi_implication(LHS, RHS), !Set) :-
    goal_vars_2(LHS ^ hlds_goal_expr, !Set),
    goal_vars_2(RHS ^ hlds_goal_expr, !Set).

goals_goal_vars([], !Set).
goals_goal_vars([Goal | Goals], !Set) :-
    goal_vars_2(Goal ^ hlds_goal_expr, !Set),
    goals_goal_vars(Goals, !Set).

:- pred cases_goal_vars(list(case)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

cases_goal_vars([], !Set).
cases_goal_vars([case(_, Goal) | Cases], !Set) :-
    goal_vars_2(Goal ^ hlds_goal_expr, !Set),
    cases_goal_vars(Cases, !Set).

:- pred rhs_goal_vars(unify_rhs::in,
    set(prog_var)::in, set(prog_var)::out) is det.

rhs_goal_vars(RHS, !Set) :-
    RHS = rhs_var(X),
    svset.insert(X, !Set).
rhs_goal_vars(RHS, !Set) :-
    RHS = rhs_functor(_Functor, _, ArgVars),
    svset.insert_list(ArgVars, !Set).
rhs_goal_vars(RHS, !Set) :-
    RHS = rhs_lambda_goal(_, _, _, NonLocals, LambdaVars, _, _, Goal),
    svset.insert_list(NonLocals, !Set),
    svset.insert_list(LambdaVars, !Set),
    goal_vars_2(Goal ^ hlds_goal_expr, !Set).

generic_call_vars(higher_order(Var, _, _, _), [Var]).
generic_call_vars(class_method(Var, _, _, _), [Var]).
generic_call_vars(event_call(_), []).
generic_call_vars(cast(_), []).

%-----------------------------------------------------------------------------%

attach_features_to_all_goals(Features, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    attach_features_goal_expr(Features, GoalExpr0, GoalExpr),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred attach_features_to_case(list(goal_feature)::in,
    case::in, case::out) is det.

attach_features_to_case(Features, case(ConsId, Goal0), case(ConsId, Goal)) :-
    attach_features_to_all_goals(Features, Goal0, Goal).

:- pred attach_features_goal_expr(list(goal_feature)::in,
    hlds_goal_expr::in, hlds_goal_expr::out) is det.

attach_features_goal_expr(Features, GoalExpr0, GoalExpr) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map(attach_features_to_all_goals(Features), Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        list.map(attach_features_to_all_goals(Features), Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(attach_features_to_case(Features), Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        attach_features_to_all_goals(Features, Cond0, Cond),
        attach_features_to_all_goals(Features, Then0, Then),
        attach_features_to_all_goals(Features, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(Goal0),
        attach_features_to_all_goals(Features, Goal0, Goal),
        GoalExpr = negation(Goal)
    ;
        GoalExpr0 = scope(Reason, Goal0),
        attach_features_to_all_goals(Features, Goal0, Goal),
        GoalExpr = scope(Reason, Goal)
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        GoalExpr = GoalExpr0
    ).

%-----------------------------------------------------------------------------%

extra_nonlocal_typeinfos(RttiVarMaps, VarTypes, ExistQVars,
        NonLocals, NonLocalTypeInfos) :-

        % Find all non-local type vars.  That is, type vars that are
        % existentially quantified or type vars that appear in the
        % type of a non-local prog_var.
        %
    set.to_sorted_list(NonLocals, NonLocalsList),
    map.apply_to_list(NonLocalsList, VarTypes, NonLocalsTypes),
    type_vars_list(NonLocalsTypes, NonLocalTypeVarsList0),
    list.append(ExistQVars, NonLocalTypeVarsList0, NonLocalTypeVarsList),
    set.list_to_set(NonLocalTypeVarsList, NonLocalTypeVars),

        % Find all the type_infos that are non-local, that is,
        % type_infos for type vars that are non-local in the above
        % sense.
        %
    TypeVarToProgVar = (func(TypeVar) = ProgVar :-
        rtti_lookup_type_info_locn(RttiVarMaps, TypeVar, Locn),
        type_info_locn_var(Locn, ProgVar)
    ),
    NonLocalTypeInfoVars = set.map(TypeVarToProgVar, NonLocalTypeVars),

        % Find all the typeclass_infos that are non-local.  These
        % include all typeclass_infos that constrain a type variable
        % that is non-local in the above sense.
        %
    solutions.solutions_set(
        (pred(Var::out) is nondet :-
            % Search through all arguments of all constraints
            % that the goal could have used.
            rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
            list.member(Constraint, Constraints),
            Constraint = constraint(_Name, ArgTypes),
            type_list_contains_var(ArgTypes, TypeVar),
            set.member(TypeVar, NonLocalTypeVars),

            % We found a constraint that is non-local. Include the variable
            % holding its typeclass_info.
            rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var)
        ), NonLocalTypeClassInfoVars),
    NonLocalTypeInfos = set.union(NonLocalTypeInfoVars,
        NonLocalTypeClassInfoVars).

%-----------------------------------------------------------------------------%

proc_body_is_leaf(hlds_goal(GoalExpr, _)) = IsLeaf :-
    (
        GoalExpr = unify(_, _, _, UnifyKind, _),
        (
            UnifyKind = complicated_unify(_, _, _),
            IsLeaf = is_not_leaf
        ;
            ( UnifyKind = construct(_, _, _, _, _, _, _)
            ; UnifyKind = deconstruct(_, _, _, _, _, _)
            ; UnifyKind = assign(_, _)
            ; UnifyKind = simple_test(_, _)
            ),
            IsLeaf = is_leaf
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        IsLeaf = is_not_leaf
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        IsLeaf = proc_body_is_leaf_goals(Goals)
    ;
        ( GoalExpr = negation(Goal)
        ; GoalExpr = scope(_, Goal)
        ),
        IsLeaf = proc_body_is_leaf(Goal)
    ;
        GoalExpr = switch(_, _, Cases),
        IsLeaf = proc_body_is_leaf_cases(Cases)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        (
            proc_body_is_leaf(Cond) = is_leaf,
            proc_body_is_leaf(Then) = is_leaf,
            proc_body_is_leaf(Else) = is_leaf
        ->
            IsLeaf = is_leaf
        ;
            IsLeaf = is_not_leaf
        )
    ;
        GoalExpr = shorthand(ShortHand),
        ShortHand = bi_implication(GoalA, GoalB),
        (
            proc_body_is_leaf(GoalA) = is_leaf,
            proc_body_is_leaf(GoalB) = is_leaf
        ->
            IsLeaf = is_leaf
        ;
            IsLeaf = is_not_leaf
        )
    ).

:- func proc_body_is_leaf_goals(list(hlds_goal)) = is_leaf.

proc_body_is_leaf_goals([]) = is_leaf.
proc_body_is_leaf_goals([Goal | Goals]) = IsLeaf :-
    (
        proc_body_is_leaf(Goal) = is_leaf,
        proc_body_is_leaf_goals(Goals) = is_leaf
    ->
        IsLeaf = is_leaf
    ;
        IsLeaf = is_not_leaf
    ).

:- func proc_body_is_leaf_cases(list(case)) = is_leaf.

proc_body_is_leaf_cases([]) = is_leaf.
proc_body_is_leaf_cases([Case | Cases]) = IsLeaf :-
    Case = case(_, Goal),
    (
        proc_body_is_leaf(Goal) = is_leaf,
        proc_body_is_leaf_cases(Cases) = is_leaf
    ->
        IsLeaf = is_leaf
    ;
        IsLeaf = is_not_leaf
    ).

%-----------------------------------------------------------------------------%

goal_is_branched(if_then_else(_, _, _, _)).
goal_is_branched(switch(_, _, _)).
goal_is_branched(disj(_)).

%-----------------------------------------------------------------------------%

goal_size(hlds_goal(GoalExpr, _), Size) :-
    goal_expr_size(GoalExpr, Size).

goals_size([], 0).
goals_size([Goal | Goals], Size) :-
    goal_size(Goal, Size1),
    goals_size(Goals, Size2),
    Size = Size1 + Size2.

clause_list_size(Clauses, GoalSize) :-
    list.foldl(clause_size_increment, Clauses, 0, GoalSize0),
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

goal_expr_size(conj(ConjType, Goals), Size) :-
    goals_size(Goals, InnerSize),
    (
        ConjType = plain_conj,
        Size = InnerSize
    ;
        ConjType = parallel_conj,
        Size = InnerSize + 1
    ).
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
goal_expr_size(negation(Goal), Size) :-
    goal_size(Goal, Size1),
    Size = Size1 + 1.
goal_expr_size(scope(_, Goal), Size) :-
    goal_size(Goal, Size1),
    Size = Size1 + 1.
goal_expr_size(plain_call(_, _, _, _, _, _), 1).
goal_expr_size(generic_call(_, _, _, _), 1).
goal_expr_size(unify(_, _, _, _, _), 1).
goal_expr_size(call_foreign_proc(_, _, _, _, _, _, _), 1).
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

goal_calls(hlds_goal(GoalExpr, _), PredProcId) :-
    goal_expr_calls(GoalExpr, PredProcId).

:- pred goals_calls(hlds_goals, pred_proc_id).
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

goal_expr_calls(conj(_ConjType, Goals), PredProcId) :-
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
goal_expr_calls(negation(Goal), PredProcId) :-
    goal_calls(Goal, PredProcId).
goal_expr_calls(scope(_, Goal), PredProcId) :-
    goal_calls(Goal, PredProcId).
goal_expr_calls(plain_call(PredId, ProcId, _, _, _, _), proc(PredId, ProcId)).

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls_pred_id as
%   goal_calls_pred_id(Goal, PredId) :-
%       goal_contains_subgoal(Goal, call(PredId, _, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls_pred_id(hlds_goal(GoalExpr, _), PredId) :-
    goal_expr_calls_pred_id(GoalExpr, PredId).

:- pred goals_calls_pred_id(hlds_goals, pred_id).
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

goal_expr_calls_pred_id(conj(_ConjType, Goals), PredId) :-
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
goal_expr_calls_pred_id(negation(Goal), PredId) :-
    goal_calls_pred_id(Goal, PredId).
goal_expr_calls_pred_id(scope(_, Goal), PredId) :-
    goal_calls_pred_id(Goal, PredId).
goal_expr_calls_pred_id(plain_call(PredId, _, _, _, _, _), PredId).

%-----------------------------------------------------------------------------%

goal_calls_proc_in_list(Goal, PredProcIds) = CalledPredProcIds :-
    goal_calls_proc_in_list_2(Goal, PredProcIds, set.init, CalledSet),
    set.to_sorted_list(CalledSet, CalledPredProcIds).

goal_list_calls_proc_in_list(Goals, PredProcIds) = CalledPredProcIds :-
    goal_list_calls_proc_in_list_2(Goals, PredProcIds, set.init, CalledSet),
    set.to_sorted_list(CalledSet, CalledPredProcIds).

:- pred goal_calls_proc_in_list_2(hlds_goal::in, list(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

goal_calls_proc_in_list_2(hlds_goal(GoalExpr, _GoalInfo), PredProcIds,
        !CalledSet) :-
    (
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( list.member(proc(PredId, ProcId), PredProcIds) ->
            svset.insert(proc(PredId, ProcId), !CalledSet)
        ;
            true
        )
    ;
        GoalExpr = generic_call(_, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = conj(_, Goals),
        goal_list_calls_proc_in_list_2(Goals, PredProcIds, !CalledSet)
    ;
        GoalExpr = disj(Goals),
        goal_list_calls_proc_in_list_2(Goals, PredProcIds, !CalledSet)
    ;
        GoalExpr = switch(_, _, Cases),
        case_list_calls_proc_in_list(Cases, PredProcIds, !CalledSet)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_calls_proc_in_list_2(Cond, PredProcIds, !CalledSet),
        goal_calls_proc_in_list_2(Then, PredProcIds, !CalledSet),
        goal_calls_proc_in_list_2(Else, PredProcIds, !CalledSet)
    ;
        GoalExpr = negation(Goal),
        goal_calls_proc_in_list_2(Goal, PredProcIds, !CalledSet)
    ;
        GoalExpr = scope(_, Goal),
        goal_calls_proc_in_list_2(Goal, PredProcIds, !CalledSet)
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file, "goal__calls_proc_in_list_2: shorthand")
    ).

:- pred goal_list_calls_proc_in_list_2(list(hlds_goal)::in,
    list(pred_proc_id)::in, set(pred_proc_id)::in, set(pred_proc_id)::out)
    is det.

goal_list_calls_proc_in_list_2([], _, !CalledSet).
goal_list_calls_proc_in_list_2([Goal | Goals], PredProcIds, !CalledSet) :-
    goal_calls_proc_in_list_2(Goal, PredProcIds, !CalledSet),
    goal_list_calls_proc_in_list_2(Goals, PredProcIds, !CalledSet).

:- pred case_list_calls_proc_in_list(list(case)::in, list(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

case_list_calls_proc_in_list([], _, !CalledSet).
case_list_calls_proc_in_list([Case | Cases], PredProcIds, !CalledSet) :-
    Case = case(_, Goal),
    goal_calls_proc_in_list_2(Goal, PredProcIds, !CalledSet),
    case_list_calls_proc_in_list(Cases, PredProcIds, !CalledSet).

%-----------------------------------------------------------------------------%

goal_contains_reconstruction(_) :-
    % This will only succeed on the alias branch with structure reuse.
    semidet_fail.
    % goal_expr_contains_reconstruction(GoalExpr).

:- pred goal_expr_contains_reconstruction(hlds_goal_expr::in) is semidet.

goal_expr_contains_reconstruction(conj(_ConjType, Goals)) :-
    goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(disj(Goals)) :-
    goals_contain_reconstruction(Goals).
goal_expr_contains_reconstruction(switch(_, _, Cases)) :-
    list.member(Case, Cases),
    Case = case(_, Goal),
    goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(if_then_else(_, Cond, Then, Else)) :-
    goals_contain_reconstruction([Cond, Then, Else]).
goal_expr_contains_reconstruction(negation(Goal)) :-
    goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(scope(_, Goal)) :-
    goal_contains_reconstruction(Goal).
goal_expr_contains_reconstruction(unify(_, _, _, Unify, _)) :-
    Unify = construct(_, _, _, _, HowToConstruct, _, _),
    HowToConstruct = reuse_cell(_).

:- pred goals_contain_reconstruction(hlds_goals::in) is semidet.

goals_contain_reconstruction(Goals) :-
    list.member(Goal, Goals),
    goal_contains_reconstruction(Goal).

%-----------------------------------------------------------------------------%

goal_contains_goal(Goal, Goal).
goal_contains_goal(hlds_goal(GoalExpr, _), SubGoal) :-
    direct_subgoal(GoalExpr, DirectSubGoal),
    goal_contains_goal(DirectSubGoal, SubGoal).

direct_subgoal(scope(_, Goal), Goal).
direct_subgoal(negation(Goal), Goal).
direct_subgoal(if_then_else(_, If, Then, Else), Goal) :-
    ( Goal = If
    ; Goal = Then
    ; Goal = Else
    ).
direct_subgoal(conj(_ConjType, ConjList), Goal) :-
    list.member(Goal, ConjList).
direct_subgoal(disj(DisjList), Goal) :-
    list.member(Goal, DisjList).
direct_subgoal(switch(_, _, CaseList), Goal) :-
    list.member(Case, CaseList),
    Case = case(_, Goal).

%-----------------------------------------------------------------------------%

switch_to_disjunction(_, [], _, [], !VarSet, !VarTypes, !ModuleInfo).
switch_to_disjunction(Var, [case(ConsId, Goal0) | Cases], InstMap,
        [Goal | Goals], !VarSet, !VarTypes, !ModuleInfo) :-
    case_to_disjunct(Var, ConsId, Goal0, InstMap, Goal, !VarSet, !VarTypes,
        !ModuleInfo),
    switch_to_disjunction(Var, Cases, InstMap, Goals, !VarSet, !VarTypes,
        !ModuleInfo).

case_to_disjunct(Var, ConsId, CaseGoal, InstMap, Disjunct, !VarSet, !VarTypes,
        !ModuleInfo) :-
    ConsArity = cons_id_arity(ConsId),
    svvarset.new_vars(ConsArity, ArgVars, !VarSet),
    map.lookup(!.VarTypes, Var, VarType),
    type_util.get_cons_id_arg_types(!.ModuleInfo, VarType, ConsId, ArgTypes),
    svmap.det_insert_from_corresponding_lists(ArgVars, ArgTypes, !VarTypes),
    instmap.lookup_var(InstMap, Var, Inst0),
    (
        inst_expand(!.ModuleInfo, Inst0, Inst1),
        get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
    ->
        ArgInsts = ArgInsts1
    ;
        unexpected(this_file, "case_to_disjunct - get_arg_insts failed")
    ),
    InstToUniMode = (pred(ArgInst::in, ArgUniMode::out) is det :-
        ArgUniMode = ((ArgInst - free) -> (ArgInst - ArgInst))
    ),
    list.map(InstToUniMode, ArgInsts, UniModes),
    UniMode = (Inst0 -> Inst0) - (Inst0 -> Inst0),
    UnifyContext = unify_context(umc_explicit, []),
    Unification = deconstruct(Var, ConsId, ArgVars, UniModes, can_fail,
        cannot_cgc),
    ExtraGoalExpr = unify(Var, rhs_functor(ConsId, no, ArgVars), UniMode,
        Unification, UnifyContext),
    set.singleton_set(NonLocals, Var),
    instmap_delta_init_reachable(ExtraInstMapDelta0),
    instmap_delta_bind_var_to_functor(Var, VarType, ConsId, InstMap,
        ExtraInstMapDelta0, ExtraInstMapDelta, !ModuleInfo),
    goal_info_init(NonLocals, ExtraInstMapDelta,
        detism_semi, purity_pure, ExtraGoalInfo),

    % Conjoin the test and the rest of the case.
    goal_to_conj_list(CaseGoal, CaseGoalConj),
    GoalList = [hlds_goal(ExtraGoalExpr, ExtraGoalInfo) | CaseGoalConj],

    % Work out the nonlocals, instmap_delta and determinism
    % of the entire conjunction.
    CaseGoal = hlds_goal(_, CaseGoalInfo),
    CaseNonLocals0 = goal_info_get_nonlocals(CaseGoalInfo),
    set.insert(CaseNonLocals0, Var, CaseNonLocals),
    CaseInstMapDelta = goal_info_get_instmap_delta(CaseGoalInfo),
    instmap_delta_apply_instmap_delta(ExtraInstMapDelta, CaseInstMapDelta,
        test_size, InstMapDelta),
    CaseDetism0 = goal_info_get_determinism(CaseGoalInfo),
    det_conjunction_detism(detism_semi, CaseDetism0, Detism),
    CasePurity = goal_info_get_purity(CaseGoalInfo),
    goal_info_init(CaseNonLocals, InstMapDelta,
        Detism, CasePurity, CombinedGoalInfo),
    Disjunct = hlds_goal(conj(plain_conj, GoalList), CombinedGoalInfo).

%-----------------------------------------------------------------------------%

if_then_else_to_disjunction(Cond0, Then, Else, GoalInfo, Goal) :-
    compute_disjunct_goal_info(Cond0, Then, GoalInfo, CondThenInfo),
    conj_list_to_goal([Cond0, Then], CondThenInfo, CondThen),

    Cond0 = hlds_goal(_, CondInfo0),
    CondDetism0 = goal_info_get_determinism(CondInfo0),

    determinism_components(CondDetism0, CondCanFail0, CondMaxSoln0),

    % Add a commit inside the negation of the condition in the else branch
    % if the condition can succeed more than once.
    (
        CondMaxSoln0 = at_most_many,
        CondMaxSoln = at_most_one,
        determinism_components(CondDetism, CondCanFail0, CondMaxSoln),
        goal_info_set_determinism(CondDetism, CondInfo0, CondInfo),
        Cond = hlds_goal(scope(commit(dont_force_pruning), Cond0), CondInfo)
    ;
        ( CondMaxSoln0 = at_most_zero
        ; CondMaxSoln0 = at_most_one
        ; CondMaxSoln0 = at_most_many_cc
        ),
        CondDetism = CondDetism0,
        CondInfo = CondInfo0,
        Cond = Cond0
    ),

    det_negation_det(CondDetism, MaybeNegCondDet),
    (
        MaybeNegCondDet = yes(NegCondDet1),
        NegCondDet = NegCondDet1
    ;
        MaybeNegCondDet = no,
        unexpected(this_file,
            "if_then_else_to_disjunction: "
                ++ "inappropriate determinism in a negation.")
    ),
    determinism_components(NegCondDet, _, NegCondMaxSoln),
    (
        NegCondMaxSoln = at_most_zero,
        instmap_delta_init_unreachable(NegCondDelta)
    ;
        ( NegCondMaxSoln = at_most_one
        ; NegCondMaxSoln = at_most_many
        ; NegCondMaxSoln = at_most_many_cc
        ),
        instmap_delta_init_reachable(NegCondDelta)
    ),
    CondNonLocals = goal_info_get_nonlocals(CondInfo),
    CondPurity = goal_info_get_purity(CondInfo),
    goal_info_init(CondNonLocals, NegCondDelta, NegCondDet, CondPurity,
        NegCondInfo),

    compute_disjunct_goal_info(hlds_goal(negation(Cond), NegCondInfo), Else,
        GoalInfo, NegCondElseInfo),
    conj_list_to_goal([hlds_goal(negation(Cond), NegCondInfo), Else],
        NegCondElseInfo, NegCondElse),
    Goal = disj([CondThen, NegCondElse]).

    % Compute a hlds_goal_info for a pair of conjoined goals.
    %
:- pred compute_disjunct_goal_info(hlds_goal::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

compute_disjunct_goal_info(Goal1, Goal2, GoalInfo, CombinedInfo) :-
    Goal1 = hlds_goal(_, GoalInfo1),
    Goal2 = hlds_goal(_, GoalInfo2),

    NonLocals1 = goal_info_get_nonlocals(GoalInfo1),
    NonLocals2 = goal_info_get_nonlocals(GoalInfo2),
    OuterNonLocals = goal_info_get_nonlocals(GoalInfo),
    set.union(NonLocals1, NonLocals2, CombinedNonLocals0),
    set.intersect(CombinedNonLocals0, OuterNonLocals, CombinedNonLocals),

    Delta1 = goal_info_get_instmap_delta(GoalInfo1),
    Delta2 = goal_info_get_instmap_delta(GoalInfo2),
    instmap_delta_apply_instmap_delta(Delta1, Delta2, test_size,
        CombinedDelta0),
    instmap_delta_restrict(OuterNonLocals, CombinedDelta0, CombinedDelta),

    Detism1 = goal_info_get_determinism(GoalInfo1),
    Detism2 = goal_info_get_determinism(GoalInfo2),
    det_conjunction_detism(Detism1, Detism2, CombinedDetism),

    goal_list_purity([Goal1, Goal2], CombinedPurity),

    goal_info_init(CombinedNonLocals, CombinedDelta,
        CombinedDetism, CombinedPurity, CombinedInfo).

%-----------------------------------------------------------------------------%

flatten_conj([], []).
flatten_conj([Goal | Goals0], Goals) :-
    flatten_conj(Goals0, Goals1),
    ( Goal ^ hlds_goal_expr = conj(plain_conj, SubGoals) ->
        list.append(SubGoals, Goals1, Goals)
    ;
        Goals = [Goal | Goals1]
    ).

%-----------------------------------------------------------------------------%

create_conj(GoalA, GoalB, Type, ConjGoal) :-
    GoalsInConj = [ GoalA, GoalB ],
    ConjGoalExpr = conj(Type, GoalsInConj),
    goal_list_nonlocals(GoalsInConj, NonLocals),
    goal_list_instmap_delta(GoalsInConj, InstMapDelta),
    goal_list_determinism(GoalsInConj, Detism),
    goal_list_purity(GoalsInConj, Purity),
    GoalAInfo = GoalA ^ hlds_goal_info,
    Context = goal_info_get_context(GoalAInfo),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context, 
        ConjGoalInfo),
    ConjGoal = hlds_goal(ConjGoalExpr, ConjGoalInfo).

%-----------------------------------------------------------------------------%

can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
        InstmapBeforeEarlierGoal, EarlierGoal,
        InstmapBeforeLaterGoal, LaterGoal) :-
    % The logic here is mostly duplicated in can_reorder_goals below
    % and in pd_can_reorder_goals in pd_util.m.

    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    EarlierPurity \= purity_impure,
    LaterPurity \= purity_impure,
    EarlierTrace = contains_no_trace_goal,
    LaterTrace = contains_no_trace_goal,

    reordering_maintains_termination_old(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal),

    % Don't reorder the goals if the later goal depends on the outputs
    % of the current goal.
    \+ goal_depends_on_earlier_goal(LaterGoal, EarlierGoal,
        InstmapBeforeEarlierGoal, VarTypes, ModuleInfo),

    % Don't reorder the goals if the later goal changes the instantiatedness
    % of any of the non-locals of the earlier goal. This is necessary if the
    % later goal clobbers any of the non-locals of the earlier goal, and
    % avoids rerunning full mode analysis in other cases.
    \+ goal_depends_on_earlier_goal(EarlierGoal, LaterGoal,
        InstmapBeforeLaterGoal, VarTypes, ModuleInfo).

can_reorder_goals(VarTypes, FullyStrict, InstmapBeforeEarlierGoal,
        EarlierGoal, InstmapBeforeLaterGoal, LaterGoal, CanReorder,
        !ModuleInfo, !IO) :-
    % The logic here is mostly duplicated in can_reorder_goals_old above
    % and in pd_can_reorder_goals in pd_util.m.

    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    (
        ( EarlierPurity = purity_impure
        ; LaterPurity = purity_impure
        ; EarlierTrace = contains_trace_goal
        ; LaterTrace = contains_trace_goal
        )
    ->
        CanReorder = no
    ;
        reordering_maintains_termination(FullyStrict,
            EarlierGoal, LaterGoal, MaintainsTermination, !ModuleInfo, !IO),
        (
            MaintainsTermination = no,
            CanReorder = no
        ;
            MaintainsTermination = yes,
            (
                % Don't reorder the goals if the later goal depends on the
                % outputs of the current goal.
                %
                goal_depends_on_earlier_goal(LaterGoal, EarlierGoal,
                    InstmapBeforeEarlierGoal, VarTypes, !.ModuleInfo)
            ->
                CanReorder = no
            ;
                % Don't reorder the goals if the later goal changes the
                % instantiatedness of any of the non-locals of the earlier
                % goal. This is necessary if the later goal clobbers any of
                % the non-locals of the earlier goal, and avoids rerunning
                % full mode analysis in other cases.
                %
                goal_depends_on_earlier_goal(EarlierGoal, LaterGoal,
                    InstmapBeforeLaterGoal, VarTypes, !.ModuleInfo)
            ->
                CanReorder = no
            ;
                CanReorder = yes
            )
        )
    ).

reordering_maintains_termination_old(ModuleInfo, FullyStrict,
        EarlierGoal, LaterGoal) :-
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    EarlierDetism = goal_info_get_determinism(EarlierGoalInfo),
    determinism_components(EarlierDetism, EarlierCanFail, _),
    LaterDetism = goal_info_get_determinism(LaterGoalInfo),
    determinism_components(LaterDetism, LaterCanFail, _),

    % If --fully-strict was specified, don't convert (can_loop, can_fail)
    % into (can_fail, can_loop).
    (
        FullyStrict = yes,
        \+ goal_cannot_loop_or_throw(EarlierGoal)
    ->
        LaterCanFail = cannot_fail
    ;
        true
    ),
    % Don't convert (can_fail, can_loop) into (can_loop, can_fail), since
    % this could worsen the termination properties of the program.
    (
        EarlierCanFail = can_fail,
        goal_cannot_loop_or_throw(ModuleInfo, LaterGoal)
    ;
        EarlierCanFail = cannot_fail
    ).

reordering_maintains_termination(FullyStrict, EarlierGoal, LaterGoal,
        MaintainsTermination, !ModuleInfo, !IO) :-
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    EarlierDetism = goal_info_get_determinism(EarlierGoalInfo),
    determinism_components(EarlierDetism, EarlierCanFail, _),
    LaterDetism = goal_info_get_determinism(LaterGoalInfo),
    determinism_components(LaterDetism, LaterCanFail, _),

    % If --fully-strict was specified, don't convert (can_loop, can_fail) into
    % (can_fail, can_loop).
    goal_can_loop_or_throw(EarlierGoal, EarlierCanLoopOrThrow, !ModuleInfo,
        !IO),
    (
        FullyStrict = yes,
        EarlierCanLoopOrThrow = can_loop_or_throw,
        LaterCanFail = can_fail
    ->
        MaintainsTermination = no
    ;
        % Don't convert (can_fail, can_loop) into (can_loop, can_fail), since
        % this could worsen the termination properties of the program.
        %
        goal_can_loop_or_throw(LaterGoal, LaterCanLoopOrThrow,
            !ModuleInfo, !IO),
        (
            EarlierCanFail = can_fail,
            LaterCanLoopOrThrow = can_loop_or_throw
        ->
            MaintainsTermination = no
        ;
            MaintainsTermination = yes
        )
    ).

    % If the earlier goal changes the instantiatedness of a variable
    % that is used in the later goal, then the later goal depends on
    % the earlier goal.
    %
    % This code does work on the alias branch.
    %
:- pred goal_depends_on_earlier_goal(hlds_goal::in, hlds_goal::in, instmap::in,
    vartypes::in, module_info::in) is semidet.

goal_depends_on_earlier_goal(LaterGoal, EarlierGoal, InstMapBeforeEarlierGoal,
        VarTypes, ModuleInfo) :-
    LaterGoal = hlds_goal(_, LaterGoalInfo),
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    EarlierInstMapDelta = goal_info_get_instmap_delta(EarlierGoalInfo),
    instmap.apply_instmap_delta(InstMapBeforeEarlierGoal,
        EarlierInstMapDelta, InstMapAfterEarlierGoal),

    instmap_changed_vars(InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal,
        VarTypes, ModuleInfo, EarlierChangedVars),

    LaterGoalNonLocals = goal_info_get_nonlocals(LaterGoalInfo),
    set.intersect(EarlierChangedVars, LaterGoalNonLocals, Intersection),
    not set.empty(Intersection).

%-----------------------------------------------------------------------------%

generate_simple_call(ModuleName, ProcName, PredOrFunc, ModeNo, Detism, Purity,
        Args, Features, InstMap, ModuleInfo, Context, Goal) :-
    list.length(Args, Arity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, Arity, ModeNo, PredId, ProcId),

    % builtin_state only uses this to work out whether
    % this is the "recursive" clause generated for the compiler
    % for each builtin, so an invalid pred_id won't cause problems.
    InvalidPredId = invalid_pred_id,
    BuiltinState = builtin_state(ModuleInfo, InvalidPredId, PredId, ProcId),

    GoalExpr = plain_call(PredId, ProcId, Args, BuiltinState, no,
        qualified(ModuleName, ProcName)),
    set.init(NonLocals0),
    set.insert_list(NonLocals0, Args, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    (
        NumSolns = at_most_zero,
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        ( NumSolns = at_most_one
        ; NumSolns = at_most_many
        ; NumSolns = at_most_many_cc
        ),
        instmap_delta_from_assoc_list(InstMap, InstMapDelta)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, PredPurity),
    expect(unify(Purity, PredPurity), this_file,
        "generate_simple_call: purity disagreement"),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_foreign_proc(ModuleName, ProcName, PredOrFunc, ModeNo, Detism,
        Purity, Attributes, Args, ExtraArgs, MaybeTraceRuntimeCond, Code,
        Features, InstMap, ModuleInfo, Context, Goal) :-
    list.length(Args, Arity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, Arity, ModeNo, PredId, ProcId),

    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        MaybeTraceRuntimeCond, fc_impl_ordinary(Code, no)),
    ArgVars = list.map(foreign_arg_var, Args),
    ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
    Vars = ArgVars ++ ExtraArgVars,
    set.list_to_set(Vars, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    (
        NumSolns = at_most_zero,
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        ( NumSolns = at_most_one
        ; NumSolns = at_most_many
        ; NumSolns = at_most_many_cc
        ),
        instmap_delta_from_assoc_list(InstMap, InstMapDelta)
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, PredPurity),
    expect(unify(Purity, PredPurity), this_file,
        "generate_simple_call: purity disagreement"),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_cast(CastType, InArg, OutArg, Context, Goal) :-
    Ground = ground_inst,
    generate_cast_with_insts(CastType, InArg, OutArg, Ground, Ground, Context,
        Goal).

generate_cast_with_insts(CastType, InArg, OutArg, InInst, OutInst, Context,
        Goal) :-
    set.list_to_set([InArg, OutArg], NonLocals),
    instmap_delta_from_assoc_list([OutArg - OutInst], InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    GoalExpr = generic_call(cast(CastType), [InArg, OutArg],
        [in_mode(InInst), out_mode(OutInst)], detism_det),
    Goal = hlds_goal(GoalExpr, GoalInfo).

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
    % Explicit lambda expression needed since goal_calls_pred_id
    % has multiple modes.
    P = (pred(PredId::out) is nondet :- goal_calls_pred_id(Goal, PredId)),
    solutions.solutions(P, PredIds).

pred_proc_ids_from_goal(Goal, PredProcIds) :-
    P = (pred(PredProcId::out) is nondet :- goal_calls(Goal, PredProcId)),
    solutions.solutions(P, PredProcIds).

%-----------------------------------------------------------------------------%

foreign_code_uses_variable(Impl, VarName) :-
    (
        Impl = fc_impl_ordinary(ForeignBody, _),
        string.sub_string_search(ForeignBody, VarName, _)
    ;
        Impl = fc_impl_model_non(FB1, _, FB2, _, FB3, _, _, FB4, _),
        ( string.sub_string_search(FB1, VarName, _)
        ; string.sub_string_search(FB2, VarName, _)
        ; string.sub_string_search(FB3, VarName, _)
        ; string.sub_string_search(FB4, VarName, _)
        )
    ).

%-----------------------------------------------------------------------------%

maybe_strip_equality_pretest(Goal0) = Goal :-
    % The if_then_else constructed by unify_proc is sometimes wrapped up
    % in conjunctions.
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        Goals = list.map(maybe_strip_equality_pretest, Goals0),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(SubGoals0),
        SubGoals = list.map(maybe_strip_equality_pretest, SubGoals0),
        GoalExpr = disj(SubGoals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        Cases = list.map(maybe_strip_equality_pretest_case, Cases0),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        SubGoal = maybe_strip_equality_pretest(SubGoal0),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        SubGoal = maybe_strip_equality_pretest(SubGoal0),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        ( goal_info_has_feature(GoalInfo0, feature_pretest_equality) ->
            Goal = Else0
        ;
            Cond = maybe_strip_equality_pretest(Cond0),
            Then = maybe_strip_equality_pretest(Then0),
            Else = maybe_strip_equality_pretest(Else0),
            GoalExpr = if_then_else(Vars, Cond, Then, Else),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "maybe_strip_equality_pretest: shorthand")
    ).

:- func maybe_strip_equality_pretest_case(case) = case.

maybe_strip_equality_pretest_case(Case0) = Case :-
    Case0 = case(ConsId, Goal0),
    Goal = maybe_strip_equality_pretest(Goal0),
    Case = case(ConsId, Goal).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "goal_util.m".

%-----------------------------------------------------------------------------%
:- end_module goal_util.
%-----------------------------------------------------------------------------%
