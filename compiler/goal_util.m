%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: goal_util.m.
% Main author: conway.
%
% This module provides various utility procedures for manipulating HLDS goals,
% e.g. attaching features to goals.
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
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module term_context.

%-----------------------------------------------------------------------------%

    % Given a goal (or its goal_info) and an initial instmap,
    % compute the final instmap that results from the initial instmap
    % after execution of the goal.
    %
:- pred update_instmap(hlds_goal::in, instmap::in, instmap::out) is det.
:- pred update_instmap_goal_info(hlds_goal_info::in,
    instmap::in, instmap::out) is det.

    % create_renaming(OutputVars, InstMapDelta, !VarTable,
    %   UnifyGoals, NewVars, Renaming):
    %
    % This predicate is intended for use in program transformations
    % that need to wrap up semidet goals, replacing Goal with
    % ( if Goal' then UnifyGoals, ... else ...), where Goal' has its output
    % variables (OutputVars) replaced with new variables (NewVars),
    % with the mapping from OutputVars to NewVars being Renaming.
    % VarTable is updated for the new variables. The final insts of NewVar
    % are taken from the insts of the corresponding OutputVar in InstMapDelta
    % (the initial inst is free).
    %
:- pred create_renaming(list(prog_var)::in, instmap_delta::in,
    var_table::in, var_table::out,
    list(hlds_goal)::out, list(prog_var)::out, prog_var_renaming::out) is det.

    % clone_variable(OldVar, OldVarTable, !VarTable, !Renaming, CloneVar):
    %
    % clone_variable typically takes an old variable OldVar, and creates a
    % clone of it, adding the clone variable to !VarTable, and adding
    % a mapping from the old variable to its clone to !Renaming.
    % The name and type of the clone are taken from OldVarTable.
    % However, if OldVar already has a clone, as shown by it already being a
    % key in !.Renaming, clone_variable does nothing. Either way, the identity
    % of the clone variable is returned in CloneVar.
    %
    % (This interface will not easily admit uniqueness in the var_table
    % arguments; such is the sacrifice for generality.)
    %
:- pred clone_variable(prog_var::in, var_table::in,
    var_table::in, var_table::out,
    prog_var_renaming::in, prog_var_renaming::out, prog_var::out) is det.

    % clone_variable_vs(OldVar, OldVarSet, OldVarTypes,
    %   !VarSet, !VarTypes, !Renaming, CloneVar):
    %
    % A version of clone_variable using varsets and vartypes.
    %
:- pred clone_variable_vs(prog_var::in, prog_varset::in, vartypes::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var_renaming::in, prog_var_renaming::out, prog_var::out) is det.

    % clone_variables(OldVars, OldVarTable, !VarTable, !Renaming):
    %
    % Invoke clone_variable on each variable in OldVars.
    %
    % The caller can find the identity of the clone of each variable in OldVars
    % by looking it up in !:Renaming.
    %
    %
:- pred clone_variables(list(prog_var)::in,
    var_table::in, var_table::in, var_table::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

    % clone_variables_vs(OldVars, OldVarSet, OldVarTypes,
    %   !VarSet, !VarTypes, !Renaming):
    %
    % A version of clone_variables using varsets and vartypes.
    %
:- pred clone_variables_vs(list(prog_var)::in, prog_varset::in, vartypes::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

    % Return all the variables in the goal or goal expression.
    % Unlike quantification.goal_vars, this predicate returns
    % even the explicitly quantified variables.
    %
    % Warning: the complexity of this predicate is proportional to the
    % size of the goal. Goals can be pretty big. Whatever you want to do,
    % if you have a way to do it *without* calling the predicate, you will
    % probably want to it that way.
    %
:- pred goal_vars(hlds_goal::in, set_of_progvar::out) is det.

    % Do the same job as goal_vars, but for a list of goals.
    %
:- pred goals_goal_vars(list(hlds_goal)::in, set_of_progvar::out) is det.

    % Return all the variables in a generic call.
    %
:- pred generic_call_vars(generic_call::in, list(prog_var)::out) is det.

:- type attach_in_from_ground_term
    --->    attach_in_from_ground_term
    ;       do_not_attach_in_from_ground_term.

    % Attach the given goal features to the given goal and all its subgoals,
    % except possibly in from_ground_term scopes.
    %
:- pred attach_features_to_all_goals(list(goal_feature),
    attach_in_from_ground_term, hlds_goal, hlds_goal).
:- mode attach_features_to_all_goals(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_all_goals(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

    % extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps,
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
    % variable, or if the type-class-info is for an existential constraint,
    % i.e. a constraint which constrains an existentially quantified type
    % variable.
    %
:- pred extra_nonlocal_typeinfos_typeclass_infos(rtti_varmaps::in,
    var_table::in, existq_tvars::in,
    set_of_progvar::in, set_of_progvar::out) is det.

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
:- pred goals_size(list(hlds_goal)::in, int::out) is det.

    % Return an indication of the size of the list of clauses.
    %
:- pred clause_list_size(list(clause)::in, int::out) is det.

    % Return a set of all the procedures referred to by the given goal or
    % goals. Differs from goal_calls not just in the fact that it returns
    % a set of pred_proc_ids all at once, but also in that it also returns
    % procedures which are not CALLED in the given goal or goals but which
    % are referred to in other ways (e.g. by having their address taken).
    %
:- func goals_proc_refs(list(hlds_goal)) = set(pred_proc_id).
:- func goal_proc_refs(hlds_goal) = set(pred_proc_id).

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

    % goal_calls_proc_in_set(Goal, PredProcIds):
    %
    % Returns the subset of PredProcIds that are called from somewhere inside
    % Goal via plain_call.
    %
:- func goal_calls_proc_in_set(hlds_goal, set(pred_proc_id))
    = set(pred_proc_id).

    % goal_list_calls_proc_in_list(Goal, PredProcIds):
    %
    % Returns the subset of PredProcIds that are called from somewhere inside
    % Goals via plain_call.
    %
:- func goal_list_calls_proc_in_set(list(hlds_goal), set(pred_proc_id))
    = set(pred_proc_id).

    % Test whether the goal contains a reconstruction
    % (a construction where the `construct_how' field is `cell_to_reuse(_)').
    %
:- pred goal_contains_reconstruction(hlds_goal::in, bool::out) is det.

    % goal_contains_goal(Goal, SubGoal) is true iff Goal contains SubGoal,
    % i.e. iff Goal = SubGoal or Goal contains SubGoal as a direct
    % or indirect sub-goal.
    %
:- pred goal_contains_goal(hlds_goal::in, hlds_goal::out) is multi.

    % direct_subgoal(Goal, DirectSubGoal) is true iff DirectSubGoal is
    % a direct sub-goal of Goal.
    %
:- pred direct_subgoal(hlds_goal_expr::in, hlds_goal::out) is nondet.

    % Returns all the pred_ids that are called from a list of goals.
    %
:- pred pred_ids_called_from_goals(list(hlds_goal)::in,
    list(pred_id)::out) is det.

    % Returns all the pred_ids that are called from a goal.
    %
:- pred pred_ids_called_from_goal(hlds_goal::in, list(pred_id)::out) is det.

    % Returns all the pred_id/arg_list pairs that are called from a goal.
    %
:- pred pred_ids_args_called_from_goal(hlds_goal::in,
    list({pred_id, list(prog_var)})::out) is det.

    % Returns all the pred_proc_ids that are called from a goal.
    %
:- pred pred_proc_ids_called_from_goal(hlds_goal::in,
    list(pred_proc_id)::out) is det.

:- type goal_is_atomic
    --->    goal_is_atomic
    ;       goal_is_nonatomic.

    % Returns whether a goal is atomic. This is undefined for shorthand goals.
    %
:- pred goal_is_atomic(hlds_goal::in, goal_is_atomic::out) is det.

%-----------------------------------------------------------------------------%
%
% NOTE: Neither switch_to_disjunction nor case_to_disjunct is currently used,
% but they could be useful in the future.
%

    % Convert a switch back into a disjunction. This used to be needed
    % by the Aditi backend for the magic set transformation.
    % This aborts if any of the constructors are existentially typed.
    %
:- pred switch_to_disjunction(prog_var::in, list(case)::in,
    instmap::in, list(hlds_goal)::out, var_table::in, var_table::out,
    module_info::in, module_info::out) is det.

    % Convert a case into a conjunction by adding a tag test
    % (deconstruction unification) to the case goal.
    % This aborts if the constructor is existentially typed.
    %
:- pred case_to_disjunct(prog_var::in, hlds_goal::in, instmap::in,
    cons_id::in, hlds_goal::out, var_table::in, var_table::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

    % Flatten the conjuncts of a conjunction.
    % Flattens only one level.
    % XXX Why not all levels recursively, as flatten_disj does?
    %
:- pred flatten_conj(list(hlds_goal)::in, list(hlds_goal)::out) is det.

    % Flatten the disjuncts of a disjunction.
    % Flattens all levels recursively.
    %
:- pred flatten_disj(list(hlds_goal)::in, list(hlds_goal)::out) is det.

    % Create a conjunction of the specified type using the specified two goals.
    % This fills in the hlds_goal_info.
    %
:- pred create_conj(hlds_goal::in, hlds_goal::in, conj_type::in,
    hlds_goal::out) is det.

    % Create a conjunction of the specified type using the specified goals,
    % This fills in the hlds_goal_info.
    %
:- pred create_conj_from_list(list(hlds_goal)::in, conj_type::in,
    hlds_goal::out) is det.

    % can_reorder_goals_old(ModuleInfo, VarTable, FullyStrict,
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
:- pred can_reorder_goals_old(module_info::in, var_table::in, bool::in,
    instmap::in, hlds_goal::in, instmap::in, hlds_goal::in) is semidet.

:- type can_reorder_goals
    --->    cannot_reorder_goals
    ;       can_reorder_goals.

    % can_reorder_goals(VarTable, FullyStrict, InstmapBeforeGoal1, Goal1,
    %   InstmapBeforeGoal2, Goal2, Result, !ModuleInfo).
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
:- pred can_reorder_goals(var_table::in, bool::in, instmap::in,
    hlds_goal::in, instmap::in, hlds_goal::in, can_reorder_goals::out,
    module_info::in, module_info::out) is det.

    % reordering_maintains_termination_old(ModuleInfo, FullyStrict,
    %   Goal1, Goal2).
    %
    % Succeeds if any possible change in termination behaviour from reordering
    % the goals is allowed according to the semantics options.
    % The information computed by termination and exception analyses is used
    % when making this decision.
    %
    % NOTE: this version is deprecated; new code should use the following
    % version because it supports the intermodule-analysis framework.
    %
:- pred reordering_maintains_termination_old(module_info::in, bool::in,
    hlds_goal::in, hlds_goal::in) is semidet.

:- type reorder_maintains_termination
    --->    reorder_maintains_termination
    ;       reorder_does_not_maintain_termination.

    % reordering_maintains_termination(FullyStrict, Goal1, Goal2, Result,
    %   !ModuleInfo).
    %
    % Result is `yes' if any possible change in termination behaviour from
    % reordering the goals is allowed according to the semantics options.
    % The information computed by termination and exception analyses is used
    % when making this decision.
    %
    % NOTE: new code should use this version as it supports the
    % intermodule-analysis framework.
    %
:- pred reordering_maintains_termination(bool::in,
    hlds_goal::in, hlds_goal::in, reorder_maintains_termination::out,
    module_info::in, module_info::out) is det.

    % generate_plain_call(ModuleInfo, PredOrFunc, ModuleName, ProcName,
    %   TIArgVars, ArgVars, InstMapDelta, ModeNo, Detism, Purity, Features,
    %   Context, CallGoal):
    %
    % Generate a call to a builtin procedure (e.g. from the private_builtin
    % or table_builtin module). This is used by HLDS->HLDS transformation
    % passes that introduce calls to builtin procedures.
    %
    % If ModeNo = only_mode, then the predicate must have exactly one
    % procedure; an error is raised if this is not the case.
    %
    % If ModeNo = mode_no(N) then the Nth procedure is used, counting from 0.
    %
:- pred generate_plain_call(module_info::in, pred_or_func::in,
    module_name::in, string::in, list(prog_var)::in, list(prog_var)::in,
    instmap_delta::in, mode_no::in, determinism::in, purity::in,
    list(goal_feature)::in, term_context::in, hlds_goal::out) is det.

    % generate_call_foreign_proc(ModuleInfo, PredOrFunc, ModuleName, ProcName,
    %   TIArgs, Args, ExtraArgs, InstMapDelta, ModeNo, Detism, Purity,
    %   Features, Attributes, MaybeTraceRuntimeCond, Code, Context, CallGoal):
    %
    % generate_call_foreign_proc is similar to generate_plain_call,
    % but also assumes that the called predicate is defined via a
    % foreign_proc, that the foreign_proc's arguments are as given in
    % TIArgs and Args, its attributes are Attributes, and its code is Code.
    % As well as returning a foreign_code instead of a call, effectively
    % inlining the call, generate_call_foreign_proc also passes ExtraArgs
    % as well as TIArgs and Args.
    %
:- pred generate_call_foreign_proc(module_info::in, pred_or_func::in,
    module_name::in, string::in, list(foreign_arg)::in, list(foreign_arg)::in,
    list(foreign_arg)::in, instmap_delta::in, mode_no::in,
    determinism::in, purity::in, list(goal_feature)::in,
    pragma_foreign_proc_attributes::in, maybe(trace_expr(trace_runtime))::in,
    string::in, term_context::in, hlds_goal::out) is det.

    % Generate a cast goal. The input and output insts are just ground.
    %
:- pred generate_cast(cast_kind::in, prog_var::in, prog_var::in,
    prog_context::in, hlds_goal::out) is det.

    % This version takes input and output inst arguments, which may be
    % necessary when casting, say, solver type values with inst any,
    % or casting between enumeration types and ints.
    %
:- pred generate_cast_with_insts(cast_kind::in, prog_var::in, prog_var::in,
    mer_inst::in, mer_inst::in, prog_context::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%

:- pred foreign_proc_uses_variable(pragma_foreign_proc_impl::in, string::in)
    is semidet.

%-----------------------------------------------------------------------------%

:- func maybe_strip_equality_pretest(hlds_goal) = hlds_goal.

%-----------------------------------------------------------------------------%

:- type maybe_transformed_goal
    --->    ok(hlds_goal)
    ;       error(string)
    ;       goal_not_found.

    % Locate the goal described by the goal path and use its first argument to
    % transform that goal before rebuilding the goal tree and returning.
    % If the goal is not found, the result is no. If the result of the higher
    % order argument is no, then the result is no.
    %
:- pred maybe_transform_goal_at_goal_path(
    pred(hlds_goal, maybe_error(hlds_goal))::in(pred(in, out) is det),
    forward_goal_path::in, hlds_goal::in, maybe_transformed_goal::out) is det.

    % As above, except that we also compute the instmap during the traversal so
    % that the transformation expressed by the higher order value can use the
    % instmap at that point within the goal tree.
    %
:- pred maybe_transform_goal_at_goal_path_with_instmap(
    pred(instmap, hlds_goal, maybe_error(hlds_goal))::
        in(pred(in, in, out) is det),
    forward_goal_path::in, instmap::in, hlds_goal::in,
    maybe_transformed_goal::out) is det.

    % Transform the given goal and all its children according to the higher
    % order argument. Children are transformed before their parents, therefore
    % the higher order argument will receive a goal with children that have
    % already been transformed.
    %
:- pred transform_all_goals(
    pred(hlds_goal, hlds_goal)::in(pred(in, out) is det),
    hlds_goal::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

update_instmap(hlds_goal(_GoalExpr0, GoalInfo0), !InstMap) :-
    DeltaInstMap = goal_info_get_instmap_delta(GoalInfo0),
    apply_instmap_delta(DeltaInstMap, !InstMap).

update_instmap_goal_info(GoalInfo0, !InstMap) :-
    DeltaInstMap = goal_info_get_instmap_delta(GoalInfo0),
    apply_instmap_delta(DeltaInstMap, !InstMap).

%-----------------------------------------------------------------------------%

create_renaming(OrigVars, InstMapDelta, !VarTable, Unifies, NewVars,
        Renaming) :-
    create_renaming_2(OrigVars, InstMapDelta, !VarTable,
        [], RevUnifies, [], RevNewVars, map.init, Renaming),
    list.reverse(RevNewVars, NewVars),
    list.reverse(RevUnifies, Unifies).

:- pred create_renaming_2(list(prog_var)::in, instmap_delta::in,
    var_table::in, var_table::out, list(hlds_goal)::in, list(hlds_goal)::out,
    list(prog_var)::in, list(prog_var)::out,
    prog_var_renaming::in, prog_var_renaming::out) is det.

create_renaming_2([], _, !VarTable, !RevUnifies, !RevNewVars, !Renaming).
create_renaming_2([OrigVar | OrigVars], InstMapDelta, !VarTable,
        !RevUnifies, !RevNewVars, !Renaming) :-
    lookup_var_entry(!.VarTable, OrigVar, OrigEntry),
    OrigEntry = vte(_, OrigType, OrigTypeIsDummy),
    NewEntry = vte("", OrigType, OrigTypeIsDummy),
    add_var_entry(NewEntry, NewVar, !VarTable),
    instmap_delta_lookup_var(InstMapDelta, OrigVar, NewInst),
    UnifyMode = unify_modes_li_lf_ri_rf(NewInst, NewInst, free, NewInst),
    Unification = assign(OrigVar, NewVar),
    UnifyContext = unify_context(umc_explicit, []),
    GoalExpr = unify(OrigVar, rhs_var(NewVar), UnifyMode, Unification,
        UnifyContext),
    set_of_var.list_to_set([OrigVar, NewVar], NonLocals),
    UnifyInstMapDelta = instmap_delta_from_assoc_list([OrigVar - NewInst]),
    goal_info_init(NonLocals, UnifyInstMapDelta, detism_det, purity_pure,
        dummy_context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    !:RevUnifies = [Goal | !.RevUnifies],
    map.det_insert(OrigVar, NewVar, !Renaming),
    !:RevNewVars = [NewVar | !.RevNewVars],
    create_renaming_2(OrigVars, InstMapDelta, !VarTable,
        !RevUnifies, !RevNewVars, !Renaming).

%-----------------------------------------------------------------------------%

clone_variable(Var, OldVarTable, !VarTable, !Renaming, CloneVar) :-
    ( if map.search(!.Renaming, Var, CloneVarPrime) then
        CloneVar = CloneVarPrime
    else
        lookup_var_entry(OldVarTable, Var, Entry),
        add_var_entry(Entry, CloneVar, !VarTable),
        map.det_insert(Var, CloneVar, !Renaming)
    ).

clone_variable_vs(Var, OldVarNames, OldVarTypes, !VarSet, !VarTypes, !Renaming,
        CloneVar) :-
    ( if map.search(!.Renaming, Var, CloneVarPrime) then
        CloneVar = CloneVarPrime
    else
        ( if varset.search_name(OldVarNames, Var, Name) then
            varset.new_named_var(Name, CloneVar, !VarSet)
        else
            varset.new_var(CloneVar, !VarSet)
        ),
        map.det_insert(Var, CloneVar, !Renaming),
        ( if search_var_type(OldVarTypes, Var, VarType) then
            add_var_type(CloneVar, VarType, !VarTypes)
        else
            % This should never happen after typechecking, but may happen
            % before it.
            true
        )
    ).

clone_variables([], _, !VarTable, !Renaming).
clone_variables([Var | Vars], OldVarTable, !VarTable, !Renaming) :-
    clone_variable(Var, OldVarTable, !VarTable, !Renaming, _Clone),
    clone_variables(Vars, OldVarTable, !VarTable, !Renaming).

clone_variables_vs([], _, _, !VarSet, !VarTypes, !Renaming).
clone_variables_vs([Var | Vars], OldVarNames, OldVarTypes, !VarSet, !VarTypes,
        !Renaming) :-
    clone_variable_vs(Var, OldVarNames, OldVarTypes, !VarSet, !VarTypes,
        !Renaming, _CloneVar),
    clone_variables_vs(Vars, OldVarNames, OldVarTypes, !VarSet, !VarTypes,
        !Renaming).

%-----------------------------------------------------------------------------%

goal_vars(Goal, !:Set) :-
    !:Set = set_of_var.init,
    goal_vars_acc(Goal, !Set).

goals_goal_vars(Goals, !:Set) :-
    !:Set = set_of_var.init,
    goals_goal_vars_acc(Goals, !Set).

:- pred goal_vars_acc(hlds_goal::in,
    set_of_progvar::in, set_of_progvar::out) is det.

goal_vars_acc(Goal, !Set) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(Var, RHS, _, Unif, _),
        set_of_var.insert(Var, !Set),
        (
            Unif = construct(_, _, _, _, CellToReuse, _, _),
            ( if CellToReuse = reuse_cell(cell_to_reuse(Var, _, _)) then
                set_of_var.insert(Var, !Set)
            else
                true
            )
        ;
            ( Unif = deconstruct(_, _, _, _, _, _)
            ; Unif = assign(_, _)
            ; Unif = simple_test(_, _)
            ; Unif = complicated_unify(_, _, _)
            )
        ),
        rhs_goal_vars_acc(RHS, !Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars, _, _, _),
        generic_call_vars(GenericCall, GenericCallVars),
        set_of_var.insert_list(GenericCallVars, !Set),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        goals_goal_vars_acc(Goals, !Set)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        set_of_var.insert(Var, !Set),
        cases_goal_vars_acc(Cases, !Set)
    ;
        GoalExpr = scope(Reason, SubGoal),
        (
            Reason = exist_quant(Vars, _),
            set_of_var.insert_list(Vars, !Set)
        ;
            Reason = promise_solutions(Vars, _),
            set_of_var.insert_list(Vars, !Set)
        ;
            Reason = from_ground_term(Var, _),
            set_of_var.insert(Var, !Set)
        ;
            ( Reason = require_complete_switch(Var)
            ; Reason = require_switch_arms_detism(Var, _)
            ),
            set_of_var.insert(Var, !Set)
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            set_of_var.insert(LCVar, !Set),
            set_of_var.insert(LCSVar, !Set)
        ;
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            )
        ),
        goal_vars_acc(SubGoal, !Set)
    ;
        GoalExpr = negation(SubGoal),
        goal_vars_acc(SubGoal, !Set)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        set_of_var.insert_list(Vars, !Set),
        goal_vars_acc(Cond, !Set),
        goal_vars_acc(Then, !Set),
        goal_vars_acc(Else, !Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        set_of_var.insert_list(ArgVars, !Set),
        set_of_var.insert_list(ExtraVars, !Set)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = atomic_goal(_, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, _),
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            set_of_var.insert(OuterDI, !Set),
            set_of_var.insert(OuterUO, !Set),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert(InnerDI, !Set),
            set_of_var.insert(InnerUO, !Set),
            (
                MaybeOutputVars = no
            ;
                MaybeOutputVars = yes(OutputVars),
                set_of_var.insert_list(OutputVars, !Set)
            ),
            goal_vars_acc(MainGoal, !Set),
            goals_goal_vars_acc(OrElseGoals, !Set)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            % The IO and Result variables would be in SubGoal.
            goal_vars_acc(SubGoal, !Set)
        ;
            Shorthand = bi_implication(LeftGoal, RightGoal),
            goal_vars_acc(LeftGoal, !Set),
            goal_vars_acc(RightGoal, !Set)
        )
    ).

:- pred goals_goal_vars_acc(list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

goals_goal_vars_acc([], !Set).
goals_goal_vars_acc([Goal | Goals], !Set) :-
    goal_vars_acc(Goal, !Set),
    goals_goal_vars_acc(Goals, !Set).

:- pred cases_goal_vars_acc(list(case)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

cases_goal_vars_acc([], !Set).
cases_goal_vars_acc([case(_, _, Goal) | Cases], !Set) :-
    goal_vars_acc(Goal, !Set),
    cases_goal_vars_acc(Cases, !Set).

:- pred rhs_goal_vars_acc(unify_rhs::in,
    set_of_progvar::in, set_of_progvar::out) is det.

rhs_goal_vars_acc(RHS, !Set) :-
    (
        RHS = rhs_var(X),
        set_of_var.insert(X, !Set)
    ;
        RHS = rhs_functor(_Functor, _, ArgVars),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, NonLocals, ArgVarsModes, _, Goal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        set_of_var.insert_list(NonLocals, !Set),
        set_of_var.insert_list(ArgVars, !Set),
        goal_vars_acc(Goal, !Set)
    ).

generic_call_vars(higher_order(Var, _, _, _), [Var]).
generic_call_vars(class_method(Var, _, _, _), [Var]).
generic_call_vars(event_call(_), []).
generic_call_vars(cast(_), []).

%-----------------------------------------------------------------------------%

attach_features_to_all_goals(Features, InFromGroundTerm, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    attach_features_to_goal_expr(Features, InFromGroundTerm,
        GoalExpr0, GoalExpr),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred attach_features_to_goals(list(goal_feature),
    attach_in_from_ground_term, list(hlds_goal), list(hlds_goal)).
:- mode attach_features_to_goals(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_goals(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

attach_features_to_goals(_Features, _InFromGroundTerm, [], []).
attach_features_to_goals(Features, InFromGroundTerm,
        [Goal0 | Goals0], [Goal | Goals]) :-
    attach_features_to_all_goals(Features, InFromGroundTerm, Goal0, Goal),
    attach_features_to_goals(Features, InFromGroundTerm, Goals0, Goals).

:- pred attach_features_to_cases(list(goal_feature),
    attach_in_from_ground_term, list(case), list(case)).
:- mode attach_features_to_cases(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_cases(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

attach_features_to_cases(_Features, _InFromGroundTerm, [], []).
attach_features_to_cases(Features, InFromGroundTerm,
        [Case0 | Cases0], [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    attach_features_to_all_goals(Features, InFromGroundTerm, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal),
    attach_features_to_cases(Features, InFromGroundTerm, Cases0, Cases).

:- pred attach_features_to_goal_expr(list(goal_feature),
    attach_in_from_ground_term, hlds_goal_expr, hlds_goal_expr).
:- mode attach_features_to_goal_expr(in,
    in(bound(attach_in_from_ground_term)), in, out) is det.
:- mode attach_features_to_goal_expr(in,
    in(bound(do_not_attach_in_from_ground_term)), in, out) is det.

attach_features_to_goal_expr(Features, InFromGroundTerm,
        GoalExpr0, GoalExpr) :-
    (
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        attach_features_to_goals(Features, InFromGroundTerm, Goals0, Goals),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        attach_features_to_goals(Features, InFromGroundTerm, Goals0, Goals),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        attach_features_to_cases(Features, InFromGroundTerm, Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        attach_features_to_all_goals(Features, InFromGroundTerm, Cond0, Cond),
        attach_features_to_all_goals(Features, InFromGroundTerm, Then0, Then),
        attach_features_to_all_goals(Features, InFromGroundTerm, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        attach_features_to_all_goals(Features, InFromGroundTerm,
            SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if Reason = from_ground_term(_, _) then
            (
                InFromGroundTerm = do_not_attach_in_from_ground_term,
                SubGoal = SubGoal0
            ;
                InFromGroundTerm = attach_in_from_ground_term,
                attach_features_to_all_goals(Features, InFromGroundTerm,
                    SubGoal0, SubGoal)
            )
        else
            attach_features_to_all_goals(Features, InFromGroundTerm,
                SubGoal0, SubGoal)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                MainGoal0, MainGoal),
            attach_features_to_goals(Features, InFromGroundTerm,
                OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(GoalA0, GoalB0),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                GoalA0, GoalA),
            attach_features_to_all_goals(Features, InFromGroundTerm,
                GoalB0, GoalB),
            ShortHand = bi_implication(GoalA, GoalB)
        ),
        GoalExpr = shorthand(ShortHand)
    ).

%-----------------------------------------------------------------------------%

extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps, VarTable, ExistQVars,
        NonLocals, NonLocalTiTciVars) :-
    % Find all non-local type vars. That is, type vars that are existentially
    % quantified or type vars that appear in the type of a non-local prog_var.
    set_of_var.to_sorted_list(NonLocals, NonLocalsList),
    lookup_var_types(VarTable, NonLocalsList, NonLocalsTypes),
    do_extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps, NonLocalsTypes,
        ExistQVars, NonLocalTiTciVars).

:- pred do_extra_nonlocal_typeinfos_typeclass_infos(rtti_varmaps::in,
    list(mer_type)::in, existq_tvars::in, set_of_progvar::out) is det.

do_extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps, NonLocalsTypes,
        ExistQVars, NonLocalTiTciVars) :-
    type_vars_in_types(NonLocalsTypes, NonLocalTypeVarsList0),
    NonLocalTypeVarsList = ExistQVars ++ NonLocalTypeVarsList0,
    set_of_var.list_to_set(NonLocalTypeVarsList, NonLocalTypeVars),

    % Find all the type_infos that are non-local, that is, type_infos for
    % type vars that are non-local in the above sense.
    TypeVarToProgVar =
        ( func(TypeVar) = ProgVar :-
            rtti_lookup_type_info_locn(RttiVarMaps, TypeVar, Locn),
            type_info_locn_var(Locn, ProgVar)
        ),
    NonLocalTypeInfoVars = set_of_var.list_to_set(
        list.map(TypeVarToProgVar, NonLocalTypeVarsList)),

    % Find all the typeclass_infos that are non-local. These include
    % all typeclass_infos that constrain a type variable that is non-local
    % in the above sense.
    solutions.solutions(
        ( pred(Var::out) is nondet :-
            % Search through all arguments of all constraints
            % that the goal could have used.
            rtti_varmaps_reusable_constraints(RttiVarMaps, Constraints),
            list.member(Constraint, Constraints),
            Constraint = constraint(_ClassName, ArgTypes),
            type_list_contains_var(ArgTypes, TypeVar),
            set_of_var.member(NonLocalTypeVars, TypeVar),

            % We found a constraint that is non-local. Include the variable
            % holding its typeclass_info.
            rtti_lookup_typeclass_info_var(RttiVarMaps, Constraint, Var)
        ), NonLocalTypeClassInfoVarsList),
    set_of_var.sorted_list_to_set(NonLocalTypeClassInfoVarsList,
        NonLocalTypeClassInfoVars),
    set_of_var.union(NonLocalTypeInfoVars, NonLocalTypeClassInfoVars,
        NonLocalTiTciVars).

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
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        IsLeaf = is_not_leaf
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        IsLeaf = proc_body_is_leaf_goals(Goals)
    ;
        GoalExpr = negation(SubGoal),
        IsLeaf = proc_body_is_leaf(SubGoal)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            IsLeaf = is_leaf
        else
            IsLeaf = proc_body_is_leaf(SubGoal)
        )
    ;
        GoalExpr = switch(_, _, Cases),
        IsLeaf = proc_body_is_leaf_cases(Cases)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( if
            proc_body_is_leaf(Cond) = is_leaf,
            proc_body_is_leaf(Then) = is_leaf,
            proc_body_is_leaf(Else) = is_leaf
        then
            IsLeaf = is_leaf
        else
            IsLeaf = is_not_leaf
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ( ShortHand = atomic_goal(_, _, _, _, _, _, _)
            ; ShortHand = try_goal(_, _, _)
            ),
            IsLeaf = is_not_leaf
        ;
            ShortHand = bi_implication(GoalA, GoalB),
            ( if
                proc_body_is_leaf(GoalA) = is_leaf,
                proc_body_is_leaf(GoalB) = is_leaf
            then
                IsLeaf = is_leaf
            else
                IsLeaf = is_not_leaf
            )
        )
    ).

:- func proc_body_is_leaf_goals(list(hlds_goal)) = is_leaf.

proc_body_is_leaf_goals([]) = is_leaf.
proc_body_is_leaf_goals([Goal | Goals]) = IsLeaf :-
    ( if
        proc_body_is_leaf(Goal) = is_leaf,
        proc_body_is_leaf_goals(Goals) = is_leaf
    then
        IsLeaf = is_leaf
    else
        IsLeaf = is_not_leaf
    ).

:- func proc_body_is_leaf_cases(list(case)) = is_leaf.

proc_body_is_leaf_cases([]) = is_leaf.
proc_body_is_leaf_cases([Case | Cases]) = IsLeaf :-
    Case = case(_, _, Goal),
    ( if
        proc_body_is_leaf(Goal) = is_leaf,
        proc_body_is_leaf_cases(Cases) = is_leaf
    then
        IsLeaf = is_leaf
    else
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
    ( if Clauses = [_] then
        GoalSize = GoalSize0
    else
        % Add one for the disjunction.
        GoalSize = GoalSize0 + 1
    ).

:- pred clause_size_increment(clause::in, int::in, int::out) is det.

clause_size_increment(Clause, Size0, Size) :-
    goal_size(Clause ^ clause_body, ClauseSize),
    Size = Size0 + ClauseSize.

:- pred cases_size(list(case)::in, int::out) is det.

cases_size([], 0).
cases_size([case(_, _, Goal) | Cases], Size) :-
    goal_size(Goal, Size1),
    cases_size(Cases, Size2),
    Size = Size1 + Size2.

:- pred goal_expr_size(hlds_goal_expr::in, int::out) is det.

goal_expr_size(GoalExpr, Size) :-
    (
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Size = 1
    ;
        GoalExpr = conj(ConjType, Goals),
        goals_size(Goals, InnerSize),
        (
            ConjType = plain_conj,
            Size = InnerSize
        ;
            ConjType = parallel_conj,
            Size = InnerSize + 1
        )
    ;
        GoalExpr = disj(Goals),
        goals_size(Goals, Size1),
        Size = Size1 + 1
    ;
        GoalExpr = switch(_, _, Cases),
        cases_size(Cases, Size1),
        Size = Size1 + 1
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_size(Cond, Size1),
        goal_size(Then, Size2),
        goal_size(Else, Size3),
        Size = Size1 + Size2 + Size3 + 1
    ;
        GoalExpr = negation(SubGoal),
        goal_size(SubGoal, Size1),
        Size = Size1 + 1
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % These scopes get turned into a single assignment.
            Size = 1
        else
            goal_size(SubGoal, Size1),
            Size = Size1 + 1
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            goal_size(MainGoal, Size1),
            goals_size(OrElseGoals, Size2),
            Size = Size1 + Size2 + 1
        ;
            ShortHand = try_goal(_, _, SubGoal),
            % Hopefully this size isn't too important as the SubGoal is not yet
            % in the final form.
            goal_size(SubGoal, Size)
        ;
            ShortHand = bi_implication(GoalA, GoalB),
            goal_size(GoalA, Size1),
            goal_size(GoalB, Size2),
            Size = Size1 + Size2 + 1
        )
    ).

%-----------------------------------------------------------------------------%

goals_proc_refs(Goals) = ReferredToProcs :-
    list.foldl(goal_proc_refs_acc, Goals, set.init, ReferredToProcs).

goal_proc_refs(Goal) = ReferredToProcs :-
    goal_proc_refs_acc(Goal, set.init, ReferredToProcs).

:- pred goal_proc_refs_acc(hlds_goal::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

goal_proc_refs_acc(Goal, !ReferredToProcs) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        list.foldl(goal_proc_refs_acc, Conjuncts, !ReferredToProcs)
    ;
        GoalExpr = disj(Disjuncts),
        list.foldl(goal_proc_refs_acc, Disjuncts, !ReferredToProcs)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl(case_proc_refs_acc, Cases, !ReferredToProcs)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_proc_refs_acc(Cond, !ReferredToProcs),
        goal_proc_refs_acc(Then, !ReferredToProcs),
        goal_proc_refs_acc(Else, !ReferredToProcs)
    ;
        GoalExpr = negation(SubGoal),
        goal_proc_refs_acc(SubGoal, !ReferredToProcs)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction and deconstruction
            % unifications respectively.
            true
        else
            goal_proc_refs_acc(SubGoal, !ReferredToProcs)
        )
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(SubGoalA, SubGoalB),
            goal_proc_refs_acc(SubGoalA, !ReferredToProcs),
            goal_proc_refs_acc(SubGoalB, !ReferredToProcs)
        ;
            Shorthand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            goal_proc_refs_acc(MainGoal, !ReferredToProcs),
            list.foldl(goal_proc_refs_acc, OrElseGoals, !ReferredToProcs)
        ;
            Shorthand = try_goal(_, _, SubGoal),
            goal_proc_refs_acc(SubGoal, !ReferredToProcs)
        )
    ;
        ( GoalExpr = plain_call(PredId, ProcId, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, PredId, ProcId, _, _, _, _)
        ),
        PredProcId = proc(PredId, ProcId),
        set.insert(PredProcId, !ReferredToProcs)
    ;
        GoalExpr = unify(_LHS, RHS, _, Unification, _),
        (
            RHS = rhs_var(_)
        ;
            RHS = rhs_functor(RHSConsId, _IsExistConstruct, _ArgVars),
            cons_id_proc_refs_acc(RHSConsId, !ReferredToProcs)
        ;
            RHS = rhs_lambda_goal(_, _, _, _, _, _, _, SubGoal),
            goal_proc_refs_acc(SubGoal, !ReferredToProcs)
        ),
        (
            Unification = construct(_LHSVar, ConstructConsId, _RHSVars,
                _ArgModes, _How, _IsUnique, _SubInfo),
            cons_id_proc_refs_acc(ConstructConsId, !ReferredToProcs)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            ; Unification = complicated_unify(_, _, _)
            )
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ).

:- pred case_proc_refs_acc(case::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

case_proc_refs_acc(Case, !ReferredToProcs) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    goal_proc_refs_acc(Goal, !ReferredToProcs).

:- pred cons_id_proc_refs_acc(cons_id::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

cons_id_proc_refs_acc(ConsId, !ReferredToProcs) :-
    (
        ( ConsId = cons(_, _, _)
        ; ConsId = tuple_cons(_)
        ; ConsId = some_int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ;
        ConsId = closure_cons(ShroudedPredProcId, _LambdaMethod),
        PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
        set.insert(PredProcId, !ReferredToProcs)
    ).

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls as
%   goal_calls(Goal, proc(PredId, ProcId)) :-
%       goal_contains_subgoal(Goal, plain_call(PredId, ProcId, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

% XXX STM
% split this predicate into two:
% goal_calls_this_proc(Goal, PredProcId) = bool
% all_called_procs_in_goal(Goal) = cord(pred_proc_id)

goal_calls(hlds_goal(GoalExpr, _), PredProcId) :-
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

cases_calls([case(_, _, Goal) | Cases], PredProcId) :-
    (
        goal_calls(Goal, PredProcId)
    ;
        cases_calls(Cases, PredProcId)
    ).

:- pred goal_expr_calls(hlds_goal_expr, pred_proc_id).
:- mode goal_expr_calls(in, in) is semidet.
:- mode goal_expr_calls(in, out) is nondet.

goal_expr_calls(GoalExpr, PredProcId) :-
    require_complete_switch [GoalExpr]
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        goals_calls(Conjuncts, PredProcId)
    ;
        GoalExpr = disj(Disjuncts),
        goals_calls(Disjuncts, PredProcId)
    ;
        GoalExpr = switch(_, _, Cases),
        cases_calls(Cases, PredProcId)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( goal_calls(Cond, PredProcId)
        ; goal_calls(Then, PredProcId)
        ; goal_calls(Else, PredProcId)
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_calls(SubGoal, PredProcId)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction and deconstruction
            % unifications respectively.
            fail
        else
            goal_calls(SubGoal, PredProcId)
        )
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        PredProcId = proc(PredId, ProcId)
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

%-----------------------------------------------------------------------------%
%
% We could implement goal_calls_pred_id as
%   goal_calls_pred_id(Goal, PredId) :-
%       goal_contains_subgoal(Goal, plain_call(PredId, _, _, _, _, _)).
% but the following is more efficient in the (in, in) mode
% since it avoids creating any choice points.
%

goal_calls_pred_id(hlds_goal(GoalExpr, _), PredId) :-
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

cases_calls_pred_id([case(_, _, Goal) | Cases], PredId) :-
    (
        goal_calls_pred_id(Goal, PredId)
    ;
        cases_calls_pred_id(Cases, PredId)
    ).

:- pred goal_expr_calls_pred_id(hlds_goal_expr, pred_id).
:- mode goal_expr_calls_pred_id(in, in) is semidet.
:- mode goal_expr_calls_pred_id(in, out) is nondet.

goal_expr_calls_pred_id(GoalExpr, PredId) :-
    require_complete_switch [GoalExpr]
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        goals_calls_pred_id(Conjuncts, PredId)
    ;
        GoalExpr = disj(Disjuncts),
        goals_calls_pred_id(Disjuncts, PredId)
    ;
        GoalExpr = switch(_, _, Cases),
        cases_calls_pred_id(Cases, PredId)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( goal_calls_pred_id(Cond, PredId)
        ; goal_calls_pred_id(Then, PredId)
        ; goal_calls_pred_id(Else, PredId)
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_calls_pred_id(SubGoal, PredId)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction and deconstruction
            % unifications respectively.
            fail
        else
            goal_calls_pred_id(SubGoal, PredId)
        )
    ;
        GoalExpr = plain_call(PredId, _, _, _, _, _)
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

%-----------------------------------------------------------------------------%

goal_calls_proc_in_set(Goal, PredProcIds) = CalledPredProcIds :-
    goal_calls_proc_in_set_acc(Goal, PredProcIds,
        set.init, CalledPredProcIds).

goal_list_calls_proc_in_set(Goals, PredProcIds) = CalledPredProcIds :-
    goal_list_calls_proc_in_set_acc(Goals, PredProcIds,
        set.init, CalledPredProcIds).

:- pred goal_calls_proc_in_set_acc(hlds_goal::in, set(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

goal_calls_proc_in_set_acc(hlds_goal(GoalExpr, _GoalInfo), PredProcIds,
        !CalledSet) :-
    (
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( if set.member(proc(PredId, ProcId), PredProcIds) then
            set.insert(proc(PredId, ProcId), !CalledSet)
        else
            true
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = conj(_, Goals),
        goal_list_calls_proc_in_set_acc(Goals, PredProcIds, !CalledSet)
    ;
        GoalExpr = disj(Goals),
        goal_list_calls_proc_in_set_acc(Goals, PredProcIds, !CalledSet)
    ;
        GoalExpr = switch(_, _, Cases),
        case_list_calls_proc_in_list_acc(Cases, PredProcIds, !CalledSet)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        goal_calls_proc_in_set_acc(Cond, PredProcIds, !CalledSet),
        goal_calls_proc_in_set_acc(Then, PredProcIds, !CalledSet),
        goal_calls_proc_in_set_acc(Else, PredProcIds, !CalledSet)
    ;
        GoalExpr = negation(Goal),
        goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet)
    ;
        GoalExpr = scope(Reason, Goal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % These goals contain only construction unifications.
            true
        else
            goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet)
        )
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, MainGoal, OrElseGoals, _),
            goal_calls_proc_in_set_acc(MainGoal, PredProcIds, !CalledSet),
            goal_list_calls_proc_in_set_acc(OrElseGoals, PredProcIds,
                !CalledSet)
        ;
            ShortHand = try_goal(_, _, SubGoal),
            goal_calls_proc_in_set_acc(SubGoal, PredProcIds, !CalledSet)
        ;
            ShortHand = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- pred goal_list_calls_proc_in_set_acc(list(hlds_goal)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::in, set(pred_proc_id)::out)
    is det.

goal_list_calls_proc_in_set_acc([], _, !CalledSet).
goal_list_calls_proc_in_set_acc([Goal | Goals], PredProcIds, !CalledSet) :-
    goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet),
    goal_list_calls_proc_in_set_acc(Goals, PredProcIds, !CalledSet).

:- pred case_list_calls_proc_in_list_acc(list(case)::in, set(pred_proc_id)::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

case_list_calls_proc_in_list_acc([], _, !CalledSet).
case_list_calls_proc_in_list_acc([Case | Cases], PredProcIds, !CalledSet) :-
    Case = case(_, _, Goal),
    goal_calls_proc_in_set_acc(Goal, PredProcIds, !CalledSet),
    case_list_calls_proc_in_list_acc(Cases, PredProcIds, !CalledSet).

%-----------------------------------------------------------------------------%

goal_contains_reconstruction(Goal, ContainsReconstruction) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = conj(_ConjType, Conjuncts),
        goals_contain_reconstruction(Conjuncts, ContainsReconstruction)
    ;
        GoalExpr = disj(Disjuncts),
        goals_contain_reconstruction(Disjuncts, ContainsReconstruction)
    ;
        GoalExpr = switch(_, _, Cases),
        cases_contain_reconstruction(Cases, ContainsReconstruction)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( if
            ( goal_contains_reconstruction(Cond, yes)
            ; goal_contains_reconstruction(Then, yes)
            ; goal_contains_reconstruction(Else, yes)
            )
        then
            ContainsReconstruction = yes
        else
            ContainsReconstruction = no
        )
    ;
        GoalExpr = negation(SubGoal),
        goal_contains_reconstruction(SubGoal, ContainsReconstruction)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % Construct scopes contain only construction unifications
            % that do no reuse. Deconstruct scopes do not contain
            % any constructions at all.
            ContainsReconstruction = no
        else
            goal_contains_reconstruction(SubGoal, ContainsReconstruction)
        )
    ;
        GoalExpr = unify(_, _, _, Unify, _),
        ( if
            Unify = construct(_, _, _, _, HowToConstruct, _, _),
            HowToConstruct = reuse_cell(_)
        then
            ContainsReconstruction = yes
        else
            ContainsReconstruction = no
        )
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ContainsReconstruction = no
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(GoalA, GoalB),
            ( if
                goal_contains_reconstruction(GoalA, yes),
                goal_contains_reconstruction(GoalB, yes)
            then
                ContainsReconstruction = yes
            else
                ContainsReconstruction = no
            )
        ;
            Shorthand = atomic_goal(_AtomicGoalType, _OuterVars, _InnerVars,
                _OutputVars, MainGoal, OrElseGoals, _Inners),
            ( if
                goal_contains_reconstruction(MainGoal, yes),
                goals_contain_reconstruction(OrElseGoals, yes)
            then
                ContainsReconstruction = yes
            else
                ContainsReconstruction = no
            )
        ;
            Shorthand = try_goal(_MaybeTryIOStateVars, _ResultVar, SubGoal),
            goal_contains_reconstruction(SubGoal, ContainsReconstruction)
        )
    ).

:- pred goals_contain_reconstruction(list(hlds_goal)::in, bool::out) is det.

goals_contain_reconstruction([], no).
goals_contain_reconstruction([Goal | Goals], ContainsReconstruction) :-
    goal_contains_reconstruction(Goal, HeadContainsReconstruction),
    (
        HeadContainsReconstruction = yes,
        ContainsReconstruction = yes
    ;
        HeadContainsReconstruction = no,
        goals_contain_reconstruction(Goals, ContainsReconstruction)
    ).

:- pred cases_contain_reconstruction(list(case)::in, bool::out) is det.

cases_contain_reconstruction([], no).
cases_contain_reconstruction([Case | Cases], ContainsReconstruction) :-
    Case = case(_, _, CaseGoal),
    goal_contains_reconstruction(CaseGoal, HeadContainsReconstruction),
    (
        HeadContainsReconstruction = yes,
        ContainsReconstruction = yes
    ;
        HeadContainsReconstruction = no,
        cases_contain_reconstruction(Cases, ContainsReconstruction)
    ).

%-----------------------------------------------------------------------------%

goal_contains_goal(Goal, Goal).
goal_contains_goal(hlds_goal(GoalExpr, _), SubGoal) :-
    direct_subgoal(GoalExpr, DirectSubGoal),
    goal_contains_goal(DirectSubGoal, SubGoal).

direct_subgoal(scope(_, Goal), Goal).
direct_subgoal(negation(Goal), Goal).
direct_subgoal(if_then_else(_, Cond, Then, Else), Goal) :-
    ( Goal = Cond
    ; Goal = Then
    ; Goal = Else
    ).
direct_subgoal(conj(_ConjType, ConjList), Goal) :-
    list.member(Goal, ConjList).
direct_subgoal(disj(DisjList), Goal) :-
    list.member(Goal, DisjList).
direct_subgoal(switch(_, _, CaseList), Goal) :-
    list.member(Case, CaseList),
    Case = case(_, _, Goal).

%-----------------------------------------------------------------------------%

pred_ids_called_from_goals(Goals, PredIds) :-
    (
        Goals = [],
        PredIds = []
    ;
        Goals = [HeadGoal | TailGoals],
        pred_ids_called_from_goal(HeadGoal, HeadPredIds),
        pred_ids_called_from_goals(TailGoals, TailPredIds),
        PredIds = HeadPredIds ++ TailPredIds
    ).

pred_ids_called_from_goal(Goal, PredIds) :-
    % Explicit lambda expression needed since goal_calls_pred_id
    % has multiple modes.
    P = ( pred(PredId::out) is nondet :-
            goal_contains_goal(Goal, SubGoal),
            SubGoal = hlds_goal(SubGoalExpr, _),
            SubGoalExpr = plain_call(PredId, _, _, _, _, _)
        ),
    solutions.solutions(P, PredIds).

pred_ids_args_called_from_goal(Goal, List) :-
    P = ( pred({PredId, Args}::out) is nondet :-
            goal_contains_goal(Goal, SubGoal),
            SubGoal = hlds_goal(SubGoalExpr, _),
            SubGoalExpr = plain_call(PredId, _, Args, _, _, _)
        ),
    solutions(P, List).

pred_proc_ids_called_from_goal(Goal, PredProcIds) :-
    P = ( pred(PredProcId::out) is nondet :-
            goal_contains_goal(Goal, SubGoal),
            SubGoal = hlds_goal(SubGoalExpr, _),
            SubGoalExpr = plain_call(PredId, ProcId, _, _, _, _),
            PredProcId = proc(PredId, ProcId)
        ),
    solutions.solutions(P, PredProcIds).

goal_is_atomic(Goal, GoalIsAtomic) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalIsAtomic = goal_is_atomic
    ;
        ( GoalExpr = conj(_, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ),
        GoalIsAtomic = goal_is_nonatomic
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

switch_to_disjunction(_, [], _, [], !VarTable, !ModuleInfo).
switch_to_disjunction(Var, [Case | Cases], InstMap, Goals,
        !VarTable, !ModuleInfo) :-
    Case = case(MainConsId, OtherConsIds, CaseGoal),
    case_to_disjunct(Var, CaseGoal, InstMap, MainConsId, MainDisjunctGoal,
        !VarTable, !ModuleInfo),
    list.map_foldl2(case_to_disjunct(Var, CaseGoal, InstMap),
        OtherConsIds, OtherDisjunctGoals, !VarTable, !ModuleInfo),
    switch_to_disjunction(Var, Cases, InstMap, CasesGoals, !VarTable,
        !ModuleInfo),
    Goals = [MainDisjunctGoal | OtherDisjunctGoals] ++ CasesGoals.

case_to_disjunct(Var, CaseGoal, InstMap, ConsId, Disjunct,
        !VarTable, !ModuleInfo) :-
    ConsArity = cons_id_arity(ConsId),
    lookup_var_type(!.VarTable, Var, VarType),
    type_util.get_cons_id_arg_types(!.ModuleInfo, VarType, ConsId, ArgTypes),
    MakeArgEntry =
        ( pred(T::in, vte("", T, IsDummy)::out) is det :-
            IsDummy = is_type_a_dummy(!.ModuleInfo, T)
        ),
    list.map(MakeArgEntry, ArgTypes, ArgEntries),
    list.map_foldl(add_var_entry, ArgEntries, ArgVars, !VarTable),

    instmap_lookup_var(InstMap, Var, Inst0),
    ( if
        inst_expand(!.ModuleInfo, Inst0, Inst1),
        get_arg_insts(Inst1, ConsId, ConsArity, ArgInsts1)
    then
        ArgInsts = ArgInsts1
    else
        unexpected($pred, "get_arg_insts failed")
    ),
    InstToArgUnifyMode =
        ( pred(ArgInst::in, ArgUnfyiMode::out) is det :-
            ArgUnfyiMode = unify_modes_li_lf_ri_rf(ArgInst, ArgInst,
                free, ArgInst)
        ),
    list.map(InstToArgUnifyMode, ArgInsts, UniModes),
    UnifyMode = unify_modes_li_lf_ri_rf(Inst0, Inst0, Inst0, Inst0),
    UnifyContext = unify_context(umc_explicit, []),
    Unification = deconstruct(Var, ConsId, ArgVars, UniModes, can_fail,
        cannot_cgc),
    RHS = rhs_functor(ConsId, is_not_exist_constr, ArgVars),
    ExtraGoalExpr = unify(Var, RHS, UnifyMode, Unification, UnifyContext),
    NonLocals = set_of_var.make_singleton(Var),
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
    set_of_var.insert(Var, CaseNonLocals0, CaseNonLocals),
    CaseInstMapDelta = goal_info_get_instmap_delta(CaseGoalInfo),
    instmap_delta_apply_instmap_delta(ExtraInstMapDelta, CaseInstMapDelta,
        test_size, InstMapDelta),
    CaseDetism0 = goal_info_get_determinism(CaseGoalInfo),
    det_conjunction_detism(detism_semi, CaseDetism0, Detism),
    CasePurity = goal_info_get_purity(CaseGoalInfo),
    goal_info_init(CaseNonLocals, InstMapDelta, Detism, CasePurity,
        CombinedGoalInfo),
    Disjunct = hlds_goal(conj(plain_conj, GoalList), CombinedGoalInfo).

%-----------------------------------------------------------------------------%

flatten_conj([], []).
flatten_conj([Goal | Goals0], FlatConj) :-
    flatten_conj(Goals0, FlatConjTail),
    ( if Goal = hlds_goal(conj(plain_conj, SubGoals), _) then
        FlatConj = SubGoals ++ FlatConjTail
    else
        FlatConj = [Goal | FlatConjTail]
    ).

flatten_disj(Disjuncts, FlatDisjuncts) :-
    list.foldr(flatten_disj_acc, Disjuncts, [], FlatDisjuncts).

:- pred flatten_disj_acc(hlds_goal::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

flatten_disj_acc(Disjunct, !FlatDisjuncts) :-
    ( if Disjunct = hlds_goal(disj(SubDisjs), _GoalInfo) then
        list.foldr(flatten_disj_acc, SubDisjs, !FlatDisjuncts)
    else
        !:FlatDisjuncts = [Disjunct | !.FlatDisjuncts]
    ).

%-----------------------------------------------------------------------------%

create_conj(GoalA, GoalB, Type, ConjGoal) :-
    create_conj_from_list([GoalA, GoalB], Type, ConjGoal).

create_conj_from_list(Conjuncts, ConjType, ConjGoal) :-
    (
        Conjuncts = [HeadGoal | TailGoals],
        (
            TailGoals = [ _ | _ ],
            ConjGoalExpr = conj(ConjType, Conjuncts),
            goal_list_nonlocals(Conjuncts, NonLocals),
            goal_list_instmap_delta(Conjuncts, InstMapDelta),
            goal_list_determinism(Conjuncts, Detism),
            goal_list_purity(Conjuncts, Purity),
            HeadGoal = hlds_goal(_, HeadGoalInfo),
            Context = goal_info_get_context(HeadGoalInfo),
            goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
                ConjGoalInfo),
            ConjGoal = hlds_goal(ConjGoalExpr, ConjGoalInfo)
        ;
            TailGoals = [],
            ConjGoal = HeadGoal
        )
    ;
        Conjuncts = [],
        unexpected($pred, "empty conjunction")
    ).

%-----------------------------------------------------------------------------%

can_reorder_goals_old(ModuleInfo, VarTable, FullyStrict,
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
    not goal_depends_on_earlier_goal(ModuleInfo, VarTable,
        LaterGoal, EarlierGoal, InstmapBeforeEarlierGoal),

    % Don't reorder the goals if the later goal changes the instantiatedness
    % of any of the non-locals of the earlier goal. This is necessary if the
    % later goal clobbers any of the non-locals of the earlier goal, and
    % avoids rerunning full mode analysis in other cases.
    not goal_depends_on_earlier_goal(ModuleInfo, VarTable,
        EarlierGoal, LaterGoal, InstmapBeforeLaterGoal).

can_reorder_goals(VarTable, FullyStrict, InstmapBeforeEarlierGoal,
        EarlierGoal, InstmapBeforeLaterGoal, LaterGoal, CanReorder,
        !ModuleInfo) :-
    % The logic here is mostly duplicated in can_reorder_goals_old above
    % and in pd_can_reorder_goals in pd_util.m.

    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    % Impure goals and trace goals cannot be reordered.
    goal_info_get_goal_purity(EarlierGoalInfo, EarlierPurity, EarlierTrace),
    goal_info_get_goal_purity(LaterGoalInfo, LaterPurity, LaterTrace),
    ( if
        ( EarlierPurity = purity_impure
        ; LaterPurity = purity_impure
        ; EarlierTrace = contains_trace_goal
        ; LaterTrace = contains_trace_goal
        )
    then
        CanReorder = cannot_reorder_goals
    else
        reordering_maintains_termination(FullyStrict,
            EarlierGoal, LaterGoal, MaintainsTermination, !ModuleInfo),
        (
            MaintainsTermination = reorder_does_not_maintain_termination,
            CanReorder = cannot_reorder_goals
        ;
            MaintainsTermination = reorder_maintains_termination,
            ( if
                % Don't reorder the goals if the later goal depends on the
                % outputs of the current goal.
                %
                goal_depends_on_earlier_goal(!.ModuleInfo, VarTable,
                    LaterGoal, EarlierGoal, InstmapBeforeEarlierGoal)
            then
                CanReorder = cannot_reorder_goals
            else if
                % Don't reorder the goals if the later goal changes the
                % instantiatedness of any of the non-locals of the earlier
                % goal. This is necessary if the later goal clobbers any of
                % the non-locals of the earlier goal, and avoids rerunning
                % full mode analysis in other cases.
                %
                goal_depends_on_earlier_goal(!.ModuleInfo, VarTable,
                    EarlierGoal, LaterGoal, InstmapBeforeLaterGoal)
            then
                CanReorder = cannot_reorder_goals
            else
                CanReorder = can_reorder_goals
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
    ( if
        FullyStrict = yes,
        not goal_cannot_loop_or_throw(EarlierGoal)
    then
        LaterCanFail = cannot_fail
    else
        true
    ),
    % Don't convert (can_fail, can_loop) into (can_loop, can_fail), since
    % this could worsen the termination properties of the program.
    (
        EarlierCanFail = can_fail,
        goal_cannot_loop_or_throw_term_info(ModuleInfo, LaterGoal)
    ;
        EarlierCanFail = cannot_fail
    ).

reordering_maintains_termination(FullyStrict, EarlierGoal, LaterGoal,
        MaintainsTermination, !ModuleInfo) :-
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    LaterGoal = hlds_goal(_, LaterGoalInfo),

    EarlierDetism = goal_info_get_determinism(EarlierGoalInfo),
    determinism_components(EarlierDetism, EarlierCanFail, _),
    LaterDetism = goal_info_get_determinism(LaterGoalInfo),
    determinism_components(LaterDetism, LaterCanFail, _),

    % If --fully-strict was specified, don't convert (can_loop, can_fail) into
    % (can_fail, can_loop).
    goal_can_loop_or_throw_imaf(EarlierGoal, EarlierCanLoopOrThrow,
        !ModuleInfo),
    ( if
        FullyStrict = yes,
        EarlierCanLoopOrThrow = can_loop_or_throw,
        LaterCanFail = can_fail
    then
        MaintainsTermination = reorder_does_not_maintain_termination
    else
        % Don't convert (can_fail, can_loop) into (can_loop, can_fail), since
        % this could worsen the termination properties of the program.
        goal_can_loop_or_throw_imaf(LaterGoal, LaterCanLoopOrThrow,
            !ModuleInfo),
        ( if
            EarlierCanFail = can_fail,
            LaterCanLoopOrThrow = can_loop_or_throw
        then
            MaintainsTermination = reorder_does_not_maintain_termination
        else
            MaintainsTermination = reorder_maintains_termination
        )
    ).

    % If the earlier goal changes the instantiatedness of a variable
    % that is used in the later goal, then the later goal depends on
    % the earlier goal.
    %
    % This code does work on the alias branch.
    %
:- pred goal_depends_on_earlier_goal(module_info::in, var_table::in,
    hlds_goal::in, hlds_goal::in, instmap::in) is semidet.

goal_depends_on_earlier_goal(ModuleInfo, VarTable,
        LaterGoal, EarlierGoal, InstMapBeforeEarlierGoal) :-
    LaterGoal = hlds_goal(_, LaterGoalInfo),
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    EarlierInstMapDelta = goal_info_get_instmap_delta(EarlierGoalInfo),
    apply_instmap_delta(EarlierInstMapDelta,
        InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal),
    instmap_changed_vars(ModuleInfo, VarTable,
        InstMapBeforeEarlierGoal, InstMapAfterEarlierGoal,
        EarlierChangedVars),
    LaterGoalNonLocals = goal_info_get_nonlocals(LaterGoalInfo),
    set_of_var.intersect(EarlierChangedVars, LaterGoalNonLocals, Intersection),
    not set_of_var.is_empty(Intersection).

%-----------------------------------------------------------------------------%

generate_plain_call(ModuleInfo, PredOrFunc, ModuleName, ProcName,
        TIArgVars, NonTIArgVars, InstMapDelta0, ModeNo, Detism, Purity,
        Features, Context, Goal) :-
    PredFormArity = arg_list_arity(NonTIArgVars),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, UserArity, ModeNo, PredId, ProcId),

    % builtin_state only uses this to work out whether
    % this is the "recursive" clause generated for the compiler
    % for each builtin, so an invalid pred_id won't cause problems.
    InvalidPredId = invalid_pred_id,
    BuiltinState = builtin_state(ModuleInfo, InvalidPredId, PredId, ProcId),

    ArgVars = TIArgVars ++ NonTIArgVars,
    GoalExpr = plain_call(PredId, ProcId, ArgVars, BuiltinState, no,
        qualified(ModuleName, ProcName)),
    set_of_var.list_to_set(ArgVars, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    (
        NumSolns = at_most_zero,
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        ( NumSolns = at_most_one
        ; NumSolns = at_most_many
        ; NumSolns = at_most_many_cc
        ),
        InstMapDelta = InstMapDelta0
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, PredPurity),
    expect(unify(Purity, PredPurity), $pred, "purity disagreement"),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, Context,
        GoalInfo0),
    list.foldl(goal_info_add_feature, Features, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_call_foreign_proc(ModuleInfo, PredOrFunc, ModuleName, ProcName,
        TIArgs, NonTIArgs, ExtraArgs, InstMapDelta0, ModeNo, Detism, Purity,
        Features, Attributes, MaybeTraceRuntimeCond, Code, Context, Goal) :-
    PredFormArity = arg_list_arity(NonTIArgs),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    lookup_builtin_pred_proc_id(ModuleInfo, ModuleName, ProcName,
        PredOrFunc, UserArity, ModeNo, PredId, ProcId),

    Args = TIArgs ++ NonTIArgs,
    GoalExpr = call_foreign_proc(Attributes, PredId, ProcId, Args, ExtraArgs,
        MaybeTraceRuntimeCond, fp_impl_ordinary(Code, no)),
    ArgVars = list.map(foreign_arg_var, Args),
    ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
    Vars = ArgVars ++ ExtraArgVars,
    set_of_var.list_to_set(Vars, NonLocals),
    determinism_components(Detism, _CanFail, NumSolns),
    (
        NumSolns = at_most_zero,
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        ( NumSolns = at_most_one
        ; NumSolns = at_most_many
        ; NumSolns = at_most_many_cc
        ),
        InstMapDelta = InstMapDelta0
    ),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_purity(PredInfo, PredPurity),
    expect(unify(Purity, PredPurity), $pred, "purity disagreement"),
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
    set_of_var.list_to_set([InArg, OutArg], NonLocals),
    InstMapDelta = instmap_delta_from_assoc_list([OutArg - OutInst]),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),
    GoalExpr = generic_call(cast(CastType), [InArg, OutArg],
        [in_mode(InInst), out_mode(OutInst)], arg_reg_types_unset, detism_det),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

foreign_proc_uses_variable(Impl, VarName) :-
    Impl = fp_impl_ordinary(ForeignBody, _),
    string.sub_string_search(ForeignBody, VarName, _).

%-----------------------------------------------------------------------------%

maybe_strip_equality_pretest(Goal0) = Goal :-
    % The if_then_else constructed by unify_proc is sometimes wrapped up
    % in conjunctions.
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
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
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        ( if goal_info_has_feature(GoalInfo0, feature_pretest_equality) then
            Goal = Else0
        else
            Cond = maybe_strip_equality_pretest(Cond0),
            Then = maybe_strip_equality_pretest(Then0),
            Else = maybe_strip_equality_pretest(Else0),
            GoalExpr = if_then_else(Vars, Cond, Then, Else),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = negation(SubGoal0),
        SubGoal = maybe_strip_equality_pretest(SubGoal0),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Goal = Goal0
        else
            SubGoal = maybe_strip_equality_pretest(SubGoal0),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            MainGoal = maybe_strip_equality_pretest(MainGoal0),
            OrElseGoals = list.map(maybe_strip_equality_pretest, OrElseGoals0),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners),
            GoalExpr = shorthand(ShortHand),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            SubGoal = maybe_strip_equality_pretest(SubGoal0),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ShortHand0 = bi_implication(_, _),
            unexpected($pred, "bi_implication")
        )
    ).

:- func maybe_strip_equality_pretest_case(case) = case.

maybe_strip_equality_pretest_case(Case0) = Case :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    Goal = maybe_strip_equality_pretest(Goal0),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

maybe_transform_goal_at_goal_path(TransformP, TargetGoalPath,
        Goal0, MaybeGoal) :-
    (
        TargetGoalPath = fgp_nil,
        TransformP(Goal0, MaybeGoal0),
        maybe_error_to_maybe_transformed_goal(MaybeGoal0, MaybeGoal)
    ;
        TargetGoalPath = fgp_cons(FirstStep, LaterPath),
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        (
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            % This search should never reach an atomic goal.
            MaybeGoal = goal_not_found
        ;
            GoalExpr0 = conj(ConjType, Conjs0),
            ( if
                FirstStep = step_conj(ConjNum),
                list.index1(Conjs0, ConjNum, Conj0)
            then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    Conj0, MaybeConj),
                (
                    MaybeConj = ok(Conj),
                    list.det_replace_nth(Conjs0, ConjNum, Conj, Conjs),
                    GoalExpr = conj(ConjType, Conjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeConj = error(_)
                    ; MaybeConj = goal_not_found
                    ),
                    MaybeGoal = MaybeConj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = disj(Disjs0),
            ( if
                FirstStep = step_disj(DisjNum),
                list.index1(Disjs0, DisjNum, Disj0)
            then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    Disj0, MaybeDisj),
                (
                    MaybeDisj = ok(Disj),
                    list.det_replace_nth(Disjs0, DisjNum, Disj, Disjs),
                    GoalExpr = disj(Disjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeDisj = error(_)
                    ; MaybeDisj = goal_not_found
                    ),
                    MaybeGoal = MaybeDisj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            ( if
                FirstStep = step_switch(CaseNum, _MaybeNumConstructors),
                list.index1(Cases0, CaseNum, Case0)
            then
                CaseGoal0 = Case0 ^ case_goal,
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    CaseGoal0, MaybeCaseGoal),
                (
                    MaybeCaseGoal = ok(CaseGoal),
                    Case = Case0 ^ case_goal := CaseGoal,
                    list.det_replace_nth(Cases0, CaseNum, Case, Cases),
                    GoalExpr = switch(Var, CanFail, Cases),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCaseGoal = error(_)
                    ; MaybeCaseGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeCaseGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = negation(SubGoal0),
            ( if FirstStep = step_neg then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = negation(SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            ( if FirstStep = step_scope(_MaybeCut) then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = scope(Reason, SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = if_then_else(ExistVars, Cond0, Then0, Else0),
            ( if FirstStep = step_ite_cond then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    Cond0, MaybeCond),
                (
                    MaybeCond = ok(Cond),
                    GoalExpr = if_then_else(ExistVars, Cond, Then0, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCond = error(_)
                    ; MaybeCond = goal_not_found
                    ),
                    MaybeGoal = MaybeCond
                )
            else if FirstStep = step_ite_then then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    Then0, MaybeThen),
                (
                    MaybeThen = ok(Then),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeThen = error(_)
                    ; MaybeThen = goal_not_found
                    ),
                    MaybeGoal = MaybeThen
                )
            else if FirstStep = step_ite_else then
                maybe_transform_goal_at_goal_path(TransformP, LaterPath,
                    Else0, MaybeElse),
                (
                    MaybeElse = ok(Else),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then0, Else),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeElse = error(_)
                    ; MaybeElse = goal_not_found
                    ),
                    MaybeGoal = MaybeElse
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = shorthand(_),
            unexpected($pred, "shorthand")
        )
    ).

maybe_transform_goal_at_goal_path_with_instmap(TransformP, TargetGoalPath,
        Instmap0, Goal0, MaybeGoal) :-
    (
        TargetGoalPath = fgp_nil,
        TransformP(Instmap0, Goal0, MaybeGoal0),
        maybe_error_to_maybe_transformed_goal(MaybeGoal0, MaybeGoal)
    ;
        TargetGoalPath = fgp_cons(FirstStep, LaterPath),
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        (
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            % This search should never reach an atomic goal.
            MaybeGoal = goal_not_found
        ;
            GoalExpr0 = conj(ConjType, Conjs0),
            ( if
                FirstStep = step_conj(ConjNum),
                list.index1(Conjs0, ConjNum, Conj0)
            then
                list.take_upto(ConjNum - 1, Conjs0, HeadConjs),
                HeadInstdeltas = map(
                    (func(G) = goal_info_get_instmap_delta(G ^ hg_info)),
                    HeadConjs),
                list.foldl(apply_instmap_delta, HeadInstdeltas,
                    Instmap0, Instmap),
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap, Conj0, MaybeConj),
                (
                    MaybeConj = ok(Conj),
                    list.det_replace_nth(Conjs0, ConjNum, Conj, Conjs),
                    GoalExpr = conj(ConjType, Conjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeConj = error(_)
                    ; MaybeConj = goal_not_found
                    ),
                    MaybeGoal = MaybeConj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = disj(Disjs0),
            ( if
                FirstStep = step_disj(DisjNum),
                list.index1(Disjs0, DisjNum, Disj0)
            then
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap0, Disj0, MaybeDisj),
                (
                    MaybeDisj = ok(Disj),
                    list.det_replace_nth(Disjs0, DisjNum, Disj, Disjs),
                    GoalExpr = disj(Disjs),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeDisj = error(_)
                    ; MaybeDisj = goal_not_found
                    ),
                    MaybeGoal = MaybeDisj
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = switch(Var, CanFail, Cases0),
            ( if
                FirstStep = step_switch(CaseNum, _MaybeNumConstructors),
                list.index1(Cases0, CaseNum, Case0)
            then
                CaseGoal0 = Case0 ^ case_goal,
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap0, CaseGoal0, MaybeCaseGoal),
                (
                    MaybeCaseGoal = ok(CaseGoal),
                    Case = Case0 ^ case_goal := CaseGoal,
                    list.det_replace_nth(Cases0, CaseNum, Case, Cases),
                    GoalExpr = switch(Var, CanFail, Cases),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCaseGoal = error(_)
                    ; MaybeCaseGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeCaseGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = negation(SubGoal0),
            ( if FirstStep = step_neg then
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap0, SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = negation(SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            ( if FirstStep = step_scope(_MaybeCut) then
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap0, SubGoal0, MaybeSubGoal),
                (
                    MaybeSubGoal = ok(SubGoal),
                    GoalExpr = scope(Reason, SubGoal),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeSubGoal = error(_)
                    ; MaybeSubGoal = goal_not_found
                    ),
                    MaybeGoal = MaybeSubGoal
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = if_then_else(ExistVars, Cond0, Then0, Else0),
            ( if FirstStep = step_ite_cond then
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap0, Cond0, MaybeCond),
                (
                    MaybeCond = ok(Cond),
                    GoalExpr = if_then_else(ExistVars, Cond, Then0, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeCond = error(_)
                    ; MaybeCond = goal_not_found
                    ),
                    MaybeGoal = MaybeCond
                )
            else if FirstStep = step_ite_then then
                apply_instmap_delta(
                    goal_info_get_instmap_delta(Cond0 ^ hg_info),
                    Instmap0, Instmap),
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap, Then0, MaybeThen),
                (
                    MaybeThen = ok(Then),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then, Else0),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeThen = error(_)
                    ; MaybeThen = goal_not_found
                    ),
                    MaybeGoal = MaybeThen
                )
            else if FirstStep = step_ite_else then
                maybe_transform_goal_at_goal_path_with_instmap(TransformP,
                    LaterPath, Instmap0, Else0, MaybeElse),
                (
                    MaybeElse = ok(Else),
                    GoalExpr = if_then_else(ExistVars, Cond0, Then0, Else),
                    MaybeGoal = ok(hlds_goal(GoalExpr, GoalInfo0))
                ;
                    ( MaybeElse = error(_)
                    ; MaybeElse = goal_not_found
                    ),
                    MaybeGoal = MaybeElse
                )
            else
                MaybeGoal = goal_not_found
            )
        ;
            GoalExpr0 = shorthand(_),
            unexpected($pred, "shorthand")
        )
    ).

:- pred maybe_error_to_maybe_transformed_goal(maybe_error(hlds_goal)::in,
    maybe_transformed_goal::out) is det.

maybe_error_to_maybe_transformed_goal(ok(Goal), ok(Goal)).
maybe_error_to_maybe_transformed_goal(error(Error), error(Error)).

transform_all_goals(TransformP, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Conjs0),
        list.map(transform_all_goals(TransformP), Conjs0, Conjs),
        GoalExpr = conj(ConjType, Conjs)
    ;
        GoalExpr0 = disj(Disjs0),
        list.map(transform_all_goals(TransformP), Disjs0, Disjs),
        GoalExpr = disj(Disjs)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(
            ( pred(Case0::in, Case::out) is det :-
                GoalI0 = Case0 ^ case_goal,
                transform_all_goals(TransformP, GoalI0, GoalI),
                Case = Case0 ^ case_goal := GoalI
            ), Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        transform_all_goals(TransformP, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        transform_all_goals(TransformP, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = if_then_else(ExistVars, Cond0, Then0, Else0),
        transform_all_goals(TransformP, Cond0, Cond),
        transform_all_goals(TransformP, Then0, Then),
        transform_all_goals(TransformP, Else0, Else),
        GoalExpr = if_then_else(ExistVars, Cond, Then, Else)
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),
    Goal1 = hlds_goal(GoalExpr, GoalInfo0),
    TransformP(Goal1, Goal).

%-----------------------------------------------------------------------------%
:- end_module hlds.goal_util.
%-----------------------------------------------------------------------------%
