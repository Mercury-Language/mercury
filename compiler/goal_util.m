%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015-2019, 2021-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: goal_util.m.
% Main author: conway.
%
% Utility operations dealing with single goals.
% (Utility operations dealing with lists of goals are in goal_list_util.m.)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.goal_util.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module list.
:- import_module maybe.
:- import_module term_context.

%---------------------------------------------------------------------------%

    % Given a goal (or its goal_info) and an initial instmap,
    % compute the final instmap that results from the initial instmap
    % after execution of the goal.
    %
:- pred apply_goal_instmap_delta(hlds_goal::in,
    instmap::in, instmap::out) is det.
:- pred apply_goal_info_instmap_delta(hlds_goal_info::in,
    instmap::in, instmap::out) is det.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

:- type is_first_disjunct
    --->    is_first_disjunct
    ;       is_not_first_disjunct.

%---------------------------------------------------------------------------%

:- type is_leaf
    --->    is_leaf
    ;       is_not_leaf.

    % See whether the given procedure body is that of a leaf procedure.
    %
:- func proc_body_is_leaf(hlds_goal) = is_leaf.

%---------------------------------------------------------------------------%

:- type goal_is_atomic
    --->    goal_is_atomic
    ;       goal_is_nonatomic.

    % Returns whether a goal is atomic. This is undefined for shorthand goals.
    %
:- pred goal_is_atomic(hlds_goal::in, goal_is_atomic::out) is det.

    % See whether the goal is a branched structure.
    %
:- pred goal_is_branched(hlds_goal_expr::in) is semidet.

%---------------------------------------------------------------------------%

    % Return an indication of the size of the goal.
    %
:- pred goal_size(hlds_goal::in, int::out) is det.

    % Return an indication of the size of the list of goals.
    %
:- pred goals_size(list(hlds_goal)::in, int::out) is det.

    % Return an indication of the size of the list of clauses.
    %
:- pred clause_list_size(list(clause)::in, int::out) is det.

%---------------------------------------------------------------------------%

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
    foreign_proc_attributes::in, maybe(trace_expr(trace_runtime))::in,
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

%---------------------------------------------------------------------------%

:- pred foreign_proc_uses_variable(pragma_foreign_proc_impl::in, string::in)
    is semidet.

%---------------------------------------------------------------------------%

    % Negate a goal, eliminating double negations as we go.
    %
:- pred negate_goal(hlds_goal::in, hlds_goal_info::in, hlds_goal::out) is det.

%---------------------------------------------------------------------------%

    % Change the contexts of the goal_infos of all the sub-goals
    % of the given goal. This is used to ensure that error messages
    % for automatically generated unification procedures have a useful
    % context.
    %
:- pred set_goal_contexts(prog_context::in, hlds_goal::in, hlds_goal::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_pred_tests.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

apply_goal_instmap_delta(hlds_goal(_GoalExpr0, GoalInfo0), !InstMap) :-
    DeltaInstMap = goal_info_get_instmap_delta(GoalInfo0),
    apply_instmap_delta(DeltaInstMap, !InstMap).

apply_goal_info_instmap_delta(GoalInfo0, !InstMap) :-
    DeltaInstMap = goal_info_get_instmap_delta(GoalInfo0),
    apply_instmap_delta(DeltaInstMap, !InstMap).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

goal_is_branched(GoalExpr) :-
    require_complete_switch [GoalExpr]
    (
        ( GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = disj(_)
        )
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = conj(_, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ; GoalExpr = shorthand(_)
        ),
        fail
    ).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

foreign_proc_uses_variable(Impl, VarName) :-
    Impl = fp_impl_ordinary(ForeignBody, _),
    string.sub_string_search(ForeignBody, VarName, _).

%---------------------------------------------------------------------------%

negate_goal(Goal, GoalInfo, NegatedGoal) :-
    ( if
        % Eliminate double negations.
        Goal = hlds_goal(negation(Goal1), _)
    then
        NegatedGoal = Goal1
    else if
        % Convert negated conjunctions of negations into disjunctions.
        Goal = hlds_goal(conj(plain_conj, NegatedGoals), _),
        all_negated(NegatedGoals, UnnegatedGoals)
    then
        NegatedGoal = hlds_goal(disj(UnnegatedGoals), GoalInfo)
    else
        NegatedGoal = hlds_goal(negation(Goal), GoalInfo)
    ).

:- pred all_negated(list(hlds_goal)::in, list(hlds_goal)::out) is semidet.

all_negated([], []).
all_negated([hlds_goal(negation(Goal), _) | NegatedGoals], [Goal | Goals]) :-
    all_negated(NegatedGoals, Goals).
all_negated([hlds_goal(conj(plain_conj, NegatedConj), _) | NegatedGoals],
        Goals) :-
    all_negated(NegatedConj, Goals1),
    all_negated(NegatedGoals, Goals2),
    Goals = Goals1 ++ Goals2.

%---------------------------------------------------------------------------%

set_goal_contexts(Context, Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo),
    (
        GoalExpr0 = conj(ConjType, SubGoals0),
        list.map(set_goal_contexts(Context), SubGoals0, SubGoals),
        GoalExpr = conj(ConjType, SubGoals)
    ;
        GoalExpr0 = disj(SubGoals0),
        list.map(set_goal_contexts(Context), SubGoals0, SubGoals),
        GoalExpr = disj(SubGoals)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        set_goal_contexts(Context, Cond0, Cond),
        set_goal_contexts(Context, Then0, Then),
        set_goal_contexts(Context, Else0, Else),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map(set_case_contexts(Context), Cases0, Cases),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        set_goal_contexts(Context, SubGoal0, SubGoal),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = negation(SubGoal0),
        set_goal_contexts(Context, SubGoal0, SubGoal),
        GoalExpr = negation(SubGoal)
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners),
            set_goal_contexts(Context, MainGoal0, MainGoal),
            list.map(set_goal_contexts(Context), OrElseGoals0, OrElseGoals),
            ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            set_goal_contexts(Context, SubGoal0, SubGoal),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ;
            ShortHand0 = bi_implication(LHS0, RHS0),
            set_goal_contexts(Context, LHS0, LHS),
            set_goal_contexts(Context, RHS0, RHS),
            ShortHand = bi_implication(LHS, RHS)
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred set_case_contexts(prog_context::in, case::in, case::out) is det.

set_case_contexts(Context, Case0, Case) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_goal_contexts(Context, Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%---------------------------------------------------------------------------%
:- end_module hlds.goal_util.
%---------------------------------------------------------------------------%
