%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: quantification.m.
% Main authors: fjh, conway.
%
% Make implicit quantification explicit, and rename apart variables with the
% same name that appear in distinct scopes. For the rules on implicit
% quantification, see the Mercury language reference manual.
%
% This pass also expands out bi-implications (that has to be done after
% quantification, and preferably as soon as possible, so we do it here).
%
% Rather than making implicit quantification explicit by inserting additional
% existential quantifiers in the form of `some/2' goals, we instead record
% existential quantification in the goal_info for each goal. In fact we could
% (and maybe even should?) even delete any explicit existential quantifiers
% that were present in the source code, since the information they convey will
% be stored in the goal_info. We currently don't do that.
%
% The important piece of information that later stages of the compiler want to
% know is "Does this goal bind any of its nonlocal variables?". So, rather
% than storing a list of the variables which _are_ existentially quantified in
% the goal_info, we store the set of variables which are _not_ quantified.
%
%---------------------------------------------------------------------------%

:- module hlds.quantification.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module parse_tree.vartypes.

:- import_module list.

%---------------------------------------------------------------------------%

    % Quantification can detect some situations (currently just one)
    % that users deserve warnings about. The reason we return warnings
    % in the form of quant_warnings, which must be converted to error_specs
    % by add_quant_warnings in make_hlds_warn.m, is that most invocations
    % of quantification are AFTER semantic analysis, and as such, they
    % do not report any warnings. Throwing away cheaply-built quant_warnings
    % is much less of a waste than throwing away relatively expensively-built
    % error_specs.
:- type quant_warning
    --->    warn_overlap(list(prog_var), prog_context).

    % When the compiler performs structure reuse, using the ordinary nonlocals
    % during code generation causes variables taken from the reused cell in
    % a reconstruction to be extracted and possibly stored on the stack
    % unnecessarily.
    %
    % For the example below, the variables `B' ... `H' are extracted
    % from the term and stored on the stack across the call.
    %
    % To avoid this, the compiler computes a set of `code-gen nonlocals'
    % which are the same as the ordinary nonlocals, except that the variables
    % taken from the reused cell are considered to be local to the goal.
    % No renaming is performed when computing the code-gen nonlocals
    % to avoid stuffing up the ordinary nonlocals.
    %
    % Mode information is always computed using the ordinary nonlocals.
    %
    % :- pred update(X::in, foo::di, foo::uo) is det.
    % update(A0, Foo0, Foo) :-
    %   Foo0 = foo(_, B, C, D, E, F, G, H),
    %   some_call(A0, A),
    %   Foo0 = foo(A, B, C, D, E, F, G, H).
    %
:- type nonlocals_to_recompute
    --->    ord_nl_maybe_lambda     % ordinary nonlocals computation
    ;       ord_nl_no_lambda        % ordinary nonlocals computation
    ;       cg_nl_no_lambda.        % code gen nonlocals computation

:- pred requantify_proc_general(nonlocals_to_recompute::in,
    proc_info::in, proc_info::out) is det.

:- pred implicitly_quantify_clause_body_general_vs(nonlocals_to_recompute::in,
    list(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.
:- pred implicitly_quantify_clause_body_general(nonlocals_to_recompute::in,
    list(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, var_table::in, var_table::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

:- pred implicitly_quantify_goal_general(nonlocals_to_recompute::in,
    set_of_progvar::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, var_table::in, var_table::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

    % free_goal_vars(Goal) = Vars:
    %
    % Vars is the set of variables that occur free (unquantified) in Goal.
    %
:- func free_goal_vars(hlds_goal) = set_of_progvar.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- inst ord_nl_maybe_lambda for nonlocals_to_recompute/0
    --->    ord_nl_maybe_lambda.
:- inst ord_nl_no_lambda for nonlocals_to_recompute/0
    --->    ord_nl_no_lambda.
:- inst cg_nl_no_lambda for nonlocals_to_recompute/0
    --->    cg_nl_no_lambda.

%---------------------------------------------------------------------------%

requantify_proc_general(NonLocalsToRecompute, !ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarmaps0),
    implicitly_quantify_clause_body_general(NonLocalsToRecompute,
        HeadVars, _, Goal0, Goal, VarTable0, VarTable,
        RttiVarmaps0, RttiVarmaps),
    proc_info_set_var_table(VarTable, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarmaps, !ProcInfo).

%---------------------%

implicitly_quantify_clause_body_general_vs(NonLocalsToRecompute, HeadVars,
        Warnings, !Goal, !VarSet, !VarTypes, !RttiVarMaps) :-
    OutsideVars = set_of_var.list_to_set(HeadVars),
    implicitly_quantify_goal_general_vs(NonLocalsToRecompute, OutsideVars,
        Warnings, !Goal, !VarSet, !VarTypes, !RttiVarMaps).

implicitly_quantify_clause_body_general(NonLocalsToRecompute, HeadVars,
        Warnings, !Goal, !VarTable, !RttiVarMaps) :-
    OutsideVars = set_of_var.list_to_set(HeadVars),
    implicitly_quantify_goal_general(NonLocalsToRecompute, OutsideVars,
        Warnings, !Goal, !VarTable, !RttiVarMaps).

%---------------------%

:- pred implicitly_quantify_goal_general_vs(nonlocals_to_recompute::in,
    set_of_progvar::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

implicitly_quantify_goal_general_vs(NonLocalsToRecompute, OutsideVars,
        Warnings, !Goal, !VarSet, !VarTypes, !RttiVarMaps) :-
    (
        NonLocalsToRecompute = ord_nl_maybe_lambda,
        implicitly_quantify_goal_vs_2(ord_nl_maybe_lambda,
            OutsideVars, Warnings, !Goal, !VarSet, !VarTypes, !RttiVarMaps)
    ;
        ( NonLocalsToRecompute = ord_nl_no_lambda
        ; NonLocalsToRecompute = cg_nl_no_lambda
        ),
        implicitly_quantify_goal_vs_2(ord_nl_no_lambda,
            OutsideVars, Warnings, !Goal, !VarSet, !VarTypes, !RttiVarMaps)
    ),
    ( if
        NonLocalsToRecompute = cg_nl_no_lambda,
        % If the goal does not contain a reconstruction, the code-gen nonlocals
        % and the ordinary nonlocals are the same.
        goal_contains_reconstruction(!.Goal, yes)
    then
        implicitly_quantify_goal_vs_2(cg_nl_no_lambda,
            OutsideVars, _, !Goal, !VarSet, !VarTypes, !RttiVarMaps)
    else
        true
    ).

implicitly_quantify_goal_general(NonLocalsToRecompute, OutsideVars,
        Warnings, !Goal, !VarTable, !RttiVarMaps) :-
    (
        NonLocalsToRecompute = ord_nl_maybe_lambda,
        implicitly_quantify_goal_2(ord_nl_maybe_lambda,
            OutsideVars, Warnings, !Goal, !VarTable, !RttiVarMaps)
    ;
        ( NonLocalsToRecompute = ord_nl_no_lambda
        ; NonLocalsToRecompute = cg_nl_no_lambda
        ),
        implicitly_quantify_goal_2(ord_nl_no_lambda,
            OutsideVars, Warnings, !Goal, !VarTable, !RttiVarMaps)
    ),
    ( if
        NonLocalsToRecompute = cg_nl_no_lambda,
        % If the goal does not contain a reconstruction, the code-gen nonlocals
        % and the ordinary nonlocals are the same.
        goal_contains_reconstruction(!.Goal, yes)
    then
        implicitly_quantify_goal_2(cg_nl_no_lambda,
            OutsideVars, _, !Goal, !VarTable, !RttiVarMaps)
    else
        true
    ).

%---------------------%

:- pred implicitly_quantify_goal_vs_2(nonlocals_to_recompute,
    set_of_progvar, list(quant_warning),
    hlds_goal, hlds_goal, prog_varset, prog_varset,
    vartypes, vartypes, rtti_varmaps, rtti_varmaps).
:- mode implicitly_quantify_goal_vs_2(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode implicitly_quantify_goal_vs_2(in(ord_nl_no_lambda),
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode implicitly_quantify_goal_vs_2(in(cg_nl_no_lambda),
    in, out, in, out, in, out, in, out, in, out) is det.

implicitly_quantify_goal_vs_2(NonLocalsToRecompute, OutsideVars, Warnings,
        !Goal, !VarSet, !VarTypes, !RttiVarMaps) :-
    VarSetTypes0 = prog_var_set_types(!.VarSet, !.VarTypes),
    VarDb0 = var_db_varset_vartypes(VarSetTypes0),
    init_quant_info(OutsideVars, VarDb0, !.RttiVarMaps, QuantInfo0),
    quantify_goal(NonLocalsToRecompute, !Goal, QuantInfo0, QuantInfo),
    get_var_db(QuantInfo, VarDb),
    (
        VarDb = var_db_varset_vartypes(VarSetTypes),
        VarSetTypes = prog_var_set_types(!:VarSet, !:VarTypes)
    ;
        VarDb = var_db_var_table(_),
        unexpected($pred, "var_db_var_table")
    ),
    get_warnings(QuantInfo, Warnings0),
    get_rtti_varmaps(QuantInfo, !:RttiVarMaps),
    list.reverse(Warnings0, Warnings).

:- pred implicitly_quantify_goal_2(nonlocals_to_recompute,
    set_of_progvar, list(quant_warning),
    hlds_goal, hlds_goal,
    var_table, var_table, rtti_varmaps, rtti_varmaps).
:- mode implicitly_quantify_goal_2(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out, in, out) is det.
:- mode implicitly_quantify_goal_2(in(ord_nl_no_lambda),
    in, out, in, out, in, out, in, out) is det.
:- mode implicitly_quantify_goal_2(in(cg_nl_no_lambda),
    in, out, in, out, in, out, in, out) is det.

implicitly_quantify_goal_2(NonLocalsToRecompute, OutsideVars, Warnings,
        !Goal, !VarTable, !RttiVarMaps) :-
    VarDb0 = var_db_var_table(!.VarTable),
    init_quant_info(OutsideVars, VarDb0, !.RttiVarMaps, QuantInfo0),
    quantify_goal(NonLocalsToRecompute, !Goal, QuantInfo0, QuantInfo),
    get_var_db(QuantInfo, VarDb),
    (
        VarDb = var_db_varset_vartypes(_),
        unexpected($pred, "var_db_varset_vartypes")
    ;
        VarDb = var_db_var_table(!:VarTable)
    ),
    get_warnings(QuantInfo, Warnings0),
    get_rtti_varmaps(QuantInfo, !:RttiVarMaps),
    list.reverse(Warnings0, Warnings).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred quantify_goal(nonlocals_to_recompute, hlds_goal, hlds_goal,
    quant_info, quant_info).
:- mode quantify_goal(in(ord_nl_maybe_lambda),
    in, out, in, out) is det.
:- mode quantify_goal(in(ord_nl_no_lambda),
    in, out, in, out) is det.
:- mode quantify_goal(in(cg_nl_no_lambda),
    in, out, in, out) is det.

quantify_goal(NonLocalsToRecompute, Goal0, Goal, !Info) :-
    some [!GoalExpr, !GoalInfo] (
        Goal0 = hlds_goal(!:GoalExpr, !:GoalInfo),
        get_seen(!.Info, SeenVars),
        quantify_goal_expr(NonLocalsToRecompute, !GoalExpr, !.GoalInfo,
            PossiblyNonLocalGoalVars0, !Info),
        get_nonlocals(!.Info, NonLocalVars),
        set_of_var.difference(PossiblyNonLocalGoalVars0, NonLocalVars,
            LocalVars),
        set_of_var.intersect(SeenVars, LocalVars, RenameVars),
        % If there are any variables that are local to the goal
        % which we have come across before, then we rename them apart.
        ( if set_of_var.is_empty(RenameVars) then
            true
        else
            rename_vars_apart(NonLocalsToRecompute, RenameVars, RenameMap,
                hlds_goal(!.GoalExpr, !.GoalInfo),
                hlds_goal(!:GoalExpr, !:GoalInfo),
                !Info),

            % Make sure that the information in the RTTI varmaps is updated
            % to reflect any new variables that we may have just introduced.

            some [!RttiVarMaps] (
                get_rtti_varmaps(!.Info, !:RttiVarMaps),
                map.foldl(rtti_var_info_duplicate, RenameMap, !RttiVarMaps),
                set_rtti_varmaps(!.RttiVarMaps, !Info)
            )
        ),
        set_goal_nonlocals(NonLocalsToRecompute, NonLocalVars,
            !GoalInfo, !Info),

        % If the nonlocals set has shrunk (e.g. because some optimization
        % optimizes away the other occurrences of a variable, causing it
        % to become local when previously it was nonlocal),
        % then we may need to likewise shrink the instmap delta.

        InstMapDelta0 = goal_info_get_instmap_delta(!.GoalInfo),
        instmap_delta_restrict(NonLocalVars, InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, !GoalInfo),
        Goal = hlds_goal(!.GoalExpr, !.GoalInfo)
    ).

    % After this pass, explicit quantifiers are redundant, since all variables
    % which were explicitly quantified have been renamed apart. So we don't
    % keep them. We need to keep the structure, though, so that mode analysis
    % doesn't try to reorder through quantifiers. (Actually it would make sense
    % to allow mode analysis to do that, but the reference manual says it
    % doesn't, so we don't.) Thus we replace `scope(exist_quant(Vars), Goal0)'
    % with an empty quantifier `scope(exist_quant([]), Goal)'.
    %
    % We pass GoalInfo0 to allow warnings to have the proper context. We don't
    % pass the context itself to avoid the work of extracting the context from
    % goal_infos in the usual (no warning) case.
    %
:- pred quantify_goal_expr(nonlocals_to_recompute,
    hlds_goal_expr, hlds_goal_expr, hlds_goal_info, set_of_progvar,
    quant_info, quant_info).
:- mode quantify_goal_expr(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_goal_expr(in(ord_nl_no_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_goal_expr(in(cg_nl_no_lambda),
    in, out, in, out, in, out) is det.

quantify_goal_expr(NonLocalsToRecompute, GoalExpr0, GoalExpr, GoalInfo0,
        PossiblyNonLocalGoalVars0, !Info) :-
    (
        GoalExpr0 = unify(_Var, _UnifyRHS0, _Mode, _Unification0,
            _UnifyContext),
        quantify_goal_unify(NonLocalsToRecompute, GoalExpr0, GoalExpr,
            GoalInfo0, PossiblyNonLocalGoalVars0, !Info)
    ;
        GoalExpr0 = plain_call(_, _, HeadVars, _, _, _),
        GoalExpr = GoalExpr0,
        quantify_primitive_goal(HeadVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = generic_call(GenericCall, CallArgVars, _, _, _),
        GoalExpr = GoalExpr0,
        goal_util.generic_call_vars(GenericCall, GenericArgVars),
        ArgVars = GenericArgVars ++ CallArgVars,
        quantify_primitive_goal(ArgVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        GoalExpr = GoalExpr0,
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        quantify_primitive_goal(AllVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            NonLocalsToRecompute = ord_nl_maybe_lambda,
            quantify_conj_maybe_lambda(NonLocalsToRecompute,
                Goals0, Goals, PossiblyNonLocalGoalVars0, !Info),
            GoalExpr = conj(ConjType, Goals)
        ;
            ( NonLocalsToRecompute = ord_nl_no_lambda
            ; NonLocalsToRecompute = cg_nl_no_lambda
            ),
            quantify_conj_no_lambda(NonLocalsToRecompute,
                Goals0, Goals, PossiblyNonLocalGoalVars0, !Info),
            GoalExpr = conj(ConjType, Goals)
        )
    ;
        GoalExpr0 = disj(Goals0),
        NonLocalVarSets0 = [],
        quantify_disj(NonLocalsToRecompute, Goals0, Goals, !Info,
            NonLocalVarSets0, NonLocalVarSets),
        set_of_var.union_list(NonLocalVarSets, NonLocalVars),
        set_nonlocals(NonLocalVars, !Info),
        GoalExpr = disj(Goals),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        NonLocalVarSets0 = [],
        quantify_cases(NonLocalsToRecompute, Cases0, Cases, !Info,
            NonLocalVarSets0, NonLocalVarSets),
        % The switch variable is guaranteed to be nonlocal to the switch, since
        % it has to be bound elsewhere, so we put it in the nonlocals here.
        set_of_var.union_list(NonLocalVarSets, NonLocalVars0),
        set_of_var.insert(Var, NonLocalVars0, NonLocalVars),
        set_nonlocals(NonLocalVars, !Info),
        GoalExpr = switch(Var, Det, Cases),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = negation(SubGoal0),
        % Quantified variables cannot be pushed inside a negation, so we insert
        % the quantified vars into the outside vars set, and initialize the new
        % quantified vars set to be empty (the lambda outside vars remain
        % unchanged).
        get_quant_vars(!.Info, QuantVars),
        get_outside(!.Info, OutsideVars),
        set_of_var.union(OutsideVars, QuantVars, OutsideVars1),
        QuantVars1 = set_of_var.init,
        set_quant_vars(QuantVars1, !Info),
        set_outside(OutsideVars1, !Info),
        quantify_goal(NonLocalsToRecompute, SubGoal0, SubGoal, !Info),
        GoalExpr = negation(SubGoal),
        set_outside(OutsideVars, !Info),
        set_quant_vars(QuantVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = if_then_else(_Vars0, _Cond0, _Then0, _Else0),
        quantify_goal_ite(NonLocalsToRecompute, GoalExpr0, GoalExpr, GoalInfo0,
            PossiblyNonLocalGoalVars0, !Info)
    ;
        GoalExpr0 = scope(_Reason0, _SubGoal0),
        quantify_goal_scope(NonLocalsToRecompute, GoalExpr0, GoalExpr,
            GoalInfo0, PossiblyNonLocalGoalVars0, !Info)
    ;
        GoalExpr0 = shorthand(_ShortHand0),
        quantify_goal_shorthand(NonLocalsToRecompute, GoalExpr0, GoalExpr,
            GoalInfo0, PossiblyNonLocalGoalVars0, !Info)
    ).

:- pred quantify_goal_unify(nonlocals_to_recompute,
    hlds_goal_expr, hlds_goal_expr, hlds_goal_info, set_of_progvar,
    quant_info, quant_info).
:- mode quantify_goal_unify(in(ord_nl_maybe_lambda),
    in(goal_expr_unify), out, in, out, in, out) is det.
:- mode quantify_goal_unify(in(ord_nl_no_lambda),
    in(goal_expr_unify), out, in, out, in, out) is det.
:- mode quantify_goal_unify(in(cg_nl_no_lambda),
    in(goal_expr_unify), out, in, out, in, out) is det.
:- pragma inline(pred(quantify_goal_unify/7)).

quantify_goal_unify(NonLocalsToRecompute, GoalExpr0, GoalExpr, GoalInfo0,
        PossiblyNonLocalGoalVars0, !Info) :-
    GoalExpr0 = unify(Var, UnifyRHS0, Mode, Unification0, UnifyContext),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    some [!GoalVars] (
        set_of_var.make_singleton(Var, !:GoalVars),
        (
            Unification0 = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, SetArgs)),
                MaybeSetArgs = yes(SetArgs),
                set_of_var.insert(ReuseVar, !GoalVars)
            ;
                How = construct_in_region(RegionVar),
                MaybeSetArgs = no,
                set_of_var.insert(RegionVar, !GoalVars)
            ;
                ( How = construct_statically(_)
                ; How = construct_dynamically
                ),
                MaybeSetArgs = no
            ),
            ( if
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            then
                set_of_var.insert(SizeVar, !GoalVars)
            else
                true
            )
        ;
            ( Unification0 = deconstruct(_, _, _, _, _, _)
            ; Unification0 = assign(_, _)
            ; Unification0 = simple_test(_, _)
            ),
            MaybeSetArgs = no
        ;
            Unification0 = complicated_unify(_, _, TypeInfoVars),
            set_of_var.insert_list(TypeInfoVars, !GoalVars),
            MaybeSetArgs = no
        ),
        AllButRHSGoalVars = !.GoalVars
    ),

    quantify_unify_rhs(NonLocalsToRecompute, MaybeSetArgs,
        GoalInfo0, UnifyRHS0, UnifyRHS, Unification0, Unification,
        RHSGoalVars, !Info),
    GoalExpr = unify(Var, UnifyRHS, Mode, Unification, UnifyContext),
    set_of_var.union(AllButRHSGoalVars, RHSGoalVars, AllGoalVars),

    update_seen_vars(AllGoalVars, !Info),
    set_of_var.intersect(AllGoalVars, OutsideVars, ONonLocalVars),
    set_of_var.intersect(AllGoalVars, LambdaOutsideVars, LONonLocalVars),
    set_of_var.union(ONonLocalVars, LONonLocalVars, NonLocalVars),
    set_nonlocals(NonLocalVars, !Info),
    goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
        PossiblyNonLocalGoalVars0).

:- pred quantify_goal_ite(nonlocals_to_recompute,
    hlds_goal_expr, hlds_goal_expr, hlds_goal_info, set_of_progvar,
    quant_info, quant_info).
:- mode quantify_goal_ite(in(ord_nl_maybe_lambda),
    in(goal_expr_ite), out, in, out, in, out) is det.
:- mode quantify_goal_ite(in(ord_nl_no_lambda),
    in(goal_expr_ite), out, in, out, in, out) is det.
:- mode quantify_goal_ite(in(cg_nl_no_lambda),
    in(goal_expr_ite), out, in, out, in, out) is det.
:- pragma inline(pred(quantify_goal_ite/7)).

quantify_goal_ite(NonLocalsToRecompute, GoalExpr0, GoalExpr, GoalInfo0,
        PossiblyNonLocalGoalVars0, !Info) :-
    GoalExpr0 = if_then_else(Vars0, Cond0, Then0, Else0),
    % After this pass, explicit quantifiers are redundant, since all
    % variables which were explicitly quantified have been renamed apart.
    % So we don't keep them. Thus we replace `if_then_else(Vars, ....)'
    % with `if_then_else([], ...)'.

    get_quant_vars(!.Info, QuantVars0),
    get_outside(!.Info, OutsideVars0),
    get_lambda_outside(!.Info, LambdaOutsideVars0),
    (
        Vars0 = [],
        QVars = set_of_var.init,
        Cond1 = Cond0,
        Then1 = Then0,
        QuantVars1 = QuantVars0
    ;
        Vars0 = [_ | _],
        QVars = set_of_var.list_to_set(Vars0),
        % Rename apart those variables that are quantified to the cond and then
        % of the i-t-e that occur outside the i-t-e.
        set_of_var.intersect(OutsideVars0, QVars, RenameVars1),
        set_of_var.intersect(LambdaOutsideVars0, QVars, RenameVars2),
        set_of_var.union(RenameVars1, RenameVars2, RenameVars),
        ( if set_of_var.is_empty(RenameVars) then
            Cond1 = Cond0,
            Then1 = Then0,
            Vars = Vars0
        else
            Context = goal_info_get_context(GoalInfo0),
            warn_overlapping_scope(RenameVars, Context, !Info),
            rename_vars_apart(NonLocalsToRecompute, RenameVars, RenameMap,
                Cond0, Cond1, !Info),
            rename_some_vars_in_goal(RenameMap, Then0, Then1),
            rename_var_list(need_not_rename, RenameMap, Vars0, Vars)
        ),
        set_of_var.insert_list(Vars, QuantVars0, QuantVars1)
    ),
    (
        NonLocalsToRecompute = ord_nl_maybe_lambda,
        goal_vars_both_maybe_lambda(NonLocalsToRecompute, Then1,
            VarsThen, LambdaVarsThen)
    ;
        ( NonLocalsToRecompute = ord_nl_no_lambda
        ; NonLocalsToRecompute = cg_nl_no_lambda
        ),
        goal_vars_both_no_lambda(NonLocalsToRecompute, Then1, VarsThen),
        LambdaVarsThen = set_of_var.init
    ),
    set_of_var.union(OutsideVars0, VarsThen, OutsideVars1),
    set_of_var.union(LambdaOutsideVars0, LambdaVarsThen, LambdaOutsideVars1),
    set_quant_vars(QuantVars1, !Info),
    set_outside(OutsideVars1, !Info),
    set_lambda_outside(LambdaOutsideVars1, !Info),
    update_seen_vars(QVars, !Info),
    quantify_goal(NonLocalsToRecompute, Cond1, Cond, !Info),
    get_nonlocals(!.Info, NonLocalsCond),
    set_of_var.union(OutsideVars0, NonLocalsCond, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    set_lambda_outside(LambdaOutsideVars0, !Info),
    quantify_goal(NonLocalsToRecompute, Then1, Then, !Info),
    get_nonlocals(!.Info, NonLocalsThen),
    set_outside(OutsideVars0, !Info),
    set_quant_vars(QuantVars0, !Info),
    quantify_goal(NonLocalsToRecompute, Else0, Else, !Info),
    GoalExpr = if_then_else([], Cond, Then, Else),

    get_nonlocals(!.Info, NonLocalsElse),
    set_of_var.union(NonLocalsCond, NonLocalsThen, NonLocalsIfThen),
    set_of_var.union(NonLocalsIfThen, NonLocalsElse, NonLocalsIfThenElse),
    set_of_var.intersect(NonLocalsIfThenElse, OutsideVars0, NonLocalsO),
    set_of_var.intersect(NonLocalsIfThenElse, LambdaOutsideVars0,
        NonLocalsL),
    set_of_var.union(NonLocalsO, NonLocalsL, NonLocals),
    set_nonlocals(NonLocals, !Info),
    goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
        PossiblyNonLocalGoalVars0).

:- pred quantify_goal_scope(nonlocals_to_recompute,
    hlds_goal_expr, hlds_goal_expr, hlds_goal_info, set_of_progvar,
    quant_info, quant_info).
:- mode quantify_goal_scope(in(ord_nl_maybe_lambda),
    in(goal_expr_scope), out, in, out, in, out) is det.
:- mode quantify_goal_scope(in(ord_nl_no_lambda),
    in(goal_expr_scope), out, in, out, in, out) is det.
:- mode quantify_goal_scope(in(cg_nl_no_lambda),
    in(goal_expr_scope), out, in, out, in, out) is det.
:- pragma inline(pred(quantify_goal_scope/7)).

quantify_goal_scope(NonLocalsToRecompute, GoalExpr0, GoalExpr, GoalInfo0,
        PossiblyNonLocalGoalVars0, !Info) :-
    GoalExpr0 = scope(Reason0, SubGoal0),
    get_quant_vars(!.Info, QuantVars),
    (
        Reason0 = exist_quant(Vars0),
        Reason1 = exist_quant([]),
        quantify_goal_scope_rename_vars(NonLocalsToRecompute,
            Reason1, Reason, SubGoal0, SubGoal1, Vars0, Vars, GoalInfo0,
            !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0),
        quantify_goal(NonLocalsToRecompute, SubGoal1, SubGoal, !Info),
        GoalExpr = scope(Reason, SubGoal),
        get_nonlocals(!.Info, NonLocals0),
        set_of_var.delete_list(Vars, NonLocals0, NonLocals),
        set_nonlocals(NonLocals, !Info)
    ;
        Reason0 = from_ground_term(TermVar, FGT),
        (
            ( FGT = from_ground_term_initial
            ; FGT = from_ground_term_construct
            ),
            % Not quantifying the subgoal is a substantial speedup. It is ok
            % because superhomogeneous.m sets up the nonlocal sets of the
            % unifications, their conjunction, and the scope goal itself,
            % and the mode analysis pass that converted the kind from
            % from_ground_term_initial to from_ground_term_construct
            % has checked that the invariants we need do indeed apply.
            SubGoal = SubGoal0,
            get_outside(!.Info, OutsideVars),
            get_lambda_outside(!.Info, LambdaOutsideVars),
            ( if
                ( set_of_var.contains(OutsideVars, TermVar)
                ; set_of_var.contains(LambdaOutsideVars, TermVar)
                )
            then
                GoalExpr = scope(Reason0, SubGoal),
                NonLocals = set_of_var.make_singleton(TermVar)
            else
                (
                    FGT = from_ground_term_initial,
                    % We couldn't have invoked the modechecker yet, since
                    % that replaces from_ground_term_initial with one of
                    % the other fgt scope kinds. This means that we may not
                    % have invoked the typechecker yet either. If we
                    % replaced the scope with the empty conjunction,
                    % we would lose type information about the variables
                    % in the scope. This would lead to the failure of
                    % the hard_coded/type_qual.m test case.
                    GoalExpr = scope(Reason0, SubGoal)
                ;
                    FGT = from_ground_term_construct,
                    GoalExpr = conj(plain_conj, [])
                ),
                NonLocals = set_of_var.init
            ),
            set_nonlocals(NonLocals, !Info),
            PossiblyNonLocalGoalVars0 = NonLocals
        ;
            ( FGT = from_ground_term_deconstruct
            ; FGT = from_ground_term_other
            ),
            goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
                PossiblyNonLocalGoalVars0),
            quantify_goal(NonLocalsToRecompute, SubGoal0, SubGoal, !Info),
            GoalExpr = scope(Reason0, SubGoal)
        )
    ;
        ( Reason0 = disable_warnings(_, _)
        ; Reason0 = promise_purity(_)
        ; Reason0 = promise_solutions(_, _)
        ; Reason0 = require_detism(_)
        ; Reason0 = require_complete_switch(_)
        ; Reason0 = require_switch_arms_detism(_, _)
        ; Reason0 = commit(_)
        ; Reason0 = barrier(_)
        ; Reason0 = loop_control(_, _, _)
        ),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0),
        quantify_goal(NonLocalsToRecompute, SubGoal0, SubGoal, !Info),
        GoalExpr = scope(Reason0, SubGoal)
    ;
        Reason0 = trace_goal(_, _, _, _, Vars0),
        quantify_goal_scope_rename_vars(NonLocalsToRecompute, Reason0, Reason,
            SubGoal0, SubGoal1, Vars0, Vars, GoalInfo0, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0),
        quantify_goal(NonLocalsToRecompute, SubGoal1, SubGoal, !Info),
        GoalExpr = scope(Reason, SubGoal),
        get_nonlocals(!.Info, NonLocals0),
        set_of_var.delete_list(Vars, NonLocals0, NonLocals),
        set_nonlocals(NonLocals, !Info)
    ),
    set_quant_vars(QuantVars, !Info).

:- pred quantify_goal_scope_rename_vars(nonlocals_to_recompute,
    scope_reason, scope_reason, hlds_goal, hlds_goal,
    list(prog_var), list(prog_var), hlds_goal_info,
    quant_info, quant_info).
:- mode quantify_goal_scope_rename_vars(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out, in, in, out) is det.
:- mode quantify_goal_scope_rename_vars(in(ord_nl_no_lambda),
    in, out, in, out, in, out, in, in, out) is det.
:- mode quantify_goal_scope_rename_vars(in(cg_nl_no_lambda),
    in, out, in, out, in, out, in, in, out) is det.
:- pragma inline(pred(quantify_goal_scope_rename_vars/10)).

quantify_goal_scope_rename_vars(NonLocalsToRecompute, Reason0, Reason,
        SubGoal0, SubGoal, Vars0, Vars, GoalInfo0, !Info) :-
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    get_quant_vars(!.Info, QuantVars0),
    % Rename apart all the quantified variables that occur outside this goal.
    QVars = list_to_set(Vars0),
    set_of_var.intersect(OutsideVars, QVars, RenameVars1),
    set_of_var.intersect(LambdaOutsideVars, QVars, RenameVars2),
    set_of_var.union(RenameVars1, RenameVars2, RenameVars),
    ( if set_of_var.is_empty(RenameVars) then
        SubGoal = SubGoal0,
        Vars = Vars0,
        Reason = Reason0
    else
        Context = goal_info_get_context(GoalInfo0),
        warn_overlapping_scope(RenameVars, Context, !Info),
        rename_vars_apart(NonLocalsToRecompute, RenameVars, RenameMap,
            SubGoal0, SubGoal, !Info),
        rename_var_list(need_not_rename, RenameMap, Vars0, Vars),
        (
            Reason0 = exist_quant(_),
            Reason = exist_quant([])
        ;
            Reason0 = trace_goal(Comp, Run, IO, Mut, TraceVars0),
            rename_var_list(need_not_rename, RenameMap, TraceVars0, TraceVars),
            Reason = trace_goal(Comp, Run, IO, Mut, TraceVars)
        ;
            ( Reason0 = disable_warnings(_, _)
            ; Reason0 = promise_purity(_)
            ; Reason0 = promise_solutions(_, _)
            ; Reason0 = require_detism(_)
            ; Reason0 = require_complete_switch(_)
            ; Reason0 = require_switch_arms_detism(_, _)
            ; Reason0 = commit(_)
            ; Reason0 = barrier(_)
            ; Reason0 = from_ground_term(_, _)
            ; Reason0 = loop_control(_, _, _)
            ),
            % We shouldn't invoke this predicate for these kinds of scopes.
            unexpected($pred, "unexpected scope")
        )
    ),
    update_seen_vars(QVars, !Info),
    set_of_var.insert_list(Vars, QuantVars0, QuantVars),
    set_quant_vars(QuantVars, !Info).

:- pred quantify_goal_shorthand(nonlocals_to_recompute,
    hlds_goal_expr, hlds_goal_expr, hlds_goal_info, set_of_progvar,
    quant_info, quant_info).
:- mode quantify_goal_shorthand(in(ord_nl_maybe_lambda),
    in(goal_expr_shorthand), out, in, out, in, out) is det.
:- mode quantify_goal_shorthand(in(ord_nl_no_lambda),
    in(goal_expr_shorthand), out, in, out, in, out) is det.
:- mode quantify_goal_shorthand(in(cg_nl_no_lambda),
    in(goal_expr_shorthand), out, in, out, in, out) is det.
:- pragma inline(pred(quantify_goal_shorthand/7)).

quantify_goal_shorthand(NonLocalsToRecompute, GoalExpr0, GoalExpr, GoalInfo0,
        PossiblyNonLocalGoalVars0, !Info) :-
    GoalExpr0 = shorthand(ShortHand0),
    (
        ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
            MainGoal0, OrElseGoals0, OrElseInners0),

        % The call to quantify_disj causes the inner STM
        % interface variables to be renamed in any or_else goals, but
        % doing it first explicitly allows the new names of these variables
        % to be stored.
        (
            OrElseInners0 = [],
            rename_or_else_inner_vars(NonLocalsToRecompute, Inner,
                OrElseGoals0, OrElseGoals1, OrElseInners, !Info)
        ;
            OrElseInners0 = [_ | _],
            OrElseInners = OrElseInners0,
            OrElseGoals1 = OrElseGoals0,
            !:Info = !.Info
        ),

        assoc_list.from_corresponding_lists([MainGoal0 | OrElseGoals1],
            [Inner | OrElseInners], AtomicGoalsWithInners0),
        NonLocalVarSets0 = [],
        quantify_atomic_goals(NonLocalsToRecompute,
            AtomicGoalsWithInners0, AllAtomicGoals, !Info,
            NonLocalVarSets0, NonLocalVarSets),

        (
            AllAtomicGoals = [MainGoal | OrElseGoals]
        ;
            AllAtomicGoals = [],
            unexpected($pred, "AllAtomicGoals = []")
        ),
        set_of_var.union_list(NonLocalVarSets, NonLocalVars0),
        (
            GoalType = unknown_atomic_goal_type,
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            set_of_var.insert_list([OuterDI, OuterUO],
                NonLocalVars0, NonLocalVars)
        ;
            ( GoalType = top_level_atomic_goal
            ; GoalType = nested_atomic_goal
            ),
            NonLocalVars = NonLocalVars0
        ),
        set_nonlocals(NonLocalVars, !Info),

        ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
            MainGoal, OrElseGoals, OrElseInners),
        GoalExpr = shorthand(ShortHand)
    ;
        ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
        quantify_goal(NonLocalsToRecompute, SubGoal0, SubGoal, !Info),
        ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
        GoalExpr = shorthand(ShortHand)
    ;
        ShortHand0 = bi_implication(LHS, RHS),
        (
            NonLocalsToRecompute = ord_nl_maybe_lambda,
            quantify_goal_bi_implication(LHS, RHS, GoalExpr, GoalInfo0, !Info)
        ;
            ( NonLocalsToRecompute = ord_nl_no_lambda
            ; NonLocalsToRecompute = cg_nl_no_lambda
            ),
            % Any bi_implications should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ),
    goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
        PossiblyNonLocalGoalVars0).

:- pred rename_or_else_inner_vars(nonlocals_to_recompute,
    atomic_interface_vars, list(hlds_goal), list(hlds_goal),
    list(atomic_interface_vars), quant_info, quant_info).
:- mode rename_or_else_inner_vars(in(ord_nl_maybe_lambda),
    in, in, out, out, in, out) is det.
:- mode rename_or_else_inner_vars(in(ord_nl_no_lambda),
    in, in, out, out, in, out) is det.
:- mode rename_or_else_inner_vars(in(cg_nl_no_lambda),
    in, in, out, out, in, out) is det.

rename_or_else_inner_vars(_, _, [], [], [], !Info).
rename_or_else_inner_vars(NonLocalsToRecompute, Inner,
        [OrElseGoal0 | OrElseGoals0], OrElseGoals, OrElseInners, !Info) :-
    Inner = atomic_interface_vars(InnerDI, InnerUO),
    RenameVars = list_to_set([InnerDI, InnerUO]),
    rename_vars_apart(NonLocalsToRecompute, RenameVars, RenameMap, OrElseGoal0,
        OrElseGoal, !Info),
    OrElseInnerDI = map.lookup(RenameMap, InnerDI),
    OrElseInnerUO = map.lookup(RenameMap, InnerUO),
    OrElseInner = atomic_interface_vars(OrElseInnerDI, OrElseInnerUO),
    rename_or_else_inner_vars(NonLocalsToRecompute, Inner, OrElseGoals0,
        OrElseGoalsTail, OrElseInnersTail, !Info),
    OrElseInners = [OrElseInner | OrElseInnersTail],
    OrElseGoals = [OrElseGoal | OrElseGoalsTail].

:- pred quantify_goal_bi_implication(hlds_goal, hlds_goal,
    hlds_goal_expr, hlds_goal_info, quant_info, quant_info).
:- mode quantify_goal_bi_implication(in, in, out, in, in, out) is det.

quantify_goal_bi_implication(LHS0, RHS0, GoalExpr, OldGoalInfo, !Info) :-
    % Get the initial values of various settings.
    get_quant_vars(!.Info, QuantVars0),
    get_outside(!.Info, OutsideVars0),
    get_lambda_outside(!.Info, LambdaOutsideVars0),

    % Quantified variables cannot be pushed inside a negation, so we insert
    % the quantified vars into the outside vars set, and initialize the new
    % quantified vars set to be empty (the lambda outside vars remain
    % unchanged).
    set_of_var.union(OutsideVars0, QuantVars0, OutsideVars1),
    QuantVars1 = set_of_var.init,
    LambdaOutsideVars1 = LambdaOutsideVars0,
    set_quant_vars(QuantVars1, !Info),

    % Prepare for quantifying the LHS: add variables from the RHS to the
    % outside vars and the outside lambda vars sets.
    goal_vars_both_maybe_lambda_and_bi_impl(RHS0, RHS_Vars, RHS_LambdaVars),
    set_of_var.union(OutsideVars1, RHS_Vars, LHS_OutsideVars),
    set_of_var.union(LambdaOutsideVars1, RHS_LambdaVars,
        LHS_LambdaOutsideVars),

    % Quantify the LHS.
    set_outside(LHS_OutsideVars, !Info),
    set_lambda_outside(LHS_LambdaOutsideVars, !Info),
    quantify_goal(ord_nl_maybe_lambda, LHS0, LHS, !Info),
    get_nonlocals(!.Info, LHS_NonLocalVars),

    % Prepare for quantifying the RHS: add nonlocals from the LHS to the
    % outside vars. (We use the nonlocals, rather than the more symmetric
    % approach of calling goal_vars on the LHS goal, because it is more
    % efficient.)
    set_of_var.union(OutsideVars1, LHS_NonLocalVars, RHS_OutsideVars),
    RHS_LambdaOutsideVars = LambdaOutsideVars1,

    % Quantify the RHS.
    set_outside(RHS_OutsideVars, !Info),
    set_lambda_outside(RHS_LambdaOutsideVars, !Info),
    quantify_goal(ord_nl_maybe_lambda, RHS0, RHS, !Info),
    get_nonlocals(!.Info, RHS_NonLocalVars),

    % Compute the nonlocals for this goal.
    set_of_var.union(LHS_NonLocalVars, RHS_NonLocalVars, AllNonLocalVars),
    set_of_var.intersect(AllNonLocalVars, OutsideVars0, NonLocalVarsO),
    set_of_var.intersect(AllNonLocalVars, LambdaOutsideVars0, NonLocalVarsL),
    set_of_var.union(NonLocalVarsO, NonLocalVarsL, NonLocalVars),
    set_nonlocals(NonLocalVars, !Info),

    % Restore the original values of various settings.
    set_outside(OutsideVars0, !Info),
    set_lambda_outside(LambdaOutsideVars0, !Info),
    set_quant_vars(QuantVars0, !Info),

    % We have figured out the quantification.
    % Now expand the bi-implication according to the usual rules:
    %   LHS <=> RHS
    % ===>
    %   (LHS => RHS), (RHS => LHS)
    % ===>
    %   (not (LHS, not RHS)), (not (RHS, not LHS))

    Context = goal_info_get_context(OldGoalInfo),
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo1),
    set_goal_nonlocals(ord_nl_maybe_lambda, LHS_NonLocalVars,
        GoalInfo1, LHS_GI, !Info),
    set_goal_nonlocals(ord_nl_maybe_lambda, RHS_NonLocalVars,
        GoalInfo1, RHS_GI, !Info),
    set_goal_nonlocals(ord_nl_maybe_lambda, NonLocalVars,
        GoalInfo1, GI, !Info),
    NotLHS = hlds_goal(negation(LHS), LHS_GI),
    NotRHS = hlds_goal(negation(RHS), RHS_GI),
    ForwardsImplicationExpr =
        negation(hlds_goal(conj(plain_conj, [LHS, NotRHS]), GI)),
    ForwardsImplication = hlds_goal(ForwardsImplicationExpr, GI),

    ReverseImplicationExpr0 =
        negation(hlds_goal(conj(plain_conj, [RHS, NotLHS]), GI)),
    ReverseImplication0 = hlds_goal(ReverseImplicationExpr0, GI),

    % Rename apart the local variables of the goals we have just duplicated.
    goal_vars_bitset_maybe_lambda_and_bi_impl(ReverseImplication0, GoalVars),
    set_of_var.difference(GoalVars, NonLocalVars, RenameVars),
    rename_vars_apart(ord_nl_maybe_lambda, RenameVars, _,
        ReverseImplication0, ReverseImplication, !Info),

    GoalExpr = conj(plain_conj, [ForwardsImplication, ReverseImplication]).

:- pred quantify_primitive_goal(list(prog_var)::in,
    quant_info::in, quant_info::out) is det.

quantify_primitive_goal(HeadVars, !Info) :-
    GoalVars = list_to_set(HeadVars),
    update_seen_vars(GoalVars, !Info),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    set_of_var.intersect(GoalVars, OutsideVars, NonLocals1),
    set_of_var.intersect(GoalVars, LambdaOutsideVars, NonLocals2),
    set_of_var.union(NonLocals1, NonLocals2, NonLocals),
    set_nonlocals(NonLocals, !Info).

:- pred quantify_unify_rhs(nonlocals_to_recompute,
    maybe(list(needs_update)), hlds_goal_info, unify_rhs, unify_rhs,
    unification, unification, set_of_progvar, quant_info, quant_info).
:- mode quantify_unify_rhs(in(ord_nl_maybe_lambda),
    in, in, in, out, in, out, out, in, out) is det.
:- mode quantify_unify_rhs(in(ord_nl_no_lambda),
    in, in, in, out, in, out, out, in, out) is det.
:- mode quantify_unify_rhs(in(cg_nl_no_lambda),
    in, in, in, out, in, out, out, in, out) is det.

quantify_unify_rhs(NonLocalsToRecompute, ReuseArgs, GoalInfo0,
        !RHS, !Unification, RHSNonLocals, !Info) :-
    (
        !.RHS = rhs_var(X),
        RHSNonLocals = set_of_var.make_singleton(X)
    ;
        !.RHS = rhs_functor(_, _, ArgVars),
        ( if
            NonLocalsToRecompute = cg_nl_no_lambda,
            ReuseArgs = yes(SetArgs)
        then
            % The fields taken from the reused cell aren't counted
            % as code-gen nonlocals.
            get_updated_fields(SetArgs, ArgVars, Vars0),
            RHSNonLocals = set_of_var.list_to_set(Vars0)
        else
            RHSNonLocals = set_of_var.list_to_set(ArgVars)
        )
    ;
        !.RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals0, ArgVarsModes0, Det, Goal0),
        assoc_list.keys_and_values(ArgVarsModes0, ArgVars0, Modes),

        % Note: make_hlds.m has already done most of the hard work for
        % lambda expressions. At this point, ArgVars0 should in fact be
        % guaranteed to be fresh distinct variables. However, the code below
        % does not assume this.

        get_outside(!.Info, OutsideVars0),
        QVars = set_of_var.list_to_set(ArgVars0),
        % Figure out which variables have overlapping scopes because they occur
        % outside the goal and are also lambda-quantified vars.
        set_of_var.intersect(OutsideVars0, QVars, RenameVars0),
        ( if set_of_var.is_empty(RenameVars0) then
            true
        else
            Context = goal_info_get_context(GoalInfo0),
            warn_overlapping_scope(RenameVars0, Context, !Info)
        ),
        % We need to rename apart any of the lambda vars that we have
        % already seen, since they are new instances.
        get_seen(!.Info, Seen0),
        set_of_var.intersect(Seen0, QVars, RenameVars1),

        set_of_var.union(RenameVars0, RenameVars1, RenameVars),
        rename_vars_apart(NonLocalsToRecompute, RenameVars, RenameMap,
            Goal0, Goal1, !Info),
        rename_var_list(need_not_rename, RenameMap, ArgVars0, ArgVars),

        % Quantified variables cannot be pushed inside a lambda goal,
        % so we insert the quantified vars into the outside vars set,
        % and initialize the new quantified vars set to be empty.
        get_quant_vars(!.Info, QuantVars0),
        set_of_var.union(OutsideVars0, QuantVars0, OutsideVars1),
        QuantVars = set_of_var.init,
        set_quant_vars(QuantVars, !Info),
        % Add the lambda vars as outside vars, since they are outside of the
        % lambda goal.
        set_of_var.insert_list(ArgVars, OutsideVars1, OutsideVars),
        set_outside(OutsideVars, !Info),
        % Set the LambdaOutsideVars set to empty, because variables that occur
        % outside this lambda expression only in other lambda expressions
        % should not be considered nonlocal.
        get_lambda_outside(!.Info, LambdaOutsideVars0),
        LambdaOutsideVars = set_of_var.init,
        set_lambda_outside(LambdaOutsideVars, !Info),
        quantify_goal(NonLocalsToRecompute, Goal1, Goal, !Info),

        get_nonlocals(!.Info, RHSNonLocals0),
        % Lambda-quantified variables are local.
        set_of_var.delete_list(ArgVars, RHSNonLocals0, RHSNonLocals),
        set_quant_vars(QuantVars0, !Info),
        set_outside(OutsideVars0, !Info),
        set_lambda_outside(LambdaOutsideVars0, !Info),

        % Work out the list of nonlocal curried arguments to the lambda
        % expression. This set must only ever decrease, since the first
        % approximation that make_hlds uses includes all variables in the
        % lambda expression except the quantified variables.

        Goal = hlds_goal(_, LambdaGoalInfo),
        LambdaGoalNonLocals = goal_info_get_nonlocals(LambdaGoalInfo),
        list.filter(set_of_var.contains(LambdaGoalNonLocals),
            LambdaNonLocals0, LambdaNonLocals),

        assoc_list.from_corresponding_lists(ArgVars, Modes, ArgVarsModes),
        !:RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals, ArgVarsModes, Det, Goal),

        % For a unification that constructs a lambda expression, the argument
        % variables of the construction are the nonlocal variables of the
        % lambda expression. So if we recompute the nonlocals, we need to
        % recompute the argument variables of the construction, and hence
        % we also need to recompute their modes. The nonlocals set must
        % only ever decrease, not increase, so we can just use the old modes.

        (
            !.Unification = construct(ConstructVar, ConsId, Args0,
                ArgModes0, HowToConstruct, Uniq, SubInfo),
            (
                SubInfo = no_construct_sub_info
            ;
                SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize),
                expect(unify(MaybeTakeAddr, no), $pred,
                    "lambda term has take addr"),
                expect(unify(MaybeSize, no), $pred,
                    "lambda term has size info")
            ),
            map.from_corresponding_lists(Args0, ArgModes0, ArgModesMap),
            Args = set_of_var.to_sorted_list(RHSNonLocals),
            map.apply_to_list(Args, ArgModesMap, ArgModes),
            !:Unification = construct(ConstructVar, ConsId, Args,
                ArgModes, HowToConstruct, Uniq, SubInfo)
        ;
            % After mode analysis, unifications with lambda variables should
            % always be construction unifications, but quantification gets
            % invoked before mode analysis, so we need to allow this case.
            ( !.Unification = deconstruct(_, _, _, _, _, _)
            ; !.Unification = assign(_, _)
            ; !.Unification = simple_test(_, _)
            ; !.Unification = complicated_unify(_, _, _)
            )
        )
    ).

:- pred quantify_conj_maybe_lambda(nonlocals_to_recompute,
    list(hlds_goal), list(hlds_goal), set_of_progvar, quant_info, quant_info).
:- mode quantify_conj_maybe_lambda(in(ord_nl_maybe_lambda),
    in, out, out, in, out) is det.

quantify_conj_maybe_lambda(NonLocalsToRecompute, !Goals,
        PossiblyNonLocalGoalVars, !Info) :-
    get_following_vars_maybe_lambda(NonLocalsToRecompute, !.Goals,
        FollowingVarsList, PossiblyNonLocalGoalVars),
    quantify_conj_maybe_lambda_2(NonLocalsToRecompute,
        FollowingVarsList, !Goals, !Info).

:- pred quantify_conj_no_lambda(nonlocals_to_recompute,
    list(hlds_goal), list(hlds_goal), set_of_progvar, quant_info, quant_info).
:- mode quantify_conj_no_lambda(in(ord_nl_no_lambda),
    in, out, out, in, out) is det.
:- mode quantify_conj_no_lambda(in(cg_nl_no_lambda),
    in, out, out, in, out) is det.

quantify_conj_no_lambda(NonLocalsToRecompute, !Goals,
        PossiblyNonLocalGoalVars, !Info) :-
    get_following_vars_no_lambda(NonLocalsToRecompute, !.Goals,
        FollowingVarsList, PossiblyNonLocalGoalVars),
    quantify_conj_no_lambda_2(NonLocalsToRecompute,
        FollowingVarsList, !Goals, !Info).

:- pred quantify_conj_maybe_lambda_2(nonlocals_to_recompute,
    list(pair(set_of_progvar)), list(hlds_goal), list(hlds_goal),
    quant_info, quant_info).
:- mode quantify_conj_maybe_lambda_2(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

quantify_conj_maybe_lambda_2(_, _, [], [], !Info) :-
    NonLocalVars = set_of_var.init,
    set_nonlocals(NonLocalVars, !Info).
quantify_conj_maybe_lambda_2(_, [], [_ | _], _, _, _) :-
    unexpected($pred, "length mismatch").
quantify_conj_maybe_lambda_2(NonLocalsToRecompute,
        [FollowingVarPair | FollowingVarPairs],
        [Goal0 | Goals0], [Goal | Goals], !Info) :-
    FollowingVarPair = FollowingVars - LambdaFollowingVars,
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    set_of_var.union(OutsideVars, FollowingVars, OutsideVars1),
    set_of_var.union(LambdaOutsideVars, LambdaFollowingVars,
        LambdaOutsideVars1),
    set_outside(OutsideVars1, !Info),
    set_lambda_outside(LambdaOutsideVars1, !Info),
    quantify_goal(NonLocalsToRecompute, Goal0, Goal, !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    set_of_var.union(OutsideVars, NonLocalVars1, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    set_lambda_outside(LambdaOutsideVars, !Info),
    quantify_conj_maybe_lambda_2(NonLocalsToRecompute,
        FollowingVarPairs, Goals0, Goals, !Info),
    get_nonlocals(!.Info, NonLocalVars2),
    set_of_var.union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
    set_of_var.intersect(NonLocalVarsConj, OutsideVars, NonLocalVarsO),
    set_of_var.intersect(NonLocalVarsConj, LambdaOutsideVars, NonLocalVarsL),
    set_of_var.union(NonLocalVarsO, NonLocalVarsL, NonLocalVars),
    set_outside(OutsideVars, !Info),
    set_nonlocals(NonLocalVars, !Info).

:- pred quantify_conj_no_lambda_2(nonlocals_to_recompute,
    list(set_of_progvar), list(hlds_goal), list(hlds_goal),
    quant_info, quant_info).
:- mode quantify_conj_no_lambda_2(in(ord_nl_no_lambda),
    in, in, out,in, out) is det.
:- mode quantify_conj_no_lambda_2(in(cg_nl_no_lambda),
    in, in, out,in, out) is det.

quantify_conj_no_lambda_2(_, _, [], [], !Info) :-
    NonLocalVars = set_of_var.init,
    set_nonlocals(NonLocalVars, !Info).
quantify_conj_no_lambda_2(_, [], [_ | _], _, _, _) :-
    unexpected($pred, "length mismatch").
quantify_conj_no_lambda_2(NonLocalsToRecompute,
        [FollowingVars | FollowingVarsList],
        [Goal0 | Goals0], [Goal | Goals], !Info) :-
    get_outside(!.Info, OutsideVars),
    union(OutsideVars, FollowingVars, OutsideVars1),
    set_outside(OutsideVars1, !Info),
    quantify_goal(NonLocalsToRecompute, Goal0, Goal, !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    set_of_var.union(OutsideVars, NonLocalVars1, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    quantify_conj_no_lambda_2(NonLocalsToRecompute,
        FollowingVarsList, Goals0, Goals, !Info),
    get_nonlocals(!.Info, NonLocalVars2),
    set_of_var.union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
    set_of_var.intersect(NonLocalVarsConj, OutsideVars, NonLocalVars),
    set_outside(OutsideVars, !Info),
    set_nonlocals(NonLocalVars, !Info).

:- pred quantify_disj(nonlocals_to_recompute,
    list(hlds_goal), list(hlds_goal), quant_info, quant_info,
    list(set_of_progvar), list(set_of_progvar)).
:- mode quantify_disj(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_disj(in(ord_nl_no_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_disj(in(cg_nl_no_lambda),
    in, out, in, out, in, out) is det.

quantify_disj(_, [], [], !Info, !NonLocalVarSets).
quantify_disj(NonLocalsToRecompute,
        [Goal0 | Goals0], [Goal | Goals], !Info, !NonLocalVarSets) :-
    quantify_goal(NonLocalsToRecompute, Goal0, Goal, !Info),
    get_nonlocals(!.Info, GoalNonLocalVars),
    !:NonLocalVarSets = [GoalNonLocalVars | !.NonLocalVarSets],
    quantify_disj(NonLocalsToRecompute, Goals0, Goals,
        !Info, !NonLocalVarSets).

:- pred quantify_atomic_goals(nonlocals_to_recompute,
    list(pair(hlds_goal, atomic_interface_vars)), list(hlds_goal),
    quant_info, quant_info, list(set_of_progvar), list(set_of_progvar)).
:- mode quantify_atomic_goals(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_atomic_goals(in(ord_nl_no_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_atomic_goals(in(cg_nl_no_lambda),
    in, out, in, out, in, out) is det.

quantify_atomic_goals(_, [], [], !Info, !NonLocalVarSets).
quantify_atomic_goals(NonLocalsToRecompute,
        [Goal0 - Inner0 | Goals0], [Goal | Goals], !Info, !NonLocalVarSets) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    ( if
        goal_info_has_feature(GoalInfo0, feature_contains_stm_inner_outer)
    then
        true
    else
        % The calls to stm_from_outer_to_inner and stm_from_inner_to_outer are
        % not inserted until the purity checking pass.
        Inner0 = atomic_interface_vars(InnerDI, InnerUO),
        get_outside(!.Info, OutsideVars0),
        set_of_var.insert_list([InnerDI, InnerUO], OutsideVars0, OutsideVars),
        set_outside(OutsideVars, !Info)
    ),
    quantify_goal(NonLocalsToRecompute, Goal0, Goal, !Info),
    get_nonlocals(!.Info, GoalNonLocalVars),
    !:NonLocalVarSets = [GoalNonLocalVars | !.NonLocalVarSets],
    quantify_atomic_goals(NonLocalsToRecompute, Goals0, Goals,
        !Info, !NonLocalVarSets).

:- pred quantify_cases(nonlocals_to_recompute,
    list(case), list(case), quant_info, quant_info,
    list(set_of_progvar), list(set_of_progvar)).
:- mode quantify_cases(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_cases(in(ord_nl_no_lambda),
    in, out, in, out, in, out) is det.
:- mode quantify_cases(in(cg_nl_no_lambda),
    in, out, in, out, in, out) is det.

quantify_cases(_, [], [], !Info, !NonLocalVarSets).
quantify_cases(NonLocalsToRecompute,
        [Case0 | Cases0], [Case | Cases], !Info, !NonLocalVarSets) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    quantify_goal(NonLocalsToRecompute, Goal0, Goal, !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    get_nonlocals(!.Info, GoalNonLocalVars),
    !:NonLocalVarSets = [GoalNonLocalVars | !.NonLocalVarSets],
    quantify_cases(NonLocalsToRecompute, Cases0, Cases,
        !Info, !NonLocalVarSets).

%---------------------------------------------------------------------------%

    % Insert the given set of variables into the set of `seen' variables.
    %
:- pred update_seen_vars(set_of_progvar::in, quant_info::in, quant_info::out)
    is det.

update_seen_vars(NewVars, !Info) :-
    get_seen(!.Info, SeenVars0),
    set_of_var.union(SeenVars0, NewVars, SeenVars),
    set_seen(SeenVars, !Info).

%---------------------------------------------------------------------------%

    % Given a list of goals, produce a corresponding list of following
    % variables, where the following variables for each goal are those
    % variables which occur free in any of the following goals in the list.
    % The following variables are divided into a pair of sets: the first set
    % contains following variables that occur not in lambda goals, and the
    % second contains following variables that occur in lambda goals.
    %
:- pred get_following_vars_maybe_lambda(nonlocals_to_recompute,
    list(hlds_goal), list(pair(set_of_progvar)), set_of_progvar).
:- mode get_following_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, out, out) is det.

get_following_vars_maybe_lambda(_, [], [], set_of_var.init).
get_following_vars_maybe_lambda(NonLocalsToRecompute, [Goal | Goals],
        [Set - LambdaSet | SetPairs], PossiblyNonLocalGoalVars) :-
    get_following_vars_maybe_lambda_2(NonLocalsToRecompute, Goals,
        Set, LambdaSet, SetPairs),
    set_of_var.union(Set, LambdaSet, GoalsBothSet),
    goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal,
        GoalSet, GoalLambdaSet),
    set_of_var.union(GoalSet, GoalLambdaSet, GoalBothSet),
    set_of_var.union(GoalBothSet, GoalsBothSet, PossiblyNonLocalGoalVars).

:- pred get_following_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    list(set_of_progvar), set_of_progvar).
:- mode get_following_vars_no_lambda(in(ord_nl_no_lambda),
    in, out, out) is det.
:- mode get_following_vars_no_lambda(in(cg_nl_no_lambda),
    in, out, out) is det.

get_following_vars_no_lambda(_, [], [], set_of_var.init).
get_following_vars_no_lambda(NonLocalsToRecompute, [Goal | Goals],
        [Set | Sets], PossiblyNonLocalGoalVars) :-
    get_following_vars_no_lambda_2(NonLocalsToRecompute, Goals, Set, Sets),
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, GoalSet),
    set_of_var.union(GoalSet, Set, PossiblyNonLocalGoalVars).

:- pred get_following_vars_maybe_lambda_2(nonlocals_to_recompute,
    list(hlds_goal), set_of_progvar, set_of_progvar,
    list(pair(set_of_progvar))).
:- mode get_following_vars_maybe_lambda_2(in(ord_nl_maybe_lambda),
    in, out, out, out) is det.

get_following_vars_maybe_lambda_2(_, [], Set, LambdaSet, []) :-
    Set = set_of_var.init,
    LambdaSet = set_of_var.init.
get_following_vars_maybe_lambda_2(NonLocalsToRecompute, [Goal | Goals],
        Set, LambdaSet, SetPairList) :-
    get_following_vars_maybe_lambda_2(NonLocalsToRecompute, Goals,
        Set0, LambdaSet0, SetPairList0),
    goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal, Set1, LambdaSet1),
    set_of_var.union(Set0, Set1, Set),
    set_of_var.union(LambdaSet0, LambdaSet1, LambdaSet),
    SetPairList = [Set0 - LambdaSet0 | SetPairList0].

:- pred get_following_vars_no_lambda_2(nonlocals_to_recompute, list(hlds_goal),
    set_of_progvar, list(set_of_progvar)).
:- mode get_following_vars_no_lambda_2(in(ord_nl_no_lambda),
    in, out, out) is det.
:- mode get_following_vars_no_lambda_2(in(cg_nl_no_lambda),
    in, out, out) is det.

get_following_vars_no_lambda_2(_, [], Set, []) :-
    Set = set_of_var.init.
get_following_vars_no_lambda_2(NonLocalsToRecompute, [Goal | Goals],
        Set, SetList) :-
    get_following_vars_no_lambda_2(NonLocalsToRecompute, Goals,
        Set0, SetList0),
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, Set1),
    set_of_var.union(Set0, Set1, Set),
    SetList = [Set0 | SetList0].

:- pred conj_vars_maybe_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode conj_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

conj_vars_maybe_lambda(_, [], !Set, !LambdaSet).
conj_vars_maybe_lambda(NonLocalsToRecompute, [Goal | Goals],
        !Set, !LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, GoalExpr,
        !Set, !LambdaSet),
    conj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet).

:- pred conj_vars_maybe_lambda_and_bi_impl(list(hlds_goal),
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode conj_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

conj_vars_maybe_lambda_and_bi_impl([], !Set, !LambdaSet).
conj_vars_maybe_lambda_and_bi_impl([Goal | Goals], !Set, !LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    goal_expr_vars_maybe_lambda_and_bi_impl_2(GoalExpr, !Set, !LambdaSet),
    conj_vars_maybe_lambda_and_bi_impl(Goals, !Set, !LambdaSet).

:- pred conj_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_progvar, set_of_progvar).
:- mode conj_vars_no_lambda(in(ord_nl_no_lambda),
    in, in, out) is det.
:- mode conj_vars_no_lambda(in(cg_nl_no_lambda),
    in, in, out) is det.

conj_vars_no_lambda(_, [], !Set).
conj_vars_no_lambda(NonLocalsToRecompute, [Goal | Goals], !Set) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    goal_expr_vars_no_lambda_2(NonLocalsToRecompute, GoalExpr, !Set),
    conj_vars_no_lambda(NonLocalsToRecompute, Goals, !Set).

:- pred disj_vars_maybe_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode disj_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

disj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet) :-
    compute_disj_vars_maybe_lambda(NonLocalsToRecompute, Goals,
        [], GoalSets, [], GoalLambdaSets),
    (
        GoalSets = [],
        GoalsSet = set_of_var.init
    ;
        GoalSets = [_ | _],
        set_of_var.union_list(GoalSets, GoalsSet)
    ),
    (
        GoalLambdaSets = [],
        GoalsLambdaSet = set_of_var.init
    ;
        GoalLambdaSets = [_ | _],
        set_of_var.union_list(GoalLambdaSets, GoalsLambdaSet)
    ),
    set_of_var.union(GoalsSet, !Set),
    set_of_var.union(GoalsLambdaSet, !LambdaSet).

:- pred disj_vars_maybe_lambda_and_bi_impl(list(hlds_goal),
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode disj_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

disj_vars_maybe_lambda_and_bi_impl(Goals, !Set, !LambdaSet) :-
    compute_disj_vars_maybe_lambda_and_bi_impl(Goals,
        [], GoalSets, [], GoalLambdaSets),
    (
        GoalSets = [],
        GoalsSet = set_of_var.init
    ;
        GoalSets = [_ | _],
        set_of_var.union_list(GoalSets, GoalsSet)
    ),
    (
        GoalLambdaSets = [],
        GoalsLambdaSet = set_of_var.init
    ;
        GoalLambdaSets = [_ | _],
        set_of_var.union_list(GoalLambdaSets, GoalsLambdaSet)
    ),
    set_of_var.union(GoalsSet, !Set),
    set_of_var.union(GoalsLambdaSet, !LambdaSet).

:- pred disj_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_progvar, set_of_progvar).
:- mode disj_vars_no_lambda(in(ord_nl_no_lambda),
    in, in, out) is det.
:- mode disj_vars_no_lambda(in(cg_nl_no_lambda),
    in, in, out) is det.

disj_vars_no_lambda(NonLocalsToRecompute, Goals, !Set) :-
    compute_disj_vars_no_lambda(NonLocalsToRecompute, Goals, [], GoalSets),
    (
        GoalSets = [],
        GoalsSet = set_of_var.init
    ;
        GoalSets = [_ | _],
        set_of_var.union_list(GoalSets, GoalsSet)
    ),
    set_of_var.union(GoalsSet, !Set).

:- pred compute_disj_vars_maybe_lambda(nonlocals_to_recompute, list(hlds_goal),
    list(set_of_progvar), list(set_of_progvar),
    list(set_of_progvar), list(set_of_progvar)).
:- mode compute_disj_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

compute_disj_vars_maybe_lambda(_, [], !Sets, !LambdaSets).
compute_disj_vars_maybe_lambda(NonLocalsToRecompute, [Goal | Goals],
        !Sets, !LambdaSets) :-
    goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal,
        GoalSet, GoalLambdaSet),
    !:Sets = [GoalSet | !.Sets],
    !:LambdaSets = [GoalLambdaSet | !.LambdaSets],
    compute_disj_vars_maybe_lambda(NonLocalsToRecompute, Goals,
        !Sets, !LambdaSets).

:- pred compute_disj_vars_maybe_lambda_and_bi_impl(list(hlds_goal),
    list(set_of_progvar), list(set_of_progvar),
    list(set_of_progvar), list(set_of_progvar)).
:- mode compute_disj_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

compute_disj_vars_maybe_lambda_and_bi_impl([], !Sets, !LambdaSets).
compute_disj_vars_maybe_lambda_and_bi_impl([Goal | Goals],
        !Sets, !LambdaSets) :-
    goal_vars_both_maybe_lambda_and_bi_impl(Goal, GoalSet, GoalLambdaSet),
    !:Sets = [GoalSet | !.Sets],
    !:LambdaSets = [GoalLambdaSet | !.LambdaSets],
    compute_disj_vars_maybe_lambda_and_bi_impl(Goals, !Sets, !LambdaSets).

:- pred compute_disj_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    list(set_of_progvar), list(set_of_progvar)).
:- mode compute_disj_vars_no_lambda(in(ord_nl_no_lambda),
    in, in, out) is det.
:- mode compute_disj_vars_no_lambda(in(cg_nl_no_lambda),
    in, in, out) is det.

compute_disj_vars_no_lambda(_, [], !Sets).
compute_disj_vars_no_lambda(NonLocalsToRecompute, [Goal | Goals], !Sets) :-
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, GoalSet),
    !:Sets = [GoalSet | !.Sets],
    compute_disj_vars_no_lambda(NonLocalsToRecompute, Goals, !Sets).

:- pred case_vars_maybe_lambda(nonlocals_to_recompute, list(case),
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode case_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

case_vars_maybe_lambda(NonLocalsToRecompute, Cases, !Set, !LambdaSet) :-
    compute_case_vars_maybe_lambda(NonLocalsToRecompute, Cases,
        [], CaseSets, [], CaseLambdaSets),
    (
        CaseSets = [],
        unexpected($pred, "no cases (1)")
    ;
        CaseSets = [_ | _],
        set_of_var.union_list(CaseSets, CasesSet)
    ),
    (
        CaseLambdaSets = [],
        unexpected($pred, "no cases (2)")
    ;
        CaseLambdaSets = [_ | _],
        set_of_var.union_list(CaseLambdaSets, CasesLambdaSet)
    ),
    set_of_var.union(CasesSet, !Set),
    set_of_var.union(CasesLambdaSet, !LambdaSet).

:- pred case_vars_maybe_lambda_and_bi_impl(list(case),
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode case_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

case_vars_maybe_lambda_and_bi_impl(Cases, !Set, !LambdaSet) :-
    compute_case_vars_maybe_lambda_and_bi_impl(Cases,
        [], CaseSets, [], CaseLambdaSets),
    (
        CaseSets = [],
        unexpected($pred, "no cases (1)")
    ;
        CaseSets = [_ | _],
        set_of_var.union_list(CaseSets, CasesSet)
    ),
    (
        CaseLambdaSets = [],
        unexpected($pred, "no cases (2)")
    ;
        CaseLambdaSets = [_ | _],
        set_of_var.union_list(CaseLambdaSets, CasesLambdaSet)
    ),
    set_of_var.union(CasesSet, !Set),
    set_of_var.union(CasesLambdaSet, !LambdaSet).

:- pred case_vars_no_lambda(nonlocals_to_recompute, list(case),
    set_of_progvar, set_of_progvar).
:- mode case_vars_no_lambda(in(ord_nl_no_lambda),
    in, in, out) is det.
:- mode case_vars_no_lambda(in(cg_nl_no_lambda),
    in, in, out) is det.

case_vars_no_lambda(NonLocalsToRecompute, Cases, !Set) :-
    compute_case_vars_no_lambda(NonLocalsToRecompute, Cases, [], CaseSets),
    (
        CaseSets = [],
        unexpected($pred, "no cases (1)")
    ;
        CaseSets = [_ | _],
        set_of_var.union_list(CaseSets, CasesSet)
    ),
    set_of_var.union(CasesSet, !Set).

:- pred compute_case_vars_maybe_lambda(nonlocals_to_recompute, list(case),
    list(set_of_progvar), list(set_of_progvar),
    list(set_of_progvar), list(set_of_progvar)).
:- mode compute_case_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

compute_case_vars_maybe_lambda(_, [], !Sets, !LambdaSets).
compute_case_vars_maybe_lambda(NonLocalsToRecompute, [Case | Cases],
        !Sets, !LambdaSets) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal,
        GoalSet, GoalLambdaSet),
    !:Sets = [GoalSet | !.Sets],
    !:LambdaSets = [GoalLambdaSet | !.LambdaSets],
    compute_case_vars_maybe_lambda(NonLocalsToRecompute, Cases,
        !Sets, !LambdaSets).

:- pred compute_case_vars_maybe_lambda_and_bi_impl(list(case),
    list(set_of_progvar), list(set_of_progvar),
    list(set_of_progvar), list(set_of_progvar)).
:- mode compute_case_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

compute_case_vars_maybe_lambda_and_bi_impl([], !Sets, !LambdaSets).
compute_case_vars_maybe_lambda_and_bi_impl([Case | Cases],
        !Sets, !LambdaSets) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    goal_vars_both_maybe_lambda_and_bi_impl(Goal, GoalSet, GoalLambdaSet),
    !:Sets = [GoalSet | !.Sets],
    !:LambdaSets = [GoalLambdaSet | !.LambdaSets],
    compute_case_vars_maybe_lambda_and_bi_impl(Cases, !Sets, !LambdaSets).

:- pred compute_case_vars_no_lambda(nonlocals_to_recompute, list(case),
    list(set_of_progvar), list(set_of_progvar)).
:- mode compute_case_vars_no_lambda(in(ord_nl_no_lambda),
    in, in, out) is det.
:- mode compute_case_vars_no_lambda(in(cg_nl_no_lambda),
    in, in, out) is det.

compute_case_vars_no_lambda(_, [], !Sets).
compute_case_vars_no_lambda(NonLocalsToRecompute, [Case | Cases], !Sets) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, GoalSet),
    !:Sets = [GoalSet | !.Sets],
    compute_case_vars_no_lambda(NonLocalsToRecompute, Cases, !Sets).

free_goal_vars(Goal) =
    free_goal_vars_nl_maybe_lambda(ord_nl_maybe_lambda, Goal).

    % free_goal_vars_nl(NonLocalsToRecompute, Goal) = Vars:
    %
    % Vars is the set of variables that occur free (unquantified) in Goal,
    % excluding unset fields of reconstructions if NonLocalsToRecompute
    % is `cg_nl_no_lambda'.
    %
:- func free_goal_vars_nl_maybe_lambda(nonlocals_to_recompute, hlds_goal)
    = set_of_progvar.
:- mode free_goal_vars_nl_maybe_lambda(in(ord_nl_maybe_lambda),
    in) = out is det.

free_goal_vars_nl_maybe_lambda(NonLocalsToRecompute, Goal) = BothSet :-
    goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, BothSet).

:- pred goal_vars_bitset_maybe_lambda(nonlocals_to_recompute,
    hlds_goal, set_of_progvar).
:- mode goal_vars_bitset_maybe_lambda(in(ord_nl_maybe_lambda),
    in, out) is det.

goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, BothSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_vars_bitset_maybe_lambda_and_bi_impl(hlds_goal, set_of_progvar).
:- mode goal_vars_bitset_maybe_lambda_and_bi_impl(in, out) is det.

goal_vars_bitset_maybe_lambda_and_bi_impl(Goal, BothSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda_and_bi_impl(GoalExpr, Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_expr_vars_bitset(nonlocals_to_recompute,
    hlds_goal_expr, set_of_progvar).
:- mode goal_expr_vars_bitset(in(ord_nl_maybe_lambda),
    in, out) is det.
:- mode goal_expr_vars_bitset(in(ord_nl_no_lambda),
    in, out) is det.
:- mode goal_expr_vars_bitset(in(cg_nl_no_lambda),
    in, out) is det.

goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr, BothSet) :-
    (
        NonLocalsToRecompute = ord_nl_maybe_lambda,
        goal_expr_vars_bitset_maybe_lambda(NonLocalsToRecompute, GoalExpr,
            BothSet)
    ;
        ( NonLocalsToRecompute = ord_nl_no_lambda
        ; NonLocalsToRecompute = cg_nl_no_lambda
        ),
        goal_expr_vars_bitset_no_lambda(NonLocalsToRecompute, GoalExpr,
            BothSet)
    ).

:- pred goal_expr_vars_bitset_maybe_lambda(nonlocals_to_recompute,
    hlds_goal_expr, set_of_progvar).
:- mode goal_expr_vars_bitset_maybe_lambda(in(ord_nl_maybe_lambda),
    in, out) is det.

goal_expr_vars_bitset_maybe_lambda(NonLocalsToRecompute, GoalExpr, BothSet) :-
    goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_expr_vars_bitset_no_lambda(nonlocals_to_recompute,
    hlds_goal_expr, set_of_progvar).
:- mode goal_expr_vars_bitset_no_lambda(in(ord_nl_no_lambda),
    in, out) is det.
:- mode goal_expr_vars_bitset_no_lambda(in(cg_nl_no_lambda),
    in, out) is det.

goal_expr_vars_bitset_no_lambda(NonLocalsToRecompute, GoalExpr, BothSet) :-
    goal_expr_vars_both_no_lambda(NonLocalsToRecompute, GoalExpr, Set),
    BothSet = Set.

    % goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal,
    %   NonLambdaSet, LambdaSet):
    %
    % Set is the set of variables that occur free (unquantified) in Goal,
    % not counting occurrences in lambda expressions. LambdaSet is the set
    % of variables that occur free (unquantified) in lambda expressions
    % in Goal.
    %
:- pred goal_vars_both_maybe_lambda(nonlocals_to_recompute, hlds_goal,
    set_of_progvar, set_of_progvar).
:- mode goal_vars_both_maybe_lambda(in(ord_nl_maybe_lambda),
    in, out, out) is det.

goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal, Set, LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet).

    % goal_vars_both_maybe_lambda_and_bi_impl(NonLocalsToRecompute, Goal,
    %   NonLambdaSet, LambdaSet):
    %
    % As goal_vars_both_maybe_lambda, but include all the variables from
    % from_groun_term scopes, not just the term variable.
    %
:- pred goal_vars_both_maybe_lambda_and_bi_impl(hlds_goal,
    set_of_progvar, set_of_progvar).
:- mode goal_vars_both_maybe_lambda_and_bi_impl(in, out, out) is det.

goal_vars_both_maybe_lambda_and_bi_impl(Goal, Set, LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda_and_bi_impl(GoalExpr, Set, LambdaSet).

:- pred goal_vars_both_no_lambda(nonlocals_to_recompute, hlds_goal,
    set_of_progvar).
:- mode goal_vars_both_no_lambda(in(ord_nl_no_lambda),
    in, out) is det.
:- mode goal_vars_both_no_lambda(in(cg_nl_no_lambda),
    in, out) is det.

goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, Set) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_no_lambda(NonLocalsToRecompute, GoalExpr, Set).

:- pred goal_expr_vars_both_maybe_lambda(nonlocals_to_recompute,
    hlds_goal_expr, set_of_progvar, set_of_progvar).
:- mode goal_expr_vars_both_maybe_lambda(in(ord_nl_maybe_lambda),
    in, out, out) is det.

goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet) :-
    Set0 = set_of_var.init,
    LambdaSet0 = set_of_var.init,
    goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, GoalExpr, Set0, Set,
        LambdaSet0, LambdaSet).

:- pred goal_expr_vars_both_maybe_lambda_and_bi_impl(hlds_goal_expr,
    set_of_progvar, set_of_progvar).
:- mode goal_expr_vars_both_maybe_lambda_and_bi_impl(in, out, out) is det.

goal_expr_vars_both_maybe_lambda_and_bi_impl(GoalExpr, Set, LambdaSet) :-
    Set0 = set_of_var.init,
    LambdaSet0 = set_of_var.init,
    goal_expr_vars_maybe_lambda_and_bi_impl_2(GoalExpr, Set0, Set,
        LambdaSet0, LambdaSet).

:- pred goal_expr_vars_both_no_lambda(nonlocals_to_recompute, hlds_goal_expr,
    set_of_progvar).
:- mode goal_expr_vars_both_no_lambda(in(ord_nl_no_lambda),
    in, out) is det.
:- mode goal_expr_vars_both_no_lambda(in(cg_nl_no_lambda),
    in, out) is det.

goal_expr_vars_both_no_lambda(NonLocalsToRecompute, GoalExpr, Set) :-
    Set0 = set_of_var.init,
    goal_expr_vars_no_lambda_2(NonLocalsToRecompute, GoalExpr, Set0, Set).

:- pred goal_expr_vars_maybe_lambda_2(nonlocals_to_recompute, hlds_goal_expr,
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode goal_expr_vars_maybe_lambda_2(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, GoalExpr,
        !Set, !LambdaSet) :-
    (
        GoalExpr = unify(LHS, RHS, _, Unification, _),
        set_of_var.insert(LHS, !Set),
        (
            Unification = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, _)),
                set_of_var.insert(ReuseVar, !Set)
            ;
                How = construct_in_region(RegionVar),
                set_of_var.insert(RegionVar, !Set)
            ;
                ( How = construct_statically(_)
                ; How = construct_dynamically
                )
            ),
            ( if
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            then
                set_of_var.insert(SizeVar, !Set)
            else
                true
            )
        ;
            Unification = complicated_unify(_, _, TypeInfoVars),
            set_of_var.insert_list(TypeInfoVars, !Set)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
        ),
        unify_rhs_vars_maybe_lambda(NonLocalsToRecompute, RHS,
            !Set, !LambdaSet)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars1, _, _, _),
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        set_of_var.insert_list(ArgVars0, !Set),
        set_of_var.insert_list(ArgVars1, !Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        set_of_var.insert_list(AllVars, !Set)
    ;
        GoalExpr = conj(_, Goals),
        conj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet)
    ;
        GoalExpr = disj(Goals),
        disj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        set_of_var.insert(Var, !Set),
        case_vars_maybe_lambda(NonLocalsToRecompute, Cases, !Set, !LambdaSet)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        % This code does the following:
        %     !:Set = !.Set + ( (vars(Cond) + vars(Then)) \ Vars ) + vars(Else)
        % where `+' is set union and `\' is relative complement.
        goal_vars_both_maybe_lambda(NonLocalsToRecompute, Cond,
            CondSet, CondLambdaSet),
        goal_vars_both_maybe_lambda(NonLocalsToRecompute, Then,
            ThenSet, ThenLambdaSet),
        goal_vars_both_maybe_lambda(NonLocalsToRecompute, Else,
            ElseSet, ElseLambdaSet),
        set_of_var.union(CondSet, ThenSet, CondThenSet),
        set_of_var.union(CondLambdaSet, ThenLambdaSet, CondThenLambdaSet),
        set_of_var.delete_list(Vars, CondThenSet, SomeCondThenSet),
        set_of_var.delete_list(Vars, CondThenLambdaSet, SomeCondThenLambdaSet),
        set_of_var.union(!.Set, SomeCondThenSet, !:Set),
        set_of_var.union(!.LambdaSet, SomeCondThenLambdaSet, !:LambdaSet),
        set_of_var.union(!.Set, ElseSet, !:Set),
        set_of_var.union(!.LambdaSet, ElseLambdaSet, !:LambdaSet)
    ;
        GoalExpr = negation(SubGoal),
        SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
        goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, SubGoalExpr,
            !Set, !LambdaSet)
    ;
        GoalExpr = scope(Reason, SubGoal),
        Set0 = !.Set,
        LambdaSet0 = !.LambdaSet,
        (
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ),
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                !:Set, !:LambdaSet)
        ;
            Reason = exist_quant(Vars),
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.delete_list(Vars, !Set),
            set_of_var.delete_list(Vars, !LambdaSet)
        ;
            Reason = promise_solutions(Vars, _Kind),
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.insert_list(Vars, !Set)
        ;
            ( Reason = require_complete_switch(Var)
            ; Reason = require_switch_arms_detism(Var, _)
            ),
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.insert(Var, !Set)
        ;
            Reason = from_ground_term(TermVar, Kind),
            (
                ( Kind = from_ground_term_initial
                ; Kind = from_ground_term_construct
                ),
                !:Set = set_of_var.init,
                set_of_var.insert(TermVar, !Set),
                !:LambdaSet = set_of_var.init
            ;
                ( Kind = from_ground_term_deconstruct
                ; Kind = from_ground_term_other
                ),
                % Unfortunately, while there will never by any lambda goals
                % inside such a SubGoal when the scope is built, there may be
                % lambda goals inside SubGoal after typechecking.
                goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                    !:Set, !:LambdaSet)
                % TermVar should have been put into the relevant sets when we
                % processed SubGoal, since it should appear in SubGoal.
            )
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.insert(LCVar, !Set),
            set_of_var.insert(LCSVar, !Set)
        ),
        set_of_var.union(Set0, !Set),
        set_of_var.union(LambdaSet0, !LambdaSet)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            % XXX STM
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert_list([OuterDI, OuterUO, InnerDI, InnerUO], !Set),
            disj_vars_maybe_lambda(NonLocalsToRecompute,
                [MainGoal | OrElseGoals], !Set, !LambdaSet)
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            % IO state variables and ResultVar are already in SubGoal.
            SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
            goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, SubGoalExpr,
                !Set, !LambdaSet)
        ;
            ShortHand = bi_implication(LHS, RHS),
            conj_vars_maybe_lambda(NonLocalsToRecompute, [LHS, RHS],
                !Set, !LambdaSet)
        )
    ).

:- pred goal_expr_vars_maybe_lambda_and_bi_impl_2(hlds_goal_expr,
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode goal_expr_vars_maybe_lambda_and_bi_impl_2(
    in, in, out, in, out) is det.

goal_expr_vars_maybe_lambda_and_bi_impl_2(GoalExpr, !Set, !LambdaSet) :-
    (
        GoalExpr = unify(LHS, RHS, _, Unification, _),
        set_of_var.insert(LHS, !Set),
        (
            Unification = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, _)),
                set_of_var.insert(ReuseVar, !Set)
            ;
                How = construct_in_region(RegionVar),
                set_of_var.insert(RegionVar, !Set)
            ;
                ( How = construct_statically(_)
                ; How = construct_dynamically
                )
            ),
            ( if
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            then
                set_of_var.insert(SizeVar, !Set)
            else
                true
            )
        ;
            Unification = complicated_unify(_, _, TypeInfoVars),
            set_of_var.insert_list(TypeInfoVars, !Set)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
        ),
        unify_rhs_vars_maybe_lambda_and_bi_impl(RHS, !Set, !LambdaSet)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars1, _, _, _),
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        set_of_var.insert_list(ArgVars0, !Set),
        set_of_var.insert_list(ArgVars1, !Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        set_of_var.insert_list(AllVars, !Set)
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj
        ;
            ConjType = parallel_conj
        ),
        conj_vars_maybe_lambda_and_bi_impl(Goals, !Set, !LambdaSet)
    ;
        GoalExpr = disj(Goals),
        disj_vars_maybe_lambda_and_bi_impl(Goals, !Set, !LambdaSet)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        set_of_var.insert(Var, !Set),
        case_vars_maybe_lambda_and_bi_impl(Cases, !Set, !LambdaSet)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        % This code does the following:
        %     !:Set = !.Set + ( (vars(Cond) + vars(Then)) \ Vars ) + vars(Else)
        % where `+' is set union and `\' is relative complement.
        goal_vars_both_maybe_lambda_and_bi_impl(Cond, CondSet, CondLambdaSet),
        goal_vars_both_maybe_lambda_and_bi_impl(Then, ThenSet, ThenLambdaSet),
        goal_vars_both_maybe_lambda_and_bi_impl(Else, ElseSet, ElseLambdaSet),
        set_of_var.union(CondSet, ThenSet, CondThenSet),
        set_of_var.union(CondLambdaSet, ThenLambdaSet, CondThenLambdaSet),
        set_of_var.delete_list(Vars, CondThenSet, SomeCondThenSet),
        set_of_var.delete_list(Vars, CondThenLambdaSet, SomeCondThenLambdaSet),
        set_of_var.union(!.Set, SomeCondThenSet, !:Set),
        set_of_var.union(!.LambdaSet, SomeCondThenLambdaSet, !:LambdaSet),
        set_of_var.union(!.Set, ElseSet, !:Set),
        set_of_var.union(!.LambdaSet, ElseLambdaSet, !:LambdaSet)
    ;
        GoalExpr = negation(SubGoal),
        SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
        goal_expr_vars_maybe_lambda_and_bi_impl_2(SubGoalExpr,
            !Set, !LambdaSet)
    ;
        GoalExpr = scope(Reason, SubGoal),
        Set0 = !.Set,
        LambdaSet0 = !.LambdaSet,
        (
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet)
        ;
            Reason = exist_quant(Vars),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.delete_list(Vars, !Set),
            set_of_var.delete_list(Vars, !LambdaSet)
        ;
            Reason = promise_solutions(Vars, _Kind),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.insert_list(Vars, !Set)
        ;
            ( Reason = require_complete_switch(Var)
            ; Reason = require_switch_arms_detism(Var, _)
            ),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.insert(Var, !Set)
        ;
            Reason = from_ground_term(_TermVar, _Kind),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet)
            % TermVar should have been put into the relevant sets when we
            % processed SubGoal, since it should appear in SubGoal.
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet),
            set_of_var.insert(LCVar, !Set),
            set_of_var.insert(LCSVar, !Set)
        ),
        set_of_var.union(Set0, !Set),
        set_of_var.union(LambdaSet0, !LambdaSet)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            % XXX STM
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert_list([OuterDI, OuterUO, InnerDI, InnerUO], !Set),
            disj_vars_maybe_lambda_and_bi_impl([MainGoal | OrElseGoals],
                !Set, !LambdaSet)
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            % IO state variables and ResultVar are already in SubGoal.
            SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
            goal_expr_vars_maybe_lambda_and_bi_impl_2(SubGoalExpr,
                !Set, !LambdaSet)
        ;
            ShortHand = bi_implication(LHS, RHS),
            conj_vars_maybe_lambda_and_bi_impl([LHS, RHS], !Set, !LambdaSet)
        )
    ).

:- pred goal_expr_vars_no_lambda_2(nonlocals_to_recompute, hlds_goal_expr,
    set_of_progvar, set_of_progvar).
:- mode goal_expr_vars_no_lambda_2(in(ord_nl_no_lambda),
    in, in, out) is det.
:- mode goal_expr_vars_no_lambda_2(in(cg_nl_no_lambda),
    in, in, out) is det.

goal_expr_vars_no_lambda_2(NonLocalsToRecompute, GoalExpr, !Set) :-
    (
        GoalExpr = unify(LHS, RHS, _, Unification, _),
        set_of_var.insert(LHS, !Set),
        (
            Unification = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, SetArgs)),
                MaybeSetArgs = yes(SetArgs),
                set_of_var.insert(ReuseVar, !Set)
            ;
                How = construct_in_region(RegionVar),
                MaybeSetArgs = no,
                set_of_var.insert(RegionVar, !Set)
            ;
                ( How = construct_statically(_)
                ; How = construct_dynamically
                ),
                MaybeSetArgs = no
            ),
            ( if
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            then
                set_of_var.insert(SizeVar, !Set)
            else
                true
            )
        ;
            Unification = complicated_unify(_, _, TypeInfoVars),
            MaybeSetArgs = no,
            set_of_var.insert_list(TypeInfoVars, !Set)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            ),
            MaybeSetArgs = no
        ),
        unify_rhs_vars_no_lambda(NonLocalsToRecompute, RHS, MaybeSetArgs, !Set)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars1, _, _, _),
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        set_of_var.insert_list(ArgVars0, !Set),
        set_of_var.insert_list(ArgVars1, !Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        set_of_var.insert_list(AllVars, !Set)
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj
        ;
            ConjType = parallel_conj
        ),
        conj_vars_no_lambda(NonLocalsToRecompute, Goals, !Set)
    ;
        GoalExpr = disj(Goals),
        disj_vars_no_lambda(NonLocalsToRecompute, Goals, !Set)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        set_of_var.insert(Var, !Set),
        case_vars_no_lambda(NonLocalsToRecompute, Cases, !Set)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        % This code does the following:
        %     !:Set = !.Set + ( (vars(Cond) + vars(Then)) \ Vars ) + vars(Else)
        % where `+' is set union and `\' is relative complement.
        goal_vars_both_no_lambda(NonLocalsToRecompute, Cond, CondSet),
        goal_vars_both_no_lambda(NonLocalsToRecompute, Then, ThenSet),
        goal_vars_both_no_lambda(NonLocalsToRecompute, Else, ElseSet),
        set_of_var.union(CondSet, ThenSet, CondThenSet),
        set_of_var.delete_list(Vars, CondThenSet, SomeCondThenSet),
        set_of_var.union(!.Set, SomeCondThenSet, !:Set),
        set_of_var.union(!.Set, ElseSet, !:Set)
    ;
        GoalExpr = negation(SubGoal),
        SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
        goal_expr_vars_no_lambda_2(NonLocalsToRecompute, SubGoalExpr, !Set)
    ;
        GoalExpr = scope(Reason, SubGoal),
        Set0 = !.Set,
        (
            ( Reason = disable_warnings(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set)
        ;
            Reason = exist_quant(Vars),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set),
            set_of_var.delete_list(Vars, !Set)
        ;
            Reason = promise_solutions(Vars, _Kind),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set),
            set_of_var.insert_list(Vars, !Set)
        ;
            ( Reason = require_complete_switch(Var)
            ; Reason = require_switch_arms_detism(Var, _)
            ),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set),
            set_of_var.insert(Var, !Set)
        ;
            Reason = from_ground_term(_TermVar, _),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set)
            % _TermVar should have been put into the relevant sets when we
            % processed SubGoal, since it should appear in SubGoal.
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set),
            set_of_var.insert(LCVar, !Set),
            set_of_var.insert(LCSVar, !Set)
        ),
        set_of_var.union(Set0, !Set)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            % XXX STM
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            set_of_var.insert_list([OuterDI, OuterUO, InnerDI, InnerUO], !Set),
            disj_vars_no_lambda(NonLocalsToRecompute, [MainGoal | OrElseGoals],
                !Set)
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            % IO state variables and ResultVar are already in SubGoal.
            SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
            goal_expr_vars_no_lambda_2(NonLocalsToRecompute, SubGoalExpr, !Set)
        ;
            ShortHand = bi_implication(LHS, RHS),
            conj_vars_no_lambda(NonLocalsToRecompute, [LHS, RHS], !Set)
        )
    ).

:- pred unify_rhs_vars_maybe_lambda(nonlocals_to_recompute, unify_rhs,
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode unify_rhs_vars_maybe_lambda(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.

unify_rhs_vars_maybe_lambda(NonLocalsToRecompute, RHS, !Set, !LambdaSet) :-
    (
        RHS = rhs_var(Y),
        set_of_var.insert(Y, !Set)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, ArgVarsModes, _, Goal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        % Note that the NonLocals list is not counted, since all the
        % variables in that list must occur in the goal.
        goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, GoalVars0),
        set_of_var.delete_list(ArgVars, GoalVars0, GoalVars),
        set_of_var.union(!.LambdaSet, GoalVars, !:LambdaSet)
    ).

:- pred unify_rhs_vars_maybe_lambda_and_bi_impl(unify_rhs,
    set_of_progvar, set_of_progvar, set_of_progvar, set_of_progvar).
:- mode unify_rhs_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

unify_rhs_vars_maybe_lambda_and_bi_impl(RHS, !Set, !LambdaSet) :-
    (
        RHS = rhs_var(Y),
        set_of_var.insert(Y, !Set)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        set_of_var.insert_list(ArgVars, !Set)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, ArgVarsModes, _, Goal),
        assoc_list.keys(ArgVarsModes, ArgVars),
        % Note that the NonLocals list is not counted, since all the
        % variables in that list must occur in the goal.
        goal_vars_bitset_maybe_lambda_and_bi_impl(Goal, GoalVars0),
        set_of_var.delete_list(ArgVars, GoalVars0, GoalVars),
        set_of_var.union(!.LambdaSet, GoalVars, !:LambdaSet)
    ).

:- pred unify_rhs_vars_no_lambda(nonlocals_to_recompute, unify_rhs,
    maybe(list(needs_update)), set_of_progvar, set_of_progvar).
:- mode unify_rhs_vars_no_lambda(in(ord_nl_no_lambda),
    in, in, in, out) is det.
:- mode unify_rhs_vars_no_lambda(in(cg_nl_no_lambda),
    in, in, in, out) is det.

unify_rhs_vars_no_lambda(NonLocalsToRecompute, RHS, MaybeSetArgs, !Set) :-
    (
        RHS = rhs_var(Y),
        set_of_var.insert(Y, !Set)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        ( if
            NonLocalsToRecompute = cg_nl_no_lambda,
            MaybeSetArgs = yes(SetArgs)
        then
            % Ignore the fields taken from the reused cell.
            get_updated_fields(SetArgs, ArgVars, ArgsToSet),
            set_of_var.insert_list(ArgsToSet, !Set)
        else
            set_of_var.insert_list(ArgVars, !Set)
        )
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _),
        unexpected($pred, "found lambda")
    ).

:- pred get_updated_fields(list(needs_update)::in,
    list(prog_var)::in, list(prog_var)::out) is det.

get_updated_fields(SetArgs, Args, ArgsToSet) :-
    get_updated_fields_acc(SetArgs, Args, [], ArgsToSet).

:- pred get_updated_fields_acc(list(needs_update)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::out) is det.

get_updated_fields_acc([], [], !ArgsToSet).
get_updated_fields_acc([], [_|_], _, _) :-
    unexpected($pred, "mismatched lists").
get_updated_fields_acc([_|_], [], _, _) :-
    unexpected($pred, "mismatched lists").
get_updated_fields_acc([SetArg | SetArgs], [Arg | Args], !ArgsToSet) :-
    (
        SetArg = needs_update,
        !:ArgsToSet = [Arg | !.ArgsToSet]
    ;
        SetArg = does_not_need_update,
        !:ArgsToSet = !.ArgsToSet
    ),
    get_updated_fields_acc(SetArgs, Args, !ArgsToSet).

%---------------------------------------------------------------------------%

:- pred warn_overlapping_scope(set_of_progvar::in, prog_context::in,
    quant_info::in, quant_info::out) is det.

warn_overlapping_scope(OverlapVars, Context, !Info) :-
    Vars = to_sorted_list(OverlapVars),
    get_warnings(!.Info, Warnings0),
    Warnings = [warn_overlap(Vars, Context) | Warnings0],
    set_warnings(Warnings, !Info).

%---------------------------------------------------------------------------%

    % rename_vars_apart(NonLocalsToRecompute, RenameSet, RenameMap,
    %   Goal0, Goal, !Info):
    %
    % For each variable V in RenameSet, create a fresh variable V',
    % and insert the mapping V->V' into RenameMap. Apply RenameMap to Goal0
    % giving Goal.
    %
:- pred rename_vars_apart(nonlocals_to_recompute, set_of_progvar,
    map(prog_var, prog_var), hlds_goal, hlds_goal, quant_info, quant_info).
:- mode rename_vars_apart(in(ord_nl_maybe_lambda),
    in, out, in, out, in, out) is det.
:- mode rename_vars_apart(in(ord_nl_no_lambda),
    in, out, in, out, in, out) is det.
:- mode rename_vars_apart(in(cg_nl_no_lambda),
    in, out, in, out, in, out) is det.

rename_vars_apart(NonLocalsToRecompute, RenameSet, RenameMap, !Goal, !Info) :-
    ( if
        % Don't rename apart variables when recomputing the code-gen nonlocals
        % -- that would stuff up the ordinary nonlocals and the mode
        % information. The ordinary nonlocals are always recomputed
        % before the code-gen nonlocals -- any necessary renaming will have
        % been done while recomputing the ordinary nonlocals.

        ( set_of_var.is_empty(RenameSet)
        ; NonLocalsToRecompute = cg_nl_no_lambda
        )
    then
        map.init(RenameMap)
    else
        RenameList = to_sorted_list(RenameSet),
        get_var_db(!.Info, VarDb0),
        map.init(RenameMap0),
        (
            VarDb0 = var_db_varset_vartypes(VarSetTypes0),
            VarSetTypes0 = prog_var_set_types(VarSet0, VarTypes0),
            clone_variables_vs(RenameList, VarSet0, VarTypes0,
                VarSet0, VarSet, VarTypes0, VarTypes, RenameMap0, RenameMap),
            VarSetTypes = prog_var_set_types(VarSet, VarTypes),
            VarDb = var_db_varset_vartypes(VarSetTypes)
        ;
            VarDb0 = var_db_var_table(VarTable0),
            clone_variables(RenameList, VarTable0,
                VarTable0, VarTable, RenameMap0, RenameMap),
            VarDb = var_db_var_table(VarTable)
        ),
        rename_some_vars_in_goal(RenameMap, !Goal),
        set_var_db(VarDb, !Info)

        % We don't need to add the newly created vars to the seen vars,
        % because we won't find them anywhere else in the enclosing goal.
        % This is a performance improvement, as it reduces the size of the set
        % of seen vars.
        % get_seen(!.Info, SeenVars0),
        % map.values(RenameMap, NewVarsList),
        % set_of_var.insert_list(NewVarsList, SeenVars0, SeenVars),
        % set_seen(SeenVars, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred set_goal_nonlocals(nonlocals_to_recompute, set_of_progvar,
    hlds_goal_info, hlds_goal_info, quant_info, quant_info).
:- mode set_goal_nonlocals(in(ord_nl_maybe_lambda),
    in, in, out, in, out) is det.
:- mode set_goal_nonlocals(in(ord_nl_no_lambda),
    in, in, out, in, out) is det.
:- mode set_goal_nonlocals(in(cg_nl_no_lambda),
    in, in, out, in, out) is det.

set_goal_nonlocals(NonLocalsToRecompute, NonLocals, !GoalInfo, !Info) :-
    (
        ( NonLocalsToRecompute = ord_nl_maybe_lambda
        ; NonLocalsToRecompute = ord_nl_no_lambda
        ),
        goal_info_set_nonlocals(NonLocals, !GoalInfo)
    ;
        NonLocalsToRecompute = cg_nl_no_lambda,
        goal_info_set_code_gen_nonlocals(NonLocals, !GoalInfo)
    ).

%---------------------------------------------------------------------------%

    % `OutsideVars' are the variables that have occurred free outside
    % this goal, not counting occurrences in parallel goals and not
    % counting occurrences in lambda goals, or which have been explicitly
    % existentially quantified over a scope which includes the current
    % goal in a negated context.
    %
    % `OutsideLambdaVars' are the variables that have occurred free in
    % a lambda expression outside this goal, not counting occurrences in
    % parallel goals (and if this goal is itself inside a lambda
    % expression, not counting occurrences outside that lambda expression).
    %
    % `QuantVars' are the variables not in `OutsideVars' that have been
    % explicitly existentially quantified over a scope which includes the
    % current goal in a positive (non-negated) context.
    %
    % For example, consider
    %
    %   test :- some [X] (p(X) ; not q(X) ; r(X), s(X)).
    %
    % When processing `r(X), s(X)':
    %   OutsideVars will be [] and QuantifiedVars will be [X].
    % When processing `r(X)':
    %   OutsideVars will be [X] and QuantifiedVars will be [],
    %   since now [X] has occured in a goal (`s(X)') outside of `r(X)'.
    % When processing `not q(X)':
    %   OutsideVars will be [] and QuantifiedVars will be [X].
    % When processing `q(X)':
    %   OutsideVars will be [X] and QuantifiedVars will be [],
    %   since the quantification can't be pushed inside the negation.
    %
    % The `outside vars', `lambda outside vars', and `quant vars' fields
    % are inputs; the `nonlocals' field is output; and the `seen so far',
    % the varset, the types, rtti_varmaps, and the warnings fields are
    % threaded (i.e. both input and output).
    % We use the convention that the input fields are callee save,
    % and the outputs are caller save.
    %
:- type quant_info
    --->    quant_info(
                qi_outside              :: set_of_progvar,
                qi_lambda_outside       :: set_of_progvar,
                qi_quant_vars           :: set_of_progvar,
                qi_nonlocals            :: set_of_progvar,
                qi_seen                 :: set_of_progvar,
                qi_var_db               :: var_db,
                qi_rtti_varmaps         :: rtti_varmaps,
                qi_warnings             :: list(quant_warning)
            ).

:- pred init_quant_info(set_of_progvar::in, var_db::in, rtti_varmaps::in,
    quant_info::out) is det.

init_quant_info(OutsideVars, VarDb, RttiVarMaps, QuantInfo) :-
    LambdaOutsideVars = set_of_var.init,
    QuantVars = set_of_var.init,
    NonLocals = set_of_var.init,
    Seen = OutsideVars,
    OverlapWarnings = [],
    QuantInfo = quant_info(OutsideVars, LambdaOutsideVars, QuantVars,
        NonLocals, Seen, VarDb, RttiVarMaps, OverlapWarnings).

:- pred get_outside(quant_info::in, set_of_progvar::out) is det.
:- pred get_lambda_outside(quant_info::in, set_of_progvar::out) is det.
:- pred get_quant_vars(quant_info::in, set_of_progvar::out) is det.
:- pred get_nonlocals(quant_info::in, set_of_progvar::out) is det.
:- pred get_seen(quant_info::in, set_of_progvar::out) is det.
:- pred get_var_db(quant_info::in, var_db::out) is det.
:- pred get_rtti_varmaps(quant_info::in, rtti_varmaps::out) is det.
:- pred get_warnings(quant_info::in, list(quant_warning)::out) is det.
:- pragma inline(pred(get_outside/2)).
:- pragma inline(pred(get_lambda_outside/2)).
:- pragma inline(pred(get_quant_vars/2)).
:- pragma inline(pred(get_nonlocals/2)).
:- pragma inline(pred(get_seen/2)).
:- pragma inline(pred(get_var_db/2)).
:- pragma inline(pred(get_rtti_varmaps/2)).
:- pragma inline(pred(get_warnings/2)).

:- pred set_outside(set_of_progvar::in,
    quant_info::in, quant_info::out) is det.
:- pred set_lambda_outside(set_of_progvar::in,
    quant_info::in, quant_info::out) is det.
:- pred set_quant_vars(set_of_progvar::in,
    quant_info::in, quant_info::out) is det.
:- pred set_nonlocals(set_of_progvar::in,
    quant_info::in, quant_info::out) is det.
:- pred set_seen(set_of_progvar::in,
    quant_info::in, quant_info::out) is det.
:- pred set_var_db(var_db::in,
    quant_info::in, quant_info::out) is det.
:- pred set_rtti_varmaps(rtti_varmaps::in,
    quant_info::in, quant_info::out) is det.
:- pred set_warnings(list(quant_warning)::in,
    quant_info::in, quant_info::out) is det.
:- pragma inline(pred(set_outside/3)).
:- pragma inline(pred(set_lambda_outside/3)).
:- pragma inline(pred(set_quant_vars/3)).
:- pragma inline(pred(set_nonlocals/3)).
:- pragma inline(pred(set_seen/3)).
:- pragma inline(pred(set_var_db/3)).
:- pragma inline(pred(set_rtti_varmaps/3)).
:- pragma inline(pred(set_warnings/3)).

get_outside(Q, X) :-
    X = Q ^ qi_outside.
get_lambda_outside(Q, X) :-
    X = Q ^ qi_lambda_outside.
get_quant_vars(Q, X) :-
    X = Q ^ qi_quant_vars.
get_nonlocals(Q, X) :-
    X = Q ^ qi_nonlocals.
get_seen(Q, X) :-
    X = Q ^ qi_seen.
get_var_db(Q, X) :-
    X = Q ^ qi_var_db.
get_rtti_varmaps(Q, X) :-
    X = Q ^ qi_rtti_varmaps.
get_warnings(Q, X) :-
    X = Q ^ qi_warnings.

set_outside(X, !Q) :-
    !Q ^ qi_outside := X.
set_lambda_outside(X, !Q) :-
    ( if private_builtin.pointer_equal(X, !.Q ^ qi_lambda_outside) then
        true
    else
        !Q ^ qi_lambda_outside := X
    ).
set_quant_vars(X, !Q) :-
    ( if private_builtin.pointer_equal(X, !.Q ^ qi_quant_vars) then
        true
    else
        !Q ^ qi_quant_vars := X
    ).
set_nonlocals(X, !Q) :-
    !Q ^ qi_nonlocals := X.
set_seen(X, !Q) :-
    !Q ^ qi_seen := X.
set_var_db(X, !Q) :-
    ( if private_builtin.pointer_equal(X, !.Q ^ qi_var_db) then
        true
    else
        !Q ^ qi_var_db := X
    ).
set_rtti_varmaps(X, !Q) :-
    ( if private_builtin.pointer_equal(X, !.Q ^ qi_rtti_varmaps) then
        true
    else
        !Q ^ qi_rtti_varmaps := X
    ).
set_warnings(X, !Q) :-
    !Q ^ qi_warnings := X.

% Access stats for the quant_info structure, derived on 2017 june 16:
%
% i      read      same      diff   same%
% 0  25189204   3601135  34593857   9.43%   outside
% 1  23593747  21960637    400666  98.21%   lambda_outside
% 2    559405    977415     42142  95.87%   quant_vars
% 3  42937242    465766  27971550   1.64%   nonlocals
% 4  29012880    364293  12366310   2.86%   seen
% 5   1796731         0     59596   0.00%   varset
% 6   1796731     45951     13645  77.10%   vartypes
% 7   1796535     59105       295  99.50%   rrti_varmaps
% 8   1737148         0        13   0.00%   quant_warnings

%---------------------------------------------------------------------------%
:- end_module hlds.quantification.
%---------------------------------------------------------------------------%
