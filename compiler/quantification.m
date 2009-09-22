%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
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
% be stored in the goal_info (we currently don't do that).
%
% The important piece of information that later stages of the compiler want to
% know is "Does this goal bind any of its nonlocal variables?".  So, rather
% than storing a list of the variables which _are_ existentially quantified in
% the goal_info, we store the set of variables which are _not_ quantified.
%
%-----------------------------------------------------------------------------%

:- module hlds.quantification.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

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
    --->    ordinary_nonlocals_maybe_lambda
    ;       ordinary_nonlocals_no_lambda
    ;       code_gen_nonlocals_no_lambda.

:- pred implicitly_quantify_clause_body_general(nonlocals_to_recompute::in,
    list(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

:- pred implicitly_quantify_goal_general(nonlocals_to_recompute::in,
    set(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

:- pred requantify_proc_general(nonlocals_to_recompute::in,
    proc_info::in, proc_info::out) is det.

    % We return a list of warnings back to make_hlds.m.
    % Currently the only thing we warn about is variables with
    % overlapping scopes.

:- type quant_warning
    --->    warn_overlap(list(prog_var), prog_context).

    % free_goal_vars(Goal) = Vars:
    %
    % Vars is the set of variables that occur free (unquantified) in Goal
    % excluding unset fields of reconstructions if
    % NonLocalsToRecompute is `code_gen_nonlocals_no_lambda'.
    %
:- func free_goal_vars(hlds_goal) = set(prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module tree_bitset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % The `outside vars', `lambda outside vars', and `quant vars'
    % fields are inputs; the `nonlocals' field is output; and
    % the `seen so far', the varset, the types, rtti_varmaps, and the
    % warnings fields are threaded (i.e. both input and output).
    % We use the convention that the input fields are callee save,
    % and the outputs are caller save.
    % The nonlocals_to_recompute field is constant.
    %
:- type quant_info
    --->    quant_info(
                qi_outside              :: set_of_var,
                qi_quant_vars           :: set_of_var,
                qi_lambda_outside       :: set_of_var,
                qi_nonlocals            :: set_of_var,
                qi_seen                 :: set_of_var,
                qi_varset               :: prog_varset,
                qi_vartypes             :: vartypes,
                qi_warnings             :: list(quant_warning),
                qi_rtti_varmaps         :: rtti_varmaps
            ).

    % Until we have user-specified pretty printing in the debugger,
    % debugging will be much easier if set_of_var is just `set(prog_var)'.
    % None of the calls to the predicates and functions operating on sets
    % in this module are module qualified so we can switch representation
    % just by changing this line.
    %
    % If you want to debug a new set representation, just import a version
    % of the bitset_tester module from tests/hard_coded, and make set_of_var
    % equivalent to the bitset_tester type.
% :- type set_of_var == set(prog_var).
:- type set_of_var == tree_bitset(prog_var).

    % `OutsideVars' are the variables that have occurred free outside
    % this goal, not counting occurrences in parallel goals and not
    % counting occurrences in lambda goals, or which have been explicitly
    % existentially quantified over a scope which includes the current
    % goal in a negated context.
    %
    % `QuantVars' are the variables not in `OutsideVars' that have been
    % explicitly existentially quantified over a scope which includes the
    % current goal in a positive (non-negated) context.
    %
    % `OutsideLambdaVars' are the variables that have occurred free in
    % a lambda expression outside this goal, not counting occurrences in
    % parallel goals (and if this goal is itself inside a lambda
    % expression, not counting occurrences outside that lambda expression).
    %
    % For example, for
    %
    %   test :- some [X] (p(X) ; not q(X) ; r(X), s(X)).
    %
    % when processing `r(X), s(X)', OutsideVars will be [] and
    % QuantifiedVars will be [X]; when processing `r(X)',
    % OutsideVars will be [X] and QuantifiedVars will be [],
    % since now [X] has occured in a goal (`s(X)') outside of `r(X)'.
    % When processing `not q(X)', OutsideVars will be [] and
    % QuantifiedVars will be [X]; when processing `q(X)',
    % OutsideVars will be [X] and QuantifiedVars will be [],
    % since the quantification can't be pushed inside the negation.

:- inst ordinary_nonlocals_maybe_lambda ---> ordinary_nonlocals_maybe_lambda.
:- inst ordinary_nonlocals_no_lambda ---> ordinary_nonlocals_no_lambda.
:- inst code_gen_nonlocals_no_lambda ---> code_gen_nonlocals_no_lambda.

%-----------------------------------------------------------------------------%

implicitly_quantify_clause_body_general(NonLocalsToRecompute, HeadVars,
        Warnings, !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    list_to_set(HeadVars, OutsideVars),
    implicitly_quantify_goal_general(NonLocalsToRecompute, OutsideVars,
        Warnings, !Goal, !Varset, !VarTypes, !RttiVarMaps).

requantify_proc_general(NonLocalsToRecompute, !ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_varset(!.ProcInfo, Varset0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarmaps0),
    implicitly_quantify_clause_body_general(NonLocalsToRecompute, HeadVars, _,
        Goal0, Goal, Varset0, Varset, VarTypes0, VarTypes,
        RttiVarmaps0, RttiVarmaps),
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarmaps, !ProcInfo).

implicitly_quantify_goal_general(NonLocalsToRecompute, OutsideVars, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    (
        NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda,
        implicitly_quantify_goal_2(ordinary_nonlocals_maybe_lambda,
            OutsideVars, Warnings, !Goal, !Varset, !VarTypes, !RttiVarMaps)
    ;
        ( NonLocalsToRecompute = ordinary_nonlocals_no_lambda
        ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
        ),
        implicitly_quantify_goal_2(ordinary_nonlocals_no_lambda,
            OutsideVars, Warnings, !Goal, !Varset, !VarTypes, !RttiVarMaps)
    ),
    (
        NonLocalsToRecompute = code_gen_nonlocals_no_lambda,

        % If the goal does not contain a reconstruction, the code-gen nonlocals
        % and the ordinary nonlocals are the same.
        goal_contains_reconstruction(!.Goal)
    ->
        implicitly_quantify_goal_2(code_gen_nonlocals_no_lambda, OutsideVars,
            _, !Goal, !Varset, !VarTypes, !RttiVarMaps)
    ;
        true
    ).

:- pred implicitly_quantify_goal_2(nonlocals_to_recompute,
    set(prog_var), list(quant_warning),
    hlds_goal, hlds_goal, prog_varset, prog_varset,
    vartypes, vartypes, rtti_varmaps, rtti_varmaps) is det.
:- mode implicitly_quantify_goal_2(in(ordinary_nonlocals_maybe_lambda),
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode implicitly_quantify_goal_2(in(ordinary_nonlocals_no_lambda),
    in, out, in, out, in, out, in, out, in, out) is det.
:- mode implicitly_quantify_goal_2(in(code_gen_nonlocals_no_lambda),
    in, out, in, out, in, out, in, out, in, out) is det.

implicitly_quantify_goal_2(NonLocalsToRecompute, OutsideVars0, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    OutsideVars = set_to_bitset(OutsideVars0),
    init_quant_info(OutsideVars, !.Varset, !.VarTypes, !.RttiVarMaps,
        QuantInfo0),
    implicitly_quantify_goal_quant_info(!Goal, NonLocalsToRecompute,
        QuantInfo0, QuantInfo),
    get_varset(QuantInfo, !:Varset),
    get_vartypes(QuantInfo, !:VarTypes),
    get_warnings(QuantInfo, Warnings0),
    get_rtti_varmaps(QuantInfo, !:RttiVarMaps),
    list.reverse(Warnings0, Warnings).

:- pred implicitly_quantify_goal_quant_info(hlds_goal, hlds_goal,
    nonlocals_to_recompute, quant_info, quant_info).
:- mode implicitly_quantify_goal_quant_info(in, out,
    in(ordinary_nonlocals_maybe_lambda), in, out) is det.
:- mode implicitly_quantify_goal_quant_info(in, out,
    in(ordinary_nonlocals_no_lambda), in, out) is det.
:- mode implicitly_quantify_goal_quant_info(in, out,
    in(code_gen_nonlocals_no_lambda), in, out) is det.

implicitly_quantify_goal_quant_info(Goal0, Goal, NonLocalsToRecompute,
        !Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    get_seen(!.Info, SeenVars),
    implicitly_quantify_goal_quant_info_2(GoalExpr0, GoalExpr1, GoalInfo0,
        NonLocalsToRecompute, PossiblyNonLocalGoalVars0, !Info),
    get_nonlocals(!.Info, NonLocalVars),
    difference(PossiblyNonLocalGoalVars0, NonLocalVars, LocalVars),
    intersect(SeenVars, LocalVars, RenameVars),
    % If there are any variables that are local to the goal
    % which we have come across before, then we rename them apart.
    ( empty(RenameVars) ->
        GoalExpr = GoalExpr1,
        GoalInfo1 = GoalInfo0
    ;
        rename_apart(RenameVars, RenameMap, NonLocalsToRecompute,
            hlds_goal(GoalExpr1, GoalInfo0), hlds_goal(GoalExpr, GoalInfo1),
            !Info),

        % Make sure that the information in the RTTI varmaps is updated
        % to reflect any new variables that we may have just introduced.

        some [!RttiVarMaps] (
            get_rtti_varmaps(!.Info, !:RttiVarMaps),
            map.foldl(rtti_var_info_duplicate, RenameMap, !RttiVarMaps),
            set_rtti_varmaps(!.RttiVarMaps, !Info)
        )
    ),
    set_goal_nonlocals_translate(NonLocalVars, NonLocals, NonLocalsToRecompute,
        GoalInfo1, GoalInfo2, !Info),

    % If the nonlocals set has shrunk (e.g. because some optimization
    % optimizes away the other occurrences of a variable, causing it
    % to become local when previously it was nonlocal),
    % then we may need to likewise shrink the instmap delta.

    InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo2),
    instmap_delta_restrict(NonLocals, InstMapDelta0, InstMapDelta),
    goal_info_set_instmap_delta(InstMapDelta, GoalInfo2, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % After this pass, explicit quantifiers are redundant, since all variables
    % which were explicitly quantified have been renamed apart. So we don't
    % keep them. We need to keep the structure, though, so that mode analysis
    % doesn't try to reorder through quantifiers. (Actually it would make sense
    % to allow mode analysis to do that, but the reference manual says it
    % doesn't, so we don't.)  Thus we replace `scope(exist_quant(Vars), Goal0)'
    % with an empty quantifier `scope(exist_quant([]), Goal)'.
    %
    % We pass GoalInfo0 to allow warnings to have the proper context. We don't
    % pass the context itself to avoid the work of extracting the context from
    % goal_infos in the usual (no warning) case.
    %
:- pred implicitly_quantify_goal_quant_info_2(hlds_goal_expr, hlds_goal_expr,
    hlds_goal_info, nonlocals_to_recompute, set_of_var,
    quant_info, quant_info).
:- mode implicitly_quantify_goal_quant_info_2(in, out, in,
    in(ordinary_nonlocals_maybe_lambda), out, in, out) is det.
:- mode implicitly_quantify_goal_quant_info_2(in, out, in,
    in(ordinary_nonlocals_no_lambda), out, in, out) is det.
:- mode implicitly_quantify_goal_quant_info_2(in, out, in,
    in(code_gen_nonlocals_no_lambda), out, in, out) is det.

implicitly_quantify_goal_quant_info_2(GoalExpr0, GoalExpr, GoalInfo0,
        NonLocalsToRecompute, PossiblyNonLocalGoalVars0, !Info) :-
    (
        GoalExpr0 = scope(Reason0, SubGoal0),
        implicitly_quantify_goal_quant_info_scope(Reason0, SubGoal0,
            GoalExpr, GoalInfo0, NonLocalsToRecompute,
            PossiblyNonLocalGoalVars0, !Info)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda,
            implicitly_quantify_conj_maybe_lambda(Goals0, Goals,
                NonLocalsToRecompute, PossiblyNonLocalGoalVars0, !Info),
            GoalExpr = conj(ConjType, Goals)
        ;
            ( NonLocalsToRecompute = ordinary_nonlocals_no_lambda
            ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
            ),
            implicitly_quantify_conj_no_lambda(Goals0, Goals,
                NonLocalsToRecompute, PossiblyNonLocalGoalVars0, !Info),
            GoalExpr = conj(ConjType, Goals)
        )
    ;
        GoalExpr0 = disj(Goals0),
        NonLocalVarSets0 = [],
        implicitly_quantify_disj(Goals0, Goals, NonLocalsToRecompute, !Info,
            NonLocalVarSets0, NonLocalVarSets),
        union_list(NonLocalVarSets, NonLocalVars),
        set_nonlocals(NonLocalVars, !Info),
        GoalExpr = disj(Goals),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = switch(Var, Det, Cases0),
        NonLocalVarSets0 = [],
        implicitly_quantify_cases(Cases0, Cases, NonLocalsToRecompute, !Info,
            NonLocalVarSets0, NonLocalVarSets),
        % The switch variable is guaranteed to be nonlocal to the switch, since
        % it has to be bound elsewhere, so we put it in the nonlocals here.
        union_list(NonLocalVarSets, NonLocalVars0),
        insert(NonLocalVars0, Var, NonLocalVars),
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
        union(OutsideVars, QuantVars, OutsideVars1),
        QuantVars1 = init,
        set_quant_vars(QuantVars1, !Info),
        set_outside(OutsideVars1, !Info),
        implicitly_quantify_goal_quant_info(SubGoal0, SubGoal,
            NonLocalsToRecompute, !Info),
        GoalExpr = negation(SubGoal),
        set_outside(OutsideVars, !Info),
        set_quant_vars(QuantVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = if_then_else(Vars0, Cond0, Then0, Else0),
        % After this pass, explicit quantifiers are redundant, since all
        % variables which were explicitly quantified have been renamed apart.
        % So we don't keep them. Thus we replace `if_then_else(Vars, ....)'
        % with `if_then_else([], ...)'.

        get_quant_vars(!.Info, QuantVars),
        get_outside(!.Info, OutsideVars),
        get_lambda_outside(!.Info, LambdaOutsideVars),
        QVars = list_to_set(Vars0),
        % Rename apart those variables that are quantified to the cond and then
        % of the i-t-e that occur outside the i-t-e.
        intersect(OutsideVars, QVars, RenameVars1),
        intersect(LambdaOutsideVars, QVars, RenameVars2),
        union(RenameVars1, RenameVars2, RenameVars),
        ( empty(RenameVars) ->
            Cond1 = Cond0,
            Then1 = Then0,
            Vars = Vars0
        ;
            Context = goal_info_get_context(GoalInfo0),
            warn_overlapping_scope(RenameVars, Context, !Info),
            rename_apart(RenameVars, RenameMap, NonLocalsToRecompute,
                Cond0, Cond1, !Info),
            rename_some_vars_in_goal(RenameMap, Then0, Then1),
            rename_var_list(need_not_rename, RenameMap, Vars0, Vars)
        ),
        insert_list(QuantVars, Vars, QuantVars1),
        (
            NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda,
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, Then1,
                VarsThen, LambdaVarsThen)
        ;
            ( NonLocalsToRecompute = ordinary_nonlocals_no_lambda
            ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
            ),
            goal_vars_both_no_lambda(NonLocalsToRecompute, Then1, VarsThen),
            LambdaVarsThen = init
        ),
        union(OutsideVars, VarsThen, OutsideVars1),
        union(LambdaOutsideVars, LambdaVarsThen, LambdaOutsideVars1),
        set_quant_vars(QuantVars1, !Info),
        set_outside(OutsideVars1, !Info),
        set_lambda_outside(LambdaOutsideVars1, !Info),
        update_seen_vars(QVars, !Info),
        implicitly_quantify_goal_quant_info(Cond1, Cond, NonLocalsToRecompute,
            !Info),
        get_nonlocals(!.Info, NonLocalsCond),
        union(OutsideVars, NonLocalsCond, OutsideVars2),
        set_outside(OutsideVars2, !Info),
        set_lambda_outside(LambdaOutsideVars, !Info),
        implicitly_quantify_goal_quant_info(Then1, Then, NonLocalsToRecompute,
            !Info),
        get_nonlocals(!.Info, NonLocalsThen),
        set_outside(OutsideVars, !Info),
        set_quant_vars(QuantVars, !Info),
        implicitly_quantify_goal_quant_info(Else0, Else, NonLocalsToRecompute,
            !Info),
        GoalExpr = if_then_else([], Cond, Then, Else),

        get_nonlocals(!.Info, NonLocalsElse),
        union(NonLocalsCond, NonLocalsThen, NonLocalsIfThen),
        union(NonLocalsIfThen, NonLocalsElse, NonLocalsIfThenElse),
        intersect(NonLocalsIfThenElse, OutsideVars, NonLocalsO),
        intersect(NonLocalsIfThenElse, LambdaOutsideVars, NonLocalsL),
        union(NonLocalsO, NonLocalsL, NonLocals),
        set_nonlocals(NonLocals, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = plain_call(_, _, HeadVars, _, _, _),
        GoalExpr = GoalExpr0,
        implicitly_quantify_primitive_goal(HeadVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = generic_call(GenericCall, CallArgVars, _, _),
        GoalExpr = GoalExpr0,
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        list.append(ArgVars0, CallArgVars, ArgVars),
        implicitly_quantify_primitive_goal(ArgVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = unify(Var, UnifyRHS0, Mode, Unification0, UnifyContext),
        get_outside(!.Info, OutsideVars),
        get_lambda_outside(!.Info, LambdaOutsideVars),
        TypeInfoVars = get_unify_typeinfos(Unification0),
        (
            Unification0 = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar0, _, SetArgs)),
                MaybeSetArgs = yes(SetArgs),
                MaybeReuseVar = yes(ReuseVar0),
                MaybeRegionVar = no
            ;
                How = construct_in_region(RegionVar0),
                MaybeSetArgs = no,
                MaybeReuseVar = no,
                MaybeRegionVar = yes(RegionVar0)
            ;
                ( How = construct_statically
                ; How = construct_dynamically
                ),
                MaybeSetArgs = no,
                MaybeReuseVar = no,
                MaybeRegionVar = no
            ),
            (
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar0))
            ->
                MaybeSizeVar = yes(SizeVar0)
            ;
                MaybeSizeVar = no
            )
        ;
            ( Unification0 = deconstruct(_, _, _, _, _, _)
            ; Unification0 = assign(_, _)
            ; Unification0 = simple_test(_, _)
            ; Unification0 = complicated_unify(_, _, _)
            ),
            MaybeSetArgs = no,
            MaybeReuseVar = no,
            MaybeSizeVar = no,
            MaybeRegionVar = no
        ),
        implicitly_quantify_unify_rhs(MaybeSetArgs, GoalInfo0,
            UnifyRHS0, UnifyRHS, Unification0, Unification,
            NonLocalsToRecompute, !Info),
        GoalExpr = unify(Var, UnifyRHS, Mode, Unification, UnifyContext),
        get_nonlocals(!.Info, VarsUnifyRHS),
        insert(VarsUnifyRHS, Var, GoalVars0),
        insert_list(GoalVars0, TypeInfoVars, GoalVars1),
        (
            MaybeReuseVar = yes(ReuseVar),
            insert(GoalVars1, ReuseVar, GoalVars2)
        ;
            MaybeReuseVar = no,
            GoalVars2 = GoalVars1
        ),
        (
            MaybeSizeVar = yes(SizeVar),
            insert(GoalVars2, SizeVar, GoalVars3)
        ;
            MaybeSizeVar = no,
            GoalVars3 = GoalVars2
        ),
        (
            MaybeRegionVar = yes(RegionVar),
            insert(GoalVars3, RegionVar, GoalVars)
        ;
            MaybeRegionVar = no,
            GoalVars = GoalVars3
        ),
        update_seen_vars(GoalVars, !Info),
        intersect(GoalVars, OutsideVars, NonLocalVars1),
        intersect(GoalVars, LambdaOutsideVars, NonLocalVars2),
        union(NonLocalVars1, NonLocalVars2, NonLocalVars),
        set_nonlocals(NonLocalVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        GoalExpr = GoalExpr0,
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        implicitly_quantify_primitive_goal(AllVars, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
                MainGoal0, OrElseGoals0, OrElseInners0),
            % The call to implicitly_quantify_disj causes the inner STM
            % interface variables to be renamed in any or_else goals, but
            % doing it first explicitly allows the new names of these
            % variables to be stored.
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
            AllAtomicGoals0 = [MainGoal0 | OrElseGoals1],
            NonLocalVarSets0 = [],
            implicitly_quantify_disj(AllAtomicGoals0, AllAtomicGoals,
                NonLocalsToRecompute, !Info,
                NonLocalVarSets0, NonLocalVarSets),
            (
                AllAtomicGoals = [MainGoal | OrElseGoals]
            ;
                AllAtomicGoals = [],
                unexpected(this_file,
                    "implicitly_quantify_goal_quant_info_2: " ++
                    "AllAtomicGoals = []")
            ),
            union_list(NonLocalVarSets, NonLocalVars0),
            (
                GoalType = unknown_atomic_goal_type,
                Outer = atomic_interface_vars(OuterDI, OuterUO),
                Inner = atomic_interface_vars(InnerDI, InnerUO),
                insert_list(NonLocalVars0, [OuterDI, OuterUO], NonLocalVars1),
                delete_list(NonLocalVars1, [InnerDI, InnerUO], NonLocalVars)
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
            implicitly_quantify_goal_quant_info(SubGoal0, SubGoal,
                NonLocalsToRecompute, !Info),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
            GoalExpr = shorthand(ShortHand)
        ;
            ShortHand0 = bi_implication(LHS, RHS),
            (
                NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda,
                implicitly_quantify_goal_quant_info_bi_implication(LHS, RHS,
                    GoalExpr, GoalInfo0, !Info)
            ;
                ( NonLocalsToRecompute = ordinary_nonlocals_no_lambda
                ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
                ),
                % Any bi_implications should have been expanded out by now.
                unexpected(this_file,
                    "implicitly_quantify_goal_quant_info_2: bi_implication")
            )
        ),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0)
    ).

:- pred rename_or_else_inner_vars(nonlocals_to_recompute,
    atomic_interface_vars, list(hlds_goal), list(hlds_goal),
    list(atomic_interface_vars), quant_info, quant_info).
:- mode rename_or_else_inner_vars(in(ordinary_nonlocals_maybe_lambda),
    in, in, out, out, in, out) is det.
:- mode rename_or_else_inner_vars(in(ordinary_nonlocals_no_lambda),
    in, in, out, out, in, out) is det.
:- mode rename_or_else_inner_vars(in(code_gen_nonlocals_no_lambda),
    in, in, out, out, in, out) is det.

rename_or_else_inner_vars(_, _, [], [], [], !Info).
rename_or_else_inner_vars(NonLocalsToRecompute, Inner,
    [OrElseGoal0 | OrElseGoals0], OrElseGoals, OrElseInners, !Info) :-
    Inner = atomic_interface_vars(InnerDI, InnerUO),
    RenameVars = list_to_set([InnerDI, InnerUO]),
    rename_apart(RenameVars, RenameMap, NonLocalsToRecompute, OrElseGoal0,
                 OrElseGoal, !Info),
    OrElseInnerDI = map.lookup(RenameMap, InnerDI),
    OrElseInnerUO = map.lookup(RenameMap, InnerUO),
    OrElseInner = atomic_interface_vars(OrElseInnerDI, OrElseInnerUO),
    rename_or_else_inner_vars(NonLocalsToRecompute, Inner, OrElseGoals0,
                              OrElseGoalsTail, OrElseInnersTail, !Info),
    OrElseInners = [OrElseInner | OrElseInnersTail],
    OrElseGoals = [OrElseGoal | OrElseGoalsTail].

:- pred implicitly_quantify_goal_quant_info_scope(scope_reason, hlds_goal,
    hlds_goal_expr, hlds_goal_info, nonlocals_to_recompute, set_of_var,
    quant_info, quant_info).
:- mode implicitly_quantify_goal_quant_info_scope(in, in, out, in,
    in(ordinary_nonlocals_maybe_lambda), out, in, out) is det.
:- mode implicitly_quantify_goal_quant_info_scope(in, in, out, in,
    in(ordinary_nonlocals_no_lambda), out, in, out) is det.
:- mode implicitly_quantify_goal_quant_info_scope(in, in, out, in,
    in(code_gen_nonlocals_no_lambda), out, in, out) is det.

implicitly_quantify_goal_quant_info_scope(Reason0, SubGoal0, GoalExpr,
        GoalInfo0, NonLocalsToRecompute, PossiblyNonLocalGoalVars0, !Info) :-
    GoalExpr0 = scope(Reason0, SubGoal0),
    get_quant_vars(!.Info, QuantVars),
    (
        Reason0 = exist_quant(Vars0),
        Reason1 = exist_quant([]),
        implicitly_quantify_goal_quant_info_scope_rename_vars(
            Reason1, Reason, SubGoal0, SubGoal1, Vars0, Vars, GoalInfo0,
            NonLocalsToRecompute, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0),
        implicitly_quantify_goal_quant_info(SubGoal1, SubGoal,
            NonLocalsToRecompute, !Info),
        get_nonlocals(!.Info, NonLocals0),
        delete_list(NonLocals0, Vars, NonLocals),
        set_nonlocals(NonLocals, !Info)
    ;
        Reason0 = from_ground_term(TermVar, from_ground_term_construct),
        Reason = Reason0,
        % Not quantifying the subgoal is a substantial speedup. It is ok
        % because superhomogeneous.m sets up the nonlocal sets of the
        % unifications, their conjunction, and the scope goal itself,
        % and every later compiler pass than can invalidate those nonlocal sets
        % will either set the kind to from_ground_term_other (or to
        % from_ground_term_deconstruct) or remove the scope altogether.
        SubGoal = SubGoal0,
        NonLocals = make_singleton_set(TermVar),
        set_nonlocals(NonLocals, !Info),
        PossiblyNonLocalGoalVars0 = NonLocals
    ;
        ( Reason0 = promise_purity(_)
        ; Reason0 = promise_solutions(_, _)
        ; Reason0 = commit(_)
        ; Reason0 = barrier(_)
        ; Reason0 = from_ground_term(_, from_ground_term_deconstruct)
        ; Reason0 = from_ground_term(_, from_ground_term_other)
        ),
        Reason = Reason0,
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0),
        implicitly_quantify_goal_quant_info(SubGoal0, SubGoal,
            NonLocalsToRecompute, !Info)
    ;
        Reason0 = trace_goal(_, _, _, _, Vars0),
        implicitly_quantify_goal_quant_info_scope_rename_vars(
            Reason0, Reason, SubGoal0, SubGoal1, Vars0, Vars, GoalInfo0,
            NonLocalsToRecompute, !Info),
        goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr0,
            PossiblyNonLocalGoalVars0),
        implicitly_quantify_goal_quant_info(SubGoal1, SubGoal,
            NonLocalsToRecompute, !Info),
        get_nonlocals(!.Info, NonLocals0),
        delete_list(NonLocals0, Vars, NonLocals),
        set_nonlocals(NonLocals, !Info)
    ),
    set_quant_vars(QuantVars, !Info),
    GoalExpr = scope(Reason, SubGoal).

:- pred implicitly_quantify_goal_quant_info_scope_rename_vars(
    scope_reason, scope_reason, hlds_goal, hlds_goal,
    list(prog_var), list(prog_var), hlds_goal_info, nonlocals_to_recompute,
    quant_info, quant_info).
:- mode implicitly_quantify_goal_quant_info_scope_rename_vars(in, out,
    in, out, in, out, in, in(ordinary_nonlocals_maybe_lambda), in, out) is det.
:- mode implicitly_quantify_goal_quant_info_scope_rename_vars(in, out,
    in, out, in, out, in, in(ordinary_nonlocals_no_lambda), in, out) is det.
:- mode implicitly_quantify_goal_quant_info_scope_rename_vars(in, out,
    in, out, in, out, in, in(code_gen_nonlocals_no_lambda), in, out) is det.

implicitly_quantify_goal_quant_info_scope_rename_vars(Reason0, Reason,
        SubGoal0, SubGoal, Vars0, Vars, GoalInfo0, NonLocalsToRecompute,
        !Info) :-
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    get_quant_vars(!.Info, QuantVars),
    % Rename apart all the quantified variables that occur
    % outside this goal.
    QVars = list_to_set(Vars0),
    intersect(OutsideVars, QVars, RenameVars1),
    intersect(LambdaOutsideVars, QVars, RenameVars2),
    union(RenameVars1, RenameVars2, RenameVars),
    ( empty(RenameVars) ->
        SubGoal = SubGoal0,
        Vars = Vars0,
        Reason = Reason0
    ;
        Context = goal_info_get_context(GoalInfo0),
        warn_overlapping_scope(RenameVars, Context, !Info),
        rename_apart(RenameVars, RenameMap, NonLocalsToRecompute,
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
            ( Reason0 = promise_purity(_)
            ; Reason0 = commit(_)
            ; Reason0 = barrier(_)
            ; Reason0 = from_ground_term(_, _)
            ; Reason0 = promise_solutions(_, _)
            ),
            % We shouldn't invoke this predicate for these kinds of scopes.
            unexpected(this_file,
                "implicitly_quantify_goal_quant_info_scope_rename_vars")
        )
    ),
    update_seen_vars(QVars, !Info),
    insert_list(QuantVars, Vars, QuantVars1),
    set_quant_vars(QuantVars1, !Info).

:- pred implicitly_quantify_goal_quant_info_bi_implication(
    hlds_goal, hlds_goal, hlds_goal_expr, hlds_goal_info,
    quant_info, quant_info).
:- mode implicitly_quantify_goal_quant_info_bi_implication(in, in, out, in,
    in, out) is det.

implicitly_quantify_goal_quant_info_bi_implication(LHS0, RHS0, GoalExpr,
        OldGoalInfo, !Info) :-
    % Get the initial values of various settings.
    get_quant_vars(!.Info, QuantVars0),
    get_outside(!.Info, OutsideVars0),
    get_lambda_outside(!.Info, LambdaOutsideVars0),

    % Quantified variables cannot be pushed inside a negation, so we insert
    % the quantified vars into the outside vars set, and initialize the new
    % quantified vars set to be empty (the lambda outside vars remain
    % unchanged).
    union(OutsideVars0, QuantVars0, OutsideVars1),
    QuantVars1 = init,
    LambdaOutsideVars1 = LambdaOutsideVars0,
    set_quant_vars(QuantVars1, !Info),

    % Prepare for quantifying the LHS: add variables from the RHS to the
    % outside vars and the outside lambda vars sets.
    goal_vars_both_maybe_lambda_and_bi_impl(RHS0, RHS_Vars, RHS_LambdaVars),
    union(OutsideVars1, RHS_Vars, LHS_OutsideVars),
    union(LambdaOutsideVars1, RHS_LambdaVars, LHS_LambdaOutsideVars),

    % Quantify the LHS.
    set_outside(LHS_OutsideVars, !Info),
    set_lambda_outside(LHS_LambdaOutsideVars, !Info),
    implicitly_quantify_goal_quant_info(LHS0, LHS,
        ordinary_nonlocals_maybe_lambda, !Info),
    get_nonlocals(!.Info, LHS_NonLocalVars),

    % Prepare for quantifying the RHS: add nonlocals from the LHS to the
    % outside vars. (We use the nonlocals rather than the more symmetric
    % approach of calling goal_vars on the LHS goal because it is more
    % efficient.)
    union(OutsideVars1, LHS_NonLocalVars, RHS_OutsideVars),
    RHS_LambdaOutsideVars = LambdaOutsideVars1,

    % Quantify the RHS.
    set_outside(RHS_OutsideVars, !Info),
    set_lambda_outside(RHS_LambdaOutsideVars, !Info),
    implicitly_quantify_goal_quant_info(RHS0, RHS,
        ordinary_nonlocals_maybe_lambda, !Info),
    get_nonlocals(!.Info, RHS_NonLocalVars),

    % Compute the nonlocals for this goal.
    union(LHS_NonLocalVars, RHS_NonLocalVars, AllNonLocalVars),
    intersect(AllNonLocalVars, OutsideVars0, NonLocalVarsO),
    intersect(AllNonLocalVars, LambdaOutsideVars0, NonLocalVarsL),
    union(NonLocalVarsO, NonLocalVarsL, NonLocalVars),
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
    set_goal_nonlocals(LHS_NonLocalVars, ordinary_nonlocals_maybe_lambda,
        GoalInfo1, LHS_GI, !Info),
    set_goal_nonlocals(RHS_NonLocalVars, ordinary_nonlocals_maybe_lambda,
        GoalInfo1, RHS_GI, !Info),
    set_goal_nonlocals(NonLocalVars, ordinary_nonlocals_maybe_lambda,
        GoalInfo1, GI, !Info),
    NotLHS = hlds_goal(negation(LHS), LHS_GI),
    NotRHS = hlds_goal(negation(RHS), RHS_GI),
    ForwardsImplicationExpr =
        negation(hlds_goal(conj(plain_conj, [LHS, NotRHS]), GI)),
    ForwardsImplication = hlds_goal(ForwardsImplicationExpr, GI),

    % Rename apart the local variables of the goals we've just duplicated.
    ReverseImplicationExpr0 =
        negation(hlds_goal(conj(plain_conj, [RHS, NotLHS]), GI)),
    ReverseImplication0 = hlds_goal(ReverseImplicationExpr0, GI),
    goal_vars_bitset_maybe_lambda_and_bi_impl(ReverseImplication0, GoalVars),
    difference(GoalVars, NonLocalVars, RenameVars),
    rename_apart(RenameVars, _, ordinary_nonlocals_maybe_lambda,
        ReverseImplication0, ReverseImplication, !Info),

    GoalExpr = conj(plain_conj, [ForwardsImplication, ReverseImplication]).

:- pred implicitly_quantify_primitive_goal(list(prog_var)::in,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_primitive_goal(HeadVars, !Info) :-
    GoalVars = list_to_set(HeadVars),
    update_seen_vars(GoalVars, !Info),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    intersect(GoalVars, OutsideVars, NonLocals1),
    intersect(GoalVars, LambdaOutsideVars, NonLocals2),
    union(NonLocals1, NonLocals2, NonLocals),
    set_nonlocals(NonLocals, !Info).

:- pred implicitly_quantify_unify_rhs(maybe(list(needs_update)),
    hlds_goal_info, unify_rhs, unify_rhs,
    unification, unification, nonlocals_to_recompute, quant_info, quant_info).
:- mode implicitly_quantify_unify_rhs(in, in, in, out, in, out,
    in(ordinary_nonlocals_maybe_lambda), in, out) is det.
:- mode implicitly_quantify_unify_rhs(in, in, in, out, in, out,
    in(ordinary_nonlocals_no_lambda), in, out) is det.
:- mode implicitly_quantify_unify_rhs(in, in, in, out, in, out,
    in(code_gen_nonlocals_no_lambda), in, out) is det.

implicitly_quantify_unify_rhs(ReuseArgs, GoalInfo0, !RHS, !Unification,
        NonLocalsToRecompute, !Info) :-
    (
        !.RHS = rhs_var(X),
        Vars = make_singleton_set(X),
        set_nonlocals(Vars, !Info)
    ;
        !.RHS = rhs_functor(_, _, ArgVars),
        (
            NonLocalsToRecompute = code_gen_nonlocals_no_lambda,
            ReuseArgs = yes(SetArgs)
        ->
            % The fields taken from the reused cell aren't counted
            % as code-gen nonlocals.
            get_updated_fields(SetArgs, ArgVars, Vars0),
            Vars = list_to_set(Vars0)
        ;
            Vars = list_to_set(ArgVars)
        ),
        set_nonlocals(Vars, !Info)
    ;
        !.RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals0, LambdaVars0, Modes, Det, Goal0),

        % Note: make_hlds.m has already done most of the hard work for
        % lambda expressions. At this point, LambdaVars0 should in fact be
        % guaranteed to be fresh distinct variables. However, the code below
        % does not assume this.

        get_outside(!.Info, OutsideVars0),
        QVars = list_to_set(LambdaVars0),
        % Figure out which variables have overlapping scopes because they occur
        % outside the goal and are also lambda-quantified vars.
        intersect(OutsideVars0, QVars, RenameVars0),
        ( empty(RenameVars0) ->
            true
        ;
            Context = goal_info_get_context(GoalInfo0),
            warn_overlapping_scope(RenameVars0, Context, !Info)
        ),
        % We need to rename apart any of the lambda vars that we have
        % already seen, since they are new instances.
        get_seen(!.Info, Seen0),
        intersect(Seen0, QVars, RenameVars1),

        union(RenameVars0, RenameVars1, RenameVars),
        rename_apart(RenameVars, RenameMap, NonLocalsToRecompute, Goal0, Goal1,
            !Info),
        rename_var_list(need_not_rename, RenameMap, LambdaVars0, LambdaVars),

        % Quantified variables cannot be pushed inside a lambda goal,
        % so we insert the quantified vars into the outside vars set,
        % and initialize the new quantified vars set to be empty.
        get_quant_vars(!.Info, QuantVars0),
        union(OutsideVars0, QuantVars0, OutsideVars1),
        QuantVars = init,
        set_quant_vars(QuantVars, !Info),
        % Add the lambda vars as outside vars, since they are outside of the
        % lambda goal.
        insert_list(OutsideVars1, LambdaVars, OutsideVars),
        set_outside(OutsideVars, !Info),
        % Set the LambdaOutsideVars set to empty, because variables that occur
        % outside this lambda expression only in other lambda expressions
        % should not be considered nonlocal.
        get_lambda_outside(!.Info, LambdaOutsideVars0),
        LambdaOutsideVars = init,
        set_lambda_outside(LambdaOutsideVars, !Info),
        implicitly_quantify_goal_quant_info(Goal1, Goal, NonLocalsToRecompute,
            !Info),

        !:RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
            LambdaNonLocals, LambdaVars, Modes, Det, Goal),

        get_nonlocals(!.Info, NonLocals0),
        % Lambda-quantified variables are local.
        delete_list(NonLocals0, LambdaVars, NonLocals),
        set_quant_vars(QuantVars0, !Info),
        set_outside(OutsideVars0, !Info),
        set_lambda_outside(LambdaOutsideVars0, !Info),
        set_nonlocals(NonLocals, !Info),

        % Work out the list of nonlocal curried arguments to the lambda
        % expression. This set must only ever decrease, since the first
        % approximation that make_hlds uses includes all variables in the
        % lambda expression except the quantified variables.

        Goal = hlds_goal(_, LambdaGoalInfo),
        LambdaGoalNonLocals = goal_info_get_nonlocals(LambdaGoalInfo),
        list.filter(contains(LambdaGoalNonLocals),
            LambdaNonLocals0, LambdaNonLocals),

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
                expect(unify(MaybeTakeAddr, no), this_file,
                    "lambda term has take addr"),
                expect(unify(MaybeSize, no), this_file,
                    "lambda term has size info")
            ),
            map.from_corresponding_lists(Args0, ArgModes0, ArgModesMap),
            Args = to_sorted_list(NonLocals),
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

:- pred implicitly_quantify_conj_maybe_lambda(list(hlds_goal), list(hlds_goal),
    nonlocals_to_recompute, set_of_var, quant_info, quant_info).
:- mode implicitly_quantify_conj_maybe_lambda(in, out,
    in(ordinary_nonlocals_maybe_lambda), out, in, out) is det.

implicitly_quantify_conj_maybe_lambda(!Goals, NonLocalsToRecompute,
        PossiblyNonLocalGoalVars, !Info) :-
    get_following_vars_maybe_lambda(NonLocalsToRecompute, !.Goals,
        FollowingVarsList, PossiblyNonLocalGoalVars),
    implicitly_quantify_conj_maybe_lambda_2(FollowingVarsList, !Goals,
        NonLocalsToRecompute, !Info).

:- pred implicitly_quantify_conj_no_lambda(list(hlds_goal), list(hlds_goal),
    nonlocals_to_recompute, set_of_var, quant_info, quant_info).
:- mode implicitly_quantify_conj_no_lambda(in, out,
    in(ordinary_nonlocals_no_lambda), out, in, out) is det.
:- mode implicitly_quantify_conj_no_lambda(in, out,
    in(code_gen_nonlocals_no_lambda), out, in, out) is det.

implicitly_quantify_conj_no_lambda(!Goals, NonLocalsToRecompute,
        PossiblyNonLocalGoalVars, !Info) :-
    get_following_vars_no_lambda(NonLocalsToRecompute, !.Goals,
        FollowingVarsList, PossiblyNonLocalGoalVars),
    implicitly_quantify_conj_no_lambda_2(FollowingVarsList, !Goals,
        NonLocalsToRecompute, !Info).

:- pred implicitly_quantify_conj_maybe_lambda_2(list(pair(set_of_var)),
    list(hlds_goal), list(hlds_goal),
    nonlocals_to_recompute, quant_info, quant_info).
:- mode implicitly_quantify_conj_maybe_lambda_2(in, in, out,
    in(ordinary_nonlocals_maybe_lambda), in, out) is det.

implicitly_quantify_conj_maybe_lambda_2(_, [], [], _, !Info) :-
    NonLocalVars = init,
    set_nonlocals(NonLocalVars, !Info).
implicitly_quantify_conj_maybe_lambda_2([], [_ | _], _, _, _, _) :-
    unexpected(this_file,
        "implicitly_quantify_conj_maybe_lambda_2: length mismatch").
implicitly_quantify_conj_maybe_lambda_2([FollowingVarPair | FollowingVarPairs],
        [Goal0 | Goals0], [Goal | Goals], NonLocalsToRecompute, !Info) :-
    FollowingVarPair = FollowingVars - LambdaFollowingVars,
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    union(OutsideVars, FollowingVars, OutsideVars1),
    union(LambdaOutsideVars, LambdaFollowingVars, LambdaOutsideVars1),
    set_outside(OutsideVars1, !Info),
    set_lambda_outside(LambdaOutsideVars1, !Info),
    implicitly_quantify_goal_quant_info(Goal0, Goal, NonLocalsToRecompute,
        !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    union(OutsideVars, NonLocalVars1, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    set_lambda_outside(LambdaOutsideVars, !Info),
    implicitly_quantify_conj_maybe_lambda_2(FollowingVarPairs, Goals0, Goals,
        NonLocalsToRecompute, !Info),
    get_nonlocals(!.Info, NonLocalVars2),
    union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
    intersect(NonLocalVarsConj, OutsideVars, NonLocalVarsO),
    intersect(NonLocalVarsConj, LambdaOutsideVars, NonLocalVarsL),
    union(NonLocalVarsO, NonLocalVarsL, NonLocalVars),
    set_outside(OutsideVars, !Info),
    set_nonlocals(NonLocalVars, !Info).

:- pred implicitly_quantify_conj_no_lambda_2(list(set_of_var),
    list(hlds_goal), list(hlds_goal),
    nonlocals_to_recompute, quant_info, quant_info).
:- mode implicitly_quantify_conj_no_lambda_2(in, in, out,
    in(ordinary_nonlocals_no_lambda), in, out) is det.
:- mode implicitly_quantify_conj_no_lambda_2(in, in, out,
    in(code_gen_nonlocals_no_lambda), in, out) is det.

implicitly_quantify_conj_no_lambda_2(_, [], [], _, !Info) :-
    NonLocalVars = init,
    set_nonlocals(NonLocalVars, !Info).
implicitly_quantify_conj_no_lambda_2([], [_ | _], _, _, _, _) :-
    unexpected(this_file,
        "implicitly_quantify_conj_no_lambda_2: length mismatch").
implicitly_quantify_conj_no_lambda_2([FollowingVars | FollowingVarsList],
        [Goal0 | Goals0], [Goal | Goals], NonLocalsToRecompute, !Info) :-
    get_outside(!.Info, OutsideVars),
    union(OutsideVars, FollowingVars, OutsideVars1),
    set_outside(OutsideVars1, !Info),
    implicitly_quantify_goal_quant_info(Goal0, Goal, NonLocalsToRecompute,
        !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    union(OutsideVars, NonLocalVars1, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    implicitly_quantify_conj_no_lambda_2(FollowingVarsList, Goals0, Goals,
        NonLocalsToRecompute, !Info),
    get_nonlocals(!.Info, NonLocalVars2),
    union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
    intersect(NonLocalVarsConj, OutsideVars, NonLocalVars),
    set_outside(OutsideVars, !Info),
    set_nonlocals(NonLocalVars, !Info).

:- pred implicitly_quantify_disj(list(hlds_goal), list(hlds_goal),
    nonlocals_to_recompute, quant_info, quant_info,
    list(set_of_var), list(set_of_var)).
:- mode implicitly_quantify_disj(in, out, in(ordinary_nonlocals_maybe_lambda),
    in, out, in, out) is det.
:- mode implicitly_quantify_disj(in, out, in(ordinary_nonlocals_no_lambda),
    in, out, in, out) is det.
:- mode implicitly_quantify_disj(in, out, in(code_gen_nonlocals_no_lambda),
    in, out, in, out) is det.

implicitly_quantify_disj([], [], _, !Info, !NonLocalVarSets).
implicitly_quantify_disj([Goal0 | Goals0], [Goal | Goals],
        NonLocalsToRecompute, !Info, !NonLocalVarSets) :-
    implicitly_quantify_goal_quant_info(Goal0, Goal, NonLocalsToRecompute,
        !Info),
    get_nonlocals(!.Info, GoalNonLocalVars),
    !:NonLocalVarSets = [GoalNonLocalVars | !.NonLocalVarSets],
    implicitly_quantify_disj(Goals0, Goals, NonLocalsToRecompute,
        !Info, !NonLocalVarSets).

:- pred implicitly_quantify_cases(list(case), list(case),
    nonlocals_to_recompute, quant_info, quant_info,
    list(set_of_var), list(set_of_var)).
:- mode implicitly_quantify_cases(in, out, in(ordinary_nonlocals_maybe_lambda),
    in, out, in, out) is det.
:- mode implicitly_quantify_cases(in, out, in(ordinary_nonlocals_no_lambda),
    in, out, in, out) is det.
:- mode implicitly_quantify_cases(in, out, in(code_gen_nonlocals_no_lambda),
    in, out, in, out) is det.

implicitly_quantify_cases([], [], _, !Info, !NonLocalVarSets).
implicitly_quantify_cases([Case0 | Cases0], [Case | Cases],
        NonLocalsToRecompute, !Info, !NonLocalVarSets) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    implicitly_quantify_goal_quant_info(Goal0, Goal, NonLocalsToRecompute,
        !Info),
    Case = case(MainConsId, OtherConsIds, Goal),
    get_nonlocals(!.Info, GoalNonLocalVars),
    !:NonLocalVarSets = [GoalNonLocalVars | !.NonLocalVarSets],
    implicitly_quantify_cases(Cases0, Cases, NonLocalsToRecompute,
        !Info, !NonLocalVarSets).

%-----------------------------------------------------------------------------%

    % Insert the given set of variables into the set of `seen' variables.
    %
:- pred update_seen_vars(set_of_var::in, quant_info::in, quant_info::out)
    is det.

update_seen_vars(NewVars, !Info) :-
    get_seen(!.Info, SeenVars0),
    union(SeenVars0, NewVars, SeenVars),
    set_seen(SeenVars, !Info).

%-----------------------------------------------------------------------------%

    % Given a list of goals, produce a corresponding list of following
    % variables, where the following variables for each goal are those
    % variables which occur free in any of the following goals in the list.
    % The following variables are divided into a pair of sets: the first set
    % contains following variables that occur not in lambda goals, and the
    % second contains following variables that occur in lambda goals.
    %
:- pred get_following_vars_maybe_lambda(nonlocals_to_recompute,
    list(hlds_goal), list(pair(set_of_var)), set_of_var).
:- mode get_following_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, out, out) is det.

get_following_vars_maybe_lambda(_, [], [], init).
get_following_vars_maybe_lambda(NonLocalsToRecompute, [Goal | Goals],
        [Set - LambdaSet | SetPairs], PossiblyNonLocalGoalVars) :-
    get_following_vars_maybe_lambda_2(NonLocalsToRecompute, Goals,
        Set, LambdaSet, SetPairs),
    union(Set, LambdaSet, GoalsBothSet),
    goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal,
        GoalSet, GoalLambdaSet),
    union(GoalSet, GoalLambdaSet, GoalBothSet),
    union(GoalBothSet, GoalsBothSet, PossiblyNonLocalGoalVars).

:- pred get_following_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    list(set_of_var), set_of_var).
:- mode get_following_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, out, out) is det.
:- mode get_following_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, out, out) is det.

get_following_vars_no_lambda(_, [], [], init).
get_following_vars_no_lambda(NonLocalsToRecompute, [Goal | Goals],
        [Set | Sets], PossiblyNonLocalGoalVars) :-
    get_following_vars_no_lambda_2(NonLocalsToRecompute, Goals, Set, Sets),
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, GoalSet),
    union(GoalSet, Set, PossiblyNonLocalGoalVars).

:- pred get_following_vars_maybe_lambda_2(nonlocals_to_recompute,
    list(hlds_goal), set_of_var, set_of_var, list(pair(set_of_var))).
:- mode get_following_vars_maybe_lambda_2(in(ordinary_nonlocals_maybe_lambda),
    in, out, out, out) is det.

get_following_vars_maybe_lambda_2(_, [], Set, LambdaSet, []) :-
    Set = init,
    LambdaSet = init.
get_following_vars_maybe_lambda_2(NonLocalsToRecompute, [Goal | Goals],
        Set, LambdaSet, SetPairList) :-
    get_following_vars_maybe_lambda_2(NonLocalsToRecompute, Goals,
        Set0, LambdaSet0, SetPairList0),
    goal_vars_both_maybe_lambda(NonLocalsToRecompute, Goal, Set1, LambdaSet1),
    union(Set0, Set1, Set),
    union(LambdaSet0, LambdaSet1, LambdaSet),
    SetPairList = [Set0 - LambdaSet0 | SetPairList0].

:- pred get_following_vars_no_lambda_2(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, list(set_of_var)).
:- mode get_following_vars_no_lambda_2(in(ordinary_nonlocals_no_lambda),
    in, out, out) is det.
:- mode get_following_vars_no_lambda_2(in(code_gen_nonlocals_no_lambda),
    in, out, out) is det.

get_following_vars_no_lambda_2(_, [], Set, []) :-
    Set = init.
get_following_vars_no_lambda_2(NonLocalsToRecompute, [Goal | Goals],
        Set, SetList) :-
    get_following_vars_no_lambda_2(NonLocalsToRecompute, Goals, Set0, SetList0),
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, Set1),
    union(Set0, Set1, Set),
    SetList = [Set0 | SetList0].

:- pred conj_vars_maybe_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode conj_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, in, out, in, out) is det.

conj_vars_maybe_lambda(_, [], !Set, !LambdaSet).
conj_vars_maybe_lambda(NonLocalsToRecompute, [Goal | Goals],
        !Set, !LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, GoalExpr,
        !Set, !LambdaSet),
    conj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet).

:- pred conj_vars_maybe_lambda_and_bi_impl(list(hlds_goal),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode conj_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

conj_vars_maybe_lambda_and_bi_impl([], !Set, !LambdaSet).
conj_vars_maybe_lambda_and_bi_impl([Goal | Goals], !Set, !LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    goal_expr_vars_maybe_lambda_and_bi_impl_2(GoalExpr, !Set, !LambdaSet),
    conj_vars_maybe_lambda_and_bi_impl(Goals, !Set, !LambdaSet).

:- pred conj_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var).
:- mode conj_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, in, out) is det.
:- mode conj_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, in, out) is det.

conj_vars_no_lambda(_, [], !Set).
conj_vars_no_lambda(NonLocalsToRecompute, [Goal | Goals], !Set) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    goal_expr_vars_no_lambda_2(NonLocalsToRecompute, GoalExpr, !Set),
    conj_vars_no_lambda(NonLocalsToRecompute, Goals, !Set).

:- pred disj_vars_maybe_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode disj_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, in, out, in, out) is det.

disj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet) :-
    compute_disj_vars_maybe_lambda(NonLocalsToRecompute, Goals,
        [], GoalSets, [], GoalLambdaSets),
    (
        GoalSets = [],
        GoalsSet = init
    ;
        GoalSets = [_ | _],
        union_list(GoalSets, GoalsSet)
    ),
    (
        GoalLambdaSets = [],
        GoalsLambdaSet = init
    ;
        GoalLambdaSets = [_ | _],
        union_list(GoalLambdaSets, GoalsLambdaSet)
    ),
    union(GoalsSet, !Set),
    union(GoalsLambdaSet, !LambdaSet).

:- pred disj_vars_maybe_lambda_and_bi_impl(list(hlds_goal),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode disj_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

disj_vars_maybe_lambda_and_bi_impl(Goals, !Set, !LambdaSet) :-
    compute_disj_vars_maybe_lambda_and_bi_impl(Goals,
        [], GoalSets, [], GoalLambdaSets),
    (
        GoalSets = [],
        GoalsSet = init
    ;
        GoalSets = [_ | _],
        union_list(GoalSets, GoalsSet)
    ),
    (
        GoalLambdaSets = [],
        GoalsLambdaSet = init
    ;
        GoalLambdaSets = [_ | _],
        union_list(GoalLambdaSets, GoalsLambdaSet)
    ),
    union(GoalsSet, !Set),
    union(GoalsLambdaSet, !LambdaSet).

:- pred disj_vars_no_lambda(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var).
:- mode disj_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, in, out) is det.
:- mode disj_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, in, out) is det.

disj_vars_no_lambda(NonLocalsToRecompute, Goals, !Set) :-
    compute_disj_vars_no_lambda(NonLocalsToRecompute, Goals, [], GoalSets),
    (
        GoalSets = [],
        GoalsSet = init
    ;
        GoalSets = [_ | _],
        union_list(GoalSets, GoalsSet)
    ),
    union(GoalsSet, !Set).

:- pred compute_disj_vars_maybe_lambda(nonlocals_to_recompute, list(hlds_goal),
    list(set_of_var), list(set_of_var), list(set_of_var), list(set_of_var)).
:- mode compute_disj_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
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
    list(set_of_var), list(set_of_var), list(set_of_var), list(set_of_var)).
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
    list(set_of_var), list(set_of_var)).
:- mode compute_disj_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, in, out) is det.
:- mode compute_disj_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, in, out) is det.

compute_disj_vars_no_lambda(_, [], !Sets).
compute_disj_vars_no_lambda(NonLocalsToRecompute, [Goal | Goals], !Sets) :-
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, GoalSet),
    !:Sets = [GoalSet | !.Sets],
    compute_disj_vars_no_lambda(NonLocalsToRecompute, Goals, !Sets).

:- pred case_vars_maybe_lambda(nonlocals_to_recompute, list(case),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode case_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, in, out, in, out) is det.

case_vars_maybe_lambda(NonLocalsToRecompute, Cases, !Set, !LambdaSet) :-
    compute_case_vars_maybe_lambda(NonLocalsToRecompute, Cases,
        [], CaseSets, [], CaseLambdaSets),
    (
        CaseSets = [],
        unexpected(this_file, "case_vars: no cases (1)")
    ;
        CaseSets = [_ | _],
        union_list(CaseSets, CasesSet)
    ),
    (
        CaseLambdaSets = [],
        unexpected(this_file, "case_vars: no cases (2)")
    ;
        CaseLambdaSets = [_ | _],
        union_list(CaseLambdaSets, CasesLambdaSet)
    ),
    union(CasesSet, !Set),
    union(CasesLambdaSet, !LambdaSet).

:- pred case_vars_maybe_lambda_and_bi_impl(list(case),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode case_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

case_vars_maybe_lambda_and_bi_impl(Cases, !Set, !LambdaSet) :-
    compute_case_vars_maybe_lambda_and_bi_impl(Cases,
        [], CaseSets, [], CaseLambdaSets),
    (
        CaseSets = [],
        unexpected(this_file, "case_vars: no cases (1)")
    ;
        CaseSets = [_ | _],
        union_list(CaseSets, CasesSet)
    ),
    (
        CaseLambdaSets = [],
        unexpected(this_file, "case_vars: no cases (2)")
    ;
        CaseLambdaSets = [_ | _],
        union_list(CaseLambdaSets, CasesLambdaSet)
    ),
    union(CasesSet, !Set),
    union(CasesLambdaSet, !LambdaSet).

:- pred case_vars_no_lambda(nonlocals_to_recompute, list(case),
    set_of_var, set_of_var).
:- mode case_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, in, out) is det.
:- mode case_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, in, out) is det.

case_vars_no_lambda(NonLocalsToRecompute, Cases, !Set) :-
    compute_case_vars_no_lambda(NonLocalsToRecompute, Cases, [], CaseSets),
    (
        CaseSets = [],
        unexpected(this_file, "case_vars: no cases (1)")
    ;
        CaseSets = [_ | _],
        union_list(CaseSets, CasesSet)
    ),
    union(CasesSet, !Set).

:- pred compute_case_vars_maybe_lambda(nonlocals_to_recompute, list(case),
    list(set_of_var), list(set_of_var), list(set_of_var), list(set_of_var)).
:- mode compute_case_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
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
    list(set_of_var), list(set_of_var), list(set_of_var), list(set_of_var)).
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
    list(set_of_var), list(set_of_var)).
:- mode compute_case_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, in, out) is det.
:- mode compute_case_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, in, out) is det.

compute_case_vars_no_lambda(_, [], !Sets).
compute_case_vars_no_lambda(NonLocalsToRecompute, [Case | Cases], !Sets) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, GoalSet),
    !:Sets = [GoalSet | !.Sets],
    compute_case_vars_no_lambda(NonLocalsToRecompute, Cases, !Sets).

:- pred union_list(list(set_of_var)::in, set_of_var::out) is det.

union_list(Sets, Union) :-
    (
        Sets = [],
        Union = init
    ;
        Sets = [_ | _],
        union_list_pass(Sets, [], MergedSets),
        ( MergedSets = [Set] ->
            Union = Set
        ;
            union_list(MergedSets, Union)
        )
    ).

:- pred union_list_pass(list(set_of_var)::in,
    list(set_of_var)::in, list(set_of_var)::out) is det.

union_list_pass([], !MergedSets).
union_list_pass([Set], !MergedSets) :-
    !:MergedSets = [Set | !.MergedSets].
union_list_pass([Set1, Set2 | Sets], !MergedSets) :-
    union(Set1, Set2, Set12),
    !:MergedSets = [Set12 | !.MergedSets],
    union_list_pass(Sets, !MergedSets).

free_goal_vars(Goal) =
    free_goal_vars_nl_maybe_lambda(ordinary_nonlocals_maybe_lambda, Goal).

    % free_goal_vars_nl(NonLocalsToRecompute, Goal) = Vars:
    %
    % Vars is the set of variables that occur free (unquantified) in Goal,
    % excluding unset fields of reconstructions if NonLocalsToRecompute
    % is `code_gen_nonlocals_no_lambda'.
    %
:- func free_goal_vars_nl_maybe_lambda(nonlocals_to_recompute, hlds_goal)
    = set(prog_var).
:- mode free_goal_vars_nl_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in) = out is det.

free_goal_vars_nl_maybe_lambda(NonLocalsToRecompute, Goal) =
        bitset_to_set(BothSet) :-
    goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, BothSet).

:- func free_goal_vars_nl_no_lambda(nonlocals_to_recompute, hlds_goal)
    = set(prog_var).
:- mode free_goal_vars_nl_no_lambda(in(ordinary_nonlocals_no_lambda),
    in) = out is det.
:- mode free_goal_vars_nl_no_lambda(in(code_gen_nonlocals_no_lambda),
    in) = out is det.

free_goal_vars_nl_no_lambda(NonLocalsToRecompute, Goal) =
        bitset_to_set(BothSet) :-
    goal_vars_bitset_no_lambda(NonLocalsToRecompute, Goal, BothSet).

:- pred goal_vars_bitset(nonlocals_to_recompute,
    hlds_goal, set_of_var).
:- mode goal_vars_bitset(in(ordinary_nonlocals_maybe_lambda),
    in, out) is det.
:- mode goal_vars_bitset(in(ordinary_nonlocals_no_lambda),
    in, out) is det.
:- mode goal_vars_bitset(in(code_gen_nonlocals_no_lambda),
    in, out) is det.

goal_vars_bitset(NonLocalsToRecompute, Goal, BothSet) :-
    (
        NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda,
        goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, BothSet)
    ;
        ( NonLocalsToRecompute = ordinary_nonlocals_no_lambda
        ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
        ),
        goal_vars_bitset_no_lambda(NonLocalsToRecompute, Goal, BothSet)
    ).

:- pred goal_vars_bitset_maybe_lambda(nonlocals_to_recompute,
    hlds_goal, set_of_var).
:- mode goal_vars_bitset_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, out) is det.

goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, BothSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_vars_bitset_maybe_lambda_and_bi_impl(hlds_goal, set_of_var).
:- mode goal_vars_bitset_maybe_lambda_and_bi_impl(in, out) is det.

goal_vars_bitset_maybe_lambda_and_bi_impl(Goal, BothSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda_and_bi_impl(GoalExpr, Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_vars_bitset_no_lambda(nonlocals_to_recompute,
    hlds_goal, set_of_var).
:- mode goal_vars_bitset_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, out) is det.
:- mode goal_vars_bitset_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, out) is det.

goal_vars_bitset_no_lambda(NonLocalsToRecompute, Goal, BothSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_no_lambda(NonLocalsToRecompute, GoalExpr, Set),
    BothSet = Set.

:- pred goal_expr_vars_bitset(nonlocals_to_recompute,
    hlds_goal_expr, set_of_var).
:- mode goal_expr_vars_bitset(in(ordinary_nonlocals_maybe_lambda),
    in, out) is det.
:- mode goal_expr_vars_bitset(in(ordinary_nonlocals_no_lambda),
    in, out) is det.
:- mode goal_expr_vars_bitset(in(code_gen_nonlocals_no_lambda),
    in, out) is det.

goal_expr_vars_bitset(NonLocalsToRecompute, GoalExpr, BothSet) :-
    (
        NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda,
        goal_expr_vars_bitset_maybe_lambda(NonLocalsToRecompute, GoalExpr,
            BothSet)
    ;
        ( NonLocalsToRecompute = ordinary_nonlocals_no_lambda
        ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
        ),
        goal_expr_vars_bitset_no_lambda(NonLocalsToRecompute, GoalExpr,
            BothSet)
    ).

:- pred goal_expr_vars_bitset_maybe_lambda(nonlocals_to_recompute,
    hlds_goal_expr, set_of_var).
:- mode goal_expr_vars_bitset_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, out) is det.

goal_expr_vars_bitset_maybe_lambda(NonLocalsToRecompute, GoalExpr, BothSet) :-
    goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_expr_vars_bitset_no_lambda(nonlocals_to_recompute,
    hlds_goal_expr, set_of_var).
:- mode goal_expr_vars_bitset_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, out) is det.
:- mode goal_expr_vars_bitset_no_lambda(in(code_gen_nonlocals_no_lambda),
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
    set_of_var, set_of_var).
:- mode goal_vars_both_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
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
    set_of_var, set_of_var).
:- mode goal_vars_both_maybe_lambda_and_bi_impl(in, out, out) is det.

goal_vars_both_maybe_lambda_and_bi_impl(Goal, Set, LambdaSet) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_maybe_lambda_and_bi_impl(GoalExpr, Set, LambdaSet).

:- pred goal_vars_both_no_lambda(nonlocals_to_recompute, hlds_goal,
    set_of_var).
:- mode goal_vars_both_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, out) is det.
:- mode goal_vars_both_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, out) is det.

goal_vars_both_no_lambda(NonLocalsToRecompute, Goal, Set) :-
    Goal = hlds_goal(GoalExpr, _),
    goal_expr_vars_both_no_lambda(NonLocalsToRecompute, GoalExpr, Set).

:- pred goal_expr_vars_both_maybe_lambda(nonlocals_to_recompute, hlds_goal_expr,
    set_of_var, set_of_var).
:- mode goal_expr_vars_both_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, out, out) is det.

goal_expr_vars_both_maybe_lambda(NonLocalsToRecompute, GoalExpr,
        Set, LambdaSet) :-
    Set0 = init,
    LambdaSet0 = init,
    goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, GoalExpr, Set0, Set,
        LambdaSet0, LambdaSet).

:- pred goal_expr_vars_both_maybe_lambda_and_bi_impl(hlds_goal_expr,
    set_of_var, set_of_var).
:- mode goal_expr_vars_both_maybe_lambda_and_bi_impl(in, out, out) is det.

goal_expr_vars_both_maybe_lambda_and_bi_impl(GoalExpr, Set, LambdaSet) :-
    Set0 = init,
    LambdaSet0 = init,
    goal_expr_vars_maybe_lambda_and_bi_impl_2(GoalExpr, Set0, Set,
        LambdaSet0, LambdaSet).

:- pred goal_expr_vars_both_no_lambda(nonlocals_to_recompute, hlds_goal_expr,
    set_of_var).
:- mode goal_expr_vars_both_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, out) is det.
:- mode goal_expr_vars_both_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, out) is det.

goal_expr_vars_both_no_lambda(NonLocalsToRecompute, GoalExpr, Set) :-
    Set0 = init,
    goal_expr_vars_no_lambda_2(NonLocalsToRecompute, GoalExpr, Set0, Set).

:- pred goal_expr_vars_maybe_lambda_2(nonlocals_to_recompute, hlds_goal_expr,
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode goal_expr_vars_maybe_lambda_2(in(ordinary_nonlocals_maybe_lambda),
    in, in, out, in, out) is det.

goal_expr_vars_maybe_lambda_2(NonLocalsToRecompute, GoalExpr,
        !Set, !LambdaSet) :-
    (
        GoalExpr = unify(LHS, RHS, _, Unification, _),
        insert(!.Set, LHS, !:Set),
        (
            Unification = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, _)),
                insert(!.Set, ReuseVar, !:Set)
            ;
                How = construct_in_region(RegionVar),
                insert(!.Set, RegionVar, !:Set)
            ;
                ( How = construct_statically
                ; How = construct_dynamically
                )
            ),
            (
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            ->
                insert(!.Set, SizeVar, !:Set)
            ;
                true
            )
        ;
            Unification = complicated_unify(_, _, TypeInfoVars),
            insert_list(!.Set, TypeInfoVars, !:Set)
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
        insert_list(!.Set, ArgVars, !:Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars1, _, _),
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        insert_list(!.Set, ArgVars0, !:Set),
        insert_list(!.Set, ArgVars1, !:Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        insert_list(!.Set, AllVars, !:Set)
    ;
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj
        ;
            ConjType = parallel_conj
        ),
        conj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet)
    ;
        GoalExpr = disj(Goals),
        disj_vars_maybe_lambda(NonLocalsToRecompute, Goals, !Set, !LambdaSet)
    ;
        GoalExpr = switch(Var, _Det, Cases),
        insert(!.Set, Var, !:Set),
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
        union(CondSet, ThenSet, CondThenSet),
        union(CondLambdaSet, ThenLambdaSet, CondThenLambdaSet),
        delete_list(CondThenSet, Vars, SomeCondThenSet),
        delete_list(CondThenLambdaSet, Vars, SomeCondThenLambdaSet),
        union(!.Set, SomeCondThenSet, !:Set),
        union(!.LambdaSet, SomeCondThenLambdaSet, !:LambdaSet),
        union(!.Set, ElseSet, !:Set),
        union(!.LambdaSet, ElseLambdaSet, !:LambdaSet)
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
            ( Reason = promise_purity(_)
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
            delete_list(!.Set, Vars, !:Set),
            delete_list(!.LambdaSet, Vars, !:LambdaSet)
        ;
            Reason = promise_solutions(Vars, _Kind),
            goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                !:Set, !:LambdaSet),
            insert_list(!.Set, Vars, !:Set)
        ;
            Reason = from_ground_term(TermVar, Kind),
            (
                Kind = from_ground_term_construct,
                !:Set = init,
                insert(!.Set, TermVar, !:Set),
                !:LambdaSet = init
            ;
                ( Kind = from_ground_term_deconstruct
                ; Kind = from_ground_term_other
                ),
                % Unfortunately, while there will never by any lambda goals
                % inside such SubGoal when the scope is built, there may be
                % lambda goals inside SubGoal after typechecking.
                goal_vars_both_maybe_lambda(NonLocalsToRecompute, SubGoal,
                    !:Set, !:LambdaSet)
                % TermVar should have been put into the relevant sets when we
                % processed SubGoal, since it should appear in SubGoal.
            )
        ),
        union(Set0, !Set),
        union(LambdaSet0, !LambdaSet)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            % XXX STM
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            insert_list(!.Set, [OuterDI, OuterUO, InnerDI, InnerUO], !:Set),
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
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode goal_expr_vars_maybe_lambda_and_bi_impl_2(
    in, in, out, in, out) is det.

goal_expr_vars_maybe_lambda_and_bi_impl_2(GoalExpr, !Set, !LambdaSet) :-
    (
        GoalExpr = unify(LHS, RHS, _, Unification, _),
        insert(!.Set, LHS, !:Set),
        (
            Unification = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, _)),
                insert(!.Set, ReuseVar, !:Set)
            ;
                How = construct_in_region(RegionVar),
                insert(!.Set, RegionVar, !:Set)
            ;
                ( How = construct_statically
                ; How = construct_dynamically
                )
            ),
            (
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            ->
                insert(!.Set, SizeVar, !:Set)
            ;
                true
            )
        ;
            Unification = complicated_unify(_, _, TypeInfoVars),
            insert_list(!.Set, TypeInfoVars, !:Set)
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
        ),
        unify_rhs_vars_maybe_lambda_and_bi_impl(RHS, !Set, !LambdaSet)
    ;
        GoalExpr = plain_call(_, _, ArgVars, _, _, _),
        insert_list(!.Set, ArgVars, !:Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars1, _, _),
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        insert_list(!.Set, ArgVars0, !:Set),
        insert_list(!.Set, ArgVars1, !:Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        insert_list(!.Set, AllVars, !:Set)
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
        insert(!.Set, Var, !:Set),
        case_vars_maybe_lambda_and_bi_impl(Cases, !Set, !LambdaSet)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        % This code does the following:
        %     !:Set = !.Set + ( (vars(Cond) + vars(Then)) \ Vars ) + vars(Else)
        % where `+' is set union and `\' is relative complement.
        goal_vars_both_maybe_lambda_and_bi_impl(Cond,
            CondSet, CondLambdaSet),
        goal_vars_both_maybe_lambda_and_bi_impl(Then,
            ThenSet, ThenLambdaSet),
        goal_vars_both_maybe_lambda_and_bi_impl(Else,
            ElseSet, ElseLambdaSet),
        union(CondSet, ThenSet, CondThenSet),
        union(CondLambdaSet, ThenLambdaSet, CondThenLambdaSet),
        delete_list(CondThenSet, Vars, SomeCondThenSet),
        delete_list(CondThenLambdaSet, Vars, SomeCondThenLambdaSet),
        union(!.Set, SomeCondThenSet, !:Set),
        union(!.LambdaSet, SomeCondThenLambdaSet, !:LambdaSet),
        union(!.Set, ElseSet, !:Set),
        union(!.LambdaSet, ElseLambdaSet, !:LambdaSet)
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
            ( Reason = promise_purity(_)
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
            delete_list(!.Set, Vars, !:Set),
            delete_list(!.LambdaSet, Vars, !:LambdaSet)
        ;
            Reason = promise_solutions(Vars, _Kind),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet),
            insert_list(!.Set, Vars, !:Set)
        ;
            Reason = from_ground_term(_TermVar, _Kind),
            goal_vars_both_maybe_lambda_and_bi_impl(SubGoal,
                !:Set, !:LambdaSet)
            % TermVar should have been put into the relevant sets when we
            % processed SubGoal, since it should appear in SubGoal.
        ),
        union(Set0, !Set),
        union(LambdaSet0, !LambdaSet)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            % XXX STM
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            insert_list(!.Set, [OuterDI, OuterUO, InnerDI, InnerUO], !:Set),
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
    set_of_var, set_of_var).
:- mode goal_expr_vars_no_lambda_2(in(ordinary_nonlocals_no_lambda),
    in, in, out) is det.
:- mode goal_expr_vars_no_lambda_2(in(code_gen_nonlocals_no_lambda),
    in, in, out) is det.

goal_expr_vars_no_lambda_2(NonLocalsToRecompute, GoalExpr, !Set) :-
    (
        GoalExpr = unify(LHS, RHS, _, Unification, _),
        insert(!.Set, LHS, !:Set),
        (
            Unification = construct(_, _, _, _, How, _, SubInfo),
            (
                How = reuse_cell(cell_to_reuse(ReuseVar, _, SetArgs)),
                MaybeSetArgs = yes(SetArgs),
                insert(!.Set, ReuseVar, !:Set)
            ;
                How = construct_in_region(RegionVar),
                MaybeSetArgs = no,
                insert(!.Set, RegionVar, !:Set)
            ;
                ( How = construct_statically
                ; How = construct_dynamically
                ),
                MaybeSetArgs = no
            ),
            (
                SubInfo = construct_sub_info(_, MaybeSize),
                MaybeSize = yes(dynamic_size(SizeVar))
            ->
                insert(!.Set, SizeVar, !:Set)
            ;
                true
            )
        ;
            Unification = complicated_unify(_, _, TypeInfoVars),
            MaybeSetArgs = no,
            insert_list(!.Set, TypeInfoVars, !:Set)
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
        insert_list(!.Set, ArgVars, !:Set)
    ;
        GoalExpr = generic_call(GenericCall, ArgVars1, _, _),
        goal_util.generic_call_vars(GenericCall, ArgVars0),
        insert_list(!.Set, ArgVars0, !:Set),
        insert_list(!.Set, ArgVars1, !:Set)
    ;
        GoalExpr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
        Vars = list.map(foreign_arg_var, Args),
        ExtraVars = list.map(foreign_arg_var, ExtraArgs),
        list.append(Vars, ExtraVars, AllVars),
        insert_list(!.Set, AllVars, !:Set)
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
        insert(!.Set, Var, !:Set),
        case_vars_no_lambda(NonLocalsToRecompute, Cases, !Set)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        % This code does the following:
        %     !:Set = !.Set + ( (vars(Cond) + vars(Then)) \ Vars ) + vars(Else)
        % where `+' is set union and `\' is relative complement.
        goal_vars_both_no_lambda(NonLocalsToRecompute, Cond, CondSet),
        goal_vars_both_no_lambda(NonLocalsToRecompute, Then, ThenSet),
        goal_vars_both_no_lambda(NonLocalsToRecompute, Else, ElseSet),
        union(CondSet, ThenSet, CondThenSet),
        delete_list(CondThenSet, Vars, SomeCondThenSet),
        union(!.Set, SomeCondThenSet, !:Set),
        union(!.Set, ElseSet, !:Set)
    ;
        GoalExpr = negation(SubGoal),
        SubGoal = hlds_goal(SubGoalExpr, _SubGoalInfo),
        goal_expr_vars_no_lambda_2(NonLocalsToRecompute, SubGoalExpr, !Set)
    ;
        GoalExpr = scope(Reason, SubGoal),
        Set0 = !.Set,
        (
            ( Reason = promise_purity(_)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set)
        ;
            Reason = exist_quant(Vars),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set),
            delete_list(!.Set, Vars, !:Set)
        ;
            Reason = promise_solutions(Vars, _Kind),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set),
            insert_list(!.Set, Vars, !:Set)
        ;
            Reason = from_ground_term(_TermVar, _),
            goal_vars_both_no_lambda(NonLocalsToRecompute, SubGoal, !:Set)
            % _TermVar should have been put into the relevant sets when we
            % processed SubGoal, since it should appear in SubGoal.
        ),
        union(Set0, !Set)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, Outer, Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            % XXX STM
            Outer = atomic_interface_vars(OuterDI, OuterUO),
            Inner = atomic_interface_vars(InnerDI, InnerUO),
            insert_list(!.Set, [OuterDI, OuterUO, InnerDI, InnerUO], !:Set),
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
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode unify_rhs_vars_maybe_lambda(in(ordinary_nonlocals_maybe_lambda),
    in, in, out, in, out) is det.

unify_rhs_vars_maybe_lambda(NonLocalsToRecompute, RHS, !Set, !LambdaSet) :-
    (
        RHS = rhs_var(Y), 
        insert(!.Set, Y, !:Set)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        insert_list(!.Set, ArgVars, !:Set)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, LambdaVars, _, _, Goal),
        % Note that the NonLocals list is not counted, since all the
        % variables in that list must occur in the goal.
        goal_vars_bitset_maybe_lambda(NonLocalsToRecompute, Goal, GoalVars),
        delete_list(GoalVars, LambdaVars, GoalVars1),
        union(!.LambdaSet, GoalVars1, !:LambdaSet)
    ).

:- pred unify_rhs_vars_maybe_lambda_and_bi_impl(unify_rhs,
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode unify_rhs_vars_maybe_lambda_and_bi_impl(
    in, in, out, in, out) is det.

unify_rhs_vars_maybe_lambda_and_bi_impl(RHS, !Set, !LambdaSet) :-
    (
        RHS = rhs_var(Y), 
        insert(!.Set, Y, !:Set)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        insert_list(!.Set, ArgVars, !:Set)
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, LambdaVars, _, _, Goal),
        % Note that the NonLocals list is not counted, since all the
        % variables in that list must occur in the goal.
        goal_vars_bitset_maybe_lambda_and_bi_impl(Goal, GoalVars),
        delete_list(GoalVars, LambdaVars, GoalVars1),
        union(!.LambdaSet, GoalVars1, !:LambdaSet)
    ).

:- pred unify_rhs_vars_no_lambda(nonlocals_to_recompute, unify_rhs,
    maybe(list(needs_update)), set_of_var, set_of_var).
:- mode unify_rhs_vars_no_lambda(in(ordinary_nonlocals_no_lambda),
    in, in, in, out) is det.
:- mode unify_rhs_vars_no_lambda(in(code_gen_nonlocals_no_lambda),
    in, in, in, out) is det.

unify_rhs_vars_no_lambda(NonLocalsToRecompute, RHS, MaybeSetArgs, !Set) :-
    (
        RHS = rhs_var(Y), 
        insert(!.Set, Y, !:Set)
    ;
        RHS = rhs_functor(_, _, ArgVars),
        (
            NonLocalsToRecompute = code_gen_nonlocals_no_lambda,
            MaybeSetArgs = yes(SetArgs)
        ->
            % Ignore the fields taken from the reused cell.
            get_updated_fields(SetArgs, ArgVars, ArgsToSet),
            insert_list(!.Set, ArgsToSet, !:Set)
        ;
            insert_list(!.Set, ArgVars, !:Set)
        )
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, _),
        unexpected(this_file, "unify_rhs_vars_no_lambda: found lambda")
    ).

:- pred insert_set_fields(list(needs_update)::in, list(prog_var)::in,
    set_of_var::in, set_of_var::out) is det.

insert_set_fields(SetArgs, Args, !Set) :-
    get_updated_fields(SetArgs, Args,  ArgsToSet),
    insert_list(!.Set, ArgsToSet, !:Set).

:- pred get_updated_fields(list(needs_update)::in,
    list(prog_var)::in, list(prog_var)::out) is det.

get_updated_fields(SetArgs, Args, ArgsToSet) :-
    get_updated_fields(SetArgs, Args, [], ArgsToSet).

:- pred get_updated_fields(list(needs_update)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::out) is det.

get_updated_fields([], [], !ArgsToSet).
get_updated_fields([], [_|_], _, _) :-
    unexpected(this_file, "get_updated_fields").
get_updated_fields([_|_], [], _, _) :-
    unexpected(this_file, "get_updated_fields").
get_updated_fields([SetArg | SetArgs], [Arg | Args], !ArgsToSet) :-
    (
        SetArg = needs_update,
        !:ArgsToSet = [Arg | !.ArgsToSet]
    ;
        SetArg = does_not_need_update,
        !:ArgsToSet = !.ArgsToSet
    ),
    get_updated_fields(SetArgs, Args, !ArgsToSet).

:- func get_unify_typeinfos(unification) = list(prog_var).

get_unify_typeinfos(Unification) = TypeInfoVars :-
    (
        Unification = complicated_unify(_, _, TypeInfoVars)
    ;
        ( Unification = construct(_, _, _, _, _, _, _)
        ; Unification = deconstruct(_, _, _, _, _, _)
        ; Unification = assign(_, _)
        ; Unification = simple_test(_, _)
        ),
        TypeInfoVars = []
    ).

%-----------------------------------------------------------------------------%

:- pred warn_overlapping_scope(set_of_var::in, prog_context::in,
    quant_info::in, quant_info::out) is det.

warn_overlapping_scope(OverlapVars, Context, !Info) :-
    Vars = to_sorted_list(OverlapVars),
    get_warnings(!.Info, Warnings0),
    Warnings = [warn_overlap(Vars, Context) | Warnings0],
    set_warnings(Warnings, !Info).

%-----------------------------------------------------------------------------%

    % rename_apart(RenameSet, RenameMap, Goal0, Goal):
    %
    % For each variable V in RenameSet, create a fresh variable V',
    % and insert the mapping V->V' into RenameMap. Apply RenameMap
    % to Goal0 giving Goal.
    %
:- pred rename_apart(set_of_var, map(prog_var, prog_var),
    nonlocals_to_recompute, hlds_goal, hlds_goal, quant_info, quant_info).
:- mode rename_apart(in, out, in(ordinary_nonlocals_maybe_lambda),
    in, out, in, out) is det.
:- mode rename_apart(in, out, in(ordinary_nonlocals_no_lambda),
    in, out, in, out) is det.
:- mode rename_apart(in, out, in(code_gen_nonlocals_no_lambda),
    in, out, in, out) is det.

rename_apart(RenameSet, RenameMap, NonLocalsToRecompute, !Goal, !Info) :-
    (
        % Don't rename apart variables when recomputing the code-gen nonlocals
        % -- that would stuff up the ordinary nonlocals and the mode
        % information. The ordinary nonlocals are always recomputed
        % before the code-gen nonlocals -- any necessary renaming will have
        % been done while recomputing the ordinary nonlocals.

        ( empty(RenameSet)
        ; NonLocalsToRecompute = code_gen_nonlocals_no_lambda
        )
    ->
        map.init(RenameMap)
    ;
        RenameList = to_sorted_list(RenameSet),
        get_varset(!.Info, Varset0),
        get_vartypes(!.Info, VarTypes0),
        map.init(RenameMap0),
        clone_variables(RenameList, Varset0, VarTypes0,
            Varset0, Varset, VarTypes0, VarTypes, RenameMap0, RenameMap),
        rename_some_vars_in_goal(RenameMap, !Goal),
        set_varset(Varset, !Info),
        set_vartypes(VarTypes, !Info)

        % We don't need to add the newly created vars to the seen vars
        % because we won't find them anywhere else in the enclosing goal.
        % This is a performance improvement because it keeps the size
        % of the seen var set down.
        % get_seen(!.Info, SeenVars0),
        % map.values(RenameMap, NewVarsList),
        % insert_list(SeenVars0, NewVarsList, SeenVars),
        % set_seen(SeenVars, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred set_goal_nonlocals(set_of_var, nonlocals_to_recompute,
    hlds_goal_info, hlds_goal_info, quant_info, quant_info).
:- mode set_goal_nonlocals(in, in(ordinary_nonlocals_maybe_lambda),
    in, out, in, out) is det.
:- mode set_goal_nonlocals(in, in(ordinary_nonlocals_no_lambda),
    in, out, in, out) is det.
:- mode set_goal_nonlocals(in, in(code_gen_nonlocals_no_lambda),
    in, out, in, out) is det.

set_goal_nonlocals(NonLocals, NonLocalsToRecompute, !GoalInfo, !Info) :-
    set_goal_nonlocals_translate(NonLocals, _, NonLocalsToRecompute,
        !GoalInfo, !Info).

:- pred set_goal_nonlocals_translate(set_of_var, set(prog_var),
    nonlocals_to_recompute, hlds_goal_info, hlds_goal_info,
    quant_info, quant_info).
:- mode set_goal_nonlocals_translate(in, out,
    in(ordinary_nonlocals_maybe_lambda), in, out, in, out) is det.
:- mode set_goal_nonlocals_translate(in, out,
    in(ordinary_nonlocals_no_lambda), in, out, in, out) is det.
:- mode set_goal_nonlocals_translate(in, out,
    in(code_gen_nonlocals_no_lambda), in, out, in, out) is det.

set_goal_nonlocals_translate(NonLocalsBitSet, NonLocals, NonLocalsToRecompute,
        !GoalInfo, !Info) :-
    NonLocals = bitset_to_set(NonLocalsBitSet),
    (
        ( NonLocalsToRecompute = ordinary_nonlocals_maybe_lambda
        ; NonLocalsToRecompute = ordinary_nonlocals_no_lambda
        ),
        goal_info_set_nonlocals(NonLocals, !GoalInfo)
    ;
        NonLocalsToRecompute = code_gen_nonlocals_no_lambda,
        goal_info_set_code_gen_nonlocals(NonLocals, !GoalInfo)
    ).

%-----------------------------------------------------------------------------%

:- func bitset_to_set(set_of_var) = set(prog_var).

bitset_to_set(Bitset) = set.sorted_list_to_set(to_sorted_list(Bitset)).

:- func set_to_bitset(set(prog_var)) = set_of_var.

set_to_bitset(Bitset) = sorted_list_to_set(set.to_sorted_list(Bitset)).

%-----------------------------------------------------------------------------%

:- pred init_quant_info(set_of_var::in,
    prog_varset::in, vartypes::in, rtti_varmaps::in, quant_info::out) is det.

init_quant_info(OutsideVars, Varset, VarTypes, RttiVarMaps, QuantInfo) :-
    OverlapWarnings = [],
    QuantInfo = quant_info(OutsideVars, QuantVars, LambdaOutsideVars,
        NonLocals, Seen, Varset, VarTypes, OverlapWarnings, RttiVarMaps),
    QuantVars = init,
    NonLocals = init,
    LambdaOutsideVars = init,
    Seen = OutsideVars.

:- pred get_outside(quant_info::in, set_of_var::out) is det.
:- pred get_quant_vars(quant_info::in, set_of_var::out) is det.
:- pred get_lambda_outside(quant_info::in, set_of_var::out) is det.
:- pred get_nonlocals(quant_info::in, set_of_var::out) is det.
:- pred get_seen(quant_info::in, set_of_var::out) is det.
:- pred get_varset(quant_info::in, prog_varset::out) is det.
:- pred get_vartypes(quant_info::in, vartypes::out) is det.
:- pred get_warnings(quant_info::in, list(quant_warning)::out) is det.
:- pred get_rtti_varmaps(quant_info::in, rtti_varmaps::out) is det.

:- pred set_outside(set_of_var::in,
    quant_info::in, quant_info::out) is det.
:- pred set_quant_vars(set_of_var::in,
    quant_info::in, quant_info::out) is det.
:- pred set_lambda_outside(set_of_var::in,
    quant_info::in, quant_info::out) is det.
:- pred set_nonlocals(set_of_var::in,
    quant_info::in, quant_info::out) is det.
:- pred set_seen(set_of_var::in,
    quant_info::in, quant_info::out) is det.
:- pred set_varset(prog_varset::in,
    quant_info::in, quant_info::out) is det.
:- pred set_vartypes(vartypes::in,
    quant_info::in, quant_info::out) is det.
:- pred set_warnings(list(quant_warning)::in,
    quant_info::in, quant_info::out) is det.
:- pred set_rtti_varmaps(rtti_varmaps::in,
    quant_info::in, quant_info::out) is det.

get_outside(Q, Q ^ qi_outside).
get_quant_vars(Q, Q ^ qi_quant_vars).
get_lambda_outside(Q, Q ^ qi_lambda_outside).
get_nonlocals(Q, Q ^ qi_nonlocals).
get_seen(Q, Q ^ qi_seen).
get_varset(Q, Q ^ qi_varset).
get_vartypes(Q, Q ^ qi_vartypes).
get_warnings(Q, Q ^ qi_warnings).
get_rtti_varmaps(Q, Q ^ qi_rtti_varmaps).

set_outside(Outside, Q, Q ^ qi_outside := Outside).
set_quant_vars(QuantVars, Q, Q ^ qi_quant_vars := QuantVars).
set_lambda_outside(LambdaOutside, Q, Q ^ qi_lambda_outside := LambdaOutside).
set_nonlocals(NonLocals, Q, Q ^ qi_nonlocals := NonLocals).
set_seen(Seen, Q, Q ^ qi_seen := Seen).
set_varset(Varset, Q, Q ^ qi_varset := Varset).
set_vartypes(VarTypes, Q, Q ^ qi_vartypes := VarTypes).
set_warnings(Warnings, Q, Q ^ qi_warnings := Warnings).
set_rtti_varmaps(RttiVarMaps, Q, Q ^ qi_rtti_varmaps := RttiVarMaps).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "quantification.m".

%-----------------------------------------------------------------------------%
:- end_module quantification.
%-----------------------------------------------------------------------------%
