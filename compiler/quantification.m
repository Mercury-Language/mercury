%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
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
    --->    ordinary_nonlocals
    ;       code_gen_nonlocals.

:- pred implicitly_quantify_clause_body_general(nonlocals_to_recompute::in,
    list(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

    % As above, with `ordinary_nonlocals' passed as the first argument.
    %
:- pred implicitly_quantify_clause_body(list(prog_var)::in,
    list(quant_warning)::out, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    rtti_varmaps::in, rtti_varmaps::out) is det.

:- pred implicitly_quantify_goal_general(nonlocals_to_recompute::in,
    set(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

    % As above, with `ordinary_nonlocals' passed as the first argument.
    %
:- pred implicitly_quantify_goal(set(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

:- pred requantify_proc_general(nonlocals_to_recompute::in,
    proc_info::in, proc_info::out) is det.

    % As above, with `ordinary_nonlocals' passed as the first argument.
    %
:- pred requantify_proc(proc_info::in, proc_info::out) is det.

    % We return a list of warnings back to make_hlds.m.
    % Currently the only thing we warn about is variables with
    % overlapping scopes.

:- type quant_warning
    --->    warn_overlap(list(prog_var), prog_context).

    % free_goal_vars(Goal) = Vars:
    %
    % Vars is the set of variables that occur free (unquantified) in Goal
    % excluding unset fields of reconstructions if
    % NonLocalsToRecompute is `code_gen_nonlocals'.
    %
:- func free_goal_vars(hlds_goal) = set(prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.

:- import_module bool.
:- import_module enum.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module sparse_bitset.
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
                nonlocals_to_recompute  :: nonlocals_to_recompute,
                outside                 :: set_of_var,
                quant_vars              :: set_of_var,
                lambda_outside          :: set_of_var,
                nonlocals               :: set_of_var,
                seen                    :: set_of_var,
                varset                  :: prog_varset,
                vartypes                :: vartypes,
                warnings                :: list(quant_warning),
                rtti_varmaps            :: rtti_varmaps
            ).

    % Until we have user-specified pretty printing in the debugger,
    % debugging will be much easier if set_of_var is just `set(prog_var)'.
    % None of the calls to the predicates and functions operating on sets
    % in this module are module qualified so we can switch representation
    % just by changing this line.
% :- type set_of_var == set(prog_var).
:- type set_of_var == sparse_bitset(prog_var).

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

:- inst ordinary_nonlocals ---> ordinary_nonlocals.
:- inst code_gen_nonlocals ---> code_gen_nonlocals.

%-----------------------------------------------------------------------------%

implicitly_quantify_clause_body(HeadVars,
        Warnings, !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    implicitly_quantify_clause_body_general(ordinary_nonlocals, HeadVars,
        Warnings, !Goal, !Varset, !VarTypes, !RttiVarMaps).

implicitly_quantify_clause_body_general(RecomputeNonLocals, HeadVars, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    list_to_set(HeadVars, OutsideVars),
    implicitly_quantify_goal_general(RecomputeNonLocals, OutsideVars, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps).

requantify_proc(ProcInfo0, ProcInfo) :-
    requantify_proc_general(ordinary_nonlocals, ProcInfo0, ProcInfo).

requantify_proc_general(RecomputeNonLocals, !ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_varset(!.ProcInfo, Varset0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarmaps0),
    implicitly_quantify_clause_body_general(RecomputeNonLocals, HeadVars, _,
        Goal0, Goal, Varset0, Varset, VarTypes0, VarTypes,
        RttiVarmaps0, RttiVarmaps),
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_rtti_varmaps(RttiVarmaps, !ProcInfo).

implicitly_quantify_goal(OutsideVars, Warnings, !Goal, !Varset, !VarTypes, 
        !RttiVarMaps) :-
    implicitly_quantify_goal_general(ordinary_nonlocals, OutsideVars, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps).

implicitly_quantify_goal_general(RecomputeNonLocals, OutsideVars, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    implicitly_quantify_goal_2(ordinary_nonlocals, OutsideVars, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps),
    (
        RecomputeNonLocals = code_gen_nonlocals,

        % If the goal does not contain a reconstruction, the code-gen nonlocals
        % and the ordinary nonlocals are the same.
        goal_contains_reconstruction(!.Goal)
    ->
        implicitly_quantify_goal_2(code_gen_nonlocals, OutsideVars, _,
            !Goal, !Varset, !VarTypes, !RttiVarMaps)
    ;
        true
    ).

:- pred implicitly_quantify_goal_2(nonlocals_to_recompute::in,
    set(prog_var)::in, list(quant_warning)::out,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, rtti_varmaps::in, rtti_varmaps::out) is det.

implicitly_quantify_goal_2(RecomputeNonLocals, OutsideVars0, Warnings,
        !Goal, !Varset, !VarTypes, !RttiVarMaps) :-
    OutsideVars = set_to_bitset(OutsideVars0),
    init(RecomputeNonLocals, OutsideVars, !.Varset, !.VarTypes, !.RttiVarMaps,
        QuantInfo0),
    implicitly_quantify_goal(!Goal, QuantInfo0, QuantInfo),
    get_varset(QuantInfo, !:Varset),
    get_vartypes(QuantInfo, !:VarTypes),
    get_warnings(QuantInfo, Warnings0),
    get_rtti_varmaps(QuantInfo, !:RttiVarMaps),
    list.reverse(Warnings0, Warnings).

:- pred implicitly_quantify_goal(hlds_goal::in, hlds_goal::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_goal(Goal0 - GoalInfo0, Goal - GoalInfo, !Info) :-
    get_seen(!.Info, SeenVars),
    goal_info_get_context(GoalInfo0, Context),
    implicitly_quantify_goal_2(Goal0, Goal1, Context, !Info),
    get_nonlocals(!.Info, NonLocalVars),
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    (
        % If there are any variables that are local to the goal
        % which we have come across before, then we rename them apart.
        goal_vars_bitset_choose(NonLocalsToRecompute, Goal0 - GoalInfo0,
            GoalVars0),
        difference(GoalVars0, NonLocalVars, LocalVars),
        intersect(SeenVars, LocalVars, RenameVars),
        \+ empty(RenameVars)
    ->
        rename_apart(RenameVars, RenameMap,
            Goal1 - GoalInfo0, Goal - GoalInfo1, !Info),
        %
        % Make sure that the information in the RTTI varmaps is updated
        % to reflect any new variables that we may have just introduced.
        %
        some [!RttiVarMaps] (
            get_rtti_varmaps(!.Info, !:RttiVarMaps),
            map.foldl(rtti_var_info_duplicate, RenameMap, !RttiVarMaps),
            set_rtti_varmaps(!.RttiVarMaps, !Info)
        )
    ;
        Goal = Goal1,
        GoalInfo1 = GoalInfo0
    ),
    set_goal_nonlocals(NonLocalVars, NonLocalVarsSet, GoalInfo1, GoalInfo2,
        !Info),
    %
    % If the nonlocals set has shrunk (e.g. because some optimization
    % optimizes away the other occurrences of a variable, causing it
    % to become local when previously it was nonlocal),
    % then we may need to likewise shrink the instmap delta.
    %
    goal_info_get_instmap_delta(GoalInfo2, InstMapDelta0),
    instmap_delta_restrict(NonLocalVarsSet, InstMapDelta0, InstMapDelta),
    goal_info_set_instmap_delta(InstMapDelta, GoalInfo2, GoalInfo).

:- pred implicitly_quantify_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
    prog_context::in, quant_info::in, quant_info::out) is det.

    % After this pass, explicit quantifiers are redundant,
    % since all variables which were explicitly quantified
    % have been renamed apart.  So we don't keep them.
    % We need to keep the structure, though, so that mode
    % analysis doesn't try to reorder through quantifiers.
    % (Actually it would make sense to allow mode analysis
    % to do that, but the reference manual says it doesn't,
    % so we don't.)  Thus we replace `scope(exist_quant(Vars), Goal0)'
    % with an empty quantifier `scope(exist_quant([]), Goal)'.

implicitly_quantify_goal_2(Expr0, Expr, Context, !Info) :-
    Expr0 = scope(Reason0, Goal0),
    (
        Reason0 = exist_quant(Vars0),
        Reason1 = exist_quant([])
    ;
        Reason0 = promise_purity(_, _),
        Reason1 = Reason0,
        Vars0 = []
    ;
        Reason0 = promise_solutions(_, _),
        Reason1 = Reason0,
        Vars0 = []
    ;
        Reason0 = commit(_),
        Reason1 = Reason0,
        Vars0 = []
    ;
        Reason0 = barrier(_),
        Reason1 = Reason0,
        Vars0 = []
    ;
        Reason0 = from_ground_term(_),
        Reason1 = Reason0,
        Vars0 = []
    ;
        Reason0 = trace_goal(_, _, _, _),
        Reason1 = Reason0,
        Vars0 = []
    ),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    get_quant_vars(!.Info, QuantVars),
    % Rename apart all the quantified variables that occur outside this goal.
    list_to_set(Vars0, QVars),
    intersect(OutsideVars, QVars, RenameVars1),
    intersect(LambdaOutsideVars, QVars, RenameVars2),
    union(RenameVars1, RenameVars2, RenameVars),
    ( empty(RenameVars) ->
        Goal1 = Goal0,
        Vars = Vars0,
        Reason = Reason1
    ;
        warn_overlapping_scope(RenameVars, Context, !Info),
        rename_apart(RenameVars, RenameMap, Goal0, Goal1, !Info),
        goal_util.rename_var_list(no, RenameMap, Vars0, Vars),
        (
            Reason1 = promise_solutions(PromiseVars0, Kind),
            goal_util.rename_var_list(no, RenameMap,
                PromiseVars0, PromiseVars),
            Reason = promise_solutions(PromiseVars, Kind)
        ;
            Reason1 = exist_quant(_),
            % We have already handled this case.
            Reason = Reason1
        ;
            ( Reason1 = promise_purity(_, _)
            ; Reason1 = commit(_)
            ; Reason1 = barrier(_)
            ; Reason1 = from_ground_term(_)
            ; Reason1 = trace_goal(_, _, _, _)
            ),
            Reason = Reason1
        )
    ),
    update_seen_vars(QVars, !Info),
    insert_list(QuantVars, Vars, QuantVars1),
    set_quant_vars(QuantVars1, !Info),
    implicitly_quantify_goal(Goal1, Goal, !Info),
    get_nonlocals(!.Info, NonLocals0),
    delete_list(NonLocals0, Vars, NonLocals),
    set_quant_vars(QuantVars, !Info),
    set_nonlocals(NonLocals, !Info),
    Expr = scope(Reason, Goal).

implicitly_quantify_goal_2(Expr0, Expr, _, !Info) :-
    Expr0 = conj(ConjType, Goals0),
    implicitly_quantify_conj(Goals0, Goals, !Info),
    Expr = conj(ConjType, Goals).

implicitly_quantify_goal_2(Expr0, Expr, _, !Info) :-
    Expr0 = disj(Goals0),
    implicitly_quantify_disj(Goals0, Goals, !Info),
    Expr = disj(Goals).

implicitly_quantify_goal_2(Expr0, Expr, _, !Info) :-
    Expr0 = switch(Var, Det, Cases0),
    implicitly_quantify_cases(Cases0, Cases, !Info),
    % The switch variable is guaranteed to be nonlocal to the switch, since
    % it has to be bound elsewhere, so we put it in the nonlocals here.
    get_nonlocals(!.Info, NonLocals0),
    insert(NonLocals0, Var, NonLocals),
    set_nonlocals(NonLocals, !Info),
    Expr = switch(Var, Det, Cases).

implicitly_quantify_goal_2(Expr0, Expr, _, !Info) :-
    Expr0 = negation(Goal0),
    % Quantified variables cannot be pushed inside a negation, so we insert
    % the quantified vars into the outside vars set, and initialize the new
    % quantified vars set to be empty (the lambda outside vars remain
    % unchanged).
    get_quant_vars(!.Info, QuantVars),
    get_outside(!.Info, OutsideVars),
    union(OutsideVars, QuantVars, OutsideVars1),
    init(QuantVars1),
    set_quant_vars(QuantVars1, !Info),
    set_outside(OutsideVars1, !Info),
    implicitly_quantify_goal(Goal0, Goal, !Info),
    Expr = negation(Goal),
    set_outside(OutsideVars, !Info),
    set_quant_vars(QuantVars, !Info).

    % After this pass, explicit quantifiers are redundant, since all variables
    % which were explicitly quantified have been renamed apart. So we don't
    % keep them. Thus we replace `if_then_else(Vars, ....)' with
    % `if_then_else([], ...)'.
implicitly_quantify_goal_2(Expr0, Expr, Context, !Info) :-
    Expr0 = if_then_else(Vars0, Cond0, Then0, Else0),
    get_quant_vars(!.Info, QuantVars),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    list_to_set(Vars0, QVars),
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
        warn_overlapping_scope(RenameVars, Context, !Info),
        rename_apart(RenameVars, RenameMap, Cond0, Cond1, !Info),
        goal_util.rename_some_vars_in_goal(RenameMap, Then0, Then1),
        goal_util.rename_var_list(no, RenameMap, Vars0, Vars)
    ),
    insert_list(QuantVars, Vars, QuantVars1),
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    goal_vars_both_choose(NonLocalsToRecompute, Then1,
        VarsThen, LambdaVarsThen),
    union(OutsideVars, VarsThen, OutsideVars1),
    union(LambdaOutsideVars, LambdaVarsThen, LambdaOutsideVars1),
    set_quant_vars(QuantVars1, !Info),
    set_outside(OutsideVars1, !Info),
    set_lambda_outside(LambdaOutsideVars1, !Info),
    update_seen_vars(QVars, !Info),
    implicitly_quantify_goal(Cond1, Cond, !Info),
    get_nonlocals(!.Info, NonLocalsCond),
    union(OutsideVars, NonLocalsCond, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    set_lambda_outside(LambdaOutsideVars, !Info),
    implicitly_quantify_goal(Then1, Then, !Info),
    get_nonlocals(!.Info, NonLocalsThen),
    set_outside(OutsideVars, !Info),
    set_quant_vars(QuantVars, !Info),
    implicitly_quantify_goal(Else0, Else, !Info),
    Expr = if_then_else([], Cond, Then, Else),

    get_nonlocals(!.Info, NonLocalsElse),
    union(NonLocalsCond, NonLocalsThen, NonLocalsIfThen),
    union(NonLocalsIfThen, NonLocalsElse, NonLocalsIfThenElse),
    intersect(NonLocalsIfThenElse, OutsideVars, NonLocalsO),
    intersect(NonLocalsIfThenElse, LambdaOutsideVars, NonLocalsL),
    union(NonLocalsO, NonLocalsL, NonLocals),
    set_nonlocals(NonLocals, !Info).

implicitly_quantify_goal_2(Expr, Expr, _, !Info) :-
    Expr = plain_call(_, _, HeadVars, _, _, _),
    implicitly_quantify_atomic_goal(HeadVars, !Info).

implicitly_quantify_goal_2(Expr, Expr, _, !Info) :-
    Expr = generic_call(GenericCall, CallArgVars, _, _),
    goal_util.generic_call_vars(GenericCall, ArgVars0),
    list.append(ArgVars0, CallArgVars, ArgVars),
    implicitly_quantify_atomic_goal(ArgVars, !Info).

implicitly_quantify_goal_2(Expr0, Expr, Context, !Info) :-
    Expr0 = unify(Var, UnifyRHS0, Mode, Unification0, UnifyContext),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    TypeInfoVars = get_unify_typeinfos(Unification0),
    ( Unification0 = construct(_, _, _, _, How, _, SubInfo) ->
        ( How = reuse_cell(cell_to_reuse(ReuseVar0, _, SetArgs)) ->
            MaybeSetArgs = yes(SetArgs),
            MaybeReuseVar = yes(ReuseVar0)
        ;
            MaybeSetArgs = no,
            MaybeReuseVar = no
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
        MaybeSetArgs = no,
        MaybeReuseVar = no,
        MaybeSizeVar = no
    ),
    implicitly_quantify_unify_rhs(MaybeSetArgs, Context, UnifyRHS0, UnifyRHS,
        Unification0, Unification, !Info),
    Expr = unify(Var, UnifyRHS, Mode, Unification, UnifyContext),
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
        insert(GoalVars2, SizeVar, GoalVars)
    ;
        MaybeSizeVar = no,
        GoalVars = GoalVars2
    ),
    update_seen_vars(GoalVars, !Info),
    intersect(GoalVars, OutsideVars, NonLocalVars1),
    intersect(GoalVars, LambdaOutsideVars, NonLocalVars2),
    union(NonLocalVars1, NonLocalVars2, NonLocalVars),
    set_nonlocals(NonLocalVars, !Info).

implicitly_quantify_goal_2(Expr, Expr, _, !Info) :-
    Expr = call_foreign_proc(_, _, _, Args, ExtraArgs, _, _),
    Vars = list.map(foreign_arg_var, Args),
    ExtraVars = list.map(foreign_arg_var, ExtraArgs),
    list.append(Vars, ExtraVars, AllVars),
    implicitly_quantify_atomic_goal(AllVars, !Info).

implicitly_quantify_goal_2(Expr0, Expr, Context, !Info) :-
    Expr0 = shorthand(ShorthandGoal),
    implicitly_quantify_goal_2_shorthand(ShorthandGoal, Context, Expr, !Info).

:- pred implicitly_quantify_goal_2_shorthand(shorthand_goal_expr::in,
    prog_context::in, hlds_goal_expr::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_goal_2_shorthand(bi_implication(LHS0, RHS0), Context, Goal,
        !Info) :-

    % Get the initial values of various settings.
    get_quant_vars(!.Info, QuantVars0),
    get_outside(!.Info, OutsideVars0),
    get_lambda_outside(!.Info, LambdaOutsideVars0),

    % Quantified variables cannot be pushed inside a negation, so we insert
    % the quantified vars into the outside vars set, and initialize the new
    % quantified vars set to be empty (the lambda outside vars remain
    % unchanged).
    union(OutsideVars0, QuantVars0, OutsideVars1),
    init(QuantVars1),
    LambdaOutsideVars1 = LambdaOutsideVars0,
    set_quant_vars(QuantVars1, !Info),

    % Prepare for quantifying the LHS: add variables from the RHS to the
    % outside vars and the outside lambda vars sets.
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    goal_vars_both_choose(NonLocalsToRecompute, RHS0,
        RHS_Vars, RHS_LambdaVars),
    union(OutsideVars1, RHS_Vars, LHS_OutsideVars),
    union(LambdaOutsideVars1, RHS_LambdaVars, LHS_LambdaOutsideVars),

    % Quantify the LHS.
    set_outside(LHS_OutsideVars, !Info),
    set_lambda_outside(LHS_LambdaOutsideVars, !Info),
    implicitly_quantify_goal(LHS0, LHS, !Info),
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
    implicitly_quantify_goal(RHS0, RHS, !Info),
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

    %
    % We've figured out the quantification.
    % Now expand the bi-implication according to the usual rules:
    %   LHS <=> RHS
    % ===>
    %   (LHS => RHS), (RHS => LHS)
    % ===>
    %   (not (LHS, not RHS)), (not (RHS, not LHS))
    %
    goal_info_init(GoalInfo0),
    goal_info_set_context(Context, GoalInfo0, GoalInfo1),
    set_goal_nonlocals(LHS_NonLocalVars, GoalInfo1, LHS_GI, !Info),
    set_goal_nonlocals(RHS_NonLocalVars, GoalInfo1, RHS_GI, !Info),
    set_goal_nonlocals(NonLocalVars, GoalInfo1, GI, !Info),
    NotLHS = negation(LHS) - LHS_GI,
    NotRHS = negation(RHS) - RHS_GI,
    ForwardsImplication = negation(conj(plain_conj, [LHS, NotRHS]) - GI) - GI,

    % Rename apart the local variables of the goals we've just duplicated.
    ReverseImplication0 = negation(conj(plain_conj, [RHS, NotLHS]) - GI) - GI,
    goal_vars_bitset_choose(NonLocalsToRecompute, ReverseImplication0,
        GoalVars),
    difference(GoalVars, NonLocalVars, RenameVars),
    rename_apart(RenameVars, _, ReverseImplication0, ReverseImplication,
        !Info),

    Goal = conj(plain_conj, [ForwardsImplication, ReverseImplication]).

:- pred implicitly_quantify_atomic_goal(list(prog_var)::in,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_atomic_goal(HeadVars, !Info) :-
    list_to_set(HeadVars, GoalVars),
    update_seen_vars(GoalVars, !Info),
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    intersect(GoalVars, OutsideVars, NonLocals1),
    intersect(GoalVars, LambdaOutsideVars, NonLocals2),
    union(NonLocals1, NonLocals2, NonLocals),
    set_nonlocals(NonLocals, !Info).

:- pred implicitly_quantify_unify_rhs(maybe(list(bool))::in, prog_context::in,
    unify_rhs::in, unify_rhs::out, unification::in, unification::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_unify_rhs(_, _, !RHS, !Unification, !Info) :-
    !.RHS = rhs_var(X),
    singleton_set(Vars, X),
    set_nonlocals(Vars, !Info).
implicitly_quantify_unify_rhs(ReuseArgs, _, !RHS, !Unification, !Info) :-
    !.RHS = rhs_functor(_, _, ArgVars),
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    (
        NonLocalsToRecompute = code_gen_nonlocals,
        ReuseArgs = yes(SetArgs)
    ->
        % The fields taken from the reused cell aren't
        % counted as code-gen nonlocals.
        get_updated_fields(SetArgs, ArgVars, Vars0),
        list_to_set(Vars0, Vars)
    ;
        list_to_set(ArgVars, Vars)
    ),
    set_nonlocals(Vars, !Info).
implicitly_quantify_unify_rhs(_, Context, !RHS, !Unification, !Info) :-
    !.RHS = rhs_lambda_goal(Purity, PredOrFunc, EvalMethod,
        LambdaNonLocals0, LambdaVars0, Modes, Det, Goal0),
    %
    % Note: make_hlds.m has already done most of the hard work for
    % lambda expressions. At this point, LambdaVars0 should in fact be
    % guaranteed to be fresh distinct variables. However, the code below
    % does not assume this.
    %
    get_outside(!.Info, OutsideVars0),
    list_to_set(LambdaVars0, QVars),
    % Figure out which variables have overlapping scopes because they occur
    % outside the goal and are also lambda-quantified vars.
    intersect(OutsideVars0, QVars, RenameVars0),
    ( empty(RenameVars0) ->
        true
    ;
        warn_overlapping_scope(RenameVars0, Context, !Info)
    ),
    % We need to rename apart any of the lambda vars that we have already seen,
    % since they are new instances.
    get_seen(!.Info, Seen0),
    intersect(Seen0, QVars, RenameVars1),

    union(RenameVars0, RenameVars1, RenameVars),
    rename_apart(RenameVars, RenameMap, Goal0, Goal1, !Info),
    goal_util.rename_var_list(no, RenameMap, LambdaVars0, LambdaVars),

    % Quantified variables cannot be pushed inside a lambda goal,
    % so we insert the quantified vars into the outside vars set,
    % and initialize the new quantified vars set to be empty.
    get_quant_vars(!.Info, QuantVars0),
    union(OutsideVars0, QuantVars0, OutsideVars1),
    init(QuantVars),
    set_quant_vars(QuantVars, !Info),
    % Add the lambda vars as outside vars, since they are outside of the
    % lambda goal.
    insert_list(OutsideVars1, LambdaVars, OutsideVars),
    set_outside(OutsideVars, !Info),
    % Set the LambdaOutsideVars set to empty, because variables that occur
    % outside this lambda expression only in other lambda expressions
    % should not be considered nonlocal.
    get_lambda_outside(!.Info, LambdaOutsideVars0),
    init(LambdaOutsideVars),
    set_lambda_outside(LambdaOutsideVars, !Info),
    implicitly_quantify_goal(Goal1, Goal, !Info),

    !:RHS = rhs_lambda_goal(Purity, PredOrFunc, EvalMethod,
        LambdaNonLocals, LambdaVars, Modes, Det, Goal),

    get_nonlocals(!.Info, NonLocals0),
    % Lambda-quantified variables are local.
    delete_list(NonLocals0, LambdaVars, NonLocals),
    set_quant_vars(QuantVars0, !Info),
    set_outside(OutsideVars0, !Info),
    set_lambda_outside(LambdaOutsideVars0, !Info),
    set_nonlocals(NonLocals, !Info),

    %
    % Work out the list of nonlocal curried arguments to the lambda
    % expression. This set must only ever decrease, since the first
    % approximation that make_hlds uses includes all variables in the
    % lambda expression except the quantified variables.
    %
    Goal = _ - LambdaGoalInfo,
    goal_info_get_nonlocals(LambdaGoalInfo, LambdaGoalNonLocals),
    list.filter(contains(LambdaGoalNonLocals),
        LambdaNonLocals0, LambdaNonLocals),

    %
    % For a unification that constructs a lambda expression,
    % the argument variables of the construction are the nonlocal
    % variables of the lambda expression.  So if we recompute the
    % nonlocals, we need to recompute the argument variables of
    % the construction, and hence we also need to recompute their modes.
    % The nonlocals set must only ever decrease, not increase,
    % so we can just use the old modes.
    %
    (
        !.Unification = construct(ConstructVar, ConsId, Args0,
            ArgModes0, HowToConstruct, Uniq, SubInfo)
    ->
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
        to_sorted_list(NonLocals, Args),
        map.apply_to_list(Args, ArgModesMap, ArgModes),
        !:Unification = construct(ConstructVar, ConsId, Args,
            ArgModes, HowToConstruct, Uniq, SubInfo)
    ;
        % After mode analysis, unifications with lambda variables should always
        % be construction unifications, but quantification gets invoked before
        % mode analysis, so we need to allow this case...
        true
    ).

:- pred implicitly_quantify_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_conj(!Goals, !Info) :-
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    get_vars_choose(NonLocalsToRecompute, !.Goals, FollowingVarsList),
    implicitly_quantify_conj_2(FollowingVarsList, !Goals, !Info).

:- pred implicitly_quantify_conj_2(list(pair(set_of_var))::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_conj_2(_, [], [], !Info) :-
    init(NonLocalVars),
    set_nonlocals(NonLocalVars, !Info).
implicitly_quantify_conj_2([], [_ | _], _, _, _) :-
    unexpected(this_file, "implicitly_quantify_conj_2: length mismatch").
implicitly_quantify_conj_2(
        [FollowingVars - LambdaFollowingVars | FollowingVarsList],
        [Goal0 | Goals0], [Goal | Goals], !Info) :-
    get_outside(!.Info, OutsideVars),
    get_lambda_outside(!.Info, LambdaOutsideVars),
    union(OutsideVars, FollowingVars, OutsideVars1),
    union(LambdaOutsideVars, LambdaFollowingVars, LambdaOutsideVars1),
    set_outside(OutsideVars1, !Info),
    set_lambda_outside(LambdaOutsideVars1, !Info),
    implicitly_quantify_goal(Goal0, Goal, !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    union(OutsideVars, NonLocalVars1, OutsideVars2),
    set_outside(OutsideVars2, !Info),
    set_lambda_outside(LambdaOutsideVars, !Info),
    implicitly_quantify_conj_2(FollowingVarsList, Goals0, Goals, !Info),
    get_nonlocals(!.Info, NonLocalVars2),
    union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
    intersect(NonLocalVarsConj, OutsideVars, NonLocalVarsO),
    intersect(NonLocalVarsConj, LambdaOutsideVars, NonLocalVarsL),
    union(NonLocalVarsO, NonLocalVarsL, NonLocalVars),
    set_outside(OutsideVars, !Info),
    set_nonlocals(NonLocalVars, !Info).

:- pred implicitly_quantify_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_disj([], [], !Info) :-
    init(NonLocalVars),
    set_nonlocals(NonLocalVars, !Info).
implicitly_quantify_disj([Goal0 | Goals0], [Goal | Goals], !Info) :-
    implicitly_quantify_goal(Goal0, Goal, !Info),
    get_nonlocals(!.Info, NonLocalVars0),
    implicitly_quantify_disj(Goals0, Goals, !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    union(NonLocalVars0, NonLocalVars1, NonLocalVars),
    set_nonlocals(NonLocalVars, !Info).

:- pred implicitly_quantify_cases(list(case)::in, list(case)::out,
    quant_info::in, quant_info::out) is det.

implicitly_quantify_cases([], [], !Info) :-
    init(NonLocalVars),
    set_nonlocals(NonLocalVars, !Info).
implicitly_quantify_cases([case(Cons, Goal0) | Cases0],
        [case(Cons, Goal) | Cases], !Info) :-
    implicitly_quantify_goal(Goal0, Goal, !Info),
    get_nonlocals(!.Info, NonLocalVars0),
    implicitly_quantify_cases(Cases0, Cases, !Info),
    get_nonlocals(!.Info, NonLocalVars1),
    union(NonLocalVars0, NonLocalVars1, NonLocalVars),
    set_nonlocals(NonLocalVars, !Info).

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
:- pred get_vars_choose(nonlocals_to_recompute::in, list(hlds_goal)::in,
    list(pair(set_of_var))::out) is det.

get_vars_choose(NonLocalsToRecompute, Goals, Pairs) :-
    (
        NonLocalsToRecompute = ordinary_nonlocals,
        get_vars(ordinary_nonlocals, Goals, Pairs)
    ;
        NonLocalsToRecompute = code_gen_nonlocals,
        get_vars(code_gen_nonlocals, Goals, Pairs)
    ).

:- pred get_vars(nonlocals_to_recompute, list(hlds_goal),
    list(pair(set_of_var))).
:- mode get_vars(in(ordinary_nonlocals), in, out) is det.
:- mode get_vars(in(code_gen_nonlocals), in, out) is det.

get_vars(_, [], []).
get_vars(NonLocalsToRecompute, [_Goal | Goals],
        [Set - LambdaSet | SetPairs]) :-
    get_vars_2(NonLocalsToRecompute, Goals, Set, LambdaSet, SetPairs).

:- pred get_vars_2(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var, list(pair(set_of_var))).
:- mode get_vars_2(in(ordinary_nonlocals), in, out, out, out) is det.
:- mode get_vars_2(in(code_gen_nonlocals), in, out, out, out) is det.

get_vars_2(_, [], Set, LambdaSet, []) :-
    init(Set),
    init(LambdaSet).
get_vars_2(NonLocalsToRecompute, [Goal | Goals], Set, LambdaSet,
        SetPairList) :-
    get_vars_2(NonLocalsToRecompute, Goals, Set0, LambdaSet0, SetPairList0),
    goal_vars_both(NonLocalsToRecompute, Goal, Set1, LambdaSet1),
    union(Set0, Set1, Set),
    union(LambdaSet0, LambdaSet1, LambdaSet),
    SetPairList = [Set0 - LambdaSet0 | SetPairList0].

:- pred conj_vars(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode conj_vars(in(ordinary_nonlocals), in, in, out, in, out) is det.
:- mode conj_vars(in(code_gen_nonlocals), in, in, out, in, out) is det.

conj_vars(_, [], !Set, !LambdaSet).
conj_vars(NonLocalsToRecompute, [Goal - _GoalInfo| Goals], !Set, !LambdaSet) :- 
    goal_vars_2(NonLocalsToRecompute, Goal, !Set, !LambdaSet),
    conj_vars(NonLocalsToRecompute, Goals, !Set, !LambdaSet).

:- pred disj_vars(nonlocals_to_recompute, list(hlds_goal),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode disj_vars(in(ordinary_nonlocals), in, in, out, in, out) is det.
:- mode disj_vars(in(code_gen_nonlocals), in, in, out, in, out) is det.

disj_vars(NonLocalsToRecompute, Goals, !Set, !LambdaSet) :-
    compute_disj_vars(NonLocalsToRecompute, Goals,
        [], GoalSets, [], GoalLambdaSets),
    (
        GoalSets = [],
        init(GoalsSet)
    ;
        GoalSets = [_ | _],
        union_list(GoalSets, GoalsSet)
    ),
    (
        GoalLambdaSets = [],
        init(GoalsLambdaSet)
    ;
        GoalLambdaSets = [_ | _],
        union_list(GoalLambdaSets, GoalsLambdaSet)
    ),
    union(GoalsSet, !Set),
    union(GoalsLambdaSet, !LambdaSet).

:- pred compute_disj_vars(nonlocals_to_recompute, list(hlds_goal),
    list(set_of_var), list(set_of_var), list(set_of_var), list(set_of_var)).
:- mode compute_disj_vars(in(ordinary_nonlocals), in, in, out, in, out) is det.
:- mode compute_disj_vars(in(code_gen_nonlocals), in, in, out, in, out) is det.

compute_disj_vars(_, [], !Sets, !LambdaSets).
compute_disj_vars(NonLocalsToRecompute, [Goal | Goals], !Sets, !LambdaSets) :-
    init(EmptySet),
    init(EmptyLambdaSet),
    Goal = GoalExpr - _,
    goal_vars_2(NonLocalsToRecompute, GoalExpr,
        EmptySet, GoalSet, EmptyLambdaSet, GoalLambdaSet),
    !:Sets = [GoalSet | !.Sets],
    !:LambdaSets = [GoalLambdaSet | !.LambdaSets],
    compute_disj_vars(NonLocalsToRecompute, Goals, !Sets, !LambdaSets).

:- pred case_vars(nonlocals_to_recompute, list(case),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode case_vars(in(ordinary_nonlocals), in, in, out, in, out) is det.
:- mode case_vars(in(code_gen_nonlocals), in, in, out, in, out) is det.

case_vars(NonLocalsToRecompute, Cases, !Set, !LambdaSet) :-
    compute_case_vars(NonLocalsToRecompute, Cases,
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

:- pred compute_case_vars(nonlocals_to_recompute, list(case),
    list(set_of_var), list(set_of_var), list(set_of_var), list(set_of_var)).
:- mode compute_case_vars(in(ordinary_nonlocals), in, in, out, in, out) is det.
:- mode compute_case_vars(in(code_gen_nonlocals), in, in, out, in, out) is det.

compute_case_vars(_, [], !Sets, !LambdaSets).
compute_case_vars(NonLocalsToRecompute, [Case | Cases], !Sets, !LambdaSets) :-
    Case = case(_Cons, Goal - _GoalInfo),
    init(EmptySet),
    init(EmptyLambdaSet),
    goal_vars_2(NonLocalsToRecompute, Goal,
        EmptySet, GoalSet, EmptyLambdaSet, GoalLambdaSet),
    !:Sets = [GoalSet | !.Sets],
    !:LambdaSets = [GoalLambdaSet | !.LambdaSets],
    compute_case_vars(NonLocalsToRecompute, Cases, !Sets, !LambdaSets).

:- pred union_list(list(set_of_var)::in, set_of_var::out) is det.

union_list(Sets, Union) :-
    (
        Sets = [],
        init(Union)
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
    free_goal_vars_nl(ordinary_nonlocals, Goal).

    % free_goal_vars_nl(NonLocalsToRecompute, Goal) = Vars:
    %
    % Vars is the set of variables that occur free (unquantified) in Goal,
    % excluding unset fields of reconstructions if NonLocalsToRecompute
    % is `code_gen_nonlocals'.
    %
:- func free_goal_vars_nl(nonlocals_to_recompute, hlds_goal) = set(prog_var).
:- mode free_goal_vars_nl(in(ordinary_nonlocals), in) = out is det.
:- mode free_goal_vars_nl(in(code_gen_nonlocals), in) = out is det.

free_goal_vars_nl(NonLocalsToRecompute, Goal) = bitset_to_set(BothSet) :-
    goal_vars_bitset(NonLocalsToRecompute, Goal, BothSet).

:- pred goal_vars_bitset_choose(nonlocals_to_recompute::in, hlds_goal::in,
    set_of_var::out) is det.

goal_vars_bitset_choose(NonLocalsToRecompute, Goal, BothSet) :-
    (
        NonLocalsToRecompute = ordinary_nonlocals,
        goal_vars_bitset(ordinary_nonlocals, Goal, BothSet)
    ;
        NonLocalsToRecompute = code_gen_nonlocals,
        goal_vars_bitset(code_gen_nonlocals, Goal, BothSet)
    ).

:- pred goal_vars_bitset(nonlocals_to_recompute, hlds_goal, set_of_var).
:- mode goal_vars_bitset(in(ordinary_nonlocals), in, out) is det.
:- mode goal_vars_bitset(in(code_gen_nonlocals), in, out) is det.

goal_vars_bitset(NonLocalsToRecompute, Goal, BothSet) :-
    goal_vars_both(NonLocalsToRecompute, Goal, Set, LambdaSet),
    BothSet = union(Set, LambdaSet).

:- pred goal_vars_both_choose(nonlocals_to_recompute::in, hlds_goal::in,
    set_of_var::out, set_of_var::out) is det.

goal_vars_both_choose(NonLocalsToRecompute, Goal, Set, LambdaSet) :-
    (
        NonLocalsToRecompute = ordinary_nonlocals,
        goal_vars_both(ordinary_nonlocals, Goal, Set, LambdaSet)
    ;
        NonLocalsToRecompute = code_gen_nonlocals,
        goal_vars_both(code_gen_nonlocals, Goal, Set, LambdaSet)
    ).

    % goal_vars_both(NonLocalsToRecompute, Goal, NonLambdaSet, LambdaSet):
    %
    % Set is the set of variables that occur free (unquantified) in Goal,
    % not counting occurrences in lambda expressions. LambdaSet is the set
    % of variables that occur free (unquantified) in lambda expressions
    % in Goal.
    %
:- pred goal_vars_both(nonlocals_to_recompute, hlds_goal,
    set_of_var, set_of_var).
:- mode goal_vars_both(in(ordinary_nonlocals), in, out, out) is det.
:- mode goal_vars_both(in(code_gen_nonlocals), in, out, out) is det.

goal_vars_both(NonLocalsToRecompute, Goal - _GoalInfo, Set, LambdaSet) :-
    init(Set0),
    init(LambdaSet0),
    goal_vars_2(NonLocalsToRecompute, Goal, Set0, Set, LambdaSet0, LambdaSet).

:- pred goal_vars_2(nonlocals_to_recompute, hlds_goal_expr,
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode goal_vars_2(in(ordinary_nonlocals), in, in, out, in, out) is det.
:- mode goal_vars_2(in(code_gen_nonlocals), in, in, out, in, out) is det.

goal_vars_2(NonLocalsToRecompute, unify(LHS, RHS, _, Unification, _),
        !Set, !LambdaSet) :-
    insert(!.Set, LHS, !:Set),
    ( Unification = construct(_, _, _, _, How, _, SubInfo) ->
        ( How = reuse_cell(cell_to_reuse(ReuseVar, _, SetArgs)) ->
            MaybeSetArgs = yes(SetArgs),
            insert(!.Set, ReuseVar, !:Set)
        ;
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
    ; Unification = complicated_unify(_, _, TypeInfoVars) ->
        MaybeSetArgs = no,
        insert_list(!.Set, TypeInfoVars, !:Set)
    ;
        MaybeSetArgs = no
    ),
    unify_rhs_vars(NonLocalsToRecompute, RHS, MaybeSetArgs, !Set, !LambdaSet).

goal_vars_2(_, generic_call(GenericCall, ArgVars1, _, _), !Set, !LambdaSet) :-
    goal_util.generic_call_vars(GenericCall, ArgVars0),
    insert_list(!.Set, ArgVars0, !:Set),
    insert_list(!.Set, ArgVars1, !:Set).

goal_vars_2(_, plain_call(_, _, ArgVars, _, _, _), !Set, !LambdaSet) :-
    insert_list(!.Set, ArgVars, !:Set).

goal_vars_2(NonLocalsToRecompute, conj(ConjType, Goals), !Set, !LambdaSet) :-
    (
        ConjType = plain_conj
    ;
        ConjType = parallel_conj
    ),
    conj_vars(NonLocalsToRecompute, Goals, !Set, !LambdaSet).

goal_vars_2(NonLocalsToRecompute, disj(Goals), !Set, !LambdaSet) :-
    disj_vars(NonLocalsToRecompute, Goals, !Set, !LambdaSet).

goal_vars_2(NonLocalsToRecompute, switch(Var, _Det, Cases), !Set,
        !LambdaSet) :-
    insert(!.Set, Var, !:Set),
    case_vars(NonLocalsToRecompute, Cases, !Set, !LambdaSet).

goal_vars_2(NonLocalsToRecompute, scope(Reason, Goal), Set0, !:Set,
        LambdaSet0, !:LambdaSet) :-
    goal_vars_both(NonLocalsToRecompute, Goal, !:Set, !:LambdaSet),
    (
        Reason = exist_quant(Vars),
        delete_list(!.Set, Vars, !:Set),
        delete_list(!.LambdaSet, Vars, !:LambdaSet)
    ;
        Reason = promise_purity(_, _)
    ;
        Reason = promise_solutions(Vars, _Kind),
        insert_list(!.Set, Vars, !:Set)
    ;
        Reason = commit(_)
    ;
        Reason = barrier(_)
    ;
        Reason = from_ground_term(_)
    ;
        Reason = trace_goal(_, _, _, _)
    ),
    union(Set0, !Set),
    union(LambdaSet0, !LambdaSet).

goal_vars_2(NonLocalsToRecompute, negation(Goal - _GoalInfo),
        !Set, !LambdaSet) :-
    goal_vars_2(NonLocalsToRecompute, Goal, !Set, !LambdaSet).

goal_vars_2(NonLocalsToRecompute, if_then_else(Vars, Cond, Then, Else),
        !Set, !LambdaSet) :-
    % This code does the following:
    %     !:Set = !.Set + ( (vars(Cond) + vars(Then)) \ Vars ) + vars(Else)
    % where `+' is set union and `\' is relative complement.
    goal_vars_both(NonLocalsToRecompute, Cond, CondSet, CondLambdaSet),
    goal_vars_both(NonLocalsToRecompute, Then, ThenSet, ThenLambdaSet),
    goal_vars_both(NonLocalsToRecompute, Else, ElseSet, ElseLambdaSet),
    union(CondSet, ThenSet, CondThenSet),
    union(CondLambdaSet, ThenLambdaSet, CondThenLambdaSet),
    delete_list(CondThenSet, Vars, SomeCondThenSet),
    delete_list(CondThenLambdaSet, Vars, SomeCondThenLambdaSet),
    union(!.Set, SomeCondThenSet, !:Set),
    union(!.LambdaSet, SomeCondThenLambdaSet, !:LambdaSet),
    union(!.Set, ElseSet, !:Set),
    union(!.LambdaSet, ElseLambdaSet, !:LambdaSet).

goal_vars_2(_, call_foreign_proc(_, _, _, Args, ExtraArgs, _, _), !Set,
        !LambdaSet) :-
    Vars = list.map(foreign_arg_var, Args),
    ExtraVars = list.map(foreign_arg_var, ExtraArgs),
    list.append(Vars, ExtraVars, AllVars),
    insert_list(!.Set, AllVars, !:Set).

goal_vars_2(NonLocalsToRecompute, shorthand(ShorthandGoal), !Set,
        !LambdaSet) :-
    goal_vars_2_shorthand(NonLocalsToRecompute, ShorthandGoal, !Set,
        !LambdaSet).

:- pred goal_vars_2_shorthand(nonlocals_to_recompute, shorthand_goal_expr,
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode goal_vars_2_shorthand(in(ordinary_nonlocals), in, in, out, in, out)
    is det.
:- mode goal_vars_2_shorthand(in(code_gen_nonlocals), in, in, out, in, out)
    is det.

goal_vars_2_shorthand(NonLocalsToRecompute, bi_implication(LHS, RHS), !Set,
        !LambdaSet) :-
    conj_vars(NonLocalsToRecompute, [LHS, RHS], !Set, !LambdaSet).

:- pred unify_rhs_vars(nonlocals_to_recompute, unify_rhs, maybe(list(bool)),
    set_of_var, set_of_var, set_of_var, set_of_var).
:- mode unify_rhs_vars(in(ordinary_nonlocals), in, in, in, out, in, out)
    is det.
:- mode unify_rhs_vars(in(code_gen_nonlocals), in, in, in, out, in, out)
    is det.

unify_rhs_vars(_, rhs_var(Y), _, !Set, !LambdaSet) :-
    insert(!.Set, Y, !:Set).
unify_rhs_vars(NonLocalsToRecompute, rhs_functor(_, _, ArgVars), MaybeSetArgs,
        !Set, !LambdaSet) :-
    (
        NonLocalsToRecompute = code_gen_nonlocals,
        MaybeSetArgs = yes(SetArgs)
    ->
        % Ignore the fields taken from the reused cell.
        get_updated_fields(SetArgs, ArgVars, ArgsToSet),
        insert_list(!.Set, ArgsToSet, !:Set)
    ;
        insert_list(!.Set, ArgVars, !:Set)
    ).
unify_rhs_vars(NonLocalsToRecompute,
        rhs_lambda_goal(_, _, _, _, LambdaVars, _, _, Goal), _,
        !Set, !LambdaSet) :-
    % Note that the NonLocals list is not counted, since all the
    % variables in that list must occur in the goal.
    goal_vars_bitset(NonLocalsToRecompute, Goal, GoalVars),
    delete_list(GoalVars, LambdaVars, GoalVars1),
    union(!.LambdaSet, GoalVars1, !:LambdaSet).

:- pred insert_set_fields(list(bool)::in, list(prog_var)::in,
    set_of_var::in, set_of_var::out) is det.

insert_set_fields(SetArgs, Args, !Set) :-
    get_updated_fields(SetArgs, Args,  ArgsToSet),
    insert_list(!.Set, ArgsToSet, !:Set).

:- pred get_updated_fields(list(bool)::in,
    list(prog_var)::in, list(prog_var)::out) is det.

get_updated_fields(SetArgs, Args, ArgsToSet) :-
    get_updated_fields(SetArgs, Args, [], ArgsToSet).

:- pred get_updated_fields(list(bool)::in,
    list(prog_var)::in, list(prog_var)::in, list(prog_var)::out) is det.

get_updated_fields([], [], !ArgsToSet).
get_updated_fields([], [_|_], _, _) :-
    unexpected(this_file, "get_updated_fields").
get_updated_fields([_|_], [], _, _) :-
    unexpected(this_file, "get_updated_fields").
get_updated_fields([SetArg | SetArgs], [Arg | Args], !ArgsToSet) :-
    (
        SetArg = yes,
        !:ArgsToSet = [Arg | !.ArgsToSet]
    ;
        SetArg = no,
        !:ArgsToSet = !.ArgsToSet
    ),
    get_updated_fields(SetArgs, Args, !ArgsToSet).

:- func get_unify_typeinfos(unification) = list(prog_var).

get_unify_typeinfos(Unification) =
    ( Unification = complicated_unify(_, _, TypeInfoVars0) ->
        TypeInfoVars0
    ;
        []
    ).

%-----------------------------------------------------------------------------%

:- pred warn_overlapping_scope(set_of_var::in, prog_context::in,
    quant_info::in, quant_info::out) is det.

warn_overlapping_scope(OverlapVars, Context, !Info) :-
    to_sorted_list(OverlapVars, Vars),
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
:- pred rename_apart(set_of_var::in, map(prog_var, prog_var)::out,
    hlds_goal::in, hlds_goal::out, quant_info::in, quant_info::out) is det.

rename_apart(RenameSet, RenameMap, !Goal, !Info) :-
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    (
        %
        % Don't rename apart variables when recomputing the code-gen nonlocals
        % -- that would stuff up the ordinary nonlocals and the mode
        % information. The ordinary nonlocals are always recomputed
        % before the code-gen nonlocals -- any necessary renaming will have
        % been done while recomputing the ordinary nonlocals.
        %
        ( empty(RenameSet)
        ; NonLocalsToRecompute = code_gen_nonlocals
        )
    ->
        map.init(RenameMap)
    ;
        to_sorted_list(RenameSet, RenameList),
        get_varset(!.Info, Varset0),
        get_vartypes(!.Info, VarTypes0),
        map.init(RenameMap0),
        goal_util.create_variables(RenameList, Varset0, VarTypes0,
            Varset0, Varset, VarTypes0, VarTypes, RenameMap0, RenameMap),
        goal_util.rename_some_vars_in_goal(RenameMap, !Goal),
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

:- pred set_goal_nonlocals(set_of_var::in,
    hlds_goal_info::in, hlds_goal_info::out,
    quant_info::in, quant_info::out) is det.

set_goal_nonlocals(NonLocals, !GoalInfo, !Info) :-
    set_goal_nonlocals(NonLocals, _, !GoalInfo, !Info).

:- pred set_goal_nonlocals(set_of_var::in, set(prog_var)::out,
    hlds_goal_info::in, hlds_goal_info::out,
    quant_info::in, quant_info::out) is det.

set_goal_nonlocals(NonLocals0, NonLocals, !GoalInfo, !Info) :-
    NonLocals = bitset_to_set(NonLocals0),
    get_nonlocals_to_recompute(!.Info, NonLocalsToRecompute),
    (
        NonLocalsToRecompute = ordinary_nonlocals,
        goal_info_set_nonlocals(NonLocals, !GoalInfo)
    ;
        NonLocalsToRecompute = code_gen_nonlocals,
        goal_info_set_code_gen_nonlocals(NonLocals, !GoalInfo)
    ).

%-----------------------------------------------------------------------------%

:- func bitset_to_set(set_of_var) = set(prog_var).

bitset_to_set(Bitset) = set.sorted_list_to_set(to_sorted_list(Bitset)).

:- func set_to_bitset(set(prog_var)) = set_of_var.

set_to_bitset(Bitset) = sorted_list_to_set(set.to_sorted_list(Bitset)).

%-----------------------------------------------------------------------------%

:- pred init(nonlocals_to_recompute::in, set_of_var::in,
    prog_varset::in, vartypes::in, rtti_varmaps::in, quant_info::out) is det.

init(RecomputeNonLocals, OutsideVars, Varset, VarTypes, RttiVarMaps,
        QuantInfo) :-
    OverlapWarnings = [],
    QuantInfo = quant_info(RecomputeNonLocals, OutsideVars, QuantVars,
        LambdaOutsideVars, NonLocals, Seen, Varset, VarTypes, OverlapWarnings,
        RttiVarMaps),
    init(QuantVars),
    init(NonLocals),
    init(LambdaOutsideVars),
    Seen = OutsideVars.

:- pred get_nonlocals_to_recompute(quant_info::in, nonlocals_to_recompute::out)
    is det.
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

get_nonlocals_to_recompute(Q, Q ^ nonlocals_to_recompute).
get_outside(Q, Q ^ outside).
get_quant_vars(Q, Q ^ quant_vars).
get_lambda_outside(Q, Q ^ lambda_outside).
get_nonlocals(Q, Q ^ nonlocals).
get_seen(Q, Q ^ seen).
get_varset(Q, Q ^ varset).
get_vartypes(Q, Q ^ vartypes).
get_warnings(Q, Q ^ warnings).
get_rtti_varmaps(Q, Q ^ rtti_varmaps).

set_outside(Outside, Q, Q ^ outside := Outside).
set_quant_vars(QuantVars, Q, Q ^ quant_vars := QuantVars).
set_lambda_outside(LambdaOutside, Q, Q ^ lambda_outside := LambdaOutside).
set_nonlocals(NonLocals, Q, Q ^ nonlocals := NonLocals).
set_seen(Seen, Q, Q ^ seen := Seen).
set_varset(Varset, Q, Q ^ varset := Varset).
set_vartypes(VarTypes, Q, Q ^ vartypes := VarTypes).
set_warnings(Warnings, Q, Q ^ warnings := Warnings).
set_rtti_varmaps(RttiVarMaps, Q, Q ^ rtti_varmaps := RttiVarMaps).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "quantification.m".

%-----------------------------------------------------------------------------%
:- end_module quantification.
%-----------------------------------------------------------------------------%
