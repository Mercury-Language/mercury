%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module provides mechanisms to
%
% - map each variable in a predicate to its earliest occurrence in each
%   clause of a given predicate (its origins), and
%
% - to map descriptions of such origins into text that can be included
%   in error messages.
%
% The overall intention is to improve the readability of error messages
% such as
%
%   Warning: unresolved polymorphism in predicate `maybe_throw_pred'/2.
%   The variable with an unbound type was:
%       `V_7': exception.exception_result(T).
%   The unbound type variable `T' will be implicitly bound to
%   the builtin type `void'.
%
% Each of these lines has traditionally been prefixed by the context
% of the predicate as a whole. When the subject of the diagnostic is
% a named variable, this does not matter, since users can look for
% occurrences of that variable in the predicate's code. However, for
% unnamed variables, they cannot. This is why this module provides
% the mechanisms needed to extend the diagnostic with text such as
%
%   `V_7' represents the term `exception(Exception)'.
%
% with the preceding context pinpointing the line where the unnamed
% variable first occurs. See tests/warnings/unresolved_polymorphism_anon.m.
%
% Because this intention is to help with error messages created during
% type analysis, this module does not look at, or use, any part of the HLDS
% that is meaningful during type analysis, such as instmap_deltas.
%
%---------------------------------------------------------------------------%

:- module hlds.var_origins.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%

:- type record_var_origin(T)
    == (pred(var_origins_map, prog_var, var_origin, T, T)).
:- inst record_var_origin           % for record_var_origin/1
    == (pred(in, in, in, in, out) is det).

    % Values of this type record the location of the origin,
    % i.e. the first occurrence, of a variable. This means
    % location both in terms of filename/linenumber, and in terms of
    % what part of what goal.
:- type var_origin
    --->    var_origin_clause_head(
                % The variable's first occurrence is in the clause head.
                voch_context    :: prog_context,

                % The number of the clause in the predicate's definition.
                % The first clause is clause #1.
                voch_clause     :: uint,

                % The position of the variable in the clause head.
                % The first argument is argument #1.
                voch_arg_num    :: uint
            )
    ;       var_origin_lambda_head(
                % The variable's first occurrence is in the head of
                % a lambda expression.
                volh_context    :: prog_context,
                volh_p_or_f     :: pred_or_func,
                volh_arg_num    :: uint
            )
    ;       var_origin_unify_var(
                vouv_context    :: prog_context,
                % The other variable that the variable mapped to this origin
                % is unified with.
                vouv_lorhs      :: lhs_or_rhs
            )
    ;       var_origin_unify_func(
                vouf_context    :: prog_context,

                % The cons_id in the unification.
                vouf_cons_id    :: cons_id,

                % If the variable mapped to this origin is the LHS var,
                % then this lists the RHS vars.
                % If the variable mapped to this origin is one of the RHS vars,
                % then this gives its position on the RHS, as well as
                % the LHS var.
                vouf_lorhsa     :: lhs_or_rhs_arg
            )
    ;       var_origin_plain_call(
                vopc_context    :: prog_context,
                % The id of the callee, and the number of the argument
                % position that is occupied by the variable mapped
                % to this origin. (If that variable is only a *component*
                % of the argument, and not the whole argument, then its
                % origin will be unification introduced by the code of
                % superhomoneous.m.)
                vopc_callee     :: pred_id,
                vopc_arg_num    :: uint
            )
    ;       var_origin_foreign_call(
                vofc_context    :: prog_context,
                % We treat foreign language calls the same way as plain calls.
                vofc_callee     :: pred_id,
                vofc_arg_num    :: uint
            )
    ;       var_origin_generic_call(
                vogc_context    :: prog_context,
                % We treat treat language calls similarly to plain calls,
                % just with a different specification of the callee.
                vogc_callee     :: generic_call,
                vogc_arg_num    :: uint
            ).

:- type lhs_or_rhs
    --->    lor_lhs(
                    rhs_var     :: prog_var
            )
    ;       lor_rhs(
                    lhs_var     :: prog_var
            ).

:- type lhs_or_rhs_arg
    --->    lora_lhs(
                rhs_argvars     :: list(prog_var)
            )
    ;       lora_rhs(
                lhs_var         :: prog_var,
                rhs_arg_num     :: uint
            ).

    % Values of this type map each variable to
    %
    % - its origin, if it has only one origin (that we have seen so far),
    %
    % - but if the variable has its origin in a (possibly nested) branched
    %   control structure, it will contain its origin on each branch.
    %   Note that multiple clauses, being equivalent to multiple disjuncts,
    %   count as branches of a branched control structure.
:- type var_origins_map == map(prog_var, set(var_origin)).

    % compute_var_origins_in_pred(CollectPred, PredInfo0, OriginsMap, !Acc):
    %
    % Given a predicate, construct and return OriginsMap, which will map
    % each variable in its clauses to its set of origins.
    %
    % Note that one kind of goal, negations, introduce scopes that 'forget'
    % origins. This is because if a variable is generated in a negated goal,
    % it cannot be referred to from outside the negation. Therefore any
    % additions to OriginsMap inside a negated goal will be effectivel
    % forgotten when the traversal leaves the negation.
    %
    % This is why instead of letting our caller post-process OriginsMap,
    % we allow it to process each addition to OriginsMap as it is made.
    % The traversal will call CollectPred after each such addition,
    % passing it both the already-updated OriginsMap, and the pair of
    % <variable, origin> that was just added.
    %
:- pred compute_var_origins_in_pred(
    record_var_origin(T)::in(record_var_origin),
    pred_info::in, var_origins_map::out, T::in, T::out) is det.

%---------------------------------------------------------------------------%

    % This predicate is intended to be invoked from the CollectPred
    % passed to compute_var_origins_in_pred. Given the overall HLDS
    % and the var_table of the predicate being processed, it returns
    % a description of the variable and its origin. The output will
    % look like this:
    %
    %   <context>: `V_7' represents the term `exception(Exception)'.
    %
:- pred explain_var_origin(module_info::in, var_table::in,
    prog_var::in, var_origin::in, list(error_msg)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.var_db.

:- import_module assoc_list.
:- import_module int.
:- import_module pair.
:- import_module uint.

%---------------------------------------------------------------------------%

compute_var_origins_in_pred(CollectPred, PredInfo0, OriginsMap, !Acc) :-
    pred_info_get_clauses_info(PredInfo0, ClausesInfo0),
    clauses_info_get_headvar_list(ClausesInfo0, HeadVars),
    clauses_info_get_clauses_rep(ClausesInfo0, ClausesRep0, _ItemNumbers),
    get_clause_list(Clauses, ClausesRep0, _ClausesRep),
    compute_var_origins_in_clauses(CollectPred, HeadVars, 1u, Clauses,
        [], RevOriginsMapList, !Acc),
    list.reverse(RevOriginsMapList, OriginsMapList),
    represent_origins_on_all_branches(OriginsMapList, OriginsMap).

:- pred compute_var_origins_in_clauses(
    record_var_origin(T)::in(record_var_origin), list(prog_var)::in,
    uint::in, list(clause)::in,
    list(var_origins_map)::in, list(var_origins_map)::out,
    T::in, T::out) is det.

compute_var_origins_in_clauses(_CollectPred, _HeadVars,
        _CurClauseNum, [], !RevOriginsMapList, !Acc).
compute_var_origins_in_clauses(CollectPred, HeadVars,
        CurClauseNum, [Clause | Clauses], !RevOriginsMapList, !Acc) :-
    Clause = clause(_ApplicableProcs, BodyGoal, Lang, Context,
        _StateVarWarnings),
    (
        Lang = impl_lang_mercury,
        OriginClauseHead = origin_clause_head(Context, CurClauseNum, HeadVars),
        update_var_origins_clause_head(CollectPred, OriginClauseHead,
            OriginsMap1, !Acc),
        compute_var_origins_in_goal(CollectPred, BodyGoal,
            OriginsMap1, OriginsMap, !Acc),
        !:RevOriginsMapList = [OriginsMap | !.RevOriginsMapList],
        NextClauseNum = CurClauseNum + 1u
    ;
        Lang = impl_lang_foreign(_),
        NextClauseNum = CurClauseNum
    ),
    compute_var_origins_in_clauses(CollectPred, HeadVars,
        NextClauseNum, Clauses, !RevOriginsMapList, !Acc).

%---------------------------------------------------------------------------%

:- pred compute_var_origins_in_goal(
    record_var_origin(T)::in(record_var_origin), hlds_goal::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

compute_var_origins_in_goal(CollectPred, Goal, !OriginsMap, !Acc) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    (
        GoalExpr = unify(LHSVar, RHS, _, _, _),
        (
            RHS = rhs_var(RHSVar),
            OriginUnifyVar = origin_unify_var(Context, LHSVar, RHSVar),
            update_var_origins_unify_var(CollectPred, OriginUnifyVar,
                !OriginsMap, !Acc)
        ;
            RHS = rhs_functor(ConsId, _IsExistConstr, RHSVars),
            OriginUnifyFunc = origin_unify_func(Context,
                LHSVar, ConsId, RHSVars),
            update_var_origins_unify_func(CollectPred, OriginUnifyFunc,
                !OriginsMap, !Acc)
        ;
            RHS = rhs_lambda_goal(_, _, PredOrFunc, _, VarsModes,
                _, LambdaGoal),
            OriginLambdaHead = origin_lambda_head(Context, PredOrFunc,
                VarsModes),
            update_var_origins_lambda_head(CollectPred, OriginLambdaHead,
                !.OriginsMap, OriginsMapAfterLambdaHead, !Acc),
            compute_var_origins_in_goal(CollectPred, LambdaGoal,
                OriginsMapAfterLambdaHead, _OriginsMapAfterLambda, !Acc)
        )
    ;
        GoalExpr = plain_call(PredId, _, ArgVars, _, _, _),
        OriginPlainCall = origin_plain_call(Context, PredId, ArgVars),
        update_var_origins_plain_call(CollectPred, OriginPlainCall,
            !OriginsMap, !Acc)
    ;
        GoalExpr = call_foreign_proc(_, PredId, _, ArgVars, _, _, _),
        OriginForeignCall = origin_foreign_call(Context, PredId, ArgVars),
        update_var_origins_foreign_call(CollectPred, OriginForeignCall,
            !OriginsMap, !Acc)
    ;
        GoalExpr = generic_call(GCall, ArgVars, _, _, _),
        OriginGenericCall = origin_generic_call(Context, GCall, ArgVars),
        update_var_origins_generic_call(CollectPred, OriginGenericCall,
            !OriginsMap, !Acc)
    ;
        GoalExpr = conj(_ConjType, Conjuncts),
        compute_var_origins_in_conj(CollectPred, Conjuncts, !OriginsMap, !Acc)
    ;
        GoalExpr = disj(Disjuncts),
        compute_var_origins_in_disj(CollectPred, !.OriginsMap, Disjuncts,
            [], OriginsMaps, !Acc),
        represent_origins_on_all_branches(OriginsMaps, !:OriginsMap)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        compute_var_origins_in_cases(CollectPred, !.OriginsMap, Cases,
            [], OriginsMaps, !Acc),
        represent_origins_on_all_branches(OriginsMaps, !:OriginsMap)
    ;
        GoalExpr = negation(SubGoal),
        compute_var_origins_in_goal(CollectPred, SubGoal,
            !.OriginsMap, _OriginsMapAfterNegation, !Acc)
    ;
        GoalExpr = scope(_, SubGoal),
        compute_var_origins_in_goal(CollectPred, SubGoal, !OriginsMap, !Acc)
    ;
        GoalExpr = if_then_else(_, CondGoal, ThenGoal, ElseGoal),
        OriginsMap0 = !.OriginsMap,
        compute_var_origins_in_goal(CollectPred, CondGoal,
            OriginsMap0, OriginsMapAfterCond, !Acc),
        compute_var_origins_in_goal(CollectPred, ThenGoal,
            OriginsMapAfterCond, OriginsMapAfterThen, !Acc),
        compute_var_origins_in_goal(CollectPred, ElseGoal,
            OriginsMap0, OriginsMapAfterElse, !Acc),
        OriginsMaps = [OriginsMapAfterThen, OriginsMapAfterElse],
        represent_origins_on_all_branches(OriginsMaps, !:OriginsMap)
    ;
        GoalExpr = shorthand(Shorthand),
        (
            Shorthand = bi_implication(_, _)
            % These should have been expanded by now.
        ;
            Shorthand = atomic_goal(_, _, _, _, _, _, _)
            % These are not yet used.
        ;
            Shorthand = try_goal(_, _, SubGoal),
            compute_var_origins_in_goal(CollectPred, SubGoal,
                !OriginsMap, !Acc)
        )
    ).

%---------------------%

:- pred compute_var_origins_in_conj(
    record_var_origin(T)::in(record_var_origin), list(hlds_goal)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

compute_var_origins_in_conj(_CollectPred, [], !OriginsMap, !Acc).
compute_var_origins_in_conj(CollectPred, [Conjunct | Conjuncts],
        !OriginsMap, !Acc) :-
    compute_var_origins_in_goal(CollectPred, Conjunct, !OriginsMap, !Acc),
    compute_var_origins_in_conj(CollectPred, Conjuncts, !OriginsMap, !Acc).

%---------------------%

:- pred compute_var_origins_in_disj(
    record_var_origin(T)::in(record_var_origin), var_origins_map::in,
    list(hlds_goal)::in,
    list(var_origins_map)::in, list(var_origins_map)::out,
    T::in, T::out) is det.

compute_var_origins_in_disj(_CollectPred, _InitialOriginsMap,
        [], !RevOriginsMaps, !Acc).
compute_var_origins_in_disj(CollectPred, InitialOriginsMap,
        [Disjunct | Disjuncts], !RevOriginsMaps, !Acc) :-
    compute_var_origins_in_goal(CollectPred, Disjunct,
        InitialOriginsMap, DisjunctOriginsMap, !Acc),
    !:RevOriginsMaps = [DisjunctOriginsMap | !.RevOriginsMaps],
    compute_var_origins_in_disj(CollectPred, InitialOriginsMap,
        Disjuncts, !RevOriginsMaps, !Acc).

:- pred compute_var_origins_in_cases(
    record_var_origin(T)::in(record_var_origin), var_origins_map::in,
    list(case)::in,
    list(var_origins_map)::in, list(var_origins_map)::out,
    T::in, T::out) is det.

compute_var_origins_in_cases(_CollectPred, _InitialOriginsMap,
        [], !RevOriginsMaps, !Acc).
compute_var_origins_in_cases(CollectPred, InitialOriginsMap,
        [Case | Cases], !RevOriginsMaps, !Acc) :-
    Case = case(_MainConsId, _OtherConsIds, SubGoal),
    compute_var_origins_in_goal(CollectPred, SubGoal,
        InitialOriginsMap, CaseOriginsMap, !Acc),
    !:RevOriginsMaps = [CaseOriginsMap | !.RevOriginsMaps],
    compute_var_origins_in_cases(CollectPred, InitialOriginsMap,
        Cases, !RevOriginsMaps, !Acc).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Descriptions of the various atomic goals that may contain the first
% occurrence of a variable.
%
% Each of the following subsections handles the recording of all the variable
% occurrences in each of these kinds of atomic goals.
%

:- type origin_clause_head
    --->    origin_clause_head(
                och_context     :: prog_context,
                och_clause      :: uint,
                och_args        :: list(prog_var)
            ).

:- type origin_lambda_head
    --->    origin_lambda_head(
                olh_context     :: prog_context,
                olh_p_or_f      :: pred_or_func,
                olh_args        :: assoc_list(prog_var, mer_mode)
            ).

:- type origin_unify_var
    --->    origin_unify_var(
                ouv_context     :: prog_context,
                ouv_lhs         :: prog_var,
                ouv_rhs         :: prog_var
            ).

:- type origin_unify_func
    --->    origin_unify_func(
                ouf_context     :: prog_context,
                ouf_lhs         :: prog_var,
                ouf_cons_id     :: cons_id,
                ouf_rhs         :: list(prog_var)
            ).

:- type origin_plain_call
    --->    origin_plain_call(
                opc_context     :: prog_context,
                opc_callee      :: pred_id,
                opc_args        :: list(prog_var)
            ).

:- type origin_foreign_call
    --->    origin_foreign_call(
                ofc_context     :: prog_context,
                ofc_callee      :: pred_id,
                ofc_args        :: list(foreign_arg)
            ).

:- type origin_generic_call
    --->    origin_generic_call(
                ogc_context     :: prog_context,
                ogc_callee      :: generic_call,
                ogc_args        :: list(prog_var)
            ).

%---------------------%

:- pred update_var_origins_clause_head(
    record_var_origin(T)::in(record_var_origin), origin_clause_head::in,
    var_origins_map::out, T::in, T::out) is det.

update_var_origins_clause_head(CollectPred, OriginClauseHead,
        !:OriginsMap, !Acc) :-
    OriginClauseHead = origin_clause_head(Context, ClauseNum, ArgVars),
    map.init(!:OriginsMap),
    update_var_origins_clause_head_args(CollectPred, Context, ClauseNum,
        1u, ArgVars, !OriginsMap, !Acc).

:- pred update_var_origins_clause_head_args(
    record_var_origin(T)::in(record_var_origin),
    prog_context::in, uint::in, uint::in, list(prog_var)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_clause_head_args(_CollectPred, _Context,
        _CurClauseNum, _CurArgNum, [], !OriginsMap, !Acc).
update_var_origins_clause_head_args(CollectPred, Context,
        CurClauseNum, CurArgNum, [ArgVar | ArgVars], !OriginsMap, !Acc) :-
    Origin = var_origin_clause_head(Context, CurClauseNum, CurArgNum),
    update_var_origin(CollectPred, ArgVar, Origin, !OriginsMap, !Acc),
    update_var_origins_clause_head_args(CollectPred, Context,
        CurClauseNum, CurArgNum + 1u, ArgVars, !OriginsMap, !Acc).

%---------------------%

:- pred update_var_origins_lambda_head(
    record_var_origin(T)::in(record_var_origin), origin_lambda_head::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_lambda_head(CollectPred, OriginLambdaHead,
        !OriginsMap, !Acc) :-
    OriginLambdaHead = origin_lambda_head(Context, PredOrFunc, ArgVarsModes),
    update_var_origins_lambda_head_args(CollectPred, Context, PredOrFunc,
        1u, ArgVarsModes, !OriginsMap, !Acc).

:- pred update_var_origins_lambda_head_args(
    record_var_origin(T)::in(record_var_origin),
    prog_context::in, pred_or_func::in, uint::in,
    assoc_list(prog_var, mer_mode)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_lambda_head_args(_CollectPred, _Context, _PredOrFunc,
        _CurArgNum, [], !OriginsMap, !Acc).
update_var_origins_lambda_head_args(CollectPred, Context, PredOrFunc,
        CurArgNum, [ArgVarMode | ArgVarsModes], !OriginsMap, !Acc) :-
    ArgVarMode = ArgVar - _Mode,
    Origin = var_origin_lambda_head(Context, PredOrFunc, CurArgNum),
    update_var_origin(CollectPred, ArgVar, Origin, !OriginsMap, !Acc),
    update_var_origins_lambda_head_args(CollectPred, Context, PredOrFunc,
        CurArgNum + 1u, ArgVarsModes, !OriginsMap, !Acc).

%---------------------%

:- pred update_var_origins_unify_var(
    record_var_origin(T)::in(record_var_origin), origin_unify_var::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_unify_var(CollectPred, OriginUnifyVar,
        !OriginsMap, !Acc) :-
    OriginUnifyVar = origin_unify_var(Context, LHSVar, RHSVar),
    OriginLHS = var_origin_unify_var(Context, lor_lhs(RHSVar)),
    OriginRHS = var_origin_unify_var(Context, lor_rhs(LHSVar)),
    update_var_origin(CollectPred, LHSVar, OriginLHS, !OriginsMap, !Acc),
    update_var_origin(CollectPred, RHSVar, OriginRHS, !OriginsMap, !Acc).

%---------------------%

:- pred update_var_origins_unify_func(
    record_var_origin(T)::in(record_var_origin), origin_unify_func::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_unify_func(CollectPred, OriginUnifyFunc,
        !OriginsMap, !Acc) :-
    OriginUnifyFunc = origin_unify_func(Context, LHSVar, ConsId, RHSVars),
    OriginLHS = var_origin_unify_func(Context, ConsId, lora_lhs(RHSVars)),
    update_var_origin(CollectPred, LHSVar, OriginLHS, !OriginsMap, !Acc),
    update_var_origins_unify_func_args(CollectPred, Context, LHSVar, ConsId,
        1u, RHSVars, !OriginsMap, !Acc).

:- pred update_var_origins_unify_func_args(
    record_var_origin(T)::in(record_var_origin),
    prog_context::in, prog_var::in, cons_id::in, uint::in, list(prog_var)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_unify_func_args(_CollectPred, _Context, _LHSVar, _ConsId,
        _CurArgNum, [], !OriginsMap, !Acc).
update_var_origins_unify_func_args(CollectPred, Context, LHSVar, ConsId,
        CurArgNum, [RHSVar | RHSVars], !OriginsMap, !Acc) :-
    OriginRHS = var_origin_unify_func(Context, ConsId,
        lora_rhs(LHSVar, CurArgNum)),
    update_var_origin(CollectPred, RHSVar, OriginRHS, !OriginsMap, !Acc),
    update_var_origins_unify_func_args(CollectPred, Context, LHSVar, ConsId,
        CurArgNum + 1u, RHSVars, !OriginsMap, !Acc).

%---------------------%

:- pred update_var_origins_plain_call(
    record_var_origin(T)::in(record_var_origin), origin_plain_call::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_plain_call(CollectPred, OriginPlainCall,
        !OriginsMap, !Acc) :-
    OriginPlainCall = origin_plain_call(Context, PredId, ArgVars),
    update_var_origins_plain_call_args(CollectPred, Context, PredId,
        1u, ArgVars, !OriginsMap, !Acc).

:- pred update_var_origins_plain_call_args(
    record_var_origin(T)::in(record_var_origin),
    prog_context::in, pred_id::in, uint::in, list(prog_var)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_plain_call_args(_CollectPred, _Context, _PredId,
        _CurArgNum, [], !OriginsMap, !Acc).
update_var_origins_plain_call_args(CollectPred, Context, PredId,
        CurArgNum, [ArgVar | ArgVars], !OriginsMap, !Acc) :-
    OriginArg = var_origin_plain_call(Context, PredId, CurArgNum),
    update_var_origin(CollectPred, ArgVar, OriginArg, !OriginsMap, !Acc),
    update_var_origins_plain_call_args(CollectPred, Context, PredId,
        CurArgNum + 1u, ArgVars, !OriginsMap, !Acc).

%---------------------%

:- pred update_var_origins_foreign_call(
    record_var_origin(T)::in(record_var_origin), origin_foreign_call::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_foreign_call(CollectPred, OriginPlainCall,
        !OriginsMap, !Acc) :-
    OriginPlainCall = origin_foreign_call(Context, PredId, ForeignArgs),
    update_var_origins_foreign_call_args(CollectPred, Context, PredId,
        1u, ForeignArgs, !OriginsMap, !Acc).

:- pred update_var_origins_foreign_call_args(
    record_var_origin(T)::in(record_var_origin),
    prog_context::in, pred_id::in, uint::in, list(foreign_arg)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_foreign_call_args(_CollectPred, _Context, _PredId,
        _CurArgNum, [], !OriginsMap, !Acc).
update_var_origins_foreign_call_args(CollectPred, Context, PredId,
        CurArgNum, [ForeignArg | ForeignArgs], !OriginsMap, !Acc) :-
    ArgVar = foreign_arg_var(ForeignArg),
    OriginArg = var_origin_foreign_call(Context, PredId, CurArgNum),
    update_var_origin(CollectPred, ArgVar, OriginArg, !OriginsMap, !Acc),
    update_var_origins_foreign_call_args(CollectPred, Context, PredId,
        CurArgNum + 1u, ForeignArgs, !OriginsMap, !Acc).

%---------------------%

:- pred update_var_origins_generic_call(
    record_var_origin(T)::in(record_var_origin), origin_generic_call::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_generic_call(CollectPred, OriginPlainCall,
        !OriginsMap, !Acc) :-
    OriginPlainCall = origin_generic_call(Context, GCall, ForeignArgs),
    update_var_origins_generic_call_args(CollectPred, Context, GCall,
        1u, ForeignArgs, !OriginsMap, !Acc).

:- pred update_var_origins_generic_call_args(
    record_var_origin(T)::in(record_var_origin),
    prog_context::in, generic_call::in, uint::in, list(prog_var)::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origins_generic_call_args(_CollectPred, _Context, _GCall,
        _CurArgNum, [], !OriginsMap, !Acc).
update_var_origins_generic_call_args(CollectPred, Context, GCall,
        CurArgNum, [ArgVar | ArgVars], !OriginsMap, !Acc) :-
    OriginArg = var_origin_generic_call(Context, GCall, CurArgNum),
    update_var_origin(CollectPred, ArgVar, OriginArg, !OriginsMap, !Acc),
    update_var_origins_generic_call_args(CollectPred, Context, GCall,
        CurArgNum + 1u, ArgVars, !OriginsMap, !Acc).

%---------------------------------------------------------------------------%

:- pred update_var_origin(record_var_origin(T)::in(record_var_origin),
    prog_var::in, var_origin::in,
    var_origins_map::in, var_origins_map::out, T::in, T::out) is det.

update_var_origin(CollectPred, Var, VarOrigin, !OriginsMap, !Acc) :-
    ( if map.search(!.OriginsMap, Var, _VarOriginsCord0) then
        % set.insert(VarOrigin, VarOriginsCord0, VarOriginsCord),
        % map.det_update(Var, VarOriginsCord, !OriginsMap)
        % ZZZ
        true
    else
        VarOriginsCord = set.make_singleton_set(VarOrigin),
        map.det_insert(Var, VarOriginsCord, !OriginsMap),
        CollectPred(!.OriginsMap, Var, VarOrigin, !Acc)
    ).

%---------------------------------------------------------------------------%

:- pred represent_origins_on_all_branches(list(var_origins_map)::in,
    var_origins_map::out) is det.

represent_origins_on_all_branches(OriginsMapList, OriginsMap) :-
    (
        OriginsMapList = [],
        map.init(OriginsMap)
    ;
        OriginsMapList = [HeadOriginsMap | TailOriginsMaps],
        map.union_list(set.union, HeadOriginsMap, TailOriginsMaps, OriginsMap)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

explain_var_origin(ModuleInfo, VarTable, Var, Origin, [Msg]) :-
    VarStr = mercury_var_to_string(VarTable, print_name_only, Var),
    (
        Origin = var_origin_clause_head(Context, ClauseNum, ArgNum),
        ArgNumInt = uint.cast_to_int(ArgNum),
        ClauseNumInt = uint.cast_to_int(ClauseNum),
        Pieces = [quote(VarStr), words("is the"),
            nth_fixed(ArgNumInt), words("argument inthe head of"),
            words("clause"), int_fixed(ClauseNumInt), suffix("."), nl]
    ;
        Origin = var_origin_lambda_head(Context, _PredOrFunc, ArgNum),
        ArgNumInt = uint.cast_to_int(ArgNum),
        Pieces = [quote(VarStr), words("is the"),
            nth_fixed(ArgNumInt), words("argument of"),
            words("the lambda expression."), nl]
    ;
        Origin = var_origin_unify_var(Context, LHSorRHS),
        ( LHSorRHS = lor_lhs(OtherVar)
        ; LHSorRHS = lor_rhs(OtherVar)
        ),
        OtherVarStr =
            mercury_var_to_string(VarTable, print_name_only, OtherVar),
        Pieces = [quote(VarStr), words("is unified with"),
            quote(OtherVarStr), suffix("."), nl]
    ;
        Origin = var_origin_unify_func(Context, ConsId, LHSorRHSArg),
        (
            LHSorRHSArg = lora_lhs(RHSArgVars),
            TermStr = functor_cons_id_to_string(ModuleInfo,
                vns_var_table(VarTable), print_name_only, ConsId, RHSArgVars),
            Pieces = [quote(VarStr), words("represents the term"),
                quote(TermStr), suffix("."), nl]
        ;
            LHSorRHSArg = lora_rhs(LHSVar, ArgNum),
            ArgNumInt = uint.cast_to_int(ArgNum),
            ConsIdStr = mercury_cons_id_to_string(output_mercury,
                does_not_need_brackets, ConsId),
            LHSVarStr =
                mercury_var_to_string(VarTable, print_name_only, LHSVar),
            Pieces = [quote(VarStr), words("is the"),
                nth_fixed(ArgNumInt), words("argument of the function symbol"),
                quote(ConsIdStr), words("unified with"),
                quote(LHSVarStr), suffix("."), nl]
        )
    ;
        ( Origin = var_origin_plain_call(Context, PredId, ArgNum)
        ; Origin = var_origin_foreign_call(Context, PredId, ArgNum)
        ),
        pred_arg_num_description(ModuleInfo, PredId, ArgNum,
            PredOrFunc, SymName, ArgNumDescPieces),
        Pieces = [quote(VarStr), words("is the")] ++
            ArgNumDescPieces ++ [words("of the"),
            p_or_f(PredOrFunc), words("call to"),
            qual_sym_name(SymName), words("here."), nl]
    ;
        Origin = var_origin_generic_call(Context, GenericCall, ArgNum),
        (
            GenericCall = higher_order(_, _, PredOrFunc,
                pred_form_arity(NumArgs), _),
            ( if ArgNum = 1u then
                Pieces = [quote(VarStr), words("is the callee of the"),
                    words("higher order"), p_or_f(PredOrFunc),
                    words("here."), nl]
            else
                ArgNumDescPieces = arg_num_description(PredOrFunc,
                    NumArgs - 1, ArgNum),
                Pieces = [quote(VarStr), words("is the")] ++
                    ArgNumDescPieces ++ [words("of the"),
                    words("higher order"), p_or_f(PredOrFunc),
                    words("call here."), nl]
            )
        ;
            GenericCall = class_method(_, _, _, PfSNA),
            PfSNA = pf_sym_name_arity(PredOrFunc, SymName,
                pred_form_arity(NumArgs)),
            ArgNumDescPieces =
                arg_num_description(PredOrFunc, NumArgs, ArgNum),
            Pieces = [quote(VarStr), words("is the")] ++
                ArgNumDescPieces ++ [words("of the"),
                p_or_f(PredOrFunc), words("call to method"),
                qual_sym_name(SymName), words("here."), nl]
        ;
            GenericCall = event_call(EventName),
            ArgNumInt = uint.cast_to_int(ArgNum),
            Pieces = [quote(VarStr), words("is the"),
                nth_fixed(ArgNumInt), words("argument"),
                words("of event"), quote(EventName), suffix("."), nl]
        ;
            GenericCall = cast(CastKind),
            ( if ArgNum = 1u then InOrOut = "input" else InOrOut = "output"),
            % I (zs) think that the other CastKinds that should appear here
            % are type coercions.
            (
                ( CastKind = unsafe_type_cast
                ; CastKind = equiv_type_cast
                ),
                Pieces = [quote(VarStr), words("is the"),
                    words(InOrOut), words("of a type cast."), nl]
            ;
                CastKind = unsafe_type_inst_cast,
                Pieces = [quote(VarStr), words("is the"),
                    words(InOrOut), words("of a type and inst cast."), nl]
            ;
                CastKind = exists_cast,
                Pieces = [quote(VarStr), words("is the"),
                    words(InOrOut), words("of an existential type cast."), nl]
            ;
                CastKind = subtype_coerce,
                Pieces = [quote(VarStr), words("is the"),
                    words(InOrOut), words("of a type coercion."), nl]
            )
        )
    ),
    Msg = msg(Context, Pieces).

:- pred pred_arg_num_description(module_info::in, pred_id::in, uint::in,
    pred_or_func::out, sym_name::out, list(format_piece)::out) is det.

pred_arg_num_description(ModuleInfo, PredId, ArgNum,
        PredOrFunc, SymName, Pieces) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_sym_name(PredInfo, SymName),
    pred_form_arity(NumArgs) = pred_info_pred_form_arity(PredInfo),
    Pieces = arg_num_description(PredOrFunc, NumArgs, ArgNum).

:- func arg_num_description(pred_or_func, int, uint) = list(format_piece).

arg_num_description(PredOrFunc, NumArgs, ArgNum) = Pieces :-
    ArgNumInt = uint.cast_to_int(ArgNum),
    ( if
        PredOrFunc = pf_function,
        ArgNumInt = NumArgs
    then
        Pieces = [words("function result")]
    else
        Pieces = [nth_fixed(ArgNumInt), words("argument")]
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.var_origins.
%---------------------------------------------------------------------------%
