%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: loop_inv.m.
% Main author: rafe.
%
% This module implements conservative loop invariant hoisting.
% The basic idea can be outlined as a transformation on functions.
% We want to convert
%
%     f(X, Y) = if p(X, Y) then g(X, Y) else f(X, h(i(X), Y))
%
% to
%
%     f(X, Y)     = if p(X, Y) then g(X, Y) else f2(X, i(X), h(i(X), Y))
%
%     f2(X, W, Y) = if p(X, Y) then g(X, Y) else f2(X, W, h(W, Y))
%
% where W, X, Y may each stand for one or more program variables.
% X stands for the loop invariant original arguments,
% W stands for the loop invariant variables that originally were not arguments,
% and Y stands for the original arguments that are not loop invariant.
%
% In the HLDS, functions are converted to predicates, hence the above
% will look like this:
%
%     f(X, Y, R) :-
%         if p(X, Y) then g(X, Y, R)
%                    else i(X, W),     h(W, Y, V),    f(X, V, R).
%
% and will be translated by the optimization into
%
%     f(X, Y, R) :-
%         if p(X, Y) then g(X, Y, R)
%                    else i(X, W),     h(W, Y, V),    f2(X, W, V, R).
%
%     f2(X, W, Y, R) :-
%         if p(X, Y) then g(X, Y, R)
%                    else h(W, Y, V),  f2(X, W, V, R).
%
% We proceed as follows:
%
% 1. Identify the invariant args to f (that is, all input args that
% are identical across all calls to f at the end of recursive paths.
% (A recursive path is a path from the start of the definition of f
% to a recursive call to f comprised entirely of model det goals,
% other than in the conditions of if-then-elses or switch
% unifications.)
%
% 2. Identify the set of invariant goals and vars in the body of f:
% - A var is invariant iff it is an invariant arg or it is the output
% of an invariant goal.
% - A goal is invariant iff
%   - it is model det,
%   - it is invoked on all recursive paths, and
%   - all of its input args are invariant vars.
%
% In the example above, X is an invariant arg, i(X, W) is an
% invariant goal, X and W are invariant vars, and
%
%     /* if */ p(X, Y), /* else */ i(X, W), h(W, Y, V), f(X, V, R)
%
% is a recursive path.
%
% At this point we construct f2, which is a copy of f taking the
% invariant vars as extra args, in which the invariant goals
% appearing on the recursive paths have been deleted, and in
% which the recursive calls to f at the end of the recursive paths
% have been replaced with calls to f2.
%
% We adjust the definition of f such that the recursive calls to f
% at the end of the recursive paths are replaced with calls to f2.
%
% NOTE that this version of the optimization does not perform
% variable renaming, so the two calls to i/1 in the code below
% will not be hoisted, because they have different output variables:
%
%     f(X, Y, R) :-
%         if      p(X, Y) then g(X, Y, R)
%         else if q(X, Y) then i(X, W1),    h1(W1, Y, V),  f(X, V, R)
%         else                 i(X, W2),    h1(W2, Y, V),  f(X, V, R)
%
% In general this means that currently the optimization will only be
% effective if there is a single recursive call.
%
% This may be the subject of a future improvement of the optimization.
% Similarly for broadening the scope of the optimization to include non
% model_det recursive paths.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.loop_inv.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.

%-----------------------------------------------------------------------------%

    % hoist_loop_invariants(PredProcId, PredInfo, !ProcInfo, !ModuleInfo)
    %
    % Analyze the procedure identified by PredProcId and, if appropriate,
    % split it into two applying the loop invariant hoisting optimization.
    %
:- pred hoist_loop_invariants(pred_proc_id::in, pred_info::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.quantification.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

hoist_loop_invariants(PredProcId, PredInfo, !ProcInfo, !ModuleInfo) :-
    ( if
        % We only want to apply this optimization to pure preds (e.g.
        % not benchmark_det_loop).
        pred_info_get_purity(PredInfo, purity_pure),

        % Next, work out whether this predicate is optimizable and
        % compute some auxiliary results along the way.

        proc_info_get_goal(!.ProcInfo, Body),
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_argmodes(!.ProcInfo, HeadVarModes),

        % Find the set of variables that are used as (partly) unique inputs
        % to calls. These variables are not safe candidates for hoisting.
        % (A variable whose initial bound inst is inferred as unique may be
        % hoistable if it is not used as a unique input to any call.)
        UniquelyUsedVars = uniquely_used_vars(!.ModuleInfo, Body),

        % Find the set of candidate goals that may be invariant
        % and the set of recursive calls involved.
        %
        % A goal must appear on all recursive paths to be a candidate.
        %
        % The recursive calls are the set of calls at the end of each
        % recursive path.
        invariant_goal_candidates_in_proc(!.ModuleInfo, PredProcId, Body,
            InvGoals0, RecCalls),

        % We can calculate the set of invariant args from the set of
        % recursive calls.
        InvArgs0 = inv_args(!.ModuleInfo, HeadVars, HeadVarModes, RecCalls),
        list.delete_elems(InvArgs0, UniquelyUsedVars, InvArgs),

        % Given the invariant args, we can calculate the set of
        % invariant goals and vars.
        inv_goals_vars(!.ModuleInfo, UniquelyUsedVars,
            InvGoals0, InvGoals1, InvArgs, InvVars1),

        % We don't want to hoist out unifications with constants (i.e.
        % constructions where the RHS has no arguments) or deconstructions
        % (it is probably cheaper to do the dereference than pass an extra
        % argument).
        %
        % We also don't want to hoist out goals that can't succeed,
        % e.g. calls to error/1, and in fact we MUST NOT hoist out
        % such goals, because if we hoisted out such goals, later
        % passes might think that the code which follows is actually
        % reachable, which may lead to internal errors because code
        % after a call to error/1 does NOT need to be determinism-correct.
        %
        % We also must not hoist impure goals.
        %
        % So here we compute the subset of InvGoals (and the corresponding
        % InvVars) that should not be hoisted.
        do_not_hoist(!.ModuleInfo, InvGoals1, DontHoistGoals, DontHoistVars),

        list.delete_elems(InvGoals1, DontHoistGoals, InvGoals),
        list.delete_elems(InvVars1, DontHoistVars, InvVars),

        % We only apply the optimization if the set of invariant goals
        % is non-empty.
        InvGoals = [_ | _]

        % NOTE!  At this point it is vital that
        % - none of the InvVars are used as (partially) unique inputs
        %   in any goals;
        % - all of the InvVars are either head vars or constructed by one of
        %   the InvGoals;
        % - all non-local vars in InvGoals are also in InvVars.
    then
        % The set of computed invariant vars is the difference between
        % the whole invariant var set and the set of invariant args.
        %
        % Some of these variables may only appear in the invariant goals,
        % and would be unused in the auxiliary procedure. Head variables may
        % become unused as well. We rely on the unused argument elimination
        % pass to remove both.
        list.delete_elems(InvVars, InvArgs, ComputedInvVars),

        % We need to calculate the initial instmap for the aux proc by applying
        % the instmap_deltas from the InvGoals to InitialInstMap.
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo,
            InitialInstMap),
        InitialAuxInstMap =
            compute_initial_aux_instmap(InvGoals, InitialInstMap),

        % Create the pred for the aux proc. This is initially a copy of the
        % in proc with the head vars extended with the list of computed
        % inv vars. The body is adjusted appropriately in the next step.
        create_aux_pred(PredProcId, HeadVars, ComputedInvVars,
            InitialAuxInstMap, AuxPredProcId, Replacement,
            AuxPredInfo, AuxProcInfo, !ModuleInfo),

        % We update the body of AuxProc by replacing adding the set of
        % computed invariant vars to the argument list, replacing invariant
        % goals in InProc with `true', and recursive calls at the end of
        % recursive paths with calls to the auxiliary procedure.
        gen_aux_proc(InvGoals, PredProcId, AuxPredProcId, Replacement, Body,
            AuxPredInfo, AuxProcInfo, !ModuleInfo),

        % We construct OutProc by replacing recursive calls to the InProc
        % at the end of recursive paths with calls to the auxiliary procedure.
        gen_out_proc(PredProcId, PredInfo, !ProcInfo, Replacement, Body,
            !ModuleInfo)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- type rec_call ==
            pair(
                hlds_goal,         % The recursive call.
                list(hlds_goal)    % The candidate invariant goal list
                                   %    for this recursive call.
            ).

:- type igc_info
    --->    igc_info(
                igc_module_info             :: module_info,

                % path_candidates is the list of accumulated invariant
                % goal candidates.
                igc_path_candidates         :: cord(hlds_goal),

                % rec_calls pairs each recursive call with the list of
                % path_candidates up to that call. We extend this list
                % whenever we identify a new recursive call.
                igc_rec_calls               :: list(rec_call)
            ).

    % invariant_goal_candidates_in_proc(PredProcId, Body, CandidateInvGoals,
    %   RecCallGoals):
    %
    % Computes (a conservative approximation to) the set of candidate
    % invariant atomic goals in Body and the set of recursive calls
    % in Body identified via PredProcId.
    %
:- pred invariant_goal_candidates_in_proc(module_info::in, pred_proc_id::in,
    hlds_goal::in, list(hlds_goal)::out, list(hlds_goal)::out) is det.

invariant_goal_candidates_in_proc(ModuleInfo, PredProcId, Body,
        CandidateInvGoals, RecCallGoals) :-
    GoalCandidates0 = igc_info(ModuleInfo, cord.empty, []),
    invariant_goal_candidates_in_goal(PredProcId, Body,
        GoalCandidates0, GoalCandidates),
    GoalCandidates = igc_info(_, _, RecCalls),
    assoc_list.keys_and_values(RecCalls, RecCallGoals, CandidateInvGoalsList),
    CandidateInvGoals = intersect_candidate_inv_goals(CandidateInvGoalsList).

%-----------------------------------------------------------------------------%

:- pred invariant_goal_candidates_in_goal(pred_proc_id::in, hlds_goal::in,
    igc_info::in, igc_info::out) is det.

invariant_goal_candidates_in_goal(PPId, Goal, !IGCs) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( if proc(PredId, ProcId) = PPId then
            add_recursive_call(Goal, !IGCs)
        else
            invariant_goal_candidates_handle_primitive_goal(Goal, !IGCs)
        )
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        invariant_goal_candidates_handle_primitive_goal(Goal, !IGCs)
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        (
            ConjType = plain_conj,
            invariant_goal_candidates_in_plain_conj(PPId, Conjuncts, !IGCs)
        ;
            ConjType = parallel_conj,
            invariant_goal_candidates_in_parallel_conj(PPId, Conjuncts, !IGCs)
        )
    ;
        GoalExpr = disj(Disjuncts),
        invariant_goal_candidates_in_disj(PPId, Disjuncts, !IGCs)
    ;
        GoalExpr = switch(_, _, Cases),
        invariant_goal_candidates_in_switch(PPId, Cases, !IGCs)
    ;
        GoalExpr = negation(SubGoal),
        invariant_goal_candidates_keeping_path_candidates(PPId, SubGoal, !IGCs)
    ;
        GoalExpr = scope(_Reason, SubGoal),
        % XXX We should specialize the handling of from_ground_term_construct
        % scopes here.
        invariant_goal_candidates_keeping_path_candidates(PPId, SubGoal, !IGCs)
    ;
        GoalExpr = if_then_else(_XVs, Cond, Then, Else),
        PathCandidates0 = !.IGCs ^ igc_path_candidates,
        invariant_goal_candidates_in_goal(PPId, Cond, !IGCs),
        invariant_goal_candidates_in_goal(PPId, Then, !IGCs),
        !IGCs ^ igc_path_candidates := PathCandidates0,
        invariant_goal_candidates_keeping_path_candidates(PPId, Else, !IGCs)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred invariant_goal_candidates_keeping_path_candidates(pred_proc_id::in,
    hlds_goal::in, igc_info::in, igc_info::out) is det.

invariant_goal_candidates_keeping_path_candidates(PPId, Goal, !IGCs) :-
    PathCandidates0 = !.IGCs ^ igc_path_candidates,
    invariant_goal_candidates_in_goal(PPId, Goal, !IGCs),
    !IGCs ^ igc_path_candidates := PathCandidates0.

%-----------------------------------------------------------------------------%

:- pred invariant_goal_candidates_in_plain_conj(pred_proc_id::in,
    list(hlds_goal)::in, igc_info::in, igc_info::out) is det.

invariant_goal_candidates_in_plain_conj(_, [], !IGCs).
invariant_goal_candidates_in_plain_conj(PPId, [Goal | Goals], !IGCs) :-
    invariant_goal_candidates_in_goal(PPId, Goal, !IGCs),
    invariant_goal_candidates_in_plain_conj(PPId, Goals, !IGCs).

:- pred invariant_goal_candidates_in_parallel_conj(pred_proc_id::in,
    list(hlds_goal)::in, igc_info::in, igc_info::out) is det.

invariant_goal_candidates_in_parallel_conj(_, [], !IGCs).
invariant_goal_candidates_in_parallel_conj(PPId, [Goal | Goals], !IGCs) :-
    invariant_goal_candidates_keeping_path_candidates(PPId, Goal, !IGCs),
    invariant_goal_candidates_in_parallel_conj(PPId, Goals, !IGCs).

:- pred invariant_goal_candidates_in_disj(pred_proc_id::in,
    list(hlds_goal)::in, igc_info::in, igc_info::out) is det.

invariant_goal_candidates_in_disj(_, [], !IGCs).
invariant_goal_candidates_in_disj(PPId, [Goal | Goals], !IGCs) :-
    invariant_goal_candidates_keeping_path_candidates(PPId, Goal, !IGCs),
    invariant_goal_candidates_in_disj(PPId, Goals, !IGCs).

:- pred invariant_goal_candidates_in_switch(pred_proc_id::in,
    list(case)::in, igc_info::in, igc_info::out) is det.

invariant_goal_candidates_in_switch(_, [], !IGCs).
invariant_goal_candidates_in_switch(PPId, [Case | Cases], !IGCs) :-
    Case = case(_, _, Goal),
    invariant_goal_candidates_keeping_path_candidates(PPId, Goal, !IGCs),
    invariant_goal_candidates_in_switch(PPId, Cases, !IGCs).

%-----------------------------------------------------------------------------%

:- pred add_recursive_call(hlds_goal::in,
    igc_info::in, igc_info::out) is det.

add_recursive_call(Goal, !IGCs) :-
    RecCall = Goal - cord.list(!.IGCs ^ igc_path_candidates),
    !IGCs ^ igc_rec_calls := [RecCall | !.IGCs ^ igc_rec_calls].

%-----------------------------------------------------------------------------%

    % NOTE: We could hoist semipure goals that have no preceeding impure goals,
    % but that is a very low-level optimization that is not entirely trivial
    % to implement.
    %
:- pred invariant_goal_candidates_handle_primitive_goal(hlds_goal::in,
    igc_info::in, igc_info::out) is det.

invariant_goal_candidates_handle_primitive_goal(Goal, !IGCs) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    ( if
        Detism = hlds_goal.goal_info_get_determinism(GoalInfo),
        code_model.determinism_to_code_model(Detism, CodeModel),
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),

        goal_info_get_purity(GoalInfo) = purity_pure,

        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_to_assoc_list(InstMapDelta, InstMapDeltaPairs),
        ModuleInfo = !.IGCs ^ igc_module_info,
        all_instmap_deltas_are_ground(ModuleInfo, InstMapDeltaPairs)
    then
        !IGCs ^ igc_path_candidates :=
            cord.snoc(!.IGCs ^ igc_path_candidates, Goal)
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred all_instmap_deltas_are_ground(module_info::in,
    assoc_list(prog_var, mer_inst)::in) is semidet.

all_instmap_deltas_are_ground(_, []).
all_instmap_deltas_are_ground(ModuleInfo, [_Var - Inst | VarInsts]) :-
    inst_is_ground(ModuleInfo, Inst),
    all_instmap_deltas_are_ground(ModuleInfo, VarInsts).

%-----------------------------------------------------------------------------%

:- func intersect_candidate_inv_goals(list(list(hlds_goal))) = list(hlds_goal).

intersect_candidate_inv_goals([]) = [].
intersect_candidate_inv_goals([Goals | Goalss]) =
    list.filter(common_goal(Goalss), Goals).

%-----------------------------------------------------------------------------%

:- pred common_goal(list(list(hlds_goal))::in, hlds_goal::in) is semidet.

common_goal(Goalss, Goal) :-
    all [Gs] (
        list.member(Gs, Goalss)
    =>
        (
            list.member(G, Gs),
            equivalent_goals(G, Goal)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred equivalent_goals(hlds_goal::in, hlds_goal::in) is semidet.

equivalent_goals(hlds_goal(GoalExprX, _), hlds_goal(GoalExprY, _)) :-
    (
        GoalExprX = GoalExprY
    ;
        GoalExprX =
            plain_call(PredId, ProcId, Args, _BuiltinX, _ContextX, _SymNameX),
        GoalExprY =
            plain_call(PredId, ProcId, Args, _BuiltinY, _ContextY, _SymNameY)
    ).

%-----------------------------------------------------------------------------%

:- func inv_args(module_info, list(prog_var), list(mer_mode), list(hlds_goal))
    = list(prog_var).

inv_args(ModuleInfo, HeadVars, HeadVarModes, RecCalls) = InvArgs :-
    MaybeInvArgs0 =
        list.map_corresponding(arg_to_maybe_inv_arg(ModuleInfo),
            HeadVars, HeadVarModes),
    MaybeInvArgs =
        list.foldl(refine_candidate_inv_args, RecCalls, MaybeInvArgs0),
    list.filter_map(maybe_is_yes, MaybeInvArgs, InvArgs).

%-----------------------------------------------------------------------------%

    % Maps an Arg in HeadVars to `yes(Arg)' if Arg is an input,
    % and to `no' otherwise.
    %
:- func arg_to_maybe_inv_arg(module_info, prog_var, mer_mode)
    = maybe(prog_var).

arg_to_maybe_inv_arg(ModuleInfo, Arg, Mode) =
    ( if is_input_arg(ModuleInfo, Arg, Mode, InvArg) then
        yes(InvArg)
    else
        no
    ).

%-----------------------------------------------------------------------------%

:- func refine_candidate_inv_args(hlds_goal, list(maybe(prog_var))) =
    list(maybe(prog_var)).

refine_candidate_inv_args(hlds_goal(RecCall, _RecCallInfo), MaybeInvArgs) =
    ( if RecCall = plain_call(_, _, CallArgs, _, _, _) then
        list.map_corresponding(refine_candidate_inv_args_2,
            MaybeInvArgs, CallArgs)
    else
        unexpected($pred, "non call/6 found in argument 1")
    ).

:- func refine_candidate_inv_args_2(maybe(prog_var), prog_var) =
    maybe(prog_var).

refine_candidate_inv_args_2(no, _) = no.
refine_candidate_inv_args_2(yes(X), Y) = ( if X = Y then yes(X) else no ).

%-----------------------------------------------------------------------------%

    % A goal is invariant if all its input args are invariant.
    % The outputs of an invariant goal are also invariant.
    %
    % Since mode reordering has already been applied at this point,
    % we know that if goal A precedes goal B in the candidate list,
    % goal A will not depend upon the results of goal B (although B
    % may depend on A).
    %
    % The list returned will not contain duplicate goals judged
    % to be the same by equivalent_goals/2.
    %
    % We do not hoist goals with unique outputs that are elsewhere
    % used as unique inputs since the user may clobber the variable
    % in question.
    %
:- pred inv_goals_vars(module_info::in, list(prog_var)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(prog_var)::in, list(prog_var)::out) is det.

inv_goals_vars(ModuleInfo, UniquelyUsedVars,
        InvGoals0, InvGoals, InvVars0, InvVars) :-
    list.foldl2(inv_goals_vars_2(ModuleInfo, UniquelyUsedVars),
        InvGoals0, [], InvGoals, InvVars0,InvVars).

%-----------------------------------------------------------------------------%

:- pred inv_goals_vars_2(module_info::in, list(prog_var)::in, hlds_goal::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(prog_var)::in, list(prog_var)::out) is det.

inv_goals_vars_2(ModuleInfo, UUVs, Goal, IGs0, IGs, IVs0, IVs) :-
    ( if
        not invariant_goal(IGs0, Goal),
        not has_uniquely_used_arg(UUVs, Goal),
        input_args_are_invariant(ModuleInfo, Goal, IVs0)
    then
        IGs = [Goal | IGs0],
        add_outputs(ModuleInfo, UUVs, Goal, IVs0, IVs)
    else
        IGs = IGs0,
        IVs = IVs0
    ).

%-----------------------------------------------------------------------------%

:- pred has_uniquely_used_arg(list(prog_var)::in, hlds_goal::in) is semidet.

has_uniquely_used_arg(UUVs, hlds_goal(_GoalExpr, GoalInfo)) :-
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    list.member(UUV, UUVs),
    set_of_var.member(NonLocals, UUV).

%-----------------------------------------------------------------------------%

:- pred invariant_goal(list(hlds_goal)::in, hlds_goal::in) is semidet.

invariant_goal(InvariantGoals, Goal) :-
    list.member(InvariantGoal, InvariantGoals),
    equivalent_goals(InvariantGoal, Goal).

%-----------------------------------------------------------------------------%

:- pred input_args_are_invariant(module_info::in, hlds_goal::in,
    list(prog_var)::in) is semidet.

input_args_are_invariant(ModuleInfo, Goal, InvVars) :-
    Inputs = goal_inputs(ModuleInfo, Goal),
    all [V] (
        list.member(V, Inputs)
    =>
        list.member(V, InvVars)
    ).

%-----------------------------------------------------------------------------%

:- pred do_not_hoist(module_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out, list(prog_var)::out) is det.

do_not_hoist(ModuleInfo, InvGoals, DontHoistGoals, DontHoistVars) :-
    list.foldl2(do_not_hoist_2(ModuleInfo), InvGoals,
        [], DontHoistGoals, [], DontHoistVars).

:- pred do_not_hoist_2(module_info::in, hlds_goal::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    list(prog_var)::in, list(prog_var)::out) is det.

do_not_hoist_2(ModuleInfo, Goal, !DHGs, !DHVs) :-
    ( if
        ( is_const_construction(Goal)
        ; is_deconstruction(Goal)
        ; is_impure_goal(Goal)
        ; cannot_succeed(Goal)
        ; call_has_inst_any(ModuleInfo, Goal)
        )
    then
        list.cons(Goal, !DHGs),
        add_outputs(ModuleInfo, [], Goal, !DHVs)
    else
        true
    ).

%-----------------------------------------------------------------------------%

    % A constant construction is a construction unification with no arguments,
    % or one which is constructed from a statically initialized constant.
    %
:- pred is_const_construction(hlds_goal::in) is semidet.

is_const_construction(hlds_goal(GoalExpr, _GoalInfo)) :-
    Construction = GoalExpr ^ unify_kind,
    ( Construction ^ construct_args = []
    ; Construction ^ construct_how = construct_statically(_)
    ).

%-----------------------------------------------------------------------------%

:- pred is_deconstruction(hlds_goal::in) is semidet.

is_deconstruction(hlds_goal(GoalExpr, _GoalInfo)) :-
    GoalExpr ^ unify_kind = deconstruct(_, _, _, _, _, _).

%-----------------------------------------------------------------------------%

:- pred is_impure_goal(hlds_goal::in) is semidet.

is_impure_goal(Goal) :-
    goal_get_purity(Goal) = purity_impure.

%-----------------------------------------------------------------------------%

:- pred cannot_succeed(hlds_goal::in) is semidet.

cannot_succeed(hlds_goal(_GoalExpr, GoalInfo)) :-
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, _CanFail, MaxSolns),
    MaxSolns = at_most_zero.

%-----------------------------------------------------------------------------%

    % Succeeds if any of the components of the insts of the modes of a
    % (generic) call is inst any.
    %
:- pred call_has_inst_any(module_info::in, hlds_goal::in) is semidet.

call_has_inst_any(ModuleInfo, Goal) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = generic_call(_, _, Modes, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        Modes = argmodes(ModuleInfo, PredId, ProcId)
    ),
    some [Mode] (
        list.member(Mode, Modes),
        mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
        (
            inst_contains_any(ModuleInfo, InitialInst)
        ;
            inst_contains_any(ModuleInfo, FinalInst)
        )
    ).

%-----------------------------------------------------------------------------%

:- pred add_outputs(module_info::in, list(prog_var)::in, hlds_goal::in,
    list(prog_var)::in, list(prog_var)::out) is det.

add_outputs(ModuleInfo, UUVs, Goal, !InvVars) :-
    list.foldl(add_output(UUVs), goal_outputs(ModuleInfo, Goal), !InvVars).

:- pred add_output(list(prog_var)::in, prog_var::in,
    list(prog_var)::in, list(prog_var)::out) is det.

add_output(UniquelyUsedVars, X, !InvVars) :-
    ( if
        not list.member(X, !.InvVars),
        not list.member(X, UniquelyUsedVars)
    then
        !:InvVars = [X | !.InvVars]
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- func compute_initial_aux_instmap(list(hlds_goal), instmap) = instmap.

compute_initial_aux_instmap(Gs, IM) = list.foldl(ApplyGoalInstMap, Gs, IM) :-
    ApplyGoalInstMap =
        ( func(hlds_goal(_GoalExpr, GoalInfo), IM0) = IM1 :-
            IMD = goal_info_get_instmap_delta(GoalInfo),
            apply_instmap_delta(IMD, IM0, IM1)
        ).

%-----------------------------------------------------------------------------%

:- pred create_aux_pred(pred_proc_id::in,
    list(prog_var)::in, list(prog_var)::in, instmap::in, pred_proc_id::out,
    hlds_goal::out, pred_info::out, proc_info::out,
    module_info::in, module_info::out) is det.

create_aux_pred(PredProcId, HeadVars, ComputedInvArgs,
        InitialAuxInstMap, AuxPredProcId, Replacement,
        AuxPredInfo, AuxProcInfo, ModuleInfo0, ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),

    AuxHeadVars = HeadVars ++ ComputedInvArgs,

    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
        PredInfo, ProcInfo),

    proc_info_get_goal(ProcInfo, Goal @ hlds_goal(_GoalExpr, GoalInfo)),
    pred_info_get_typevarset(PredInfo, TVarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    pred_info_get_class_context(PredInfo, ClassContext),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_origin(PredInfo, OrigOrigin),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    pred_info_get_var_name_remap(PredInfo, VarNameRemap),

    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Context = goal_info_get_context(GoalInfo),
    term.context_line(Context, Line),
    ( if Line = 0 then
        % Use the predicate number to distinguish between similarly named
        % generated predicates, e.g. special predicates.
        Counter = pred_id_to_int(PredId)
    else
        Counter = 1
    ),
    make_pred_name_with_context(PredModule, "loop_inv",
        PredOrFunc, PredName, Line, Counter, AuxPredSymName0),
    hlds_pred.proc_id_to_int(ProcId, ProcNo),
    Suffix = string.format("_%d", [i(ProcNo)]),
    add_sym_name_suffix(AuxPredSymName0, Suffix, AuxPredSymName),

    Origin = origin_transformed(transform_loop_invariant(ProcNo),
        OrigOrigin, PredId),
    hlds_pred.define_new_pred(
        Origin,         % in    - The origin of this new predicate
        Goal,           % in    - The goal for the new aux proc.
        Replacement,    % out   - How we can call the new aux proc.
        AuxHeadVars,    % in    - The args for the new aux proc.
        _ExtraArgs,     % out   - Extra args prepended to Args for typeinfo
                        %           liveness purposes.
        InitialAuxInstMap,
                        % in    - The initial instmap for the new aux proc.
        AuxPredSymName, % in    - The name of the new aux proc.
        TVarSet,        % in    - ???
        VarTypes,       % in    - The var -> type mapping for the new aux proc.
        ClassContext,   % in    - Typeclass constraints on the new aux proc.
        RttiVarMaps,    % in    - type_info and typeclass_info locations.
        VarSet,         % in    - ???
        InstVarSet,     % in    - ???
        Markers,        % in    - Markers for the new aux proc.
        address_is_not_taken,
                        % in    - The address of the new aux proc is not taken.
        HasParallelConj, % in
        VarNameRemap,   % in
        ModuleInfo0,
        ModuleInfo,
        AuxPredProcId   % out   - The pred_proc_id for the new aux proc.
    ),

    % Note on Replacement:
    % - we change the call args as necessary in gen_aux_call;
    % - we handle the changes to nonlocals by requantifying
    %   over the entire goal after we've transformed it.

    AuxPredProcId = proc(AuxPredId, AuxProcId),
    module_info_pred_proc_info(ModuleInfo, AuxPredId, AuxProcId, AuxPredInfo,
        AuxProcInfo).

%-----------------------------------------------------------------------------%

:- type gen_aux_proc_info
    --->    gen_aux_proc_info(
                gapi_module_info            :: module_info,
                gapi_inv_goals              :: list(hlds_goal),
                gapi_pred_proc_id           :: pred_proc_id,
                gapi_replament_goal         :: hlds_goal
            ).

    % Replace the invariant goals in the original Body
    % with just `true' in the new AuxBody.
    %
:- pred gen_aux_proc(list(hlds_goal)::in, pred_proc_id::in, pred_proc_id::in,
    hlds_goal::in, hlds_goal::in, pred_info::in, proc_info::in,
    module_info::in, module_info::out) is det.

gen_aux_proc(InvGoals, PredProcId, AuxPredProcId, Replacement, Body,
        AuxPredInfo, !.AuxProcInfo, !ModuleInfo) :-
    % Compute the aux proc body.
    GapInfo = gen_aux_proc_info(!.ModuleInfo, InvGoals, PredProcId,
        Replacement),
    gen_aux_proc_goal(GapInfo, Body, AuxBody),

    % Put the new proc body and instmap into the module_info.
    AuxPredProcId = proc(AuxPredId, AuxProcId),
    hlds_pred.proc_info_set_goal(AuxBody, !AuxProcInfo),

    requantify_proc_general(ordinary_nonlocals_no_lambda, !AuxProcInfo),
    recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
        !AuxProcInfo, !ModuleInfo),

    module_info_set_pred_proc_info(AuxPredId, AuxProcId,
        AuxPredInfo, !.AuxProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred gen_aux_proc_goal(gen_aux_proc_info::in, hlds_goal::in, hlds_goal::out)
    is det.

gen_aux_proc_goal(Info, Goal, AuxGoal) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = plain_call(PredId, ProcId, _,_,_,_),
        ( if proc(PredId, ProcId) = Info ^ gapi_pred_proc_id then
            gen_aux_call(Info ^ gapi_replament_goal, Goal, AuxGoal)
        else
            gen_aux_proc_handle_non_recursive_call(Info, Goal, AuxGoal)
        )
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        gen_aux_proc_handle_non_recursive_call(Info, Goal, AuxGoal)
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        list.map(gen_aux_proc_goal(Info), Conjuncts, AuxConjuncts),
        AuxGoalExpr = conj(ConjType, AuxConjuncts),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = disj(Disjuncts),
        list.map(gen_aux_proc_goal(Info), Disjuncts, AuxDisjuncts),
        AuxGoalExpr = disj(AuxDisjuncts),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = switch(Var, CanFail, Cases),
        list.map(gen_aux_proc_case(Info), Cases, AuxCases),
        AuxGoalExpr = switch(Var, CanFail, AuxCases),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = negation(SubGoal),
        gen_aux_proc_goal(Info, SubGoal, AuxSubGoal),
        AuxGoalExpr = negation(AuxSubGoal),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        % XXX We should consider special casing the handling of
        % from_ground_term_construct scopes.
        gen_aux_proc_goal(Info, SubGoal, AuxSubGoal),
        AuxGoalExpr = scope(Reason, AuxSubGoal),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        gen_aux_proc_goal(Info, Cond, AuxCond),
        gen_aux_proc_goal(Info, Then, AuxThen),
        gen_aux_proc_goal(Info, Else, AuxElse),
        AuxGoalExpr = if_then_else(Vars, AuxCond, AuxThen, AuxElse),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred gen_aux_proc_case(gen_aux_proc_info::in, case::in, case::out) is det.

gen_aux_proc_case(Info, Case, AuxCase) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    gen_aux_proc_goal(Info, Goal, AuxGoal),
    AuxCase = case(MainConsId, OtherConsIds, AuxGoal).

%-----------------------------------------------------------------------------%

:- pred gen_aux_proc_handle_non_recursive_call(gen_aux_proc_info::in,
    hlds_goal::in, hlds_goal::out) is det.

gen_aux_proc_handle_non_recursive_call(Info, Goal, AuxGoal) :-
    ( if invariant_goal(Info ^ gapi_inv_goals, Goal) then
        AuxGoal = true_goal
    else
        AuxGoal = Goal
    ).

%-----------------------------------------------------------------------------%

    % We construct OutProc by replacing recursive calls to the InProc at the
    % end of recursive paths with calls to the auxiliary procedure.
    %
:- pred gen_out_proc(pred_proc_id::in, pred_info::in,
    proc_info::in, proc_info::out, hlds_goal::in, hlds_goal::in,
    module_info::in, module_info::out) is det.

gen_out_proc(PredProcId, PredInfo0, ProcInfo0, ProcInfo, Replacement, Body0,
        !ModuleInfo) :-
    % Compute the new procedure body.
    gen_out_proc_goal(PredProcId, Replacement, Body0, Body),

    % Put the new procedure body into the module_info.
    PredProcId = proc(PredId, ProcId),

    proc_info_get_varset(ProcInfo0, VarSet),
    proc_info_get_vartypes(ProcInfo0, VarTypes),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    proc_info_get_rtti_varmaps(ProcInfo0, RttiVarMaps),

    proc_info_set_body(VarSet, VarTypes, HeadVars, Body,
        RttiVarMaps, ProcInfo0, ProcInfo1),

    requantify_proc_general(ordinary_nonlocals_no_lambda,
        ProcInfo1, ProcInfo2),
    recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
        ProcInfo2, ProcInfo, !ModuleInfo),

    module_info_set_pred_proc_info(PredId, ProcId,
        PredInfo0, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % gen_out_proc_goal(PredProcId, Replacement, Goal, AuxGoal):
    %
    % AuxGoal is Goal with calls to PredProcId replaced with Replacement.
    %
:- pred gen_out_proc_goal(pred_proc_id::in, hlds_goal::in,
    hlds_goal::in, hlds_goal::out) is det.

gen_out_proc_goal(PPId, Replacement, Goal, AuxGoal) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = plain_call(PredId, ProcId, _, _, _, _),
        ( if proc(PredId, ProcId) = PPId then
            gen_aux_call(Replacement, Goal, AuxGoal)
        else
            AuxGoal = Goal
        )
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        AuxGoal = Goal
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        list.map(gen_out_proc_goal(PPId, Replacement),
            Conjuncts, AuxConjuncts),
        AuxGoalExpr = conj(ConjType, AuxConjuncts),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = disj(Disjuncts),
        list.map(gen_out_proc_goal(PPId, Replacement),
            Disjuncts, AuxDisjuncts),
        AuxGoalExpr = disj(AuxDisjuncts),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = switch(Var, CanFail, Cases),
        list.map(gen_out_proc_case(PPId, Replacement), Cases, AuxCases),
        AuxGoalExpr = switch(Var, CanFail, AuxCases),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = negation(SubGoal),
        gen_out_proc_goal(PPId, Replacement, SubGoal, AuxSubGoal),
        AuxGoalExpr = negation(AuxSubGoal),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = scope(Reason, SubGoal),
        % XXX We should consider special casing the handling of
        % from_ground_term_construct scopes.
        gen_out_proc_goal(PPId, Replacement, SubGoal, AuxSubGoal),
        AuxGoalExpr = scope(Reason, AuxSubGoal),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        gen_out_proc_goal(PPId, Replacement, Cond, AuxCond),
        gen_out_proc_goal(PPId, Replacement, Then, AuxThen),
        gen_out_proc_goal(PPId, Replacement, Else, AuxElse),
        AuxGoalExpr = if_then_else(Vars, AuxCond, AuxThen, AuxElse),
        AuxGoal = hlds_goal(AuxGoalExpr, GoalInfo)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred gen_out_proc_case(pred_proc_id::in, hlds_goal::in, case::in, case::out)
    is det.

gen_out_proc_case(PPId, Replacement, Case, AuxCase) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    gen_out_proc_goal(PPId, Replacement, Goal, AuxGoal),
    AuxCase = case(MainConsId, OtherConsIds, AuxGoal).

%-----------------------------------------------------------------------------%

:- pred gen_aux_call(hlds_goal::in, hlds_goal::in, hlds_goal::out) is det.

gen_aux_call(Replacement, CallGoal, AuxCallGoal) :-
    Replacement = hlds_goal(ReplacementExpr, _ReplacementInfo0),
    CallGoal = hlds_goal(CallExpr, CallInfo),
    ( if
        ReplacementArgs0 = ReplacementExpr ^ call_args,
        Args0 = CallExpr ^ call_args,
        replace_initial_args(Args0, ReplacementArgs0, Args),
        AuxCallGoalExpr = ReplacementExpr ^ call_args := Args

        % Note that one might expect instmap_delta to change, however the
        % invariant arguments are just that -invariant- hence their insts
        % are not changed by the recursive call and there is no need
        % to adjust the instmap_delta. All other fields are correct for
        % CallInfo.
    then
        AuxCallGoal = hlds_goal(AuxCallGoalExpr, CallInfo)
    else
        unexpected($pred, "args not both ordinary calls")
    ).

    % replace_initial_args(Rs, Xs0, Xs):
    %
    % If Rs has N elements, then replace the first N elements of Xs0 with Rs.
    %
:- pred replace_initial_args(list(T)::in, list(T)::in, list(T)::out) is det.

replace_initial_args([], Xs, Xs).
replace_initial_args([R | Rs], [_ | Xs0], [R | Xs]) :-
    replace_initial_args(Rs, Xs0, Xs).
replace_initial_args([_ | _], [], _) :-
    unexpected($pred, "first arg longer than second").

%-----------------------------------------------------------------------------%

    % This predicate computes the set of variables that are used as (partly)
    % unique inputs to goals. This information is needed because unique local
    % values for which uniqueness is important cannot be hoisted, although
    % those for which uniqueness is inferred, but not important, can be
    % hoisted.
    %
    % TODO: get this to handle unification properly. See the XXX below.
    %
:- func uniquely_used_vars(module_info, hlds_goal) = list(prog_var).

uniquely_used_vars(ModuleInfo, Goal) =
    list.sort_and_remove_dups(used_vars(ModuleInfo, Goal)).

%-----------------------------------------------------------------------------%

:- func used_vars(module_info, hlds_goal) = list(prog_var).

used_vars(ModuleInfo, Goal) = UsedVars :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = plain_call(PredId, ProcId, Args, _, _, _),
        list.filter_map_corresponding(uniquely_used_args(ModuleInfo),
            Args, argmodes(ModuleInfo, PredId, ProcId), UsedVars)
    ;
        GoalExpr = generic_call(_, Args, Modes, _, _),
        list.filter_map_corresponding(uniquely_used_args(ModuleInfo),
            Args, Modes, UsedVars)
    ;
        GoalExpr = call_foreign_proc(_, PredId, ProcId,
            ForeignArgs, ExtraForeignArgs, _, _),
        % XXX `Extras' should be empty for pure calls. We cannot apply LIO
        % to non-pure goals so we shouldn't need to consider `Extras'.
        % However, we currently don't deal with the situation where we may be
        % trying to apply LIO to a non-pure goal until *after* we have called
        % this predicate, so `Extras' may not be empty. As a work-around,
        % we just add any variables in `Extras' to the set of variables
        % that cannot be hoisted.
        list.filter_map_corresponding(uniquely_used_args(ModuleInfo),
            list.map(foreign_arg_var, ForeignArgs),
            argmodes(ModuleInfo, PredId, ProcId), UsedArgVars),
        UsedExtraArgVars = list.map(foreign_arg_var, ExtraForeignArgs),
        UsedVars = UsedArgVars ++ UsedExtraArgVars
    ;
        GoalExpr = unify(_LHS, _RHS, _UMode, _UKind, _),
        % XXX This is very conservative!
        UsedVars = []
    ;
        GoalExpr = conj(_, Conjuncts),
        UsedVars = list.condense(list.map(used_vars(ModuleInfo), Conjuncts))
    ;
        GoalExpr = disj(Disjuncts),
        UsedVars = list.condense(list.map(used_vars(ModuleInfo), Disjuncts))
    ;
        GoalExpr = switch(_, _, Cases),
        UsedVars = list.condense(list.map(used_vars(ModuleInfo),
            case_goals(Cases)))
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        UsedVars = used_vars(ModuleInfo, Cond) ++
            used_vars(ModuleInfo, Then) ++ used_vars(ModuleInfo, Else)
    ;
        GoalExpr = negation(SubGoal),
        UsedVars = used_vars(ModuleInfo, SubGoal)
    ;
        GoalExpr = scope(_Reason, SubGoal),
        % XXX We should consider special casing the handling of
        % from_ground_term_construct scopes.
        UsedVars = used_vars(ModuleInfo, SubGoal)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- func case_goals(list(case)) = list(hlds_goal).

case_goals(Cases) =
    list.map(func(case(_MainConsId, _OtherConsIds, Goal)) = Goal, Cases).

%-----------------------------------------------------------------------------%

:- pred uniquely_used_args(module_info::in, prog_var::in, mer_mode::in,
    prog_var::out) is semidet.

uniquely_used_args(ModuleInfo, X, M, X) :-
    mode_get_insts(ModuleInfo, M, InInst, _OutInst),
    not inst_is_not_partly_unique(ModuleInfo, InInst).

%-----------------------------------------------------------------------------%

:- func argmodes(module_info, pred_id, proc_id) = list(mer_mode).

argmodes(ModuleInfo, PredId, ProcId) = ArgModes :-
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes).

%-----------------------------------------------------------------------------%

    % Find the list of vars for a goal that are free before the call.
    % This only applies to calls and unifications.
    %
:- func goal_inputs(module_info, hlds_goal) = list(prog_var).

goal_inputs(ModuleInfo, Goal) = Inputs :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = plain_call(PredId, ProcId, Args, _, _, _),
        list.filter_map_corresponding(is_input_arg(ModuleInfo),
            Args, argmodes(ModuleInfo, PredId, ProcId), Inputs)
    ;
        GoalExpr = generic_call(GenericCall, Args, ArgModes, _, _),
        generic_call_vars(GenericCall, GenericCallVars),
        list.filter_map_corresponding(is_input_arg(ModuleInfo),
            Args, ArgModes, Inputs0),
        Inputs = GenericCallVars ++ Inputs0
    ;
        GoalExpr = call_foreign_proc(_, PredId, ProcId, ForeignArgs, _, _, _),
        list.filter_map_corresponding(is_input_arg(ModuleInfo),
            list.map(foreign_arg_var, ForeignArgs),
            argmodes(ModuleInfo, PredId, ProcId), Inputs)
    ;
        GoalExpr = unify(LHS, UnifyRHS, _, Kind, _),
        (
            % The LHS is always an output var in constructions.
            Kind = construct(_, _, RHSArgs, ArgModes, _, _, _),
            list.filter_map_corresponding(is_input_arg(ModuleInfo),
                RHSArgs, list.map(unify_mode_to_rhs_mode, ArgModes),
                Inputs)
        ;
            % The LHS is always in input var in deconstructions.
            Kind = deconstruct(_, _, RHSArgs, ArgModes, _, _),
            list.filter_map_corresponding(is_input_arg(ModuleInfo),
                RHSArgs, list.map(unify_mode_to_rhs_mode, ArgModes),
                RHSInputs),
            Inputs = [LHS | RHSInputs]
        ;
            % The RHS is the only input in an assignment.
            Kind = assign(_, RHS),
            Inputs = [RHS]
        ;
            % Both sides of a simple test are inputs.
            Kind = simple_test(_, RHS),
            Inputs = [LHS, RHS]
        ;
            % Both sides of a complicated unification are inputs.
            Kind = complicated_unify(_, _, _),
            (
                UnifyRHS = rhs_var(RHS),
                Inputs = [LHS, RHS]
            ;
                UnifyRHS = rhs_functor(_, _, _),
                Inputs = [LHS]
            ;
                UnifyRHS = rhs_lambda_goal(_, _, _, _, _, _, _, _),
                % These should have been expanded out by now.
                unexpected($pred, "lambda goal")
            )
        )
    ;
        ( GoalExpr = conj(_, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ; GoalExpr = shorthand(_)
        ),
        unexpected($pred, "compound goal")
    ).

    % Find the list of vars for a goal that are free before the call and bound
    % afterwards. This only applies to calls and unifications.
    %
:- func goal_outputs(module_info, hlds_goal) = list(prog_var).

goal_outputs(ModuleInfo, Goal) = Outputs :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = plain_call(PredId, ProcId, Args, _, _, _),
        list.filter_map_corresponding(is_output_arg(ModuleInfo),
            Args, argmodes(ModuleInfo, PredId, ProcId), Outputs)
    ;
        GoalExpr = generic_call(_, Args, ArgModes, _, _),
        list.filter_map_corresponding(is_output_arg(ModuleInfo),
            Args, ArgModes, Outputs)
    ;
        GoalExpr = call_foreign_proc(_, PredId, ProcId, ForeignArgs, _, _, _),
        list.filter_map_corresponding(is_output_arg(ModuleInfo),
            list.map(foreign_arg_var, ForeignArgs),
            argmodes(ModuleInfo, PredId, ProcId), Outputs)
    ;
        GoalExpr = unify(LHS, _RHS, _, Kind, _),
        (
            % The LHS is the only output in a construction.
            Kind = construct(_, _, _, _, _, _, _),
            Outputs = [LHS]
        ;
            % The LHS is always in input in deconstructions.
            Kind = deconstruct(_, _, RHSArgs, ArgModes, _, _),
            list.filter_map_corresponding(is_output_arg(ModuleInfo),
                RHSArgs, list.map(unify_mode_to_rhs_mode, ArgModes),
                Outputs)
        ;
            % The LHS is the only output in an assignment.
            Kind = assign(_, _),
            Outputs = [LHS]
        ;
            % Both sides of a simple test are inputs.
            Kind = simple_test(_, _),
            Outputs = []
        ;
            % Both sides of a complicated unification are inputs.
            Kind = complicated_unify(_, _, _),
            Outputs = []
        )
    ;
        ( GoalExpr = conj(_, _)
        ; GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ; GoalExpr = shorthand(_)
        ),
        unexpected($pred, "compound goal")
    ).

    % An input arg is one whose initial inst is not free.
    %
:- pred is_input_arg(module_info::in, prog_var::in, mer_mode::in,
    prog_var::out) is semidet.

is_input_arg(ModuleInfo, Var, Mode, Var) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, _FinalInst),
    not inst_is_free(ModuleInfo, InitInst).

    % An output arg is one whose initial inst is free and whose final inst
    % is ground.
    %
:- pred is_output_arg(module_info::in, prog_var::in, mer_mode::in,
    prog_var::out) is semidet.

is_output_arg(ModuleInfo, Var, Mode, Var) :-
    mode_is_fully_output(ModuleInfo, Mode).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.loop_inv.
%-----------------------------------------------------------------------------%
