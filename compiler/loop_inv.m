%-----------------------------------------------------------------------------%
% loop_inv.m
% Main author: rafe
% vim: ft=mercury ts=4 sw=4 et tw=0 wm=0
%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% CONSERVATIVE LOOP INVARIANT HOISTING.
%
%------------------------------------------------------------------------------%
%
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
% where W, X, Y may each stand for more several program variables.
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
% are identical across all calls to f at the end of recursive paths
% (a recursive path is a path from the start of the definition of f
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
%
%
% NOTE that this version of the optimization does not perform
% variable renaming, so the two calls to i/1 here will not be
% hoisted because they have different output variables:
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
% model-det recursive paths.
%------------------------------------------------------------------------------%

:- module transform_hlds__loop_inv.

:- interface.

:- import_module hlds.
:- import_module hlds__hlds_pred, hlds__hlds_module.

    % hoist_loop_invariants(PredId, ProcId, PredInfo, 
    %       ProcInfo0, ProcInfo, ModuleInfo0, ModuleInfo)
    %
    % Analyze the procedure identified by PredProcId and, if
    % appropriate, split it into two applying the loop invariant
    % hoisting optimization.
    %
:- pred hoist_loop_invariants(pred_id, proc_id, pred_info,
            proc_info, proc_info, module_info, module_info).
:- mode hoist_loop_invariants(in, in, in, in, out, in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module list, assoc_list, std_util, require, set, term, string, bool.
:- import_module parse_tree__prog_data, parse_tree__inst, parse_tree__prog_util.
:- import_module hlds__hlds_goal, hlds__instmap, hlds__error_util.
:- import_module hlds__quantification.
:- import_module check_hlds.
:- import_module check_hlds__mode_util, check_hlds__det_util.
:- import_module check_hlds__inst_match, check_hlds__purity.
:- import_module backend_libs__code_model.



:- func this_file = string.

this_file = "loop_inv.m".

%------------------------------------------------------------------------------%

hoist_loop_invariants(PredId, ProcId, PredInfo, ProcInfo0, ProcInfo,
        ModuleInfo0, ModuleInfo) :-

    ( if

            % We only want to apply this optimization to pure preds (e.g.
            % not benchmark_det_loop).
            %
        hlds_pred__pred_info_get_purity(PredInfo, pure),

            % Next, work out whether this predicate is optimizable and
            % compute some auxiliary results along the way.

            % Obtain the requisite info for this procedure.
            %
        PredProcId = proc(PredId, ProcId),
        hlds_pred__proc_info_goal(ProcInfo0, Body),
        hlds_pred__proc_info_headvars(ProcInfo0, HeadVars),
        hlds_pred__proc_info_argmodes(ProcInfo0, HeadVarModes),
        hlds_pred__proc_info_get_initial_instmap(ProcInfo0, ModuleInfo0,
                InitialInstMap),

            % Find the set of variables that are used as (partly) unique
            % inputs to calls.  These variables are not safe candidates
            % for hoisting.  (A variable whose initial bound inst is
            % inferred as unique may be hoistable if it is not used as a
            % unique input to any call.)
            %
        UniquelyUsedVars = uniquely_used_vars(ModuleInfo0, Body),

            % Find the set of candidate goals that may be invariant
            % and the set of recursive calls involved.
            %
            % A goal must appear on all recursive paths to be a
            % candidate.
            %
            % The recursive calls are the set of calls at the end
            % of each recursive path.
            %
        invariant_goal_candidates(PredProcId, Body, InvGoals0, RecCalls),

            % We can calculate the set of invariant args from
            % the set of recursive calls.
            %
        InvArgs0 = inv_args(ModuleInfo0, HeadVars, HeadVarModes, RecCalls),
        InvArgs  = InvArgs0 `difference` UniquelyUsedVars,

            % Given the invariant args, we can calculate the set
            % of invariant goals and vars.
            %
        inv_goals_vars(ModuleInfo0, UniquelyUsedVars,
                InvGoals0, InvGoals1, InvArgs, InvVars1),

            % We don't want to hoist out unifications with constants (i.e.
            % constructions where the RHS has no arguments) or deconstructions
            % (it's probably cheaper to do the dereference than pass an extra
            % argument).
            %
            % So here we compute the subset of InvGoals (and the corresponding
            % InvVars) that should not be hoisted.
            %
        dont_hoist(ModuleInfo0, InvGoals1, DontHoistGoals, DontHoistVars),

        InvGoals = InvGoals1 `difference` DontHoistGoals,
        InvVars  = InvVars1  `difference` DontHoistVars,

            % We only apply the optimization if the set of invariant goals
            % is non-empty.
            %
        InvGoals \= []
      
      then

            % The set of computed invariant vars is the difference
            % between the whole invariant var set and the set of
            % invariant args.
            %
        ComputedInvVars = InvVars `difference` InvArgs,

            % We need to calculate the initial instmap for the aux
            % proc by applying the instmap_deltas from the InvGoals
            % to InitialInstMap.
            %
        InitialAuxInstMap =
                compute_initial_aux_instmap(InvGoals, InitialInstMap),

            % Create the pred for the aux proc.  This is initially a
            % copy of the in proc with the head vars extended with the
            % list of computed inv vars.  The body is adjusted
            % appropriately in the next step.
            %
        create_aux_pred(PredProcId, HeadVars, ComputedInvVars,
                InitialAuxInstMap,
                AuxPredProcId, CallAux, AuxPredInfo, AuxProcInfo,
                ModuleInfo0, ModuleInfo1),

            % We update the body of AuxProc by replacing adding the
            % set of computed invariant vars to the argument list,
            % replacing invariant goals in InProc with `true', and
            % recursive calls at the end of recursive paths with
            % calls to the auxiliary procedure.
            %
        gen_aux_proc(InvGoals, PredProcId,
                AuxPredProcId, CallAux, Body, AuxPredInfo, AuxProcInfo,
                ModuleInfo1, ModuleInfo2),

            % We construct OutProc by replacing recursive calls to
            % the InProc at the end of recursive paths with calls
            % to the auxiliary procedure.
            %
        gen_out_proc(PredProcId, PredInfo, ProcInfo0, ProcInfo, CallAux,
                Body, ModuleInfo2, ModuleInfo)

      else

        ProcInfo   = ProcInfo0,
        ModuleInfo = ModuleInfo0
    ).

%------------------------------------------------------------------------------%

:- func list(T) `difference` list(T) = list(T).

Xs `difference` Ys = Xs `delete_elems` Ys.

%------------------------------------------------------------------------------%

:- type rec_call ==
            pair(
                hlds_goal,         % The recursive call.
                list(hlds_goal)    % The candidate invariant goal list
                                   %    for this recursive call.
            ).

:- type rec_calls == list(rec_call).

:- type invariant_goal_candidates_acc
    --->    invariant_goal_candidates_acc(
                    % path_candidates is the list of accumulated invariant
                    % goal candidates.
                    %
                path_candidates         :: hlds_goals,

                    % rec_calls is the list of pairs of recursive calls
                    % with the path_candidates up to that point.  This is
                    % extended whenever a recursive call is identified.
                    %
                rec_calls               :: rec_calls
            ).

    % invariant_goal_candidates(PredProcId, Body, CandidateInvGoals,
    %       RecCallGoals)
    %
    % Computes (a conservative approximation to) the set of candidate
    % invariant atomic goals in Body and the set of recursive calls
    % in Body identified via PredProcId.
    %
:- pred invariant_goal_candidates(pred_proc_id, hlds_goal,
            hlds_goals, hlds_goals).
:- mode invariant_goal_candidates(in, in, out, out) is det.

invariant_goal_candidates(PredProcId, Body,
        CandidateInvGoals, RecCallGoals) :-
    invariant_goal_candidates_acc(_, RecCalls) =
        invariant_goal_candidates_2(PredProcId, Body,
            invariant_goal_candidates_acc([], [])),
    assoc_list__keys_and_values(RecCalls, RecCallGoals, CandidateInvGoalsList),
    CandidateInvGoals = intersect_candidate_inv_goals(CandidateInvGoalsList).

%------------------------------------------------------------------------------%

:- func invariant_goal_candidates_2(pred_proc_id, hlds_goal,
            invariant_goal_candidates_acc
        ) = invariant_goal_candidates_acc.

invariant_goal_candidates_2(PPId,
        Call @ call(PredId, ProcId, _, _, _, _)      - GoalInfo,  IGCs) =
    ( if   proc(PredId, ProcId) = PPId
      then add_recursive_call(Call - GoalInfo, IGCs)
      else invariant_goal_candidates_handle_non_recursive_call(Call - GoalInfo,
                IGCs)
    ).

invariant_goal_candidates_2(_PPId,
        Call @ generic_call(_, _, _, _)             - GoalInfo,  IGCs) =
    invariant_goal_candidates_handle_non_recursive_call(Call - GoalInfo,
        IGCs).

invariant_goal_candidates_2(_PPId,
        Unification @ unify(_, _, _, _, _)          - GoalInfo,  IGCs) =
    invariant_goal_candidates_handle_non_recursive_call(Unification - GoalInfo,
        IGCs).

invariant_goal_candidates_2(_PPId,
        ForeignProc @ foreign_proc(_,_,_,_,_,_,_)   - GoalInfo,  IGCs) =
    invariant_goal_candidates_handle_non_recursive_call(ForeignProc - GoalInfo,
        IGCs).

invariant_goal_candidates_2(PPId,
        conj(Conjuncts)                              - _GoalInfo, IGCs) =
    list__foldl(invariant_goal_candidates_2(PPId),
                Conjuncts,
                IGCs).

invariant_goal_candidates_2(PPId,
        par_conj(ParConjuncts)                       - _GoalInfo, IGCs) =
    list__foldl(invariant_goal_candidates_keeping_path_candidates(PPId),
                ParConjuncts,
                IGCs).

invariant_goal_candidates_2(PPId,
        disj(Disjuncts)                              - _GoalInfo, IGCs) =
    list__foldl(invariant_goal_candidates_keeping_path_candidates(PPId),
                Disjuncts,
                IGCs).

invariant_goal_candidates_2(PPId,
        switch(_, _, Cases)                          - _GoalInfo, IGCs) =
    list__foldl(invariant_goal_candidates_keeping_path_candidates(PPId),
                case_goals(Cases),
                IGCs).

invariant_goal_candidates_2(PPId,
        not(NegatedGoal)                             - _GoalInfo, IGCs) =
    invariant_goal_candidates_keeping_path_candidates(PPId, NegatedGoal, IGCs).

invariant_goal_candidates_2(PPId,
        some(_, _, QuantifiedGoal)                   - _GoalInfo, IGCs) =
    invariant_goal_candidates_2(PPId, QuantifiedGoal, IGCs).

invariant_goal_candidates_2(PPId,
        if_then_else(_XVs, Cond, Then, Else)         - GoalInfo,  IGCs0) = IGCs
 :-
    CondThenGoal = conj([Cond, Then]) - GoalInfo,
    IGCs1        = invariant_goal_candidates_keeping_path_candidates(PPId,
                        CondThenGoal, IGCs0),
    ElseGoal     = Else,
    IGCs         = invariant_goal_candidates_keeping_path_candidates(PPId,
                        ElseGoal,     IGCs1).

invariant_goal_candidates_2(_PPId,
        shorthand(_)                                 - _GoalInfo, _IGCs) = _ :-
    unexpected(this_file,
        "invariant_goal_candidates_2/3: shorthand/1 in hlds_goal").

%------------------------------------------------------------------------------%

:- func invariant_goal_candidates_keeping_path_candidates(pred_proc_id,
            hlds_goal, invariant_goal_candidates_acc
        ) = invariant_goal_candidates_acc.

invariant_goal_candidates_keeping_path_candidates(PPId, Goal, IGCs) =
    ( invariant_goal_candidates_2(PPId, Goal, IGCs) ^ path_candidates :=
        IGCs ^ path_candidates ).

%------------------------------------------------------------------------------%

:- func case_goals(list(case)) = hlds_goals.

case_goals(Cases) =
    list__map(func(case(_ConsId, Goal)) = Goal, Cases).

%------------------------------------------------------------------------------%

:- func add_recursive_call(hlds_goal, invariant_goal_candidates_acc) =
            invariant_goal_candidates_acc.

    % We have to reverse the path_candidates because they are
    % accumulated in reverse order, whereas we need them in
    % producer-consumer order as they appear in the procedure.
    %
add_recursive_call(Goal, IGCs) =
    IGCs ^ rec_calls :=
        [Goal - list__reverse(IGCs ^ path_candidates) | IGCs ^ rec_calls].

%------------------------------------------------------------------------------%

    % NOTE: we could hoist semipure goals that have no preceeding
    % impure goals, but that's a very low-level optimization that
    % is not entirely trivial to implement.
    %
:- func invariant_goal_candidates_handle_non_recursive_call(
            hlds_goal, invariant_goal_candidates_acc
        ) = invariant_goal_candidates_acc.

invariant_goal_candidates_handle_non_recursive_call(
        Goal @ (_GoalExpr - GoalInfo), IGCs) =
    ( if   not model_non(GoalInfo),
           purity__goal_info_is_pure(GoalInfo)
      then IGCs ^ path_candidates := [Goal | IGCs ^ path_candidates]
      else IGCs
    ).

%------------------------------------------------------------------------------%

:- pred model_non(hlds_goal_info).
:- mode model_non(in) is semidet.

model_non(GoalInfo) :-
    hlds_goal__goal_info_get_determinism(GoalInfo, Detism),
    code_model__determinism_to_code_model(Detism, model_non).

%------------------------------------------------------------------------------%

:- func intersect_candidate_inv_goals(list(hlds_goals)) = hlds_goals.

intersect_candidate_inv_goals([]) = [].

intersect_candidate_inv_goals([Goals | Goalss]) =
    list__filter(common_goal(Goalss), Goals).

%------------------------------------------------------------------------------%

:- pred common_goal(list(hlds_goals), hlds_goal).
:- mode common_goal(in, in) is semidet.

common_goal(Goalss, Goal) :-
    all [Gs] (
        list__member(Gs, Goalss)
    =>
        (
            list__member(G,  Gs),
            equivalent_goals(G, Goal)
        )
    ).

%------------------------------------------------------------------------------%

:- pred equivalent_goals(hlds_goal, hlds_goal).
:- mode equivalent_goals(in, in) is semidet.

equivalent_goals(GoalExprX - _GoalInfoX, GoalExprY - _GoalInfoY) :-
    (
        GoalExprX = GoalExprY
    ;
        GoalExprX =
            call(PredId, ProcId, Args, _BuiltinStateX, _ContextX, _SymNameX),
        GoalExprY =
            call(PredId, ProcId, Args, _BuiltinStateY, _ContextY, _SymNameY)
    ).

%------------------------------------------------------------------------------%

:- func inv_args(module_info, prog_vars, list(mode), hlds_goals) = prog_vars.

inv_args(ModuleInfo, HeadVars, HeadVarModes, RecCalls) = InvArgs :-
    MaybeInvArgs0 =
        list__map_corresponding(
                arg_to_maybe_inv_arg(ModuleInfo), HeadVars, HeadVarModes),
    MaybeInvArgs  =
        list__foldl(refine_candidate_inv_args, RecCalls, MaybeInvArgs0),
    InvArgs       =
        list__filter_map(func(yes(Arg)) = Arg is semidet, MaybeInvArgs).

%------------------------------------------------------------------------------%

    % Maps an Arg in HeadVars to yes(Arg) if Arg is an input
    %                      or to no       otherwise.
    %
:- func arg_to_maybe_inv_arg(module_info, prog_var, mode) = maybe(prog_var).

arg_to_maybe_inv_arg(ModuleInfo, Arg, Mode) =
    ( if input_arg(ModuleInfo, Arg, Mode) = InvArg then yes(InvArg) else no ).

%------------------------------------------------------------------------------%

:- func refine_candidate_inv_args(hlds_goal, list(maybe(prog_var))) =
            list(maybe(prog_var)).

refine_candidate_inv_args(RecCall - _RecCallInfo, MaybeInvArgs) =
    ( if   RecCall = call(_, _, CallArgs, _, _, _)
      then list__map_corresponding(refine_candidate_inv_args_2,
                                   MaybeInvArgs,
                                   CallArgs)
      else func_error("refine_candidate_inv_args/2: non call/6 \
found in argument 1")
    ).



:- func refine_candidate_inv_args_2(maybe(prog_var), prog_var) =
            maybe(prog_var).

refine_candidate_inv_args_2(no,     _) = no.
refine_candidate_inv_args_2(yes(X), Y) = ( if X = Y then yes(X) else no ).

%------------------------------------------------------------------------------%

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
:- pred inv_goals_vars(module_info, prog_vars,
            hlds_goals, hlds_goals, prog_vars, prog_vars).
:- mode inv_goals_vars(in, in, in, out, in, out) is det.

inv_goals_vars(ModuleInfo, UniquelyUsedVars,
        InvGoals0, InvGoals, InvVars0, InvVars) :-
    list__foldl2(
        inv_goals_vars_2(ModuleInfo, UniquelyUsedVars),
        InvGoals0,
        [],         InvGoals,
        InvVars0,   InvVars
    ).

%------------------------------------------------------------------------------%

:- pred inv_goals_vars_2(module_info, prog_vars, hlds_goal,
            hlds_goals, hlds_goals, prog_vars, prog_vars).
:- mode inv_goals_vars_2(in, in, in, in, out, in, out) is det.

inv_goals_vars_2(MI, UUVs, Goal, IGs0, IGs, IVs0, IVs) :-
    ( if
        not invariant_goal(IGs0, Goal),
        input_args_are_invariant(MI, Goal, IVs0)
      then
        IGs = [Goal | IGs0],
        IVs = add_outputs(MI, UUVs, Goal, IVs0)
      else
        IGs = IGs0,
        IVs = IVs0
    ).

%------------------------------------------------------------------------------%

:- pred invariant_goal(hlds_goals, hlds_goal).
:- mode invariant_goal(in, in) is semidet.

invariant_goal(InvariantGoals, Goal) :-
    list__member(InvariantGoal, InvariantGoals),
    equivalent_goals(InvariantGoal, Goal).

%------------------------------------------------------------------------------%

:- pred input_args_are_invariant(module_info, hlds_goal, prog_vars).
:- mode input_args_are_invariant(in, in, in) is semidet.

input_args_are_invariant(ModuleInfo, Goal, InvVars) :-
    Inputs = goal_inputs(ModuleInfo, Goal),
    all [V]
        ( list__member(V, Inputs) => list__member(V, InvVars) ).

%------------------------------------------------------------------------------%

:- pred dont_hoist(module_info, hlds_goals, hlds_goals, prog_vars).
:- mode dont_hoist(in, in, out, out) is det.

dont_hoist(MI, InvGoals, DontHoistGoals, DontHoistVars) :-
    list__foldl2(dont_hoist_2(MI), InvGoals,
        [], DontHoistGoals, [], DontHoistVars).


:- pred dont_hoist_2(module_info, hlds_goal,
            hlds_goals, hlds_goals, prog_vars, prog_vars).
:- mode dont_hoist_2(in, in, in, out, in, out) is det.

dont_hoist_2(MI, Goal, DHGs0, DHGs, DHVs0, DHVs) :-
    ( if
        (   const_construction(Goal)
        ;   deconstruction(Goal)
        ;   const_goal(MI, Goal)
        ;   impure_goal(Goal)
        )
      then
        DHGs = [Goal | DHGs0],
        DHVs = add_outputs(MI, [], Goal, DHVs0)
      else
        DHGs = DHGs0,
        DHVs = DHVs0
    ).

%------------------------------------------------------------------------------%

    % A constant construction is a construction unification with no
    % arguments or which is constructed from a statically initialized
    % constant.
    %
:- pred const_construction(hlds_goal).
:- mode const_construction(in) is semidet.

const_construction(GoalExpr - _GoalInfo) :-
    Construction = GoalExpr ^ unify_kind,
    (   Construction ^ construct_args = []
    ;   Construction ^ construct_how  = construct_statically(_)
    ).

%------------------------------------------------------------------------------%

:- pred deconstruction(hlds_goal).
:- mode deconstruction(in) is semidet.

deconstruction(GoalExpr - _GoalInfo) :-
    GoalExpr ^ unify_kind = deconstruct(_, _, _, _, _, _).

%------------------------------------------------------------------------------%

    % A const goal has no inputs.
    %
:- pred const_goal(module_info, hlds_goal).
:- mode const_goal(in, in) is semidet.

const_goal(ModuleInfo, Goal) :-
    goal_inputs(ModuleInfo, Goal) = [].

%------------------------------------------------------------------------------%

:- pred impure_goal(hlds_goal).
:- mode impure_goal(in) is semidet.

impure_goal(_GoalExpr - GoalInfo) :-
    purity__goal_info_is_impure(GoalInfo).

%------------------------------------------------------------------------------%

:- type inst_info == {module_info, instmap}.

:- pred arg_is_input(inst_info, prog_var).
:- mode arg_is_input(in, in) is semidet.

arg_is_input(InstInfo, Arg) :-
    InstInfo = {_ModuleInfo, InstMap},
    instmap__lookup_var(InstMap, Arg, Inst),
    inst_is_input(InstInfo, Inst).

%------------------------------------------------------------------------------%

    % We take an initial inst to be an input if it is fully ground
    % and not unique.
    %
:- pred inst_is_input(inst_info, inst).
:- mode inst_is_input(in, in) is semidet.

inst_is_input({ModuleInfo, _InstMap}, Inst) :-
    inst_match__inst_is_ground(ModuleInfo, Inst),
    inst_match__inst_is_not_partly_unique(ModuleInfo, Inst).

%------------------------------------------------------------------------------%

:- func update_inst_info(hlds_goal, inst_info) = inst_info.

update_inst_info(Goal, {ModuleInfo, InstMap0}) = {ModuleInfo, InstMap} :-
    det_util__update_instmap(Goal, InstMap0, InstMap).

%------------------------------------------------------------------------------%

:- func add_outputs(module_info, prog_vars, hlds_goal, prog_vars) =
            prog_vars.

add_outputs(ModuleInfo, UUVs, Goal, InvVars) =
    list__foldl(add_output(UUVs), goal_outputs(ModuleInfo, Goal), InvVars).



:- func add_output(prog_vars, prog_var, prog_vars) = prog_vars.

add_output(UniquelyUsedVars, X, InvVars) =
    ( if   not list__member(X, InvVars),
           not list__member(X, UniquelyUsedVars)
      then [X | InvVars]
      else InvVars
    ).

%------------------------------------------------------------------------------%

:- func compute_initial_aux_instmap(hlds_goals, instmap) = instmap.

compute_initial_aux_instmap(Gs, IM) =
    list__foldl(ApplyGoalInstMap, Gs, IM)
 :-
    ApplyGoalInstMap =
        ( func(_GoalExpr - GoalInfo, IM0) = IM1 :-
            hlds_goal__goal_info_get_instmap_delta(GoalInfo, IMD),
            instmap__apply_instmap_delta(IM0, IMD, IM1)
        ).

%------------------------------------------------------------------------------%

:- pred create_aux_pred(
            pred_proc_id, prog_vars, prog_vars, instmap,
            pred_proc_id, hlds_goal, pred_info, proc_info,
            module_info, module_info).
:- mode create_aux_pred(
            in, in, in, in,
            out, out, out, out,
            in, out) is det.

create_aux_pred(PredProcId, HeadVars, ComputedInvArgs,
        InitialAuxInstMap, AuxPredProcId, CallAux,
        AuxPredInfo, AuxProcInfo,
        ModuleInfo0, ModuleInfo) :-

    PredProcId = proc(PredId, ProcId),

    AuxHeadVars = HeadVars ++ ComputedInvArgs,

    hlds_module__module_info_name(ModuleInfo0, ModuleName),
    hlds_module__module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
            PredInfo, ProcInfo),

    hlds_pred__proc_info_goal(ProcInfo, Goal @ (_GoalExpr - GoalInfo)),
    hlds_pred__pred_info_typevarset(PredInfo, TVarSet),
    hlds_pred__proc_info_vartypes(ProcInfo, VarTypes),
    hlds_pred__pred_info_get_class_context(PredInfo, ClassContext),
    hlds_pred__proc_info_typeinfo_varmap(ProcInfo, TVarMap),
    hlds_pred__proc_info_typeclass_info_varmap(ProcInfo, TCVarMap),
    hlds_pred__proc_info_varset(ProcInfo, VarSet),
    hlds_pred__proc_info_inst_varset(ProcInfo, InstVarSet),
    hlds_pred__pred_info_get_markers(PredInfo, Markers),
    hlds_pred__pred_info_get_aditi_owner(PredInfo, Owner),

    hlds_pred__pred_info_name(PredInfo, PredName),
    hlds_goal__goal_info_get_context(GoalInfo, Context),
    term__context_line(Context, Line),
    hlds_pred__proc_id_to_int(ProcId, ProcNo),
    AuxNamePrefix = string__format("loop_inv_%d", [i(ProcNo)]),
    prog_util__make_pred_name_with_context(ModuleName, AuxNamePrefix,
            predicate, PredName, Line, 1, AuxPredSymName),
    (
        AuxPredSymName = unqualified(AuxPredName)
    ;
        AuxPredSymName = qualified(_ModuleSpecifier, AuxPredName)
    ),

        % Put in oven at gas mark 11 and bake.
        %
    hlds_pred__define_new_pred(
        Goal,           % in    - The goal for the new aux proc.
        CallAux,        % out   - How we can call the new aux proc.
        AuxHeadVars,    % in    - The args for the new aux proc.
        _ExtraArgs,     % out   - Extra args prepended to Args for typeinfo
                        %           liveness purposes.
        InitialAuxInstMap,
                        % in    - The initial instmap for the new aux proc.
        AuxPredName,    % in    - The name of the new aux proc.
        TVarSet,        % in    - ???
        VarTypes,       % in    - The var -> type mapping for the new aux proc.
        ClassContext,   % in    - Typeclass constraints on the new aux proc.
        TVarMap,        % in    - The tvar -> type_info_locn map for this proc.
        TCVarMap,       % in    - The class_constraint -> var map for
                        %           locating the type class typeclass_info.
        VarSet,         % in    - ???
        InstVarSet,     % in    - ???
        Markers,        % in    - Markers for the new aux proc.
        Owner,          % in    - The Aditi owner string for the new aux proc.
        address_is_not_taken,
                        % in    - The address of the new aux proc is not taken.
        ModuleInfo0,
        ModuleInfo,
        AuxPredProcId   % out   - The pred_proc_id for the new aux proc.
    ),

        % Note on CallAux:
        % - we change the call args as necessary in gen_aux_call;
        % - we handle the changes to nonlocals by requantifying
        %   over the entire goal after we've transformed it.

    AuxPredProcId = proc(AuxPredId, AuxProcId),
    hlds_module__module_info_pred_proc_info(ModuleInfo, AuxPredId, AuxProcId,
            AuxPredInfo, AuxProcInfo).

%------------------------------------------------------------------------------%

:- type gen_aux_proc_info
    --->    gen_aux_proc_info(
                module_info             :: module_info,
                inv_goals               :: hlds_goals,
                pred_proc_id            :: pred_proc_id,
                call_aux_goal           :: hlds_goal
            ).

    % Replace the invariant goals in the original Body
    % with just `true' in the new AuxBody.
    %
:- pred gen_aux_proc(hlds_goals, pred_proc_id,
            pred_proc_id, hlds_goal, hlds_goal,
            pred_info, proc_info,
            module_info, module_info).
:- mode gen_aux_proc(in, in,
            in, in, in,
            in, in,
            in, out) is det.

gen_aux_proc(InvGoals, PredProcId,
        AuxPredProcId, CallAux, Body,
        AuxPredInfo, AuxProcInfo0,
        ModuleInfo0, ModuleInfo) :-

        % Compute the aux proc body.
        %
    GapInfo = gen_aux_proc_info(ModuleInfo0, InvGoals, PredProcId, CallAux),
    AuxBody = gen_aux_proc_2(GapInfo, Body),

        % Put the new proc body and instmap into the module_info.
        %
    AuxPredProcId = proc(AuxPredId, AuxProcId),

    hlds_pred__proc_info_varset(AuxProcInfo0, AuxVarSet),
    hlds_pred__proc_info_vartypes(AuxProcInfo0, AuxVarTypes),
    hlds_pred__proc_info_headvars(AuxProcInfo0, AuxHeadVars),
    hlds_pred__proc_info_typeinfo_varmap(AuxProcInfo0, AuxTVarMap),
    hlds_pred__proc_info_typeclass_info_varmap(AuxProcInfo0, AuxTCVarMap),

    hlds_pred__proc_info_set_body(AuxProcInfo0, AuxVarSet, AuxVarTypes,
            AuxHeadVars, AuxBody, AuxTVarMap, AuxTCVarMap, AuxProcInfo1),

    quantification__requantify_proc(AuxProcInfo1, AuxProcInfo2),
    mode_util__recompute_instmap_delta_proc(no, AuxProcInfo2, AuxProcInfo,
            ModuleInfo0, ModuleInfo1),

    hlds_module__module_info_set_pred_proc_info(ModuleInfo1,
            AuxPredId, AuxProcId, AuxPredInfo, AuxProcInfo, ModuleInfo).

%------------------------------------------------------------------------------%

:- func gen_aux_proc_2(gen_aux_proc_info, hlds_goal) = hlds_goal.

gen_aux_proc_2(Info, Call @ call(PredId, ProcId, _,_,_,_)    - GoalInfo) =
    ( if   proc(PredId, ProcId) = Info ^ pred_proc_id
      then gen_aux_call(Info ^ call_aux_goal, Call - GoalInfo)
      else gen_aux_proc_handle_non_recursive_call(Info, Call - GoalInfo)
    ).

gen_aux_proc_2(Info, Call @ generic_call(_, _, _, _) - GoalInfo) =
    gen_aux_proc_handle_non_recursive_call(Info, Call - GoalInfo).

gen_aux_proc_2(Info, Unification @ unify(_, _, _, _, _) - GoalInfo) =
    gen_aux_proc_handle_non_recursive_call(Info, Unification - GoalInfo).

gen_aux_proc_2(Info, ForeignProc @ foreign_proc(_, _, _, _, _, _, _) -
        GoalInfo) =
    gen_aux_proc_handle_non_recursive_call(Info, ForeignProc - GoalInfo).

gen_aux_proc_2(Info, conj(Conjuncts) - GoalInfo) =
    conj(gen_aux_proc_list(Info, Conjuncts)) - GoalInfo.

gen_aux_proc_2(Info, par_conj(Conjs) - GoalInfo) =
    par_conj(gen_aux_proc_list(Info, Conjs)) - GoalInfo.

gen_aux_proc_2(Info, disj(Disjuncts) - GoalInfo) =
    disj(gen_aux_proc_list(Info, Disjuncts)) - GoalInfo.

gen_aux_proc_2(Info, switch(Var, CanFail, Cases) - GoalInfo) =
    switch(Var, CanFail, gen_aux_proc_switch(Info, Cases)) - GoalInfo.

gen_aux_proc_2(Info, not(NegatedGoal) - GoalInfo) =
    not(gen_aux_proc_2(Info, NegatedGoal)) - GoalInfo.

gen_aux_proc_2(Info, some(XVars, CanRemove, QuantifiedGoal) - GoalInfo) =
    some(XVars, CanRemove, gen_aux_proc_2(Info, QuantifiedGoal)) - GoalInfo.

gen_aux_proc_2(Info, if_then_else(XVars, Cond, Then, Else) - GoalInfo) =
    if_then_else(XVars,
                 gen_aux_proc_2(Info, Cond),
                 gen_aux_proc_2(Info, Then),
                 gen_aux_proc_2(Info, Else)
    ) - GoalInfo.

gen_aux_proc_2(_Info, shorthand(_) - _GoalInfo) = _ :-
    unexpected(this_file, "gen_aux_proc_2/2: shorthand/1 in hlds_goal").

%------------------------------------------------------------------------------%

:- func gen_aux_proc_list(gen_aux_proc_info, hlds_goals) = hlds_goals.

gen_aux_proc_list(Info, Goals) = list__map(gen_aux_proc_2(Info), Goals).

%------------------------------------------------------------------------------%

:- func gen_aux_proc_switch(gen_aux_proc_info, list(case)) = list(case).

gen_aux_proc_switch(Info, Cases) =
    list__map(
        func(case(CaseId, Goal)) = case(CaseId, gen_aux_proc_2(Info, Goal)),
        Cases
    ).

%------------------------------------------------------------------------------%

:- func gen_aux_proc_handle_non_recursive_call(gen_aux_proc_info, hlds_goal) =
            hlds_goal.

gen_aux_proc_handle_non_recursive_call(Info, Goal0) = Goal :-
    ( if   invariant_goal(Info ^ inv_goals, Goal0)
      then true_goal(Goal)
      else Goal = Goal0
    ).

%------------------------------------------------------------------------------%

    % We construct OutProc by replacing recursive calls to
    % the InProc at the end of recursive paths with calls
    % to the auxiliary procedure.
    %
:- pred gen_out_proc(pred_proc_id, pred_info, proc_info, proc_info,
            hlds_goal, hlds_goal,
            module_info, module_info).
:- mode gen_out_proc(in, in, in, out, in, in, in, out) is det.

gen_out_proc(PredProcId, PredInfo0, ProcInfo0, ProcInfo, CallAux, Body0,
        ModuleInfo0, ModuleInfo) :-

        % Compute the new procedure body.
        %
    Body = gen_out_proc_2(PredProcId, CallAux, Body0),

        % Put the new procedure body into the module_info.
        %
    PredProcId = proc(PredId, ProcId),

    hlds_pred__proc_info_varset(ProcInfo0, VarSet),
    hlds_pred__proc_info_vartypes(ProcInfo0, VarTypes),
    hlds_pred__proc_info_headvars(ProcInfo0, HeadVars),
    hlds_pred__proc_info_typeinfo_varmap(ProcInfo0, TVarMap),
    hlds_pred__proc_info_typeclass_info_varmap(ProcInfo0, TCVarMap),

    hlds_pred__proc_info_set_body(ProcInfo0, VarSet, VarTypes,
            HeadVars, Body, TVarMap, TCVarMap, ProcInfo1),
    
    quantification__requantify_proc(ProcInfo1, ProcInfo2),
    mode_util__recompute_instmap_delta_proc(no, ProcInfo2, ProcInfo,
            ModuleInfo0, ModuleInfo1),

    hlds_module__module_info_set_pred_proc_info(ModuleInfo1, PredId, ProcId,
            PredInfo0, ProcInfo, ModuleInfo).

%------------------------------------------------------------------------------%

    % gen_out_proc_2(PredProcId, CallAux, Goal0) = Goal:
    %   Goal is Goal0 with calls to PredProcId replaced with CallAux.
    %
:- func gen_out_proc_2(pred_proc_id, hlds_goal, hlds_goal) = hlds_goal.

gen_out_proc_2(PPId, CallAux,
        Call @ call(PredId, ProcId, _, _, _, _)        - GoalInfo) =
    ( if   proc(PredId, ProcId) = PPId
      then gen_aux_call(CallAux, Call - GoalInfo)
      else Call - GoalInfo
    ).

gen_out_proc_2(_PPId, _CallAux,
        Call @ generic_call(_, _, _, _)              - GoalInfo) =
    Call - GoalInfo.

gen_out_proc_2(_PPId, _CallAux,
        Unification @ unify(_, _, _, _, _)           - GoalInfo) =
    Unification - GoalInfo.

gen_out_proc_2(_PPId, _CallAux,
        ForeignProc @ foreign_proc(_,_,_,_,_,_,_)    - GoalInfo) =
    ForeignProc - GoalInfo.

gen_out_proc_2(PPId, CallAux,
        conj(Conjuncts)                                - GoalInfo) =
    conj(list__map(gen_out_proc_2(PPId, CallAux), Conjuncts)) - GoalInfo.

gen_out_proc_2(PPId, CallAux,
        par_conj(ParConjuncts)                         - GoalInfo) =
    par_conj(list__map(gen_out_proc_2(PPId, CallAux), ParConjuncts)) - GoalInfo.

gen_out_proc_2(PPId, CallAux,
        disj(Disjuncts)                                - GoalInfo) =
    disj(list__map(gen_out_proc_2(PPId, CallAux), Disjuncts)) - GoalInfo.

gen_out_proc_2(PPId, CallAux,
        switch(Var, CanFail, Cases)                    - GoalInfo) =
    switch(Var, CanFail, list__map(GOPCase, Cases)) - GoalInfo
 :-
    GOPCase =
        ( func(case(ConsId, Goal)) =
                case(ConsId, gen_out_proc_2(PPId, CallAux, Goal)) ).

gen_out_proc_2(PPId, CallAux,
        not(NegatedGoal)                               - GoalInfo) =
    not(gen_out_proc_2(PPId, CallAux, NegatedGoal)) - GoalInfo.

gen_out_proc_2(PPId, CallAux,
        some(XVars, CanRemove, QuantifiedGoal)         - GoalInfo) =
    some(XVars, CanRemove, gen_out_proc_2(PPId, CallAux, QuantifiedGoal)) -
            GoalInfo.

gen_out_proc_2(PPId, CallAux,
        if_then_else(XVars, Cond, Then, Else)          - GoalInfo) =
    if_then_else(
        XVars,
        gen_out_proc_2(PPId, CallAux, Cond),
        gen_out_proc_2(PPId, CallAux, Then),
        gen_out_proc_2(PPId, CallAux, Else)
    ) - GoalInfo.

gen_out_proc_2(_PPId, _CallAux,
        shorthand(_)                                   - _GoalInfo) = _ :-
    unexpected(this_file, "gen_out_proc_2/3: shorthand/1 in hlds_goal").

%------------------------------------------------------------------------------%

:- func gen_aux_call(hlds_goal, hlds_goal) = hlds_goal.

gen_aux_call(CallAux0 - _CallAuxInfo0, Call - CallInfo) =
    ( if
        AuxArgs0      = CallAux0   ^ call_args,
        Args0         = Call       ^ call_args,
        Args          = replace_initial_args(Args0, AuxArgs0),
        CallAux       = ( CallAux0 ^ call_args := Args )
            %
            % Note that one might expect instmap_delta to change,
            % however the invariant arguments are just that -
            % invariant - hence their insts are not changed by
            % the recursive call and there is no need to
            % adjust the instmap_delta.  All other fields
            % are correct for CallInfo.
      then
        CallAux - CallInfo
      else
        func_error("gen_aux_call/2: args not both ordinary calls")
    ).

%------------------------------------------------------------------------------%

:- func replace_initial_args(list(T), list(T)) = list(T).

replace_initial_args([],       Ys      ) = Ys.

replace_initial_args([X | Xs], [_ | Ys]) = [X | replace_initial_args(Xs, Ys)].

replace_initial_args([_ | _],  []      ) = _ :-
    error("replace_initial_args/2: first arg longer than second").

%------------------------------------------------------------------------------%

    % This predicate computes the set of variables that are used
    % as (partly) unique inputs to goals.  This information is
    % needed because unique local values for which uniqueness is
    % important cannot be hoisted, although those for which uniqueness
    % is inferred, but not important, can be hoisted.
    %
    % TODO: get this to handle unification properly.  See the XXX below.
    %
:- func uniquely_used_vars(module_info, hlds_goal) = prog_vars.

uniquely_used_vars(ModuleInfo, Goal) =
    list__sort_and_remove_dups(uniquely_used_vars_2(ModuleInfo, Goal)).

%------------------------------------------------------------------------------%

:- func uniquely_used_vars_2(module_info, hlds_goal) = prog_vars.

uniquely_used_vars_2(MI, call(PredId, ProcId, Args, _, _, _) - _) =
    list__filter_map_corresponding(uniquely_used_args(MI),
                                   Args,
                                   argmodes(MI,PredId,ProcId)).

uniquely_used_vars_2(MI, generic_call(_, Args, Modes, _) - _) =
    list__filter_map_corresponding(uniquely_used_args(MI),
                                   Args,
                                   Modes).

uniquely_used_vars_2(MI, foreign_proc(_, PredId, ProcId, Args, _, _, _) - _) =
    list__filter_map_corresponding(uniquely_used_args(MI),
                                   Args,
                                   argmodes(MI,PredId,ProcId)).

    % XXX This is very conservative!
    %
uniquely_used_vars_2(_MI, unify(_LHS, _RHS, _UMode, _UKind, _) - _) = [].

uniquely_used_vars_2(MI, conj(Conjuncts) - _) =
    list__condense(list__map(uniquely_used_vars_2(MI), Conjuncts)).

uniquely_used_vars_2(MI, par_conj(ParConjuncts) - _) =
    list__condense(list__map(uniquely_used_vars_2(MI), ParConjuncts)).

uniquely_used_vars_2(MI, disj(Disjuncts) - _) =
    list__condense(list__map(uniquely_used_vars_2(MI), Disjuncts)).

uniquely_used_vars_2(MI, switch(_, _, Cases) - _) =
    list__condense(list__map(uniquely_used_vars_2(MI), case_goals(Cases))).

uniquely_used_vars_2(MI, not(NegatedGoal) - _) =
    uniquely_used_vars_2(MI, NegatedGoal).

uniquely_used_vars_2(MI, some(_, _, QuantifiedGoal) - _) =
    uniquely_used_vars_2(MI, QuantifiedGoal).

uniquely_used_vars_2(MI, if_then_else(_, Cond, Then, Else) - _) =
    uniquely_used_vars_2(MI, Cond) ++
        uniquely_used_vars_2(MI, Then) ++
            uniquely_used_vars_2(MI, Else).

uniquely_used_vars_2(_MI, shorthand(_) - _) = _ :-
    unexpected(this_file, "uniquely_used_vars_2/3: shorthand/1 in hlds_goal").

%------------------------------------------------------------------------------%

:- func uniquely_used_args(module_info, prog_var, mode) = prog_var is semidet.

uniquely_used_args(MI, X, M) = X :-
    mode_util__mode_get_insts(MI, M, InInst, _OutInst),
    not inst_match__inst_is_not_partly_unique(MI, InInst).

%------------------------------------------------------------------------------%

:- func argmodes(module_info, pred_id, proc_id) = list(mode).

argmodes(ModuleInfo, PredId, ProcId) = ArgModes :-
    hlds_module__module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _,
            ProcInfo),
    hlds_pred__proc_info_argmodes(ProcInfo, ArgModes).

%------------------------------------------------------------------------------%

    % Find the list of vars for a goal that are free before the call.
    % This only applies to calls and unifications.
    %
:- func goal_inputs(module_info, hlds_goal) = prog_vars.

goal_inputs(MI, call(PredId, ProcId, Args, _, _, _) - _) =
    list__filter_map_corresponding(
            input_arg(MI), Args, argmodes(MI, PredId, ProcId)).

goal_inputs(MI, generic_call(_, Args, ArgModes, _) - _) =
    list__filter_map_corresponding(
            input_arg(MI), Args, ArgModes).

goal_inputs(MI, foreign_proc(_, PredId, ProcId, Args, _, _, _) - _) =
    list__filter_map_corresponding(
            input_arg(MI), Args, argmodes(MI, PredId, ProcId)).

goal_inputs(MI, unify(LHS, UnifyRHS, _, Kind, _) - _) = Inputs :-
    (
            % The LHS is always an output var in constructions.
            %
        Kind   = construct(_, _, RHSArgs, ArgUniModes, _, _, _),
        Inputs = list__filter_map_corresponding(
                        input_arg(MI), RHSArgs, rhs_modes(ArgUniModes))
    ;
            % The LHS is always in input var in deconstructions.
            %
        Kind   = deconstruct(_, _, RHSArgs, ArgUniModes, _, _),
        Inputs = [ LHS
                 | list__filter_map_corresponding(
                        input_arg(MI), RHSArgs, rhs_modes(ArgUniModes)) ]
    ;
            % The RHS is the only input in an assignment.
            %
        Kind   = assign(_, RHS),
        Inputs = [RHS]
    ;
            % Both sides of a simple test are inputs.
            %
        Kind   = simple_test(_, RHS),
        Inputs = [LHS, RHS]
    ;
            % Both sides of a complicated unification are inputs.
            %
        Kind   = complicated_unify(_, _, _),
        Inputs = ( if UnifyRHS = var(RHS) then [LHS, RHS] else [LHS] )
    ).

goal_inputs(_MI, conj(_) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: conj/1 in hlds_goal").

goal_inputs(_MI, switch(_, _, _) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: switch/3 in hlds_goal").

goal_inputs(_MI, disj(_) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: disj/1 in hlds_goal").

goal_inputs(_MI, not(_) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: not/1 in hlds_goal").

goal_inputs(_MI, some(_, _, _) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: some/3 in hlds_goal").

goal_inputs(_MI, if_then_else(_, _, _, _) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: if_then_else/4 in hlds_goal").

goal_inputs(_MI, par_conj(_) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: par_conj/2 in hlds_goal").

goal_inputs(_MI, shorthand(_) - _) = _ :-
    unexpected(this_file, "goal_inputs/2: shorthand/1 in hlds_goal").

%------------------------------------------------------------------------------%

    % An input arg is one whose pre-call inst is not free.
    %
:- func input_arg(module_info, prog_var, mode) = prog_var is semidet.

input_arg(MI, X, M) = X :-
    mode_util__mode_get_insts(MI, M, InInst, _OutInst),
    not inst_match__inst_is_free(MI, InInst).

%------------------------------------------------------------------------------%

    % Find the list of vars for a goal that are free before the call.
    % This only applies to calls and unifications.
    %
:- func goal_outputs(module_info, hlds_goal) = prog_vars.

goal_outputs(MI, call(PredId, ProcId, Args, _, _, _) - _) =
    list__filter_map_corresponding(
            output_arg(MI), Args, argmodes(MI, PredId, ProcId)).

goal_outputs(MI, generic_call(_, Args, ArgModes, _) - _) =
    list__filter_map_corresponding(
            output_arg(MI), Args, ArgModes).

goal_outputs(MI, foreign_proc(_, PredId, ProcId, Args, _, _, _) - _) =
    list__filter_map_corresponding(
            output_arg(MI), Args, argmodes(MI, PredId, ProcId)).

goal_outputs(MI, unify(LHS, _RHS, _, Kind, _) - _) = Outputs :-
    (
            % The LHS is the only output in a construction.
            %
        Kind    = construct(_, _, _, _, _, _, _),
        Outputs = [LHS]
    ;
            % The LHS is always in input in deconstructions.
            %
        Kind    = deconstruct(_, _, RHSArgs, ArgUniModes, _, _),
        Outputs = list__filter_map_corresponding(
                            output_arg(MI), RHSArgs, rhs_modes(ArgUniModes))
    ;
            % The LHS is the only output in an assignment.
            %
        Kind    = assign(_, _),
        Outputs = [LHS]
    ;
            % Both sides of a simple test are inputs.
            %
        Kind    = simple_test(_, _),
        Outputs = []
    ;
            % Both sides of a complicated unification are inputs.
            %
        Kind    = complicated_unify(_, _, _),
        Outputs = []
    ).

goal_outputs(_MI, conj(_) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: conj/1 in hlds_goal").

goal_outputs(_MI, switch(_, _, _) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: switch/3 in hlds_goal").

goal_outputs(_MI, disj(_) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: disj/1 in hlds_goal").

goal_outputs(_MI, not(_) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: not/1 in hlds_goal").

goal_outputs(_MI, some(_, _, _) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: some/3 in hlds_goal").

goal_outputs(_MI, if_then_else(_, _, _, _) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: if_then_else/4 in hlds_goal").

goal_outputs(_MI, par_conj(_) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: par_conj/1 in hlds_goal").

goal_outputs(_MI, shorthand(_) - _) = _ :-
    unexpected(this_file, "goal_outputs/2: shorthand/1 in hlds_goal").

%------------------------------------------------------------------------------%

    % An output arg is one whose pre-call inst is free.
    %
:- func output_arg(module_info, prog_var, mode) = prog_var is semidet.

output_arg(MI, X, M) = X :-
    mode_util__mode_get_insts(MI, M, InInst, _OutInst),
    inst_match__inst_is_free(MI, InInst).

%------------------------------------------------------------------------------%

:- func rhs_modes(list(uni_mode)) = list(mode).

rhs_modes(UniModes) =
    list__map(func((_ - Pre) -> (_ - Post)) = (Pre -> Post), UniModes).

%------------------------------------------------------------------------------%

:- func lhs_modes(list(uni_mode)) = list(mode).

lhs_modes(UniModes) =
    list__map(func((Pre - _) -> (Post - _)) = (Pre -> Post), UniModes).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
