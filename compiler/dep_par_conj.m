%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: dep_par_conj.m.
% Author: wangp.
% 
% This module transforms the HLDS to implement dependent parallel conjunction.
% The transformation involves adding calls to the predicates defined in
% library/par_builtin.m.
% 
% For a parallel conjunction (A & B), if the goal B is dependent on a variable
% X which is bound by goal A, we transform the conjunction into the following:
%
%    par_builtin.new_future(FutureX),
%    (
%        (
%            A(X),  % binds X
%            impure par_builtin.signal(FutureX, X)
%        )
%    &
%        (
%            par_builtin.wait(FutureX, X1),
%            B(X1)  % uses X
%        )
%    )
%
% That is, goal B must wait for the value to be produced by A before it begins
% executing.  This transformation is not yet useful in practice (you might as
% well use a sequential conjunction).  A later version of this transformation
% will move signal and wait calls into goals to, hopefully, actually allow
% parallelism.
%
% If building in a non-parallel grade then dependent parallel conjunctions
% are simply converted into sequential conjunctions.
%
% TODO:
% - move signal and wait calls into goals
% - only run this pass if parallel conjunctions are present in a module
% - reconsider when this pass is run; in particular par_builtin primitives
%   ought to be inlined
% 
%-----------------------------------------------------------------------------%

:- module transform_hlds.dep_par_conj.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred dependent_par_conj(module_info::in, module_info::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.dependency_graph.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svset.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

dependent_par_conj(!ModuleInfo, !IO) :-
    module_info_predids(!.ModuleInfo, PredIds),
    list.foldl2(dependent_par_conj_2, PredIds, !ModuleInfo, !IO).

:- pred dependent_par_conj_2(pred_id::in, module_info::in, module_info::out,
    io::di, io::uo) is det.

dependent_par_conj_2(PredId, !ModuleInfo, !IO) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.foldl2(search_proc_for_par_conj(PredId), ProcIds, !ModuleInfo, !IO).

:- pred search_proc_for_par_conj(pred_id::in, proc_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

search_proc_for_par_conj(PredId, ProcId, !ModuleInfo, !IO) :-
    some [!PredInfo, !ProcInfo] (
        module_info_preds(!.ModuleInfo, PredTable0),
        map.lookup(PredTable0, PredId, !:PredInfo),
        pred_info_get_procedures(!.PredInfo, ProcTable0),
        map.lookup(ProcTable0, ProcId, !:ProcInfo),

        proc_info_get_goal(!.ProcInfo, Body0),
        proc_info_get_varset(!.ProcInfo, VarSet0),
        proc_info_get_vartypes(!.ProcInfo, VarTypes0),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap),

        search_goal_for_par_conj(!.ModuleInfo, 
            VarSet0, VarSet, VarTypes0, VarTypes,
            InstMap, _, Body0, Body, !IO),

        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_goal(Body, !ProcInfo),

        requantify_proc(!ProcInfo),

        RecomputeAtomic = no,
        recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo),

        map.det_update(ProcTable0, ProcId, !.ProcInfo, ProcTable),
        pred_info_set_procedures(ProcTable, !PredInfo),

        % The transformation doesn't pay attention to the purity
        % of compound goals, so recompute the purity here.
        repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),

        map.det_update(PredTable0, PredId, !.PredInfo, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo)
    ).

:- pred search_goal_for_par_conj(module_info::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    instmap::in, instmap::out, hlds_goal::in, hlds_goal::out,
    io::di, io::uo) is det.

search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap0, InstMap,
        Goal0, Goal, !IO) :-
    search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap0,
        Goal0, Goal, !IO),
    update_instmap(Goal0, InstMap0, InstMap).

:- pred search_goal_for_par_conj_2(module_info::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    instmap::in, hlds_goal::in, hlds_goal::out, io::di, io::uo) is det.

search_goal_for_par_conj_2(_ModuleInfo, !VarSet, !VarTypes, _InstMap,
        G @ (Goal - _), G, !IO) :-
    Goal = unify(_, _, _, _Kind, _).
search_goal_for_par_conj_2(_ModuleInfo, !VarSet, !VarTypes, _InstMap,
        G @ (Goal - _), G, !IO) :-
    Goal = plain_call(_CallPredId, _CallProcId, _CallArgs, _, _, _).
search_goal_for_par_conj_2(_ModuleInfo, !VarSet, !VarTypes, _InstMap,
        G @ (Goal - _), G, !IO) :-
    Goal = generic_call(_Details, _Args, _ArgModes, _).
search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap,
        negation(Goal0) - GI, negation(Goal) - GI, !IO) :-
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap, _,
        Goal0, Goal, !IO).
search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Goal0 - GI, Goal - GI, !IO) :-
    Goal0 = scope(Reason, ScopeGoal0),
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap, _,
        ScopeGoal0, ScopeGoal, !IO),
    Goal = scope(Reason, ScopeGoal).
search_goal_for_par_conj_2(_ModuleInfo, !VarSet, !VarTypes, _InstMap,
        G @ (Goal - _), G, !IO) :-
    Goal = call_foreign_proc(_Attributes, _, _, _, _, _, _).
search_goal_for_par_conj_2(_, _,_, _,_, _, shorthand(_) - _, _, _,_) :-
    unexpected(this_file,
        "shorthand goal encountered during dependent parallel " ++
        "conjunction transformation.").
search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Goal0 - GI, Goal - GI, !IO) :-
    Goal0 = switch(Var, CanFail, Cases0),
    search_cases_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Cases0, Cases, !IO),
    Goal = switch(Var, CanFail, Cases).
search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap0,
        Goal0 - GI, Goal - GI, !IO) :-
    Goal0 = if_then_else(Quant, If0, Then0, Else0),
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes,
        InstMap0, InstMap1, If0, If, !IO),
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes,
        InstMap1, _InstMap2, Then0, Then, !IO),
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes,
        InstMap0, _InstMap3, Else0, Else, !IO),
    Goal = if_then_else(Quant, If, Then, Else).
search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Goal0 - GI, Goal - GI, !IO) :-
    Goal0 = disj(Goals0),
    search_goals_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Goals0, Goals, !IO),
    Goal = disj(Goals).
search_goal_for_par_conj_2(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Goal0 - GI, Goal, !IO) :-
    Goal0 = conj(ConjType, Goals0),
    (
        ConjType = plain_conj,
        search_goals_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
            Goals0, Goals, !IO),
        conj_list_to_goal(Goals, GI, Goal)
    ;
        ConjType = parallel_conj,
        maybe_transform_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
            Goals0, GI, Goal, !IO)
    ).

:- pred search_cases_for_par_conj(module_info::in, 
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    instmap::in, list(case)::in, list(case)::out, io::di, io::uo) is det.

search_cases_for_par_conj(_ModuleInfo, !VarSet, !VarTypes, _InstMap,
        [], [], !IO).
search_cases_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        [Case0 | Cases0], [Case | Cases], !IO) :-
    Case0 = case(Functor, Goal0),
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap, _,
        Goal0, Goal, !IO),
    Case = case(Functor, Goal),
    search_cases_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Cases0, Cases, !IO).

:- pred search_goals_for_par_conj(module_info::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    instmap::in, hlds_goals::in, hlds_goals::out, io::di, io::uo) is det.

search_goals_for_par_conj(_ModuleInfo, !VarSet, !VarTypes, _InstMap0,
        [], [], !IO).
search_goals_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap0,
        [Goal0 | Goals0], [Goal | Goals], !IO) :-
    search_goal_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap0, InstMap,
        Goal0, Goal, !IO),
    search_goals_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Goals0, Goals, !IO).

%-----------------------------------------------------------------------------%

    % We found a parallel conjunction.  We need to check there any dependencies
    % between the conjuncts and, if so, insert sychronisation primitives.
    %
:- pred maybe_transform_par_conj(module_info::in, prog_varset::in,
    prog_varset::out, vartypes::in, vartypes::out, instmap::in,
    hlds_goals::in, hlds_goal_info::in, hlds_goal::out, io::di, io::uo) is det.

maybe_transform_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Conjuncts0, GoalInfo, NewGoal, !IO) :-
    % Search subgoals for nested parallel conjunctions.
    search_goals_for_par_conj(ModuleInfo, !VarSet, !VarTypes, InstMap,
        Conjuncts0, Conjuncts, !IO),

    % Find the variables that are shared between conjuncts.
    find_shared_variables(ModuleInfo, InstMap, Conjuncts, SharedVars),

    % Dependent parallel conjunctions only supported on lowlevel C parallel
    % grades.  For other grades convert any dependent parallel conjunctions
    % into plain conjunctions. Independent parallel conjunctions can be left
    % as-is.
    (if
        set.empty(SharedVars)
    then
        par_conj_list_to_goal(Conjuncts, GoalInfo, NewGoal)
    else
        globals.io_get_target(Target, !IO),
        globals.io_lookup_bool_option(highlevel_code, HighLevelCode, !IO),
        globals.io_lookup_bool_option(parallel, Parallel, !IO),
        (if
            Target = target_c,
            HighLevelCode = no,
            Parallel = yes
        then
            transform_conjunction(ModuleInfo, SharedVars,
                Conjuncts, GoalInfo, NewGoal, InstMap, !VarSet, !VarTypes)
        else
            conj_list_to_goal(Conjuncts, GoalInfo, NewGoal)
        )
    ).

    % Transforming the parallel conjunction.
    %
    % As a simple first step, all we do is insert waits and signals directly
    % into the conjunction.  If a conjunct produces a shared variable we turn
    % that conjunct into a sequential conjunction:
    %
    %   prod(Y)  ==>  ( prod(Y), impure signal(PrY, Y) )
    %
    % If the conjunct consumes a shared variable we will turn that conjunct
    % into a sequential conjunction:
    %
    %   consume(Y)  ==>  ( wait(PrY, Y), consume(Y) )
    %
    % References to shared variables need to be renamed apart so that the
    % conjuncts only share futures.
    %
    % Example:
    %
    %    p(A, B, ABA) :-
    %        ( append(A, B, AB)
    %        & append(AB, A, ABA)
    %        ).
    %
    % becomes:
    %
    %   p(A, B, ABA) :-
    %       new_future(FutureAB),
    %       (
    %           append(A, B, AB_7),
    %           impure signal(FutureAB, AB_7)
    %       &
    %           wait(FutureAB, AB_10),
    %           append(AB_10, A, ABA)
    %       ).
    %
:- pred transform_conjunction(module_info::in, set(prog_var)::in,
    hlds_goals::in, hlds_goal_info::in, hlds_goal::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

transform_conjunction(ModuleInfo, SharedVars, Goals, GoalInfo, NewGoal,
        InstMap, !VarSet, !VarTypes) :-
    allocate_futures(ModuleInfo, SharedVars, !VarTypes, !VarSet,
        AllocateFutures, FutureMap),
    list.map_foldl3(transform_conjunct(ModuleInfo, SharedVars, FutureMap),
        Goals, NewGoals, InstMap, _, !VarSet, !VarTypes),
    Conj = AllocateFutures ++ [conj(parallel_conj, NewGoals) - GoalInfo],
    conj_list_to_goal(Conj, GoalInfo, NewGoal0),
    % Wrap a purity scope around the goal if purity would have been lessened 
    % by the addition of signal goals.
    goal_info_get_purity(GoalInfo, Purity),
    (if Purity = purity_impure then
        NewGoal = NewGoal0
    else
        Reason = promise_purity(dont_make_implicit_promises, Purity),
        NewGoal = scope(Reason, NewGoal0) - GoalInfo
    ).

:- pred transform_conjunct(module_info::in, set(prog_var)::in,
    future_map::in, hlds_goal::in, hlds_goal::out,
    instmap::in, instmap::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

transform_conjunct(ModuleInfo, SharedVars, FutureMap, Goal0, Goal,
        !InstMap, !VarSet, !VarTypes) :-
    goal_get_nonlocals(Goal0, Nonlocals),
    set.intersect(Nonlocals, SharedVars, Intersect),
    (if set.empty(Intersect) then
        Goal = Goal0,
        update_instmap(Goal, !InstMap)
    else
        Goal0 = (_ - GoalInfo0),
        goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),

        % Divide shared variables into those that are produced by this
        % conjunction, and those that are consumed.
        set.divide(produced_variable(ModuleInfo, !.InstMap, InstMapDelta0),
            Intersect, ProducedVars, ConsumedVars),

        % Rename references to consumed variables.  We let the producer keep
        % the original name in order to avoid having to rename references to
        % the original name following the parallel conjunction.
        create_variables(set.to_sorted_list(ConsumedVars),
            !.VarSet, !.VarTypes,
            !VarSet, !VarTypes, map.init, Renaming),
        rename_vars_in_goal(Renaming, Goal0, Goal1),

        % Make wait and signal goals.
        list.map(make_wait(ModuleInfo, FutureMap, Renaming),
            set.to_sorted_list(ConsumedVars), WaitGoals),
        list.map(make_signal(ModuleInfo, FutureMap),
            set.to_sorted_list(ProducedVars), SignalGoals),

        % Insert signal goals after the conjunct and waits before the conjunct.
        Goal1 = _ - GoalInfo,
        conj_list_to_goal(WaitGoals, GoalInfo, WaitGoalsConj),
        conjoin_goal_and_goal_list(Goal1, SignalGoals, Goal2),
        conjoin_goals(WaitGoalsConj, Goal2, Goal),

        % Use the goal_info from _before_ the renaming of variables to update
        % the instmap, as the rest of the conjuncts won't know anything about
        % the new names.
        update_instmap(Goal0, !InstMap)
    ).

    % Make a goal to wait on a future for a consumed variable to be produced.
    %
:- pred make_wait(module_info::in, future_map::in,
    prog_var_renaming::in, prog_var::in, hlds_goal::out) is det.

make_wait(ModuleInfo, FutureMap, Renaming, ConsumedVar, WaitGoal) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    map.lookup(Renaming, ConsumedVar, WaitVar),

    ModuleName = mercury_par_builtin_module,
    PredName = "wait",
    Args = [FutureVar, WaitVar],
    Features = [],
    InstMapSrc = [WaitVar - ground(shared, none)],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, predicate,
        only_mode, detism_det, purity_pure, Args, Features, InstMapSrc,
        ModuleInfo, Context, WaitGoal).

    % Make a goal to signal that a variable is produced.
    %
:- pred make_signal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::out) is det.

make_signal(ModuleInfo, FutureMap, ProducedVar, SignalGoal) :-
    map.lookup(FutureMap, ProducedVar, FutureVar),

    ModuleName = mercury_par_builtin_module,
    PredName = "signal",
    Args = [FutureVar, ProducedVar],
    Features = [],
    InstMapSrc = [],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, predicate,
        only_mode, detism_det, purity_impure, Args, Features, InstMapSrc,
        ModuleInfo, Context, SignalGoal).

    % Succeed if Var is a variable bound between InstMap and
    % InstMap+InstMapDelta.
    %
:- pred produced_variable(module_info::in, instmap::in, instmap_delta::in,
    prog_var::in) is semidet.

produced_variable(ModuleInfo, InstMap, InstMapDelta, Var) :-
    instmap.lookup_var(InstMap, Var, OldVarInst),
    inst_is_free(ModuleInfo, OldVarInst),
    instmap_delta_search_var(InstMapDelta, Var, VarInst),
    inst_is_bound(ModuleInfo, VarInst).

%-----------------------------------------------------------------------------%

    % If a variable is nonlocal to a conjunct, and appears in the
    % instmap_delta of a _different_ conjunct, then we say that variable is
    % shared.
    %
    % (1) A variable must be nonlocal to a conjunct if it is shared.
    % (2) If the variable does not appear in the instmap_delta
    %     of any of the conjuncts of the parallel conjunction
    %     then it could not have been further instantiated within
    %     by the conjunction as a whole.
    %
    % XXX this code is probably too complicated.  I think Thomas already had a
    % more elegant way to find the shared variables somewhere, using multisets.
    %
:- pred find_shared_variables(module_info::in, instmap::in, hlds_goals::in,
    set(prog_var)::out) is det.

find_shared_variables(ModuleInfo, InstMap, Goals, SharedVars) :-
    list.map2(get_nonlocals_and_instmaps, Goals, Nonlocals, InstMapDeltas),
    find_shared_variables_2(ModuleInfo, 0, Nonlocals, InstMap, InstMapDeltas,
        set.init, SharedVars).

:- pred get_nonlocals_and_instmaps(hlds_goal::in,
    set(prog_var)::out, instmap_delta::out) is det.

get_nonlocals_and_instmaps(_Goal - GoalInfo, Nonlocals, InstMapDelta) :-
    goal_info_get_nonlocals(GoalInfo, Nonlocals),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta).

:- pred find_shared_variables_2(module_info::in, int::in,
    list(set(prog_var))::in, instmap::in, list(instmap_delta)::in,
    set(prog_var)::in, set(prog_var)::out) is det.

find_shared_variables_2(_ModuleInfo, _ConjunctIndex,
        [], _InstMap, _InstMapDeltas, !SharedVars).
find_shared_variables_2(ModuleInfo, ConjunctIndex,
        [Nonlocals | MoreNonlocals], InstMap, InstMapDeltas, !SharedVars) :-
    det_delete_nth(ConjunctIndex, InstMapDeltas, InstMapDeltasB),
    % Keep only nonlocals which were not already bound at the start of the
    % parallel conjunction.
    Filter = (pred(Var::in) is semidet :-
        not (
            instmap.lookup_var(InstMap, Var, VarInst),
            inst_is_bound(ModuleInfo, VarInst)
        )
    ),
    set.filter(Filter, Nonlocals) = UnboundNonlocals,
    set.fold(find_changed_vars(ModuleInfo, InstMapDeltasB),
        UnboundNonlocals, !SharedVars),
    find_shared_variables_2(ModuleInfo, ConjunctIndex+1, MoreNonlocals,
        InstMap, InstMapDeltas, !SharedVars).

:- pred find_changed_vars(module_info::in, list(instmap_delta)::in,
    prog_var::in, set(prog_var)::in, set(prog_var)::out) is det.

find_changed_vars(ModuleInfo, InstMapDeltas, UnboundVar, !SharedVars) :-
    (if
        % Is the unbound nonlocal bound in one of the conjuncts?
        InstMapDelta `list.member` InstMapDeltas,
        instmap_delta_search_var(InstMapDelta, UnboundVar, Inst),
        inst_is_bound(ModuleInfo, Inst)
    then
        svset.insert(UnboundVar, !SharedVars)
    else
        true
    ).

%-----------------------------------------------------------------------------%

    % A map from a variable to the future object created for that variable.
    % i.e. for variable X with future F, when X is bound to a value then F is
    % signalled.  A consumer of X waits (blocks) on F until that happens.
    %
:- type future_map == map(prog_var, prog_var).

    % Make goals to allocate future objects for variables shared
    % between two parallel conjuncts (a producer and one or more consumers).
    %
:- pred allocate_futures(module_info::in, set(prog_var)::in,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    hlds_goals::out, future_map::out) is det.

allocate_futures(ModuleInfo, SharedVars, !VarTypes, !VarSet,
        AllocGoals, FutureMap) :-
    set.fold4(allocate_future(ModuleInfo), SharedVars,
        !VarTypes, !VarSet, [], AllocGoals, map.init, FutureMap).

:- pred allocate_future(module_info::in, prog_var::in,
    vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
    hlds_goals::in, hlds_goals::out, future_map::in, future_map::out) is det.

allocate_future(ModuleInfo, SharedVar, !VarTypes, !VarSet,
        !AllocGoals, !FutureMap) :-
    map.lookup(!.VarTypes, SharedVar, SharedVarType),
    make_future(ModuleInfo, SharedVarType, SharedVar, !VarTypes, !VarSet,
        AllocGoal, FutureVar),
    list.cons(AllocGoal, !AllocGoals),
    svmap.det_insert(SharedVar, FutureVar, !FutureMap).

:- pred make_future(module_info::in, mer_type::in, prog_var::in, vartypes::in,
    vartypes::out, prog_varset::in, prog_varset::out, hlds_goal::out,
    prog_var::out) is det.

make_future(ModuleInfo, SharedVarType, SharedVar, !VarTypes, !VarSet,
        AllocGoal, FutureVar) :-
    construct_future_type(SharedVarType, FutureType),
    varset.lookup_name(!.VarSet, SharedVar, SharedVarName),
    svvarset.new_named_var("Future" ++ SharedVarName, FutureVar, !VarSet),
    svmap.det_insert(FutureVar, FutureType, !VarTypes),

    ModuleName = mercury_par_builtin_module,
    PredName = "new_future",
    Args = [FutureVar],
    Features = [],
    InstMapSrc = [FutureVar - ground(shared, none)],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, predicate,
        only_mode, detism_det, purity_pure, Args, Features, InstMapSrc,
        ModuleInfo, Context, AllocGoal).

    % Construct type future(T) given type T.
    %
:- pred construct_future_type(mer_type::in, mer_type::out) is det.

construct_future_type(T, FutureT) :-
    Future = qualified(mercury_par_builtin_module, "future"),
    FutureCtor = type_ctor(Future, 1),
    construct_type(FutureCtor, [T], FutureT).

%-----------------------------------------------------------------------------%

:- pred det_delete_nth(int::in, list(T)::in, list(T)::out) is det.

det_delete_nth(N, List0, List) :-
    list.det_split_list(N, List0, Left, Right),
    List = Left ++ det_tail(Right).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "dep_par_conj.m".

%-----------------------------------------------------------------------------%
:- end_module dep_par_conj.
%-----------------------------------------------------------------------------%
