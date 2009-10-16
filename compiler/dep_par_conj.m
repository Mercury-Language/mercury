%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: dep_par_conj.m.
% Author: wangp.
%
% This module implements dependent parallel conjunction using a HLDS->HLDS
% transformation. The transformation has two main components: a synchronization
% transformation and a specialization transformation.
%
% 1 The synchronization transformation ensures that consumers do not access
%   shared variables before producers generate them. We do this by adding calls
%   to the synchronisation primitives defined in library/par_builtin.m.
%   In general, we make producers signal the availability of shared variables
%   as soon as possible and we make consumers wait for the shared variables
%   as late as possible.
%
% 2 The specialization transformation spots the need for and creates new
%   versions of procedures. If some shared variables in a parallel conjunction
%   are produced and/or consumed inside a call, we create a specialized version
%   of the called procedure that does the signalling of the produced variables
%   (as early as possible) and/or the waiting for the consumed variables (as
%   late as possible). In the absence of these specialized procedures, we would
%   have to assume that all calls consume their inputs immediately and generate
%   their outputs only when they return, which in many cases is an excessively
%   pessimistic assumption.
%
% To see how the synchronization transformation works, consider this example:
%
% p(A::in, B::in, C::out) :-
%   (
%       q(A, X),
%       r(X, Y)
%   )
%   &
%   (
%       s(B, W),
%       t(X, W, Z)
%   ),
%   C = X + Y + Z.
%
% The only variable shared between the parallel conjuncts is X, which is
% produced by the call to q and is used in the call to t. We transform this
% code to
%
% p(A::in, B::in, C::out) :-
%   promise_pure(
%       par_builtin.new_future(FutureX),
%       (
%           q(A, X),
%           impure par_builtin.signal(FutureX, X)
%           r(X, Y)
%       )
%       &
%       (
%           s(B, W),
%           par_builtin.wait(FutureX, X')
%           t(X', W, Z)
%       )
%   ),
%   C = X + Y + Z.
%
% For each shared variable, we create a new future variable, which serves as
% the conduit between the producer and the consumers both for synchronization
% and for the transmission of the shared variable's value. Note that we
% create a new, distinct name for each shared variable in each consumer,
% so that after the transformation, the only variables that occur in more than
% one conjunct of the parallel conjunction are the variables that were already
% ground before the parallel conjunction is entered (these include the future
% variables).
%
% The specialization transformation looks for calls preceded by a contiguous
% sequence of one or more calls to par_builtin.wait and/or followed by a
% contiguous sequence of one or more calls to par_builtin.signal. When it finds
% one, it (a) replaces the sequence with a call to a specialized version of
% the called procedure, a version which will do all the waits and/or signals
% internally, and (b) creates that specialized version of the called procedure,
% pushing the waits as late as possible and the signals as early as possible.
% For example,
%
%   wait(FutureX, X),
%   p(X, Y),
%   impure signal(FutureY, Y)
%
% would be transformed into:
%
%   Parallel__p(FutureX, FutureY),
%
% where the wait and signal calls are now in the body of Parallel__p.
%
% - The predicates and functions in this module whose names start with the
%   prefixes "sync_dep_par_conjs", "insert_wait_in" and "insert_signal_in"
%   implement the synchronization transformation
% - Those whose names start with "find_specialization_requests" or include
%   "specialization" implement part (a) of the specialization transformation.
% - Those whose names start with "add_requested_specialized" implement part (b)
%   of the specialization transformation.
%
% TODO:
% - reconsider when this pass is run; in particular par_builtin primitives
%   ought to be inlined
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.dep_par_conj.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Transform all the parallel conjunctions in the procedures of this module
    % according to the scheme shown above.
    %
:- pred impl_dep_par_conjs_in_module(module_info::in, module_info::out)
    is det.

    % Given the conjunct goals in a parallel conjunction and the instmap before
    % it, return the set of variables that need synchronization, i.e. the
    % variables that are produced in one conjunct and consumed in one or more
    % other conjuncts.
    %
    % This function is exported for use by the implicit_parallelism pass.
    %
:- func find_shared_variables(module_info, instmap, list(hlds_goal))
    = set(prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

impl_dep_par_conjs_in_module(!ModuleInfo) :-
    InitialModuleInfo = !.ModuleInfo,

    % Phase one: insert synchronization code into all parallel conjunctions
    % in the module.
    module_info_predids(PredIds, !ModuleInfo),
    list.foldl2(maybe_sync_dep_par_conjs_in_pred, PredIds,
        !ModuleInfo, [], ProcsToScan),

    % Phase two: attempt to push the synchronization code inside procedures
    % as far as we can. We do this by creating specialized versions of
    % procedures. We do this to a fixpoint, since creating a specialized
    % version of a procedure may require us to create more specialized versions
    % of the other procedures.

    DoneParProcs0 = map.init,
    PendingParProcs0 = [],
    Pushability0 = map.init,
    list.foldl3(
        find_specialization_requests_in_proc(DoneParProcs0, InitialModuleInfo),
        ProcsToScan, !ModuleInfo, PendingParProcs0, PendingParProcs,
        Pushability0, Pushability),
    add_requested_specialized_par_procs(PendingParProcs, Pushability,
        DoneParProcs0, InitialModuleInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The synchronization transformation.
%

    % This type holds information relevant to the synchronization
    % transformation.
    %
:- type sync_info
    --->    sync_info(
                % The current module. This field is read only.
                sync_module_info            :: module_info,

                % Variables which should not be replaced by futures in this
                % pass because it has already been done. This field is
                % read only.
                sync_ignore_vars            :: set(prog_var),

                % The value of the --allow-some-paths-only-waits option.
                % Read-only.
                sync_allow_some_paths_only  :: bool,

                % The varset and vartypes for the procedure being analysed.
                % These fields are updated when we add new variables.
                % XXX We may also need the rtti_var_maps.
                sync_varset                 :: prog_varset,
                sync_vartypes               :: vartypes
            ).

:- pred maybe_sync_dep_par_conjs_in_pred(pred_id::in,
    module_info::in, module_info::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

maybe_sync_dep_par_conjs_in_pred(PredId, !ModuleInfo, !ProcsToScan) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    list.foldl2(maybe_sync_dep_par_conjs_in_proc(PredId), ProcIds,
        !ModuleInfo, !ProcsToScan).

:- pred maybe_sync_dep_par_conjs_in_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

maybe_sync_dep_par_conjs_in_proc(PredId, ProcId, !ModuleInfo, !ProcsToScan) :-
    module_info_proc_info(!.ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    (
        HasParallelConj = no
    ;
        HasParallelConj = yes,
        sync_dep_par_conjs_in_proc(PredId, ProcId, set.init,
            !ModuleInfo, !ProcsToScan)
    ).

:- pred sync_dep_par_conjs_in_proc(pred_id::in, proc_id::in, set(prog_var)::in,
    module_info::in, module_info::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

sync_dep_par_conjs_in_proc(PredId, ProcId, IgnoreVars, !ModuleInfo,
        !ProcsToScan) :-
    some [!PredInfo, !ProcInfo, !Goal, !VarSet, !VarTypes, !SyncInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, !:Goal),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, allow_some_paths_only_waits,
            AllowSomePathsOnly),

        !:SyncInfo = sync_info(!.ModuleInfo, IgnoreVars, AllowSomePathsOnly,
            !.VarSet, !.VarTypes),
        sync_dep_par_conjs_in_goal(!Goal, InstMap0, _, !SyncInfo),
        !.SyncInfo = sync_info(_, _, _, !:VarSet, !:VarTypes),
        % XXX RTTI varmaps may need to be updated

        % We really only need to run this part if something changed, but we
        % only run this predicate on procedures which are likely to have
        % parallel conjunctions.
        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo),
        proc_info_set_goal(!.Goal, !ProcInfo),
        fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo,
            !ModuleInfo),
        PredProcId = proc(PredId, ProcId),
        !:ProcsToScan = [PredProcId | !.ProcsToScan]
    ).

    % Traverse the goal looking for dependent parallel conjunctions,
    % and insert code to synchronize the accesses of the various
    % parallel conjuncts to the variables they share.
    %
:- pred sync_dep_par_conjs_in_goal(hlds_goal::in, hlds_goal::out,
    instmap::in, instmap::out, sync_info::in, sync_info::out)
    is det.

sync_dep_par_conjs_in_goal(Goal0, Goal, InstMap0, InstMap, !SyncInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        sync_dep_par_conjs_in_conj(Goals0, Goals, InstMap0, !SyncInfo),
        (
            ConjType = plain_conj,
            conj_list_to_goal(Goals, GoalInfo0, Goal)
        ;
            ConjType = parallel_conj,
            maybe_sync_dep_par_conj(Goals, GoalInfo0, Goal, InstMap0,
                !SyncInfo)
        )
    ;
        GoalExpr0 = disj(Goals0),
        sync_dep_par_conjs_in_disj(Goals0, Goals, InstMap0, !SyncInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        sync_dep_par_conjs_in_cases(Cases0, Cases, InstMap0, !SyncInfo),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(QuantVars, Cond0, Then0, Else0),
        sync_dep_par_conjs_in_goal(Cond0, Cond, InstMap0, InstMap1,
            !SyncInfo),
        sync_dep_par_conjs_in_goal(Then0, Then, InstMap1, _, !SyncInfo),
        sync_dep_par_conjs_in_goal(Else0, Else, InstMap0, _, !SyncInfo),
        GoalExpr = if_then_else(QuantVars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        sync_dep_par_conjs_in_goal(SubGoal0, SubGoal, InstMap0, _,
            !SyncInfo),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            Goal = Goal0
        ;
            sync_dep_par_conjs_in_goal(SubGoal0, SubGoal, InstMap0, _,
                !SyncInfo),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "sync_dep_par_conjs_in_goal: shorthand")
    ),
    update_instmap(Goal, InstMap0, InstMap).

:- pred sync_dep_par_conjs_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, sync_info::in, sync_info::out) is det.

sync_dep_par_conjs_in_conj([], [], _, !SyncInfo).
sync_dep_par_conjs_in_conj([Goal0 | Goals0], [Goal | Goals], !.InstMap,
        !SyncInfo) :-
    sync_dep_par_conjs_in_goal(Goal0, Goal, !InstMap, !SyncInfo),
    sync_dep_par_conjs_in_conj(Goals0, Goals, !.InstMap, !SyncInfo).

:- pred sync_dep_par_conjs_in_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    instmap::in, sync_info::in, sync_info::out) is det.

sync_dep_par_conjs_in_disj([], [], _InstMap0, !SyncInfo).
sync_dep_par_conjs_in_disj([Goal0 | Goals0], [Goal | Goals], InstMap0,
        !SyncInfo) :-
    sync_dep_par_conjs_in_goal(Goal0, Goal, InstMap0, _InstMap, !SyncInfo),
    sync_dep_par_conjs_in_disj(Goals0, Goals, InstMap0, !SyncInfo).

:- pred sync_dep_par_conjs_in_cases(list(case)::in, list(case)::out,
    instmap::in, sync_info::in, sync_info::out) is det.

sync_dep_par_conjs_in_cases([], [], _InstMap0, !SyncInfo).
sync_dep_par_conjs_in_cases([Case0 | Cases0], [Case | Cases], InstMap0,
        !SyncInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    sync_dep_par_conjs_in_goal(Goal0, Goal, InstMap0, _, !SyncInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    sync_dep_par_conjs_in_cases(Cases0, Cases, InstMap0, !SyncInfo).

%-----------------------------------------------------------------------------%

    % We found a parallel conjunction. Check for any dependencies between
    % the conjuncts and, if we find some, insert sychronisation primitives.
    %
:- pred maybe_sync_dep_par_conj(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out, instmap::in, sync_info::in, sync_info::out)
    is det.

maybe_sync_dep_par_conj(Conjuncts, GoalInfo, NewGoal, InstMap, !SyncInfo) :-
    !.SyncInfo = sync_info(ModuleInfo, IgnoreVars, AllowSomePathsOnly,
        VarSet0, VarTypes0),
    % Find the variables that are shared between conjuncts.
    SharedVars0 = find_shared_variables(ModuleInfo, InstMap, Conjuncts),

    % Filter out all the variables which have already have associated futures,
    % i.e. they were head variables which were replaced by futures; signal and
    % wait calls will already have been inserted for them.
    SharedVars = set.filter(isnt(set.contains(IgnoreVars)), SharedVars0),

    ( set.empty(SharedVars) ->
        % Independent parallel conjunctions need no transformation.
        par_conj_list_to_goal(Conjuncts, GoalInfo, NewGoal)
    ;
        sync_dep_par_conj(ModuleInfo, AllowSomePathsOnly, SharedVars,
            Conjuncts, GoalInfo, NewGoal, InstMap,
            VarSet0, VarSet, VarTypes0, VarTypes),
        !:SyncInfo = sync_info(ModuleInfo, IgnoreVars, AllowSomePathsOnly,
            VarSet, VarTypes)
    ).

    % Transforming the parallel conjunction.
    %
    % We insert waits as deeply into the conjunction as possible, and signals
    % as early as possible.
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
:- pred sync_dep_par_conj(module_info::in, bool::in, set(prog_var)::in,
    list(hlds_goal)::in, hlds_goal_info::in, hlds_goal::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

sync_dep_par_conj(ModuleInfo, AllowSomePathsOnly, SharedVars, Goals, GoalInfo,
        NewGoal, InstMap, !VarSet, !VarTypes) :-
    SharedVarsList = set.to_sorted_list(SharedVars),
    list.map_foldl3(allocate_future(ModuleInfo), SharedVarsList,
        AllocateFutures, !VarSet, !VarTypes, map.init, FutureMap),
    list.map_foldl3(
        sync_dep_par_conjunct(ModuleInfo, AllowSomePathsOnly,
            par_conjunct_is_in_conjunction, SharedVars, FutureMap),
        Goals, NewGoals, InstMap, _, !VarSet, !VarTypes),

    LastGoal = hlds_goal(conj(parallel_conj, NewGoals), GoalInfo),
    Conj = AllocateFutures ++ [LastGoal],
    conj_list_to_goal(Conj, GoalInfo, NewGoal0),

    % Wrap a purity scope around the goal if purity would have been lessened
    % by the addition of signal goals (which are impure) or calls to
    % parallelised procs (which may be impure).
    Purity = goal_info_get_purity(GoalInfo),
    (
        Purity = purity_impure,
        NewGoal = NewGoal0
    ;
        ( Purity = purity_pure
        ; Purity = purity_semipure
        ),
        Reason = promise_purity(Purity),
        NewGoal = hlds_goal(scope(Reason, NewGoal0), GoalInfo)
    ).

:- type par_conjunct_status
    --->    par_conjunct_is_in_conjunction
    ;       par_conjunct_is_proc_body.

:- pred sync_dep_par_conjunct(module_info::in, bool::in,
    par_conjunct_status::in, set(prog_var)::in, future_map::in,
    hlds_goal::in, hlds_goal::out, instmap::in, instmap::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

sync_dep_par_conjunct(ModuleInfo, AllowSomePathsOnly, ParConjunctStatus,
        SharedVars, FutureMap, !Goal, !InstMap, !VarSet, !VarTypes) :-
    Nonlocals = goal_get_nonlocals(!.Goal),
    set.intersect(Nonlocals, SharedVars, NonlocalSharedVars),
    ( set.empty(NonlocalSharedVars) ->
        true
    ;
        GoalInfo0 = !.Goal ^ hlds_goal_info,
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),

        % Divide the shared variables into those that are produced by this
        % conjunct, and those that are consumed by it.
        % XXX We should check that the initial instantiation of each variable
        % in ProducedVars in !.InstMap is free. However, at the moment, there
        % is nothing useful we can do if it isn't.
        IsProducedVar = var_is_bound_in_instmap_delta(ModuleInfo,
            !.InstMap, InstMapDelta0),
        set.divide(IsProducedVar, NonlocalSharedVars,
            ProducedVars, ConsumedVars),
        ConsumedVarsList = set.to_sorted_list(ConsumedVars),
        ProducedVarsList = set.to_sorted_list(ProducedVars),

        % Insert waits into the conjunct, as late as possible.
        list.map_foldl3(
            insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                FutureMap),
            ConsumedVarsList, _WaitedOnAllSuccessPaths,
            !Goal, !VarSet, !VarTypes),

        % Insert signals into the conjunct, as early as possible.
        list.foldl3(insert_signal_in_goal(ModuleInfo, FutureMap),
            ProducedVarsList, !Goal, !VarSet, !VarTypes),

        % Each consumer will have its own local name for the consumed variable,
        % so they can each wait for it when they need to.
        (
            ParConjunctStatus = par_conjunct_is_in_conjunction,
            clone_variables(ConsumedVarsList, !.VarSet, !.VarTypes,
                !VarSet, !VarTypes, map.init, Renaming),
            rename_some_vars_in_goal(Renaming, !Goal)
        ;
            ParConjunctStatus = par_conjunct_is_proc_body
            % We don't need to rename anything.
        )
    ),
    update_instmap(!.Goal, !InstMap).

%-----------------------------------------------------------------------------%

:- type waited_on_all_success_paths
    --->    waited_on_all_success_paths
    ;       not_waited_on_all_success_paths.

:- pred join_branches(waited_on_all_success_paths::in,
    waited_on_all_success_paths::in, waited_on_all_success_paths::out) is det.

join_branches(waited_on_all_success_paths, waited_on_all_success_paths,
    waited_on_all_success_paths).
join_branches(waited_on_all_success_paths, not_waited_on_all_success_paths,
    not_waited_on_all_success_paths).
join_branches(not_waited_on_all_success_paths, waited_on_all_success_paths,
    not_waited_on_all_success_paths).
join_branches(not_waited_on_all_success_paths, not_waited_on_all_success_paths,
    not_waited_on_all_success_paths).

    % insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar, Goal0, Goal,
    %   !VarSet, !VarTypes):
    %
    % Insert a wait on the future version of ConsumedVar *just before*
    % the first reference to it inside Goal0. If there is no reference to
    % ConsumedVar inside Goal0, then insert a wait for ConsumedVar at the
    % end of Goal0 (unless Goal0 cannot succeed, in which case the wait
    % would never be reached.
    %
    % Call this predicate if either (a) Goal0 consumes ConsumedVar, or (b)
    % some other goal that Goal0 is parallel to consumes ConsumedVar.
    % (We must ensure that if one branch of a branched goal inserts a wait
    % for a variable, then *all* branches of that goal insert a wait.)
    %
:- pred insert_wait_in_goal(module_info::in, bool::in, future_map::in,
    prog_var::in, waited_on_all_success_paths::out,
    hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        WaitedOnAllSuccessPaths, Goal0, Goal, !VarSet, !VarTypes) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    % InvariantEstablished should be true if AllowSomePathsOnly = no
    % implies WaitedOnAllSuccessPaths0 = waited_on_all_success_paths.
    ( var_in_nonlocals(ConsumedVar, Goal0) ->
        (
            GoalExpr0 = conj(ConjType, Goals0),
            InvariantEstablished = yes,
            (
                ConjType = plain_conj,
                insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar, WaitedOnAllSuccessPaths0,
                    Goals0, Goals, !VarSet, !VarTypes)
            ;
                ConjType = parallel_conj,
                insert_wait_in_par_conj(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    have_not_waited_in_conjunct, WaitedInConjunct,
                    Goals0, Goals, !VarSet, !VarTypes),
                (
                    WaitedInConjunct = have_not_waited_in_conjunct,
                    WaitedOnAllSuccessPaths0 = not_waited_on_all_success_paths
                ;
                    WaitedInConjunct =
                        waited_in_conjunct(WaitedOnAllSuccessPaths0)
                )
            ),
            GoalExpr = conj(ConjType, Goals),
            Goal1 = hlds_goal(GoalExpr, GoalInfo0)
        ;
            GoalExpr0 = disj(Disjuncts0),
            InvariantEstablished = yes,
            (
                Disjuncts0 = [],
                % This path ends in failure.
                WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
                Goal1 = Goal0
            ;
                Disjuncts0 = [FirstDisjunct0 | LaterDisjuncts0],
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    FirstWaitedOnAllSuccessPaths,
                    FirstDisjunct0, FirstDisjunct, !VarSet, !VarTypes),
                insert_wait_in_disj(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    FirstWaitedOnAllSuccessPaths, WaitedOnAllSuccessPaths0,
                    LaterDisjuncts0, LaterDisjuncts, !VarSet, !VarTypes),
                Disjuncts = [FirstDisjunct | LaterDisjuncts],
                GoalExpr = disj(Disjuncts),
                Goal1 = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
            InvariantEstablished = yes,
            ( ConsumedVar = SwitchVar ->
                insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Goal0, Goal1, !VarSet, !VarTypes),
                WaitedOnAllSuccessPaths0 = waited_on_all_success_paths
            ;
                (
                    Cases0 = [],
                    unexpected(this_file, "insert_wait_in_goal: no cases")
                ;
                    Cases0 = [FirstCase0 | LaterCases0],
                    FirstCase0 = case(MainConsId, OtherConsIds, FirstGoal0),
                    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                        FutureMap, ConsumedVar,
                        FirstWaitedOnAllSuccessPaths,
                        FirstGoal0, FirstGoal, !VarSet, !VarTypes),
                    FirstCase = case(MainConsId, OtherConsIds, FirstGoal),
                    insert_wait_in_cases(ModuleInfo, AllowSomePathsOnly,
                        FutureMap, ConsumedVar,
                        FirstWaitedOnAllSuccessPaths, WaitedOnAllSuccessPaths0,
                        LaterCases0, LaterCases, !VarSet, !VarTypes),
                    Cases = [FirstCase | LaterCases],
                    GoalExpr = switch(SwitchVar, CanFail, Cases),
                    Goal1 = hlds_goal(GoalExpr, GoalInfo0)
                )
            )
        ;
            GoalExpr0 = if_then_else(Quant, Cond, Then0, Else0),
            InvariantEstablished = yes,
            ( var_in_nonlocals(ConsumedVar, Cond) ->
                % XXX We could try to wait for the shared variable only when
                % the condition needs it. This would require also waiting
                % for the shared variable somewhere in the else branch.
                % However, the compiler requires that the conditions of
                % if-then-elses bind no variable that is accessible from
                % anywhere except the condition and the then-part of that
                % if-then-else, so we would have to do tricks like renaming
                % the variable waited-for by the condition, and then assigning
                % the renamed variable to its original name in the then-part.
                WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
                insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Goal0, Goal1, !VarSet, !VarTypes)
            ;
                % If ConsumedVar is not in the nonlocals of Cond, then it
                % must be in the nonlocals of at least one of Then0 and Else0.
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    ThenWaitedOnAllSuccessPaths,
                    Then0, Then, !VarSet, !VarTypes),
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    ElseWaitedOnAllSuccessPaths,
                    Else0, Else, !VarSet, !VarTypes),
                join_branches(ThenWaitedOnAllSuccessPaths,
                    ElseWaitedOnAllSuccessPaths, WaitedOnAllSuccessPaths0),
                GoalExpr = if_then_else(Quant, Cond, Then, Else),
                Goal1 = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            InvariantEstablished = yes,
            ( Reason = from_ground_term(_, from_ground_term_construct) ->
                % These scopes do not consume anything.
                unexpected(this_file,
                    "insert_wait_in_goal: from_ground_term_construct")
            ;
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar, WaitedOnAllSuccessPaths0,
                    SubGoal0, SubGoal, !VarSet, !VarTypes),
                GoalExpr = scope(Reason, SubGoal),
                Goal1 = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = negation(_SubGoal0),
            InvariantEstablished = yes,
            % We treat the negated goal just as we treat the condition of
            % an if-then-else.
            WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal1, !VarSet, !VarTypes)
        ;
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            InvariantEstablished = no,
            WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal1, !VarSet, !VarTypes)
        ;
            GoalExpr0 = shorthand(_),
            unexpected(this_file, "insert_wait_in_goal: shorthand")
        ),
        (
            WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
            Goal2 = Goal1
        ;
            WaitedOnAllSuccessPaths0 = not_waited_on_all_success_paths,
            % Some code in this goal may wait on ConsumedVar, and some code
            % in later conjoined goals may wait on ConsumedVar. We must
            % therefore ensure that the wait operations instantiate different
            % variables. We do so by renaming any occurrences of ConsumedVar
            % in this goal.
            % so we shouldn't update the argument of waited_in_conjunct.
            clone_variable(ConsumedVar, !.VarSet, !.VarTypes,
                !VarSet, !VarTypes, map.init, Renaming, _CloneVar),
            rename_some_vars_in_goal(Renaming, Goal1, Goal2)
        )
    ;
        InvariantEstablished = no,
        WaitedOnAllSuccessPaths0 = not_waited_on_all_success_paths,
        Goal2 = Goal0
    ),
    Detism = goal_info_get_determinism(GoalInfo0),
    determinism_components(Detism, _, MaxSolns),
    (
        MaxSolns = at_most_zero,
        WaitedOnAllSuccessPaths = waited_on_all_success_paths,
        Goal = Goal2
    ;
        ( MaxSolns = at_most_one
        ; MaxSolns = at_most_many
        ; MaxSolns = at_most_many_cc
        ),
        (
            WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
            WaitedOnAllSuccessPaths = WaitedOnAllSuccessPaths0,
            Goal = Goal2
        ;
            WaitedOnAllSuccessPaths0 = not_waited_on_all_success_paths,
            (
                AllowSomePathsOnly = yes,
                WaitedOnAllSuccessPaths = WaitedOnAllSuccessPaths0,
                Goal = Goal2
            ;
                AllowSomePathsOnly = no,
                (
                    InvariantEstablished = no,
                    WaitedOnAllSuccessPaths = waited_on_all_success_paths,
                    insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
                        Goal2, Goal, !VarSet, !VarTypes)
                ;
                    InvariantEstablished = yes,
                    unexpected(this_file, "insert_wait_in_goal: " ++
                        "not_waited_on_all_success_paths invariant violation")
                )
            )
        )
    ).

:- pred insert_wait_before_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    make_wait_goal(ModuleInfo, !.VarTypes, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hlds_goal_info, WaitGoal, Goal0,
        Goal).

:- pred insert_wait_after_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    make_wait_goal(ModuleInfo, !.VarTypes, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hlds_goal_info, Goal0, WaitGoal,
        Goal).

    % Insert a wait for ConsumedVar in the first goal in the conjunction
    % that references it. Any later conjuncts will get the waited-for variable
    % without having to call wait.
    %
:- pred insert_wait_in_plain_conj(module_info::in, bool::in, future_map::in,
    prog_var::in, waited_on_all_success_paths::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_plain_conj(_, _, _, _,
        not_waited_on_all_success_paths, [], [], !VarSet, !VarTypes).
insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, WaitedOnAllSuccessPaths,
        [FirstGoal0 | LaterGoals0], Goals, !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ConsumedVar, FirstGoal0) ->
        insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
            FutureMap, ConsumedVar, GoalWaitedOnAllSuccessPaths,
            FirstGoal0, FirstGoal, !VarSet, !VarTypes),
        (
            GoalWaitedOnAllSuccessPaths = waited_on_all_success_paths,
            % We wait for ConsumedVar on all paths FirstGoal1 that can lead
            % to LaterGoals0, so the code in LaterGoals0 will be able to
            % access ConsumedVar without any further waiting.
            WaitedOnAllSuccessPaths = waited_on_all_success_paths,
            LaterGoals = LaterGoals0
        ;
            GoalWaitedOnAllSuccessPaths = not_waited_on_all_success_paths,
            % We waited for ConsumedVar on some but not all paths in FirstGoal0
            % that can lead to LaterGoals0. LaterGoals may therefore also wait
            % for ConsumedVar, and any such waits will also bind ConsumedVar. 
            % We do not want both FirstGoal and LaterGoals binding ConsumedVar,
            % so the call to insert_wait_in_goal above has replaced all
            % occurrences of ConsumedVar in FirstGoal with a fresh variable.
            insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly,
                FutureMap, ConsumedVar, WaitedOnAllSuccessPaths,
                LaterGoals0, LaterGoals, !VarSet, !VarTypes)
        ),
        ( FirstGoal ^ hlds_goal_expr = conj(plain_conj, FirstGoalConj) ->
            Goals = FirstGoalConj ++ LaterGoals
        ;
            Goals = [FirstGoal | LaterGoals]
        )
    ;
        % ConsumedVar does not appear in FirstGoal0, so wait for it
        % in LaterGoals0.
        insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly, FutureMap,
            ConsumedVar, WaitedOnAllSuccessPaths, LaterGoals0, LaterGoals1,
            !VarSet, !VarTypes),
        Goals = [FirstGoal0 | LaterGoals1]
    ).

    % Have we inserted waits into any one of the parallel conjuncts yet?
    % If yes, say whether the first such conjunct (the one in which we
    % do *not* rename the waited for instance of ConsumedVar) waits for
    % ConsumedVar on all success paths.
    %
:- type waited_in_conjunct
    --->    waited_in_conjunct(waited_on_all_success_paths)
    ;       have_not_waited_in_conjunct.

    % Insert a wait for ConsumedVar in the *every* goal in the conjunction
    % that references it. "Later" conjuncts cannot get the variable that
    % "earlier" conjuncts waited for, since those waits may not have finished
    % yet.
    %
:- pred insert_wait_in_par_conj(module_info::in, bool::in,
    future_map::in, prog_var::in,
    waited_in_conjunct::in, waited_in_conjunct::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_par_conj(_, _, _, _,
        !WaitedInConjunct, [], [], !VarSet, !VarTypes).
insert_wait_in_par_conj(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedInConjunct, [Goal0 | Goals0], [Goal | Goals],
        !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ConsumedVar, Goal0) ->
        % ConsumedVar appears in Goal0, so wait for it in Goal0, but the code
        % in Goals0 will *not* be able to access ConsumedVar without waiting,
        % since the conjuncts are executed independently.
        insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap,
            ConsumedVar, WaitedOnAllSuccessPaths, Goal0, Goal1,
            !VarSet, !VarTypes),
        (
            !.WaitedInConjunct = have_not_waited_in_conjunct,
            !:WaitedInConjunct = waited_in_conjunct(WaitedOnAllSuccessPaths),
            Goal = Goal1
        ;
            !.WaitedInConjunct = waited_in_conjunct(_),
            % This is not the first conjunct that waits for ConsumedVar,
            % so we shouldn't update the argument of waited_in_conjunct.
            clone_variable(ConsumedVar, !.VarSet, !.VarTypes,
                !VarSet, !VarTypes, map.init, Renaming, _CloneVar),
            rename_some_vars_in_goal(Renaming, Goal1, Goal)
        )
    ;
        Goal = Goal0
    ),
    insert_wait_in_par_conj(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, !WaitedInConjunct, Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_wait_in_disj(module_info::in, bool::in,
    future_map::in, prog_var::in,
    waited_on_all_success_paths::in, waited_on_all_success_paths::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_disj(_, _, _, _,
        !WaitedOnAllSuccessPaths, [], [], !VarSet, !VarTypes).
insert_wait_in_disj(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, [Goal0 | Goals0], [Goal | Goals],
        !VarSet, !VarTypes) :-
    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        FirstWaitedOnAllSuccessPaths,
        Goal0, Goal, !VarSet, !VarTypes),
    join_branches(FirstWaitedOnAllSuccessPaths, !WaitedOnAllSuccessPaths),
    insert_wait_in_disj(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_wait_in_cases(module_info::in, bool::in,
    future_map::in, prog_var::in,
    waited_on_all_success_paths::in, waited_on_all_success_paths::out,
    list(case)::in, list(case)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_cases(_, _, _, _,
        !WaitedOnAllSuccessPaths, [], [], !VarSet, !VarTypes).
insert_wait_in_cases(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, [Case0 | Cases0], [Case | Cases],
        !VarSet, !VarTypes) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        FirstWaitedOnAllSuccessPaths, Goal0, Goal, !VarSet, !VarTypes),
    Case = case(MainConsId, OtherConsIds, Goal),
    join_branches(FirstWaitedOnAllSuccessPaths, !WaitedOnAllSuccessPaths),
    insert_wait_in_cases(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, Cases0, Cases, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%

    % Look for the first instance of the produced variable down every
    % branch. The first goal referring to the variable must produce it,
    % so insert a signal call right after that goal.
    %
    % XXX This assumption won't *necessarily* be correct after we start
    % supporting partially instantiated data structures. The first occurrence
    % of ProducedVar may instantiate it partially, with a second or later
    % occurrence instantiating it to ground. We want to execute the signal
    % only when ProducedVar is ground.
    %
:- pred insert_signal_in_goal(module_info::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( var_in_nonlocals(ProducedVar, Goal0) ->
        (
            GoalExpr0 = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                insert_signal_in_plain_conj(ModuleInfo, FutureMap, ProducedVar,
                    Goals0, Goals, !VarSet, !VarTypes)
            ;
                ConjType = parallel_conj,
                insert_signal_in_par_conj(ModuleInfo, FutureMap, ProducedVar,
                    Goals0, Goals, !VarSet, !VarTypes)
            ),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            GoalExpr0 = disj(Goals0),
            insert_signal_in_disj(ModuleInfo, FutureMap, ProducedVar,
                Goals0, Goals, !VarSet, !VarTypes),
            GoalExpr = disj(Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
            ( ProducedVar = SwitchVar ->
                unexpected(this_file, "switch on unbound shared variable")
            ;
                insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
                    Cases0, Cases, !VarSet, !VarTypes),
                GoalExpr = switch(SwitchVar, CanFail, Cases),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = if_then_else(QuantVars, Cond, Then0, Else0),
            expect(var_not_in_nonlocals(ProducedVar, Cond),
                this_file, "condition binds shared variable"),
            insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
                Then0, Then, !VarSet, !VarTypes),
            insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
                Else0, Else, !VarSet, !VarTypes),
            GoalExpr = if_then_else(QuantVars, Cond, Then, Else),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            GoalExpr0 = negation(_),
            unexpected(this_file, "negation binds shared variable")
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            ( Reason = from_ground_term(_, from_ground_term_construct) ->
                % Pushing the signal into the scope would invalidate the
                % invariant that from_ground_term_construct scopes do nothing
                % except construct a ground term. It would also be pointless,
                % since the code generator will turn the entire scope into a
                % single assignment statement. We therefore put he signal
                % *after* the scope.
                insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
                    Goal0, Goal, !VarSet, !VarTypes)
            ;
                SubGoal0 = hlds_goal(_, SubGoalInfo0),
                Detism0 = goal_info_get_determinism(GoalInfo0),
                SubDetism0 = goal_info_get_determinism(SubGoalInfo0),
                determinism_components(Detism0, _, MaxSolns0),
                determinism_components(SubDetism0, _, SubMaxSolns0),
                (
                    SubMaxSolns0 = at_most_many,
                    MaxSolns0 \= at_most_many
                ->
                    % The value of ProducedVar is not stable inside SubGoal0,
                    % i.e. SubGoal0 can generate a value for ProducedVar and
                    % then backtrack over the goal that generated it. In such
                    % cases, we can signal the availability of ProducedVar
                    % only when it has become stable, which is when the scope
                    % has cut away any possibility of further backtracking
                    % inside SubGoal0.
                    insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
                        Goal0, Goal, !VarSet, !VarTypes)
                ;
                    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
                        SubGoal0, SubGoal, !VarSet, !VarTypes),
                    GoalExpr = scope(Reason, SubGoal),
                    Goal = hlds_goal(GoalExpr, GoalInfo0)
                )
            )
        ;
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
                Goal0, Goal, !VarSet, !VarTypes)
        ;
            GoalExpr0 = shorthand(_),
            unexpected(this_file, "insert_signal_in_goal: shorthand")
        )
    ;
        % When inserting waits into a goal, it is ok for the goal not to
        % mention the consumed variable, but when inserting signals into a
        % goal, the goal must produce the variable if it succeeds, so if
        % the goal does not mention the variable, it cannot succeed.
        Detism = goal_info_get_determinism(GoalInfo0),
        determinism_components(Detism, _CanFail, SolnCount),
        expect(unify(SolnCount, at_most_zero), this_file,
            "insert_signal_in_goal: ProducedVar is not in nonlocals"),

        % There would be no point in adding a signal to the end of Goal0,
        % since execution cannot get there.
        Goal = Goal0
    ).

:- pred insert_signal_after_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    make_signal_goal(ModuleInfo, FutureMap, ProducedVar, !.VarTypes,
        SignalGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hlds_goal_info, Goal0, SignalGoal,
        Goal).

:- pred insert_signal_in_plain_conj(module_info::in, future_map::in,
    prog_var::in, list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_plain_conj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarSet, !VarTypes).
insert_signal_in_plain_conj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], Goals, !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ProducedVar, Goal0) ->
        % The first conjunct that mentions ProducedVar should bind ProducedVar.
        % Since we don't recurse in this case, we get here only for the first
        % conjunct.
        Goal0 = hlds_goal(_, GoalInfo0),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        expect(set.contains(ChangedVars, ProducedVar), this_file,
            "insert_signal_in_plain_conj: ProducedVar not in ChangedVars"),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal1, !VarSet, !VarTypes),
        ( Goal1 ^ hlds_goal_expr = conj(plain_conj, GoalConjs1) ->
            Goals = GoalConjs1 ++ Goals0
        ;
            Goals = [Goal1 | Goals0]
        )
    ;
        insert_signal_in_plain_conj(ModuleInfo, FutureMap, ProducedVar,
            Goals0, Goals1, !VarSet, !VarTypes),
        Goals = [Goal0 | Goals1]
    ).

:- pred insert_signal_in_par_conj(module_info::in, future_map::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_par_conj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarSet, !VarTypes).
insert_signal_in_par_conj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ProducedVar, Goal0) ->
        % The first conjunct that mentions ProducedVar should bind ProducedVar.
        % Since we don't recurse in this case, we get here only for the first
        % conjunct.
        Goal0 = hlds_goal(_, GoalInfo0),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        expect(set.contains(ChangedVars, ProducedVar), this_file,
            "insert_signal_in_plain_conj: ProducedVar not in ChangedVars"),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal, !VarSet, !VarTypes),
        Goals = Goals0
    ;
        Goal = Goal0,
        insert_signal_in_par_conj(ModuleInfo, FutureMap, ProducedVar,
            Goals0, Goals, !VarSet, !VarTypes)
    ).

:- pred insert_signal_in_disj(module_info::in, future_map::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_disj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarSet, !VarTypes).
insert_signal_in_disj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes) :-
    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    insert_signal_in_disj(ModuleInfo, FutureMap, ProducedVar,
        Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_signal_in_cases(module_info::in, future_map::in, prog_var::in,
    list(case)::in, list(case)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_signal_in_cases(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarSet, !VarTypes).
insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
        [Case0 | Cases0], [Case | Cases], !VarSet, !VarTypes) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    Case = case(MainConsId, OtherConsIds, Goal),
    insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
        Cases0, Cases, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% The specialization transformation.
%

    % This type holds information relevant to the specialization
    % transformation.
    %
:- type spec_info
    --->    spec_info(
                % The set of parallelised procedures that we have already
                % created. This field is constant: it should never be updated.
                % (The set of done procs is updated only between the lifetimes
                % of values of this type.)
                spec_done_procs             :: done_par_procs,

                % The version of the module before dep_par_conj.m started
                % modifying it. This field is constant; it should never be
                % updated.
                spec_initial_module         :: module_info,

                % The variable types of the procedure we are scanning.
                % This field is constant; it should never be updated.
                spec_vartypes               :: vartypes,

                % The current module. Updated when requesting a new
                % specialization, since to get the pred_id for the specialized
                % predicate we need to update the module_info.
                spec_module_info            :: module_info,

                % Parallelised procedures waiting to be added. Updated when
                % requesting a new specialization.
                spec_pending_procs          :: pending_par_procs,

                spec_pushability            :: pushable_args_map
            ).

    % Parallelised procedures that have been added to the module already.
    % The calling pattern is the original pred_proc_id of the procedure
    % being called, plus the list of arguments which have been replaced
    % by futures.
    %
:- type done_par_procs == map(par_proc_call_pattern, new_par_proc).

    % Parallelised procedures that are scheduled to be added.
    % One or more procedures in the module will already be making calls
    % to the scheduled procedure.
    %
:- type pending_par_procs == assoc_list(par_proc_call_pattern, new_par_proc).

:- type par_proc_call_pattern
    --->    par_proc_call_pattern(
                old_ppid        :: pred_proc_id,
                future_args     :: list(arg_pos)
            ).

:- type new_par_proc
    --->    new_par_proc(
                new_ppid        :: pred_proc_id,
                new_name        :: sym_name
            ).

:- type arg_pos == int.

    % For each procedure we have looked at pushing wait and/or signal
    % operations into to create a specialized version, record which arguments
    % we know are worth pushing into the procedure, and which we know are *not*
    % worth pushing into the procedure (because the input is needed
    % immediately, or because an output is generated only at the very end).
    %
    % If a procedure does not appear in this map, it means it has not been
    % looked at before.
    %
:- type pushable_args_map == map(pred_proc_id, proc_pushable_args_map).

    % If an argument position does not appear in this map, it means no call
    % has tried to push that argument before, and therefore we don't yet know
    % whether it is worth pushing.
    %
:- type proc_pushable_args_map == map(arg_pos, maybe_worth_pushing).

:- type maybe_worth_pushing
    --->    worth_pushing
    ;       not_worth_pushing.

    % A map from a variable to the future object created for that variable.
    % If it maps e.g. X to FutureX, then
    %
    % - after a producer binds X to a value, it will signal FutureX, and
    % - before a consumer needs X, it will wait on FutureX.
    %
:- type future_map == map(prog_var, prog_var).

%-----------------------------------------------------------------------------%

:- pred find_specialization_requests_in_proc(done_par_procs::in,
    module_info::in, pred_proc_id::in, module_info::in, module_info::out,
    pending_par_procs::in, pending_par_procs::out,
    pushable_args_map::in, pushable_args_map::out) is det.

find_specialization_requests_in_proc(DoneProcs, InitialModuleInfo, PredProcId,
        !ModuleInfo, !PendingParProcs, !Pushability) :-
    PredProcId = proc(PredId, ProcId),
    some [!PredInfo, !ProcInfo, !Goal, !SpecInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_vartypes(!.ProcInfo, VarTypes),
        proc_info_get_goal(!.ProcInfo, !:Goal),
        !:SpecInfo = spec_info(DoneProcs, InitialModuleInfo, VarTypes,
            !.ModuleInfo, !.PendingParProcs, !.Pushability),

        trace [compile_time(flag("debug-dep-par-conj")), io(!IO)] (
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_accumulating_option(Globals, debug_dep_par_conj,
                DebugDepParConjWords),
            PredIdInt = pred_id_to_int(PredId),
            PredIdStr = string.int_to_string(PredIdInt),
            (
                some [DebugDepParConjWord] (
                    list.member(DebugDepParConjWord, DebugDepParConjWords),
                    DebugDepParConjWord = PredIdStr
                )
            ->
                OutInfo = init_hlds_out_info(Globals),
                proc_info_get_varset(!.ProcInfo, VarSet),
                write_goal(OutInfo, !.Goal, !.ModuleInfo, VarSet,
                    yes, 0, "", !IO)
            ;
                true
            )
        ),

        specialize_sequences_in_goal(!Goal, !SpecInfo),
        !.SpecInfo = spec_info(_, _, _,
            !:ModuleInfo, !:PendingParProcs, !:Pushability),
        proc_info_set_goal(!.Goal, !ProcInfo),
        % Optimization opportunity: we should not fix up the same procedure
        % twice, i.e. first in sync_dep_par_conjs_in_proc and then here.
        fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

:- pred add_requested_specialized_par_procs(pending_par_procs::in,
    pushable_args_map::in, done_par_procs::in, module_info::in,
    module_info::in, module_info::out) is det.

add_requested_specialized_par_procs(!.PendingParProcs, !.Pushability,
        !.DoneParProcs, InitialModuleInfo, !ModuleInfo) :-
    (
        !.PendingParProcs = []
    ;
        !.PendingParProcs = [CallPattern - NewProc | !:PendingParProcs],
        % Move the procedure we are about to parallelise into the list of
        % done procedures, in case of recursive calls.
        svmap.det_insert(CallPattern, NewProc, !DoneParProcs),
        add_requested_specialized_par_proc(CallPattern, NewProc,
            !PendingParProcs, !Pushability, !.DoneParProcs, InitialModuleInfo,
            !ModuleInfo),
        add_requested_specialized_par_procs(!.PendingParProcs, !.Pushability,
            !.DoneParProcs, InitialModuleInfo, !ModuleInfo)
    ).

:- pred add_requested_specialized_par_proc(par_proc_call_pattern::in,
    new_par_proc::in, pending_par_procs::in, pending_par_procs::out,
    pushable_args_map::in, pushable_args_map::out, done_par_procs::in,
    module_info::in, module_info::in, module_info::out) is det.

add_requested_specialized_par_proc(CallPattern, NewProc, !PendingParProcs,
        !Pushability, DoneParProcs, InitialModuleInfo, !ModuleInfo) :-
    CallPattern = par_proc_call_pattern(OldPredProcId, FutureArgs),
    NewProc = new_par_proc(NewPredProcId, _Name),
    OldPredProcId = proc(OldPredId, OldProcId),
    NewPredProcId = proc(NewPredId, NewProcId),

    some [!VarSet, !VarTypes, !NewProcInfo] (
        % Get the proc_info from _before_ the dependent parallel conjunction
        % pass was ever run, so we get untransformed procedure bodies.
        % Our transformation does not attempt to handle already transformed
        % parallel conjunctions.
        module_info_proc_info(InitialModuleInfo, OldPredId, OldProcId,
            !:NewProcInfo),
        proc_info_get_varset(!.NewProcInfo, !:VarSet),
        proc_info_get_vartypes(!.NewProcInfo, !:VarTypes),
        proc_info_get_headvars(!.NewProcInfo, HeadVars0),
        proc_info_get_argmodes(!.NewProcInfo, ArgModes0),
        proc_info_get_goal(!.NewProcInfo, Goal0),
        proc_info_get_initial_instmap(!.NewProcInfo, InitialModuleInfo,
            InstMap0),

        % Set up the mapping from head variables to futures.
        list.foldl3(map_arg_to_new_future(HeadVars0), FutureArgs,
            map.init, FutureMap, !VarSet, !VarTypes),

        % Replace head variables by their futures.
        replace_head_vars(!.ModuleInfo, FutureMap,
            HeadVars0, HeadVars, ArgModes0, ArgModes),

        % Insert signals and waits into the procedure body. We treat the body
        % as it were a conjunct of a parallel conjunction, since it is.
        module_info_get_globals(InitialModuleInfo, Globals),
        globals.lookup_bool_option(Globals, allow_some_paths_only_waits,
            AllowSomePathsOnly),
        SharedVars = set.from_list(map.keys(FutureMap)),
        sync_dep_par_conjunct(!.ModuleInfo, AllowSomePathsOnly,
            par_conjunct_is_proc_body, SharedVars, FutureMap, Goal0, Goal,
            InstMap0, _, !VarSet, !VarTypes),

        proc_info_set_varset(!.VarSet, !NewProcInfo),
        proc_info_set_vartypes(!.VarTypes, !NewProcInfo),
        proc_info_set_headvars(HeadVars, !NewProcInfo),
        proc_info_set_argmodes(ArgModes, !NewProcInfo),
        proc_info_set_goal(Goal, !NewProcInfo),

        module_info_pred_info(!.ModuleInfo, NewPredId, NewPredInfo0),

        % Mark this predicate impure if it no longer has any output arguments
        % (having been replaced by a future, which is an input argument which
        % is destructively updated).
        ( any_output_arguments(!.ModuleInfo, ArgModes) ->
            NewPredInfo = NewPredInfo0
        ;
            pred_info_get_markers(NewPredInfo0, Markers0),
            add_marker(marker_is_impure, Markers0, Markers),
            pred_info_set_markers(Markers, NewPredInfo0, NewPredInfo)
        ),

        fixup_and_reinsert_proc(NewPredId, NewProcId, NewPredInfo,
            !.NewProcInfo, !ModuleInfo),

        % Look for and process any dependent parallel conjunctions inside
        % the newly created (sort of; the previous version was only a
        % placeholder) specialized procedure.
        IgnoreVars = set.from_list(map.keys(FutureMap)),
        sync_dep_par_conjs_in_proc(NewPredId, NewProcId, IgnoreVars,
            !ModuleInfo, [], _ProcsToScan),
        find_specialization_requests_in_proc(DoneParProcs, InitialModuleInfo,
            NewPredProcId, !ModuleInfo, !PendingParProcs, !Pushability)
    ).

:- pred map_arg_to_new_future(list(prog_var)::in, arg_pos::in,
    future_map::in, future_map::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

map_arg_to_new_future(HeadVars, FutureArg, !FutureMap, !VarSet, !VarTypes) :-
    HeadVar = list.det_index1(HeadVars, FutureArg),
    map.lookup(!.VarTypes, HeadVar, VarType),
    make_future_var(HeadVar, VarType, !VarSet, !VarTypes, FutureVar,
        _FutureVarType),
    svmap.det_insert(HeadVar, FutureVar, !FutureMap).

:- pred replace_head_vars(module_info::in, future_map::in,
    prog_vars::in, prog_vars::out, list(mer_mode)::in, list(mer_mode)::out)
    is det.

replace_head_vars(_ModuleInfo, _FutureMap, [], [], [], []).
replace_head_vars(ModuleInfo, FutureMap,
        [Var0 | Vars0], [Var | Vars], [Mode0 | Modes0], [Mode | Modes]) :-
    ( map.search(FutureMap, Var0, FutureVar) ->
        Var = FutureVar,
        ( mode_is_input(ModuleInfo, Mode0) ->
            Mode = Mode0
        ; mode_is_output(ModuleInfo, Mode0) ->
            Mode = (ground(shared, none) -> ground(shared, none))
        ;
            sorry(this_file,
                "dependent parallel conjunction transformation " ++
                "only understands input and output modes")
        )
    ;
        Var = Var0,
        Mode = Mode0
    ),
    replace_head_vars(ModuleInfo, FutureMap, Vars0, Vars, Modes0, Modes).
replace_head_vars(_, _, [_ | _], _, [], _) :-
    unexpected(this_file, "replace_head_vars: length mismatch").
replace_head_vars(_, _, [], _, [_ | _], _) :-
    unexpected(this_file, "replace_head_vars: length mismatch").

:- pred any_output_arguments(module_info::in, list(mer_mode)::in) is semidet.

any_output_arguments(ModuleInfo, [Mode | Modes]) :-
    ( mode_is_output(ModuleInfo, Mode)
    ; any_output_arguments(ModuleInfo, Modes)
    ).

%-----------------------------------------------------------------------------%

    % Replace contiguous sequences of waits, a call to P, then signals by a
    % call to a parallelised procedure P'. Queue P' to be created later,
    % if it has not been created already.
    %
:- pred specialize_sequences_in_goal(hlds_goal::in, hlds_goal::out,
    spec_info::in, spec_info::out) is det.

specialize_sequences_in_goal(Goal0, Goal, !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            specialize_sequences_in_conj(Goals0, Goals, !SpecInfo),
            conj_list_to_goal(Goals, GoalInfo0, Goal)
        ;
            ConjType = parallel_conj,
            specialize_sequences_in_goals(Goals0, Goals, !SpecInfo),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        GoalExpr0 = disj(Goals0),
        specialize_sequences_in_goals(Goals0, Goals, !SpecInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        specialize_sequences_in_cases(Cases0, Cases, !SpecInfo),
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0),
        specialize_sequences_in_goal(Cond0, Cond, !SpecInfo),
        specialize_sequences_in_goal(Then0, Then, !SpecInfo),
        specialize_sequences_in_goal(Else0, Else, !SpecInfo),
        GoalExpr = if_then_else(Quant, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        specialize_sequences_in_goal(SubGoal0, SubGoal, !SpecInfo),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            % We don't pu either wait or signal operations in such scopes,
            % so there is nothing to specialize.
            Goal = Goal0
        ;
            specialize_sequences_in_goal(SubGoal0, SubGoal, !SpecInfo),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "specialize_sequences_in_goal: shorthand")
    ).

:- pred specialize_sequences_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    spec_info::in, spec_info::out) is det.

specialize_sequences_in_conj(Goals0, Goals, !SpecInfo) :-
    % For each call goal, look backwards for as many wait calls as possible
    % and forward for as many signal calls as possible. To allow us to look
    % backwards, we maintain a stack of the preceding goals.
    specialize_sequences_in_conj_2([], Goals0, Goals, !SpecInfo).

:- pred specialize_sequences_in_conj_2(list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out, spec_info::in, spec_info::out)
    is det.

specialize_sequences_in_conj_2(RevGoals, [], list.reverse(RevGoals),
        !SpecInfo).
specialize_sequences_in_conj_2(RevGoals0, [Goal0 | Goals0], Goals,
        !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        not is_wait_goal(Goal0),
        not is_signal_goal(Goal0)
    ->
        CallGoal0 = hlds_goal(GoalExpr0, GoalInfo0),  % dumb mode system
        maybe_specialize_call_and_goals(RevGoals0, CallGoal0, Goals0,
            RevGoals1, Goals1, !SpecInfo),
        specialize_sequences_in_conj_2(RevGoals1, Goals1, Goals, !SpecInfo)
    ;
        specialize_sequences_in_goal(Goal0, Goal, !SpecInfo),
        specialize_sequences_in_conj_2([Goal | RevGoals0], Goals0, Goals,
            !SpecInfo)
    ).

:- pred specialize_sequences_in_goals(list(hlds_goal)::in, list(hlds_goal)::out,
    spec_info::in, spec_info::out) is det.

specialize_sequences_in_goals([], [], !SpecInfo).
specialize_sequences_in_goals([Goal0 | Goals0], [Goal | Goals], !SpecInfo) :-
    specialize_sequences_in_goal(Goal0, Goal, !SpecInfo),
    specialize_sequences_in_goals(Goals0, Goals, !SpecInfo).

:- pred specialize_sequences_in_cases(list(case)::in, list(case)::out,
    spec_info::in, spec_info::out) is det.

specialize_sequences_in_cases([], [], !SpecInfo).
specialize_sequences_in_cases([Case0 | Cases0], [Case | Cases], !SpecInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    specialize_sequences_in_goal(Goal0, Goal, !SpecInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    specialize_sequences_in_cases(Cases0, Cases, !SpecInfo).

%-----------------------------------------------------------------------------%

:- inst call_goal_expr
    ==  bound(plain_call(ground, ground, ground, ground, ground, ground)).

:- inst call_goal
    ==  bound(hlds_goal(call_goal_expr, ground)).

:- pred maybe_specialize_call_and_goals(list(hlds_goal)::in,
    hlds_goal::in(call_goal), list(hlds_goal)::in,
    list(hlds_goal)::out, list(hlds_goal)::out,
    spec_info::in, spec_info::out) is det.

maybe_specialize_call_and_goals(RevGoals0, Goal0, FwdGoals0,
        RevGoals, FwdGoals, !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, _),
    GoalExpr0 = plain_call(PredId, ProcId, CallVars, _, _, _),

    module_info_pred_info(!.SpecInfo ^ spec_module_info, PredId, PredInfo),
    % We cannot push wait or signal goals into a procedure whose code we don't
    % have access to.
    % XXX: We have access to opt_imported procedures. The reason why this test
    % does not look for them is that we used to run dep_par_conj only *after*
    % mercury_compile used to invoke dead_proc_elim to delete opt_imported
    % procedures.
    ( list.member(ProcId, pred_info_non_imported_procids(PredInfo)) ->
        % Look for a contiguous sequence of wait goals at the start of
        % RevGoals (i.e. the goals immediately before Goal0) and for a
        % contiguous sequence of signal goals at the start of FwdGoals0
        % (the goals immediately following Goal0).
        %
        % Partition these wait and signal goals into
        % - those that are relevant (i.e. they mention arguments of the call)
        %   *and* worth pushing into the called procedure, and
        % - those that fail either or both of these criteria.
        %
        % We maintain the invariant that
        %   RevGoals0 = WaitGoals   ++ RevGoals1
        %   FwdGoals0 = SignalGoals ++ FwdGoals1
        % where WaitGoals is some interleaving of UnPushedWaitGoals and
        % the wait goals represented by PushedWaitPairs, and similarly
        % for SignalGoals.

        PredProcId = proc(PredId, ProcId),
        find_relevant_pushable_wait_goals(RevGoals0, PredProcId,
            CallVars, PushedWaitPairs, UnPushedWaitGoals, RevGoals1,
            !SpecInfo),
        find_relevant_pushable_signal_goals(FwdGoals0, PredProcId,
            CallVars, PushedSignalPairs, UnPushedSignalGoals, FwdGoals1,
            !SpecInfo),

        (
            PushedWaitPairs = [],
            PushedSignalPairs = []
        ->
            RevGoals = [Goal0 | RevGoals0],
            FwdGoals = FwdGoals0
        ;
            specialize_dep_par_call(PushedWaitPairs, PushedSignalPairs,
                Goal0, Goal, !SpecInfo),

            % After the replaced call may be further references to a signalled
            % or waited variable.  Add `get' goals after the transformed goal
            % to make sure the variable is bound.  We assume the get goals will
            % be simplified away if they turn out to be unnecessary.
            %
            % XXX The simplify pass that comes later doesn't always remove
            % these calls, even if they're unnecessary.

            VarTypes = !.SpecInfo ^ spec_vartypes,
            list.map(make_get_goal(!.SpecInfo ^ spec_module_info, VarTypes),
                PushedSignalPairs ++ PushedWaitPairs, GetGoals),

            RevGoals = GetGoals ++ [Goal] ++ UnPushedWaitGoals ++ RevGoals1,
            FwdGoals = UnPushedSignalGoals ++ FwdGoals1
        )
    ;
        RevGoals = [Goal0 | RevGoals0],
        FwdGoals = FwdGoals0
    ).

:- type future_var_pair
    --->    future_var_pair(
                fvp_future  :: prog_var,
                fvp_var     :: prog_var
            ).

:- func fvp_var(future_var_pair) = prog_var.

:- pred find_relevant_pushable_wait_goals(list(hlds_goal)::in,
    pred_proc_id::in, list(prog_var)::in, list(future_var_pair)::out,
    list(hlds_goal)::out, list(hlds_goal)::out,
    spec_info::in, spec_info::out) is det.

find_relevant_pushable_wait_goals([], _, _, [], [], [], !SpecInfo).
find_relevant_pushable_wait_goals([Goal | Goals], PredProcId, CallVars,
        PushedWaitPairs, UnPushedWaitGoals, RemainingGoals, !SpecInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = plain_call(_, _, WaitArgs, _, _, SymName),
        SymName = qualified(mercury_par_builtin_module, wait_future_pred_name),
        WaitArgs = [FutureVar, ConsumedVar]
    ->
        % This is a wait goal.
        find_relevant_pushable_wait_goals(Goals, PredProcId, CallVars,
            PushedWaitPairsTail, UnPushedWaitGoalsTail, RemainingGoals,
            !SpecInfo),
        ( list.nth_member_search(CallVars, ConsumedVar, ArgPos) ->
            % This wait goal waits for one of the variables consumed by the
            % following call, so we must consider whether to push the wait
            % into the called procedure.
            should_we_push(PredProcId, ArgPos, push_wait, IsWorthPushing,
                !SpecInfo),
            (
                IsWorthPushing = worth_pushing,
                PushedWaitPair = future_var_pair(FutureVar, ConsumedVar),
                PushedWaitPairs = [PushedWaitPair | PushedWaitPairsTail],
                UnPushedWaitGoals = UnPushedWaitGoalsTail
            ;
                IsWorthPushing = not_worth_pushing,
                % ConsumedVar is needed immediately by the called procedure,
                % so there is no point in pushing the wait operation into its
                % code.
                PushedWaitPairs = PushedWaitPairsTail,
                UnPushedWaitGoals = [Goal | UnPushedWaitGoalsTail]
            )
        ;
            % This wait goal waits for a variable that is *not* consumed by the
            % following call, so we cannot push the wait into the called
            % procedure.
            PushedWaitPairs = PushedWaitPairsTail,
            UnPushedWaitGoals = [Goal | UnPushedWaitGoalsTail]
        )
    ;
        % The sequence of wait goals (if any) has ended.
        PushedWaitPairs = [],
        UnPushedWaitGoals = [],
        RemainingGoals = [Goal | Goals]
    ).

:- pred find_relevant_pushable_signal_goals(list(hlds_goal)::in,
    pred_proc_id::in, list(prog_var)::in, list(future_var_pair)::out,
    list(hlds_goal)::out, list(hlds_goal)::out,
    spec_info::in, spec_info::out) is det.

find_relevant_pushable_signal_goals([], _, _, [], [], [], !SpecInfo).
find_relevant_pushable_signal_goals([Goal | Goals], PredProcId, CallVars,
        PushedSignalPairs, UnPushedSignalGoals, RemainingGoals, !SpecInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = plain_call(_, _, SignalArgs, _, _, SymName),
        SymName = qualified(mercury_par_builtin_module,
            signal_future_pred_name),
        SignalArgs = [FutureVar, ProducedVar]
    ->
        % This is a signal goal.
        find_relevant_pushable_signal_goals(Goals, PredProcId, CallVars,
            PushedSignalPairsTail, UnPushedSignalGoalsTail, RemainingGoals,
            !SpecInfo),
        ( list.nth_member_search(CallVars, ProducedVar, ArgPos) ->
            % This signal goal signals one of the variables produced by the
            % preceding call, so we must consider whether to push the signal
            % into the called procedure.
            should_we_push(PredProcId, ArgPos, push_signal, IsWorthPushing,
                !SpecInfo),
            (
                IsWorthPushing = worth_pushing,
                PushedSignalPair = future_var_pair(FutureVar, ProducedVar),
                PushedSignalPairs = [PushedSignalPair | PushedSignalPairsTail],
                UnPushedSignalGoals = UnPushedSignalGoalsTail
            ;
                IsWorthPushing = not_worth_pushing,
                % ProducedVar is generated just before the called procedure
                % returns, so there is no point in pushing the signal operation
                % into its code.
                PushedSignalPairs = PushedSignalPairsTail,
                UnPushedSignalGoals = [Goal | UnPushedSignalGoalsTail]
            )
        ;
            % This signal goal signals a variable that is *not* produced by the
            % preceding call, so we cannot push the signal into the called
            % procedure.
            PushedSignalPairs = PushedSignalPairsTail,
            UnPushedSignalGoals = [Goal | UnPushedSignalGoalsTail]
        )
    ;
        % The sequence of signal goals (if any) has ended.
        PushedSignalPairs = [],
        UnPushedSignalGoals = [],
        RemainingGoals = [Goal | Goals]
    ).

:- pred specialize_dep_par_call(
    list(future_var_pair)::in, list(future_var_pair)::in,
    hlds_goal::in(call_goal), hlds_goal::out,
    spec_info::in, spec_info::out) is det.

specialize_dep_par_call(WaitPairs, SignalPairs, Goal0, Goal, !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    GoalExpr0 = plain_call(PredId, ProcId, CallVars, _Builtin, Context, _Name),
    OrigPPId = proc(PredId, ProcId),

    WaitVars   = list.map(fvp_var, WaitPairs),
    SignalVars = list.map(fvp_var, SignalPairs),
    number_future_args(1, CallVars, WaitVars ++ SignalVars, [], FutureArgs),

    CallPattern = par_proc_call_pattern(OrigPPId, FutureArgs),
    (
        find_spec_par_proc_for_call_pattern(!.SpecInfo ^ spec_done_procs,
            !.SpecInfo ^ spec_pending_procs, CallPattern, SpecNewParProc)
    ->
        SpecNewParProc = new_par_proc(SpecPPId, SpecSymName)
    ;
        % Queue a new parallel procedure to be made. We add the new specialized
        % predicate and procedure to the module_info now; its final body
        % will be set later.
        ModuleInfo0 = !.SpecInfo ^ spec_module_info,
        PendingParProcs0 = !.SpecInfo ^ spec_pending_procs,
        create_new_spec_parallel_pred(FutureArgs, OrigPPId, SpecPPId, SpecName,
            ModuleInfo0, ModuleInfo),
        module_info_get_name(ModuleInfo, ModuleName),
        SpecSymName = qualified(ModuleName, SpecName),
        queue_par_proc(CallPattern, new_par_proc(SpecPPId, SpecSymName),
            PendingParProcs0, PendingParProcs),
        !SpecInfo ^ spec_module_info := ModuleInfo,
        !SpecInfo ^ spec_pending_procs := PendingParProcs
    ),

    % Replace the call with a call to the parallelised procedure.
    list.map(replace_args_with_futures(WaitPairs ++ SignalPairs),
        CallVars, NewCallVars),
    SpecPPId = proc(SpecPredId, SpecProcId),
    GoalExpr = plain_call(SpecPredId, SpecProcId, NewCallVars, not_builtin,
        Context, SpecSymName),
    Goal = hlds_goal(GoalExpr, GoalInfo0).

:- pred find_spec_par_proc_for_call_pattern(done_par_procs::in,
    pending_par_procs::in, par_proc_call_pattern::in,
    new_par_proc::out) is semidet.

find_spec_par_proc_for_call_pattern(DoneParProcs, PendingProcs, CallPattern,
        SpecProc) :-
    ( search(DoneParProcs, CallPattern, SpecProcPrime) ->
        SpecProc = SpecProcPrime
    ; search(PendingProcs, CallPattern, SpecProcPrime) ->
        SpecProc = SpecProcPrime
    ;
        fail
    ).

:- pred queue_par_proc(par_proc_call_pattern::in, new_par_proc::in,
    pending_par_procs::in, pending_par_procs::out) is det.

queue_par_proc(CallPattern, NewProc, !PendingParProcs) :-
    !:PendingParProcs = [CallPattern - NewProc | !.PendingParProcs].

:- pred replace_args_with_futures(list(future_var_pair)::in,
    prog_var::in, prog_var::out) is det.

replace_args_with_futures([], Var, Var).
replace_args_with_futures([H | T], Var0, Var) :-
    H = future_var_pair(Future, X),
    ( X = Var0 ->
        Var = Future
    ;
        replace_args_with_futures(T, Var0, Var)
    ).

:- pred number_future_args(arg_pos::in, prog_vars::in, list(prog_var)::in,
    list(arg_pos)::in, list(arg_pos)::out) is det.

number_future_args(_, [], _, RevAcc, reverse(RevAcc)).
number_future_args(ArgNo, [Arg | Args], WaitSignalVars, !RevAcc) :-
    ( list.member(Arg, WaitSignalVars) ->
        list.cons(ArgNo, !RevAcc)
    ;
        true
    ),
    number_future_args(ArgNo+1, Args, WaitSignalVars, !RevAcc).

%-----------------------------------------------------------------------------%

:- pred create_new_spec_parallel_pred(list(arg_pos)::in, pred_proc_id::in,
    pred_proc_id::out, string::out, module_info::in, module_info::out) is det.

create_new_spec_parallel_pred(FutureArgs, OrigPPId, NewPPId,
        NewPredName, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, OrigPPId,
        OrigPredInfo, OrigProcInfo),
    Status = status_local,
    make_new_spec_parallel_pred_info(FutureArgs, Status, OrigPPId,
        OrigPredInfo, NewPredInfo0),
    NewPredName = pred_info_name(NewPredInfo0),

    % Assign the old procedure to a new predicate, which will be modified
    % in a later pass.
    OrigPPId = proc(_, ProcId),
    pred_info_get_procedures(NewPredInfo0, NewProcs0),
    map.set(NewProcs0, ProcId, OrigProcInfo, NewProcs),
    pred_info_set_procedures(NewProcs, NewPredInfo0, NewPredInfo),

    % Add the new predicate to the pred table.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),
    NewPPId = proc(NewPredId, ProcId).

    % The comments in this predicate are from unused_args.m
    %
:- pred make_new_spec_parallel_pred_info(list(arg_pos)::in, import_status::in,
    pred_proc_id::in, pred_info::in, pred_info::out) is det.

make_new_spec_parallel_pred_info(FutureArgs, Status, PPId, !PredInfo) :-
    PPId = proc(PredId, ProcId),
    PredModule = pred_info_module(!.PredInfo),
    Name0 = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    pred_info_get_arg_types(!.PredInfo, Tvars, ExistQVars, ArgTypes0),
    pred_info_get_origin(!.PredInfo, OrigOrigin),
    make_pred_name(PredModule, "Parallel", yes(PredOrFunc),
        Name0, newpred_parallel_args(FutureArgs), Name1),
    % The mode number is included because we want to avoid the creation of
    % more than one predicate with the same name if more than one mode of
    % a predicate is parallelised. Since the names of e.g. deep profiling
    % proc_static structures are derived from the names of predicates,
    % duplicate predicate names lead to duplicate global variable names
    % and hence to link errors.
    proc_id_to_int(ProcId, ProcInt),
    add_sym_name_suffix(Name1, "_" ++ int_to_string(ProcInt), Name),
    Arity = pred_info_orig_arity(!.PredInfo),
    pred_info_get_typevarset(!.PredInfo, TypeVars),

    futurise_argtypes(1, FutureArgs, ArgTypes0, ArgTypes),

    pred_info_get_context(!.PredInfo, Context),
    pred_info_get_clauses_info(!.PredInfo, ClausesInfo),
    pred_info_get_markers(!.PredInfo, Markers),
    pred_info_get_goal_type(!.PredInfo, GoalType),
    pred_info_get_class_context(!.PredInfo, ClassContext),
    pred_info_get_var_name_remap(!.PredInfo, VarNameRemap),

    % Since this pred_info isn't built until after the polymorphism
    % transformation is complete, we just use dummy maps for the class
    % constraints.
    map.init(EmptyProofs),
    map.init(EmptyConstraintMap),
    Origin = origin_transformed(transform_dependent_parallel_conjunction,
        OrigOrigin, PredId),
    pred_info_init(PredModule, Name, Arity, PredOrFunc, Context, Origin,
        Status, GoalType, Markers, ArgTypes, Tvars, ExistQVars,
        ClassContext, EmptyProofs, EmptyConstraintMap, ClausesInfo,
        VarNameRemap, !:PredInfo),
    pred_info_set_typevarset(TypeVars, !PredInfo).

:- pred futurise_argtypes(arg_pos::in, list(arg_pos)::in, list(mer_type)::in,
    list(mer_type)::out) is det.

futurise_argtypes(_, [], ArgTypes, ArgTypes).
futurise_argtypes(ArgNo, [FutureArg | FutureArgs], [ArgType | ArgTypes],
        [FuturisedArgType | FuturisedArgTypes]) :-
    ( ArgNo = FutureArg ->
        FuturisedArgType = future_type(ArgType),
        futurise_argtypes(ArgNo + 1, FutureArgs,
            ArgTypes, FuturisedArgTypes)
    ;
        FuturisedArgType = ArgType,
        futurise_argtypes(ArgNo + 1, [FutureArg | FutureArgs],
            ArgTypes, FuturisedArgTypes)
    ).
futurise_argtypes(_, [_ | _], [], _) :-
    unexpected(this_file,
        "futurise_argtypes: more future arguments than argument types").

%-----------------------------------------------------------------------------%

:- type push_op
    --->    push_wait
    ;       push_signal.

:- pred should_we_push(pred_proc_id::in, int::in, push_op::in,
    maybe_worth_pushing::out, spec_info::in, spec_info::out) is det.

should_we_push(PredProcId, ArgPos, PushOp, IsWorthPushing, !SpecInfo) :-
    Pushability0 = !.SpecInfo ^ spec_pushability,
    ( map.search(Pushability0, PredProcId, ProcPushMap0) ->
        ( map.search(ProcPushMap0, ArgPos, KnownWorthPushing) ->
            IsWorthPushing = KnownWorthPushing
        ;
            should_we_push_test(PredProcId, ArgPos, PushOp, IsWorthPushing,
                !.SpecInfo),
            map.det_insert(ProcPushMap0, ArgPos, IsWorthPushing, ProcPushMap),
            map.det_update(Pushability0, PredProcId, ProcPushMap, Pushability),
            !SpecInfo ^ spec_pushability := Pushability
        )
    ;
        InitialModuleInfo = !.SpecInfo ^ spec_initial_module,
        module_info_get_globals(InitialModuleInfo, Globals),
        globals.lookup_bool_option(Globals, always_specialize_in_dep_par_conjs,
            AlwaysSpecialize),
        (
            AlwaysSpecialize = yes,
            IsWorthPushing = worth_pushing
        ;
            AlwaysSpecialize = no,
            should_we_push_test(PredProcId, ArgPos, PushOp, IsWorthPushing,
                !.SpecInfo)
        ),
        map.det_insert(map.init, ArgPos, IsWorthPushing, ProcPushMap),
        map.det_insert(Pushability0, PredProcId, ProcPushMap, Pushability),
        !SpecInfo ^ spec_pushability := Pushability
    ).

:- pred should_we_push_test(pred_proc_id::in, int::in, push_op::in,
    maybe_worth_pushing::out, spec_info::in) is det.

should_we_push_test(PredProcId, ArgPos, PushOp, IsWorthPushing, SpecInfo) :-
    InitialModuleInfo = SpecInfo ^ spec_initial_module,
    module_info_proc_info(InitialModuleInfo, PredProcId, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    list.index1_det(HeadVars, ArgPos, Var),
    proc_info_get_goal(ProcInfo, Goal),
    (
        PushOp = push_wait,
        should_we_push_wait(Var, Goal, CostBeforeWait),
        (
            CostBeforeWait = seen_wait_negligible_cost_before,
            IsWorthPushing = not_worth_pushing
        ;
            ( CostBeforeWait = not_seen_wait_non_negligible_cost_so_far
            ; CostBeforeWait = seen_wait_non_negligible_cost_before
            ),
            IsWorthPushing = worth_pushing
        ;
            CostBeforeWait = not_seen_wait_negligible_cost_so_far,
            % This should not happen unless (a) the procedure ignores its
            % input, or (b) we made an incorrect approximation in
            % should_we_push_wait.
            IsWorthPushing = worth_pushing
        )
    ;
        PushOp = push_signal,
        should_we_push_signal(Var, Goal, not_seen_signal, CostAfterSignal),
        (
            CostAfterSignal = not_seen_signal,
            % This should not happen, since it is a mode error.
            unexpected(this_file, "should_we_push_test: not_seen_signal")
        ;
            CostAfterSignal = seen_signal_negligible_cost_after,
            IsWorthPushing = not_worth_pushing
        ;
            CostAfterSignal = seen_signal_non_negligible_cost_after,
            IsWorthPushing = worth_pushing
        )
    ).

%-----------------------------------------------------------------------------%

:- type cost_before_wait
    --->    not_seen_wait_negligible_cost_so_far
    ;       not_seen_wait_non_negligible_cost_so_far
    ;       seen_wait_negligible_cost_before
    ;       seen_wait_non_negligible_cost_before.

    % Separate cost_before_wait into its components: seen (yes/no) and
    % non-negligible cost (yes/no), or put it back together. The fact that
    % the costs are the costs of different things for different values of seen
    % is just something we have to live with.
:- pred cost_before_wait_components(cost_before_wait, bool, bool).
:- mode cost_before_wait_components(in, out, out) is det.
:- mode cost_before_wait_components(out, in, in) is det.

cost_before_wait_components(not_seen_wait_negligible_cost_so_far, no, no).
cost_before_wait_components(not_seen_wait_non_negligible_cost_so_far, no, yes).
cost_before_wait_components(seen_wait_negligible_cost_before, yes, no).
cost_before_wait_components(seen_wait_non_negligible_cost_before, yes, yes).

:- pred should_we_push_wait(prog_var::in, hlds_goal::in, cost_before_wait::out)
    is det.

should_we_push_wait(Var, Goal, Wait) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    % When handling calls, we could use profiling data to decide whether
    % a call site has negligible cost or not. In the absence of such data,
    % we have to assume that all call sites have non-negligible cost, because
    % if we assumed that they have negligible cost, then we would have to infer
    % that *all* goals have negligible cost, which besides being incorrect,
    % would mean that there is never any point in pushing waits, rendering
    % this entire code useless.
    (
        GoalExpr = unify(_, _, _, _, _),
        ( set.member(Var, NonLocals) ->
            Wait = seen_wait_negligible_cost_before
        ;
            Wait = not_seen_wait_negligible_cost_so_far
        )
    ;
        GoalExpr = plain_call(_, _, _, BuiltinStatus, _, _),
        (
            BuiltinStatus = inline_builtin,
            ( set.member(Var, NonLocals) ->
                Wait = seen_wait_negligible_cost_before
            ;
                Wait = not_seen_wait_negligible_cost_so_far
            )
        ;
            ( BuiltinStatus = not_builtin
            ; BuiltinStatus = out_of_line_builtin
            ),
            ( set.member(Var, NonLocals) ->
                Wait = seen_wait_non_negligible_cost_before
            ;
                Wait = not_seen_wait_non_negligible_cost_so_far
            )
        )
    ;
        ( GoalExpr = generic_call(_, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ( set.member(Var, NonLocals) ->
            Wait = seen_wait_non_negligible_cost_before
        ;
            Wait = not_seen_wait_non_negligible_cost_so_far
        )
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        (
            ConjType = plain_conj,
            should_we_push_wait_in_conj(Var, Conjuncts, Wait)
        ;
            ConjType = parallel_conj,
            list.map(should_we_push_wait(Var), Conjuncts, Waits),
            ( list.member(seen_wait_non_negligible_cost_before, Waits) ->
                % At least one of the parallel conjuncts can benefit from not
                % waiting for Var at the start.
                Wait = seen_wait_non_negligible_cost_before
            ; list.member(not_seen_wait_non_negligible_cost_so_far, Waits) ->
                % At least one of the parallel conjuncts does not need to wait
                % for Var at all, and has non-negligible cost. That conjunct
                % can also benefit from not waiting for Var at the start.
                Wait = not_seen_wait_non_negligible_cost_so_far
            ; list.member(seen_wait_negligible_cost_before, Waits) ->
                Wait = seen_wait_negligible_cost_before
            ;
                Wait = not_seen_wait_negligible_cost_so_far
            )
        )
    ;
        GoalExpr = disj(Disjuncts),
        Detism = goal_info_get_determinism(GoalInfo),
        determinism_components(Detism, _, SolnCount),
        (
            SolnCount = at_most_many,
            (
                Disjuncts = [FirstDisjunct | _LaterDisjuncts],
                should_we_push_wait(Var, FirstDisjunct, WaitFirst),
                (
                    ( WaitFirst = seen_wait_negligible_cost_before
                    ; WaitFirst = seen_wait_non_negligible_cost_before
                    ),
                    % If FirstDisjunct waits for Var, the cost before that
                    % wait in FirstDisjunct tells us the cost before the wait
                    % in Goal.
                    Wait = WaitFirst
                ;
                    ( WaitFirst = not_seen_wait_negligible_cost_so_far
                    ; WaitFirst = not_seen_wait_non_negligible_cost_so_far
                    ),
                    % If FirstDisjunct does not wait for Var, then we may
                    % execute an arbitrary initial subsequence of the code
                    % following the disjunct before execution backtracks
                    % to the later disjuncts. We therefore want this following
                    % code to decide whether we push the wait.
                    Wait = not_seen_wait_negligible_cost_so_far
                )
            ;
                Disjuncts = [],
                Wait = not_seen_wait_negligible_cost_so_far
            )
        ;
            ( SolnCount = at_most_zero
            ; SolnCount = at_most_one
            ; SolnCount = at_most_many_cc
            ),
            % The most expensive thing we can do is to execute one disjunct
            % after another, with all disjuncts except possibly the last
            % all failing at the last moment. This is like a conjunction
            % in which we execute only some of the goals.
            should_we_push_wait_in_conj(Var, Disjuncts, Wait)
        )
    ;
        GoalExpr = switch(SwitchVar, _, Cases),
        ( Var = SwitchVar ->
            Wait = seen_wait_negligible_cost_before
        ;
            should_we_push_wait_in_cases(Var, Cases, no, Wait)
        )
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        should_we_push_wait(Var, Cond, WaitCond),
        (
            ( WaitCond = seen_wait_negligible_cost_before
            ; WaitCond = seen_wait_non_negligible_cost_before
            ),
            % If Cond waits for Var, the cost before that wait in Cond
            % tells us the cost before the wait in Goal if Cond succeeds.
            % The cost when Cond fails could be the opposite, and we have
            % no certain way of deciding right at compile time, though we could
            % in principle we could come close by analyzing the determinism of
            % Cond's component goals. The following code is a very simple
            % guess.
            Wait = WaitCond
        ;
            WaitCond = not_seen_wait_non_negligible_cost_so_far,
            % Execution paths on which the condition succeeds would definitely
            % benefit from pushing the wait.
            Wait = not_seen_wait_non_negligible_cost_so_far
        ;
            WaitCond = not_seen_wait_negligible_cost_so_far,
            % Execution will reach the start of either the then or the else
            % branch without waiting or incurring non-negligible cost.
            should_we_push_wait(Var, Then, WaitThen),
            should_we_push_wait(Var, Else, WaitElse),
            cost_before_wait_components(WaitThen, ThenSeen, ThenCost),
            cost_before_wait_components(WaitElse, ElseSeen, ElseCost),
            bool.or(ThenSeen, ElseSeen, Seen),
            % If ThenSeen != ElseSeen, then this mixes two kinds of cost:
            % the cost so far before seeing a wait, and the cost before a wait.
            % However, the result we get here is should still be a reasonable
            % approximation, and that is all we need.
            bool.or(ThenCost, ElseCost, Cost),
            cost_before_wait_components(Wait, Seen, Cost)
        )
    ;
        GoalExpr = negation(SubGoal),
        should_we_push_wait(Var, SubGoal, Wait)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            % The SubGoal may be huge, but since the code generator will
            % turn it all into a single assignment of a pointer to a large
            % static data structure, its cost in execution time is negligible.
            Wait = not_seen_wait_negligible_cost_so_far
        ;
            should_we_push_wait(Var, SubGoal, Wait)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file, "should_we_push_wait: shorthand")
    ).

:- pred should_we_push_wait_in_conj(prog_var::in, list(hlds_goal)::in,
    cost_before_wait::out) is det.

should_we_push_wait_in_conj(_, [], not_seen_wait_negligible_cost_so_far).
should_we_push_wait_in_conj(Var, [Goal | Goals], CostBeforeWait) :-
    should_we_push_wait(Var, Goal, CostBeforeWaitHead),
    (
        CostBeforeWaitHead = not_seen_wait_negligible_cost_so_far,
        % Nothing significant has happened so far; whether we want to push
        % the wait depends on the rest of the conjunction.
        should_we_push_wait_in_conj(Var, Goals, CostBeforeWait)
    ;
        CostBeforeWaitHead = not_seen_wait_non_negligible_cost_so_far,
        % We already know that we will want to push the wait, since doing
        % the wait after the non-negligible cost of Goal will be a win.
        CostBeforeWait = not_seen_wait_non_negligible_cost_so_far
    ;
        CostBeforeWaitHead = seen_wait_negligible_cost_before,
        % We already know that along this execution path, we don't want
        % to push the wait.
        CostBeforeWait = seen_wait_negligible_cost_before
    ;
        CostBeforeWaitHead = seen_wait_non_negligible_cost_before,
        % We already know that we will want to push the wait, since doing
        % the wait after the non-negligible cost of part of Goal will be a win.
        CostBeforeWait = seen_wait_non_negligible_cost_before
    ).

:- pred should_we_push_wait_in_cases(prog_var::in, list(case)::in,
    bool::in, cost_before_wait::out) is det.

should_we_push_wait_in_cases(_, [], SeenWait, CostBeforeWait) :-
    (
        SeenWait = no,
        CostBeforeWait = not_seen_wait_negligible_cost_so_far
    ;
        SeenWait = yes,
        CostBeforeWait = seen_wait_negligible_cost_before
    ).
should_we_push_wait_in_cases(Var, [Case | Cases], SeenWait, CostBeforeWait) :-
    Case = case(_MainConsId, _OtherConsIds, Goal),
    should_we_push_wait(Var, Goal, CostBeforeWaitHead),
    (
        CostBeforeWaitHead = not_seen_wait_negligible_cost_so_far,
        % Nothing significant happens in this switch arm; whether we want
        % to push the wait depends on the rest of the arms.
        should_we_push_wait_in_cases(Var, Cases, SeenWait, CostBeforeWait)
    ;
        CostBeforeWaitHead = not_seen_wait_non_negligible_cost_so_far,
        % We already know that we will want to push the wait, since doing
        % the wait after the non-negligible cost of Goal will be a win.
        CostBeforeWait = not_seen_wait_non_negligible_cost_so_far
    ;
        CostBeforeWaitHead = seen_wait_negligible_cost_before,
        % There is no benefit along this execution path to pushing the wait,
        % but there may be benefit along execution paths involving other switch
        % arms.
        NewSeenWait = yes,
        should_we_push_wait_in_cases(Var, Cases, NewSeenWait, CostBeforeWait)
    ;
        CostBeforeWaitHead = seen_wait_non_negligible_cost_before,
        % We already know that we will want to push the wait, since doing
        % the wait after the non-negligible cost of part of Goal will be a win.
        CostBeforeWait = seen_wait_non_negligible_cost_before
    ).

%-----------------------------------------------------------------------------%

:- type cost_after_signal
    --->    not_seen_signal
    ;       seen_signal_negligible_cost_after
    ;       seen_signal_non_negligible_cost_after.

:- pred seen_produced_var(cost_after_signal::in, cost_after_signal::out)
    is det.

seen_produced_var(!Signal) :-
    (
        !.Signal = not_seen_signal,
        !:Signal = seen_signal_negligible_cost_after
    ;
        ( !.Signal = seen_signal_negligible_cost_after
        ; !.Signal = seen_signal_non_negligible_cost_after
        )
    ).

:- pred seen_nontrivial_cost(cost_after_signal::in, cost_after_signal::out)
    is det.

seen_nontrivial_cost(!Signal) :-
    (
        !.Signal = not_seen_signal
        % We are not interested in costs before the signal.
    ;
        !.Signal = seen_signal_negligible_cost_after,
        !:Signal = seen_signal_non_negligible_cost_after
    ;
        !.Signal = seen_signal_non_negligible_cost_after
    ).

:- pred should_we_push_signal(prog_var::in, hlds_goal::in,
    cost_after_signal::in, cost_after_signal::out) is det.

should_we_push_signal(Var, Goal, !Signal) :-
    expect(negate(unify(!.Signal, seen_signal_non_negligible_cost_after)),
        this_file, "should_we_push_signal: already know we want to push"),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    % When handling calls, we could use profiling data to decide whether
    % a call site has negligible cost or not. In the absence of such data,
    % we have to assume that all call sites have non-negligible cost, because
    % if we assumed that they have negligible cost, then we would have to infer
    % that *all* goals have negligible cost, which besides being incorrect,
    % would mean that there is never any point in pushing signals, rendering
    % this entire code useless.
    (
        GoalExpr = unify(_, _, _, _, _),
        ( set.member(Var, NonLocals) ->
            seen_produced_var(!Signal)
        ;
            true
        )
    ;
        % With generic calls, the only safe assumption is that they produce
        % Var just before return. With foreign code, the signal is done after
        % the return to Mercury execution.
        ( GoalExpr = generic_call(_, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ( set.member(Var, NonLocals) ->
            seen_produced_var(!Signal)
        ;
            seen_nontrivial_cost(!Signal)
        )
    ;
        GoalExpr = plain_call(_, _, _, _, _, _),
        % XXX We should invoke should_we_push recursively on the called
        % procedure, though that would require safeguards against infinite
        % recursion.
        ( set.member(Var, NonLocals) ->
            seen_produced_var(!Signal)
        ;
            seen_nontrivial_cost(!Signal)
        )
    ;
        GoalExpr = conj(ConjType, Conjuncts),
        (
            ConjType = plain_conj,
            should_we_push_signal_in_plain_conj(Var, Conjuncts, !Signal)
        ;
            ConjType = parallel_conj,
            should_we_push_signal_in_par_conj(Var, Conjuncts, !.Signal,
                !Signal)
        )
    ;
        GoalExpr = disj(Disjuncts),
        % What we do in this case doesn't usually matter. Semidet disjunctions
        % cannot bind any nonlocal variables (and thus cannot bind Var).
        % Nondet disjunctions can bind variables, but we want to parallelize
        % only model_det code. The only case where what we do here matters
        % is when a nondet disjunction is inside a scope that commits to the
        % first success.
        should_we_push_signal_in_disj(Var, Disjuncts, !.Signal, !Signal)
    ;
        GoalExpr = switch(SwitchVar, _, Cases),
        ( Var = SwitchVar ->
            % !.Signal must show that we have already seen a signal.
            expect(negate(unify(!.Signal, not_seen_signal)),
                this_file, "should_we_push_signal: not seen switch var")
        ;
            should_we_push_signal_in_cases(Var, Cases, !.Signal, !Signal)
        )
    ;
        GoalExpr = if_then_else(_Vars, _Cond, Then, Else),
        % The condition cannot produce a nonlocal variable such as Var.
        should_we_push_signal(Var, Then, !.Signal, SignalThen),
        should_we_push_signal(Var, Else, !.Signal, SignalElse),
        (
            ( SignalThen = seen_signal_non_negligible_cost_after
            ; SignalElse = seen_signal_non_negligible_cost_after
            )
        ->
            % It is worth pushing the signal into at least one of the then and
            % else cases.
            !:Signal = seen_signal_non_negligible_cost_after
        ;
            ( SignalThen = seen_signal_negligible_cost_after
            ; SignalElse = seen_signal_negligible_cost_after
            )
        ->
            (
                Then = hlds_goal(_, ThenInfo),
                ThenDetism = goal_info_get_determinism(ThenInfo),
                determinism_components(ThenDetism, _, ThenMaxSolns),
                SignalThen = not_seen_signal,
                not ( ThenMaxSolns = at_most_zero)
            ->
                unexpected(this_file,
                    "should_we_push_signal: ite mode mismatch")
            ;
                true
            ),
            (
                Else = hlds_goal(_, ElseInfo),
                ElseDetism = goal_info_get_determinism(ElseInfo),
                determinism_components(ElseDetism, _, ElseMaxSolns),
                SignalElse = not_seen_signal,
                not ( ElseMaxSolns = at_most_zero)
            ->
                unexpected(this_file,
                    "should_we_push_signal: ite mode mismatch")
            ;
                true
            ),
            % Both arms of the if-then-else signal Var (if they succeed
            % at all), but neither does anything nontrivial after the signal.
            !:Signal = seen_signal_negligible_cost_after
        ;
            expect(unify(SignalThen, not_seen_signal),
                this_file, "should_we_push_signal: ite not_seen_signal"),
            expect(unify(SignalElse, not_seen_signal),
                this_file, "should_we_push_signal: ite not_seen_signal"),
            !:Signal = not_seen_signal
        )
    ;
        GoalExpr = negation(SubGoal),
        (
            !.Signal = not_seen_signal
            % A negated goal cannot produce a nonlocal variable such as Var,
            % and we don't care about the cost of computations before the
            % signal.
        ;
            !.Signal = seen_signal_negligible_cost_after,
            % We do care whether the cost of SubGoal is negligible or not.
            should_we_push_signal(Var, SubGoal, !Signal)
        ;
            !.Signal = seen_signal_non_negligible_cost_after,
            unexpected(this_file, "seen_signal_non_negligible_cost_after")
        )
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( Reason = from_ground_term(TermVar, from_ground_term_construct) ->
            ( Var = TermVar ->
                seen_produced_var(!Signal)
            ;
                true
            )
        ;   
            should_we_push_signal(Var, SubGoal, !Signal)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file, "should_we_push_signal: shorthand")
    ).

:- pred should_we_push_signal_in_plain_conj(prog_var::in, list(hlds_goal)::in,
    cost_after_signal::in, cost_after_signal::out) is det.

should_we_push_signal_in_plain_conj(_Var, [], !Signal).
should_we_push_signal_in_plain_conj(Var, [Conjunct | Conjuncts], !Signal) :-
    should_we_push_signal(Var, Conjunct, !Signal),
    (
        !.Signal = seen_signal_non_negligible_cost_after
        % There is no point in looking at Conjuncts; we already know
        % we want to push the signal.
    ;
        ( !.Signal = not_seen_signal
        ; !.Signal = seen_signal_negligible_cost_after
        ),
        should_we_push_signal_in_plain_conj(Var, Conjuncts, !Signal)
    ).

:- pred should_we_push_signal_in_par_conj(prog_var::in, list(hlds_goal)::in,
    cost_after_signal::in, cost_after_signal::in, cost_after_signal::out)
    is det.

should_we_push_signal_in_par_conj(_Var, [], _OrigSignal, !FinalSignal).
should_we_push_signal_in_par_conj(Var, [Conjunct | Conjuncts], OrigSignal,
        !FinalSignal) :-
    FinalSignal0 = !.FinalSignal,
    should_we_push_signal(Var, Conjunct, OrigSignal, ConjunctSignal),
    (
        ConjunctSignal = not_seen_signal,
        % Neither the goal before the parallel conjunction nor the parallel
        % conjuncts we have looked at so far produce Var.
        should_we_push_signal_in_par_conj(Var, Conjuncts, OrigSignal,
            !FinalSignal)
    ;
        ConjunctSignal = seen_signal_negligible_cost_after,
        (
            Conjuncts = [],
            % There are no more conjuncts after this one, so Var is produced
            % just before the end of the final conjunct.
            !:FinalSignal = seen_signal_negligible_cost_after
        ;
            Conjuncts = [_ | _],
            % There are more conjuncts after this one, and since negligible
            % cost goals are not worth parallelizing, we can assume that 
            % at least on some executions, the signal of Var will be followed
            % by the nontrivial execution of some of Conjuncts.
            !:FinalSignal = seen_signal_non_negligible_cost_after
        )
    ;
        ConjunctSignal = seen_signal_non_negligible_cost_after,
        % There is no point in looking at Conjuncts; we already know
        % we want to push the signal.
        !:FinalSignal = seen_signal_non_negligible_cost_after
    ),
    FinalSignal = !.FinalSignal,
    expect(seen_more_signal(FinalSignal0, FinalSignal), this_file,
        "should_we_push_signal_in_par_conj: final signal goes backwards").

:- pred should_we_push_signal_in_disj(prog_var::in, list(hlds_goal)::in,
    cost_after_signal::in, cost_after_signal::in, cost_after_signal::out)
    is det.

should_we_push_signal_in_disj(_Var, [], _OrigSignal, !FinalSignal).
should_we_push_signal_in_disj(Var, [FirstGoal | LaterGoals], OrigSignal,
        _, !:FinalSignal) :-
    should_we_push_signal(Var, FirstGoal, OrigSignal, SignalFirst),
    (
        SignalFirst = not_seen_signal,
        % If FirstGoal does not signal Var, the rest of the disjuncts
        % shouldn't either.
        !:FinalSignal = SignalFirst
    ;
        SignalFirst = seen_signal_non_negligible_cost_after,
        % We already know we want to push the signal.
        !:FinalSignal = SignalFirst
    ;
        SignalFirst = seen_signal_negligible_cost_after,
        % We want to push the signal only if it is worth pushing
        % into one of the the rest of the disjuncts.
        !:FinalSignal = SignalFirst,
        should_we_push_signal_in_disj(Var, LaterGoals, OrigSignal,
            !FinalSignal)
    ).

:- pred should_we_push_signal_in_cases(prog_var::in, list(case)::in,
    cost_after_signal::in, cost_after_signal::in, cost_after_signal::out)
    is det.

should_we_push_signal_in_cases(_Var, [], _OrigSignal, !FinalSignal).
should_we_push_signal_in_cases(Var, [FirstCase | LaterCases], OrigSignal,
        _, !:FinalSignal) :-
    FirstCase = case(_, _, FirstGoal),
    should_we_push_signal(Var, FirstGoal, OrigSignal, SignalFirst),
    (
        SignalFirst = not_seen_signal,
        % If FirstCase does not signal Var, the rest of the cases
        % shouldn't either.
        !:FinalSignal = SignalFirst
    ;
        SignalFirst = seen_signal_non_negligible_cost_after,
        % We already know we want to push the signal.
        !:FinalSignal = SignalFirst
    ;
        SignalFirst = seen_signal_negligible_cost_after,
        % We want to push the signal only if it is worth pushing
        % into one of the the rest of the cases.
        !:FinalSignal = SignalFirst,
        should_we_push_signal_in_cases(Var, LaterCases, OrigSignal,
            !FinalSignal)
    ).

:- pred seen_more_signal(cost_after_signal::in, cost_after_signal::in)
    is semidet.

seen_more_signal(SignalA, SignalB) :-
    seen_more_signal_2(SignalA, SignalB) = yes.

:- func seen_more_signal_2(cost_after_signal, cost_after_signal) = bool.

seen_more_signal_2(not_seen_signal, _) = yes.
seen_more_signal_2(seen_signal_negligible_cost_after,
    not_seen_signal) = no.
seen_more_signal_2(seen_signal_negligible_cost_after,
    seen_signal_negligible_cost_after) = yes.
seen_more_signal_2(seen_signal_negligible_cost_after,
    seen_signal_non_negligible_cost_after) = yes.
seen_more_signal_2(seen_signal_non_negligible_cost_after,
    not_seen_signal) = no.
seen_more_signal_2(seen_signal_non_negligible_cost_after,
    seen_signal_negligible_cost_after) = no.
seen_more_signal_2(seen_signal_non_negligible_cost_after,
    seen_signal_non_negligible_cost_after) = yes.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Utilities for working with par_builtin.
%

    % Given a variable SharedVar of type SharedVarType, add a new variable
    % FutureVar of type future(SharedVarType), add the mapping from SharedVar
    % to FutureVar to FutureMap, and generate the goal AllocGoal that calls
    % `par_builtin.new_future/1' to allocate FutureVar.
    %
:- pred allocate_future(module_info::in, prog_var::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    future_map::in, future_map::out) is det.

allocate_future(ModuleInfo, SharedVar, AllocGoal, !VarSet, !VarTypes,
        !FutureMap) :-
    map.lookup(!.VarTypes, SharedVar, SharedVarType),
    make_future_var(SharedVar, SharedVarType, !VarSet, !VarTypes,
        FutureVar, FutureVarType),
    svmap.det_insert(SharedVar, FutureVar, !FutureMap),

    ModuleName = mercury_par_builtin_module,
    PredName = new_future_pred_name,
    Features = [],
    InstMapDelta = instmap_delta_bind_var(FutureVar),
    Context = term.context_init,
    ShouldInline = should_inline_par_builtin_calls(ModuleInfo),
    (
        ShouldInline = no,
        ArgVars = [FutureVar],
        generate_simple_call(ModuleName, PredName, pf_predicate,
            only_mode, detism_det, purity_pure, ArgVars, Features,
            InstMapDelta, ModuleInfo, Context, AllocGoal)
    ;
        ShouldInline = yes,
        ForeignAttrs = par_builtin_foreign_proc_attributes(purity_pure, no),
        Arg1 = foreign_arg(FutureVar, yes("Future" - out_mode),
            FutureVarType, native_if_possible),
        Args = [Arg1],
        ExtraArgs = [],
        Code = "MR_par_builtin_new_future(Future);",
        generate_foreign_proc(ModuleName, PredName, pf_predicate,
            only_mode, detism_det, purity_pure, ForeignAttrs, Args, ExtraArgs,
            no, Code, Features, InstMapDelta, ModuleInfo, Context, AllocGoal)
    ).

    % Given a variable SharedVar of type SharedVarType, add a new variable
    % FutureVar of type future(SharedVarType).
    %
:- pred make_future_var(prog_var::in, mer_type::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var::out, mer_type::out) is det.

make_future_var(SharedVar, SharedVarType, !VarSet, !VarTypes,
        FutureVar, FutureVarType) :-
    FutureVarType = future_type(SharedVarType),
    varset.lookup_name(!.VarSet, SharedVar, SharedVarName),
    svvarset.new_named_var("Future" ++ SharedVarName, FutureVar, !VarSet),
    svmap.det_insert(FutureVar, FutureVarType, !VarTypes).

:- pred make_wait_goal(module_info::in, vartypes::in,
    prog_var::in, prog_var::in, hlds_goal::out) is det.

make_wait_goal(ModuleInfo, VarTypes, FutureVar, WaitVar, WaitGoal) :-
    make_wait_or_get(ModuleInfo, VarTypes, FutureVar, WaitVar, wait_pred,
        WaitGoal).

:- pred make_get_goal(module_info::in, vartypes::in, future_var_pair::in,
    hlds_goal::out) is det.

make_get_goal(ModuleInfo, VarTypes, future_var_pair(FutureVar, WaitVar),
        WaitGoal) :-
    make_wait_or_get(ModuleInfo, VarTypes, FutureVar, WaitVar, get_pred,
        WaitGoal).

:- type wait_or_get_pred
    --->    wait_pred
    ;       get_pred.

:- pred make_wait_or_get(module_info::in, vartypes::in,
    prog_var::in, prog_var::in, wait_or_get_pred::in, hlds_goal::out) is det.

make_wait_or_get(ModuleInfo, VarTypes, FutureVar, ConsumedVar, WaitOrGetPred,
        WaitGoal) :-
    ModuleName = mercury_par_builtin_module,
    (
        WaitOrGetPred = wait_pred,
        PredName = wait_future_pred_name,
        Code = "MR_par_builtin_wait_future(Future, Value);"
    ;
        WaitOrGetPred = get_pred,
        PredName = get_future_pred_name,
        Code = "MR_par_builtin_get_future(Future, Value);"
    ),
    Features = [],
    InstMapDelta = instmap_delta_bind_var(ConsumedVar),
    Context = term.context_init,
    ShouldInline = should_inline_par_builtin_calls(ModuleInfo),
    (
        ShouldInline = no,
        ArgVars = [FutureVar, ConsumedVar],
        generate_simple_call(ModuleName, PredName, pf_predicate,
            only_mode, detism_det, purity_pure, ArgVars, Features,
            InstMapDelta, ModuleInfo, Context, WaitGoal)
    ;
        ShouldInline = yes,
        ForeignAttrs = par_builtin_foreign_proc_attributes(purity_pure, no),
        Arg1 = foreign_arg(FutureVar, yes("Future" - in_mode),
            map.lookup(VarTypes, FutureVar), native_if_possible),
        Arg2 = foreign_arg(ConsumedVar, yes("Value" - out_mode),
            map.lookup(VarTypes, ConsumedVar), native_if_possible),
        Args = [Arg1, Arg2],
        ExtraArgs = [],
        generate_foreign_proc(ModuleName, PredName, pf_predicate,
            only_mode, detism_det, purity_pure, ForeignAttrs, Args, ExtraArgs,
            no, Code, Features, InstMapDelta, ModuleInfo, Context, WaitGoal)
    ).

:- pred make_signal_goal(module_info::in, future_map::in, prog_var::in,
    vartypes::in, hlds_goal::out) is det.

make_signal_goal(ModuleInfo, FutureMap, ProducedVar, VarTypes, SignalGoal) :-
    FutureVar = map.lookup(FutureMap, ProducedVar),
    ModuleName = mercury_par_builtin_module,
    PredName = signal_future_pred_name,
    Features = [],
    InstMapDelta = instmap_delta_bind_no_var,
    Context = term.context_init,
    ShouldInline = should_inline_par_builtin_calls(ModuleInfo),
    (
        ShouldInline = no,
        ArgVars = [FutureVar, ProducedVar],
        generate_simple_call(ModuleName, PredName, pf_predicate,
            only_mode, detism_det, purity_impure, ArgVars, Features,
            InstMapDelta, ModuleInfo, Context, SignalGoal)
    ;
        ShouldInline = yes,
        ForeignAttrs = par_builtin_foreign_proc_attributes(purity_impure,
            yes(needs_call_standard_output_registers)),
        Arg1 = foreign_arg(FutureVar, yes("Future" - in_mode),
            map.lookup(VarTypes, FutureVar), native_if_possible),
        Arg2 = foreign_arg(ProducedVar, yes("Value" - in_mode),
            map.lookup(VarTypes, ProducedVar), native_if_possible),
        Args = [Arg1, Arg2],
        ExtraArgs = [],
        Code = "MR_par_builtin_signal_future(Future, Value);",
        generate_foreign_proc(ModuleName, PredName, pf_predicate,
            only_mode, detism_det, purity_impure, ForeignAttrs,
            Args, ExtraArgs, no, Code, Features, InstMapDelta, ModuleInfo,
            Context, SignalGoal)
    ).

:- pred is_wait_goal(hlds_goal::in) is semidet.

is_wait_goal(hlds_goal(plain_call(_, _, _, _, _, SymName), _GoalInfo)) :-
    SymName = qualified(mercury_par_builtin_module, wait_future_pred_name).

:- pred is_signal_goal(hlds_goal::in) is semidet.

is_signal_goal(hlds_goal(plain_call(_, _, _, _, _, SymName), _GoalInfo)) :-
    SymName = qualified(mercury_par_builtin_module, signal_future_pred_name).

:- func new_future_pred_name = string.
:- func wait_future_pred_name = string.
:- func get_future_pred_name = string.
:- func signal_future_pred_name = string.

new_future_pred_name = "new_future".
wait_future_pred_name = "wait_future".
get_future_pred_name = "get_future".
signal_future_pred_name = "signal_future".

:- func should_inline_par_builtin_calls(module_info) = bool.

should_inline_par_builtin_calls(ModuleInfo) = ShouldInline :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, inline_par_builtins, ShouldInline).

:- func par_builtin_foreign_proc_attributes(purity,
    maybe(pragma_foreign_proc_extra_attribute))
    = pragma_foreign_proc_attributes.

par_builtin_foreign_proc_attributes(Purity, MaybeExtraAttr) = Attrs :-
    some [!Attrs] (
        !:Attrs = default_attributes(lang_c),
        set_may_call_mercury(proc_will_not_call_mercury, !Attrs),
        % Even signal is thread safe, since it does its own locking.
        set_thread_safe(proc_thread_safe, !Attrs),
        set_purity(Purity, !Attrs),
        set_terminates(proc_terminates, !Attrs),
        set_may_throw_exception(proc_will_not_throw_exception, !Attrs),
        set_may_modify_trail(proc_will_not_modify_trail, !Attrs),
        set_affects_liveness(proc_does_not_affect_liveness, !Attrs),
        set_allocates_memory(proc_allocates_bounded_memory, !Attrs),
        set_registers_roots(proc_does_not_register_roots, !Attrs),
        set_may_duplicate(yes(proc_may_duplicate), !Attrs),
        (
            MaybeExtraAttr = no
        ;
            MaybeExtraAttr = yes(ExtraAttr),
            add_extra_attribute(ExtraAttr, !Attrs)
        ),
        Attrs = !.Attrs
    ).

%-----------------------------------------------------------------------------%

:- pred conjoin_goal_and_goal_list_update_goal_infos(hlds_goal_info::in,
    hlds_goal::in, list(hlds_goal)::in, hlds_goal::out) is det.

conjoin_goal_and_goal_list_update_goal_infos(!.GoalInfo, GoalA, GoalsB,
        Goal) :-
    GoalA = hlds_goal(GoalExprA, _),

    ( GoalExprA = conj(plain_conj, GoalListA) ->
        GoalList = GoalListA ++ GoalsB
    ;
        GoalList = [GoalA | GoalsB]
    ),
    GoalExpr = conj(plain_conj, GoalList),

    goal_list_determinism(GoalList, Detism),
    goal_list_instmap_delta(GoalList, InstMapDelta),
    goal_list_nonlocals(GoalList, NonLocals),

    goal_info_set_nonlocals(NonLocals, !GoalInfo),
    goal_info_set_determinism(Detism, !GoalInfo),
    goal_info_set_instmap_delta(InstMapDelta, !GoalInfo),

    Goal = hlds_goal(GoalExpr, !.GoalInfo).

:- pred conjoin_goals_update_goal_infos(hlds_goal_info::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::out) is det.

conjoin_goals_update_goal_infos(!.GoalInfo, GoalA, GoalB, Goal) :-
    ( GoalB = hlds_goal(conj(plain_conj, GoalsB), _) ->
        GoalListB = GoalsB
    ;
        GoalListB = [GoalB]
    ),
    conjoin_goal_and_goal_list_update_goal_infos(!.GoalInfo, GoalA, GoalListB,
        Goal).

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
    % XXX This code is probably too complicated.  I think Thomas already had a
    % more elegant way to find the shared variables somewhere, using multisets.
    %
find_shared_variables(ModuleInfo, InstMap, Goals) = SharedVars :-
    list.map2(get_nonlocals_and_instmaps, Goals, Nonlocals, InstMapDeltas),
    find_shared_variables_2(ModuleInfo, 0, Nonlocals, InstMap, InstMapDeltas,
        set.init, SharedVars).

:- pred get_nonlocals_and_instmaps(hlds_goal::in,
    set(prog_var)::out, instmap_delta::out) is det.

get_nonlocals_and_instmaps(hlds_goal(_, GoalInfo), Nonlocals, InstMapDelta) :-
    Nonlocals = goal_info_get_nonlocals(GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo).

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
        instmap_lookup_var(InstMap, Var, VarInst),
        not inst_is_bound(ModuleInfo, VarInst)
    ),
    UnboundNonlocals = set.filter(Filter, Nonlocals),
    Changed =
        set.filter(changed_var(ModuleInfo, InstMapDeltasB), UnboundNonlocals),
    set.union(Changed, !SharedVars),
    find_shared_variables_2(ModuleInfo, ConjunctIndex+1, MoreNonlocals,
        InstMap, InstMapDeltas, !SharedVars).

:- pred changed_var(module_info::in, list(instmap_delta)::in, prog_var::in)
    is semidet.

changed_var(ModuleInfo, InstMapDeltas, UnboundVar) :-
    % Is the unbound nonlocal bound in one of the conjuncts?
    list.member(InstMapDelta, InstMapDeltas),
    instmap_delta_search_var(InstMapDelta, UnboundVar, Inst),
    inst_is_bound(ModuleInfo, Inst).

%-----------------------------------------------------------------------------%

:- pred fixup_and_reinsert_proc(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out) is det.

fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo, !ModuleInfo) :-
    requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
    recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
        !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

:- pred det_delete_nth(int::in, list(T)::in, list(T)::out) is det.

det_delete_nth(N, List0, List) :-
    list.det_split_list(N, List0, Left, Right),
    List = Left ++ det_tail(Right).

:- pred var_in_nonlocals(prog_var::in, hlds_goal::in) is semidet.

var_in_nonlocals(Var, Goal) :-
    set.member(Var, goal_get_nonlocals(Goal)).

:- pred var_not_in_nonlocals(prog_var::in, hlds_goal::in) is semidet.

var_not_in_nonlocals(Var, Goal) :-
    not var_in_nonlocals(Var, Goal).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "dep_par_conj.m".

%-----------------------------------------------------------------------------%
:- end_module dep_par_conj.
%-----------------------------------------------------------------------------%
