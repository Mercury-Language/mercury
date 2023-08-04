%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: dep_par_conj.m.
% Author: wangp.
%
% This module implements dependent parallel conjunction using HLDS->HLDS
% transformations. The overall process has two main components:
%
% - a synchronization transformation, and
% - a specialization transformation.
%
% 1 The synchronization transformation ensures that consumers do not access
%   shared variables before producers generate them. We do this by adding calls
%   to the synchronisation primitives defined in library/par_builtin.m.
%   In general, we make producers signal the availability of shared variables
%   as soon as possible, and we make consumers wait for the shared variables
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
% the conduit between the producer and the consumers, both for synchronization
% and for the transmission of the shared variable's value. Note that we
% create a new, distinct name for each shared variable in each consumer,
% so that after the transformation, the only variables that occur in more than
% one conjunct of the parallel conjunction are the variables that were already
% ground before the parallel conjunction is entered. (These include the future
% variables.)
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
%   implement the synchronization transformation.
%
% - Those whose names start with "find_specialization_requests" or include
%   "specialization" implement part (a) of the specialization transformation.
%
% - Those whose names start with "add_requested_specialized" implement part (b)
%   of the specialization transformation.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.dep_par_conj.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%---------------------------------------------------------------------------%

    % Transform all the parallel conjunctions in the procedures of this module
    % according to the scheme shown above.
    %
:- pred impl_dep_par_conjs_in_module(module_info::in, module_info::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.purity.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

impl_dep_par_conjs_in_module(!ModuleInfo) :-
    InitialModuleInfo = !.ModuleInfo,

    % Phase one: insert synchronization code into all parallel conjunctions
    % in the module.
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    module_info_get_ts_rev_string_table(!.ModuleInfo, _, RevTable0),
    make_ts_string_table(RevTable0, TSStringTable0),
    list.foldl3(maybe_sync_dep_par_conjs_in_pred, PredIds,
        !ModuleInfo, [], ProcsToScan, TSStringTable0, TSStringTable1),

    % Phase two: attempt to push the synchronization code inside procedures
    % as far as we can. We do this by creating specialized versions of
    % procedures. We do this to a fixpoint, since creating a specialized
    % version of a procedure may require us to create more specialized versions
    % of the other procedures.

    DoneParProcs0 = map.init,
    PendingParProcs0 = [],
    Pushability0 = map.init,
    RevProcMap0 = map.init,
    list.foldl4(
        find_specialization_requests_in_proc(DoneParProcs0, InitialModuleInfo),
        ProcsToScan, !ModuleInfo, PendingParProcs0, PendingParProcs,
        Pushability0, Pushability, RevProcMap0, RevProcMap),
    add_requested_specialized_par_procs(PendingParProcs, Pushability,
        DoneParProcs0, InitialModuleInfo, !ModuleInfo, RevProcMap, _,
        TSStringTable1, TSStringTable),
    module_info_set_ts_rev_string_table(TSStringTable ^ st_size,
        TSStringTable ^ st_rev_table, !ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
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
                sync_ignore_vars            :: set_of_progvar,

                % The value of the --allow-some-paths-only-waits option.
                % Read-only.
                sync_allow_some_paths_only  ::
                                            maybe_allow_some_paths_only_waits,

                % The var_table for the procedure being analysed.
                % This field is updated when we add new variables.
                % XXX We may also need the rtti_var_maps.
                sync_var_table              :: var_table,

                % The current procedure.
                sync_this_proc              :: pred_proc_id,

                % The current threadscope string table.
                sync_ts_string_table        :: ts_string_table
            ).

:- pred maybe_sync_dep_par_conjs_in_pred(pred_id::in,
    module_info::in, module_info::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    ts_string_table::in, ts_string_table::out) is det.

maybe_sync_dep_par_conjs_in_pred(PredId, !ModuleInfo, !ProcsToScan,
        !TSStringTable) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_non_imported_procids(PredInfo),
    list.foldl3(maybe_sync_dep_par_conjs_in_proc(PredId), ProcIds,
        !ModuleInfo, !ProcsToScan, !TSStringTable).

:- pred maybe_sync_dep_par_conjs_in_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    ts_string_table::in, ts_string_table::out) is det.

maybe_sync_dep_par_conjs_in_proc(PredId, ProcId, !ModuleInfo, !ProcsToScan,
        !TSStringTable) :-
    module_info_proc_info(!.ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    (
        HasParallelConj = has_no_parallel_conj
    ;
        HasParallelConj = has_parallel_conj,
        sync_dep_par_conjs_in_proc(PredId, ProcId, set_of_var.init,
            !ModuleInfo, !ProcsToScan, !TSStringTable)
    ).

:- pred sync_dep_par_conjs_in_proc(pred_id::in, proc_id::in,
    set_of_progvar::in, module_info::in, module_info::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out,
    ts_string_table::in, ts_string_table::out) is det.

sync_dep_par_conjs_in_proc(PredId, ProcId, IgnoreVars, !ModuleInfo,
        !ProcsToScan, !TSStringTable) :-
    some [!PredInfo, !ProcInfo, !Goal, !VarTable, !SyncInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, !:Goal),
        proc_info_get_var_table(!.ProcInfo, !:VarTable),
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        AllowSomePathsOnly = OptTuple ^ ot_allow_some_paths_only_waits,

        % We rely on dependency information in order to determine which calls
        % are recursive. The information is stored within !ModuleInfo, so it
        % doesn't need to be kept here; this call simply forces an update.
        module_info_rebuild_dependency_info(!ModuleInfo, _),

        GoalBeforeDepParConj = !.Goal,
        !:SyncInfo = sync_info(!.ModuleInfo, IgnoreVars, AllowSomePathsOnly,
            !.VarTable, proc(PredId, ProcId), !.TSStringTable),
        sync_dep_par_conjs_in_goal(!Goal, InstMap0, _, !SyncInfo),
        !.SyncInfo = sync_info(_, _, _, !:VarTable, _, !:TSStringTable),
        % XXX RTTI varmaps may need to be updated

        trace [compile_time(flag("debug-dep-par-conj")), io(!IO)] (
            globals.lookup_accumulating_option(Globals, debug_dep_par_conj,
                DebugDepParConjWords),
            PredIdInt = pred_id_to_int(PredId),
            PredIdStr = string.int_to_string(PredIdInt),
            ( if
                some [DebugDepParConjWord] (
                    list.member(DebugDepParConjWord, DebugDepParConjWords),
                    DebugDepParConjWord = PredIdStr
                )
            then
                OutInfo = init_hlds_out_info(Globals, output_debug),
                io.output_stream(Stream, !IO),
                io.format(Stream, "Pred/Proc: %s/%s before dep-par-conj:\n",
                    [s(string(PredId)), s(string(ProcId))], !IO),
                write_goal_nl(OutInfo, Stream, !.ModuleInfo,
                    vns_var_table(!.VarTable), print_name_and_num,
                    0, "", GoalBeforeDepParConj, !IO),
                io.nl(Stream, !IO),
                io.write_string(Stream, "After dep-par-conj:\n", !IO),
                write_goal_nl(OutInfo, Stream, !.ModuleInfo,
                    vns_var_table(!.VarTable), print_name_and_num,
                    0, "", !.Goal, !IO)
            else
                true
            )
        ),

        % We really only need to run this part if something changed, but we
        % only run this predicate on procedures which are likely to have
        % parallel conjunctions.
        proc_info_set_var_table(!.VarTable, !ProcInfo),
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
    instmap::in, instmap::out, sync_info::in, sync_info::out) is det.

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
            Goal0InstmapDelta = goal_info_get_instmap_delta(Goal0 ^ hg_info),
            ( if instmap_delta_is_unreachable(Goal0InstmapDelta) then
                % If the instmap becomes unreachable then calculating the
                % produces and consumers for the dependant parallel conjunction
                % transformation becomes impossible. Since this probably
                % throws an exception anyway there's no point parallelising it.
                % This should not be a compiler error. For instance in the
                % bug_130 test case a call to a deterministic predicate whose
                % body is erroneous is inlined. Generating an error in this
                % case would confuse the programmer.
                conj_list_to_goal(Goals, GoalInfo0, Goal)
            else
                maybe_sync_dep_par_conj(Goals, GoalInfo0, Goal, InstMap0,
                    !SyncInfo)
            )
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
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            Goal = Goal0
        else
            sync_dep_par_conjs_in_goal(SubGoal0, SubGoal, InstMap0, _,
                !SyncInfo),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
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

%---------------------------------------------------------------------------%

    % We found a parallel conjunction. Check for any dependencies between
    % the conjuncts and, if we find some, insert sychronisation primitives.
    %
:- pred maybe_sync_dep_par_conj(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out, instmap::in, sync_info::in, sync_info::out)
    is det.

maybe_sync_dep_par_conj(Conjuncts, GoalInfo, NewGoal, InstMap, !SyncInfo) :-
    !.SyncInfo = sync_info(ModuleInfo0, IgnoreVars, AllowSomePathsOnly,
        VarTable0, PredProcId, TSStringTable0),
    % Find the variables that are shared between conjuncts.
    SharedVars0 = find_shared_variables(ModuleInfo0, InstMap, Conjuncts),

    % Filter out all the variables which have already have associated futures,
    % i.e. they were head variables which were replaced by futures; signal and
    % wait calls will already have been inserted for them.
    SharedVars = set_of_var.filter(isnt(set_of_var.contains(IgnoreVars)),
        SharedVars0),

    ( if set_of_var.is_empty(SharedVars) then
        % Independent parallel conjunctions can sometimes be re-ordered to
        % generate faster code.
        module_info_get_globals(ModuleInfo0, Globals),
        globals.lookup_bool_option(Globals, par_loop_control, ParLoopControl),
        (
            ParLoopControl = no,
            reorder_indep_par_conj(PredProcId, VarTable0, InstMap, Conjuncts,
                GoalInfo, NewGoal, ModuleInfo0, ModuleInfo),
            !:SyncInfo = sync_info(ModuleInfo, IgnoreVars, AllowSomePathsOnly,
                VarTable0, PredProcId, TSStringTable0)
        ;
            ParLoopControl = yes,
            % Don't swap the conjuncts, parallel loop control can do a better
            % job of optimizing this code.
            NewGoal = hlds_goal(conj(parallel_conj, Conjuncts), GoalInfo)
        )
    else
        sync_dep_par_conj(ModuleInfo0, AllowSomePathsOnly, SharedVars,
            Conjuncts, GoalInfo, NewGoal, InstMap,
            VarTable0, VarTable, TSStringTable0, TSStringTable),
        !:SyncInfo = sync_info(ModuleInfo0, IgnoreVars, AllowSomePathsOnly,
            VarTable, PredProcId, TSStringTable)
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
:- pred sync_dep_par_conj(module_info::in,
    maybe_allow_some_paths_only_waits::in, set_of_progvar::in,
    list(hlds_goal)::in, hlds_goal_info::in, hlds_goal::out, instmap::in,
    var_table::in, var_table::out,
    ts_string_table::in, ts_string_table::out) is det.

sync_dep_par_conj(ModuleInfo, AllowSomePathsOnly, SharedVars, Goals, GoalInfo,
        NewGoal, InstMap, !VarTable, !TSStringTable) :-
    SharedVarsList = set_of_var.to_sorted_list(SharedVars),
    list.map_foldl3(allocate_future(ModuleInfo), SharedVarsList,
        AllocateFuturesGoals, !VarTable, map.init, FutureMap, !TSStringTable),
    list.condense(AllocateFuturesGoals, AllocateFutures),
    list.map_foldl2(
        sync_dep_par_conjunct(ModuleInfo, AllowSomePathsOnly, SharedVars,
            FutureMap),
        Goals, NewGoals, InstMap, _, !VarTable),

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

    % Add waits and signals into the body of a procedure. This is slightly
    % different from adding them to a parallel conjunct. We have to maintain
    % the extra invariant that the procedure guarantees that all futures have
    % been waited on, For futures that would have been inputs to the procedure
    % before the transformation).
    %
    % XXX: In some cases the pushed variable appears in the head of the
    % procedure but not in the body, that is to say it is an unused argument.
    % In these cases the specialization creates slower code than the original
    % procedure simply to maintain the above invariant. Can an unused argument
    % analysis prevent this situation?
    %
:- pred sync_dep_par_proc_body(module_info::in,
    maybe_allow_some_paths_only_waits::in, set_of_progvar::in,
    future_map::in, instmap::in, hlds_goal::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

sync_dep_par_proc_body(ModuleInfo, AllowSomePathsOnly, SharedVars, FutureMap,
        InstMap, !Goal, !VarTable) :-
    Nonlocals = goal_get_nonlocals(!.Goal),
    set_of_var.intersect(Nonlocals, SharedVars, NonlocalSharedVars),
    ( if set_of_var.is_empty(NonlocalSharedVars) then
        true
    else
        GoalInfo0 = !.Goal ^ hg_info,
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
        consumed_and_produced_vars(ModuleInfo, InstMap, InstMapDelta0,
            NonlocalSharedVars, ConsumedVarsList, ProducedVarsList),

        % Insert waits into the conjunct.
        list.foldl2(
            insert_wait_in_goal_for_proc(ModuleInfo,
                AllowSomePathsOnly, FutureMap),
            ConsumedVarsList, !Goal, !VarTable),

        % Insert signals into the conjunct, as early as possible.
        list.foldl2(insert_signal_in_goal(ModuleInfo, FutureMap),
            ProducedVarsList, !Goal, !VarTable)
    ),

    set_of_var.difference(SharedVars, Nonlocals, WaitAfterVars),
    ( if set_of_var.is_empty(WaitAfterVars) then
        true
    else
        % WaitAfterVars are pushed into this call but not consumed in the body.
        % Our caller expects them to be consumed by the time this call returns
        % so we must wait for them.
        list.foldl2(insert_wait_after_goal(ModuleInfo, FutureMap),
            set_of_var.to_sorted_list(WaitAfterVars),
            !Goal, !VarTable)
    ).

:- pred sync_dep_par_conjunct(module_info::in,
    maybe_allow_some_paths_only_waits::in, set_of_progvar::in, future_map::in,
    hlds_goal::in, hlds_goal::out, instmap::in, instmap::out,
    var_table::in, var_table::out) is det.

sync_dep_par_conjunct(ModuleInfo, AllowSomePathsOnly, SharedVars, FutureMap,
        !Goal, !InstMap, !VarTable) :-
    Nonlocals = goal_get_nonlocals(!.Goal),
    set_of_var.intersect(Nonlocals, SharedVars, NonlocalSharedVars),
    ( if set_of_var.is_empty(NonlocalSharedVars) then
        true
    else
        GoalInfo0 = !.Goal ^ hg_info,
        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
        consumed_and_produced_vars(ModuleInfo, !.InstMap, InstMapDelta0,
            NonlocalSharedVars, ConsumedVarsList, ProducedVarsList),

        % Insert waits into the conjunct, as late as possible.
        list.map_foldl2(
            insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap),
            ConsumedVarsList, _WaitedOnAllSuccessPaths,
            !Goal, !VarTable),

        % Insert signals into the conjunct, as early as possible.
        list.foldl2(insert_signal_in_goal(ModuleInfo, FutureMap),
            ProducedVarsList, !Goal, !VarTable),

        % Each consumer will have its own local name for the consumed variable,
        % so they can each wait for it when they need to.
        clone_variables(ConsumedVarsList, !.VarTable, !VarTable,
            map.init, Renaming),
        rename_some_vars_in_goal(Renaming, !Goal)
    ),
    update_instmap(!.Goal, !InstMap).

    % Divide the shared variables into
    % - those that are consumed by this conjunct, and
    % - those that are produced by it.
    %
:- pred consumed_and_produced_vars(module_info::in, instmap::in,
    instmap_delta::in, set_of_progvar::in,
    list(prog_var)::out, list(prog_var)::out) is det.

consumed_and_produced_vars(ModuleInfo, InstMap, InstMapDelta, Vars,
        ConsumedVarsList, ProducedVarsList) :-
    % XXX We should check that the initial instantiation of each variable
    % in ProducedVars in !.InstMap is free. However, at the moment, there
    % is nothing useful we can do if it isn't.
    IsProducedVar = var_is_bound_in_instmap_delta(ModuleInfo, InstMap,
        InstMapDelta),
    set_of_var.divide(IsProducedVar, Vars, ProducedVars, ConsumedVars),
    ConsumedVarsList = set_of_var.to_sorted_list(ConsumedVars),
    ProducedVarsList = set_of_var.to_sorted_list(ProducedVars).

:- pred insert_wait_in_goal_for_proc(module_info::in,
    maybe_allow_some_paths_only_waits::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

insert_wait_in_goal_for_proc(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, !Goal, !VarTable) :-
    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, WaitedOnAllSuccessPaths, !Goal, !VarTable),
    % If we did not wait on all success paths, then we must insert a wait here.
    % This preserves the invariant that a procedure is called with a future
    % that it should wait on, it will actually wait on it in all cases.
    % This way, any future_get calls after such a call are safe.
    (
        WaitedOnAllSuccessPaths = waited_on_all_success_paths
    ;
        WaitedOnAllSuccessPaths = not_waited_on_all_success_paths,
        insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
            !Goal, !VarTable)
    ).

%---------------------------------------------------------------------------%

:- type waited_on_all_success_paths
    --->    waited_on_all_success_paths
    ;       not_waited_on_all_success_paths.

:- pred join_branches(waited_on_all_success_paths::in,
    waited_on_all_success_paths::in, waited_on_all_success_paths::out) is det.

join_branches(WaitedA, WaitedB, Waited) :-
    ( if
        WaitedA = waited_on_all_success_paths,
        WaitedB = waited_on_all_success_paths
    then
        Waited = waited_on_all_success_paths
    else
        Waited = not_waited_on_all_success_paths
    ).

    % insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar, Goal0, Goal,
    %   !VarTable):
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
:- pred insert_wait_in_goal(module_info::in,
    maybe_allow_some_paths_only_waits::in, future_map::in,
    prog_var::in, waited_on_all_success_paths::out,
    hlds_goal::in, hlds_goal::out, var_table::in, var_table::out) is det.

insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        WaitedOnAllSuccessPaths, Goal0, Goal, !VarTable) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    % InvariantEstablished should be true if AllowSomePathsOnly = no
    % implies WaitedOnAllSuccessPaths0 = waited_on_all_success_paths.
    ( if var_in_nonlocals(Goal0, ConsumedVar) then
        (
            GoalExpr0 = conj(ConjType, Goals0),
            InvariantEstablished = yes,
            (
                ConjType = plain_conj,
                insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar, WaitedOnAllSuccessPaths0,
                    Goals0, Goals, !VarTable)
            ;
                ConjType = parallel_conj,
                insert_wait_in_par_conj(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    have_not_waited_in_conjunct, WaitedInConjunct,
                    Goals0, Goals, !VarTable),
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
                    FirstDisjunct0, FirstDisjunct, !VarTable),
                insert_wait_in_disj(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    FirstWaitedOnAllSuccessPaths, WaitedOnAllSuccessPaths0,
                    LaterDisjuncts0, LaterDisjuncts, !VarTable),
                Disjuncts = [FirstDisjunct | LaterDisjuncts],
                GoalExpr = disj(Disjuncts),
                Goal1 = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
            InvariantEstablished = yes,
            ( if ConsumedVar = SwitchVar then
                insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Goal0, Goal1, !VarTable),
                WaitedOnAllSuccessPaths0 = waited_on_all_success_paths
            else
                (
                    Cases0 = [],
                    unexpected($pred, "no cases")
                ;
                    Cases0 = [FirstCase0 | LaterCases0],
                    FirstCase0 = case(MainConsId, OtherConsIds, FirstGoal0),
                    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                        FutureMap, ConsumedVar,
                        FirstWaitedOnAllSuccessPaths,
                        FirstGoal0, FirstGoal, !VarTable),
                    FirstCase = case(MainConsId, OtherConsIds, FirstGoal),
                    insert_wait_in_cases(ModuleInfo, AllowSomePathsOnly,
                        FutureMap, ConsumedVar,
                        FirstWaitedOnAllSuccessPaths, WaitedOnAllSuccessPaths0,
                        LaterCases0, LaterCases, !VarTable),
                    Cases = [FirstCase | LaterCases],
                    GoalExpr = switch(SwitchVar, CanFail, Cases),
                    Goal1 = hlds_goal(GoalExpr, GoalInfo0)
                )
            )
        ;
            GoalExpr0 = if_then_else(Quant, Cond, Then0, Else0),
            InvariantEstablished = yes,
            ( if var_in_nonlocals(Cond, ConsumedVar) then
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
                    Goal0, Goal1, !VarTable)
            else
                % If ConsumedVar is not in the nonlocals of Cond, then it
                % must be in the nonlocals of at least one of Then0 and Else0.
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    ThenWaitedOnAllSuccessPaths,
                    Then0, Then, !VarTable),
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar,
                    ElseWaitedOnAllSuccessPaths,
                    Else0, Else, !VarTable),
                join_branches(ThenWaitedOnAllSuccessPaths,
                    ElseWaitedOnAllSuccessPaths, WaitedOnAllSuccessPaths0),
                GoalExpr = if_then_else(Quant, Cond, Then, Else),
                Goal1 = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            InvariantEstablished = yes,
            ( if Reason = from_ground_term(_, from_ground_term_construct) then
                % These scopes do not consume anything.
                unexpected($pred, "from_ground_term_construct")
            else
                % XXX If Reason = from_ground_term(X,
                % from_ground_term_deconstruct), then the only variable
                % that we can wait for is X. We should be able to use that fact
                % to avoid processing SubGoal0.
                insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
                    FutureMap, ConsumedVar, WaitedOnAllSuccessPaths0,
                    SubGoal0, SubGoal, !VarTable),
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
                Goal0, Goal1, !VarTable)
        ;
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            InvariantEstablished = no,
            WaitedOnAllSuccessPaths0 = waited_on_all_success_paths,
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal1, !VarTable)
        ;
            GoalExpr0 = shorthand(_),
            unexpected($pred, "shorthand")
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
            clone_variable(ConsumedVar, !.VarTable, !VarTable,
                map.init, Renaming, _CloneVar),
            rename_some_vars_in_goal(Renaming, Goal1, Goal2)
        )
    else
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
                AllowSomePathsOnly = allow_some_paths_only_waits,
                WaitedOnAllSuccessPaths = WaitedOnAllSuccessPaths0,
                Goal = Goal2
            ;
                AllowSomePathsOnly = do_not_allow_some_paths_only_waits,
                (
                    InvariantEstablished = no,
                    WaitedOnAllSuccessPaths = waited_on_all_success_paths,
                    insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
                        Goal2, Goal, !VarTable)
                ;
                    InvariantEstablished = yes,
                    unexpected($pred,
                        "not_waited_on_all_success_paths invariant violation")
                )
            )
        )
    ).

:- pred insert_wait_before_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarTable) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    make_wait_goal(ModuleInfo, !.VarTable, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hg_info, WaitGoal, Goal0, Goal).

:- pred insert_wait_after_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarTable) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    make_wait_goal(ModuleInfo, !.VarTable, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hg_info, Goal0, WaitGoal, Goal).

    % Insert a wait for ConsumedVar in the first goal in the conjunction
    % that references it. Any later conjuncts will get the waited-for variable
    % without having to call wait.
    %
:- pred insert_wait_in_plain_conj(module_info::in,
    maybe_allow_some_paths_only_waits::in, future_map::in,
    prog_var::in, waited_on_all_success_paths::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

insert_wait_in_plain_conj(_, _, _, _,
        not_waited_on_all_success_paths, [], [], !VarTable).
insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, WaitedOnAllSuccessPaths,
        [FirstGoal0 | LaterGoals0], Goals, !VarTable) :-
    ( if var_in_nonlocals(FirstGoal0, ConsumedVar) then
        insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly,
            FutureMap, ConsumedVar, GoalWaitedOnAllSuccessPaths,
            FirstGoal0, FirstGoal, !VarTable),
        (
            GoalWaitedOnAllSuccessPaths = waited_on_all_success_paths,
            % We wait for ConsumedVar on all paths in FirstGoal that can lead
            % to LaterGoals0, so the code in LaterGoals0 will be able to
            % access ConsumedVar without any further waiting.
            WaitedOnAllSuccessPaths = waited_on_all_success_paths,
            LaterGoals = LaterGoals0
        ;
            GoalWaitedOnAllSuccessPaths = not_waited_on_all_success_paths,
            % We waited for ConsumedVar on some but not all paths in FirstGoal
            % that can lead to LaterGoals0. LaterGoals may therefore also wait
            % for ConsumedVar, and any such waits will also bind ConsumedVar.
            % We do not want both FirstGoal and LaterGoals binding ConsumedVar,
            % so the call to insert_wait_in_goal above has replaced all
            % occurrences of ConsumedVar in FirstGoal0 with a fresh variable.
            insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly,
                FutureMap, ConsumedVar, WaitedOnAllSuccessPaths,
                LaterGoals0, LaterGoals, !VarTable)
        ),
        ( if FirstGoal ^ hg_expr = conj(plain_conj, FirstGoalConj) then
            Goals = FirstGoalConj ++ LaterGoals
        else
            Goals = [FirstGoal | LaterGoals]
        )
    else
        % ConsumedVar does not appear in FirstGoal0, so wait for it
        % in LaterGoals0.
        insert_wait_in_plain_conj(ModuleInfo, AllowSomePathsOnly, FutureMap,
            ConsumedVar, WaitedOnAllSuccessPaths, LaterGoals0, LaterGoals1,
            !VarTable),
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
:- pred insert_wait_in_par_conj(module_info::in,
    maybe_allow_some_paths_only_waits::in, future_map::in, prog_var::in,
    waited_in_conjunct::in, waited_in_conjunct::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

insert_wait_in_par_conj(_, _, _, _, !WaitedInConjunct, [], [], !VarTable).
insert_wait_in_par_conj(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedInConjunct, [Goal0 | Goals0], [Goal | Goals], !VarTable) :-
    ( if var_in_nonlocals(Goal0, ConsumedVar) then
        % ConsumedVar appears in Goal0, so wait for it in Goal0, but the code
        % in Goals0 will *not* be able to access ConsumedVar without waiting,
        % since the conjuncts are executed independently.
        insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap,
            ConsumedVar, WaitedOnAllSuccessPaths, Goal0, Goal1, !VarTable),
        (
            !.WaitedInConjunct = have_not_waited_in_conjunct,
            !:WaitedInConjunct = waited_in_conjunct(WaitedOnAllSuccessPaths),
            Goal = Goal1
        ;
            !.WaitedInConjunct = waited_in_conjunct(_),
            % This is not the first conjunct that waits for ConsumedVar,
            % so we shouldn't update the argument of waited_in_conjunct.
            clone_variable(ConsumedVar, !.VarTable, !VarTable,
                map.init, Renaming, _CloneVar),
            rename_some_vars_in_goal(Renaming, Goal1, Goal)
        )
    else
        Goal = Goal0
    ),
    insert_wait_in_par_conj(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, !WaitedInConjunct, Goals0, Goals, !VarTable).

:- pred insert_wait_in_disj(module_info::in,
    maybe_allow_some_paths_only_waits::in, future_map::in, prog_var::in,
    waited_on_all_success_paths::in, waited_on_all_success_paths::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

insert_wait_in_disj(_, _, _, _, !WaitedOnAllSuccessPaths, [], [], !VarTable).
insert_wait_in_disj(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, [Goal0 | Goals0], [Goal | Goals],
        !VarTable) :-
    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        FirstWaitedOnAllSuccessPaths,
        Goal0, Goal, !VarTable),
    join_branches(FirstWaitedOnAllSuccessPaths, !WaitedOnAllSuccessPaths),
    insert_wait_in_disj(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, Goals0, Goals, !VarTable).

:- pred insert_wait_in_cases(module_info::in,
    maybe_allow_some_paths_only_waits::in, future_map::in, prog_var::in,
    waited_on_all_success_paths::in, waited_on_all_success_paths::out,
    list(case)::in, list(case)::out, var_table::in, var_table::out) is det.

insert_wait_in_cases(_, _, _, _, !WaitedOnAllSuccessPaths, [], [], !VarTable).
insert_wait_in_cases(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        !WaitedOnAllSuccessPaths, [Case0 | Cases0], [Case | Cases],
        !VarTable) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    insert_wait_in_goal(ModuleInfo, AllowSomePathsOnly, FutureMap, ConsumedVar,
        FirstWaitedOnAllSuccessPaths, Goal0, Goal, !VarTable),
    Case = case(MainConsId, OtherConsIds, Goal),
    join_branches(FirstWaitedOnAllSuccessPaths, !WaitedOnAllSuccessPaths),
    insert_wait_in_cases(ModuleInfo, AllowSomePathsOnly, FutureMap,
        ConsumedVar, !WaitedOnAllSuccessPaths, Cases0, Cases, !VarTable).

%---------------------------------------------------------------------------%

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
    hlds_goal::in, hlds_goal::out, var_table::in, var_table::out) is det.

insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarTable) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Detism = goal_info_get_determinism(GoalInfo0),
    determinism_components(Detism, _CanFail, NumSolutions),
    (
        ( NumSolutions = at_most_one
        ; NumSolutions = at_most_many_cc
        ; NumSolutions = at_most_many
        ),
        ( if var_in_nonlocals(Goal0, ProducedVar) then
            (
                GoalExpr0 = conj(ConjType, Goals0),
                (
                    ConjType = plain_conj,
                    insert_signal_in_plain_conj(ModuleInfo, FutureMap,
                        ProducedVar, Goals0, Goals, !VarTable)
                ;
                    ConjType = parallel_conj,
                    insert_signal_in_par_conj(ModuleInfo, FutureMap,
                        ProducedVar, Goals0, Goals, !VarTable)
                ),
                GoalExpr = conj(ConjType, Goals),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                GoalExpr0 = disj(Goals0),
                insert_signal_in_disj(ModuleInfo, FutureMap, ProducedVar,
                    Goals0, Goals, !VarTable),
                GoalExpr = disj(Goals),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
                ( if ProducedVar = SwitchVar then
                    unexpected($pred, "switch on unbound shared variable")
                else
                    insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
                        Cases0, Cases, !VarTable),
                    GoalExpr = switch(SwitchVar, CanFail, Cases),
                    Goal = hlds_goal(GoalExpr, GoalInfo0)
                )
            ;
                GoalExpr0 = if_then_else(QuantVars, Cond, Then0, Else0),
                expect(var_not_in_nonlocals(Cond, ProducedVar), $pred,
                    "condition binds shared variable"),
                insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
                    Then0, Then, !VarTable),
                insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
                    Else0, Else, !VarTable),
                GoalExpr = if_then_else(QuantVars, Cond, Then, Else),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                GoalExpr0 = negation(_),
                unexpected($pred, "negation binds shared variable")
            ;
                GoalExpr0 = scope(Reason, SubGoal0),
                ( if
                    Reason = from_ground_term(_, from_ground_term_construct)
                then
                    % Pushing the signal into the scope would invalidate the
                    % invariant that from_ground_term_construct scopes do
                    % nothing except construct a ground term. It would also be
                    % pointless, since the code generator will turn the entire
                    % scope into a single assignment statement. We therefore
                    % put he signal *after* the scope.
                    insert_signal_after_goal(ModuleInfo, FutureMap,
                        ProducedVar, Goal0, Goal, !VarTable)
                else
                    SubGoal0 = hlds_goal(_, SubGoalInfo0),
                    Detism0 = goal_info_get_determinism(GoalInfo0),
                    SubDetism0 = goal_info_get_determinism(SubGoalInfo0),
                    determinism_components(Detism0, _, MaxSolns0),
                    determinism_components(SubDetism0, _, SubMaxSolns0),
                    ( if
                        SubMaxSolns0 = at_most_many,
                        MaxSolns0 \= at_most_many
                    then
                        % The value of ProducedVar is not stable inside
                        % SubGoal0, i.e. SubGoal0 can generate a value for
                        % ProducedVar and then backtrack over the goal that
                        % generated it. In such cases, we can signal the
                        % availability of ProducedVar only when it has become
                        % stable, which is when the scope has cut away any
                        % possibility of further backtracking inside SubGoal0.
                        insert_signal_after_goal(ModuleInfo, FutureMap,
                            ProducedVar, Goal0, Goal, !VarTable)
                    else
                        insert_signal_in_goal(ModuleInfo, FutureMap,
                            ProducedVar, SubGoal0, SubGoal, !VarTable),
                        GoalExpr = scope(Reason, SubGoal),
                        Goal = hlds_goal(GoalExpr, GoalInfo0)
                    )
                )
            ;
                ( GoalExpr0 = unify(_, _, _, _, _)
                ; GoalExpr0 = plain_call(_, _, _, _, _, _)
                ; GoalExpr0 = generic_call(_, _, _, _, _)
                ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
                ),
                insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
                    Goal0, Goal, !VarTable)
            ;
                GoalExpr0 = shorthand(_),
                unexpected($pred, "shorthand")
            )
        else
            % We expected this goal to produce the variable that
            % we are looking for.
            unexpected($pred, "ProducedVar is not in nonlocals")
        )
    ;
        NumSolutions = at_most_zero,
        % We don't bother pushing signals into code that has no solutions.
        % Note that we can't call unexpected here since we could be trying to
        % push a signal into a procedure during specialisation. We must fail
        % gracefully.
        Goal = Goal0
    ).

:- pred insert_signal_after_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    var_table::in, var_table::out) is det.

insert_signal_after_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarTable) :-
    make_signal_goal(ModuleInfo, !.VarTable, FutureMap, ProducedVar,
        SignalGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hg_info, Goal0, SignalGoal, Goal).

:- pred insert_signal_in_plain_conj(module_info::in, future_map::in,
    prog_var::in, list(hlds_goal)::in, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

insert_signal_in_plain_conj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarTable).
insert_signal_in_plain_conj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], Goals, !VarTable) :-
    ( if var_in_nonlocals(Goal0, ProducedVar) then
        % The first conjunct that mentions ProducedVar should bind ProducedVar.
        % Since we don't recurse in this case, we get here only for the first
        % conjunct.
        Goal0 = hlds_goal(_, GoalInfo0),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        expect(set_of_var.contains(ChangedVars, ProducedVar), $pred,
            "ProducedVar not in ChangedVars"),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal1, !VarTable),
        ( if Goal1 ^ hg_expr = conj(plain_conj, GoalConjs1) then
            Goals = GoalConjs1 ++ Goals0
        else
            Goals = [Goal1 | Goals0]
        )
    else
        insert_signal_in_plain_conj(ModuleInfo, FutureMap, ProducedVar,
            Goals0, Goals1, !VarTable),
        Goals = [Goal0 | Goals1]
    ).

:- pred insert_signal_in_par_conj(module_info::in, future_map::in,
    prog_var::in, list(hlds_goal)::in, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

insert_signal_in_par_conj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarTable).
insert_signal_in_par_conj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarTable) :-
    ( if var_in_nonlocals(Goal0, ProducedVar) then
        % The first conjunct that mentions ProducedVar should bind ProducedVar.
        % Since we don't recurse in this case, we get here only for the first
        % conjunct.
        Goal0 = hlds_goal(_, GoalInfo0),
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        expect(set_of_var.contains(ChangedVars, ProducedVar), $pred,
            "ProducedVar not in ChangedVars"),
        insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
            Goal0, Goal, !VarTable),
        Goals = Goals0
    else
        Goal = Goal0,
        insert_signal_in_par_conj(ModuleInfo, FutureMap, ProducedVar,
            Goals0, Goals, !VarTable)
    ).

:- pred insert_signal_in_disj(module_info::in, future_map::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    var_table::in, var_table::out) is det.

insert_signal_in_disj(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarTable).
insert_signal_in_disj(ModuleInfo, FutureMap, ProducedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarTable) :-
    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarTable),
    insert_signal_in_disj(ModuleInfo, FutureMap, ProducedVar,
        Goals0, Goals, !VarTable).

:- pred insert_signal_in_cases(module_info::in, future_map::in, prog_var::in,
    list(case)::in, list(case)::out, var_table::in, var_table::out) is det.

insert_signal_in_cases(_ModuleInfo, _FutureMap, _ProducedVar,
        [], [], !VarTable).
insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
        [Case0 | Cases0], [Case | Cases], !VarTable) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
        Goal0, Goal, !VarTable),
    Case = case(MainConsId, OtherConsIds, Goal),
    insert_signal_in_cases(ModuleInfo, FutureMap, ProducedVar,
        Cases0, Cases, !VarTable).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The independent parallel conjunction re-ordering transformation.
%

:- pred reorder_indep_par_conj(pred_proc_id::in, var_table::in, instmap::in,
    hlds_goals::in, hlds_goal_info::in, hlds_goal::out,
    module_info::in, module_info::out) is det.

reorder_indep_par_conj(PredProcId, VarTable, InstMapBefore, Conjuncts0,
        GoalInfo, Goal, !ModuleInfo) :-
    module_info_dependency_info(!.ModuleInfo, DependencyInfo),
    Ordering = dependency_info_get_bottom_up_sccs(DependencyInfo),
    find_procs_scc(Ordering, PredProcId, SCC),
    CallsToSameSCC = goal_list_calls_proc_in_set(Conjuncts0, SCC),
    ( if set.is_empty(CallsToSameSCC) then
        % The conjunction doesn't contain a recursive or mutually-recursive
        % call, so this optimisation does not apply.
        Conjuncts = Conjuncts0
    else
        reorder_indep_par_conj_2(SCC, VarTable, InstMapBefore, Conjuncts0,
            Conjuncts, !ModuleInfo)
    ),
    GoalExpr = conj(parallel_conj, Conjuncts),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred reorder_indep_par_conj_2(scc::in, var_table::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    module_info::in, module_info::out) is det.

reorder_indep_par_conj_2(_, _, _, [], [], !ModuleInfo).
reorder_indep_par_conj_2(SCC, VarTable, InstMapBefore, [Goal | Goals0],
        Goals, !ModuleInfo) :-
    apply_instmap_delta(goal_info_get_instmap_delta(Goal ^ hg_info),
        InstMapBefore, InstMapBeforeGoals0),
    reorder_indep_par_conj_2(SCC, VarTable, InstMapBeforeGoals0, Goals0,
        Goals1, !ModuleInfo),
    % These instmaps are equal since they both still apply Goal's instmap
    % delta.
    InstMapBeforeGoals1 = InstMapBeforeGoals0,

    % If Goal is non recursive, try to push it down into the conjunction.
    ( if
        set.member(CallPredProcId, SCC),
        goal_calls(Goal, CallPredProcId)
    then
        % Goal is recursive.
        Goals = [Goal | Goals1]
    else
        % Goal is non-recursive.
        push_goal_into_conj(VarTable, InstMapBefore, Goal, InstMapBeforeGoals1,
            Goals1, MaybeGoals, !ModuleInfo),
        (
            MaybeGoals = yes(Goals)
        ;
            MaybeGoals = no,
            Goals = [Goal | Goals1]
        )
    ).

:- pred push_goal_into_conj(var_table::in, instmap::in, hlds_goal::in,
    instmap::in, hlds_goals::in, maybe(hlds_goals)::out,
    module_info::in, module_info::out) is det.

push_goal_into_conj(_, _, Goal, _, [], yes([Goal]), !ModuleInfo).
push_goal_into_conj(VarTable, InstMapBeforeGoal, Goal, InstMapBeforePivotGoal,
        [PivotGoal | Goals0], MaybeGoals, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    lookup_bool_option(Globals, fully_strict, FullyStrict),
    can_reorder_goals(VarTable, FullyStrict,
        InstMapBeforeGoal, Goal, InstMapBeforePivotGoal, PivotGoal,
        CanReorderGoals, !ModuleInfo),
    (
        CanReorderGoals = can_reorder_goals,
        % InstMapBeforeGoalAfterPivot represents the inst map before Goal given
        % that it has already been swapped with PivotGoal, that is PivotGoal
        % occurs before Goal.
        PivotInstMapDelta = goal_info_get_instmap_delta(PivotGoal ^ hg_info),
        apply_instmap_delta(PivotInstMapDelta,
            InstMapBeforeGoal, InstMapBeforeGoalAfterPivot),

        GoalInstMapDelta = goal_info_get_instmap_delta(Goal ^ hg_info),
        apply_instmap_delta(GoalInstMapDelta,
            InstMapBeforeGoalAfterPivot, InstMapAfterPivotAndGoal),

        push_goal_into_conj(VarTable, InstMapBeforeGoalAfterPivot, Goal,
            InstMapAfterPivotAndGoal, Goals0, MaybeGoals1, !ModuleInfo),
        (
            MaybeGoals1 = yes(Goals1),
            Goals = [PivotGoal | Goals1]
        ;
            MaybeGoals1 = no,
            Goals = [Goal, PivotGoal | Goals0]
        ),
        MaybeGoals = yes(Goals)
    ;
        CanReorderGoals = cannot_reorder_goals,
        MaybeGoals = no
    ).

:- pred find_procs_scc(list(scc)::in, pred_proc_id::in, scc::out) is det.

find_procs_scc([], _PredProcId, _) :-
    unexpected($pred, "Couldn't find SCC for pred/proc id.").
find_procs_scc([SCC | SCCs], PredProcId, PredProcsSCC) :-
    ( if set.member(PredProcId, SCC) then
        PredProcsSCC = SCC
    else
        find_procs_scc(SCCs, PredProcId, PredProcsSCC)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
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
                spec_rev_proc_map           :: rev_proc_map,

                % The version of the module before dep_par_conj.m started
                % modifying it. This field is constant; it should never be
                % updated.
                spec_initial_module         :: module_info,

                % The procedure that's currently being scanned.
                spec_ppid                   :: pred_proc_id,

                % The variable types of the procedure we are scanning.
                % This field is constant; it should never be updated.
                spec_var_table              :: var_table,

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

    % A map from specialised pred proc ids back to the pred proc id of the
    % procedure that they are based on.
    %
:- type rev_proc_map == map(pred_proc_id, pred_proc_id).

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

%---------------------------------------------------------------------------%

:- pred find_specialization_requests_in_proc(done_par_procs::in,
    module_info::in, pred_proc_id::in, module_info::in, module_info::out,
    pending_par_procs::in, pending_par_procs::out,
    pushable_args_map::in, pushable_args_map::out,
    rev_proc_map::in, rev_proc_map::out) is det.

find_specialization_requests_in_proc(DoneProcs, InitialModuleInfo, PredProcId,
        !ModuleInfo, !PendingParProcs, !Pushability, !RevProcMap) :-
    PredProcId = proc(PredId, ProcId),
    some [!PredInfo, !ProcInfo, !Goal, !SpecInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_var_table(!.ProcInfo, VarTable),
        proc_info_get_goal(!.ProcInfo, !:Goal),
        !:SpecInfo = spec_info(DoneProcs, !.RevProcMap, InitialModuleInfo,
            PredProcId, VarTable, !.ModuleInfo, !.PendingParProcs,
            !.Pushability),

        trace [compile_time(flag("debug-dep-par-conj")), io(!IO)] (
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_accumulating_option(Globals, debug_dep_par_conj,
                DebugDepParConjWords),
            PredIdInt = pred_id_to_int(PredId),
            PredIdStr = string.int_to_string(PredIdInt),
            ( if
                some [DebugDepParConjWord] (
                    list.member(DebugDepParConjWord, DebugDepParConjWords),
                    DebugDepParConjWord = PredIdStr
                )
            then
                io.output_stream(Stream, !IO),
                OutInfo = init_hlds_out_info(Globals, output_debug),
                io.format(Stream,
                    "About to search %d/%d for dependant par conjs:\n",
                    [i(PredIdInt), i(proc_id_to_int(ProcId))], !IO),
                write_goal_nl(OutInfo, Stream, !.ModuleInfo,
                    vns_var_table(VarTable), print_name_and_num,
                    0, "", !.Goal, !IO)
            else
                true
            )
        ),

        specialize_sequences_in_goal(!Goal, !SpecInfo),
        !.SpecInfo = spec_info(_, !:RevProcMap, _, _, _,
            !:ModuleInfo, !:PendingParProcs, !:Pushability),
        proc_info_set_goal(!.Goal, !ProcInfo),
        % Optimization opportunity: we should not fix up the same procedure
        % twice, i.e. first in sync_dep_par_conjs_in_proc and then here.
        fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

:- pred add_requested_specialized_par_procs(pending_par_procs::in,
    pushable_args_map::in, done_par_procs::in, module_info::in,
    module_info::in, module_info::out, rev_proc_map::in, rev_proc_map::out,
    ts_string_table::in, ts_string_table::out) is det.

add_requested_specialized_par_procs(!.PendingParProcs,
        !.Pushability, !.DoneParProcs, InitialModuleInfo,
        !ModuleInfo, !RevProcMap, !TSStringTable) :-
    (
        !.PendingParProcs = []
    ;
        !.PendingParProcs = [CallPattern - NewProc | !:PendingParProcs],
        % Move the procedure we are about to parallelise into the list of
        % done procedures, in case of recursive calls.
        map.det_insert(CallPattern, NewProc, !DoneParProcs),
        add_requested_specialized_par_proc(CallPattern, NewProc,
            !PendingParProcs, !Pushability, !.DoneParProcs, InitialModuleInfo,
            !ModuleInfo, !RevProcMap, !TSStringTable),
        disable_warning [suspicious_recursion] (
            add_requested_specialized_par_procs(!.PendingParProcs,
                !.Pushability, !.DoneParProcs, InitialModuleInfo,
                !ModuleInfo, !RevProcMap, !TSStringTable)
        )
    ).

:- pred add_requested_specialized_par_proc(par_proc_call_pattern::in,
    new_par_proc::in, pending_par_procs::in, pending_par_procs::out,
    pushable_args_map::in, pushable_args_map::out, done_par_procs::in,
    module_info::in, module_info::in, module_info::out,
    rev_proc_map::in, rev_proc_map::out,
    ts_string_table::in, ts_string_table::out) is det.

add_requested_specialized_par_proc(CallPattern, NewProc, !PendingParProcs,
        !Pushability, DoneParProcs, InitialModuleInfo, !ModuleInfo,
        !RevProcMap, !TSStringTable) :-
    CallPattern = par_proc_call_pattern(OldPredProcId, FutureArgs),
    NewProc = new_par_proc(NewPredProcId, _Name),
    OldPredProcId = proc(OldPredId, OldProcId),
    NewPredProcId = proc(NewPredId, NewProcId),

    some [!VarTable, !NewProcInfo] (
        % Get the proc_info from _before_ the dependent parallel conjunction
        % pass was ever run, so we get untransformed procedure bodies.
        % Our transformation does not attempt to handle already transformed
        % parallel conjunctions.
        module_info_proc_info(InitialModuleInfo, OldPredId, OldProcId,
            !:NewProcInfo),
        proc_info_get_var_table(!.NewProcInfo, !:VarTable),
        proc_info_get_headvars(!.NewProcInfo, HeadVars0),
        proc_info_get_argmodes(!.NewProcInfo, ArgModes0),
        proc_info_get_goal(!.NewProcInfo, Goal0),
        proc_info_get_initial_instmap(InitialModuleInfo, !.NewProcInfo,
            InstMap0),

        % Set up the mapping from head variables to futures.
        list.foldl2(map_arg_to_new_future(HeadVars0), FutureArgs,
            map.init, FutureMap, !VarTable),

        % Replace head variables by their futures.
        replace_head_vars(!.ModuleInfo, FutureMap,
            HeadVars0, HeadVars, ArgModes0, ArgModes),

        % Insert signals and waits into the procedure body. We treat the body
        % as it were a conjunct of a parallel conjunction, since it is.
        module_info_get_globals(InitialModuleInfo, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        AllowSomePathsOnly = OptTuple ^ ot_allow_some_paths_only_waits,
        SharedVars = set_of_var.sorted_list_to_set(map.keys(FutureMap)),
        sync_dep_par_proc_body(!.ModuleInfo, AllowSomePathsOnly, SharedVars,
            FutureMap, InstMap0, Goal0, Goal, !VarTable),

        proc_info_set_var_table(!.VarTable, !NewProcInfo),
        proc_info_set_headvars(HeadVars, !NewProcInfo),
        proc_info_set_argmodes(ArgModes, !NewProcInfo),
        proc_info_set_goal(Goal, !NewProcInfo),

        module_info_pred_info(!.ModuleInfo, NewPredId, NewPredInfo0),

        % Mark this predicate impure if it no longer has any output arguments
        % (having been replaced by a future, which is an input argument which
        % is destructively updated).
        ( if any_output_arguments(!.ModuleInfo, ArgModes) then
            NewPredInfo = NewPredInfo0
        else
            pred_info_get_markers(NewPredInfo0, Markers0),
            add_marker(marker_is_impure, Markers0, Markers),
            pred_info_set_markers(Markers, NewPredInfo0, NewPredInfo)
        ),

        fixup_and_reinsert_proc(NewPredId, NewProcId, NewPredInfo,
            !.NewProcInfo, !ModuleInfo),

        % Look for and process any dependent parallel conjunctions inside
        % the newly created (sort of; the previous version was only a
        % placeholder) specialized procedure.
        IgnoreVars = set_of_var.sorted_list_to_set(map.keys(FutureMap)),
        sync_dep_par_conjs_in_proc(NewPredId, NewProcId, IgnoreVars,
            !ModuleInfo, [], _ProcsToScan, !TSStringTable),
        find_specialization_requests_in_proc(DoneParProcs, InitialModuleInfo,
            NewPredProcId, !ModuleInfo, !PendingParProcs, !Pushability,
            !RevProcMap)
    ).

:- pred map_arg_to_new_future(list(prog_var)::in, arg_pos::in,
    future_map::in, future_map::out, var_table::in, var_table::out) is det.

map_arg_to_new_future(HeadVars, FutureArg, !FutureMap, !VarTable) :-
    HeadVar = list.det_index1(HeadVars, FutureArg),
    lookup_var_entry(!.VarTable, HeadVar, HeadVarEntry),
    HeadVarType = HeadVarEntry ^ vte_type,
    HeadVarName = var_entry_name(HeadVar, HeadVarEntry),
    make_future_var(HeadVarName, HeadVarType, FutureVar, _FutureVarType,
        !VarTable),
    map.det_insert(HeadVar, FutureVar, !FutureMap).

:- pred replace_head_vars(module_info::in, future_map::in,
    prog_vars::in, prog_vars::out, list(mer_mode)::in, list(mer_mode)::out)
    is det.

replace_head_vars(_ModuleInfo, _FutureMap, [], [], [], []).
replace_head_vars(_, _, [_ | _], _, [], _) :-
    unexpected($pred, "length mismatch").
replace_head_vars(_, _, [], _, [_ | _], _) :-
    unexpected($pred, "length mismatch").
replace_head_vars(ModuleInfo, FutureMap,
        [Var0 | Vars0], [Var | Vars], [Mode0 | Modes0], [Mode | Modes]) :-
    ( if map.search(FutureMap, Var0, FutureVar) then
        Var = FutureVar,
        ( if mode_is_input(ModuleInfo, Mode0) then
            Mode = Mode0
        else if mode_is_output(ModuleInfo, Mode0) then
            Ground = ground(shared, none_or_default_func),
            Mode = from_to_mode(Ground, Ground)
        else
            sorry($pred,
                "the dependent parallel conjunction transformation " ++
                "only understands input and output modes")
        )
    else
        Var = Var0,
        Mode = Mode0
    ),
    replace_head_vars(ModuleInfo, FutureMap, Vars0, Vars, Modes0, Modes).

:- pred any_output_arguments(module_info::in, list(mer_mode)::in) is semidet.

any_output_arguments(ModuleInfo, [Mode | Modes]) :-
    ( mode_is_output(ModuleInfo, Mode)
    ; any_output_arguments(ModuleInfo, Modes)
    ).

%---------------------------------------------------------------------------%

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
            NonLocals = goal_get_nonlocals(Goal0),
            specialize_sequences_in_conj(Goals0, Goals, NonLocals, !SpecInfo),
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
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % We don't put either wait or signal operations in such scopes,
            % so there is nothing to specialize.
            Goal = Goal0
        else
            specialize_sequences_in_goal(SubGoal0, SubGoal, !SpecInfo),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        )
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred specialize_sequences_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    set_of_progvar::in, spec_info::in, spec_info::out) is det.

specialize_sequences_in_conj(Goals0, Goals, NonLocals, !SpecInfo) :-
    % For each call goal, look backwards for as many wait calls as possible
    % and forward for as many signal calls as possible. To allow us to look
    % backwards, we maintain a stack of the preceding goals.
    specialize_sequences_in_conj_2([], Goals0, Goals, NonLocals, !SpecInfo).

:- pred specialize_sequences_in_conj_2(list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out, set_of_progvar::in,
    spec_info::in, spec_info::out) is det.

specialize_sequences_in_conj_2(RevGoals, [], list.reverse(RevGoals),
        _, !SpecInfo).
specialize_sequences_in_conj_2(RevGoals0, [Goal0 | Goals0], Goals,
        NonLocals, !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        not is_wait_goal(Goal0),
        not is_signal_goal(Goal0)
    then
        CallGoal0 = hlds_goal(GoalExpr0, GoalInfo0),  % dumb mode system
        maybe_specialize_call_and_goals(RevGoals0, CallGoal0, Goals0,
            RevGoals1, Goals1, NonLocals, !SpecInfo),
        specialize_sequences_in_conj_2(RevGoals1, Goals1, Goals,
            NonLocals, !SpecInfo)
    else
        specialize_sequences_in_goal(Goal0, Goal, !SpecInfo),
        specialize_sequences_in_conj_2([Goal | RevGoals0], Goals0, Goals,
            NonLocals, !SpecInfo)
    ).

:- pred specialize_sequences_in_goals(list(hlds_goal)::in,
    list(hlds_goal)::out, spec_info::in, spec_info::out) is det.

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

%---------------------------------------------------------------------------%

:- pred maybe_specialize_call_and_goals(list(hlds_goal)::in,
    hlds_goal::in(goal_plain_call), list(hlds_goal)::in,
    list(hlds_goal)::out, list(hlds_goal)::out,
    set_of_progvar::in, spec_info::in, spec_info::out) is det.

maybe_specialize_call_and_goals(RevGoals0, Goal0, FwdGoals0,
        RevGoals, FwdGoals, NonLocals, !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, _),
    GoalExpr0 = plain_call(PredId, ProcId, CallVars, _, _, _),

    ModuleInfo = !.SpecInfo ^ spec_module_info,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    PredProcId = proc(PredId, ProcId),
    CallerPredProcId = !.SpecInfo ^ spec_ppid,
    ( if
        % We cannot push wait or signal goals into a procedure whose code we
        % don't have access to.
        % XXX: We have access to opt_imported procedures. The reason why this
        % test does not look for them is that we used to run dep_par_conj only
        % *after* mercury_compile used to invoke dead_proc_elim to delete
        % opt_imported procedures.
        list.member(ProcId, pred_info_all_non_imported_procids(PredInfo)),

        % This test avoids some problems we have had with pushing signals and
        % waits into callees, which could result in incorrect code being
        % generated.
        % See also a similar check in get_or_create_spec_par_proc/6.
        %
        % Don't push signals or waits into any procedure that contains a new
        % parallel conjunction, unless this is a recursive call.
        (
            proc_info_get_has_parallel_conj(ProcInfo, has_parallel_conj)
        =>
            (
                PredProcId = CallerPredProcId
            ;
                % Or this call is to the original version of a specialised
                % procedure. This occurs for recursive calls in specialised
                % procedures.
                map.search(!.SpecInfo ^ spec_rev_proc_map, CallerPredProcId,
                    PredProcId)
            )
        )
    then
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

        find_relevant_pushable_wait_goals(RevGoals0, PredProcId,
            CallVars, PushedWaitPairs, UnPushedWaitGoals, RevGoals1,
            !SpecInfo),
        find_relevant_pushable_signal_goals(FwdGoals0, PredProcId,
            CallVars, PushedSignalPairs, UnPushedSignalGoals, FwdGoals1,
            !SpecInfo),

        ( if
            PushedWaitPairs = [],
            PushedSignalPairs = []
        then
            RevGoals = [Goal0 | RevGoals0],
            FwdGoals = FwdGoals0
        else
            specialize_dep_par_call(PushedWaitPairs, PushedSignalPairs,
                Goal0, MaybeGoal, !SpecInfo),
            (
                MaybeGoal = yes(Goal),

                % After the replaced call may be further references to a
                % signalled or waited variable. If so, add `get' goals after
                % the transformed goal to make sure the variable is bound.
                PushedPairs = PushedSignalPairs ++ PushedWaitPairs,
                list.filter(should_add_get_goal(NonLocals, FwdGoals1),
                    PushedPairs, PushedPairsNeedGets),
                VarTable = !.SpecInfo ^ spec_var_table,
                list.map(
                    make_get_goal(!.SpecInfo ^ spec_module_info, VarTable),
                    PushedPairsNeedGets, GetGoals),

                RevGoals = GetGoals ++ [Goal] ++ UnPushedWaitGoals
                    ++ RevGoals1,
                FwdGoals = UnPushedSignalGoals ++ FwdGoals1
            ;
                MaybeGoal = no,
                RevGoals = [Goal0 | RevGoals0],
                FwdGoals = FwdGoals0
            )
        )
    else
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
    ( if
        GoalExpr = plain_call(_, _, WaitArgs, _, _, SymName),
        SymName = qualified(mercury_par_builtin_module, wait_future_pred_name),
        WaitArgs = [FutureVar, ConsumedVar]
    then
        % This is a wait goal.
        find_relevant_pushable_wait_goals(Goals, PredProcId, CallVars,
            PushedWaitPairsTail, UnPushedWaitGoalsTail, RemainingGoals,
            !SpecInfo),
        ( if
            list.index1_of_first_occurrence(CallVars, ConsumedVar, ArgPos)
        then
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
        else
            % This wait goal waits for a variable that is *not* consumed by the
            % following call, so we cannot push the wait into the called
            % procedure.
            PushedWaitPairs = PushedWaitPairsTail,
            UnPushedWaitGoals = [Goal | UnPushedWaitGoalsTail]
        )
    else
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
    ( if
        GoalExpr = plain_call(_, _, SignalArgs, _, _, SymName),
        SymName = qualified(mercury_par_builtin_module,
            signal_future_pred_name),
        SignalArgs = [FutureVar, ProducedVar]
    then
        % This is a signal goal.
        find_relevant_pushable_signal_goals(Goals, PredProcId, CallVars,
            PushedSignalPairsTail, UnPushedSignalGoalsTail, RemainingGoals,
            !SpecInfo),
        ( if
            list.index1_of_first_occurrence(CallVars, ProducedVar, ArgPos)
        then
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
        else
            % This signal goal signals a variable that is *not* produced by the
            % preceding call, so we cannot push the signal into the called
            % procedure.
            PushedSignalPairs = PushedSignalPairsTail,
            UnPushedSignalGoals = [Goal | UnPushedSignalGoalsTail]
        )
    else
        % The sequence of signal goals (if any) has ended.
        PushedSignalPairs = [],
        UnPushedSignalGoals = [],
        RemainingGoals = [Goal | Goals]
    ).

:- pred specialize_dep_par_call(
    list(future_var_pair)::in, list(future_var_pair)::in,
    hlds_goal::in(goal_plain_call), maybe(hlds_goal)::out,
    spec_info::in, spec_info::out) is det.

specialize_dep_par_call(WaitPairs, SignalPairs, Goal0, MaybeGoal, !SpecInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    GoalExpr0 = plain_call(PredId, ProcId, CallVars, _Builtin, Context, _Name),
    OrigPPId = proc(PredId, ProcId),

    WaitVars   = list.map(fvp_var, WaitPairs),
    SignalVars = list.map(fvp_var, SignalPairs),
    number_future_args(1, CallVars, WaitVars ++ SignalVars, [], FutureArgs),

    CallPattern = par_proc_call_pattern(OrigPPId, FutureArgs),
    get_or_create_spec_par_proc(FutureArgs, CallPattern, OrigPPId,
        MaybeSpecProc, !SpecInfo),

    (
        MaybeSpecProc = spec_proc(SpecPPId, SpecName),

        % Replace the call with a call to the parallelised procedure.
        list.map(replace_args_with_futures(WaitPairs ++ SignalPairs),
            CallVars, NewCallVars),
        SpecPPId = proc(SpecPredId, SpecProcId),
        GoalExpr = plain_call(SpecPredId, SpecProcId, NewCallVars, not_builtin,
            Context, SpecName),
        MaybeGoal = yes(hlds_goal(GoalExpr, GoalInfo0))
    ;
        MaybeSpecProc = will_not_specialise,
        MaybeGoal = no
    ).

:- type maybe_spec_proc
    --->    will_not_specialise
    ;       spec_proc(
                sp_ppid         :: pred_proc_id,
                sp_name         :: sym_name
            ).

:- pred get_or_create_spec_par_proc(list(arg_pos)::in,
    par_proc_call_pattern::in, pred_proc_id::in, maybe_spec_proc::out,
    spec_info::in, spec_info::out) is det.

get_or_create_spec_par_proc(FutureArgs, CallPattern, OrigPPId, MaybeSpecProc,
        !SpecInfo) :-
    ( if
        find_spec_par_proc_for_call_pattern(!.SpecInfo ^ spec_done_procs,
            !.SpecInfo ^ spec_pending_procs, CallPattern, SpecNewParProc)
    then
        SpecNewParProc = new_par_proc(SpecPPId, SpecSymName),
        MaybeSpecProc = spec_proc(SpecPPId, SpecSymName)
    else if
        % This check prevents invalid code from being generated. See also
        % a similar check in maybe_specialize_call_and_goals/6
        %
        % Don't push signals or waits into any procedure that has
        % already been specialised but doesn't match our specialisation.
        (
            some [DoneParProc] (
                map.member(!.SpecInfo ^ spec_done_procs, _, DoneParProc),
                OrigPPId = DoneParProc ^ new_ppid
            )
        ;
            some [PendingParProc] (
                list.member(_ - PendingParProc,
                    !.SpecInfo ^ spec_pending_procs),
                OrigPPId = PendingParProc ^ new_ppid
            )
        )
    then
        MaybeSpecProc = will_not_specialise
    else
        % Queue a new parallel procedure to be made. We add the new specialized
        % predicate and procedure to the module_info now; its final body
        % will be set later.
        ModuleInfo0 = !.SpecInfo ^ spec_module_info,
        PendingParProcs0 = !.SpecInfo ^ spec_pending_procs,
        RevProcMap0 = !.SpecInfo ^ spec_rev_proc_map,
        create_new_spec_parallel_pred(FutureArgs, OrigPPId, SpecPPId, SpecName,
            ModuleInfo0, ModuleInfo),
        module_info_get_name(ModuleInfo, ModuleName),
        SpecSymName = qualified(ModuleName, SpecName),
        MaybeSpecProc = spec_proc(SpecPPId, SpecSymName),
        queue_par_proc(CallPattern, new_par_proc(SpecPPId, SpecSymName),
            PendingParProcs0, PendingParProcs),
        RevProcMap = map.det_insert(RevProcMap0, SpecPPId, OrigPPId),
        !SpecInfo ^ spec_module_info := ModuleInfo,
        !SpecInfo ^ spec_pending_procs := PendingParProcs,
        !SpecInfo ^ spec_rev_proc_map := RevProcMap
    ).

:- pred find_spec_par_proc_for_call_pattern(done_par_procs::in,
    pending_par_procs::in, par_proc_call_pattern::in,
    new_par_proc::out) is semidet.

find_spec_par_proc_for_call_pattern(DoneParProcs, PendingProcs, CallPattern,
        SpecProc) :-
    ( if search(DoneParProcs, CallPattern, SpecProcPrime) then
        SpecProc = SpecProcPrime
    else if search(PendingProcs, CallPattern, SpecProcPrime) then
        SpecProc = SpecProcPrime
    else
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
    ( if X = Var0 then
        Var = Future
    else
        replace_args_with_futures(T, Var0, Var)
    ).

:- pred number_future_args(arg_pos::in, prog_vars::in, list(prog_var)::in,
    list(arg_pos)::in, list(arg_pos)::out) is det.

number_future_args(_, [], _, RevAcc, reverse(RevAcc)).
number_future_args(ArgNo, [Arg | Args], WaitSignalVars, !RevAcc) :-
    ( if list.member(Arg, WaitSignalVars) then
        list.cons(ArgNo, !RevAcc)
    else
        true
    ),
    number_future_args(ArgNo+1, Args, WaitSignalVars, !RevAcc).

    % should_add_get_goal(NonLocals, FwdGoals, FutureVarPair).
    %
    % True the variable wrapped in the FutureVarPair is needed by a another
    % goal, as indicated by NonLocals (of the conjuction) or FwdGoals (the
    % remaining goals in the conjunction).
    %
:- pred should_add_get_goal(set_of_progvar::in, list(hlds_goal)::in,
    future_var_pair::in) is semidet.

should_add_get_goal(NonLocals, FwdGoals, future_var_pair(_, Var)) :-
    (
        % If the variable is in the nonlocals set of the entire conjunction
        % then we need to add a get goal, because that means that a goal
        % outside the conjunction also uses the variable.
        set_of_var.contains(NonLocals, Var)
    ;
        % If any of the other goals in the conjunction mention the variable,
        % then we should also add a get_future variable call. We don't need to
        % check RevGoals, the only reason the variable might be mentioned there
        % would be because it was previously partially instantiated. But since
        % we are adding a get_future call that does not make sense. I [who?]
        % am assuming that only free -> ground instantiation state changes
        % are allowed for these variables.
        member(Goal, FwdGoals),
        GoalNonLocals = goal_get_nonlocals(Goal),
        set_of_var.contains(GoalNonLocals, Var)
    ).

%---------------------------------------------------------------------------%

:- pred create_new_spec_parallel_pred(list(arg_pos)::in, pred_proc_id::in,
    pred_proc_id::out, string::out, module_info::in, module_info::out) is det.

create_new_spec_parallel_pred(FutureArgs, OrigPPId, NewPPId,
        NewPredName, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, OrigPPId,
        OrigPredInfo, OrigProcInfo),
    PredStatus = pred_status(status_local),
    make_new_spec_parallel_pred_info(FutureArgs, PredStatus, OrigPPId,
        OrigPredInfo, NewPredInfo0),
    NewPredName = pred_info_name(NewPredInfo0),

    % Assign the old procedure to a new predicate, which will be modified
    % in a later pass.
    OrigPPId = proc(_, ProcId),
    pred_info_get_proc_table(NewPredInfo0, NewProcs0),
    map.set(ProcId, OrigProcInfo, NewProcs0, NewProcs),
    pred_info_set_proc_table(NewProcs, NewPredInfo0, NewPredInfo),

    % Add the new predicate to the pred table.
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(NewPredInfo, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),
    NewPPId = proc(NewPredId, ProcId).

    % The comments in this predicate are from unused_args.m
    %
:- pred make_new_spec_parallel_pred_info(list(arg_pos)::in, pred_status::in,
    pred_proc_id::in, pred_info::in, pred_info::out) is det.

make_new_spec_parallel_pred_info(FutureArgs, PredStatus, PPId, !PredInfo) :-
    PPId = proc(PredId, ProcId),
    PredModule = pred_info_module(!.PredInfo),
    Name0 = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    pred_info_get_arg_types(!.PredInfo, Tvars, ExistQVars, ArgTypes0),
    pred_info_get_origin(!.PredInfo, OrigOrigin),
    % The mode number is included because we want to avoid the creation of
    % more than one predicate with the same name if more than one mode of
    % a predicate is parallelised. Since the names of e.g. deep profiling
    % proc_static structures are derived from the names of predicates,
    % duplicate predicate names lead to duplicate global variable names
    % and hence to link errors.
    Transform =
        tn_dep_par_conj(PredOrFunc, proc_id_to_int(ProcId), FutureArgs),
    make_transformed_pred_name(Name0, Transform, TransformedName),

    PredFormArity = pred_info_pred_form_arity(!.PredInfo),
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
    ProcTransform = proc_transform_dep_par_conj(FutureArgs),
    Origin = origin_proc_transform(ProcTransform, OrigOrigin, PredId, ProcId),
    CurUserDecl = maybe.no,
    pred_info_init(PredOrFunc, PredModule, TransformedName, PredFormArity,
        Context, Origin, PredStatus, CurUserDecl, GoalType, Markers, ArgTypes,
        Tvars, ExistQVars, ClassContext, EmptyProofs, EmptyConstraintMap,
        ClausesInfo, VarNameRemap, !:PredInfo),
    pred_info_set_typevarset(TypeVars, !PredInfo).

:- pred futurise_argtypes(arg_pos::in, list(arg_pos)::in, list(mer_type)::in,
    list(mer_type)::out) is det.

futurise_argtypes(_, [], ArgTypes, ArgTypes).
futurise_argtypes(_, [_ | _], [], _) :-
    unexpected($pred, "more future arguments than argument types").
futurise_argtypes(ArgNo, [FutureArg | FutureArgs], [ArgType | ArgTypes],
        [FuturisedArgType | FuturisedArgTypes]) :-
    ( if ArgNo = FutureArg then
        FuturisedArgType = future_type(ArgType),
        futurise_argtypes(ArgNo + 1, FutureArgs,
            ArgTypes, FuturisedArgTypes)
    else
        FuturisedArgType = ArgType,
        futurise_argtypes(ArgNo + 1, [FutureArg | FutureArgs],
            ArgTypes, FuturisedArgTypes)
    ).

%---------------------------------------------------------------------------%

:- type push_op
    --->    push_wait
    ;       push_signal.

:- pred should_we_push(pred_proc_id::in, int::in, push_op::in,
    maybe_worth_pushing::out, spec_info::in, spec_info::out) is det.

should_we_push(PredProcId, ArgPos, PushOp, IsWorthPushing, !SpecInfo) :-
    Pushability0 = !.SpecInfo ^ spec_pushability,
    ( if map.search(Pushability0, PredProcId, ProcPushMap0) then
        ( if map.search(ProcPushMap0, ArgPos, KnownWorthPushing) then
            IsWorthPushing = KnownWorthPushing
        else
            should_we_push_test(PredProcId, ArgPos, PushOp, IsWorthPushing,
                !.SpecInfo),
            map.det_insert(ArgPos, IsWorthPushing, ProcPushMap0, ProcPushMap),
            map.det_update(PredProcId, ProcPushMap, Pushability0, Pushability),
            !SpecInfo ^ spec_pushability := Pushability
        )
    else
        InitialModuleInfo = !.SpecInfo ^ spec_initial_module,
        module_info_get_globals(InitialModuleInfo, Globals),
        globals.get_opt_tuple(Globals, OptTuple),
        AlwaysSpecialize = OptTuple ^ ot_spec_in_all_dep_par_conjs,
        (
            AlwaysSpecialize = spec_in_all_dep_par_conjs,
            IsWorthPushing = worth_pushing
        ;
            AlwaysSpecialize = do_not_spec_in_all_dep_par_conjs,
            should_we_push_test(PredProcId, ArgPos, PushOp, IsWorthPushing,
                !.SpecInfo)
        ),
        ProcPushMap = map.singleton(ArgPos, IsWorthPushing),
        map.det_insert(PredProcId, ProcPushMap, Pushability0, Pushability),
        !SpecInfo ^ spec_pushability := Pushability
    ).

:- pred should_we_push_test(pred_proc_id::in, int::in, push_op::in,
    maybe_worth_pushing::out, spec_info::in) is det.

should_we_push_test(PredProcId, ArgPos, PushOp, IsWorthPushing, SpecInfo) :-
    InitialModuleInfo = SpecInfo ^ spec_initial_module,
    module_info_proc_info(InitialModuleInfo, PredProcId, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    list.det_index1(HeadVars, ArgPos, Var),
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
            unexpected($pred, "not_seen_signal")
        ;
            CostAfterSignal = seen_signal_negligible_cost_after,
            IsWorthPushing = not_worth_pushing
        ;
            CostAfterSignal = seen_signal_non_negligible_cost_after,
            IsWorthPushing = worth_pushing
        ;
            CostAfterSignal = code_has_no_solutions,
            % The signal will never be executed no matter where we put it,
            % don't bother specialising code.
            IsWorthPushing = not_worth_pushing
        )
    ).

%---------------------------------------------------------------------------%

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
        ( if set_of_var.member(NonLocals, Var) then
            Wait = seen_wait_negligible_cost_before
        else
            Wait = not_seen_wait_negligible_cost_so_far
        )
    ;
        GoalExpr = plain_call(_, _, _, BuiltinStatus, _, _),
        (
            BuiltinStatus = inline_builtin,
            ( if set_of_var.member(NonLocals, Var) then
                Wait = seen_wait_negligible_cost_before
            else
                Wait = not_seen_wait_negligible_cost_so_far
            )
        ;
            BuiltinStatus = not_builtin,
            ( if set_of_var.member(NonLocals, Var) then
                Wait = seen_wait_non_negligible_cost_before
            else
                Wait = not_seen_wait_non_negligible_cost_so_far
            )
        )
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        ( if set_of_var.member(NonLocals, Var) then
            Wait = seen_wait_non_negligible_cost_before
        else
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
            ( if
                list.member(seen_wait_non_negligible_cost_before, Waits)
            then
                % At least one of the parallel conjuncts can benefit from not
                % waiting for Var at the start.
                Wait = seen_wait_non_negligible_cost_before
            else if
                list.member(not_seen_wait_non_negligible_cost_so_far, Waits)
            then
                % At least one of the parallel conjuncts does not need to wait
                % for Var at all, and has non-negligible cost. That conjunct
                % can also benefit from not waiting for Var at the start.
                Wait = not_seen_wait_non_negligible_cost_so_far
            else if
                list.member(seen_wait_negligible_cost_before, Waits)
            then
                Wait = seen_wait_negligible_cost_before
            else
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
        ( if Var = SwitchVar then
            Wait = seen_wait_negligible_cost_before
        else
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
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % The SubGoal may be huge, but since the code generator will
            % turn it all into a single assignment of a pointer to a large
            % static data structure, its cost in execution time is negligible.
            Wait = not_seen_wait_negligible_cost_so_far
        else
            % XXX If Reason = from_ground_term(X,
            % from_ground_term_deconstruct), then the only variable
            % that we can wait for is X. We should be able to use that fact
            % to avoid processing SubGoal.
            should_we_push_wait(Var, SubGoal, Wait)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
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

%---------------------------------------------------------------------------%

:- type cost_after_signal
    --->    not_seen_signal
    ;       code_has_no_solutions
            % The goal has no solutions and therefore does not produce
            % the result.

    ;       seen_signal_negligible_cost_after
    ;       seen_signal_non_negligible_cost_after.

    % The should we signal code only makes sense when its input is one of
    % these values for !.Signal.
    %
:- inst cost_after_signal_in for cost_after_signal/0
    --->    not_seen_signal
    ;       seen_signal_negligible_cost_after.

:- pred seen_produced_var(cost_after_signal::in(cost_after_signal_in),
    cost_after_signal::out) is det.

seen_produced_var(!Signal) :-
    (
        !.Signal = not_seen_signal,
        !:Signal = seen_signal_negligible_cost_after
    ;
        !.Signal = seen_signal_negligible_cost_after
    ).

:- pred seen_nontrivial_cost(cost_after_signal::in(cost_after_signal_in),
    cost_after_signal::out) is det.

seen_nontrivial_cost(!Signal) :-
    (
        !.Signal = not_seen_signal
        % We are not interested in costs before the signal.
    ;
        !.Signal = seen_signal_negligible_cost_after,
        !:Signal = seen_signal_non_negligible_cost_after
    ).

:- pred should_we_push_signal(prog_var::in, hlds_goal::in,
    cost_after_signal::in(cost_after_signal_in), cost_after_signal::out)
    is det.

should_we_push_signal(Var, Goal, !Signal) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, _CanFail, NumSolutions),
    (
        ( NumSolutions = at_most_one
        ; NumSolutions = at_most_many_cc
        ; NumSolutions = at_most_many
        ),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        % When handling calls, we could use profiling data to decide whether a
        % call site has negligible cost or not. In the absence of such data, we
        % have to assume that all call sites have non-negligible cost, because
        % if we assumed that they have negligible cost, then we would have to
        % infer that *all* goals have negligible cost, which besides being
        % incorrect, would mean that there is never any point in pushing
        % signals, rendering this entire code useless.
        (
            GoalExpr = unify(_, _, _, _, _),
            ( if set_of_var.member(NonLocals, Var) then
                seen_produced_var(!Signal)
            else
                true
            )
        ;
            % With generic calls, the only safe assumption is that they produce
            % Var just before return. With foreign code, the signal is done
            % after the return to Mercury execution.
            ( GoalExpr = generic_call(_, _, _, _, _)
            ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            ( if set_of_var.member(NonLocals, Var) then
                seen_produced_var(!Signal)
            else
                seen_nontrivial_cost(!Signal)
            )
        ;
            GoalExpr = plain_call(_, _, _, _, _, _),
            % XXX We should invoke should_we_push recursively on the called
            % procedure, though that would require safeguards against infinite
            % recursion.
            ( if set_of_var.member(NonLocals, Var) then
                seen_produced_var(!Signal)
            else
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
            % What we do in this case doesn't usually matter. Semidet
            % disjunctions cannot bind any nonlocal variables (and thus cannot
            % bind Var). Nondet disjunctions can bind variables, but we want
            % to parallelize only model_det code. The only case where what we
            % do here matters is when a nondet disjunction is inside a scope
            % that commits to the first success.
            should_we_push_signal_in_disj(Var, Disjuncts, !Signal)
        ;
            GoalExpr = switch(SwitchVar, _, Cases),
            ( if Var = SwitchVar then
                % !.Signal must show that we have already seen a signal.
                expect(negate(unify(!.Signal, not_seen_signal)), $pred,
                    "not seen switch var")
            else
                should_we_push_signal_in_cases(Var, Cases, !Signal)
            )
        ;
            GoalExpr = if_then_else(_Vars, _Cond, Then, Else),
            % The condition cannot produce a nonlocal variable such as Var.
            should_we_push_signal(Var, Then, !.Signal, SignalThen),
            should_we_push_signal(Var, Else, !.Signal, SignalElse),
            (
                SignalThen = not_seen_signal,
                (
                    ( SignalElse = not_seen_signal
                    ; SignalElse = code_has_no_solutions
                    ),
                    !:Signal = not_seen_signal
                ;
                    ( SignalElse = seen_signal_non_negligible_cost_after
                    ; SignalElse = seen_signal_negligible_cost_after
                    ),
                    unexpected($pred, "ITE is not mode safe")
                )
            ;
                SignalThen = code_has_no_solutions,
                !:Signal = SignalElse
            ;
                SignalThen = seen_signal_non_negligible_cost_after,
                (
                    SignalElse = not_seen_signal,
                    unexpected($pred, "ITE is not mode safe")
                ;
                    ( SignalElse = code_has_no_solutions
                    ; SignalElse = seen_signal_non_negligible_cost_after
                    ; SignalElse = seen_signal_negligible_cost_after
                    ),
                    !:Signal = seen_signal_non_negligible_cost_after
                )
            ;
                SignalThen = seen_signal_negligible_cost_after,
                (
                    SignalElse = not_seen_signal,
                    unexpected($pred, "ITE is not mode safe")
                ;
                    ( SignalElse = code_has_no_solutions
                    ; SignalElse = seen_signal_negligible_cost_after
                    ),
                    !:Signal = seen_signal_negligible_cost_after
                ;
                    SignalElse = seen_signal_non_negligible_cost_after,
                    !:Signal = seen_signal_non_negligible_cost_after
                )
            )
        ;
            GoalExpr = negation(SubGoal),
            (
                !.Signal = not_seen_signal
                % A negated goal cannot produce a nonlocal variable
                % such as Var, and we don't care about the cost of computations
                % before the signal.
            ;
                !.Signal = seen_signal_negligible_cost_after,
                % We do care whether the cost of SubGoal is negligible or not.
                should_we_push_signal(Var, SubGoal, !Signal)
            )
        ;
            GoalExpr = scope(Reason, SubGoal),
            ( if
                Reason = from_ground_term(TermVar, from_ground_term_construct)
            then
                ( if Var = TermVar then
                    seen_produced_var(!Signal)
                else
                    true
                )
            else
                should_we_push_signal(Var, SubGoal, !Signal)
            )
        ;
            GoalExpr = shorthand(_),
            unexpected($pred, "shorthand")
        )
    ;
        NumSolutions = at_most_zero,
        % The goal can never complete, which means that it can never produce
        % the future and has an 'unreachable' instmap. Note that we haven't
        % checked that this goal or a goal after it definitely produce the
        % variable.
        !:Signal = code_has_no_solutions
    ).

:- pred should_we_push_signal_in_plain_conj(prog_var::in, list(hlds_goal)::in,
    cost_after_signal::in(cost_after_signal_in), cost_after_signal::out)
    is det.

should_we_push_signal_in_plain_conj(_Var, [], !Signal).
should_we_push_signal_in_plain_conj(Var, [Conjunct | Conjuncts], !Signal) :-
    should_we_push_signal(Var, Conjunct, !Signal),
    (
        !.Signal = seen_signal_non_negligible_cost_after
        % There is no point in looking at Conjuncts; we already know
        % we want to push the signal.
    ;
        !.Signal = code_has_no_solutions
        % We don't bother checking if the signal occurs in unreachable code.
    ;
        ( !.Signal = not_seen_signal
        ; !.Signal = seen_signal_negligible_cost_after
        ),
        should_we_push_signal_in_plain_conj(Var, Conjuncts, !Signal)
    ).

:- pred should_we_push_signal_in_par_conj(prog_var::in, list(hlds_goal)::in,
    cost_after_signal::in(cost_after_signal_in),
    cost_after_signal::in, cost_after_signal::out) is det.

should_we_push_signal_in_par_conj(_Var, [], _OrigSignal, !FinalSignal).
should_we_push_signal_in_par_conj(Var, [Conjunct | Conjuncts],
        OrigSignal, !FinalSignal) :-
    FinalSignal0 = !.FinalSignal,
    should_we_push_signal(Var, Conjunct, OrigSignal, ConjunctSignal),
    (
        ConjunctSignal = not_seen_signal,
        % Neither the goal before the parallel conjunction nor the parallel
        % conjuncts we have looked at so far produce Var.
        should_we_push_signal_in_par_conj(Var, Conjuncts,
            OrigSignal, !FinalSignal)
    ;
        ConjunctSignal = code_has_no_solutions,
        !:FinalSignal = code_has_no_solutions
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
    expect(we_have_seen_more_signal(FinalSignal0, FinalSignal), $pred,
        "final signal goes backwards").

:- pred should_we_push_signal_in_disj(prog_var::in, list(hlds_goal)::in,
    cost_after_signal::in(cost_after_signal_in),
    cost_after_signal::out) is det.

should_we_push_signal_in_disj(_Var, [], _OrigSignal, code_has_no_solutions).
should_we_push_signal_in_disj(Var, [FirstGoal | LaterGoals],
        OrigSignal, Signal) :-
    should_we_push_signal(Var, FirstGoal, OrigSignal, SignalFirst),
    (
        SignalFirst = not_seen_signal,
        % If FirstGoal does not signal Var, the rest of the disjuncts
        % shouldn't either.
        Signal = SignalFirst
    ;
        SignalFirst = seen_signal_non_negligible_cost_after,
        % We already know we want to push the signal.
        Signal = SignalFirst
    ;
        ( SignalFirst = seen_signal_negligible_cost_after
        ; SignalFirst = code_has_no_solutions
        ),
        % We want to push the signal only if it is worth pushing
        % into one of the rest of the disjuncts.
        should_we_push_signal_in_disj(Var, LaterGoals, OrigSignal, Signal0),
        (
            SignalFirst = seen_signal_negligible_cost_after,
            (
                Signal0 = not_seen_signal,
                unexpected($pred, "The program doesn't seem mode correct")
            ;
                Signal0 = code_has_no_solutions,
                Signal = SignalFirst
            ;
                ( Signal0 = seen_signal_negligible_cost_after
                ; Signal0 = seen_signal_non_negligible_cost_after
                ),
                Signal = Signal0
            )
        ;
            SignalFirst = code_has_no_solutions,
            Signal = Signal0
        )
    ).

:- pred should_we_push_signal_in_cases(prog_var::in, list(case)::in,
    cost_after_signal::in(cost_after_signal_in),
    cost_after_signal::out) is det.

should_we_push_signal_in_cases(_Var, [], _OrigSignal, code_has_no_solutions).
should_we_push_signal_in_cases(Var, [FirstCase | LaterCases],
        OrigSignal, Signal) :-
    FirstCase = case(_, _, FirstGoal),
    should_we_push_signal(Var, FirstGoal, OrigSignal, SignalFirst),
    (
        SignalFirst = not_seen_signal,
        % If FirstCase does not signal Var, the rest of the cases
        % shouldn't either.
        Signal = SignalFirst
    ;
        SignalFirst = seen_signal_non_negligible_cost_after,
        % We already know we want to push the signal.
        Signal = SignalFirst
    ;
        ( SignalFirst = seen_signal_negligible_cost_after
        ; SignalFirst = code_has_no_solutions
        ),
        % We want to push the signal only if it is worth pushing
        % into one of the rest of the cases.
        should_we_push_signal_in_cases(Var, LaterCases, OrigSignal, Signal0),
        (
            SignalFirst = seen_signal_negligible_cost_after,
            (
                Signal0 = not_seen_signal,
                unexpected($pred, "The program doesn't seem mode correct")
            ;
                Signal0 = code_has_no_solutions,
                Signal = SignalFirst
            ;
                ( Signal0 = seen_signal_negligible_cost_after
                ; Signal0 = seen_signal_non_negligible_cost_after
                ),
                Signal = Signal0
            )
        ;
            SignalFirst = code_has_no_solutions,
            Signal = Signal0
        )
    ).

:- pred we_have_seen_more_signal(cost_after_signal::in, cost_after_signal::in)
    is semidet.

we_have_seen_more_signal(SignalA, SignalB) :-
    seen_more_signal(SignalA, SignalB) = yes.

:- func seen_more_signal(cost_after_signal, cost_after_signal) = bool.

seen_more_signal(FinalSignal0, FinalSignal) = SeenMoreSignal :-
    (
        FinalSignal0 = not_seen_signal,
        SeenMoreSignal = yes
    ;
        FinalSignal0 = code_has_no_solutions,
        (
            ( FinalSignal = not_seen_signal
            ; FinalSignal = seen_signal_negligible_cost_after
            ; FinalSignal = seen_signal_non_negligible_cost_after
            ),
            SeenMoreSignal = no
        ;
            FinalSignal = code_has_no_solutions,
            SeenMoreSignal = yes
        )
    ;
        FinalSignal0 = seen_signal_negligible_cost_after,
        (
            FinalSignal = not_seen_signal,
            SeenMoreSignal = no
        ;
            ( FinalSignal = code_has_no_solutions
            ; FinalSignal = seen_signal_negligible_cost_after
            ; FinalSignal = seen_signal_non_negligible_cost_after
            ),
            SeenMoreSignal = yes
        )
    ;
        FinalSignal0 = seen_signal_non_negligible_cost_after,
        (
            ( FinalSignal = not_seen_signal
            ; FinalSignal = seen_signal_negligible_cost_after
            ),
            SeenMoreSignal = no
        ;
            ( FinalSignal = code_has_no_solutions
            ; FinalSignal = seen_signal_non_negligible_cost_after
            ),
            SeenMoreSignal = yes
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utilities for working with par_builtin.
%

    % Given a variable SharedVar of type SharedVarType, add a new variable
    % FutureVar of type future(SharedVarType), add the mapping from SharedVar
    % to FutureVar to FutureMap, and generate the goal AllocGoal that calls
    % `par_builtin.new_future/1' to allocate FutureVar.
    %
:- pred allocate_future(module_info::in, prog_var::in, hlds_goals::out,
    var_table::in, var_table::out, future_map::in, future_map::out,
    ts_string_table::in, ts_string_table::out) is det.

allocate_future(ModuleInfo, SharedVar, Goals, !VarTable,
        !FutureMap, !TSStringTable) :-
    lookup_var_entry(!.VarTable, SharedVar, SharedVarEntry),
    SharedVarType = SharedVarEntry ^ vte_type,
    SharedVarName = var_entry_name(SharedVar, SharedVarEntry),
    make_future_var(SharedVarName, SharedVarType, FutureVar, FutureVarType,
        !VarTable),
    make_future_name_var_and_goal(SharedVarName, FutureNameVar, SetNameGoal,
        !VarTable, !TSStringTable),
    map.det_insert(SharedVar, FutureVar, !FutureMap),

    ModuleName = mercury_par_builtin_module,
    PredName = new_future_pred_name,
    Features = [],
    InstMapDelta = instmap_delta_bind_var(FutureVar),
    Context = dummy_context,
    ShouldInline = should_inline_par_builtin_calls(ModuleInfo),
    (
        ShouldInline = do_not_inline_par_builtins,
        ArgVars = [FutureNameVar, FutureVar],
        generate_plain_call(ModuleInfo, pf_predicate, ModuleName, PredName,
            [], ArgVars, InstMapDelta, only_mode, detism_det, purity_pure,
            Features, Context, AllocGoal)
    ;
        ShouldInline = inline_par_builtins,
        ForeignAttrs = par_builtin_foreign_proc_attributes(purity_pure,
            no_request_for_call_std_out_regs),
        ArgName = foreign_arg(FutureNameVar,
            yes(foreign_arg_name_mode("Name", in_mode)),
            builtin_type(builtin_type_int(int_type_int)),
            bp_native_if_possible),
        ArgFuture = foreign_arg(FutureVar,
            yes(foreign_arg_name_mode("Future", out_mode)),
            FutureVarType, bp_native_if_possible),
        Args = [ArgName, ArgFuture],
        ExtraArgs = [],
        Code = new_future_code,
        generate_call_foreign_proc(ModuleInfo, pf_predicate,
            ModuleName, PredName, [], Args, ExtraArgs, InstMapDelta,
            only_mode, detism_det, purity_pure, Features, ForeignAttrs,
            no, Code, Context, AllocGoal)
    ),
    Goals = [SetNameGoal, AllocGoal].

    % Given a variable SharedVar of type SharedVarType, add a new variable
    % FutureVar of type future(SharedVarType).
    %
:- pred make_future_var(string::in, mer_type::in,
    prog_var::out, mer_type::out, var_table::in, var_table::out) is det.

make_future_var(SharedVarName, SharedVarType, FutureVar, FutureVarType,
        !VarTable) :-
    FutureVarType = future_type(SharedVarType),
    FutureVarName = "Future" ++ SharedVarName,
    FutureVarEntry = vte(FutureVarName, FutureVarType, is_not_dummy_type),
    add_var_entry(FutureVarEntry, FutureVar, !VarTable).

:- pred make_future_name_var_and_goal(string::in,
    prog_var::out, hlds_goal::out, var_table::in, var_table::out,
    ts_string_table::in, ts_string_table::out) is det.

make_future_name_var_and_goal(Name, FutureNameVar, Goal,
        !VarTable, !TSStringTable) :-
    FutureNameVarName = "FutureName" ++ Name,
    FutureNameVarEntry = vte(FutureNameVarName, int_type, is_not_dummy_type),
    add_var_entry(FutureNameVarEntry, FutureNameVar, !VarTable),
    allocate_ts_string(Name, NameId, !TSStringTable),
    NameIdConsId = some_int_const(int_const(NameId)),
    RHS = rhs_functor(NameIdConsId, is_not_exist_constr, []),
    Ground = ground(unique, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    Unification = construct(FutureNameVar, NameIdConsId, [], [],
        construct_statically(born_static), cell_is_unique,
        no_construct_sub_info),
    UnifyContext =
        unify_context(umc_implicit("dep_par_conj transformation"), []),
    GoalExpr = unify(FutureNameVar, RHS, UnifyMode, Unification, UnifyContext),
    InstmapDelta = instmap_delta_from_assoc_list([FutureNameVar - Ground]),
    goal_info_init(set_of_var.make_singleton(FutureNameVar), InstmapDelta,
        detism_det, purity_pure, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred make_wait_goal(module_info::in, var_table::in,
    prog_var::in, prog_var::in, hlds_goal::out) is det.

make_wait_goal(ModuleInfo, VarTable, FutureVar, WaitVar, WaitGoal) :-
    make_wait_or_get(ModuleInfo, VarTable, FutureVar, WaitVar, wait_pred,
        WaitGoal).

:- pred make_get_goal(module_info::in, var_table::in, future_var_pair::in,
    hlds_goal::out) is det.

make_get_goal(ModuleInfo, VarTable, future_var_pair(FutureVar, WaitVar),
        WaitGoal) :-
    make_wait_or_get(ModuleInfo, VarTable, FutureVar, WaitVar, get_pred,
        WaitGoal).

:- type wait_or_get_pred
    --->    wait_pred
    ;       get_pred.

:- pred make_wait_or_get(module_info::in, var_table::in,
    prog_var::in, prog_var::in, wait_or_get_pred::in, hlds_goal::out) is det.

make_wait_or_get(ModuleInfo, VarTable, FutureVar, ConsumedVar, WaitOrGetPred,
        WaitGoal) :-
    ModuleName = mercury_par_builtin_module,
    (
        WaitOrGetPred = wait_pred,
        PredName = wait_future_pred_name,
        Purity = purity_impure,
        Code = "MR_par_builtin_wait_future(Future, Value);"
    ;
        WaitOrGetPred = get_pred,
        PredName = get_future_pred_name,
        Purity = purity_pure,
        Code = "MR_par_builtin_get_future(Future, Value);"
    ),
    Features = [],
    InstMapDelta = instmap_delta_bind_var(ConsumedVar),
    Context = dummy_context,
    ShouldInline = should_inline_par_builtin_calls(ModuleInfo),
    (
        ShouldInline = do_not_inline_par_builtins,
        ArgVars = [FutureVar, ConsumedVar],
        generate_plain_call(ModuleInfo, pf_predicate, ModuleName, PredName,
            [], ArgVars, InstMapDelta, only_mode, detism_det, Purity,
            Features, Context, WaitGoal)
    ;
        ShouldInline = inline_par_builtins,
        ForeignAttrs = par_builtin_foreign_proc_attributes(Purity,
            no_request_for_call_std_out_regs),
        lookup_var_type(VarTable, FutureVar, FutureVarType),
        lookup_var_type(VarTable, ConsumedVar, ConsumedVarType),
        Arg1 = foreign_arg(FutureVar,
            yes(foreign_arg_name_mode("Future", in_mode)),
            FutureVarType, bp_native_if_possible),
        Arg2 = foreign_arg(ConsumedVar,
            yes(foreign_arg_name_mode("Value", out_mode)),
            ConsumedVarType, bp_native_if_possible),
        Args = [Arg1, Arg2],
        ExtraArgs = [],
        generate_call_foreign_proc(ModuleInfo, pf_predicate,
            ModuleName, PredName, [], Args, ExtraArgs, InstMapDelta, only_mode,
            detism_det, Purity, Features, ForeignAttrs,
            no, Code, Context, WaitGoal)
    ).

:- pred make_signal_goal(module_info::in, var_table::in, future_map::in,
    prog_var::in, hlds_goal::out) is det.

make_signal_goal(ModuleInfo, VarTable, FutureMap, ProducedVar, SignalGoal) :-
    FutureVar = map.lookup(FutureMap, ProducedVar),
    ModuleName = mercury_par_builtin_module,
    PredName = signal_future_pred_name,
    Features = [],
    InstMapDelta = instmap_delta_bind_no_var,
    Context = dummy_context,
    ShouldInline = should_inline_par_builtin_calls(ModuleInfo),
    (
        ShouldInline = do_not_inline_par_builtins,
        ArgVars = [FutureVar, ProducedVar],
        generate_plain_call(ModuleInfo, pf_predicate, ModuleName, PredName,
            [], ArgVars, InstMapDelta, only_mode, detism_det, purity_impure,
            Features, Context, SignalGoal)
    ;
        ShouldInline = inline_par_builtins,
        ForeignAttrs = par_builtin_foreign_proc_attributes(purity_impure,
            needs_call_std_out_regs),
        lookup_var_type(VarTable, FutureVar, FutureVarType),
        lookup_var_type(VarTable, ProducedVar, ProducedVarType),
        Arg1 = foreign_arg(FutureVar,
            yes(foreign_arg_name_mode("Future", in_mode)),
            FutureVarType, bp_native_if_possible),
        Arg2 = foreign_arg(ProducedVar,
            yes(foreign_arg_name_mode("Value", in_mode)),
            ProducedVarType, bp_native_if_possible),
        Args = [Arg1, Arg2],
        ExtraArgs = [],
        Code = "MR_par_builtin_signal_future(Future, Value);",
        generate_call_foreign_proc(ModuleInfo, pf_predicate,
            ModuleName, PredName, [], Args, ExtraArgs, InstMapDelta,
            only_mode, detism_det, purity_impure, Features, ForeignAttrs,
            no, Code, Context, SignalGoal)
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

:- func new_future_code = string.

new_future_code = "
    #ifdef MR_THREADSCOPE
        MR_par_builtin_new_future(Name, Future);
    #else
        MR_par_builtin_new_future(Future);
    #endif
".

:- func should_inline_par_builtin_calls(module_info) =
    maybe_inline_par_builtins.

should_inline_par_builtin_calls(ModuleInfo) = ShouldInline :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    ShouldInline = OptTuple ^ ot_inline_par_builtins.

:- func par_builtin_foreign_proc_attributes(purity, maybe_call_std_out_regs)
    = foreign_proc_attributes.

par_builtin_foreign_proc_attributes(Purity, CallStdOutRegs) = !:Attrs :-
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
    set_call_std_out_regs(CallStdOutRegs, !Attrs),
    set_may_duplicate(yes(proc_may_duplicate), !Attrs).

%---------------------------------------------------------------------------%

:- pred conjoin_goal_and_goal_list_update_goal_infos(hlds_goal_info::in,
    hlds_goal::in, list(hlds_goal)::in, hlds_goal::out) is det.

conjoin_goal_and_goal_list_update_goal_infos(!.GoalInfo, GoalA, GoalsB,
        Goal) :-
    GoalA = hlds_goal(GoalExprA, _),

    ( if GoalExprA = conj(plain_conj, GoalListA) then
        GoalList = GoalListA ++ GoalsB
    else
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
    ( if GoalB = hlds_goal(conj(plain_conj, GoalsB), _) then
        GoalListB = GoalsB
    else
        GoalListB = [GoalB]
    ),
    conjoin_goal_and_goal_list_update_goal_infos(!.GoalInfo, GoalA, GoalListB,
        Goal).

%---------------------------------------------------------------------------%

    % Given the conjunct goals in a parallel conjunction and the instmap before
    % it, return the set of variables that need synchronization, i.e. the
    % variables that are produced in one conjunct and consumed in one or more
    % other conjuncts.
    %
:- func find_shared_variables(module_info, instmap, list(hlds_goal))
    = set_of_progvar.

find_shared_variables(ModuleInfo, InstMap, Goals) = SharedVars :-
    % If a variable is nonlocal to a conjunct, and appears in the instmap_delta
    % of a _different_ conjunct, then we say that variable is shared.
    %
    % (1) A variable must be nonlocal to a conjunct if it is shared.
    % (2) If the variable does not appear in the instmap_delta
    %     of any of the conjuncts of the parallel conjunction
    %     then it could not have been further instantiated within
    %     by the conjunction as a whole.
    %
    % XXX This code is probably too complicated. I think Thomas already had a
    % more elegant way to find the shared variables somewhere, using multisets.
    %
    list.map2(get_nonlocals_and_instmaps, Goals, Nonlocals, InstMapDeltas),
    find_shared_variables_2(ModuleInfo, 0, Nonlocals, InstMap, InstMapDeltas,
        set_of_var.init, SharedVars).

:- pred get_nonlocals_and_instmaps(hlds_goal::in,
    set_of_progvar::out, instmap_delta::out) is det.

get_nonlocals_and_instmaps(hlds_goal(_, GoalInfo), Nonlocals, InstMapDelta) :-
    Nonlocals = goal_info_get_nonlocals(GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo).

:- pred find_shared_variables_2(module_info::in, int::in,
    list(set_of_progvar)::in, instmap::in, list(instmap_delta)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

find_shared_variables_2(_ModuleInfo, _ConjunctIndex,
        [], _InstMap, _InstMapDeltas, !SharedVars).
find_shared_variables_2(ModuleInfo, ConjunctIndex,
        [Nonlocals | MoreNonlocals], InstMap, InstMapDeltas, !SharedVars) :-
    det_delete_nth(ConjunctIndex, InstMapDeltas, InstMapDeltasB),
    % Keep only nonlocals which were not already bound at the start of the
    % parallel conjunction.
    Filter =
        ( pred(Var::in) is semidet :-
            instmap_lookup_var(InstMap, Var, VarInst),
            not inst_is_bound(ModuleInfo, VarInst)
        ),
    UnboundNonlocals = set_of_var.filter(Filter, Nonlocals),
    Changed =
        set_of_var.filter(changed_var(ModuleInfo, InstMapDeltasB),
            UnboundNonlocals),
    set_of_var.union(Changed, !SharedVars),
    find_shared_variables_2(ModuleInfo, ConjunctIndex+1, MoreNonlocals,
        InstMap, InstMapDeltas, !SharedVars).

:- pred changed_var(module_info::in, list(instmap_delta)::in, prog_var::in)
    is semidet.

changed_var(ModuleInfo, InstMapDeltas, UnboundVar) :-
    % Is the unbound nonlocal bound in one of the conjuncts?
    list.member(InstMapDelta, InstMapDeltas),
    instmap_delta_search_var(InstMapDelta, UnboundVar, Inst),
    inst_is_bound(ModuleInfo, Inst).

%---------------------------------------------------------------------------%

:- pred fixup_and_reinsert_proc(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out) is det.

fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo, !ModuleInfo) :-
    requantify_proc_general(ord_nl_no_lambda, !ProcInfo),
    recompute_instmap_delta_proc(no_recomp_atomics, !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

:- pred det_delete_nth(int::in, list(T)::in, list(T)::out) is det.

det_delete_nth(N, List0, List) :-
    list.det_split_list(N, List0, Left, Right),
    List = Left ++ det_tail(Right).

:- pred var_in_nonlocals(hlds_goal::in, prog_var::in) is semidet.

var_in_nonlocals(Goal, Var) :-
    set_of_var.member(goal_get_nonlocals(Goal), Var).

:- pred var_not_in_nonlocals(hlds_goal::in, prog_var::in) is semidet.

var_not_in_nonlocals(Goal, Var) :-
    not var_in_nonlocals(Goal, Var).

%---------------------------------------------------------------------------%
%
% Threadscope support used in this module.
%

:- type ts_string_table
    --->    ts_string_table(
                st_lookup_map       :: map(string, int),
                st_rev_table        :: list(string),
                st_size             :: int
            ).

:- pred allocate_ts_string(string::in, int::out,
    ts_string_table::in, ts_string_table::out) is det.

allocate_ts_string(String, Id, !Table) :-
    !.Table = ts_string_table(Map0, RevTable0, Size0),
    ( if map.search(Map0, String, ExistingId) then
        Id = ExistingId
    else
        Id = Size0,
        Size = Size0 + 1,
        RevTable = [String | RevTable0],
        map.det_insert(String, Id, Map0, Map),
        !:Table = ts_string_table(Map, RevTable, Size)
    ).

:- pred make_ts_string_table(list(string)::in, ts_string_table::out) is det.

make_ts_string_table(RevTable, ts_string_table(Map, RevTable, Size)) :-
    make_ts_string_table_2(RevTable, Size, map.init, Map).

:- pred make_ts_string_table_2(list(string)::in, int::out,
    map(string, int)::in, map(string, int)::out) is det.

make_ts_string_table_2([], 0, !Map).
make_ts_string_table_2([Str | Strs], Size, !Map) :-
    make_ts_string_table_2(Strs, Size0, !Map),
    Size = Size0 + 1,
    map.det_insert(Str, Size0, !Map).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.dep_par_conj.
%---------------------------------------------------------------------------%
