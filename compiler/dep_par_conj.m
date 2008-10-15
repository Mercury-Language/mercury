%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2008 The University of Melbourne.
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
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
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

                % The varset and vartypes for the procedure being analysed.
                % These fields are updated when we add new variables.
                sync_varset                 :: prog_varset,
                sync_vartypes               :: vartypes

                % XXX We may also need the rtti_var_maps.
            ).

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

                % The current module. Updated when requesting a new
                % specialization, since to get the pred_id for the specialized
                % predicate we need to update the module_info.
                spec_module_info            :: module_info,

                % Parallelised procedures waiting to be added. Updated when
                % requesting a new specialization.
                spec_pending_procs          :: pending_par_procs
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

    % A map from a variable to the future object created for that variable.
    % If it maps e.g. X to FutureX, then
    %
    % - after a producer binds X to a value, it will signal FutureX, and
    % - before a consumer needs X, it will wait on FutureX.
    %
:- type future_map == map(prog_var, prog_var).

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

    PendingParProcs0 = [],
    DoneParProcs0 = map.init,
    list.foldl2(find_specialization_requests_in_proc(DoneParProcs0),
        ProcsToScan, !ModuleInfo, PendingParProcs0, PendingParProcs),
    add_requested_specialized_par_procs(PendingParProcs, DoneParProcs0,
        InitialModuleInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

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

        !:SyncInfo = sync_info(!.ModuleInfo, IgnoreVars, !.VarSet, !.VarTypes),
        sync_dep_par_conjs_in_goal(!Goal, InstMap0, _, !SyncInfo),
        !.SyncInfo = sync_info(_, _, !:VarSet, !:VarTypes),
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

:- pred fixup_and_reinsert_proc(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, module_info::in, module_info::out) is det.

fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo, !ModuleInfo) :-
    requantify_proc(!ProcInfo),
    recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
        !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred find_specialization_requests_in_proc(done_par_procs::in,
    pred_proc_id::in, module_info::in, module_info::out,
    pending_par_procs::in, pending_par_procs::out) is det.

find_specialization_requests_in_proc(DoneProcs, PredProcId, !ModuleInfo,
        !PendingParProcs) :-
    PredProcId = proc(PredId, ProcId),
    some [!PredInfo, !ProcInfo, !Goal, !SpecInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
            !:PredInfo, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, !:Goal),
        !:SpecInfo = spec_info(DoneProcs, !.ModuleInfo, !.PendingParProcs),
        specialize_sequences_in_goal(!Goal, !SpecInfo),
        !.SpecInfo = spec_info(_, !:ModuleInfo, !:PendingParProcs),
        proc_info_set_goal(!.Goal, !ProcInfo),
        % Optimization oppotunity: we should not fix up the same procedure
        % twice, i.e. in sync_dep_par_conjs_in_proc and here.
        fixup_and_reinsert_proc(PredId, ProcId, !.PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

:- pred add_requested_specialized_par_procs(pending_par_procs::in,
    done_par_procs::in, module_info::in, module_info::in, module_info::out)
    is det.

add_requested_specialized_par_procs(!.PendingParProcs, !.DoneParProcs,
        InitialModuleInfo, !ModuleInfo) :-
    (
        !.PendingParProcs = []
    ;
        !.PendingParProcs = [CallPattern - NewProc | !:PendingParProcs],
        % Move the procedure we are about to parallelise into the list of
        % done procedures, in case of recursive calls.
        svmap.det_insert(CallPattern, NewProc, !DoneParProcs),
        add_requested_specialized_par_proc(CallPattern, NewProc,
            !PendingParProcs, !.DoneParProcs, InitialModuleInfo, !ModuleInfo),
        add_requested_specialized_par_procs(!.PendingParProcs, !.DoneParProcs,
            InitialModuleInfo, !ModuleInfo)
    ).

:- pred add_requested_specialized_par_proc(par_proc_call_pattern::in,
    new_par_proc::in, pending_par_procs::in, pending_par_procs::out,
    done_par_procs::in, module_info::in, module_info::in, module_info::out)
    is det.

add_requested_specialized_par_proc(CallPattern, NewProc, !PendingParProcs,
        DoneParProcs, InitialModuleInfo, !ModuleInfo) :-
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
        SharedVars = set.from_list(map.keys(FutureMap)),
        sync_dep_par_conjunct(!.ModuleInfo, par_conjunct_is_proc_body,
            SharedVars, FutureMap, Goal0, Goal, InstMap0, _,
            !VarSet, !VarTypes),

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
        find_specialization_requests_in_proc(DoneParProcs, NewPredProcId,
            !ModuleInfo, !PendingParProcs)
    ).

:- pred map_arg_to_new_future(list(prog_var)::in, arg_pos::in,
    future_map::in, future_map::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

map_arg_to_new_future(HeadVars, FutureArg, !FutureMap, !VarSet, !VarTypes) :-
    HeadVar = list.det_index1(HeadVars, FutureArg),
    map.lookup(!.VarTypes, HeadVar, VarType),
    make_future_var(HeadVar, VarType, !VarSet, !VarTypes, FutureVar),
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
%-----------------------------------------------------------------------------%

    % Determine if a parallel conjunction is a dependent parallel conjunction.
    % If so, allocate futures for variables shared between conjuncts.
    % Insert wait and signal calls for those futures into the conjuncts.
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
        sync_dep_par_conjs_in_goal(SubGoal0, SubGoal, InstMap0, _,
            !SyncInfo),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
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
    !.SyncInfo = sync_info(ModuleInfo, IgnoreVars, VarSet0, VarTypes0),
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
        sync_dep_par_conj(ModuleInfo, SharedVars, Conjuncts, GoalInfo, NewGoal,
            InstMap, VarSet0, VarSet, VarTypes0, VarTypes),
        !:SyncInfo = sync_info(ModuleInfo, IgnoreVars, VarSet, VarTypes)
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
:- pred sync_dep_par_conj(module_info::in, set(prog_var)::in,
    list(hlds_goal)::in, hlds_goal_info::in, hlds_goal::out, instmap::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

sync_dep_par_conj(ModuleInfo, SharedVars, Goals, GoalInfo, NewGoal, InstMap,
        !VarSet, !VarTypes) :-
    SharedVarsList = set.to_sorted_list(SharedVars),
    list.map_foldl3(allocate_future(ModuleInfo), SharedVarsList,
        AllocateFutures, !VarSet, !VarTypes, map.init, FutureMap),
    list.map_foldl3(
        sync_dep_par_conjunct(ModuleInfo, par_conjunct_is_in_conjunction,
            SharedVars, FutureMap),
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
        Reason = promise_purity(dont_make_implicit_promises, Purity),
        NewGoal = hlds_goal(scope(Reason, NewGoal0), GoalInfo)
    ).

:- type par_conjunct_status
    --->    par_conjunct_is_in_conjunction
    ;       par_conjunct_is_proc_body.

:- pred sync_dep_par_conjunct(module_info::in, par_conjunct_status::in,
    set(prog_var)::in, future_map::in,
    hlds_goal::in, hlds_goal::out, instmap::in, instmap::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

sync_dep_par_conjunct(ModuleInfo, ParConjunctStatus, SharedVars, FutureMap,
        !Goal, !InstMap, !VarSet, !VarTypes) :-
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
        list.foldl3(insert_wait_in_goal(ModuleInfo, FutureMap),
            ConsumedVarsList, !Goal, !VarSet, !VarTypes),

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
        instmap.lookup_var(InstMap, Var, VarInst),
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
:- pred insert_wait_in_goal(module_info::in, future_map::in, prog_var::in,
    hlds_goal::in, hlds_goal::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ConsumedVar, Goal0) ->
        Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
        (
            GoalExpr0 = conj(ConjType, Goals0),
            (
                ConjType = plain_conj,
                insert_wait_in_plain_conj(ModuleInfo, FutureMap, ConsumedVar,
                    have_not_waited_in_conjunct, Waited,
                    Goals0, Goals, !VarSet, !VarTypes)
            ;
                ConjType = parallel_conj,
                insert_wait_in_par_conj(ModuleInfo, FutureMap, ConsumedVar,
                    have_not_waited_in_conjunct, Waited,
                    Goals0, Goals, !VarSet, !VarTypes)
            ),
            GoalExpr = conj(ConjType, Goals),
            Goal1 = hlds_goal(GoalExpr, GoalInfo0),
            (
                Waited = waited_in_conjunct,
                Goal = Goal1
            ;
                Waited = have_not_waited_in_conjunct,
                insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Goal1, Goal, !VarSet, !VarTypes)
            )
        ;
            GoalExpr0 = disj(Goals0),
            insert_wait_in_disj(ModuleInfo, FutureMap, ConsumedVar,
                Goals0, Goals, !VarSet, !VarTypes),
            GoalExpr = disj(Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
            ( ConsumedVar = SwitchVar ->
                insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Goal0, Goal, !VarSet, !VarTypes)
            ;
                insert_wait_in_cases(ModuleInfo, FutureMap, ConsumedVar,
                    Cases0, Cases, !VarSet, !VarTypes),
                GoalExpr = switch(SwitchVar, CanFail, Cases),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = if_then_else(Quant, Cond, Then0, Else0),
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
                insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Goal0, Goal, !VarSet, !VarTypes)
            ;
                % If ConsumedVar is not in the nonlocals of Cond, then it
                % must be in the nonlocals of at least one of Then0 and Else0.
                insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Then0, Then, !VarSet, !VarTypes),
                insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
                    Else0, Else, !VarSet, !VarTypes),
                GoalExpr = if_then_else(Quant, Cond, Then, Else),
                Goal = hlds_goal(GoalExpr, GoalInfo0)
            )
        ;
            GoalExpr0 = scope(Reason, SubGoal0),
            insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
                SubGoal0, SubGoal, !VarSet, !VarTypes),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            GoalExpr0 = negation(_SubGoal0),
            % We treat the negated goal just as we treat the condition of
            % an if-then-else.
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal, !VarSet, !VarTypes)
        ;
            ( GoalExpr0 = unify(_, _, _, _, _)
            ; GoalExpr0 = plain_call(_, _, _, _, _, _)
            ; GoalExpr0 = generic_call(_, _, _, _)
            ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            ),
            insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
                Goal0, Goal, !VarSet, !VarTypes)
        ;
            GoalExpr0 = shorthand(_),
            unexpected(this_file, "insert_wait_in_goal: shorthand")
        )
    ;
        insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
            Goal0, Goal, !VarSet, !VarTypes)
    ).

:- pred insert_wait_before_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_before_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    make_wait_goal(ModuleInfo, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hlds_goal_info, WaitGoal, Goal0,
        Goal).

:- pred insert_wait_after_goal(module_info::in, future_map::in,
    prog_var::in, hlds_goal::in, hlds_goal::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_after_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes) :-
    map.lookup(FutureMap, ConsumedVar, FutureVar),
    make_wait_goal(ModuleInfo, FutureVar, ConsumedVar, WaitGoal),
    conjoin_goals_update_goal_infos(Goal0 ^ hlds_goal_info, Goal0, WaitGoal,
        Goal).

:- type waited_in_conjunct
    --->    waited_in_conjunct
    ;       have_not_waited_in_conjunct.

    % Insert a wait for ConsumedVar in the first goal in the conjunction
    % that references it. Any later conjuncts will get the waited-for variable
    % without having to call wait.
    %
:- pred insert_wait_in_plain_conj(module_info::in, future_map::in,
    prog_var::in, waited_in_conjunct::in, waited_in_conjunct::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_plain_conj(_ModuleInfo, _FutureMap, _ConsumedVar, !Waited,
        [], [], !VarSet, !VarTypes).
insert_wait_in_plain_conj(ModuleInfo, FutureMap, ConsumedVar, !Waited,
        [Goal0 | Goals0], Goals, !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ConsumedVar, Goal0) ->
        % ConsumedVar appears in Goal0, so wait for it in Goal0. The code
        % in Goals0 will then be able to access ConsumedVar without any further
        % waiting.
        insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
            Goal0, Goal, !VarSet, !VarTypes),
        ( Goal ^ hlds_goal_expr = conj(plain_conj, GoalConj) ->
            Goals = GoalConj ++ Goals0
        ;
            Goals = [Goal | Goals0]
        ),
        !:Waited = waited_in_conjunct
    ;
        % ConsumedVar does not appear in Goal0, so wait for it in Goals0.
        insert_wait_in_plain_conj(ModuleInfo, FutureMap, ConsumedVar, !Waited,
            Goals0, Goals1, !VarSet, !VarTypes),
        Goals = [Goal0 | Goals1]
    ).

    % Insert a wait for ConsumedVar in the *every* goal in the conjunction
    % that references it. "Later" conjuncts cannot get the variable that
    % "earlier" conjuncts waited for, since those waits may not have finished
    % yet.
    %
:- pred insert_wait_in_par_conj(module_info::in, future_map::in, prog_var::in,
    waited_in_conjunct::in, waited_in_conjunct::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_par_conj(_ModuleInfo, _FutureMap, _ConsumedVar, !Waited,
        [], [], !VarSet, !VarTypes).
insert_wait_in_par_conj(ModuleInfo, FutureMap, ConsumedVar, !Waited,
        [Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes) :-
    ( var_in_nonlocals(ConsumedVar, Goal0) ->
        % ConsumedVar appears in Goal0, so wait for it in Goal0, but the code
        % in Goals0 will *not* be able to access ConsumedVar without waiting,
        % since the conjuncts are executed independently.
        insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
            Goal0, Goal1, !VarSet, !VarTypes),
        (
            !.Waited = have_not_waited_in_conjunct,
            Goal = Goal1
        ;
            !.Waited = waited_in_conjunct,
            clone_variable(ConsumedVar, !.VarSet, !.VarTypes,
                !VarSet, !VarTypes, map.init, Renaming, _CloneVar),
            rename_some_vars_in_goal(Renaming, Goal1, Goal)
        ),
        !:Waited = waited_in_conjunct
    ;
        Goal = Goal0
    ),
    insert_wait_in_par_conj(ModuleInfo, FutureMap, ConsumedVar, !Waited,
        Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_wait_in_disj(module_info::in, future_map::in, prog_var::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_disj(_ModuleInfo, _FutureMap, _ConsumedVar,
        [], [], !VarSet, !VarTypes).
insert_wait_in_disj(ModuleInfo, FutureMap, ConsumedVar,
        [Goal0 | Goals0], [Goal | Goals], !VarSet, !VarTypes) :-
    insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    insert_wait_in_disj(ModuleInfo, FutureMap, ConsumedVar,
        Goals0, Goals, !VarSet, !VarTypes).

:- pred insert_wait_in_cases(module_info::in, future_map::in, prog_var::in,
    list(case)::in, list(case)::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

insert_wait_in_cases(_ModuleInfo, _FutureMap, _ConsumedVar,
        [], [], !VarSet, !VarTypes).
insert_wait_in_cases(ModuleInfo, FutureMap, ConsumedVar,
        [Case0 | Cases0], [Case | Cases], !VarSet, !VarTypes) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    insert_wait_in_goal(ModuleInfo, FutureMap, ConsumedVar,
        Goal0, Goal, !VarSet, !VarTypes),
    Case = case(MainConsId, OtherConsIds, Goal),
    insert_wait_in_cases(ModuleInfo, FutureMap, ConsumedVar,
        Cases0, Cases, !VarSet, !VarTypes).

%-----------------------------------------------------------------------------%

    % Look for the first instance of the produced variable down every
    % branch. The first goal referring to the variable must produce it,
    % so insert a signal call right after that goal.
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
            insert_signal_in_goal(ModuleInfo, FutureMap, ProducedVar,
                SubGoal0, SubGoal, !VarSet, !VarTypes),
            GoalExpr = scope(Reason, SubGoal),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
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
    make_signal_goal(ModuleInfo, FutureMap, ProducedVar, SignalGoal),
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

:- pred var_in_nonlocals(prog_var::in, hlds_goal::in) is semidet.

var_in_nonlocals(Var, Goal) :-
    set.member(Var, goal_get_nonlocals(Goal)).

:- pred var_not_in_nonlocals(prog_var::in, hlds_goal::in) is semidet.

var_not_in_nonlocals(Var, Goal) :-
    not var_in_nonlocals(Var, Goal).

%-----------------------------------------------------------------------------%

    % Replace contiguous sequences of waits, a call to P, then signals by a
    % call to a parallelised procedure P'.  Queue P' to be created later,
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
        specialize_sequences_in_goal(SubGoal0, SubGoal, !SpecInfo),
        GoalExpr = scope(Reason, SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        ( GoalExpr0 = unify(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "shorthand")
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
    ( list.member(ProcId, pred_info_non_imported_procids(PredInfo)) ->
        % RevGoals0 = WaitGoals1   ++ RevGoals1
        % FwdGoals0 = SignalGoals1 ++ FwdGoals1
        %
        list.takewhile(is_wait_goal,   RevGoals0, WaitGoals1,   RevGoals1),
        list.takewhile(is_signal_goal, FwdGoals0, SignalGoals1, FwdGoals1),

        % Partition the wait and signal goals into those that are relevant
        % (i.e. they mention arguments of the call) and those that are not.
        %
        list.filter_map(relevant_wait_goal(CallVars), WaitGoals1,
            WaitPairs, IrrelevantWaitGoals),
        list.filter_map(relevant_signal_goal(CallVars), SignalGoals1,
            SignalPairs, IrrelevantSignalGoals),

        (
            WaitPairs = [],
            SignalPairs = []
        ->
            RevGoals = [Goal0 | RevGoals0],
            FwdGoals = FwdGoals0
        ;
            specialize_dep_par_call(WaitPairs, SignalPairs, Goal0, Goal,
                !SpecInfo),

            % After the replaced call may be further references to a signalled
            % or waited variable.  Add `get' goals after the transformed goal
            % to make sure the variable is bound.  We assume the get goals will
            % be simplified away if they turn out to be unnecessary.
            %
            % XXX the simplify pass that comes later doesn't always remove
            %     these calls even if they're unnecessary
            %
            list.map(make_get_goal(!.SpecInfo ^ spec_module_info),
                SignalPairs ++ WaitPairs, GetGoals),

            RevGoals = GetGoals ++ [Goal] ++ IrrelevantWaitGoals ++ RevGoals1,
            FwdGoals = IrrelevantSignalGoals ++ FwdGoals1
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

:- pred relevant_wait_goal(list(prog_var)::in, hlds_goal::in,
    future_var_pair::out) is semidet.

relevant_wait_goal(CallVars, hlds_goal(GoalExpr, _GoalInfo),
        future_var_pair(Future, WaitVar)) :-
    GoalExpr = plain_call(_, _, [Future, WaitVar], _, _, _),
    list.member(WaitVar, CallVars).

:- pred relevant_signal_goal(list(prog_var)::in, hlds_goal::in,
    future_var_pair::out) is semidet.

relevant_signal_goal(CallVars, hlds_goal(GoalExpr, _GoalInfo),
        future_var_pair(Future, SignalVar)) :-
    GoalExpr = plain_call(_, _, [Future, SignalVar], _, _, _),
    list.member(SignalVar, CallVars).

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
futurise_argtypes(ArgNo, [FutureArg | FutureArgs], [ArgType0 | ArgTypes0],
        [ArgType | ArgTypes]) :-
    ( ArgNo = FutureArg ->
        construct_future_type(ArgType0, ArgType),
        futurise_argtypes(ArgNo+1, FutureArgs,
            ArgTypes0, ArgTypes)
    ;
        ArgType = ArgType0,
        futurise_argtypes(ArgNo+1, [FutureArg | FutureArgs],
            ArgTypes0, ArgTypes)
    ).
futurise_argtypes(_, [_ | _], [], _) :-
    unexpected(this_file,
        "futurise_argtypes: more future arguments than argument types").

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
    make_future_var(SharedVar, SharedVarType, !VarSet, !VarTypes, FutureVar),
    svmap.det_insert(SharedVar, FutureVar, !FutureMap),

    ModuleName = mercury_par_builtin_module,
    PredName = "new_future",
    Args = [FutureVar],
    Features = [],
    InstMapSrc = [FutureVar - ground(shared, none)],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, pf_predicate,
        only_mode, detism_det, purity_pure, Args, Features, InstMapSrc,
        ModuleInfo, Context, AllocGoal).

    % Given a variable SharedVar of type SharedVarType, add a new variable
    % FutureVar of type future(SharedVarType).
    %
:- pred make_future_var(prog_var::in, mer_type::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var::out) is det.

make_future_var(SharedVar, SharedVarType, !VarSet, !VarTypes, FutureVar) :-
    construct_future_type(SharedVarType, FutureType),
    varset.lookup_name(!.VarSet, SharedVar, SharedVarName),
    svvarset.new_named_var("Future" ++ SharedVarName, FutureVar, !VarSet),
    svmap.det_insert(FutureVar, FutureType, !VarTypes).

    % Construct type future(T) given type T.
    %
:- pred construct_future_type(mer_type::in, mer_type::out) is det.

construct_future_type(T, FutureT) :-
    Future = qualified(mercury_par_builtin_module, "future"),
    FutureCtor = type_ctor(Future, 1),
    construct_type(FutureCtor, [T], FutureT).

:- pred make_wait_goal(module_info::in, prog_var::in, prog_var::in,
    hlds_goal::out) is det.

make_wait_goal(ModuleInfo, FutureVar, WaitVar, WaitGoal) :-
    make_wait_or_get(ModuleInfo, FutureVar, WaitVar, "wait", WaitGoal).

:- pred make_get_goal(module_info::in, future_var_pair::in,
    hlds_goal::out) is det.

make_get_goal(ModuleInfo, future_var_pair(FutureVar, WaitVar), WaitGoal) :-
    make_wait_or_get(ModuleInfo, FutureVar, WaitVar, "get", WaitGoal).

:- pred make_wait_or_get(module_info::in, prog_var::in, prog_var::in,
    string::in, hlds_goal::out) is det.

make_wait_or_get(ModuleInfo, FutureVar, WaitVar, PredName, WaitGoal) :-
    ModuleName = mercury_par_builtin_module,
    Args = [FutureVar, WaitVar],
    Features = [],
    InstMapSrc = [WaitVar - ground(shared, none)],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, pf_predicate,
        only_mode, detism_det, purity_pure, Args, Features, InstMapSrc,
        ModuleInfo, Context, WaitGoal).

:- pred make_signal_goal(module_info::in, future_map::in, prog_var::in,
    hlds_goal::out) is det.

make_signal_goal(ModuleInfo, FutureMap, ProducedVar, SignalGoal) :-
    FutureVar = map.lookup(FutureMap, ProducedVar),
    ModuleName = mercury_par_builtin_module,
    PredName = "signal",
    Args = [FutureVar, ProducedVar],
    Features = [],
    InstMapSrc = [],
    Context = term.context_init,
    goal_util.generate_simple_call(ModuleName, PredName, pf_predicate,
        only_mode, detism_det, purity_impure, Args, Features, InstMapSrc,
        ModuleInfo, Context, SignalGoal).

:- pred is_wait_goal(hlds_goal::in) is semidet.

is_wait_goal(hlds_goal(plain_call(_, _, _, _, _, SymName), _GoalInfo)) :-
    SymName = qualified(mercury_par_builtin_module, "wait").

:- pred is_signal_goal(hlds_goal::in) is semidet.

is_signal_goal(hlds_goal(plain_call(_, _, _, _, _, SymName), _GoalInfo)) :-
    SymName = qualified(mercury_par_builtin_module, "signal").

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
