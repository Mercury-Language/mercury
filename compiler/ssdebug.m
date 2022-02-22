%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: transform_hlds.ssdebug.m.
% Authors: oannet, wangp.
%
% The ssdebug module does a source-to-source tranformation on each procedure
% that allows the procedure to be debugged.
%
% The ssdebug transformation is disabled on standard library predicates,
% because it would introduce cyclic dependencies between ssdb.m and the
% standard library. Disabling the transformation on the standard library is
% also useful for maintaining decent performance.
%
% The transformation is divided into two passes.
%
% The first pass replaces calls to standard library predicates, and closure
% constructions referring to standard library predicates, by calls to and
% closures over proxy predicates. The proxy predicates generate events on
% behalf of the standard library predicates. There will be no events for
% further calls within the standard library, but that is better for
% performance.
%
% The first pass also inserts calls to a context update procedure before every
% procedure call (first or higher order). This will update global variables
% with the location of the next call site, which will be used by the CALL event
% handler. Context update calls are not required within proxy predicates.
%
% The second pass performs the main ssdebug transformation, adding calls to
% procedures to handle debugger events. The transformation depends on the
% determinism of the procedure.
%
% det/cc_multi:
%
%   The promise_equivalent_solutions is required if p is declared cc_nondet
%   but inferred cc_multi.
%
%   p(...) :-
%       promise_<original_purity> (
%           CallVarDescs = [ ... ],
%           Level = ...,
%           impure handle_event_call(ProcId, CallVarDescs, Level),
%           promise_equivalent_solutions [ ... ] (
%               <original body>     % renaming outputs
%           ),
%           ExitVarDescs = [ ... | CallVarDescs ],
%           impure handle_event_exit(ProcId, ExitVarDescs, DoRetry),
%           (
%               DoRetry = do_retry,
%               p(...)
%           ;
%               DoRetry = do_not_retry,
%               % bind outputs
%           )
%       ).
%
% semidet/cc_nondet:
%
%   The promise_equivalent_solutions is required only if p is declared
%   cc_nondet.
%
%   p(...) :-
%       promise_<original_purity> (
%           CallVarDescs = [ ... ],
%           Level = ...,
%           impure handle_event_call(ProcId, CallVarDescs, Level),
%           ( if
%               promise_equivalent_solutions [...] (
%                   <original body>     % renaming outputs
%               )
%           then
%               ExitVarDescs = [ ... | CallVarDescs ],
%               impure handle_event_exit(ProcId, ExitVarDescs, DoRetryA),
%               (
%                   DoRetryA = do_retry,
%                   p(...)
%               ;
%                   DoRetryA = do_not_retry,
%                   % bind outputs
%               )
%           else
%               impure handle_event_fail(ProcId, CallVarDescs, DoRetryB),
%               (
%                   DoRetryB = do_retry,
%                   p(...)
%               ;
%                   DoRetryB = do_not_retry,
%                   fail
%               )
%           )
%       ).
%
% nondet:
%
%   p(...) :-
%       promise_<original_purity> (
%           (
%               CallVarDescs = [ ... ],
%               Level = ...,
%               impure handle_event_call_nondet(ProcId, CallVarDescs, Level),
%               <original body>,
%               ExitVarDescs = [ ... | CallVarDescs ],
%               (
%                   impure handle_event_exit_nondet(ProcId, ExitVarDescs)
%                   % Go to fail port if retry.
%               ;
%                   % preserve_backtrack_into,
%                   impure handle_event_redo_nondet(ProcId, ExitVarDescs),
%                   fail
%               )
%           ;
%               % preserve_backtrack_into
%               FailVarDescs = [ ... ],
%               impure handle_event_fail_nondet(ProcId, FailVarDescs, DoRetry),
%               (
%                   DoRetry = do_retry,
%                   p(...)
%               ;
%                   DoRetry = do_not_retry,
%                   fail
%               )
%           )
%       ).
%
% failure:
%
%   p(...) :-
%       promise_<original_purity> (
%           CallVarDescs = [ ... ],
%           Level = ...,
%           impure handle_event_call(ProcId, CallVarDescs, Level),
%           (
%               <original body>
%           ;
%               % preserve_backtrack_into
%               impure handle_event_fail(ProcId, CallVarDescs, DoRetry),
%               (
%                   DoRetry = do_retry,
%                   p(...)
%               ;
%                   DoRetry = do_not_retry,
%                   fail
%               )
%           )
%       ).
%
% erroneous:
%
%   p(...) :-
%       promise_<original_purity> (
%           CallVarDescs = [ ... ],
%           Level = ...,
%           impure handle_event_call(ProcId, CallVarDescs, Level),
%           <original body>
%       ).
%
% where CallVarDescs, ExitVarDescs are lists of var_value and Level
% is a ssdb.ssdb_tracel_level.
%
%    :- type var_value
%        --->    unbound_head_var(var_name, pos)           :: out      variable
%        ;       some [T] bound_head_var(var_name, pos, T) :: in       variable
%        ;       some [T] bound_other_var(var_name, T).    :: internal variable
%
%    :- type var_name == string.
%
%    :- type pos == int.
%
%    :- type ssdb_tracel_level
%       --->    shallow
%       ;       deep.
%
% Output head variables may appear twice in a variable description list --
% initially unbound, then overridden by a bound_head_var functor. Then the
% ExitVarDescs can add output variable bindings to the CallVarDescs list,
% instead of building new lists. The pos fields give the argument numbers
% of head variables.
%
% The ProcId is of type ssdb.ssdb_proc_id.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ssdebug.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.

:- import_module io.

:- inst shallow_or_deep for globals.ssdb_trace_level/0
    --->    ssdb_shallow
    ;       ssdb_deep.

:- pred ssdebug_transform_module(ssdb_trace_level::in(shallow_or_deep),
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.
:- import_module parse_tree.pred_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

ssdebug_transform_module(SSTraceLevel, !ModuleInfo, !IO) :-
    (
        SSTraceLevel = ssdb_shallow,
        % With the shallow trace level, the parent of a library procedure
        % will also be have trace level shallow, thus we don't need to proxy
        % the library methods.
        process_valid_nonimported_procs(
            update_module(ssdebug_process_proc_if_needed(SSTraceLevel)),
            !ModuleInfo)
    ;
        SSTraceLevel = ssdb_deep,
        ssdebug_first_pass(!ModuleInfo),
        process_valid_nonimported_procs(
            update_module(ssdebug_process_proc_if_needed(SSTraceLevel)),
            !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%
%
% Create proxies for standard library predicates and insert context updates.
%

:- type proxy_map == map(pred_id, maybe(pred_id)).

:- pred ssdebug_first_pass(module_info::in, module_info::out) is det.

ssdebug_first_pass(!ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl2(ssdebug_first_pass_in_pred, PredIds,
        map.init, _ProxyMap, !ModuleInfo).

:- pred ssdebug_first_pass_in_pred(pred_id::in, proxy_map::in, proxy_map::out,
    module_info::in, module_info::out) is det.

ssdebug_first_pass_in_pred(PredId, !ProxyMap, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
    list.foldl2(ssdebug_first_pass_in_proc(PredId), ProcIds,
        !ProxyMap, !ModuleInfo).

:- pred ssdebug_first_pass_in_proc(pred_id::in, proc_id::in,
    proxy_map::in, proxy_map::out, module_info::in, module_info::out) is det.

ssdebug_first_pass_in_proc(PredId, ProcId, !ProxyMap, !ModuleInfo) :-
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId, PredInfo,
            !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, Goal0),
        ssdebug_first_pass_in_goal(Goal0, Goal, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

:- pred ssdebug_first_pass_in_goal(hlds_goal::in, hlds_goal::out,
    proc_info::in, proc_info::out, proxy_map::in, proxy_map::out,
    module_info::in, module_info::out) is det.

ssdebug_first_pass_in_goal(!Goal, !ProcInfo, !ProxyMap, !ModuleInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_, _, _, Unification0, _),
        ( if
            Unification0 = construct(_, ConsId0, _, _, _, _, _),
            ConsId0 = closure_cons(ShroudedPredProcId, lambda_normal)
        then
            PredProcId = unshroud_pred_proc_id(ShroudedPredProcId),
            PredProcId = proc(PredId, ProcId),
            lookup_proxy_pred(PredId, MaybeNewPredId, !ProxyMap, !ModuleInfo),
            (
                MaybeNewPredId = yes(NewPredId),
                NewPredProcId = proc(NewPredId, ProcId),
                NewShroundPredProcId = shroud_pred_proc_id(NewPredProcId),
                ConsId = closure_cons(NewShroundPredProcId, lambda_normal),
                Unification = Unification0 ^ construct_cons_id := ConsId,
                GoalExpr = GoalExpr0 ^ unify_kind := Unification,
                !:Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                MaybeNewPredId = no
            )
        else
            true
        )
    ;
        GoalExpr0 = plain_call(PredId, ProcId, Args, Builtin, Context,
            _SymName),
        (
            Builtin = not_builtin,
            lookup_proxy_pred(PredId, MaybeNewPredId, !ProxyMap, !ModuleInfo),
            (
                MaybeNewPredId = yes(NewPredId),
                module_info_pred_info(!.ModuleInfo, NewPredId, NewPredInfo),
                NewModuleName = pred_info_module(NewPredInfo),
                NewPredName = pred_info_name(NewPredInfo),
                NewSymName = qualified(NewModuleName, NewPredName),
                GoalExpr = plain_call(NewPredId, ProcId, Args, Builtin,
                    Context, NewSymName),
                !:Goal = hlds_goal(GoalExpr, GoalInfo0)
            ;
                MaybeNewPredId = no
            ),
            insert_context_update_call(!.ModuleInfo, !Goal, !ProcInfo)
        ;
            Builtin = inline_builtin
        )
    ;
        GoalExpr0 = generic_call(_, _, _, _, _),
        insert_context_update_call(!.ModuleInfo, !Goal, !ProcInfo)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl3(ssdebug_first_pass_in_goal, Goals0, Goals, !ProcInfo,
            !ProxyMap, !ModuleInfo),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        list.map_foldl3(ssdebug_first_pass_in_goal, Goals0, Goals, !ProcInfo,
            !ProxyMap, !ModuleInfo),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map_foldl3(ssdebug_first_pass_in_case, Cases0, Cases, !ProcInfo,
            !ProxyMap, !ModuleInfo),
        GoalExpr = switch(Var, CanFail, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        ssdebug_first_pass_in_goal(SubGoal0, SubGoal, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        GoalExpr = negation(SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ssdebug_first_pass_in_goal(SubGoal0, SubGoal, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        GoalExpr = scope(Reason, SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        ssdebug_first_pass_in_goal(Cond0, Cond, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        ssdebug_first_pass_in_goal(Then0, Then, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        ssdebug_first_pass_in_goal(Else0, Else, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "unexpected shorthand")
    ).

:- pred ssdebug_first_pass_in_case(case::in, case::out,
    proc_info::in, proc_info::out,
    proxy_map::in, proxy_map::out, module_info::in, module_info::out) is det.

ssdebug_first_pass_in_case(Case0, Case, !ProcInfo, !ProxyMap, !ModuleInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    ssdebug_first_pass_in_goal(Goal0, Goal, !ProcInfo, !ProxyMap, !ModuleInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

    % Look up the proxy for a predicate, creating one if appropriate.
    %
:- pred lookup_proxy_pred(pred_id::in, maybe(pred_id)::out,
    proxy_map::in, proxy_map::out, module_info::in, module_info::out) is det.

lookup_proxy_pred(PredId, MaybeNewPredId, !ProxyMap, !ModuleInfo) :-
    ( if map.search(!.ProxyMap, PredId, MaybeNewPredId0) then
        MaybeNewPredId = MaybeNewPredId0
    else
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        ( if mercury_std_library_module_name(PredModule) then
            create_proxy_pred(PredId, PredInfo, NewPredId, !ModuleInfo),
            MaybeNewPredId = yes(NewPredId)
        else
            MaybeNewPredId = no
        ),
        map.det_insert(PredId, MaybeNewPredId, !ProxyMap)
    ).

:- pred create_proxy_pred(pred_id::in, pred_info::in, pred_id::out,
    module_info::in, module_info::out) is det.

create_proxy_pred(PredId, !.PredInfo, NewPredId, !ModuleInfo) :-
    pred_info_set_status(pred_status(status_local), !PredInfo),

    ProcIds = pred_info_valid_procids(!.PredInfo),
    list.foldl2(create_proxy_proc(PredId), ProcIds, !PredInfo,
        !ModuleInfo),

    % The proxy is defined in this module, even if the original is not.
    module_info_get_name(!.ModuleInfo, ModuleName),
    pred_info_set_module_name(ModuleName, !PredInfo),
    % Change the name, so that the proxy is not confused with the original.
    Name = pred_info_name(!.PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
    Transform = tn_ssdb_stdlib_proxy(PredOrFunc),
    make_transformed_pred_name(Name, Transform, ProxyName),
    pred_info_set_name(ProxyName, !PredInfo),

    % Set the predicate origin so that the later pass can find the name of
    % the original predicate.
    pred_info_get_origin(!.PredInfo, Origin),
    NewOrigin = origin_transformed(transform_source_to_source_debug,
        Origin, PredId),
    pred_info_set_origin(NewOrigin, !PredInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(!.PredInfo, NewPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred create_proxy_proc(pred_id::in, proc_id::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out) is det.

create_proxy_proc(PredId, ProcId, !PredInfo, !ModuleInfo) :-
    some [!ProcInfo] (
        % The proxy just has to call the original procedure.
        pred_info_proc_info(!.PredInfo, ProcId, !:ProcInfo),
        proc_info_get_headvars(!.ProcInfo, Args),
        pred_info_get_sym_name(!.PredInfo, SymName),
        CallExpr = plain_call(PredId, ProcId, Args, not_builtin, no, SymName),
        proc_info_get_goal(!.ProcInfo, hlds_goal(_, GoalInfo0)),
        proc_info_interface_determinism(!.ProcInfo, Detism),
        goal_info_set_determinism(Detism, GoalInfo0, GoalInfo),
        Goal = hlds_goal(CallExpr, GoalInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
        recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
            !ProcInfo, !ModuleInfo),
        pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo)
    ).

:- pred insert_context_update_call(module_info::in,
    hlds_goal::in, hlds_goal::out, proc_info::in, proc_info::out) is det.

insert_context_update_call(ModuleInfo, Goal0, Goal, !ProcInfo) :-
    Goal0 = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    Context = term.context(FileName, LineNumber),

    some [!VarSet, !VarTypes] (
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        make_string_const_construction_alloc(FileName, yes("FileName"),
            MakeFileName, FileNameVar, !VarSet, !VarTypes),
        make_int_const_construction_alloc(LineNumber, yes("LineNumber"),
            MakeLineNumber, LineNumberVar, !VarSet, !VarTypes),
        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo)
    ),

    ArgVars = [FileNameVar, LineNumberVar],
    Features = [],
    instmap_delta_init_reachable(InstMapDelta),
    generate_simple_call(ModuleInfo, mercury_ssdb_builtin_module,
        "set_context", pf_predicate, only_mode, detism_det, purity_impure,
        [], ArgVars, Features, InstMapDelta, Context, SetContextGoal),

    conj_list_to_goal([MakeFileName, MakeLineNumber, SetContextGoal, Goal0],
        GoalInfo, Goal).

%-----------------------------------------------------------------------------%
%
% The main transformation.
%

:- pred ssdebug_process_proc_if_needed(ssdb_trace_level::in,
    pred_proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc_if_needed(SSTraceLevel, PredProcId,
        !ProcInfo, !ModuleInfo) :-
    (
        SSTraceLevel = ssdb_none
    ;
        SSTraceLevel = ssdb_shallow,
        PredProcId = proc(PredId, _ProcId),
        % Only transform the procedures in the interface.
        % XXX We still need to fix the ssdb so that events generated
        % below the shallow call event aren't seen.
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
        ( if pred_info_is_exported(PredInfo) then
            ssdebug_process_proc(SSTraceLevel, PredProcId,
                !ProcInfo, !ModuleInfo)
        else
            true
        )
    ;
        SSTraceLevel = ssdb_deep,
        % Transfrom all procedures.
        ssdebug_process_proc(SSTraceLevel, PredProcId, !ProcInfo, !ModuleInfo)
    ).

:- pred ssdebug_process_proc(ssdb_trace_level::in(shallow_or_deep),
    pred_proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc(SSTraceLevel, PredProcId, !ProcInfo, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    proc_info_get_argmodes(!.ProcInfo, ArgModes),
    ( if all_args_fully_input_or_output(!.ModuleInfo, ArgModes) then
        % We have different transformations for procedures of different
        % determinisms.

        % XXX It might be possible to factor out the common code in the four
        % ssdebug_process_proc_* predicates.

        proc_info_get_inferred_determinism(!.ProcInfo, Determinism),
        (
            ( Determinism = detism_det
            ; Determinism = detism_cc_multi
            ),
            ssdebug_process_proc_det(SSTraceLevel, PredId, ProcId,
                !ProcInfo, !ModuleInfo)
        ;
            ( Determinism = detism_semi
            ; Determinism = detism_cc_non
            ),
            ssdebug_process_proc_semi(SSTraceLevel, PredId, ProcId,
                !ProcInfo, !ModuleInfo)
        ;
            ( Determinism = detism_multi
            ; Determinism = detism_non
            ),
            ssdebug_process_proc_nondet(SSTraceLevel, PredId, ProcId,
                !ProcInfo, !ModuleInfo)
        ;
            Determinism = detism_erroneous,
            ssdebug_process_proc_erroneous(SSTraceLevel, PredId, ProcId,
                !ProcInfo, !ModuleInfo)
        ;
            Determinism = detism_failure,
            ssdebug_process_proc_failure(SSTraceLevel, PredId, ProcId,
                !ProcInfo, !ModuleInfo)
        )
    else
        % In the case of a mode which is not fully input or output,
        % we don't transform the procedure, since we don't know how.
        true
    ).

    % Source-to-source transformation for a deterministic goal.
    %
:- pred ssdebug_process_proc_det(ssdb_trace_level::in(shallow_or_deep),
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc_det(SSTraceLevel, PredId, ProcId,
        !ProcInfo, !ModuleInfo) :-
    some [!PredInfo, !VarSet, !VarTypes] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        proc_info_get_goal(!.ProcInfo, OrigBodyGoal),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        get_stripped_headvars(!.PredInfo, !.ProcInfo, FullHeadVars, HeadVars,
            ArgModes),

        % Make the ssdb_proc_id.
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !VarSet, !VarTypes),

        % Make a list which records the value for each of the head
        % variables at the call port.
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, map.init, BoundVarDescsAtCall),

        % Set the ssdb_tracing_level.
        make_level_construction(SSTraceLevel,
            ConstructLevelGoal, LevelVar, !VarSet, !VarTypes),

        % Generate the call to handle_event_call(ProcId, VarList).
        make_handle_event("handle_event_call",
            [ProcIdVar, CallArgListVar, LevelVar], HandleEventCallGoal,
            !ModuleInfo, !VarSet, !VarTypes),

        % In the case of a retry, the output variables will be bound by the
        % retried call.
        get_output_args(!.ModuleInfo, HeadVars, ArgModes, OutputVars),
        rename_outputs(OutputVars, OrigBodyGoal, RenamedBodyGoal,
            AssignOutputsGoal, Renaming, !VarSet, !VarTypes),

        % If the procedure (which we call recursively on retry) is declared
        % cc_nondet but inferred cc_multi, then we must put the original body
        % in a single solution context.
        proc_info_interface_determinism(!.ProcInfo, ProcDetism),
        determinism_components(ProcDetism, CanFail, _Solns),
        (
            CanFail = can_fail,
            map.apply_to_list(OutputVars, Renaming, RenamedOutputVars),
            add_promise_equivalent_solutions(RenamedOutputVars,
                RenamedBodyGoal, ScopedRenamedBodyGoal)
        ;
            CanFail = cannot_fail,
            ScopedRenamedBodyGoal = RenamedBodyGoal
        ),

        % Make the variable list at the exit port. It's currently a
        % completely new list instead of adding on to the list generated
        % for the call port.
        update_instmap(OrigBodyGoal, InitInstMap, FinalInstMap),
        make_arg_list(0, FinalInstMap, HeadVars, Renaming, ExitArgListVar,
            ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

        % Generate the call to handle_event_exit.
        make_retry_var("DoRetry", RetryVar, !VarSet, !VarTypes),
        make_handle_event("handle_event_exit",
            [ProcIdVar, ExitArgListVar, RetryVar], HandleEventExitGoal,
            !ModuleInfo, !VarSet, !VarTypes),

        % Generate the recursive call in the case of a retry.
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
            FullHeadVars, RecursiveGoal),

        % Create the switch on Retry at exit port.
        make_switch_goal(RetryVar, RecursiveGoal, AssignOutputsGoal,
            SwitchGoal),

        % Put it all together.
        BodyGoals = list.condense([
            ProcIdGoals,
            CallArgListGoals,
            [ConstructLevelGoal],
            [HandleEventCallGoal],
            [ScopedRenamedBodyGoal],
            ExitArgListGoals,
            [HandleEventExitGoal],
            [SwitchGoal]
        ]),
        commit_goal_changes(BodyGoals, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.VarSet, !.VarTypes)
    ).

    % Source-to-source transformation for a semidet goal.
    %
:- pred ssdebug_process_proc_semi(ssdb_trace_level::in(shallow_or_deep),
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc_semi(SSTraceLevel, PredId, ProcId,
        !ProcInfo, !ModuleInfo) :-
    some [!PredInfo, !VarSet, !VarTypes] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        proc_info_get_goal(!.ProcInfo, OrigBodyGoal),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        get_stripped_headvars(!.PredInfo, !.ProcInfo, FullHeadVars, HeadVars,
            ArgModes),

        % Make the ssdb_proc_id.
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !VarSet, !VarTypes),

        % Make a list which records the value for each of the head
        % variables at the call port.
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, map.init, BoundVarDescsAtCall),

        % Set the ssdb_tracing_level.
        make_level_construction(SSTraceLevel,
            ConstructLevelGoal, LevelVar, !VarSet, !VarTypes),

        % Generate the call to handle_event_call.
        make_handle_event("handle_event_call",
            [ProcIdVar, CallArgListVar, LevelVar],
            HandleEventCallGoal, !ModuleInfo, !VarSet, !VarTypes),

        % In the case of a retry, the output variables will be bound by the
        % retried call.
        get_output_args(!.ModuleInfo, HeadVars, ArgModes, OutputVars),
        rename_outputs(OutputVars, OrigBodyGoal, RenamedBodyGoal,
            AssignOutputsGoal, Renaming, !VarSet, !VarTypes),

        % Make the variable list at the exit port. It's currently a
        % completely new list instead of adding on to the list generated
        % for the call port.
        update_instmap(OrigBodyGoal, InitInstMap, FinalInstMap),
        make_arg_list(0, FinalInstMap, HeadVars, Renaming, ExitArgListVar,
            ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

        % Generate the call to handle_event_exit.
        make_retry_var("DoRetryA", RetryAVar, !VarSet, !VarTypes),
        make_handle_event("handle_event_exit",
            [ProcIdVar, ExitArgListVar, RetryAVar], HandleEventExitGoal,
            !ModuleInfo, !VarSet, !VarTypes),

        % Generate the recursive call in the case of a retry.
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
            FullHeadVars, RecursiveGoal),

        % Generate the list of arguments at the fail port.
        make_arg_list(0, InitInstMap, [], Renaming, FailArgListVar,
            FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

        % Generate the call to handle_event_fail.
        make_retry_var("DoRetryB", RetryBVar, !VarSet, !VarTypes),
        make_handle_event("handle_event_fail",
            [ProcIdVar, FailArgListVar, RetryBVar], HandleEventFailGoal,
            !ModuleInfo, !VarSet, !VarTypes),

        proc_info_interface_determinism(!.ProcInfo, ProcDetism),
        ImpureGoalInfo = impure_goal_info(ProcDetism),

        % The condition of the if-then-else is the original body with renamed
        % output variables. Introduce a promise_equivalent_solutions scope to
        % put it into a single solution context if the procedure (which we call
        % recursively later) was _declared_ to have more solutions.
        determinism_components(ProcDetism, _CanFail, Solns),
        (
            Solns = at_most_one,
            CondGoal = RenamedBodyGoal
        ;
            ( Solns = at_most_many_cc
            ; Solns = at_most_many
            ),
            map.apply_to_list(OutputVars, Renaming, RenamedOutputVars),
            add_promise_equivalent_solutions(RenamedOutputVars,
                RenamedBodyGoal, CondGoal)
        ;
            Solns = at_most_zero,
            unexpected($pred, "zero solutions")
        ),

        % Create the `then' branch.
        make_switch_goal(RetryAVar, RecursiveGoal, AssignOutputsGoal,
            SwitchExitPortGoal),
        GoalsThen = list.condense([
            ExitArgListGoals,
            [HandleEventExitGoal],
            [SwitchExitPortGoal]
        ]),
        ThenGoal = hlds_goal(conj(plain_conj, GoalsThen), ImpureGoalInfo),

        % Create the `else' branch.
        make_switch_goal(RetryBVar, RecursiveGoal, fail_goal,
            SwitchFailPortGoal),
        GoalsElse = list.condense([
            FailArgListGoals,
            [HandleEventFailGoal],
            [SwitchFailPortGoal]
        ]),
        ElseGoal = hlds_goal(conj(plain_conj, GoalsElse), ImpureGoalInfo),

        % Put it all together.
        OrigBodyGoal = hlds_goal(_, OrigGoalInfo),
        goal_info_set_determinism(ProcDetism, OrigGoalInfo, IteGoalInfo),
        IteGoal = hlds_goal(if_then_else([], CondGoal, ThenGoal, ElseGoal),
            IteGoalInfo),
        BodyGoals = list.condense([
            ProcIdGoals,
            CallArgListGoals,
            [ConstructLevelGoal],
            [HandleEventCallGoal],
            [IteGoal]
        ]),
        commit_goal_changes(BodyGoals, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.VarSet, !.VarTypes)
    ).

    % Source-to-source transformation for a nondeterministic procedure.
    %
:- pred ssdebug_process_proc_nondet(ssdb_trace_level::in(shallow_or_deep),
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc_nondet(SSTraceLevel, PredId, ProcId,
        !ProcInfo, !ModuleInfo) :-
    some [!PredInfo, !VarSet, !VarTypes] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        proc_info_get_goal(!.ProcInfo, OrigBodyGoal),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        get_stripped_headvars(!.PredInfo, !.ProcInfo, FullHeadVars, HeadVars,
            _ArgModes),

        % Make the ssdb_proc_id.
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !VarSet, !VarTypes),

        % Make a list which records the value for each of the head
        % variables at the call port.
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, map.init, BoundVarDescsAtCall),

        % Set the ssdb_tracing_level.
        make_level_construction(SSTraceLevel,
            ConstructLevelGoal, LevelVar, !VarSet, !VarTypes),

        % Generate the call to handle_event_call.
        make_handle_event("handle_event_call_nondet",
            [ProcIdVar, CallArgListVar, LevelVar],
            HandleEventCallGoal, !ModuleInfo, !VarSet, !VarTypes),

        % Make the variable list at the exit port. It's currently a
        % completely new list instead of adding on to the list generated
        % for the call port.
        update_instmap(OrigBodyGoal, InitInstMap, FinalInstMap),
        make_arg_list(0, FinalInstMap, HeadVars, map.init, ExitArgListVar,
            ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

        proc_info_interface_determinism(!.ProcInfo, ProcDetism),

        % Create the disjunct that handles call, exit and redo ports.
        make_handle_event("handle_event_exit_nondet",
            [ProcIdVar, ExitArgListVar],
            HandleEventExitGoal, !ModuleInfo, !VarSet, !VarTypes),
        ExitDisjunct = HandleEventExitGoal,

        make_handle_event("handle_event_redo_nondet",
            [ProcIdVar, ExitArgListVar],
            HandleEventRedoGoal, !ModuleInfo, !VarSet, !VarTypes),
        RedoDisjunct = hlds_goal(conj(plain_conj,
            [HandleEventRedoGoal, fail_goal]),
            impure_backtrack_goal_info(detism_failure)),

        ExitOrRedoGoal = hlds_goal(disj([ExitDisjunct, RedoDisjunct]),
            impure_goal_info(detism_non)),
        CallExitRedoDisjunctGoals = list.condense([
            CallArgListGoals,
            [ConstructLevelGoal],
            [HandleEventCallGoal],
            [OrigBodyGoal],
            ExitArgListGoals,
            [ExitOrRedoGoal]
        ]),
        CallExitRedoDisjunct = hlds_goal(
            conj(plain_conj, CallExitRedoDisjunctGoals),
            impure_goal_info(ProcDetism)),

        % Create the disjunct that handles the fail port.
        FailArgListVar = CallArgListVar,
        FailArgListGoals = CallArgListGoals,
        make_retry_var("DoRetry", RetryVar, !VarSet, !VarTypes),
        make_handle_event("handle_event_fail_nondet",
            [ProcIdVar, FailArgListVar, RetryVar],
            HandleEventFailGoal, !ModuleInfo, !VarSet, !VarTypes),
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
            FullHeadVars, RecursiveGoal),
        make_switch_goal(RetryVar, RecursiveGoal, fail_goal,
            SwitchFailPortGoal),
        FailDisjunctGoals = list.condense([
            FailArgListGoals,
            [HandleEventFailGoal],
            [SwitchFailPortGoal]
        ]),
        FailDisjunct = hlds_goal(conj(plain_conj, FailDisjunctGoals),
            impure_backtrack_goal_info(ProcDetism)),

        % Put it together.
        BodyDisj = hlds_goal(disj([CallExitRedoDisjunct, FailDisjunct]),
            impure_goal_info(ProcDetism)),
        BodyGoals = ProcIdGoals ++ [BodyDisj],
        commit_goal_changes(BodyGoals, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.VarSet, !.VarTypes)
    ).

    % Source-to-source transformation for a failure procedure.
    %
:- pred ssdebug_process_proc_failure(ssdb_trace_level::in(shallow_or_deep),
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc_failure(SSTraceLevel, PredId, ProcId,
        !ProcInfo, !ModuleInfo) :-
    some [!PredInfo, !VarSet, !VarTypes] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        proc_info_get_goal(!.ProcInfo, OrigBodyGoal),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        get_stripped_headvars(!.PredInfo, !.ProcInfo, FullHeadVars, HeadVars,
            _ArgModes),

        % Make the ssdb_proc_id.
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !VarSet, !VarTypes),

        % Make a list which records the value for each of the head
        % variables at the call port.
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, map.init, _BoundVarDescsAtCall),

        % Set the ssdb_tracing_level.
        make_level_construction(SSTraceLevel,
            ConstructLevelGoal, LevelVar, !VarSet, !VarTypes),

        % Generate the call to handle_event_call.
        make_handle_event("handle_event_call",
            [ProcIdVar, CallArgListVar, LevelVar],
            HandleEventCallGoal, !ModuleInfo, !VarSet, !VarTypes),

        % Generate the call to handle_event_fail.
        FailArgListVar = CallArgListVar,
        make_retry_var("DoRetry", RetryVar, !VarSet, !VarTypes),
        make_handle_event("handle_event_fail",
            [ProcIdVar, FailArgListVar, RetryVar],
            HandleEventFailGoal, !ModuleInfo, !VarSet, !VarTypes),

        % Generate the recursive call in the case of a retry.
        make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
            FullHeadVars, RecursiveGoal),

        % Create the switch on Retry at fail port.
        make_switch_goal(RetryVar, RecursiveGoal, fail_goal, SwitchGoal),

        % Put it all together.
        proc_info_interface_determinism(!.ProcInfo, ProcDetism),
        FailDisjunct = hlds_goal(
            conj(plain_conj, [HandleEventFailGoal, SwitchGoal]),
            impure_backtrack_goal_info(ProcDetism)),
        DisjGoal = hlds_goal(disj([OrigBodyGoal, FailDisjunct]),
            impure_goal_info(ProcDetism)),
        BodyGoals = list.condense([
            ProcIdGoals,
            CallArgListGoals,
            [ConstructLevelGoal],
            [HandleEventCallGoal],
            [DisjGoal]
        ]),
        commit_goal_changes(BodyGoals, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.VarSet, !.VarTypes)
    ).

    % Source-to-source transformation for an erroneous procedure.
    %
:- pred ssdebug_process_proc_erroneous(ssdb_trace_level::in(shallow_or_deep),
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

ssdebug_process_proc_erroneous(SSTraceLevel, PredId, ProcId,
        !ProcInfo, !ModuleInfo) :-
    some [!PredInfo, !VarSet, !VarTypes] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        proc_info_get_goal(!.ProcInfo, OrigBodyGoal),
        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),
        get_stripped_headvars(!.PredInfo, !.ProcInfo, _FullHeadVars, HeadVars,
            _ArgModes),

        % Make the ssdb_proc_id.
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !VarSet, !VarTypes),

        % Make a list which records the value for each of the head
        % variables at the call port.
        proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitInstMap),
        make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
            CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
            !VarTypes, map.init, _BoundVarDescsAtCall),

        % Set the ssdb_tracing_level.
        make_level_construction(SSTraceLevel,
            ConstructLevelGoal, LevelVar, !VarSet, !VarTypes),

        % Generate the call to handle_event_call(ProcId, VarList).
        make_handle_event("handle_event_call",
            [ProcIdVar, CallArgListVar, LevelVar],
            HandleEventCallGoal, !ModuleInfo, !VarSet, !VarTypes),

        % Put it all together.
        BodyGoals = list.condense([
            ProcIdGoals,
            CallArgListGoals,
            [ConstructLevelGoal],
            [HandleEventCallGoal],
            [OrigBodyGoal]
        ]),
        commit_goal_changes(BodyGoals, PredId, ProcId, !.PredInfo, !ProcInfo,
            !ModuleInfo, !.VarSet, !.VarTypes)
    ).

:- pred get_stripped_headvars(pred_info::in, proc_info::in,
    list(prog_var)::out, list(prog_var)::out, list(mer_mode)::out) is det.

get_stripped_headvars(PredInfo, ProcInfo, FullHeadVars, HeadVars, ArgModes) :-
    % XXX using orig_arity here does not work when the predicate is one
    % produced by higher-order specialization. See tests/valid/gh89.m.
    PredArity = pred_info_orig_arity(PredInfo),
    proc_info_get_headvars(ProcInfo, FullHeadVars),
    proc_info_get_argmodes(ProcInfo, FullArgModes),
    list.length(FullHeadVars, NumHeadVars),
    % Strip off the extra type_info arguments inserted at the front by
    % polymorphism.m.
    NumToDrop = NumHeadVars - PredArity,
    list.det_drop(NumToDrop, FullHeadVars, HeadVars),
    list.det_drop(NumToDrop, FullArgModes, ArgModes).

:- pred get_output_args(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, list(prog_var)::out) is det.

get_output_args(ModuleInfo, HeadVars, ArgModes, OutputVars) :-
    F = (func(Var, Mode) = Var is semidet :-
        mode_is_output(ModuleInfo, Mode)
    ),
    OutputVars = list.filter_map_corresponding(F, HeadVars, ArgModes).

:- pred rename_outputs(list(prog_var)::in, hlds_goal::in, hlds_goal::out,
    hlds_goal::out, prog_var_renaming::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

rename_outputs(OutputVars, !Goal, UnifyGoal, Renaming, !VarSet, !VarTypes) :-
    GoalInfo0 = get_hlds_goal_info(!.Goal),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
    create_renaming(OutputVars, InstMapDelta, !VarSet, !VarTypes,
        UnifyGoals, _NewVars, Renaming),
    goal_info_init(UnifyGoalInfo0),
    goal_info_set_determinism(detism_det, UnifyGoalInfo0, UnifyGoalInfo),
    conj_list_to_goal(UnifyGoals, UnifyGoalInfo, UnifyGoal),
    rename_some_vars_in_goal(Renaming, !Goal).

:- pred add_promise_equivalent_solutions(list(prog_var)::in,
    hlds_goal::in, hlds_goal::out) is det.

add_promise_equivalent_solutions(OutputVars, Goal0, Goal) :-
    Goal0 = hlds_goal(_, GoalInfo),
    Reason = promise_solutions(OutputVars, equivalent_solutions),
    Goal = hlds_goal(scope(Reason, Goal0), GoalInfo).

%-----------------------------------------------------------------------------%

    % Create the output variable DoRetry.
    %
:- pred make_retry_var(string::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_retry_var(VarName, RetryVar, !VarSet, !VarTypes) :-
    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_retry"), 0),
    construct_type(TypeCtor, [], RetryType),
    varset.new_named_var(VarName, RetryVar, !VarSet),
    add_var_type(RetryVar, RetryType, !VarTypes).

    % Create the goal for recursive call in the case of a retry.
    %
:- pred make_recursive_call(pred_info::in, module_info::in, pred_id::in,
    proc_id::in, list(prog_var)::in, hlds_goal::out) is det.

make_recursive_call(PredInfo, ModuleInfo, PredId, ProcId, HeadVars, Goal) :-
    PredName = pred_info_name(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    SymName = qualified(ModuleName, PredName),
    BuiltIn = builtin_state(ModuleInfo, PredId, PredId, ProcId),
    GoalExpr = plain_call(PredId, ProcId, HeadVars, BuiltIn, no, SymName),

    % We use the goal info of the top level goal in the proc info
    % as this goal is the equivalent of what the recursive call
    % is doing, ie binding the head vars.
    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    proc_info_get_goal(ProcInfo, BodyGoal0),
    GoalInfoHG0 = get_hlds_goal_info(BodyGoal0),

    proc_info_interface_determinism(ProcInfo, Determinism),
    goal_info_set_determinism(Determinism, GoalInfoHG0, GoalInfoHG),

    Goal = hlds_goal(GoalExpr, GoalInfoHG).

    % make_switch_goal(SwitchVar, RecursiveGoal, FailGoal, Goal).
    %
    % Create an output Goal, which is a switch with following pattern :
    %   (
    %       SwitchVar = do_retry,
    %       SwitchCase1
    %   ;
    %       SwitchVar = do_not_retry,
    %       SwitchCase2
    %   )
    %
:- pred make_switch_goal(prog_var::in, hlds_goal::in, hlds_goal::in,
    hlds_goal::out) is det.

make_switch_goal(SwitchVar, DoRetryGoal, DoNotRetryGoal, SwitchGoal) :-
    SSDBModule = mercury_ssdb_builtin_module,
    RetryTypeSymName = qualified(SSDBModule, "ssdb_retry"),
    RetryTypeCtor = type_ctor(RetryTypeSymName, 0),
    ConsIdDoRetry = cons(qualified(SSDBModule, "do_retry"), 0,
        RetryTypeCtor),
    ConsIdDoNotRetry = cons(qualified(SSDBModule, "do_not_retry"), 0,
        RetryTypeCtor),
    CaseDoRetry = case(ConsIdDoRetry, [], DoRetryGoal),
    CaseDoNotRetry = case(ConsIdDoNotRetry, [], DoNotRetryGoal),
    SwitchGoalExpr = switch(SwitchVar, cannot_fail,
        [CaseDoRetry, CaseDoNotRetry]),

    RetryGoalInfo = get_hlds_goal_info(DoRetryGoal),
    NoRetryGoalInfo = get_hlds_goal_info(DoNotRetryGoal),
    RetryDetism = goal_info_get_determinism(RetryGoalInfo),
    NoRetryDetism = goal_info_get_determinism(NoRetryGoalInfo),

    det_switch_detism(RetryDetism, NoRetryDetism, SwitchDetism),

    goal_info_init(GoalInfo0),
    goal_info_set_determinism(SwitchDetism, GoalInfo0, GoalInfo1),
    goal_info_set_purity(purity_impure, GoalInfo1, GoalInfo),

    SwitchGoal = hlds_goal(SwitchGoalExpr, GoalInfo).

    % Update the proc_info and pred_info with the result of the
    % source-to-source transformation.
    %
:- pred commit_goal_changes(list(hlds_goal)::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, prog_varset::in, vartypes::in) is det.

commit_goal_changes(ConjGoals, PredId, ProcId, !.PredInfo, !ProcInfo,
        !ModuleInfo, VarSet, VarTypes) :-
    goal_list_determinism(ConjGoals, ConjDetism),
    ConjGoalInfo = impure_goal_info(ConjDetism),
    Conj = hlds_goal(conj(plain_conj, ConjGoals), ConjGoalInfo),

    proc_info_get_goal(!.ProcInfo, hlds_goal(_, OrigGoalInfo)),
    proc_info_interface_determinism(!.ProcInfo, ProcDetism),
    % This is needed due to the determinism of the recursive call.
    goal_info_set_determinism(ProcDetism, OrigGoalInfo, ScopeGoalInfo),
    Purity = goal_info_get_purity(OrigGoalInfo),
    Goal = hlds_goal(scope(promise_purity(Purity), Conj), ScopeGoalInfo),

    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
    recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
        !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

:- func impure_goal_info(determinism) = hlds_goal_info.

impure_goal_info(Detism) = GoalInfo :-
    goal_info_init(GoalInfo0),
    goal_info_set_purity(purity_impure, GoalInfo0, GoalInfo1),
    goal_info_set_determinism(Detism, GoalInfo1, GoalInfo).

:- func impure_backtrack_goal_info(determinism) = hlds_goal_info.

impure_backtrack_goal_info(Detism) = GoalInfo :-
    GoalInfo0 = impure_goal_info(Detism),
    goal_info_add_feature(feature_preserve_backtrack_into,
        GoalInfo0, GoalInfo).

%-----------------------------------------------------------------------------%

    % Build the following goal : handle_event_EVENT(ProcId, Arguments).
    % EVENT = call,exit,fail or redo
    % Argument = ProcId, ListHeadVars and eventually Retry
    %
:- pred make_handle_event(string::in, list(prog_var)::in, hlds_goal::out,
    module_info::in, module_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_handle_event(HandleTypeString, ArgVars, HandleEventGoal, !ModuleInfo,
        !VarSet, !VarTypes) :-
    SSDBModule = mercury_ssdb_builtin_module,
    Features = [],
    Context = term.context_init,
    generate_simple_call(!.ModuleInfo, SSDBModule, HandleTypeString,
        pf_predicate, only_mode, detism_det, purity_impure, [], ArgVars,
        Features, instmap_delta_bind_no_var, Context, HandleEventGoal).

    % make_proc_id_construction(ModuleInfo, PredInfo, Goals, Var,
    %   !VarSet, !VarTypes)
    %
    % Returns a set of goals, Goals, which build the ssdb_proc_id structure
    % for the given pred and proc infos. The Var returned holds the
    % ssdb_proc_id.
    %
:- pred make_proc_id_construction(module_info::in, pred_info::in,
    hlds_goals::out, prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_proc_id_construction(ModuleInfo, PredInfo, Goals, ProcIdVar,
        !VarSet, !VarTypes) :-
    pred_info_get_origin(PredInfo, Origin),
    ( if
        Origin = origin_transformed(transform_source_to_source_debug, _,
            OrigPredId)
    then
        % This predicate is a proxy for a standard library predicate.
        module_info_pred_info(ModuleInfo, OrigPredId, OrigPredInfo)
    else
        OrigPredInfo = PredInfo
    ),
    SymModuleName = pred_info_module(OrigPredInfo),
    ModuleName = sym_name_to_string(SymModuleName),
    PredName = pred_info_name(OrigPredInfo),

    make_string_const_construction_alloc(ModuleName, yes("ModuleName"),
        ConstructModuleName, ModuleNameVar, !VarSet, !VarTypes),

    make_string_const_construction_alloc(PredName, yes("PredName"),
        ConstructPredName, PredNameVar, !VarSet, !VarTypes),

    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_proc_id"), 0),

    varset.new_named_var("ProcId", ProcIdVar, !VarSet),
    ConsId = cons(qualified(SSDBModule, "ssdb_proc_id"), 2, TypeCtor),
    construct_type(TypeCtor, [], ProcIdType),
    add_var_type(ProcIdVar, ProcIdType, !VarTypes),
    construct_functor(ProcIdVar, ConsId, [ModuleNameVar, PredNameVar],
        ConstructProcIdGoal),

    Goals = [ConstructModuleName, ConstructPredName, ConstructProcIdGoal].

    % Construct the goal which sets the ssdb_tracing_level for
    % the current goal. ie Level = shallow
    %
:- pred make_level_construction(ssdb_trace_level::in(shallow_or_deep),
    hlds_goal::out, prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_level_construction(SSTraceLevel, Goal, LevelVar, !VarSet, !VarTypes) :-
    (
        SSTraceLevel = ssdb_shallow,
        ConsId = shallow_cons_id
    ;
        SSTraceLevel = ssdb_deep,
        ConsId = deep_cons_id
    ),
    make_const_construction_alloc(ConsId, ssdb_tracing_level_type,
        yes("Level"), Goal, LevelVar, !VarSet, !VarTypes).

    % Succeed if all the given argument modes are fully input or fully output.
    % XXX At the moment, we don't handle arguments modes other than
    % fully input or fully output, so if a procedure has such modes,
    % we won't generate code for it.
    %
:- pred all_args_fully_input_or_output(module_info::in, list(mer_mode)::in)
    is semidet.

all_args_fully_input_or_output(ModuleInfo, HeadModes) :-
    all [Mode] (
        list.member(Mode, HeadModes)
    =>
        ( mode_is_fully_input(ModuleInfo, Mode)
        ; mode_is_fully_output(ModuleInfo, Mode)
        )
    ).

%-----------------------------------------------------------------------------%

    % The following code concern predicates which create the list argument at
    % event point.
    %

    % make_arg_list(Pos, InstMap, Vars, RenamedVar, FullListVar, Goals,
    %   !ModuleInfo, !ProcInfo, !PredInfo, !VarSet, !VarTypes, !BoundedVarDesc)
    %
    % Processes each variable in Vars creating a list(var_value) named
    % FullListVar which records the value of each of the variables. Vars points
    % to the start of the list and Goals is the list of goals to construct the
    % list. Pos indicates which argument position the first variable in Vars
    % is.
    % InstMap is used to work out if the variable is instantiated enough yet
    % to display.
    % RenamedVar is a map(X, Y) where Y is the X renamed Var, it is use to
    % replace the output variable at the call of the predicate.
    % BoundedVarDes is a map(X, Y) where Y is the VarDesc of X, it is
    % use while generation to recover the description of already bounded
    % variables.
    %
:- pred make_arg_list(int::in, instmap::in, list(prog_var)::in,
    map(prog_var, prog_var)::in, prog_var::out, list(hlds_goal)::out,
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out,
    map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

make_arg_list(_Pos, _InstMap, [], _Renaming, OutVar, [Goal], !ModuleInfo,
        !ProcInfo, !PredInfo, !VarSet, !VarTypes, !BoundVarDescs) :-
    varset.new_named_var("EmptyVarList", OutVar, !VarSet),
    add_var_type(OutVar, list_var_value_type, !VarTypes),
    ListTypeSymName = qualified(mercury_list_module, "list"),
    ListTypeCtor = type_ctor(ListTypeSymName, 1),
    ConsId = cons(qualified(mercury_list_module, "[]" ), 0, ListTypeCtor),
    construct_functor(OutVar, ConsId, [], Goal).

make_arg_list(Pos0, InstMap, [ProgVar | ProgVars], Renaming, OutVar,
        Goals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet, !VarTypes,
        !BoundVarDescs) :-
    Pos = Pos0 + 1,
    make_arg_list(Pos, InstMap, ProgVars, Renaming, OutVar0, Goals0,
        !ModuleInfo, !ProcInfo, !PredInfo, !VarSet, !VarTypes, !BoundVarDescs),

    lookup_var_type(!.VarTypes, ProgVar, ProgVarType),
    ( if
        ( ProgVarType = io_state_type
        ; ProgVarType = io_io_type
        )
    then
        OutVar = OutVar0,
        Goals = Goals0
    else
        % BoundVarDescs is filled with the description of the input variable
        % during the first call to make_arg_list predicate.
        % At the second call, we search if the current ProgVar already exist
        % in the map and if yes, copy his recorded description.

        ( if map.search(!.BoundVarDescs, ProgVar, ExistingVarDesc) then
            ValueGoals = [],
            VarDesc = ExistingVarDesc
        else
            make_var_value(InstMap, ProgVar, Renaming, VarDesc, Pos0,
                ValueGoals, !ModuleInfo, !ProcInfo, !PredInfo, !VarSet,
                !VarTypes, !BoundVarDescs)
        ),

        varset.new_named_var("FullListVar", OutVar, !VarSet),
        add_var_type(OutVar, list_var_value_type, !VarTypes),
        ListTypeSymName = qualified(mercury_list_module, "list"),
        ListTypeCtor = type_ctor(ListTypeSymName, 1),
        ConsId = cons(qualified(unqualified("list"), "[|]" ), 2, ListTypeCtor),
        construct_functor(OutVar, ConsId, [VarDesc, OutVar0], Goal),

        %XXX Optimize me: repeated appends are slow.
        Goals = Goals0 ++ ValueGoals ++ [Goal]
    ).

    % Return the type list(var_value).
    %
:- func list_var_value_type = mer_type.

list_var_value_type = ListVarValueType :-
    SSDBModule = mercury_ssdb_builtin_module,
    VarValueTypeCtor = type_ctor(qualified(SSDBModule, "var_value"), 0),
    construct_type(VarValueTypeCtor, [], VarValueType),
    ListTypeCtor = type_ctor(qualified(mercury_list_module, "list"), 1),
    construct_type(ListTypeCtor, [VarValueType], ListVarValueType).

    % Create the goal's argument description:
    % -> unbound_head_var(Name, Pos) if it is an unbound argument
    % -> bound_head_var(type_of_T, Name, Position, T) if it is a bound argument
    %
:- pred make_var_value(instmap::in, prog_var::in, map(prog_var, prog_var)::in,
    prog_var::out, int::in, list(hlds_goal)::out,
    module_info::in, module_info::out, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, map(prog_var, prog_var)::in,
    map(prog_var, prog_var)::out) is det.

make_var_value(InstMap, VarToInspect, Renaming, VarDesc, VarPos, Goals,
        !ModuleInfo, !ProcInfo, !PredInfo, !VarSet, !VarTypes,
        !BoundVarDescs) :-
    SSDBModule = mercury_ssdb_builtin_module,
    VarValueTypeCtor = type_ctor(qualified(SSDBModule, "var_value"), 0),
    construct_type(VarValueTypeCtor, [], VarValueType),
    varset.lookup_name(!.VarSet, VarToInspect, VarName),
    make_string_const_construction_alloc(VarName, yes("VarName"),
        ConstructVarName, VarNameVar, !VarSet, !VarTypes),
    make_int_const_construction_alloc(VarPos, yes("VarPos"),
        ConstructVarPos, VarPosVar, !VarSet, !VarTypes),

    varset.new_named_var("VarDesc", VarDesc, !VarSet),
    ( if var_is_ground_in_instmap(!.ModuleInfo, InstMap, VarToInspect) then
        % Update proc_varset and proc_vartypes; without this,
        % polymorphism_make_type_info_var uses a prog_var which is
        % already bound.

        proc_info_set_varset(!.VarSet, !ProcInfo),
        proc_info_set_vartypes(!.VarTypes, !ProcInfo),

        % Create dynamic constructor for the value of the argument.
        %
        % Call polymorphism.m to create the type_infos, add an hidden field
        % which is the polymorphic type of the value.
        %
        % some[T] bound_head_var(string, int, T) ---->
        %   some[T] bound_head_var(type_of_T, string, int, T)

        term.context_init(Context),
        lookup_var_type(!.VarTypes, VarToInspect, MerType),
        polymorphism_make_type_info_var_raw(MerType, Context,
            TypeInfoVar, TypeInfoGoals0, !ModuleInfo, !PredInfo, !ProcInfo),

        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),

        % Constructor of the variable's description.
        ConsId = cons(qualified(SSDBModule, "bound_head_var"), 3,
            VarValueTypeCtor),
        add_var_type(VarDesc, VarValueType, !VarTypes),

        % Renaming contains the names of all instantiated arguments
        % during the execution of the procedure's body.
        ( if map.is_empty(Renaming) then
            construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar,
                VarPosVar, VarToInspect], ConstructVarGoal)
        else
            map.lookup(Renaming, VarToInspect, RenamedVar),
            construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar,
                VarPosVar, RenamedVar], ConstructVarGoal)
        ),

        % The type_info of an existentally typed variable is an output, so
        % could be renamed away. The exit port handler is called before the
        % assignment of the original type_info variable, so we need to use the
        % renamed variable here.
        rename_vars_in_goals(need_not_rename, Renaming, TypeInfoGoals0,
            TypeInfoGoals),

        Goals = [ConstructVarName, ConstructVarPos | TypeInfoGoals] ++
            [ConstructVarGoal],
        map.det_insert(VarToInspect, VarDesc, !BoundVarDescs)
    else
        ConsId = cons(qualified(SSDBModule, "unbound_head_var"), 2,
            VarValueTypeCtor),
        add_var_type(VarDesc, VarValueType, !VarTypes),
        construct_functor(VarDesc, ConsId, [VarNameVar, VarPosVar],
            ConstructVarGoal),

        Goals = [ConstructVarName, ConstructVarPos, ConstructVarGoal]
    ).

%-----------------------------------------------------------------------------%

:- func shallow_cons_id = cons_id.

shallow_cons_id = ssdb_tracing_level_cons_id("shallow").

:- func deep_cons_id = cons_id.

deep_cons_id = ssdb_tracing_level_cons_id("deep").

:- func ssdb_tracing_level_cons_id(string) = cons_id.

ssdb_tracing_level_cons_id(Level) = Cons :-
    DataCtor = qualified(mercury_ssdb_builtin_module, Level),
    Cons = cons(DataCtor, 0, ssdb_tracing_level_type_ctor).

:- func ssdb_tracing_level_type_ctor = type_ctor.

ssdb_tracing_level_type_ctor = type_ctor(ssdb_tracing_level_name, 0).

:- func ssdb_tracing_level_type = mer_type.

ssdb_tracing_level_type = defined_type(ssdb_tracing_level_name, [], kind_star).

:- func ssdb_tracing_level_name = sym_name.

ssdb_tracing_level_name =
    qualified(mercury_ssdb_builtin_module, "ssdb_tracing_level").

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ssdebug.
%-----------------------------------------------------------------------------%
