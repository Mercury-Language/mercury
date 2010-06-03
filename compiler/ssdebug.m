%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2007-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: transform_hlds.ssdebug.m.
% Authors: oannet, wangp.
%
% The ssdebug module does a source to source tranformation on each procedure
% which allows the procedure to be debugged.
%
% The ssdebug transformation is disabled on standard library predicates,
% because it would introduce cyclic dependencies between ssdb.m and the
% standard library.  Disabling the transformation on the standard library is
% also useful for maintaining decent performance.
%
% The tranformation is divided into two passes.
%
% The first pass replaces calls to standard library predicates, and closure
% constructions referring to standard library predicates, by calls to and
% closures over proxy predicates.  The proxy predicates generate events on
% behalf of the standard library predicates.  There will be no events for
% further calls within the standard library, but that is better for
% performance.
%
% The first pass also inserts calls to a context update procedure before every
% procedure call (first or higher order).  This will update global variables
% with the location of the next call site, which will be used by the CALL event
% handler.  Context update calls are not required within proxy predicates.
%
% The second pass performs the main ssdebug transformation, adding calls to
% procedures to handle debugger events.  The transformation depends on the
% determinism of the procedure.
%
% det/cc_multi:
%
%    p(...) :-
%        promise_<original_purity> (
%            CallVarDescs = [ ... ],
%            impure handle_event_call(ProcId, CallVarDescs),
%            <original body>,    % renaming outputs
%            ExitVarDescs = [ ... | CallVarDescs ],
%            impure handle_event_exit(ProcId, ExitVarDescs, DoRetry),
%            (
%                DoRetry = do_retry,
%                p(...)
%            ;
%                DoRetry = do_not_retry,
%                % bind outputs
%            )
%        ).
%
% semidet/cc_nondet:
%
%    p(...) :-
%        promise_<original_purity> (
%            CallVarDescs = [ ... ],
%            (
%                impure handle_event_call(ProcId, CallVarDescs),
%                <original body>    % renaming outputs
%            ->
%                ExitVarDescs = [ ... | CallVarDescs ],
%                impure handle_event_exit(ProcId, ExitVarDescs, DoRetryA),
%                (
%                    DoRetryA = do_retry,
%                    p(...)
%                ;
%                    DoRetryA = do_not_retry,
%                    % bind outputs
%                )
%            ;
%                impure handle_event_fail(ProcId, CallVarDescs, DoRetryB),
%                (
%                    DoRetryB = do_retry,
%                    p(...)
%                ;
%                    DoRetryB = do_not_retry,
%                    fail
%                )
%            )
%        ).
%
% nondet:
%
%    p(...) :-
%        promise_<original_purity> (
%            (
%                CallVarDescs = [ ... ],
%                impure handle_event_call(ProcId, CallVarDescs),
%                <original body>,
%                ExitVarDescs = [ ... | CallVarDescs ],
%                (
%                    impure handle_event_exit(ProcId, ExitVarDescs)
%                    % Go to fail port if retry.
%                ;
%                    % preserve_backtrack_into,
%                    impure handle_event_redo(ProcId, ExitVarDescs),
%                    fail
%                )
%            ;
%                % preserve_backtrack_into
%                impure handle_event_fail(ProcId, CallVarDescs, DoRetryB),
%                (
%                    DoRetryB = do_retry,
%                    p(...)
%                ;
%                    DoRetryB = do_not_retry,
%                    fail
%                )
%            )
%        ).
%
% failure:
%
%   p(...) :-
%       promise_<original_purity> (
%           (
%               CallVarDescs = [ ... ],
%               impure handle_event_call(ProcId, CallVarDescs),
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
%           impure handle_event_call(ProcId, CallVarDescs),
%           <original body>
%       ).
%
% where CallVarDescs, ExitVarDescs are lists of var_value
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
% Output head variables may appear twice in a variable description list --
% initially unbound, then overridden by a bound_head_var functor.  Then the
% ExitVarDescs can add output variable bindings to the CallVarDescs list,
% instead of building new lists.  The pos fields give the argument numbers
% of head variables.
%
% The ProcId is of type ssdb.ssdb_proc_id.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ssdebug.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

:- pred ssdebug.transform_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.purity.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

ssdebug.transform_module(!ModuleInfo, !IO) :-
    ssdebug.first_pass(!ModuleInfo),
    process_all_nonimported_procs(update_module(ssdebug.process_proc),
        !ModuleInfo, !IO).

%-----------------------------------------------------------------------------%
%
% Create proxies for standard library predicates and insert context updates
%

:- type proxy_map == map(pred_id, maybe(pred_id)).

:- pred first_pass(module_info::in, module_info::out) is det.

first_pass(!ModuleInfo) :-
    module_info_predids(PredIds, !ModuleInfo),
    list.foldl2(first_pass_in_pred, PredIds, map.init, _ProxyMap, !ModuleInfo).

:- pred first_pass_in_pred(pred_id::in, proxy_map::in, proxy_map::out,
    module_info::in, module_info::out) is det.

first_pass_in_pred(PredId, !ProxyMap, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_non_imported_procids(PredInfo),
    list.foldl2(first_pass_in_proc(PredId), ProcIds, !ProxyMap, !ModuleInfo).

:- pred first_pass_in_proc(pred_id::in, proc_id::in,
    proxy_map::in, proxy_map::out, module_info::in, module_info::out) is det.

first_pass_in_proc(PredId, ProcId, !ProxyMap, !ModuleInfo) :-
    some [!ProcInfo] (
        module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId, PredInfo,
            !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, Goal0),
        first_pass_in_goal(Goal0, Goal, !ProcInfo, !ProxyMap, !ModuleInfo),
        proc_info_set_goal(Goal, !ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, PredInfo, !.ProcInfo,
            !ModuleInfo)
    ).

:- pred first_pass_in_goal(hlds_goal::in, hlds_goal::out,
    proc_info::in, proc_info::out, proxy_map::in, proxy_map::out,
    module_info::in, module_info::out) is det.

first_pass_in_goal(!Goal, !ProcInfo, !ProxyMap, !ModuleInfo) :-
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_, _, _, Unification0, _),
        (
            Unification0 = construct(_, ConsId0, _, _, _, _, _),
            ConsId0 = closure_cons(ShroudedPredProcId, lambda_normal)
        ->
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
        ;
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
        ;
            Builtin = out_of_line_builtin
        )
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        insert_context_update_call(!.ModuleInfo, !Goal, !ProcInfo)
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl3(first_pass_in_goal, Goals0, Goals, !ProcInfo,
            !ProxyMap, !ModuleInfo),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = disj(Goals0),
        list.map_foldl3(first_pass_in_goal, Goals0, Goals, !ProcInfo,
            !ProxyMap, !ModuleInfo),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        list.map_foldl3(first_pass_in_case, Cases0, Cases, !ProcInfo,
            !ProxyMap, !ModuleInfo),
        GoalExpr = switch(Var, CanFail, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = negation(SubGoal0),
        first_pass_in_goal(SubGoal0, SubGoal, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        GoalExpr = negation(SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        first_pass_in_goal(SubGoal0, SubGoal, !ProcInfo, !ProxyMap,
            !ModuleInfo),
        GoalExpr = scope(Reason, SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        first_pass_in_goal(Cond0, Cond, !ProcInfo, !ProxyMap, !ModuleInfo),
        first_pass_in_goal(Then0, Then, !ProcInfo, !ProxyMap, !ModuleInfo),
        first_pass_in_goal(Else0, Else, !ProcInfo, !ProxyMap, !ModuleInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "create_proxies_in_goal: unexpected shorthand")
    ).

:- pred first_pass_in_case(case::in, case::out,
    proc_info::in, proc_info::out,
    proxy_map::in, proxy_map::out, module_info::in, module_info::out) is det.

first_pass_in_case(Case0, Case, !ProcInfo, !ProxyMap, !ModuleInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    first_pass_in_goal(Goal0, Goal, !ProcInfo, !ProxyMap, !ModuleInfo),
    Case = case(MainConsId, OtherConsIds, Goal).

    % Look up the proxy for a predicate, creating one if appropriate.
    %
:- pred lookup_proxy_pred(pred_id::in, maybe(pred_id)::out,
    proxy_map::in, proxy_map::out, module_info::in, module_info::out) is det.

lookup_proxy_pred(PredId, MaybeNewPredId, !ProxyMap, !ModuleInfo) :-
    ( map.search(!.ProxyMap, PredId, MaybeNewPredId0) ->
        MaybeNewPredId = MaybeNewPredId0
    ;
        module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        ( mercury_std_library_module_name(PredModule) ->
            create_proxy_pred(PredId, NewPredId, !ModuleInfo),
            MaybeNewPredId = yes(NewPredId)
        ;
            MaybeNewPredId = no
        ),
        svmap.det_insert(PredId, MaybeNewPredId, !ProxyMap)
    ).

:- pred create_proxy_pred(pred_id::in, pred_id::out,
    module_info::in, module_info::out) is det.

create_proxy_pred(PredId, NewPredId, !ModuleInfo) :-
    some [!PredInfo] (
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        pred_info_set_import_status(status_local, !PredInfo),

        ProcIds = pred_info_procids(!.PredInfo),
        list.foldl2(create_proxy_proc(PredId), ProcIds, !PredInfo, !ModuleInfo),

        % Change the name so that the proxy is not confused with the original.
        Name = pred_info_name(!.PredInfo),
        pred_info_set_name("SSDBPR__" ++ Name, !PredInfo),

        % Set the predicate origin so that the later pass can find the name of
        % the original predicate.
        pred_info_get_origin(!.PredInfo, Origin),
        NewOrigin = origin_transformed(transform_source_to_source_debug,
            Origin, PredId),
        pred_info_set_origin(NewOrigin, !PredInfo),

        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_insert(!.PredInfo, NewPredId, PredTable0, PredTable),
        module_info_set_predicate_table(PredTable, !ModuleInfo)
    ).

:- pred create_proxy_proc(pred_id::in, proc_id::in,
    pred_info::in, pred_info::out, module_info::in, module_info::out) is det.

create_proxy_proc(PredId, ProcId, !PredInfo, !ModuleInfo) :-
    some [!ProcInfo] (
        % The proxy just has to call the original procedure.
        pred_info_proc_info(!.PredInfo, ProcId, !:ProcInfo),
        proc_info_get_goal(!.ProcInfo, hlds_goal(_, GoalInfo)),
        proc_info_get_headvars(!.ProcInfo, Args),
        pred_info_get_sym_name(!.PredInfo, SymName),
        CallExpr = plain_call(PredId, ProcId, Args, not_builtin, no, SymName),
        proc_info_set_goal(hlds_goal(CallExpr, GoalInfo), !ProcInfo),
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

    Args = [FileNameVar, LineNumberVar],
    Features = [],
    instmap_delta_init_reachable(InstMapDelta),
    generate_simple_call(mercury_ssdb_builtin_module, "set_context",
        pf_predicate, only_mode, detism_det, purity_impure, Args, Features,
        InstMapDelta, ModuleInfo, Context, SetContextGoal),

    conj_list_to_goal([MakeFileName, MakeLineNumber, SetContextGoal, Goal0],
        GoalInfo, Goal).

%-----------------------------------------------------------------------------%
%
% The main transformation
%

:- pred process_proc(pred_id::in, proc_id::in, pred_info::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

process_proc(PredId, ProcId, _PredInfo, !ProcInfo, !ModuleInfo) :-
    % We have different transformations for procedures of different
    % determinisms.

    % XXX The definitions of the four process_proc_* predicates are very
    % similar; they look to have generated using cut-and-paste.
    % The common parts should be factored out and moved here.

    proc_info_get_inferred_determinism(!.ProcInfo, Determinism),
    (
        ( Determinism = detism_det
        ; Determinism = detism_cc_multi
        ),
        process_proc_det(PredId, ProcId, !ProcInfo, !ModuleInfo)
    ;
        ( Determinism = detism_semi
        ; Determinism = detism_cc_non
        ),
        process_proc_semi(PredId, ProcId, !ProcInfo, !ModuleInfo)
    ;
        ( Determinism = detism_multi
        ; Determinism = detism_non
        ),
        process_proc_nondet(PredId, ProcId, !ProcInfo, !ModuleInfo)
    ;
        Determinism = detism_erroneous,
        process_proc_erroneous(PredId, ProcId, !ProcInfo, !ModuleInfo)
    ;
        Determinism = detism_failure,
        process_proc_failure(PredId, ProcId, !ProcInfo, !ModuleInfo)
    ).

    % Source-to-source transformation for a deterministic goal.
    %
:- pred process_proc_det(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

process_proc_det(PredId, ProcId, !ProcInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    BodyGoalInfo0 = get_hlds_goal_info(BodyGoal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Make the ssdb_proc_id.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !Varset, !Vartypes),

        % Get the list of head variables and their instantiation state.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),
        proc_info_get_argmodes(!.ProcInfo, ListMerMode),

        ( check_arguments_modes(!.ModuleInfo, ListMerMode) ->
            % Make a list which records the value for each of the head
            % variables at the call port.
            make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
                CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, map.init, BoundVarDescsAtCall),

            % Generate the call to handle_event_call(ProcId, VarList).
            make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
                HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

            % Get the InstMap at the end of the procedure.
            update_instmap(BodyGoal0, InitInstMap, FinalInstMap),

            % In the case of a retry, the output variables will be bound by the
            % retried call.
            proc_info_instantiated_head_vars(!.ModuleInfo, !.ProcInfo,
                InstantiatedVars),
            goal_info_get_instmap_delta(BodyGoalInfo0) = InstMapDelta,
            create_renaming(InstantiatedVars, InstMapDelta, !Varset, !Vartypes,
                RenamingGoals, _NewVars, Renaming),
            rename_some_vars_in_goal(Renaming, BodyGoal0, BodyGoal1),

            % Make the variable list at the exit port. It's currently a
            % completely new list instead of adding on to the list generated
            % for the call port.
            make_arg_list(0, FinalInstMap, HeadVars, Renaming, ExitArgListVar,
                ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

            % Create DoRetry output variable.
            make_retry_var("DoRetry", RetryVar, !Varset, !Vartypes),

            % Generate the call to handle_event_exit(ProcId, VarList, DoRetry).
            make_handle_event("handle_event_exit",
                [ProcIdVar, ExitArgListVar, RetryVar], HandleEventExitGoal,
                !ModuleInfo, !Varset, !Vartypes),

            % Generate the recursive call in the case of a retry.
            make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
                HeadVars, RecursiveGoal),

            % Organize the order of the generated code.
            goal_to_conj_list(BodyGoal1, BodyGoalList),
            % Set the determinism.
            Determinism = detism_det,
            goal_info_init(GoalInfo0),
            goal_info_set_determinism(Determinism, GoalInfo0, GoalInfoDet),
            goal_info_set_purity(purity_impure, GoalInfoDet,
                GoalInfoImpureDet),

            conj_list_to_goal(RenamingGoals, GoalInfoImpureDet, RenamingGoal),
            % Create the switch on Retry at exit port.
            make_switch_goal(RetryVar, RecursiveGoal, RenamingGoal,
                GoalInfoImpureDet, SwitchGoal),

            ConjGoals = ProcIdGoals ++ CallArgListGoals ++
                [HandleEventCallGoal | BodyGoalList] ++
                ExitArgListGoals ++ [HandleEventExitGoal, SwitchGoal],

            conj_list_to_goal(ConjGoals, GoalInfoImpureDet, GoalWithoutPurity),

            % Add the purity scope.
            Purity = goal_info_get_purity(BodyGoalInfo0),
            wrap_with_purity_scope(Purity, GoalInfoDet, GoalWithoutPurity,
                Goal),

            commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
                !ModuleInfo, !.Varset, !.Vartypes)
        ;
            % In the case of a mode which is not fully input or output,
            % the procedure is not transformed.
            true
        )
    ).

    % Source-to-source transformation for a semidet goal.
    %
:- pred process_proc_semi(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

process_proc_semi(PredId, ProcId, !ProcInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    get_hlds_goal_info(BodyGoal0) = BodyGoalInfo0,

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Get the list of head variables and their initial instantiations.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),
        proc_info_get_argmodes(!.ProcInfo, ListMerMode),

        ( check_arguments_modes(!.ModuleInfo, ListMerMode) ->

            % Make the ssdb_proc_id.
            module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
            make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
                ProcIdVar, !Varset, !Vartypes),

            % Make a list which records the value for each of the head
            % variables at the call port.
            make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
                CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, map.init, BoundVarDescsAtCall),

            % Generate the call to handle_event_call(ProcId, VarList).
            make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
                HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

            % Get the InstMap at the end of the procedure.
            update_instmap(BodyGoal0, InitInstMap, FinalInstMap),

            % In the case of a retry, the output variables will be bound by the
            % retried call.
            proc_info_instantiated_head_vars(!.ModuleInfo, !.ProcInfo,
                InstantiatedVars),
            goal_info_get_instmap_delta(BodyGoalInfo0) = InstMapDelta,
            create_renaming(InstantiatedVars, InstMapDelta, !Varset, !Vartypes,
                RenamingGoals, _NewVars, Renaming),
            rename_some_vars_in_goal(Renaming, BodyGoal0, BodyGoal1),

            % Make the variable list at the exit port. It's currently a
            % completely new list instead of adding on to the list generated
            % for the call port.
            make_arg_list(0, FinalInstMap, HeadVars, Renaming, ExitArgListVar,
                ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

            % Create DoRetryA output variable
            make_retry_var("DoRetryA", RetryAVar, !Varset, !Vartypes),

            % Generate the call to
            %   handle_event_exit(ProcId, VarList, DoRetryA).
            make_handle_event("handle_event_exit",
                [ProcIdVar, ExitArgListVar, RetryAVar], HandleEventExitGoal,
                !ModuleInfo, !Varset, !Vartypes),

            % Generate the recursive call in the case of a retry
            make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
                HeadVars, RecursiveGoal),

            % Generate the list of arguments at the fail port.
            make_arg_list(0, InitInstMap, [], Renaming, FailArgListVar,
                FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

            % Create DoRetryB output variable
            make_retry_var("DoRetryB", RetryBVar, !Varset, !Vartypes),

            % Generate the call to
            %   handle_event_fail(ProcId, VarList, DoRetryB).
            make_handle_event("handle_event_fail",
                [ProcIdVar, FailArgListVar, RetryBVar], HandleEventFailGoal,
                !ModuleInfo, !Varset, !Vartypes),

            make_fail_call(FailGoal, !.ModuleInfo),

            % Organize the order of the generated code.

            % Get a flattened goal to avoid nested conjuction.
            goal_to_conj_list(BodyGoal1, BodyGoalList),
            GoalsCond = BodyGoalList,

            % Create the switch on DoRetryA at exit port.
            goal_info_init(GoalInfo0),
            goal_info_set_purity(purity_impure, GoalInfo0, GoalInfoImpure),
            goal_list_purity(GoalsCond, PurityCond),
            goal_list_determinism(GoalsCond, DetismCond),
            goal_info_set_determinism(DetismCond, GoalInfo0,
                GoalInfoCondDet),
            goal_info_set_purity(PurityCond, GoalInfoCondDet,
                GoalInfoCondPurDet),

            SemiDet = detism_semi,
            goal_info_set_determinism(SemiDet, GoalInfo0, GoalInfoSemiDet),
            goal_info_set_purity(purity_impure, GoalInfoSemiDet,
                GoalInfoImpureSemiDet),
            goal_info_set_determinism(detism_det, GoalInfoImpure,
                GoalInfoImpureDet),
            conj_list_to_goal(RenamingGoals, GoalInfoImpureDet, RenamingGoal),

            % Create the switch on DoRetryA at exit port.
            make_switch_goal(RetryAVar, RecursiveGoal, RenamingGoal,
                GoalInfoImpureSemiDet, SwitchExitPortGoal),
            % Create the switch on DoRetryB at fail port.
            make_switch_goal(RetryBVar, RecursiveGoal, FailGoal,
                GoalInfoImpureSemiDet, SwitchFailPortGoal),

            GoalsThen   = ExitArgListGoals ++
                [HandleEventExitGoal, SwitchExitPortGoal],
            GoalsElse   = FailArgListGoals ++
                [HandleEventFailGoal, SwitchFailPortGoal],

            goal_info_set_determinism(detism_semi, GoalInfoImpure,
                GoalInfoThen),
            goal_info_set_determinism(detism_semi, GoalInfoImpure,
                GoalInfoElse),

            IteExistVars = [],
            conj_list_to_goal(GoalsCond, GoalInfoCondPurDet, CondGoal),
            ThenGoal = hlds_goal(conj(plain_conj, GoalsThen), GoalInfoThen),
            ElseGoal = hlds_goal(conj(plain_conj, GoalsElse), GoalInfoElse),

            CallVarGoal = ProcIdGoals ++ CallArgListGoals ++
                [HandleEventCallGoal],
            % XXX not sure about determinism in an if-then-else.
            GoalITE = hlds_goal(if_then_else(IteExistVars, CondGoal, ThenGoal,
                ElseGoal), GoalInfoCondPurDet),

            ConjGoal = CallVarGoal ++ [GoalITE],
            GoalWithoutPurity = hlds_goal(conj(plain_conj, ConjGoal),
                GoalInfoCondPurDet),

            % Add the purity scope.
            Purity = goal_info_get_purity(BodyGoalInfo0),
            wrap_with_purity_scope(Purity, GoalInfoSemiDet, GoalWithoutPurity,
                Goal),

            commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
                !ModuleInfo, !.Varset, !.Vartypes)
        ;
            % In the case of a mode which is not fully input or output,
            % the procedure is not transformed.
            true
        )
    ).

    % Source-to-source transformation for a nondeterministic procedure.
    %
:- pred process_proc_nondet(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

process_proc_nondet(PredId, ProcId, !ProcInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    get_hlds_goal_info(BodyGoal0) = BodyGoalInfo0,

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),

        % Make the ssdb_proc_id.
        module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
        make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
            ProcIdVar, !Varset, !Vartypes),

        % Get the list of head variables and their instantiation state.
        proc_info_get_headvars(!.ProcInfo, HeadVars),
        proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InitInstMap),
        proc_info_get_argmodes(!.ProcInfo, ListMerMode),

        ( check_arguments_modes(!.ModuleInfo, ListMerMode) ->

            % Make a list which records the value for each of the head
            % variables at the call port.
            make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
                CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, map.init, BoundVarDescsAtCall),

            % Generate the call to handle_event_call(ProcId, VarList).
            make_handle_event("handle_event_call_nondet",
                [ProcIdVar, CallArgListVar],
                HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

            % Get the InstMap at the end of the procedure.
            update_instmap(BodyGoal0, InitInstMap, FinalInstMap),

            % Make the variable list at the exit port. It's currently a
            % completely new list instead of adding on to the list generated
            % for the call port.
            make_arg_list(0, FinalInstMap, HeadVars, map.init, ExitArgListVar,
                ExitArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtExit),

            % Generate the call to handle_event_exit_nondet(ProcId, VarList).
            make_handle_event("handle_event_exit_nondet",
                [ProcIdVar, ExitArgListVar],
                HandleEventExitGoal, !ModuleInfo, !Varset, !Vartypes),

            % Generate the call to handle_event_redo(ProcId, VarList).
            make_handle_event("handle_event_redo_nondet",
                [ProcIdVar, ExitArgListVar],
                HandleEventRedoGoal, !ModuleInfo, !Varset, !Vartypes),

            % Generate the list of argument at the fail port.
            make_arg_list(0, InitInstMap, [], map.init, FailArgListVar,
                FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

            % Create DoRetry output variable
            make_retry_var("DoRetry", RetryVar, !Varset, !Vartypes),

            % Generate the call to
            %   handle_event_fail_nondet(ProcId, VarList, DoRetry).
            make_handle_event("handle_event_fail_nondet",
                [ProcIdVar, FailArgListVar, RetryVar],
                HandleEventFailGoal, !ModuleInfo, !Varset, !Vartypes),

            make_fail_call(FailGoal, !.ModuleInfo),

            % Organize the order of the generated code.
            % Get a flattened goal to avoid nested conjuction.
            goal_to_conj_list(BodyGoal0, BodyGoalList0),
            CallVarGoal0 = CallArgListGoals ++
                [HandleEventCallGoal | BodyGoalList0] ++ ExitArgListGoals,
            goal_info_init(GoalInfo0),
            conj_list_to_goal(CallVarGoal0, GoalInfo0, CallVarGoal1),
            goal_to_conj_list(CallVarGoal1, CallVarGoal),

            % Generate the recursive call in the case of a retry
            make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
                HeadVars, RecursiveGoal),

            Det = detism_det,
            FailDet = detism_failure,
            NonDet = detism_non,
            goal_info_set_purity(purity_impure, GoalInfo0, GoalInfoImpure),
            goal_info_set_determinism(Det, GoalInfoImpure, GoalInfoImpureDet),
            goal_info_set_determinism(FailDet, GoalInfoImpure,
                GoalInfoImpureFailDet),
            goal_info_set_determinism(NonDet, GoalInfoImpure,
                GoalInfoImpureNonDet),
            goal_list_determinism(BodyGoalList0, Detism),
            goal_info_set_determinism(Detism, GoalInfo0, GoalInfoDetism),
            goal_info_set_determinism(Detism, GoalInfoImpure,
                GoalInfoImpureDetism),

            % Create the switch on DoRetry at fail port.
            make_switch_goal(RetryVar, RecursiveGoal, FailGoal,
                GoalInfoImpureNonDet, SwitchFailPortGoal),

            ConjGoal11 = hlds_goal(conj(plain_conj,
                [HandleEventExitGoal]), GoalInfoImpureDet),
            ConjGoal120 = hlds_goal(conj(plain_conj,
                [HandleEventRedoGoal, FailGoal]), GoalInfoImpureFailDet),
            goal_add_feature(feature_preserve_backtrack_into, ConjGoal120,
                ConjGoal12),

            DisjGoal1 = hlds_goal(disj([ConjGoal11, ConjGoal12]),
                GoalInfoImpureDetism),

            ConjGoal21 = hlds_goal(conj(plain_conj,
                CallVarGoal ++ [DisjGoal1]), GoalInfoImpureDetism),
            ConjGoal220 = hlds_goal(conj(plain_conj, FailArgListGoals ++
                [HandleEventFailGoal, SwitchFailPortGoal]),
                GoalInfoImpureNonDet),
            goal_add_feature(feature_preserve_backtrack_into, ConjGoal220,
                ConjGoal22),
            DisjGoal2 = hlds_goal(disj([ConjGoal21, ConjGoal22]),
                GoalInfoImpureDetism),

            GoalWithoutPurity = hlds_goal(conj(plain_conj,
                ProcIdGoals ++ [DisjGoal2]), GoalInfoImpureDetism),

            % Add the purity scope.
            Purity = goal_info_get_purity(BodyGoalInfo0),
            wrap_with_purity_scope(Purity, GoalInfoDetism, GoalWithoutPurity,
                Goal),

            commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
                !ModuleInfo, !.Varset, !.Vartypes)
        ;
            true
        )
    ).

      % Source-to-source transformation for a failure procedure.
      %
:- pred process_proc_failure(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

process_proc_failure(PredId, ProcId, !ProcInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    BodyGoalInfo0 = get_hlds_goal_info(BodyGoal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),
        proc_info_get_argmodes(!.ProcInfo, ListMerMode),

        ( check_arguments_modes(!.ModuleInfo, ListMerMode) ->
            % Make the ssdb_proc_id.
            module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
            make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
                ProcIdVar, !Varset, !Vartypes),

            % Get the list of head variables and their instantiation state.
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo,
                InitInstMap),

            % Make a list which records the value for each of the head
            % variables at the call port.
            make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
                CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, map.init, BoundVarDescsAtCall),

            % Generate the call to handle_event_call(ProcId, VarList).
            make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
                HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

            % Make the variable list at the exit port. It's currently a
            % completely new list instead of adding on to the list generated
            % for the call port.
            make_arg_list(0, InitInstMap, [], map.init, FailArgListVar,
                FailArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, BoundVarDescsAtCall, _BoundVarDescsAtFail),

            % Create DoRetry output variable.
            make_retry_var("DoRetry", RetryVar, !Varset, !Vartypes),

            % Generate the call to handle_event_exit(ProcId, VarList, DoRetry).
            make_handle_event("handle_event_fail",
                [ProcIdVar, FailArgListVar, RetryVar],
                HandleEventFailGoal, !ModuleInfo, !Varset, !Vartypes),

            make_fail_call(FailGoal, !.ModuleInfo),

            % Generate the recursive call in the case of a retry.
            make_recursive_call(!.PredInfo, !.ModuleInfo, PredId, ProcId,
                HeadVars, RecursiveGoal),

            % Organize the order of the generated code.

            goal_to_conj_list(BodyGoal0, BodyGoalList),
            % Set the determinism.
            Determinism = detism_failure,
            goal_info_init(GoalInfo0),
            goal_info_set_determinism(Determinism, GoalInfo0, GoalInfoFail),
            goal_info_set_purity(purity_impure, GoalInfoFail,
                GoalInfoImpureFail),

            % Create the switch on Retry at fail port.
            make_switch_goal(RetryVar, RecursiveGoal, FailGoal,
                GoalInfoImpureFail, SwitchGoal),

            ConjGoal1 = hlds_goal(conj(plain_conj, BodyGoalList),
                GoalInfoImpureFail),
            ConjGoal20 = hlds_goal(conj(plain_conj, FailArgListGoals ++
                [HandleEventFailGoal, SwitchGoal]), GoalInfoImpureFail),
            goal_add_feature(feature_preserve_backtrack_into, ConjGoal20,
                ConjGoal2),

            DisjGoal = hlds_goal(disj([ConjGoal1, ConjGoal2]),
                GoalInfoImpureFail),

            ConjGoals = ProcIdGoals ++ CallArgListGoals ++
                [HandleEventCallGoal, DisjGoal],

            conj_list_to_goal(ConjGoals, GoalInfoImpureFail,
                GoalWithoutPurity),

            % Add the purity scope.
            Purity = goal_info_get_purity(BodyGoalInfo0),
            wrap_with_purity_scope(Purity, GoalInfoFail, GoalWithoutPurity,
                Goal),

            commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
                !ModuleInfo, !.Varset, !.Vartypes)
        ;
            true
        )
    ).

      % Source-to-source transformation for an erroneous procedure.
      % XXX ERRONEOUS procedure have currently just a call port.
      %
:- pred process_proc_erroneous(pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

process_proc_erroneous(PredId, ProcId, !ProcInfo, !ModuleInfo) :-
    proc_info_get_goal(!.ProcInfo, BodyGoal0),
    BodyGoalInfo0 = get_hlds_goal_info(BodyGoal0),

    some [!PredInfo, !Varset, !Vartypes] (
        proc_info_get_varset(!.ProcInfo, !:Varset),
        proc_info_get_vartypes(!.ProcInfo, !:Vartypes),
        proc_info_get_argmodes(!.ProcInfo, ListMerMode),

        ( check_arguments_modes(!.ModuleInfo, ListMerMode) ->

            % Make the ssdb_proc_id.
            module_info_pred_info(!.ModuleInfo, PredId, !:PredInfo),
            make_proc_id_construction(!.ModuleInfo, !.PredInfo, ProcIdGoals,
                ProcIdVar, !Varset, !Vartypes),

            % Get the list of head variables and their instantiation state.
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo,
                InitInstMap),

            % Make a list which records the value for each of the head
            % variables at the call port.
            make_arg_list(0, InitInstMap, HeadVars, map.init, CallArgListVar,
                CallArgListGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, map.init, _BoundVarDescsAtCall),

            % Generate the call to handle_event_call(ProcId, VarList).
            make_handle_event("handle_event_call", [ProcIdVar, CallArgListVar],
                HandleEventCallGoal, !ModuleInfo, !Varset, !Vartypes),

            % Organize the order of the generated code.
            goal_to_conj_list(BodyGoal0, BodyGoalList),
            % Set the determinism.
            DeterminismErr = detism_erroneous,
            goal_info_init(GoalInfo0),
            goal_info_set_determinism(DeterminismErr, GoalInfo0,
                GoalInfoErr),
            goal_info_set_purity(purity_impure, GoalInfoErr,
                GoalInfoImpureErr),

            ConjGoals = ProcIdGoals ++ CallArgListGoals ++
                [HandleEventCallGoal | BodyGoalList],

            conj_list_to_goal(ConjGoals, GoalInfoImpureErr, GoalWithoutPurity),

            % Add the purity scope.
            Purity = goal_info_get_purity(BodyGoalInfo0),
            wrap_with_purity_scope(Purity, GoalInfoErr, GoalWithoutPurity,
                Goal),

            commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo,
                !ModuleInfo, !.Varset, !.Vartypes)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Create the output variable DoRetry.
    %
:- pred make_retry_var(string::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

make_retry_var(VarName, RetryVar, !VarSet, !VarTypes) :-
    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_retry"), 0),
    construct_type(TypeCtor, [], RetryType),
    svvarset.new_named_var(VarName, RetryVar, !VarSet),
    svmap.det_insert(RetryVar, RetryType, !VarTypes).

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
    GoalInfoHG = get_hlds_goal_info(BodyGoal0),

    Goal = hlds_goal(GoalExpr, GoalInfoHG).

    % make_switch_goal(SwitchVar, SwitchCase1, SwitchCase2, GoalInfo, Goal).
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
    hlds_goal_info::in, hlds_goal::out) is det.

make_switch_goal(SwitchVar, DoRetryGoal, DoNotRetryGoal, GoalInfo,
        SwitchGoal) :-
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
    SwitchGoal = hlds_goal(SwitchGoalExpr, GoalInfo).

    % wrap_with_purity_scope(Purity, GoalInfo, Goal0, Goal):
    %
    % The Goal0 is wrap with the Purity to give Goal.
    %
:- pred wrap_with_purity_scope(purity::in, hlds_goal_info::in, hlds_goal::in,
    hlds_goal::out) is det.

wrap_with_purity_scope(Purity, GoalInfo0, GoalWithoutPurity, Goal) :-
    goal_info_set_purity(Purity, GoalInfo0, GoalInfo),
    ScopeReason = promise_purity(Purity),
    Goal = hlds_goal(scope(ScopeReason, GoalWithoutPurity), GoalInfo).

    % Update the proc_info and pred_info with the result of the
    % source-to-source transformation.
    %
:- pred commit_goal_changes(hlds_goal::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, prog_varset::in, vartypes::in) is det.

commit_goal_changes(Goal, PredId, ProcId, !.PredInfo, !ProcInfo, !ModuleInfo,
        Varset, Vartypes) :-
    proc_info_set_varset(Varset, !ProcInfo),
    proc_info_set_vartypes(Vartypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    requantify_proc_general(ordinary_nonlocals_no_lambda, !ProcInfo),
    recompute_instmap_delta_proc(recompute_atomic_instmap_deltas,
        !ProcInfo, !ModuleInfo),
    pred_info_set_proc_info(ProcId, !.ProcInfo, !PredInfo),
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Build the following goal : handle_event_EVENT(ProcId, Arguments).
    % EVENT = call,exit,fail or redo
    % Argument = ProcId, ListHeadVars and eventually Retry
    %
:- pred make_handle_event(string::in, list(prog_var)::in, hlds_goal::out,
    module_info::in, module_info::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_handle_event(HandleTypeString, Arguments, HandleEventGoal, !ModuleInfo,
        !Varset, !Vartypes) :-
    SSDBModule = mercury_ssdb_builtin_module,
    Features = [],
    Context = term.context_init,
    goal_util.generate_simple_call(SSDBModule, HandleTypeString,
        pf_predicate, only_mode, detism_det, purity_impure, Arguments,
        Features, instmap_delta_bind_no_var, !.ModuleInfo, Context,
        HandleEventGoal).

    % make_proc_id_construction(ModuleInfo, PredInfo, Goals, Var,
    %   !Varset, !Vartypes)
    %
    % Returns a set of goals, Goals, which build the ssdb_proc_id structure
    % for the given pred and proc infos.  The Var returned holds the
    % ssdb_proc_id.
    %
:- pred make_proc_id_construction(module_info::in, pred_info::in,
    hlds_goals::out, prog_var::out, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out) is det.

make_proc_id_construction(ModuleInfo, PredInfo, Goals, ProcIdVar,
        !Varset, !Vartypes) :-
    pred_info_get_origin(PredInfo, Origin),
    (
        Origin = origin_transformed(transform_source_to_source_debug, _,
            OrigPredId)
    ->
        % This predicate is a proxy for a standard library predicate.
        module_info_pred_info(ModuleInfo, OrigPredId, OrigPredInfo)
    ;
        OrigPredInfo = PredInfo
    ),
    SymModuleName = pred_info_module(OrigPredInfo),
    ModuleName = sym_name_to_string(SymModuleName),
    PredName = pred_info_name(OrigPredInfo),

    make_string_const_construction_alloc(ModuleName, yes("ModuleName"),
        ConstructModuleName, ModuleNameVar, !Varset, !Vartypes),

    make_string_const_construction_alloc(PredName, yes("PredName"),
        ConstructPredName, PredNameVar, !Varset, !Vartypes),

    SSDBModule = mercury_ssdb_builtin_module,
    TypeCtor = type_ctor(qualified(SSDBModule, "ssdb_proc_id"), 0),

    svvarset.new_named_var("ProcId", ProcIdVar, !Varset),
    ConsId = cons(qualified(SSDBModule, "ssdb_proc_id"), 2, TypeCtor),
    construct_type(TypeCtor, [], ProcIdType),
    svmap.det_insert(ProcIdVar, ProcIdType, !Vartypes),
    construct_functor(ProcIdVar, ConsId, [ModuleNameVar, PredNameVar],
        ConstructProcIdGoal),

    Goals = [ConstructModuleName, ConstructPredName, ConstructProcIdGoal].

    % make_fail_call(FailGoal, ModuleInfo)
    %
    % Construct the fail goal.
    %
:- pred make_fail_call(hlds_goal::out, module_info::in) is det.

make_fail_call(FailGoal, ModuleInfo) :-
    Features = [],
    Context = term.context_init,
    goal_util.generate_simple_call(mercury_public_builtin_module,
        "false", pf_predicate, only_mode, detism_failure, purity_pure, [],
        Features, instmap_delta_bind_no_var, ModuleInfo, Context, FailGoal).

    % Detect if all argument's mode are fully input or output.
    % XXX Other mode than fully input or output are not handled for the
    % moment. So the code of these procedures will not be generated.
    %
:- pred check_arguments_modes(module_info::in, list(mer_mode)::in)
    is semidet.

check_arguments_modes(ModuleInfo, HeadModes) :-
    all [Modes] (
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
    %   !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes, !BoundedVarDesc)
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
        !ProcInfo, !PredInfo, !Varset, !Vartypes, !BoundVarDescs) :-
    svvarset.new_named_var("EmptyVarList", OutVar, !Varset),
    svmap.det_insert(OutVar, list_var_value_type, !Vartypes),
    ListTypeSymName = qualified(mercury_list_module, "list"),
    ListTypeCtor = type_ctor(ListTypeSymName, 1),
    ConsId = cons(qualified(mercury_list_module, "[]" ), 0, ListTypeCtor),
    construct_functor(OutVar, ConsId, [], Goal).

make_arg_list(Pos0, InstMap, [ProgVar | ProgVars], Renaming, OutVar,
        Goals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes,
        !BoundVarDescs) :-
    Pos = Pos0 + 1,
    make_arg_list(Pos, InstMap, ProgVars, Renaming, OutVar0, Goals0,
        !ModuleInfo, !ProcInfo, !PredInfo, !Varset, !Vartypes, !BoundVarDescs),

    map.lookup(!.Vartypes, ProgVar, ProgVarType),
    (
        ( ProgVarType = io_state_type
        ; ProgVarType = io_io_type
        )
    ->
        OutVar = OutVar0,
        Goals = Goals0
    ;
        % BoundVarDescs is filled with the description of the input variable
        % during the first call to make_arg_list predicate.
        % At the second call, we search if the current ProgVar already exist
        % in the map and if yes, copy his recorded description.

        ( map.search(!.BoundVarDescs, ProgVar, ExistingVarDesc) ->
            ValueGoals = [],
            VarDesc = ExistingVarDesc
        ;
            make_var_value(InstMap, ProgVar, Renaming, VarDesc, Pos0,
                ValueGoals, !ModuleInfo, !ProcInfo, !PredInfo, !Varset,
                !Vartypes, !BoundVarDescs)
        ),

        svvarset.new_named_var("FullListVar", OutVar, !Varset),
        svmap.det_insert(OutVar, list_var_value_type, !Vartypes),
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

    % Create the goal's argument description :
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

    svvarset.new_named_var("VarDesc", VarDesc, !VarSet),
    ( var_is_ground_in_instmap(!.ModuleInfo, InstMap, VarToInspect) ->
        % Update proc_varset and proc_vartypes, without this, the
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

        create_poly_info(!.ModuleInfo, !.PredInfo, !.ProcInfo, PolyInfo0),
        term.context_init(Context),
        map.lookup(!.VarTypes, VarToInspect, MerType),
        polymorphism_make_type_info_var(MerType, Context, TypeInfoVar,
            TypeInfoGoal, PolyInfo0, PolyInfo),
        poly_info_extract(PolyInfo, !PredInfo, !ProcInfo, !:ModuleInfo),

        proc_info_get_varset(!.ProcInfo, !:VarSet),
        proc_info_get_vartypes(!.ProcInfo, !:VarTypes),

        % Constructor of the variable's description.
        ConsId = cons(qualified(SSDBModule, "bound_head_var"), 3,
            VarValueTypeCtor),
        svmap.det_insert(VarDesc, VarValueType, !VarTypes),

        % Renaming contains the names of all instantiated arguments
        % during the execution of the procedure's body.
        ( map.is_empty(Renaming) ->
            construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar,
                VarPosVar, VarToInspect], ConstructVarGoal)
        ;
            map.lookup(Renaming, VarToInspect, RenamedVar),
            construct_functor(VarDesc, ConsId, [TypeInfoVar, VarNameVar,
                VarPosVar, RenamedVar], ConstructVarGoal)
        ),
        Goals = [ConstructVarName, ConstructVarPos | TypeInfoGoal] ++
            [ConstructVarGoal],
        svmap.det_insert(VarToInspect, VarDesc, !BoundVarDescs)
    ;
        ConsId = cons(qualified(SSDBModule, "unbound_head_var"), 2,
            VarValueTypeCtor),
        svmap.det_insert(VarDesc, VarValueType, !VarTypes),
        construct_functor(VarDesc, ConsId, [VarNameVar, VarPosVar],
            ConstructVarGoal),

        Goals = [ConstructVarName, ConstructVarPos, ConstructVarGoal]
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ssdebug.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ssdebug.
%-----------------------------------------------------------------------------%
