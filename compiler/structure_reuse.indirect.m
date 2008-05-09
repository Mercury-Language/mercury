%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.indirect.m.
% Main authors: nancy.
%
% Determine the indirect reuse.  This requires a fixpoint computation.
%
%------------------------------------------------------------------------------%

:- module structure_reuse.indirect.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module io.
:- import_module list.

%------------------------------------------------------------------------------%

    % Direct reuse analysis derives information about deconstructions that
    % under certain circumstances (formalised as "reuse conditions") form
    % the last ever (memory) access to the deconstructed term.
    %
    % Indirect reuse analysis is about verifying procedure calls to see
    % whether these procedure calls satisfy these reuse conditions for
    % direct reuse, and under what circumstances (again expressed as
    % reuse conditions). Using a fixpoint computation, we determine the
    % overall reuse conditions for a procedure call to be replaced by a
    % call to an optimised version of that procedure w.r.t. its memory usage.
    %
    % The results of the analysis are primarily stored in the reuse table, yet
    % also involves annotations at the level of the individual procedure calls,
    % which explains the need for updating the HLDS as well.
    %
:- pred indirect_reuse_pass(sharing_as_table::in, module_info::in,
    module_info::out, reuse_as_table::in, reuse_as_table::out,
    list(pred_proc_id)::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.fixpoint_table.
:- import_module transform_hlds.ctgc.livedata.
:- import_module transform_hlds.ctgc.util.
:- import_module transform_hlds.dependency_graph.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.

%------------------------------------------------------------------------------%

:- type dep_procs == list(pred_proc_id).

%------------------------------------------------------------------------------%

indirect_reuse_pass(SharingTable, !ModuleInfo, !ReuseTable, DepProcs, !IO):-
    %
    % Perform a bottom-up traversal of the SCCs in the module,
    % analysing indirect structure reuse in each one as we go.
    %
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_get_maybe_dependency_info(!.ModuleInfo, MaybeDepInfo),
    (
        MaybeDepInfo = yes(DepInfo),
        hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
        list.foldl3(indirect_reuse_analyse_scc(SharingTable), SCCs,
            !ModuleInfo, !ReuseTable, [], DepProcs0),
        DepProcs = list.sort_and_remove_dups(DepProcs0)
    ;
        MaybeDepInfo = no,
        unexpected(this_file, "No dependency information.")
    ).

:- pred indirect_reuse_analyse_scc(sharing_as_table::in,
    list(pred_proc_id)::in, module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out, dep_procs::in, dep_procs::out)
    is det.

indirect_reuse_analyse_scc(SharingTable, SCC, !ModuleInfo, !ReuseTable,
        !DepProcs) :-
    ( some_preds_requiring_no_analysis(!.ModuleInfo, SCC) ->
        true
    ;
        FixpointTable0 = sr_fixpoint_table_init(!.ModuleInfo, SCC,
            !.ReuseTable),
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable,
            SCC, !.ReuseTable, !ModuleInfo, FixpointTable0, FixpointTable,
            !DepProcs),
        list.foldl(update_reuse_in_table(FixpointTable), SCC, !ReuseTable)
    ).

:- pred indirect_reuse_analyse_scc_until_fixpoint(sharing_as_table::in,
    list(pred_proc_id)::in, reuse_as_table::in,
    module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

indirect_reuse_analyse_scc_until_fixpoint(SharingTable, SCC,
        ReuseTable, !ModuleInfo, !FixpointTable, !DepProcs) :-
    list.foldl3(indirect_reuse_analyse_pred_proc(SharingTable, ReuseTable),
        SCC, !ModuleInfo, !FixpointTable, !DepProcs),
    ( sr_fixpoint_table_stable(!.FixpointTable) ->
        true
    ;
        sr_fixpoint_table_new_run(!FixpointTable),
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable,
            SCC, ReuseTable, !ModuleInfo, !FixpointTable, !DepProcs)
    ).

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_pred_proc(sharing_as_table::in,
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

indirect_reuse_analyse_pred_proc(SharingTable, ReuseTable, PPId,
        !ModuleInfo, !FixpointTable, !DepProcs) :-
    PPId = proc(PredId, _),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),
    ( Origin = origin_special_pred(_) ->
        % We can't analyse compiler generated special predicates.
        true
    ;
        indirect_reuse_analyse_pred_proc_2(SharingTable, ReuseTable, PPId,
            !ModuleInfo, !FixpointTable, !DepProcs)
    ).

:- pred indirect_reuse_analyse_pred_proc_2(sharing_as_table::in,
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

indirect_reuse_analyse_pred_proc_2(SharingTable, ReuseTable, PPId,
        !ModuleInfo, !FixpointTable, !DepProcs):-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(Globals, debug_indirect_reuse, DebugIndirect),

    PPId = proc(PredId, ProcId),

    % Some feedback..
    Run = sr_fixpoint_table_which_run(!.FixpointTable),
    (
        ( VeryVerbose = yes
        ; DebugIndirect = yes
        )
    ->
        trace [io(!IO)] (
            io.write_string("% Indirect reuse analysis (run ", !IO),
            io.write_int(Run, !IO),
            io.write_string(") ", !IO),
            write_pred_proc_id_pair(!.ModuleInfo, PredId, ProcId, !IO),
            io.nl(!IO)
        )
    ;
        true
    ),

    % Some initialisation work...
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),
    BaseInfo = ir_background_info_init(!.ModuleInfo, PPId, PredInfo0,
        ProcInfo0, SharingTable, ReuseTable),
    IrInfo0 = ir_analysis_info_init(PPId, !.FixpointTable),

    % The actual analysis of the goal:
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, IrInfo0, IrInfo,
        !DepProcs),
    !:FixpointTable = IrInfo ^ fptable,

    % Some feedback.
    (
        ( VeryVerbose = yes
        ; DebugIndirect = yes
        )
    ->
        trace [io(!IO)] (
            io.write_string("% FPT: ", !IO),
            io.write_string(
                sr_fixpoint_table_get_short_description(PPId, !.FixpointTable),
                !IO),
            io.nl(!IO)
        )
    ;
        true
    ),

    % Record the obtained reuse description in the fixpoint table...
    ReuseAs_Status = reuse_as_and_status(IrInfo ^ reuse_as,
        IrInfo ^ analysis_status),
    sr_fixpoint_table_new_as(!.ModuleInfo, ProcInfo0, PPId, ReuseAs_Status,
        !FixpointTable),

    % As the analysis changes the goal, we must update proc_info and
    % module_info:
    proc_info_set_goal(Goal, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo0, ProcInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % The type ir_background_info has the purpose to collect all the necessary
    % background information needed to be able to perform the indirect reuse
    % (ir) analysis of a specific procedure.
    %
:- type ir_background_info
    --->    ir_background_info(
                module_info     :: module_info,
                pred_proc_id    :: pred_proc_id,
                pred_info       :: pred_info,
                proc_info       :: proc_info,
                sharing_table   :: sharing_as_table,
                reuse_table     :: reuse_as_table,
                headvars        :: list(prog_var),
                very_verbose    :: bool,
                debug_indirect  :: bool
            ).

    % The type ir_analysis_info gathers the analysis information that may
    % change from goal to goal.
    %
:- type ir_analysis_info
    --->    ir_analysis_info(
                sharing_as      :: sharing_as,
                reuse_as        :: reuse_as,
                analysis_status :: analysis_status,
                static_vars     :: set(prog_var),
                fptable         :: sr_fixpoint_table
            ).

:- func ir_background_info_init(module_info, pred_proc_id, pred_info,
    proc_info, sharing_as_table, reuse_as_table) = ir_background_info.

ir_background_info_init(ModuleInfo, PPId, PredInfo, ProcInfo, SharingTable,
        ReuseTable) = BG :-
    % We don't need to keep track of any information regarding inserted
    % type-info arguments and alike, so we remove them from the list
    % of head variables:
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_vartypes(ProcInfo, Vartypes),
    HeadVarsOfInterest =
        remove_typeinfo_vars(Vartypes, HeadVars),

    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(Globals, debug_indirect_reuse, DebugIndirect),

    BG = ir_background_info(ModuleInfo, PPId, PredInfo, ProcInfo,
        SharingTable, ReuseTable, HeadVarsOfInterest, VeryVerbose,
        DebugIndirect).

:- func ir_analysis_info_init(pred_proc_id, sr_fixpoint_table) =
    ir_analysis_info.

ir_analysis_info_init(PPId, FixpointTable) = Info :-
    ReuseAs_Sharing = sr_fixpoint_table_get_final_as(PPId, FixpointTable),
    ReuseAs_Sharing = reuse_as_and_status(ReuseAs, Status),
    Info = ir_analysis_info(sharing_as_init, ReuseAs, Status, set.init,
        FixpointTable).

    % When analysing disjuncts (or switches) each branch yields its own
    % analysis information. This needs to be combined to form one single
    % analysis information to continue the analysis with.
    %
:- pred ir_analysis_info_combine(ir_background_info::in,
    list(ir_analysis_info)::in, sr_fixpoint_table::in, ir_analysis_info::in,
    ir_analysis_info::out) is det.

ir_analysis_info_combine(BaseInfo, IrInfoList, FixpointTable, !IrInfo) :-
    % If the IrInfoList = [], then the disjunct was simply empty, hence
    % nothing to be done. Otherwise, compute the lub of each of the components
    % of ir_analysis_info.
    (
        IrInfoList = []
    ;
        IrInfoList = [_ | _],
        list.foldl(ir_analysis_info_lub(BaseInfo), IrInfoList, !IrInfo),
        !IrInfo ^ fptable := FixpointTable
    ).

:- pred ir_analysis_info_lub(ir_background_info::in, ir_analysis_info::in,
    ir_analysis_info::in, ir_analysis_info::out) is det.

ir_analysis_info_lub(BaseInfo, IrInfo0, !IrInfo):-
    ModuleInfo = BaseInfo ^ module_info,
    ProcInfo = BaseInfo ^ proc_info,
    % Lub of the sharing
    NewSharing = sharing_as_least_upper_bound(ModuleInfo, ProcInfo,
        !.IrInfo ^ sharing_as, IrInfo0 ^ sharing_as),
    % Lub of the reuse
    NewReuse = reuse_as_least_upper_bound(ModuleInfo, ProcInfo,
        !.IrInfo ^ reuse_as, IrInfo0 ^ reuse_as),
    % Lub of the analysis status.
    NewStatus = lub(!.IrInfo ^ analysis_status, IrInfo0 ^ analysis_status),
    % Union of the static vars
    NewStaticVars = set.union(!.IrInfo ^ static_vars, IrInfo0 ^ static_vars),
    !:IrInfo = ir_analysis_info(NewSharing, NewReuse, NewStatus, NewStaticVars,
        !.IrInfo ^ fptable).

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_goal(ir_background_info::in, hlds_goal::in,
    hlds_goal::out, ir_analysis_info::in, ir_analysis_info::out,
    dep_procs::in, dep_procs::out) is det.

indirect_reuse_analyse_goal(BaseInfo, !Goal, !IrInfo, !DepProcs) :-
    ModuleInfo = BaseInfo ^ module_info,
    PredInfo = BaseInfo ^ pred_info,
    ProcInfo = BaseInfo ^ proc_info,
    SharingTable = BaseInfo ^ sharing_table,
    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl2(indirect_reuse_analyse_goal(BaseInfo),
            Goals0, Goals, !IrInfo, !DepProcs),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = plain_call(CalleePredId, CalleeProcId, CalleeArgs,
            _, _, _),
        verify_indirect_reuse(BaseInfo, CalleePredId, CalleeProcId,
            CalleeArgs, GoalInfo0, GoalInfo, !IrInfo),
        OldSharing = !.IrInfo ^ sharing_as,
        lookup_sharing_and_comb(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            CalleePredId, CalleeProcId, CalleeArgs, OldSharing, NewSharing),
        !IrInfo ^ sharing_as := NewSharing,

        % If the called procedure was imported (not opt_imported) then remember
        % that this module depends on the results for that procedure.
        (
            pred_info_get_import_status(PredInfo, PredImportStatus),
            status_defined_in_this_module(PredImportStatus) = yes,
            module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
            pred_info_get_import_status(CalleePredInfo, CalleeImportStatus),
            CalleeImportStatus = status_imported(_),
            \+ is_unify_or_compare_pred(CalleePredInfo)
        ->
            CalleePPId = proc(CalleePredId, CalleeProcId),
            !:DepProcs = [CalleePPId | !.DepProcs]
        ;
            true
        ),

        !:Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        GoalExpr0 = generic_call(_GenDetails, _, _, _),
        Context = goal_info_get_context(GoalInfo0),
        context_to_string(Context, ContextString),
        SharingAs = !.IrInfo ^ sharing_as,
        Msg = "generic call (" ++ ContextString ++ ")",
        !IrInfo ^ sharing_as :=
            sharing_as_top_sharing_accumulate(top_cannot_improve(Msg),
                SharingAs)
    ;
        GoalExpr0 = unify(_, _, _, Unification, _),
        % Record the statically constructed variables.
        (
            Unification = construct(Var, _, _, _, HowToConstruct, _, _),
            (
                HowToConstruct = construct_statically(_),
                !IrInfo ^ static_vars :=
                    set.insert(!.IrInfo ^ static_vars, Var)
            ;
                ( HowToConstruct = construct_dynamically
                ; HowToConstruct = reuse_cell(_)
                ; HowToConstruct = construct_in_region(_)
                )
            )
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
        ;
            Unification = complicated_unify(_, _, _),
            unexpected(this_file,
            "complicated unification in indirect structure sharing analysis.")
        ),
        !IrInfo ^ sharing_as :=
            add_unify_sharing(ModuleInfo, ProcInfo, Unification, GoalInfo0,
                !.IrInfo ^ sharing_as)
    ;
        GoalExpr0 = disj(Goals0),
        list.map2_foldl2(
            indirect_reuse_analyse_disj(BaseInfo, !.IrInfo),
            Goals0, Goals, IrInfoList, !.IrInfo ^ fptable, NewFixpointTable,
            !DepProcs),
        ir_analysis_info_combine(BaseInfo, IrInfoList, NewFixpointTable,
            !IrInfo),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map2_foldl2(
            indirect_reuse_analyse_case(BaseInfo, !.IrInfo),
            Cases0, Cases, IrInfoList, !.IrInfo ^ fptable, NewFixpointTable,
            !DepProcs),
        ir_analysis_info_combine(BaseInfo, IrInfoList, NewFixpointTable,
            !IrInfo),
        GoalExpr = switch(A, B, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        % XXX To check and compare with the theory.
        GoalExpr0 = negation(_Goal)
    ;
        GoalExpr0 = scope(A, SubGoal0),
        indirect_reuse_analyse_goal(BaseInfo, SubGoal0, SubGoal, !IrInfo,
            !DepProcs),
        GoalExpr = scope(A, SubGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        % Brief sketch:
        % * IrInfo0 --> IfGoal --> IrInfoIfGoal,
        % * IrInfoIfGoal --> ThenGoal --> IrInfoThenGoal,
        % * update IrInfo0 to include the latest state of the fixpoint
        % table, yields IrInfoElseGoal0
        % * IrInfoElseGoal0 --> ElseGoal --> IrInfoElseGoal
        % * and then compute the lub of IrInfoThenGoal,
        % and IrInfoElseGoal. Make sure that the result contains
        % the latest state of the fixpoint table, i.e., the one recorded
        % in IrInfoElseGoal.
        GoalExpr0 = if_then_else(A, IfGoal0, ThenGoal0, ElseGoal0),
        IrInfo0 = !.IrInfo,
        indirect_reuse_analyse_goal(BaseInfo, IfGoal0, IfGoal,
            IrInfo0, IrInfoIfGoal, !DepProcs),
        indirect_reuse_analyse_goal(BaseInfo, ThenGoal0, ThenGoal,
            IrInfoIfGoal, IrInfoThenGoal, !DepProcs),
        IrInfoElseGoal0 = IrInfo0 ^ fptable := IrInfoThenGoal ^ fptable,
        indirect_reuse_analyse_goal(BaseInfo, ElseGoal0, ElseGoal,
            IrInfoElseGoal0, IrInfoElseGoal, !DepProcs),
        ir_analysis_info_lub(BaseInfo, IrInfoThenGoal, IrInfoElseGoal,
            !:IrInfo),
        GoalExpr = if_then_else(A, IfGoal, ThenGoal, ElseGoal),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = call_foreign_proc(Attributes, ForeignPredId, ForeignProcId,
            Args, _ExtraArgs, _MaybeTraceRuntimeCond, _Impl),
        ForeignPPId = proc(ForeignPredId, ForeignProcId),
        Context = goal_info_get_context(GoalInfo0),
        OldSharing = !.IrInfo ^ sharing_as,
        add_foreign_proc_sharing(ModuleInfo, PredInfo, ProcInfo,
            ForeignPPId, Attributes, Args, Context, OldSharing, NewSharing),
        !IrInfo ^ sharing_as := NewSharing
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "indirect_reuse_analyse_goal: shorthand")
    ).

    % Analyse each branch of a disjunction with respect to an input
    % ir_analysis_info, producing a resulting ir_analysis_info, and possibly
    % updating the state of the sr_fixpoint_table.
    %
:- pred indirect_reuse_analyse_disj(ir_background_info::in,
    ir_analysis_info::in, hlds_goal::in, hlds_goal::out, ir_analysis_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

indirect_reuse_analyse_disj(BaseInfo, IrInfo0, Goal0, Goal, IrInfo,
        !FixpointTable, !DepProcs) :-
    % Replace the state of the fixpoint_table in IrInfo0:
    NewIrInfo = IrInfo0 ^ fptable := !.FixpointTable,
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, NewIrInfo, IrInfo,
        !DepProcs),
    !:FixpointTable = IrInfo ^ fptable.

    % Similar to indirect_reuse_analyse_disj.
:- pred indirect_reuse_analyse_case(ir_background_info::in,
    ir_analysis_info::in, case::in, case::out, ir_analysis_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

indirect_reuse_analyse_case(BaseInfo, IrInfo0, Case0, Case, IrInfo,
        !FixpointTable, !DepProcs) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    % Replace the state of the fixpoint_table in IrInfo0:
    NewIrInfo = IrInfo0 ^ fptable := !.FixpointTable,
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, NewIrInfo, IrInfo,
        !DepProcs),
    !:FixpointTable = IrInfo ^ fptable,
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%

:- type verify_indirect_reuse_reason
    --->    callee_has_no_reuses
    ;       callee_has_only_unconditional_reuse
    ;       current_sharing_is_top
    ;       reuse_is_unsafe(prog_vars)
    ;       reuse_is_unconditional
    ;       reuse_is_conditional.

:- pred verify_indirect_reuse(ir_background_info::in, pred_id::in, proc_id::in,
    prog_vars::in, hlds_goal_info::in, hlds_goal_info::out,
    ir_analysis_info::in, ir_analysis_info::out) is det.

verify_indirect_reuse(BaseInfo, CalleePredId, CalleeProcId, CalleeArgs,
        !GoalInfo, !IrInfo) :-
    % Find the reuse information of the called procedure in the reuse table:
    CalleePPId = proc(CalleePredId, CalleeProcId),
    lookup_reuse_as(BaseInfo, CalleePPId, !IrInfo, FormalReuseAs_Status),
    FormalReuseAs_Status = reuse_as_and_status(FormalReuseAs, LookupStatus),

    (
        % If there is no reuse, then nothing can be done.
        reuse_as_no_reuses(FormalReuseAs)
    ->
        Reason = callee_has_no_reuses
    ;
        reuse_as_all_unconditional_reuses(FormalReuseAs)
    ->
        % With unconditional reuse, we need to mark that the call is always
        % a reuse call.
        reuse_as_add_unconditional(!.IrInfo ^ reuse_as, NewReuseAs),
        !:IrInfo = !.IrInfo ^ reuse_as := NewReuseAs,
        goal_info_set_reuse(reuse(reuse_call(unconditional_reuse)), !GoalInfo),
        Reason = callee_has_only_unconditional_reuse
    ;
        % With a conditional reuse, we need to check the conditions. If they
        % are satisfied, these conditions need to be translated to the callers
        % environment. This translation can result in the reuse being
        % unconditional (this is the case if the reused data structures are
        % local to the procedure in which the call appears), or conditional.
        (
            % If the current sharing is top, then there is no use in
            % verifying reuse explicitly, as we don't have any information
            % anymore about existing (and therefore non-existing) sharing
            % pairs. In this case, reuse is not allowed.
            sharing_as_is_top(!.IrInfo ^ sharing_as)
        ->
            % no need to update anything
            Reason = current_sharing_is_top
        ;
            verify_indirect_reuse_2(BaseInfo, !.IrInfo, !.GoalInfo,
                CalleePPId, CalleeArgs, FormalReuseAs, NewAndRenamedReuseAs,
                NotDeadVars),
            (
                reuse_as_no_reuses(NewAndRenamedReuseAs)
            ->
                % Don't do anything.
                Reason = reuse_is_unsafe(NotDeadVars)
            ;
                reuse_as_all_unconditional_reuses(NewAndRenamedReuseAs)
            ->
                % Update reuse information and goal_info:
                reuse_as_add_unconditional(!.IrInfo ^ reuse_as, NewReuseAs),
                !IrInfo ^ reuse_as := NewReuseAs,
                goal_info_set_reuse(reuse(reuse_call(unconditional_reuse)),
                    !GoalInfo),
                Reason = reuse_is_unconditional
            ;
                % Update reuse information and goal_info:
                reuse_as_least_upper_bound(BaseInfo ^ module_info,
                    BaseInfo ^ proc_info, !.IrInfo ^ reuse_as,
                    NewAndRenamedReuseAs, NewReuseAs),
                !IrInfo ^ reuse_as := NewReuseAs,
                goal_info_set_reuse(
                    potential_reuse(reuse_call(conditional_reuse)),
                    !GoalInfo),
                Reason = reuse_is_conditional
            )
        )
    ),

    % Combine the status of the reuse information with the status of the
    % current analysis.
    !IrInfo ^ analysis_status := lub(LookupStatus, !.IrInfo ^ analysis_status),

    % Output the reasoning behind the result.
    trace [io(!IO)] (
        DebugIndirect = BaseInfo ^ debug_indirect,
        (
            DebugIndirect = yes,
            ModuleInfo = BaseInfo ^ module_info,
            GoalReuse = goal_info_get_reuse(!.GoalInfo),
            Context = goal_info_get_context(!.GoalInfo),
            proc_info_get_varset(BaseInfo ^ proc_info, VarSet),
            io.write_string("\tcall to ", !IO),
            write_pred_proc_id_pair(ModuleInfo, CalleePredId, CalleeProcId,
                !IO),
            io.write_string("\n\tfrom ", !IO),
            write_context(Context, !IO),
            io.write_string("\n\t\treuse: ", !IO),
            io.write(GoalReuse, !IO),
            io.write_string("\n\t\treason: ", !IO),
            write_verify_indirect_reuse_reason(Reason, VarSet, !IO),
            io.nl(!IO)
        ;
            DebugIndirect = no
        )
    ).

:- pred lookup_reuse_as(ir_background_info::in, pred_proc_id::in,
    ir_analysis_info::in, ir_analysis_info::out, reuse_as_and_status::out)
    is det.

lookup_reuse_as(BaseInfo, PPId, !IrInfo, ReuseAs) :-
    (
        % Check in the fixpoint table
        sr_fixpoint_table_get_as(PPId, ReuseAs0, !.IrInfo ^ fptable,
            NewFixpointTable)
    ->
        ReuseAs = ReuseAs0,
        !IrInfo ^ fptable := NewFixpointTable
    ;
        % Or check in the reuse table
        ReuseAs = get_reuse_as(BaseInfo ^ module_info, BaseInfo ^ reuse_table,
            PPId)
    ).

    % Verify whether the caller's environment satisfies the reuse conditions
    % stated in the reuse description of the called procedure. If this
    % succeeds, then translate those reuse conditions to this caller's
    % environment.
    %
    % Pre-conditions: The sharing is not top, and reuse_as contains at least
    % one conditional reuse condition.
    %
:- pred verify_indirect_reuse_2(ir_background_info::in, ir_analysis_info::in,
    hlds_goal_info::in, pred_proc_id::in, list(prog_var)::in, reuse_as::in,
    reuse_as::out, prog_vars::out) is det.

verify_indirect_reuse_2(BaseInfo, IrInfo, GoalInfo, CalleePPId,
        CalleeArgs, FormalReuseAs, NewReuseAs, NotDeadVars):-
    ModuleInfo = BaseInfo ^ module_info,
    PredInfo = BaseInfo ^ pred_info,
    ProcInfo = BaseInfo ^ proc_info,
    SharingAs = IrInfo ^ sharing_as,
    proc_info_get_vartypes(ProcInfo, ActualVarTypes),
    pred_info_get_typevarset(PredInfo, CallerTypeVarSet),
    pred_info_get_univ_quant_tvars(PredInfo, CallerHeadTypeParams),
    map.apply_to_list(CalleeArgs, ActualVarTypes, CalleeTypes),
    reuse_as_rename_using_module_info(ModuleInfo, CalleePPId,
        CalleeArgs, CalleeTypes, CallerTypeVarSet, CallerHeadTypeParams,
        FormalReuseAs, ActualReuseAs),
    LiveData = livedata_init_at_goal(ModuleInfo, ProcInfo, GoalInfo,
        SharingAs),
    ProjectedLiveData = livedata_project(CalleeArgs, LiveData),
    StaticVars = set.to_sorted_list(IrInfo ^ static_vars),
 
    reuse_as_satisfied(ModuleInfo, ProcInfo, ProjectedLiveData,
        SharingAs, StaticVars, ActualReuseAs, Result),
    (
        Result = reuse_possible,
        LFU = goal_info_get_lfu(GoalInfo),
        LBU = goal_info_get_lbu(GoalInfo),
        LU = set.union(LFU, LBU),
        LuList = set.to_sorted_list(LU),
        LuData = list.map(datastruct_init, LuList),
        NewReuseAs = reuse_as_from_called_procedure_to_local_reuse_as(
            ModuleInfo, ProcInfo, BaseInfo ^ headvars, LuData, SharingAs,
            ActualReuseAs),
        NotDeadVars = []
    ;
        Result = reuse_not_possible(Reason),
        NewReuseAs = reuse_as_init,  % no reuse
        (
            ( Reason = no_reuse
            ; Reason = unknown_livedata
            ; Reason = reuse_nodes_have_sharing
            ),
            NotDeadVars = []
        ;
            Reason = reuse_condition_violated(NotDeadVars)
        )
    ).

:- pred write_verify_indirect_reuse_reason(verify_indirect_reuse_reason::in,
    prog_varset::in, io::di, io::uo) is det.

write_verify_indirect_reuse_reason(Reason, VarSet, !IO) :-
    (
        ( Reason = callee_has_no_reuses
        ; Reason = callee_has_only_unconditional_reuse
        ; Reason = current_sharing_is_top
        ; Reason = reuse_is_unconditional
        ; Reason = reuse_is_conditional
        ),
        io.write(Reason, !IO)
    ;
        Reason = reuse_is_unsafe(Vars),
        io.write_string("reuse_is_unsafe(", !IO),
        mercury_output_vars(VarSet, yes, Vars, !IO),
        io.write_string(")", !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred update_reuse_in_table(sr_fixpoint_table::in, pred_proc_id::in,
    reuse_as_table::in, reuse_as_table::out) is det.

update_reuse_in_table(FixpointTable, PPId, !ReuseTable) :-
    reuse_as_table_set(PPId,
        sr_fixpoint_table_get_final_as(PPId, FixpointTable), !ReuseTable).

%-----------------------------------------------------------------------------%
%
% Structure reuse fixpoint table
%

:- type sr_fixpoint_table ==
    fixpoint_table(pred_proc_id, reuse_as_and_status).

    % Initialise the fixpoint table for the given set of pred_proc_id's.
    %
:- func sr_fixpoint_table_init(module_info, list(pred_proc_id), reuse_as_table)
    = sr_fixpoint_table.

    % Add the results of a new analysis pass to the already existing
    % fixpoint table.
    %
:- pred sr_fixpoint_table_new_run(sr_fixpoint_table::in,
    sr_fixpoint_table::out) is det.

    % The fixpoint table keeps track of the number of analysis passes. This
    % predicate returns this number.
    %
:- func sr_fixpoint_table_which_run(sr_fixpoint_table) = int.

    % A fixpoint is reached if all entries in the table are stable,
    % i.e. haven't been modified by the last analysis pass.
    %
:- pred sr_fixpoint_table_stable(sr_fixpoint_table::in) is semidet.

    % Give a string description of the state of the fixpoint table.
    %
:- func sr_fixpoint_table_description(sr_fixpoint_table) = string.

    % Enter the newly computed structure reuse description for a given
    % procedure.  If the description is different from the one that was
    % already stored for that procedure, the stability of the fixpoint
    % table is set to "unstable".
    % Software error if the procedure is not in the fixpoint table.
    %
:- pred sr_fixpoint_table_new_as(module_info::in, proc_info::in,
    pred_proc_id::in, reuse_as_and_status::in,
    sr_fixpoint_table::in, sr_fixpoint_table::out) is det.

    % Retrieve the structure reuse information for a given pred_proc_id.
    %
    % If the id is part of the fixpoint table, but does not yet record any
    % reuse information about that pred_proc_id, then this means that the
    % set of pred_proc_id's to which the fixpoint table relates is mutually
    % recursive, hence the table is characterised as recursive.
    %
    % If the id is not part of the fixpoint table: fail.
    %
:- pred sr_fixpoint_table_get_as(pred_proc_id::in, reuse_as_and_status::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out) is semidet.

:- func sr_fixpoint_table_get_short_description(pred_proc_id,
    sr_fixpoint_table) = string.

    % Retrieve the structure reuse information without changing the table.
    % To be used after fixpoint has been reached.
    % Software error if the procedure is not in the table.
    %
:- func sr_fixpoint_table_get_final_as(pred_proc_id,
    sr_fixpoint_table) = reuse_as_and_status.

    % Same as sr_fixpoint_table_get_final_as, yet fails instead of aborting
    % if the procedure is not in the table.
    %
:- pred sr_fixpoint_table_get_final_as_semidet(pred_proc_id::in,
    sr_fixpoint_table::in, reuse_as_and_status::out) is semidet.

%-----------------------------------------------------------------------------%

:- func get_reuse_as(module_info, reuse_as_table, pred_proc_id) =
    reuse_as_and_status.

get_reuse_as(ModuleInfo, ReuseTable, PPId) = ReuseAs :-
    ( reuse_as_table_search(PPId, ReuseTable, ReuseAs0) ->
        ReuseAs = ReuseAs0
    ;
        PPId = proc(PredId, _),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        (
            ( is_unify_or_compare_pred(PredInfo)
            ; pred_info_get_import_status(PredInfo, status_external(_))
            )
        ->
            Status = optimal
        ;
            % XXX not sure about this
            Status = suboptimal
        ),
        ReuseAs = reuse_as_and_status(reuse_as_init, Status)
    ).

sr_fixpoint_table_init(ModuleInfo, Keys, ReuseTable) = Table :-
    Table = init_fixpoint_table(get_reuse_as(ModuleInfo, ReuseTable), Keys).

sr_fixpoint_table_new_run(!Table) :-
    fixpoint_table.new_run(!Table).

sr_fixpoint_table_which_run(Tin) = fixpoint_table.which_run(Tin).

sr_fixpoint_table_stable(Table) :-
    fixpoint_table.fixpoint_reached(Table).

sr_fixpoint_table_description(Table) = fixpoint_table.description(Table).

sr_fixpoint_table_new_as(ModuleInfo, ProcInfo, Id, ReuseAs, !Table) :-
    add_to_fixpoint_table(reuse_as_and_status_subsumed_by(ModuleInfo, ProcInfo),
        Id, ReuseAs, !Table).

sr_fixpoint_table_get_as(PPId, ReuseAs, !Table) :-
    get_from_fixpoint_table(PPId, ReuseAs, !Table).

sr_fixpoint_table_get_short_description(PPId, Table) = Descr :-
    ( fixpoint_table.is_recursive(Table) ->
        Rec = "(rec)"
    ;
        Rec = "(non-rec)"
    ),
    (
        sr_fixpoint_table_get_final_as_semidet(PPId, Table,
            reuse_as_and_status(ReuseAs, _))
    ->
        Descr0 = reuse_as_short_description(ReuseAs)
    ;
        Descr0 = "-"
    ),
    Descr = Descr0 ++ " " ++ Rec.

sr_fixpoint_table_get_final_as(PPId, T) =
    get_from_fixpoint_table_final(PPId, T).

sr_fixpoint_table_get_final_as_semidet(PPId, T, Elem) :-
    get_from_fixpoint_table_final_semidet(PPId, T, Elem).

%------------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.indirect.m".

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
