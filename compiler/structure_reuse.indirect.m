%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.indirect.m.
% Main authors: nancy, wangp.
%
% Determine the indirect reuse.  This requires a fixpoint computation.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.indirect.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module set.

%-----------------------------------------------------------------------------%

    % Represents a request to perform analyses of a procedure with
    % restriction on which arguments may be clobbered.
    %
:- type sr_request
    --->    sr_request(
                srreq_ppid  :: pred_proc_id,
                srreq_args  :: no_clobber_args
            ).

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
    % Returns requests for analyses of procedures with specific call patterns,
    % both for procedures defined in this module and externally.
    %
:- pred indirect_reuse_pass(sharing_as_table::in, module_info::in,
    module_info::out, reuse_as_table::in, reuse_as_table::out,
    set(ppid_no_clobbers)::out, set(sr_request)::out, set(sr_request)::out)
    is det.

    % Repeat the indirect structure reuse analysis.
    %
:- pred indirect_reuse_rerun(sharing_as_table::in, module_info::in,
    module_info::out, reuse_as_table::in, reuse_as_table::out,
    set(ppid_no_clobbers)::out, set(sr_request)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module analysis.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.fixpoint_table.
:- import_module transform_hlds.ctgc.livedata.
:- import_module transform_hlds.ctgc.util.

:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type dep_procs == set(ppid_no_clobbers).

%-----------------------------------------------------------------------------%

indirect_reuse_pass(SharingTable, !ModuleInfo, !ReuseTable, DepProcs,
        Requests, IntermodRequests) :-
    % Perform a bottom-up traversal of the SCCs in the module,
    % analysing indirect structure reuse in each one as we go.
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl5(indirect_reuse_analyse_scc(SharingTable), SCCs,
        !ModuleInfo, !ReuseTable, set.init, DepProcs, set.init, Requests,
        set.init, IntermodRequests).

:- pred indirect_reuse_analyse_scc(sharing_as_table::in, scc::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out,
    dep_procs::in, dep_procs::out,
    set(sr_request)::in, set(sr_request)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

indirect_reuse_analyse_scc(SharingTable, SCC, !ModuleInfo, !ReuseTable,
        !DepProcs, !Requests, !IntermodRequests) :-
    set.to_sorted_list(SCC, SCCProcs),
    ( if some_preds_require_no_analysis(!.ModuleInfo, SCC) then
        true
    else
        FixpointTable0 = sr_fixpoint_table_init(SCCProcs, !.ReuseTable),
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable,
            SCCProcs, !.ReuseTable, !ModuleInfo, FixpointTable0, FixpointTable,
            !DepProcs, !Requests, !IntermodRequests),
        set.foldl(update_reuse_in_table(FixpointTable), SCC, !ReuseTable)
    ).

:- pred update_reuse_in_table(sr_fixpoint_table::in, pred_proc_id::in,
    reuse_as_table::in, reuse_as_table::out) is det.

update_reuse_in_table(FixpointTable, PPId, !ReuseTable) :-
    FinalAs = sr_fixpoint_table_get_final_as(PPId, FixpointTable),
    reuse_as_table_set(PPId, FinalAs, !ReuseTable).

%-----------------------------------------------------------------------------%

indirect_reuse_rerun(SharingTable, !ModuleInfo, !ReuseTable,
        DepProcs, Requests, !IntermodRequests) :-
    module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl5(indirect_reuse_rerun_analyse_scc(SharingTable),
        SCCs, !ModuleInfo, !ReuseTable, set.init, DepProcs, set.init, Requests,
        !IntermodRequests).

:- pred indirect_reuse_rerun_analyse_scc(sharing_as_table::in, scc::in,
    module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out,
    dep_procs::in, dep_procs::out,
    set(sr_request)::in, set(sr_request)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

indirect_reuse_rerun_analyse_scc(SharingTable, SCC, !ModuleInfo,
        !ReuseTable, !DepProcs, !Requests, !IntermodRequests) :-
    ( if some_preds_require_no_analysis(!.ModuleInfo, SCC) then
        true
    else
        % Also analyse reuse versions of any procedures in the SCC at the same
        % time.
        set.to_sorted_list(SCC, SCCProcs),
        extend_scc_with_reuse_procs(!.ReuseTable, SCCProcs, ExtendedSCC),

        FixpointTable0 = sr_fixpoint_table_init(ExtendedSCC, !.ReuseTable),
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable,
            ExtendedSCC, !.ReuseTable, !ModuleInfo, FixpointTable0,
            FixpointTable, !DepProcs, !Requests, !IntermodRequests),
        list.foldl(update_reuse_in_table(FixpointTable), ExtendedSCC,
            !ReuseTable)
    ).

:- pred extend_scc_with_reuse_procs(reuse_as_table::in, list(pred_proc_id)::in,
    list(pred_proc_id)::out) is det.

extend_scc_with_reuse_procs(ReuseTable, SCC, ExtendedSCC) :-
    ReuseVersionMap = bimap.forward_map(ReuseTable ^ reuse_version_map),
    solutions(
        ( pred(NewPPId::out) is nondet :-
            member(OrigPPId, SCC),
            map.member(ReuseVersionMap, ppid_no_clobbers(OrigPPId, _), NewPPId)
        ), Extension),
    ExtendedSCC = SCC ++ Extension.

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_scc_until_fixpoint(sharing_as_table::in,
    list(pred_proc_id)::in, reuse_as_table::in,
    module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out,
    set(sr_request)::in, set(sr_request)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

indirect_reuse_analyse_scc_until_fixpoint(SharingTable, SCC,
        ReuseTable, !ModuleInfo, !FixpointTable, !DepProcs, !Requests,
        !IntermodRequests) :-
    list.foldl5(indirect_reuse_analyse_pred_proc(SharingTable, ReuseTable),
        SCC, !ModuleInfo, !FixpointTable, !DepProcs, !Requests,
        !IntermodRequests),
    ( if sr_fixpoint_table_stable(!.FixpointTable) then
        true
    else
        sr_fixpoint_table_new_run(!FixpointTable),
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable, SCC,
            ReuseTable, !ModuleInfo, !FixpointTable, !DepProcs, !Requests,
            !IntermodRequests)
    ).

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_pred_proc(sharing_as_table::in,
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out,
    set(sr_request)::in, set(sr_request)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

indirect_reuse_analyse_pred_proc(SharingTable, ReuseTable, PPId,
        !ModuleInfo, !FixpointTable, !DepProcs, !Requests,
        !IntermodRequests) :-
    PPId = proc(PredId, _),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_origin(PredInfo, Origin),
    ( if Origin = origin_special_pred(_, _) then
        % We can't analyse compiler generated special predicates.
        true
    else
        indirect_reuse_analyse_pred_proc_2(SharingTable, ReuseTable, PPId,
            !ModuleInfo, !FixpointTable, !DepProcs, !Requests,
            !IntermodRequests)
    ).

:- pred indirect_reuse_analyse_pred_proc_2(sharing_as_table::in,
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out,
    dep_procs::in, dep_procs::out,
    set(sr_request)::in, set(sr_request)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

indirect_reuse_analyse_pred_proc_2(SharingTable, ReuseTable, PPId,
        !ModuleInfo, !FixpointTable, !DepProcs, !Requests,
        !IntermodRequests) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(Globals, debug_indirect_reuse, DebugIndirect),

    PPId = proc(PredId, ProcId),

    % Some feedback..
    Run = sr_fixpoint_table_which_run(!.FixpointTable),
    ( if
        ( VeryVerbose = yes
        ; DebugIndirect = yes
        )
    then
        trace [io(!IO)] (
            io.write_string("% Indirect reuse analysis (run ", !IO),
            io.write_int(Run, !IO),
            io.write_string(") ", !IO),
            write_pred_proc_id_pair(!.ModuleInfo, PredId, ProcId, !IO),
            io.nl(!IO)
        )
    else
        true
    ),

    % Some initialisation work...
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),
    BaseInfo = ir_background_info_init(!.ModuleInfo, PPId, PredInfo0,
        ProcInfo0, SharingTable, ReuseTable),
    IrInfo0 = ir_analysis_info_init(PPId, !.FixpointTable, !.DepProcs,
        !.Requests, !.IntermodRequests),

    % The actual analysis of the goal:
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, IrInfo0, IrInfo),

    IrInfo = ir_analysis_info(_, _, _, _, !:FixpointTable, !:DepProcs,
        !:Requests, !:IntermodRequests),

    % Some feedback.
    ( if
        ( VeryVerbose = yes
        ; DebugIndirect = yes
        )
    then
        trace [io(!IO)] (
            io.write_string("% FPT: ", !IO),
            io.write_string(
                sr_fixpoint_table_get_short_description(PPId,
                    !.FixpointTable),
                !IO),
            io.nl(!IO),

            NumConditions = reuse_as_count_conditions(IrInfo ^ reuse_as),
            io.write_string("% Number of conditions: ", !IO),
            io.write_int(NumConditions, !IO),
            io.nl(!IO)
        )
    else
        true
    ),

    % Record the obtained reuse description in the fixpoint table...
    ReuseAs_Status = reuse_as_and_status(IrInfo ^ reuse_as,
        IrInfo ^ analysis_status),
    sr_fixpoint_table_new_as(!.ModuleInfo, ProcInfo0, PPId,
        ReuseAs_Status, !FixpointTable),

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
                max_conditions  :: int,
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
                fptable         :: sr_fixpoint_table,
                dep_procs       :: dep_procs,
                requests        :: set(sr_request),
                                % Requests to locally-defined procedures.
                inter_requests  :: set(sr_request)
                                % Requests to imported procedures.
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
    HeadVarsOfInterest = remove_typeinfo_vars(Vartypes, HeadVars),

    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, structure_reuse_max_conditions,
        MaxConditions),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(Globals, debug_indirect_reuse, DebugIndirect),

    BG = ir_background_info(ModuleInfo, PPId, PredInfo, ProcInfo,
        SharingTable, ReuseTable, HeadVarsOfInterest, MaxConditions,
        VeryVerbose, DebugIndirect).

:- func ir_analysis_info_init(pred_proc_id, sr_fixpoint_table, dep_procs,
    set(sr_request), set(sr_request)) = ir_analysis_info.

ir_analysis_info_init(PPId, FixpointTable, DepProcs, Requests,
        IntermodRequests) = Info :-
    ReuseAs_Sharing = sr_fixpoint_table_get_final_as(PPId, FixpointTable),
    ReuseAs_Sharing = reuse_as_and_status(ReuseAs, Status),
    Info = ir_analysis_info(sharing_as_init, ReuseAs, Status, set.init,
        FixpointTable, DepProcs, Requests, IntermodRequests).

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

    % Union of the dependencies.
    NewDepProcs = set.union(!.IrInfo ^ dep_procs, IrInfo0 ^ dep_procs),

    % Union of the requests.
    NewRequests = set.union(!.IrInfo ^ requests, IrInfo0 ^ requests),
    NewIntermodRequests = set.union(!.IrInfo ^ inter_requests,
        IrInfo0 ^ inter_requests),

    % The fixpoint table field is updated in ir_analysis_info_combine.
    !:IrInfo = ir_analysis_info(NewSharing, NewReuse, NewStatus, NewStaticVars,
        !.IrInfo ^ fptable, NewDepProcs, NewRequests, NewIntermodRequests).

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_goal(ir_background_info::in, hlds_goal::in,
    hlds_goal::out, ir_analysis_info::in, ir_analysis_info::out) is det.

indirect_reuse_analyse_goal(BaseInfo, !Goal, !IrInfo) :-
    ModuleInfo = BaseInfo ^ module_info,
    PredInfo = BaseInfo ^ pred_info,
    ProcInfo = BaseInfo ^ proc_info,

    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl(indirect_reuse_analyse_goal(BaseInfo),
            Goals0, Goals, !IrInfo),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = plain_call(_, _, _, _, _, _),
        indirect_reuse_analyse_plain_call(BaseInfo,
            hlds_goal(GoalExpr0, GoalInfo0), !:Goal, !IrInfo)
    ;
        GoalExpr0 = generic_call(GenDetails, CallArgs, Modes, _MaybeArgRegs,
            _Detism),
        indirect_reuse_analyse_generic_call(BaseInfo, GenDetails, CallArgs,
            Modes, GoalInfo0, !IrInfo)
    ;
        GoalExpr0 = unify(_, _, _, Unification, _),
        % Record the statically constructed variables.
        (
            Unification = construct(Var, _, _, _, HowToConstruct, _, _),
            (
                HowToConstruct = construct_statically,
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
            unexpected($pred, "complicated unification")
        ),
        OldSharing = !.IrInfo ^ sharing_as,
        NewSharing = add_unify_sharing(ModuleInfo, ProcInfo, Unification,
            GoalInfo0, OldSharing),
        update_sharing_as(BaseInfo, OldSharing, NewSharing, !IrInfo)
    ;
        GoalExpr0 = disj(Goals0),
        list.map2_foldl(indirect_reuse_analyse_disj(BaseInfo, !.IrInfo),
            Goals0, Goals, IrInfoList, !.IrInfo ^ fptable, NewFixpointTable),
        ir_analysis_info_combine(BaseInfo, IrInfoList, NewFixpointTable,
            !IrInfo),
        GoalExpr = disj(Goals),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map2_foldl(
            indirect_reuse_analyse_case(BaseInfo, !.IrInfo),
            Cases0, Cases, IrInfoList, !.IrInfo ^ fptable, NewFixpointTable),
        ir_analysis_info_combine(BaseInfo, IrInfoList, NewFixpointTable,
            !IrInfo),
        GoalExpr = switch(A, B, Cases),
        !:Goal = hlds_goal(GoalExpr, GoalInfo0)
    ;
        % XXX To check and compare with the theory.
        GoalExpr0 = negation(_Goal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        % XXX We should special-case the handling of from_ground_term_construct
        % scopes.
        indirect_reuse_analyse_goal(BaseInfo, SubGoal0, SubGoal, !IrInfo),
        GoalExpr = scope(Reason, SubGoal),
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
            IrInfo0, IrInfoIfGoal),
        indirect_reuse_analyse_goal(BaseInfo, ThenGoal0, ThenGoal,
            IrInfoIfGoal, IrInfoThenGoal),
        IrInfoElseGoal0 = IrInfo0 ^ fptable := IrInfoThenGoal ^ fptable,
        indirect_reuse_analyse_goal(BaseInfo, ElseGoal0, ElseGoal,
            IrInfoElseGoal0, IrInfoElseGoal),
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
        update_sharing_as(BaseInfo, OldSharing, NewSharing, !IrInfo)
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred indirect_reuse_analyse_plain_call(ir_background_info::in,
    hlds_goal::in(goal_plain_call), hlds_goal::out(goal_plain_call),
    ir_analysis_info::in, ir_analysis_info::out) is det.

indirect_reuse_analyse_plain_call(BaseInfo, !Goal, !IrInfo) :-
    ModuleInfo = BaseInfo ^ module_info,
    PredInfo = BaseInfo ^ pred_info,
    ProcInfo = BaseInfo ^ proc_info,
    SharingTable = BaseInfo ^ sharing_table,

    !.Goal = hlds_goal(GoalExpr0, GoalInfo0),
    GoalExpr0 = plain_call(CalleePredId, CalleeProcId, CalleeArgs,
        _Builtin, _Context, _Sym),
    Reuse0 = goal_info_get_reuse(GoalInfo0),
    (
        Reuse0 = no_reuse_info,
        Verify = yes
    ;
        Reuse0 = no_possible_reuse,
        Verify = no
    ;
        (
            Reuse0 = missed_reuse(_)
        ;
            Reuse0 = potential_reuse(_)
        ;
            Reuse0 = reuse(_)
            % It's possible that the called procedure had "unconditional
            % reuse only" previously but has since gained reuse conditions.
        ),
        Verify = yes
        % For imported procedures, even though we know its reuse information
        % can't have changed since the last pass (so verification is guaranteed
        % to succeed a second time), we still need to call
        % `verify_indirect_reuse' so that its conditions will be added to the
        % reuse_as.
    ),
    (
        Verify = yes,

        % Attempt to limit the number of reuse conditions on a procedure.
        % If there are too many conditions already, don't make any more
        % calls to reuse procedures which have conditions on them.
        MaxConditions = BaseInfo ^ max_conditions,
        ( if
            reuse_as_count_conditions(!.IrInfo ^ reuse_as) >= MaxConditions
        then
            CondReuseHandling = ignore_conditional_reuse
        else
            CondReuseHandling = allow_conditional_reuse
        ),
        NoClobbers = [],
        verify_indirect_reuse(BaseInfo, proc(CalleePredId, CalleeProcId),
            NoClobbers, CalleeArgs, CondReuseHandling, GoalInfo0, GoalInfo,
            !IrInfo),
        !:Goal = hlds_goal(GoalExpr0, GoalInfo)
    ;
        Verify = no
    ),
    OldSharing = !.IrInfo ^ sharing_as,
    lookup_sharing_and_comb(ModuleInfo, PredInfo, ProcInfo, SharingTable,
        CalleePredId, CalleeProcId, CalleeArgs, OldSharing, NewSharing),
    update_sharing_as(BaseInfo, OldSharing, NewSharing, !IrInfo).

:- pred indirect_reuse_analyse_generic_call(ir_background_info::in,
    generic_call::in, prog_vars::in, list(mer_mode)::in, hlds_goal_info::in,
    ir_analysis_info::in, ir_analysis_info::out) is det.

indirect_reuse_analyse_generic_call(BaseInfo, GenDetails, CallArgs, Modes,
        GoalInfo, !IrInfo) :-
    ModuleInfo = BaseInfo ^ module_info,
    ProcInfo = BaseInfo ^ proc_info,
    (
        ( GenDetails = higher_order(_, _, _, _)
        ; GenDetails = class_method(_, _, _, _)
        ),
        proc_info_get_vartypes(ProcInfo, CallerVarTypes),
        lookup_var_types(CallerVarTypes, CallArgs, ActualTypes),
        ( if
            bottom_sharing_is_safe_approximation_by_args(ModuleInfo, Modes,
                ActualTypes)
        then
            SetToTop = no
        else
            SetToTop = yes
        )
    ;
        ( GenDetails = event_call(_) % XXX too conservative
        ; GenDetails = cast(_)
        ),
        SetToTop = yes
    ),
    (
        SetToTop = no
    ;
        SetToTop = yes,
        Context = goal_info_get_context(GoalInfo),
        context_to_string(Context, ContextString),
        Msg = "generic call (" ++ ContextString ++ ")",
        OldSharing = !.IrInfo ^ sharing_as,
        NewSharing = sharing_as_top_sharing_accumulate(
            top_cannot_improve(Msg), OldSharing),
        update_sharing_as(BaseInfo, OldSharing, NewSharing, !IrInfo)
    ).

    % Analyse each branch of a disjunction with respect to an input
    % ir_analysis_info, producing a resulting ir_analysis_info, and possibly
    % updating the state of the sr_fixpoint_table.
    %
:- pred indirect_reuse_analyse_disj(ir_background_info::in,
    ir_analysis_info::in, hlds_goal::in, hlds_goal::out, ir_analysis_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out) is det.

indirect_reuse_analyse_disj(BaseInfo, IrInfo0, Goal0, Goal, IrInfo,
        !FixpointTable) :-
    % Replace the state of the fixpoint_table in IrInfo0:
    NewIrInfo = IrInfo0 ^ fptable := !.FixpointTable,
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, NewIrInfo, IrInfo),
    !:FixpointTable = IrInfo ^ fptable.

    % Similar to indirect_reuse_analyse_disj.
:- pred indirect_reuse_analyse_case(ir_background_info::in,
    ir_analysis_info::in, case::in, case::out, ir_analysis_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out) is det.

indirect_reuse_analyse_case(BaseInfo, IrInfo0, Case0, Case, IrInfo,
        !FixpointTable) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    % Replace the state of the fixpoint_table in IrInfo0:
    NewIrInfo = IrInfo0 ^ fptable := !.FixpointTable,
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, NewIrInfo, IrInfo),
    !:FixpointTable = IrInfo ^ fptable,
    Case = case(MainConsId, OtherConsIds, Goal).

:- pred update_sharing_as(ir_background_info::in, sharing_as::in,
    sharing_as::in, ir_analysis_info::in, ir_analysis_info::out) is det.

update_sharing_as(BaseInfo, OldSharing, NewSharing, !IrInfo) :-
    DebugIndirect = BaseInfo ^ debug_indirect,
    (
        DebugIndirect = yes,
        trace [io(!IO)] (
            ( if
                sharing_as_is_top(NewSharing),
                not sharing_as_is_top(OldSharing)
            then
                io.write_string("\tsharing is now top\n", !IO)
            else
                true
            )
        )
    ;
        DebugIndirect = no
    ),
    !IrInfo ^ sharing_as := NewSharing.

%-----------------------------------------------------------------------------%
%
% Verification of a reuse calls
%

:- type conditional_reuse_handling
    --->    allow_conditional_reuse
    ;       ignore_conditional_reuse.

:- type verify_indirect_reuse_reason
    --->    callee_has_no_reuses
    ;       callee_has_only_unconditional_reuse
    ;       current_sharing_is_top
    ;       reuse_is_unsafe(prog_vars)
    ;       reuse_is_unconditional
    ;       reuse_is_conditional.

:- pred verify_indirect_reuse(ir_background_info::in, pred_proc_id::in,
    list(int)::in, prog_vars::in, conditional_reuse_handling::in,
    hlds_goal_info::in, hlds_goal_info::out,
    ir_analysis_info::in, ir_analysis_info::out) is det.

    % CalleePPId refers to the original procedure, not the procedure of any
    % reuse version of another procedure.
    %
verify_indirect_reuse(BaseInfo, CalleePPId, NoClobbers, CalleeArgs,
        CondReuseHandling, !GoalInfo, !IrInfo) :-
    % Find the reuse information of the called procedure in the reuse table:
    % XXX if we can't find an exact match for NoClobbers, we could try
    % procedures which have no-clobber sets which are supersets of NoClobbers.
    lookup_reuse_as(BaseInfo, CalleePPId, NoClobbers, !IrInfo, FormalReuseAs),
    ( if
        % If there is no reuse, then nothing can be done.
        reuse_as_no_reuses(FormalReuseAs)
    then
        Reason = callee_has_no_reuses,
        % Setting `no_possible_reuse' here would have adverse effects because
        % the reuse_as from `lookup_reuse_as' is not definitive.  It may
        % return something else later.
        trace [io(!IO)] (
            maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId,
                NoClobbers, !.GoalInfo, Reason, !IO)
        )
    else if
        reuse_as_all_unconditional_reuses(FormalReuseAs)
    then
        % With unconditional reuse, we need to mark that the call is always
        % a reuse call.
        reuse_as_add_unconditional(!.IrInfo ^ reuse_as, NewReuseAs),
        !IrInfo ^ reuse_as := NewReuseAs,
        goal_info_set_reuse(reuse(reuse_call(unconditional_reuse, NoClobbers)),
            !GoalInfo),
        trace [io(!IO)] (
            maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId,
                NoClobbers, !.GoalInfo, callee_has_only_unconditional_reuse,
                !IO)
        )
    else
        (
            CondReuseHandling = allow_conditional_reuse,
            % With a conditional reuse, we need to check the conditions.
            % If they are satisfied, these conditions need to be translated
            % to the callers environment. This translation can result in
            % the reuse being unconditional (this is the case if the reused
            % data structures are local to the procedure in which the call
            % appears), or conditional.
            ( if
                % If the current sharing is top, then there is no use in
                % verifying reuse explicitly, as we don't have any information
                % anymore about existing (and therefore non-existing) sharing
                % pairs. In this case, reuse is not allowed.
                sharing_as_is_top(!.IrInfo ^ sharing_as)
            then
                goal_info_set_reuse(no_possible_reuse, !GoalInfo),
                trace [io(!IO)] (
                    maybe_write_verify_indirect_reuse_reason(BaseInfo,
                        CalleePPId, NoClobbers, !.GoalInfo,
                        current_sharing_is_top, !IO)
                )
            else
                verify_indirect_reuse_conditional(BaseInfo, CalleePPId,
                    NoClobbers, CalleeArgs, FormalReuseAs, !GoalInfo, !IrInfo)
            )
        ;
            CondReuseHandling = ignore_conditional_reuse,
            goal_info_set_reuse(no_possible_reuse, !GoalInfo)
        )
    ).

:- pred verify_indirect_reuse_conditional(ir_background_info::in,
    pred_proc_id::in, no_clobber_args::in, prog_vars::in, reuse_as::in,
    hlds_goal_info::in, hlds_goal_info::out, ir_analysis_info::in,
    ir_analysis_info::out) is det.

verify_indirect_reuse_conditional(BaseInfo, CalleePPId, NoClobbers, CalleeArgs,
        FormalReuseAs, !GoalInfo, !IrInfo) :-
    verify_indirect_reuse_for_call(BaseInfo, !.IrInfo, !.GoalInfo, CalleePPId,
        CalleeArgs, FormalReuseAs, NewAndRenamedReuseAs, NotDeadVars),
    ( if reuse_as_no_reuses(NewAndRenamedReuseAs) then
        get_var_indices(NotDeadVars, CalleeArgs, 1, NotDeadArgNums0),
        NotDeadArgNums = list.sort_and_remove_dups(NotDeadArgNums0
            ++ NoClobbers),
        ( if
            NotDeadArgNums = NoClobbers
        then
            % Don't do anything.  Don't even request a new version.
            goal_info_set_reuse(no_possible_reuse, !GoalInfo),
            trace [io(!IO)] (
                maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId,
                    NoClobbers, !.GoalInfo, reuse_is_unsafe(NotDeadVars), !IO)
            )
        else if
            % If there is already an entry for the callee procedure with the
            % same set of no-clobber arguments we don't need to make a request.
            % XXX might we look up the result for the procedures we're
            % currently analysing, and would that be a problem?
            reuse_as_table_search_reuse_version_proc(BaseInfo ^ reuse_table,
                CalleePPId, NotDeadArgNums, _ReusePPId)
        then
            verify_indirect_reuse(BaseInfo, CalleePPId, NotDeadArgNums,
                CalleeArgs, allow_conditional_reuse, !GoalInfo, !IrInfo)
        else
            % Request another version of the procedure.
            add_request(BaseInfo, CalleePPId, NotDeadArgNums, IntraModule,
                !IrInfo),
            (
                IntraModule = yes,
                goal_info_set_reuse(no_reuse_info, !GoalInfo)
            ;
                IntraModule = no,
                goal_info_set_reuse(no_possible_reuse, !GoalInfo)
            ),
            trace [io(!IO)] (
                maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId,
                    NoClobbers, !.GoalInfo, reuse_is_unsafe(NotDeadVars), !IO)
            )
        )
    else if reuse_as_all_unconditional_reuses(NewAndRenamedReuseAs) then
        % Update reuse information and goal_info:
        reuse_as_add_unconditional(!.IrInfo ^ reuse_as, NewReuseAs),
        !IrInfo ^ reuse_as := NewReuseAs,
        goal_info_set_reuse(reuse(reuse_call(unconditional_reuse, NoClobbers)),
            !GoalInfo),
        trace [io(!IO)] (
            maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId,
                NoClobbers, !.GoalInfo, reuse_is_unconditional, !IO)
        )
    else if reuse_as_conditional_reuses(NewAndRenamedReuseAs) then
        % Update reuse information and goal_info:
        reuse_as_least_upper_bound(BaseInfo ^ module_info,
            BaseInfo ^ proc_info, !.IrInfo ^ reuse_as, NewAndRenamedReuseAs,
            NewReuseAs),
        !IrInfo ^ reuse_as := NewReuseAs,
        goal_info_set_reuse(
            potential_reuse(reuse_call(conditional_reuse, NoClobbers)),
            !GoalInfo),
        trace [io(!IO)] (
            maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId,
                NoClobbers, !.GoalInfo, reuse_is_conditional, !IO)
        )
    else
        unexpected($pred, "unknown NewReuseAs")
    ).

    % Verify whether the caller's environment satisfies the reuse conditions
    % stated in the reuse description of the called procedure. If this
    % succeeds, then translate those reuse conditions to this caller's
    % environment.
    %
    % Pre-conditions: The sharing is not top, and reuse_as contains at least
    % one conditional reuse condition.
    %
:- pred verify_indirect_reuse_for_call(ir_background_info::in,
    ir_analysis_info::in, hlds_goal_info::in, pred_proc_id::in,
    list(prog_var)::in, reuse_as::in, reuse_as::out, prog_vars::out) is det.

verify_indirect_reuse_for_call(BaseInfo, IrInfo, GoalInfo, CalleePPId,
        CalleeArgs, FormalReuseAs, NewReuseAs, NotDeadVars) :-
    ModuleInfo = BaseInfo ^ module_info,
    PredInfo = BaseInfo ^ pred_info,
    ProcInfo = BaseInfo ^ proc_info,
    SharingAs = IrInfo ^ sharing_as,
    proc_info_get_vartypes(ProcInfo, ActualVarTypes),
    pred_info_get_typevarset(PredInfo, CallerTypeVarSet),
    pred_info_get_univ_quant_tvars(PredInfo, CallerHeadTypeParams),
    lookup_var_types(ActualVarTypes, CalleeArgs, CalleeTypes),
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
        LU = set_of_var.union(LFU, LBU),
        LuList = set_of_var.to_sorted_list(LU),
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
            ),
            NotDeadVars = []
        ;
            Reason = reuse_condition_violated(NotDeadVars)
        ;
            Reason = reuse_nodes_have_sharing(NotDeadVars)
        )
    ).

:- pred lookup_reuse_as(ir_background_info::in, pred_proc_id::in,
    list(int)::in, ir_analysis_info::in, ir_analysis_info::out,
    reuse_as::out) is det.

lookup_reuse_as(BaseInfo, OrigPPId, NoClobbers, !IrInfo, ReuseAs) :-
    ( if
        reuse_as_table_search_reuse_version_proc(BaseInfo ^ reuse_table,
            OrigPPId, NoClobbers, PPId)
    then
        lookup_reuse_as_2(BaseInfo, OrigPPId, PPId, NoClobbers, !IrInfo,
            ReuseAs)
    else if
        NoClobbers = []
    then
        lookup_reuse_as_2(BaseInfo, OrigPPId, OrigPPId, NoClobbers, !IrInfo,
            ReuseAs)
    else
        unexpected($pred, "conditions failed")
    ).

:- pred lookup_reuse_as_2(ir_background_info::in, pred_proc_id::in,
    pred_proc_id::in, list(int)::in, ir_analysis_info::in,
    ir_analysis_info::out, reuse_as::out) is det.

lookup_reuse_as_2(BaseInfo, OrigPPId, PPId, NoClobbers, !IrInfo, ReuseAs) :-
    ( if
        % Check in the fixpoint table
        sr_fixpoint_table_get_as(PPId, ReuseAs_Status0, !.IrInfo ^ fptable,
            NewFixpointTable)
    then
        ReuseAs_Status = ReuseAs_Status0,
        !IrInfo ^ fptable := NewFixpointTable
    else
        % Or check in the reuse table
        ReuseAs_Status = get_reuse_as(BaseInfo ^ reuse_table, PPId)
    ),

    ReuseAs_Status = reuse_as_and_status(ReuseAs, Status),

    % Combine the status of the reuse information with the status of the
    % current analysis.
    !IrInfo ^ analysis_status := lub(Status, !.IrInfo ^ analysis_status),

    % If the called procedure was imported (not opt_imported) then remember
    % that this module depends on the results for that procedure.
    OrigPPId = proc(CalleePredId, _),
    module_info_pred_info(BaseInfo ^ module_info, CalleePredId,
        CalleePredInfo),
    ( if
        pred_info_is_imported_not_external(CalleePredInfo),
        not is_unify_index_or_compare_pred(CalleePredInfo)
    then
        Dep = ppid_no_clobbers(OrigPPId, NoClobbers),
        !IrInfo ^ dep_procs := set.insert(!.IrInfo ^ dep_procs, Dep)
    else
        true
    ).

    % Output the reasoning behind the result.
    %
:- pred maybe_write_verify_indirect_reuse_reason(ir_background_info::in,
    pred_proc_id::in, list(int)::in, hlds_goal_info::in,
    verify_indirect_reuse_reason::in, io::di, io::uo) is det.

maybe_write_verify_indirect_reuse_reason(BaseInfo, CalleePPId, NoClobbers,
        GoalInfo, Reason, !IO) :-
    DebugIndirect = BaseInfo ^ debug_indirect,
    (
        DebugIndirect = yes,
        ModuleInfo = BaseInfo ^ module_info,
        GoalReuse = goal_info_get_reuse(GoalInfo),
        Context = goal_info_get_context(GoalInfo),
        proc_info_get_varset(BaseInfo ^ proc_info, VarSet),
        io.write_string("\tcall to ", !IO),
        write_pred_proc_id(ModuleInfo, CalleePPId, !IO),
        io.write_string("\n\tfrom ", !IO),
        write_context(Context, !IO),
        io.write_string("\n\twith NoClobbers = ", !IO),
        io.write(NoClobbers, !IO),
        io.write_string("\n\t\treuse: ", !IO),
        io.write(GoalReuse, !IO),
        io.write_string("\n\t\treason: ", !IO),
        write_verify_indirect_reuse_reason(Reason, VarSet, !IO),
        io.nl(!IO)
    ;
        DebugIndirect = no
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
        mercury_output_vars(VarSet, print_name_and_num, Vars, !IO),
        io.write_string(")", !IO)
    ).

:- pred get_var_indices(prog_vars::in, prog_vars::in, int::in,
    list(int)::out) is det.

get_var_indices(_, [], _, []).
get_var_indices(List, [Var | Vars], Index, Indices) :-
    get_var_indices(List, Vars, Index + 1, Indices0),
    ( if list.member(Var, List) then
        Indices = [Index | Indices0]
    else
        Indices = Indices0
    ).

    % Add an intra- or inter-module request for the called procedure.
    %
:- pred add_request(ir_background_info::in, pred_proc_id::in, list(int)::in,
    bool::out, ir_analysis_info::in, ir_analysis_info::out) is det.

add_request(BaseInfo, CalleePPId, NotDeadArgNums, IntraModule, !IrInfo) :-
    CalleePPId = proc(CalleePredId, _),
    ModuleInfo = BaseInfo ^ module_info,
    module_info_pred_info(ModuleInfo, CalleePredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        ( pred_status_defined_in_this_module(PredStatus) = yes
        ; PredStatus = pred_status(status_opt_imported)
        )
    then
        IntraModule = yes,
        Request = sr_request(CalleePPId, NotDeadArgNums),
        !IrInfo ^ requests := set.insert(!.IrInfo ^ requests, Request)
    else
        IntraModule = no,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, intermodule_analysis,
            IntermoduleAnalysis),
        (
            IntermoduleAnalysis = yes,
            Request = sr_request(CalleePPId, NotDeadArgNums),
            !IrInfo ^ inter_requests :=
                set.insert(!.IrInfo ^ inter_requests, Request)
        ;
            IntermoduleAnalysis = no
        )
    ).

%-----------------------------------------------------------------------------%
%
% Structure reuse fixpoint table
%

:- type sr_fixpoint_table ==
    fixpoint_table(pred_proc_id, reuse_as_and_status).

    % Initialise the fixpoint table for the given set of pred_proc_id's.
    %
:- func sr_fixpoint_table_init(list(pred_proc_id), reuse_as_table)
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
:- pragma consider_used(sr_fixpoint_table_description/1).

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

:- func get_reuse_as(reuse_as_table, pred_proc_id) = reuse_as_and_status.

get_reuse_as(ReuseTable, PPId) = ReuseAs :-
    ( if reuse_as_table_search(ReuseTable, PPId, ReuseAs0) then
        ReuseAs = ReuseAs0
    else
        % We assume an unknown answer is `optimal' otherwise we would not be
        % able to get mutually recursive procedures out of the `suboptimal'
        % state.
        ReuseAs = reuse_as_and_status(reuse_as_init, optimal)
    ).

sr_fixpoint_table_init(Keys, ReuseTable) = Table :-
    Table = init_fixpoint_table(get_reuse_as(ReuseTable), Keys).

sr_fixpoint_table_new_run(!Table) :-
    fixpoint_table.new_run(!Table).

sr_fixpoint_table_which_run(Tin) = fixpoint_table.which_run(Tin).

sr_fixpoint_table_stable(Table) :-
    fixpoint_table.fixpoint_reached(Table).

sr_fixpoint_table_description(Table) = fixpoint_table.description(Table).

sr_fixpoint_table_new_as(ModuleInfo, ProcInfo, Id, ReuseAs, !Table) :-
    add_to_fixpoint_table(
        reuse_as_and_status_subsumed_by(ModuleInfo, ProcInfo),
        Id, ReuseAs, !Table).

sr_fixpoint_table_get_as(PPId, ReuseAs, !Table) :-
    get_from_fixpoint_table(PPId, ReuseAs, !Table).

sr_fixpoint_table_get_short_description(PPId, Table) = Descr :-
    ( if fixpoint_table.is_recursive(Table) then
        Rec = "(rec)"
    else
        Rec = "(non-rec)"
    ),
    ( if
        sr_fixpoint_table_get_final_as_semidet(PPId, Table,
            reuse_as_and_status(ReuseAs, _))
    then
        Descr0 = reuse_as_short_description(ReuseAs)
    else
        Descr0 = "-"
    ),
    Descr = Descr0 ++ " " ++ Rec.

sr_fixpoint_table_get_final_as(PPId, T) =
    get_from_fixpoint_table_final(PPId, T).

sr_fixpoint_table_get_final_as_semidet(PPId, T, Elem) :-
    get_from_fixpoint_table_final_semidet(PPId, T, Elem).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.indirect.
%-----------------------------------------------------------------------------%
