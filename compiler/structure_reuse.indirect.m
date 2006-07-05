%------------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
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
%------------------------------------------------------------------------------%

:- module structure_reuse.indirect.
:- interface.

:- import_module hlds.hlds_module.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module io.

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
    io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.ctgc.datastruct.
:- import_module transform_hlds.ctgc.fixpoint_table.
:- import_module transform_hlds.ctgc.livedata.
:- import_module transform_hlds.ctgc.util.
:- import_module transform_hlds.dependency_graph.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.

%------------------------------------------------------------------------------%

indirect_reuse_pass(SharingTable, !ModuleInfo, !ReuseTable, !IO):-
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
            !ModuleInfo, !ReuseTable, !IO)
    ;
        MaybeDepInfo = no,
        unexpected(this_file, "No dependency information.")
    ).

:- pred indirect_reuse_analyse_scc(sharing_as_table::in,
    list(pred_proc_id)::in, module_info::in, module_info::out, 
    reuse_as_table::in, reuse_as_table::out, io::di, io::uo) is det.

indirect_reuse_analyse_scc(SharingTable, SCC, !ModuleInfo, !ReuseTable, !IO) :- 
    ( preds_requiring_no_analysis(!.ModuleInfo, SCC) ->
        true
    ;
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable,
            SCC, !.ReuseTable, !ModuleInfo, 
            sr_fixpoint_table_init(SCC, !.ReuseTable), FixpointTable, !IO),
        list.foldl(update_reuse_in_table(FixpointTable), SCC, !ReuseTable)
    ).

:- pred indirect_reuse_analyse_scc_until_fixpoint(sharing_as_table::in, 
    list(pred_proc_id)::in, reuse_as_table::in,
    module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out, io::di, io::uo) is det.

indirect_reuse_analyse_scc_until_fixpoint(SharingTable, SCC, 
        ReuseTable, !ModuleInfo, !FixpointTable, !IO):-
    list.foldl3(indirect_reuse_analyse_pred_proc(SharingTable, ReuseTable), 
        SCC, !ModuleInfo, !FixpointTable, !IO),
    ( sr_fixpoint_table_stable(!.FixpointTable) ->
        true
    ;
        sr_fixpoint_table_new_run(!FixpointTable),
        indirect_reuse_analyse_scc_until_fixpoint(SharingTable,
            SCC, ReuseTable, !ModuleInfo, !FixpointTable, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_pred_proc(sharing_as_table::in,
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out, io::di, io::uo) is det.

indirect_reuse_analyse_pred_proc(SharingTable, ReuseTable, PPId, 
        !ModuleInfo, !FixpointTable, !IO):- 
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),

    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    PPId = proc(PredId, ProcId),

    % Some feedback.. 
    Run = string.int_to_string(sr_fixpoint_table_which_run(!.FixpointTable)),
    passes_aux.write_proc_progress_message(
        "% Indirect reuse analysis (run " ++ Run ++ ") ",
        PredId, ProcId, !.ModuleInfo, !IO),

    % Some initialisation work...
    proc_info_get_goal(ProcInfo0, Goal0),
    BaseInfo = ir_background_info_init(!.ModuleInfo, PredInfo0, ProcInfo0, 
        SharingTable, ReuseTable),
    AnalysisInfo0 = analysis_info_init(PPId, !.FixpointTable),

    % The actual analysis of the goal: 
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, AnalysisInfo0, 
        AnalysisInfo, !IO),
    !:FixpointTable = AnalysisInfo ^ fptable, 

    % Some feedback.
    maybe_write_string(VeryVerbose, "% FPT: " ++
        sr_fixpoint_table_get_short_description(PPId, !.FixpointTable)
        ++ "\n", !IO),

    % Record the obtained reuse description in the fixpoint table...
    sr_fixpoint_table_new_as(!.ModuleInfo, ProcInfo0, PPId, 
        AnalysisInfo ^ reuse_as, !FixpointTable),

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
                pred_info       :: pred_info, 
                proc_info       :: proc_info, 
                sharing_table   :: sharing_as_table, 
                reuse_table     :: reuse_as_table,
                headvars        :: list(prog_var)
            ).
            
    % The type analysis_info gathers the analysis information that may change
    % from goal to goal.
    %
:- type ir_analysis_info 
    --->    ir_analysis_info(
                sharing_as      :: sharing_as,
                reuse_as        :: reuse_as,
                static_vars     :: set(prog_var),
                fptable         :: sr_fixpoint_table
            ).

:- func ir_background_info_init(module_info, pred_info, proc_info,
    sharing_as_table, reuse_as_table) = ir_background_info. 

ir_background_info_init(ModuleInfo, PredInfo, ProcInfo, SharingTable, 
        ReuseTable) = BG :- 
    PredOrigArity = pred_info_orig_arity(PredInfo), 
    proc_info_get_headvars(ProcInfo, HeadVars),
    PredArity = list.length(HeadVars), 
    Diff = PredArity - PredOrigArity, 
    % We don't need to keep track of any information regarding inserted
    % type-info arguments and alike, so we remove them from the list
    % of headvariables: 
    list.det_split_list(Diff, HeadVars, _AddedHeadVars, OrigHeadVars), 

    BG = ir_background_info(ModuleInfo, PredInfo, ProcInfo, 
        SharingTable, ReuseTable, OrigHeadVars).

:- func analysis_info_init(pred_proc_id, sr_fixpoint_table) = ir_analysis_info.

analysis_info_init(PPId, FixpointTable) = Info :- 
    ReuseAs = sr_fixpoint_table_get_final_as(PPId, FixpointTable),
    Info = ir_analysis_info(sharing_as_init, ReuseAs, set.init, FixpointTable).

    % When analysing disjuncts (or switches) each branch yields its own
    % analysis information. This needs to be combined to form one single
    % analysis information to continue the analysis with. 
    %
:- pred analysis_info_combine(ir_background_info::in, 
    list(ir_analysis_info)::in, sr_fixpoint_table::in, ir_analysis_info::in, 
    ir_analysis_info::out) is det.

analysis_info_combine(BaseInfo, AnalysisInfoList, FixpointTable, 
        !AnalysisInfo) :- 
    % If the AnalysisInfoList = [], then the disjunct was simply empty, hence
    % nothing to be done. Otherwise, compute the lub of each of the components
    % of analysis_info.
    (
        AnalysisInfoList = []
    ;
        AnalysisInfoList = [_ | _],
        list.foldl(analysis_info_lub(BaseInfo), AnalysisInfoList, 
            !AnalysisInfo),
        !:AnalysisInfo = !.AnalysisInfo ^ fptable := FixpointTable
    ).

:- pred analysis_info_lub(ir_background_info::in, ir_analysis_info::in,
    ir_analysis_info::in, ir_analysis_info::out) is det.

analysis_info_lub(BaseInfo, AnalysisInfo0, !AnalysisInfo):- 
    ModuleInfo = BaseInfo ^ module_info, 
    ProcInfo = BaseInfo ^ proc_info, 
    % Lub of the sharing
    NewSharing = sharing_as_least_upper_bound(ModuleInfo, ProcInfo, 
        !.AnalysisInfo ^ sharing_as, AnalysisInfo0 ^ sharing_as),
    % Lub of the reuse
    NewReuse = reuse_as_least_upper_bound(ModuleInfo, ProcInfo, 
        !.AnalysisInfo ^ reuse_as, AnalysisInfo0 ^ reuse_as),
    % Union of the static vars
    NewStaticVars = set.union(!.AnalysisInfo ^ static_vars, 
        AnalysisInfo0 ^ static_vars),
    !:AnalysisInfo = ir_analysis_info(NewSharing, NewReuse, NewStaticVars,
        !.AnalysisInfo ^ fptable).

%-----------------------------------------------------------------------------%

:- pred indirect_reuse_analyse_goal(ir_background_info::in, hlds_goal::in, 
    hlds_goal::out, ir_analysis_info::in, ir_analysis_info::out, 
    io::di, io::uo) is det.

indirect_reuse_analyse_goal(BaseInfo, !Goal, !AnalysisInfo, !IO) :-
    ModuleInfo = BaseInfo ^ module_info, 
    PredInfo = BaseInfo ^ pred_info, 
    ProcInfo = BaseInfo ^ proc_info, 
    SharingTable = BaseInfo ^ sharing_table, 
    !.Goal = GoalExpr0 - GoalInfo0,
    (
        GoalExpr0 = conj(ConjType, Goals0),
        list.map_foldl2(indirect_reuse_analyse_goal(BaseInfo), Goals0, 
            Goals, !AnalysisInfo, !IO),
        GoalExpr = conj(ConjType, Goals),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = call(CalleePredId, CalleeProcId, CalleeArgs, _, _, _),
        verify_indirect_reuse(BaseInfo, CalleePredId, CalleeProcId, 
            CalleeArgs, GoalInfo0, GoalInfo, !AnalysisInfo, !IO),
        lookup_sharing_and_comb(ModuleInfo, PredInfo, ProcInfo, SharingTable, 
            CalleePredId, CalleeProcId, CalleeArgs, 
            !.AnalysisInfo ^ sharing_as, NewSharing),
        !:AnalysisInfo = !.AnalysisInfo ^ sharing_as := NewSharing,
        GoalExpr = GoalExpr0,
        !:Goal = GoalExpr - GoalInfo
    ;
        GoalExpr0 = generic_call(_GenDetails, _, _, _),
        goal_info_get_context(GoalInfo0, Context),
        context_to_string(Context, ContextString),
        !:AnalysisInfo = !.AnalysisInfo ^ sharing_as :=
            sharing_as_top_sharing_accumulate("generic call (" 
                ++ ContextString ++ ")", 
            !.AnalysisInfo ^ sharing_as)
    ;
        GoalExpr0 = unify(_, _, _, Unification, _),
        % Record the statically constructed variables: 
        ( Unification = construct(Var, _, _, _, 
                construct_statically(_), _, _) ->
            !:AnalysisInfo = !.AnalysisInfo ^ static_vars := 
                set.insert(!.AnalysisInfo ^ static_vars, Var)
        ;
            true
        ), 
        !:AnalysisInfo = !.AnalysisInfo ^ sharing_as :=
            add_unify_sharing(ModuleInfo, ProcInfo, Unification, 
            GoalInfo0, !.AnalysisInfo ^ sharing_as)
    ;
        GoalExpr0 = disj(Goals0),
        list.map2_foldl2(
            indirect_reuse_analyse_disj(BaseInfo, !.AnalysisInfo),
            Goals0, Goals, AnalysisInfoList, !.AnalysisInfo ^ fptable,
            NewFixpointTable, !IO),
        analysis_info_combine(BaseInfo, AnalysisInfoList, NewFixpointTable,
            !AnalysisInfo),
        GoalExpr = disj(Goals),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = switch(A, B, Cases0),
        list.map2_foldl2(
            indirect_reuse_analyse_case(BaseInfo, !.AnalysisInfo),
            Cases0, Cases, AnalysisInfoList, !.AnalysisInfo ^ fptable,
            NewFixpointTable, !IO),
        analysis_info_combine(BaseInfo, AnalysisInfoList, NewFixpointTable,
            !AnalysisInfo),
        GoalExpr = switch(A, B, Cases),
        !:Goal = GoalExpr - GoalInfo0
    ;
        % XXX To check and compare with the theory. 
        GoalExpr0 = not(_Goal)
    ;
        GoalExpr0 = scope(A, SubGoal0),
        indirect_reuse_analyse_goal(BaseInfo, SubGoal0, SubGoal, 
            !AnalysisInfo, !IO),
        GoalExpr = scope(A, SubGoal),
        !:Goal = GoalExpr - GoalInfo0
    ;
        % Brief sketch: 
        % * AnalysisInfo0 --> IfGoal --> AnalysisInfoIfGoal,
        % * AnalysisInfoIfGoal --> ThenGoal --> AnalysisInfoThenGoal,
        % * update AnalysisInfo0 to include the latest state of the fixpoint
        % table, yields AnalysisInfoElseGoal0
        % * AnalysisInfoElseGoal0 --> ElseGoal --> AnalysisInfoElseGoal
        % * and then compute the lub of AnalysisInfoThenGoal, 
        % and AnalysisInfoElseGoal. Make sure that the result contains
        % the latest state of the fixpoint table, i.e., the one recorded
        % in AnalysisInfoElseGoal.
        GoalExpr0 = if_then_else(A, IfGoal0, ThenGoal0, ElseGoal0),
        AnalysisInfo0 = !.AnalysisInfo, 
        indirect_reuse_analyse_goal(BaseInfo, IfGoal0, IfGoal, 
            AnalysisInfo0, AnalysisInfoIfGoal, !IO), 
        indirect_reuse_analyse_goal(BaseInfo, ThenGoal0, ThenGoal,
            AnalysisInfoIfGoal, AnalysisInfoThenGoal, !IO),
        AnalysisInfoElseGoal0 = AnalysisInfo0 ^ fptable :=
            AnalysisInfoThenGoal ^ fptable,
        indirect_reuse_analyse_goal(BaseInfo, ElseGoal0, ElseGoal,
            AnalysisInfoElseGoal0, AnalysisInfoElseGoal, !IO), 
        analysis_info_lub(BaseInfo, AnalysisInfoThenGoal, 
            AnalysisInfoElseGoal, !:AnalysisInfo),
        GoalExpr = if_then_else(A, IfGoal, ThenGoal, ElseGoal),
        !:Goal = GoalExpr - GoalInfo0
    ;
        GoalExpr0 = foreign_proc(Attributes, ForeignPredId, ForeignProcId,
            _ForeignArgs, _, _),
        goal_info_get_context(GoalInfo0, Context),
        !:AnalysisInfo = !.AnalysisInfo ^ sharing_as :=
            add_foreign_proc_sharing(ModuleInfo, ProcInfo, 
            proc(ForeignPredId, ForeignProcId), Attributes, Context, 
            !.AnalysisInfo ^ sharing_as)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "indirect_reuse_analyse_goal: shorthand goal.")
    ).
   
    % Analyse each branch of a disjunction with respect to an input
    % analysis_info, producing a resulting analysis_info, and possibly
    % updating the state of the sr_fixpoint_table.
    %
:- pred indirect_reuse_analyse_disj(ir_background_info::in, 
    ir_analysis_info::in, hlds_goal::in, hlds_goal::out, ir_analysis_info::out, 
    sr_fixpoint_table::in, sr_fixpoint_table::out, io::di, io::uo) is det.

indirect_reuse_analyse_disj(BaseInfo, AnalysisInfo0, Goal0, Goal, AnalysisInfo,
        !FixpointTable, !IO) :- 
    % Replace the state of the fixpoint_table in AnalysisInfo0:
    NewAnalysisInfo = AnalysisInfo0 ^ fptable := !.FixpointTable,
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, NewAnalysisInfo, 
        AnalysisInfo, !IO),
    !:FixpointTable = AnalysisInfo ^ fptable.

    % Similar to indirect_reuse_analyse_disj.
:- pred indirect_reuse_analyse_case(ir_background_info::in, 
    ir_analysis_info::in, case::in, case::out, ir_analysis_info::out, 
    sr_fixpoint_table::in, sr_fixpoint_table::out, io::di, io::uo) is det.

indirect_reuse_analyse_case(BaseInfo, AnalysisInfo0, Case0, Case, AnalysisInfo,
        !FixpointTable, !IO) :- 
    Case0 = case(ConsId, Goal0),
    % Replace the state of the fixpoint_table in AnalysisInfo0:
    NewAnalysisInfo = AnalysisInfo0 ^ fptable := !.FixpointTable,
    indirect_reuse_analyse_goal(BaseInfo, Goal0, Goal, NewAnalysisInfo, 
        AnalysisInfo, !IO),
    !:FixpointTable = AnalysisInfo ^ fptable,
    Case = case(ConsId, Goal).

%-----------------------------------------------------------------------------%

:- pred verify_indirect_reuse(ir_background_info::in, pred_id::in, proc_id::in, 
    prog_vars::in, hlds_goal_info::in, hlds_goal_info::out, 
    ir_analysis_info::in, ir_analysis_info::out, io::di, io::uo) is det.

verify_indirect_reuse(BaseInfo, CalleePredId, CalleeProcId, CalleeArgs,
        !GoalInfo, !AnalysisInfo, !IO):-
    % Find the reuse information of the called procedure in the reuse table:
    CalleePPId = proc(CalleePredId, CalleeProcId),
    lookup_reuse_as(BaseInfo, CalleePPId, !AnalysisInfo, FormalReuseAs),

    (
        % If there is no reuse, then nothing can be done.
        reuse_as_no_reuses(FormalReuseAs)
    ->
        true
    ;   
        reuse_as_all_unconditional_reuses(FormalReuseAs)
    ->
        % With unconditional reuse, we need to mark that the call is always
        % a reuse call, yet without implying conditions.
        reuse_as_add_unconditional(!.AnalysisInfo ^ reuse_as, NewReuseAs),
        !:AnalysisInfo = !.AnalysisInfo ^ reuse_as := NewReuseAs,
        goal_info_set_reuse(reuse(reuse_call(unconditional_reuse)),
            !GoalInfo)
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
            sharing_as_is_top(!.AnalysisInfo ^ sharing_as)
        ->
            % no need to update anything
            true
        ;
            verify_indirect_reuse_2(BaseInfo, !.AnalysisInfo, !.GoalInfo, 
                CalleePPId, CalleeArgs, FormalReuseAs, 
                NewAndRenamedReuseAs),
            (
                reuse_as_no_reuses(NewAndRenamedReuseAs)
            ->
                % Don't do anything.
                true
            ;
                reuse_as_all_unconditional_reuses(NewAndRenamedReuseAs)
            ->
                % Update reuse information and goal_info:
                reuse_as_add_unconditional(!.AnalysisInfo ^ reuse_as, 
                    NewReuseAs),
                !:AnalysisInfo = !.AnalysisInfo ^ reuse_as := NewReuseAs,
                goal_info_set_reuse(reuse(reuse_call(unconditional_reuse)),
                    !GoalInfo)
            ;
                % Update reuse information and goal_info:
                reuse_as_least_upper_bound(BaseInfo ^ module_info, 
                    BaseInfo ^ proc_info, !.AnalysisInfo ^ reuse_as, 
                    NewAndRenamedReuseAs, NewReuseAs),
                !:AnalysisInfo = !.AnalysisInfo ^ reuse_as := NewReuseAs,
                goal_info_set_reuse(
                    potential_reuse(reuse_call(conditional_reuse)),
                    !GoalInfo)
            )
        )
    ).
    
:- pred lookup_reuse_as(ir_background_info::in, pred_proc_id::in,
    ir_analysis_info::in, ir_analysis_info::out, reuse_as::out) is det.

lookup_reuse_as(BaseInfo, PPId, !AnalysisInfo, ReuseAs) :-
    (
        % Check in the fixpoint table
        sr_fixpoint_table_get_as(PPId, ReuseAs0, !.AnalysisInfo ^ fptable,
            NewFixpointTable)
    ->
        ReuseAs = ReuseAs0,
        !:AnalysisInfo = !.AnalysisInfo ^ fptable := NewFixpointTable
    ;
        % Or check in the reuse table
        ReuseAs0 = reuse_as_table_search(PPId, BaseInfo ^ reuse_table)
    ->
        ReuseAs = ReuseAs0
    ;
        ReuseAs = reuse_as_init
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
    reuse_as::out) is det.

verify_indirect_reuse_2(BaseInfo, AnalysisInfo, GoalInfo, CalleePPId, 
        CalleeArgs, FormalReuseAs, NewReuseAs):-
    ModuleInfo = BaseInfo ^ module_info, 
    PredInfo = BaseInfo ^ pred_info,
    ProcInfo = BaseInfo ^ proc_info, 
    SharingAs = AnalysisInfo ^ sharing_as, 
    proc_info_get_vartypes(ProcInfo, ActualVarTypes),
    pred_info_get_typevarset(PredInfo, ActualTVarset),  
    list.map(map.lookup(ActualVarTypes), CalleeArgs, CalleeTypes),
    reuse_as_rename_using_module_info(ModuleInfo, CalleePPId,
        CalleeArgs, CalleeTypes, ActualTVarset, FormalReuseAs, ActualReuseAs),
    LiveData = livedata_init_at_goal(ModuleInfo, ProcInfo, GoalInfo,
        SharingAs),
    ProjectedLiveData = livedata_project(CalleeArgs, LiveData),
    (
        reuse_as_satisfied(ModuleInfo, ProcInfo, ProjectedLiveData,
            SharingAs, set.to_sorted_list(AnalysisInfo ^ static_vars), 
            ActualReuseAs)
    ->
        LuData = list.map(datastruct_init, 
            set.to_sorted_list(set.union(goal_info_get_lfu(GoalInfo),
                goal_info_get_lbu(GoalInfo)))),
        NewReuseAs = reuse_as_from_called_procedure_to_local_reuse_as(
            ModuleInfo, ProcInfo, BaseInfo ^ headvars, LuData, SharingAs,
            ActualReuseAs) 
    ;
        NewReuseAs = reuse_as_init  % no reuse
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

:- type sr_fixpoint_table == fixpoint_table(pred_proc_id, reuse_as).

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

    % Enter the newly computed structure reuse description for a given
    % procedure.  If the description is different from the one that was
    % already stored for that procedure, the stability of the fixpoint
    % table is set to "unstable".
    % Software error if the procedure is not in the fixpoint table.
    %
:- pred sr_fixpoint_table_new_as(module_info::in, proc_info::in,
    pred_proc_id::in, reuse_as::in,
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
:- pred sr_fixpoint_table_get_as(pred_proc_id::in, reuse_as::out,
    sr_fixpoint_table::in, sr_fixpoint_table::out) is semidet.

:- func sr_fixpoint_table_get_short_description(pred_proc_id,
    sr_fixpoint_table) = string.

    % Retrieve the structure reuse information without changing the table.
    % To be used after fixpoint has been reached.
    % Software error if the procedure is not in the table.
    %
:- func sr_fixpoint_table_get_final_as(pred_proc_id,
    sr_fixpoint_table) = reuse_as.

    % Same as sr_fixpoint_table_get_final_as, yet fails instead of aborting
    % if the procedure is not in the table.
    %
:- func sr_fixpoint_table_get_final_as_semidet(pred_proc_id,
    sr_fixpoint_table) = reuse_as is semidet.

%-----------------------------------------------------------------------------%

:- func get_reuse_as(reuse_as_table, pred_proc_id) = reuse_as.

get_reuse_as(ReuseTable, PPId) = ReuseAs :- 
    ( ReuseAs0 = reuse_as_table_search(PPId, ReuseTable) ->
        ReuseAs = ReuseAs0
    ;
        ReuseAs = reuse_as_init
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
    add_to_fixpoint_table(reuse_as_subsumed_by(ModuleInfo, ProcInfo),
        Id, ReuseAs, !Table).

sr_fixpoint_table_get_as(PPId, ReuseAs, !Table) :-
    get_from_fixpoint_table(PPId, ReuseAs, !Table).

sr_fixpoint_table_get_short_description(PPId, Table) = Descr :-
    ( fixpoint_table.is_recursive(Table) -> Rec = "(r)" ; Rec = "(-)"),
    ( As = sr_fixpoint_table_get_final_as_semidet(PPId, Table) ->
        Descr0 = reuse_as_short_description(As)
    ;
        Descr0 = "-"
    ), 
    Descr = Descr0 ++ " " ++ Rec.

sr_fixpoint_table_get_final_as(PPId, T) =
    get_from_fixpoint_table_final(PPId, T).

sr_fixpoint_table_get_final_as_semidet(PPId, T) =
    get_from_fixpoint_table_final_semidet(PPId, T).
    
%------------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.indirect.m".

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
