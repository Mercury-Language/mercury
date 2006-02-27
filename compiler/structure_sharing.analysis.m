%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: structure_sharing.analysis.m
% Main authors: nancy

% Implementation of the structure sharing analysis needed for compile-time
% garbage collection (CTGC).

%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.analysis.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred structure_sharing_analysis(module_info::in, module_info::out,
    sharing_as_table::out, io::di, io::uo) is det.

:- pred write_pred_sharing_info(module_info::in, pred_id::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.liveness.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.ctgc.fixpoint_table.
:- import_module transform_hlds.ctgc.util.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

structure_sharing_analysis(!ModuleInfo, SharingTable, !IO):-
    % preliminary step:
    % annotate the liveness (as in liveness.m)
    annotate_liveness(!ModuleInfo, !IO),
    % load all structure sharing information present in the HLDS.
    load_structure_sharing_table(!.ModuleInfo, SharingTable0),

    % analysis
    sharing_analysis(!ModuleInfo, SharingTable0, SharingTable, !IO).

%-----------------------------------------------------------------------------%
%
% Preliminary steps
%

:- pred load_structure_sharing_table(module_info::in, sharing_as_table::out)
    is det.

load_structure_sharing_table(ModuleInfo, SharingTable) :-
    module_info_predids(ModuleInfo, PredIds),
    list.foldl(load_structure_sharing_table_2(ModuleInfo), PredIds,
        sharing_as_table_init, SharingTable).

:- pred load_structure_sharing_table_2(module_info::in, pred_id::in,
    sharing_as_table::in, sharing_as_table::out) is det.

load_structure_sharing_table_2(ModuleInfo, PredId, !SharingTable) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    list.foldl(load_structure_sharing_table_3(ModuleInfo, PredId),
        pred_info_procids(PredInfo), !SharingTable).

:- pred load_structure_sharing_table_3(module_info::in, pred_id::in,
    proc_id::in, sharing_as_table::in, sharing_as_table::out) is det.

load_structure_sharing_table_3(ModuleInfo, PredId, ProcId, !SharingTable):-
    module_info_proc_info(ModuleInfo, PredId, ProcId, ProcInfo),
    proc_info_get_structure_sharing(ProcInfo, MaybePublicSharing),
    (
        MaybePublicSharing = yes(PublicSharing),
        sharing_as_table_set(proc(PredId, ProcId),
            from_structure_sharing_domain(PublicSharing), !SharingTable)
    ;
        MaybePublicSharing = no
    ).

    % Annotate the HLDS with pre-birth and post-death information, as
    % used by the liveness pass (liveness.m). This information is used to
    % eliminate useless sharing pairs during sharing analysis.
    %
:- pred annotate_liveness(module_info::in, module_info::out, io::di,
    io::uo) is det.

annotate_liveness(!ModuleInfo, !IO):-
    process_all_nonimported_procs(update_proc_io(detect_liveness_proc),
        !ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

:- pred sharing_analysis(module_info::in, module_info::out,
    sharing_as_table::in, sharing_as_table::out, io::di, io::uo) is det.

sharing_analysis(!ModuleInfo, !SharingTable, !IO):-
    % Perform the analysis based on the strongly connected components.
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_get_maybe_dependency_info(!.ModuleInfo, MaybeDepInfo),
    (
        MaybeDepInfo = yes(DepInfo),
        hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
        list.foldl2(analyse_scc(!.ModuleInfo), SCCs, !SharingTable, !IO)
    ;
        MaybeDepInfo = no,
        unexpected(this_file, "No dependency information.")
    ),
    %
    % Record the sharing results in the HLDS.
    %    
    map.foldl(save_sharing_in_module_info, !.SharingTable, !ModuleInfo).

:- pred save_sharing_in_module_info(pred_proc_id::in, sharing_as::in,
    module_info::in, module_info::out) is det.

save_sharing_in_module_info(PPId, SharingAs, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo0, ProcInfo0),
    proc_info_set_structure_sharing(to_structure_sharing_domain(SharingAs),
        ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo0, ProcInfo, !ModuleInfo).

:- pred analyse_scc(module_info::in, list(pred_proc_id)::in,
    sharing_as_table::in, sharing_as_table::out, io::di, io::uo) is det.

analyse_scc(ModuleInfo, SCC, !SharingTable, !IO):-
    ( preds_requiring_no_analysis(ModuleInfo, SCC) ->
        true
    ;
        analyse_scc_until_fixpoint(ModuleInfo, SCC, !.SharingTable,
            ss_fixpoint_table_init(SCC), FixpointTable, !IO),
        list.foldl(update_sharing_in_table(FixpointTable), SCC, !SharingTable)
    ).

:- pred analyse_scc_until_fixpoint(module_info::in, list(pred_proc_id)::in,
    sharing_as_table::in, ss_fixpoint_table::in, ss_fixpoint_table::out,
    io::di, io::uo) is det.

analyse_scc_until_fixpoint(ModuleInfo, SCC, SharingTable,
        !FixpointTable, !IO) :-
    list.foldl2(analyse_pred_proc(ModuleInfo, SharingTable), SCC,
        !FixpointTable, !IO),
    ( ss_fixpoint_table_stable(!.FixpointTable) ->
        true
    ;
        ss_fixpoint_table_new_run(!FixpointTable),
        analyse_scc_until_fixpoint(ModuleInfo, SCC, SharingTable,
            !FixpointTable, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Perform structure sharing analysis on a procedure
%

:- pred analyse_pred_proc(module_info::in, sharing_as_table::in,
    pred_proc_id::in, ss_fixpoint_table::in, ss_fixpoint_table::out,
    io::di, io::uo) is det.

analyse_pred_proc(ModuleInfo, SharingTable, PPId, !FixpointTable, !IO) :-
    % Collect relevant compiler options.
    globals.io_lookup_bool_option(very_verbose, Verbose, !IO),
    globals.io_lookup_int_option(structure_sharing_widening, WideningLimit,
        !IO),

    % Collect relevant procedure information.
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    PPId = proc(PredId, ProcId),
    proc_info_headvars(ProcInfo, HeadVars),

    % Write progress message for the start of analysing current procedure.
    Run = ss_fixpoint_table_which_run(!.FixpointTable),
    TabledAsDescr = ss_fixpoint_table_get_short_description(PPId,
        !.FixpointTable),
    passes_aux.write_proc_progress_message(
        "% Sharing analysis (run " ++ string.int_to_string(Run) ++ ") ",
        PredId, ProcId, ModuleInfo, !IO),

    % In some cases the sharing can be predicted to be bottom, in which
    % case a full sharing analysis is not needed.
    Sharing0 = structure_sharing.domain.init,
    (
        bottom_sharing_is_safe_approximation(ModuleInfo, ProcInfo)
    ->
        maybe_write_string(Verbose, "\t\t: bottom predicted", !IO),
        Sharing = Sharing0
    ;
        % Start analysis.
        proc_info_goal(ProcInfo, Goal),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Goal,
            !FixpointTable, Sharing0, Sharing1, !IO),
        FullAsDescr = short_description(Sharing1),

        structure_sharing.domain.project(HeadVars, Sharing1, Sharing2),
        ProjAsDescr = short_description(Sharing2),

        structure_sharing.domain.apply_widening(ModuleInfo, ProcInfo,
           WideningLimit, WideningDone, Sharing2, Sharing3),
        (
            WideningDone = yes,
            WidenAsDescr = short_description(Sharing3)
        ;
            WideningDone = no,
            WidenAsDescr = "-"
        ),

        maybe_write_string(Verbose, "\t\t: " ++
            TabledAsDescr ++ "->" ++
            FullAsDescr ++ "/" ++
            ProjAsDescr ++ "/" ++
            WidenAsDescr, !IO),

        Sharing = Sharing3
    ),
    ss_fixpoint_table_new_as(ModuleInfo, ProcInfo, PPId, Sharing,
        !FixpointTable),

    maybe_write_string(Verbose, "\t\t (ft = " ++
        ss_fixpoint_table_description(!.FixpointTable) ++
        ")\n", !IO).

:- pred analyse_goal(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, hlds_goal::in, ss_fixpoint_table::in,
    ss_fixpoint_table::out, sharing_as::in, sharing_as::out,
    io::di, io::uo) is det.

analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Goal,
        !FixpointTable, !SharingAs, !IO) :-
    Goal = GoalExpr - GoalInfo,
    (
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj,
            list.foldl3(analyse_goal(ModuleInfo, PredInfo, ProcInfo,
                SharingTable), Goals, !FixpointTable, !SharingAs, !IO)
        ;
            ConjType = parallel_conj,
            goal_info_get_context(GoalInfo, Context),
            context_to_string(Context, ContextString),
            !:SharingAs = top_sharing("par_conj (" ++ ContextString ++ ")",
                !.SharingAs)
        )
    ;
        GoalExpr = call(CalleePredId, CalleeProcId, CalleeArgs, _, _, _),
        CalleePPId = proc(CalleePredId, CalleeProcId),
        lookup_sharing(ModuleInfo, SharingTable, CalleePPId,
            !FixpointTable, CalleeSharing),

        % Rename
        proc_info_vartypes(ProcInfo, AllTypes),
        list.map(map.lookup(AllTypes), CalleeArgs, ActualTypes),

        pred_info_typevarset(PredInfo, ActualTVarset),

        rename_using_module_info(ModuleInfo, CalleePPId, CalleeArgs,
            ActualTypes, ActualTVarset, CalleeSharing, RenamedSharing),

        % Combine
        !:SharingAs = comb(ModuleInfo, ProcInfo, RenamedSharing, !.SharingAs)
    ;
        GoalExpr = generic_call(_GenDetails, _, _, _),
        goal_info_get_context(GoalInfo, Context),
        context_to_string(Context, ContextString),
        !:SharingAs = top_sharing("generic call (" ++
            ContextString ++ ")", !.SharingAs)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl3(analyse_case(ModuleInfo, PredInfo, ProcInfo,
            SharingTable, !.SharingAs),
            Cases, !FixpointTable,
            structure_sharing.domain.init, !:SharingAs,
            !IO)
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        !:SharingAs = add(ModuleInfo, ProcInfo, Unification, GoalInfo,
            !.SharingAs)
    ;
        GoalExpr = disj(Goals),
        SharingBase = !.SharingAs,
        AnalyseGoal = ( pred(G::in,
                FP0::in, FP::out, Sh0::in, Sh::out,
                IO0::di, IO1::uo) is det :-
            (
                analyse_goal(ModuleInfo, PredInfo, ProcInfo,
                    SharingTable, G, FP0, FP,
                    SharingBase, Sh1, IO0, IO1),
                Sh = least_upper_bound(ModuleInfo, ProcInfo, Sh0, Sh1)
            )),
        list.foldl3(AnalyseGoal, Goals, !FixpointTable,
            structure_sharing.domain.init, !:SharingAs, !IO)
    ;
        GoalExpr = not(_Goal)
        % XXX Check theory, but a negated goal can not create bindings,
        % hence it also can not create additional sharing.
    ;
        GoalExpr = scope(_, _),
        % XXX Check theory, check meaing of "scope/2" goal.
        goal_info_get_context(GoalInfo, Context),
        context_to_string(Context, ContextString),
        !:SharingAs = top_sharing("scope (" ++ ContextString ++ ")",
            !.SharingAs)
    ;
        GoalExpr = if_then_else(_, IfGoal, ThenGoal, ElseGoal),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            IfGoal, !FixpointTable, !.SharingAs, IfSharingAs, !IO),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            ThenGoal, !FixpointTable, IfSharingAs, ThenSharingAs, !IO),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            ElseGoal, !FixpointTable, !.SharingAs, ElseSharingAs, !IO),
        !:SharingAs = least_upper_bound(ModuleInfo, ProcInfo,
            ThenSharingAs, ElseSharingAs)
    ;
        GoalExpr = foreign_proc(_Attrs, _ForeignPredId, _ForeignProcId,
            _ForeignArgs, _, _),
        % XXX User annotated structure sharing information is not yet
        % supported.
        goal_info_get_context(GoalInfo, Context),
        context_to_string(Context, ContextString),
        !:SharingAs = top_sharing("foreign_proc not handles yet ("
            ++ ContextString ++ ")", !.SharingAs)
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file, "analyse_goal: shorthand goal.")
    ).

:- pred analyse_case(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, sharing_as::in, case::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    sharing_as::in, sharing_as::out, io::di, io::uo) is det.

analyse_case(ModuleInfo, PredInfo, ProcInfo, SharingTable, Sharing0,
    Case, !FixpointTable, !Sharing, !IO) :-
    Case = case(_, Goal),
    analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Goal,
        !FixpointTable, Sharing0, CaseSharing, !IO),
    !:Sharing = least_upper_bound(ModuleInfo, ProcInfo, !.Sharing,
        CaseSharing).

%-----------------------------------------------------------------------------%

    % Lookup the sharing information of a procedure identified by its
    % pred_proc_id.
    % 1 - first look in the fixpoint table (which may change the state
    %     of this table wrt recursiveness);
    % 2 - then look in sharing_s_table (as we might already have analysed
    %     the predicate, if defined in same module, or analysed in other
    %     imported module)
    % 3 - try to predict bottom;
    % 4 - react appropriately if the calls happen to be to
    %     * either compiler generated predicates
    %     * or predicates from builtin.m and private_builtin.m
    %
:- pred lookup_sharing(module_info::in, sharing_as_table::in, pred_proc_id::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out, sharing_as::out) is det.

lookup_sharing(ModuleInfo, SharingTable, PPId, !FixpointTable, SharingAs) :-
    (
        % 1 -- check fixpoint table
        ss_fixpoint_table_get_as(PPId, SharingAs0, !FixpointTable)
    ->
        SharingAs = SharingAs0
    ;
        % 2 -- look up in SharingTable
        SharingAs0 = sharing_as_table_search(PPId, SharingTable)
    ->
        SharingAs = SharingAs0
    ;
        % 3 -- predict bottom sharing
        %
        % If it is neither in the fixpoint table, nor in the sharing
        % table, then this means that we have never analysed the called
        % procedure, yet in some cases we can still simply predict that
        % the sharing the called procedure creates is bottom.
        predict_called_pred_is_bottom(ModuleInfo, PPId)
    ->
        SharingAs = structure_sharing.domain.init
    ;
        % 4 -- use top-sharing with appropriate message.
        SharingAs = top_sharing_not_found(ModuleInfo, PPId)
    ).

:- pred predict_called_pred_is_bottom(module_info::in, pred_proc_id::in)
    is semidet.

predict_called_pred_is_bottom(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    (
        % 1. inferred determinism is erroneous/failure.
        proc_info_inferred_determinism(ProcInfo, Determinism),
        (
            Determinism = erroneous
        ;
            Determinism = failure
        )
    ;
        % 2. bottom_sharing_is_safe_approximation
        bottom_sharing_is_safe_approximation(ModuleInfo, ProcInfo)
    ;
        % 3. call to builtin/private_builtin procedures, namely
        % "unify", "index" or "compare".
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),
        (
            special_pred_name_arity(_, PredName, _, PredArity)
        ;
            special_pred_name_arity(_, _, PredName, PredArity)
        )
    ;
        % 4. (XXX UNSAFE!! To verify) any call to private_builtin and builtin
        % procedures.
        PredModule = pred_info_module(PredInfo),
        (
            mercury_private_builtin_module(PredModule)
        ;
            mercury_public_builtin_module(PredModule)
        )
    ).

:- func top_sharing_not_found(module_info, pred_proc_id) = sharing_as.

top_sharing_not_found(ModuleInfo, PPId) = TopSharing :-
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, _),
    PPId = proc(PredId, ProcId),
    PredModuleName = pred_info_module(PredInfo),

    TopSharing = top_sharing("Lookup sharing failed for " ++
        sym_name_to_escaped_string(PredModuleName) ++ "." ++
        pred_info_name(PredInfo) ++ "/" ++
        int_to_string(pred_info_orig_arity(PredInfo)) ++ " (id = " ++
        int_to_string(pred_id_to_int(PredId)) ++ "," ++
        int_to_string(proc_id_to_int(ProcId))).

%-----------------------------------------------------------------------------%

    % Succeeds if the sharing of a procedure can safely be approximated by
    % "bottom", simply by looking at the modes and types of the arguments.
    %
:- pred bottom_sharing_is_safe_approximation(module_info::in,
    proc_info::in) is semidet.

bottom_sharing_is_safe_approximation(ModuleInfo, ProcInfo):-
    proc_info_headvars(ProcInfo, HeadVars),
    proc_info_argmodes(ProcInfo, Modes),
    proc_info_vartypes(ProcInfo, VarTypes),
    list.map(map.lookup(VarTypes), HeadVars, Types),

    ModeTypePairs = assoc_list.from_corresponding_lists(Modes, Types),

    Test = (pred(Pair::in) is semidet :-
        Pair = Mode - Type,

        % mode is not unique nor clobbered.
        mode_get_insts(ModuleInfo, Mode, _LeftInst, RightInst),
        \+ inst_is_unique(ModuleInfo, RightInst),
        \+ inst_is_clobbered(ModuleInfo, RightInst),

        % mode is output.
        mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
        ArgMode = top_out,

        % type is not primitive
        \+ type_is_atomic(Type, ModuleInfo)
    ),
    list.filter(Test, ModeTypePairs, []).

%-----------------------------------------------------------------------------%

:- pred update_sharing_in_table(ss_fixpoint_table::in, pred_proc_id::in,
    sharing_as_table::in, sharing_as_table::out) is det.

update_sharing_in_table(FixpointTable, PPId, !SharingTable):-
    sharing_as_table_set(PPId,
        ss_fixpoint_table_get_final_as(PPId, FixpointTable),
        !SharingTable).

%-----------------------------------------------------------------------------%
%
% Structure sharing fixpoint table.
%

:- type ss_fixpoint_table == fixpoint_table(pred_proc_id, sharing_as).

    % Initialise the fixpoint table for the given set of pred_proc_id's.
    %
:- func ss_fixpoint_table_init(list(pred_proc_id)) = ss_fixpoint_table.

    % Add the results of a new analysis pass to the already existing
    % fixpoint table.
    %
:- pred ss_fixpoint_table_new_run(ss_fixpoint_table::in,
    ss_fixpoint_table::out) is det.

    % The fixpoint table keeps track of the number of analysis passes. This
    % predicate returns this number.
    %
:- func ss_fixpoint_table_which_run(ss_fixpoint_table) = int.

    % A fixpoint is reached if all entries in the table are stable,
    % i.e. haven't been modified by the last analysis pass.
    %
:- pred ss_fixpoint_table_stable(ss_fixpoint_table::in) is semidet.

    % Give a string description of the state of the fixpoint table.
    %
:- func ss_fixpoint_table_description(ss_fixpoint_table) = string.

    % Enter the newly computed structure sharing description for a given
    % procedure.  If the description is different from the one that was
    % already stored for that procedure, the stability of the fixpoint
    % table is set to "unstable".
    % Software error if the procedure is not in the fixpoint table.
    %
:- pred ss_fixpoint_table_new_as(module_info::in, proc_info::in,
    pred_proc_id::in, sharing_as::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out) is det.

    % Retrieve the structure sharing description for a given pred_proc_id.
    %
    % If the id is part of the fixpoint table, but does not yet record any
    % sharing information about that pred_proc_id, then this means that the
    % set of pred_proc_id's to which the fixpoint table relates is mutually
    % recursive, hence the table is characterised as recursive.
    %
    % If the id is not part of the fixpoint table: fail.
    %
:- pred ss_fixpoint_table_get_as(pred_proc_id::in, sharing_as::out,
    ss_fixpoint_table::in, ss_fixpoint_table::out) is semidet.

:- func ss_fixpoint_table_get_short_description(pred_proc_id,
    ss_fixpoint_table) = string.

    % Retreive the structure sharing information without changing the
    % table. To be used after fixpoint has been reached.
    % Software error if the procedure is not in the table.
    %
:- func ss_fixpoint_table_get_final_as(pred_proc_id,
    ss_fixpoint_table) = sharing_as.

    % Same as ss_fixpoint_table_get_final_as, yet fails instead of aborting
    % if the procedure is not in the table.
    %
:- func ss_fixpoint_table_get_final_as_semidet(pred_proc_id,
    ss_fixpoint_table) = sharing_as is semidet.

%-----------------------------------------------------------------------------%

:- func wrapped_init(pred_proc_id) = sharing_as.

wrapped_init(_Id) = structure_sharing.domain.init.

ss_fixpoint_table_init(Keys) = fixpoint_table.init(wrapped_init, Keys).

ss_fixpoint_table_new_run(!Table) :-
    fixpoint_table.new_run(!Table).

ss_fixpoint_table_which_run(Tin) = fixpoint_table.which_run(Tin).

ss_fixpoint_table_stable(Table) :- fixpoint_table.fixpoint_reached(Table).

ss_fixpoint_table_description(Table) = fixpoint_table.description(Table).

ss_fixpoint_table_new_as(ModuleInfo, ProcInfo, Id, SharingAs, !Table):-
    fixpoint_table.add(domain.is_subsumed_by(ModuleInfo, ProcInfo),
        Id, SharingAs, !Table).

ss_fixpoint_table_get_as(PPId, SharingAs, !Table) :-
    fixpoint_table.get(PPId, SharingAs, !Table).

ss_fixpoint_table_get_short_description(PPId, Table) = Descr :-
    (
        As = ss_fixpoint_table_get_final_as_semidet(PPId, Table)
    ->
        Descr = short_description(As)
    ;
        Descr = "-"
    ).

ss_fixpoint_table_get_final_as(PPId, T) =
    fixpoint_table.get_final(PPId, T).

ss_fixpoint_table_get_final_as_semidet(PPId, T) =
    fixpoint_table.get_final_semidet(PPId, T).

%-----------------------------------------------------------------------------%

write_pred_sharing_info(ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_import_status(PredInfo, ImportStatus),
    (
        (
            ImportStatus = exported
        ;
            ImportStatus = opt_exported
        ),
        \+ is_unify_or_compare_pred(PredInfo)
    ->
        % XXX We most probably need to handle predicate produced by type
        % specialization here as well (see termination.m).    
        PredName = pred_info_name(PredInfo),
        ProcIds = pred_info_procids(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        ModuleName = pred_info_module(PredInfo),
        pred_info_procedures(PredInfo, ProcTable),
        pred_info_context(PredInfo, Context),
        SymName = qualified(ModuleName, PredName),
        pred_info_typevarset(PredInfo, TypeVarSet),
        list.foldl(
            write_proc_sharing_info(PredId, ProcTable, PredOrFunc,
                SymName, Context, TypeVarSet),
            ProcIds, !IO)
    ;
        true
    ).

:- pred write_proc_sharing_info(pred_id::in, proc_table::in,
    pred_or_func::in, sym_name::in, prog_context::in, tvarset::in,
    proc_id::in, io::di, io::uo) is det.

write_proc_sharing_info(_PredId, ProcTable, PredOrFunc, SymName,
        Context, TypeVarSet, ProcId, !IO) :-
    globals.io_lookup_bool_option(structure_sharing_analysis,
        SharingAnalysis, !IO),
    (
        SharingAnalysis = yes,
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_structure_sharing(ProcInfo, MaybeSharingAs),
        proc_info_declared_argmodes(ProcInfo, Modes),
        proc_info_varset(ProcInfo, VarSet),
        proc_info_headvars(ProcInfo, HeadVars),
        proc_info_vartypes(ProcInfo, VarTypes),
        list.map(map.lookup(VarTypes), HeadVars, HeadVarTypes),
        write_pragma_structure_sharing_info(PredOrFunc, SymName, Modes,
            Context, HeadVars, yes(VarSet), HeadVarTypes, yes(TypeVarSet),
            MaybeSharingAs, !IO)
    ;
        SharingAnalysis = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_sharing.analysis.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_sharing.analysis.
%-----------------------------------------------------------------------------%
