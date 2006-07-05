%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: structure_sharing.analysis.m.
% Main authors: nancy.
% 
% Implementation of the structure sharing analysis needed for compile-time
% garbage collection (CTGC).
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.analysis.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Perform structure sharing analysis on the procedures defined in the
    % current module. 
    %
:- pred structure_sharing_analysis(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write all the sharing information concerning the specified predicate as
    % reuse pragmas.  
    %
:- pred write_pred_sharing_info(module_info::in, pred_id::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.liveness.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.ctgc.fixpoint_table.
:- import_module transform_hlds.ctgc.structure_sharing.domain.
:- import_module transform_hlds.ctgc.util.
:- import_module transform_hlds.dependency_graph.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module term.

%-----------------------------------------------------------------------------%

structure_sharing_analysis(!ModuleInfo, !IO) :-
    % 
    % Process all the imported sharing information. 
    %
    process_imported_sharing(!ModuleInfo), 
    %
    % Annotate the HLDS with liveness information.
    %
    annotate_liveness(!ModuleInfo, !IO),
    %
    % Load all structure sharing information present in the HLDS.
    %
    LoadedSharingTable = load_structure_sharing_table(!.ModuleInfo), 
    %
    % Analyse structure sharing for the module.
    %
    sharing_analysis(!ModuleInfo, LoadedSharingTable, !IO),
    %
    % Maybe write structure sharing pragmas to .opt files.
    %
    globals.io_lookup_bool_option(make_optimization_interface,
        MakeOptInt, !IO),
    (
        MakeOptInt = yes,
        make_opt_int(!.ModuleInfo, !IO)
    ;
        MakeOptInt = no
    ).

%-----------------------------------------------------------------------------%
%
% Preliminary steps
%

    % Process the imported sharing information. 
    %
:- pred process_imported_sharing(module_info::in, module_info::out) is det.

process_imported_sharing(!ModuleInfo):-
    module_info_predids(!.ModuleInfo, PredIds), 
    list.foldl(process_imported_sharing_in_pred, PredIds, !ModuleInfo).

:- pred process_imported_sharing_in_pred(pred_id::in, module_info::in,
    module_info::out) is det.

process_imported_sharing_in_pred(PredId, !ModuleInfo) :- 
    some [!PredTable] (
        module_info_preds(!.ModuleInfo, !:PredTable), 
        PredInfo0 = !.PredTable ^ det_elem(PredId), 
        process_imported_sharing_in_procs(PredInfo0, PredInfo),
        svmap.det_update(PredId, PredInfo, !PredTable),
        module_info_set_preds(!.PredTable, !ModuleInfo)
    ).

:- pred process_imported_sharing_in_procs(pred_info::in, 
    pred_info::out) is det.

process_imported_sharing_in_procs(!PredInfo) :- 
    some [!ProcTable] (
        pred_info_get_procedures(!.PredInfo, !:ProcTable), 
        ProcIds = pred_info_procids(!.PredInfo), 
        list.foldl(process_imported_sharing_in_proc(!.PredInfo), 
            ProcIds, !ProcTable),
        pred_info_set_procedures(!.ProcTable, !PredInfo)
    ).

:- pred process_imported_sharing_in_proc(pred_info::in, proc_id::in, 
    proc_table::in, proc_table::out) is det.

process_imported_sharing_in_proc(PredInfo, ProcId, !ProcTable) :- 
    some [!ProcInfo] (
        !:ProcInfo = !.ProcTable ^ det_elem(ProcId), 
        (
            proc_info_get_imported_structure_sharing(!.ProcInfo, 
                ImpHeadVars, ImpTypes, ImpSharing)
        ->
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            pred_info_get_arg_types(PredInfo, HeadVarTypes),
            map.from_corresponding_lists(ImpHeadVars, HeadVars, VarRenaming), 
            some [!TypeSubst] (
                !:TypeSubst = map.init, 
                (
                    type_unify_list(ImpTypes, HeadVarTypes, [], !.TypeSubst,
                        TypeSubstNew)
                ->
                    !:TypeSubst = TypeSubstNew
                ;
                    true
                ),
                rename_structure_sharing_domain(VarRenaming, !.TypeSubst,
                    ImpSharing, Sharing)
            ),
            proc_info_set_structure_sharing(Sharing, !ProcInfo), 
            proc_info_reset_imported_structure_sharing(!ProcInfo),
            svmap.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Annotate the HLDS with pre-birth and post-death information, as
    % used by the liveness pass (liveness.m). This information is used to
    % eliminate useless sharing pairs during sharing analysis.
    %
:- pred annotate_liveness(module_info::in, module_info::out, io::di,
    io::uo) is det.

annotate_liveness(!ModuleInfo, !IO) :-
    process_all_nonimported_procs(update_proc_io(detect_liveness_proc),
        !ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

:- pred sharing_analysis(module_info::in, module_info::out,
    sharing_as_table::in, io::di, io::uo) is det.

sharing_analysis(!ModuleInfo, !.SharingTable, !IO) :-
    %
    % Perform a bottom-up traversal of the SCCs in the program,
    % analysing structure sharing in each one as we go.
    %
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
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_set_structure_sharing(to_structure_sharing_domain(SharingAs),
        ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

:- pred analyse_scc(module_info::in, list(pred_proc_id)::in,
    sharing_as_table::in, sharing_as_table::out, io::di, io::uo) is det.

analyse_scc(ModuleInfo, SCC, !SharingTable, !IO) :-
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
    %
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),

    % Write progress message for the start of analysing current procedure.
    %
    Run = ss_fixpoint_table_which_run(!.FixpointTable),
    TabledAsDescr = ss_fixpoint_table_get_short_description(PPId,
        !.FixpointTable),
    write_proc_progress_message(
        "% Sharing analysis (run " ++ string.int_to_string(Run) ++ ") ",
        PPId, ModuleInfo, !IO),

    % In some cases the sharing can be predicted to be bottom, in which
    % case a full sharing analysis is not needed.
    %
    some [!Sharing] (
        !:Sharing = sharing_as_init,
        ( bottom_sharing_is_safe_approximation(ModuleInfo, ProcInfo) ->
            maybe_write_string(Verbose, "\t\t: bottom predicted", !IO)
        ;
            % Start analysis.
            proc_info_get_goal(ProcInfo, Goal),
            analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Goal,
                !FixpointTable, !Sharing, !IO),
            FullAsDescr = sharing_as_short_description(!.Sharing),

            sharing_as_project(HeadVars, !Sharing),
            ProjAsDescr = sharing_as_short_description(!.Sharing),

            domain.apply_widening(ModuleInfo, ProcInfo, WideningLimit,
                WideningDone, !Sharing),
            (
                WideningDone = yes,
                WidenAsDescr = sharing_as_short_description(!.Sharing)
            ;
                WideningDone = no,
                WidenAsDescr = "-"
            ),

            maybe_write_string(Verbose, "\t\t: " ++
                TabledAsDescr ++ "->" ++
                FullAsDescr ++ "/" ++
                ProjAsDescr ++ "/" ++
                WidenAsDescr, !IO)
        ),
        ss_fixpoint_table_new_as(ModuleInfo, ProcInfo, PPId, !.Sharing,
            !FixpointTable)
    ),
    maybe_write_string(Verbose, "\t\t (ft = " ++
        ss_fixpoint_table_description(!.FixpointTable) ++ ")\n", !IO).

%-----------------------------------------------------------------------------%
%
% Structure sharing analysis of goals
%

:- pred analyse_goal(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, hlds_goal::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    sharing_as::in, sharing_as::out, io::di, io::uo) is det.

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
            !:SharingAs = sharing_as_top_sharing_accumulate(
                "par_conj (" ++ ContextString ++ ")", !.SharingAs)
        )
    ;
        GoalExpr = call(CalleePredId, CalleeProcId, CalleeArgs, _, _, _),
        CalleePPId = proc(CalleePredId, CalleeProcId),
        lookup_sharing(ModuleInfo, SharingTable, CalleePPId,
            !FixpointTable, CalleeSharing),

        % Rename
        proc_info_get_vartypes(ProcInfo, AllTypes),
        list.map(map.lookup(AllTypes), CalleeArgs, ActualTypes),
        pred_info_get_typevarset(PredInfo, ActualTVarset),
        sharing_as_rename_using_module_info(ModuleInfo, CalleePPId, CalleeArgs,
            ActualTypes, ActualTVarset, CalleeSharing, RenamedSharing),

        % Combine
        !:SharingAs = sharing_as_comb(ModuleInfo, ProcInfo,
            RenamedSharing, !.SharingAs)
    ;
        GoalExpr = generic_call(_GenDetails, _, _, _),
        goal_info_get_context(GoalInfo, Context),
        context_to_string(Context, ContextString),
        !:SharingAs = sharing_as_top_sharing_accumulate(
            "generic call (" ++ ContextString ++ ")", !.SharingAs)
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        !:SharingAs = add_unify_sharing(ModuleInfo, ProcInfo, Unification,
            GoalInfo, !.SharingAs)
    ;
        GoalExpr = disj(Goals),
        list.foldl3(
            analyse_disj(ModuleInfo, PredInfo, ProcInfo,
                SharingTable, !.SharingAs),
            Goals, !FixpointTable, sharing_as_init, !:SharingAs, !IO)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl3(
            analyse_case(ModuleInfo, PredInfo, ProcInfo,
                SharingTable, !.SharingAs),
            Cases, !FixpointTable, sharing_as_init, !:SharingAs, !IO)
    ;
        GoalExpr = not(_Goal)
        % XXX Check theory, but a negated goal can not create bindings,
        % hence it also can not create additional sharing.
    ;
        GoalExpr = scope(_, SubGoal),
        % XXX Check theory.
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, SubGoal,
            !FixpointTable, !SharingAs, !IO)
    ;
        GoalExpr = if_then_else(_, IfGoal, ThenGoal, ElseGoal),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            IfGoal, !FixpointTable, !.SharingAs, IfSharingAs, !IO),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            ThenGoal, !FixpointTable, IfSharingAs, ThenSharingAs, !IO),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
            ElseGoal, !FixpointTable, !.SharingAs, ElseSharingAs, !IO),
        !:SharingAs = sharing_as_least_upper_bound(ModuleInfo, ProcInfo,
            ThenSharingAs, ElseSharingAs)
    ;
        GoalExpr = foreign_proc(Attributes, ForeignPredId, ForeignProcId,
            _ForeignArgs, _, _),
        goal_info_get_context(GoalInfo, Context),
        !:SharingAs = add_foreign_proc_sharing(ModuleInfo, ProcInfo, 
            proc(ForeignPredId, ForeignProcId), Attributes, Context, 
            !.SharingAs)
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file, "analyse_goal: shorthand goal.")
    ).

%-----------------------------------------------------------------------------%
%
% Additional code for analysing disjuctions
%

:- pred analyse_disj(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, sharing_as::in, hlds_goal::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    sharing_as::in, sharing_as::out, io::di, io::uo) is det.

analyse_disj(ModuleInfo, PredInfo, ProcInfo, SharingTable, SharingBeforeDisj,
        Goal, !FixpointTable, !Sharing, !IO) :-
    analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Goal,
        !FixpointTable, SharingBeforeDisj, GoalSharing, !IO),
    !:Sharing = sharing_as_least_upper_bound(ModuleInfo, ProcInfo, !.Sharing,
        GoalSharing).

%-----------------------------------------------------------------------------%
%
% Additional code for analysing switches
%

:- pred analyse_case(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, sharing_as::in, case::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    sharing_as::in, sharing_as::out, io::di, io::uo) is det.

analyse_case(ModuleInfo, PredInfo, ProcInfo, SharingTable, Sharing0,
        Case, !FixpointTable, !Sharing, !IO) :-
    Case = case(_, Goal),
    analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Goal,
        !FixpointTable, Sharing0, CaseSharing, !IO),
    !:Sharing = sharing_as_least_upper_bound(ModuleInfo, ProcInfo, !.Sharing,
        CaseSharing).

%-----------------------------------------------------------------------------%
%
% Code for handling calls
%
    
    % Lookup the sharing information of a procedure identified by its
    % pred_proc_id.
    %
:- pred lookup_sharing(module_info::in, sharing_as_table::in, pred_proc_id::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out, sharing_as::out) is det.

lookup_sharing(ModuleInfo, SharingTable, PPId, !FixpointTable, SharingAs) :-
    (
        % check fixpoint table
        ss_fixpoint_table_get_as(PPId, SharingAs0, !FixpointTable)
    ->
        SharingAs = SharingAs0
    ;
        lookup_sharing_or_predict(ModuleInfo, SharingTable, PPId, SharingAs)
    ).

%-----------------------------------------------------------------------------%

:- pred update_sharing_in_table(ss_fixpoint_table::in, pred_proc_id::in,
    sharing_as_table::in, sharing_as_table::out) is det.

update_sharing_in_table(FixpointTable, PPId, !SharingTable) :-
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

    % Retrieve the structure sharing information without changing the table.
    % To be used after fixpoint has been reached.
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

wrapped_init(_Id) = sharing_as_init.

ss_fixpoint_table_init(Keys) = init_fixpoint_table(wrapped_init, Keys).

ss_fixpoint_table_new_run(!Table) :-
    fixpoint_table.new_run(!Table).

ss_fixpoint_table_which_run(Tin) = fixpoint_table.which_run(Tin).

ss_fixpoint_table_stable(Table) :-
    fixpoint_table.fixpoint_reached(Table).

ss_fixpoint_table_description(Table) = fixpoint_table.description(Table).

ss_fixpoint_table_new_as(ModuleInfo, ProcInfo, Id, SharingAs, !Table) :-
    add_to_fixpoint_table(sharing_as_is_subsumed_by(ModuleInfo, ProcInfo),
        Id, SharingAs, !Table).

ss_fixpoint_table_get_as(PPId, SharingAs, !Table) :-
    get_from_fixpoint_table(PPId, SharingAs, !Table).

ss_fixpoint_table_get_short_description(PPId, Table) = Descr :-
    ( As = ss_fixpoint_table_get_final_as_semidet(PPId, Table) ->
        Descr = sharing_as_short_description(As)
    ;
        Descr = "-"
    ).

ss_fixpoint_table_get_final_as(PPId, T) =
    get_from_fixpoint_table_final(PPId, T).

ss_fixpoint_table_get_final_as_semidet(PPId, T) =
    get_from_fixpoint_table_final_semidet(PPId, T).

%-----------------------------------------------------------------------------%
%
% Code for writing out optimization interfaces
%

:- pred make_opt_int(module_info::in, io::di, io::uo) is det.

make_opt_int(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".opt.tmp", no, OptFileName, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Appending structure_sharing pragmas to ",
        !IO),
    maybe_write_string(Verbose, add_quotes(OptFileName), !IO),
    maybe_write_string(Verbose, "...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_predids(ModuleInfo, PredIds),   
        list.foldl(write_pred_sharing_info(ModuleInfo), PredIds, !IO),
        io.set_output_stream(OldStream, _, !IO),
        io.close_output(OptFile, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        OptFileRes = error(IOError),
        maybe_write_string(Verbose, " failed!\n", !IO),
        io.error_message(IOError, IOErrorMessage),
        io.write_strings(["Error opening file `",
            OptFileName, "' for output: ", IOErrorMessage], !IO),
        io.set_exit_status(1, !IO)
    ).  

%-----------------------------------------------------------------------------%
%
% Code for writing out structure_sharing pragmas
%

write_pred_sharing_info(ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, ImportStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    (
        (
            ImportStatus = exported
        ;
            ImportStatus = opt_exported
        ),
        \+ is_unify_or_compare_pred(PredInfo),

        % XXX These should be allowed, but the predicate declaration for the
        % specialized predicate is not produced before the structure_sharing
        % pragmas are read in, resulting in an undefined predicate error.
        \+ set.member(PredId, TypeSpecForcePreds)
    ->
        PredName = pred_info_name(PredInfo),
        ProcIds = pred_info_procids(PredInfo),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        ModuleName = pred_info_module(PredInfo),
        pred_info_get_procedures(PredInfo, ProcTable),
        pred_info_context(PredInfo, Context),
        SymName = qualified(ModuleName, PredName),
        pred_info_get_typevarset(PredInfo, TypeVarSet),
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

write_proc_sharing_info(_PredId, ProcTable, PredOrFunc, SymName, Context,
        TypeVarSet, ProcId, !IO) :-
    globals.io_lookup_bool_option(structure_sharing_analysis,
        SharingAnalysis, !IO),
    (
        SharingAnalysis = yes,
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_structure_sharing(ProcInfo, MaybeSharingAs),
        proc_info_declared_argmodes(ProcInfo, Modes),
        proc_info_get_varset(ProcInfo, VarSet),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_vartypes(ProcInfo, VarTypes),
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
