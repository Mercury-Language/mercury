%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.analysis.m.
% Main authors: nancy.
%
% Implementation of the structure reuse analysis (compile-time garbage
% collection system): each procedure is analysed to see whether some
% of the terms it manipulates become garbage thus making it possible
% to reuse that garbage straight away for creating new terms.
%
% Structure reuse is broken up into three phases: 
%   * the direct reuse analysis (structure_reuse.direct.m) 
%   * the indirect analysis (structure_reuse.indirect.m)
%   * and the generation of the optimised procedures.
% 
% The following example shows instances of direct and indirect reuse: 
%
% list.append(H1, H2, H3) :-
%   (
%       H1 => [],
%       H3 := H2
%   ;
%           % Cell H1 dies provided some condition about the
%           % structure sharing of H1 is true.  A deconstruction
%           % generating a dead cell, followed by a
%           % construction reusing that cell, is called a direct
%           % reuse. 
%       H1 => [X | Xs],
%
%           % If the condition about the structure sharing of H1
%           % is true then we can call the version of list.append 
%           % which does reuse. Calling the optimised version here leads
%           % to a new condition to be met by the headvars of any
%           % call to the resulting optimised version of append.
%           % This is an indirect reuse.
%       list.append(Xs, H2, Zs),
%
%           % Reuse the dead cell H1.  This is a direct reuse.
%       H3 <= [X | Zs]
%   ).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io. 

%-----------------------------------------------------------------------------%

    % Perform structure reuse analysis on the procedures defined in the
    % current module. 
    %
:- pred structure_reuse_analysis(module_info::in, module_info::out, 
    io::di, io::uo) is det.

    % Write all the reuse information concerning the specified predicate as
    % reuse pragmas.  
    %
:- pred write_pred_reuse_info(module_info::in, pred_id::in, 
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type structure_reuse_call.
:- type structure_reuse_answer.
:- type structure_reuse_func_info.

:- instance analysis(structure_reuse_func_info, structure_reuse_call,   
    structure_reuse_answer).

:- instance call_pattern(structure_reuse_func_info, structure_reuse_call).
:- instance partial_order(structure_reuse_func_info, structure_reuse_call).
:- instance to_string(structure_reuse_call).

:- instance answer_pattern(structure_reuse_func_info, structure_reuse_answer).
:- instance partial_order(structure_reuse_func_info, structure_reuse_answer).
:- instance to_string(structure_reuse_answer).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.ctgc.structure_reuse.direct.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_reuse.indirect.
:- import_module transform_hlds.ctgc.structure_reuse.lbu.
:- import_module transform_hlds.ctgc.structure_reuse.lfu.
:- import_module transform_hlds.ctgc.structure_reuse.versions.
:- import_module transform_hlds.ctgc.structure_sharing.domain.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.

%-----------------------------------------------------------------------------%

structure_reuse_analysis(!ModuleInfo, !IO):- 
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),

    % Process all imported reuse information.
    globals.io_lookup_bool_option(intermodule_analysis, IntermodAnalysis, !IO),
    (
        IntermodAnalysis = yes,
        process_intermod_analysis_imported_reuse(!ModuleInfo)
    ;
        IntermodAnalysis = no,
        process_imported_reuse(!ModuleInfo)
    ),

    % Load all available structure sharing information into a sharing table.
    SharingTable = load_structure_sharing_table(!.ModuleInfo),

    % Load all the available reuse information into a reuse table.
    ReuseTable0 = load_structure_reuse_table(!.ModuleInfo), 
    InitialReuseTable = ReuseTable0,
   
    % Pre-annotate each of the goals with "Local Forward Use" and
    % "Local Backward Use" information, and fill in all the goal_path slots
    % as well. 

    maybe_write_string(VeryVerbose, "% Annotating in use information...", !IO), 
    process_all_nonimported_procs(update_proc_io(annotate_in_use_information),
         !ModuleInfo, !IO),
    maybe_write_string(VeryVerbose, "done.\n", !IO),

    % Determine information about possible direct reuses.
    maybe_write_string(VeryVerbose, "% Direct reuse...\n", !IO), 
    direct_reuse_pass(SharingTable, !ModuleInfo, ReuseTable0, ReuseTable1, !IO),
    maybe_write_string(VeryVerbose, "% Direct reuse: done.\n", !IO),
    reuse_as_table_maybe_dump(VeryVerbose, !.ModuleInfo, ReuseTable1, !IO),

    % Determine information about possible indirect reuses.
    maybe_write_string(VeryVerbose, "% Indirect reuse...\n", !IO), 
    indirect_reuse_pass(SharingTable, !ModuleInfo, ReuseTable1, ReuseTable2, 
       DepProcs, !IO), 
    maybe_write_string(VeryVerbose, "% Indirect reuse: done.\n", !IO),
    reuse_as_table_maybe_dump(VeryVerbose, !.ModuleInfo, ReuseTable2, !IO),

    % For every procedure that has some potential (conditional) reuse (either 
    % direct or indirect), create a new procedure that actually implements
    % that reuse. 
    create_reuse_procedures(ReuseTable2, !ModuleInfo, !IO),
    FinalReuseTable = ReuseTable2,

    % Create forwarding procedures for procedures which we thought had
    % conditional reuse when making the `.opt' file, but with further
    % information (say, from `.trans_opt' files) we decide has no reuse
    % opportunities. Otherwise other modules may contain references to
    % reuse versions of procedures which we never produce.
    create_forwarding_procedures(InitialReuseTable, FinalReuseTable,
        !ModuleInfo),

    % Record the results of the reuse table into the HLDS.
    map.foldl(save_reuse_in_module_info, ReuseTable2, !ModuleInfo),

    % Only write structure reuse pragmas to `.opt' files for
    % `--intermodule-optimization' not `--intermodule-analysis'.
    globals.io_lookup_bool_option(make_optimization_interface, MakeOptInt,
        !IO),
    (
        MakeOptInt = yes,
        IntermodAnalysis = no
    ->
        make_opt_int(!ModuleInfo, !IO)
    ;
        true
    ),

    % If making a `.analysis' file, record structure sharing results, analysis
    % dependencies, assumed answers and requests in the analysis framework.
    globals.io_lookup_bool_option(make_analysis_registry, MakeAnalysisRegistry,
        !IO),
    (
        MakeAnalysisRegistry = yes,
        some [!AnalysisInfo] (
            module_info_get_analysis_info(!.ModuleInfo, !:AnalysisInfo),
            map.foldl(record_structure_reuse_results(!.ModuleInfo),
                ReuseTable2, !AnalysisInfo),
            list.foldl(handle_dep_procs(!.ModuleInfo), DepProcs,
                !AnalysisInfo),
            module_info_set_analysis_info(!.AnalysisInfo, !ModuleInfo)
        )
    ;
        MakeAnalysisRegistry = no
    ).

    % Output some profiling information.
    % XXX TO DO!
    % profiling(!.ModuleInfo, ReuseTable3).

%-----------------------------------------------------------------------------%

:- pred create_forwarding_procedures(reuse_as_table::in, reuse_as_table::in,
    module_info::in, module_info::out) is det.

create_forwarding_procedures(InitialReuseTable, FinalReuseTable,
        !ModuleInfo) :-
    map.foldl(create_forwarding_procedures_2(FinalReuseTable),
        InitialReuseTable, !ModuleInfo).

:- pred create_forwarding_procedures_2(reuse_as_table::in, pred_proc_id::in,
    reuse_as_and_status::in, module_info::in, module_info::out) is det.

create_forwarding_procedures_2(FinalReuseTable, PPId,
        reuse_as_and_status(InitialReuseAs, _), !ModuleInfo) :-
    PPId = proc(PredId, _),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, ImportStatus),
    (
        reuse_as_conditional_reuses(InitialReuseAs),
        status_defined_in_this_module(ImportStatus) = yes,
        map.search(FinalReuseTable, PPId, FinalReuseAs_Status),
        FinalReuseAs_Status = reuse_as_and_status(FinalReuseAs, _),
        reuse_as_no_reuses(FinalReuseAs)
    ->
        create_fake_reuse_procedure(PPId, !ModuleInfo)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Process all the reuse annotation from imported predicates.
    %
:- pred process_imported_reuse(module_info::in, module_info::out) is det.

process_imported_reuse(!ModuleInfo):-
    module_info_predids(PredIds, !ModuleInfo), 
    list.foldl(process_imported_reuse_in_pred, PredIds, !ModuleInfo).

:- pred process_imported_reuse_in_pred(pred_id::in, module_info::in,
    module_info::out) is det.

process_imported_reuse_in_pred(PredId, !ModuleInfo) :- 
    some [!PredTable] (
        module_info_preds(!.ModuleInfo, !:PredTable), 
        PredInfo0 = !.PredTable ^ det_elem(PredId), 
        process_imported_reuse_in_procs(PredInfo0, PredInfo),
        svmap.det_update(PredId, PredInfo, !PredTable),
        module_info_set_preds(!.PredTable, !ModuleInfo)
    ).

:- pred process_imported_reuse_in_procs(pred_info::in, 
    pred_info::out) is det.

process_imported_reuse_in_procs(!PredInfo) :- 
    some [!ProcTable] (
        pred_info_get_procedures(!.PredInfo, !:ProcTable), 
        ProcIds = pred_info_procids(!.PredInfo), 
        list.foldl(process_imported_reuse_in_proc(!.PredInfo), 
            ProcIds, !ProcTable),
        pred_info_set_procedures(!.ProcTable, !PredInfo)
    ).

:- pred process_imported_reuse_in_proc(pred_info::in, proc_id::in, 
    proc_table::in, proc_table::out) is det.

process_imported_reuse_in_proc(PredInfo, ProcId, !ProcTable) :- 
    some [!ProcInfo] (
        !:ProcInfo = !.ProcTable ^ det_elem(ProcId), 
        (
            proc_info_get_imported_structure_reuse(!.ProcInfo, 
                ImpHeadVars, ImpTypes, ImpReuse)
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
                rename_structure_reuse_domain(VarRenaming, !.TypeSubst,
                    ImpReuse, Reuse)
            ),
            % Optimality does not apply to `--intermodule-optimisation'
            % system, only `--intermodule-analysis'.
            proc_info_set_structure_reuse(
                structure_reuse_domain_and_status(Reuse, optimal), !ProcInfo), 
            proc_info_reset_imported_structure_reuse(!ProcInfo),
            svmap.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Process the intermodule imported sharing information from the analysis
    % framework
    %
:- pred process_intermod_analysis_imported_reuse(module_info::in,
    module_info::out) is det.

process_intermod_analysis_imported_reuse(!ModuleInfo):-
    module_info_predids(PredIds, !ModuleInfo), 
    list.foldl(process_intermod_analysis_imported_reuse_in_pred, PredIds,
        !ModuleInfo).

:- pred process_intermod_analysis_imported_reuse_in_pred(pred_id::in,
    module_info::in, module_info::out) is det.

process_intermod_analysis_imported_reuse_in_pred(PredId, !ModuleInfo) :- 
    some [!PredTable] (
        module_info_preds(!.ModuleInfo, !:PredTable), 
        PredInfo0 = !.PredTable ^ det_elem(PredId), 
        pred_info_get_import_status(PredInfo0, ImportStatus),
        ( ImportStatus = status_imported(_) ->
            module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo),
            process_intermod_analysis_imported_reuse_in_procs(!.ModuleInfo,
                AnalysisInfo, PredId, PredInfo0, PredInfo),
            svmap.det_update(PredId, PredInfo, !PredTable),
            module_info_set_preds(!.PredTable, !ModuleInfo)
        ;
            true
        )
    ).

:- pred process_intermod_analysis_imported_reuse_in_procs(module_info::in,
    analysis_info::in, pred_id::in, pred_info::in, pred_info::out) is det.

process_intermod_analysis_imported_reuse_in_procs(ModuleInfo, AnalysisInfo,
        PredId, !PredInfo) :- 
    some [!ProcTable] (
        pred_info_get_procedures(!.PredInfo, !:ProcTable), 
        ProcIds = pred_info_procids(!.PredInfo), 
        list.foldl(
            process_intermod_analysis_imported_reuse_in_proc(ModuleInfo,
                AnalysisInfo, PredId, !.PredInfo), 
            ProcIds, !ProcTable),
        pred_info_set_procedures(!.ProcTable, !PredInfo)
    ).

:- pred process_intermod_analysis_imported_reuse_in_proc(module_info::in,
    analysis_info::in, pred_id::in, pred_info::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

process_intermod_analysis_imported_reuse_in_proc(ModuleInfo, AnalysisInfo,
        PredId, PredInfo, ProcId, !ProcTable) :- 
    PPId = proc(PredId, ProcId),
    some [!ProcInfo] (
        !:ProcInfo = !.ProcTable ^ det_elem(ProcId), 

        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        FuncInfo = structure_reuse_func_info(ModuleInfo, !.ProcInfo),
        lookup_best_result(AnalysisInfo, ModuleName, FuncId, FuncInfo,
            structure_reuse_call, MaybeBestResult),
        (
            MaybeBestResult = yes(analysis_result(_Call, Answer,
                ResultStatus)),
            structure_reuse_answer_to_domain(PredInfo, !.ProcInfo, Answer,
                Reuse),
            proc_info_set_structure_reuse(
                structure_reuse_domain_and_status(Reuse, ResultStatus),
                !ProcInfo),
            svmap.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            MaybeBestResult = no
        )
    ).

:- pred structure_reuse_answer_to_domain(pred_info::in,
    proc_info::in, structure_reuse_answer::in, structure_reuse_domain::out)
    is det.

structure_reuse_answer_to_domain(PredInfo, ProcInfo, Answer, Reuse) :-
    (
        Answer = structure_reuse_answer_no_reuse,
        Reuse = has_no_reuse
    ;
        Answer = structure_reuse_answer_unconditional,
        Reuse = has_only_unconditional_reuse
    ;
        Answer = structure_reuse_answer_conditional(ImpHeadVars, ImpTypes,
            ImpReuseAs),
        proc_info_get_headvars(ProcInfo, HeadVars),
        pred_info_get_arg_types(PredInfo, HeadVarTypes),
        map.from_corresponding_lists(ImpHeadVars, HeadVars, VarRenaming),
        ( type_unify_list(ImpTypes, HeadVarTypes, [], map.init, TypeSubst) ->
            ImpReuseDomain = to_structure_reuse_domain(ImpReuseAs),
            rename_structure_reuse_domain(VarRenaming, TypeSubst,
                ImpReuseDomain, Reuse)
        ;
            unexpected(this_file,
                "structure_reuse_answer_to_domain: type_unify_list failed")
        )
    ).

%-----------------------------------------------------------------------------%

:- pred save_reuse_in_module_info(pred_proc_id::in, reuse_as_and_status::in,
    module_info::in, module_info::out) is det.

save_reuse_in_module_info(PPId, ReuseAs_Status, !ModuleInfo) :- 
    save_reuse_in_module_info_2(PPId, ReuseAs_Status, !ModuleInfo), 
    module_info_get_structure_reuse_map(!.ModuleInfo, ReuseMap), 
    ( map.search(ReuseMap, PPId, Result) -> 
        Result = ReusePPId - _Name, 
        save_reuse_in_module_info_2(ReusePPId, ReuseAs_Status, !ModuleInfo)
    ;
        true
    ).

:- pred save_reuse_in_module_info_2(pred_proc_id::in, reuse_as_and_status::in,
    module_info::in, module_info::out) is det.

save_reuse_in_module_info_2(PPId, ReuseAs_Status, !ModuleInfo) :- 
    ReuseAs_Status = reuse_as_and_status(ReuseAs, Status),
    ReuseDomain = to_structure_reuse_domain(ReuseAs),
    Domain_Status = structure_reuse_domain_and_status(ReuseDomain, Status),

    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_set_structure_reuse(Domain_Status, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

:- pred annotate_in_use_information(pred_id::in, proc_id::in,
    module_info::in, proc_info::in, proc_info::out, io::di, io::uo) is det.

annotate_in_use_information(_PredId, _ProcId, ModuleInfo, !ProcInfo, !IO) :- 
    forward_use_information(!ProcInfo), 
    backward_use_information(ModuleInfo, !ProcInfo),
    fill_goal_path_slots(ModuleInfo, !ProcInfo).

%-----------------------------------------------------------------------------%
%
% Code for writing out optimization interfaces
%

:- pred make_opt_int(module_info::in, module_info::out, io::di, io::uo) is det.

make_opt_int(!ModuleInfo, !IO) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    module_name_to_file_name(ModuleName, ".opt.tmp", no, OptFileName, !IO),
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Appending structure_reuse pragmas to ",
        !IO),
    maybe_write_string(Verbose, add_quotes(OptFileName), !IO),
    maybe_write_string(Verbose, "...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_predids(PredIds, !ModuleInfo),   
        list.foldl(write_pred_reuse_info(!.ModuleInfo), PredIds, !IO),
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
% Code for writing out structure_reuse pragmas
%

write_pred_reuse_info(ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredName = pred_info_name(PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    pred_info_get_procedures(PredInfo, ProcTable),
    pred_info_get_context(PredInfo, Context),
    SymName = qualified(ModuleName, PredName),
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    list.foldl(write_proc_reuse_info(ModuleInfo, PredId, PredInfo, ProcTable,
        PredOrFunc, SymName, Context, TypeVarSet), ProcIds, !IO).

:- pred write_proc_reuse_info(module_info::in, pred_id::in, pred_info::in,
    proc_table::in, pred_or_func::in, sym_name::in, prog_context::in,
    tvarset::in, proc_id::in, io::di, io::uo) is det.

write_proc_reuse_info(ModuleInfo, PredId, PredInfo, ProcTable, PredOrFunc,
        SymName, Context, TypeVarSet, ProcId, !IO) :-
    should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
        disallow_type_spec_preds, ShouldWrite),
    (
        ShouldWrite = yes,
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_structure_reuse(ProcInfo, MaybeStructureReuseDomain),
        proc_info_declared_argmodes(ProcInfo, Modes),
        proc_info_get_varset(ProcInfo, VarSet),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        list.map(map.lookup(VarTypes), HeadVars, HeadVarTypes),
        (
            MaybeStructureReuseDomain = yes(
                structure_reuse_domain_and_status(Reuse, _Status)),
            MaybeReuse = yes(Reuse)
        ;
            MaybeStructureReuseDomain = no,
            MaybeReuse = no
        ),
        write_pragma_structure_reuse_info(PredOrFunc, SymName, Modes,
            Context, HeadVars, yes(VarSet), HeadVarTypes, yes(TypeVarSet),
            MaybeReuse, !IO)
    ;
        ShouldWrite = no
    ).

%-----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework
%

:- type structure_reuse_call
    --->    structure_reuse_call.
            % Eventually we should have different call patterns.

:- type structure_reuse_answer
    --->    structure_reuse_answer_no_reuse
    ;       structure_reuse_answer_unconditional
    ;       structure_reuse_answer_conditional(
                prog_vars,
                list(mer_type),
                reuse_as
            ).

:- type structure_reuse_func_info
    --->    structure_reuse_func_info(
                module_info,
                proc_info
            ).

:- func analysis_name = string.

analysis_name = "structure_reuse".

:- instance analysis(structure_reuse_func_info, structure_reuse_call,
    structure_reuse_answer) where
[
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 1,
    preferred_fixpoint_type(_, _) = greatest_fixpoint,
    bottom(_, _) = structure_reuse_answer_no_reuse,
    ( top(_, _) = _ :-
        % We have no representation for "all possible conditions".
        unexpected(this_file, "top/2 called")
    ),
    ( get_func_info(ModuleInfo, ModuleName, FuncId, _, _, FuncInfo) :-
        func_id_to_ppid(ModuleInfo, ModuleName, FuncId, PPId),
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo)
    )
].

:- instance call_pattern(structure_reuse_func_info, structure_reuse_call)
    where [].

:- instance partial_order(structure_reuse_func_info, structure_reuse_call)
        where [
    (more_precise_than(_, _, _) :-
        semidet_fail
    ),
    equivalent(_, Call, Call)
].

:- instance to_string(structure_reuse_call) where [
    to_string(structure_reuse_call) = "",
    from_string("") = structure_reuse_call
].

:- instance answer_pattern(structure_reuse_func_info, structure_reuse_answer)
    where [].

:- instance partial_order(structure_reuse_func_info, structure_reuse_answer)
        where [

    % We deliberately have `conditional' reuse incomparable with
    % `unconditional' reuse.  If they were comparable, a caller using an
    % `conditional' answer would would only be marked `suboptimal' if that
    % answer changes to `unconditional'.  Since we don't honour the old
    % `conditional' answer by generating that version of the procedure, there
    % would be a linking error if the caller is not updated to call the
    % unconditional version.

    (more_precise_than(FuncInfo, Answer1, Answer2) :-
        (
            Answer1 = structure_reuse_answer_conditional(_, _, _),
            Answer2 = structure_reuse_answer_no_reuse
        ;
            Answer1 = structure_reuse_answer_unconditional,
            Answer2 = structure_reuse_answer_no_reuse
        ;
            Answer1 = structure_reuse_answer_conditional(_, _, ReuseAs1),
            Answer2 = structure_reuse_answer_conditional(_, _, ReuseAs2),
            % XXX can we implement this more efficiently?
            FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
            reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs1, ReuseAs2),
            not reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs2, ReuseAs1)
        )
    ),

    (equivalent(FuncInfo, Answer1, Answer2) :-
        (
            Answer1 = Answer2
        ;
            Answer1 = structure_reuse_answer_conditional(_, _, ReuseAs1),
            Answer2 = structure_reuse_answer_conditional(_, _, ReuseAs2),
            % XXX can we implement this more efficiently?
            FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
            reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs2, ReuseAs1),
            reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs1, ReuseAs2)
        )
    )
].

:- instance to_string(structure_reuse_answer) where [
    func(to_string/1) is reuse_answer_to_string,
    func(from_string/1) is reuse_answer_from_string
].

:- func reuse_answer_to_string(structure_reuse_answer) = string.

reuse_answer_to_string(Answer) = String :-
    (
        Answer = structure_reuse_answer_no_reuse,
        String = "no_reuse"
    ;
        Answer = structure_reuse_answer_unconditional,
        String = "uncond"
    ;
        Answer = structure_reuse_answer_conditional(HeadVars, Types, ReuseAs),
        ReuseDomain = to_structure_reuse_domain(ReuseAs),
        String = string({HeadVars, Types, ReuseDomain})
    ).

:- func reuse_answer_from_string(string::in) =
    (structure_reuse_answer::out) is det.

reuse_answer_from_string(String) = Answer :-
    ( String = "no_reuse" ->
        Answer = structure_reuse_answer_no_reuse
    ; String = "uncond" ->
        Answer = structure_reuse_answer_unconditional
    ;
        % XXX this is ugly.  Later we should move to writing call and answer
        % patterns in analysis files as terms rather than strings which will
        % clean this up.
        StringStop = String ++ ".",
        io.read_from_string("", StringStop, string.length(StringStop), Res,
            posn(0, 0, 0), _Posn),
        (
            Res = ok({HeadVars, Types, ReuseDomain}),
            ReuseAs = from_structure_reuse_domain(ReuseDomain),
            Answer = structure_reuse_answer_conditional(HeadVars, Types,
                ReuseAs)
        ;
            ( Res = eof
            ; Res = error(_, _)
            ),
            unexpected(this_file, "reuse_answer_from_string: " ++ String)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Additional predicates used for intermodule analysis
%

:- pred record_structure_reuse_results(module_info::in, pred_proc_id::in,
    reuse_as_and_status::in, analysis_info::in, analysis_info::out) is det.

record_structure_reuse_results(ModuleInfo, PPId, ReuseAs_Status,
        !AnalysisInfo) :-
    PPId = proc(PredId, ProcId),
    ReuseAs_Status = reuse_as_and_status(ReuseAs, Status),

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
        allow_type_spec_preds, ShouldWrite),
    (
        ShouldWrite = yes,
        ( reuse_as_no_reuses(ReuseAs) ->
            Answer = structure_reuse_answer_no_reuse
        ; reuse_as_all_unconditional_reuses(ReuseAs) ->
            Answer = structure_reuse_answer_unconditional
        ; reuse_as_conditional_reuses(ReuseAs) ->
            module_info_pred_proc_info(ModuleInfo, PPId, _PredInfo, ProcInfo),
            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_vartypes(ProcInfo, VarTypes),
            map.apply_to_list(HeadVars, VarTypes, HeadVarTypes),
            Answer = structure_reuse_answer_conditional(HeadVars, HeadVarTypes,
                ReuseAs)
        ;
            unexpected(this_file, "record_structure_reuse_results")
        ),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, structure_reuse_call, Answer, Status,
            !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

:- pred handle_dep_procs(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

handle_dep_procs(ModuleInfo, DepPPId, !AnalysisInfo) :-
    % Record that we depend on the result for the called procedure.
    module_info_get_name(ModuleInfo, ThisModuleName),
    module_name_func_id(ModuleInfo, DepPPId, DepModuleName, DepFuncId),
    Call = structure_reuse_call,
    record_dependency(ThisModuleName, analysis_name, DepModuleName, DepFuncId,
        Call, !AnalysisInfo),

    % If the called procedure didn't have an answer in the analysis registry,
    % record the assumed answer for it so that when it does get
    % analysed, it will have something to compare against.
    module_info_proc_info(ModuleInfo, DepPPId, ProcInfo),
    FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
    lookup_matching_results(!.AnalysisInfo, DepModuleName, DepFuncId, FuncInfo,
        Call, AnyResults : list(analysis_result(structure_reuse_call,
            structure_reuse_answer))),
    (
        AnyResults = [],
        Answer = bottom(FuncInfo, Call) : structure_reuse_answer,
        record_result(DepModuleName, DepFuncId, Call, Answer, suboptimal,
            !AnalysisInfo),
        % Record a request as well.
        record_request(analysis_name, DepModuleName, DepFuncId, Call,
            !AnalysisInfo)
    ;
        AnyResults = [_ | _]
    ).

%-----------------------------------------------------------------------------%

:- type allow_type_spec_preds
    --->    allow_type_spec_preds
    ;       disallow_type_spec_preds.

:- pred should_write_reuse_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, allow_type_spec_preds::in, bool::out) is det.

should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
        AllowTypeSpecPreds, ShouldWrite) :-
    (
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        \+ is_unify_or_compare_pred(PredInfo),

        % Don't write out info for reuse versions of procedures.
        pred_info_get_origin(PredInfo, PredOrigin),
        PredOrigin \= origin_transformed(transform_structure_reuse, _, _),

        (
            AllowTypeSpecPreds = allow_type_spec_preds
        ;
            AllowTypeSpecPreds = disallow_type_spec_preds,
            % XXX These should be allowed, but the predicate declaration for
            % the specialized predicate is not produced before the structure
            % reuse pragmas are read in, resulting in an undefined predicate
            % error.
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            \+ set.member(PredId, TypeSpecForcePreds)
        )
    ->
        ShouldWrite = yes
    ;
        ShouldWrite = no
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.analysis.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.analysis.
%-----------------------------------------------------------------------------%
