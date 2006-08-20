%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.goal_path.
:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.ctgc.structure_reuse.direct.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_reuse.indirect.
:- import_module transform_hlds.ctgc.structure_reuse.lbu.
:- import_module transform_hlds.ctgc.structure_reuse.lfu.
:- import_module transform_hlds.ctgc.structure_reuse.versions.
:- import_module transform_hlds.ctgc.structure_sharing.domain.

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
    process_imported_reuse(!ModuleInfo), 

    % Load all available structure sharing information into a sharing table.
    SharingTable = load_structure_sharing_table(!.ModuleInfo),

    % Load all the available reuse information into a reuse table.
    % XXX TO DO!
    ReuseTable0 = load_structure_reuse_table(!.ModuleInfo), 
   
    % Pre-annotate each of the goals with "Local Forward Use" and
    % "Local Backward Use" information, and fill in all the goal_path slots
    % as well. 

    maybe_write_string(VeryVerbose, "% Annotating in use information...", !IO), 
    process_all_nonimported_procs(update_proc_io(annotate_in_use_information),
         !ModuleInfo, !IO),
    maybe_write_string(VeryVerbose, "done.\n", !IO),

    % Determine information about possible direct reuses.
    maybe_write_string(VeryVerbose, "% Direct reuse...\n", !IO), 
    direct_reuse_pass(SharingTable, !ModuleInfo, 
        ReuseTable0, ReuseTable1, !IO),
    maybe_write_string(VeryVerbose, "% Direct reuse: done.\n", !IO),
    reuse_as_table_maybe_dump(VeryVerbose, ReuseTable1, !IO),

    % Determine information about possible indirect reuses.
    maybe_write_string(VeryVerbose, "% Indirect reuse...\n", !IO), 
    indirect_reuse_pass(SharingTable, !ModuleInfo, ReuseTable1, ReuseTable2, 
       !IO), 
    maybe_write_string(VeryVerbose, "% Indirect reuse: done.\n", !IO),
    reuse_as_table_maybe_dump(VeryVerbose, ReuseTable2, !IO),

    % For every procedure that has some potential (conditional) reuse (either 
    % direct or indirect), create a new procedure that actually implements
    % that reuse. 
    % XXX TO DO!
    create_reuse_procedures(ReuseTable2, !ModuleInfo, !IO),

    % Record the results of the reuse table into the HLDS.
    map.foldl(save_reuse_in_module_info, ReuseTable2, !ModuleInfo).
    %
    % Output some profiling information.
    % XXX TO DO!
    % profiling(!.ModuleInfo, ReuseTable3).

%-----------------------------------------------------------------------------%

    % Process all the reuse annotation from imported predicates.
    %
:- pred process_imported_reuse(module_info::in, module_info::out) is det.

process_imported_reuse(!ModuleInfo):-
    module_info_predids(!.ModuleInfo, PredIds), 
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
            proc_info_set_structure_reuse(Reuse, !ProcInfo), 
            proc_info_reset_imported_structure_reuse(!ProcInfo),
            svmap.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

:- pred save_reuse_in_module_info(pred_proc_id::in, reuse_as::in,
    module_info::in, module_info::out) is det.

save_reuse_in_module_info(PPId, ReuseAs, !ModuleInfo) :- 
    save_reuse_in_module_info_2(PPId, ReuseAs, !ModuleInfo), 
    module_info_get_structure_reuse_map(!.ModuleInfo, ReuseMap), 
    ( map.search(ReuseMap, PPId, Result) -> 
        Result = ReusePPId - _Name, 
        save_reuse_in_module_info_2(ReusePPId, ReuseAs, !ModuleInfo)
    ;
        true
    ).

:- pred save_reuse_in_module_info_2(pred_proc_id::in, reuse_as::in,
    module_info::in, module_info::out) is det.

save_reuse_in_module_info_2(PPId, ReuseAs, !ModuleInfo) :- 
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_set_structure_reuse(to_structure_reuse_domain(ReuseAs),
        ProcInfo0, ProcInfo),
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

:- pred make_opt_int(module_info::in, io::di, io::uo) is det.

make_opt_int(ModuleInfo, !IO) :-
    module_info_get_name(ModuleInfo, ModuleName),
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
        module_info_predids(ModuleInfo, PredIds),   
        list.foldl(write_pred_reuse_info(ModuleInfo), PredIds, !IO),
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
    pred_info_get_import_status(PredInfo, ImportStatus),
    module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
    TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
    (
        (
            ImportStatus = status_exported
        ;
            ImportStatus = status_opt_exported
        ),
        \+ is_unify_or_compare_pred(PredInfo),

        % XXX These should be allowed, but the predicate declaration for the
        % specialized predicate is not produced before the structure_reuse
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
        list.foldl(write_proc_reuse_info(ProcTable, PredOrFunc, SymName, 
            Context, TypeVarSet), ProcIds, !IO)
    ;
        true
    ).

:- pred write_proc_reuse_info(proc_table::in, pred_or_func::in, 
    sym_name::in, prog_context::in, tvarset::in, proc_id::in, 
    io::di, io::uo) is det.

write_proc_reuse_info(ProcTable, PredOrFunc, SymName, 
        Context, TypeVarSet, ProcId, !IO) :-
    globals.io_lookup_bool_option(structure_reuse_analysis,
        ReuseAnalysis, !IO),
    (
        ReuseAnalysis = yes,
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_structure_reuse(ProcInfo, MaybeStructureReuseDomain),
        proc_info_declared_argmodes(ProcInfo, Modes),
        proc_info_get_varset(ProcInfo, VarSet),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        list.map(map.lookup(VarTypes), HeadVars, HeadVarTypes),
        write_pragma_structure_reuse_info(PredOrFunc, SymName, Modes,
            Context, HeadVars, yes(VarSet), HeadVarTypes, yes(TypeVarSet),
            MaybeStructureReuseDomain, !IO)
    ;
        ReuseAnalysis = no
    ).
%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "structure_reuse.analysis.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.analysis.
%-----------------------------------------------------------------------------%
