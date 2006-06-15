%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: trans_opt.m
% main author: crs
%
% Transitive intermodule optimization allows the compiler to do intermodule
% optimization that depends on other .trans_opt files. In comparison to .opt
% files, .trans_opt files allow much more accurate optimization to occur,
% but at the cost of an increased number of compilations required. The fact
% that a .trans_opt file may depend on other .trans_opt files introduces
% the possibility of circular dependencies occuring. These circular
% dependencies would occur if the data in A.trans_opt depended on the data
% in B.trans_opt being correct, and vice versa.
%
% The following system is used to ensure that circular dependencies cannot
% occur:
%
%   When mmake <module>.depend is run, mmc calculates a suitable ordering.
%   This ordering is then used to create each of the .d files. This allows
%   make to ensure that all necessary trans_opt files are up to date before
%   creating any other trans_opt files. This same information is used by mmc
%   to decide which trans_opt files may be imported when creating another
%   .trans_opt file. By observing the ordering decided upon when mmake
%   module.depend was run, any circularities which may have been created
%   are avoided.
%
% This module writes out the interface for transitive intermodule optimization.
% The .trans_opt file includes:
%   :- pragma termination_info declarations for all exported preds
%   :- pragma exceptions declartions for all exported preds
%   :- pragma trailing_info declarations for all exported preds.
%
% All these items should be module qualified.
% Constructors should be explicitly type qualified.
%
% Note that the .trans_opt file does not (yet) include clauses, `pragma
% foreign_proc' declarations, or any of the other information that would be
% needed for inlining or other optimizations; currently it is only used
% for termination analysis, exception and trail usage analysis.
%
% This module also contains predicates to read in the .trans_opt files.
%
% See also intermod.m, which handles `.opt' files.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.trans_opt.

:- interface.

:- import_module hlds.hlds_module.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.modules.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Open the file "<module-name>.trans_opt.tmp", and write out the
    % declarations.
    %
:- pred write_optfile(module_info::in, io::di, io::uo) is det.

    % grab_optfiles(ModuleList, !ModuleImports, Error, !IO):
    %
    % Add the items from each of the modules in ModuleList.trans_opt to
    % the items in ModuleImports.
    %
:- pred grab_optfiles(list(module_name)::in,
    module_imports::in, module_imports::out, bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.
:- import_module transform_hlds.ctgc.
:- import_module transform_hlds.ctgc.structure_reuse.
:- import_module transform_hlds.ctgc.structure_reuse.analysis.
:- import_module transform_hlds.ctgc.structure_sharing.
:- import_module transform_hlds.ctgc.structure_sharing.analysis.
:- import_module transform_hlds.exception_analysis.
:- import_module transform_hlds.intermod.
:- import_module transform_hlds.tabling_analysis.
:- import_module transform_hlds.term_constr_main.
:- import_module transform_hlds.termination.
:- import_module transform_hlds.trailing_analysis.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

write_optfile(Module, !IO) :-
    module_info_get_name(Module, ModuleName),
    module_name_to_file_name(ModuleName, ".trans_opt.tmp", yes, TmpOptName,
        !IO),
    io.open_output(TmpOptName, Result, !IO),
    (
        Result = error(Error),
        io.error_message(Error, Msg),
        io.progname_base("trans_opt.m", ProgName, !IO),
        io.write_string(ProgName, !IO),
        io.write_string(
            ": cannot open transitive optimisation file `", !IO),
        io.write_string(TmpOptName, !IO),
        io.write_string("' \n", !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": for output: ", !IO),
        io.write_string(Msg, !IO),
        io.nl(!IO),
        io.set_exit_status(1, !IO)
    ;
        Result = ok(Stream),
        io.set_output_stream(Stream, OldStream, !IO),
        module_info_get_name(Module, ModName),
        io.write_string(":- module ", !IO),
        mercury_output_bracketed_sym_name(ModName, !IO),
        io.write_string(".\n", !IO),

        % All predicates to write global items into the .trans_opt
        % file should go here.

        % Select all the predicates for which something should be writting
        % into the .trans_opt file. 
        %
        module_info_predids(Module, PredIds),
        module_info_get_structure_reuse_map(Module, ReuseMap), 
        map.values(ReuseMap, ReuseResults), 
        list.map(fst, ReuseResults, ReusePredProcIds), 
        list.map(get_pred_id, ReusePredProcIds, ReusePredIds), 
        list.delete_elems(PredIds, ReusePredIds, PredIdsNoReuseVersions), 

        list.foldl(termination.write_pred_termination_info(Module),
            PredIdsNoReuseVersions, !IO),
        list.foldl(term_constr_main.output_pred_termination2_info(Module),
            PredIdsNoReuseVersions, !IO),

        list.foldl(structure_sharing.analysis.write_pred_sharing_info(Module),
            PredIdsNoReuseVersions, !IO), 
        list.foldl(structure_reuse.analysis.write_pred_reuse_info(Module), 
            PredIdsNoReuseVersions, !IO), 

        module_info_get_exception_info(Module, ExceptionInfo),
        list.foldl(
            exception_analysis.write_pragma_exceptions(Module, ExceptionInfo),
            PredIdsNoReuseVersions, !IO),

        module_info_get_trailing_info(Module, TrailingInfo),
        list.foldl(
            write_pragma_trailing_info(Module, TrailingInfo), 
            PredIdsNoReuseVersions, !IO),

        module_info_get_mm_tabling_info(Module, TablingInfo),
        list.foldl(
            write_pragma_mm_tabling_info(Module, TablingInfo),
            PredIdsNoReuseVersions, !IO),

        io.set_output_stream(OldStream, _, !IO),
        io.close_output(Stream, !IO),

        module_name_to_file_name(ModuleName, ".trans_opt", no, OptName, !IO),
        update_interface(OptName, !IO),
        touch_interface_datestamp(ModuleName, ".trans_opt_date", !IO)
    ).

:- pred get_pred_id(pred_proc_id::in, pred_id::out) is det.

get_pred_id(proc(PredId, _ProcId), PredId). 

%-----------------------------------------------------------------------------%
%
% Read and process the transitive optimization interfaces.
%

grab_optfiles(TransOptDeps, !Module, FoundError, !IO) :-
    globals.io_lookup_bool_option(verbose, Verbose, !IO),
    maybe_write_string(Verbose, "% Reading .trans_opt files..\n", !IO),
    maybe_flush_output(Verbose, !IO),

    read_trans_opt_files(TransOptDeps, [], OptItems, no, FoundError, !IO),

    append_pseudo_decl(opt_imported, !Module),
    module_imports_get_items(!.Module, Items0),
    list.append(Items0, OptItems, Items),
    module_imports_set_items(Items, !Module),
    module_imports_set_error(no_module_errors, !Module),

    maybe_write_string(Verbose, "% Done.\n", !IO).

:- pred read_trans_opt_files(list(module_name)::in, item_list::in,
    item_list::out, bool::in, bool::out, io::di, io::uo) is det.

read_trans_opt_files([], !Items, !Error, !IO).
read_trans_opt_files([Import | Imports], !Items, !Error, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    maybe_write_string(VeryVerbose,
        "% Reading transitive optimization interface for module", !IO),
    maybe_write_string(VeryVerbose, " `", !IO),
    sym_name_to_string(Import, ImportString),
    maybe_write_string(VeryVerbose, ImportString, !IO),
    maybe_write_string(VeryVerbose, "'... ", !IO),
    maybe_flush_output(VeryVerbose, !IO),

    module_name_to_search_file_name(Import, ".trans_opt", FileName, !IO),
    prog_io.read_opt_file(FileName, Import,
        ModuleError, Messages, NewItems, !IO),

    maybe_write_string(VeryVerbose, " done.\n", !IO),

    intermod.update_error_status(trans_opt, FileName, ModuleError,
        Messages, !Error, !IO),
    list.append(!.Items, NewItems, !:Items),
    read_trans_opt_files(Imports, !Items, !Error, !IO).

%-----------------------------------------------------------------------------%
:- end_module trans_opt.
%-----------------------------------------------------------------------------%
