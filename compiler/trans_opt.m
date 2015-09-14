%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: trans_opt.m.
% Main author: crs.
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
% See also intermod.m, which handles `.opt' files.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.trans_opt.

:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Open the file "<module-name>.trans_opt.tmp", and write out the
    % declarations.
    %
:- pred write_trans_opt_file(module_info::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.module_cmds.
:- import_module transform_hlds.intermod.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

write_trans_opt_file(ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".trans_opt.tmp",
        do_create_dirs, TmpOptName, !IO),
    io.open_output(TmpOptName, Result, !IO),
    (
        Result = error(Error),
        io.error_message(Error, Msg),
        io.progname_base("trans_opt.m", ProgName, !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": cannot open transitive optimisation file `", !IO),
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
        io.write_string(":- module ", !IO),
        mercury_output_bracketed_sym_name(ModuleName, !IO),
        io.write_string(".\n", !IO),

        % All predicates to write global items into the .trans_opt
        % file should go here.

        % Select all the predicates for which something should be written
        % into the .trans_opt file.

        module_info_get_valid_pred_ids(ModuleInfo, PredIds),
        PredIdsSet = set.from_list(PredIds),
        module_info_get_structure_reuse_preds(ModuleInfo, ReusePredsSet),
        PredIdsNoReusePredsSet = set.difference(PredIdsSet, ReusePredsSet),
        PredIdsNoReuseVersions = set.to_sorted_list(PredIdsNoReusePredsSet),

        list.foldl(
            write_pred_termination_info(ModuleInfo),
            PredIdsNoReuseVersions, !IO),
        list.foldl(
            output_pragma_termination2_infos_for_pred(ModuleInfo),
            PredIdsNoReuseVersions, !IO),

        list.foldl(
            write_pred_sharing_info(ModuleInfo),
            PredIdsNoReuseVersions, !IO),
        list.foldl(
            write_pred_reuse_info(ModuleInfo),
            PredIdsNoReuseVersions, !IO),

        module_info_get_exception_info(ModuleInfo, ExceptionInfo),
        list.foldl(
            write_pragma_exceptions(ModuleInfo, ExceptionInfo),
            PredIdsNoReuseVersions, !IO),

        module_info_get_trailing_info(ModuleInfo, TrailingInfo),
        list.foldl(
            write_pragma_trailing_info(ModuleInfo, TrailingInfo),
            PredIdsNoReuseVersions, !IO),

        module_info_get_mm_tabling_info(ModuleInfo, TablingInfo),
        list.foldl(
            write_pragma_mm_tabling_info(ModuleInfo, TablingInfo),
            PredIdsNoReuseVersions, !IO),

        io.set_output_stream(OldStream, _, !IO),
        io.close_output(Stream, !IO),

        module_name_to_file_name(Globals, ModuleName, ".trans_opt",
            do_not_create_dirs, OptName, !IO),
        update_interface(Globals, OptName, !IO),
        touch_interface_datestamp(Globals, ModuleName, ".trans_opt_date", !IO)
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.trans_opt.
%-----------------------------------------------------------------------------%
