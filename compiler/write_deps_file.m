%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: write_deps_file.m.
%
% This module writes .dep, .dv and .d files, after getting its subcontractors
% generate_dep_d_files.m and generate_mmakefile_fragments.m to figure out
% what should go into them.
%
% We generate one .dep and one .dv file for each program, with those files
% being named prog.dep and prog.dv (if the name of the program is "prog").
% We generate one .d file for each module in the program, with the file
% being named mod.d (if the name of the module is "mod").
%
% The .dv file contains the definitions of all the mmake variable definitions
% relating to the program, while the .dep file contains all the rules
% relating to the program. The reason for this split is that we want mmake
% to glue all these mmakefile fragments together in the following order:
%
% - the program's .dv file
% - the Mmakefile in the current directory
% - the .d files of the program's modules
% - the program's .dep file
% - the standard Mmake.rules file
%
% This arrangement gives the Mmakefile access to the values of the
% variables defined in the program's .dv file, for example as lists
% of files on which a target depends. On the other hand, by including
% the automatically generated .dep file *after* the Mmakefile, we allow
% the rules in the .dep file to refer to variables defined in the Mmakefile
% (Usually the rules allow, but do not require, the Mmakefile to define
% these variables.)
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_deps_file.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.generate_mmakefile_fragments.
:- import_module parse_tree.module_baggage.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

    % generate_and_write_dep_file_gendep(ProgressStream, Globals,
    %   FileOrModule, DepsMap, Specs, !IO):
    %
    % Generate the per-program makefile dependencies file (`.dep' file)
    % for the program whose top-level module is specified by FileOrModule.
    % This involves first transitively reading in all imported or ancestor
    % modules. While we are at it, we also save the per-module makefile
    % dependency files (`.d' files) for all those modules. Return any errors
    % and/or warnings to be printed in Specs.
    %
:- pred generate_and_write_dep_file_gendep(io.text_output_stream::in,
    globals::in, file_or_module::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

    % generate_and_write_d_file_gendep(ProgressStream, Globals, FIleOrModule,
    %   DepsMap, Specs, !IO):
    %
    % Generate the per-module makefile dependency file ('.d' file)
    % for the given module.
    %
:- pred generate_and_write_d_file_gendep(io.text_output_stream::in,
    globals::in, file_or_module::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % generate_and_write_d_file_hlds(ProgressStream, Globals,
    %   BurdenedAugCompUnit, AllDeps, MaybeInclTransOptRule, !IO):
    %
    % Write out the per-module makefile dependencies (`.d') file for the
    % specified module. AllDeps is the set of all module names which the
    % generated code for this module might depend on, i.e. all that have been
    % used or imported, directly or indirectly, into this module, including
    % via .opt or .trans_opt files, and including parent modules of nested
    % modules. MaybeInclTransOptRule controls whether to include a
    % trans_opt_deps rule in the file, and if so, what the rule should say.
    %
:- pred generate_and_write_d_file_hlds(io.text_output_stream::in, globals::in,
    burdened_aug_comp_unit::in, set(module_name)::in,
    maybe_include_trans_opt_rule::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.mmakefiles.
:- import_module libs.options.
:- import_module parse_tree.file_names.
:- import_module parse_tree.generate_dep_d_files.
:- import_module parse_tree.make_module_file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_parse_tree.

:- import_module bool.
:- import_module dir.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

generate_and_write_dep_file_gendep(ProgressStream, Globals, FileOrModule,
        DepsMap, !:Specs, !IO) :-
    generate_deps_map(ProgressStream, Globals, do_not_search,
        FileOrModule, ModuleName, DepsMap, !:Specs, !IO),
    do_we_have_a_valid_module_dep(DepsMap, ModuleName, MaybeBurdenedModule),
    (
        MaybeBurdenedModule = error1(FatalErrorSpecs),
        % The error_specs in FatalErrorSpecs may already be in !.Specs,
        % but even if we add them again here, they will be printed just once.
        !:Specs = FatalErrorSpecs ++ !.Specs
    ;
        MaybeBurdenedModule = ok1(BurdenedModule),
        BurdenedModule = burdened_module(Baggage, _ParseTreeModuleSrc),
        generate_and_write_dep_dv_files(ProgressStream, Globals, ModuleName,
            DepsMap, Baggage, !IO),
        compute_deps_for_d_files_gendep(ProgressStream, Globals, ModuleName,
            DepsMap, DepGraphs, BurdenedModules, !Specs, !IO),
        generate_and_write_d_file_gendep_depgraphs(ProgressStream, Globals,
            BurdenedModules, DepGraphs, !IO)
    ).

generate_and_write_d_file_gendep(ProgressStream, Globals, FileOrModule,
        DepsMap, !:Specs, !IO) :-
    generate_deps_map(ProgressStream, Globals, do_search,
        FileOrModule, ModuleName, DepsMap, !:Specs, !IO),
    do_we_have_a_valid_module_dep(DepsMap, ModuleName, MaybeBurdenedModule),
    (
        MaybeBurdenedModule = error1(FatalErrorSpecs),
        % The error_specs in FatalErrorSpecs may already be in !.Specs,
        % but even if we add them again here, they will be printed just once.
        !:Specs = FatalErrorSpecs ++ !.Specs
    ;
        MaybeBurdenedModule = ok1(BurdenedModule),
        compute_deps_for_d_files_gendep(ProgressStream, Globals, ModuleName,
            DepsMap, DepGraphs, _BurdenedModules, !Specs, !IO),
        generate_and_write_d_file_gendep_depgraphs(ProgressStream, Globals,
            [BurdenedModule], DepGraphs, !IO)
    ).

    % Check whether we could read the main `.m' file.
    %
:- pred do_we_have_a_valid_module_dep(deps_map::in, module_name::in,
    maybe1(burdened_module)::out) is det.

do_we_have_a_valid_module_dep(DepsMap, ModuleName, MaybeBurdenedModule) :-
    ( if map.search(DepsMap, ModuleName, ModuleDep) then
        ModuleDep = deps(_, BurdenedModule),
        BurdenedModule = burdened_module(Baggage, _ParseTreeModuleSrc),
        Errors = Baggage ^ mb_errors,
        FatalErrors = Errors ^ rm_fatal_errors,
        ( if set.is_non_empty(FatalErrors) then
            FatalErrorSpecs = Errors ^ rm_fatal_error_specs,
            (
                FatalErrorSpecs = [],
                string.format("FatalErrorSpecs = [], with FatalErrors = %s\n",
                    [s(string.string(FatalErrors))], UnexpectedMsg),
                unexpected($pred, UnexpectedMsg)
            ;
                FatalErrorSpecs = [_ | _],
                MaybeBurdenedModule = error1(FatalErrorSpecs)
            )
        else
            MaybeBurdenedModule = ok1(BurdenedModule)
        )
    else
        % We don't have an error message to return here, but we also
        % neither want or need to return one, because if execution got here,
        % then the compiler should have already tried to read in ModuleName,
        % and printed a diagnostic for the failure of that attempt.
        MaybeBurdenedModule = error1([])
    ).

%---------------------------------------------------------------------------%

:- pred generate_and_write_dep_dv_files(io.text_output_stream::in, globals::in,
    module_name::in, deps_map::in, module_baggage::in, io::di, io::uo) is det.

generate_and_write_dep_dv_files(ProgressStream, Globals, ModuleName, DepsMap,
        Baggage, !IO) :-
    % First, build up a map of the dependencies.
    SourceFileName = Baggage ^ mb_source_file_name,

    generate_dv_mmakefile(Globals, SourceFileName, ModuleName, DepsMap,
        MmakeFileDv),
    generate_dep_mmakefile(Globals, SourceFileName, ModuleName, DepsMap,
        MmakeFileDep, !IO),
    MmakeFileStrDv = mmakefile_to_string(MmakeFileDv),
    MmakeFileStrDep = mmakefile_to_string(MmakeFileDep),

    % XXX LEGACY
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_dv), ModuleName,
        FileNameDv, _FileNameDvProposed, !IO),
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_mf_dep), ModuleName,
        FileNameDep, _FileNameDepProposed, !IO),

    write_string_to_file(ProgressStream, Globals,
        "Writing auto-dependency file", FileNameDv, MmakeFileStrDv,
        _SucceededDv, !IO),
    write_string_to_file(ProgressStream, Globals,
        "Writing auto-dependency file", FileNameDep, MmakeFileStrDep,
        _SucceededDep, !IO),

    % For Java, the main target is actually a shell script
    % which will set CLASSPATH appropriately, and then invoke java
    % on the appropriate .class file. Rather than generating
    % an Mmake rule to build this file when it is needed,
    % we just generate this file at "mmake depend" time, since
    % that is simpler and probably more efficient anyway.
    globals.get_target(Globals, Target),
    (
        Target = target_java,
        create_java_shell_script(ProgressStream, Globals, ModuleName,
            _Succeeded, !IO)
    ;
        ( Target = target_c
        ; Target = target_csharp
        )
    ).

%---------------------------------------------------------------------------%

generate_and_write_d_file_hlds(ProgressStream, Globals, BurdenedAugCompUnit,
        AllDeps, MaybeInclTransOptRule, !IO) :-
    map.init(Cache0),
    StdDeps = construct_std_deps_hlds(Globals, BurdenedAugCompUnit),
    DFileDeps = d_file_deps(StdDeps, AllDeps, MaybeInclTransOptRule),
    generate_d_mmakefile_contents(Globals, BurdenedAugCompUnit, DFileDeps,
        FileNameD, FileContentsStrD, Cache0, _Cache, !IO),
    write_out_d_file(ProgressStream, Globals,
        FileNameD, FileContentsStrD, !IO).

%---------------------------------------------------------------------------%

    % generate_and_write_d_file_gendep_depgraphs(ProgressStream, Globals,
    %   BurdenedModules, DepGraphs, !IO):
    %
    % This predicate writes out the .d files for all the modules in the
    % BurdenedModules list.
    %
:- pred generate_and_write_d_file_gendep_depgraphs(io.text_output_stream::in,
    globals::in, list(burdened_module)::in, dep_graphs::in,
    io::di, io::uo) is det.

generate_and_write_d_file_gendep_depgraphs(ProgressStream,
        Globals, BurdenedModules, DepGraphs, !IO) :-
    map.init(Cache0),
    generate_and_write_d_file_gendep_depgraphs_loop(ProgressStream,
        Globals, BurdenedModules, DepGraphs, Cache0, _Cache, !IO).

:- pred generate_and_write_d_file_gendep_depgraphs_loop(
    io.text_output_stream::in, globals::in, list(burdened_module)::in,
    dep_graphs::in, module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_and_write_d_file_gendep_depgraphs_loop(_, _, [], _, !Cache, !IO).
generate_and_write_d_file_gendep_depgraphs_loop(ProgressStream, Globals,
        [BurdenedModule | BurdenedModules], DepGraphs, !Cache, !IO) :-
    generate_and_write_d_file_gendep_depgraphs(ProgressStream, Globals,
        BurdenedModule, DepGraphs, !Cache, !IO),
    generate_and_write_d_file_gendep_depgraphs_loop(ProgressStream, Globals,
        BurdenedModules, DepGraphs, !Cache, !IO).

:- pred generate_and_write_d_file_gendep_depgraphs(io.text_output_stream::in,
    globals::in, burdened_module::in, dep_graphs::in,
    module_file_name_cache::in, module_file_name_cache::out,
    io::di, io::uo) is det.

generate_and_write_d_file_gendep_depgraphs(ProgressStream, Globals,
        BurdenedModule, DepGraphs, !Cache, !IO) :-
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),
    % XXX DFILE Note that even if a fatal error occurred for one of the files
    % that the current Module depends on, a .d file is still produced,
    % even though it probably contains incorrect information.
    ModuleErrors = Baggage ^ mb_errors,
    FatalErrors = ModuleErrors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        init_aug_compilation_unit(ParseTreeModuleSrc, AugCompUnit),
        BurdenedAugCompUnit = burdened_aug_comp_unit(Baggage, AugCompUnit),
        DFileDeps = construct_d_file_deps_gendep(Globals, DepGraphs,
            ParseTreeModuleSrc),
        % XXX DFILE The way IndirectOptDeps is computed seems to have nothing
        % to do with the way the generate_d_file_fragment predicate's
        % corresponding argument is computed. This seems to me (zs)
        % to be a BUG.
        generate_d_mmakefile_contents(Globals, BurdenedAugCompUnit,
            DFileDeps, FileNameD, FileContentsStrD, !Cache, !IO),
        write_out_d_file(ProgressStream, Globals,
            FileNameD, FileContentsStrD, !IO)
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred write_out_d_file(io.text_output_stream::in, globals::in,
    file_name::in, string::in, io::di, io::uo) is det.

write_out_d_file(ProgressStream, Globals, FileNameD, FileContentsStrD, !IO) :-
    % To avoid problems with concurrent updates of `.d' files during
    % parallel makes, we first create the file with a temporary name,
    % and then rename it to the desired name when we have finished.
    % XXX I (zs) think that if two mmake actions both update the same .d file,
    % then having them executed in parallel is itself a bug, and that bug
    % should be fixed, instead of being accommodated, as we do here.
    % Therefore I think that this predicate should be deleted,
    % and its uses replaced by calls to write_string_to_file.
    io.file.make_temp_file(dir.dirname(FileNameD), "tmp_d", "",
        TmpFileNameDResult, !IO),
    (
        TmpFileNameDResult = error(Error),
        Message = "Could not create temporary file: " ++ error_message(Error),
        report_error(ProgressStream, Message, !IO)
    ;
        TmpFileNameDResult = ok(TmpFileNameD),
        globals.lookup_bool_option(Globals, verbose, Verbose),
        (
            Verbose = no
        ;
            Verbose = yes,
            io.format(ProgressStream,
                "%% Writing auto-dependency file `%s'...",
                [s(FileNameD)], !IO),
            io.flush_output(ProgressStream, !IO)
        ),
        io.open_output(TmpFileNameD, Result, !IO),
        (
            Result = error(IOError),
            maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening temporary file `%s' for output: %s",
                [s(TmpFileNameD), s(IOErrorMessage)], Message),
            report_error(ProgressStream, Message, !IO)
        ;
            Result = ok(DepStream),
            io.write_string(DepStream, FileContentsStrD, !IO),
            io.close_output(DepStream, !IO),

            io.file.rename_file(TmpFileNameD, FileNameD,
                FirstRenameResult, !IO),
            (
                FirstRenameResult = error(_),
                % On some systems, we need to remove the existing file first,
                % if any. So try again that way.
                io.file.remove_file(FileNameD, RemoveResult, !IO),
                (
                    RemoveResult = error(Error4),
                    maybe_write_string(ProgressStream, Verbose,
                        " failed.\n", !IO),
                    maybe_flush_output(ProgressStream, Verbose, !IO),
                    io.error_message(Error4, ErrorMsg),
                    string.format("can't remove file `%s': %s",
                        [s(FileNameD), s(ErrorMsg)], Message),
                    report_error(ProgressStream, Message, !IO)
                ;
                    RemoveResult = ok,
                    io.file.rename_file(TmpFileNameD,
                        FileNameD, SecondRenameResult, !IO),
                    (
                        SecondRenameResult = error(Error5),
                        maybe_write_string(ProgressStream, Verbose,
                            " failed.\n", !IO),
                        maybe_flush_output(ProgressStream, Verbose, !IO),
                        io.error_message(Error5, ErrorMsg),
                        string.format("can't rename file `%s' as `%s': %s",
                            [s(TmpFileNameD), s(FileNameD),
                            s(ErrorMsg)], Message),
                        report_error(ProgressStream, Message, !IO)
                    ;
                        SecondRenameResult = ok,
                        maybe_write_string(ProgressStream, Verbose,
                            " done.\n", !IO)
                    )
                )
            ;
                FirstRenameResult = ok,
                maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_deps_file.
%---------------------------------------------------------------------------%
