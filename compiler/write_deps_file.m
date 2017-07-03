%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: write_deps_file.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_deps_file.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.module_imports.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

    % write_dependency_file(Globals, Module, AllDeps, MaybeTransOptDeps):
    %
    % Write out the per-module makefile dependencies (`.d') file for the
    % specified module. AllDeps is the set of all module names which the
    % generated code for this module might depend on, i.e. all that have been
    % used or imported, directly or indirectly, into this module, including
    % via .opt or .trans_opt files, and including parent modules of nested
    % modules. MaybeTransOptDeps is a list of module names which the
    % `.trans_opt' file may depend on. This is set to `no' if the
    % dependency list is not available.
    %
:- pred write_dependency_file(globals::in, module_and_imports::in,
    set(module_name)::in, maybe(list(module_name))::in, io::di, io::uo) is det.

    % generate_dependencies_write_d_files(Globals, Modules,
    %   IntDepsRel, ImplDepsRel, IndirectDepsRel, IndirectOptDepsRel,
    %   TransOptOrder, DepsMap, !IO):
    %
    % This predicate writes out the .d files for all the modules in the
    % Modules list.
    % IntDepsGraph gives the interface dependency graph.
    % ImplDepsGraph gives the implementation dependency graph.
    % IndirectDepsGraph gives the indirect dependency graph
    % (this includes dependencies on `*.int2' files).
    % IndirectOptDepsGraph gives the indirect optimization dependencies
    % (this includes dependencies via `.opt' and `.trans_opt' files).
    % These are all computed from the DepsMap.
    % TransOptOrder gives the ordering that is used to determine
    % which other modules the .trans_opt files may depend on.
    %
:- pred generate_dependencies_write_d_files(globals::in, list(deps)::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    list(module_name)::in, deps_map::in, io::di, io::uo) is det.

    % Write out the `.dv' file, using the information collected in the
    % deps_map data structure.
    %
:- pred generate_dependencies_write_dv_file(globals::in, file_name::in,
    module_name::in, deps_map::in, io::di, io::uo) is det.

    % Write out the `.dep' file, using the information collected in the
    % deps_map data structure.
    %
:- pred generate_dependencies_write_dep_file(globals::in, file_name::in,
    module_name::in, deps_map::in, io::di, io::uo) is det.

:- pred maybe_output_module_order(globals::in, module_name::in,
    list(set(module_name))::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % For each dependency, search intermod_directories for a .Suffix
    % file or a .m file, filtering out those for which the search fails.
    % If --use-opt-files is set, only look for `.opt' files,
    % not `.m' files.
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
    % This predicate must operate on lists, not sets, of module names,
    % because it needs to preserve the chosen trans_opt deps ordering,
    % which is derived from the dependency graph between modules,
    % and not just the modules' names.
    %
:- pred get_opt_deps(globals::in, bool::in, list(string)::in, string::in,
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.mmakefiles.
:- import_module make.                          % XXX undesirable dependency
:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.        % XXX undesirable dependency
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.source_file_map.

:- import_module assoc_list.
:- import_module cord.
:- import_module digraph.
:- import_module dir.
:- import_module library.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module sparse_bitset.
:- import_module string.

%---------------------------------------------------------------------------%

write_dependency_file(Globals, ModuleAndImports, AllDeps,
        MaybeTransOptDeps, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),

    % To avoid problems with concurrent updates of `.d' files during
    % parallel makes, we first create the file with a temporary name,
    % and then rename it to the desired name when we have finished.
    ModuleName = ModuleAndImports ^ mai_module_name,
    module_name_to_file_name(Globals, do_create_dirs, ".d",
        ModuleName, DependencyFileName, !IO),
    io.make_temp_file(dir.dirname(DependencyFileName), "tmp_d",
        "", TmpDependencyFileNameRes, !IO),
    (
        TmpDependencyFileNameRes = error(Error),
        Message = "Could not create temporary file: " ++ error_message(Error),
        report_error(Message, !IO)
    ;
        TmpDependencyFileNameRes = ok(TmpDependencyFileName),
        maybe_write_string(Verbose, "% Writing auto-dependency file `", !IO),
        maybe_write_string(Verbose, DependencyFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        maybe_flush_output(Verbose, !IO),
        io.open_output(TmpDependencyFileName, Result, !IO),
        (
            Result = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening temporary file `",
                TmpDependencyFileName, "' for output: ",
                IOErrorMessage], Message),
            report_error(Message, !IO)
        ;
            Result = ok(DepStream),
            start_mmakefile(DepStream, MmakeFile0, !IO),
            do_write_dependency_file(DepStream, Globals, ModuleAndImports,
                AllDeps, MaybeTransOptDeps, MmakeFile0, MmakeFile, !IO),
            end_mmakefile(DepStream, MmakeFile, !IO),
            io.close_output(DepStream, !IO),

            io.rename_file(TmpDependencyFileName, DependencyFileName,
                FirstRenameResult, !IO),
            (
                FirstRenameResult = error(_),
                % On some systems, we need to remove the existing file first,
                % if any. So try again that way.
                io.remove_file(DependencyFileName, RemoveResult, !IO),
                (
                    RemoveResult = error(Error4),
                    maybe_write_string(Verbose, " failed.\n", !IO),
                    maybe_flush_output(Verbose, !IO),
                    io.error_message(Error4, ErrorMsg),
                    string.append_list(["can't remove file `",
                        DependencyFileName, "': ", ErrorMsg], Message),
                    report_error(Message, !IO)
                ;
                    RemoveResult = ok,
                    io.rename_file(TmpDependencyFileName, DependencyFileName,
                        SecondRenameResult, !IO),
                    (
                        SecondRenameResult = error(Error5),
                        maybe_write_string(Verbose, " failed.\n", !IO),
                        maybe_flush_output(Verbose, !IO),
                        io.error_message(Error5, ErrorMsg),
                        string.append_list(["can't rename file `",
                            TmpDependencyFileName, "' as `",
                            DependencyFileName, "': ", ErrorMsg], Message),
                        report_error(Message, !IO)
                    ;
                        SecondRenameResult = ok,
                        maybe_write_string(Verbose, " done.\n", !IO)
                    )
                )
            ;
                FirstRenameResult = ok,
                maybe_write_string(Verbose, " done.\n", !IO)
            )
        )
    ).

:- pred do_write_dependency_file(io.text_output_stream::in,
    globals::in, module_and_imports::in,
    set(module_name)::in, maybe(list(module_name))::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

do_write_dependency_file(DepStream, Globals, ModuleAndImports,
        AllDeps, MaybeTransOptDeps, !MmakeFile, !IO) :-
    io.write_string(DepStream,
        "# vim: ts=8 sw=8 noexpandtab ft=make\n\n", !IO),

    ModuleAndImports = module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, _ModuleNameContext, ParentDeps, IntDeps, ImpDeps,
        IndirectDeps, _Children, InclDeps, NestedDeps, FactDeps0,
        ForeignImportModules0, ForeignIncludeFilesCord,
        ContainsForeignCode, _ContainsForeignExport,
        SrcItemBlocks, DirectIntItemBlocksCord, IndirectIntItemBlocksCord,
        OptItemBlocksCord, IntForOptItemBlocksCord, _ModuleVersionNumbersCord,
        _Specs, _Error, _Timestamps, _HasMain, _Dir),

    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),

    MmakeStartComment = mmake_start_comment("module dependencies",
        ModuleNameString, SourceFileName, Version, FullArch),
    gather_mmake_entry(DepStream, MmakeStartComment, !MmakeFile, !IO),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),

    set.union(IntDeps, ImpDeps, LongDeps0),
    ShortDeps0 = IndirectDeps,
    set.delete(ModuleName, LongDeps0, LongDeps),
    set.difference(ShortDeps0, LongDeps, ShortDeps1),
    set.delete(ModuleName, ShortDeps1, ShortDeps),
    list.sort_and_remove_dups(FactDeps0, FactDeps),

    module_name_to_file_name(Globals, do_not_create_dirs, ".trans_opt_date",
        ModuleName, TransOptDateFileName, !IO),
    (
        MaybeTransOptDeps = yes(TransOptDeps0),
        set.intersect(set.list_to_set(TransOptDeps0), LongDeps,
            TransOptDateDeps),

        % Note that maybe_read_dependency_file searches for
        % this exact pattern.
        io.write_strings(DepStream, [TransOptDateFileName, " :"], !IO),
        write_dependencies_set(Globals, DepStream, ".trans_opt",
            TransOptDateDeps, !IO),
        io.write_string(DepStream, "\n\n", !IO),

        make_module_file_names_with_suffix(Globals, ".trans_opt",
            set.to_sorted_list(TransOptDateDeps), TransOptDateDepsFileNames,
            !IO),
        MmakeRuleTransOpt = mmake_simple_rule("trans_opt_deps",
            mmake_rule_is_not_phony,
            TransOptDateFileName,
            TransOptDateDepsFileNames,
            []),
        gather_mmake_entry(DepStream, MmakeRuleTransOpt, !MmakeFile, !IO)
    ;
        MaybeTransOptDeps = no
    ),

    (
        FactDeps = [_ | _],
        io.write_strings(DepStream,
            ["\n\n", ModuleMakeVarName, ".fact_tables ="], !IO),
        write_file_dependencies_list(DepStream, "", FactDeps, !IO),
        io.nl(DepStream, !IO),

        MmakeVarFactTables = mmake_var_defn_list(
            ModuleMakeVarName ++ ".fact_tables",
            FactDeps),
        gather_mmake_entry(DepStream, MmakeVarFactTables, !MmakeFile, !IO),

        io.write_strings(DepStream, [
            "\n\n", ModuleMakeVarName,
            ".fact_tables.os = $(", ModuleMakeVarName,
            ".fact_tables:%=$(os_subdir)%.$O)\n\n",
            ModuleMakeVarName,
            ".fact_tables.cs = $(", ModuleMakeVarName,
            ".fact_tables:%=$(cs_subdir)%.c)\n\n"
        ], !IO),

        MmakeVarFactTablesOs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.os",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(os_subdir)%.$O)"),
        MmakeVarFactTablesCs = mmake_var_defn(
            ModuleMakeVarName ++ ".fact_tables.cs",
            "$(" ++ ModuleMakeVarName ++ ".fact_tables:%=$(cs_subdir)%.c)"),
        MmakeVarsFactTables = [MmakeVarFactTablesOs, MmakeVarFactTablesCs],
        gather_mmake_entries(DepStream, MmakeVarsFactTables,
            !MmakeFile, !IO)
    ;
        FactDeps = []
    ),

    ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
        ErrFileName = SourceFileBase ++ ".err"
    else
        unexpected($pred, "source file name doesn't end in `.m'")
    ),

    module_name_to_file_name(Globals, do_not_create_dirs, ".optdate",
        ModuleName, OptDateFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".c_date",
        ModuleName, CDateFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".$O",
        ModuleName, ObjFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".java_date",
        ModuleName, JavaDateFileName, !IO),
    % XXX Why is the extension hardcoded to .pic_o here?  That looks wrong.
    % It should probably be .$(EXT_FOR_PIC_OBJECT) - juliensf.
    module_name_to_file_name(Globals, do_not_create_dirs, ".pic_o",
        ModuleName, PicObjFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".int0",
        ModuleName, Int0FileName, !IO),

    some [TargetGroup, TargetGroups, !SourceGroups] (
    io.write_strings(DepStream, ["\n\n",
        OptDateFileName, " ",
        TransOptDateFileName, " ",
        ErrFileName, " ",
        CDateFileName, " ",
        JavaDateFileName
    ], !IO),
    io.write_string(DepStream, " : ", !IO),
    TargetGroup = mmake_file_name_group("",
        one_or_more(OptDateFileName,
            [TransOptDateFileName, ErrFileName,
            CDateFileName, JavaDateFileName])),
    TargetGroups = one_or_more(TargetGroup, []),

    io.write_string(DepStream, SourceFileName, !IO),
    !:SourceGroups = [make_singleton_file_name_group(SourceFileName)],
    % If the module contains nested submodules then the `.int0' file
    % must first be built.
    ( if set.is_empty(InclDeps) then
        true
    else
        io.write_strings(DepStream, [" ", Int0FileName], !IO),
        !:SourceGroups = !.SourceGroups ++
            [make_singleton_file_name_group(Int0FileName)]
    ),
    write_dependencies_set(Globals, DepStream,
        ".int0", ParentDeps, !IO),
    write_dependencies_set(Globals, DepStream,
        ".int", LongDeps, !IO),
    write_dependencies_set(Globals, DepStream,
        ".int2", ShortDeps, !IO),
    make_module_file_name_group_with_suffix(Globals, "parent deps", ".int0",
        ParentDeps, ParentDepsSourceGroups, !IO),
    make_module_file_name_group_with_suffix(Globals, "long deps", ".int",
        LongDeps, LongDepsSourceGroups, !IO),
    make_module_file_name_group_with_suffix(Globals, "short deps", ".int2",
        ShortDeps, ShortDepsSourceGroups, !IO),
    !:SourceGroups = !.SourceGroups ++
        ParentDepsSourceGroups ++
        LongDepsSourceGroups ++
        ShortDepsSourceGroups,

    ForeignIncludeFiles = cord.list(ForeignIncludeFilesCord),
    % This is conservative: a target file for foreign language A
    % does not truly depend on a file included for foreign language B.
    write_foreign_include_file_dependencies_list(DepStream,
        SourceFileName, ForeignIncludeFiles, !IO),
    ForeignImportFileNames =
        list.map(foreign_include_file_path_name(SourceFileName),
            ForeignIncludeFiles),
    !:SourceGroups = !.SourceGroups ++
        make_file_name_group("foreign imports", ForeignImportFileNames),

    (
        FactDeps = [_ | _],
        io.write_strings(DepStream, [
            " \\\n\t$(", ModuleMakeVarName, ".fact_tables)"
        ], !IO),
        FactTableSourceGroup = mmake_file_name_group("fact tables",
            one_or_more("$(" ++ ModuleMakeVarName ++ ".fact_tables)", [])),
        !:SourceGroups = !.SourceGroups ++ [FactTableSourceGroup]
    ;
        FactDeps = []
    ),
    io.write_string(DepStream, "\n\n", !IO),

    MmakeRuleDateFileDeps = mmake_general_rule("date_file_deps",
        mmake_rule_is_not_phony,
        TargetGroups,
        !.SourceGroups,
        []),
    gather_mmake_entry(DepStream, MmakeRuleDateFileDeps, !MmakeFile, !IO)

    ),

    (
        FactDeps = [_ | _],
        % XXX These rules seem wrong to me. -zs

        io.write_strings(DepStream, [
            "$(", ModuleMakeVarName, ".fact_tables.os) : $(",
            ModuleMakeVarName, ".fact_tables) ",
            SourceFileName, "\n\n",
            "$(", ModuleMakeVarName, ".fact_tables.cs) : ",
            ObjFileName, "\n"
        ], !IO),

        MmakeRuleFactOs = mmake_simple_rule("fact_table_os",
            mmake_rule_is_not_phony,
            "$(" ++ ModuleMakeVarName ++ ".fact_tables.os)",
            ["$(" ++ ModuleMakeVarName ++  ".fact_tables)", SourceFileName],
            []),
        MmakeRuleFactCs = mmake_simple_rule("fact_table_cs",
            mmake_rule_is_not_phony,
            "$(" ++ ModuleMakeVarName ++ ".fact_tables.cs)",
            [ObjFileName],
            []),
        MmakeRulesFactOsCs = [MmakeRuleFactOs, MmakeRuleFactCs],
        gather_mmake_entries(DepStream, MmakeRulesFactOsCs, !MmakeFile, !IO)
    ;
        FactDeps = []
    ),

    NestedExts = [
        ".optdate",
        ".trans_opt_date",
        ".c_date",
        ".dir/*.$O",
        ".java_date"],

    % If a module contains nested-submodules then we need to build
    % the nested children before attempting to build the parent module.
    ( if set.is_empty(NestedDeps) then
        true
    else
        list.map_foldl(
            write_nested_deps(DepStream, Globals, ModuleName,
                set.to_sorted_list(NestedDeps)),
            NestedExts, MmakeRulesNestedDeps, !IO),
        gather_mmake_entries(DepStream, MmakeRulesNestedDeps, !MmakeFile, !IO)
    ),

    globals.lookup_bool_option(Globals, use_opt_files, UseOptFiles),
    globals.lookup_bool_option(Globals, intermodule_optimization,
        Intermod),
    globals.lookup_accumulating_option(Globals, intermod_directories,
        IntermodDirs),

    % If intermodule_optimization is enabled, then all the .mh files
    % must exist because it is possible that the .c file imports them
    % directly or indirectly.
    (
        Intermod = yes,
        io.write_strings(DepStream, ["\n\n", ObjFileName, " : "], !IO),
        write_dependencies_list(Globals, DepStream, ".mh",
            set.to_sorted_list(AllDeps), !IO),
        io.write_string(DepStream, "\n\n", !IO),

        make_module_file_names_with_suffix(Globals, ".mh",
            set.to_sorted_list(AllDeps), AllDepsFileNames, !IO),
        MmakeRuleMhDeps = mmake_simple_rule("machine_dependent_header_deps",
            mmake_rule_is_not_phony,
            ObjFileName,
            AllDepsFileNames,
            []),
        gather_mmake_entry(DepStream, MmakeRuleMhDeps, !MmakeFile, !IO)
    ;
        Intermod = no
    ),
    ( if
        ( Intermod = yes
        ; UseOptFiles = yes
        )
    then
        some [Targets, Sources] (

        io.write_strings(DepStream, [
            "\n\n",
            TransOptDateFileName, " ",
            ErrFileName, " ",
            CDateFileName, " ",
            JavaDateFileName, " : "
        ], !IO),
        Targets = one_or_more(TransOptDateFileName,
            [ErrFileName, CDateFileName, JavaDateFileName]),

        % The target (e.g. C) file only depends on the .opt files from the
        % current directory, so that inter-module optimization works when
        % the .opt files for the library are unavailable. This is only
        % necessary because make doesn't allow conditional dependencies.
        % The dependency on the current module's .opt file is to make sure
        % the module gets type-checked without having the definitions
        % of abstract types from other modules.
        %
        % XXX The code here doesn't correctly handle dependencies
        % on `.int' and `.int2' files needed by the `.opt' files.
        globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
        globals.lookup_bool_option(Globals, use_trans_opt_files, UseTransOpt),

        bool.not(UseTransOpt, BuildOptFiles),
        ( if
            ( TransOpt = yes
            ; UseTransOpt = yes
            )
        then
            get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs,
                [ModuleName | set.to_sorted_list(LongDeps)],
                OptDeps, TransOptDeps1, !IO),
            MaybeTransOptDeps1 = yes(TransOptDeps1)
        else
            get_opt_deps(Globals, BuildOptFiles, IntermodDirs, ".opt",
                [ModuleName | set.to_sorted_list(LongDeps)],
                OptDeps, !IO),
            MaybeTransOptDeps1 = no
        ),

        OptInt0Deps = set.union_list(
            list.map(get_ancestors_set, OptDeps)),
        % The calls to write_dependencies_{list,set} convert
        % OptDeps and OptInt0Deps to filenames, and write them out.
        write_dependencies_list(Globals, DepStream, ".opt",
            OptDeps, !IO),
        write_dependencies_set(Globals, DepStream, ".int0",
            OptInt0Deps, !IO),
        io.write_string(DepStream, "\n\n", !IO),

        % The calls to make_module_file_names_with_suffix convert
        % OptDeps and OptInt0Deps to filenames. They will be written out
        % as part of MmakeRuleDateOptInt0Deps.
        %
        % This pattern is repeated below.
        make_module_file_names_with_suffix(Globals, ".opt",
            OptDeps, OptDepsFileNames, !IO),
        make_module_file_names_with_suffix(Globals, ".int0",
            set.to_sorted_list(OptInt0Deps), OptInt0DepsFileNames, !IO),

        MmakeRuleDateOptInt0Deps = mmake_flat_rule("dates_on_opts_and_int0s",
            mmake_rule_is_not_phony,
            Targets,
            OptDepsFileNames ++ OptInt0DepsFileNames,
            []),
        gather_mmake_entry(DepStream, MmakeRuleDateOptInt0Deps,
            !MmakeFile, !IO)

        ),

        (
            MaybeTransOptDeps1 = yes(TransOptDeps2),
            some [Targets] (

            io.write_strings(DepStream, [
                "\n\n",
                ErrFileName, " ",
                CDateFileName, " ",
                JavaDateFileName, " : "
            ], !IO),
            Targets = one_or_more(ErrFileName,
                [CDateFileName, JavaDateFileName]),

            write_dependencies_list(Globals, DepStream, ".trans_opt",
                TransOptDeps2, !IO),
            io.write_string(DepStream, "\n\n", !IO),
            make_module_file_names_with_suffix(Globals, ".trans_opt",
                TransOptDeps2, TransOptDepsOptFileNames, !IO),

            MmakeRuleTransOptOpts = mmake_flat_rule(
                "dates_on_trans_opts'_opts",
                mmake_rule_is_not_phony,
                Targets,
                TransOptDepsOptFileNames,
                []),
            gather_mmake_entry(DepStream, MmakeRuleTransOptOpts,
                !MmakeFile, !IO)

            )
        ;
            MaybeTransOptDeps1 = no
        )
    else
        true
    ),

    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    globals.get_target(Globals, CompilationTarget),
    ( if
        HighLevelCode = yes,
        CompilationTarget = target_c
    then
        % For --high-level-code with --target c, we need to make sure that
        % we generate the header files for imported modules before compiling
        % the C files, since the generated C files #include those header files.

        some [Targets, AllDepsFileNames] (
        io.write_strings(DepStream, [
            "\n\n",
            PicObjFileName, " ",
            ObjFileName, " :"
        ], !IO),
        Targets = one_or_more(PicObjFileName, [ObjFileName]),
        write_dependencies_set(Globals, DepStream, ".mih", AllDeps, !IO),
        io.write_string(DepStream, "\n\n", !IO),

        make_module_file_names_with_suffix(Globals, ".mih",
            set.to_sorted_list(AllDeps), AllDepsFileNames, !IO),

        MmakeRuleObjOnMihs = mmake_flat_rule("objs_on_mihs",
            mmake_rule_is_not_phony,
            Targets,
            AllDepsFileNames,
            []),
        gather_mmake_entry(DepStream, MmakeRuleObjOnMihs, !MmakeFile, !IO)
        )
    else
        true
    ),

    % We need to tell make how to make the header files. The header files
    % are actually built by the same command that creates the .c or .s file,
    % so we just make them depend on the .c or .s files. This is needed
    % for the --high-level-code rule above, and for the rules introduced for
    % `:- pragma foreign_import_module' declarations. In some grades the header
    % file won't actually be built (e.g. LLDS grades for modules not containing
    % `:- pragma export' declarations), but this rule won't do any harm.

    module_name_to_file_name(Globals, do_not_create_dirs, ".c",
        ModuleName, CFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".mh",
        ModuleName, MhHeaderFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".mih",
        ModuleName, MihHeaderFileName, !IO),
    io.write_strings(DepStream, [
        "\n\n",
        MhHeaderFileName, " ", MihHeaderFileName, " : ", CFileName, "\n\n"
    ], !IO),

    MmakeRuleMhMihOnC = mmake_flat_rule("mh_and_mih_on_c",
        mmake_rule_is_not_phony,
        one_or_more(MhHeaderFileName, [MihHeaderFileName]),
        [CFileName],
        []),
    gather_mmake_entry(DepStream, MmakeRuleMhMihOnC, !MmakeFile, !IO),

    % The `.module_dep' file is made as a side effect of
    % creating the `.c', `.s', or `.java'.

    module_name_to_file_name(Globals, do_not_create_dirs, ".java",
        ModuleName, JavaFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs,
        make_module_dep_file_extension, ModuleName, ModuleDepFileName, !IO),
    io.write_strings(DepStream, [
        "\n\n",
        "ifeq ($(findstring java,$(GRADE)),java)\n",
        ModuleDepFileName, " : ", JavaFileName, "\n",
        "else\n",
        ModuleDepFileName, " : ", CFileName, "\n",
        "endif\n\n"
    ], !IO),

    MmakeFragmentModuleDep = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        mmake_simple_rule("module_dep_on_java",
            mmake_rule_is_not_phony,
            ModuleDepFileName,
            [JavaFileName],
            []),
        mmake_simple_rule("module_dep_on_c",
            mmake_rule_is_not_phony,
            ModuleDepFileName,
            [CFileName],
            [])
    ),
    gather_mmake_fragment(DepStream, MmakeFragmentModuleDep, !MmakeFile, !IO),

    % The .date and .date0 files depend on the .int0 files for the parent
    % modules, and the .int3 files for the directly and indirectly imported
    % modules.
    %
    % For nested submodules, the `.date' files for the parent modules
    % also depend on the same things as the `.date' files for this module,
    % since all the `.date' files will get produced by a single mmc command.
    % Similarly for `.date0' files, except these don't depend on the `.int0'
    % files, because when doing the `--make-private-interface' for nested
    % modules, mmc will process the modules in outermost to innermost order
    % so as to produce each `.int0' file before it is needed.

    module_name_to_file_name(Globals, do_not_create_dirs, ".date",
        ModuleName, DateFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".date0",
        ModuleName, Date0FileName, !IO),

    io.write_strings(DepStream, ["\n\n", DateFileName, " ", Date0FileName],
        !IO),
    write_dependencies_set(Globals, DepStream, ".date", ParentDeps, !IO),
    io.write_strings(DepStream, [" : ", SourceFileName], !IO),
    write_dependencies_set(Globals, DepStream, ".int0", ParentDeps, !IO),
    write_dependencies_set(Globals, DepStream, ".int3", LongDeps, !IO),
    write_dependencies_set(Globals, DepStream, ".int3", ShortDeps, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    make_module_file_names_with_suffix(Globals, ".date",
        set.to_sorted_list(ParentDeps), ParentDepDateFileNames, !IO),
    make_module_file_names_with_suffix(Globals, ".int0",
        set.to_sorted_list(ParentDeps), ParentDepInt0FileNames, !IO),
    make_module_file_names_with_suffix(Globals, ".int3",
        set.to_sorted_list(LongDeps), LongDepInt3FileNames, !IO),
    make_module_file_names_with_suffix(Globals, ".int3",
        set.to_sorted_list(ShortDeps), ShortDepInt3FileNames, !IO),
    MmakeRuleParentDates = mmake_general_rule("self_and_parent_date_deps",
        mmake_rule_is_not_phony,
        one_or_more(
            mmake_file_name_group("",
                one_or_more(DateFileName,
                    [Date0FileName | ParentDepDateFileNames])),
            []),
        [make_singleton_file_name_group(SourceFileName)] ++
            make_file_name_group("parent dep int0", ParentDepInt0FileNames) ++
            make_file_name_group("long dep int3s", LongDepInt3FileNames) ++
            make_file_name_group("short dep int3s", ShortDepInt3FileNames),
        []),
    gather_mmake_entry(DepStream, MmakeRuleParentDates, !MmakeFile, !IO),

    io.write_strings(DepStream, ["\n\n", Date0FileName], !IO),
    write_dependencies_set(Globals, DepStream, ".date0", ParentDeps, !IO),
    io.write_strings(DepStream, [" : ", SourceFileName], !IO),
    write_dependencies_set(Globals, DepStream, ".int3", LongDeps, !IO),
    write_dependencies_set(Globals, DepStream, ".int3", ShortDeps, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    make_module_file_names_with_suffix(Globals, ".date0",
        set.to_sorted_list(ParentDeps), ParentDepDate0FileNames, !IO),
    MmakeRuleParentDate0s = mmake_general_rule("self_and_parent_date0_deps",
        mmake_rule_is_not_phony,
        one_or_more(
            mmake_file_name_group("",
                one_or_more(Date0FileName, ParentDepDate0FileNames)),
            []),
        [make_singleton_file_name_group(SourceFileName)] ++
            make_file_name_group("long dep int3s", LongDepInt3FileNames) ++
            make_file_name_group("short dep int3s", ShortDepInt3FileNames),
        []),
    gather_mmake_entry(DepStream, MmakeRuleParentDate0s, !MmakeFile, !IO),

    % If we can pass the module name rather than the file name, then do so.
    % `--smart-recompilation' doesn't work if the file name is passed
    % and the module name doesn't match the file name.

    have_source_file_map(HaveMap, !IO),
    (
        HaveMap = yes,
        module_name_to_file_name_stem(SourceFileModuleName, ModuleArg)
    ;
        HaveMap = no,
        ModuleArg = SourceFileName
    ),

    (
        ContainsForeignCode = contains_foreign_code(_LangSet),
        ForeignImportModules = ForeignImportModules0
    ;
        ContainsForeignCode = contains_no_foreign_code,
        ForeignImportModules = ForeignImportModules0
    ;
        ContainsForeignCode = contains_foreign_code_unknown,
        get_foreign_code_indicators_from_item_blocks(Globals,
            SrcItemBlocks,
            _SrcLangSet, SrcForeignImportModules, _, _),
        % XXX ITEM_LIST DirectIntItemBlocksCord should not be needed
        % XXX ITEM_LIST IndirectIntItemBlocksCord should not be needed
        IntItemBlocksCord =
            DirectIntItemBlocksCord ++ IndirectIntItemBlocksCord,
        get_foreign_code_indicators_from_item_blocks(Globals,
            cord.list(IntItemBlocksCord),
            _IntLangSet, IntForeignImportModules, _, _),
        get_foreign_code_indicators_from_item_blocks(Globals,
            cord.list(OptItemBlocksCord),
            _OptLangSet, OptForeignImportModules, _, _),
        get_foreign_code_indicators_from_item_blocks(Globals,
            cord.list(IntForOptItemBlocksCord),
            _IntForOptLangSet, IntForOptForeignImportModules, _, _),
        % If we are generating the `.dep' file, ForeignImportModuless0
        % will contain a conservative approximation to the set of foreign
        % imports needed which will include imports required by imported
        % modules. XXX ITEM_LIST What is the correctness argument that supports
        % the above assertion?
        ( if
            ForeignImportModules0 = foreign_import_modules(
                C0, CSharp0, Java0, Erlang0),
            set.is_empty(C0),
            set.is_empty(CSharp0),
            set.is_empty(Java0),
            set.is_empty(Erlang0)
        then
            SrcForeignImportModules = foreign_import_modules(
                SrcC, SrcCSharp, SrcJava, SrcErlang),
            IntForeignImportModules = foreign_import_modules(
                IntC, IntCSharp, IntJava, IntErlang),
            OptForeignImportModules = foreign_import_modules(
                OptC, OptCSharp, OptJava, OptErlang),
            IntForOptForeignImportModules = foreign_import_modules(
                IntForOptC, IntForOptCSharp, IntForOptJava,
                IntForOptErlang),
            C = set.union_list([SrcC, IntC, OptC, IntForOptC]),
            CSharp = set.union_list([
                SrcCSharp, IntCSharp, OptCSharp, IntForOptCSharp]),
            Java= set.union_list([SrcJava, IntJava, OptJava, IntForOptJava]),
            Erlang = set.union_list([
                SrcErlang, IntErlang, OptErlang, IntForOptErlang]),
            ForeignImportModules = foreign_import_modules(
                C, CSharp, Java, Erlang)
        else
            ForeignImportModules = ForeignImportModules0
        )
    ),

    ForeignImports = get_all_foreign_import_module_infos(ForeignImportModules),

    % Handle dependencies introduced by
    % `:- pragma foreign_import_module' declarations.

    set.filter_map(
        ( pred(ForeignImportMod::in, ImportModuleName::out) is semidet :-
            ImportModuleName = foreign_import_module_name_from_module(
                ForeignImportMod, SourceFileModuleName),

            % XXX We can't include mercury.dll as mmake can't find it,
            % but we know that it exists.
            ImportModuleName \= unqualified("mercury")
        ), ForeignImports, ForeignImportedModuleNames),
    ( if set.is_empty(ForeignImportedModuleNames) then
        true
    else
        globals.get_target(Globals, Target),
        (
            Target = target_csharp,
            % XXX don't know enough about C# yet
            ForeignImportTargets = [],
            ForeignImportExt = ".cs"
        ;
            Target = target_java,
            module_name_to_file_name(Globals, do_not_create_dirs, ".class",
                ModuleName, ClassFileName, !IO),
            ForeignImportTargets = [ClassFileName],
            ForeignImportExt = ".java"
        ;
            Target = target_erlang,
            module_name_to_file_name(Globals, do_not_create_dirs, ".beam",
                ModuleName, BeamFileName, !IO),
            ForeignImportTargets = [BeamFileName],
            ForeignImportExt = ".hrl"
        ;
            Target = target_c,
            % NOTE: for C the possible targets might be a .o file _or_ a
            % .pic_o file. We need to include dependencies for the latter
            % otherwise invoking mmake with a <module>.pic_o target will break.
            ForeignImportTargets = [ObjFileName, PicObjFileName],
            ForeignImportExt = ".mh"
        ),
        % XXX Instead of generating a separate rule for each target in
        % ForeignImportTargets, generate one rule with all those targets
        % before the colon.
        list.map_foldl(
            write_foreign_import_deps(DepStream, Globals, ForeignImportExt,
                set.to_sorted_list(ForeignImportedModuleNames)),
            ForeignImportTargets, MmakeRulesForeignImports, !IO),
        gather_mmake_entries(DepStream, MmakeRulesForeignImports,
            !MmakeFile, !IO)
    ),

    module_name_to_file_name(Globals, do_not_create_dirs, ".int",
        ModuleName, IntFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".int2",
        ModuleName, Int2FileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".int3",
        ModuleName, Int3FileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".opt",
        ModuleName, OptFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".trans_opt",
        ModuleName, TransOptFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".date3",
        ModuleName, Date3FileName, !IO),

    % We add some extra dependencies to the generated `.d' files, so that
    % local `.int', `.opt', etc. files shadow the installed versions properly
    % (e.g. for when you're trying to build a new version of an installed
    % library). This saves the user from having to add these explicitly
    % if they have multiple libraries installed in the same installation
    % hierarchy which aren't independent (e.g. one uses another). These extra
    % dependencies are necessary due to the way the combination of search paths
    % and pattern rules works in Make.
    %
    % Be very careful about changing the following rules. The `@:' is a
    % silent do-nothing command. It is used to force GNU Make to recheck
    % the timestamp on the target file. (It is a pity that GNU Make
    % doesn't have a way of handling these sorts of rules in a nicer manner.)

    io.write_strings(DepStream, [
        "\n",
        Int0FileName, " : ", Date0FileName, "\n",
        "\t@:\n",
        IntFileName, " : ", DateFileName, "\n",
        "\t@:\n",
        Int2FileName, " : ", DateFileName, "\n",
        "\t@:\n",
        Int3FileName, " : ", Date3FileName, "\n",
        "\t@:\n",
        OptFileName, " : ", OptDateFileName, "\n",
        "\t@:\n",
        TransOptFileName, " : ", TransOptDateFileName, "\n",
        "\t@:\n",
        "\n"
    ], !IO),

    MmakeRulesInstallShadows = [
        mmake_simple_rule("", mmake_rule_is_not_phony,
            Int0FileName, [Date0FileName], [silent_noop_action]),
        mmake_simple_rule("", mmake_rule_is_not_phony,
            IntFileName, [DateFileName], [silent_noop_action]),
        mmake_simple_rule("", mmake_rule_is_not_phony,
            Int2FileName, [DateFileName], [silent_noop_action]),
        mmake_simple_rule("", mmake_rule_is_not_phony,
            Int3FileName, [Date3FileName], [silent_noop_action]),
        mmake_simple_rule("", mmake_rule_is_not_phony,
            OptFileName, [OptDateFileName], [silent_noop_action]),
        mmake_simple_rule("", mmake_rule_is_not_phony,
            TransOptFileName, [TransOptDateFileName], [silent_noop_action])
    ],
    gather_mmake_entries(DepStream, MmakeRulesInstallShadows, !MmakeFile, !IO),

    globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
    (
        UseSubdirs = yes,
        io.nl(DepStream, !IO),
        SubDirShorthandExts =
            [".c", ".$O", ".pic_o", ".java", ".class", ".dll"],
        list.map_foldl(
            write_subdirs_shorthand_rule(Globals, DepStream, ModuleName),
            SubDirShorthandExts, MmakeRulesSubDirShorthand, !IO),
        gather_mmake_entries(DepStream, MmakeRulesSubDirShorthand,
            !MmakeFile, !IO)
    ;
        UseSubdirs = no
    ),

    ( if SourceFileName = default_source_file(ModuleName) then
        true
    else
        % The pattern rules in Mmake.rules won't work, since the source file
        % name doesn't match the expected source file name for this module
        % name. This can occur due to just the use of different source file
        % names, or it can be due to the use of nested modules. So we need
        % to output hard-coded rules in this case.
        %
        % The rules output below won't work in the case of nested modules
        % with parallel makes, because it will end up invoking the same command
        % twice (since it produces two output files) at the same time.
        %
        % Any changes here will require corresponding changes to
        % scripts/Mmake.rules. See that file for documentation on these rules.

        io.write_strings(DepStream, [
            "\n",
            Date0FileName, " : ", SourceFileName, "\n",
            "\t$(MCPI) $(ALL_GRADEFLAGS) ",
                "$(ALL_MCPIFLAGS) ", ModuleArg, "\n",
            DateFileName, " : ", SourceFileName, "\n",
            "\t$(MCI) $(ALL_GRADEFLAGS) $(ALL_MCIFLAGS) ",
                ModuleArg, "\n",
            Date3FileName, " : ", SourceFileName, "\n",
            "\t$(MCSI) $(ALL_GRADEFLAGS) $(ALL_MCSIFLAGS) ",
                ModuleArg, "\n",
            OptDateFileName, " : ", SourceFileName, "\n",
            "\t$(MCOI) $(ALL_GRADEFLAGS) $(ALL_MCOIFLAGS) ",
                ModuleArg, "\n",
            TransOptDateFileName, " : ", SourceFileName, "\n",
            "\t$(MCTOI) $(ALL_GRADEFLAGS) ",
                "$(ALL_MCTOIFLAGS) ", ModuleArg, "\n",
            CDateFileName, " : ", SourceFileName, "\n",
            "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                ModuleArg, " $(ERR_REDIRECT)\n",
            JavaDateFileName, " : ", SourceFileName, "\n",
            "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                "--java-only ", ModuleArg, " $(ERR_REDIRECT)\n\n"
        ], !IO),

        MmakeRulesPattern = [
            mmake_simple_rule("", mmake_rule_is_not_phony,
                Date0FileName, [SourceFileName],
                ["$(MCPI) $(ALL_GRADEFLAGS) $(ALL_MCPIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("", mmake_rule_is_not_phony,
                DateFileName, [SourceFileName],
                ["$(MCI) $(ALL_GRADEFLAGS) $(ALL_MCIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("", mmake_rule_is_not_phony,
                Date3FileName, [SourceFileName],
                ["$(MCSI) $(ALL_GRADEFLAGS) $(ALL_MCSIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("", mmake_rule_is_not_phony,
                OptDateFileName, [SourceFileName],
                ["$(MCOI) $(ALL_GRADEFLAGS) $(ALL_MCOIFLAGS) " ++ ModuleArg]),
            mmake_simple_rule("", mmake_rule_is_not_phony,
                TransOptDateFileName, [SourceFileName],
                ["$(MCTOI) $(ALL_GRADEFLAGS) $(ALL_MCTOIFLAGS) " ++
                    ModuleArg]),
            mmake_simple_rule("", mmake_rule_is_not_phony,
                CDateFileName, [SourceFileName],
                ["$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) " ++ ModuleArg ++
                    " $(ERR_REDIRECT)"]),
            mmake_simple_rule("", mmake_rule_is_not_phony,
                JavaDateFileName, [SourceFileName],
                ["$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) --java-only " ++
                    ModuleArg ++ " $(ERR_REDIRECT)"])
        ],
        gather_mmake_entries(DepStream, MmakeRulesPattern, !MmakeFile, !IO)
    ).

:- pred write_nested_deps(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in, string::in, mmake_entry::out,
    io::di, io::uo) is det.

write_nested_deps(DepStream, Globals, ModuleName, NestedDeps, Ext, MmakeRule,
        !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, Ext,
        ModuleName, ExtName, !IO),
    io.write_strings(DepStream, ["\n", ExtName, " : "], !IO),
    write_dependencies_list(Globals, DepStream, Ext, NestedDeps, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    make_module_file_names_with_suffix(Globals, Ext,
        NestedDeps, NestedDepsFileNames, !IO),
    MmakeRule = mmake_simple_rule("nested_deps_for_" ++ Ext,
        mmake_rule_is_not_phony,
        ExtName,
        NestedDepsFileNames,
        []).

:- pred write_foreign_import_deps(io.text_output_stream::in, globals::in,
    string::in, list(module_name)::in, string::in, mmake_entry::out,
    io::di, io::uo) is det.

write_foreign_import_deps(DepStream, Globals, ForeignImportExt,
        ForeignImportedModuleNames, ForeignImportTarget, MmakeRule, !IO) :-
    io.write_string(DepStream, "\n\n", !IO),
    io.write_string(DepStream, ForeignImportTarget, !IO),
    io.write_string(DepStream, " : ", !IO),
    write_dependencies_list(Globals, DepStream, ForeignImportExt,
        ForeignImportedModuleNames, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    make_module_file_names_with_suffix(Globals, ForeignImportExt,
        ForeignImportedModuleNames, ForeignImportedFileNames, !IO),
    MmakeRule = mmake_simple_rule("foreign_deps_for_" ++ ForeignImportExt,
        mmake_rule_is_not_phony,
        ForeignImportTarget,
        ForeignImportedFileNames,
        []).

%---------------------------------------------------------------------------%
%
% These predicates take a list of module names and a suffix,
% find the filename that contains each module with that suffix,
% and write out the result directly.
%
% When constructing mmakefiles, we want to do the same job
% *without* the final output. The predicates that do that are
% make_module_file_names_with_suffix and
% make_module_file_name_group_with_suffix.
%

:- pred write_dependencies_set(globals::in, io.output_stream::in,
    string::in, set(module_name)::in, io::di, io::uo) is det.

write_dependencies_set(Globals, DepStream, Suffix, Modules, !IO) :-
    write_dependencies_list(Globals, DepStream, Suffix,
        set.to_sorted_list(Modules), !IO).

:- pred write_dependencies_list(globals::in, io.output_stream::in,
    string::in, list(module_name)::in, io::di, io::uo) is det.

write_dependencies_list(_, _, _, [], !IO).
write_dependencies_list(Globals, DepStream, Suffix, [Module | Modules], !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, Suffix,
        Module, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_dependencies_list(Globals, DepStream, Suffix, Modules, !IO).

%---------------------%

:- pred make_module_file_names_with_suffix(globals::in,
    string::in, list(module_name)::in, list(mmake_file_name)::out,
    io::di, io::uo) is det.

make_module_file_names_with_suffix(Globals, Suffix,
        Modules, FileNames, !IO) :-
    list.map_foldl(
        module_name_to_file_name(Globals, do_not_create_dirs, Suffix),
        Modules, FileNames, !IO).

:- pred make_module_file_name_group_with_suffix(globals::in, string::in,
    string::in, set(module_name)::in, list(mmake_file_name_group)::out,
    io::di, io::uo) is det.

make_module_file_name_group_with_suffix(Globals, GroupName, Suffix,
        Modules, Groups, !IO) :-
    list.map_foldl(
        module_name_to_file_name(Globals, do_not_create_dirs, Suffix),
        set.to_sorted_list(Modules), FileNames, !IO),
    Groups = make_file_name_group(GroupName, FileNames).

%---------------------------------------------------------------------------%

:- pred write_compact_dependencies_list(io.output_stream::in,
    string::in, string::in, pair(string)::in, io::di, io::uo) is det.

write_compact_dependencies_list(DepStream, Prefix, Suffix, Basis, !IO) :-
    Basis = VarName - OldSuffix,
    io.write_string(DepStream, "$(", !IO),
    io.write_string(DepStream, VarName, !IO),
    io.write_string(DepStream, ":%", !IO),
    io.write_string(DepStream, OldSuffix, !IO),
    io.write_string(DepStream, "=", !IO),
    io.write_string(DepStream, Prefix, !IO),
    io.write_string(DepStream, "%", !IO),
    io.write_string(DepStream, Suffix, !IO),
    io.write_string(DepStream, ")", !IO).

% Not currently needed.
%
% :- pred write_dll_dependencies_list(globals::in, io.output_stream::in,
%     string::in, list(module_name)::in, io::di, io::uo) is det.
%
% write_dll_dependencies_list(_Globals, _DepStream, _Prefix, [], !IO).
% write_dll_dependencies_list(Globals, DepStream, Prefix, [Module | Modules],
%         !IO) :-
%     write_dll_dependency(Globals, DepStream, Prefix, Module, !IO),
%     write_dll_dependencies_list(Globals, DepStream, Prefix, Modules, !IO).
%
% :- pred write_dll_dependency(globals::in, io.output_stream::in, string::in,
%     module_name::in, io::di, io::uo) is det.
%
% write_dll_dependency(Globals, DepStream, Prefix, Module, !IO) :-
%     module_name_to_file_name(Globals, Module, ".dll", do_not_create_dirs,
%         FileName, !IO),
%     io.write_string(DepStream, " \\\n\t", !IO),
%     io.write_string(DepStream, Prefix, !IO),
%     io.write_string(DepStream, FileName, !IO).

:- pred write_foreign_include_file_dependencies_list(io.output_stream::in,
    file_name::in, list(foreign_include_file_info)::in, io::di, io::uo) is det.

write_foreign_include_file_dependencies_list(DepStream, SourceFileName,
        IncludeFiles, !IO) :-
    list.foldl(
        write_foreign_include_file_dependency(DepStream, SourceFileName),
        IncludeFiles, !IO).

    % The version of this predicate that does not do the final output
    % is foreign_include_file_path_name.
    %
:- pred write_foreign_include_file_dependency(io.output_stream::in,
    file_name::in, foreign_include_file_info::in, io::di, io::uo) is det.

write_foreign_include_file_dependency(DepStream, SourceFileName,
        IncludeFile, !IO) :-
    IncludeFile = foreign_include_file_info(_Lang, IncludeFileName),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, IncludePath, !IO).

:- func foreign_include_file_path_name(file_name, foreign_include_file_info)
    = string.

foreign_include_file_path_name(SourceFileName, IncludeFile) = IncludePath :-
    IncludeFile = foreign_include_file_info(_Lang, IncludeFileName),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath).

:- pred write_fact_table_dependencies_list(globals::in, io.output_stream::in,
    string::in, module_name::in, list(file_name)::in, io::di, io::uo) is det.

write_fact_table_dependencies_list(_, _, _, _, [], !IO).
write_fact_table_dependencies_list(Globals, DepStream, Suffix, Module,
        [FactTable | FactTables], !IO) :-
    fact_table_file_name(Globals, Module, FactTable, Suffix,
        do_not_create_dirs, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_fact_table_dependencies_list(Globals, DepStream, Suffix, Module,
        FactTables, !IO).

:- pred write_extra_link_dependencies_list(globals::in, io.output_stream::in,
    string::in, assoc_list(file_name, module_name)::in, list(string)::out,
    io::di, io::uo) is det.

write_extra_link_dependencies_list(_, _, _, [], [], !IO).
write_extra_link_dependencies_list(Globals, DepStream, Suffix,
        [ExtraLink - Module | ExtraLinks], [FileName | FileNames], !IO) :-
    extra_link_obj_file_name(Globals, Module, ExtraLink, Suffix,
        do_not_create_dirs, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_extra_link_dependencies_list(Globals, DepStream, Suffix,
        ExtraLinks, FileNames, !IO).

:- pred write_file_dependencies_list(io.output_stream::in, string::in,
    list(string)::in, io::di, io::uo) is det.

write_file_dependencies_list(_, _, [], !IO).
write_file_dependencies_list(DepStream, Suffix, [FileName | FileNames], !IO) :-
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    io.write_string(DepStream, Suffix, !IO),
    write_file_dependencies_list(DepStream, Suffix, FileNames, !IO).

    % With `--use-subdirs', allow users to type `mmake module.c'
    % rather than `mmake Mercury/cs/module.c'.
    %
:- pred write_subdirs_shorthand_rule(globals::in, io.output_stream::in,
    module_name::in, string::in, mmake_entry::out, io::di, io::uo) is det.

write_subdirs_shorthand_rule(Globals, DepStream, ModuleName, Ext, MmakeRule,
        !IO) :-
    module_name_to_file_name_stem(ModuleName, ModuleStr),
    module_name_to_file_name(Globals, do_not_create_dirs, Ext,
        ModuleName, Target, !IO),
    ShorthandTarget = ModuleStr ++ Ext,
    io.write_string(DepStream, ".PHONY: ", !IO),
    io.write_string(DepStream, ShorthandTarget, !IO),
    io.nl(DepStream, !IO),
    io.write_string(DepStream, ShorthandTarget, !IO),
    io.write_string(DepStream, ": ", !IO),
    io.write_string(DepStream, Target, !IO),
    io.nl(DepStream, !IO),

    MmakeRule = mmake_simple_rule("subdir_shorthand_for_" ++ Ext,
        mmake_rule_is_phony, ShorthandTarget, [Target], []).

%---------------------------------------------------------------------------%

generate_dependencies_write_d_files(_, [], _, _, _, _, _, _, !IO).
generate_dependencies_write_d_files(Globals, [Dep | Deps],
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptOrder, DepsMap, !IO) :-
    generate_dependencies_write_d_file(Globals, Dep,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptOrder, DepsMap, !IO),
    generate_dependencies_write_d_files(Globals, Deps,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptOrder, DepsMap, !IO).

:- pred generate_dependencies_write_d_file(globals::in, deps::in,
    deps_graph::in, deps_graph::in, deps_graph::in, deps_graph::in,
    list(module_name)::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies_write_d_file(Globals, Dep,
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptOrder, _DepsMap, !IO) :-
    % XXX The fact that _DepsMap is unused here may be a bug.
    %
    % XXX Updating !ModuleAndImports does not look a correct thing to do
    % in this predicate, since it doesn't actually process any module imports.
    some [!ModuleAndImports] (
        Dep = deps(_, !:ModuleAndImports),

        % Look up the interface/implementation/indirect dependencies
        % for this module from the respective dependency graphs,
        % and save them in the module_and_imports structure.

        module_and_imports_get_module_name(!.ModuleAndImports, ModuleName),
        get_dependencies_from_graph(IndirectOptDepsGraph, ModuleName,
            IndirectOptDeps),
        globals.lookup_bool_option(Globals, intermodule_optimization,
            Intermod),
        (
            Intermod = yes,
            % Be conservative with inter-module optimization -- assume a
            % module depends on the `.int', `.int2' and `.opt' files
            % for all transitively imported modules.
            IntDeps = IndirectOptDeps,
            ImpDeps = IndirectOptDeps,
            IndirectDeps = IndirectOptDeps
        ;
            Intermod = no,
            get_dependencies_from_graph(IntDepsGraph, ModuleName, IntDeps),
            get_dependencies_from_graph(ImpDepsGraph, ModuleName, ImpDeps),
            get_dependencies_from_graph(IndirectDepsGraph, ModuleName,
                IndirectDeps)
        ),

        % Assume we need the `.mh' files for all imported modules
        % (we will if they define foreign types).
        % XXX This overly conservative assumption can lead to a lot of
        % unnecessary recompilations.
        ForeignImportModules0 = init_foreign_import_modules,
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            ForeignImportModules =
                ForeignImportModules0 ^ fim_c := IndirectOptDeps
        ;
            Target = target_csharp,
            ForeignImportModules =
                ForeignImportModules0 ^ fim_csharp := IndirectOptDeps
        ;
            Target = target_java,
            ForeignImportModules =
                ForeignImportModules0 ^ fim_java := IndirectOptDeps
        ;
            Target = target_erlang,
            ForeignImportModules =
                ForeignImportModules0 ^ fim_erlang := IndirectOptDeps
        ),
        !ModuleAndImports ^ mai_foreign_import_modules := ForeignImportModules,

        module_and_imports_set_int_deps(IntDeps, !ModuleAndImports),
        module_and_imports_set_imp_deps(ImpDeps, !ModuleAndImports),
        module_and_imports_set_indirect_deps(IndirectDeps, !ModuleAndImports),

        % Compute the trans-opt dependencies for this module. To avoid
        % the possibility of cycles, each module is only allowed to depend
        % on modules that occur later than it in the TransOptOrder.

        FindModule = (pred(OtherModule::in) is semidet :-
            ModuleName \= OtherModule
        ),
        list.drop_while(FindModule, TransOptOrder, TransOptDeps0),
        ( if TransOptDeps0 = [_ | TransOptDeps1] then
            % The module was found in the list.
            TransOptDeps = TransOptDeps1
        else
            TransOptDeps = []
        ),

        % Note that even if a fatal error occured for one of the files
        % that the current Module depends on, a .d file is still produced,
        % even though it probably contains incorrect information.
        Errors = !.ModuleAndImports ^ mai_errors,
        set.intersect(Errors, fatal_read_module_errors, FatalErrors),
        ( if set.is_empty(FatalErrors) then
            write_dependency_file(Globals, !.ModuleAndImports, IndirectOptDeps,
                yes(TransOptDeps), !IO)
        else
            true
        )
    ).

:- pred get_dependencies_from_graph(deps_graph::in, module_name::in,
    set(module_name)::out) is det.

get_dependencies_from_graph(DepsGraph0, ModuleName, Dependencies) :-
    digraph.add_vertex(ModuleName, ModuleKey, DepsGraph0, DepsGraph),
    digraph.lookup_key_set_from(DepsGraph, ModuleKey, DepsKeysSet),
    AddKeyDep =
        ( pred(Key::in, Deps0::in, Deps::out) is det :-
            digraph.lookup_vertex(DepsGraph, Key, Dep),
            set.insert(Dep, Deps0, Deps)
        ),
    sparse_bitset.foldl(AddKeyDep, DepsKeysSet, set.init, Dependencies).

%---------------------------------------------------------------------------%

generate_dependencies_write_dv_file(Globals, SourceFileName, ModuleName,
        DepsMap, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_file_name(Globals, do_create_dirs, ".dv",
        ModuleName, DvFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DvFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io.open_output(DvFileName, DvResult, !IO),
    (
        DvResult = ok(DvStream),
        start_mmakefile(DvStream, MmakeFile0, !IO),
        generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap,
            DvStream, MmakeFile0, MmakeFile, !IO),
        end_mmakefile(DvStream, MmakeFile, !IO),
        io.close_output(DvStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        DvResult = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.append_list(["error opening file `", DvFileName,
            "' for output: ", IOErrorMessage], DvMessage),
        report_error(DvMessage, !IO)
    ).

%---------------------------------------------------------------------------%

generate_dependencies_write_dep_file(Globals, SourceFileName, ModuleName,
        DepsMap, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_file_name(Globals, do_create_dirs, ".dep",
        ModuleName, DepFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DepFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io.open_output(DepFileName, DepResult, !IO),
    (
        DepResult = ok(DepStream),
        start_mmakefile(DepStream, MmakeFile0, !IO),
        generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap,
            DepStream, MmakeFile0, MmakeFile, !IO),
        end_mmakefile(DepStream, MmakeFile, !IO),
        io.close_output(DepStream, !IO),
        maybe_write_string(Verbose, "% done.\n", !IO)
    ;
        DepResult = error(IOError),
        maybe_write_string(Verbose, " failed.\n", !IO),
        maybe_flush_output(Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.append_list(["error opening file `", DepFileName,
            "' for output: ", IOErrorMessage], DepMessage),
        report_error(DepMessage, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred generate_dv_file(globals::in, file_name::in, module_name::in,
    deps_map::in, io.output_stream::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap, DepStream,
        !MmakeFile, !IO) :-
    io.write_string(DepStream,
        "# vim: ts=8 sw=8 noexpandtab ft=make\n\n", !IO),
    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),

    io.write_string(DepStream,
        "# Automatically generated dependency variables for module `", !IO),
    io.write_string(DepStream, ModuleNameString, !IO),
    io.write_string(DepStream, "'\n", !IO),
    io.write_string(DepStream, "# generated from source file `", !IO),
    io.write_string(DepStream, SourceFileName, !IO),
    io.write_string(DepStream, "'\n", !IO),

    io.write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io.write_string(DepStream, Version, !IO),
    io.write_string(DepStream, ",\n# configured for ", !IO),
    io.write_string(DepStream, FullArch, !IO),
    io.write_string(DepStream, ".\n\n", !IO),

    MmakeStartComment = mmake_start_comment("dependency variables",
        ModuleNameString, SourceFileName, Version, FullArch),
    gather_mmake_entry(DepStream, MmakeStartComment, !MmakeFile, !IO),

    map.keys(DepsMap, Modules0),
    select_ok_modules(Modules0, DepsMap, Modules1),
    list.sort(compare_module_names, Modules1, Modules),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),
    list.map(get_source_file(DepsMap), Modules, SourceFiles0),
    list.sort_and_remove_dups(SourceFiles0, SourceFiles),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".ms =", !IO),
    write_file_dependencies_list(DepStream, ".m", SourceFiles, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarModuleMs = mmake_var_defn_list(ModuleMakeVarName ++ ".ms",
        list.map(add_suffix(".m"), SourceFiles)),
    gather_mmake_entry(DepStream, MmakeVarModuleMs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".errs =", !IO),
    write_file_dependencies_list(DepStream, ".err", SourceFiles, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarModuleErrs = mmake_var_defn_list(ModuleMakeVarName ++ ".errs",
        list.map(add_suffix(".err"), SourceFiles)),
    gather_mmake_entry(DepStream, MmakeVarModuleErrs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".mods =", !IO),
    write_dependencies_list(Globals, DepStream, "", Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    make_module_file_names_with_suffix(Globals, "", Modules,
        ModulesSourceFileNames, !IO),
    MmakeVarModuleMods = mmake_var_defn_list(ModuleMakeVarName ++ ".mods",
        ModulesSourceFileNames),
    gather_mmake_entry(DepStream, MmakeVarModuleMods, !MmakeFile, !IO),

    % The modules for which we need to generate .int0 files.
    ModulesWithSubModules = list.filter(
        ( pred(Module::in) is semidet :-
            map.lookup(DepsMap, Module, deps(_, ModuleImports)),
            set.non_empty(ModuleImports ^ mai_children)
        ), Modules),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".parent_mods =", !IO),
    write_dependencies_list(Globals, DepStream, "", ModulesWithSubModules,
        !IO),
    io.write_string(DepStream, "\n", !IO),

    make_module_file_names_with_suffix(Globals, "", ModulesWithSubModules,
        ModulesWithSubModulesSourceFileNames, !IO),
    MmakeVarModuleParentMods = mmake_var_defn_list(
        ModuleMakeVarName ++ ".parent_mods",
        ModulesWithSubModulesSourceFileNames),
    gather_mmake_entry(DepStream, MmakeVarModuleParentMods, !MmakeFile, !IO),

    globals.get_target(Globals, Target),
    (
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        ForeignModulesAndExts = []
    ),
    ForeignModules = assoc_list.keys(ForeignModulesAndExts),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".foreign =", !IO),
    write_dependencies_list(Globals, DepStream, "", ForeignModules, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    make_module_file_names_with_suffix(Globals, "", ForeignModules,
        ForeignModulesFileNames, !IO),
    MmakeVarForeignModules =
        mmake_var_defn_list(ModuleMakeVarName ++ ".foreign",
            ForeignModulesFileNames),
    gather_mmake_entry(DepStream, MmakeVarForeignModules, !MmakeFile, !IO),

    MakeFileName =
        ( pred(M - E::in, F::out, IO0::di, IO::uo) is det :-
            module_name_to_file_name(Globals, do_create_dirs, E, M, F0,
                IO0, IO),
            F = "$(os_subdir)" ++ F0
        ),
    list.map_foldl(MakeFileName, ForeignModulesAndExts, ForeignFileNames, !IO),

    % .foreign_cs are the source files which have had foreign code placed
    % in them.
    % XXX This rule looks wrong: why are we looking for (a) stuff with an
    % unknown suffix in (b) the os_subdir, when we (c) refer to it
    % using a make variable whose name ends in "_cs"?
    % Of course, since ForeignModulesAndExts is always zero with our current
    % set of target languages, this does not matter.
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".foreign_cs = ", !IO),
    write_file_dependencies_list(DepStream, "", ForeignFileNames, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarForeignFileNames = mmake_var_defn_list(
        ModuleMakeVarName ++ ".foreign_cs",
        ForeignFileNames),
    gather_mmake_entry(DepStream, MmakeVarForeignFileNames, !MmakeFile, !IO),

    ForeignVarName = ModuleMakeVarName ++ ".foreign",
    ForeignBasis = ForeignVarName - "",

    % The dlls that contain the foreign_code.
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".foreign_dlls = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(dlls_subdir)", ".dll", ForeignBasis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarForeignDlls = mmake_var_defn(ModuleMakeVarName ++ ".foreign_dlls",
        string.format("$(%s.foreign:%%=$(dlls_subdir)%%.dll)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarForeignDlls, !MmakeFile, !IO),

    ModsVarName = ModuleMakeVarName ++ ".mods",
    Basis = ModsVarName - "",

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".init_cs = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(cs_subdir)", ".c", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarInitCs = mmake_var_defn(ModuleMakeVarName ++ ".init_cs",
        string.format("$(%s.mods:%%=$(cs_subdir)%%.c)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarInitCs, !MmakeFile, !IO),

    get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".cs = $(", !IO),
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".init_cs) ", !IO),
    write_extra_link_dependencies_list(Globals, DepStream,
        ".c", ExtraLinkObjs, ExtraLinkObjFileNamesC, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarCs = mmake_var_defn_list(ModuleMakeVarName ++ ".cs",
        ["$(" ++ ModuleMakeVarName ++ ".init_cs)" | ExtraLinkObjFileNamesC]),
    gather_mmake_entry(DepStream, MmakeVarCs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".dlls = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(dlls_subdir)", ".dll", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDlls = mmake_var_defn(ModuleMakeVarName ++ ".dlls",
        string.format("$(%s.mods:%%=$(dlls_subdir)%%.dll)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDlls, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_os = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(os_subdir)", ".$O", Basis, !IO),
    write_extra_link_dependencies_list(Globals, DepStream,
        ".$O", ExtraLinkObjs, ExtraLinkObjFileNamesOs, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarAllOs = mmake_var_defn_list(ModuleMakeVarName ++ ".all_os",
        [string.format("$(%s.mods:%%=$(os_subdir)%%.$O)",
            [s(ModuleMakeVarName)]) |
        ExtraLinkObjFileNamesOs]),
    gather_mmake_entry(DepStream, MmakeVarAllOs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_os = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(os_subdir)", ".$(EXT_FOR_PIC_OBJECTS)", Basis, !IO),
    write_extra_link_dependencies_list(Globals, DepStream,
        ".$(EXT_FOR_PIC_OBJECTS)", ExtraLinkObjs, ExtraLinkObjFileNamesPicOs,
        !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarAllPicOs = mmake_var_defn_list(ModuleMakeVarName ++ ".all_pic_os",
        [string.format("$(%s.mods:%%=$(os_subdir)%%.$(EXT_FOR_PIC_OBJECTS))",
            [s(ModuleMakeVarName)]) |
        ExtraLinkObjFileNamesPicOs]),
    gather_mmake_entry(DepStream, MmakeVarAllPicOs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".os = $(", !IO),
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_os)\n", !IO),

    MmakeVarOs = mmake_var_defn(ModuleMakeVarName ++ ".os",
        string.format("$(%s.all_os)", [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarOs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".pic_os = $(", !IO),
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_os)\n", !IO),

    MmakeVarPicOs = mmake_var_defn(ModuleMakeVarName ++ ".pic_os",
        string.format("$(%s.all_pic_os)", [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarPicOs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".useds = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(useds_subdir)", ".used", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarUseds = mmake_var_defn(ModuleMakeVarName ++ ".useds",
        string.format("$(%s.mods:%%=$(used_subdir)%%.used)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarUseds, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".javas = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(javas_subdir)", ".java", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarJavas = mmake_var_defn(ModuleMakeVarName ++ ".javas",
        string.format("$(%s.mods:%%=$(javas_subdir)%%.java)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarJavas, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".classes = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(classes_subdir)", ".class", Basis, !IO),
    io.write_string(DepStream, " ", !IO),
    % The Java compiler creates a .class file for each class within the
    % original .java file. The filenames of all these can be matched with
    % `module\$*.class', hence the "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    io.write_string(DepStream, "$(wildcard ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(classes_subdir)", "\\$$*.class", Basis, !IO),
    io.write_string(DepStream, ")\n", !IO),

    MmakeVarClasses = mmake_var_defn_list(ModuleMakeVarName ++ ".classes",
        [string.format("$(%s.mods:%%=$(classes_subdir)%%.class)",
            [s(ModuleMakeVarName)]),
        string.format(
            "$(wildcard $(%s.mods:%%=$(classes_subdir)%%\\$$*.class))",
            [s(ModuleMakeVarName)])]),
    gather_mmake_entry(DepStream, MmakeVarClasses, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".dirs = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(dirs_subdir)", ".dir", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDirs = mmake_var_defn(ModuleMakeVarName ++ ".dirs",
        string.format("$(%s.mods:%%=$(dirs_subdir)%%.dir)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDirs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".dir_os = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(dirs_subdir)", ".dir/*.$O", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDirOs = mmake_var_defn(ModuleMakeVarName ++ ".dir_os",
        string.format("$(%s.mods:%%=$(dirs_subdir)%%.dir/*.$O)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDirOs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".dates = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(dates_subdir)", ".date", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDates = mmake_var_defn(ModuleMakeVarName ++ ".dates",
        string.format("$(%s.mods:%%=$(dates_subdir)%%.date)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDates, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".date0s = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(date0s_subdir)", ".date0", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDate0s = mmake_var_defn(ModuleMakeVarName ++ ".date0s",
        string.format("$(%s.mods:%%=$(date0s_subdir)%%.date0)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDate0s, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".date3s = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(date3s_subdir)", ".date3", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDate3s = mmake_var_defn(ModuleMakeVarName ++ ".date3s",
        string.format("$(%s.mods:%%=$(date3s_subdir)%%.date3)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDate3s, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".optdates = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(optdates_subdir)", ".optdate", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarOptDates = mmake_var_defn(ModuleMakeVarName ++ ".optdates",
        string.format("$(%s.mods:%%=$(optdates_subdir)%%.optdate)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarOptDates, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".trans_opt_dates = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(trans_opt_dates_subdir)", ".trans_opt_date", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarTransOptDates =
        mmake_var_defn(ModuleMakeVarName ++ ".trans_opt_dates",
            string.format(
                "$(%s.mods:%%=$(trans_opt_dates_subdir)%%.trans_opt_date)",
                [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarTransOptDates, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".c_dates = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(c_dates_subdir)", ".c_date", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarCDates = mmake_var_defn(ModuleMakeVarName ++ ".c_dates",
        string.format("$(%s.mods:%%=$(c_dates_subdir)%%.c_date)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarCDates, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".java_dates = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(java_dates_subdir)", ".java_date", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarJavaDates = mmake_var_defn(ModuleMakeVarName ++ ".java_dates",
        string.format("$(%s.mods:%%=$(java_dates_subdir)%%.java_date)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarJavaDates, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".ds = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(ds_subdir)", ".d", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarDs = mmake_var_defn(ModuleMakeVarName ++ ".ds",
        string.format("$(%s.mods:%%=$(ds_subdir)%%.d)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarDs, !MmakeFile, !IO),

    % XXX Why is make_module_dep_file_extension a function?
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".module_deps = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(module_deps_subdir)", make_module_dep_file_extension,
        Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarModuleDeps = mmake_var_defn(ModuleMakeVarName ++ ".module_deps",
        string.format("$(%s.mods:%%=$(module_deps_subdir)%%%s)",
            [s(ModuleMakeVarName), s(make_module_dep_file_extension)])),
    gather_mmake_entry(DepStream, MmakeVarModuleDeps, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".mihs = ", !IO),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = yes,
        (
            Target = target_c,
            % For the `--target c' MLDS back-end, we generate `.mih' files
            % for every module.
            write_compact_dependencies_list(DepStream,
                "$(mihs_subdir)", ".mih", Basis, !IO),
            MihSources = [string.format("$(%s.mods:%%=$(mihs_subdir)%%.mih)",
                [s(ModuleMakeVarName)])]
        ;
            % For the Java target, currently we don't generate
            % `.mih' files at all; although perhaps we should...
            % XXX Why?  What's that comment even supposed to mean?
            % - juliensf
            ( Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            ),
            MihSources = []
        )
    ;
        % For the LLDS back-end, we don't use `.mih' files at all.
        HighLevelCode = no,
        MihSources = []
    ),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarMihs =
        mmake_var_defn_list(ModuleMakeVarName ++ ".mihs", MihSources),
    gather_mmake_entry(DepStream, MmakeVarMihs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".mhs = ", !IO),
    (
        Target = target_c,
        write_compact_dependencies_list(DepStream, "", ".mh", Basis, !IO),
        MhSources =
            [string.format("$(%s.mods:%%=%%.mh)", [s(ModuleMakeVarName)])]
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        ),
        MhSources = []
    ),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarMhs = mmake_var_defn_list(ModuleMakeVarName ++ ".mhs", MhSources),
    gather_mmake_entry(DepStream, MmakeVarMhs, !MmakeFile, !IO),

    % The `<module>.all_mihs' variable is like `<module>.mihs' except
    % that it contains header files for all the modules, regardless
    % of the grade or --target option. It is used by the rule for
    % `mmake realclean', which should remove anything that could have
    % been automatically generated, even if the grade or --target option
    % has changed.
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_mihs = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(mihs_subdir)", ".mih", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarAllMihs = mmake_var_defn(ModuleMakeVarName ++ ".all_mihs",
        string.format("$(%s.mods:%%=$(mihs_subdir)%%.mih)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarAllMihs, !MmakeFile, !IO),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except
    % that it contains header files for all the modules, as for
    % `<module>.all_mihs' above.
    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_mhs = ", !IO),
    write_compact_dependencies_list(DepStream,
        "", ".mh", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarAllMhs = mmake_var_defn(ModuleMakeVarName ++ ".all_mhs",
        string.format("$(%s.mods:%%=%%.mh)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarAllMhs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".ints = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(ints_subdir)", ".int", Basis, !IO),
    io.write_string(DepStream, " ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(int2s_subdir)", ".int2", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarInts = mmake_var_defn_list(ModuleMakeVarName ++ ".ints",
        [string.format("$(%s.mods:%%=$(ints_subdir)%%.int)",
            [s(ModuleMakeVarName)]),
        string.format("$(%s.mods:%%=$(int2s_subdir)%%.int2)",
            [s(ModuleMakeVarName)])]),
    gather_mmake_entry(DepStream, MmakeVarInts, !MmakeFile, !IO),

    % `.int0' files are only generated for modules with submodules.
    % XXX ... or at least they should be. Currently we end up generating
    % .int0 files for nested submodules that don't have any children.
    % (We do the correct thing for separate submodules.)

    ParentModsVarName = ModuleMakeVarName ++ ".parent_mods",
    ParentBasis = ParentModsVarName - "",

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".int0s = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(int0s_subdir)", ".int0", ParentBasis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarInt0s = mmake_var_defn(ModuleMakeVarName ++ ".int0s",
        string.format("$(%s.parent_mods:%%=$(int0s_subdir)%%.int0)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarInt0s, !MmakeFile, !IO),

    % XXX The `<module>.all_int0s' variables is like `<module>.int0s' except
    % that it contains .int0 files for all modules, regardless of whether
    % they should have been created or not. It is used by the rule for
    % `mmake realclean' to ensure that we clean up all the .int0 files,
    % including the ones that were accidently created by the bug described
    % above.

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".all_int0s = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(int0s_subdir)", ".int0", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarAllInt0s = mmake_var_defn(ModuleMakeVarName ++ ".all_int0s",
        string.format("$(%s.mods:%%=$(int0s_subdir)%%.int0)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarAllInt0s, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".int3s = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(int3s_subdir)", ".int3", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarInt3s = mmake_var_defn(ModuleMakeVarName ++ ".int3s",
        string.format("$(%s.mods:%%=$(int3s_subdir)%%.int3)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarInt3s, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".opts = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(opts_subdir)", ".opt", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarOpts = mmake_var_defn(ModuleMakeVarName ++ ".opts",
        string.format("$(%s.mods:%%=$(opts_subdir)%%.opt)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarOpts, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".trans_opts = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(trans_opts_subdir)", ".trans_opt", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarTransOpts = mmake_var_defn(ModuleMakeVarName ++ ".trans_opts",
        string.format("$(%s.mods:%%=$(trans_opts_subdir)%%.trans_opt)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarTransOpts, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".analysiss = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(analysiss_subdir)", ".analysis", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarAnalysiss = mmake_var_defn(ModuleMakeVarName ++ ".analysiss",
        string.format("$(%s.mods:%%=$(analysiss_subdir)%%.analysis)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarAnalysiss, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".requests = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(requests_subdir)", ".request", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarRequests = mmake_var_defn(ModuleMakeVarName ++ ".requests",
        string.format("$(%s.mods:%%=$(requests_subdir)%%.request)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarRequests, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".imdgs = ", !IO),
    write_compact_dependencies_list(DepStream,
        "$(imdgs_subdir)", ".imdg", Basis, !IO),
    io.write_string(DepStream, "\n", !IO),

    MmakeVarImdgs = mmake_var_defn(ModuleMakeVarName ++ ".imdgs",
        string.format("$(%s.mods:%%=$(imdgs_subdir)%%.imdg)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarImdgs, !MmakeFile, !IO),

    io.write_string(DepStream, ModuleMakeVarName, !IO),
    io.write_string(DepStream, ".profs = ", !IO),
    write_compact_dependencies_list(DepStream,
        "", ".prof", Basis, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    MmakeVarProfs = mmake_var_defn(ModuleMakeVarName ++ ".profs",
        string.format("$(%s.mods:%%=%%.prof)",
            [s(ModuleMakeVarName)])),
    gather_mmake_entry(DepStream, MmakeVarProfs, !MmakeFile, !IO).

%---------------------------------------------------------------------------%

:- pred select_ok_modules(list(module_name)::in, deps_map::in,
    list(module_name)::out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
    select_ok_modules(Modules0, DepsMap, ModulesTail),
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    module_and_imports_get_errors(ModuleImports, Errors),
    set.intersect(Errors, fatal_read_module_errors, FatalErrors),
    ( if set.is_empty(FatalErrors) then
        Modules = [Module | ModulesTail]
    else
        Modules = ModulesTail
    ).

%---------------------------------------------------------------------------%

    % get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs):
    %
    % Find any extra .$O files that should be linked into the executable.
    % These include fact table object files and object files for foreign
    % code that can't be generated inline for this target.
    %
:- pred get_extra_link_objects(list(module_name)::in, deps_map::in,
    compilation_target::in, assoc_list(file_name, module_name)::out) is det.

get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs) :-
    get_extra_link_objects_2(Modules, DepsMap, Target, [], ExtraLinkObjs0),
    list.reverse(ExtraLinkObjs0, ExtraLinkObjs).

:- pred get_extra_link_objects_2(list(module_name)::in, deps_map::in,
    compilation_target::in,
    assoc_list(file_name, module_name)::in,
    assoc_list(file_name, module_name)::out) is det.

get_extra_link_objects_2([], _DepsMap, _Target, !ExtraLinkObjs).
get_extra_link_objects_2([Module | Modules], DepsMap, Target,
        !ExtraLinkObjs) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),

    % Handle object files for fact tables.
    FactDeps = ModuleImports ^ mai_fact_table_deps,
    list.length(FactDeps, NumFactDeps),
    list.duplicate(NumFactDeps, Module, ModuleList),
    assoc_list.from_corresponding_lists(FactDeps, ModuleList, FactTableObjs),

    % Handle object files for foreign code.
    % NOTE: currently none of the backends support foreign code in a non
    % target language.
    NewLinkObjs = FactTableObjs,
    list.append(NewLinkObjs, !ExtraLinkObjs),
    get_extra_link_objects_2(Modules, DepsMap, Target, !ExtraLinkObjs).

%---------------------------------------------------------------------------%

:- type maybe_mmake_var == pair(list(string), string).

:- pred generate_dep_file(globals::in, file_name::in, module_name::in,
    deps_map::in, io.output_stream::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap, DepStream,
        !MmakeFile, !IO) :-
    io.write_string(DepStream,
        "# vim: ts=8 sw=8 noexpandtab ft=make\n\n", !IO),

    ModuleNameString = sym_name_to_string(ModuleName),
    library.version(Version, FullArch),

    io.write_string(DepStream,
        "# Automatically generated dependencies for module `", !IO),
    io.write_string(DepStream, ModuleNameString, !IO),
    io.write_string(DepStream, "'\n", !IO),
    io.write_string(DepStream,
        "# generated from source file `", !IO),
    io.write_string(DepStream, SourceFileName, !IO),
    io.write_string(DepStream, "'\n", !IO),

    io.write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io.write_string(DepStream, Version, !IO),
    io.write_string(DepStream, "\n# configured for ", !IO),
    io.write_string(DepStream, FullArch, !IO),
    io.write_string(DepStream, ".\n\n", !IO),

    MmakeStartComment = mmake_start_comment("program dependencies",
        ModuleNameString, SourceFileName, Version, FullArch),
    gather_mmake_entry(DepStream, MmakeStartComment, !MmakeFile, !IO),

    module_name_to_make_var_name(ModuleName, ModuleMakeVarName),

    module_name_to_file_name(Globals, do_create_dirs, ".init",
        ModuleName, InitFileName, !IO),
    module_name_to_file_name(Globals, do_create_dirs, "_init.c",
        ModuleName, InitCFileName, !IO),
    module_name_to_file_name(Globals, do_create_dirs, "_init.$O",
        ModuleName, InitObjFileName, !IO),
    module_name_to_file_name(Globals, do_create_dirs, "_init.pic_o",
        ModuleName, InitPicObjFileName, !IO),

    globals.lookup_bool_option(Globals, generate_mmc_make_module_dependencies,
        MmcMakeDeps),
    globals.lookup_bool_option(Globals, intermodule_optimization, Intermod),
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    (
        MmcMakeDeps = yes,
        ModuleDepsVar = "$(" ++ ModuleMakeVarName ++ ".module_deps)",
        MaybeModuleDepsVar = [ModuleDepsVar],
        MaybeModuleDepsVarSpace = ModuleDepsVar ++ " "
    ;
        MmcMakeDeps = no,
        MaybeModuleDepsVar = [],
        MaybeModuleDepsVarSpace = ""
    ),
    (
        Intermod = yes,
        OptsVar = "$(" ++ ModuleMakeVarName ++ ".opts)",
        MaybeOptsVar = [OptsVar],
        MaybeOptsVarSpace = OptsVar ++ " "
    ;
        Intermod = no,
        MaybeOptsVar = [],
        MaybeOptsVarSpace = ""
    ),
    (
        TransOpt = yes,
        TransOptsVar = "$(" ++ ModuleMakeVarName ++ ".trans_opts)",
        MaybeTransOptsVar = [TransOptsVar],
        MaybeTransOptsVarSpace = TransOptsVar ++ " "
    ;
        TransOpt = no,
        MaybeTransOptsVar = [],
        MaybeTransOptsVarSpace = ""
    ),
    MaybeModuleDepsVarPair = MaybeModuleDepsVar - MaybeModuleDepsVarSpace,
    MaybeOptsVarPair = MaybeOptsVar - MaybeOptsVarSpace,
    MaybeTransOptsVarPair = MaybeTransOptsVar - MaybeTransOptsVarSpace,

    generate_dep_file_exec_library_targets(Globals, DepStream, ModuleName,
        ModuleMakeVarName, InitFileName, InitObjFileName,
        MaybeOptsVarPair, MaybeTransOptsVarPair,
        ExeFileName, JarFileName, LibFileName, SharedLibFileName,
        !MmakeFile, !IO),
    generate_dep_file_init_targets(Globals, DepStream, ModuleName,
        ModuleMakeVarName, InitCFileName, InitFileName,
        DepFileName, DvFileName, !MmakeFile, !IO),
    generate_dep_file_install_targets(Globals, DepStream, ModuleName, DepsMap,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVarPair, MaybeOptsVarPair, MaybeTransOptsVarPair,
        !MmakeFile, !IO),
    generate_dep_file_collective_targets(Globals, DepStream, ModuleName,
        ModuleMakeVarName, !MmakeFile, !IO),
    generate_dep_file_clean_targets(Globals, DepStream, ModuleName,
        ModuleMakeVarName, ExeFileName, InitCFileName,
        InitObjFileName, InitPicObjFileName, InitFileName,
        LibFileName, SharedLibFileName, JarFileName, DepFileName, DvFileName,
        !MmakeFile, !IO).

:- pred generate_dep_file_exec_library_targets(globals::in,
    io.output_stream::in, module_name::in, string::in, string::in, string::in,
    maybe_mmake_var::in, maybe_mmake_var::in,
    string::out, string::out, string::out, string::out,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_exec_library_targets(Globals, DepStream, ModuleName,
        ModuleMakeVarName, InitFileName, InitObjFileName,
        MaybeOptsVarPair, MaybeTransOptsVarPair,
        ExeFileName, JarFileName, LibFileName, SharedLibFileName,
        !MmakeFile, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, "",
        ModuleName, ExeFileName, !IO),

    io.write_strings(DepStream, [
        "ifneq ($(EXT_FOR_EXE),)\n",
        ".PHONY : ", ExeFileName, "\n",
        ExeFileName, " : ", ExeFileName, "$(EXT_FOR_EXE)\n",
        "endif\n"
    ], !IO),

    MmakeRuleExtForExe = mmake_simple_rule("ext_for_exe",
        mmake_rule_is_phony,
        ExeFileName,
        [ExeFileName ++ "$(EXT_FOR_EXE)"],
        []),
    MmakeFragmentExtForExe = mmf_conditional_fragments(
        mmake_cond_strings_not_equal("$(EXT_FOR_EXE)", ""),
        [mmf_entry(MmakeRuleExtForExe)], []),
    gather_mmake_fragment(DepStream, MmakeFragmentExtForExe, !MmakeFile, !IO),

    % Note we have to do some ``interesting'' hacks to get
    % `$(ALL_MLLIBS_DEP)' to work in the dependency list,
    % without getting complaints about undefined variables.
    string.append_list([
        "$(foreach @,", ModuleMakeVarName, ",$(ALL_MLLIBS_DEP))"],
        All_MLLibsDep),
    string.append_list(
        ["$(foreach @,", ModuleMakeVarName, ",$(ALL_MLOBJS))"],
        All_MLObjs),
    string.append_list([
        "$(patsubst %.o,%.$(EXT_FOR_PIC_OBJECTS),",
        "$(foreach @,", ModuleMakeVarName, ",$(ALL_MLOBJS)))"],
        All_MLPicObjs),

    NL_All_MLLibsDep = "\\\n\t\t" ++ All_MLLibsDep,
    NL_All_MLObjs = "\\\n\t\t" ++ All_MLObjs,
    NL_All_MLPicObjs = "\\\n\t\t" ++ All_MLPicObjs,

    % When compiling to C, we want to include $(foo.cs) first in
    % the dependency list, before $(foo.os).
    % This is not strictly necessary, since the .$O files themselves depend
    % on the .c files, but want to do it to ensure that Make will try to
    % create all the C files first, thus detecting errors early,
    % rather than first spending time compiling C files to .$O,
    % which could be a waste of time if the program contains errors.

    IfJava = ["ifeq ($(findstring java,$(GRADE)),java)\n"],
    Else = ["else\n"],
    EndIf = ["endif\n"],

    ModuleMakeVarNameClasses = "$(" ++ ModuleMakeVarName ++ ".classes)", 
    JavaMainRule = [ExeFileName, " : ", ModuleMakeVarNameClasses, "\n"],

    ModuleMakeVarNameOs = "$(" ++ ModuleMakeVarName ++ ".os)", 
    NonJavaMainRuleAction1Line1 =
        "$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) " ++
            "$(EXEFILE_OPT)" ++ ExeFileName ++ "$(EXT_FOR_EXE) " ++
            InitObjFileName ++ " \\",
    NonJavaMainRuleAction1Line2 =
        "\t" ++ ModuleMakeVarNameOs ++ " " ++ NL_All_MLObjs ++
            " $(ALL_MLLIBS)",
    NonJavaMainRule =
        [ExeFileName, "$(EXT_FOR_EXE) : ",
            ModuleMakeVarNameOs, " ", InitObjFileName, " ",
            NL_All_MLObjs, " ", NL_All_MLLibsDep, "\n",
        "\t" ++ NonJavaMainRuleAction1Line1 ++ "\n",
        "\t" ++ NonJavaMainRuleAction1Line2 ++ "\n"],

    Rules = IfJava ++ JavaMainRule ++ Else ++ NonJavaMainRule ++ EndIf,
    io.write_strings(DepStream, Rules, !IO),

    MmakeRuleExecutableJava = mmake_simple_rule("executable_java",
        mmake_rule_is_not_phony,
        ExeFileName,
        [ModuleMakeVarNameClasses],
        []),
    MmakeRuleExecutableNonJava = mmake_simple_rule("executable_non_java",
        mmake_rule_is_not_phony,
        ExeFileName ++ "$(EXT_FOR_EXE)",
        [ModuleMakeVarNameOs, InitObjFileName, All_MLObjs, All_MLLibsDep],
        [NonJavaMainRuleAction1Line1, NonJavaMainRuleAction1Line2]),
    MmakeFragmentExecutable = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        MmakeRuleExecutableJava, MmakeRuleExecutableNonJava),
    gather_mmake_fragment(DepStream, MmakeFragmentExecutable, !MmakeFile, !IO),

    module_name_to_lib_file_name(Globals, "lib", ModuleName, "",
        do_not_create_dirs, LibTargetName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName, ".$A",
        do_create_dirs, LibFileName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".$(EXT_FOR_SHARED_LIB)", do_create_dirs, SharedLibFileName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".$(EXT_FOR_SHARED_LIB)", do_not_create_dirs, MaybeSharedLibFileName,
        !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".jar",
        ModuleName, JarFileName, !IO),

    % Set up the installed name for shared libraries.

    globals.lookup_bool_option(Globals, shlib_linker_use_install_name,
        UseInstallName),
    (
        UseInstallName = yes,
        get_install_name_option(Globals, SharedLibFileName, InstallNameOpt)
    ;
        UseInstallName = no,
        InstallNameOpt = ""
    ),

    MaybeOptsVarPair = MaybeOptsVar - MaybeOptsVarSpace,
    MaybeTransOptsVarPair = MaybeTransOptsVar - MaybeTransOptsVarSpace,

    ModuleMakeVarNameInts = "$(" ++ ModuleMakeVarName ++ ".ints)",
    ModuleMakeVarNameInt3s = "$(" ++ ModuleMakeVarName ++ ".int3s)",
    AllInts = [ModuleMakeVarNameInts, " ", ModuleMakeVarNameInt3s, " ",
        MaybeOptsVarSpace, MaybeTransOptsVarSpace, InitFileName, "\n\n"],
    AllIntSources = [ModuleMakeVarNameInts, ModuleMakeVarNameInt3s] ++
        MaybeOptsVar ++ MaybeTransOptsVar ++ [InitFileName],

    JavaLibRule = [
        LibTargetName, " : ", JarFileName, " \\\n\t\t"
        | AllInts
    ],
    NonJavaLibRule = [
        LibTargetName, " : ", LibFileName, " ",
        MaybeSharedLibFileName, " \\\n\t\t"
        | AllInts
    ],
    LibRules = IfJava ++ JavaLibRule ++ Else ++ NonJavaLibRule ++ EndIf,
    io.write_strings(DepStream, [
        ".PHONY : ", LibTargetName, "\n" |
        LibRules
    ], !IO),

    MmakeRuleLibTargetJava = mmake_simple_rule("lib_target_java",
        mmake_rule_is_phony,
        LibTargetName,
        [JarFileName | AllIntSources],
        []),
    MmakeRuleLibTargetNonJava = mmake_simple_rule("lib_target_non_java",
        mmake_rule_is_phony,
        LibTargetName,
        [LibFileName, MaybeSharedLibFileName | AllIntSources],
        []),
    MmakeFragmentLibTarget = mmf_conditional_entry(
        mmake_cond_grade_has_component("java"),
        MmakeRuleLibTargetJava, MmakeRuleLibTargetNonJava),
    gather_mmake_fragment(DepStream, MmakeFragmentLibTarget, !MmakeFile, !IO),

    ModuleMakeVarNamePicOs = "$(" ++ ModuleMakeVarName ++ ".pic_os)",
    SharedLibAction1Line1 =
        "$(ML) --make-shared-lib $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) " ++
        "-- " ++ InstallNameOpt ++ " $(ALL_LD_LIBFLAGS) " ++
        "-o " ++ SharedLibFileName ++ " \\",
    SharedLibAction1Line2 = "\t" ++ ModuleMakeVarNamePicOs ++ " \\",
    SharedLibAction1Line3 = "\t" ++ All_MLPicObjs ++ " $(ALL_MLLIBS)",
    io.write_strings(DepStream, [
        "ifneq ($(EXT_FOR_SHARED_LIB),$A)\n",
        SharedLibFileName, " : ",
            ModuleMakeVarNamePicOs, " ", NL_All_MLPicObjs, " ",
            NL_All_MLLibsDep, "\n",
        "\t", SharedLibAction1Line1, "\n",
        "\t", SharedLibAction1Line2, "\n",
        "\t", SharedLibAction1Line3, "\n",
        "endif\n\n"
    ], !IO),

    MmakeRuleSharedLib = mmake_simple_rule("shared_lib",
        mmake_rule_is_not_phony,
        SharedLibFileName,
        [ModuleMakeVarNamePicOs, All_MLPicObjs, All_MLLibsDep],
        [SharedLibAction1Line1, SharedLibAction1Line2, SharedLibAction1Line3]),
    MmakeFragmentSharedLib = mmf_conditional_fragments(
        mmake_cond_strings_not_equal("$(EXT_FOR_SHARED_LIB)", "$(A)"),
        [mmf_entry(MmakeRuleSharedLib)], []),
    gather_mmake_fragment(DepStream, MmakeFragmentSharedLib, !MmakeFile, !IO),

    LibAction1 = "rm -f " ++ LibFileName,
    LibAction2Line1 =
        "$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)" ++ LibFileName ++
            " " ++ ModuleMakeVarNameOs ++ " \\",
    LibAction2Line2 = "\t" ++ All_MLObjs,
    LibAction3 = "$(RANLIB) $(ALL_RANLIBFLAGS) " ++ LibFileName,
    io.write_strings(DepStream, [
        LibFileName, " : ", ModuleMakeVarNameOs, " ", NL_All_MLObjs, "\n",
        "\t", LibAction1, "\n",
        "\t", LibAction2Line1, "\n",
        "\t", LibAction2Line2, "\n",
        "\t", LibAction3, "\n\n"
    ], !IO),

    MmakeRuleLib = mmake_simple_rule("lib",
        mmake_rule_is_not_phony,
        LibFileName,
        [ModuleMakeVarNameOs, All_MLObjs],
        [LibAction1, LibAction2Line1, LibAction2Line2, LibAction3]),
    gather_mmake_entry(DepStream, MmakeRuleLib, !MmakeFile, !IO),

    list_class_files_for_jar_mmake(Globals, ModuleMakeVarNameClasses,
        ListClassFiles),
    JarAction1 = "$(JAR) $(JAR_CREATE_FLAGS) " ++ JarFileName ++ " " ++
        ListClassFiles,
    io.write_strings(DepStream, [
        JarFileName, " : ", ModuleMakeVarNameClasses, "\n",
        "\t", JarAction1, "\n\n"
    ], !IO),

    MmakeRuleJar = mmake_simple_rule("jar",
        mmake_rule_is_not_phony,
        JarFileName,
        [ModuleMakeVarNameClasses],
        [JarAction1]),
    gather_mmake_entry(DepStream, MmakeRuleJar, !MmakeFile, !IO).

:- pred generate_dep_file_init_targets(globals::in, io.output_stream::in,
    module_name::in, string::in, string::in, string::in,
    string::out, string::out,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_init_targets(Globals, DepStream,
        ModuleName, ModuleMakeVarName, InitCFileName, InitFileName,
        DepFileName, DvFileName, !MmakeFile, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, ".dep",
        ModuleName, DepFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".dv",
        ModuleName, DvFileName, !IO),

    ModuleMakeVarNameCs = "$(" ++ ModuleMakeVarName ++ ".cs)",
    InitAction1 = "echo > " ++ InitFileName,
    InitAction2 = "$(MKLIBINIT) " ++ ModuleMakeVarNameCs ++
        " >> " ++ InitFileName,
    % $(EXTRA_INIT_COMMAND) should expand to a command to
    % generate extra entries in the `.init' file for a library.
    % It may expand to the empty string.
    InitAction3 = "$(EXTRA_INIT_COMMAND) >> " ++ InitFileName,
    io.write_strings(DepStream, [
        InitFileName, " : ", DepFileName, " ", ModuleMakeVarNameCs, "\n",
        "\t", InitAction1, "\n",
        "\t", InitAction2, "\n",
        "\t", InitAction3, "\n\n"
    ], !IO),

    MmakeRuleInitFile = mmake_simple_rule("init_file",
        mmake_rule_is_not_phony,
        InitFileName,
        [DepFileName, ModuleMakeVarNameCs],
        [InitAction1, InitAction2, InitAction3]),
    gather_mmake_entry(DepStream, MmakeRuleInitFile, !MmakeFile, !IO),

    % The `force-module_init' dependency forces the commands for
    % the `module_init.c' rule to be run every time the rule
    % is considered.
    ModuleFileName = sym_name_to_string(ModuleName),
    ForceC2InitTarget = "force-" ++ ModuleFileName ++ "_init",
    io.write_strings(DepStream, [
        ForceC2InitTarget, " :\n\n"
    ], !IO),

    MmakeRuleForceInitCFile = mmake_simple_rule("force_init_c_file",
        mmake_rule_is_not_phony,
        ForceC2InitTarget,
        [],
        []),
    gather_mmake_entry(DepStream, MmakeRuleForceInitCFile, !MmakeFile, !IO),

    TmpInitCFileName = InitCFileName ++ ".tmp",
    ModuleMakeVarNameInitCs = "$(" ++ ModuleMakeVarName ++ ".init_cs)",
    InitCAction1 =
        "@$(C2INIT) $(ALL_GRADEFLAGS) $(ALL_C2INITFLAGS) " ++
            "--init-c-file " ++ TmpInitCFileName ++ " " ++
            ModuleMakeVarNameInitCs ++ " $(ALL_C2INITARGS)",
    InitCAction2 = "@mercury_update_interface " ++ InitCFileName,
    io.write_strings(DepStream, [
        InitCFileName, " : ", ForceC2InitTarget, " ",
            ModuleMakeVarNameCs, "\n",
        "\t", InitCAction1, "\n",
        "\t", InitCAction2, "\n\n"
    ], !IO),

    MmakeRuleInitCFile = mmake_simple_rule("init_c_file",
        mmake_rule_is_not_phony,
        InitCFileName,
        [ForceC2InitTarget, ModuleMakeVarNameCs],
        [InitCAction1, InitCAction2]),
    gather_mmake_entry(DepStream, MmakeRuleInitCFile, !MmakeFile, !IO).

:- pred generate_dep_file_install_targets(globals::in, io.output_stream::in,
    module_name::in, deps_map::in, string::in, bool::in, bool::in, bool::in,
    maybe_mmake_var::in, maybe_mmake_var::in, maybe_mmake_var::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_install_targets(Globals, DepStream, ModuleName, DepsMap,
        ModuleMakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVarPair, MaybeOptsVarPair, MaybeTransOptsVarPair,
        !MmakeFile, !IO) :-
    % XXX  Note that we install the `.opt' and `.trans_opt' files
    % in two places: in the `lib/$(GRADE)/opts' directory, so
    % that mmc will find them, and also in the `ints' directory,
    % so that Mmake will find them. That is not ideal, but it works.

    MaybeOptsVarPair = MaybeOptsVar - MaybeOptsVarSpace,
    MaybeTransOptsVarPair = MaybeTransOptsVar - MaybeTransOptsVarSpace,
    MaybeModuleDepsVarPair = MaybeModuleDepsVar - MaybeModuleDepsVarSpace,

    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".install_ints", do_not_create_dirs,
        LibInstallIntsTargetName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".install_opts", do_not_create_dirs,
        LibInstallOptsTargetName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".install_hdrs", do_not_create_dirs,
        LibInstallHdrsTargetName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".install_grade_hdrs", do_not_create_dirs,
        LibInstallGradeHdrsTargetName, !IO),

    ModuleMakeVarNameInts = "$(" ++ ModuleMakeVarName ++ ".ints)",
    ModuleMakeVarNameInt3s = "$(" ++ ModuleMakeVarName ++ ".int3s)",

    (
        Intermod = yes,
        MaybeSpaceOptStr = " opt"
    ;
        Intermod = no,
        MaybeSpaceOptStr = ""
    ),
    ( if
        Intermod = yes,
        some [ModuleAndImports] (
            map.member(DepsMap, _, deps(_, ModuleAndImports)),
            set.non_empty(ModuleAndImports ^ mai_children)
        )
    then
        % The `.int0' files only need to be installed with
        % `--intermodule-optimization'.
        SpaceInt0Str = " int0",
        ModuleVarNameInt0s = "$(" ++ ModuleMakeVarName ++ ".int0s)",
        MaybeModuleVarNameInt0sSpace = ModuleVarNameInt0s ++ " ",
        MaybeModuleVarNameInt0s = [ModuleVarNameInt0s]
    else
        SpaceInt0Str = "",
        MaybeModuleVarNameInt0sSpace = "",
        MaybeModuleVarNameInt0s = []
    ),
    (
        TransOpt = yes,
        MaybeSpaceTransOptStr = " trans_opt"
    ;
        TransOpt = no,
        MaybeSpaceTransOptStr = ""
    ),
    (
        MmcMakeDeps = yes,
        MaybeSpaceDepStr = " module_dep"
    ;
        MmcMakeDeps = no,
        MaybeSpaceDepStr = ""
    ),

    LibInstallIntsFiles = """" ++
        ModuleMakeVarNameInts ++ " " ++ ModuleMakeVarNameInt3s ++ " " ++
        MaybeModuleVarNameInt0sSpace ++ MaybeOptsVarSpace ++
        MaybeTransOptsVarSpace ++ MaybeModuleDepsVarSpace ++ """",

    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallIntsTargetName, "\n",
        LibInstallIntsTargetName, " : ",
            ModuleMakeVarNameInts, " ", ModuleMakeVarNameInt3s, " ",
            MaybeModuleVarNameInt0sSpace, MaybeOptsVarSpace,
            MaybeTransOptsVarSpace, MaybeModuleDepsVarSpace,
            " install_lib_dirs\n",
        "\tfiles=", LibInstallIntsFiles, "; \\\n",
        "\tfor file in $$files; do \\\n",
        "\t\ttarget=""$(INSTALL_INT_DIR)/`basename $$file`""; \\\n",
        "\t\tif cmp -s ""$$file"" ""$$target""; then \\\n",
        "\t\t\techo \"$$target unchanged\"; \\\n",
        "\t\telse \\\n",
        "\t\t\techo \"installing $$target\"; \\\n",
        "\t\t\t$(INSTALL) ""$$file"" ""$$target""; \\\n",
        "\t\tfi; \\\n",
        "\tdone\n",
        "\t# The following is needed to support the `--use-subdirs' option\n",
        "\t# We try using `$(LN_S)', but if that fails, then we just use\n",
        "\t# `$(INSTALL)'.\n",
        "\tfor ext in int int2 int3",
            SpaceInt0Str, MaybeSpaceOptStr, MaybeSpaceTransOptStr,
            MaybeSpaceDepStr, "; do \\\n",
        "\t\tdir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\\n",
        "\t\trm -rf ""$$dir""; \\\n",
        "\t\t$(LN_S) .. ""$$dir"" || { \\\n",
        "\t\t\t{ [ -d ""$$dir"" ] || \\\n",
        "\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\\n",
        "\t\t\t$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext ""$$dir""; \\\n",
        "\t\t} || exit 1; \\\n",
        "\tdone\n\n"
    ], !IO),

    MmakeRuleLibInstallInts = mmake_simple_rule("lib_install_ints",
        mmake_rule_is_phony,
        LibInstallIntsTargetName,
        [ModuleMakeVarNameInts, ModuleMakeVarNameInt3s] ++
            MaybeModuleVarNameInt0s ++ MaybeOptsVar ++
            MaybeTransOptsVar ++ MaybeModuleDepsVar ++
            ["install_lib_dirs"],
        ["files=" ++ LibInstallIntsFiles ++ "; \\",
        "for file in $$files; do \\",
        "\ttarget=""$(INSTALL_INT_DIR)/`basename $$file`""; \\",
        "\tif cmp -s ""$$file"" ""$$target""; then \\",
        "\t\techo \"$$target unchanged\"; \\",
        "\telse \\",
        "\t\techo \"installing $$target\"; \\",
        "\t\t$(INSTALL) ""$$file"" ""$$target""; \\",
        "\tfi; \\",
        "done",
        "# The following is needed to support the `--use-subdirs' option.",
        "# We try using `$(LN_S)', but if that fails, then we just use",
        "# `$(INSTALL)'.",
        "for ext in int int2 int3" ++
            SpaceInt0Str ++ MaybeSpaceOptStr ++ MaybeSpaceTransOptStr ++
            MaybeSpaceDepStr ++ "; do \\",
        "\tdir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\",
        "\trm -rf ""$$dir""; \\",
        "\t$(LN_S) .. ""$$dir"" || { \\",
        "\t\t{ [ -d ""$$dir"" ] || \\",
        "\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\",
        "\t\t$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext ""$$dir""; \\",
        "\t} || exit 1; \\",
        "done"]),
    gather_mmake_entry(DepStream, MmakeRuleLibInstallInts,
        !MmakeFile, !IO),

    io.write_strings(DepStream,
        [".PHONY : ", LibInstallOptsTargetName, "\n",
        LibInstallOptsTargetName, " : "], !IO),
    ( if
        Intermod = no,
        TransOpt = no
    then
        io.write_string(DepStream, "\n\t@:\n\n", !IO),
        LibInstallOptsSources = [],
        LibInstallOptsActions = [silent_noop_action]
    else
        io.write_strings(DepStream, [
            MaybeOptsVarSpace, MaybeTransOptsVarSpace, "install_grade_dirs\n",
            "\tfiles=""", MaybeOptsVarSpace, MaybeTransOptsVarSpace,
                """; \\\n",
            "\tfor file in $$files; do \\\n",
            "\t\ttarget=""$(INSTALL_GRADE_INT_DIR)/`basename $$file`"";\\\n",
            "\t\tif cmp -s ""$$file"" ""$$target""; then \\\n",
            "\t\t\techo \"$$target unchanged\"; \\\n",
            "\t\telse \\\n",
            "\t\t\techo \"installing $$target\"; \\\n",
            "\t\t\t$(INSTALL) ""$$file"" ""$$target""; \\\n",
            "\t\tfi; \\\n",
            "\tdone\n",
            "\t# The following is needed to support the",
                " `--use-subdirs' option\n",
            "\t# We try using `$(LN_S)', but if that fails,",
                " then we just use\n",
            "\t# `$(INSTALL)'.\n",
            "\tfor ext in ", MaybeSpaceOptStr, MaybeSpaceTransOptStr,
                "; do \\\n",
            "\t\tdir=""$(INSTALL_GRADE_INT_DIR)/Mercury/$${ext}s""; \\\n",
            "\t\trm -rf ""$$dir""; \\\n",
            "\t\t$(LN_S) .. ""$$dir"" || { \\\n",
            "\t\t\t{ [ -d ""$$dir"" ] || \\\n",
            "\t\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\\n",
            "\t\t\t$(INSTALL) ""$(INSTALL_GRADE_INT_DIR)""/*.$$ext \\\n",
            "\t\t\t\t""$$dir""; \\\n",
            "\t\t} || exit 1; \\\n",
            "\tdone\n\n"
        ], !IO),

        LibInstallOptsSources = MaybeOptsVar ++ MaybeTransOptsVar ++
            ["install_grade_dirs"],
        LibInstallOptsFiles =
            """" ++ MaybeOptsVarSpace ++ MaybeTransOptsVarSpace ++ """",
        LibInstallOptsActions =
            ["files=" ++ LibInstallOptsFiles ++ "; \\",
            "for file in $$files; do \\",
            "\ttarget=""$(INSTALL_GRADE_INT_DIR)/`basename $$file`"";\\",
            "\tif cmp -s ""$$file"" ""$$target""; then \\",
            "\t\techo \"$$target unchanged\"; \\",
            "\telse \\",
            "\t\techo \"installing $$target\"; \\",
            "\t\t$(INSTALL) ""$$file"" ""$$target""; \\",
            "\tfi; \\",
            "done",
            "# The following is needed to support the `--use-subdirs' option",
            "# We try using `$(LN_S)', but if that fails, then we just use",
            "# `$(INSTALL)'.",
            "for ext in " ++ MaybeSpaceOptStr ++ MaybeSpaceTransOptStr ++
                "; do \\",
            "\tdir=""$(INSTALL_GRADE_INT_DIR)/Mercury/$${ext}s""; \\",
            "\trm -rf ""$$dir""; \\",
            "\t$(LN_S) .. ""$$dir"" || { \\",
            "\t\t{ [ -d ""$$dir"" ] || \\",
            "\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\",
            "\t\t$(INSTALL) ""$(INSTALL_GRADE_INT_DIR)""/*.$$ext \\",
            "\t\t\t""$$dir""; \\",
            "\t} || exit 1; \\",
            "done"]
    ),
    MmakeRuleLibInstallOpts = mmake_simple_rule("lib_install_opts",
        mmake_rule_is_phony,
        LibInstallOptsTargetName,
        LibInstallOptsSources,
        LibInstallOptsActions),
    gather_mmake_entry(DepStream, MmakeRuleLibInstallOpts,
        !MmakeFile, !IO),

    % XXX  Note that we install the header files in two places:
    % in the `lib/inc' or `lib/$(GRADE)/$(FULLARCH)/inc' directory,
    % so that the C compiler will find them, and also in the `ints' directory,
    % so that Mmake will find them. That's not ideal, but it works.
    %
    % (A better fix would be to change the VPATH setting in
    % scripts/Mmake.vars.in so that Mmake also searches the
    % `lib/$(GRADE)/$(FULLARCH)/inc' directory, but doing that properly
    % is non-trivial.)

    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallHdrsTargetName, "\n",
        LibInstallHdrsTargetName, " : ",
            "$(", ModuleMakeVarName, ".mhs) ",
            "install_lib_dirs\n",
        "ifeq ($(", ModuleMakeVarName, ".mhs),)\n",
        "\t@:\n",
        "else\n",
        "\tfor hdr in $(", ModuleMakeVarName, ".mhs); do \\\n",
        "\t\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\\n",
        "\t\t$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\\n",
        "\tdone\n",
        "endif\n\n"], !IO),

    ModuleMakeVarNameMhs = string.format("$(%s.mhs)", [s(ModuleMakeVarName)]),
    MmakeRuleLibInstallHdrsNoMhs = mmake_simple_rule("install_lib_hdrs_nomhs",
        mmake_rule_is_phony,
        LibInstallHdrsTargetName,
        [ModuleMakeVarNameMhs, "install_lib_dirs"],
        [silent_noop_action]),
    MmakeRuleLibInstallHdrsMhs = mmake_simple_rule("install_lib_hdrs_mhs",
        mmake_rule_is_phony,
        LibInstallHdrsTargetName,
        [ModuleMakeVarNameMhs, "install_lib_dirs"],
        ["for hdr in " ++ ModuleMakeVarNameMhs ++ "; do \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\",
        "done"]),
    MmakeFragmentLibInstallHdrs = mmf_conditional_entry(
        mmake_cond_strings_equal(ModuleMakeVarNameMhs, ""),
        MmakeRuleLibInstallHdrsNoMhs,
        MmakeRuleLibInstallHdrsMhs),
    gather_mmake_fragment(DepStream, MmakeFragmentLibInstallHdrs,
        !MmakeFile, !IO),

    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallGradeHdrsTargetName, "\n",
        LibInstallGradeHdrsTargetName, " : ",
            "$(", ModuleMakeVarName, ".mihs) ",
            "install_grade_dirs\n",
    "ifeq ($(", ModuleMakeVarName, ".mihs),)\n",
    "\t@:\n",
    "else\n",
    "\tfor hdr in $(", ModuleMakeVarName, ".mihs); do \\\n",
    "\t\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\\n",
    "\t\t$(INSTALL) $$hdr $(INSTALL_GRADE_INC_DIR); \\\n",
    "\tdone\n",
    "\t# The following is needed to support the `--use-subdirs' option\n",
    "\t# We try using `$(LN_S)', but if that fails, then we just use\n",
    "\t# `$(INSTALL)'.\n",
    "\trm -rf $(INSTALL_GRADE_INC_SUBDIR)\n",
    "\t$(LN_S) .. $(INSTALL_GRADE_INC_SUBDIR) || { \\\n",
    "\t\t{ [ -d $(INSTALL_GRADE_INC_SUBDIR) ] || \\\n",
    "\t\t\t$(INSTALL_MKDIR) $(INSTALL_GRADE_INC_SUBDIR); \\\n",
    "\t\t} && \\\n",
    "\t\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\\n",
    "\t\t\t$(INSTALL_GRADE_INC_SUBDIR); \\\n",
    "\t} || exit 1\n",
    "\trm -rf $(INSTALL_INT_DIR)/Mercury/mihs\n",
    "\t$(LN_S) .. $(INSTALL_INT_DIR)/Mercury/mihs || { \\\n",
    "\t\t{ [ -d $(INSTALL_INT_DIR)/Mercury/mihs ] || \\\n",
    "\t\t\t$(INSTALL_MKDIR) \\\n",
    "\t\t\t$(INSTALL_INT_DIR)/Mercury/mihs; \\\n",
    "\t\t} && \\\n",
    "\t\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\\n",
    "\t\t\t$(INSTALL_INT_DIR); \\\n",
    "\t} || exit 1\n",
    "endif\n\n"], !IO),

    ModuleMakeVarNameMihs =
        string.format("$(%s.mihs)", [s(ModuleMakeVarName)]),
    MmakeRuleLibInstallGradeHdrsNoMihs = mmake_simple_rule(
        "install_grade_hdrs_no_mihs",
        mmake_rule_is_phony,
        LibInstallGradeHdrsTargetName,
        [ModuleMakeVarNameMihs, "install_grade_dirs"],
        [silent_noop_action]),
    MmakeRuleLibInstallGradeHdrsMihs = mmake_simple_rule(
        "install_grade_hdrs_mihs",
        mmake_rule_is_phony,
        LibInstallGradeHdrsTargetName,
        [ModuleMakeVarNameMihs, "install_grade_dirs"],
        ["for hdr in " ++ ModuleMakeVarNameMihs ++ "; do \\",
        "\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\",
        "\t$(INSTALL) $$hdr $(INSTALL_GRADE_INC_DIR); \\",
        "done",
        "# The following is needed to support the `--use-subdirs' option.",
        "# We try using `$(LN_S)', but if that fails, then we just use",
        "# `$(INSTALL)'.",
        "rm -rf $(INSTALL_GRADE_INC_SUBDIR)",
        "$(LN_S) .. $(INSTALL_GRADE_INC_SUBDIR) || { \\",
        "\t{ [ -d $(INSTALL_GRADE_INC_SUBDIR) ] || \\",
        "\t\t$(INSTALL_MKDIR) $(INSTALL_GRADE_INC_SUBDIR); \\",
        "\t} && \\",
        "\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\",
        "\t\t$(INSTALL_GRADE_INC_SUBDIR); \\",
        "} || exit 1",
        "rm -rf $(INSTALL_INT_DIR)/Mercury/mihs",
        "$(LN_S) .. $(INSTALL_INT_DIR)/Mercury/mihs || { \\",
        "\t{ [ -d $(INSTALL_INT_DIR)/Mercury/mihs ] || \\",
        "\t\t$(INSTALL_MKDIR) \\",
        "\t\t$(INSTALL_INT_DIR)/Mercury/mihs; \\",
        "\t} && \\",
        "\t$(INSTALL) $(INSTALL_GRADE_INC_DIR)/*.mih \\",
        "\t\t$(INSTALL_INT_DIR); \\",
        "} || exit 1"]),
    MmakeFragmentLibInstallGradeHdrs = mmf_conditional_entry(
        mmake_cond_strings_equal(ModuleMakeVarNameMihs, ""),
        MmakeRuleLibInstallGradeHdrsNoMihs,
        MmakeRuleLibInstallGradeHdrsMihs),
    gather_mmake_fragment(DepStream, MmakeFragmentLibInstallGradeHdrs,
        !MmakeFile, !IO).

:- pred generate_dep_file_collective_targets(globals::in, io.output_stream::in,
    module_name::in, string::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_collective_targets(Globals, DepStream, ModuleName,
        ModuleMakeVarName, !MmakeFile, !IO) :-
    list.foldl2(
        generate_dep_file_collective_target(Globals, DepStream, ModuleName,
            ModuleMakeVarName),
        [
            ".check" - ".errs",
            ".ints" - ".dates",
            ".int3s" - ".date3s",
            ".opts" - ".optdates",
            ".trans_opts" - ".trans_opt_dates",
            ".javas" - ".javas",
            ".classes" - ".classes",
            ".all_ints" - ".dates",
            ".all_int3s" - ".date3s",
            ".all_opts" - ".optdates",
            ".all_trans_opts" - ".trans_opt_dates"
        ], !MmakeFile, !IO).

:- pred generate_dep_file_collective_target(globals::in, io.output_stream::in,
    module_name::in, string::in, pair(string, string)::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_collective_target(Globals, DepStream, ModuleName,
        ModuleMakeVarName, Extension - VarExtension, !MmakeFile, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, Extension,
        ModuleName, TargetName, !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", TargetName, "\n",
        TargetName, " : $(", ModuleMakeVarName, VarExtension, ")\n\n"
    ], !IO),

    Source = string.format("$(%s%s)", [s(ModuleMakeVarName), s(VarExtension)]),
    MmakeRule = mmake_simple_rule(
        "collective_target_" ++ Extension ++ VarExtension, mmake_rule_is_phony,
        TargetName, [Source], []),
    gather_mmake_entry(DepStream, MmakeRule, !MmakeFile, !IO).

:- pred generate_dep_file_clean_targets(globals::in, io.output_stream::in,
    module_name::in, string::in, string::in, string::in,
    string::in, string::in, string::in, string::in, string::in, string::in,
    string::in, string::in,
    mmakefile::in, mmakefile::out, io::di, io::uo) is det.

generate_dep_file_clean_targets(Globals, DepStream,
        ModuleName, ModuleMakeVarName, ExeFileName, InitCFileName,
        InitObjFileName, InitPicObjFileName, InitFileName, LibFileName,
        SharedLibFileName, JarFileName, DepFileName, DvFileName,
        !MmakeFile, !IO) :-
    % If you change the clean targets below, please also update the
    % documentation in doc/user_guide.texi.

    module_name_to_file_name(Globals, do_not_create_dirs, ".clean",
        ModuleName, CleanTargetName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, ".realclean",
        ModuleName, RealCleanTargetName, !IO),

    io.write_strings(DepStream, [
        "clean_local : ", CleanTargetName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", CleanTargetName, "\n",
        CleanTargetName, " :\n",
        "\t-echo $(", ModuleMakeVarName, ".dirs) | xargs rm -rf \n",
        "\t-echo $(", ModuleMakeVarName, ".cs) ", InitCFileName,
            " | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".mihs) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".all_os) ", InitObjFileName,
            " | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".all_pic_os) ", InitPicObjFileName,
            " | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".c_dates) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".java_dates) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".useds) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".javas) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".profs) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".errs) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".foreign_cs) | xargs rm -f\n",
        "\n"
    ], !IO),

    % XXX Put these into a logical order.
    CleanSuffixes = [".dirs", ".cs", ".mihs", ".all_os", ".all_pic_os",
        ".c_dates", ".java_dates", ".useds", ".javas", ".profs",
        ".errs", ".foreign_cs"],
    CleanFiles = [InitCFileName, InitObjFileName, InitPicObjFileName],
    MmakeRulesClean =
        % XXX Why is the first rule not phony?
        [mmake_simple_rule("clean_local", mmake_rule_is_not_phony,
            "clean_local", [CleanTargetName], []),
        mmake_simple_rule("clean_target", mmake_rule_is_phony,
            CleanTargetName,
            [],
            list.map(remove_suffix_files_cmd(ModuleMakeVarName),
                CleanSuffixes) ++
            [remove_files_cmd(CleanFiles)])],
    gather_mmake_entries(DepStream, MmakeRulesClean, !MmakeFile, !IO),

    % XXX We delete $(ModuleMakeVarName).all_int0s instead of
    % $(ModuleMakeVarName).int0s to make sure that we delete
    % any spurious .int0 files created for nested submodules.
    % For further details, see the XXX comments above.
    io.write_strings(DepStream, [
        "realclean_local : ", RealCleanTargetName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", RealCleanTargetName, "\n",
        RealCleanTargetName, " : ", CleanTargetName, "\n",
        "\t-echo $(", ModuleMakeVarName, ".dates) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".date0s) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".date3s) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".optdates) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".trans_opt_dates) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".ints) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".all_int0s) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".int3s) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".opts) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".trans_opts) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".analysiss) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".requests) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".imdgs) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".ds) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".module_deps) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".all_mhs) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".all_mihs) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".dlls) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".foreign_dlls) | xargs rm -f\n",
        "\t-echo $(", ModuleMakeVarName, ".classes) | xargs rm -f\n"
    ], !IO),
    io.write_strings(DepStream, [
        "\t-rm -f ",
            ExeFileName, "$(EXT_FOR_EXE) ",
            InitFileName, " ",
            LibFileName, " ",
            SharedLibFileName, " ",
            JarFileName, " ",
            DepFileName, " ",
            DvFileName, "\n\n"
    ], !IO),

    RealCleanSuffixes = [".dates", ".date0s", ".date3s",
        ".optdates", ".trans_opt_dates", ".ints", ".all_int0s", ".int3s",
        ".opts", ".trans_opts", ".analysiss", ".requests", ".imdgs",
        ".ds", ".module_deps", ".all_mhs", ".all_mihs", ".dlls",
        ".foreign_dlls", ".classes"],
    RealCleanFiles = [ExeFileName ++ "$(EXT_FOR_EXE) ", InitFileName,
        LibFileName, SharedLibFileName, JarFileName, DepFileName, DvFileName],
    MmakeRulesRealClean =
        % XXX Why is the first rule not phony?
        [mmake_simple_rule("realclean_local", mmake_rule_is_not_phony,
            "realclean_local", [RealCleanTargetName], []),
        mmake_simple_rule("realclean_target", mmake_rule_is_phony,
            RealCleanTargetName,
            [CleanTargetName],
            list.map(remove_suffix_files_cmd(ModuleMakeVarName),
                RealCleanSuffixes) ++
            [remove_files_cmd(RealCleanFiles)])],
    gather_mmake_entries(DepStream, MmakeRulesRealClean, !MmakeFile, !IO).

    % remove_suffix_files_cmd(ModuleMakeVarName, Extension):
    %
    % Return a command to delete the files in $(ModuleMakeVarNameExtension).
    %
    % XXX Xargs doesn't handle special characters in the file names correctly.
    % This is currently not a problem in practice as we never generate
    % file names containing special characters.
    %
    % Any fix for this problem will also require a fix in `mmake.in'.
    %
:- func remove_suffix_files_cmd(string, string) = string.

remove_suffix_files_cmd(ModuleMakeVarName, Extension) =
    string.format("-echo $(%s%s) | xargs rm -f",
        [s(ModuleMakeVarName), s(Extension)]).

:- func remove_files_cmd(list(string)) = string.

remove_files_cmd(Files) =
    "-rm -f " ++ string.join_list(" ", Files).

%---------------------------------------------------------------------------%

:- pred get_source_file(deps_map::in, module_name::in, file_name::out) is det.

get_source_file(DepsMap, ModuleName, FileName) :-
    map.lookup(DepsMap, ModuleName, Deps),
    Deps = deps(_, ModuleAndImports),
    module_and_imports_get_source_file_name(ModuleAndImports, SourceFileName),
    ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
        FileName = SourceFileBase
    else
        unexpected($module, $pred, "source file name doesn't end in `.m'")
    ).

%---------------------------------------------------------------------------%

maybe_output_module_order(Globals, Module, DepsOrdering, !IO) :-
    globals.lookup_bool_option(Globals, generate_module_order, Order),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Order = yes,
        module_name_to_file_name(Globals, do_create_dirs, ".order",
            Module, OrdFileName, !IO),
        maybe_write_string(Verbose, "% Creating module order file `", !IO),
        maybe_write_string(Verbose, OrdFileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        io.open_output(OrdFileName, OrdResult, !IO),
        (
            OrdResult = ok(OrdStream),
            io.write_list(OrdStream, DepsOrdering, "\n\n",
                write_module_scc(OrdStream), !IO),
            io.close_output(OrdStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            OrdResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening file `", OrdFileName,
                "' for output: ", IOErrorMessage], OrdMessage),
            report_error(OrdMessage, !IO)
        )
    ;
        Order = no
    ).

:- pred write_module_scc(io.output_stream::in, set(module_name)::in,
    io::di, io::uo) is det.

write_module_scc(Stream, SCC0, !IO) :-
    set.to_sorted_list(SCC0, SCC),
    io.write_list(Stream, SCC, "\n", prog_out.write_sym_name, !IO).

%---------------------------------------------------------------------------%

    % get_both_opt_deps(Globals, BuildOptFiles, Deps, IntermodDirs,
    %   OptDeps, TransOptDeps, !IO):
    %
    % For each dependency, search intermod_directories for a .m file.
    % If it exists, add it to both output lists. Otherwise, if a .opt
    % file exists, add it to the OptDeps list, and if a .trans_opt
    % file exists, add it to the TransOptDeps list.
    % If --use-opt-files is set, don't look for `.m' files, since we are
    % not building `.opt' files, only using those which are available.
    % XXX This won't find nested submodules.
    % XXX Use `mmc --make' if that matters.
    %
:- pred get_both_opt_deps(globals::in, bool::in, list(string)::in,
    list(module_name)::in, list(module_name)::out, list(module_name)::out,
    io::di, io::uo) is det.

get_both_opt_deps(_, _, _, [], [], [], !IO).
get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs, [Dep | Deps],
        !:OptDeps, !:TransOptDeps, !IO) :-
    get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs, Deps,
        !:OptDeps, !:TransOptDeps, !IO),
    (
        BuildOptFiles = yes,
        search_for_module_source(Globals, IntermodDirs, IntermodDirs,
            Dep, MaybeFileName, !IO),
        (
            MaybeFileName = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            !:TransOptDeps = [Dep | !.TransOptDeps],
            Found = yes
        ;
            MaybeFileName = error(_),
            Found = no
        )
    ;
        BuildOptFiles = no,
        Found = no
    ),
    (
        Found = no,
        module_name_to_file_name(Globals, do_not_create_dirs, ".opt",
            Dep, OptName, !IO),
        search_for_file_returning_dir(IntermodDirs, OptName, MaybeOptDir, !IO),
        (
            MaybeOptDir = ok(_),
            !:OptDeps = [Dep | !.OptDeps]
        ;
            MaybeOptDir = error(_)
        ),
        module_name_to_file_name(Globals, do_not_create_dirs, ".trans_opt",
            Dep, TransOptName, !IO),
        search_for_file_returning_dir(IntermodDirs, TransOptName,
            MaybeTransOptDir, !IO),
        (
            MaybeTransOptDir = ok(_),
            !:TransOptDeps = [Dep | !.TransOptDeps]
        ;
            MaybeTransOptDir = error(_)
        )
    ;
        Found = yes
    ).

get_opt_deps(_Globals, _BuildOptFiles, _IntermodDirs, _Suffix, [], [], !IO).
get_opt_deps(Globals, BuildOptFiles, IntermodDirs, Suffix, [Dep | Deps],
        !:OptDeps, !IO) :-
    get_opt_deps(Globals, BuildOptFiles, IntermodDirs, Suffix, Deps,
        !:OptDeps, !IO),
    (
        BuildOptFiles = yes,
        search_for_module_source(Globals, IntermodDirs, IntermodDirs,
            Dep, Result1, !IO),
        (
            Result1 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            Found = yes
        ;
            Result1 = error(_),
            Found = no
        )
    ;
        BuildOptFiles = no,
        Found = no
    ),
    (
        Found = no,
        module_name_to_search_file_name(Globals, Suffix, Dep, OptName, !IO),
        search_for_file(IntermodDirs, OptName, MaybeOptDir, !IO),
        (
            MaybeOptDir = ok(_),
            !:OptDeps = [Dep | !.OptDeps]
        ;
            MaybeOptDir = error(_)
        )
    ;
        Found = yes
    ).

%---------------------------------------------------------------------------%

:- pred compare_module_names(module_name::in, module_name::in,
    comparison_result::out) is det.

compare_module_names(Sym1, Sym2, Result) :-
    Str1 = sym_name_to_string(Sym1),
    Str2 = sym_name_to_string(Sym2),
    compare(Result, Str1, Str2).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % add_suffix(Suffix, Str) = StrSuffix:
    %
    % Does the same job as Str ++ Suffix = StrSuffix, but allows
    % using list.map to add the same suffix to many strings.
    %
    % XXX Should this be in library/string.m?
    %
:- func add_suffix(string, string) = string.

add_suffix(Suffix, Str) = Str ++ Suffix.

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_deps_file.
%---------------------------------------------------------------------------%
