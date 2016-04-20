%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: write_deps_file.m.
%
%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

    % Generate the list of .NET DLLs which could be referred to by this module
    % (including the module itself).
    %
    % If we are compiling a module within the standard library we should
    % reference the runtime DLLs and all other library DLLs. If we are
    % outside the library we should just reference mercury.dll (which will
    % contain all the DLLs).
    %
:- func referenced_dlls(module_name, set(module_name)) = set(module_name).

%-----------------------------------------------------------------------------%

    % For each dependency, search intermod_directories for a .Suffix
    % file or a .m file, filtering out those for which the search fails.
    % If --use-opt-files is set, only look for `.opt' files,
    % not `.m' files.
    % XXX This won't find nested sub-modules.
    % XXX Use `mmc --make' if that matters.
    %
    % This predicate must operate on lists, not sets, of module names,
    % because it needs to preserve the chosen trans_opt deps ordering,
    % which is derived from the dependency graph between modules,
    % and not just the modules' names.
    %
:- pred get_opt_deps(globals::in, bool::in, list(string)::in, string::in,
    list(module_name)::in, list(module_name)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
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
:- import_module term.

%-----------------------------------------------------------------------------%

write_dependency_file(Globals, ModuleAndImports, AllDeps,
        MaybeTransOptDeps, !IO) :-
    ModuleAndImports = module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, _ModuleNameContext, ParentDeps, IntDeps, ImpDeps,
        IndirectDeps, _Children, InclDeps, NestedDeps, FactDeps0,
        ForeignImportModules0, ForeignIncludeFilesCord,
        ContainsForeignCode, _ContainsForeignExport,
        SrcItemBlocks, DirectIntItemBlocksCord, IndirectIntItemBlocksCord,
        OptItemBlocksCord, IntForOptItemBlocksCord, _ModuleVersionNumbersCord,
        _Specs, _Error, _Timestamps, _HasMain, _Dir),

    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_make_var_name(ModuleName, MakeVarName),
    module_name_to_file_name(Globals, ModuleName, ".d",
        do_create_dirs, DependencyFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, ".trans_opt_date",
        do_not_create_dirs, TransOptDateFileName, !IO),

    % To avoid problems with concurrent updates of `.d' files during
    % parallel makes, we first create the file with a temporary name,
    % and then rename it to the desired name when we've finished.

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
            set.union(IntDeps, ImpDeps, LongDeps0),
            ShortDeps0 = IndirectDeps,
            set.delete(ModuleName, LongDeps0, LongDeps),
            set.difference(ShortDeps0, LongDeps, ShortDeps1),
            set.delete(ModuleName, ShortDeps1, ShortDeps),
            list.sort_and_remove_dups(FactDeps0, FactDeps),

            (
                MaybeTransOptDeps = yes(TransOptDeps0),
                set.intersect(set.list_to_set(TransOptDeps0), LongDeps,
                    TransOptDateDeps),

                % Note that maybe_read_dependency_file searches for
                % this exact pattern.
                io.write_strings(DepStream, [TransOptDateFileName, " :"], !IO),
                write_dependencies_set(Globals, DepStream, ".trans_opt",
                    TransOptDateDeps, !IO)
            ;
                MaybeTransOptDeps = no
            ),

            (
                FactDeps = [_ | _],
                io.write_strings(DepStream,
                    ["\n\n", MakeVarName, ".fact_tables ="], !IO),
                write_file_dependencies_list(DepStream, "", FactDeps, !IO),
                io.nl(DepStream, !IO),
                globals.lookup_bool_option(Globals, assume_gmake, AssumeGmake),
                (
                    AssumeGmake = yes,
                    io.write_strings(DepStream, [
                        "\n\n", MakeVarName,
                        ".fact_tables.os = $(", MakeVarName,
                        ".fact_tables:%=$(os_subdir)%.$O)\n\n",
                        MakeVarName,
                        ".fact_tables.cs = $(", MakeVarName,
                        ".fact_tables:%=$(cs_subdir)%.c)\n\n"
                    ], !IO)
                ;
                    AssumeGmake = no,
                    io.write_strings(DepStream,
                        [MakeVarName, ".fact_tables.cs ="], !IO),
                    write_fact_table_dependencies_list(Globals, DepStream,
                        ".c", ModuleName, FactDeps, !IO),
                    io.write_strings(DepStream, ["\n\n", MakeVarName,
                        ".fact_tables.os ="], !IO),
                    write_fact_table_dependencies_list(Globals, DepStream,
                        ".$O", ModuleName, FactDeps, !IO),
                    io.nl(DepStream, !IO)
                )
            ;
                FactDeps = []
            ),

            ( if string.remove_suffix(SourceFileName, ".m", SourceFileBase) then
                ErrFileName = SourceFileBase ++ ".err"
            else
                unexpected($module, $pred, "source file doesn't end in `.m'")
            ),
            module_name_to_file_name(Globals, ModuleName, ".optdate",
                do_not_create_dirs, OptDateFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".c_date",
                do_not_create_dirs, CDateFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".$O",
                do_not_create_dirs, ObjFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".java_date",
                do_not_create_dirs, JavaDateFileName, !IO),
            % XXX Why is the extension hardcoded to .pic_o here?  That looks
            % wrong.  It should probably be .$(EXT_FOR_PIC_OBJECT) -
            % juliensf.
            module_name_to_file_name(Globals, ModuleName, ".pic_o",
                do_not_create_dirs, PicObjFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".int0",
                do_not_create_dirs, Int0FileName, !IO),
            io.write_strings(DepStream, ["\n\n",
                OptDateFileName, " ",
                TransOptDateFileName, " ",
                ErrFileName, " ",
                CDateFileName, " ",
                JavaDateFileName
            ], !IO),
            io.write_strings(DepStream, [" : ", SourceFileName], !IO),
            % If the module contains nested sub-modules then the `.int0' file
            % must first be built.
            ( if set.is_empty(InclDeps) then
                true
            else
                io.write_strings(DepStream, [" ", Int0FileName], !IO)
            ),
            write_dependencies_set(Globals, DepStream, ".int0", ParentDeps, !IO),
            write_dependencies_set(Globals, DepStream, ".int", LongDeps, !IO),
            write_dependencies_set(Globals, DepStream, ".int2", ShortDeps, !IO),

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
                Write = (pred(Ext::in, !.LIO::di, !:LIO::uo) is det :-
                    module_name_to_file_name(Globals, ModuleName, Ext,
                        do_not_create_dirs, ExtName, !LIO),
                    io.write_strings(DepStream, ["\n\n", ExtName, " : "], !LIO),
                    write_dependencies_set(Globals, DepStream, Ext, NestedDeps,
                        !LIO)
                ),
                list.foldl(Write, NestedExts, !IO)
            ),

            ForeignIncludeFiles = cord.list(ForeignIncludeFilesCord),
            % This is conservative: a target file for foreign language A
            % does not does not truly depend on a file included for foreign
            % language B.
            write_foreign_include_file_dependencies_list(DepStream,
                SourceFileName, ForeignIncludeFiles, !IO),

            (
                FactDeps = [_ | _],
                io.write_strings(DepStream, [
                    " \\\n\t$(", MakeVarName, ".fact_tables)\n\n",
                    "$(", MakeVarName, ".fact_tables.os) : $(",
                    MakeVarName, ".fact_tables) ",
                    SourceFileName, "\n\n",
                    "$(", MakeVarName, ".fact_tables.cs) : ",
                    ObjFileName, "\n"
                ], !IO)
            ;
                FactDeps = []
            ),

            globals.lookup_bool_option(Globals, use_opt_files, UseOptFiles),
            globals.lookup_bool_option(Globals, intermodule_optimization,
                Intermod),
            globals.lookup_accumulating_option(Globals, intermod_directories,
                IntermodDirs),

            % If intermodule_optimization is enabled then all the .mh files
            % must exist because it is possible that the .c file imports them
            % directly or indirectly.
            (
                Intermod = yes,
                io.write_strings(DepStream, ["\n\n", ObjFileName, " : "], !IO),
                write_dependencies_list(Globals, DepStream, ".mh",
                    set.to_sorted_list(AllDeps), !IO)
            ;
                Intermod = no
            ),
            ( if
                ( Intermod = yes
                ; UseOptFiles = yes
                )
            then
                io.write_strings(DepStream, [
                    "\n\n",
                    TransOptDateFileName, " ",
                    ErrFileName, " ",
                    CDateFileName, " ",
                    JavaDateFileName, " : "
                ], !IO),

                % The target (e.g. C) file only depends on the .opt files
                % from the current directory, so that inter-module optimization
                % works when the .opt files for the library are unavailable.
                % This is only necessary because make doesn't allow conditional
                % dependencies. The dependency on the current module's .opt file
                % is to make sure the module gets type-checked without having
                % the definitions of abstract types from other modules.
                %
                % XXX The code here doesn't correctly handle dependencies
                % on `.int' and `.int2' files needed by the `.opt' files.
                globals.lookup_bool_option(Globals, transitive_optimization,
                    TransOpt),
                globals.lookup_bool_option(Globals, use_trans_opt_files,
                    UseTransOpt),

                ( if
                    ( TransOpt = yes
                    ; UseTransOpt = yes
                    )
                then
                    bool.not(UseTransOpt, BuildOptFiles),
                    get_both_opt_deps(Globals, BuildOptFiles, IntermodDirs,
                        [ModuleName | set.to_sorted_list(LongDeps)],
                        OptDeps, TransOptDeps, !IO),
                    OptInt0Deps = set.union_list(
                        list.map(get_ancestors_set, OptDeps)),
                    write_dependencies_list(Globals, DepStream, ".opt",
                        OptDeps, !IO),
                    write_dependencies_set(Globals, DepStream, ".int0",
                        OptInt0Deps, !IO),

                    io.write_strings(DepStream, [
                        "\n\n",
                        ErrFileName, " ",
                        CDateFileName, " ",
                        JavaDateFileName, " : "
                    ], !IO),
                    write_dependencies_list(Globals, DepStream, ".trans_opt",
                        TransOptDeps, !IO)
                else
                    bool.not(UseOptFiles, BuildOptFiles),
                    get_opt_deps(Globals, BuildOptFiles, IntermodDirs, ".opt",
                        [ModuleName | set.to_sorted_list(LongDeps)],
                        OptDeps, !IO),
                    OptInt0Deps = set.union_list(
                        list.map(get_ancestors_set, OptDeps)),
                    write_dependencies_list(Globals, DepStream, ".opt",
                        OptDeps, !IO),
                    write_dependencies_set(Globals, DepStream, ".int0",
                        OptInt0Deps, !IO)
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
                % For --high-level-code with --target c, we need to make
                % sure that we generate the header files for imported
                % modules before compiling the C files, since the generated
                % C files #include those header files.

                io.write_strings(DepStream, [
                    "\n\n",
                    PicObjFileName, " ",
                    ObjFileName, " :"
                ], !IO),
                write_dependencies_set(Globals, DepStream, ".mih", AllDeps, !IO)
            else
                true
            ),

            % We need to tell make how to make the header files. The header
            % files are actually built by the same command that creates the
            % .c or .s file, so we just make them depend on the .c or .s
            % files.  This is needed for the --high-level-code rule above,
            % and for the rules introduced for `:- pragma
            % foreign_import_module' declarations. In some grades the header
            % file won't actually be built (e.g. LLDS grades for modules not
            % containing `:- pragma export' declarations), but this rule
            % won't do any harm.

            module_name_to_file_name(Globals, ModuleName, ".c",
                do_not_create_dirs, CFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".mh",
                do_not_create_dirs, HeaderFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".mih",
                do_not_create_dirs, HeaderFileName2, !IO),
            io.write_strings(DepStream, [
                "\n\n",
                HeaderFileName, " ",  HeaderFileName2, " : ", CFileName, "\n"
            ], !IO),

            % The `.module_dep' file is made as a side effect of
            % creating the `.c', `.s', or `.java'.

            module_name_to_file_name(Globals, ModuleName, ".java",
                do_not_create_dirs, JavaFileName, !IO),
            module_name_to_file_name(Globals, ModuleName,
                make_module_dep_file_extension, do_not_create_dirs,
                ModuleDepFileName, !IO),
            io.write_strings(DepStream, [
                "\n\n",
                "ifeq ($(findstring java,$(GRADE)),java)\n",
                ModuleDepFileName, " : ", JavaFileName, "\n",
                "else\n",
                ModuleDepFileName, " : ", CFileName, "\n",
                "endif\n"
            ], !IO),

            % The .date and .date0 files depend on the .int0 files for the
            % parent modules, and the .int3 files for the directly and
            % indirectly imported modules.
            %
            % For nested sub-modules, the `.date' files for the parent
            % modules also depend on the same things as the `.date' files
            % for this module, since all the `.date' files will get produced
            % by a single mmc command. Similarly for `.date0' files, except
            % these don't depend on the `.int0' files, because when doing
            % the `--make-private-interface' for nested modules, mmc will
            % process the modules in outermost to innermost order so as to
            % produce each `.int0' file before it is needed.

            module_name_to_file_name(Globals, ModuleName, ".date",
                do_not_create_dirs, DateFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".date0",
                do_not_create_dirs, Date0FileName, !IO),
            io.write_strings(DepStream, [
                "\n\n", DateFileName, " ", Date0FileName
            ], !IO),
            write_dependencies_set(Globals, DepStream, ".date", ParentDeps,
                !IO),
            io.write_strings(DepStream, [" : ", SourceFileName], !IO),
            write_dependencies_set(Globals, DepStream, ".int0", ParentDeps,
                !IO),
            write_dependencies_set(Globals, DepStream, ".int3", LongDeps, !IO),
            write_dependencies_set(Globals, DepStream, ".int3", ShortDeps,
                !IO),

            io.write_strings(DepStream, ["\n\n", Date0FileName], !IO),
            write_dependencies_set(Globals, DepStream, ".date0", ParentDeps,
                !IO),
            io.write_strings(DepStream, [" : ", SourceFileName], !IO),
            write_dependencies_set(Globals, DepStream, ".int3", LongDeps, !IO),
            write_dependencies_set(Globals, DepStream, ".int3", ShortDeps, !IO),
            io.write_string(DepStream, "\n\n", !IO),

            % If we can pass the module name rather than the file name, then
            % do so.  `--smart-recompilation' doesn't work if the file name
            % is passed and the module name doesn't match the file name.

            have_source_file_map(HaveMap, !IO),
            (
                HaveMap = yes,
                module_name_to_file_name_stem(SourceFileModuleName, ModuleArg)
            ;
                HaveMap = no,
                ModuleArg = SourceFileName
            ),

            globals.get_target(Globals, Target),
            module_name_to_file_name(Globals, ModuleName, ".class",
                do_not_create_dirs, ClassFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".beam",
                do_not_create_dirs, BeamFileName, !IO),

            (
                ContainsForeignCode = contains_foreign_code(_LangSet),
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
                % If we are generating the `.dep' file,
                % ForeignImportModuless0 will contain a conservative
                % approximation to the set of foreign imports needed which
                % will include imports required by imported modules.  XXX
                % ITEM_LIST What is the correctness argument that supports
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
                    C = set.union_list([
                        SrcC, IntC, OptC, IntForOptC]),
                    CSharp = set.union_list([
                        SrcCSharp, IntCSharp, OptCSharp, IntForOptCSharp]),
                    Java= set.union_list([
                        SrcJava, IntJava, OptJava, IntForOptJava]),
                    Erlang = set.union_list([
                        SrcErlang, IntErlang, OptErlang, IntForOptErlang]),
                    ForeignImportModules = foreign_import_modules(
                        C, CSharp, Java, Erlang)
                else
                    ForeignImportModules = ForeignImportModules0
                )
            ;
                ContainsForeignCode = contains_no_foreign_code,
                ForeignImportModules = ForeignImportModules0
            ),

            ForeignImports =
                get_all_foreign_import_module_infos(ForeignImportModules),

            % Handle dependencies introduced by
            % `:- pragma foreign_import_module' declarations.

            set.filter_map(
                ( pred(ForeignImportMod::in, ImportModuleName::out)
                        is semidet :-
                    ImportModuleName = foreign_import_module_name_from_module(
                        ForeignImportMod, SourceFileModuleName),

                    % XXX We can't include mercury.dll as mmake can't find
                    % it, but we know that it exists.
                    ImportModuleName \= unqualified("mercury")
                ), ForeignImports, ForeignImportedModuleNames),
            ( if set.is_empty(ForeignImportedModuleNames) then
                true
            else
                (
                    Target = target_csharp,
                    % XXX don't know enough about C# yet
                    ForeignImportTargets = [],
                    ForeignImportExt = ".cs"
                ;
                    Target = target_java,
                    ForeignImportTargets = [ClassFileName],
                    ForeignImportExt = ".java"
                ;
                    Target = target_erlang,
                    ForeignImportTargets = [BeamFileName],
                    ForeignImportExt = ".hrl"
                ;
                    Target = target_c,
                    % NOTE: for C the possible targets might be a .o file
                    % _or_ a .pic_o file. We need to include dependencies
                    % for the latter otherwise invoking mmake with a
                    % <module>.pic_o target will break.
                    ForeignImportTargets = [ObjFileName, PicObjFileName],
                    ForeignImportExt = ".mh"
                ),
                WriteForeignImportTarget =
                    ( pred(ForeignImportTarget::in, !.IO::di, !:IO::uo)
                            is det :-
                        io.write_string(DepStream, "\n\n", !IO),
                        io.write_string(DepStream, ForeignImportTarget, !IO),
                        io.write_string(DepStream, " : ", !IO),
                        write_dependencies_list(Globals, DepStream,
                            ForeignImportExt,
                            set.to_sorted_list(ForeignImportedModuleNames),
                            !IO),
                        io.write_string(DepStream, "\n\n", !IO)
                    ),
                list.foldl(WriteForeignImportTarget, ForeignImportTargets,
                    !IO)
            ),

            module_name_to_file_name(Globals, ModuleName, ".int",
                do_not_create_dirs, IntFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".int2",
                do_not_create_dirs, Int2FileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".int3",
                do_not_create_dirs, Int3FileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".opt",
                do_not_create_dirs, OptFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".trans_opt",
                do_not_create_dirs, TransOptFileName, !IO),
            module_name_to_file_name(Globals, ModuleName, ".date3",
                do_not_create_dirs, Date3FileName, !IO),

            % We add some extra dependencies to the generated `.d' files, so
            % that local `.int', `.opt', etc. files shadow the installed
            % versions properly (e.g. for when you're trying to build a new
            % version of an installed library). This saves the user from
            % having to add these explicitly if they have multiple libraries
            % installed in the same installation hierarchy which aren't
            % independent (e.g.  one uses another). These extra dependencies
            % are necessary due to the way the combination of search paths
            % and pattern rules works in Make.
            %
            % Be very careful about changing the following rules. The `@:'
            % is a silent do-nothing command. It is used to force GNU Make
            % to recheck the timestamp on the target file. (It is a pity
            % that GNU Make doesn't have a way of handling these sorts of
            % rules in a nicer manner.)

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
                "\t@:\n"
            ], !IO),

            globals.lookup_bool_option(Globals, use_subdirs, UseSubdirs),
            (
                UseSubdirs = yes,
                io.nl(DepStream, !IO),
                list.foldl(
                    write_subdirs_shorthand_rule(Globals, DepStream,
                        ModuleName),
                    [".c", ".$O", ".pic_o", ".java", ".class", ".dll"], !IO)
            ;
                UseSubdirs = no
            ),

            ( if SourceFileName = default_source_file(ModuleName) then
                true
            else
                % The pattern rules in Mmake.rules won't work, since the
                % source file name doesn't match the expected source file
                % name for this module name. This can occur due to just the
                % use of different source file names, or it can be due to
                % the use of nested modules.  So we need to output
                % hard-coded rules in this case.
                %
                % The rules output below won't work in the case of nested
                % modules with parallel makes, because it will end up
                % invoking the same command twice (since it produces two
                % output files) at the same time.
                %
                % Any changes here will require corresponding changes to
                % scripts/Mmake.rules. See that file for documentation on
                % these rules.

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
                    TransOptDateFileName, " : ", SourceFileName,
                        "\n",
                    "\t$(MCTOI) $(ALL_GRADEFLAGS) ",
                        "$(ALL_MCTOIFLAGS) ", ModuleArg, "\n",
                    CDateFileName, " : ", SourceFileName, "\n",
                    "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                        ModuleArg, " $(ERR_REDIRECT)\n",
                    JavaDateFileName, " : ", SourceFileName, "\n",
                    "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                        "--java-only ", ModuleArg,
                        " $(ERR_REDIRECT)\n"
                ], !IO)
            ),

            io.close_output(DepStream, !IO),
            io.rename_file(TmpDependencyFileName, DependencyFileName,
                Result3, !IO),
            (
                Result3 = error(_),
                % On some systems, we need to remove the existing file
                % first, if any. So try again that way.
                io.remove_file(DependencyFileName, Result4, !IO),
                (
                    Result4 = error(Error4),
                    maybe_write_string(Verbose, " failed.\n", !IO),
                    maybe_flush_output(Verbose, !IO),
                    io.error_message(Error4, ErrorMsg),
                    string.append_list(["can't remove file `",
                        DependencyFileName, "': ", ErrorMsg], Message),
                    report_error(Message, !IO)
                ;
                    Result4 = ok,
                    io.rename_file(TmpDependencyFileName, DependencyFileName,
                        Result5, !IO),
                    (
                        Result5 = error(Error5),
                        maybe_write_string(Verbose, " failed.\n", !IO),
                        maybe_flush_output(Verbose, !IO),
                        io.error_message(Error5, ErrorMsg),
                        string.append_list(["can't rename file `",
                            TmpDependencyFileName, "' as `",
                            DependencyFileName, "': ", ErrorMsg], Message),
                        report_error(Message, !IO)
                    ;
                        Result5 = ok,
                        maybe_write_string(Verbose, " done.\n", !IO)
                    )
                )
            ;
                Result3 = ok,
                maybe_write_string(Verbose, " done.\n", !IO)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred write_dependencies_set(globals::in, io.output_stream::in,
    string::in, set(module_name)::in, io::di, io::uo) is det.

write_dependencies_set(Globals, DepStream, Suffix, Modules, !IO) :-
    write_dependencies_list(Globals, DepStream, Suffix,
        set.to_sorted_list(Modules), !IO).

:- pred write_dependencies_list(globals::in, io.output_stream::in,
    string::in, list(module_name)::in, io::di, io::uo) is det.

write_dependencies_list(_, _, _, [], !IO).
write_dependencies_list(Globals, DepStream, Suffix, [Module | Modules], !IO) :-
    module_name_to_file_name(Globals, Module, Suffix, do_not_create_dirs,
        FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_dependencies_list(Globals, DepStream, Suffix, Modules, !IO).

:- pred write_compact_dependencies_list(globals::in, io.output_stream::in,
    string::in, string::in, maybe(pair(string))::in, list(module_name)::in,
    io::di, io::uo) is det.

write_compact_dependencies_list(Globals, DepStream, Prefix, Suffix, Basis,
        Modules, !IO) :-
    (
        Basis = yes(VarName - OldSuffix),
        io.write_string(DepStream, "$(", !IO),
        io.write_string(DepStream, VarName, !IO),
        io.write_string(DepStream, ":%", !IO),
        io.write_string(DepStream, OldSuffix, !IO),
        io.write_string(DepStream, "=", !IO),
        io.write_string(DepStream, Prefix, !IO),
        io.write_string(DepStream, "%", !IO),
        io.write_string(DepStream, Suffix, !IO),
        io.write_string(DepStream, ")", !IO)
    ;
        Basis = no,
        write_dependencies_list(Globals, DepStream, Suffix, Modules, !IO)
    ).

:- pred write_compact_dependencies_separator(io.output_stream::in,
    maybe(pair(string))::in, io::di, io::uo) is det.

write_compact_dependencies_separator(_DepStream, no, !IO).
write_compact_dependencies_separator(DepStream, yes(_), !IO) :-
    io.write_string(DepStream, " ", !IO).

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

:- pred write_foreign_include_file_dependency(io.output_stream::in,
    file_name::in, foreign_include_file_info::in, io::di, io::uo) is det.

write_foreign_include_file_dependency(DepStream, SourceFileName,
        IncludeFile, !IO) :-
    IncludeFile = foreign_include_file_info(_Lang, IncludeFileName),
    make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, IncludePath, !IO).

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
    string::in, assoc_list(file_name, module_name)::in, io::di, io::uo) is det.

write_extra_link_dependencies_list(_, _, _, [], !IO).
write_extra_link_dependencies_list(Globals, DepStream, Suffix,
        [ExtraLink - Module | ExtraLinks], !IO) :-
    extra_link_obj_file_name(Globals, Module, ExtraLink, Suffix,
        do_not_create_dirs, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_extra_link_dependencies_list(Globals, DepStream, Suffix,
        ExtraLinks, !IO).

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
    module_name::in, string::in, io::di, io::uo) is det.

write_subdirs_shorthand_rule(Globals, DepStream, ModuleName, Ext, !IO) :-
    module_name_to_file_name_stem(ModuleName, ModuleStr),
    module_name_to_file_name(Globals, ModuleName, Ext, do_not_create_dirs,
        Target, !IO),
    ShorthandTarget = ModuleStr ++ Ext,
    io.write_string(DepStream, ".PHONY: ", !IO),
    io.write_string(DepStream, ShorthandTarget, !IO),
    io.nl(DepStream, !IO),
    io.write_string(DepStream, ShorthandTarget, !IO),
    io.write_string(DepStream, ": ", !IO),
    io.write_string(DepStream, Target, !IO),
    io.nl(DepStream, !IO).

%-----------------------------------------------------------------------------%

generate_dependencies_write_d_files(_, [], _, _, _, _, _, _, !IO).
generate_dependencies_write_d_files(Globals, [Dep | Deps],
        IntDepsGraph, ImpDepsGraph, IndirectDepsGraph, IndirectOptDepsGraph,
        TransOptOrder, DepsMap, !IO) :-
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
        ),
        generate_dependencies_write_d_files(Globals, Deps,
            IntDepsGraph, ImpDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptOrder, DepsMap, !IO)
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

%-----------------------------------------------------------------------------%

generate_dependencies_write_dv_file(Globals, SourceFileName, ModuleName,
        DepsMap, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_file_name(Globals, ModuleName, ".dv", do_create_dirs,
        DvFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DvFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io.open_output(DvFileName, DvResult, !IO),
    (
        DvResult = ok(DvStream),
        generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap,
            DvStream, !IO),
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

%-----------------------------------------------------------------------------%

generate_dependencies_write_dep_file(Globals, SourceFileName, ModuleName,
        DepsMap, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_file_name(Globals, ModuleName, ".dep", do_create_dirs,
        DepFileName, !IO),
    maybe_write_string(Verbose, "% Creating auto-dependency file `", !IO),
    maybe_write_string(Verbose, DepFileName, !IO),
    maybe_write_string(Verbose, "'...\n", !IO),
    io.open_output(DepFileName, DepResult, !IO),
    (
        DepResult = ok(DepStream),
        generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap,
            DepStream, !IO),
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

%-----------------------------------------------------------------------------%

:- pred generate_dv_file(globals::in, file_name::in, module_name::in,
    deps_map::in, io.output_stream::in, io::di, io::uo) is det.

generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap, DepStream,
        !IO) :-
    io.write_string(DepStream,
        "# Automatically generated dependency variables for module `", !IO),
    ModuleNameString = sym_name_to_string(ModuleName),
    io.write_string(DepStream, ModuleNameString, !IO),
    io.write_string(DepStream, "'\n", !IO),
    io.write_string(DepStream, "# generated from source file `", !IO),
    io.write_string(DepStream, SourceFileName, !IO),
    io.write_string(DepStream, "'\n", !IO),

    library.version(Version, Fullarch),
    io.write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io.write_string(DepStream, Version, !IO),
    io.write_string(DepStream, ",\n# configured for ", !IO),
    io.write_string(DepStream, Fullarch, !IO),
    io.write_string(DepStream, ".\n\n", !IO),

    map.keys(DepsMap, Modules0),
    select_ok_modules(Modules0, DepsMap, Modules1),
    list.sort(compare_module_names, Modules1, Modules),

    module_name_to_make_var_name(ModuleName, MakeVarName),
    list.map(get_source_file(DepsMap), Modules, SourceFiles0),
    list.sort_and_remove_dups(SourceFiles0, SourceFiles),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ms =", !IO),
    write_file_dependencies_list(DepStream, ".m", SourceFiles, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".errs =", !IO),
    write_file_dependencies_list(DepStream, ".err", SourceFiles, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mods =", !IO),
    write_dependencies_list(Globals, DepStream, "", Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The modules for which we need to generate .int0 files.
    ModulesWithSubModules = list.filter(
      ( pred(Module::in) is semidet :-
          map.lookup(DepsMap, Module, deps(_, ModuleImports)),
          set.non_empty(ModuleImports ^ mai_children)
      ), Modules),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".parent_mods =", !IO),
    write_dependencies_list(Globals, DepStream, "", ModulesWithSubModules,
        !IO),
    io.write_string(DepStream, "\n", !IO),

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
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign =", !IO),
    write_dependencies_list(Globals, DepStream, "", ForeignModules, !IO),
    io.write_string(DepStream, "\n\n", !IO),

    globals.lookup_bool_option(Globals, assume_gmake, Gmake),
    (
        Gmake = yes,
        string.append(MakeVarName, ".mods", ModsVarName),
        Basis = yes(ModsVarName - ""),

        string.append(MakeVarName, ".foreign", ForeignVarName),
        ForeignBasis = yes(ForeignVarName - ""),

        string.append(MakeVarName, ".parent_mods", ParentModsVarName),
        ParentBasis = yes(ParentModsVarName - "")
    ;
        Gmake = no,
        Basis = no,
        ForeignBasis = no,
        ParentBasis = no
    ),

    get_extra_link_objects(Modules, DepsMap, Target, ExtraLinkObjs),

    MakeFileName = (pred(M - E::in, F::out, IO0::di, IO::uo) is det :-
        module_name_to_file_name(Globals, M, E, do_create_dirs, F0, IO0, IO),
        F = "$(os_subdir)" ++ F0
    ),

    list.map_foldl(MakeFileName, ForeignModulesAndExts, ForeignFileNames, !IO),

    % .foreign_cs are the source files which have had foreign code placed
    % in them.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign_cs = ", !IO),
    write_file_dependencies_list(DepStream, "", ForeignFileNames, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The dlls that contain the foreign_code.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign_dlls = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(dlls_subdir)", ".dll", ForeignBasis, ForeignModules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".init_cs = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(cs_subdir)", ".c", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".cs = $(", !IO),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".init_cs) ", !IO),
    write_extra_link_dependencies_list(Globals, DepStream,
        ".c", ExtraLinkObjs, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dlls = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(dlls_subdir)", ".dll", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_os = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(os_subdir)", ".$O", Basis, Modules, !IO),
    write_extra_link_dependencies_list(Globals, DepStream,
        ".$O",  ExtraLinkObjs, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_os = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(os_subdir)", ".$(EXT_FOR_PIC_OBJECTS)", Basis, Modules, !IO),
    write_extra_link_dependencies_list(Globals, DepStream,
        ".$(EXT_FOR_PIC_OBJECTS)", ExtraLinkObjs, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".os = $(", !IO),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_os)\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".pic_os = $(", !IO),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_os)\n", !IO),

    % $(foo.cs_or_ss) contains the names of the generated intermediate
    % files between `.m' and `.o' files. This is used in foo.dep
    % to make sure the intermediate files are generated before the
    % object files, so that errors are reported as soon as possible.
    %
    % XXX the .cs_or_ss target used to be required for the GCC backend.
    % We should remove it at some point.

    io.write_strings(DepStream, [
        MakeVarName, ".cs_or_ss =$(", MakeVarName, ".cs)\n\n"
    ], !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".useds = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(useds_subdir)", ".used", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".javas = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(javas_subdir)", ".java", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".classes = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(classes_subdir)", ".class", Basis, Modules, !IO),
    io.write_string(DepStream, " ", !IO),
    % The Java compiler creates a .class file for each class within the
    % original .java file. The filenames of all these can be matched with
    % `module\$*.class', hence the "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    % XXX This relies on GNU Make.
    io.write_string(DepStream, "$(wildcard ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(classes_subdir)", "\\$$*.class", Basis, Modules, !IO),
    io.write_string(DepStream, ")\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dirs = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(dirs_subdir)", ".dir", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dir_os = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(dirs_subdir)", ".dir/*.$O", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dates = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(dates_subdir)", ".date", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".date0s = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(date0s_subdir)", ".date0", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".date3s = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(date3s_subdir)", ".date3", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".optdates = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(optdates_subdir)", ".optdate", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".trans_opt_dates = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(trans_opt_dates_subdir)", ".trans_opt_date", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".c_dates = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(c_dates_subdir)", ".c_date", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".java_dates = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(java_dates_subdir)", ".java_date", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ds = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(ds_subdir)", ".d", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".module_deps = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(module_deps_subdir)", make_module_dep_file_extension,
        Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mihs = ", !IO),
    globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
    (
        HighLevelCode = yes,
        (
            Target = target_c,
            % For the `--target c' MLDS back-end, we generate `.mih' files
            % for every module.
            write_compact_dependencies_list(Globals, DepStream,
                "$(mihs_subdir)", ".mih", Basis, Modules, !IO)
        ;
            % For the Java target, currently we don't generate
            % `.mih' files at all; although perhaps we should...
            % XXX Why?  What's that comment even supposed to mean?
            % - juliensf
            ( Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            )
        )
    ;
        % For the LLDS back-end, we don't use `.mih' files at all
        HighLevelCode = no
    ),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mhs = ", !IO),
    (
        Target = target_c,
        write_compact_dependencies_list(Globals, DepStream, "", ".mh", Basis,
            Modules, !IO)
    ;
        ( Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        )
    ),
    io.write_string(DepStream, "\n", !IO),

    % The `<module>.all_mihs' variable is like `<module>.mihs' except
    % that it contains header files for all the modules, regardless
    % of the grade or --target option. It is used by the rule for
    % `mmake realclean', which should remove anything that could have
    % been automatically generated, even if the grade or --target option
    % has changed.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_mihs = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(mihs_subdir)", ".mih", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except
    % that it contains header files for all the modules, as for
    % `<module>.all_mihs' above.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_mhs = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "", ".mh", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ints = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(ints_subdir)", ".int", Basis, Modules, !IO),
    write_compact_dependencies_separator(DepStream, Basis, !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(int2s_subdir)", ".int2", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    % `.int0' files are only generated for modules with sub-modules.
    % XXX ... or at least they should be. Currently we end up generating
    % .int0 files for nested submodules that don't have any children.
    % (We do the correct thing for separate sub-modules.)

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".int0s = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(int0s_subdir)", ".int0", ParentBasis, ModulesWithSubModules, !IO),
    io.write_string(DepStream, "\n", !IO),

    % XXX The `<module>.all_int0s' variables is like `<module>.int0s' except
    % that it contains .int0 files for all modules, regardless of whether
    % they should have been created or not. It is used by the rule for
    % `mmake realclean' to ensure that we clean up all the .int0 files,
    % including the ones that were accidently created by the bug described
    % above.

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_int0s = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(int0s_subdir)", ".int0", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".int3s = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(int3s_subdir)", ".int3", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".opts = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(opts_subdir)", ".opt", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".trans_opts = ", !IO),
    write_compact_dependencies_list(Globals,DepStream,
        "$(trans_opts_subdir)", ".trans_opt", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".analysiss = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(analysiss_subdir)", ".analysis", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".requests = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(requests_subdir)", ".request", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".imdgs = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "$(imdgs_subdir)", ".imdg", Basis, Modules, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".profs = ", !IO),
    write_compact_dependencies_list(Globals, DepStream,
        "", ".prof", Basis,  Modules, !IO),
    io.write_string(DepStream, "\n\n", !IO).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

:- pred generate_dep_file(globals::in, file_name::in, module_name::in,
    deps_map::in, io.output_stream::in, io::di, io::uo) is det.

generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap, DepStream,
        !IO) :-
    io.write_string(DepStream,
        "# Automatically generated dependencies for module `", !IO),
    ModuleNameString = sym_name_to_string(ModuleName),
    io.write_string(DepStream, ModuleNameString, !IO),
    io.write_string(DepStream, "'\n", !IO),
    io.write_string(DepStream,
        "# generated from source file `", !IO),
    io.write_string(DepStream, SourceFileName, !IO),
    io.write_string(DepStream, "'\n", !IO),

    library.version(Version, Fullarch),
    io.write_string(DepStream,
        "# Generated by the Mercury compiler, version ", !IO),
    io.write_string(DepStream, Version, !IO),
    io.write_string(DepStream, "\n# configured for ", !IO),
    io.write_string(DepStream, Fullarch, !IO),
    io.write_string(DepStream, ".\n\n", !IO),

    module_name_to_make_var_name(ModuleName, MakeVarName),

    module_name_to_file_name(Globals, ModuleName, ".init",
        do_create_dirs, InitFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, "_init.c",
        do_create_dirs, InitCFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, "_init.$O",
        do_create_dirs, InitObjFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, "_init.pic_o",
        do_create_dirs, InitPicObjFileName, !IO),

    globals.lookup_bool_option(Globals, intermodule_optimization, Intermod),
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    globals.lookup_bool_option(Globals, generate_mmc_make_module_dependencies,
        MmcMakeDeps),

    generate_dep_file_exec_library_targets(Globals, DepStream, ModuleName,
        MakeVarName, InitFileName, InitObjFileName,
        Intermod, TransOpt, MmcMakeDeps,
        MaybeOptsVar, MaybeTransOptsVar, MaybeModuleDepsVar,
        ExeFileName, JarFileName, LibFileName, SharedLibFileName, !IO),
    generate_dep_file_init_targets(Globals, DepStream, ModuleName,
        MakeVarName, InitCFileName, InitFileName, DepFileName, DvFileName,
        !IO),
    generate_dep_file_install_targets(Globals, DepStream, ModuleName, DepsMap,
        MakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVar, MaybeOptsVar, MaybeTransOptsVar, !IO),
    generate_dep_file_collective_targets(Globals, DepStream, ModuleName,
        MakeVarName, !IO),
    generate_dep_file_clean_targets(Globals, DepStream, ModuleName,
        MakeVarName, ExeFileName, InitCFileName,
        InitObjFileName, InitPicObjFileName, InitFileName,
        LibFileName, SharedLibFileName, JarFileName, DepFileName, DvFileName,
        !IO).

:- pred generate_dep_file_exec_library_targets(globals::in,
    io.output_stream::in, module_name::in, string::in, string::in, string::in,
    bool::in, bool::in, bool::in, string::out, string::out, string::out,
    string::out, string::out, string::out, string::out, io::di, io::uo) is det.

generate_dep_file_exec_library_targets(Globals, DepStream, ModuleName,
        MakeVarName, InitFileName, InitObjFileName,
        Intermod, TransOpt, MmcMakeDeps,
        MaybeOptsVar, MaybeTransOptsVar, MaybeModuleDepsVar,
        ExeFileName, JarFileName, LibFileName, SharedLibFileName, !IO) :-
    % Note we have to do some ``interesting'' hacks to get
    % `$(ALL_MLLIBS_DEP)' to work in the dependency list
    % (and not complain about undefined variables).
    % These hacks rely on features of GNU Make, so should not be used
    % if we cannot assume we are using GNU Make.
    globals.lookup_bool_option(Globals, assume_gmake, Gmake),
    (
        Gmake = yes,
        append_list(["\\\n\t\t$(foreach @,", MakeVarName,
            ",$(ALL_MLLIBS_DEP))"],
            All_MLLibsDepString),
        append_list(["\\\n\t\t$(foreach @,", MakeVarName,
            ",$(ALL_MLOBJS))"],
            All_MLObjsString),
        append_list([
        "\\\n\t\t$(patsubst %.o,%.$(EXT_FOR_PIC_OBJECTS),$(foreach @,",
            MakeVarName, ",$(ALL_MLOBJS)))"],
            All_MLPicObjsString)
    ;
        Gmake = no,
        All_MLLibsDepString = "$(ALL_MLLIBS_DEP)",
        All_MLObjsString = "$(ALL_MLOBJS)",
        All_MLPicObjsString = "$(ALL_MLPICOBJS)"
    ),

    % When compiling to C, we want to include $(foo.cs) first in
    % the dependency list, before $(foo.os).
    % This is not strictly necessary, since the .$O files themselves depend
    % on the .c files, but want to do it to ensure that Make will try to
    % create all the C files first, thus detecting errors early,
    % rather than first spending time compiling C files to .$O,
    % which could be a waste of time if the program contains errors.
    %
    % When compiling to assembler, we want to do the same kind of thing,
    % for the same reason, but with the `.s' files rather than the `.c' files.

    module_name_to_file_name(Globals, ModuleName, "",
        do_not_create_dirs, ExeFileName, !IO),

    IfJava = ["ifeq ($(findstring java,$(GRADE)),java)\n"],
    JavaMainRule = [ExeFileName, " : $(", MakeVarName, ".classes)\n"],
    Else = ["else\n"],
    EndIf = ["endif\n"],

    % XXX The output here is GNU Make-specific.
    io.write_strings(DepStream, [
        "ifneq ($(EXT_FOR_EXE),)\n",
        ".PHONY : ", ExeFileName, "\n",
        ExeFileName, " : ", ExeFileName, "$(EXT_FOR_EXE)\n",
        "endif\n"
    ], !IO),

    MainRule =
        [ExeFileName, "$(EXT_FOR_EXE) : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".os) ", InitObjFileName, " ",
            All_MLObjsString, " ", All_MLLibsDepString, "\n",
        "\t$(ML) $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) -- $(ALL_LDFLAGS) ",
            "$(EXEFILE_OPT)", ExeFileName, "$(EXT_FOR_EXE) ",
            InitObjFileName, " \\\n",
        "\t\t$(", MakeVarName, ".os) ", All_MLObjsString, " $(ALL_MLLIBS)\n"],

    globals.get_target(Globals, Target),
    (
        Gmake = yes,
        Rules = IfJava ++ JavaMainRule ++ Else ++ MainRule ++ EndIf
    ;
        Gmake = no,
        (
            Target = target_csharp,
            % XXX not yet
            Rules = []
        ;
            Target = target_java,
            Rules = JavaMainRule
        ;
            Target = target_erlang,
            % XXX not yet
            Rules = []
        ;
            Target = target_c,
            Rules = MainRule
        )
    ),
    io.write_strings(DepStream, Rules, !IO),

    (
        Intermod = yes,
        MaybeOptsVar = "$(" ++ MakeVarName ++ ".opts) "
    ;
        Intermod = no,
        MaybeOptsVar = ""
    ),
    (
        TransOpt = yes,
        MaybeTransOptsVar = "$(" ++ MakeVarName ++ ".trans_opts) "
    ;
        TransOpt = no,
        MaybeTransOptsVar = ""
    ),
    (
        MmcMakeDeps = yes,
        MaybeModuleDepsVar = "$(" ++ MakeVarName ++ ".module_deps) "
    ;
        MmcMakeDeps = no,
        MaybeModuleDepsVar = ""
    ),

    module_name_to_lib_file_name(Globals, "lib", ModuleName, "",
        do_not_create_dirs, LibTargetName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName, ".$A",
        do_create_dirs, LibFileName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".$(EXT_FOR_SHARED_LIB)", do_create_dirs, SharedLibFileName, !IO),
    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".$(EXT_FOR_SHARED_LIB)", do_not_create_dirs, MaybeSharedLibFileName,
        !IO),
    module_name_to_file_name(Globals, ModuleName, ".jar",
        do_not_create_dirs, JarFileName, !IO),

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

    AllInts = [
        "$(", MakeVarName, ".ints) ",
        "$(", MakeVarName, ".int3s) ",
        MaybeOptsVar, MaybeTransOptsVar,
        InitFileName, "\n\n"
    ],
    JavaLibRule = [
        LibTargetName, " : ", JarFileName, " \\\n\t\t"
        | AllInts
    ],
    LibRule = [
        LibTargetName, " : ", LibFileName, " ",
        MaybeSharedLibFileName, " \\\n\t\t"
        | AllInts
    ],
    (
        Gmake = yes,
        LibRules = IfJava ++ JavaLibRule ++ Else ++
            LibRule ++ EndIf
    ;
        Gmake = no,
        (
            Target = target_csharp,
            % XXX not done yet
            LibRules = []
        ;
            Target = target_java,
            LibRules = JavaLibRule
        ;
            Target = target_erlang,
            % XXX not done yet
            LibRules = []
        ;
            Target = target_c,
            LibRules = LibRule
        )
    ),
    io.write_strings(DepStream, [
        ".PHONY : ", LibTargetName, "\n" |
        LibRules
    ], !IO),

    io.write_strings(DepStream, [
        "ifneq ($(EXT_FOR_SHARED_LIB),$A)\n",
        SharedLibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".pic_os) ",
            All_MLPicObjsString, " ", All_MLLibsDepString, "\n",
        "\t$(ML) --make-shared-lib $(ALL_GRADEFLAGS) $(ALL_MLFLAGS) ",
            "-- ", InstallNameOpt, " $(ALL_LD_LIBFLAGS) -o ",
            SharedLibFileName, " \\\n",
        "\t\t$(", MakeVarName, ".pic_os) ", All_MLPicObjsString,
            " $(ALL_MLLIBS)\n",
        "endif\n\n"
    ], !IO),

    io.write_strings(DepStream, [
        LibFileName, " : $(", MakeVarName, ".cs_or_ss) ",
            "$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
        "\trm -f ", LibFileName, "\n",
        "\t$(AR) $(ALL_ARFLAGS) $(AR_LIBFILE_OPT)", LibFileName, " ",
            "$(", MakeVarName, ".os) ", All_MLObjsString, "\n",
        "\t$(RANLIB) $(ALL_RANLIBFLAGS) ", LibFileName, "\n\n"
    ], !IO),

    ClassFiles = "$(" ++ MakeVarName ++ ".classes)",
    list_class_files_for_jar_mmake(Globals, ClassFiles, ListClassFiles),
    io.write_strings(DepStream, [
        JarFileName, " : ", "$(", MakeVarName, ".classes)\n",
        "\t$(JAR) $(JAR_CREATE_FLAGS) ", JarFileName, " ",
        ListClassFiles, "\n\n"
    ], !IO).

:- pred generate_dep_file_init_targets(globals::in, io.output_stream::in,
    module_name::in, string::in, string::in, string::in,
    string::out, string::out, io::di, io::uo) is det.

generate_dep_file_init_targets(Globals, DepStream, ModuleName,
        MakeVarName, InitCFileName, InitFileName, DepFileName, DvFileName,
        !IO) :-
    module_name_to_file_name(Globals, ModuleName, ".dep",
        do_not_create_dirs, DepFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, ".dv",
        do_not_create_dirs, DvFileName, !IO),

    io.write_strings(DepStream, [
        InitFileName, " : ", DepFileName, " $(", MakeVarName, ".cs)\n",
        "\techo > ", InitFileName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        "\t$(MKLIBINIT) ", "$(", MakeVarName, ".cs)", " >> ",
        InitFileName, "\n"
    ], !IO),

    % $(EXTRA_INIT_COMMAND) should expand to a command to
    % generate extra entries in the `.init' file for a library.
    % It may expand to the empty string.
    io.write_string(DepStream, "\t$(EXTRA_INIT_COMMAND) >> ", !IO),
    io.write_string(DepStream, InitFileName, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The `force-module_init' dependency forces the commands for
    % the `module_init.c' rule to be run every time the rule
    % is considered.
    ModuleFileName = sym_name_to_string(ModuleName),
    ForceC2InitTarget = "force-" ++ ModuleFileName ++ "_init",
    TmpInitCFileName = InitCFileName ++ ".tmp",
    io.write_strings(DepStream, [
        ForceC2InitTarget, " :\n\n",
        InitCFileName, " : ", ForceC2InitTarget, " $(", MakeVarName, ".cs)\n",
        "\t@$(C2INIT) $(ALL_GRADEFLAGS) $(ALL_C2INITFLAGS) ",
            "--init-c-file ", TmpInitCFileName,
            " $(", MakeVarName, ".init_cs) $(ALL_C2INITARGS)\n",
        "\t@mercury_update_interface ", InitCFileName, "\n\n"
    ], !IO).

:- pred generate_dep_file_install_targets(globals::in, io.output_stream::in,
    module_name::in, deps_map::in, string::in, bool::in, bool::in, bool::in,
    string::in, string::in, string::in, io::di, io::uo) is det.

generate_dep_file_install_targets(Globals, DepStream, ModuleName, DepsMap,
        MakeVarName, MmcMakeDeps, Intermod, TransOpt,
        MaybeModuleDepsVar, MaybeOptsVar, MaybeTransOptsVar, !IO) :-
    % XXX  Note that we install the `.opt' and `.trans_opt' files
    % in two places: in the `lib/$(GRADE)/opts' directory, so
    % that mmc will find them, and also in the `ints' directory,
    % so that Mmake will find them. That's not ideal, but it works.

    module_name_to_lib_file_name(Globals, "lib", ModuleName, ".install_ints",
        do_not_create_dirs, LibInstallIntsTargetName, !IO),
    (
        Intermod = yes,
        OptStr = " opt"
    ;
        Intermod = no,
        OptStr = ""
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
        Int0Str = " int0",
        MaybeInt0sVar = "$(" ++ MakeVarName ++ ".int0s) "
    else
        Int0Str = "",
        MaybeInt0sVar = ""
    ),
    (
        TransOpt = yes,
        TransOptStr = " trans_opt"
    ;
        TransOpt = no,
        TransOptStr = ""
    ),
    (
        MmcMakeDeps = yes,
        DepStr = " module_dep"
    ;
        MmcMakeDeps = no,
        DepStr = ""
    ),

    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallIntsTargetName, "\n",
        LibInstallIntsTargetName, " : $(", MakeVarName, ".ints) $(",
            MakeVarName, ".int3s) ", MaybeInt0sVar, MaybeOptsVar,
            MaybeTransOptsVar, MaybeModuleDepsVar,
            " install_lib_dirs\n",
        "\tfiles=""$(", MakeVarName, ".ints) $(", MakeVarName,
            ".int3s) ", MaybeInt0sVar, MaybeOptsVar,
            MaybeTransOptsVar, MaybeModuleDepsVar, """; \\\n",
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
        Int0Str, OptStr, TransOptStr, DepStr,
        "; do \\\n",
        "\t\tdir=""$(INSTALL_INT_DIR)/Mercury/$${ext}s""; \\\n",
        "\t\trm -rf ""$$dir""; \\\n",
        "\t\t$(LN_S) .. ""$$dir"" || { \\\n",
        "\t\t\t{ [ -d ""$$dir"" ] || \\\n",
        "\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\\n",
        "\t\t\t$(INSTALL) ""$(INSTALL_INT_DIR)""/*.$$ext ""$$dir""; \\\n",
        "\t\t} || exit 1; \\\n",
        "\tdone\n\n"
    ], !IO),

    module_name_to_lib_file_name(Globals, "lib", ModuleName, ".install_opts",
        do_not_create_dirs, LibInstallOptsTargetName, !IO),
    io.write_strings(DepStream,
        [".PHONY : ", LibInstallOptsTargetName, "\n",
        LibInstallOptsTargetName, " : "], !IO),
    ( if
        Intermod = no,
        TransOpt = no
    then
        io.write_string(DepStream, "\n\t@:\n\n", !IO)
    else
        io.write_strings(DepStream, [
            MaybeOptsVar, MaybeTransOptsVar, "install_grade_dirs\n",
            "\tfiles=""", MaybeOptsVar, MaybeTransOptsVar, """; \\\n",
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
            "\tfor ext in ", OptStr, TransOptStr, "; do \\\n",
            "\t\tdir=""$(INSTALL_GRADE_INT_DIR)/Mercury/$${ext}s""; \\\n",
            "\t\trm -rf ""$$dir""; \\\n",
            "\t\t$(LN_S) .. ""$$dir"" || { \\\n",
            "\t\t\t{ [ -d ""$$dir"" ] || \\\n",
            "\t\t\t\t$(INSTALL_MKDIR) ""$$dir""; } && \\\n",
            "\t\t\t$(INSTALL) ""$(INSTALL_GRADE_INT_DIR)""/*.$$ext \\\n",
            "\t\t\t\t""$$dir""; \\\n",
            "\t\t} || exit 1; \\\n",
            "\tdone\n\n"
        ], !IO)
    ),

    % XXX  Note that we install the header files in two places:
    % in the `lib/inc' or `lib/$(GRADE)/$(FULLARCH)/inc' directory,
    % so that the C compiler will find them, and also in the `ints' directory,
    % so that Mmake will find them. That's not ideal, but it works.
    %
    % (A better fix would be to change the VPATH setting in
    % scripts/Mmake.vars.in so that Mmake also searches the
    % `lib/$(GRADE)/$(FULLARCH)/inc' directory, but doing that properly
    % is non-trivial.)

    module_name_to_lib_file_name(Globals, "lib", ModuleName, ".install_hdrs",
        do_not_create_dirs, LibInstallHdrsTargetName, !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallHdrsTargetName, "\n",
        LibInstallHdrsTargetName, " : ",
            "$(", MakeVarName, ".mhs) ",
            "install_lib_dirs\n",
        "ifeq ($(", MakeVarName, ".mhs),)\n",
        "\t@:\n",
        "else\n",
        "\tfor hdr in $(", MakeVarName, ".mhs); do \\\n",
        "\t\t$(INSTALL) $$hdr $(INSTALL_INT_DIR); \\\n",
        "\t\t$(INSTALL) $$hdr $(INSTALL_INC_DIR); \\\n",
        "\tdone\n",
        "endif\n\n"], !IO),

    module_name_to_lib_file_name(Globals, "lib", ModuleName,
        ".install_grade_hdrs", do_not_create_dirs,
        LibInstallGradeHdrsTargetName, !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", LibInstallGradeHdrsTargetName, "\n",
        LibInstallGradeHdrsTargetName, " : ",
            "$(", MakeVarName, ".mihs) ",
            "install_grade_dirs\n",
    "ifeq ($(", MakeVarName, ".mihs),)\n",
    "\t@:\n",
    "else\n",
    "\tfor hdr in $(", MakeVarName, ".mihs); do \\\n",
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
    "endif\n\n"], !IO).

:- pred generate_dep_file_collective_targets(globals::in, io.output_stream::in,
    module_name::in, string::in, io::di, io::uo) is det.

generate_dep_file_collective_targets(Globals, DepStream, ModuleName,
        MakeVarName, !IO) :-
    list.foldl(
        generate_dep_file_collective_target(Globals, DepStream, ModuleName,
            MakeVarName),
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
        ], !IO).

:- pred generate_dep_file_collective_target(globals::in, io.output_stream::in,
    module_name::in, string::in, pair(string, string)::in,
    io::di, io::uo) is det.

generate_dep_file_collective_target(Globals, DepStream, ModuleName,
        MakeVarName, Extension - VarExtension, !IO) :-
    module_name_to_file_name(Globals, ModuleName, Extension,
        do_not_create_dirs, TargetName, !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", TargetName, "\n",
        TargetName, " : $(", MakeVarName, VarExtension, ")\n\n"
    ], !IO).

:- pred generate_dep_file_clean_targets(globals::in, io.output_stream::in,
    module_name::in, string::in, string::in, string::in,
    string::in, string::in, string::in, string::in, string::in, string::in,
    string::in, string::in, io::di, io::uo) is det.

generate_dep_file_clean_targets(Globals, DepStream, ModuleName, MakeVarName,
        ExeFileName, InitCFileName,
        InitObjFileName, InitPicObjFileName, InitFileName, LibFileName,
        SharedLibFileName, JarFileName, DepFileName, DvFileName, !IO) :-
    % If you change the clean targets below, please also update the
    % documentation in doc/user_guide.texi.
    %
    % XXX The use of xargs in the clean targets doesn't handle special
    % characters in the file names correctly. This is currently not a problem
    % in practice as we never generate names containing special characters,
    % any fix for this problem will also require a fix in `mmake.in'.

    module_name_to_file_name(Globals, ModuleName, ".clean",
        do_not_create_dirs, CleanTargetName, !IO),
    io.write_strings(DepStream, [
        "clean_local : ", CleanTargetName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", CleanTargetName, "\n",
        CleanTargetName, " :\n",
        "\t-echo $(", MakeVarName, ".dirs) | xargs rm -rf \n",
        "\t-echo $(", MakeVarName, ".cs) ", InitCFileName,
            " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".mihs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_os) ", InitObjFileName,
            " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_pic_os) ",
            InitPicObjFileName, " | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".c_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".java_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".useds) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".javas) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".profs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".errs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".foreign_cs) | xargs rm -f\n",
        "\n"
    ], !IO),

    module_name_to_file_name(Globals, ModuleName, ".realclean",
        do_not_create_dirs, RealCleanTargetName, !IO),
    io.write_strings(DepStream, [
        "realclean_local : ", RealCleanTargetName, "\n"
    ], !IO),
    io.write_strings(DepStream, [
        ".PHONY : ", RealCleanTargetName, "\n",
        RealCleanTargetName, " : ", CleanTargetName, "\n",
        "\t-echo $(", MakeVarName, ".dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".date0s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".date3s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".optdates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".trans_opt_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".ints) | xargs rm -f\n",
        % XXX This should acutally be .int0s but we need to make sure that
        % we delete any spurious .int0 files created for nested sub-modules.
        % For further details see the XXX comments above.
        "\t-echo $(", MakeVarName, ".all_int0s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".int3s) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".opts) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".trans_opts) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".analysiss) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".requests) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".imdgs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".ds) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".module_deps) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_mhs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".all_mihs) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".dlls) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".foreign_dlls) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".classes) | xargs rm -f\n"
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
    ], !IO).

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

maybe_output_module_order(Globals, Module, DepsOrdering, !IO) :-
    globals.lookup_bool_option(Globals, generate_module_order, Order),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        Order = yes,
        module_name_to_file_name(Globals, Module, ".order",
            do_create_dirs, OrdFileName, !IO),
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

%-----------------------------------------------------------------------------%

referenced_dlls(Module, DepModules0) = Modules :-
    set.insert(Module, DepModules0, DepModules),

    % If we are not compiling a module in the mercury std library then
    % replace all the std library dlls with one reference to mercury.dll.
    ( if mercury_std_library_module_name(Module) then
        % In the standard library we need to add the runtime dlls.
        AddedModules =
            [unqualified("mercury_dotnet"), unqualified("mercury_il")],
        set.insert_list(AddedModules, DepModules, Modules)
    else
        F = (func(M) =
            ( if mercury_std_library_module_name(M) then
                unqualified("mercury")
            else
                % A sub module is located in the top level assembly.
                unqualified(outermost_qualifier(M))
            )
        ),
        Modules = set.map(F, DepModules)
    ).

%-----------------------------------------------------------------------------%

    % get_both_opt_deps(Globals, BuildOptFiles, Deps, IntermodDirs,
    %   OptDeps, TransOptDeps, !IO):
    %
    % For each dependency, search intermod_directories for a .m file.
    % If it exists, add it to both output lists. Otherwise, if a .opt
    % file exists, add it to the OptDeps list, and if a .trans_opt
    % file exists, add it to the TransOptDeps list.
    % If --use-opt-files is set, don't look for `.m' files, since we are
    % not building `.opt' files, only using those which are available.
    % XXX This won't find nested sub-modules.
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
        % ZZZ document io.seen effect
        search_for_module_source(Globals, IntermodDirs, IntermodDirs,
            Dep, Result1, !IO),
        (
            Result1 = ok(_),
            !:OptDeps = [Dep | !.OptDeps],
            !:TransOptDeps = [Dep | !.TransOptDeps],
            io.seen(!IO),
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
        module_name_to_file_name(Globals, Dep, ".opt",
            do_not_create_dirs, OptName, !IO),
        search_for_file_returning_dir(do_not_open_file, IntermodDirs,
            OptName, Result2, !IO),
        (
            Result2 = ok(_),
            !:OptDeps = [Dep | !.OptDeps]
        ;
            Result2 = error(_)
        ),
        module_name_to_file_name(Globals, Dep, ".trans_opt",
            do_not_create_dirs, TransOptName, !IO),
        search_for_file_returning_dir(do_not_open_file, IntermodDirs,
            TransOptName, Result3, !IO),
        (
            Result3 = ok(_),
            !:TransOptDeps = [Dep | !.TransOptDeps]
        ;
            Result3 = error(_)
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
            Found = yes,
            io.seen(!IO)
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
        module_name_to_search_file_name(Globals, Dep, Suffix, OptName, !IO),
        search_for_file(do_not_open_file, IntermodDirs, OptName, Result2, !IO),
        (
            Result2 = ok(_),
            !:OptDeps = [Dep | !.OptDeps]
        ;
            Result2 = error(_)
        )
    ;
        Found = yes
    ).

%-----------------------------------------------------------------------------%

:- pred compare_module_names(module_name::in, module_name::in,
    comparison_result::out) is det.

compare_module_names(Sym1, Sym2, Result) :-
    Str1 = sym_name_to_string(Sym1),
    Str2 = sym_name_to_string(Sym2),
    compare(Result, Str1, Str2).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.write_deps_file.
%-----------------------------------------------------------------------------%
