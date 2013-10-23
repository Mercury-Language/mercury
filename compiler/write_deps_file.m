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

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.deps_map.
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
    % `.trans_opt' file may depend on.  This is set to `no' if the
    % dependency list is not available.
    %
:- pred write_dependency_file(globals::in, module_and_imports::in,
    set(module_name)::in, maybe(list(module_name))::in, io::di, io::uo) is det.

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

%-----------------------------------------------------------------------------%

    % Generate the list of .NET DLLs which could be referred to by this module
    % (including the module itself).
    %
    % If we are compiling a module within the standard library we should
    % reference the runtime DLLs and all other library DLLs.  If we are
    % outside the library we should just reference mercury.dll (which will
    % contain all the DLLs).
    %
:- func referenced_dlls(module_name, list(module_name)) = list(module_name).

%-----------------------------------------------------------------------------%

    % For each dependency, search intermod_directories for a .Suffix
    % file or a .m file, filtering out those for which the search fails.
    % If --use-opt-files is set, only look for `.opt' files,
    % not `.m' files.
    % XXX This won't find nested sub-modules.
    % XXX Use `mmc --make' if that matters.
    %
:- pred get_opt_deps(globals::in, bool::in, list(module_name)::in,
    list(string)::in, string::in, list(module_name)::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module make.                      % undesirable dependency
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_io.        % undesirable dependency
:- import_module parse_tree.prog_item.
:- import_module parse_tree.source_file_map.

:- import_module assoc_list.
:- import_module cord.
:- import_module dir.
:- import_module library.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

write_dependency_file(Globals, Module, AllDepsSet, MaybeTransOptDeps, !IO) :-
    Module = module_and_imports(SourceFileName, SourceFileModuleName,
        ModuleName, ParentDeps, IntDeps, ImplDeps, IndirectDeps,
        _Children, InclDeps, NestedDeps, FactDeps0,
        ContainsForeignCode, ForeignImports0, _ContainsForeignExport,
        Items, _Specs, _Error, _Timestamps, _HasMain, _Dir),

    globals.lookup_bool_option(Globals, verbose, Verbose),
    module_name_to_make_var_name(ModuleName, MakeVarName),
    module_name_to_file_name(Globals, ModuleName, ".d",
        do_create_dirs, DependencyFileName, !IO),
    module_name_to_file_name(Globals, ModuleName, ".trans_opt_date",
        do_not_create_dirs, TransOptDateFileName, !IO),

    % To avoid problems with concurrent updates of `.d' files during
    % parallel makes, we first create the file with a temporary name,
    % and then rename it to the desired name when we've finished.

    io.make_temp(dir.dirname(DependencyFileName), "tmp_d",
        TmpDependencyFileName, !IO),
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
        list.append(IntDeps, ImplDeps, LongDeps0),
        ShortDeps0 = IndirectDeps,
        set.list_to_set(LongDeps0, LongDepsSet0),
        set.delete(ModuleName, LongDepsSet0, LongDepsSet),
        set.list_to_set(ShortDeps0, ShortDepsSet0),
        set.difference(ShortDepsSet0, LongDepsSet, ShortDepsSet1),
        set.delete(ModuleName, ShortDepsSet1, ShortDepsSet),
        set.to_sorted_list(LongDepsSet, LongDeps),
        set.to_sorted_list(ShortDepsSet, ShortDeps),
        set.to_sorted_list(AllDepsSet, AllDeps),
        list.sort_and_remove_dups(FactDeps0, FactDeps),

        (
            MaybeTransOptDeps = yes(TransOptDeps0),
            set.list_to_set(TransOptDeps0, TransOptDepsSet0),
            set.intersect(TransOptDepsSet0, LongDepsSet, TransOptDepsSet),
            set.to_sorted_list(TransOptDepsSet, TransOptDateDeps),

            % Note that maybe_read_dependency_file searches for
            % this exact pattern.
            io.write_strings(DepStream, [TransOptDateFileName, " :"], !IO),
            write_dependencies_list(Globals, TransOptDateDeps, ".trans_opt",
                DepStream, !IO)
        ;
            MaybeTransOptDeps = no
        ),

        (
            FactDeps = [_ | _],
            io.write_strings(DepStream,
                ["\n\n", MakeVarName, ".fact_tables ="], !IO),
            write_file_dependencies_list(FactDeps, "", DepStream, !IO),
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
                write_fact_table_dependencies_list(Globals, ModuleName,
                    FactDeps, ".c", DepStream, !IO),
                io.write_strings(DepStream, ["\n\n", MakeVarName,
                    ".fact_tables.os ="], !IO),
                write_fact_table_dependencies_list(Globals, ModuleName,
                    FactDeps, ".$O", DepStream, !IO),
                io.nl(DepStream, !IO)
            )
        ;
            FactDeps = []
        ),

        ( string.remove_suffix(SourceFileName, ".m", SourceFileBase) ->
            ErrFileName = SourceFileBase ++ ".err"
        ;
            unexpected($module, $pred, "source file doesn't end in `.m'")
        ),
        module_name_to_file_name(Globals, ModuleName, ".optdate",
            do_not_create_dirs, OptDateFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".c_date",
            do_not_create_dirs, CDateFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".s_date",
            do_not_create_dirs, AsmDateFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".pic_s_date",
            do_not_create_dirs, PicAsmDateFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".$O",
            do_not_create_dirs, ObjFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".il_date",
            do_not_create_dirs, ILDateFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".java_date",
            do_not_create_dirs, JavaDateFileName, !IO),
        % XXX Why is the extension hardcoded to .pic_o here?  That looks
        % wrong.  It should probably be .$(EXT_FOR_PIC_OBJECT) - juliensf.
        module_name_to_file_name(Globals, ModuleName, ".pic_o",
            do_not_create_dirs, PicObjFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".int0",
            do_not_create_dirs, Int0FileName, !IO),
        io.write_strings(DepStream, ["\n\n",
            OptDateFileName, " ",
            TransOptDateFileName, " ",
            ErrFileName, " ",
            CDateFileName, " ",
            AsmDateFileName, " ",
            PicAsmDateFileName, " ",
            ILDateFileName, " ",
            JavaDateFileName
        ], !IO),
        io.write_strings(DepStream, [" : ", SourceFileName], !IO),
        % If the module contains nested sub-modules then the `.int0' file
        % must first be built.
        (
            InclDeps = [_ | _],
            io.write_strings(DepStream, [" ", Int0FileName], !IO)
        ;
            InclDeps = []
        ),
        write_dependencies_list(Globals, ParentDeps, ".int0", DepStream, !IO),
        write_dependencies_list(Globals, LongDeps, ".int", DepStream, !IO),
        write_dependencies_list(Globals, ShortDeps, ".int2", DepStream, !IO),

        NestedExts = [
            ".optdate",
            ".trans_opt_date",
            ".c_date",
            ".s_date",
            ".pic_s_date",
            ".dir/*.$O",
            ".il_date",
            ".java_date"],

        % If a module contains nested-submodules then we need to build
        % the nested children before attempting to build the parent module.
        (
            NestedDeps = []
        ;
            NestedDeps = [_ | _],
            Write = (pred(Ext::in, !.LIO::di, !:LIO::uo) is det :-
                module_name_to_file_name(Globals, ModuleName, Ext,
                    do_not_create_dirs, ExtName, !LIO),
                io.write_strings(DepStream, ["\n\n", ExtName, " : "], !LIO),
                write_dependencies_list(Globals, NestedDeps, Ext, DepStream,
                    !LIO)
            ),
            list.foldl(Write, NestedExts, !IO)
        ),
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
            write_dependencies_list(Globals, AllDeps, ".mh", DepStream, !IO)
        ;
            Intermod = no
        ),
        (
            ( Intermod = yes
            ; UseOptFiles = yes
            )
        ->
            io.write_strings(DepStream, [
                "\n\n",
                TransOptDateFileName, " ",
                ErrFileName, " ",
                CDateFileName, " ",
                AsmDateFileName, " ",
                PicAsmDateFileName, " ",
                ILDateFileName, " ",
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

            (
                ( TransOpt = yes
                ; UseTransOpt = yes
                )
            ->
                bool.not(UseTransOpt, BuildOptFiles),
                get_both_opt_deps(Globals, BuildOptFiles,
                    [ModuleName | LongDeps], IntermodDirs,
                    OptDeps, TransOptDeps, !IO),
                OptInt0Deps = sort_and_remove_dups(
                    condense(list.map(get_ancestors, OptDeps))),
                write_dependencies_list(Globals, OptDeps,
                    ".opt", DepStream, !IO),
                write_dependencies_list(Globals, OptInt0Deps,
                    ".int0", DepStream, !IO),

                io.write_strings(DepStream, [
                    "\n\n",
                    ErrFileName, " ",
                    CDateFileName, " ",
                    AsmDateFileName, " ",
                    PicAsmDateFileName, " ",
                    ILDateFileName, " ",
                    JavaDateFileName, " : "
                ], !IO),
                write_dependencies_list(Globals, TransOptDeps,
                    ".trans_opt", DepStream, !IO)
            ;
                bool.not(UseOptFiles, BuildOptFiles),
                get_opt_deps(Globals, BuildOptFiles, [ModuleName | LongDeps],
                    IntermodDirs, ".opt", OptDeps, !IO),
                OptInt0Deps = sort_and_remove_dups(
                    condense(list.map(get_ancestors, OptDeps))),
                write_dependencies_list(Globals, OptDeps,
                    ".opt", DepStream, !IO),
                write_dependencies_list(Globals, OptInt0Deps,
                    ".int0", DepStream, !IO)
            )
        ;
            true
        ),

        globals.lookup_bool_option(Globals, highlevel_code, HighLevelCode),
        globals.get_target(Globals, CompilationTarget),
        (
            HighLevelCode = yes,
            CompilationTarget = target_c
        ->
            % For --high-level-code with --target c, we need to make sure that
            % we generate the header files for imported modules before
            % compiling the C files, since the generated C files
            % #include those header files.

            io.write_strings(DepStream, [
                "\n\n",
                PicObjFileName, " ",
                ObjFileName, " :"
            ], !IO),
            write_dependencies_list(Globals, AllDeps, ".mih", DepStream, !IO)
        ;
            true
        ),

        % We need to tell make how to make the header files. The header files
        % are actually built by the same command that creates the .c or .s
        % file, so we just make them depend on the .c or .s files.
        % This is needed for the --high-level-code rule above, and for
        % the rules introduced for `:- pragma foreign_import_module'
        % declarations. In some grades the header file won't actually be built
        % (e.g. LLDS grades for modules not containing `:- pragma export'
        % declarations), but this rule won't do any harm.

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
        % creating the `.c', `.s', `.il', or `.java'.

        module_name_to_file_name(Globals, ModuleName, ".il",
            do_not_create_dirs, ILFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".java",
            do_not_create_dirs, JavaFileName, !IO),
        module_name_to_file_name(Globals, ModuleName,
            make_module_dep_file_extension, do_not_create_dirs,
            ModuleDepFileName, !IO),
        io.write_strings(DepStream, [
            "\n\n",
            "ifeq ($(findstring il,$(GRADE)),il)\n",
            ModuleDepFileName, " : ", ILFileName, "\n",
            "else\n",
            " ifeq ($(findstring java,$(GRADE)),java)\n",
            ModuleDepFileName, " : ", JavaFileName, "\n",
            " else\n",
            ModuleDepFileName, " : ", CFileName, "\n",
            " endif\n",
            "endif\n"
        ], !IO),

        % The .date and .date0 files depend on the .int0 files for the parent
        % modules, and the .int3 files for the directly and indirectly imported
        % modules.
        %
        % For nested sub-modules, the `.date' files for the parent modules
        % also depend on the same things as the `.date' files for this module,
        % since all the `.date' files will get produced by a single mmc
        % command. Similarly for `.date0' files, except these don't depend
        % on the `.int0' files, because when doing the
        % `--make-private-interface' for nested modules, mmc will process
        % the modules in outermost to innermost order so as to produce each
        % `.int0' file before it is needed.

        module_name_to_file_name(Globals, ModuleName, ".date",
            do_not_create_dirs, DateFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".date0",
            do_not_create_dirs, Date0FileName, !IO),
        io.write_strings(DepStream, [
            "\n\n", DateFileName, " ", Date0FileName
        ], !IO),
        write_dependencies_list(Globals, ParentDeps, ".date", DepStream, !IO),
        io.write_strings(DepStream, [" : ", SourceFileName], !IO),
        write_dependencies_list(Globals, ParentDeps, ".int0", DepStream, !IO),
        write_dependencies_list(Globals, LongDeps, ".int3", DepStream, !IO),
        write_dependencies_list(Globals, ShortDeps, ".int3", DepStream, !IO),

        io.write_strings(DepStream, ["\n\n", Date0FileName], !IO),
        write_dependencies_list(Globals, ParentDeps, ".date0", DepStream, !IO),
        io.write_strings(DepStream, [" : ", SourceFileName], !IO),
        write_dependencies_list(Globals, LongDeps, ".int3", DepStream, !IO),
        write_dependencies_list(Globals, ShortDeps, ".int3", DepStream, !IO),
        io.write_string(DepStream, "\n\n", !IO),

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

        globals.get_target(Globals, Target),
        globals.lookup_bool_option(Globals, il_sign_assembly, SignAssembly),

        % If we are on the IL backend, add the dependency that the
        % top level dll of a nested module hierachy depends on all
        % of it sub-modules dlls, as they are referenced from
        % inside the top level dll.
        % XXX Do we need to do the same for Java?

        module_name_to_file_name(Globals, ModuleName, ".dll",
            do_not_create_dirs, DllFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".class",
            do_not_create_dirs, ClassFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".beam",
            do_not_create_dirs, BeamFileName, !IO),
        SubModules = submodules(ModuleName, AllDeps),
        (
            Target = target_il,
            SubModules = [_ | _]
        ->
            io.write_strings(DepStream, [DllFileName, " : "], !IO),
            write_dll_dependencies_list(Globals, SubModules, "", DepStream,
                !IO),
            io.nl(DepStream, !IO)
        ;
            true
        ),

        (
            ContainsForeignCode = contains_foreign_code(LangSet),
            ForeignImports = ForeignImports0
        ;
            ContainsForeignCode = contains_foreign_code_unknown,
            get_item_list_foreign_code(Globals, cord.list(Items), LangSet,
                ForeignImports1, _),
            % If we're generating the `.dep' file, ForeignImports0 will contain
            % a conservative approximation to the set of foreign imports
            % needed which will include imports required by imported modules.
            (
                ForeignImports0 = [],
                ForeignImports = ForeignImports1
            ;
                ForeignImports0 = [_ | _],
                ForeignImports = ForeignImports0
            )
        ;
            ContainsForeignCode = contains_no_foreign_code,
            set.init(LangSet),
            ForeignImports = ForeignImports0
        ),

        % Handle dependencies introduced by
        % `:- pragma foreign_import_module' declarations.

        list.filter_map(
            (pred(ForeignImportMod::in, Import::out) is semidet :-
                Import = foreign_import_module_name_from_module(
                    ForeignImportMod, SourceFileModuleName),

                % XXX We can't include mercury.dll as mmake can't find it,
                % but we know that it exists.
                Import \= unqualified("mercury")
            ), ForeignImports, ForeignImportedModules),
        (
            ForeignImportedModules = []
        ;
            ForeignImportedModules = [_ | _],
            (
                Target = target_il,
                ForeignImportTargets = [DllFileName],
                ForeignImportExt = ".dll"
            ;
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
                % NOTE: for C (and asm) the possible targets might be a .o
                % file _or_ a .pic_o file.  We need to include dependencies
                % for the latter otherwise invoking mmake with a <module>.pic_o
                % target will break.
                ForeignImportTargets = [ObjFileName, PicObjFileName],
                ForeignImportExt = ".mh"
            ;
                % XXX These are just the C ones at the moment.
                Target = target_x86_64,
                ForeignImportTargets = [ObjFileName, PicObjFileName],
                ForeignImportExt = ".mh"
            ),
            WriteForeignImportTarget = (pred(ForeignImportTarget::in,
                    !.IO::di, !:IO::uo) is det :-
                io.write_string(DepStream, "\n\n", !IO),
                io.write_string(DepStream, ForeignImportTarget, !IO),
                io.write_string(DepStream, " : ", !IO),
                write_dependencies_list(Globals, ForeignImportedModules,
                    ForeignImportExt, DepStream, !IO),
                io.write_string(DepStream, "\n\n", !IO)
            ),
            list.foldl(WriteForeignImportTarget, ForeignImportTargets, !IO)
        ),

        (
            Target = target_il,
            not set.empty(LangSet)
        ->
            Langs = set.to_sorted_list(LangSet),
            list.foldl(write_foreign_dependency_for_il(Globals, DepStream,
                ModuleName, AllDeps, ForeignImports), Langs, !IO)
        ;
            true
        ),

        % If we are signing the assembly, then we will need the strong key
        % to sign the il file with so add a dependency that the il file
        % requires the strong name file `mercury.sn'. Also add the variable
        % ILASM_KEYFLAG-<module> which is used to build the command line
        % for ilasm.
        (
            Target = target_il,
            SignAssembly = yes
        ->
            module_name_to_make_var_name(ModuleName, ModuleNameString),
            module_name_to_file_name(Globals, ModuleName, ".il",
                do_not_create_dirs, IlFileName, !IO),

            io.write_strings(DepStream, [
                "ILASM_KEYFLAG-", ModuleNameString,
                    " = /keyf=mercury.sn\n",
                IlFileName, " : mercury.sn\n"], !IO)
        ;
            true
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

        % We add some extra dependencies to the generated `.d' files, so that
        % local `.int', `.opt', etc. files shadow the installed versions
        % properly (e.g. for when you're trying to build a new version
        % of an installed library). This saves the user from having to add
        % these explicitly if they have multiple libraries installed
        % in the same installation hierarchy which aren't independent (e.g.
        % one uses another). These extra dependencies are necessary due to
        % the way the combination of search paths and pattern rules
        % works in Make.
        %
        % Be very careful about changing the following rules. The `@:' is a
        % silent do-nothing command. It is used to force GNU Make to recheck
        % the timestamp on the target file.  (It is a pity that GNU Make
        % doesn't have a way of handling these sorts of rules in a
        % nicer manner.)

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
                write_subdirs_shorthand_rule(Globals, DepStream, ModuleName),
                [".c", ".$O", ".pic_o", ".s", ".pic_s",
                ".java", ".class", ".il", ".dll"], !IO)
        ;
            UseSubdirs = no
        ),

        ( SourceFileName \= default_source_file(ModuleName) ->
            % The pattern rules in Mmake.rules won't work, since the source
            % file name doesn't match the expected source file name for this
            % module name. This can occur due to just the use of different
            % source file names, or it can be due to the use of nested modules.
            % So we need to output hard-coded rules in this case.
            %
            % The rules output below won't work in the case of nested modules
            % with parallel makes, because it will end up invoking the same
            % command twice (since it produces two output files) at the same
            % time.
            %
            % Any changes here will require corresponding changes to
            % scripts/Mmake.rules. See that file for documentation
            % on these rules.

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
                ILDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--il-only ", ModuleArg,
                    " $(ERR_REDIRECT)\n",
                JavaDateFileName, " : ", SourceFileName, "\n",
                "\t$(MCG) $(ALL_GRADEFLAGS) $(ALL_MCGFLAGS) ",
                    "--java-only ", ModuleArg,
                    " $(ERR_REDIRECT)\n"
            ], !IO)
        ;
            true
        ),

        io.close_output(DepStream, !IO),
        io.rename_file(TmpDependencyFileName, DependencyFileName, Result3,
            !IO),
        (
            Result3 = error(_),
            % On some systems, we need to remove the existing file
            % first, if any.  So try again that way.
            io.remove_file(DependencyFileName, Result4, !IO),
            (
                Result4 = error(Error4),
                maybe_write_string(Verbose, " failed.\n", !IO),
                maybe_flush_output(Verbose, !IO),
                io.error_message(Error4, ErrorMsg),
                string.append_list(["can't remove file `", DependencyFileName,
                    "': ", ErrorMsg], Message),
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
                        TmpDependencyFileName, "' as `", DependencyFileName,
                        "': ", ErrorMsg], Message),
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
    ).

    % submodules(Module, Imports):
    %
    % Returns the list of submodules from Imports which are sub-modules of
    % Module, if Module is a top level module and not in the std library.
    % Otherwise it returns the empty list.
    %
:- func submodules(module_name, list(module_name)) = list(module_name).

submodules(Module, Modules0) = Modules :-
    (
        Module = unqualified(Str),
        \+ mercury_std_library_module_name(Module)
    ->
        P = (pred(M::in) is semidet :-
            Str = outermost_qualifier(M),
            M \= Module
        ),
        list.filter(P, Modules0, Modules)
    ;
        Modules = []
    ).

%-----------------------------------------------------------------------------%

:- pred write_dependencies_list(globals::in, list(module_name)::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_dependencies_list(_, [], _, _, !IO).
write_dependencies_list(Globals, [Module | Modules], Suffix, DepStream, !IO) :-
    module_name_to_file_name(Globals, Module, Suffix, do_not_create_dirs,
        FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_dependencies_list(Globals, Modules, Suffix, DepStream, !IO).

:- pred write_compact_dependencies_list(globals::in, list(module_name)::in,
    string::in, string::in, maybe(pair(string))::in, io.output_stream::in,
    io::di, io::uo) is det.

write_compact_dependencies_list(Globals, Modules, Prefix, Suffix, Basis,
        DepStream, !IO) :-
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
        write_dependencies_list(Globals, Modules, Suffix, DepStream, !IO)
    ).

:- pred write_compact_dependencies_separator(maybe(pair(string))::in,
    io.output_stream::in, io::di, io::uo) is det.

write_compact_dependencies_separator(no, _DepStream, !IO).
write_compact_dependencies_separator(yes(_), DepStream, !IO) :-
    io.write_string(DepStream, " ", !IO).

:- pred write_dll_dependencies_list(globals::in, list(module_name)::in,
    string::in, io.output_stream::in, io::di, io::uo) is det.

write_dll_dependencies_list(_Globals, [], _Prefix, _DepStream, !IO).
write_dll_dependencies_list(Globals, [Module | Modules], Prefix, DepStream,
        !IO) :-
    write_dll_dependency(Globals, Module, Prefix, DepStream, !IO),
    write_dll_dependencies_list(Globals, Modules, Prefix, DepStream, !IO).

:- pred write_dll_dependency(globals::in, module_name::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_dll_dependency(Globals, Module, Prefix, DepStream, !IO) :-
    module_name_to_file_name(Globals, Module, ".dll", do_not_create_dirs,
        FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, Prefix, !IO),
    io.write_string(DepStream, FileName, !IO).

:- pred write_fact_table_dependencies_list(globals::in, module_name::in,
    list(file_name)::in, string::in, io.output_stream::in,
    io::di, io::uo) is det.

write_fact_table_dependencies_list(_, _, [], _, _, !IO).
write_fact_table_dependencies_list(Globals, Module, [FactTable | FactTables],
        Suffix, DepStream, !IO) :-
    fact_table_file_name(Globals, Module, FactTable, Suffix,
        do_not_create_dirs, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_fact_table_dependencies_list(Globals, Module, FactTables, Suffix,
        DepStream, !IO).

:- pred write_extra_link_dependencies_list(globals::in,
    assoc_list(file_name, module_name)::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_extra_link_dependencies_list(_, [], _, _, !IO).
write_extra_link_dependencies_list(Globals, [ExtraLink - Module | ExtraLinks],
        Suffix, DepStream, !IO) :-
    extra_link_obj_file_name(Globals, Module, ExtraLink, Suffix,
        do_not_create_dirs, FileName, !IO),
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    write_extra_link_dependencies_list(Globals, ExtraLinks, Suffix, DepStream,
        !IO).

:- pred write_file_dependencies_list(list(string)::in, string::in,
    io.output_stream::in, io::di, io::uo) is det.

write_file_dependencies_list([], _, _, !IO).
write_file_dependencies_list([FileName | FileNames], Suffix, DepStream, !IO) :-
    io.write_string(DepStream, " \\\n\t", !IO),
    io.write_string(DepStream, FileName, !IO),
    io.write_string(DepStream, Suffix, !IO),
    write_file_dependencies_list(FileNames, Suffix, DepStream, !IO).

    % Generate the following dependency.  This dependency is
    % needed because module__cpp_code.dll might refer to
    % high level data in any of the mercury modules it
    % imports plus itself.
    % We also generate a dependency on the .il file, so that mmake
    % knows we need to generate the .il file to get the foreign language
    % source file (e.g. .cpp file).
    %
    % For example, for MC++ we generate:
    %
    %   <module>__cpp_code.dll : <module>.dll <imports>.dll
    %   <module>__cpp_code.cpp : <module>.il
    %
    % (the rule to generate .dll from .cpp is a pattern rule in
    % scripts/Mmake.rules).
    %
:- pred write_foreign_dependency_for_il(globals::in, io.output_stream::in,
    sym_name::in, list(module_name)::in, foreign_import_module_info_list::in,
    foreign_language::in, io::di, io::uo) is det.

write_foreign_dependency_for_il(Globals, DepStream, ModuleName, AllDeps,
        ForeignImports, ForeignLang, !IO) :-
    (
        ForeignModuleName = foreign_language_module_name(ModuleName,
            ForeignLang),
        ForeignExt = foreign_language_file_extension(ForeignLang)
    ->
        module_name_to_make_var_name(ForeignModuleName,
            ForeignModuleNameString),
        module_name_to_file_name(Globals, ForeignModuleName, ForeignExt,
            do_not_create_dirs, ForeignFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".il",
            do_not_create_dirs, IlFileName, !IO),
        module_name_to_file_name(Globals, ModuleName, ".dll",
            do_not_create_dirs, DllFileName, !IO),
        module_name_to_file_name(Globals, ForeignModuleName, ".dll",
            do_not_create_dirs, ForeignDllFileName, !IO),

        io.write_strings(DepStream,
            [ForeignDllFileName, " : ", DllFileName], !IO),
        % XXX This change doesn't work correctly because mmake can't find
        % the dlls which don't reside in the current directory.
        % write_dll_dependencies_list(ModuleName, AllDeps, DepStream, !IO),
        io.nl(DepStream, !IO),

        io.write_strings(DepStream,
            [ForeignFileName, " : ", IlFileName, "\n\n"], !IO),

        (
            ForeignLang = lang_csharp,
            % Store in the variable CSHARP_ASSEMBLY_REFS-foreign_code_name
            % the command line argument to reference all the dlls the
            % foreign code module references.
            io.write_strings(DepStream,
                ["CSHARP_ASSEMBLY_REFS-", ForeignModuleNameString, "="], !IO),
            ( mercury_std_library_module_name(ModuleName) ->
                Prefix = "/addmodule:"
            ;
                Prefix = "/r:"
            ),
            ForeignDeps = list.map(
                (func(M) = foreign_import_module_name_from_module(M,
                    ModuleName)),
                ForeignImports),
            Deps = AllDeps ++ ForeignDeps,
            write_dll_dependencies_list(Globals,
                referenced_dlls(ModuleName, Deps), Prefix, DepStream, !IO),
            io.nl(DepStream, !IO)
        ;
            ( ForeignLang = lang_c
            ; ForeignLang = lang_java
            ; ForeignLang = lang_il
            ; ForeignLang = lang_erlang
            )
        )
    ;
        % This foreign language doesn't generate an external file
        % so there are no dependencies to generate.
        true
    ).

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
    write_file_dependencies_list(SourceFiles, ".m", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".errs =", !IO),
    write_file_dependencies_list(SourceFiles, ".err", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mods =", !IO),
    write_dependencies_list(Globals, Modules, "", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The modules for which we need to generate .int0 files.
    ModulesWithSubModules = list.filter(
      (pred(Module::in) is semidet :-
          map.lookup(DepsMap, Module, deps(_, ModuleImports)),
          ModuleImports ^ mai_children = [_ | _]
      ), Modules),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".parent_mods =", !IO),
    write_dependencies_list(Globals, ModulesWithSubModules, "", DepStream,
        !IO),
    io.write_string(DepStream, "\n", !IO),

    globals.get_target(Globals, Target),
    (
        Target = target_il,
        ForeignModulesAndExts = foreign_modules(Modules, DepsMap)
    ;
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_x86_64
        ; Target = target_erlang
        ),
        ForeignModulesAndExts = []
    ),
    ForeignModules = assoc_list.keys(ForeignModulesAndExts),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign =", !IO),
    write_dependencies_list(Globals, ForeignModules, "", DepStream, !IO),
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
    write_file_dependencies_list(ForeignFileNames, "", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The dlls that contain the foreign_code.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".foreign_dlls = ", !IO),
    write_compact_dependencies_list(Globals, ForeignModules,
        "$(dlls_subdir)", ".dll", ForeignBasis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".init_cs = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(cs_subdir)", ".c", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".cs = $(", !IO),
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".init_cs) ", !IO),
    write_extra_link_dependencies_list(Globals, ExtraLinkObjs,
        ".c", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dlls = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(dlls_subdir)", ".dll", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_os = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(os_subdir)", ".$O", Basis, DepStream, !IO),
    write_extra_link_dependencies_list(Globals, ExtraLinkObjs,
        ".$O", DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_pic_os = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(os_subdir)", ".$(EXT_FOR_PIC_OBJECTS)", Basis, DepStream, !IO),
    write_extra_link_dependencies_list(Globals, ExtraLinkObjs,
        ".$(EXT_FOR_PIC_OBJECTS)", DepStream, !IO),
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
    write_compact_dependencies_list(Globals, Modules,
        "$(useds_subdir)", ".used", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ils = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(ils_subdir)", ".il", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".javas = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(javas_subdir)", ".java", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".classes = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(classes_subdir)", ".class", Basis, DepStream, !IO),
    io.write_string(DepStream, " ", !IO),
    % The Java compiler creates a .class file for each class
    % within the original .java file.  The filenames of all
    % these can be matched with `module\$*.class', hence the
    % "\\$$*.class" below.
    % If no such files exist, Make will use the pattern verbatim,
    % so we enclose the pattern in a `wildcard' function to prevent this.
    % XXX This relies on GNU Make.
    io.write_string(DepStream, "$(wildcard ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(classes_subdir)", "\\$$*.class", Basis, DepStream, !IO),
    io.write_string(DepStream, ")\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dirs = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(dirs_subdir)", ".dir", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dir_os = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(dirs_subdir)", ".dir/*.$O", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".dates = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(dates_subdir)", ".date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".date0s = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(date0s_subdir)", ".date0", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".date3s = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(date3s_subdir)", ".date3", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".optdates = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(optdates_subdir)", ".optdate", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".trans_opt_dates = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(trans_opt_dates_subdir)", ".trans_opt_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".c_dates = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(c_dates_subdir)", ".c_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".il_dates = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(il_dates_subdir)", ".il_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".java_dates = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(java_dates_subdir)", ".java_date", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ds = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(ds_subdir)", ".d", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".module_deps = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(module_deps_subdir)", make_module_dep_file_extension,
        Basis, DepStream, !IO),
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
            write_compact_dependencies_list(Globals, Modules,
                "$(mihs_subdir)", ".mih", Basis, DepStream, !IO)
        ;
            % For the IL and Java targets, currently we don't generate
            % `.mih' files at all; although perhaps we should...
            ( Target = target_il
            ; Target = target_csharp
            ; Target = target_java
            ; Target = target_erlang
            )
        ;
            Target = target_x86_64,
            unexpected($module, $pred, "--highlevel-code with --target x86_64")
        )
    ;
        % For the LLDS back-end, we don't use `.mih' files at all
        HighLevelCode = no
    ),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".mhs = ", !IO),
    (
        ( Target = target_c
        ; Target = target_x86_64
        ),
        write_compact_dependencies_list(Globals, Modules, "", ".mh", Basis,
            DepStream, !IO)
    ;
        ( Target = target_il
        ; Target = target_csharp
        ; Target = target_java
        ; Target = target_erlang
        )
    ),
    io.write_string(DepStream, "\n", !IO),

    % The `<module>.all_mihs' variable is like `<module>.mihs' except
    % that it contains header files for all the modules, regardless
    % of the grade or --target option.  It is used by the rule for
    % `mmake realclean', which should remove anything that could have
    % been automatically generated, even if the grade or --target option
    % has changed.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_mihs = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(mihs_subdir)", ".mih", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % The `<module>.all_mhs' variable is like `<module>.mhs' except
    % that it contains header files for all the modules, as for
    % `<module>.all_mihs' above.
    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_mhs = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "", ".mh", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".ints = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(ints_subdir)", ".int", Basis, DepStream, !IO),
    write_compact_dependencies_separator(Basis, DepStream, !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(int2s_subdir)", ".int2", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % `.int0' files are only generated for modules with sub-modules.
    % XXX ... or at least they should be.  Currently we end up generating
    % .int0 files for nested submodules that don't have any children.
    % (We do the correct thing for separate sub-modules.)

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".int0s = ", !IO),
    write_compact_dependencies_list(Globals, ModulesWithSubModules,
        "$(int0s_subdir)", ".int0", ParentBasis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    % XXX The `<module>.all_int0s' variables is like `<module>.int0s' except
    % that it contains .int0 files for all modules, regardless of whether
    % they should have been created or not.  It is used by the rule for
    % `mmake realclean' to ensure that we clean up all the .int0 files,
    % including the ones that were accidently created by the bug described
    % above.

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".all_int0s = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(int0s_subdir)", ".int0", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".int3s = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(int3s_subdir)", ".int3", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".opts = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(opts_subdir)", ".opt", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".trans_opts = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(trans_opts_subdir)", ".trans_opt", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".analysiss = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(analysiss_subdir)", ".analysis", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".requests = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(requests_subdir)", ".request", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".imdgs = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "$(imdgs_subdir)", ".imdg", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n", !IO),

    io.write_string(DepStream, MakeVarName, !IO),
    io.write_string(DepStream, ".profs = ", !IO),
    write_compact_dependencies_list(Globals, Modules,
        "", ".prof", Basis, DepStream, !IO),
    io.write_string(DepStream, "\n\n", !IO).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(module_name)::in, deps_map::in,
    list(module_name)::out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
    select_ok_modules(Modules0, DepsMap, ModulesTail),
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    module_and_imports_get_results(ModuleImports, _Items, _Specs, Error),
    (
        Error = fatal_module_errors,
        Modules = ModulesTail
    ;
        ( Error = no_module_errors
        ; Error = some_module_errors
        ),
        Modules = [Module | ModulesTail]
    ).

%-----------------------------------------------------------------------------%

    % Find out which modules will generate as external foreign language files.
    % We return the module names and file extensions.
    %
:- func foreign_modules(list(module_name), deps_map)
    = assoc_list(module_name, string).

foreign_modules(Modules, DepsMap) = ForeignModules :-
    P = (pred(M::in, FMs::out) is semidet :-
            module_has_foreign(DepsMap, M, LangList),
            FMs = list.filter_map((func(L) = (NewM - Ext) is semidet :-
                NewM = foreign_language_module_name(M, L),
                Ext = foreign_language_file_extension(L)
            ), LangList
        )
    ),
    list.filter_map(P, Modules, ForeignModulesList),
    ForeignModules = list.condense(ForeignModulesList).

    % Succeed iff we need to generate a foreign language output file
    % for the specified module.
    %
:- pred module_has_foreign(deps_map::in, module_name::in,
    list(foreign_language)::out) is semidet.

module_has_foreign(DepsMap, Module, LangList) :-
    map.lookup(DepsMap, Module, deps(_, ModuleImports)),
    ModuleImports ^ mai_has_foreign_code = contains_foreign_code(Langs),
    LangList = set.to_sorted_list(Langs).

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

    IfIL = ["ifeq ($(findstring il,$(GRADE)),il)\n"],
    ILMainRule = [ExeFileName, " : ", ExeFileName, ".exe\n",
        ExeFileName, ".exe : ", "$(", MakeVarName, ".dlls) ",
        "$(", MakeVarName, ".foreign_dlls)\n"],
    IfJava2 = [" ifeq ($(findstring java,$(GRADE)),java)\n"],
    JavaMainRule = [ExeFileName, " : $(", MakeVarName, ".classes)\n"],

    Else = ["else\n"],
    Else2 = [" else\n"],
    EndIf = ["endif\n"],
    EndIf2 = [" endif\n"],

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
        Rules = IfIL ++ ILMainRule ++ Else ++
            IfJava2 ++ JavaMainRule ++ Else2 ++
            MainRule ++ EndIf2 ++ EndIf
    ;
        Gmake = no,
        (
            Target = target_il,
            Rules = ILMainRule
        ;
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
            ( Target = target_c
            ; Target = target_x86_64    % XXX this is only provisional.
            ),
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
    ILLibRule = [
        LibTargetName, " : ", "$(", MakeVarName, ".dlls) ",
            "$(", MakeVarName, ".foreign_dlls) \\\n\t\t"
        | AllInts
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
        LibRules = IfIL ++ ILLibRule ++ Else ++
            IfJava2 ++ JavaLibRule ++ Else2 ++
            LibRule ++ EndIf2 ++ EndIf
    ;
        Gmake = no,
        (
            Target = target_il,
            LibRules = ILLibRule
        ;
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
            ( Target = target_c
            ; Target = target_x86_64    % XXX This is only provisional.
            ),
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
    % so that Mmake will find them.  That's not ideal, but it works.

    module_name_to_lib_file_name(Globals, "lib", ModuleName, ".install_ints",
        do_not_create_dirs, LibInstallIntsTargetName, !IO),
    (
        Intermod = yes,
        OptStr = " opt"
    ;
        Intermod = no,
        OptStr = ""
    ),
    (
        Intermod = yes,
        map.member(DepsMap, _, deps(_, Imports)),
        Imports ^ mai_children = [_ | _]
    ->
        % The `.int0' files only need to be installed with
        % `--intermodule-optimization'.
        Int0Str = " int0",
        MaybeInt0sVar = "$(" ++ MakeVarName ++ ".int0s) "
    ;
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
    (
        Intermod = no,
        TransOpt = no
    ->
        io.write_string(DepStream, "\n\t@:\n\n", !IO)
    ;
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
    % so that Mmake will find them.  That's not ideal, but it works.
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
            ".ils" - ".ils",
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
        "\t-echo $(", MakeVarName, ".il_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".java_dates) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".useds) | xargs rm -f\n",
        "\t-echo $(", MakeVarName, ".ils) | xargs rm -f\n",
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
    Deps = deps(_, ModuleImports),
    module_and_imports_get_source_file_name(ModuleImports, SourceFileName),
    ( string.remove_suffix(SourceFileName, ".m", SourceFileBase) ->
        FileName = SourceFileBase
    ;
        unexpected($module, $pred, "source file name doesn't end in `.m'")
    ).

%-----------------------------------------------------------------------------%

referenced_dlls(Module, DepModules0) = Modules :-
    DepModules = [Module | DepModules0],

    % If we are not compiling a module in the mercury std library then
    % replace all the std library dlls with one reference to mercury.dll.
    ( mercury_std_library_module_name(Module) ->
        % In the standard library we need to add the runtime dlls.
        Modules = list.remove_dups(
            [unqualified("mercury_dotnet"), unqualified("mercury_il")
                | DepModules])
    ;
        F = (func(M) =
            ( mercury_std_library_module_name(M) ->
                unqualified("mercury")
            ;
                % A sub module is located in the top level assembly.
                unqualified(outermost_qualifier(M))
            )
        ),
        Modules = list.remove_dups(list.map(F, DepModules))
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
:- pred get_both_opt_deps(globals::in, bool::in, list(module_name)::in,
    list(string)::in, list(module_name)::out, list(module_name)::out,
    io::di, io::uo) is det.

get_both_opt_deps(_, _, [], _, [], [], !IO).
get_both_opt_deps(Globals, BuildOptFiles, [Dep | Deps], IntermodDirs,
        !:OptDeps, !:TransOptDeps, !IO) :-
    get_both_opt_deps(Globals, BuildOptFiles, Deps, IntermodDirs,
        !:OptDeps, !:TransOptDeps, !IO),
    (
        BuildOptFiles = yes,
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

get_opt_deps(_, _, [], _, _, [], !IO).
get_opt_deps(Globals, BuildOptFiles, [Dep | Deps], IntermodDirs, Suffix,
        !:OptDeps, !IO) :-
    get_opt_deps(Globals, BuildOptFiles, Deps, IntermodDirs, Suffix,
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
