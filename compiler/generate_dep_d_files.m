%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: generate_dep_d_files.m.
% Original author: fjh (when this code was in modules.m)
%
% This module figures out the information from which write_deps_file.m
% creates dependency files (.dv, .dep and .d files) for mmake.
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

:- module parse_tree.generate_dep_d_files.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

    % generate_dep_file_for_module(ProgressStream, Globals, ModuleName,
    %   Specs, !IO):
    %
    % Generate the per-program makefile dependencies file (`.dep' file)
    % for a program whose top-level module is `ModuleName'. This involves
    % first transitively reading in all imported or ancestor modules.
    % While we are at it, we also save the per-module makefile dependency files
    % (`.d' files) for all those modules. Return any errors and/or warnings
    % to be printed in Specs.
    %
:- pred generate_dep_file_for_module(io.text_output_stream::in, globals::in,
    module_name::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

    % generate_dep_file_for_file(ProgressStream, Globals, FileName,
    %   Specs, !IO):
    %
    % Same as generate_dep_file_for_module, but takes a file name
    % instead of a module name.
    %
:- pred generate_dep_file_for_file(io.text_output_stream::in, globals::in,
    file_name::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

    % generate_d_file_for_module(ProgressStream, Globals, ModuleName,
    %   Specs, !IO):
    %
    % Generate the per-module makefile dependency file ('.d' file)
    % for the given module.
    %
:- pred generate_d_file_for_module(io.text_output_stream::in, globals::in,
    module_name::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

    % generate_d_file_for_file(ProgressStream, Globals, FileName, Specs, !IO):
    %
    % Same as generate_d_file_for_module, but takes a file name
    % instead of a module name.
    %
:- pred generate_d_file_for_file(io.text_output_stream::in, globals::in,
    file_name::in, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.mmakefiles.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.file_names.
:- import_module parse_tree.generate_mmakefile_fragments.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.opt_deps_spec.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.write_deps_file.

:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_dep_file_for_module(ProgressStream, Globals, ModuleName,
        DepsMap, Specs, !IO) :-
    map.init(DepsMap0),
    generate_dot_dx_files(ProgressStream, Globals,
        output_all_program_dot_dx_files, do_not_search,
        ModuleName, DepsMap0, DepsMap, Specs, !IO).

generate_dep_file_for_file(ProgressStream, Globals, FileName,
        DepsMap, Specs, !IO) :-
    build_initial_deps_map_for_file(ProgressStream, Globals, FileName,
        ModuleName, DepsMap0, InitialSpecs, !IO),
    generate_dot_dx_files(ProgressStream, Globals,
        output_all_program_dot_dx_files, do_not_search,
        ModuleName, DepsMap0, DepsMap, LaterSpecs, !IO),
    Specs = InitialSpecs ++ LaterSpecs.

generate_d_file_for_module(ProgressStream, Globals, ModuleName,
        DepsMap, Specs, !IO) :-
    map.init(DepsMap0),
    generate_dot_dx_files(ProgressStream, Globals, output_module_dot_d_file,
        do_search, ModuleName, DepsMap0, DepsMap, Specs, !IO).

generate_d_file_for_file(ProgressStream, Globals, FileName,
        DepsMap, Specs, !IO) :-
    build_initial_deps_map_for_file(ProgressStream, Globals, FileName,
        ModuleName, DepsMap0, InitialSpecs, !IO),
    generate_dot_dx_files(ProgressStream, Globals, output_module_dot_d_file,
        do_search, ModuleName, DepsMap0, DepsMap, LaterSpecs, !IO),
    Specs = InitialSpecs ++ LaterSpecs.

%---------------------------------------------------------------------------%

:- pred build_initial_deps_map_for_file(io.text_output_stream::in, globals::in,
    file_name::in, module_name::out, deps_map::out, list(error_spec)::out,
    io::di, io::uo) is det.

build_initial_deps_map_for_file(ProgressStream, Globals, FileName, ModuleName,
        DepsMap, Specs, !IO) :-
    % Read in the top-level file (to figure out its module name).
    FileNameDotM = FileName ++ ".m",
    read_module_src_from_file(ProgressStream, Globals, FileName, FileNameDotM,
        rrm_file, do_not_search, always_read_module(do_not_return_timestamp),
        HaveReadModuleSrc, !IO),
    (
        HaveReadModuleSrc = have_module(_FN, ParseTreeSrc, Source),
        Source = was_read(MaybeTimestamp, ReadModuleErrors),
        ParseTreeSrc = parse_tree_src(ModuleName, _, _),
        parse_tree_src_to_burdened_module_list(Globals, FileNameDotM,
            ReadModuleErrors, MaybeTimestamp, ParseTreeSrc,
            Specs, BurdenedModules)
    ;
        HaveReadModuleSrc = have_not_read_module(_, ReadModuleErrors),
        get_default_module_name_for_file(FileName, FileNameDotM,
            ModuleName, !IO),
        % XXX Caller should not need this info.
        Specs = get_read_module_specs(ReadModuleErrors),
        BurdenedModules = []
    ),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map(non_dummy_burdened_module),
        BurdenedModules, DepsMap0, DepsMap).

%---------------------------------------------------------------------------%

:- type which_dot_dx_files
    --->    output_module_dot_d_file
            % Output the given module's .d file.
    ;       output_all_program_dot_dx_files.
            % The given module is (or should be!) the main module of a program.
            % Output the program's .dep and .dv files, and the .d file
            % of every module in the program.

:- pred generate_dot_dx_files(io.text_output_stream::in, globals::in,
    which_dot_dx_files::in, maybe_search::in, module_name::in,
    deps_map::in, deps_map::out, list(error_spec)::out, io::di, io::uo) is det.

generate_dot_dx_files(ProgressStream, Globals, Mode, Search, ModuleName,
        DepsMap0, DepsMap, !:Specs, !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(ProgressStream, Globals, Search, ModuleName,
        ReadModules, UnreadModules, DepsMap0, DepsMap, [], !:Specs, !IO),
    warn_about_any_unread_modules_with_read_ancestors(ReadModules,
        UnreadModules, !Specs),

    % Check whether we could read the main `.m' file.
    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, _, BurdenedModule),
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
            % The error_specs in FatalErrorSpecs may already be in !.Specs,
            % but even if they are, they will be printed just once.
            !:Specs = FatalErrorSpecs ++ !.Specs
        )
    else
        (
            Mode = output_module_dot_d_file
        ;
            Mode = output_all_program_dot_dx_files,
            SourceFileName = Baggage ^ mb_source_file_name,

            map.init(Cache0),
            generate_dv_file(Globals, SourceFileName, ModuleName, DepsMap,
                MmakeFileDv, Cache0, _Cache, !IO),
            generate_dep_file(Globals, SourceFileName, ModuleName, DepsMap,
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
            )
        ),

        % Compute the interface deps graph and the implementation deps graph
        % from the deps map.
        digraph.init(IntDepsGraph0),
        digraph.init(ImpDepsGraph0),
        map.values(DepsMap, DepsList),
        deps_list_to_deps_graph(DepsMap, DepsList, BurdenedModules,
            IntDepsGraph0, IntDepsGraph, ImpDepsGraph0, ImpDepsGraph),
        maybe_output_imports_graph(ProgressStream, Globals, ModuleName,
            IntDepsGraph, ImpDepsGraph, !IO),

        trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
            io(!TIO)]
        (
            io.format(ProgressStream, "generate_dot_dx_files for %s\n",
                [s(sym_name_to_string(ModuleName))], !TIO),

            set_tree234.to_sorted_list(ReadModules, ReadModuleList),
            set_tree234.to_sorted_list(UnreadModules, UnreadModuleList),
            ReadStrs = list.map(sym_name_to_string, ReadModuleList),
            UnreadStrs = list.map(sym_name_to_string, UnreadModuleList),

            io.write_string(ProgressStream, "ReadModules\n", !TIO),
            io.write_line(ProgressStream, ReadStrs, !TIO),
            io.write_string(ProgressStream, "UnreadModules\n", !TIO),
            io.write_line(ProgressStream, UnreadStrs, !TIO),

            digraph.to_assoc_list(IntDepsGraph, IntDepsAL),
            io.write_string(ProgressStream, "IntDepsAL:\n", !TIO),
            list.foldl(io.write_line(ProgressStream), IntDepsAL, !TIO),

            digraph.to_assoc_list(ImpDepsGraph, ImpDepsAL),
            io.write_string(ProgressStream, "ImpDepsAL:\n", !TIO),
            list.foldl(io.write_line(ProgressStream), ImpDepsAL, !TIO)
        ),

        compute_opt_trans_opt_deps_graph(ProgressStream, Globals, ModuleName,
            ImpDepsGraph, IndirectOptDepsGraph,
            TransOptDepsGraph, TransOptDepsOrdering, !Specs, !IO),

        % Compute the indirect dependencies: they are equal to the composition
        % of the implementation dependencies with the transitive closure of the
        % implementation dependencies. (We used to take the transitive closure
        % of the interface dependencies, but we now include implementation
        % details in the interface files).
        digraph.tc(ImpDepsGraph, TransImpDepsGraph),
        digraph.compose(ImpDepsGraph, TransImpDepsGraph, IndirectDepsGraph),

        globals.lookup_accumulating_option(Globals, intermod_directories,
            IntermodDirs),
        get_ext_opt_deps(Globals, look_for_src, IntermodDirs,
            ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_opt_trans),
            TransOptDepsOrdering, TransOptOrder, !IO),
        (
            Mode = output_module_dot_d_file,
            DFilesToWrite = [BurdenedModule]
        ;
            Mode = output_all_program_dot_dx_files,
            DFilesToWrite = BurdenedModules
        ),
        generate_dependencies_write_d_files(ProgressStream, Globals,
            DFilesToWrite, IntDepsGraph, ImpDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptDepsGraph, TransOptOrder, !IO)
    ).

    % Construct a pair of dependency graphs (the interface dependencies
    % and the implementation dependencies) for all the modules in the program.
    %
:- pred deps_list_to_deps_graph(deps_map::in,
    list(deps)::in, list(burdened_module)::out,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

deps_list_to_deps_graph(_, [], [], !IntDepsGraph, !ImpDepsGraph).
deps_list_to_deps_graph(DepsMap,
        [Deps | DepsList], [BurdenedModule | BurdenedModules],
        !IntDepsGraph, !ImpDepsGraph) :-
    Deps = deps(_, _, BurdenedModule),
    Baggage = BurdenedModule ^ bm_baggage,
    Errors = Baggage ^ mb_errors,
    FatalErrors = Errors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        add_module_dep_info_to_deps_graph(ModuleDepInfo,
            lookup_burdened_module_in_deps_map(DepsMap),
            !IntDepsGraph, !ImpDepsGraph)
    else
        true
    ),
    deps_list_to_deps_graph(DepsMap, DepsList, BurdenedModules,
        !IntDepsGraph, !ImpDepsGraph).

:- func lookup_burdened_module_in_deps_map(deps_map, module_name)
    = module_dep_info.

lookup_burdened_module_in_deps_map(DepsMap, ModuleName) = ModuleDepInfo :-
    map.lookup(DepsMap, ModuleName, deps(_, _, BurdenedModule)),
    ModuleDepInfo = module_dep_info_full(BurdenedModule).

%---------------------------------------------------------------------------%

:- pred warn_about_any_unread_modules_with_read_ancestors(
    set_tree234(module_name)::in, set_tree234(module_name)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_about_any_unread_modules_with_read_ancestors(ReadModules, UnreadModules0,
        !Specs) :-
    % When module mod_a.mod_b is nested inside mod_a.m, the source file
    % containing module mod_a, then it is possible for an attempt to read
    % mod_a.mod_b.m to fail (since module mod_a.mod_b is not there),
    % but for the module to be later found by reading mod_a.m.
    % This would result in mod_a.mod_b being included in both
    % ReadModules and UnreadModules0.
    set_tree234.difference(UnreadModules0, ReadModules, UnreadModules),
    set_tree234.to_sorted_list(UnreadModules, UnreadModuleList),
    find_read_ancestors_of_unread_modules(ReadModules, UnreadModuleList,
        set_tree234.init, Parents, set_tree234.init, Ancestors, 
        set_tree234.init, BadUnreads),
    set_tree234.to_sorted_list(BadUnreads, BadUnreadList),
    (
        BadUnreadList = []
    ;
        BadUnreadList = [_ | _],
        BadUnreadModulePieces = list.map(wrap_module_name, BadUnreadList),
        list.intersperse(nl, BadUnreadModulePieces, BadUnreadModuleListPieces),
        TheModules = choose_number(BadUnreadList, "the module", "the modules"),
        BadUnreadPieces =
            [words(TheModules), nl_indent_delta(1), blank_line] ++
            BadUnreadModuleListPieces ++ [nl_indent_delta(-1), blank_line],

        set_tree234.to_sorted_list(Parents, ParentList),
        set_tree234.to_sorted_list(Ancestors, AncestorList),
        ParentModulePieces = list.map(wrap_module_name, ParentList),
        AncestorModulePieces = list.map(wrap_module_name, AncestorList),
        (
            ParentModulePieces = [],
            ParentPieces = []
        ;
            ParentModulePieces = [_ | TailParentModulePieces],
            (
                TailParentModulePieces = [],
                ParentWords = "a parent module,"
            ;
                TailParentModulePieces = [_ | _],
                ParentWords = "parent modules,"
            ),
            list.intersperse(nl, ParentModulePieces, ParentModuleListPieces),
            ParentPieces =
                [words(ParentWords), words("specifically"),
                    nl_indent_delta(1), blank_line] ++
                ParentModuleListPieces ++ [nl_indent_delta(-1), blank_line]
        ),
        (
            AncestorModulePieces = [],
            AncestorPieces = []
        ;
            AncestorModulePieces = [_ | TailAncestorModulePieces],
            (
                TailAncestorModulePieces = [],
                AncestorWords = "an ancestor module,"
            ;
                TailAncestorModulePieces = [_ | _],
                AncestorWords = "ancestor modules,"
            ),
            list.intersperse(nl, AncestorModulePieces,
                AncestorModuleListPieces),
            AncestorPieces0 =
                [words(AncestorWords), words("specifically"),
                    nl_indent_delta(1), blank_line] ++
                AncestorModuleListPieces ++ [nl_indent_delta(-1), blank_line],
            (
                ParentModulePieces = [],
                AncestorPieces = AncestorPieces0
            ;
                ParentModulePieces = [_ | _],
                AncestorPieces = [words("and") | AncestorPieces0]
            )
        ),
        Pieces =
            [words("Warning:")] ++ BadUnreadPieces ++
            [words("which the compiler could not find"),
            words("in the current directory,"), words("have")] ++
            ParentPieces ++ AncestorPieces ++
            [words("which the compiler *did* find in the current directory."),
            blank_line,
            words("This usually indicates that the Mercury.modules file,"),
            words("which contains the module name to source file name map,"),
            words("is either missing or out-of-date."),
            words("You need to rebuild it."),
            words("This is usually done using a command such as"),
            quote("mmc -f *.m"), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_warning, phase_read_files, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- func wrap_module_name(module_name) = format_piece.

wrap_module_name(ModuleName) = fixed(sym_name_to_string(ModuleName)).

:- pred find_read_ancestors_of_unread_modules(set_tree234(module_name)::in,
    list(module_name)::in,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out,
    set_tree234(module_name)::in, set_tree234(module_name)::out) is det.

find_read_ancestors_of_unread_modules(_ReadModules, [],
        !Parents, !Ancestors, !BadUnreads).
find_read_ancestors_of_unread_modules(ReadModules,
        [UnreadModule | UnreadModules],
        !Parents, !Ancestors, !BadUnreads) :-
    (
        UnreadModule = unqualified(_)
    ;
        UnreadModule = qualified(ParentModule, _),
        ( if
            find_first_read_ancestor(ReadModules, ParentModule, parent,
                AncestorModule, PoA)
        then
            set_tree234.insert(UnreadModule, !BadUnreads),
            (
                PoA = parent,
                set_tree234.insert(AncestorModule, !Parents)
            ;
                PoA = ancestor,
                set_tree234.insert(AncestorModule, !Ancestors)
            )
        else
            true
        )
    ),
    find_read_ancestors_of_unread_modules(ReadModules, UnreadModules,
        !Parents, !Ancestors, !BadUnreads).

:- type parent_or_ancestor
    --->    parent
    ;       ancestor.

:- pred find_first_read_ancestor(set_tree234(module_name)::in,
    module_name::in, parent_or_ancestor::in,
    module_name::out, parent_or_ancestor::out) is semidet.

find_first_read_ancestor(ReadModules, Module, CurPoA, AncestorModule, PoA) :-
    ( if set_tree234.contains(ReadModules, Module) then
        AncestorModule = Module,
        PoA = CurPoA
    else
        Module = qualified(ParentModule, _),
        find_first_read_ancestor(ReadModules, ParentModule, ancestor,
            AncestorModule, PoA)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_output_imports_graph(io.text_output_stream::in, globals::in,
    module_name::in, digraph(sym_name)::in, digraph(sym_name)::in,
    io::di, io::uo) is det.

maybe_output_imports_graph(ProgressStream, Globals, ModuleName,
        IntDepsGraph, ImpDepsGraph, !IO) :-
    globals.lookup_bool_option(Globals, imports_graph, ImportsGraph),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        ImportsGraph = yes,
        module_name_to_cur_dir_file_name(ext_cur_user_imports_graph,
            ModuleName, ImportsGraphFileName),
        (
            Verbose = no
        ;
            Verbose = yes,
            io.format(ProgressStream,
                "%% Creating imports graph file `%s'...",
                [s(ImportsGraphFileName)], !IO)
        ),
        io.open_output(ImportsGraphFileName, ImportsGraphOpenResult, !IO),
        (
            ImportsGraphOpenResult = ok(ImportsGraphStream),
            Deps0 = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(IntDepsGraph), digraph.init),
            Deps = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(ImpDepsGraph), Deps0),
            write_graph(ImportsGraphStream, "imports", Deps, !IO),
            io.close_output(ImportsGraphStream, !IO),
            (
                Verbose = no
            ;
                Verbose = yes,
                io.write_string(ProgressStream, " done.\n", !IO)
            )
        ;
            ImportsGraphOpenResult = error(IOError),
            (
                Verbose = no
            ;
                Verbose = yes,
                io.write_string(ProgressStream, " failed.\n", !IO),
                io.flush_output(ProgressStream, !IO)
            ),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening file `%s' for output: %s\n",
                [s(ImportsGraphFileName), s(IOErrorMessage)], ImpMessage),
            report_error(ProgressStream, ImpMessage, !IO)
        )
    ;
        ImportsGraph = no
    ).

:- func filter_imports_graph(pair(sym_name, sym_name), digraph(sym_name)) =
    digraph(sym_name).

filter_imports_graph(A - B, DepsGraph) =
    ( if
        % Don't keep the edge if it points to a builtin module,
        % or if the relationship is between two standard library modules.
        % XXX It would be better to change this to only keep those edges
        % for which the left-hand side is in the current directory.
        (
            any_mercury_builtin_module(B)
        ;
            is_std_lib_module_name(A, _),
            is_std_lib_module_name(B, _)
        )
    then
        DepsGraph
    else
        digraph.add_vertices_and_edge(A, B, DepsGraph)
    ).

:- pred write_graph(io.text_output_stream::in, string::in,
    digraph(module_name)::in, io::di, io::uo) is det.

write_graph(Stream, Name, Graph, !IO) :-
    io.write_string(Stream, "digraph " ++ Name ++ " {\n", !IO),
    io.write_string(Stream, "label=\"" ++ Name ++ "\";\n", !IO),
    io.write_string(Stream, "center=true;\n", !IO),
    digraph.traverse(Graph, write_node(Stream), write_edge(Stream), !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred write_node(io.text_output_stream::in, module_name::in,
    io::di, io::uo) is det.

write_node(Stream, Node, !IO) :-
    io.format(Stream, "%s;\n", [s(module_name_to_node_id(Node))], !IO).

:- pred write_edge(io.text_output_stream::in, module_name::in, module_name::in,
    io::di, io::uo) is det.

write_edge(Stream, A, B, !IO) :-
    io.format(Stream, "%s -> %s;\n",
        [s(module_name_to_node_id(A)), s(module_name_to_node_id(B))], !IO).

:- func module_name_to_node_id(module_name) = string.

module_name_to_node_id(ModuleName) =
    % Names can't contain "." so use "__"
    "\"" ++ sym_name_to_string_sep(ModuleName, "__") ++ "\"".

%---------------------------------------------------------------------------%
:- end_module parse_tree.generate_dep_d_files.
%---------------------------------------------------------------------------%
