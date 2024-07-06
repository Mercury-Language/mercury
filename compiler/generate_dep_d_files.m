%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2017, 2019, 2020-2023 The Mercury team.
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

:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.write_deps_file.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module mercury_term_parser.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

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
        rrm_file, do_not_search, always_read_module(dont_return_timestamp),
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
            unexpected($pred, "FatalErrorSpecs = []")
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
            generate_dependencies_write_dv_file(ProgressStream,
                Globals, SourceFileName, ModuleName, DepsMap, !IO),
            generate_dependencies_write_dep_file(ProgressStream,
                Globals, SourceFileName, ModuleName, DepsMap, !IO),

            % For Java, the main target is actually a shell script
            % which will set CLASSPATH appropriately, and then invoke java
            % on the appropriate .class file. Rather than generating
            % an Mmake rule to build this file when it is needed,
            % we just generate this file at "mmake depend" time, since
            % that is simpler and probably more efficient anyway.
            globals.get_target(Globals, Target),
            ( if Target = target_java then
                create_java_shell_script(ProgressStream, Globals, ModuleName,
                    _Succeeded, !IO)
            else
                true
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

        globals.lookup_bool_option(Globals, generate_module_order,
            OutputOrder),
        (
            OutputOrder = yes,
            ImpDepsOrdering =
                digraph.return_sccs_in_from_to_order(ImpDepsGraph),
            output_module_order(ProgressStream, Globals, ModuleName,
                ext_cur(ext_cur_user_order), ImpDepsOrdering, !IO)
        ;
            OutputOrder = no
        ),

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

        % Compute the indirect dependencies: they are equal to the composition
        % of the implementation dependencies with the transitive closure of the
        % implementation dependencies. (We used to take the transitive closure
        % of the interface dependencies, but we now include implementation
        % details in the interface files).
        digraph.tc(ImpDepsGraph, TransImpDepsGraph),
        digraph.compose(ImpDepsGraph, TransImpDepsGraph, IndirectDepsGraph),

        % Compute the indirect optimization dependencies: indirect
        % dependencies including those via `.opt' or `.trans_opt' files.
        % Actually we cannot compute that, since we don't know
        % which modules the `.opt' files will import!
        % Instead, we need to make a conservative (over-)approximation,
        % and assume that the each module's `.opt' file might import any
        % of that module's implementation dependencies; in actual fact,
        % it will be some subset of that.
        digraph.tc(ImpDepsGraph, IndirectOptDepsGraph),

        % Compute the trans-opt deps for the purpose of making `.trans_opt'
        % files. This is normally equal to the transitive closure of the
        % indirect dependencies (i.e. IndirectOptDepsGraph) since a module
        % may read the `.trans_opt' file of any directly or indirectly
        % imported module.
        %
        % To deal with cycles in the graph, by default, we impose an arbitrary
        % order on modules so that when making the .trans_opt file for a module
        % "earlier" in the cycle, the compiler may read the .trans_opt files
        % of modules "later" in the cycle, but not vice versa.
        %
        % This has two problems.
        %
        % - Lack of parallelism. The .trans_opt files for modules within a
        %   single SCC have to be made one after another.
        %
        % - The arbitrary ordering is likely to produce sub-optimal
        %   information transfer between .trans_opt files.
        %
        % To help the user fix both problems at least partially,
        % we allow them to specify a list of edges (in a file read in by
        % read_trans_opt_deps_spec) that the code of apply_trans_opt_deps_spec
        % will then remove from the dependency graph. The intention is that
        % this should allow users to break up SCCs in a manner of their
        % choosing.
        %
        % Note that if the removal of the edges specified by the user
        % does not convert the graph into a dag (directed acyclic graph),
        % the compiler will use the default algorithm described above
        % to finish the job.
        globals.lookup_maybe_string_option(Globals, trans_opt_deps_spec,
            MaybeSpecFileName),
        (
            MaybeSpecFileName = yes(SpecFileName),
            read_trans_opt_deps_spec_file(SpecFileName, MaybeEdgesToRemove,
                !IO),
            (
                MaybeEdgesToRemove = ok1(EdgesToRemove),
                report_unknown_module_names_in_deps_spec(ImpDepsGraph,
                    EdgesToRemove, UnknownModuleSpecs),
                !:Specs = UnknownModuleSpecs ++ !.Specs,
                apply_trans_opt_deps_spec(EdgesToRemove, ImpDepsGraph,
                    TransOptDepsGraph0),
                digraph.tc(TransOptDepsGraph0, TransOptDepsGraph)
            ;
                MaybeEdgesToRemove = error1(EdgeSpecs),
                !:Specs = EdgeSpecs ++ !.Specs,
                TransOptDepsGraph = IndirectOptDepsGraph
            )
        ;
            MaybeSpecFileName = no,
            TransOptDepsGraph = IndirectOptDepsGraph
        ),
        TransOptDepsOrdering0 =
            digraph.return_sccs_in_from_to_order(TransOptDepsGraph),
        (
            OutputOrder = yes,
            output_module_order(ProgressStream, Globals, ModuleName,
                ext_cur(ext_cur_user_order_to), TransOptDepsOrdering0, !IO)
        ;
            OutputOrder = no
        ),
        list.map(set.to_sorted_list, TransOptDepsOrdering0,
            TransOptDepsOrdering1),
        list.condense(TransOptDepsOrdering1, TransOptDepsOrdering),
        globals.lookup_accumulating_option(Globals, intermod_directories,
            IntermodDirs),
        get_opt_deps(Globals, yes, IntermodDirs,
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
        module_name_to_file_name_create_dirs(Globals, $pred,
            ext_cur(ext_cur_user_imports_graph), ModuleName, FileName, !IO),
        (
            Verbose = no
        ;
            Verbose = yes,
            io.format(ProgressStream,
                "%% Creating imports graph file `%s'...",
                [s(FileName)], !IO)
        ),
        io.open_output(FileName, ImpResult, !IO),
        (
            ImpResult = ok(ImpStream),
            Deps0 = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(IntDepsGraph), digraph.init),
            Deps = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(ImpDepsGraph), Deps0),
            write_graph(ImpStream, "imports", sym_name_to_node_id, Deps, !IO),
            io.close_output(ImpStream, !IO),
            (
                Verbose = no
            ;
                Verbose = yes,
                io.write_string(ProgressStream, " done.\n", !IO)
            )
        ;
            ImpResult = error(IOError),
            (
                Verbose = no
            ;
                Verbose = yes,
                io.write_string(ProgressStream, " failed.\n", !IO),
                io.flush_output(ProgressStream, !IO)
            ),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening file `%s' for output: %s\n",
                [s(FileName), s(IOErrorMessage)], ImpMessage),
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

    % XXX What is the point of the arguments of this type
    % in the predicates below? They could, and I (zs) think they should,
    % be deleted, with all references replaced by the only value of this type
    % that we ever use, sym_name_to_node_id.
:- type gen_node_name(T) == (func(T) = string).

:- pred write_graph(io.text_output_stream::in, string::in,
    gen_node_name(T)::in, digraph(T)::in, io::di, io::uo) is det.

write_graph(Stream, Name, GenNodeName, Graph, !IO) :-
    io.write_string(Stream, "digraph " ++ Name ++ " {\n", !IO),
    io.write_string(Stream, "label=\"" ++ Name ++ "\";\n", !IO),
    io.write_string(Stream, "center=true;\n", !IO),
    digraph.traverse(Graph, write_node(Stream, GenNodeName),
        write_edge(Stream, GenNodeName), !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred write_node(io.text_output_stream::in,
    gen_node_name(T)::in, T::in, io::di, io::uo) is det.

write_node(Stream, GenNodeName, Node, !IO) :-
    io.format(Stream, "%s;\n", [s(GenNodeName(Node))], !IO).

:- pred write_edge(io.text_output_stream::in, gen_node_name(T)::in,
    T::in, T::in, io::di, io::uo) is det.

write_edge(Stream, GenNodeName, A, B, !IO) :-
    io.format(Stream, "%s -> %s;\n",
        [s(GenNodeName(A)), s(GenNodeName(B))], !IO).

:- func sym_name_to_node_id(sym_name) = string.

sym_name_to_node_id(SymName) =
    % Names can't contain "." so use "__"
    % XXX But sym_name_to_string DOES use "." to separate SymName's components.
    "\"" ++ sym_name_to_string(SymName) ++ "\"".

%---------------------------------------------------------------------------%

:- pred output_module_order(io.text_output_stream::in, globals::in,
    module_name::in, ext::in, list(set(module_name))::in,
    io::di, io::uo) is det.

output_module_order(ProgressStream, Globals, ModuleName, Ext,
        DepsOrdering, !IO) :-
    module_name_to_file_name_create_dirs(Globals, $pred, Ext,
        ModuleName, OrdFileName, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    string.format("%% Creating module order file `%s'...",
        [s(OrdFileName)], CreatingMsg),
    maybe_write_string(ProgressStream, Verbose, CreatingMsg, !IO),
    io.open_output(OrdFileName, OrdResult, !IO),
    (
        OrdResult = ok(OrdStream),
        io.write_list(OrdStream, DepsOrdering, "\n\n",
            write_module_scc(OrdStream), !IO),
        io.close_output(OrdStream, !IO),
        maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
    ;
        OrdResult = error(IOError),
        maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        io.error_message(IOError, IOErrorMessage),
        string.format("error opening file `%s' for output: %s",
            [s(OrdFileName), s(IOErrorMessage)], OrdMessage),
        report_error(ProgressStream, OrdMessage, !IO)
    ).

:- pred write_module_scc(io.text_output_stream::in, set(module_name)::in,
    io::di, io::uo) is det.

write_module_scc(Stream, SCC0, !IO) :-
    set.to_sorted_list(SCC0, SCC),
    % XXX This is suboptimal (the stream should be specified once, not twice),
    % but in the absence of a test case, I (zs) am leaving it alone for now.
    io.write_list(Stream, SCC, "\n", write_escaped_sym_name(Stream), !IO).

%---------------------------------------------------------------------------%

:- type trans_opt_deps_spec
    ==  map(module_name, allow_or_disallow_trans_opt_deps).

    % The contexts, and the order of the module names in the second arguments
    % of the module_{allow,disallow}_deps, are needed only for generating
    % meaningful error messages.
:- type allow_or_disallow_trans_opt_deps
    --->    module_allow_deps(
                % The context of the first argument.
                term_context,
                % The module names listed in the second argument, and their
                % contexts.
                assoc_list(term_context, module_name)
            )
    ;       module_disallow_deps(
                % The context of the first argument.
                term_context,
                % The module names listed in the second argument, and their
                % contexts.
                assoc_list(term_context, module_name)
            ).

    % The --trans-opt-deps-spec file shall contain a series of terms
    % of either form:
    %
    %   module_allow_deps(M, [ALLOW]).
    %   module_disallow_deps(M, [DISALLOW]).
    %
    % where M is a Mercury module name,
    % and ALLOW and DISALLOW are comma-separated lists of module names.
    %
    % To make the file less verbose, `builtin' and `private_builtin' are
    % implicitly included in an ALLOW list unless M is itself `builtin'
    % or `private_builtin'.
    %
    % It is an error to provide both a module_allow_deps term and a
    % module_disallow_deps term for the same module M.
    %
    % A module_allow_deps term with a first argument M specifies that
    % in the process of making M.trans_opt, the compiler may read
    % T.trans_opt only if T is in the ALLOW list.
    %
    % A module_disallow_deps term with a first argument M specifies that
    % in the process of making M.trans_opt, the compiler may NOT read
    % T.trans_opt if T is in the DISALLOW list.
    %
:- pred read_trans_opt_deps_spec_file(string::in,
    maybe1(trans_opt_deps_spec)::out, io::di, io::uo) is det.

read_trans_opt_deps_spec_file(FileName, Result, !IO) :-
    io.read_named_file_as_string(FileName, ReadResult, !IO),
    (
        ReadResult = ok(Contents),
        string.length(Contents, ContentsLen),
        StartPos = init_posn,
        parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
            StartPos, _EndPos, map.init, EdgesToRemove, [], FileSpecs0),
        (
            FileSpecs0 = [],
            Result = ok1(EdgesToRemove)
        ;
            FileSpecs0 = [_ | _],
            list.foldl(accumulate_contexts, FileSpecs0,
                set.init, FileSpecContextsSet),
            set.to_sorted_list(FileSpecContextsSet, FileSpecContexts),
            list.reverse(FileSpecContexts, RevFileSpecContexts),
            (
                RevFileSpecContexts = [],
                % Every error_spec parse_trans_opt_deps_spec_file constructs
                % should have a context.
                unexpected($pred, "RevFileSpecContexts = []")
            ;
                RevFileSpecContexts = [LastContext | _]
            ),
            IgnorePieces = [invis_order_default_end(0, ""),
                words("Ignoring"), quote(FileName),
                words("due to the presence of errors."), nl],
            IgnoreSpec = spec($pred, severity_error, phase_read_files,
                LastContext, IgnorePieces),
            FileSpecs = [IgnoreSpec | FileSpecs0],
            Result = error1(FileSpecs)
        )
    ;
        ReadResult = error(Error),
        Pieces = [words("Error: cannot open"), quote(FileName), suffix(":"),
            words(io.error_message(Error)), suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_error, phase_read_files, Pieces),
        Result = error1([Spec])
    ).

:- pred parse_trans_opt_deps_spec_file(string::in, string::in, int::in,
    posn::in, posn::out, trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
        !Pos, !EdgesToRemove, !Specs) :-
    read_term_from_substring(FileName, Contents, ContentsLen, !Pos, ReadTerm),
    (
        ReadTerm = eof
    ;
        ReadTerm = error(Error, LineNum),
        Pieces = [words("Read error:"), words(Error), suffix("."), nl],
        Context = context(FileName, LineNum),
        Spec = spec($pred, severity_error, phase_read_files, Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ;
        ReadTerm = term(VarSet, Term),
        parse_trans_opt_deps_spec_term(VarSet, Term, !EdgesToRemove, !Specs),
        parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
            !Pos, !EdgesToRemove, !Specs)
    ).

:- pred parse_trans_opt_deps_spec_term(varset::in, term::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_term(VarSet, Term, !EdgesToRemove, !Specs) :-
    ( if
        Term = functor(atom(AtomName), [LeftTerm, RightTerm], _Context),
        (
            AtomName = "module_allow_deps"
        ;
            AtomName = "module_disallow_deps"
        ),
        try_parse_sym_name(LeftTerm, SourceName)
    then
        parse_trans_opt_deps_spec_module_list(VarSet, RightTerm,
            cord.init, TargetCord0, [], EntrySpecs),
        (
            EntrySpecs = [],
            TargetList0 = cord.list(TargetCord0),
            LeftTermContext = get_term_context(LeftTerm),
            (
                AtomName = "module_allow_deps",
                ( if
                    SourceName \= unqualified("builtin"),
                    SourceName \= unqualified("private_builtin")
                then
                    TargetList = [
                        dummy_context - unqualified("builtin"),
                        dummy_context - unqualified("private_builtin") |
                        TargetList0
                    ]
                else
                    TargetList = TargetList0
                ),
                AllowOrDisallow = module_allow_deps(LeftTermContext,
                    TargetList)
            ;
                AtomName = "module_disallow_deps",
                AllowOrDisallow = module_disallow_deps(LeftTermContext,
                    TargetList0)
            ),
            map.search_insert(SourceName, AllowOrDisallow,
                MaybeOldAllowOrDisallow, !EdgesToRemove),
            (
                MaybeOldAllowOrDisallow = no
            ;
                MaybeOldAllowOrDisallow = yes(OldAllowOrDisallow),
                ( OldAllowOrDisallow = module_allow_deps(OldContext, _)
                ; OldAllowOrDisallow = module_disallow_deps(OldContext, _)
                ),
                Pieces1 = [words("Error: duplicate entry for source module"),
                    qual_sym_name(SourceName), suffix("."), nl],
                Pieces2 = [words("The original entry is here."), nl],
                Msg1 = msg(LeftTermContext, Pieces1),
                Msg2 = msg(OldContext, Pieces2),
                Spec = error_spec($pred, severity_error, phase_read_files,
                    [Msg1, Msg2]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            EntrySpecs = [_ | _],
            !:Specs = EntrySpecs ++ !.Specs
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected either"),
            nl_indent_delta(1),
            quote("module_allow_deps(module_name, module_name_list)"),
            nl_indent_delta(-1),
            words("or"),
            nl_indent_delta(1),
            quote("module_disallow_deps(module_name, module_name_list)"),
            suffix(","), nl_indent_delta(-1),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_trans_opt_deps_spec_module_list(varset::in, term::in,
    cord(pair(term_context, module_name))::in,
    cord(pair(term_context, module_name))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_module_list(VarSet, Term, !ModuleNameCord, !Specs) :-
    ( if list_term_to_term_list(Term, TermList) then
        parse_trans_opt_deps_spec_module_names(VarSet, TermList,
            !ModuleNameCord, !Specs)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a list, got"),
            quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred parse_trans_opt_deps_spec_module_names(varset::in, list(term)::in,
    cord(pair(term_context, module_name))::in,
    cord(pair(term_context, module_name))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_trans_opt_deps_spec_module_names(_VarSet, [], !ModuleNameCord, !Specs).
parse_trans_opt_deps_spec_module_names(VarSet, [Term | Terms],
        !ModuleNameCord, !Specs) :-
    ( if try_parse_sym_name(Term, ModuleName) then
        cord.snoc(get_term_context(Term) - ModuleName, !ModuleNameCord)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a module name, got"),
            quote(TermStr), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_read_files,
            get_term_context(Term), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    parse_trans_opt_deps_spec_module_names(VarSet, Terms,
        !ModuleNameCord, !Specs).

%---------------------------------------------------------------------------%

:- pred report_unknown_module_names_in_deps_spec(digraph(module_name)::in,
    trans_opt_deps_spec::in, list(error_spec)::out) is det.

report_unknown_module_names_in_deps_spec(Graph, DepsSpec, Specs) :-
    digraph.vertices(Graph, KnownModules),
    map.foldl(report_unknown_module_names_in_allow_disallow(KnownModules),
        DepsSpec, [], Specs).

:- pred report_unknown_module_names_in_allow_disallow(set(module_name)::in,
    module_name::in, allow_or_disallow_trans_opt_deps::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unknown_module_names_in_allow_disallow(KnownModules,
        Module, AllowOrDisallow, !Specs) :-
    (
        AllowOrDisallow = module_allow_deps(Context, TargetModules),
        AoD = "allowed"
    ;
        AllowOrDisallow = module_disallow_deps(Context, TargetModules),
        AoD = "disallowed"
    ),
    ( if set.contains(KnownModules, Module) then
        true
    else
        Pieces = [words("Warning: the module name"), qual_sym_name(Module),
            words("does not occur in the dependency graph."), nl],
        Spec = spec($pred, severity_warning, phase_read_files,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    report_unknown_module_names_in_module_names(KnownModules, AoD, 1,
        TargetModules, map.init, !Specs).

:- pred report_unknown_module_names_in_module_names(set(module_name)::in,
    string::in, int::in, assoc_list(term_context, module_name)::in,
    map(module_name, int)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_unknown_module_names_in_module_names(_, _, _, [], _OrdMap, !Specs).
report_unknown_module_names_in_module_names(KnownModules, AoD, N,
        [Context - Module | ContextModules], !.OrdMap, !Specs) :-
    ( if set.contains(KnownModules, Module) then
        map.search_insert(Module, N, MaybeOldN, !OrdMap),
        (
            MaybeOldN = no
        ;
            MaybeOldN = yes(OldN),
            Pieces = [words("Warning: the"), nth_fixed(N), words(AoD),
                words("module name"), qual_sym_name(Module),
                words("is the same as the"), nth_fixed(OldN), words(AoD),
                words("module name."), nl],
            Spec = spec($pred, severity_warning, phase_read_files,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    else
        Pieces = [words("Warning: the"), nth_fixed(N), words(AoD),
            words("module name"), qual_sym_name(Module),
            words("does not occur in the dependency graph."), nl],
        Spec = spec($pred, severity_warning, phase_read_files,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    report_unknown_module_names_in_module_names(KnownModules,
        AoD, N + 1, ContextModules, !.OrdMap, !Specs).

%---------------------------------------------------------------------------%

:- pred apply_trans_opt_deps_spec(trans_opt_deps_spec::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec(EdgesToRemove, !Graph) :-
    SCCs = set.to_sorted_list(digraph.cliques(!.Graph)),
    list.foldl2(apply_trans_opt_deps_spec_in_scc, SCCs,
        EdgesToRemove, _EdgesToRemove, !Graph).

:- pred apply_trans_opt_deps_spec_in_scc(set(digraph_key(module_name))::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec_in_scc(SCC, !EdgesToRemove, !Graph) :-
    set.foldl2(apply_trans_opt_deps_spec_for_module, SCC,
        !EdgesToRemove, !Graph).

:- pred apply_trans_opt_deps_spec_for_module(digraph_key(module_name)::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec_for_module(SourceKey, !EdgesToRemove, !Graph) :-
    digraph.lookup_vertex(!.Graph, SourceKey, SourceName),
    ( if map.search(!.EdgesToRemove, SourceName, AllowOrDisallow) then
        digraph.lookup_from(!.Graph, SourceKey, TargetSet),
        (
            AllowOrDisallow = module_allow_deps(_Context, AllowList),
            assoc_list.values(AllowList, AllowModuleList),
            set.list_to_set(AllowModuleList, AllowSet),
            set.foldl(apply_module_allow_deps(AllowSet, SourceKey),
                TargetSet, !Graph)
        ;
            AllowOrDisallow = module_disallow_deps(_Context, DisallowList),
            assoc_list.values(DisallowList, DisallowModuleList),
            set.list_to_set(DisallowModuleList, DisallowSet),
            set.foldl(apply_module_disallow_deps(DisallowSet, SourceKey),
                TargetSet, !Graph)
        )
    else
        true
    ).

:- pred apply_module_allow_deps(set(module_name)::in,
    digraph_key(module_name)::in, digraph_key(module_name)::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_module_allow_deps(AllowSet, SourceKey, TargetKey, !Graph) :-
    digraph.lookup_vertex(!.Graph, TargetKey, TargetName),
    ( if set.contains(AllowSet, TargetName) then
        true
    else
        digraph.delete_edge(SourceKey, TargetKey, !Graph)
    ).

:- pred apply_module_disallow_deps(set(module_name)::in,
    digraph_key(module_name)::in, digraph_key(module_name)::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_module_disallow_deps(DisallowSet, SourceKey, TargetKey, !Graph) :-
    digraph.lookup_vertex(!.Graph, TargetKey, TargetName),
    ( if set.contains(DisallowSet, TargetName) then
        digraph.delete_edge(SourceKey, TargetKey, !Graph)
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.generate_dep_d_files.
%---------------------------------------------------------------------------%
