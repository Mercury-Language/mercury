%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: generate_dep_d_files.m.
% Main author: fjh (when this code was in modules.m)
%
% This module figures out the information from which write_deps_file.m
% creates dependency files (.dep and .d files) for mmake.
%
%---------------------------------------------------------------------------%

:- module parse_tree.generate_dep_d_files.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.

    % generate_module_dependencies(Globals, ModuleName, !IO):
    %
    % Generate the per-program makefile dependencies (`.dep') file for a
    % program whose top-level module is `ModuleName'. This involves first
    % transitively reading in all imported or ancestor modules. While we're
    % at it, we also save the per-module makefile dependency (`.d') files
    % for all those modules.
    %
:- pred generate_dep_file_for_module(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_file_dependencies(Globals, FileName, !IO):
    %
    % Same as generate_module_dependencies, but takes a file name instead of
    % a module name.
    %
:- pred generate_dep_file_for_file(globals::in, file_name::in,
    io::di, io::uo) is det.

    % generate_module_dependency_file(Globals, ModuleName, !IO):
    %
    % Generate the per module makefile dependency ('.d') file for the
    % given module.
    %
:- pred generate_d_file_for_module(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_file_dependency_file(Globals, FileName, !IO):
    %
    % Same as generate_module_dependency_file, but takes a file name instead of
    % a module name.
    %
:- pred generate_d_file_for_file(globals::in, file_name::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.write_deps_file.

:- import_module bool.
:- import_module digraph.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_dep_file_for_module(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_dep_file_for_file(Globals, FileName, !IO) :-
    build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_d_file_for_module(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_d_file_only, do_search,
        ModuleName, DepsMap, !IO).

generate_d_file_for_file(Globals, FileName, !IO) :-
    build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO),
    generate_dependencies(Globals, output_d_file_only, do_search,
        ModuleName, DepsMap, !IO).

%---------------------------------------------------------------------------%

:- pred build_deps_map(globals::in, file_name::in,
    module_name::out, deps_map::out, io::di, io::uo) is det.

build_deps_map(Globals, FileName, ModuleName, DepsMap, !IO) :-
    % Read in the top-level file (to figure out its module name).
    FileNameDotM = FileName ++ ".m",
    read_module_src_from_file(Globals, FileName, FileNameDotM, "Reading file",
        do_not_search, always_read_module(dont_return_timestamp), _,
        ParseTreeSrc, Specs0, ReadModuleErrors, !IO),
    ParseTreeSrc = parse_tree_src(ModuleName, _, _),
    parse_tree_src_to_module_and_imports_list(Globals, FileNameDotM,
        ParseTreeSrc, ReadModuleErrors, Specs0, Specs,
        _RawCompUnits, ModuleAndImportsList),
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    write_error_specs_ignore(ErrorStream, Globals, Specs, !IO),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map, ModuleAndImportsList, DepsMap0, DepsMap).

%---------------------------------------------------------------------------%

:- type generate_dependencies_mode
    --->    output_d_file_only
    ;       output_all_dependencies.

:- pred generate_dependencies(globals::in, generate_dependencies_mode::in,
    maybe_search::in, module_name::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies(Globals, Mode, Search, ModuleName, DepsMap0,
        !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(Globals, Search, ModuleName, DepsMap0, DepsMap,
        [], DepsMapSpecs, !IO),
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    write_error_specs_ignore(ErrorStream, Globals, DepsMapSpecs, !IO),

    % Check whether we could read the main `.m' file.
    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, ModuleAndImports),
    module_and_imports_get_errors(ModuleAndImports, Errors),
    set.intersect(Errors, fatal_read_module_errors, FatalErrors),
    ( if set.is_non_empty(FatalErrors) then
        ModuleNameStr = sym_name_to_string(ModuleName),
        ( if set.contains(FatalErrors, rme_could_not_open_file) then
            string.format("cannot read source file for module `%s'.",
                [s(ModuleNameStr)], Message)
        else
            string.format("cannot parse source file for module `%s'.\n",
                [s(ModuleNameStr)], Message)
        ),
        report_error(ErrorStream, Message, !IO)
    else
        (
            Mode = output_d_file_only
        ;
            Mode = output_all_dependencies,
            module_and_imports_get_source_file_name(ModuleAndImports,
                SourceFileName),
            generate_dependencies_write_dv_file(Globals, SourceFileName,
                ModuleName, DepsMap, !IO),
            generate_dependencies_write_dep_file(Globals, SourceFileName,
                ModuleName, DepsMap, !IO)
        ),

        % Compute the interface deps graph and the implementation deps
        % graph from the deps map.

        digraph.init(IntDepsGraph0),
        digraph.init(ImpDepsGraph0),
        map.values(DepsMap, DepsList),
        deps_list_to_deps_graph(DepsList, DepsMap, IntDepsGraph0, IntDepsGraph,
            ImpDepsGraph0, ImpDepsGraph),
        maybe_output_imports_graph(Globals, ModuleName,
            IntDepsGraph, ImpDepsGraph, !IO),

        % Compute the trans-opt deps ordering, by doing an approximate
        % topological sort of the implementation deps, and then finding
        % the subset of those for which of those we have (or can make)
        % trans-opt files.

        digraph.atsort(ImpDepsGraph, ImpDepsOrdering0),
        maybe_output_module_order(Globals, ModuleName, ImpDepsOrdering0, !IO),
        list.map(set.to_sorted_list, ImpDepsOrdering0, ImpDepsOrdering),
        list.condense(ImpDepsOrdering, TransOptDepsOrdering0),
        globals.lookup_accumulating_option(Globals, intermod_directories,
            IntermodDirs),
        get_opt_deps(Globals, yes, IntermodDirs, other_ext(".trans_opt"),
            TransOptDepsOrdering0, TransOptDepsOrdering, !IO),

        trace [compiletime(flag("deps_graph")), runtime(env("DEPS_GRAPH")),
            io(!TIO)]
        (
            digraph.to_assoc_list(ImpDepsGraph, ImpDepsAL),
            get_debug_output_stream(Globals, ModuleName, DebugStream, !TIO),
            io.write_string(DebugStream, "ImpDepsAL:\n", !TIO),
            list.foldl(io.write_line(DebugStream), ImpDepsAL, !TIO)
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

        (
            Mode = output_d_file_only,
            DFilesToWrite = [ModuleDep]
        ;
            Mode = output_all_dependencies,
            DFilesToWrite = DepsList
        ),
        generate_dependencies_write_d_files(Globals, DFilesToWrite,
            IntDepsGraph, ImpDepsGraph,
            IndirectDepsGraph, IndirectOptDepsGraph,
            TransOptDepsOrdering, DepsMap, !IO)
    ),

    % For Java, the main target is actually a shell script which will
    % set CLASSPATH appropriately and invoke java on the appropriate
    % .class file. Rather than generating an Mmake rule to build this
    % file when it is needed, we just generate this file "mmake depend"
    % time, since that is simpler and probably more efficient anyway.

    globals.get_target(Globals, Target),
    ( if
        Target = target_java,
        Mode = output_all_dependencies
    then
        create_java_shell_script(Globals, ModuleName, _Succeeded, !IO)
    else
        true
    ).

    % Construct a pair of dependency graphs (the interface dependencies
    % and the implementation dependencies) for all the modules in the program.
    %
:- pred deps_list_to_deps_graph(list(deps)::in, deps_map::in,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

deps_list_to_deps_graph([], _, !IntDepsGraph, !ImplDepsGraph).
deps_list_to_deps_graph([Deps | DepsList], DepsMap,
        !IntDepsGraph, !ImplDepsGraph) :-
    Deps = deps(_, ModuleAndImports),
    module_and_imports_get_errors(ModuleAndImports, ModuleErrors),
    set.intersect(ModuleErrors, fatal_read_module_errors, FatalModuleErrors),
    ( if set.is_empty(FatalModuleErrors) then
        ModuleDepInfo = module_dep_info_imports(ModuleAndImports),
        add_module_dep_info_to_deps_graph(ModuleDepInfo,
            lookup_module_and_imports_in_deps_map(DepsMap),
            !IntDepsGraph, !ImplDepsGraph)
    else
        true
    ),
    deps_list_to_deps_graph(DepsList, DepsMap, !IntDepsGraph, !ImplDepsGraph).

:- func lookup_module_and_imports_in_deps_map(deps_map, module_name)
    = module_dep_info.

lookup_module_and_imports_in_deps_map(DepsMap, ModuleName)
        = ModuleDepInfo :-
    map.lookup(DepsMap, ModuleName, deps(_, ModuleAndImports)),
    ModuleDepInfo = module_dep_info_imports(ModuleAndImports).

%---------------------------------------------------------------------------%

:- pred maybe_output_imports_graph(globals::in, module_name::in,
    digraph(sym_name)::in, digraph(sym_name)::in, io::di, io::uo) is det.

maybe_output_imports_graph(Globals, ModuleName, IntDepsGraph, ImpDepsGraph,
        !IO) :-
    globals.lookup_bool_option(Globals, imports_graph, ImportsGraph),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        ImportsGraph = yes,
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".imports_graph")), ModuleName, FileName, !IO),
        (
            Verbose = no,
            MaybeProgressStream = no
        ;
            Verbose = yes,
            get_progress_output_stream(Globals, ModuleName,
                ProgressStream0, !IO),
            io.format(ProgressStream0,
                "%% Creating imports graph file `%s'...",
                [s(FileName)], !IO),
            MaybeProgressStream = yes(ProgressStream0)
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
                MaybeProgressStream = no
            ;
                MaybeProgressStream = yes(ProgressStream),
                io.write_string(ProgressStream, " done.\n", !IO)
            )
        ;
            ImpResult = error(IOError),
            (
                MaybeProgressStream = no
            ;
                MaybeProgressStream = yes(ProgressStream),
                io.write_string(ProgressStream, " failed.\n", !IO),
                io.flush_output(ProgressStream, !IO)
            ),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening file `%s' for output: %s\n",
                [s(FileName), s(IOErrorMessage)], ImpMessage),
            report_error(ErrorStream, ImpMessage, !IO)
        )
    ;
        ImportsGraph = no
    ).

:- func filter_imports_graph(pair(sym_name, sym_name), digraph(sym_name)) =
    digraph(sym_name).

filter_imports_graph(A - B, DepsGraph) =
    ( if
        % Don't keep the edge if it points to a builtin-module or if the
        % relationship is between two standard library modules.
        % XXX It would be better to change this to be only keep those
        % edges for which the left-hand side is in the current directory.
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

:- type gen_node_name(T) == (func(T) = string).

:- pred write_graph(io.output_stream::in, string::in,
    gen_node_name(T)::in, digraph(T)::in, io::di, io::uo) is det.

write_graph(Stream, Name, GenNodeName, Graph, !IO) :-
    io.write_string(Stream, "digraph " ++ Name ++ " {\n", !IO),
    io.write_string(Stream, "label=\"" ++ Name ++ "\";\n", !IO),
    io.write_string(Stream, "center=true;\n", !IO),
    digraph.traverse(Graph, write_node(Stream, GenNodeName),
        write_edge(Stream, GenNodeName), !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred write_node(io.output_stream::in,
    gen_node_name(T)::in, T::in, io::di, io::uo) is det.

write_node(Stream, GenNodeName, Node, !IO) :-
    % Names can't contain "." so use "__"
    io.write_string(Stream, GenNodeName(Node), !IO),
    io.write_string(Stream, ";\n", !IO).

:- pred write_edge(io.output_stream::in, gen_node_name(T)::in, T::in, T::in,
    io::di, io::uo) is det.

write_edge(Stream, GenNodeName, A, B, !IO) :-
    io.write_string(Stream, GenNodeName(A), !IO),
    io.write_string(Stream, " -> ", !IO),
    io.write_string(Stream, GenNodeName(B), !IO),
    io.write_string(Stream, ";\n", !IO).

:- func sym_name_to_node_id(sym_name) = string.

sym_name_to_node_id(Name) =
    "\"" ++ sym_name_to_string(Name) ++ "\"".

%---------------------------------------------------------------------------%
:- end_module parse_tree.generate_dep_d_files.
%---------------------------------------------------------------------------%
