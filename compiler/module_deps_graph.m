%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: module_deps_graph.m.
%
% XXX document me.
% The contents of this module used to be in the old giant version of modules.m.
% If you want to understand it and clean it up, you will also want to look at
% deps_map.m, whose functionality seems to be quite related.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.module_deps_graph.
:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.module_imports.

:- import_module digraph.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % (Module1 -> Module2) means Module1 is imported by Module2.
:- type deps_graph == digraph(module_name).
:- type deps_graph_key == digraph_key(module_name).

%-----------------------------------------------------------------------------%

:- type lookup_module_and_imports == (func(module_name) = module_and_imports).
:- mode lookup_module_and_imports == in(func(in) = out is det).

    % add_module_relations(LookupModuleImports, ModuleName,
    %   !IntDepsRel, !ImplDepsRel)
    %
    % Add a module's interface and implementation dependencies to IntDepsRel
    % and ImplDepsRel respectively. Dependencies are found using the
    % LookupModuleImports function.
    %
:- pred add_module_relations(
    lookup_module_and_imports::lookup_module_and_imports,
    module_name::in, digraph(module_name)::in, digraph(module_name)::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

    % Construct a pair of dependency graphs (the interface dependencies
    % and the implementation dependencies) for all the modules in the program.
    %
:- pred deps_list_to_deps_graph(list(deps)::in, deps_map::in,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

%-----------------------------------------------------------------------------%

:- pred get_dependencies_from_graph(deps_graph::in, module_name::in,
    list(module_name)::out) is det.

%-----------------------------------------------------------------------------%

:- pred maybe_output_imports_graph(globals::in, module_name::in,
    digraph(sym_name)::in, digraph(sym_name)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.deps_map.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_io_error.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module sparse_bitset.
:- import_module string.

%-----------------------------------------------------------------------------%

add_module_relations(LookupModuleImports, ModuleName,
        !IntDepsGraph, !ImplDepsGraph) :-
    ModuleImports = LookupModuleImports(ModuleName),
    add_module_and_imports_to_deps_graph(ModuleImports, LookupModuleImports,
        !IntDepsGraph, !ImplDepsGraph).

%-----------------------------------------------------------------------------%

deps_list_to_deps_graph([], _, !IntDepsGraph, !ImplDepsGraph).
deps_list_to_deps_graph([Deps | DepsList], DepsMap,
        !IntDepsGraph, !ImplDepsGraph) :-
    Deps = deps(_, ModuleImports),
    ModuleErrors = ModuleImports ^ mai_errors,
    set.intersect(ModuleErrors, fatal_read_module_errors, FatalModuleErrors),
    ( if set.is_empty(FatalModuleErrors) then
        add_module_and_imports_to_deps_graph(ModuleImports,
            lookup_module_and_imports_in_deps_map(DepsMap),
            !IntDepsGraph, !ImplDepsGraph)
    else
        true
    ),
    deps_list_to_deps_graph(DepsList, DepsMap, !IntDepsGraph, !ImplDepsGraph).

:- func lookup_module_and_imports_in_deps_map(deps_map, module_name)
    = module_and_imports.

lookup_module_and_imports_in_deps_map(DepsMap, ModuleName) = ModuleImports :-
    map.lookup(DepsMap, ModuleName, deps(_, ModuleImports)).

%-----------------------------------------------------------------------------%

:- pred add_module_and_imports_to_deps_graph(module_and_imports::in,
    lookup_module_and_imports::lookup_module_and_imports,
    deps_graph::in, deps_graph::out, deps_graph::in, deps_graph::out) is det.

add_module_and_imports_to_deps_graph(ModuleImports, LookupModuleImports,
        !IntDepsGraph, !ImplDepsGraph) :-
    % Add interface dependencies to the interface deps graph.
    %
    % Note that we need to do this both for the interface imports of this
    % module and for the *implementation* imports of its ancestors.
    % This is because if this module is defined in the implementation section
    % of its parent, then the interface of this module may depend on things
    % imported only by its parent's implementation.
    %
    % If this module was actually defined in the interface section of one
    % of its ancestors, then it should only depend on the interface imports
    % of that ancestor, so the dependencies added here are in fact more
    % conservative than they need to be in that case. However, that should
    % not be a major problem.
    % XXX Actually, I (zs) think it is, because I suspect that after some
    % source code changes, it can lead to the unnecessary recompilation
    % of not just a few, but many modules.

    ModuleName = ModuleImports ^ mai_module_name,
    ParentDeps = ModuleImports ^ mai_parent_deps,
    digraph.add_vertex(ModuleName, IntModuleKey, !IntDepsGraph),
    add_int_deps(IntModuleKey, ModuleImports, !IntDepsGraph),
    add_parent_impl_deps_list(LookupModuleImports, IntModuleKey, ParentDeps,
        !IntDepsGraph),

    % Add implementation dependencies to the impl. deps graph.
    % (The implementation dependencies are a superset of the interface
    % dependencies.)
    %
    % Note that we need to do this both for the imports of this module
    % and for the imports of its parents, because this module may depend on
    % things imported only by its parents.

    digraph.add_vertex(ModuleName, ImplModuleKey, !ImplDepsGraph),
    add_impl_deps(ImplModuleKey, ModuleImports, !ImplDepsGraph),
    add_parent_impl_deps_list(LookupModuleImports, ImplModuleKey, ParentDeps,
        !ImplDepsGraph).

    % Add interface dependencies to the interface deps graph.
    %
:- pred add_int_deps(deps_graph_key::in, module_and_imports::in,
    deps_graph::in, deps_graph::out) is det.

add_int_deps(ModuleKey, ModuleImports, !DepsGraph) :-
    AddDep = add_dep(ModuleKey),
    list.foldl(AddDep, ModuleImports ^ mai_parent_deps, !DepsGraph),
    list.foldl(AddDep, ModuleImports ^ mai_int_deps, !DepsGraph).

    % Add direct implementation dependencies for a module to the
    % implementation deps graph.
    %
:- pred add_impl_deps(deps_graph_key::in, module_and_imports::in,
    deps_graph::in, deps_graph::out) is det.

add_impl_deps(ModuleKey, ModuleImports, !DepsGraph) :-
    % The implementation dependencies are a superset of the
    % interface dependencies, so first we add the interface deps.
    add_int_deps(ModuleKey, ModuleImports, !DepsGraph),
    % then we add the impl deps
    module_and_imports_get_impl_deps(ModuleImports, ImplDeps),
    list.foldl(add_dep(ModuleKey), ImplDeps, !DepsGraph).

    % Add parent implementation dependencies for the given Parent module
    % to the impl. deps graph values for the given ModuleKey.
    %
:- pred add_parent_impl_deps(
    lookup_module_and_imports::lookup_module_and_imports,
    deps_graph_key::in, module_name::in, deps_graph::in, deps_graph::out)
    is det.

add_parent_impl_deps(LookupModuleImports, ModuleKey, Parent, !DepsGraph) :-
    ParentModuleImports = LookupModuleImports(Parent),
    add_impl_deps(ModuleKey, ParentModuleImports, !DepsGraph).

:- pred add_parent_impl_deps_list(
    lookup_module_and_imports::lookup_module_and_imports,
    deps_graph_key::in, list(module_name)::in, deps_graph::in, deps_graph::out)
    is det.

add_parent_impl_deps_list(LookupModuleImports, ModuleKey, Parents,
        !DepsGraph) :-
    list.foldl(add_parent_impl_deps(LookupModuleImports, ModuleKey), Parents,
        !DepsGraph).

    % Add a single dependency to a graph.
    %
:- pred add_dep(digraph_key(T)::in, T::in, digraph(T)::in, digraph(T)::out)
    is det.

add_dep(ModuleKey, Dep, !DepsGraph) :-
    digraph.add_vertex(Dep, DepKey, !DepsGraph),
    digraph.add_edge(ModuleKey, DepKey, !DepsGraph).

%-----------------------------------------------------------------------------%

get_dependencies_from_graph(DepsGraph0, ModuleName, Deps) :-
    digraph.add_vertex(ModuleName, ModuleKey, DepsGraph0, DepsGraph),
    digraph.lookup_key_set_from(DepsGraph, ModuleKey, DepsKeysSet),
    sparse_bitset.foldl(
        (pred(Key::in, Deps0::in, [Dep | Deps0]::out) is det :-
            digraph.lookup_vertex(DepsGraph, Key, Dep)
        ), DepsKeysSet, [], Deps).

%-----------------------------------------------------------------------------%

maybe_output_imports_graph(Globals, Module, IntDepsGraph, ImplDepsGraph,
        !IO) :-
    globals.lookup_bool_option(Globals, imports_graph, ImportsGraph),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    (
        ImportsGraph = yes,
        module_name_to_file_name(Globals, Module, ".imports_graph",
            do_create_dirs, FileName, !IO),
        maybe_write_string(Verbose, "% Creating imports graph file `", !IO),
        maybe_write_string(Verbose, FileName, !IO),
        maybe_write_string(Verbose, "'...", !IO),
        io.open_output(FileName, ImpResult, !IO),
        (
            ImpResult = ok(ImpStream),

            Deps0 = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(IntDepsGraph), digraph.init),
            Deps = list.foldl(filter_imports_graph,
                digraph.to_assoc_list(ImplDepsGraph), Deps0),

            write_graph(ImpStream, "imports", sym_name_to_node_id, Deps, !IO),

            io.close_output(ImpStream, !IO),
            maybe_write_string(Verbose, " done.\n", !IO)
        ;
            ImpResult = error(IOError),
            maybe_write_string(Verbose, " failed.\n", !IO),
            maybe_flush_output(Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.append_list(["error opening file `", FileName,
                "' for output: ", IOErrorMessage], ImpMessage),
            report_error(ImpMessage, !IO)
        )
    ;
        ImportsGraph = no
    ).

:- func filter_imports_graph(pair(sym_name, sym_name), digraph(sym_name)) =
    digraph(sym_name).

filter_imports_graph(A - B, DepsGraph) =
    (
        % Don't keep the edge if it points to a builtin-module or if the
        % relationship is between two standard library modules.
        % XXX it would be better to change this to be only keep those
        % edges for which the left-hand side is in the current directory.
        (
            any_mercury_builtin_module(B)
        ;
            is_std_lib_module_name(A, _),
            is_std_lib_module_name(B, _)
        )
    ->
        DepsGraph
    ;
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

%-----------------------------------------------------------------------------%
:- end_module parse_tree.module_deps_graph.
%-----------------------------------------------------------------------------%
