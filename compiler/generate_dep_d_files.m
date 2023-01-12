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

    % generate_dep_file_for_module(Globals, ModuleName, !IO):
    %
    % Generate the per-program makefile dependencies file (`.dep' file)
    % for a program whose top-level module is `ModuleName'. This involves
    % first transitively reading in all imported or ancestor modules.
    % While we are at it, we also save the per-module makefile dependency files
    % (`.d' files) for all those modules.
    %
:- pred generate_dep_file_for_module(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_dep_file_for_file(Globals, FileName, !IO):
    %
    % Same as generate_dep_file_for_module, but takes a file name
    % instead of a module name.
    %
:- pred generate_dep_file_for_file(globals::in, file_name::in,
    io::di, io::uo) is det.

    % generate_d_file_for_module(Globals, ModuleName, !IO):
    %
    % Generate the per-module makefile dependency file ('.d' file)
    % for the given module.
    %
:- pred generate_d_file_for_module(globals::in, module_name::in,
    io::di, io::uo) is det.

    % generate_d_file_for_file(Globals, FileName, !IO):
    %
    % Same as generate_d_file_for_module, but takes a file name
    % instead of a module name.
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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.module_deps_graph.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.write_deps_file.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module digraph.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module mercury_term_parser.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

generate_dep_file_for_module(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap, !IO).

generate_dep_file_for_file(Globals, FileName, !IO) :-
    build_initial_deps_map_for_file(Globals, FileName,
        ModuleName, DepsMap0, !IO),
    generate_dependencies(Globals, output_all_dependencies, do_not_search,
        ModuleName, DepsMap0, !IO).

generate_d_file_for_module(Globals, ModuleName, !IO) :-
    map.init(DepsMap),
    generate_dependencies(Globals, output_d_file_only, do_search,
        ModuleName, DepsMap, !IO).

generate_d_file_for_file(Globals, FileName, !IO) :-
    build_initial_deps_map_for_file(Globals, FileName,
        ModuleName, DepsMap0, !IO),
    generate_dependencies(Globals, output_d_file_only, do_search,
        ModuleName, DepsMap0, !IO).

%---------------------------------------------------------------------------%

:- pred build_initial_deps_map_for_file(globals::in, file_name::in,
    module_name::out, deps_map::out, io::di, io::uo) is det.

build_initial_deps_map_for_file(Globals, FileName, ModuleName, DepsMap, !IO) :-
    % Read in the top-level file (to figure out its module name).
    FileNameDotM = FileName ++ ".m",
    read_module_src_from_file(Globals, FileName, FileNameDotM, rrm_file,
        do_not_search, always_read_module(dont_return_timestamp),
        HaveReadModuleSrc, !IO),
    (
        HaveReadModuleSrc = have_read_module(_FN, _MTS,
            ParseTreeSrc, ReadModuleErrors),
        ParseTreeSrc = parse_tree_src(ModuleName, _, _),
        parse_tree_src_to_burdened_module_list(Globals, FileNameDotM,
            ParseTreeSrc, ReadModuleErrors, Specs, BurdenedModules)
    ;
        HaveReadModuleSrc = have_not_read_module(_, ReadModuleErrors),
        get_default_module_name_for_file(FileName, FileNameDotM,
            ModuleName, !IO),
        % XXX Caller should not need this info.
        Specs = get_read_module_specs(ReadModuleErrors),
        BurdenedModules = []
    ),
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    write_error_specs(ErrorStream, Globals, Specs, !IO),
    map.init(DepsMap0),
    list.foldl(insert_into_deps_map, BurdenedModules, DepsMap0, DepsMap).

%---------------------------------------------------------------------------%

:- type generate_dependencies_mode
    --->    output_d_file_only
    ;       output_all_dependencies.

:- pred generate_dependencies(globals::in, generate_dependencies_mode::in,
    maybe_search::in, module_name::in, deps_map::in, io::di, io::uo) is det.

generate_dependencies(Globals, Mode, Search, ModuleName, DepsMap0, !IO) :-
    % First, build up a map of the dependencies.
    generate_deps_map(Globals, Search, ModuleName, DepsMap0, DepsMap,
        [], DepsMapSpecs, !IO),
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    write_error_specs(ErrorStream, Globals, DepsMapSpecs, !IO),

    % Check whether we could read the main `.m' file.
    map.lookup(DepsMap, ModuleName, ModuleDep),
    ModuleDep = deps(_, BurdenedModule),
    BurdenedModule = burdened_module(Baggage, _ParseTreeModuleSrc),
    Errors = Baggage ^ mb_errors,
    FatalErrors = Errors ^ rm_fatal_errors,
    ( if set.is_non_empty(FatalErrors) then
        ModuleNameStr = sym_name_to_string(ModuleName),
        ( if set.contains(FatalErrors, frme_could_not_open_file) then
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
            SourceFileName = Baggage ^ mb_source_file_name,
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

        globals.lookup_bool_option(Globals, generate_module_order,
            OutputOrder),
        (
            OutputOrder = yes,
            digraph.atsort(ImpDepsGraph, ImpDepsOrdering),
            output_module_order(Globals, ModuleName, other_ext(".order"),
                ImpDepsOrdering, !IO)
        ;
            OutputOrder = no
        ),

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

        % Compute the trans-opt deps for the purpose of making `.trans_opt'
        % files. This is normally equal to transitive closure of the indirect
        % dependencies (i.e. IndirectOptDepsGraph) since a module may read the
        % `.trans_opt' file of any directly or indirectly imported module.
        %
        % To deal with cycles in the graph, by default, we impose an arbitrary
        % order on modules so that when making the trans-opt file for a module
        % "earlier" in the cycle, the compiler may read the trans-opt files
        % of modules "later" in the cycle, but not vice versa.
        %
        % The problem with that is twofold:
        % - Lack of parallelism. The trans-opt files for modules within a
        %   single SCC have to be made one after another.
        % - The arbitrary ordering is likely to produce sub-optimal
        %   information transfer between trans-opt files.
        %
        % To improve the situation, we allow the user to specify a list of
        % edges for the code below to remove from the dependency graph
        % (see read_trans_opt_deps_spec), with the intention of breaking up
        % SCCs and, hence, converting the graph into a dag.
        globals.lookup_maybe_string_option(Globals,
            trans_opt_deps_spec, MaybeSpecFileName),
        (
            MaybeSpecFileName = yes(SpecFileName),
            read_trans_opt_deps_spec_file(SpecFileName, MaybeEdgesToRemove,
                !IO),
            (
                MaybeEdgesToRemove = ok(EdgesToRemove),
                apply_trans_opt_deps_spec(EdgesToRemove, ImpDepsGraph,
                    TransOptDepsGraph0),
                digraph.tc(TransOptDepsGraph0, TransOptDepsGraph)
            ;
                MaybeEdgesToRemove = error(Error),
                report_error(ErrorStream, Error, !IO),
                TransOptDepsGraph = IndirectOptDepsGraph
            )
        ;
            MaybeSpecFileName = no,
            TransOptDepsGraph = IndirectOptDepsGraph
        ),
        digraph.atsort(TransOptDepsGraph, TransOptDepsOrdering0),
        (
            OutputOrder = yes,
            output_module_order(Globals, ModuleName,
                other_ext(".order-trans-opt"), TransOptDepsOrdering0, !IO)
        ;
            OutputOrder = no
        ),
        list.map(set.to_sorted_list, TransOptDepsOrdering0,
            TransOptDepsOrdering1),
        list.condense(TransOptDepsOrdering1, TransOptDepsOrdering),
        globals.lookup_accumulating_option(Globals, intermod_directories,
            IntermodDirs),
        get_opt_deps(Globals, yes, IntermodDirs, other_ext(".trans_opt"),
            TransOptDepsOrdering, TransOptOrder, !IO),

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
            TransOptDepsGraph, TransOptOrder, DepsMap, !IO)
    ),

    % For Java, the main target is actually a shell script which will set
    % CLASSPATH appropriately and invoke java on the appropriate .class file.
    % Rather than generating an Mmake rule to build this file when it is
    % needed, we just generate this file "mmake depend" time, since that is
    % simpler and probably more efficient anyway.

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

deps_list_to_deps_graph([], _, !IntDepsGraph, !ImpDepsGraph).
deps_list_to_deps_graph([Deps | DepsList], DepsMap,
        !IntDepsGraph, !ImpDepsGraph) :-
    Deps = deps(_, BurdenedModule),
    Baggage = BurdenedModule ^ bm_baggage,
    Errors = Baggage ^ mb_errors,
    FatalErrors = Errors ^ rm_fatal_errors,
    ( if set.is_empty(FatalErrors) then
        ModuleDepInfo = module_dep_info_full(BurdenedModule),
        add_module_dep_info_to_deps_graph(ModuleDepInfo,
            lookup_module_and_imports_in_deps_map(DepsMap),
            !IntDepsGraph, !ImpDepsGraph)
    else
        true
    ),
    deps_list_to_deps_graph(DepsList, DepsMap, !IntDepsGraph, !ImpDepsGraph).

:- func lookup_module_and_imports_in_deps_map(deps_map, module_name)
    = module_dep_info.

lookup_module_and_imports_in_deps_map(DepsMap, ModuleName)
        = ModuleDepInfo :-
    map.lookup(DepsMap, ModuleName, deps(_, BurdenedModule)),
    ModuleDepInfo = module_dep_info_full(BurdenedModule).

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

:- type trans_opt_deps_spec
    ==  map(module_name, allow_or_disallow_trans_opt_deps).

:- type allow_or_disallow_trans_opt_deps
    --->    module_allow_deps(set(module_name))
    ;       module_disallow_deps(set(module_name)).

    % The --trans-opt-deps-spec file shall contain a series of terms
    % of either form:
    %
    %   module_allow_deps(M, [ ALLOW ]).
    %   module_disallow_deps(M, [ DISALLOW ]).
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
    % A module_allow_deps term with a first argument M specifies that,
    % if the module M and another module T depend on each other,
    % directly or indirectly,
    % then in the process of making M.trans_opt,
    % the compiler may read T.trans_opt only if T is in the ALLOW list.
    %
    % A module_disallow_deps term with a first argument M specifies that,
    % if the module M and another module T depend on each other,
    % directly or indirectly,
    % then in the process of making M.trans_opt,
    % the compiler may read T.trans_opt unless T is in the DISALLOW list.
    %
    % TODO: report errors using error specs
    % TODO: report multiple errors
    %
:- pred read_trans_opt_deps_spec_file(string::in,
    maybe_error(trans_opt_deps_spec)::out, io::di, io::uo) is det.

read_trans_opt_deps_spec_file(FileName, Result, !IO) :-
    io.read_named_file_as_string(FileName, ReadResult, !IO),
    (
        ReadResult = ok(Contents),
        string.length(Contents, ContentsLen),
        StartPos = posn(1, 0, 0),
        parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
            ParseResult, StartPos, _EndPos, map.init, EdgesToRemove),
        (
            ParseResult = ok,
            Result = ok(EdgesToRemove)
        ;
            ParseResult = error(Error),
            Result = error(Error)
        )
    ;
        ReadResult = error(Error),
        Result = error(io.error_message(Error))
    ).

:- pred parse_trans_opt_deps_spec_file(string::in, string::in, int::in,
    maybe_error::out, posn::in, posn::out,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out) is det.

parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen, Result,
        !Pos, !EdgesToRemove) :-
    read_term_from_substring(FileName, Contents, ContentsLen, !Pos, ReadTerm),
    (
        ReadTerm = eof,
        Result = ok
    ;
        ReadTerm = error(Error, LineNum),
        string.format("%s:%d: %s", [s(FileName), i(LineNum), s(Error)], Msg),
        Result = error(Msg)
    ;
        ReadTerm = term(_VarSet, Term),
        parse_trans_opt_deps_spec_term(Term, Result0, !EdgesToRemove),
        (
            Result0 = ok,
            parse_trans_opt_deps_spec_file(FileName, Contents, ContentsLen,
                Result, !Pos, !EdgesToRemove)
        ;
            Result0 = error(Error),
            Result = error(Error)
        )
    ).

:- pred parse_trans_opt_deps_spec_term(term::in, maybe_error::out,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out) is det.

parse_trans_opt_deps_spec_term(Term, Result, !EdgesToRemove) :-
    ( if
        Term = functor(atom(AtomName), [LeftTerm, RightTerm], _Context),
        (
            AtomName = "module_allow_deps"
        ;
            AtomName = "module_disallow_deps"
        ),
        try_parse_symbol_name(LeftTerm, SourceName)
    then
        parse_trans_opt_deps_spec_module_list(RightTerm, Result0,
            [], TargetList0),
        (
            Result0 = ok,
            (
                AtomName = "module_allow_deps",
                ( if
                    SourceName \= unqualified("builtin"),
                    SourceName \= unqualified("private_builtin")
                then
                    TargetList = [
                        unqualified("builtin"),
                        unqualified("private_builtin") |
                        TargetList0
                    ]
                else
                    TargetList = TargetList0
                ),
                set.list_to_set(TargetList, TargetSet),
                AllowOrDisallow = module_allow_deps(TargetSet)
            ;
                AtomName = "module_disallow_deps",
                set.list_to_set(TargetList0, TargetSet),
                AllowOrDisallow = module_disallow_deps(TargetSet)
            ),
            ( if map.insert(SourceName, AllowOrDisallow, !EdgesToRemove) then
                Result = ok
            else
                get_term_context(Term) = context(FileName, LineNum),
                string.format("%s:%d: duplicate source module %s",
                    [s(FileName), i(LineNum),
                    s(sym_name_to_string(SourceName))], Msg),
                Result = error(Msg)
            )
        ;
            Result0 = error(Error),
            Result = error(Error)
        )
    else
        get_term_context(Term) = context(FileName, LineNum),
        string.format("%s:%d: expected module_allow_deps/2 or " ++
            "module_disallow_deps/2", [s(FileName), i(LineNum)], Msg),
        Result = error(Msg)
    ).

:- pred parse_trans_opt_deps_spec_module_list(term::in, maybe_error::out,
    list(module_name)::in, list(module_name)::out) is det.

parse_trans_opt_deps_spec_module_list(Term, Result, !RevModuleNames) :-
    ( if list_term_to_term_list(Term, TermList) then
        parse_trans_opt_deps_spec_module_names(TermList, Result,
            !RevModuleNames)
    else
        get_term_context(Term) = context(FileName, LineNum),
        string.format("%s:%d: expected list", [s(FileName), i(LineNum)],
            Msg),
        Result = error(Msg)
    ).

:- pred parse_trans_opt_deps_spec_module_names(list(term)::in,
    maybe_error::out, list(module_name)::in, list(module_name)::out) is det.

parse_trans_opt_deps_spec_module_names(Terms, Result, !RevModuleNames) :-
    (
        Terms = [],
        Result = ok
    ;
        Terms = [HeadTerm | TailTerm],
        ( if try_parse_symbol_name(HeadTerm, ModuleName) then
            !:RevModuleNames = [ModuleName | !.RevModuleNames],
            parse_trans_opt_deps_spec_module_names(TailTerm, Result,
                !RevModuleNames)
        else
            get_term_context(HeadTerm) = context(FileName, LineNum),
            string.format("%s:%d: expected module name",
                [s(FileName), i(LineNum)], Msg),
            Result = error(Msg)
        )
    ).

%---------------------------------------------------------------------------%

:- pred apply_trans_opt_deps_spec(trans_opt_deps_spec::in,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec(EdgesToRemove, !Graph) :-
    SCCs = set.to_sorted_list(digraph.cliques(!.Graph)),
    % TODO: report unseen source/target modules listed in the spec file
    list.foldl2(apply_trans_opt_deps_spec_in_scc, SCCs,
        EdgesToRemove, _EdgesToRemove, !Graph).

:- pred apply_trans_opt_deps_spec_in_scc(set(digraph_key(module_name))::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec_in_scc(SCC, !EdgesToRemove, !Graph) :-
    ( if set.count(SCC) > 1 then
        set.foldl2(apply_trans_opt_deps_spec_for_module, SCC,
            !EdgesToRemove, !Graph)
    else
        true
    ).

:- pred apply_trans_opt_deps_spec_for_module(digraph_key(module_name)::in,
    trans_opt_deps_spec::in, trans_opt_deps_spec::out,
    digraph(module_name)::in, digraph(module_name)::out) is det.

apply_trans_opt_deps_spec_for_module(SourceKey, !EdgesToRemove, !Graph) :-
    digraph.lookup_vertex(!.Graph, SourceKey, SourceName),
    ( if map.search(!.EdgesToRemove, SourceName, AllowOrDisallow) then
        digraph.lookup_from(!.Graph, SourceKey, TargetSet),
        (
            AllowOrDisallow = module_allow_deps(AllowSet),
            set.foldl(apply_module_allow_deps(AllowSet, SourceKey),
                TargetSet, !Graph)
        ;
            AllowOrDisallow = module_disallow_deps(DisallowSet),
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
