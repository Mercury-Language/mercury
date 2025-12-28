%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: output_imports_graph.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.output_imports_graph.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module digraph.
:- import_module io.

%---------------------------------------------------------------------------%

:- pred maybe_output_imports_graph(io.text_output_stream::in, globals::in,
    module_name::in, digraph(sym_name)::in, digraph(sym_name)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.file_names.

:- import_module bool.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

maybe_output_imports_graph(ProgressStream, Globals, ModuleName,
        IntDepsGraph, ImpDepsGraph, !IO) :-
    globals.lookup_bool_option(Globals, show_imports_graph, ShowImportsGraph),
    (
        ShowImportsGraph = yes,
        module_name_to_cur_dir_file_name(ext_cur_user_imports_graph,
            ModuleName, ImportsGraphFileName),
        globals.lookup_bool_option(Globals, verbose, Verbose),
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
            digraph.init(DepsGraph0),
            list.foldl(filter_imports_graph,
                digraph.to_assoc_list(IntDepsGraph), DepsGraph0, DepsGraph1),
            list.foldl(filter_imports_graph,
                digraph.to_assoc_list(ImpDepsGraph), DepsGraph1, DepsGraph),
            write_graph(ImportsGraphStream, "imports", DepsGraph, !IO),
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
            report_cannot_open_file_for_output(ProgressStream, Globals,
                ImportsGraphFileName, IOError, !IO)
        )
    ;
        ShowImportsGraph = no
    ).

:- pred filter_imports_graph(pair(sym_name, sym_name)::in,
    digraph(sym_name)::in, digraph(sym_name)::out) is det.

filter_imports_graph(A - B, !DepsGraph) :-
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
        true
    else
        digraph.add_vertices_and_edge(A, B, !DepsGraph)
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
:- end_module parse_tree.output_imports_graph.
%---------------------------------------------------------------------------%
