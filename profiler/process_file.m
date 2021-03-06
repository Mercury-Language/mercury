%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-1997,2000, 2004-2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: process_file.m
% Main author: petdr.
%
% Process the files that contain the label declarations, label counts and
% the caller-callee pairs, also builds the dynamic call graph if the option
% set.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module process_file.
:- interface.

:- import_module prof_info.

:- import_module digraph.
:- import_module io.

%---------------------------------------------------------------------------%

:- pred process_profiling_data_files(io.text_output_stream::in,
    io.text_output_stream::in, prof::out, digraph(string)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module read.
:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module unit.

%---------------------------------------------------------------------------%

process_profiling_data_files(ProgressStream, ErrorStream,
        Prof, DynamicCallGraph, !IO) :-
    globals.io_lookup_bool_option(very_verbose, VVerbose, !IO),
    globals.io_lookup_string_option(declfile, DeclFile, !IO),
    globals.io_lookup_string_option(countfile, CountFile, !IO),
    globals.io_lookup_string_option(pairfile, PairFile, !IO),
    % globals.io_lookup_string_option(libraryfile, LibFile, !IO),
    globals.io_lookup_bool_option(dynamic_cg, Dynamic, !IO),

    % process the decl file
    maybe_write_string(ProgressStream, VVerbose, "\n\t% Processing ", !IO),
    maybe_write_string(ProgressStream, VVerbose, DeclFile, !IO),
    maybe_write_string(ProgressStream, VVerbose, "...", !IO),
    process_addr_decl(AddrDeclMap0, ProfNodeMap0, !IO),
    maybe_write_string(ProgressStream, VVerbose, " done.\n", !IO),

    % process the timing counts file
    maybe_write_string(ProgressStream, VVerbose, "\t% Processing ", !IO),
    maybe_write_string(ProgressStream, VVerbose, CountFile, !IO),
    maybe_write_string(ProgressStream, VVerbose, "...", !IO),
    process_addr(ErrorStream, WhatToProfile, Scale, Units, TotalCounts,
        ProfNodeMap0, ProfNodeMap1, !IO),
    maybe_write_string(ProgressStream, VVerbose, " done.\n", !IO),

    % Process the call pair counts file.
    maybe_write_string(ProgressStream, VVerbose, "\t% Processing ", !IO),
    maybe_write_string(ProgressStream, VVerbose, PairFile, !IO),
    maybe_write_string(ProgressStream, VVerbose, "...", !IO),
    process_addr_pair(DynamicCallGraph,
        ProfNodeMap1, ProfNodeMap, AddrDeclMap0, AddrDeclMap, !IO),
    maybe_write_string(ProgressStream, VVerbose, " done.\n", !IO),

    map.init(CycleMap),
    prof_set_entire(Scale, Units, TotalCounts, AddrDeclMap,
        ProfNodeMap, CycleMap, Prof),
    globals.io_get_globals(Globals0, !IO),
    globals.set_what_to_profile(WhatToProfile, Globals0, Globals),
    globals.io_set_globals(Globals, !IO),

    (
        Dynamic = no
        % maybe_write_string(ProgressStream, VVerbose, "\t% Processing "),
        % maybe_write_string(ProgressStream, VVerbose, LibFile),
        % maybe_write_string(ProgressStream, VVerbose, "..."),
        % process_library_callgraph(_, _),
        % maybe_write_string(ProgressStream, VVerbose, " done.\n"),
    ;
        Dynamic = yes
    ).

%---------------------------------------------------------------------------%

    % process_addr_decl(AddrDeclMap, ProfNodeMap, !IO):
    %
    % Reads in the Prof.Decl file.
    % Builds the addrdecl map which associates label names(key)
    % with label addresses.
    % Also builds the prof_node_map which associates label addresses
    % with the prof_node structure.  Initialises and inserts the label name
    % into the structure at the same time.
    %
:- pred process_addr_decl(addrdecl::out, prof_node_map::out,
    io::di, io::uo) is det.

process_addr_decl(AddrDeclMap, ProfNodeMap, !IO) :-
    globals.io_lookup_string_option(declfile, DeclFile, !IO),
    io.open_input(DeclFile, DeclResult, !IO),
    (
        DeclResult = ok(DeclFileStream),
        process_addr_decl_2(DeclFileStream,
            map.init, AddrDeclMap, map.init, ProfNodeMap, !IO),
        io.close_input(DeclFileStream, !IO)
    ;
        DeclResult = error(Error),
        ErrorStr = "error opening declaration file `" ++ DeclFile ++
            "': " ++ io.error_message(Error) ++ "\n",
        error(ErrorStr)
    ).

:- pred process_addr_decl_2(io.text_input_stream::in,
    addrdecl::in, addrdecl::out, prof_node_map::in, prof_node_map::out,
    io::di, io::uo) is det.

process_addr_decl_2(InputStream, !AddrDecl, !ProfNodeMap, !IO) :-
    maybe_read_label_addr(InputStream, MaybeLabelAddr, !IO),
    (
        MaybeLabelAddr = yes(LabelAddr),
        read_label_name(InputStream, LabelName, !IO),
        ProfNode = prof_node_init(LabelName),
        map.det_insert(LabelName, LabelAddr, !AddrDecl),

        % Labels with different names but the same addresses.
        ( if map.insert(LabelAddr, ProfNode, !ProfNodeMap) then
            true
        else
            lookup_addr(LabelAddr, ProfNode0, !AddrDecl, !ProfNodeMap),
            prof_node_concat_to_name_list(LabelName, ProfNode0, NewProfNode),
            map.det_update(LabelAddr, NewProfNode, !ProfNodeMap)
        ),
        process_addr_decl_2(InputStream, !AddrDecl, !ProfNodeMap, !IO)
    ;
        MaybeLabelAddr = no
    ).

%---------------------------------------------------------------------------%

    % process_addr(!ProfNodeMap, WhatToProfile, Scale, Units, TotalCounts,
    %   !IO):
    %
    % Reads in the Prof.Counts file and stores all the counts in the prof_node
    % structure. Also sums the total counts at the same time.
    %
:- pred process_addr(io.text_output_stream::in,
    what_to_profile::out, float::out, string::out, int::out,
    prof_node_map::in, prof_node_map::out, io::di, io::uo) is det.

process_addr(ErrorStream, WhatToProfile, Scale, Units, TotalCounts,
        !ProfNodeMap, !IO) :-
    globals.io_lookup_string_option(countfile, CountFile, !IO),
    io.open_input(CountFile, CountResult, !IO),
    (
        CountResult = ok(CountFileStream),
        read_what_to_profile(CountFileStream, WhatToProfile, !IO),
        read_float(CountFileStream, Scale, !IO),
        read_string(CountFileStream, Units, !IO),
        process_addr_2(CountFileStream, ErrorStream,
            0, TotalCounts, !ProfNodeMap, !IO),
        io.close_input(CountFileStream, !IO)
    ;
        CountResult = error(Error),
        io.error_message(Error, ErrorMsg),
        io.format(ErrorStream, "\nWarning: error opening `%s': %s\n",
            [s(CountFile), s(ErrorMsg)], !IO),
        io.write_string(ErrorStream,
            "The generated profile will only include call counts.\n\n", !IO),
        TotalCounts = 0,
        % We can use any arbitrary values for WhatToProfile and Scale;
        % the values specified here won't be used,
        % since all the times will be zero.
        WhatToProfile = user_plus_system_time,
        Scale = 1.0,
        Units = ""
    ).

:- pred process_addr_2(io.text_input_stream::in, io.text_output_stream::in,
    int::in, int::out, prof_node_map::in, prof_node_map::out,
    io::di, io::uo) is det.

process_addr_2(InputStream, ErrorStream, !TotalCounts, !ProfNodeMap, !IO) :-
    maybe_read_label_addr(InputStream, MaybeLabelAddr, !IO),
    (
        MaybeLabelAddr = yes(LabelAddr),
        read_int(InputStream, Count, !IO),

        % Add to initial counts if we have a ProfNode structure
        % for the address otherwise ignore it.
        ( if map.search(!.ProfNodeMap, LabelAddr, ProfNode0) then
            prof_node_get_initial_counts(ProfNode0, InitCount0),
            InitCount = InitCount0 + Count,
            prof_node_set_initial_counts(InitCount, ProfNode0, ProfNode),
            map.set(LabelAddr, ProfNode, !ProfNodeMap),
            !:TotalCounts = !.TotalCounts + Count
        else
            io.format(ErrorStream,
                "\nWarning address %d not found!  " ++
                "Ignoring address and continuing computation.\n",
                [i(LabelAddr)], !IO)
        ),
        process_addr_2(InputStream, ErrorStream,
            !TotalCounts, !ProfNodeMap, !IO)
    ;
        MaybeLabelAddr = no
    ).

%---------------------------------------------------------------------------%

    % process_addr_pair(!ProfNodeMap, !AddrDecl, DynamicCallGraph, !IO):
    %
    % Reads in the Prof.CallPair file and stores the data in the relevant
    % lists of the prof_node structure.  Also calculates the number of
    % times a predicate is called.
    %
:- pred process_addr_pair(digraph(string)::out,
    prof_node_map::in, prof_node_map::out, addrdecl::in, addrdecl::out,
    io::di, io::uo) is det.

process_addr_pair(DynamicCallGraph, !ProfNodeMap, !AddrDecl, !IO) :-
    globals.io_lookup_bool_option(dynamic_cg, Dynamic, !IO),
    globals.io_lookup_string_option(pairfile, PairFile, !IO),
    io.open_input(PairFile, Result, !IO),
    (
        Result = ok(PairFileStream),
        process_addr_pair_2(PairFileStream, Dynamic,
            digraph.init, DynamicCallGraph, !ProfNodeMap, !AddrDecl, !IO),
        io.close_input(PairFileStream, !IO)
    ;
        Result = error(Error),
        ErrorStr = "error opening pair file `" ++ PairFile  ++
            "': " ++ io.error_message(Error) ++ "\n",
        error(ErrorStr)
    ).

:- pred process_addr_pair_2(io.text_input_stream::in, bool::in,
    digraph(string)::in, digraph(string)::out,
    prof_node_map::in, prof_node_map::out,
    addrdecl::in, addrdecl::out, io::di, io::uo) is det.

process_addr_pair_2(InputStream, Dynamic, !DynamicCallGraph,
        !ProfNodeMap, !AddrDecl, !IO) :-
    maybe_read_label_addr(InputStream, MaybeLabelAddr, !IO),
    (
        MaybeLabelAddr = yes(CallerAddr),
        read_label_addr(InputStream, CalleeAddr, !IO),
        read_int(InputStream, Count, !IO),

        % Get child and parent information.
        lookup_addr(CallerAddr, CallerProfNode0, !AddrDecl, !ProfNodeMap),
        lookup_addr(CalleeAddr, CalleeProfNode0, !AddrDecl, !ProfNodeMap),
        prof_node_get_pred_name(CallerProfNode0, CallerName),
        prof_node_get_pred_name(CalleeProfNode0, CalleeName),

        % Insert child information.
        prof_node_concat_to_child(CalleeName, Count, CallerProfNode0,
            CallerProfNode),
        map.set(CallerAddr, CallerProfNode, !ProfNodeMap),

        % Update the total calls field if not self recursive.
        ( if CalleeAddr = CallerAddr then
            prof_node_set_self_calls(Count, CalleeProfNode0, CalleeProfNode)
        else
            prof_node_get_total_calls(CalleeProfNode0, TotalCalls0),
            TotalCalls = TotalCalls0 + Count,
            prof_node_set_total_calls(TotalCalls, CalleeProfNode0,
                CalleeProfNode1),
            prof_node_concat_to_parent(CallerName, Count,
                CalleeProfNode1, CalleeProfNode)
        ),

        % Insert parent information.
        map.set(CalleeAddr, CalleeProfNode, !ProfNodeMap),

        % Add edge to call graph if generating dynamic call graph.
        (
            Dynamic = yes,
            digraph.add_vertex(CallerName, CallerKey, !DynamicCallGraph),
            digraph.add_vertex(CalleeName, CalleeKey, !DynamicCallGraph),
            digraph.add_edge(CallerKey, CalleeKey, !DynamicCallGraph)
        ;
            Dynamic = no
        ),
        process_addr_pair_2(InputStream, Dynamic, !DynamicCallGraph,
            !ProfNodeMap, !AddrDecl, !IO)
    ;
        MaybeLabelAddr = no
    ).

%---------------------------------------------------------------------------%

    % process_library_callgraph(LibraryATSort, LibPredMap, !IO):
    %
    % XXX
    %
:- pred process_library_callgraph(list(string)::out, map(string, unit)::out,
    io::di, io::uo) is det.

process_library_callgraph(LibraryATSort, LibPredMap, !IO) :-
    globals.io_lookup_string_option(libraryfile, LibFile, !IO),
    LibraryATSort0 = [],
    map.init(LibPredMap0),
    io.open_input(LibFile, Result, !IO),
    (
        Result = ok(LibFileStream),
        process_library_callgraph_2(LibFileStream,
            LibraryATSort0, LibraryATSort, LibPredMap0, LibPredMap, !IO),
        io.close_input(LibFileStream, !IO)
    ;
        Result = error(Error),
        io.error_message(Error, ErrorMsg),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr, "mprof: error opening pair file `%s': %s\n",
            [s(LibFile), s(ErrorMsg)], !IO),
        LibraryATSort = LibraryATSort0,
        LibPredMap = LibPredMap0
    ).

:- pred process_library_callgraph_2(io.text_input_stream::in,
    list(string)::in, list(string)::out,
    map(string, unit)::in, map(string, unit)::out, io::di, io::uo) is det.

process_library_callgraph_2(InputStream, !LibATSort, !LibPredMap, !IO) :-
    maybe_read_label_name(InputStream, MaybeLabelName, !IO),
    (
        MaybeLabelName = yes(LabelName),
        map.det_insert(LabelName, unit, !LibPredMap),
        list.cons(LabelName, !LibATSort),
        process_library_callgraph_2(InputStream, !LibATSort, !LibPredMap, !IO)
    ;
        MaybeLabelName = no
    ).

%---------------------------------------------------------------------------%

    % Attempt to lookup the addr in the prof_node_map, if it does not exist
    % then record the name as unknown__<address> in the relevant data
    % structures.
    %
:- pred lookup_addr(int::in, prof_node::out, addrdecl::in, addrdecl::out,
    prof_node_map::in, prof_node_map::out) is det.

lookup_addr(Addr, ProfNode, !AddrDeclMap, !ProfNodeMap) :-
    ( if map.search(!.ProfNodeMap, Addr, ProfNode0) then
        ProfNode = ProfNode0
    else
        Str = string.format("unknown__%d", [i(Addr)]),
        ProfNode = prof_node_init(Str),
        map.det_insert(Addr, ProfNode, !ProfNodeMap),
        map.det_insert(Str, Addr, !AddrDeclMap)
    ).

%---------------------------------------------------------------------------%
:- end_module process_file.
%---------------------------------------------------------------------------%
