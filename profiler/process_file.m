%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997,2000, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% process_file.m
%
% Main author: petdr.
%
% Processs the files that contain the label declarations, label counts and
% the caller-callee pairs, also builds the dynamic call graph if the option
% set.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module process_file.

:- interface.

:- import_module prof_info.
:- import_module io, relation.

:- pred process_file__main(prof::out, relation(string)::out, io::di, io::uo)
	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module read.
:- import_module globals, options.
:- import_module bool, int, require, std_util, string.
:- import_module list, map.

%-----------------------------------------------------------------------------%

process_file__main(Prof, DynamicCallGraph, !IO) :-
	globals__io_lookup_bool_option(very_verbose, VVerbose, !IO),
	globals__io_lookup_string_option(declfile, DeclFile, !IO),
	globals__io_lookup_string_option(countfile, CountFile, !IO),
	globals__io_lookup_string_option(pairfile, PairFile, !IO),
	% globals__io_lookup_string_option(libraryfile, LibFile, !IO),
	globals__io_lookup_bool_option(dynamic_cg, Dynamic, !IO),

	% process the decl file
	maybe_write_string(VVerbose, "\n\t% Processing ", !IO),
	maybe_write_string(VVerbose, DeclFile, !IO),
	maybe_write_string(VVerbose, "...", !IO),
	process_addr_decl(AddrDeclMap0, ProfNodeMap0, !IO),
	maybe_write_string(VVerbose, " done.\n", !IO),

	% process the timing counts file
	maybe_write_string(VVerbose, "\t% Processing ", !IO),
	maybe_write_string(VVerbose, CountFile, !IO),
	maybe_write_string(VVerbose, "...", !IO),
	process_addr(ProfNodeMap0, ProfNodeMap1, WhatToProfile, Scale, Units,
		TotalCounts, !IO),
	maybe_write_string(VVerbose, " done.\n", !IO),

	% process the call pair counts file
	maybe_write_string(VVerbose, "\t% Processing ", !IO),
	maybe_write_string(VVerbose, PairFile, !IO),
	maybe_write_string(VVerbose, "...", !IO),
	process_addr_pair(ProfNodeMap1, ProfNodeMap,
		AddrDeclMap0, AddrDeclMap, DynamicCallGraph, !IO),
	maybe_write_string(VVerbose, " done.\n", !IO),

	map__init(CycleMap),
	prof_set_entire(Scale, Units, TotalCounts, AddrDeclMap, 
		ProfNodeMap, CycleMap, Prof),
	globals__io_get_globals(Globals0, !IO),
	globals__set_what_to_profile(Globals0, WhatToProfile, Globals),
	globals__io_set_globals(Globals, !IO),
	
	( Dynamic = no ->
		% maybe_write_string(VVerbose, "\t% Processing "),
		% maybe_write_string(VVerbose, LibFile),
		% maybe_write_string(VVerbose, "..."),
		% process_library_callgraph(_, _),
		% maybe_write_string(VVerbose, " done.\n"),
		true 
	;
		true
	).

%-----------------------------------------------------------------------------%

	% process_addr_decl:
	% Reads in the Prof.Decl file.
	% Builds the addrdecl map which associates label names(key)
	% with label addresses.
	% Also builds the prof_node_map which associates label addresses
	% with the prof_node structure.  Initialises and inserts the
	% label name into the structure at the same time.
	%
:- pred process_addr_decl(addrdecl::out, prof_node_map::out,
	io::di, io::uo) is det.

process_addr_decl(AddrDeclMap, ProfNodeMap, !IO) :-
	globals__io_lookup_string_option(declfile, DeclFile, !IO),
	io__see(DeclFile, Result, !IO),
	(
		Result = ok,
		process_addr_decl_2(map.init, AddrDeclMap, map.init, 
			ProfNodeMap, !IO),
		io__seen(!IO)
	;
		Result = error(Error),
		ErrorStr = "error opening declaration file `" ++ DeclFile ++
			"': " ++ io.error_message(Error) ++ "\n",
		error(ErrorStr)
	).

:- pred process_addr_decl_2(addrdecl::in, addrdecl::out, prof_node_map::in,
	prof_node_map::out, io::di, io::uo) is det.

process_addr_decl_2(!AddrDecl, !ProfNodeMap, !IO) :-
	maybe_read_label_addr(MaybeLabelAddr, !IO),
	( MaybeLabelAddr = yes(LabelAddr) ->
		read_label_name(LabelName, !IO),
		ProfNode = prof_node_init(LabelName),
		map__det_insert(!.AddrDecl, LabelName, LabelAddr, !:AddrDecl),

		% Labels with different names but the same addresses.
		( 
			map__insert(!.ProfNodeMap, LabelAddr, ProfNode, 
				!:ProfNodeMap)
		->
			true
		;
			lookup_addr(LabelAddr, ProfNode0, !AddrDecl,
				!ProfNodeMap),
			prof_node_concat_to_name_list(LabelName, ProfNode0,
				NewProfNode),
			map__det_update(!.ProfNodeMap, LabelAddr, NewProfNode,
				!:ProfNodeMap)
		),
		process_addr_decl_2(!AddrDecl, !ProfNodeMap, !IO) 
	;
		true
	).
		

%-----------------------------------------------------------------------------%

	% process_addr:
	% Reads in the Prof.Counts file and stores all the counts in the 
	% prof_node structure.  Also sums the total counts at the same time.
	%
:- pred process_addr(prof_node_map::in, prof_node_map::out,
	what_to_profile::out, float::out, string::out, int::out,
	io::di, io::uo) is det.

process_addr(!ProfNodeMap, WhatToProfile, Scale, Units, TotalCounts, !IO) :-
	globals__io_lookup_string_option(countfile, CountFile, !IO),
	io__see(CountFile, Result, !IO),
	(
		Result = ok,
		read_what_to_profile(WhatToProfile, !IO),
		read_float(Scale, !IO),
		read_string(Units, !IO),
		process_addr_2(0, TotalCounts, !ProfNodeMap, !IO),
		io__seen(!IO)
	;
		Result = error(Error),
		io__error_message(Error, ErrorMsg),
		io__write_string("\nWarning: error opening `", !IO),
		io__write_string(CountFile, !IO),
		io__write_string("': ", !IO),
		io__write_string(ErrorMsg, !IO),
		io__write_string("\n", !IO),
		io__write_string("The generated profile will only include ", !IO),
		io__write_string("call counts.\n\n", !IO),
		TotalCounts = 0,
		% We can use any arbitrary values for WhatToProfile and Scale;
		% the values specified here won't be used,
		% since all the times will be zero.
		WhatToProfile = user_plus_system_time,
		Scale = 1.0,
		Units = ""
	).

:- pred process_addr_2(int::in, int::out,
	prof_node_map::in, prof_node_map::out, io::di, io::uo) is det.

process_addr_2(!TotalCounts, !ProfNodeMap, !IO) :-
	maybe_read_label_addr(MaybeLabelAddr, !IO),
	( MaybeLabelAddr = yes(LabelAddr) ->
		read_int(Count, !IO),

		% Add to initial counts if we have a ProfNode structure
		% for the address otherwise ignore it.
		(
			map__search(!.ProfNodeMap, LabelAddr, ProfNode0)
		->
			prof_node_get_initial_counts(ProfNode0, 
				InitCount0),
			InitCount = InitCount0 + Count,
			prof_node_set_initial_counts(InitCount, 
				ProfNode0, ProfNode),
			map__set(!.ProfNodeMap, LabelAddr, ProfNode,
				!:ProfNodeMap),
			!:TotalCounts = !.TotalCounts + Count
		;
			io__format("\nWarning address " ++
				"%d not found!  Ignoring address and " ++
				"continuing computation.\n", 
				[i(LabelAddr)], !IO)
		),
		process_addr_2(!TotalCounts, !ProfNodeMap, !IO)
	;
		true
	).

%-----------------------------------------------------------------------------%

	% process_addr_pair:
	% Reads in the Prof.CallPair file and stores the data in the relevant
	% lists of the prof_node structure.  Also calculates the number of
	% times a predicate is called.
	%
:- pred process_addr_pair(prof_node_map::in, prof_node_map::out,
	addrdecl::in, addrdecl::out, relation(string)::out, io::di, io::uo)
	is det.

process_addr_pair(!ProfNodeMap, !AddrDecl, DynamicCallGraph, !IO) :-
	globals__io_lookup_bool_option(dynamic_cg, Dynamic, !IO),
	globals__io_lookup_string_option(pairfile, PairFile, !IO),
	io__see(PairFile, Result, !IO),
	(
		Result = ok,
		process_addr_pair_2(Dynamic, relation.init, DynamicCallGraph,
			!ProfNodeMap, !AddrDecl, !IO),
		io__seen(!IO)
	;
		Result = error(Error),
		ErrorStr = "error opening pair file `" ++ PairFile  ++
			"': " ++ io.error_message(Error) ++ "\n",
		error(ErrorStr)
	).

:- pred process_addr_pair_2(bool::in,
	relation(string)::in, relation(string)::out,
	prof_node_map::in, prof_node_map::out,
	addrdecl::in, addrdecl::out, io::di, io::uo) is det.

process_addr_pair_2(Dynamic, !DynamicCallGraph, !ProfNodeMap, !AddrDecl,
		!IO) :-
	maybe_read_label_addr(MaybeLabelAddr, !IO),
	( MaybeLabelAddr = yes(CallerAddr) ->
		read_label_addr(CalleeAddr, !IO),
		read_int(Count, !IO),

		% Get child and parent information
		lookup_addr(CallerAddr, CallerProfNode0, !AddrDecl,
			!ProfNodeMap),
		lookup_addr(CalleeAddr, CalleeProfNode0, !AddrDecl,
			!ProfNodeMap),
		prof_node_get_pred_name(CallerProfNode0, CallerName),
		prof_node_get_pred_name(CalleeProfNode0, CalleeName),

		% Insert child information

		prof_node_concat_to_child(CalleeName, Count, CallerProfNode0,
			CallerProfNode),
		map__set(!.ProfNodeMap, CallerAddr, CallerProfNode,
			!:ProfNodeMap),

		% Update the total calls field if not self recursive
		( CalleeAddr \= CallerAddr ->
			prof_node_get_total_calls(CalleeProfNode0, TotalCalls0),
			TotalCalls = TotalCalls0 + Count,
			prof_node_set_total_calls(TotalCalls, CalleeProfNode0,
				CalleeProfNode1),
			prof_node_concat_to_parent(CallerName, Count,
				CalleeProfNode1, CalleeProfNode)
		;
			prof_node_set_self_calls(Count, CalleeProfNode0, 
				CalleeProfNode)
		),

		% Insert parent information
		map__set(!.ProfNodeMap, CalleeAddr, CalleeProfNode, !:ProfNodeMap),

		% Add edge to call graph if generating dynamic call graph.
		( Dynamic = yes ->
			relation__add_element(!.DynamicCallGraph,
				CallerName, CallerKey, !:DynamicCallGraph),
			relation__add_element(!.DynamicCallGraph,
				CalleeName, CalleeKey, !:DynamicCallGraph),
			relation__add(!.DynamicCallGraph, CallerKey, 
				CalleeKey, !:DynamicCallGraph)
		;
			true
		),
		process_addr_pair_2(Dynamic, !DynamicCallGraph, !ProfNodeMap,
			!AddrDecl, !IO) 	
	;
		true
	).


%-----------------------------------------------------------------------------%

% process_library_callgraph:
%	XXX
%
%

:- pred process_library_callgraph(list(string)::out, map(string, unit)::out,
	io::di, io::uo) is det.

process_library_callgraph(LibraryATSort, LibPredMap, !IO) :-
	globals__io_lookup_string_option(libraryfile, LibFile, !IO),
	map__init(LibPredMap0),
	io__see(LibFile, Result, !IO),
	(
		Result = ok,
		process_library_callgraph_2([], LibraryATSort, LibPredMap0, 
			LibPredMap, !IO),
		io__seen(!IO)
	;
		Result = error(Error),
		io__error_message(Error, ErrorMsg),
		io__stderr_stream(StdErr, !IO),
		io__write_strings(StdErr, ["mprof: error opening pair file `",
			LibFile, "': ", ErrorMsg, "\n"], !IO),
		LibraryATSort = [],
		LibPredMap = LibPredMap0
	).

:- pred process_library_callgraph_2(list(string)::in, list(string)::out,
	map(string, unit)::in, map(string, unit)::out, io::di, io::uo) is det.

process_library_callgraph_2(!LibATSort, !LibPredMap, !IO) :-
	maybe_read_label_name(MaybeLabelName, !IO),
	( MaybeLabelName = yes(LabelName) ->
		map__det_insert(!.LibPredMap, LabelName, unit, !:LibPredMap),
		list__cons(LabelName, !LibATSort),
		process_library_callgraph_2(!LibATSort, !LibPredMap, !IO)
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Attempt to lookup the addr in the prof_node_map, if it 
	% does not exist then record the name as unknown__<address>
	% in the relevant data structures.
	%
:- pred lookup_addr(int::in, prof_node::out, addrdecl::in, addrdecl::out,
	prof_node_map::in, prof_node_map::out) is det.

lookup_addr(Addr, ProfNode, !AddrDeclMap, !ProfNodeMap) :-
	( map__search(!.ProfNodeMap, Addr, ProfNode0) ->
		ProfNode = ProfNode0
	;
		Str = string__format("unknown__%d", [i(Addr)]),
		ProfNode = prof_node_init(Str),
		map__det_insert(!.ProfNodeMap, Addr, ProfNode, !:ProfNodeMap),
		map__det_insert(!.AddrDeclMap, Str, Addr, !:AddrDeclMap)
	).

%-----------------------------------------------------------------------------%
:- end_module process_file.
%-----------------------------------------------------------------------------%
