%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
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

:- pred process_file__main(prof, relation(string), io__state, io__state).
:- mode process_file__main(out, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module read.
:- import_module globals, options.
:- import_module bool, int, require, std_util, string.
:- import_module list, map.


process_file__main(Prof, DynamicCallGraph) -->
	globals__io_lookup_bool_option(very_verbose, VVerbose),
	globals__io_lookup_string_option(declfile, DeclFile),
	globals__io_lookup_string_option(countfile, CountFile),
	globals__io_lookup_string_option(pairfile, PairFile),
	% globals__io_lookup_string_option(libraryfile, LibFile),
	globals__io_lookup_bool_option(dynamic_cg, Dynamic),

	% process the decl file
	maybe_write_string(VVerbose, "\n\t% Processing "),
	maybe_write_string(VVerbose, DeclFile),
	maybe_write_string(VVerbose, "..."),
	process_addr_decl(AddrDeclMap, ProfNodeMap0),
	maybe_write_string(VVerbose, " done.\n"),

	% process the timing counts file
	maybe_write_string(VVerbose, "\t% Processing "),
	maybe_write_string(VVerbose, CountFile),
	maybe_write_string(VVerbose, "..."),
	process_addr(ProfNodeMap0, ProfNodeMap1, WhatToProfile, Scale, Units,
		TotalCounts),
	maybe_write_string(VVerbose, " done.\n"),

	% process the call pair counts file
	maybe_write_string(VVerbose, "\t% Processing "),
	maybe_write_string(VVerbose, PairFile),
	maybe_write_string(VVerbose, "..."),
	process_addr_pair(ProfNodeMap1, DynamicCallGraph, ProfNodeMap),
	maybe_write_string(VVerbose, " done.\n"),

	{ map__init(CycleMap) },
	{ prof_set_entire(Scale, Units, TotalCounts, AddrDeclMap, 
						ProfNodeMap, CycleMap, Prof) },
	globals__io_get_globals(Globals0),
	{ globals__set_what_to_profile(Globals0, WhatToProfile, Globals) },
	globals__io_set_globals(Globals),
	
	(
		{ Dynamic = no }
	->
		% maybe_write_string(VVerbose, "\t% Processing "),
		% maybe_write_string(VVerbose, LibFile),
		% maybe_write_string(VVerbose, "..."),
		% process_library_callgraph(_, _),
		% maybe_write_string(VVerbose, " done.\n"),
		{ true }

	;
		{ true }
	).


%-----------------------------------------------------------------------------%


% process_addr_decl:
%	Reads in the Prof.Decl file.
%	Builds the addrdecl map which associates label names(key) with 
%	label addresses.
%	Also builds the prof_node_map which associates label addresses with
%	the prof_node structure.  Initialises and inserts the label name into
%	the structure at the same time.
%
:- pred process_addr_decl(addrdecl, prof_node_map, io__state, io__state).
:- mode process_addr_decl(out, out, di, uo) is det.

process_addr_decl(AddrDeclMap, ProfNodeMap) -->
	{ map__init(AddrDeclMap0) },
	{ map__init(ProfNodeMap0) },
	globals__io_lookup_string_option(declfile, DeclFile),
	io__see(DeclFile, Result),
	(
		{ Result = ok },
		process_addr_decl_2(AddrDeclMap0, ProfNodeMap0, AddrDeclMap, 
								ProfNodeMap),
		io__seen
	;
		{ Result = error(Error) },
		{ io__error_message(Error, ErrorMsg) },

		{ string__append("error opening declaration file `", DeclFile, 
					Str0) },
		{ string__append(Str0, "': ", Str1) },
		{ string__append(Str1, ErrorMsg, Str2) },
		{ string__append(Str2, "\n", ErrorStr) },
		{ error(ErrorStr) }
	).

:- pred process_addr_decl_2(addrdecl, prof_node_map, addrdecl, prof_node_map,
							io__state, io__state).
:- mode process_addr_decl_2(in, in, out, out, di, uo) is det.

process_addr_decl_2(AddrDecl0, ProfNodeMap0, AddrDecl, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_label_name(LabelName),
		{ prof_node_init(LabelName, ProfNode) },
		{ map__det_insert(AddrDecl0, LabelName, LabelAddr, AddrDecl1) },

		% Labels with different names but the same addresses.
		( 
			{ map__insert(ProfNodeMap0, LabelAddr, ProfNode, 
								ProfNodeMap1) }
		->
			{ ProfNodeMap2 = ProfNodeMap1 }
		;
			lookup_addr(ProfNodeMap0, LabelAddr, ProfNode0),
			{ prof_node_concat_to_name_list(LabelName, ProfNode0,
								NewProfNode) },
			{ map__det_update(ProfNodeMap0, LabelAddr, NewProfNode,
								ProfNodeMap2) }
		),
		process_addr_decl_2(AddrDecl1, ProfNodeMap2, AddrDecl, 
								ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ AddrDecl = AddrDecl0 },
		{ ProfNodeMap = ProfNodeMap0 }
	).
		

%-----------------------------------------------------------------------------%


% process_addr:
% 	Reads in the Prof.Counts file and stores all the counts in the 
% 	prof_node structure.  Also sums the total counts at the same time.
%
:- pred process_addr(prof_node_map, prof_node_map,
		what_to_profile, float, string, int, io__state, io__state).
:- mode process_addr(in, out, out, out, out, out, di, uo) is det.

process_addr(ProfNodeMap0, ProfNodeMap, WhatToProfile, Scale, Units,
		TotalCounts) -->
	globals__io_lookup_string_option(countfile, CountFile),
	io__see(CountFile, Result),
	(
		{ Result = ok },
		read_what_to_profile(WhatToProfile),
		read_float(Scale),
		read_string(Units),
		process_addr_2(0, ProfNodeMap0, TotalCounts, ProfNodeMap),
		io__seen
	;
		{ Result = error(Error) },
		{ io__error_message(Error, ErrorMsg) },
		io__write_string("\nWarning: error opening `"),
		io__write_string(CountFile),
		io__write_string("': "),
		io__write_string(ErrorMsg),
		io__write_string("\n"),
		io__write_string("The generated profile will only include "),
		io__write_string("call counts.\n\n"),
		{ TotalCounts = 0 },
		{ ProfNodeMap = ProfNodeMap0 },
		% We can use any arbitrary values for WhatToProfile and Scale;
		% the values specified here won't be used,
		% since all the times will be zero.
		{ WhatToProfile = user_plus_system_time },
		{ Scale = 1.0 },
		{ Units = "" }
	).

:- pred process_addr_2(int, prof_node_map, int, prof_node_map, 
							io__state, io__state).
:- mode process_addr_2(in, in, out, out, di, uo) is det.

process_addr_2(TotalCounts0, ProfNodeMap0, TotalCounts, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_int(Count),

		% Add to initial counts if we have a ProfNode structure
		% for the address otherwise ignore it.
		(
			{map__search(ProfNodeMap0,LabelAddr, ProfNode0)}
		->
			{ prof_node_get_initial_counts(ProfNode0, 
							InitCount0) },
			{ InitCount is InitCount0 + Count },
			{ prof_node_set_initial_counts(InitCount, 
						ProfNode0, ProfNode) },
			{ map__set(ProfNodeMap0, LabelAddr, ProfNode,
							ProfNodeMap1) },
			{ TC1 is TotalCounts0 + Count }
		;
			{ TC1 = TotalCounts0 },
			{ ProfNodeMap1 = ProfNodeMap0 },
			{ string__format("\nWarning address %d not found!  Ignoring address and continuing computation.\n", [ i(LabelAddr) ], String) },
			io__write_string(String)
		),

		process_addr_2(TC1, ProfNodeMap1, TotalCounts, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ ProfNodeMap = ProfNodeMap0 },
		{ TotalCounts = TotalCounts0 }
	).


%-----------------------------------------------------------------------------%


% process_addr_pair:
%	Reads in the Prof.CallPair file and stores the data in the relevant
%	lists of the prof_node structure.  Also calculates the number of times
%	a predicate is called.
%
:- pred process_addr_pair(prof_node_map, relation(string), prof_node_map, 
							io__state, io__state).
:- mode process_addr_pair(in, out, out, di, uo) is det.

process_addr_pair(ProfNodeMap0, DynamicCallGraph, ProfNodeMap) -->
	{ relation__init(DynamicCallGraph0) },
	globals__io_lookup_bool_option(dynamic_cg, Dynamic),
	globals__io_lookup_string_option(pairfile, PairFile),
	io__see(PairFile, Result),
	(
		{ Result = ok },
		process_addr_pair_2(DynamicCallGraph0, ProfNodeMap0, Dynamic,
						DynamicCallGraph, ProfNodeMap),
		io__seen
	;
		{ Result = error(Error) },
		{ io__error_message(Error, ErrorMsg) },
		{ string__append("error opening pair file `", PairFile, 
					Str0) },
		{ string__append(Str0, "': ", Str1) },
		{ string__append(Str1, ErrorMsg, Str2) },
		{ string__append(Str2, "\n", ErrorStr) },
		{ error(ErrorStr) }
	).

:- pred process_addr_pair_2(relation(string), prof_node_map, bool, 
			relation(string), prof_node_map, io__state, io__state).
:- mode process_addr_pair_2(in, in, in, out, out, di, uo) is det.

process_addr_pair_2(DynamicCallGraph0, ProfNodeMap0, Dynamic, DynamicCallGraph,
								ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(CallerAddr) },
		read_label_addr(CalleeAddr),
		read_int(Count),

		% Get child and parent information
		lookup_addr(ProfNodeMap0, CallerAddr, CallerProfNode0),
		lookup_addr(ProfNodeMap0, CalleeAddr, CalleeProfNode0),
		{ prof_node_get_pred_name(CallerProfNode0, CallerName) },
		{ prof_node_get_pred_name(CalleeProfNode0, CalleeName) },

		% Insert child information

		{ prof_node_concat_to_child(CalleeName, Count, CallerProfNode0,
							CallerProfNode) },
		{map__set(ProfNodeMap0, CallerAddr, CallerProfNode, PNodeMap1)},

		% Update the total calls field if not self recursive
		({
			CalleeAddr \= CallerAddr
		->
			prof_node_get_total_calls(CalleeProfNode0, TotalCalls0),
			TotalCalls is TotalCalls0 + Count,
			prof_node_set_total_calls(TotalCalls, CalleeProfNode0,
							CalleeProfNode1),
			prof_node_concat_to_parent(CallerName, Count,
					CalleeProfNode1, CalleeProfNode)
		;
			prof_node_set_self_calls(Count, CalleeProfNode0, 
							CalleeProfNode)
		}),

		% Insert parent information
		{ map__set(PNodeMap1, CalleeAddr, CalleeProfNode, PNodeMap2) },

		% Add edge to call graph if generating dynamic call graph.
		({
			Dynamic = yes
		->
			relation__add_element(DynamicCallGraph0,
				CallerName, CallerKey, DynamicCallGraph1),
			relation__add_element(DynamicCallGraph1,
				CalleeName, CalleeKey, DynamicCallGraph2),
			relation__add(DynamicCallGraph2, CallerKey, 
				CalleeKey, DynamicCallGraph99)
		;
			DynamicCallGraph99 = DynamicCallGraph0
		}),
			
		process_addr_pair_2(DynamicCallGraph99, PNodeMap2, Dynamic,
						DynamicCallGraph, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ DynamicCallGraph = DynamicCallGraph0 },
		{ ProfNodeMap = ProfNodeMap0 }
	).


%-----------------------------------------------------------------------------%

% process_library_callgraph:
%	XXX
%
:- pred process_library_callgraph(list(string), map(string, unit), 
							io__state, io__state).
:- mode process_library_callgraph(out, out, di, uo) is det.

process_library_callgraph(LibraryATSort, LibPredMap) -->
	globals__io_lookup_string_option(libraryfile, LibFile),
	{ map__init(LibPredMap0) },
	io__see(LibFile, Result),
	(
		{ Result = ok },
		process_library_callgraph_2([], LibraryATSort, LibPredMap0, 
								LibPredMap),
		io__seen
	;
		{ Result = error(Error) },
		{ io__error_message(Error, ErrorMsg) },
		io__stderr_stream(StdErr),
		io__write_strings(StdErr, ["mprof: error opening pair file `",
			LibFile, "': ", ErrorMsg, "\n"]),
		{ LibraryATSort = [] },
		{ LibPredMap = LibPredMap0 }
	).

:- pred process_library_callgraph_2(list(string), list(string), 
		map(string, unit), map(string, unit), io__state, io__state).
:- mode process_library_callgraph_2(in, out, in, out, di, uo) is det.

process_library_callgraph_2(LibATSort0, LibATSort, LibPredMap0, LibPredMap) -->
	maybe_read_label_name(MaybeLabelName),
	(
		{ MaybeLabelName = yes(LabelName) },

		{ map__det_insert(LibPredMap0, LabelName, unit, LibPredMap1) },
		{ LibATSort1 = [ LabelName | LibATSort0 ] },

		process_library_callgraph_2(LibATSort1, LibATSort, LibPredMap1,
								LibPredMap)
	;
		{ MaybeLabelName = no },

		{ LibPredMap = LibPredMap0 },
		{ LibATSort = LibATSort0 }
	).

%-----------------------------------------------------------------------------%

% Utility functions so as to replace the lookup functions 

:- pred lookup_addr(prof_node_map, int, prof_node, io__state, io__state).
:- mode lookup_addr(in, in, out, di, uo) is det.

lookup_addr(ProfNodeMap, Addr, ProfNode) -->
	(
		{ map__search(ProfNodeMap, Addr, ProfNode0) }
	->
		{ ProfNode = ProfNode0 }
	;
		{ string__format("\nKey = %d\n", [ i(Addr) ], String) },
		io__write_string(String),
		{ error("map__lookup: key not found\n") }
	).
