%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
:- import_module relation, io.

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
        maybe_write_string(VVerbose, "\n\t% Processing Prof.Decl..."),
        process_addr_decl(AddrDeclMap, ProfNodeMap0),
        maybe_write_string(VVerbose, " done.\n\t% Processing Prof.Counts..."),
        process_addr(ProfNodeMap0, ProfNodeMap1, Hertz, ClockTicks,TotalCounts),
        maybe_write_string(VVerbose, " done.\n\t% Processing Prof.CallPair..."),
        process_addr_pair(ProfNodeMap1, DynamicCallGraph, ProfNodeMap),
        maybe_write_string(VVerbose, " done.\n"),

        {Prof = prof(Hertz, ClockTicks, TotalCounts, AddrDeclMap, ProfNodeMap)}.


%-----------------------------------------------------------------------------%


% process_addr_decl:
%	Reads in the addrdecl.out file.
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
                io__stderr_stream(StdErr),
                io__write_strings(StdErr,
			["mprof: error opening declaration file `",
                        DeclFile, "': ", ErrorMsg, "\n"]),

		{ ProfNodeMap = ProfNodeMap0 },
		{ AddrDeclMap = AddrDeclMap0 }
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
% 	Reads in the addr.out file and stores all the counts in the 
% 	prof_node structure.  Also sums the total counts at the same time.
%
:- pred process_addr(prof_node_map, prof_node_map, int, int, int,
							io__state, io__state).
:- mode process_addr(in, out, out, out, out, di, uo) is det.

process_addr(ProfNodeMap0, ProfNodeMap, Hertz, ClockTicks, TotalCounts) -->
	globals__io_lookup_string_option(countfile, CountFile),
	io__see(CountFile, Result),
	(
		{ Result = ok },
		read_int(Hertz),
		read_int(ClockTicks),
		process_addr_2(0, ProfNodeMap0, TotalCounts, ProfNodeMap),
		io__seen
	;
		{ Result = error(Error) },
		{ io__error_message(Error, ErrorMsg) },
		io__stderr_stream(StdErr),
		io__write_strings(StdErr, ["mprof: error opening count file `",
			CountFile, "': ", ErrorMsg, "\n"]),

		{ ProfNodeMap = ProfNodeMap0 },
		{ Hertz = 1 },
		{ ClockTicks = 1 },
		{ TotalCounts = 0 }
	).

:- pred process_addr_2(int, prof_node_map, int, prof_node_map, 
							io__state, io__state).
:- mode process_addr_2(in, in, out, out, di, uo) is det.

process_addr_2(TotalCounts0, ProfNodeMap0, TotalCounts, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_int(Count),

		% XXX This is a just a quick hack to avoid an error
		% with the "0 1" line in the Prof.Counts file.
		% Probably a better fix would be to ensure that 
		% that line never gets generated.
		% But really I have no idea what the right solution is. -fjh.
		( { LabelAddr = 0 } ->
			{ ProfNodeMap1 = ProfNodeMap0 },
			{ TC1 = TotalCounts0 }
		;

			% Add to initial counts.
			lookup_addr(ProfNodeMap0, LabelAddr, ProfNode0),
			{ prof_node_get_initial_counts(ProfNode0, InitCount0) },
			{ InitCount is InitCount0 + Count },
			{ prof_node_set_initial_counts(InitCount, ProfNode0,
							ProfNode) },
			{ map__set(ProfNodeMap0, LabelAddr, ProfNode,
				ProfNodeMap1) },

			{ TC1 is TotalCounts0 + Count }

		),

		process_addr_2(TC1, ProfNodeMap1, TotalCounts, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ ProfNodeMap = ProfNodeMap0 },
		{ TotalCounts = TotalCounts0 }
	).


%-----------------------------------------------------------------------------%


% process_addr_pair:
%	Reads in the addrpair.out file and stores the data in the relevant
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
                io__stderr_stream(StdErr),
                io__write_strings(StdErr, ["mprof: error opening pair file `",
                        PairFile, "': ", ErrorMsg, "\n"]),

		{ DynamicCallGraph = DynamicCallGraph0 },
		{ ProfNodeMap = ProfNodeMap0 }
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
		{ prof_node_concat_to_child(pred_info(CalleeName, Count),
					CallerProfNode0, CallerProfNode) },
		{map__set(ProfNodeMap0, CallerAddr, CallerProfNode, PNodeMap1)},

		% Update the total calls field if not self recursive
		({
			CalleeAddr \= CallerAddr
		->
			prof_node_get_total_calls(CalleeProfNode0, TotalCalls0),
			TotalCalls is TotalCalls0 + Count,
			prof_node_set_total_calls(TotalCalls, CalleeProfNode0,
							CalleeProfNode1),
			prof_node_concat_to_parent(pred_info(CallerName, Count),
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
			relation__add(DynamicCallGraph0, CallerName, 
						CalleeName, DynamicCallGraph1)
		;
			DynamicCallGraph1 = DynamicCallGraph0
		}),
			
		process_addr_pair_2(DynamicCallGraph1, PNodeMap2, Dynamic,
						DynamicCallGraph, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ DynamicCallGraph = DynamicCallGraph0 },
		{ ProfNodeMap = ProfNodeMap0 }
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
