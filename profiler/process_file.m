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
% Process's the file's that contain the label declarations, label counts and
% the caller-callee pairs, also build's the dynamic call graph if the option
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
:- import_module int, require, std_util, string.
:- import_module list, map.


process_file__main(Prof, DynamicCallGraph) -->
        globals__io_lookup_bool_option(very_verbose, VVerbose),
        maybe_write_string(VVerbose, "\n\t% Processing addrdecl.out..."),
        process_addr_decl(AddrDeclMap, ProfNodeMap0),
        maybe_write_string(VVerbose, " done.\n\t% Processing addr.out..."),
        process_addr(ProfNodeMap0, ProfNodeMap1, Hertz, ClockTicks,TotalCounts),
        maybe_write_string(VVerbose, " done.\n\t% Processing addrpair.out..."),
        process_addr_pair(ProfNodeMap1, DynamicCallGraph, ProfNodeMap),
        maybe_write_string(VVerbose, " done.\n"),

        {Prof = prof(Hertz, ClockTicks, TotalCounts, AddrDeclMap, ProfNodeMap)}.


%-----------------------------------------------------------------------------%


% process_addr_decl:
%	Read's in the addrdecl.out file.
%	Build's the addrdecl map which associate's label names(key) with 
%	label address's.
%	Also builds the prof_node_map which associate's label address's with
%	the prof_node structure.  Initialises and insert's the label name into
%	the structure at the same time.
%
:- pred process_addr_decl(addrdecl, prof_node_map, io__state, io__state).
:- mode process_addr_decl(out, out, di, uo) is det.

process_addr_decl(AddrDeclMap, ProfNodeMap) -->
	globals__io_lookup_string_option(declfile, DeclFile),
	io__see(DeclFile, Result),
	(
		{ Result = ok }
	->
		{ map__init(AddrDeclMap0) },
		{ map__init(ProfNodeMap0) },
		process_addr_decl_2(AddrDeclMap0, ProfNodeMap0, AddrDeclMap, 
								ProfNodeMap),
		io__seen
	;
		{ error("process_addr_decl: Couldn't open declaration file\n") }
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

		% Label's with different name's but the same addresses.
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
% 	Read's in the addr.out file and store's all the count's in the 
% 	prof_node structure.  Also sum's the total count's at the same time.
%
:- pred process_addr(prof_node_map, prof_node_map, int, int, int,
							io__state, io__state).
:- mode process_addr(in, out, out, out, out, di, uo) is det.

process_addr(ProfNodeMap0, ProfNodeMap, Hertz, ClockTicks, TotalCounts) -->
	globals__io_lookup_string_option(countfile, CountFile),
	io__see(CountFile, Result),
	(
		{ Result = ok }
	->
		read_int(Hertz),
		read_int(ClockTicks),
		process_addr_2(0, ProfNodeMap0, TotalCounts, ProfNodeMap),
		io__seen
	;
		{ error("process_addr: Couldn't open count file\n") }
	).


:- pred process_addr_2(int, prof_node_map, int, prof_node_map, 
							io__state, io__state).
:- mode process_addr_2(in, in, out, out, di, uo) is det.

process_addr_2(TotalCounts0, ProfNodeMap0, TotalCounts, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_int(Count),

		% Add to initial counts.
		lookup_addr(ProfNodeMap0, LabelAddr, ProfNode0),
		{ prof_node_get_initial_counts(ProfNode0, InitCount0) },
		{ InitCount is InitCount0 + Count },
		{ prof_node_set_initial_counts(InitCount, ProfNode0, ProfNode) },
		{ map__set(ProfNodeMap0, LabelAddr, ProfNode, ProfNodeMap1) },

		{ TC1 is TotalCounts0 + Count },

		process_addr_2(TC1, ProfNodeMap1, TotalCounts, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ ProfNodeMap = ProfNodeMap0 },
		{ TotalCounts = TotalCounts0 }
	).


%-----------------------------------------------------------------------------%


% process_addr_pair:
%	Read's in the addrpair.out file and store's the data in the relevant
%	list's of the prof_node structure.  Also calculate's the number of times
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
		{ Result = ok }
	->
		process_addr_pair_2(DynamicCallGraph0, ProfNodeMap0, Dynamic,
						DynamicCallGraph, ProfNodeMap),
		io__seen
	;
		{ error("process_addr_pair: Couldn't open pair file\n") }
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

		% Update the total call's field if not self recursive
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
