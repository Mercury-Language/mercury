%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Mercury profiler
% Main author: petdr.
%
% Notes:
%	Process's the *.out and *.prof files to produce an output similar to
%	GPROF
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mercury_profile.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prof_debug.
:- import_module read.
:- import_module float, int, list, map, string, set.
:- import_module getopt, options, globals.
:- import_module std_util, require.
:- import_module relation.


	% prof: Data structure which contains ALL the relevant info for use
	%	in generating the output.
:- type prof --->
		prof(
			int,			% Hertz of the system clock
			int,			% Total count's of the profile
						% run
			addrdecl,		% Map between label name and
						% label addr used to find key
						% to look up prof_node_map.
			prof_node_map		% Map between label address's
						% and all the relevant data 
						% about that predicate
		).

	% addrdecl = map(label_name, label_address)
:- type addrdecl	==	map(string, int).

	% prof_node_map = map(label_address, prof_node)
:- type prof_node_map	==	map(int, prof_node).

	% prof_node: Contains all the info used for output, for a single pred.
:- type prof_node 	---> 
		prof_node(
			string, 		% current predicate (label)
			int,			% index number
			int, 			% self counts
			float, 			% propogated counts
			list(pred_info), 	% Parent pred and the number
						% of time's it call's this
						% predicate
			list(pred_info),	% Child pred and the number of
						% time's they are called from
						% this predicate.
			int,			% total count of time's this 
						% predicate called.
			int,			% Number of self recursive 
						% calls of this routine
			list(string)		% Alternative names for this
						% predicate eg. Label's with
						% different names but the same
						% address.
		).

:- type pred_info --->
		pred_info(
			string,			% predicate (label)     
			int			% count     (to or from)
		).

:- type junk		==	int.


%-----------------------------------------------------------------------------%


main -->
	io__command_line_arguments(Args0),
	{ getopt__process_options(Args0, Args, Result0) },
	postprocess_options(Result0, Result),
	main_2(Result, Args).


	% XXX Need's to be expanded so as to process all the options.
:- pred postprocess_options(maybe_option_table, maybe(string),
	io__state, io__state).
:- mode postprocess_options(in, out, di, uo) is det.

postprocess_options(error(ErrorMessage), yes(ErrorMessage)) --> [].
postprocess_options(ok(OptionTable), no) --> 
	globals__io_init(OptionTable),
	% --very-verbose implies --verbose
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	(
		{ VeryVerbose = yes }
	->
		 globals__io_set_option(verbose, bool(yes))
	;
		[]
	).


        % Display error message and then usage message
:- pred usage_error(string::in, io__state::di, io__state::uo) is det.
usage_error(ErrorMessage) -->
        io__progname_base("mercury_profile", ProgName),
        io__stderr_stream(StdErr),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, ": "),
        io__write_string(StdErr, ErrorMessage),
        io__write_string(StdErr, "\n"),
        io__set_exit_status(1),
        usage.


        % Display usage message
:- pred usage(io__state::di, io__state::uo) is det.
usage -->
        io__progname_base("mprof", ProgName),
        io__stderr_stream(StdErr),
        io__write_string(StdErr, "Mercury Profiler, version 0.3\n"),
        io__write_string(StdErr, "Copyright (C) 1995 University of Melbourne\n"),
        io__write_string(StdErr, "Usage: "),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " [<options>]\n"),
        io__write_string(StdErr, "Use `"),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " --help' for more information.\n").

:- pred long_usage(io__state::di, io__state::uo) is det.
long_usage -->
        io__progname_base("mprof", ProgName),
        io__write_string("Mercury Profiler, version 0.3\n"),
        io__write_string("Copyright (C) 1995 University of Melbourne\n"),
        io__write_string("Usage: "),
        io__write_string(ProgName),
        io__write_string(" [<options>]\n"),
        io__write_string("Options:\n"),
        options_help.


%-----------------------------------------------------------------------------%


:- pred main_2(maybe(string), list(string), io__state, io__state).
:- mode main_2(in, in, di, uo) is det.

main_2(yes(ErrorMessage), _) -->
        usage_error(ErrorMessage).
main_2(no, Args) -->
        globals__io_lookup_bool_option(help, Help),
        (
                { Help = yes }
        ->
                long_usage
        ;
		globals__io_lookup_bool_option(verbose, Verbose),

		maybe_write_string(Verbose, "% Processing input files..."),
		process_addr_files(Prof0),
		maybe_write_string(Verbose, " done\n"),
		
		maybe_write_string(Verbose, "% Building call graph..."),
		build_call_graph(Args, Prof0, CallGraph),
		{ relation__atsort(CallGraph, Cliques) },
		maybe_write_string(Verbose, " done\n"),

		maybe_write_string(Verbose, "% Propogating counts..."),
		propogate_counts(Cliques, Prof0, Prof),
		maybe_write_string(Verbose, " done\n"),
		
		{ prof_get_profnode(Prof, ProfNodeMap) },
		{ map__values(ProfNodeMap, ProfNodeList) },

		output_prof_nodes(ProfNodeList, Prof)
        ).


%-----------------------------------------------------------------------------%


% process_addr_files:
%	process's all the addr*out files.
%
:- pred process_addr_files(prof, io__state, io__state).
:- mode process_addr_files(out, di, uo) is det.

process_addr_files(Prof) -->
	globals__io_lookup_bool_option(very_verbose, VVerbose),
	maybe_write_string(VVerbose, "\n\t% Processing addrdecl.out..."),
	process_addr_decl(AddrDeclMap, ProfNodeMap0),
	maybe_write_string(VVerbose, " done.\n\t% Processing addr.out..."),
	process_addr(ProfNodeMap0, ProfNodeMap1, Hertz, TotalCounts),
	maybe_write_string(VVerbose, " done.\n\t% Processing addrpair.out..."),
	process_addr_pair(ProfNodeMap1, ProfNodeMap),
	maybe_write_string(VVerbose, " done.\n"),
	
	{ Prof = prof(Hertz, TotalCounts, AddrDeclMap, ProfNodeMap) }.


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
	io__see("addrdecl.out", Result),
	(
		{ Result = ok }
	->
		{ map__init(AddrDeclMap0) },
		{ map__init(ProfNodeMap0) },
		process_addr_decl_2(AddrDeclMap0, ProfNodeMap0, AddrDeclMap, 
								ProfNodeMap),
		io__seen
	;
		{ error("process_addr_decl: Couldn't open 'addrdecl.out'\n") }
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
		( 
			{ map__insert(ProfNodeMap0, LabelAddr, ProfNode, 
								ProfNodeMapa) }
		->
			{ ProfNodeMap1 = ProfNodeMapa }
		;
			{ map__lookup(ProfNodeMap0, LabelAddr, ProfNode0) },
			{ prof_node_concat_to_name_list(LabelName, ProfNode0,
								NewProfNode) },
			{ map__det_update(ProfNodeMap0, LabelAddr, NewProfNode,
								ProfNodeMap1) }
		),
		process_addr_decl_2(AddrDecl1, ProfNodeMap1, AddrDecl, 
								ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ AddrDecl = AddrDecl0 },
		{ ProfNodeMap = ProfNodeMap0 }
	).
		

%-----------------------------------------------------------------------------%


% process_addr:
% 	Read's in the addr.out file and store's all the count's in the 
% 	prof_node structure.
%
:- pred process_addr(prof_node_map, prof_node_map, int, int, 
							io__state, io__state).
:- mode process_addr(in, out, out, out, di, uo) is det.

process_addr(ProfNodeMap0, ProfNodeMap, Hertz, TotalCounts) -->
	io__see("addr.out", Result),
	(
		{ Result = ok }
	->
		read_int(Hertz),
		process_addr_2(0, ProfNodeMap0, TotalCounts, ProfNodeMap),
		io__seen
	;
		{ error("process_addr: Couldn't open 'addr.out'\n") }
	).


:- pred process_addr_2(int, prof_node_map, int, prof_node_map, 
							io__state, io__state).
:- mode process_addr_2(in, in, out, out, di, uo) is det.

process_addr_2(TotalCounts0, ProfNodeMap0, TotalCounts, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_int(Count),

		{ map__lookup(ProfNodeMap0, LabelAddr, ProfNode0) },
		{ prof_node_set_initial_count(Count, ProfNode0, ProfNode) },
		{ map__set(ProfNodeMap0, LabelAddr, ProfNode, ProfNodeMap1) },

		{ TC0 is TotalCounts0 + Count },

		process_addr_2(TC0, ProfNodeMap1, TotalCounts, ProfNodeMap)
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
:- pred process_addr_pair(prof_node_map, prof_node_map, io__state, io__state).
:- mode process_addr_pair(in, out, di, uo) is det.

process_addr_pair(ProfNodeMap0, ProfNodeMap) -->
	io__see("addrpair.out", Result),
	(
		{ Result = ok }
	->
		process_addr_pair_2(ProfNodeMap0, ProfNodeMap),
		io__seen
	;
		{ error("process_addr_pair: Couldn't open 'addrpair.out'\n") }
	).

:- pred process_addr_pair_2(prof_node_map, prof_node_map, io__state, io__state).
:- mode process_addr_pair_2(in, out, di, uo) is det.

process_addr_pair_2(ProfNodeMap0, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(CallerAddr) },
		read_label_addr(CalleeAddr),
		read_int(Count),

		% Get child and parent information
		{ map__lookup(ProfNodeMap0, CallerAddr, CallerProfNode0) },
		{ map__lookup(ProfNodeMap0, CalleeAddr, CalleeProfNode0) },
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

		process_addr_pair_2(PNodeMap2, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ ProfNodeMap = ProfNodeMap0 }
	).


%-----------------------------------------------------------------------------%


% build_call_graph:
%	Build's the static call graph if the .prof files are available
%	(signified by being included in arg list).
%	Otherwise build dynamic call_graph from addrpair.out
:- pred build_call_graph(list(string), prof, relation(string), 
							io__state, io__state).
:- mode build_call_graph(in, in, out, di, uo) is det.

build_call_graph([], Prof, CallGraph) -->
	{ relation__init(CallGraph0) },
	build_dynamic_call_graph(Prof, CallGraph0, CallGraph).
build_call_graph([F | Files], _Prof, CallGraph) -->
	{ relation__init(CallGraph0) },
	build_static_call_graph([F | Files], CallGraph0, CallGraph).


% build_dynamic_call_graph:
%	Build's the dynamic call graph from the file addrpair.out file.
%
:- pred build_dynamic_call_graph(prof, relation(string), relation(string),
							io__state, io__state).
:- mode build_dynamic_call_graph(in, in, out, di, uo) is det.

build_dynamic_call_graph(Prof, DynamicCallGraph0, DynamicCallGraph) -->
	io__see("addrpair.out", Result),
	(
		{ Result = ok },
		{ Prof = prof(_H, _TC, _A, ProfNodeMap) },
		process_addr_pair_3(ProfNodeMap, DynamicCallGraph0, 
							DynamicCallGraph),
		io__seen
	;
		{ Result = error(Error) },
		{ DynamicCallGraph = DynamicCallGraph0 },

		io__seen,
		{ io__error_message(Error, ErrorMsg) },
		io__write_string(ErrorMsg),
		io__write_string("\n")
	).
	

:- pred process_addr_pair_3(prof_node_map, relation(string), relation(string),
							io__state, io__state).
:- mode process_addr_pair_3(in, in, out, di, uo) is det.

process_addr_pair_3(ProfNodeMap, DynamicCallGraph0, DynamicCallGraph) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(CallerAddr) },
		read_label_addr(CalleeAddr),
		read_int(_Count),

		% Get child and parent information
		{ map__lookup(ProfNodeMap, CallerAddr, CallerProfNode) },
		{ map__lookup(ProfNodeMap, CalleeAddr, CalleeProfNode) },
		{ prof_node_get_pred_name(CallerProfNode, CallerName) },
		{ prof_node_get_pred_name(CalleeProfNode, CalleeName) },

		{ relation__add(DynamicCallGraph0, CallerName, CalleeName,
							DynamicCallGraph1) },
		
		process_addr_pair_3(ProfNodeMap, DynamicCallGraph1, 
							DynamicCallGraph)
	;	
		{ MaybeLabelAddr = no },
		{ DynamicCallGraph = DynamicCallGraph0 }
	).


% build_static_call_graph:
% 	Build's the static call graph located in the *.prof files.
%
:- pred build_static_call_graph(list(string), relation(string), 
					relation(string), io__state, io__state).
:- mode build_static_call_graph(in, in, out, di, uo) is det.

build_static_call_graph([], StaticCallGraph, StaticCallGraph) --> []. 
build_static_call_graph([File | Files], StaticCallGraph0, StaticCallGraph) -->
	process_prof_file(File, StaticCallGraph0, StaticCallGraph1),
	build_static_call_graph(Files, StaticCallGraph1, StaticCallGraph).
	
		
% process_prof_file:
%	Put's all the Caller and Callee label pairs from File into the 
%	static call graph relation.
%
:- pred process_prof_file(string, relation(string), relation(string),
							io__state, io__state).
:- mode process_prof_file(in, in, out, di, uo) is det.

process_prof_file(File, StaticCallGraph0, StaticCallGraph) -->
	io__see(File, Result),
	(
		{ Result = ok },
		process_prof_file_2(StaticCallGraph0, StaticCallGraph),
		io__seen
	;
		{ Result = error(Error) },
		{ StaticCallGraph = StaticCallGraph0 },

		io__seen,
		{ io__error_message(Error, ErrorMsg) },
		io__write_string(ErrorMsg),
		io__write_string("\n")
	).

:- pred process_prof_file_2(relation(string), relation(string), 
							io__state, io__state).
:- mode process_prof_file_2(in, out, di, uo) is det.

process_prof_file_2(StaticCallGraph0, StaticCallGraph) -->
	maybe_read_label_name(MaybeLabelName),
	(
		{ MaybeLabelName = yes(CallerLabel) },
		read_label_name(CalleeLabel),
		{ relation__add(StaticCallGraph0, CallerLabel, CalleeLabel,
							StaticCallGraph1) },
		process_prof_file_2(StaticCallGraph1, StaticCallGraph)
	;
		{ MaybeLabelName = no },
		{ StaticCallGraph = StaticCallGraph0 }
	).


%-----------------------------------------------------------------------------%


% XXX What does this code do about predicate's which are never called.
%     It shouldn't add their parent's to the parent list.

% propogate_counts:
%	propogate's the count's around the call_graph and assign's index 
%	number's to the predicates.
%	Assign's the index number's on the way down eg predicates to the top
%	of the call graph get the low numbers.
%	On the way back up it propgate's the count's.  eg start propogating 
%	from the leaves of the call graph.
%
:- pred propogate_counts(list(set(string)), prof, prof, io__state, io__state).
:- mode propogate_counts(in, in, out, di, uo) is det.

propogate_counts([], Prof, Prof) --> [].
propogate_counts([Clique | Cliques], Prof0, Prof) -->
	{ prof_get_addrdecl(Prof0, AddrDeclMap) },
	{ prof_get_profnode(Prof0, ProfNodeMap0) },

	{ set__to_sorted_list(Clique, CliqueList) },
	assign_index_nums(CliqueList, AddrDeclMap, 1, ProfNodeMap0, N, 
								ProfNodeMap1),
	propogate_counts_2(Cliques, N, AddrDeclMap, ProfNodeMap1, ProfNodeMap),
	
	{ prof_set_profnode(ProfNodeMap, Prof0, Prof) }.


:- pred propogate_counts_2(list(set(string)), int, addrdecl, prof_node_map, 
					prof_node_map, io__state, io__state).
:- mode propogate_counts_2(in, in, in, in, out, di, uo) is det.

propogate_counts_2([], _, _, ProfNodeMap, ProfNodeMap) --> [].
propogate_counts_2([Clique | Cs], N0, AddrDecl, ProfNodeMap0, ProfNodeMap) -->
	{ set__to_sorted_list(Clique, CliqueList) },

	% On the way down assign the index number's.
	assign_index_nums(CliqueList, AddrDecl, N0, ProfNodeMap0, N, 
								ProfNodeMap1),
	propogate_counts_2(Cs, N, AddrDecl, ProfNodeMap1, ProfNodeMap2),

	% On the way up propogate the counts.
	{ build_parent_map(CliqueList, AddrDecl, ProfNodeMap2, ParentMap) },
	{ sum_counts(CliqueList, AddrDecl, ProfNodeMap2, TotalCounts) },
	{ sum_calls(ParentMap, TotalCalls) },
	{ map__to_assoc_list(ParentMap, ParentList) },
	propogate_counts_3(ParentList, TotalCounts, TotalCalls, AddrDecl, 
						ProfNodeMap2, ProfNodeMap).


:- pred propogate_counts_3(assoc_list(string, int), float, int, addrdecl, 
			prof_node_map, prof_node_map, io__state, io__state).
:- mode propogate_counts_3(in, in, in, in, in, out, di, uo) is det.

propogate_counts_3([], _, _, _, ProfNodeMap, ProfNodeMap) --> [].
propogate_counts_3([ Pred - Calls | Ps], TotalCounts, TotalCalls, AddrMap, 
						ProfNodeMap0, ProfNodeMap) -->
	{ map__lookup(AddrMap, Pred, Key),
	map__lookup(ProfNodeMap0, Key, ProfNode0),

	% Work out the number of count's to propogate.
	int__to_float(Calls, FloatCalls),
	int__to_float(TotalCalls, FloatTotalCalls),
	builtin_float_divide(FloatCalls, FloatTotalCalls, Proportion),
	builtin_float_times(Proportion, TotalCounts, ToPropCount),

	% Add new count's to current propogated counts
	prof_node_get_propogated_counts(ProfNode0, PropCount0),
	builtin_float_plus(PropCount0, ToPropCount, PropCount),
	prof_node_set_propogated_counts(PropCount, ProfNode0, ProfNode),
	map__det_update(ProfNodeMap0, Key, ProfNode, ProfNodeMap1) },

	propogate_counts_3(Ps, TotalCounts, TotalCalls, AddrMap, ProfNodeMap1,
								ProfNodeMap).


% assign_index_nums:
% 	Gives all the pred's index number's.  
%	Assumes that a pred can only be a member of one clique.
%
:- pred assign_index_nums(list(string), addrdecl, int, prof_node_map, int, 
					prof_node_map, io__state, io__state).
:- mode assign_index_nums(in, in, in, in, out, out, di, uo) is det.

assign_index_nums([], _, N, ProfNodeMap, N, ProfNodeMap) --> [].
assign_index_nums([Pred | Preds], AddrMap, N0, ProfNodeMap0, N, ProfNodeMap) -->
	{ map__lookup(AddrMap, Pred, Key) },
	{ map__lookup(ProfNodeMap0, Key, ProfNode0) },
	{ prof_node_set_index_num(N0, ProfNode0, ProfNode) },
	{ N1 is N0 + 1 },
	{ map__set(ProfNodeMap0, Key, ProfNode, ProfNodeMap1) },
	assign_index_nums(Preds, AddrMap, N1, ProfNodeMap1, N, ProfNodeMap).


% build_parent_map:
%	Build's a map which contains all the parent's of a clique, and the 
%	total number of time's that parent is called.  Doesn't include the 
%	clique members, and caller's which never call any of the member's of
%	the clique.
%
:- pred build_parent_map(list(string), addrdecl, prof_node_map, 
							map(string, int)).
:- mode build_parent_map(in, in, in, out) is det.

build_parent_map([], _AddrMap, _ProfNodeMap, _ParentMap) :-
	error("build_parent_map: empty clique list\n").
build_parent_map([C | Cs], AddrMap, ProfNodeMap, ParentMap) :-
	map__init(ParentMap0),
	build_parent_map_2([C | Cs], [C | Cs], AddrMap, ProfNodeMap, 
						ParentMap0, ParentMap).


:- pred build_parent_map_2(list(string), list(string), addrdecl, prof_node_map, 
					map(string, int), map(string, int)). 
:- mode build_parent_map_2(in, in, in, in, in, out) is det.

build_parent_map_2([], _, _, _, ParentMap, ParentMap).
build_parent_map_2([C | Cs], CliqueList, AddrMap, ProfNodeMap, ParentMap0, 	
								ParentMap) :-
	get_prof_node(C, AddrMap, ProfNodeMap, ProfNode),
	prof_node_get_parent_list(ProfNode, ParentList),
	add_to_parent_map(ParentList, CliqueList, ParentMap0, ParentMap1),
	build_parent_map_2(Cs, CliqueList, AddrMap, ProfNodeMap, ParentMap1, 
								ParentMap).


% add_to_parent_map:
% 	Add's list of parent's to parent map.  Ignore's clique member's and
%	repeat's and caller's which never call current predicate.
%	Also return's the total number of time's predicate is called.
%
:- pred add_to_parent_map(list(pred_info), list(string), map(string, int), 
							map(string, int)).
:- mode add_to_parent_map(in, in, in, out) is det.

add_to_parent_map([], _CliqueList, ParentMap, ParentMap).
add_to_parent_map([P | Ps], CliqueList, ParentMap0, ParentMap) :-
	pred_info_get_pred_name(P, PredName),
	pred_info_get_counts(P, Counts),
	(
		(
			list__member(PredName, CliqueList)
		;
			Counts = 0
		)
	->
		add_to_parent_map(Ps, CliqueList, ParentMap0, ParentMap)
	;	
		(
			map__search(ParentMap0, PredName, CurrCount0)
		->
			CurrCount is CurrCount0 + Counts,
			map__det_update(ParentMap0, PredName, CurrCount, 
								ParentMap1)
		;
			map__det_insert(ParentMap0, PredName, Counts, 
								ParentMap1)
		),
		add_to_parent_map(Ps, CliqueList, ParentMap1, ParentMap)
	).


% sum_counts:
%	sum's the total number of count's in a clique list.
%
:- pred sum_counts(list(string), addrdecl, prof_node_map, float).
:- mode sum_counts(in, in, in, out) is det.

sum_counts([], _, _, 0.0).
sum_counts([Pred | Preds], AddrMap, ProfNodeMap, TotalCount) :-
	get_prof_node(Pred, AddrMap, ProfNodeMap, ProfNode),
	prof_node_get_initial_counts(ProfNode, InitCount),
	prof_node_get_propogated_counts(ProfNode, PropCounts),
	sum_counts(Preds, AddrMap, ProfNodeMap, TotalCount0),
	int__to_float(InitCount, InitCountFloat),
	builtin_float_plus(PropCounts, InitCountFloat, PredCount),
	builtin_float_plus(TotalCount0, PredCount, TotalCount).


% sum_calls:
%	sum's the total number of call's into the clique list.
%
:- pred sum_calls(map(string, int), int).
:- mode sum_calls(in, out) is det.

sum_calls(ParentMap, TotalCalls) :-
	map__values(ParentMap, CallList),
	sum_int_list(CallList, TotalCalls).

:- pred sum_int_list(list(int), int).
:- mode sum_int_list(in, out) is det.

sum_int_list([], 0).
sum_int_list([X | Xs], Total) :-
	sum_int_list(Xs, Total0),
	Total is X + Total0.
	


%-----------------------------------------------------------------------------%


% get_prof_node:
%  	Get's the prof_node given a label name.
%
:- pred get_prof_node(string, addrdecl, prof_node_map, prof_node).
:- mode get_prof_node(in, in, in, out) is det.

get_prof_node(Pred, AddrMap, ProfNodeMap, ProfNode) :-
	map__lookup(AddrMap, Pred, Key),
	map__lookup(ProfNodeMap, Key, ProfNode).


%-----------------------------------------------------------------------------%


% obtain_total_counts:
%	Sum's the total number of count's 
%
:- pred obtain_total_counts(list(prof_node), float).
:- mode obtain_total_counts(in, out) is det.

obtain_total_counts([], 0.0).
obtain_total_counts([PN | PNs], TotalCount) :-
	obtain_total_counts(PNs, TotalCount0),

	prof_node_get_initial_counts(PN, Initial),
	int__to_float(Initial, FloatInitial),

	builtin_float_plus(TotalCount0, FloatInitial, TotalCount).


%-----------------------------------------------------------------------------%

:- pred prof_node_init(string, prof_node).
:- mode prof_node_init(in, out) is det.

prof_node_init(PredName, prof_node(PredName, 0, 0, 0.0, [], [], 0, 0, [])).

% *** Access prof_node predicates *** %

:- pred prof_node_get_pred_name(prof_node, string).
:- mode prof_node_get_pred_name(in, out) is det.

prof_node_get_pred_name(prof_node(Name, _, _, _, _, _, _, _, _), Name).

:- pred prof_node_get_index_number(prof_node, int).
:- mode prof_node_get_index_number(in, out) is det.

prof_node_get_index_number(prof_node(_, Index, _, _, _, _, _, _, _), Index).

:- pred prof_node_get_initial_counts(prof_node, int).
:- mode prof_node_get_initial_counts(in, out) is det.

prof_node_get_initial_counts(prof_node(_, _, Count, _, _, _, _, _, _), Count).

:- pred prof_node_get_propogated_counts(prof_node, float).
:- mode prof_node_get_propogated_counts(in, out) is det.

prof_node_get_propogated_counts(prof_node(_, _, _, Count, _, _, _, _, _), Count).

:- pred prof_node_get_parent_list(prof_node, list(pred_info)).
:- mode prof_node_get_parent_list(in, out) is det.

prof_node_get_parent_list(prof_node(_, _, _, _, PList, _, _, _, _), PList).

:- pred prof_node_get_child_list(prof_node, list(pred_info)).
:- mode prof_node_get_child_list(in, out) is det.

prof_node_get_child_list(prof_node(_, _, _, _, _, Clist, _, _, _), Clist).

:- pred prof_node_get_total_calls(prof_node, int).
:- mode prof_node_get_total_calls(in, out) is det.

prof_node_get_total_calls(prof_node(_, _, _, _, _, _, Calls, _, _), Calls).

:- pred prof_node_get_self_calls(prof_node, int).
:- mode prof_node_get_self_calls(in, out) is det.

prof_node_get_self_calls(prof_node(_, _, _, _, _, _, _, Calls, _), Calls).

% *** Update prof_node predicates *** %

:- pred prof_node_set_index_num(int, prof_node, prof_node).
:- mode prof_node_set_index_num(in, in, out) is det.

prof_node_set_index_num(Index, prof_node(A, _, C, D, E, F, G, H, I), 
				prof_node(A, Index, C, D, E, F, G, H, I)).

:- pred prof_node_set_initial_count(int, prof_node, prof_node).
:- mode prof_node_set_initial_count(in, in, out) is det.

prof_node_set_initial_count(Count, prof_node(A, B, _, D, E, F, G, H, I), 
				prof_node(A, B, Count, D, E, F, G, H, I)).

:- pred prof_node_set_propogated_counts(float, prof_node, prof_node).
:- mode prof_node_set_propogated_counts(in, in, out) is det.

prof_node_set_propogated_counts(Count, prof_node(A, B, C, _, E, F, G, H, I),
				 prof_node(A, B, C, Count, E, F, G, H, I)).

:- pred prof_node_concat_to_parent(pred_info, prof_node, prof_node).
:- mode prof_node_concat_to_parent(in, in, out) is det.

prof_node_concat_to_parent(PredInfo, prof_node(A, B, C, D, PList, F, G, H, I), 
			prof_node(A, B, C, D, [PredInfo | PList], F, G, H, I)).

:- pred prof_node_concat_to_child(pred_info, prof_node, prof_node).
:- mode prof_node_concat_to_child(in, in, out) is det.

prof_node_concat_to_child(PredInfo, prof_node(A, B, C, D, E, CList, G, H, I), 
			prof_node(A, B, C, D, E, [PredInfo | CList], G, H, I)).

:- pred prof_node_set_total_calls(int, prof_node, prof_node).
:- mode prof_node_set_total_calls(in, in, out) is det.

prof_node_set_total_calls(Calls, prof_node(A, B, C, D, E, F, _, H, I),
				prof_node(A, B, C, D, E, F, Calls, H, I)).

:- pred prof_node_set_self_calls(int, prof_node, prof_node).
:- mode prof_node_set_self_calls(in, in, out) is det.

prof_node_set_self_calls(Calls, prof_node(A, B, C, D, E, F, G, _, I),
				prof_node(A, B, C, D, E, F, G, Calls, I)).

:- pred prof_node_concat_to_name_list(string, prof_node, prof_node).
:- mode prof_node_concat_to_name_list(in, in, out) is det.

prof_node_concat_to_name_list(Name, prof_node(A, B, C, D, E, F, G, H, NL), 
			prof_node(A, B, C, D, E, F, G, H, [Name | NL])).


%-----------------------------------------------------------------------------%

% *** Access predicates for pred_info *** %

:- pred pred_info_get_pred_name(pred_info, string).
:- mode pred_info_get_pred_name(in, out) is det.

pred_info_get_pred_name(pred_info(Name, _), Name).

:- pred pred_info_get_counts(pred_info, int).
:- mode pred_info_get_counts(in, out) is det.

pred_info_get_counts(pred_info(_, Count), Count).

%-----------------------------------------------------------------------------%

:- pred output_prof_info(prof_node, io__state, io__state).
:- mode output_prof_info(in, di, uo) is det.

output_prof_info(ProfNode) -->
        { prof_node_get_pred_name(ProfNode, Name) },
	{ prof_node_get_index_number(ProfNode, Index) },
        { prof_node_get_initial_counts(ProfNode, InitCounts) },
	{ prof_node_get_propogated_counts(ProfNode, PropCounts) },
	{ prof_node_get_parent_list(ProfNode, ParentList) },
	{ prof_node_get_child_list(ProfNode, ChildList) },

        io__write_string("*** prof_node ***\n"),
        io__write_string("Name:\t"),
        io__write_string(Name),
        io__write_string("\nIndex:\t"),
        io__write_int(Index),
        io__write_string("\nInitial:\t"),
        io__write_int(InitCounts),
        io__write_string("\nProp:\t"),
        io__write_float(PropCounts),
	io__write_string("\nParent List ->\n"),
	output_pred_info_list(ParentList),
	io__write_string("\nChild List ->\n"),
	output_pred_info_list(ChildList),
        io__write_string("\n").

%-----------------------------------------------------------------------------%

:- pred output_pred_info_list(list(pred_info), io__state, io__state).
:- mode output_pred_info_list(in, di, uo) is det.

output_pred_info_list([]) --> [].
output_pred_info_list([X | Xs]) -->
	{ pred_info_get_pred_name(X, Name) },
	{ pred_info_get_counts(X, Counts) },

	io__write_string("\t"),
	io__write_string(Name),
	io__write_string(" : "),
	io__write_int(Counts),
	io__write_string("\n"),
	output_pred_info_list(Xs).

%-----------------------------------------------------------------------------%

:- pred output_prof_nodes(list(prof_node), prof, io__state, io__state).
:- mode output_prof_nodes(in, in, di, uo) is det.

output_prof_nodes([], _Prof) -->
	io__write_string("No profiling information to output.\n").
output_prof_nodes([PN | PNs], Prof) -->
	io__write_string("                                  called/total"),
	io__write_string("       parents\n"),
	io__write_string("index  %time    self descendents  called+self"),
	io__write_string("    name           index\n"),
	io__write_string("                                  called/total"),
	io__write_string("       children\n\n"),
	output_prof_nodes_2([PN|PNs], Prof).


:- pred output_prof_nodes_2(list(prof_node), prof, io__state, io__state).
:- mode output_prof_nodes_2(in, in, di, uo) is det.

output_prof_nodes_2([], _) --> [].
output_prof_nodes_2([PN | PNs], Prof) -->
	output_formatted_prof_node(PN, Prof),
	io__write_string("-----------------------------------------------\n\n"),
	output_prof_nodes_2(PNs, Prof).
	
	
:- pred output_formatted_prof_node(prof_node, prof, io__state, io__state).
:- mode output_formatted_prof_node(in, in, di, uo) is det.

output_formatted_prof_node(ProfNode, Prof) -->
	{ Prof = prof(Hertz, IntTotalCounts, AddrMap, ProfNodeMap) },
	{ int__to_float(IntTotalCounts, TotalCounts) },

        { prof_node_get_pred_name(ProfNode, LabelName) },
	{ prof_node_get_index_number(ProfNode, Index) },
	{ prof_node_get_initial_counts(ProfNode, Initial) },
	{ prof_node_get_propogated_counts(ProfNode, Prop) },
	{ prof_node_get_parent_list(ProfNode, ParentList) },
	{ prof_node_get_child_list(ProfNode, ChildList) },
	{ prof_node_get_total_calls(ProfNode, TotalCalls) },
	{ prof_node_get_self_calls(ProfNode, SelfCalls) },
	
	% Calculate proportion of time in current predicate as a percentage.
	{
	int__to_float(Initial, InitialFloat),
	builtin_float_plus(InitialFloat, Prop, CurrentCount),
	builtin_float_divide(CurrentCount, TotalCounts, Proportion),
	builtin_float_times(100.0, Proportion, Percentage)
	},

	% Calculate the self time spent in the current predicate.
	% XXX 5.0 is the number of clock tick's need's to be command line 
	% option.
	{
	int__to_float(Hertz, HertzFloat),
	builtin_float_divide(InitialFloat, HertzFloat, InterA),
	builtin_float_times(InterA, 5.0, Selftime)
	},

	% Calculate the descendant time, which is the time spent in the 
	% current predicate and it's descendant's
	{
	builtin_float_divide(CurrentCount, HertzFloat, InterB),
	builtin_float_times(InterB, 5.0, DescTime)
	},

	% Set all string's up correctly
	{
	string__int_to_string(Index, IndexStr0),
	string__append_list(["[", IndexStr0, "] "], IndexStr1),
	string__pad_right(IndexStr1, ' ', 7, IndexStr),
	string__split(LabelName, 9, _, Name) 
	},

	% Output self-recursive info
	(
		{ SelfCalls = 0 }
	->
		[]
	;
		% XXX Need's to be placed in the correct place for > 10.
		io__write_formatted_int(SelfCalls, "%40d"),
		io__write_string("             "),
		io__write_string(Name),
		io__write_string(" "),
		io__write_string(IndexStr1),
		io__write_string("\n")
	),

	output_formatted_parent_list(ParentList, Selftime, DescTime, TotalCalls,
									Prof),

	io__write_string(IndexStr),
	io__write_formatted_float(Percentage, "%5.1f"),
	io__write_string(" "),
	io__write_formatted_float(Selftime, "%7.2f"),
	io__write_string(" "),
	io__write_formatted_float(DescTime, "%11.2f"),
	io__write_string(" "),
	io__write_formatted_int(TotalCalls, "%7d"),
	(
		{ SelfCalls = 0 }
	->
		io__write_string("        ")
	;
		io__write_string("+"),
		io__write_formatted_int(SelfCalls, "%7-d")
	),
	io__write_string(" "),
	io__write_string(Name),
	io__write_string(" "),
	io__write_string(IndexStr1),
	io__write_string("\n"),

	output_formatted_child_list(ChildList, AddrMap, ProfNodeMap),

	% Output self-recursive info
	(
		{ SelfCalls = 0 }
	->
		[]
	;
		io__write_formatted_int(SelfCalls, "%40d"),
		io__write_string("             "),
		io__write_string(Name),
		io__write_string(" "),
		io__write_string(IndexStr1),
		io__write_string("\n")
	),
	io__write_string("\n").


:- pred output_formatted_parent_list(list(pred_info), float, float, int, prof, 
							io__state, io__state).
:- mode output_formatted_parent_list(in, in, in, in, in, di, uo) is det.

output_formatted_parent_list([], _, _, _, _) --> 
	{ string__pad_left("<spontaneous>\n", ' ', 67, String) },
	io__write_string(String).
output_formatted_parent_list([P | Ps], Selftime, DescTime, TotalCalls, Prof) -->
	% XXX Place bit of code here to output self recursive call's.
	output_formatted_parent_list_2([P | Ps], Selftime, DescTime, TotalCalls,
									Prof).

:- pred output_formatted_parent_list_2(list(pred_info), float, float, int, prof,
							io__state, io__state).
:- mode output_formatted_parent_list_2(in, in, in, in, in, di, uo) is det.

output_formatted_parent_list_2([], _, _, _, _) --> [].
output_formatted_parent_list_2([pred_info(LabelName, Calls) | Ps], Selftime,
						DescTime, TotalCalls, Prof) -->
	{ Prof = prof(_Hertz, _TotalCounts, AddrMap, ProfNodeMap) },

	{
	int__to_float(TotalCalls, FloatTotalCalls),
	int__to_float(Calls, FloatCalls),
	builtin_float_divide(FloatCalls, FloatTotalCalls, Proportion)
	},

	% Calculate the amount of the current predicate's self-time spent
	% due to the parent.
	% and the amount of the current predicate's descendant-time spent
	% due to the parent.
	{
	builtin_float_times(Selftime, Proportion, PropSelftime),
	builtin_float_times(DescTime, Proportion, PropDescTime)
	},


	% Lookup map for index number
	{ get_prof_node(LabelName, AddrMap, ProfNodeMap, ProfNode) },
	{ prof_node_get_index_number(ProfNode, Index) },

	% Set all string's up correctly
	{
	string__int_to_string(Index, IndexStr0),
	string__append_list([" [", IndexStr0, "]"], IndexStr),
	string__split(LabelName, 9, _, Name) 
	},

	io__write_formatted_float(PropSelftime, "%20.2f"),
	io__write_formatted_float(PropDescTime, "%12.2f"),
	io__write_formatted_int(Calls, "%8d"),
	io__write_string("/"),
	io__write_formatted_int(TotalCalls, "%11-d"),
	io__write_string(" "),
	io__write_string(Name),
	io__write_string(IndexStr),
	io__write_string("\n"),

	output_formatted_parent_list_2(Ps, Selftime, DescTime, TotalCalls,Prof).


:- pred output_formatted_child_list(list(pred_info), addrdecl, prof_node_map, 
							io__state, io__state).
:- mode output_formatted_child_list(in, in, in, di, uo) is det.

output_formatted_child_list([], _, _) --> [].
output_formatted_child_list([pred_info(LabelName, Calls) | Ps], AddrMap, 
								ProfNodeMap) -->
	% Lookup map for index number
	{ get_prof_node(LabelName, AddrMap, ProfNodeMap, ProfNode) },
	{ prof_node_get_index_number(ProfNode, Index) },
	{ prof_node_get_total_calls(ProfNode, TotalCalls) },

	% Set all string's up correctly
	{
	string__int_to_string(Index, IndexStr0),
	string__append_list([" [", IndexStr0, "]"], IndexStr),
	string__split(LabelName, 9, _, Name) 
	},

	io__write_formatted_int(Calls, "%40d"),
	io__write_string("/"),
	io__write_formatted_int(TotalCalls, "%11-d"),
	io__write_string(" "),
	io__write_string(Name),
	io__write_string(IndexStr),
	io__write_string("\n"),

	output_formatted_child_list(Ps, AddrMap, ProfNodeMap).


%-----------------------------------------------------------------------------% 


:- pred output_basic(list(prof_node), io__state, io__state).  
:- mode output_basic(in, di, uo) is det.
output_basic([]) --> [].
output_basic([PN | PNs])  -->
	io__write_string("-------------------------------------------------\n"),
	output_formatted_prof_node_2(PN),
	output_basic(PNs).

:- pred output_formatted_prof_node_2(prof_node, io__state, io__state).
:- mode output_formatted_prof_node_2(in, di, uo) is det.

output_formatted_prof_node_2(ProfNode) -->
        { prof_node_get_pred_name(ProfNode, Name) },
	{ prof_node_get_parent_list(ProfNode, ParentList) },
	{ prof_node_get_child_list(ProfNode, ChildList) },

	output_formatted_list(ParentList),
	io__write_string(Name),
	io__write_string("\n"),
	output_formatted_list(ChildList),
	io__write_string("-------------------------------------------------\n").


:- pred output_formatted_list(list(pred_info), io__state, io__state).
:- mode output_formatted_list(in, di, uo) is det.

output_formatted_list([]) --> [].
output_formatted_list([X | Xs]) -->
	{ pred_info_get_pred_name(X, Name0) },
	{ pred_info_get_counts(X, Counts) },
	{ string__pad_right(Name0, ' ', 40, Name) },

	io__write_string("\t"),
	io__write_string(Name),
	io__write_string(" : "),
	io__write_int(Counts),
	io__write_string("\n"),
	output_formatted_list(Xs).


%-----------------------------------------------------------------------------% 


:- pred prof_init(prof).
:- mode prof_init(out) is det.

prof_init(prof(0, 0, AddrDeclMap, ProfNodeMap)) :-
	map__init(AddrDeclMap),
	map__init(ProfNodeMap).

% *** Access predicates for prof *** %

:- pred prof_get_addrdecl(prof, addrdecl).
:- mode prof_get_addrdecl(in, out) is det.

prof_get_addrdecl(prof(_, _, AddrDeclMap, _), AddrDeclMap).

:- pred prof_get_profnode(prof, prof_node_map).
:- mode prof_get_profnode(in, out) is det.

prof_get_profnode(prof(_, _, _, ProfNodeMap), ProfNodeMap).


% *** Update predicates for prof *** %

:- pred prof_set_addrdecl(addrdecl, prof, prof).
:- mode prof_set_addrdecl(in, in, out) is det.

prof_set_addrdecl(AddrDeclMap, prof(A, B, _, D), prof(A, B, AddrDeclMap, D)).

:- pred prof_set_profnode(prof_node_map, prof, prof).
:- mode prof_set_profnode(in, in, out) is det.

prof_set_profnode(ProfNodeMap, prof(A, B, C, _), prof(A, B, C, ProfNodeMap)).


%-----------------------------------------------------------------------------% 


