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

:- import_module debug.
:- import_module read.
:- import_module float, int, list, map, string, set.
:- import_module getopt, options, globals.
:- import_module std_util, require.
:- import_module relation.


	% prof_node: Contains all the info used for output.
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
			junk,
			junk,
			junk
		).

:- type pred_info --->
		pred_info(
			string,			% predicate (label)     
			int,			% index number
			int			% count ( to or from)
		).

	% addrdecl = map(label_name, label_address)
:- type addrdecl	==	map(string, int).

	% prof_node_map = map(label_address, prof_node)
:- type prof_node_map	==	map(int, prof_node).

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
		maybe_write_string(Verbose, "% Processing input files ..."),
		process_addr_files(_Hertz, AddrDeclMap, ProfNodeMap0),
		maybe_write_string(Verbose, " done\n% Building call graph..."),
		build_static_call_graph(Args, StaticCallGraph),
		{ relation__atsort(StaticCallGraph, Cliques) },
		maybe_write_string(Verbose, " done\n% Propogating counts..."),
		propogate_counts(Cliques, AddrDeclMap, ProfNodeMap0, 
								ProfNodeMap),
		maybe_write_string(Verbose, " done\n"),
		% output_cliques(Cliques),
		{ map__values(ProfNodeMap, ProfNodeList) },
		output_basic(ProfNodeList)
        ).


%-----------------------------------------------------------------------------%


% process_addr_files:
%	process's all the addr*out files.
%
:- pred process_addr_files(int, addrdecl, prof_node_map, io__state, io__state).
:- mode process_addr_files(out, out, out, di, uo) is det.

process_addr_files(Hertz, AddrDeclMap, ProfNodeMap) -->
	globals__io_lookup_bool_option(very_verbose, VVerbose),
	maybe_write_string(VVerbose, "\n\t% Processing addrdecl.out..."),
	process_addr_decl(AddrDeclMap, ProfNodeMap0),
	maybe_write_string(VVerbose, " done.\n\t% Processing addr.out..."),
	process_addr(ProfNodeMap0, ProfNodeMap1, Hertz),
	maybe_write_string(VVerbose, " done.\n\t% Processing addrpair.out..."),
	process_addr_pair(ProfNodeMap1, ProfNodeMap),
	maybe_write_string(VVerbose, " done.\n").


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
		{ map__det_insert(ProfNodeMap0, LabelAddr, ProfNode, ProfNodeMap1) },
		process_addr_decl_2(AddrDecl1, ProfNodeMap1, AddrDecl, ProfNodeMap)
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
:- pred process_addr(prof_node_map, prof_node_map, int, io__state, io__state).
:- mode process_addr(in, out, out, di, uo) is det.

process_addr(ProfNodeMap0, ProfNodeMap, Hertz) -->
	io__see("addr.out", Result),
	(
		{ Result = ok }
	->
		read_int(Hertz),
		process_addr_2(ProfNodeMap0, ProfNodeMap),
		io__seen
	;
		{ error("process_addr: Couldn't open 'addr.out'\n") }
	).


:- pred process_addr_2(prof_node_map, prof_node_map, io__state, io__state).
:- mode process_addr_2(in, out, di, uo) is det.

process_addr_2(ProfNodeMap0, ProfNodeMap) -->
	maybe_read_label_addr(MaybeLabelAddr),
	(
		{ MaybeLabelAddr = yes(LabelAddr) },
		read_int(Count),
		{ map__lookup(ProfNodeMap0, LabelAddr, ProfNode0) },
		{ prof_node_set_initial_count(Count, ProfNode0, ProfNode) },
		{ map__set(ProfNodeMap0, LabelAddr, ProfNode, ProfNodeMap1) },
		process_addr_2(ProfNodeMap1, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ ProfNodeMap = ProfNodeMap0 }
	).


%-----------------------------------------------------------------------------%


% process_addr_pair:
%	Read's in the addrpair.out file and store's the data in the relevant
%	list's of the prof_node structure.
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
		{ prof_node_concat_to_child(pred_info(CalleeName, 0, Count),
					CallerProfNode0, CallerProfNode) },
		{ map__set(ProfNodeMap0, CallerAddr, CallerProfNode, PNodeMap1) },

		% Insert parent information
		{ prof_node_concat_to_parent(pred_info(CallerName, 0, Count),
					CalleeProfNode0, CalleeProfNode) },
		{ map__set(PNodeMap1, CalleeAddr, CalleeProfNode, PNodeMap2) },

		process_addr_pair_2(PNodeMap2, ProfNodeMap)
	;
		{ MaybeLabelAddr = no },
		{ ProfNodeMap = ProfNodeMap0 }
	).


%-----------------------------------------------------------------------------%


% build_static_call_graph:
% 	Build's the static call graph located in the *.prof files.
%	XXX Should be changed so that it can build from the dynamic call
%	graph if the *.prof files aren't available.
%
:- pred build_static_call_graph(list(string), relation(string), 
							io__state, io__state).
:- mode build_static_call_graph(in, out, di, uo) is det.

build_static_call_graph(Files, StaticCallGraph) -->
	{ relation__init(StaticCallGraph0) },
	build_static_call_graph_2(Files, StaticCallGraph0, StaticCallGraph).

:- pred build_static_call_graph_2(list(string), relation(string), 
					relation(string), io__state, io__state).
:- mode build_static_call_graph_2(in, in, out, di, uo) is det.

build_static_call_graph_2([], StaticCallGraph, StaticCallGraph) --> []. 
build_static_call_graph_2([File | Files], StaticCallGraph0, StaticCallGraph) -->
	process_prof_file(File, StaticCallGraph0, StaticCallGraph1),
	build_static_call_graph_2(Files, StaticCallGraph1, StaticCallGraph).
	
		
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


% propogate_counts:
%	propogate's the count's around the call_graph and assign's index 
%	number's to the predicates.
%	Assign's the index number's on the way down eg predicates to the top
%	of the call graph get the low numbers.
%	On the way back up it propgate's the count's.  eg start propogating 
%	from the leaves of the call graph.
%
:- pred propogate_counts(list(set(string)), addrdecl, prof_node_map,
					prof_node_map, io__state, io__state).
:- mode propogate_counts(in, in, in, out, di, uo) is det.

propogate_counts([], _, ProfNodeMap, ProfNodeMap) --> [].
propogate_counts([Clique | Cliques], AddrDeclMap, ProfNodeMap0, ProfNodeMap) -->
	{ set__to_sorted_list(Clique, CliqueList) },
	assign_index_nums(CliqueList, AddrDeclMap, 1, ProfNodeMap0, N, 
								ProfNodeMap1),
	propogate_counts_2(Cliques, N, AddrDeclMap, ProfNodeMap1, ProfNodeMap).


:- pred propogate_counts_2(list(set(string)), int, addrdecl, prof_node_map, 
					prof_node_map, io__state, io__state).
:- mode propogate_counts_2(in, in, in, in, out, di, uo) is det.

propogate_counts_2([], _, _, ProfNodeMap, ProfNodeMap) --> [].
propogate_counts_2([Clique | Cs], N0, AddrDecl, ProfNodeMap0, ProfNodeMap) -->
	{ set__to_sorted_list(Clique, CliqueList) },
	assign_index_nums(CliqueList, AddrDecl, N0, ProfNodeMap0, N, 
								ProfNodeMap1),
	propogate_counts_2(Cs, N, AddrDecl, ProfNodeMap1, ProfNodeMap2),
	{ build_parent_map(CliqueList, AddrDecl, ProfNodeMap2, ParentMap) },
	{ sum_counts(CliqueList, AddrDecl, ProfNodeMap2, TotalCounts) },
	% io__write_string("Total counts: "),
	% io__write_float(TotalCounts),
	% io__write_string("\n"),
	{ sum_calls(ParentMap, TotalCalls) },
	% io__write_string("Total calls: "),
	% io__write_int(TotalCalls),
	% io__write_string("\n"),
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
	builtin_float_divide(FloatCalls, FloatTotalCalls, Percentage),
	builtin_float_times(Percentage, TotalCounts, ToPropCount),

	% Add new count's to current propogated counts
	prof_node_get_propogated_counts(ProfNode0, PropCount0),
	builtin_float_plus(PropCount0, ToPropCount, PropCount),
	prof_node_set_propogated_counts(PropCount, ProfNode0, ProfNode),
	map__det_update(ProfNodeMap0, Key, ProfNode, ProfNodeMap1) },

	% io__write_string("++++ propogate_counts_3 ++++\n"),
	% io__write_string("Percentage: "),
	% io__write_float(Percentage),
	% io__write_string("\n"),
	% io__write_string("ToPropCount: "),
	% io__write_float(ToPropCount),
	% io__write_string("\n"),

	% output_prof_info(ProfNode),
	% io__write_string("+++++++++++++++++++++++++++\n"),
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
	% output_prof_info(ProfNode),
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
		%;
		%	Counts \= 0
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

:- pred prof_node_init(string, prof_node).
:- mode prof_node_init(in, out) is det.

prof_node_init(PredName, prof_node(PredName, 0, 0, 0.0, [], [], 0, 0, 0)).

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

%-----------------------------------------------------------------------------%

% *** Access predicates for pred_info *** %

:- pred pred_info_get_pred_name(pred_info, string).
:- mode pred_info_get_pred_name(in, out) is det.

pred_info_get_pred_name(pred_info(Name, _, _), Name).

:- pred pred_info_get_index(pred_info, int).
:- mode pred_info_get_index(in, out) is det.

pred_info_get_index(pred_info(_, Index, _), Index).

:- pred pred_info_get_counts(pred_info, int).
:- mode pred_info_get_counts(in, out) is det.

pred_info_get_counts(pred_info(_, _, Count), Count).

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


:- pred output_basic(list(prof_node), io__state, io__state).
:- mode output_basic(in, di, uo) is det.

output_basic([]) --> [].
output_basic([PN | PNs])  -->
	io__write_string("-------------------------------------------------\n"),
	output_formatted_prof_node(PN),
	output_basic(PNs).

:- pred output_formatted_prof_node(prof_node, io__state, io__state).
:- mode output_formatted_prof_node(in, di, uo) is det.

output_formatted_prof_node(ProfNode) -->
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
