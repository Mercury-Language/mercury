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
:- import_module string, set, list, map.
:- import_module getopt, options, globals.
:- import_module std_util, require.
:- import_module relation.


	% prof_node: Contains all the info used for output.
:- type prof_node 	---> 
		prof_node(
			string, 		% current predicate (label)
			int,			% index number
			int, 			% self counts
			int, 			% propogated counts
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
postprocess_options(ok(OptionTable0), no) --> [].


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
        io__write_string(StdErr, "Mercury profiler version 0.1\n"),
        io__write_string(StdErr, "Usage: "),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " [<options>]\n"),
        io__write_string(StdErr, "Use `"),
        io__write_string(StdErr, ProgName),
        io__write_string(StdErr, " --help' for more information.\n").

:- pred long_usage(io__state::di, io__state::uo) is det.
long_usage -->
        io__progname_base("mprof", ProgName),
        io__write_string("Mercury profiler version 0.1\n"),
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
        % globals__io_lookup_bool_option(help, Help),
        % (
        %         { Help = yes }
        % ->
        %         long_usage
        % ;
                process_addr_decl(AddrDeclMap, ProfNodeMap0),
		process_addr(ProfNodeMap0, ProfNodeMap1, Hertz),
		process_addr_pair(ProfNodeMap1, ProfNodeMap),
		build_static_call_graph(Args, StaticCallGraph),
		{ relation__atsort(StaticCallGraph, Cliques) },
		% output_cliques(Cliques),
                io__stdout_stream(StdOut),
                io__write_string(StdOut, "mprof done\n").
        % ).


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
								ProfNodeMap)
	;
		{ error("get_addr_decl: Couldn't open 'addrdecl.out'\n") }
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
		process_addr_2(ProfNodeMap0, ProfNodeMap)
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
		process_addr_pair_2(ProfNodeMap0, ProfNodeMap)
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
	io__write_string(File),
	io__write_string("\n"),
	process_prof_file(File, StaticCallGraph0, StaticCallGraph1),
	build_static_call_graph_2(Files, StaticCallGraph1, StaticCallGraph).
	
		
% process_prof_file:
%	Put's all the Caller and Callee label pairs from File into the 
%	static call graph relation.
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

:- pred prof_node_init(string, prof_node).
:- mode prof_node_init(in, out) is det.

prof_node_init(PredName, prof_node(PredName, 0, 0, 0, [], [], 0, 0, 0)).

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

:- pred prof_node_get_propogated_counts(prof_node, int).
:- mode prof_node_get_propogated_counts(in, out) is det.

prof_node_get_propogated_counts(prof_node(_, _, _, Count, _, _, _, _, _), Count).

:- pred prof_node_get_parent_list(prof_node, list(pred_info)).
:- mode prof_node_get_parent_list(in, out) is det.

prof_node_get_parent_list(prof_node(_, _, _, _, PList, _, _, _, _), PList).

:- pred prof_node_get_child_list(prof_node, list(pred_info)).
:- mode prof_node_get_child_list(in, out) is det.

prof_node_get_child_list(prof_node(_, _, _, _, _, Clist, _, _, _), Clist).

% *** Update prof_node predicates *** %

:- pred prof_node_set_initial_count(int, prof_node, prof_node).
:- mode prof_node_set_initial_count(in, in, out) is det.

prof_node_set_initial_count(Count, prof_node(A, B, _, D, E, F, G, H, I), 
				prof_node(A, B, Count, D, E, F, G, H, I)).

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
        io__write_int(PropCounts),
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
