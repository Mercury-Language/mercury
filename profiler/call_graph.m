%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% call_graph.m
%
% Main author: petdr.
%
% Responsible for building the static call graph.  The dynamic call graph is
% built during the processing of 'Prof.CallPair', if the appropiate option is
% set.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module call_graph.

:- interface.

:- import_module relation, io, list, string.

:- pred call_graph__main(list(string), relation(string), relation(string),
							io__state, io__state).
:- mode call_graph__main(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module read.
:- import_module options, globals.
:- import_module require, bool, std_util.


call_graph__main(Args, StaticCallGraph0, StaticCallGraph) -->
	globals__io_lookup_bool_option(dynamic_cg, Dynamic),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),

	% We can only build the static call graph if the *.prof files are 
	% available.  NB The dynamic call graph is built as it is read in
	% in process_addr_pair_file
	(
		{ Dynamic = yes } 
	->
		{ StaticCallGraph = StaticCallGraph0 }
	;
		build_static_call_graph(Args, StaticCallGraph0, VeryVerbose,
								StaticCallGraph)
	).


% build_static_call_graph:
% 	Builds the static call graph located in the *.prof files.
%
:- pred build_static_call_graph(list(string), relation(string), bool,
					relation(string), io__state, io__state).
:- mode build_static_call_graph(in, in, in, out, di, uo) is det.

build_static_call_graph([], StaticCallGraph, _, StaticCallGraph) --> []. 
build_static_call_graph([File | Files], StaticCallGraph0, VeryVerbose, 
							StaticCallGraph) -->
	maybe_write_string(VeryVerbose, "\n\tProcessing "),
	maybe_write_string(VeryVerbose, File),
	maybe_write_string(VeryVerbose, "..."),
	process_prof_file(File, StaticCallGraph0, StaticCallGraph1),
	maybe_write_string(VeryVerbose, " done"),
	build_static_call_graph(Files, StaticCallGraph1, VeryVerbose,
							StaticCallGraph).
	
		
% process_prof_file:
%	Puts all the Caller and Callee label pairs from File into the 
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

		{ io__error_message(Error, ErrorMsg) },
		io__stderr_stream(StdErr),
		io__write_strings(StdErr, ["mprof: error opening file `",
			File, "': ", ErrorMsg, "\n"])
	).

:- pred process_prof_file_2(relation(string), relation(string), 
							io__state, io__state).
:- mode process_prof_file_2(in, out, di, uo) is det.

process_prof_file_2(StaticCallGraph0, StaticCallGraph) -->
	maybe_read_label_name(MaybeLabelName),
	(
		{ MaybeLabelName = yes(CallerLabel) },
		read_label_name(CalleeLabel),
		{ relation__lookup_element(StaticCallGraph0, CallerLabel,
					CallerKey) },
		{ relation__lookup_element(StaticCallGraph0, CalleeLabel,
					CalleeKey) },
		{ relation__add(StaticCallGraph0, CallerKey, CalleeKey,
							StaticCallGraph1) },
		process_prof_file_2(StaticCallGraph1, StaticCallGraph)
	;
		{ MaybeLabelName = no },
		{ StaticCallGraph = StaticCallGraph0 }
	).


%-----------------------------------------------------------------------------%
