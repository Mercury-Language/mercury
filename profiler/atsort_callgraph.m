%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% atsort_callgraph.m
%
% Main author: petdr.
%
% Takes a list of files which contains the callgraph of a Mercury module and
% approximately topologically sorts them to std out.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module atsort_callgraph.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

:- import_module read.
:- import_module relation, set, std_util.

main -->
	io__command_line_arguments(Files),
	{ relation__init(CallGraph0) },
	build_call_graph(Files, CallGraph0, CallGraph),
	{ relation__atsort(CallGraph, Cliques) },
	{ list__reverse(Cliques, RevCliques) },
	print_cliques(RevCliques).


%-----------------------------------------------------------------------------%


% build__call_graph:
% 	Builds the  call graph located in the *.prof files.
%
:- pred build_call_graph(list(string), relation(string), 
					relation(string), io__state, io__state).
:- mode build_call_graph(in, in, out, di, uo) is det.

build_call_graph([], CallGraph, CallGraph) --> []. 
build_call_graph([File | Files], CallGraph0, CallGraph) -->
	process_prof_file(File, CallGraph0, CallGraph1),
	build_call_graph(Files, CallGraph1, CallGraph).
	
		
% process_prof_file:
%	Puts all the Caller and Callee label pairs from File into the 
%	 call graph relation.
%
:- pred process_prof_file(string, relation(string), relation(string),
							io__state, io__state).
:- mode process_prof_file(in, in, out, di, uo) is det.

process_prof_file(File, CallGraph0, CallGraph) -->
	io__see(File, Result),
	(
		{ Result = ok },
		process_prof_file_2(CallGraph0, CallGraph),
		io__seen
	;
		{ Result = error(Error) },
		{ CallGraph = CallGraph0 },

		{ io__error_message(Error, ErrorMsg) },
		io__stderr_stream(StdErr),
		io__write_strings(StdErr, 
			["atsort_callgraph: error opening file `",
			File, "': ", ErrorMsg, "\n"])
	).

:- pred process_prof_file_2(relation(string), relation(string), 
							io__state, io__state).
:- mode process_prof_file_2(in, out, di, uo) is det.

process_prof_file_2(CallGraph0, CallGraph) -->
	maybe_read_label_name(MaybeLabelName),
	(
		{ MaybeLabelName = yes(CallerLabel) },
		read_label_name(CalleeLabel),
		{ relation__add(CallGraph0, CallerLabel, CalleeLabel,
							CallGraph1) },
		process_prof_file_2(CallGraph1, CallGraph)
	;
		{ MaybeLabelName = no },
		{ CallGraph = CallGraph0 }
	).


%-----------------------------------------------------------------------------%


:- pred print_cliques(list(set(string)), io__state, io__state).
:- mode print_cliques(in, di, uo) is det.

print_cliques([]) --> [].
print_cliques([Clique | Cliques]) -->
	print_clique(Clique),
	print_cliques(Cliques).

:- pred print_clique(set(string), io__state, io__state).
:- mode print_clique(in, di, uo) is det.

print_clique(Clique) -->
	{ set__to_sorted_list(Clique, CliqueList) },
	print_list(CliqueList).

:- pred print_list(list(string), io__state, io__state).
:- mode print_list(in, di, uo) is det.

print_list([]) --> [].
print_list([C | Cs]) -->
	io__write_string(C),
	io__write_string("\n"),
	print_list(Cs).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
