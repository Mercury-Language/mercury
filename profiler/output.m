%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% output.m
%
% Main author: petdr.
%
% Prints out the output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module output.

:- interface.

:- import_module string, int, map, io.
:- import_module output_prof_info.

:- pred output__main(output, map(string, int), io__state, io__state).
:- mode output__main(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list, bool, float, list, require, std_util.
:- import_module globals, options, generate_output.

output__main(Output, IndexMap) -->
	{ Output = output(InfoMap, CallList, FlatList) },
	globals__io_lookup_bool_option(call_graph, CallGraphOpt),
	(
		{ CallGraphOpt = yes } 
	->
		output__call_graph_headers,
		output_call_graph(CallList, InfoMap, IndexMap)
	;
		{ true }
	),
	output__flat_headers,
	output__flat_profile(FlatList, 0.0, InfoMap, IndexMap),
	output_alphabet_headers,
	output_alphabet_listing(IndexMap).


:- pred output__call_graph_headers(io__state, io__state).
:- mode output__call_graph_headers(di, uo) is det.

output__call_graph_headers -->
	io__write_string("call graph profile:\n"),
	io__write_string("\tSorted on the %time field.\n\n"),

	io__write_string("\tpredicate entries:\n\n"),

	io__write_string("index\tthe index number of the predicate in the call graph\n"),
	io__write_string("\tlisting.\n\n"),

	io__write_string("%time\tthe percentage of the total running time of\n"),
	io__write_string("\tthe program spent in this predicate and its\n"),
	io__write_string("\tdescendents.\n\n"),

	io__write_string("self\tthe number of seconds spent actually executing\n"),
	io__write_string("\tthe predicate's own code.\n\n"),

	io__write_string("descendents\n"),
	io__write_string("\tthe number of seconds spent executing the\n"),
	io__write_string("\tdescendents of the current predicate.\n\n"),

	io__write_string("called\tthe number of times the current predicate is\n"),
	io__write_string("\tcalled (not counting self recursive calls).\n\n"),

	io__write_string("self\tthe number of self recursive calls.\n\n"), 

	io__write_string("name\tthe name of the current predicate.\n\n"),

	io__write_string("index\ta index number to locate the function easily.\n\n\n\n"),

	io__write_string("\tparent listings:\n\n"),

	io__write_string("self*\tthe number of seconds of the current predicates self\n"),
	io__write_string("\ttime due to calls from this parent.\n\n"),

	io__write_string("descendents*\n"),
	io__write_string("\tthe number of seconds of the current predicate's descendent\n"),
	io__write_string("\ttime which is due to calls from this parent.\n\n"),

	io__write_string("called*\tthe number of times the current predicate is called\n"),
	io__write_string("\tby this parent.\n\n"),

	io__write_string("total\tthe number of times this predicate is called by its parents.\n\n"),

	io__write_string("parents\tthe name of this parent.\n\n"),

	io__write_string("index\tthe index number of the parent predicate\n\n\n\n"),



	io__write_string("children listings:\n\n"),

	io__write_string("self*\tthe number of seconds of this child's self time which is due\n"),
	io__write_string("\tto being called by the current predicate.\n\n"),

	io__write_string("descendent*\n"),
	io__write_string("\tthe number of seconds of this child's desdendent time which is due\n"),
	io__write_string("\tto the current predicate.\n\n"),

	io__write_string("called*\tthe number of times this child is called by the current\n"),
	io__write_string("\tpredicate.\n\n"),

	io__write_string("total*\tthe number of times this child is called by all predicates.\n\n"),

	io__write_string("children\tthe name of this child.\n\n"),

	io__write_string("index\tthe index number of the child.\n\n"),



	io__write_string("                                  called/total"),
	io__write_string("       parents\n"),
	io__write_string("index  %time    self descendents  called+self"),
	io__write_string("    name           index\n"),
	io__write_string("                                  called/total"),
	io__write_string("       children\n\n").

	
:- pred output_call_graph(list(string), map(string, output_prof), 
					map(string, int), io__state, io__state).
:- mode output_call_graph(in, in, in, di, uo) is det.

output_call_graph([], _, _) --> [].
output_call_graph([LabelName | LNs], InfoMap, IndexMap) -->
	{ map__lookup(InfoMap, LabelName, PN) },
	{ map__lookup(IndexMap, LabelName, Index) },
	output_formatted_prof_node(PN, Index, IndexMap),
	io__write_string("\n-----------------------------------------------\n\n"),
	output_call_graph(LNs, InfoMap, IndexMap).
	
	
:- pred output_formatted_prof_node(output_prof, int, map(string, int), 
							io__state, io__state).
:- mode output_formatted_prof_node(in, in, in, di, uo) is det.

output_formatted_prof_node(ProfNode, Index, IndexMap) -->
	(
		{ ProfNode = output_prof(	Name,		CycleNum,
						Percentage,
						_,		Self,
						Descendant,	TotalCalls,
						SelfCalls,	ParentList,
						ChildList,	CycleParentList,
						CycleChildList
					)
		}
	;
		{ ProfNode = output_cycle_prof(_, _, _, _, _, _, _, _, _) },
		{ error("output_formatted_prof_node: Cannot have output_cycle_prof\n") }
	),

	% Set up all the output strings.
	{
	output__construct_name(Name, CycleNum, FullName),
	string__int_to_string(Index, IndexStr0),
	string__append_list(["[", IndexStr0, "] "], IndexStr),
	string__format("%40d             %s [%d]\n", 
			[i(SelfCalls),s(FullName),i(Index)], SelfCallsString),
	string__format("%-6s %5.1f %7.2f %11.2f %7d", [s(IndexStr), 
			f(Percentage) , f(Self), f(Descendant), i(TotalCalls)],
			InitMiddleStr)
	},
		
	(
		{ SelfCalls \= 0 }
	->
		io__write_string(SelfCallsString)
	;
		[]
	),

	(	
		{ CycleParentList = [] },
		{ ParentList = [] }
	->
		{ string__format("%67s", [s("<spontaneous>\n")], String) }, 
		io__write_string(String)
	;
		output_formatted_cycle_parent_list(CycleParentList, IndexMap),
		output_formatted_parent_list(ParentList, IndexMap, TotalCalls)
	),


	% Output the info about the current predicate.
	io__write_string(InitMiddleStr),
	(
		{ SelfCalls = 0 }
	->
		io__write_string("         ")
	;
		io__write_string("+"),
		{ string__format("%-7d", [i(SelfCalls)], Str) },
		io__write_string(Str)
	),
	io__write_string(FullName),
	io__write_string(" "),
	io__write_string(IndexStr),
	io__write_string("\n"),

	output_formatted_child_list(ChildList, IndexMap),
	output_formatted_cycle_child_list(CycleChildList, IndexMap),

	(
		{ SelfCalls \= 0 }
	->
		io__write_string(SelfCallsString)
	;
		[]
	).
	

% output_formatted_cycle_parent_list
%	outputs the parents of a predicate that are in the same cycle.
%
:- pred output_formatted_cycle_parent_list(list(parent), map(string, int),
                                                        io__state, io__state).
:- mode output_formatted_cycle_parent_list(in, in, di, uo) is det.

output_formatted_cycle_parent_list([], _) --> [].
output_formatted_cycle_parent_list([Parent | Parents], IndexMap) -->
	{ Parent = parent(LabelName,	CycleNum,	_Self,
		 	  _Descendant,	Calls
			 ),

	output__construct_name(LabelName, CycleNum, Name),
	string__format("%40d             %s [%d]\n", 
			[i(Calls),s(Name),i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_cycle_parent_list(Parents, IndexMap).	
	

% output_formatted_parent_list:
%	Outputs the parent list of the current predicate 

:- pred output_formatted_parent_list(list(parent), map(string, int), int, 
							io__state, io__state).
:- mode output_formatted_parent_list(in, in, in, di, uo) is det.

output_formatted_parent_list([], _, _) --> [].
output_formatted_parent_list([Parent | Parents], IndexMap, TotalCalls) -->
	{ Parent = parent(LabelName,	CycleNum,	Self,
		 	  Descendant,	Calls
			 ),
	output__construct_name(LabelName, CycleNum, Name),
	string__format("%20.2f %11.2f %7d/%-11d %s [%d]\n", [f(Self),
				f(Descendant), i(Calls), i(TotalCalls), 
				s(Name), i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_parent_list(Parents, IndexMap, TotalCalls).	
	

% output_formatted_cycle_child_list
%	outputs the childs of a predicate that are in the same cycle.
%
:- pred output_formatted_cycle_child_list(list(child), map(string, int),
                                                        io__state, io__state).
:- mode output_formatted_cycle_child_list(in, in, di, uo) is det.

output_formatted_cycle_child_list([], _) --> [].
output_formatted_cycle_child_list([Child | Childs], IndexMap) -->
	{ Child = child(LabelName,	CycleNum,	_Self,
		 	  _Descendant,	Calls, _
			 ),

	output__construct_name(LabelName, CycleNum, Name),
	string__format("%40d             %s [%d]\n", 
			[i(Calls),s(Name),i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_cycle_child_list(Childs, IndexMap).	
	

% output_formatted_child_list:
%	outputs the child list of the current predicate.
%
:- pred output_formatted_child_list(list(child), map(string, int),
							io__state, io__state).
:- mode output_formatted_child_list(in, in, di, uo) is det.

output_formatted_child_list([], _) --> [].
output_formatted_child_list([Child | Children], IndexMap) -->
	{ Child = child(  LabelName,	CycleNum,	Self,
		 	  Descendant,	Calls, 		TotalCalls
			 ),
	output__construct_name(LabelName, CycleNum, Name),
	string__format("%20.2f %11.2f %7d/%-11d %s [%d]\n", [f(Self),
				f(Descendant), i(Calls), i(TotalCalls), 
				s(Name), i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_child_list(Children, IndexMap).	

:- pred output__flat_headers(io__state, io__state).
:- mode output__flat_headers(di, uo) is det.

output__flat_headers -->
	io__write_string("\nflat profile:\n\n"),

	io__write_string(" %\tthe percentage of total running time of the program\n"),
	io__write_string("time\tused by this function.\n\n"),

	io__write_string(" cum\tthe total time of the current predicate and the ones\n"),
	io__write_string("time\tlisted above it.\n\n"),

	io__write_string(" self\tthe number of seconds accounted for by this predicate alone.\n"),
	io__write_string("seconds\tThe listing is sorted on this row.\n\n"),

	io__write_string("calls\tthe number of times this predicate was called.\n\n"),

	io__write_string(" self\tthe average number of milliseconds spent in\n"),
	io__write_string("ms/call\tthis predicate per call.\n\n"),

	io__write_string(" total\tthe average number of milliseconds spent in this predicate and its\n"),
	io__write_string("ms/call\tdescendents per call.\n\n"),

	io__write_string("name\tthe name of the predicate followed by its index number.\n\n"),

	io__write_string("   %  cumulative    self              self"),
	io__write_string("    total\n"),
	io__write_string(" time   seconds   seconds    calls  ms/call"),
	io__write_string("  ms/call name\n").


:- pred output__flat_profile(list(string), float, map(string, output_prof),
					map(string, int), io__state, io__state).
:- mode output__flat_profile(in, in, in, in, di, uo) is det.

output__flat_profile([], _, _, _) --> [].
output__flat_profile([LabelName | LNs], CumTime0, InfoMap, IndexMap) -->
	{ map__lookup(InfoMap, LabelName, ProfNode) },
	{ map__lookup(IndexMap, LabelName, Index) },
	(
		{ ProfNode = output_prof(	Name,		CycleNum,
						_Percentage,
						Percentage,	Self,
						Descendant,	TotalCalls,
						SelfCalls,	_ParentList,
						_ChildList,     _,
						_
					)
		}
	;
		{ ProfNode = output_cycle_prof(_, _, _, _, _, _, _, _, _) },
		{ error("output_flat_profile: Cannot have output_cycle_prof\n")}
	),

	{
	int__to_float(SelfCalls, FloatSelfCalls),
	int__to_float(TotalCalls, FloatTotalCalls0),

	FloatTotalCalls is FloatTotalCalls0 + FloatSelfCalls,
	Calls is SelfCalls + TotalCalls,


	builtin_float_plus(Self, CumTime0, CumTime),
	checked_float_divide(Self, FloatTotalCalls, Self1),
	checked_float_divide(Descendant, FloatTotalCalls, Desc1),
	builtin_float_times(1000.0, Self1, SelfMs),
	builtin_float_times(1000.0, Desc1, DescMs),

	output__construct_name(Name, CycleNum, FullName),
	string__int_to_string(Index, IndexStr0),
	string__append_list(["[", IndexStr0, "] "], IndexStr),
	string__format("%5.1f %10.2f %8.2f %8d %8.2f %8.2f %s %s\n",
				[ f(Percentage),	f(CumTime),
				  f(Self),		i(Calls),
				  f(SelfMs),		f(DescMs),
				  s(FullName),		s(IndexStr)
				],
				String)
	},

	io__write_string(String),

	output__flat_profile(LNs, CumTime, InfoMap, IndexMap).


:- pred output_alphabet_headers(io__state, io__state).
:- mode output_alphabet_headers(di, uo) is det.

output_alphabet_headers -->
	io__write_string("\n\n\nalphabetic listing:\n\n").


:- pred output_alphabet_listing(map(string, int), io__state, io__state).
:- mode output_alphabet_listing(in, di, uo) is det.

output_alphabet_listing(IndexMap) -->
	{ map__to_assoc_list(IndexMap, IndexList) },
	output_alphabet_listing_2(IndexList).

:- pred output_alphabet_listing_2(assoc_list(string, int), io__state, io__state).
:- mode output_alphabet_listing_2(in, di, uo) is det.

output_alphabet_listing_2([]) --> 
	io__write_string("\n").
output_alphabet_listing_2([Name - Index | T]) -->
	{ string__format("[%d]\t%-30s", [i(Index), s(Name)], String) },
	io__write_string(String),
	output_alphabet_listing_3(T).

:- pred output_alphabet_listing_3(assoc_list(string, int), io__state, io__state).
:- mode output_alphabet_listing_3(in, di, uo) is det.

output_alphabet_listing_3([]) --> [].
output_alphabet_listing_3([Name - Index | T]) -->
	{ string__format("[%d]\t%-30s\n", [i(Index), s(Name)], String) },
	io__write_string(String),
	output_alphabet_listing_2(T).

% output__construct_name
%	Constructs an output name with an optional cycle number if required.
%
:- pred output__construct_name(string, int, string).
:- mode output__construct_name(in, in, out) is det.

output__construct_name(Name, CycleNum, FullName) :-
	(
		CycleNum = 0
	->
		FullName = Name
	;
		string__int_to_string(CycleNum, CycleStr),
		string__append_list([Name, "  <cycle ", CycleStr, ">"], FullName)
	).
