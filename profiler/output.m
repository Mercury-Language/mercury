%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2004 The University of Melbourne.
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
	globals__io_get_globals(Globals),
	{ globals__get_what_to_profile(Globals, WhatToProfile) },
	{ what_to_profile(WhatToProfileString, WhatToProfile) },
	io__format("*** profiling %s ***\n\n", [s(WhatToProfileString)]),

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

:- type header_category
	--->	time_headers
	;	memory_words_headers
	;	memory_cells_headers.

:- pred classify_profile(what_to_profile::in, header_category::out) is det.
classify_profile(user_time, time_headers).
classify_profile(user_plus_system_time, time_headers).
classify_profile(real_time, time_headers).
classify_profile(memory_words, memory_words_headers).
classify_profile(memory_cells, memory_cells_headers).

:- pred units(header_category::in, string::out, string::out, string::out, string::out,
		string::out, string::out, string::out, string::out) is det.
units(time_headers,	"time", "time", "running time",
			"seconds", "seconds", "milliseconds", "ms/call", "spent executing").
units(memory_words_headers,
			"mem", "memory", "allocated memory",
			"k-words", "kilowords", "words", "wds/call", "allocated by").
units(memory_cells_headers,
			"cells", "allocations", "number of memory allocations",
			"k-cells", "kilocells", "cells", "cls/call", "occurring in").

:- pred output__call_graph_headers(io__state, io__state).
:- mode output__call_graph_headers(di, uo) is det.

output__call_graph_headers -->
	globals__io_get_globals(Globals),
	{ globals__get_what_to_profile(Globals, WhatToProfile) },
	{ classify_profile(WhatToProfile, Category) },
	{ units(Category, ShortWhat, What, LongWhat,
			_ShortUnits, Units, _MilliUnits, _MilliUnitsPerCall, SpentIn) },

	io__write_string("call graph profile:\n"),
	io__format("\tSorted on the %%%s field.\n\n", [s(ShortWhat)]),

	io__write_string("\tprocedure entries:\n\n"),

	io__write_string("index\t\tthe index number of the procedure in the call graph\n"),
	io__write_string("\t\tlisting.\n\n"),

	io__format("%%%s\t\tthe percentage of the total %s of\n",
		[s(ShortWhat), s(LongWhat)]),
	io__format("\t\tthe program %s this procedure and its\n",
		[s(SpentIn)]),
	io__write_string("\t\tdescendents.\n\n"),

	io__format("self\t\tthe number of %s actually %s\n",
		[s(Units), s(SpentIn)]),
	io__write_string("\t\tthe procedure's own code.\n\n"),

	io__format("descendents\tthe number of %s %s the\n",
		[s(Units), s(SpentIn)]),
	io__write_string("\t\tdescendents of the current procedure.\n\n"),

	io__write_string("called\t\tthe number of times the current procedure is\n"),
	io__write_string("\t\tcalled (not counting self recursive calls).\n\n"),

	io__write_string("self\t\tthe number of self recursive calls.\n\n"), 

	io__write_string("name\t\tthe name of the current procedure.\n\n"),

	io__write_string("index\t\tan index number to locate the function easily.\n\n\n\n"),

	io__write_string("\tparent listings:\n\n"),

	io__format("self*\t\tthe number of %s of the current procedure's self\n",
		[s(Units)]),
	io__format("\t\t%s due to calls from this parent.\n\n",
		[s(What)]),

	io__format("descendents*\tthe number of %s of the current procedure's descendent\n",
		[s(Units)]),
	io__format("\t\t%s which is due to calls from this parent.\n\n",
		[s(What)]),

	io__write_string("called*\t\tthe number of times the current procedure is called\n"),
	io__write_string("\t\tby this parent.\n\n"),

	io__write_string("total\t\tthe number of times this procedure is called by its parents.\n\n"),

	io__write_string("parents\t\tthe name of this parent.\n\n"),

	io__write_string("index\t\tthe index number of the parent procedure\n\n\n\n"),



	io__write_string("children listings:\n\n"),

	io__format("self*\t\tthe number of %s of this child's self %s which is\n",
		[s(Units), s(What)]),
	io__write_string("\t\tdue to being called by the current procedure.\n\n"),

	io__format("descendent*\tthe number of %s of this child's descendent %s which\n",
		[s(Units), s(What)]),
	io__write_string("\t\tis due to the current procedure.\n\n"),

	io__write_string("called*\t\tthe number of times this child is called by the current\n"),
	io__write_string("\t\tprocedure.\n\n"),

	io__write_string("total*\t\tthe number of times this child is called by all procedures.\n\n"),

	io__write_string("children\tthe name of this child.\n\n"),

	io__write_string("index\t\tthe index number of the child.\n\n\n\n"),



	io__write_string("                                  called/total"),
	io__write_string("       parents\n"),
	{ string__append("%", ShortWhat, PercentShortWhat) },
	io__format("index %6s    self descendents  called+self",
		[s(PercentShortWhat)]),
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
		{ list__sort(CycleParentList, SortedCycleParentList) },
		output_formatted_cycle_parent_list(SortedCycleParentList,
			IndexMap),
		{ list__sort(ParentList, SortedParentList) },
		output_formatted_parent_list(SortedParentList, IndexMap,
			TotalCalls)
	),


	% Output the info about the current procedure.
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

	{ list__sort(ChildList, SortedChildList) },
	output_formatted_child_list(SortedChildList, IndexMap),
	{ list__sort(CycleChildList, SortedCycleChildList) },
	output_formatted_cycle_child_list(SortedCycleChildList, IndexMap),

	(
		{ SelfCalls \= 0 }
	->
		io__write_string(SelfCallsString)
	;
		[]
	).
	

% output_formatted_cycle_parent_list
%	outputs the parents of a procedure that are in the same cycle.
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
%	Outputs the parent list of the current procedure 

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
%	outputs the children of a procedure that are in the same cycle.
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
%	outputs the child list of the current procedure.
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
	globals__io_get_globals(Globals),
	{ globals__get_what_to_profile(Globals, WhatToProfile) },
	{ classify_profile(WhatToProfile, Category) },
	{ units(Category, ShortWhat, _What, LongWhat,
			ShortUnits, Units, MilliUnits, MilliUnitsPerCall, SpentIn) },

	io__write_string("\nflat profile:\n\n"),

	io__format("%%\t\tthe percentage of total %s of the program\n",
		[s(LongWhat)]),
	io__format("%s\t\tused by this procedure.\n\n",
		[s(ShortWhat)]),

	io__format(
		"cumulative\tthe total number of %s for the current procedure and\n",
		[s(Units)]),
	io__format("%s\t\tthe ones listed above it.\n\n",
		[s(ShortUnits)]),

	io__format(
	    "self\t\tthe number of %s accounted for by this procedure alone.\n",
		[s(Units)]),
	io__format("%s\t\tThe listing is sorted on this row.\n\n",
		[s(ShortUnits)]),

	io__write_string(
		"calls\t\tthe number of times this procedure was called.\n\n"),

	io__format("self\t\tthe average number of %s %s\n",
		[s(MilliUnits), s(SpentIn)]),
	io__format("%s  \tthis procedure per call.\n\n",
		[s(MilliUnitsPerCall)]),

	io__format(
	   "total\t\tthe average number of %s %s this procedure and its\n",
		[s(MilliUnits), s(SpentIn)]),
	io__format("%s  \tdescendents per call.\n\n",
		[s(MilliUnitsPerCall)]),

	io__write_string(
	  "name\t\tthe name of the procedure followed by its index number.\n\n"),

	io__write_string("   %  cumulative    self              self"),
	io__write_string("    total\n"),
	io__format(" %4s    %7s  %7s    calls %8s %8s name\n",
		[s(ShortWhat), s(ShortUnits), s(ShortUnits),
		 s(MilliUnitsPerCall), s(MilliUnitsPerCall)]).


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
	FloatSelfCalls = float__float(SelfCalls),
	FloatTotalCalls0 = float__float(TotalCalls),
	FloatTotalCalls is FloatTotalCalls0 + FloatSelfCalls,
	Calls is SelfCalls + TotalCalls,
	CumTime is CumTime0 + Self,
	checked_float_divide(Self, FloatTotalCalls, SelfSeconds),
	checked_float_divide(Descendant, FloatTotalCalls, DescSeconds),
	SelfMs is 1000.0 * SelfSeconds,
	DescMs is 1000.0 * DescSeconds,

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
