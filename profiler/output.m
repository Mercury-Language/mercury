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
% Print's out the output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module output.

:- interface.

:- import_module output_prof_info.
:- import_module float, int, io, list, map, std_util, string.

:- pred output__main(output, map(string, int), io__state, io__state).
:- mode output__main(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

output__main(Output, IndexMap) -->
	{ Output = output(InfoMap, CallList, FlatList) },
	output__call_graph_headers,
	output_call_graph(CallList, InfoMap, IndexMap),
	output_alphabet_headers,
	output_alphabet_listing(IndexMap),
	output__flat_headers,
	output__flat_profile(FlatList, 0.0, InfoMap, IndexMap).


:- pred output__call_graph_headers(io__state, io__state).
:- mode output__call_graph_headers(di, uo) is det.

output__call_graph_headers -->
	io__write_string("call graph profile:\n"),
	io__write_string("\tSorted on the %time field.\n\n"),

	io__write_string("\tpredicate entries:\n\n"),

	io__write_string("index\tthe index of the predicate in the call graph\n"),
	io__write_string("\tlisting.\n\n"),

	io__write_string("%time\tthe percentage of the total running time of\n"),
	io__write_string("\tthe program spent in this predicate and it's\n"),
	io__write_string("\tdescendents.\n\n"),

	io__write_string("self\tthe number of second's spent actually executing\n"),
	io__write_string("\tthe predicate's own code.\n\n"),

	io__write_string("descendents\n"),
	io__write_string("\tthe number of second's spent executing the\n"),
	io__write_string("\tdescendent's of the current predicate.\n\n"),

	io__write_string("called\tthe number of times the current predicate is\n"),
	io__write_string("\tcalled (not counting self recursive calls).\n\n"),

	io__write_string("self\tthe number of self recursive calls.\n\n"), 

	io__write_string("name\tthe name of the current predicate.\n\n"),

	io__write_string("index\ta index number to locate the function easily.\n\n\n\n"),

	io__write_string("\tparent listings:\n\n"),

	io__write_string("self*\tthe number of seconds of the current predicates self\n"),
	io__write_string("\ttime due to calls from this parent.\n\n"),

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
	{ ProfNode = output_prof(	Name,		Percentage,
					_,		Self,
					Descendant,	TotalCalls,
					SelfCalls,	ParentList,
					ChildList
				)
	},

	% Set up all the output strings.
	{
	string__int_to_string(Index, IndexStr0),
	string__append_list(["[", IndexStr0, "] "], IndexStr),
	string__format("%40d             %s [%d]\n", 
			[i(SelfCalls),s(Name),i(Index)], SelfCallsString),
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

	output_formatted_parent_list(ParentList, IndexMap, TotalCalls),

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
	io__write_string(Name),
	io__write_string(" "),
	io__write_string(IndexStr),
	io__write_string("\n"),

	output_formatted_child_list(ChildList, IndexMap),

	(
		{ SelfCalls \= 0 }
	->
		io__write_string(SelfCallsString)
	;
		[]
	).
	

% output_formatted_parent_list:
%	Output's the parent list of the current predicate also look's for the
%	special case where the parent list is empty and hence the function
%	spontaneously came into existance.
%
:- pred output_formatted_parent_list(list(parent), map(string, int), int, 
							io__state, io__state).
:- mode output_formatted_parent_list(in, in, in, di, uo) is det.

output_formatted_parent_list([], _, _) --> 
	{ string__format("%67s", [s("<spontaneous>\n")], String) }, 
	io__write_string(String).
output_formatted_parent_list([Parent | Parents], IndexMap, TotalCalls) -->
	output_formatted_parent_list_2([Parent|Parents], IndexMap, TotalCalls).

:- pred output_formatted_parent_list_2(list(parent), map(string, int), int, 
							io__state, io__state).
:- mode output_formatted_parent_list_2(in, in, in, di, uo) is det.

output_formatted_parent_list_2([], _, _) --> [].
output_formatted_parent_list_2([Parent | Parents], IndexMap, TotalCalls) -->
	{ Parent = parent(LabelName,	_,		Self,
		 	  Descendant,	Calls
			 ),
	string__format("%20.2f %11.2f %7d/%-11d %s [%d]\n", [f(Self),
				f(Descendant), i(Calls), i(TotalCalls), 
				s(LabelName), i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_parent_list_2(Parents, IndexMap, TotalCalls).	

% output_formatted_child_list:
%	output's the child list of the current predicate.
%
:- pred output_formatted_child_list(list(child), map(string, int),
							io__state, io__state).
:- mode output_formatted_child_list(in, in, di, uo) is det.

output_formatted_child_list([], _) --> [].
output_formatted_child_list([Child | Children], IndexMap) -->
	{ Child = child(  LabelName,	_,		Self,
		 	  Descendant,	Calls, 		TotalCalls
			 ),
	string__format("%20.2f %11.2f %7d/%-11d %s [%d]\n", [f(Self),
				f(Descendant), i(Calls), i(TotalCalls), 
				s(LabelName), i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_child_list(Children, IndexMap).	


:- pred output__flat_headers(io__state, io__state).
:- mode output__flat_headers(di, uo) is det.

output__flat_headers -->
	io__write_string("\nflat profile:\n\n"),
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
	{ ProfNode = output_prof(	Name,		_DescPercentage,
					Percentage,	Self,
					Descendant,	TotalCalls,
					_SelfCalls,	_ParentList,
					_ChildList
				)
	},

	{
	int__to_float(TotalCalls, FloatTotalCalls),

	builtin_float_plus(Self, CumTime0, CumTime),
	builtin_float_divide(Self, FloatTotalCalls, Self1),
	builtin_float_divide(Descendant, FloatTotalCalls, Desc1),
	builtin_float_times(1000.0, Self1, SelfMs),
	builtin_float_times(1000.0, Desc1, DescMs),

	string__int_to_string(Index, IndexStr0),
	string__append_list(["[", IndexStr0, "] "], IndexStr),
	string__format("%5.1f %10.2f %8.2f %8d %8.2f %8.2f %s %s\n",
				[ f(Percentage),	f(CumTime),
				  f(Self),		i(TotalCalls),
				  f(SelfMs),		f(DescMs),
				  s(Name),		s(IndexStr)
				],
				String)
	},

	io__write_string(String),

	output__flat_profile(LNs, CumTime, InfoMap, IndexMap).


:- pred output_alphabet_headers(io__state, io__state).
:- mode output_alphabet_headers(di, uo) is det.

output_alphabet_headers -->
	io__write_string("\n\n\naplhabetic listing:\n\n").


:- pred output_alphabet_listing(map(string, int), io__state, io__state).
:- mode output_alphabet_listing(in, di, uo) is det.

output_alphabet_listing(IndexMap) -->
	{ map__to_assoc_list(IndexMap, IndexList) },
	output_alphabet_listing_2(IndexList).

:- pred output_alphabet_listing_2(assoc_list(string, int), io__state, io__state).
:- mode output_alphabet_listing_2(in, di, uo) is det.

output_alphabet_listing_2([]) --> [].
output_alphabet_listing_2([Name - Index | T]) -->
	{ string__format("[%d]\t%s\n", [i(Index), s(Name)], String) },
	io__write_string(String),
	output_alphabet_listing_2(T).
