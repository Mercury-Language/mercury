
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
:- import_module float, int, io, list, map, string.

:- pred output__main(list(output_prof), map(string, int), io__state, io__state).
:- mode output__main(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

output__main([], _) -->
	io__write_string("No profiling information to output.\n").
output__main([PN | PNs], IndexMap) -->
	io__write_string("                                  called/total"),
	io__write_string("       parents\n"),
	io__write_string("index  %time    self descendents  called+self"),
	io__write_string("    name           index\n"),
	io__write_string("                                  called/total"),
	io__write_string("       children\n\n"),
	output_prof_nodes([PN|PNs], IndexMap).


:- pred output_prof_nodes(list(output_prof), map(string, int), 
						io__state, io__state).
:- mode output_prof_nodes(in, in, di, uo) is det.

output_prof_nodes([], _) --> [].
output_prof_nodes([PN | PNs], IndexMap) -->
	output_formatted_prof_node(PN, IndexMap),
	io__write_string("-----------------------------------------------\n\n"),
	output_prof_nodes(PNs, IndexMap).
	
	
:- pred output_formatted_prof_node(output_prof, map(string, int), 
							io__state, io__state).
:- mode output_formatted_prof_node(in, in, di, uo) is det.

output_formatted_prof_node(ProfNode, IndexMap) -->
	{ ProfNode = output_prof(	LabelName,	Index,
					Percentage,	Self,
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
			[i(SelfCalls),s(LabelName),i(Index)], SelfCallsString),
	string__format("%6-s %5.1f %7.2f %11.2f %7d", [s(IndexStr), 
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
		io__write_string("        ")
	;
		io__write_string("+"),
		{ string__format("%7-d", [i(SelfCalls)], Str) },
		io__write_string(Str)
	),
	io__write_string(LabelName),
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
	string__format("%20.2f %11.2f %7d/%11-d %s [%d]\n", [f(Self),
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
	string__format("%20.2f %11.2f %7d/%11-d %s [%d]\n", [f(Self),
				f(Descendant), i(Calls), i(TotalCalls), 
				s(LabelName), i(Index)], Output),
	map__lookup(IndexMap, LabelName, Index)
	},
	io__write_string(Output),
	output_formatted_child_list(Children, IndexMap).	
