%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% generate_output.m
%
% Main author: petdr.
%
% Takes the prof structure and generates the output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module generate_output.

:- interface.
:- import_module float, int, string, map, io, prof_info, output_prof_info.

:- pred generate_output__main(prof, map(string, int), output,  
							io__state, io__state).
:- mode generate_output__main(in, out, out, di, uo) is det.

:- pred checked_float_divide(float::in, float::in, float::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, options.
:- import_module bool, list, rbtree, relation.

% :- import_module writeln.

	% rbtrees are used because they allow duplicate values to be stored.
	% This means that we can then convert to a sorted list of names which
	% can be used to lookup the output_prof map when we actually output.
:- type	profiling --->
	    	profiling(
			map(string, output_prof),   % associate name with the 
						    % output_prof structure.
			rbtree(float, string),	    % associate call graph 
						    % percentage with a name.
			rbtree(flat_key, string)    % as above except for flat 
						    % profile.
	    	).

:- type flat_key --->
		flat_key(
			float,	% per cent time in this predicate
			int	% number of calls to this predicate
		).


generate_output__main(Prof, IndexMap, Output) -->
	% Get intitial values of use.
	{ prof_get_entire(Prof, _, _, IntTotalCounts, _, ProfNodeMap, _) },
	{ _TotalCounts = float__float(IntTotalCounts) },

	{ map__values(ProfNodeMap, ProfNodeList) },

	{ profiling_init(OutputProf0) },

	globals__io_lookup_bool_option(very_verbose, VV),
	process_prof_node_list(ProfNodeList, Prof, VV, OutputProf0, OutputProf),
	
	{ OutputProf = profiling(InfoMap, CallTree, FlatTree) },

	{ rbtree__values(CallTree, CallList0) },
	{ rbtree__values(FlatTree, FlatList0) },

	{ assign_index_numbers(CallList0, IndexMap, CallList) },
	{ list__reverse(FlatList0, FlatList) },

	{ Output = output(InfoMap, CallList, FlatList) }.


:- pred process_prof_node_list(list(prof_node), prof, bool, 
				profiling, profiling, io__state, io__state).
:- mode process_prof_node_list(in, in, in, in, out, di, uo) is det.

process_prof_node_list([], _, _, OutputProf, OutputProf) --> [].
process_prof_node_list([PN | PNs], Prof, VVerbose, OutputProf0, OutputProf) -->
	(
		{ VVerbose = yes }
	->
		{ prof_node_get_pred_name(PN, LabelName) },
		io__write_string("\n\t% Processing "),
		io__write_string(LabelName)
	;
		[]
	),
	{ process_prof_node(PN, Prof, OutputProf0, OutputProf1) },
	process_prof_node_list(PNs, Prof, VVerbose, OutputProf1, OutputProf).


% process_prof_node:
%	This is the main function.  It converts the prof_node structure to the
%	output_prof structure.
%
:- pred process_prof_node(prof_node, prof, profiling, profiling). 
:- mode process_prof_node(in, in, in, out) is det.

process_prof_node(ProfNode, Prof, OutputProf0, OutputProf) :-
	prof_node_type(ProfNode, ProfNodeType),
	(
		ProfNodeType = cycle,
		OutputProf = OutputProf0
		% generate_output__cycle(ProfNode, Prof, OutputProf0,
								% OutputProf)
	;
		ProfNodeType = predicate,
		generate_output__single_predicate(ProfNode, Prof, OutputProf0,
								OutputProf)
	).


% generate_output__cycle
%	XXX
%
:- pred generate_output__cycle(prof_node, prof, profiling, profiling).
:- mode generate_output__cycle(in, in, in, out) is det.

generate_output__cycle(ProfNode, Prof, OutputProf0, OutputProf) :-
	prof_get_entire(Prof, Scale, _Units, IntTotalCounts, _, _,
								_CycleMap),
	TotalCounts = float__float(IntTotalCounts),

	prof_node_get_entire_cycle(ProfNode, Name, CycleNum, Initial, Prop, 
					_CycleMembers, TotalCalls, SelfCalls),

	OutputProf0 = profiling(InfoMap0, CallTree0, FreeTree),

	% Calculate proportion of time in current predicate and its
	% descendents as a percentage.
	% 
	InitialFloat = float__float(Initial),
	(
		TotalCounts = 0.0
	->
		DescPercentage = 0.0
	;
		DescPercentage is (InitialFloat + Prop) / TotalCounts * 100.0
	),

	% Calculate the self time spent in the current predicate.
	% Calculate the descendant time, which is the time spent in the 
	% current predicate and its descendants
	SelfTime is InitialFloat * Scale,
	DescTime is (InitialFloat+Prop) * Scale,

	OutputProfNode = output_cycle_prof(	Name, CycleNum, SelfTime, 
						DescPercentage,
						DescTime, TotalCalls, SelfCalls,
						[], []
					),

	map__det_insert(InfoMap0, Name, OutputProfNode, InfoMap),
	rbtree__insert_duplicate(CallTree0, DescPercentage, 
					Name, CallTree),

	OutputProf = profiling(InfoMap, CallTree, FreeTree).


% generate_output__single_predicate:
%	Fills out the output_prof structure when pred is a single predicate.
%
:- pred generate_output__single_predicate(prof_node, prof, profiling,profiling).
:- mode generate_output__single_predicate(in, in, in, out) is det.

generate_output__single_predicate(ProfNode, Prof, OutputProf0, OutputProf) :-
	prof_get_entire(Prof, Scale, _Units, IntTotalCounts, _, _, 
								CycleMap),
	TotalCounts = float__float(IntTotalCounts),

	prof_node_get_entire_pred(ProfNode, LabelName, CycleNum, Initial, Prop,
					ParentList, ChildList, TotalCalls,
					SelfCalls, NameList),	
	
	% Node only needs to be processed if it has a parent or a child.
	(
		ParentList = [],
		ChildList = []
	->
		OutputProf = OutputProf0
	;
		OutputProf0 = profiling(InfoMap0, CallTree0, FlatTree0),

		construct_name(NameList, Name0),
		string__append(LabelName, Name0, Name),

		% Calculate proportion of time in current predicate and its
		% descendents as a percentage.
		% Calculate proportion of time in current predicate 
		% as a percentage.
		InitialFloat = float__float(Initial),	
		(
			TotalCounts = 0.0
		->
			DescPercentage = 0.0,
			FlatPercentage = 0.0

		;
			DescPercentage is (InitialFloat + Prop) / TotalCounts 
								* 100.0,
			FlatPercentage is InitialFloat / TotalCounts * 100.0
		),

		% Calculate the self time spent in the current predicate.
		% Calculate the descendant time, which is the time spent in the 
		% current predicate and its descendants
		SelfTime is InitialFloat * Scale,
		DescTime is (InitialFloat+Prop) * Scale,

		process_prof_node_parents(ParentList, SelfTime, DescTime, 
				TotalCalls, CycleNum, CycleMap, 
				OutputParentList, OutputCycleParentList),
		process_prof_node_children(ChildList, CycleNum, CycleMap, 
				Prof, OutputChildList, OutputCycleChildList),

		OutputProfNode = output_prof(	Name,		CycleNum,
						DescPercentage,
						FlatPercentage,	SelfTime,
						DescTime,	TotalCalls,
						SelfCalls,	
						OutputParentList,
						OutputChildList,
						OutputCycleParentList,
						OutputCycleChildList
					),

		map__det_insert(InfoMap0, LabelName, OutputProfNode, InfoMap),
		rbtree__insert_duplicate(CallTree0, DescPercentage, 
						LabelName, CallTree),
		rbtree__insert_duplicate(FlatTree0, flat_key(FlatPercentage,
							TotalCalls), 
						LabelName, FlatTree),
		
		OutputProf = profiling(InfoMap, CallTree, FlatTree)
	).


% construct_name:
%	When more then one predicate maps to the same address.  This predicate
%	will build a string of all the different names separated by 'or's.
:- pred construct_name(list(string), string).
:- mode construct_name(in, out) is det.

construct_name([], "").
construct_name([Name | Names], NameStr) :-
	construct_name(Names, NameStr0),
	string__append(" or ", Name, NameStr1),
	string__append(NameStr1, NameStr0, NameStr).


% process_prof_node_parents:
%	generate the parents output structure.
%
:- pred process_prof_node_parents(list(pred_info), float, float, int, int,
					cycle_map, list(parent), list(parent)).
:- mode process_prof_node_parents(in, in, in, in, in, in, out, out) is det.

process_prof_node_parents(Parents0, SelfTime, DescTime, TotalCalls0, CycleNum,
			CycleMap, OutputParentList, OutputCycleParentList) :-
	remove_cycle_members(Parents0, TotalCalls0, CycleNum, CycleMap, 
				TotalCalls, Parents, OutputCycleParentList),
	FltTotalCalls = float__float(TotalCalls),
	process_prof_node_parents_2(Parents, SelfTime, DescTime, FltTotalCalls,
						CycleMap, OutputParentList).


% remove_cycle_members
%	removes any members of the same cycle from the parent listing
%	of a predicate.  Then adjusts the total calls so as not to include
%	that predicate.
%
:- pred remove_cycle_members(list(pred_info), int, int, cycle_map, int,
						list(pred_info), list(parent)).
:- mode remove_cycle_members(in, in, in, in, out, out, out) is det.

remove_cycle_members([], TotalCalls, _, _, TotalCalls, [], []).
remove_cycle_members([PN | PNs], TotalCalls0, CycleNum, CycleMap, TotalCalls,
						List, OutputCycleParentList) :-
	pred_info_get_entire(PN, LabelName, Calls),
	(
		map__search(CycleMap, LabelName, ParentCycleNum)
	->
		(
			ParentCycleNum = CycleNum
		->
			% writeln("Throwing away parent "),
			% writeln(LabelName),

			TotalCalls1 is TotalCalls0 - Calls,
			remove_cycle_members(PNs, TotalCalls1, CycleNum, 
					CycleMap, TotalCalls, List, OC0),
			Parent = parent(LabelName, CycleNum, 0.0, 0.0, Calls),
			OutputCycleParentList = [ Parent | OC0 ]
		;
			remove_cycle_members(PNs, TotalCalls0, CycleNum, 
					CycleMap, TotalCalls, List0, OC0),
			OutputCycleParentList = OC0,
			List = [PN | List0]
		)
	;
		remove_cycle_members(PNs, TotalCalls0, CycleNum, CycleMap, 
				TotalCalls, List0, OutputCycleParentList),
		List = [PN | List0]
	).
									

:- pred process_prof_node_parents_2(list(pred_info), float, float, float, 
						cycle_map, list(parent)).
:- mode process_prof_node_parents_2(in, in, in, in, in, out) is det.

process_prof_node_parents_2([], _, _, _, _, []).
process_prof_node_parents_2([P | Ps], SelfTime, DescTime, TotalCalls, 
						CycleMap, OutputParentList) :-
	rbtree__init(Output0),
	process_prof_node_parents_3([P | Ps], SelfTime, DescTime, 
						TotalCalls, CycleMap,
						Output0, Output),
	rbtree__values(Output, OutputParentList).


:- pred process_prof_node_parents_3(list(pred_info), float, float, float, 
			cycle_map, rbtree(int, parent), rbtree(int, parent)).
:- mode process_prof_node_parents_3(in, in, in, in, in, in, out) is det.

process_prof_node_parents_3([], _, _, _, _, Output, Output).
process_prof_node_parents_3([PN | PNs], SelfTime, DescTime, TotalCalls, 
					CycleMap, Output0, Output) :-
	pred_info_get_entire(PN, LabelName, Calls),

	(
		        % if parent member of cycle
		map__search(CycleMap, LabelName, ParentCycleNum0) 
	->
		ParentCycleNum = ParentCycleNum0
	;
		ParentCycleNum = 0
	),

        FloatCalls = float__float(Calls),
        checked_float_divide(FloatCalls, TotalCalls, Proportion),

	% Calculate the amount of the current predicate's self-time spent
        % due to the parent.
        % and the amount of the current predicate's descendant-time spent
        % due to the parent.
        PropSelfTime is SelfTime * Proportion,
        PropDescTime is DescTime * Proportion,
	
	Parent = parent(LabelName, ParentCycleNum, PropSelfTime, 
							PropDescTime, Calls),
	rbtree__insert_duplicate(Output0, Calls, Parent, Output1),
	
	process_prof_node_parents_3(PNs, SelfTime, DescTime, TotalCalls, 
						CycleMap, Output1, Output).


:- pred process_prof_node_children(list(pred_info), int, cycle_map, 
						prof, list(child), list(child)).
:- mode process_prof_node_children(in, in, in, in, out, out) is det.

process_prof_node_children([], _, _, _, [], []).
process_prof_node_children([C | Cs], CycleNum, CycleMap, Prof, OutputChildList, 
						OutputCycleChildList) :-
	remove_child_cycle_members([C|Cs], CycleNum, CycleMap, Children, 
							OutputCycleChildList),
	rbtree__init(Output0),
	process_prof_node_children_2(Children, Prof, Output0, Output),
	rbtree__values(Output, OutputChildList).

% remove_child_cycle_members
%	removes any members of the same cycle from the child listing
%	of a predicate and adds them to a new list
%
:- pred remove_child_cycle_members(list(pred_info), int, cycle_map, 
						list(pred_info), list(child)).
:- mode remove_child_cycle_members(in, in, in, out, out) is det.

remove_child_cycle_members([], _, _, [], []).
remove_child_cycle_members([PN | PNs], CycleNum, CycleMap, List, 
							CycleChildList) :-
	pred_info_get_entire(PN, LabelName, Calls),
	(
		map__search(CycleMap, LabelName, ChildCycleNum)
	->
		(
			ChildCycleNum = CycleNum
		->
			% writeln("Throwing away child "),
			% writeln(LabelName),

			remove_child_cycle_members(PNs, CycleNum, CycleMap,
								List, OC0),
			Child = child(LabelName, CycleNum, 0.0, 0.0, Calls, 0),
			CycleChildList = [ Child | OC0 ]
		;
			remove_child_cycle_members(PNs, CycleNum, CycleMap,
								List0, OC0),
			CycleChildList = OC0,
			List = [PN | List0]
		)
	;
		remove_child_cycle_members(PNs, CycleNum, CycleMap, List0, 
								CycleChildList),
		List = [PN | List0]
	).

									
:- pred process_prof_node_children_2(list(pred_info), prof, rbtree(int, child),
							rbtree(int, child)).
:- mode process_prof_node_children_2(in, in, in, out) is det.

process_prof_node_children_2([], _, Output, Output).
process_prof_node_children_2([PN | PNs], Prof, Output0, Output) :-
	pred_info_get_entire(PN, LabelName, Calls),
	prof_get_entire(Prof, Scale, _Units, _, AddrMap, ProfNodeMap, 
								CycleMap),

	(
		map__search(CycleMap, LabelName, CycleNum0)
	->
		CycleNum = CycleNum0
	;
		CycleNum = 0
	),

	get_prof_node(LabelName, AddrMap, ProfNodeMap, ProfNode),
	prof_node_get_initial_counts(ProfNode, Initial),
	prof_node_get_propagated_counts(ProfNode, Prop),
	prof_node_get_total_calls(ProfNode, TotalCalls),

	InitialFloat = float__float(Initial),
	CurrentCount is InitialFloat + Prop,

	FloatTotalCalls = float__float(TotalCalls),
	FloatCalls = float__float(Calls),
        checked_float_divide(FloatCalls, FloatTotalCalls, Proportion),

	% Calculate the self time spent in the current predicate.
	SelfTime is InitialFloat * Scale,

	% Calculate the descendant time, which is the time spent in the 
	% current predicate and its descendants
	DescTime is CurrentCount * Scale,

	% Calculate the amount of the current predicate's self-time spent
        % due to the parent.
        % and the amount of the current predicate's descendant-time spent
        % due to the parent.
        PropSelfTime is SelfTime * Proportion,
        PropDescTime is DescTime * Proportion,
	
	Child = child(LabelName, CycleNum, PropSelfTime, PropDescTime, Calls, 
								TotalCalls),
	rbtree__insert_duplicate(Output0, Calls, Child, Output1),
	process_prof_node_children_2(PNs, Prof, Output1, Output).
	


% assign_index_numbers:
%	Reverses the output list so that the predicates which account for
%	most of the time come first and then assigns index numbers.
%
:- pred assign_index_numbers(list(string), map(string, int), list(string)).
:- mode assign_index_numbers(in, out, out) is det.

assign_index_numbers(List0, IndexMap, List) :-
	map__init(IndexMap0),
	list__reverse(List0, List),

	assign_index_numbers_2(List, IndexMap0, 1, IndexMap).


:- pred assign_index_numbers_2(list(string), map(string, int), int,
							map(string, int)).
:- mode assign_index_numbers_2(in, in, in, out) is det.

assign_index_numbers_2([], IndexMap, _, IndexMap).
assign_index_numbers_2([X0|Xs0], IndexMap0, N0, IndexMap) :-
	map__det_insert(IndexMap0, X0, N0, IndexMap1),
	N is N0 + 1,
	assign_index_numbers_2(Xs0, IndexMap1, N, IndexMap).


:- pred profiling_init(profiling).
:- mode profiling_init(out) is det.

profiling_init(Profiling) :-
	map__init(InfoMap),
	rbtree__init(CallTree),
	rbtree__init(FlatTree),
	Profiling = profiling(InfoMap, CallTree, FlatTree).

checked_float_divide(A, B, C) :-
	( B = 0.0 ->
		C = 0.0
	;
		C is A / B
	).

