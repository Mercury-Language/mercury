%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

:- import_module globals, options.
:- import_module output_prof_info, prof_info.
:- import_module bool, float, int, io, list, map, rbtree, relation, string.

:- pred generate_output__main(prof, map(string, int), output,  
							io__state, io__state).
:- mode generate_output__main(in, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

	% rbtrees are used because they allow duplicate values to be stored.
	% This means that we can then convert to a sorted list of names which
	% can be used to lookup the output_prof map when we actually output.
:- type	profiling --->
	    	profiling(
			map(string, output_prof),   % associate name with the 
						    % output_prof structure.
			rbtree(float, string),	    % associate call graph 
						    % percentage with a name.
			rbtree(float, string)	    % as above except for flat 
						    % profile.
	    	).


generate_output__main(Prof, IndexMap, Output) -->
	% Get intitial values of use.
	{ Prof = prof(_Hz, _ClckTcks, IntTotalCounts, _AddrMap, ProfNodeMap) },
	{ int__to_float(IntTotalCounts, _TotalCounts) },

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
		io__write_string("\n\t% Processing predicate "),
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
	Prof = prof(Hertz, ClockTicks, IntTotalCounts, _AddrMap, _ProfNodeMap),
	int__to_float(IntTotalCounts, TotalCounts),

	ProfNode = prof_node( LabelName,	 _Index,
				Initial,	 Prop,
				ParentList,	 ChildList,
				TotalCalls,	 SelfCalls,
				NameList		
			),	
	
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
		int__to_float(Initial, InitialFloat),
		builtin_float_plus(InitialFloat, Prop, CurrentCount),
		builtin_float_divide(CurrentCount, TotalCounts, Proportion),
		builtin_float_times(100.0, Proportion, DescPercentage),

		% Calculate proportion of time in current predicate 
		% as a percentage.
		builtin_float_divide(InitialFloat, TotalCounts, Proportion2),
		builtin_float_times(100.0, Proportion2, FlatPercentage),

		% Calculate the self time spent in the current predicate.
		int__to_float(Hertz, HertzFloat),
		int__to_float(ClockTicks, ClockTicksFloat),
		builtin_float_divide(InitialFloat, HertzFloat, InterA),
		builtin_float_times(InterA, ClockTicksFloat, Selftime),

		% Calculate the descendant time, which is the time spent in the 
		% current predicate and its descendants
		builtin_float_divide(CurrentCount, HertzFloat, InterB),
		builtin_float_times(InterB, ClockTicksFloat, DescTime),

		int__to_float(TotalCalls, FloatTotalCalls),
		process_prof_node_parents(ParentList, Selftime, DescTime, 
					FloatTotalCalls, OutputParentList),
		process_prof_node_children(ChildList, Prof, OutputChildList),

		OutputProfNode = output_prof(	Name,		DescPercentage,
						FlatPercentage,	Selftime,
						DescTime,	TotalCalls,
						SelfCalls,	
						OutputParentList,
						OutputChildList
					),

		map__det_insert(InfoMap0, LabelName, OutputProfNode, InfoMap),
		rbtree__insert_duplicate(CallTree0, DescPercentage, 
						LabelName, CallTree),
		rbtree__insert_duplicate(FlatTree0, FlatPercentage, 
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
:- pred process_prof_node_parents(list(pred_info), float, float, float, 
								list(parent)).
:- mode process_prof_node_parents(in, in, in, in, out) is det.

process_prof_node_parents([], _, _, _, []).
process_prof_node_parents([P | Ps], Selftime, DescTime, TotalCalls,
							OutputParentList) :-
	rbtree__init(Output0),
	process_prof_node_parents_2([P | Ps], Selftime, DescTime, TotalCalls,
							Output0, Output),
	rbtree__values(Output, OutputParentList).

:- pred process_prof_node_parents_2(list(pred_info), float, float, float,
				rbtree(int, parent), rbtree(int, parent)).
:- mode process_prof_node_parents_2(in, in, in, in, in, out) is det.

process_prof_node_parents_2([], _, _, _, Output, Output).
process_prof_node_parents_2([pred_info(LabelName, Calls) | PNs], Selftime, 
				DescTime, TotalCalls, Output0, Output) :-
        int__to_float(Calls, FloatCalls),
        builtin_float_divide(FloatCalls, TotalCalls, Proportion),

	% Calculate the amount of the current predicate's self-time spent
        % due to the parent.
        % and the amount of the current predicate's descendant-time spent
        % due to the parent.
        builtin_float_times(Selftime, Proportion, PropSelftime),
        builtin_float_times(DescTime, Proportion, PropDescTime),
	
	Parent = parent(LabelName, 0, PropSelftime, PropDescTime, Calls),
	rbtree__insert_duplicate(Output0, Calls, Parent, Output1),
	
	process_prof_node_parents_2(PNs, Selftime, DescTime, TotalCalls,
							Output1, Output).


:- pred process_prof_node_children(list(pred_info), prof, list(child)).
:- mode process_prof_node_children(in, in, out) is det.

process_prof_node_children([], _, []).
process_prof_node_children([C | Cs], Prof, OutputChildList) :-
	rbtree__init(Output0),
	process_prof_node_children_2([C | Cs], Prof, Output0, 
									Output),
	rbtree__values(Output, OutputChildList).

:- pred process_prof_node_children_2(list(pred_info), prof, rbtree(int, child),
							rbtree(int, child)).
:- mode process_prof_node_children_2(in, in, in, out) is det.

process_prof_node_children_2([], _, Output, Output).
process_prof_node_children_2([pred_info(LabelName, Calls) | PNs], Prof,
							Output0, Output) :-
	Prof = prof(Hertz, ClockTicks, _IntTotalCounts, AddrMap, ProfNodeMap),
	get_prof_node(LabelName, AddrMap, ProfNodeMap, ProfNode),
	prof_node_get_initial_counts(ProfNode, Initial),
	prof_node_get_propagated_counts(ProfNode, Prop),
	prof_node_get_total_calls(ProfNode, TotalCalls),

	int__to_float(Initial, InitialFloat),
	builtin_float_plus(InitialFloat, Prop, CurrentCount),

	int__to_float(TotalCalls, FloatTotalCalls),
        int__to_float(Calls, FloatCalls),
        builtin_float_divide(FloatCalls, FloatTotalCalls, Proportion),

	% Calculate the self time spent in the current predicate.
	int__to_float(Hertz, HertzFloat),
	int__to_float(ClockTicks, ClockTicksFloat),
	builtin_float_divide(InitialFloat, HertzFloat, InterA),
	builtin_float_times(InterA, ClockTicksFloat, Selftime),

	% Calculate the descendant time, which is the time spent in the 
	% current predicate and its descendants
	builtin_float_divide(CurrentCount, HertzFloat, InterB),
	builtin_float_times(InterB, ClockTicksFloat, DescTime),

	% Calculate the amount of the current predicate's self-time spent
        % due to the parent.
        % and the amount of the current predicate's descendant-time spent
        % due to the parent.
        builtin_float_times(Selftime, Proportion, PropSelftime),
        builtin_float_times(DescTime, Proportion, PropDescTime),
	
	Child = child(LabelName, 0, PropSelftime, PropDescTime, Calls, 
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

