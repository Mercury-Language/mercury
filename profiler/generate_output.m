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
% Take's the prof structure and generate's the output.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module generate_output.

:- interface.

:- import_module globals, options.
:- import_module output_prof_info, prof_info.
:- import_module float, int, io, list, map, rbtree, relation, std_util.

:- pred generate_output__main(prof, map(string, int), list(output_prof),  
							io__state, io__state).
:- mode generate_output__main(in, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

generate_output__main(Prof, IndexMap, OutputProfList) -->
	% Get intitial values of use.
	{ Prof = prof(_Hertz, IntTotalCounts, _AddrMap, ProfNodeMap) },
	{ int__to_float(IntTotalCounts, _TotalCounts) },

	{ map__values(ProfNodeMap, ProfNodeList) },
	{ rbtree__init(OutputProf0) },

	globals__io_lookup_bool_option(very_verbose, VV),
	process_prof_node_list(ProfNodeList, Prof, VV, OutputProf0, OutputProf),
	
	{ rbtree__flatten_values(OutputProf, OutputProfList0) },
	{ assign_index_numbers(OutputProfList0, IndexMap, OutputProfList) }.

:- pred process_prof_node_list(list(prof_node), prof, bool, 
			rbtree(float, output_prof), rbtree(float, output_prof), 
							io__state, io__state).
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


:- pred process_prof_node(prof_node, prof, rbtree(float, output_prof), 
						rbtree(float, output_prof)).
:- mode process_prof_node(in, in, in, out) is det.

process_prof_node(ProfNode, Prof, OutputProf0, OutputProf) :-
	Prof = prof(Hertz, IntTotalCounts, _AddrMap, _ProfNodeMap),
	int__to_float(IntTotalCounts, TotalCounts),

	ProfNode = prof_node( LabelName,	 _Index,
				Initial,	 Prop,
				ParentList,	 ChildList,
				TotalCalls,	 SelfCalls,
				_NameList		
			),	
	
	% Node only need's to be processed if it has a parent or a child.
	(
		ParentList = [],
		ChildList = []
	->
		OutputProf = OutputProf0
	;
		% Calculate proportion of time in current predicate as a 
		% percentage.
		int__to_float(Initial, InitialFloat),
		builtin_float_plus(InitialFloat, Prop, CurrentCount),
		builtin_float_divide(CurrentCount, TotalCounts, Proportion),
		builtin_float_times(100.0, Proportion, Percentage),

		% Calculate the self time spent in the current predicate.
		% XXX 5.0 is the number of clock tick's need's to be command 
		% line option.
		int__to_float(Hertz, HertzFloat),
		builtin_float_divide(InitialFloat, HertzFloat, InterA),
		builtin_float_times(InterA, 5.0, Selftime),

		% Calculate the descendant time, which is the time spent in the 
		% current predicate and it's descendant's
		builtin_float_divide(CurrentCount, HertzFloat, InterB),
		builtin_float_times(InterB, 5.0, DescTime),

		int__to_float(TotalCalls, FloatTotalCalls),
		process_prof_node_parents(ParentList, Selftime, DescTime, 
					FloatTotalCalls, OutputParentList),
		process_prof_node_children(ChildList, Prof, OutputChildList),

		OutputProfNode = output_prof(	LabelName,	0,
						Percentage,	Selftime,
						DescTime,	TotalCalls,
						SelfCalls,	
						OutputParentList,
						OutputChildList
					),

		rbtree__insert_duplicate(OutputProf0, Percentage,
						OutputProfNode, OutputProf)
	).


:- pred process_prof_node_parents(list(pred_info), float, float, float, 
								list(parent)).
:- mode process_prof_node_parents(in, in, in, in, out) is det.

process_prof_node_parents([], _, _, _, []).
process_prof_node_parents([P | Ps], Selftime, DescTime, TotalCalls,
							OutputParentList) :-
	rbtree__init(Output0),
	process_prof_node_parents_2([P | Ps], Selftime, DescTime, TotalCalls,
							Output0, Output),
	rbtree__flatten_values(Output, OutputParentList).

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
	rbtree__flatten_values(Output, OutputChildList).

:- pred process_prof_node_children_2(list(pred_info), prof, rbtree(int, child),
							rbtree(int, child)).
:- mode process_prof_node_children_2(in, in, in, out) is det.

process_prof_node_children_2([], _, Output, Output).
process_prof_node_children_2([pred_info(LabelName, Calls) | PNs], Prof,
							Output0, Output) :-
	Prof = prof(Hertz, _IntTotalCounts, AddrMap, ProfNodeMap),
	get_prof_node(LabelName, AddrMap, ProfNodeMap, ProfNode),
	prof_node_get_initial_counts(ProfNode, Initial),
	prof_node_get_propogated_counts(ProfNode, Prop),
	prof_node_get_total_calls(ProfNode, TotalCalls),

	int__to_float(Initial, InitialFloat),
	builtin_float_plus(InitialFloat, Prop, CurrentCount),

	int__to_float(TotalCalls, FloatTotalCalls),
        int__to_float(Calls, FloatCalls),
        builtin_float_divide(FloatCalls, FloatTotalCalls, Proportion),

	% Calculate the self time spent in the current predicate.
	% XXX 5.0 is the number of clock tick's need's to be command 
	% line option.
	int__to_float(Hertz, HertzFloat),
	builtin_float_divide(InitialFloat, HertzFloat, InterA),
	builtin_float_times(InterA, 5.0, Selftime),

	% Calculate the descendant time, which is the time spent in the 
	% current predicate and it's descendant's
	builtin_float_divide(CurrentCount, HertzFloat, InterB),
	builtin_float_times(InterB, 5.0, DescTime),

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
%	Reverse's the output list so that the predicate's which account for
%	most of the time come first and then assign's index numbers.
%
:- pred assign_index_numbers(list(output_prof), map(string, int), 
							list(output_prof)).
:- mode assign_index_numbers(in, out, out) is det.

assign_index_numbers(List0, IndexMap, List) :-
	map__init(IndexMap0),
	list__reverse(List0, List1),

	assign_index_numbers_2(List1, IndexMap0, 1, List, IndexMap).


:- pred assign_index_numbers_2(list(output_prof), map(string, int), int,
					list(output_prof), map(string, int)).
:- mode assign_index_numbers_2(in, in, in, out, out) is det.

assign_index_numbers_2([], IndexMap, _, [], IndexMap).
assign_index_numbers_2([X0|Xs0], IndexMap0, N0, [X|Xs], IndexMap) :-
	X0 = output_prof(LabelName,	_,		Percentage,
			Self,		Descendant,	Calls,
			SelfCalls,	ParentList,	ChildList),
	X = output_prof(LabelName,	N0,		Percentage,
			Self,           Descendant,     Calls,
			SelfCalls,      ParentList,     ChildList),
	map__det_insert(IndexMap0, LabelName, N0, IndexMap1),
	N is N0 + 1,
	assign_index_numbers_2(Xs0, IndexMap1, N, Xs, IndexMap).
