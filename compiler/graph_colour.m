%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
%
% file: graph_colour.nl
% main author: conway.
%
% This file contains functionality to find a 'good' colouring of a graph.
% The predicate graph_colour__group_elements(set(set(T)), set(set(T))),
% takes a set of sets each containing elements that touch, and returns
% a set of sets each containing elements that can be assigned the same
% colour.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module graph_colour.

:- interface.

:- import_module set.

:- pred graph_colour__group_elements(set(set(T)), set(set(T))).
:- mode graph_colour__group_elements(in, out) is det.

:- implementation.

:- import_module list, require.

graph_colour__group_elements(Constraints, Colours) :-
	set__power_union(Constraints, AllVars),
	set__init(EmptySet),
	set__remove(Constraints, EmptySet, Constraints1),
	set__to_sorted_list(Constraints1, ConstraintList),
	graph_colour__find_all_colours(ConstraintList, AllVars, ColourList),
	set__list_to_set(ColourList, Colours),
	(
		% some [S,C] (
			set__member(S, Colours),
			set__member(C, S),
			set__member(T, Colours),
			T \= S,
			set__member(C, T)
		% )
	->
		error("graph_colour__group_elements: sanity check failed")
	;
		true
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__find_all_colours(list(set(T)), set(T), list(set(T))).
:- mode graph_colour__find_all_colours(in, in, out) is det.

graph_colour__find_all_colours(ConstraintList, Vars, ColourList) :-
	(
		ConstraintList = []
	->
		ColourList = []
	;
		graph_colour__next_colour(Vars, ConstraintList,
					RemainingConstraints, Colour),
		set__difference(Vars, Colour, RestVars),
		graph_colour__find_all_colours(RemainingConstraints, RestVars,
					ColourList0),
		ColourList = [Colour|ColourList0]
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__next_colour(set(T), list(set(T)), list(set(T)), set(T)).
:- mode graph_colour__next_colour(in, in, out, out) is det.

graph_colour__next_colour(Vars, ConstraintList, Remainder, SameColour) :-
	(
		ConstraintList \= []
	->
		graph_colour__choose_var(Vars, Var, Vars1),
		graph_colour__divide_constraints(Var, ConstraintList,
				WereContaining, NotContaining, Vars1, RestVars),
		(
			NotContaining \= []
		->
			(
				\+ set__empty(RestVars)
			->
				graph_colour__next_colour(RestVars,
					NotContaining, ResidueSets,
							SameColour0),
				set__insert(SameColour0, Var, SameColour)
			;
				set__singleton_set(SameColour, Var),
				ResidueSets = NotContaining
			)
		;
			set__singleton_set(SameColour, Var),
			ResidueSets = []
		),
		list__append(ResidueSets, WereContaining, Remainder)
	;
		Remainder = [],
		set__init(SameColour)
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__divide_constraints(T, list(set(T)), list(set(T)),
				list(set(T)), set(T), set(T)).
:- mode graph_colour__divide_constraints(in, in, out, out, in, out) is det.

graph_colour__divide_constraints(_Var, [], [], [], Vars, Vars).
graph_colour__divide_constraints(Var, [S|Ss], C, NC, Vars0, Vars) :-
	graph_colour__divide_constraints(Var, Ss, C0, NC0, Vars0, Vars1),
	(
		set__member(Var, S)
	->
		set__remove(S, Var, T),
		(
			set__empty(T)
		->
			C = C0
		;
			C = [T|C0]
		),
		NC = NC0,
		set__difference(Vars1, T, Vars)
	;
		C = C0,
		NC = [S|NC0],
		Vars = Vars1
	).

%------------------------------------------------------------------------------%

:- pred graph_colour__choose_var(set(T), T, set(T)).
:- mode graph_colour__choose_var(in, out, out) is det.

graph_colour__choose_var(Vars, Var, Vars1) :-
	set__to_sorted_list(Vars, VarList),
	(
		VarList = [VarA|Vars1A]
	->
		Var = VarA,
		set__list_to_set(Vars1A, Vars1)
	;
		error("graph_colour__choose_var: no vars!")
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
