%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: rl_key.m
% Main author: stayl
%
% Extract key ranges from join and select conditions.
% B-tree operations work by specifying a key range (upper and lower bound)
% on which the operation is to be applied. The B-tree structure allows
% the system to efficiently find the lower bound of the key range then
% traverse the relation in sorted order to the upper bound.
%
%-----------------------------------------------------------------------------%
:- module aditi_backend__rl_key.

:- interface.

:- import_module hlds__hlds_goal, hlds__hlds_module, parse_tree__prog_data.
:- import_module aditi_backend__rl.
:- import_module list, map.

	% Work out the upper and lower bounds for the inputs to an
	% goal which could result in the goal succeeding.
:- pred rl_key__extract_indexing(rl_goal_inputs::in,
		list(hlds_goal)::in, module_info::in, map(prog_var, type)::in,
		list(rl_var_bounds)::out) is det.

	% Given an index specifier, work out whether the bounds of the
	% arguments give useful key ranges for that index. If so, return
	% the list of key ranges.
:- pred rl_key__get_select_key_ranges(module_info::in, map(prog_var, type)::in,
		list(prog_var)::in, index_spec::in, list(rl_var_bounds)::in,
		list(key_range)::out) is semidet.

	% As above except for joins. The first list of arguments is
	% for the non-indexed relation. The values of these arguments
	% are used to construct the key_ranges for the indexed second
	% relation.
:- pred rl_key__get_join_key_ranges(module_info::in, map(prog_var, type)::in,
		list(prog_var)::in, list(prog_var)::in, index_spec::in,
		list(rl_var_bounds)::in, list(key_range)::out) is semidet.

	% Succeed if a join is an equi-join, returning the list
	% of arguments in each tuple which must be equivalent.
:- pred rl_key__is_equijoin(rl_goal_inputs::in, list(rl_var_bounds)::in,
		list(int)::out, list(int)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds__hlds_data, hlds__hlds_pred, parse_tree__prog_util.
:- import_module check_hlds__type_util.
:- import_module assoc_list, bool, int, require, set, std_util.

rl_key__extract_indexing(no_inputs, _, _, _, []). 
rl_key__extract_indexing(one_input(Args), Goals, ModuleInfo, VarTypes,
		ArgBounds) :-
	rl_key__compute_var_bound_maps(Goals, ModuleInfo, VarTypes, VarMaps),
	list__filter_map(
		rl_key__compute_var_bounds(ModuleInfo, Args),
		VarMaps, ArgBounds).
rl_key__extract_indexing(two_inputs(Args1, Args2), Goals, ModuleInfo, VarTypes,
		ArgBounds) :-
	list__append(Args1, Args2, Args),
	rl_key__compute_var_bound_maps(Goals, ModuleInfo, VarTypes, VarMaps),
	list__filter_map(
		rl_key__compute_var_bounds(ModuleInfo, Args),
		VarMaps, ArgBounds).

:- pred rl_key__compute_var_bound_maps(list(hlds_goal)::in, module_info::in,
		map(prog_var, type)::in, list(var_map)::out) is det.

rl_key__compute_var_bound_maps(Goals, ModuleInfo, VarTypes, Cnstrs) :-
	map__init(VarMap),
	map__init(Compares),
	KeyInfo0 = key_info(ModuleInfo, VarTypes, [var_map(VarMap, Compares)]),
	list__foldl(rl_key__extract_key_range, Goals, KeyInfo0, KeyInfo),
	key_info_get_constraints(Cnstrs, KeyInfo, _).

:- pred rl_key__compute_var_bounds(module_info::in, list(prog_var)::in,
		var_map::in, rl_var_bounds::out) is semidet.

rl_key__compute_var_bounds(ModuleInfo, Args, VarMap, ArgBounds) :-
	list__map(rl_key__get_var_bounds(ModuleInfo, VarMap),
		Args, ArgBounds0),
	list__filter_map(rl_key__useful_bounds(ModuleInfo),
		ArgBounds0, ArgBounds1),
	ArgBounds1 \= [],
	map__from_assoc_list(ArgBounds1, ArgBounds).

:- pred rl_key__useful_bounds(module_info::in, 
		pair(prog_var, pair(key_term))::in,
		pair(prog_var, pair(key_term))::out) is semidet.

rl_key__useful_bounds(ModuleInfo, Var - (LowerBound0 - UpperBound0),
		Var - (LowerBound - UpperBound)) :-
	rl_key__remove_useless_info(ModuleInfo, LowerBound0, LowerBound),
	rl_key__remove_useless_info(ModuleInfo, UpperBound0, UpperBound),
	( rl_key__useful_var_bounds(Var, LowerBound)
	; rl_key__useful_var_bounds(Var, UpperBound)
	).

	% Convert functor(ConsId, Type, [unknown, ...]) into unknown if
	% ConsId is the only functor in Type.
:- pred rl_key__remove_useless_info(module_info::in, key_term::in,
		key_term::out) is det.

rl_key__remove_useless_info(_ModuleInfo, var - Vars, var - Vars).
rl_key__remove_useless_info(ModuleInfo,
		functor(ConsId, Type, ArgBounds0) - Vars, 
		Bound) :-
	list__map(rl_key__remove_useless_info(ModuleInfo),
		ArgBounds0, ArgBounds),
	( 
		type_is_tuple(Type, _)
	->
		Bound = var - Vars
	;
		\+ (
			list__member(ArgBound, ArgBounds),
			ArgBound \= var - _
		),
		classify_type(Type, ModuleInfo, TypeClass),
		( TypeClass = user_type
		; TypeClass = enum_type
		),
		module_info_types(ModuleInfo, Types),
		type_to_ctor_and_args(Type, TypeCtor, _),
		map__search(Types, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, Body),
		Body ^ du_type_ctors = []
	->	
		Bound = var - Vars
	;
		Bound = functor(ConsId, Type, ArgBounds) - Vars
	).

:- pred rl_key__useful_var_bounds(prog_var::in, key_term::in) is semidet.

rl_key__useful_var_bounds(_, functor(_, _, _) - _).
rl_key__useful_var_bounds(Var, var - Vars0) :-
	set__delete(Vars0, Var, Vars),
	\+ set__empty(Vars).

%-----------------------------------------------------------------------------%

rl_key__get_select_key_ranges(ModuleInfo, VarTypes, Args, Index,
		VarBoundsLists, KeyRanges) :-
	list__map(rl_key__bounds_to_key_range(no, Args, VarTypes, Index),
		VarBoundsLists, KeyRanges0),
	rl_key__merge_key_ranges(ModuleInfo, KeyRanges0, [], KeyRanges).

rl_key__get_join_key_ranges(ModuleInfo, VarTypes, Args1, Args2, Index,
		VarBoundsLists, KeyRanges) :-
	list__map(rl_key__bounds_to_key_range(yes(Args1), Args2,
		VarTypes, Index), VarBoundsLists, KeyRanges0),
	rl_key__merge_key_ranges(ModuleInfo, KeyRanges0, [], KeyRanges).

:- pred rl_key__bounds_to_key_range(maybe(list(prog_var))::in,
	list(prog_var)::in, map(prog_var, type)::in, index_spec::in,
	map(prog_var, pair(key_term))::in, key_range::out) is semidet.

rl_key__bounds_to_key_range(MaybeConstructArgs, Args, VarTypes,
		index_spec(_, Attrs), VarBoundMap, KeyRange) :-
	( MaybeConstructArgs = yes(ConstructArgs) ->
		map__apply_to_list(ConstructArgs, VarTypes, ConstructArgTypes),
		MaybeConstructArgTypes = yes(ConstructArgTypes)
	;
		MaybeConstructArgTypes = no
	),
		% Partial matches on indexes aren't yet allowed.
	list__map(lambda([Attr::in, (Attr - AttrBound)::out] is semidet, (
			list__index1(Args, Attr, KeyArg), 
			map__search(VarBoundMap, KeyArg, AttrBound)
		)), Attrs, AttrBounds),
				
	list__map(list__index1(Args), Attrs, KeyArgs),
	map__apply_to_list(KeyArgs, VarTypes, ArgTypes),

	rl_key__split_key_tuples(AttrBounds, LowerTuple, UpperTuple),
	rl_key__convert_bound(MaybeConstructArgs, LowerTuple, LowerBound),
	rl_key__convert_bound(MaybeConstructArgs, UpperTuple, UpperBound),
	\+ (LowerBound = infinity, UpperBound = infinity),
	KeyRange = key_range(LowerBound, UpperBound,
		MaybeConstructArgTypes, ArgTypes).

:- pred rl_key__convert_bound(maybe(list(prog_var))::in,
	assoc_list(int, key_term)::in, bounding_tuple::out) is det.

rl_key__convert_bound(MaybeArgs, Tuple, Bound) :-
	assoc_list__keys(Tuple, Indexes),
	assoc_list__values(Tuple, Terms),
	SeenInfinity0 = no,
	list__map_foldl(rl_key__convert_key_attr(MaybeArgs),
		Terms, Attrs, SeenInfinity0, SeenInfinity),

	% If there is an attribute of the key tuple which is
	% not constrained either by a test against the arguments
	% of the other input tuple, or by a test against
	% a constructor, treat the entire bound as infinity.
	% XXX this is temporary, until Aditi supports infinities
	% within bounds tuples.
	(
		SeenInfinity = yes
	->
		Bound = infinity
	;
		% If all attributes are infinity, the entire bound is just
		% infinity.
		Attrs = [KeyAttr | _],
		KeyAttr = infinity
	->
		Bound = infinity
	;
		assoc_list__from_corresponding_lists(Indexes, Attrs, KeyTuple),
		Bound = bound(KeyTuple)
	).

:- pred rl_key__convert_key_attr(maybe(list(prog_var))::in,
		key_term::in, key_attr::out, bool::in, bool::out) is det.

rl_key__convert_key_attr(MaybeArgs, KeyTerm, KeyAttr,
		SeenInfinity0, SeenInfinity) :-
	(
		% As soon as we've seen one infinity in the key tuple,
		% the remaining attributes can't affect the result of
		% the comparison in a B-tree search.
		SeenInfinity0 = yes,
		KeyAttr = infinity,
		SeenInfinity = yes
	;
		SeenInfinity0 = no,
		rl_key__convert_key_attr_2(MaybeArgs, KeyTerm,
			KeyAttr, SeenInfinity)
	).

:- pred rl_key__convert_key_attr_2(maybe(list(prog_var))::in,
		key_term::in, key_attr::out, bool::out) is det.

rl_key__convert_key_attr_2(MaybeArgs, var - Vars, Attr,
		SeenInfinity) :-
	(
		MaybeArgs = yes(Args),
		set__list_to_set(Args, ArgSet),
		set__intersect(ArgSet, Vars, Intersection),
		set__to_sorted_list(Intersection, [Arg | _]),
		list__nth_member_search(Args, Arg, Index)
	->
		Attr = input_field(Index),
		SeenInfinity = no
	;
		Attr = infinity,
		SeenInfinity = yes
	).
rl_key__convert_key_attr_2(MaybeArgs, functor(ConsId, Type, Terms) - _,
		functor(ConsId, Type, Attrs), SeenInfinity) :-
	SeenInfinity0 = no,
	list__map_foldl(rl_key__convert_key_attr(MaybeArgs),
		Terms, Attrs, SeenInfinity0, SeenInfinity).
		
:- pred rl_key__split_key_tuples(assoc_list(int, pair(key_term))::in,
	assoc_list(int, key_term)::out, assoc_list(int, key_term)::out) is det.

rl_key__split_key_tuples([], [], []).
rl_key__split_key_tuples([Index - (Lower - Upper) | Tuples0],
		[Index - Lower | Lowers], [Index - Upper | Uppers]) :-
	rl_key__split_key_tuples(Tuples0, Lowers, Uppers).

%-----------------------------------------------------------------------------%

	% Merge overlapping key ranges. For joins, this really needs
	% to be done at runtime, since the key ranges depend on the tuples
	% of the non-indexed relation.
:- pred rl_key__merge_key_ranges(module_info::in,
		list(key_range)::in, list(key_range)::in,
		list(key_range)::out) is det.

rl_key__merge_key_ranges(_, [], Ranges, Ranges).
rl_key__merge_key_ranges(ModuleInfo, [Range | Ranges0],
		NeededRanges0, NeededRanges) :-
	rl_key__merge_key_ranges_2(ModuleInfo, Range,
		Ranges0, Ranges2, RangeNeeded),
	( RangeNeeded = yes ->
		NeededRanges1 = [Range | NeededRanges0]
	;
		NeededRanges1 = NeededRanges0
	),
	rl_key__merge_key_ranges(ModuleInfo,
		Ranges2, NeededRanges1, NeededRanges).

:- pred rl_key__merge_key_ranges_2(module_info::in, key_range::in,
		list(key_range)::in, list(key_range)::out, bool::out) is det.

rl_key__merge_key_ranges_2(_, _, [], [], yes).
rl_key__merge_key_ranges_2(ModuleInfo, Range1, [Range2 | Ranges0],
		Ranges, IsNeeded) :-
	Range1 = key_range(Lower1, Upper1, Attrs, Schema),
	Range2 = key_range(Lower2, Upper2, _, _),
	(
		(
			bounding_tuple_less_or_equal(ModuleInfo,
				lower, Lower2, upper, Upper1),
			bounding_tuple_less_or_equal(ModuleInfo,
				lower, Lower1, upper, Upper2)
		;
			bounding_tuple_less_or_equal(ModuleInfo,
				lower, Lower1, upper, Upper2),
			bounding_tuple_less_or_equal(ModuleInfo,
				lower, Lower2, upper, Upper1)
		)
	->
		rl_key__min_max(ModuleInfo, lower, Lower1,
			lower, Lower2, Lower, _),
		rl_key__min_max(ModuleInfo, upper, Upper1,
			upper, Upper2, _, Upper),
		Range = key_range(Lower, Upper, Attrs, Schema),
		Ranges = [Range | Ranges0],
		IsNeeded = no
	;
		rl_key__merge_key_ranges_2(ModuleInfo, Range1, Ranges0,
			Ranges1, IsNeeded),
		Ranges = [Range2 | Ranges1]
	).

:- pred rl_key__min_max(module_info::in, upper_lower::in,
		bounding_tuple::in, upper_lower::in, bounding_tuple::in,
		bounding_tuple::out, bounding_tuple::out) is det.

rl_key__min_max(ModuleInfo, UpperLower1, Tuple1,
		UpperLower2, Tuple2, Min, Max) :-
	(
		bounding_tuple_less_or_equal(ModuleInfo,
			UpperLower1, Tuple1, UpperLower2, Tuple2)
	->
		Min = Tuple1,
		Max = Tuple2
	;
		Min = Tuple2,
		Max = Tuple1
	).

:- pred bounding_tuple_less_or_equal(module_info::in, upper_lower::in,
	bounding_tuple::in, upper_lower::in, bounding_tuple::in) is semidet.

bounding_tuple_less_or_equal(_ModuleInfo, lower, infinity, _, _).
bounding_tuple_less_or_equal(_ModuleInfo, _, _, upper, infinity).
bounding_tuple_less_or_equal(ModuleInfo, UpperLower1, bound(Bound1),
		UpperLower2, bound(Bound2)) :-
	key_tuple_less_or_equal(ModuleInfo, UpperLower1, Bound1,
			UpperLower2, Bound2).

:- pred key_tuple_less_or_equal(module_info::in, upper_lower::in,
		assoc_list(int, key_attr)::in, upper_lower::in,
		assoc_list(int, key_attr)::in) is semidet.

key_tuple_less_or_equal(_ModuleInfo, _UpperLower, [], _, []).
key_tuple_less_or_equal(ModuleInfo, UpperLower1, [Index1 - Term1 | Tuple1],
		UpperLower2, [Index2 - Term2 | Tuple2]) :-
	Index1 = Index2,
	key_term_less_or_equal(ModuleInfo, UpperLower1, Term1,
		UpperLower2, Term2),
	key_tuple_less_or_equal(ModuleInfo, UpperLower1, Tuple1,
		UpperLower2, Tuple2).

:- pred key_term_less_or_equal(module_info::in, upper_lower::in,
		key_attr::in, upper_lower::in, key_attr::in) is semidet.

key_term_less_or_equal(_ModuleInfo, _, functor(_, _, _), upper, infinity).
key_term_less_or_equal(_ModuleInfo, lower, infinity, _, functor(_, _, _)).
key_term_less_or_equal(ModuleInfo, UpperLower1, functor(ConsId1, _, Args1), 
		UpperLower2, functor(ConsId2, Type, Args2)) :-
	( ConsId1 = ConsId2 ->
		assoc_list__from_corresponding_lists(Args1, Args2, Args),
		\+ (
			list__member(Arg1 - Arg2, Args),
			\+ key_term_less_or_equal(ModuleInfo,
				UpperLower1, Arg1, UpperLower2, Arg2)
		)
	;
		rl_key__choose_cons_id(ModuleInfo, lower, Type,
			ConsId1, ConsId2, ConsId),
		ConsId = ConsId1
	).	

%-----------------------------------------------------------------------------%

rl_key__is_equijoin(two_inputs(Args1, Args2), [VarBound0 | VarBounds],
		Attrs1, Attrs2) :-
	%
	% For each attribute of the first tuple, work out which
	% attributes of the second must be equal for the join
	% condition to succeed.
	% XXX we don't yet handle cases such as p(X, Y), p(X + 10, Z).
	%
	rl_key__restrict_bounds_to_arg_vars(Args1, Args2,
			VarBound0, EqArgs0),
	list__foldl(rl_key__intersect_branch_eq_args, VarBounds,
		EqArgs0, EqArgs),
	EqArgs \= [],

	%
	% For each attribute of the first tuple, choose one attribute
	% of the second which must be equal.
	%
	list__map(rl_key__var_and_eq_args_to_attr_pair(Args1, Args2),
		EqArgs, AttrPairs),
	assoc_list__keys(AttrPairs, Attrs1),
	assoc_list__values(AttrPairs, Attrs2).

:- pred rl_key__var_and_eq_args_to_attr_pair(list(prog_var)::in,
		list(prog_var)::in, pair(prog_var, set(prog_var))::in,
		pair(int)::out) is det.

rl_key__var_and_eq_args_to_attr_pair(Args1, Args2, Arg1 - EqArgs2,
		AttrPair) :-
	(
		list__nth_member_search(Args1, Arg1, Attr1),
		set__to_sorted_list(EqArgs2, EqArgsList2),
		EqArgsList2 = [Arg2 | _],
		list__nth_member_search(Args2, Arg2, Attr2)
	->
		AttrPair = Attr1 - Attr2
	;	
		error("rl_key__var_and_eq_args_to_attr_pair")
	).	

:- pred rl_key__intersect_branch_eq_args(rl_var_bounds::in,
		assoc_list(prog_var, set(prog_var))::in,
		assoc_list(prog_var, set(prog_var))::out) is semidet.

rl_key__intersect_branch_eq_args(Bounds, EqArgs0, EqArgs) :-
	list__filter_map(rl_key__intersect_eq_args(Bounds), EqArgs0, EqArgs),
	EqArgs \= [].

:- pred rl_key__intersect_eq_args(rl_var_bounds::in,
		pair(prog_var, set(prog_var))::in,
		pair(prog_var, set(prog_var))::out) is semidet.

rl_key__intersect_eq_args(Bounds, Var - EqVars0, Var - EqVars) :-
	map__search(Bounds, Var, VarBounds),
	rl_key__extract_bounds_eq_vars(VarBounds, BoundsEqArgs),
	set__intersect(EqVars0, BoundsEqArgs, EqVars),
	\+ set__empty(EqVars).

:- pred rl_key__restrict_bounds_to_arg_vars(list(prog_var)::in,
		list(prog_var)::in, rl_var_bounds::in,
		assoc_list(prog_var, set(prog_var))::out) is det.

rl_key__restrict_bounds_to_arg_vars(Args, ArgsOfOtherTuple,
		Bounds0, ArgsAndEqOtherArgs) :-
	set__list_to_set(ArgsOfOtherTuple, ArgsOfOtherTupleSet),
	list__filter_map(
		(pred(Arg::in, ArgAndEqOtherArgs::out) is semidet :-
			map__search(Bounds0, Arg, ArgBounds),

			rl_key__extract_bounds_eq_vars(ArgBounds,
				BoundsEqArgs),
			set__intersect(BoundsEqArgs, ArgsOfOtherTupleSet,
				EqOtherArgs),
			\+ set__empty(EqOtherArgs),
			ArgAndEqOtherArgs = Arg - EqOtherArgs
		), Args, ArgsAndEqOtherArgs).

:- pred rl_key__extract_bounds_eq_vars(pair(key_term)::in,
		set(prog_var)::out) is det.

rl_key__extract_bounds_eq_vars(LBound - UBound, BoundsEqArgs) :-
	LBound = _ - LessThanOrEqVars,
	UBound = _ - GreaterThanOrEqVars,
	set__intersect(LessThanOrEqVars, GreaterThanOrEqVars, BoundsEqArgs).

%-----------------------------------------------------------------------------%

:- pred rl_key__get_var_bounds(module_info::in, var_map::in,
		prog_var::in, pair(prog_var, pair(key_term))::out) is det.

rl_key__get_var_bounds(ModuleInfo, VarMap, Var,
		Var - (LowerBound - UpperBound)) :-
	VarMap = var_map(Map, _),
	( map__search(Map, Var, VarInfo) ->
		VarInfo = var_info(LowerBound0, UpperBound0),
		set__init(PropagatedVars),
		rl_key__propagate_alias_bounds(ModuleInfo, lower, VarMap,
			LowerBound0, var - PropagatedVars, LowerBound),
		rl_key__propagate_alias_bounds(ModuleInfo, upper, VarMap,
			UpperBound0, var - PropagatedVars, UpperBound)
	;
		set__singleton_set(Vars, Var),
		LowerBound = var - Vars,
		UpperBound = var - Vars 
	).

:- type upper_lower
	--->	upper
	;	lower
	.

	% Propagate the collected aliasing information into the bounds
	% so they are as accurate as possible.
:- pred rl_key__propagate_alias_bounds(module_info::in, upper_lower::in,
		var_map::in, key_term::in, key_term::in, key_term::out) is det.

rl_key__propagate_alias_bounds(ModuleInfo, UpperLower, VarMap, Term - Vars0,
		Bound0, Bound) :-
	set__to_sorted_list(Vars0, Vars),
	list__foldl(rl_key__propagate_var_bounds(ModuleInfo, UpperLower,
		VarMap),
		Vars, Bound0, Bound1),
	rl_key__propagate_alias_bounds_2(ModuleInfo, UpperLower, VarMap,
		Term, Bound1, Bound).

:- pred rl_key__propagate_alias_bounds_2(module_info::in, upper_lower::in,
	var_map::in, key_term_node::in, key_term::in, key_term::out) is det.

rl_key__propagate_alias_bounds_2(_, _, _, var, Bound, Bound).
rl_key__propagate_alias_bounds_2(ModuleInfo, UpperLower, VarMap,
		functor(ConsId1, Type, ArgTerms), Bound0, Bound) :-
	(
		Bound0 = functor(ConsId2, _, ArgBounds0) - Vars,
		( ConsId2 = ConsId1 ->
			ConsId = ConsId1,
			rl_key__propagate_alias_bounds_list(ModuleInfo,
				UpperLower, VarMap, ArgTerms, ArgBounds0,
				ArgBounds)
		;
			list__length(ArgTerms, Arity),
			set__init(NoVars),
			list__duplicate(Arity, var - NoVars, UnknownArgs),
			rl_key__det_choose_cons_id(ModuleInfo, UpperLower,
				Type, ConsId1, UnknownArgs,
				ConsId2, ArgBounds0, ConsId, ArgBounds1),

			( ConsId = ConsId1 ->
				rl_key__propagate_alias_bounds_list(
					ModuleInfo, UpperLower, VarMap,
					ArgTerms, ArgBounds1, ArgBounds)
			;
				ArgBounds = ArgBounds0
			)
		),
		Bound = functor(ConsId, Type, ArgBounds) - Vars
	;
		Bound0 = var - Vars,
		list__length(ArgTerms, Arity),
		set__init(NoVars),
		list__duplicate(Arity, var - NoVars, ArgBounds0),
		rl_key__propagate_alias_bounds_list(ModuleInfo,
			UpperLower, VarMap, ArgTerms, ArgBounds0, ArgBounds),
		Bound = functor(ConsId1, Type, ArgBounds) - Vars
	).

:- pred rl_key__propagate_alias_bounds_list(module_info::in,
		upper_lower::in, var_map::in, list(key_term)::in,
		list(key_term)::in, list(key_term)::out) is det.

rl_key__propagate_alias_bounds_list(_, _, _, [], [], []).
rl_key__propagate_alias_bounds_list(_, _, _, [_|_], [], _) :-
	error("rl_key__propagate_alias_bounds").
rl_key__propagate_alias_bounds_list(_, _, _, [], [_|_], _) :-
	error("rl_key__propagate_alias_bounds").
rl_key__propagate_alias_bounds_list(ModuleInfo, UpperLower, VarMap,
		[ArgTerm | ArgTerms], [ArgBound0 | ArgBounds0],
		[ArgBound | ArgBounds]) :-
	rl_key__propagate_alias_bounds(ModuleInfo, UpperLower, VarMap,
		ArgTerm, ArgBound0, ArgBound),
	rl_key__propagate_alias_bounds_list(ModuleInfo, UpperLower,
		VarMap, ArgTerms, ArgBounds0, ArgBounds).

:- pred rl_key__propagate_var_bounds(module_info::in, upper_lower::in,
		var_map::in, prog_var::in, key_term::in, key_term::out) is det.

rl_key__propagate_var_bounds(ModuleInfo, UpperLower, VarMap, Var,
		Bound0, Bound) :-
	Bound0 = Term0 - PropagatedVars0,
	( set__member(Var, PropagatedVars0) ->
		Bound = Bound0
	;
		set__insert(PropagatedVars0, Var, PropagatedVars1),
		Bound1 = Term0 - PropagatedVars1,
		VarMap = var_map(Map, _),
		( map__search(Map, Var, VarInfo) ->
			VarInfo = var_info(VarLowerBound, VarUpperBound),
			(
				UpperLower = upper,
				VarBound = VarUpperBound
			;
				UpperLower = lower,
				VarBound = VarLowerBound
			),
			rl_key__propagate_alias_bounds(ModuleInfo,
				UpperLower, VarMap, VarBound, Bound1, Bound)
		;
			Bound = Bound0
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Try to infer upper and lower bounds on variables involved in a goal.
:- pred rl_key__extract_key_range(hlds_goal::in,
		key_info::in, key_info::out) is det.

rl_key__extract_key_range(Goal) -->
	( { Goal = unify(_, _, _, Unify, _) - _ } ->
		rl_key__extract_key_range_unify(Unify)
	; { Goal = call(PredId, ProcId, CallArgs, _, _, _) - _ } ->
		rl_key__extract_key_range_call(PredId, ProcId, CallArgs)
	; { Goal = disj(Goals) - _ } ->
		key_info_get_constraints(Cnstrs0),
		rl_key__extract_key_range_disj(Cnstrs0, Goals, [], Cnstrs),
		key_info_set_constraints(Cnstrs)
	; { Goal = switch(Var, _CanFail, Cases) - _ } ->
		key_info_get_constraints(Cnstrs0),
		rl_key__extract_key_range_switch(Cnstrs0, Var, Cases,
			[], Cnstrs),
		key_info_set_constraints(Cnstrs)
	; { Goal = if_then_else(_, Cond, Then, Else) - _ } ->
		key_info_get_constraints(Cnstrs0),
		rl_key__extract_key_range(Cond),
		rl_key__extract_key_range(Then),
		key_info_get_constraints(Cnstrs1),
		key_info_set_constraints(Cnstrs0),
		rl_key__extract_key_range(Else),
		key_info_get_constraints(Cnstrs2),
		{ list__append(Cnstrs1, Cnstrs2, Cnstrs) },
		key_info_set_constraints(Cnstrs)
	;
		[]
	).

:- pred rl_key__extract_key_range_disj(list(var_map)::in,
		list(hlds_goal)::in, list(var_map)::in, list(var_map)::out,
		key_info::in, key_info::out) is det.

rl_key__extract_key_range_disj(_, [], Maps, Maps) --> [].
rl_key__extract_key_range_disj(Cnstrs0, [Goal | Goals], Maps0, Maps) -->
	key_info_set_constraints(Cnstrs0),
	rl_key__extract_key_range(Goal),
	key_info_get_constraints(Cnstrs),
	{ list__append(Cnstrs, Maps0, Maps1) },
	rl_key__extract_key_range_disj(Cnstrs0, Goals, Maps1, Maps).

:- pred rl_key__extract_key_range_switch(list(var_map)::in, prog_var::in,
		list(case)::in, list(var_map)::in, list(var_map)::out,
		key_info::in, key_info::out) is det.

rl_key__extract_key_range_switch(_, _Var, [], Maps, Maps) --> [].
rl_key__extract_key_range_switch(Cnstrs0, Var, [Case | Cases],
		Maps0, Maps) -->
	{ Case = case(ConsId, Goal) },
	key_info_set_constraints(Cnstrs0),
	rl_key__add_functor_constraint(Var, ConsId),
	rl_key__extract_key_range(Goal),
	key_info_get_constraints(Cnstrs),
	{ list__append(Cnstrs, Maps0, Maps1) },
	rl_key__extract_key_range_switch(Cnstrs0, Var, Cases, Maps1, Maps).

%-----------------------------------------------------------------------------%

	% Interpret calls to comparison and unification procedures.
	% XXX Interpret integer and floating point builtins as well.
:- pred rl_key__extract_key_range_call(pred_id::in, proc_id::in,
		list(prog_var)::in, key_info::in, key_info::out) is det.

rl_key__extract_key_range_call(PredId, ProcId, Args) -->
	key_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },	
	{ pred_info_module(PredInfo, PredModule) },
	{ pred_info_name(PredInfo, PredName) },
	{ list__length(Args, Arity) },
	(
		{ hlds_pred__in_in_unification_proc_id(ProcId) },
		{ is_builtin_unify_pred(PredModule, PredName, Arity) }
	->
		% Find the last two arguments, the rest will be type_infos.
		{ list__reverse(Args, RevArgs) },
		( { RevArgs = [Arg1, Arg2 | _] } ->
			rl_key__unify_var_var(Arg1, Arg2)
		;
			{ error("rl_key__extract_key_range_call: __Unify__") }
		)
	; 
		{ is_builtin_compare_pred(PredModule,
			PredName, Arity, CompareType) }
	->
		rl_key__update_compare_bounds(CompareType, Args)
	;
		[]
	).


:- pred rl_key__update_compare_bounds(compare_type::in, list(prog_var)::in, 
		key_info::in, key_info::out) is det.	

rl_key__update_compare_bounds(result(Result), Args0) -->
	{ list__reverse(Args0, Args) },
	( { Args = [Arg2, Arg1 | _] } ->
		rl_key__update_compare_bounds_2(Result, Arg1, Arg2)
	;
		{ error("rl_key__update_compare_bounds") }	
	).
rl_key__update_compare_bounds(unknown, Args) -->
	( { Args = [Arg2, Arg1, ResultVar | _] } ->
		rl_key__add_compare_result(ResultVar, Arg1, Arg2)
	;
		{ error("rl_key__update_compare_bounds") }	
	).

:- pred rl_key__add_compare_result(prog_var::in, prog_var::in, prog_var::in,
		key_info::in, key_info::out) is det.

rl_key__add_compare_result(CompareResult, Arg1, Arg2) -->
	key_info_get_constraints(Cnstrs0),
	{ UpdateCompares =
		lambda([VarMap0::in, VarMap::out] is det, (
			VarMap0 = var_map(Map, Compares0),
			map__set(Compares0, CompareResult,
				Arg1 - Arg2, Compares),
			VarMap = var_map(Map, Compares)
		)) },
	{ list__map(UpdateCompares, Cnstrs0, Cnstrs) },
	key_info_set_constraints(Cnstrs).

:- pred rl_key__update_compare_bounds_2(comparison_result::in,
	prog_var::in, prog_var::in, key_info::in, key_info::out) is det.	

rl_key__update_compare_bounds_2((<), Arg1, Arg2) -->
	rl_key__add_var_upper_bound(Arg1, Arg2),
	rl_key__add_var_lower_bound(Arg2, Arg1).
rl_key__update_compare_bounds_2((>), Arg1, Arg2) -->
	rl_key__add_var_lower_bound(Arg1, Arg2),
	rl_key__add_var_upper_bound(Arg2, Arg1).
rl_key__update_compare_bounds_2((=), Arg1, Arg2) -->
	rl_key__unify_var_var(Arg1, Arg2).
	

	% Note that all ranges in a B-tree are closed, so '<' is treated
	% as '=<' when computing the key range. The join condition must be
	% sure to do the '\=' comparison at run-time.
:- type compare_type 
	--->	result(comparison_result) % ordering if the predicate succeeds.
	;	unknown		% result of compare/3, to be tested later.
	.

:- pred is_builtin_compare_pred(sym_name::in, string::in,
		int::in, compare_type::out) is semidet.

is_builtin_compare_pred(_, "__Compare__", _, unknown).
is_builtin_compare_pred(Module, "compare", 4, unknown) :-
	mercury_public_builtin_module(Module).
is_builtin_compare_pred(unqualified("int"), "<", 2, result(<)).
is_builtin_compare_pred(unqualified("int"), "=<", 2, result(<)).
is_builtin_compare_pred(unqualified("int"), ">", 2, result(>)).
is_builtin_compare_pred(unqualified("int"), ">=", 2, result(>)).
is_builtin_compare_pred(Module, "builtin_compare_int", 3, unknown) :-
	mercury_private_builtin_module(Module).
is_builtin_compare_pred(unqualified("float"), "<", 2, result(<)).
is_builtin_compare_pred(unqualified("float"), "=<", 2, result(<)).
is_builtin_compare_pred(unqualified("float"), ">", 2, result(>)).
is_builtin_compare_pred(unqualified("float"), ">=", 2, result(>)).
is_builtin_compare_pred(Module, "builtin_compare_float", 3, unknown) :-
	mercury_private_builtin_module(Module).
is_builtin_compare_pred(Module, "builtin_compare_string", 3, unknown) :-
	mercury_private_builtin_module(Module).

:- pred is_builtin_unify_pred(sym_name::in, string::in, int::in) is semidet.

is_builtin_unify_pred(_, "__Unify__", _).
is_builtin_unify_pred(Module, "unify", 3) :-
	mercury_public_builtin_module(Module).
is_builtin_unify_pred(Module, "builtin_unify_int", 2) :-
	mercury_private_builtin_module(Module).
is_builtin_unify_pred(Module, "builtin_unify_character", 2) :-
	mercury_private_builtin_module(Module).
is_builtin_unify_pred(Module, "builtin_unify_string", 2) :-
	mercury_private_builtin_module(Module).
is_builtin_unify_pred(Module, "builtin_unify_float", 2) :-
	mercury_private_builtin_module(Module).

%-----------------------------------------------------------------------------%

:- pred rl_key__extract_key_range_unify(unification::in,
		key_info::in, key_info::out) is det.

rl_key__extract_key_range_unify(simple_test(Var1, Var2)) -->
	rl_key__unify_var_var(Var1, Var2).
rl_key__extract_key_range_unify(assign(Var1, Var2)) -->
	rl_key__unify_var_var(Var1, Var2).
rl_key__extract_key_range_unify(construct(Var, ConsId, Args, _, _, _, _)) -->
	rl_key__unify_functor(Var, ConsId, Args).
rl_key__extract_key_range_unify(
		deconstruct(Var, ConsId, Args, _, _, _)) -->
	rl_key__unify_functor(Var, ConsId, Args).
rl_key__extract_key_range_unify(complicated_unify(_, _, _)) -->
	{ error("rl_key__extract_key_range_unify") }.

:- pred rl_key__unify_functor(prog_var::in, cons_id::in, list(prog_var)::in,
		key_info::in, key_info::out) is det.

rl_key__unify_functor(Var, ConsId, Args) -->
	key_info_get_constraints(Constraints0),	
	{ GetArgTerm = lambda([Arg::in, ArgTerm::out] is det, (
			set__singleton_set(ArgSet, Arg),
			ArgTerm = var - ArgSet
		)) },
	{ list__map(GetArgTerm, Args, ArgTerms) },

	{ set__singleton_set(VarSet, Var) },
	key_info_get_vartypes(VarTypes),
	{ map__lookup(VarTypes, Var, Type) },
	{ Term1 = functor(ConsId, Type, ArgTerms) - VarSet },
	key_info_get_module_info(ModuleInfo),
	{ AddConstraint =
	    lambda([VarMap0::in, VarMap::out] is semidet, (
		VarMap0 = var_map(Map0, CompRes),
		( map__search(Map0, Var, VarInfo0) ->
			VarInfo0 = var_info(LBound0, UBound0),
			rl_key__unify_term(ModuleInfo,
				lower, Term1, LBound0, LBound),
			rl_key__unify_term(ModuleInfo,
				upper, Term1, UBound0, UBound),
			VarInfo = var_info(LBound, UBound)
		;
			VarInfo = var_info(Term1, Term1)
		),
		map__set(Map0, Var, VarInfo, Map),
		VarMap = var_map(Map, CompRes)
	)) },
	{ list__filter_map(AddConstraint, Constraints0, Constraints) },
	key_info_set_constraints(Constraints).

:- pred rl_key__unify_var_var(prog_var::in, prog_var::in,
		key_info::in, key_info::out) is det.

rl_key__unify_var_var(Var1, Var2) -->
	key_info_get_constraints(Constraints0),	
	{ AddEquality =
	    lambda([Map0::in, Map::out] is det, (
		rl_key__add_alias(Var1, Var2, Map0, Map1),
		rl_key__add_alias(Var2, Var1, Map1, Map)
	    )) }, 
	{ list__map(AddEquality, Constraints0, Constraints) },
	key_info_set_constraints(Constraints).

:- pred rl_key__add_alias(prog_var::in, prog_var::in,
		var_map::in, var_map::out) is det.

rl_key__add_alias(Var1, Var2, Map0, Map) :-
	Map0 = var_map(VarMap0, Compares0),
	( map__search(VarMap0, Var1, VarInfo0) ->
		VarInfo0 = var_info(LBound0, UBound0),
		rl_key__add_var_to_node(LBound0, Var2, LBound),
		rl_key__add_var_to_node(UBound0, Var2, UBound),
		VarInfo  = var_info(LBound, UBound)
	;
		set__singleton_set(Vars, Var2),
		VarInfo = var_info(var - Vars, var - Vars)
	),
	map__set(VarMap0, Var1, VarInfo, VarMap),
	Map = var_map(VarMap, Compares0).

:- pred rl_key__add_var_to_node(key_term::in,
		prog_var::in, key_term::out) is det.

rl_key__add_var_to_node(Node - Vars0, Var, Node - Vars) :-
	set__insert(Vars0, Var, Vars).

:- pred rl_key__add_functor_constraint(prog_var::in, cons_id::in,
		key_info::in, key_info::out) is det.

rl_key__add_functor_constraint(Var, ConsId) -->
	(
		{ ConsId = int_const(_), Arity = 0
		; ConsId = float_const(_), Arity = 0
		; ConsId = string_const(_), Arity = 0
		; ConsId = cons(_, Arity) 
		}
	->
		key_info_get_vartypes(VarTypes),
		{ map__lookup(VarTypes, Var, VarType) },
		{ set__init(InitVars) },
		{ list__duplicate(Arity, var - InitVars, Args) },
		{ Term = functor(ConsId, VarType, Args) - InitVars },
		rl_key__add_equality_constraint(Var, Term)
	;
		[]
	).

:- pred rl_key__add_equality_constraint(prog_var::in, key_term::in,
		key_info::in, key_info::out) is det.

rl_key__add_equality_constraint(Var, Term) -->
	rl_key__update_bounds(Var, Term, Term).

:- pred rl_key__add_var_lower_bound(prog_var::in, prog_var::in,
		key_info::in, key_info::out) is det.

rl_key__add_var_lower_bound(Var, BoundVar) -->
	{ set__singleton_set(Vars, BoundVar) },
	rl_key__add_lower_bound(Var, var - Vars).

:- pred rl_key__add_var_upper_bound(prog_var::in, prog_var::in,
		key_info::in, key_info::out) is det.

rl_key__add_var_upper_bound(Var, BoundVar) -->
	{ set__singleton_set(Vars, BoundVar) },
	rl_key__add_upper_bound(Var, var - Vars).

:- pred rl_key__add_lower_bound(prog_var::in, key_term::in,
		key_info::in, key_info::out) is det.

rl_key__add_lower_bound(Var, Term) -->
	{ set__init(Vars) },
	rl_key__update_bounds(Var, Term, var - Vars).

:- pred rl_key__add_upper_bound(prog_var::in, key_term::in,
		key_info::in, key_info::out) is det.

rl_key__add_upper_bound(Var, Term) -->
	{ set__init(Vars) },
	rl_key__update_bounds(Var, var - Vars, Term).

:- pred rl_key__update_bounds(prog_var::in, key_term::in, key_term::in,
		key_info::in, key_info::out) is det.

rl_key__update_bounds(Var, LowerBound1, UpperBound1) -->
	key_info_get_constraints(Cnstrs0),
	key_info_get_module_info(ModuleInfo),
	{ UpdateBounds =
	    lambda([VarMap0::in, VarMap::out] is det, (
		VarMap0 = var_map(Map0, CompRes),
		( map__search(Map0, Var, VarInfo0) ->
			VarInfo0 = var_info(LowerBound0, UpperBound0),
			rl_key__unify_term(ModuleInfo, lower,
				LowerBound0, LowerBound1, LowerBound),
			rl_key__unify_term(ModuleInfo, upper,
				UpperBound0, UpperBound1, UpperBound),
			VarInfo = var_info(LowerBound, UpperBound)
		;
			VarInfo = var_info(LowerBound1, UpperBound1)
		),
		map__set(Map0, Var, VarInfo, Map),
		VarMap = var_map(Map, CompRes)
	    )) },
	{ list__map(UpdateBounds, Cnstrs0, Cnstrs) },
	key_info_set_constraints(Cnstrs).

	% If the bounds can't be combined, we should throw away 
	% this set of constraints, since they are unsatisfiable.
:- pred rl_key__unify_term(module_info::in, upper_lower::in,
		key_term::in, key_term::in, key_term::out) is det.

rl_key__unify_term(ModuleInfo, UpperLower, Term1 - Vars1,
		Term2 - Vars2, Term - Vars) :-
	set__union(Vars1, Vars2, Vars),
	rl_key__unify_term_2(ModuleInfo, UpperLower,
		Term1, Term2, Term).

:- pred rl_key__unify_term_2(module_info::in, upper_lower::in,
	key_term_node::in, key_term_node::in, key_term_node::out) is det.

rl_key__unify_term_2(_, _, var, var, var).
rl_key__unify_term_2(_, _, functor(ConsId, Type, Args),
		var, functor(ConsId, Type, Args)).
rl_key__unify_term_2(_, _, var, functor(ConsId, Type, Args),
		functor(ConsId, Type, Args)).
rl_key__unify_term_2(ModuleInfo, UpperLower,
		functor(ConsId1, Type, Args1),
		functor(ConsId2, _, Args2),
		functor(ConsId, Type, Args)) :-
	( ConsId1 = ConsId2 ->	
		ConsId = ConsId1,
		assoc_list__from_corresponding_lists(Args1, Args2, Args3),
		UpdatePair = 
			lambda([Pair::in, Bound::out] is det, (
				Pair = Bound1 - Bound2,
				rl_key__unify_term(ModuleInfo,
					UpperLower, Bound1, Bound2, Bound)
			)),
		list__map(UpdatePair, Args3, Args)
	;
		rl_key__det_choose_cons_id(ModuleInfo, UpperLower, 
			Type, ConsId1, Args1, ConsId2, Args2, ConsId, Args)
	).

%-----------------------------------------------------------------------------%

	% Given two non-identical cons_ids of the same type, choose
	% the one which gives the largest range on the possible
	% value of the term.
:- pred rl_key__det_choose_cons_id(module_info::in, upper_lower::in,
	(type)::in, cons_id::in, list(T)::in, cons_id::in, list(T)::in,
	cons_id::out, list(T)::out) is det.

rl_key__det_choose_cons_id(ModuleInfo, UpperLower, Type, ConsId1, Args1,
		ConsId2, Args2, ConsId, Args) :-
	(
		rl_key__choose_cons_id(ModuleInfo, UpperLower, 
			Type, ConsId1, ConsId2, ChosenConsId)
	->
		ConsId = ChosenConsId,
		( ConsId = ConsId1 ->
			Args = Args1
		;
			Args = Args2
		)
	;
		error("rl_key__det_choose_cons_id: invalid cons_id pair")	
	).

:- pred rl_key__choose_cons_id(module_info::in, upper_lower::in,
		(type)::in, cons_id::in, cons_id::in, cons_id::out) is semidet.
		
rl_key__choose_cons_id(ModuleInfo, UpperLower, Type,
		ConsId1, ConsId2, ConsId) :-
	(
		ConsId1 = cons(_, _),
		ConsId2 = cons(_, _)
	->	
		module_info_types(ModuleInfo, Types),
		type_to_ctor_and_args(Type, TypeCtor, _),
		map__search(Types, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, Body),
		% If there's a user defined equality pred we're in trouble.
		Body ^ du_type_usereq = no,
		rl_key__choose_cons_id_2(Body ^ du_type_ctors,
			UpperLower, ConsId1, ConsId2, ConsId)
	;
		% int_consts etc. can be directly compared.
		compare(CompareRes, ConsId1, ConsId2),
		rl_key__choose_cons_id_3(CompareRes, UpperLower, 
			ConsId1, ConsId2, ConsId)
	).

	% Find the cons_id which compares lowest, then choose 
	% the one which gives the largest range.
:- pred rl_key__choose_cons_id_2(list(constructor)::in, upper_lower::in,
		cons_id::in, cons_id::in, cons_id::out) is semidet.	

rl_key__choose_cons_id_2([], _, _, _, _) :-
	error("rl_key__choose_cons_id_2: couldn't find ctor").
rl_key__choose_cons_id_2([Ctor | Ctors], UpperLower,
		ConsId1, ConsId2, ConsId) :-
	Ctor = ctor(_, _, SymName, Args),
	list__length(Args, Arity),
	ThisConsId = cons(SymName, Arity),
	( ThisConsId = ConsId1 ->
		rl_key__choose_cons_id_3((<), UpperLower,
			ConsId1, ConsId2, ConsId)
	; ThisConsId = ConsId2 ->
		rl_key__choose_cons_id_3((<), UpperLower,
			ConsId2, ConsId1, ConsId)
	;
		rl_key__choose_cons_id_2(Ctors, UpperLower,
			ConsId1, ConsId2, ConsId)
	).

:- pred rl_key__choose_cons_id_3(comparison_result::in, upper_lower::in,
		cons_id::in, cons_id::in, cons_id::out) is semidet.

rl_key__choose_cons_id_3((<), upper, _, ConsId, ConsId).
rl_key__choose_cons_id_3((<), lower, ConsId, _, ConsId).
rl_key__choose_cons_id_3((>), upper, ConsId, _, ConsId).
rl_key__choose_cons_id_3((>), lower, _, ConsId, ConsId).

%-----------------------------------------------------------------------------%

:- type key_info
	---> key_info(
		module_info,
		map(prog_var, type),
		list(var_map)
	).

:- type var_map
	---> var_map(
		map(prog_var, var_info),
		map(prog_var, pair(prog_var)) 	% map from comparison result
						% to the pair of compared vars
	).

:- type var_info
	---> var_info(
		key_term,		% lower bound term
		key_term		% upper bound term
	).

:- pred key_info_get_module_info(module_info, key_info, key_info).
:- mode key_info_get_module_info(out, in, out) is det.

key_info_get_module_info(ModuleInfo, Info, Info) :-
	Info = key_info(ModuleInfo, _, _).

:- pred key_info_get_vartypes(map(prog_var, type), key_info, key_info).
:- mode key_info_get_vartypes(out, in, out) is det.

key_info_get_vartypes(VarTypes, Info, Info) :-
	Info = key_info(_, VarTypes, _).

:- pred key_info_get_constraints(list(var_map), key_info, key_info).
:- mode key_info_get_constraints(out, in, out) is det.

key_info_get_constraints(Cnstrs, Info, Info) :-
	Info = key_info(_, _, Cnstrs).

:- pred key_info_set_constraints(list(var_map), key_info, key_info).
:- mode key_info_set_constraints(in, in, out) is det.

key_info_set_constraints(Cnstrs, Info0, Info) :-
	Info0 = key_info(A, B, _),
	Info = key_info(A, B, Cnstrs).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
