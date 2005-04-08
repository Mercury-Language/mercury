%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_norm.m
% Main author: crs.
%
% This modules defines predicates for computing functor norms.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__term_norm.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module libs__globals.
:- import_module parse_tree__prog_data.

:- import_module int.
:- import_module list.

%-----------------------------------------------------------------------------%

	% The functor_info type contains information about how the weight
	% of a term is calculated.
	%
:- type functor_info.

	% This predicate sets the functor_info depending on the value of the
	% termination_norm or termination2_norm option.
	%
:- func set_functor_info(globals.termination_norm, module_info) = functor_info.  

	% This predicate computes the weight of a functor and the set of
	% arguments of that functor whose sizes should be counted towards the
	% size of the whole term.
	%
	% NOTE: the list of arguments and the list of modes must be the
	% same length.  They must also *not* contain any type-info
	% related arguments as this may cause an exception to be thrown
	% when using the `--num-data-elems' norm.  (This is because the
	% weight table doesn't keep track of type-info related variables
	% - it used to but intervening compiler passes tend to do things
	% to the code in the mean time so the whole lot becomes
	% inconsistent - in the end it's just easier to ignore them). 
	%
:- pred functor_norm(functor_info::in, type_ctor::in, cons_id::in,
	module_info::in, int::out, list(prog_var)::in, list(prog_var)::out,
	list(uni_mode)::in, list(uni_mode)::out) is det.

	% This function computes a lower bound on the weight of a fuctor.
	% If the lower bound is zero then the weight of that functor is
	% also zero.  If the lower bound is non-zero then there may be
	% no upper bound on the size of the functor.  (And if there was
	% this function wouldn't tell you about it anyhow).
	%
:- func functor_lower_bound(functor_info, type_ctor, cons_id, module_info) 
	= int.
	
	% Succeeds if all values of the given type are zero size
	% (for all norms).
	%
:- pred zero_size_type((type)::in, module_info::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

% We use semilinear norms (denoted by ||) to compute the sizes of terms.
% These have the form
%
% | f(t1, ... tn) | = weight(f) + sum of | ti |
% where i is an element of a set I, and I is a subset of {1, ... n}
%
% We currently support four kinds of semilinear norms.
% XXX Actually we currently only use three of them.  `use_map/1' is unused.

:- type functor_info
	--->	simple	% All non-constant functors have weight 1,
			% while constants have weight 0.
			% Use the size of all subterms (I = {1, ..., n}.

	;	total	% All functors have weight = arity of the functor.
			% Use the size of all subterms (I = {1, ..., n}.

	;	use_map(weight_table)
			% The weight of each functor is given by the table.
			% Use the size of all subterms (I = {1, ..., n}.

	;	use_map_and_args(weight_table).
			% The weight of each functor is given by the table,
			% and so is the set of arguments of the functor whose
			% size should be counted (I is given by the table
			% entry of the functor).

%-----------------------------------------------------------------------------%

% Calculate the weight to be assigned to each function symbol for the
% use_map and use_map_and_args semilinear norms.
%
% Given a type definition such as
%
% :- type t(Tk)	--->	f1(a11, ... a1n1)	where n1 is the arity of f1
%		;	...
%		;	fm(am1, ... amnm)	where nm is the arity of fm
%
% we check, for each aij, whether its type is recursive (i.e. it is t with
% type variable arguments that are a permutation of Tk). The weight info
% we compute for each functor will have a boolean list that has a `yes'
% for each recursive argument and a `no' for each nonrecursive argument.
% The weight to be assigned to the functor is the number of nonrecursive
% arguments, except that we assign a weight of at least 1 to all functors
% which are not constants.

:- type weight_table == map(pair(type_ctor, cons_id), weight_info).

:- type weight_info ---> weight(int, list(bool)).

:- pred find_weights(module_info::in, weight_table::out) is det.

find_weights(ModuleInfo, Weights) :-
	module_info_types(ModuleInfo, TypeTable),
	map__to_assoc_list(TypeTable, TypeList),
	map__init(Weights0),
	list__foldl(find_weights_for_type, TypeList, Weights0, Weights).

:- pred find_weights_for_type(pair(type_ctor, hlds_type_defn)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_type(TypeCtor - TypeDefn, !Weights) :-
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	(
		Constructors = TypeBody ^ du_type_ctors,
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
		list__foldl(find_weights_for_cons(TypeCtor, TypeParams),
			Constructors, !Weights)
	;
		% This type does not introduce any functors
		TypeBody = eqv_type(_)
	;
		% This type may introduce some functors,
		% but we will never see them in this analysis
		TypeBody = abstract_type(_)
	;
		% This type does not introduce any functors
		TypeBody = foreign_type(_)
	;
		% This type does not introduce any functors
		TypeBody = solver_type(_, _)
	).

:- pred find_weights_for_cons(type_ctor::in, list(type_param)::in,
	constructor::in, weight_table::in, weight_table::out) is det.

% For existentially typed data items the compiler may insert some
% type-info related arguments into the functor.  We ignore these
% arguments when calculating the weight of a functor and we do not
% include them in the list of counted arguments.

find_weights_for_cons(TypeCtor, Params, Ctor, !Weights) :-
	Ctor = ctor(_ExistQVars, _Constraints, SymName, Args),
	list__length(Args, Arity),
	( Arity > 0 ->
		find_and_count_nonrec_args(Args, TypeCtor, Params,
			NumNonRec, ArgInfos0),
		( NumNonRec = 0 ->
			Weight = 1,
			list__duplicate(Arity, yes, ArgInfos)
		;
			Weight = NumNonRec,
			ArgInfos = ArgInfos0
		),
		WeightInfo = weight(Weight, ArgInfos)
	;
		WeightInfo = weight(0, [])
	),
	ConsId = cons(SymName, Arity),
	map__det_insert(!.Weights, TypeCtor - ConsId, WeightInfo, !:Weights).

:- pred find_weights_for_tuple(arity::in, weight_info::out) is det.

find_weights_for_tuple(Arity, weight(Weight, ArgInfos)) :-
	% None of the tuple arguments are recursive.
	Weight = Arity,
	list__duplicate(Arity, yes, ArgInfos).

:- pred find_and_count_nonrec_args(list(constructor_arg)::in,
	type_ctor::in, list(type_param)::in,
	int::out, list(bool)::out) is det.

find_and_count_nonrec_args([], _, _, 0, []).
find_and_count_nonrec_args([Arg | Args], Id, Params, NonRecArgs, ArgInfo) :-
	find_and_count_nonrec_args(Args, Id, Params, NonRecArgs0, ArgInfo0),
	( is_arg_recursive(Arg, Id, Params) ->
		NonRecArgs = NonRecArgs0,
		ArgInfo = [yes | ArgInfo0]
	;
		NonRecArgs = NonRecArgs0 + 1,
		ArgInfo = [no | ArgInfo0]
	).

:- pred is_arg_recursive(constructor_arg::in, type_ctor::in,
	list(type_param)::in) is semidet.

is_arg_recursive(Arg, TypeCtor, Params) :-
	Arg = _Name - ArgType,
	type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeParams),
	TypeCtor = ArgTypeCtor,
	list__perm(Params, ArgTypeParams).

:- pred search_weight_table(weight_table::in, type_ctor::in, cons_id::in,
	weight_info::out) is semidet.

search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) :-
	( map__search(WeightMap, TypeCtor - ConsId, WeightInfo0) ->
		WeightInfo = WeightInfo0
	; type_ctor_is_tuple(TypeCtor) ->
		TypeCtor = _ - Arity,
		find_weights_for_tuple(Arity, WeightInfo)
	;
		fail
	).

%-----------------------------------------------------------------------------%

set_functor_info(total, _Module) = total.
set_functor_info(simple, _Module) = simple.
set_functor_info(num_data_elems, Module) = use_map_and_args(WeightMap) :-
	find_weights(Module, WeightMap).
set_functor_info(size_data_elems, Module) = use_map(WeightMap) :-
	find_weights(Module, WeightMap).

%-----------------------------------------------------------------------------%

% Although the module info is not used in any of these norms, it could
% be needed for other norms, so it should not be removed.

functor_norm(simple, _, ConsId, _, Int, !Args, !Modes) :-
	(
		ConsId = cons(_, Arity),
		Arity \= 0
	->
		Int = 1
	;
		Int = 0
	).
functor_norm(total, _, ConsId, _, Int, !Args, !Modes) :-
	( ConsId = cons(_, Arity) ->
		Int = Arity
	;
		Int = 0
	).
functor_norm(use_map(WeightMap), TypeCtor, ConsId, _, Int, !Args, !Modes) :-
	( search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) ->
		WeightInfo = weight(Int, _)
	;
		Int = 0
	).
functor_norm(use_map_and_args(WeightMap), TypeCtor, ConsId, _, Int, !Args,
		!Modes) :-
	( search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) ->
		WeightInfo = weight(Int, UseArgList),
		(
			functor_norm_filter_args(UseArgList, !Args, !Modes)
		->
			true
		;
			unexpected(this_file, "Unmatched lists in " ++
				"functor_norm_filter_args.")
		)
	;
		Int = 0
	).

	% This predicate will fail if the length of the input lists are
	% not matched.
	%
:- pred functor_norm_filter_args(list(bool)::in, list(prog_var)::in,
	list(prog_var)::out, list(uni_mode)::in, list(uni_mode)::out)
	is semidet.

functor_norm_filter_args([], [], [], [], []).
functor_norm_filter_args([yes | Bools], [Arg0 | Args0], [Arg0 | Args],
		[Mode0 | Modes0], [Mode0 | Modes]) :-
	functor_norm_filter_args(Bools, Args0, Args, Modes0, Modes).
functor_norm_filter_args([no | Bools], [_Arg0 | Args0], Args,
		[_Mode0 | Modes0], Modes) :-
	functor_norm_filter_args(Bools, Args0, Args, Modes0, Modes).

%-----------------------------------------------------------------------------%

functor_lower_bound(simple, _, ConsId, _) =
	( if ConsId = cons(_, Arity), Arity \= 0 then 1 else 0).
functor_lower_bound(total, _, ConsId, _) =
	( if ConsId = cons(_, Arity) then Arity else 0 ).
functor_lower_bound(use_map(WeightMap), TypeCtor, ConsId, _) = Weight :-
	( if	search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo)
	  then	WeightInfo = weight(Weight, _)
	  else	Weight = 0
	).
functor_lower_bound(use_map_and_args(WeightMap), TypeCtor, ConsId, _) 
		= Weight :-
	( if	search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo)
	  then	WeightInfo = weight(Weight, _)
	  else	Weight = 0
	).

%-----------------------------------------------------------------------------%

zero_size_type(Type, Module) :-
	type_util.classify_type(Module, Type) = TypeCategory,
	zero_size_type_category(TypeCategory, yes).

:- pred zero_size_type_category(type_category::in, bool::out) is det.

zero_size_type_category(int_type, yes).
zero_size_type_category(char_type, yes).
zero_size_type_category(str_type, yes).
zero_size_type_category(float_type, yes).
zero_size_type_category(void_type, yes).
zero_size_type_category(type_info_type, yes).
zero_size_type_category(type_ctor_info_type, yes).
zero_size_type_category(typeclass_info_type, yes).
zero_size_type_category(base_typeclass_info_type, yes).
zero_size_type_category(higher_order_type, yes).
zero_size_type_category(tuple_type, no).
zero_size_type_category(enum_type, yes).
zero_size_type_category(variable_type, no).
zero_size_type_category(user_ctor_type, no).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_norm.m".

%-----------------------------------------------------------------------------%
:- end_module term_norm.
%-----------------------------------------------------------------------------%
