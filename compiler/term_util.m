%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% term_util.m
% Main author: crs.
%
% This module:
%
% -	defines the types used by termination analysis
% -	defines predicates for computing functor norms
% -	defines some utility predicates
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__term_util.

:- interface.

:- import_module transform_hlds__term_errors, parse_tree__prog_data.
:- import_module hlds__hlds_module, hlds__hlds_pred, hlds__hlds_data.
:- import_module hlds__hlds_goal.

:- import_module std_util, bool, int, list, map, bag.

%-----------------------------------------------------------------------------%
%
% The types `arg_size_info' and `termination_info' hold information
% about procedures which is used for termination analysis.
% These types are stored as fields in the HLDS proc_info.
% For cross-module analysis, the information is written out as
% `pragma termination_info(...)' declarations in the
% `.opt' and `.trans_opt' files.  The module prog_data.m defines
% types similar to these two (but without the `list(term_errors__error)')
% which are used when parsing `termination_info' pragmas.
%

% The arg size info defines an upper bound on the difference
% between the sizes of the output arguments of a procedure and the sizes
% of the input arguments:
%
% | input arguments | + constant >= | output arguments |
%
% where | | represents a semilinear norm.

:- type arg_size_info
	--->	finite(int, list(bool))
				% The termination constant is a finite integer.
				% The list of bool has a 1:1 correspondence
				% with the input arguments of the procedure.
				% It stores whether the argument contributes
				% to the size of the output arguments.

	;	infinite(list(term_errors__error)).
				% There is no finite integer for which the
				% above equation is true. The argument says
				% why the analysis failed to find a finite
				% constant.

:- type termination_info
	---> 	cannot_loop	% This procedure terminates for all
				% possible inputs.

	;	can_loop(list(term_errors__error)).
				% The analysis could not prove that the
				% procedure terminates.

% The type `used_args' holds a mapping which specifies for each procedure
% which of its arguments are used.

:- type used_args	==	map(pred_proc_id, list(bool)).

%-----------------------------------------------------------------------------%

% We use semilinear norms (denoted by ||) to compute the sizes of terms.
% These have the form
%
% | f(t1, ... tn) | = weight(f) + sum of | ti |
% where i is an element of a set I, and I is a subset of {1, ... n}
%
% We currently support four kinds of semilinear norms.

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

:- type unify_info	==	pair(map(prog_var, type), functor_info).

:- type weight_info	--->	weight(int, list(bool)).
:- type weight_table	==	map(pair(type_ctor, cons_id), weight_info).

:- pred find_weights(module_info::in, weight_table::out) is det.

% This predicate is computes the weight of a functor and the set of arguments
% of that functor whose sizes should be counted towards the size of the whole
% term.

:- pred functor_norm(functor_info::in, type_ctor::in, cons_id::in,
	module_info::in, int::out, list(prog_var)::in, list(prog_var)::out,
	list(uni_mode)::in, list(uni_mode)::out) is det.

:- type pass_info
	--->	pass_info(
			functor_info,
			int,		% Max number of errors to gather.
			int		% Max number of paths to analyze.
		).

%-----------------------------------------------------------------------------%

% This predicate partitions the arguments of a call into a list of input
% variables and a list of output variables,

:- pred partition_call_args(module_info::in, list(mode)::in, list(prog_var)::in,
	bag(prog_var)::out, bag(prog_var)::out) is det.

% Given a list of variables from a unification, this predicate divides the
% list into a bag of input variables, and a bag of output variables.

:- pred split_unification_vars(list(prog_var)::in, list(uni_mode)::in,
	module_info::in, bag(prog_var)::out, bag(prog_var)::out) is det.

%  Used to create lists of boolean values, which are used for used_args.
%  make_bool_list(HeadVars, BoolIn, BoolOut) creates a bool list which is
%  (length(HeadVars) - length(BoolIn)) `no' followed by BoolIn.  This is
%  used to set the used args for compiler generated predicates.  The no's
%  at the start are because the Type infos are not used. length(BoolIn)
%  should equal the arity of the predicate, and the difference in length
%  between the arity of the procedure and the arity of the predicate is
%  the number of type infos.

:- pred term_util__make_bool_list(list(_T)::in, list(bool)::in,
	list(bool)::out) is det.

% Removes variables from the InVarBag that are not used in the call.
% remove_unused_args(InVarBag0, VarList, BoolList, InVarBag)
% VarList and BoolList are corresponding lists.  Any variable in VarList
% that has a `no' in the corresponding place in the BoolList is removed
% from InVarBag.

:- pred remove_unused_args(bag(prog_var), list(prog_var), list(bool),
		bag(prog_var)).
:- mode remove_unused_args(in, in, in, out) is det.

% This predicate sets the argument size info of a given a list of procedures.

:- pred set_pred_proc_ids_arg_size_info(list(pred_proc_id)::in,
	arg_size_info::in, module_info::in, module_info::out) is det.

% This predicate sets the termination info of a given a list of procedures.

:- pred set_pred_proc_ids_termination_info(list(pred_proc_id)::in,
	termination_info::in, module_info::in, module_info::out) is det.

:- pred lookup_proc_termination_info(module_info::in, pred_proc_id::in,
	maybe(termination_info)::out) is det.

:- pred lookup_proc_arg_size_info(module_info::in, pred_proc_id::in,
	maybe(arg_size_info)::out) is det.

% Succeeds if one or more variables in the list are higher order.

:- pred horder_vars(list(prog_var), map(prog_var, type)).
:- mode horder_vars(in, in) is semidet.

% Succeeds if all values of the given type are zero size (for all norms).

:- pred zero_size_type(type, module_info).
:- mode zero_size_type(in, in) is semidet.

:- pred get_context_from_scc(list(pred_proc_id)::in, module_info::in,
	prog_context::out) is det.

%-----------------------------------------------------------------------------%

% Convert a prog_data__pragma_termination_info into a
% term_util__termination_info, by adding the appropriate context.

:- pred add_context_to_termination_info(maybe(pragma_termination_info),
		prog_context, maybe(termination_info)).
:- mode add_context_to_termination_info(in, in, out) is det.

% Convert a prog_data__pragma_arg_size_info into a
% term_util__arg_size_info, by adding the appropriate context.

:- pred add_context_to_arg_size_info(maybe(pragma_arg_size_info),
		prog_context, maybe(arg_size_info)).
:- mode add_context_to_arg_size_info(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match, parse_tree__prog_out.
:- import_module check_hlds__mode_util, check_hlds__type_util.
:- import_module libs__globals, libs__options.

:- import_module assoc_list, require.

%-----------------------------------------------------------------------------%
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

find_weights(ModuleInfo, Weights) :-
	module_info_types(ModuleInfo, TypeTable),
	map__to_assoc_list(TypeTable, TypeList),
	map__init(Weights0),
	find_weights_for_type_list(TypeList, Weights0, Weights).

:- pred find_weights_for_type_list(assoc_list(type_ctor, hlds_type_defn)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_type_list([], Weights, Weights).
find_weights_for_type_list([TypeCtor - TypeDefn | TypeList],
		Weights0, Weights) :-
	find_weights_for_type(TypeCtor, TypeDefn, Weights0, Weights1),
	find_weights_for_type_list(TypeList, Weights1, Weights).

:- pred find_weights_for_type(type_ctor::in, hlds_type_defn::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_type(TypeCtor, TypeDefn, Weights0, Weights) :-
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	(
		TypeBody = du_type(Constructors, _, _, _),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
		find_weights_for_cons_list(Constructors, TypeCtor, TypeParams,
			Weights0, Weights)
	;
		% This type does not introduce any functors
		TypeBody = eqv_type(_),
		Weights = Weights0
	;
		% This type may introduce some functors,
		% but we will never see them in this analysis
		TypeBody = abstract_type,
		Weights = Weights0
	;
		% This type does not introduce any functors
		TypeBody = foreign_type(_, _, _),
		Weights = Weights0
	).

:- pred find_weights_for_cons_list(list(constructor)::in,
	type_ctor::in, list(type_param)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_cons_list([], _, _, Weights, Weights).
find_weights_for_cons_list([Constructor | Constructors], TypeCtor, Params,
		Weights0, Weights) :-
	find_weights_for_cons(Constructor, TypeCtor, Params,
		Weights0, Weights1),
	find_weights_for_cons_list(Constructors, TypeCtor, Params,
		Weights1, Weights).

:- pred find_weights_for_cons(constructor::in,
	type_ctor::in, list(type_param)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_cons(Ctor, TypeCtor, Params, Weights0, Weights) :-
	% XXX should we do something about ExistQVars here?
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
	map__det_insert(Weights0, TypeCtor - ConsId, WeightInfo, Weights).

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
		NonRecArgs is NonRecArgs0 + 1,
		ArgInfo = [no | ArgInfo0]
	).

:- pred is_arg_recursive(constructor_arg::in,
	type_ctor::in, list(type_param)::in) is semidet.

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

% Although the module info is not used in either of these norms, it could
% be needed for other norms, so it should not be removed.

functor_norm(simple, _, ConsId, _, Int, Args, Args, Modes, Modes) :-
	(
		ConsId = cons(_, Arity),
		Arity \= 0
	->
		Int = 1
	;
		Int = 0
	).
functor_norm(total, _, ConsId, _Module, Int, Args, Args, Modes, Modes) :-
	( ConsId = cons(_, Arity) ->
		Int = Arity
	;
		Int = 0
	).
functor_norm(use_map(WeightMap), TypeCtor, ConsId, _Module, Int,
		Args, Args, Modes, Modes) :-
	( search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) ->
		WeightInfo = weight(Int, _)
	;
		Int = 0
	).
functor_norm(use_map_and_args(WeightMap), TypeCtor, ConsId, _Module, Int,
		Args0, Args, Modes0, Modes) :-
	( search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) ->
		WeightInfo = weight(Int, UseArgList),
		(
			functor_norm_filter_args(UseArgList, Args0, Args1,
				Modes0, Modes1)
		->
			Modes = Modes1,
			Args = Args1
		;
			error("Unmatched lists in functor_norm_filter_args.")
		)
	;
		Int = 0,
		Modes = Modes0,
		Args = Args0
	).

% This predicate will fail if the length of the input lists are not matched.

:- pred functor_norm_filter_args(list(bool), list(prog_var), list(prog_var),
	list(uni_mode), list(uni_mode)).
:- mode functor_norm_filter_args(in, in, out, in, out) is semidet.

functor_norm_filter_args([], [], [], [], []).
functor_norm_filter_args([yes | Bools], [Arg0 | Args0], [Arg0 | Args],
		[Mode0 | Modes0], [Mode0 | Modes]) :-
	functor_norm_filter_args(Bools, Args0, Args, Modes0, Modes).
functor_norm_filter_args([no | Bools], [_Arg0 | Args0], Args,
		[_Mode0 | Modes0], Modes) :-
	functor_norm_filter_args(Bools, Args0, Args, Modes0, Modes).

%-----------------------------------------------------------------------------%

partition_call_args(Module, ArgModes, Args, InVarsBag, OutVarsBag) :-
	partition_call_args_2(Module, ArgModes, Args, InVars, OutVars),
	bag__from_list(InVars, InVarsBag),
	bag__from_list(OutVars, OutVarsBag).

:- pred partition_call_args_2(module_info::in, list(mode)::in,
	list(prog_var)::in, list(prog_var)::out, list(prog_var)::out) is det.

partition_call_args_2(_, [], [], [], []).
partition_call_args_2(_, [], [_ | _], _, _) :-
	error("Unmatched variables in term_util:partition_call_args").
partition_call_args_2(_, [_ | _], [], _, _) :-
	error("Unmatched variables in term_util__partition_call_args").
partition_call_args_2(ModuleInfo, [ArgMode | ArgModes], [Arg | Args],
		InputArgs, OutputArgs) :-
	partition_call_args_2(ModuleInfo, ArgModes, Args,
		InputArgs1, OutputArgs1),
	( mode_is_input(ModuleInfo, ArgMode) ->
		InputArgs = [Arg | InputArgs1],
		OutputArgs = OutputArgs1
	; mode_is_output(ModuleInfo, ArgMode) ->
		InputArgs = InputArgs1,
		OutputArgs = [Arg | OutputArgs1]
	;
		InputArgs = InputArgs1,
		OutputArgs = OutputArgs1
	).

% For these next two predicates (split_unification_vars and
% partition_call_args) there is a problem of what needs to be done for
% partially instantiated data structures.  The correct answer is that the
% system shoud use a norm such that the size of the uninstantiated parts of
% a partially instantiated structure have no effect on the size of the data
% structure according to the norm.  For example when finding the size of a
% list-skeleton, list-length norm should be used.  Therefore, the size of
% any term must be given by
% sizeof(term) = constant + sum of the size of each
% 			(possibly partly) instantiated subterm.
% It is probably easiest to implement this by modifying term_weights.
% The current implementation does not correctly handle partially
% instantiated data structures.

split_unification_vars([], Modes, _ModuleInfo, Vars, Vars) :-
	bag__init(Vars),
	( Modes = [] ->
		true
	;
		error("term_util:split_unification_vars: Unmatched Variables")
	).
split_unification_vars([Arg | Args], Modes, ModuleInfo,
		InVars, OutVars):-
	( Modes = [UniMode | UniModes] ->
		split_unification_vars(Args, UniModes, ModuleInfo,
			InVars0, OutVars0),
		UniMode = ((_VarInit - ArgInit) -> (_VarFinal - ArgFinal)),
		( % if
			inst_is_bound(ModuleInfo, ArgInit)
		->
			% Variable is an input variable
			bag__insert(InVars0, Arg, InVars),
			OutVars = OutVars0
		; % else if
			inst_is_free(ModuleInfo, ArgInit),
			inst_is_bound(ModuleInfo, ArgFinal)
		->
			% Variable is an output variable
			InVars = InVars0,
			bag__insert(OutVars0, Arg, OutVars)
		; % else
			InVars = InVars0,
			OutVars = OutVars0
		)
	;
		error("term_util__split_unification_vars: Unmatched Variables")
	).

%-----------------------------------------------------------------------------%

term_util__make_bool_list(HeadVars0, Bools, Out) :-
	list__length(Bools, Arity),
	( list__drop(Arity, HeadVars0, HeadVars1) ->
		HeadVars = HeadVars1
	;
		error("Unmatched variables in term_util:make_bool_list")
	),
	term_util__make_bool_list_2(HeadVars, Bools, Out).

:- pred term_util__make_bool_list_2(list(_T), list(bool), list(bool)).
:- mode term_util__make_bool_list_2(in, in, out) is det.

term_util__make_bool_list_2([], Bools, Bools).
term_util__make_bool_list_2([ _ | Vars ], Bools, [no | Out]) :-
	term_util__make_bool_list_2(Vars, Bools, Out).

remove_unused_args(Vars, [], [], Vars).
remove_unused_args(Vars, [], [_X | _Xs], Vars) :-
	error("Unmatched variables in term_util:remove_unused_args").
remove_unused_args(Vars, [_X | _Xs], [], Vars) :-
	error("Unmatched variables in term_util__remove_unused_args").
remove_unused_args(Vars0, [ Arg | Args ], [ UsedVar | UsedVars ], Vars) :-
	( UsedVar = yes ->
		% The variable is used, so leave it
		remove_unused_args(Vars0, Args, UsedVars, Vars)
	;
		% The variable is not used in producing output vars, so
		% dont include it as an input variable.
		bag__delete(Vars0, Arg, Vars1),
		remove_unused_args(Vars1, Args, UsedVars, Vars)
	).

%-----------------------------------------------------------------------------%

set_pred_proc_ids_arg_size_info([], _ArgSize, Module, Module).
set_pred_proc_ids_arg_size_info([PPId | PPIds], ArgSize, Module0, Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(Module0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_set_maybe_arg_size_info(ProcInfo0, yes(ArgSize), ProcInfo),

	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(Module0, PredTable, Module1),
	set_pred_proc_ids_arg_size_info(PPIds, ArgSize, Module1, Module).

set_pred_proc_ids_termination_info([], _Termination, Module, Module).
set_pred_proc_ids_termination_info([PPId | PPIds], Termination,
		Module0, Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(Module0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_set_maybe_termination_info(ProcInfo0, yes(Termination),
		ProcInfo),

	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(Module0, PredTable, Module1),
	set_pred_proc_ids_termination_info(PPIds, Termination,
		Module1, Module).

lookup_proc_termination_info(Module, PredProcId, MaybeTermination) :-
	PredProcId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, MaybeTermination).

lookup_proc_arg_size_info(Module, PredProcId, MaybeArgSize) :-
	PredProcId = proc(PredId, ProcId),
	module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
	proc_info_get_maybe_arg_size_info(ProcInfo, MaybeArgSize).

horder_vars([Arg | Args], VarType) :-
	(
		map__lookup(VarType, Arg, Type),
		type_is_higher_order(Type, _, _, _)
	;
		horder_vars(Args, VarType)
	).

zero_size_type(Type, Module) :-
	classify_type(Type, Module, TypeCategory),
	zero_size_type_category(TypeCategory, Type, Module, yes).

:- pred zero_size_type_category(builtin_type, type, module_info, bool).
:- mode zero_size_type_category(in, in, in, out) is det.

zero_size_type_category(int_type, _, _, yes).
zero_size_type_category(char_type, _, _, yes).
zero_size_type_category(str_type, _, _, yes).
zero_size_type_category(float_type, _, _, yes).
zero_size_type_category(pred_type, _, _, no).
zero_size_type_category(tuple_type, _, _, no).
zero_size_type_category(enum_type, _, _, yes).
zero_size_type_category(polymorphic_type, _, _, no).
zero_size_type_category(user_type, _, _, no).

%-----------------------------------------------------------------------------%

get_context_from_scc(SCC, Module, Context) :-
	( SCC = [proc(PredId, _) | _] ->
		module_info_pred_info(Module, PredId, PredInfo),
		pred_info_context(PredInfo, Context)
	;
		error("Empty SCC in pass 2 of termination analysis")
	).

%-----------------------------------------------------------------------------%

add_context_to_termination_info(no, _, no).
add_context_to_termination_info(yes(cannot_loop), _, yes(cannot_loop)).
add_context_to_termination_info(yes(can_loop), Context,
		yes(can_loop([Context - imported_pred]))).

add_context_to_arg_size_info(no, _, no).
add_context_to_arg_size_info(yes(finite(A, B)), _, yes(finite(A, B))).
add_context_to_arg_size_info(yes(infinite), Context,
				yes(infinite([Context - imported_pred]))).

%-----------------------------------------------------------------------------%
