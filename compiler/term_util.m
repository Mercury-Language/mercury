%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
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

:- module term_util.

:- interface.

:- import_module term_errors, prog_data.
:- import_module hlds_module, hlds_pred, hlds_data, hlds_goal.

:- import_module std_util, bimap, bool, int, list, lp_rational, map, bag, 
							set, size_varset, term.

%-----------------------------------------------------------------------------%

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

	;	infinite(list(term_errors__error))
				% There is no finite integer for which the
				% above equation is true. The argument says
				% why the analysis failed to find a finite
				% constant.

	;	constraints(equations, set(var), bimap(var, size_var)).

:- type termination_info
	---> 	cannot_loop	% This procedure terminates for all
				% possible inputs.

	;	can_loop(list(term_errors__error)).
				% The analysis could not prove that the
				% procedure terminates.

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

:- type unify_info	==	pair(map(var, type), functor_info).

:- type weight_info	--->	weight(int, list(bool)).
:- type weight_table	==	map(pair(type_id, cons_id), weight_info).

:- pred find_weights(module_info::in, weight_table::out) is det.

% This predicate is computes the weight of a functor and the set of arguments
% of that functor whose sizes should be counted towards the size of the whole
% term.

:- pred functor_norm(functor_info::in, type_id::in, cons_id::in,
	module_info::in, int::out, list(var)::in, list(var)::out,
	list(uni_mode)::in, list(uni_mode)::out) is det.

:- type pass_info
	--->	pass_info(
			functor_info,
			int,		% Max number of errors to gather.
			int		% Max number of paths to analyze.
		).
%-----------------------------------------------------------------------------%

:- type constraint_info 
	---> 	constr_info(  

		bimap(var, size_var),		
				% The bimap
				% stores the corespondence between
				% vars (i.e. variables
				% appearing in a particular procedure)
				% and SizeVars from the single varset 
				% used to get variables for passing to 
				% lp.

		size_varset,	% A varset is used to create a set of
				% non-clashing variables to pass to
				% project and convex_hull.  

		set(var),	% A set of all the variables discovered
				% to have zero-size-type.

		equations	% All the constraints so far found.	
		).			
 
%-----------------------------------------------------------------------------%

% These predicates are for dealing with equations.

:- pred rename_vars(list(var), list(size_var), module_info, map(var, type), 
					constraint_info, constraint_info).
:- mode rename_vars(in, out, in, in, in, out) is det.

:- pred rename_var(var, size_var, constraint_info, constraint_info).
:- mode rename_var(in, out, in, out) is det.  

% Inserts a new variable into the set of zero-size-variables (does
% nothing if that variable is already there).
:- pred new_zero_var(var, constraint_info, constraint_info).
:- mode new_zero_var(in, in, out) is det.

% Compares two equations, assumed to be in canonical form.  Two equations
% are equal if they have the same coefficients, operation and constant.
% Eqn1 < Eqn2 if they are parallel but the constant in Eqn1 is greater than
% that in Eqn2 (i.e. Eqn1 is weaker than Eqn2, since canonical form includes
% only (=<) as an op).
% Non-parallel equations are sorted according to lexicographic order of
% their variable names (there isn't a good reason for choosing this particular
% order, except that it's easy to compute.  Anything that sorts on variables 
% is acceptable).
:- pred compare_eqns(equation, equation, comparison_result).
:- mode compare_eqns(in, in, out) is det.

%##Write what this does.
:- pred compare_coeff_lists(list(coeff), list(coeff), comparison_result).
:- mode compare_coeff_lists(in, in, out) is det.

% Put equations into a canonical form:  Every equation contains =<, 
% has its coefficients listed in increasing order of variable name,
% and has first coefficient equal to plus or minus 1.
:- pred canonical_form(equations, equations).
:- mode canonical_form(in, out) is det.

%-----------------------------------------------------------------------------%


% This predicate partitions the arguments of a call into a list of input
% variables and a list of output variables,

:- pred partition_call_args(module_info::in, list(mode)::in, list(var)::in,
	bag(var)::out, bag(var)::out) is det.

% Given a list of variables from a unification, this predicate divides the
% list into a bag of input variables, and a bag of output variables.

:- pred split_unification_vars(list(var)::in, list(uni_mode)::in,
	module_info::in, bag(var)::out, bag(var)::out) is det.

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

:- pred remove_unused_args(bag(var), list(var), list(bool), bag(var)).
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

:- pred horder_vars(list(var), map(var, type)).
:- mode horder_vars(in, in) is semidet.

% Succeeds if all values of the given type are zero size (for all norms).

:- pred zero_size_type(type, module_info).
:- mode zero_size_type(in, in) is semidet.

:- pred get_context_from_scc(list(pred_proc_id)::in, module_info::in,
	term__context::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module inst_match, prog_out, mode_util, type_util.
:- import_module globals, options.

:- import_module assoc_list, require.
:- import_module rat, lp_rational.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% Equation-handling predicates:

rename_vars(Vars, Renamed_Vars0, Module, VarTypes, Constr_info0, 
								Constr_info) :-
	rename_vars_acc(Vars, [], Renamed_Vars0, Module, VarTypes, Constr_info0,
								Constr_info).

:- pred rename_vars_acc(list(var), list(size_var), list(size_var),
		module_info, map(var, type), constraint_info, constraint_info).
:- mode rename_vars_acc(in, in, out, in, in, in, out) is det.

rename_vars_acc([], Renamed_Vars0, Renamed_Vars, _Module, _VarTypes, 
						Constr_info, Constr_info):-
	list__reverse(Renamed_Vars0, Renamed_Vars).


rename_vars_acc([V|Vars], Renamed_Vars0, Renamed_Vars, Module, VarTypes, 
						Constr_info0, Constr_info) :-
	
	map__lookup(VarTypes, V, Type),
	( zero_size_type(Type, Module) ->
		new_zero_var(V, Constr_info0, Constr_info1),
		Renamed_Vars1 = Renamed_Vars0

	;
		rename_var(V, RV, Constr_info0, Constr_info1),
		Renamed_Vars1 = [RV|Renamed_Vars0]
	),
	rename_vars_acc(Vars, Renamed_Vars1, Renamed_Vars, Module, VarTypes, 
						Constr_info1, Constr_info).


% This should only be called when the var is not one of zero-size-type.
%##Should check here?
rename_var(Var0, Var1, Constr_info0, Constr_info) :-
% see code in lp_rat.  
	Constr_info0 = constr_info(BMap0, Vars0, ZeroVars0, Eqns0),

	(bimap__search(BMap0, Var0, OutVar) ->
		Var1 = OutVar,
		Vars = Vars0,
		BMap = BMap0,
		Eqns = Eqns0
	;
		size_varset__new_var(Vars0, NewVar, Vars),
		(bimap__insert(BMap0, Var0, NewVar, BMap1) ->
			Var1 = NewVar,
			BMap = BMap1,
			Eqns = [eqn([NewVar-one], (>=), zero) | Eqns0]
		;
			error("term_traversal2: bimap__insert failed\n")
		)
	),
	Constr_info = constr_info(BMap, Vars, ZeroVars0, Eqns).


new_zero_var(Var, constr_info(Vars, Map, Zeros0, Eqns), 
			constr_info(Vars, Map, Zeros, Eqns) ) :-
	set__insert(Zeros0, Var, Zeros).
	



compare_eqns(Eqn1, Eqn2, Result) :-
        (
                Eqn1 = eqn(Coeffs1, (=<), Const1),
                Eqn2 = eqn(Coeffs2, (=<), Const2)
        ->
                compare_coeff_lists(Coeffs1, Coeffs2, Coeffs_result),
		( Coeffs_result = (=) ->
			( rat:'<'(Const1, Const2) ->
				Result = (>)
			;
				( rat:'='(Const1, Const2) ->
					Result = (=)
				;
					Result = (<)
				)
			)
		;
			Result = Coeffs_result
		)
	;
		error("term_constr_pass1: Non-canonical equation passed to 
								compare_eqns\n")
	).

compare_coeff_lists([], [], (=)).
compare_coeff_lists([], [_|_], (<)).
compare_coeff_lists([_|_], [], (>)).
compare_coeff_lists([C1|Coeffs1], [C2|Coeffs2], Result) :-
	compare_coeffs(C1, C2, Coeff_Result),
	( Coeff_Result = (=) ->
		compare_coeff_lists(Coeffs1, Coeffs2, Result)
	;
		Result = Coeff_Result
	).

canonical_form(Eqns0, Canonical_eqns) :-
        fm_standardize_equations(Eqns0, Standardized_eqns),
        eqns_to_vectors(Standardized_eqns, Vectors),

        Normalize_on_first_var = lambda([Vec1::in, Norm_vec::out] is det,(
                Vec1 = pair(Map0, _),
		( map__remove_smallest(Map0, LeastVar, _, _) ->
			normalize_vector(Vec1, LeastVar, Norm_vec)
		;
			error("term_constr_pass1:Can't normalise null vector\n")

		)
	)),
	list__map(Normalize_on_first_var, Vectors, Canonical_vecs),

	vectors_to_eqns(Canonical_vecs, Almost_canonical_eqns),
											Sort_coeffs = lambda([eqn(Coeffs, Op, Num)::in, Sort_eqn::out] is det, (
		list__sort(compare_coeffs, Coeffs, Sorted_coeffs),
		Sort_eqn = eqn(Sorted_coeffs, Op, Num)
	)),
	list__map(Sort_coeffs, Almost_canonical_eqns, Canonical_eqns).

:- pred compare_coeffs(coeff, coeff, comparison_result).
:- mode compare_coeffs(in, in, uo) is det.
compare_coeffs(Var1-_, Var2-_, Result) :-
	compare(Result, Var1, Var2).

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

:- pred find_weights_for_type_list(assoc_list(type_id, hlds_type_defn)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_type_list([], Weights, Weights).
find_weights_for_type_list([TypeId - TypeDefn | TypeList], Weights0, Weights) :-
	find_weights_for_type(TypeId, TypeDefn, Weights0, Weights1),
	find_weights_for_type_list(TypeList, Weights1, Weights).

:- pred find_weights_for_type(type_id::in, hlds_type_defn::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_type(TypeId, TypeDefn, Weights0, Weights) :-
	hlds_data__get_type_defn_body(TypeDefn, TypeBody),
	(
		TypeBody = du_type(Constructors, _, _, _),
		hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
		find_weights_for_cons_list(Constructors, TypeId, TypeParams,
			Weights0, Weights)
	;
		TypeBody = uu_type(_),
		error("undiscriminated union types not yet implemented")
	;
		% This type does not introduce any functors
		TypeBody = eqv_type(_),
		Weights = Weights0
	;
		% This type may introduce some functors,
		% but we will never see them in this analysis
		TypeBody = abstract_type,
		Weights = Weights0
	).

:- pred find_weights_for_cons_list(list(constructor)::in,
	type_id::in, list(type_param)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_cons_list([], _, _, Weights, Weights).
find_weights_for_cons_list([Constructor | Constructors], TypeId, Params,
		Weights0, Weights) :-
	find_weights_for_cons(Constructor, TypeId, Params, Weights0, Weights1),
	find_weights_for_cons_list(Constructors, TypeId, Params,
		Weights1, Weights).

:- pred find_weights_for_cons(constructor::in,
	type_id::in, list(type_param)::in,
	weight_table::in, weight_table::out) is det.

find_weights_for_cons(Ctor, TypeId, Params, Weights0, Weights) :-
	% XXX should we do something about ExistQVars here?
 	Ctor = ctor(_ExistQVars, _Constraints, SymName, Args),
	list__length(Args, Arity),
	( Arity > 0 ->
		find_and_count_nonrec_args(Args, TypeId, Params,
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
	map__det_insert(Weights0, TypeId - ConsId, WeightInfo, Weights).

:- pred find_and_count_nonrec_args(list(constructor_arg)::in,
	type_id::in, list(type_param)::in,
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
	type_id::in, list(type_param)::in) is semidet.

is_arg_recursive(Arg, Id, Params) :-
	Arg = _Name - ArgType,
	type_to_type_id(ArgType, ArgTypeId, ArgTypeParams),
	Id = ArgTypeId,
	list__perm(Params, ArgTypeParams).

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
functor_norm(use_map(WeightMap), TypeId, ConsId, _Module, Int,
		Args, Args, Modes, Modes) :-
	( map__search(WeightMap, TypeId - ConsId, WeightInfo) ->
		WeightInfo = weight(Int, _)
	;
		Int = 0
	).
functor_norm(use_map_and_args(WeightMap), TypeId, ConsId, _Module, Int,
		Args0, Args, Modes0, Modes) :-
	( map__search(WeightMap, TypeId - ConsId, WeightInfo) ->
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

:- pred functor_norm_filter_args(list(bool), list(var), list(var),
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

:- pred partition_call_args_2(module_info::in, list(mode)::in, list(var)::in,
	list(var)::out, list(var)::out) is det.

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
		type_is_higher_order(Type, _, _)
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
