%-----------------------------------------------------------------------------
%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------
%
% term_util.m
% Main author: crs.
% 
% This file contains utilities that are used by termination analysis.
%
%-----------------------------------------------------------------------------

:- module term_util.

:- interface.

:- import_module list, bool, bag, int, hlds_module, hlds_pred, hlds_data.
:- import_module term_errors, io, hlds_goal, term, prog_data, map.

	% term(TermConstant, Terminates, UsedArgs, MaybeError)
	%   TermConstant	See description below
	%   Terminates		See description below
	%   UsedArgs		This list of bool, when set, has a 1:1 
	%   			  correspondance with the arguments of the
	%   			  procedure. It stores whether the argument
	%   			  is used in producing the output
	%   			  arguments.  
	%   MaybeError		If the analysis fails to prove termination
	%   			  of a procedure, then this value indicates
	%   			  the reason that the analysis failed.
:- type termination 
	--->	 term(term_util__constant, terminates, maybe(list(bool)),
		maybe(term_errors__error)).

% term_util__constant defines the level by which a procedures arguments
% grow or shrink.  It is used to determine the termination properties of a 
% predicate.  The termination constant defines the relative sizes of input
% and output arguments in the following equation:
% | input arguments | + constant >= | output arguments |
% where | | represents a semilinear norm.
:- type term_util__constant
	--->	inf(term_errors__error)
				% The smallest constant that the analysis
				% could prove was valid is infinity.
				% Whenever a constant is set to infinity,
				% an error is associated with it which
				% states why the analysis failed to find a
				% smaller constant.
	;	not_set		% The termination constant has not been 
				% set yet.  After
				% term_pass1:proc_inequalities has been
				% run, the constant should be set for all
				% the procedures in the module.  Failure to
				% set the constant would indicate a
				% programming error.
	;	set(int).	% The termination constant has been 
				% set to int.

:- type terminates 	
	---> 	dont_know	% The analysis could not prove that the 
				% procedure terminates.
	;	not_set		% Each procedure has its `terminates' set 
				% set to not_set initially.
				% The termination analysis uses the fact
				% that if a procedure is called whose
				% terminates is not_set, then the call is
				% mutually recursive. (If the call is not
				% mutually recursive, then the analysis
				% will have already processed the
				% procedure, and would have set the
				% constant). term_pass2:termination should
				% set the terminates value of all
				% procedures.  Failure to do so indicates a
				% software error.
	;	yes.		% This procedure terminates for all 
				% possible inputs.

% The functor algorithm defines how a weight is assigned to each functor.
:- type functor_algorithm
	--->	simple		% simple assigns all non-constant 
				% functors a norm of 1.
	;	total		% All functors have a norm = arity of functor
			% Use the weight table to find the
			% weight of any constructor.
	;	use_map(weight_table)
			% Use the weight table, and use its bool list to
			% determine which arguments of any functor are to
			% be considered when calculating the norm of a
			% functor.
	;	use_map_and_args(weight_table).

:- type term_util__result(T) 
	--->	ok
	;	error(T).

:- type unify_info == pair(map(var, type), functor_info).
:- type functor_info == functor_algorithm.

% These types are used to assign functor weights to functor symbols
% The weight_info is assigned to a cons_id. weight(Integer, BoolList) gives
% the weight to assign to this functor symbol, and the BoolList states
% whether or not each argument of this functor should be used to calculate
% the size of the output.  That is, the BoolList has a 1:1 mapping with the
% arguments of the functor.
:- type weight_info	--->	weight(int, list(bool)).
:- type weight_table	==	map(pair(type_id, cons_id), weight_info).

% This predicate provides an initialised termination structure.
:- pred term_util__init(termination).
:- mode term_util__init(out) is det.

% This predicate calculates a weight table according to the following
% rules:
% If a type is directly recursive, its weight is given by the number of
% directly recursive arguments of this cons id.  For example, a list has
% size 1, a binary tree has size 2.  Each element of the bool list is
% assigned yes if the corresponding argument is directly recursive, and no
% otherwise.
% If a type is not directly recursive, then it is assigned a weight of 1,
% and the bool list is set to be all yes.
%
% If the bool list is used (by setting functor_algorithm to
% use_map_and_args), then the effect of this norm is that the size
% of a data structure is given by the number of data elements in the data
% structure.  If the bool list is ignored, then the size of all the
% arguments of any functor considered, then the size of a data structure is
% given by the total size of its data elements.
:- pred find_weights(module_info::in, weight_table::out) is det.

% This predicate is used to assign a norm (integer) to a functor, depending
% on its type. Depending on the functor_algorithm, this predicate may also
% modify the list of args and modes.
:- pred functor_norm(functor_info, type_id, cons_id, module_info,  int, 
	list(var), list(var), list(uni_mode), list(uni_mode)).
:- mode functor_norm(in, in, in, in, out, in, out, in, out) is det.

% This predicate should be called whenever a procedure needs its termination
% set to dont_know.  This predicate checks to see whether the termination
% is already set to dont_know, and if so it does nothing.  If the
% termination is set to yes, or not_set, it changes the termination to
% dont_know, and checks whether a 
% check_termination pragma has been defined for this predicate, and if so,
% this outputs a useful error message.
:- pred do_ppid_check_terminates(list(pred_proc_id), term_errors__error, 
	module_info, module_info, io__state, io__state).
:- mode do_ppid_check_terminates(in, in, in, out, di, uo) is det.

%  Used to create lists of boolean values, which are used for used_args.
%  make_bool_list(HeadVars, BoolIn, BoolOut) creates a bool list which is 
%  (length(HeadVars) - length(BoolIn)) `no' followed by BoolIn.  This is
%  used to set the used args for compiler generated predicates.  The no's
%  at the start are because the Type infos are not used. length(BoolIn)
%  should equal the arity of the predicate, and the difference in length
%  between the arity of the procedure and the arity of the predicate is
%  the number of type infos. 
:- pred term_util__make_bool_list(list(_T), list(bool), list(bool)).
:- mode term_util__make_bool_list(in, in, out) is det.

% This predicate partitions the arguments of a call into a list of input
% variables and a list of output variables,
:- pred partition_call_args(inst_table, module_info, list(mode), list(var),
	list(var), list(var)).
:- mode partition_call_args(in, in, in, in, out, out) is det.

% Removes variables from the InVarBag that are not used in the call.
% remove_unused_args(InVarBag0, VarList, BoolList, InVarBag)
% VarList and BoolList are corresponding lists.  Any variable in VarList
% that has a `no' in the corresponding place in the BoolList is removed
% from InVarBag.
:- pred remove_unused_args(bag(var), list(var), list(bool), bag(var)).
:- mode remove_unused_args(in, in, in, out) is det.

% Given a list of pred_proc_ids, this predicate sets the termination
% constant of them all to the constant that is passed to it.
:- pred set_pred_proc_ids_const(list(pred_proc_id), term_util__constant,
	module_info, module_info).
:- mode set_pred_proc_ids_const(in, in, in, out) is det.

% Given a list of pred_proc_ids, this predicate sets the error and
% terminates value of them all to the values that are passed to the
% predicate.
:- pred set_pred_proc_ids_terminates(list(pred_proc_id), terminates,
	maybe(term_errors__error), module_info, module_info).
:- mode set_pred_proc_ids_terminates(in, in, in, in, out) is det.

% Fails if one or more variables in the list are higher order
:- pred check_horder_args(list(var), map(var, type)).  
:- mode check_horder_args(in, in) is semidet.	

% Given a list of variables from a unification, this predicate divides the
% list into a bag of input variables, and a bag of output variables.
:- pred split_unification_vars(list(var), list(uni_mode), inst_table,
	module_info, bag(var), bag(var)).
:- mode split_unification_vars(in, in, in, in, out, out) is det.

:- implementation.

:- import_module map, std_util, require, mode_util, prog_out, type_util.
:- import_module globals, options, inst_match, assoc_list.

term_util__init(term(not_set, not_set, no, no)).

check_horder_args([], _).
check_horder_args([Arg | Args], VarType) :-
	map__lookup(VarType, Arg, Type),
	\+ type_is_higher_order(Type, _, _),
	check_horder_args(Args, VarType).

set_pred_proc_ids_const([], _Const, Module, Module).
set_pred_proc_ids_const([PPId | PPIds], Const, Module0, Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(Module0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_termination(ProcInfo0, Termination0),

	Termination0 = term(_Const, Term, UsedArgs, MaybeError),
	Termination = term(Const, Term, UsedArgs, MaybeError),

	proc_info_set_termination(ProcInfo0, Termination, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(Module0, PredTable, Module1),
	set_pred_proc_ids_const(PPIds, Const, Module1, Module).

set_pred_proc_ids_terminates([], _Terminates, _, Module, Module).
set_pred_proc_ids_terminates([PPId | PPIds], Terminates, MaybeError, 
		Module0, Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(Module0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_termination(ProcInfo0, Termination0),

	Termination0 = term(Const, _Terminates, UsedArgs, _),
	Termination = term(Const, Terminates, UsedArgs, MaybeError),

	proc_info_set_termination(ProcInfo0, Termination, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(Module0, PredTable, Module1),
	set_pred_proc_ids_terminates(PPIds, Terminates, MaybeError, 
		Module1, Module).

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
split_unification_vars([], Modes, _InstTable, _ModuleInfo, Vars, Vars) :-
	bag__init(Vars),
	( Modes = [] ->
		true
	;
		error("term_util:split_unification_vars: Unmatched Variables")
	).
split_unification_vars([Arg | Args], Modes, InstTable, ModuleInfo,
		InVars, OutVars):-
	( Modes = [UniMode | UniModes] ->
		split_unification_vars(Args, UniModes, InstTable, ModuleInfo,
			InVars0, OutVars0),
		UniMode = ((_VarInit - ArgInit) -> (_VarFinal - ArgFinal)),
		( % if
			inst_is_bound(ArgInit, InstTable, ModuleInfo)
		->
			% Variable is an input variable
			bag__insert(InVars0, Arg, InVars),
			OutVars = OutVars0
		; % else if
			inst_is_free(ArgInit, InstTable, ModuleInfo),
			inst_is_bound(ArgFinal, InstTable, ModuleInfo) 
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

partition_call_args(_, _, [], [_ | _], _, _) :-
	error("Unmatched variables in term_util:partition_call_args").
partition_call_args(_, _, [_ | _], [], _, _) :-
	error("Unmatched variables in term_util__partition_call_args").
partition_call_args(_, _, [], [], [], []).
partition_call_args(InstTable, ModuleInfo, [ArgMode | ArgModes], [Arg | Args],
		InputArgs, OutputArgs) :-
	partition_call_args(InstTable, ModuleInfo, ArgModes, Args,
		InputArgs1, OutputArgs1),
	( mode_is_input(InstTable, ModuleInfo, ArgMode) ->
		InputArgs = [Arg | InputArgs1],
		OutputArgs = OutputArgs1
	; mode_is_output(InstTable, ModuleInfo, ArgMode) ->
		InputArgs = InputArgs1,
		OutputArgs = [Arg | OutputArgs1]
	;
		InputArgs = InputArgs1,
		OutputArgs = OutputArgs1
	).


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
		
%----------------------------------------------------------------------------%

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
functor_norm(use_map(ConsIdMap), TypeId, ConsId, _Module, Int,
		Args, Args, Modes, Modes) :-
	( map__search(ConsIdMap, TypeId - ConsId, WeightInfo) ->
		WeightInfo = weight(Int, _)
	;
		Int = 0
	).
functor_norm(use_map_and_args(ConsIdMap), TypeId, ConsId, _Module, Int,
		Args0, Args, Modes0, Modes) :-
	( map__search(ConsIdMap, TypeId - ConsId, WeightInfo) ->
		WeightInfo = weight(Int, UseArgList),
		(
			functor_norm_remove_elems(UseArgList, Args0, Args1, 
				Modes0, Modes1)
		->
			Modes = Modes1,
			Args = Args1
		;
			error("Unmatched lists in term_util__functor_norm_remove_elems.")
		)
	;
		Int = 0,
		Modes = Modes0,
		Args = Args0
	).
		
% This predicate will fail if the length of the input lists are not matched.
:- pred functor_norm_remove_elems(list(bool), list(var), list(var),
	list(uni_mode), list(uni_mode)).
:- mode functor_norm_remove_elems(in, in, out, in, out) is semidet.
functor_norm_remove_elems([], [], [], [], []).
functor_norm_remove_elems([yes | Bools], [Arg0 | Args0], [Arg0 | Args], 
		[Mode0 | Modes0], [Mode0 | Modes]) :-
	functor_norm_remove_elems(Bools, Args0, Args, Modes0, Modes).
functor_norm_remove_elems([no | Bools], [_Arg0 | Args0], Args, 
		[_Mode0 | Modes0], Modes) :-
	functor_norm_remove_elems(Bools, Args0, Args, Modes0, Modes).
	


%----------------------------------------------------------------------------%

do_ppid_check_terminates([] , _Error, Module, Module) --> [].
do_ppid_check_terminates([ PPId | PPIds ], Error, Module0, Module) --> 
	% look up markers
	{ PPId = proc(PredId, ProcId) },

	{ module_info_preds(Module0, PredTable0) },
	{ map__lookup(PredTable0, PredId, PredInfo0) },
	{ pred_info_procedures(PredInfo0, ProcTable0) },
	{ map__lookup(ProcTable0, ProcId, ProcInfo0) },
	{ proc_info_termination(ProcInfo0, Termination0) },
	{ Termination0 = term(Const, Terminates, UsedArgs, _) },
	( { Terminates = dont_know } ->
		{ Module2 = Module0 }
	;
		{ Termination = term(Const, dont_know, UsedArgs, yes(Error)) },
		{ proc_info_set_termination(ProcInfo0, Termination, ProcInfo)},
		{ map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable) },
		{ pred_info_set_procedures(PredInfo0, ProcTable, PredInfo) },
		{ map__det_update(PredTable0, PredId, PredInfo, PredTable) },
		{ module_info_set_preds(Module0, PredTable, Module1) },
		{ pred_info_get_markers(PredInfo, Markers) },
		globals__io_lookup_bool_option(check_termination,
			NormalErrors),
		globals__io_lookup_bool_option(verbose_check_termination,
			VerboseErrors),
		% If a check_terminates pragma exists, print out an error
		% message.
		% Note that this allows the one error to be printed out
		% multiple times.  This is because one error can cause a
		% number of predicates to be non terminating, and if
		% check_terminates is defined on all of the predicates,
		% then the error is printed out for each of them.
		( 
			{ \+ pred_info_is_imported(PredInfo) },
			{ check_marker(Markers, check_termination) }
		->
			term_errors__output(PredId, ProcId, Module1,
				Success),
			% Success is only no if there was no error
			% defined for this predicate.  As we just set the
			% error, term_errors__output should succeed.
			{ require(unify(Success, yes), "term_util.m: Unexpected value in do_ppid_check_terminates") },
			io__set_exit_status(1),
			{ module_info_incr_errors(Module1, Module2) }
		; % else if
			{ \+ pred_info_is_imported(PredInfo) },
			% Only output warnings of non-termination for
			% important errors, unless verbose errors are
			% enabled.  Important errors are errors where the
			% compiler analysed the code and was not able to
			% prove termination.  Unimportant warnings are
			% created when code is used/called which the
			% compiler was unable to analyse/prove termination
			% of.  
			(
				{ VerboseErrors = yes }
			;
				{ NormalErrors = yes },
				{ Error = _Context - TermError },
				{ \+ simple_error(TermError) }
			)
		->
			term_errors__output(PredId, ProcId, Module1,
				Success),
			% Success is only no if there was no error
			% defined for this predicate.  As we just set the
			% error, term_errors__output should succeed.
			{ require(unify(Success, yes), "term_util.m: Unexpected value in do_ppid_check_terminates") },
			{ Module2 = Module1 }
		;
			% Even though the predicate may not terminate, no
			% warning has been requested for it.
			{ Module2 = Module1 }
		)
	),
	do_ppid_check_terminates(PPIds, Error, Module2, Module).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
% This predicate calculates a weight table according to the following
% rules:
% If a type is directly recursive, its weight is given by the number of
% directly recursive arguments of this cons id.  For example, a list has
% size 1, a binary tree has size 2.  Each element of the bool list is
% assigned yes if the corresponding argument is directly recursive, and no
% otherwise.
% If a type is not directly recursive, then it is assigned a weight of 1,
% and the bool list is set to be all yes.
%
% If the bool list is used, then the effect of this norm is that the size
% of a data structure is given by the number of data elements in the data
% structure.  If the bool list is ignored, and the size of all the
% arguments are considered, then the size of a data structure is given by
% the total size of its data elements.

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

find_weights_for_cons(SymName - Args, TypeId, Params, Weights0, Weights) :-
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

