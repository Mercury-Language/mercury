%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
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
% -	defines some utility predicates
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__term_util.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module parse_tree__prog_data.
:- import_module transform_hlds__term_errors.
:- import_module transform_hlds__term_norm.

:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module std_util.

%-----------------------------------------------------------------------------%
%
% The types `arg_size_info' and `termination_info' hold information
% about procedures which is used for termination analysis.
% These types are stored as fields in the HLDS proc_info.
% For cross-module analysis, the information is written out as
% `pragma termination_info(...)' declarations in the
% `.opt' and `.trans_opt' files.  The module prog_data.m defines
% types similar to these two (but without the
% `list(termination_error_context)') which are used when parsing
% `termination_info' pragmas.
%

% The arg size info defines an upper bound on the difference
% between the sizes of the output arguments of a procedure and the sizes
% of the input arguments:
%
% | input arguments | + constant >= | output arguments |
%
% where | | represents a semilinear norm.

:- type arg_size_info ==
	generic_arg_size_info(list(termination_error_context)).

:- type termination_info ==
	generic_termination_info(unit, list(termination_error_context)).

% The type `used_args' holds a mapping which specifies for each procedure
% which of its arguments are used.

:- type used_args	==	map(pred_proc_id, list(bool)).

:- type pass_info
	--->	pass_info(
			functor_info,
			int,		% Max number of errors to gather.
			int		% Max number of paths to analyze.
		).

%-----------------------------------------------------------------------------%

% This predicate partitions the arguments of a call into a list of input
% variables and a list of output variables,

:- pred partition_call_args(module_info::in, list(mer_mode)::in, list(prog_var)::in,
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

:- pred remove_unused_args(bag(prog_var)::in, list(prog_var)::in,
	list(bool)::in, bag(prog_var)::out) is det.

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
:- pred horder_vars(list(prog_var)::in , vartypes::in) is semidet.

:- pred get_context_from_scc(list(pred_proc_id)::in, module_info::in,
	prog_context::out) is det.

	% Succeeds if the termination status of a procedure is known.
:- pred is_termination_known(module_info::in, pred_proc_id::in) is semidet.

	% Succeeds if the foreign proc attributes imply that a procedure
	% is terminating.
:- pred attributes_imply_termination(pragma_foreign_proc_attributes::in)
	is semidet.

	% terminates(Module, PPId).
	% Succeeds iff the procedure given by 'PPId' has been
	% proven to terminate.
	%
:- pred terminates(module_info::in, pred_proc_id::in) is semidet.

%-----------------------------------------------------------------------------%

	% Convert a prog_data__pragma_termination_info into a
	% term_util__termination_info, by adding the appropriate context.

:- pred add_context_to_termination_info(maybe(pragma_termination_info)::in,
	prog_context::in, maybe(termination_info)::out) is det.

	% Convert a prog_data__pragma_arg_size_info into a
	% term_util__arg_size_info, by adding the appropriate context.

:- pred add_context_to_arg_size_info(maybe(pragma_arg_size_info)::in,
	prog_context::in, maybe(arg_size_info)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_type.

:- import_module assoc_list.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

partition_call_args(Module, ArgModes, Args, InVarsBag, OutVarsBag) :-
	partition_call_args_2(Module, ArgModes, Args, InVars, OutVars),
	bag__from_list(InVars, InVarsBag),
	bag__from_list(OutVars, OutVarsBag).

:- pred partition_call_args_2(module_info::in, list(mer_mode)::in,
	list(prog_var)::in, list(prog_var)::out, list(prog_var)::out) is det.

partition_call_args_2(_, [], [], [], []).
partition_call_args_2(_, [], [_ | _], _, _) :-
	unexpected(this_file, "partition_call_args_2/5: unmatched variables.").
partition_call_args_2(_, [_ | _], [], _, _) :-
	unexpected(this_file, "partition_call_args_2/5: unmatched variables.").
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
		unexpected(this_file,
			"split_unification_vars/5: unmatched variables.")
	).
split_unification_vars([Arg | Args], Modes, ModuleInfo,
		InVars, OutVars):-
	( Modes = [UniMode | UniModes] ->
		split_unification_vars(Args, UniModes, ModuleInfo,
			InVars0, OutVars0),
		UniMode = ((_VarInit - ArgInit) -> (_VarFinal - ArgFinal)),
		(
			inst_is_bound(ModuleInfo, ArgInit)
		->
			% Variable is an input variable
			bag__insert(InVars0, Arg, InVars),
			OutVars = OutVars0
		;
			inst_is_free(ModuleInfo, ArgInit),
			inst_is_bound(ModuleInfo, ArgFinal)
		->
			% Variable is an output variable
			InVars = InVars0,
			bag__insert(OutVars0, Arg, OutVars)
		;
			InVars = InVars0,
			OutVars = OutVars0
		)
	;
		unexpected(this_file,
			"split_unification_vars/5: unmatched variables.")
	).

%-----------------------------------------------------------------------------%

make_bool_list(HeadVars0, Bools, Out) :-
	list__length(Bools, Arity),
	( list__drop(Arity, HeadVars0, HeadVars1) ->
		HeadVars = HeadVars1
	;
		unexpected(this_file, "make_bool_list/3: unmatched variables.")
	),
	make_bool_list_2(HeadVars, Bools, Out).

:- pred make_bool_list_2(list(_T)::in, list(bool)::in, list(bool)::out) is det.

make_bool_list_2([], Bools, Bools).
make_bool_list_2([ _ | Vars ], Bools, [no | Out]) :-
	make_bool_list_2(Vars, Bools, Out).

remove_unused_args(Vars, [], [], Vars).
remove_unused_args(Vars, [], [_X | _Xs], Vars) :-
	unexpected(this_file, "remove_unused_args/4: unmatched variables.").
remove_unused_args(Vars, [_X | _Xs], [], Vars) :-
	unexpected(this_file, "remove_unused_args/4: unmatched variables.").
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

set_pred_proc_ids_arg_size_info([], _ArgSize, !Module).
set_pred_proc_ids_arg_size_info([PPId | PPIds], ArgSize, !Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(!.Module, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0, ProcInfo),

	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(PredTable, !Module),
	set_pred_proc_ids_arg_size_info(PPIds, ArgSize, !Module).

set_pred_proc_ids_termination_info([], _Termination, !Module).
set_pred_proc_ids_termination_info([PPId | PPIds], Termination, !Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(!.Module, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),

	proc_info_set_maybe_termination_info(yes(Termination),
		ProcInfo0, ProcInfo),

	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(PredTable, !Module),
	set_pred_proc_ids_termination_info(PPIds, Termination, !Module).

lookup_proc_termination_info(Module, PPId, MaybeTermination) :-
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, MaybeTermination).

lookup_proc_arg_size_info(Module, PPId, MaybeArgSize) :-
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_get_maybe_arg_size_info(ProcInfo, MaybeArgSize).

horder_vars([Arg | Args], VarType) :-
	(
		map__lookup(VarType, Arg, Type),
		type_is_higher_order(Type, _, _, _, _)
	;
		horder_vars(Args, VarType)
	).

%-----------------------------------------------------------------------------%

get_context_from_scc(SCC, Module, Context) :-
	( SCC = [proc(PredId, _) | _] ->
		module_info_pred_info(Module, PredId, PredInfo),
		pred_info_context(PredInfo, Context)
	;
		unexpected(this_file, "get_context_from_scc/3: empty SCC.")
	).

%-----------------------------------------------------------------------------%

add_context_to_termination_info(no, _, no).
add_context_to_termination_info(yes(cannot_loop(_)), _,
	yes(cannot_loop(unit))).
add_context_to_termination_info(yes(can_loop(_)), Context,
	yes(can_loop([Context - imported_pred]))).

add_context_to_arg_size_info(no, _, no).
add_context_to_arg_size_info(yes(finite(A, B)), _, yes(finite(A, B))).
add_context_to_arg_size_info(yes(infinite(_)), Context,
		yes(infinite([Context - imported_pred]))).

%-----------------------------------------------------------------------------%

is_termination_known(Module, PPId) :-
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, yes(_)).

attributes_imply_termination(Attributes) :-
	(
		terminates(Attributes) = terminates
	;
		terminates(Attributes) = depends_on_mercury_calls,
		may_call_mercury(Attributes) = will_not_call_mercury
	).

%-----------------------------------------------------------------------------%

terminates(Module, PPId) :-
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, TerminationInfo),
	TerminationInfo = yes(cannot_loop(_)).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_util.m".

%-----------------------------------------------------------------------------%
:- end_module term_util.
%-----------------------------------------------------------------------------%
