%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File      : post_typecheck.m
% Author    : fjh
% Purpose   : finish off type checking.
%
% This module does the final parts of type analysis:
%
%	- it resolves predicate overloading
%	- it resolves function overloading
%	- it expands field access functions
%	- it propagates type information into the modes of procedures
%	- it checks for unbound type variables and if there are any,
%	  it reports an error (or a warning, binding them to the type `void').
%	- it reports errors for unbound inst variables in predicate or
%	  function mode declarations
%	- it reports errors for unsatisfied type class constraints
%	- it reports an error if there are indistinguishable modes for
%	  a predicate of function.
%
% These actions cannot be done until after type inference is complete,
% so they need to be a separate "post-typecheck pass".  For efficiency
% reasons, this is in fact done at the same time as purity analysis --
% the routines here are called from purity.m rather than mercury_compile.m.
%

:- module check_hlds__post_typecheck.
:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module list, io, bool, std_util, term.

	% post_typecheck__finish_preds(PredIds, ReportTypeErrors,
	%	NumErrors, FoundTypeError, Module0, Module)
	%
	% Check that all Aditi predicates have an `aditi__state' argument.
	% Check that the all of the types which have been inferred
	% for the variables in the clause do not contain any unbound type
	% variables other than those that occur in the types of head
	% variables, and that there are no unsatisfied type class
	% constraints, and if ReportErrors = yes, print appropriate
	% warning/error messages.
	% Also bind any unbound type variables to the type `void'.
	% Note that when checking assertions we take the conservative
	% approach of warning about unbound type variables.  There may
	% be cases for which this doesn't make sense.
	% FoundTypeError will be `yes' if there were errors which
	% should prevent further processing (e.g. polymorphism or
	% mode analysis).
	%
:- pred post_typecheck__finish_preds(list(pred_id)::in, bool::in,
	int::out, bool::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

	% As above, but don't check for `aditi__state's and return
	% the list of procedures containing unbound inst variables
	% instead of reporting the errors directly.
	%
:- pred post_typecheck__finish_pred_no_io(module_info::in, list(proc_id)::out,
	pred_info::in, pred_info::out) is det.

:- pred post_typecheck__finish_imported_pred_no_io(module_info::in,
	list(proc_id)::out, pred_info::in, pred_info::out) is det.

:- pred post_typecheck__finish_ill_typed_pred(module_info::in, pred_id::in,
	pred_info::in, pred_info::out, io::di, io::uo) is det.

	% Now that the assertion has finished being typechecked,
	% remove it from further processing and store it in the
	% assertion_table.
:- pred post_typecheck__finish_promise(promise_type::in, pred_id::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

	% Handle any unresolved overloading for a predicate call.
	%
:- pred post_typecheck__resolve_pred_overloading(list(prog_var)::in,
	pred_info::in, module_info::in, sym_name::in, sym_name::out,
	pred_id::in, pred_id::out) is det.

	% Resolve overloading and fill in the argument modes
	% of a call to an Aditi builtin.
	% Check that a relation modified by one of the Aditi update
	% goals is a base relation.
	%
:- pred post_typecheck__finish_aditi_builtin(module_info::in, pred_info::in,
	list(prog_var)::in, term__context::in,
	aditi_builtin::in, aditi_builtin::out,
	simple_call_id::in, simple_call_id::out, list(mode)::out,
	maybe(aditi_builtin_error)::out) is det.

:- type aditi_builtin_error
	--->	aditi_update_of_derived_relation(prog_context,
			aditi_builtin, simple_call_id).

:- pred report_aditi_builtin_error(aditi_builtin_error::in, io::di, io::uo)
	is det.

	% Work out whether a var-functor unification is actually a function
	% call. If so, replace the unification goal with a call.
	%
:- pred post_typecheck__resolve_unify_functor(prog_var::in, cons_id::in,
	list(prog_var)::in, unify_mode::in, unification::in, unify_context::in,
	hlds_goal_info::in, module_info::in, pred_info::in, pred_info::out,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	hlds_goal::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_errors.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__modecheck_call.
:- import_module check_hlds__type_util.
:- import_module check_hlds__typecheck.
:- import_module hlds__assertion.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_out.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module map, set, assoc_list, term, require, int.
:- import_module string, varset.

%-----------------------------------------------------------------------------%

post_typecheck__finish_preds(PredIds, ReportTypeErrors, NumErrors,
		FoundTypeError, !ModuleInfo, !IO) :-
	post_typecheck__finish_preds(PredIds, ReportTypeErrors,
		!ModuleInfo, 0, NumErrors, no, FoundTypeError, !IO).

:- pred post_typecheck__finish_preds(list(pred_id)::in, bool::in,
	module_info::in, module_info::out, int::in, int::out,
	bool::in, bool::out, io::di, io::uo) is det.

post_typecheck__finish_preds([], _, !ModuleInfo, !NumErrors,
		!PostTypecheckError, !IO).
post_typecheck__finish_preds([PredId | PredIds], ReportTypeErrors,
		!ModuleInfo, !NumErrors, !FoundTypeError, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
	(
		( pred_info_is_imported(PredInfo0)
		; pred_info_is_pseudo_imported(PredInfo0)
		)
	->
		post_typecheck__finish_imported_pred(!.ModuleInfo, PredId,
			PredInfo0, PredInfo, !IO)
	;
		%
		% Only report error messages for unbound type variables
		% if we didn't get any type errors already; this avoids
		% a lot of spurious diagnostics.
		%
		post_typecheck__check_type_bindings(!.ModuleInfo, PredId,
			PredInfo0, PredInfo1, ReportTypeErrors,
			UnboundTypeErrsInThisPred, !IO),

		%
		% if there were any unsatisfied type class constraints,
		% then that can cause internal errors in polymorphism.m
		% if we try to continue, so we need to halt compilation
		% after this pass.
		%
		( UnboundTypeErrsInThisPred \= 0 ->
			!:FoundTypeError = yes
		;
			true
		),

		post_typecheck__finish_pred_no_io(!.ModuleInfo,
			ErrorProcs, PredInfo1, PredInfo2),
		report_unbound_inst_vars(!.ModuleInfo, PredId,
			ErrorProcs, PredInfo2, PredInfo3, !IO),
		check_for_indistinguishable_modes(!.ModuleInfo, PredId,
			PredInfo3, PredInfo, !IO),

		%
		% check that main/2 has the right type
		%
		( ReportTypeErrors = yes ->
			check_type_of_main(PredInfo, !IO)
		;
			true
		),

		%
		% Check that all Aditi predicates have an `aditi__state'
		% argument. This must be done after typechecking because
		% of type inference -- the types of some Aditi predicates
		% may not be known before.
		%
		pred_info_get_markers(PredInfo, Markers),
		( ReportTypeErrors = yes, check_marker(Markers, aditi) ->
			check_aditi_state(!.ModuleInfo, PredInfo, !IO)
		;
			true
		),

		!:NumErrors = !.NumErrors + UnboundTypeErrsInThisPred
	),
	module_info_set_pred_info(PredId, PredInfo, !ModuleInfo),
	post_typecheck__finish_preds(PredIds, ReportTypeErrors,
		!ModuleInfo, !NumErrors, !FoundTypeError, !IO).

%-----------------------------------------------------------------------------%
%			Check for unbound type variables
%
%  Check that the all of the types which have been inferred
%  for the variables in the clause do not contain any unbound type
%  variables other than those that occur in the types of head
%  variables, and that there are no unsatisfied type class constraints.

:- pred post_typecheck__check_type_bindings(module_info::in, pred_id::in,
	pred_info::in, pred_info::out, bool::in, int::out, io::di, io::uo)
	is det.

post_typecheck__check_type_bindings(ModuleInfo, PredId, PredInfo0, PredInfo,
		ReportErrs, NumErrors, !IO) :-
	(
		ReportErrs = yes,
		pred_info_get_unproven_body_constraints(PredInfo0,
			UnprovenConstraints0),
		UnprovenConstraints0 \= []
	->
		list__sort_and_remove_dups(UnprovenConstraints0,
			UnprovenConstraints),
		report_unsatisfied_constraints(UnprovenConstraints,
			PredId, PredInfo0, ModuleInfo, !IO),
		list__length(UnprovenConstraints, NumErrors)
	;
		NumErrors = 0
	),

	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	pred_info_get_head_type_params(PredInfo0, HeadTypeParams),
	clauses_info_varset(ClausesInfo0, VarSet),
	clauses_info_vartypes(ClausesInfo0, VarTypesMap0),
	map__to_assoc_list(VarTypesMap0, VarTypesList),
	set__init(Set0),
	check_type_bindings_2(VarTypesList, HeadTypeParams, [], Errs,
		Set0, Set),
	( Errs = [] ->
		PredInfo = PredInfo0
	;
		( ReportErrs = yes ->
			%
			% report the warning
			%
			report_unresolved_type_warning(Errs, PredId, PredInfo0,
				ModuleInfo, VarSet, !IO)
		;
			true
		),

		%
		% bind all the type variables in `Set' to `void' ...
		%
		pred_info_get_constraint_proofs(PredInfo0, Proofs0),
		bind_type_vars_to_void(Set, VarTypesMap0, VarTypesMap,
			Proofs0, Proofs),
		clauses_info_set_vartypes(VarTypesMap,
			ClausesInfo0, ClausesInfo),
		pred_info_set_clauses_info(ClausesInfo, PredInfo0, PredInfo1),
		pred_info_set_constraint_proofs(Proofs, PredInfo1, PredInfo)
	).

:- pred check_type_bindings_2(assoc_list(prog_var, (type))::in, list(tvar)::in,
	assoc_list(prog_var, (type))::in, assoc_list(prog_var, (type))::out,
	set(tvar)::in, set(tvar)::out) is det.

check_type_bindings_2([], _, !Errs, !Set).
check_type_bindings_2([Var - Type | VarTypes], HeadTypeParams, !Errs, !Set) :-
	term__vars(Type, TVars),
	set__list_to_set(TVars, TVarsSet0),
	set__delete_list(TVarsSet0, HeadTypeParams, TVarsSet1),
	( \+ set__empty(TVarsSet1) ->
		!:Errs = [Var - Type | !.Errs],
		set__union(!.Set, TVarsSet1, !:Set)
	;
		true
	),
	check_type_bindings_2(VarTypes, HeadTypeParams, !Errs, !Set).

%
% bind all the type variables in `UnboundTypeVarsSet' to the type `void' ...
%
:- pred bind_type_vars_to_void(set(tvar)::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	constraint_proof_map::in, constraint_proof_map::out) is det.

bind_type_vars_to_void(UnboundTypeVarsSet, !VarTypesMap, !Proofs) :-
	%
	% first create a pair of corresponding lists (UnboundTypeVars, Voids)
	% that map the unbound type variables to void
	%
	set__to_sorted_list(UnboundTypeVarsSet, UnboundTypeVars),
	list__length(UnboundTypeVars, Length),
	list__duplicate(Length, void_type, Voids),

	%
	% then create a *substitution* that maps the
	% unbound type variables to void.
	%
	map__from_corresponding_lists(UnboundTypeVars, Voids, VoidSubst),

	%
	% then apply the substitutions we just created to the variable types
	% and constraint proofs
	%
	map__keys(!.VarTypesMap, Vars),
	map__values(!.VarTypesMap, Types0),
	term__substitute_corresponding_list(UnboundTypeVars, Voids,
		Types0, Types),
	map__from_corresponding_lists(Vars, Types, !:VarTypesMap),

	apply_subst_to_constraint_proofs(VoidSubst, !Proofs).

%-----------------------------------------------------------------------------%
%
% report an error: unsatisfied type class constraints
%
:- pred report_unsatisfied_constraints(list(class_constraint)::in,
	pred_id::in, pred_info::in, module_info::in, io::di, io::uo) is det.

report_unsatisfied_constraints(Constraints, PredId, PredInfo, ModuleInfo) -->
	io__set_exit_status(1),

	{ pred_info_typevarset(PredInfo, TVarSet) },
	{ pred_info_context(PredInfo, Context) },

        prog_out__write_context(Context),
	io__write_string("In "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),

	prog_out__write_context(Context),
	io__write_string(
		"  type error: unsatisfied typeclass constraint(s):\n"),

	prog_out__write_context(Context),
	io__write_string("  "),
	{ AppendVarnums = no },
	io__write_list(Constraints, ", ",
		mercury_output_constraint(TVarSet, AppendVarnums)),
	io__write_string(".\n").

%
% report a warning: uninstantiated type parameter
%
:- pred report_unresolved_type_warning(assoc_list(prog_var, (type))::in,
	pred_id::in, pred_info::in, module_info::in, prog_varset::in,
	io::di, io::uo) is det.

report_unresolved_type_warning(Errs, PredId, PredInfo, ModuleInfo, VarSet) -->
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn),
	( { HaltAtWarn = yes } ->
		 io__set_exit_status(1)
	;
		[]
	),

	{ pred_info_typevarset(PredInfo, TypeVarSet) },
	{ pred_info_context(PredInfo, Context) },

        prog_out__write_context(Context),
	io__write_string("In "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),

        prog_out__write_context(Context),
	io__write_string("  warning: unresolved polymorphism.\n"),
	prog_out__write_context(Context),
	( { Errs = [_] } ->
		io__write_string("  The variable with an unbound type was:\n")
	;
		io__write_string("  The variables with unbound types were:\n")
	),
	write_type_var_list(Errs, Context, VarSet, TypeVarSet),
	prog_out__write_context(Context),
	io__write_string("  The unbound type variable(s) will be implicitly\n"),
	prog_out__write_context(Context),
	io__write_string("  bound to the builtin type `void'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_strings([
"\tThe body of the clause contains a call to a polymorphic predicate,\n",
"\tbut I can't determine which version should be called,\n",
"\tbecause the type variables listed above didn't get bound.\n",
% "\tYou may need to use an explicit type qualifier.\n",
% XXX improve error message
"\t(I ought to tell you which call caused the problem, but I'm afraid\n",
"\tyou'll have to work it out yourself.  My apologies.)\n"
			])
	;
		[]
	).

:- pred write_type_var_list(assoc_list(prog_var, (type))::in, prog_context::in,
	prog_varset::in, tvarset::in, io::di, io::uo) is det.

write_type_var_list([], _, _, _) --> [].
write_type_var_list([Var - Type | Rest], Context, VarSet, TVarSet) -->
	prog_out__write_context(Context),
	io__write_string("      "),
	mercury_output_var(Var, VarSet, no),
	io__write_string(": "),
	mercury_output_term(Type, TVarSet, no),
	io__write_string("\n"),
	write_type_var_list(Rest, Context, VarSet, TVarSet).

%-----------------------------------------------------------------------------%
%			resolve predicate overloading

% In the case of a call to an overloaded predicate, typecheck.m
% does not figure out the correct pred_id.  We must do that here.

post_typecheck__resolve_pred_overloading(Args0, CallerPredInfo,
		ModuleInfo, PredName0, PredName, PredId0, PredId) :-
	( PredId0 = invalid_pred_id ->
		%
		% Find the set of candidate pred_ids for predicates which
		% have the specified name and arity
		%
		pred_info_typevarset(CallerPredInfo, TVarSet),
		pred_info_get_markers(CallerPredInfo, Markers),
		pred_info_clauses_info(CallerPredInfo, ClausesInfo),
		clauses_info_vartypes(ClausesInfo, VarTypes),
		map__apply_to_list(Args0, VarTypes, ArgTypes),
		typecheck__resolve_pred_overloading(ModuleInfo, Markers,
			ArgTypes, TVarSet, PredName0, PredName, PredId)
        ;
		PredId = PredId0,
		PredName = get_qualified_pred_name(ModuleInfo, PredId)
        ).

:- func get_qualified_pred_name(module_info, pred_id) = sym_name.

get_qualified_pred_name(ModuleInfo, PredId)
		= qualified(PredModule, PredName) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	PredModule = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo).

%-----------------------------------------------------------------------------%

post_typecheck__finish_aditi_builtin(ModuleInfo, CallerPredInfo, Args, Context,
		aditi_tuple_update(Update, PredId0), Builtin,
		PredOrFunc - SymName0/Arity, InsertCallId,
		Modes, MaybeError) :-
	% make_hlds.m checks the arity, so this is guaranteed to succeed.
	get_state_args_det(Args, OtherArgs, _, _),

	% The tuple to insert has the same argument types as
	% the relation being inserted into.
	post_typecheck__resolve_pred_overloading(OtherArgs, CallerPredInfo,
		ModuleInfo, SymName0, SymName, PredId0, PredId),

	Builtin = aditi_tuple_update(Update, PredId),
	InsertCallId = PredOrFunc - SymName/Arity,

	module_info_pred_info(ModuleInfo, PredId, RelationPredInfo),
	check_base_relation(Context, RelationPredInfo,
		Builtin, InsertCallId, MaybeError),

	% `aditi_insert' calls do not use the `aditi_state' argument
	% in the tuple to insert, so set its mode to `unused'.
	% The other arguments all have mode `in'.
	pred_info_arg_types(RelationPredInfo, ArgTypes),
	in_mode(InMode),
	unused_mode(AditiStateMode),
	aditi_builtin_modes(InMode, AditiStateMode, ArgTypes, InsertArgModes),
	list__append(InsertArgModes, [aditi_di_mode, aditi_uo_mode], Modes).

post_typecheck__finish_aditi_builtin(ModuleInfo, CallerPredInfo, Args, Context,
		Builtin0, Builtin, PredOrFunc - SymName0/Arity,
		UpdateCallId, Modes, MaybeError) :-
	Builtin0 = aditi_bulk_update(Update, PredId0, Syntax),
	UnchangedArgTypes = (pred(X::in, X::out) is det),
	(
		Update = bulk_insert,
		AdjustArgTypes = UnchangedArgTypes
	;
		Update = bulk_delete,
		AdjustArgTypes = UnchangedArgTypes
	;
		Update = bulk_modify,
		% The argument types of the closure passed to `aditi_modify'
		% contain two copies of the arguments of the base relation -
		% one set input and one set output.
		AdjustArgTypes =
		    (pred(Types0::in, Types::out) is det :-
			list__length(Types0, Length),
			HalfLength = Length // 2,
			( list__split_list(HalfLength, Types0, Types1, _) ->
				Types = Types1
			;
				error(
			"post_typecheck__finish_aditi_builtin: aditi_modify")
			)
		    )
	),
	resolve_aditi_builtin_overloading(ModuleInfo, CallerPredInfo, Args,
		AdjustArgTypes, PredId0, PredId, SymName0, SymName),
	Builtin = aditi_bulk_update(Update, PredId, Syntax),

	UpdateCallId = PredOrFunc - SymName/Arity,

	module_info_pred_info(ModuleInfo, PredId, RelationPredInfo),
	check_base_relation(Context, RelationPredInfo,
		Builtin, UpdateCallId, MaybeError),

	pred_info_arg_types(RelationPredInfo, ArgTypes),
	post_typecheck__bulk_update_closure_info(Update,
		PredOrFunc, ArgTypes, ClosurePredOrFunc,
		ClosureArgModes, ClosureDetism),

	Inst = ground(shared, higher_order(pred_inst_info(ClosurePredOrFunc,
		ClosureArgModes, ClosureDetism))),
	Modes = [(Inst -> Inst), aditi_di_mode, aditi_uo_mode].

:- pred post_typecheck__bulk_update_closure_info(aditi_bulk_update::in,
	pred_or_func::in, list(type)::in, pred_or_func::out, list(mode)::out,
	determinism::out) is det.

post_typecheck__bulk_update_closure_info(bulk_insert, PredOrFunc,
		ArgTypes, PredOrFunc, ClosureArgModes, nondet) :-
	out_mode(OutMode),
	AditiStateMode = aditi_mui_mode,
	aditi_builtin_modes(OutMode, AditiStateMode,
		ArgTypes, ClosureArgModes).
post_typecheck__bulk_update_closure_info(bulk_delete,
		PredOrFunc, ArgTypes, PredOrFunc, ClosureArgModes, nondet) :-
	ArgMode = out_mode,
	AditiStateMode = aditi_mui_mode,
	aditi_builtin_modes(ArgMode, AditiStateMode,
		ArgTypes, ClosureArgModes).
post_typecheck__bulk_update_closure_info(bulk_modify,
		_PredOrFunc, ArgTypes, LambdaPredOrFunc,
		ClosureArgModes, nondet) :-
	LambdaPredOrFunc = predicate,
	out_mode(OutMode),
	unused_mode(UnusedMode),
	DeleteArgMode = OutMode,
	DeleteAditiStateMode = aditi_mui_mode,
	aditi_builtin_modes(DeleteArgMode, DeleteAditiStateMode,
			ArgTypes, DeleteArgModes),

	InsertArgMode = OutMode,
	InsertAditiStateMode = UnusedMode,
	aditi_builtin_modes(InsertArgMode, InsertAditiStateMode,
		ArgTypes, InsertArgModes),
	list__append(DeleteArgModes, InsertArgModes, ClosureArgModes).

	% Use the type of the closure passed to an `aditi_delete',
	% `aditi_bulk_insert', `aditi_bulk_delete' or `aditi_modify'
	% call to work out which predicate is being updated.
:- pred resolve_aditi_builtin_overloading(module_info::in, pred_info::in,
	list(prog_var)::in,
	pred(list(type), list(type))::in(pred(in, out) is det),
	pred_id::in, pred_id::out, sym_name::in, sym_name::out) is det.

resolve_aditi_builtin_overloading(ModuleInfo, CallerPredInfo, Args,
		AdjustArgTypes, PredId0, PredId, SymName0, SymName) :-
	% make_hlds.m checks the arity, so this is guaranteed to succeed.
	get_state_args_det(Args, OtherArgs, _, _),
	( PredId0 = invalid_pred_id ->
		(
			OtherArgs = [HOArg],
			pred_info_typevarset(CallerPredInfo, TVarSet),
			pred_info_clauses_info(CallerPredInfo, ClausesInfo),
			clauses_info_vartypes(ClausesInfo, VarTypes),
			map__lookup(VarTypes, HOArg, HOArgType),
			type_is_higher_order(HOArgType, _Purity,
				_, EvalMethod, ArgTypes0),
			EvalMethod \= normal
		->
			call(AdjustArgTypes, ArgTypes0, ArgTypes),
			pred_info_get_markers(CallerPredInfo, Markers),
			typecheck__resolve_pred_overloading(ModuleInfo,
				Markers, ArgTypes, TVarSet,
				SymName0, SymName, PredId)
		;
			error(
			"post_typecheck__resolve_aditi_builtin_overloading")
		)
	;
		PredId = PredId0,
		SymName = get_qualified_pred_name(ModuleInfo, PredId)
	).

	% Work out the modes of the arguments of a closure passed
	% to an Aditi update.
	% The `Mode' passed is the mode of all arguments apart
	% from the `aditi__state'.
:- pred aditi_builtin_modes((mode)::in, (mode)::in, list(type)::in,
	list(mode)::out) is det.

aditi_builtin_modes(_, _, [], []).
aditi_builtin_modes(Mode, AditiStateMode, [ArgType | ArgTypes],
		[ArgMode | ArgModes]) :-
	( type_is_aditi_state(ArgType) ->
		ArgMode = AditiStateMode
	;
		ArgMode = Mode
	),
	aditi_builtin_modes(Mode, AditiStateMode, ArgTypes, ArgModes).

	% Report an error if a predicate modified by an Aditi builtin
	% is not a base relation.
:- pred check_base_relation(prog_context::in, pred_info::in, aditi_builtin::in,
	simple_call_id::in, maybe(aditi_builtin_error)::out) is det.

check_base_relation(Context, PredInfo, Builtin, CallId, MaybeError) :-
	( hlds_pred__pred_info_is_base_relation(PredInfo) ->
		MaybeError = no
	;
		MaybeError = yes(aditi_update_of_derived_relation(Context,
					Builtin, CallId))
	).

report_aditi_builtin_error(
		aditi_update_of_derived_relation(Context, Builtin, CallId)) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("In "),
	hlds_out__write_call_id(generic_call(aditi_builtin(Builtin, CallId))),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: the modified "),
	{ CallId = PredOrFunc - _ },
	prog_out__write_pred_or_func(PredOrFunc),
	io__write_string(" is not a base relation.\n").

%-----------------------------------------------------------------------------%

post_typecheck__finish_pred_no_io(ModuleInfo, ErrorProcs,
		PredInfo0, PredInfo) :-
	post_typecheck__propagate_types_into_modes(ModuleInfo,
			ErrorProcs, PredInfo0, PredInfo).

	%
	% For ill-typed preds, we just need to set the modes up correctly
	% so that any calls to that pred from correctly-typed predicates
	% won't result in spurious mode errors.
	%
post_typecheck__finish_ill_typed_pred(ModuleInfo, PredId, !PredInfo, !IO) :-
	post_typecheck__propagate_types_into_modes(ModuleInfo, ErrorProcs,
		!PredInfo),
	report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs, !PredInfo,
		!IO),
	check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo, !IO).

	%
	% For imported preds, we just need to ensure that all
	% constructors occurring in predicate mode declarations are
	% module qualified.
	%
:- pred post_typecheck__finish_imported_pred(module_info::in, pred_id::in,
	pred_info::in, pred_info::out, io::di, io::uo) is det.

post_typecheck__finish_imported_pred(ModuleInfo, PredId, !PredInfo, !IO) :-
	pred_info_get_markers(!.PredInfo, Markers),
	(
		check_marker(Markers, base_relation),
		ModuleName = pred_info_module(!.PredInfo),
		module_info_name(ModuleInfo, ModuleName)
	->
		check_aditi_state(ModuleInfo, !.PredInfo, !IO)
	;
		true
	),
	% XXX maybe the rest should be replaced with a call to
	% post_typecheck__finish_ill_typed_pred? [zs]
	post_typecheck__finish_imported_pred_no_io(ModuleInfo, ErrorProcs,
		!PredInfo),
	report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs, !PredInfo,
		!IO),
	check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo, !IO).

post_typecheck__finish_imported_pred_no_io(ModuleInfo, Errors, !PredInfo) :-
	% Make sure the var-types field in the clauses_info is
	% valid for imported predicates.
	% Unification procedures have clauses generated, so
	% they already have valid var-types.
	( pred_info_is_pseudo_imported(!.PredInfo) ->
		true
	;
		pred_info_clauses_info(!.PredInfo, ClausesInfo0),
		clauses_info_headvars(ClausesInfo0, HeadVars),
		pred_info_arg_types(!.PredInfo, ArgTypes),
		map__from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
		clauses_info_set_vartypes(VarTypes, ClausesInfo0, ClausesInfo),
		pred_info_set_clauses_info(ClausesInfo, !PredInfo)
	),
	post_typecheck__propagate_types_into_modes(ModuleInfo, Errors,
		!PredInfo).

	%
	% Now that the promise has finished being typechecked,
	% and has had all of its pred_ids identified,
	% remove the promise from the list of pred ids to be processed
	% in the future and place the pred_id associated with the
	% promise into the assertion or promise_ex table.
	% For each assertion that is in the interface, you need to check
	% that it doesn't refer to any symbols which are local to that
	% module.
	% Also record for each predicate that is used in an assertion
	% which assertion it is used in, or for a promise ex declaration
	% record in the promise ex table the predicates used by the
	% declaration.
	%
post_typecheck__finish_promise(PromiseType, PromiseId, !Module, !IO) :-
		% Store the declaration in the appropriate table and get
		% the goal for the promise
	store_promise(PromiseType, PromiseId, !Module, Goal),

		% Remove from further processing.
	module_info_remove_predid(PromiseId, !Module),

		% If the promise is in the interface, then ensure that
		% it doesn't refer to any local symbols.
	module_info_pred_info(!.Module, PromiseId, PredInfo),
	( pred_info_is_exported(PredInfo) ->
		assertion__in_interface_check(Goal, PredInfo, !Module, !IO)
	;
		true
	).

	% store promise declaration, normalise goal and return new
	% module_info and the goal for further processing
:- pred store_promise(promise_type::in, pred_id::in,
	module_info::in, module_info::out, hlds_goal::out) is det.

store_promise(PromiseType, PromiseId, !Module, Goal) :-
	(
		% case for assertions
		PromiseType = true
	->
		module_info_assertion_table(!.Module, AssertTable0),
		assertion_table_add_assertion(PromiseId, AssertionId,
			AssertTable0, AssertTable),
		module_info_set_assertion_table(AssertTable, !Module),
		assertion__goal(AssertionId, !.Module, Goal),
		assertion__record_preds_used_in(Goal, AssertionId, !Module)
	;
		% case for exclusivity
		(
			PromiseType = exclusive
		;
			PromiseType = exclusive_exhaustive
		)
	->
		promise_ex_goal(PromiseId, !.Module, Goal),
		predids_from_goal(Goal, PredIds),
		module_info_exclusive_table(!.Module, Table0),
		list__foldl(exclusive_table_add(PromiseId), PredIds,
			Table0, Table),
		module_info_set_exclusive_table(Table, !Module)

	;
		% case for exhaustiveness -- XXX not yet implemented
		promise_ex_goal(PromiseId, !.Module, Goal)
	).

	% get the goal from a promise_ex declaration
:- pred promise_ex_goal(pred_id::in, module_info::in, hlds_goal::out) is det.

promise_ex_goal(ExclusiveDecl, Module, Goal) :-
        module_info_pred_info(Module, ExclusiveDecl, PredInfo),
        pred_info_clauses_info(PredInfo, ClausesInfo),
        clauses_info_clauses(ClausesInfo, Clauses),
        (
		Clauses = [clause(_ProcIds, Goal0, _Lang, _Context)]
	->
		assertion__normalise_goal(Goal0, Goal)
	;
		error("promise_ex__goal: not an promise")
	).

%-----------------------------------------------------------------------------%

:- pred check_type_of_main(pred_info::in, io::di, io::uo) is det.

check_type_of_main(PredInfo, !IO) :-
	(
		%
		% Check if this predicate is the
		% program entry point main/2.
		%
		pred_info_name(PredInfo) = "main",
		pred_info_arity(PredInfo) = 2,
		pred_info_is_exported(PredInfo)
	->
		%
		% Check that the arguments of main/2
		% have type `io__state'.
		%
		pred_info_arg_types(PredInfo, ArgTypes),
		(
			ArgTypes = [Arg1, Arg2],
			type_is_io_state(Arg1),
			type_is_io_state(Arg2)
		->
			true
		;
			pred_info_context(PredInfo, Context),
			error_util__write_error_pieces(Context, 0,
				[words("Error: arguments of main/2"),
				words("must have type `io__state'.")], !IO),
			io__set_exit_status(1, !IO)
		)
	;
		true
	).

%-----------------------------------------------------------------------------%

	%
	% Ensure that all constructors occurring in predicate mode
	% declarations are module qualified.
	%
:- pred post_typecheck__propagate_types_into_modes(module_info::in,
	list(proc_id)::out, pred_info::in, pred_info::out) is det.

post_typecheck__propagate_types_into_modes(ModuleInfo, ErrorProcs,
		!PredInfo) :-
	pred_info_arg_types(!.PredInfo, ArgTypes),
	pred_info_procedures(!.PredInfo, Procs0),
	ProcIds = pred_info_procids(!.PredInfo),
	propagate_types_into_proc_modes(ModuleInfo, ProcIds, ArgTypes,
		[], ErrorProcs, Procs0, Procs),
	pred_info_set_procedures(Procs, !PredInfo).

%-----------------------------------------------------------------------------%

:- pred propagate_types_into_proc_modes(module_info::in, list(proc_id)::in,
	list(type)::in, list(proc_id)::in, list(proc_id)::out,
	proc_table::in, proc_table::out) is det.

propagate_types_into_proc_modes(_, [], _,
		ErrorProcs, list__reverse(ErrorProcs), !Procs).
propagate_types_into_proc_modes(ModuleInfo, [ProcId | ProcIds], ArgTypes,
		!ErrorProcs, !Procs) :-
	map__lookup(!.Procs, ProcId, ProcInfo0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	propagate_types_into_mode_list(ArgTypes, ModuleInfo,
		ArgModes0, ArgModes),

	%
	% check for unbound inst vars
	% (this needs to be done after propagate_types_into_mode_list,
	% because we need the insts to be module-qualified; and it
	% needs to be done before mode analysis, to avoid internal errors)
	%
	( mode_list_contains_inst_var(ArgModes, ModuleInfo, _InstVar) ->
		!:ErrorProcs = [ProcId | !.ErrorProcs]
	;
		proc_info_set_argmodes(ArgModes, ProcInfo0, ProcInfo),
		map__det_update(!.Procs, ProcId, ProcInfo, !:Procs)
	),
	propagate_types_into_proc_modes(ModuleInfo, ProcIds, ArgTypes,
		!ErrorProcs, !Procs).

:- pred report_unbound_inst_vars(module_info::in, pred_id::in,
	list(proc_id)::in, pred_info::in, pred_info::out,
	io::di, io::uo) is det.

report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs, !PredInfo, !IO) :-
	( ErrorProcs = [] ->
		true
	;
		pred_info_procedures(!.PredInfo, ProcTable0),
		list__foldl2(report_unbound_inst_var_error(ModuleInfo, PredId),
			ErrorProcs, ProcTable0, ProcTable, !IO),
		pred_info_set_procedures(ProcTable, !PredInfo)
	).

:- pred report_unbound_inst_var_error(module_info::in,
	pred_id::in, proc_id::in, proc_table::in, proc_table::out,
	io::di, io::uo) is det.

report_unbound_inst_var_error(ModuleInfo, PredId, ProcId, Procs0, Procs,
		!IO) :-
	map__lookup(Procs0, ProcId, ProcInfo),
	unbound_inst_var_error(PredId, ProcInfo, ModuleInfo, !IO),
	% delete this mode, to avoid internal errors
	map__det_remove(Procs0, ProcId, _, Procs).

:- pred unbound_inst_var_error(pred_id::in, proc_info::in, module_info::in,
	io::di, io::uo) is det.

unbound_inst_var_error(PredId, ProcInfo, ModuleInfo) -->
	{ proc_info_context(ProcInfo, Context) },
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("In mode declaration for "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: unbound inst variable(s).\n"),
	prog_out__write_context(Context),
	io__write_string("  (Sorry, polymorphic modes are not supported.)\n").

%-----------------------------------------------------------------------------%

:- pred check_for_indistinguishable_modes(module_info::in, pred_id::in,
	pred_info::in, pred_info::out, io::di, io::uo) is det.

check_for_indistinguishable_modes(ModuleInfo, PredId, !PredInfo, !IO) :-
	(
		%
		% Don't check for indistinguishable modes in unification
		% predicates.  The default (in, in) mode must be
		% semidet, but for single-value types we also want to
		% create a det mode which will be indistinguishable
		% from the semidet mode.
		% (When the type is known, the det mode is called,
		% but the polymorphic unify needs to be able to call
		% the semidet mode.)
		%
		pred_info_get_maybe_special_pred(!.PredInfo, MaybeSpecial),
		MaybeSpecial = yes(unify - _)
	->
		true
	;
		ProcIds = pred_info_procids(!.PredInfo),
		check_for_indistinguishable_modes(ModuleInfo, PredId,
			ProcIds, [], !PredInfo, !IO)
	).

:- pred check_for_indistinguishable_modes(module_info::in, pred_id::in,
	list(proc_id)::in, list(proc_id)::in, pred_info::in, pred_info::out,
	io::di, io::uo) is det.

check_for_indistinguishable_modes(_, _, [], _, !PredInfo, !IO).
check_for_indistinguishable_modes(ModuleInfo, PredId, [ProcId | ProcIds],
		PrevProcIds, !PredInfo, !IO) :-
	check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId,
		PrevProcIds, Removed, !PredInfo, !IO),
	PrevProcIds1 =
		( if Removed = yes then PrevProcIds
		  else [ProcId | PrevProcIds]
		),
	check_for_indistinguishable_modes(ModuleInfo, PredId, ProcIds,
		PrevProcIds1, !PredInfo, !IO).

:- pred check_for_indistinguishable_mode(module_info::in, pred_id::in,
	proc_id::in, list(proc_id)::in, bool::out,
	pred_info::in, pred_info::out, io::di, io::uo) is det.

check_for_indistinguishable_mode(_, _, _, [], no, !PredInfo, !IO).
check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId1,
		[ProcId | ProcIds], Removed, !PredInfo, !IO) :-
	(
		modes_are_indistinguishable(ProcId, ProcId1,
			!.PredInfo, ModuleInfo)
	->
		pred_info_import_status(!.PredInfo, Status),
		globals__io_lookup_bool_option(intermodule_optimization,
			Intermod, !IO),
		globals__io_lookup_bool_option(make_optimization_interface,
			MakeOptInt, !IO),
		(
			% With `--intermodule-optimization' we can read
			% the declarations for a predicate from the `.int'
			% and `.int0' files, so ignore the error in that case.
			(
				status_defined_in_this_module(Status, yes)
			;
				Intermod = no
			;
				MakeOptInt = yes
			)
		->
			report_indistinguishable_modes_error(ProcId1,
				ProcId, PredId, !.PredInfo, ModuleInfo, !IO)
		;
			true
		),
		pred_info_remove_procid(ProcId1, !PredInfo),
		Removed = yes
	;
		check_for_indistinguishable_mode(ModuleInfo, PredId, ProcId1,
			ProcIds, Removed, !PredInfo, !IO)
	).

%-----------------------------------------------------------------------------%

:- pred check_aditi_state(module_info::in, pred_info::in,
	io::di, io::uo) is det.

check_aditi_state(ModuleInfo, PredInfo, !IO) :-
	pred_info_arg_types(PredInfo, ArgTypes),
	list__filter(type_is_aditi_state, ArgTypes, AditiStateTypes),
	( AditiStateTypes = [] ->
		report_no_aditi_state(PredInfo, !IO)
	;
		ProcIds = pred_info_procids(PredInfo),
		list__foldl(
			check_aditi_state_modes(ModuleInfo,
				PredInfo, ArgTypes),
			ProcIds, !IO)
	).

	% If the procedure has declared modes, check that there
	% is an input `aditi__state' argument.
:- pred check_aditi_state_modes(module_info::in, pred_info::in, list(type)::in,
	proc_id::in, io::di, io::uo) is det.

check_aditi_state_modes(ModuleInfo, PredInfo, ArgTypes, ProcId, !IO) :-
	pred_info_procedures(PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	proc_info_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
	(
		MaybeArgModes = yes(ArgModes),
		AditiUi = aditi_mui_mode,
		mode_get_insts(ModuleInfo, AditiUi, AditiUiInitialInst, _),
		(
			check_aditi_state_modes_2(ModuleInfo, ArgTypes,
				ArgModes, AditiUiInitialInst)
		->
			true
		;
			proc_info_context(ProcInfo, Context),
			report_no_input_aditi_state(PredInfo, Context, !IO)
		)
	;
		% XXX Handling procedures for which modes are inferred
		% is a little tricky, because if the procedure doesn't
		% directly or indirectly call any base relations, a mode
		% of `unused' for the `aditi__state' argument may be inferred.
		% In the worst case, a runtime error will be reported
		% if the predicate is called outside of a transaction.
		MaybeArgModes = no
	).

:- pred check_aditi_state_modes_2(module_info::in, list(type)::in,
	list(mode)::in, (inst)::in) is semidet.

check_aditi_state_modes_2(ModuleInfo, [Type | Types], [Mode | Modes],
		InitialAditiStateInst) :-
	(
		type_is_aditi_state(Type),
		mode_get_insts(ModuleInfo, Mode, InitialInst, _),
		% Mode analysis will check the final inst.
		inst_matches_initial(InitialInst, InitialAditiStateInst,
			Type, ModuleInfo)
	;
		check_aditi_state_modes_2(ModuleInfo, Types, Modes,
			InitialAditiStateInst)
	).

:- pred report_no_aditi_state(pred_info::in, io::di, io::uo) is det.

report_no_aditi_state(PredInfo, !IO) :-
	io__set_exit_status(1, !IO),
	pred_info_context(PredInfo, Context),
	report_aditi_pragma(PredInfo, PredErrorPieces),
	list__append(PredErrorPieces,
		[words("without an `aditi__state' argument.")], ErrorPieces),
	error_util__write_error_pieces(Context, 0, ErrorPieces, !IO).

:- pred report_no_input_aditi_state(pred_info::in, prog_context::in,
	io::di, io::uo) is det.

report_no_input_aditi_state(PredInfo, Context, !IO) :-
	io__set_exit_status(1, !IO),
	report_aditi_pragma(PredInfo, PredErrorPieces),
	list__append(PredErrorPieces,
		[words(
		"without an `aditi__state' argument with mode `aditi_mui'.")],
		ErrorPieces),
	error_util__write_error_pieces(Context, 0, ErrorPieces, !IO).

:- pred report_aditi_pragma(pred_info::in, list(format_component)::out) is det.

report_aditi_pragma(PredInfo, ErrorPieces) :-
	Module = pred_info_module(PredInfo),
	Name = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	( check_marker(Markers, base_relation) ->
		Pragma = "base_relation"
	;
		Pragma = "aditi"
	),
	string__append_list(["`:- pragma ", Pragma, "'"], PragmaStr),
	CallId = PredOrFunc - qualified(Module, Name)/Arity,
	CallIdStr = hlds_out__simple_call_id_to_string(CallId),
	ErrorPieces = [fixed("Error:"), fixed(PragmaStr),
		words("declaration for"), fixed(CallIdStr)].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

post_typecheck__resolve_unify_functor(X0, ConsId0, ArgVars0, Mode0,
		Unification0, UnifyContext, GoalInfo0,
		ModuleInfo, !PredInfo, !VarTypes, !VarSet, Goal) :-

	map__lookup(!.VarTypes, X0, TypeOfX),
	list__length(ArgVars0, Arity),
	(
		%
		% Is the function symbol apply/N or ''/N,
		% representing a higher-order function call?
		% Or the impure/semipure equivalents impure_apply/N
		% and semipure_apply/N?
		% (XXX FIXME We should use nicer syntax for impure apply/N.)
		%
		ConsId0 = cons(unqualified(ApplyName), _),
		( ApplyName = "apply", Purity = (pure)
		; ApplyName = "", Purity = (pure)
		; ApplyName = "impure_apply", Purity = (impure)
		; ApplyName = "semipure_apply", Purity = (semipure)
		),
		Arity >= 1,
		ArgVars0 = [FuncVar | FuncArgVars]
	->
		%
		% Convert the higher-order function call (apply/N)
		% into a higher-order predicate call
		% (i.e., replace `X = apply(F, A, B, C)'
		% with `call(F, A, B, C, X)')
		%
		list__append(FuncArgVars, [X0], ArgVars),
		Modes = [],
		Det = erroneous,
		adjust_func_arity(function, Arity, FullArity),
		HOCall = generic_call(
			higher_order(FuncVar, Purity, function, FullArity),
			ArgVars, Modes, Det),
		Goal = HOCall - GoalInfo0
	;
		%
		% Is the function symbol a user-defined function, rather
		% than a functor which represents a data constructor?
		%

		% Find the set of candidate predicates which have the
		% specified name and arity (and module, if module-qualified)
		ConsId0 = cons(PredName, _),

		%
		% We don't do this for compiler-generated predicates;
		% they are assumed to have been generated with all
		% functions already expanded.
		% If we did this check for compiler-generated
		% predicates, it would cause the wrong behaviour
		% in the case where there is a user-defined function
		% whose type is exactly the same as the type of
		% a constructor.  (Normally that would cause
		% a type ambiguity error, but compiler-generated
		% predicates are not type-checked.)
		%
		\+ is_unify_or_compare_pred(!.PredInfo),

		%
		% We don't do this for the clause introduced by the
		% compiler for a field access function -- that needs
		% to be expanded into unifications below.
		%
		\+ pred_info_is_field_access_function(ModuleInfo, !.PredInfo),

		pred_info_get_markers(!.PredInfo, Markers),
		module_info_get_predicate_table(ModuleInfo, PredTable),
		predicate_table_search_func_sym_arity(PredTable,
			calls_are_fully_qualified(Markers),
			PredName, Arity, PredIds),

		% Check if any of the candidate functions have
		% argument/return types which subsume the actual
		% argument/return types of this function call

		pred_info_typevarset(!.PredInfo, TVarSet),
		map__apply_to_list(ArgVars0, !.VarTypes, ArgTypes0),
		list__append(ArgTypes0, [TypeOfX], ArgTypes),
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, PredId, QualifiedFuncName)
	->
		%
		% Convert function calls into predicate calls:
		% replace `X = f(A, B, C)'
		% with `f(A, B, C, X)'
		%
		ProcId = invalid_proc_id,
		list__append(ArgVars0, [X0], ArgVars),
		FuncCallUnifyContext = call_unify_context(X0,
			functor(ConsId0, no, ArgVars0), UnifyContext),
		FuncCall = call(PredId, ProcId, ArgVars, not_builtin,
			yes(FuncCallUnifyContext), QualifiedFuncName),
		Goal = FuncCall - GoalInfo0
	;
		%
		% Is the function symbol a higher-order predicate
		% or function constant?
		%
		ConsId0 = cons(Name, _),
		type_is_higher_order(TypeOfX, _Purity, PredOrFunc,
			EvalMethod, HOArgTypes),

		%
		% We don't do this for the clause introduced by the
		% compiler for a field access function -- that needs
		% to be expanded into unifications below.
		%
		\+ pred_info_is_field_access_function(ModuleInfo, !.PredInfo),

		%
		% Find the pred_id of the constant.
		%
		map__apply_to_list(ArgVars0, !.VarTypes, ArgTypes0),
		AllArgTypes = ArgTypes0 ++ HOArgTypes,
		pred_info_typevarset(!.PredInfo, TVarSet),
		pred_info_get_markers(!.PredInfo, Markers),
		get_pred_id(calls_are_fully_qualified(Markers), Name,
			PredOrFunc, TVarSet, AllArgTypes, ModuleInfo, PredId)
	->
		get_proc_id(ModuleInfo, PredId, ProcId),
		ShroudedPredProcId =
			shroud_pred_proc_id(proc(PredId, ProcId)),
		ConsId = pred_const(ShroudedPredProcId, EvalMethod),
		Goal = unify(X0, functor(ConsId, no, ArgVars0), Mode0,
			Unification0, UnifyContext) - GoalInfo0
	;
		%
		% Is it a call to an automatically generated field access
		% function. This test must come after the tests for
		% function calls and higher-order terms above.
		% It's done that way because it's easier to check
		% that the types match for functions calls and
		% higher-order terms.
		%
		ConsId0 = cons(Name, Arity),
		is_field_access_function_name(ModuleInfo, Name, Arity,
			AccessType, FieldName),

		%
		% We don't do this for compiler-generated predicates --
		% they will never contain calls to field access functions.
		%
		\+ is_unify_or_compare_pred(!.PredInfo),

		%
		% If there is a constructor for which the argument types
		% match, this unification couldn't be a call to a field
		% access function, otherwise there would have been an
		% error reported for unresolved overloading.
		%
		pred_info_typevarset(!.PredInfo, TVarSet),
		map__apply_to_list(ArgVars0, !.VarTypes, ArgTypes0),
		\+ find_matching_constructor(ModuleInfo, TVarSet,
			ConsId0, TypeOfX, ArgTypes0)
	->
		post_typecheck__finish_field_access_function(ModuleInfo,
			!PredInfo, !VarTypes, !VarSet, AccessType, FieldName,
			UnifyContext, X0, ArgVars0, GoalInfo0, Goal)
	;
		%
		% Module qualify ordinary construction/deconstruction
		% unifications.
		%
		(
			ConsId0 = cons(Name0, Arity),
			type_to_ctor_and_args(TypeOfX, TypeCtorOfX, _),
			TypeCtorOfX = qualified(TypeModule, _) - _
		->
			unqualify_name(Name0, Name),
			ConsId = cons(qualified(TypeModule, Name), Arity)
		;
			ConsId = ConsId0
		),
		Goal = unify(X0, functor(ConsId, no, ArgVars0), Mode0,
				Unification0, UnifyContext) - GoalInfo0
	).

%-----------------------------------------------------------------------------%

	% Succeed if there is a constructor which matches the given
	% cons_id, type and argument types.
:- pred find_matching_constructor(module_info::in, tvarset::in,
	cons_id::in, (type)::in, list(type)::in) is semidet.

find_matching_constructor(ModuleInfo, TVarSet, ConsId, Type, ArgTypes) :-
	type_to_ctor_and_args(Type, TypeCtor, _),
	module_info_ctors(ModuleInfo, ConsTable),
	map__search(ConsTable, ConsId, ConsDefns),
	list__member(ConsDefn, ConsDefns),

	% Overloading resolution ignores the class constraints.
	ConsDefn = hlds_cons_defn(ConsExistQVars, _,
			ConsArgs, ConsTypeCtor, _),
	ConsTypeCtor = TypeCtor,

	module_info_types(ModuleInfo, Types),
	map__search(Types, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_tvarset(TypeDefn, TypeTVarSet),

	assoc_list__values(ConsArgs, ConsArgTypes),
	arg_type_list_subsumes(TVarSet, ArgTypes,
		TypeTVarSet, ConsExistQVars, ConsArgTypes).

%-----------------------------------------------------------------------------%

	% Convert a field access function call into the equivalent unifications
	% so that later passes do not have to handle them as a special case.
	% The error messages from mode analysis and determinism analysis
	% shouldn't be too much worse than if the goals were special cases.
	%
:- pred post_typecheck__finish_field_access_function(module_info::in,
	pred_info::in, pred_info::out, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out,
	field_access_type::in, ctor_field_name::in,
	unify_context::in, prog_var::in, list(prog_var)::in,
	hlds_goal_info::in, hlds_goal::out) is det.

post_typecheck__finish_field_access_function(ModuleInfo, !PredInfo,
		!VarTypes, !VarSet, AccessType, FieldName, UnifyContext,
		Var, Args, GoalInfo, GoalExpr - GoalInfo) :-
	(
		AccessType = get,
		field_extraction_function_args(Args, TermVar),
		post_typecheck__translate_get_function(ModuleInfo,
			!PredInfo, !VarTypes, !VarSet, FieldName, UnifyContext,
			Var, TermVar, GoalInfo, GoalExpr)
	;
		AccessType = set,
		field_update_function_args(Args, TermInputVar, FieldVar),
		post_typecheck__translate_set_function(ModuleInfo,
			!PredInfo, !VarTypes, !VarSet, FieldName, UnifyContext,
			FieldVar, TermInputVar, Var,
			GoalInfo, GoalExpr)
	).

:- pred post_typecheck__translate_get_function(module_info::in,
	pred_info::in, pred_info::out, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, ctor_field_name::in,
	unify_context::in, prog_var::in, prog_var::in,
	hlds_goal_info::in, hlds_goal_expr::out) is det.

post_typecheck__translate_get_function(ModuleInfo, !PredInfo,
		!VarTypes, !VarSet, FieldName, UnifyContext,
		FieldVar, TermInputVar, OldGoalInfo, GoalExpr) :-
	map__lookup(!.VarTypes, TermInputVar, TermType),
	get_constructor_containing_field(ModuleInfo, TermType, FieldName,
		ConsId, FieldNumber),

	get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, ConsId,
		TermType, ArgTypes0, ExistQVars, !PredInfo),

	%
	% If the type of the field we are extracting contains existentially
	% quantified type variables then we need to rename any other
	% occurrences of those type variables in the arguments of the
	% constructor so that they match those in the type of the field.
	% (We don't need to do this for field updates because if any
	% existentially quantified type variables occur in field to set
	% and other fields then the field update should have been disallowed
	% by typecheck.m because the result can't be well-typed).
	%
	( ExistQVars \= [] ->
		map__lookup(!.VarTypes, FieldVar, FieldType),
		list__index1_det(ArgTypes0, FieldNumber, FieldArgType),
		(
			type_list_subsumes([FieldArgType], [FieldType],
				FieldSubst)
		->
			term__apply_rec_substitution_to_list(ArgTypes0,
				FieldSubst, ArgTypes)
		;
			error("post_typecheck__translate_get_function: " ++
				"type_list_subsumes failed")
		)
	;
		ArgTypes = ArgTypes0
	),

	split_list_at_index(FieldNumber, ArgTypes,
		TypesBeforeField, _, TypesAfterField),

	make_new_vars(TypesBeforeField, VarsBeforeField, !VarTypes, !VarSet),
	make_new_vars(TypesAfterField, VarsAfterField, !VarTypes, !VarSet),

	list__append(VarsBeforeField, [FieldVar | VarsAfterField], ArgVars),

	goal_info_get_nonlocals(OldGoalInfo, RestrictNonLocals),
	create_atomic_unification_with_nonlocals(TermInputVar,
		functor(ConsId, no, ArgVars), OldGoalInfo,
		RestrictNonLocals, [FieldVar, TermInputVar],
		UnifyContext, FunctorGoal),
	FunctorGoal = GoalExpr - _.

:- pred post_typecheck__translate_set_function(module_info::in,
	pred_info::in, pred_info::out, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, ctor_field_name::in,
	unify_context::in, prog_var::in, prog_var::in, prog_var::in,
	hlds_goal_info::in, hlds_goal_expr::out) is det.

post_typecheck__translate_set_function(ModuleInfo, !PredInfo,
		!VarTypes, !VarSet, FieldName, UnifyContext,
		FieldVar, TermInputVar, TermOutputVar, OldGoalInfo, Goal) :-
	map__lookup(!.VarTypes, TermInputVar, TermType),

	get_constructor_containing_field(ModuleInfo, TermType, FieldName,
		ConsId0, FieldNumber),

	get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, ConsId0,
		TermType, ArgTypes, ExistQVars, !PredInfo),

	split_list_at_index(FieldNumber, ArgTypes,
		TypesBeforeField, TermFieldType, TypesAfterField),

	make_new_vars(TypesBeforeField, VarsBeforeField, !VarTypes, !VarSet),
	make_new_var(TermFieldType, SingletonFieldVar, !VarTypes, !VarSet),
	make_new_vars(TypesAfterField, VarsAfterField, !VarTypes, !VarSet),

	%
	% Build a goal to deconstruct the input.
	%
	list__append(VarsBeforeField, [SingletonFieldVar | VarsAfterField],
		DeconstructArgs),
	goal_info_get_nonlocals(OldGoalInfo, OldNonLocals),
	list__append(VarsBeforeField, VarsAfterField, NonLocalArgs),
	set__insert_list(OldNonLocals, NonLocalArgs,
		DeconstructRestrictNonLocals),

	create_atomic_unification_with_nonlocals(TermInputVar,
		functor(ConsId0, no, DeconstructArgs), OldGoalInfo,
		DeconstructRestrictNonLocals, [TermInputVar | DeconstructArgs],
		UnifyContext, DeconstructGoal),

	%
	% Build a goal to construct the output.
	%
	list__append(VarsBeforeField, [FieldVar | VarsAfterField],
		ConstructArgs),
	set__insert_list(OldNonLocals, NonLocalArgs,
		ConstructRestrictNonLocals),

	% If the cons_id is existentially quantified, add a `new' prefix
	% so that polymorphism.m adds the appropriate type_infos.
	( ExistQVars = [] ->
		ConsId = ConsId0
	;
		( ConsId0 = cons(ConsName0, ConsArity) ->
			remove_new_prefix(ConsName, ConsName0),
			ConsId = cons(ConsName, ConsArity)
		;
			error("post_typecheck__translate_set_function: " ++
				"invalid cons_id")
		)
	),

	create_atomic_unification_with_nonlocals(TermOutputVar,
		functor(ConsId, no, ConstructArgs), OldGoalInfo,
		ConstructRestrictNonLocals, [TermOutputVar | ConstructArgs],
		UnifyContext, ConstructGoal),

	Conj = conj([DeconstructGoal, ConstructGoal]) - OldGoalInfo,

	% Make mode analysis treat the translated access function
	% as an atomic goal.
	Goal = some([], can_remove, Conj).

:- pred get_cons_id_arg_types_adding_existq_tvars(module_info::in, cons_id::in,
	(type)::in, list(type)::out, list(tvar)::out,
	pred_info::in, pred_info::out) is det.

get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, ConsId, TermType,
		ArgTypes, NewExistQVars, !PredInfo) :-
	%
	% Split the list of argument types at the named field.
	%
	type_util__get_type_and_cons_defn(ModuleInfo, TermType,
		ConsId, TypeDefn, ConsDefn),
	ConsDefn = hlds_cons_defn(ExistQVars, _, Args, _, _),
	assoc_list__values(Args, ArgTypes0),
	( ExistQVars = [] ->
		ArgTypes1 = ArgTypes0,
		NewExistQVars = []
	;
		%
		% Rename apart the existentially quantified type variables.
		%
		list__length(ExistQVars, NumExistQVars),
		pred_info_typevarset(!.PredInfo, TVarSet0),
		varset__new_vars(TVarSet0, NumExistQVars, NewExistQVars,
			TVarSet),
		pred_info_set_typevarset(TVarSet, !PredInfo),
		map__from_corresponding_lists(ExistQVars, NewExistQVars,
			TVarSubst),
		term__apply_variable_renaming_to_list(ArgTypes0, TVarSubst,
			ArgTypes1)
	),
	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
	term__term_list_to_var_list(TypeParams, TypeDefnArgs),
	( type_to_ctor_and_args(TermType, _, TypeArgs) ->
		map__from_corresponding_lists(TypeDefnArgs, TypeArgs, TSubst)
	;
		error("get_cons_id_arg_types_adding_existq_tvars: " ++
			"type_to_ctor_and_args failed")

	),
	term__apply_substitution_to_list(ArgTypes1, TSubst, ArgTypes).

:- pred split_list_at_index(int::in, list(T)::in, list(T)::out, T::out,
	list(T)::out) is det.

split_list_at_index(Index, List, Before, At, After) :-
	(
		list__split_list(Index - 1, List, Before0, AtAndAfter),
		AtAndAfter = [At0 | After0]
	->
		Before = Before0,
		At = At0,
		After = After0
	;
		error("post_typecheck__split_list_at_index")
	).

%-----------------------------------------------------------------------------%

	% Work out which constructor of the type has an argument with the
	% given field name.
:- pred get_constructor_containing_field(module_info::in, (type)::in,
	ctor_field_name::in, cons_id::out, int::out) is det.

get_constructor_containing_field(ModuleInfo, TermType, FieldName,
		ConsId, FieldNumber) :-
	( type_to_ctor_and_args(TermType, TermTypeCtor0, _) ->
		TermTypeCtor = TermTypeCtor0
	;
		error("get_constructor_containing_field: " ++
			"type_to_ctor_and_args failed")
	),
	module_info_types(ModuleInfo, Types),
	map__lookup(Types, TermTypeCtor, TermTypeDefn),
	hlds_data__get_type_defn_body(TermTypeDefn, TermTypeBody),
	( Ctors = TermTypeBody ^ du_type_ctors ->
		get_constructor_containing_field_2(Ctors, FieldName, ConsId,
			FieldNumber)
	;
		error("get_constructor_containing_field: not du type")
	).

:- pred get_constructor_containing_field_2(list(constructor)::in,
	ctor_field_name::in, cons_id::out, int::out) is det.

get_constructor_containing_field_2([], _, _, _) :-
	error("get_constructor_containing_field: can't find field").
get_constructor_containing_field_2([Ctor | Ctors], FieldName,
		ConsId, FieldNumber) :-
	Ctor = ctor(_, _, SymName, CtorArgs),
	(
		get_constructor_containing_field_3(CtorArgs,
			FieldName, 1, FieldNumber0)
	->
		list__length(CtorArgs, Arity),
		ConsId = cons(SymName, Arity),
		FieldNumber = FieldNumber0
	;
		get_constructor_containing_field_2(Ctors, FieldName,
			ConsId, FieldNumber)
	).

:- pred get_constructor_containing_field_3(list(constructor_arg)::in,
	ctor_field_name::in, int::in, int::out) is semidet.

get_constructor_containing_field_3([MaybeArgFieldName - _ | CtorArgs],
		FieldName, FieldNumber0, FieldNumber) :-
	(
		MaybeArgFieldName = yes(ArgFieldName),
		unqualify_name(ArgFieldName, UnqualFieldName),
		unqualify_name(FieldName, UnqualFieldName)
	->
		FieldNumber = FieldNumber0
	;
		get_constructor_containing_field_3(CtorArgs, FieldName,
			FieldNumber0 + 1, FieldNumber)
	).

%-----------------------------------------------------------------------------%

:- pred create_atomic_unification_with_nonlocals(prog_var::in, unify_rhs::in,
	hlds_goal_info::in, set(prog_var)::in, list(prog_var)::in,
	unify_context::in, hlds_goal::out) is det.

create_atomic_unification_with_nonlocals(Var, RHS, OldGoalInfo,
		RestrictNonLocals, VarsList, UnifyContext, Goal) :-
	goal_info_get_context(OldGoalInfo, Context),
	UnifyContext = unify_context(UnifyMainContext, UnifySubContext),
	create_atomic_unification(Var, RHS,
		Context, UnifyMainContext, UnifySubContext, Goal0),
	Goal0 = GoalExpr0 - GoalInfo0,

	% Compute the nonlocals of the goal.
	set__list_to_set(VarsList, NonLocals1),
	set__intersect(RestrictNonLocals, NonLocals1, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
	Goal = GoalExpr0 - GoalInfo.

:- pred make_new_vars(list(type)::in, list(prog_var)::out,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out) is det.

make_new_vars(Types, Vars, !VarTypes, !VarSet) :-
	list__length(Types, NumVars),
	varset__new_vars(!.VarSet, NumVars, Vars, !:VarSet),
	map__det_insert_from_corresponding_lists(!.VarTypes, Vars, Types,
		!:VarTypes).

:- pred make_new_var((type)::in, prog_var::out, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out) is det.

make_new_var(Type, Var, !VarTypes, !VarSet) :-
	varset__new_var(!.VarSet, Var, !:VarSet),
	map__det_insert(!.VarTypes, Var, Type, !:VarTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
