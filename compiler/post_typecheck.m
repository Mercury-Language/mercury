%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2001 The University of Melbourne.
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
%	- it checks for unbound type variables and if there are any,
%	  it reports an error (or a warning, binding them to the type `void').
%
% These actions cannot be done until after type inference is complete,
% so they need to be a separate "post-typecheck pass".  For efficiency
% reasons, this is in fact done at the same time as purity analysis --
% the routines here are called from purity.m rather than mercury_compile.m.
%
% This module also copies the clause_info structure
% to the proc_info structures. This is done in the post_typecheck pass
% and not at the start of modecheck because modecheck may be
% reinvoked after HLDS transformations. Any transformation that
% needs typechecking should work with the clause_info structure.
% Type information is also propagated into the modes of procedures
% by this pass if the ModeError parameter is no. 
% ModeError should be yes if any undefined modes	
% were found by previous passes.
%

:- module post_typecheck.
:- interface.
:- import_module hlds_data, hlds_goal, hlds_module, hlds_pred, prog_data.
:- import_module list, io, bool, std_util.

	% check_type_bindings(PredId, PredInfo, ModuleInfo, ReportErrors):
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
	%
:- pred post_typecheck__check_type_bindings(pred_id, pred_info, module_info,
		bool, pred_info, int, io__state, io__state).
:- mode post_typecheck__check_type_bindings(in, in, in, in, out, out, di, uo)
		is det.

	% Handle any unresolved overloading for a predicate call.
	%
:- pred post_typecheck__resolve_pred_overloading(pred_id, list(prog_var),
		pred_info, module_info, sym_name, sym_name, pred_id).
:- mode post_typecheck__resolve_pred_overloading(in, in, in, in, in,
		out, out) is det.

	% Resolve overloading and fill in the argument modes
	% of a call to an Aditi builtin.
	% Check that a relation modified by one of the Aditi update
	% goals is a base relation.
	%
:- pred post_typecheck__finish_aditi_builtin(module_info, pred_info,
		list(prog_var), term__context, aditi_builtin, aditi_builtin,
		simple_call_id, simple_call_id, list(mode),
		maybe(aditi_builtin_error)).
:- mode post_typecheck__finish_aditi_builtin(in, in, in, in,
		in, out, in, out, out, out) is det.

:- type aditi_builtin_error
	--->	aditi_update_of_derived_relation(prog_context,
			aditi_builtin, simple_call_id).

:- pred report_aditi_builtin_error(aditi_builtin_error, io__state, io__state).
:- mode report_aditi_builtin_error(in, di, uo) is det.

	% Work out whether a var-functor unification is actually a function
	% call. If so, replace the unification goal with a call.
	%
:- pred post_typecheck__resolve_unify_functor(prog_var, cons_id,
		list(prog_var), unify_mode, unification, unify_context,
		hlds_goal_info, module_info, pred_info, pred_info,
		vartypes, vartypes, prog_varset, prog_varset, hlds_goal).
:- mode post_typecheck__resolve_unify_functor(in, in, in, in, in, in,
		in, in, in, out, in, out, in, out, out) is det.

	% Do the stuff needed to initialize the pred_infos and proc_infos
	% so that a pred is ready for running polymorphism and then
	% mode checking.
	% Also check that all predicates with an `aditi' marker have
	% an `aditi__state' argument.
	%
:- pred post_typecheck__finish_pred(module_info, pred_id, pred_info, pred_info,
		io__state, io__state).
:- mode post_typecheck__finish_pred(in, in, in, out, di, uo) is det.

:- pred post_typecheck__finish_imported_pred(module_info, pred_id,
		pred_info, pred_info, io__state, io__state).
:- mode post_typecheck__finish_imported_pred(in, in, in, out, di, uo) is det.

	% As above, but don't check for `aditi__state's and return
	% the list of procedures containing unbound inst variables
	% instead of reporting the errors directly.
	%
:- pred post_typecheck__finish_pred_no_io(module_info, list(proc_id),
		pred_info, pred_info).
:- mode post_typecheck__finish_pred_no_io(in, out, in, out) is det.

:- pred post_typecheck__finish_imported_pred_no_io(module_info,
		list(proc_id), pred_info, pred_info).
:- mode post_typecheck__finish_imported_pred_no_io(in, out, in, out) is det.

:- pred post_typecheck__finish_ill_typed_pred(module_info, pred_id,
		pred_info, pred_info, io__state, io__state).
:- mode post_typecheck__finish_ill_typed_pred(in, in, in, out, di, uo) is det.

	% Now that the assertion has finished being typechecked,
	% remove it from further processing and store it in the
	% assertion_table.
:- pred post_typecheck__finish_assertion(module_info, pred_id,
		module_info, io__state, io__state) is det.
:- mode post_typecheck__finish_assertion(in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module (assertion), code_util, typecheck, clause_to_proc.
:- import_module mode_util, inst_match, (inst), prog_util, error_util.
:- import_module mercury_to_mercury, prog_out, hlds_out, type_util.
:- import_module globals, options.

:- import_module map, set, assoc_list, term, require, int.
:- import_module string, varset.

%-----------------------------------------------------------------------------%
%			Check for unbound type variables
%
%  Check that the all of the types which have been inferred
%  for the variables in the clause do not contain any unbound type
%  variables other than those that occur in the types of head
%  variables, and that there are no unsatisfied type class constraints.

post_typecheck__check_type_bindings(PredId, PredInfo0, ModuleInfo, ReportErrs,
		PredInfo, NumErrors, IOState0, IOState) :-
	(
		ReportErrs = yes,
		pred_info_get_unproven_body_constraints(PredInfo0,
			UnprovenConstraints0),
		UnprovenConstraints0 \= []
	->
		list__sort_and_remove_dups(UnprovenConstraints0,
			UnprovenConstraints),
		report_unsatisfied_constraints(UnprovenConstraints,
			PredId, PredInfo0, ModuleInfo, IOState0, IOState1),
		list__length(UnprovenConstraints, NumErrors)
	;
		NumErrors = 0,
		IOState1 = IOState0
	),
		
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	pred_info_get_head_type_params(PredInfo0, HeadTypeParams),
	clauses_info_varset(ClausesInfo0, VarSet),
	clauses_info_vartypes(ClausesInfo0, VarTypesMap0),
	map__to_assoc_list(VarTypesMap0, VarTypesList),
	set__init(Set0),
	check_type_bindings_2(VarTypesList, HeadTypeParams,
			[], Errs, Set0, Set),
	( Errs = [] ->
		PredInfo = PredInfo0,
		IOState2 = IOState1
	;
		( ReportErrs = yes ->
			%
			% report the warning
			%
			report_unresolved_type_warning(Errs, PredId, PredInfo0,
				ModuleInfo, VarSet, IOState1, IOState2)
		;
			IOState2 = IOState1
		),

		%
		% bind all the type variables in `Set' to `void' ...
		%
		pred_info_get_constraint_proofs(PredInfo0, Proofs0),
		bind_type_vars_to_void(Set, VarTypesMap0, VarTypesMap,
			Proofs0, Proofs),
		clauses_info_set_vartypes(ClausesInfo0, VarTypesMap,
			ClausesInfo),
		pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo1),
		pred_info_set_constraint_proofs(PredInfo1, Proofs, PredInfo)
	),

	%
	% check that main/2 has the right type
	%
	( ReportErrs = yes ->
		check_type_of_main(PredInfo, IOState2, IOState3)
	;
		IOState3 = IOState2
	),

	%
	% Check that all Aditi predicates have an `aditi__state' argument.
	% This must be done after typechecking because of type inference --
	% the types of some Aditi predicates may not be known before.
	%
	pred_info_get_markers(PredInfo, Markers),
	( ReportErrs = yes, check_marker(Markers, aditi) ->
		check_aditi_state(ModuleInfo, PredInfo, IOState3, IOState)
	;
		IOState = IOState3
	).

:- pred check_type_bindings_2(assoc_list(prog_var, (type)), list(tvar),
		assoc_list(prog_var, (type)), assoc_list(prog_var, (type)),
		set(tvar), set(tvar)).
:- mode check_type_bindings_2(in, in, in, out, in, out) is det.

check_type_bindings_2([], _, Errs, Errs, Set, Set).
check_type_bindings_2([Var - Type | VarTypes], HeadTypeParams,
			Errs0, Errs, Set0, Set) :-
	term__vars(Type, TVars),
	set__list_to_set(TVars, TVarsSet0),
	set__delete_list(TVarsSet0, HeadTypeParams, TVarsSet1),
	( \+ set__empty(TVarsSet1) ->
		Errs1 = [Var - Type | Errs0],
		set__union(Set0, TVarsSet1, Set1)
	;
		Errs1 = Errs0,
		Set0 = Set1
	),
	check_type_bindings_2(VarTypes, HeadTypeParams,
		Errs1, Errs, Set1, Set).

%
% bind all the type variables in `UnboundTypeVarsSet' to the type `void' ...
%
:- pred bind_type_vars_to_void(set(tvar),
				map(prog_var, type), map(prog_var, type),
				constraint_proof_map, constraint_proof_map).
:- mode bind_type_vars_to_void(in, in, out, in, out) is det.

bind_type_vars_to_void(UnboundTypeVarsSet,
		VarTypesMap0, VarTypesMap, Proofs0, Proofs) :-
	%
	% first create a pair of corresponding lists (UnboundTypeVars, Voids)
	% that map the unbound type variables to void
	%
	set__to_sorted_list(UnboundTypeVarsSet, UnboundTypeVars),
	list__length(UnboundTypeVars, Length),
	term__context_init(InitContext),
	Void = term__functor(term__atom("void"), [], InitContext),
	list__duplicate(Length, Void, Voids),

	%
	% then create a *substitution* that maps the 
	% unbound type variables to void.
	%
	map__from_corresponding_lists(UnboundTypeVars, Voids, 
		VoidSubst),

	%
	% then apply the substitutions we just created to the variable types
	% and constraint proofs
	%
	map__keys(VarTypesMap0, Vars),
	map__values(VarTypesMap0, Types0),
	term__substitute_corresponding_list(UnboundTypeVars, Voids,
		Types0, Types),
	map__from_corresponding_lists(Vars, Types, VarTypesMap),

	apply_subst_to_constraint_proofs(VoidSubst, Proofs0, Proofs).

%-----------------------------------------------------------------------------%
%
% report an error: unsatisfied type class constraints
%
:- pred report_unsatisfied_constraints(list(class_constraint),
		pred_id, pred_info, module_info, io__state, io__state).
:- mode report_unsatisfied_constraints(in, in, in, in, di, uo) is det.

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
:- pred report_unresolved_type_warning(assoc_list(prog_var, (type)), pred_id,
		pred_info, module_info, prog_varset, io__state, io__state).
:- mode report_unresolved_type_warning(in, in, in, in, in, di, uo) is det.

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

:- pred write_type_var_list(assoc_list(prog_var, (type)), prog_context,
			prog_varset, tvarset, io__state, io__state).
:- mode write_type_var_list(in, in, in, in, di, uo) is det.

write_type_var_list([], _, _, _) --> [].
write_type_var_list([Var - Type | Rest], Context, VarSet, TVarSet) -->
	prog_out__write_context(Context),
	io__write_string("      "),
	mercury_output_var(Var, VarSet, no),
	io__write_string(" :: "),
	mercury_output_term(Type, TVarSet, no),
	io__write_string("\n"),
	write_type_var_list(Rest, Context, VarSet, TVarSet).

%-----------------------------------------------------------------------------%
%			resolve predicate overloading

% In the case of a call to an overloaded predicate, typecheck.m
% does not figure out the correct pred_id.  We must do that here.

post_typecheck__resolve_pred_overloading(PredId0, Args0, CallerPredInfo,
		ModuleInfo, PredName0, PredName, PredId) :-
	( invalid_pred_id(PredId0) ->
		%
		% Find the set of candidate pred_ids for predicates which
		% have the specified name and arity
		% 
		pred_info_typevarset(CallerPredInfo, TVarSet),
		pred_info_clauses_info(CallerPredInfo, ClausesInfo),
		clauses_info_vartypes(ClausesInfo, VarTypes),
		map__apply_to_list(Args0, VarTypes, ArgTypes),
		typecheck__resolve_pred_overloading(ModuleInfo,
			ArgTypes, TVarSet, PredName0, PredName, PredId)
        ;
		PredId = PredId0,
		get_qualified_pred_name(ModuleInfo, PredId, PredName)
        ).

:- pred get_qualified_pred_name(module_info, pred_id, sym_name).
:- mode get_qualified_pred_name(in, in, out) is det.

get_qualified_pred_name(ModuleInfo, PredId, qualified(PredModule, PredName)) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, PredModule),
	pred_info_name(PredInfo, PredName).

%-----------------------------------------------------------------------------%

post_typecheck__finish_aditi_builtin(_, _, _, _, aditi_call(_, _, _, _),
               _, _, _, _, _) :-
	% These are only added by magic.m.
	error("post_typecheck__finish_aditi_builtin: aditi_call").

post_typecheck__finish_aditi_builtin(ModuleInfo, CallerPredInfo, Args, Context,
		aditi_tuple_insert_delete(InsertDelete, PredId0), Builtin,
		PredOrFunc - SymName0/Arity, InsertCallId,
		Modes, MaybeError) :-
	% make_hlds.m checks the arity, so this is guaranteed to succeed.
	get_state_args_det(Args, OtherArgs, _, _),

	% The tuple to insert has the same argument types as
	% the relation being inserted into.
	post_typecheck__resolve_pred_overloading(PredId0, OtherArgs,
		CallerPredInfo, ModuleInfo, SymName0, SymName, PredId),

	Builtin = aditi_tuple_insert_delete(InsertDelete, PredId),
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
	Builtin0 = aditi_insert_delete_modify(InsertDelMod, PredId0, Syntax),
	UnchangedArgTypes = (pred(X::in, X::out) is det),
	(
		InsertDelMod = bulk_insert,
		AdjustArgTypes = UnchangedArgTypes
	;
		InsertDelMod = delete(_),
		AdjustArgTypes = UnchangedArgTypes
	;
		InsertDelMod = modify(_),
		% The argument types of the closure passed to `aditi_modify'
		% contain two copies of the arguments of the base relation -
		% one set input and one set output.
		AdjustArgTypes =
		    (pred(Types0::in, Types::out) is det :-
			list__length(Types0, Length),
			HalfLength is Length // 2,
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
	Builtin = aditi_insert_delete_modify(InsertDelMod, PredId, Syntax),

	UpdateCallId = PredOrFunc - SymName/Arity,

	module_info_pred_info(ModuleInfo, PredId, RelationPredInfo),
	check_base_relation(Context, RelationPredInfo,
		Builtin, UpdateCallId, MaybeError),

	pred_info_arg_types(RelationPredInfo, ArgTypes),
	post_typecheck__insert_delete_modify_closure_info(InsertDelMod,
		PredOrFunc, ArgTypes, ClosurePredOrFunc,
		ClosureArgModes, ClosureDetism),

	Inst = ground(shared, higher_order(pred_inst_info(ClosurePredOrFunc,
		ClosureArgModes, ClosureDetism))),
	Modes = [(Inst -> Inst), aditi_di_mode, aditi_uo_mode].

:- pred post_typecheck__insert_delete_modify_closure_info(
		aditi_insert_delete_modify, pred_or_func, list(type),
		pred_or_func, list(mode), determinism).
:- mode post_typecheck__insert_delete_modify_closure_info(in, in, in,
		out, out, out) is det.

post_typecheck__insert_delete_modify_closure_info(bulk_insert, PredOrFunc,
		ArgTypes, PredOrFunc, ClosureArgModes, nondet) :-
	out_mode(OutMode),
	AditiStateMode = aditi_mui_mode,
	aditi_builtin_modes(OutMode, AditiStateMode,
		ArgTypes, ClosureArgModes).
post_typecheck__insert_delete_modify_closure_info(delete(BulkOrFilter),
		PredOrFunc, ArgTypes, PredOrFunc, ClosureArgModes, nondet) :-
	(
		BulkOrFilter = bulk,
		out_mode(ArgMode)
	;
		BulkOrFilter = filter,
		in_mode(ArgMode)
	),
	AditiStateMode = aditi_mui_mode,
	aditi_builtin_modes(ArgMode, AditiStateMode,
		ArgTypes, ClosureArgModes).
post_typecheck__insert_delete_modify_closure_info(modify(BulkOrFilter),
		_PredOrFunc, ArgTypes, LambdaPredOrFunc,
		ClosureArgModes, nondet) :-
	LambdaPredOrFunc = predicate,
	out_mode(OutMode),
	in_mode(InMode),
	unused_mode(UnusedMode),
	(
		BulkOrFilter = bulk,
		DeleteArgMode = OutMode,
		DeleteAditiStateMode = aditi_mui_mode
	;
		BulkOrFilter = filter,
		DeleteArgMode = InMode,
		DeleteAditiStateMode = UnusedMode
	),

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
:- pred resolve_aditi_builtin_overloading(module_info, pred_info,
		list(prog_var), pred(list(type), list(type)),
		pred_id, pred_id, sym_name, sym_name).
:- mode resolve_aditi_builtin_overloading(in, in, in, pred(in, out) is det,
		in, out, in, out) is det.

resolve_aditi_builtin_overloading(ModuleInfo, CallerPredInfo, Args,
		AdjustArgTypes, PredId0, PredId, SymName0, SymName) :-
	% make_hlds.m checks the arity, so this is guaranteed to succeed.
	get_state_args_det(Args, OtherArgs, _, _),
	( invalid_pred_id(PredId0) ->
		(
			OtherArgs = [HOArg],
			pred_info_typevarset(CallerPredInfo, TVarSet),
			pred_info_clauses_info(CallerPredInfo, ClausesInfo),
			clauses_info_vartypes(ClausesInfo, VarTypes),
			map__lookup(VarTypes, HOArg, HOArgType),
			type_is_higher_order(HOArgType,
				_, EvalMethod, ArgTypes0),
			EvalMethod \= normal
		->
			call(AdjustArgTypes, ArgTypes0, ArgTypes),
			typecheck__resolve_pred_overloading(ModuleInfo,
				ArgTypes, TVarSet, SymName0, SymName, PredId)
		;
			error(
			"post_typecheck__resolve_aditi_builtin_overloading")
		)
	;
		PredId = PredId0,
		get_qualified_pred_name(ModuleInfo, PredId, SymName)
	).

	% Work out the modes of the arguments of a closure passed
	% to an Aditi update.
	% The `Mode' passed is the mode of all arguments apart
	% from the `aditi__state'.
:- pred aditi_builtin_modes((mode), (mode), list(type), list(mode)).
:- mode aditi_builtin_modes(in, in, in, out) is det.

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
:- pred check_base_relation(prog_context, pred_info, aditi_builtin,
	simple_call_id, maybe(aditi_builtin_error)).
:- mode check_base_relation(in, in, in, in, out) is det.

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
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" is not a base relation.\n").

%-----------------------------------------------------------------------------%

	% 
	% Ensure that all constructors occurring in predicate mode 
	% declarations are module qualified.
	% 
post_typecheck__finish_pred(ModuleInfo, PredId, PredInfo0, PredInfo) -->
	{ post_typecheck__finish_pred_no_io(ModuleInfo,
			ErrorProcs, PredInfo0, PredInfo1) },
	report_unbound_inst_vars(ModuleInfo, PredId,
			ErrorProcs, PredInfo1, PredInfo).

post_typecheck__finish_pred_no_io(ModuleInfo, ErrorProcs,
		PredInfo0, PredInfo) :-
	post_typecheck__propagate_types_into_modes(ModuleInfo,
			ErrorProcs, PredInfo0, PredInfo).

	%
	% For ill-typed preds, we just need to set the modes up correctly
	% so that any calls to that pred from correctly-typed predicates
	% won't result in spurious mode errors.
	%
post_typecheck__finish_ill_typed_pred(ModuleInfo, PredId,
			PredInfo0, PredInfo) -->
	{ post_typecheck__propagate_types_into_modes(ModuleInfo,
			ErrorProcs, PredInfo0, PredInfo1) },
	report_unbound_inst_vars(ModuleInfo, PredId,
			ErrorProcs, PredInfo1, PredInfo).

	% 
	% For imported preds, we just need to ensure that all
	% constructors occurring in predicate mode declarations are
	% module qualified.
	% 
post_typecheck__finish_imported_pred(ModuleInfo, PredId,
		PredInfo0, PredInfo) -->
	{ pred_info_get_markers(PredInfo0, Markers) },
	(
		{ check_marker(Markers, base_relation) },
		{ pred_info_module(PredInfo0, ModuleName) },
		{ module_info_name(ModuleInfo, ModuleName) }
	->
		check_aditi_state(ModuleInfo, PredInfo0)
	;
		[]
	),
	{ post_typecheck__finish_imported_pred_no_io(ModuleInfo, ErrorProcs,
		PredInfo0, PredInfo1) },
	report_unbound_inst_vars(ModuleInfo, PredId,
		ErrorProcs, PredInfo1, PredInfo).

post_typecheck__finish_imported_pred_no_io(ModuleInfo, Errors,
		PredInfo0, PredInfo) :-
	% Make sure the var-types field in the clauses_info is
	% valid for imported predicates.
	% Unification procedures have clauses generated, so
	% they already have valid var-types.
	( pred_info_is_pseudo_imported(PredInfo0) ->
		PredInfo1 = PredInfo0
	;
		pred_info_clauses_info(PredInfo0, ClausesInfo0),
		clauses_info_headvars(ClausesInfo0, HeadVars),
		pred_info_arg_types(PredInfo0, ArgTypes),
		map__from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
		clauses_info_set_vartypes(ClausesInfo0, VarTypes, ClausesInfo),
		pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo1)
	),
	post_typecheck__propagate_types_into_modes(ModuleInfo,
		Errors, PredInfo1, PredInfo).

	%
	% Now that the assertion has finished being typechecked,
	% and has had all of its pred_ids identified,
	% remove the assertion from the list of pred ids to be processed
	% in the future and place the pred_id associated with the
	% assertion into the assertion table.
	% For each assertion that is in the interface, you need to check
	% that it doesn't refer to any symbols which are local to that
	% module.
	% Also record for each predicate that is used in an assertion
	% which assertion it is used in.
	% 
post_typecheck__finish_assertion(Module0, PredId, Module) -->
		% store into assertion table.
	{ module_info_assertion_table(Module0, AssertTable0) },
	{ assertion_table_add_assertion(PredId,
			AssertTable0, AssertionId, AssertTable) },
	{ module_info_set_assertion_table(Module0, AssertTable, Module1) },
		
		% Remove from further processing.
	{ module_info_remove_predid(Module1, PredId, Module2) },

		% If the assertion is in the interface, then ensure that
		% it doesn't refer to any local symbols.
	{ module_info_pred_info(Module2, PredId, PredInfo) },
	{ assertion__goal(AssertionId, Module2, Goal) },
	(
		{ pred_info_is_exported(PredInfo) }
	->
		assertion__in_interface_check(Goal, PredInfo, Module2, Module3)
	;
		{ Module3 = Module2 }
	),

		% record which predicates are used in assertions
	{ assertion__record_preds_used_in(Goal, AssertionId, Module3, Module) }.
	
%-----------------------------------------------------------------------------%

:- pred check_type_of_main(pred_info, io__state, io__state).
:- mode check_type_of_main(in, di, uo) is det.

check_type_of_main(PredInfo) -->
	( 
		%
		% Check if this predicate is the
		% program entry point main/2.
		%
		{ pred_info_name(PredInfo, "main") },
		{ pred_info_arity(PredInfo, 2) },
		{ pred_info_is_exported(PredInfo) }
	->
		%
		% Check that the arguments of main/2
		% have type `io__state'.
		%
		{ pred_info_arg_types(PredInfo, ArgTypes) },
		(
			{ ArgTypes = [Arg1, Arg2] },
			{ type_is_io_state(Arg1) },
			{ type_is_io_state(Arg2) }
		->
			[]
		;
			{ pred_info_context(PredInfo, Context) },
			error_util__write_error_pieces(Context, 0,
				[words("Error: arguments of main/2"),
				words("must have type `io__state'.")]),
			io__set_exit_status(1)
		)
	;
		[]
	).
	
%-----------------------------------------------------------------------------%

	% 
	% Ensure that all constructors occurring in predicate mode
	% declarations are module qualified.
	% 
:- pred post_typecheck__propagate_types_into_modes(module_info,
		list(proc_id), pred_info, pred_info).
:- mode post_typecheck__propagate_types_into_modes(in, out, in, out)
		is det.
post_typecheck__propagate_types_into_modes(ModuleInfo, ErrorProcs,
		PredInfo0, PredInfo) :-
	pred_info_arg_types(PredInfo0, ArgTypes),
	pred_info_procedures(PredInfo0, Procs0),
	pred_info_procids(PredInfo0, ProcIds),
	propagate_types_into_proc_modes(ModuleInfo, ProcIds, ArgTypes,
			[], ErrorProcs, Procs0, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

%-----------------------------------------------------------------------------%

:- pred propagate_types_into_proc_modes(module_info, list(proc_id),
	list(type), list(proc_id), list(proc_id), proc_table, proc_table).
:- mode propagate_types_into_proc_modes(in, in, in, in, out, in, out) is det.		
propagate_types_into_proc_modes(_, [], _,
		ErrorProcs, list__reverse(ErrorProcs), Procs, Procs).
propagate_types_into_proc_modes(ModuleInfo, [ProcId | ProcIds],
		ArgTypes, ErrorProcs0, ErrorProcs, Procs0, Procs) :-
	map__lookup(Procs0, ProcId, ProcInfo0),
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
		ErrorProcs1 = [ProcId | ErrorProcs0],
		Procs1 = Procs0
	;
		ErrorProcs1 = ErrorProcs0,
		proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo),
		map__det_update(Procs0, ProcId, ProcInfo, Procs1)
	),
	propagate_types_into_proc_modes(ModuleInfo, ProcIds,
		ArgTypes, ErrorProcs1, ErrorProcs, Procs1, Procs).

:- pred report_unbound_inst_vars(module_info, pred_id, list(proc_id),
		pred_info, pred_info, io__state, io__state).
:- mode report_unbound_inst_vars(in, in, in, in, out, di, uo) is det.

report_unbound_inst_vars(ModuleInfo, PredId, ErrorProcs,
		PredInfo0, PredInfo) -->
	( { ErrorProcs = [] } ->
	    { PredInfo = PredInfo0 }
	;
	    { pred_info_procedures(PredInfo0, ProcTable0) },
	    list__foldl2(
		(pred(ProcId::in, Procs0::in, Procs::out, di, uo) is det -->
		    { map__lookup(Procs0, ProcId, ProcInfo) },
		    unbound_inst_var_error(PredId, ProcInfo, ModuleInfo),
		    % delete this mode, to avoid internal errors
		    { map__det_remove(Procs0, ProcId, _, Procs) }
		), ErrorProcs, ProcTable0, ProcTable),
	    { pred_info_set_procedures(PredInfo0, ProcTable, PredInfo) }
	).

:- pred unbound_inst_var_error(pred_id, proc_info, module_info,
				io__state, io__state).
:- mode unbound_inst_var_error(in, in, in, di, uo) is det.

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

:- pred check_aditi_state(module_info, pred_info, io__state, io__state).
:- mode check_aditi_state(in, in, di, uo) is det.

check_aditi_state(ModuleInfo, PredInfo) -->
	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ list__filter(type_is_aditi_state, ArgTypes, AditiStateTypes) },
	( { AditiStateTypes = [] } ->
		report_no_aditi_state(PredInfo)
	;
		{ pred_info_procids(PredInfo, ProcIds) },
		list__foldl(
			check_aditi_state_modes(ModuleInfo,
				PredInfo, ArgTypes),
			ProcIds)
	).

	% If the procedure has declared modes, check that there
	% is an input `aditi__state' argument.
:- pred check_aditi_state_modes(module_info, pred_info, list(type),
		proc_id, io__state, io__state).
:- mode check_aditi_state_modes(in, in, in, in, di, uo) is det.

check_aditi_state_modes(ModuleInfo, PredInfo, ArgTypes, ProcId) -->
	{ pred_info_procedures(PredInfo, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_maybe_declared_argmodes(ProcInfo, MaybeArgModes) },
	(
		{ MaybeArgModes = yes(ArgModes) },
		{ AditiUi = aditi_mui_mode },
		{ mode_get_insts(ModuleInfo, AditiUi, AditiUiInitialInst, _) },
		(
			{ check_aditi_state_modes_2(ModuleInfo, ArgTypes,
				ArgModes, AditiUiInitialInst) }
		->
			[]
		;
			{ proc_info_context(ProcInfo, Context) },
			report_no_input_aditi_state(PredInfo, Context)
		)
	;
		% XXX Handling procedures for which modes are inferred
		% is a little tricky, because if the procedure doesn't
		% directly or indirectly call any base relations, a mode
		% of `unused' for the `aditi__state' argument may be inferred.
		% In the worst case, a runtime error will be reported
		% if the predicate is called outside of a transaction.
		{ MaybeArgModes = no }
	).

:- pred check_aditi_state_modes_2(module_info, list(type), list(mode), (inst)).
:- mode check_aditi_state_modes_2(in, in, in, in) is semidet.

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

:- pred report_no_aditi_state(pred_info, io__state, io__state).
:- mode report_no_aditi_state(in, di, uo) is det.

report_no_aditi_state(PredInfo) -->
	io__set_exit_status(1),
	{ pred_info_context(PredInfo, Context) },
	{ report_aditi_pragma(PredInfo, PredErrorPieces) },
	{ list__append(PredErrorPieces,
		[words("without an `aditi__state' argument.")], ErrorPieces) },
	error_util__write_error_pieces(Context, 0, ErrorPieces).

:- pred report_no_input_aditi_state(pred_info, prog_context,
		io__state, io__state).
:- mode report_no_input_aditi_state(in, in, di, uo) is det.

report_no_input_aditi_state(PredInfo, Context) -->
	io__set_exit_status(1),
	{ report_aditi_pragma(PredInfo, PredErrorPieces) },
	{ list__append(PredErrorPieces,
		[words(
		"without an `aditi__state' argument with mode `aditi_mui'.")],
		ErrorPieces) },
	error_util__write_error_pieces(Context, 0, ErrorPieces).

:- pred report_aditi_pragma(pred_info, list(format_component)).
:- mode report_aditi_pragma(in, out) is det.

report_aditi_pragma(PredInfo, ErrorPieces) :-
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_get_markers(PredInfo, Markers),
	( check_marker(Markers, base_relation) ->
		Pragma = "base_relation"
	;
		Pragma = "aditi"
	),
	string__append_list(["`:- pragma ", Pragma, "'"], PragmaStr),
	CallId = PredOrFunc - qualified(Module, Name)/Arity,
	hlds_out__simple_call_id_to_string(CallId, CallIdStr),
	ErrorPieces = [fixed("Error:"), fixed(PragmaStr),
			words("declaration for"), fixed(CallIdStr)].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

post_typecheck__resolve_unify_functor(X0, ConsId0, ArgVars0, Mode0,
		Unification0, UnifyContext, GoalInfo0,
		ModuleInfo, PredInfo0, PredInfo, VarTypes0, VarTypes,
		VarSet0, VarSet, Goal) :-

	map__lookup(VarTypes0, X0, TypeOfX),
	list__length(ArgVars0, Arity),
	(
		%
		% Is the function symbol apply/N or ''/N,
		% representing a higher-order function call?
		%
		ConsId0 = cons(unqualified(ApplyName), _),
		( ApplyName = "apply" ; ApplyName = "" ),
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
			higher_order(FuncVar, function, FullArity),
			ArgVars, Modes, Det),

		PredInfo = PredInfo0,
		VarTypes = VarTypes0,
		VarSet = VarSet0,
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
		\+ code_util__compiler_generated(PredInfo0),

		%
		% We don't do this for the clause introduced by the
		% compiler for a field access function -- that needs
		% to be expanded into unifications below.
		%
		\+ pred_info_is_field_access_function(ModuleInfo, PredInfo0),

		module_info_get_predicate_table(ModuleInfo, PredTable),
		predicate_table_search_func_sym_arity(PredTable,
			PredName, Arity, PredIds),

		% Check if any of the candidate functions have
		% argument/return types which subsume the actual
		% argument/return types of this function call

		pred_info_typevarset(PredInfo0, TVarSet),
		map__apply_to_list(ArgVars0, VarTypes0, ArgTypes0),
		list__append(ArgTypes0, [TypeOfX], ArgTypes),
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, PredId, QualifiedFuncName)
	->
		%
		% Convert function calls into predicate calls:
		% replace `X = f(A, B, C)'
		% with `f(A, B, C, X)'
		%
		invalid_proc_id(ProcId),
		list__append(ArgVars0, [X0], ArgVars),
		FuncCallUnifyContext = call_unify_context(X0,
			functor(ConsId0, ArgVars0), UnifyContext),
		FuncCall = call(PredId, ProcId, ArgVars, not_builtin,
			yes(FuncCallUnifyContext), QualifiedFuncName),

		PredInfo = PredInfo0,
		VarTypes = VarTypes0,
		VarSet = VarSet0,
		Goal = FuncCall - GoalInfo0
	;
		%
		% Is the function symbol a higher-order predicate
		% or function constant?
		%
		ConsId0 = cons(Name, _),
		type_is_higher_order(TypeOfX, PredOrFunc,
			EvalMethod, HOArgTypes),

		%
		% We don't do this for the clause introduced by the
		% compiler for a field access function -- that needs
		% to be expanded into unifications below.
		%
		\+ pred_info_is_field_access_function(ModuleInfo, PredInfo0),

		%
		% Find the pred_id of the constant.
		%
		map__apply_to_list(ArgVars0, VarTypes0, ArgTypes0),
		AllArgTypes = ArgTypes0 ++ HOArgTypes,
		pred_info_typevarset(PredInfo0, TVarSet),
		get_pred_id(Name, PredOrFunc, TVarSet, AllArgTypes,
			ModuleInfo, PredId)
	->
		get_proc_id(ModuleInfo, PredId, ProcId),
		ConsId = pred_const(PredId, ProcId, EvalMethod),
		Goal = unify(X0, functor(ConsId, ArgVars0), Mode0,
			Unification0, UnifyContext) - GoalInfo0,
		PredInfo = PredInfo0,
		VarTypes = VarTypes0,
		VarSet = VarSet0
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
		\+ code_util__compiler_generated(PredInfo0),

		%
		% If there is a constructor for which the argument types
		% match, this unification couldn't be a call to a field
		% access function, otherwise there would have been an
		% error reported for unresolved overloading. 
		%
		pred_info_typevarset(PredInfo0, TVarSet),
		map__apply_to_list(ArgVars0, VarTypes0, ArgTypes0),
		\+ find_matching_constructor(ModuleInfo, TVarSet,
			ConsId0, TypeOfX, ArgTypes0)
	->
		post_typecheck__finish_field_access_function(ModuleInfo,
			PredInfo0, PredInfo, VarTypes0, VarTypes,
			VarSet0, VarSet, AccessType, FieldName,
			UnifyContext, X0, ArgVars0, GoalInfo0, Goal)
	;
		%
		% Module qualify ordinary construction/deconstruction
		% unifications.
		%
		PredInfo = PredInfo0,
		VarTypes = VarTypes0,
		VarSet = VarSet0,
		(
			ConsId0 = cons(Name0, Arity),
			type_to_type_id(TypeOfX, TypeIdOfX, _),
			TypeIdOfX = qualified(TypeModule, _) - _
		->
			unqualify_name(Name0, Name),
			ConsId = cons(qualified(TypeModule, Name), Arity)	
		;
			ConsId = ConsId0
		),
		Goal = unify(X0, functor(ConsId, ArgVars0), Mode0,
				Unification0, UnifyContext) - GoalInfo0
	).

%-----------------------------------------------------------------------------%

	% Succeed if there is a constructor which matches the given
	% cons_id, type and argument types.
:- pred find_matching_constructor(module_info, tvarset,
		cons_id, type, list(type)).
:- mode find_matching_constructor(in, in, in, in, in) is semidet.

find_matching_constructor(ModuleInfo, TVarSet, ConsId, Type, ArgTypes) :-
	type_to_type_id(Type, TypeId, _),
	module_info_ctors(ModuleInfo, ConsTable),
	map__search(ConsTable, ConsId, ConsDefns),
	list__member(ConsDefn, ConsDefns),

	% Overloading resolution ignores the class constraints.
	ConsDefn = hlds_cons_defn(ConsExistQVars, _,
			ConsArgs, ConsTypeId, _),
	ConsTypeId = TypeId,

	module_info_types(ModuleInfo, Types),
	map__search(Types, TypeId, TypeDefn),
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
:- pred post_typecheck__finish_field_access_function(module_info, pred_info,
		pred_info, vartypes, vartypes, prog_varset, prog_varset,
		field_access_type, ctor_field_name,
		unify_context, prog_var, list(prog_var),
		hlds_goal_info, hlds_goal).
:- mode post_typecheck__finish_field_access_function(in, in, out, in, out,
		in, out, in, in, in, in, in, in, out) is det.

post_typecheck__finish_field_access_function(ModuleInfo, PredInfo0, PredInfo,
		VarTypes0, VarTypes, VarSet0, VarSet, AccessType, FieldName,
		UnifyContext, Var, Args, GoalInfo, GoalExpr - GoalInfo) :-
	(
		AccessType = get,
		field_extraction_function_args(Args, TermVar),
		post_typecheck__translate_get_function(ModuleInfo,
			PredInfo0, PredInfo, VarTypes0, VarTypes,
			VarSet0, VarSet, FieldName, UnifyContext,
			Var, TermVar, GoalInfo, GoalExpr)
	;
		AccessType = set,
		field_update_function_args(Args, TermInputVar, FieldVar),
		post_typecheck__translate_set_function(ModuleInfo,
			PredInfo0, PredInfo, VarTypes0, VarTypes,
			VarSet0, VarSet, FieldName, UnifyContext,
			FieldVar, TermInputVar, Var,
			GoalInfo, GoalExpr)
	).

:- pred post_typecheck__translate_get_function(module_info,
		pred_info, pred_info, vartypes, vartypes,
		prog_varset, prog_varset, ctor_field_name,
		unify_context, prog_var, prog_var,
		hlds_goal_info, hlds_goal_expr).
:- mode post_typecheck__translate_get_function(in, in, out, in, out, in, out,
		in, in, in, in, in, out) is det.

post_typecheck__translate_get_function(ModuleInfo, PredInfo0, PredInfo,
		VarTypes0, VarTypes, VarSet0, VarSet, FieldName, UnifyContext,
		FieldVar, TermInputVar, OldGoalInfo, GoalExpr) :-
	map__lookup(VarTypes0, TermInputVar, TermType),
	get_constructor_containing_field(ModuleInfo, TermType, FieldName,
		ConsId, FieldNumber),

	get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, ConsId,
		TermType, ArgTypes0, ExistQVars, PredInfo0, PredInfo),

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
		map__lookup(VarTypes0, FieldVar, FieldType),
		list__index1_det(ArgTypes0, FieldNumber, FieldArgType),
		(
			type_list_subsumes([FieldArgType], [FieldType],
				FieldSubst)
		->
			term__apply_rec_substitution_to_list(ArgTypes0,
				FieldSubst, ArgTypes)
		;
			error(
	"post_typecheck__translate_get_function: type_list_subsumes failed")
		)
	;
		ArgTypes = ArgTypes0
	),

	split_list_at_index(FieldNumber, ArgTypes,
		TypesBeforeField, _, TypesAfterField),

	make_new_vars(TypesBeforeField, VarsBeforeField,
		VarTypes0, VarTypes1, VarSet0, VarSet1),
	make_new_vars(TypesAfterField, VarsAfterField,
		VarTypes1, VarTypes, VarSet1, VarSet),

	list__append(VarsBeforeField, [FieldVar | VarsAfterField], ArgVars),

	goal_info_get_nonlocals(OldGoalInfo, RestrictNonLocals),
	create_atomic_unification_with_nonlocals(TermInputVar,
		functor(ConsId, ArgVars), OldGoalInfo,
		RestrictNonLocals, [FieldVar, TermInputVar],
		UnifyContext, FunctorGoal),
	FunctorGoal = GoalExpr - _.

:- pred post_typecheck__translate_set_function(module_info,
		pred_info, pred_info, vartypes, vartypes,
		prog_varset, prog_varset, ctor_field_name, unify_context,
		prog_var, prog_var, prog_var, hlds_goal_info, hlds_goal_expr).
:- mode post_typecheck__translate_set_function(in, in, out, in, out, in, out,
		in, in, in, in, in, in, out) is det.

post_typecheck__translate_set_function(ModuleInfo, PredInfo0, PredInfo,
		VarTypes0, VarTypes, VarSet0, VarSet, FieldName, UnifyContext,
		FieldVar, TermInputVar, TermOutputVar, OldGoalInfo, Goal) :-
	map__lookup(VarTypes0, TermInputVar, TermType),

	get_constructor_containing_field(ModuleInfo, TermType, FieldName,
		ConsId0, FieldNumber),

	get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, ConsId0,
		TermType, ArgTypes, ExistQVars, PredInfo0, PredInfo),

	split_list_at_index(FieldNumber, ArgTypes,
		TypesBeforeField, TermFieldType, TypesAfterField),

	make_new_vars(TypesBeforeField, VarsBeforeField, VarTypes0, VarTypes1,
		VarSet0, VarSet1),
	make_new_var(TermFieldType, SingletonFieldVar, VarTypes1, VarTypes2,
		VarSet1, VarSet2),
	make_new_vars(TypesAfterField, VarsAfterField, VarTypes2, VarTypes,
		VarSet2, VarSet),

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
		functor(ConsId0, DeconstructArgs), OldGoalInfo,
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
			error(
		"post_typecheck__translate_set_function: invalid cons_id")
		)
	),

	create_atomic_unification_with_nonlocals(TermOutputVar,
		functor(ConsId, ConstructArgs), OldGoalInfo,
		ConstructRestrictNonLocals, [TermOutputVar | ConstructArgs],
		UnifyContext, ConstructGoal),
	
	Conj = conj([DeconstructGoal, ConstructGoal]) - OldGoalInfo,

	% Make mode analysis treat the translated access function
	% as an atomic goal.
	Goal = some([], can_remove, Conj).

:- pred get_cons_id_arg_types_adding_existq_tvars(module_info, cons_id,
		(type), list(type), list(tvar), pred_info, pred_info).
:- mode get_cons_id_arg_types_adding_existq_tvars(in, in, in,
		out, out, in, out) is det.

get_cons_id_arg_types_adding_existq_tvars(ModuleInfo, ConsId, TermType,
		ArgTypes, NewExistQVars, PredInfo0, PredInfo) :-
	%
	% Split the list of argument types at the named field. 
	%
	type_util__get_type_and_cons_defn(ModuleInfo, TermType,
		ConsId, TypeDefn, ConsDefn),
	ConsDefn = hlds_cons_defn(ExistQVars, _, Args, _, _),
	assoc_list__values(Args, ArgTypes0),
	( ExistQVars = [] ->
		ArgTypes1 = ArgTypes0,
		PredInfo = PredInfo0,
		NewExistQVars = []
	;
		%
		% Rename apart the existentially quantified type variables.
		%
		list__length(ExistQVars, NumExistQVars),
		pred_info_typevarset(PredInfo0, TVarSet0),
		varset__new_vars(TVarSet0, NumExistQVars, NewExistQVars,
			TVarSet),
		pred_info_set_typevarset(PredInfo0, TVarSet, PredInfo),
		map__from_corresponding_lists(ExistQVars, NewExistQVars,
			TVarSubst),
		term__apply_variable_renaming_to_list(ArgTypes0, TVarSubst,
			ArgTypes1)
	),
	hlds_data__get_type_defn_tparams(TypeDefn, TypeParams),
	term__term_list_to_var_list(TypeParams, TypeDefnArgs),
	( type_to_type_id(TermType, _, TypeArgs) ->
		map__from_corresponding_lists(TypeDefnArgs, TypeArgs, TSubst)
	;
		error(
	"get_cons_id_arg_types_adding_existq_tvars: type_to_type_id failed")

	),
	term__apply_substitution_to_list(ArgTypes1, TSubst, ArgTypes).

:- pred split_list_at_index(int, list(T), list(T), T, list(T)).
:- mode split_list_at_index(in, in, out, out, out) is det.

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
:- pred get_constructor_containing_field(module_info, (type), ctor_field_name,
		cons_id, int).
:- mode get_constructor_containing_field(in, in, in, out, out) is det.

get_constructor_containing_field(ModuleInfo, TermType, FieldName,
		ConsId, FieldNumber) :-
	( type_to_type_id(TermType, TermTypeId0, _) ->
		TermTypeId = TermTypeId0
	;
		error(
		"get_constructor_containing_field: type_to_type_id failed")
	),
	module_info_types(ModuleInfo, Types),
	map__lookup(Types, TermTypeId, TermTypeDefn),
	hlds_data__get_type_defn_body(TermTypeDefn, TermTypeBody),
	( TermTypeBody = du_type(Ctors, _, _, _) ->
		get_constructor_containing_field_2(Ctors, FieldName, ConsId,
			FieldNumber)
	;
		error("get_constructor_containing_field: not du type")
	).

:- pred get_constructor_containing_field_2(list(constructor),
		ctor_field_name, cons_id, int).
:- mode get_constructor_containing_field_2(in, in, out, out) is det.

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

:- pred get_constructor_containing_field_3(list(constructor_arg),
		ctor_field_name, int, int).
:- mode get_constructor_containing_field_3(in, in, in, out) is semidet.

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

:- pred create_atomic_unification_with_nonlocals(prog_var, unify_rhs,
		hlds_goal_info, set(prog_var), list(prog_var),
		unify_context, hlds_goal).
:- mode create_atomic_unification_with_nonlocals(in, in,
		in, in, in, in, out) is det.

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

:- pred make_new_vars(list(type), list(prog_var), vartypes, vartypes,
		prog_varset, prog_varset).
:- mode make_new_vars(in, out, in, out, in, out) is det.

make_new_vars(Types, Vars, VarTypes0, VarTypes, VarSet0, VarSet) :-
	list__length(Types, NumVars),
	varset__new_vars(VarSet0, NumVars, Vars, VarSet),
	map__det_insert_from_corresponding_lists(VarTypes0,
		Vars, Types, VarTypes).

:- pred make_new_var((type), prog_var, vartypes, vartypes,
		prog_varset, prog_varset).
:- mode make_new_var(in, out, in, out, in, out) is det.

make_new_var(Type, Var, VarTypes0, VarTypes, VarSet0, VarSet) :-
	varset__new_var(VarSet0, Var, VarSet),
	map__det_insert(VarTypes0, Var, Type, VarTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
