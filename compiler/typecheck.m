%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck.m.
% Main author: fjh.
%
% This file contains the Mercury type-checker.
%
% The predicates in this module are named as follows:
%
% 	Predicates that type check a particular language
% 	construct (goal, clause, etc.) are called typecheck_*.
% 	These will eventually have to iterate over every type
% 	assignment in the type assignment set.
%
% 	Predicates that unify two things with respect to a
% 	single type assignment, as opposed to a type assignment set
% 	are called type_assign_*.
%
% 	Access predicates for the typecheck_info data structure are called
% 	typecheck_info_*.
%
% There are four sorts of types:
%
% 1) discriminated unions:
%	:- type tree(T) ---> nil ; t(tree(T), T, tree(T)).
%
% 2) equivalent types (treated identically, ie, same name.  Any number
%	of types can be equivalent; the *canonical* one is the one
%	which is not defined using ==):
%	:- type real == float.
%
%    Currently references to equivalence types are expanded
%    in a separate pass by mercury_compile.m.  It would be better
%    to avoid expanding them (and instead modify the type unification
%    algorithm to handle equivalent types) because this would
%    give better error messages.  However, this is not a high
%    priority.
%
% 3) higher-order predicate and function types
%	pred, pred(T), pred(T1, T2), pred(T1, T2, T3), ...
%	func(T1) = T2, func(T1, T2) = T3, ...
%
% 4) builtin types
%	character, int, float, string
%       These types have special syntax for constants.
%	There may be other types (list(T), unit, univ,
%	etc.) provided by the system, but they can just
%	be part of the standard library.
%
% Each exported predicate must have a `:- pred' declaration specifying the
% types of the arguments for that predicate.  For predicates that are
% local to a module, we infer the types.
%
%-----------------------------------------------------------------------------%
%
% Known Bugs:
%
% XXX	Type inference doesn't handle ambiguity as well as it could do.
%	We should do a topological sort, and then typecheck it all
%	bottom-up.  If we infer an ambiguous type for a pred, we should
%	not reject it immediately; instead we should give it an overloaded
%	type, and keep going.  When we've finished type inference, we should
%	then delete unused overloadings, and only then should we report
%	ambiguity errors, if any overloading still remains.
%
% Wish list:
%
%	we should handle equivalence types here
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds__typecheck.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__hlds_data.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool, io, list, map.

	% typecheck(Module0, Module, FoundError,
	%		ExceededIterationLimit, IO0, IO)
	%
	% Type-checks Module0 and annotates it with variable typings
	% (returning the result in Module), printing out appropriate
	% error messages.
	% FoundError is set to `yes' if there are any errors and
	% `no' otherwise.
	% ExceededIterationLimit is set to `yes' if the type inference
	% iteration limit was reached and `no' otherwise.

:- pred typecheck(module_info::in, module_info::out, bool::out, bool::out,
	io::di, io::uo) is det.

	% Find a predicate which matches the given name and argument types.
	% Abort if there is no matching pred.
	% Abort if there are multiple matching preds.

:- pred typecheck__resolve_pred_overloading(module_info::in, pred_markers::in,
	list(type)::in, tvarset::in, sym_name::in, sym_name::out, pred_id::out)
	is det.

	% Find a predicate or function from the list of pred_ids
	% which matches the given name and argument types.
	% Fail if there is no matching pred.
	% Abort if there are multiple matching preds.

:- pred typecheck__find_matching_pred_id(list(pred_id)::in, module_info::in,
	tvarset::in, list(type)::in, pred_id::out, sym_name::out) is semidet.

	% Apply context reduction to the list of class constraints by applying
	% the instance rules or superclass rules, building up proofs for
	% redundant constraints
:- pred typecheck__reduce_context_by_rule_application(instance_table::in,
	superclass_table::in, list(class_constraint)::in, tsubst::in,
	tvarset::in, tvarset::out,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out,
	list(class_constraint)::in, list(class_constraint)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__clause_to_proc.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__passes_aux.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module parse_tree__prog_type.

:- import_module getopt_io.
:- import_module int, set, string, require, multi_map.
:- import_module assoc_list, std_util, term, varset, term_io.

%-----------------------------------------------------------------------------%

typecheck(!Module, FoundError, ExceededIterationLimit, !IO) :-
	globals__io_lookup_bool_option(statistics, Statistics, !IO),
	globals__io_lookup_bool_option(verbose, Verbose, !IO),
	maybe_write_string(Verbose, "% Type-checking clauses...\n", !IO),
	typecheck_module(!Module, FoundError, ExceededIterationLimit, !IO),
	maybe_report_stats(Statistics, !IO).

%-----------------------------------------------------------------------------%

	% Type-check the code for all the predicates in a module.

:- pred typecheck_module(module_info::in, module_info::out,
	bool::out, bool::out, io::di, io::uo) is det.

typecheck_module(!Module, FoundError, ExceededIterationLimit, !IO) :-
	module_info_predids(!.Module, PredIds),
	globals__io_lookup_int_option(type_inference_iteration_limit,
		MaxIterations, !IO),
	typecheck_to_fixpoint(1, MaxIterations, PredIds, !Module,
		FoundError, ExceededIterationLimit, !IO),
	write_inference_messages(PredIds, !.Module, !IO).

	% Repeatedly typecheck the code for a group of predicates
	% until a fixpoint is reached, or until some errors are detected.

:- pred typecheck_to_fixpoint(int::in, int::in, list(pred_id)::in,
	module_info::in, module_info::out, bool::out, bool::out,
	io::di, io::uo) is det.

typecheck_to_fixpoint(Iteration, NumIterations, PredIds, !Module,
		FoundError, ExceededIterationLimit, !IO) :-
	typecheck_module_one_iteration(Iteration, PredIds, !Module,
		no, FoundError1, no, Changed, !IO),
	( ( Changed = no ; FoundError1 = yes ) ->
		FoundError = FoundError1,
		ExceededIterationLimit = no
	;
		globals__io_lookup_bool_option(debug_types, DebugTypes, !IO),
		( DebugTypes = yes ->
			write_inference_messages(PredIds, !.Module, !IO)
		;
			true
		),
		( Iteration < NumIterations ->
			typecheck_to_fixpoint(Iteration + 1, NumIterations,
				PredIds, !Module, FoundError,
				ExceededIterationLimit, !IO)
		;
			typecheck_report_max_iterations_exceeded(!IO),
			FoundError = yes,
			ExceededIterationLimit = yes
		)
	).

:- pred typecheck_report_max_iterations_exceeded(io::di, io::uo) is det.

typecheck_report_max_iterations_exceeded -->
	io__set_exit_status(1),
	io__write_strings([
		"Type inference iteration limit exceeded.\n",
		"This probably indicates that your program has a type error.\n",
		"You should declare the types explicitly.\n"
	]),
	globals__io_lookup_int_option(type_inference_iteration_limit,
		MaxIterations),
	io__format("(The current limit is %d iterations.  You can use the\n",
		[i(MaxIterations)]),
	io__write_string("`--type-inference-iteration-limit' option " ++
		"to increase the limit).\n").

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred typecheck_module_one_iteration(int::in, list(pred_id)::in,
	module_info::in, module_info::out, bool::in, bool::out,
	bool::in, bool::out, io::di, io::uo) is det.

typecheck_module_one_iteration(_, [], !ModuleInfo, !Error, !Changed, !IO).
typecheck_module_one_iteration(Iteration, [PredId | PredIds], !ModuleInfo,
		!Error, !Changed, !IO) :-
	module_info_preds(!.ModuleInfo, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	( pred_info_is_imported(PredInfo0) ->
		true
	;
		typecheck_pred_if_needed(Iteration, PredId, PredInfo0,
			PredInfo1, !ModuleInfo, NewError, NewChanged, !IO),
		(
			NewError = no,
			map__det_update(Preds0, PredId, PredInfo1, Preds),
			module_info_set_preds(Preds, !ModuleInfo)
		;
			NewError = yes,
%		/********************
%		This code is not needed at the moment,
%		since currently we don't run mode analysis if
%		there are any type errors.
%		And this code also causes problems:
%		if there are undefined modes,
%		this code can end up calling error/1,
%		since post_typecheck__finish_ill_typed_pred
%		assumes that there are no undefined modes.
%			%
%			% if we get an error, we need to call
%			% post_typecheck__finish_ill_typed_pred on the
%			% pred, to ensure that its mode declaration gets
%			% properly module qualified; then we call
%			% `remove_predid', so that the predicate's definition
%			% will be ignored by later passes (the declaration
%			% will still be used to check any calls to it).
%			%
%			post_typecheck__finish_ill_typed_pred(ModuleInfo0,
%				PredId, PredInfo1, PredInfo),
%			map__det_update(Preds0, PredId, PredInfo, Preds),
%		*******************/
			map__det_update(Preds0, PredId, PredInfo1, Preds),
			module_info_set_preds(Preds, !ModuleInfo),
			module_info_remove_predid(PredId, !ModuleInfo)
		),
		bool__or(NewError, !Error),
		bool__or(NewChanged, !Changed)
	),
	typecheck_module_one_iteration(Iteration, PredIds, !ModuleInfo,
		!Error, !Changed, !IO).

:- pred typecheck_pred_if_needed(int::in, pred_id::in,
	pred_info::in, pred_info::out, module_info::in, module_info::out,
	bool::out, bool::out, io::di, io::uo) is det.

typecheck_pred_if_needed(Iteration, PredId, !PredInfo, !ModuleInfo,
		Error, Changed, !IO) :-
	(
		% Compiler-generated predicates are created already
		% type-correct, so there's no need to typecheck them.
		% The same is true for builtins. But, compiler-generated
		% unify predicates are not guaranteed to be type-correct
		% if they call a user-defined equality or comparison predicate
		% or if it is a special pred for an existentially typed data
		% type.
		(
			is_unify_or_compare_pred(!.PredInfo),
			\+ special_pred_needs_typecheck(!.PredInfo,
				!.ModuleInfo)
		;
			pred_info_is_builtin(!.PredInfo)
		)
	->
		pred_info_clauses_info(!.PredInfo, ClausesInfo0),
		clauses_info_clauses(ClausesInfo0, Clauses0),
		( Clauses0 = [] ->
			pred_info_mark_as_external(!PredInfo)
		;
			true
		),
		Error = no,
		Changed = no
	;
		typecheck_pred(Iteration, PredId, !PredInfo, !ModuleInfo,
			Error, Changed, !IO)
	).

:- pred typecheck_pred(int::in, pred_id::in,
	pred_info::in, pred_info::out, module_info::in, module_info::out,
	bool::out, bool::out, io::di, io::uo) is det.

typecheck_pred(Iteration, PredId, !PredInfo, !ModuleInfo, Error, Changed,
		!IO) :-
	globals__io_get_globals(Globals, !IO),
	( Iteration = 1 ->
		maybe_add_field_access_function_clause(!.ModuleInfo,
			!PredInfo),
		maybe_improve_headvar_names(Globals, !PredInfo),

		% The goal_type of the pred_info may have been changed
		% by maybe_add_field_access_function_clause.
		module_info_set_pred_info(PredId, !.PredInfo, !ModuleInfo)
	;
		true
	),
	pred_info_arg_types(!.PredInfo, _ArgTypeVarSet, ExistQVars0,
		ArgTypes0),
	pred_info_clauses_info(!.PredInfo, ClausesInfo0),
	clauses_info_clauses(ClausesInfo0, Clauses0),
	clauses_info_headvars(ClausesInfo0, HeadVars),
	clauses_info_varset(ClausesInfo0, VarSet0),
	clauses_info_explicit_vartypes(ClausesInfo0, ExplicitVarTypes0),
	pred_info_get_markers(!.PredInfo, Markers0),
	% Handle the --allow-stubs and --warn-stubs options.
	% If --allow-stubs is set, and there are no clauses,
	% issue a warning if --warn-stubs is set, and then
	% generate a "stub" clause that just throws an exception.
	(
		Clauses0 = [],
		globals__lookup_bool_option(Globals, allow_stubs, yes),
		\+ check_marker(Markers0, class_method)
	->
		(
			globals__lookup_bool_option(Globals,
				warn_stubs, yes)
		->
			report_no_clauses("Warning", PredId,
				!.PredInfo, !.ModuleInfo, !IO)
		;
			true
		),
		PredPieces = describe_one_pred_name(!.ModuleInfo,
			should_module_qualify, PredId),
		PredName = error_pieces_to_string(PredPieces),
		generate_stub_clause(PredName, !PredInfo, !.ModuleInfo,
			StubClause, VarSet0, VarSet),
		Clauses1 = [StubClause],
		clauses_info_set_clauses(Clauses1, ClausesInfo0, ClausesInfo1),
		clauses_info_set_varset(VarSet, ClausesInfo1, ClausesInfo2)
	;
		VarSet = VarSet0,
		Clauses1 = Clauses0,
		ClausesInfo2 = ClausesInfo0
	),
	(
		Clauses1 = []
	->
		% There are no clauses for class methods.
		% The clauses are generated later on,
		% in polymorphism__expand_class_method_bodies
		( check_marker(Markers0, class_method) ->
			% For the moment, we just insert the types
			% of the head vars into the clauses_info
			map__from_corresponding_lists(HeadVars, ArgTypes0,
				VarTypes),
			clauses_info_set_vartypes(VarTypes,
				ClausesInfo2, ClausesInfo),
			pred_info_set_clauses_info(ClausesInfo, !PredInfo),
				% We also need to set the head_type_params
				% field to indicate that all the existentially
				% quantified tvars in the head of this
				% pred are indeed bound by this predicate.
			term__vars_list(ArgTypes0,
				HeadVarsIncludingExistentials),
			pred_info_set_head_type_params(
				HeadVarsIncludingExistentials, !PredInfo),
			Error = no,
			Changed = no
		;
			report_no_clauses("Error", PredId, !.PredInfo,
				!.ModuleInfo, !IO),
			Error = yes,
			Changed = no
		)
	;
		pred_info_typevarset(!.PredInfo, TypeVarSet0),
		pred_info_import_status(!.PredInfo, Status),
		( check_marker(Markers0, infer_type) ->
			% For a predicate whose type is inferred,
			% the predicate is allowed to bind the type
			% variables in the head of the predicate's
			% type declaration.  Such predicates are given an
			% initial type declaration of
			% `pred foo(T1, T2, ..., TN)' by make_hlds.m.
			Inferring = yes,
			write_pred_progress_message("% Inferring type of ",
				PredId, !.ModuleInfo, !IO),
			HeadTypeParams1 = [],
			PredConstraints = constraints([], [])
		;
			Inferring = no,
			write_pred_progress_message("% Type-checking ",
				PredId, !.ModuleInfo, !IO),
			term__vars_list(ArgTypes0, HeadTypeParams0),
			list__delete_elems(HeadTypeParams0, ExistQVars0,
				HeadTypeParams1),
			pred_info_get_class_context(!.PredInfo,
				PredConstraints)
		),

		%
		% let the initial constraint set be the
		% dual of the constraints for this pred
		% (anything which we can assume in the caller
		% is something that we must prove in the callee,
		% and vice versa)
		%
		dual_constraints(PredConstraints, Constraints),
		(
			pred_info_is_field_access_function(!.ModuleInfo,
				!.PredInfo)
		->
			IsFieldAccessFunction = yes
		;
			IsFieldAccessFunction = no
		),
		pred_info_get_markers(!.PredInfo, Markers),
		typecheck_info_init(!.ModuleInfo, PredId,
			IsFieldAccessFunction, TypeVarSet0, VarSet,
			ExplicitVarTypes0, HeadTypeParams1,
			Constraints, Status, Markers, Info1),
		typecheck_info_get_type_assign_set(Info1, OrigTypeAssignSet),
		typecheck_clause_list(HeadVars, ArgTypes0,
			Clauses1, Clauses, Info1, Info2, !IO),
		% we need to perform a final pass of context reduction
		% at the end, before checking the typeclass constraints
		perform_context_reduction(OrigTypeAssignSet, Info2, Info3,
			!IO),
		typecheck_check_for_ambiguity(whole_pred, HeadVars,
			Info3, Info4, !IO),
		typecheck_info_get_final_info(Info4, HeadTypeParams1,
			ExistQVars0, ExplicitVarTypes0, TypeVarSet,
			HeadTypeParams2, InferredVarTypes0,
			InferredTypeConstraints0, ConstraintProofs,
			TVarRenaming, ExistTypeRenaming),
		map__optimize(InferredVarTypes0, InferredVarTypes),
		clauses_info_set_vartypes(InferredVarTypes,
			ClausesInfo2, ClausesInfo3),

		%
		% Apply substitutions to the explicit vartypes.
		%
		( ExistQVars0 = [] ->
			ExplicitVarTypes1 = ExplicitVarTypes0
		;
			apply_variable_renaming_to_type_map(ExistTypeRenaming,
				ExplicitVarTypes0, ExplicitVarTypes1)
		),
		apply_variable_renaming_to_type_map(TVarRenaming,
			ExplicitVarTypes1, ExplicitVarTypes),

		clauses_info_set_explicit_vartypes(ExplicitVarTypes,
			ClausesInfo3, ClausesInfo4),
		clauses_info_set_clauses(Clauses, ClausesInfo4, ClausesInfo),
		pred_info_set_clauses_info(ClausesInfo, !PredInfo),
		pred_info_set_typevarset(TypeVarSet, !PredInfo),
		pred_info_set_constraint_proofs(ConstraintProofs, !PredInfo),

		%
		% Split the inferred type class constraints into those
		% that apply only to the head variables, and those that
		% apply to type variables which occur only in the body.
		%
		map__apply_to_list(HeadVars, InferredVarTypes, ArgTypes),
		term__vars_list(ArgTypes, ArgTypeVars),
		restrict_to_head_vars(InferredTypeConstraints0, ArgTypeVars,
			InferredTypeConstraints, UnprovenBodyConstraints),

		%
		% If there are any as-yet-unproven constraints on type
		% variables in the body, then save these in the pred_info.
		% If it turns out that this pass was the last pass of type
		% inference, the post_typecheck.m will report an error.
		% But we can't report an error now, because a later pass
		% of type inference could cause some type variables to become
		% bound in to types that make the constraints satisfiable,
		% causing the error to go away.
		%
		pred_info_set_unproven_body_constraints(
			UnprovenBodyConstraints, !PredInfo),

		(
			Inferring = yes,
			%
			% We need to infer which of the head variable
			% types must be existentially quantified
			%
			infer_existential_types(ArgTypeVars, ExistQVars,
				HeadTypeParams2, HeadTypeParams),

			%
			% Now save the information we inferred in the pred_info
			%
			pred_info_set_head_type_params(HeadTypeParams,
				!PredInfo),
			pred_info_set_arg_types(TypeVarSet, ExistQVars,
				ArgTypes, !PredInfo),
			pred_info_get_class_context(!.PredInfo,
				OldTypeConstraints),
			pred_info_set_class_context(InferredTypeConstraints,
				!PredInfo),
			%
			% Check if anything changed
			%
			(
				% If the argument types and the type
				% constraints are identical up to renaming,
				% then nothing has changed.
				argtypes_identical_up_to_renaming(
					ExistQVars0, ArgTypes0,
					OldTypeConstraints,
					ExistQVars, ArgTypes,
					InferredTypeConstraints)
			->
				Changed = no
			;
				Changed = yes
			)
		;
			Inferring = no,
			pred_info_set_head_type_params(HeadTypeParams2,
				!PredInfo),
			pred_info_get_origin(!.PredInfo, Origin0),

			%
			% leave the original argtypes etc., but
			% apply any substititions that map existentially
			% quantified type variables to other type vars,
			% and then rename them all to match the new typevarset,
			% so that the type variables names match up
			% (e.g. with the type variables in the
			% constraint_proofs)
			%

			% apply any type substititions that map existentially
			% quantified type variables to other type vars
			( ExistQVars0 = [] ->
				% optimize common case
				ExistQVars1 = [],
				ArgTypes1 = ArgTypes0,
				PredConstraints1 = PredConstraints,
				Origin1 = Origin0
			;
				apply_var_renaming_to_var_list(ExistQVars0,
					ExistTypeRenaming, ExistQVars1),
				term__apply_variable_renaming_to_list(
					ArgTypes0, ExistTypeRenaming,
					ArgTypes1),
				apply_variable_renaming_to_constraints(
					ExistTypeRenaming,
					PredConstraints, PredConstraints1),
				rename_instance_method_constraints(
					ExistTypeRenaming, Origin0, Origin1)
			),

			% rename them all to match the new typevarset
			apply_var_renaming_to_var_list(ExistQVars1,
				TVarRenaming, ExistQVars),
			term__apply_variable_renaming_to_list(ArgTypes1,
				TVarRenaming, RenamedOldArgTypes),
			apply_variable_renaming_to_constraints(TVarRenaming,
				PredConstraints1, RenamedOldConstraints),
			rename_instance_method_constraints(TVarRenaming,
				Origin1, Origin),

			% save the results in the pred_info
			pred_info_set_arg_types(TypeVarSet, ExistQVars,
				RenamedOldArgTypes, !PredInfo),
			pred_info_set_class_context(RenamedOldConstraints,
				!PredInfo),
			pred_info_set_origin(Origin, !PredInfo),

			Changed = no
		),
		typecheck_info_get_found_error(Info4, Error)
	).

	% Mark the predicate as a stub, and generate a clause of the form
	%	<p>(...) :-
	%		PredName = "<Predname>",
	%		private_builtin.no_clauses(PredName).
	% or
	%	<p>(...) :-
	%		PredName = "<Predname>",
	%		private_builtin.sorry(PredName).
	% depending on whether the predicate is part of
	% the Mercury standard library or not.
:- pred generate_stub_clause(string::in, pred_info::in, pred_info::out,
	module_info::in, clause::out, prog_varset::in, prog_varset::out)
	is det.

generate_stub_clause(PredName, !PredInfo, ModuleInfo, StubClause, !VarSet) :-
	%
	% Mark the predicate as a stub
	% (i.e. record that it originally had no clauses)
	%
	pred_info_get_markers(!.PredInfo, Markers0),
	add_marker(stub, Markers0, Markers),
	pred_info_set_markers(Markers, !PredInfo),

	%
	% Generate `PredName = "<PredName>"'
	%
	varset__new_named_var(!.VarSet, "PredName", PredNameVar, !:VarSet),
	make_string_const_construction(PredNameVar, PredName, UnifyGoal),
	%
	% Generate `private_builtin.no_clauses(PredName)'
	% or `private_builtin.sorry(PredName)'
	%
	ModuleName = pred_info_module(!.PredInfo),
	( mercury_std_library_module_name(ModuleName) ->
		CalleeName = "sorry"
	;
		CalleeName = "no_clauses"
	),
	pred_info_context(!.PredInfo, Context),
	generate_simple_call(mercury_private_builtin_module, CalleeName,
		predicate, only_mode, det, [PredNameVar], [], [], ModuleInfo,
		Context, CallGoal),
	%
	% Combine the unification and call into a conjunction
	%
	goal_info_init(Context, GoalInfo),
	Body = conj([UnifyGoal, CallGoal]) - GoalInfo,
	StubClause = clause([], Body, mercury, Context).

:- pred rename_instance_method_constraints(map(tvar, tvar)::in,
	pred_origin::in,
	pred_origin::out) is det.

rename_instance_method_constraints(Renaming, Origin0, Origin) :-
	( Origin0 = instance_method(Constraints0) ->
		Constraints0 = instance_method_constraints(ClassId,
			InstanceTypes0, InstanceConstraints0,
			ClassMethodClassContext0),
		term__apply_variable_renaming_to_list(InstanceTypes0,
			Renaming, InstanceTypes),
		apply_variable_renaming_to_constraint_list(Renaming,
			InstanceConstraints0, InstanceConstraints),
		apply_variable_renaming_to_constraints(Renaming,
			ClassMethodClassContext0, ClassMethodClassContext),
		Constraints = instance_method_constraints(ClassId,
			InstanceTypes, InstanceConstraints,
			ClassMethodClassContext),
		Origin = instance_method(Constraints)
	;
		Origin = Origin0
	).

	%
	% infer which of the head variable
	% types must be existentially quantified
	%
:- pred infer_existential_types(list(tvar)::in, existq_tvars::out,
	head_type_params::in, head_type_params::out) is det.

infer_existential_types(ArgTypeVars, ExistQVars,
		HeadTypeParams0, HeadTypeParams) :-
	%
	% First, infer which of the head variable
	% types must be existentially quantified:
	% anything that was inserted into the HeadTypeParams0
	% set must have been inserted due to an existential
	% type in something we called, and thus must be
	% existentially quantified.
	% (Note that concrete types are "more general" than
	% existentially quantified types, so we prefer to
	% infer a concrete type if we can rather than an
	% existential type.)
	%
	set__list_to_set(ArgTypeVars, ArgTypeVarsSet),
	set__list_to_set(HeadTypeParams0, HeadTypeParamsSet),
	set__intersect(ArgTypeVarsSet, HeadTypeParamsSet, ExistQVarsSet),
	set__difference(ArgTypeVarsSet, ExistQVarsSet, UnivQVarsSet),
	set__to_sorted_list(ExistQVarsSet, ExistQVars),
	set__to_sorted_list(UnivQVarsSet, UnivQVars),

	%
	% Then we need to insert the universally
	% quantified head variable types into the
	% HeadTypeParams set, which will now contain
	% all the type variables that are produced
	% either by stuff we call or by our caller.
	% This is needed so that it has the right
	% value when post_typecheck.m uses it to
	% check for unbound type variables.
	%
	list__append(UnivQVars, HeadTypeParams0, HeadTypeParams).

	%
	% restrict_to_head_vars(Constraints0, HeadVarTypes, Constraints,
	%		UnprovenConstraints):
	%	Constraints is the subset of Constraints0 which contain
	%	no type variables other than those in HeadVarTypes.
	%	UnprovenConstraints is any unproven (universally quantified)
	%	type constraints on variables not in HeadVarTypes.
	%
:- pred restrict_to_head_vars(class_constraints::in, list(tvar)::in,
	class_constraints::out, list(class_constraint)::out) is det.

restrict_to_head_vars(constraints(UnivCs0, ExistCs0), ArgVarTypes,
		constraints(UnivCs, ExistCs), UnprovenCs) :-
	restrict_to_head_vars_2(UnivCs0, ArgVarTypes, UnivCs, UnprovenCs),
	restrict_to_head_vars_2(ExistCs0, ArgVarTypes, ExistCs, _).

:- pred restrict_to_head_vars_2(list(class_constraint)::in, list(tvar)::in,
	list(class_constraint)::out, list(class_constraint)::out) is det.

restrict_to_head_vars_2(ClassConstraints, HeadTypeVars, HeadClassConstraints,
		OtherClassConstraints) :-
	list__filter(is_head_class_constraint(HeadTypeVars),
		ClassConstraints, HeadClassConstraints, OtherClassConstraints).

:- pred is_head_class_constraint(list(tvar)::in, class_constraint::in)
	is semidet.

is_head_class_constraint(HeadTypeVars, constraint(_Name, Types)) :-
	% SICStus does not allow the following syntax
	% all [TVar] (
	% 	term__contains_var_list(Types, TVar) =>
	% 		list__member(TVar, HeadTypeVars)
	% ).
	\+ (
		term__contains_var_list(Types, TVar),
		\+ list__member(TVar, HeadTypeVars)
	).

% Check whether the argument types, type quantifiers, and type constraints
% are identical up to renaming.
%
% Note that we can't compare each of the parts seperately, since we need to
% ensure that the renaming (if any) is consistent over all the arguments and
% all the constraints.  So we need to append all the relevant types into one
% big type list and then compare them in a single call to
% identical_up_to_renaming.

:- pred argtypes_identical_up_to_renaming(
	existq_tvars::in, list(type)::in, class_constraints::in,
	existq_tvars::in, list(type)::in, class_constraints::in) is semidet.

argtypes_identical_up_to_renaming(ExistQVarsA, ArgTypesA, TypeConstraintsA,
		ExistQVarsB, ArgTypesB, TypeConstraintsB) :-
	same_structure(TypeConstraintsA, TypeConstraintsB,
		ConstrainedTypesA, ConstrainedTypesB),
	term__var_list_to_term_list(ExistQVarsA, ExistQVarTermsA),
	term__var_list_to_term_list(ExistQVarsB, ExistQVarTermsB),
	list__condense([ExistQVarTermsA, ArgTypesA, ConstrainedTypesA],
		TypesListA),
	list__condense([ExistQVarTermsB, ArgTypesB, ConstrainedTypesB],
		TypesListB),
	identical_up_to_renaming(TypesListA, TypesListB).

% check if two sets of type class constraints have the same structure
% (i.e. they specify the same list of type classes with the same arities)
% and if so, concatenate the argument types for all the type classes
% in each set of type class constraints and return them.
%
:- pred same_structure(class_constraints::in, class_constraints::in,
	list(type)::out, list(type)::out) is semidet.

same_structure(ConstraintsA, ConstraintsB, TypesA, TypesB) :-
	ConstraintsA = constraints(UnivCsA, ExistCsA),
	ConstraintsB = constraints(UnivCsB, ExistCsB),
	% these calls to same_length are just an optimization,
	% to catch the simple cases quicker
	list__same_length(UnivCsA, UnivCsB),
	list__same_length(ExistCsA, ExistCsB),
	same_structure_2(UnivCsA, UnivCsB, UnivTypesA, UnivTypesB),
	same_structure_2(ExistCsA, ExistCsB, ExistTypesA, ExistTypesB),
	list__append(ExistTypesA, UnivTypesA, TypesA),
	list__append(ExistTypesB, UnivTypesB, TypesB).

:- pred same_structure_2(list(class_constraint)::in, list(class_constraint)::in,
	list(type)::out, list(type)::out) is semidet.

same_structure_2([], [], [], []).
same_structure_2([ConstraintA | ConstraintsA], [ConstraintB | ConstraintsB],
		TypesA, TypesB) :-
	ConstraintA = constraint(ClassName, ArgTypesA),
	ConstraintB = constraint(ClassName, ArgTypesB),
	list__same_length(ArgTypesA, ArgTypesB),
	same_structure_2(ConstraintsA, ConstraintsB, TypesA0, TypesB0),
	list__append(ArgTypesA, TypesA0, TypesA),
	list__append(ArgTypesB, TypesB0, TypesB).

%
% A compiler-generated predicate only needs type checking if
%	(a) it is a user-defined equality pred
% or	(b) it is the unification or comparison predicate for an
%	    existially quantified type.
%
% In case (b), we need to typecheck it to fill in the head_type_params
% field in the pred_info.
%

:- pred special_pred_needs_typecheck(pred_info::in, module_info::in)
	is semidet.

special_pred_needs_typecheck(PredInfo, ModuleInfo) :-
	%
	% check if the predicate is a compiler-generated special
	% predicate, and if so, for which type
	%
	pred_info_get_origin(PredInfo, Origin),
	Origin = special_pred(SpecialPredId - TypeCtor),
	%
	% check that the special pred isn't one of the builtin
	% types which don't have a hlds_type_defn
	%
	\+ list__member(TypeCtor, builtin_type_ctors_with_no_hlds_type_defn),
	%
	% check whether that type is a type for which there is
	% a user-defined equality predicate, or which is existentially typed.
	%
	module_info_types(ModuleInfo, TypeTable),
	map__lookup(TypeTable, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, Body),
	special_pred_for_type_needs_typecheck(ModuleInfo, SpecialPredId, Body).

%-----------------------------------------------------------------------------%

	%
	% For a field access function for which the user has supplied
	% a declaration but no clauses, add a clause
	% 'foo :='(X, Y) = 'foo :='(X, Y).
	% As for the default clauses added for builtins, this is not a
	% recursive call -- post_typecheck.m will expand the body into
	% unifications.
	%
:- pred maybe_add_field_access_function_clause(module_info::in,
	pred_info::in, pred_info::out) is det.

maybe_add_field_access_function_clause(ModuleInfo, !PredInfo) :-
	pred_info_import_status(!.PredInfo, ImportStatus),
	pred_info_clauses_info(!.PredInfo, ClausesInfo0),
	clauses_info_clauses(ClausesInfo0, Clauses0),
	(
		pred_info_is_field_access_function(ModuleInfo, !.PredInfo),
		Clauses0 = [],
		status_defined_in_this_module(ImportStatus, yes)
	->
		clauses_info_headvars(ClausesInfo0, HeadVars),
		pred_args_to_func_args(HeadVars, FuncArgs, FuncRetVal),
		pred_info_context(!.PredInfo, Context),
		FuncModule = pred_info_module(!.PredInfo),
		FuncName = pred_info_name(!.PredInfo),
		PredArity = pred_info_orig_arity(!.PredInfo),
		adjust_func_arity(function, FuncArity, PredArity),
		FuncSymName = qualified(FuncModule, FuncName),
		create_atomic_unification(FuncRetVal,
			functor(cons(FuncSymName, FuncArity), no, FuncArgs),
			Context, explicit, [], Goal0),
		Goal0 = GoalExpr - GoalInfo0,
		set__list_to_set(HeadVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
		Goal = GoalExpr - GoalInfo,
		ProcIds = [], % the clause applies to all procedures.
		Clause = clause(ProcIds, Goal, mercury, Context),
		clauses_info_set_clauses([Clause], ClausesInfo0, ClausesInfo),
		pred_info_update_goal_type(clauses, !PredInfo),
		pred_info_set_clauses_info(ClausesInfo, !PredInfo),
		pred_info_get_markers(!.PredInfo, Markers0),
		add_marker(calls_are_fully_qualified, Markers0, Markers),
		pred_info_set_markers(Markers, !PredInfo)
	;
		true
	).

	% If there is only one clause, use the original head variables
	% from the clause rather than the introduced `HeadVar__n' variables
	% as the head variables in the proc_info.
	% This gives better error messages, more meaningful variable
	% names in the debugger and slightly faster compilation.
:- pred maybe_improve_headvar_names(globals::in, pred_info::in, pred_info::out)
	is det.

maybe_improve_headvar_names(Globals, !PredInfo) :-
	pred_info_clauses_info(!.PredInfo, ClausesInfo0),
	clauses_info_clauses(ClausesInfo0, Clauses0),
	clauses_info_headvars(ClausesInfo0, HeadVars0),
	clauses_info_varset(ClausesInfo0, VarSet0),
	(
		% Don't do this when making a `.opt' file.
		% intermod.m needs to perform a similar transformation
		% which this transformation would interfere with (intermod.m
		% places the original argument terms, not just the argument
		% variables in the clause head, and this pass would make it
		% difficult to work out what were the original arguments).
		globals__lookup_bool_option(Globals,
			make_optimization_interface, yes)
	->
		true
	;
		Clauses0 = [SingleClause0]
	->
		SingleClause0 = clause(A, Goal0, C, D),

		Goal0 = _ - GoalInfo0,
		goal_to_conj_list(Goal0, Conj0),
		improve_single_clause_headvars(Conj0, HeadVars0, [],
			VarSet0, VarSet, map__init, Subst, [], RevConj),

		goal_info_get_nonlocals(GoalInfo0, NonLocals0),
		goal_util__rename_vars_in_var_set(NonLocals0, no,
			Subst, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
		conj_list_to_goal(list__reverse(RevConj), GoalInfo, Goal),

		apply_partial_map_to_list(HeadVars0, Subst, HeadVars),
		clauses_info_set_headvars(HeadVars,
			ClausesInfo0, ClausesInfo1),

		SingleClause = clause(A, Goal, C, D),
		clauses_info_set_clauses([SingleClause],
			ClausesInfo1, ClausesInfo2),
		clauses_info_set_varset(VarSet, ClausesInfo2, ClausesInfo),
		pred_info_set_clauses_info(ClausesInfo, !PredInfo)
	;
		%
		% If a headvar is assigned to a variable with the
		% same name (or no name) in every clause, rename
		% it to have that name.
		%
		list__foldl2(find_headvar_names_in_clause(VarSet0, HeadVars0),
			Clauses0, map__init, HeadVarNames, yes, _),
		VarSet = map__foldl(
			(func(HeadVar, MaybeHeadVarName, VarSet1) =
				( MaybeHeadVarName = yes(HeadVarName) ->
					varset__name_var(VarSet1, HeadVar,
						HeadVarName)
				;
					VarSet1
				)
			), HeadVarNames, VarSet0),
		clauses_info_set_varset(VarSet, ClausesInfo0, ClausesInfo),
		pred_info_set_clauses_info(ClausesInfo, !PredInfo)
	).

:- pred improve_single_clause_headvars(list(hlds_goal)::in, list(prog_var)::in,
	list(prog_var)::in, prog_varset::in, prog_varset::out,
	map(prog_var, prog_var)::in, map(prog_var, prog_var)::out,
	list(hlds_goal)::in, list(hlds_goal)::out) is det.

improve_single_clause_headvars([], _, _, !VarSet, !Subst, !RevConj).
improve_single_clause_headvars([Goal | Conj0], HeadVars, SeenVars0,
		!VarSet, !Subst, !RevConj) :-
	( goal_is_headvar_unification(HeadVars, Goal, HeadVar, OtherVar) ->
		%
		% If the headvar doesn't appear elsewhere the
		% unification can be removed.
		%
		(
			% The headvars must be distinct variables, so check
			% that this variable doesn't already appear in the
			% argument list.
			\+ list__member(OtherVar, HeadVars),
			\+ list__member(OtherVar, SeenVars0),

			\+ ( some [OtherGoal] (
				( list__member(OtherGoal, Conj0)
				; list__member(OtherGoal, !.RevConj)
				),
				OtherGoal = _ - OtherGoalInfo,
				goal_info_get_nonlocals(OtherGoalInfo,
					OtherNonLocals),
				set__member(HeadVar, OtherNonLocals)
			))
		->
			SeenVars = [OtherVar | SeenVars0],
			!:Subst = map__det_insert(!.Subst, HeadVar, OtherVar),

			%
			% If the variable wasn't named, use the
			% `HeadVar__n' name.
			%
			(
				\+ varset__search_name(!.VarSet, OtherVar, _),
				varset__search_name(!.VarSet, HeadVar,
					HeadVarName)
			->
				varset__name_var(!.VarSet, OtherVar,
					HeadVarName, !:VarSet)
			;
				true
			)
		;
			!:RevConj = [Goal | !.RevConj],
			SeenVars = SeenVars0,
			(
				varset__search_name(!.VarSet, OtherVar,
					OtherVarName)
			->
				%
				% The unification can't be eliminated,
				% so just rename the head variable.
				%
				varset__name_var(!.VarSet, HeadVar,
					OtherVarName, !:VarSet)
			;
				varset__search_name(!.VarSet, HeadVar,
					HeadVarName)
			->
				%
				% If the variable wasn't named, use the
				% `HeadVar__n' name.
				%
				varset__name_var(!.VarSet, OtherVar,
					HeadVarName, !:VarSet)
			;
				true
			)
		)
	;
		!:RevConj = [Goal | !.RevConj],
		SeenVars = SeenVars0
	),
	improve_single_clause_headvars(Conj0, HeadVars, SeenVars,
		!VarSet, !Subst, !RevConj).

	% Head variables which have the same name in each clause.
	% will have an entry of `yes(Name)' in the result map.
:- pred find_headvar_names_in_clause(prog_varset::in,
	list(prog_var)::in, clause::in,
	map(prog_var, maybe(string))::in, map(prog_var, maybe(string))::out,
	bool::in, bool::out) is det.

find_headvar_names_in_clause(VarSet, HeadVars, Clause,
		HeadVarMap0, HeadVarMap, IsFirstClause, no) :-
	Goal = Clause ^ clause_body,
	goal_to_conj_list(Goal, Conj),
	ClauseHeadVarMap = list__foldl(
		find_headvar_names_in_goal(VarSet, HeadVars),
		Conj, map__init),
	( IsFirstClause = yes ->
		HeadVarMap = ClauseHeadVarMap
	;
		% Check that the variables in this clause match
		% the names in previous clauses.
		HeadVarMap1 = map__foldl(
			(func(HeadVar, MaybeHeadVarName, Map) =
				(
					map__search(Map, HeadVar,
						MaybeClauseHeadVarName),
					MaybeHeadVarName =
						MaybeClauseHeadVarName
				->
					Map
				;
					map__set(Map, HeadVar, no)
				)
			), HeadVarMap0, ClauseHeadVarMap),

		% Check for variables which weren't named in previous
		% clauses. It would be confusing to refer to variable
		% `A' in the second clause below.
		% 	p(A, _).
		%	p([_ | _], _).
		HeadVarMap = map__foldl(
			(func(HeadVar, _, Map) =
				( map__contains(HeadVarMap0, HeadVar) ->
					Map
				;
					map__set(Map, HeadVar, no)
				)
			), HeadVarMap1, HeadVarMap1)
	).

:- func find_headvar_names_in_goal(prog_varset, list(prog_var), hlds_goal,
	map(prog_var, maybe(string))) = map(prog_var, maybe(string)).

find_headvar_names_in_goal(VarSet, HeadVars, Goal, HeadVarMap0) = HeadVarMap :-
	( goal_is_headvar_unification(HeadVars, Goal, HeadVar, OtherVar) ->
		maybe_pred(varset__search_name(VarSet), OtherVar,
			MaybeOtherVarName),
		( map__search(HeadVarMap0, HeadVar, MaybeHeadVarName) ->
			( MaybeOtherVarName = MaybeHeadVarName ->
				HeadVarMap = HeadVarMap0
			;
				HeadVarMap = map__det_update(HeadVarMap0,
					HeadVar, no)
			)
		;
			HeadVarMap = map__set(HeadVarMap0, HeadVar,
				MaybeOtherVarName)
		)
	;
		HeadVarMap = HeadVarMap0
	).

:- pred goal_is_headvar_unification(list(prog_var)::in, hlds_goal::in,
	prog_var::out, prog_var::out) is semidet.

goal_is_headvar_unification(HeadVars, Goal, HeadVar, OtherVar) :-
	Goal = unify(LVar, var(RVar), _, _, _) - _,
	( list__member(LVar, HeadVars) ->
		HeadVar = LVar,
		OtherVar = RVar
	; list__member(RVar, HeadVars) ->
		HeadVar = RVar,
		OtherVar = LVar
	;
		fail
	).

%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.

:- pred typecheck_clause_list(list(prog_var)::in, list(type)::in,
	list(clause)::in, list(clause)::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_clause_list(_, _, [], [], !Info, !IO).
typecheck_clause_list(HeadVars, ArgTypes, [Clause0 | Clauses0],
		[Clause | Clauses], !Info, !IO) :-
	typecheck_clause(HeadVars, ArgTypes, Clause0, Clause, !Info, !IO),
	typecheck_clause_list(HeadVars, ArgTypes, Clauses0, Clauses, !Info,
		!IO).

%-----------------------------------------------------------------------------%

	% Type-check a single clause.

	% As we go through a clause, we determine the possible
	% type assignments for the clause.  A type assignment
	% is an assignment of a type to each variable in the
	% clause.
	%
	% Note that this may cause exponential time & space usage
	% in the presence of overloading of predicates and/or
	% functors.  This is a potentially serious problem, but
	% there's no easy solution apparent.
	%
	% It would be more natural to use non-determinism to write
	% this code, and perhaps even more efficient.
	% But doing it nondeterministically would make good error
	% messages very difficult.

	% we should perhaps do manual garbage collection here

:- pred typecheck_clause(list(prog_var)::in, list(type)::in,
	clause::in, clause::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_clause(HeadVars, ArgTypes, !Clause, !Info, !IO) :-
		% XXX abstract clause/3
	Body0 = !.Clause ^ clause_body,
	Context = !.Clause ^clause_context,
	typecheck_info_set_context(Context, !Info),
		% typecheck the clause - first the head unification, and
		% then the body
	typecheck_var_has_type_list(HeadVars, ArgTypes, 1, !Info, !IO),
	typecheck_goal(Body0, Body, !Info, !IO),
	checkpoint("end of clause", !Info, !IO),
	!:Clause = !.Clause ^ clause_body := Body,
	typecheck_info_set_context(Context, !Info),
	typecheck_check_for_ambiguity(clause_only, HeadVars, !Info, !IO).

%-----------------------------------------------------------------------------%

	% typecheck_check_for_ambiguity/3:
	% If there are multiple type assignments,
	% then we issue an error message here.

	%
	% If stuff-to-check = whole_pred, report an error for any ambiguity,
	% and also check for unbound type variables.
	% But if stuff-to-check = clause_only, then only report
	% errors for type ambiguities that don't involve the head vars,
	% because we may be able to resolve a type ambiguity for a head var
	% in one clause by looking at later clauses.
	% (Ambiguities in the head variables can only arise if we are
	% inferring the type for this pred.)
	%
:- type stuff_to_check
	--->	clause_only
	;	whole_pred.

:- pred typecheck_check_for_ambiguity(stuff_to_check::in, list(prog_var)::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_check_for_ambiguity(StuffToCheck, HeadVars, !Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet),
	(
		% there should always be a type assignment, because
		% if there is an error somewhere, instead of setting
		% the current type assignment set to the empty set,
		% the type-checker should continue with the previous
		% type assignment set (so that it can detect other
		% errors in the same clause).
		TypeAssignSet = [],
		error("internal error in typechecker: no type-assignment")
	;
		TypeAssignSet = [_SingleTypeAssign]

	;
		TypeAssignSet = [TypeAssign1, TypeAssign2 | _],
		%
		% we only report an ambiguity error if
		% (a) we haven't encountered any other errors
		% and if StuffToCheck = clause_only(_),
		% also (b) the ambiguity occurs only in the body,
		% rather than in the head variables (and hence
		% can't be resolved by looking at later clauses).
		%
		typecheck_info_get_found_error(!.Info, FoundError),
		(
			FoundError = no,
			(
				StuffToCheck = whole_pred
			;
				StuffToCheck = clause_only,
				%
				% only report an error if the headvar types
				% are identical (which means that the ambiguity
				% must have occurred in the body)
				%
				type_assign_get_var_types(TypeAssign1,
					VarTypes1),
				type_assign_get_var_types(TypeAssign2,
					VarTypes2),
				type_assign_get_type_bindings(TypeAssign1,
					TypeBindings1),
				type_assign_get_type_bindings(TypeAssign2,
					TypeBindings2),
				map__apply_to_list(HeadVars, VarTypes1,
					HeadTypes1),
				map__apply_to_list(HeadVars, VarTypes2,
					HeadTypes2),
				term__apply_rec_substitution_to_list(
					HeadTypes1, TypeBindings1,
					FinalHeadTypes1),
				term__apply_rec_substitution_to_list(
					HeadTypes2, TypeBindings2,
					FinalHeadTypes2),
				identical_up_to_renaming(FinalHeadTypes1,
					FinalHeadTypes2)
			)
		->
			typecheck_info_set_found_error(yes, !Info),
			report_ambiguity_error(!.Info, TypeAssign1,
				TypeAssign2, !IO)
		;
			true
		)
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal(hlds_goal::in, hlds_goal::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

	% Typecheck a goal.
	% Note that we save the context of the goal in the typeinfo for
	% use in error messages.  Also, if the context of the goal is empty,
	% we set the context of the goal from the surrounding
	% context saved in the type-info.  (That should probably be done
	% in make_hlds, but it was easier to do here.)

typecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, !Info, !IO) :-
	goal_info_get_context(GoalInfo0, Context),
	term__context_init(EmptyContext),
	( Context = EmptyContext ->
		typecheck_info_get_context(!.Info, EnclosingContext),
		goal_info_set_context(GoalInfo0, EnclosingContext, GoalInfo)
	;
		GoalInfo = GoalInfo0,
		typecheck_info_set_context(Context, !Info)
	),
		% type-check the goal
	typecheck_goal_2(Goal0, Goal, !Info, !IO),
	check_warn_too_much_overloading(!Info, !IO).

:- pred typecheck_goal_2(hlds_goal_expr::in, hlds_goal_expr::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_goal_2(conj(List0), conj(List), !Info, !IO) :-
	checkpoint("conj", !Info, !IO),
	typecheck_goal_list(List0, List, !Info, !IO).

typecheck_goal_2(par_conj(List0), par_conj(List), !Info, !IO) :-
	checkpoint("par_conj", !Info, !IO),
	typecheck_goal_list(List0, List, !Info, !IO).

typecheck_goal_2(disj(List0), disj(List), !Info, !IO) :-
	checkpoint("disj", !Info, !IO),
	typecheck_goal_list(List0, List, !Info, !IO).

typecheck_goal_2(if_then_else(Vars, Cond0, Then0, Else0),
		if_then_else(Vars, Cond, Then, Else), !Info, !IO) :-
	checkpoint("if", !Info, !IO),
	typecheck_goal(Cond0, Cond, !Info, !IO),
	checkpoint("then", !Info, !IO),
	typecheck_goal(Then0, Then, !Info, !IO),
	checkpoint("else", !Info, !IO),
	typecheck_goal(Else0, Else, !Info, !IO),
	ensure_vars_have_a_type(Vars, !Info, !IO).

typecheck_goal_2(not(SubGoal0), not(SubGoal), !Info, !IO) :-
	checkpoint("not", !Info, !IO),
	typecheck_goal(SubGoal0, SubGoal, !Info, !IO).

typecheck_goal_2(some(Vars, B, SubGoal0), some(Vars,B, SubGoal), !Info, !IO) :-
	checkpoint("some", !Info, !IO),
	typecheck_goal(SubGoal0, SubGoal, !Info, !IO),
	ensure_vars_have_a_type(Vars, !Info, !IO).

typecheck_goal_2(call(_, B, Args, D, E, Name),
		call(PredId, B, Args, D, E, Name), !Info, !IO) :-
	checkpoint("call", !Info, !IO),
	list__length(Args, Arity),
	typecheck_info_set_called_predid(call(predicate - Name/Arity), !Info),
	typecheck_call_pred(predicate - Name/Arity, Args, PredId, !Info, !IO).

typecheck_goal_2(generic_call(GenericCall0, Args, C, D),
		generic_call(GenericCall, Args, C, D), !Info, !IO) :-
	hlds_goal__generic_call_id(GenericCall0, CallId),
	typecheck_info_set_called_predid(CallId, !Info),
	(
		GenericCall0 = higher_order(PredVar, Purity, _, _),
		GenericCall = GenericCall0,
		checkpoint("higher-order call", !Info, !IO),
		typecheck_higher_order_call(PredVar, Purity, Args, !Info, !IO)
	;
		GenericCall0 = class_method(_, _, _, _),
		error("typecheck_goal_2: unexpected class method call")
	;
		GenericCall0 = unsafe_cast,
		% A cast imposes no restrictions on its argument types,
		% so nothing needs to be done here.
		GenericCall = GenericCall0
	;
		GenericCall0 = aditi_builtin(AditiBuiltin0, PredCallId),
		checkpoint("aditi builtin", !Info, !IO),
		typecheck_aditi_builtin(PredCallId, Args,
			AditiBuiltin0, AditiBuiltin, !Info, !IO),
		GenericCall = aditi_builtin(AditiBuiltin, PredCallId)
	).

typecheck_goal_2(unify(LHS, RHS0, C, D, UnifyContext),
		unify(LHS, RHS, C, D, UnifyContext), !Info, !IO) :-
	checkpoint("unify", !Info, !IO),
	typecheck_info_set_arg_num(0, !Info),
	typecheck_info_set_unify_context(UnifyContext, !Info),
	typecheck_unification(LHS, RHS0, RHS, !Info, !IO).

typecheck_goal_2(switch(_, _, _), _, !Info, !IO) :-
	error("unexpected switch").

typecheck_goal_2(Goal @ foreign_proc(_, PredId, _, Args, _, _), Goal,
		!Info, !IO) :-
	% foreign_procs are automatically generated, so they will
	% always be type-correct, but we need to do the type analysis in order
	% to correctly compute the HeadTypeParams that result from
	% existentially typed foreign_procs. (We could probably do that
	% more efficiently than the way it is done below, though.)
	typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),
	ArgVars = list__map(foreign_arg_var, Args),
	typecheck_call_pred_id(PredId, ArgVars, !Info, !IO),
	perform_context_reduction(OrigTypeAssignSet, !Info, !IO).

typecheck_goal_2(shorthand(ShorthandGoal0), shorthand(ShorthandGoal), !Info,
		!IO) :-
	typecheck_goal_2_shorthand(ShorthandGoal0, ShorthandGoal, !Info, !IO).

:- pred typecheck_goal_2_shorthand(shorthand_goal_expr::in,
	shorthand_goal_expr::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_goal_2_shorthand(bi_implication(LHS0, RHS0),
		bi_implication(LHS, RHS), !Info, !IO) :-
	checkpoint("<=>", !Info, !IO),
	typecheck_goal(LHS0, LHS, !Info, !IO),
	typecheck_goal(RHS0, RHS, !Info, !IO).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds_goal)::in, list(hlds_goal)::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_goal_list([], [], !Info, !IO).
typecheck_goal_list([Goal0 | Goals0], [Goal | Goals], !Info, !IO) :-
	typecheck_goal(Goal0, Goal, !Info, !IO),
	typecheck_goal_list(Goals0, Goals, !Info, !IO).

%-----------------------------------------------------------------------------%

	% ensure_vars_have_a_type(Vars):
	%	Ensure that each variable in Vars has been assigned a type.

:- pred ensure_vars_have_a_type(list(prog_var)::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

ensure_vars_have_a_type(Vars, !Info, !IO) :-
	( Vars = [] ->
		true
	;
		% invent some new type variables to use as the types of
		% these variables
		list__length(Vars, NumVars),
		varset__init(TypeVarSet0),
		varset__new_vars(TypeVarSet0, NumVars, TypeVars, TypeVarSet),
		term__var_list_to_term_list(TypeVars, Types),
		typecheck_var_has_polymorphic_type_list(Vars, TypeVarSet, [],
			Types, constraints([], []), !Info, !IO)
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(prog_var::in, purity::in,
	list(prog_var)::in, typecheck_info::in, typecheck_info::out,
	io::di, io::uo) is det.

typecheck_higher_order_call(PredVar, Purity, Args, !Info, !IO) :-
	list__length(Args, Arity),
	higher_order_pred_type(Purity, Arity, normal,
		TypeVarSet, PredVarType, ArgTypes),
		% The class context is empty because higher-order predicates
		% are always monomorphic.  Similarly for ExistQVars.
	ClassContext = constraints([], []),
	ExistQVars = [],
	typecheck_var_has_polymorphic_type_list([PredVar | Args], TypeVarSet,
		ExistQVars, [PredVarType | ArgTypes], ClassContext, !Info,
		!IO).

:- pred higher_order_pred_type(purity::in, int::in, lambda_eval_method::in,
	tvarset::out, (type)::out, list(type)::out) is det.

	% higher_order_pred_type(Purity, N, EvalMethod,
	%	TypeVarSet, PredType, ArgTypes):
	% Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
	% PredType = `Purity EvalMethod pred(T1, T2, ..., TN)', and
	% ArgTypes = [T1, T2, ..., TN].

higher_order_pred_type(Purity, Arity, EvalMethod, TypeVarSet, PredType,
		ArgTypes) :-
	varset__init(TypeVarSet0),
	varset__new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet),
	term__var_list_to_term_list(ArgTypeVars, ArgTypes),
	construct_higher_order_type(Purity, predicate, EvalMethod, ArgTypes,
		PredType).

:- pred higher_order_func_type(purity::in, int::in, lambda_eval_method::in,
	tvarset::out, (type)::out, list(type)::out, (type)::out) is det.

	% higher_order_func_type(Purity, N, EvalMethod, TypeVarSet,
	%	FuncType, ArgTypes, RetType):
	% Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
	% FuncType = `Purity EvalMethod func(T1, T2, ..., TN) = T0',
	% ArgTypes = [T1, T2, ..., TN], and
	% RetType = T0.

higher_order_func_type(Purity, Arity, EvalMethod, TypeVarSet,
		FuncType, ArgTypes, RetType) :-
	varset__init(TypeVarSet0),
	varset__new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet1),
	varset__new_var(TypeVarSet1, RetTypeVar, TypeVarSet),
	term__var_list_to_term_list(ArgTypeVars, ArgTypes),
	RetType = term__variable(RetTypeVar),
	construct_higher_order_func_type(Purity, EvalMethod,
		ArgTypes, RetType, FuncType).

%-----------------------------------------------------------------------------%

:- pred typecheck_aditi_builtin(simple_call_id::in, list(prog_var)::in,
	aditi_builtin::in, aditi_builtin::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_aditi_builtin(CallId, Args, !Builtin, !Info, !IO) :-
	% This must succeed because make_hlds.m does not add a clause
	% to the clauses_info if it contains Aditi updates with the
	% wrong number of arguments.
	get_state_args_det(Args, OtherArgs, State0, State),
	% XXX using the old version of !Builtin in typecheck_aditi_state_args
	% looks wrong; it should be documented or fixed - zs
	Builtin0 = !.Builtin,
	typecheck_aditi_builtin_2(CallId, OtherArgs, !Builtin, !Info, !IO),
	typecheck_aditi_state_args(Builtin0, CallId, State0, State, !Info,
		!IO).

	% Typecheck the arguments of an Aditi update other than
	% the `aditi__state' arguments.
:- pred typecheck_aditi_builtin_2(simple_call_id::in, list(prog_var)::in,
	aditi_builtin::in, aditi_builtin::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_aditi_builtin_2(CallId, Args,
		aditi_tuple_update(Update, _),
		aditi_tuple_update(Update, PredId), !Info, !IO) :-
	% The tuple to insert or delete has the same argument types
	% as the relation being inserted into or deleted from.
	typecheck_call_pred(CallId, Args, PredId, !Info, !IO).
typecheck_aditi_builtin_2(CallId, Args,
		aditi_bulk_update(Update, _, Syntax),
		aditi_bulk_update(Update, PredId, Syntax), !Info, !IO) :-
	CallId = PredOrFunc - _,
	InsertDeleteAdjustArgTypes =
		(pred(RelationArgTypes::in, UpdateArgTypes::out) is det :-
			construct_higher_order_type((pure), PredOrFunc,
				(aditi_bottom_up), RelationArgTypes,
				ClosureType),
			UpdateArgTypes = [ClosureType]
	),

	% `aditi_modify' takes a closure which takes two sets of arguments
	% corresponding to those of the base relation, one set for
	% the tuple to delete, and one for the tuple to insert.
	ModifyAdjustArgTypes =
		(pred(RelationArgTypes::in, AditiModifyTypes::out) is det :-
			list__append(RelationArgTypes, RelationArgTypes,
				ClosureArgTypes),
			construct_higher_order_pred_type((pure),
				(aditi_bottom_up), ClosureArgTypes,
				ClosureType),
			AditiModifyTypes = [ClosureType]
	),
	(
		Update = bulk_insert,
		AdjustArgTypes = InsertDeleteAdjustArgTypes
	;
		Update = bulk_delete,
		AdjustArgTypes = InsertDeleteAdjustArgTypes
	;
		Update = bulk_modify,
		AdjustArgTypes = ModifyAdjustArgTypes
	),
	typecheck_aditi_builtin_closure(CallId, Args, AdjustArgTypes, PredId,
		!Info, !IO).

	% Check that there is only one argument (other than the `aditi__state'
	% arguments) passed to an `aditi_delete', `aditi_bulk_insert',
	% `aditi_bulk_delete' or `aditi_modify', then typecheck that argument.
:- pred typecheck_aditi_builtin_closure(simple_call_id::in,
	list(prog_var)::in, adjust_arg_types::in(adjust_arg_types),
	pred_id::out, typecheck_info::in, typecheck_info::out,
	io::di, io::uo) is det.

typecheck_aditi_builtin_closure(CallId, OtherArgs, AdjustArgTypes, PredId,
		!Info, !IO) :-
	( OtherArgs = [HOArg] ->
		typecheck_call_pred_adjust_arg_types(CallId, [HOArg],
			AdjustArgTypes, PredId, !Info, !IO)
	;
		% An error should have been reported by make_hlds.m.
		error("typecheck_aditi_builtin: " ++
			"incorrect arity for builtin")
	).

	% Typecheck the DCG state arguments in the argument
	% list of an Aditi builtin.
:- pred typecheck_aditi_state_args(aditi_builtin::in, simple_call_id::in,
	prog_var::in, prog_var::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_aditi_state_args(Builtin, CallId, AditiState0Var, AditiStateVar,
		!Info, !IO) :-
	StateType = aditi_state_type,
	typecheck_var_has_type_list([AditiState0Var, AditiStateVar],
		[StateType, StateType],
		aditi_builtin_first_state_arg(Builtin, CallId), !Info, !IO).

	% Return the index in the argument list of the first
	% `aditi__state' DCG argument.
:- func aditi_builtin_first_state_arg(aditi_builtin, simple_call_id) = int.

aditi_builtin_first_state_arg(aditi_tuple_update(_, _),
		_ - _/Arity) = Arity + 1.
	% XXX removing the space between the 2 and the `.' will possibly
	% cause lexing to fail as io__putback_char will be called twice
	% in succession in lexer__get_int_dot.
aditi_builtin_first_state_arg(aditi_bulk_update(_, _, _), _) = 2 .

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(simple_call_id::in, list(prog_var)::in,
	pred_id::out, typecheck_info::in, typecheck_info::out,
	io::di, io::uo) is det.

typecheck_call_pred(CallId, Args, PredId, !Info, !IO) :-
	typecheck_call_pred_adjust_arg_types(CallId, Args, assign, PredId,
		!Info, !IO).

	% A closure of this type performs a transformation on
	% the argument types of the called predicate. It is used to
	% convert the argument types of the base relation for an Aditi
	% update builtin to the type of the higher-order argument of
	% the update predicate. For an ordinary predicate call,
	% the types are not transformed.
:- type adjust_arg_types == pred(list(type), list(type)).
:- inst adjust_arg_types == (pred(in, out) is det).

	% Typecheck a predicate, performing the given transformation on the
	% argument types.
:- pred typecheck_call_pred_adjust_arg_types(simple_call_id::in,
	list(prog_var)::in, adjust_arg_types::in(adjust_arg_types),
	pred_id::out, typecheck_info::in, typecheck_info::out,
	io::di, io::uo) is det.

typecheck_call_pred_adjust_arg_types(CallId, Args, AdjustArgTypes, PredId,
		!Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),

		% look up the called predicate's arg types
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		CallId = PorF - SymName/Arity,
		predicate_table_search_pf_sym_arity(PredicateTable,
			calls_are_fully_qualified(!.Info ^ pred_markers),
			PorF, SymName, Arity, PredIdList)
	->
		% handle the case of a non-overloaded predicate specially
		% (so that we can optimize the case of a non-overloaded,
		% non-polymorphic predicate)
		( PredIdList = [PredId0] ->
			PredId = PredId0,
			typecheck_call_pred_id_adjust_arg_types(PredId, Args,
				AdjustArgTypes, !Info, !IO)
		;
			typecheck_call_overloaded_pred(PredIdList, Args,
				AdjustArgTypes, !Info, !IO),

			%
			% In general, we can't figure out which
			% predicate it is until after we have
			% resolved any overloading, which may
			% require type-checking the entire clause.
			% Hence, for the moment, we just record an
			% invalid pred_id in the HLDS.  This will be
			% rectified by modes.m during mode-checking;
			% at that point, enough information is
			% available to determine which predicate it is.
			%
			PredId = invalid_pred_id
		),

			% Arguably, we could do context reduction at
			% a different point. See the paper:
			% "Type classes: an exploration of the design
			% space", S. Peyton-Jones, M. Jones 1997.
			% for a discussion of some of the issues.
		perform_context_reduction(OrigTypeAssignSet, !Info, !IO)

	;
		PredId = invalid_pred_id,
		report_pred_call_error(CallId, !Info, !IO)
	).

	% Typecheck a call to a specific predicate.
:- pred typecheck_call_pred_id(pred_id::in, list(prog_var)::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_call_pred_id(PredId, Args, !Info, !IO) :-
	typecheck_call_pred_id_adjust_arg_types(PredId, Args, assign, !Info,
		!IO).

	% Typecheck a call to a specific predicate, performing the given
	% transformation on the argument types.
:- pred typecheck_call_pred_id_adjust_arg_types(pred_id::in,
	list(prog_var)::in, adjust_arg_types::in(adjust_arg_types),
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_call_pred_id_adjust_arg_types(PredId, Args, AdjustArgTypes, !Info,
		!IO) :-
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
		PredArgTypes0),
	AdjustArgTypes(PredArgTypes0, PredArgTypes),
	pred_info_get_class_context(PredInfo, PredClassContext),
	%
	% rename apart the type variables in
	% called predicate's arg types and then
	% unify the types of the call arguments
	% with the called predicates' arg types
	% (optimize for the common case of
	% a non-polymorphic, non-constrained predicate)
	%
	(
		varset__is_empty(PredTypeVarSet),
		PredClassContext = constraints([], [])
	->
		typecheck_var_has_type_list(Args, PredArgTypes, 1, !Info, !IO)
	;
		typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet,
			PredExistQVars, PredArgTypes, PredClassContext, !Info,
			!IO)
	).

:- pred report_pred_call_error(simple_call_id::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

report_pred_call_error(PredCallId, !Info, !IO) :-
	PredCallId = PredOrFunc0 - SymName/_Arity,
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		predicate_table_search_pf_sym(PredicateTable,
			calls_are_fully_qualified(!.Info ^ pred_markers),
			PredOrFunc0, SymName, OtherIds),
		predicate_table_get_preds(PredicateTable, Preds),
		OtherIds \= []
	->
		typecheck_find_arities(Preds, OtherIds, Arities),
		report_error_pred_num_args(!.Info, PredCallId, Arities, !IO)
	;
		( PredOrFunc0 = predicate, PredOrFunc = function
		; PredOrFunc0 = function, PredOrFunc = predicate
		),
		predicate_table_search_pf_sym(PredicateTable,
			calls_are_fully_qualified(!.Info ^ pred_markers),
			PredOrFunc, SymName, OtherIds),
		OtherIds \= []
	->
		report_error_func_instead_of_pred(!.Info, PredOrFunc,
			PredCallId, !IO)
	;
		report_error_undef_pred(!.Info, PredCallId, !IO)
	),
	typecheck_info_set_found_error(yes, !Info).

:- pred typecheck_find_arities(pred_table::in, list(pred_id)::in,
	list(int)::out) is det.

typecheck_find_arities(_, [], []).
typecheck_find_arities(Preds, [PredId | PredIds], [Arity | Arities]) :-
	map__lookup(Preds, PredId, PredInfo),
	Arity = pred_info_orig_arity(PredInfo),
	typecheck_find_arities(Preds, PredIds, Arities).

:- pred typecheck_call_overloaded_pred(list(pred_id)::in, list(prog_var)::in,
	adjust_arg_types::in(adjust_arg_types),
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_call_overloaded_pred(PredIdList, Args, AdjustArgTypes, !Info, !IO) :-
	%
	% let the new arg_type_assign_set be the cross-product
	% of the current type_assign_set and the set of possible
	% lists of argument types for the overloaded predicate,
	% suitable renamed apart
	%
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
	get_overloaded_pred_arg_types(PredIdList, Preds, AdjustArgTypes,
		TypeAssignSet0, [], ArgsTypeAssignSet),
	%
	% then unify the types of the call arguments with the
	% called predicates' arg types
	%
	typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, !Info,
		!IO).

:- pred get_overloaded_pred_arg_types(list(pred_id)::in, pred_table::in,
	adjust_arg_types::in(adjust_arg_types), type_assign_set::in,
	args_type_assign_set::in, args_type_assign_set::out) is det.

get_overloaded_pred_arg_types([], _Preds, _AdjustArgTypes, _TypeAssignSet0,
		!ArgsTypeAssignSet).
get_overloaded_pred_arg_types([PredId | PredIds], Preds, AdjustArgTypes,
		TypeAssignSet0, !ArgsTypeAssignSet) :-
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
		PredArgTypes0),
	call(AdjustArgTypes, PredArgTypes0, PredArgTypes),
	pred_info_get_class_context(PredInfo, PredClassContext),
	rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
		PredArgTypes, PredClassContext, !ArgsTypeAssignSet),
	get_overloaded_pred_arg_types(PredIds, Preds, AdjustArgTypes,
		TypeAssignSet0, !ArgsTypeAssignSet).

%-----------------------------------------------------------------------------%

	% Note: calls to preds declared in `.opt' files should always be
	% module qualified, so they should not be considered
	% when resolving overloading.

typecheck__resolve_pred_overloading(ModuleInfo, CallerMarkers,
		ArgTypes, TVarSet, PredName0, PredName, PredId) :-
	module_info_get_predicate_table(ModuleInfo, PredTable),
	(
		predicate_table_search_pred_sym(PredTable,
			calls_are_fully_qualified(CallerMarkers),
			PredName0, PredIds0)
	->
		PredIds = PredIds0
	;
		PredIds = []
	),

	%
	% Check if there any of the candidate pred_ids
	% have argument/return types which subsume the actual
	% argument/return types of this function call
	%
	(
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, PredId1, PredName1)
	->
		PredId = PredId1,
		PredName = PredName1
	;
		% if there is no matching predicate for this call,
		% then this predicate must have a type error which
		% should have been caught by typechecking.
		error("type error in pred call: no matching pred")
	).

typecheck__find_matching_pred_id([PredId | PredIds], ModuleInfo,
		TVarSet, ArgTypes, ThePredId, PredName) :-
	(
		%
		% lookup the argument types of the candidate predicate
		% (or the argument types + return type of the candidate
		% function)
		%
		module_info_pred_info(ModuleInfo, PredId, PredInfo),
		pred_info_arg_types(PredInfo, PredTVarSet, PredExistQVars0,
			PredArgTypes0),

		arg_type_list_subsumes(TVarSet, ArgTypes,
			PredTVarSet, PredExistQVars0, PredArgTypes0)
	->
		%
		% we've found a matching predicate
		% was there was more than one matching predicate/function?
		%
		PName = pred_info_name(PredInfo),
		Module = pred_info_module(PredInfo),
		PredName = qualified(Module, PName),
		(
			typecheck__find_matching_pred_id(PredIds,
				ModuleInfo, TVarSet, ArgTypes, _OtherPredId, _)
		->
			% XXX this should report an error properly, not
			% via error/1
			error("Type error in predicate call: " ++
				"unresolvable predicate overloading. " ++
				"You need to use an explicit " ++
				"module qualifier. " ++
				"Compile with -V to find out where.")
		;
			ThePredId = PredId
		)
	;
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, ThePredId, PredName)
	).

%-----------------------------------------------------------------------------%

	% Rename apart the type variables in called predicate's arg types
	% separately for each type assignment, resulting in an "arg type
	% assignment set", and then for each arg type assignment in the
	% arg type assignment set, check that the argument variables have
	% the expected types.
	% A set of class constraints are also passed in, which must have the
	% types contained within renamed apart.

:- pred typecheck_var_has_polymorphic_type_list(list(prog_var)::in,
	tvarset::in, existq_tvars::in, list(type)::in, class_constraints::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet, PredExistQVars,
		PredArgTypes, PredClassConstraints, !Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
	rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
		PredArgTypes, PredClassConstraints, [], ArgsTypeAssignSet),
	typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, !Info,
		!IO).

:- pred rename_apart(type_assign_set::in, tvarset::in, existq_tvars::in,
	list(type)::in, class_constraints::in,
	args_type_assign_set::in, args_type_assign_set::out) is det.

rename_apart([], _, _, _, _, !ArgTypeAssigns).
rename_apart([TypeAssign0 | TypeAssigns0], PredTypeVarSet, PredExistQVars0,
		PredArgTypes0, PredClassConstraints0, !ArgTypeAssigns) :-
	%
	% rename everything apart
	%
	type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
		TypeAssign1, PredArgTypes, Subst),
	apply_substitution_to_var_list(PredExistQVars0, Subst, PredExistQVars),
	apply_subst_to_constraints(Subst, PredClassConstraints0,
		PredClassConstraints),

	%
	% insert the existentially quantified type variables for the called
	% predicate into HeadTypeParams (which holds the set of type
	% variables which the caller is not allowed to bind).
	%
	type_assign_get_head_type_params(TypeAssign1, HeadTypeParams0),
	list__append(PredExistQVars, HeadTypeParams0, HeadTypeParams),
	type_assign_set_head_type_params(HeadTypeParams,
		TypeAssign1, TypeAssign),
	%
	% save the results and recurse
	%
	NewArgTypeAssign = args(TypeAssign, PredArgTypes,
		PredClassConstraints),
	!:ArgTypeAssigns = [NewArgTypeAssign | !.ArgTypeAssigns],
	rename_apart(TypeAssigns0, PredTypeVarSet, PredExistQVars0,
		PredArgTypes0, PredClassConstraints0, !ArgTypeAssigns).

:- pred type_assign_rename_apart(type_assign::in, tvarset::in, list(type)::in,
	type_assign::out, list(type)::out, tsubst::out) is det.

type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
		TypeAssign, PredArgTypes, Subst) :-
	type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
	varset__merge_subst(TypeVarSet0, PredTypeVarSet, TypeVarSet,
		Subst),
	term__apply_substitution_to_list(PredArgTypes0, Subst,
		PredArgTypes),
	type_assign_set_typevarset(TypeVarSet, TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_arg_type_list(list(prog_var)::in, int::in,
	args_type_assign_set::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_arg_type_list([], _, ArgTypeAssignSet, !Info, !IO) :-
	convert_nonempty_args_type_assign_set(ArgTypeAssignSet, TypeAssignSet),
	typecheck_info_set_type_assign_set(TypeAssignSet, !Info).

typecheck_var_has_arg_type_list([Var | Vars], ArgNum, ArgTypeAssignSet0, !Info,
		!IO) :-
	typecheck_info_set_arg_num(ArgNum, !Info),
	typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet1,
		!Info, !IO),
	typecheck_var_has_arg_type_list(Vars, ArgNum + 1, ArgTypeAssignSet1,
		!Info, !IO).

:- pred convert_nonempty_args_type_assign_set(args_type_assign_set::in,
	type_assign_set::out) is det.

convert_nonempty_args_type_assign_set([], []).
convert_nonempty_args_type_assign_set([ArgTypeAssign | ArgTypeAssigns],
		[TypeAssign | TypeAssigns]) :-
	ArgTypeAssign = args(_, Args, _),
	( Args = [] ->
		convert_args_type_assign(ArgTypeAssign, TypeAssign)
	;
		% this should never happen, since the arguments should
		% all have been processed at this point
		error("convert_nonempty_args_type_assign_set")
	),
	convert_nonempty_args_type_assign_set(ArgTypeAssigns, TypeAssigns).

:- pred convert_args_type_assign_set(args_type_assign_set::in,
	type_assign_set::out) is det.

	% Same as convert_nonempty_args_type_assign_set, but does not abort
	% when the args are empty
convert_args_type_assign_set([], []).
convert_args_type_assign_set([X | Xs], [Y | Ys]) :-
	convert_args_type_assign(X, Y),
	convert_args_type_assign_set(Xs, Ys).

:- pred convert_args_type_assign(args_type_assign::in, type_assign::out)
	is det.

convert_args_type_assign(args(TypeAssign0, _, Constraints0), TypeAssign) :-
	type_assign_get_typeclass_constraints(TypeAssign0, OldConstraints),
	type_assign_get_type_bindings(TypeAssign0, Bindings),
	apply_rec_subst_to_constraints(Bindings, Constraints0, Constraints),
	add_constraints(OldConstraints, Constraints, NewConstraints),
	type_assign_set_typeclass_constraints(NewConstraints,
		TypeAssign0, TypeAssign).

:- pred typecheck_var_has_arg_type(prog_var::in,
	args_type_assign_set::in, args_type_assign_set::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet, !Info,
		!IO) :-
	typecheck_var_has_arg_type_2(ArgTypeAssignSet0,
		Var, [], ArgTypeAssignSet1),
	(
		ArgTypeAssignSet1 = [],
		ArgTypeAssignSet0 \= []
	->
		skip_arg(ArgTypeAssignSet0, ArgTypeAssignSet),
		report_error_arg_var(!.Info, Var, ArgTypeAssignSet0, !IO),
		typecheck_info_set_found_error(yes, !Info)
	;
		ArgTypeAssignSet = ArgTypeAssignSet1
	).

:- pred skip_arg(args_type_assign_set::in, args_type_assign_set::out) is det.

skip_arg([], []).
skip_arg([ArgTypeAssign0 | ArgTypeAssigns0],
		[ArgTypeAssign | ArgTypeAssigns]) :-
	ArgTypeAssign0 = args(TypeAssign, Args0, Constraints),
	( Args0 = [_ | Args1] ->
		Args = Args1
	;
		% this should never happen
		error("typecheck.m: skip_arg")
	),
	ArgTypeAssign = args(TypeAssign, Args, Constraints),
	skip_arg(ArgTypeAssigns0, ArgTypeAssigns).

:- pred typecheck_var_has_arg_type_2(args_type_assign_set::in, prog_var::in,
	args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_var_has_arg_type_2([], _, !ArgTypeAssignSet).
typecheck_var_has_arg_type_2([ArgsTypeAssign | ArgsTypeAssignSets], Var,
		!ArgsTypeAssignSet) :-
	ArgsTypeAssign = args(TypeAssign0, ArgTypes0, ClassContext),
	arg_type_assign_var_has_type(TypeAssign0, ArgTypes0,
		Var, ClassContext, !ArgsTypeAssignSet),
	typecheck_var_has_arg_type_2(ArgsTypeAssignSets, Var,
		!ArgsTypeAssignSet).

:- pred arg_type_assign_var_has_type(type_assign::in, list(type)::in,
	prog_var::in, class_constraints::in,
	args_type_assign_set::in, args_type_assign_set::out) is det.

arg_type_assign_var_has_type(TypeAssign0, ArgTypes0, Var, ClassContext,
		!ArgTypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( ArgTypes0 = [Type | ArgTypes] ->
		( map__search(VarTypes0, Var, VarType) ->
			(
				type_assign_unify_type(TypeAssign0, VarType,
					Type, TypeAssign1)
			->
				NewTypeAssign = args(TypeAssign1, ArgTypes,
					ClassContext),
				!:ArgTypeAssignSet =
					[NewTypeAssign | !.ArgTypeAssignSet]
			;
				true
			)
		;
			map__det_insert(VarTypes0, Var, Type, VarTypes),
			type_assign_set_var_types(VarTypes, TypeAssign0,
				TypeAssign),
			NewTypeAssign = args(TypeAssign, ArgTypes,
				ClassContext),
			!:ArgTypeAssignSet = [NewTypeAssign |
				!.ArgTypeAssignSet]
		)
	;
		error("arg_type_assign_var_has_type")
	).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_type_list(list(prog_var)::in, list(type)::in, int::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_type_list([], [_ | _], _, !Info, !IO) :-
	error("typecheck_var_has_type_list: length mismatch").
typecheck_var_has_type_list([_ | _], [], _, !Info, !IO) :-
	error("typecheck_var_has_type_list: length mismatch").
typecheck_var_has_type_list([], [], _, !Info, !IO).
typecheck_var_has_type_list([Var | Vars], [Type | Types], ArgNum, !Info, !IO) :-
	typecheck_info_set_arg_num(ArgNum, !Info),
	typecheck_var_has_type(Var, Type, !Info, !IO),
	typecheck_var_has_type_list(Vars, Types, ArgNum + 1, !Info, !IO).

:- pred typecheck_var_has_type(prog_var::in, (type)::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_var_has_type(Var, Type, !Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
	typecheck_var_has_type_2(TypeAssignSet0, Var, Type, [],
		TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		report_error_var(!.Info, Var, Type, TypeAssignSet0, !IO),
		typecheck_info_set_found_error(yes, !Info)
	;
		typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
	).

	% Given a type assignment set and a variable,
	% return the list of possible different types for the variable.

:- type type_stuff ---> type_stuff(type, tvarset, tsubst, head_type_params).

:- pred get_type_stuff(type_assign_set::in, prog_var::in,
	list(type_stuff)::out) is det.

get_type_stuff([], _Var, []).
get_type_stuff([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
	get_type_stuff(TypeAssigns, Var, TailTypeStuffs),
	type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	( map__search(VarTypes, Var, Type0) ->
		Type = Type0
	;
		% this shouldn't happen - how can a variable which has
		% not yet been assigned a type variable fail to have
		% the correct type?
		term__context_init(Context),
		Type = term__functor(term__atom("<any>"), [], Context)
	),
	TypeStuff = type_stuff(Type, TVarSet, TypeBindings, HeadTypeParams),
	( list__member(TypeStuff, TailTypeStuffs) ->
		TypeStuffs = TailTypeStuffs
	;
		TypeStuffs = [TypeStuff | TailTypeStuffs]
	).

	% Given an arg type assignment set and a variable id,
	% return the list of possible different types for the argument
	% and the variable.

:- type arg_type_stuff --->
	arg_type_stuff(type, type, tvarset, head_type_params).

:- pred get_arg_type_stuff(args_type_assign_set::in, prog_var::in,
	list(arg_type_stuff)::out) is det.

get_arg_type_stuff([], _Var, []).
get_arg_type_stuff([ArgTypeAssign | ArgTypeAssigns], Var, ArgTypeStuffs) :-
	ArgTypeAssign = args(TypeAssign, ArgTypes, _),
	get_arg_type_stuff(ArgTypeAssigns, Var, TailArgTypeStuffs),
	type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	( map__search(VarTypes, Var, VarType0) ->
		VarType = VarType0
	;
		% this shouldn't happen - how can a variable which has
		% not yet been assigned a type variable fail to have
		% the correct type?
		term__context_init(Context),
		VarType = term__functor(term__atom("<any>"), [], Context)
	),
	list__index0_det(ArgTypes, 0, ArgType),
	term__apply_rec_substitution(ArgType, TypeBindings, ArgType2),
	term__apply_rec_substitution(VarType, TypeBindings, VarType2),
	ArgTypeStuff = arg_type_stuff(ArgType2, VarType2, TVarSet,
		HeadTypeParams),
	( list__member(ArgTypeStuff, TailArgTypeStuffs) ->
		ArgTypeStuffs = TailArgTypeStuffs
	;
		ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
	).

:- type headtypes == list(tvar).

:- pred typecheck_var_has_type_2(type_assign_set::in, prog_var::in, (type)::in,
	type_assign_set::in, type_assign_set::out) is det.

typecheck_var_has_type_2([], _, _, !TypeAssignSet).
typecheck_var_has_type_2([TypeAssign0 | TypeAssignSet0], Var, Type,
		!TypeAssignSet) :-
	type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet),
	typecheck_var_has_type_2(TypeAssignSet0, Var, Type, !TypeAssignSet).

:- pred type_assign_var_has_type(type_assign::in, prog_var::in, (type)::in,
	type_assign_set::in, type_assign_set::out) is det.

type_assign_var_has_type(TypeAssign0, Var, Type, !TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( %%% if some [VarType]
		map__search(VarTypes0, Var, VarType)
	->
		( %%% if some [TypeAssign1]
			type_assign_unify_type(TypeAssign0, VarType, Type,
				TypeAssign1)
		->
			!:TypeAssignSet = [TypeAssign1 | !.TypeAssignSet]
		;
			!:TypeAssignSet = !.TypeAssignSet
		)
	;
		map__det_insert(VarTypes0, Var, Type, VarTypes),
		type_assign_set_var_types(VarTypes, TypeAssign0, TypeAssign),
		!:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
	).

%-----------------------------------------------------------------------------%

	% type_assign_var_has_type_list(Vars, Types, TypeAssign, Info,
	%		TypeAssignSet0, TypeAssignSet):
	% 	Let TAs = { TA | TA is an extension of TypeAssign
	%		for which the types of the Vars unify with
	%		their respective Types },
	% 	list__append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_var_has_type_list(list(prog_var)::in, list(type)::in,
	type_assign::in, typecheck_info::in,
	type_assign_set::in, type_assign_set::out) is det.

type_assign_var_has_type_list([], [_ | _], _, _, _, _) :-
	error("type_assign_var_has_type_list: length mis-match").
type_assign_var_has_type_list([_ | _], [], _, _, _, _) :-
	error("type_assign_var_has_type_list: length mis-match").
type_assign_var_has_type_list([], [], TypeAssign, _,
		TypeAssignSet, [TypeAssign | TypeAssignSet]).
type_assign_var_has_type_list([Arg | Args], [Type | Types], TypeAssign0,
		Info, TypeAssignSet0, TypeAssignSet) :-
	type_assign_var_has_type(TypeAssign0, Arg, Type, [], TypeAssignSet1),
	type_assign_list_var_has_type_list(TypeAssignSet1,
		Args, Types, Info, TypeAssignSet0, TypeAssignSet).

	% type_assign_list_var_has_type_list(TAs, Terms, Types,
	%		Info, TypeAssignSet0, TypeAssignSet):
	% 	Let TAs2 = { TA | TA is an extension of a member of TAs
	%		for which the types of the Terms unify with
	%		their respective Types },
	% 	list__append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_list_var_has_type_list(type_assign_set::in,
	list(prog_var)::in, list(type)::in, typecheck_info::in,
	type_assign_set::in, type_assign_set::out) is det.

type_assign_list_var_has_type_list([], _, _, _, !TypeAssignSet).
type_assign_list_var_has_type_list([TA | TAs], Args, Types, Info,
		!TypeAssignSet) :-
	type_assign_var_has_type_list(Args, Types, TA, Info, !TypeAssignSet),
	type_assign_list_var_has_type_list(TAs, Args, Types, Info,
		!TypeAssignSet).

%-----------------------------------------------------------------------------%

	% Because we allow overloading, type-checking is NP-complete.
	% Rather than disallow it completely, we issue a warning
	% whenever "too much" overloading is used.  "Too much"
	% is arbitrarily defined as anthing which results in
	% more than 50 possible type assignments.

:- pred check_warn_too_much_overloading(typecheck_info::in,
	typecheck_info::out, io::di, io::uo) is det.

check_warn_too_much_overloading(!Info, !IO) :-
	(
		typecheck_info_get_warned_about_overloading(!.Info,
			AlreadyWarned),
		AlreadyWarned = no,
		typecheck_info_get_type_assign_set(!.Info, TypeAssignSet),
		list__length(TypeAssignSet, Count),
		Count > 50
	->
		report_warning_too_much_overloading(!.Info, !IO),
		typecheck_info_set_warned_about_overloading(yes, !Info)
	;
		true
	).

%-----------------------------------------------------------------------------%

	% used for debugging

:- pred checkpoint(string::in, typecheck_info::in, typecheck_info::out,
	io::di, io::uo) is det.

checkpoint(Msg, !Info, !IO) :-
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, debug_types, DoCheckPoint),
	( DoCheckPoint = yes ->
		checkpoint_2(Msg, !.Info, !IO)
	;
		true
	).

:- pred checkpoint_2(string::in, typecheck_info::in, io::di, io::uo) is det.

checkpoint_2(Msg, T0, !IO) :-
	io__write_string("At ", !IO),
	io__write_string(Msg, !IO),
	io__write_string(": ", !IO),
	globals__io_lookup_bool_option(statistics, Statistics, !IO),
	maybe_report_stats(Statistics, !IO),
	io__write_string("\n", !IO),
	typecheck_info_get_type_assign_set(T0, TypeAssignSet),
	(
		Statistics = yes,
		TypeAssignSet = [TypeAssign | _]
	->
		type_assign_get_var_types(TypeAssign, VarTypes),
		checkpoint_tree_stats("\t`var -> type' map", VarTypes, !IO),
		type_assign_get_type_bindings(TypeAssign, TypeBindings),
		checkpoint_tree_stats("\t`type var -> type' map", TypeBindings,
			!IO)
	;
		true
	),
	typecheck_info_get_varset(T0, VarSet),
	write_type_assign_set(TypeAssignSet, VarSet, !IO).

:- pred checkpoint_tree_stats(string::in, map(_K, _V)::in, io::di, io::uo)
	is det.

checkpoint_tree_stats(Description, Tree, !IO) :-
	map__count(Tree, Count),
	io__write_string(Description, !IO),
	io__write_string(": count = ", !IO),
	io__write_int(Count, !IO),
	io__write_string("\n", !IO).

%-----------------------------------------------------------------------------%

	% Type check a unification.
	% Get the type assignment set from the type info and then just
	% iterate over all the possible type assignments.

:- pred typecheck_unification(prog_var::in, unify_rhs::in, unify_rhs::out,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_unification(X, var(Y), var(Y), !Info, !IO) :-
	typecheck_unify_var_var(X, Y, !Info, !IO).
typecheck_unification(X, functor(F, E, As), functor(F, E, As), !Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, OrigTypeAssignSet),
	typecheck_unify_var_functor(X, F, As, !Info, !IO),
	perform_context_reduction(OrigTypeAssignSet, !Info, !IO).
typecheck_unification(X,
		lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
			NonLocals, Vars, Modes, Det, Goal0),
		lambda_goal(Purity, PredOrFunc, EvalMethod, FixModes,
			NonLocals, Vars, Modes, Det, Goal), !Info, !IO) :-
 	typecheck_lambda_var_has_type(Purity, PredOrFunc, EvalMethod, X, Vars,
		!Info, !IO),
	typecheck_goal(Goal0, Goal, !Info, !IO).

:- pred typecheck_unify_var_var(prog_var::in, prog_var::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_unify_var_var(X, Y, !Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
	typecheck_unify_var_var_2(TypeAssignSet0, X, Y, [], TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		report_error_unif_var_var(!.Info, X, Y, TypeAssignSet0, !IO),
		typecheck_info_set_found_error(yes, !Info)
	;
		typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
	).

:- pred typecheck_unify_var_functor(prog_var::in, cons_id::in,
	list(prog_var)::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_unify_var_functor(Var, Functor, Args, !Info, !IO) :-
	%
	% get the list of possible constructors that match this functor/arity
	% if there aren't any, report an undefined constructor error
	%
	list__length(Args, Arity),
	typecheck_info_get_ctor_list(!.Info, Functor, Arity,
		ConsDefnList, InvalidConsDefnList),
	( ConsDefnList = [] ->
		report_error_undef_cons(!.Info, InvalidConsDefnList,
			Functor, Arity, !IO),
		typecheck_info_set_found_error(yes, !Info)
	;
		%
		% produce the ConsTypeAssignSet, which is essentially the
		% cross-product of the TypeAssignSet0 and the ConsDefnList
		%
		typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
		typecheck_unify_var_functor_get_ctors(TypeAssignSet0,
			!.Info, ConsDefnList, [], ConsTypeAssignSet),
		(
			ConsTypeAssignSet = [],
			TypeAssignSet0 \= []
		->
			% this should never happen, since undefined ctors
			% should be caught by the check just above
			error("typecheck_unify_var_functor: undefined cons?")
		;
			true
		),

		%
		% check that the type of the functor matches the type
		% of the variable
		%
		typecheck_functor_type(ConsTypeAssignSet, Var, [],
			ArgsTypeAssignSet),
		(
			ArgsTypeAssignSet = [],
			ConsTypeAssignSet \= []
		->
			report_error_functor_type(!.Info, Var, ConsDefnList,
				Functor, Arity, TypeAssignSet0, !IO),
			typecheck_info_set_found_error(yes, !Info)
		;
			true
		),

		%
		% check that the type of the arguments of the functor matches
		% their expected type for this functor
		%
		typecheck_functor_arg_types( ArgsTypeAssignSet, Args, !.Info,
			[], TypeAssignSet),
		(
			TypeAssignSet = [],
			ArgsTypeAssignSet \= []
		->
			report_error_functor_arg_types(!.Info, Var,
				ConsDefnList, Functor, Args,
				ArgsTypeAssignSet, !IO),
			typecheck_info_set_found_error(yes, !Info)
		;
			true
		),
		%
		% if we encountered an error, continue checking with the
		% original type assign set
		%
		( TypeAssignSet = [] ->
			typecheck_info_set_type_assign_set(TypeAssignSet0,
				!Info)
		;
			typecheck_info_set_type_assign_set(TypeAssignSet,
				!Info)
		)
	).

	% typecheck_unify_var_functor_get_ctors(TypeAssignSet, Info,
	%	ConsDefns):
	%
	% Iterate over all the different possible type assignments and
	% constructor definitions.
	% For each type assignment in `TypeAssignSet', and constructor
	% definition in `ConsDefns', produce a pair
	%
	%	TypeAssign - cons_type(Type, ArgTypes)
	%
	% where `cons_type(Type, ArgTypes)' records one of the possible
	% types for the constructor in `ConsDefns', and where `TypeAssign' is
	% the type assignment renamed apart from the types of the constructors.

:- type cons_type ---> cons_type(type, list(type)).
:- type cons_type_assign_set == list(pair(type_assign, cons_type)).

:- type args_type_assign_set == list(args_type_assign).

:- type args_type_assign
	--->	args(
			caller_arg_assign	:: type_assign,
						% Type assignment,
			callee_arg_types	:: list(type),
						% types of callee args,
			callee_constraints	:: class_constraints
						% constraints from callee
		).

:- func get_caller_arg_assign(args_type_assign) = type_assign.
:- func get_callee_arg_types(args_type_assign) = list(type).

get_caller_arg_assign(ArgsTypeAssign) = ArgsTypeAssign ^ caller_arg_assign.
get_callee_arg_types(ArgsTypeAssign) = ArgsTypeAssign ^ callee_arg_types.

:- pred typecheck_unify_var_functor_get_ctors(type_assign_set::in,
	typecheck_info::in, list(cons_type_info)::in,
	cons_type_assign_set::in, cons_type_assign_set::out) is det.

	% Iterate over the type assign sets

typecheck_unify_var_functor_get_ctors([], _, _, !TypeAssignSet).
typecheck_unify_var_functor_get_ctors([TypeAssign | TypeAssigns], Info,
		ConsDefns, !TypeAssignSet) :-
	typecheck_unify_var_functor_get_ctors_2(ConsDefns, Info, TypeAssign,
		!TypeAssignSet),
	typecheck_unify_var_functor_get_ctors(TypeAssigns, Info, ConsDefns,
		!TypeAssignSet).

	% Iterate over all the different cons defns.

:- pred typecheck_unify_var_functor_get_ctors_2(list(cons_type_info)::in,
	typecheck_info::in, type_assign::in,
	cons_type_assign_set::in, cons_type_assign_set::out) is det.

typecheck_unify_var_functor_get_ctors_2([], _, _, !ConsTypeAssignSet).
typecheck_unify_var_functor_get_ctors_2([ConsDefn | ConsDefns], Info,
		TypeAssign0, !ConsTypeAssignSet) :-
	get_cons_stuff(ConsDefn, TypeAssign0, Info, ConsType, ArgTypes,
		TypeAssign1),
	list__append([TypeAssign1 - cons_type(ConsType, ArgTypes)],
		!ConsTypeAssignSet),
	typecheck_unify_var_functor_get_ctors_2(ConsDefns, Info, TypeAssign0,
		!ConsTypeAssignSet).

	% typecheck_functor_type(ConsTypeAssignSet, Var):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor type,
	% check that the type of `Var' matches this type.

:- pred typecheck_functor_type(cons_type_assign_set::in, prog_var::in,
	args_type_assign_set::in, args_type_assign_set::out) is det.

typecheck_functor_type([], _, !ArgsTypeAssignSet).
typecheck_functor_type([TypeAssign - ConsType | ConsTypeAssigns], Var,
		!ArgsTypeAssignSet) :-
	ConsType = cons_type(Type, ArgTypes),
	type_assign_check_functor_type(Type, ArgTypes, Var, TypeAssign,
		!ArgsTypeAssignSet),
	typecheck_functor_type(ConsTypeAssigns, Var, !ArgsTypeAssignSet).

	% typecheck_functor_arg_types(ConsTypeAssignSet, Var, Args, ...):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor argument types,
	% check that the types of `Args' matches these types.

:- pred typecheck_functor_arg_types(args_type_assign_set::in,
	list(prog_var)::in, typecheck_info::in,
	type_assign_set::in, type_assign_set::out) is det.

typecheck_functor_arg_types([], _, _, !TypeAssignSet).
typecheck_functor_arg_types([ConsTypeAssign | ConsTypeAssigns], Args, Info,
		!TypeAssignSet) :-
	ConsTypeAssign = args(TypeAssign, ArgTypes, _),
	type_assign_var_has_type_list(Args, ArgTypes, TypeAssign, Info,
		!TypeAssignSet),
	typecheck_functor_arg_types(ConsTypeAssigns, Args, Info,
		!TypeAssignSet).

	% iterate over all the possible type assignments.

:- pred typecheck_unify_var_var_2(type_assign_set::in,
	prog_var::in, prog_var::in,
	type_assign_set::in, type_assign_set::out) is det.

typecheck_unify_var_var_2([], _, _, !TypeAssignSet).
typecheck_unify_var_var_2([TypeAssign0 | TypeAssigns0], X, Y,
		!TypeAssignSet) :-
	type_assign_unify_var_var(X, Y, TypeAssign0, !TypeAssignSet),
	typecheck_unify_var_var_2(TypeAssigns0, X, Y, !TypeAssignSet).

%-----------------------------------------------------------------------------%

	% Type-check the unification of two variables,
	% and update the type assignment.
	% TypeAssign0 is the type assignment we are updating,
	% TypeAssignSet0 is an accumulator for the list of possible
	% type assignments so far, and TypeAssignSet is TypeAssignSet plus
	% any type assignment(s) resulting from TypeAssign0 and this
	% unification.

:- pred type_assign_unify_var_var(prog_var::in, prog_var::in, type_assign::in,
	type_assign_set::in, type_assign_set::out) is det.

type_assign_unify_var_var(X, Y, TypeAssign0, !TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( map__search(VarTypes0, X, TypeX) ->
		( map__search(VarTypes0, Y, TypeY) ->
			% both X and Y already have types - just
			% unify their types
			(
				type_assign_unify_type(TypeAssign0,
					TypeX, TypeY, TypeAssign3)
			->
				!:TypeAssignSet = [TypeAssign3 |
					!.TypeAssignSet]
			;
				!:TypeAssignSet = !.TypeAssignSet
			)
		;
			% Y is a fresh variable which hasn't been
			% assigned a type yet
			map__det_insert(VarTypes0, Y, TypeX, VarTypes),
			type_assign_set_var_types(VarTypes, TypeAssign0,
				TypeAssign),
			!:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
		)
	;
		( map__search(VarTypes0, Y, TypeY) ->
			% X is a fresh variable which hasn't been
			% assigned a type yet
			map__det_insert(VarTypes0, X, TypeY, VarTypes),
			type_assign_set_var_types(VarTypes,
				TypeAssign0, TypeAssign),
			!:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
		;
			% both X and Y are fresh variables -
			% introduce a fresh type variable to represent
			% their type
			type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
			varset__new_var(TypeVarSet0, TypeVar, TypeVarSet),
			type_assign_set_typevarset(TypeVarSet,
				TypeAssign0, TypeAssign1),
			Type = term__variable(TypeVar),
			map__det_insert(VarTypes0, X, Type, VarTypes1),
			( X \= Y ->
				map__det_insert(VarTypes1, Y, Type, VarTypes)
			;
				VarTypes = VarTypes1
			),
			type_assign_set_var_types(VarTypes, TypeAssign1,
				TypeAssign),
			!:TypeAssignSet = [TypeAssign | !.TypeAssignSet]
		)
	).

%-----------------------------------------------------------------------------%

:- pred type_assign_check_functor_type((type)::in, list(type)::in,
	prog_var::in, type_assign::in,
	args_type_assign_set::in, args_type_assign_set::out) is det.

type_assign_check_functor_type(ConsType, ArgTypes, Y, TypeAssign1,
		!TypeAssignSet) :-

		% unify the type of Var with the type of the constructor
	type_assign_get_var_types(TypeAssign1, VarTypes0),
	( %%% if some [TypeY]
		map__search(VarTypes0, Y, TypeY)
	->
		( %%% if some [TypeAssign2]
			type_assign_unify_type(TypeAssign1, ConsType, TypeY,
				TypeAssign2)
		->
				% The constraints are empty here because
				% none are added by unification with a
				% functor
			Constraints = constraints([], []),
			ArgsTypeAssign = args(TypeAssign2, ArgTypes,
				Constraints),
			!:TypeAssignSet = [ArgsTypeAssign | !.TypeAssignSet]
		;
			true
		)
	;
			% The constraints are empty here because
			% none are added by unification with a
			% functor
		map__det_insert(VarTypes0, Y, ConsType, VarTypes),
		type_assign_set_var_types(VarTypes, TypeAssign1, TypeAssign3),
		Constraints = constraints([], []),
		ArgsTypeAssign = args(TypeAssign3, ArgTypes, Constraints),
		!:TypeAssignSet = [ArgsTypeAssign | !.TypeAssignSet]
	).

%-----------------------------------------------------------------------------%

	% Given an cons_type_info, construct a type for the
	% constructor and a list of types of the arguments,
	% suitable renamed apart from the current type_assign's
	% typevarset.

:- pred get_cons_stuff(cons_type_info::in, type_assign::in, typecheck_info::in,
	(type)::out, list(type)::out, type_assign::out) is det.

get_cons_stuff(ConsDefn, TypeAssign0, _Info, ConsType, ArgTypes, TypeAssign) :-
	ConsDefn = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
		ConsType0, ArgTypes0, ClassConstraints0),

	% Rename apart the type vars in the type of the constructor
	% and the types of its arguments.
	% (Optimize the common case of a non-polymorphic type)

	( varset__is_empty(ConsTypeVarSet) ->
		ConsType = ConsType0,
		ArgTypes = ArgTypes0,
		TypeAssign2 = TypeAssign0,
		ConstraintsToAdd = ClassConstraints0
	;
		type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
			[ConsType0 | ArgTypes0],
			TypeAssign1, [ConsType1 | ArgTypes1], Subst)
	->
		apply_substitution_to_var_list(ConsExistQVars0, Subst,
			ConsExistQVars),
		apply_subst_to_constraints(Subst, ClassConstraints0,
			ConstraintsToAdd),
		type_assign_get_head_type_params(TypeAssign1, HeadTypeParams0),
		list__append(ConsExistQVars, HeadTypeParams0, HeadTypeParams),
		type_assign_set_head_type_params(HeadTypeParams, TypeAssign1,
			TypeAssign2),

		ConsType = ConsType1,
		ArgTypes = ArgTypes1
	;
		error("get_cons_stuff: type_assign_rename_apart failed")
	),
		%
		% Add the constraints for this functor to the current
		% constraint set.  Note that there can still be (ground)
		% constraints even if the varset is empty.
		%
		% For functors which are data constructors, the fact that we
		% don't take the dual corresponds to assuming that they will
		% be used as deconstructors rather than as constructors.
		%
	type_assign_get_typeclass_constraints(TypeAssign2, OldConstraints),
	add_constraints(OldConstraints, ConstraintsToAdd, ClassConstraints),
	type_assign_set_typeclass_constraints(ClassConstraints, TypeAssign2,
		TypeAssign).

	%
	% compute the dual of a set of constraints:
	% anything which we can assume in the caller
	% is something that we must prove in the callee,
	% and vice versa
	%
:- pred dual_constraints(class_constraints::in, class_constraints::out) is det.

dual_constraints(constraints(Univs, Exists), constraints(Exists, Univs)).

	% add_constraints(Cs0, CsToAdd, Cs) :-
	% add the specified constraints to the current constraint set
	%
:- pred add_constraints(class_constraints::in, class_constraints::in,
	class_constraints::out) is det.

add_constraints(Constraints0, CsToAdd, Constraints) :-
	Constraints0 = constraints(UnivCs0, ExistCs0),
	CsToAdd = constraints(UnivCsToAdd, ExistCsToAdd),
	list__append(UnivCsToAdd, UnivCs0, UnivCs),
	list__append(ExistCsToAdd, ExistCs0, ExistCs),
	Constraints = constraints(UnivCs, ExistCs).

:- pred apply_substitution_to_var_list(list(var(T))::in, map(var(T),
	term(T))::in, list(var(T))::out) is det.

apply_substitution_to_var_list(Vars0, RenameSubst, Vars) :-
	term__var_list_to_term_list(Vars0, Terms0),
	term__apply_substitution_to_list(Terms0, RenameSubst, Terms),
	term__term_list_to_var_list(Terms, Vars).

:- pred apply_var_renaming_to_var_list(list(var(T))::in, map(var(T),
	var(T))::in, list(var(T))::out) is det.

apply_var_renaming_to_var_list(Vars0, RenameSubst, Vars) :-
	list__map(apply_var_renaming_to_var(RenameSubst), Vars0, Vars).

:- pred apply_var_renaming_to_var(map(var(T), var(T))::in, var(T)::in,
	var(T)::out) is det.

apply_var_renaming_to_var(RenameSubst, Var0, Var) :-
	( map__search(RenameSubst, Var0, Var1) ->
		Var = Var1
	;
		Var = Var0
	).

%-----------------------------------------------------------------------------%

	% typecheck_lambda_var_has_type(Var, ArgVars, ...)
	% checks that `Var' has type `pred(T1, T2, ...)' where
	% T1, T2, ... are the types of the `ArgVars'.

:- pred typecheck_lambda_var_has_type(purity::in, pred_or_func::in,
	lambda_eval_method::in, prog_var::in, list(prog_var)::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

typecheck_lambda_var_has_type(Purity, PredOrFunc, EvalMethod, Var, ArgVars,
		!Info, !IO) :-
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
	typecheck_lambda_var_has_type_2(TypeAssignSet0, Purity, PredOrFunc,
		EvalMethod, Var, ArgVars, [], TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		report_error_lambda_var(!.Info, PredOrFunc, EvalMethod,
			Var, ArgVars, TypeAssignSet0, !IO),
		typecheck_info_set_found_error(yes, !Info)
	;
		typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
	).

:- pred typecheck_lambda_var_has_type_2(type_assign_set::in, purity::in,
	pred_or_func::in, lambda_eval_method::in, prog_var::in,
	list(prog_var)::in, type_assign_set::in, type_assign_set::out) is det.

typecheck_lambda_var_has_type_2([], _, _, _, _, _, !TypeAssignSet).
typecheck_lambda_var_has_type_2([TypeAssign0 | TypeAssignSet0], Purity,
		PredOrFunc, EvalMethod, Var, ArgVars, !TypeAssignSet) :-
	type_assign_get_types_of_vars(ArgVars, ArgVarTypes,
		TypeAssign0, TypeAssign1),
	construct_higher_order_type(Purity, PredOrFunc, EvalMethod,
		ArgVarTypes, LambdaType),
	type_assign_var_has_type(TypeAssign1, Var, LambdaType, !TypeAssignSet),
	typecheck_lambda_var_has_type_2(TypeAssignSet0,
		Purity, PredOrFunc, EvalMethod, Var, ArgVars, !TypeAssignSet).

:- pred type_assign_get_types_of_vars(list(prog_var)::in, list(type)::out,
	type_assign::in, type_assign::out) is det.

type_assign_get_types_of_vars([], [], !TypeAssign).
type_assign_get_types_of_vars([Var | Vars], [Type | Types], !TypeAssign) :-
	% check whether the variable already has a type
	type_assign_get_var_types(!.TypeAssign, VarTypes0),
	( map__search(VarTypes0, Var, VarType) ->
		% if so, use that type
		Type = VarType
	;
		% otherwise, introduce a fresh type variable to
		% use as the type of that variable
		type_assign_get_typevarset(!.TypeAssign, TypeVarSet0),
		varset__new_var(TypeVarSet0, TypeVar, TypeVarSet),
		type_assign_set_typevarset(TypeVarSet, !TypeAssign),
		Type = term__variable(TypeVar),
		map__det_insert(VarTypes0, Var, Type, VarTypes1),
		type_assign_set_var_types(VarTypes1, !TypeAssign)
	),
	% recursively process the rest of the variables.
	type_assign_get_types_of_vars(Vars, Types, !TypeAssign).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two types in a type assignment
	% and update the type bindings.

:- pred type_assign_unify_type(type_assign::in, (type)::in, (type)::in,
	type_assign::out) is semidet.

type_assign_unify_type(TypeAssign0, X, Y, TypeAssign) :-
	type_assign_get_head_type_params(TypeAssign0, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
	type_unify(X, Y, HeadTypeParams, TypeBindings0, TypeBindings),
	type_assign_set_type_bindings(TypeBindings, TypeAssign0, TypeAssign).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% builtin_atomic_type(Const, TypeName)
	%	If Const is a constant of a builtin atomic type,
	%	instantiates TypeName to the name of that type,
	%	otherwise fails.

:- pred builtin_atomic_type(cons_id::in, string::out) is semidet.

builtin_atomic_type(int_const(_), "int").
builtin_atomic_type(float_const(_), "float").
builtin_atomic_type(string_const(_), "string").
builtin_atomic_type(cons(unqualified(String), 0), "character") :-
	string__char_to_string(_, String).

	% builtin_pred_type(Info, Functor, Arity, PredConsInfoList) :
	%	If Functor/Arity is a constant of a pred type,
	%	instantiates the output parameters, otherwise fails.
	%
	%	Instantiates PredConsInfoList to the set of cons_type_info
	%	structures for each predicate with name `Functor' and arity
	%	greater than or equal to Arity.
	%
	%	For example, functor `map__search/1' has type `pred(K,V)'
	%	(hence PredTypeParams = [K,V]) and argument types [map(K,V)].

:- pred builtin_pred_type(typecheck_info::in, cons_id::in, int::in,
	list(cons_type_info)::out) is semidet.

builtin_pred_type(Info, Functor, Arity, PredConsInfoList) :-
	Functor = cons(SymName, _),
	typecheck_info_get_module_info(Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	(
		predicate_table_search_sym(PredicateTable,
			calls_are_fully_qualified(Info ^ pred_markers),
			SymName, PredIdList)
	->
		predicate_table_get_preds(PredicateTable, Preds),
		make_pred_cons_info_list(Info, PredIdList, Preds, Arity,
			ModuleInfo, [], PredConsInfoList)
	;
		PredConsInfoList = []
	).

:- pred make_pred_cons_info_list(typecheck_info::in, list(pred_id)::in,
	pred_table::in, int::in, module_info::in,
	list(cons_type_info)::in, list(cons_type_info)::out) is det.

make_pred_cons_info_list(_, [], _, _, _, !ConsTypeInfos).
make_pred_cons_info_list(Info, [PredId | PredIds], PredTable, Arity,
		ModuleInfo, !ConsTypeInfos) :-
	make_pred_cons_info(Info, PredId, PredTable, Arity,
		ModuleInfo, !ConsTypeInfos),
	make_pred_cons_info_list(Info, PredIds, PredTable, Arity,
		ModuleInfo, !ConsTypeInfos).

:- type cons_type_info
	--->	cons_type_info(
			tvarset, 		% Type variables
			existq_tvars,		% Existentially quantified
						% type vars
			type, 			% Constructor type
			list(type), 		% Types of the arguments
			class_constraints	% Constraints introduced by
						% this constructor (e.g. if
						% it is actually a function,
						% or if it is an existentially
						% quantified data constructor)
		).

:- pred make_pred_cons_info(typecheck_info::in, pred_id::in, pred_table::in,
	int::in, module_info::in, list(cons_type_info)::in,
	list(cons_type_info)::out) is det.

make_pred_cons_info(_Info, PredId, PredTable, FuncArity, _, !ConsInfos) :-
	map__lookup(PredTable, PredId, PredInfo),
	PredArity = pred_info_orig_arity(PredInfo),
	IsPredOrFunc = pred_info_is_pred_or_func(PredInfo),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
		CompleteArgTypes),
	pred_info_get_purity(PredInfo, Purity),
	(
		IsPredOrFunc = predicate,
		PredArity >= FuncArity,
		% we don't support first-class polymorphism,
		% so you can't take the address of an existentially
		% quantified predicate
		PredExistQVars = []
	->
		(
			list__split_list(FuncArity, CompleteArgTypes,
				ArgTypes, PredTypeParams)
		->
			construct_higher_order_pred_type(Purity, normal,
				PredTypeParams, PredType),
			ConsInfo = cons_type_info(PredTypeVarSet,
				PredExistQVars, PredType, ArgTypes,
				ClassContext),
			!:ConsInfos = [ConsInfo | !.ConsInfos],

			% If the predicate has an Aditi marker,
			% we also add the `aditi pred(...)' type,
			% which is used for inputs to the Aditi bulk update
			% operations and also to Aditi aggregates.
			pred_info_get_markers(PredInfo, Markers),
			( check_marker(Markers, aditi) ->
				construct_higher_order_pred_type(Purity,
					(aditi_bottom_up), PredTypeParams,
					PredType2),
				ConsInfo2 = cons_type_info(PredTypeVarSet,
					PredExistQVars, PredType2,
					ArgTypes, ClassContext),
				!:ConsInfos = [ConsInfo2 | !.ConsInfos]
			;
				true
			)
		;
			error("make_pred_cons_info: split_list failed")
		)
	;
		IsPredOrFunc = function,
		PredAsFuncArity = PredArity - 1,
		PredAsFuncArity >= FuncArity,
		% We don't support first-class polymorphism,
		% so you can't take the address of an existentially
		% quantified function.  You can however call such
		% a function, so long as you pass *all* the parameters.
		( PredExistQVars = [] ; PredAsFuncArity = FuncArity )
	->
		(
			list__split_list(FuncArity, CompleteArgTypes,
				FuncArgTypes, FuncTypeParams),
			pred_args_to_func_args(FuncTypeParams,
				FuncArgTypeParams, FuncReturnTypeParam)
		->
			( FuncArgTypeParams = [] ->
				FuncType = FuncReturnTypeParam
			;
				construct_higher_order_func_type(Purity,
					normal, FuncArgTypeParams,
					FuncReturnTypeParam, FuncType)
			),
			ConsInfo = cons_type_info(PredTypeVarSet,
				PredExistQVars, FuncType, FuncArgTypes,
				ClassContext),
			!:ConsInfos = [ConsInfo | !.ConsInfos]
		;
			error("make_pred_cons_info: split_list failed")
		)
	;
		true
	).

	% builtin_apply_type(Info, Functor, Arity, ConsTypeInfos):
	% Succeed if Functor is the builtin apply/N or ''/N (N>=2),
	% which is used to invoke higher-order functions.
	% If so, bind ConsTypeInfos to a singleton list containing
	% the appropriate type for apply/N of the specified Arity.

:- pred builtin_apply_type(typecheck_info::in, cons_id::in, int::in,
	list(cons_type_info)::out) is semidet.

builtin_apply_type(_Info, Functor, Arity, ConsTypeInfos) :-
	Functor = cons(unqualified(ApplyName), _),
	( ApplyName = "apply", Purity = (pure)
	; ApplyName = "", Purity = (pure)
	% XXX FIXME handle impure apply/N more elegantly (e.g. nicer syntax)
	; ApplyName = "impure_apply", Purity = (impure)
	; ApplyName = "semipure_apply", Purity = (semipure)
	),
	Arity >= 1,
	Arity1 = Arity - 1,
	higher_order_func_type(Purity, Arity1, normal, TypeVarSet, FuncType,
		ArgTypes, RetType),
	ExistQVars = [],
	Constraints = constraints([], []),
	ConsTypeInfos = [cons_type_info(TypeVarSet, ExistQVars, RetType,
		[FuncType | ArgTypes], Constraints)].

	% builtin_field_access_function_type(Info, Functor,
	%	Arity, ConsTypeInfos):
	% Succeed if Functor is the name of one the automatically
	% generated field access functions (fieldname, '<fieldname> :=').
:- pred builtin_field_access_function_type(typecheck_info::in, cons_id::in,
	arity::in, list(maybe_cons_type_info)::out) is semidet.

builtin_field_access_function_type(Info, Functor, Arity, MaybeConsTypeInfos) :-
	%
	% Taking the address of automatically generated field access
	% functions is not allowed, so currying does have to be
	% considered here.
	%
	Functor = cons(Name, Arity),
	typecheck_info_get_module_info(Info, ModuleInfo),
	is_field_access_function_name(ModuleInfo, Name, Arity, AccessType,
		FieldName),

	module_info_ctor_field_table(ModuleInfo, CtorFieldTable),
	map__search(CtorFieldTable, FieldName, FieldDefns),

	list__filter_map(
		make_field_access_function_cons_type_info(Info, Name,
			Arity, AccessType, FieldName),
		FieldDefns, MaybeConsTypeInfos).

:- pred make_field_access_function_cons_type_info(typecheck_info::in,
	sym_name::in, arity::in, field_access_type::in, ctor_field_name::in,
	hlds_ctor_field_defn::in, maybe_cons_type_info::out) is semidet.

make_field_access_function_cons_type_info(Info, FuncName, Arity,
		AccessType, FieldName, FieldDefn, ConsTypeInfo) :-
	get_field_access_constructor(Info, FuncName, Arity,
		AccessType, FieldDefn, MaybeFunctorConsTypeInfo),
	(
		MaybeFunctorConsTypeInfo = ok(FunctorConsTypeInfo),
		convert_field_access_cons_type_info(AccessType, FieldName,
			FieldDefn, FunctorConsTypeInfo, ConsTypeInfo)
	;
		MaybeFunctorConsTypeInfo = error(_),
		ConsTypeInfo = MaybeFunctorConsTypeInfo
	).

:- pred get_field_access_constructor(typecheck_info::in, sym_name::in,
	arity::in, field_access_type::in, hlds_ctor_field_defn::in,
	maybe_cons_type_info::out) is semidet.

get_field_access_constructor(Info, FuncName, Arity, _AccessType, FieldDefn,
		FunctorConsTypeInfo) :-
	FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, ConsId, _),
	TypeCtor = qualified(TypeModule, _) - _,

	%
	% If the user has supplied a declaration, we use that instead
	% of the automatically generated version, unless we are typechecking
	% the clause introduced for the user-supplied declaration.
	% The user-declared version will be picked up by builtin_pred_type.
	%
	typecheck_info_get_module_info(Info, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredTable),
	unqualify_name(FuncName, UnqualFuncName),
	(
		Info ^ is_field_access_function = no,
		\+ predicate_table_search_func_m_n_a(PredTable,
			is_fully_qualified, TypeModule, UnqualFuncName,
			Arity, _)
	;
		Info ^ is_field_access_function = yes
	),
	module_info_ctors(ModuleInfo, Ctors),
	map__lookup(Ctors, ConsId, ConsDefns0),
	list__filter(
		(pred(CtorDefn::in) is semidet :-
			TypeCtor = CtorDefn ^ cons_type_ctor
		), ConsDefns0, ConsDefns),
	ConsDefns = [ConsDefn],
	convert_cons_defn(Info, ConsDefn, FunctorConsTypeInfo).

:- type maybe_cons_type_info
	--->	ok(cons_type_info)
	;	error(cons_error).

:- type cons_error
	--->	foreign_type_constructor(type_ctor, hlds_type_defn)
	;	abstract_imported_type
	;	invalid_field_update(ctor_field_name, hlds_ctor_field_defn,
			tvarset, list(tvar)).

:- pred convert_field_access_cons_type_info(field_access_type::in,
	ctor_field_name::in, hlds_ctor_field_defn::in,
	cons_type_info::in, maybe_cons_type_info::out) is det.

convert_field_access_cons_type_info(AccessType, FieldName, FieldDefn,
		FunctorConsTypeInfo, ConsTypeInfo) :-
	FunctorConsTypeInfo = cons_type_info(TVarSet0, ExistQVars0,
		FunctorType, ConsArgTypes, ClassConstraints0),
	FieldDefn = hlds_ctor_field_defn(_, _, _, _, FieldNumber),
	list__index1_det(ConsArgTypes, FieldNumber, FieldType),
	(
		AccessType = get,
		RetType = FieldType,
		ArgTypes = [FunctorType],
		TVarSet = TVarSet0,
		ExistQVars = ExistQVars0,
		ClassConstraints = ClassConstraints0,
		ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
			RetType, ArgTypes, ClassConstraints))
	;
		AccessType = set,

		%
		% A `'field :='/2' function has no existentially
		% quantified type variables - the values of all
		% type variables in the field are supplied by
		% the caller, all the others are supplied by
		% the input term.
		%
		ExistQVars = [],

		%
		% When setting a polymorphic field, the type of the
		% field in the result is not necessarily the
		% same as in the input.
		% If a type variable occurs only in the field being set,
		% create a new type variable for it in the result type.
		%
		% This allows code such as
		% :- type pair(T, U)
		%	---> '-'(fst::T, snd::U).
		%
		%	Pair0 = 1 - 'a',
		% 	Pair = Pair0 ^ snd := 2.
		%
		term__vars(FieldType, TVarsInField),
		( TVarsInField = [] ->
			TVarSet = TVarSet0,
			RetType = FunctorType,
			ArgTypes = [FunctorType, FieldType],

			%
			% Remove any existential constraints - the
			% typeclass-infos supplied by the input term
			% are local to the set function, so they don't
			% have to be considered here.
			%
			ClassConstraints0 = constraints(UnivConstraints, _),
			ClassConstraints = constraints(UnivConstraints, []),
			ConsTypeInfo = ok(cons_type_info(TVarSet, ExistQVars,
				RetType, ArgTypes, ClassConstraints))
		;
			%
			% XXX This demonstrates a problem - if a
			% type variable occurs in the types of multiple
			% fields, any predicates changing values of
			% one of these fields cannot change their types.
			% This especially a problem for existentially typed
			% fields, because setting the field always changes
			% the type.
			%
			% Haskell gets around this problem by allowing
			% multiple fields to be set by the same expression.
			% Haskell doesn't handle all cases -- it is not
			% possible to get multiple existentially typed fields
			% using record syntax and pass them to a function
			% whose type requires that the fields are of the
			% same type. It probably won't come up too often.
			%
			list__replace_nth_det(ConsArgTypes, FieldNumber,
				int_type, ArgTypesWithoutField),
			term__vars_list(ArgTypesWithoutField,
				TVarsInOtherArgs),
			set__intersect(
				set__list_to_set(TVarsInField),
				set__intersect(
					set__list_to_set(TVarsInOtherArgs),
					set__list_to_set(ExistQVars0)
				),
				ExistQVarsInFieldAndOthers),
			( set__empty(ExistQVarsInFieldAndOthers) ->
				%
				% Rename apart type variables occurring only
				% in the field to be replaced - the values of
				% those type variables will be supplied by the
				% replacement field value.
				%
				list__delete_elems(TVarsInField,
					TVarsInOtherArgs, TVarsOnlyInField0),
				list__sort_and_remove_dups(TVarsOnlyInField0,
					TVarsOnlyInField),
				list__length(TVarsOnlyInField, NumNewTVars),
				varset__new_vars(TVarSet0, NumNewTVars,
					NewTVars, TVarSet),
				map__from_corresponding_lists(TVarsOnlyInField,
					NewTVars, TVarRenaming),
				term__apply_variable_renaming(FieldType,
					TVarRenaming, RenamedFieldType),
				term__apply_variable_renaming(FunctorType,
					TVarRenaming, OutputFunctorType),

				%
				% Rename the class constraints, projecting
				% the constraints onto the set of type variables
				% occuring in the types of the arguments of
				% the call to `'field :='/2'.
				%
				term__vars_list([FunctorType, FieldType],
					CallTVars0),
				set__list_to_set(CallTVars0, CallTVars),
				project_rename_flip_class_constraints(CallTVars,
					TVarRenaming, ClassConstraints0,
					ClassConstraints),

				RetType = OutputFunctorType,
				ArgTypes = [FunctorType, RenamedFieldType],
				ConsTypeInfo = ok(cons_type_info(TVarSet,
					ExistQVars, RetType, ArgTypes,
					ClassConstraints))
			;
				%
				% This field cannot be set. Pass out some
				% information so that we can give a better
				% error message. Errors involving changing
				% the types of universally quantified type
				% variables will be caught by
				% typecheck_functor_arg_types.
				%
				set__to_sorted_list(ExistQVarsInFieldAndOthers,
					ExistQVarsInFieldAndOthers1),
				ConsTypeInfo =
					error(invalid_field_update(FieldName,
						FieldDefn, TVarSet0,
						ExistQVarsInFieldAndOthers1))
			)
		)
	).

	% Rename constraints containing variables that have been renamed.
	% These constraints are all universal constraints - the values
	% of the type variables are supplied by the caller.
:- pred project_rename_flip_class_constraints(set(tvar)::in,
	map(tvar, tvar)::in, class_constraints::in, class_constraints::out)
	is det.

project_rename_flip_class_constraints(CallTVars, TVarRenaming,
		Constraints0, Constraints) :-
	Constraints0 = constraints(UnivConstraints0, ExistConstraints0),

	%
	% XXX We currently don't allow universal constraints on
	% types or data constructors (but we should). When we
	% implement handling of those, they will need to be renamed
	% here as well.
	%
	( UnivConstraints0 = [] ->
		true
	;
		error(
		"project_rename_flip_class_constraints: universal constraints")
	),

	%
	% Project the constraints down onto the list of tvars
	% in the call.
	%
	list__filter(project_constraint(CallTVars),
		ExistConstraints0, ExistConstraints1),
	list__filter_map(rename_constraint(TVarRenaming),
		ExistConstraints1, NewConstraints),

	% The variables which were previously existentially quantified
	% are now universally quantified.
	Constraints = constraints(NewConstraints, []).

:- pred project_constraint(set(tvar)::in, class_constraint::in) is semidet.

project_constraint(CallTVars, Constraint) :-
	Constraint = constraint(_, TypesToCheck),
	term__vars_list(TypesToCheck, TVarsToCheck0),
	set__list_to_set(TVarsToCheck0, TVarsToCheck),
	set__intersect(TVarsToCheck, CallTVars, RelevantTVars),
	\+ set__empty(RelevantTVars).

:- pred rename_constraint(map(tvar, tvar)::in, class_constraint::in,
	class_constraint::out) is semidet.

rename_constraint(TVarRenaming, Constraint0, Constraint) :-
	Constraint0 = constraint(ClassName, ConstraintTypes0),
	some [Var] (
		term__contains_var_list(ConstraintTypes0, Var),
		map__contains(TVarRenaming, Var)
	),
	term__apply_variable_renaming_to_list(ConstraintTypes0,
		TVarRenaming, ConstraintTypes),
	Constraint = constraint(ClassName, ConstraintTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The typecheck_info data structure and access predicates.

:- type typecheck_info --->
	typecheck_info(
		module_info		:: module_info,

		call_id			:: call_id,
					% The call_id of the pred
					% being called (if any)

		arg_num			:: int,
					% The argument number within
					% a pred call

		pred_id			:: pred_id,
					% The pred we're checking

		import_status		:: import_status,
					% Import status of the pred
					% being checked

		pred_markers		:: pred_markers,
					% Markers of the pred being checked

		is_field_access_function :: bool,
					% Is the pred we're checking
					% a field access function?
					% If so, there should only
					% be a field access function
					% application in the body, not
					% predicate or function calls
					% or constructor applications.

		context			:: prog_context,
					% The context of the goal
					% we're checking

		unify_context		:: unify_context,
					% The original source of the
					% unification we're checking

		varset			:: prog_varset,
					% Variable names

		type_assign_set		:: type_assign_set,
					% This is the main piece of
					% information that we are
					% computing and which gets
					% updated as we go along

		found_error		:: bool,
					% did we find any type errors?

		warned_about_overloading :: bool
					% Have we already warned about
					% highly ambiguous overloading?
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_init(module_info::in, pred_id::in,
	bool::in, tvarset::in, prog_varset::in, map(prog_var, type)::in,
	headtypes::in, class_constraints::in, import_status::in,
	pred_markers::in, typecheck_info::out) is det.

typecheck_info_init(ModuleInfo, PredId, IsFieldAccessFunction,
		TypeVarSet, VarSet, VarTypes, HeadTypeParams,
		Constraints, Status, Markers, Info) :-
	CallPredId = call(predicate - unqualified("") / 0),
	term__context_init(Context),
	map__init(TypeBindings),
	map__init(Proofs),
	FoundTypeError = no,
	WarnedAboutOverloading = no,
	Info = typecheck_info(
		ModuleInfo, CallPredId, 0, PredId, Status, Markers,
		IsFieldAccessFunction, Context,
		unify_context(explicit, []), VarSet,
		[type_assign(VarTypes, TypeVarSet, HeadTypeParams,
			TypeBindings, Constraints, Proofs)],
		FoundTypeError, WarnedAboutOverloading
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_module_info(typecheck_info::in, module_info::out)
	is det.
:- pred typecheck_info_get_called_predid(typecheck_info::in, call_id::out)
	is det.
:- pred typecheck_info_get_arg_num(typecheck_info::in, int::out) is det.
:- pred typecheck_info_get_predid(typecheck_info::in, pred_id::out) is det.
:- pred typecheck_info_get_context(typecheck_info::in,
	prog_context::out) is det.
:- pred typecheck_info_get_unify_context(typecheck_info::in,
	unify_context::out) is det.
:- pred typecheck_info_get_varset(typecheck_info::in, prog_varset::out) is det.
:- pred typecheck_info_get_type_assign_set(typecheck_info::in,
	type_assign_set::out) is det.
:- pred typecheck_info_get_found_error(typecheck_info::in, bool::out) is det.
:- pred typecheck_info_get_warned_about_overloading(typecheck_info::in,
	bool::out) is det.
:- pred typecheck_info_get_pred_import_status(typecheck_info::in,
	import_status::out) is det.

typecheck_info_get_module_info(Info, Info ^ module_info).
typecheck_info_get_called_predid(Info, Info ^ call_id).
typecheck_info_get_arg_num(Info, Info ^ arg_num).
typecheck_info_get_predid(Info, Info ^ pred_id).
typecheck_info_get_context(Info, Info ^ context).
typecheck_info_get_unify_context(Info, Info ^ unify_context).
typecheck_info_get_varset(Info, Info ^ varset).
typecheck_info_get_type_assign_set(Info, Info ^ type_assign_set).
typecheck_info_get_found_error(Info, Info ^ found_error).
typecheck_info_get_warned_about_overloading(Info,
		Info ^ warned_about_overloading).
typecheck_info_get_pred_import_status(Info, Info ^ import_status).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_called_predid(call_id::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_arg_num(int::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_context(prog_context::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_unify_context(unify_context::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_type_assign_set(type_assign_set::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_found_error(bool::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_warned_about_overloading(bool::in,
	typecheck_info::in, typecheck_info::out) is det.
:- pred typecheck_info_set_pred_import_status(import_status::in,
	typecheck_info::in, typecheck_info::out) is det.

typecheck_info_set_called_predid(PredCallId, Info,
		Info ^ call_id := PredCallId).
typecheck_info_set_arg_num(ArgNum, Info, Info ^ arg_num := ArgNum).
typecheck_info_set_context(Context, Info, Info ^ context := Context).
typecheck_info_set_unify_context(UnifyContext, Info,
		Info ^ unify_context := UnifyContext).
typecheck_info_set_type_assign_set(TypeAssignSet, Info,
		Info ^ type_assign_set := TypeAssignSet).
typecheck_info_set_found_error(FoundError, Info,
		Info ^ found_error := FoundError).
typecheck_info_set_warned_about_overloading(Warned, Info,
		Info ^ warned_about_overloading := Warned).
typecheck_info_set_pred_import_status(Status, Info,
		Info ^ import_status := Status).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_module_name(typecheck_info::in, module_name::out)
	is det.
:- pred typecheck_info_get_preds(typecheck_info::in, predicate_table::out)
	is det.
:- pred typecheck_info_get_types(typecheck_info::in, type_table::out) is det.
:- pred typecheck_info_get_ctors(typecheck_info::in, cons_table::out) is det.

typecheck_info_get_module_name(Info, Name) :-
	module_info_name(Info ^ module_info, Name).
typecheck_info_get_preds(Info, Preds) :-
	module_info_get_predicate_table(Info ^ module_info, Preds).
typecheck_info_get_types(Info, Types) :-
	module_info_types(Info ^ module_info, Types).
typecheck_info_get_ctors(Info, Ctors) :-
	module_info_ctors(Info ^ module_info, Ctors).

:- pred typecheck_info_get_pred_markers(typecheck_info::in, pred_markers::out)
	is det.

typecheck_info_get_pred_markers(Info, PredMarkers) :-
	typecheck_info_get_module_info(Info, ModuleInfo),
	typecheck_info_get_predid(Info, PredId),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, PredMarkers).

%-----------------------------------------------------------------------------%

% typecheck_info_get_final_info(Info, OldHeadTypeParams, OldExistQVars,
%		OldExplicitVarTypes, NewTypeVarSet, New* ..., TypeRenaming,
%		ExistTypeRenaming):
%	extracts the final inferred types from Info.
%
%	OldHeadTypeParams should be the type variables from the head of the
%	predicate.
%	OldExistQVars should be the declared existentially quantified
%	type variables (if any).
%	OldExplicitVarTypes is the vartypes map containing the explicit
%	type qualifications.
%	New* is the newly inferred types, in NewTypeVarSet.
%	TypeRenaming is a map to rename things from the old TypeVarSet
%	to the NewTypeVarSet.
%	ExistTypeRenaming is a map (which should be applied *before*
%	applying TypeRenaming) to rename existential type variables
%	in OldExistQVars.

:- pred typecheck_info_get_final_info(typecheck_info::in, list(tvar)::in,
	existq_tvars::in, vartypes::in, tvarset::out, existq_tvars::out,
	map(prog_var, type)::out, class_constraints::out,
	map(class_constraint, constraint_proof)::out, map(tvar, tvar)::out,
	map(tvar, tvar)::out) is det.

typecheck_info_get_final_info(Info, OldHeadTypeParams, OldExistQVars,
		OldExplicitVarTypes, NewTypeVarSet, NewHeadTypeParams,
		NewVarTypes, NewTypeConstraints, NewConstraintProofs, TSubst,
		ExistTypeRenaming) :-
	typecheck_info_get_type_assign_set(Info, TypeAssignSet),
	( TypeAssignSet = [TypeAssign | _] ->
		type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
		type_assign_get_typevarset(TypeAssign, OldTypeVarSet),
		type_assign_get_var_types(TypeAssign, VarTypes0),
		type_assign_get_type_bindings(TypeAssign, TypeBindings),
		type_assign_get_typeclass_constraints(TypeAssign,
			TypeConstraints),
		type_assign_get_constraint_proofs(TypeAssign,
			ConstraintProofs0),

		map__keys(VarTypes0, Vars),
		expand_types(Vars, TypeBindings, VarTypes0, VarTypes),
		apply_rec_subst_to_constraint_proofs(TypeBindings,
			ConstraintProofs0, ConstraintProofs),

		%
		% figure out how we should rename the existential types
		% in the type declaration (if any)
		%
		get_existq_tvar_renaming(OldHeadTypeParams, OldExistQVars,
			TypeBindings, ExistTypeRenaming),

		%
		% We used to just use the OldTypeVarSet that we got
		% from the type assignment.
		%
		% However, that caused serious efficiency problems,
		% because the typevarsets get bigger and bigger with each
		% inference step.  Instead, we now construct a new
		% typevarset NewTypeVarSet which contains only the
		% variables we want, and we rename the type variables
		% so that they fit into this new typevarset.
		%

		%
		% First, find the set (sorted list) of type variables
		% that we need.  This must include any type variables
		% in the inferred types, the explicit type qualifications,
		% and any existentially typed variables that will remain
		% in the declaration.
		%
		% There may also be some type variables in the HeadTypeParams
		% which do not occur in the type of any variable (e.g. this
		% can happen in the case of code containing type errors).
		% We'd better keep those, too, to avoid errors
		% when we apply the TSubst to the HeadTypeParams.
		% (XXX should we do the same for TypeConstraints and
		% ConstraintProofs too?)
		%
		map__values(VarTypes, Types),
		term__vars_list(Types, TypeVars0),
		map__values(OldExplicitVarTypes, ExplicitTypes),
		term__vars_list(ExplicitTypes, ExplicitTypeVars0),
		map__keys(ExistTypeRenaming, ExistQVarsToBeRenamed),
		list__delete_elems(OldExistQVars, ExistQVarsToBeRenamed,
			ExistQVarsToRemain),
		list__condense([ExistQVarsToRemain, HeadTypeParams,
			TypeVars0, ExplicitTypeVars0], TypeVars1),
		list__sort_and_remove_dups(TypeVars1, TypeVars),
		%
		% Next, create a new typevarset with the same number of
		% variables.
		%
		varset__squash(OldTypeVarSet, TypeVars, NewTypeVarSet,
			TSubst),
		%
		% Finally, rename the types and type class constraints
		% to use the new typevarset type variables.
		%
		term__apply_variable_renaming_to_list(Types, TSubst,
			NewTypes),
		map__from_corresponding_lists(Vars, NewTypes, NewVarTypes),
		map__apply_to_list(HeadTypeParams, TSubst, NewHeadTypeParams),
		apply_variable_renaming_to_constraints(TSubst, TypeConstraints,
			NewTypeConstraints),
		( map__is_empty(ConstraintProofs) ->
			% optimize simple case
			NewConstraintProofs = ConstraintProofs
		;
			map__keys(ConstraintProofs, ProofKeysList),
			map__values(ConstraintProofs, ProofValuesList),
			apply_variable_renaming_to_constraint_list(TSubst,
				ProofKeysList, NewProofKeysList),
			list__map(rename_constraint_proof(TSubst),
				ProofValuesList, NewProofValuesList),
			map__from_corresponding_lists(NewProofKeysList,
				NewProofValuesList, NewConstraintProofs)
		)
	;
		error("internal error in typecheck_info_get_vartypes")
	).

%
% We rename any existentially quantified type variables which
% get mapped to other type variables, unless they are mapped to
% universally quantified type variables from the head of the predicate.
%
:- pred get_existq_tvar_renaming(list(tvar)::in, existq_tvars::in, tsubst::in,
	map(tvar, tvar)::out) is det.

get_existq_tvar_renaming(OldHeadTypeParams, ExistQVars, TypeBindings,
		ExistTypeRenaming) :-
	list__foldl(
		get_existq_tvar_renaming_2(OldHeadTypeParams, TypeBindings),
		ExistQVars, map__init, ExistTypeRenaming).

:- pred get_existq_tvar_renaming_2(existq_tvars::in, tsubst::in,
	tvar::in, map(tvar, tvar)::in, map(tvar, tvar)::out) is det.

get_existq_tvar_renaming_2(OldHeadTypeParams, TypeBindings, TVar, !Renaming) :-
	term__apply_rec_substitution(term__variable(TVar), TypeBindings,
		Result),
	(
		Result = term__variable(NewTVar),
		NewTVar \= TVar,
		\+ list__member(NewTVar, OldHeadTypeParams)
	->
		map__det_insert(!.Renaming, TVar, NewTVar, !:Renaming)
	;
		true
	).

	% fully expand the types of the variables by applying the type
	% bindings

:- pred expand_types(list(prog_var)::in, tsubst::in, map(prog_var, type)::in,
	map(prog_var, type)::out) is det.

expand_types([], _, !VarTypes).
expand_types([Var | Vars], TypeSubst, !VarTypes) :-
	map__lookup(!.VarTypes, Var, Type0),
	term__apply_rec_substitution(Type0, TypeSubst, Type),
	map__det_update(!.VarTypes, Var, Type, !:VarTypes),
	expand_types(Vars, TypeSubst, !VarTypes).

:- pred rename_constraint_proof(map(tvar, tvar)::in, constraint_proof::in,
	constraint_proof::out) is det.

% apply a type variable renaming to a class constraint proof

rename_constraint_proof(_TSubst, apply_instance(Num), apply_instance(Num)).
rename_constraint_proof(TSubst, superclass(ClassConstraint0),
		superclass(ClassConstraint)) :-
	apply_variable_renaming_to_constraint(TSubst, ClassConstraint0,
		ClassConstraint).

%-----------------------------------------------------------------------------%

	%
	% Note: changes here may require changes to
	% post_typecheck__resolve_unify_functor,
	% intermod__module_qualify_unify_rhs,
	% recompilation__usage__find_matching_constructors
	% and recompilation__check__check_functor_ambiguities.
	%
:- pred typecheck_info_get_ctor_list(typecheck_info::in, cons_id::in, int::in,
	list(cons_type_info)::out, list(cons_error)::out) is det.

typecheck_info_get_ctor_list(Info, Functor, Arity, ConsInfoList, ConsErrors) :-
	(
		%
		% If we're typechecking the clause added for
		% a field access function for which the user
		% has supplied type or mode declarations, the
		% goal should only contain an application of the
		% field access function, not constructor applications
		% or function calls. The clauses in `.opt' files will
		% already have been expanded into unifications.
		%
		Info ^ is_field_access_function = yes,
		Info ^ import_status \= opt_imported
	->
		(
			builtin_field_access_function_type(Info,
				Functor, Arity, FieldAccessConsInfoList)
		->
			split_cons_errors(FieldAccessConsInfoList,
				ConsInfoList, ConsErrors)
		;
			ConsInfoList = [],
			ConsErrors = []
		)
	;
		typecheck_info_get_ctor_list_2(Info, Functor, Arity,
			ConsInfoList, ConsErrors)
	).

:- pred typecheck_info_get_ctor_list_2(typecheck_info::in, cons_id::in,
	int::in, list(cons_type_info)::out, list(cons_error)::out) is det.

typecheck_info_get_ctor_list_2(Info, Functor, Arity, ConsInfoList,
		ConsErrors) :-
	% Check if `Functor/Arity' has been defined as a constructor
	% in some discriminated union type(s).  This gives
	% us a list of possible cons_type_infos.
	typecheck_info_get_ctors(Info, Ctors),
	(
		Functor = cons(_, _),
		map__search(Ctors, Functor, HLDS_ConsDefnList)
	->
		convert_cons_defn_list(Info, HLDS_ConsDefnList,
			MaybeConsInfoList0)
	;
		MaybeConsInfoList0 = []
	),

	% For "existentially typed" functors, whether the functor
	% is actually existentially typed depends on whether it is
	% used as a constructor or as a deconstructor.  As a constructor,
	% it is universally typed, but as a deconstructor, it is
	% existentially typed.  But type checking and polymorphism need
	% to know whether it is universally or existentially quantified
	% _before_ mode analysis has inferred the mode of the unification.
	% Therefore, we use a special syntax for construction unifications
	% with existentially quantified functors: instead of just using the
	% functor name (e.g. "Y = foo(X)", the programmer must use the
	% special functor name "new foo" (e.g. "Y = 'new foo'(X)").
	%
	% Here we check for occurrences of functor names starting with
	% "new ".  For these, we look up the original functor in the
	% constructor symbol table, and for any occurrences of that
	% functor we flip the quantifiers on the type definition
	% (i.e. convert the existential quantifiers and constraints
	% into universal ones).
	(
		Functor = cons(Name, Arity),
		remove_new_prefix(Name, OrigName),
		OrigFunctor = cons(OrigName, Arity),
		map__search(Ctors, OrigFunctor, HLDS_ExistQConsDefnList)
	->
		convert_cons_defn_list(Info, HLDS_ExistQConsDefnList,
			ExistQuantifiedConsInfoList),
		list__filter_map(flip_quantifiers, ExistQuantifiedConsInfoList,
			UnivQuantifiedConsInfoList),
		list__append(UnivQuantifiedConsInfoList,
			MaybeConsInfoList0, MaybeConsInfoList1)
	;
		MaybeConsInfoList1 = MaybeConsInfoList0
	),

	%
	% Check if Functor is a field access function for which the
	% user has not supplied a declaration.
	%
	(
		builtin_field_access_function_type(Info, Functor, Arity,
			FieldAccessConsInfoList)
	->
		MaybeConsInfoList = FieldAccessConsInfoList ++
			MaybeConsInfoList1
	;
		MaybeConsInfoList = MaybeConsInfoList1
	),

	split_cons_errors(MaybeConsInfoList, ConsInfoList1, ConsErrors),

	% Check if Functor is a constant of one of the builtin atomic
	% types (string, float, int, character).  If so, insert
	% the resulting cons_type_info at the start of the list.
	(
		Arity = 0,
		builtin_atomic_type(Functor, BuiltInTypeName)
	->
		% ZZZ
		construct_type(unqualified(BuiltInTypeName) - 0, [], ConsType),
		varset__init(ConsTypeVarSet),
		ConsInfo = cons_type_info(ConsTypeVarSet, [], ConsType, [],
			constraints([], [])),
		ConsInfoList2 = [ConsInfo | ConsInfoList1]
	;
		ConsInfoList2 = ConsInfoList1
	),

	%
	% Check if Functor is a tuple constructor.
	%
	(
		Functor = cons(unqualified("{}"), TupleArity)
	->
		%
		% Make some fresh type variables for the argument types.
		%
		varset__init(TupleConsTypeVarSet0),
		varset__new_vars(TupleConsTypeVarSet0, TupleArity,
			TupleArgTVars, TupleConsTypeVarSet),
		term__var_list_to_term_list(TupleArgTVars, TupleArgTypes),

		% ZZZ
		construct_type(unqualified("{}") - TupleArity, TupleArgTypes,
			TupleConsType),

		% Tuples can't have existentially typed arguments.
		TupleExistQVars = [],

		TupleConsInfo = cons_type_info(TupleConsTypeVarSet,
			TupleExistQVars, TupleConsType,
			TupleArgTypes, constraints([], [])),
		ConsInfoList3 = [TupleConsInfo | ConsInfoList2]
	;
		ConsInfoList3 = ConsInfoList2
	),

	% Check if Functor is the name of a predicate which takes at least
	% Arity arguments.  If so, insert the resulting cons_type_info
	% at the start of the list.
	( builtin_pred_type(Info, Functor, Arity, PredConsInfoList) ->
		list__append(ConsInfoList3, PredConsInfoList, ConsInfoList4)
	;
		ConsInfoList4 = ConsInfoList3
	),

	%
	% Check for higher-order function calls
	%
	( builtin_apply_type(Info, Functor, Arity, ApplyConsInfoList) ->
		ConsInfoList = list__append(ConsInfoList4, ApplyConsInfoList)
	;
		ConsInfoList = ConsInfoList4
	).

:- pred flip_quantifiers(maybe_cons_type_info::in, maybe_cons_type_info::out)
	is semidet.

flip_quantifiers(Error @ error(_), Error).
flip_quantifiers(ok(cons_type_info(A, ExistQVars0, C, D, Constraints0)),
		ok(cons_type_info(A, ExistQVars, C, D, Constraints))) :-
	% Fail if there are no existentially quantifier variables.
	% We do this because we want to allow the 'new foo' syntax only
	% for existentially typed functors, not for ordinary functors.
	%
	ExistQVars0 \= [],

	% convert the existentially quantified type vars into
	% universally quantified type vars by just discarding
	% the old list of existentially quantified type vars and
	% replacing it with an empty list.
	ExistQVars = [],

	% convert the existential constraints into universal constraints
	dual_constraints(Constraints0, Constraints).

:- pred split_cons_errors(list(maybe_cons_type_info)::in,
	list(cons_type_info)::out, list(cons_error)::out) is det.

split_cons_errors(MaybeConsInfoList, ConsInfoList, ConsErrors) :-
	%
	% Filter out the errors (they aren't actually reported as
	% errors unless there was no other matching constructor).
	%
	list__filter_map(
		(pred(ok(ConsInfo)::in, ConsInfo::out) is semidet),
		MaybeConsInfoList, ConsInfoList, ConsErrors0),
	(
		list__map(
			(pred(error(ConsError)::in, ConsError::out)
				is semidet),
			ConsErrors0, ConsErrors1)
	->
		ConsErrors = ConsErrors1
	;
		error("typecheck_info_get_ctor_list")
	).

%-----------------------------------------------------------------------------%

:- pred report_unsatisfiable_constraints(type_assign_set::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

report_unsatisfiable_constraints(TypeAssignSet, !Info, !IO) :-
	typecheck_info_get_context(!.Info, Context),
	write_context_and_pred_id(!.Info, !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  unsatisfiable typeclass constraint(s):\n", !IO),

		% XXX this won't be very pretty when there are
		% XXX multiple type_assigns.
	io__write_list(TypeAssignSet, "\n", write_constraints(Context), !IO),
	typecheck_info_set_found_error(yes, !Info).

:- pred write_constraints(prog_context::in, type_assign::in, io::di, io::uo)
	is det.

write_constraints(Context, TypeAssign, !IO) :-
	type_assign_get_typeclass_constraints(TypeAssign, Constraints),
	Constraints = constraints(UnprovenConstraints0, _AssumedConstraints),

	type_assign_get_typevarset(TypeAssign, VarSet),
	type_assign_get_type_bindings(TypeAssign, Bindings),
	apply_rec_subst_to_constraint_list(Bindings,
		UnprovenConstraints0, UnprovenConstraints1),
	list__sort_and_remove_dups(UnprovenConstraints1,
		UnprovenConstraints),
	prog_out__write_context(Context, !IO),
	io__write_string("  `", !IO),
	AppendVarnums = no,
	io__write_list(UnprovenConstraints, "', `",
		mercury_output_constraint(VarSet, AppendVarnums),
		!IO),
	io__write_string("'.\n", !IO).

%-----------------------------------------------------------------------------%

% perform_context_reduction(OrigTypeAssignSet, Info0, Info)
%	is true iff either
% 	Info is the typecheck_info that results from performing
% 	context reduction on the type_assigns in Info0,
%	or, if there is no valid context reduction, then
%	Info is Info0 with the type assign set replaced by
%	OrigTypeAssignSet (see below).
%
% 	Context reduction is the process of eliminating redundant constraints
% 	from the constraints in the type_assign and adding the proof of the
% 	constraint's redundancy to the proofs in the same type_assign. There
% 	are three ways in which a constraint may be redundant:
%		- if a constraint occurs in the pred/func declaration for this
%		  predicate or function, then it is redundant
%		  (in this case, the proof is trivial, so there is no need
%		  to record it in the proof map)
% 		- if a constraint is present in the set of constraints and all
% 		  of the "superclass" constraints for the constraints are all
% 		  present, then all the superclass constraints are eliminated
% 		- if there is an instance declaration that may be applied, the
% 		  constraint is replaced by the constraints from that instance
% 		  declaration
%
% 	In addition, context reduction removes repeated constraints.
%
% 	If context reduction fails on a type_assign, that type_assign is
% 	removed from the type_assign_set. Context reduction fails if there is
%	a constraint where the type of (at least) one of the arguments to
%	the constraint has its top level functor bound, but there is no
%	instance declaration for that type.
%
%	If all type_assigns from the typecheck_info are rejected, than an
%	appropriate error message is given, the type_assign_set is
%	restored to the original one given by OrigTypeAssignSet,
%	but without any typeclass constraints.
%	The reason for this is to avoid reporting the same error at
%	subsequent calls to perform_context_reduction.

:- pred perform_context_reduction(type_assign_set::in,
	typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

perform_context_reduction(OrigTypeAssignSet, !Info, !IO) :-
	checkpoint("before context reduction", !Info, !IO),
	typecheck_info_get_module_info(!.Info, ModuleInfo),
	module_info_superclasses(ModuleInfo, SuperClassTable),
	module_info_instances(ModuleInfo, InstanceTable),
	typecheck_info_get_type_assign_set(!.Info, TypeAssignSet0),
	list__filter_map(
		reduce_type_assign_context(SuperClassTable, InstanceTable),
		TypeAssignSet0, TypeAssignSet),
	(
			% Check that this context reduction hasn't eliminated
			% all the type assignments.
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		report_unsatisfiable_constraints(TypeAssignSet0, !Info, !IO),
		DeleteConstraints = (pred(TA0::in, TA::out) is det :-
			type_assign_get_typeclass_constraints(TA0,
				constraints(_, AssumedConstraints)),
			type_assign_set_typeclass_constraints(
				constraints([], AssumedConstraints), TA0, TA)
		),
		list__map(DeleteConstraints, OrigTypeAssignSet,
			NewTypeAssignSet),
		typecheck_info_set_type_assign_set(NewTypeAssignSet, !Info)
	;
		typecheck_info_set_type_assign_set(TypeAssignSet, !Info)
	).

:- pred reduce_type_assign_context(superclass_table::in, instance_table::in,
	type_assign::in, type_assign::out) is semidet.

reduce_type_assign_context(SuperClassTable, InstanceTable, !TypeAssign) :-
	type_assign_get_head_type_params(!.TypeAssign, HeadTypeParams),
	type_assign_get_type_bindings(!.TypeAssign, Bindings),
	type_assign_get_typeclass_constraints(!.TypeAssign, Constraints0),
	type_assign_get_typevarset(!.TypeAssign, TVarSet0),
	type_assign_get_constraint_proofs(!.TypeAssign, Proofs0),

	Constraints0 = constraints(UnprovenConstraints0, AssumedConstraints),
	typecheck__reduce_context_by_rule_application(InstanceTable,
		SuperClassTable, AssumedConstraints,
		Bindings, TVarSet0, TVarSet, Proofs0, Proofs,
		UnprovenConstraints0, UnprovenConstraints),
	check_satisfiability(UnprovenConstraints, HeadTypeParams),
	Constraints = constraints(UnprovenConstraints, AssumedConstraints),

	type_assign_set_typeclass_constraints(Constraints, !TypeAssign),
	type_assign_set_typevarset(TVarSet, !TypeAssign),
	type_assign_set_constraint_proofs(Proofs, !TypeAssign).

typecheck__reduce_context_by_rule_application(InstanceTable, SuperClassTable,
		AssumedConstraints, Bindings, TVarSet0, TVarSet,
		Proofs0, Proofs, Constraints0, Constraints) :-
	typecheck__reduce_context_by_rule_application_2(InstanceTable,
		SuperClassTable, AssumedConstraints, Bindings, TVarSet0,
		TVarSet, Proofs0, Proofs, Constraints0, Constraints,
		Constraints0, _).

:- pred typecheck__reduce_context_by_rule_application_2(instance_table::in,
	superclass_table::in, list(class_constraint)::in, tsubst::in,
	tvarset::in, tvarset::out,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out,
	list(class_constraint)::in, list(class_constraint)::out,
	list(class_constraint)::in, list(class_constraint)::out) is det.

typecheck__reduce_context_by_rule_application_2(InstanceTable,
		SuperClassTable, AssumedConstraints, Bindings,
		!TVarSet, !Proofs, !Constraints, !Seen) :-
	InitialTVarSet = !.TVarSet,
	apply_rec_subst_to_constraint_list(Bindings, !Constraints),
	eliminate_assumed_constraints(AssumedConstraints, !Constraints,
		Changed1),
	apply_instance_rules(InstanceTable, !TVarSet, !Proofs, !Seen,
		!Constraints, Changed2),
	varset__vars(!.TVarSet, TVars),
	apply_class_rules(AssumedConstraints, TVars, SuperClassTable,
		InitialTVarSet, !Proofs, !Constraints, Changed3),
	(
		Changed1 = no, Changed2 = no, Changed3 = no
	->
			% We have reached fixpoint
		list__sort_and_remove_dups(!Constraints)
	;
		typecheck__reduce_context_by_rule_application_2(InstanceTable,
			SuperClassTable, AssumedConstraints, Bindings,
			!TVarSet, !Proofs, !Constraints, !Seen)
	).

:- pred eliminate_assumed_constraints(list(class_constraint)::in,
	list(class_constraint)::in, list(class_constraint)::out, bool::out)
	is det.

eliminate_assumed_constraints(_, [], [], no).
eliminate_assumed_constraints(AssumedCs, [C | Cs], NewCs, Changed) :-
	eliminate_assumed_constraints(AssumedCs, Cs, NewCs0, Changed0),
	( list__member(C, AssumedCs) ->
		NewCs = NewCs0,
		Changed = yes
	;
		NewCs = [C | NewCs0],
		Changed = Changed0
	).

:- pred apply_instance_rules(instance_table::in, tvarset::in, tvarset::out,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out,
	list(class_constraint)::in, list(class_constraint)::out,
	list(class_constraint)::in, list(class_constraint)::out, bool::out)
	is det.

apply_instance_rules(_, !TVarSet, !Proofs, !Seen, [], [], no).
apply_instance_rules(InstanceTable, !TVarSet, !Proofs, !Seen,
		[C | Cs], Constraints, Changed) :-
	C = constraint(ClassName, Types),
	list__length(Types, Arity),
	map__lookup(InstanceTable, class_id(ClassName, Arity), Instances),
	InitialTVarSet = !.TVarSet,
	(
		find_matching_instance_rule(Instances, ClassName, Types,
			!TVarSet, !Proofs, NewConstraints0),

			% Remove any constraints we've already seen.
			% This ensures we don't get into an infinite loop.
		NewConstraints1 = NewConstraints0 `list__delete_elems` !.Seen
	->
			% Put the new constraints at the front of the list
		NewConstraints = NewConstraints1,
		!:Seen = NewConstraints ++ !.Seen,
		Changed1 = yes
	;
			% Put the old constraint at the front of the list
		NewConstraints = [C],
		!:TVarSet = InitialTVarSet,
		Changed1 = no
	),
	apply_instance_rules(InstanceTable, !TVarSet, !Proofs, !Seen,
		Cs, TailConstraints, Changed2),
	bool__or(Changed1, Changed2, Changed),
	list__append(NewConstraints, TailConstraints, Constraints).

	% We take the first matching instance rule that we can find; any
	% overlapping instance declarations will have been caught earlier.

	% This pred also catches tautological constraints since the
	% NewConstraints will be [].

	% XXX Surely we shouldn't need to re-name the variables and return
	% XXX a new varset: this substitution should have been worked out
	% XXX before, as these varsets would already have been merged.
:- pred find_matching_instance_rule(list(hlds_instance_defn)::in, sym_name::in,
	list(type)::in, tvarset::in, tvarset::out,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out,
	list(class_constraint)::out) is semidet.

find_matching_instance_rule(Instances, ClassName, Types, !TVarSet, !Proofs,
		NewConstraints) :-
	% Start a counter so we remember which instance decl we have used.
	find_matching_instance_rule_2(Instances, 1, ClassName, Types,
		!TVarSet, !Proofs, NewConstraints).

:- pred find_matching_instance_rule_2(list(hlds_instance_defn)::in, int::in,
	sym_name::in, list(type)::in, tvarset::in, tvarset::out,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out,
	list(class_constraint)::out) is semidet.

find_matching_instance_rule_2([Instance | Instances], InstanceNum0, ClassName,
		Types, !TVarSet, !Proofs, NewConstraints) :-
	(
		NewConstraints0 = Instance ^ instance_constraints,
		InstanceTypes0 = Instance ^ instance_types,
		InstanceTVarSet = Instance ^ instance_tvarset,
		varset__merge_subst(!.TVarSet, InstanceTVarSet, NewTVarSet,
			RenameSubst),
		term__apply_substitution_to_list(InstanceTypes0,
			RenameSubst, InstanceTypes),
		type_list_subsumes(InstanceTypes, Types, Subst)
	->
		!:TVarSet = NewTVarSet,
		apply_subst_to_constraint_list(RenameSubst,
			NewConstraints0, NewConstraints1),
		apply_rec_subst_to_constraint_list(Subst,
			NewConstraints1, NewConstraints),

		NewProof = apply_instance(InstanceNum0),
		Constraint = constraint(ClassName, Types),
		map__set(!.Proofs, Constraint, NewProof, !:Proofs)
	;
		InstanceNum = InstanceNum0 + 1,
		find_matching_instance_rule_2(Instances, InstanceNum,
			ClassName, Types, !TVarSet, !Proofs, NewConstraints)
	).

	% To reduce a constraint using class declarations, we search the
	% superclass relation to find a path from the inferred constraint to
	% another (declared or inferred) constraint.
:- pred apply_class_rules(list(class_constraint)::in, list(tvar)::in,
	superclass_table::in, tvarset::in,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out,
	list(class_constraint)::in, list(class_constraint)::out, bool::out)
	is det.

apply_class_rules(_, _, _, _, !Proofs, [], [], no).
apply_class_rules(AssumedConstraints, HeadTypeParams, SuperClassTable, TVarSet,
		!Proofs, [Constraint0 | Constraints0], Constraints, Changed) :-
	(
		Parents = [],
		eliminate_constraint_by_class_rules(Constraint0, _, _,
			HeadTypeParams, AssumedConstraints, SuperClassTable,
			TVarSet, Parents, !Proofs)
	->
		apply_class_rules(AssumedConstraints, HeadTypeParams,
			SuperClassTable, TVarSet, !Proofs,
			Constraints0, Constraints, _),
		Changed = yes
	;
		apply_class_rules(AssumedConstraints, HeadTypeParams,
			SuperClassTable, TVarSet, !Proofs,
			Constraints0, TailConstraints, Changed),
		Constraints = [Constraint0 | TailConstraints]
	).

	% eliminate_constraint_by_class_rules eliminates a class constraint
	% by applying the superclass relation. A list of "parent" constraints
	% is also passed in --- these are the constraints that we are
	% (recursively) in the process of checking, and is used to ensure that
	% we don't get into a cycle in the relation.
	%
	% The list(tvar) argument contains all the variables from the
	% original constraint that we are trying to prove. (These are the
	% type variables that must not be bound as we search through the
	% superclass relation).
:- pred eliminate_constraint_by_class_rules(class_constraint::in,
	class_constraint::out, tsubst::out, list(tvar)::in,
	list(class_constraint)::in, superclass_table::in, tvarset::in,
	list(class_constraint)::in,
	map(class_constraint, constraint_proof)::in,
	map(class_constraint, constraint_proof)::out) is semidet.

eliminate_constraint_by_class_rules(C, SubstC, SubClassSubst, ConstVars,
		AssumedConstraints, SuperClassTable, TVarSet,
		ParentConstraints, Proofs0, Proofs) :-

		% Make sure we aren't in a cycle in the
		% superclass relation
	\+ list__member(C, ParentConstraints),

	C = constraint(SuperClassName, SuperClassTypes),
	list__length(SuperClassTypes, SuperClassArity),
	SuperClassId = class_id(SuperClassName, SuperClassArity),
	multi_map__search(SuperClassTable, SuperClassId, SubClasses),

		% Convert all the subclass_details into class_constraints by
		% doing the appropriate variable renaming and applying the
		% type variable bindings.
		% If the unification of the type variables for a particular
		% constraint fails then that constraint is eliminated because it
		% cannot contribute to proving the constraint we are trying to
		% prove.
	list__filter_map(subclass_details_to_constraint(TVarSet,
			SuperClassTypes),
		SubClasses, SubClassConstraints),

	(
			% Do the first level of search. We search for
			% an assumed constraint which unifies with any
			% of the subclass constraints.
		find_first_map(
			match_assumed_constraint(ConstVars,
				SubClassConstraints),
			AssumedConstraints, SubClass - SubClassSubst0)
	->
		SubClassSubst = SubClassSubst0,
		apply_rec_subst_to_constraint(SubClassSubst, C, SubstC),
		map__set(Proofs0, SubstC, superclass(SubClass), Proofs)
	;
		NewParentConstraints = [C | ParentConstraints],

			% Recursively search the rest of the superclass
			% relation.
		SubClassSearch = (pred(Constraint::in, CnstrtAndProof::out)
				is semidet :-
			eliminate_constraint_by_class_rules(Constraint,
				SubstConstraint, SubClassSubst0,
				ConstVars, AssumedConstraints, SuperClassTable,
				TVarSet, NewParentConstraints,
				Proofs0, SubProofs),
			CnstrtAndProof = {SubstConstraint, SubClassSubst0,
				SubProofs}
		),
			% XXX this could (and should) be more efficient.
			% (i.e. by manually doing a "cut").
		find_first_map(SubClassSearch, SubClassConstraints,
			{NewSubClass, SubClassSubst, NewProofs}),
		apply_rec_subst_to_constraint(SubClassSubst, C, SubstC),
		map__set(NewProofs, SubstC, superclass(NewSubClass), Proofs)
	).

:- pred match_assumed_constraint(list(tvar)::in, list(class_constraint)::in,
	class_constraint::in, pair(class_constraint, tsubst)::out) is semidet.

match_assumed_constraint(ConstVars, SubClassConstraints, AssumedConstraint,
		Match) :-
	find_first_map(
		match_assumed_constraint_2(ConstVars, AssumedConstraint),
		SubClassConstraints, Match).

:- pred match_assumed_constraint_2(list(tvar)::in, class_constraint::in,
	class_constraint::in, pair(class_constraint, tsubst)::out) is semidet.

match_assumed_constraint_2(ConstVars, AssumedConstraint,
		SubClassConstraint, Match) :-
	AssumedConstraint = constraint(AssumedConstraintClass,
		AssumedConstraintTypes),
	SubClassConstraint = constraint(AssumedConstraintClass,
		SubClassConstraintTypes),
	map__init(EmptySub),
	type_unify_list(SubClassConstraintTypes, AssumedConstraintTypes,
		ConstVars, EmptySub, AssumedConstraintSub),
	Match = AssumedConstraint - AssumedConstraintSub.

	% subclass_details_to_constraint will fail iff the call to
	% type_unify_list fails.

:- pred subclass_details_to_constraint(tvarset::in, list(type)::in,
	subclass_details::in, class_constraint::out) is semidet.

subclass_details_to_constraint(TVarSet, SuperClassTypes, SubClassDetails,
		SubC) :-
	SubClassDetails = subclass_details(SuperVars0, SubID, SubVars0,
		SuperVarSet),

		% Rename the variables from the typeclass
		% declaration into those of the current pred
	varset__merge_subst(TVarSet, SuperVarSet, _NewTVarSet, RenameSubst),
	term__var_list_to_term_list(SubVars0, SubVars1),
	term__apply_substitution_to_list(SubVars1, RenameSubst, SubVars),
	term__apply_substitution_to_list(SuperVars0, RenameSubst, SuperVars),

		% Work out what the (renamed) vars from the
		% typeclass declaration are bound to here.
	type_unify_list(SuperVars, SuperClassTypes, [], map__init, Bindings),
	SubID = class_id(SubName, _SubArity),
	term__apply_substitution_to_list(SubVars, Bindings, SubClassTypes),
	SubC = constraint(SubName, SubClassTypes).

	%
	% check_satisfiability(Constraints, HeadTypeParams):
	% 	Check that all of the constraints are satisfiable.
	%	Fail if any are definitely not satisfiable.
	%
	% We disallow ground constraints
	% for which there are no matching instance rules,
	% even though the module system means that it would
	% make sense to allow them: even if there
	% is no instance declaration visible in the current
	% module, there may be one visible in the caller.
	% The reason we disallow them is that in practice
	% allowing this causes type inference to let too
	% many errors slip through, with the error diagnosis
	% being too far removed from the real cause of the
	% error.  Note that ground constraints *are* allowed
	% if you declare them, since we removed declared
	% constraints before checking satisfiability.
	%
	% Similarly, for constraints on head type params
	% (universally quantified type vars in this pred's type decl,
	% or existentially quantified type vars in type decls for
	% callees), we know that the head type params can never get bound.
	% This means that if the constraint wasn't an assumed constraint
	% and can't be eliminated by instance rule or class rule
	% application, then we can report an error now, rather than
	% later.  (For non-head-type-param type variables,
	% we need to wait, in case the type variable gets bound
	% to a type for which there is a valid instance declaration.)
	%
	% So a constraint is considered satisfiable iff it
	% contains at least one type variable that is not in the
	% head type params.
	%
:- pred check_satisfiability(list(class_constraint)::in, head_type_params::in)
	is semidet.

check_satisfiability(Constraints, HeadTypeParams) :-
	all [Constraint] list__member(Constraint, Constraints) => (
		Constraint = constraint(_ClassName, Types),
		term__contains_var_list(Types, TVar),
		not list__member(TVar, HeadTypeParams)
	).

%-----------------------------------------------------------------------------%

:- pred convert_cons_defn_list(typecheck_info::in, list(hlds_cons_defn)::in,
	list(maybe_cons_type_info)::out) is det.

convert_cons_defn_list(_Info, [], []).
convert_cons_defn_list(Info, [X | Xs], [Y | Ys]) :-
	convert_cons_defn(Info, X, Y),
	convert_cons_defn_list(Info, Xs, Ys).

:- pred convert_cons_defn(typecheck_info::in, hlds_cons_defn::in,
	maybe_cons_type_info::out) is det.

convert_cons_defn(Info, HLDS_ConsDefn, ConsTypeInfo) :-
	HLDS_ConsDefn = hlds_cons_defn(ExistQVars, ExistConstraints, Args,
		TypeCtor, _),
	assoc_list__values(Args, ArgTypes),
	typecheck_info_get_types(Info, Types),
	map__lookup(Types, TypeCtor, TypeDefn),
	hlds_data__get_type_defn_tvarset(TypeDefn, ConsTypeVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, ConsTypeParams),
	hlds_data__get_type_defn_body(TypeDefn, Body),

	%
	% If this type has `:- pragma foreign_type' declarations, we
	% can only use its constructors in predicates which have foreign
	% clauses and in the unification and comparison predicates for
	% the type (otherwise the code wouldn't compile when using a
	% back-end which caused another version of the type to be selected).
	% The constructors may also appear in the automatically generated
	% unification and comparison predicates.
	%
	% XXX This check isn't quite right -- we really need to check for
	% each procedure that there is a foreign_proc declaration for all
	% languages for which this type has a foreign_type declaration, but
	% this will do for now. Such a check may be difficult because by
	% this point we've thrown away the clauses which we aren't using
	% in the current compilation.
	%
	% The `.opt' files don't contain the foreign clauses from the source
	% file that aren't used when compiling in the current grade, so we
	% allow foreign type constructors in `opt_imported' predicates even
	% if there are no foreign clauses. Errors will be caught when creating
	% the `.opt' file.
	%
	typecheck_info_get_predid(Info, PredId),
	typecheck_info_get_module_info(Info, ModuleInfo),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	(
		Body ^ du_type_is_foreign_type = yes(_),
		\+ pred_info_get_goal_type(PredInfo, clauses_and_pragmas),
		\+ is_unify_or_compare_pred(PredInfo),
		\+ pred_info_import_status(PredInfo, opt_imported)
	->
		ConsTypeInfo = error(foreign_type_constructor(TypeCtor,
			TypeDefn))
	;
		% Do not allow constructors for abstract_imported types unless
		% the current predicate is opt_imported.
		hlds_data__get_type_defn_status(TypeDefn, abstract_imported),
		\+ is_unify_or_compare_pred(PredInfo),
		\+ pred_info_import_status(PredInfo, opt_imported)
	->
		ConsTypeInfo = error(abstract_imported_type)
	;
		construct_type(TypeCtor, ConsTypeParams, ConsType),
		UnivConstraints = [],
		Constraints = constraints(UnivConstraints, ExistConstraints),
		ConsTypeInfo = ok(cons_type_info(ConsTypeVarSet, ExistQVars,
			ConsType, ArgTypes, Constraints))
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The type_assign and type_assign_set data structures.

:- type type_assign_set	==	list(type_assign).

:- type type_assign --->
	type_assign(
		var_types		:: map(prog_var, type),
		type_varset		:: tvarset,
					% type names
		head_type_params	:: headtypes,
					% universally quantified type variables
		type_bindings		:: tsubst,
					% type bindings
		class_constraints	:: class_constraints,
			% This field has the form
			% `constraints(Universal, Existential)',
			% The first element in this pair
			% (the "universal" constraints) holds
			% the constraints that we must prove,
			% i.e. universal constraints from callees,
			% or existential constraints on the declaration
			% of the predicate we are analyzing.
			% The second element in the pair
			% (the "existential" constraints) holds
			% the constraints we can assume,
			% i.e. existential constraints from callees,
			% or universal constraints on the declaration
			% of the predicate we are analyzing.
		constraint_proofs	:: map(class_constraint,
						constraint_proof)
					% for each constraint
					% found to be redundant,
					% why is it so?
	).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_var_types(type_assign::in,
	map(prog_var, type)::out) is det.
:- pred type_assign_get_typevarset(type_assign::in,
	tvarset::out) is det.
:- pred type_assign_get_head_type_params(type_assign::in,
	headtypes::out) is det.
:- pred type_assign_get_type_bindings(type_assign::in,
	tsubst::out) is det.
:- pred type_assign_get_typeclass_constraints(type_assign::in,
	class_constraints::out) is det.
:- pred type_assign_get_constraint_proofs(type_assign::in,
	map(class_constraint, constraint_proof)::out) is det.

type_assign_get_var_types(TA, TA ^ var_types).
type_assign_get_typevarset(TA, TA ^ type_varset).
type_assign_get_head_type_params(TA, TA ^ head_type_params).
type_assign_get_type_bindings(TA, TA ^ type_bindings).
type_assign_get_typeclass_constraints(TA, TA ^ class_constraints).
type_assign_get_constraint_proofs(TA, TA ^ constraint_proofs).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_var_types(map(prog_var, type)::in,
	type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typevarset(tvarset::in,
	type_assign::in, type_assign::out) is det.
:- pred type_assign_set_head_type_params(headtypes::in,
	type_assign::in, type_assign::out) is det.
:- pred type_assign_set_type_bindings(tsubst::in,
	type_assign::in, type_assign::out) is det.
:- pred type_assign_set_typeclass_constraints(class_constraints::in,
	type_assign::in, type_assign::out) is det.
:- pred type_assign_set_constraint_proofs(
	map(class_constraint, constraint_proof)::in,
	type_assign::in, type_assign::out) is det.

type_assign_set_var_types(X, TA, TA ^ var_types := X).
type_assign_set_typevarset(X, TA, TA ^ type_varset := X).
type_assign_set_head_type_params(X, TA, TA ^ head_type_params := X).
type_assign_set_type_bindings(X, TA, TA ^ type_bindings := X).
type_assign_set_typeclass_constraints(X, TA, TA ^ class_constraints := X).
type_assign_set_constraint_proofs(X, TA, TA ^ constraint_proofs := X).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The following section contains predicates for writing diagnostics
% (warnings and errors).

%-----------------------------------------------------------------------------%

	% write out the inferred `pred' or `func' declarations
	% for a list of predicates.  Don't write out the inferred types
	% for assertions.

:- pred write_inference_messages(list(pred_id)::in, module_info::in,
	io::di, io::uo) is det.

write_inference_messages([], _, !IO).
write_inference_messages([PredId | PredIds], ModuleInfo, !IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	(
		check_marker(Markers, infer_type),
		module_info_predids(ModuleInfo, ValidPredIds),
		list__member(PredId, ValidPredIds),
		\+ pred_info_get_goal_type(PredInfo, promise(_))
	->
		write_inference_message(PredInfo, !IO)
	;
		true
	),
	write_inference_messages(PredIds, ModuleInfo, !IO).

	% write out the inferred `pred' or `func' declaration
	% for a single predicate.

:- pred write_inference_message(pred_info::in, io::di, io::uo) is det.

write_inference_message(PredInfo, !IO) :-
	PredName = pred_info_name(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	Name = unqualified(PredName),
	pred_info_context(PredInfo, Context),
	pred_info_arg_types(PredInfo, VarSet, ExistQVars, Types0),
	strip_builtin_qualifiers_from_type_list(Types0, Types),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_get_purity(PredInfo, Purity),
	MaybeDet = no,
	prog_out__write_context(Context, !IO),
	io__write_string("Inferred ", !IO),
	AppendVarNums = no,
	(
		PredOrFunc = predicate,
		mercury_output_pred_type(VarSet, ExistQVars, Name, Types,
			MaybeDet, Purity, ClassContext, Context, AppendVarNums,
			!IO)
	;	PredOrFunc = function,
		pred_args_to_func_args(Types, ArgTypes, RetType),
		mercury_output_func_type(VarSet, ExistQVars, Name, ArgTypes,
			RetType, MaybeDet, Purity, ClassContext, Context,
			AppendVarNums, !IO)
	).

%-----------------------------------------------------------------------------%

:- pred report_no_clauses(string::in, pred_id::in, pred_info::in,
	module_info::in, io::di, io::uo) is det.

report_no_clauses(MessageKind, PredId, PredInfo, ModuleInfo, !IO) :-
	pred_info_context(PredInfo, Context),
	PredPieces = describe_one_pred_name(ModuleInfo,	
		should_not_module_qualify, PredId),
	ErrorMsg = [words(MessageKind ++ ": no clauses for ") | PredPieces] ++
		[suffix(".")],
	error_util__write_error_pieces(Context, 0, ErrorMsg, !IO).

%-----------------------------------------------------------------------------%

:- pred report_warning_too_much_overloading(typecheck_info::in,
	io::di, io::uo) is det.

report_warning_too_much_overloading(Info, !IO) :-
	typecheck_info_get_context(Info, Context),
	make_pred_id_preamble(Info, Preamble),
	SmallWarning = [fixed(Preamble),
		words("warning: highly ambiguous overloading.") ],
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	(
		VerboseErrors = yes,
		VerboseWarning = [
			words("This may cause type-checking to be very"),
			words("slow. It may also make your code"),
			words("difficult to understand.") ],
		list__append(SmallWarning, VerboseWarning, Warning)
	;
		VerboseErrors = no,
		Warning = SmallWarning
	),
	error_util__report_warning(Context, 0, Warning, !IO).

%-----------------------------------------------------------------------------%

:- pred report_error_unif_var_var(typecheck_info::in,
	prog_var::in, prog_var::in, type_assign_set::in, io::di, io::uo)
	is det.

report_error_unif_var_var(Info, X, Y, TypeAssignSet) -->
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_varset(Info, VarSet) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },

	write_context_and_pred_id(Info),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  type error in unification of variable `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  and variable `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("'.\n"),

	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(X, VarSet, no),
	io__write_string("'"),
	write_type_of_var(Info, Context, TypeAssignSet, X),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("'"),
	write_type_of_var(Info, Context, TypeAssignSet, Y),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_functor_type(typecheck_info::in,
	prog_var::in, list(cons_type_info)::in, cons_id::in, int::in,
	type_assign_set::in, io::di, io::uo) is det.

report_error_functor_type(Info, Var, ConsDefnList, Functor, Arity,
		TypeAssignSet) -->
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_varset(Info, VarSet) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },

	write_context_and_pred_id(Info),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  type error in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  and "),
	write_functor_name(Functor, Arity),
	io__write_string(".\n"),

	prog_out__write_context(Context),
	io__write_string("  "),
	write_argument_name(VarSet, Var),
	write_type_of_var(Info, Context, TypeAssignSet, Var),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  "),
	write_functor_name(Functor, Arity),
	write_type_of_functor(Functor, Arity, Context, ConsDefnList),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_lambda_var(typecheck_info::in, pred_or_func::in,
	lambda_eval_method::in, prog_var::in, list(prog_var)::in,
	type_assign_set::in, io::di, io::uo) is det.

report_error_lambda_var(Info, PredOrFunc, EvalMethod, Var, ArgVars,
		TypeAssignSet) -->
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_varset(Info, VarSet) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },

	write_context_and_pred_id(Info),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  type error in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string("\n"),
	prog_out__write_context(Context),

	{ EvalMethod = normal, EvalStr = ""
	; EvalMethod = (aditi_bottom_up), EvalStr = "aditi_bottom_up "
	},

	(
		{ PredOrFunc = predicate },
		io__write_string("  and `"),
		io__write_string(EvalStr),
		io__write_string("pred("),
		mercury_output_vars(ArgVars, VarSet, no),
		io__write_string(") :- ...':\n")
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgVars, FuncArgs, RetVar) },
		io__write_string("  and `"),
		io__write_string(EvalStr),
		io__write_string("func("),
		mercury_output_vars(FuncArgs, VarSet, no),
		io__write_string(") = "),
		mercury_output_var(RetVar, VarSet, no),
		io__write_string(" :- ...':\n")
	),

	prog_out__write_context(Context),
	io__write_string("  "),
	write_argument_name(VarSet, Var),
	write_type_of_var(Info, Context, TypeAssignSet, Var),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  lambda expression has type `"),
	(
		{ PredOrFunc = predicate },
		io__write_string("pred"),
		( { ArgVars = [] } ->
			[]
		;
			io__write_string("(_"),
			{ list__length(ArgVars, NumArgVars) },
			{ NumArgVars1 = NumArgVars - 1 },
			{ list__duplicate(NumArgVars1, ", _", Strings) },
			io__write_strings(Strings),
			io__write_string(")")
		)
	;
		{ PredOrFunc = function },
		io__write_string("func"),
		{ pred_args_to_func_args(ArgVars, FuncArgs2, _) },
		( { FuncArgs2 = [] } ->
			[]
		;
			io__write_string("(_"),
			{ list__length(FuncArgs2, NumArgVars) },
			{ NumArgVars1 = NumArgVars - 1 },
			{ list__duplicate(NumArgVars1, ", _", Strings) },
			io__write_strings(Strings),
			io__write_string(")")
		),
		io__write_string(" = _")
	),
	io__write_string("'.\n"),
	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_functor_arg_types(typecheck_info::in, prog_var::in,
	list(cons_type_info)::in, cons_id::in, list(prog_var)::in,
	args_type_assign_set::in, io::di, io::uo) is det.

report_error_functor_arg_types(Info, Var, ConsDefnList, Functor, Args,
		ArgsTypeAssignSet) -->
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_varset(Info, VarSet) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	{ typecheck_info_get_module_info(Info, ModuleInfo) },
	{ list__length(Args, Arity) },

	write_context_and_pred_id(Info),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  and term `"),
	{ strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor) },
	hlds_out__write_functor_cons_id(StrippedFunctor, Args, VarSet,
		ModuleInfo, no),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  type error in argument(s) of "),
	write_functor_name(StrippedFunctor, Arity),
	io__write_string(".\n"),

	{ ConsArgTypesSet = list__map(get_callee_arg_types,
		ArgsTypeAssignSet) },

	% If we know the type of the function symbol, and each argument
	% also has at most one possible type, then we prefer to print an
	% error message that mentions the actual and expected types of the
	% arguments only for the arguments in which the two types differ.
	(
		{ list__all_same(ConsArgTypesSet) },
		{ ConsArgTypesSet = [ConsArgTypes | _] },
		{ assoc_list__from_corresponding_lists(Args, ConsArgTypes,
			ArgExpTypes) },
		{ TypeAssigns = list__map(get_caller_arg_assign,
			ArgsTypeAssignSet) },
		{ find_mismatched_args(ArgExpTypes, TypeAssigns, 1,
			SimpleMismatches, ComplexMismatches, AllMismatches) },
		{ require(list__is_not_empty(AllMismatches),
			"report_error_functor_arg_types: no mismatches") },
		{ ComplexMismatches = [] }
	->
		report_mismatched_args(SimpleMismatches, yes, VarSet, Functor,
			Context)
	;
		% XXX If we can compute AllMismatches, then we should use it
		% to report which arguments are OK, and which are suspect.

		{ convert_args_type_assign_set(ArgsTypeAssignSet,
			TypeAssignSet) },

		%
		% For polymorphic data structures,
		% the type of `Var' (the functor's result type)
		% can affect the valid types for the arguments.
		%
		(
			% could the type of the functor be polymorphic?
			{ list__member(ConsDefn, ConsDefnList) },
			{ ConsDefn = cons_type_info(_, _, _, ConsArgTypes, _) },
			{ ConsArgTypes \= [] }
		->
			% if so, print out the type of `Var'
			prog_out__write_context(Context),
			io__write_string("  "),
			write_argument_name(VarSet, Var),
			write_type_of_var(Info, Context, TypeAssignSet, Var),
			io__write_string(",\n")
		;
			[]
		),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_functor_name(Functor, Arity),
		write_type_of_functor(Functor, Arity, Context, ConsDefnList),

		write_types_of_vars(Args, VarSet, Context, Info,
			TypeAssignSet),

		write_type_assign_set_msg(TypeAssignSet, VarSet)
	).

:- type mismatch_info
	--->	mismatch_info(
			int,		% argument number, starting from 1
			prog_var,	% variable in that position
			list(type_mismatch)
					% list of possible type mismatches
		).

:- type type_mismatch
	--->	type_mismatch(
			type,		% actual type of that variable
			type,		% expected type of that variable
			tvarset,	% the type vars in the expected
					% and expected types
			head_type_params % existentially quantified type vars
		).

:- pred find_mismatched_args(assoc_list(prog_var, type)::in,
	type_assign_set::in, int::in, list(mismatch_info)::out,
	list(mismatch_info)::out, list(mismatch_info)::out) is det.

find_mismatched_args([], _, _, [], [], []).
find_mismatched_args([Arg - ExpType | ArgExpTypes], TypeAssignSet, ArgNum0,
		SimpleMismatches, ComplexMismatches, AllMismatches) :-
	ArgNum1 = ArgNum0 + 1,
	find_mismatched_args(ArgExpTypes, TypeAssignSet, ArgNum1,
		SimpleMismatchesTail, ComplexMismatchesTail,
		AllMismatchesTail),
	get_type_stuff(TypeAssignSet, Arg, TypeStuffList),
	list__filter_map(substitute_types_check_match(ExpType), TypeStuffList,
		TypeMismatches0),
	list__sort_and_remove_dups(TypeMismatches0, TypeMismatches),
	(
		TypeMismatches = [],
		SimpleMismatches = SimpleMismatchesTail,
		ComplexMismatches = ComplexMismatchesTail,
		AllMismatches = AllMismatchesTail
	;
		TypeMismatches = [_],
		Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
		SimpleMismatches = [Mismatch | SimpleMismatchesTail],
		ComplexMismatches = ComplexMismatchesTail,
		AllMismatches = [Mismatch | AllMismatchesTail]
	;
		TypeMismatches = [_, _ | _],
		Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
		SimpleMismatches = SimpleMismatchesTail,
		ComplexMismatches = [Mismatch | ComplexMismatchesTail],
		AllMismatches = [Mismatch | AllMismatchesTail]
	).

:- pred substitute_types_check_match((type)::in, type_stuff::in,
	type_mismatch::out) is semidet.

substitute_types_check_match(ExpType, TypeStuff, TypeMismatch) :-
	TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, HeadTypeParams),
	term__apply_rec_substitution(ArgType, TypeBindings, FullArgType),
	term__apply_rec_substitution(ExpType, TypeBindings, FullExpType),
	(
		(
			% there is no mismatch if the actual type of the
			% argument is the same as the expected type
			identical_types(FullArgType, FullExpType)
		;
			% there is no mismatch if the actual type of the
			% argument has no constraints on it
			FullArgType = term__functor(term__atom("<any>"), [], _)
		)
	->
		fail
	;
		TypeMismatch = type_mismatch(FullArgType, FullExpType,
			TVarSet, HeadTypeParams)
	).

:- pred report_mismatched_args(list(mismatch_info)::in, bool::in,
	prog_varset::in, cons_id::in, prog_context::in, io::di, io::uo) is det.

report_mismatched_args([], _, _, _, _) --> [].
report_mismatched_args([Mismatch | Mismatches], First, VarSet, Functor,
		Context) -->
	{ Mismatch = mismatch_info(ArgNum, Var, TypeMismatches) },
	{ TypeMismatches = [TypeMismatch] ->
		TypeMismatch = type_mismatch(ActType, ExpType, TVarSet,
			HeadTypeParams)
	;
		error("report_mismatched_args: more than one type mismatch")
	},
	prog_out__write_context(Context),
	(
		% Handle higher-order syntax such as ''(F, A) specially:
		% output
		%	Functor (F) has type ...;
		%	argument 1 (A) has type ...
		% instead of
		%	Argument 1 (F) has type ...;
		%	argument 2 (A) has type ...
		{ Functor = cons(unqualified(""), Arity) },
		{ Arity > 0 }
	->
		( { First = yes } ->
			io__write_string("  Functor")
		;
			io__write_string("  argument "),
			io__write_int(ArgNum - 1)
		)
	;
		( { First = yes } ->
			io__write_string("  Argument ")
		;
			io__write_string("  argument ")
		),
		io__write_int(ArgNum)
	),
	( { varset__search_name(VarSet, Var, _) } ->
		io__write_string(" ("),
		mercury_output_var(Var, VarSet, no),
		io__write_string(")")
	;
		[]
	),
	io__write_string(" has type `"),
	output_type(ActType, TVarSet, HeadTypeParams),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected type was `"),
	output_type(ExpType, TVarSet, HeadTypeParams),
	( { Mismatches = [] } ->
		io__write_string("'.\n")
	;
		io__write_string("';\n"),
		report_mismatched_args(Mismatches, no, VarSet, Functor, Context)
	).

:- pred write_types_of_vars(list(prog_var)::in, prog_varset::in,
	prog_context::in, typecheck_info::in, type_assign_set::in,
	io::di, io::uo) is det.

write_types_of_vars([], _, _, _, _) -->
	io__write_string(".\n").
write_types_of_vars([Var | Vars], VarSet, Context, Info, TypeAssignSet) -->
	io__write_string(",\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	write_argument_name(VarSet, Var),
	write_type_of_var(Info, Context, TypeAssignSet, Var),
	write_types_of_vars(Vars, VarSet, Context, Info, TypeAssignSet).

:- pred write_argument_name(prog_varset::in, prog_var::in, io::di, io::uo)
	is det.

write_argument_name(VarSet, Var) -->
	( { varset__search_name(VarSet, Var, _) } ->
		io__write_string("variable `"),
		mercury_output_var(Var, VarSet, no),
		io__write_string("'")
	;
		io__write_string("argument")
	).

:- pred write_functor_name(cons_id::in, int::in, io::di, io::uo) is det.

write_functor_name(Functor, Arity) -->
	{ strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor) },
	( { Arity = 0 } ->
		io__write_string("constant `"),
		( { Functor = cons(Name, _) } ->
			prog_out__write_sym_name(Name)
		;
			hlds_out__write_cons_id(StrippedFunctor)
		),
		io__write_string("'")
	; { Functor = cons(unqualified(""), _) } ->
		io__write_string("higher-order term (with arity "),
		io__write_int(Arity - 1),
		io__write_string(")")
	;
		io__write_string("functor `"),
		hlds_out__write_cons_id(StrippedFunctor),
		io__write_string("'")
	).

:- pred write_type_of_var(typecheck_info::in, prog_context::in,
	type_assign_set::in, prog_var::in, io::di, io::uo) is det.

write_type_of_var(_Info, Context, TypeAssignSet, Var, !IO) :-
	get_type_stuff(TypeAssignSet, Var, TypeStuffList),
	TypeStrs0 = list__map(typestuff_to_typestr, TypeStuffList),
	list__sort_and_remove_dups(TypeStrs0, TypeStrs),
	( TypeStrs = [TypeStr] ->
		io__write_string(" has type `", !IO),
		io__write_string(TypeStr, !IO),
		io__write_string("'", !IO)
	;
		io__write_string(" has overloaded type {\n", !IO),
		write_types_list(Context, TypeStrs, !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  }", !IO)
	).

:- pred write_type_of_functor(cons_id::in, int::in, prog_context::in,
	list(cons_type_info)::in, io::di, io::uo) is det.

write_type_of_functor(Functor, Arity, Context, ConsDefnList) -->
	( { ConsDefnList = [SingleDefn] } ->
		io__write_string(" has type "),
		( { Arity \= 0 } ->
			io__write_string("\n"),
			prog_out__write_context(Context),
			io__write_string("  `")
		;
			io__write_string("`")
		),
		write_cons_type(SingleDefn, Functor, Context),
		io__write_string("'")
	;
		io__write_string(" has overloaded type\n"),
		prog_out__write_context(Context),
		io__write_string("  { "),
		write_cons_type_list(ConsDefnList, Functor, Arity, Context),
		io__write_string(" }")
	).

:- pred write_cons_type(cons_type_info::in, cons_id::in, prog_context::in,
	io::di, io::uo) is det.

	% XXX Should we mention the context here?
write_cons_type(cons_type_info(TVarSet, ExistQVars, ConsType, ArgTypes, _),
		Functor, _) -->
	( { ArgTypes \= [] } ->
		( { cons_id_and_args_to_term(Functor, ArgTypes, Term) } ->
			output_type(Term, TVarSet, ExistQVars)
		;
			{ error("typecheck:write_cons_type - invalid cons_id") }
		),
		io__write_string(": ")
	;
		[]
	),
	output_type(ConsType, TVarSet, ExistQVars).

:- pred write_cons_type_list(list(cons_type_info)::in, cons_id::in, int::in,
	prog_context::in, io::di, io::uo) is det.

write_cons_type_list([], _, _, _) --> [].
write_cons_type_list([ConsDefn | ConsDefns], Functor, Arity, Context) -->
	write_cons_type(ConsDefn, Functor, Context),
	( { ConsDefns = [] } ->
		[]
	;
		( { Arity = 0 } ->
			io__write_string(", ")
		;
			io__write_string(",\n"),
			prog_out__write_context(Context),
			io__write_string("  ")
		),
		write_cons_type_list(ConsDefns, Functor, Arity, Context)
	).

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set_msg(type_assign_set::in, prog_varset::in,
	io::di, io::uo) is det.

write_type_assign_set_msg(TypeAssignSet, VarSet) -->
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		( { TypeAssignSet = [_] } ->
			io__write_string(
				"\tThe partial type assignment was:\n")
		;
			io__write_string("\tThe possible partial type " ++
				"assignments were:\n")
		),
		write_type_assign_set(TypeAssignSet, VarSet)
	;
		[]
	).

:- pred write_args_type_assign_set_msg(args_type_assign_set::in,
	prog_varset::in, io::di, io::uo) is det.

write_args_type_assign_set_msg(ArgTypeAssignSet, VarSet) -->
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		( { ArgTypeAssignSet = [_] } ->
			io__write_string(
				"\tThe partial type assignment was:\n")
		;
			io__write_string("\tThe possible partial type " ++
				"assignments were:\n")
		),
		write_args_type_assign_set(ArgTypeAssignSet, VarSet)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set(type_assign_set::in, prog_varset::in,
	io::di, io::uo) is det.

write_type_assign_set([], _) --> [].
write_type_assign_set([TypeAssign | TypeAssigns], VarSet) -->
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_type_assign_set(TypeAssigns, VarSet).

:- pred write_args_type_assign_set(args_type_assign_set::in, prog_varset::in,
	io::di, io::uo) is det.

write_args_type_assign_set([], _) --> [].
write_args_type_assign_set([ArgTypeAssign | ArgTypeAssigns], VarSet) -->
	{ ArgTypeAssign = args(TypeAssign, _ArgTypes, _Cnstrs) },
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_args_type_assign_set(ArgTypeAssigns, VarSet).

:- pred write_type_assign(type_assign::in, prog_varset::in, io::di, io::uo)
	is det.

write_type_assign(TypeAssign, VarSet, !IO) :-
	type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
	type_assign_get_var_types(TypeAssign, VarTypes),
	type_assign_get_typeclass_constraints(TypeAssign, Constraints),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TypeVarSet),
	map__keys(VarTypes, Vars),
	( HeadTypeParams = [] ->
		true
	;
		io__write_string("some [", !IO),
		mercury_output_vars(HeadTypeParams, TypeVarSet, no, !IO),
		io__write_string("]\n\t", !IO)
	),
	write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
		TypeVarSet, no, !IO),
	write_type_assign_constraints(Constraints, TypeBindings, TypeVarSet,
		!IO),
	io__write_string("\n", !IO).

:- pred write_type_assign_types(list(prog_var)::in, prog_varset::in,
	map(prog_var, type)::in, tsubst::in, tvarset::in, bool::in,
	io::di, io::uo) is det.

write_type_assign_types([], _, _, _, _, FoundOne, !IO) :-
	( FoundOne = no ->
		io__write_string("(No variables were assigned a type)", !IO)
	;
		true
	).
write_type_assign_types([Var | Vars], VarSet, VarTypes, TypeBindings,
		TypeVarSet, FoundOne, !IO) :-
	(
		map__search(VarTypes, Var, Type)
	->
		( FoundOne = yes ->
			io__write_string("\n\t", !IO)
		;
			true
		),
		mercury_output_var(Var, VarSet, no, !IO),
		io__write_string(": ", !IO),
		write_type_b(Type, TypeVarSet, TypeBindings, !IO),
		write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, yes, !IO)
	;
		write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, FoundOne, !IO)
	).

:- pred write_type_assign_constraints(class_constraints::in,
	tsubst::in, tvarset::in, io::di, io::uo) is det.

write_type_assign_constraints(Constraints, TypeBindings, TypeVarSet, !IO) :-
	Constraints = constraints(ConstraintsToProve, AssumedConstraints),
	write_type_assign_constraints("&", AssumedConstraints,
		TypeBindings, TypeVarSet, no, !IO),
	write_type_assign_constraints("<=", ConstraintsToProve,
		TypeBindings, TypeVarSet, no, !IO).

:- pred write_type_assign_constraints(string::in, list(class_constraint)::in,
	tsubst::in, tvarset::in, bool::in, io::di, io::uo) is det.

write_type_assign_constraints(_, [], _, _, _, !IO).
write_type_assign_constraints(Operator, [Constraint | Constraints],
		TypeBindings, TypeVarSet, FoundOne, !IO) :-
	( FoundOne = no ->
		io__write_strings(["\n\t", Operator, " "], !IO)
	;
		io__write_string(",\n\t   ", !IO)
	),
	apply_rec_subst_to_constraint(TypeBindings, Constraint,
		BoundConstraint),
	AppendVarNums = no,
	mercury_output_constraint(TypeVarSet, AppendVarNums, BoundConstraint,
		!IO),
	write_type_assign_constraints(Operator, Constraints, TypeBindings,
		TypeVarSet, yes, !IO).

	% write_type_b writes out a type after applying the type bindings.

:- pred write_type_b((type)::in, tvarset::in, tsubst::in, head_type_params::in,
	io::di, io::uo) is det.

write_type_b(Type0, TypeVarSet, TypeBindings, HeadTypeParams, !IO) :-
	Type = maybe_add_existential_quantifier(HeadTypeParams, Type0),
	write_type_b(Type, TypeVarSet, TypeBindings, !IO).

:- pred write_type_b((type)::in, tvarset::in, tsubst::in, io::di, io::uo)
	is det.

write_type_b(Type, TypeVarSet, TypeBindings, !IO) :-
	term__apply_rec_substitution(Type, TypeBindings, Type2),
	strip_builtin_qualifiers_from_type(Type2, Type3),
	mercury_output_term(Type3, TypeVarSet, no, !IO).

:- func typestuff_to_typestr(type_stuff) = string.

typestuff_to_typestr(TypeStuff) = TypeStr :-
	TypeStuff = type_stuff(Type0, TypeVarSet, TypeBindings,
		HeadTypeParams),
	term__apply_rec_substitution(Type0, TypeBindings, Type1),
	strip_builtin_qualifiers_from_type(Type1, Type2),
	Type = maybe_add_existential_quantifier(HeadTypeParams, Type2),
	TypeStr = mercury_term_to_string(Type, TypeVarSet, no).

	%
	% Check if any of the type variables in the type are existentially
	% quantified (occur in HeadTypeParams), and if so, add an
	% appropriate existential quantifier at the front of the type.
	%
:- func maybe_add_existential_quantifier(head_type_params, (type)) = (type).

maybe_add_existential_quantifier(HeadTypeParams, Type0) = Type :-
	prog_type__vars(Type0, TVars),
	ExistQuantTVars = set__to_sorted_list(set__intersect(
		set__list_to_set(HeadTypeParams), set__list_to_set(TVars))),
	( ExistQuantTVars = [] ->
		Type = Type0
	;
		Type = term__functor(term__atom("some"),
			[make_list_term(ExistQuantTVars), Type0],
			term__context_init)
	).

:- func make_list_term(list(tvar)) = (type).

make_list_term([]) = term__functor(term__atom("[]"), [], term__context_init).
make_list_term([Var | Vars]) = term__functor(term__atom("[|]"),
	[term__variable(Var), make_list_term(Vars)], term__context_init).

%-----------------------------------------------------------------------------%

:- pred report_error_var(typecheck_info::in, prog_var::in, (type)::in,
	type_assign_set::in, io::di, io::uo) is det.

report_error_var(Info, Var, Type, TypeAssignSet0) -->
	{ typecheck_info_get_pred_markers(Info, PredMarkers) },
	{ typecheck_info_get_called_predid(Info, CalledPredId) },
	{ typecheck_info_get_arg_num(Info, ArgNum) },
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	{ get_type_stuff(TypeAssignSet0, Var, TypeStuffList) },
	{ typecheck_info_get_varset(Info, VarSet) },
	write_context_and_pred_id(Info),
	write_call_context(Context, PredMarkers,
		CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	( { TypeStuffList = [SingleTypeStuff] } ->
		write_argument_name(VarSet, Var),
		{ SingleTypeStuff = type_stuff(VType, TVarSet, TBinding,
			HeadTypeParams) },
		io__write_string(" has type `"),
		write_type_b(VType, TVarSet, TBinding, HeadTypeParams),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		write_type_b(Type, TVarSet, TBinding, HeadTypeParams),
		io__write_string("'.\n")
	;
		io__write_string("type of "),
		write_argument_name(VarSet, Var),
		io__write_string(" does not match its expected type;\n"),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_argument_name(VarSet, Var),
		io__write_string(" has overloaded actual/expected types {\n"),

		write_var_type_stuff_list(Context, TypeStuffList, Type),
		io__write_string("\n"),

		prog_out__write_context(Context),
		io__write_string("  }.\n")
	),
	write_type_assign_set_msg(TypeAssignSet0, VarSet).

:- pred report_error_arg_var(typecheck_info::in, prog_var::in,
	args_type_assign_set::in, io::di, io::uo) is det.

report_error_arg_var(Info, Var, ArgTypeAssignSet0) -->
	{ typecheck_info_get_pred_markers(Info, PredMarkers) },
	{ typecheck_info_get_called_predid(Info, CalledPredId) },
	{ typecheck_info_get_arg_num(Info, ArgNum) },
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	{ get_arg_type_stuff(ArgTypeAssignSet0, Var, ArgTypeStuffList) },
	{ typecheck_info_get_varset(Info, VarSet) },
	write_context_and_pred_id(Info),
	write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
		UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	( { ArgTypeStuffList = [SingleArgTypeStuff] } ->
		write_argument_name(VarSet, Var),
		{ SingleArgTypeStuff = arg_type_stuff(Type0, VType0, TVarSet,
			HeadTypeParams) },
		io__write_string(" has type `"),
		output_type(VType0, TVarSet, HeadTypeParams),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		output_type(Type0, TVarSet, HeadTypeParams),
		io__write_string("'.\n")
	;
		io__write_string("type of "),
		write_argument_name(VarSet, Var),
		io__write_string(" does not match its expected type;\n"),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_argument_name(VarSet, Var),
		io__write_string(" has overloaded actual/expected types {\n"),

		write_arg_type_stuff_list(Context, ArgTypeStuffList),
		io__write_string("\n"),

		prog_out__write_context(Context),
		io__write_string("  }.\n")
	),
	write_args_type_assign_set_msg(ArgTypeAssignSet0, VarSet).

:- pred output_type((type)::in, tvarset::in, head_type_params::in,
	io::di, io::uo) is det.

output_type(Type0, TVarSet, HeadTypeParams, !IO) :-
	strip_builtin_qualifiers_from_type(Type0, Type1),
	Type = maybe_add_existential_quantifier(HeadTypeParams, Type1),
	mercury_output_term(Type, TVarSet, no, !IO).

:- pred write_types_list(prog_context::in, list(string)::in,
	io::di, io::uo) is det.

write_types_list(_Context, []) --> [].
write_types_list(Context, [Type | Types]) -->
	prog_out__write_context(Context),
	io__write_string("    "),
	io__write_string(Type),
	( { Types = [] } ->
		io__write_string("\n")
	;
		io__write_string(",\n"),
		write_types_list(Context, Types)
	).

:- pred write_type_stuff(type_stuff::in, io::di, io::uo) is det.

write_type_stuff(type_stuff(Type, TVarSet, TypeBinding, HeadTypeParams),
		!IO) :-
	write_type_b(Type, TVarSet, TypeBinding, HeadTypeParams, !IO).

:- pred write_var_type_stuff_list(prog_context::in, list(type_stuff)::in,
	(type)::in, io::di, io::uo) is det.

write_var_type_stuff_list(Context, TypeStuffs, Type, !IO) :-
	io__write_list(TypeStuffs, ",\n", write_var_type_stuff(Context, Type),
		!IO).

:- pred write_var_type_stuff(prog_context::in, (type)::in, type_stuff::in,
	io::di, io::uo) is det.

write_var_type_stuff(Context, Type, VarTypeStuff) -->
	{ VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding,
		HeadTypeParams) },
	prog_out__write_context(Context),
	io__write_string("    (inferred) "),
	write_type_b(VarType, TVarSet, TypeBinding, HeadTypeParams),
	io__write_string(",\n"),
	prog_out__write_context(Context),
	io__write_string("    (expected) "),
	write_type_b(Type, TVarSet, TypeBinding, HeadTypeParams).

:- pred write_arg_type_stuff_list(prog_context::in, list(arg_type_stuff)::in,
	io::di, io::uo) is det.

write_arg_type_stuff_list(Context, TypeStuffs, !IO) :-
	io__write_list(TypeStuffs, ",\n", write_arg_type_stuff(Context), !IO).

:- pred write_arg_type_stuff(prog_context::in, arg_type_stuff::in,
	io::di, io::uo) is det.

write_arg_type_stuff(Context, ArgTypeStuff) -->
	{ ArgTypeStuff = arg_type_stuff(Type, VarType, TVarSet,
		HeadTypeParams) },
	prog_out__write_context(Context),
	io__write_string("    (inferred) "),
	output_type(VarType, TVarSet, HeadTypeParams),
	io__write_string(",\n"),
	prog_out__write_context(Context),
	io__write_string("    (expected) "),
	output_type(Type, TVarSet, HeadTypeParams).

%-----------------------------------------------------------------------------%

:- pred report_error_undef_pred(typecheck_info::in, simple_call_id::in,
	io::di, io::uo) is det.

report_error_undef_pred(Info, PredOrFunc - PredCallId, !IO) :-
	PredCallId = PredName/Arity,
	typecheck_info_get_context(Info, Context),
	write_typecheck_info_context(Info, !IO),
	(
		PredName = unqualified("->"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: `->' without `;'.\n", !IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		( VerboseErrors = yes ->
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Note: the else part is not optional.\n",
				!IO),
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Every if-then must have an else.\n", !IO)
		;
			true
		)
	;
		PredName = unqualified("else"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: unmatched `else'.\n", !IO)
	;
		PredName = unqualified("if"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: `if' without `then' or `else'.\n",
			!IO)
	;
		PredName = unqualified("then"),
		( Arity = 2 ; Arity = 4 )
	->
		io__write_string("  error: `then' without `if' or `else'.\n",
			!IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		( VerboseErrors = yes ->
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Note: the `else' part is not optional.\n",
				!IO),
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Every if-then must have an `else'.\n", !IO)
		;
			true
		)
	;
		PredName = unqualified("apply"),
		Arity >= 1
	->
		report_error_apply_instead_of_pred(Info, !IO)
	;
		PredName = unqualified(PurityString),
		Arity = 1,
		( PurityString = "impure" ; PurityString = "semipure" )
	->
		io__write_string("  error: `", !IO),
		io__write_string(PurityString, !IO),
		io__write_string("' marker in an inappropriate place.\n", !IO),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors,
			!IO),
		( VerboseErrors = yes ->
			prog_out__write_context(Context, !IO),
			io__write_string("  Such markers only belong " ++
				"before predicate calls.\n", !IO)
		;
			true
		)
	;
		PredName = unqualified("some"),
		Arity = 2
	->
		io__write_string("  syntax error in existential " ++
			"quantification: first\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  argument of `some' should be " ++
			"a list of variables.\n", !IO)
	;
		io__write_string("  error: undefined ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc - PredCallId, !IO),
		( PredName = qualified(ModuleQualifier, _) ->
			maybe_report_missing_import(Info,
				ModuleQualifier, !IO)
		;
			io__write_string(".\n", !IO)
		)
	).

:- pred maybe_report_missing_import(typecheck_info::in, module_specifier::in,
	io::di, io::uo) is det.

maybe_report_missing_import(Info, ModuleQualifier, !IO) :-
	typecheck_info_get_context(Info, Context),
	%
	% first check if this module wasn't imported
	%
	typecheck_info_get_module_info(Info, ModuleInfo),
	(
		% if the module qualifier couldn't match any of the
		% visible modules, then we report that the module
		% has not been imported
		\+ (
			visible_module(VisibleModule, ModuleInfo),
			match_sym_name(ModuleQualifier, VisibleModule)
		)
	->
		io__write_string("\n", !IO),
		error_util__write_error_pieces(Context, 2,
			[words("(the module "),
			fixed(error_util__describe_sym_name(ModuleQualifier)),
			words("has not been imported).")], !IO)
	;
		% The module qualifier matches one or more of the
		% visible modules.  But maybe the user forgot to
		% import the parent module(s) of that module...
		solutions(get_unimported_parent(ModuleQualifier,
			ModuleInfo), UnimportedParents),
		UnimportedParents \= []
	->
		io__write_string("\n", !IO),
		report_unimported_parents(Context, UnimportedParents, !IO)
	;
		io__write_string(".\n", !IO)
	).

	% nondeterministically return all the possible parent
	% modules which could be parent modules of the given
	% module qualifier, and which are not imported.
:- pred get_unimported_parent(module_name::in, module_info::in,
	module_name::out) is nondet.

get_unimported_parent(ModuleQualifier, ModuleInfo, UnimportedParent) :-
	visible_module(ModuleName, ModuleInfo),
	match_sym_name(ModuleQualifier, ModuleName),
	ParentModules = get_ancestors(ModuleName),
	list__member(UnimportedParent, ParentModules),
	\+ visible_module(UnimportedParent, ModuleInfo).

:- pred report_unimported_parents(prog_context::in, list(module_name)::in,
	io::di, io::uo) is det.

report_unimported_parents(Context, UnimportedParents, !IO) :-
	UnimportedParentDescs = list__map(error_util__describe_sym_name,
		UnimportedParents),
	AllUnimportedParents = list_to_pieces(UnimportedParentDescs),
	error_util__write_error_pieces(Context, 2,
		( AllUnimportedParents = [_] ->
			[words("(the possible parent module ")]
			++ AllUnimportedParents
			++ [words("has not been imported).")]
		;
			[words("(the possible parent modules ")]
			++ AllUnimportedParents
			++ [words("have not been imported).")]
		), !IO).

:- pred report_error_func_instead_of_pred(typecheck_info::in, pred_or_func::in,
	simple_call_id::in, io::di, io::uo) is det.

report_error_func_instead_of_pred(Info, PredOrFunc, PredCallId) -->
	report_error_undef_pred(Info, PredCallId),
	{ typecheck_info_get_context(Info, Context) },
	prog_out__write_context(Context),
	io__write_string("  (There is a *"),
	prog_out__write_pred_or_func(PredOrFunc),
	io__write_string("* with that name, however."),
	( { PredOrFunc = function } ->
		io__nl,
		prog_out__write_context(Context),
		io__write_string("  Perhaps you forgot to add ` = ...'?)\n")
	;
		io__write_string(")\n")
	).

:- pred report_error_apply_instead_of_pred(typecheck_info::in, io::di, io::uo)
	is det.

report_error_apply_instead_of_pred(Info) -->
	io__write_string("  error: the language construct `apply' should\n"),
	{ typecheck_info_get_context(Info, Context) },
	prog_out__write_context(Context),
	io__write_string("  be used as an expression, not as a goal.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  (Perhaps you forgot to add ` = ...'?)\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  If you're trying to invoke a higher-order\n"),
		prog_out__write_context(Context),
		io__write_string("  predicate, use `call', not `apply'.\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  If you're trying to curry a higher-order\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  function, use a forwarding function:\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  e.g. instead of `NewFunc = apply(OldFunc, X)'\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  use `NewFunc = my_apply(OldFunc, X)'\n"),
		prog_out__write_context(Context),
		io__write_string(
		"  where `my_apply' is defined with the appropriate arity,\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  e.g. `my_apply(Func, X, Y) :- apply(Func, X, Y).'\n")
	;
		[]
	).

:- pred report_error_pred_num_args(typecheck_info::in, simple_call_id::in,
	list(int)::in, io::di, io::uo) is det.

report_error_pred_num_args(Info, PredOrFunc - SymName/Arity, Arities) -->
	write_context_and_pred_id(Info),
	{ typecheck_info_get_context(Info, Context) },
	prog_out__write_context(Context),
	io__write_string("  error: "),
	report_error_num_args(yes(PredOrFunc), Arity, Arities),
	io__nl,
	prog_out__write_context(Context),
	io__write_string("  in call to "),
	prog_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	prog_out__write_sym_name(SymName),
	io__write_string("'.\n").

:- pred report_error_undef_cons(typecheck_info::in, list(cons_error)::in,
	cons_id::in, int::in, io::di, io::uo) is det.

report_error_undef_cons(Info, ConsErrors, Functor, Arity) -->
	{ typecheck_info_get_pred_markers(Info, PredMarkers) },
	{ typecheck_info_get_called_predid(Info, CalledPredId) },
	{ typecheck_info_get_arg_num(Info, ArgNum) },
	{ typecheck_info_get_context(Info, Context) },
	{ typecheck_info_get_unify_context(Info, UnifyContext) },
	write_context_and_pred_id(Info),
	write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
		UnifyContext),
	prog_out__write_context(Context),
	%
	% check for some special cases, so that we can give
	% clearer error messages
	%
	(
		{ Functor = cons(unqualified(Name), _) },
		{ language_builtin(Name, Arity) }
	->
		io__write_string("  error: the language construct "),
		hlds_out__write_cons_id(Functor),
		io__write_string(" should be\n"),
		prog_out__write_context(Context),
		io__write_string("  used as a goal, not as an expression.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
		"  If you are trying to use a goal as a boolean function,\n"),
			prog_out__write_context(Context),
			io__write_string(
		"  you should write `if <goal> then yes else no' instead.\n"),
			( { Name = "call" } ->
				prog_out__write_context(Context),
				io__write_string(
		"  If you are trying to invoke a higher-order\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  function, you should use `apply', not `call'.\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  If you're trying to curry a higher-order predicate,\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  see the ""Creating higher-order terms"" section of the\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  Mercury Language Reference Manual.\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  If you really are trying to use `call' as an expression\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  and not as an application of the language builtin\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  call/N, make sure that you have the arity correct, and\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  that the functor `call' is actually defined (if it is\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  defined in a separate module, check that the module is\n"),
				prog_out__write_context(Context),
				io__write_string(
		"  correctly imported).\n")
			;
				[]
			)
		;
			[]
		)
	; { Functor = cons(unqualified("else"), 2) } ->
		io__write_string("  error: unmatched `else'.\n")
	; { Functor = cons(unqualified("if"), 2) } ->
		io__write_string("  error: `if' without `then' or `else'.\n")
	; { Functor = cons(unqualified("then"), 2) } ->
		io__write_string("  error: `then' without `if' or `else'.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
				"  Note: the `else' part is not optional.\n"),
			prog_out__write_context(Context),
			io__write_string(
				"  Every if-then must have an `else'.\n")
		;
			[]
		)
	; { Functor = cons(unqualified("->"), 2) } ->
		io__write_string("  error: `->' without `;'.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
				"  Note: the else part is not optional.\n"),
			prog_out__write_context(Context),
			io__write_string(
				"  Every if-then must have an else.\n")
		;
			[]
		)
	; { Functor = cons(unqualified("^"), 2) } ->
		io__write_string("  error: invalid use of field selection " ++
			"operator (`^').\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  This is probably some kind " ++
				"of syntax error.\n"),
			prog_out__write_context(Context),
			io__write_string("  The field name must be an " ++
				"atom, not a variable or other term.\n")
		;
			[]
		)
	; { Functor = cons(unqualified(":="), 2) } ->
		io__write_string("  error: invalid use of field update " ++
			"operator (`:=').\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  This is probably some kind " ++
				"of syntax error.\n")
		;
			[]
		)
	; { Functor = cons(unqualified(":-"), 2) } ->
		io__write_string("  syntax error in lambda expression " ++
			"(`:-').\n")
	; { Functor = cons(unqualified("-->"), 2) } ->
		io__write_string("  syntax error in DCG lambda expression " ++
			"(`-->').\n")
	; { Functor = cons(unqualified("."), 2) } ->
		io__write_string("  error: the list constructor is " ++
			"now `[|]/2', not `./2'.\n")
	; { Functor = cons(unqualified("!"), 1) } ->
		io__write_string("  error: invalid use of `!' " ++
			"state variable operator.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string("  You probably meant to use " ++
				"`!.' or `!:'.\n")
		;
			[]
		)
	;
		(
			{ Functor = cons(Constructor, Arity) },
			{ typecheck_info_get_ctors(Info, ConsTable) },
			{ solutions(
				(pred(N::out) is nondet :-
					map__member(ConsTable,
						cons(Constructor, N),
						_),
				N \= Arity
			), ActualArities) },
			{ ActualArities \= [] }
		->
			report_wrong_arity_constructor(Constructor, Arity,
				ActualArities, Context)
		;
			io__write_string("  error: undefined symbol `"),
			{ strip_builtin_qualifier_from_cons_id(Functor,
				StrippedFunctor) },
			hlds_out__write_cons_id(StrippedFunctor),
			io__write_string("'"),
			(
				{ Functor = cons(Constructor, _) },
				{ Constructor = qualified(ModQual, _) }
			->
				maybe_report_missing_import(Info, ModQual)
			;
				{ Functor = cons(unqualified("[|]"), 2) }
			->
				maybe_report_missing_import(Info,
					unqualified("list"))
			;
				io__write_string(".\n")
			)
		),
		( { ConsErrors \= [] } ->
			list__foldl(report_cons_error(Context), ConsErrors)
		;
			[]
		)
	).

:- pred report_cons_error(prog_context::in, cons_error::in, io::di, io::uo)
	is det.

report_cons_error(Context,
		foreign_type_constructor(TypeName - TypeArity, _)) -->
	{ ErrorPieces =
		[words("There are"),  fixed("`:- pragma foreign_type'"),
		words("declarations for type"),
		fixed(describe_sym_name_and_arity(TypeName / TypeArity) ++ ","),
		words("so it is treated as an abstract type in all"),
		words("predicates and functions which are not implemented"),
		words("for those foreign types.")] },
	error_util__write_error_pieces_not_first_line(Context,
		0, ErrorPieces).

report_cons_error(_, abstract_imported_type) --> [].
		% For `abstract_imported_type' errors, the "undefined symbol"
		% error written by `report_error_undef_cons' is sufficient so
		% we do not print an additional error message here.

report_cons_error(_,
		invalid_field_update(FieldName, FieldDefn, TVarSet, TVars)) -->
	{ FieldDefn = hlds_ctor_field_defn(Context, _, _, ConsId, _) },
	prog_out__write_context(Context),
	io__write_string("  Field `"),
	prog_out__write_sym_name(FieldName),
	io__write_string("' cannot be updated because\n"),
	prog_out__write_context(Context),
	io__write_string("  the existentially quantified type "),
	(
		{ TVars = [] },
		{ error("report_invalid_field_update: no type variables") }
	;
		{ TVars = [TVar] },
		io__write_string("variable `"),
		mercury_output_var(TVar, TVarSet, no),
		io__write_string("' occurs\n")
	;
		{ TVars = [_, _ | _] },
		io__write_string("variables `"),
		mercury_output_vars(TVars, TVarSet, no),
		io__write_string("' occur\n")
	),
	prog_out__write_context(Context),
	io__write_string("  in the types of field `"),
	prog_out__write_sym_name(FieldName),
	io__write_string("' and some other field\n"),
	prog_out__write_context(Context),
	io__write_string("  in definition of constructor `"),
	hlds_out__write_cons_id(ConsId),
	io__write_string(" '.\n").

:- pred report_wrong_arity_constructor(sym_name::in, arity::in, list(int)::in,
	prog_context::in, io::di, io::uo) is det.

report_wrong_arity_constructor(Name, Arity, ActualArities, Context) -->
	io__write_string("  error: "),
	{ MaybePredOrFunc = no },
	report_error_num_args(MaybePredOrFunc, Arity, ActualArities),
	io__nl,
	prog_out__write_context(Context),
	io__write_string("  in use of constructor `"),
	prog_out__write_sym_name(Name),
	io__write_string("'.\n").

% language_builtin(Name, Arity) is true iff Name/Arity
% is the name of a builtin language construct that should be
% used as a goal, not as an expression.

:- pred language_builtin(string::in, arity::in) is semidet.

language_builtin("=", 2).
language_builtin("\\=", 2).
language_builtin(",", 2).
language_builtin(";", 2).
language_builtin("\\+", 1).
language_builtin("not", 1).
language_builtin("<=>", 2).
language_builtin("=>", 2).
language_builtin("<=", 2).
language_builtin("call", _).
language_builtin("impure", 1).
language_builtin("semipure", 1).
language_builtin("all", 2).
language_builtin("some", 2).
language_builtin("aditi_insert", 3).
language_builtin("aditi_delete", 3).
language_builtin("aditi_bulk_insert", 3).
language_builtin("aditi_bulk_insert", 4).
language_builtin("aditi_bulk_delete", 3).
language_builtin("aditi_bulk_delete", 4).
language_builtin("aditi_bulk_modify", 3).
language_builtin("aditi_bulk_modify", 4).

:- pred write_call_context(prog_context::in, pred_markers::in,
	call_id::in, int::in, unify_context::in, io::di, io::uo) is det.

write_call_context(Context, PredMarkers, CallId, ArgNum, UnifyContext) -->
	( { ArgNum = 0 } ->
		hlds_out__write_unify_context(UnifyContext, Context)
	;
		prog_out__write_context(Context),
		io__write_string("  in "),
		hlds_out__write_call_arg_id(CallId, ArgNum, PredMarkers),
		io__write_string(":\n")
	).

%-----------------------------------------------------------------------------%

:- pred write_typecheck_info_context(typecheck_info::in, io::di, io::uo)
	is det.

write_typecheck_info_context(Info, !IO) :-
	write_context_and_pred_id(Info, !IO),
	typecheck_info_get_context(Info, Context),
	prog_out__write_context(Context, !IO).

:- pred write_context_and_pred_id(typecheck_info::in, io::di, io::uo) is det.

write_context_and_pred_id(Info, !IO) :-
	typecheck_info_get_module_info(Info, ModuleInfo),
	typecheck_info_get_context(Info, Context),
	typecheck_info_get_predid(Info, PredId),
	prog_out__write_context(Context, !IO),
	io__write_string("In clause for ", !IO),
	hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
	io__write_string(":\n", !IO).

	% This is intended to supercede the above predicate - It performs the
	% same action, but instead of just writing to the output straight away
	% the resultant string is passed back to the caller to deal with.
	% This allows `nicer' handling of error messages, since this string
	% can be used by the predicates in error_util.m
	%
	% The string generated by this predicate is of the form:
	%   "In clause for module:pred/N:"
:- pred make_pred_id_preamble(typecheck_info::in, string::out) is det.

make_pred_id_preamble(Info, Preamble) :-
	typecheck_info_get_module_info(Info, Module),
	typecheck_info_get_predid(Info, PredId),
	PredPieces = describe_one_pred_name(Module, should_not_module_qualify,
		PredId),
	PredName = error_pieces_to_string(PredPieces),
	Preamble = "In clause for " ++ PredName ++ ":".

%-----------------------------------------------------------------------------%

:- pred report_ambiguity_error(typecheck_info::in,
	type_assign::in, type_assign::in, io::di, io::uo) is det.

report_ambiguity_error(Info, TypeAssign1, TypeAssign2, !IO) :-
	write_typecheck_info_context(Info, !IO),
	io__write_string(
		"  error: ambiguous overloading causes type ambiguity.\n", !IO),
	typecheck_info_get_varset(Info, VarSet),
	type_assign_get_var_types(TypeAssign1, VarTypes1),
	map__keys(VarTypes1, Vars1),
	report_ambiguity_error_2(Vars1, VarSet, Info, TypeAssign1, TypeAssign2,
		no, Found, !IO),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	typecheck_info_get_context(Info, Context),
	( Found = no ->
		prog_out__write_context(Context, !IO),
		io__write_string("  One or more of the predicates or " ++
			"functions called\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  is declared in more than one module.\n",
			!IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  Try adding explicit module qualifiers.\n",
			!IO)
	; VerboseErrors = yes ->
		io__write_strings([
"\tYou will need to add an explicit type qualification to resolve the\n",
"\ttype ambiguity.\n",
"\tThe way to add an explicit type qualification is to use ""with_type"".\n",
"\tFor details see the ""Explicit type qualification"" sub-section\n",
"\tof the ""Data-terms"" section of the ""Syntax"" chapter\n",
"\tof the Mercury langauge reference manual.\n"
		], !IO)
	;
		true
	).

:- pred report_ambiguity_error_2(list(prog_var)::in, prog_varset::in,
	typecheck_info::in, type_assign::in, type_assign::in,
	bool::in, bool::out, io::di, io::uo) is det.

report_ambiguity_error_2([], _VarSet, _, _TypeAssign1, _TypeAssign2,
		!Found, !IO).
report_ambiguity_error_2([V | Vs], VarSet, Info, TypeAssign1,
		TypeAssign2, !Found, !IO) :-
	type_assign_get_var_types(TypeAssign1, VarTypes1),
	type_assign_get_var_types(TypeAssign2, VarTypes2),
	type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
	type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
	type_assign_get_head_type_params(TypeAssign1, HeadTypeParams1),
	type_assign_get_head_type_params(TypeAssign2, HeadTypeParams2),
	(
		map__search(VarTypes1, V, Type1),
		map__search(VarTypes2, V, Type2),
		term__apply_rec_substitution(Type1, TypeBindings1, T1),
		term__apply_rec_substitution(Type2, TypeBindings2, T2),
		\+ identical_types(T1, T2)
	->
		typecheck_info_get_context(Info, Context),
		( !.Found = no ->
			prog_out__write_context(Context, !IO),
			io__write_string(
				"  Possible type assignments include:\n", !IO)
		;
			true
		),
		!:Found = yes,
		prog_out__write_context(Context, !IO),
		mercury_output_var(V, VarSet, no, !IO),
		io__write_string(": ", !IO),
		type_assign_get_typevarset(TypeAssign1, TVarSet1),
		output_type(T1, TVarSet1, HeadTypeParams1, !IO),
		io__write_string(" or ", !IO),
		type_assign_get_typevarset(TypeAssign2, TVarSet2),
		output_type(T2, TVarSet2, HeadTypeParams2, !IO),
		io__write_string("\n", !IO)
	;
		true
	),
	report_ambiguity_error_2(Vs, VarSet, Info, TypeAssign1, TypeAssign2,
		!Found, !IO).

	% Check whether two types are identical ignoring their
	% prog_contexts, i.e. whether they can be unified without
	% binding any type parameters.

:- pred identical_types((type)::in, (type)::in) is semidet.

identical_types(Type1, Type2) :-
	map__init(TypeSubst0),
	type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
	TypeSubst = TypeSubst0.

	% Check whether two lists of types are identical up to renaming.

:- pred identical_up_to_renaming(list(type)::in, list(type)::in) is semidet.

identical_up_to_renaming(TypesList1, TypesList2) :-
	% They are identical up to renaming if they each subsume each other.
	type_list_subsumes(TypesList1, TypesList2, _),
	type_list_subsumes(TypesList2, TypesList1, _).

	% Make error messages more readable by removing "builtin:"
	% qualifiers.

:- pred strip_builtin_qualifiers_from_type((type)::in, (type)::out) is det.

strip_builtin_qualifiers_from_type(Type0, Type) :-
	( type_to_ctor_and_args(Type0, TypeCtor0, Args0) ->
		strip_builtin_qualifiers_from_type_list(Args0, Args),
		TypeCtor0 = SymName0 - Arity,
		(
			SymName0 = qualified(Module, Name),
			mercury_public_builtin_module(Module)
		->
			SymName = unqualified(Name)
		;
			SymName = SymName0
		),
		construct_type(SymName - Arity, Args, Type)
	;
		Type = Type0
	).

:- pred strip_builtin_qualifiers_from_type_list(list(type)::in,
	list(type)::out) is det.

strip_builtin_qualifiers_from_type_list(Types0, Types) :-
	list__map(strip_builtin_qualifiers_from_type, Types0, Types).

%-----------------------------------------------------------------------------%

:- pred assign(T::in, T::out) is det.

assign(X, X).

%-----------------------------------------------------------------------------%

	% XXX this should probably work its way into the library.
	% This is just like list__filter_map except that it only returns
	% the first match:
	% 	find_first_map(X, Y, Z) <=> list__filter_map(X, Y, [Z | _])
	%
:- pred find_first_map(pred(X, Y)::in(pred(in, out) is semidet),
	list(X)::in, Y::out) is semidet.

find_first_map(Pred, [X | Xs], Result) :-
	( call(Pred, X, Result0) ->
		Result = Result0
	;
		find_first_map(Pred, Xs, Result)
	).

	% find_first(X, Y, Z) <=> list__takewhile(not X, Y, _, [Z | _])

:- pred find_first(pred(X)::in(pred(in) is semidet), list(X)::in, X::out)
	is semidet.

find_first(Pred, [X | Xs], Result) :-
	( call(Pred, X) ->
		Result = X
	;
		find_first(Pred, Xs, Result)
	).

%-----------------------------------------------------------------------------%
:- end_module typecheck.
%-----------------------------------------------------------------------------%
