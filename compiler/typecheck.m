%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2001 The University of Melbourne.
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
% Note that DCGS are used for THREE different purposes in this file:
%
%	1.  For accumulating io__states as usual.
%
%	2.  For accumulating typecheck_infos, which contain:
%		- an io_state, which is modified if we need to
%		  write out an error message
%		- various semi-global info which doesn't change often,
%		  namely the pred_id and prog_context of the clause
%		  we are type-checking
%		- a type_assign_set which stores the set of possible
%		  type assignments and is modified as we traverse through
%		  the clause
%
%	3.  For accumulating type_assign_sets.  This is when we are
%	    type-checking a single atomic construct (unification or
%	    predicate), and we are iterating through all the
%	    possible existing type-assignments to accumulate a new
%	    type-assignment set.
%
% This can be a little confusing if you're not aware of it, so be
% careful to look at the pred declarations to see what each DCG predicate
% is actually accumulating.
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

:- module typecheck.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_data, prog_data.
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

:- pred typecheck(module_info, module_info, bool, bool, io__state, io__state).
:- mode typecheck(in, out, out, out, di, uo) is det.

	% Find a predicate which matches the given name and argument types.
	% Abort if there is no matching pred.
	% Abort if there are multiple matching preds.

:- pred typecheck__resolve_pred_overloading(module_info, list(type),
		tvarset, sym_name, sym_name, pred_id).
:- mode typecheck__resolve_pred_overloading(in, in, in, in, out, out) is det.

	% Find a predicate or function from the list of pred_ids
	% which matches the given name and argument types.
	% Fail if there is no matching pred.
	% Abort if there are multiple matching preds.

:- pred typecheck__find_matching_pred_id(list(pred_id), module_info,
		tvarset, list(type), pred_id, sym_name).
:- mode typecheck__find_matching_pred_id(in, in, in, in, out, out) is semidet.

	% Apply context reduction to the list of class constraints by applying
	% the instance rules or superclass rules, building up proofs for
	% redundant constraints
:- pred typecheck__reduce_context_by_rule_application(instance_table,
	superclass_table, list(class_constraint), tsubst, tvarset, tvarset, 
	map(class_constraint, constraint_proof), 
	map(class_constraint, constraint_proof),
	list(class_constraint), list(class_constraint)).
:- mode typecheck__reduce_context_by_rule_application(in, in, in, in,
	in, out, in, out, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, prog_util, type_util, modules, code_util.
:- import_module prog_io, prog_io_util, prog_out, hlds_out, error_util.
:- import_module mercury_to_mercury, mode_util, options, getopt, globals.
:- import_module passes_aux, clause_to_proc, special_pred, inst_match.

:- import_module int, set, string, require, multi_map.
:- import_module assoc_list, std_util, term, varset, term_io.

%-----------------------------------------------------------------------------%

typecheck(Module0, Module, FoundError, ExceededIterationLimit) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),

	maybe_write_string(Verbose, "% Type-checking clauses...\n"),
	check_pred_types(Module0, Module, FoundError, ExceededIterationLimit),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%

	% Type-check the code for all the predicates in a module.

:- pred check_pred_types(module_info, module_info, bool, bool,
		io__state, io__state).
:- mode check_pred_types(in, out, out, out, di, uo) is det.

check_pred_types(Module0, Module, FoundError, ExceededIterationLimit) -->
	{ module_info_predids(Module0, PredIds) },
	globals__io_lookup_int_option(type_inference_iteration_limit,
		MaxIterations),
	typecheck_to_fixpoint(MaxIterations, PredIds, Module0,
		Module, FoundError, ExceededIterationLimit),
	write_inference_messages(PredIds, Module).

	% Repeatedly typecheck the code for a group of predicates
	% until a fixpoint is reached, or until some errors are detected.

:- pred typecheck_to_fixpoint(int, list(pred_id), module_info, module_info, 
		bool, bool, io__state, io__state).
:- mode typecheck_to_fixpoint(in, in, in, out, out, out, di, uo) is det.

typecheck_to_fixpoint(NumIterations, PredIds, Module0, Module,
		FoundError, ExceededIterationLimit) -->
	typecheck_pred_types_2(PredIds, Module0, Module1,
		no, FoundError1, no, Changed),
	( { Changed = no ; FoundError1 = yes } ->
		{ Module = Module1 },
		{ FoundError = FoundError1 },
		{ ExceededIterationLimit = no }
	;
		globals__io_lookup_bool_option(debug_types, DebugTypes),
		( { DebugTypes = yes } ->
			write_inference_messages(PredIds, Module1)
		;
			[]
		),
		{ NumIterations1 = NumIterations - 1 },
		( { NumIterations1 > 0 } ->
			typecheck_to_fixpoint(NumIterations1, PredIds, Module1,
				Module, FoundError, ExceededIterationLimit)
		;
			typecheck_report_max_iterations_exceeded,
			{ Module = Module1 },
			{ FoundError = yes },
			{ ExceededIterationLimit = yes }
		)
	).

:- pred typecheck_report_max_iterations_exceeded(io__state, io__state).
:- mode typecheck_report_max_iterations_exceeded(di, uo) is det.

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
	io__write_string("`--type-inference-iteration-limit' option to increase the limit).\n").

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred typecheck_pred_types_2(list(pred_id), module_info, module_info,
	bool, bool, bool, bool, io__state, io__state).
:- mode typecheck_pred_types_2(in, in, out,
	in, out, in, out, di, uo) is det.

typecheck_pred_types_2([], ModuleInfo, ModuleInfo, 
		Error, Error, Changed, Changed) --> [].
typecheck_pred_types_2([PredId | PredIds], ModuleInfo0, ModuleInfo, 
		Error0, Error, Changed0, Changed) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	(
		{ pred_info_is_imported(PredInfo0) }
	->
		{ Error2 = Error0 },
		{ ModuleInfo2 = ModuleInfo0 },
		{ Changed2 = Changed0 }
	;
		typecheck_pred_type(PredId, PredInfo0, ModuleInfo0, 
			PredInfo1, Error1, Changed1),
		(
			{ Error1 = no },
			{ map__det_update(Preds0, PredId, PredInfo1, Preds) },
			{ module_info_set_preds(ModuleInfo0, Preds,
				ModuleInfo2) }
		;
			{ Error1 = yes },
		/********************
		This code is not needed at the moment,
		since currently we don't run mode analysis if
		there are any type errors.
		And this code also causes problems:
		if there are undefined modes,
		this code can end up calling error/1,
		since post_typecheck__finish_ill_typed_pred
		assumes that there are no undefined modes.
			%
			% if we get an error, we need to call
			% post_typecheck__finish_ill_typed_pred on the
			% pred, to ensure that its mode declaration gets
			% properly module qualified; then we call
			% `remove_predid', so that the predicate's definition
			% will be ignored by later passes (the declaration
			% will still be used to check any calls to it).
			%
			post_typecheck__finish_ill_typed_pred(ModuleInfo0,
				PredId, PredInfo1, PredInfo),
			{ map__det_update(Preds0, PredId, PredInfo, Preds) },
		*******************/
			{ map__det_update(Preds0, PredId, PredInfo1, Preds) },
			{ module_info_set_preds(ModuleInfo0, Preds,
				ModuleInfo1) },
			{ module_info_remove_predid(ModuleInfo1, PredId,
				ModuleInfo2) }
		),
		{ bool__or(Error0, Error1, Error2) },
		{ bool__or(Changed0, Changed1, Changed2) }
	),
	typecheck_pred_types_2(PredIds, ModuleInfo2, ModuleInfo, 
		Error2, Error, Changed2, Changed).

:- pred typecheck_pred_type(pred_id, pred_info, module_info,
		pred_info, bool, bool, io__state, io__state).
:- mode typecheck_pred_type(in, in, in, out, out, out, di, uo) is det.

typecheck_pred_type(PredId, PredInfo0, ModuleInfo, PredInfo, Error, Changed,
		IOState0, IOState) :-
	(
	    % Compiler-generated predicates are created already type-correct,
	    % there's no need to typecheck them.  Same for builtins.
	    % But, compiler-generated unify predicates are not guaranteed
	    % to be type-correct if they call a user-defined equality pred
	    % or if it is a special pred for an existentially typed data type.
	    ( code_util__compiler_generated(PredInfo0),
	      \+ special_pred_needs_typecheck(PredInfo0, ModuleInfo)
	    ; code_util__predinfo_is_builtin(PredInfo0)
	    )
	->
	    pred_info_clauses_info(PredInfo0, ClausesInfo0),
	    clauses_info_clauses(ClausesInfo0, Clauses0),
	    ( Clauses0 = [] ->
		pred_info_mark_as_external(PredInfo0, PredInfo)
	    ;
	        PredInfo = PredInfo0
	    ),
	    Error = no,
	    Changed = no,
	    IOState = IOState0
	;
	    maybe_add_field_access_function_clause(ModuleInfo,
		    PredInfo0, PredInfo1),
	    pred_info_arg_types(PredInfo1, _ArgTypeVarSet, ExistQVars0,
		    ArgTypes0),
	    pred_info_clauses_info(PredInfo1, ClausesInfo0),
	    clauses_info_clauses(ClausesInfo0, Clauses0),
	    clauses_info_headvars(ClausesInfo0, HeadVars),
	    clauses_info_varset(ClausesInfo0, VarSet),
	    clauses_info_explicit_vartypes(ClausesInfo0, ExplicitVarTypes0),
	    ( 
		Clauses0 = [] 
	    ->
			% There are no clauses for class methods.
			% The clauses are generated later on,
			% in polymorphism__expand_class_method_bodies
	        pred_info_get_markers(PredInfo1, Markers),
		( check_marker(Markers, class_method) ->
			IOState = IOState0,
				% For the moment, we just insert the types
				% of the head vars into the clauses_info
			map__from_corresponding_lists(HeadVars, ArgTypes0,
				VarTypes),
			clauses_info_set_vartypes(ClausesInfo0, VarTypes,
				ClausesInfo),
			pred_info_set_clauses_info(PredInfo1, ClausesInfo,
				PredInfo2),
				% We also need to set the head_type_params
				% field to indicate that all the existentially
				% quantified tvars in the head of this
				% pred are indeed bound by this predicate.
			term__vars_list(ArgTypes0,
				HeadVarsIncludingExistentials),
			pred_info_set_head_type_params(PredInfo2,
				HeadVarsIncludingExistentials, PredInfo),
			Error = no,
			Changed = no
		;
			report_error_no_clauses(PredId, PredInfo1, ModuleInfo,
			    IOState0, IOState),
			PredInfo = PredInfo1,
			Error = yes,
			Changed = no
		)
	    ;
	        pred_info_typevarset(PredInfo1, TypeVarSet0),
	        pred_info_import_status(PredInfo1, Status),
	        pred_info_get_markers(PredInfo1, Markers),
		( check_marker(Markers, infer_type) ->
			% For a predicate whose type is inferred,
			% the predicate is allowed to bind the type
			% variables in the head of the predicate's
			% type declaration.  Such predicates are given an
			% initial type declaration of 
			% `pred foo(T1, T2, ..., TN)' by make_hlds.m.
			Inferring = yes,
			write_pred_progress_message("% Inferring type of ",
				PredId, ModuleInfo, IOState0, IOState1),
			HeadTypeParams1 = [],
			PredConstraints = constraints([], [])
		;
			Inferring = no,
			write_pred_progress_message("% Type-checking ",
				PredId, ModuleInfo, IOState0, IOState1),
			term__vars_list(ArgTypes0, HeadTypeParams0),
			list__delete_elems(HeadTypeParams0, ExistQVars0,
				HeadTypeParams1),
			pred_info_get_class_context(PredInfo1,
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

		( pred_info_is_field_access_function(ModuleInfo, PredInfo1) ->
			IsFieldAccessFunction = yes
		;
			IsFieldAccessFunction = no
		),
		typecheck_info_init(IOState1, ModuleInfo, PredId,
				IsFieldAccessFunction, TypeVarSet0, VarSet,
				ExplicitVarTypes0, HeadTypeParams1,
				Constraints, Status, TypeCheckInfo1),
		typecheck_info_get_type_assign_set(TypeCheckInfo1,
				OrigTypeAssignSet),
		typecheck_clause_list(Clauses0, HeadVars, ArgTypes0, Clauses,
				TypeCheckInfo1, TypeCheckInfo2),
		% we need to perform a final pass of context reduction
		% at the end, before checking the typeclass constraints
		perform_context_reduction(OrigTypeAssignSet, TypeCheckInfo2,
				TypeCheckInfo3),
		typecheck_check_for_ambiguity(whole_pred, HeadVars,
				TypeCheckInfo3, TypeCheckInfo4),
		typecheck_info_get_final_info(TypeCheckInfo4, HeadTypeParams1, 
				ExistQVars0, ExplicitVarTypes0, TypeVarSet,
				HeadTypeParams2, InferredVarTypes0,
				InferredTypeConstraints0, ConstraintProofs,
				TVarRenaming, ExistTypeRenaming),
		map__optimize(InferredVarTypes0, InferredVarTypes),
		clauses_info_set_vartypes(ClausesInfo0, InferredVarTypes,
				ClausesInfo1),

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

		clauses_info_set_explicit_vartypes(ClausesInfo1,
			ExplicitVarTypes, ClausesInfo2),
		clauses_info_set_clauses(ClausesInfo2, Clauses, ClausesInfo),
		pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo2),
		pred_info_set_typevarset(PredInfo2, TypeVarSet, PredInfo3),
		pred_info_set_constraint_proofs(PredInfo3, ConstraintProofs,
			PredInfo4),

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
		pred_info_set_unproven_body_constraints(PredInfo4,
				UnprovenBodyConstraints, PredInfo5),

		( Inferring = yes ->
			%
			% We need to infer which of the head variable
			% types must be existentially quantified
			%
			infer_existential_types(ArgTypeVars, HeadTypeParams2,
				ExistQVars, HeadTypeParams),

			%
			% Now save the information we inferred in the pred_info
			%
			pred_info_set_head_type_params(PredInfo5,
				HeadTypeParams, PredInfo6),
			pred_info_set_arg_types(PredInfo6, TypeVarSet,
				ExistQVars, ArgTypes, PredInfo7),
			pred_info_get_class_context(PredInfo1,
				OldTypeConstraints),
			pred_info_set_class_context(PredInfo7,
				InferredTypeConstraints, PredInfo),
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
		; % Inferring = no
			pred_info_set_head_type_params(PredInfo5,
				HeadTypeParams2, PredInfo6),
			pred_info_get_maybe_instance_method_constraints(
				PredInfo6, MaybeInstanceMethodConstraints0),

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
				MaybeInstanceMethodConstraints1 = 
					MaybeInstanceMethodConstraints0
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
					ExistTypeRenaming,
					MaybeInstanceMethodConstraints0,
					MaybeInstanceMethodConstraints1)
			),

			% rename them all to match the new typevarset
			apply_var_renaming_to_var_list(ExistQVars1,
				TVarRenaming, ExistQVars),
			term__apply_variable_renaming_to_list(ArgTypes1,
				TVarRenaming, RenamedOldArgTypes),
			apply_variable_renaming_to_constraints(TVarRenaming,
				PredConstraints1, RenamedOldConstraints),
			rename_instance_method_constraints(TVarRenaming,
				MaybeInstanceMethodConstraints1,
				MaybeInstanceMethodConstraints),

			% save the results in the pred_info
			pred_info_set_arg_types(PredInfo6, TypeVarSet,
				ExistQVars, RenamedOldArgTypes, PredInfo7),
			pred_info_set_class_context(PredInfo7,
				RenamedOldConstraints, PredInfo8),
			pred_info_set_maybe_instance_method_constraints(
				PredInfo8, MaybeInstanceMethodConstraints,
				PredInfo),

			Changed = no
		),
		typecheck_info_get_found_error(TypeCheckInfo4, Error),
		typecheck_info_get_io_state(TypeCheckInfo4, IOState)
	    )
	).

:- pred rename_instance_method_constraints(map(tvar, tvar),
		maybe(instance_method_constraints),
		maybe(instance_method_constraints)).
:- mode rename_instance_method_constraints(in, in, out) is det.

rename_instance_method_constraints(_, no, no).
rename_instance_method_constraints(Renaming,
		yes(Constraints0), yes(Constraints)) :-
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
		ClassMethodClassContext).

	%
	% infer which of the head variable
	% types must be existentially quantified
	%
:- pred infer_existential_types(list(tvar), head_type_params,
		existq_tvars, head_type_params).
:- mode infer_existential_types(in, in, out, out) is det.

infer_existential_types(ArgTypeVars, HeadTypeParams0,
		ExistQVars, HeadTypeParams) :-
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
:- pred restrict_to_head_vars(class_constraints, list(tvar), class_constraints,
		list(class_constraint)).
:- mode restrict_to_head_vars(in, in, out, out) is det.

restrict_to_head_vars(constraints(UnivCs0, ExistCs0), ArgVarTypes,
		constraints(UnivCs, ExistCs), UnprovenCs) :-
	restrict_to_head_vars_2(UnivCs0, ArgVarTypes, UnivCs, UnprovenCs),
	restrict_to_head_vars_2(ExistCs0, ArgVarTypes, ExistCs, _).

:- pred restrict_to_head_vars_2(list(class_constraint), list(tvar),
		list(class_constraint), list(class_constraint)).
:- mode restrict_to_head_vars_2(in, in, out, out) is det.

restrict_to_head_vars_2(ClassConstraints, HeadTypeVars, HeadClassConstraints,
		OtherClassConstraints) :-
	list__filter(is_head_class_constraint(HeadTypeVars),
		ClassConstraints, HeadClassConstraints, OtherClassConstraints).

:- pred is_head_class_constraint(list(tvar), class_constraint).
:- mode is_head_class_constraint(in, in) is semidet.

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
		existq_tvars, list(type), class_constraints,
		existq_tvars, list(type), class_constraints).
:- mode argtypes_identical_up_to_renaming(in, in, in, in, in, in) is semidet.

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
%           existially quantified type.
%
% In case (b), we need to typecheck it to fill in the head_type_params
% field in the pred_info.
%

:- pred special_pred_needs_typecheck(pred_info::in, module_info::in)
	is semidet.

special_pred_needs_typecheck(PredInfo, ModuleInfo) :-
	%
	% check if the predicate is a compiler-generated special
	% predicate
	%
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	special_pred_name_arity(_, _, PredName, PredArity),
	%
	% find out which type it is a special predicate for,
	% and check whether that type is a type for which there is
	% a user-defined equality predicate, or which is existentially typed.
	%
	pred_info_arg_types(PredInfo, ArgTypes),
	special_pred_get_type(PredName, ArgTypes, Type),
	type_to_type_id(Type, TypeId, _TypeArgs),
	module_info_types(ModuleInfo, TypeTable),
	map__lookup(TypeTable, TypeId, TypeDefn),
	hlds_data__get_type_defn_body(TypeDefn, Body),
	special_pred_for_type_needs_typecheck(Body).

%-----------------------------------------------------------------------------%

	%
	% For a field access function for which the user has supplied
	% a declaration but no clauses, add a clause
	% 'foo :='(X, Y) = 'foo :='(X, Y).
	% As for the default clauses added for builtins, this is not a
	% recursive call -- post_typecheck.m will expand the body into
	% unifications.
	%
:- pred maybe_add_field_access_function_clause(module_info,
		pred_info, pred_info).
:- mode maybe_add_field_access_function_clause(in, in, out) is det.

maybe_add_field_access_function_clause(ModuleInfo, PredInfo0, PredInfo) :-
	pred_info_import_status(PredInfo0, ImportStatus),
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	clauses_info_clauses(ClausesInfo0, Clauses0),
	(
		pred_info_is_field_access_function(ModuleInfo, PredInfo0),
		Clauses0 = [],
		status_defined_in_this_module(ImportStatus, yes)
	->
		clauses_info_headvars(ClausesInfo0, HeadVars),
		pred_args_to_func_args(HeadVars, FuncArgs, FuncRetVal),
		pred_info_context(PredInfo0, Context),
		pred_info_module(PredInfo0, FuncModule),
		pred_info_name(PredInfo0, FuncName),
		pred_info_arity(PredInfo0, PredArity),
		adjust_func_arity(function, FuncArity, PredArity),
		FuncSymName = qualified(FuncModule, FuncName),
		create_atomic_unification(FuncRetVal,
			functor(cons(FuncSymName, FuncArity), FuncArgs),
			Context, explicit, [], Goal0),
		Goal0 = GoalExpr - GoalInfo0,
		set__list_to_set(HeadVars, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
		Goal = GoalExpr - GoalInfo,
		ProcIds = [], % the clause applies to all procedures.
		Clause = clause(ProcIds, Goal, mercury, Context),
		clauses_info_set_clauses(ClausesInfo0, [Clause], ClausesInfo),
		pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo)
	;
		PredInfo = PredInfo0
	).


%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.

:- pred typecheck_clause_list(list(clause), list(prog_var), list(type),
		list(clause), typecheck_info, typecheck_info).
:- mode typecheck_clause_list(in, in, in, out, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_clause_list([], _, _, []) --> [].
typecheck_clause_list([Clause0|Clauses0], HeadVars, ArgTypes,
			[Clause|Clauses]) -->
	typecheck_clause(Clause0, HeadVars, ArgTypes, Clause),
	typecheck_clause_list(Clauses0, HeadVars, ArgTypes, Clauses).

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

:- pred typecheck_clause(clause, list(prog_var), list(type), clause,
			typecheck_info, typecheck_info).
:- mode typecheck_clause(in, in, in, out, typecheck_info_di, typecheck_info_uo)
			is det.

typecheck_clause(Clause0, HeadVars, ArgTypes, Clause) -->
		% XXX abstract clause/3
	{ Clause0 = clause(Modes, Body0, Lang, Context) },
	typecheck_info_set_context(Context),
		% typecheck the clause - first the head unification, and
		% then the body
	typecheck_var_has_type_list(HeadVars, ArgTypes, 1),
	typecheck_goal(Body0, Body),
	checkpoint("end of clause"),
	{ Clause = clause(Modes, Body, Lang, Context) },
	typecheck_info_set_context(Context),
	typecheck_check_for_ambiguity(clause_only, HeadVars).

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

:- pred typecheck_check_for_ambiguity(stuff_to_check, list(prog_var),
				typecheck_info, typecheck_info).
:- mode typecheck_check_for_ambiguity(in, in,
				typecheck_info_di, typecheck_info_uo) is det.

typecheck_check_for_ambiguity(StuffToCheck, HeadVars,
		TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet),
	( TypeAssignSet = [_SingleTypeAssign] ->
		TypeCheckInfo = TypeCheckInfo0
	; TypeAssignSet = [TypeAssign1, TypeAssign2 | _] ->
		%
		% we only report an ambiguity error if
		% (a) we haven't encountered any other errors
		% and if StuffToCheck = clause_only(_),
		% also (b) the ambiguity occurs only in the body,
		% rather than in the head variables (and hence
		% can't be resolved by looking at later clauses).
		%
		typecheck_info_get_found_error(TypeCheckInfo0, FoundError),
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
			    type_assign_get_var_types(TypeAssign1, VarTypes1),
			    type_assign_get_var_types(TypeAssign2, VarTypes2),
			    type_assign_get_type_bindings(TypeAssign1,
					TypeBindings1),
			    type_assign_get_type_bindings(TypeAssign2,
					TypeBindings2),
			    map__apply_to_list(HeadVars, VarTypes1, HeadTypes1),
			    map__apply_to_list(HeadVars, VarTypes2, HeadTypes2),
			    term__apply_rec_substitution_to_list(
			    		HeadTypes1, TypeBindings1,
					FinalHeadTypes1),
			    term__apply_rec_substitution_to_list(
			    		HeadTypes2, TypeBindings2,
					FinalHeadTypes2),
			    identical_up_to_renaming(
					FinalHeadTypes1, FinalHeadTypes2)
			)
		->
			typecheck_info_set_found_error(TypeCheckInfo0, yes, 
				TypeCheckInfo1),
			typecheck_info_get_io_state(TypeCheckInfo1, IOState0),
			report_ambiguity_error(TypeCheckInfo1, TypeAssign1,
				TypeAssign2, IOState0, IOState),
			typecheck_info_set_io_state(TypeCheckInfo1, IOState, 
				TypeCheckInfo)
		;
			TypeCheckInfo = TypeCheckInfo0
		)
	;
		% there should always be a type assignment, because
		% if there is an error somewhere, instead of setting
		% the current type assignment set to the empty set,
		% the type-checker should continue with the previous
		% type assignment set (so that it can detect other
		% errors in the same clause).
		error("internal error in typechecker: no type-assignment"),
		TypeCheckInfo = TypeCheckInfo0
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal(hlds_goal, hlds_goal, typecheck_info, typecheck_info).
:- mode typecheck_goal(in, out, typecheck_info_di, typecheck_info_uo) is det.

	% Typecheck a goal.
	% Note that we save the context of the goal in the typeinfo for
	% use in error messages.  Also, if the context of the goal is empty,
	% we set the context of the goal from the surrounding
	% context saved in the type-info.  (That should probably be done
	% in make_hlds, but it was easier to do here.)

typecheck_goal(Goal0 - GoalInfo0, Goal - GoalInfo, TypeCheckInfo0,
			TypeCheckInfo) :-
	goal_info_get_context(GoalInfo0, Context),
	term__context_init(EmptyContext),
	( Context = EmptyContext ->
		typecheck_info_get_context(TypeCheckInfo0, EnclosingContext),
		goal_info_set_context(GoalInfo0, EnclosingContext, GoalInfo),
		TypeCheckInfo1 = TypeCheckInfo0
	;
		GoalInfo = GoalInfo0,
		typecheck_info_set_context(Context, TypeCheckInfo0,
			TypeCheckInfo1)
	),

		% type-check the goal
	typecheck_goal_2(Goal0, Goal, TypeCheckInfo1, TypeCheckInfo2),

	check_warn_too_much_overloading(TypeCheckInfo2, TypeCheckInfo).

:- pred typecheck_goal_2(hlds_goal_expr, hlds_goal_expr,
				typecheck_info, typecheck_info).
:- mode typecheck_goal_2(in, out, typecheck_info_di, typecheck_info_uo) is det.

typecheck_goal_2(conj(List0), conj(List)) -->
	checkpoint("conj"),
	typecheck_goal_list(List0, List).
typecheck_goal_2(par_conj(List0, SM), par_conj(List, SM)) -->
	checkpoint("par_conj"),
	typecheck_goal_list(List0, List).
typecheck_goal_2(disj(List0, SM), disj(List, SM)) -->
	checkpoint("disj"),
	typecheck_goal_list(List0, List).
typecheck_goal_2(if_then_else(Vs, A0, B0, C0, SM),
		if_then_else(Vs, A, B, C, SM)) -->
	checkpoint("if"),
	typecheck_goal(A0, A),
	checkpoint("then"),
	typecheck_goal(B0, B),
	checkpoint("else"),
	typecheck_goal(C0, C),
	ensure_vars_have_a_type(Vs).
typecheck_goal_2(not(A0), not(A)) -->
	checkpoint("not"),
	typecheck_goal(A0, A).
typecheck_goal_2(some(Vs, C, G0), some(Vs, C, G)) -->
	checkpoint("some"),
	typecheck_goal(G0, G),
	ensure_vars_have_a_type(Vs).
typecheck_goal_2(call(_, Mode, Args, Builtin, Context, Name),
		call(PredId, Mode, Args, Builtin, Context, Name)) -->
	checkpoint("call"),
	{ list__length(Args, Arity) },
	typecheck_info_set_called_predid(call(predicate - Name/Arity)),
	typecheck_call_pred(predicate - Name/Arity, Args, PredId).
typecheck_goal_2(generic_call(GenericCall0, Args, C, D),
		generic_call(GenericCall, Args, C, D)) -->
	{ hlds_goal__generic_call_id(GenericCall0, CallId) },
	typecheck_info_set_called_predid(CallId),
	(
		{ GenericCall0 = higher_order(PredVar, _, _) },
		{ GenericCall = GenericCall0 },
		checkpoint("higher-order call"),
		typecheck_higher_order_call(PredVar, Args)
	;
		{ GenericCall0 = class_method(_, _, _, _) },
		{ error("typecheck_goal_2: unexpected class method call") }
	;
		{ GenericCall0 = aditi_builtin(AditiBuiltin0, PredCallId) },
		checkpoint("aditi builtin"),
		typecheck_aditi_builtin(PredCallId, Args,
			AditiBuiltin0, AditiBuiltin),
		{ GenericCall = aditi_builtin(AditiBuiltin, PredCallId) }
	).
typecheck_goal_2(unify(A, B0, Mode, Info, UnifyContext),
		unify(A, B, Mode, Info, UnifyContext)) -->
	checkpoint("unify"),
	typecheck_info_set_arg_num(0),
	typecheck_info_set_unify_context(UnifyContext),
	typecheck_unification(A, B0, B).
typecheck_goal_2(switch(_, _, _, _), _) -->
	{ error("unexpected switch") }.
typecheck_goal_2(foreign_proc(A, PredId, C, Args, E, F, G), 
		foreign_proc(A, PredId, C, Args, E, F, G)) -->
	% pragma_foreign_codes are automatically generated, so they
	% will always be type-correct, but we need to do
	% the type analysis in order to correctly compute the
	% HeadTypeParams that result from existentially typed pragma_c_codes.
	% (We could probably do that more efficiently that the way
	% it is done below, though.)
	=(TypeCheckInfo0),
	{ typecheck_info_get_type_assign_set(TypeCheckInfo0,
		OrigTypeAssignSet) },
	typecheck_call_pred_id(PredId, Args),
	perform_context_reduction(OrigTypeAssignSet).
typecheck_goal_2(shorthand(ShorthandGoal0), shorthand(ShorthandGoal)) -->
	typecheck_goal_2_shorthand(ShorthandGoal0, ShorthandGoal).


:- pred typecheck_goal_2_shorthand(shorthand_goal_expr, shorthand_goal_expr,
				typecheck_info, typecheck_info).
:- mode typecheck_goal_2_shorthand(in, out, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_goal_2_shorthand(bi_implication(LHS0, RHS0), 
		bi_implication(LHS, RHS)) -->
	checkpoint("<=>"),
	typecheck_goal(LHS0, LHS),
	typecheck_goal(RHS0, RHS).

%-----------------------------------------------------------------------------%

:- pred typecheck_goal_list(list(hlds_goal), list(hlds_goal),
				typecheck_info, typecheck_info).
:- mode typecheck_goal_list(in, out, typecheck_info_di, typecheck_info_uo)
				is det.

typecheck_goal_list([], []) --> [].
typecheck_goal_list([Goal0 | Goals0], [Goal | Goals]) -->
	typecheck_goal(Goal0, Goal),
	typecheck_goal_list(Goals0, Goals).

%-----------------------------------------------------------------------------%

	% ensure_vars_have_a_type(Vars):
	%	Ensure that each variable in Vars has been assigned a type.

:- pred ensure_vars_have_a_type(list(prog_var), typecheck_info, typecheck_info).
:- mode ensure_vars_have_a_type(in, typecheck_info_di, typecheck_info_uo)
	is det.

ensure_vars_have_a_type(Vars) -->
	( { Vars = [] } ->
		[]
	;
		% invent some new type variables to use as the types of
		% these variables
		{ list__length(Vars, NumVars) },
		{ varset__init(TypeVarSet0) },
		{ varset__new_vars(TypeVarSet0, NumVars,
			TypeVars, TypeVarSet) },
		{ term__var_list_to_term_list(TypeVars, Types) },
		typecheck_var_has_polymorphic_type_list(Vars, TypeVarSet, [],
			Types, constraints([], []))
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(prog_var, list(prog_var),
		typecheck_info, typecheck_info).
:- mode typecheck_higher_order_call(in, in, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_higher_order_call(PredVar, Args) -->
	{ list__length(Args, Arity) },
	{ higher_order_pred_type(Arity, normal,
		TypeVarSet, PredVarType, ArgTypes) },
		% The class context is empty because higher-order predicates
		% are always monomorphic.  Similarly for ExistQVars.
	{ ClassContext = constraints([], []) },
	{ ExistQVars = [] },
	typecheck_var_has_polymorphic_type_list([PredVar|Args], TypeVarSet,
		ExistQVars, [PredVarType|ArgTypes], ClassContext).

:- pred higher_order_pred_type(int, lambda_eval_method,
		tvarset, type, list(type)).
:- mode higher_order_pred_type(in, in, out, out, out) is det.

	% higher_order_pred_type(N, EvalMethod,
	%	TypeVarSet, PredType, ArgTypes):
	% Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
	% PredType = `EvalMethod pred(T1, T2, ..., TN)', and
	% ArgTypes = [T1, T2, ..., TN].

higher_order_pred_type(Arity, EvalMethod, TypeVarSet, PredType, ArgTypes) :-
	varset__init(TypeVarSet0),
	varset__new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet),
	term__var_list_to_term_list(ArgTypeVars, ArgTypes),
	construct_higher_order_type(predicate, EvalMethod, ArgTypes, PredType).

:- pred higher_order_func_type(int, lambda_eval_method,
		tvarset, type, list(type), type).
:- mode higher_order_func_type(in, in, out, out, out, out) is det.

	% higher_order_func_type(N, EvalMethod, TypeVarSet,
	%	FuncType, ArgTypes, RetType):
	% Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
	% FuncType = `EvalMethod func(T1, T2, ..., TN) = T0',
	% ArgTypes = [T1, T2, ..., TN], and
	% RetType = T0.

higher_order_func_type(Arity, EvalMethod, TypeVarSet,
		FuncType, ArgTypes, RetType) :-
	varset__init(TypeVarSet0),
	varset__new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet1),
	varset__new_var(TypeVarSet1, RetTypeVar, TypeVarSet),
	term__var_list_to_term_list(ArgTypeVars, ArgTypes),
	RetType = term__variable(RetTypeVar),
	construct_higher_order_func_type(EvalMethod,
		ArgTypes, RetType, FuncType).

%-----------------------------------------------------------------------------%

:- pred typecheck_aditi_builtin(simple_call_id, list(prog_var),
		aditi_builtin, aditi_builtin,
		typecheck_info, typecheck_info).
:- mode typecheck_aditi_builtin(in, in, in, out, typecheck_info_di, 
		typecheck_info_uo) is det.

typecheck_aditi_builtin(CallId, Args, Builtin0, Builtin) -->
	% This must succeed because make_hlds.m does not add a clause
	% to the clauses_info if it contains Aditi updates with the
	% wrong number of arguments.
	{ get_state_args_det(Args, OtherArgs, State0, State) },

	typecheck_aditi_builtin_2(CallId, OtherArgs,
		Builtin0, Builtin),

	typecheck_aditi_state_args(Builtin0, CallId,
		State0, State).

	% Typecheck the arguments of an Aditi update other than
	% the `aditi__state' arguments.
:- pred typecheck_aditi_builtin_2(simple_call_id, list(prog_var),
		aditi_builtin, aditi_builtin, typecheck_info, typecheck_info).
:- mode typecheck_aditi_builtin_2(in, in, in, out,
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_aditi_builtin_2(_, _, aditi_call(_, _, _, _), _) -->
	% There are only added by magic.m.
	{ error("typecheck_aditi_builtin: unexpected aditi_call") }.
typecheck_aditi_builtin_2(CallId, Args,
		aditi_tuple_insert_delete(InsertDelete, _),
		aditi_tuple_insert_delete(InsertDelete, PredId)) -->
	% The tuple to insert or delete has the same argument types
	% as the relation being inserted into or deleted from.
	typecheck_call_pred(CallId, Args, PredId).
typecheck_aditi_builtin_2(CallId, Args,
		aditi_insert_delete_modify(InsertDelMod, _, Syntax),
		aditi_insert_delete_modify(InsertDelMod, PredId, Syntax)) -->
	{ aditi_insert_del_mod_eval_method(InsertDelMod, EvalMethod) },

	{ CallId = PredOrFunc - _ },
	{ InsertDeleteAdjustArgTypes = 
	    lambda([RelationArgTypes::in, UpdateArgTypes::out] is det, (
			construct_higher_order_type(PredOrFunc,
				EvalMethod, RelationArgTypes, ClosureType),
			UpdateArgTypes = [ClosureType]
	    )) },

	% `aditi_modify' takes a closure which takes two sets of arguments
	% corresponding to those of the base relation, one set for
	% the tuple to delete, and one for the tuple to insert.
	{ ModifyAdjustArgTypes = 
	    lambda([RelationArgTypes::in, AditiModifyTypes::out] is det, (
			list__append(RelationArgTypes, RelationArgTypes,
				ClosureArgTypes),
			construct_higher_order_pred_type(EvalMethod,
				ClosureArgTypes, ClosureType),
			AditiModifyTypes = [ClosureType]
	    )) },

	{
		InsertDelMod = bulk_insert,
		AdjustArgTypes = InsertDeleteAdjustArgTypes
	;
		InsertDelMod = delete(_),
		AdjustArgTypes = InsertDeleteAdjustArgTypes
	;
		InsertDelMod = modify(_),
		AdjustArgTypes = ModifyAdjustArgTypes
	},
	typecheck_aditi_builtin_closure(CallId, Args, AdjustArgTypes, PredId).

:- pred aditi_insert_del_mod_eval_method(aditi_insert_delete_modify,
		lambda_eval_method).
:- mode aditi_insert_del_mod_eval_method(in, out) is det.

aditi_insert_del_mod_eval_method(bulk_insert, (aditi_bottom_up)).
aditi_insert_del_mod_eval_method(delete(filter), (aditi_top_down)).
aditi_insert_del_mod_eval_method(delete(bulk), (aditi_bottom_up)).
aditi_insert_del_mod_eval_method(modify(filter), (aditi_top_down)).
aditi_insert_del_mod_eval_method(modify(bulk), (aditi_bottom_up)).

	% Check that there is only one argument (other than the `aditi__state'
	% arguments) passed to an `aditi_delete', `aditi_bulk_insert',
	% `aditi_bulk_delete' or `aditi_modify', then typecheck that argument.
:- pred typecheck_aditi_builtin_closure(simple_call_id,
		list(prog_var), adjust_arg_types, pred_id,
		typecheck_info, typecheck_info).
:- mode typecheck_aditi_builtin_closure(in,
		in, in(adjust_arg_types), out,
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_aditi_builtin_closure(CallId, OtherArgs, AdjustArgTypes, PredId) -->
	( { OtherArgs = [HOArg] } ->
		typecheck_call_pred_adjust_arg_types(CallId, [HOArg],
			AdjustArgTypes, PredId)
	;
		% An error should have been reported by make_hlds.m.
		{ error(
		"typecheck_aditi_builtin: incorrect arity for builtin") }
	).

	% Typecheck the DCG state arguments in the argument
	% list of an Aditi builtin.
:- pred typecheck_aditi_state_args(aditi_builtin, simple_call_id,
		prog_var, prog_var, typecheck_info, typecheck_info).
:- mode typecheck_aditi_state_args(in, in, in, in,
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_aditi_state_args(Builtin, CallId, AditiState0Var, AditiStateVar) -->
	{ construct_type(qualified(unqualified("aditi"), "state") - 0,
		[], StateType) },
	typecheck_var_has_type_list([AditiState0Var, AditiStateVar],
		[StateType, StateType],
		aditi_builtin_first_state_arg(Builtin, CallId)).

	% Return the index in the argument list of the first
	% `aditi__state' DCG argument.
:- func aditi_builtin_first_state_arg(aditi_builtin, simple_call_id) = int.

aditi_builtin_first_state_arg(aditi_call(_, _, _, _), _) = _ :-
	error("aditi_builtin_first_state_arg: unexpected_aditi_call").
aditi_builtin_first_state_arg(aditi_tuple_insert_delete(_, _),
		_ - _/Arity) = Arity + 1.
	% XXX removing the space between the 2 and the `.' will possibly
	% cause lexing to fail as io__putback_char will be called twice
	% in succession in lexer__get_int_dot.
aditi_builtin_first_state_arg(aditi_insert_delete_modify(_, _, _), _) = 2 .

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(simple_call_id, list(prog_var), pred_id,
				typecheck_info, typecheck_info).
:- mode typecheck_call_pred(in, in, out, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_call_pred(CallId, Args, PredId, TypeCheckInfo0, TypeCheckInfo) :-
	AdjustArgTypes = lambda([X::in, X::out] is det, true),
	typecheck_call_pred_adjust_arg_types(CallId, Args, AdjustArgTypes,
		PredId, TypeCheckInfo0, TypeCheckInfo).

	% A closure of this type performs a transformation on
	% the argument types of the called predicate. It is used to
	% convert the argument types of the base relation for an Aditi
	% update builtin to the type of the higher-order argument of
	% the update predicate. For an ordinary predicate call,
	% the types are not transformed.
:- type adjust_arg_types == pred(list(type), list(type)).
:- inst adjust_arg_types = (pred(in, out) is det).

	% Typecheck a predicate, performing the given transformation on the
	% argument types.
:- pred typecheck_call_pred_adjust_arg_types(simple_call_id, list(prog_var),
	adjust_arg_types, pred_id, typecheck_info, typecheck_info).
:- mode typecheck_call_pred_adjust_arg_types(in, in, in(adjust_arg_types), out,
	typecheck_info_di, typecheck_info_uo) is det.

typecheck_call_pred_adjust_arg_types(CallId, Args, AdjustArgTypes,
		PredId, TypeCheckInfo1, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo1, OrigTypeAssignSet),

		% look up the called predicate's arg types
	typecheck_info_get_module_info(TypeCheckInfo1, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	( 
		CallId = PorF - SymName/Arity,
		predicate_table_search_pf_sym_arity(PredicateTable,
			PorF, SymName, Arity, PredIdList)
	->
		% handle the case of a non-overloaded predicate specially
		% (so that we can optimize the case of a non-overloaded,
		% non-polymorphic predicate)
		( PredIdList = [PredId0] ->
			PredId = PredId0,
			typecheck_call_pred_id_adjust_arg_types(PredId, Args,
				AdjustArgTypes, TypeCheckInfo1, TypeCheckInfo2)
		;
			typecheck_call_overloaded_pred(PredIdList, Args,
				AdjustArgTypes, TypeCheckInfo1,
				TypeCheckInfo2),

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
			invalid_pred_id(PredId)
		),

			% Arguably, we could do context reduction at
			% a different point. See the paper:
			% "Type classes: an exploration of the design
			% space", S. Peyton-Jones, M. Jones 1997.
			% for a discussion of some of the issues.
		perform_context_reduction(OrigTypeAssignSet, TypeCheckInfo2,
			TypeCheckInfo)

	;
		invalid_pred_id(PredId),
		report_pred_call_error(CallId, TypeCheckInfo1, TypeCheckInfo)
	).

	% Typecheck a call to a specific predicate.
:- pred typecheck_call_pred_id(pred_id, list(prog_var), 
				typecheck_info, typecheck_info).
:- mode typecheck_call_pred_id(in, in, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_call_pred_id(PredId, Args, TypeCheckInfo0, TypeCheckInfo) :-
	AdjustArgTypes = lambda([X::in, X::out] is det, true),
	typecheck_call_pred_id_adjust_arg_types(PredId, Args, AdjustArgTypes,
		TypeCheckInfo0, TypeCheckInfo).

	% Typecheck a call to a specific predicate, performing the given
	% transformation on the argument types.
:- pred typecheck_call_pred_id_adjust_arg_types(pred_id, list(prog_var),
		adjust_arg_types, typecheck_info, typecheck_info).
:- mode typecheck_call_pred_id_adjust_arg_types(in, in, in(adjust_arg_types),
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_call_pred_id_adjust_arg_types(PredId, Args, AdjustArgTypes,
		TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_module_info(TypeCheckInfo0, ModuleInfo),
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
	% a non-polymorphic predicate)
	%
	( varset__is_empty(PredTypeVarSet) ->
		typecheck_var_has_type_list(Args,
			PredArgTypes, 1, TypeCheckInfo0,
			TypeCheckInfo),
		( 
			% sanity check
			PredClassContext \= constraints([], [])
		->
			error("non-polymorphic pred has class context")
		;
			true
		)
	;
		typecheck_var_has_polymorphic_type_list(Args,
			PredTypeVarSet, PredExistQVars, PredArgTypes,
			PredClassContext, TypeCheckInfo0, TypeCheckInfo)
	).

:- pred report_pred_call_error(simple_call_id, typecheck_info, typecheck_info).
:- mode report_pred_call_error(in, typecheck_info_di,
		typecheck_info_uo) is det.

report_pred_call_error(PredCallId, TypeCheckInfo1, TypeCheckInfo) :-
	PredCallId = PredOrFunc0 - SymName/_Arity,
	typecheck_info_get_module_info(TypeCheckInfo1, ModuleInfo), 
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	typecheck_info_get_io_state(TypeCheckInfo1, IOState0),
	(
		predicate_table_search_pf_sym(PredicateTable,
			PredOrFunc0, SymName, OtherIds),
		predicate_table_get_preds(PredicateTable, Preds),
		OtherIds \= []
	->
		typecheck_find_arities(Preds, OtherIds, Arities),
		report_error_pred_num_args(TypeCheckInfo1, PredCallId,
			Arities, IOState0, IOState)
	;
		( PredOrFunc0 = predicate, PredOrFunc = function
		; PredOrFunc0 = function, PredOrFunc = predicate
		),
		predicate_table_search_pf_sym(PredicateTable,
			PredOrFunc, SymName, OtherIds),
		OtherIds \= []
	->
		report_error_func_instead_of_pred(TypeCheckInfo1, PredOrFunc,
			PredCallId, IOState0, IOState)
	;
		report_error_undef_pred(TypeCheckInfo1, PredCallId,
			IOState0, IOState)
	),
	typecheck_info_set_io_state(TypeCheckInfo1, IOState, TypeCheckInfo2),
	typecheck_info_set_found_error(TypeCheckInfo2, yes, TypeCheckInfo).

:- pred typecheck_find_arities(pred_table, list(pred_id), list(int)).
:- mode typecheck_find_arities(in, in, out) is det.

typecheck_find_arities(_, [], []).
typecheck_find_arities(Preds, [PredId | PredIds], [Arity | Arities]) :-
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arity(PredInfo, Arity),
	typecheck_find_arities(Preds, PredIds, Arities).

:- pred typecheck_call_overloaded_pred(list(pred_id), list(prog_var),
		adjust_arg_types, typecheck_info, typecheck_info).
:- mode typecheck_call_overloaded_pred(in, in, in(adjust_arg_types),
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_call_overloaded_pred(PredIdList, Args, AdjustArgTypes,
				TypeCheckInfo0, TypeCheckInfo) :-
	%
	% let the new arg_type_assign_set be the cross-product
	% of the current type_assign_set and the set of possible
	% lists of argument types for the overloaded predicate,
	% suitable renamed apart
	%
	typecheck_info_get_module_info(TypeCheckInfo0, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	get_overloaded_pred_arg_types(PredIdList, Preds, AdjustArgTypes,
			TypeAssignSet0, [], ArgsTypeAssignSet),
	%
	% then unify the types of the call arguments with the
	% called predicates' arg types
	%
	typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet, 
		TypeCheckInfo0, TypeCheckInfo).

:- pred get_overloaded_pred_arg_types(list(pred_id), pred_table,
		adjust_arg_types, type_assign_set,
		args_type_assign_set, args_type_assign_set).
:- mode get_overloaded_pred_arg_types(in, in, in(adjust_arg_types),
		in, in, out) is det.

get_overloaded_pred_arg_types([], _Preds, _AdjustArgTypes, _TypeAssignSet0,
		ArgsTypeAssignSet, ArgsTypeAssignSet).
get_overloaded_pred_arg_types([PredId | PredIds], Preds, AdjustArgTypes,
		TypeAssignSet0, ArgsTypeAssignSet0, ArgsTypeAssignSet) :-
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
		PredArgTypes0),
	call(AdjustArgTypes, PredArgTypes0, PredArgTypes),
	pred_info_get_class_context(PredInfo, PredClassContext),
	rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
		PredArgTypes, PredClassContext,
		ArgsTypeAssignSet0, ArgsTypeAssignSet1),
	get_overloaded_pred_arg_types(PredIds, Preds, AdjustArgTypes,
		TypeAssignSet0, ArgsTypeAssignSet1, ArgsTypeAssignSet).

%-----------------------------------------------------------------------------%

	% Note: calls to preds declared in `.opt' files should always be 
	% module qualified, so they should not be considered
	% when resolving overloading.

typecheck__resolve_pred_overloading(ModuleInfo, ArgTypes, TVarSet,
		 PredName0, PredName, PredId) :-
	module_info_get_predicate_table(ModuleInfo, PredTable),
	(
		predicate_table_search_pred_sym(PredTable, PredName0,
			PredIds0)
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
		pred_info_name(PredInfo, PName),
		pred_info_module(PredInfo, Module),
		PredName = qualified(Module, PName),
		(
			typecheck__find_matching_pred_id(PredIds,
				ModuleInfo, TVarSet, ArgTypes, _OtherPredId, _)
		->
			% XXX this should report an error properly, not
			% via error/1
			error("Type error in predicate call: unresolvable predicate overloading.  You need to use an explicit module qualifier.  Compile with -V to find out where.")
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

:- pred typecheck_var_has_polymorphic_type_list(list(prog_var),
		tvarset, existq_tvars, list(type), class_constraints,
		typecheck_info, typecheck_info).
:- mode typecheck_var_has_polymorphic_type_list(in, in, in, in, in,
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet, PredExistQVars,
		PredArgTypes, PredClassConstraints,
		TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	rename_apart(TypeAssignSet0, PredTypeVarSet, PredExistQVars,
				PredArgTypes, PredClassConstraints,
				[], ArgsTypeAssignSet),
	typecheck_var_has_arg_type_list(Args, 1, ArgsTypeAssignSet,
				TypeCheckInfo0, TypeCheckInfo).

:- pred rename_apart(type_assign_set, tvarset, existq_tvars, list(type),
			class_constraints,
                        args_type_assign_set, args_type_assign_set).
:- mode rename_apart(in, in, in, in, in, in, out) is det.

rename_apart([], _, _, _, _, ArgTypeAssigns, ArgTypeAssigns).
rename_apart([TypeAssign0 | TypeAssigns0], PredTypeVarSet, PredExistQVars0,
		PredArgTypes0, PredClassConstraints0,
		ArgTypeAssigns0, ArgTypeAssigns) :-
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
	type_assign_set_head_type_params(TypeAssign1, HeadTypeParams,
			TypeAssign),
	%
	% save the results and recurse
	%
	NewArgTypeAssign = args(TypeAssign, PredArgTypes,
			PredClassConstraints),
        ArgTypeAssigns1 = [NewArgTypeAssign | ArgTypeAssigns0],
        rename_apart(TypeAssigns0, PredTypeVarSet, PredExistQVars0,
			PredArgTypes0, PredClassConstraints0,
			ArgTypeAssigns1, ArgTypeAssigns).

:- pred type_assign_rename_apart(type_assign, tvarset, list(type),
			type_assign, list(type), tsubst).
:- mode type_assign_rename_apart(in, in, in, out, out, out) is det.

type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
		TypeAssign, PredArgTypes, Subst) :-
	type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
	varset__merge_subst(TypeVarSet0, PredTypeVarSet, TypeVarSet,
		Subst),
	term__apply_substitution_to_list(PredArgTypes0, Subst,
		PredArgTypes),
	type_assign_set_typevarset(TypeAssign0, TypeVarSet, TypeAssign).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_arg_type_list(list(prog_var), int,
		args_type_assign_set, typecheck_info, typecheck_info).
:- mode typecheck_var_has_arg_type_list(in, in, in, 
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_arg_type_list([], _, ArgTypeAssignSet,
		TypeCheckInfo0, TypeCheckInfo) :-
	convert_args_type_assign_set(ArgTypeAssignSet, TypeAssignSet),
	typecheck_info_set_type_assign_set(TypeCheckInfo0, TypeAssignSet, 
		TypeCheckInfo).

typecheck_var_has_arg_type_list([Var|Vars], ArgNum, ArgTypeAssignSet0) -->
	typecheck_info_set_arg_num(ArgNum),
	typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet1),
	{ ArgNum1 is ArgNum + 1 },
	typecheck_var_has_arg_type_list(Vars, ArgNum1, ArgTypeAssignSet1).

:- pred convert_args_type_assign_set(args_type_assign_set, type_assign_set).
:- mode convert_args_type_assign_set(in, out) is det.

convert_args_type_assign_set([], []).
convert_args_type_assign_set(
			[ArgTypeAssign|ArgTypeAssigns],
			[TypeAssign | TypeAssigns]) :-
	ArgTypeAssign = args(_, Args, _),
	( Args = [] ->
		convert_args_type_assign(ArgTypeAssign, TypeAssign)
	;
		% this should never happen, since the arguments should
		% all have been processed at this point
		error("convert_args_type_assign_set")
	),
	convert_args_type_assign_set(ArgTypeAssigns, TypeAssigns).

:- pred conv_args_type_assign_set(args_type_assign_set, type_assign_set).
:- mode conv_args_type_assign_set(in, out) is det.

	% Same as conv_args_type_assign_set, but does not abort when the args
	% are empty
conv_args_type_assign_set([], []).
conv_args_type_assign_set([X|Xs], [Y|Ys]) :-
	convert_args_type_assign(X, Y),
	conv_args_type_assign_set(Xs, Ys).
                 
:- pred convert_args_type_assign(args_type_assign, type_assign).
:- mode convert_args_type_assign(in, out) is det.

convert_args_type_assign(args(TypeAssign0, _, Constraints0), TypeAssign) :-
	type_assign_get_typeclass_constraints(TypeAssign0, OldConstraints),
	type_assign_get_type_bindings(TypeAssign0, Bindings),
	apply_rec_subst_to_constraints(Bindings, Constraints0, Constraints),
	add_constraints(OldConstraints, Constraints, NewConstraints),
	type_assign_set_typeclass_constraints(TypeAssign0,
		NewConstraints, TypeAssign).

:- pred typecheck_var_has_arg_type(prog_var, 
				args_type_assign_set, args_type_assign_set,
				typecheck_info, typecheck_info).
:- mode typecheck_var_has_arg_type(in, in, out,
				typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_arg_type(VarId, ArgTypeAssignSet0, ArgTypeAssignSet,
				TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_var_has_arg_type_2(ArgTypeAssignSet0,
				VarId, [], ArgTypeAssignSet1),
	(
		ArgTypeAssignSet1 = [],
		ArgTypeAssignSet0 \= []
	->
		skip_arg(ArgTypeAssignSet0, ArgTypeAssignSet),
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_arg_var(TypeCheckInfo0, VarId,
				ArgTypeAssignSet0, IOState0, IOState),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState, 
			TypeCheckInfo1),
		typecheck_info_set_found_error(TypeCheckInfo1, yes,
			TypeCheckInfo)
	;
		ArgTypeAssignSet = ArgTypeAssignSet1,
		TypeCheckInfo = TypeCheckInfo0
	).

:- pred skip_arg(args_type_assign_set, args_type_assign_set).
:- mode skip_arg(in, out) is det.

skip_arg([], []).
skip_arg([args(TypeAssign, Args0, Constraints) | ArgTypeAssigns0],
		[args(TypeAssign, Args, Constraints)| ArgTypeAssigns]) :-
	( Args0 = [_ | Args1] ->
		Args = Args1
	;
		% this should never happen
		error("typecheck.m: skip_arg")
	),
	skip_arg(ArgTypeAssigns0, ArgTypeAssigns).

:- pred typecheck_var_has_arg_type_2(args_type_assign_set, prog_var,
				args_type_assign_set, args_type_assign_set).
:- mode typecheck_var_has_arg_type_2(in, in, in, out) is det.

typecheck_var_has_arg_type_2([], _) --> [].
typecheck_var_has_arg_type_2(
		[args(TypeAssign0, ArgTypes0, ClassContext) | TypeAssignSet0],
		VarId) -->
	arg_type_assign_var_has_type(TypeAssign0, ArgTypes0,
					VarId, ClassContext),
	typecheck_var_has_arg_type_2(TypeAssignSet0, VarId).

:- pred arg_type_assign_var_has_type(type_assign, list(type), prog_var,
				class_constraints,
				args_type_assign_set, args_type_assign_set).
:- mode arg_type_assign_var_has_type(in, in, in, in, in, out) is det.

arg_type_assign_var_has_type(TypeAssign0, ArgTypes0, VarId,
		ClassContext, ArgTypeAssignSet0, ArgTypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( ArgTypes0 = [Type | ArgTypes] ->
	    (
		map__search(VarTypes0, VarId, VarType)
	    ->
		(
		    type_assign_unify_type(TypeAssign0,
				VarType, Type, TypeAssign1)
		->
		    ArgTypeAssignSet = 
		    		[args(TypeAssign1, ArgTypes, ClassContext) |
				ArgTypeAssignSet0]
		;
		    ArgTypeAssignSet = ArgTypeAssignSet0
		)
	    ;
		map__det_insert(VarTypes0, VarId, Type, VarTypes),
		type_assign_set_var_types(TypeAssign0, VarTypes, TypeAssign),
		ArgTypeAssignSet = [args(TypeAssign, ArgTypes, ClassContext)
					| ArgTypeAssignSet0]
	    )
	;
	    error("arg_type_assign_var_has_type")
	).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_type_list(list(prog_var), list(type), int,
		typecheck_info, typecheck_info).
:- mode typecheck_var_has_type_list(in, in, in,
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_type_list([], [_|_], _) -->
	{ error("typecheck_var_has_type_list: length mismatch") }.
typecheck_var_has_type_list([_|_], [], _) -->
	{ error("typecheck_var_has_type_list: length mismatch") }.
typecheck_var_has_type_list([], [], _) --> [].
typecheck_var_has_type_list([Var|Vars], [Type|Types], ArgNum) -->
	typecheck_info_set_arg_num(ArgNum),
	typecheck_var_has_type(Var, Type),
	{ ArgNum1 is ArgNum + 1 },
	typecheck_var_has_type_list(Vars, Types, ArgNum1).

:- pred typecheck_var_has_type(prog_var, type, typecheck_info, typecheck_info).
:- mode typecheck_var_has_type(in, in, typecheck_info_di, typecheck_info_uo)
	is det.

typecheck_var_has_type(VarId, Type, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	typecheck_var_has_type_2(TypeAssignSet0, VarId, Type,
			[], TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_var(TypeCheckInfo0, VarId, 
				Type, TypeAssignSet0, IOState0, IOState),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState, 
				TypeCheckInfo1),
		typecheck_info_set_found_error(TypeCheckInfo1, yes, 
				TypeCheckInfo)
	;
		typecheck_info_set_type_assign_set(TypeCheckInfo0, 
				TypeAssignSet, TypeCheckInfo)
	).

	% Given a type assignment set and a variable id,
	% return the list of possible different types for the variable.

:- type type_stuff ---> type_stuff(type, tvarset, tsubst).

:- pred get_type_stuff(type_assign_set, prog_var, list(type_stuff)).
:- mode get_type_stuff(in, in, out) is det.
get_type_stuff([], _VarId, []).
get_type_stuff([TypeAssign | TypeAssigns], VarId, L) :-
	get_type_stuff(TypeAssigns, VarId, L0),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	(
		map__search(VarTypes, VarId, Type0)
	->
		Type = Type0
	;
		% this shouldn't happen - how can a variable which has
		% not yet been assigned a type variable fail to have
		% the correct type?
		term__context_init(Context),
		Type = term__functor(term__atom("<any>"), [], Context)
	),
	TypeStuff = type_stuff(Type, TVarSet, TypeBindings),
	(
		list__member(TypeStuff, L0)
	->
		L = L0
	;
		L = [TypeStuff | L0]
	).

	% Given an arg type assignment set and a variable id,
	% return the list of possible different types for the argument
	% and the variable.

:- type arg_type_stuff ---> arg_type_stuff(type, type, tvarset).

:- pred get_arg_type_stuff(args_type_assign_set, prog_var,
		list(arg_type_stuff)).
:- mode get_arg_type_stuff(in, in, out) is det.
get_arg_type_stuff([], _VarId, []).
get_arg_type_stuff([args(TypeAssign, ArgTypes, _) | ArgTypeAssigns], 
			VarId, L) :-
	get_arg_type_stuff(ArgTypeAssigns, VarId, L0),
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_typevarset(TypeAssign, TVarSet),
	type_assign_get_var_types(TypeAssign, VarTypes),
	(
		map__search(VarTypes, VarId, VarType0)
	->
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
	TypeStuff = arg_type_stuff(ArgType2, VarType2, TVarSet),
	(
		list__member(TypeStuff, L0)
	->
		L = L0
	;
		L = [TypeStuff | L0]
	).

:- type headtypes == list(tvar).

:- pred typecheck_var_has_type_2(type_assign_set, prog_var, type,
				type_assign_set, type_assign_set).
:- mode typecheck_var_has_type_2(in, in, in, in, out) is det.

typecheck_var_has_type_2([], _, _) --> [].
typecheck_var_has_type_2([TypeAssign0 | TypeAssignSet0], VarId,
		Type) -->
	type_assign_var_has_type(TypeAssign0, VarId, Type),
	typecheck_var_has_type_2(TypeAssignSet0, VarId, Type).

:- pred type_assign_var_has_type(type_assign, prog_var, type,
				type_assign_set, type_assign_set).
:- mode type_assign_var_has_type(in, in, in, in, out) is det.

type_assign_var_has_type(TypeAssign0, VarId, Type,
		TypeAssignSet0, TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( %%% if some [VarType]
		map__search(VarTypes0, VarId, VarType)
	->
		( %%% if some [TypeAssign1]
			type_assign_unify_type(TypeAssign0,
				VarType, Type, TypeAssign1)
		->
			TypeAssignSet = [TypeAssign1 | TypeAssignSet0]
		;
			TypeAssignSet = TypeAssignSet0
		)
	;
		map__det_insert(VarTypes0, VarId, Type, VarTypes),
		type_assign_set_var_types(TypeAssign0, VarTypes, TypeAssign),
		TypeAssignSet = [TypeAssign | TypeAssignSet0]
	).

%-----------------------------------------------------------------------------%

	% type_assign_var_has_type_list(Vars, Types, TypeAssign, TypeCheckInfo,
	%		TypeAssignSet0, TypeAssignSet):
	% 	Let TAs = { TA | TA is a an extension of TypeAssign
	%		    	 for which the types of the Vars unify with
	%		    	 their respective Types },
	% 	list__append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_var_has_type_list(list(prog_var), list(type),
		type_assign, typecheck_info, type_assign_set, type_assign_set).
:- mode type_assign_var_has_type_list(in, in, in, typecheck_info_ui, in, out)
	is det.

type_assign_var_has_type_list([], [_|_], _, _, _, _) :-
	error("type_assign_var_has_type_list: length mis-match").
type_assign_var_has_type_list([_|_], [], _, _, _, _) :-
	error("type_assign_var_has_type_list: length mis-match").
type_assign_var_has_type_list([], [], TypeAssign, _,
		TypeAssignSet, [TypeAssign|TypeAssignSet]).
type_assign_var_has_type_list([Arg | Args], [Type | Types], TypeAssign0,
		TypeCheckInfo, TypeAssignSet0, TypeAssignSet) :-
	type_assign_var_has_type(TypeAssign0, Arg, Type, [], TypeAssignSet1),
	type_assign_list_var_has_type_list(TypeAssignSet1,
		Args, Types, TypeCheckInfo, TypeAssignSet0, TypeAssignSet).

	% type_assign_list_var_has_type_list(TAs, Terms, Types, 
	%		TypeCheckInfo, TypeAssignSet0, TypeAssignSet):
	% 	Let TAs2 = { TA | TA is a an extension of a member of TAs
	%		    	  for which the types of the Terms unify with
	%		    	  their respective Types },
	% 	list__append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_list_var_has_type_list(type_assign_set, list(prog_var),
		list(type), typecheck_info, type_assign_set, type_assign_set).
:- mode type_assign_list_var_has_type_list(in, in, in, typecheck_info_ui, 
		in, out) is det.

type_assign_list_var_has_type_list([], _, _, _) --> [].
type_assign_list_var_has_type_list([TA | TAs], Args, Types, TypeCheckInfo) -->
	type_assign_var_has_type_list(Args, Types, TA, TypeCheckInfo),
	type_assign_list_var_has_type_list(TAs, Args, Types, TypeCheckInfo).

%-----------------------------------------------------------------------------%

	% Because we allow overloading, type-checking is NP-complete.
	% Rather than disallow it completely, we issue a warning
	% whenever "too much" overloading is used.  "Too much"
	% is arbitrarily defined as anthing which results in
	% more than 50 possible type assignments.

:- pred check_warn_too_much_overloading(typecheck_info, typecheck_info).
:- mode check_warn_too_much_overloading(typecheck_info_di, typecheck_info_uo) 
				is det.

check_warn_too_much_overloading(TypeCheckInfo0, TypeCheckInfo) :-
	(
		typecheck_info_get_warned_about_overloading(TypeCheckInfo0,
			AlreadyWarned),
		AlreadyWarned = no,
		typecheck_info_get_type_assign_set(TypeCheckInfo0,
			TypeAssignSet),
		list__length(TypeAssignSet, Count),
		Count > 50
	->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_warning_too_much_overloading(TypeCheckInfo0,
			IOState0, IOState1),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState1, 
			TypeCheckInfo1),
		typecheck_info_set_warned_about_overloading(TypeCheckInfo1,
			yes, TypeCheckInfo)
	;
		TypeCheckInfo = TypeCheckInfo0
	).

%-----------------------------------------------------------------------------%

	% used for debugging

:- pred checkpoint(string, typecheck_info, typecheck_info).
:- mode checkpoint(in, typecheck_info_di, typecheck_info_uo) is det.

checkpoint(Msg, T0, T) :-
	typecheck_info_get_module_info(T0, ModuleInfo),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, debug_types, DoCheckPoint),
	( DoCheckPoint = yes ->
		typecheck_info_get_io_state(T0, I0),
		checkpoint_2(Msg, T0, I0, I),
		typecheck_info_set_io_state(T0, I, T)
	;
		T = T0
	).

:- pred checkpoint_2(string, typecheck_info, io__state, io__state).
:- mode checkpoint_2(in, typecheck_info_no_io, di, uo) is det.

checkpoint_2(Msg, T0) -->
	io__write_string("At "),
	io__write_string(Msg),
	io__write_string(": "),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
	io__write_string("\n"),
	{ typecheck_info_get_type_assign_set(T0, TypeAssignSet) },
	(
		{ Statistics = yes },
		{ TypeAssignSet = [TypeAssign | _] }
	->
		{ type_assign_get_var_types(TypeAssign, VarTypes) },
		checkpoint_tree_stats("\t`var -> type' map", VarTypes),
		{ type_assign_get_type_bindings(TypeAssign, TypeBindings) },
		checkpoint_tree_stats("\t`type var -> type' map", TypeBindings)
	;
		[]
	),
	{ typecheck_info_get_varset(T0, VarSet) },
	write_type_assign_set(TypeAssignSet, VarSet).

:- pred checkpoint_tree_stats(string, map(_K, _V), io__state, io__state).
:- mode checkpoint_tree_stats(in, in, di, uo) is det.

checkpoint_tree_stats(Description, Tree) -->
        { map__count(Tree, Count) },
        io__write_string(Description),
        io__write_string(": count = "),
        io__write_int(Count),
        io__write_string("\n").

%-----------------------------------------------------------------------------%

	% Type check a unification.
	% Get the type assignment set from the type info and then just
	% iterate over all the possible type assignments.

:- pred typecheck_unification(prog_var, unify_rhs, unify_rhs, typecheck_info, 					typecheck_info).
:- mode typecheck_unification(in, in, out, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_unification(X, var(Y), var(Y)) -->
	typecheck_unify_var_var(X, Y).
typecheck_unification(X, functor(F, As), functor(F, As)) -->
	=(OrigTypeCheckInfo),
	{ typecheck_info_get_type_assign_set(OrigTypeCheckInfo,
		OrigTypeAssignSet) },
	typecheck_unify_var_functor(X, F, As),
	perform_context_reduction(OrigTypeAssignSet).
typecheck_unification(X, 
		lambda_goal(PredOrFunc, EvalMethod, FixModes, NonLocals, Vars,
			Modes, Det, Goal0),
		lambda_goal(PredOrFunc, EvalMethod, FixModes, NonLocals, Vars,
			Modes, Det, Goal)) -->
 	typecheck_lambda_var_has_type(PredOrFunc, EvalMethod, X, Vars),
	typecheck_goal(Goal0, Goal).

:- pred typecheck_unify_var_var(prog_var, prog_var,
		typecheck_info, typecheck_info).
:- mode typecheck_unify_var_var(in, in, typecheck_info_di, typecheck_info_uo)
				is det.

typecheck_unify_var_var(X, Y, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	typecheck_unify_var_var_2(TypeAssignSet0, X, Y, [], TypeAssignSet),
	( TypeAssignSet = [], TypeAssignSet0 \= [] ->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_unif_var_var(TypeCheckInfo0, X, Y, TypeAssignSet0,
				IOState0, IOState1),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState1,
				TypeCheckInfo1),
		typecheck_info_set_found_error(TypeCheckInfo1, yes,
				TypeCheckInfo)
	;
		typecheck_info_set_type_assign_set(TypeCheckInfo0,
				TypeAssignSet, TypeCheckInfo)
	).

:- pred typecheck_unify_var_functor(prog_var, cons_id, list(prog_var),
		typecheck_info, typecheck_info).
:- mode typecheck_unify_var_functor(in, in, in, typecheck_info_di,
				typecheck_info_uo) is det.

typecheck_unify_var_functor(Var, Functor, Args, TypeCheckInfo0, 
			TypeCheckInfo) :-
	%
	% get the list of possible constructors that match this functor/arity
	% if there aren't any, report an undefined constructor error
	%
	list__length(Args, Arity),
	typecheck_info_get_ctor_list(TypeCheckInfo0, Functor, Arity,
			ConsDefnList, InvalidConsDefnList),
	( ConsDefnList = [] ->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_undef_cons(TypeCheckInfo0, InvalidConsDefnList,
				Functor, Arity, IOState0, IOState1),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState1,
				TypeCheckInfo1),
		typecheck_info_set_found_error(TypeCheckInfo1, yes,
				TypeCheckInfo)
	;
		%
		% produce the ConsTypeAssignSet, which is essentially the
		% cross-product of the TypeAssignSet0 and the ConsDefnList
		%
		typecheck_info_get_type_assign_set(TypeCheckInfo0, 
			TypeAssignSet0),
		typecheck_unify_var_functor_get_ctors(TypeAssignSet0,
			TypeCheckInfo0, ConsDefnList, [], ConsTypeAssignSet),
		( ConsTypeAssignSet = [], TypeAssignSet0 \= [] ->
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
		typecheck_functor_type(ConsTypeAssignSet, Var,
			[], ArgsTypeAssignSet),
		( ArgsTypeAssignSet = [], ConsTypeAssignSet \= [] ->
			typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
			report_error_functor_type(TypeCheckInfo0,
				Var, ConsDefnList, Functor, Arity,
				TypeAssignSet0,
				IOState0, IOState1),
			typecheck_info_set_io_state(TypeCheckInfo0, IOState1, 
				TypeCheckInfo1),
			typecheck_info_set_found_error(TypeCheckInfo1, yes, 
				TypeCheckInfo2)
		;
			TypeCheckInfo2 = TypeCheckInfo0
		),

		%
		% check that the type of the arguments of the functor matches
		% their expected type for this functor
		%
		typecheck_functor_arg_types( ArgsTypeAssignSet, Args,
			TypeCheckInfo2, [], TypeAssignSet),
		( TypeAssignSet = [], ArgsTypeAssignSet \= [] ->
			typecheck_info_get_io_state(TypeCheckInfo2, IOState2),
			report_error_functor_arg_types(TypeCheckInfo2,
				Var, ConsDefnList, Functor, Args,
				ArgsTypeAssignSet, IOState2, IOState3),
			typecheck_info_set_io_state(TypeCheckInfo2, IOState3, 
				TypeCheckInfo3),
			typecheck_info_set_found_error(TypeCheckInfo3, yes, 
				TypeCheckInfo4)
		;
			TypeCheckInfo4 = TypeCheckInfo2
		),
		%
		% if we encountered an error, continue checking with the
		% original type assign set
		%
		( TypeAssignSet = [] ->
			typecheck_info_set_type_assign_set(TypeCheckInfo4, 
				TypeAssignSet0, TypeCheckInfo)
		;
			typecheck_info_set_type_assign_set(TypeCheckInfo4, 
				TypeAssignSet, TypeCheckInfo)
		)
	).

	% typecheck_unify_var_functor_get_ctors(TypeAssignSet, TypeCheckInfo,
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
			type_assign, 	% Type assignment, 
			list(type), 	% types of callee args,
			class_constraints % constraints from callee
		).

:- pred typecheck_unify_var_functor_get_ctors(type_assign_set,
				typecheck_info, list(cons_type_info),
				cons_type_assign_set, cons_type_assign_set).
:- mode typecheck_unify_var_functor_get_ctors(in, in, in, in, out) is det.

	% Iterate over the type assign sets

typecheck_unify_var_functor_get_ctors([], _, _) --> [].
typecheck_unify_var_functor_get_ctors([TypeAssign | TypeAssigns], TypeCheckInfo,
		ConsDefns) -->
	typecheck_unify_var_functor_get_ctors_2(ConsDefns, TypeCheckInfo,
		TypeAssign),
	typecheck_unify_var_functor_get_ctors(TypeAssigns, TypeCheckInfo,
		ConsDefns).

	% Iterate over all the different cons defns.

:- pred typecheck_unify_var_functor_get_ctors_2(list(cons_type_info), 
			typecheck_info, type_assign, cons_type_assign_set, 
			cons_type_assign_set).
:- mode typecheck_unify_var_functor_get_ctors_2(in, in, in, in, out) is det.

typecheck_unify_var_functor_get_ctors_2([], _, _) --> [].
typecheck_unify_var_functor_get_ctors_2([ConsDefn | ConsDefns], TypeCheckInfo,
					TypeAssign0) -->
	{ get_cons_stuff(ConsDefn, TypeAssign0, TypeCheckInfo,
			ConsType, ArgTypes, TypeAssign1) },
	list__append([TypeAssign1 - cons_type(ConsType, ArgTypes)]),
	typecheck_unify_var_functor_get_ctors_2(ConsDefns, TypeCheckInfo,
			TypeAssign0).

	% typecheck_functor_type(ConsTypeAssignSet, Var):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor type,
	% check that the type of `Var' matches this type.

:- pred typecheck_functor_type(cons_type_assign_set, prog_var,
				args_type_assign_set, args_type_assign_set).
:- mode typecheck_functor_type(in, in, in, out) is det.

typecheck_functor_type([], _) --> [].
typecheck_functor_type([TypeAssign - ConsType | ConsTypeAssigns], Var) -->
	{ ConsType = cons_type(Type, ArgTypes) },
	type_assign_check_functor_type(Type, ArgTypes, Var, TypeAssign),
	typecheck_functor_type(ConsTypeAssigns, Var).

	% typecheck_functor_arg_types(ConsTypeAssignSet, Var, Args, ...):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor argument types,
	% check that the types of `Args' matches these types.

:- pred typecheck_functor_arg_types(args_type_assign_set, list(prog_var),
		typecheck_info, type_assign_set, type_assign_set).
:- mode typecheck_functor_arg_types(in, in, typecheck_info_ui, in, out)
	is det.

typecheck_functor_arg_types([], _, _) --> [].
typecheck_functor_arg_types([args(TypeAssign, ArgTypes, _) | ConsTypeAssigns],
			Args, TypeCheckInfo) -->
	type_assign_var_has_type_list(Args, ArgTypes, TypeAssign,
		TypeCheckInfo),
	typecheck_functor_arg_types(ConsTypeAssigns, Args, TypeCheckInfo).

	% iterate over all the possible type assignments.

:- pred typecheck_unify_var_var_2(type_assign_set, prog_var, prog_var,
				type_assign_set, type_assign_set).
:- mode typecheck_unify_var_var_2(in, in, in, in, out) is det.

typecheck_unify_var_var_2([], _, _) --> [].
typecheck_unify_var_var_2([TypeAssign0 | TypeAssigns0], X, Y) -->
	type_assign_unify_var_var(X, Y, TypeAssign0),
	typecheck_unify_var_var_2(TypeAssigns0, X, Y).

%-----------------------------------------------------------------------------%

	% Type-check the unification of two variables,
	% and update the type assignment.
	% TypeAssign0 is the type assignment we are updating,
	% TypeAssignSet0 is an accumulator for the list of possible
	% type assignments so far, and TypeAssignSet is TypeAssignSet plus
	% any type assignment(s) resulting from TypeAssign0 and this
	% unification.

:- pred type_assign_unify_var_var(prog_var, prog_var, type_assign, 
				type_assign_set, type_assign_set).
:- mode type_assign_unify_var_var(in, in, in, in, out) is det.

type_assign_unify_var_var(X, Y, TypeAssign0, TypeAssignSet0, TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(
		map__search(VarTypes0, X, TypeX)
	->
		(
			map__search(VarTypes0, Y, TypeY)
		->
			% both X and Y already have types - just
			% unify their types
			( 
				type_assign_unify_type(TypeAssign0,
					TypeX, TypeY, TypeAssign3)
			->
				TypeAssignSet = [TypeAssign3 | TypeAssignSet0]
			;
				TypeAssignSet = TypeAssignSet0
			)
		;
			% Y is a fresh variable which hasn't been
			% assigned a type yet
			map__det_insert(VarTypes0, Y, TypeX, VarTypes),
			type_assign_set_var_types(TypeAssign0, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		)
	;
		(
			map__search(VarTypes0, Y, TypeY)
		->
			% X is a fresh variable which hasn't been
			% assigned a type yet
			map__det_insert(VarTypes0, X, TypeY, VarTypes),
			type_assign_set_var_types(TypeAssign0, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		;
			% both X and Y are fresh variables -
			% introduce a fresh type variable to represent
			% their type
			type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
			varset__new_var(TypeVarSet0, TypeVar, TypeVarSet),
			type_assign_set_typevarset(TypeAssign0, TypeVarSet,
				TypeAssign1),
			Type = term__variable(TypeVar),
			map__det_insert(VarTypes0, X, Type, VarTypes1),
			map__det_insert(VarTypes1, Y, Type, VarTypes),
			type_assign_set_var_types(TypeAssign1, VarTypes,
				TypeAssign),
			TypeAssignSet = [TypeAssign | TypeAssignSet0]
		)
	).

%-----------------------------------------------------------------------------%

:- pred type_assign_check_functor_type(type, list(type), 
		prog_var, type_assign,
		args_type_assign_set, args_type_assign_set).
:- mode type_assign_check_functor_type(in, in, in, in, in, out) is det.

type_assign_check_functor_type(ConsType, ArgTypes, Y, TypeAssign1,
		TypeAssignSet0, TypeAssignSet) :-

		% unify the type of Var with the type of the constructor
	type_assign_get_var_types(TypeAssign1, VarTypes0),
	( %%% if some [TypeY]
		map__search(VarTypes0, Y, TypeY)
	->
		( %%% if some [TypeAssign2]
			type_assign_unify_type(TypeAssign1,
					ConsType, TypeY, TypeAssign2)
		->
				% The constraints are empty here because
				% none are added by unification with a
				% functor
			Constraints = constraints([], []),
			TypeAssignSet = [args(TypeAssign2, ArgTypes,
					Constraints) | TypeAssignSet0]
		;
			TypeAssignSet = TypeAssignSet0
		)
	;
			% The constraints are empty here because
			% none are added by unification with a
			% functor
		map__det_insert(VarTypes0, Y, ConsType, VarTypes),
		type_assign_set_var_types(TypeAssign1, VarTypes, TypeAssign3),
		Constraints = constraints([], []),
		TypeAssignSet = [args(TypeAssign3, ArgTypes, Constraints) | 
				TypeAssignSet0]
	).

%-----------------------------------------------------------------------------%

	% Given an cons_type_info, construct a type for the
	% constructor and a list of types of the arguments,
	% suitable renamed apart from the current type_assign's
	% typevarset.

:- pred get_cons_stuff(cons_type_info, type_assign, typecheck_info,
			type, list(type), type_assign).
:- mode get_cons_stuff(in, in, in, out, out, out) is det.

get_cons_stuff(ConsDefn, TypeAssign0, _TypeCheckInfo, ConsType, ArgTypes,
			TypeAssign) :-

	ConsDefn = cons_type_info(ConsTypeVarSet, ConsExistQVars0,
			ConsType0, ArgTypes0, ClassConstraints0),

	% Rename apart the type vars in the type of the constructor
	% and the types of its arguments.
	% (Optimize the common case of a non-polymorphic type)

	( varset__is_empty(ConsTypeVarSet) ->
		ConsType = ConsType0,
		ArgTypes = ArgTypes0,
		TypeAssign = TypeAssign0
	;
		type_assign_rename_apart(TypeAssign0, ConsTypeVarSet,
			[ConsType0 | ArgTypes0],
			TypeAssign1, [ConsType1 | ArgTypes1], Subst)
	->
		apply_substitution_to_var_list(ConsExistQVars0, Subst,
			ConsExistQVars),
		apply_subst_to_constraints(Subst, ClassConstraints0,
			ConstraintsToAdd),
		type_assign_get_typeclass_constraints(TypeAssign1,
			OldConstraints),
		%
		% add the constraints for this functor
		% to the current constraint set
		% For functors which are data constructors,
		% the fact that we don't take the dual
		% corresponds to assuming that they
		% will be used as deconstructors rather than as
		% constructors.
		%
		add_constraints(OldConstraints, ConstraintsToAdd,
			ClassConstraints),
		type_assign_set_typeclass_constraints(TypeAssign1,
			ClassConstraints, TypeAssign2),
		type_assign_get_head_type_params(TypeAssign2,
			HeadTypeParams0),
		list__append(ConsExistQVars, HeadTypeParams0,
			HeadTypeParams),
		type_assign_set_head_type_params(TypeAssign2,
			HeadTypeParams, TypeAssign),

		ConsType = ConsType1,
		ArgTypes = ArgTypes1
	;
		error("get_cons_stuff: type_assign_rename_apart failed")
	).

	%
	% compute the dual of a set of constraints:
	% anything which we can assume in the caller
	% is something that we must prove in the callee,
	% and vice versa
	%
:- pred dual_constraints(class_constraints, class_constraints).
:- mode dual_constraints(in, out) is det.
dual_constraints(constraints(Univs, Exists), constraints(Exists, Univs)).

	% add_constraints(Cs0, CsToAdd, Cs) :-
	% add the specified constraints to the current constraint set
	%
:- pred add_constraints(class_constraints, class_constraints,
			class_constraints).
:- mode add_constraints(in, in, out) is det.

add_constraints(Cs0, CsToAdd, Cs) :-
	Cs0 = constraints(UnivCs0, ExistCs0),
	CsToAdd = constraints(UnivCsToAdd, ExistCsToAdd),
	list__append(UnivCsToAdd, UnivCs0, UnivCs),
	list__append(ExistCsToAdd, ExistCs0, ExistCs),
	Cs = constraints(UnivCs, ExistCs).

:- pred apply_substitution_to_var_list(list(var(T)), map(var(T), term(T)),
		list(var(T))).
:- mode apply_substitution_to_var_list(in, in, out) is det.

apply_substitution_to_var_list(Vars0, RenameSubst, Vars) :-
	term__var_list_to_term_list(Vars0, Terms0),
	term__apply_substitution_to_list(Terms0, RenameSubst, Terms),
	term__term_list_to_var_list(Terms, Vars).

:- pred apply_var_renaming_to_var_list(list(var(T)), map(var(T), var(T)),
		list(var(T))).
:- mode apply_var_renaming_to_var_list(in, in, out) is det.

apply_var_renaming_to_var_list(Vars0, RenameSubst, Vars) :-
	list__map(apply_var_renaming_to_var(RenameSubst), Vars0, Vars).

:- pred apply_var_renaming_to_var(map(var(T), var(T)), var(T), var(T)).
:- mode apply_var_renaming_to_var(in, in, out) is det.

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

:- pred typecheck_lambda_var_has_type(pred_or_func, lambda_eval_method,
		prog_var, list(prog_var), typecheck_info, typecheck_info).
:- mode typecheck_lambda_var_has_type(in, in, in, in, typecheck_info_di, 
		typecheck_info_uo) is det.

typecheck_lambda_var_has_type(PredOrFunc, EvalMethod, Var,
		ArgVars, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	typecheck_lambda_var_has_type_2(TypeAssignSet0,
		PredOrFunc, EvalMethod, Var, ArgVars, [], TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_lambda_var(TypeCheckInfo0, PredOrFunc, EvalMethod,
			Var, ArgVars, TypeAssignSet0, IOState0, IOState),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState,
			TypeCheckInfo1),
		typecheck_info_set_found_error(TypeCheckInfo1, yes,
			TypeCheckInfo)
	;
		typecheck_info_set_type_assign_set(TypeCheckInfo0,
			TypeAssignSet, TypeCheckInfo)
	).

:- pred typecheck_lambda_var_has_type_2(type_assign_set, 
		pred_or_func, lambda_eval_method, prog_var, list(prog_var),
		type_assign_set, type_assign_set).
:- mode typecheck_lambda_var_has_type_2(in, in, in, in, in, in, out) is det.

typecheck_lambda_var_has_type_2([], _, _, _, _) --> [].
typecheck_lambda_var_has_type_2([TypeAssign0 | TypeAssignSet0],
				PredOrFunc, EvalMethod, Var, ArgVars) -->
	{ type_assign_get_types_of_vars(ArgVars, TypeAssign0, ArgVarTypes,
					TypeAssign1) },
	{ construct_higher_order_type(PredOrFunc,
		EvalMethod, ArgVarTypes, LambdaType) },
	type_assign_var_has_type(TypeAssign1, Var, LambdaType),
	typecheck_lambda_var_has_type_2(TypeAssignSet0,
		PredOrFunc, EvalMethod, Var, ArgVars).

:- pred type_assign_get_types_of_vars(list(prog_var), type_assign, list(type),
					type_assign).
:- mode type_assign_get_types_of_vars(in, in, out, out) is det.

type_assign_get_types_of_vars([], TypeAssign, [], TypeAssign).
type_assign_get_types_of_vars([Var|Vars], TypeAssign0,
				[Type|Types], TypeAssign) :-
	% check whether the variable already has a type
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( map__search(VarTypes0, Var, VarType) ->
		% if so, use that type
		Type = VarType,
		TypeAssign2 = TypeAssign0
	;
		% otherwise, introduce a fresh type variable to
		% use as the type of that variable
		type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
		varset__new_var(TypeVarSet0, TypeVar, TypeVarSet),
		type_assign_set_typevarset(TypeAssign0, TypeVarSet,
						TypeAssign1),
		Type = term__variable(TypeVar),
		map__det_insert(VarTypes0, Var, Type, VarTypes1),
		type_assign_set_var_types(TypeAssign1, VarTypes1, TypeAssign2)
	),
	% recursively process the rest of the variables.
	type_assign_get_types_of_vars(Vars, TypeAssign2, Types, TypeAssign).

%-----------------------------------------------------------------------------%

	% Unify (with occurs check) two types in a type assignment 
	% and update the type bindings.

:- pred type_assign_unify_type(type_assign, type, type, type_assign).
:- mode type_assign_unify_type(in, in, in, out) is semidet.

type_assign_unify_type(TypeAssign0, X, Y, TypeAssign) :-
	type_assign_get_head_type_params(TypeAssign0, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign0, TypeBindings0),
	type_unify(X, Y, HeadTypeParams, TypeBindings0, TypeBindings),
	type_assign_set_type_bindings(TypeAssign0, TypeBindings, TypeAssign).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% builtin_atomic_type(Const, TypeName)
	%	If Const is a constant of a builtin atomic type,
	%	instantiates TypeName to the name of that type,
	%	otherwise fails.

:- pred builtin_atomic_type(cons_id, string).
:- mode builtin_atomic_type(in, out) is semidet.

builtin_atomic_type(int_const(_), "int").
builtin_atomic_type(float_const(_), "float").
builtin_atomic_type(string_const(_), "string").
builtin_atomic_type(cons(unqualified(String), 0), "character") :-
	string__char_to_string(_, String).

	% builtin_pred_type(TypeCheckInfo, Functor, Arity, PredConsInfoList) :
	%	If Functor/Arity is a constant of a pred type,
	%	instantiates the output parameters, otherwise fails.
	%
	%	Instantiates PredConsInfoList to the set of cons_type_info
	%	structures for each predicate with name `Functor' and arity
	%	greater than or equal to Arity.
	%
	%	For example, functor `map__search/1' has type `pred(K,V)'
	%	(hence PredTypeParams = [K,V]) and argument types [map(K,V)].


:- pred builtin_pred_type(typecheck_info, cons_id, int, list(cons_type_info)).
:- mode builtin_pred_type(typecheck_info_ui, in, in, out) is semidet.

builtin_pred_type(TypeCheckInfo, Functor, Arity, PredConsInfoList) :-
	Functor = cons(SymName, _),
	typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	( predicate_table_search_sym(PredicateTable, SymName, PredIdList) ->
		predicate_table_get_preds(PredicateTable, Preds),
		make_pred_cons_info_list(TypeCheckInfo, PredIdList, Preds,
			Arity, ModuleInfo, [], PredConsInfoList)
	;
		PredConsInfoList = []
	).

:- pred make_pred_cons_info_list(typecheck_info, list(pred_id), pred_table, int,
		module_info, list(cons_type_info), list(cons_type_info)).
:- mode make_pred_cons_info_list(typecheck_info_ui, in, in, in, in, in, 
		out) is det.

make_pred_cons_info_list(_, [], _PredTable, _Arity, _ModuleInfo, L, L).
make_pred_cons_info_list(TypeCheckInfo, [PredId|PredIds], PredTable, Arity,
		ModuleInfo, L0, L) :-
	make_pred_cons_info(TypeCheckInfo, PredId, PredTable, Arity,
				ModuleInfo, L0, L1),
	make_pred_cons_info_list(TypeCheckInfo, PredIds, PredTable, Arity,
				ModuleInfo, L1, L).

:- type cons_type_info 
	---> cons_type_info(
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

:- pred make_pred_cons_info(typecheck_info, pred_id, pred_table, int,
		module_info, list(cons_type_info), list(cons_type_info)).
:- mode make_pred_cons_info(typecheck_info_ui, in, in, in, in, in, out) is det.

make_pred_cons_info(_TypeCheckInfo, PredId, PredTable, FuncArity,
		_ModuleInfo, L0, L) :-
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_arity(PredInfo, PredArity),
	pred_info_get_is_pred_or_func(PredInfo, IsPredOrFunc),
	pred_info_get_class_context(PredInfo, ClassContext),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredExistQVars,
			CompleteArgTypes),
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
			construct_higher_order_pred_type(normal,
					PredTypeParams, PredType),
			ConsInfo = cons_type_info(PredTypeVarSet,
					PredExistQVars,
					PredType, ArgTypes, ClassContext),
			L1 = [ConsInfo | L0],

			% If the predicate has an Aditi marker,
			% we also add the `aditi pred(...)' type,
			% which is used for inputs to the Aditi bulk update
			% operations and also to Aditi aggregates.
			pred_info_get_markers(PredInfo, Markers),
			( check_marker(Markers, aditi) ->
				construct_higher_order_pred_type(
					(aditi_bottom_up), PredTypeParams,
					PredType2),
				ConsInfo2 = cons_type_info(PredTypeVarSet,
					PredExistQVars, PredType2,
					ArgTypes, ClassContext),
				L = [ConsInfo2 | L1]
			;
				L = L1
			)
		;
			error("make_pred_cons_info: split_list failed")
		)
	;
		IsPredOrFunc = function,
		PredAsFuncArity is PredArity - 1,
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
				construct_higher_order_func_type(normal,
					FuncArgTypeParams, FuncReturnTypeParam,
					FuncType)	
			),
			ConsInfo = cons_type_info(PredTypeVarSet,
					PredExistQVars,
					FuncType, FuncArgTypes, ClassContext),
			L = [ConsInfo | L0]
		;
			error("make_pred_cons_info: split_list failed")
		)
	;
		L = L0
	).

	% builtin_apply_type(TypeCheckInfo, Functor, Arity, ConsTypeInfos):
	% Succeed if Functor is the builtin apply/N or ''/N (N>=2),
	% which is used to invoke higher-order functions.
	% If so, bind ConsTypeInfos to a singleton list containing
	% the appropriate type for apply/N of the specified Arity.

:- pred builtin_apply_type(typecheck_info, cons_id, int, list(cons_type_info)).
:- mode builtin_apply_type(typecheck_info_ui, in, in, out) is semidet.

builtin_apply_type(_TypeCheckInfo, Functor, Arity, ConsTypeInfos) :-
	Functor = cons(unqualified(ApplyName), _),
	( ApplyName = "apply" ; ApplyName = "" ),
	Arity >= 1,
	Arity1 is Arity - 1,
	higher_order_func_type(Arity1, normal, TypeVarSet,
		FuncType, ArgTypes, RetType),
	ExistQVars = [],
	Constraints = constraints([], []),
	ConsTypeInfos = [cons_type_info(TypeVarSet, ExistQVars, RetType,
					[FuncType | ArgTypes], Constraints)].

	% builtin_field_access_function_type(TypeCheckInfo, Functor,
	%	Arity, ConsTypeInfos):
	% Succeed if Functor is the name of one the automatically
	% generated field access functions (fieldname, '<fieldname> :=').
:- pred builtin_field_access_function_type(typecheck_info, cons_id, arity,
		list(cons_type_info), list(invalid_field_update)).
:- mode builtin_field_access_function_type(typecheck_info_ui, in, in,
		out, out) is semidet.

builtin_field_access_function_type(TypeCheckInfo, Functor, Arity,
		ConsTypeInfos, InvalidFieldUpdates) :-
	%
	% Taking the address of automatically generated field access
	% functions is not allowed, so currying does have to be
	% considered here.
	%
	Functor = cons(Name, Arity),
	typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo),
	is_field_access_function_name(ModuleInfo, Name, Arity,
		AccessType, FieldName),

	module_info_ctor_field_table(ModuleInfo, CtorFieldTable),
	map__search(CtorFieldTable, FieldName, FieldDefns),
	
	list__filter_map(
		make_field_access_function_cons_type_info(TypeCheckInfo, Name,
			Arity, AccessType, FieldName),
		FieldDefns, MaybeConsTypeInfos),

	list__filter_map(
		(pred(MaybeConsTypeInfo::in, ConsTypeInfo::out) is semidet :-
			MaybeConsTypeInfo = cons_type_info(ConsTypeInfo)
		), MaybeConsTypeInfos, ConsTypeInfos),	

	list__filter_map(
		(pred(MaybeConsTypeInfo::in, InvalidCons::out) is semidet :-
			MaybeConsTypeInfo = invalid_field_update(InvalidCons)
		), MaybeConsTypeInfos, InvalidFieldUpdates).

:- pred make_field_access_function_cons_type_info(typecheck_info,
		sym_name, arity, field_access_type,
		ctor_field_name, hlds_ctor_field_defn, maybe_cons_type_info).
:- mode make_field_access_function_cons_type_info(in,
		in, in, in, in, in, out) is semidet.

make_field_access_function_cons_type_info(TypeCheckInfo, FuncName, Arity,
		AccessType, FieldName, FieldDefn, ConsTypeInfo) :-
	get_field_access_constructor(TypeCheckInfo, FuncName, Arity,
		AccessType, FieldDefn, FunctorConsTypeInfo),
	convert_field_access_cons_type_info(AccessType, FieldName, FieldDefn,
		FunctorConsTypeInfo, ConsTypeInfo).

:- pred get_field_access_constructor(typecheck_info, sym_name, arity,
		field_access_type, hlds_ctor_field_defn, cons_type_info).
:- mode get_field_access_constructor(typecheck_info_ui,
		in, in, in, in, out) is semidet.

get_field_access_constructor(TypeCheckInfo, FuncName, Arity, _AccessType,
		FieldDefn, FunctorConsTypeInfo) :-

	FieldDefn = hlds_ctor_field_defn(_, _, TypeId, ConsId, _),
	TypeId = qualified(TypeModule, _) - _,

	%
	% If the user has supplied a declaration, we use that instead
	% of the automatically generated version, unless we are typechecking
	% the clause introduced for the user-supplied declaration.
	% The user-declared version will be picked up by builtin_pred_type.
	%
	typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredTable),
	unqualify_name(FuncName, UnqualFuncName),
	(
		TypeCheckInfo ^ is_field_access_function = no,
		\+ predicate_table_search_func_m_n_a(PredTable, TypeModule,
			UnqualFuncName, Arity, _)
	;
		TypeCheckInfo ^ is_field_access_function = yes
	),

	module_info_ctors(ModuleInfo, Ctors),
	map__lookup(Ctors, ConsId, ConsDefns0),
	list__filter(
		(pred(CtorDefn::in) is semidet :-
			CtorDefn = hlds_cons_defn(_, _, _, TypeId, _)
		), ConsDefns0, ConsDefns),
	ConsDefns = [ConsDefn],
	convert_cons_defn(TypeCheckInfo, ConsDefn, FunctorConsTypeInfo).

:- type maybe_cons_type_info
	--->	cons_type_info(cons_type_info)
	;	invalid_field_update(invalid_field_update)
	.

:- type invalid_field_update
	--->	invalid_field_update(ctor_field_name, hlds_ctor_field_defn,
			tvarset, list(tvar)).

:- pred convert_field_access_cons_type_info(field_access_type,
		ctor_field_name, hlds_ctor_field_defn,
		cons_type_info, maybe_cons_type_info) is det.
:- mode convert_field_access_cons_type_info(in, in, in, in, out) is det.

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
	ConsTypeInfo = cons_type_info(cons_type_info(TVarSet, ExistQVars,
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
		ConsTypeInfo = cons_type_info(
			cons_type_info(TVarSet, ExistQVars,
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
		list__replace_nth_det(ConsArgTypes, FieldNumber, int_type, 
			ArgTypesWithoutField),
		term__vars_list(ArgTypesWithoutField,
			TVarsInOtherArgs),
		set__intersect(
			set__list_to_set(TVarsInField),
			set__intersect(
				set__list_to_set(TVarsInOtherArgs),
				set__list_to_set(ExistQVars0)
			),
			ExistQVarsInFieldAndOthers),
		(
			set__empty(ExistQVarsInFieldAndOthers)
		->
			%
			% Rename apart type variables occurring only in the
			% field to be replaced - the values of those
			% type variables will be supplied by the
			% replacement field value.
			%
			list__delete_elems(TVarsInField, TVarsInOtherArgs,
				TVarsOnlyInField0),
			list__sort_and_remove_dups(TVarsOnlyInField0,
				TVarsOnlyInField),
			list__length(TVarsOnlyInField, NumNewTVars),
			varset__new_vars(TVarSet0, NumNewTVars,
				NewTVars, TVarSet),
			map__from_corresponding_lists(TVarsOnlyInField,
				NewTVars, TVarRenaming),
			term__apply_variable_renaming(FieldType, TVarRenaming,
				RenamedFieldType), 
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
			ConsTypeInfo = cons_type_info(
				cons_type_info(TVarSet, ExistQVars,
					RetType, ArgTypes, ClassConstraints))
		;
			%
			% This field cannot be set. Pass out some information
			% so that we can give a better error message.
			% Errors involving changing the types of universally
			% quantified type variables will be caught by 
			% typecheck_functor_arg_types.
			%
			set__to_sorted_list(ExistQVarsInFieldAndOthers,
				ExistQVarsInFieldAndOthers1),
			ConsTypeInfo =
				invalid_field_update(
				invalid_field_update(FieldName, FieldDefn,
					TVarSet0, ExistQVarsInFieldAndOthers1))
		)
	)
    ).

	% Rename constraints containing variables that have been renamed.
	% These constraints are all universal constraints - the values
	% of the type variables are supplied by the caller.
:- pred project_rename_flip_class_constraints(set(tvar), map(tvar, tvar),
		class_constraints, class_constraints).
:- mode project_rename_flip_class_constraints(in, in, in, out) is det.

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
	ProjectConstraints =
		(pred(ConstraintToCheck::in) is semidet :-
			ConstraintToCheck = constraint(_, TypesToCheck),
			term__vars_list(TypesToCheck, TVarsToCheck0),
			set__list_to_set(TVarsToCheck0, TVarsToCheck),
			set__intersect(TVarsToCheck, CallTVars, RelevantTVars),
			\+ set__empty(RelevantTVars)
		),
	list__filter(ProjectConstraints, ExistConstraints0, ExistConstraints1),
	
	RenameConstraints = 
		(pred(Constraint0::in, Constraint::out) is semidet :-
			Constraint0 = constraint(ClassName, ConstraintTypes0),
			some [Var] (
				term__contains_var_list(ConstraintTypes0, Var),
				map__contains(TVarRenaming, Var)
			),
			term__apply_variable_renaming_to_list(ConstraintTypes0,
				TVarRenaming, ConstraintTypes),
			Constraint = constraint(ClassName, ConstraintTypes)
		), 
	list__filter_map(RenameConstraints, ExistConstraints1, NewConstraints),

	% The variables which were previously existentially quantified
	% are now universally quantified.
	Constraints = constraints(NewConstraints, []).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The typecheck_info data structure and access predicates.

:- type typecheck_info
	---> typecheck_info(
			io_state :: io__state, 
					% The io state

			module_info :: module_info,
					% The global symbol tables

			call_id :: call_id,
					% The call_id of the pred
					% being called (if any)

			arg_num :: int,	
					% The argument number within
					% a pred call 

			pred_id :: pred_id,
					% The pred we're checking

			import_status :: import_status,
					% Import status of the pred
					% being checked

			is_field_access_function :: bool,
					% Is the pred we're checking
					% a field access function?
					% If so, there should only
					% be a field access function
					% application in the body, not
					% predicate or function calls
					% or constructor applications.

			context :: prog_context,
					% The context of the goal
					% we're checking

			unify_context :: unify_context,
					% The original source of the
					% unification we're checking

			varset :: prog_varset,
					% Variable names

			type_assign_set :: type_assign_set,
					% This is the main piece of
					% information that we are
					% computing and which gets
					% updated as we go along

			found_error :: bool,	
					% did we find any type errors?

			warned_about_overloading :: bool
					% Have we already warned about
					% highly ambiguous overloading?
		).

	% The normal inst of a typecheck_info struct: ground, with
	% the io_state and the struct itself unique, but with
	% multiple references allowed for the other parts.

/*
:- inst uniq_typecheck_info	=	bound_unique(
					typecheck_info(
						unique, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground
					)
				).
*/
:- inst uniq_typecheck_info	=	ground.

:- mode typecheck_info_uo :: (free -> uniq_typecheck_info).
:- mode typecheck_info_ui :: (uniq_typecheck_info -> uniq_typecheck_info).
:- mode typecheck_info_di :: (uniq_typecheck_info -> dead).

	% Some fiddly modes used when we want to extract
	% the io_state from a typecheck_info struct and then put it back again.

/*
:- inst typecheck_info_no_io	=	bound_unique(
					typecheck_info(
						dead, ground,
						ground, ground, ground, ground,
						ground, ground, ground, ground,
						ground, ground, ground
					)
				).
*/
:- inst typecheck_info_no_io	=	ground.

:- mode typecheck_info_get_io_state 	::
		(uniq_typecheck_info -> typecheck_info_no_io).
:- mode typecheck_info_no_io 	:: 
		(typecheck_info_no_io -> typecheck_info_no_io).
:- mode typecheck_info_set_io_state 	:: (typecheck_info_no_io -> dead).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_init(io__state, module_info, pred_id, bool, tvarset,
	prog_varset, map(prog_var, type), headtypes, class_constraints,
	import_status, typecheck_info).
:- mode typecheck_info_init(di, in, in, in, in, in, in, in, in, in,
	typecheck_info_uo)
	is det.

typecheck_info_init(IOState0, ModuleInfo, PredId, IsFieldAccessFunction,
		TypeVarSet, VarSet, VarTypes, HeadTypeParams,
		Constraints, Status, TypeCheckInfo) :-
	CallPredId = call(predicate - unqualified("") / 0),
	term__context_init(Context),
	map__init(TypeBindings),
	map__init(Proofs),
	FoundTypeError = no,
	WarnedAboutOverloading = no,
	unsafe_promise_unique(IOState0, IOState),	% XXX
	TypeCheckInfo = typecheck_info(
		IOState, ModuleInfo, CallPredId, 0, PredId, Status,
		IsFieldAccessFunction, Context,
		unify_context(explicit, []), VarSet, 
		[type_assign(VarTypes, TypeVarSet, HeadTypeParams,
			TypeBindings, Constraints, Proofs)],
		FoundTypeError, WarnedAboutOverloading
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_io_state(typecheck_info, io__state).
:- mode typecheck_info_get_io_state(typecheck_info_get_io_state, uo) is det.

typecheck_info_get_io_state(TypeCheckInfo, IOState) :-
	unsafe_promise_unique(TypeCheckInfo ^ io_state, IOState). % XXX

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_io_state(typecheck_info, io__state, typecheck_info).
:- mode typecheck_info_set_io_state(typecheck_info_set_io_state, di, 
				typecheck_info_uo) is det.

typecheck_info_set_io_state(TypeCheckInfo, IOState0,
		TypeCheckInfo ^ io_state := IOState) :-
	unsafe_promise_unique(IOState0, IOState).	% XXX

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_module_name(typecheck_info, module_name).
:- mode typecheck_info_get_module_name(in, out) is det.

typecheck_info_get_module_name(TypeCheckInfo, Name) :-
	module_info_name(TypeCheckInfo ^ module_info, Name).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_module_info(typecheck_info, module_info).
:- mode typecheck_info_get_module_info(in, out) is det.

typecheck_info_get_module_info(TypeCheckInfo, TypeCheckInfo ^ module_info).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_preds(typecheck_info, predicate_table).
:- mode typecheck_info_get_preds(in, out) is det.

typecheck_info_get_preds(TypeCheckInfo, Preds) :-
	module_info_get_predicate_table(TypeCheckInfo ^ module_info, Preds).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_types(typecheck_info, type_table).
:- mode typecheck_info_get_types(in, out) is det.

typecheck_info_get_types(TypeCheckInfo, Types) :-
	module_info_types(TypeCheckInfo ^ module_info, Types).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_ctors(typecheck_info, cons_table).
:- mode typecheck_info_get_ctors(in, out) is det.

typecheck_info_get_ctors(TypeCheckInfo, Ctors) :-
	module_info_ctors(TypeCheckInfo ^ module_info, Ctors).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_called_predid(typecheck_info, call_id).
:- mode typecheck_info_get_called_predid(in, out) is det.

typecheck_info_get_called_predid(TypeCheckInfo, TypeCheckInfo ^ call_id).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_pred_markers(typecheck_info, pred_markers).
:- mode typecheck_info_get_pred_markers(in, out) is det.

typecheck_info_get_pred_markers(TypeCheckInfo, PredMarkers) :-
	typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo),
	typecheck_info_get_predid(TypeCheckInfo, PredId),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, PredMarkers).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_called_predid(call_id, typecheck_info,
			typecheck_info).
:- mode typecheck_info_set_called_predid(in, typecheck_info_di,
			typecheck_info_uo) is det.

typecheck_info_set_called_predid(PredCallId, TypeCheckInfo,
		TypeCheckInfo ^ call_id := PredCallId).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_arg_num(typecheck_info, int).
:- mode typecheck_info_get_arg_num(in, out) is det.

typecheck_info_get_arg_num(TypeCheckInfo, TypeCheckInfo ^ arg_num).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_arg_num(int, typecheck_info, typecheck_info).
:- mode typecheck_info_set_arg_num(in, typecheck_info_di, 
		typecheck_info_uo) is det.

typecheck_info_set_arg_num(ArgNum, TypeCheckInfo,
		TypeCheckInfo ^ arg_num := ArgNum).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_predid(typecheck_info, pred_id).
:- mode typecheck_info_get_predid(in, out) is det.

typecheck_info_get_predid(TypeCheckInfo, TypeCheckInfo ^ pred_id).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_context(typecheck_info, prog_context).
:- mode typecheck_info_get_context(in, out) is det.

typecheck_info_get_context(TypeCheckInfo, TypeCheckInfo ^ context).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_context(prog_context, typecheck_info, 
			typecheck_info).
:- mode typecheck_info_set_context(in, typecheck_info_di, 
			typecheck_info_uo) is det.

typecheck_info_set_context(Context, TypeCheckInfo,
		TypeCheckInfo ^ context := Context).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_unify_context(typecheck_info, unify_context).
:- mode typecheck_info_get_unify_context(in, out) is det.

typecheck_info_get_unify_context(TypeCheckInfo, TypeCheckInfo ^ unify_context).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_unify_context(unify_context, typecheck_info, 
			typecheck_info).
:- mode typecheck_info_set_unify_context(in, typecheck_info_di, 
			typecheck_info_uo) is det.

typecheck_info_set_unify_context(UnifyContext, TypeCheckInfo,
		TypeCheckInfo ^ unify_context := UnifyContext).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_varset(typecheck_info, prog_varset).
:- mode typecheck_info_get_varset(in, out) is det.

typecheck_info_get_varset(TypeCheckInfo, TypeCheckInfo ^ varset).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_type_assign_set(typecheck_info, type_assign_set).
:- mode typecheck_info_get_type_assign_set(in, out) is det.

typecheck_info_get_type_assign_set(TypeCheckInfo,
		TypeCheckInfo ^ type_assign_set).

%-----------------------------------------------------------------------------%

% typecheck_info_get_final_info(TypeCheckInfo, 
% 		OldHeadTypeParams, OldExistQVars, OldExplicitVarTypes,
%		NewTypeVarSet, New* ..., TypeRenaming, ExistTypeRenaming):
%	extracts the final inferred types from TypeCheckInfo.
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

:- pred typecheck_info_get_final_info(typecheck_info, list(tvar), existq_tvars,
		vartypes, tvarset, existq_tvars, map(prog_var, type),
		class_constraints, map(class_constraint, constraint_proof),
		map(tvar, tvar), map(tvar, tvar)).
:- mode typecheck_info_get_final_info(in, in, in, in,
		out, out, out, out, out, out, out) is det.

typecheck_info_get_final_info(TypeCheckInfo, OldHeadTypeParams, OldExistQVars, 
		OldExplicitVarTypes, NewTypeVarSet, NewHeadTypeParams,
		NewVarTypes, NewTypeConstraints, NewConstraintProofs, TSubst,
		ExistTypeRenaming) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo, TypeAssignSet),
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
:- pred get_existq_tvar_renaming(list(tvar), existq_tvars, tsubst, 
	map(tvar, tvar)).
:- mode get_existq_tvar_renaming(in, in, in, out) is det.

get_existq_tvar_renaming(OldHeadTypeParams, ExistQVars, TypeBindings,
		ExistTypeRenaming) :-
	MaybeAddToMap = lambda([TVar::in, Renaming0::in, Renaming::out] is det,
		(
			term__apply_rec_substitution(
				term__variable(TVar),
				TypeBindings, Result),
			(
				Result = term__variable(NewTVar),
				NewTVar \= TVar,
				\+ list__member(NewTVar, OldHeadTypeParams)
			->
				map__det_insert(Renaming0, TVar, NewTVar,
					Renaming)
			;
				Renaming = Renaming0
			)
		)),
	map__init(ExistTypeRenaming0),
	list__foldl(MaybeAddToMap, ExistQVars, ExistTypeRenaming0,
		ExistTypeRenaming).


	% fully expand the types of the variables by applying the type
	% bindings

:- pred expand_types(list(prog_var), tsubst, map(prog_var, type),
		map(prog_var, type)).
:- mode expand_types(in, in, in, out) is det.

expand_types([], _, VarTypes, VarTypes).
expand_types([Var | Vars], TypeSubst, VarTypes0, VarTypes) :-
	map__lookup(VarTypes0, Var, Type0),
	term__apply_rec_substitution(Type0, TypeSubst, Type),
	map__det_update(VarTypes0, Var, Type, VarTypes1),
	expand_types(Vars, TypeSubst, VarTypes1, VarTypes).

:- pred rename_constraint_proof(map(tvar, tvar), constraint_proof,
				constraint_proof).
:- mode rename_constraint_proof(in, in, out) is det.

% apply a type variable renaming to a class constraint proof

rename_constraint_proof(_TSubst, apply_instance(Num),
				apply_instance(Num)).
rename_constraint_proof(TSubst, superclass(ClassConstraint0),
			superclass(ClassConstraint)) :-
	apply_variable_renaming_to_constraint(TSubst, ClassConstraint0,
			ClassConstraint).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_type_assign_set(typecheck_info, type_assign_set,
			typecheck_info).
:- mode typecheck_info_set_type_assign_set(typecheck_info_di, in,
			typecheck_info_uo) is det.

typecheck_info_set_type_assign_set(TypeCheckInfo, TypeAssignSet,
		TypeCheckInfo ^ type_assign_set := TypeAssignSet).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_found_error(typecheck_info, bool).
:- mode typecheck_info_get_found_error(typecheck_info_ui, out) is det.

typecheck_info_get_found_error(TypeCheckInfo, TypeCheckInfo ^ found_error).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_found_error(typecheck_info, bool, typecheck_info).
:- mode typecheck_info_set_found_error(typecheck_info_di, in, 
			typecheck_info_uo) is det.

typecheck_info_set_found_error(TypeCheckInfo, FoundError,
	TypeCheckInfo ^ found_error := FoundError).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_warned_about_overloading(typecheck_info, bool).
:- mode typecheck_info_get_warned_about_overloading(typecheck_info_ui, out)
			is det.

typecheck_info_get_warned_about_overloading(TypeCheckInfo,
		TypeCheckInfo ^ warned_about_overloading).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_warned_about_overloading(typecheck_info, bool, 
			typecheck_info).
:- mode typecheck_info_set_warned_about_overloading(typecheck_info_di, in, 
			typecheck_info_uo) is det.

typecheck_info_set_warned_about_overloading(TypeCheckInfo, Warned,
		TypeCheckInfo ^ warned_about_overloading := Warned).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_pred_import_status(typecheck_info, import_status).
:- mode typecheck_info_get_pred_import_status(typecheck_info_ui, out) is det.

typecheck_info_get_pred_import_status(TypeCheckInfo,
		TypeCheckInfo ^ import_status).

:- pred typecheck_info_set_pred_import_status(typecheck_info, import_status,
			typecheck_info).
:- mode typecheck_info_set_pred_import_status(typecheck_info_di, in,
			typecheck_info_uo) is det.

typecheck_info_set_pred_import_status(TypeCheckInfo, Status,
		TypeCheckInfo ^ import_status := Status).

%-----------------------------------------------------------------------------%

	%
	% Note: changes here may require changes to
	% post_typecheck__resolve_unify_functor,
	% intermod__module_qualify_unify_rhs,
	% recompilation_usage__find_matching_constructors
	% and recompilation_check__check_functor_ambiguities.
:- pred typecheck_info_get_ctor_list(typecheck_info, cons_id, int, 
			list(cons_type_info), list(invalid_field_update)).
:- mode typecheck_info_get_ctor_list(typecheck_info_ui,
			in, in, out, out) is det.

typecheck_info_get_ctor_list(TypeCheckInfo, Functor, Arity,
		ConsInfoList, InvalidFieldUpdates) :-
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
		TypeCheckInfo ^ is_field_access_function = yes,
		TypeCheckInfo ^ import_status \= opt_imported
	->
		(
			builtin_field_access_function_type(TypeCheckInfo,
				Functor, Arity, FieldAccessConsInfoList,
				InvalidFieldUpdates0)
		->
			ConsInfoList = FieldAccessConsInfoList,
			InvalidFieldUpdates = InvalidFieldUpdates0
		;
			ConsInfoList = [],
			InvalidFieldUpdates = []
		)
	;
		typecheck_info_get_ctor_list_2(TypeCheckInfo, Functor, Arity,
			ConsInfoList, InvalidFieldUpdates)
	).

:- pred typecheck_info_get_ctor_list_2(typecheck_info, cons_id,
		int, list(cons_type_info), list(invalid_field_update)).
:- mode typecheck_info_get_ctor_list_2(typecheck_info_ui,
		in, in, out, out) is det.

typecheck_info_get_ctor_list_2(TypeCheckInfo, Functor, Arity,
		ConsInfoList, InvalidFieldUpdates) :-
	% Check if `Functor/Arity' has been defined as a constructor
	% in some discriminated union type(s).  This gives
	% us a list of possible cons_type_infos.
	typecheck_info_get_ctors(TypeCheckInfo, Ctors),
	(
		Functor = cons(_, _),
		map__search(Ctors, Functor, HLDS_ConsDefnList)
	->
		convert_cons_defn_list(TypeCheckInfo, HLDS_ConsDefnList,
			ConsInfoList0)
	;
		ConsInfoList0 = []
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
		convert_cons_defn_list(TypeCheckInfo, HLDS_ExistQConsDefnList,
			ExistQuantifiedConsInfoList),
		list__filter_map(flip_quantifiers, ExistQuantifiedConsInfoList,
			UnivQuantifiedConsInfoList),
		list__append(UnivQuantifiedConsInfoList,
			ConsInfoList0, ConsInfoList1)
	;
		ConsInfoList1 = ConsInfoList0
	),

	% Check if Functor is a constant of one of the builtin atomic
	% types (string, float, int, character).  If so, insert
	% the resulting cons_type_info at the start of the list.
	(
		Arity = 0,
		builtin_atomic_type(Functor, BuiltInTypeName)
	->
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
	(
		builtin_pred_type(TypeCheckInfo, Functor, Arity,
			PredConsInfoList)
	->
		list__append(ConsInfoList3, PredConsInfoList, ConsInfoList4)
	;
		ConsInfoList4 = ConsInfoList3
	),
	
	%
	% Check if Functor is a field access function for which the
	% user has not supplied a declaration.
	%
	(
		builtin_field_access_function_type(TypeCheckInfo,
			Functor, Arity, FieldAccessConsInfoList,
			InvalidFieldUpdates0)
	->
		list__append(FieldAccessConsInfoList,
			ConsInfoList4, ConsInfoList5),
		InvalidFieldUpdates = InvalidFieldUpdates0
	;
		InvalidFieldUpdates = [],
		ConsInfoList5 = ConsInfoList4
	),

	%
	% Check for higher-order function calls
	%
	(
		builtin_apply_type(TypeCheckInfo, Functor, Arity,
			ApplyConsInfoList)
	->
		ConsInfoList = list__append(ConsInfoList5, ApplyConsInfoList)
	;
		ConsInfoList = ConsInfoList5
	).

:- pred flip_quantifiers(cons_type_info, cons_type_info).
:- mode flip_quantifiers(in, out) is semidet.

flip_quantifiers(cons_type_info(A, ExistQVars0, C, D, Constraints0),
		cons_type_info(A, ExistQVars, C, D, Constraints)) :-
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

%-----------------------------------------------------------------------------%

:- pred report_unsatisfiable_constraints(type_assign_set, typecheck_info,
					typecheck_info).
:- mode report_unsatisfiable_constraints(in, typecheck_info_di,
					typecheck_info_uo) is det.

report_unsatisfiable_constraints(TypeAssignSet, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_io_state(TypeCheckInfo0, IOState0),

	typecheck_info_get_context(TypeCheckInfo0, Context),
	write_context_and_pred_id(TypeCheckInfo0, IOState0, IOState1),
	prog_out__write_context(Context, IOState1, IOState2),
	io__write_string("  unsatisfiable typeclass constraint(s):\n",
		IOState2, IOState3),

	WriteConstraints = lambda([TypeAssign::in, IO0::di, IO::uo] is det,
		(
			type_assign_get_typeclass_constraints(
				TypeAssign, Constraints),
			Constraints = constraints(UnprovenConstraints0,
					_AssumedConstraints),
			
			type_assign_get_typevarset(TypeAssign, VarSet),
			type_assign_get_type_bindings(TypeAssign, Bindings),
			apply_rec_subst_to_constraint_list(Bindings,
				UnprovenConstraints0, UnprovenConstraints1),
			list__sort_and_remove_dups(UnprovenConstraints1,
				UnprovenConstraints),
			prog_out__write_context(Context, IO0, IO1),
			io__write_string("  `", IO1, IO2),
			AppendVarnums = no,
			io__write_list(UnprovenConstraints, "', `",
			    mercury_output_constraint(VarSet, AppendVarnums),
			    IO2, IO3),
			io__write_string("'.\n", IO3, IO)
		)),

		% XXX this won't be very pretty when there are
		% XXX multiple type_assigns.
	io__write_list(TypeAssignSet, "\n", WriteConstraints, 
		IOState3, IOState),

	typecheck_info_set_io_state(TypeCheckInfo0, IOState, TypeCheckInfo1),
	typecheck_info_set_found_error(TypeCheckInfo1, yes, TypeCheckInfo).

%-----------------------------------------------------------------------------%

% perform_context_reduction(OrigTypeAssignSet, TypeCheckInfo0, TypeCheckInfo)
%	is true iff either
% 	TypeCheckInfo is the typecheck_info that results from performing
% 	context reduction on the type_assigns in TypeCheckInfo0,
%	or, if there is no valid context reduction, then
%	TypeCheckInfo is TypeCheckInfo0 with the type assign set replaced by
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

:- pred perform_context_reduction(type_assign_set,
			typecheck_info, typecheck_info).
:- mode perform_context_reduction(in,
			typecheck_info_di, typecheck_info_uo) is det.

perform_context_reduction(OrigTypeAssignSet, TypeCheckInfo0, TypeCheckInfo) :-
	checkpoint("before context reduction", TypeCheckInfo0, TypeCheckInfo1),
	typecheck_info_get_module_info(TypeCheckInfo1, ModuleInfo),
	module_info_superclasses(ModuleInfo, SuperClassTable),
	module_info_instances(ModuleInfo, InstanceTable),
	typecheck_info_get_type_assign_set(TypeCheckInfo1, TypeAssignSet0),
	list__filter_map(reduce_type_assign_context(SuperClassTable, 
			InstanceTable), 
		TypeAssignSet0, TypeAssignSet),
	(
			% Check that this context reduction hasn't eliminated
			% all the type assignments.
		TypeAssignSet = [], 
		TypeAssignSet0 \= []
	->
		report_unsatisfiable_constraints(TypeAssignSet0,
			TypeCheckInfo1, TypeCheckInfo2),
		DeleteConstraints = lambda([TA0::in, TA::out] is det, (
			type_assign_get_typeclass_constraints(TA0, 
				constraints(_, AssumedConstraints)),
			type_assign_set_typeclass_constraints(TA0, 
				constraints([], AssumedConstraints), TA)
		)),
		list__map(DeleteConstraints, OrigTypeAssignSet,
			NewTypeAssignSet),
		typecheck_info_set_type_assign_set(TypeCheckInfo2,
			NewTypeAssignSet, TypeCheckInfo)
	;
		typecheck_info_set_type_assign_set(TypeCheckInfo1,
			TypeAssignSet, TypeCheckInfo)
	).

:- pred reduce_type_assign_context(superclass_table, instance_table,
			type_assign, type_assign).
:- mode reduce_type_assign_context(in, in, in, out) is semidet.

reduce_type_assign_context(SuperClassTable, InstanceTable, 
		TypeAssign0, TypeAssign) :-
	type_assign_get_head_type_params(TypeAssign0, HeadTypeParams),
	type_assign_get_type_bindings(TypeAssign0, Bindings),
	type_assign_get_typeclass_constraints(TypeAssign0, Constraints0),
	type_assign_get_typevarset(TypeAssign0, Tvarset0),
	type_assign_get_constraint_proofs(TypeAssign0, Proofs0),

	Constraints0 = constraints(UnprovenConstraints0, AssumedConstraints),

	typecheck__reduce_context_by_rule_application(InstanceTable, 
		SuperClassTable, AssumedConstraints,
		Bindings, Tvarset0, Tvarset, Proofs0, Proofs,
		UnprovenConstraints0, UnprovenConstraints),

	check_satisfiability(UnprovenConstraints, HeadTypeParams),

	Constraints = constraints(UnprovenConstraints, AssumedConstraints),

	type_assign_set_typeclass_constraints(TypeAssign0, Constraints,
		TypeAssign1),
	type_assign_set_typevarset(TypeAssign1, Tvarset, TypeAssign2),
	type_assign_set_constraint_proofs(TypeAssign2, Proofs, TypeAssign).


typecheck__reduce_context_by_rule_application(InstanceTable, SuperClassTable, 
		AssumedConstraints, Bindings, Tvarset0, Tvarset,
		Proofs0, Proofs, Constraints0, Constraints) :-
	apply_rec_subst_to_constraint_list(Bindings, Constraints0,
		Constraints1),
	eliminate_assumed_constraints(Constraints1, AssumedConstraints,
		Constraints2, Changed1),
	apply_instance_rules(Constraints2, InstanceTable, 
		Tvarset0, Tvarset1, Proofs0, Proofs1, Constraints3, Changed2),
	varset__vars(Tvarset1, Tvars),
	apply_class_rules(Constraints3, AssumedConstraints, Tvars,
		SuperClassTable, Tvarset0, Proofs1, Proofs2, Constraints4,
		Changed3),
	(
		Changed1 = no, Changed2 = no, Changed3 = no
	->
			% We have reached fixpoint
		list__sort_and_remove_dups(Constraints4, Constraints),
		Tvarset = Tvarset1,
		Proofs = Proofs2
	;
		typecheck__reduce_context_by_rule_application(InstanceTable,
			SuperClassTable, AssumedConstraints,
			Bindings, Tvarset1, Tvarset, Proofs2, Proofs, 
			Constraints4, Constraints)
	).

:- pred eliminate_assumed_constraints(list(class_constraint), 
	list(class_constraint), list(class_constraint), bool).
:- mode eliminate_assumed_constraints(in, in, out, out) is det.

eliminate_assumed_constraints([], _, [], no).
eliminate_assumed_constraints([C|Cs], AssumedCs, NewCs, Changed) :-
	eliminate_assumed_constraints(Cs, AssumedCs, NewCs0, Changed0),
	(
		list__member(C, AssumedCs)
	->
		NewCs = NewCs0,
		Changed = yes
	;
		NewCs = [C|NewCs0],
		Changed = Changed0
	).

:- pred apply_instance_rules(list(class_constraint), instance_table,
	tvarset, tvarset, map(class_constraint, constraint_proof),
	map(class_constraint, constraint_proof), list(class_constraint), bool).
:- mode apply_instance_rules(in, in, in, out, in, out, out, out) is det.

apply_instance_rules([], _, Names, Names, Proofs, Proofs, [], no).
apply_instance_rules([C|Cs], InstanceTable, TVarSet, NewTVarSet,
		Proofs0, Proofs, Constraints, Changed) :-
	C = constraint(ClassName, Types),
	list__length(Types, Arity),
	map__lookup(InstanceTable, class_id(ClassName, Arity), Instances),
	(
		find_matching_instance_rule(Instances, ClassName, Types,
			TVarSet, NewTVarSet0, Proofs0, Proofs1,
			NewConstraints0)
	->
			% Put the new constraints at the front of the list
		NewConstraints = NewConstraints0,
		NewTVarSet1 = NewTVarSet0,
		Proofs2 = Proofs1,
		Changed1 = yes
	;
			% Put the old constraint at the front of the list
		NewConstraints = [C],
		NewTVarSet1 = TVarSet,
		Proofs2 = Proofs0,
		Changed1 = no
	),
	apply_instance_rules(Cs, InstanceTable, NewTVarSet1,
		NewTVarSet, Proofs2, Proofs, TheRest, Changed2),
	bool__or(Changed1, Changed2, Changed),
	list__append(NewConstraints, TheRest, Constraints).

	% We take the first matching instance rule that we can find; any
	% overlapping instance declarations will have been caught earlier.

	% This pred also catches tautological constraints since the
	% NewConstraints will be [].

	% XXX Surely we shouldn't need to re-name the variables and return
	% XXX a new varset: this substitution should have been worked out
	% XXX before, as these varsets would already have been merged.
:- pred find_matching_instance_rule(list(hlds_instance_defn), sym_name,
	list(type), tvarset, tvarset, map(class_constraint, constraint_proof), 
	map(class_constraint, constraint_proof), list(class_constraint)).
:- mode find_matching_instance_rule(in, in, in, in, out, in, out, out) 
	is semidet.

find_matching_instance_rule(Instances, ClassName, Types, TVarSet,
		NewTVarSet, Proofs0, Proofs, NewConstraints) :-
		
		% Start a counter so we remember which instance decl we have	
		% used.
	find_matching_instance_rule_2(Instances, 1, ClassName, Types,
		TVarSet, NewTVarSet, Proofs0, Proofs, NewConstraints).

:- pred find_matching_instance_rule_2(list(hlds_instance_defn), int,
	sym_name, list(type), tvarset, tvarset,
	map(class_constraint, constraint_proof), 
	map(class_constraint, constraint_proof), list(class_constraint)).
:- mode find_matching_instance_rule_2(in, in, in, in, in, out, in, out, out) 
	is semidet.

find_matching_instance_rule_2([I|Is], N0, ClassName, Types, TVarSet,
		NewTVarSet, Proofs0, Proofs, NewConstraints) :-
	I = hlds_instance_defn(_InstanceModule, _Status, _Context,
		NewConstraints0, InstanceTypes0, _Interface, _PredProcIds,
		InstanceNames, _SuperClassProofs),
	(
		varset__merge_subst(TVarSet, InstanceNames, NewTVarSet0,
			RenameSubst),
		term__apply_substitution_to_list(InstanceTypes0,
			RenameSubst, InstanceTypes),
		type_list_subsumes(InstanceTypes, Types, Subst)
	->
		NewTVarSet = NewTVarSet0,
		apply_subst_to_constraint_list(RenameSubst, 
			NewConstraints0, NewConstraints1),
		apply_rec_subst_to_constraint_list(Subst, 
			NewConstraints1, NewConstraints),

		NewProof = apply_instance(N0),
		Constraint = constraint(ClassName, Types),
		map__set(Proofs0, Constraint, NewProof, Proofs)
	;
		N is N0 + 1,
		find_matching_instance_rule_2(Is, N, ClassName,
			Types, TVarSet, NewTVarSet, Proofs0,
			Proofs, NewConstraints)
	).

	% To reduce a constraint using class declarations, we search the
	% superclass relation to find a path from the inferred constraint to
	% another (declared or inferred) constraint.
:- pred apply_class_rules(list(class_constraint), list(class_constraint),
	list(tvar), superclass_table, tvarset, 
	map(class_constraint, constraint_proof),
	map(class_constraint, constraint_proof), list(class_constraint), bool).
:- mode apply_class_rules(in, in, in, in, in, in, out, out, out) is det.

apply_class_rules([], _, _, _, _, Proofs, Proofs, [], no).
apply_class_rules([C|Constraints0], AssumedConstraints, HeadTypeParams,
		SuperClassTable, TVarSet, Proofs0, Proofs, 
		Constraints, Changed) :-
	(
		Parents = [],
		eliminate_constraint_by_class_rules(C, HeadTypeParams, 
			AssumedConstraints,
			SuperClassTable, TVarSet, Parents, Proofs0, Proofs1)
	->
		apply_class_rules(Constraints0, AssumedConstraints, 
			HeadTypeParams, SuperClassTable, TVarSet, 
			Proofs1, Proofs, Constraints, _),
		Changed = yes
	;
		apply_class_rules(Constraints0, AssumedConstraints,
			HeadTypeParams, SuperClassTable, TVarSet, 
			Proofs0, Proofs, Constraints1, Changed),
		Constraints = [C|Constraints1]
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
:- pred eliminate_constraint_by_class_rules(class_constraint, list(tvar),
	list(class_constraint), superclass_table, tvarset,
	list(class_constraint),
	map(class_constraint, constraint_proof),
	map(class_constraint, constraint_proof)).
:- mode eliminate_constraint_by_class_rules(in, in, in, in, in, in, in, out) 
	is semidet.

eliminate_constraint_by_class_rules(C, ConstVars,
		AssumedConstraints, SuperClassTable,
		TVarSet, ParentConstraints, Proofs0, Proofs) :-

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
	SubDetailsToConstraint = (pred(SubClassDetails::in, SubC::out) 
			is det :-
		SubClassDetails = subclass_details(SuperVars0, SubID,
			SubVars0, SuperVarset),

			% Rename the variables from the typeclass
			% declaration into those of the current pred
		varset__merge_subst(TVarSet, SuperVarset, _NewTVarSet, 
			RenameSubst),
		term__var_list_to_term_list(SubVars0, SubVars1),
		term__apply_substitution_to_list(SubVars1, 
			RenameSubst, SubVars),
		term__var_list_to_term_list(SuperVars0, SuperVars1),
		term__apply_substitution_to_list(SuperVars1,
			RenameSubst, SuperVars),

			% Work out what the (renamed) vars from the
			% typeclass declaration are bound to here
		map__init(Empty),
		(
			type_unify_list(SuperVars, SuperClassTypes, [],
				Empty, Bindings)
		->
			SubID = class_id(SubName, _SubArity),
			term__apply_substitution_to_list(SubVars, Bindings,
				SubClassTypes),
			SubC = constraint(SubName, SubClassTypes)
		;
			error("eliminate_constraint_by_class_rules: type_unify_list failed")
		)
	),
	list__map(SubDetailsToConstraint, SubClasses, SubClassConstraints),

	(
			% Do the first level of search. We search for
			% an assumed constraint which unifies with any
			% of the subclass constraints.
		FindSub = (pred(TheConstraint::in) is semidet :-
			some [SubClassConstraint] (
				TheConstraint = constraint(TheConstraintClass,
					TheConstraintTypes),
				list__member(SubClassConstraint, 
					SubClassConstraints),
				SubClassConstraint = 
					constraint(TheConstraintClass, 
					SubClassConstraintTypes),
				map__init(EmptySub),
				type_unify_list(SubClassConstraintTypes, 
					TheConstraintTypes,
					ConstVars, EmptySub, _)
			)
		),
		find_first_match(FindSub, AssumedConstraints, Sub)
	->
		map__set(Proofs0, C, superclass(Sub), Proofs)
	;
		NewParentConstraints = [C|ParentConstraints],

			% Recursively search the rest of the superclass
			% relation.
		SubClassSearch = (pred(Constraint::in, CnstrtAndProof::out) 
				is semidet :-
			eliminate_constraint_by_class_rules(Constraint, 
				ConstVars, AssumedConstraints, SuperClassTable,
				TVarSet, NewParentConstraints,
				Proofs0, SubProofs),
			CnstrtAndProof = Constraint - SubProofs
		),
			% XXX this could (and should) be more efficient. 
			% (ie. by manually doing a "cut").
		find_first(SubClassSearch, SubClassConstraints,
			NewSub - NewProofs),
		map__set(NewProofs, C, superclass(NewSub), Proofs)
	).

	% XXX this should probably work its way into the library.
	% This is just like list__filter_map except that it only returns
	% the first match:
	% 	find_first(X,Y,Z) <=> list__filter_map(X,Y,[Z|_])
	%
:- pred find_first(pred(X, Y), list(X), Y).
:- mode find_first(pred(in, out) is semidet, in, out) is semidet.

find_first(Pred, [X|Xs], Result) :-
	(
		call(Pred, X, Result0)
	->
		Result = Result0
	;
		find_first(Pred, Xs, Result)
	).

	% 	find_first_match(X,Y,Z) <=> list__takewhile(not X,Y,_, [Z|_])
:- pred find_first_match(pred(X), list(X), X).
:- mode find_first_match(pred(in) is semidet, in, out) is semidet.

find_first_match(Pred, [X|Xs], Result) :-
	(
		call(Pred, X)
	->
		Result = X
	;
		find_first_match(Pred, Xs, Result)
	).

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
:- pred check_satisfiability(list(class_constraint), head_type_params).
:- mode check_satisfiability(in, in) is semidet.

check_satisfiability(Constraints, HeadTypeParams) :-
	all [C] list__member(C, Constraints) => (
		C = constraint(_ClassName, Types),
		term__contains_var_list(Types, TVar),
		not list__member(TVar, HeadTypeParams)
	).

%-----------------------------------------------------------------------------%

:- pred convert_cons_defn_list(typecheck_info, list(hlds_cons_defn),
				list(cons_type_info)).
:- mode convert_cons_defn_list(typecheck_info_ui, in, out) is det.

convert_cons_defn_list(_TypeCheckInfo, [], []).
convert_cons_defn_list(TypeCheckInfo, [X|Xs], [Y|Ys]) :-
	convert_cons_defn(TypeCheckInfo, X, Y),
	convert_cons_defn_list(TypeCheckInfo, Xs, Ys).

:- pred convert_cons_defn(typecheck_info, hlds_cons_defn, cons_type_info).
:- mode convert_cons_defn(typecheck_info_ui, in, out) is det.

convert_cons_defn(TypeCheckInfo, HLDS_ConsDefn, ConsTypeInfo) :-
	HLDS_ConsDefn = hlds_cons_defn(ExistQVars, ExistConstraints, Args,
				TypeId, _),
	assoc_list__values(Args, ArgTypes),
	typecheck_info_get_types(TypeCheckInfo, Types),
	map__lookup(Types, TypeId, TypeDefn),
	hlds_data__get_type_defn_tvarset(TypeDefn, ConsTypeVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, ConsTypeParams),
	construct_type(TypeId, ConsTypeParams, ConsType),
	UnivConstraints = [],
	Constraints = constraints(UnivConstraints, ExistConstraints),
	ConsTypeInfo = cons_type_info(ConsTypeVarSet, ExistQVars,
				ConsType, ArgTypes, Constraints).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The type_assign and type_assign_set data structures.

:- type type_assign_set	==	list(type_assign).

:- type type_assign	
	--->	type_assign(
			map(prog_var, type),	% var types
			tvarset,		% type names
			headtypes,		% universally quantified
						% type variables
			tsubst,			% type bindings
			class_constraints,	% typeclass constraints.
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
			map(class_constraint,	% for each constraint
			    constraint_proof)	% found to be redundant, 
			    			% why is it so?
		).

%-----------------------------------------------------------------------------%

	% Access predicates for the type_assign data structure.
	% Excruciatingly boring code.

:- pred type_assign_get_var_types(type_assign, map(prog_var, type)).
:- mode type_assign_get_var_types(in, out) is det.

type_assign_get_var_types(type_assign(VarTypes, _, _, _, _, _), VarTypes).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_typevarset(type_assign, tvarset).
:- mode type_assign_get_typevarset(in, out) is det.

type_assign_get_typevarset(type_assign(_, TypeVarSet, _, _, _, _), TypeVarSet).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_head_type_params(type_assign, headtypes).
:- mode type_assign_get_head_type_params(in, out) is det.

type_assign_get_head_type_params(type_assign(_, _, HeadTypeParams, _, _, _),
			HeadTypeParams).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_type_bindings(type_assign, tsubst).
:- mode type_assign_get_type_bindings(in, out) is det.

type_assign_get_type_bindings(type_assign(_, _, _, TypeBindings, _, _),
	TypeBindings).
%-----------------------------------------------------------------------------%

:- pred type_assign_get_typeclass_constraints(type_assign, class_constraints).
:- mode type_assign_get_typeclass_constraints(in, out) is det.

type_assign_get_typeclass_constraints(type_assign(_, _, _, _, Constraints, _),
	Constraints).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_constraint_proofs(type_assign,
	map(class_constraint, constraint_proof)).
:- mode type_assign_get_constraint_proofs(in, out) is det.

type_assign_get_constraint_proofs(type_assign(_, _, _, _, _, Proofs), Proofs).  
%-----------------------------------------------------------------------------%

:- pred type_assign_set_var_types(type_assign, map(prog_var, type),
		type_assign).
:- mode type_assign_set_var_types(in, in, out) is det.

type_assign_set_var_types(type_assign(_, B, C, D, E, F), VarTypes,
			type_assign(VarTypes, B, C, D, E, F)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_typevarset(type_assign, tvarset, type_assign).
:- mode type_assign_set_typevarset(in, in, out) is det.

type_assign_set_typevarset(type_assign(A, _, C, D, E, F), TypeVarSet,
			type_assign(A, TypeVarSet, C, D, E, F)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_head_type_params(type_assign, headtypes, type_assign).
:- mode type_assign_set_head_type_params(in, in, out) is det.

type_assign_set_head_type_params(type_assign(A, B, _, D, E, F), HeadTypeParams,
			type_assign(A, B, HeadTypeParams, D, E, F)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_type_bindings(type_assign, tsubst, type_assign).
:- mode type_assign_set_type_bindings(in, in, out) is det.

type_assign_set_type_bindings(type_assign(A, B, C, _, E, F), TypeBindings,
			type_assign(A, B, C, TypeBindings, E, F)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_typeclass_constraints(type_assign, class_constraints,
			type_assign).
:- mode type_assign_set_typeclass_constraints(in, in, out) is det.

type_assign_set_typeclass_constraints(type_assign(A, B, C, D, _, F),
			Constraints, type_assign(A, B, C, D, Constraints, F)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_constraint_proofs(type_assign,
	map(class_constraint, constraint_proof), type_assign).
:- mode type_assign_set_constraint_proofs(in, in, out) is det.

type_assign_set_constraint_proofs(type_assign(A, B, C, D, E, _),
			Proofs, type_assign(A, B, C, D, E, Proofs)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The next section contains predicates for writing diagnostics
% (warnings and errors).

%-----------------------------------------------------------------------------%

	% write out the inferred `pred' or `func' declarations
	% for a list of predicates.  Don't write out the inferred types
	% for assertions.

:- pred write_inference_messages(list(pred_id), module_info,
				io__state, io__state).
:- mode write_inference_messages(in, in, di, uo) is det.

write_inference_messages([], _) --> [].
write_inference_messages([PredId | PredIds], ModuleInfo) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_markers(PredInfo, Markers) },
	(
		{ check_marker(Markers, infer_type) },
		{ module_info_predids(ModuleInfo, ValidPredIds) },
		{ list__member(PredId, ValidPredIds) },
		{ \+ pred_info_get_goal_type(PredInfo, assertion) }
	->
		write_inference_message(PredInfo)
	;
		[]
	),
	write_inference_messages(PredIds, ModuleInfo).

	% write out the inferred `pred' or `func' declaration
	% for a single predicate.

:- pred write_inference_message(pred_info, io__state, io__state).
:- mode write_inference_message(in, di, uo) is det.

write_inference_message(PredInfo) -->
	{ pred_info_name(PredInfo, PredName) },
	{ Name = unqualified(PredName) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_arg_types(PredInfo, VarSet, ExistQVars, Types0) },
	{ strip_builtin_qualifiers_from_type_list(Types0, Types) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_get_class_context(PredInfo, ClassContext) },
	{ pred_info_get_purity(PredInfo, Purity) },
	{ MaybeDet = no },
	prog_out__write_context(Context),
	io__write_string("Inferred "),
	{ AppendVarNums = no },
	(	{ PredOrFunc = predicate },
		mercury_output_pred_type(VarSet, ExistQVars, Name, Types,
			MaybeDet, Purity, ClassContext, Context, AppendVarNums)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Types, ArgTypes, RetType) },
		mercury_output_func_type(VarSet, ExistQVars, Name, ArgTypes,
			RetType, MaybeDet, Purity, ClassContext, Context,
			AppendVarNums)
	).

%-----------------------------------------------------------------------------%

:- pred report_error_no_clauses(pred_id, pred_info,
					module_info, io__state, io__state).
:- mode report_error_no_clauses(in, in, in, di, uo) is det.

report_error_no_clauses(PredId, PredInfo, ModuleInfo) -->
	{ pred_info_context(PredInfo, Context) },
	{ error_util__describe_one_pred_name(ModuleInfo, PredId, PredName0) },
	{ string__append(PredName0, ".", PredName) },
	{ ErrorMsg = [ words("Error: no clauses for "), fixed(PredName) ] },
	error_util__write_error_pieces(Context, 0, ErrorMsg).

%-----------------------------------------------------------------------------%

:- pred report_warning_too_much_overloading(typecheck_info, io__state, 
			io__state).
:- mode report_warning_too_much_overloading(typecheck_info_no_io, di, uo)
			is det.

report_warning_too_much_overloading(TypeCheckInfo) -->
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ make_pred_id_preamble(TypeCheckInfo, Preamble) },
	{ SmallWarning = [ fixed(Preamble),
			words("warning: highly ambiguous overloading.") ] },
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		{ VerboseWarning = [
			words("This may cause type-checking to be very"),
			words("slow. It may also make your code"),
			words("difficult to understand.") ] },
		{ list__append(SmallWarning, VerboseWarning, Warning) }
	;
		{ Warning = SmallWarning }
	),
	error_util__report_warning(Context, 0, Warning).

%-----------------------------------------------------------------------------%

:- pred report_error_unif_var_var(typecheck_info, prog_var, prog_var,
		type_assign_set, io__state, io__state).
:- mode report_error_unif_var_var(typecheck_info_no_io, in, in, in, di, uo)
					is det.

report_error_unif_var_var(TypeCheckInfo, X, Y, TypeAssignSet) -->

	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },

	write_context_and_pred_id(TypeCheckInfo),
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
	write_type_of_var(TypeCheckInfo, Context, TypeAssignSet, X),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("'"),
	write_type_of_var(TypeCheckInfo, Context, TypeAssignSet, Y),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_functor_type(typecheck_info, prog_var,
		list(cons_type_info), cons_id, int, type_assign_set,
		io__state, io__state).
:- mode report_error_functor_type(typecheck_info_no_io, in, in, in, in, in,
					di, uo) is det.

report_error_functor_type(TypeCheckInfo, Var, ConsDefnList, Functor, Arity,
		TypeAssignSet) -->

	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },

	write_context_and_pred_id(TypeCheckInfo),
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
	write_type_of_var(TypeCheckInfo, Context, TypeAssignSet, Var),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  "),
	write_functor_name(Functor, Arity),
	write_type_of_functor(Functor, Arity, Context, ConsDefnList),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_lambda_var(typecheck_info, pred_or_func,
		lambda_eval_method, prog_var, list(prog_var), type_assign_set,
		io__state, io__state).
:- mode report_error_lambda_var(typecheck_info_no_io,
		in, in, in, in, in, di, uo) is det.

report_error_lambda_var(TypeCheckInfo, PredOrFunc, EvalMethod, Var, ArgVars,
				TypeAssignSet) -->

	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },

	write_context_and_pred_id(TypeCheckInfo),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  type error in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string("\n"),
	prog_out__write_context(Context),

	{ EvalMethod = normal, EvalStr = ""
	; EvalMethod = (aditi_bottom_up), EvalStr = "aditi_bottom_up "
	; EvalMethod = (aditi_top_down), EvalStr = "aditi_top_down "
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
	write_type_of_var(TypeCheckInfo, Context, TypeAssignSet, Var),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  lambda expression has type `"),
	(	{ PredOrFunc = predicate },
		io__write_string("pred"),
		( { ArgVars = [] } ->
			[]
		;
			io__write_string("(_"),
			{ list__length(ArgVars, NumArgVars) },
			{ NumArgVars1 is NumArgVars - 1 },
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
			{ NumArgVars1 is NumArgVars - 1 },
			{ list__duplicate(NumArgVars1, ", _", Strings) },
			io__write_strings(Strings),
			io__write_string(")")
		),
		io__write_string(" = _")
	),
	io__write_string("'.\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_functor_arg_types(typecheck_info, prog_var,
		list(cons_type_info), cons_id, list(prog_var),
		args_type_assign_set, io__state, io__state).
:- mode report_error_functor_arg_types(typecheck_info_no_io, in, in, in, in,
			in, di, uo) is det.

report_error_functor_arg_types(TypeCheckInfo, Var, ConsDefnList,
			Functor, Args, ArgsTypeAssignSet) -->

	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	{ typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo) },
	{ list__length(Args, Arity) },

	write_context_and_pred_id(TypeCheckInfo),
	hlds_out__write_unify_context(UnifyContext, Context),

	prog_out__write_context(Context),
	io__write_string("  in unification of "),
	write_argument_name(VarSet, Var),
	io__write_string("\n"),
	prog_out__write_context(Context),
	io__write_string("  and term `"),
	{ strip_builtin_qualifier_from_cons_id(Functor, Functor1) },
	hlds_out__write_functor_cons_id(Functor1, Args, VarSet, ModuleInfo, no),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  type error in argument(s) of "),
	write_functor_name(Functor1, Arity),
	io__write_string(".\n"),

	% If we know the type of the function symbol, and each argument
	% also has at most one possible type, then we prefer to print an
	% error message that mentions the actual and expected types of the
	% arguments only for the arguments in which the two types differ.
	(
		{ ArgsTypeAssignSet = [SingleArgsTypeAssign] },
		{ SingleArgsTypeAssign = args(TypeAssign, ConsArgTypes, _) },
		{ assoc_list__from_corresponding_lists(Args, ConsArgTypes,
			ArgExpTypes) },
		{ find_mismatched_args(ArgExpTypes, [TypeAssign], 1,
			Mismatches) },
		{ Mismatches = [_|_] }
	->
		report_mismatched_args(Mismatches, yes, VarSet, Functor,
			Context)
	;

		{ conv_args_type_assign_set(ArgsTypeAssignSet,
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
			write_type_of_var(TypeCheckInfo, Context,
				TypeAssignSet, Var),
			io__write_string(",\n")
		;
			[]
		),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_functor_name(Functor, Arity),
		write_type_of_functor(Functor, Arity, Context, ConsDefnList),

		write_types_of_vars(Args, VarSet, Context, TypeCheckInfo, 
			TypeAssignSet),

		write_type_assign_set_msg(TypeAssignSet, VarSet)
	).

:- type mismatch_info
	--->	mismatch(
			int,		% argument number, starting from 1
			prog_var,	% variable in that position
			type,		% actual type of that variable
			type,		% expected type of that variable
			tvarset		% the type vars in the expected
					% and expected types
		).

:- pred find_mismatched_args(assoc_list(prog_var, type), type_assign_set, int,
				list(mismatch_info)).
:- mode find_mismatched_args(in, in, in, out) is semidet.

find_mismatched_args([], _, _, []).
find_mismatched_args([Arg - ExpType | ArgExpTypes], TypeAssignSet, ArgNum0,
		Mismatched) :-
	ArgNum1 is ArgNum0 + 1,
	find_mismatched_args(ArgExpTypes, TypeAssignSet, ArgNum1, Mismatched1),
	get_type_stuff(TypeAssignSet, Arg, TypeStuffList),
	TypeStuffList = [type_stuff(ArgType, TVarSet, TypeBindings)],
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
		Mismatched = Mismatched1
	;
		Mismatched = [mismatch(ArgNum0, Arg, FullArgType, FullExpType,
				TVarSet) | Mismatched1]
	).

:- pred report_mismatched_args(list(mismatch_info), bool, prog_varset, cons_id,
		prog_context, io__state, io__state).
:- mode report_mismatched_args(in, in, in, in, in, di, uo) is det.

report_mismatched_args([], _, _, _, _) --> [].
report_mismatched_args([Mismatch | Mismatches], First, VarSet, Functor,
		Context) -->
	{ Mismatch = mismatch(ArgNum, Var, ActType, ExpType, TVarSet) },
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
	mercury_output_term(ActType, TVarSet, no),
	io__write_string("',\n"),
	prog_out__write_context(Context),
	io__write_string("  expected type was `"),
	mercury_output_term(ExpType, TVarSet, no),
	( { Mismatches = [] } ->
		io__write_string("'.\n")
	;
		io__write_string("';\n"),
		report_mismatched_args(Mismatches, no, VarSet, Functor, Context)
	).

:- pred write_types_of_vars(list(prog_var), prog_varset, prog_context,
		typecheck_info, type_assign_set, io__state, io__state).
:- mode write_types_of_vars(in, in, in, typecheck_info_ui, in, di, uo) is det.

write_types_of_vars([], _, _, _, _) -->
	io__write_string(".\n").
write_types_of_vars([Var | Vars], VarSet, Context, TypeCheckInfo, 
			TypeAssignSet) -->
	io__write_string(",\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	write_argument_name(VarSet, Var),
	write_type_of_var(TypeCheckInfo, Context, TypeAssignSet, Var),
	write_types_of_vars(Vars, VarSet, Context, TypeCheckInfo,
		TypeAssignSet).

:- pred write_argument_name(prog_varset, prog_var, io__state, io__state).
:- mode write_argument_name(in, in, di, uo) is det.

write_argument_name(VarSet, VarId) -->
	( { varset__search_name(VarSet, VarId, _) } ->
		io__write_string("variable `"),
		mercury_output_var(VarId, VarSet, no),
		io__write_string("'")
	;
		io__write_string("argument")
	).

:- pred write_functor_name(cons_id, int, io__state, io__state).
:- mode write_functor_name(in, in, di, uo) is det.

write_functor_name(Functor, Arity) -->
	{ strip_builtin_qualifier_from_cons_id(Functor, Functor1) },
	( { Arity = 0 } ->
		io__write_string("constant `"),
		( { Functor = cons(Name, _) } ->
			prog_out__write_sym_name(Name)
		;
			hlds_out__write_cons_id(Functor1)
		),
		io__write_string("'")
	; { Functor = cons(unqualified(""), _) } ->
		io__write_string("higher-order term (with arity "),
		io__write_int(Arity - 1),
		io__write_string(")")
	;
		io__write_string("functor `"),
		hlds_out__write_cons_id(Functor1),
		io__write_string("'")
	).

:- pred write_type_of_var(typecheck_info, prog_context, type_assign_set,
	prog_var, io__state, io__state).
:- mode write_type_of_var(typecheck_info_no_io, in, in, in, di, uo) is det.

write_type_of_var(_TypeCheckInfo, Context, TypeAssignSet, Var) -->
	{ get_type_stuff(TypeAssignSet, Var, TypeStuffList) },
	{ TypeStrs0 = list__map(typestuff_to_typestr, TypeStuffList) },
	{ list__sort_and_remove_dups(TypeStrs0, TypeStrs) },
	( { TypeStrs = [TypeStr] } ->
		io__write_string(" has type `"),
		io__write_string(TypeStr),
		io__write_string("'")
	;
		io__write_string(" has overloaded type {\n"),
		write_types_list(Context, TypeStrs),
		prog_out__write_context(Context),
		io__write_string("  }")
	).

:- pred write_type_of_functor(cons_id, int, prog_context, list(cons_type_info),
				io__state, io__state).
:- mode write_type_of_functor(in, in, in, in, di, uo) is det.

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

:- pred write_cons_type(cons_type_info, cons_id, prog_context,
			io__state, io__state).
:- mode write_cons_type(in, in, in, di, uo) is det.

	% XXX Should we mention the context here?
write_cons_type(cons_type_info(TVarSet, _ExistQVars, ConsType0, ArgTypes0, _), 
		Functor, _) -->
	{ strip_builtin_qualifier_from_cons_id(Functor, Functor1) },
	{ strip_builtin_qualifiers_from_type_list(ArgTypes0, ArgTypes) },
	( { ArgTypes \= [] } ->
		( { cons_id_and_args_to_term(Functor1, ArgTypes, Term) } ->
			mercury_output_term(Term, TVarSet, no)
		;
			{ error("typecheck:write_cons_type - invalid cons_id") }
		),	
		io__write_string(" :: ")
	;
		[]
	),
	{ strip_builtin_qualifiers_from_type(ConsType0, ConsType) },
	mercury_output_term(ConsType, TVarSet, no).

:- pred write_cons_type_list(list(cons_type_info), cons_id, int, prog_context,
				io__state, io__state).
:- mode write_cons_type_list(in, in, in, in, di, uo) is det.

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

:- pred write_type_assign_set_msg(type_assign_set, prog_varset,
				io__state, io__state).
:- mode write_type_assign_set_msg(in, in, di, uo) is det.

write_type_assign_set_msg(TypeAssignSet, VarSet) -->
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		( { TypeAssignSet = [_] } ->
		    io__write_string("\tThe partial type assignment was:\n")
		;
		    io__write_string(
			"\tThe possible partial type assignments were:\n")
		),
		write_type_assign_set(TypeAssignSet, VarSet)
	;
		[]
	).

:- pred write_args_type_assign_set_msg(args_type_assign_set, prog_varset,
				io__state, io__state).
:- mode write_args_type_assign_set_msg(in, in, di, uo) is det.

write_args_type_assign_set_msg(ArgTypeAssignSet, VarSet) -->
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		( { ArgTypeAssignSet = [_] } ->
		    io__write_string("\tThe partial type assignment was:\n")
		;
		    io__write_string(
			"\tThe possible partial type assignments were:\n")
		),
		write_args_type_assign_set(ArgTypeAssignSet, VarSet)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred write_type_assign_set(type_assign_set, prog_varset,
		io__state, io__state).
:- mode write_type_assign_set(in, in, di, uo) is det.

write_type_assign_set([], _) --> [].
write_type_assign_set([TypeAssign | TypeAssigns], VarSet) -->
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_type_assign_set(TypeAssigns, VarSet).

:- pred write_args_type_assign_set(args_type_assign_set, prog_varset,
				io__state, io__state).
:- mode write_args_type_assign_set(in, in, di, uo) is det.

write_args_type_assign_set([], _) --> [].
write_args_type_assign_set([args(TypeAssign, _ArgTypes, _Cnstrs)| TypeAssigns], 
		VarSet) -->
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_args_type_assign_set(TypeAssigns, VarSet).

:- pred write_type_assign(type_assign, prog_varset, io__state, io__state).
:- mode write_type_assign(in, in, di, uo) is det.

write_type_assign(TypeAssign, VarSet) -->
	{
	  type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
	  type_assign_get_var_types(TypeAssign, VarTypes),
	  type_assign_get_typeclass_constraints(TypeAssign, Constraints),
	  type_assign_get_type_bindings(TypeAssign, TypeBindings),
	  type_assign_get_typevarset(TypeAssign, TypeVarSet),
	  map__keys(VarTypes, Vars)
	},
	( { HeadTypeParams = [] } ->
		[]
	;
		io__write_string("some ["),
		mercury_output_vars(HeadTypeParams, TypeVarSet, no),
		io__write_string("]\n\t")
	),
	write_type_assign_types(Vars, VarSet, VarTypes,
			TypeBindings, TypeVarSet, no),
	write_type_assign_constraints(Constraints, TypeBindings, TypeVarSet),
	io__write_string("\n").

:- pred write_type_assign_types(list(prog_var), prog_varset,
		map(prog_var, type), tsubst, tvarset, bool,
		io__state, io__state).
:- mode write_type_assign_types(in, in, in, in, in, in, di, uo) is det.

write_type_assign_types([], _, _, _, _, FoundOne) -->
	( { FoundOne = no } ->
		io__write_string("(No variables were assigned a type)")
	;
		[]
	).

write_type_assign_types([Var | Vars], VarSet, VarTypes, TypeBindings,
				TypeVarSet, FoundOne) -->
	( 
		{ map__search(VarTypes, Var, Type) }
	->
		( { FoundOne = yes } ->
			io__write_string("\n\t")
		;
			[]
		),
		mercury_output_var(Var, VarSet, no),
		io__write_string(" :: "),
		write_type_b(Type, TypeVarSet, TypeBindings),
		write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, yes)
	;
		write_type_assign_types(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, FoundOne)
	).

:- pred write_type_assign_constraints(class_constraints,
			tsubst, tvarset, io__state, io__state).
:- mode write_type_assign_constraints(in, in, in, di, uo) is det.

write_type_assign_constraints(Constraints, TypeBindings, TypeVarSet) -->
	{ Constraints = constraints(ConstraintsToProve, AssumedConstraints) },
	write_type_assign_constraints("&", AssumedConstraints,
		TypeBindings, TypeVarSet, no),
	write_type_assign_constraints("<=", ConstraintsToProve,
		TypeBindings, TypeVarSet, no).

:- pred write_type_assign_constraints(string, list(class_constraint),
			tsubst, tvarset, bool, io__state, io__state).
:- mode write_type_assign_constraints(in, in, in, in, in, di, uo) is det.

write_type_assign_constraints(_, [], _, _, _) --> [].
write_type_assign_constraints(Operator, [Constraint | Constraints],
			TypeBindings, TypeVarSet, FoundOne) -->
	( { FoundOne = no } ->
		io__write_strings(["\n\t", Operator, " "])
	;
		io__write_string(",\n\t   ")
	),
	{ apply_rec_subst_to_constraint(TypeBindings, Constraint,
		BoundConstraint) },
	{ AppendVarNums = no },
	mercury_output_constraint(TypeVarSet, AppendVarNums, BoundConstraint),
	write_type_assign_constraints(Operator, Constraints,
		TypeBindings, TypeVarSet, yes).

	% write_type_b writes out a type after applying the type bindings.

:- pred write_type_b(type, tvarset, tsubst, io__state, io__state).
:- mode write_type_b(in, in, in, di, uo) is det.

write_type_b(Type, TypeVarSet, TypeBindings) -->
	{ term__apply_rec_substitution(Type, TypeBindings, Type2) },
	{ strip_builtin_qualifiers_from_type(Type2, Type3) },
	mercury_output_term(Type3, TypeVarSet, no).

:- func typestuff_to_typestr(type_stuff) = string.

typestuff_to_typestr(TypeStuff) = TypeStr :-
	TypeStuff = type_stuff(Type0, TypeVarSet, TypeBindings),
	term__apply_rec_substitution(Type0, TypeBindings, Type1),
	strip_builtin_qualifiers_from_type(Type1, Type),
	TypeStr = mercury_term_to_string(Type, TypeVarSet, no).

%-----------------------------------------------------------------------------%

:- pred report_error_var(typecheck_info, prog_var, type, type_assign_set,
			io__state, io__state).
:- mode report_error_var(typecheck_info_no_io, in, in, in, di, uo) is det.

report_error_var(TypeCheckInfo, VarId, Type, TypeAssignSet0) -->
	{ typecheck_info_get_pred_markers(TypeCheckInfo, PredMarkers) },
	{ typecheck_info_get_called_predid(TypeCheckInfo, CalledPredId) },
	{ typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	{ get_type_stuff(TypeAssignSet0, VarId, TypeStuffList) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	write_context_and_pred_id(TypeCheckInfo),
	write_call_context(Context, PredMarkers,
		CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	( { TypeStuffList = [SingleTypeStuff] } ->
		write_argument_name(VarSet, VarId),
		{ SingleTypeStuff = type_stuff(VType, TVarSet, TBinding) },
		io__write_string(" has type `"),
		write_type_b(VType, TVarSet, TBinding),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		write_type_b(Type, TVarSet, TBinding),
		io__write_string("'.\n")
	;
		io__write_string("type of "),
		write_argument_name(VarSet, VarId),
		io__write_string(" does not match its expected type;\n"),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_argument_name(VarSet, VarId),
		io__write_string(" has overloaded actual/expected types {\n"),

		prog_out__write_context(Context),
		io__write_string("    "),
		write_var_type_stuff_list(TypeStuffList, Type),
		io__write_string("\n"),

		prog_out__write_context(Context),
		io__write_string("  }.\n")
	),
	write_type_assign_set_msg(TypeAssignSet0, VarSet).

:- pred report_error_arg_var(typecheck_info, prog_var, args_type_assign_set,
			io__state, io__state).
:- mode report_error_arg_var(typecheck_info_no_io, in, in, di, uo) is det.

report_error_arg_var(TypeCheckInfo, VarId, ArgTypeAssignSet0) -->
	{ typecheck_info_get_pred_markers(TypeCheckInfo, PredMarkers) },
	{ typecheck_info_get_called_predid(TypeCheckInfo, CalledPredId) },
	{ typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	{ get_arg_type_stuff(ArgTypeAssignSet0, VarId, ArgTypeStuffList) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	write_context_and_pred_id(TypeCheckInfo),
	write_call_context(Context, PredMarkers,
		CalledPredId, ArgNum, UnifyContext),
	prog_out__write_context(Context),
	io__write_string("  type error: "),
	( { ArgTypeStuffList = [SingleArgTypeStuff] } ->
		write_argument_name(VarSet, VarId),
		{ SingleArgTypeStuff = arg_type_stuff(Type0, VType0, TVarSet) },
		io__write_string(" has type `"),
		{ strip_builtin_qualifiers_from_type(VType0, VType) },
		mercury_output_term(VType, TVarSet, no),
		io__write_string("',\n"),
		prog_out__write_context(Context),
		io__write_string("  expected type was `"),
		{ strip_builtin_qualifiers_from_type(Type0, Type) },
		mercury_output_term(Type, TVarSet, no),
		io__write_string("'.\n")
	;
		io__write_string("type of "),
		write_argument_name(VarSet, VarId),
		io__write_string(" does not match its expected type;\n"),

		prog_out__write_context(Context),
		io__write_string("  "),
		write_argument_name(VarSet, VarId),
		io__write_string(" has overloaded actual/expected types {\n"),

		prog_out__write_context(Context),
		io__write_string("    "),
		write_arg_type_stuff_list(ArgTypeStuffList),
		io__write_string("\n"),

		prog_out__write_context(Context),
		io__write_string("  }.\n")
	),
	write_args_type_assign_set_msg(ArgTypeAssignSet0, VarSet).

:- pred write_types_list(prog_context::in, list(string)::in,
	io__state::di, io__state::uo) is det.

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

:- pred write_type_stuff(type_stuff, io__state, io__state).
:- mode write_type_stuff(in, di, uo) is det.

write_type_stuff(type_stuff(T, TVarSet, TBinding)) -->
	write_type_b(T, TVarSet, TBinding).

:- pred write_var_type_stuff_list(list(type_stuff), type, io__state, io__state).
:- mode write_var_type_stuff_list(in, in, di, uo) is det.

write_var_type_stuff_list(Ts, T) -->
	io__write_list(Ts, ", ", write_var_type_stuff(T)).

:- pred write_var_type_stuff(type, type_stuff, io__state, io__state).
:- mode write_var_type_stuff(in, in, di, uo) is det.
	
write_var_type_stuff(T, type_stuff(VT, TVarSet, TBinding)) -->
	write_type_b(VT, TVarSet, TBinding),
	io__write_string("/"),
	write_type_b(T, TVarSet, TBinding).

:- pred write_arg_type_stuff_list(list(arg_type_stuff), io__state, io__state).
:- mode write_arg_type_stuff_list(in, di, uo) is det.

write_arg_type_stuff_list(Ts) -->
	io__write_list(Ts, ", ", write_arg_type_stuff).

:- pred write_arg_type_stuff(arg_type_stuff, io__state, io__state).
:- mode write_arg_type_stuff(in, di, uo) is det.

write_arg_type_stuff(arg_type_stuff(T0, VT0, TVarSet)) -->
	{ strip_builtin_qualifiers_from_type(VT0, VT) },
	mercury_output_term(VT, TVarSet, no),
	io__write_string("/"),
	{ strip_builtin_qualifiers_from_type(T0, T) },
	mercury_output_term(T, TVarSet, no).

%-----------------------------------------------------------------------------%

:- pred report_error_undef_pred(typecheck_info, simple_call_id, 
			io__state, io__state).
:- mode report_error_undef_pred(typecheck_info_no_io, in, di, uo) is det.

report_error_undef_pred(TypeCheckInfo, PredOrFunc - PredCallId) -->
	{ PredCallId = PredName/Arity },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	write_typecheck_info_context(TypeCheckInfo),
	(
		{ PredName = unqualified("->"), (Arity = 2 ; Arity = 4) }
	->
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
	;
		{ PredName = unqualified("else"), (Arity = 2 ; Arity = 4) }
	->
		io__write_string("  error: unmatched `else'.\n")
	;
		{ PredName = unqualified("if"), (Arity = 2 ; Arity = 4) }
	->
		io__write_string("  error: `if' without `then' or `else'.\n")
	;
		{ PredName = unqualified("then"), (Arity = 2 ; Arity = 4) }
	->
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
	;
		{ PredName = unqualified("apply"), Arity >= 1 }
	->
		report_error_apply_instead_of_pred(TypeCheckInfo)
	;
		{ PredName = unqualified(PurityString), Arity = 1,
		  ( PurityString = "impure" ; PurityString = "semipure" )
		}
	->
		io__write_string("  error: `"),
		io__write_string(PurityString),
		io__write_string("' marker in an inappropriate place.\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
				"  Such markers only belong before predicate calls.\n")
		;
			[]
		)
	;
		{ PredName = unqualified("some"), Arity = 2 }
	->
		io__write_string(
		    "  syntax error in existential quantification: first\n"),
		prog_out__write_context(Context),
		io__write_string(
		    "  argument of `some' should be a list of variables.\n")
	;
		io__write_string("  error: undefined "),
		hlds_out__write_simple_call_id(PredOrFunc - PredCallId),
		( { PredName = qualified(ModQual, _) } ->
			maybe_report_missing_import(TypeCheckInfo, ModQual)
		;
			io__write_string(".\n")
		)
	).

:- pred maybe_report_missing_import(typecheck_info, module_specifier,
		io__state, io__state).
:- mode maybe_report_missing_import(typecheck_info_no_io, in, di, uo) is det.

maybe_report_missing_import(TypeCheckInfo, ModuleQualifier) -->
	{ typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo) },
	(
		% if the module qualifier couldn't match any of the
		% visible modules, then we report that the module
		% has not been imported
		\+ (
			{ visible_module(VisibleModule, ModuleInfo) },
			{ match_sym_name(ModuleQualifier, VisibleModule) }
		)
	->
		io__write_string("\n"),
		{ typecheck_info_get_context(TypeCheckInfo, Context) },
		prog_out__write_context(Context),
		io__write_string("  (the module `"),
		mercury_output_bracketed_sym_name(ModuleQualifier),
		io__write_string("' has not been imported).\n")
	;
		io__write_string(".\n")
	).

:- pred report_error_func_instead_of_pred(typecheck_info, pred_or_func,
			simple_call_id, io__state, io__state).
:- mode report_error_func_instead_of_pred(typecheck_info_no_io, in, in,
			di, uo) is det.

report_error_func_instead_of_pred(TypeCheckInfo, PredOrFunc, PredCallId) -->
	report_error_undef_pred(TypeCheckInfo, PredCallId),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  (There is a *"),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string("* with that name, however."),
	( { PredOrFunc = function } ->
		io__nl,
		prog_out__write_context(Context),
		io__write_string("  Perhaps you forgot to add ` = ...'?)\n")
	;
		io__write_string(")\n")
	).

:- pred report_error_apply_instead_of_pred(typecheck_info, io__state, 
			io__state).
:- mode report_error_apply_instead_of_pred(typecheck_info_no_io, di, uo) is det.

report_error_apply_instead_of_pred(TypeCheckInfo) -->
	io__write_string("  error: the language construct `apply' should\n"),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
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

:- pred report_error_pred_num_args(typecheck_info, simple_call_id,
		list(int), io__state, io__state).
:- mode report_error_pred_num_args(typecheck_info_no_io, in,
		in, di, uo) is det.

report_error_pred_num_args(TypeCheckInfo,
		PredOrFunc - SymName/Arity, Arities) -->
	write_context_and_pred_id(TypeCheckInfo),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  error: "),
	report_error_num_args(yes(PredOrFunc), Arity, Arities),
	io__nl,
	prog_out__write_context(Context),
	io__write_string("  in call to "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" `"),
	prog_out__write_sym_name(SymName),
	io__write_string("'.\n").

:- pred report_error_undef_cons(typecheck_info, list(invalid_field_update),
			cons_id, int, io__state, io__state).
:- mode report_error_undef_cons(typecheck_info_no_io, in,
			in, in, di, uo) is det.

report_error_undef_cons(TypeCheckInfo, InvalidFieldUpdates, Functor, Arity) -->
	{ typecheck_info_get_pred_markers(TypeCheckInfo, PredMarkers) },
	{ typecheck_info_get_called_predid(TypeCheckInfo, CalledPredId) },
	{ typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	write_context_and_pred_id(TypeCheckInfo),
	write_call_context(Context, PredMarkers,
		CalledPredId, ArgNum, UnifyContext),
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
		io__write_string(
		  "  error: invalid use of field selection operator (`^').\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
			  "  This is probably some kind of syntax error.\n"),
			prog_out__write_context(Context),
			io__write_string(
			  "  The field name must be an atom, not a variable or other term.\n")
		;
			[]
		)
	; { Functor = cons(unqualified(":="), 2) } ->
		io__write_string(
		  "  error: invalid use of field update operator (`:=').\n"),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		( { VerboseErrors = yes } ->
			prog_out__write_context(Context),
			io__write_string(
			  "  This is probably some kind of syntax error.\n")
		;
			[]
		)
	; { Functor = cons(unqualified(":-"), 2) } ->
		io__write_string(
		  "  syntax error in lambda expression (`:-').\n")
	; { Functor = cons(unqualified("-->"), 2) } ->
		io__write_string(
		  "  syntax error in DCG lambda expression (`-->').\n")
	; { InvalidFieldUpdates = [_ | _] } ->
		io__write_string(
			"  error: invalid field update `"),
		hlds_out__write_cons_id(Functor),
		io__write_string("':\n"),
		report_invalid_field_updates(InvalidFieldUpdates)
	;
		(
			{ Functor = cons(Constructor, Arity) },
			{ typecheck_info_get_ctors(TypeCheckInfo, ConsTable) },
			{ solutions(lambda([N::out] is nondet, 
				map__member(ConsTable, 
					    cons(Constructor, N),
					    _)),
				ActualArities) },
			{ ActualArities \= [] }
		->
			report_wrong_arity_constructor(Constructor, Arity,
				ActualArities, Context)
		;
			io__write_string("  error: undefined symbol `"),
			{ strip_builtin_qualifier_from_cons_id(Functor, 
				Functor1) },
			hlds_out__write_cons_id(Functor1),
			io__write_string("'"),
			(
				{ Functor = cons(Constructor, _) },
				{ Constructor = qualified(ModQual, _) }
			->
				maybe_report_missing_import(TypeCheckInfo,
					ModQual)
			;
				io__write_string(".\n")
			)
		)
	).

:- pred report_invalid_field_updates(list(invalid_field_update),
			io__state, io__state).
:- mode report_invalid_field_updates(in, di, uo) is det.

report_invalid_field_updates(Updates) -->
	io__write_list(Updates, ", ", report_invalid_field_update).

:- pred report_invalid_field_update(invalid_field_update,
			io__state, io__state).
:- mode report_invalid_field_update(in, di, uo) is det.

report_invalid_field_update(invalid_field_update(FieldName, FieldDefn,
				TVarSet, TVars)) -->
	{ FieldDefn = hlds_ctor_field_defn(Context, _, _, ConsId, _) },
	prog_out__write_context(Context),
	io__write_string("  existentially quantified type "),
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

:- pred report_wrong_arity_constructor(sym_name, arity, list(int), 
		prog_context, io__state, io__state).
:- mode report_wrong_arity_constructor(in, in, in, in, di, uo) is det.

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

:- pred write_call_context(prog_context, pred_markers,
		call_id, int, unify_context, io__state, io__state).
:- mode write_call_context(in, in, in, in, in, di, uo) is det.

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

:- pred write_typecheck_info_context(typecheck_info, io__state, io__state).
:- mode write_typecheck_info_context(typecheck_info_no_io, di, uo) is det.

write_typecheck_info_context(TypeCheckInfo) -->
	write_context_and_pred_id(TypeCheckInfo),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	prog_out__write_context(Context).

:- pred write_context_and_pred_id(typecheck_info, io__state, io__state).
:- mode write_context_and_pred_id(typecheck_info_no_io, di, uo) is det.

write_context_and_pred_id(TypeCheckInfo) -->
	{ typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_predid(TypeCheckInfo, PredId) },
	prog_out__write_context(Context),
	io__write_string("In clause for "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n").

	% This is intended to supercede the above predicate - It performs the
	% same action, but instead of just writing to the output straight away
	% the resultant string is passed back to the caller to deal with.
	% This allows `nicer' handling of error messages, since this string
	% can be used by the predicates in error_util.m
	%
	% The string generated by this predicate is of the form:
	%   "In clause for module:pred/N:"
:- pred make_pred_id_preamble(typecheck_info, string).
:- mode make_pred_id_preamble(typecheck_info_no_io, out) is det.

make_pred_id_preamble(TypeCheckInfo, Preamble) :-
	typecheck_info_get_module_info(TypeCheckInfo, Module),
	typecheck_info_get_predid(TypeCheckInfo, PredID),
	error_util__describe_one_pred_name(Module, PredID, PredName),
	Words = "In clause for ",
	Colon = ":",
	string__append(Words, PredName, Preamble0),
	string__append(Preamble0, Colon, Preamble).
%-----------------------------------------------------------------------------%

:- pred report_ambiguity_error(typecheck_info, type_assign, type_assign,
				io__state, io__state).
:- mode report_ambiguity_error(typecheck_info_no_io, in, in, di, uo) is det.

report_ambiguity_error(TypeCheckInfo, TypeAssign1, TypeAssign2) -->
	write_typecheck_info_context(TypeCheckInfo),
	io__write_string(
		"  error: ambiguous overloading causes type ambiguity.\n"),
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ type_assign_get_var_types(TypeAssign1, VarTypes1) },
	{ map__keys(VarTypes1, Vars1) },
	report_ambiguity_error_2(Vars1, VarSet, TypeCheckInfo,
			TypeAssign1, TypeAssign2, no, Found),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	( { Found = no } ->
		prog_out__write_context(Context),
		io__write_string("  One or more of the predicates or functions called\n"),
		prog_out__write_context(Context),
		io__write_string("  is declared in more than one module.\n"),
		prog_out__write_context(Context),
		io__write_string("  Try adding explicit module qualifiers.\n")
	; { VerboseErrors = yes } ->
		io__write_strings([
"\tYou will need to add an explicit type qualification to resolve the\n",
"\ttype ambiguity.\n",
"\tThe way to add an explicit type qualification is to use ""with_type"".\n",
"\tFor details see the ""Explicit type qualification"" sub-section\n",
"\tof the ""Data-terms"" section of the ""Syntax"" chapter\n",
"\tof the Mercury langauge reference manual.\n"
		])
	;
		[]
	).

:- pred report_ambiguity_error_2(list(prog_var), prog_varset, typecheck_info,
		type_assign, type_assign, bool, bool, io__state, io__state).
:- mode report_ambiguity_error_2(in, in, typecheck_info_no_io, in, in, in, out,
				di, uo) is det.

report_ambiguity_error_2([], _VarSet, _, _TypeAssign1, _TypeAssign2,
		Found, Found) --> [].
report_ambiguity_error_2([V | Vs], VarSet, TypeCheckInfo, TypeAssign1,
		TypeAssign2, Found0, Found) -->
	{ type_assign_get_var_types(TypeAssign1, VarTypes1) },
	{ type_assign_get_var_types(TypeAssign2, VarTypes2) },
	{ type_assign_get_type_bindings(TypeAssign1, TypeBindings1) },
	{ type_assign_get_type_bindings(TypeAssign2, TypeBindings2) },
	( {
		map__search(VarTypes1, V, Type1),
		map__search(VarTypes2, V, Type2),
		term__apply_rec_substitution(Type1, TypeBindings1, T1a),
		term__apply_rec_substitution(Type2, TypeBindings2, T2a),
		\+ identical_types(T1a, T2a)
	} ->
		{ typecheck_info_get_context(TypeCheckInfo, Context) },
		( { Found0 = no } ->
			prog_out__write_context(Context),
			io__write_string(
				"  Possible type assignments include:\n")
		;
			[]
		),
		{ Found1 = yes },
		prog_out__write_context(Context),
		mercury_output_var(V, VarSet, no),
		io__write_string(" :: "),
		{ type_assign_get_typevarset(TypeAssign1, TVarSet1) },
		{ strip_builtin_qualifiers_from_type(T1a, T1) },
		mercury_output_term(T1, TVarSet1, no),
		io__write_string(" or "),
		{ type_assign_get_typevarset(TypeAssign2, TVarSet2) },
		{ strip_builtin_qualifiers_from_type(T2a, T2) },
		mercury_output_term(T2, TVarSet2, no),
		io__write_string("\n")
	;
		{ Found1 = Found0 }
	),
	report_ambiguity_error_2(Vs, VarSet, TypeCheckInfo,
			TypeAssign1, TypeAssign2, Found1, Found).

	% Check whether two types are identical ignoring their
	% prog_contexts, i.e. whether they can be unified without
	% binding any type parameters.

:- pred identical_types(type, type).
:- mode identical_types(in, in) is semidet.

identical_types(Type1, Type2) :-
	map__init(TypeSubst0),
	type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
	TypeSubst = TypeSubst0.

	% Check whether two lists of types are identical up to renaming.

:- pred identical_up_to_renaming(list(type), list(type)).
:- mode identical_up_to_renaming(in, in) is semidet.

identical_up_to_renaming(TypesList1, TypesList2) :-
	% They are identical up to renaming if they each subsume each other.
	type_list_subsumes(TypesList1, TypesList2, _),
	type_list_subsumes(TypesList2, TypesList1, _).


	% Make error messages more readable by removing "builtin:"
	% qualifiers.

:- pred strip_builtin_qualifiers_from_type((type), (type)).
:- mode strip_builtin_qualifiers_from_type(in, out) is det.

strip_builtin_qualifiers_from_type(Type0, Type) :-
	(
		type_to_type_id(Type0, TypeId0, Args0)
	->
		strip_builtin_qualifiers_from_type_list(Args0, Args),
		TypeId0 = SymName0 - Arity,
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

:- pred strip_builtin_qualifiers_from_type_list(list(type), list(type)).
:- mode strip_builtin_qualifiers_from_type_list(in, out) is det.

strip_builtin_qualifiers_from_type_list(Types0, Types) :-
	list__map(strip_builtin_qualifiers_from_type, Types0, Types).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
