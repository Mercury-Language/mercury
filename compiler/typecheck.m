%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
%		  namely the pred_id and term__context of the clause
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
% XXX	Type inference loops for some type-incorrect programs, e.g.
%		p([X]) :- p(X).
%
% XXX	Type inference doesn't handle ambiguity as well as it could do.
%	We should do a topological sort, and then typecheck it all
%	bottom-up.  If we infer an ambiguous type for a pred, we should
%	not reject it immediately; instead we should give it an overloaded
%	type, and keep going.  When we've finished type inference, we should
%	then delete unused overloadings, and only then should we report
%	ambiguity errors, if any overloading still remains.
%
% XXX	There seems to be a performance bug with the handling of
%	typevarsets; during type inference, the typevarsets get bigger
%	and bigger.
%
% Wish list:
%
%	we should handle equivalence types here
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module typecheck.

:- interface.

:- import_module hlds_module, hlds_pred.
:- import_module bool, io.

:- pred typecheck(module_info, module_info, bool, bool, io__state, io__state).
:- mode typecheck(in, out, in, out, di, uo) is det.

/*
	Formally, typecheck(Module0, Module, ModeError, FoundError, IO0, IO) is
	intended to be true iff Module is Module0 annotated with the
	variable typings that result from the process of type-checking,
	FoundError is `yes' if Module0 contains any type errors and `no'
	otherwise, and IO is the io__state that results from IO0 after
	printing out appropriate error messages for the type errors in
	Module0, if any. ModeError should be true if any undefined modes	
	were found by previous passes.

	Informally, typecheck(Module0, Module, ModeError, FoundError, IO0, IO) 
	type-checks Module0 and annotates it with variable typings
	(returning the result in Module), prints out appropriate error
	messages, and sets FoundError to `yes' if it finds any errors
	and `no' otherwise.

	Typecheck also copies the clause_info structure it annotates
	to the proc structures. This is done at the end of typecheck
	and not at the start of modecheck because modecheck may be
	reinvoked after HLDS transformations. Any transformation that
	needs typechecking should work with the clause_info structure.
	Type information is also propagated into the modes of procedures
	by this pass if the ModeError parameter is no. 
*/


	% Find a predicate which matches the given name and argument types.
	% Abort if there is no matching pred.
	% Abort if there are multiple matching preds.

:- pred typecheck__resolve_pred_overloading(module_info, list(var),
			map(var, type), tvarset, sym_name, sym_name, pred_id).
:- mode typecheck__resolve_pred_overloading(in, in, in, in,
			in, out, out) is det.

	% Find a predicate or function from the list of pred_ids
	% which matches the given name and argument types.
	% Fail if there is no matching pred.
	% Abort if there are multiple matching preds.

:- pred typecheck__find_matching_pred_id(list(pred_id), module_info,
			tvarset, list(type), pred_id, sym_name).
:- mode typecheck__find_matching_pred_id(in, in, in, in, out, out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, hlds_data, prog_util, type_util, code_util.
:- import_module prog_data, prog_io, prog_io_util, prog_out, hlds_out.
:- import_module mercury_to_mercury, mode_util, options, getopt, globals.
:- import_module passes_aux, clause_to_proc.

:- import_module int, list, map, set, string, require, std_util, tree234.
:- import_module assoc_list, varset, term, term_io.

%-----------------------------------------------------------------------------%

	% XXX need to pass FoundError to all steps

typecheck(Module0, Module, ModeError, FoundError) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_bool_option(verbose, Verbose),
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),

	maybe_write_string(Verbose, "% Type-checking clauses...\n"),
	check_pred_types(Module0, Module, ModeError, FoundError),
	maybe_report_stats(Statistics),

	io__set_output_stream(OldStream, _).

%-----------------------------------------------------------------------------%

	% Type-check the code for all the predicates in a module.

:- pred check_pred_types(module_info, module_info, bool, bool,
		io__state, io__state).
:- mode check_pred_types(in, out, in, out, di, uo) is det.

check_pred_types(Module0, Module, ModeError, FoundError) -->
	{ module_info_predids(Module0, PredIds) },
	typecheck_to_fixpoint(PredIds, Module0, Module, ModeError, FoundError),
	write_inference_messages(PredIds, Module).

	% Repeatedly typecheck the code for a group of predicates
	% until a fixpoint is reached, or until some errors are detected.

:- pred typecheck_to_fixpoint(list(pred_id), module_info, module_info, 
		bool, bool, io__state, io__state).
:- mode typecheck_to_fixpoint(in, in, out, in, out, di, uo) is det.

typecheck_to_fixpoint(PredIds, Module0, Module, ModeError, FoundError) -->
	typecheck_pred_types_2(PredIds, Module0, Module1,
		ModeError, no, FoundError1, no, Changed),
	( { Changed = no ; FoundError1 = yes } ->
		{ Module = Module1 },
		{ FoundError = FoundError1 }
	;
		globals__io_lookup_bool_option(debug_types, DebugTypes),
		( { DebugTypes = yes } ->
			write_inference_messages(PredIds, Module1)
		;
			[]
		),
		typecheck_to_fixpoint(PredIds, Module1, Module,
			ModeError, FoundError)
	).

%-----------------------------------------------------------------------------%

	% Iterate over the list of pred_ids in a module.

:- pred typecheck_pred_types_2(list(pred_id), module_info, module_info,
	bool, bool, bool, bool, bool, io__state, io__state).
:- mode typecheck_pred_types_2(in, in, out,
	in, in, out, in, out, di, uo) is det.

typecheck_pred_types_2([], ModuleInfo, ModuleInfo, 
		_, Error, Error, Changed, Changed) --> [].
typecheck_pred_types_2([PredId | PredIds], ModuleInfo0, ModuleInfo, 
		ModeError, Error0, Error, Changed0, Changed) -->
	{ module_info_preds(ModuleInfo0, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	{ pred_info_procids(PredInfo0, ProcIds) },
	(
		{ pred_info_is_imported(PredInfo0) }
	->
		{ Error1 = Error0 },

		% 
		% Ensure that all constructors occurring in predicate mode
		% declarations are module qualified.
		% 
		{
		pred_info_arg_types(PredInfo0, _, ArgTypes),
		pred_info_procedures(PredInfo0, Procs0),
		typecheck_propagate_types_into_proc_modes(
		    ModuleInfo0, ProcIds, ArgTypes, Procs0, Procs),
		pred_info_set_procedures(PredInfo0, Procs, PredInfo),
		map__set(Preds0, PredId, PredInfo, Preds),
		module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1),
		Changed2 = Changed0
		}
	;
		typecheck_pred_type(PredId, PredInfo0, ModuleInfo0, 
			ModeError, MaybePredInfo, Changed1),
		{
			MaybePredInfo = yes(PredInfo),
			Error1 = Error0,
			map__det_update(Preds0, PredId, PredInfo, Preds),
			module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1)
		;
			MaybePredInfo = no,
			Error1 = yes,
			module_info_remove_predid(ModuleInfo0, PredId,
				ModuleInfo1)
		},
		{ bool__or(Changed0, Changed1, Changed2) }
	),
	typecheck_pred_types_2(PredIds, ModuleInfo1, ModuleInfo, 
		ModeError, Error1, Error, Changed2, Changed).

:- pred typecheck_pred_type(pred_id, pred_info, module_info, bool,
	maybe(pred_info), bool, io__state, io__state).
:- mode typecheck_pred_type(in, in, in, in, out, out, di, uo) is det.

typecheck_pred_type(PredId, PredInfo0, ModuleInfo, ModeError, 
		MaybePredInfo, Changed, IOState0, IOState) :-
	typecheck_pred_type_2(PredId, PredInfo0, ModuleInfo, MaybePredInfo0,
		Changed, IOState0, IOState),
	(
		MaybePredInfo0 = no,
		MaybePredInfo = no
	;
		MaybePredInfo0 = yes(PredInfo1),

		( ModeError = no ->
			% 
			% Copy clauses to procs, then ensure that all 
			% constructors occurring in predicate mode 
			% declarations are module qualified, unless undefined
			% modes were found by an earlier pass.
			% 
			maybe_add_default_mode(PredInfo1, PredInfo2),
			copy_clauses_to_procs(PredInfo2, PredInfo3),
			pred_info_arg_types(PredInfo3, _, ArgTypes),
			pred_info_procedures(PredInfo3, Procs1),
			pred_info_procids(PredInfo3, ProcIds),
			typecheck_propagate_types_into_proc_modes(
				ModuleInfo, ProcIds, ArgTypes, Procs1, Procs),
			pred_info_set_procedures(PredInfo3, Procs, PredInfo)
		;
			PredInfo = PredInfo1
		),
		MaybePredInfo = yes(PredInfo)
	).

:- pred typecheck_propagate_types_into_proc_modes(module_info,
		list(proc_id), list(type), proc_table, proc_table).
:- mode typecheck_propagate_types_into_proc_modes(in,
		in, in, in, out) is det.		

typecheck_propagate_types_into_proc_modes(_, [], _, Procs, Procs).
typecheck_propagate_types_into_proc_modes(ModuleInfo, [ProcId | ProcIds],
		ArgTypes, Procs0, Procs) :-
	map__lookup(Procs0, ProcId, ProcInfo0),
	proc_info_argmodes(ProcInfo0, ArgModes0),
	propagate_types_into_mode_list(ArgTypes, ModuleInfo,
		ArgModes0, ArgModes),
	proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs1),
	typecheck_propagate_types_into_proc_modes(ModuleInfo, ProcIds,
		ArgTypes, Procs1, Procs).

:- pred typecheck_pred_type_2(pred_id, pred_info, module_info,
	maybe(pred_info), bool, io__state, io__state).
:- mode typecheck_pred_type_2(in, in, in, out, out, di, uo) is det.

typecheck_pred_type_2(PredId, PredInfo0, ModuleInfo, MaybePredInfo, Changed,
		IOState0, IOState) :-
	(
	    % Compiler-generated predicates are created already type-correct,
	    % there's no need to typecheck them.  Same for builtins.
	    ( code_util__compiler_generated(PredInfo0)
	    ; code_util__predinfo_is_builtin(PredInfo0)
	    )
	->
	    pred_info_clauses_info(PredInfo0, ClausesInfo0),
	    ClausesInfo0 = clauses_info(_, _, _, _, Clauses0),
	    ( Clauses0 = [] ->
		pred_info_mark_as_external(PredInfo0, PredInfo)
	    ;
	        PredInfo = PredInfo0
	    ),
	    MaybePredInfo = yes(PredInfo),
	    Changed = no,
	    IOState = IOState0
	;
	    pred_info_arg_types(PredInfo0, ArgTypeVarSet, ArgTypes0),
	    pred_info_typevarset(PredInfo0, TypeVarSet0),
	    pred_info_clauses_info(PredInfo0, ClausesInfo0),
	    pred_info_import_status(PredInfo0, Status),
	    ClausesInfo0 = clauses_info(VarSet, ExplicitVarTypes,
				_OldInferredVarTypes, HeadVars, Clauses0),
	    ( 
		Clauses0 = [] 
	    ->
	        report_error_no_clauses(PredId, PredInfo0, ModuleInfo,
		    IOState0, IOState),
	        MaybePredInfo = no,
		Changed = no
	    ;
		pred_info_get_marker_list(PredInfo0, Markers),
		( list__member(request(infer_type), Markers) ->
			% For a predicate whose type is inferred,
			% the predicate is allowed to bind the type
			% variables in the head of the predicate's
			% type declaration.  Such predicates are given an
			% initial type declaration of 
			% `pred foo(T1, T2, ..., TN)' by make_hlds.m.
			Inferring = yes,
			HeadTypeParams = [],
			write_pred_progress_message("% Inferring type of ",
				PredId, ModuleInfo, IOState0, IOState1)
		;
			Inferring = no,
			varset__vars(ArgTypeVarSet, HeadTypeParams),
			write_pred_progress_message("% Type-checking ",
				PredId, ModuleInfo, IOState0, IOState1)
		),
		bool(Inferring), % dummy pred call to avoid type ambiguity

		typecheck_info_init(IOState1, ModuleInfo, PredId,
				TypeVarSet0, VarSet, ExplicitVarTypes,
				HeadTypeParams, Status, TypeCheckInfo1),
		typecheck_clause_list(Clauses0, HeadVars, ArgTypes0, Clauses,
				TypeCheckInfo1, TypeCheckInfo2),
		typecheck_check_for_ambiguity(whole_pred, HeadVars,
				TypeCheckInfo2, TypeCheckInfo3),
		typecheck_info_get_final_info(TypeCheckInfo3, TypeVarSet, 
				InferredVarTypes0),
		map__optimize(InferredVarTypes0, InferredVarTypes),
		ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
				InferredVarTypes, HeadVars, Clauses),
		pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo1),
		pred_info_set_typevarset(PredInfo1, TypeVarSet, PredInfo2),
		( Inferring = no ->
			PredInfo = PredInfo2,
			Changed = no
		;
			map__apply_to_list(HeadVars, InferredVarTypes,
				ArgTypes),
			pred_info_set_arg_types(PredInfo2, TypeVarSet,
				ArgTypes, PredInfo),
			( identical_up_to_renaming(ArgTypes0, ArgTypes) ->
				Changed = no
			;
				Changed = yes
			)
		),
		typecheck_info_get_found_error(TypeCheckInfo2, Error),
		(
			Error = yes,
			MaybePredInfo = no
		;
			Error = no,
			MaybePredInfo = yes(PredInfo)
		),
		typecheck_info_get_io_state(TypeCheckInfo2, IOState)
	    )
	).

	% bool/1 is used to avoid a type ambiguity
:- pred bool(bool::in) is det.
bool(_).

%-----------------------------------------------------------------------------%

	% Iterate over the list of clauses for a predicate.

:- pred typecheck_clause_list(list(clause), list(var), list(type), list(clause),
				typecheck_info, typecheck_info).
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

:- pred typecheck_clause(clause, list(var), list(type), clause,
			typecheck_info, typecheck_info).
:- mode typecheck_clause(in, in, in, out, typecheck_info_di, typecheck_info_uo)
			is det.

typecheck_clause(Clause0, HeadVars, ArgTypes, Clause) -->
		% XXX abstract clause/3
	{ Clause0 = clause(Modes, Body0, Context) },
	typecheck_info_set_context(Context),
		% typecheck the clause - first the head unification, and
		% then the body
	typecheck_var_has_type_list(HeadVars, ArgTypes, 0),
	typecheck_goal(Body0, Body),
	{ Clause = clause(Modes, Body, Context) },
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

:- pred typecheck_check_for_ambiguity(stuff_to_check, list(var),
				typecheck_info, typecheck_info).
:- mode typecheck_check_for_ambiguity(in, in,
				typecheck_info_di, typecheck_info_uo) is det.

typecheck_check_for_ambiguity(StuffToCheck, HeadVars,
		TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet),
	( TypeAssignSet = [TypeAssign] ->
		typecheck_info_get_found_error(TypeCheckInfo0, FoundError),
		(
			StuffToCheck = whole_pred,
			FoundError = no
		->
			check_type_bindings(TypeAssign, HeadVars,
				TypeCheckInfo0, TypeCheckInfo)
		;
			TypeCheckInfo = TypeCheckInfo0
		)
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
			    term__apply_rec_substitution_to_list(HeadTypes1,
					TypeBindings1, FinalHeadTypes1),
			    term__apply_rec_substitution_to_list(HeadTypes2,
					TypeBindings2, FinalHeadTypes2),
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

	% Check that the all of the types which have been inferred
	% for the variables in the clause do not contain any unbound type
	% variables other than those that occur in the types of head
	% variables.

:- pred check_type_bindings(type_assign, list(var),
			typecheck_info, typecheck_info).
:- mode check_type_bindings(in, in,
			typecheck_info_di, typecheck_info_uo) is det.

check_type_bindings(TypeAssign, HeadVars, TypeCheckInfo0, TypeCheckInfo) :-
	type_assign_get_type_bindings(TypeAssign, TypeBindings),
	type_assign_get_var_types(TypeAssign, VarTypesMap),
	map__apply_to_list(HeadVars, VarTypesMap, HeadVarTypes0),
	term__apply_rec_substitution_to_list(HeadVarTypes0, TypeBindings,
		HeadVarTypes),
	term__vars_list(HeadVarTypes, HeadVarTypeParams),
	map__to_assoc_list(VarTypesMap, VarTypesList),
	set__init(Set0),
	check_type_bindings_2(VarTypesList, TypeBindings, HeadVarTypeParams,
		[], Errs, Set0, _Set),
	% ... we could at this point bind all the type variables in `Set'
	% to `void' ...
	( Errs = [] ->
		TypeCheckInfo = TypeCheckInfo0
	;
		type_assign_get_typevarset(TypeAssign, TVarSet),
		report_unresolved_type_error(Errs, TVarSet, TypeCheckInfo0,
				TypeCheckInfo)
	).

:- pred check_type_bindings_2(assoc_list(var, (type)), tsubst, headtypes,
			assoc_list(var, (type)), assoc_list(var, (type)),
			set(tvar), set(tvar)).
:- mode check_type_bindings_2(in, in, in, in, out, in, out) is det.

check_type_bindings_2([], _, _, Errs, Errs, Set, Set).
check_type_bindings_2([Var - Type0 | VarTypes], TypeBindings, HeadTypeParams,
			Errs0, Errs, Set0, Set) :-
	term__apply_rec_substitution(Type0, TypeBindings, Type),
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
	check_type_bindings_2(VarTypes, TypeBindings, HeadTypeParams,
		Errs1, Errs, Set1, Set).

	% report a warning: uninstantiated type parameter

:- pred report_unresolved_type_error(assoc_list(var, (type)), tvarset,
				typecheck_info, typecheck_info).
:- mode report_unresolved_type_error(in, in, typecheck_info_di, 
				typecheck_info_uo) is det.

report_unresolved_type_error(Errs, TVarSet, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
	globals__io_lookup_bool_option(infer_types, Inferring,
		IOState0, IOState1),
	( Inferring = yes ->
		%
		% If type inferences is enabled, it can result in spurious
		% unresolved type warnings in the early passes; the warnings
		% may be spurious because the types may get resolved in later
		% passes.  Unfortunately there's no way to tell which
		% is the last pass until after it is finished... 
		% probably these warnings ought to be issued in a different
		% pass than type checking.
		%
		% For the moment, if type inference is enabled, you just don't
		% get these warnings.
		%
		IOState = IOState1
	;
		report_unresolved_type_error_2(TypeCheckInfo0, Errs, TVarSet,
			IOState1, IOState)
	),
	typecheck_info_set_io_state(TypeCheckInfo0, IOState, TypeCheckInfo).
	% Currently it is just a warning, not an error.
	% typecheck_info_set_found_error(TypeCheckInfo1, yes, TypeCheckInfo).

:- pred report_unresolved_type_error_2(typecheck_info, assoc_list(var, (type)),
					tvarset, io__state, io__state).
:- mode report_unresolved_type_error_2(typecheck_info_no_io, in, in, di, uo) 
					is det.

report_unresolved_type_error_2(TypeCheckInfo, Errs, TVarSet) -->
	write_typecheck_info_context(TypeCheckInfo),
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	io__write_string("  warning: unresolved polymorphism.\n"),
	prog_out__write_context(Context),
	( { Errs = [_] } ->
		io__write_string("  The variable with an unbound type was:\n")
	;
		io__write_string("  The variables with unbound types were:\n")
	),
	write_type_var_list(Errs, Context, VarSet, TVarSet),
	prog_out__write_context(Context),
	io__write_string("  The unbound type variable(s) will be implicitly\n"),
	prog_out__write_context(Context),
	io__write_string("  bound to the builtin type `void'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		io__write_string("\tThe body of the clause contains a call to a polymorphic predicate,\n"),
		io__write_string("\tbut I can't determine which version should be called,\n"),
		io__write_string("\tbecause the type variables listed above didn't get bound.\n"),
			% XXX improve error message
		io__write_string("\t(I ought to tell you which call caused the problem, but I'm afraid\n"),
		io__write_string("\tyou'll have to work it out yourself.  My apologies.)\n")
	;
		[]
	).

:- pred write_type_var_list(assoc_list(var, (type)), term__context,
			varset, tvarset, io__state, io__state).
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
typecheck_goal_2(some(Vs, G0), some(Vs, G)) -->
	checkpoint("some"),
	typecheck_goal(G0, G),
	ensure_vars_have_a_type(Vs).
typecheck_goal_2(call(_, Mode, Args, Builtin, Context, Name),
		call(PredId, Mode, Args, Builtin, Context, Name)) -->
	checkpoint("call"),
	typecheck_call_pred(Name, Args, PredId).
typecheck_goal_2(higher_order_call(PredVar, Args, C, D, E),
		higher_order_call(PredVar, Args, C, D, E)) -->
	checkpoint("higher-order call"),
	typecheck_higher_order_call(PredVar, Args).
typecheck_goal_2(unify(A, B0, Mode, Info, UnifyContext),
		unify(A, B, Mode, Info, UnifyContext)) -->
	checkpoint("unify"),
	typecheck_info_set_arg_num(0),
	typecheck_info_set_unify_context(UnifyContext),
	typecheck_unification(A, B0, B).
typecheck_goal_2(switch(_, _, _, _), _) -->
	{ error("unexpected switch") }.
% no need to typecheck pragmas
typecheck_goal_2(pragma_c_code(A,B,C,D,E,F,G,H),
		pragma_c_code(A,B,C,D,E,F,G,H))
	--> []. 

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

:- pred ensure_vars_have_a_type(list(var), typecheck_info, typecheck_info).
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
		typecheck_var_has_polymorphic_type_list(Vars,
			TypeVarSet, Types)
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_higher_order_call(var, list(var), typecheck_info, 
				typecheck_info).
:- mode typecheck_higher_order_call(in, in, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_higher_order_call(PredVar, Args) -->
	{ list__length(Args, Arity) },
	{ higher_order_pred_type(Arity, TypeVarSet, PredVarType, ArgTypes) },
	{ Arity1 is Arity + 1 },
	{ PredCallId = unqualified("call")/Arity1 },
	typecheck_info_set_called_predid(PredCallId),
	typecheck_var_has_polymorphic_type_list([PredVar|Args], TypeVarSet,
		[PredVarType|ArgTypes]).

:- pred higher_order_pred_type(int, tvarset, type, list(type)).
:- mode higher_order_pred_type(in, out, out, out) is det.

	% higher_order_pred_type(N, TypeVarSet, PredType, ArgTypes):
	% Given an arity N, let TypeVarSet = {T1, T2, ..., TN},
	% PredType = `pred(T1, T2, ..., TN)', and
	% ArgTypes = [T1, T2, ..., TN].

higher_order_pred_type(Arity, TypeVarSet, PredType, ArgTypes) :-
	varset__init(TypeVarSet0),
	varset__new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet),
	term__var_list_to_term_list(ArgTypeVars, ArgTypes),
	term__context_init(Context),
	PredType = term__functor(term__atom("pred"), ArgTypes, Context).

:- pred higher_order_func_type(int, tvarset, type, list(type), type).
:- mode higher_order_func_type(in, out, out, out, out) is det.

	% higher_order_func_type(N, TypeVarSet, FuncType, ArgTypes, RetType):
	% Given an arity N, let TypeVarSet = {T0, T1, T2, ..., TN},
	% FuncType = `func(T1, T2, ..., TN) = T0',
	% ArgTypes = [T1, T2, ..., TN], and
	% RetType = T0.

higher_order_func_type(Arity, TypeVarSet, FuncType, ArgTypes, RetType) :-
	varset__init(TypeVarSet0),
	varset__new_vars(TypeVarSet0, Arity, ArgTypeVars, TypeVarSet1),
	varset__new_var(TypeVarSet1, RetTypeVar, TypeVarSet),
	term__var_list_to_term_list(ArgTypeVars, ArgTypes),
	RetType = term__variable(RetTypeVar),
	term__context_init(Context),
	FuncType = term__functor(term__atom("="),
			[term__functor(term__atom("func"), ArgTypes, Context),
			 RetType],
			Context).

%-----------------------------------------------------------------------------%

:- pred typecheck_call_pred(sym_name, list(var), pred_id, typecheck_info,
				typecheck_info).
:- mode typecheck_call_pred(in, in, out, typecheck_info_di, 
				typecheck_info_uo) is det.

typecheck_call_pred(PredName, Args, PredId, TypeCheckInfo0, TypeCheckInfo) :-
	list__length(Args, Arity),
	PredCallId = PredName/Arity,
	typecheck_info_set_called_predid(PredCallId, TypeCheckInfo0,
		TypeCheckInfo1),

		% look up the called predicate's arg types
	typecheck_info_get_module_info(TypeCheckInfo1, ModuleInfo),
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	( 
		predicate_table_search_pred_sym_arity(PredicateTable,
			PredName, Arity, PredIdList)
	->
		% handle the case of a non-overloaded predicate specially
		% (so that we can optimize the case of a non-overloaded,
		% non-polymorphic predicate)
		( PredIdList = [PredId0] ->
			
			PredId = PredId0,
			predicate_table_get_preds(PredicateTable, Preds),
			map__lookup(Preds, PredId, PredInfo),
			pred_info_arg_types(PredInfo, PredTypeVarSet,
						PredArgTypes),

				% rename apart the type variables in 
				% called predicate's arg types and then
				% unify the types of the call arguments
				% with the called predicates' arg types
				% (optimize for the common case of
				% a non-polymorphic predicate)
			( varset__is_empty(PredTypeVarSet) ->
			    typecheck_var_has_type_list(Args,
				PredArgTypes, 0, TypeCheckInfo1,
				TypeCheckInfo)
			;
			    typecheck_var_has_polymorphic_type_list(
				Args, PredTypeVarSet, PredArgTypes,
				TypeCheckInfo1, TypeCheckInfo)
			)
		;
			typecheck_info_get_pred_import_status(TypeCheckInfo1,
						CallingStatus),
			typecheck_call_overloaded_pred(PredIdList, Args,
				CallingStatus, TypeCheckInfo1, TypeCheckInfo),
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
		)
	;
		invalid_pred_id(PredId),
		report_pred_call_error(TypeCheckInfo1, ModuleInfo,
				PredicateTable, PredCallId, TypeCheckInfo)
	).

:- pred report_pred_call_error(typecheck_info, module_info, predicate_table, 
			pred_call_id, typecheck_info).
:- mode report_pred_call_error(typecheck_info_di, in, in,
			in, typecheck_info_uo) is det.

report_pred_call_error(TypeCheckInfo1, _ModuleInfo, PredicateTable,
			PredCallId, TypeCheckInfo) :-
	PredCallId = PredName/_Arity,
	typecheck_info_get_io_state(TypeCheckInfo1, IOState0),
	(
		predicate_table_search_pred_sym(PredicateTable,
			PredName, OtherIds),
		predicate_table_get_preds(PredicateTable, Preds),
		OtherIds \= []
	->
		typecheck_find_arities(Preds, OtherIds, Arities),
		report_error_pred_num_args(TypeCheckInfo1, PredCallId,
			Arities, IOState0, IOState)
	;
		predicate_table_search_func_sym(PredicateTable,
			PredName, OtherIds),
		OtherIds \= []
	->
		report_error_func_instead_of_pred(TypeCheckInfo1, PredCallId,
			IOState0, IOState)
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

:- pred typecheck_call_overloaded_pred(list(pred_id), list(var),
				import_status, typecheck_info, typecheck_info).
:- mode typecheck_call_overloaded_pred(in, in, in,
				typecheck_info_di, typecheck_info_uo) is det.

typecheck_call_overloaded_pred(PredIdList, Args, CallingPredStatus,
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
	get_overloaded_pred_arg_types(PredIdList, Preds, CallingPredStatus,
			TypeAssignSet0, [], ArgsTypeAssignSet),
	%
	% then unify the types of the call arguments with the
	% called predicates' arg types
	%
	typecheck_var_has_arg_type_list(Args, 0, ArgsTypeAssignSet, 
		TypeCheckInfo0, TypeCheckInfo).

:- pred get_overloaded_pred_arg_types(list(pred_id), pred_table, import_status,
		type_assign_set, args_type_assign_set, args_type_assign_set).
:- mode get_overloaded_pred_arg_types(in, in, in, in, in, out) is det.

get_overloaded_pred_arg_types([], _Preds, _CallingPredStatus,
		_TypeAssignSet0, ArgsTypeAssignSet, ArgsTypeAssignSet).
get_overloaded_pred_arg_types([PredId | PredIds], Preds, CallingPredStatus,
		TypeAssignSet0, ArgsTypeAssignSet0, ArgsTypeAssignSet) :-
	map__lookup(Preds, PredId, PredInfo),
	pred_info_arg_types(PredInfo, PredTypeVarSet, PredArgTypes),
	rename_apart(TypeAssignSet0, PredTypeVarSet, PredArgTypes,
		ArgsTypeAssignSet0, ArgsTypeAssignSet1),
	get_overloaded_pred_arg_types(PredIds, Preds, CallingPredStatus,
		TypeAssignSet0, ArgsTypeAssignSet1, ArgsTypeAssignSet).

%-----------------------------------------------------------------------------%

typecheck__resolve_pred_overloading(ModuleInfo, Args, VarTypes, TVarSet,
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
	map__apply_to_list(Args, VarTypes, ArgTypes),
	(
		typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, ArgTypes, PredId1, PredName1)
	->
		PredId = PredId1,
		PredName = PredName1
	;
		% if there is no matching predicate for this call,
		% then this predicate must have a type error which
		% should have been caught by in typechecking.
		error("type error in pred call: no matching pred")
	).

typecheck__find_matching_pred_id([PredId | PredIds], ModuleInfo,
		TVarSet, ArgTypes, ThePredId, PredName) :-
	(
		% Calls to preds declared in .opt files should always be 
		% module qualified, so they should not be considered
		% when resolving overloading.
		module_info_pred_info(ModuleInfo, PredId, PredInfo),

		%
		% lookup the argument types of the candidate predicate
		% (or the argument types + return type of the candidate
		% function)
		%
		pred_info_arg_types(PredInfo, PredTVarSet, PredArgTypes0),

		%
		% rename them apart from the actual argument types
		%
		varset__merge_subst(TVarSet, PredTVarSet, _TVarSet1, Subst),
		term__apply_substitution_to_list(PredArgTypes0, Subst,
					PredArgTypes),

		%
		% check that the types of the candidate predicate/function
		% subsume the actual argument types
		%
		type_list_subsumes(PredArgTypes, ArgTypes, _TypeSubst)
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

:- pred typecheck_var_has_polymorphic_type_list(list(var), tvarset, list(type),
		typecheck_info, typecheck_info).
:- mode typecheck_var_has_polymorphic_type_list(in, in, in,
		typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_polymorphic_type_list(Args, PredTypeVarSet, PredArgTypes,
		TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	rename_apart(TypeAssignSet0, PredTypeVarSet, PredArgTypes,
				[], ArgsTypeAssignSet),
	typecheck_var_has_arg_type_list(Args, 0, ArgsTypeAssignSet,
				TypeCheckInfo0, TypeCheckInfo).

:- pred rename_apart(type_assign_set, tvarset, list(type),
                        args_type_assign_set, args_type_assign_set).
:- mode rename_apart(in, in, in, in, out) is det.

rename_apart([], _, _, ArgTypeAssigns, ArgTypeAssigns).
rename_apart([TypeAssign0 | TypeAssigns0], PredTypeVarSet, PredArgTypes0,
		ArgTypeAssigns0, ArgTypeAssigns) :-
        type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
                        TypeAssign, PredArgTypes),
        ArgTypeAssigns1 = [TypeAssign - PredArgTypes | ArgTypeAssigns0],
        rename_apart(TypeAssigns0, PredTypeVarSet, PredArgTypes0,
			ArgTypeAssigns1, ArgTypeAssigns).

:- pred type_assign_rename_apart(type_assign, tvarset, list(type),
			type_assign, list(type)).
:- mode type_assign_rename_apart(in, in, in, out, out) is det.

type_assign_rename_apart(TypeAssign0, PredTypeVarSet, PredArgTypes0,
		TypeAssign, PredArgTypes) :-
	type_assign_get_typevarset(TypeAssign0, TypeVarSet0),
	varset__merge(TypeVarSet0, PredTypeVarSet, PredArgTypes0,
			  TypeVarSet, PredArgTypes),
	type_assign_set_typevarset(TypeAssign0, TypeVarSet, TypeAssign).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_arg_type_list(list(var), int, args_type_assign_set,
				typecheck_info, typecheck_info).
:- mode typecheck_var_has_arg_type_list(in, in, in, 
				typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_arg_type_list([], _, ArgTypeAssignSet,
		TypeCheckInfo0, TypeCheckInfo) :-
	convert_args_type_assign_set(ArgTypeAssignSet, TypeAssignSet),
	typecheck_info_set_type_assign_set(TypeCheckInfo0, TypeAssignSet, 
		TypeCheckInfo).

typecheck_var_has_arg_type_list([Var|Vars], ArgNum, ArgTypeAssignSet0) -->
	{ ArgNum1 is ArgNum + 1 },
	typecheck_info_set_arg_num(ArgNum1),
	typecheck_var_has_arg_type(Var, ArgTypeAssignSet0, ArgTypeAssignSet1),
	typecheck_var_has_arg_type_list(Vars, ArgNum1, ArgTypeAssignSet1).

:- pred convert_args_type_assign_set(args_type_assign_set, type_assign_set).
:- mode convert_args_type_assign_set(in, out) is det.

convert_args_type_assign_set([], []).
convert_args_type_assign_set([TypeAssign - Args | ArgTypeAssigns],
				[TypeAssign | TypeAssigns]) :-
	( Args = [] ->
		true
	;
		% this should never happen, since the arguments should
		% all have been processed at this point
		error("convert_args_type_assign_set")
	),
	convert_args_type_assign_set(ArgTypeAssigns, TypeAssigns).

:- pred typecheck_var_has_arg_type(var, 
				args_type_assign_set, args_type_assign_set,
				typecheck_info, typecheck_info).
:- mode typecheck_var_has_arg_type(in, in, out,
				typecheck_info_di, typecheck_info_uo) is det.

typecheck_var_has_arg_type(VarId, ArgTypeAssignSet0, ArgTypeAssignSet,
				TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_head_type_params(TypeCheckInfo0, HeadTypeParams),
	typecheck_var_has_arg_type_2(ArgTypeAssignSet0, HeadTypeParams,
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
skip_arg([TypeAssign - Args0 | ArgTypeAssigns0],
				[TypeAssign - Args| ArgTypeAssigns]) :-
	( Args0 = [_ | Args1] ->
		Args = Args1
	;
		% this should never happen
		error("typecheck.m: skip_arg")
	),
	skip_arg(ArgTypeAssigns0, ArgTypeAssigns).

:- pred typecheck_var_has_arg_type_2(args_type_assign_set, headtypes, var,
				args_type_assign_set, args_type_assign_set).
:- mode typecheck_var_has_arg_type_2(in, in, in, in, out) is det.

typecheck_var_has_arg_type_2([], _, _) --> [].
typecheck_var_has_arg_type_2([TypeAssign0 - ArgTypes0 | TypeAssignSet0],
				HeadTypeParams, VarId) -->
	arg_type_assign_var_has_type(TypeAssign0, ArgTypes0,
					HeadTypeParams, VarId),
	typecheck_var_has_arg_type_2(TypeAssignSet0, HeadTypeParams, VarId).

:- pred arg_type_assign_var_has_type(type_assign, list(type), headtypes, var,
				args_type_assign_set, args_type_assign_set).
:- mode arg_type_assign_var_has_type(in, in, in, in, in, out) is det.

arg_type_assign_var_has_type(TypeAssign0, ArgTypes0, HeadTypeParams, VarId,
		ArgTypeAssignSet0, ArgTypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( ArgTypes0 = [Type | ArgTypes] ->
	    (
		map__search(VarTypes0, VarId, VarType)
	    ->
		(
		    type_assign_unify_type(TypeAssign0, HeadTypeParams,
				VarType, Type, TypeAssign1)
		->
		    ArgTypeAssignSet = [TypeAssign1 - ArgTypes |
					ArgTypeAssignSet0]
		;
		    ArgTypeAssignSet = ArgTypeAssignSet0
		)
	    ;
		map__det_insert(VarTypes0, VarId, Type, VarTypes),
		type_assign_set_var_types(TypeAssign0, VarTypes, TypeAssign),
		ArgTypeAssignSet = [TypeAssign - ArgTypes | ArgTypeAssignSet0]
	    )
	;
	    error("arg_type_assign_var_has_type")
	).

%-----------------------------------------------------------------------------%

	% Given a list of variables and a list of types, ensure
	% that each variable has the corresponding type.

:- pred typecheck_var_has_type_list(list(var), list(type), int, typecheck_info,
					typecheck_info).
:- mode typecheck_var_has_type_list(in, in, in, typecheck_info_di, 
					typecheck_info_uo) is det.

typecheck_var_has_type_list([], [_|_], _) -->
	{ error("typecheck_var_has_type_list: length mismatch") }.
typecheck_var_has_type_list([_|_], [], _) -->
	{ error("typecheck_var_has_type_list: length mismatch") }.
typecheck_var_has_type_list([], [], _) --> [].
typecheck_var_has_type_list([Var|Vars], [Type|Types], ArgNum) -->
	{ ArgNum1 is ArgNum + 1 },
	typecheck_info_set_arg_num(ArgNum1),
	typecheck_var_has_type(Var, Type),
	typecheck_var_has_type_list(Vars, Types, ArgNum1).

:- pred typecheck_var_has_type(var, type, typecheck_info, typecheck_info).
:- mode typecheck_var_has_type(in, in, typecheck_info_di, typecheck_info_uo)
	is det.

typecheck_var_has_type(VarId, Type, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	typecheck_info_get_head_type_params(TypeCheckInfo0, HeadTypeParams),
	typecheck_var_has_type_2(TypeAssignSet0, HeadTypeParams, VarId, Type,
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

:- pred get_type_stuff(type_assign_set, var, list(type_stuff)).
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

:- pred get_arg_type_stuff(args_type_assign_set, var, list(arg_type_stuff)).
:- mode get_arg_type_stuff(in, in, out) is det.
get_arg_type_stuff([], _VarId, []).
get_arg_type_stuff([TypeAssign - ArgTypes | ArgTypeAssigns], VarId, L) :-
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

:- pred typecheck_var_has_type_2(type_assign_set, headtypes, var, type,
				type_assign_set, type_assign_set).
:- mode typecheck_var_has_type_2(in, in, in, in, in, out) is det.

typecheck_var_has_type_2([], _, _, _) --> [].
typecheck_var_has_type_2([TypeAssign0 | TypeAssignSet0], HeadTypeParams, VarId,
		Type) -->
	type_assign_var_has_type(TypeAssign0, HeadTypeParams, VarId, Type),
	typecheck_var_has_type_2(TypeAssignSet0, HeadTypeParams, VarId, Type).

:- pred type_assign_var_has_type(type_assign, headtypes, var, type,
				type_assign_set, type_assign_set).
:- mode type_assign_var_has_type(in, in, in, in, in, out) is det.

type_assign_var_has_type(TypeAssign0, HeadTypeParams, VarId, Type,
		TypeAssignSet0, TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	( %%% if some [VarType]
		map__search(VarTypes0, VarId, VarType)
	->
		( %%% if some [TypeAssign1]
			type_assign_unify_type(TypeAssign0, HeadTypeParams,
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

:- pred type_assign_var_has_type_list(list(var), list(type), type_assign,
			typecheck_info, type_assign_set, type_assign_set).
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
	typecheck_info_get_head_type_params(TypeCheckInfo, HeadTypeParams),
	type_assign_var_has_type(TypeAssign0, HeadTypeParams, Arg, Type,
		[], TypeAssignSet1),
	type_assign_list_var_has_type_list(TypeAssignSet1,
		Args, Types, TypeCheckInfo, TypeAssignSet0, TypeAssignSet).

	% type_assign_list_var_has_type_list(TAs, Terms, Types, 
	%		TypeCheckInfo, TypeAssignSet0, TypeAssignSet):
	% 	Let TAs2 = { TA | TA is a an extension of a member of TAs
	%		    	  for which the types of the Terms unify with
	%		    	  their respective Types },
	% 	list__append(TAs, TypeAssignSet0, TypeAssignSet).

:- pred type_assign_list_var_has_type_list(type_assign_set, list(var),
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
	typecheck_info_get_io_state(T0, I0),
	globals__io_lookup_bool_option(debug_types, DoCheckPoint, I0, I1),
	( DoCheckPoint = yes ->
		checkpoint_2(Msg, T0, I1, I)
	;
		I = I1
	),
	typecheck_info_set_io_state(T0, I, T).

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
        { tree234__count(Tree, Count) },
        %{ tree234__depth(Tree, Depth) },
        io__write_string(Description),
        io__write_string(": count = "),
        io__write_int(Count),
        %io__write_string(", depth = "),
        %io__write_int(Depth),
        io__write_string("\n").

%-----------------------------------------------------------------------------%

	% Type check a unification.
	% Get the type assignment set from the type info and then just
	% iterate over all the possible type assignments.

:- pred typecheck_unification(var, unify_rhs, unify_rhs, typecheck_info, 					typecheck_info).
:- mode typecheck_unification(in, in, out, typecheck_info_di, 
				typecheck_info_uo) is det.

:- typecheck_unification(_, Y, _, _, _) when Y.

typecheck_unification(X, var(Y), var(Y)) -->
	typecheck_unify_var_var(X, Y).
typecheck_unification(X, functor(F, As), functor(F, As)) -->
	typecheck_unify_var_functor(X, F, As).
typecheck_unification(X, lambda_goal(PredOrFunc, Vars, Modes, Det, Goal0),
			 lambda_goal(PredOrFunc, Vars, Modes, Det, Goal)) -->
 	typecheck_lambda_var_has_type(PredOrFunc, X, Vars),
	typecheck_goal(Goal0, Goal).

:- pred typecheck_unify_var_var(var, var, typecheck_info, typecheck_info).
:- mode typecheck_unify_var_var(in, in, typecheck_info_di, typecheck_info_uo)
				is det.

typecheck_unify_var_var(X, Y, TypeCheckInfo0, TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	typecheck_unify_var_var_2(TypeAssignSet0, X, Y, TypeCheckInfo0,
			[], TypeAssignSet),
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

:- pred typecheck_unify_var_functor(var, cons_id, list(var), typecheck_info,
				typecheck_info).
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
			ConsDefnList),
	( ConsDefnList = [] ->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_undef_cons(TypeCheckInfo0, Functor, Arity, 
				IOState0, IOState1),
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
		typecheck_functor_type( ConsTypeAssignSet, Var,
			TypeCheckInfo0, [], ArgsTypeAssignSet),
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
				TypeAssignSet0,
				IOState2, IOState3),
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

:- type args_type_assign_set == list(pair(type_assign, list(type))).

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

	% typecheck_functor_type(ConsTypeAssignSet, Var, ...):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor type,
	% check that the type of `Var' matches this type.

:- pred typecheck_functor_type(cons_type_assign_set, var, typecheck_info,
				args_type_assign_set, args_type_assign_set).
:- mode typecheck_functor_type(in, in, typecheck_info_ui, in, out) is det.

typecheck_functor_type([], _, _) --> [].
typecheck_functor_type([TypeAssign - ConsType | ConsTypeAssigns],
			Var, TypeCheckInfo) -->
	{ ConsType = cons_type(Type, ArgTypes) },
	type_assign_check_functor_type(Type, ArgTypes, Var,
					TypeAssign, TypeCheckInfo),
	typecheck_functor_type(ConsTypeAssigns, Var, TypeCheckInfo).

	% typecheck_functor_arg_types(ConsTypeAssignSet, Var, Args, ...):
	%
	% For each possible cons type assignment in `ConsTypeAssignSet',
	% for each possible constructor argument types,
	% check that the types of `Args' matches these types.

:- pred typecheck_functor_arg_types(args_type_assign_set, list(var),
				typecheck_info, type_assign_set, 
				type_assign_set).
:- mode typecheck_functor_arg_types(in, in, typecheck_info_ui, in, out)
	is det.

typecheck_functor_arg_types([], _, _) --> [].
typecheck_functor_arg_types([TypeAssign - ArgTypes | ConsTypeAssigns],
			Args, TypeCheckInfo) -->
	type_assign_var_has_type_list(Args, ArgTypes, TypeAssign,
		TypeCheckInfo),
	typecheck_functor_arg_types(ConsTypeAssigns, Args, TypeCheckInfo).

	% iterate over all the possible type assignments.

:- pred typecheck_unify_var_var_2(type_assign_set, var, var, typecheck_info, 
				type_assign_set, type_assign_set).
:- mode typecheck_unify_var_var_2(in, in, in, typecheck_info_ui, in, out)
				is det.

typecheck_unify_var_var_2([], _, _, _) --> [].
typecheck_unify_var_var_2([TypeAssign0 | TypeAssigns0], X, Y, TypeCheckInfo) -->
	type_assign_unify_var_var(X, Y, TypeAssign0, TypeCheckInfo),
	typecheck_unify_var_var_2(TypeAssigns0, X, Y, TypeCheckInfo).

%-----------------------------------------------------------------------------%

	% Type-check the unification of two variables,
	% and update the type assignment.
	% TypeAssign0 is the type assignment we are updating,
	% TypeAssignSet0 is an accumulator for the list of possible
	% type assignments so far, and TypeAssignSet is TypeAssignSet plus
	% any type assignment(s) resulting from TypeAssign0 and this
	% unification.

:- pred type_assign_unify_var_var(var, var, type_assign, typecheck_info,
				type_assign_set, type_assign_set).
:- mode type_assign_unify_var_var(in, in, in, typecheck_info_ui, in, out)
				is det.

type_assign_unify_var_var(X, Y, TypeAssign0, TypeCheckInfo, TypeAssignSet0,
			TypeAssignSet) :-
	type_assign_get_var_types(TypeAssign0, VarTypes0),
	(
		map__search(VarTypes0, X, TypeX)
	->
		(
			map__search(VarTypes0, Y, TypeY)
		->
			% both X and Y already have types - just
			% unify their types
			typecheck_info_get_head_type_params(TypeCheckInfo,
					HeadTypeParams),
			( 
				type_assign_unify_type(TypeAssign0,
					HeadTypeParams, TypeX, TypeY,
					TypeAssign3)
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
		var, type_assign, typecheck_info,
		args_type_assign_set, args_type_assign_set).
:- mode type_assign_check_functor_type(in, in, in, in, typecheck_info_ui,
		in, out) is det.

type_assign_check_functor_type(ConsType, ArgTypes, Y, TypeAssign1,
		TypeCheckInfo, TypeAssignSet0, TypeAssignSet) :-

		% unify the type of Var with the type of the constructor
	type_assign_get_var_types(TypeAssign1, VarTypes0),
	( %%% if some [TypeY]
		map__search(VarTypes0, Y, TypeY)
	->
		typecheck_info_get_head_type_params(TypeCheckInfo,
				HeadTypeParams),
		( %%% if some [TypeAssign2]
			type_assign_unify_type(TypeAssign1, HeadTypeParams,
					ConsType, TypeY, TypeAssign2)
		->
			TypeAssignSet = [TypeAssign2 - ArgTypes |
					TypeAssignSet0]
		;
			TypeAssignSet = TypeAssignSet0
		)
	;
		map__det_insert(VarTypes0, Y, ConsType, VarTypes),
		type_assign_set_var_types(TypeAssign1, VarTypes, TypeAssign3),
		TypeAssignSet = [TypeAssign3 - ArgTypes | TypeAssignSet0]
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

	ConsDefn = cons_type_info(ConsTypeVarSet, ConsType0, ArgTypes0),

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
			TypeAssign1, [ConsType1 | ArgTypes1])
	->
		ConsType = ConsType1,
		ArgTypes = ArgTypes1,
		TypeAssign = TypeAssign1
	;
		error("get_cons_stuff: type_assign_rename_apart failed")
	).

%-----------------------------------------------------------------------------%

	% typecheck_lambda_var_has_type(Var, ArgVars, ...)
	% checks that `Var' has type `pred(T1, T2, ...)' where
	% T1, T2, ... are the types of the `ArgVars'.

:- pred typecheck_lambda_var_has_type(pred_or_func, var, list(var),
					typecheck_info, typecheck_info).
:- mode typecheck_lambda_var_has_type(in, in, in, typecheck_info_di, 
					typecheck_info_uo) is det.

typecheck_lambda_var_has_type(PredOrFunc, Var, ArgVars, TypeCheckInfo0,
				TypeCheckInfo) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo0, TypeAssignSet0),
	typecheck_info_get_head_type_params(TypeCheckInfo0, HeadTypeParams),
	typecheck_lambda_var_has_type_2(TypeAssignSet0, HeadTypeParams,
			PredOrFunc, Var, ArgVars, [], TypeAssignSet),
	(
		TypeAssignSet = [],
		TypeAssignSet0 \= []
	->
		typecheck_info_get_io_state(TypeCheckInfo0, IOState0),
		report_error_lambda_var(TypeCheckInfo0, PredOrFunc, Var,
				ArgVars, TypeAssignSet0, IOState0, IOState),
		typecheck_info_set_io_state(TypeCheckInfo0, IOState,
				TypeCheckInfo1),
		typecheck_info_set_found_error(TypeCheckInfo1, yes,
				TypeCheckInfo)
	;
		typecheck_info_set_type_assign_set(TypeCheckInfo0,
				TypeAssignSet, TypeCheckInfo)
	).

:- pred typecheck_lambda_var_has_type_2(type_assign_set, headtypes,
				pred_or_func, var, list(var),
				type_assign_set, type_assign_set).
:- mode typecheck_lambda_var_has_type_2(in, in, in, in, in, in, out) is det.

typecheck_lambda_var_has_type_2([], _, _, _, _) --> [].
typecheck_lambda_var_has_type_2([TypeAssign0 | TypeAssignSet0],
				HeadTypeParams, PredOrFunc, Var, ArgVars) -->
	{ type_assign_get_types_of_vars(ArgVars, TypeAssign0, ArgVarTypes,
					TypeAssign1) },
	{ term__context_init(Context) },
	{
		PredOrFunc = predicate, 
		LambdaType = term__functor(term__atom("pred"), ArgVarTypes,
					Context)
	;	
		PredOrFunc = function,
		pred_args_to_func_args(ArgVarTypes, FuncArgTypes, RetType),
		LambdaType = term__functor(term__atom("="),
				[term__functor(term__atom("func"),
					FuncArgTypes, Context),
				RetType], Context)
	},
	type_assign_var_has_type(TypeAssign1, HeadTypeParams, Var, LambdaType),
	typecheck_lambda_var_has_type_2(TypeAssignSet0, HeadTypeParams,
					PredOrFunc, Var, ArgVars).

:- pred type_assign_get_types_of_vars(list(var), type_assign, list(type),
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

:- pred type_assign_unify_type(type_assign, headtypes, type, type, type_assign).
:- mode type_assign_unify_type(in, in, in, in, out) is semidet.

type_assign_unify_type(TypeAssign0, HeadTypeParams, X, Y, TypeAssign) :-
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

:- type cons_type_info ---> cons_type_info(tvarset, type, list(type)).

:- pred make_pred_cons_info(typecheck_info, pred_id, pred_table, int,
		module_info, list(cons_type_info), list(cons_type_info)).
:- mode make_pred_cons_info(typecheck_info_ui, in, in, in, in, in, out) is det.

make_pred_cons_info(_TypeCheckInfo, PredId, PredTable, FuncArity,
		_ModuleInfo, L0, L) :-
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_arity(PredInfo, PredArity),
	pred_info_get_is_pred_or_func(PredInfo, IsPredOrFunc),
	(
		IsPredOrFunc = predicate,
		PredArity >= FuncArity
	->
		pred_info_arg_types(PredInfo, PredTypeVarSet,
					CompleteArgTypes),
		(
			list__split_list(FuncArity, CompleteArgTypes,
				ArgTypes, PredTypeParams)
		->
			term__context_init("<builtin>", 0, Context),
			PredType = term__functor(term__atom("pred"),
					PredTypeParams, Context),
			ConsInfo = cons_type_info(PredTypeVarSet,
					PredType, ArgTypes),
			L = [ConsInfo | L0]
		;
			error("make_pred_cons_info: split_list failed")
		)
	;
		IsPredOrFunc = function,
		PredAsFuncArity is PredArity - 1,
		PredAsFuncArity >= FuncArity
	->
		pred_info_arg_types(PredInfo, PredTypeVarSet,
					CompleteArgTypes),
		(
			list__split_list(FuncArity, CompleteArgTypes,
				FuncArgTypes, FuncTypeParams),
			list__length(FuncTypeParams, NumParams0),
			NumParams1 is NumParams0 - 1,
			list__split_list(NumParams1, FuncTypeParams,
			    FuncArgTypeParams, [FuncReturnTypeParam])
		->
			( FuncArgTypeParams = [] ->
				FuncType = FuncReturnTypeParam
			;
				term__context_init("<builtin>", 0, Context),
				FuncType = term__functor(
					term__atom("="), [
					term__functor(term__atom("func"),
						FuncArgTypeParams,
						Context),
					FuncReturnTypeParam
					], Context)
			),
			ConsInfo = cons_type_info(PredTypeVarSet,
					FuncType, FuncArgTypes),
			L = [ConsInfo | L0]
		;
			error("make_pred_cons_info: split_list or remove_suffix failed")
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
	Arity >= 2,
	Arity1 is Arity - 1,
	higher_order_func_type(Arity1, TypeVarSet, FuncType, ArgTypes, RetType),
	ConsTypeInfos = [cons_type_info(TypeVarSet, RetType,
					[FuncType | ArgTypes])].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The typecheck_info data structure and access predicates.

:- type typecheck_info
	---> typecheck_info(
			io__state, 	% The io state

			module_info, 	% The global symbol tables

			pred_call_id,	% The pred_call_id of the pred
					% being called (if any)

			int,		% The argument number within
					% a pred call 

			pred_id,	% The pred we're checking

			term__context,	% The context of the goal
					% we're checking

			unify_context,	% The original source of the
					% unification we're checking

			varset,		% Variable names

			type_assign_set,
					% This is the main piece of
					% information that we are
					% computing and which gets
					% updated as we go along

			bool,		% did we find any type errors?

			headtypes,	% Head type params

			bool,		% Have we already warned about
					% highly ambiguous overloading?
			import_status
					% Import status of the pred
					% being checked
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

:- pred typecheck_info_init(io__state, module_info, pred_id, varset,
	varset, map(var, type), headtypes, import_status, typecheck_info).
:- mode typecheck_info_init(di, in, in, in, in, in, in, in, typecheck_info_uo)
	is det.

typecheck_info_init(IOState0, ModuleInfo, PredId, TypeVarSet, VarSet,
		VarTypes, HeadTypeParams, Status, TypeCheckInfo) :-
	CallPredId = unqualified("") / 0,
	term__context_init(Context),
	map__init(TypeBindings),
	FoundTypeError = no,
	WarnedAboutOverloading = no,
	copy(IOState0, IOState),	% XXX
	TypeCheckInfo = typecheck_info(
		IOState, ModuleInfo, CallPredId, 0, PredId, Context,
		unify_context(explicit, []),
		VarSet, [type_assign(VarTypes, TypeVarSet, TypeBindings)],
		FoundTypeError, HeadTypeParams, WarnedAboutOverloading,
		Status
	).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_io_state(typecheck_info, io__state).
:- mode typecheck_info_get_io_state(typecheck_info_get_io_state, uo) is det.

typecheck_info_get_io_state(typecheck_info(IOState0,_,_,_,_,_,_,_,_,_,_,_,_), 
		IOState) :-
	copy(IOState0, IOState).	% XXX

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_io_state(typecheck_info, io__state, typecheck_info).
:- mode typecheck_info_set_io_state(typecheck_info_set_io_state, di, 
				typecheck_info_uo) is det.

typecheck_info_set_io_state(typecheck_info(_,B,C,D,E,F,G,H,I,J,K,L,M), IOState0,
			typecheck_info(IOState,B,C,D,E,F,G,H,I,J,K,L,M)) :-
	copy(IOState0, IOState).	% XXX

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_module_name(typecheck_info, string).
:- mode typecheck_info_get_module_name(in, out) is det.

typecheck_info_get_module_name(TypeCheckInfo, Name) :-
	TypeCheckInfo = typecheck_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_),
	module_info_name(ModuleInfo, Name).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_module_info(typecheck_info, module_info).
:- mode typecheck_info_get_module_info(in, out) is det.

typecheck_info_get_module_info(TypeCheckInfo, ModuleInfo) :-
	TypeCheckInfo = typecheck_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_preds(typecheck_info, predicate_table).
:- mode typecheck_info_get_preds(in, out) is det.

typecheck_info_get_preds(TypeCheckInfo, Preds) :-
	TypeCheckInfo = typecheck_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_), 
	module_info_get_predicate_table(ModuleInfo, Preds).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_types(typecheck_info, type_table).
:- mode typecheck_info_get_types(in, out) is det.

typecheck_info_get_types(TypeCheckInfo, Types) :-
	TypeCheckInfo = typecheck_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_),
	module_info_types(ModuleInfo, Types).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_ctors(typecheck_info, cons_table).
:- mode typecheck_info_get_ctors(in, out) is det.

typecheck_info_get_ctors(TypeCheckInfo, Ctors) :-
	TypeCheckInfo = typecheck_info(_,ModuleInfo,_,_,_,_,_,_,_,_,_,_,_),
	module_info_ctors(ModuleInfo, Ctors).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_called_predid(typecheck_info, pred_call_id).
:- mode typecheck_info_get_called_predid(in, out) is det.

typecheck_info_get_called_predid(TypeCheckInfo, PredId) :-
	TypeCheckInfo = typecheck_info(_,_,PredId,_,_,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_called_predid(pred_call_id, typecheck_info,
			typecheck_info).
:- mode typecheck_info_set_called_predid(in, typecheck_info_di,
			typecheck_info_uo) is det.

typecheck_info_set_called_predid(PredCallId, TypeCheckInfo0, TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,_,D,E,F,G,H,I,J,K,L,M),
	TypeCheckInfo = typecheck_info(A,B,PredCallId,D,E,F,G,H,I,J,K,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_arg_num(typecheck_info, int).
:- mode typecheck_info_get_arg_num(in, out) is det.

typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) :-
	TypeCheckInfo = typecheck_info(_,_,_,ArgNum,_,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_arg_num(int, typecheck_info, typecheck_info).
:- mode typecheck_info_set_arg_num(in, typecheck_info_di, 
		typecheck_info_uo) is det.

typecheck_info_set_arg_num(ArgNum, TypeCheckInfo0, TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,_,E,F,G,H,I,J,K,L,M),
	TypeCheckInfo = typecheck_info(A,B,C,ArgNum,E,F,G,H,I,J,K,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_predid(typecheck_info, pred_id).
:- mode typecheck_info_get_predid(in, out) is det.

typecheck_info_get_predid(TypeCheckInfo, PredId) :- 
	TypeCheckInfo = typecheck_info(_,_,_,_,PredId,_,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_context(typecheck_info, term__context).
:- mode typecheck_info_get_context(in, out) is det.

typecheck_info_get_context(TypeCheckInfo, Context) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,Context,_,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_context(term__context, typecheck_info, 
			typecheck_info).
:- mode typecheck_info_set_context(in, typecheck_info_di, 
			typecheck_info_uo) is det.

typecheck_info_set_context(Context, TypeCheckInfo0, TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,_,G,H,I,J,K,L,M),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,Context,G,H,I,J,K,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_unify_context(typecheck_info, unify_context).
:- mode typecheck_info_get_unify_context(in, out) is det.

typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,UnifyContext,_,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_unify_context(unify_context, typecheck_info, 
			typecheck_info).
:- mode typecheck_info_set_unify_context(in, typecheck_info_di, 
			typecheck_info_uo) is det.

typecheck_info_set_unify_context(UnifyContext, TypeCheckInfo0, TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,F,_,H,I,J,K,L,M),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,F,UnifyContext,H,I,J,K,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_varset(typecheck_info, varset).
:- mode typecheck_info_get_varset(in, out) is det.

typecheck_info_get_varset(TypeCheckInfo, VarSet) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,_,VarSet,_,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_type_assign_set(typecheck_info, type_assign_set).
:- mode typecheck_info_get_type_assign_set(in, out) is det.

typecheck_info_get_type_assign_set(TypeCheckInfo, TypeAssignSet) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,_,_,TypeAssignSet,_,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_final_info(typecheck_info, tvarset, map(var, type)).
:- mode typecheck_info_get_final_info(in, out, out) is det.

typecheck_info_get_final_info(TypeCheckInfo, NewTypeVarSet, NewVarTypes) :-
	typecheck_info_get_type_assign_set(TypeCheckInfo, TypeAssignSet),
	( TypeAssignSet = [TypeAssign | _] ->
		type_assign_get_typevarset(TypeAssign, OldTypeVarSet),
		type_assign_get_var_types(TypeAssign, VarTypes0),
		type_assign_get_type_bindings(TypeAssign, TypeBindings),
		map__keys(VarTypes0, Vars),
		expand_types(Vars, TypeBindings, VarTypes0, VarTypes),

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
		% that we need.
		%
		map__values(VarTypes, Types),
		term__vars_list(Types, TypeVars0),
		list__sort_and_remove_dups(TypeVars0, TypeVars),
		%
		% Next, create a new typevarset with the same number of
		% variables. 
		%
		list__length(TypeVars, NumTypeVars),
		varset__init(NewTypeVarSet0),
		varset__new_vars(NewTypeVarSet0, NumTypeVars, 
			NewTypeVars0, NewTypeVarSet1),
		%
		% We need to sort the fresh variables, to
		% ensure that the type substitution that we create below
		% does not alter the relative ordering of the type variables
		% (since that affects the order in which type_info
		% parameters will be passed).
		%
		list__sort(NewTypeVars0, NewTypeVars),
		%
		% Copy the type variable names across from the old
		% typevarset to the new typevarset.
		%
		varset__var_name_list(OldTypeVarSet, TypeVarNames),
		map__from_corresponding_lists(TypeVars, NewTypeVars, TSubst),
		copy_type_var_names(TypeVarNames, TSubst, NewTypeVarSet1,
			NewTypeVarSet),
		%
		% Finally, rename the types to use the new typevarset
		% type variables.
		%
		term__apply_variable_renaming_to_list(Types, TSubst, NewTypes),
		map__from_corresponding_lists(Vars, NewTypes, NewVarTypes)
	;
		error("internal error in typecheck_info_get_vartypes")
	).

	% fully expand the types of the variables by applying the type
	% bindings

:- pred expand_types(list(var), tsubst, map(var, type), map(var, type)).
:- mode expand_types(in, in, in, out) is det.

expand_types([], _, VarTypes, VarTypes).
expand_types([Var | Vars], TypeSubst, VarTypes0, VarTypes) :-
	map__lookup(VarTypes0, Var, Type0),
	term__apply_rec_substitution(Type0, TypeSubst, Type),
	map__det_update(VarTypes0, Var, Type, VarTypes1),
	expand_types(Vars, TypeSubst, VarTypes1, VarTypes).

:- pred copy_type_var_names(assoc_list(tvar, string), map(tvar, tvar),
				tvarset, tvarset).
:- mode copy_type_var_names(in, in, in, out) is det.

copy_type_var_names([], _TSubst, NewTypeVarSet, NewTypeVarSet).
copy_type_var_names([OldTypeVar - Name | Rest], TypeSubst, NewTypeVarSet0,
			NewTypeVarSet) :-
	( map__search(TypeSubst, OldTypeVar, NewTypeVar) ->
		varset__name_var(NewTypeVarSet0, NewTypeVar, Name,
			NewTypeVarSet1)
	;
		NewTypeVarSet1 = NewTypeVarSet0
	),
	copy_type_var_names(Rest, TypeSubst, NewTypeVarSet1, NewTypeVarSet).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_type_assign_set(typecheck_info, type_assign_set,
			typecheck_info).
:- mode typecheck_info_set_type_assign_set(typecheck_info_di, in,
			typecheck_info_uo) is det.

typecheck_info_set_type_assign_set(TypeCheckInfo0, TypeAssignSet, 
					TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,F,G,H,_,J,K,L,M),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,F,G,H,TypeAssignSet,J,K,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_found_error(typecheck_info, bool).
:- mode typecheck_info_get_found_error(typecheck_info_ui, out) is det.

typecheck_info_get_found_error(TypeCheckInfo, FoundError) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,_,_,_,FoundError,_,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_found_error(typecheck_info, bool, typecheck_info).
:- mode typecheck_info_set_found_error(typecheck_info_di, in, 
			typecheck_info_uo) is det.

typecheck_info_set_found_error(TypeCheckInfo0, FoundError, TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,F,G,H,I,_,K,L,M),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,F,G,H,I,FoundError,K,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_head_type_params(typecheck_info, headtypes).
:- mode typecheck_info_get_head_type_params(typecheck_info_ui, out) is det.

typecheck_info_get_head_type_params(TypeCheckInfo, HeadTypeParams) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,_,_,_,_,HeadTypeParams,_,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_head_type_params(typecheck_info, headtypes, 
			typecheck_info).
:- mode typecheck_info_set_head_type_params(typecheck_info_di, in, 
			typecheck_info_uo) is det.

typecheck_info_set_head_type_params(TypeCheckInfo0, HeadTypeParams,
					TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,F,G,H,I,J,_,L,M),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,F,G,H,I,J,HeadTypeParams,L,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_warned_about_overloading(typecheck_info, bool).
:- mode typecheck_info_get_warned_about_overloading(typecheck_info_ui, out)
			is det.

typecheck_info_get_warned_about_overloading(TypeCheckInfo, Warned) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,_,_,_,_,_,Warned,_).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_set_warned_about_overloading(typecheck_info, bool, 
			typecheck_info).
:- mode typecheck_info_set_warned_about_overloading(typecheck_info_di, in, 
			typecheck_info_uo) is det.

typecheck_info_set_warned_about_overloading(TypeCheckInfo0, Warned,
				TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,F,G,H,I,J,K,_,M),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,F,G,H,I,J,K,Warned,M).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_pred_import_status(typecheck_info, import_status).
:- mode typecheck_info_get_pred_import_status(typecheck_info_ui, out) is det.

typecheck_info_get_pred_import_status(TypeCheckInfo, Status) :-
	TypeCheckInfo = typecheck_info(_,_,_,_,_,_,_,_,_,_,_,_,Status).

:- pred typecheck_info_set_pred_import_status(typecheck_info, import_status,
			typecheck_info).
:- mode typecheck_info_set_pred_import_status(typecheck_info_di, in,
			typecheck_info_uo) is det.

typecheck_info_set_pred_import_status(TypeCheckInfo0, Status, TypeCheckInfo) :-
	TypeCheckInfo0 = typecheck_info(A,B,C,D,E,F,G,H,I,J,K,L,_),
	TypeCheckInfo = typecheck_info(A,B,C,D,E,F,G,H,I,J,K,L,Status).

%-----------------------------------------------------------------------------%

:- pred typecheck_info_get_ctor_list(typecheck_info, cons_id, int, 
			list(cons_type_info)).
:- mode typecheck_info_get_ctor_list(typecheck_info_ui, in, in, out) is det.

typecheck_info_get_ctor_list(TypeCheckInfo, Functor, Arity, ConsInfoList) :-
	(
		builtin_apply_type(TypeCheckInfo, Functor, Arity,
			ApplyConsInfoList)
	->
		ConsInfoList = ApplyConsInfoList
	;
		typecheck_info_get_ctor_list_2(TypeCheckInfo, Functor, Arity,
			ConsInfoList)
	).

:- pred typecheck_info_get_ctor_list_2(typecheck_info, cons_id,
		int, list(cons_type_info)).
:- mode typecheck_info_get_ctor_list_2(typecheck_info_ui, in, in, out) is det.

typecheck_info_get_ctor_list_2(TypeCheckInfo, Functor, Arity, ConsInfoList) :-
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
	% Check if Functor is a constant of one of the builtin atomic
	% types (string, float, int, character).  If so, insert
	% the resulting cons_type_info at the start of the list.
	(
		Arity = 0,
		builtin_atomic_type(Functor, BuiltInTypeName)
	->
		term__context_init("<builtin>", 0, Context),
		ConsType = term__functor(term__atom(BuiltInTypeName), [],
				Context),
		varset__init(ConsTypeVarSet),
		ConsInfo = cons_type_info(ConsTypeVarSet, ConsType, []),
		ConsInfoList1 = [ConsInfo | ConsInfoList0]
	;
		ConsInfoList1 = ConsInfoList0
	),
	% Check if Functor is the name of a predicate which takes at least
	% Arity arguments.  If so, insert the resulting cons_type_info
	% at the start of the list.
	(
		builtin_pred_type(TypeCheckInfo, Functor, Arity,
			PredConsInfoList)
	->
		list__append(ConsInfoList1, PredConsInfoList, ConsInfoList)
	;
		ConsInfoList = ConsInfoList1
	).

:- pred convert_cons_defn_list(typecheck_info, list(hlds_cons_defn),
				list(cons_type_info)).
:- mode convert_cons_defn_list(typecheck_info_ui, in, out) is det.

:- convert_cons_defn_list(_, L, _) when L.	% NU-Prolog indexing.

convert_cons_defn_list(_TypeCheckInfo, [], []).
convert_cons_defn_list(TypeCheckInfo, [X|Xs], [Y|Ys]) :-
	convert_cons_defn(TypeCheckInfo, X, Y),
	convert_cons_defn_list(TypeCheckInfo, Xs, Ys).

:- pred convert_cons_defn(typecheck_info, hlds_cons_defn, cons_type_info).
:- mode convert_cons_defn(typecheck_info_ui, in, out) is det.

convert_cons_defn(TypeCheckInfo, HLDS_ConsDefn, ConsTypeInfo) :-
	HLDS_ConsDefn = hlds_cons_defn(ArgTypes, TypeId, Context),
	typecheck_info_get_types(TypeCheckInfo, Types),
	map__lookup(Types, TypeId, TypeDefn),
	hlds_data__get_type_defn_tvarset(TypeDefn, ConsTypeVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, ConsTypeParams),
	construct_type(TypeId, ConsTypeParams, Context, ConsType),
	ConsTypeInfo = cons_type_info(ConsTypeVarSet, ConsType, ArgTypes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% The type_assign and type_assign_set data structures.

:- type type_assign_set	==	list(type_assign).

:- type type_assign	--->	type_assign(
					map(var, type),		% var types
					tvarset,		% type names
					tsubst			% type bindings
				).

%-----------------------------------------------------------------------------%

	% Access predicates for the type_assign data structure.
	% Excruciatingly boring code.

:- pred type_assign_get_var_types(type_assign, map(var, type)).
:- mode type_assign_get_var_types(in, out) is det.

type_assign_get_var_types(type_assign(VarTypes, _, _), VarTypes).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_typevarset(type_assign, tvarset).
:- mode type_assign_get_typevarset(in, out) is det.

type_assign_get_typevarset(type_assign(_, TypeVarSet, _), TypeVarSet).

%-----------------------------------------------------------------------------%

:- pred type_assign_get_type_bindings(type_assign, tsubst).
:- mode type_assign_get_type_bindings(in, out) is det.

type_assign_get_type_bindings(type_assign(_, _, TypeBindings), TypeBindings).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_var_types(type_assign, map(var, type), type_assign).
:- mode type_assign_set_var_types(in, in, out) is det.

type_assign_set_var_types(type_assign(_, B, C), VarTypes,
			type_assign(VarTypes, B, C)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_typevarset(type_assign, tvarset, type_assign).
:- mode type_assign_set_typevarset(in, in, out) is det.

type_assign_set_typevarset(type_assign(A, _, C), TypeVarSet,
			type_assign(A, TypeVarSet, C)).

%-----------------------------------------------------------------------------%

:- pred type_assign_set_type_bindings(type_assign, tsubst, type_assign).
:- mode type_assign_set_type_bindings(in, in, out) is det.

type_assign_set_type_bindings(type_assign(A, B, _), TypeBindings,
			type_assign(A, B, TypeBindings)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The next section contains predicates for writing diagnostics
% (warnings and errors).

%-----------------------------------------------------------------------------%

	% write out the inferred `pred' or `func' declarations
	% for a list of predicates.

:- pred write_inference_messages(list(pred_id), module_info,
				io__state, io__state).
:- mode write_inference_messages(in, in, di, uo) is det.

write_inference_messages([], _) --> [].
write_inference_messages([PredId | PredIds], ModuleInfo) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_marker_list(PredInfo, Markers) },
	(
		{ list__member(request(infer_type), Markers) },
		{ module_info_predids(ModuleInfo, ValidPredIds) },
		{ list__member(PredId, ValidPredIds) }
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
	{ pred_info_arg_types(PredInfo, VarSet, Types0) },
	{ strip_builtin_qualifiers_from_type_list(Types0, Types) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ MaybeDet = no },
	prog_out__write_context(Context),
	io__write_string("Inferred "),
	(	{ PredOrFunc = predicate },
		mercury_output_pred_type(VarSet, Name, Types, MaybeDet,
			Context)
	;	{ PredOrFunc = function },
		{ pred_args_to_func_args(Types, ArgTypes, RetType) },
		mercury_output_func_type(VarSet, Name, ArgTypes,
			RetType, MaybeDet, Context)
	).

%-----------------------------------------------------------------------------%

:- pred report_error_no_clauses(pred_id, pred_info,
					module_info, io__state, io__state).
:- mode report_error_no_clauses(in, in, in, di, uo) is det.

report_error_no_clauses(PredId, PredInfo, ModuleInfo) -->
	{ pred_info_context(PredInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("Error: no clauses for "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string("\n").

%-----------------------------------------------------------------------------%

:- pred report_warning_too_much_overloading(typecheck_info, io__state, 
			io__state).
:- mode report_warning_too_much_overloading(typecheck_info_no_io, di, uo)
			is det.

report_warning_too_much_overloading(TypeCheckInfo) -->
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	write_context_and_pred_id(TypeCheckInfo),
	prog_out__write_context(Context),
	report_warning("  warning: highly ambiguous overloading.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string(
		    "  This may cause type-checking to be very slow.\n"
		),
		prog_out__write_context(Context),
		io__write_string(
		    "  It may also make your code difficult to understand.\n"
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred report_error_unif_var_var(typecheck_info, var, var, type_assign_set,
					io__state, io__state).
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
	write_type_of_var(TypeCheckInfo, TypeAssignSet, X),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  `"),
	mercury_output_var(Y, VarSet, no),
	io__write_string("'"),
	write_type_of_var(TypeCheckInfo, TypeAssignSet, Y),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_functor_type(typecheck_info, var, list(cons_type_info),
					cons_id, int,
					type_assign_set,
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
	write_type_of_var(TypeCheckInfo, TypeAssignSet, Var),
	io__write_string(",\n"),

	prog_out__write_context(Context),
	io__write_string("  "),
	write_functor_name(Functor, Arity),
	write_type_of_functor(Functor, Arity, Context, ConsDefnList),
	io__write_string(".\n"),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred report_error_lambda_var(typecheck_info, pred_or_func, var, list(var),
				type_assign_set, io__state, io__state).
:- mode report_error_lambda_var(typecheck_info_no_io, in, in, in, in, di, uo)
				is det.

report_error_lambda_var(TypeCheckInfo, PredOrFunc, Var, ArgVars,
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
	(
		{ PredOrFunc = predicate },
		io__write_string("  and `pred("),
		mercury_output_vars(ArgVars, VarSet, no),
		io__write_string(") :- ...':\n")
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgVars, FuncArgs, RetVar) },
		io__write_string("  and `func("),
		mercury_output_vars(FuncArgs, VarSet, no),
		io__write_string(") = "),
		mercury_output_var(RetVar, VarSet, no),
		io__write_string(" :- ...':\n")
	),

	prog_out__write_context(Context),
	io__write_string("  "),
	write_argument_name(VarSet, Var),
	write_type_of_var(TypeCheckInfo, TypeAssignSet, Var),
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

:- pred report_error_functor_arg_types(typecheck_info, var,
			list(cons_type_info), cons_id, list(var),
			type_assign_set, io__state, io__state).
:- mode report_error_functor_arg_types(typecheck_info_no_io, in, in, in, in,
			in, di, uo) is det.

report_error_functor_arg_types(TypeCheckInfo, Var, ConsDefnList, Functor, Args,
		TypeAssignSet) -->

	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
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
	hlds_out__write_functor_cons_id(Functor1, Args, VarSet, no),
	io__write_string("':\n"),
	prog_out__write_context(Context),
	io__write_string("  type error in argument(s) of "),
	write_functor_name(Functor1, Arity),
	io__write_string(".\n"),

	% XXX we should print type pairs (one type from each side)
	% only for the arguments in which the two types differ.
	prog_out__write_context(Context),
	io__write_string("  "),
	write_functor_name(Functor, Arity),
	write_type_of_functor(Functor, Arity, Context, ConsDefnList),

	write_types_of_vars(Args, VarSet, Context, TypeCheckInfo, 
		TypeAssignSet),

	write_type_assign_set_msg(TypeAssignSet, VarSet).

:- pred write_types_of_vars(list(var), varset, term__context, typecheck_info,
				type_assign_set, io__state, io__state).
:- mode write_types_of_vars(in, in, in, typecheck_info_ui, in, di, uo) is det.

write_types_of_vars([], _, _, _, _) -->
	io__write_string(".\n").
write_types_of_vars([Var | Vars], VarSet, Context, TypeCheckInfo, 
			TypeAssignSet) -->
	io__write_string(",\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	write_argument_name(VarSet, Var),
	write_type_of_var(TypeCheckInfo, TypeAssignSet, Var),
	write_types_of_vars(Vars, VarSet, Context, TypeCheckInfo,
		TypeAssignSet).

:- pred write_argument_name(varset, var, io__state, io__state).
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
		)
	;
		io__write_string("functor `"),
		hlds_out__write_cons_id(Functor1)
	),
	io__write_string("'").

:- pred write_type_of_var(typecheck_info, type_assign_set, var,
				io__state, io__state).
:- mode write_type_of_var(typecheck_info_no_io, in, in, di, uo) is det.

write_type_of_var(_TypeCheckInfo, TypeAssignSet, Var) -->
	{ get_type_stuff(TypeAssignSet, Var, TypeStuffList) },
	( { TypeStuffList = [SingleTypeStuff] } ->
		{ SingleTypeStuff = type_stuff(VType, TVarSet, TBinding) },
		io__write_string(" has type `"),
		write_type_b(VType, TVarSet, TBinding),
		io__write_string("'")
	;
		io__write_string(" has overloaded type { "),
		write_type_stuff_list(TypeStuffList),
		io__write_string(" }")
	).

:- pred write_type_of_functor(cons_id, int, term__context, list(cons_type_info),
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

:- pred write_cons_type(cons_type_info, cons_id, term__context,
			io__state, io__state).
:- mode write_cons_type(in, in, in, di, uo) is det.

write_cons_type(cons_type_info(TVarSet, ConsType0, ArgTypes0), Functor, _) -->
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

:- pred write_cons_type_list(list(cons_type_info), cons_id, int, term__context,
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

:- pred write_type_assign_set_msg(type_assign_set, tvarset,
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

:- pred write_args_type_assign_set_msg(args_type_assign_set, tvarset,
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

:- pred write_type_assign_set(type_assign_set, tvarset, io__state, io__state).
:- mode write_type_assign_set(in, in, di, uo) is det.

write_type_assign_set([], _) --> [].
write_type_assign_set([TypeAssign | TypeAssigns], VarSet) -->
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_type_assign_set(TypeAssigns, VarSet).

:- pred write_args_type_assign_set(args_type_assign_set, tvarset,
				io__state, io__state).
:- mode write_args_type_assign_set(in, in, di, uo) is det.

write_args_type_assign_set([], _) --> [].
write_args_type_assign_set([TypeAssign - _ArgTypes| TypeAssigns], VarSet) -->
	io__write_string("\t"),
	write_type_assign(TypeAssign, VarSet),
	io__write_string("\n"),
	write_args_type_assign_set(TypeAssigns, VarSet).

:- pred write_type_assign(type_assign, tvarset, io__state, io__state).
:- mode write_type_assign(in, in, di, uo) is det.

write_type_assign(TypeAssign, VarSet) -->
	{
	  type_assign_get_var_types(TypeAssign, VarTypes),
	  type_assign_get_type_bindings(TypeAssign, TypeBindings),
	  type_assign_get_typevarset(TypeAssign, TypeVarSet),
	  map__keys(VarTypes, Vars)
	},
	write_type_assign_2(Vars, VarSet, VarTypes, TypeBindings, TypeVarSet,
			no),
	io__write_string("\n").

:- pred write_type_assign_2(list(var), varset, map(var, type),
			tsubst, tvarset, bool, io__state, io__state).
:- mode write_type_assign_2(in, in, in, in, in, in, di, uo) is det.

write_type_assign_2([], _, _, _, _, FoundOne) -->
	( { FoundOne = no } ->
		io__write_string("(No variables were assigned a type)")
	;
		[]
	).

write_type_assign_2([Var | Vars], VarSet, VarTypes, TypeBindings, TypeVarSet,
			FoundOne) -->
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
		write_type_assign_2(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, yes)
	;
		write_type_assign_2(Vars, VarSet, VarTypes, TypeBindings,
			TypeVarSet, FoundOne)
	).

	% write_type_b writes out a type after applying the type bindings.

:- pred write_type_b(type, tvarset, tsubst, io__state, io__state).
:- mode write_type_b(in, in, in, di, uo) is det.

write_type_b(Type, TypeVarSet, TypeBindings) -->
	{ term__apply_rec_substitution(Type, TypeBindings, Type2) },
	{ strip_builtin_qualifiers_from_type(Type2, Type3) },
	mercury_output_term(Type3, TypeVarSet, no).

%-----------------------------------------------------------------------------%

:- pred report_error_var(typecheck_info, var, type, type_assign_set,
			io__state, io__state).
:- mode report_error_var(typecheck_info_no_io, in, in, in, di, uo) is det.

report_error_var(TypeCheckInfo, VarId, Type, TypeAssignSet0) -->
	{ typecheck_info_get_called_predid(TypeCheckInfo, CalledPredId) },
	{ typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	{ get_type_stuff(TypeAssignSet0, VarId, TypeStuffList) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	write_context_and_pred_id(TypeCheckInfo),
	write_call_context(Context, CalledPredId, ArgNum, UnifyContext),
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

:- pred report_error_arg_var(typecheck_info, var, args_type_assign_set,
			io__state, io__state).
:- mode report_error_arg_var(typecheck_info_no_io, in, in, di, uo) is det.

report_error_arg_var(TypeCheckInfo, VarId, ArgTypeAssignSet0) -->
	{ typecheck_info_get_called_predid(TypeCheckInfo, CalledPredId) },
	{ typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	{ get_arg_type_stuff(ArgTypeAssignSet0, VarId, ArgTypeStuffList) },
	{ typecheck_info_get_varset(TypeCheckInfo, VarSet) },
	write_context_and_pred_id(TypeCheckInfo),
	write_call_context(Context, CalledPredId, ArgNum, UnifyContext),
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

:- pred write_type_stuff_list(list(type_stuff), io__state, io__state).
:- mode write_type_stuff_list(in, di, uo) is det.

write_type_stuff_list([]) --> [].
write_type_stuff_list([type_stuff(T, TVarSet, TBinding) | Ts]) -->
	write_type_b(T, TVarSet, TBinding),
	write_type_stuff_list_2(Ts).

:- pred write_type_stuff_list_2(list(type_stuff), io__state, io__state).
:- mode write_type_stuff_list_2(in, di, uo) is det.

write_type_stuff_list_2([]) --> [].
write_type_stuff_list_2([type_stuff(T, TVarSet, TBinding) | Ts]) -->
	io__write_string(", "),
	write_type_b(T, TVarSet, TBinding),
	write_type_stuff_list_2(Ts).

:- pred write_var_type_stuff_list(list(type_stuff), type, io__state, io__state).
:- mode write_var_type_stuff_list(in, in, di, uo) is det.

write_var_type_stuff_list([], _Type) --> [].
write_var_type_stuff_list([type_stuff(VT, TVarSet, TBinding) | Ts], T) -->
	write_type_b(VT, TVarSet, TBinding),
	io__write_string("/"),
	write_type_b(T, TVarSet, TBinding),
	write_var_type_stuff_list_2(Ts, T).

:- pred write_var_type_stuff_list_2(list(type_stuff), type,
					io__state, io__state).
:- mode write_var_type_stuff_list_2(in, in, di, uo) is det.

write_var_type_stuff_list_2([], _Type) --> [].
write_var_type_stuff_list_2([type_stuff(VT, TVarSet, TBinding) | Ts], T) -->
	io__write_string(", "),
	write_type_b(VT, TVarSet, TBinding),
	io__write_string("/"),
	write_type_b(T, TVarSet, TBinding),
	write_type_stuff_list_2(Ts).

:- pred write_arg_type_stuff_list(list(arg_type_stuff), io__state, io__state).
:- mode write_arg_type_stuff_list(in, di, uo) is det.

write_arg_type_stuff_list([]) --> [].
write_arg_type_stuff_list([arg_type_stuff(T0, VT0, TVarSet) | Ts]) -->
	{ strip_builtin_qualifiers_from_type(VT0, VT) },
	mercury_output_term(VT, TVarSet, no),
	io__write_string("/"),
	{ strip_builtin_qualifiers_from_type(T0, T) },
	mercury_output_term(T, TVarSet, no),
	write_arg_type_stuff_list_2(Ts).

:- pred write_arg_type_stuff_list_2(list(arg_type_stuff), io__state, io__state).
:- mode write_arg_type_stuff_list_2(in, di, uo) is det.

write_arg_type_stuff_list_2([]) --> [].
write_arg_type_stuff_list_2([arg_type_stuff(T0, VT0, TVarSet) | Ts]) -->
	io__write_string(", "),
	{ strip_builtin_qualifiers_from_type(VT0, VT) },
	mercury_output_term(VT, TVarSet, no),
	io__write_string("/"),
	{ strip_builtin_qualifiers_from_type(T0, T) },
	mercury_output_term(T, TVarSet, no),
	write_arg_type_stuff_list_2(Ts).

%-----------------------------------------------------------------------------%

:- pred report_error_undef_pred(typecheck_info, pred_call_id, 
			io__state, io__state).
:- mode report_error_undef_pred(typecheck_info_no_io, in, di, uo) is det.

report_error_undef_pred(TypeCheckInfo, PredCallId) -->
	{ PredCallId = PredName/Arity },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	write_typecheck_info_context(TypeCheckInfo),
	(
		{ PredName = unqualified("->"), Arity = 2 }
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
		{ PredName = unqualified("else"), Arity = 2 }
	->
		io__write_string("  error: unmatched `else'.\n")
	;
		{ PredName = unqualified("if"), Arity = 2 }
	->
		io__write_string("  error: `if' without `then' or `else'.\n")
	;
		{ PredName = unqualified("then"), Arity = 2 }
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
		{ PredName = unqualified("apply"), Arity >= 2 }
	->
		report_error_apply_instead_of_pred(TypeCheckInfo)
	;
		io__write_string("  error: undefined predicate `"),
		hlds_out__write_pred_call_id(PredCallId),
		io__write_string("'.\n")
	).

:- pred report_error_func_instead_of_pred(typecheck_info, pred_call_id,
					io__state, io__state).
:- mode report_error_func_instead_of_pred(typecheck_info_no_io, in, di, uo)
					is det.

report_error_func_instead_of_pred(TypeCheckInfo, PredCallId) -->
	report_error_undef_pred(TypeCheckInfo, PredCallId),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  (There is a *function* with that name, however.\n"),
	prog_out__write_context(Context),
	io__write_string("  Perhaps you forgot to add ` = ...'?)\n").

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

:- pred report_error_pred_num_args(typecheck_info, pred_call_id, list(int),
					io__state, io__state).
:- mode report_error_pred_num_args(typecheck_info_no_io, in, in, di, uo) is det.

report_error_pred_num_args(TypeCheckInfo, Name / Arity, Arities) -->
	write_typecheck_info_context(TypeCheckInfo),
	io__write_string("  error: wrong number of arguments ("),
	io__write_int(Arity),
	io__write_string("; should be "),
	report_error_right_num_args(Arities),
	io__write_string(")\n"),
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	prog_out__write_context(Context),
	io__write_string("  in call to pred `"),
	prog_out__write_sym_name(Name),
	io__write_string("'.\n").

:- pred report_error_right_num_args(list(int), io__state, io__state).
:- mode report_error_right_num_args(in, di, uo) is det.

report_error_right_num_args([]) --> [].
report_error_right_num_args([Arity | Arities]) -->
	io__write_int(Arity),
	( { Arities = [] } ->
		[]
	; { Arities = [_] } ->
		io__write_string(" or ")
	;
		io__write_string(", ")
	),
	report_error_right_num_args(Arities).

:- pred report_error_undef_cons(typecheck_info, cons_id, int, io__state, 
			io__state).
:- mode report_error_undef_cons(typecheck_info_no_io, in, in, di, uo) is det.

report_error_undef_cons(TypeCheckInfo, Functor, Arity) -->
	{ typecheck_info_get_called_predid(TypeCheckInfo, CalledPredId) },
	{ typecheck_info_get_arg_num(TypeCheckInfo, ArgNum) },
	{ typecheck_info_get_context(TypeCheckInfo, Context) },
	{ typecheck_info_get_unify_context(TypeCheckInfo, UnifyContext) },
	write_context_and_pred_id(TypeCheckInfo),
	write_call_context(Context, CalledPredId, ArgNum, UnifyContext),
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
		"  Mercury Language Reference Manual.\n")
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
	; 
		(
			{ Functor = cons(Constructor, Arity) },
			{ typecheck_info_get_ctors(TypeCheckInfo, ConsTable) },
			{ solutions(lambda([N::out] is nondet, 
				map__member(ConsTable, 
					    cons(Constructor, N),
					    _)),
				ActualArities) },
			{ ActualArities = [_|_] }
		->
			report_wrong_arity_constructor(Constructor, Arity,
				ActualArities, Context)
		;
			io__write_string("  error: undefined symbol `"),
			{ strip_builtin_qualifier_from_cons_id(Functor, 
				Functor1) },
			hlds_out__write_cons_id(Functor1),
			io__write_string("'.\n")
		)
	).

:- pred report_wrong_arity_constructor(sym_name, arity, list(int), 
	term__context, io__state, io__state).
:- mode report_wrong_arity_constructor(in, in, in, in, di, uo) is det.

report_wrong_arity_constructor(Name, Arity, ActualArities, Context) -->
	io__write_string("  error: wrong number of arguments ("),
	io__write_int(Arity),
	io__write_string("; should be "),
	report_error_right_num_args(ActualArities),
	io__write_string(")\n"),
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
language_builtin("\\+", 2).
language_builtin("not", 2).
language_builtin("<=>", 2).
language_builtin("=>", 2).
language_builtin("<=", 2).
language_builtin("call", _).

:- pred write_call_context(term__context, pred_call_id, int, unify_context,
				io__state, io__state).
:- mode write_call_context(in, in, in, in, di, uo) is det.

write_call_context(Context, PredCallId, ArgNum, UnifyContext) -->
	( { ArgNum = 0 } ->
		hlds_out__write_unify_context(UnifyContext, Context)
	;
		prog_out__write_context(Context),
		io__write_string("  in argument "),
		io__write_int(ArgNum),
		io__write_string(" of call to pred `"),
		hlds_out__write_pred_call_id(PredCallId),
		io__write_string("':\n")
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
		io__write_string("\tYou will need to add an explicit type qualification to resolve the\n"),
		io__write_string("\ttype ambiguity.\n"),
		io__write_string("\tThe way to add an explicit type qualification\n"),
		io__write_string("\tis to insert a call to a dummy predicate whose `:- pred'\n"),
		io__write_string("\tdeclaration specifies the appropriate argument types.\n")
	;
		[]
	).

:- pred report_ambiguity_error_2(list(var), varset, typecheck_info,
			type_assign, type_assign, bool, bool,
			io__state, io__state).
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
	% term__contexts, i.e. whether they can be unified without
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


	% Make error messages more readable by removing "mercury_builtin"
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
			SymName0 = qualified("mercury_builtin", Name)
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
