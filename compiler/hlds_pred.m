%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines the part of the HLDS that deals with predicates
% and procedures.

% Main authors: fjh, conway.

:- module hlds_pred.

:- interface.

:- import_module hlds_data, hlds_goal, hlds_module, llds, prog_data, instmap.
:- import_module purity, globals.
:- import_module bool, list, set, map, std_util, term, varset.
:- import_module term_util.

:- implementation.

:- import_module code_aux, goal_util, make_hlds, prog_util.
:- import_module mode_util, type_util, options.
:- import_module int, string, require, assoc_list.

%-----------------------------------------------------------------------------%

:- interface.

	% A proc_id is the name of a mode within a particular predicate -
	% not to be confused with a mode_id, which is the name of a
	% user-defined mode.

:- type pred_id.
:- type proc_id.

:- pred hlds_pred__initial_pred_id(pred_id).
:- mode hlds_pred__initial_pred_id(out) is det.

:- pred hlds_pred__initial_proc_id(proc_id).
:- mode hlds_pred__initial_proc_id(out) is det.

:- pred hlds_pred__next_pred_id(pred_id, pred_id).
:- mode hlds_pred__next_pred_id(in, out) is det.

:- pred hlds_pred__next_proc_id(proc_id, proc_id).
:- mode hlds_pred__next_proc_id(in, out) is det.

:- pred pred_id_to_int(pred_id, int).
:- mode pred_id_to_int(in, out) is det.
:- mode pred_id_to_int(out, in) is det.

:- pred proc_id_to_int(proc_id, int).
:- mode proc_id_to_int(in, out) is det.
:- mode proc_id_to_int(out, in) is det.

	% For semidet complicated unifications with mode (in, in),
	% these are defined to have the same proc_id (0).  This
	% returns that proc_id.

:- pred hlds_pred__in_in_unification_proc_id(proc_id).
:- mode hlds_pred__in_in_unification_proc_id(out) is det.

        % Return an invalid pred_id. Used to initialize the pred_id
        % in call(...) goals before we do typechecking or when type-checking
        % finds that there was no predicate which matched the call.

:- pred invalid_pred_id(pred_id).
:- mode invalid_pred_id(out) is det.

:- pred invalid_proc_id(proc_id).
:- mode invalid_proc_id(out) is det.

:- type pred_info.
:- type proc_info.

:- type proc_table	==	map(proc_id, proc_info).

:- type pred_call_id	--->	sym_name / arity.

:- type pred_proc_id	--->	proc(pred_id, proc_id).
:- type pred_proc_list	==	list(pred_proc_id).

	% The clauses_info structure contains the clauses for a predicate
	% after conversion from the item_list by make_hlds.m.
	% Typechecking is performed on the clauses info, then the clauses
	% are copied to create the proc_info for each procedure.
	% After mode analysis the clauses and the procedure goals are not
	% guaranteed to be the same, and the clauses are only kept so that
	% the optimized goal can be compared with the original in HLDS dumps.
:- type clauses_info	--->	clauses_info(
					varset,		% variable names
					map(var, type), % variable types from
						% explicit qualifications
					map(var, type), % variable types
						% inferred by typecheck.m.
					list(var),	% head vars
					list(clause)
				).

:- type clause		--->	clause(
					list(proc_id),	% modes for which
							% this clause applies
					hlds_goal,	% Body
					term__context
				).

	% The type of goals that have been given for a pred.

:- type goal_type 	--->	pragmas		% pragma c_code(...)
			;	clauses		
			;	none.

	% The evaluation method that should be used for a pred

:- type eval_method	--->	eval_normal		% normal mercury 
							% evaluation
			;	eval_loop_check		% loop check only
			;	eval_memo		% memoing + loop check 
			;	eval_minimal.		% minimal model 
							% evaluation 
							
	% Note: `liveness' and `liveness_info' record liveness in the sense
	% used by code generation.  This is *not* the same thing as the notion
	% of liveness used by mode analysis!  See compiler/notes/glossary.html.

:- type liveness_info	==	set(var).	% The live variables

:- type liveness	--->	live
			;	dead.

:- type arg_info	--->	arg_info(
					arg_loc,	% stored location
					arg_mode	% mode of top functor
				).

:- type arg_mode	--->	top_in
			;	top_out
			;	top_unused.

:- type arg_loc		==	int.

	% The type `import_status' describes whether an entity (a predicate,
	% type, inst, or mode) is local to the current module, exported from
	% the current module, or imported from some other module.
	% Only predicates can have status pseudo_exported or pseudo_imported.
	% Only types can have status abstract_exported or abstract_imported.

:- type import_status
	--->	imported	% defined in the interface of some other module
				% or `external' (in some other language)
	;	opt_imported	% defined in the optimization 
				% interface of another module
	;	abstract_imported % describes a type with only an abstract
				% declaration imported, maybe with the body
				% of the type imported from a .opt file
	;	pseudo_imported % this is used for entities that are defined
				% in the interface of some other module but
				% for which we may generate some code in
				% this module - in particular, this is used
				% for unification predicates (see comments in
				% unify_proc.m)
	;	exported	% defined in the interface of this module
	;	abstract_exported % describes a type with only an abstract
				% declaration exported
	;	pseudo_exported % the converse of pseudo_imported
				% this means that only the (in, in) mode
				% of a unification is exported
	;	local.		% defined in the implementation of this module

	% N-ary functions are converted into N+1-ary predicates.
	% (Clauses are converted in make_hlds, but calls to functions
	% cannot be converted until after type-checking, once we have
	% resolved overloading. So we do that during mode analysis.)
	% The `is_pred_or_func' field of the pred_info records whether
	% a pred_info is really for a predicate or whether it is for
	% what was originally a function.

:- type pred_or_func
	--->	predicate
	;	function.

	% Predicates can be marked with various boolean flags, called
	% "markers".

	% an abstract set of markers.
:- type pred_markers. 

:- type marker
	--->	infer_type	% Requests type inference for the predicate
				% These markers are inserted by make_hlds
				% for undeclared predicates.
	;	infer_modes	% Requests mode inference for the predicate
				% These markers are inserted by make_hlds
				% for undeclared predicates.
	;	obsolete	% Requests warnings if this predicate is used.
				% Used for pragma(obsolete).
	;	inline		% Requests that this predicate be inlined.
				% Used for pragma(inline).
	;	no_inline	% Requests that this be predicate not be 
				% inlined.
				% Used for pragma(no_inline).
				% Conflicts with `inline' marker.
	;	dnf		% Requests that this predicate be transformed
				% into disjunctive normal form.
	;	magic		% Requests that this predicate be transformed
				% using the magic set transformation
				% Used for pragma(memo).
				% Used for pragma(memo).
	;	class_method	% Requests that this predicate be transformed
				% into the appropriate call to a class method
	;	(impure)	% Requests that no transformation that would
				% be inappropriate for impure code be
				% performed on calls to this predicate.  This
				% includes reordering calls to it relative to
				% other goals (in both conjunctions and
				% disjunctions), and removing redundant calls
				% to it.
	;	(semipure)	% Requests that no transformation that would
				% be inappropriate for semipure code be
				% performed on calls to this predicate.  This
				% includes removing redundant calls to it on
				% different sides of an impure goal.
	;	promised_pure	% Requests that calls to this predicate be
				% transformed as usual, despite any impure
				% or semipure markers present.

				% The terminates and does_not_terminate
				% pragmas are kept as markers to ensure
				% that conflicting declarations are not
				% made by the user.  Otherwise, the
				% information could be added to the
				% ProcInfos directly.
	;	terminates	% The user guarantees that this predicate
				% will terminate for all (finite?) input
	;	does_not_terminate
				% States that this predicate does not
				% terminate.  This is useful for pragma
				% c_code, which the compiler assumes to be
				% terminating.
	;	check_termination
				% The user requires the compiler to guarantee
				% the termination of this predicate.
				% If the compiler cannot guarantee termination
				% then it must give an error message.
	.

:- type type_info_locn	
	--->	type_info(var)		% it is a normal type info 
					% (ie. the type is not constrained)
	;	typeclass_info(var, int).
					% it is packed inside a typeclass_info,
					% and is at the given offset

:- pred type_info_locn_var(type_info_locn::in, var::out) is det.

:- pred type_info_locn_set_var(type_info_locn::in, var::in, 
		type_info_locn::out) is det.

	% hlds_pred__define_new_pred(Goal, CallGoal, Args, InstMap, PredName,
	% 	TVarSet, VarTypes, ClassContext, TVarMap, TCVarMap, 
	%	VarSet, Markers, ModuleInfo0, ModuleInfo, PredProcId)
	%
	% Create a new predicate for the given goal, returning a goal to 
	% call the created predicate.
:- pred hlds_pred__define_new_pred(hlds_goal, hlds_goal, list(var),
		instmap, string, tvarset, map(var, type),
		list(class_constraint), map(tvar, type_info_locn),
		map(class_constraint, var), varset, pred_markers, 
		module_info, module_info, pred_proc_id).
:- mode hlds_pred__define_new_pred(in, out, in, in, in, in, in,
		in, in, in, in, in, in, out, out) is det.

	% Various predicates for accessing the information stored in the
	% pred_id and pred_info data structures.

:- pred pred_info_init(module_name, sym_name, arity, tvarset, list(type),
	condition, term__context, clauses_info, import_status,
	pred_markers, goal_type, pred_or_func, list(class_constraint), 
	map(class_constraint, constraint_proof), pred_info).
:- mode pred_info_init(in, in, in, in, in, in, in, in, in, in, in, in, in, in,
	out) is det.

:- pred pred_info_create(module_name, sym_name, tvarset, list(type),
	condition, term__context, import_status, pred_markers,
	pred_or_func, list(class_constraint), proc_info, proc_id, pred_info).
:- mode pred_info_create(in, in, in, in, in, in, in, in, in, in, in, out, out)
	is det.

:- pred pred_info_set(tvarset, list(type), condition, clauses_info, proc_table,
	term__context, module_name, string, arity, import_status,
	tvarset, goal_type, pred_markers, pred_or_func, 
	list(class_constraint), map(class_constraint, constraint_proof),
	pred_info).
:- mode pred_info_set(in, in, in, in, in, in, in, in, in, in, in, in, in, in, 
	in, in, out) is det.

:- pred pred_info_module(pred_info, module_name).
:- mode pred_info_module(in, out) is det.

:- pred pred_info_name(pred_info, string).
:- mode pred_info_name(in, out) is det.

	% pred_info_arity returns the arity of the predicate
	% *not* counting inserted type_info arguments for polymorphic preds.
:- pred pred_info_arity(pred_info, arity).
:- mode pred_info_arity(in, out) is det.

	% Return a list of all the proc_ids for the different modes
	% of this predicate.
:- pred pred_info_procids(pred_info, list(proc_id)).
:- mode pred_info_procids(in, out) is det.

	% Return a list of the proc_ids for all the modes
	% of this predicate that are not imported.
:- pred pred_info_non_imported_procids(pred_info, list(proc_id)).
:- mode pred_info_non_imported_procids(in, out) is det.

	% Return a list of the proc_ids for all the modes
	% of this predicate that are exported.
:- pred pred_info_exported_procids(pred_info, list(proc_id)).
:- mode pred_info_exported_procids(in, out) is det.

:- pred pred_info_arg_types(pred_info, tvarset, list(type)).
:- mode pred_info_arg_types(in, out, out) is det.

:- pred pred_info_set_arg_types(pred_info, tvarset, list(type), pred_info).
:- mode pred_info_set_arg_types(in, in, in, out) is det.

:- pred pred_info_clauses_info(pred_info, clauses_info).
:- mode pred_info_clauses_info(in, out) is det.

:- pred pred_info_set_clauses_info(pred_info, clauses_info, pred_info).
:- mode pred_info_set_clauses_info(in, in, out) is det.

:- pred pred_info_procedures(pred_info, proc_table).
:- mode pred_info_procedures(in, out) is det.

:- pred pred_info_set_procedures(pred_info, proc_table, pred_info).
:- mode pred_info_set_procedures(in, in, out) is det.

:- pred pred_info_context(pred_info, term__context).
:- mode pred_info_context(in, out) is det.

:- pred pred_info_import_status(pred_info::in, import_status::out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

	% Set the import_status of the predicate to `imported'.
	% This is used for `:- external(foo/2).' declarations.

:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

:- pred pred_info_set_import_status(pred_info::in, import_status::in,
				pred_info::out) is det.

:- pred pred_info_typevarset(pred_info, tvarset).
:- mode pred_info_typevarset(in, out) is det.

:- pred pred_info_set_typevarset(pred_info, tvarset, pred_info).
:- mode pred_info_set_typevarset(in, in, out) is det.

:- pred pred_info_get_goal_type(pred_info, goal_type).
:- mode pred_info_get_goal_type(in, out) is det.

:- pred pred_info_set_goal_type(pred_info, goal_type, pred_info).
:- mode pred_info_set_goal_type(in, in, out) is det.

	% Succeeds if there was a `:- pragma inline(...)' declaration
	% for this predicate. Note that the compiler may decide
	% to inline a predicate even if there was no pragma inline(...)
	% declaration for that predicate.

:- pred pred_info_requested_inlining(pred_info).
:- mode pred_info_requested_inlining(in) is semidet.

	% Succeeds if there was a `:- pragma no_inline(...)' declaration
	% for this predicate.

:- pred pred_info_requested_no_inlining(pred_info).
:- mode pred_info_requested_no_inlining(in) is semidet.

:- pred pred_info_get_is_pred_or_func(pred_info, pred_or_func).
:- mode pred_info_get_is_pred_or_func(in, out) is det.

:- pred pred_info_get_class_context(pred_info, list(class_constraint)).
:- mode pred_info_get_class_context(in, out) is det.

:- pred pred_info_set_class_context(pred_info, list(class_constraint), 
	pred_info).
:- mode pred_info_set_class_context(in, in, out) is det.

:- pred pred_info_get_constraint_proofs(pred_info, 
	map(class_constraint, constraint_proof)).
:- mode pred_info_get_constraint_proofs(in, out) is det.

:- pred pred_info_set_constraint_proofs(pred_info, 
	map(class_constraint, constraint_proof), pred_info).
:- mode pred_info_set_constraint_proofs(in, in, out) is det.

:- pred pred_info_get_purity(pred_info, purity).
:- mode pred_info_get_purity(in, out) is det.

:- pred pred_info_get_promised_pure(pred_info, bool).
:- mode pred_info_get_promised_pure(in, out) is det.

:- pred purity_to_markers(purity, pred_markers).
:- mode purity_to_markers(in, out) is det.

:- type pred_markers.

:- pred pred_info_get_markers(pred_info, pred_markers).
:- mode pred_info_get_markers(in, out) is det.

:- pred pred_info_set_markers(pred_info, pred_markers, pred_info).
:- mode pred_info_set_markers(in, in, out) is det.

	% create an empty set of markers
:- pred init_markers(pred_markers).
:- mode init_markers(out) is det.

	% check if a particular is in the set
:- pred check_marker(pred_markers, marker).
:- mode check_marker(in, in) is semidet.

	% add a marker to the set
:- pred add_marker(pred_markers, marker, pred_markers).
:- mode add_marker(in, in, out) is det.

	% convert the set to a list
:- pred markers_to_marker_list(pred_markers, list(marker)).
:- mode markers_to_marker_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- type pred_id		==	int.
:- type proc_id		==	int.

hlds_pred__initial_pred_id(0).

hlds_pred__initial_proc_id(0).

hlds_pred__next_pred_id(PredId, NextPredId) :-
	NextPredId is PredId + 1.

hlds_pred__next_proc_id(ProcId, NextProcId) :-
	NextProcId is ProcId + 1.

pred_id_to_int(PredId, PredId).

proc_id_to_int(ProcId, ProcId).

hlds_pred__in_in_unification_proc_id(0).

invalid_pred_id(-1).

invalid_proc_id(-1).

	% The information specific to a predicate, as opposed to a procedure.
	%
	% Any changes in this type definition will almost certainly require
	% corresponding changes in define.m.
	% XXX: This comment is either ancient, or prophesy. It seems
	% define.m doesn't exist yet.

:- type pred_info
	--->	predicate(
			tvarset,	% names of type vars
					% in the predicate's type decl
			list(type),	% argument types
			condition,	% formal specification
					% (not used)

			clauses_info,

			proc_table,

			term__context,	% the location (line #)
					% of the :- pred decl.

			module_name,	% module in which pred occurs
			string,		% predicate name
			arity,		% the arity of the pred
					% (*not* counting any inserted
					% type_info arguments)
			import_status,
			tvarset,	% names of type vars
					% in the predicate's type decl
					% or in the variable type assignments
			goal_type,	% whether the goals seen so far for
					% this pred are clauses, 
					% pragma c_code(...) decs, or none
			pred_markers,	% various boolean flags
			pred_or_func,	% whether this "predicate" was really
					% a predicate or a function
			list(class_constraint),
					% the class constraints on the 
					% predicate
			map(class_constraint, constraint_proof)
					% explanations of how redundant
					% constraints were eliminated. These
					% are needed by polymorphism.m to
					% work out where to get the
					% typeclass_infos from.
		).

pred_info_init(ModuleName, SymName, Arity, TypeVarSet, Types, Cond, Context,
		ClausesInfo, Status, Markers, GoalType, PredOrFunc, 
		ClassContext, ClassProofs, PredInfo) :-
	map__init(Procs),
	unqualify_name(SymName, PredName),
	sym_name_get_module_name(SymName, ModuleName, PredModuleName),
	PredInfo = predicate(TypeVarSet, Types, Cond, ClausesInfo, Procs,
		Context, PredModuleName, PredName, Arity, Status, TypeVarSet, 
		GoalType, Markers, PredOrFunc, ClassContext, ClassProofs).

pred_info_create(ModuleName, SymName, TypeVarSet, Types, Cond, Context,
		Status, Markers, PredOrFunc, ClassContext, ProcInfo, ProcId,
		PredInfo) :-
	map__init(Procs0),
	proc_info_declared_determinism(ProcInfo, MaybeDetism),
	next_mode_id(Procs0, MaybeDetism, ProcId),
	map__det_insert(Procs0, ProcId, ProcInfo, Procs),
	list__length(Types, Arity),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_headvars(ProcInfo, HeadVars),
	unqualify_name(SymName, PredName),
	% The empty list of clauses is a little white lie.
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes, HeadVars, []),
	map__init(ClassProofs),
	PredInfo = predicate(TypeVarSet, Types, Cond, ClausesInfo, Procs,
		Context, ModuleName, PredName, Arity, Status, TypeVarSet, 
		clauses, Markers, PredOrFunc, ClassContext, ClassProofs).

pred_info_set(HeadTVarSet, Types, Cond, ClausesInfo, Procs, Context,
		PredModuleName, PredName, Arity, Status, AllTVarSet,
		GoalType, Markers, PredOrFunc, ClassContext, ClassProofs,
		PredInfo) :-
	PredInfo = predicate(HeadTVarSet, Types, Cond, ClausesInfo, Procs,
		Context, PredModuleName, PredName, Arity, Status, AllTVarSet, 
		GoalType, Markers, PredOrFunc, ClassContext, ClassProofs).

pred_info_procids(PredInfo, ProcIds) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _, _, 
		_, _, _, _),
	map__keys(Procs, ProcIds).

pred_info_non_imported_procids(PredInfo, ProcIds) :-
	pred_info_import_status(PredInfo, ImportStatus),
	( ImportStatus = imported ->
		ProcIds = []
	; ImportStatus = pseudo_imported ->
		pred_info_procids(PredInfo, ProcIds0),
		% for pseduo_imported preds, procid 0 is imported
		list__delete_all(ProcIds0, 0, ProcIds)
	;
		pred_info_procids(PredInfo, ProcIds)
	).

pred_info_exported_procids(PredInfo, ProcIds) :-
	pred_info_import_status(PredInfo, ImportStatus),
	( ImportStatus = exported ->
		pred_info_procids(PredInfo, ProcIds)
	; ImportStatus = pseudo_exported ->
		ProcIds = [0]
	;
		ProcIds = []
	).

pred_info_clauses_info(PredInfo, Clauses) :-
	PredInfo = predicate(_, _, _, Clauses, _, _, _, _, _, _, _, _,
		_, _, _, _).

pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo) :-
	PredInfo0 = predicate(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P),
	PredInfo = predicate(A, B, C, Clauses, E, F, G, H, I, J, K, 
		L, M, N, O, P).

pred_info_arg_types(PredInfo, TypeVars, ArgTypes) :-
	PredInfo = predicate(TypeVars, ArgTypes, 
		_, _, _, _, _, _, _, _, _, _, _, _, _, _).

pred_info_set_arg_types(PredInfo0, TypeVarSet, ArgTypes, PredInfo) :-
	PredInfo0 = predicate(_, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P),
	PredInfo = predicate(TypeVarSet, ArgTypes, 
			C, D, E, F, G, H, I, J, K, L, M, N, O, P).

pred_info_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _, 
		_, _, _, _, _).

pred_info_set_procedures(PredInfo0, Procedures, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P),
	PredInfo = predicate(A, B, C, D, Procedures, F, G, H, I, J, K, L, M, 
		N, O, P).

pred_info_context(PredInfo, Context) :-
	PredInfo = predicate(_, _, _, _, _, Context, _, _, _, 
		_, _, _, _, _, _, _).

pred_info_module(PredInfo, Module) :-
	PredInfo = predicate(_, _, _, _, _, _, Module, _, _, _, _, 
		_, _, _, _, _).

pred_info_name(PredInfo, PredName) :-
	PredInfo = predicate(_, _, _, _, _, _, _, PredName, _, _, _, 
		_, _, _, _, _).

pred_info_arity(PredInfo, Arity) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, Arity, _, _, 
		_, _, _, _, _).

pred_info_import_status(PredInfo, ImportStatus) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, ImportStatus, _, _, _,
				_, _, _).

pred_info_is_imported(PredInfo) :-
	pred_info_import_status(PredInfo, imported).

pred_info_is_pseudo_imported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = pseudo_imported.

pred_info_is_exported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = exported.

pred_info_is_pseudo_exported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = pseudo_exported.

pred_info_mark_as_external(PredInfo0, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, imported, K, L, M, 
		N, O, P).

pred_info_set_import_status(PredInfo0, Status, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, Status, K, 
		L, M, N, O, P).

pred_info_typevarset(PredInfo, TypeVarSet) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, TypeVarSet, _, _, 
		_, _, _).

pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, TypeVarSet, L, M,
				N, O, P).

pred_info_get_goal_type(PredInfo, GoalType) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, GoalType, _, 
		_, _, _).

pred_info_set_goal_type(PredInfo0, GoalType, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, P),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, GoalType, M, 
		N, O, P).

pred_info_requested_inlining(PredInfo0) :-
	pred_info_get_markers(PredInfo0, Markers),
	check_marker(Markers, inline).

pred_info_requested_no_inlining(PredInfo0) :-
	pred_info_get_markers(PredInfo0, Markers),
	check_marker(Markers, no_inline).

pred_info_get_purity(PredInfo0, Purity) :-
	pred_info_get_markers(PredInfo0, Markers),
	(   check_marker(Markers, (impure)) ->
		Purity = (impure)
	;   check_marker(Markers, (semipure)) ->
		Purity = (semipure)
	;
		Purity = pure
	).

pred_info_get_promised_pure(PredInfo0, Promised) :-
	pred_info_get_markers(PredInfo0, Markers),
	(   check_marker(Markers, promised_pure) ->
		Promised = yes
	;
		Promised = no
	).

purity_to_markers(pure, []).
purity_to_markers(semipure, [semipure]).
purity_to_markers(impure, [impure]).

pred_info_get_markers(PredInfo, Markers) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, Markers, 
		_, _, _).

pred_info_set_markers(PredInfo0, Markers, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, Markers, 
		N, O, P).

pred_info_get_is_pred_or_func(PredInfo, IsPredOrFunc) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _,
			IsPredOrFunc, _, _).

pred_info_set_class_context(PredInfo0, ClassContext, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, P),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		ClassContext, P).

pred_info_get_class_context(PredInfo, ClassContext) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		ClassContext, _).

pred_info_set_constraint_proofs(PredInfo0, Proofs, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, Proofs).

pred_info_get_constraint_proofs(PredInfo, ConstraintProofs) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		ConstraintProofs).

%-----------------------------------------------------------------------------%

type_info_locn_var(type_info(Var), Var).
type_info_locn_var(typeclass_info(Var, _), Var).

type_info_locn_set_var(type_info(_), Var, type_info(Var)).
type_info_locn_set_var(typeclass_info(_, Num), Var, typeclass_info(Var, Num)).

%-----------------------------------------------------------------------------%
:- type pred_markers == list(marker).

init_markers([]).

check_marker(Markers, Marker) :-
	list__member(Marker, Markers).

add_marker(Markers, Marker, [Marker | Markers]).

markers_to_marker_list(Markers, Markers).

%-----------------------------------------------------------------------------%

hlds_pred__define_new_pred(Goal0, Goal, ArgVars0, InstMap0, PredName, TVarSet, 
		VarTypes0, ClassContext, TVarMap, TCVarMap, VarSet0, 
		Markers, ModuleInfo0, ModuleInfo, PredProcId) :-
	Goal0 = _GoalExpr - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

	% If typeinfo_liveness is set, all type_infos for the argument
	% variables need to be passed in, not just the ones that are used.
	module_info_globals(ModuleInfo0, Globals),
	globals__lookup_bool_option(Globals, typeinfo_liveness,
		TypeInfoLiveness),
	( TypeInfoLiveness = yes ->
		goal_util__extra_nonlocal_typeinfos(TVarMap, VarTypes0,
			Goal0, ExtraTypeInfos0),
		set__delete_list(ExtraTypeInfos0, ArgVars0, ExtraTypeInfos),
		set__to_sorted_list(ExtraTypeInfos, ExtraArgs),
		list__append(ExtraArgs, ArgVars0, ArgVars)
	;
		ArgVars = ArgVars0
	),

	goal_info_get_context(GoalInfo, Context),
	goal_info_get_determinism(GoalInfo, Detism),
	compute_arg_types_modes(ArgVars, VarTypes0, InstMap0, InstMap,
		ArgTypes, ArgModes),

	module_info_name(ModuleInfo0, ModuleName),
	SymName = qualified(ModuleName, PredName),

		% Remove unneeded variables from the vartypes and varset.
	goal_util__goal_vars(Goal0, GoalVars0), 
	set__insert_list(GoalVars0, ArgVars, GoalVars),
	map__select(VarTypes0, GoalVars, VarTypes),
	varset__select(VarSet0, GoalVars, VarSet),

		% Approximate the termination information 
		% for the new procedure.
	( code_aux__goal_cannot_loop(ModuleInfo0, Goal0) ->
		TermInfo = yes(cannot_loop)
	;
		TermInfo = no
	),

	globals__get_args_method(Globals, ArgsMethod),

	proc_info_create(VarSet, VarTypes, ArgVars, ArgModes, Detism,
		Goal0, Context, TVarMap, TCVarMap, ArgsMethod, ProcInfo0),
	proc_info_set_maybe_termination_info(ProcInfo0, TermInfo, ProcInfo),

	pred_info_create(ModuleName, SymName, TVarSet, ArgTypes, true,
		Context, local, Markers, predicate, ClassContext, 
		ProcInfo, ProcId, PredInfo),

	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, PredInfo, PredId,
		PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable,
		ModuleInfo),

	GoalExpr = call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
	Goal = GoalExpr - GoalInfo,
	PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(list(var)::in, map(var, type)::in,
	instmap::in, instmap::in, list(type)::out, list(mode)::out) is det.

compute_arg_types_modes([], _, _, _, [], []).
compute_arg_types_modes([Var | Vars], VarTypes, InstMap0, InstMap,
		[Type | Types], [Mode | Modes]) :-
	map__lookup(VarTypes, Var, Type),
	instmap__lookup_var(InstMap0, Var, Inst0),
	instmap__lookup_var(InstMap, Var, Inst),
	Mode = (Inst0 -> Inst),
	compute_arg_types_modes(Vars, VarTypes, InstMap0, InstMap,
		Types, Modes).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the proc_info data structure.

:- interface.

:- pred proc_info_init(arity, list(mode), maybe(list(mode)),
	maybe(list(is_live)), maybe(determinism), term__context, 
	args_method, proc_info).
:- mode proc_info_init(in, in, in, in, in, in, in, out) is det.

:- pred proc_info_set(maybe(determinism), varset, map(var, type), list(var),
	list(mode), maybe(list(is_live)), hlds_goal, term__context,
	stack_slots, determinism, bool, list(arg_info), liveness_info,
	map(tvar, type_info_locn), map(class_constraint, var), 
	maybe(arg_size_info), maybe(termination_info), args_method, proc_info).
:- mode proc_info_set(in, in, in, in, in, in, in, in, in, in, in, in, in, in,
	in, in, in, in, out) is det.

:- pred proc_info_create(varset, map(var, type), list(var), list(mode),
	determinism, hlds_goal, term__context, map(tvar, type_info_locn),
	map(class_constraint, var), args_method, proc_info).
:- mode proc_info_create(in, in, in, in, in, in, in, in, in, in, out) is det.

:- pred proc_info_set_body(proc_info, varset, map(var, type), list(var),
	hlds_goal, proc_info).
:- mode proc_info_set_body(in, in, in, in, in, out) is det.

:- pred proc_info_declared_determinism(proc_info, maybe(determinism)).
:- mode proc_info_declared_determinism(in, out) is det.

:- pred proc_info_inferred_determinism(proc_info, determinism).
:- mode proc_info_inferred_determinism(in, out) is det.

:- pred proc_info_interface_determinism(proc_info, determinism).
:- mode proc_info_interface_determinism(in, out) is det.

:- pred proc_info_interface_code_model(proc_info, code_model).
:- mode proc_info_interface_code_model(in, out) is det.

	% proc_info_never_succeeds(ProcInfo, Result):
	% return Result = yes if the procedure is known to never succeed
	% according to the declared determinism.
	%
:- pred proc_info_never_succeeds(proc_info, bool).
:- mode proc_info_never_succeeds(in, out) is det.

:- pred proc_info_varset(proc_info, varset).
:- mode proc_info_varset(in, out) is det.

:- pred proc_info_set_varset(proc_info, varset, proc_info).
:- mode proc_info_set_varset(in, in, out) is det.

:- pred proc_info_vartypes(proc_info, map(var, type)).
:- mode proc_info_vartypes(in, out) is det.

:- pred proc_info_set_vartypes(proc_info, map(var, type), proc_info).
:- mode proc_info_set_vartypes(in, in, out) is det.

:- pred proc_info_headvars(proc_info, list(var)).
:- mode proc_info_headvars(in, out) is det.

:- pred proc_info_set_headvars(proc_info, list(var), proc_info).
:- mode proc_info_set_headvars(in, in, out) is det.

:- pred proc_info_argmodes(proc_info, list(mode)).
:- mode proc_info_argmodes(in, out) is det.

:- pred proc_info_set_argmodes(proc_info, list(mode), proc_info).
:- mode proc_info_set_argmodes(in, in, out) is det.

:- pred proc_info_arglives(proc_info, module_info, list(is_live)).
:- mode proc_info_arglives(in, in, out) is det.

:- pred proc_info_maybe_arglives(proc_info, maybe(list(is_live))).
:- mode proc_info_maybe_arglives(in, out) is det.

:- pred proc_info_set_maybe_arglives(proc_info, maybe(list(is_live)),
					proc_info).
:- mode proc_info_set_maybe_arglives(in, in, out) is det.

:- pred proc_info_goal(proc_info, hlds_goal).
:- mode proc_info_goal(in, out) is det.

:- pred proc_info_context(proc_info, term__context).
:- mode proc_info_context(in, out) is det.

:- pred proc_info_stack_slots(proc_info, stack_slots).
:- mode proc_info_stack_slots(in, out) is det.

:- pred proc_info_liveness_info(proc_info, liveness_info).
:- mode proc_info_liveness_info(in, out) is det.

:- pred proc_info_can_process(proc_info, bool).
:- mode proc_info_can_process(in, out) is det.

:- pred proc_info_set_inferred_determinism(proc_info, determinism, proc_info).
:- mode proc_info_set_inferred_determinism(in, in, out) is det.

:- pred proc_info_set_goal(proc_info, hlds_goal, proc_info).
:- mode proc_info_set_goal(in, in, out) is det.

:- pred proc_info_arg_info(proc_info, list(arg_info)).
:- mode proc_info_arg_info(in, out) is det.

:- pred proc_info_set_arg_info(proc_info, list(arg_info), proc_info).
:- mode proc_info_set_arg_info(in, in, out) is det.

:- pred proc_info_get_initial_instmap(proc_info, module_info, instmap).
:- mode proc_info_get_initial_instmap(in, in, out) is det.

:- pred proc_info_set_liveness_info(proc_info, liveness_info, proc_info).
:- mode proc_info_set_liveness_info(in, in, out) is det.

:- pred proc_info_set_stack_slots(proc_info, stack_slots, proc_info).
:- mode proc_info_set_stack_slots(in, in, out) is det.

:- pred proc_info_get_maybe_arg_size_info(proc_info, maybe(arg_size_info)).
:- mode proc_info_get_maybe_arg_size_info(in, out) is det.

:- pred proc_info_set_maybe_arg_size_info(proc_info, maybe(arg_size_info),
	proc_info).
:- mode proc_info_set_maybe_arg_size_info(in, in, out) is det.

:- pred proc_info_get_maybe_termination_info(proc_info,
	maybe(termination_info)).
:- mode proc_info_get_maybe_termination_info(in, out) is det.

:- pred proc_info_set_maybe_termination_info(proc_info,
	maybe(termination_info), proc_info).
:- mode proc_info_set_maybe_termination_info(in, in, out) is det.

:- pred proc_info_set_can_process(proc_info, bool, proc_info).
:- mode proc_info_set_can_process(in, in, out) is det.

:- pred proc_info_typeinfo_varmap(proc_info, map(tvar, type_info_locn)).
:- mode proc_info_typeinfo_varmap(in, out) is det.

:- pred proc_info_set_typeinfo_varmap(proc_info, map(tvar, type_info_locn),
	proc_info).
:- mode proc_info_set_typeinfo_varmap(in, in, out) is det.

:- pred proc_info_eval_method(proc_info, eval_method).
:- mode proc_info_eval_method(in, out) is det.

:- pred proc_info_set_eval_method(proc_info, eval_method, proc_info).
:- mode proc_info_set_eval_method(in, in, out) is det.

:- pred proc_info_typeclass_info_varmap(proc_info, map(class_constraint, var)).
:- mode proc_info_typeclass_info_varmap(in, out) is det.

:- pred proc_info_set_typeclass_info_varmap(proc_info, 
	map(class_constraint, var), proc_info).
:- mode proc_info_set_typeclass_info_varmap(in, in, out) is det.

:- pred proc_info_maybe_declared_argmodes(proc_info, maybe(list(mode))).
:- mode proc_info_maybe_declared_argmodes(in, out) is det.

:- pred proc_info_declared_argmodes(proc_info, list(mode)).
:- mode proc_info_declared_argmodes(in, out) is det.

:- pred proc_info_args_method(proc_info, args_method).
:- mode proc_info_args_method(in, out) is det.

:- pred proc_info_set_args_method(proc_info, args_method, proc_info).
:- mode proc_info_set_args_method(in, in, out) is det.

	% For a set of variables V, find all the type variables in the types 
	% of the variables in V, and return set of typeinfo variables for 
	% those type variables. (find all typeinfos for variables in V).
	%
	% This set of typeinfos is often needed in liveness computation
	% for accurate garbage collection - live variables need to have 
	% their typeinfos stay live too.

:- pred proc_info_get_typeinfo_vars_setwise(proc_info, set(var), set(var)).
:- mode proc_info_get_typeinfo_vars_setwise(in, in, out) is det.

:- pred proc_info_ensure_unique_names(proc_info, proc_info).
:- mode proc_info_ensure_unique_names(in, out) is det.

	% Create a new variable of the given type to the procedure.
:- pred proc_info_create_var_from_type(proc_info, type, var, proc_info).
:- mode proc_info_create_var_from_type(in, in, out, out) is det.

	% Create a new variable for each element of the list of types.
:- pred proc_info_create_vars_from_types(proc_info, 
		list(type), list(var), proc_info).
:- mode proc_info_create_vars_from_types(in, in, out, out) is det.

:- implementation.

:- type proc_info
	--->	procedure(
			maybe(determinism),
					% _declared_ determinism
					% or `no' if there was no detism decl
			varset,		% variable names
			map(var, type),	% variable types
			list(var),	% head vars
			list(mode), 	% modes of args
			maybe(list(is_live)),
					% liveness (in the mode analysis sense)
					% of the arguments
			hlds_goal,	% Body
			term__context,	% The context of the `:- mode' decl
					% (or the context of the first clause,
					% if there was no mode declaration).
			stack_slots,	% stack allocations
			determinism,	% _inferred_ determinism
			bool,		% no if we must not process this
					% procedure yet (used to delay
					% mode checking etc. for complicated
					% modes of unification procs until
					% the end of the unique_modes pass.)
			list(arg_info),	% calling convention of each arg:
					% information computed by arg_info.m
					% (based on the modes etc.)
					% and used by code generation
					% to determine how each argument
					% should be passed.
			liveness_info,	% the initial liveness,
					% for code generation
			map(tvar, type_info_locn),	
					% typeinfo vars for
					% type parameters
			map(class_constraint, var),
					% typeclass_info vars for class
					% constraints
			eval_method,	% how should the proc be evaluated	
			maybe(arg_size_info),
					% Information about the relative sizes
					% of the input and output args of the
					% procedure. Set by termination
					% analysis.
			maybe(termination_info),
					% The termination properties of the
					% procedure. Set by termination
					% analysis.
			maybe(list(mode)),
					% declared modes of arguments.
			args_method
					% The args_method to be used for
					% the procedure. Usually this will
					% be set to the value of the --args
					% option stored in the globals. 
					% lambda.m will set this field to
					% `compact' for procedures it creates
					% which must be directly callable by
					% a higher_order_call goal.
		).

	% Some parts of the procedure aren't known yet. We initialize
	% them to any old garbage which we will later throw away.

	% Inferred determinism gets initialized to `erroneous'.
	% This is what `det_analysis.m' wants. det_analysis.m
	% will later provide the correct inferred determinism for it.

proc_info_init(Arity, Modes, DeclaredModes, MaybeArgLives,
		MaybeDet, MContext, ArgsMethod, NewProc) :-
	map__init(BodyTypes),
	goal_info_init(GoalInfo),
	varset__init(BodyVarSet0),
	make_n_fresh_vars("HeadVar__", Arity, BodyVarSet0,
		HeadVars, BodyVarSet),
	InferredDet = erroneous,
	map__init(StackSlots),
	set__init(InitialLiveness),
	ArgInfo = [],
	ClauseBody = conj([]) - GoalInfo,
	CanProcess = yes,
	map__init(TVarsMap),
	map__init(TCVarsMap),
	NewProc = procedure(
		MaybeDet, BodyVarSet, BodyTypes, HeadVars, Modes, MaybeArgLives,
		ClauseBody, MContext, StackSlots, InferredDet, CanProcess,
		ArgInfo, InitialLiveness, TVarsMap, TCVarsMap, eval_normal,
		no, no, DeclaredModes, ArgsMethod
	).

proc_info_set(DeclaredDetism, BodyVarSet, BodyTypes, HeadVars, HeadModes,
		HeadLives, Goal, Context, StackSlots, InferredDetism,
		CanProcess, ArgInfo, Liveness, TVarMap, TCVarsMap,
		ArgSizes, Termination, ArgsMethod, ProcInfo) :-
	ProcInfo = procedure(
		DeclaredDetism, BodyVarSet, BodyTypes, HeadVars, HeadModes,
		HeadLives, Goal, Context, StackSlots, InferredDetism,
		CanProcess, ArgInfo, Liveness, TVarMap, TCVarsMap, eval_normal, 
		ArgSizes, Termination, no, ArgsMethod).

proc_info_create(VarSet, VarTypes, HeadVars, HeadModes, Detism, Goal,
		Context, TVarMap, TCVarsMap, ArgsMethod, ProcInfo) :-
	map__init(StackSlots),
	set__init(Liveness),
	MaybeHeadLives = no,
	ProcInfo = procedure(yes(Detism), VarSet, VarTypes, HeadVars, HeadModes,
		MaybeHeadLives, Goal, Context, StackSlots, Detism, yes, [],
		Liveness, TVarMap, TCVarsMap, eval_normal, no, no, no, 
			ArgsMethod).

proc_info_set_body(ProcInfo0, VarSet, VarTypes, HeadVars, Goal, ProcInfo) :-
	ProcInfo0 = procedure(A, _, _, _, E, F, _,
		H, I, J, K, L, M, N, O, P, Q, R, S, T),
	ProcInfo = procedure(A, VarSet, VarTypes, HeadVars, E, F, Goal,
		H, I, J, K, L, M, N, O, P, Q, R, S, T).

proc_info_interface_determinism(ProcInfo, Determinism) :-
	proc_info_declared_determinism(ProcInfo, MaybeDeterminism),
	(
		MaybeDeterminism = no,
		proc_info_inferred_determinism(ProcInfo, Determinism)
	;
		MaybeDeterminism = yes(Determinism)
	).

proc_info_interface_code_model(ProcInfo, CodeModel) :-
	proc_info_interface_determinism(ProcInfo, Determinism),
	determinism_to_code_model(Determinism, CodeModel).

	% Return Result = yes if the called predicate is known to never succeed.
	%
proc_info_never_succeeds(ProcInfo, Result) :-
	proc_info_declared_determinism(ProcInfo, DeclaredDeterminism),
	(
		DeclaredDeterminism = no,
		Result = no
	;
		DeclaredDeterminism = yes(Determinism),
		determinism_components(Determinism, _, HowMany),
		( HowMany = at_most_zero ->
			Result = yes
		;
			Result = no
		)
	).

proc_info_arglives(ProcInfo, ModuleInfo, ArgLives) :-
	proc_info_maybe_arglives(ProcInfo, MaybeArgLives),
	( MaybeArgLives = yes(ArgLives0) ->
		ArgLives = ArgLives0
	;
		proc_info_argmodes(ProcInfo, Modes),
		get_arg_lives(Modes, ModuleInfo, ArgLives)
	).

proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_argmodes(ProcInfo, ArgModes),
	mode_list_get_initial_insts(ArgModes, ModuleInfo, InitialInsts),
	assoc_list__from_corresponding_lists(HeadVars, InitialInsts, InstAL),
	instmap__from_assoc_list(InstAL, InstMap).

proc_info_declared_argmodes(ProcInfo, ArgModes) :-
	proc_info_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
	( MaybeArgModes = yes(ArgModes1) ->
		ArgModes = ArgModes1
	;
		proc_info_argmodes(ProcInfo, ArgModes)
	).

proc_info_declared_determinism(ProcInfo, A) :-
	ProcInfo = procedure(A, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
    		_, _, _, _).

proc_info_varset(ProcInfo, B) :-
	ProcInfo = procedure(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_vartypes(ProcInfo, C) :-
	ProcInfo = procedure(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_headvars(ProcInfo, D) :-
	ProcInfo = procedure(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_argmodes(ProcInfo, E) :-
	ProcInfo = procedure(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_maybe_arglives(ProcInfo, F) :-
	ProcInfo = procedure(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_goal(ProcInfo, G) :-
	ProcInfo = procedure(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_context(ProcInfo, H) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_stack_slots(ProcInfo, I) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_inferred_determinism(ProcInfo, J) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, 
		_, _, _, _).

proc_info_can_process(ProcInfo, K) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, 
		_, _, _, _).

proc_info_arg_info(ProcInfo, L) :- 
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, 
		_, _, _, _).

proc_info_liveness_info(ProcInfo, M) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _,
		_, _, _, _).

proc_info_typeinfo_varmap(ProcInfo, N) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, 
		_, _, _, _).

proc_info_typeclass_info_varmap(ProcInfo, O) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _, 
		_, _, _, _).

proc_info_eval_method(ProcInfo, P) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, 
		_, _, _, _).

proc_info_get_maybe_arg_size_info(ProcInfo, Q) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		Q, _, _, _).

proc_info_get_maybe_termination_info(ProcInfo, R) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, R, _, _).

proc_info_maybe_declared_argmodes(ProcInfo, S) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, S, _).

proc_info_args_method(ProcInfo, T) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, T).

% :- type proc_info
% 	--->	procedure(
% A			maybe(determinism),
% 					% _declared_ determinism
% 					% or `no' if there was no detism decl
% B			varset,		% variable names
% C 			map(var, type),	% variable types
% D			list(var),	% head vars
% E			list(mode), 	% modes of args
% F			maybe(list(is_live)),
% 					% liveness (in the mode analysis sense)
% 					% of the arguments
% G			hlds_goal,	% Body
% H			term__context,	% The context of the `:- mode' decl
% 					% (or the context of the first clause,
% 					% if there was no mode declaration).
% I			stack_slots,	% stack allocations
% J			determinism,	% _inferred_ determinism
% K			bool,		% no if we must not process this
% 					% procedure yet (used to delay
% 					% mode checking etc. for complicated
% 					% modes of unification procs until
% 					% the end of the unique_modes pass.)
% L			list(arg_info),	% calling convention of each arg:
% 					% information computed by arg_info.m
% 					% (based on the modes etc.)
% 					% and used by code generation
% 					% to determine how each argument
% 					% should be passed.
% M			liveness_info,	% the initial liveness,
% 					% for code generation
% N			map(tvar, type_info_locn),	
% 					% typeinfo vars for
% 					% type parameters
% O			map(class_constraint, var),
% 					% typeclass_info vars for class
% 					% constraints
% P			eval_method,
%					% info on how the proc sould be 
%					% evaluated
% Q			maybe(arg_size_info),
% 					% Information about the relative sizes
% 					% of the input and output args of the
% 					% procedure. Set by termination
% 					% analysis.
% R			maybe(termination_info),
% 					% The termination properties of the
% 					% procedure. Set by termination
% 					% analysis.
% S			maybe(list(mode))
% 					% declared modes of arguments.
% T			args_method
% 					% The args_method to be used for
%					% the procedure. Usually this will
%					% be set to the value of the --args
%					% option stored in the globals. 
%					% lambda.m will set this field to
%					% `compact' for procedures it creates
%					% which must be directly callable by
%					% a higher_order_call goal.	
%		).

proc_info_set_varset(ProcInfo0, B, ProcInfo) :-
	ProcInfo0 = procedure(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_vartypes(ProcInfo0, C, ProcInfo) :-
	ProcInfo0 = procedure(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_headvars(ProcInfo0, D, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_argmodes(ProcInfo0, E, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_maybe_arglives(ProcInfo0, F, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_goal(ProcInfo0, G, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_stack_slots(ProcInfo0, I, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, _, J, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_inferred_determinism(ProcInfo0, J, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_can_process(ProcInfo0, K, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_arg_info(ProcInfo0, L, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_liveness_info(ProcInfo0, M, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_typeinfo_varmap(ProcInfo0, N, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_typeclass_info_varmap(ProcInfo0, O, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, 
		P, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_eval_method(ProcInfo0, P, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
		_, Q, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
		P, Q, R, S, T).

proc_info_set_maybe_arg_size_info(ProcInfo0, Q, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, _, R, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_maybe_termination_info(ProcInfo0, R, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, _, S, T),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_set_args_method(ProcInfo0, T, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, _),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T).

proc_info_get_typeinfo_vars_setwise(ProcInfo, Vars, TypeInfoVars) :-
	set__to_sorted_list(Vars, VarList),
	proc_info_get_typeinfo_vars_2(ProcInfo, VarList, TypeInfoVarList),
	set__list_to_set(TypeInfoVarList, TypeInfoVars).

	% auxiliary predicate - traverses variables and builds a list of
	% variables that store typeinfos for these variables. 
:- pred proc_info_get_typeinfo_vars_2(proc_info, list(var), list(var)).
:- mode proc_info_get_typeinfo_vars_2(in, in, out) is det.

proc_info_get_typeinfo_vars_2(_, [], []).
proc_info_get_typeinfo_vars_2(ProcInfo, [Var | Vars1], TypeInfoVars) :-
	proc_info_vartypes(ProcInfo, VarTypeMap),
	( 
		map__search(VarTypeMap, Var, Type)
	->
		type_util__vars(Type, TypeVars),
		(
			% Optimize common case
			TypeVars = []
		->
			proc_info_get_typeinfo_vars_2(ProcInfo, Vars1, 
				TypeInfoVars)
		;
			% XXX It's possible there are some complications with
			% higher order pred types here -- if so, maybe
			% treat them specially.
			proc_info_typeinfo_varmap(ProcInfo, TVarMap),

				% The type_info is either stored in a variable,
				% or in a typeclass_info. Either get the
				% type_info variable or the typeclass_info
				% variable
			LookupVar = lambda([TVar::in, TVarVar::out] is det,
				(
					map__lookup(TVarMap, TVar, Locn),
					type_info_locn_var(Locn, TVarVar)
				)),
			list__map(LookupVar, TypeVars, TypeInfoVars0),

			proc_info_get_typeinfo_vars_2(ProcInfo, Vars1,
				TypeInfoVars1),
			list__append(TypeInfoVars0, TypeInfoVars1, TypeInfoVars)
		)
	;
		error("proc_info_get_typeinfo_vars_2: var not found in typemap")
	).

proc_info_ensure_unique_names(ProcInfo0, ProcInfo) :-
	proc_info_vartypes(ProcInfo0, VarTypes),
	map__keys(VarTypes, AllVars),
	proc_info_varset(ProcInfo0, VarSet0),
	varset__ensure_unique_names(AllVars, "p", VarSet0, VarSet),
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo).

proc_info_create_var_from_type(ProcInfo0, Type, NewVar, ProcInfo) :-
	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	varset__new_var(VarSet0, NewVar, VarSet),
	map__det_insert(VarTypes0, NewVar, Type, VarTypes),
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo).

proc_info_create_vars_from_types(ProcInfo0, Types, NewVars, ProcInfo) :-
	list__length(Types, NumVars),
	proc_info_varset(ProcInfo0, VarSet0),
	proc_info_vartypes(ProcInfo0, VarTypes0),
	varset__new_vars(VarSet0, NumVars, NewVars, VarSet),
	map__det_insert_from_corresponding_lists(VarTypes0, 
		NewVars, Types, VarTypes),
	proc_info_set_varset(ProcInfo0, VarSet, ProcInfo1),
	proc_info_set_vartypes(ProcInfo1, VarTypes, ProcInfo).

%-----------------------------------------------------------------------------%

:- interface.

	% make_n_fresh_vars(Name, N, VarSet0, Vars, VarSet):
	%	`Vars' is a list of `N' fresh variables allocated from
	%	`VarSet0'.  The variables will be named "<Name>1", "<Name>2",
	%	"<Name>3", and so on, where <Name> is the value of `Name'.
	%	`VarSet' is the resulting varset.

:- pred make_n_fresh_vars(string, int, varset, list(var), varset).
:- mode make_n_fresh_vars(in, in, in, out, out) is det.

	% given the list of predicate arguments for a predicate that
	% is really a function, split that list into the function arguments
	% and the function return type.
:- pred pred_args_to_func_args(list(T), list(T), T).
:- mode pred_args_to_func_args(in, out, out) is det.

:- implementation.

make_n_fresh_vars(BaseName, N, VarSet0, Vars, VarSet) :-
	make_n_fresh_vars_2(BaseName, 0, N, VarSet0, Vars, VarSet).

:- pred make_n_fresh_vars_2(string, int, int, varset, list(var), varset).
:- mode make_n_fresh_vars_2(in, in, in, in, out, out) is det.

make_n_fresh_vars_2(BaseName, N, Max, VarSet0, Vars, VarSet) :-
	(N = Max ->
		VarSet = VarSet0,
		Vars = []
	;
		N1 is N + 1,
		varset__new_var(VarSet0, Var, VarSet1),
		string__int_to_string(N1, Num),
		string__append(BaseName, Num, VarName),
		varset__name_var(VarSet1, Var, VarName, VarSet2),
		Vars = [Var | Vars1],
		make_n_fresh_vars_2(BaseName, N1, Max, VarSet2, Vars1, VarSet)
	).

pred_args_to_func_args(PredArgs, FuncArgs, FuncReturn) :-
	list__length(PredArgs, NumPredArgs),
	NumFuncArgs is NumPredArgs - 1,
	( list__split_list(NumFuncArgs, PredArgs, FuncArgs0, [FuncReturn0]) ->
		FuncArgs = FuncArgs0,
		FuncReturn = FuncReturn0
	;
		error("pred_args_to_func_args: function missing return value?")
	).

%-----------------------------------------------------------------------------%

:- interface.

	% Check if the given evaluation method is allowed with
	% the given code model.
:- pred valid_code_model_for_eval_method(eval_method, code_model).
:- mode valid_code_model_for_eval_method(in, in) is semidet.
:- mode valid_code_model_for_eval_method(in, out) is multidet.

	% Convert an evaluation method to a string.
:- pred eval_method_to_string(eval_method, string).
:- mode eval_method_to_string(in, out) is det.

	% Return true if the given evaluation method requires a
	% stratification check.
:- pred eval_method_need_stratification(eval_method).
:- mode eval_method_need_stratification(in) is semidet.

	% Return the change a given evaluation method can do to a given 
	% determinism.
:- pred eval_method_change_determinism(eval_method, determinism, 
		determinism).
:- mode eval_method_change_determinism(in, in, out) is det.

:- implementation.

:- import_module det_analysis.

valid_code_model_for_eval_method(eval_normal, model_det).
valid_code_model_for_eval_method(eval_normal, model_semi).
valid_code_model_for_eval_method(eval_normal, model_non).
valid_code_model_for_eval_method(eval_memo, model_det).
valid_code_model_for_eval_method(eval_memo, model_semi).
valid_code_model_for_eval_method(eval_memo, model_non).
valid_code_model_for_eval_method(eval_loop_check, model_det).
valid_code_model_for_eval_method(eval_loop_check, model_semi).
valid_code_model_for_eval_method(eval_loop_check, model_non).
valid_code_model_for_eval_method(eval_minimal, model_semi).
valid_code_model_for_eval_method(eval_minimal, model_non).

eval_method_to_string(eval_normal,		"normal").
eval_method_to_string(eval_memo,		"memo").
eval_method_to_string(eval_loop_check,		"loop_check").
eval_method_to_string(eval_minimal, 		"minimal_model").
	
eval_method_need_stratification(eval_minimal).

eval_method_change_determinism(eval_normal, Detism, Detism).
eval_method_change_determinism(eval_memo, Detism, Detism).
eval_method_change_determinism(eval_loop_check, Detism, Detism).
eval_method_change_determinism(eval_minimal, Det0, Det) :-
	det_conjunction_detism(semidet, Det0, Det).

