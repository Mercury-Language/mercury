%-----------------------------------------------------------------------------%
% Copyright (C) 1996-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module defines the part of the HLDS that deals with predicates
% and procedures.

% Main authors: fjh, conway.

:- module hlds_pred.

:- interface.

:- import_module hlds_data, hlds_goal, hlds_module, llds, prog_data, instmap.
:- import_module globals, term_util.
:- import_module bool, list, set, map, std_util, term, varset.

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

:- type call_id
	--->	call(simple_call_id)
	;	generic_call(generic_call_id)
	.

:- type generic_call_id
	--->	higher_order(pred_or_func, arity)
	;	class_method(class_id, simple_call_id)
	;	aditi_builtin(aditi_builtin, simple_call_id)
	.

:- type simple_call_id == pair(pred_or_func, sym_name_and_arity).

:- type pred_proc_id	--->	proc(pred_id, proc_id).
:- type pred_proc_list	==	list(pred_proc_id).

%-----------------------------------------------------------------------------%
	
	% This is used for a closure executed top-down on the Aditi
	% side of the connection.
	% These expression numbers are stored in the proc_info - the owner
	% and module name from the pred_info are also required to completely
	% identify the expressions.
:- type rl_exprn_id == int.

%-----------------------------------------------------------------------------%

	% The clauses_info structure contains the clauses for a predicate
	% after conversion from the item_list by make_hlds.m.
	% Typechecking is performed on the clauses info, then the clauses
	% are copied to create the proc_info for each procedure.
	% After mode analysis the clauses and the procedure goals are not
	% guaranteed to be the same, and the clauses are only kept so that
	% the optimized goal can be compared with the original in HLDS dumps.
:- type clauses_info	--->	clauses_info(
					prog_varset,	% variable names
					vartypes,
						% variable types from
						% explicit qualifications
					vartypes,
						% variable types
						% inferred by typecheck.m.
					list(prog_var),	% head vars
					list(clause),
						% the following two fields
						% are computed by
						% polymorphism.m
					type_info_varmap,
					typeclass_info_varmap
				).

:- type vartypes == map(prog_var, type).

:- pred clauses_info_varset(clauses_info, prog_varset).
:- mode clauses_info_varset(in, out) is det.

	% This partial map holds the types specified by any explicit
	% type qualifiers in the clauses.
:- pred clauses_info_explicit_vartypes(clauses_info, vartypes).
:- mode clauses_info_explicit_vartypes(in, out) is det.

	% This map contains the types of all the variables, as inferred
	% by typecheck.m.
:- pred clauses_info_vartypes(clauses_info, vartypes).
:- mode clauses_info_vartypes(in, out) is det.

:- pred clauses_info_type_info_varmap(clauses_info, type_info_varmap).
:- mode clauses_info_type_info_varmap(in, out) is det.

:- pred clauses_info_typeclass_info_varmap(clauses_info,
				typeclass_info_varmap).
:- mode clauses_info_typeclass_info_varmap(in, out) is det.

:- pred clauses_info_headvars(clauses_info, list(prog_var)).
:- mode clauses_info_headvars(in, out) is det.

:- pred clauses_info_clauses(clauses_info, list(clause)).
:- mode clauses_info_clauses(in, out) is det.

:- pred clauses_info_set_headvars(clauses_info, list(prog_var), clauses_info).
:- mode clauses_info_set_headvars(in, in, out) is det.

:- pred clauses_info_set_clauses(clauses_info, list(clause), clauses_info).
:- mode clauses_info_set_clauses(in, in, out) is det.

:- pred clauses_info_set_varset(clauses_info, prog_varset, clauses_info).
:- mode clauses_info_set_varset(in, in, out) is det.

	% This partial map holds the types specified by any explicit
	% type qualifiers in the clauses.
:- pred clauses_info_set_explicit_vartypes(clauses_info, vartypes,
		clauses_info).
:- mode clauses_info_set_explicit_vartypes(in, in, out) is det.

	% This map contains the types of all the variables, as inferred
	% by typecheck.m.
:- pred clauses_info_set_vartypes(clauses_info, vartypes,
		clauses_info).
:- mode clauses_info_set_vartypes(in, in, out) is det.

:- pred clauses_info_set_type_info_varmap(clauses_info, type_info_varmap,
		clauses_info).
:- mode clauses_info_set_type_info_varmap(in, in, out) is det.

:- pred clauses_info_set_typeclass_info_varmap(clauses_info,
				typeclass_info_varmap, clauses_info).
:- mode clauses_info_set_typeclass_info_varmap(in, in, out) is det.


:- type clause		--->	clause(
					list(proc_id),	% modes for which
							% this clause applies
							% (empty list means
							% it applies to all
							% clauses)
					hlds_goal,	% Body
					prog_context
				).

%-----------------------------------------------------------------------------%

	% The type of goals that have been given for a pred.

:- type goal_type 	--->	pragmas		% pragma c_code(...)
			;	clauses		
			;	(assertion)
			;	none.

	% Note: `liveness' and `liveness_info' record liveness in the sense
	% used by code generation.  This is *not* the same thing as the notion
	% of liveness used by mode analysis!  See compiler/notes/glossary.html.

:- type liveness_info	==	set(prog_var).	% The live variables

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
	--->	imported(section)
				% defined in the interface of some other module
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
	;	exported_to_submodules
				% defined in the implementation of this module,
				% and thus in a sense local,
				% but the module contains sub-modules,
				% so the entity needs to be exported
				% to those sub-modules
	;	local.		% defined in the implementation of this module,
				% and the module does not contain any
				% sub-modules.

	% returns yes if the status indicates that the item was
	% in any way exported -- that is, if it could be used
	% by any other module, or by sub-modules of this module.
:- pred status_is_exported(import_status::in, bool::out) is det.

	% returns yes if the status indicates that the item was
	% in any way imported -- that is, if it was defined in
	% some other module, or in a sub-module of this module.
	% This is the opposite of status_defined_in_this_module.
:- pred status_is_imported(import_status::in, bool::out) is det.

	% returns yes if the status indicates that the item was
	% defined in this module.  This is the opposite of
	% status_is_imported.
:- pred status_defined_in_this_module(import_status::in, bool::out) is det.

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

		% The default flags for Aditi predicates are
		% aditi, dnf, supp_magic, psn and memo.

	;	dnf		% Requests that this predicate be transformed
				% into disjunctive normal form.

	;	aditi		% Generate bottom-up Aditi-RL for this
				% predicate.

	;	(aditi_top_down)
				% Generate top-down Aditi-RL, not C, for this
				% predicate. This is used for the builtin
				% `delete' predicate - the closure is used
				% to select which tuples are to be deleted.

	;	base_relation	% This predicate is an Aditi base relation.

			% `naive' and `psn' are mutually exclusive.
	;	naive		% Use naive evaluation of this Aditi predicate.
	;	psn		% Use predicate semi-naive evaluation of this
				% Aditi predicate.
	
	;	aditi_memo	% Requests that this Aditi predicate be
				% evaluated using memoing. This has no
				% relation to eval_method field of the
				% pred_info, which is ignored for Aditi
				% predicates.
	;	aditi_no_memo	% Ensure that this Aditi predicate
				% is not memoed.
	
			% `context' and `supp_magic' are mutually
			% exclusive. One of them must be performed
			% on all Aditi predicates. `supp_magic'
			% is the default

	;	supp_magic	% Perform the supplementary magic sets
				% transformation on this predicate. See magic.m
	;	context		% Perform the context transformation on
				% the predicate. See context.m

	;	generate_inline % Used for small Aditi predicates which
				% project a relation to be used as input to a 
				% call to an Aditi predicate in a lower SCC.
				% The goal for the predicate should consist
				% of fail, true or a single rule.
				% These relations are never memoed.
				% The reason for this marker is explained 
				% where it is introduced in
				% magic_util__create_closure.

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

	% Aditi predicates are identified by their owner as well as
	% module, name and arity.
:- type aditi_owner == string.

	% The constraint_proof_map is a map which for each type class
	% constraint records how/why that constraint was satisfied.
	% This information is used to determine how to construct the
	% typeclass_info for that constraint.
:- type constraint_proof_map == map(class_constraint, constraint_proof).

	% A typeclass_info_varmap is a map which for each type class constraint
	% records which variable contains the typeclass_info for that
	% constraint.
:- type typeclass_info_varmap == map(class_constraint, prog_var).

	% A type_info_varmap is a map which for each type variable
	% records where the type_info for that type variable is stored.
:- type type_info_varmap == map(tvar, type_info_locn).

	% A type_info_locn specifies how to access a type_info.
:- type type_info_locn	
	--->	type_info(prog_var)
				% It is a normal type_info, i.e. the type
				% is not constrained.

	;	typeclass_info(prog_var, int).
				% The type_info is packed inside a
				% typeclass_info. If the int is N, it is
				% the Nth type_info inside the typeclass_info,
				% but there may be several superclass pointers
				% before the block of type_infos, so it won't
				% be the Nth word of the typeclass_info.
				%
				% To find the type_info inside the
				% typeclass_info, use the predicate
				% type_info_from_typeclass_info from Mercury
				% code; from C code use the macro
				% MR_typeclass_info_superclass_info.

	% type_info_locn_var(TypeInfoLocn, Var): 
	% 	Var is the variable corresponding to the TypeInfoLocn. Note 
	% 	that this does *not* mean that Var is a type_info; it may be
	% 	a typeclass_info in which the type_info is nested.
:- pred type_info_locn_var(type_info_locn::in, prog_var::out) is det.

:- pred type_info_locn_set_var(type_info_locn::in, prog_var::in, 
		type_info_locn::out) is det.

	% hlds_pred__define_new_pred(Goal, CallGoal, Args, ExtraArgs, InstMap,
	% 	PredName, TVarSet, VarTypes, ClassContext, TVarMap, TCVarMap, 
	%	VarSet, Markers, Owner, IsAddressTaken,
	%	ModuleInfo0, ModuleInfo, PredProcId)
	%
	% Create a new predicate for the given goal, returning a goal to 
	% call the created predicate. ExtraArgs is the list of extra
	% type_infos and typeclass_infos required by typeinfo liveness
	% which were added to the front of the argument list.
:- pred hlds_pred__define_new_pred(hlds_goal, hlds_goal, list(prog_var),
		list(prog_var), instmap, string, tvarset, vartypes,
		class_constraints, type_info_varmap, typeclass_info_varmap,
		prog_varset, pred_markers, aditi_owner, is_address_taken,
		module_info, module_info, pred_proc_id).
:- mode hlds_pred__define_new_pred(in, out, in, out, in, in, in, in, in,
		in, in, in, in, in, in, in, out, out) is det.

	% Various predicates for accessing the information stored in the
	% pred_id and pred_info data structures.

:- pred pred_info_init(module_name, sym_name, arity, tvarset, existq_tvars,
	list(type), condition, prog_context, clauses_info, import_status,
	pred_markers, goal_type, pred_or_func, class_constraints, 
	constraint_proof_map, aditi_owner, pred_info).
:- mode pred_info_init(in, in, in, in, in, in, in, in, in, in, in, in, in,
	in, in, in, out) is det.

:- pred pred_info_create(module_name, sym_name, tvarset, existq_tvars,
	list(type), condition, prog_context, import_status, pred_markers,
	pred_or_func, class_constraints, aditi_owner, set(assert_id),
	proc_info, proc_id, pred_info).
:- mode pred_info_create(in, in, in, in, in, in, in, in, in, in, in, in, in,
		in, out, out) is det.

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

:- pred pred_info_arg_types(pred_info, list(type)).
:- mode pred_info_arg_types(in, out) is det.

:- pred pred_info_arg_types(pred_info, tvarset, existq_tvars, list(type)).
:- mode pred_info_arg_types(in, out, out, out) is det.

:- pred pred_info_set_arg_types(pred_info, tvarset, existq_tvars, list(type),
			pred_info).
:- mode pred_info_set_arg_types(in, in, in, in, out) is det.

:- pred pred_info_get_exist_quant_tvars(pred_info, existq_tvars).
:- mode pred_info_get_exist_quant_tvars(in, out) is det.

:- pred pred_info_get_univ_quant_tvars(pred_info, existq_tvars).
:- mode pred_info_get_univ_quant_tvars(in, out) is det.

:- type head_type_params == list(tvar).

:- pred pred_info_get_head_type_params(pred_info, head_type_params).
:- mode pred_info_get_head_type_params(in, out) is det.

:- pred pred_info_set_head_type_params(pred_info, head_type_params, pred_info).
:- mode pred_info_set_head_type_params(in, in, out) is det.

:- pred pred_info_get_unproven_body_constraints(pred_info,
					list(class_constraint)).
:- mode pred_info_get_unproven_body_constraints(in, out) is det.

:- pred pred_info_set_unproven_body_constraints(pred_info,
				list(class_constraint), pred_info).
:- mode pred_info_set_unproven_body_constraints(in, in, out) is det.

:- pred pred_info_clauses_info(pred_info, clauses_info).
:- mode pred_info_clauses_info(in, out) is det.

:- pred pred_info_set_clauses_info(pred_info, clauses_info, pred_info).
:- mode pred_info_set_clauses_info(in, in, out) is det.

:- pred pred_info_procedures(pred_info, proc_table).
:- mode pred_info_procedures(in, out) is det.

:- pred pred_info_set_procedures(pred_info, proc_table, pred_info).
:- mode pred_info_set_procedures(in, in, out) is det.

:- pred pred_info_context(pred_info, prog_context).
:- mode pred_info_context(in, out) is det.

:- pred pred_info_import_status(pred_info::in, import_status::out) is det.

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

	% pred_info_is_exported does *not* include predicates which are
	% exported_to_submodules or pseudo_exported
:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_exported_to_submodules(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

	% procedure_is_exported includes all modes of exported or
	% exported_to_submodules predicates, plus the in-in mode
	% for pseudo_exported unification predicates.
:- pred procedure_is_exported(pred_info::in, proc_id::in) is semidet.

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

	% N-ary functions are converted into N+1-ary predicates.
	% (Clauses are converted in make_hlds, but calls to functions
	% cannot be converted until after type-checking, once we have
	% resolved overloading. So we do that during mode analysis.)
	% The `is_pred_or_func' field of the pred_info records whether
	% a pred_info is really for a predicate or whether it is for
	% what was originally a function.

:- pred pred_info_get_is_pred_or_func(pred_info, pred_or_func).
:- mode pred_info_get_is_pred_or_func(in, out) is det.

:- pred pred_info_get_class_context(pred_info, class_constraints).
:- mode pred_info_get_class_context(in, out) is det.

:- pred pred_info_set_class_context(pred_info, class_constraints, pred_info).
:- mode pred_info_set_class_context(in, in, out) is det.

:- pred pred_info_get_constraint_proofs(pred_info, constraint_proof_map).
:- mode pred_info_get_constraint_proofs(in, out) is det.

:- pred pred_info_set_constraint_proofs(pred_info, constraint_proof_map,
	pred_info).
:- mode pred_info_set_constraint_proofs(in, in, out) is det.

:- pred pred_info_get_aditi_owner(pred_info, string).
:- mode pred_info_get_aditi_owner(in, out) is det.

:- pred pred_info_set_aditi_owner(pred_info, string, pred_info).
:- mode pred_info_set_aditi_owner(in, in, out) is det.

:- pred pred_info_get_indexes(pred_info, list(index_spec)).
:- mode pred_info_get_indexes(in, out) is det.

:- pred pred_info_set_indexes(pred_info, list(index_spec), pred_info).
:- mode pred_info_set_indexes(in, in, out) is det.

:- pred pred_info_get_assertions(pred_info, set(assert_id)).
:- mode pred_info_get_assertions(in, out) is det.

:- pred pred_info_set_assertions(pred_info, set(assert_id), pred_info).
:- mode pred_info_set_assertions(in, in, out) is det.

:- pred pred_info_get_purity(pred_info, purity).
:- mode pred_info_get_purity(in, out) is det.

:- pred pred_info_get_promised_pure(pred_info, bool).
:- mode pred_info_get_promised_pure(in, out) is det.

:- pred purity_to_markers(purity, pred_markers).
:- mode purity_to_markers(in, out) is det.

:- pred pred_info_get_markers(pred_info, pred_markers).
:- mode pred_info_get_markers(in, out) is det.

:- pred pred_info_set_markers(pred_info, pred_markers, pred_info).
:- mode pred_info_set_markers(in, in, out) is det.

:- pred pred_info_get_call_id(pred_info, simple_call_id).
:- mode pred_info_get_call_id(in, out) is det.

	% create an empty set of markers
:- pred init_markers(pred_markers).
:- mode init_markers(out) is det.

	% check if a particular is in the set
:- pred check_marker(pred_markers, marker).
:- mode check_marker(in, in) is semidet.

	% add a marker to the set
:- pred add_marker(pred_markers, marker, pred_markers).
:- mode add_marker(in, in, out) is det.

	% remove a marker from the set
:- pred remove_marker(pred_markers, marker, pred_markers).
:- mode remove_marker(in, in, out) is det.

	% convert the set to a list
:- pred markers_to_marker_list(pred_markers, list(marker)).
:- mode markers_to_marker_list(in, out) is det.

:- pred marker_list_to_markers(list(marker), pred_markers).
:- mode marker_list_to_markers(in, out) is det.

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

status_is_exported(imported(_),			no).
status_is_exported(abstract_imported,		no).
status_is_exported(pseudo_imported,		no).
status_is_exported(opt_imported,		no).
status_is_exported(exported,			yes).
status_is_exported(abstract_exported,		yes).
status_is_exported(pseudo_exported,		yes).
status_is_exported(exported_to_submodules,	yes).
status_is_exported(local,			no).

status_is_imported(Status, Imported) :-
	status_defined_in_this_module(Status, InThisModule),
	bool__not(InThisModule, Imported).

status_defined_in_this_module(imported(_),		no).
status_defined_in_this_module(abstract_imported,	no).
status_defined_in_this_module(pseudo_imported,		no).
status_defined_in_this_module(opt_imported,		no).
status_defined_in_this_module(exported,			yes).
status_defined_in_this_module(abstract_exported,	yes).
status_defined_in_this_module(pseudo_exported,		yes).
status_defined_in_this_module(exported_to_submodules,	yes).
status_defined_in_this_module(local,			yes).

	% The information specific to a predicate, as opposed to a procedure.
	% (Functions count as predicates.)

:- type pred_info
	--->	predicate(
			tvarset,	% names of type vars
					% in the predicate's type decl
			list(type),	% argument types
			condition,	% formal specification
					% (not used)

			clauses_info,

			proc_table,

			prog_context,
					% the location (line #)
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
			class_constraints,
					% the class constraints on the 
					% type variables in the predicate's
					% type declaration
			constraint_proof_map,
					% explanations of how redundant
					% constraints were eliminated. These
					% are needed by polymorphism.m to
					% work out where to get the
					% typeclass_infos from.
					% Computed during type checking.
			existq_tvars,	% the set of existentially quantified
					% type variables in the predicate's
					% type decl
			head_type_params,
					% The set of type variables which the
					% body of the predicate can't bind,
					% and whose type_infos are produced
					% elsewhere.  This includes
					% universally quantified head types
					% (the type_infos are passed in)
					% plus existentially quantified types
					% in preds called from the body
					% (the type_infos are returned from
					% the called preds).
					% Computed during type checking.
			list(class_constraint),
					% unproven class constraints on type
					% variables in the predicate's body,
					% if any (if this remains non-empty
					% after type checking has finished,
					% post_typecheck.m will report a type
					% error).
			aditi_owner,
					% The owner of this predicate if
					% it is an Aditi predicate. Set to
					% the value of --aditi-user if no
					% `:- pragma owner' declaration exists.
			list(index_spec),
					% Indexes if this predicate is
					% an Aditi base relation, ignored
					% otherwise.
			set(assert_id)
					% List of assertions which
					% mention this predicate.
		).

pred_info_init(ModuleName, SymName, Arity, TypeVarSet, ExistQVars, Types,
		Cond, Context, ClausesInfo, Status, Markers, GoalType,
		PredOrFunc, ClassContext, ClassProofs, User, PredInfo) :-
	map__init(Procs),
	unqualify_name(SymName, PredName),
	sym_name_get_module_name(SymName, ModuleName, PredModuleName),
	term__vars_list(Types, TVars),
	list__delete_elems(TVars, ExistQVars, HeadTypeParams),
	UnprovenBodyConstraints = [],
	Indexes = [],
	set__init(Assertions),
	PredInfo = predicate(TypeVarSet, Types, Cond, ClausesInfo, Procs,
		Context, PredModuleName, PredName, Arity, Status, TypeVarSet, 
		GoalType, Markers, PredOrFunc, ClassContext, ClassProofs,
		ExistQVars, HeadTypeParams, UnprovenBodyConstraints, User,
		Indexes, Assertions).

pred_info_create(ModuleName, SymName, TypeVarSet, ExistQVars, Types, Cond,
		Context, Status, Markers, PredOrFunc, ClassContext, User,
		Assertions, ProcInfo, ProcId, PredInfo) :-
	map__init(Procs0),
	proc_info_declared_determinism(ProcInfo, MaybeDetism),
	next_mode_id(Procs0, MaybeDetism, ProcId),
	map__det_insert(Procs0, ProcId, ProcInfo, Procs),
	list__length(Types, Arity),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_typeinfo_varmap(ProcInfo, TypeInfoMap),
	proc_info_typeclass_info_varmap(ProcInfo, TypeClassInfoMap),
	unqualify_name(SymName, PredName),
	% The empty list of clauses is a little white lie.
	Clauses = [],
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes, HeadVars,
		Clauses, TypeInfoMap, TypeClassInfoMap),
	map__init(ClassProofs),
	term__vars_list(Types, TVars),
	list__delete_elems(TVars, ExistQVars, HeadTypeParams),
	UnprovenBodyConstraints = [],
	Indexes = [],
	PredInfo = predicate(TypeVarSet, Types, Cond, ClausesInfo, Procs,
		Context, ModuleName, PredName, Arity, Status, TypeVarSet, 
		clauses, Markers, PredOrFunc, ClassContext, ClassProofs,
		ExistQVars, HeadTypeParams, UnprovenBodyConstraints, User,
		Indexes, Assertions).

pred_info_procids(PredInfo, ProcIds) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _, _, 
		_, _, _, _, _, _, _, _, _, _),
	map__keys(Procs, ProcIds).

pred_info_non_imported_procids(PredInfo, ProcIds) :-
	pred_info_import_status(PredInfo, ImportStatus),
	( ImportStatus = imported(_) ->
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
	(
		( ImportStatus = exported
		; ImportStatus = exported_to_submodules
		)
	->
		pred_info_procids(PredInfo, ProcIds)
	;
		ImportStatus = pseudo_exported
	->
		ProcIds = [0]
	;
		ProcIds = []
	).

pred_info_clauses_info(PredInfo, Clauses) :-
	PredInfo = predicate(_, _, _, Clauses, _, _, _, _, _, _, _, _, _,
		_, _, _, _, _, _, _, _, _).

pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo) :-
	PredInfo0 = predicate(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, P,
		Q, R, S, T, U, V),
	PredInfo = predicate(A, B, C, Clauses, E, F, G, H, I, J, K, 
		L, M, N, O, P, Q, R, S, T, U, V).

pred_info_arg_types(PredInfo, ArgTypes) :-
	pred_info_arg_types(PredInfo, _TypeVars, _ExistQVars, ArgTypes).

pred_info_arg_types(PredInfo, TypeVars, ExistQVars, ArgTypes) :-
	PredInfo = predicate(TypeVars, ArgTypes, _, _, _, _, _, _, _, _, _,
		_, _, _, _, _, ExistQVars, _, _, _, _, _).

pred_info_set_arg_types(PredInfo0, TypeVarSet, ExistQVars, ArgTypes,
		PredInfo) :-
	PredInfo0 = predicate(_, _, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		_, R, S, T, U, V),
	PredInfo = predicate(TypeVarSet, ArgTypes, C, D, E, F, G, H, I, J, K,
		L, M, N, O, P, ExistQVars, R, S, T, U, V).

pred_info_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_, _, _, _, Procs, _, _, _, _, _, _, 
		_, _, _, _, _, _, _, _, _, _, _).

pred_info_set_procedures(PredInfo0, Procedures, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, P,
		Q, R, S, T, U, V),
	PredInfo = predicate(A, B, C, D, Procedures, F, G, H, I, J, K, L, M, 
		N, O, P, Q, R, S, T, U, V).

pred_info_context(PredInfo, Context) :-
	PredInfo = predicate(_, _, _, _, _, Context, _, _, _, 
		_, _, _, _, _, _, _, _, _, _, _, _, _).

pred_info_module(PredInfo, Module) :-
	PredInfo = predicate(_, _, _, _, _, _, Module, _, _, _, _, 
		_, _, _, _, _, _, _, _, _, _, _).

pred_info_name(PredInfo, PredName) :-
	PredInfo = predicate(_, _, _, _, _, _, _, PredName, _, _, _, 
		_, _, _, _, _, _, _, _, _, _, _).

pred_info_arity(PredInfo, Arity) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, Arity, _, _, 
		_, _, _, _, _, _, _, _, _, _, _).

pred_info_import_status(PredInfo, ImportStatus) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, ImportStatus, _, _, _,
				_, _, _, _, _, _, _, _, _).

pred_info_is_imported(PredInfo) :-
	pred_info_import_status(PredInfo, imported(_)).

pred_info_is_pseudo_imported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = pseudo_imported.

pred_info_is_exported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = exported.

pred_info_is_exported_to_submodules(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = exported_to_submodules.

pred_info_is_pseudo_exported(PredInfo) :-
	pred_info_import_status(PredInfo, ImportStatus),
	ImportStatus = pseudo_exported.

procedure_is_exported(PredInfo, ProcId) :-
	(
		pred_info_is_exported(PredInfo)
	;
		pred_info_is_exported_to_submodules(PredInfo)
	;
		pred_info_is_pseudo_exported(PredInfo),
		hlds_pred__in_in_unification_proc_id(ProcId)
	).

pred_info_mark_as_external(PredInfo0, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, ImportStatus,
		K, L, M, N, O, P, Q, R, S, T, U, V),
	status_is_exported(ImportStatus, Exported),
	(
		Exported = yes,
		PredInfo = predicate(A, B, C, D, E, F, G, H, I,
				imported(interface), K, L, M, N, O,
				P, Q, R, S, T, U, V)
	;
		Exported = no,
		PredInfo = predicate(A, B, C, D, E, F, G, H, I,
				imported(implementation), K, L, M, N, O,
				P, Q, R, S, T, U, V)
	).

pred_info_set_import_status(PredInfo0, Status, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, P,
		Q, R, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, Status, K, 
		L, M, N, O, P, Q, R, S, T, U, V).

pred_info_typevarset(PredInfo, TypeVarSet) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, TypeVarSet, _, _, 
		_, _, _, _, _, _, _, _, _).

pred_info_set_typevarset(PredInfo0, TypeVarSet, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, P,
		Q, R, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, TypeVarSet, L, M,
				N, O, P, Q, R, S, T, U, V).

pred_info_get_goal_type(PredInfo, GoalType) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, GoalType, _, 
		_, _, _, _, _, _, _, _, _).

pred_info_set_goal_type(PredInfo0, GoalType, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, P,
		Q, R, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, GoalType, M, 
		N, O, P, Q, R, S, T, U, V).

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
		_, _, _, _, _, _, _, _, _).

pred_info_set_markers(PredInfo0, Markers, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, P,
		Q, R, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, Markers, 
		N, O, P, Q, R, S, T, U, V).

pred_info_get_is_pred_or_func(PredInfo, IsPredOrFunc) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _,
			IsPredOrFunc, _, _, _, _, _, _, _, _).

pred_info_set_class_context(PredInfo0, ClassContext, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, P,
		Q, R, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		ClassContext, P, Q, R, S, T, U, V).

pred_info_get_class_context(PredInfo, ClassContext) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		ClassContext, _, _, _, _, _, _, _).

pred_info_set_constraint_proofs(PredInfo0, Proofs, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, _,
		Q, R, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, Proofs, Q, R, S, T, U, V).

pred_info_get_constraint_proofs(PredInfo, ConstraintProofs) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		ConstraintProofs, _, _, _, _, _, _).

pred_info_get_exist_quant_tvars(PredInfo, ExistQVars) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, ExistQVars, _, _, _, _, _).

pred_info_get_univ_quant_tvars(PredInfo, UnivQVars) :-
	pred_info_arg_types(PredInfo, ArgTypes),
	term__vars_list(ArgTypes, ArgTypeVars0),
	list__sort_and_remove_dups(ArgTypeVars0, ArgTypeVars),
	pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
	list__delete_elems(ArgTypeVars, ExistQVars, UnivQVars).

pred_info_get_head_type_params(PredInfo, HeadTypeParams) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, HeadTypeParams, _, _, _, _).

pred_info_set_head_type_params(PredInfo0, HeadTypeParams, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q, _, S, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q, HeadTypeParams, S, T, U, V).

pred_info_get_unproven_body_constraints(PredInfo, UnprovenBodyConstraints) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, UnprovenBodyConstraints, _, _, _).

pred_info_set_unproven_body_constraints(PredInfo0, UnprovenBodyConstraints,
		PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q, R, _, T, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P,
		Q, R, UnprovenBodyConstraints, T, U, V).


pred_info_get_aditi_owner(PredInfo, Owner) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _, Owner, _, _).

pred_info_set_aditi_owner(PredInfo0, Owner, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		O, P, Q, R, S, _, U, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S, Owner, U, V).

pred_info_get_indexes(PredInfo, Indexes) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _, _, Indexes, _).

pred_info_set_indexes(PredInfo0, Indexes, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		O, P, Q, R, S, T, _, V),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S, T, Indexes, V).

pred_info_get_assertions(PredInfo, Assertions) :-
	PredInfo = predicate(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
		_, _, _, _, _, _, Assertions).

pred_info_set_assertions(PredInfo0, Assertions, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N,
		O, P, Q, R, S, T, U, _),
	PredInfo  = predicate(A, B, C, D, E, F, G, H, I, J, K, L, M, N, 
		O, P, Q, R, S, T, U, Assertions).

%-----------------------------------------------------------------------------%

pred_info_get_call_id(PredInfo, PredOrFunc - qualified(Module, Name)/Arity) :-
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity).

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

remove_marker(Markers0, Marker, Markers) :-
	list__delete_all(Markers0, Marker, Markers).

markers_to_marker_list(Markers, Markers).

marker_list_to_markers(Markers, Markers).

%-----------------------------------------------------------------------------%

% :- type clauses_info	--->	clauses_info(
% 					prog_varset,	% variable names
% 					vartypes,
% 						% variable types from
% 						% explicit qualifications
% 					vartypes,
% 						% variable types
% 						% inferred by typecheck.m.
% 					list(prog_var),	% head vars
% 					list(clause),
%					type_info_varmap,
%					typeclass_info_varmap,
% 				).

clauses_info_varset(clauses_info(VarSet, _, _, _, _, _, _), VarSet).
clauses_info_explicit_vartypes(
	clauses_info(_, ExplicitVarTypes, _, _, _, _, _), ExplicitVarTypes).
clauses_info_vartypes(clauses_info(_, _, VarTypes, _, _, _, _), VarTypes).
clauses_info_headvars(clauses_info(_, _, _, HeadVars, _, _, _), HeadVars).
clauses_info_clauses(clauses_info(_, _, _, _, Clauses, _, _), Clauses).
clauses_info_type_info_varmap(clauses_info(_, _, _, _, _, TIMap, _), TIMap).
clauses_info_typeclass_info_varmap(clauses_info(_, _, _, _, _, _, TCIMap),
		TCIMap).

clauses_info_set_varset(clauses_info(_, B, C, D, E, F, G), VarSet,
		clauses_info(VarSet, B, C, D, E, F, G)).
clauses_info_set_explicit_vartypes(clauses_info(A, _, C, D, E, F, G),
		ExplicitVarTypes,
		clauses_info(A, ExplicitVarTypes, C, D, E, F, G)).
clauses_info_set_vartypes(clauses_info(A, B, _, D, E, F, G), VarTypes,
		clauses_info(A, B, VarTypes, D, E, F, G)).
clauses_info_set_headvars(clauses_info(A, B, C, _, E, F, G), HeadVars,
		clauses_info(A, B, C, HeadVars, E, F, G)).
clauses_info_set_clauses(clauses_info(A, B, C, D, _, F, G), Clauses,
		clauses_info(A, B, C, D, Clauses, F, G)).
clauses_info_set_type_info_varmap(clauses_info(A, B, C, D, E, _, G), TIMap,
		clauses_info(A, B, C, D, E, TIMap, G)).
clauses_info_set_typeclass_info_varmap(clauses_info(A, B, C, D, E, F, _),
		TCIMap,
		clauses_info(A, B, C, D, E, F, TCIMap)).

%-----------------------------------------------------------------------------%

hlds_pred__define_new_pred(Goal0, Goal, ArgVars0, ExtraTypeInfos, InstMap0,
		PredName, TVarSet, VarTypes0, ClassContext, TVarMap, TCVarMap,
		VarSet0, Markers, Owner, IsAddressTaken,
		ModuleInfo0, ModuleInfo, PredProcId) :-
	Goal0 = _GoalExpr - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),

	% XXX The set of existentially quantified type variables
	% here might not be correct.
	ExistQVars = [],

	% If interface typeinfo liveness is set, all type_infos for the
	% arguments need to be passed in, not just the ones that are used.
	% Similarly if the address of a procedure of this predicate is taken,
	% so that we can copy the closure.
	module_info_globals(ModuleInfo0, Globals),
	ExportStatus = local,
	interface_should_use_typeinfo_liveness(ExportStatus,
		IsAddressTaken, Globals, TypeInfoLiveness),
	( TypeInfoLiveness = yes ->
		goal_info_get_nonlocals(GoalInfo, NonLocals),
		goal_util__extra_nonlocal_typeinfos(TVarMap, TCVarMap,
			VarTypes0, ExistQVars, NonLocals, ExtraTypeInfos0),
		set__delete_list(ExtraTypeInfos0, ArgVars0, ExtraTypeInfos1),
		set__to_sorted_list(ExtraTypeInfos1, ExtraTypeInfos),
		list__append(ExtraTypeInfos, ArgVars0, ArgVars)
	;
		ArgVars = ArgVars0,
		ExtraTypeInfos = []
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

	proc_info_create(VarSet, VarTypes, ArgVars, ArgModes, Detism, Goal0,
		Context, TVarMap, TCVarMap, IsAddressTaken, ProcInfo0),
	proc_info_set_maybe_termination_info(ProcInfo0, TermInfo, ProcInfo),

	set__init(Assertions),

	pred_info_create(ModuleName, SymName, TVarSet, ExistQVars, ArgTypes,
		true, Context, ExportStatus, Markers, predicate, ClassContext, 
		Owner, Assertions, ProcInfo, ProcId, PredInfo),

	module_info_get_predicate_table(ModuleInfo0, PredTable0),
	predicate_table_insert(PredTable0, PredInfo, PredId,
		PredTable),
	module_info_set_predicate_table(ModuleInfo0, PredTable,
		ModuleInfo),

	GoalExpr = call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
	Goal = GoalExpr - GoalInfo,
	PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(list(prog_var)::in, vartypes::in,
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

:- type is_address_taken
	--->	address_is_taken
	;	address_is_not_taken.

:- pred proc_info_init(arity, list(type), list(mode), maybe(list(mode)),
	maybe(list(is_live)), maybe(determinism), prog_context,
	is_address_taken, proc_info).
:- mode proc_info_init(in, in, in, in, in, in, in, in, out) is det.

:- pred proc_info_set(maybe(determinism), prog_varset, vartypes,
	list(prog_var), list(mode), maybe(list(is_live)), hlds_goal,
	prog_context, stack_slots, determinism, bool, list(arg_info),
	liveness_info, type_info_varmap, typeclass_info_varmap,
	maybe(arg_size_info), maybe(termination_info), is_address_taken,
	proc_info).
:- mode proc_info_set(in, in, in, in, in, in, in, in, in, in, in, in, in, in,
	in, in, in, in, out) is det.

:- pred proc_info_create(prog_varset, vartypes, list(prog_var),
	list(mode), determinism, hlds_goal, prog_context,
	type_info_varmap, typeclass_info_varmap, is_address_taken, proc_info).
:- mode proc_info_create(in, in, in, in, in, in, in, in, in, in, out) is det.

:- pred proc_info_set_body(proc_info, prog_varset, vartypes,
		list(prog_var), hlds_goal, type_info_varmap,
		typeclass_info_varmap, proc_info).
:- mode proc_info_set_body(in, in, in, in, in, in, in, out) is det.

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

:- pred proc_info_varset(proc_info, prog_varset).
:- mode proc_info_varset(in, out) is det.

:- pred proc_info_set_varset(proc_info, prog_varset, proc_info).
:- mode proc_info_set_varset(in, in, out) is det.

:- pred proc_info_vartypes(proc_info, vartypes).
:- mode proc_info_vartypes(in, out) is det.

:- pred proc_info_set_vartypes(proc_info, vartypes, proc_info).
:- mode proc_info_set_vartypes(in, in, out) is det.

:- pred proc_info_headvars(proc_info, list(prog_var)).
:- mode proc_info_headvars(in, out) is det.

:- pred proc_info_set_headvars(proc_info, list(prog_var), proc_info).
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

:- pred proc_info_context(proc_info, prog_context).
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

:- pred proc_info_typeinfo_varmap(proc_info, type_info_varmap).
:- mode proc_info_typeinfo_varmap(in, out) is det.

:- pred proc_info_set_typeinfo_varmap(proc_info, type_info_varmap, proc_info).
:- mode proc_info_set_typeinfo_varmap(in, in, out) is det.

:- pred proc_info_eval_method(proc_info, eval_method).
:- mode proc_info_eval_method(in, out) is det.

:- pred proc_info_set_eval_method(proc_info, eval_method, proc_info).
:- mode proc_info_set_eval_method(in, in, out) is det.

:- pred proc_info_typeclass_info_varmap(proc_info, typeclass_info_varmap).
:- mode proc_info_typeclass_info_varmap(in, out) is det.

:- pred proc_info_set_typeclass_info_varmap(proc_info, typeclass_info_varmap,
	proc_info).
:- mode proc_info_set_typeclass_info_varmap(in, in, out) is det.

:- pred proc_info_maybe_declared_argmodes(proc_info, maybe(list(mode))).
:- mode proc_info_maybe_declared_argmodes(in, out) is det.

:- pred proc_info_declared_argmodes(proc_info, list(mode)).
:- mode proc_info_declared_argmodes(in, out) is det.

:- pred proc_info_is_address_taken(proc_info, is_address_taken).
:- mode proc_info_is_address_taken(in, out) is det.

:- pred proc_info_get_rl_exprn_id(proc_info, maybe(rl_exprn_id)).
:- mode proc_info_get_rl_exprn_id(in, out) is det.

:- pred proc_info_set_rl_exprn_id(proc_info, rl_exprn_id, proc_info).
:- mode proc_info_set_rl_exprn_id(in, in, out) is det.

	% For a set of variables V, find all the type variables in the types 
	% of the variables in V, and return set of typeinfo variables for 
	% those type variables. (find all typeinfos for variables in V).
	%
	% This set of typeinfos is often needed in liveness computation
	% for accurate garbage collection - live variables need to have 
	% their typeinfos stay live too.

:- pred proc_info_get_typeinfo_vars_setwise(proc_info, set(prog_var),
		set(prog_var)).
:- mode proc_info_get_typeinfo_vars_setwise(in, in, out) is det.

:- pred proc_info_ensure_unique_names(proc_info, proc_info).
:- mode proc_info_ensure_unique_names(in, out) is det.

	% Create a new variable of the given type to the procedure.
:- pred proc_info_create_var_from_type(proc_info, type, prog_var, proc_info).
:- mode proc_info_create_var_from_type(in, in, out, out) is det.

	% Create a new variable for each element of the list of types.
:- pred proc_info_create_vars_from_types(proc_info, 
		list(type), list(prog_var), proc_info).
:- mode proc_info_create_vars_from_types(in, in, out, out) is det.

	% Return true if the interface of the given procedure must include
	% typeinfos for all the type variables in the types of the arguments.
:- pred proc_interface_should_use_typeinfo_liveness(pred_info, proc_id,
	globals, bool).
:- mode proc_interface_should_use_typeinfo_liveness(in, in, in, out) is det.

	% Return true if the interface of a procedure with the given
	% characteristics (import/export/local status, address taken status)
	% must include typeinfos for all the type variables in the types
	% of the arguments.
:- pred interface_should_use_typeinfo_liveness(import_status, is_address_taken,
		globals, bool).
:- mode interface_should_use_typeinfo_liveness(in, in, in, out) is det.

	% Return true if the body of the procedure must keep a typeinfo
	% variable alive during the lifetime of all variables whose type
	% includes the corresponding type variable. Note that body typeinfo
	% liveness implies interface typeinfo liveness, but not vice versa.
:- pred body_should_use_typeinfo_liveness(globals, bool).
:- mode body_should_use_typeinfo_liveness(in, out) is det.

:- implementation.

:- type proc_info
	--->	procedure(
			maybe(determinism),
					% _declared_ determinism
					% or `no' if there was no detism decl
			prog_varset,	% variable names
			vartypes,	% variable types
			list(prog_var),	% head vars
			list(mode), 	% modes of args
			maybe(list(is_live)),
					% liveness (in the mode analysis sense)
					% of the arguments
			hlds_goal,	% Body
			prog_context,
					% The context of the `:- mode' decl
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
			type_info_varmap,	
					% typeinfo vars for type parameters
			typeclass_info_varmap,
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
			is_address_taken,
					% Is the address of this procedure
					% taken? If yes, we will need to use
					% typeinfo liveness for them, so that
					% deep_copy and accurate gc have the
					% RTTI they need for copying closures.
					%
					% Note that any non-local procedure
					% must be considered as having its
					% address taken, since it is possible
					% that some other module may do so.
			maybe(rl_exprn_id)
					% For predicates with an
					% `aditi_top_down' marker, which are
					% executed top-down on the Aditi side
					% of the connection, we generate an RL
					% expression, for which this is an
					% identifier. See rl_update.m.
		).

	% Some parts of the procedure aren't known yet. We initialize
	% them to any old garbage which we will later throw away.

	% Inferred determinism gets initialized to `erroneous'.
	% This is what `det_analysis.m' wants. det_analysis.m
	% will later provide the correct inferred determinism for it.

proc_info_init(Arity, Types, Modes, DeclaredModes, MaybeArgLives,
		MaybeDet, MContext, IsAddressTaken, NewProc) :-
	varset__init(BodyVarSet0),
	make_n_fresh_vars("HeadVar__", Arity, BodyVarSet0,
		HeadVars, BodyVarSet),
	map__from_corresponding_lists(HeadVars, Types, BodyTypes),
	InferredDet = erroneous,
	map__init(StackSlots),
	set__init(InitialLiveness),
	ArgInfo = [],
	goal_info_init(GoalInfo),
	ClauseBody = conj([]) - GoalInfo,
	CanProcess = yes,
	map__init(TVarsMap),
	map__init(TCVarsMap),
	RLExprn = no,
	NewProc = procedure(
		MaybeDet, BodyVarSet, BodyTypes, HeadVars, Modes, MaybeArgLives,
		ClauseBody, MContext, StackSlots, InferredDet, CanProcess,
		ArgInfo, InitialLiveness, TVarsMap, TCVarsMap, eval_normal,
		no, no, DeclaredModes, IsAddressTaken, RLExprn
	).

proc_info_set(DeclaredDetism, BodyVarSet, BodyTypes, HeadVars, HeadModes,
		HeadLives, Goal, Context, StackSlots, InferredDetism,
		CanProcess, ArgInfo, Liveness, TVarMap, TCVarsMap, ArgSizes,
		Termination, IsAddressTaken, ProcInfo) :-
	RLExprn = no,
	ProcInfo = procedure(
		DeclaredDetism, BodyVarSet, BodyTypes, HeadVars, HeadModes,
		HeadLives, Goal, Context, StackSlots, InferredDetism,
		CanProcess, ArgInfo, Liveness, TVarMap, TCVarsMap, eval_normal, 
		ArgSizes, Termination, no, IsAddressTaken, RLExprn).

proc_info_create(VarSet, VarTypes, HeadVars, HeadModes, Detism, Goal,
		Context, TVarMap, TCVarsMap, IsAddressTaken, ProcInfo) :-
	map__init(StackSlots),
	set__init(Liveness),
	MaybeHeadLives = no,
	RLExprn = no,
	ProcInfo = procedure(yes(Detism), VarSet, VarTypes, HeadVars, HeadModes,
		MaybeHeadLives, Goal, Context, StackSlots, Detism, yes, [],
		Liveness, TVarMap, TCVarsMap, eval_normal, no, no, no, 
		IsAddressTaken, RLExprn).

proc_info_set_body(ProcInfo0, VarSet, VarTypes, HeadVars, Goal,
		TI_VarMap, TCI_VarMap, ProcInfo) :-
	ProcInfo0 = procedure(A, _, _, _, E, F, _,
		H, I, J, K, L, M, _, _, P, Q, R, S, T, U),
	ProcInfo = procedure(A, VarSet, VarTypes, HeadVars, E, F, Goal,
		H, I, J, K, L, M, TI_VarMap, TCI_VarMap, P, Q, R, S, T, U).

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
    		_, _, _, _, _).

proc_info_varset(ProcInfo, B) :-
	ProcInfo = procedure(_, B, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_vartypes(ProcInfo, C) :-
	ProcInfo = procedure(_, _, C, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_headvars(ProcInfo, D) :-
	ProcInfo = procedure(_, _, _, D, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_argmodes(ProcInfo, E) :-
	ProcInfo = procedure(_, _, _, _, E, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_maybe_arglives(ProcInfo, F) :-
	ProcInfo = procedure(_, _, _, _, _, F, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_goal(ProcInfo, G) :-
	ProcInfo = procedure(_, _, _, _, _, _, G, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_context(ProcInfo, H) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, H, _, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_stack_slots(ProcInfo, I) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, I, _, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_inferred_determinism(ProcInfo, J) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, J, _, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_can_process(ProcInfo, K) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, K, _, _, _, _, _, 
		_, _, _, _, _).

proc_info_arg_info(ProcInfo, L) :- 
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, L, _, _, _, _, 
		_, _, _, _, _).

proc_info_liveness_info(ProcInfo, M) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, M, _, _, _,
		_, _, _, _, _).

proc_info_typeinfo_varmap(ProcInfo, N) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, N, _, _, 
		_, _, _, _, _).

proc_info_typeclass_info_varmap(ProcInfo, O) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, O, _, 
		_, _, _, _, _).

proc_info_eval_method(ProcInfo, P) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, P, 
		_, _, _, _, _).

proc_info_get_maybe_arg_size_info(ProcInfo, Q) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		Q, _, _, _, _).

proc_info_get_maybe_termination_info(ProcInfo, R) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, R, _, _, _).

proc_info_maybe_declared_argmodes(ProcInfo, S) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, S, _, _).

proc_info_is_address_taken(ProcInfo, T) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, T, _).

proc_info_get_rl_exprn_id(ProcInfo, U) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, 
		_, _, _, _, U).

% :- type proc_info
% 	--->	procedure(
% A			maybe(determinism),
% 					% _declared_ determinism
% 					% or `no' if there was no detism decl
% B			varset,		% variable names
% C 			map(var, type),	% variable types
% D			list(prog_var),	% head vars
% E			list(mode), 	% modes of args
% F			maybe(list(is_live)),
% 					% liveness (in the mode analysis sense)
% 					% of the arguments
% G			hlds_goal,	% Body
% H			prog_context,
%					% The context of the `:- mode' decl
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
% N			type_info_varmap,	
% 					% typeinfo vars for type parameters
% O			typeclass_info_varmap,
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
% S			maybe(list(mode)),
% 					% declared modes of arguments.
% T			is_address_taken
%					% Is the address of this procedure
%					% taken? If yes, we will need to use
%					% typeinfo liveness for them, so that
%					% deep_copy and accurate gc have the
%					% RTTI they need for copying closures.
% U			maybe(rl_exprn_id)
%					% For predicates with an
%					% `aditi_top_down' marker, which are
%					% executed top-down on the Aditi side
%					% of the connection, we generate an RL
%					% expression, for which this is an
%					% identifier. See rl_update.m.
%		).

proc_info_set_varset(ProcInfo0, B, ProcInfo) :-
	ProcInfo0 = procedure(A, _, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_vartypes(ProcInfo0, C, ProcInfo) :-
	ProcInfo0 = procedure(A, B, _, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_headvars(ProcInfo0, D, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, _, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_argmodes(ProcInfo0, E, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, _, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_maybe_arglives(ProcInfo0, F, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, _, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_goal(ProcInfo0, G, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, _, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_stack_slots(ProcInfo0, I, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, _, J, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_inferred_determinism(ProcInfo0, J, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, _, K, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_can_process(ProcInfo0, K, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, _, L, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_arg_info(ProcInfo0, L, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, _, M, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_liveness_info(ProcInfo0, M, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, _, N, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_typeinfo_varmap(ProcInfo0, N, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, _, O, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_typeclass_info_varmap(ProcInfo0, O, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, _, 
		P, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_eval_method(ProcInfo0, P, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
		_, Q, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O,
		P, Q, R, S, T, U).

proc_info_set_maybe_arg_size_info(ProcInfo0, Q, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, _, R, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_maybe_termination_info(ProcInfo0, R, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, _, S, T, U),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, U).

proc_info_set_rl_exprn_id(ProcInfo0, U, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, _),
	ProcInfo  = procedure(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, 
		P, Q, R, S, T, yes(U)).

proc_info_get_typeinfo_vars_setwise(ProcInfo, Vars, TypeInfoVars) :-
	set__to_sorted_list(Vars, VarList),
	proc_info_get_typeinfo_vars_2(ProcInfo, VarList, TypeInfoVarList),
	set__list_to_set(TypeInfoVarList, TypeInfoVars).

	% auxiliary predicate - traverses variables and builds a list of
	% variables that store typeinfos for these variables. 
:- pred proc_info_get_typeinfo_vars_2(proc_info, list(prog_var),
		list(prog_var)).
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

proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId, Globals,
		InterfaceTypeInfoLiveness) :-
	pred_info_import_status(PredInfo, Status),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_is_address_taken(ProcInfo, IsAddressTaken),
	interface_should_use_typeinfo_liveness(Status, IsAddressTaken, Globals,
		InterfaceTypeInfoLiveness).

interface_should_use_typeinfo_liveness(Status, IsAddressTaken, Globals,
		InterfaceTypeInfoLiveness) :-
	(
		(
			IsAddressTaken = address_is_taken
		;
			% If the predicate is exported, its address may have
			% been taken elsewhere. If it is imported, then it
			% follows that it must be exported somewhere.
			Status \= local
		;
			body_should_use_typeinfo_liveness(Globals, yes)
		)
	->
		InterfaceTypeInfoLiveness = yes
	;
		InterfaceTypeInfoLiveness = no
	).

body_should_use_typeinfo_liveness(Globals, BodyTypeInfoLiveness) :-
	globals__lookup_bool_option(Globals, body_typeinfo_liveness,
		BodyTypeInfoLiveness).

%-----------------------------------------------------------------------------%

:- interface.

	% make_n_fresh_vars(Name, N, VarSet0, Vars, VarSet):
	%	`Vars' is a list of `N' fresh variables allocated from
	%	`VarSet0'.  The variables will be named "<Name>1", "<Name>2",
	%	"<Name>3", and so on, where <Name> is the value of `Name'.
	%	`VarSet' is the resulting varset.

:- pred make_n_fresh_vars(string, int, varset(T), list(var(T)), varset(T)).
:- mode make_n_fresh_vars(in, in, in, out, out) is det.

	% given the list of predicate arguments for a predicate that
	% is really a function, split that list into the function arguments
	% and the function return type.
:- pred pred_args_to_func_args(list(T), list(T), T).
:- mode pred_args_to_func_args(in, out, out) is det.

	% adjust_func_arity(PredOrFunc, FuncArity, PredArity).
	%
	% We internally store the arity as the length of the argument
	% list including the return value, which is one more than the
	% arity of the function reported in error messages.
:- pred adjust_func_arity(pred_or_func, int, int).
:- mode adjust_func_arity(in, in, out) is det.
:- mode adjust_func_arity(in, out, in) is det.

	% Get the last two arguments from the list, failing if there
	% aren't at least two arguments.
:- pred get_state_args(list(T), list(T), T, T).
:- mode get_state_args(in, out, out, out) is semidet.

	% Get the last two arguments from the list, aborting if there
	% aren't at least two arguments.
:- pred get_state_args_det(list(T), list(T), T, T).
:- mode get_state_args_det(in, out, out, out) is det.

:- implementation.

make_n_fresh_vars(BaseName, N, VarSet0, Vars, VarSet) :-
	make_n_fresh_vars_2(BaseName, 0, N, VarSet0, Vars, VarSet).

:- pred make_n_fresh_vars_2(string, int, int, varset(T), list(var(T)),
		varset(T)).
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

adjust_func_arity(predicate, Arity, Arity).
adjust_func_arity(function, Arity - 1, Arity).

get_state_args(Args0, Args, State0, State) :-
	list__reverse(Args0, RevArgs0),
	RevArgs0 = [State, State0 | RevArgs],
	list__reverse(RevArgs, Args).

get_state_args_det(Args0, Args, State0, State) :-
	( get_state_args(Args0, Args1, State0A, StateA) ->
		Args = Args1,
		State0 = State0A,
		State = StateA
	;
		error("hlds_pred__get_state_args_det")
	).

%-----------------------------------------------------------------------------%

	% Predicates to check whether a given predicate 
	% is an Aditi query.

:- interface.

:- pred hlds_pred__is_base_relation(module_info, pred_id).
:- mode hlds_pred__is_base_relation(in, in) is semidet.

:- pred hlds_pred__is_derived_relation(module_info, pred_id).
:- mode hlds_pred__is_derived_relation(in, in) is semidet.

	% Is the given predicate a base or derived Aditi relation.
:- pred hlds_pred__is_aditi_relation(module_info, pred_id).
:- mode hlds_pred__is_aditi_relation(in, in) is semidet.

	% Is the predicate `aditi:aggregate_compute_initial', declared
	% in extras/aditi/aditi.m.
	% Special code is generated for each call to this in rl_gen.m.
:- pred hlds_pred__is_aditi_aggregate(module_info, pred_id).
:- mode hlds_pred__is_aditi_aggregate(in, in) is semidet.

:- pred hlds_pred__pred_info_is_aditi_relation(pred_info).
:- mode hlds_pred__pred_info_is_aditi_relation(in) is semidet.

:- pred hlds_pred__pred_info_is_aditi_aggregate(pred_info).
:- mode hlds_pred__pred_info_is_aditi_aggregate(in) is semidet.

:- pred hlds_pred__pred_info_is_base_relation(pred_info).
:- mode hlds_pred__pred_info_is_base_relation(in) is semidet.

	% Aditi can optionally memo the results of predicates
	% between calls to reduce redundant computation.
:- pred hlds_pred__is_aditi_memoed(module_info, pred_id).
:- mode hlds_pred__is_aditi_memoed(in, in) is semidet.

	% Differential evaluation is a method of evaluating recursive
	% Aditi predicates which uses the just new tuples in each
	% iteration where possible rather than the full relations,
	% reducing the sizes of joins.
:- pred hlds_pred__is_differential(module_info, pred_id).
:- mode hlds_pred__is_differential(in, in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

hlds_pred__is_base_relation(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	hlds_pred__pred_info_is_base_relation(PredInfo).

hlds_pred__pred_info_is_base_relation(PredInfo) :-
	pred_info_get_markers(PredInfo, Markers),
	check_marker(Markers, base_relation).

hlds_pred__is_derived_relation(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	check_marker(Markers, aditi),
	\+ hlds_pred__pred_info_is_base_relation(PredInfo),
	\+ hlds_pred__pred_info_is_aditi_aggregate(PredInfo).

hlds_pred__is_aditi_relation(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	hlds_pred__pred_info_is_aditi_relation(PredInfo).

hlds_pred__pred_info_is_aditi_relation(PredInfo) :-
	pred_info_get_markers(PredInfo, Markers),
	check_marker(Markers, aditi).

hlds_pred__is_aditi_aggregate(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	hlds_pred__pred_info_is_aditi_aggregate(PredInfo).

hlds_pred__pred_info_is_aditi_aggregate(PredInfo) :-
	pred_info_module(PredInfo, Module),
	pred_info_name(PredInfo, Name),
	pred_info_arity(PredInfo, Arity),
	hlds_pred__aditi_aggregate(Module, Name, Arity).

:- pred hlds_pred__aditi_aggregate(sym_name, string, int).
:- mode hlds_pred__aditi_aggregate(in, in, in) is semidet.

hlds_pred__aditi_aggregate(unqualified("aditi"),
		"aggregate_compute_initial", 5).

hlds_pred__is_aditi_memoed(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	(
		check_marker(Markers, aditi_memo)
	;
		% Memoing is the default for Aditi procedures.
		semidet_fail, 	% XXX leave it off for now to
				% reduce memory usage.
		check_marker(Markers, aditi),
		\+ check_marker(Markers, aditi_no_memo)
	).

hlds_pred__is_differential(ModuleInfo, PredId) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	(
		check_marker(Markers, psn)
	;
		% Predicate semi-naive evaluation is the default. 
		check_marker(Markers, aditi),
		\+ check_marker(Markers, naive)
	).

%-----------------------------------------------------------------------------%
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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
