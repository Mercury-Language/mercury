%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
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
%	  (perhaps it ought to also resolve function overloading,
%	  converting unifications that are function calls into
%	  HLDS call instructions, but currently that is still done
%	  in modecheck_unify.m)
%
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
:- import_module hlds_module, hlds_pred, io.
:- import_module list, prog_data.

	% Check that the all of the types which have been inferred
	% for the variables in the clause do not contain any unbound type
	% variables other than those that occur in the types of head
	% variables, and that there are no unsatisfied type class
	% constraints.
	%
:- pred post_typecheck__check_type_bindings(pred_id, pred_info, pred_info,
		module_info, int, io__state, io__state).
:- mode post_typecheck__check_type_bindings(in, in, out, in, out, di, uo)
		is det.

	% Handle any unresolved overloading for a predicate call.
	%
:- pred post_typecheck__resolve_pred_overloading(pred_id, list(prog_var),
		pred_info, module_info, sym_name, sym_name, pred_id).
:- mode post_typecheck__resolve_pred_overloading(in, in, in, in, in,
		out, out) is det.

	% Do the stuff needed to initialize the proc_infos so that
	% a pred is ready for mode checking (copy clauses from the
	% clause_info to the proc_info, etc.)
	% Also check that all predicates with an `aditi' marker have
	% an `aditi:state' argument.
	%
:- pred post_typecheck__finish_pred(module_info, pred_id, pred_info, pred_info,
		io__state, io__state).
:- mode post_typecheck__finish_pred(in, in, in, out, di, uo) is det.

:- pred post_typecheck__finish_imported_pred(module_info, pred_id,
		pred_info, pred_info, io__state, io__state).
:- mode post_typecheck__finish_imported_pred(in, in, in, out, di, uo) is det.

:- pred post_typecheck__finish_ill_typed_pred(module_info, pred_id,
		pred_info, pred_info, io__state, io__state).
:- mode post_typecheck__finish_ill_typed_pred(in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module typecheck, clause_to_proc, mode_util, inst_match.
:- import_module mercury_to_mercury, prog_out, hlds_out, type_util.
:- import_module globals, options.

:- import_module map, set, assoc_list, bool, std_util, term.

%-----------------------------------------------------------------------------%
%			Check for unbound type variables
%
%  Check that the all of the types which have been inferred
%  for the variables in the clause do not contain any unbound type
%  variables other than those that occur in the types of head
%  variables, and that there are no unsatisfied type class constraints.

post_typecheck__check_type_bindings(PredId, PredInfo0, PredInfo, ModuleInfo,
		NumErrors, IOState0, IOState) :-
	pred_info_get_unproven_body_constraints(PredInfo0, UnprovenConstraints0),
	( UnprovenConstraints0 \= [] ->
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
	ClausesInfo0 = clauses_info(VarSet, B, VarTypesMap0, HeadVars, E),
	map__to_assoc_list(VarTypesMap0, VarTypesList),
	set__init(Set0),
	check_type_bindings_2(VarTypesList, HeadTypeParams,
			[], Errs, Set0, Set),
	( Errs = [] ->
		PredInfo = PredInfo0,
		IOState2 = IOState1
	;
		%
		% report the warning
		%
		report_unresolved_type_warning(Errs, PredId, PredInfo0,
				ModuleInfo, VarSet, IOState1, IOState2),

		%
		% bind all the type variables in `Set' to `void' ...
		%
		pred_info_context(PredInfo0, Context),
		bind_type_vars_to_void(Set, Context, VarTypesMap0, VarTypesMap),
		ClausesInfo = clauses_info(VarSet, B, VarTypesMap, HeadVars, E),
		pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo)
	),

	%
	% Check that all Aditi predicates have an `aditi__state' argument.
	% This must be done after typechecking because of type inference --
	% the types of some Aditi predicates may not be known before.
	%
	pred_info_get_markers(PredInfo, Markers),
	pred_info_arg_types(PredInfo, ArgTypes),
	( check_marker(Markers, aditi) ->
		list__filter(type_is_aditi_state, ArgTypes, AditiStateTypes),
		( AditiStateTypes = [] ->
			report_no_aditi_state(PredInfo, IOState2, IOState)
		; AditiStateTypes = [_, _ | _] ->
			report_multiple_aditi_states(PredInfo,
				IOState2, IOState)
		;
			IOState = IOState2
		)
	;
		IOState = IOState2
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
:- pred bind_type_vars_to_void(set(tvar), prog_context,
				map(prog_var, type), map(prog_var, type)).
:- mode bind_type_vars_to_void(in, in, in, out) is det.

bind_type_vars_to_void(UnboundTypeVarsSet, Context,
		VarTypesMap0, VarTypesMap) :-
	%
	% first create a pair of corresponding lists (UnboundTypeVars, Voids)
	% that map the unbound type variables to void
	%
	set__to_sorted_list(UnboundTypeVarsSet, UnboundTypeVars),
	list__length(UnboundTypeVars, Length),
	Void = term__functor(term__atom("void"), [], Context),
	list__duplicate(Length, Void, Voids),

	%
	% then apply the substitution we just created to the variable types
	%
	map__keys(VarTypesMap0, Vars),
	map__values(VarTypesMap0, Types0),
	term__substitute_corresponding_list(UnboundTypeVars, Voids,
		Types0, Types),
	map__from_corresponding_lists(Vars, Types, VarTypesMap).

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
	io__write_list(Constraints, ", ", mercury_output_constraint(TVarSet)),
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
        (   invalid_pred_id(PredId0) ->
		%
		% Find the set of candidate pred_ids for predicates which
		% have the specified name and arity
		% 
		pred_info_typevarset(CallerPredInfo, TVarSet),
		pred_info_clauses_info(CallerPredInfo, ClausesInfo),
		ClausesInfo = clauses_info(_, _, VarTypes, _, _),
		typecheck__resolve_pred_overloading(ModuleInfo, Args0,
			VarTypes, TVarSet, PredName0, PredName, PredId)
        ;
                PredId = PredId0,
                PredName = PredName0
        ).

%-----------------------------------------------------------------------------%

	% 
	% Copy clauses to procs, then ensure that all 
	% constructors occurring in predicate mode 
	% declarations are module qualified.
	% 
post_typecheck__finish_pred(ModuleInfo, PredId, PredInfo1, PredInfo) -->
	{ maybe_add_default_mode(ModuleInfo, PredInfo1, PredInfo2, _) },
	{ copy_clauses_to_procs(PredInfo2, PredInfo3) },
	post_typecheck__propagate_types_into_modes(ModuleInfo, PredId,
			PredInfo3, PredInfo).

	%
	% For ill-typed preds, we just need to set the modes up correctly
	% so that any calls to that pred from correctly-typed predicates
	% won't result in spurious mode errors.
	%
post_typecheck__finish_ill_typed_pred(ModuleInfo, PredId,
		PredInfo0, PredInfo) -->
	{ maybe_add_default_mode(ModuleInfo, PredInfo0, PredInfo1, _) },
	post_typecheck__propagate_types_into_modes(ModuleInfo, PredId,
		PredInfo1, PredInfo).

	% 
	% For imported preds, we just need to ensure that all
	% constructors occurring in predicate mode declarations are
	% module qualified.
	% 
post_typecheck__finish_imported_pred(ModuleInfo, PredId,
		PredInfo0, PredInfo) -->
	post_typecheck__propagate_types_into_modes(ModuleInfo, PredId,
		PredInfo0, PredInfo).

	% 
	% Ensure that all constructors occurring in predicate mode
	% declarations are module qualified.
	% 
:- pred post_typecheck__propagate_types_into_modes(module_info, pred_id,
		pred_info, pred_info, io__state, io__state).
:- mode post_typecheck__propagate_types_into_modes(in, in, in, out, di, uo)
		is det.
post_typecheck__propagate_types_into_modes(ModuleInfo, PredId, PredInfo0,
		PredInfo) -->
	{ pred_info_arg_types(PredInfo0, ArgTypes) },
	{ pred_info_procedures(PredInfo0, Procs0) },
	{ pred_info_procids(PredInfo0, ProcIds) },

	propagate_types_into_proc_modes(ModuleInfo, PredId, ProcIds, ArgTypes,
			Procs0, Procs),
	{ pred_info_set_procedures(PredInfo0, Procs, PredInfo) }.

%-----------------------------------------------------------------------------%

:- pred propagate_types_into_proc_modes(module_info,
		pred_id, list(proc_id), list(type), proc_table, proc_table,
		io__state, io__state).
:- mode propagate_types_into_proc_modes(in,
		in, in, in, in, out, di, uo) is det.		

propagate_types_into_proc_modes(_, _, [], _, Procs, Procs) --> [].
propagate_types_into_proc_modes(ModuleInfo, PredId,
		[ProcId | ProcIds], ArgTypes, Procs0, Procs) -->
	{ map__lookup(Procs0, ProcId, ProcInfo0) },
	{ proc_info_argmodes(ProcInfo0, ArgModes0) },
	{ propagate_types_into_mode_list(ArgTypes, ModuleInfo,
		ArgModes0, ArgModes) },
	%
	% check for unbound inst vars
	% (this needs to be done after propagate_types_into_mode_list,
	% because we need the insts to be module-qualified; and it
	% needs to be done before mode analysis, to avoid internal errors)
	%
	( { mode_list_contains_inst_var(ArgModes, ModuleInfo, _InstVar) } ->
		unbound_inst_var_error(PredId, ProcInfo0, ModuleInfo),
		% delete this mode, to avoid internal errors
		{ map__det_remove(Procs0, ProcId, _, Procs1) }
	;
		{ proc_info_set_argmodes(ProcInfo0, ArgModes, ProcInfo) },
		{ map__det_update(Procs0, ProcId, ProcInfo, Procs1) }
	),
	propagate_types_into_proc_modes(ModuleInfo, PredId, ProcIds,
		ArgTypes, Procs1, Procs).

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

:- pred report_no_aditi_state(pred_info, io__state, io__state).
:- mode report_no_aditi_state(in, di, uo) is det.

report_no_aditi_state(PredInfo) -->
	io__set_exit_status(1),
	{ pred_info_context(PredInfo, Context) },
	prog_out__write_context(Context),
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	io__write_string("Error: `:- pragma aditi' declaration for "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" "),
	hlds_out__write_pred_call_id(qualified(Module, Name)/Arity),
	io__write_string("  without an `aditi:state' argument.\n").

:- pred report_multiple_aditi_states(pred_info, io__state, io__state).
:- mode report_multiple_aditi_states(in, di, uo) is det.

report_multiple_aditi_states(PredInfo) -->
	io__set_exit_status(1),
	{ pred_info_context(PredInfo, Context) },
	prog_out__write_context(Context),
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	io__write_string("Error: `:- pragma aditi' declaration for "),
	hlds_out__write_pred_or_func(PredOrFunc),
	io__write_string(" "),
	hlds_out__write_pred_call_id(qualified(Module, Name)/Arity),
	io__nl,
	prog_out__write_context(Context),
	io__write_string("  with multiple `aditi:state' arguments.\n").

%-----------------------------------------------------------------------------%
