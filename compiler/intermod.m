%-----------------------------------------------------------------------------%
% Copyright (C) 1996 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: intermod.m
% main author: stayl
%
% This module writes out the interface for inter-module optimization.
% The .opt file includes:
%	- The clauses for exported preds that can be inlined.
%	- The clauses for exported preds that have higher-order pred arguments.
%	- The pred/mode declarations for local predicates that the
%	  above clauses use.
% 	- Non-exported types, insts and modes used by the above.
%	- :- import_module declarations to import stuff used by the above.
%	- pragma declarations for the exported preds.
%	- pragma c_header declarations if any pragma_c_code preds are written.
% All these items should be module qualified.
% Constructors should be explicitly type qualified.
%
% This module also contains predicates to read in the .opt files and
% to adjust the import status of local predicates which are exported for
% intermodule optimization.
%	
% Note that predicates which call predicates that do not have mode or
% determinism declarations do not have clauses exported, since this would
% require running mode analysis and determinism analysis before writing the
% .opt file, significantly increasing compile time for a very small gain.
%
%-----------------------------------------------------------------------------%

:- module intermod.

%-----------------------------------------------------------------------------%

:- interface.

:- import_module io.
:- import_module hlds_module, modules.

:- pred intermod__write_optfile(module_info, module_info,
				io__state, io__state).
:- mode intermod__write_optfile(in, out, di, uo) is det.

	% Add the items from the .opt files of imported modules to
	% the items for this module.
:- pred intermod__grab_optfiles(module_imports, module_imports, bool,
				io__state, io__state).
:- mode intermod__grab_optfiles(in, out, out, di, uo) is det.

	% Make sure that local preds which have been exported in the .opt
	% file get an exported(_) label.
:- pred intermod__adjust_pred_import_status(module_info, module_info).
:- mode intermod__adjust_pred_import_status(in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list, bool, dir, getopt, int, list, map, require, set.
:- import_module std_util, string, term, varset.

:- import_module code_util, globals, goal_util.
:- import_module hlds_data, hlds_goal, hlds_pred, hlds_out, inlining, llds.
:- import_module mercury_to_mercury, mode_util, modules.
:- import_module options, passes_aux, prog_data, prog_io, prog_out, prog_util.
:- import_module special_pred, typecheck, type_util.

%-----------------------------------------------------------------------------%

% Open the file "<module-name>.opt", and write out the
% declarations and clauses for intermodule optimization.

intermod__write_optfile(ModuleInfo0, ModuleInfo) -->
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ string__append(ModuleName, ".opt.tmp", TmpName) },
	io__tell(TmpName, Result2),
	(
		{ Result2 = error(Err2) },
		{ io__error_message(Err2, Msg2) },
		io__stderr_stream(ErrStream2),
		io__write_string(ErrStream2, Msg2),
		io__set_exit_status(1),
		{ ModuleInfo = ModuleInfo0 }
	;
		{ Result2 = ok },
		{ module_info_predids(ModuleInfo0, PredIds) },
		{ init_intermod_info(ModuleInfo0, IntermodInfo0) },
		globals__io_lookup_int_option(
			intermod_inline_simple_threshold, Threshold),
		{ intermod__gather_preds(PredIds, yes, Threshold,
				IntermodInfo0, IntermodInfo1) },
		{ intermod__gather_abstract_exported_types(IntermodInfo1,
				IntermodInfo2) },
		{ intermod_info_get_pred_decls(PredDeclsSet,
				IntermodInfo2, IntermodInfo3) },
		{ intermod_info_get_module_info(ModuleInfo1,
				IntermodInfo3, IntermodInfo4) },
		{ module_info_insts(ModuleInfo1, Insts) },
		{ inst_table_get_user_insts(Insts, UserInsts) },
		{ user_inst_table_get_inst_defns(UserInsts, InstDefns) },
		{ module_info_modes(ModuleInfo1, Modes) },
		{ mode_table_get_mode_defns(Modes, ModeDefns) },
		{ set__to_sorted_list(PredDeclsSet, PredDecls) },
		{ intermod__gather_modes(ModuleInfo1, ModeDefns, InstDefns,
				PredDecls, IntermodInfo4, IntermodInfo) },
		intermod__write_intermod_info(IntermodInfo),
		io__told,
		{ string__append(ModuleName, ".opt", OptName) },
		update_interface(OptName),
		globals__io_lookup_bool_option(intermod_unused_args,
			UnusedArgs),
		( { UnusedArgs = yes } ->
			{ do_adjust_pred_import_status(IntermodInfo,
				ModuleInfo1, ModuleInfo) }
		;
			{ ModuleInfo = ModuleInfo1 }
		),
		touch_interface_datestamp(ModuleName, ".optdate")
	).

	% a collection of stuff to go in the .opt file
:- type intermod_info
		 ---> info(
			set(module_name),	% modules to import
			set(pred_id), 		% preds to output clauses for
			set(pred_id),	 	% preds to output decls for
			set(type_id), 		% local types to export
			set(mode_id), 		% local modes to export
			set(inst_id), 		% local insts to export
			module_info,
			bool,			% do the c_header_codes for
				% the module need writing, yes if there
				% are pragma_c_code procs being exported
			map(var, type),		% Vartypes and tvarset for the
			tvarset			% current pred
		).

:- pred init_intermod_info(module_info::in, intermod_info::out) is det.

init_intermod_info(ModuleInfo, IntermodInfo) :-
	set__init(Modules),
	set__init(Procs),
	set__init(ProcDecls),
	set__init(Types),
	set__init(Insts),
	set__init(Modes),
	map__init(VarTypes),
	varset__init(TVarSet),
	IntermodInfo = info(Modules, Procs, ProcDecls, Types, Modes,
				Insts, ModuleInfo, no, VarTypes, TVarSet).
			
%-----------------------------------------------------------------------------%
	% Predicates to gather stuff to output to .opt file.

:- pred intermod__gather_preds(list(pred_id)::in, bool::in, int::in,
		intermod_info::in, intermod_info::out) is det.

intermod__gather_preds([], _CollectTypes, _) --> [].
intermod__gather_preds([PredId | PredIds], CollectTypes, InlineThreshold) -->
	intermod_info_get_module_info(ModuleInfo0),
	{ module_info_preds(ModuleInfo0, PredTable0) },
	{ map__lookup(PredTable0, PredId, PredInfo0) },
	(
		{ pred_info_is_exported(PredInfo0) },
		{ pred_info_procids(PredInfo0, [ProcId | _ProcIds]) },
		{ pred_info_procedures(PredInfo0, Procs) },
		{ map__lookup(Procs, ProcId, ProcInfo) },
		{ proc_info_goal(ProcInfo, Goal) },
		(
			% Don't export builtins since they will be
			% recreated in the importing module anyway.
			{ \+ code_util__compiler_generated(PredInfo0) },
			{ \+ code_util__predinfo_is_builtin(PredInfo0) },
			(
				{ inlining__is_simple_goal(Goal,
						InlineThreshold) }
			;
				{ pred_info_requested_inlining(PredInfo0) }
			;
				{ has_ho_input(ModuleInfo0, ProcInfo) }
			)
		)
	->
		{ pred_info_clauses_info(PredInfo0, ClausesInfo0) },
		{ pred_info_typevarset(PredInfo0, TVarSet) },
		{ ClausesInfo0 = clauses_info(VarSet, DeclTypes, VarTypes,
						HeadVars, Clauses0) },
		intermod_info_set_var_types(VarTypes),
		intermod_info_set_tvarset(TVarSet),
		intermod__traverse_clauses(Clauses0, Clauses, DoWrite),
		( { DoWrite = yes } ->
			{ ClausesInfo = clauses_info(VarSet, DeclTypes,
					VarTypes, HeadVars, Clauses) },
			{ pred_info_set_clauses_info(PredInfo0, ClausesInfo,
					PredInfo) },	
			{ map__det_update(PredTable0, PredId,
					PredInfo, PredTable) },
			{ module_info_set_preds(ModuleInfo0, PredTable,
					ModuleInfo) },
			intermod_info_get_preds(Preds0),
			( { pred_info_get_goal_type(PredInfo, pragmas) } ->
				% The header code must be written since
				% it could be used by the pragma_c_code.
				intermod_info_set_write_header
			;
				[]
			),
			{ set__insert(Preds0, PredId, Preds) },
			intermod_info_set_preds(Preds),
			( { CollectTypes = yes } ->
				{ module_info_types(ModuleInfo, TypeTable) },
				{ map__values(VarTypes, Types) },
				intermod__gather_types(ModuleInfo,
						TypeTable, Types)
			;
				[]
			),
			intermod_info_set_module_info(ModuleInfo)
		;
			[]
		)
	;
		[]
	),
	intermod__gather_preds(PredIds, CollectTypes, InlineThreshold).

:- pred intermod__traverse_clauses(list(clause)::in, list(clause)::out,
		bool::out, intermod_info::in, intermod_info::out) is det.

intermod__traverse_clauses([], [], yes) --> [].
intermod__traverse_clauses([clause(P, Goal0, C) | Clauses0],
			[clause(P, Goal, C) | Clauses], DoWrite) -->
	intermod__traverse_goal(Goal0, Goal, DoWrite1),
	( { DoWrite1 = yes } ->
		intermod__traverse_clauses(Clauses0, Clauses, DoWrite)
	;
		{ Clauses = Clauses0 },
		{ DoWrite = no }
	).


:- pred has_ho_input(module_info::in, proc_info::in) is semidet.

has_ho_input(ModuleInfo, ProcInfo) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_argmodes(ProcInfo, ArgModes),
	proc_info_vartypes(ProcInfo, VarTypes),
	check_for_ho_input_args(ModuleInfo, HeadVars, ArgModes, VarTypes).

:- pred check_for_ho_input_args(module_info::in, list(var)::in,
		list(mode)::in, map(var, type)::in) is semidet.

check_for_ho_input_args(ModuleInfo, [HeadVar | HeadVars],
			[ArgMode | ArgModes], VarTypes) :-
	(
		mode_is_input(ModuleInfo, ArgMode),
		map__lookup(VarTypes, HeadVar, Type),
		classify_type(Type, ModuleInfo, pred_type)
	;
		check_for_ho_input_args(ModuleInfo, HeadVars,
							ArgModes, VarTypes)
	).

	% Add all local types used in a type to the intermod info.
	% It may be sufficient (and much more efficient! to just export
	% the definitions of all local types).
:- pred intermod__gather_types(module_info::in, type_table::in, list(type)::in,
			intermod_info::in, intermod_info::out) is det.

intermod__gather_types(_ModuleInfo, _TypeTable, []) --> [].
intermod__gather_types(ModuleInfo, TypeTable, [TypeToCheck | TypesToCheck]) -->
	(
		{ type_to_type_id(TypeToCheck, TypeId, ArgTypes) },
		{ map__search(TypeTable, TypeId, TypeDefn) }
	->
		{ hlds_data__get_type_defn_status(TypeDefn, Status) },
		( { Status = imported ; Status = abstract_imported } ->
			{ type_util__type_id_module(ModuleInfo,
						TypeId, Module) },
			intermod_info_get_modules(Modules0),
			{ set__insert(Modules0, Module, Modules) },
			intermod_info_set_modules(Modules)
		; { Status = local ; Status = abstract_exported } ->
			intermod_info_get_types(TypesToExport0),
			{ set__insert(TypesToExport0, TypeId,
						TypesToExport) },
			intermod_info_set_types(TypesToExport)
		;
			[]
		),
		intermod__gather_types(ModuleInfo, TypeTable, ArgTypes)
	;
		[]
	),
	intermod__gather_types(ModuleInfo, TypeTable, TypesToCheck).


	% All equivalence types that only have a :- type foo. in the
	% interface section need to be exported in full. All other
	% types of type will be exported by intermod__gather_types.
:- pred intermod__gather_abstract_exported_types(intermod_info::in,
					intermod_info::out) is det.

intermod__gather_abstract_exported_types -->
	intermod_info_get_module_info(ModuleInfo),
	{ module_info_types(ModuleInfo, Types) },
	{ map__to_assoc_list(Types, TypeList) },
	{ AddAbstractEquivType =
		lambda([TypeAndDefn::in, Info0::in, Info::out] is det, (
			TypeAndDefn = TypeId - TypeDefn,
			hlds_data__get_type_defn_status(TypeDefn, Status),
			hlds_data__get_type_defn_body(TypeDefn, Body),
			( 
				Body = eqv_type(EqvType),
				Status = abstract_exported
			->
				intermod__gather_types(ModuleInfo, Types,
						[EqvType], Info0, Info1),
				intermod_info_get_types(TypesToExport0,
						Info1, Info2),
				set__insert(TypesToExport0, TypeId,
						TypesToExport),
				intermod_info_set_types(TypesToExport,
						Info2, Info)
			;
				Info = Info0
			)
		)) },
	list__foldl(AddAbstractEquivType, TypeList).


	% Go over the goal of an exported proc looking for proc decls, types,
	% insts and modes that we need to write to the optfile.
:- pred intermod__traverse_goal(hlds_goal::in, hlds_goal::out, bool::out,
			intermod_info::in, intermod_info::out) is det.

intermod__traverse_goal(conj(Goals0) - Info, conj(Goals) - Info, DoWrite) -->
	intermod__traverse_list_of_goals(Goals0, Goals, DoWrite).

intermod__traverse_goal(disj(Goals0, SM) - Info, disj(Goals, SM) - Info,
		DoWrite) -->
	intermod__traverse_list_of_goals(Goals0, Goals, DoWrite).

intermod__traverse_goal(
	call(PredId0, B, Args, D, MaybeUnifyContext, PredName0) - Info,
	call(PredId, B, Args, D, MaybeUnifyContext, PredName) - Info, DoWrite)
		-->
	intermod_info_get_module_info(ModuleInfo),
	intermod_info_get_var_types(VarTypes),
	intermod_info_get_tvarset(TVarSet),
	( { invalid_pred_id(PredId0) } ->
		{ typecheck__resolve_pred_overloading(ModuleInfo, Args,
			VarTypes, TVarSet, PredName0, PredName1, PredId) }
	;
		{ PredId = PredId0 },
		{ PredName1 = PredName0 }
	),	
	(
		{ PredName1 = qualified(_, _) },
		{ PredName = PredName1 }
	;
		{ PredName1 = unqualified(Name) },
		{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
		{ pred_info_module(PredInfo, PredModule) },
		{ PredName = qualified(PredModule, Name) }
	),
	(
		% We don't need to export complicated unification
		% pred declarations, since they will be recreated when 
		% mode analysis is run on the importing module.
		{ MaybeUnifyContext = no }
	->
		intermod_info_add_proc(PredId, DoWrite)
	;
		{ DoWrite = yes }
	).

intermod__traverse_goal(higher_order_call(A,B,C,D,E) - Info,
			higher_order_call(A,B,C,D,E) - Info, yes) --> [].

intermod__traverse_goal(switch(A, B, Cases0, D) - Info,
		switch(A, B, Cases, D) - Info, DoWrite) -->
	intermod__traverse_cases(Cases0, Cases, DoWrite).

	% Export declarations for preds used in higher order pred constants
	% or function calls.
intermod__traverse_goal(unify(LVar, RHS0, C, D, E) - Info,
			unify(LVar, RHS, C, D, E) - Info, DoWrite) -->
	intermod__module_qualify_unify_rhs(LVar, RHS0, RHS, DoWrite).

intermod__traverse_goal(not(Goal0) - Info, not(Goal) - Info, DoWrite) -->
	intermod__traverse_goal(Goal0, Goal, DoWrite).

intermod__traverse_goal(some(Vars, Goal0) - Info, some(Vars, Goal) - Info,
		DoWrite) -->
	intermod__traverse_goal(Goal0, Goal, DoWrite).

intermod__traverse_goal(if_then_else(Vars, Cond0, Then0, Else0, SM) - Info,
		if_then_else(Vars, Cond, Then, Else, SM) - Info, DoWrite) -->
	intermod__traverse_goal(Cond0, Cond, DoWrite1),
	intermod__traverse_goal(Then0, Then, DoWrite2),
	intermod__traverse_goal(Else0, Else, DoWrite3),
	{ bool__and_list([DoWrite1, DoWrite2, DoWrite3], DoWrite) }.

	% Inlineable exported pragma_c_code goals can't use any
	% non-exported types, so we just write out the clauses. 
intermod__traverse_goal(pragma_c_code(A,B,C,D,E,F,G,H) - Info,
			pragma_c_code(A,B,C,D,E,F,G,H) - Info, yes) --> [].


:- pred intermod__traverse_list_of_goals(hlds_goals::in, hlds_goals::out,
		bool::out, intermod_info::in, intermod_info::out) is det.

intermod__traverse_list_of_goals([], [], yes) --> [].
intermod__traverse_list_of_goals([Goal0 | Goals0], [Goal | Goals], DoWrite) -->
	intermod__traverse_goal(Goal0, Goal, DoWrite1),
	( { DoWrite1 = yes } ->
		intermod__traverse_list_of_goals(Goals0, Goals, DoWrite)
	;
		{ DoWrite = no },
		{ Goals = Goals0 }
	).

:- pred intermod__traverse_cases(list(case)::in, list(case)::out, bool::out,
			intermod_info::in, intermod_info::out) is det.

intermod__traverse_cases([], [], yes) --> [].
intermod__traverse_cases([case(F, Goal0) | Cases0],
		[case(F, Goal) | Cases], DoWrite) -->
	intermod__traverse_goal(Goal0, Goal, DoWrite1),
	( { DoWrite1 = yes } ->
		intermod__traverse_cases(Cases0, Cases, DoWrite)
	;
		{ DoWrite = no },
		{ Cases = Cases0 }
	).

	% If a proc called within an exported proc is non-local, we need
	% to include an :- import_module declaration.
:- pred intermod_info_add_proc(pred_id::in, bool::out,
		intermod_info::in, intermod_info::out) is det.

intermod_info_add_proc(PredId, DoWrite) -->
	intermod_info_get_module_info(ModuleInfo),
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_import_status(PredInfo, Status) },
	{ pred_info_procids(PredInfo, ProcIds) },
	{ pred_info_get_marker_list(PredInfo, Markers) },
	( { list__member(request(infer_modes), Markers) } ->
		% Don't write this pred if it calls preds without mode decls.
		{ DoWrite = no }
	; 
		{
		pred_info_procedures(PredInfo, Procs),
		list__member(ProcId, ProcIds),
		map__lookup(Procs, ProcId, ProcInfo),
		proc_info_declared_determinism(ProcInfo, no)
		}
	->
		% Don't write this pred if it calls preds
		% without determinism decls.
		{ DoWrite = no }
	;
		{ Status = local }
	->
		intermod_info_get_pred_decls(PredDecls0),
		{ set__insert(PredDecls0, PredId, PredDecls) },
		intermod_info_set_pred_decls(PredDecls),
		{ DoWrite = yes }
	;
		{ Status = imported }
	->
		{ DoWrite = yes },
		{ pred_info_module(PredInfo, Module) },
		( { module_info_name(ModuleInfo, Module) } ->
			% :- external pred - add decl 
			intermod_info_get_pred_decls(PredDecls0),
			{ set__insert(PredDecls0, PredId, PredDecls) },
			intermod_info_set_pred_decls(PredDecls)
		;	
			% imported pred - add import for module
			intermod_info_get_modules(Modules0),
			{ set__insert(Modules0, Module, Modules) },
			intermod_info_set_modules(Modules)
		)
	;
		{ DoWrite = yes }
	).

	% Resolve overloading and module qualify everything in a unify_rhs.
:- pred intermod__module_qualify_unify_rhs(var::in, unify_rhs::in,
		unify_rhs::out, bool::out, intermod_info::in,
		intermod_info::out) is det.

intermod__module_qualify_unify_rhs(_, var(Var), var(Var), yes) --> [].
intermod__module_qualify_unify_rhs(_LVar, lambda_goal(A,B,Modes,D,Goal0),
		lambda_goal(A,B,Modes,D,Goal), DoWrite) -->
	intermod__traverse_goal(Goal0, Goal, DoWrite),
	intermod_info_get_module_info(ModuleInfo),
	{ module_info_modes(ModuleInfo, ModeTable) },
	{ mode_table_get_mode_defns(ModeTable, ModeDefns) },
	{ module_info_insts(ModuleInfo, Insts) },
	{ inst_table_get_user_insts(Insts, UserInsts) },
	{ user_inst_table_get_inst_defns(UserInsts, UserInstDefns) },
	intermod__gather_proc_modes(ModuleInfo, ModeDefns,
				UserInstDefns, Modes).

	% Check if the functor is a function call, a higher-order
	% term, or an unqualified symbol. If so, module qualify.
	% For function calls and higher-order terms, call intermod__add_proc
	% so that the predicate or function will be exported if necessary.
intermod__module_qualify_unify_rhs(LVar, functor(Functor0, Vars),
				functor(Functor, Vars), DoWrite) -->
	intermod_info_get_module_info(ModuleInfo),
	{ module_info_get_predicate_table(ModuleInfo, PredTable) },
	intermod_info_get_tvarset(TVarSet),
	intermod_info_get_var_types(VarTypes),
	(
		%
		% Is it a function call?
		%
		{ Functor0 = cons(FuncName, Arity) },
		{ predicate_table_search_func_sym_arity(PredTable,
				FuncName, Arity, PredIds) },
		{ list__append(Vars, [LVar], FuncArgs) },
		{ map__apply_to_list(FuncArgs, VarTypes, FuncArgTypes) },
		{ typecheck__find_matching_pred_id(PredIds, ModuleInfo,
			TVarSet, FuncArgTypes, PredId, QualifiedFuncName) }
	->
		%
		% Yes, it is a function call.
		% Module-qualify it.
		% Make sure that the called function will be exported.
		%
		{ Functor = cons(QualifiedFuncName, Arity) },
		intermod_info_add_proc(PredId, DoWrite)
	;
		%
		% Is this a higher-order predicate or higher-order function
		% term?
		%
		{ Functor0 = cons(PredName, Arity) },
		intermod_info_get_var_types(VarTypes),
		{ map__lookup(VarTypes, LVar, LVarType) },
		{ type_is_higher_order(LVarType, PredOrFunc, PredArgTypes) }
	->
		%
		% Yes, the unification creates a higher-order term.
		% Make sure that the predicate/function is exported.
		%
		{ map__apply_to_list(Vars, VarTypes, Types) },
		{ list__append(Types, PredArgTypes, ArgTypes) },
		{ get_pred_id_and_proc_id(PredName, PredOrFunc,
			TVarSet, ArgTypes, ModuleInfo, PredId, _ProcId) },
		intermod_info_add_proc(PredId, DoWrite),
		%
		% Module-qualify it, if necessary.
		%
		{ PredName = unqualified(UnqualPredName) ->
			predicate_module(ModuleInfo, PredId, Module),
			QualifiedPredName = qualified(Module, UnqualPredName),
			Functor = cons(QualifiedPredName, Arity)
		;
			Functor = Functor0
		}
	;
		%
		% Is it an unqualified functor symbol?
		%
		{ Functor0 = cons(unqualified(ConsName), ConsArity) },
		{ map__lookup(VarTypes, LVar, VarType) },
		{ type_to_type_id(VarType, TypeId, _) },
		{ TypeId = qualified(TypeModule, _) - _ }
	->
		{ Functor = cons(qualified(TypeModule, ConsName), ConsArity) },
		{ DoWrite = yes }
	;
		{ Functor = Functor0 },
		{ DoWrite = yes }
	).

%-----------------------------------------------------------------------------%
	% Gather all the user defined modes and insts used by all the
	% local predicates we are exporting.
:- pred intermod__gather_modes(module_info::in, mode_defns::in,
		user_inst_defns::in, list(pred_id)::in,
		intermod_info::in, intermod_info::out) is det.

intermod__gather_modes(_, _, _, []) --> [].
intermod__gather_modes(ModuleInfo, Modes, Insts, [PredId | PredIds]) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_procids(PredInfo, ProcIds) },
	{ pred_info_procedures(PredInfo, Procs) },
	intermod__gather_pred_modes(ModuleInfo, Modes, Insts, Procs, ProcIds),
	intermod__gather_modes(ModuleInfo, Modes, Insts, PredIds).


:- pred intermod__gather_pred_modes(module_info::in, mode_defns::in,
		user_inst_defns::in, proc_table::in, list(proc_id)::in,
		intermod_info::in, intermod_info::out) is det.

intermod__gather_pred_modes(_, _, _, _, []) --> [].
intermod__gather_pred_modes(ModuleInfo, Modes, Insts, Procs, [ProcId | ProcIds])
		-->
	{ map__lookup(Procs, ProcId, ProcInfo) }, 
	{ proc_info_declared_argmodes(ProcInfo, ArgModes) },
	intermod__gather_proc_modes(ModuleInfo, Modes, Insts, ArgModes),
	intermod__gather_pred_modes(ModuleInfo, Modes, Insts, Procs, ProcIds).

	% Get the modes from pred and func declarations.
:- pred intermod__gather_proc_modes(module_info::in, mode_defns::in,
		user_inst_defns::in, list(mode)::in,
		intermod_info::in, intermod_info::out) is det.

intermod__gather_proc_modes(_, _, _, []) --> [].
intermod__gather_proc_modes(ModuleInfo, ModeTable,
			UserInstTable, [Mode | Modes]) -->
	{ mode_get_insts(ModuleInfo, Mode, Inst1, Inst2) },
	intermod__gather_insts(UserInstTable, [Inst1, Inst2]),
	( { Mode = user_defined_mode(Name, Args) } ->
		intermod__gather_insts(UserInstTable, Args),
		{ list__length(Args, Arity) },
		{ ModeId = Name - Arity },
		{ map__lookup(ModeTable, ModeId, ModeDefn) },
		{ ModeDefn = hlds_mode_defn(_,_,_,_,_, Status) },
		( { Status = local } ->
			intermod_info_get_modes(ModesToExport0),
			{ set__insert(ModesToExport0, ModeId,
							ModesToExport) },
			intermod_info_set_modes(ModesToExport) 
		; { Status = imported } ->
			(
				{ Name = qualified(Module, _) },
				intermod_info_get_modules(Modules0),
				{ set__insert(Modules0, Module, Modules) },
				intermod_info_set_modules(Modules)
			;
				{ Name = unqualified(_) }
			)
		;
			[]
		)
	;
		[]
	),
	intermod__gather_proc_modes(ModuleInfo, ModeTable,
						UserInstTable, Modes).

:- pred intermod__gather_insts(user_inst_defns::in, list((inst))::in,
			intermod_info::in, intermod_info::out) is det.

intermod__gather_insts(_, []) --> [].
intermod__gather_insts(UserInstTable, [Inst | Insts]) -->
	intermod__add_inst(UserInstTable, Inst),
	intermod__gather_insts(UserInstTable, Insts).

:- pred intermod__add_inst(user_inst_defns::in, (inst)::in,
			intermod_info::in, intermod_info::out) is det.

intermod__add_inst(UserInstTable, Inst) -->
	(
		{ Inst = defined_inst(InstName) },
		{ InstName = user_inst(Name, InstArgs) }
	->
		intermod__gather_insts(UserInstTable, InstArgs),
		{ list__length(InstArgs, Arity) },
		{ InstId = Name - Arity },
		{ map__lookup(UserInstTable, InstId, InstDefn) },
		{ InstDefn = hlds_inst_defn(_,_,_,_,_, Status) },
		( { Status = local } ->
			intermod_info_get_insts(InstsToExport0),
			{ set__insert(InstsToExport0, InstId,
							InstsToExport) },
			intermod_info_set_insts(InstsToExport)
		; { Status = imported } ->
			( { Name = qualified(Module, _) } ->
				intermod_info_get_modules(Modules0),
				{ set__insert(Modules0, Module, Modules) },
				intermod_info_set_modules(Modules)
			;
				{ error("unqualified imported inst") }
			)
		;
			[]
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%
	% Output module imports, types, modes, insts and predicates

:- pred intermod__write_intermod_info(intermod_info::in,
				io__state::di, io__state::uo) is det.

intermod__write_intermod_info(IntermodInfo) -->
	{ IntermodInfo = info(Modules0, Preds0, PredDecls0, Types0,
			Modes0, Insts0, ModuleInfo, WriteHeader, _, _) },
	{ set__to_sorted_list(Modules0, Modules) },
	{ set__to_sorted_list(Preds0, Preds) }, 
	{ set__to_sorted_list(PredDecls0, PredDecls) },
	{ set__to_sorted_list(Types0, Types) },
	{ set__to_sorted_list(Modes0, Modes) },
	{ set__to_sorted_list(Insts0, Insts) },
	{ module_info_name(ModuleInfo, ModName) },
	io__write_string(":- module "),
	mercury_output_bracketed_constant(term__atom(ModName)),
	io__write_string(".\n"),
	( { Modules \= [] } ->
		io__write_string(":- use_module "),
		intermod__write_modules(Modules)
	;
		[]
	),
	% Disable verbose dumping of clauses.
	globals__io_lookup_string_option(verbose_dump_hlds, VerboseDump),
	globals__io_set_option(verbose_dump_hlds, string("")),
	( { WriteHeader = yes } ->
		{ module_info_get_c_header(ModuleInfo, CHeader) },
		intermod__write_c_header(CHeader)
	;
		[]
	),
	{ module_info_types(ModuleInfo, TypeTable) },
	intermod__write_types(ModuleInfo, TypeTable, Types),
	{ module_info_modes(ModuleInfo, ModeTable) },
	{ mode_table_get_mode_defns(ModeTable, ModeDefns) },
	intermod__write_modes(ModuleInfo, ModeDefns, Modes),
	{ module_info_insts(ModuleInfo, InstTable) },
	{ inst_table_get_user_insts(InstTable, UserInstTable) },
	{ user_inst_table_get_inst_defns(UserInstTable, InstDefns) },
	intermod__write_insts(ModuleInfo, InstDefns, Insts),
	intermod__write_pred_decls(ModuleInfo, PredDecls),
	intermod__write_preds(ModuleInfo, Preds),
	globals__io_set_option(verbose_dump_hlds, string(VerboseDump)).


:- pred intermod__write_modules(list(module_name)::in,
			io__state::di, io__state::uo) is det.

intermod__write_modules([]) --> [].
intermod__write_modules([Module | Rest]) -->
	mercury_output_bracketed_constant(term__atom(Module)),
	(
		{ Rest = [] },
		io__write_string(".\n")
	;
		{ Rest = [_ | _] },
		io__write_string(", "),
		intermod__write_modules(Rest)
	).

:- pred intermod__write_c_header(list(c_header_code)::in,
				io__state::di, io__state::uo) is det.

intermod__write_c_header([]) --> [].
intermod__write_c_header([Header - _ | Headers]) -->
        intermod__write_c_header(Headers),
        mercury_output_pragma_c_header(Header).

:- pred intermod__write_types(module_info::in, type_table::in,
		list(type_id)::in, io__state::di, io__state::uo) is det.

intermod__write_types(_, _, []) --> [].
intermod__write_types(ModuleInfo, TypeTable, [TypeId | TypeIds]) -->
	{ TypeId = Name - _Arity },
	{ map__lookup(TypeTable, TypeId, TypeDefn) },
	{ hlds_data__get_type_defn_tvarset(TypeDefn, VarSet) },
	{ hlds_data__get_type_defn_tparams(TypeDefn, Args) },
	{ hlds_data__get_type_defn_body(TypeDefn, Body) },
	{ hlds_data__get_type_defn_context(TypeDefn, Context) },
	(
		{ Body = du_type(Ctors, _, _) },
		mercury_output_type_defn(VarSet, du_type(Name, Args, Ctors),
				Context)
	;
		{ Body = uu_type(_) },
		{ error("uu types not implemented") }
	;
		{ Body = eqv_type(EqvType) },
		mercury_output_type_defn(VarSet,
				eqv_type(Name, Args, EqvType), Context)
	;
		{ Body = abstract_type },
		mercury_output_type_defn(VarSet, abstract_type(Name, Args),
				Context)
	),
	intermod__write_types(ModuleInfo, TypeTable, TypeIds).

:- pred intermod__write_modes(module_info::in, mode_defns::in,
		list(mode_id)::in, io__state::di, io__state::uo) is det.

intermod__write_modes(_, _, []) --> [].
intermod__write_modes(ModuleInfo, ModeTable, [ModeId | Modes]) -->
	{ ModeId = SymName - _Arity },
	{ map__lookup(ModeTable, ModeId, ModeDefn) },
	{ ModeDefn = hlds_mode_defn(Varset, Args, eqv_mode(Mode),
							_, Context, _) },
	mercury_output_mode_defn(
			Varset,
			eqv_mode(SymName, Args, Mode),
			Context
	),
	intermod__write_modes(ModuleInfo, ModeTable, Modes).

:- pred intermod__write_insts(module_info::in, user_inst_defns::in, 
		list(inst_id)::in, io__state::di, io__state::uo) is det.

intermod__write_insts(_, _, []) --> [].
intermod__write_insts(ModuleInfo, UserInstTable, [Inst | Insts]) -->
	io__write_char(':'),
	{ Inst = SymName - _Arity },
	{ map__lookup(UserInstTable, Inst, InstDefn) },
	{ InstDefn = hlds_inst_defn(Varset, Args, Body, _, Context, _) },
	(
		{ Body = eqv_inst(Inst2) },
		mercury_output_inst_defn(
				Varset,
				eqv_inst(SymName, Args, Inst2),
				Context
		)
	;
		{ Body = abstract_inst },
		mercury_output_inst_defn(
				Varset,
				abstract_inst(SymName, Args),
				Context
		)
	),
	io__write_string(".\n"),
	intermod__write_insts(ModuleInfo, UserInstTable, Insts).

	% We need to write all the declarations for local predicates so
	% the procedure labels for the C code are calculated correctly.
:- pred intermod__write_pred_decls(module_info::in, list(pred_id)::in,
			io__state::di, io__state::uo) is det.

intermod__write_pred_decls(_, []) --> [].
intermod__write_pred_decls(ModuleInfo, [PredId | PredIds]) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arg_types(PredInfo, TVarSet, ArgTypes) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	(
		{ PredOrFunc = predicate },
		mercury_output_pred_type(TVarSet, qualified(Module, Name),
					ArgTypes, no, Context)
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgTypes, FuncArgTypes, FuncRetType) },
		mercury_output_func_type(TVarSet,
			qualified(Module, Name), FuncArgTypes,
			FuncRetType, no, Context)
	),
	{ pred_info_procedures(PredInfo, Procs) },
	{ pred_info_procids(PredInfo, ProcIds) },
		% Make sure the mode declarations go out in the same
		% order they came in, so that the all the modes get the
		% same proc_id in the importing modules.
	{ CompareProcId =
		 lambda([ProcId1::in, ProcId2::in, Result::out] is det, (
			proc_id_to_int(ProcId1, ProcInt1),
			ActualProcId1 is ProcInt1 mod 10000,
			proc_id_to_int(ProcId2, ProcInt2),
			ActualProcId2 is ProcInt2 mod 10000,
			compare(Result, ActualProcId1, ActualProcId2)
		)) },
	{ list__sort(CompareProcId, ProcIds, SortedProcIds) },
	intermod__write_pred_modes(Procs, qualified(Module, Name),
					PredOrFunc, SortedProcIds),
	intermod__write_pred_decls(ModuleInfo, PredIds).

:- pred intermod__write_pred_modes(map(proc_id, proc_info)::in, 
		sym_name::in, pred_or_func::in, list(proc_id)::in,
		io__state::di, io__state::uo) is det.

intermod__write_pred_modes(_, _, _, []) --> [].
intermod__write_pred_modes(Procs, SymName, PredOrFunc, [ProcId | ProcIds]) -->
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_maybe_declared_argmodes(ProcInfo, MaybeArgModes) },
	{ proc_info_declared_determinism(ProcInfo, MaybeDetism) },
	{ MaybeArgModes = yes(ArgModes0), MaybeDetism = yes(Detism0) ->
		ArgModes = ArgModes0,
		Detism = Detism0
	;
		error("intermod__write_pred_modes: attempt to write undeclared mode")
	},
	{ proc_info_context(ProcInfo, Context) },
	{ varset__init(Varset) },
	(
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ArgModes, FuncArgModes, FuncRetMode) },
		mercury_output_func_mode_decl(Varset, SymName,
			FuncArgModes, FuncRetMode,
			yes(Detism), Context)
	;
		{ PredOrFunc = predicate },
		mercury_output_pred_mode_decl(Varset, SymName,
				ArgModes, yes(Detism), Context)
	),
	intermod__write_pred_modes(Procs, SymName, PredOrFunc, ProcIds).	

:- pred intermod__write_preds(module_info::in, list(pred_id)::in,
				io__state::di, io__state::uo) is det.

intermod__write_preds(_, []) --> [].
intermod__write_preds(ModuleInfo, [PredId | PredIds]) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_arg_types(PredInfo, _, ArgTypes) },
	{ list__length(ArgTypes, Arity) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ SymName = qualified(Module, Name) },
	{ pred_info_get_marker_list(PredInfo, Markers) },
	intermod__write_pragmas(SymName, Arity, Markers),
	{ pred_info_clauses_info(PredInfo, ClausesInfo) },
	{ ClausesInfo = clauses_info(Varset, _, _VarTypes, HeadVars, Clauses) },
		% handle pragma(c_code, ...) separately
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	( { pred_info_get_goal_type(PredInfo, pragmas) } ->
		{ pred_info_procedures(PredInfo, Procs) },
		intermod__write_c_code(SymName, PredOrFunc, HeadVars, Varset,
						Clauses, Procs)
	;
		% { pred_info_typevarset(PredInfo, TVarSet) },
		hlds_out__write_clauses(1, ModuleInfo, PredId, Varset, no,
			HeadVars, PredOrFunc, Clauses, no)
		%	HeadVars, Clauses, yes(TVarSet, VarTypes))
	),
	intermod__write_preds(ModuleInfo, PredIds).

:- pred intermod__write_pragmas(sym_name::in, int::in, list(marker_status)::in,
				io__state::di, io__state::uo) is det.

intermod__write_pragmas(_, _, []) --> [].
intermod__write_pragmas(SymName, Arity, [MarkerStatus | Markers]) -->
	(
		{ MarkerStatus = request(Marker) }
	;
		{ MarkerStatus = done(Marker) }
	),
	(
		\+ (
			% Since the inferred declarations are output, these
			% don't need to be done in the importing module.
			{ Marker = infer_type }
		 ;	{ Marker = infer_modes }
		)
	->
		{ hlds_out__marker_name(Marker, Name) },
		mercury_output_pragma_decl(SymName, Arity, Name)
	;
		[]
	),
	intermod__write_pragmas(SymName, Arity, Markers).

	% Some pretty kludgy stuff to get c code written correctly.
:- pred intermod__write_c_code(sym_name::in, pred_or_func::in, 
	list(var)::in, varset::in,
	list(clause)::in, proc_table::in, io__state::di, io__state::uo) is det.

intermod__write_c_code(_, _, _, _, [], _) --> [].
intermod__write_c_code(SymName, PredOrFunc, HeadVars, Varset, 
		[Clause | Clauses], Procs) -->
	{ Clause = clause(ProcIds, Goal, _) },
	(
		(
			% Pull the C code out of the goal.
			{ Goal = conj(Goals) - _ },
			{ list__filter(
				lambda([X::in] is semidet, (
					X = pragma_c_code(_,_,_,_,_,_,_,_) - _
				)),
				Goals, [CCodeGoal]) },
			{ CCodeGoal = pragma_c_code(CCode, MayCallMercury,
						_, _, Vars, _, _, _) - _ }
		;
			{ Goal = pragma_c_code(CCode, MayCallMercury,
						_, _, Vars, _, _, _) - _ }
		)
	->	
		intermod__write_c_clauses(Procs, ProcIds, PredOrFunc, CCode, 
					MayCallMercury, Vars, Varset, SymName)
	;
		{ error("intermod__write_c_code called with non c_code goal") }
	),
	intermod__write_c_code(SymName, PredOrFunc, HeadVars, Varset, 
				Clauses, Procs).

:- pred intermod__write_c_clauses(proc_table::in, list(proc_id)::in, 
		pred_or_func::in, string::in, may_call_mercury::in,
		list(var)::in, varset::in, sym_name::in,
		io__state::di, io__state::uo) is det.

intermod__write_c_clauses(_, [], _, _, _, _, _, _) --> [].
intermod__write_c_clauses(Procs, [ProcId | ProcIds], PredOrFunc,
			CCode, MayCallMercury, Vars, Varset, SymName) -->
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_maybe_declared_argmodes(ProcInfo, MaybeArgModes) },
	( { MaybeArgModes = yes(ArgModes) } ->
		{ get_pragma_c_code_vars(Vars, Varset, ArgModes, PragmaVars) },
		% XXX will need modification for nondet pragma C code
		mercury_output_pragma_c_code(MayCallMercury, SymName,
			PredOrFunc, PragmaVars, no, Varset, CCode),
		intermod__write_c_clauses(Procs, ProcIds, PredOrFunc, CCode,
			MayCallMercury, Vars, Varset, SymName)
	;
		{ error("intermod__write_c_clauses: no mode declaration") }
	).

:- pred get_pragma_c_code_vars(list(var)::in, varset::in,
		list(mode)::in, list(pragma_var)::out) is det.

get_pragma_c_code_vars(HeadVars, VarNames,
			ArgModes, PragmaVars) :- 
	(
		HeadVars = [Var | Vars],
		ArgModes = [Mode | Modes]
	->
		varset__lookup_name(VarNames, Var, Name),
		PragmaVar = pragma_var(Var, Name, Mode),
		get_pragma_c_code_vars(Vars, VarNames, Modes, PragmaVars1),
		PragmaVars = [PragmaVar | PragmaVars1] 
	;
		HeadVars = [],
		ArgModes = []
	->
		PragmaVars = []
	;
		error("intermod:get_pragma_c_code_vars")
	).

%-----------------------------------------------------------------------------%
	% Access predicates.

:- pred intermod_info_get_modules(set(module_name)::out, intermod_info::in,
			intermod_info::out) is det.
:- pred intermod_info_get_preds(set(pred_id)::out, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_pred_decls(set(pred_id)::out, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_types(set(type_id)::out, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_modes(set(mode_id)::out, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_insts(set(inst_id)::out, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_module_info(module_info::out,
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_write_c_header(bool::out,
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_var_types(map(var, type)::out,
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_get_tvarset(tvarset::out, intermod_info::in,
			intermod_info::out) is det.

intermod_info_get_modules(Modules)	--> =(info(Modules,_,_,_,_,_,_,_,_,_)). 
intermod_info_get_preds(Procs)		--> =(info(_,Procs,_,_,_,_,_,_,_,_)).
intermod_info_get_pred_decls(ProcDecls) -->
					=(info(_,_,ProcDecls,_,_,_,_,_,_,_)).
intermod_info_get_types(Types)		--> =(info(_,_,_,Types,_,_,_,_,_,_)).
intermod_info_get_modes(Modes)		--> =(info(_,_,_,_,Modes,_,_,_,_,_)).
intermod_info_get_insts(Insts)		--> =(info(_,_,_,_,_,Insts,_,_,_,_)).
intermod_info_get_module_info(Module)	--> =(info(_,_,_,_,_,_,Module,_,_,_)).
intermod_info_get_write_c_header(Write)	--> =(info(_,_,_,_,_,_,_,Write,_,_)).
intermod_info_get_var_types(VarTypes)	--> =(info(_,_,_,_,_,_,_,_,VarTypes,_)).
intermod_info_get_tvarset(TVarSet)	--> =(info(_,_,_,_,_,_,_,_,_,TVarSet)).


:- pred intermod_info_set_modules(set(module_name)::in,
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_preds(set(pred_id)::in, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_pred_decls(set(pred_id)::in, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_types(set(type_id)::in, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_modes(set(mode_id)::in, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_insts(set(inst_id)::in, 
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_module_info(module_info::in,
			intermod_info::in, intermod_info::out) is det.
:- pred intermod_info_set_write_header(intermod_info::in,
			intermod_info::out) is det.
:- pred intermod_info_set_var_types(map(var, type)::in, intermod_info::in, 
			intermod_info::out) is det.
:- pred intermod_info_set_tvarset(tvarset::in, intermod_info::in,
			intermod_info::out) is det.

intermod_info_set_modules(Modules, info(_,B,C,D,E,F,G,H,I,J),
				info(Modules, B,C,D,E,F,G,H,I,J)).

intermod_info_set_preds(Procs, info(A,_,C,D,E,F,G,H,I,J),
				info(A, Procs, C,D,E,F,G,H,I,J)).

intermod_info_set_pred_decls(ProcDecls, info(A,B,_,D,E,F,G,H,I,J),
				info(A,B, ProcDecls, D,E,F,G,H,I,J)).

intermod_info_set_types(Types, info(A,B,C,_,E,F,G,H,I,J),
				info(A,B,C, Types, E,F,G,H,I,J)).

intermod_info_set_modes(Modes, info(A,B,C,D,_,F,G,H,I,J),
				info(A,B,C,D, Modes, F,G,H,I,J)).

intermod_info_set_insts(Insts, info(A,B,C,D,E,_,G,H,I,J),
				info(A,B,C,D,E, Insts, G,H,I,J)).

intermod_info_set_module_info(ModuleInfo, info(A,B,C,D,E,F,_,H,I,J),
				info(A,B,C,D,E,F, ModuleInfo, H,I,J)).

intermod_info_set_write_header(info(A,B,C,D,E,F,G,_,I,J),
				 info(A,B,C,D,E,F,G, yes,I,J)).

intermod_info_set_var_types(VarTypes, info(A,B,C,D,E,F,G,H,_,J),
				info(A,B,C,D,E,F,G,H,VarTypes,J)).

intermod_info_set_tvarset(TVarSet, info(A,B,C,D,E,F,G,H,I,_),
				info(A,B,C,D,E,F,G,H,I, TVarSet)).

%-----------------------------------------------------------------------------%

	% Make sure the labels of local preds needed by predicates in 
	% the .opt file are exported, and inhibit dead proc elimination
	% on those preds.
intermod__adjust_pred_import_status(Module0, Module) :-
	init_intermod_info(Module0, Info0),
	module_info_predids(Module0, PredIds),
	module_info_globals(Module0, Globals),
	globals__lookup_int_option(Globals, intermod_inline_simple_threshold, 
			Threshold),
	intermod__gather_preds(PredIds, yes, Threshold, Info0, Info1),
	do_adjust_pred_import_status(Info1, Module0, Module).

:- pred do_adjust_pred_import_status(intermod_info::in,
		module_info::in, module_info::out) is det.

do_adjust_pred_import_status(Info, Module0, Module) :-
	intermod_info_get_pred_decls(PredDecls0, Info, _),
	intermod_info_get_types(TypeIds0, Info, _),
	set__to_sorted_list(PredDecls0, PredDecls),
	set__to_sorted_list(TypeIds0, TypeIds),
	module_info_types(Module0, Types0),
	set_list_of_types_exported(TypeIds, Types0, Types),
	module_info_set_types(Module0, Types, Module1),
	special_pred_list(SpecPredIdList),
	module_info_get_special_pred_map(Module1, SpecPredMap),
	module_info_preds(Module1, Preds0),
	fixup_special_preds(TypeIds, SpecPredIdList,
		SpecPredMap, Preds0, Preds1),
	set_list_of_preds_exported(PredDecls, Preds1, Preds2),
	module_info_set_preds(Module1, Preds2, Module).

:- pred set_list_of_types_exported(list(type_id)::in, type_table::in,
					type_table::out) is det.

set_list_of_types_exported([], Types, Types).
set_list_of_types_exported([TypeId | TypeIds], Types0, Types) :-
	map__lookup(Types0, TypeId, TypeDefn0),
	hlds_data__set_type_defn_status(TypeDefn0, exported, TypeDefn),
	map__det_update(Types0, TypeId, TypeDefn, Types1),
	set_list_of_types_exported(TypeIds, Types1, Types).

:- pred fixup_special_preds(list(type_id)::in, list(special_pred_id)::in,
		special_pred_map::in, pred_table::in, pred_table::out) is det.

fixup_special_preds([], _, _, Preds, Preds).
fixup_special_preds([TypeId | TypeIds], SpecialPredList,
			SpecMap, Preds0, Preds) :-
	list__map(lambda([SpecPredId::in, PredId::out] is det, (
			map__lookup(SpecMap, SpecPredId - TypeId, PredId)
		)), SpecialPredList, NewPredIds),
	set_list_of_preds_exported(NewPredIds, Preds0, Preds1),
	fixup_special_preds(TypeIds, SpecialPredList, SpecMap, Preds1, Preds).


:- pred set_list_of_preds_exported(list(pred_id)::in, pred_table::in,
					pred_table::out) is det.

set_list_of_preds_exported([], Preds, Preds).
set_list_of_preds_exported([PredId | PredIds], Preds0, Preds) :-
	map__lookup(Preds0, PredId, PredInfo0),
	( pred_info_import_status(PredInfo0, local) ->	
		(
			pred_info_name(PredInfo0, "__Unify__"),
			pred_info_arity(PredInfo0, 2)
		->
			NewStatus = pseudo_exported
		;
			NewStatus = exported
		),
		pred_info_set_import_status(PredInfo0, NewStatus, PredInfo),
		map__det_update(Preds0, PredId, PredInfo, Preds1)
	;
		Preds1 = Preds0
	),
	set_list_of_preds_exported(PredIds, Preds1, Preds).

%-----------------------------------------------------------------------------%
	% Read in and process the optimization interfaces.

intermod__grab_optfiles(Module0, Module, FoundError) -->
	{ Module0 = module_imports(ModuleName, DirectImports0,
					IndirectImports0, Items0, _) },
	{ list__sort_and_remove_dups(DirectImports0, DirectImports1) },
	read_optimization_interfaces(DirectImports1, [],
		OptItems, no, OptError),
	{ get_dependencies(OptItems, NewImportDeps0, NewUseDeps0) },
	{ list__append(NewImportDeps0, NewUseDeps0, NewDeps0) },
	{ set__list_to_set(NewDeps0, NewDepsSet0) },
	{ set__delete_list(NewDepsSet0, [ModuleName | DirectImports1],
						NewDepsSet) },
	{ set__to_sorted_list(NewDepsSet, NewDeps) },
	{ Module1 = module_imports(ModuleName, DirectImports1,
					IndirectImports0, [], no) },
	process_module_interfaces(NewDeps, [], Module1, Module2),
	{ Module2 = module_imports(_, DirectImports, IndirectImports,
		InterfaceItems, IntError) },
	{ list__append(OptItems, InterfaceItems, NewItems) },
	{ term__context_init("blah", 0, Context) },
		% Let make_hlds know the opt_imported stuff is coming.
	globals__io_lookup_bool_option(intermod_unused_args, UnusedArgs),
	{ varset__init(Varset) },

		%
		% Get the :- pragma unused_args(...) declarations created
		% when writing the .opt file for the current module. These
		% are needed because we can probably remove more arguments
		% with intermod_unused_args, but the interface for other
		% modules must remain the same.
		%

	( { UnusedArgs = yes } ->
		read_optimization_interfaces([ModuleName], [],
				LocalItems, no, UAError),
		{ IsPragmaUnusedArgs = lambda([Item::in] is semidet, (
					Item = pragma(PragmaType) - _,
					PragmaType = unused_args(_,_,_,_,_)
				)) },
		{ list__filter(IsPragmaUnusedArgs, LocalItems, PragmaItems) }
	;
		{ PragmaItems = [] },
		{ UAError = no }
	),
	{ ( IntError \= no ; OptError = yes ; UAError = yes) ->
		FoundError = yes
	;
		FoundError = no
	},
	{ OptDefn = module_defn(Varset, opt_imported) - Context },
	{ list__append([OptDefn | PragmaItems], NewItems, NewItems2) }, 
	{ list__append(Items0, NewItems2, Items) },
	{ Module = module_imports(ModuleName, DirectImports,
				IndirectImports, Items, no) }.


:- pred read_optimization_interfaces(list(module_name)::in, item_list::in,
			item_list::out, bool::in, bool::out,
			io__state::di, io__state::uo) is det.

read_optimization_interfaces([], Items, Items, Error, Error) --> [].
read_optimization_interfaces([Import | Imports],
		Items0, Items, Error0, Error) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose,
			"% Reading optimization interface for module"),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, Import),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	maybe_write_string(VeryVerbose, "% done.\n"),

	{ string__append(Import, ".opt", FileName) },
	prog_io__read_module(FileName, Import, yes,
			ModuleError, Messages, Items1),
	update_error_status(FileName, ModuleError, Messages, Error0, Error1),
	{ list__append(Items0, Items1, Items2) },
	read_optimization_interfaces(Imports, Items2, Items, Error1, Error).

:- pred update_error_status(string::in, module_error::in, message_list::in, 
		bool::in, bool::out, io__state::di, io__state::uo) is det.

update_error_status(FileName, ModuleError, Messages, Error0, Error1) -->
	(
		{ ModuleError = no },
		{ Error1 = Error0 }
	;
		{ ModuleError = yes },
		prog_out__write_messages(Messages),
		{ Error1 = yes }
	;
		{ ModuleError = fatal },
		globals__io_lookup_bool_option(warn_missing_opt_files, DoWarn),
		( { DoWarn = yes } ->
			io__write_string("Warning: cannot open `"),
			io__write_string(FileName),
			io__write_string("'.\n"),
			globals__io_lookup_bool_option(halt_at_warn,
					HaltAtWarn),
			{ HaltAtWarn = yes ->
				Error1 = yes
			;
				Error1 = Error0
			}
		;
			{ Error1 = Error0 }	
		)
	).

%-----------------------------------------------------------------------------%
