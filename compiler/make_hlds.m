%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: make_hlds.m.
% Main author: fjh.

% This module converts from the parse tree structure which is read in by
% prog_io.m, into the simplified high level data structure defined in
% hlds.m.  In the parse tree, the program is represented as a list of
% items; we insert each item into the appropriate symbol table, and report
% any duplicate definition errors.  We also transform clause bodies from
% (A,B,C) into conj([A,B,C]) form, convert all unifications into
% super-homogenous form, and introduce implicit quantification.
% 
% XXX we should record each error using module_info_incr_errors.

% WISHLIST - we should handle explicit module quantification

:- module make_hlds.
:- interface.

:- import_module prog_data, hlds_module, hlds_pred, hlds_goal, hlds_data.
:- import_module equiv_type, module_qual, globals.

:- import_module io, std_util, list, bool.

% parse_tree_to_hlds(ParseTree, MQInfo, EqvMap, HLDS, UndefTypes, UndefModes):
%	Given MQInfo (returned by module_qual.m) and EqvMap (returned by
%	equiv_type.m), converts ParseTree to HLDS.
%	Any errors found are recorded in the HLDS num_errors field.
%	Returns UndefTypes = yes if undefined types found.
%	Returns UndefModes = yes if undefined modes found.
:- pred parse_tree_to_hlds(compilation_unit, mq_info, eqv_map, module_info,
			bool, bool, io__state, io__state).
:- mode parse_tree_to_hlds(in, in, in, out, out, out, di, uo) is det.

:- pred create_atomic_unification(prog_var, unify_rhs, prog_context,
			unify_main_context, unify_sub_contexts, hlds_goal).
:- mode create_atomic_unification(in, in, in, in, in, out) is det.

:- pred add_new_proc(pred_info, arity, argument_modes, maybe(argument_modes),
		maybe(list(is_live)), maybe(determinism),
		prog_context, args_method, pred_info, proc_id).
:- mode add_new_proc(in, in, in, in, in, in, in, in, out, out) is det.

:- pred clauses_info_init(int::in, clauses_info::out) is det.

:- pred next_mode_id(proc_table, maybe(determinism), proc_id).
:- mode next_mode_id(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module prog_io, prog_io_goal, prog_io_dcg, prog_io_util, prog_out.
:- import_module modules, module_qual, prog_util, options, hlds_out.
:- import_module make_tags, quantification, (inst), instmap, term, varset.
:- import_module code_util, unify_proc, special_pred, type_util, mode_util.
:- import_module mercury_to_mercury, passes_aux, clause_to_proc, inst_match.
:- import_module fact_table, purity, goal_util, term_util, export, llds.

:- import_module string, char, int, set, bintree, map, multi_map, require.
:- import_module getopt, assoc_list, term_io.

parse_tree_to_hlds(module(Name, Items), MQInfo0, EqvMap, Module, 
		UndefTypes, UndefModes) -->
	globals__io_get_globals(Globals),
	{ module_info_init(Name, Globals, Module0) },
	add_item_list_decls_pass_1(Items,
		item_status(local, may_be_unqualified), Module0, Module1),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
	add_item_list_decls_pass_2(Items,
		item_status(local, may_be_unqualified), Module1, Module2),
	maybe_report_stats(Statistics),
		% balance the binary trees
	{ module_info_optimize(Module2, Module3) },
	maybe_report_stats(Statistics),
	{ init_qual_info(MQInfo0, EqvMap, Info0) },
	add_item_list_clauses(Items, local, Module3, Module4,
				Info0, Info),
	{ qual_info_get_mq_info(Info, MQInfo) },
	{ mq_info_get_type_error_flag(MQInfo, UndefTypes) },
	{ mq_info_get_mode_error_flag(MQInfo, UndefModes) },
	{ mq_info_get_num_errors(MQInfo, MQ_NumErrors) },
	{ module_info_num_errors(Module4, NumErrors0) },
	{ NumErrors is NumErrors0 + MQ_NumErrors },
	{ module_info_set_num_errors(Module4, NumErrors, Module5) },
		% the predid list is constructed in reverse order, for
		% efficiency, so we return it to the correct order here.
	{ module_info_reverse_predids(Module5, Module) }.

%-----------------------------------------------------------------------------%

	% When adding an item to the HLDS we need to know both its 
	% import_status and whether uses of it must be module qualified.
:- type item_status
	---> item_status(import_status, need_qualifier).

%-----------------------------------------------------------------------------%

	% pass 1:
	% Add the declarations one by one to the module,
	% except for type definitions and pragmas.

:- pred add_item_list_decls_pass_1(item_list, item_status,
				module_info, module_info,
				io__state, io__state).
:- mode add_item_list_decls_pass_1(in, in, in, out, di, uo) is det.

add_item_list_decls_pass_1([], _, Module, Module) --> [].
add_item_list_decls_pass_1([Item - Context | Items], Status0, Module0, Module)
		-->
	add_item_decl_pass_1(Item, Context, Status0, Module0, Status1, Module1),
	add_item_list_decls_pass_1(Items, Status1, Module1, Module).

	% pass 2:
	% Add the type definitions and pragmas one by one to the module,
	% and add default modes for functions with no mode declaration.
	%
	% Adding type definitions needs to come after we have added the
	% pred declarations,
	% since we need to have the pred_id for `index/2' and `compare/3'
	% when we add compiler-generated clauses for `compare/3'.
	% (And similarly for other compiler-generated predicates like that.)
	%
	% Adding pragmas needs to come after we have added the
	% pred declarations, in order to allow the pragma declarations 
	% for a predicate to syntactically precede the pred declaration.
	%
	% Adding default modes for functions needs to come after we have
	% processed all the mode declarations, since otherwise we can't be 
	% sure that there isn't a mode declaration for the function.

:- pred add_item_list_decls_pass_2(item_list, item_status,
		module_info, module_info, io__state, io__state).
:- mode add_item_list_decls_pass_2(in, in, in, out, di, uo) is det.

add_item_list_decls_pass_2([], _, Module, Module) --> [].
add_item_list_decls_pass_2([Item - Context | Items], Status0, Module0, Module)
		-->
	add_item_decl_pass_2(Item, Context, Status0, Module0, Status1, Module1),
	add_item_list_decls_pass_2(Items, Status1, Module1, Module).

	% pass 3:
	% add the clauses one by one to the module
	% (I supposed this could conceivably be folded into pass 2?)

:- pred add_item_list_clauses(item_list, import_status, module_info,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode add_item_list_clauses(in, in, in, out, in, out, di, uo) is det.

add_item_list_clauses([], _Status, Module, Module, Info, Info) --> [].
add_item_list_clauses([Item - Context | Items], Status0,
		Module0, Module, Info0, Info) -->
	add_item_clause(Item, Status0, Status1, Context,
			Module0, Module1, Info0, Info1),
	add_item_list_clauses(Items, Status1, Module1, Module, Info1, Info).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl_pass_1(item, prog_context, item_status,
		module_info, item_status, module_info, io__state, io__state).
:- mode add_item_decl_pass_1(in, in, in, in, out, out, di, uo) is det.

	% skip clauses
add_item_decl_pass_1(pred_clause(_, _, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_1(func_clause(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].

add_item_decl_pass_1(type_defn(_, _, _), _, Status, Module, Status, Module)
		--> [].

add_item_decl_pass_1(inst_defn(VarSet, InstDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Context,
			Status, Module).

add_item_decl_pass_1(mode_defn(VarSet, ModeDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Context,
			Status, Module).

add_item_decl_pass_1(pred(TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, MaybeDet, Cond, Purity, ClassContext),
		Context, Status, Module0, Status, Module) -->
	{ init_markers(Markers) },
	module_add_pred(Module0, TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, MaybeDet, Cond, Purity, ClassContext, Markers,
		Context, Status, _, Module).

add_item_decl_pass_1(func(TypeVarSet, InstVarSet, ExistQVars, FuncName,
		TypesAndModes, RetTypeAndMode, MaybeDet, Cond, Purity,
		ClassContext), Context, Status, Module0, Status, Module) -->
	{ init_markers(Markers) },
	module_add_func(Module0, TypeVarSet, InstVarSet, ExistQVars, FuncName,
		TypesAndModes, RetTypeAndMode, MaybeDet, Cond, Purity,
		ClassContext, Markers, Context, Status, _, Module).

add_item_decl_pass_1(pred_mode(VarSet, PredName, Modes, MaybeDet, Cond),
		Context, Status, Module0, Status, Module) -->
	module_add_mode(Module0, VarSet, PredName, Modes, MaybeDet, Cond,
		Context, predicate, _, Module).

add_item_decl_pass_1(func_mode(VarSet, FuncName, Modes, RetMode, MaybeDet,
		Cond), Context, Status, Module0, Status, Module) -->
	{ Modes = argument_modes(InstTable, ArgModes) },
	{ list__append(ArgModes, [RetMode], ArgModes1) },
	module_add_mode(Module0, VarSet, FuncName,
		argument_modes(InstTable, ArgModes1), MaybeDet, Cond,
		Context, function, _, Module).

add_item_decl_pass_1(pragma(_), _, Status, Module, Status, Module) --> [].

add_item_decl_pass_1(module_defn(_VarSet, ModuleDefn), Context,
		Status0, Module0, Status, Module) -->
	( { module_defn_update_import_status(ModuleDefn, Status1) } ->
		{ Status = Status1 },
		{ Module = Module0 }
	; { ModuleDefn = import(module(Specifiers)) } ->
		{ Status = Status0 },
		{ Status = item_status(IStat, _) },
		( { IStat = local ; IStat = exported } ->
			{ module_add_imported_module_specifiers(Specifiers,
				Module0, Module) }
		;
			{ Module = Module0 }
		)
	; { ModuleDefn = use(module(Specifiers)) } ->
		{ Status = Status0 },
		{ Status = item_status(IStat, _) },
		( { IStat = local ; IStat = exported } ->
			{ module_add_imported_module_specifiers(Specifiers,
				Module0, Module) }
		;
			{ Module = Module0 }
		)
	; { ModuleDefn = include_module(_) } ->
		{ Status = Status0 },
		{ Module = Module0 }
	; { ModuleDefn = external(External) } ->
		( { External = name_arity(Name, Arity) } ->
			{ Status = Status0 },
			module_mark_as_external(Name, Arity, Context,
				Module0, Module)
		;
			{ Status = Status0 },
			{ Module = Module0 },
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			report_warning("Warning: `external' declaration requires arity.\n"),
			io__set_output_stream(OldStream, _)
		)
	;
		{ Status = Status0 },
		{ Module = Module0 },
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		report_warning("Warning: declaration not yet implemented.\n"),
		io__set_output_stream(OldStream, _)
	).

add_item_decl_pass_1(nothing, _, Status, Module, Status, Module) --> [].

add_item_decl_pass_1(typeclass(Constraints, Name, Vars, Interface, VarSet), 
		Context, Status, Module0, Status, Module) -->
	module_add_class_defn(Module0, Constraints, Name, Vars, Interface,
		VarSet, Context, Status, Module).

	% We add instance declarations on the second pass so that we don't add
	% an instance declaration before its class declaration.
add_item_decl_pass_1(instance(_, _, _, _, _), _, Status, Module, Status,
	Module) --> [].

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl_pass_2(item, prog_context, item_status,
			module_info, item_status, module_info,
			io__state, io__state).
:- mode add_item_decl_pass_2(in, in, in, in, out, out, di, uo) is det.

add_item_decl_pass_2(module_defn(_VarSet, ModuleDefn), _Context,
		Status0, Module, Status, Module) -->
	{ module_defn_update_import_status(ModuleDefn, Status1) ->
		Status = Status1
	;
		Status = Status0
	}.

add_item_decl_pass_2(type_defn(VarSet, TypeDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Context, Status,
		Module).

add_item_decl_pass_2(pragma(Pragma), Context, Status, Module0, Status, Module)
		-->
	%
	% check for invalid pragmas in the `interface' section
	%
	{ Status = item_status(ImportStatus, _) },
	{ pragma_allowed_in_interface(Pragma, Allowed) },
	( { Allowed = no } ->
		check_not_exported(ImportStatus, Context,
			"`pragma' declaration")
	;
		[]
	),

	%
	% switch on the pragma type
	%
	(
		% ignore `pragma source_file' declarations - they're dealt
		% with elsewhere
		{ Pragma = source_file(_) },
		{ Module = Module0 }
	;
		{ Pragma = c_code(C_Body_Code) },
		{ module_add_c_body_code(C_Body_Code, Context,
			Module0, Module) }
	;
		{ Pragma  = c_header_code(C_Header) },
		{ module_add_c_header(C_Header, Context, Module0, Module) }
	;
		% Handle pragma c_code decls later on (when we process
		% clauses).
		{ Pragma = c_code(_, _, _, _, _, _) },
		{ Module = Module0 }
	;	
		% Handle pragma tabled decls later on (when we process
		% clauses).
		{ Pragma = tabled(_, _, _, _, _) },
		{ Module = Module0 }
	;
		{ Pragma = inline(Name, Arity) },
		add_pred_marker(Module0, "inline", Name, Arity, Context,
			inline, [no_inline], Module)
	;
		{ Pragma = no_inline(Name, Arity) },
		add_pred_marker(Module0, "no_inline", Name, Arity, Context,
			no_inline, [inline], Module)
	;
		{ Pragma = obsolete(Name, Arity) },
		add_pred_marker(Module0, "obsolete", Name, Arity, Context,
			obsolete, [], Module)
	;
		% Handle pragma import decls later on (when we process
		% clauses and pragma c_code).
		{ Pragma = import(_, _, _, _, _) },
		{ Module = Module0 }
	;
		{ Pragma = export(Name, PredOrFunc, Modes, C_Function) },
		add_pragma_export(Name, PredOrFunc, Modes, C_Function,
				Context, Module0, Module)
	;
			% Used for inter-module unused argument elimination.
			% This can only appear in .opt files.
		{ Pragma = unused_args(PredOrFunc, SymName,
				Arity, ProcId, UnusedArgs) },
		( { ImportStatus \= opt_imported } ->
			prog_out__write_context(Context),
			io__write_string(
			    "Error: illegal use of pragma `unused_args'.\n"),
			{ module_info_incr_errors(Module0, Module) }
		;
			add_pragma_unused_args(PredOrFunc, SymName, Arity,
				ProcId, UnusedArgs, Context, Module0, Module)
			
		)
	;
		% Handle pragma fact_table decls later on (when we process
		% clauses).
		{ Pragma = fact_table(_, _, _) },
		{ Module = Module0 }
	;
		{ Pragma = promise_pure(Name, Arity) },
		add_pred_marker(Module0, "promise_pure", Name, Arity, Context,
				promised_pure, [], Module)
	;
		{ Pragma = termination_info(PredOrFunc, SymName, Modes, 
			MaybeArgSizeInfo, MaybeTerminationInfo) },
		add_pragma_termination_info(PredOrFunc, SymName, Modes,
			MaybeArgSizeInfo, MaybeTerminationInfo, Context,
			Module0, Module)
	;
		{ Pragma = terminates(Name, Arity) },
		add_pred_marker(Module0, "terminates", Name, Arity,
			Context, terminates,
			[check_termination, does_not_terminate], Module)
	;
		{ Pragma = does_not_terminate(Name, Arity) },
		add_pred_marker(Module0, "does_not_terminate", Name, Arity,
			Context, does_not_terminate,
			[check_termination, terminates], Module)
	;
		{ Pragma = check_termination(Name, Arity) },
		add_pred_marker(Module0, "check_termination", Name, Arity, 
			Context, check_termination, 
			[terminates, does_not_terminate], 
			Module)
	).

add_item_decl_pass_2(func(_TypeVarSet, _InstVarSet, _ExistQVars, FuncName,
		TypesAndModes, _RetTypeAndMode, _MaybeDet, _Cond, _Purity,
		_ClassContext), _Context, Status, Module0, Status, Module) -->
	%
	% add default modes for function declarations, if necessary
	%
	{ TypesAndModes = types_and_modes(_InstTable, TMs) },
	{ list__length(TMs, Arity) },
	{ module_info_get_predicate_table(Module0, PredTable0) },
	(
		{ predicate_table_search_func_sym_arity(PredTable0,
			FuncName, Arity, PredIds) }
	->
		{ predicate_table_get_preds(PredTable0, Preds0) },
		{ maybe_add_default_modes(Module0, PredIds, Preds0, Preds) },
		{ predicate_table_set_preds(PredTable0, Preds, PredTable) },
		{ module_info_set_predicate_table(Module0, PredTable, Module) }
	;
		{ error("make_hlds.m: can't find func declaration") }
	).

add_item_decl_pass_2(func_clause(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(pred_clause(_, _, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(inst_defn(_, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(mode_defn(_, _, _), _, Status, Module, Status, Module)
		--> [].
add_item_decl_pass_2(pred(_, _, _, _, _, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(pred_mode(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(func_mode(_, _, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(nothing, _, Status, Module, Status, Module) --> [].
add_item_decl_pass_2(typeclass(_, _, _, _, _)
	, _, Status, Module, Status, Module) --> [].
add_item_decl_pass_2(instance(Constraints, Name, Types, Interface, VarSet), 
		Context, Status, Module0, Status, Module) -->
	{ Status = item_status(ImportStatus, _) },
	module_add_instance_defn(Module0, Constraints, Name, Types, Interface,
		VarSet, ImportStatus, Context, Module).

%------------------------------------------------------------------------------

	% If a module_defn updates the import_status, return the new
	% status and whether uses of the following items must be module 
	% qualified, otherwise fail.
:- pred module_defn_update_import_status(module_defn::in,
		item_status::out) is semidet.

module_defn_update_import_status(interface,
		item_status(exported, may_be_unqualified)).
module_defn_update_import_status(implementation,
		item_status(local, may_be_unqualified)).
module_defn_update_import_status(private_interface,
		item_status(exported_to_submodules, may_be_unqualified)).
module_defn_update_import_status(imported, 
		item_status(imported, may_be_unqualified)).
module_defn_update_import_status(used, 
		item_status(imported, must_be_qualified)).
module_defn_update_import_status(opt_imported, 
		item_status(opt_imported, must_be_qualified)).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_clause(item, import_status, import_status, prog_context, 
	module_info, module_info, qual_info, qual_info, io__state, io__state).
:- mode add_item_clause(in, in, out, in, in, out, in, out, di, uo) is det.

add_item_clause(func_clause(VarSet, PredName, Args, Result, Body), Status,
		Status, Context, Module0, Module, Info0, Info) -->
	check_not_exported(Status, Context, "clause"),
	module_add_func_clause(Module0, VarSet, PredName, Args, Result, Body,
		Status, Context, Module, Info0, Info).
add_item_clause(pred_clause(VarSet, PredName, Args, Body), Status, Status,
		Context, Module0, Module, Info0, Info) -->
	check_not_exported(Status, Context, "clause"),
	module_add_pred_clause(Module0, VarSet, PredName, Args, Body, Status,
		Context, Module, Info0, Info).
add_item_clause(type_defn(_, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(inst_defn(_, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(mode_defn(_, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(pred(_, _, _, _, _, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(func(_, _, _, _, _, _, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(pred_mode(_, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(func_mode(_, _, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(module_defn(_, Defn), Status0, Status, _,
		Module, Module, Info0, Info) --> 
	{ module_defn_update_import_status(Defn, ItemStatus1) ->
		ItemStatus1 = item_status(Status1, NeedQual),
		qual_info_get_mq_info(Info0, MQInfo0),
		mq_info_set_need_qual_flag(MQInfo0, NeedQual, MQInfo),
		qual_info_set_mq_info(Info0, MQInfo, Info),
		Status = Status1
	;
		Info = Info0,
		Status = Status0
	}.
add_item_clause(pragma(Pragma), Status, Status, Context,
		Module0, Module, Info0, Info) -->
	(
		{ Pragma = c_code(Attributes, Pred, PredOrFunc, Vars, 
			VarSet, PragmaImpl) }
	->
		module_add_pragma_c_code(Attributes, Pred, PredOrFunc,
			Vars, VarSet, PragmaImpl, Status, Context,
			Module0, Module, Info0, Info)
	;
		{ Pragma = import(Name, PredOrFunc, Modes, Attributes,
			C_Function) }
	->
		module_add_pragma_import(Name, PredOrFunc, Modes,
			Attributes, C_Function, Status, Context,
			Module0, Module, Info0, Info)
	;
		{ Pragma = fact_table(Pred, Arity, File) }
	->
		module_add_pragma_fact_table(Pred, Arity, File, 
			Status, Context, Module0, Module, Info0, Info)
	;
		{ Pragma = tabled(Type, Name, Arity, PredOrFunc, Mode) }
	->
		globals__io_lookup_bool_option(type_layout, TypeLayout),
		(
			{ TypeLayout = yes }
		->
			module_add_pragma_tabled(Type, Name, Arity, PredOrFunc,
				Mode, Context, Module0, Module)
		;
			{ module_info_incr_errors(Module0, Module) },
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma "),
			{ eval_method_to_string(Type, EvalMethodS) },
			io__write_string(EvalMethodS),
			io__write_string(
"' declaration requires the base_type_layout\n"),
			prog_out__write_context(Context),
			io__write_string(
"    structures. Use the --type-layout flag to enable them.\n")
		),
		{ Info = Info0 }
	;
		% don't worry about any pragma decs but c_code, tabling
		% and fact_table here
		{ Module = Module0 },
		{ Info = Info0 }	
	).
add_item_clause(nothing, Status, Status, _, Module, Module, Info, Info) --> [].
add_item_clause(typeclass(_, _, _, _, _),
	Status, Status, _, Module, Module, Info, Info) --> [].
add_item_clause(instance(_, _, _, _, _),
	Status, Status, _, Module, Module, Info, Info) --> [].

%-----------------------------------------------------------------------------%

:- pred check_not_exported(import_status, prog_context, string,
			io__state, io__state).
:- mode check_not_exported(in, in, in, di, uo) is det.

check_not_exported(Status, Context, Message) -->
		%
		% check that clauses are not exported
		%
	( { Status = exported } ->
		prog_out__write_context(Context),
		{ string__append_list(
			["Warning: ", Message, " in module interface.\n"],
			WarningMessage) },
		report_warning(WarningMessage)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_export(sym_name, pred_or_func, argument_modes, string,
	prog_context, module_info, module_info, io__state, io__state).
:- mode add_pragma_export(in, in, in, in, in, in, out, di, uo) is det.

add_pragma_export(Name, PredOrFunc, Modes, C_Function, Context,
			Module0, Module) -->
	{ module_info_get_predicate_table(Module0, PredTable) },
	{ Modes = argument_modes(_ArgInstTable, ArgModes) },
	{ list__length(ArgModes, Arity) },
	(
		{ predicate_table_search_pf_sym_arity(PredTable,
			PredOrFunc, Name, Arity, [PredId]) }
	->
		{ predicate_table_get_preds(PredTable, Preds) },
		{ map__lookup(Preds, PredId, PredInfo) },
		{ pred_info_procedures(PredInfo, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		(
			% YYY What if ArgInstTable is non-empty?
			{ get_procedure_matching_declmodes(
				ExistingProcs, ArgModes, Module0, ProcId)}
		->
			{ module_info_get_pragma_exported_procs(Module0,
				PragmaExportedProcs0) },
			{ NewExportedProc = pragma_exported_proc(PredId,
				ProcId, C_Function) },
			{ PragmaExportedProcs = 
				[NewExportedProc|PragmaExportedProcs0]},
			{ module_info_set_pragma_exported_procs(Module0,
				PragmaExportedProcs, Module) }
		;
			undefined_mode_error(Name, Arity, Context,
				"`:- pragma export' declaration"),
			{ module_info_incr_errors(Module0, Module) }
		)
	;
		undefined_pred_or_func_error(Name, Arity, Context,
			"`:- pragma export' declaration"),
		{ module_info_incr_errors(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_unused_args(pred_or_func, sym_name, arity, proc_id,
		list(int), prog_context, module_info, module_info,
		io__state, io__state).
:- mode add_pragma_unused_args(in, in, in, in, in, in, in, out, di, uo) is det.

add_pragma_unused_args(PredOrFunc, SymName, Arity, ProcId, UnusedArgs, Context,
		Module0, Module) -->
	{ module_info_get_predicate_table(Module0, Preds) },
	(
		{ predicate_table_search_pf_sym_arity(Preds,
			PredOrFunc, SymName, Arity, [PredId]) }
	->
		{ module_info_unused_arg_info(Module0, UnusedArgInfo0) },
		{ map__set(UnusedArgInfo0, proc(PredId, ProcId), UnusedArgs,
			UnusedArgInfo) },
		{ module_info_set_unused_arg_info(Module0, UnusedArgInfo,
			Module) }
	;
		prog_out__write_context(Context),
		io__write_string(
"Internal compiler error: unknown predicate in `pragma unused_args'.\n"),
		{ module_info_incr_errors(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_termination_info(pred_or_func, sym_name, argument_modes,
		maybe(arg_size_info), maybe(termination_info),
		prog_context, module_info, module_info, io__state, io__state).
:- mode add_pragma_termination_info(in, in, in, in, in, in, in, out, di, uo)
		is det.

add_pragma_termination_info(PredOrFunc, SymName, Modes, MaybeArgSizeInfo,
		MaybeTerminationInfo, Context, Module0, Module) -->
	{ module_info_get_predicate_table(Module0, Preds) },
	{ Modes = argument_modes(_ArgInstTable, ArgModes) },
	{ list__length(ArgModes, Arity) },
	(
	    { predicate_table_search_pf_sym_arity(Preds,
		PredOrFunc, SymName, Arity, PredIds) },
	    { PredIds \= [] }
	->
	    ( { PredIds = [PredId] } ->
		{ module_info_preds(Module0, PredTable0) },
		{ map__lookup(PredTable0, PredId, PredInfo0) },
		{ pred_info_procedures(PredInfo0, ProcTable0)},
		{ map__to_assoc_list(ProcTable0, ProcList) },
		( 
			{ get_procedure_matching_declmodes(ProcList, 
				ArgModes, Module0, ProcId) }
		->
			{ map__lookup(ProcTable0, ProcId, ProcInfo0) },
			{ proc_info_set_maybe_arg_size_info(ProcInfo0, 
				MaybeArgSizeInfo, ProcInfo1) },
			{ proc_info_set_maybe_termination_info(ProcInfo1, 
				MaybeTerminationInfo, ProcInfo) },
			{ map__det_update(ProcTable0, ProcId, ProcInfo,
				ProcTable) },
			{ pred_info_set_procedures(PredInfo0, ProcTable,
				PredInfo) },
			{ map__det_update(PredTable0, PredId, PredInfo,
				PredTable) },
			{ module_info_set_preds(Module0, PredTable,
				Module) }
		;
			{ module_info_incr_errors(Module0, Module) }, 
			prog_out__write_context(Context),
			io__write_string(
				"Error: `:- pragma termination_info' "),
			io__write_string(
				"declaration for undeclared mode of "),
			hlds_out__write_call_id(PredOrFunc, 
				SymName/Arity),
			io__write_string(".\n")
		)
	    ;
		prog_out__write_context(Context),
		io__write_string("Error: ambiguous predicate name"),
		hlds_out__write_call_id(PredOrFunc, SymName/Arity),
		io__nl,
		prog_out__write_context(Context),
		io__write_string(
			"  in `pragma termination_info'.\n"),
		{ module_info_incr_errors(Module0, Module) }
	    )
	;
	    % XXX This happens in `.trans_opt' files sometimes --
	    % so just ignore it
	    { Module = Module0 }
	    /***
	    ****   undefined_pred_or_func_error(
	    ****	    SymName, Arity, Context,
	    **** 	    "`:- pragma termination_info' declaration"),
	    ****   { module_info_incr_errors(Module0, Module) }
	    ***/
	).

%-----------------------------------------------------------------------------%

	% add_pred_marker(ModuleInfo0, PragmaName, Name, Arity, Context, 
	% 	Marker, ConflictMarkers, ModuleInfo, IO0, IO)
	% Adds Marker to the marker list of the pred(s) with give Name and
	% Arity, updating the ModuleInfo. If the named pred does not exist,
	% or the pred already has a marker in ConflictMarkers, report
	% an error.
:- pred add_pred_marker(module_info, string, sym_name, arity,
	prog_context, marker, list(marker), module_info,
	io__state, io__state).
:- mode add_pred_marker(in, in, in, in, in, in, in, out, di, uo) is det.

add_pred_marker(Module0, PragmaName, Name, Arity, Context, Marker,
		ConflictMarkers, Module) --> 
	{ module_info_get_predicate_table(Module0, PredTable0) },
	% check that the pragma is module qualified.
	(
		{ Name = unqualified(_) }
	->
		{ error("add_pred_marker: unqualified name") }
	; % else if
		{ predicate_table_search_sym_arity(PredTable0, Name, 
			Arity, PredIds) }
	->
		{ predicate_table_get_preds(PredTable0, Preds0) },
		{ pragma_add_marker(Preds0, PredIds, Marker, Preds) },
		{ predicate_table_set_preds(PredTable0, Preds, 
			PredTable) },
		{ module_info_set_predicate_table(Module0, PredTable, 
			Module1) },
		{ pragma_check_markers(Preds, PredIds, ConflictMarkers, 
			Conflict) },
		(
			{ Conflict = yes }
		->
			pragma_conflict_error(Name, Arity, Context,
				PragmaName),
			{ module_info_incr_errors(Module1, Module) }
		;
			{ Module = Module1 }
		)
	;
		{ string__append_list(["`", PragmaName, "' pragma"],
				      Description) },
		undefined_pred_or_func_error(Name, Arity, Context,
			Description),
		{ module_info_incr_errors(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

:- pred module_mark_as_external(sym_name, int, prog_context,
			module_info, module_info, io__state, io__state).
:- mode module_mark_as_external(in, in, in, in, out, di, uo) is det.

module_mark_as_external(PredName, Arity, Context, Module0, Module) -->
	% `external' declarations can only apply to things defined
	% in this module, since everything else is already external.
	{ module_info_get_predicate_table(Module0, PredicateTable0) },
	(
		{ predicate_table_search_sym_arity(PredicateTable0,
			PredName, Arity, PredIdList) }
	->
		{ module_mark_preds_as_external(PredIdList, Module0, Module) }
	;
		undefined_pred_or_func_error(PredName, Arity,
			Context, "`external' declaration"),
		{ module_info_incr_errors(Module0, Module) }
	).

:- pred module_mark_preds_as_external(list(pred_id), module_info, module_info).
:- mode module_mark_preds_as_external(in, in, out) is det.

module_mark_preds_as_external([], Module, Module).
module_mark_preds_as_external([PredId | PredIds], Module0, Module) :-
	module_info_preds(Module0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_mark_as_external(PredInfo0, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Module0, Preds, Module1),
	module_mark_preds_as_external(PredIds, Module1, Module).

%-----------------------------------------------------------------------------%

:- pred module_add_inst_defn(module_info, inst_varset, inst_defn, condition,
		prog_context, item_status, 
		module_info, io__state, io__state).
:- mode module_add_inst_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_inst_defn(Module0, VarSet, InstDefn, Cond,
		Context, item_status(Status, _NeedQual), Module) -->
	{ module_info_user_insts(Module0, Insts0) },
	insts_add(Insts0, VarSet, InstDefn, Cond, Context, Status, Insts),
	{ module_info_set_user_insts(Module0, Insts, Module) }.

:- pred insts_add(user_inst_table, inst_varset, inst_defn, condition,
		prog_context, import_status, user_inst_table,
		io__state, io__state).
:- mode insts_add(in, in, in, in, in, in, out, di, uo) is det.

	% XXX handle abstract insts
insts_add(_, _, abstract_inst(_, _), _, _, _, _) -->
	{ error("sorry, abstract insts not implemented") }.
insts_add(Insts0, VarSet, eqv_inst(Name, Args, Body),
			Cond, Context, Status, Insts) -->
	{ list__length(Args, Arity) },
	(
		% XXX For now, Body has no aliasing declarations.
		{ I = hlds_inst_defn(VarSet, Args, eqv_inst(Body), Cond,
					Context, Status) },
		{ user_inst_table_insert(Insts0, Name - Arity, I, Insts1) }
	->
		{ Insts = Insts1 }
	;
		{ Insts = Insts0 },
		% If abstract insts are implemented, this will need to change
		% to update the hlds_inst_defn to the non-abstract inst.
		( { Status = opt_imported } ->
			[]
		;
			% XXX we should record each error using 
			%	 module_info_incr_errors
			{ user_inst_table_get_inst_defns(Insts, InstDefns) },
			{ map__lookup(InstDefns, Name - Arity, OrigI) },
			{ OrigI = hlds_inst_defn(_, _, _, _,
						OrigContext, _) },
			multiple_def_error(Name, Arity, "inst",
					Context, OrigContext)
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_mode_defn(module_info, inst_varset, mode_defn, condition,
		prog_context, item_status, module_info, io__state, io__state).
:- mode module_add_mode_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_mode_defn(Module0, VarSet, ModeDefn, Cond,
		Context, item_status(Status, _NeedQual), Module) -->
	{ module_info_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, ModeDefn, Cond, Context, Status, Modes),
	{ module_info_set_modes(Module0, Modes, Module) }.

:- pred modes_add(mode_table, inst_varset, mode_defn, condition, prog_context,
			import_status, mode_table, io__state, io__state).
:- mode modes_add(in, in, in, in, in, in, out, di, uo) is det.

modes_add(Modes0, VarSet, eqv_mode(Name, Args, Body),
			Cond, Context, Status, Modes) -->
	{ list__length(Args, Arity) },
	(
		{ I = hlds_mode_defn(VarSet, Args, eqv_mode(Body), Cond,
			Context, Status) },
		{ mode_table_insert(Modes0, Name - Arity, I, Modes1) }
	->
		{ Modes = Modes1 }
	;
		{ Modes = Modes0 },
		( { Status = opt_imported } ->
			[]
		;
			{ mode_table_get_mode_defns(Modes, ModeDefns) },
			{ map__lookup(ModeDefns, Name - Arity, OrigI) },
			{ OrigI = hlds_mode_defn(_, _, _, _,
						OrigContext, _) },
			% XXX we should record each error using
			% 	module_info_incr_errors
			multiple_def_error(Name, Arity, "mode",
					Context, OrigContext)
		)
	).

:- pred mode_name_args(mode_defn, sym_name, list(inst_param), hlds_mode_body).
:- mode mode_name_args(in, out, out, out) is det.

mode_name_args(eqv_mode(Name, Args, Body), Name, Args, eqv_mode(Body)).

%-----------------------------------------------------------------------------%

	% We allow more than one "definition" for a given type so
	% long all of them except one are actually just declarations,
	% e.g. `:- type t.', which is parsed as an type definition for
	% t which defines t as an abstract_type.

:- pred module_add_type_defn(module_info, tvarset, type_defn, condition,
		prog_context, item_status, module_info, io__state, io__state).
:- mode module_add_type_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_type_defn(Module0, TVarSet, TypeDefn, _Cond, Context,
		item_status(Status0, NeedQual), Module) -->
	{ module_info_types(Module0, Types0) },
	globals__io_get_globals(Globals),
	{ convert_type_defn(TypeDefn, Globals, Name, Args, Body) },
	{ list__length(Args, Arity) },
	{ Body = abstract_type ->
		( Status0 = exported ->
			Status1 = abstract_exported
		; Status0 = imported ->
			Status1 = abstract_imported
		;
			Status1 = Status0
		)
	;
		Status1 = Status0
	},
	{ 
		% the type is exported if *any* occurrence is exported,
		% even a previous abstract occurrence
		map__search(Types0, Name - Arity, OldDefn)
	->
		hlds_data__get_type_defn_status(OldDefn, OldStatus),
		combine_status(Status1, OldStatus, Status)
	;
		Status = Status1 
	},
	{ hlds_data__set_type_defn(TVarSet, Args, Body, Status, Context, T) },
	{ TypeId = Name - Arity },
	(
		% if there was an existing non-abstract definition for the type
		{ map__search(Types0, TypeId, T2) },
		{ hlds_data__get_type_defn_tparams(T2, Params) },
		{ hlds_data__get_type_defn_body(T2, Body_2) },
		{ hlds_data__get_type_defn_context(T2, OrigContext) },
		{ hlds_data__get_type_defn_status(T2, OrigStatus) },
		{ Body_2 \= abstract_type }
	->
	  	(
			% then if this definition was abstract, ignore it
			% (but update the status of the old defn if necessary)
			{ Body = abstract_type }
		->
			{
				Status = OrigStatus
			->
				Module = Module0
			;
				hlds_data__set_type_defn(TVarSet, Params,
					Body_2, OrigStatus, OrigContext, T3),
				map__det_update(Types0, TypeId, T3, Types),
				module_info_set_types(Module0, Types, Module)
			}
		;

			% otherwise issue an error message if the second
			% definition wasn't read while reading .opt files. 
			{ Status = opt_imported }
		->
			{ Module = Module0 }
		;
			{ module_info_incr_errors(Module0, Module) },
			multiple_def_error(Name, Arity, "type", Context,
				OrigContext)
		)
	;
		{ map__set(Types0, TypeId, T, Types) },
		(
			{ Body = du_type(ConsList, _, _, _) }
		->
			{ module_info_ctors(Module0, Ctors0) },
			ctors_add(ConsList, TypeId, NeedQual, 
				Context, Ctors0, Ctors),
			{ module_info_set_ctors(Module0, Ctors, Module1) }
		;
			{ Module1 = Module0 }
		),
		{ construct_qualified_term(Name, Args, Type) },
		(
			( 
				{ Body = abstract_type }
			;
				{ type_id_is_hand_defined(TypeId) }
			)
		->
			special_pred_list(SpecialPredIds),
			{ add_special_pred_decl_list(SpecialPredIds,
					Module1, TVarSet, Type, TypeId,
					Context, Status, Module2) }
		;
			special_pred_list(SpecialPredIds),
			{ add_special_pred_list(SpecialPredIds,
					Module1, TVarSet, Type, TypeId,
					Body, Context, Status, Module2) }
		),
		{ module_info_set_types(Module2, Types, Module) },
		( { Body = uu_type(_) } ->
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string(
	"Sorry, not implemented: undiscriminated union type.\n"),
			prog_out__write_context(Context),
			io__write_string(
	"(The syntax for type equivalence is `:- type t1 == t2'.)\n"),
			io__set_exit_status(1),
			io__set_output_stream(OldStream, _)
		;
			% XXX we can't handle abstract exported
			% polymorphic equivalence types with monomorphic
			% bodies, because the compiler stuffs up the
			% type_info handling -- the caller passes type_infos,
			% but the callee expects no type_infos
			{ Body = eqv_type(EqvType) },
			{ Status = abstract_exported },
			{ term__contains_var_list(Args, Var) },
			{ \+ term__contains_var(EqvType, Var) }
		->
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string(
	"Sorry, not implemented: polymorphic equivalence type,\n"),
			prog_out__write_context(Context),
			io__write_string(
	"  with monomorphic definition, exported as abstract type.\n"),
			globals__io_lookup_bool_option(verbose_errors,
				VerboseErrors),
			( { VerboseErrors = yes } ->
				io__write_strings([
	"\tA quick work-around is just export the type as a concrete type,\n",
	"\tby putting the type definition in the interface section.\n",
	"\tA better work-around is to use a ""wrapper"" type, with just one\n",
	"\tfunctor that has just one arg, instead of an equivalence type.\n",
	"\t(There's no performance penalty for this -- the compiler will\n",
	"\toptimize the wrapper away.)\n"])
			;
				[]
			),
			io__set_exit_status(1),
			io__set_output_stream(OldStream, _)
		;
			[]
		)
	).

:- pred combine_status(import_status, import_status, import_status).
:- mode combine_status(in, in, out) is det.

combine_status(StatusA, StatusB, Status) :-
	( combine_status_2(StatusA, StatusB, CombinedStatus) ->
		Status = CombinedStatus
	;
		error("pseudo_imported or pseudo_exported type definition")
	).

:- pred combine_status_2(import_status, import_status, import_status).
:- mode combine_status_2(in, in, out) is semidet.

combine_status_2(imported, Status2, Status) :-
	 combine_status_imported(Status2, Status).
combine_status_2(local, Status2, Status) :-
	combine_status_local(Status2, Status).
combine_status_2(exported, _Status2, exported).
combine_status_2(exported_to_submodules, _Status2, exported_to_submodules).
combine_status_2(opt_imported, _Status2, opt_imported).
combine_status_2(abstract_imported, Status2, Status) :-
	combine_status_abstract_imported(Status2, Status).
combine_status_2(abstract_exported, Status2, Status) :-
	combine_status_abstract_exported(Status2, Status).

:- pred combine_status_imported(import_status, import_status).
:- mode combine_status_imported(in, out) is semidet.

combine_status_imported(imported,	imported).
combine_status_imported(local,		imported).
combine_status_imported(exported,	exported).
combine_status_imported(opt_imported,	opt_imported).
combine_status_imported(abstract_imported, imported).
combine_status_imported(abstract_exported, abstract_exported).

:- pred combine_status_local(import_status, import_status).
:- mode combine_status_local(in, out) is semidet.

combine_status_local(imported,		local).
combine_status_local(local,		local).
combine_status_local(exported,		exported).
combine_status_local(opt_imported,	local).
combine_status_local(abstract_imported, local).
combine_status_local(abstract_exported, abstract_exported).

:- pred combine_status_abstract_exported(import_status, import_status).
:- mode combine_status_abstract_exported(in, out) is det.

combine_status_abstract_exported(Status2, Status) :-
	( Status2 = exported ->
		Status = exported
	;
		Status = abstract_exported
	).

:- pred combine_status_abstract_imported(import_status, import_status).
:- mode combine_status_abstract_imported(in, out) is det.

combine_status_abstract_imported(Status2, Status) :-
	( Status2 = imported ->
		Status = imported
	;
		Status = abstract_imported
	).

:- pred add_special_preds(module_info, tvarset, type, type_id, 
		hlds_type_body, prog_context, import_status, module_info,
		io__state, io__state).
:- mode add_special_preds(in, in, in, in, in, in, in, out, di, uo) is det.

add_special_preds(Module0, TVarSet, Type, TypeId,
			Body, Context, Status, Module, IO0, IO) :-
	special_pred_list(SpecialPredIds, IO0, IO),
	( Body = abstract_type ->
		add_special_pred_decl_list(SpecialPredIds, Module0, TVarSet,
				Type, TypeId, Context, Status, Module)
	;
		add_special_pred_list(SpecialPredIds, Module0, TVarSet, Type,
				TypeId, Body, Context, Status, Module)
	).
	
:- pred convert_type_defn(type_defn, globals,
			sym_name, list(type_param), hlds_type_body).
:- mode convert_type_defn(in, in, out, out, out) is det.

convert_type_defn(du_type(Name, Args, Body, EqualityPred),
		Globals, Name, Args,
		du_type(Body, CtorTags, IsEnum, EqualityPred)) :-
	assign_constructor_tags(Body, Globals, CtorTags, IsEnum).
convert_type_defn(uu_type(Name, Args, Body), _, Name, Args, uu_type(Body)).
convert_type_defn(eqv_type(Name, Args, Body), _, Name, Args, eqv_type(Body)).
convert_type_defn(abstract_type(Name, Args), _, Name, Args, abstract_type).

:- pred ctors_add(list(constructor), type_id, need_qualifier, prog_context, 
			cons_table, cons_table, io__state, io__state).
:- mode ctors_add(in, in, in, in, in, out, di, uo) is det.

ctors_add([], _TypeId, _NeedQual, _Context, Ctors, Ctors) --> [].
ctors_add([Ctor | Rest], TypeId, NeedQual, Context, Ctors0, Ctors) -->
	{ Ctor = ctor(ExistQVars, Constraints, Name, Args) },
	{ make_cons_id(Name, Args, TypeId, QualifiedConsId) },
	{ assoc_list__values(Args, Types) },
	{ ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Types, TypeId,
				Context) },
	(
		{ map__search(Ctors0, QualifiedConsId, QualifiedConsDefns0) }
	->
		{ QualifiedConsDefns1 = QualifiedConsDefns0 }
	;
		{ QualifiedConsDefns1 = [] }
	),
	(
		{ list__member(OtherConsDefn, QualifiedConsDefns1) },
		{ OtherConsDefn = hlds_cons_defn(_, _, _, TypeId, _) }
	->
		% XXX we should record each error using module_info_incr_errors
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("Error: constructor `"),
		hlds_out__write_cons_id(QualifiedConsId),
		io__write_string("' for type `"),
		hlds_out__write_type_id(TypeId),
		io__write_string("' multiply defined.\n"),
		io__set_exit_status(1),
		io__set_output_stream(OldStream, _),
		{ QualifiedConsDefns = QualifiedConsDefns1 }
	;
		{ QualifiedConsDefns = [ConsDefn | QualifiedConsDefns1] }	
	),
	{ map__set(Ctors0, QualifiedConsId, QualifiedConsDefns, Ctors1) },

	% XXX the code below does the wrong thing if you mix
	% `import_module' and `use_module' declarations for
	% parent and child modules.
	% It assumes that all parents of an imported module were imported,
	% and that all parents of a used module were used.

	{
		QualifiedConsId = cons(qualified(Module, ConsName), Arity),
		NeedQual = may_be_unqualified
	->
		% Add unqualified version of the cons_id to the cons_table.
		UnqualifiedConsId = cons(unqualified(ConsName), Arity),
		multi_map__set(Ctors1, UnqualifiedConsId, ConsDefn, Ctors2),

		% Add partially qualified versions of the cons_id
		get_partial_qualifiers(Module, PartialQuals),
		list__map_foldl(add_ctor(ConsName, Arity, ConsDefn),
			PartialQuals, _PartiallyQualifiedConsIds,
			Ctors2, Ctors3)
	;
		Ctors3 = Ctors1
	},
	ctors_add(Rest, TypeId, NeedQual, Context, Ctors3, Ctors).

:- pred add_ctor(string::in, int::in, hlds_cons_defn::in, module_name::in,
		cons_id::out, cons_table::in, cons_table::out) is det.

add_ctor(ConsName, Arity, ConsDefn, ModuleQual, ConsId, CtorsIn, CtorsOut) :-
	ConsId = cons(qualified(ModuleQual, ConsName), Arity),
	multi_map__set(CtorsIn, ConsId, ConsDefn, CtorsOut).

%-----------------------------------------------------------------------------%

:- pred module_add_pred(module_info, tvarset, inst_varset, existq_tvars,
		sym_name, types_and_modes, maybe(determinism), condition,
		purity, class_constraints, pred_markers, prog_context,
		item_status, maybe(pair(pred_id, proc_id)), module_info, 
		io__state, io__state).
:- mode module_add_pred(in, in, in, in, in, in, in, in, in, in, in, in, in,
		out, out, di, uo) is det.

module_add_pred(Module0, TypeVarSet, InstVarSet, ExistQVars, PredName,
		TypesAndModes, MaybeDet, Cond, Purity, ClassContext, Markers,
		Context, item_status(Status, NeedQual), MaybePredProcId,
		Module) -->
	% Only preds with opt_imported clauses are tagged as opt_imported, so
	% that the compiler doesn't look for clauses for other preds read in
	% from optimization interfaces.
	{ Status = opt_imported ->
		DeclStatus = imported
	;
		DeclStatus = Status
	},
	{ TypesAndModes = types_and_modes(ArgInstTable, TMs) },
	{ split_types_and_modes(TMs, Types, MaybeModes) },
	add_new_pred(Module0, TypeVarSet, ExistQVars, PredName, Types, Cond,
		Purity, ClassContext, Markers, Context, DeclStatus, NeedQual, 
		predicate, Module1),
	(
		{ MaybeModes = yes(Modes) }
	->
		module_add_mode(Module1, InstVarSet, PredName,
			argument_modes(ArgInstTable, Modes), MaybeDet,
			Cond, Context, predicate, PredProcId, Module),
		{ MaybePredProcId = yes(PredProcId) }
	;
		{ Module = Module1 },
		{ MaybePredProcId = no }
	).

:- pred module_add_func(module_info, tvarset, inst_varset, existq_tvars,
		sym_name, types_and_modes, type_and_mode, maybe(determinism),
		condition, purity, class_constraints, pred_markers,
		prog_context, item_status, maybe(pair(pred_id, proc_id)),
		module_info, io__state, io__state).
:- mode module_add_func(in, in, in, in, in, in, in, in, in, in, in, in, in, in,
		out, out, di, uo) is det.

module_add_func(Module0, TypeVarSet, InstVarSet, ExistQVars, FuncName,
		TypesAndModes, RetTypeAndMode, MaybeDet, Cond, Purity,
		ClassContext, Markers, Context,
		item_status(Status, NeedQual), MaybePredProcId, Module) -->
	% Only funcs with opt_imported clauses are tagged as opt_imported, so
	% that the compiler doesn't look for clauses for other preds.
	{ Status = opt_imported ->
		DeclStatus = imported
	;
		DeclStatus = Status
	},
	{ TypesAndModes = types_and_modes(ArgInstTable, TMs) },
	{ split_types_and_modes(TMs, Types, MaybeModes) },
	{ split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode) },
	{ list__append(Types, [RetType], Types1) },
	add_new_pred(Module0, TypeVarSet, ExistQVars, FuncName, Types1, Cond,
		Purity, ClassContext, Markers, Context, DeclStatus, NeedQual,
		function, Module1),
	(
		{ MaybeModes = yes(Modes) },
		{ MaybeRetMode = yes(RetMode) }
	->
		{ list__append(Modes, [RetMode], Modes1) },
		module_add_mode(Module1, InstVarSet, FuncName,
			argument_modes(ArgInstTable, Modes1), MaybeDet, Cond,
			Context, function, PredProcId, Module),
		{ MaybePredProcId = yes(PredProcId) }
	;
		{ Module = Module1 },
		{ MaybePredProcId = no}
	).

:- pred module_add_class_defn(module_info, list(class_constraint), sym_name,
	list(tvar), class_interface, tvarset, prog_context, 
	item_status, module_info, io__state, io__state).
:- mode module_add_class_defn(in, in, in, in, in, in, in, in, out, 
	di, uo) is det.

module_add_class_defn(Module0, Constraints, Name, Vars, Interface, VarSet,
		Context, Status, Module) -->
	{ module_info_classes(Module0, Classes0) },
	{ module_info_superclasses(Module0, SuperClasses0) },
	{ list__length(Vars, ClassArity) },
	{ ClassId = class_id(Name, ClassArity) },
	(
		{ map__search(Classes0, ClassId, OldValue) }
	->
		{ OldValue = hlds_class_defn(_, _, _, _, OldContext) },
		multiple_def_error(Name, ClassArity, "typeclass", 
			Context, OldContext),
		io__set_exit_status(1),
		{ Module = Module0 }
	;
		module_add_class_interface(Module0, Name, Vars, Interface,
			Status, PredProcIds0, Module1),
			% Get rid of the `no's from the list of maybes
		{ IsYes = lambda([Maybe::in, PredProcId::out] is semidet,
			(
				Maybe = yes(Pred - Proc),
				PredProcId = hlds_class_proc(Pred, Proc)
			)) },
		{ list__filter_map(IsYes, PredProcIds0, PredProcIds) },
		{ Value = hlds_class_defn(Constraints, Vars, PredProcIds, 
			VarSet, Context) },
		{ map__det_insert(Classes0, ClassId, Value, Classes) },
		{ module_info_set_classes(Module1, Classes, Module2) },

			% insert an entry into the super class table for each
			% super class of this class
		{ AddSuper = lambda([Super::in, Ss0::in, Ss::out] is det,
			(
				Super = constraint(SuperName, SuperTypes),
				list__length(SuperTypes, SuperClassArity),
				term__vars_list(SuperTypes, SuperVars),
				SuperClassId = class_id(SuperName,
					SuperClassArity),
				SubClassDetails = subclass_details(SuperVars,
					ClassId, Vars, VarSet),
				multi_map__set(Ss0, SuperClassId,
					SubClassDetails, Ss)
			)) },
		{ list__foldl(AddSuper, Constraints, 
			SuperClasses0, SuperClasses) },

		{ module_info_set_superclasses(Module2, 
			SuperClasses, Module3) },

			% When we find the class declaration, make an
			% entry for the instances.
		{ module_info_instances(Module3, Instances0) },
		{ map__det_insert(Instances0, ClassId, [], Instances) },
		{ module_info_set_instances(Module3, Instances, Module) }
	).

:- pred module_add_class_interface(module_info, sym_name, list(tvar),
	class_interface, item_status, list(maybe(pair(pred_id, proc_id))), 
	module_info, io__state, io__state).
:- mode module_add_class_interface(in, in, in, in, in, out, out, di, uo) is det.

module_add_class_interface(Module0, Name, Vars, Methods, Status, PredProcIds, 
		Module) -->
	module_add_class_interface_2(Module0, Name, Vars, Methods, Status,
		PredProcIds0, Module1),
	{ add_default_class_method_func_modes(Methods, PredProcIds0,
		PredProcIds, Module1, Module) }.

:- pred module_add_class_interface_2(module_info, sym_name, list(tvar),
	class_interface, item_status, list(maybe(pair(pred_id, proc_id))), 
	module_info, io__state, io__state).
:- mode module_add_class_interface_2(in, in, in, in, in, out, out, 
	di, uo) is det.

module_add_class_interface_2(Module, _, _, [], _, [], Module) --> [].
module_add_class_interface_2(Module0, Name, Vars, [M|Ms], Status, [P|Ps], 
		Module) -->
	module_add_class_method(M, Name, Vars, Status, P, Module0, Module1),
	module_add_class_interface_2(Module1, Name, Vars, Ms, Status, Ps,
		Module).

:- pred module_add_class_method(class_method, sym_name, list(tvar), 
	item_status, maybe(pair(pred_id, proc_id)), module_info, module_info,
	io__state, io__state).
:- mode module_add_class_method(in, in, in, in, out, in, out, di, uo) is det.
	
module_add_class_method(Method, Name, Vars, Status, MaybePredIdProcId, 
		Module0, Module) -->
	(
		{ Method = pred(TypeVarSet, InstVarSet, ExistQVars, PredName,
			TypesAndModes, MaybeDet, Cond, ClassContext, Context) },
		{ term__var_list_to_term_list(Vars, VarTerms) },
		{ ClassContext = constraints(UnivCnstrs, ExistCnstrs) },
		{ NewUnivCnstrs = [constraint(Name, VarTerms) | UnivCnstrs] },
		{ NewClassContext = constraints(NewUnivCnstrs, ExistCnstrs) },
		{ init_markers(Markers0) },
		{ add_marker(Markers0, class_method, Markers) },
		module_add_pred(Module0, TypeVarSet, InstVarSet, ExistQVars,
			PredName, TypesAndModes, MaybeDet, Cond, pure,
			NewClassContext, Markers, Context, Status,
			MaybePredIdProcId, Module)
	;
		{ Method = func(TypeVarSet, InstVarSet, ExistQVars, FuncName,
			TypesAndModes, RetTypeAndMode, MaybeDet, Cond,
			ClassContext, Context) },
		{ term__var_list_to_term_list(Vars, VarTerms) },
		{ ClassContext = constraints(UnivCnstrs, ExistCnstrs) },
		{ NewUnivCnstrs = [constraint(Name, VarTerms) | UnivCnstrs] },
		{ NewClassContext = constraints(NewUnivCnstrs, ExistCnstrs) },
		{ init_markers(Markers0) },
		{ add_marker(Markers0, class_method, Markers) },
		module_add_func(Module0, TypeVarSet, InstVarSet, ExistQVars,
			FuncName, TypesAndModes, RetTypeAndMode, MaybeDet,
			Cond, pure, NewClassContext, Markers, Context, Status,
			MaybePredIdProcId, Module)
	;
		{ Method = pred_mode(VarSet, PredName, Modes, MaybeDet, 
			Cond, Context) },
		module_add_mode(Module0, VarSet, PredName, Modes, MaybeDet, 
			Cond, Context, predicate, PredIdProcId, Module),
		{ MaybePredIdProcId = yes(PredIdProcId) }
	;
		{ Method = func_mode(VarSet, FuncName, Modes, RetMode, MaybeDet,
			Cond, Context) },
		{ Modes = argument_modes(ArgInstTable, ArgModes) },
		{ list__append(ArgModes, [RetMode], ArgModes1) },
		module_add_mode(Module0, VarSet, FuncName,
			argument_modes(ArgInstTable, ArgModes1), MaybeDet,
			Cond, Context, function, PredIdProcId, Module),
		{ MaybePredIdProcId = yes(PredIdProcId) }
	).

	% Go through the list of class methods, looking for functions without
	% mode declarations.
:- pred add_default_class_method_func_modes(class_interface, 
	list(maybe(pair(pred_id, proc_id))), 
	list(maybe(pair(pred_id, proc_id))), module_info, module_info).
:- mode add_default_class_method_func_modes(in, in, out, in, out) is det.

add_default_class_method_func_modes([], PredProcIds, PredProcIds,
		Module, Module).
add_default_class_method_func_modes([M|Ms], PredProcIds0, PredProcIds,
		Module0, Module) :-
	(
		M = func(_, _, _, FuncName, TypesAndModes, _, _, _, _, _)
	->
		( FuncName = qualified(ModuleName0, Func0) ->
			ModuleName = ModuleName0,
			Func = Func0
		;
			% The class interface should be fully module qualified
			% by prog_io.m at the time it is read in.
			error(
		       "add_default_class_method_func_modes: unqualified func")
		),

		TypesAndModes = types_and_modes(_, TMs),
		list__length(TMs, FuncArity),
		module_info_get_predicate_table(Module0, PredTable),
		(
			predicate_table_search_func_m_n_a(PredTable,
				ModuleName, Func, FuncArity, [PredId])
		->
			module_info_pred_info(Module0, PredId, PredInfo0),
			maybe_add_default_mode(Module0, PredInfo0, 
				PredInfo, MaybeProc),
			(
				MaybeProc = no,
				PredProcIds1 = PredProcIds0,
				Module1 = Module0
			;
				MaybeProc = yes(ProcId),
				NewPredProc = yes(PredId - ProcId),
				PredProcIds1 = [NewPredProc | PredProcIds0],
				module_info_set_pred_info(Module0, PredId,
					PredInfo, Module1)
			)
		;
			error("add_default_class_method_func_modes")
		)
	;
		PredProcIds1 = PredProcIds0,
		Module1 = Module0
	),
	add_default_class_method_func_modes(Ms, PredProcIds1, PredProcIds,
		Module1, Module).

:- pred module_add_instance_defn(module_info, list(class_constraint), sym_name,
	list(type), instance_interface, tvarset, import_status, prog_context, 
	module_info, io__state, io__state).
:- mode module_add_instance_defn(in, in, in, in, in, in, in, in, out, 
	di, uo) is det.

module_add_instance_defn(Module0, Constraints, Name, Types, Interface, VarSet,
		Status, Context, Module) -->
	{ module_info_classes(Module0, Classes) },
	{ module_info_instances(Module0, Instances0) },
	{ list__length(Types, ClassArity) },
	{ Key = class_id(Name, ClassArity) },
	(
		{ map__search(Classes, Key, _) }
	->
		{ map__init(Empty) },
		{ NewValue = hlds_instance_defn(Status, Context, Constraints, 
			Types, Interface, no, VarSet, Empty) },
		{ map__lookup(Instances0, Key, Values) },
		{ map__det_update(Instances0, Key, [NewValue|Values], 
			Instances) },
		{ module_info_set_instances(Module0, Instances, Module) }
	;
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("Error: typeclass `"),
		prog_out__write_sym_name(Name),
		io__write_char('/'),
		io__write_int(ClassArity),
		io__write_string("' not defined.\n"),
		io__set_exit_status(1),
		io__set_output_stream(OldStream, _),
		{ Module = Module0 }
	).

%-----------------------------------------------------------------------------%

:- pred add_new_pred(module_info, tvarset, existq_tvars, sym_name, list(type),
		condition, purity, class_constraints, pred_markers,
		prog_context, import_status, need_qualifier,
		pred_or_func, module_info, io__state, io__state).
:- mode add_new_pred(in, in, in, in, in, in, in, in, in, in, in, in, in, out,
		di, uo) is det.

% NB.  Predicates are also added in lambda.m, which converts
% lambda expressions into separate predicates, so any changes may need
% to be reflected there too.

add_new_pred(Module0, TVarSet, ExistQVars, PredName, Types, Cond, Purity,
		ClassContext, Markers0, Context, Status, NeedQual,
		PredOrFunc, Module) -->
	{ module_info_name(Module0, ModuleName) },
	{ list__length(Types, Arity) },
	(
		{ PredName = unqualified(_PName) },
		{ module_info_incr_errors(Module0, Module) },
		unqualified_pred_error(PredName, Arity, Context)
		% All predicate names passed into this predicate should have 
		% been qualified by prog_io.m, when they were first read.
	;
		{ PredName = qualified(MNameOfPred, PName) },
		{ Module1 = Module0 },
		{ module_info_get_predicate_table(Module1, PredicateTable0) },
		{ clauses_info_init(Arity, ClausesInfo) },
		{ map__init(Proofs) },
		{ purity_to_markers(Purity, PurityMarkers) },
		{ markers_to_marker_list(PurityMarkers, MarkersList) },
		{ AddMarker = lambda(
			[M::in, TheMarkers0::in, TheMarkers::out] is det,
			(
				add_marker(TheMarkers0, M, TheMarkers)
			)) },
		{ list__foldl(AddMarker, MarkersList, Markers0, Markers) },
		{ pred_info_init(ModuleName, PredName, Arity, TVarSet,
				ExistQVars, Types,
				Cond, Context, ClausesInfo, Status, Markers,
				none, PredOrFunc, ClassContext, Proofs,
				PredInfo0) },
		(
			{ predicate_table_search_pf_m_n_a(PredicateTable0,
				PredOrFunc, MNameOfPred, PName, Arity,
				[OrigPred|_]) }
		->
			( { Status \= opt_imported } ->
				{ module_info_incr_errors(Module1, Module) },
				{ module_info_pred_info(Module, OrigPred,
					OrigPredInfo) },
				{ pred_info_context(OrigPredInfo,
					OrigContext) },
				{ hlds_out__pred_or_func_to_str(PredOrFunc,
					DeclString) },
				multiple_def_error(PredName, Arity, DeclString,
					Context, OrigContext)
			;
				% This can happen for exported external preds.
				{ Module = Module0 }
			)
		;
			{ predicate_table_insert(PredicateTable0, PredInfo0, 
				NeedQual, PredId, PredicateTable1) },
			( 
				{ code_util__predinfo_is_builtin(PredInfo0) }
			->
				{ add_builtin(PredId, Types,
					PredInfo0, PredInfo) },
				{ predicate_table_get_preds(PredicateTable1,
					Preds1) },
				{ map__det_update(Preds1, PredId, PredInfo,
					Preds) },
				{ predicate_table_set_preds(PredicateTable1,
					Preds, PredicateTable) }
			;
				{ PredicateTable = PredicateTable1 }
			),
			{ module_info_set_predicate_table(Module1, 
					PredicateTable, Module) }
		)
	).

%-----------------------------------------------------------------------------%

:- pred add_builtin(pred_id, list(type), pred_info, pred_info).
:- mode add_builtin(in, in, in, out) is det.

	% For a builtin predicate, say foo/2, we add a clause
	%
	%	foo(H1, H2) :- foo(H1, H2).
	%
	% This does not generate an infinite loop!
	% Instead, the compiler will generate the usual builtin inline code
	% for foo/2 in the body.  The reason for generating this
	% forwarding code stub is so that things work correctly if
	% you take the address of the predicate.

add_builtin(PredId, Types, PredInfo0, PredInfo) :-
		%
		% lookup some useful info: Module, Name, Context, HeadVars
		%
	pred_info_module(PredInfo0, Module),
	pred_info_name(PredInfo0, Name),
	pred_info_context(PredInfo0, Context),
	pred_info_clauses_info(PredInfo0, ClausesInfo0),
	ClausesInfo0 = clauses_info(VarSet, _VarTypes0, _VarTypes1,
					HeadVars, _ClauseList0),

		%
		% construct the pseudo-recursive call to Module:Name(HeadVars)
		%
	SymName = qualified(Module, Name),
	invalid_proc_id(ModeId),	% mode checking will figure it out
	MaybeUnifyContext = no,
	Call = call(PredId, ModeId, HeadVars, inline_builtin, MaybeUnifyContext,
			SymName),

		%
		% construct a clause containing that pseudo-recursive call
		%
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo1),
	set__list_to_set(HeadVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo1, NonLocals, GoalInfo),
	Goal = Call - GoalInfo,
	Clause = clause([], Goal, Context),

		%
		% put the clause we just built into the pred_info,
		% annotateed with the appropriate types
		%
	ClauseList = [Clause],
	map__from_corresponding_lists(HeadVars, Types, VarTypes),
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes,
					HeadVars, ClauseList),
	pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo).

%-----------------------------------------------------------------------------%

:- pred add_special_pred_list(list(special_pred_id),
			module_info, tvarset, type, type_id, hlds_type_body,
			prog_context, import_status, module_info).
:- mode add_special_pred_list(in, in, in, in, in, in, in, in, out) is det.

add_special_pred_list([], Module, _, _, _, _, _, _, Module).
add_special_pred_list([SpecialPredId | SpecialPredIds], Module0,
		TVarSet, Type, TypeId, Body, Context, Status, Module) :-
	add_special_pred(SpecialPredId, Module0,
		TVarSet, Type, TypeId, Body, Context, Status, Module1),
	add_special_pred_list(SpecialPredIds, Module1,
		TVarSet, Type, TypeId, Body, Context, Status, Module).

:- pred add_special_pred(special_pred_id,
			module_info, tvarset, type, type_id, hlds_type_body,
			prog_context, import_status, module_info).
:- mode add_special_pred(in, in, in, in, in, in, in, in, out) is det.

add_special_pred(SpecialPredId,
		Module0, TVarSet, Type, TypeId, TypeBody, Context, Status0,
		Module) :-
	adjust_special_pred_status(Status0, SpecialPredId, Status),
	module_info_get_special_pred_map(Module0, SpecialPredMap0),
	( map__contains(SpecialPredMap0, SpecialPredId - TypeId) ->
		Module1 = Module0
	;
		add_special_pred_decl(SpecialPredId,
			Module0, TVarSet, Type, TypeId, Context, Status,
			Module1)
	),
	module_info_get_special_pred_map(Module1, SpecialPredMap1),
	map__lookup(SpecialPredMap1, SpecialPredId - TypeId, PredId),
	module_info_preds(Module1, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% if the type was imported, then the special preds for that
	% type should be imported too
	( (Status = imported ; Status = pseudo_imported) ->
		pred_info_set_import_status(PredInfo0, Status, PredInfo1)
	;
		PredInfo1 = PredInfo0
	),
	unify_proc__generate_clause_info(SpecialPredId, Type, TypeBody,
				Context, Module1, ClausesInfo),
	pred_info_set_clauses_info(PredInfo1, ClausesInfo, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Module1, Preds, Module).

:- pred add_special_pred_decl_list(list(special_pred_id),
			module_info, tvarset, type, type_id, 
			prog_context, import_status, module_info).
:- mode add_special_pred_decl_list(in, in, in, in, in, in, in, out) is det.

add_special_pred_decl_list([], Module, _, _, _, _, _, Module).
add_special_pred_decl_list([SpecialPredId | SpecialPredIds], Module0,
		TVarSet, Type, TypeId, Context, Status, Module) :-
	add_special_pred_decl(SpecialPredId, Module0,
		TVarSet, Type, TypeId, Context, Status, Module1),
	add_special_pred_decl_list(SpecialPredIds, Module1,
		TVarSet, Type, TypeId, Context, Status, Module).

:- pred add_special_pred_decl(special_pred_id,
		module_info, tvarset, type, type_id, prog_context,
		import_status, module_info).
:- mode add_special_pred_decl(in, in, in, in, in, in, in, out) is det.

add_special_pred_decl(SpecialPredId,
			Module0, TVarSet, Type, TypeId, Context, Status0,
			Module) :-
	module_info_name(Module0, ModuleName),
	PredName = unqualified(Name),
	special_pred_info(SpecialPredId, Type, Name, ArgTypes, ArgModes, Det),
	special_pred_name_arity(SpecialPredId, _, _, Arity),
	Cond = true,
	clauses_info_init(Arity, ClausesInfo0),
	adjust_special_pred_status(Status0, SpecialPredId, Status),
	map__init(Proofs),
	init_markers(Markers),
		% XXX If/when we have "comparable" or "unifiable" typeclasses, 
		% XXX this context might not be empty
	ClassContext = constraints([], []),
	ExistQVars = [],
	pred_info_init(ModuleName, PredName, Arity, TVarSet, ExistQVars,
		ArgTypes, Cond, Context, ClausesInfo0, Status, Markers,
		none, predicate, ClassContext, Proofs, PredInfo0),
	ArgLives = no,
	module_info_globals(Module0, Globals),
	globals__get_args_method(Globals, ArgsMethod),
	add_new_proc(PredInfo0, Arity, ArgModes, yes(ArgModes),
		ArgLives, yes(Det), Context, ArgsMethod, PredInfo, _),

	module_info_get_predicate_table(Module0, PredicateTable0),
	predicate_table_insert(PredicateTable0, PredInfo, may_be_unqualified, 
		PredId, PredicateTable),
	module_info_set_predicate_table(Module0, PredicateTable,
		Module1),
	module_info_get_special_pred_map(Module1, SpecialPredMap0),
	map__set(SpecialPredMap0, SpecialPredId - TypeId, PredId,
		SpecialPredMap),
	module_info_set_special_pred_map(Module1, SpecialPredMap, Module).

:- pred adjust_special_pred_status(import_status, special_pred_id,
				import_status).
:- mode adjust_special_pred_status(in, in, out) is det.

adjust_special_pred_status(Status0, SpecialPredId, Status) :-
	( ( Status0 = opt_imported ; Status0 = abstract_imported ) ->
		Status1 = imported
	; Status0 = abstract_exported ->
		Status1 = exported
	;
		Status1 = Status0
	),

	% unification predicates are special - they are 
	% "pseudo"-imported/exported (only mode 0 is imported/exported).
	( SpecialPredId = unify ->
		( Status1 = imported ->
			Status = pseudo_imported
		; Status1 = exported ->
			Status = pseudo_exported
		;
			Status = Status1
		)
	;
		Status = Status1
	).

add_new_proc(PredInfo0, Arity, ArgModes, MaybeDeclaredArgModes, MaybeArgLives, 
		MaybeDet, Context, ArgsMethod, PredInfo, ModeId) :-
	pred_info_procedures(PredInfo0, Procs0),
	pred_info_arg_types(PredInfo0, ArgTypes),
	next_mode_id(Procs0, MaybeDet, ModeId),
	proc_info_init(Arity, ArgTypes, ArgModes, MaybeDeclaredArgModes,
		MaybeArgLives, MaybeDet, Context, ArgsMethod, NewProc),
	map__det_insert(Procs0, ModeId, NewProc, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

%-----------------------------------------------------------------------------%

	% Add a mode declaration for a predicate.

:- pred module_add_mode(module_info, inst_varset, sym_name, argument_modes,
		maybe(determinism), condition, prog_context, pred_or_func,
		pair(pred_id, proc_id), module_info, 
		io__state, io__state).
:- mode module_add_mode(in, in, in, in, in, in, in, in, out, out, 
		di, uo) is det.

	% We should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments.

module_add_mode(ModuleInfo0, _VarSet, PredName, Modes, MaybeDet, _Cond,
			MContext, PredOrFunc, PredProcId, ModuleInfo) -->

		% Lookup the pred or func declaration in the predicate table.
		% If it's not there (or if it is ambiguous), optionally print a
		% warning message and insert an implicit definition for the
		% predicate; it is presumed to be local, and its type
		% will be inferred automatically.

	{ module_info_name(ModuleInfo0, ModuleName0) },
	{ sym_name_get_module_name(PredName, ModuleName0, ModuleName) },
	{ Modes = argument_modes(_InstTable, ModesList) },
	{ list__length(ModesList, Arity) },
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity,
			[PredId0]) }
	->
		{ PredicateTable1 = PredicateTable0 },
		{ PredId = PredId0 }
	;
		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			MContext, "mode declaration"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, MContext,
				PredOrFunc,
				PredId, PredicateTable1) }
	),

		% Lookup the pred_info for this predicate
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },

		% check that the determinism was specified
	(
		{ MaybeDet = no }
	->
		{ pred_info_import_status(PredInfo0, ImportStatus) },
		( { status_is_exported(ImportStatus, yes) } ->
			unspecified_det_for_exported(PredName, Arity,
				PredOrFunc, MContext)
		;
			globals__io_lookup_bool_option(infer_det, InferDet),
			(
				{ InferDet = no }
			->
				unspecified_det_for_local(PredName, Arity,
					PredOrFunc, MContext)
			;
				[]
			)
		)
	;
		[]
	),

		% add the mode declaration to the pred_info for this procedure.
	{ ArgLives = no },
	globals__io_get_args_method(ArgsMethod),
	{ add_new_proc(PredInfo0, Arity, Modes, yes(Modes), ArgLives,
			MaybeDet, MContext, ArgsMethod, PredInfo, ProcId) },
	{ map__det_update(Preds0, PredId, PredInfo, Preds) },
	{ predicate_table_set_preds(PredicateTable1, Preds, PredicateTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo) },
	{ PredProcId = PredId - ProcId }.

	% Whenever there is a clause or mode declaration for an undeclared
	% predicate, we add an implicit declaration
	%	:- pred p(T1, T2, ..., Tn).
	% for that predicate; the real types will be inferred by
	% type inference.

:- pred preds_add_implicit(predicate_table, module_name, sym_name, arity,
		prog_context, pred_or_func, pred_id, predicate_table).
:- mode preds_add_implicit(in, in, in, in, in, in, out, out) is det.

preds_add_implicit(PredicateTable0,
			ModuleName, PredName, Arity, Context, PredOrFunc,
			PredId, PredicateTable) :-
	varset__init(TVarSet0),
	make_n_fresh_vars("T", Arity, TVarSet0, TypeVars, TVarSet),
	term__var_list_to_term_list(TypeVars, Types),
	Cond = true,
	clauses_info_init(Arity, ClausesInfo),
	map__init(Proofs),
		% The class context is empty since this is an implicit
		% definition. Inference will fill it in.
	ClassContext = constraints([], []),
		% We assume none of the arguments are existentially typed.
		% Existential types must be declared, they won't be inferred.
	ExistQVars = [],
	init_markers(Markers0),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, ExistQVars,
		Types, Cond, Context, ClausesInfo, local, Markers0, none,
		PredOrFunc, ClassContext, Proofs, PredInfo0),
	add_marker(Markers0, infer_type, Markers),
	pred_info_set_markers(PredInfo0, Markers, PredInfo),
	(
		\+ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, _)
	->
		predicate_table_insert(PredicateTable0, PredInfo, 
			may_be_unqualified, PredId, PredicateTable)
	;	
		error("preds_add_implicit")
	).

	% This is a quick hack, efficiency could be improved --
	% we should probably store the next available ModeId rather
	% than recomputing it all the time.
	% The unused second argument is there for obsolete historical reasons.

next_mode_id(Procs, _MaybeDet, ModeId) :-
	map__to_assoc_list(Procs, List),
	list__length(List, ModeInt),
	proc_id_to_int(ModeId, ModeInt).

%-----------------------------------------------------------------------------%

:- pred module_add_pred_clause(module_info, prog_varset, sym_name,
		list(prog_term), goal, import_status, prog_context,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode module_add_pred_clause(in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.

module_add_pred_clause(ModuleInfo0, ClauseVarSet, PredName, Args, Body,
			Status, Context, ModuleInfo, Info0, Info) -->
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		{ list__length(Args, Arity) },
		io__write_string("% Processing clause for predicate `"),
		hlds_out__write_pred_call_id(PredName/Arity),
		io__write_string("'...\n")
	;
		[]
	),
	module_add_clause(ModuleInfo0, ClauseVarSet, PredName, Args, Body,
		Status, Context, predicate, ModuleInfo, Info0, Info).

:- pred module_add_func_clause(module_info, prog_varset, sym_name,
		list(prog_term), prog_term, goal, import_status, prog_context,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode module_add_func_clause(in, in, in, in, in,
	in, in, in, out, in, out, di, uo) is det.

module_add_func_clause(ModuleInfo0, ClauseVarSet, FuncName, Args0, Result, Body,
			Status, Context, ModuleInfo, Info0, Info) -->
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("% Processing clause for function `"),
		{ list__length(Args0, Arity) },
		hlds_out__write_pred_call_id(FuncName/Arity),
		io__write_string("'...\n")
	;
		[]
	),
	{ list__append(Args0, [Result], Args) },
	module_add_clause(ModuleInfo0, ClauseVarSet, FuncName, Args, Body,
		Status, Context, function, ModuleInfo, Info0, Info).

:- pred module_add_clause(module_info, prog_varset, sym_name, list(prog_term),
		goal, import_status, prog_context, pred_or_func,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode module_add_clause(in, in, in, in, in, in, in, in,
		out, in, out, di, uo) is det.

module_add_clause(ModuleInfo0, ClauseVarSet, PredName, Args, Body, Status,
			Context, PredOrFunc, ModuleInfo, Info0, Info) -->
		% Lookup the pred declaration in the predicate table.
		% (If it's not there, call maybe_undefined_pred_error
		% and insert an implicit declaration for the predicate.)
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ list__length(Args, Arity) },
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
				PredOrFunc, PredName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ PredicateTable1 = PredicateTable0 }
	;

		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			Context, "clause"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				PredOrFunc,
				PredId, PredicateTable1) }
	),
		% Lookup the pred_info for this pred,
		% add the clause to the clauses_info in the pred_info,
		% if there are no modes add an `infer_modes' marker,
		% and then save the pred_info.
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause for them
	{ Status = opt_imported ->
		pred_info_set_import_status(PredInfo0, opt_imported, PredInfo1)
	;
		PredInfo1 = PredInfo0
	},
	(
		{ pred_info_get_goal_type(PredInfo1, pragmas) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: clause for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  with `:- pragma c_code' declaration preceding.\n"),
		{ Info = Info0 }
	;
		{
		pred_info_clauses_info(PredInfo1, Clauses0),
		pred_info_typevarset(PredInfo1, TVarSet0),
		maybe_add_default_mode(ModuleInfo0, PredInfo1, PredInfo2, _),
		pred_info_procedures(PredInfo2, Procs),
		map__keys(Procs, ModeIds)
		},
		clauses_info_add_clause(Clauses0, PredId, ModeIds,
			ClauseVarSet, TVarSet0, Args, Body, Context, Goal,
			VarSet, TVarSet, Clauses, Warnings, Info0, Info),
		{
		pred_info_set_clauses_info(PredInfo2, Clauses, PredInfo3),
		pred_info_set_goal_type(PredInfo3, clauses, PredInfo4),
		pred_info_set_typevarset(PredInfo4, TVarSet, PredInfo5),
		pred_info_arg_types(PredInfo5, _ArgTVarSet,
				ExistQVars, ArgTypes),
		pred_info_set_arg_types(PredInfo5, TVarSet,
				ExistQVars, ArgTypes, PredInfo6),

		%
		% check if there are still no modes for the predicate,
		% and if so, set the `infer_modes' flag for that predicate
		%
		( ModeIds = [] ->
			pred_info_get_markers(PredInfo6, Markers0),
			add_marker(Markers0, infer_modes, Markers),
			pred_info_set_markers(PredInfo6, Markers, PredInfo)
		;
			PredInfo = PredInfo6
		),
		map__det_update(Preds0, PredId, PredInfo, Preds),
		predicate_table_set_preds(PredicateTable1, Preds,
			PredicateTable),
		module_info_set_predicate_table(ModuleInfo0, PredicateTable,
			ModuleInfo)
		},
		( { Status \= opt_imported } ->
			% warn about singleton variables 
			maybe_warn_singletons(VarSet,
				PredOrFunc - PredName/Arity, ModuleInfo, Goal),
			% warn about variables with overlapping scopes
			maybe_warn_overlap(Warnings, VarSet, PredOrFunc,
						PredName/Arity)
		;
			[]
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_c_header(string, prog_context, module_info, module_info).
:- mode module_add_c_header(in, in, in, out) is det.

module_add_c_header(C_Header, Context, Module0, Module) :-
	module_info_get_c_header(Module0, C_HeaderIndex0),
		% store the c headers in reverse order and reverse them later
		% for efficiency
	C_HeaderIndex1 = [C_Header - Context|C_HeaderIndex0],
	module_info_set_c_header(Module0, C_HeaderIndex1, Module).
	
:- pred module_add_c_body_code(string, prog_context, module_info, module_info).
:- mode module_add_c_body_code(in, in, in, out) is det.

module_add_c_body_code(C_Body_Code, Context, Module0, Module) :-
	module_info_get_c_body_code(Module0, C_Body_List0),
		% store the c headers in reverse order and reverse them later
		% for efficiency
	C_Body_List = [C_Body_Code - Context | C_Body_List0],
	module_info_set_c_body_code(Module0, C_Body_List, Module).
	
%-----------------------------------------------------------------------------%
%
% module_add_pragma_import:
%	Handles `pragma import' declarations, by figuring out which predicate
%	the `pragma import' declaration applies to, and adding a clause
%	for that predicate containing an appropriate HLDS `pragma_c_code'
%	instruction.
%	(Note: `pragma import' and `pragma c_code' are distinct at the
%	parse_tree stage, but make_hlds converts both `pragma import'
%	and `pragma c_code' into HLDS `pragma_c_code' instructions,
%	so from HLDS onwards they are indistinguishable.)
%
%	NB. Any changes here might also require similar changes to the
%	handling of `pragma export' declarations, in export.m.

:- pred module_add_pragma_import(sym_name, pred_or_func, argument_modes,
		pragma_c_code_attributes, string, import_status, prog_context,
		module_info, module_info, qual_info, qual_info,
		io__state, io__state).
:- mode module_add_pragma_import(in, in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.

module_add_pragma_import(PredName, PredOrFunc, Modes, Attributes,
		C_Function, Status, Context, ModuleInfo0, ModuleInfo,
		Info0, Info) -->
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ Modes = argument_modes(_ArgInstTable, ArgModes) },
	{ list__length(ArgModes, Arity) },

		%
		% print out a progress message
		%
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma import' for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("...\n")
	;
		[]
	),

		%
		% Lookup the pred declaration in the predicate table.
		% (If it's not there, print an error message and insert
		% a dummy declaration for the predicate.) 
		%
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) }, 
	( 
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ PredicateTable1 = PredicateTable0 }
	;
		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			Context, "`:- pragma import' declaration"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				PredOrFunc, PredId, PredicateTable1) }
	),
		%
		% Lookup the pred_info for this pred,
		% and check that it is valid.
		%
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause (including
	% a `pragma import' clause) for them
	{ Status = opt_imported ->
		pred_info_set_import_status(PredInfo0, opt_imported, PredInfo1)
	;
		PredInfo1 = PredInfo0
	},
	( 
		{ pred_info_is_imported(PredInfo1) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma import' "),
		io__write_string("declaration for imported "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n"),
		{ Info = Info0 }
	;	
		{ pred_info_get_goal_type(PredInfo1, clauses) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma import' declaration "),
		io__write_string("for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  with preceding clauses.\n"),
		{ Info = Info0 }
	;
		{ pred_info_set_goal_type(PredInfo1, pragmas, PredInfo2) },
		%
		% add the pragma declaration to the proc_info for this procedure
		%
		{ pred_info_procedures(PredInfo2, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		(
			% YYY What if ArgInstTable is non-empty?
			{ get_procedure_matching_argmodes(ExistingProcs,
					ArgModes, ModuleInfo0, ProcId) }
		->
			pred_add_pragma_import(PredInfo2, PredId, ProcId,
				Attributes, C_Function, Context,
				ModuleInfo0, PredInfo, Info0, Info),
			{ map__det_update(Preds0, PredId, PredInfo, Preds) },
			{ predicate_table_set_preds(PredicateTable1, Preds,
				PredicateTable) },
			{ module_info_set_predicate_table(ModuleInfo0,
				PredicateTable, ModuleInfo) }
		;
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }, 
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma import' "),
			io__write_string("declaration for undeclared mode "),
			io__write_string("of "),
			hlds_out__write_call_id(PredOrFunc, PredName/Arity),
			io__write_string(".\n"),
			io__set_output_stream(OldStream, _),
			{ Info = Info0 }
		)
	).

% pred_add_pragma_import:
%	This is a subroutine of module_add_pragma_import which adds
%	the c_code for a `pragma import' declaration to a pred_info.

:- pred pred_add_pragma_import(pred_info, pred_id, proc_id,
		pragma_c_code_attributes, string, prog_context, module_info,
		pred_info, qual_info, qual_info, io__state, io__state).
:- mode pred_add_pragma_import(in, in, in, in, in, in, in, out, in, out,
		di, uo) is det.
pred_add_pragma_import(PredInfo0, PredId, ProcId, Attributes, C_Function,
		Context, ModuleInfo, PredInfo, Info0, Info) -->
	%
	% lookup some information we need from the pred_info and proc_info
	%
	{ pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc) },
	{ pred_info_clauses_info(PredInfo0, Clauses0) },
	{ pred_info_arg_types(PredInfo0, ArgTypes) },
	{ pred_info_get_purity(PredInfo0, Purity) },
	{ pred_info_procedures(PredInfo0, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ proc_info_argmodes(ProcInfo, Modes) },
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
	{ proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap) },

	%
	% Build a list of argument variables, together with their
	% names, modes, and types.
	%
	{ varset__init(VarSet0) },
	{ Modes = argument_modes(ArgInstTable, ArgModes) },
	{ list__length(ArgModes, Arity) },
	{ varset__new_vars(VarSet0, Arity, Vars, VarSet) },
	{ create_pragma_vars(Vars, ArgModes, 0, PragmaVars) },
	{ assoc_list__from_corresponding_lists(PragmaVars, ArgTypes,
			PragmaVarsAndTypes) },

	%
	% Construct the C_Code string for calling C_Function.
	% This C code fragment invokes the specified C function
	% with the appropriate arguments from the list constructed
	% above, passed in the appropriate manner (by value, or by
	% passing the address to simulate pass-by-reference), and
	% assigns the return value (if any) to the appropriate place.
	%
	{ handle_return_value(CodeModel, PredOrFunc, PragmaVarsAndTypes,
		InstMap, ArgInstTable, ModuleInfo, ArgPragmaVarsAndTypes,
		C_Code0) },
	{ string__append_list([C_Code0, C_Function, "("], C_Code1) },
	{ assoc_list__keys(ArgPragmaVarsAndTypes, ArgPragmaVars) },
	{ create_pragma_import_c_code(ArgPragmaVars, InstMap, ArgInstTable,
			ModuleInfo, C_Code1, C_Code2) },
	{ string__append(C_Code2, ");", C_Code) },

	%
	% Add the C_Code for this `pragma import' to the clauses_info
	%
	{ PragmaImpl = ordinary(C_Code, no) },
	clauses_info_add_pragma_c_code(Clauses0, Purity, Attributes,
		PredId, ProcId, VarSet, PragmaVars, ArgTypes, PragmaImpl,
		Context, Clauses, Info0, Info),

	%
	% Store the clauses_info etc. back into the pred_info
	%
	{ pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo) }.

%
% handle_return_value(CodeModel, PredOrFunc, Args0, M, Args, C_Code0):
%	Figures out what to do with the C function's return value,
%	based on Mercury procedure's code model, whether it is a predicate
%	or a function, and (if it is a function) the type and mode of the
%	function result.  Constructs a C code fragment `C_Code0' which
%	is a string of the form "<Something> =" that assigns the return
%	value to the appropriate place, if there is a return value,
%	or is an empty string, if there is no return value.
%	Returns in Args all of Args0 that must be passed as arguments
%	(i.e. all of them, or all of them except the return value).
%
:- pred handle_return_value(code_model, pred_or_func,
		assoc_list(pragma_var, type), instmap, inst_table,
		module_info, assoc_list(pragma_var, type), string).
:- mode handle_return_value(in, in, in, in, in, in, out, out) is det.

handle_return_value(CodeModel, PredOrFunc, Args0, InstMap, InstTable,
			ModuleInfo, Args, C_Code0) :-
	( CodeModel = model_det,
		(
			PredOrFunc = function,
			pred_args_to_func_args(Args0, Args1, RetArg),
			RetArg = pragma_var(_, RetArgName, RetMode) - RetType,
			mode_to_arg_mode(InstMap, InstTable, ModuleInfo,
				RetMode, RetType, RetArgMode),
			RetArgMode = top_out,
			\+ export__exclude_argument_type(RetType)
		->
			string__append(RetArgName, " = ", C_Code0),
			Args2 = Args1
		;
			C_Code0 = "",
			Args2 = Args0
		)
	; CodeModel = model_semi,
		% we treat semidet functions the same as semidet predicates,
		% which means that for Mercury functions the Mercury return
		% value becomes the last argument, and the C return value
		% is a bool that is used to indicate success or failure.
		C_Code0 = "SUCCESS_INDICATOR = ",
		Args2 = Args0
	; CodeModel = model_non,
		% XXX we should report an error here, rather than generating
		% C code with `#error'...
		C_Code0 = "\n#error ""cannot import nondet procedure""\n",
		Args2 = Args0
	),
	list__filter(include_import_arg(InstMap, InstTable, ModuleInfo),
			Args2, Args).

%
% include_import_arg(M, Arg):
%	Succeeds iff Arg should be included in the arguments of the C
%	function.  Fails if `Arg' has a type such as `io__state' that
%	is just a dummy argument that should not be passed to C.
%
:- pred include_import_arg(instmap, inst_table, module_info,
			pair(pragma_var, type)).
:- mode include_import_arg(in, in, in, in) is semidet.

include_import_arg(InstMap, InstTable, ModuleInfo,
			pragma_var(_Var, _Name, Mode) - Type) :-
	mode_to_arg_mode(InstMap, InstTable, ModuleInfo, Mode, Type, ArgMode),
	ArgMode \= top_unused,
	\+ export__exclude_argument_type(Type).

%
% create_pragma_vars(Vars, Modes, ArgNum0, PragmaVars):
%	given list of vars and modes, and an initial argument number,
%	allocate names to all the variables, and
%	construct a single list containing the variables, names, and modes.
%
:- pred create_pragma_vars(list(prog_var), list(mode), int, list(pragma_var)).
:- mode create_pragma_vars(in, in, in, out) is det.

create_pragma_vars([], [], _Num, []).

create_pragma_vars([Var|Vars], [Mode|Modes], ArgNum0,
		[PragmaVar | PragmaVars]) :-
	%
	% Figure out a name for the C variable which will hold this argument
	%
	ArgNum is ArgNum0 + 1,
	string__int_to_string(ArgNum, ArgNumString),
	string__append("Arg", ArgNumString, ArgName),

	PragmaVar = pragma_var(Var, ArgName, Mode),

	create_pragma_vars(Vars, Modes, ArgNum, PragmaVars).

create_pragma_vars([_|_], [], _, _) :-
	error("create_pragma_vars: length mis-match").
create_pragma_vars([], [_|_], _, _) :-
	error("create_pragma_vars: length mis-match").

%
% create_pragma_import_c_code(PragmaVars, M, C_Code0, C_Code):
%	This predicate creates the C code fragments for each argument
%	in PragmaVars, and appends them to C_Code0, returning C_Code.
%
:- pred create_pragma_import_c_code(list(pragma_var), instmap, inst_table,
				module_info, string, string).
:- mode create_pragma_import_c_code(in, in, in, in, in, out) is det.

create_pragma_import_c_code([], _InstMap, _InstTable, _ModuleInfo,
		C_Code, C_Code).

create_pragma_import_c_code([PragmaVar | PragmaVars], InstMap, InstTable,
		ModuleInfo, C_Code0, C_Code) :-
	PragmaVar = pragma_var(_Var, ArgName, Mode),

	%
	% Construct the C code fragment for passing this argument,
	% and append it to C_Code0.
	% Note that C handles output arguments by passing the variable'
	% address, so if the mode is output, we need to put an `&' before
	% the variable name.
	%
	( mode_is_output(InstMap, InstTable, ModuleInfo, Mode) ->
		string__append(C_Code0, "&", C_Code1)
	;
		C_Code1 = C_Code0
	),
	string__append(C_Code1, ArgName, C_Code2),
	( PragmaVars \= [] ->
		string__append(C_Code2, ", ", C_Code3)
	;
		C_Code3 = C_Code2
	),

	create_pragma_import_c_code(PragmaVars, InstMap, InstTable, ModuleInfo,
			C_Code3, C_Code).

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_c_code(pragma_c_code_attributes, sym_name,
	pred_or_func, list(pragma_var), prog_varset, pragma_c_code_impl,
	import_status, prog_context, module_info, module_info,
	qual_info, qual_info, io__state, io__state).
:- mode module_add_pragma_c_code(in, in, in, in, in, in, in, in, in, out,
	in, out, di, uo) is det.  

module_add_pragma_c_code(Attributes, PredName, PredOrFunc, PVars, VarSet, 
		PragmaImpl, Status, Context, ModuleInfo0, ModuleInfo,
		Info0, Info) --> 
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ list__length(PVars, Arity) },
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma c_code' for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("...\n")
	;
		[]
	),

		% Lookup the pred declaration in the predicate table.
		% (If it's not there, print an error message and insert
		% a dummy declaration for the predicate.) 
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) }, 
	( 
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ PredicateTable1 = PredicateTable0 }
	;
		maybe_undefined_pred_error(PredName, Arity, PredOrFunc,
			Context, "`:- pragma c_code' declaration"),
		{ preds_add_implicit(PredicateTable0,
			ModuleName, PredName, Arity, Context,
			PredOrFunc, PredId, PredicateTable1) }
	),
		% Lookup the pred_info for this pred,
		% add the pragma to the proc_info in the proc_table in the
		% pred_info, and save the pred_info.
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause (including
	% a `pragma c_code' clause) for them
	{ Status = opt_imported ->
		pred_info_set_import_status(PredInfo0, opt_imported, PredInfo1)
	;
		PredInfo1 = PredInfo0
	},
	( 
		{ pred_info_is_imported(PredInfo1) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma c_code' "),
		io__write_string("declaration for imported "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n"),
		{ Info = Info0 }
	;	
		{ pred_info_get_goal_type(PredInfo1, clauses) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma c_code' declaration "),
		io__write_string("for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  with preceding clauses.\n"),
		{ Info = Info0 }
	;
		% add the pragma declaration to the proc_info for this procedure
		{ pred_info_procedures(PredInfo1, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		{ pragma_get_modes(PVars, Modes) },
		(
			{ get_procedure_matching_argmodes(ExistingProcs, Modes,
						ModuleInfo0, ProcId) }
		->
			{ pred_info_clauses_info(PredInfo1, Clauses0) },
			{ pred_info_arg_types(PredInfo1, ArgTypes) },
			{ pred_info_get_purity(PredInfo1, Purity) },
			clauses_info_add_pragma_c_code(Clauses0, Purity,
				Attributes, PredId, ProcId, VarSet,
				PVars, ArgTypes, PragmaImpl, Context,
				Clauses, Info0, Info),
			{ pred_info_set_clauses_info(PredInfo1, Clauses, 
				PredInfo2) },
			{ pred_info_set_goal_type(PredInfo2, pragmas, 
				PredInfo) },
			{ map__det_update(Preds0, PredId, PredInfo, Preds) },
			{ predicate_table_set_preds(PredicateTable1, Preds, 
				PredicateTable) },
			{ module_info_set_predicate_table(ModuleInfo0, 
				PredicateTable, ModuleInfo) },
			{ pragma_get_var_infos(PVars, ArgInfo) },
			{ inst_table_init(InstTable) },		% YYY
			{ instmap__init_reachable(InstMap) },	% YYY
			maybe_warn_pragma_singletons(PragmaImpl, InstMap,
				InstTable, ArgInfo, Context,
				PredOrFunc - PredName/Arity, ModuleInfo)
		;
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }, 
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma c_code' "),
			io__write_string("declaration for undeclared mode "),
			io__write_string("of "),
			hlds_out__write_call_id(PredOrFunc, PredName/Arity),
			io__write_string(".\n"),
			io__set_output_stream(OldStream, _),
			{ Info = Info0 }
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_tabled(eval_method, sym_name, int, 
		maybe(pred_or_func), maybe(argument_modes), 
		prog_context, module_info, module_info, 
		io__state, io__state).
:- mode module_add_pragma_tabled(in, in, in, in, in, in, in, out, 
	di, uo) is det. 
	
module_add_pragma_tabled(EvalMethod, PredName, Arity, MaybePredOrFunc, 
		MaybeModes,  Context, ModuleInfo0, ModuleInfo) --> 
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) }, 
 	{ eval_method_to_string(EvalMethod, EvalMethodS) },
		
	% Find out if we are tabling a predicate or a function 
	(
		{ MaybePredOrFunc = yes(PredOrFunc0) }
	->
		{ PredOrFunc = PredOrFunc0 },

			% Lookup the pred declaration in the predicate table.
			% (If it's not there, print an error message and insert
			% a dummy declaration for the predicate.) 
		(
			{ predicate_table_search_pf_sym_arity(PredicateTable0,
				PredOrFunc, PredName, Arity, PredIds0) }
		->
			{ PredIds = PredIds0 },
			{ ModuleInfo1 = ModuleInfo0 }	
		;
			{ module_info_name(ModuleInfo0, ModuleName) },
			{ string__format("pragma (%s)", [s(EvalMethodS)], 
				Message1) },
			maybe_undefined_pred_error(PredName, Arity, 
				PredOrFunc, Context, Message1),
			{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				PredOrFunc, PredId, PredicateTable1) },
			{ module_info_set_predicate_table(ModuleInfo0,
				PredicateTable1, ModuleInfo1) },
			{ PredIds = [PredId] }
		)
	;
		(	
			{ predicate_table_search_sym_arity(PredicateTable0,
					PredName, Arity, PredIds0) }
		->
			{ ModuleInfo1 = ModuleInfo0 },
			{ PredIds = PredIds0 }
		;
			{ module_info_name(ModuleInfo0, ModuleName) },
			{ string__format("pragma (%s)", [s(EvalMethodS)], 
				Message1) },
			maybe_undefined_pred_error(PredName, Arity, 
				predicate, Context, Message1),
			{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				predicate, PredId, PredicateTable1) },
			{ module_info_set_predicate_table(ModuleInfo0,
				PredicateTable1, ModuleInfo1) },
			{ PredIds = [PredId] }
		)
	),
	list__foldl2(module_add_pragma_tabled_2(EvalMethod, PredName, 
			Arity, MaybePredOrFunc, MaybeModes, Context), 
			PredIds, ModuleInfo1, ModuleInfo).


:- pred module_add_pragma_tabled_2(eval_method, sym_name, int, 
		maybe(pred_or_func), maybe(argument_modes), prog_context,
		pred_id, module_info, module_info, io__state, io__state).
:- mode module_add_pragma_tabled_2(in, in, in, in, in, in, in, in, out,
		di, uo) is det.

module_add_pragma_tabled_2(EvalMethod, PredName, Arity0, MaybePredOrFunc, 
		MaybeModes, Context, PredId, ModuleInfo0, ModuleInfo) -->
	
		% Lookup the pred_info for this pred,
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable) }, 
	{ predicate_table_get_preds(PredicateTable, Preds) },
	{ map__lookup(Preds, PredId, PredInfo0) },
	
	% Find out if we are tabling a predicate or a function 
	(
		{ MaybePredOrFunc = yes(PredOrFunc0) }
	->
		{ PredOrFunc = PredOrFunc0 }
	;
		{ pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc) }
	),
	(
		{ PredOrFunc = predicate },
		{ Arity = Arity0 }
	;
		{ PredOrFunc = function },
		{ Arity is Arity0 + 1 }
	),
		
		% print out a progress message
	{ eval_method_to_string(EvalMethod, EvalMethodS) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma "),
		io__write_string(EvalMethodS),
		io__write_string("' for "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string("...\n")
	;
		[]
	),
	
	( 
		{ pred_info_is_imported(PredInfo0) }
	->
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma "),
		io__write_string(EvalMethodS),
		io__write_string("' declaration for imported "),
		hlds_out__write_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n")
	;
		% do we have to make sure the tabled preds are stratified?
		(
			{ eval_method_need_stratification(EvalMethod) }
		->
			{ module_info_stratified_preds(ModuleInfo0, 
				StratPredIds0) },
			{ set__insert(StratPredIds0, PredId, StratPredIds) },
			{ module_info_set_stratified_preds(ModuleInfo0, 
				StratPredIds, ModuleInfo1) }
		;
			{ ModuleInfo1 = ModuleInfo0 }
		),
		
		% add the eval model to the proc_info for this procedure
		{ pred_info_procedures(PredInfo0, Procs0) },
		{ map__to_assoc_list(Procs0, ExistingProcs) },
		(
			{ MaybeModes = yes(argument_modes(_ArgIKT, Modes)) } 
				% YYY Assumes ArgIKT is empty
		->
			(
				{ get_procedure_matching_argmodes(
					ExistingProcs, Modes, ModuleInfo1, 
					ProcId) }
			->
				{ map__lookup(Procs0, ProcId, ProcInfo0) },
				{ proc_info_set_eval_method(ProcInfo0, 
					EvalMethod, ProcInfo) },
				{ map__det_update(Procs0, ProcId, ProcInfo, 
					Procs) },
				{ pred_info_set_procedures(PredInfo0, Procs, 
					PredInfo) },
				{ module_info_set_pred_info(ModuleInfo1, 
					PredId, PredInfo, ModuleInfo) }
			;
				{ module_info_incr_errors(ModuleInfo1, 
					ModuleInfo) }, 
				prog_out__write_context(Context),
				io__write_string("Error: `:- pragma "),
				io__write_string(EvalMethodS),
				io__write_string(
				     "' declaration for undeclared mode of "), 
				hlds_out__write_call_id(PredOrFunc, 
					PredName/Arity),
				io__write_string(".\n")
			)
		;
			{ set_eval_method_list(ExistingProcs, EvalMethod, 
				Procs0, Procs) },
			{ pred_info_set_procedures(PredInfo0, Procs, 
				PredInfo) },
			{ module_info_set_pred_info(ModuleInfo1, PredId, 
				PredInfo, ModuleInfo) }
		)
	).

:- pred set_eval_method_list(assoc_list(proc_id, proc_info), eval_method, 
	proc_table, proc_table).
:- mode set_eval_method_list(in, in, in, out) is det.

set_eval_method_list([], _, Procs, Procs).
set_eval_method_list([ProcId - ProcInfo0|Rest], EvalMethod, Procs0, Procs) :-
	proc_info_set_eval_method(ProcInfo0, EvalMethod, ProcInfo),
	map__det_update(Procs0, ProcId, ProcInfo, Procs1),
	set_eval_method_list(Rest, EvalMethod, Procs1, Procs).
	
%-----------------------------------------------------------------------------%

	% from the list of pragma_vars extract the modes.
:- pred pragma_get_modes(list(pragma_var), list(mode)).
:- mode pragma_get_modes(in, out) is det.

pragma_get_modes([], []).
pragma_get_modes([PragmaVar | Vars], [Mode | Modes]) :-
	PragmaVar = pragma_var(_Var, _Name, Mode),
	pragma_get_modes(Vars, Modes).

%-----------------------------------------------------------------------------%

	% from the list of pragma_vars , extract the vars.
:- pred pragma_get_vars(list(pragma_var), list(prog_var)).
:- mode pragma_get_vars(in, out) is det.

pragma_get_vars([], []).
pragma_get_vars([PragmaVar | PragmaVars], [Var | Vars]) :-
	PragmaVar = pragma_var(Var, _Name, _Mode),
	pragma_get_vars(PragmaVars, Vars).

%---------------------------------------------------------------------------%

	% from the list of pragma_vars, extract the names.

:- pred pragma_get_var_infos(list(pragma_var), list(maybe(pair(string, mode)))).
:- mode pragma_get_var_infos(in, out) is det.

pragma_get_var_infos([], []).
pragma_get_var_infos([PragmaVar | PragmaVars], [yes(Name - Mode) | Info]) :-
	PragmaVar = pragma_var(_Var, Name, Mode),
	pragma_get_var_infos(PragmaVars, Info).

%---------------------------------------------------------------------------%

	% For each pred_id in the list, check whether markers
	% present in the list of conflicting markers are
	% also present in the corresponding pred_info.
	% The bool indicates whether there was a conflicting marker
	% present.

:- pred pragma_check_markers(pred_table, list(pred_id), list(marker), bool).
:- mode pragma_check_markers(in, in, in, out) is det.

pragma_check_markers(_, [], _, no).
pragma_check_markers(PredTable, [PredId | PredIds], ConflictList, 
		WasConflict) :-
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	(
		list__member(Marker, ConflictList),
		check_marker(Markers, Marker)
	->
		WasConflict = yes
	;
		pragma_check_markers(PredTable, PredIds, ConflictList,
			WasConflict)
	).

	% For each pred_id in the list, add the given markers to the
	% list of markers in the corresponding pred_info.

:- pred pragma_add_marker(pred_table, list(pred_id), marker, pred_table).
:- mode pragma_add_marker(in, in, in, out) is det.

pragma_add_marker(PredTable, [], _, PredTable).
pragma_add_marker(PredTable0, [PredId | PredIds], Marker, PredTable) :-
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_get_markers(PredInfo0, Markers0),
	add_marker(Markers0, Marker, Markers),
	pred_info_set_markers(PredInfo0, Markers, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable1),
	pragma_add_marker(PredTable1, PredIds, Marker, PredTable).

%---------------------------------------------------------------------------%

	% Find the procedure with argmodes which match the ones we want.

:- pred get_procedure_matching_argmodes(assoc_list(proc_id, proc_info),
		list(mode), module_info, proc_id).
:- mode get_procedure_matching_argmodes(in, in, in, out) is semidet.
get_procedure_matching_argmodes([P|Procs], Modes, ModuleInfo, OurProcId) :-
	P = ProcId - ProcInfo,
	proc_info_argmodes(ProcInfo, argument_modes(_ArgIKT, ArgModes0)),
	% YYY Assume ArgIKT is empty
	ArgModes = ArgModes0,
	( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
		OurProcId = ProcId
	;
		get_procedure_matching_argmodes(Procs, Modes, ModuleInfo, 
			OurProcId)
	).

	% Find the procedure with declared argmodes which match the ones 
	% we want.  If there was no mode declaration, then use the inferred
	% argmodes.
:- pred get_procedure_matching_declmodes(assoc_list(proc_id, proc_info),
		list(mode), module_info, proc_id).
:- mode get_procedure_matching_declmodes(in, in, in, out) is semidet.
get_procedure_matching_declmodes([P|Procs], Modes, ModuleInfo, OurProcId) :-
	P = ProcId - ProcInfo,
	proc_info_declared_argmodes(ProcInfo,
		argument_modes(_ArgIKT, ArgModes0)),
	% YYY Assume ArgIKT is empty
	ArgModes = ArgModes0,
	( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
		OurProcId = ProcId
	;
		get_procedure_matching_declmodes(Procs, Modes, ModuleInfo, 
			OurProcId)
	).

:- pred mode_list_matches(list(mode), list(mode), module_info).
:- mode mode_list_matches(in, in, in) is semidet.

mode_list_matches([], [], _).
mode_list_matches([Mode1 | Modes1], [Mode2 | Modes2], ModuleInfo) :-
	% Use mode_get_insts_semidet instead of mode_get_insts to avoid
	% aborting if there are undefined modes.
	mode_get_insts_semidet(ModuleInfo, Mode1, Inst1, Inst2),
	mode_get_insts_semidet(ModuleInfo, Mode2, Inst1, Inst2),
	mode_list_matches(Modes1, Modes2, ModuleInfo).

%-----------------------------------------------------------------------------%

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once.
	%
:- pred maybe_warn_overlap(list(quant_warning), prog_varset,
				pred_or_func, pred_call_id,
				io__state, io__state).
:- mode maybe_warn_overlap(in, in, in, in, di, uo) is det.

maybe_warn_overlap(Warnings, VarSet, PredOrFunc, PredCallId) -->
	globals__io_lookup_bool_option(warn_overlapping_scopes,
			WarnOverlappingScopes),
	( { WarnOverlappingScopes = yes } ->
		warn_overlap(Warnings, VarSet, PredOrFunc, PredCallId)
	;	
		[]
	).

:- pred warn_overlap(list(quant_warning), prog_varset, pred_or_func,
		pred_call_id, io__state, io__state).
:- mode warn_overlap(in, in, in, in, di, uo) is det.

warn_overlap([], _, _, _) --> [].
warn_overlap([Warn|Warns], VarSet, PredOrFunc, PredCallId) -->
	{ Warn = warn_overlap(Vars, Context) },
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string(StdErr, "In clause for "),
	hlds_out__write_call_id(PredOrFunc, PredCallId),
	io__write_string(StdErr, ":\n"),
	prog_out__write_context(Context),
	( { Vars = [Var] } ->
		io__write_string(StdErr, "  warning: variable `"),
		mercury_output_var(Var, VarSet, no),
		report_warning(StdErr, "' has overlapping scopes.\n")
	;
		io__write_string(StdErr, "  warning: variables `"),
		mercury_output_vars(Vars, VarSet, no),
		report_warning(StdErr, "'\n"),
		prog_out__write_context(Context),
		report_warning(StdErr, "  each have overlapping scopes.\n")
	),
	io__set_output_stream(OldStream, _),
	warn_overlap(Warns, VarSet, PredOrFunc, PredCallId).

%-----------------------------------------------------------------------------%

:- type pred_or_func_call_id == pair(pred_or_func, pred_call_id).

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once, or about variables that do not occur in
	% C code strings when they should.
	%
:- pred maybe_warn_singletons(prog_varset, pred_or_func_call_id, module_info,
		hlds_goal, io__state, io__state).
:- mode maybe_warn_singletons(in, in, in, in, di, uo) is det.

maybe_warn_singletons(VarSet, PredCallId, ModuleInfo, Body) -->
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars),
	( { WarnSingletonVars = yes } ->
		{ set__init(QuantVars) },
		warn_singletons_in_goal(Body, QuantVars, VarSet, PredCallId,
			ModuleInfo)
	;	
		[]
	).

:- pred warn_singletons_in_goal(hlds_goal, set(prog_var), prog_varset,
	pred_or_func_call_id, module_info, io__state, io__state).
:- mode warn_singletons_in_goal(in, in, in, in, in, di, uo) is det.

warn_singletons_in_goal(Goal - GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet,
		PredCallId, MI).

:- pred warn_singletons_in_goal_2(hlds_goal_expr, hlds_goal_info, set(prog_var),
		prog_varset, pred_or_func_call_id, module_info,
		io__state, io__state).
:- mode warn_singletons_in_goal_2(in, in, in, in, in, in, di, uo) is det.

warn_singletons_in_goal_2(conj(Goals), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(par_conj(Goals, _SM), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(disj(Goals, _), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(switch(_Var, _CanFail, Cases, _),
			_GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_cases(Cases, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(not(Goal), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal(Goal, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(some(Vars, SubGoal), GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	%
	% warn if any quantified variables occur only in the quantifier
	%
	( { Vars \= [] } ->
		{ quantification__goal_vars(SubGoal, SubGoalVars) },
		{ goal_info_get_context(GoalInfo, Context) },
		{ set__init(EmptySet) },
		warn_singletons(Vars, EmptySet, SubGoalVars, VarSet, Context,
			PredCallId)
	;
		[]
	),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	warn_singletons_in_goal(SubGoal, QuantVars1, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(if_then_else(Vars, Cond, Then, Else, _), GoalInfo,
				QuantVars, VarSet, PredCallId, MI) -->
	%
	% warn if any quantified variables do not occur in the condition
	% or the "then" part of the if-then-else
	%
	( { Vars \= [] } ->
		{ quantification__goal_vars(Cond, CondVars) },
		{ quantification__goal_vars(Then, ThenVars) },
		{ set__union(CondVars, ThenVars, CondThenVars) },
		{ goal_info_get_context(GoalInfo, Context) },
		{ set__init(EmptySet) },
		warn_singletons(Vars, EmptySet, CondThenVars, VarSet,
			Context, PredCallId)
	;
		[]
	),

	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	warn_singletons_in_goal(Cond, QuantVars1, VarSet, PredCallId, MI),
	warn_singletons_in_goal(Then, QuantVars1, VarSet, PredCallId, MI),
	warn_singletons_in_goal(Else, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(call(_, _, Args, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId, _) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(higher_order_call(_, Args, _, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId, _) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

	% This code should never be called anyway.
warn_singletons_in_goal_2(class_method_call(_, _, Args, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId, _) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(unify(Var, RHS, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_unify(Var, RHS, GoalInfo, QuantVars, VarSet,
		PredCallId, MI).

warn_singletons_in_goal_2(pragma_c_code(_, _, _, _, ArgInfo0, _, PragmaImpl), 
		GoalInfo, _QuantVars, _VarSet, PredCallId, MI) --> 
	{ goal_info_get_context(GoalInfo, Context) },
	{ ArgInfo0 = pragma_c_code_arg_info(InstTable, ArgInfo) },
	{ instmap__init_reachable(InstMap) },	% YYY
	warn_singletons_in_pragma_c_code(PragmaImpl, InstMap, InstTable,
		ArgInfo, Context, PredCallId, MI).

:- pred warn_singletons_in_goal_list(list(hlds_goal), set(prog_var),
		prog_varset, pred_or_func_call_id, module_info,
		io__state, io__state).
:- mode warn_singletons_in_goal_list(in, in, in, in, in, di, uo) is det.

warn_singletons_in_goal_list([], _, _, _, _) --> [].
warn_singletons_in_goal_list([Goal|Goals], QuantVars, VarSet, CallPredId, MI)
		-->
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI),
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, CallPredId, MI).

:- pred warn_singletons_in_cases(list(case), set(prog_var), prog_varset,
	pred_or_func_call_id, module_info, io__state, io__state).
:- mode warn_singletons_in_cases(in, in, in, in, in, di, uo) is det.

warn_singletons_in_cases([], _, _, _, _) --> [].
warn_singletons_in_cases([Case|Cases], QuantVars, VarSet, CallPredId, MI) -->
	{ Case = case(_ConsId, _IMDelta, Goal) },
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI),
	warn_singletons_in_cases(Cases, QuantVars, VarSet, CallPredId, MI).

:- pred warn_singletons_in_unify(prog_var, unify_rhs, hlds_goal_info,
		set(prog_var), prog_varset, pred_or_func_call_id, module_info,
		io__state, io__state).
:- mode warn_singletons_in_unify(in, in, in, in, in, in, in, di, uo) is det.

warn_singletons_in_unify(X, var(Y), GoalInfo, QuantVars, VarSet, CallPredId, _)
		-->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons([X, Y], NonLocals, QuantVars, VarSet,
			Context, CallPredId).

warn_singletons_in_unify(X, functor(_ConsId, Vars), GoalInfo, QuantVars, VarSet,
				CallPredId, _) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons([X | Vars], NonLocals, QuantVars, VarSet,
			Context, CallPredId).

warn_singletons_in_unify(X, lambda_goal(_PredOrFunc, _NonLocals, LambdaVars, 
				_Modes, _Det, _IMDelta, LambdaGoal),
			GoalInfo, QuantVars, VarSet, CallPredId, MI) -->
	%
	% warn if any lambda-quantified variables occur only in the quantifier
	%
	{ LambdaGoal = _ - LambdaGoalInfo },
	{ goal_info_get_nonlocals(LambdaGoalInfo, LambdaNonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(LambdaVars, LambdaNonLocals, QuantVars, VarSet,
			Context, CallPredId),

	%
	% warn if X (the variable we're unifying the lambda expression with)
	% is singleton
	%
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	warn_singletons([X], NonLocals, QuantVars, VarSet, Context, CallPredId),

	%
	% warn if the lambda-goal contains singletons
	%
	warn_singletons_in_goal(LambdaGoal, QuantVars, VarSet, CallPredId, MI).

%-----------------------------------------------------------------------------%

:- pred maybe_warn_pragma_singletons(pragma_c_code_impl, instmap, inst_table,
	list(maybe(pair(string, mode))), prog_context, pred_or_func_call_id,
	module_info, io__state, io__state).
:- mode maybe_warn_pragma_singletons(in, in, in, in, in, in, in,
	di, uo) is det.

maybe_warn_pragma_singletons(PragmaImpl, InstMap, InstTable, ArgInfo, Context,
			CallId, MI) -->
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars),
	( { WarnSingletonVars = yes } ->
		% YYY { ArgInfo0 = pragma_c_code_arg_info(InstTable, ArgInfo) },
		warn_singletons_in_pragma_c_code(PragmaImpl, InstMap, InstTable,
			ArgInfo, Context, CallId, MI)
	;	
		[]
	).

	% warn_singletons_in_pragma_c_code checks to see if each variable is
	% mentioned at least once in the c code fragments that ought to
	% mention it. If not, it gives a warning.
:- pred warn_singletons_in_pragma_c_code(pragma_c_code_impl, instmap,
		inst_table, list(maybe(pair(string, mode))), prog_context,
		pred_or_func_call_id, module_info, io__state, io__state).
:- mode warn_singletons_in_pragma_c_code(in, in, in, in, in, in, in,
		di, uo) is det.

warn_singletons_in_pragma_c_code(PragmaImpl, InstMap, InstTable, ArgInfo,
		Context, PredOrFunc - PredCallId, ModuleInfo) -->
	(
		{ PragmaImpl = ordinary(C_Code, _) },
		{ c_code_to_name_list(C_Code, C_CodeList) },
		{ solutions(lambda([Name::out] is nondet, (
				list__member(yes(Name - _), ArgInfo),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, C_CodeList)
			)), UnmentionedVars) },
		( { UnmentionedVars = [] } ->
			[]
		;
			io__stderr_stream(StdErr1),
			io__set_output_stream(StdErr1, OldStream1),
			prog_out__write_context(Context),
			io__write_string("In `:- pragma c_code' for "),
			hlds_out__write_call_id(PredOrFunc, PredCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			( { UnmentionedVars = [_] } ->
				io__write_string("  warning: variable `"),
				write_string_list(UnmentionedVars),
				io__write_string("' does not occur in the C code.\n")
			;
				io__write_string("  warning: variables `"),
				write_string_list(UnmentionedVars),
				io__write_string("' do not occur in the C code.\n")
			),
			io__set_output_stream(OldStream1, _)
		)
	;
		{ PragmaImpl = nondet(_, _, FirstCode, _,
			LaterCode, _, _, SharedCode, _) },
		{ c_code_to_name_list(FirstCode, FirstCodeList) },
		{ c_code_to_name_list(LaterCode, LaterCodeList) },
		{ c_code_to_name_list(SharedCode, SharedCodeList) },
		{ solutions(lambda([Name::out] is nondet, (
				list__member(yes(Name - Mode), ArgInfo),
				mode_is_input(InstMap, InstTable, ModuleInfo,
						Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, FirstCodeList)
			)), UnmentionedInputVars) },
		( { UnmentionedInputVars = [] } ->
			[]
		;
			io__stderr_stream(StdErr2),
			io__set_output_stream(StdErr2, OldStream2),
			prog_out__write_context(Context),
			io__write_string("In `:- pragma c_code' for "),
			hlds_out__write_call_id(PredOrFunc, PredCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			( { UnmentionedInputVars = [_] } ->
				io__write_string("  warning: variable `"),
				write_string_list(UnmentionedInputVars),
				io__write_string("' does not occur in the first C code.\n")
			;
				io__write_string("  warning: variables `"),
				write_string_list(UnmentionedInputVars),
				io__write_string("' do not occur in the first C code.\n")
			),
			io__set_output_stream(OldStream2, _)
		),
		{ solutions(lambda([Name::out] is nondet, (
				list__member(yes(Name - Mode), ArgInfo),
				mode_is_output(InstMap, InstTable, ModuleInfo,
						Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, FirstCodeList),
				\+ list__member(Name, SharedCodeList)
			)), UnmentionedFirstOutputVars) },
		( { UnmentionedFirstOutputVars = [] } ->
			[]
		;
			io__stderr_stream(StdErr3),
			io__set_output_stream(StdErr3, OldStream3),
			prog_out__write_context(Context),
			io__write_string("In `:- pragma c_code' for "),
			hlds_out__write_call_id(PredOrFunc, PredCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			( { UnmentionedFirstOutputVars = [_] } ->
				io__write_string("  warning: variable `"),
				write_string_list(UnmentionedFirstOutputVars),
				io__write_string("' does not occur in the first C code or the shared C code.\n")
			;
				io__write_string("  warning: variables `"),
				write_string_list(UnmentionedFirstOutputVars),
				io__write_string("' do not occur in the first C code or the shared C code.\n")
			),
			io__set_output_stream(OldStream3, _)
		),
		{ solutions(lambda([Name::out] is nondet, (
				list__member(yes(Name - Mode), ArgInfo),
				mode_is_output(InstMap, InstTable, ModuleInfo,
						Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, LaterCodeList),
				\+ list__member(Name, SharedCodeList)
			)), UnmentionedLaterOutputVars) },
		( { UnmentionedLaterOutputVars = [] } ->
			[]
		;
			io__stderr_stream(StdErr4),
			io__set_output_stream(StdErr4, OldStream4),
			prog_out__write_context(Context),
			io__write_string("In `:- pragma c_code' for "),
			hlds_out__write_call_id(PredOrFunc, PredCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			( { UnmentionedLaterOutputVars = [_] } ->
				io__write_string("  warning: variable `"),
				write_string_list(UnmentionedLaterOutputVars),
				io__write_string("' does not occur in the retry C code or the shared C code.\n")
			;
				io__write_string("  warning: variables `"),
				write_string_list(UnmentionedLaterOutputVars),
				io__write_string("' do not occur in the retry C code or the shared C code.\n")
			),
			io__set_output_stream(OldStream4, _)
		)
	).

%-----------------------------------------------------------------------------%

	% c_code_to_name_list(Code, List) is true iff List is a list of the 
	% identifiers used in the C code in Code.
:- pred c_code_to_name_list(string, list(string)).
:- mode c_code_to_name_list(in, out) is det.

c_code_to_name_list(Code, List) :-
	string__to_char_list(Code, CharList),
	c_code_to_name_list_2(CharList, List).

:- pred c_code_to_name_list_2(list(char), list(string)).
:- mode c_code_to_name_list_2(in, out) is det.

c_code_to_name_list_2(C_Code, List) :-
	get_first_c_name(C_Code, NameCharList, TheRest),
	(
		NameCharList = []
	->
		% no names left
		List = []
	;
		c_code_to_name_list_2(TheRest, Names),
		string__from_char_list(NameCharList, Name),
		List = [Name|Names]
	).

:- pred get_first_c_name(list(char), list(char), list(char)).
:- mode get_first_c_name(in, out, out) is det.
	
get_first_c_name([], [], []).
get_first_c_name([C|CodeChars], NameCharList, TheRest) :-
	(
		char__is_alnum_or_underscore(C)
	->
		get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
		NameCharList = [C|NameCharList0]
	;
			% strip off any characters in the C code which 
			% don't form part of an identifier.
		get_first_c_name(CodeChars, NameCharList, TheRest)
	).
	
:- pred get_first_c_name_in_word(list(char), list(char), list(char)).
:- mode get_first_c_name_in_word(in, out, out) is det.

get_first_c_name_in_word([], [], []).
get_first_c_name_in_word([C|CodeChars], NameCharList, TheRest) :-
	(
		char__is_alnum_or_underscore(C)
	->
			% There are more characters in the word
		get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
		NameCharList = [C|NameCharList0]
	;
			% The word is finished
		NameCharList = [],
		TheRest = CodeChars
	).

%-----------------------------------------------------------------------------%

:- pred write_string_list(list(string), io__state, io__state).
:- mode write_string_list(in, di, uo) is det.

write_string_list([]) --> [].
write_string_list([X|Xs]) -->
	io__write_string(X),
	(
		{ Xs = [] }
	->
		[]
	;
		io__write_string(", "),
		write_string_list(Xs)
	).

%-----------------------------------------------------------------------------%

	% warn_singletons(Vars, NonLocals, QuantVars, ...):
	% 	Warn if any of the non-underscore variables in Vars don't
	%	occur in NonLocals and don't have the same name as any variable
	%	in QuantVars, or if any of the underscore variables
	%	in Vars do occur in NonLocals.

:- pred warn_singletons(list(prog_var), set(prog_var), set(prog_var),
		prog_varset, prog_context, pred_or_func_call_id,
		io__state, io__state).
:- mode warn_singletons(in, in, in, in, in, in, di, uo) is det.

warn_singletons(GoalVars, NonLocals, QuantVars, VarSet, Context,
		PredOrFunc - CallId) -->
	io__stderr_stream(StdErr),

	% find all the variables in the goal that don't occur outside the
	% goal (i.e. are singleton), have a variable name that doesn't
	% start with "_" or "DCG_", and don't have the same name as any
	% variable in QuantVars (i.e. weren't explicitly quantified).
	
	{ solutions(lambda([Var::out] is nondet, (
		  	list__member(Var, GoalVars),
			\+ set__member(Var, NonLocals),
			varset__search_name(VarSet, Var, Name),
			\+ string__prefix(Name, "_"),
			\+ string__prefix(Name, "DCG_"),
			\+ (	
				set__member(QuantVar, QuantVars),
				varset__search_name(VarSet, QuantVar, Name)
			)
		)), SingletonVars) },

	% if there were any such variables, issue a warning

	( { SingletonVars = [] } ->
		[]
	;
		prog_out__write_context(Context),
		io__write_string(StdErr, "In clause for "),
		hlds_out__write_call_id(PredOrFunc, CallId),
		io__write_string(StdErr, ":\n"),
		prog_out__write_context(Context),
		( { SingletonVars = [_] } ->
			io__write_string(StdErr, "  warning: variable `"),
			mercury_output_vars(SingletonVars, VarSet, no),
			report_warning(StdErr, "' occurs only once in this scope.\n")
		;
			io__write_string(StdErr, "  warning: variables `"),
			mercury_output_vars(SingletonVars, VarSet, no),
			report_warning(StdErr, "' occur only once in this scope.\n")
		)
	),

	% find all the variables in the goal that do occur outside the
	% goal (i.e. are not singleton) and have a variable name that starts
	% with "_".
	
	{ solutions(lambda([Var2::out] is nondet, (
		  	list__member(Var2, GoalVars),
			set__member(Var2, NonLocals),
			varset__search_name(VarSet, Var2, Name2),
			string__prefix(Name2, "_")
		)), MultiVars) },

	% if there were any such variables, issue a warning

	( { MultiVars = [] } ->
		[]
	;
		prog_out__write_context(Context),
		io__write_string(StdErr, "In clause for "),
		hlds_out__write_call_id(PredOrFunc, CallId),
		io__write_string(StdErr, ":\n"),
		prog_out__write_context(Context),
		( { MultiVars = [_] } ->
			io__write_string(StdErr, "  warning: variable `"),
			mercury_output_vars(MultiVars, VarSet, no),
			report_warning(StdErr, "' occurs more than once in this scope.\n")
		;
			io__write_string(StdErr, "  warning: variables `"),
			mercury_output_vars(MultiVars, VarSet, no),
			report_warning(StdErr, "' occur more than once in this scope.\n")
		)
	).

%-----------------------------------------------------------------------------

clauses_info_init(Arity, ClausesInfo) :-
	map__init(VarTypes),
	varset__init(VarSet0),
	make_n_fresh_vars("HeadVar__", Arity, VarSet0, HeadVars, VarSet),
	ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes, HeadVars, []).

:- pred clauses_info_add_clause(clauses_info::in, pred_id::in, 
		list(proc_id)::in, prog_varset::in, tvarset::in,
		list(prog_term)::in, goal::in, prog_context::in,
		hlds_goal::out, prog_varset::out, tvarset::out,
		clauses_info::out, list(quant_warning)::out, qual_info::in,
		qual_info::out, io__state::di, io__state::uo) is det.

clauses_info_add_clause(ClausesInfo0, PredId, ModeIds, CVarSet, TVarSet0,
		Args, Body, Context, Goal, VarSet, TVarSet0,
		ClausesInfo, Warnings, Info0, Info) -->
	{ ClausesInfo0 = clauses_info(VarSet0, VarTypes0, VarTypes1,
					HeadVars, ClauseList0) },
	{ update_qual_info(Info0, TVarSet0, VarTypes0, PredId, Info1) },
	{ varset__merge_subst(VarSet0, CVarSet, VarSet1, Subst) },
	transform(Subst, HeadVars, Args, Body, VarSet1, Context,
				Goal, VarSet, Warnings, Info1, Info),
		% XXX we should avoid append - this gives O(N*N)
	{ list__append(ClauseList0, [clause(ModeIds, Goal, Context)],
							ClauseList) },
	{ qual_info_get_var_types(Info, VarTypes) },
	{ ClausesInfo = clauses_info(VarSet, VarTypes, VarTypes1,
					HeadVars, ClauseList) }.

%-----------------------------------------------------------------------------

% Add the pragma_c_code goal to the clauses_info for this procedure.
% To do so, we must also insert unifications between the variables in the
% pragma c_code declaration and the head vars of the pred. Also return the
% hlds_goal.

:- pred clauses_info_add_pragma_c_code(clauses_info, purity,
	pragma_c_code_attributes, pred_id, proc_id, prog_varset,
	list(pragma_var), list(type), pragma_c_code_impl, prog_context,
	clauses_info, qual_info, qual_info, io__state, io__state) is det.
:- mode clauses_info_add_pragma_c_code(in, in, in, in, in, in, in, in, in, in,
	out, in, out, di, uo) is det.

clauses_info_add_pragma_c_code(ClausesInfo0, Purity, Attributes, PredId,
		ModeId, PVarSet, PVars, OrigArgTypes, PragmaImpl, Context,
		ClausesInfo, Info0, Info) -->
	{
	ClausesInfo0 = clauses_info(VarSet0, VarTypes, VarTypes1,
				 HeadVars, ClauseList),
	pragma_get_vars(PVars, Args0),
	pragma_get_var_infos(PVars, ArgInfo0),
	inst_table_init(InstTable),
	ArgInfo = pragma_c_code_arg_info(InstTable, ArgInfo0),

		% merge the varsets of the proc and the new pragma_c_code
	varset__merge_subst(VarSet0, PVarSet, VarSet1, Subst),
	map__apply_to_list(Args0, Subst, TermArgs),
	term__term_list_to_var_list(TermArgs, Args),

		% build the pragma_c_code
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo1),
	% Put the purity in the goal_info in case this c code is inlined
	add_goal_info_purity_feature(GoalInfo1, Purity, GoalInfo),
	HldsGoal0 = pragma_c_code(Attributes, PredId, ModeId, Args,
		ArgInfo, OrigArgTypes, PragmaImpl) - GoalInfo
	}, 
		% Apply unifications with the head args.
		% Since the set of head vars and the set vars in the
		% pragma C code are disjoint, the unifications can be
		% implemented as substitutions, and they will be.
	insert_arg_unifications(HeadVars, TermArgs, Context, head, yes,
		HldsGoal0, VarSet1, HldsGoal1, VarSet2, Info0, Info),
	{
	map__init(Empty),
	implicitly_quantify_clause_body(HeadVars, HldsGoal1, VarSet2, Empty,
		HldsGoal, VarSet, _, _Warnings),
	NewClause = clause([ModeId], HldsGoal, Context),
	ClausesInfo =  clauses_info(VarSet, VarTypes, VarTypes1, HeadVars, 
		[NewClause|ClauseList])
	}.

:- pred allocate_vars_for_saved_vars(list(string), list(pair(prog_var, string)),
	prog_varset, prog_varset).
:- mode allocate_vars_for_saved_vars(in, out, in, out) is det.

allocate_vars_for_saved_vars([], [], VarSet, VarSet).
allocate_vars_for_saved_vars([Name | Names], [Var - Name | VarNames],
		VarSet0, VarSet) :-
	varset__new_var(VarSet0, Var, VarSet1),
	allocate_vars_for_saved_vars(Names, VarNames, VarSet1, VarSet).

%-----------------------------------------------------------------------------

:- pred transform(prog_substitution, list(prog_var), list(prog_term), goal,
		prog_varset, prog_context, hlds_goal, prog_varset, 
		list(quant_warning), qual_info, qual_info,
		io__state, io__state).
:- mode transform(in, in, in, in, in, in, out, out, out,
			in, out, di, uo) is det.

transform(Subst, HeadVars, Args0, Body, VarSet0, Context,
		Goal, VarSet, Warnings, Info0, Info) -->
	transform_goal(Body, VarSet0, Subst, Goal1, VarSet1, Info0, Info1),
	{ term__apply_substitution_to_list(Args0, Subst, Args) },
	insert_arg_unifications(HeadVars, Args, Context, head, no,
		Goal1, VarSet1, Goal2, VarSet2, Info1, Info),
	{ map__init(Empty) },
	{ implicitly_quantify_clause_body(HeadVars, Goal2, VarSet2, Empty,
				Goal, VarSet, _, Warnings) }.

%-----------------------------------------------------------------------------%

	% Convert goals from the prog_data `goal' structure into the
	% hlds `hlds_goal' structure.  At the same time, convert
	% it to super-homogeneous form by unravelling all the complex
	% unifications, and annotate those unifications with a unify_context
	% so that we can still give good error messages.
	% And also at the same time, apply the given substitution to
	% the goal, to rename it apart from the other clauses.

:- pred transform_goal(goal, prog_varset, prog_substitution, hlds_goal,
		prog_varset, qual_info, qual_info, io__state, io__state).
:- mode transform_goal(in, in, in, out, out, in, out, di, uo) is det.

transform_goal(Goal0 - Context, VarSet0, Subst, Goal1 - GoalInfo1, VarSet,
		Info0, Info) -->
	transform_goal_2(Goal0, Context, VarSet0, Subst, Goal1 - GoalInfo0,
					VarSet, Info0, Info),
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo1) }.

:- pred transform_goal_2(goal_expr, prog_context, prog_varset,
		prog_substitution, hlds_goal, prog_varset,
		qual_info, qual_info, io__state, io__state).
:- mode transform_goal_2(in, in, in, in, out, out, in, out, di, uo) is det.

transform_goal_2(fail, _, VarSet, _, disj([], Empty) - GoalInfo, VarSet,
		Info, Info) -->
	{ map__init(Empty) },
	{ goal_info_init(GoalInfo) }.

transform_goal_2(true, _, VarSet, _, conj([]) - GoalInfo, VarSet,
		Info, Info) -->
	{ goal_info_init(GoalInfo) }.

	% Convert `all [Vars] Goal' into `not some [Vars] not Goal'.
transform_goal_2(all(Vars0, Goal0), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	{ TransformedGoal = not(some(Vars0, not(Goal0) - Context) - Context) },
	transform_goal_2(TransformedGoal, Context, VarSet0, Subst,
						Goal, VarSet, Info0, Info).

transform_goal_2(some(Vars0, Goal0), _, VarSet0, Subst,
		some(Vars, Goal) - GoalInfo, VarSet, Info0, Info) -->
	{ substitute_vars(Vars0, Subst, Vars) },
	transform_goal(Goal0, VarSet0, Subst, Goal, VarSet, Info0, Info),
	{ goal_info_init(GoalInfo) }.


transform_goal_2(if_then_else(Vars0, A0, B0, C0), _, VarSet0, Subst,
	if_then_else(Vars, A, B, C, Empty) - GoalInfo, VarSet, Info0, Info)
		-->
	{ substitute_vars(Vars0, Subst, Vars) },
	transform_goal(A0, VarSet0, Subst, A, VarSet1, Info0, Info1),
	transform_goal(B0, VarSet1, Subst, B, VarSet2, Info1, Info2),
	transform_goal(C0, VarSet2, Subst, C, VarSet, Info2, Info),
	{ map__init(Empty) },
	{ goal_info_init(GoalInfo) }.

transform_goal_2(if_then(Vars0, A0, B0), Context, Subst, VarSet0,
		Goal, VarSet, Info0, Info) -->
	transform_goal_2(if_then_else(Vars0, A0, B0, true - Context),
			Context, Subst, VarSet0, Goal, VarSet, Info0, Info).

transform_goal_2(not(A0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	transform_goal(A0, VarSet0, Subst, A, VarSet, Info0, Info),
	{
		% eliminate double negations
		A = not(Goal1) - _
	->
		Goal = Goal1
	;
		% convert negated conjunctions of negations
		% into disjunctions
		A = conj(NegatedGoals) - _,
		all_negated(NegatedGoals, UnnegatedGoals)
	->
		goal_info_init(GoalInfo),
		map__init(StoreMap),
		Goal = disj(UnnegatedGoals, StoreMap) - GoalInfo
	;
		goal_info_init(GoalInfo),
		Goal = not(A) - GoalInfo
	}.

transform_goal_2((A0,B0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	get_conj(B0, Subst, [], VarSet0, L0, VarSet1, Info0, Info1),
	get_conj(A0, Subst, L0, VarSet1, L, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ conj_list_to_goal(L, GoalInfo, Goal) }.

transform_goal_2((A0 & B0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	get_par_conj(B0, Subst, [], VarSet0, L0, VarSet1, Info0, Info1),
	get_par_conj(A0, Subst, L0, VarSet1, L, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ par_conj_list_to_goal(L, GoalInfo, Goal) }.

transform_goal_2((A0;B0), _, VarSet0, Subst, Goal, VarSet, Info0, Info) -->
	get_disj(B0, Subst, [], VarSet0, L0, VarSet1, Info0, Info1),
	get_disj(A0, Subst, L0, VarSet1, L, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ disj_list_to_goal(L, GoalInfo, Goal) }.

transform_goal_2(implies(P, Q), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
		% `P => Q' is defined as `not (P, not Q)'
	{ TransformedGoal = not( (P, not(Q) - Context) - Context ) },
	transform_goal_2(TransformedGoal, Context, VarSet0, Subst,
		Goal, VarSet, Info0, Info).

transform_goal_2(equivalent(P, Q), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
		% `P <=> Q' is defined as `(P => Q), (Q => P)'
	{ TransformedGoal = (implies(P, Q) - Context,
				implies(Q, P) - Context) },
	transform_goal_2(TransformedGoal, Context, VarSet0, Subst,
		Goal, VarSet, Info0, Info).

transform_goal_2(call(Name, Args0, Purity), Context, VarSet0, Subst, Goal,
		VarSet, Info0, Info) -->
	( 
		{ Name = unqualified("\\=") },
		{ Args0 = [LHS, RHS] }
	->
			% `LHS \= RHS' is defined as `not (RHS = RHS)'
		transform_goal_2(not(unify(LHS, RHS) - Context), Context,
				VarSet0, Subst, Goal, VarSet, Info0, Info)
	;
		{ term__apply_substitution_to_list(Args0, Subst, Args) },
		{ make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1) },
		(
			% check for a higher-order call,
			% i.e. a call to either call/N or ''/N.
			{ Name = unqualified("call")
			; Name = unqualified("")
			},
			{ HeadVars = [PredVar | RealHeadVars] }
		->
			{ % initialize some fields to junk
			  Types = [],
			  Modes = [],
			  Det = erroneous,
			  inst_table_init(InstTable),
			  ArgModes = argument_modes(InstTable, Modes),
			  Call = higher_order_call(PredVar, RealHeadVars,
						   Types, ArgModes, Det,
						   predicate),
			  Purity1 = pure
			},
			(
				{ Purity = pure }
			->
				[]
			;
				prog_out__write_context(Context),
				io__write_string("Warning: unnecessary `"),
				write_purity(Purity),
				io__write_string("' marker.\n"),
				prog_out__write_context(Context),
				io__write_string("  Higher-order goals are always pure.\n")
			)
		;
			% initialize some fields to junk
			{ invalid_pred_id(PredId),
			  invalid_proc_id(ModeId),
			  MaybeUnifyContext = no,
			  Call = call(PredId, ModeId, HeadVars, not_builtin,
				      MaybeUnifyContext, Name),
			  Purity1 = Purity
			}
		),
		{ goal_info_init(GoalInfo0) },
		{ goal_info_set_context(GoalInfo0, Context, GoalInfo1) },
		{ add_goal_info_purity_feature(GoalInfo1, Purity1, GoalInfo) },
		{ Goal0 = Call - GoalInfo },

		{ list__length(Args, Arity) },
		{ PredCallId = Name/Arity },
		insert_arg_unifications(HeadVars, Args,
			Context, call(PredCallId), no,
			Goal0, VarSet1, Goal, VarSet, Info0, Info)
	).

transform_goal_2(unify(A0, B0), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	{ term__apply_substitution(A0, Subst, A) },
	{ term__apply_substitution(B0, Subst, B) },
	unravel_unification(A, B, Context, explicit, [],
			VarSet0, Goal, VarSet, Info0, Info).

:- pred all_negated(list(hlds_goal), list(hlds_goal)).
:- mode all_negated(in, out) is semidet.

all_negated([], []).
all_negated([not(Goal) - _ | NegatedGoals], [Goal | Goals]) :-
	all_negated(NegatedGoals, Goals).
% nested conjunctions shouldn't occur here anyway, but just in case...
all_negated([conj(NegatedConj) - _GoalInfo | NegatedGoals], Goals) :-
	all_negated(NegatedConj, Goals1),
	all_negated(NegatedGoals, Goals2),
	list__append(Goals1, Goals2, Goals).

%-----------------------------------------------------------------------------

	% `insert_arg_unifications' takes a list of variables,
	% a list of terms to unify them with, and a goal, and
	% inserts the appropriate unifications onto the front of
	% the goal.  It calls `unravel_unification' to ensure
	% that each unification gets reduced to superhomogeneous form.
	% It also gets passed a `arg_context', which indicates
	% where the terms came from.

	% We never insert unifications of the form X = X.
	% If ForPragmaC is yes, we process unifications of the form
	% X = Y by substituting the var expected by the outside environment
	% (the head variable) for the variable inside the goal (which was
	% created just for the pragma_c_code goal), while giving the headvar
	% the name of the just eliminated variable. The result will be
	% a proc_info in which the head variables have meaningful names
	% and the body goal is just a pragma C code. Without this special
	% treatment, the body goal will be a conjunction, which would
	% complicate the handling of code generation for nondet pragma C codes.

:- type arg_context
	--->	head		% the arguments in the head of the clause
	;	call(pred_call_id) % the arguments in a call to a predicate
	;	functor(	% the arguments in a functor
			cons_id,
			unify_main_context,
			unify_sub_contexts
		).

:- pred insert_arg_unifications(list(prog_var), list(prog_term),
		prog_context, arg_context, bool, hlds_goal, prog_varset,
		hlds_goal, prog_varset, qual_info, qual_info,
		io__state, io__state).
:- mode insert_arg_unifications(in, in, in, in, in, in, in, out,
		out, in, out, di, uo) is det.

insert_arg_unifications(HeadVars, Args, Context, ArgContext, ForPragmaC,
		Goal0, VarSet0, Goal, VarSet, Info0, Info) -->
	( { HeadVars = [] } ->
		{ Goal = Goal0 },
		{ VarSet = VarSet0 },
		{ Info = Info0 }
	;
		{ Goal0 = _ - GoalInfo },
		{ goal_to_conj_list(Goal0, List0) },
		insert_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			ForPragmaC, 0, List0, VarSet0, List, VarSet,
			Info0, Info),
		{ conj_list_to_goal(List, GoalInfo, Goal) }
	).

:- pred insert_arg_unifications_2(list(prog_var), list(prog_term),
		prog_context, arg_context, bool, int, list(hlds_goal),
		prog_varset, list(hlds_goal), prog_varset,
		qual_info, qual_info, io__state, io__state).
:- mode insert_arg_unifications_2(in, in, in, in, in, in, in, in,
		out, out, in, out, di, uo) is det.

insert_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, _, _, _) -->
	{ error("insert_arg_unifications_2: length mismatch") }.
insert_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, _, _, _) -->
	{ error("insert_arg_unifications_2: length mismatch") }.
insert_arg_unifications_2([], [], _, _, _, _, List, VarSet, List, VarSet,
		Info, Info) --> [].
insert_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext,
		ForPragmaC, N0, List0, VarSet0, List, VarSet, Info0, Info) -->
	{ N1 is N0 + 1 },
	(
		{ Arg = term__variable(Var) }
	->
		% Skip unifications of the form `X = X'
		insert_arg_unifications_2(Vars, Args, Context,
			ArgContext, ForPragmaC, N1, List0, VarSet0, List,
			VarSet, Info0, Info)
	;
		{ Arg = term__variable(ArgVar) },
		{ ForPragmaC = yes }
	->
		% Handle unifications of the form `X = Y' by substitution
		% if this is safe.
		{ map__init(Subst0) },
		{ map__det_insert(Subst0, ArgVar, Var, Subst) },
		{ goal_util__rename_vars_in_goals(List0, no, Subst,
			List1) },
		{ varset__search_name(VarSet0, ArgVar, ArgVarName) ->
			varset__name_var(VarSet0, Var, ArgVarName, VarSet1)
		;
			VarSet1 = VarSet0
		},
		insert_arg_unifications_2(Vars, Args, Context, ArgContext,
			ForPragmaC, N1, List1, VarSet1, List, VarSet,
			Info0, Info)
	;
		{ arg_context_to_unify_context(ArgContext, N1,
			UnifyMainContext, UnifySubContext) },
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			VarSet0, Goal, VarSet1, Info0, Info1),
		{ goal_to_conj_list(Goal, ConjList) },
		{ list__append(ConjList, List1, List) },
		insert_arg_unifications_2(Vars, Args, Context, ArgContext,
			ForPragmaC, N1, List0, VarSet1, List1, VarSet,
			Info1, Info)
	).

	% append_arg_unifications is the same as insert_arg_unifications,
	% except that the unifications are added after the goal rather
	% than before the goal.

:- pred append_arg_unifications(list(prog_var), list(prog_term),
		prog_context, arg_context, hlds_goal, prog_varset, hlds_goal,
		prog_varset, qual_info, qual_info, io__state, io__state).
:- mode append_arg_unifications(in, in, in, in, in, in,
		out, out, in, out, di, uo) is det.

append_arg_unifications(HeadVars, Args, Context, ArgContext, Goal0, VarSet0,
			Goal, VarSet, Info0, Info) -->
	( { HeadVars = [] } ->
		{ Goal = Goal0 },
		{ VarSet = VarSet0 },
		{ Info = Info0 }
	;
		{ Goal0 = _ - GoalInfo },
		{ goal_to_conj_list(Goal0, List0) },
		append_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			0, List0, VarSet0, List, VarSet, Info0, Info),
		{ conj_list_to_goal(List, GoalInfo, Goal) }
	).

:- pred append_arg_unifications_2(list(prog_var), list(prog_term),
	prog_context, arg_context, int, list(hlds_goal), prog_varset,
	list(hlds_goal), prog_varset, qual_info, qual_info,
	io__state, io__state).
:- mode append_arg_unifications_2(in, in, in, in, in, in, in,
	out, out, in, out, di, uo) is det.

append_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, _, _) -->
	{ error("append_arg_unifications_2: length mismatch") }.
append_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, _, _) -->
	{ error("append_arg_unifications_2: length mismatch") }.
append_arg_unifications_2([], [], _, _, _, List, VarSet, List, VarSet,
			Info, Info) --> [].
append_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext, N0,
			List0, VarSet0, List, VarSet, Info0, Info) -->
	{ N1 is N0 + 1 },
		% skip unifications of the form `X = X'
	( { Arg = term__variable(Var) } ->
		append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
				List0, VarSet0, List, VarSet, Info0, Info)
	;
		{ arg_context_to_unify_context(ArgContext, N1,
				UnifyMainContext, UnifySubContext) },
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			VarSet0, Goal, VarSet1, Info0, Info1),
		{ goal_to_conj_list(Goal, ConjList) },
		{ list__append(List0, ConjList, List1) },
		append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
				List1, VarSet1, List, VarSet, Info1, Info)
	).

:- pred arg_context_to_unify_context(arg_context, int,
				unify_main_context, unify_sub_contexts).
:- mode arg_context_to_unify_context(in, in, out, out) is det.

arg_context_to_unify_context(head, N, head(N), []).
arg_context_to_unify_context(call(PredId), N, call(PredId, N), []).
arg_context_to_unify_context(functor(ConsId, MainContext, SubContexts), N,
			MainContext, [ConsId - N | SubContexts]).

%-----------------------------------------------------------------------------%

	% make_fresh_arg_vars(Args, VarSet0, Vars, VarSet):
	%	`Vars' is a list of distinct variables corresponding to
	%	the terms in `Args'.  For each term in `Args', if
	%	the term is a variable V which is distinct from the
	%	variables already produced, then the corresponding
	%	variable in `Vars' is just V, otherwise a fresh variable
	%	is allocated from `VarSet0'.   `VarSet' is the resulting
	%	varset after all the necessary variables have been allocated.
	%
	%	For efficiency, the list `Vars' is constructed backwards
	%	and then reversed to get the correct order.

:- pred make_fresh_arg_vars(list(prog_term), prog_varset, list(prog_var),
		prog_varset).
:- mode make_fresh_arg_vars(in, in, out, out) is det.

make_fresh_arg_vars(Args, VarSet0, Vars, VarSet) :-
	make_fresh_arg_vars_2(Args, [], VarSet0, Vars1, VarSet),
	list__reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(prog_term), list(prog_var), prog_varset,
		list(prog_var), prog_varset).
:- mode make_fresh_arg_vars_2(in, in, in, out, out) is det.

make_fresh_arg_vars_2([], Vars, VarSet, Vars, VarSet).
make_fresh_arg_vars_2([Arg | Args], Vars0, VarSet0, Vars, VarSet) :-
	( Arg = term__variable(ArgVar), \+ list__member(ArgVar, Vars0) ->
		Var = ArgVar,
		VarSet1 = VarSet0
	;
		varset__new_var(VarSet0, Var, VarSet1)
	),
	make_fresh_arg_vars_2(Args, [Var | Vars0], VarSet1, Vars, VarSet).

%-----------------------------------------------------------------------------%

:- pred unravel_unification(prog_term, prog_term, prog_context,
		unify_main_context, unify_sub_contexts, prog_varset, hlds_goal,
		prog_varset, qual_info, qual_info, io__state, io__state).
:- mode unravel_unification(in, in, in, in, in, in, out, out,
		in, out, di, uo) is det.

	% `X = Y' needs no unravelling.

unravel_unification(term__variable(X), term__variable(Y), Context,
	MainContext, SubContext, VarSet0, Goal, VarSet, Info, Info)
		-->
	{ create_atomic_unification(X, var(Y), Context, MainContext,
		SubContext, Goal) },
	{ VarSet0 = VarSet }.

	% If we find a unification of the form
	%	X = f(A1, A2, A3)
	% we replace it with
	%	X = f(NewVar1, NewVar2, NewVar3),
	%	NewVar1 = A1,
	%	NewVar2 = A2,
	%	NewVar3 = A3.
	% In the trivial case `X = c', no unravelling occurs.

unravel_unification(term__variable(X), RHS,
			Context, MainContext, SubContext, VarSet0,
			Goal, VarSet, Info0, Info) -->
	{ RHS = term__functor(F, Args, FunctorContext) },
	(
		% Handle explicit type qualification.
		{ semidet_fail },
		{ F = term__atom("TYPE_QUAL_OP") },
		{ Args = [RVal, DeclType0] }
	->
		{ term__coerce(DeclType0, DeclType) },
		{ varset__coerce(VarSet0, DeclVarSet) },
		process_type_qualification(X, DeclType, DeclVarSet,
			Context, Info0, Info1),
		unravel_unification(term__variable(X), RVal,
			Context, MainContext, SubContext, VarSet0,
			Goal, VarSet, Info1, Info)
	;	
	    {
		% handle lambda expressions
		F = term__atom("lambda"),
		Args = [LambdaExpressionTerm0, GoalTerm0],
		term__coerce(LambdaExpressionTerm0, LambdaExpressionTerm),
		parse_lambda_expression(LambdaExpressionTerm,
			Vars0, Modes0, Det0)
	    ->
		Vars1 = Vars0, Modes1 = Modes0, Det1 = Det0,
		GoalTerm1 = GoalTerm0
	    ;
		% handle higher-order pred expressions -
		% same semantics as lambda expressions, different syntax
		% (the original lambda expression syntax is now deprecated)
		F = term__atom(":-"),
		Args = [PredTerm0, GoalTerm0],
		term__coerce(PredTerm0, PredTerm),
		parse_pred_expression(PredTerm, Vars0, Modes0, Det0)
	    ->
		Vars1 = Vars0, Modes1 = Modes0, Det1 = Det0,
		GoalTerm1 = GoalTerm0
	    ;
	    	FuncTerm0 = term__functor(F, Args, FunctorContext),
		term__coerce(FuncTerm0, FuncTerm),
		parse_pred_expression(FuncTerm, Vars1, Modes1, Det1),
		GoalTerm1 = term__functor(term__atom("true"), [], Context)
	    }
	->
		{ qual_info_get_mq_info(Info0, MQInfo0) },
		module_qual__qualify_lambda_mode_list(Modes1, Modes, Context,
						MQInfo0, MQInfo1),
		{ qual_info_set_mq_info(Info0, MQInfo1, Info1) },
		{ Det = Det1 },
		{ make_fresh_arg_vars(Vars1, VarSet0, Vars, VarSet1) },
		{ term__coerce(GoalTerm1, GoalTerm) },
		{ parse_goal(GoalTerm, VarSet1, ParsedGoal, VarSet2) },
		{ map__init(Substitution) },
		transform_goal(ParsedGoal, VarSet2, Substitution,
				HLDS_Goal0, VarSet3, Info1, Info2),
		insert_arg_unifications(Vars, Vars1, Context, head, no,
			HLDS_Goal0, VarSet3, HLDS_Goal, VarSet, Info2, Info),
		{ instmap_delta_init_reachable(InstMapDelta) },
		{ inst_table_init(InstTable) },

			 % quantification will reduce this down to
			 % the proper set of nonlocal arguments.
		{ goal_util__goal_vars(HLDS_Goal, LambdaGoalVars0) }, 
		{ set__delete_list(LambdaGoalVars0, Vars, LambdaGoalVars1) },
		{ set__to_sorted_list(LambdaGoalVars1, LambdaNonLocals) },

		{ create_atomic_unification(X,
			lambda_goal(predicate, LambdaNonLocals, Vars, 
				argument_modes(InstTable, Modes), Det,
				InstMapDelta, HLDS_Goal),
			Context, MainContext, SubContext, Goal) }
	;
	    {
		% handle higher-order dcg pred expressions -
		% same semantics as higher-order pred expressions,
		% but has two extra arguments, and the goal is expanded
		% as a DCG goal.
		F = term__atom("-->"),
		Args = [PredTerm0, GoalTerm0],
		term__coerce(PredTerm0, PredTerm),
		parse_dcg_pred_expression(PredTerm, Vars0, Modes0, Det)
	    }
	->
		{ qual_info_get_mq_info(Info0, MQInfo0) },
		module_qual__qualify_lambda_mode_list(Modes0, Modes, Context,
			MQInfo0, MQInfo1),
		{ qual_info_set_mq_info(Info0, MQInfo1, Info1) },
		{ term__coerce(GoalTerm0, GoalTerm) },
		{ parse_dcg_pred_goal(GoalTerm, VarSet0,
			ParsedGoal, DCG0, DCGn, VarSet1) },
		{ list__append(Vars0, [term__variable(DCG0),
				term__variable(DCGn)], Vars1) },
		{ make_fresh_arg_vars(Vars1, VarSet1, Vars, VarSet2) },
		{ map__init(Substitution) },
		transform_goal(ParsedGoal, VarSet2, Substitution,
			HLDS_Goal0, VarSet3, Info1, Info2),
		insert_arg_unifications(Vars, Vars1, Context, head, no,
			HLDS_Goal0, VarSet3, HLDS_Goal, VarSet, Info2, Info),
		{ instmap_delta_init_reachable(InstMapDelta) },
		{ inst_table_init(InstTable) },

			 % quantification will reduce this down to
			 % the proper set of nonlocal arguments.
		{ goal_util__goal_vars(HLDS_Goal, LambdaGoalVars0) }, 
		{ set__delete_list(LambdaGoalVars0, Vars, LambdaGoalVars1) },
		{ set__to_sorted_list(LambdaGoalVars1, LambdaNonLocals) },

		{ create_atomic_unification(X,
			lambda_goal(predicate, LambdaNonLocals, Vars, 
				argument_modes(InstTable, Modes), Det,
				InstMapDelta, HLDS_Goal),
			Context, MainContext, SubContext, Goal) }
	;
	    {
		% handle higher-order func expressions -
		% like higher-order pred expressions, but for functions
		F = term__atom(":-"),
		Args = [FuncTerm0, GoalTerm0],
		term__coerce(FuncTerm0, FuncTerm),
		parse_func_expression(FuncTerm, Vars0, Modes0, Det0)
	    ->
		Vars1 = Vars0, Modes1 = Modes0, Det1 = Det0,
		GoalTerm1 = GoalTerm0
	    ;
	    	FuncTerm0 = term__functor(F, Args, FunctorContext),
		term__coerce(FuncTerm0, FuncTerm),
		parse_func_expression(FuncTerm, Vars1, Modes1, Det1),
		GoalTerm1 = term__functor(term__atom("true"), [], Context)
	    }
	->
		{ qual_info_get_mq_info(Info0, MQInfo0) },
		module_qual__qualify_lambda_mode_list(Modes1, Modes, Context,
						MQInfo0, MQInfo1),
		{ qual_info_set_mq_info(Info0, MQInfo1, Info1) },
		{ Det = Det1 },
		{ make_fresh_arg_vars(Vars1, VarSet0, Vars, VarSet1) },
		{ term__coerce(GoalTerm1, GoalTerm) },
		{ parse_goal(GoalTerm, VarSet1, ParsedGoal, VarSet2) },
		{ map__init(Substitution) },
		transform_goal(ParsedGoal, VarSet2, Substitution,
				HLDS_Goal0, VarSet3, Info1, Info2),
		insert_arg_unifications(Vars, Vars1, Context, head, no,
			HLDS_Goal0, VarSet3, HLDS_Goal, VarSet, Info2, Info),
		{ instmap_delta_init_reachable(InstMapDelta) },
		{ inst_table_init(InstTable) },
			 % quantification will reduce this down to
			 % the proper set of nonlocal arguments.
		{ goal_util__goal_vars(HLDS_Goal, LambdaGoalVars0) }, 
		{ set__delete_list(LambdaGoalVars0, Vars, LambdaGoalVars1) },
		{ set__to_sorted_list(LambdaGoalVars1, LambdaNonLocals) },

		{ create_atomic_unification(X,
			lambda_goal(function, LambdaNonLocals, Vars, 
				argument_modes(InstTable, Modes), Det,
				InstMapDelta, HLDS_Goal),
			Context, MainContext, SubContext, Goal) }
	;
		% handle if-then-else expressions
		{   F = term__atom("else"),
		    Args = [term__functor(term__atom("if"), [
				term__functor(term__atom("then"),
					[IfTerm0, ThenTerm], _)
				], _),
			    ElseTerm]
		;   F = term__atom(";"),
		    Args = [term__functor(term__atom("->"),
				[IfTerm0, ThenTerm], _),
			    ElseTerm]
		},
		{ term__coerce(IfTerm0, IfTerm) },
		{ parse_some_vars_goal(IfTerm, VarSet0, Vars,
			IfParseTree, VarSet11) }
	->
		{ map__init(Subst) },
		transform_goal(IfParseTree, VarSet11, Subst, IfGoal, VarSet22,
			Info0, Info1),
		unravel_unification(term__variable(X), ThenTerm,
			Context, MainContext, SubContext, VarSet22, ThenGoal,
			VarSet33, Info1, Info2),
		unravel_unification(term__variable(X), ElseTerm,
			Context, MainContext, SubContext, VarSet33, ElseGoal,
			VarSet, Info2, Info),
		{ map__init(Empty) },
		{ IfThenElse = if_then_else(Vars, IfGoal, ThenGoal, ElseGoal,
			Empty) },
		{ goal_info_init(GoalInfo0) },
		{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
		{ Goal = IfThenElse - GoalInfo }
	;
		{ parse_qualified_term(RHS, RHS, "", MaybeFunctor) },
		(
			{ MaybeFunctor = ok(FunctorName, FunctorArgs) },
			{ list__length(FunctorArgs, Arity) },
			{ ConsId = cons(FunctorName, Arity) }
		;
			% float, int or string constant
			% 	- any errors will be caught by typechecking
			{ MaybeFunctor = error(_, _) },
			{ list__length(Args, Arity) },
			{ make_functor_cons_id(F, Arity, ConsId) },
			{ FunctorArgs = Args }
		),
		( { FunctorArgs = [] } ->
			{ create_atomic_unification(X, functor(ConsId, []),
				Context, MainContext, SubContext, Goal) },
			{ VarSet = VarSet0 },
			{ Info = Info0 }
		;
			{ make_fresh_arg_vars(FunctorArgs, VarSet0,
				HeadVars, VarSet1) },
			{ create_atomic_unification(X,
				functor(ConsId, HeadVars), Context,
				MainContext, SubContext, Goal0) },
			{ ArgContext = functor(ConsId,
				MainContext, SubContext) },
			% Should this be insert_... rather than append_...?
			% No, because that causes efficiency problems
			% with type-checking :-(
			append_arg_unifications(HeadVars, FunctorArgs,
				FunctorContext, ArgContext, Goal0,
				VarSet1, Goal, VarSet, Info0, Info)
		)
	).

	% Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification(term__functor(F, As, FC), term__variable(Y),
		C, MC, SC, VarSet0, Goal, VarSet, Info0, Info) -->
	unravel_unification(term__variable(Y),
		term__functor(F, As, FC),
		C, MC, SC, VarSet0, Goal, VarSet, Info0, Info).

	% If we find a unification of the form `f1(...) = f2(...)',
	% then we replace it with `Tmp = f1(...), Tmp = f2(...)',
	% and then process it according to the rule above.
	% Note that we can't simplify it yet, because we might simplify
	% away type errors.

unravel_unification(term__functor(LeftF, LeftAs, LeftC),
			term__functor(RightF, RightAs, RightC),
			Context, MainContext, SubContext, VarSet0,
			Goal, VarSet, Info0, Info) -->
	{ varset__new_var(VarSet0, TmpVar, VarSet1) },
	unravel_unification(
		term__variable(TmpVar),
		term__functor(LeftF, LeftAs, LeftC),
		Context, MainContext, SubContext,
		VarSet1, Goal0, VarSet2, Info0, Info1),
	unravel_unification(
		term__variable(TmpVar),
		term__functor(RightF, RightAs, RightC),
		Context, MainContext, SubContext,
		VarSet2, Goal1, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ goal_to_conj_list(Goal0, ConjList0) },
	{ goal_to_conj_list(Goal1, ConjList1) },
	{ list__append(ConjList0, ConjList1, ConjList) },
	{ conj_list_to_goal(ConjList, GoalInfo, Goal) }.

	% create the hlds_goal for a unification which cannot be
	% further simplified, filling in all the as yet
	% unknown slots with dummy values

create_atomic_unification(A, B, Context, UnifyMainContext, UnifySubContext,
		Goal) :-
	UMode = ((free(unique) - free(unique)) -> 
		(free(unique) - free(unique))),
	Mode = ((free(unique) - free(unique)) - 
		(free(unique) - free(unique))),
	UnifyInfo = complicated_unify(UMode, can_fail),
	UnifyC = unify_context(UnifyMainContext, UnifySubContext),
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo),
	Goal = unify(A, B, Mode, UnifyInfo, UnifyC) - GoalInfo.

%-----------------------------------------------------------------------------%

	% Process an explicit type qualification.
:- pred process_type_qualification(prog_var, type, tvarset, prog_context,
		qual_info, qual_info, io__state, io__state).
:- mode process_type_qualification(in, in, in, in, in, out, di, uo) is det.

process_type_qualification(Var, Type0, VarSet, Context, Info0, Info) -->
	{ Info0 = qual_info(EqvMap, TVarSet0, TVarRenaming0, Index0,
				VarTypes0, PredId, MQInfo0) },

	module_qual__qualify_type_qualification(Type0, Type1, 
		Context, MQInfo0, MQInfo),
	{
	% Find any new type variables introduced by this type, and
	% add them to the var-name index and the variable renaming.
	term__vars(Type1, TVars),
	get_new_tvars(TVars, VarSet, TVarSet0, TVarSet1,
		Index0, Index, TVarRenaming0, TVarRenaming),
			
	% Apply the updated renaming to convert type variables in
	% the clause to type variables in the tvarset.
	term__apply_variable_renaming(Type1, TVarRenaming, Type2),

	% Expand equivalence types.
	equiv_type__replace_in_type(Type2, TVarSet1, EqvMap, Type, TVarSet)
	},
	update_var_types(VarTypes0, Var, Type, Context, VarTypes),	
	{ Info = qual_info(EqvMap, TVarSet, TVarRenaming,
			Index, VarTypes, PredId, MQInfo) }.

:- pred update_var_types(map(prog_var, type), prog_var, type, prog_context,
			map(prog_var, type), io__state, io__state).
:- mode update_var_types(in, in, in, in, out, di, uo) is det.

update_var_types(VarTypes0, Var, Type, Context, VarTypes) -->
	( { map__search(VarTypes0, Var, Type0) } ->
		( { Type = Type0 } ->
			{ VarTypes = VarTypes0 }
		;
			prog_out__write_context(Context),
			io__write_string("Error: explicit type qualification does\n"),
			prog_out__write_context(Context),
			io__write_string("  not match prior qualification.\n"),
			io__set_exit_status(1),
			{ VarTypes = VarTypes0 }
		)
	;
		{ map__det_insert(VarTypes0, Var, Type, VarTypes) }
	).

	% Add new type variables for those introduced by a type qualification.
:- pred get_new_tvars(list(tvar), tvarset, tvarset, tvarset,
	map(string, tvar), map(string, tvar), map(tvar, tvar), map(tvar, tvar)).
:- mode get_new_tvars(in, in, in, out, in, out, in, out) is det.

get_new_tvars([], _, T, T, I, I, R, R).
get_new_tvars([TVar | TVars], VarSet, TVarSet0, TVarSet,
		Index0, Index, TVarRenaming0, TVarRenaming) :-
	( map__contains(TVarRenaming0, TVar) ->
		TVarRenaming1 = TVarRenaming0,
		TVarSet2 = TVarSet0,
		Index1 = Index0
	;
		varset__lookup_name(VarSet, TVar, TVarName),
		( map__search(Index0, TVarName, TVarSetVar) ->
			map__det_insert(TVarRenaming0, TVar, TVarSetVar,
						TVarRenaming1),
			TVarSet2 = TVarSet0,
			Index1 = Index0
		;
			varset__new_var(TVarSet0, NewTVar, TVarSet1),
			varset__name_var(TVarSet1, NewTVar,
					TVarName, TVarSet2),
			map__det_insert(Index0, TVarName, NewTVar, Index1),
			map__det_insert(TVarRenaming0, TVar, NewTVar,
					TVarRenaming1)
		)
	),
	get_new_tvars(TVars, VarSet, TVarSet2, TVarSet,
		 Index1, Index, TVarRenaming1, TVarRenaming).
			
%-----------------------------------------------------------------------------%

% substitute_vars(Vars0, Subst, Vars)
%	apply substitiution `Subst' (which must only rename vars) to `Vars0',
%	and return the result in `Vars'.

:- pred substitute_vars(list(var(T)), substitution(T), list(var(T))).
:- mode substitute_vars(in, in, out) is det.

substitute_vars([], _, []).
substitute_vars([Var0 | Vars0], Subst, [Var | Vars]) :-
	term__apply_substitution(term__variable(Var0), Subst, Term),
	( Term = term__variable(Var1) ->
		Var = Var1
	;
		error("substitute_vars: invalid substitution")
	),
	substitute_vars(Vars0, Subst, Vars).

%-----------------------------------------------------------------------------%

% get_conj(Goal, Conj0, Subst, Conj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list (applying Subst),
%	append Conj0, and return the result in Conj.

:- pred get_conj(goal, prog_substitution, list(hlds_goal), prog_varset,
	list(hlds_goal), prog_varset, qual_info, qual_info,
	io__state, io__state).
:- mode get_conj(in, in, in, in, out, out, in, out, di, uo) is det.

get_conj(Goal, Subst, Conj0, VarSet0, Conj, VarSet, Info0, Info) -->
	(
		{ Goal = (A,B) - _Context }
	->
		get_conj(B, Subst, Conj0, VarSet0, Conj1, VarSet1,
						Info0, Info1),
		get_conj(A, Subst, Conj1, VarSet1, Conj, VarSet, Info1, Info)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet,
						Info0, Info),
		{ goal_to_conj_list(Goal1, ConjList) },
		{ list__append(ConjList, Conj0, Conj) }
	).

% get_par_conj(Goal, ParConj0, Subst, ParConj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list (applying Subst),
%	append ParConj0, and return the result in ParConj.

:- pred get_par_conj(goal, prog_substitution, list(hlds_goal), prog_varset,
		list(hlds_goal), prog_varset, qual_info, qual_info,
		io__state, io__state).
:- mode get_par_conj(in, in, in, in, out, out, in, out, di, uo) is det.

get_par_conj(Goal, Subst, ParConj0, VarSet0, ParConj, VarSet, Info0, Info) -->
	(
		{ Goal = (A & B) - _Context }
	->
		get_par_conj(B, Subst, ParConj0, VarSet0, ParConj1, VarSet1,
						Info0, Info1),
		get_par_conj(A, Subst, ParConj1, VarSet1, ParConj, VarSet,
						Info1, Info)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet,
						Info0, Info),
		{ goal_to_par_conj_list(Goal1, ParConjList) },
		{ list__append(ParConjList, ParConj0, ParConj) }
	).

% get_disj(Goal, Subst, Disj0, Disj) :
% 	Goal is a tree of disjuncts.  Flatten it into a list (applying Subst)
%	append Disj0, and return the result in Disj.

:- pred get_disj(goal, prog_substitution, list(hlds_goal), prog_varset,
		list(hlds_goal), prog_varset, qual_info, qual_info,
		io__state, io__state).
:- mode get_disj(in, in, in, in, out, out, in, out, di, uo) is det.

get_disj(Goal, Subst, Disj0, VarSet0, Disj, VarSet, Info0, Info) -->
	(
		{ Goal = (A;B) - _Context }
	->
		get_disj(B, Subst, Disj0, VarSet0, Disj1, VarSet1,
							Info0, Info1),
		get_disj(A, Subst, Disj1, VarSet1, Disj, VarSet, Info1, Info)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet,
							Info0, Info),
		{ Disj = [Goal1 | Disj0] }
	).

%-----------------------------------------------------------------------------%

	% Information used to process explicit type qualifications.
:- type qual_info
	--->	qual_info(
			eqv_map,	% Used to expand equivalence types. 
			tvarset,	% All type variables for predicate.
			map(tvar, tvar),
					% Map from clause type variable to
					% actual type variable in tvarset.
			map(string, tvar),
				% Type variables in tvarset indexed by name.
			map(prog_var, type), % Var types
			pred_id,	% Last pred processed.
			mq_info		% Module qualification info.
		).

:- pred init_qual_info(mq_info, eqv_map, qual_info).
:- mode init_qual_info(in, in, out) is det.

init_qual_info(MQInfo0, EqvMap, QualInfo) :-
	mq_info_set_need_qual_flag(MQInfo0, may_be_unqualified, MQInfo),
	varset__init(TVarSet),
	map__init(Renaming),
	map__init(Index),
	map__init(VarTypes),
	invalid_pred_id(PredId),
	QualInfo = qual_info(EqvMap, TVarSet, Renaming,
			Index, VarTypes, PredId, MQInfo).

	% Update the qual_info when processing a new clause.
:- pred update_qual_info(qual_info, tvarset, map(prog_var, type),
				pred_id, qual_info).
:- mode update_qual_info(in, in, in, in, out) is det.

update_qual_info(QualInfo0, TVarSet, VarTypes, PredId, QualInfo) :-
	QualInfo0 = qual_info(EqvMap, TVarSet0, _Renaming0, Index0,
					VarTypes0, PredId0, MQInfo),
	( PredId = PredId0 ->
		% The renaming for one clause is useless in the others.
		map__init(Renaming),
		QualInfo = qual_info(EqvMap, TVarSet0, Renaming,
				Index0, VarTypes0, PredId0, MQInfo)
	;
		varset__create_name_var_map(TVarSet, Index),
		map__init(Renaming),
		QualInfo = qual_info(EqvMap, TVarSet, Renaming,
				Index, VarTypes, PredId, MQInfo)
	).

	% All the other items are needed all at once in one or two places,
	% so access predicates for them would be a waste of time.

:- pred qual_info_get_mq_info(qual_info, mq_info).
:- mode qual_info_get_mq_info(in, out) is det.

qual_info_get_mq_info(qual_info(_,_,_,_,_,_,MQInfo), MQInfo).

:- pred qual_info_set_mq_info(qual_info, mq_info, qual_info).
:- mode qual_info_set_mq_info(in, in, out) is det.

qual_info_set_mq_info(qual_info(A,B,C,D,E,F,_), MQInfo,
			qual_info(A,B,C,D,E,F, MQInfo)).

:- pred qual_info_get_var_types(qual_info, map(prog_var, type)).
:- mode qual_info_get_var_types(in, out) is det.

qual_info_get_var_types(qual_info(_,_,_,_,VarTypes,_,_), VarTypes).

%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred multiple_def_error(sym_name, int, string, prog_context, prog_context,
				io__state, io__state).
:- mode multiple_def_error(in, in, in, in, in, di, uo) is det.

multiple_def_error(Name, Arity, DefType, Context, OrigContext) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' multiply defined.\n"),
	prog_out__write_context(OrigContext),
	io__write_string(
		"  Here is the previous definition of "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n").

:- pred undefined_pred_or_func_error(sym_name, int, prog_context, string,
				io__state, io__state).
:- mode undefined_pred_or_func_error(in, in, in, in, di, uo) is det.

undefined_pred_or_func_error(Name, Arity, Context, Description) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for "),
	hlds_out__write_pred_call_id(Name/Arity),
	io__write_string("\n"),
	prog_out__write_context(Context),
	% This used to say `preceding' instead of `corresponding.'
	% Which is more correct?
	io__write_string("  without corresponding `pred' or `func' declaration.\n").

:- pred undefined_mode_error(sym_name, int, prog_context, string,
				io__state, io__state).
:- mode undefined_mode_error(in, in, in, in, di, uo) is det.

undefined_mode_error(Name, Arity, Context, Description) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	hlds_out__write_pred_call_id(Name/Arity),
	io__write_string("' specifies non-existent mode.\n").

:- pred maybe_undefined_pred_error(sym_name, int, pred_or_func, prog_context,
				string, io__state, io__state).
:- mode maybe_undefined_pred_error(in, in, in, in, in, di, uo) is det.

% This is not considered an unconditional error anymore:
% if there is no :- pred declaration, we just infer one,
% unless the `--no-infer-types' option was specified.

maybe_undefined_pred_error(Name, Arity, PredOrFunc, Context, Description) -->
	globals__io_lookup_bool_option(infer_types, InferTypes),
	( { InferTypes = yes } ->
		[]
	;
		io__set_exit_status(1),
		prog_out__write_context(Context),
		io__write_string("Error: "),
		io__write_string(Description),
		io__write_string(" for "),
		hlds_out__write_call_id(PredOrFunc, Name/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  without preceding `"),
		{ hlds_out__pred_or_func_to_str(PredOrFunc, DeclString) },
		io__write_string(DeclString),
		io__write_string("' declaration.\n")
	).

:- pred undefined_type_class_error(sym_name, int, prog_context, string,
				io__state, io__state).
:- mode undefined_type_class_error(in, in, in, in, di, uo) is det.

undefined_type_class_error(ClassName, Arity, Context, Description) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	hlds_out__write_pred_call_id(ClassName/Arity),
	io__write_string("' without preceding typeclass declaration.\n").

:- pred unspecified_det_for_local(sym_name, arity, pred_or_func, prog_context, 
				io__state, io__state).
:- mode unspecified_det_for_local(in, in, in, in, di, uo) is det.

unspecified_det_for_local(Name, Arity, PredOrFunc, Context) -->
	prog_out__write_context(Context),
	report_warning("Error: no determinism declaration for local\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  (This is an error because you specified the `--no-infer-det'"),
		prog_out__write_context(Context),
		io__write_string("  option.  Use the `--infer-det' option if you want the"),
		prog_out__write_context(Context),
		io__write_string("  compiler to automatically infer the determinism of"),
		prog_out__write_context(Context),
		io__write_string("  local predicates.)")
	;
		[]
	).

:- pred unspecified_det_for_exported(sym_name, arity, pred_or_func,
			prog_context, io__state, io__state).
:- mode unspecified_det_for_exported(in, in, in, in, di, uo) is det.

unspecified_det_for_exported(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: no determinism declaration for exported\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred clause_for_imported_pred_error(sym_name, arity, pred_or_func,
				prog_context, io__state, io__state).
:- mode clause_for_imported_pred_error(in, in, in, in, di, uo) is det.

clause_for_imported_pred_error(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: clause for imported "),
	hlds_out__write_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred unqualified_pred_error(sym_name, int, prog_context,
				io__state, io__state).
:- mode unqualified_pred_error(in, in, in, di, uo) is det.

unqualified_pred_error(PredName, Arity, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Internal error: an unqualified predicate name `"),
	prog_out__write_sym_name(PredName),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n"),
	prog_out__write_context(Context),
	io__write_string("  should have been qualified by prog_io.m.\n").

:- pred pragma_conflict_error(sym_name, int, prog_context, string,
				io__state, io__state).
:- mode pragma_conflict_error(in, in, in, in, di, uo) is det.

pragma_conflict_error(Name, Arity, Context, PragmaName) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: `:- pragma "),
	io__write_string(PragmaName),
	io__write_string("' declaration conflicts with\n"),
	prog_out__write_context(Context),
	io__write_string("  previous pragma for "),
	hlds_out__write_pred_call_id(Name/Arity),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%
%	module_add_pragma_fact_table(PredName, Arity, FileName, 
%		Status, Context, Module0, Module, Info0, Info)
% Add a `pragma fact_table' declaration to the HLDS.  This predicate calls the 
% fact table compiler (fact_table_compile_facts) to create a separate `.o' file
% for the fact_table and then creates separate pieces of `pragma c_code' to 
% access the table in each mode of the fact table predicate.

:- pred module_add_pragma_fact_table(sym_name, arity, string, 
		import_status, prog_context, module_info, module_info,
		qual_info, qual_info, io__state, io__state).
:- mode module_add_pragma_fact_table(in, in, in, in, in, in, out, in, out,
		di, uo) is det.

module_add_pragma_fact_table(Pred, Arity, FileName, Status, Context,
		Module0, Module, Info0, Info) -->
	{ module_info_get_predicate_table(Module0, PredicateTable) },
	(
	    { predicate_table_search_sym_arity(PredicateTable, Pred, 
		    Arity, PredIDs0) },
	    { PredIDs0 = [PredID | PredIDs1] }
	->
	    (
		{ PredIDs1 = [] }, 		% only one predicate found
		{ module_info_pred_info(Module0, PredID, PredInfo0) },

		    % compile the fact table into a separate .o file
		fact_table_compile_facts(Pred, Arity, FileName, 
			PredInfo0, PredInfo, Context, Module0, C_HeaderCode, 
			PrimaryProcID),

		{module_info_set_pred_info(Module0, PredID, PredInfo, Module1)},
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ pred_info_procids(PredInfo, ProcIDs) },
		{ pred_info_arg_types(PredInfo, ArgTypes) },
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		{
		    PredOrFunc = predicate,
		    NumArgs = Arity
		;
		    PredOrFunc =  function,
		    NumArgs is Arity + 1
		},

		    % create pragma c_header_code to declare extern variables
		{ module_add_c_header(C_HeaderCode, Context, Module1, Module2)},

		io__get_exit_status(ExitStatus),
		(
		    { ExitStatus = 1 }
		->
		    { Module = Module2 },
		    { Info = Info0 }
		;
			% create some pragma c_code to access table in each mode
		    module_add_fact_table_procedures(ProcIDs, PrimaryProcID, 
			ProcTable, Pred, PredOrFunc, NumArgs, ArgTypes, 
			Status, Context, Module2, Module, Info0, Info)
		)
	    ;
	    	{ PredIDs1 = [_ | _] },		% >1 predicate found
	    	io__set_exit_status(1),
	    	prog_out__write_context(Context),
		io__write_string("In pragma fact_table for `"),
		hlds_out__write_pred_call_id(Pred/Arity),
		io__write_string("':\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  error: ambiguous predicate/function name.\n"),
		{ Module = Module0 },
		{ Info = Info0 }
	    )
	;
	    undefined_pred_or_func_error(Pred, Arity, Context, 
	    	"pragma fact_table"),
	    { Module = Module0 },
	    { Info = Info0 }
	).

	% Add a `pragma c_code' for each mode of the fact table lookup to the
	% HLDS.
	% `pragma fact_table's are represented in the HLDS by a 
	% `pragma c_code' for each mode of the predicate.

:- pred module_add_fact_table_procedures(list(proc_id), proc_id, proc_table, 
		sym_name, pred_or_func, arity, list(type), import_status, 
		prog_context, module_info, module_info, qual_info, qual_info, 
		io__state, io__state).
:- mode module_add_fact_table_procedures(in, in, in, in, in, in, in, in, 
		in, in, out, in, out, di, uo) is det.

module_add_fact_table_procedures([],_,_,_,_,_,_,_,_,Mod,Mod,Inf,Inf) --> [].
module_add_fact_table_procedures([ProcID | ProcIDs], PrimaryProcID, ProcTable, 
		SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
		Module0, Module, Info0, Info) -->
	module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName, 
			PredOrFunc, Arity, ArgTypes, Status, Context, 
			Module0, Module1, Info0, Info1),
	module_add_fact_table_procedures(ProcIDs, PrimaryProcID, ProcTable, 
		SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
		Module1, Module, Info1, Info).

:- pred module_add_fact_table_proc(proc_id, proc_id, proc_table, sym_name, 
		pred_or_func, arity, list(type), import_status, 
		prog_context, module_info, module_info, qual_info, qual_info, 
		io__state, io__state).
:- mode module_add_fact_table_proc(in, in, in, in, in, in, in, in, in, in,
		out, in, out, di, uo) is det.

module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName, 
		PredOrFunc, Arity, ArgTypes, Status, Context, 
		Module0, Module, Info0, Info) -->
	{ map__lookup(ProcTable, ProcID, ProcInfo) },
	{ varset__init(VarSet0) },
	{ varset__new_vars(VarSet0, Arity, Vars, VarSet) },
	% XXX This code may break if the proc has aliased argument modes.
	{ proc_info_argmodes(ProcInfo, argument_modes(_, Modes)) },
	{ fact_table_pragma_vars(Vars, Modes, VarSet, PragmaVars) },
	fact_table_generate_c_code(SymName, PragmaVars, ProcID, PrimaryProcID,
		ProcInfo, ArgTypes, Module0, C_ProcCode, C_ExtraCode),

	% XXX this should be modified to use nondet pragma c_code.
	{ default_attributes(Attrs0) },
	{ set_may_call_mercury(Attrs0, will_not_call_mercury, Attrs) },
	module_add_pragma_c_code(Attrs, SymName, PredOrFunc, 
		PragmaVars, VarSet, ordinary(C_ProcCode, no),
		Status, Context, Module0, Module1, Info0, Info),
	{
		C_ExtraCode = ""
	->
		Module = Module1
	;
		module_add_c_body_code(C_ExtraCode, Context, Module1, Module)
	}.

	% Create a list(pragma_var) that looks like the ones that are created
	% for pragma c_code in prog_io.m.
	% This is required by module_add_pragma_c_code to add the C code for
	% the procedure to the HLDS.

:- pred fact_table_pragma_vars(list(prog_var), list(mode), prog_varset,
		list(pragma_var)).
:- mode fact_table_pragma_vars(in, in, in, out) is det.

fact_table_pragma_vars(Vars0, Modes0, VarSet, PragmaVars0) :-
	(
		Vars0 = [Var | Vars1],
		Modes0 = [Mode | Modes1]
	->
		varset__lookup_name(VarSet, Var, Name),
		PragmaVar = pragma_var(Var, Name, Mode),
		fact_table_pragma_vars(Vars1, Modes1, VarSet, PragmaVars1),
		PragmaVars0 = [PragmaVar | PragmaVars1]
	;
		PragmaVars0 = []
	).

%-----------------------------------------------------------------------------%
