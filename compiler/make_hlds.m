%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2002 The University of Melbourne.
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

:- import_module prog_data, hlds_data, hlds_module, hlds_pred.
:- import_module equiv_type, module_qual, special_pred.

:- import_module io, std_util, list, bool.

% parse_tree_to_hlds(ParseTree, MQInfo, EqvMap, HLDS, QualInfo,
%		UndefTypes, UndefModes):
%	Given MQInfo (returned by module_qual.m) and EqvMap (returned by
%	equiv_type.m), converts ParseTree to HLDS.
%	Any errors found are recorded in the HLDS num_errors field.
%	Returns UndefTypes = yes if undefined types found.
%	Returns UndefModes = yes if undefined modes found.
%	QualInfo is an abstract type that is then passed back to
%	produce_instance_method_clauses (see below).	
:- pred parse_tree_to_hlds(compilation_unit, mq_info, eqv_map, module_info,
			qual_info, bool, bool, io__state, io__state).
:- mode parse_tree_to_hlds(in, in, in, out, out, out, out, di, uo) is det.

:- pred add_new_proc(pred_info, inst_varset, arity, list(mode),
		maybe(list(mode)), maybe(list(is_live)), maybe(determinism),
		prog_context, is_address_taken, pred_info, proc_id).
:- mode add_new_proc(in, in, in, in, in, in, in, in, in, out, out) is det.

	% add_special_pred_for_real(SpecialPredId, ModuleInfo0, TVarSet,
	% 	Type, TypeId, TypeBody, TypeContext, TypeStatus, ModuleInfo).
	%
	% Add declarations and clauses for a special predicate.
	% This is used by unify_proc.m to add a unification predicate
	% for an imported type for which special predicates are being
	% generated only when a unification procedure is requested
	% during mode analysis.
:- pred add_special_pred_for_real(special_pred_id,
		module_info, tvarset, type, type_id, hlds_type_body,
		prog_context, import_status, module_info).
:- mode add_special_pred_for_real(in, in, in, in, in, in, in, in, out) is det.

	% add_special_pred_decl_for_real(SpecialPredId, ModuleInfo0, TVarSet,
	% 	Type, TypeId, TypeContext, TypeStatus, ModuleInfo).
	%
	% Add declarations for a special predicate.
	% This is used by higher_order.m when specializing an in-in
	% unification for an imported type for which unification procedures
	% are generated lazily.	
:- pred add_special_pred_decl_for_real(special_pred_id,
		module_info, tvarset, type, type_id, prog_context,
		import_status, module_info).
:- mode add_special_pred_decl_for_real(in, in, in, in, in, in, in, out) is det.

:- type qual_info.

	% Given the definition for a predicate or function from a
	% type class instance declaration, produce the clauses_info
	% for that definition.
:- pred produce_instance_method_clauses(instance_proc_def::in,
		pred_or_func::in, arity::in, list(type)::in, pred_markers::in,
		term__context::in, import_status::in, clauses_info::out,
		module_info::in, module_info::out,
		qual_info::in, qual_info::out,
		io__state::di, io__state::uo) is det.

	% Move the recompilation_info from the qual_info to the module_info
	% after make_hlds is finished with it and the qual_info is dead.
:- pred set_module_recompilation_info(qual_info::in,
		module_info::in, module_info::out) is det.

:- pred next_mode_id(proc_table, maybe(determinism), proc_id).
:- mode next_mode_id(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal.
:- import_module prog_io, prog_io_goal, prog_io_dcg, prog_io_util, prog_out.
:- import_module modules, module_qual, prog_util, options, hlds_out, typecheck.
:- import_module make_tags, quantification, (inst), globals.
:- import_module code_util, unify_proc, type_util, mode_util, mode_errors.
:- import_module mercury_to_mercury, passes_aux, clause_to_proc, inst_match.
:- import_module fact_table, purity, goal_util, term_util, export, llds.
:- import_module error_util, foreign.
:- import_module recompilation.

:- import_module string, char, int, set, bintree, map, multi_map, require.
:- import_module bag, term, varset, getopt, assoc_list, term_io.

parse_tree_to_hlds(module(Name, Items), MQInfo0, EqvMap, Module, QualInfo,
		UndefTypes, UndefModes) -->
	globals__io_get_globals(Globals),
	{ mq_info_get_partial_qualifier_info(MQInfo0, PQInfo) },
	{ module_info_init(Name, Items, Globals, PQInfo, no, Module0) },
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
	{ init_qual_info(MQInfo0, EqvMap, QualInfo0) },
	add_item_list_clauses(Items, local, Module3, Module4,
				QualInfo0, QualInfo),
	{ qual_info_get_mq_info(QualInfo, MQInfo) },
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
	% Add the clauses one by one to the module.
	% (I supposed this could conceivably be folded into pass 2?)
	%
	% Check that the declarations for field extraction
	% and update functions are sensible.

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
add_item_decl_pass_1(clause(_, _, _, _, _), _, Status, Module, Status, Module)
		--> [].

add_item_decl_pass_1(type_defn(_, _, _, _, _), _, Status, Module,
		Status, Module) --> [].

add_item_decl_pass_1(inst_defn(VarSet, Name, Params, InstDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_inst_defn(Module0, VarSet, Name, Params,
		InstDefn, Cond, Context, Status, Module).

add_item_decl_pass_1(mode_defn(VarSet, Name, Params, ModeDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_mode_defn(Module0, VarSet, Name, Params, ModeDefn,
		Cond, Context, Status, Module).

add_item_decl_pass_1(pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
		PredOrFunc, PredName, TypesAndModes, MaybeDet, Cond,
		Purity, ClassContext),
		Context, Status, Module0, Status, Module) -->
	{ init_markers(Markers) },
	module_add_pred_or_func(Module0, TypeVarSet, InstVarSet, ExistQVars,
		PredOrFunc, PredName, TypesAndModes, MaybeDet, Cond,
		Purity, ClassContext, Markers, Context, Status, _, Module).

add_item_decl_pass_1(
		pred_or_func_mode(VarSet, PredOrFunc, PredName,
			Modes, MaybeDet, Cond),
		Context, Status, Module0, Status, Module) -->
	{ Status = item_status(ImportStatus, _) },
	{ IsClassMethod = no },
	module_add_mode(Module0, VarSet, PredName, Modes, MaybeDet, Cond,
		ImportStatus, Context, PredOrFunc, IsClassMethod, _, Module).

add_item_decl_pass_1(pragma(_), _, Status, Module, Status, Module) --> [].

add_item_decl_pass_1(assertion(_, _), _, Status, Module, Status, Module) --> [].

add_item_decl_pass_1(module_defn(_VarSet, ModuleDefn), Context,
		Status0, Module0, Status, Module) -->
	( { module_defn_update_import_status(ModuleDefn, Status1) } ->
		{ Status = Status1 },
		{ Module = Module0 }
	; { ModuleDefn = import(module(Specifiers)) } ->
		{ Status = Status0 },
		{ Status = item_status(IStat, _) },
		( { status_defined_in_this_module(IStat, yes) } ->
			{ module_add_imported_module_specifiers(Specifiers,
				Module0, Module) }
		;
			{ module_add_indirectly_imported_module_specifiers(
				Specifiers, Module0, Module) }
		)
	; { ModuleDefn = use(module(Specifiers)) } ->
		{ Status = Status0 },
		{ Status = item_status(IStat, _) },
		( { status_defined_in_this_module(IStat, yes) } ->
			{ module_add_imported_module_specifiers(Specifiers,
				Module0, Module) }
		;
			{ module_add_indirectly_imported_module_specifiers(
				Specifiers, Module0, Module) }
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
	; { ModuleDefn = module(_ModuleName) } ->
		report_unexpected_decl("module", Context),
		{ Status = Status0 },
		{ Module = Module0 }
	; { ModuleDefn = end_module(_ModuleName) } ->
		report_unexpected_decl("end_module", Context),
		{ Status = Status0 },
		{ Module = Module0 }
	; { ModuleDefn = version_numbers(_, _) } ->
		{ Status = Status0 },
		{ Module = Module0 }
	; { ModuleDefn = transitively_imported } ->
		{ Status = Status0 },
		{ Module = Module0 }
	;
		{ Status = Status0 },
		{ Module = Module0 },
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		report_warning("Warning: declaration not yet implemented.\n"),
		io__set_output_stream(OldStream, _)
	).

add_item_decl_pass_1(nothing(_), _, Status, Module, Status, Module) --> [].

add_item_decl_pass_1(typeclass(Constraints, Name, Vars, Interface, VarSet), 
		Context, Status, Module0, Status, Module) -->
	module_add_class_defn(Module0, Constraints, Name, Vars, Interface,
		VarSet, Context, Status, Module).

	% We add instance declarations on the second pass so that we don't add
	% an instance declaration before its class declaration.
add_item_decl_pass_1(instance(_, _, _, _, _,_), _, Status, Module, Status,
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

add_item_decl_pass_2(type_defn(VarSet, Name, Args, TypeDefn, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_type_defn(Module0, VarSet, Name, Args, TypeDefn,
		Cond, Context, Status, Module).

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
		{ Pragma = foreign_code(Lang, Body_Code) },
		{ module_add_foreign_body_code(Lang, Body_Code, Context,
			Module0, Module) }
	;
		{ Pragma  = foreign_decl(Lang, C_Header) },
		{ module_add_foreign_decl(Lang, C_Header, Context,
			Module0, Module) }
	;
		{ Pragma  = foreign_import_module(Lang, Import) },
		{ module_add_foreign_import_module(Lang, Import, Context,
			Module0, Module) }
	;
		% Handle pragma foreign procs later on (when we process
		% clauses).
		{ Pragma = foreign_proc(_, _, _, _, _, _) },
		{ Module = Module0 }
	;	
		{ Pragma = foreign_type(ForeignType, _MercuryType, Name) },

		{ ForeignType = il(RefOrVal,
				ForeignTypeLocation, ForeignTypeName) },

		{ RefOrVal = reference,
			IsBoxed = yes
		; RefOrVal = value,
			IsBoxed = no
		},

		{ varset__init(VarSet) },
		{ Args = [] },
		{ Body = foreign_type(IsBoxed,
				ForeignTypeName, ForeignTypeLocation) },
		{ Cond = true },

		{ TypeId = Name - 0 },
		{ module_info_types(Module0, Types) },
		{ TypeStr = error_util__describe_sym_name_and_arity(
				Name / 0) },
		( 
			{ map__search(Types, TypeId, OldDefn) }
		->
			{ hlds_data__get_type_defn_status(OldDefn, OldStatus) },
			{ combine_status(OldStatus, ImportStatus, NewStatus) },
			( { NewStatus = abstract_exported } ->
				{ ErrorPieces = [
					words("Error: pragma foreign_type "),
					fixed(TypeStr),
					words("must have the same visibility as the type declaration.")
				] },
				error_util__write_error_pieces(Context, 0, ErrorPieces),
				{ module_info_incr_errors(Module0, Module) }

			;
				module_add_type_defn_2(Module0, VarSet, Name,
					Args, Body, Cond, Context, Status,
					Module)
			)
		;
			{ ErrorPieces = [
				words("Error: type "),
				fixed(TypeStr),
				words("defined as foreign_type without being declared.")
			] },
			error_util__write_error_pieces(Context, 0, ErrorPieces),
			{ module_info_incr_errors(Module0, Module) }
		)
	;	
		% Handle pragma tabled decls later on (when we process
		% clauses).
		{ Pragma = tabled(_, _, _, _, _) },
		{ Module = Module0 }
	;
		{ Pragma = inline(Name, Arity) },
		add_pred_marker(Module0, "inline", Name, Arity, ImportStatus,
			Context, inline, [no_inline], Module)
	;
		{ Pragma = no_inline(Name, Arity) },
		add_pred_marker(Module0, "no_inline", Name, Arity,
			ImportStatus, Context, no_inline, [inline], Module)
	;
		{ Pragma = obsolete(Name, Arity) },
		add_pred_marker(Module0, "obsolete", Name, Arity, ImportStatus,
			Context, obsolete, [], Module)
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
				Arity, ModeNum, UnusedArgs) },
		( { ImportStatus \= opt_imported } ->
			prog_out__write_context(Context),
			io__write_string(
			    "Error: illegal use of pragma `unused_args'.\n"),
			{ module_info_incr_errors(Module0, Module) }
		;
			add_pragma_unused_args(PredOrFunc, SymName, Arity,
				ModeNum, UnusedArgs, Context, Module0, Module)
		)
	;
		% Handle pragma type_spec decls later on (when we process
		% clauses).
		{ Pragma = type_spec(_, _, _, _, _, _, _, _) },
		{ Module = Module0 }
	;
		% Handle pragma fact_table decls later on (when we process
		% clauses).
		{ Pragma = fact_table(_, _, _) },
		{ Module = Module0 }
	;
		{ Pragma = aditi(PredName, Arity) },
		maybe_enable_aditi_compilation(Status, Context,
			Module0, Module1),
		add_pred_marker(Module1, "aditi", PredName, Arity,
			ImportStatus, Context, aditi, [], Module2),
		add_stratified_pred(Module2, "aditi", PredName, Arity, Context, 
			Module)
	;
		{ Pragma = base_relation(PredName, Arity) },
		maybe_enable_aditi_compilation(Status, Context,
			Module0, Module1),
		add_pred_marker(Module1, "aditi", PredName, Arity,
			ImportStatus, Context, aditi, [], Module2),
		add_pred_marker(Module2, "base_relation", PredName, Arity,
			ImportStatus, Context, base_relation, [], Module3),
		module_mark_as_external(PredName, Arity, Context,
			Module3, Module)
	;
		{ Pragma = aditi_index(PredName, Arity, Index) },
		add_base_relation_index(Module0, PredName, Arity, Index,
			ImportStatus, Context, Module)
	;
		{ Pragma = naive(PredName, Arity) },
		add_pred_marker(Module0, "naive", PredName, Arity,
			ImportStatus, Context, naive, [psn], Module)
	;
		{ Pragma = psn(PredName, Arity) },
		add_pred_marker(Module0, "psn", PredName, Arity,
			ImportStatus, Context, psn, [naive], Module)
	;
		{ Pragma = aditi_memo(Name, Arity) },
		add_pred_marker(Module0, "aditi_memo", Name, Arity,
			ImportStatus, Context, aditi_memo,
			[aditi_no_memo], Module)
	;
		{ Pragma = aditi_no_memo(PredName, Arity) },
		add_pred_marker(Module0, "aditi_no_memo", PredName, Arity,
			ImportStatus, Context, aditi_no_memo,
			[aditi_memo], Module)
	;
		{ Pragma = supp_magic(PredName, Arity) },
		add_pred_marker(Module0, "supp_magic", PredName, Arity, 
			ImportStatus, Context, supp_magic, [context], Module)
	;
		{ Pragma = context(PredName, Arity) },
		add_pred_marker(Module0, "context", PredName, Arity, 
			ImportStatus, Context, context, [supp_magic], Module)
	;
		{ Pragma = owner(PredName, Arity, Owner) },
		set_pred_owner(Module0, PredName, Arity, Owner, ImportStatus,
			Context, Module)
	;
		{ Pragma = promise_pure(Name, Arity) },
		add_pred_marker(Module0, "promise_pure", Name, Arity,
			ImportStatus, Context, promised_pure, [], Module)
	;
		{ Pragma = promise_semipure(Name, Arity) },
		add_pred_marker(Module0, "promise_semipure", Name, Arity,
			ImportStatus, Context, promised_semipure, [], Module)
	;
		{ Pragma = termination_info(PredOrFunc, SymName, ModeList, 
			MaybeArgSizeInfo, MaybeTerminationInfo) },
		add_pragma_termination_info(PredOrFunc, SymName, ModeList,
			MaybeArgSizeInfo, MaybeTerminationInfo, Context,
			Module0, Module)
	;
		{ Pragma = terminates(Name, Arity) },
		add_pred_marker(Module0, "terminates", Name, Arity,
			ImportStatus, Context, terminates,
			[check_termination, does_not_terminate], Module)
	;
		{ Pragma = does_not_terminate(Name, Arity) },
		add_pred_marker(Module0, "does_not_terminate", Name, Arity,
			ImportStatus, Context, does_not_terminate,
			[check_termination, terminates], Module)
	;
		{ Pragma = check_termination(Name, Arity) },
		add_pred_marker(Module0, "check_termination", Name, Arity, 
			ImportStatus, Context, check_termination, 
			[terminates, does_not_terminate], 
			Module)
	).

add_item_decl_pass_2(
		pred_or_func(_TypeVarSet, _InstVarSet, _ExistQVars,
			PredOrFunc, SymName, TypesAndModes, _MaybeDet,
			_Cond, _Purity, _ClassContext),
		_Context, Status, Module0, Status, Module) -->
	%
	% add default modes for function declarations, if necessary
	%
	{
		PredOrFunc = predicate,
		Module = Module0
	;
		PredOrFunc = function,
		list__length(TypesAndModes, Arity),
		adjust_func_arity(function, FuncArity, Arity),
		module_info_get_predicate_table(Module0, PredTable0),
		(
			predicate_table_search_func_sym_arity(PredTable0,
				SymName, FuncArity, PredIds)
		->
			predicate_table_get_preds(PredTable0, Preds0),
			maybe_add_default_func_modes(PredIds, Preds0, Preds),
			predicate_table_set_preds(PredTable0,
				Preds, PredTable),
			module_info_set_predicate_table(Module0,
				PredTable, Module)
		;
			error("make_hlds.m: can't find func declaration")
		)
	}.

add_item_decl_pass_2(assertion(_, _), _, Status, Module, Status, Module) --> [].
add_item_decl_pass_2(clause(_, _, _, _, _), _, Status, Module, Status,
		Module) --> [].
add_item_decl_pass_2(inst_defn(_, _, _, _, _), _, Status, Module,
		Status, Module) --> [].
add_item_decl_pass_2(mode_defn(_, _, _, _, _), _, Status, Module,
		Status, Module) --> [].
add_item_decl_pass_2(pred_or_func_mode(_, _, _, _, _, _), _,
		Status, Module, Status, Module) --> [].
add_item_decl_pass_2(nothing(_), _, Status, Module, Status, Module) --> [].
add_item_decl_pass_2(typeclass(_, _, _, _, _)
	, _, Status, Module, Status, Module) --> [].
add_item_decl_pass_2(instance(Constraints, Name, Types, Body, VarSet,
		InstanceModuleName), Context,
		Status, Module0, Status, Module) -->
	{ Status = item_status(ImportStatus, _) },
	{ Body = abstract ->
		make_status_abstract(ImportStatus, BodyStatus)
	;
		BodyStatus = ImportStatus
	},
	module_add_instance_defn(Module0, InstanceModuleName, Constraints,
		Name, Types, Body, VarSet, BodyStatus, Context, Module).

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
module_defn_update_import_status(imported(Section),
		item_status(imported(Section), may_be_unqualified)).
module_defn_update_import_status(used(Section),
		item_status(imported(Section), must_be_qualified)).
module_defn_update_import_status(opt_imported,
		item_status(opt_imported, must_be_qualified)).

%-----------------------------------------------------------------------------%

	% If there are any Aditi procedures enable Aditi compilation.
	% If there are only imported Aditi procedures, magic.m still
	% needs to remove the `aditi' and `base_relation' markers
	% so that the procedures are not ignored by the code
	% generation annotation passes (e.g. arg_info.m).
:- pred maybe_enable_aditi_compilation(item_status, term__context,
		module_info, module_info, io__state, io__state).
:- mode maybe_enable_aditi_compilation(in, in, in, out, di, uo) is det.

maybe_enable_aditi_compilation(_Status, Context, Module0, Module) -->
	globals__io_lookup_bool_option(aditi, Aditi),
	( { Aditi = no } ->
		prog_out__write_context(Context),
		io__write_string("Error: compilation of Aditi procedures\n"),
		prog_out__write_context(Context),
		io__write_string("  requires the `--aditi' option.\n"),
		io__set_exit_status(1),
		{ module_info_incr_errors(Module0, Module) }
	;
		% There are Aditi procedures - enable Aditi code generation.
		{ module_info_set_do_aditi_compilation(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_clause(item, import_status, import_status, prog_context, 
	module_info, module_info, qual_info, qual_info, io__state, io__state).
:- mode add_item_clause(in, in, out, in, in, out, in, out, di, uo) is det.

add_item_clause(clause(VarSet, PredOrFunc, PredName, Args, Body),
		Status, Status, Context, Module0, Module, Info0, Info) -->
	check_not_exported(Status, Context, "clause"),
	{ IsAssertion = no },
	module_add_clause(Module0, VarSet, PredOrFunc, PredName,
		Args, Body, Status, Context, IsAssertion, Module, Info0, Info).
add_item_clause(type_defn(_, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(inst_defn(_, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(mode_defn(_, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(
		pred_or_func(_, _, _, PredOrFunc, SymName, TypesAndModes,
			_, _, _, _),
		Status, Status, Context, Module, Module, Info, Info) -->
	(
		{ PredOrFunc = predicate }
	;
		{ PredOrFunc = function},
		{ list__length(TypesAndModes, PredArity) },
		{ adjust_func_arity(function, FuncArity, PredArity) },
		maybe_check_field_access_function(SymName, FuncArity,
			Status, Context, Module)
	).
		
add_item_clause(pred_or_func_mode(_, _, _, _, _, _), Status, Status, _,
				Module, Module, Info, Info) --> [].
add_item_clause(module_defn(_, Defn), Status0, Status, _,
		Module0, Module, Info0, Info) --> 
	{ Defn = version_numbers(ModuleName, ModuleVersionNumbers) ->
		%
		% Record the version numbers for each imported module
		% if smart recompilation is enabled.
		%
		apply_to_recompilation_info(
		    (pred(RecompInfo0::in, RecompInfo::out) is det :-
			RecompInfo = RecompInfo0 ^ version_numbers ^
				map__elem(ModuleName) := ModuleVersionNumbers
		    ),
		    transform_info(Module0, Info0),
		    transform_info(Module, Info)),
		Status = Status0
	; module_defn_update_import_status(Defn, ItemStatus1) ->
		ItemStatus1 = item_status(Status1, NeedQual),
		qual_info_get_mq_info(Info0, MQInfo0),
		mq_info_set_need_qual_flag(MQInfo0, NeedQual, MQInfo),
		qual_info_set_mq_info(Info0, MQInfo, Info),
		Module = Module0,
		Status = Status1
	;
		Module = Module0,
		Info = Info0,
		Status = Status0
	}.
add_item_clause(pragma(Pragma), Status, Status, Context,
		Module0, Module, Info0, Info) -->
	(
		{ Pragma = foreign_proc(Attributes, Pred, PredOrFunc,
			Vars, VarSet, PragmaImpl) }
	->
		module_add_pragma_foreign_proc(Attributes, 
			Pred, PredOrFunc, Vars, VarSet, PragmaImpl,
			Status, Context, Module0, Module, Info0, Info)
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
				Mode, Status, Context, Module0, Module)
		;
			{ module_info_incr_errors(Module0, Module) },
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma "),
			{ eval_method_to_string(Type, EvalMethodS) },
			io__write_string(EvalMethodS),
			io__write_string(
"' declaration requires the type_ctor_layout\n"),
			prog_out__write_context(Context),
			io__write_string(
"    structures. Use the --type-layout flag to enable them.\n")
		),
		{ Info = Info0 }
	;
		{ Pragma = type_spec(_, _, _, _, _, _, _, _) }
	->
		add_pragma_type_spec(Pragma, Context, Module0, Module,
			Info0, Info)
	;
		% don't worry about any pragma decs but c_code, tabling,
		% type_spec and fact_table here
		{ Module = Module0 },
		{ Info = Info0 }	
	).
add_item_clause(assertion(Goal0, VarSet),
		Status, Status, Context, Module0, Module, Info0, Info) -->

		%
		% If the outermost existentially quantified variables
		% are placed in the head of the assertion, the
		% typechecker will avoid warning about unbound
		% type variables as this implicity adds a universal
		% quantification of the typevariables needed.
		%
	(
		{ Goal0 = all(Vars, AllGoal) - _Context }
	->
		{ term__var_list_to_term_list(Vars, HeadVars) },
		{ Goal = AllGoal }
	;
		{ HeadVars = [] },
		{ Goal = Goal0 }
	),

	{ term__context_line(Context, Line) },
	{ term__context_file(Context, File) },
	{ string__format("assertion__%d__%s", [i(Line), s(File)], Name) },

		%
		% The assertions are recorded as a predicate whose
		% goal_type is set to assertion.  This allows us to
		% leverage off all the other checks in the compiler that
		% operate on predicates.
		%
		% :- promise all [A,B,R] ( R = A + B <=> R = B + A ).
		%
		% becomes
		%
		% assertion__lineno_filename(A, B, R) :-
		% 	( R = A + B <=> R = B + A ).
		%
	{ IsAssertion = yes },
	module_add_clause(Module0, VarSet, predicate, unqualified(Name),
			HeadVars, Goal, Status, Context, IsAssertion, Module,
			Info0, Info).

add_item_clause(nothing(_), Status, Status, _,
	Module, Module, Info, Info) --> [].
add_item_clause(typeclass(_, _, _, _, _),
	Status, Status, _, Module, Module, Info, Info) --> [].
add_item_clause(instance(_, _, _, _, _, _),
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

:- pred add_pragma_export(sym_name, pred_or_func, list(mode), string,
	prog_context, module_info, module_info, io__state, io__state).
:- mode add_pragma_export(in, in, in, in, in, in, out, di, uo) is det.

add_pragma_export(Name, PredOrFunc, Modes, C_Function, Context,
			Module0, Module) -->
	{ module_info_get_predicate_table(Module0, PredTable) },
	{ list__length(Modes, Arity) },
	(
		{ predicate_table_search_pf_sym_arity(PredTable,
			PredOrFunc, Name, Arity, [PredId]) }
	->
		{ predicate_table_get_preds(PredTable, Preds) },
		{ map__lookup(Preds, PredId, PredInfo) },
		{ pred_info_procedures(PredInfo, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		(
			{ get_procedure_matching_declmodes(
				ExistingProcs, Modes, Module0, ProcId)}
		->
			{ module_info_get_pragma_exported_procs(Module0,
				PragmaExportedProcs0) },
			{ NewExportedProc = pragma_exported_proc(PredId,
				ProcId, C_Function, Context) },
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

:- pred add_pragma_unused_args(pred_or_func, sym_name, arity, mode_num,
		list(int), prog_context, module_info, module_info,
		io__state, io__state).
:- mode add_pragma_unused_args(in, in, in, in, in, in, in, out, di, uo) is det.

add_pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum, UnusedArgs,
		Context, Module0, Module) -->
	{ module_info_get_predicate_table(Module0, Preds) },
	(
		{ predicate_table_search_pf_sym_arity(Preds,
			PredOrFunc, SymName, Arity, [PredId]) }
	->
		{ module_info_unused_arg_info(Module0, UnusedArgInfo0) },
		% convert the mode number to a proc_id
		{ proc_id_to_int(ProcId, ModeNum) },
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

:- pred add_pragma_type_spec(pragma_type, term__context,
		module_info, module_info, qual_info, qual_info,
		io__state, io__state).
:- mode add_pragma_type_spec(in(type_spec), in, in, out,
		in, out, di, uo) is det.

add_pragma_type_spec(Pragma, Context, Module0, Module, Info0, Info) -->
	{ Pragma = type_spec(SymName, _, Arity, MaybePredOrFunc, _, _, _, _) },
	{ module_info_get_predicate_table(Module0, Preds) },
	(
		{ MaybePredOrFunc = yes(PredOrFunc) ->
			adjust_func_arity(PredOrFunc, Arity, PredArity),
			predicate_table_search_pf_sym_arity(Preds,
				PredOrFunc, SymName, PredArity, PredIds)
		;
			predicate_table_search_sym_arity(Preds,
				SymName, Arity, PredIds)
		},
		{ PredIds \= [] }
	->
		list__foldl2(add_pragma_type_spec_2(Pragma, Context),
			PredIds, transform_info(Module0, Info0),
			transform_info(Module, Info))
	;
		{ Info = Info0 },
		undefined_pred_or_func_error(SymName, Arity, Context,
			"`:- pragma type_spec' declaration"),
		{ module_info_incr_errors(Module0, Module) }
	).

:- pred add_pragma_type_spec_2(pragma_type, prog_context, pred_id,
		transform_info, transform_info, io__state, io__state).
:- mode add_pragma_type_spec_2(in(type_spec), in, in, in, out, di, uo) is det.

add_pragma_type_spec_2(Pragma0, Context, PredId,
		transform_info(ModuleInfo0, Info0), TransformInfo) -->
	{ Pragma0 = type_spec(SymName, SpecName, Arity, _,
			MaybeModes, Subst, TVarSet0, UsedEquivTypes) },
	{ module_info_pred_info(ModuleInfo0, PredId, PredInfo0) },
	handle_pragma_type_spec_subst(Context, Subst, TVarSet0, PredInfo0,
		TVarSet, Types, ExistQVars, ClassContext, SubstOk,
		ModuleInfo0, ModuleInfo1),
	( { SubstOk = yes(RenamedSubst) } ->
	    { pred_info_procedures(PredInfo0, Procs0) },
	    handle_pragma_type_spec_modes(SymName, Arity, Context,
	    	MaybeModes, ProcIds, Procs0, Procs, ModesOk,
		ModuleInfo1, ModuleInfo2),
	    globals__io_lookup_bool_option(user_guided_type_specialization,
	    	DoTypeSpec),
	    globals__io_lookup_bool_option(smart_recompilation, Smart),
	    {
		ModesOk = yes,
		% Even if we aren't doing type specialization, we need
		% to create the interface procedures for local predicates
		% to check the type-class correctness of the requested
		% specializations.
		%
		% If we're doing smart recompilation we need to record the
		% pragmas even if we aren't doing type specialization to
		% avoid problems with differing output for the recompilation
		% tests in debugging grades.
		%
		( DoTypeSpec = yes
	    	; \+ pred_info_is_imported(PredInfo0)
		; Smart = yes
	    	)
	    ->
		%
		% Build a clause to call the old predicate with the
		% specified types to force the specialization. For imported
		% predicates this forces the creation of the proper interface. 
		%
		pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc),
		adjust_func_arity(PredOrFunc, Arity, PredArity),
		varset__init(ArgVarSet0),
		make_n_fresh_vars("HeadVar__", PredArity,
			ArgVarSet0, Args, ArgVarSet),
		% XXX We could use explicit type qualifications here
		% for the argument types, but explicit type qualification
		% doesn't work correctly with type inference due to
		% a bug somewhere in typecheck.m -- the explicitly declared
		% types are not kept in sync with the predicate's tvarset
		% after the first pass of type checking.
		% map__from_corresponding_lists(Args, Types, VarTypes0).
		map__init(VarTypes0),
		goal_info_init(GoalInfo0),
		set__list_to_set(Args, NonLocals),
		goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
		goal_info_set_context(GoalInfo1, Context, GoalInfo),

		%
		% We don't record the called predicate as used --
		% it is only used if there is some other call.
		% This call is only used to make higher_order.m generate
		% the interface for the type specialized procedure, and
		% will be removed by higher_order.m after that is done.
		%
		do_construct_pred_or_func_call(PredId, PredOrFunc, SymName,
			Args, GoalInfo, Goal),
		Clause = clause(ProcIds, Goal, mercury, Context),
		map__init(TI_VarMap),
		map__init(TCI_VarMap),
		map__init(TVarNameMap),
		HasForeignClauses = no,
		Clauses = clauses_info(ArgVarSet, VarTypes0, TVarNameMap,
			VarTypes0, Args, [Clause], TI_VarMap, TCI_VarMap,
			HasForeignClauses),
		pred_info_get_markers(PredInfo0, Markers),
		map__init(Proofs),

		( pred_info_is_imported(PredInfo0) ->
			Status = opt_imported
		;
			pred_info_import_status(PredInfo0, Status)
		),

		pred_info_module(PredInfo0, ModuleName),
		pred_info_get_aditi_owner(PredInfo0, Owner),
		pred_info_init(ModuleName, SpecName, PredArity, TVarSet,
			ExistQVars, Types, true, Context, Clauses,
			Status, Markers, none, PredOrFunc,
			ClassContext, Proofs, Owner, NewPredInfo0),
		pred_info_set_procedures(NewPredInfo0,
			Procs, NewPredInfo),
		module_info_get_predicate_table(ModuleInfo2, PredTable0),
		predicate_table_insert(PredTable0, NewPredInfo,
			NewPredId, PredTable),
		module_info_set_predicate_table(ModuleInfo2,
			PredTable, ModuleInfo3),

		%
		% Record the type specialisation in the module_info.
		%
		module_info_type_spec_info(ModuleInfo3, TypeSpecInfo0),
		TypeSpecInfo0 = type_spec_info(ProcsToSpec0,
			ForceVersions0, SpecMap0, PragmaMap0),
		list__map(lambda([ProcId::in, PredProcId::out] is det, (
				PredProcId = proc(PredId, ProcId)
			)), ProcIds, PredProcIds),
		set__insert_list(ProcsToSpec0, PredProcIds, ProcsToSpec),
		set__insert(ForceVersions0, NewPredId, ForceVersions),

		( Status = opt_imported ->
			% For imported predicates dead_proc_elim.m needs
			% to know that if the original predicate is used,
			% the predicate to force the production of the
			% specialised interface is also used.
			multi_map__set(SpecMap0, PredId, NewPredId, SpecMap)
		;
			SpecMap = SpecMap0
		),
		Pragma = type_spec(SymName, SpecName, Arity,
				yes(PredOrFunc), MaybeModes,
				map__to_assoc_list(RenamedSubst), TVarSet,
				UsedEquivTypes), 
		multi_map__set(PragmaMap0, PredId, Pragma, PragmaMap),
		TypeSpecInfo = type_spec_info(ProcsToSpec,
			ForceVersions, SpecMap, PragmaMap),
		module_info_set_type_spec_info(ModuleInfo3,
			TypeSpecInfo, ModuleInfo),

		TransformInfo1 = transform_info(ModuleInfo, Info0),
		( status_is_imported(Status, yes) ->
		    ItemType = pred_or_func_to_item_type(PredOrFunc),
		    apply_to_recompilation_info(
			recompilation__record_used_equivalence_types(
				item_id(ItemType, SymName - Arity), 
				UsedEquivTypes),
			TransformInfo1, TransformInfo)
		;
		    TransformInfo = TransformInfo1
		)
	    ;
		TransformInfo = transform_info(ModuleInfo2, Info0)
	    }
	;
	    { TransformInfo = transform_info(ModuleInfo1, Info0) }
	).

	% Check that the type substitution for a `:- pragma type_spec'
	% declaration is valid.
	% A type substitution is invalid if:
	%	- it substitutes unknown type variables
	% 	- it substitutes existentially quantified type variables
	% Type substitutions are also invalid if the replacement types are
	% not ground, however this is a (hopefully temporary) limitation
	% of the current implementation, so it only results in a warning.
:- pred handle_pragma_type_spec_subst(prog_context, assoc_list(tvar, type),
	tvarset, pred_info, tvarset, list(type), existq_tvars,
	class_constraints, maybe(tsubst), module_info, module_info,
	io__state, io__state).
:- mode handle_pragma_type_spec_subst(in, in, in, in, out, out, out, out, out,
		in, out, di, uo) is det.

handle_pragma_type_spec_subst(Context, Subst, TVarSet0, PredInfo0,
		TVarSet, Types, ExistQVars, ClassContext, SubstOk,
		ModuleInfo0, ModuleInfo) -->
	{ assoc_list__keys(Subst, VarsToSub) },
	(
	    { Subst = [] }
	->
	    { error("handle_pragma_type_spec_subst: empty substitution") }
	;
	    { find_duplicate_list_elements(VarsToSub, MultiSubstVars0) },
	    { MultiSubstVars0 \= [] }
	->
    	    { list__sort_and_remove_dups(MultiSubstVars0, MultiSubstVars) },
	    report_multiple_subst_vars(PredInfo0, Context,
	    	TVarSet0, MultiSubstVars),
	    { module_info_incr_errors(ModuleInfo0, ModuleInfo) },
	    io__set_exit_status(1),
	    { ExistQVars = [] },
	    { Types = [] },
	    { ClassContext = constraints([], []) },
	    { varset__init(TVarSet) },
	    { SubstOk = no }
	;
	    { pred_info_typevarset(PredInfo0, CalledTVarSet) },
	    { varset__create_name_var_map(CalledTVarSet, NameVarIndex0) },
	    { list__filter(lambda([Var::in] is semidet, (
		varset__lookup_name(TVarSet0, Var, VarName),
		\+ map__contains(NameVarIndex0, VarName)
	    )), VarsToSub, UnknownVarsToSub) },
	    ( { UnknownVarsToSub = [] } ->
		% Check that the substitution is not recursive.
		{ set__list_to_set(VarsToSub, VarsToSubSet) },

		{ assoc_list__values(Subst, SubstTypes0) },
		{ term__vars_list(SubstTypes0, TVarsInSubstTypes0) },
		{ set__list_to_set(TVarsInSubstTypes0, TVarsInSubstTypes) },

		{ set__intersect(TVarsInSubstTypes, VarsToSubSet,
			RecSubstTVars0) },
		{ set__to_sorted_list(RecSubstTVars0, RecSubstTVars) },

		( { RecSubstTVars = [] } ->
		    { map__init(TVarRenaming0) },
		    { list__append(VarsToSub, TVarsInSubstTypes0,
				VarsToReplace) },
		    
		    { get_new_tvars(VarsToReplace, TVarSet0, CalledTVarSet,
				TVarSet, NameVarIndex0, _,
				TVarRenaming0, TVarRenaming) },

		    % Check that none of the existentially quantified
		    % variables were substituted.
		    { map__apply_to_list(VarsToSub, TVarRenaming,
				RenamedVarsToSub) },
		    { pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars) },
		    { list__filter(lambda([RenamedVar::in] is semidet, (
				list__member(RenamedVar, ExistQVars)
			)), RenamedVarsToSub, SubExistQVars) },
		    ( { SubExistQVars = [] } ->
			{
			map__init(TypeSubst0),
			term__apply_variable_renaming_to_list(SubstTypes0,
				TVarRenaming, SubstTypes),
			assoc_list__from_corresponding_lists(RenamedVarsToSub,
				SubstTypes, SubAL),
			list__foldl(
			    lambda([(TVar - Type)::in, TSubst0::in,
			    		TSubst::out] is det, (
				map__set(TSubst0, TVar, Type, TSubst)
			)), SubAL, TypeSubst0, TypeSubst),

			% Apply the substitution.
			pred_info_arg_types(PredInfo0, Types0),
			pred_info_get_class_context(PredInfo0,
				ClassContext0),
			term__apply_rec_substitution_to_list(Types0,
				TypeSubst, Types),
			apply_rec_subst_to_constraints(TypeSubst,
				ClassContext0, ClassContext),
			SubstOk = yes(TypeSubst),
			ModuleInfo = ModuleInfo0
			}
		    ;
			report_subst_existq_tvars(PredInfo0, Context,
					SubExistQVars),
			io__set_exit_status(1),
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
			{ Types = [] },
			{ ClassContext = constraints([], []) },
			{ SubstOk = no }
		    )
		;
		    report_recursive_subst(PredInfo0, Context,
		    	TVarSet0, RecSubstTVars),
		    io__set_exit_status(1),
		    { module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		    { ExistQVars = [] },
		    { Types = [] },
		    { ClassContext = constraints([], []) },
		    { varset__init(TVarSet) },
		    { SubstOk = no }
		)
	    ;	
		report_unknown_vars_to_subst(PredInfo0, Context,
		    TVarSet0, UnknownVarsToSub),
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		io__set_exit_status(1),
		{ ExistQVars = [] },
		{ Types = [] },
		{ ClassContext = constraints([], []) },
		{ varset__init(TVarSet) },
		{ SubstOk = no }
	    )
	).

:- pred find_duplicate_list_elements(list(T), list(T)).
:- mode find_duplicate_list_elements(in, out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], Vars) :-
	find_duplicate_list_elements(T, Vars0),
	( list__member(H, T) ->
		Vars = [H | Vars0]
	;
		Vars = Vars0
	).

:- pred report_subst_existq_tvars(pred_info, prog_context,
		list(tvar), io__state, io__state).
:- mode report_subst_existq_tvars(in, in, in, di, uo) is det.

report_subst_existq_tvars(PredInfo0, Context, SubExistQVars) -->
	report_pragma_type_spec(PredInfo0, Context),
	prog_out__write_context(Context),
	io__write_string(
		"  error: the substitution includes the existentially\n"),
	prog_out__write_context(Context),
	io__write_string("  quantified type "),
	{ pred_info_typevarset(PredInfo0, TVarSet) },
	report_variables(SubExistQVars, TVarSet),
	io__write_string(".\n").

:- pred report_recursive_subst(pred_info, prog_context, tvarset,
		list(tvar), io__state, io__state).
:- mode report_recursive_subst(in, in, in, in, di, uo) is det.

report_recursive_subst(PredInfo0, Context, TVarSet, RecursiveVars) -->
	report_pragma_type_spec(PredInfo0, Context),
	prog_out__write_context(Context),
	io__write_string("  error: "),
	report_variables(RecursiveVars, TVarSet),
	( { RecursiveVars = [_] } ->
		io__write_string(" occurs\n")
	;
		io__write_string(" occur\n")
	),
	prog_out__write_context(Context),
	io__write_string("  on both sides of the substitution.\n").

:- pred report_multiple_subst_vars(pred_info, prog_context, tvarset,
		list(tvar), io__state, io__state).
:- mode report_multiple_subst_vars(in, in, in, in, di, uo) is det.

report_multiple_subst_vars(PredInfo0, Context, TVarSet, MultiSubstVars) -->
	report_pragma_type_spec(PredInfo0, Context),
	prog_out__write_context(Context),
	io__write_string("  error: "),
	report_variables(MultiSubstVars, TVarSet),
	( { MultiSubstVars = [_] } ->
		io__write_string(" has ")
	;
		io__write_string(" have ")
	),
	io__write_string("multiple replacement types.\n").

:- pred report_unknown_vars_to_subst(pred_info, prog_context, tvarset,
		list(tvar), io__state, io__state).
:- mode report_unknown_vars_to_subst(in, in, in, in, di, uo) is det.

report_unknown_vars_to_subst(PredInfo0, Context, TVarSet, UnknownVars) -->
	report_pragma_type_spec(PredInfo0, Context),
	prog_out__write_context(Context),
	io__write_string("  error: "),
	report_variables(UnknownVars, TVarSet),
	( { UnknownVars = [_] } ->
		io__write_string(" does not ")
	;
		io__write_string(" do not ")
	),
	{ pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc) },
	(
		{ PredOrFunc = predicate },
		{ Decl = "`:- pred'" }
	;
		{ PredOrFunc = function },
		{ Decl = "`:- func'" }
	),
	io__write_string("occur in the "),
	io__write_string(Decl),
	io__write_string(" declaration.\n").

:- pred report_pragma_type_spec(pred_info, term__context,
		io__state, io__state).
:- mode report_pragma_type_spec(in, in, di, uo) is det.

report_pragma_type_spec(PredInfo0, Context) -->
	{ pred_info_module(PredInfo0, Module) },
	{ pred_info_name(PredInfo0, Name) },
	{ pred_info_arity(PredInfo0, Arity) },
	{ pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc) },
	prog_out__write_context(Context),
	io__write_string("In `:- pragma type_spec' declaration for "),
	hlds_out__write_simple_call_id(PredOrFunc,
		qualified(Module, Name)/Arity),
	io__write_string(":\n").

:- pred report_variables(list(tvar), tvarset, io__state, io__state).
:- mode report_variables(in, in, di, uo) is det.

report_variables(SubExistQVars, VarSet) -->
	( { SubExistQVars = [_] } ->
		io__write_string("variable `")
	;
		io__write_string("variables `")
	),
	mercury_output_vars(SubExistQVars, VarSet, no),
	io__write_string("'").

	% Check that the mode list for a `:- pragma type_spec' declaration
	% specifies a known procedure.
:- pred handle_pragma_type_spec_modes(sym_name, arity,
		prog_context, maybe(list(mode)), list(proc_id),
		proc_table, proc_table, bool, module_info, module_info,
		io__state, io__state).
:- mode handle_pragma_type_spec_modes(in, in, in, in, out, in, out,
		out, in, out, di, uo) is det.

handle_pragma_type_spec_modes(SymName, Arity, Context, MaybeModes, ProcIds,
		Procs0, Procs, ModesOk, ModuleInfo0, ModuleInfo) -->
	( { MaybeModes = yes(Modes) } ->
		{ map__to_assoc_list(Procs0, ExistingProcs) },
		(
			{ get_procedure_matching_argmodes(ExistingProcs,
				Modes, ModuleInfo0, ProcId) }
		->
			{ map__lookup(Procs0, ProcId, ProcInfo) },
			{ map__init(Procs1) },
			{ map__det_insert(Procs1, ProcId, ProcInfo, Procs) },
			{ ProcIds = [ProcId] },
			{ ModesOk = yes },
			{ ModuleInfo = ModuleInfo0 }
		;
			{ ProcIds = [] },
			{ Procs = Procs0 },
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) }, 
			undefined_mode_error(SymName, Arity, Context,
				"`:- pragma type_spec' declaration"),
			{ ModesOk = no }
		)
	;
		{ Procs = Procs0 },
		{ map__keys(Procs, ProcIds) },
		{ ModesOk = yes },
		{ ModuleInfo = ModuleInfo0 }
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_termination_info(pred_or_func, sym_name, list(mode),
		maybe(pragma_arg_size_info), maybe(pragma_termination_info),
		prog_context, module_info, module_info, io__state, io__state).
:- mode add_pragma_termination_info(in, in, in, in, in, in, in, out, di, uo)
		is det.

add_pragma_termination_info(PredOrFunc, SymName, ModeList,
		MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo,
		Context, Module0, Module) -->
	{ module_info_get_predicate_table(Module0, Preds) },
	{ list__length(ModeList, Arity) },
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
				ModeList, Module0, ProcId) }
		->
			{ add_context_to_arg_size_info(MaybePragmaArgSizeInfo,
				Context, MaybeArgSizeInfo) },
			{ add_context_to_termination_info(
				MaybePragmaTerminationInfo, Context,
				MaybeTerminationInfo) },
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
			hlds_out__write_simple_call_id(PredOrFunc,
				SymName/Arity),
			io__write_string(".\n")
		)
	    ;
		prog_out__write_context(Context),
		io__write_string("Error: ambiguous predicate name "),
		hlds_out__write_simple_call_id(PredOrFunc, SymName/Arity),
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

:- pred add_stratified_pred(module_info, string, sym_name, arity,
	term__context, module_info, io__state, io__state).
:- mode add_stratified_pred(in, in, in, in, in, out, di, uo) is det.

add_stratified_pred(Module0, PragmaName, Name, Arity, Context, Module) -->
	{ module_info_get_predicate_table(Module0, PredTable0) },
	(
		{ predicate_table_search_sym_arity(PredTable0, Name, 
			Arity, PredIds) }
	->
		{ module_info_stratified_preds(Module0, StratPredIds0) },
		{ set__insert_list(StratPredIds0, PredIds, StratPredIds) },
		{ module_info_set_stratified_preds(Module0, StratPredIds, 
			Module) }
	;
		{ string__append_list(
			["`:- pragma ", PragmaName, "' declaration"],
			Description) },
		undefined_pred_or_func_error(Name, Arity, Context,
			Description),
		{ module_info_incr_errors(Module0, Module) }
	).

%-----------------------------------------------------------------------------%

	% add_pred_marker(ModuleInfo0, PragmaName, Name, Arity, Status,
	%	Context, Marker, ConflictMarkers, ModuleInfo, IO0, IO)
	%
	% Adds Marker to the marker list of the pred(s) with give Name and
	% Arity, updating the ModuleInfo. If the named pred does not exist,
	% or the pred already has a marker in ConflictMarkers, report
	% an error.
:- pred add_pred_marker(module_info, string, sym_name, arity,
	import_status, prog_context, marker, list(marker), module_info,
	io__state, io__state).
:- mode add_pred_marker(in, in, in, in, in, in, in, in, out, di, uo) is det.

add_pred_marker(Module0, PragmaName, Name, Arity, Status, Context, Marker,
		ConflictMarkers, Module) --> 
	( { marker_must_be_exported(Marker) } ->
		{ MustBeExported = yes }
	;
		{ MustBeExported = no }
	),	
	do_add_pred_marker(Module0, PragmaName, Name, Arity, Status,
		MustBeExported, Context, add_marker_pred_info(Marker),
		Module1, PredIds),
	{ module_info_preds(Module1, Preds) },
	{ pragma_check_markers(Preds, PredIds, ConflictMarkers, Conflict) },
	( { Conflict = yes } ->
		pragma_conflict_error(Name, Arity, Context,
			PragmaName),
		{ module_info_incr_errors(Module1, Module) }
	;
		{ Module = Module1 }
	).

:- pred set_pred_owner(module_info, sym_name, arity, string, import_status,
	prog_context, module_info, io__state, io__state).
:- mode set_pred_owner(in, in, in, in, in, in, out, di, uo) is det.

set_pred_owner(Module0, Name, Arity, Owner, Status, Context, Module) -->
	{ SetOwner =
	    lambda([PredInfo0::in, PredInfo::out] is det, (
		pred_info_set_aditi_owner(PredInfo0, Owner, PredInfo)
	)) },
	{ MarkerMustBeExported = yes },
	do_add_pred_marker(Module0, "owner", Name, Arity, Status,
		MarkerMustBeExported, Context, SetOwner, Module, _).

:- pred add_base_relation_index(module_info, sym_name, arity, index_spec,
	import_status, prog_context, module_info, io__state, io__state).
:- mode add_base_relation_index(in, in, in, in, in, in, out, di, uo) is det.

add_base_relation_index(Module0, Name, Arity, Index, Status,
		Context, Module) -->
	{ AddIndex =
		lambda([PredInfo0::in, PredInfo::out] is det, (
			pred_info_get_indexes(PredInfo0, Indexes0),
			Indexes = [Index | Indexes0],
			pred_info_set_indexes(PredInfo0, Indexes, PredInfo)
		)) },
	{ MarkerMustBeExported = yes }, 
	do_add_pred_marker(Module0, "aditi_index", Name, Arity, Status,
		MarkerMustBeExported, Context, AddIndex, Module, PredIds),
	{ Index = index_spec(_, Attrs) },
	list__foldl(check_index_attribute(Name, Arity, Context), Attrs),
	list__foldl(check_index_attribute_pred(Module, Name,
		Arity, Context, Attrs), PredIds).

	% Check that the index attributes are legal for the predicate's arity.
:- pred check_index_attribute(sym_name, arity, term__context, int,
		io__state, io__state).
:- mode check_index_attribute(in, in, in, in, di, uo) is det.

check_index_attribute(Name, Arity, Context, Attr) -->
	( { Attr > 0, Attr =< Arity } ->
		[]
	;
		prog_out__write_context(Context),
		io__write_string(
			"In `:- pragma aditi_index' declaration for `"),
		prog_out__write_sym_name_and_arity(Name/Arity),
		io__write_string("':\n"),
		prog_out__write_context(Context),
		io__write_string("  attribute "),
		io__write_int(Attr),
		io__write_string(" is out of range.\n"),
		io__set_exit_status(1)
	).

	% Check that a relation with an index specified is a base relation
	% and that the indexed attributes do not include aditi__states.
:- pred check_index_attribute_pred(module_info, sym_name, arity, term__context,
		list(int), pred_id, io__state, io__state).
:- mode check_index_attribute_pred(in, in, in, in, in, in, di, uo) is det.

check_index_attribute_pred(ModuleInfo, Name, Arity, Context, Attrs, PredId) -->
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	{ pred_info_get_markers(PredInfo, Markers) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	( { check_marker(Markers, base_relation) } ->
		[]
	;
		prog_out__write_context(Context),
		io__write_string(
			"Error: `:- pragma aditi_index' declaration"),
		io__nl,
		prog_out__write_context(Context),
		io__write_string("  for "),
		hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
		io__write_string(" without preceding\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  `:- pragma base_relation' declaration.\n"),
		io__set_exit_status(1)
	),

	{ pred_info_arg_types(PredInfo, ArgTypes) },
	{ AttrIsAditiState = 
		lambda([Attr::in] is semidet, (
			list__index0(ArgTypes, Attr, ArgType),
			type_is_aditi_state(ArgType)
		)) },
	{ list__filter(AttrIsAditiState, Attrs, AditiStateAttrs) },	
	
	( { AditiStateAttrs = [AditiStateAttr | _] } ->
		% Indexing on aditi__state attributes is pretty silly,
		% since they're removed by magic.m.
		prog_out__write_context(Context),
		io__write_string(
			"In `:- pragma aditi_index' declaration for "),
		hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
		io__write_string(":\n"),
		prog_out__write_context(Context),
		io__write_string("  attribute "),
		io__write_int(AditiStateAttr),
		io__write_string(" is an aditi__state.\n"),
		io__set_exit_status(1)
	;
		[]
	).

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info = (pred(in, out) is det).

:- pred do_add_pred_marker(module_info, string, sym_name, arity,
	import_status, bool, term__context, add_marker_pred_info,
	module_info, list(pred_id), io__state, io__state).
:- mode do_add_pred_marker(in, in, in, in, in, in, in,
	in(add_marker_pred_info), out, out, di, uo) is det.

do_add_pred_marker(Module0, PragmaName, Name, Arity, Status,
		MustBeExported, Context, UpdatePredInfo, Module, PredIds) --> 
	( { get_matching_pred_ids(Module0, Name, Arity, PredIds0) } ->
		{ PredIds = PredIds0 },
		{ module_info_get_predicate_table(Module0, PredTable0) },
		{ predicate_table_get_preds(PredTable0, Preds0) },

		{ pragma_add_marker(Preds0, PredIds, UpdatePredInfo,
			Status, MustBeExported, Preds, WrongStatus) },
		(
			{ WrongStatus = yes }
		->
			pragma_status_error(Name, Arity, Context, PragmaName),
			{ module_info_incr_errors(Module0, Module1) }
		;
			{ Module1 = Module0 }
		),
			
		{ predicate_table_set_preds(PredTable0, Preds, 
			PredTable) },
		{ module_info_set_predicate_table(Module1, PredTable, 
			Module) }
	;
		{ PredIds = [] },
		{ string__append_list(
			["`:- pragma ", PragmaName, "' declaration"],
			Description) },
		undefined_pred_or_func_error(Name, Arity, Context,
			Description),
		{ module_info_incr_errors(Module0, Module) }
	).
	
:- pred get_matching_pred_ids(module_info, sym_name, arity, list(pred_id)).
:- mode get_matching_pred_ids(in, in, in, out) is semidet.
		
get_matching_pred_ids(Module0, Name, Arity, PredIds) :-
	module_info_get_predicate_table(Module0, PredTable0),
	% check that the pragma is module qualified.
	(
		Name = unqualified(_)
	->
		error("get_matching_pred_ids: unqualified name")
	;
		predicate_table_search_sym_arity(PredTable0, Name, 
			Arity, PredIds)
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
			Context, "`:- external' declaration"),
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

:- pred module_add_inst_defn(module_info, inst_varset, sym_name, list(inst_var),
		inst_defn, condition, prog_context, item_status, 
		module_info, io__state, io__state).
:- mode module_add_inst_defn(in, in, in, in, in, in, in, in,
		out, di, uo) is det.

module_add_inst_defn(Module0, VarSet, Name, Args, InstDefn, Cond,
		Context, item_status(Status, _NeedQual), Module) -->
	{ module_info_insts(Module0, InstTable0) },
	{ inst_table_get_user_insts(InstTable0, Insts0) },
	insts_add(Insts0, VarSet, Name, Args, InstDefn, Cond,
		Context, Status, Insts),
	{ inst_table_set_user_insts(InstTable0, Insts, InstTable) },
	{ module_info_set_insts(Module0, InstTable, Module) }.

:- pred insts_add(user_inst_table, inst_varset, sym_name, list(inst_var),
		inst_defn, condition, prog_context, import_status,
		user_inst_table, io__state, io__state).
:- mode insts_add(in, in, in, in, in, in, in, in, out, di, uo) is det.

	% XXX handle abstract insts
insts_add(_, _, _, _, abstract_inst, _, _, _, _) -->
	{ error("sorry, abstract insts not implemented") }.
insts_add(Insts0, VarSet, Name, Args, eqv_inst(Body),
			Cond, Context, Status, Insts) -->
	{ list__length(Args, Arity) },
	(
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

:- pred module_add_mode_defn(module_info, inst_varset, sym_name,
		list(inst_var), mode_defn, condition, prog_context,
		item_status, module_info, io__state, io__state).
:- mode module_add_mode_defn(in, in, in, in, in, in, in,
		in, out, di, uo) is det.

module_add_mode_defn(Module0, VarSet, Name, Params, ModeDefn, Cond,
		Context, item_status(Status, _NeedQual), Module) -->
	{ module_info_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, Name, Params, ModeDefn,
		Cond, Context, Status, Modes),
	{ module_info_set_modes(Module0, Modes, Module) }.

:- pred modes_add(mode_table, inst_varset, sym_name, list(inst_var),
		mode_defn, condition, prog_context, import_status,
		mode_table, io__state, io__state).
:- mode modes_add(in, in, in, in, in, in, in, in, out, di, uo) is det.

modes_add(Modes0, VarSet, Name, Args, eqv_mode(Body),
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

%-----------------------------------------------------------------------------%

	% We allow more than one "definition" for a given type so
	% long all of them except one are actually just declarations,
	% e.g. `:- type t.', which is parsed as an type definition for
	% t which defines t as an abstract_type.

:- pred module_add_type_defn(module_info, tvarset, sym_name, list(type_param),
		type_defn, condition, prog_context, item_status,
		module_info, io__state, io__state).
:- mode module_add_type_defn(in, in, in, in, in,
		in, in, in, out, di, uo) is det.

module_add_type_defn(Module0, TVarSet, Name, Args, TypeDefn, Cond, Context,
		item_status(Status0, NeedQual), Module) -->
	globals__io_get_globals(Globals),
	{ list__length(Args, Arity) },
	{ TypeId = Name - Arity },
	{ convert_type_defn(TypeDefn, TypeId, Globals, Body) },
	module_add_type_defn_2(Module0, TVarSet, Name, Args, Body, Cond,
			Context, item_status(Status0, NeedQual), Module).

:- pred module_add_type_defn_2(module_info, tvarset, sym_name, list(type_param),
		hlds_type_body, condition, prog_context, item_status,
		module_info, io__state, io__state).
:- mode module_add_type_defn_2(in, in, in, in, in,
		in, in, in, out, di, uo) is det.

module_add_type_defn_2(Module0, TVarSet, Name, Args, Body, _Cond, Context,
		item_status(Status0, NeedQual), Module) -->
	{ module_info_types(Module0, Types0) },
	globals__io_get_globals(Globals),
	{ list__length(Args, Arity) },
	{ TypeId = Name - Arity },
	{ Body = abstract_type ->
		make_status_abstract(Status0, Status1)
	;
		Status1 = Status0
	},
	{ 
		% the type is exported if *any* occurrence is exported,
		% even a previous abstract occurrence
		map__search(Types0, TypeId, OldDefn)
	->
		hlds_data__get_type_defn_status(OldDefn, OldStatus),
		combine_status(Status1, OldStatus, Status),
		MaybeOldDefn = yes(OldDefn)
	;
		MaybeOldDefn = no,
		Status = Status1 
	},
	{ hlds_data__set_type_defn(TVarSet, Args, Body, Status, Context, T) },
	(
		% if there was an existing non-abstract definition for the type
		{ MaybeOldDefn = yes(T2) },
		{ hlds_data__get_type_defn_tvarset(T2, TVarSet_2) },
		{ hlds_data__get_type_defn_tparams(T2, Params_2) },
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
				hlds_data__set_type_defn(TVarSet_2, Params_2,
					Body_2, Status, OrigContext, T3),
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
		{ construct_type(TypeId, Args, Type) },
		(
			{ Body = du_type(ConsList, _, _, _) }
		->
			{ module_info_ctors(Module0, Ctors0) },
			{ module_info_get_partial_qualifier_info(Module0,
				PQInfo) },
			{ module_info_ctor_field_table(Module0,
				CtorFields0) },
			ctors_add(ConsList, TypeId, TVarSet, NeedQual, PQInfo,
				Context, Status, CtorFields0, CtorFields,
				Ctors0, Ctors),
			{ module_info_set_ctors(Module0, Ctors, Module1) },
			{ module_info_set_ctor_field_table(Module1,
				CtorFields, Module1a) },
			{
				type_constructors_should_be_no_tag(ConsList, 
					Globals, Name, CtorArgType, _)
			->
				NoTagType = no_tag_type(Args,
					Name, CtorArgType),
				module_info_no_tag_types(Module1a,
					NoTagTypes0),
				map__set(NoTagTypes0, TypeId, NoTagType,
					NoTagTypes),
				module_info_set_no_tag_types(Module1a,
					NoTagTypes, Module2)
			;
				Module2 = Module1a
			}
		;
			{ Module2 = Module0 }
		),
		{ add_special_preds(Module2, TVarSet, Type, TypeId,
			Body, Context, Status, Module3) },
		{ module_info_set_types(Module3, Types, Module) },
		( { Body = uu_type(_) } ->
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string("Error in type declaration: \n"),
			prog_out__write_context(Context),
			io__write_string(
	"  the syntax for type equivalence is `:- type t1 == t2'.\n"),
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

:- pred make_status_abstract(import_status, import_status).
:- mode make_status_abstract(in, out) is det.

make_status_abstract(Status, AbstractStatus) :-
	( Status = exported ->
		AbstractStatus = abstract_exported
	; Status = imported(_) ->
		AbstractStatus = abstract_imported
	;
		AbstractStatus = Status
	).

:- pred combine_status(import_status, import_status, import_status).
:- mode combine_status(in, in, out) is det.

combine_status(StatusA, StatusB, Status) :-
	( combine_status_2(StatusA, StatusB, CombinedStatus) ->
		Status = CombinedStatus
	;
		error("unexpected status for type definition")
	).

:- pred combine_status_2(import_status, import_status, import_status).
:- mode combine_status_2(in, in, out) is semidet.

combine_status_2(imported(_), Status2, Status) :-
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

combine_status_imported(imported(Section),	imported(Section)).
combine_status_imported(local,			imported(implementation)).
combine_status_imported(exported,		exported).
combine_status_imported(opt_imported,		opt_imported).
combine_status_imported(abstract_imported,	imported(interface)).
combine_status_imported(abstract_exported,	abstract_exported).

:- pred combine_status_local(import_status, import_status).
:- mode combine_status_local(in, out) is semidet.

combine_status_local(imported(_),	local).
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
	( Status2 = imported(Section) ->
		Status = imported(Section)
	;
		Status = abstract_imported
	).

:- pred convert_type_defn(type_defn, type_id, globals, hlds_type_body).
:- mode convert_type_defn(in, in, in, out) is det.

convert_type_defn(du_type(Body, EqualityPred), TypeId, Globals,
		du_type(Body, CtorTags, IsEnum, EqualityPred)) :-
	assign_constructor_tags(Body, TypeId, Globals, CtorTags, IsEnum).
convert_type_defn(uu_type(Body), _, _, uu_type(Body)).
convert_type_defn(eqv_type(Body), _, _, eqv_type(Body)).
convert_type_defn(abstract_type, _, _, abstract_type).

:- pred ctors_add(list(constructor), type_id, tvarset, need_qualifier,
		partial_qualifier_info, prog_context, import_status,
		ctor_field_table, ctor_field_table,
		cons_table, cons_table, io__state, io__state).
:- mode ctors_add(in, in, in, in, in, in, in, in, out, in, out, di, uo) is det.

ctors_add([], _, _, _, _, _, _, FieldNameTable, FieldNameTable,
		Ctors, Ctors) --> [].
ctors_add([Ctor | Rest], TypeId, TVarSet, NeedQual, PQInfo, Context,
		ImportStatus, FieldNameTable0, FieldNameTable,
		Ctors0, Ctors) -->
	{ Ctor = ctor(ExistQVars, Constraints, Name, Args) },
	{ make_cons_id(Name, Args, TypeId, QualifiedConsId) },
	{ ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Args, TypeId,
				Context) },
	%
	% Insert the fully-qualified version of this cons_id into the
	% cons_table.
	% Also check that there is at most one definition of a given
	% cons_id in each type.
	%
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

	( { QualifiedConsId = cons(qualified(Module, ConsName), Arity) } ->
		% Add unqualified version of the cons_id to the
		% cons_table, if appropriate.
		{
			NeedQual = may_be_unqualified
		->
			UnqualifiedConsId = cons(unqualified(ConsName), Arity),
			multi_map__set(Ctors1, UnqualifiedConsId, ConsDefn,
				Ctors2)
		;
			Ctors2 = Ctors1
		},

		% Add partially qualified versions of the cons_id
		{ get_partial_qualifiers(Module, PQInfo, PartialQuals) },
		{ list__map_foldl(add_ctor(ConsName, Arity, ConsDefn),
			PartialQuals, _PartiallyQualifiedConsIds,
			Ctors2, Ctors3) },

		{ assoc_list__keys(Args, FieldNames) },
		{ FirstField = 1 },
		
		add_ctor_field_names(FieldNames, NeedQual, PartialQuals,
			TypeId, QualifiedConsId, Context, ImportStatus,
			FirstField, FieldNameTable0, FieldNameTable1)
	;
		{ error("ctors_add: cons_id not qualified") }
	),

	ctors_add(Rest, TypeId, TVarSet, NeedQual, PQInfo, Context,
		ImportStatus, FieldNameTable1, FieldNameTable, Ctors3, Ctors).

:- pred add_ctor(string, int, hlds_cons_defn, module_name,
		cons_id, cons_table, cons_table).
:- mode add_ctor(in, in, in, in, out, in, out) is det.

add_ctor(ConsName, Arity, ConsDefn, ModuleQual, ConsId, CtorsIn, CtorsOut) :-
	ConsId = cons(qualified(ModuleQual, ConsName), Arity),
	multi_map__set(CtorsIn, ConsId, ConsDefn, CtorsOut).

:- pred add_ctor_field_names(list(maybe(ctor_field_name)),
		need_qualifier, list(module_name), type_id, cons_id,
		prog_context, import_status, int, ctor_field_table,
		ctor_field_table, io__state, io__state).
:- mode add_ctor_field_names(in, in, in, in, in, in, in, in,
		in, out, di, uo) is det.

add_ctor_field_names([], _, _, _, _, _, _, _,
		FieldNameTable, FieldNameTable) --> [].
add_ctor_field_names([MaybeFieldName | FieldNames], NeedQual,
		PartialQuals, TypeId, ConsId, Context, ImportStatus,
		FieldNumber, FieldNameTable0, FieldNameTable) -->
	(
		{ MaybeFieldName = yes(FieldName) },
		{ FieldDefn = hlds_ctor_field_defn(Context, ImportStatus,
			TypeId, ConsId, FieldNumber) },
		add_ctor_field_name(FieldName, FieldDefn, NeedQual,
			PartialQuals, FieldNameTable0, FieldNameTable2)
	;
		{ MaybeFieldName = no },
		{ FieldNameTable2 = FieldNameTable0 }
	),
	add_ctor_field_names(FieldNames, NeedQual, PartialQuals, TypeId,
		ConsId, Context, ImportStatus, FieldNumber + 1,
		FieldNameTable2, FieldNameTable).

:- pred add_ctor_field_name(ctor_field_name, hlds_ctor_field_defn,
		need_qualifier, list(module_name), ctor_field_table,
		ctor_field_table, io__state, io__state).
:- mode add_ctor_field_name(in, in, in, in, in, out, di, uo) is det.

add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
		FieldNameTable0, FieldNameTable) -->
	{ FieldName = qualified(FieldModule0, _) ->
		FieldModule = FieldModule0
	;
		error("add_ctor_field_name: unqualified field name")
	},
	(
		%
		% Field names must be unique within a module, not
		% just within a type because the function names for
		% user-defined override functions for the builtin field
		% access functions must be unique within a module.
		%
		{ map__search(FieldNameTable0, FieldName, ConflictingDefns) }
	->
		{ ConflictingDefns = [ConflictingDefn] ->
			ConflictingDefn =
				hlds_ctor_field_defn(OrigContext, _, _, _, _)
		;
			error(
			"add_ctor_field_name: multiple conflicting fields")
		},

		% XXX we should record each error
		% using module_info_incr_errors
		{ FieldDefn = hlds_ctor_field_defn(Context, _, _, _, _) },
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		{ prog_out__sym_name_to_string(FieldName, FieldString) },
		{ ErrorPieces = [
			words("Error: field"),
			fixed(string__append_list(["`", FieldString, "'"])),
			words("multiply defined.")
		] },
		error_util__write_error_pieces(Context, 0, ErrorPieces),

		% This type of error doesn't fit well with
		% how error_util does things -- error_util.m
		% wants to write everything with a single context.
		prog_out__write_context(OrigContext),
		io__write_string(
			"  Here is the previous definition of field `"),
		io__write_string(FieldString),
		io__write_string("'.\n"),
		io__set_exit_status(1),
		io__set_output_stream(OldStream, _),
		{ FieldNameTable = FieldNameTable0 }
	;
		{ unqualify_name(FieldName, UnqualFieldName) },

		% Add an unqualified version of the field name to the
		% table, if appropriate.
		{
			NeedQual = may_be_unqualified
		->
			multi_map__set(FieldNameTable0,
				unqualified(UnqualFieldName),
				FieldDefn, FieldNameTable1)
		;
			FieldNameTable1 = FieldNameTable0
		},

		% Add partially qualified versions of the cons_id
		{ list__foldl(
			do_add_ctor_field(UnqualFieldName, FieldDefn),
			[FieldModule | PartialQuals],
			FieldNameTable1, FieldNameTable) }
	).

:- pred do_add_ctor_field(string, hlds_ctor_field_defn, module_name,
		ctor_field_table, ctor_field_table).
:- mode do_add_ctor_field(in, in, in, in, out) is det.

do_add_ctor_field(FieldName, FieldNameDefn, ModuleName,
		FieldNameTable0, FieldNameTable) :-
	multi_map__set(FieldNameTable0, qualified(ModuleName, FieldName),
		FieldNameDefn, FieldNameTable).

%-----------------------------------------------------------------------------%

:- pred module_add_pred_or_func(module_info, tvarset, inst_varset,
		existq_tvars, pred_or_func, sym_name, list(type_and_mode),
		maybe(determinism), condition, purity, class_constraints,
		pred_markers, prog_context, item_status,
		maybe(pair(pred_id, proc_id)), module_info, 
		io__state, io__state).
:- mode module_add_pred_or_func(in, in, in, in, in, in, in, in, in, in, in, in,
		in, in, out, out, di, uo) is det.

module_add_pred_or_func(Module0, TypeVarSet, InstVarSet, ExistQVars,
		PredOrFunc, PredName, TypesAndModes, MaybeDet, Cond, Purity,
		ClassContext, Markers, Context, item_status(Status, NeedQual),
		MaybePredProcId, Module) -->
	% Only preds with opt_imported clauses are tagged as opt_imported, so
	% that the compiler doesn't look for clauses for other preds read in
	% from optimization interfaces.
	{ Status = opt_imported ->
		DeclStatus = imported(interface)
	;
		DeclStatus = Status
	},
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes0) },
	add_new_pred(Module0, TypeVarSet, ExistQVars, PredName, Types, Cond,
		Purity, ClassContext, Markers, Context, DeclStatus, NeedQual, 
		PredOrFunc, Module1),
	{
	    	PredOrFunc = predicate,
		MaybeModes0 = yes(Modes0),

		% For predicates with no arguments, if the determinism
		% is not declared a mode is not added. The determinism
		% can be specified by a separate mode declaration.
		Modes0 = [],
		MaybeDet = no
	->
		MaybeModes = no
	;
		% Assume that a function with no modes but with a determinism
		% declared has the default modes.
		PredOrFunc = function,
		MaybeModes0 = no,
		MaybeDet = yes(_)
	->
		list__length(Types, Arity),
		adjust_func_arity(function, FuncArity, Arity),
		in_mode(InMode),
		list__duplicate(FuncArity, InMode, InModes),
		out_mode(OutMode),
		list__append(InModes, [OutMode], ArgModes),
		MaybeModes = yes(ArgModes)
	;
		MaybeModes = MaybeModes0
	},

	(
		{ MaybeModes = yes(Modes) },
		{ check_marker(Markers, class_method) ->
			IsClassMethod = yes
		;
			IsClassMethod = no
		},
		module_add_mode(Module1, InstVarSet, PredName, Modes, MaybeDet,
			Cond, Status, Context, PredOrFunc, IsClassMethod,
			PredProcId, Module),
		{ MaybePredProcId = yes(PredProcId) }
	;
		{ MaybeModes = no },
		{ Module = Module1 },
		{ MaybePredProcId = no }
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
	{ Status = item_status(ImportStatus0, _) },
	{ Interface = abstract ->
		make_status_abstract(ImportStatus0, ImportStatus1)
	;
		ImportStatus1 = ImportStatus0
	},
	( 
		% the typeclass is exported if *any* occurrence is exported,
		% even a previous abstract occurrence
		{ map__search(Classes0, ClassId, OldDefn) }
	->
		{ OldDefn = hlds_class_defn(OldStatus, OldConstraints,
				OldVars, OldInterface, OldMethods,
				OldVarSet, OldContext) },	
		{ combine_status(ImportStatus1, OldStatus, ImportStatus) },
		{
			OldInterface = concrete(_),
			ClassMethods0 = OldMethods,
			ClassInterface = OldInterface
		;
			OldInterface = abstract,
			ClassMethods0 = [],
			ClassInterface = Interface
		},
		(
			\+ { superclass_constraints_are_identical(OldVars,
				OldVarSet, OldConstraints, Vars, VarSet,
				Constraints) }
		->
			multiple_def_error(Name, ClassArity,
				"typeclass", Context, OldContext),
			prog_out__write_context(Context),
			io__write_string(
			"  The superclass constraints do not match.\n"),
			io__set_exit_status(1),
			{ FoundError = yes }
		;
			{ Interface = concrete(_) },
			{ OldInterface = concrete(_) }
		->
			{ FoundError = yes },
			( { ImportStatus = opt_imported } ->
				[]
			;
				multiple_def_error(Name, ClassArity,
					"typeclass", Context, OldContext),
				io__set_exit_status(1)
			)
		;
			{ FoundError = no }
		),

		{ IsNewDefn = no }
	;
		{ IsNewDefn = yes `with_type` bool },
		{ FoundError = no `with_type` bool },
		{ ClassMethods0 = [] },
		{ ClassInterface = Interface },
		{ ImportStatus = ImportStatus1 }
	),
	( { FoundError = no } ->
		(
			{ Interface = concrete(Methods) },
			module_add_class_interface(Module0, Name, Vars,
				Methods, Status, PredProcIds0, Module1),
				% Get rid of the `no's from the list of maybes
			{ IsYes =
			    (pred(Maybe::in, PredProcId::out) is semidet :-
				(
					Maybe = yes(Pred - Proc),
					PredProcId = hlds_class_proc(Pred, Proc)
			    )) },
			{ list__filter_map(IsYes, PredProcIds0, PredProcIds1) },

				%
				% The list must be sorted on pred_id and then
				% proc_id -- check_typeclass.m assumes this
				% when it is generating the corresponding list
				% of pred_proc_ids for instance definitions.
				%
			{ list__sort(PredProcIds1, ClassMethods) }
		;
			{ Interface = abstract },
			{ ClassMethods = ClassMethods0 },
			{ Module1 = Module0 }
		),

		{ Defn = hlds_class_defn(ImportStatus, Constraints, Vars,
			ClassInterface, ClassMethods, VarSet, Context) },
		{ map__set(Classes0, ClassId, Defn, Classes) },
		{ module_info_set_classes(Module1, Classes, Module2) },

		( { IsNewDefn = yes } ->
				% insert an entry into the super class table
				% for each super class of this class
			{ AddSuper =
			    (pred(Super::in, Ss0::in, Ss::out) is det :-
				(
					Super = constraint(SuperName,
						SuperTypes),
					list__length(SuperTypes,
						SuperClassArity),
					term__vars_list(SuperTypes, SuperVars),
					SuperClassId = class_id(SuperName,
						SuperClassArity),
					SubClassDetails =
					    subclass_details(SuperVars,
						ClassId, Vars, VarSet),
					multi_map__set(Ss0, SuperClassId,
						SubClassDetails, Ss)
				)
			    ) },
			{ list__foldl(AddSuper, Constraints, 
				SuperClasses0, SuperClasses) },

			{ module_info_set_superclasses(Module2, 
				SuperClasses, Module3) },

				% When we find the class declaration, make an
				% entry for the instances.
			{ module_info_instances(Module3, Instances0) },
			{ map__det_insert(Instances0, ClassId, [], Instances) },
			{ module_info_set_instances(Module3,
				Instances, Module) }
		;
			{ Module = Module2 }
		)
	;
		{ Module = Module0 }
	).

:- pred superclass_constraints_are_identical(list(tvar), tvarset,
	list(class_constraint), list(tvar), tvarset, list(class_constraint)).
:- mode superclass_constraints_are_identical(in, in,
	in, in, in, in) is semidet.

superclass_constraints_are_identical(OldVars0, OldVarSet, OldConstraints0,
		Vars, VarSet, Constraints) :-
	varset__merge_subst(VarSet, OldVarSet, _, Subst),
	apply_subst_to_constraint_list(Subst,
		OldConstraints0, OldConstraints1),
	OldVars = term__term_list_to_var_list(
		map__apply_to_list(OldVars0, Subst)),

	map__from_corresponding_lists(OldVars, Vars, VarRenaming),
	apply_variable_renaming_to_constraint_list(VarRenaming,
		OldConstraints1, OldConstraints),
	OldConstraints = Constraints.
	
:- pred module_add_class_interface(module_info, sym_name, list(tvar),
	list(class_method), item_status, list(maybe(pair(pred_id, proc_id))), 
	module_info, io__state, io__state).
:- mode module_add_class_interface(in, in, in, in, in, out, out, di, uo) is det.

module_add_class_interface(Module0, Name, Vars, Methods, Status, PredProcIds, 
		Module) -->
	module_add_class_interface_2(Module0, Name, Vars, Methods, Status,
		PredProcIds0, Module1),
	check_method_modes(Methods, PredProcIds0,
		PredProcIds, Module1, Module).

:- pred module_add_class_interface_2(module_info, sym_name, list(tvar),
	list(class_method), item_status, list(maybe(pair(pred_id, proc_id))), 
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
		{ Method = pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
			PredOrFunc, PredName, TypesAndModes, MaybeDet,
			Cond, Purity, ClassContext, Context) },
		{ term__var_list_to_term_list(Vars, VarTerms) },
		{ ClassContext = constraints(UnivCnstrs, ExistCnstrs) },
		{ NewUnivCnstrs = [constraint(Name, VarTerms) | UnivCnstrs] },
		{ NewClassContext = constraints(NewUnivCnstrs, ExistCnstrs) },
		{ init_markers(Markers0) },
		{ add_marker(Markers0, class_method, Markers) },
		module_add_pred_or_func(Module0, TypeVarSet, InstVarSet,
			ExistQVars, PredOrFunc, PredName, TypesAndModes,
			MaybeDet, Cond, Purity, NewClassContext, Markers,
			Context, Status, MaybePredIdProcId, Module)
	;
		{ Method = pred_or_func_mode(VarSet, PredOrFunc, PredName,
			Modes, MaybeDet, Cond, Context) },
		{ Status = item_status(ImportStatus, _) },
		{ IsClassMethod = yes },
		module_add_mode(Module0, VarSet, PredName, Modes, MaybeDet,
			Cond, ImportStatus, Context, PredOrFunc,
			IsClassMethod, PredIdProcId, Module),
		{ MaybePredIdProcId = yes(PredIdProcId) }
	).

	% Go through the list of class methods, looking for
	% - functions without mode declarations: add a default mode
	% - predicates without mode declarations: report an error
	% - mode declarations with no determinism: report an error
:- pred check_method_modes(list(class_method), 
	list(maybe(pair(pred_id, proc_id))), 
	list(maybe(pair(pred_id, proc_id))), module_info, module_info,
	io__state, io__state).
:- mode check_method_modes(in, in, out, in, out, di, uo) is det.

check_method_modes([], PredProcIds, PredProcIds, Module, Module) --> [].
check_method_modes([M|Ms], PredProcIds0, PredProcIds, Module0, Module) -->
	(
		{ M = pred_or_func(_, _, _, PorF, QualName, TypesAndModes,
			_, _, _, _, _) }
	->
		{ QualName = qualified(ModuleName0, Name0) ->
			ModuleName = ModuleName0,
			Name = Name0
		;
			% The class interface should be fully module qualified
			% by prog_io.m at the time it is read in.
			error(
		       "add_default_class_method_func_modes: unqualified func")
		},

		{ list__length(TypesAndModes, PredArity) },
		{ module_info_get_predicate_table(Module0, PredTable) },
		(
			{ predicate_table_search_pf_m_n_a(PredTable, PorF,
				ModuleName, Name, PredArity, [PredId]) }
		->
			{ module_info_pred_info(Module0, PredId, PredInfo0) },
			(
				{ PorF = function },
				{ maybe_add_default_func_mode(PredInfo0,
					PredInfo, MaybeProc) },
				{
					MaybeProc = no,
					PredProcIds1 = PredProcIds0,
					Module1 = Module0
				;
					MaybeProc = yes(ProcId),
					NewPredProc = yes(PredId - ProcId),
					PredProcIds1 = [NewPredProc |
						PredProcIds0],
					module_info_set_pred_info(Module0,
						PredId, PredInfo, Module1)
				}
			;
				{ PorF = predicate },
				{ pred_info_procedures(PredInfo0, Procs) },
				( { map__is_empty(Procs) } ->
					pred_method_with_no_modes_error(
						PredInfo0)
				;
					[]
				),
				{ Module1 = Module0 },
				{ PredProcIds1 = PredProcIds0 }
			)
		;
			{ error("handle_methods_with_no_modes") }
		)
	;
		{ PredProcIds1 = PredProcIds0 },
		{ Module1 = Module0 }
	),
	check_method_modes(Ms, PredProcIds1, PredProcIds, Module1, Module).

:- pred module_add_instance_defn(module_info, module_name,
		list(class_constraint), sym_name, list(type), instance_body,
		tvarset, import_status, prog_context, module_info,
		io__state, io__state).
:- mode module_add_instance_defn(in, in, in, in, in, in, in, in, in, out, 
		di, uo) is det.

module_add_instance_defn(Module0, InstanceModuleName, Constraints, ClassName,
		Types, Body, VarSet, Status, Context, Module) -->
	{ module_info_classes(Module0, Classes) },
	{ module_info_instances(Module0, Instances0) },
	{ list__length(Types, ClassArity) },
	{ ClassId = class_id(ClassName, ClassArity) },
	(
		{ map__search(Classes, ClassId, _) }
	->
		{ map__init(Empty) },
		{ NewInstanceDefn = hlds_instance_defn(InstanceModuleName,
			Status, Context, Constraints, Types, Body, no,
			VarSet, Empty) },
		{ map__lookup(Instances0, ClassId, InstanceDefns) },
		check_for_overlapping_instances(NewInstanceDefn, InstanceDefns,
			ClassId),
		{ map__det_update(Instances0, ClassId,
			[NewInstanceDefn | InstanceDefns], Instances) },
		{ module_info_set_instances(Module0, Instances, Module) }
	;
		undefined_type_class_error(ClassName, ClassArity, Context,
			"instance declaration"),
		{ Module = Module0 }
	).

:- pred check_for_overlapping_instances(hlds_instance_defn,
		list(hlds_instance_defn), class_id, io__state, io__state).
:- mode check_for_overlapping_instances(in, in, in, di, uo) is det.

check_for_overlapping_instances(NewInstanceDefn, InstanceDefns, ClassId) -->
	{ IsOverlapping = lambda([(Context - OtherContext)::out] is nondet, (
		NewInstanceDefn = hlds_instance_defn(_, _Status, Context,
				_, Types, Body, _, VarSet, _),
		Body \= abstract, % XXX
		list__member(OtherInstanceDefn, InstanceDefns),
		OtherInstanceDefn = hlds_instance_defn(_, _OtherStatus,
				OtherContext, _, OtherTypes, OtherBody,
				_, OtherVarSet, _),
		OtherBody \= abstract, % XXX
		varset__merge(VarSet, OtherVarSet, OtherTypes,
				_NewVarSet, NewOtherTypes),
		type_list_subsumes(Types, NewOtherTypes, _)
	)) },
	aggregate(IsOverlapping,
		report_overlapping_instance_declaration(ClassId)).

:- pred report_overlapping_instance_declaration(class_id, pair(prog_context),
		io__state, io__state).
:- mode report_overlapping_instance_declaration(in, in, di, uo) is det.

report_overlapping_instance_declaration(class_id(ClassName, ClassArity),
		Context - OtherContext) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: multiply defined (or overlapping) instance\n"),
	prog_out__write_context(Context),
	io__write_string("declarations for class `"),
        prog_out__write_sym_name(ClassName),
	io__write_string("/"),
	io__write_int(ClassArity),
	io__write_string("'.\n"),
	prog_out__write_context(OtherContext),
	io__write_string("Previous instance declaration was here.\n").

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
	check_tvars_in_constraints(ClassContext, Types, TVarSet,
		PredOrFunc, PredName, Context, Module0, Module1),

	{ module_info_name(Module1, ModuleName) },
	{ list__length(Types, Arity) },
	(
		{ PredName = unqualified(_PName) },
		{ module_info_incr_errors(Module1, Module) },
		unqualified_pred_error(PredName, Arity, Context)
		% All predicate names passed into this predicate should have 
		% been qualified by prog_io.m, when they were first read.
	;
		{ PredName = qualified(MNameOfPred, PName) },
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
		globals__io_lookup_string_option(aditi_user, Owner),
		{ pred_info_init(ModuleName, PredName, Arity, TVarSet,
				ExistQVars, Types,
				Cond, Context, ClausesInfo, Status, Markers,
				none, PredOrFunc, ClassContext, Proofs,
				Owner, PredInfo0) },
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
				{ adjust_func_arity(PredOrFunc,
					OrigArity, Arity) },
				multiple_def_error(PredName, OrigArity,
					DeclString, Context, OrigContext)
			;
				% This can happen for exported external preds.
				{ Module = Module0 }
			)
		;
			{ module_info_get_partial_qualifier_info(Module1,
				PQInfo) },
			{ predicate_table_insert(PredicateTable0, PredInfo0, 
				NeedQual, PQInfo, PredId, PredicateTable1) },
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

	%
	% check for type variables which occur in the the class constraints,
	% but which don't occur in the predicate argument types
	%
:- pred check_tvars_in_constraints(class_constraints, list(type), tvarset,
		pred_or_func, sym_name, prog_context,
		module_info, module_info, io__state, io__state).
:- mode check_tvars_in_constraints(in, in, in, in, in, in,
		in, out, di, uo) is det.

check_tvars_in_constraints(ClassContext, ArgTypes, TVarSet,
		PredOrFunc, PredName, Context, Module0, Module) -->
	{ solutions(constrained_tvar_not_in_arg_types(ClassContext, ArgTypes),
		UnboundTVars) },
	( { UnboundTVars = [] } ->
		{ Module = Module0 }
	;
		{ module_info_incr_errors(Module0, Module) },
		report_unbound_tvars_in_class_context(UnboundTVars, ArgTypes,
			TVarSet, PredOrFunc, PredName, Context)
	).

:- pred constrained_tvar_not_in_arg_types(class_constraints, list(type), tvar).
:- mode constrained_tvar_not_in_arg_types(in, in, out) is nondet.

constrained_tvar_not_in_arg_types(ClassContext, ArgTypes, TVar) :-
	ClassContext = constraints(UnivCs, ExistCs),
	( Constraints = UnivCs ; Constraints = ExistCs ),
	type_util__constraint_list_get_tvars(Constraints, TVars),
	list__member(TVar, TVars),
	\+ term__contains_var_list(ArgTypes, TVar).

:- pred report_unbound_tvars_in_class_context(list(tvar), list(type), tvarset,
		pred_or_func, sym_name, prog_context,
		io__state, io__state).
:- mode report_unbound_tvars_in_class_context(in, in, in, in, in, in,
		di, uo) is det.

/*
The error message is intended to look like this:

very_long_module_name:001: In declaration for function `very_long_function/2':
very_long_module_name:001:   error in type class constraints: type variables
very_long_module_name:001:   `T1, T2, T3' occur only in the constraints,
very_long_module_name:001:   not in the function's argument or result types.

very_long_module_name:002: In declaration for predicate `very_long_predicate/3':
very_long_module_name:002:   error in type class constraints: type variable
very_long_module_name:002:   `T' occurs only in the constraints,
very_long_module_name:002:   not in the predicate's argument types.
*/

report_unbound_tvars_in_class_context(UnboundTVars, ArgTypes, TVarSet,
		PredOrFunc, PredName, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("In declaration for "),
	{ list__length(ArgTypes, Arity) },
	hlds_out__write_simple_call_id(PredOrFunc, PredName, Arity),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error in type class constraints: "),
	(
		{ UnboundTVars = [] },
		{ error("report_unbound_tvars_in_class_context: no type vars") }
	;
		{ UnboundTVars = [SingleTVar] },
		io__write_string("type variable\n"),
		prog_out__write_context(Context),
		io__write_string("  `"),
		mercury_output_var(SingleTVar, TVarSet, no),
		io__write_string("' occurs ")
	;
		{ UnboundTVars = [_, _ | _] },
		io__write_string("type variables\n"),
		prog_out__write_context(Context),
		io__write_string("  `"),
		mercury_output_vars(UnboundTVars, TVarSet, no),
		io__write_string("' occur ")
	),
	io__write_string("only in the constraints,\n"),
	prog_out__write_context(Context),
	io__write_string("  not in the "),
	(
		{ PredOrFunc = predicate },
		io__write_string("predicate's argument types.\n")
	;
		{ PredOrFunc = function },
		io__write_string("function's argument or result types.\n")
	).

%-----------------------------------------------------------------------------%
	
:- pred maybe_check_field_access_function(sym_name, arity, import_status,
		prog_context, module_info, io__state, io__state).
:- mode maybe_check_field_access_function(in, in, in, in, in, di, uo) is det.

maybe_check_field_access_function(FuncName, FuncArity,
		Status, Context, Module) -->
	(
		{ is_field_access_function_name(Module, FuncName, FuncArity,
			AccessType, FieldName) }
	->
		check_field_access_function(AccessType, FieldName, FuncName,
			FuncArity, Status, Context, Module)
	;
		[]
	).
	
:- pred check_field_access_function(field_access_type,
		ctor_field_name, sym_name,
		arity, import_status, prog_context, module_info,
		io__state, io__state).
:- mode check_field_access_function(in, in, in, in, in, in, in,
		di, uo) is det.
		
check_field_access_function(_AccessType, FieldName, FuncName, FuncArity,
		FuncStatus, Context, Module, IO0, IO) :-
	adjust_func_arity(function, FuncArity, PredArity),
	FuncCallId = function - FuncName/PredArity,

	%
	% Check that a function applied to an exported type
	% is also exported.
	%
	module_info_ctor_field_table(Module, CtorFieldTable),
	(
		% Abstract types have status `abstract_exported',
		% so errors won't be reported for local field
		% access functions for them.
		map__search(CtorFieldTable, FieldName, [FieldDefn]),
		FieldDefn = hlds_ctor_field_defn(_, DefnStatus, _, _, _),
		DefnStatus = exported, FuncStatus \= exported
	->
		report_field_status_mismatch(Context,
			FuncCallId, IO0, IO)
	;
		IO = IO0
	).

:- pred report_field_status_mismatch(prog_context, simple_call_id,
		io__state, io__state).
:- mode report_field_status_mismatch(in, in, di, uo) is det.

report_field_status_mismatch(Context, CallId) -->
	{ hlds_out__simple_call_id_to_string(CallId, CallIdString) },
	{ ErrorPieces = [
		words("In declaration of"),
		fixed(string__append(CallIdString, ":")),
		nl,
		words("error: a field access function for an"),
		words("exported field must also be exported.")
	]},
	error_util__write_error_pieces(Context, 0, ErrorPieces),
	io__set_exit_status(1).

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
	clauses_info_varset(ClausesInfo0, VarSet),
	clauses_info_headvars(ClausesInfo0, HeadVars),

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
	goal_info_init(Context, GoalInfo0),
	set__list_to_set(HeadVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo),
	Goal = Call - GoalInfo,
	Clause = clause([], Goal, mercury, Context),

		%
		% put the clause we just built into the pred_info,
		% annotateed with the appropriate types
		%
	ClauseList = [Clause],
	map__from_corresponding_lists(HeadVars, Types, VarTypes),
	map__init(TVarNameMap),
	map__init(TI_VarMap),
	map__init(TCI_VarMap),
	HasForeignClauses = no,
	ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
				HeadVars, ClauseList, TI_VarMap, TCI_VarMap,
				HasForeignClauses),
	pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo1),

		%
		% It's pointless but harmless to inline these clauses.
		% The main purpose of the `no_inline' marker is to stop
		% constraint propagation creating real infinite loops in
		% the generated code when processing calls to these
		% predicates. The code generator will still generate
		% inline code for calls to these predicates.
		%
	pred_info_get_markers(PredInfo1, Markers0),
	add_marker(Markers0, no_inline, Markers),
	pred_info_set_markers(PredInfo1, Markers, PredInfo).

%-----------------------------------------------------------------------------%

:- pred add_special_preds(module_info, tvarset, type, type_id, 
	hlds_type_body, prog_context, import_status, module_info).
:- mode add_special_preds(in, in, in, in, in, in, in, out) is det.

	% The only place that the index predicate for a type can ever
	% be called from is the compare predicate for that type.
	% The only types whose compare predicates call the type's index
	% predicate are discriminated union types which
	%
	% - do not have user-defined equality (the compare predicates for
	%   types with user-defined equality generate a runtime abort),
	%
	% - are not enums (comparison predicates for enums just do an integer
	%   comparison), and
	%
	% - have more than one constructor (for types with only one
	%   constructor, the comparison predicate just deconstructs the
	%   arguments and compares them).
	%
	% The compare predicate for an equivalence type never calls the index
	% predicate for that type; it calls the compare predicate of the
	% expanded type instead.
	%
	% When we see an abstract type declaration, we do not declare an index
	% predicate for that type, since the actual type definition may later
	% turn out not to require one. If the type does turn out to need
	% an index predicate, its declaration will be generated together with
	% its implementation.
	%
	% We also do not declare index predicates for types with hand defined
	% RTTI, since such types do not have index predicates.
	%
	% What we do here for uu types does not matter much, since such types
	% are not yet supported.
	%
	% Note: this predicate should include index in the list of special
	% predicates to be defined only for the kinds of types which do not
	% lead unify_proc__generate_index_clauses to abort.

add_special_preds(Module0, TVarSet, Type, TypeId,
			Body, Context, Status, Module) :-
	(
		special_pred_is_generated_lazily(Module0,
			TypeId, Body, Status)
	->
		Module = Module0
	;
		can_generate_special_pred_clauses_for_type(TypeId, Body)
	->
		add_special_pred(unify, Module0, TVarSet, Type, TypeId,
			Body, Context, Status, Module1),
		(
			status_defined_in_this_module(Status, yes)
		->
			(
				Body = du_type(Ctors, _, IsEnum,
						UserDefinedEquality),
				IsEnum = no,
				UserDefinedEquality = no,
				module_info_globals(Module0, Globals),
				globals__lookup_int_option(Globals,
					compare_specialization, CompareSpec),
				list__length(Ctors, CtorCount),
				CtorCount > CompareSpec
			->
				SpecialPredIds = [index, compare]
			;
				SpecialPredIds = [compare]
			),
			add_special_pred_list(SpecialPredIds,
				Module1, TVarSet, Type, TypeId,
				Body, Context, Status, Module)
		;
			% Never add clauses for comparison predicates
			% for imported types -- they will never be used.
			module_info_get_special_pred_map(Module1,
				SpecialPreds),
			( map__contains(SpecialPreds, compare - TypeId) ->
				Module = Module1
			;
				add_special_pred_decl(compare, Module1,
					TVarSet, Type, TypeId, Body,
					Context, Status, Module)
			)
		)
	;
		SpecialPredIds = [unify, compare],
		add_special_pred_decl_list(SpecialPredIds, Module0, TVarSet,
			Type, TypeId, Body, Context, Status, Module)
	).

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

add_special_pred(SpecialPredId, Module0, TVarSet, Type, TypeId, TypeBody,
		Context, Status0, Module) :-
	module_info_globals(Module0, Globals),
	globals__lookup_bool_option(Globals, special_preds, GenSpecialPreds),
	(
		GenSpecialPreds = yes,
		add_special_pred_for_real(SpecialPredId, Module0, TVarSet,
			Type, TypeId, TypeBody, Context, Status0, Module)
	;
		GenSpecialPreds = no,
		(
			SpecialPredId = unify,
			add_special_pred_unify_status(TypeBody, Status0,
				Status),
			add_special_pred_for_real(SpecialPredId, Module0,
				TVarSet, Type, TypeId, TypeBody, Context,
				Status, Module)
		;
			SpecialPredId = index,
			Module = Module0
		;
			SpecialPredId = compare,
			( TypeBody = du_type(_, _, _, yes(_)) ->
					% The compiler generated comparison
					% procedure prints an error message,
					% since comparisons of types with
					% user-defined equality are not
					% allowed. We get the runtime system
					% invoke this procedure instead of
					% printing the error message itself,
					% because it is easier to generate
					% a good error message in Mercury code
					% than in C code.
				add_special_pred_for_real(SpecialPredId,
					Module0, TVarSet, Type, TypeId,
					TypeBody, Context, Status0, Module)
			;
				Module = Module0
			)
		)
	).

add_special_pred_for_real(SpecialPredId,
		Module0, TVarSet, Type, TypeId, TypeBody, Context, Status0,
		Module) :-
	adjust_special_pred_status(Status0, SpecialPredId, Status),
	module_info_get_special_pred_map(Module0, SpecialPredMap0),
	( map__contains(SpecialPredMap0, SpecialPredId - TypeId) ->
		Module1 = Module0
	;
		add_special_pred_decl_for_real(SpecialPredId,
			Module0, TVarSet, Type, TypeId, Context, Status,
			Module1)
	),
	module_info_get_special_pred_map(Module1, SpecialPredMap1),
	map__lookup(SpecialPredMap1, SpecialPredId - TypeId, PredId),
	module_info_preds(Module1, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% if the type was imported, then the special preds for that
	% type should be imported too
	(
		(Status = imported(_) ; Status = pseudo_imported)
	->
		pred_info_set_import_status(PredInfo0, Status, PredInfo1)
	;
		TypeBody = du_type(_, _, _, yes(_)),
		pred_info_import_status(PredInfo0, OldStatus),
		OldStatus = pseudo_imported,
		status_is_imported(Status, no)
	->
		% We can only get here with --no-special-preds if the old
		% status is from an abstract declaration of the type.
		% Since the compiler did not then know that the type definition
		% will specify a user-defined equality predicate, it set up
		% the status as pseudo_imported in order to prevent the
		% generation of code for mode 0 of the __Unify__ predicate
		% for the type. However, for types with user-defined equality,
		% we *do* want to generate code for mode 0 of __Unify__,
		% so we fix the status.
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
			module_info, tvarset, type, type_id, hlds_type_body,
			prog_context, import_status, module_info).
:- mode add_special_pred_decl_list(in, in, in, in, in, in, in, in, out) is det.

add_special_pred_decl_list([], Module, _, _, _, _, _, _, Module).
add_special_pred_decl_list([SpecialPredId | SpecialPredIds], Module0,
		TVarSet, Type, TypeId, TypeBody, Context, Status, Module) :-
	add_special_pred_decl(SpecialPredId, Module0,
		TVarSet, Type, TypeId, TypeBody, Context, Status, Module1),
	add_special_pred_decl_list(SpecialPredIds, Module1,
		TVarSet, Type, TypeId, TypeBody, Context, Status, Module).

:- pred add_special_pred_decl(special_pred_id,
		module_info, tvarset, type, type_id, hlds_type_body,
		prog_context, import_status, module_info).
:- mode add_special_pred_decl(in, in, in, in, in, in, in, in, out) is det.

add_special_pred_decl(SpecialPredId, Module0, TVarSet, Type, TypeId, TypeBody,
		Context, Status0, Module) :-
	module_info_globals(Module0, Globals),
	globals__lookup_bool_option(Globals, special_preds, GenSpecialPreds),
	( GenSpecialPreds = yes ->
		add_special_pred_decl_for_real(SpecialPredId, Module0,
			TVarSet, Type, TypeId, Context, Status0, Module)
	; SpecialPredId = unify ->
		add_special_pred_unify_status(TypeBody, Status0, Status),
		add_special_pred_decl_for_real(SpecialPredId, Module0,
			TVarSet, Type, TypeId, Context, Status, Module)
	;
		Module = Module0
	).

add_special_pred_decl_for_real(SpecialPredId,
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
	module_info_globals(Module0, Globals),
	globals__lookup_string_option(Globals, aditi_user, Owner),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, ExistQVars,
		ArgTypes, Cond, Context, ClausesInfo0, Status, Markers,
		none, predicate, ClassContext, Proofs, Owner, PredInfo0),
	ArgLives = no,
	varset__init(InstVarSet),
		% Should not be any inst vars here so it's ok to use a 
		% fresh inst_varset.
	add_new_proc(PredInfo0, InstVarSet, Arity, ArgModes, yes(ArgModes),
		ArgLives, yes(Det), Context, address_is_not_taken, PredInfo,
		_),

	module_info_get_predicate_table(Module0, PredicateTable0),
	predicate_table_insert(PredicateTable0, PredInfo,
		PredId, PredicateTable),
	module_info_set_predicate_table(Module0, PredicateTable,
		Module1),
	module_info_get_special_pred_map(Module1, SpecialPredMap0),
	map__set(SpecialPredMap0, SpecialPredId - TypeId, PredId,
		SpecialPredMap),
	module_info_set_special_pred_map(Module1, SpecialPredMap, Module).

:- pred add_special_pred_unify_status(hlds_type_body::in, import_status::in,
	import_status::out) is det.

add_special_pred_unify_status(TypeBody, Status0, Status) :-
	( TypeBody = du_type(_, _, _, yes(_)) ->
			% If the type has user-defined equality,
			% then we create a real __Unify__ predicate
			% for it, whose body calls the user-specified
			% predicate. The compiler's usual type checking
			% algorithm will handle any necessary
			% disambiguation from predicates with the same
			% name but different argument types, and the
			% usual mode checking algorithm will select
			% the right mode of the chosen predicate.
		Status = Status0
	;
		Status = pseudo_imported
	).

:- pred adjust_special_pred_status(import_status, special_pred_id,
				import_status).
:- mode adjust_special_pred_status(in, in, out) is det.

adjust_special_pred_status(Status0, SpecialPredId, Status) :-
	( ( Status0 = opt_imported ; Status0 = abstract_imported ) ->
		Status1 = imported(interface)
	; Status0 = abstract_exported ->
		Status1 = exported
	;
		Status1 = Status0
	),

	% unification predicates are special - they are 
	% "pseudo"-imported/exported (only mode 0 is imported/exported).
	( SpecialPredId = unify ->
		( Status1 = imported(_) ->
			Status = pseudo_imported
		; Status1 = exported ->
			Status = pseudo_exported
		;
			Status = Status1
		)
	;
		Status = Status1
	).

add_new_proc(PredInfo0, InstVarSet, Arity, ArgModes, MaybeDeclaredArgModes,
		MaybeArgLives, MaybeDet, Context, IsAddressTaken, PredInfo,
		ModeId) :-
	pred_info_procedures(PredInfo0, Procs0),
	pred_info_arg_types(PredInfo0, ArgTypes),
	next_mode_id(Procs0, MaybeDet, ModeId),
	proc_info_init(Arity, ArgTypes, ArgModes, MaybeDeclaredArgModes,
		MaybeArgLives, MaybeDet, Context, IsAddressTaken, NewProc0),
	proc_info_set_inst_varset(NewProc0, InstVarSet, NewProc),
	map__det_insert(Procs0, ModeId, NewProc, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo).

%-----------------------------------------------------------------------------%

	% Add a mode declaration for a predicate.

:- pred module_add_mode(module_info, inst_varset, sym_name, list(mode),
		maybe(determinism), condition, import_status, prog_context,
		pred_or_func, bool, pair(pred_id, proc_id), module_info, 
		io__state, io__state).
:- mode module_add_mode(in, in, in, in, in, in, in, in, in, in, out, out, 
		di, uo) is det.

	% We should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments.

module_add_mode(ModuleInfo0, InstVarSet, PredName, Modes, MaybeDet, _Cond,
		Status, MContext, PredOrFunc, IsClassMethod, PredProcId,
		ModuleInfo) -->

		% Lookup the pred or func declaration in the predicate table.
		% If it's not there (or if it is ambiguous), optionally print a
		% warning message and insert an implicit definition for the
		% predicate; it is presumed to be local, and its type
		% will be inferred automatically.

	{ module_info_name(ModuleInfo0, ModuleName0) },
	{ sym_name_get_module_name(PredName, ModuleName0, ModuleName) },
	{ list__length(Modes, Arity) },
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity,
			[PredId0]) }
	->
		{ ModuleInfo1 = ModuleInfo0 },
		{ PredId = PredId0 }
	;
		preds_add_implicit_report_error(ModuleName,
			PredOrFunc, PredName, Arity, Status, IsClassMethod,
			MContext, "mode declaration", PredId,
			ModuleInfo0, ModuleInfo1)
	),

		% Lookup the pred_info for this predicate
	{ module_info_get_predicate_table(ModuleInfo1, PredicateTable1) },
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },

	module_do_add_mode(PredInfo0, InstVarSet, Arity, Modes, MaybeDet,
		IsClassMethod, MContext, PredInfo, ProcId),
	{ map__det_update(Preds0, PredId, PredInfo, Preds) },
	{ predicate_table_set_preds(PredicateTable1, Preds, PredicateTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo) },
	{ PredProcId = PredId - ProcId }.

:- pred module_do_add_mode(pred_info, inst_varset, arity, list(mode),
		maybe(determinism), bool, prog_context, pred_info, proc_id,
		io__state, io__state).
:- mode module_do_add_mode(in, in, in, in, in, in, in, out, out, di, uo)
		is det.

module_do_add_mode(PredInfo0, InstVarSet, Arity, Modes, MaybeDet,
		IsClassMethod, MContext, PredInfo, ProcId) -->
		% check that the determinism was specified
	(
		{ MaybeDet = no }
	->
		{ pred_info_import_status(PredInfo0, ImportStatus) },
		{ pred_info_get_is_pred_or_func(PredInfo0, PredOrFunc) },
		{ pred_info_module(PredInfo0, PredModule) },
		{ pred_info_name(PredInfo0, PredName) },
		{ PredSymName = qualified(PredModule, PredName) },
		( { IsClassMethod = yes } ->
			unspecified_det_for_method(PredSymName, Arity,
				PredOrFunc, MContext)
		; { status_is_exported(ImportStatus, yes) } ->
			unspecified_det_for_exported(PredSymName, Arity,
				PredOrFunc, MContext)
		;
			globals__io_lookup_bool_option(infer_det, InferDet),
			(
				{ InferDet = no }
			->
				unspecified_det_for_local(PredSymName, Arity,
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
	{ add_new_proc(PredInfo0, InstVarSet, Arity, Modes, yes(Modes),
		ArgLives, MaybeDet, MContext, address_is_not_taken, PredInfo,
		ProcId) }.

	% Whenever there is a clause or mode declaration for an undeclared
	% predicate, we add an implicit declaration
	%	:- pred p(T1, T2, ..., Tn).
	% for that predicate; the real types will be inferred by
	% type inference.

:- pred preds_add_implicit_report_error(module_name, pred_or_func, sym_name,
		arity, import_status, bool, prog_context, string,
		pred_id, module_info, module_info, io__state, io__state).
:- mode preds_add_implicit_report_error(in, in, in, in, in, in, in, in,
		out, in, out, di, uo) is det.

preds_add_implicit_report_error(ModuleName, PredOrFunc, PredName, Arity,
		Status, IsClassMethod, Context, Description, PredId,
		ModuleInfo0, ModuleInfo) -->
	maybe_undefined_pred_error(PredName, Arity, PredOrFunc, Status,
		IsClassMethod, Context, Description),

	( { PredOrFunc = function } ->
		{ adjust_func_arity(function, FuncArity, Arity) },
		maybe_check_field_access_function(PredName, FuncArity,
			Status, Context, ModuleInfo0)
	;
		[]
	),

	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	{ preds_add_implicit(ModuleInfo0, PredicateTable0, ModuleName,
		PredName, Arity, Status, Context, PredOrFunc,
		PredId, PredicateTable) },
	{ module_info_set_predicate_table(ModuleInfo0,
		PredicateTable, ModuleInfo) }.

:- pred preds_add_implicit(module_info, predicate_table, module_name,
		sym_name, arity, import_status, prog_context,
		pred_or_func, pred_id, predicate_table).
:- mode preds_add_implicit(in, in, in, in, in, in, in, in, out, out) is det.

preds_add_implicit(ModuleInfo, PredicateTable0, ModuleName, PredName, Arity,
		Status, Context, PredOrFunc, PredId, PredicateTable) :-
	clauses_info_init(Arity, ClausesInfo),
	preds_add_implicit_2(ClausesInfo, ModuleInfo, PredicateTable0,
			ModuleName, PredName, Arity, Status, Context,
			PredOrFunc, PredId, PredicateTable).

:- pred preds_add_implicit_for_assertion(prog_vars, module_info,
		predicate_table, module_name, sym_name, arity,
		import_status, prog_context, pred_or_func,
		pred_id, predicate_table).
:- mode preds_add_implicit_for_assertion(in, in, in,
		in, in, in, in, in, in, out, out) is det.

preds_add_implicit_for_assertion(HeadVars,
		ModuleInfo, PredicateTable0, ModuleName,
		PredName, Arity, Status, Context,
		PredOrFunc, PredId, PredicateTable) :-
	clauses_info_init_for_assertion(HeadVars, ClausesInfo),
	preds_add_implicit_2(ClausesInfo, ModuleInfo, PredicateTable0,
			ModuleName, PredName, Arity, Status, Context,
			PredOrFunc, PredId, PredicateTable).

:- pred preds_add_implicit_2(clauses_info, module_info, predicate_table,
		module_name, sym_name, arity, import_status, prog_context,
		pred_or_func, pred_id, predicate_table).
:- mode preds_add_implicit_2(in, in, in,
		in, in, in, in, in, in, out, out) is det.

preds_add_implicit_2(ClausesInfo, ModuleInfo, PredicateTable0, ModuleName,
		PredName, Arity, Status, Context,
		PredOrFunc, PredId, PredicateTable) :-
	varset__init(TVarSet0),
	make_n_fresh_vars("T", Arity, TVarSet0, TypeVars, TVarSet),
	term__var_list_to_term_list(TypeVars, Types),
	Cond = true,
	map__init(Proofs),
		% The class context is empty since this is an implicit
		% definition. Inference will fill it in.
	ClassContext = constraints([], []),
		% We assume none of the arguments are existentially typed.
		% Existential types must be declared, they won't be inferred.
	ExistQVars = [],
	init_markers(Markers0),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_string_option(Globals, aditi_user, Owner),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, ExistQVars,
		Types, Cond, Context, ClausesInfo, Status, Markers0, none,
		PredOrFunc, ClassContext, Proofs, Owner, PredInfo0),
	add_marker(Markers0, infer_type, Markers),
	pred_info_set_markers(PredInfo0, Markers, PredInfo),
	(
		\+ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, _)
	->
		module_info_get_partial_qualifier_info(ModuleInfo,
			MQInfo),
		predicate_table_insert(PredicateTable0, PredInfo, 
			may_be_unqualified, MQInfo, PredId, PredicateTable)
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

:- pred module_add_clause(module_info, prog_varset, pred_or_func, sym_name,
		list(prog_term), goal, import_status, prog_context, bool,
		module_info, qual_info, qual_info, io__state, io__state).
:- mode module_add_clause(in, in, in, in, in, in, in, in, in,
		out, in, out, di, uo) is det.

module_add_clause(ModuleInfo0, ClauseVarSet, PredOrFunc, PredName, Args, Body,
			Status, Context, IsAssertion, ModuleInfo,
			Info0, Info) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("% Processing clause for "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" `"),
		{ list__length(Args, PredArity) },
		{ adjust_func_arity(PredOrFunc, OrigArity, PredArity) },
		prog_out__write_sym_name_and_arity(PredName/OrigArity),
		io__write_string("'...\n")
	;
		[]
	),

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
		{ ModuleInfo1 = ModuleInfo0 },
		(
			{ IsAssertion = yes }
		->
			{ prog_out__sym_name_to_string(PredName, NameString) },
			{ string__format("%s %s %s (%s).\n",
				[s("Attempted to introduce a predicate"),
				s("for a promise with an identical"),
				s("name to an existing predicate"),
				s(NameString)], String) },
			{ error(String) }
		;
			[]
		)
	;
		(
				% An assertion will not have a
				% corresponding pred declaration.
			{ IsAssertion = yes },
			{ term__term_list_to_var_list(Args, HeadVars) },
			{ preds_add_implicit_for_assertion(HeadVars,
					ModuleInfo0, PredicateTable0,
					ModuleName, PredName, Arity, Status,
					Context, PredOrFunc,
					PredId, PredicateTable1) },
			{ module_info_set_predicate_table(ModuleInfo0,
					PredicateTable1, ModuleInfo1) }
		;
			{ IsAssertion = no },

			preds_add_implicit_report_error(ModuleName,
				PredOrFunc, PredName, Arity, Status, no,
				Context, "clause", PredId,
				ModuleInfo0, ModuleInfo1)
		)
	),
		% Lookup the pred_info for this pred,
		% add the clause to the clauses_info in the pred_info,
		% if there are no modes add an `infer_modes' marker,
		% and then save the pred_info.
	{ module_info_get_predicate_table(ModuleInfo1, PredicateTable2) },
	{ predicate_table_get_preds(PredicateTable2, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause for them
	{ Status = opt_imported ->
		pred_info_set_import_status(PredInfo0, opt_imported, PredInfo1)
	;
		PredInfo1 = PredInfo0
	},
	(
		{ pred_info_pragma_goal_type(PredInfo1) },
		{ get_mode_annotations(Args, _, empty, ModeAnnotations) },
		{ ModeAnnotations = empty ; ModeAnnotations = none }
	->
			% If we have a pragma foreign_proc for this procedure
			% already, and we are trying to add a non-mode specific
			% Mercury clause 
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: non mode-specific clause for "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  with `:- pragma foreign_proc' declaration preceding.\n"),
		{ Info = Info0 }
	;
		%
		% User-supplied clauses for field access functions are
		% not allowed -- the clauses are always generated by the
		% compiler.
		%
		{ PredOrFunc = function },	
		{ adjust_func_arity(function, FuncArity, Arity) },
		{ is_field_access_function_name(ModuleInfo1, PredName,
			FuncArity, _, _) },

		% Don't report errors for clauses for field access
		% function clauses in `.opt' files.
		{ Status \= opt_imported }
	->
		{ Info = Info0 },
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		{ hlds_out__simple_call_id_to_string(
			PredOrFunc - PredName/Arity, CallIdString0) },
		{ string__append(CallIdString0, ".", CallIdString) },
		{ ErrorPieces0 = [
			words("Error: clause for automatically generated"),
			words("field access"),
			fixed(CallIdString),
			nl
		] },
		globals__io_lookup_bool_option(verbose_errors, Verbose),
		(
			{ Verbose = yes },
			{ ErrorPieces1 = [
				words("Clauses for field access functions"),
				words("are automatically generated by the"),
				words("compiler. To supply your own"),
				words("definition for a field access"),
				words("function, for example to check"),
				words("the input to a field update,"),
				words("give the field of the constructor a"),
				words("different name.")
			] },
			{ list__append(ErrorPieces0, ErrorPieces1,
				ErrorPieces) }
		;
			{ Verbose = no },
			{ ErrorPieces = ErrorPieces0 }
		),
		error_util__write_error_pieces(Context, 0, ErrorPieces)
	;
		% Ignore clauses for builtins. This makes bootstrapping
		% easier when redefining builtins to use normal Mercury code.
		{ code_util__predinfo_is_builtin(PredInfo1) }
	->
		prog_out__write_context(Context),
		report_warning("Warning: clause for builtin.\n"),
		{ ModuleInfo = ModuleInfo1 },
		{ Info = Info0 }
	;
		{ pred_info_clauses_info(PredInfo1, Clauses0) },
		{ pred_info_typevarset(PredInfo1, TVarSet0) },
		{ maybe_add_default_func_mode(PredInfo1, PredInfo2, _) },
		select_applicable_modes(Args, ClauseVarSet, Status, Context,
			PredId, PredInfo2, ModuleInfo1, Info0,
			ArgTerms, ProcIdsForThisClause, ModuleInfo2, Info1),
		clauses_info_add_clause(Clauses0, ProcIdsForThisClause,
			ClauseVarSet, TVarSet0, ArgTerms, Body, Context,
			Status, PredOrFunc, Arity, IsAssertion, Goal,
			VarSet, TVarSet, Clauses, Warnings,
			ModuleInfo2, ModuleInfo3, Info1, Info),
		{
		pred_info_set_clauses_info(PredInfo2, Clauses, PredInfo3),
		(
			IsAssertion = yes
		->
			pred_info_set_goal_type(PredInfo3, assertion, PredInfo4)
		;
			pred_info_set_goal_type(PredInfo3, clauses, PredInfo4)
		),
		pred_info_set_typevarset(PredInfo4, TVarSet, PredInfo5),
		pred_info_arg_types(PredInfo5, _ArgTVarSet,
				ExistQVars, ArgTypes),
		pred_info_set_arg_types(PredInfo5, TVarSet,
				ExistQVars, ArgTypes, PredInfo6),

		%
		% check if there are still no modes for the predicate,
		% and if so, set the `infer_modes' flag for that predicate
		%
		pred_info_all_procids(PredInfo6, ProcIds),
		( ProcIds = [] ->
			pred_info_get_markers(PredInfo6, Markers0),
			add_marker(Markers0, infer_modes, Markers),
			pred_info_set_markers(PredInfo6, Markers, PredInfo)
		;
			PredInfo = PredInfo6
		),
		map__det_update(Preds0, PredId, PredInfo, Preds),
		predicate_table_set_preds(PredicateTable2, Preds,
			PredicateTable),
		module_info_set_predicate_table(ModuleInfo3, PredicateTable,
			ModuleInfo)
		},
		( { Status \= opt_imported } ->
			% warn about singleton variables 
			maybe_warn_singletons(VarSet,
				PredOrFunc - PredName/Arity, ModuleInfo, Goal),
			% warn about variables with overlapping scopes
			maybe_warn_overlap(Warnings, VarSet,
				PredOrFunc - PredName/Arity)
		;
			[]
		)
	).

	% Extract the mode annotations (if any) from the clause arguments,
	% and determine which mode(s) this clause should apply to.

:- pred select_applicable_modes(list(prog_term)::in, prog_varset::in,
		import_status::in, prog_context::in, pred_id::in,
		pred_info::in, module_info::in, qual_info::in,
		list(prog_term)::out, list(proc_id)::out,
		module_info::out, qual_info::out,
		io__state::di, io__state::uo) is det.

select_applicable_modes(Args0, VarSet, Status, Context, PredId, PredInfo,
		ModuleInfo0, Info0, Args, ProcIds, ModuleInfo, Info) -->
	{ get_mode_annotations(Args0, Args, empty, ModeAnnotations) },
	(
		{ ModeAnnotations = modes(ModeList0) },

		%
		% The user specified some mode annotations on this clause.
		% First module-qualify the mode annotations. The annotations
		% on clauses from `.opt' files will already be fully module
		% qualified.
		%
		( { Status = opt_imported } ->
			{ ModeList = ModeList0 },
			{ Info = Info0 }
		;
			{ qual_info_get_mq_info(Info0, MQInfo0) },
			module_qual__qualify_clause_mode_list(ModeList0,
				ModeList, Context, MQInfo0, MQInfo),
			{ qual_info_set_mq_info(Info0, MQInfo, Info) }
		),

		%
		% Now find the procedure which matches these mode annotations.
		%
		{ pred_info_procedures(PredInfo, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		(
			{ get_procedure_matching_declmodes(ExistingProcs,
				ModeList, ModuleInfo0, ProcId) }
		->
			{ ProcIds = [ProcId] },
			{ ModuleInfo = ModuleInfo0 }
		;
			{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
			undeclared_mode_error(
				ModeList, VarSet, PredId, PredInfo,
				ModuleInfo, Context),
			% apply the clause to all modes
			% XXX would it be better to apply it to none?
			{ pred_info_all_procids(PredInfo, ProcIds) }
		)
	;
		{ ModeAnnotations = empty },
		{ ProcIds = [] }, % this means the clauses applies to all modes
		{ ModuleInfo = ModuleInfo0 },
		{ Info = Info0 }
	;
		{ ModeAnnotations = none },
		{ ProcIds = [] }, % this means the clauses applies to all modes
		{ ModuleInfo = ModuleInfo0 },
		{ Info = Info0 }
	;
		{ ModeAnnotations = mixed },
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo) },
		{ Info = Info0 },
		io__set_exit_status(1),
		prog_out__write_context(Context),
		io__write_string("In clause for "),
		hlds_out__write_pred_id(ModuleInfo, PredId),
		io__write_string(":\n"),
		prog_out__write_context(Context),
		io__write_string(
	"  syntax error: some but not all arguments have mode annotations.\n"),
		% apply the clause to all modes
		% XXX would it be better to apply it to none?
		{ pred_info_all_procids(PredInfo, ProcIds) }
	).
			
	% Clauses can have mode annotations on them, to indicate that the
	% clause should only be used for particular modes of a predicate.
	% This type specifies the mode annotations on a clause.
:- type mode_annotations
	--->	empty	% No arguments.
	;	none	% One or more arguments,
			% each without any mode annotations.
	;	modes(list(mode))
			% One or more arguments, each with a mode annotation.
	;	mixed   % Two or more arguments, including some with mode
			% annotations and some without.  (This is not allowed.)
	.


	% Extract the mode annotations (if any) from a list of arguments.
:- pred get_mode_annotations(list(prog_term)::in, list(prog_term)::out,
		mode_annotations::in, mode_annotations::out) is det.

get_mode_annotations([], [], Annotations, Annotations).
get_mode_annotations([Arg0 | Args0], [Arg | Args],
		Annotations0, Annotations) :-
	get_mode_annotation(Arg0, Arg, MaybeAnnotation),
	add_annotation(Annotations0, MaybeAnnotation, Annotations1),
	get_mode_annotations(Args0, Args, Annotations1, Annotations).

:- pred add_annotation(mode_annotations::in, maybe(mode)::in,
		mode_annotations::out) is det.

add_annotation(empty, no, none).
add_annotation(empty, yes(Mode), modes([Mode])).
add_annotation(modes(_ `with_type` list(mode)), no, mixed).
add_annotation(modes(Modes), yes(Mode), modes(Modes ++ [Mode])).
add_annotation(none, no, none).
add_annotation(none, yes(_), mixed).
add_annotation(mixed, _, mixed).

	% Extract the mode annotations (if any) from a single argument.
:- pred get_mode_annotation(prog_term::in, prog_term::out, maybe(mode)::out)
		is det.

get_mode_annotation(Arg0, Arg, MaybeAnnotation) :-
	(
		Arg0 = term__functor(term__atom("::"), [Arg1, ModeTerm], _),
		convert_mode(term__coerce(ModeTerm), Mode)
	->
		Arg = Arg1,
		MaybeAnnotation = yes(Mode)
	;
		Arg = Arg0,
		MaybeAnnotation = no
	).

%-----------------------------------------------------------------------------%
%
% Generate the clauses_info for the introduced predicate that we generate
% for each method in a type class instance declaration.
%

	% handle the `pred(<MethodName>/<Arity>) is <ImplName>' syntax
produce_instance_method_clauses(name(InstancePredName), PredOrFunc, PredArity,
		ArgTypes, Markers, Context, _Status, ClausesInfo,
		ModuleInfo0, ModuleInfo, QualInfo0, QualInfo, IO, IO) :-

		% Add the body of the introduced pred

		% First the goal info
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo1),
	set__list_to_set(HeadVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo1, NonLocals, GoalInfo2),
	(
		check_marker(Markers, (impure))
	->
		goal_info_add_feature(GoalInfo2, (impure), GoalInfo)
	;
		check_marker(Markers, (semipure))
	->
		goal_info_add_feature(GoalInfo2, (semipure), GoalInfo)
	;
		GoalInfo = GoalInfo2
	),

		% Then the goal itself
	varset__init(VarSet0),
	make_n_fresh_vars("HeadVar__", PredArity, VarSet0, HeadVars, VarSet), 
	invalid_pred_id(InvalidPredId),
	construct_pred_or_func_call(InvalidPredId, PredOrFunc,
		InstancePredName, HeadVars, GoalInfo, IntroducedGoal,
		transform_info(ModuleInfo0, QualInfo0),
		transform_info(ModuleInfo, QualInfo)),
	IntroducedClause = clause([], IntroducedGoal, mercury, Context),

	map__from_corresponding_lists(HeadVars, ArgTypes, VarTypes),
	map__init(TVarNameMap),
	map__init(TI_VarMap),
	map__init(TCI_VarMap),
	HasForeignClauses = no,
	ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
		HeadVars, [IntroducedClause], TI_VarMap, TCI_VarMap,
		HasForeignClauses).

	% handle the arbitrary clauses syntax
produce_instance_method_clauses(clauses(InstanceClauses), PredOrFunc,
		PredArity, _ArgTypes, _Markers, Context, Status, ClausesInfo,
		ModuleInfo0, ModuleInfo, QualInfo0, QualInfo, IO0, IO) :-
	clauses_info_init(PredArity, ClausesInfo0),
	list__foldl2(
		produce_instance_method_clause(PredOrFunc, Context, Status),
		InstanceClauses, ModuleInfo0 - QualInfo0 - ClausesInfo0,
		ModuleInfo - QualInfo - ClausesInfo, IO0, IO).

:- pred produce_instance_method_clause(pred_or_func::in,
		prog_context::in, import_status::in, item::in,
		pair(pair(module_info, qual_info), clauses_info)::in,
		pair(pair(module_info, qual_info), clauses_info)::out,
		io__state::di, io__state::uo) is det.
produce_instance_method_clause(PredOrFunc, Context, Status, InstanceClause,
		ModuleInfo0 - QualInfo0 - ClausesInfo0,
		ModuleInfo - QualInfo - ClausesInfo) -->
	(
		{ InstanceClause = clause(CVarSet, PredOrFunc, PredName,
				HeadTerms, Body) }
	->
		{ PredArity = list__length(HeadTerms) },
		{ adjust_func_arity(PredOrFunc, Arity, PredArity) },
		% The tvarset argument is only used for explicit type
		% qualifications, of which there are none in this clause,
		% so it is set to a dummy value.
		{ varset__init(TVarSet0) },

		{ ProcIds = [] }, % means this clause applies to _every_
				  % mode of the procedure
		{ IsAssertion = no },
		clauses_info_add_clause(ClausesInfo0, ProcIds,
			CVarSet, TVarSet0, HeadTerms, Body, Context, Status,
			PredOrFunc, Arity, IsAssertion, Goal,
			VarSet, _TVarSet, ClausesInfo, Warnings,
			ModuleInfo0, ModuleInfo, QualInfo0, QualInfo),

		% warn about singleton variables 
		maybe_warn_singletons(VarSet,
			PredOrFunc - PredName/Arity, ModuleInfo, Goal),

		% warn about variables with overlapping scopes
		maybe_warn_overlap(Warnings, VarSet,
			PredOrFunc - PredName/Arity)
	;
		{ error("produce_clause: invalid instance item") }
	).

%-----------------------------------------------------------------------------%
%
% module_add_pragma_import:
%	Handles `pragma import' declarations, by figuring out which predicate
%	the `pragma import' declaration applies to, and adding a clause
%	for that predicate containing an appropriate HLDS `pragma_c_code'
%	instruction.
%
%	NB. Any changes here might also require similar changes to the
%	handling of `pragma export' declarations, in export.m.

:- pred module_add_pragma_import(sym_name, pred_or_func, list(mode),
		pragma_foreign_proc_attributes, string, import_status,
		prog_context, module_info, module_info, qual_info, qual_info,
		io__state, io__state).
:- mode module_add_pragma_import(in, in, in, in, in, in, in, in, out,
		in, out, di, uo) is det.

module_add_pragma_import(PredName, PredOrFunc, Modes, Attributes,
		C_Function, Status, Context, ModuleInfo0, ModuleInfo,
		Info0, Info) -->
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ list__length(Modes, Arity) },

		%
		% print out a progress message
		%
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma import' for "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
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
		{ ModuleInfo1 = ModuleInfo0 }
	;
		preds_add_implicit_report_error(ModuleName,
			PredOrFunc, PredName, Arity, Status, no, Context,
			"`:- pragma import' declaration",
			PredId, ModuleInfo0, ModuleInfo1)
	),
		%
		% Lookup the pred_info for this pred,
		% and check that it is valid.
		%
	{ module_info_get_predicate_table(ModuleInfo1, PredicateTable2) },
	{ predicate_table_get_preds(PredicateTable2, Preds0) },
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
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma import' "),
		io__write_string("declaration for imported "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n"),
		{ Info = Info0 }
	;	
		{ pred_info_clause_goal_type(PredInfo1) }
	->
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma import' declaration "),
		io__write_string("for "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
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
			{ get_procedure_matching_argmodes(ExistingProcs, Modes,
						ModuleInfo1, ProcId) }
		->
			pred_add_pragma_import(PredInfo2, PredId, ProcId,
				Attributes, C_Function, Context,
				PredInfo, ModuleInfo1, ModuleInfo2,
				Info0, Info),
			{ map__det_update(Preds0, PredId, PredInfo, Preds) },
			{ predicate_table_set_preds(PredicateTable2, Preds,
				PredicateTable) },
			{ module_info_set_predicate_table(ModuleInfo2,
				PredicateTable, ModuleInfo) }
		;
			{ module_info_incr_errors(ModuleInfo1, ModuleInfo) }, 
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma import' "),
			io__write_string("declaration for undeclared mode "),
			io__write_string("of "),
			hlds_out__write_simple_call_id(PredOrFunc,
				PredName/Arity),
			io__write_string(".\n"),
			io__set_output_stream(OldStream, _),
			{ Info = Info0 }
		)
	).

% pred_add_pragma_import:
%	This is a subroutine of module_add_pragma_import which adds
%	the c_code for a `pragma import' declaration to a pred_info.

:- pred pred_add_pragma_import(pred_info, pred_id, proc_id,
		pragma_foreign_proc_attributes, string, prog_context, pred_info,
		module_info, module_info, qual_info, qual_info,
		io__state, io__state).
:- mode pred_add_pragma_import(in, in, in, in, in, in, out, in, out, in, out,
		di, uo) is det.
pred_add_pragma_import(PredInfo0, PredId, ProcId, Attributes, C_Function,
		Context, PredInfo, ModuleInfo0, ModuleInfo, Info0, Info) -->
	{ pred_info_procedures(PredInfo0, Procs) },
	{ map__lookup(Procs, ProcId, ProcInfo) },
	{ foreign__make_pragma_import(PredInfo0, ProcInfo, C_Function, Context,
		ModuleInfo0, PragmaImpl, VarSet, PragmaVars, ArgTypes,
		Arity, PredOrFunc) },

	%
	% lookup some information we need from the pred_info and proc_info
	%
	{ pred_info_name(PredInfo0, PredName) },
	{ pred_info_module(PredInfo0, PredModule) },
	{ pred_info_clauses_info(PredInfo0, Clauses0) },
	{ pred_info_get_purity(PredInfo0, Purity) },

	%
	% Add the code for this `pragma import' to the clauses_info
	%
	clauses_info_add_pragma_foreign_proc(Clauses0, Purity, Attributes,
		PredId, ProcId, VarSet, PragmaVars, ArgTypes, PragmaImpl,
		Context, PredOrFunc, qualified(PredModule, PredName),
		Arity, Clauses, ModuleInfo0, ModuleInfo, Info0, Info),

	%
	% Store the clauses_info etc. back into the pred_info
	%
	{ pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo) }.

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_foreign_proc(pragma_foreign_proc_attributes,
	sym_name, pred_or_func, list(pragma_var), prog_varset,
	pragma_foreign_code_impl, import_status, prog_context,
	module_info, module_info, qual_info, qual_info, io__state,
	io__state).
:- mode module_add_pragma_foreign_proc(in, in, in, in, in, in, in, in,
	in, out, in, out, di, uo) is det.  

module_add_pragma_foreign_proc(Attributes, PredName, PredOrFunc,
		PVars, VarSet, PragmaImpl, Status, Context,
		ModuleInfo0, ModuleInfo, Info0, Info) --> 
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ foreign_language(Attributes, PragmaForeignLanguage) },
	{ list__length(PVars, Arity) },
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma foreign_proc' for "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
		io__write_string("...\n")
	;
		[]
	),

	globals__io_get_backend_foreign_languages(BackendForeignLangs),

		% Lookup the pred declaration in the predicate table.
		% (If it's not there, print an error message and insert
		% a dummy declaration for the predicate.) 
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) }, 
	( 
		{ predicate_table_search_pf_sym_arity(PredicateTable0,
			PredOrFunc, PredName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ ModuleInfo1 = ModuleInfo0 }
	;
		preds_add_implicit_report_error(ModuleName,
			PredOrFunc, PredName, Arity, Status, no, Context,
			"`:- pragma foreign_proc' declaration",
			PredId, ModuleInfo0, ModuleInfo1)
	),
		% Lookup the pred_info for this pred,
		% add the pragma to the proc_info in the proc_table in the
		% pred_info, and save the pred_info.
	{ module_info_get_predicate_table(ModuleInfo1, PredicateTable1) },
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
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma foreign_proc' (or `pragma c_code')\n"),
		prog_out__write_context(Context),
		io__write_string("declaration for imported "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n"),
		{ Info = Info0 }
	;	
		{ pred_info_clause_goal_type(PredInfo1) },
		{ pred_info_clauses_info(PredInfo1, CInfo) },
		{ clauses_info_clauses(CInfo, ClauseList) },
		{ list__member(clause([], _, mercury, _), ClauseList) }

	->
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		prog_out__write_context(Context),
		io__write_string("Error: `:- pragma foreign_proc' (or `pragma c_code')\n"),
		prog_out__write_context(Context),
		io__write_string("declaration for "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("  with preceding non-mode specific clauses.\n"),
		{ Info = Info0 }
	;
			% Don't add clauses for foreign languages other
			% than the ones we can generate code for.
		{ not list__member(PragmaForeignLanguage, BackendForeignLangs) }
	->
		{ ModuleInfo = ModuleInfo1 },
		{ Info = Info0 }
	;		
		% add the pragma declaration to the proc_info for this procedure
		{ pred_info_procedures(PredInfo1, Procs) },
		{ map__to_assoc_list(Procs, ExistingProcs) },
		{ pragma_get_modes(PVars, Modes) },
		(
			{ get_procedure_matching_argmodes(ExistingProcs, Modes,
						ModuleInfo1, ProcId) }
		->
			{ pred_info_clauses_info(PredInfo1, Clauses0) },

			{ pred_info_arg_types(PredInfo1, ArgTypes) },
			{ pred_info_get_purity(PredInfo1, Purity) },
			clauses_info_add_pragma_foreign_proc(
				Clauses0, Purity, Attributes, PredId,
				ProcId, VarSet, PVars, ArgTypes,
				PragmaImpl, Context, PredOrFunc,
				PredName, Arity, Clauses, ModuleInfo1,
				ModuleInfo2, Info0, Info),
			{ pred_info_set_clauses_info(PredInfo1, Clauses, 
				PredInfo2) },
			{ pred_info_clause_goal_type(PredInfo2) ->
				pred_info_set_goal_type(PredInfo2,
					clauses_and_pragmas, PredInfo)
			;
				pred_info_set_goal_type(PredInfo2, pragmas,
					PredInfo)
			},
			{ map__det_update(Preds0, PredId, PredInfo, Preds) },
			{ predicate_table_set_preds(PredicateTable1, Preds, 
				PredicateTable) },
			{ module_info_set_predicate_table(ModuleInfo2, 
				PredicateTable, ModuleInfo) },
			{ pragma_get_var_infos(PVars, ArgInfo) },
			maybe_warn_pragma_singletons(PragmaImpl, 
				PragmaForeignLanguage, ArgInfo,
				Context, PredOrFunc - PredName/Arity,
				ModuleInfo)
		;
			{ module_info_incr_errors(ModuleInfo1, ModuleInfo) }, 
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma foreign_proc' "),
			io__write_string("declaration for undeclared mode "),
			io__write_string("of "),
			hlds_out__write_simple_call_id(PredOrFunc,
				PredName/Arity),
			io__write_string(".\n"),
			io__set_output_stream(OldStream, _),
			{ Info = Info0 }
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_tabled(eval_method, sym_name, int, 
		maybe(pred_or_func), maybe(list(mode)), 
		import_status, prog_context, module_info, module_info, 
		io__state, io__state).
:- mode module_add_pragma_tabled(in, in, in, in, in, in, in, in, out, 
	di, uo) is det. 
	
module_add_pragma_tabled(EvalMethod, PredName, Arity, MaybePredOrFunc, 
		MaybeModes, Status, Context, ModuleInfo0, ModuleInfo) --> 
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
			{ string__format("`:- pragma %s' declaration",
				[s(EvalMethodS)], Message1) },

			preds_add_implicit_report_error(ModuleName,
				PredOrFunc, PredName, Arity, Status, no,
				Context, Message1, PredId,
				ModuleInfo0, ModuleInfo1),
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
			{ string__format("`:- pragma %s' declaration",
				[s(EvalMethodS)], Message1) },

			preds_add_implicit_report_error(ModuleName,
				predicate, PredName, Arity, Status, no,
				Context, Message1, PredId,
				ModuleInfo0, ModuleInfo1),
			{ PredIds = [PredId] }
		)
	),
	list__foldl2(module_add_pragma_tabled_2(EvalMethod, PredName, 
			Arity, MaybePredOrFunc, MaybeModes, Context), 
			PredIds, ModuleInfo1, ModuleInfo).


:- pred module_add_pragma_tabled_2(eval_method, sym_name, int, 
		maybe(pred_or_func), maybe(list(mode)), prog_context,
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
	{ adjust_func_arity(PredOrFunc, Arity0, Arity) },
		
		% print out a progress message
	{ eval_method_to_string(EvalMethod, EvalMethodS) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( 
		{ VeryVerbose = yes }
	->
		io__write_string("% Processing `:- pragma "),
		io__write_string(EvalMethodS),
		io__write_string("' for "),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
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
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity),
		io__write_string(".\n")
	;
		% do we have to make sure the tabled preds are stratified?
		(
			{ eval_method_needs_stratification(EvalMethod) = yes }
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
			{ MaybeModes = yes(Modes) }
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
				hlds_out__write_simple_call_id(PredOrFunc, 
					PredName/Arity),
				io__write_string(".\n")
			)
		;
			{ ExistingProcs = [] }
		->
			{ module_info_incr_errors(ModuleInfo1, ModuleInfo) }, 
			prog_out__write_context(Context),
			io__write_string("Error: `:- pragma "),
			io__write_string(EvalMethodS),
			io__write_string("' declaration for\n"), 
			prog_out__write_context(Context),
			io__write_string("  "),
			hlds_out__write_simple_call_id(PredOrFunc, 
				PredName/Arity),
			io__write_string(" with no declared modes.\n")
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

:- pred pragma_add_marker(pred_table, list(pred_id), add_marker_pred_info,
		import_status, bool, pred_table, bool).
:- mode pragma_add_marker(in, in, in(add_marker_pred_info),
		in, in, out, out) is det.

pragma_add_marker(PredTable, [], _, _, _, PredTable, no). 
pragma_add_marker(PredTable0, [PredId | PredIds], UpdatePredInfo, Status,
		MustBeExported, PredTable, WrongStatus) :-
	map__lookup(PredTable0, PredId, PredInfo0),
	call(UpdatePredInfo, PredInfo0, PredInfo),
	(
		pred_info_is_exported(PredInfo),
		MustBeExported = yes,
		Status \= exported
	->
		WrongStatus0 = yes
	;
		WrongStatus0 = no
	),
	map__det_update(PredTable0, PredId, PredInfo, PredTable1),
	pragma_add_marker(PredTable1, PredIds, UpdatePredInfo, Status,
		MustBeExported, PredTable, WrongStatus1),
	bool__or(WrongStatus0, WrongStatus1, WrongStatus).

:- pred add_marker_pred_info(marker, pred_info, pred_info).
:- mode add_marker_pred_info(in, in, out) is det.

add_marker_pred_info(Marker, PredInfo0, PredInfo) :-
	pred_info_get_markers(PredInfo0, Markers0),
	add_marker(Markers0, Marker, Markers),
	pred_info_set_markers(PredInfo0, Markers, PredInfo).

	% Succeed if a marker for an exported procedure must also
	% be exported.
:- pred marker_must_be_exported(marker).
:- mode marker_must_be_exported(in) is semidet.

marker_must_be_exported(aditi).
marker_must_be_exported(base_relation).

%---------------------------------------------------------------------------%

	% Find the procedure with argmodes which match the ones we want.

:- pred get_procedure_matching_argmodes(assoc_list(proc_id, proc_info),
		list(mode), module_info, proc_id).
:- mode get_procedure_matching_argmodes(in, in, in, out) is semidet.
get_procedure_matching_argmodes([P|Procs], Modes, ModuleInfo, OurProcId) :-
	P = ProcId - ProcInfo,
	proc_info_argmodes(ProcInfo, ArgModes),
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
	proc_info_declared_argmodes(ProcInfo, ArgModes),
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
				simple_call_id, io__state, io__state).
:- mode maybe_warn_overlap(in, in, in, di, uo) is det.

maybe_warn_overlap(Warnings, VarSet, PredCallId) -->
	globals__io_lookup_bool_option(warn_overlapping_scopes,
			WarnOverlappingScopes),
	( { WarnOverlappingScopes = yes } ->
		warn_overlap(Warnings, VarSet, PredCallId)
	;	
		[]
	).

:- pred warn_overlap(list(quant_warning), prog_varset, simple_call_id,
		io__state, io__state).
:- mode warn_overlap(in, in, in, di, uo) is det.

warn_overlap([], _, _) --> [].
warn_overlap([Warn|Warns], VarSet, PredCallId) -->
	{ Warn = warn_overlap(Vars, Context) },
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	prog_out__write_context(Context),
	io__write_string(StdErr, "In clause for "),
	hlds_out__write_simple_call_id(PredCallId),
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
	warn_overlap(Warns, VarSet, PredCallId).

%-----------------------------------------------------------------------------%

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once, or about variables that do not occur in
	% C code strings when they should.
	%
:- pred maybe_warn_singletons(prog_varset, simple_call_id, module_info,
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
	simple_call_id, module_info, io__state, io__state).
:- mode warn_singletons_in_goal(in, in, in, in, in, di, uo) is det.

warn_singletons_in_goal(Goal - GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet,
		PredCallId, MI).

:- pred warn_singletons_in_goal_2(hlds_goal_expr, hlds_goal_info, set(prog_var),
		prog_varset, simple_call_id, module_info,
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

warn_singletons_in_goal_2(some(Vars, _, SubGoal), GoalInfo, QuantVars, VarSet,
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

warn_singletons_in_goal_2(generic_call(GenericCall, Args0, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId, _) -->
	{ goal_util__generic_call_vars(GenericCall, Args1) },
	{ list__append(Args0, Args1, Args) },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(unify(Var, RHS, _, _, _),
			GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_unify(Var, RHS, GoalInfo, QuantVars, VarSet,
		PredCallId, MI).

warn_singletons_in_goal_2(foreign_proc(Attrs, _, _, _, ArgInfo, _,
		PragmaImpl), GoalInfo, _QuantVars, _VarSet, PredCallId, MI) --> 
	{ goal_info_get_context(GoalInfo, Context) },
	{ foreign_language(Attrs, Lang) },
	warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang,
		ArgInfo, Context, PredCallId, MI).

warn_singletons_in_goal_2(shorthand(ShorthandGoal), GoalInfo, QuantVars,
		VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_2_shorthand(ShorthandGoal, GoalInfo, 
		QuantVars, VarSet, PredCallId, MI).


:- pred warn_singletons_in_goal_2_shorthand(shorthand_goal_expr,
		hlds_goal_info, set(prog_var), prog_varset, simple_call_id,
		module_info, io__state, io__state).
:- mode warn_singletons_in_goal_2_shorthand(in, in, in, in, in, in, di, uo)
		is det.

warn_singletons_in_goal_2_shorthand(bi_implication(LHS, RHS), _GoalInfo, 
		QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_list([LHS, RHS], QuantVars, VarSet,
		PredCallId, MI).
		

:- pred warn_singletons_in_goal_list(list(hlds_goal), set(prog_var),
		prog_varset, simple_call_id, module_info,
		io__state, io__state).
:- mode warn_singletons_in_goal_list(in, in, in, in, in, di, uo) is det.

warn_singletons_in_goal_list([], _, _, _, _) --> [].
warn_singletons_in_goal_list([Goal|Goals], QuantVars, VarSet, CallPredId, MI)
		-->
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI),
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, CallPredId, MI).

:- pred warn_singletons_in_cases(list(case), set(prog_var), prog_varset,
	simple_call_id, module_info, io__state, io__state).
:- mode warn_singletons_in_cases(in, in, in, in, in, di, uo) is det.

warn_singletons_in_cases([], _, _, _, _) --> [].
warn_singletons_in_cases([Case|Cases], QuantVars, VarSet, CallPredId, MI) -->
	{ Case = case(_ConsId, Goal) },
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI),
	warn_singletons_in_cases(Cases, QuantVars, VarSet, CallPredId, MI).

:- pred warn_singletons_in_unify(prog_var, unify_rhs, hlds_goal_info,
		set(prog_var), prog_varset, simple_call_id, module_info,
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

warn_singletons_in_unify(X, lambda_goal(_PredOrFunc, _Eval, _Fix, _NonLocals,
				LambdaVars, _Modes, _Det, LambdaGoal),
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

:- pred maybe_warn_pragma_singletons(pragma_foreign_code_impl,
	foreign_language, list(maybe(pair(string, mode))), prog_context,
	simple_call_id, module_info, io__state, io__state).
:- mode maybe_warn_pragma_singletons(in, in, in, in, in, in, di, uo) is det.

maybe_warn_pragma_singletons(PragmaImpl, Lang, ArgInfo, Context, CallId, MI) -->
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars),
	( { WarnSingletonVars = yes } ->
		warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang,
			ArgInfo, Context, CallId, MI)
	;	
		[]
	).

	% warn_singletons_in_pragma_foreign_proc checks to see if each
	% variable is mentioned at least once in the foreign code
	% fragments that ought to mention it. If not, it gives a
	% warning.
	% (Note that for some foreign languages it might not be
	% appropriate to do this check, or you may need to add a
	% transformation to map Mercury variable names into identifiers
	% for that foreign language).
:- pred warn_singletons_in_pragma_foreign_proc(pragma_foreign_code_impl,
	foreign_language, list(maybe(pair(string, mode))), prog_context,
	simple_call_id, module_info, io__state, io__state).
:- mode warn_singletons_in_pragma_foreign_proc(in, in, in, in, in, in,
	di, uo) is det.

warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang, ArgInfo, 
		Context, PredOrFuncCallId, ModuleInfo) -->
	{ LangStr = foreign_language_string(Lang) },
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
			io__write_string("In the " ++ LangStr ++ " code for "),
			hlds_out__write_simple_call_id(PredOrFuncCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			write_variable_warning_start(UnmentionedVars),
			io__write_string("not occur in the " ++
				LangStr ++ " code.\n"),
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
				mode_is_input(ModuleInfo, Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, FirstCodeList)
			)), UnmentionedInputVars) },
		( { UnmentionedInputVars = [] } ->
			[]
		;
			io__stderr_stream(StdErr2),
			io__set_output_stream(StdErr2, OldStream2),
			prog_out__write_context(Context),
			io__write_string("In the " ++ LangStr ++ " code for "),
			hlds_out__write_simple_call_id(PredOrFuncCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			write_variable_warning_start(UnmentionedInputVars),
			io__write_string("not occur in the first " ++
				LangStr ++ " code.\n "),
			io__set_output_stream(OldStream2, _)
		),
		{ solutions(lambda([Name::out] is nondet, (
				list__member(yes(Name - Mode), ArgInfo),
				mode_is_output(ModuleInfo, Mode),
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
			io__write_string("In the " ++ LangStr ++ " code for "),
			hlds_out__write_simple_call_id(PredOrFuncCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			write_variable_warning_start(
				UnmentionedFirstOutputVars),
			io__write_string("not occur in the first " ++
				LangStr ++ " code or the shared " ++ LangStr ++
				" code.\n "),
			io__set_output_stream(OldStream3, _)
		),
		{ solutions(lambda([Name::out] is nondet, (
				list__member(yes(Name - Mode), ArgInfo),
				mode_is_output(ModuleInfo, Mode),
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
			io__write_string("In the " ++ LangStr ++ " code for "),
			hlds_out__write_simple_call_id(PredOrFuncCallId),
			io__write_string(":\n"),
			prog_out__write_context(Context),
			write_variable_warning_start(
				UnmentionedLaterOutputVars),
			io__write_string("not occur in the retry " ++
				LangStr ++ " code or the shared " ++ LangStr ++
				" code.\n "),
			io__set_output_stream(OldStream4, _)
		)
	;
		{ PragmaImpl = import(_, _, _, _) }
	).

:- pred write_variable_warning_start(list(string)::in, io__state::di,
		io__state::uo) is det.
write_variable_warning_start(UnmentionedVars) -->
	( { UnmentionedVars = [_] } ->
		io__write_string("  warning: variable `"),
		write_string_list(UnmentionedVars),
		io__write_string("' does ")
	;
		io__write_string("  warning: variables `"),
		write_string_list(UnmentionedVars),
		io__write_string("' do ")
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
		prog_varset, prog_context, simple_call_id,
		io__state, io__state).
:- mode warn_singletons(in, in, in, in, in, in, di, uo) is det.

warn_singletons(GoalVars, NonLocals, QuantVars, VarSet, Context,
		PredOrFuncCallId) -->
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
		hlds_out__write_simple_call_id(PredOrFuncCallId),
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
		hlds_out__write_simple_call_id(PredOrFuncCallId),
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

:- pred clauses_info_init_for_assertion(prog_vars::in,
		clauses_info::out) is det.

clauses_info_init_for_assertion(HeadVars, ClausesInfo) :-
	map__init(VarTypes),
	map__init(TVarNameMap),
	varset__init(VarSet),
	map__init(TI_VarMap),
	map__init(TCI_VarMap),
	HasForeignClauses = no,
	ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
		HeadVars, [], TI_VarMap, TCI_VarMap, HasForeignClauses).

:- pred clauses_info_init(int::in, clauses_info::out) is det.

clauses_info_init(Arity, ClausesInfo) :-
	map__init(VarTypes),
	map__init(TVarNameMap),
	varset__init(VarSet0),
	make_n_fresh_vars("HeadVar__", Arity, VarSet0, HeadVars, VarSet),
	map__init(TI_VarMap),
	map__init(TCI_VarMap),
	HasForeignClauses = no,
	ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap,
		VarTypes, HeadVars, [], TI_VarMap, TCI_VarMap,
		HasForeignClauses).

:- pred clauses_info_add_clause(clauses_info::in,
		list(proc_id)::in, prog_varset::in, tvarset::in,
		list(prog_term)::in, goal::in, prog_context::in,
		import_status::in, pred_or_func::in, arity::in, bool::in,
		hlds_goal::out, prog_varset::out, tvarset::out,
		clauses_info::out, list(quant_warning)::out, 
		module_info::in, module_info::out, qual_info::in,
		qual_info::out, io__state::di, io__state::uo) is det.

clauses_info_add_clause(ClausesInfo0, ModeIds0, CVarSet, TVarSet0,
		Args, Body, Context, Status, PredOrFunc, Arity, IsAssertion,
		Goal, VarSet, TVarSet, ClausesInfo, Warnings, Module0, Module,
		Info0, Info) -->
	{ ClausesInfo0 = clauses_info(VarSet0, ExplicitVarTypes0, TVarNameMap0,
				InferredVarTypes, HeadVars, ClauseList0,
				TI_VarMap, TCI_VarMap, HasForeignClauses) },
	{ ClauseList0 = [] ->
		% Create the mapping from type variable name, used to
		% rename type variables occurring in explicit type
		% qualifications. The version of this mapping stored
		% in the clauses_info should only contain type variables
		% which occur in the argument types of the predicate.
		% Type variables which only occur in explicit type
		% qualifications are local to the clause in which they appear.
		varset__create_name_var_map(TVarSet0, TVarNameMap)
	;
		TVarNameMap = TVarNameMap0
	},
	{ update_qual_info(Info0, TVarNameMap, TVarSet0,
			ExplicitVarTypes0, Status, Info1) },
	{ varset__merge_subst(VarSet0, CVarSet, VarSet1, Subst) },
	transform(Subst, HeadVars, Args, Body, VarSet1, Context, PredOrFunc,
			Arity, IsAssertion, Goal0, VarSet, Warnings,
			transform_info(Module0, Info1),
			transform_info(Module, Info2)),
	{ TVarSet = Info2 ^ tvarset },
	{ qual_info_get_found_syntax_error(Info2, FoundError) },
	{ qual_info_set_found_syntax_error(no, Info2, Info) },
	(
		{ FoundError = yes },
			% Don't insert clauses containing syntax errors into
			% the clauses_info, because doing that would cause
			% typecheck.m to report spurious type errors.
		{ ClausesInfo = ClausesInfo0 },
			% Don't report singleton variable warnings if there
			% were syntax errors.
		{ true_goal(Goal) }
	;
		{ FoundError = no },
		{ Goal = Goal0 },

			% If we have foreign clauses, we should only
			% add this clause for modes *not* covered by the 
			% foreign clauses.
		{ HasForeignClauses = yes ->
			ForeignModeIds = list__condense(list__filter_map(
				(func(C) = ProcIds is semidet :-
					C = clause(ProcIds, _, ClauseLang, _),
					not ClauseLang = mercury
				),
				ClauseList0)),
			ModeIds = list__delete_elems(ModeIds0, ForeignModeIds),
			( ModeIds = [] ->
				ClauseList = ClauseList0
			;
				% XXX we should avoid append - this gives O(N*N)
				list__append(ClauseList0, 
					[clause(ModeIds, Goal, mercury,
						Context)], ClauseList)
			)
		;
			% XXX we should avoid append - this gives O(N*N)
			list__append(ClauseList0, [clause(ModeIds0, Goal,
				mercury, Context)], ClauseList)
		},
		{ qual_info_get_var_types(Info, ExplicitVarTypes) },
		{ ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
				TVarNameMap, InferredVarTypes, HeadVars,
				ClauseList, TI_VarMap, TCI_VarMap,
				HasForeignClauses) }
	).

%-----------------------------------------------------------------------------

:- type foreign_proc_action
	--->	ignore
	;	add
	; 	split_add(int, clause)
	;	replace(int).


% Add the pragma_foreign_proc goal to the clauses_info for this procedure.
% To do so, we must also insert unifications between the variables in the
% pragma foreign_proc declaration and the head vars of the pred. Also
% return the hlds_goal.

:- pred clauses_info_add_pragma_foreign_proc(
	clauses_info::in, purity::in, pragma_foreign_proc_attributes::in,
	pred_id::in, proc_id::in, prog_varset::in, list(pragma_var)::in,
	list(type)::in, pragma_foreign_code_impl::in, prog_context::in,
	pred_or_func::in, sym_name::in, arity::in, clauses_info::out,
	module_info::in, module_info::out, qual_info::in,
	qual_info::out, io__state::di, io__state::uo) is det.

clauses_info_add_pragma_foreign_proc(ClausesInfo0, Purity, Attributes0, PredId,
		ProcId, PVarSet, PVars, OrigArgTypes, PragmaImpl0, Context,
		PredOrFunc, PredName, Arity, ClausesInfo, ModuleInfo0,
		ModuleInfo, Info0, Info) -->

	{ ClausesInfo0 = clauses_info(VarSet0, VarTypes, TVarNameMap,
		VarTypes1, HeadVars, ClauseList, TI_VarMap, TCI_VarMap,
		_HasForeignClauses) },


		% Find all the existing clauses for this mode, and
		% extract their implementation language and clause number
		% (that is, their index in the list).
	{ foreign_language(Attributes0, NewLang) },

	globals__io_get_globals(Globals),
	globals__io_get_target(Target),


		% We traverse the clauses, and decide which action to perform.
		%
		% If there are no clauses, we will simply add this clause.
		%
		% If there are matching foreign_proc clauses for this proc_id,
		% we will either replace them or ignore the new clause
		% (depending on the preference of the two foreign languages).
		%
		% If there is a matching Mercury clause for this proc_id, we
		% will either
		% 	- replace it if there is only one matching mode in its
		% 	  proc_id list.
		%	- remove the matching proc_id from its proc_id list,
		%	  and add this clause as a new clause for this mode.


	{ list__foldl2(
		(pred(C::in, Action0::in, Action::out, N0::in, N::out) is det :-
			C = clause(ProcIds, B, ClauseLang, D),
			( 
				ClauseLang = mercury,
				ProcIds = [ProcId]
			->
				Action = replace(N0)
			;
				ClauseLang = mercury,
				list__delete_first(ProcIds, ProcId,
					MercuryProcIds)
			->
				NewMercuryClause = clause(
					MercuryProcIds, B, ClauseLang, D),
				Action = split_add(N0, NewMercuryClause)	
			;
				ClauseLang = foreign_language(OldLang),
				list__member(ProcId, ProcIds)
			->
				(
					yes = foreign__prefer_foreign_language(
						Globals, Target, OldLang,
						NewLang)
				->

					% This language is preferred to the old
					% language, so we should replace it
					Action = replace(N0)
				;
					% Just ignore it.
					Action = ignore
				)
			;
				Action = Action0
			),
			N = N0 + 1
		), ClauseList, add, FinalAction, 1, _) },

	{ UpdateClauses = (pred(NewCl::in, Cs::out) is det :-
		( FinalAction = ignore,
			Cs = ClauseList
		; FinalAction = add,
			Cs = [NewCl | ClauseList]
		; FinalAction = replace(X),
			list__replace_nth_det(ClauseList, X, NewCl, Cs)
		; FinalAction = split_add(X, Clause),
			list__replace_nth_det(ClauseList, X, Clause, Cs1),
			Cs = [NewCl | Cs1]
		)
	) },



	globals__io_get_backend_foreign_languages(BackendForeignLanguages),
	{ 
	pragma_get_vars(PVars, Args0),
	pragma_get_var_infos(PVars, ArgInfo),

	%
	% If the foreign language not one of the backend 
	% languages, we will have to generate an interface to it in a
	% backend language.
	%
	foreign__extrude_pragma_implementation(BackendForeignLanguages,
		PVars, PredName, PredOrFunc, Context,
		ModuleInfo0, Attributes0, PragmaImpl0,
		ModuleInfo1, Attributes, PragmaImpl),

	%
	% Check for arguments occurring multiple times.
	%
	bag__init(ArgBag0),
	bag__insert_list(ArgBag0, Args0, ArgBag),
	bag__to_assoc_list(ArgBag, ArgBagAL0),
	list__filter(
		(pred(Arg::in) is semidet :-
			Arg = _ - Occurrences,
			Occurrences > 1
		), ArgBagAL0, ArgBagAL),
	assoc_list__keys(ArgBagAL, MultipleArgs)
	},

	( { MultipleArgs = [_ | _] } ->
		{ ClausesInfo = ClausesInfo0 },
		{ ModuleInfo = ModuleInfo1 },
		{ Info = Info0 },
		prog_out__write_context(Context),
		io__write_string(
			"In `:- pragma foreign_proc' declaration for "),
		{ adjust_func_arity(PredOrFunc, OrigArity, Arity) },
		hlds_out__write_simple_call_id(
			PredOrFunc - PredName/OrigArity),
		io__write_string(":\n"),
		prog_out__write_context(Context),
		io__write_string("  error: "),
		(
			{ MultipleArgs = [MultipleArg] },
			io__write_string("variable `"),
			mercury_output_var(MultipleArg, PVarSet, no),
			io__write_string("' occurs multiple times\n")
		;
			{ MultipleArgs = [_, _ | _] },
			io__write_string("variables `"),
			mercury_output_vars(MultipleArgs, PVarSet, no),
			io__write_string(
				"' occur multiple times\n")
		),
		prog_out__write_context(Context),
		io__write_string("  in the argument list.\n"),
		io__set_exit_status(1)
	;
		% merge the varsets of the proc and the new pragma_c_code
		{
		varset__merge_subst(VarSet0, PVarSet, VarSet1, Subst),
		map__apply_to_list(Args0, Subst, TermArgs),
		term__term_list_to_var_list(TermArgs, Args),

			% build the pragma_c_code
		goal_info_init(GoalInfo0),
		goal_info_set_context(GoalInfo0, Context, GoalInfo1),
		% Put the purity in the goal_info in case
		% this foreign code is inlined
		add_goal_info_purity_feature(GoalInfo1, Purity, GoalInfo),
		HldsGoal0 = foreign_proc(Attributes, PredId, 
			ProcId, Args, ArgInfo, OrigArgTypes, PragmaImpl)
			- GoalInfo
		}, 
			% Apply unifications with the head args.
			% Since the set of head vars and the set vars in the
			% pragma foreign code are disjoint, the
			% unifications can be implemented as
			% substitutions, and they will be.
		insert_arg_unifications(HeadVars, TermArgs, Context,
			head(PredOrFunc, Arity), yes, HldsGoal0, VarSet1,
			HldsGoal1, VarSet2, transform_info(ModuleInfo1, Info0),
				transform_info(ModuleInfo, Info)),
		{
		map__init(EmptyVarTypes),
		implicitly_quantify_clause_body(HeadVars,
			HldsGoal1, VarSet2, EmptyVarTypes,
			HldsGoal, VarSet, _, _Warnings),
		NewClause = clause([ProcId], HldsGoal,
			foreign_language(NewLang), Context),
		UpdateClauses(NewClause, NewClauseList),
		HasForeignClauses = yes,
		ClausesInfo =  clauses_info(VarSet, VarTypes, TVarNameMap,
			VarTypes1, HeadVars, NewClauseList,
			TI_VarMap, TCI_VarMap, HasForeignClauses)
		}
	).

:- pred allocate_vars_for_saved_vars(list(string), list(pair(prog_var, string)),
	prog_varset, prog_varset).
:- mode allocate_vars_for_saved_vars(in, out, in, out) is det.

allocate_vars_for_saved_vars([], [], VarSet, VarSet).
allocate_vars_for_saved_vars([Name | Names], [Var - Name | VarNames],
		VarSet0, VarSet) :-
	varset__new_var(VarSet0, Var, VarSet1),
	allocate_vars_for_saved_vars(Names, VarNames, VarSet1, VarSet).

%-----------------------------------------------------------------------------

:- type transform_info ---> 
	transform_info(
		module_info	:: module_info,
		qual_info	:: qual_info
	).

:- pred transform(prog_substitution, list(prog_var), list(prog_term), goal,
		prog_varset, prog_context, pred_or_func, arity, bool,
		hlds_goal, prog_varset, list(quant_warning),
		transform_info, transform_info,
		io__state, io__state).
:- mode transform(in, in, in, in, in, in, in, in, in, out, out, out,
		in, out, di, uo) is det.

transform(Subst, HeadVars, Args0, Body, VarSet0, Context, PredOrFunc,
		Arity, IsAssertion, Goal, VarSet, Warnings, Info0, Info) -->
	transform_goal(Body, VarSet0, Subst, Goal1, VarSet1, Info0, Info1),
	{ term__apply_substitution_to_list(Args0, Subst, Args) },
		
		% The head variables of an assertion will always be
		% variables, so it is unnecessary to insert unifications.
	(
		{ IsAssertion = yes }
	->
		{ VarSet2 = VarSet1 },
		{ Goal2 = Goal1 },
		{ Info2 = Info0 }
	;
		{ ArgContext = head(PredOrFunc, Arity) },
		insert_arg_unifications(HeadVars, Args, Context, ArgContext,
			no, Goal1, VarSet1, Goal2, VarSet2, Info1, Info2)
	),
	{ VarTypes2 = Info2 ^ qual_info ^ vartypes },
	{ implicitly_quantify_clause_body(HeadVars, Goal2, VarSet2, VarTypes2,
		Goal, VarSet, VarTypes, Warnings) },
	{ Info = Info2 ^ qual_info ^ vartypes := VarTypes }.

%-----------------------------------------------------------------------------%

	% Convert goals from the prog_data `goal' structure into the
	% hlds `hlds_goal' structure.  At the same time, convert
	% it to super-homogeneous form by unravelling all the complex
	% unifications, and annotate those unifications with a unify_context
	% so that we can still give good error messages.
	% And also at the same time, apply the given substitution to
	% the goal, to rename it apart from the other clauses.

:- pred transform_goal(goal, prog_varset, prog_substitution, hlds_goal,
		prog_varset, transform_info, transform_info,
		io__state, io__state).
:- mode transform_goal(in, in, in, out, out, in, out, di, uo) is det.

transform_goal(Goal0 - Context, VarSet0, Subst, Goal1 - GoalInfo1, VarSet,
		Info0, Info) -->
	transform_goal_2(Goal0, Context, VarSet0, Subst, Goal1 - GoalInfo0,
					VarSet, Info0, Info),
	{ goal_info_set_context(GoalInfo0, Context, GoalInfo1) }.

:- pred transform_goal_2(goal_expr, prog_context, prog_varset,
		prog_substitution, hlds_goal, prog_varset,
		transform_info, transform_info, io__state, io__state).
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
		some(Vars, can_remove, Goal) - GoalInfo,
		VarSet, Info0, Info) -->
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
	{ goal_info_init(GoalInfo) },
	{ Goal = not(A) - GoalInfo }.

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

transform_goal_2(equivalent(P0, Q0), _Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	%
	% `P <=> Q' is defined as `(P => Q), (Q => P)',
	% but that transformation must not be done until
	% after quantification analysis, lest the duplication of
	% the goals concerned affect the implicit quantification
	% of the variables inside them.
	%
	{ goal_info_init(GoalInfo) },
	transform_goal(P0, VarSet0, Subst, P, VarSet1, Info0, Info1),
	transform_goal(Q0, VarSet1, Subst, Q, VarSet, Info1, Info),
	{ Goal = shorthand(bi_implication(P, Q)) - GoalInfo }.

transform_goal_2(call(Name, Args0, Purity), Context, VarSet0, Subst, Goal,
		VarSet, Info0, Info) -->
	( 
		{ Name = unqualified("\\=") },
		{ Args0 = [LHS, RHS] }
	->
			% `LHS \= RHS' is defined as `not (LHS = RHS)'
		transform_goal_2(not(unify(LHS, RHS, Purity) - Context),
			Context, VarSet0, Subst, Goal, VarSet, Info0, Info)
	;
		% check for a DCG field access goal:
		% get:  Field =^ field
		% set:  ^ field := Field
		{ Name = unqualified(Operator) },
		( { Operator = "=^" }
		; { Operator = ":=" }
		)
	->
		{ term__apply_substitution_to_list(Args0, Subst, Args1) },
		transform_dcg_record_syntax(Operator, Args1, Context,
			VarSet0, Goal, VarSet, Info0, Info)
	;
		% check for an Aditi builtin
		{ Purity = pure },
		{ Name = unqualified(Name1) },
		{ Name1 = "aditi_insert"
		; Name1 = "aditi_delete"
		; Name1 = "aditi_bulk_insert"
		; Name1 = "aditi_bulk_delete"
		; Name1 = "aditi_bulk_modify"

		% These are not yet implemented in Aditi.
		%; Name1 = "aditi_filter"
		%; Name1 = "aditi_modify"
		}
	->
		{ term__apply_substitution_to_list(Args0, Subst, Args1) },
		transform_aditi_builtin(Name1, Args1, Context, VarSet0,
			Goal, VarSet, Info0, Info)
	;
		{ term__apply_substitution_to_list(Args0, Subst, Args) },
		{ make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1) },
		{ list__length(Args, Arity) },
		(
			% check for a higher-order call,
			% i.e. a call to either call/N or ''/N.
			{ Name = unqualified("call")
			; Name = unqualified("")
			},
			{ HeadVars = [PredVar | RealHeadVars] }
		->
			{
			  % initialize some fields to junk
			  Modes = [],
			  Det = erroneous,

			  GenericCall = higher_order(PredVar,
			  	predicate, Arity),
			  Call = generic_call(GenericCall,
			  	RealHeadVars, Modes, Det),

			  hlds_goal__generic_call_id(GenericCall, CallId),
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
			{
			  % initialize some fields to junk
			  invalid_pred_id(PredId),
			  invalid_proc_id(ModeId),

			  MaybeUnifyContext = no,
			  Call = call(PredId, ModeId, HeadVars, not_builtin,
				      MaybeUnifyContext, Name),
			  CallId = call(predicate - Name/Arity),
			  Purity1 = Purity
			}
		),
		{ goal_info_init(Context, GoalInfo0) },
		{ add_goal_info_purity_feature(GoalInfo0,
			Purity1, GoalInfo) },
		{ Goal0 = Call - GoalInfo },

		{ record_called_pred_or_func(predicate, Name, Arity,
			Info0, Info1) },
		insert_arg_unifications(HeadVars, Args,
			Context, call(CallId), no,
			Goal0, VarSet1, Goal, VarSet, Info1, Info)
	).

transform_goal_2(unify(A0, B0, Purity), Context, VarSet0, Subst, Goal, VarSet,
		Info0, Info) -->
	{ term__apply_substitution(A0, Subst, A) },
	{ term__apply_substitution(B0, Subst, B) },
	unravel_unification(A, B, Context, explicit, [],
			VarSet0, Purity, Goal, VarSet, Info0, Info).
	

:- inst dcg_record_syntax_op = bound("=^"; ":=").

:- pred transform_dcg_record_syntax(string, list(prog_term), prog_context,
		prog_varset, hlds_goal, prog_varset, transform_info,
		transform_info, io__state, io__state).
:- mode transform_dcg_record_syntax(in(dcg_record_syntax_op),
		in, in, in, out, out, in, out, di, uo) is det.

transform_dcg_record_syntax(Operator, ArgTerms0, Context, VarSet0,
		Goal, VarSet, Info0, Info) -->
	{ goal_info_init(Context, GoalInfo) },
	(
		{ ArgTerms0 = [LHSTerm, RHSTerm,
				TermInputTerm, TermOutputTerm] },
		{
			Operator = "=^",
			AccessType = get,
			FieldNameTerm = RHSTerm,
			FieldValueTerm = LHSTerm
		;
			Operator = ":=",
			AccessType = set,
			LHSTerm = term__functor(term__atom("^"),
				[FieldNameTerm0], _),
			FieldNameTerm = FieldNameTerm0,
			FieldValueTerm = RHSTerm
		}
	->
		{ parse_field_list(FieldNameTerm, MaybeFieldNames) },
		(
			{ MaybeFieldNames = ok(FieldNames) },
			{ ArgTerms = [FieldValueTerm, TermInputTerm,
					TermOutputTerm] },

			transform_dcg_record_syntax_2(AccessType,
				FieldNames, ArgTerms, Context, VarSet0, Goal,
				VarSet, Info0, Info)
		;
			{ MaybeFieldNames = error(Msg, ErrorTerm) },
			{ invalid_goal("^", ArgTerms0, GoalInfo,
				Goal, VarSet0, VarSet) },
			{ qual_info_set_found_syntax_error(yes, 
				Info0 ^ qual_info, QualInfo) },
			{ Info = Info0 ^ qual_info := QualInfo },
			io__set_exit_status(1),
			prog_out__write_context(Context),
			io__write_string("In DCG field "),
			(
				{ AccessType = set },
				io__write_string("update")
			;
				{ AccessType = get },
				io__write_string("extraction")
			),
			io__write_string(" goal:\n"),
			prog_out__write_context(Context),
			io__write_string("  error: "),
			io__write_string(Msg),
			io__write_string(" at term `"),
			term_io__write_term(VarSet, ErrorTerm),
			io__write_string("'.\n")
		)
	;
		{ invalid_goal("^", ArgTerms0, GoalInfo,
			Goal, VarSet0, VarSet) },
		{ qual_info_set_found_syntax_error(yes, Info0 ^ qual_info,
			QualInfo) },
		{ Info = Info0 ^ qual_info := QualInfo },
		io__set_exit_status(1),
		prog_out__write_context(Context),
		io__write_string(
		"Error: expected `Field =^ field1 ^ ... ^ fieldN'\n"),
		prog_out__write_context(Context),
		io__write_string("  or `^ field1 ^ ... ^ fieldN := Field'.\n"),
		prog_out__write_context(Context),
		io__write_string("  in DCG field access goal.\n")
	).

:- pred transform_dcg_record_syntax_2(field_access_type,
		field_list, list(prog_term), prog_context,
		prog_varset, hlds_goal, prog_varset,
		transform_info, transform_info, io__state, io__state).
:- mode transform_dcg_record_syntax_2(in, in, in, in, in, out, out,
		in, out, di, uo) is det.

transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms, Context,
		VarSet0, Goal, VarSet, Info0, Info, IO0, IO) :-
	make_fresh_arg_vars(ArgTerms, VarSet0, ArgVars, VarSet1),
	( ArgVars = [FieldValueVar, TermInputVar, TermOutputVar] ->
		(
			AccessType = set,
			expand_set_field_function_call(Context, explicit, [],
				FieldNames, FieldValueVar, TermInputVar,
				TermOutputVar, VarSet1, VarSet2, Functor,
				InnermostFunctor - InnermostSubContext, Goal0,
				Info0, Info1, IO0, IO1),


			FieldArgNumber = 2,
			FieldArgContext = functor(InnermostFunctor, explicit,
					InnermostSubContext),
			InputTermArgNumber = 1,
			InputTermArgContext = functor(Functor, explicit, []),
			( Functor = cons(FuncName0, FuncArity0) ->
				FuncName = FuncName0,
				FuncArity = FuncArity0
			;
				error("transform_dcg_record_syntax_2")
			),
			% DCG arguments should always be distinct variables,
			% so this context should never be used.
			OutputTermArgNumber = 3,
			OutputTermArgContext = call(
				call(function - FuncName/FuncArity)),

			ArgContexts = [
				FieldArgNumber - FieldArgContext,
				InputTermArgNumber - InputTermArgContext,
				OutputTermArgNumber - OutputTermArgContext
			],
			insert_arg_unifications_with_supplied_contexts(ArgVars,
				ArgTerms, ArgContexts, Context, Goal0, VarSet2,
				Goal, VarSet, Info1, Info, IO1, IO)
		;
			AccessType = get,
			expand_dcg_field_extraction_goal(Context, explicit,
				[], FieldNames, FieldValueVar, TermInputVar,
				TermOutputVar, VarSet1, VarSet2, Functor,
				InnermostFunctor - _InnerSubContext, Goal0,
				Info0, Info1, IO0, IO1),
			InputTermArgNumber = 1,
			InputTermArgContext = functor(Functor, explicit, []),

			( InnermostFunctor = cons(FuncName0, FuncArity0) ->
				FuncName = FuncName0,
				FuncArity = FuncArity0
			;
				error("transform_dcg_record_syntax_2")
			),
			FieldArgNumber = 2,
			FieldArgContext = call(
					call(function - FuncName/FuncArity)),

			% DCG arguments should always be distinct variables,
			% so this context should never be used.
			OutputTermArgNumber = 1,
			OutputTermArgContext = functor(Functor, explicit, []),
			ArgContexts = [
				FieldArgNumber - FieldArgContext,
				InputTermArgNumber - InputTermArgContext,
				OutputTermArgNumber - OutputTermArgContext
			],
			insert_arg_unifications_with_supplied_contexts(ArgVars,
				ArgTerms, ArgContexts, Context, Goal0, VarSet2,
				Goal, VarSet, Info1, Info, IO1, IO)
		)
	;
		error("make_hlds__do_transform_dcg_record_syntax")
	).

	% Expand a field update goal into a list of goals which
	% each get or set one level of the structure.
	%
	% A field update goal:
	% 	Term = Term0 ^ module_info ^ ctors :=  Ctors
	% is expanded into
	%	V_1 = Term0 ^ module_info,
	%	V_3 = V_2 ^ ctors := Ctors,
	%	Term = Term0 ^ module_info := V_3.
	%
:- pred expand_set_field_function_call(prog_context,
		unify_main_context, unify_sub_contexts,
		field_list, prog_var, prog_var,
		prog_var, prog_varset, prog_varset, cons_id,
		pair(cons_id, unify_sub_contexts), hlds_goal,
		transform_info, transform_info, io__state, io__state).
:- mode expand_set_field_function_call(in, in, in, in, in, in,
		in, in, out, out, out, out, in, out, di, uo) is det.

expand_set_field_function_call(Context, MainContext, SubContext0,
		FieldNames, FieldValueVar, TermInputVar,
		TermOutputVar, VarSet0, VarSet,
		Functor, FieldSubContext, Goal, Info0, Info) -->
	expand_set_field_function_call_2(Context, MainContext,
		SubContext0, FieldNames, FieldValueVar, TermInputVar,
		TermOutputVar, VarSet0, VarSet,
		Functor, FieldSubContext, Goals, Info0, Info),
	{ goal_info_init(Context, GoalInfo) },
	{ conj_list_to_goal(Goals, GoalInfo, Goal) }.

:- pred expand_set_field_function_call_2(prog_context,
		unify_main_context, unify_sub_contexts,
		field_list, prog_var, prog_var,
		prog_var, prog_varset, prog_varset, cons_id,
		pair(cons_id, unify_sub_contexts), list(hlds_goal),
		transform_info, transform_info, io__state, io__state).
:- mode expand_set_field_function_call_2(in, in, in, in, in, in,
		in, in, out, out, out, out, in, out, di, uo) is det.

expand_set_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _, _, _, _) -->
	{ error(
	"expand_set_field_function_call_2: empty list of field names") }.
expand_set_field_function_call_2(Context, MainContext, SubContext0,
		[FieldName - FieldArgs | FieldNames], FieldValueVar,
		TermInputVar, TermOutputVar, VarSet0, VarSet, Functor,
		FieldSubContext, Goals, Info0, Info) -->
	{ make_fresh_arg_vars(FieldArgs, VarSet0, FieldArgVars, VarSet1) },
	( { FieldNames = [_|_] } ->
		{ varset__new_var(VarSet1, SubTermInputVar, VarSet2) },
		{ varset__new_var(VarSet2, SubTermOutputVar, VarSet3) },
		{ SetArgs = list__append(FieldArgVars,
				[TermInputVar, SubTermOutputVar]) },
		{ construct_field_access_function_call(set, Context,
			MainContext, SubContext0, FieldName,
			TermOutputVar, SetArgs,
			Functor, UpdateGoal, Info0, Info1) },

		% extract the field containing the field to update.
		{ construct_field_access_function_call(get, Context,
			MainContext, SubContext0, FieldName, SubTermInputVar,
			list__append(FieldArgVars, [TermInputVar]), _,
			GetSubFieldGoal, Info1, Info2) },

		% recursively update the field.
		{ SubTermInputArgNumber = 2 + list__length(FieldArgs) },
		{ TermInputContext = Functor - SubTermInputArgNumber },
		{ SubContext = [TermInputContext | SubContext0] },
		expand_set_field_function_call_2(Context, MainContext,
			SubContext, FieldNames, FieldValueVar, SubTermInputVar,
			SubTermOutputVar, VarSet3, VarSet4, _,
			FieldSubContext, Goals0, Info2, Info3),

		{ list__append([GetSubFieldGoal | Goals0],
			[UpdateGoal], Goals1) }
	;
		{ VarSet4 = VarSet1 },
		{ SetArgs = list__append(FieldArgVars,
				[TermInputVar, FieldValueVar]) },
		{ construct_field_access_function_call(set, Context,
			MainContext, SubContext0, FieldName, TermOutputVar,
			SetArgs, Functor, Goal, Info0, Info3) },
		{ FieldSubContext = Functor - SubContext0 },
		{ Goals1 = [Goal] }

	),
	{ ArgContext = functor(Functor, MainContext, SubContext0) },
	{ goal_info_init(Context, GoalInfo) },
	{ conj_list_to_goal(Goals1, GoalInfo, Conj0) },
	append_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
		Conj0, VarSet4, Conj, VarSet, Info3, Info),
	{ goal_to_conj_list(Conj, Goals) }.

	% Expand a field extraction goal into a list of goals which
	% each get one level of the structure.
	%
	% A field extraction goal:
	%	:= (ModuleName, ^ module_info ^ sub_info ^ module_name,
	%		DCG_in, DCG_out).
	% is expanded into
	%	DCG_out = DCG_in,
	%	V_1 = DCG_out ^ module_info
	%	V_2 = V_1 ^ sub_info,
	%	ModuleName = V_2 ^ module_name.
	%
:- pred expand_dcg_field_extraction_goal(prog_context, unify_main_context,
		unify_sub_contexts, field_list, prog_var, prog_var,
		prog_var, prog_varset, prog_varset, cons_id,
		pair(cons_id, unify_sub_contexts), hlds_goal,
		transform_info, transform_info, io__state, io__state).
:- mode expand_dcg_field_extraction_goal(in, in, in, in, in,
		in, in, in, out, out, out, out, in, out, di, uo) is det.

expand_dcg_field_extraction_goal(Context, MainContext, SubContext,
		FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
		VarSet0, VarSet, Functor, FieldSubContext,
		Goal, Info0, Info) -->
	% unify the DCG input and output variables
	{ make_atomic_unification(TermOutputVar, var(TermInputVar),
			Context, MainContext, SubContext, UnifyDCG,
			Info0, Info1) },

	% process the access function as a get function on
	% the output DCG variable
	expand_get_field_function_call_2(Context, MainContext, SubContext,
		FieldNames, FieldValueVar, TermOutputVar, VarSet0, VarSet,
		Functor, FieldSubContext, Goals1, Info1, Info),
	{ Goals = [UnifyDCG | Goals1] },
	{ goal_info_init(Context, GoalInfo) },
	{ conj_list_to_goal(Goals, GoalInfo, Goal) }.
	
	% Expand a field extraction function call into a list of goals which
	% each get one level of the structure.
	%
	% A field extraction goal:
	% 	ModuleName = Info ^ module_info ^ sub_info ^ module_name
	% is expanded into
	%	V_1 = Info ^ module_info,
	%	V_2 = V_1 ^ sub_info,
	%	ModuleName = V_2 ^ module_name.
	%
:- pred expand_get_field_function_call(prog_context, unify_main_context,
		unify_sub_contexts, field_list, prog_var,
		prog_var, prog_varset, prog_varset, cons_id,
		pair(cons_id, unify_sub_contexts), hlds_goal,
		transform_info, transform_info, io__state, io__state).
:- mode expand_get_field_function_call(in, in, in, in, in,
		in, in, out, out, out, out, in, out, di, uo) is det.

expand_get_field_function_call(Context, MainContext, SubContext0,
		FieldNames, FieldValueVar, TermInputVar, VarSet0, VarSet,
		Functor, FieldSubContext, Goal, Info0, Info) -->
	expand_get_field_function_call_2(Context, MainContext, SubContext0,
		FieldNames, FieldValueVar, TermInputVar,
		VarSet0, VarSet, Functor, FieldSubContext, Goals, Info0, Info),
	{ goal_info_init(Context, GoalInfo) },
	{ conj_list_to_goal(Goals, GoalInfo, Goal) }.

:- pred expand_get_field_function_call_2(prog_context, unify_main_context,
		unify_sub_contexts, field_list, prog_var,
		prog_var, prog_varset, prog_varset, cons_id,
		pair(cons_id, unify_sub_contexts), list(hlds_goal),
		transform_info, transform_info, io__state, io__state).
:- mode expand_get_field_function_call_2(in, in, in, in, in,
		in, in, out, out, out, out, in, out, di, uo) is det.

expand_get_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _, _, _) -->
	{ error(
	"expand_get_field_function_call_2: empty list of field names") }.
expand_get_field_function_call_2(Context, MainContext, SubContext0,
		[FieldName - FieldArgs | FieldNames], FieldValueVar,
		TermInputVar, VarSet0, VarSet, Functor,
		FieldSubContext, Goals, Info0, Info) -->
	{ make_fresh_arg_vars(FieldArgs, VarSet0, FieldArgVars, VarSet1) },
	{ GetArgVars = list__append(FieldArgVars, [TermInputVar]) },
	( { FieldNames = [_|_] } ->
		{ varset__new_var(VarSet1, SubTermInputVar, VarSet2) },
		{ construct_field_access_function_call(get, Context,
			MainContext, SubContext0, FieldName, SubTermInputVar,
			GetArgVars, Functor, Goal, Info0, Info1) },

		% recursively extract until we run out of field names
		{ TermInputArgNumber = 1 + list__length(FieldArgVars) },
		{ TermInputContext = Functor - TermInputArgNumber },
		{ SubContext = [TermInputContext | SubContext0] },
		expand_get_field_function_call_2(Context, MainContext,
			SubContext, FieldNames, FieldValueVar, SubTermInputVar,
			VarSet2, VarSet3, _, FieldSubContext,
			Goals1, Info1, Info2),
		{ Goals2 = [Goal | Goals1] }
	;
		{ VarSet3 = VarSet1 },
		{ FieldSubContext = Functor - SubContext0 },
		{ construct_field_access_function_call(get, Context,
			MainContext, SubContext0, FieldName, FieldValueVar,
			GetArgVars, Functor, Goal, Info0, Info2) },
		{ Goals2 = [Goal] }
	),
	{ ArgContext = functor(Functor, MainContext, SubContext0) },
	{ goal_info_init(Context, GoalInfo) },
	{ conj_list_to_goal(Goals2, GoalInfo, Conj0) },
	append_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
		Conj0, VarSet3, Conj, VarSet, Info2, Info),
	{ goal_to_conj_list(Conj, Goals) }.

:- pred construct_field_access_function_call(field_access_type, prog_context,
		unify_main_context, unify_sub_contexts, ctor_field_name,
		prog_var, list(prog_var), cons_id, hlds_goal,
		transform_info, transform_info).
:- mode construct_field_access_function_call(in, in, in, in, in,
		in, in, out, out, in, out) is det.

construct_field_access_function_call(AccessType, Context,
		MainContext, SubContext, FieldName, RetArg, Args,
		Functor, Goal, Info0, Info) :-
	field_access_function_name(AccessType, FieldName, FuncName),
	list__length(Args, Arity),
	Functor = cons(FuncName, Arity),
	make_atomic_unification(RetArg, functor(Functor, Args),
		Context, MainContext, SubContext, Goal, Info0, Info).

:- type field_list == assoc_list(ctor_field_name, list(prog_term)).

:- pred parse_field_list(prog_term,
		maybe1(field_list, prog_var_type)).
:- mode parse_field_list(in, out) is det.

parse_field_list(Term, MaybeFieldNames) :-
	(
		Term = term__functor(term__atom("^"),
			[FieldNameTerm, OtherFieldNamesTerm], _)
	->
		(
			parse_qualified_term(FieldNameTerm, FieldNameTerm,
				"field name", Result),
			Result = ok(FieldName, Args)
		->
			parse_field_list(OtherFieldNamesTerm,
				MaybeFieldNames1),
			(
				MaybeFieldNames1 = error(_, _),
				MaybeFieldNames = MaybeFieldNames1
			;
				MaybeFieldNames1 = ok(FieldNames1),
				MaybeFieldNames =
					ok([FieldName - Args | FieldNames1])
			)
		;
			MaybeFieldNames = error("expected field name",
				FieldNameTerm)
		)
	;
		(
			parse_qualified_term(Term, Term, "field name", Result),
			Result = ok(FieldName, Args)
		->
			MaybeFieldNames = ok([FieldName - Args])
		;	
			MaybeFieldNames = error("expected field name",
				Term)
		)
	).

%-----------------------------------------------------------------------------%

:- inst aditi_update_str =
	bound(	"aditi_insert"
	;	"aditi_delete"
	;	"aditi_bulk_insert"
	;	"aditi_bulk_delete"
	;	"aditi_filter"
	;	"aditi_bulk_modify"
	;	"aditi_modify"
	).

	% See the "Aditi update syntax" section of the
	% Mercury Language Reference Manual.
:- pred transform_aditi_builtin(string, list(prog_term), prog_context,
		prog_varset, hlds_goal, prog_varset,
		transform_info, transform_info, io__state, io__state).
:- mode transform_aditi_builtin(in(aditi_update_str), in,
		in, in, out, out, in, out, di, uo) is det.

transform_aditi_builtin(UpdateStr, Args0, Context, VarSet0,
		Goal, VarSet, Info0, Info) -->
	(
		{ UpdateStr = "aditi_insert", InsertDelete = insert
		; UpdateStr = "aditi_delete", InsertDelete = delete
		}
	->
		transform_aditi_tuple_insert_delete(UpdateStr, InsertDelete,
			Args0, Context, VarSet0, Goal,
			VarSet, Info0, Info)
	;
		{
			UpdateStr = "aditi_insert",
			% This is handled above
			error("transform_aditi_builtin: aditi_insert")
		;
			UpdateStr = "aditi_delete",
			% This is handled above
			error("transform_aditi_builtin: aditi_delete")
		;
			UpdateStr = "aditi_bulk_insert",
			Update = bulk_insert
		;
			UpdateStr = "aditi_bulk_delete",
			Update = delete(bulk)
		;
			UpdateStr = "aditi_bulk_modify",
			Update = modify(bulk)
		;
			UpdateStr = "aditi_filter",
			% not yet implemented
			Update = delete(filter)
		;
			UpdateStr = "aditi_modify",
			% not yet implemented
			Update = modify(filter)
		},
		transform_aditi_insert_delete_modify(UpdateStr,
			Update, Args0, Context, VarSet0, Goal,
			VarSet, Info0, Info)
		
	).

:- pred transform_aditi_tuple_insert_delete(string, aditi_insert_delete,
		list(prog_term), prog_context,
		prog_varset, hlds_goal, prog_varset,
		transform_info, transform_info, io__state, io__state).
:- mode transform_aditi_tuple_insert_delete(in, in, in, in,
		in, out, out, in, out, di, uo) is det.

transform_aditi_tuple_insert_delete(UpdateStr, InsertDelete, Args0, Context,
		VarSet0, Goal, VarSet, Info0, Info) -->
	% Build an empty goal_info. 
	{ goal_info_init(Context, GoalInfo) },

	%
	% Syntax -
	% aditi_insert(p(_DB, X, Y), DB0, DB).
	%
	% `p(_DB, X, Y)' is the tuple to insert, not a higher-order term.
	%
	( { Args0 = [InsertTupleTerm, AditiState0Term, AditiStateTerm] } ->
		(
			% Parse the tuple to insert.
			{ parse_pred_or_func_and_args(InsertTupleTerm,
				PredOrFunc, SymName, TupleArgTerms) }
		->
			{
			%
			% Make new variables for the arguments.
			% The argument list of the `aditi_insert'
			% goal contains the arguments of the tuple
			% to insert and the `aditi__state' arguments.
			%
			make_fresh_arg_var(AditiState0Term, AditiState0Var, [],
				VarSet0, VarSet1),
			make_fresh_arg_var(AditiStateTerm, AditiStateVar, [],
				VarSet1, VarSet2),
			make_fresh_arg_vars(TupleArgTerms, VarSet2,
				TupleArgVars, VarSet3),
			list__append(TupleArgVars,
				[AditiState0Var, AditiStateVar], AllArgs),
			list__length(TupleArgVars, InsertArity),

			invalid_pred_id(PredId),
			Builtin = aditi_tuple_insert_delete(InsertDelete,
					PredId),
			InsertCallId = PredOrFunc - SymName/InsertArity,
			Call = generic_call(
				aditi_builtin(Builtin, InsertCallId),
				AllArgs, [], det),
			Goal0 = Call - GoalInfo,
			CallId = generic_call(aditi_builtin(Builtin,
				InsertCallId)),
			list__append(TupleArgTerms,
				[AditiState0Term, AditiStateTerm],
				AllArgTerms)
			},

			{ record_called_pred_or_func(PredOrFunc, SymName, 
				InsertArity, Info0, Info1) },
			insert_arg_unifications(AllArgs, AllArgTerms,
				Context, call(CallId), no,
				Goal0, VarSet3, Goal, VarSet, Info1, Info)
		;
			{ invalid_goal(UpdateStr, Args0, GoalInfo,
				Goal, VarSet0, VarSet) },
			{ qual_info_set_found_syntax_error(yes, 
				Info0 ^ qual_info, QualInfo) },
			{ Info = Info0 ^ qual_info := QualInfo },
			io__set_exit_status(1),
			prog_out__write_context(Context),
			io__write_string("Error: expected tuple to "),
			io__write(InsertDelete),
			io__write_string(" in `"),
			io__write_string(UpdateStr),
			io__write_string("'.\n")
		)
	;
		{ invalid_goal(UpdateStr, Args0, GoalInfo,
			Goal, VarSet0, VarSet) },
		{ qual_info_set_found_syntax_error(yes, Info0 ^ qual_info,
			QualInfo) },
		{ Info = Info0 ^ qual_info := QualInfo },
		{ list__length(Args0, Arity) },
		aditi_update_arity_error(Context, UpdateStr, Arity, [3])
	).

	% Parse an `aditi_delete' or `aditi_modify' goal.
:- pred transform_aditi_insert_delete_modify(string,
		aditi_insert_delete_modify, list(prog_term), prog_context,
		prog_varset, hlds_goal, prog_varset, transform_info, 
		transform_info, io__state, io__state).
:- mode transform_aditi_insert_delete_modify(in, in, in, in, in, out, out,
		in, out, di, uo) is det.

transform_aditi_insert_delete_modify(Descr, InsertDelMod, Args0, Context,
		VarSet0, UpdateGoal, VarSet, Info0, Info) -->
	{ goal_info_init(Context, GoalInfo) },
	(
		{ list__length(Args0, Arity) },
		{ Arity \= 3 },
		{ Arity \= 4 }
	->
		{ invalid_goal(Descr, Args0, GoalInfo,
			UpdateGoal, VarSet0, VarSet) },
		{ qual_info_set_found_syntax_error(yes, Info0 ^ qual_info,
			QualInfo) },
		{ Info = Info0 ^ qual_info := QualInfo },
		aditi_update_arity_error(Context, Descr, Arity, [3, 4])
	;
		%
		% First syntax -
		%	aditi_insert((p(X, Y, _DB0) :- X = 2, Y = 1), DB0, DB).
		% or
		%	aditi_delete((p(X, Y, _DB0) :- X = 2), DB0, DB).
		% or
		% 	aditi_modify((p(X0, Y0, _DB0) ==> p(X0, Y, _DB) :-
		%		X0 < 100, Y = Y0 + 1), DB0, DB).
		%
		{ Args0 = [HOTerm, AditiState0Term, AditiStateTerm] },
		{ parse_rule_term(Context, HOTerm, HeadTerm, GoalTerm1) },
		{ 
			InsertDelMod = bulk_insert,
			parse_pred_or_func_and_args(HeadTerm,
				PredOrFunc, SymName, HeadArgs1),
			list__length(HeadArgs1, PredArity)
		;
			InsertDelMod = delete(_),
			parse_pred_or_func_and_args(HeadTerm,
				PredOrFunc, SymName, HeadArgs1),
			list__length(HeadArgs1, PredArity)
		;
			InsertDelMod = modify(_),
			HeadTerm = term__functor(term__atom("==>"),
				[LeftHeadTerm, RightHeadTerm], _),
			parse_pred_or_func_and_args(LeftHeadTerm,
				PredOrFunc, SymName, LeftHeadArgs),
			parse_pred_or_func_and_args(RightHeadTerm,
				PredOrFunc, SymName, RightHeadArgs),
			list__append(LeftHeadArgs, RightHeadArgs, HeadArgs1),
			list__length(LeftHeadArgs, PredArity),
			list__length(RightHeadArgs, PredArity)
		}
	->
		%
		% This syntax is transformed into a construction of
		% a lambda expression for the modification condition
		% and a call to an update goal with that closure.
		% The transformed code is equivalent to the
		% `sym_name_and_closure' syntax which is parsed below.
		%
		{ Syntax = pred_term },

		%
		% Parse the modification goal as for a lambda expression.
		%
		{ make_fresh_arg_vars(HeadArgs1, VarSet0, HeadArgs, VarSet1) },
		{ term__coerce(GoalTerm1, GoalTerm) },
		{ parse_goal(GoalTerm, VarSet1, ParsedGoal, VarSet2) },
		{ map__init(Substitution) },
		transform_goal(ParsedGoal, VarSet2, Substitution,
			PredGoal0, VarSet3, Info0, Info1),
		{ ArgContext = head(PredOrFunc, PredArity) },
		insert_arg_unifications(HeadArgs, HeadArgs1, Context,
			ArgContext, no, PredGoal0, VarSet3, PredGoal1, VarSet4,
			Info1, Info2),

		% Quantification will reduce this down to
		% the proper set of nonlocal arguments.
		{ goal_util__goal_vars(PredGoal, LambdaGoalVars0) }, 
		{ set__delete_list(LambdaGoalVars0,
			HeadArgs, LambdaGoalVars1) },
		{ set__to_sorted_list(LambdaGoalVars1, LambdaNonLocals) },
		{ aditi_delete_insert_delete_modify_goal_info(InsertDelMod,
			PredOrFunc, SymName, PredArity, HeadArgs,
			LambdaPredOrFunc, EvalMethod, LambdaModes,
			Detism, PredGoal1, PredGoal) },
		{ ModifiedCallId = PredOrFunc - SymName/PredArity },

		{ invalid_pred_id(PredId) },
		{ Builtin = aditi_insert_delete_modify(InsertDelMod,
				PredId, Syntax) },
		{ MainContext =
			call(generic_call(
				aditi_builtin(Builtin, ModifiedCallId)),
			1) }, 
		{ varset__new_var(VarSet4, LambdaVar, VarSet5) },

		% Tell purity.m to change the mode of the `aditi__state'
		% arguments of the closure to `unused', to make sure
		% that the closure does not call any Aditi relations.
		% We don't know which argument is the `aditi__state' until
		% after typechecking.
		% The `aditi__state's are passed even though they are not
		% used to make the arguments of the closure match the
		% arguments of the relation being updated.
		{ FixModes = modes_need_fixing },

		% Build the lambda expression for the modification condition.
		{ make_atomic_unification(LambdaVar,
			lambda_goal(LambdaPredOrFunc, EvalMethod,
				FixModes, LambdaNonLocals,
				HeadArgs, LambdaModes, Detism, PredGoal),
			Context, MainContext, [], LambdaConstruct,
			Info2, Info3) },

		{ make_fresh_arg_var(AditiState0Term, AditiState0Var, [],
			VarSet5, VarSet6) },
		{ make_fresh_arg_var(AditiStateTerm, AditiStateVar, [],
			VarSet6, VarSet7) },
		{ AllArgs = [LambdaVar, AditiState0Var, AditiStateVar] },
		
		% post_typecheck.m will fill this in.
		{ GenericCallModes = [] },

		{ Call = generic_call(aditi_builtin(Builtin, ModifiedCallId),
			AllArgs, GenericCallModes, det) - GoalInfo },

		%
		% Wrap an explicit quantification around the goal to make
		% sure that the closure construction and the
		% `aditi_delete' or `aditi_modify' call are not separated.
		% Separating the goals would make optimization of the update
		% using indexes more difficult.
		%
		{ UpdateConj = some([], cannot_remove,
			conj([LambdaConstruct, Call]) - GoalInfo) - GoalInfo },

		{ CallId = call(generic_call(
			aditi_builtin(Builtin, ModifiedCallId))) },

		{ record_called_pred_or_func(PredOrFunc, SymName, PredArity,
			Info3, Info4) },
		insert_arg_unifications(AllArgs,
			[term__variable(LambdaVar), AditiState0Term,
				AditiStateTerm],
			Context, CallId, no, UpdateConj, VarSet7, UpdateGoal,
			VarSet, Info4, Info)
	;
		%
		% Second syntax -
		% aditi_bulk_delete(pred p/3,
		%	(aditi_bottom_up pred(..) :- ..), DB0, DB).
		% aditi_bulk_modify(pred p/3,
		%	(aditi_top_down pred(..) :- ..), DB0, DB).
		%
		% The `pred_term' syntax parsed above is transformed
		% into the equivalent of this syntax.
		%
		{ Args0 = [PredCallIdTerm | OtherArgs0] },
		{ OtherArgs0 = [_, _, _] },

		{ parse_pred_or_func_name_and_arity(PredCallIdTerm,
			PredOrFunc, SymName, Arity0) },
		{ adjust_func_arity(PredOrFunc, Arity0, Arity) }
	->
		{ Syntax = sym_name_and_closure },

		{ make_fresh_arg_vars(OtherArgs0,
			VarSet0, OtherArgs, VarSet1) },
		{ invalid_pred_id(PredId) },

		{ Builtin = aditi_insert_delete_modify(InsertDelMod,
				PredId, Syntax) },

		{ ModifiedCallId = PredOrFunc - SymName/Arity },
		
		% post_typecheck.m will fill this in.
		{ GenericCallModes = [] },

		{ Call = generic_call(aditi_builtin(Builtin, ModifiedCallId),
			OtherArgs, GenericCallModes, det) - GoalInfo },
		{ CallId = call(generic_call(
			aditi_builtin(Builtin, ModifiedCallId))) },
		{ record_called_pred_or_func(PredOrFunc, SymName, Arity,
			Info0, Info1) },
		insert_arg_unifications(OtherArgs, OtherArgs0, Context, CallId,
			no, Call, VarSet1, UpdateGoal, VarSet, Info1, Info)
	;
		{ invalid_goal(Descr, Args0, GoalInfo,
			UpdateGoal, VarSet0, VarSet) },
		{ qual_info_set_found_syntax_error(yes, Info0 ^ qual_info,
			QualInfo) },
		{ Info = Info0 ^ qual_info := QualInfo },
		io__set_exit_status(1),
		output_expected_aditi_update_syntax(Context, InsertDelMod)
	).

:- pred aditi_delete_insert_delete_modify_goal_info(aditi_insert_delete_modify,
		pred_or_func, sym_name, arity, list(prog_var), pred_or_func,
		lambda_eval_method, list(mode), determinism,
		hlds_goal, hlds_goal).
:- mode aditi_delete_insert_delete_modify_goal_info(in, in, in, in, in, out,
		out, out, out, in, out) is det.

aditi_delete_insert_delete_modify_goal_info(bulk_insert, PredOrFunc, _SymName,
		PredArity, _Args, LambdaPredOrFunc, EvalMethod,
		LambdaModes, Detism, Goal, Goal) :-
	LambdaPredOrFunc = PredOrFunc,
	EvalMethod = (aditi_bottom_up),
	out_mode(OutMode),
	Detism = nondet,
	% Modes for the arguments of the input tuple.
	list__duplicate(PredArity, OutMode, LambdaModes).

aditi_delete_insert_delete_modify_goal_info(delete(BulkOrFilter), PredOrFunc, 
		SymName, PredArity, Args, LambdaPredOrFunc, EvalMethod,
		LambdaModes, Detism, Goal0, Goal) :-
	LambdaPredOrFunc = PredOrFunc,

	(
		BulkOrFilter = filter,
		EvalMethod = (aditi_top_down),
		in_mode(InMode),
		list__duplicate(PredArity, InMode, LambdaModes),
		Detism = semidet,
		Goal = Goal0
	;
		BulkOrFilter = bulk,
		EvalMethod = (aditi_bottom_up),
		Detism = nondet,
		out_mode(OutMode),
		list__duplicate(PredArity, OutMode, LambdaModes),

		% Join the result of the deletion goal with the
		% relation to be updated.
		conjoin_aditi_update_goal_with_call(PredOrFunc, SymName,
			Args, Goal0, Goal)
	).

aditi_delete_insert_delete_modify_goal_info(modify(BulkOrFilter), PredOrFunc, 
		SymName, PredArity, Args, LambdaPredOrFunc, EvalMethod,
		LambdaModes, Detism, Goal0, Goal) :-

	% The closure passed to `aditi_modify' and `aditi_bulk_modify'
	% is always a predicate closure.
	LambdaPredOrFunc = predicate,

	in_mode(InMode),
	out_mode(OutMode),
	(
		BulkOrFilter = filter,

		% Modes for the arguments corresponding to
		% the input tuple.
		list__duplicate(PredArity, InMode,
			DeleteModes),
		EvalMethod = (aditi_top_down),
		Detism = semidet,
		Goal = Goal0
	;
		BulkOrFilter = bulk,
		EvalMethod = (aditi_bottom_up),
		Detism = nondet,

		% Modes for the arguments corresponding to
		% the input tuple.
		list__duplicate(PredArity, OutMode,
			DeleteModes),

		% `Args' must have length `PredArity * 2',
		% so this will always succeed.
		( list__take(PredArity, Args, CallArgs0) ->
			CallArgs = CallArgs0
		;
			error("aditi_delete_insert_delete_modify_goal_info")
		),

		% Join the result of the modify goal with the
		% relation to be updated.
		conjoin_aditi_update_goal_with_call(PredOrFunc, SymName,
			CallArgs, Goal0, Goal)
	),

	% Modes for the arguments corresponding to
	% the output tuple.
	list__duplicate(PredArity, OutMode, InsertModes),
	list__append(DeleteModes, InsertModes, LambdaModes).

:- pred conjoin_aditi_update_goal_with_call(pred_or_func, sym_name,
		list(prog_var), hlds_goal, hlds_goal).
:- mode conjoin_aditi_update_goal_with_call(in, in, in, in, out) is det.

conjoin_aditi_update_goal_with_call(PredOrFunc, SymName, Args, Goal0, Goal) :-
	invalid_pred_id(PredId),
	Goal0 = _ - GoalInfo,

	% The predicate is recorded as used in
	% transform_aditi_tuple_insert_delete and
	% transform_aditi_insert_delete_modify
	do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, CallGoal),

	Goal = conj([CallGoal, Goal0]) - GoalInfo.
	
:- pred output_expected_aditi_update_syntax(prog_context,
		aditi_insert_delete_modify, io__state, io__state). 
:- mode output_expected_aditi_update_syntax(in, in, di, uo) is det.

output_expected_aditi_update_syntax(Context, bulk_insert) -->
	output_insert_or_delete_expected_syntax(Context, "aditi_bulk_insert").
output_expected_aditi_update_syntax(Context, delete(bulk)) -->
	output_insert_or_delete_expected_syntax(Context, "aditi_bulk_delete").
output_expected_aditi_update_syntax(Context, delete(filter)) -->
	output_insert_or_delete_expected_syntax(Context, "aditi_delete").
output_expected_aditi_update_syntax(Context, modify(BulkOrFilter)) -->
	{ BulkOrFilter = bulk, Name = "aditi_bulk_modify"
	; BulkOrFilter = filter, Name = "aditi_modify"
	},
	prog_out__write_context(Context),
	io__write_string("Error: expected\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	io__write_string(Name),
	io__write_string("(\n"),
	prog_out__write_context(Context),
	io__write_string(
		"    (p(<Args0>) ==> p(<Args>) :- <Goal>),\n"),
	prog_out__write_context(Context),
	io__write_string(
	"    DB0, DB)'\n"),
	output_aditi_closure_syntax(Context, Name).

:- pred output_insert_or_delete_expected_syntax(prog_context, string,
		io__state, io__state).
:- mode output_insert_or_delete_expected_syntax(in, in, di, uo) is det.

output_insert_or_delete_expected_syntax(Context, Name) -->
	prog_out__write_context(Context),
	io__write_string("Error: expected `"),
	io__write_string(Name),
	io__write_string("((p(<Args>) :- <Goal>), DB0, DB)'\n"),
	output_aditi_closure_syntax(Context, Name).

:- pred output_aditi_closure_syntax(prog_context, string,
		io__state, io__state).
:- mode output_aditi_closure_syntax(in, in, di, uo) is det.

output_aditi_closure_syntax(Context, Name) -->
	prog_out__write_context(Context),
	io__write_string("  or `"),
	io__write_string(Name),
	io__write_string("(PredOrFunc p/N, Closure, DB0, DB)'.\n").

	% Report an error for an Aditi update with the wrong number
	% of arguments.
:- pred aditi_update_arity_error(prog_context, string, int, list(int),
		io__state, io__state).
:- mode aditi_update_arity_error(in, in, in, in, di, uo) is det.

aditi_update_arity_error(Context, UpdateStr, Arity, ExpectedArities) -->
	io__set_exit_status(1),
	{ MaybePredOrFunc = no },
	prog_out__write_context(Context),
	io__write_string("Error: "),
	{ MaybePredOrFunc = no },
	report_error_num_args(MaybePredOrFunc, Arity, ExpectedArities),
	io__nl,
	prog_out__write_context(Context),
	io__write_string("  in `"),
	io__write_string(UpdateStr),
	io__write_string("'.\n").

	% Produce an invalid goal when parsing of an Aditi update fails.
:- pred invalid_goal(string, list(prog_term), hlds_goal_info,
		hlds_goal, prog_varset, prog_varset).
:- mode invalid_goal(in, in, in, out, in, out) is det.

invalid_goal(UpdateStr, Args0, GoalInfo, Goal, VarSet0, VarSet) :-
	invalid_pred_id(PredId),
	invalid_proc_id(ProcId),
	make_fresh_arg_vars(Args0, VarSet0, HeadVars, VarSet),
	MaybeUnifyContext = no,
	Goal = call(PredId, ProcId, HeadVars, not_builtin,
		MaybeUnifyContext, unqualified(UpdateStr)) - GoalInfo.

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
	--->	
		% the arguments in the head of the clause
		head(pred_or_func, arity)	
	;
		% the arguments in a call to a predicate
		call(call_id)
	;	
		% the arguments in a functor
		functor(
			cons_id,
			unify_main_context,
			unify_sub_contexts
		).

:- pred insert_arg_unifications(list(prog_var), list(prog_term),
		prog_context, arg_context, bool, hlds_goal, prog_varset,
		hlds_goal, prog_varset, transform_info, transform_info,
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
		{ Goal0 = _ - GoalInfo0 },
		{ goal_to_conj_list(Goal0, List0) },
		insert_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			ForPragmaC, 0, List0, VarSet0, List, VarSet,
			Info0, Info),
		{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
		{ conj_list_to_goal(List, GoalInfo, Goal) }
	).

:- pred insert_arg_unifications_2(list(prog_var), list(prog_term),
		prog_context, arg_context, bool, int, list(hlds_goal),
		prog_varset, list(hlds_goal), prog_varset,
		transform_info, transform_info, io__state, io__state).
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
	insert_arg_unification(Var, Arg, Context, ArgContext,
		ForPragmaC, N1, List0, VarSet0, List1, VarSet1, ArgUnifyConj,
		Info0, Info1),
	(
		{ ArgUnifyConj = [] }
	->
		insert_arg_unifications_2(Vars, Args, Context, ArgContext,
			ForPragmaC, N1, List1, VarSet1, List, VarSet,
			Info1, Info)
	;
		insert_arg_unifications_2(Vars, Args, Context, ArgContext,
			ForPragmaC, N1, List1, VarSet1, List2, VarSet,
			Info1, Info),
		{ list__append(ArgUnifyConj, List2, List) }
	).	

:- pred insert_arg_unifications_with_supplied_contexts(list(prog_var),
		list(prog_term), assoc_list(int, arg_context), prog_context,
		hlds_goal, prog_varset, hlds_goal, prog_varset,
		transform_info, transform_info, io__state, io__state).
:- mode insert_arg_unifications_with_supplied_contexts(in, in, in, in, in, in,
		out, out, in, out, di, uo) is det.

insert_arg_unifications_with_supplied_contexts(ArgVars,
		ArgTerms, ArgContexts, Context, Goal0, VarSet0,
		Goal, VarSet, Info0, Info) -->
	( { ArgVars = [] } ->
		{ Goal = Goal0 },
		{ VarSet = VarSet0 },
		{ Info = Info0 }
	;
		{ Goal0 = _ - GoalInfo0 },
		{ goal_to_conj_list(Goal0, GoalList0) },
		insert_arg_unifications_with_supplied_contexts_2(ArgVars,
			ArgTerms, ArgContexts, Context, GoalList0,
			VarSet0, GoalList, VarSet, Info0, Info),
		{ goal_info_set_context(GoalInfo0, Context, GoalInfo) },
		{ conj_list_to_goal(GoalList, GoalInfo, Goal) }
	).

:- pred insert_arg_unifications_with_supplied_contexts_2(list(prog_var),
		list(prog_term), assoc_list(int, arg_context), prog_context,
		list(hlds_goal), prog_varset, list(hlds_goal), prog_varset,
		transform_info, transform_info, io__state, io__state).
:- mode insert_arg_unifications_with_supplied_contexts_2(in, in, in, in, in,
		in, out, out, in, out, di, uo) is det.

insert_arg_unifications_with_supplied_contexts_2(Vars, Terms, ArgContexts,
		Context, List0, VarSet0, List, VarSet, Info0, Info) -->
	(
		{ Vars = [], Terms = [], ArgContexts = [] }
	->
		{ List = List0 },
		{ VarSet = VarSet0 },
		{ Info = Info0 }
	;
		{ Vars = [Var | Vars1] },
		{ Terms = [Term | Terms1] },
		{ ArgContexts = [ArgNumber - ArgContext | ArgContexts1] }
	->
		insert_arg_unification(Var, Term, Context, ArgContext, no,
			ArgNumber, List0, VarSet0, List1, VarSet1,
			UnifyConj, Info0, Info1),
		insert_arg_unifications_with_supplied_contexts_2(Vars1, Terms1,
			ArgContexts1, Context, List1, VarSet1, List2, VarSet,
			Info1, Info),
		{ list__append(UnifyConj, List2, List) }
	;
		{ error("insert_arg_unifications_with_supplied_contexts") }
	).

:- pred insert_arg_unification(prog_var, prog_term,
		prog_context, arg_context, bool, int,
		list(hlds_goal), prog_varset, list(hlds_goal), prog_varset,
		list(hlds_goal), transform_info, transform_info,
		io__state, io__state).
:- mode insert_arg_unification(in, in, in, in, in, in,
		in, in, out, out, out, in, out, di, uo) is det.

insert_arg_unification(Var, Arg, Context, ArgContext, ForPragmaC, N1,
		List0, VarSet0, List1, VarSet1, ArgUnifyConj, Info0, Info) -->
	(
		{ Arg = term__variable(Var) }
	->
		% Skip unifications of the form `X = X'
		{ VarSet1 = VarSet0 },
		{ Info = Info0 },
		{ ArgUnifyConj = [] },
		{ List1 = List0 }
	;
		{ Arg = term__variable(ArgVar) },
		{ ForPragmaC = yes }
	->
		% Handle unifications of the form `X = Y' by substitution
		% if this is safe.
		{ Info = Info0 },
		{ ArgUnifyConj = [] },
		{ map__init(Subst0) },
		{ map__det_insert(Subst0, ArgVar, Var, Subst) },
		{ goal_util__rename_vars_in_goals(List0, no, Subst,
			List1) },
		{ varset__search_name(VarSet0, ArgVar, ArgVarName) ->
			varset__name_var(VarSet0, Var, ArgVarName, VarSet1)
		;
			VarSet1 = VarSet0
		}
	;
		{ arg_context_to_unify_context(ArgContext, N1,
			UnifyMainContext, UnifySubContext) },
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			VarSet0, pure, Goal, VarSet1, Info0, Info),
		{ goal_to_conj_list(Goal, ArgUnifyConj) },
		{ List1 = List0 }
	).

	% append_arg_unifications is the same as insert_arg_unifications,
	% except that the unifications are added after the goal rather
	% than before the goal.

:- pred append_arg_unifications(list(prog_var), list(prog_term),
		prog_context, arg_context, hlds_goal, prog_varset, hlds_goal,
		prog_varset, transform_info, transform_info,
		io__state, io__state).
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
	list(hlds_goal), prog_varset, transform_info, transform_info,
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
	append_arg_unification(Var, Arg, Context, ArgContext,
		N1, ConjList, VarSet0, VarSet1, Info0, Info1),
	{ list__append(List0, ConjList, List1) },
	append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
		List1, VarSet1, List, VarSet, Info1, Info).

:- pred append_arg_unification(prog_var, prog_term, prog_context, arg_context,
		int, list(hlds_goal), prog_varset, prog_varset,
		transform_info, transform_info, io__state, io__state).
:- mode append_arg_unification(in, in, in, in, in, out, in,
		out, in, out, di, uo) is det.

append_arg_unification(Var, Arg, Context, ArgContext,
		N1, ConjList, VarSet0, VarSet, Info0, Info) -->
	( { Arg = term__variable(Var) } ->
		% skip unifications of the form `X = X'
		{ Info = Info0 },
		{ VarSet = VarSet0 },
		{ ConjList = [] }
	;
		{ arg_context_to_unify_context(ArgContext, N1,
					UnifyMainContext, UnifySubContext) },
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			VarSet0, pure, Goal, VarSet, Info0, Info),
		{ goal_to_conj_list(Goal, ConjList) }
	).

:- pred arg_context_to_unify_context(arg_context, int,
				unify_main_context, unify_sub_contexts).
:- mode arg_context_to_unify_context(in, in, out, out) is det.

arg_context_to_unify_context(head(PredOrFunc, Arity), ArgNum,
		ArgContext, []) :-
	( PredOrFunc = function, ArgNum = Arity ->
		% it's the function result term in the head
		ArgContext = head_result
	;
		% it's a head argument
		ArgContext = head(ArgNum)
	).
arg_context_to_unify_context(call(PredId), ArgNum, call(PredId, ArgNum), []).
arg_context_to_unify_context(functor(ConsId, MainContext, SubContexts), ArgNum,
			MainContext, [ConsId - ArgNum | SubContexts]).

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
	make_fresh_arg_var(Arg, Var, Vars0, VarSet0, VarSet1),
	make_fresh_arg_vars_2(Args, [Var | Vars0], VarSet1, Vars, VarSet).

:- pred make_fresh_arg_var(prog_term, prog_var, list(prog_var),
		prog_varset, prog_varset).
:- mode make_fresh_arg_var(in, out, in, in, out) is det.

make_fresh_arg_var(Arg, Var, Vars0, VarSet0, VarSet) :-
	( Arg = term__variable(ArgVar), \+ list__member(ArgVar, Vars0) ->
		Var = ArgVar,
		VarSet = VarSet0
	;
		varset__new_var(VarSet0, Var, VarSet)
	).

%-----------------------------------------------------------------------------%

	%
	% XXX We could do better on the error messages for
	% lambda expressions and field extraction and update expressions.
	%
:- pred unravel_unification(prog_term, prog_term, prog_context,
		unify_main_context, unify_sub_contexts, prog_varset, 
		purity, hlds_goal, prog_varset, transform_info, transform_info,
		io__state, io__state).
:- mode unravel_unification(in, in, in, in, in, in, in, out, out,
		in, out, di, uo) is det.

	% `X = Y' needs no unravelling.

unravel_unification(term__variable(X), term__variable(Y), Context,
	MainContext, SubContext, VarSet0, Purity, Goal, VarSet, Info0, Info)
		-->
	{ make_atomic_unification(X, var(Y), Context, MainContext,
		SubContext, Goal, Info0, Info1) },
	check_expr_purity(Purity, Context, Info1, Info),
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
			Context, MainContext, SubContext, VarSet0, Purity,
			Goal, VarSet, Info0, Info) -->
	{ RHS = term__functor(F, Args, FunctorContext) },
	(
		% Handle explicit type qualification.
		{ F = term__atom("with_type") },
		{ Args = [RVal, DeclType0] }
	->
		{ convert_type(DeclType0, DeclType) },
		{ varset__coerce(VarSet0, DeclVarSet) },
		process_type_qualification(X, DeclType, DeclVarSet,
			Context, Info0, Info1),
		unravel_unification(term__variable(X), RVal,
			Context, MainContext, SubContext, VarSet0,
			Purity, Goal, VarSet, Info1, Info)
	;
		% Handle unification expressions.
		{ F = term__atom("@") },
		{ Args = [LVal, RVal] }
	->
		unravel_unification(term__variable(X), LVal,
			Context, MainContext, SubContext,
			VarSet0, Purity, Goal1, VarSet1, Info0, Info1),
		unravel_unification(term__variable(X), RVal,
			Context, MainContext, SubContext,
			VarSet1, Purity, Goal2, VarSet, Info1, Info),
		{ goal_info_init(GoalInfo) },
		{ goal_to_conj_list(Goal1, ConjList1) },
		{ goal_to_conj_list(Goal2, ConjList2) },
		{ list__append(ConjList1, ConjList2, ConjList) },
		{ conj_list_to_goal(ConjList, GoalInfo, Goal) }
	;	
	    {
		% handle lambda expressions
		parse_lambda_eval_method(RHS, EvalMethod0, RHS1),
		RHS1 = term__functor(term__atom("lambda"), Args1, _),
		Args1 = [LambdaExpressionTerm0, GoalTerm0],
		term__coerce(LambdaExpressionTerm0, LambdaExpressionTerm),
		parse_lambda_expression(LambdaExpressionTerm,
			Vars0, Modes0, Det0)
	    ->
		PredOrFunc = predicate,
		EvalMethod = EvalMethod0, Vars1 = Vars0,
		Modes1 = Modes0, Det1 = Det0, GoalTerm1 = GoalTerm0
	    ;
		% handle higher-order pred and func expressions -
		% same semantics as lambda expressions, different syntax
		% (the original lambda expression syntax is now deprecated)
		parse_rule_term(Context, RHS, HeadTerm0, GoalTerm1),
		term__coerce(HeadTerm0, HeadTerm),
		(
			parse_pred_expression(HeadTerm, EvalMethod0,
				Vars0, Modes0, Det0)
		->
			PredOrFunc = predicate,
			EvalMethod = EvalMethod0, Vars1 = Vars0,
			Modes1 = Modes0, Det1 = Det0
		;
			parse_func_expression(HeadTerm, EvalMethod,
				Vars1, Modes1, Det1),
			PredOrFunc = function
		)
	    }
	->
		check_expr_purity(Purity, Context, Info0, Info1),
		make_hlds__qualify_lambda_mode_list(Modes1, Modes,
			Context, Info1, Info2),
		{ Det = Det1 },
		{ term__coerce(GoalTerm1, GoalTerm) },
		{ parse_goal(GoalTerm, VarSet0, ParsedGoal, VarSet1) },
		build_lambda_expression(X, PredOrFunc, EvalMethod, Vars1,
			Modes, Det, ParsedGoal, VarSet1,
			Context, MainContext, SubContext, Goal, VarSet,
			Info2, Info)
	;
	    {
		% handle higher-order dcg pred expressions -
		% same semantics as higher-order pred expressions,
		% but has two extra arguments, and the goal is expanded
		% as a DCG goal.
		F = term__atom("-->"),
		Args = [PredTerm0, GoalTerm0],
		term__coerce(PredTerm0, PredTerm),
		parse_dcg_pred_expression(PredTerm, EvalMethod,
			Vars0, Modes0, Det)
	    }
	->
		make_hlds__qualify_lambda_mode_list(Modes0, Modes,
			Context, Info0, Info1),
		{ term__coerce(GoalTerm0, GoalTerm) },
		{ parse_dcg_pred_goal(GoalTerm, VarSet0,
			ParsedGoal, DCG0, DCGn, VarSet1) },
		{ list__append(Vars0, [term__variable(DCG0),
				term__variable(DCGn)], Vars1) },
		build_lambda_expression(X, predicate, EvalMethod, Vars1,
			Modes, Det, ParsedGoal, VarSet1,
			Context, MainContext, SubContext, Goal0, VarSet,
			Info1, Info),
		{ Goal0 = GoalExpr - GoalInfo0 },
		{ add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo) },
		{ Goal = GoalExpr - GoalInfo }
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
		check_expr_purity(Purity, Context, Info0, Info1),
		{ map__init(Subst) },
		transform_goal(IfParseTree, VarSet11, Subst, IfGoal, VarSet22,
			Info1, Info2),
		unravel_unification(term__variable(X), ThenTerm,
			Context, MainContext, SubContext, VarSet22, 
			pure, ThenGoal, VarSet33, Info2, Info3),
		unravel_unification(term__variable(X), ElseTerm,
			Context, MainContext, SubContext, VarSet33, pure,
			ElseGoal, VarSet, Info3, Info),
		{ map__init(Empty) },
		{ IfThenElse = if_then_else(Vars, IfGoal, ThenGoal, ElseGoal,
			Empty) },
		{ goal_info_init(Context, GoalInfo) },
		{ Goal = IfThenElse - GoalInfo }
	;
		% handle field extraction expressions
		{ F = term__atom("^") },
		{ Args = [InputTerm, FieldNameTerm] },
		{ parse_field_list(FieldNameTerm, FieldNameResult) },
		{ FieldNameResult = ok(FieldNames) }
	->
		check_expr_purity(Purity, Context, Info0, Info1),
		{ make_fresh_arg_var(InputTerm, InputTermVar, [],
			VarSet0, VarSet1) },
		expand_get_field_function_call(Context, MainContext,
			SubContext, FieldNames, X, InputTermVar,
			VarSet1, VarSet2, Functor, _, Goal0, Info1, Info2),

		{ ArgContext = functor(Functor, MainContext, SubContext) },
		append_arg_unifications([InputTermVar], [InputTerm],
			FunctorContext, ArgContext, Goal0,
			VarSet2, Goal, VarSet, Info2, Info)
	;
		% handle field update expressions
		{ F = term__atom(":=") },
		{ Args = [FieldDescrTerm, FieldValueTerm] },
		{ FieldDescrTerm = term__functor(term__atom("^"),
			[InputTerm, FieldNameTerm], _) },
		{ parse_field_list(FieldNameTerm, FieldNameResult) },
		{ FieldNameResult = ok(FieldNames) }
	->
		check_expr_purity(Purity, Context, Info0, Info1),
		{ make_fresh_arg_var(InputTerm, InputTermVar, [],
			VarSet0, VarSet1) },
		{ make_fresh_arg_var(FieldValueTerm, FieldValueVar,
			[InputTermVar], VarSet1, VarSet2) },

		expand_set_field_function_call(Context, MainContext,
			SubContext, FieldNames, FieldValueVar, InputTermVar, X,
			VarSet2, VarSet3, Functor,
			InnerFunctor - FieldSubContext, Goal0, Info1, Info2),

		{ TermArgContext = functor(Functor, MainContext, SubContext) },
		{ TermArgNumber = 1 },
		append_arg_unification(InputTermVar, InputTerm,
			FunctorContext, TermArgContext, TermArgNumber,
			TermUnifyConj, VarSet3, VarSet4, Info2, Info3),

		{ FieldArgContext = functor(InnerFunctor,
			MainContext, FieldSubContext) },
		{ FieldArgNumber = 2 },
		append_arg_unification(FieldValueVar, FieldValueTerm,
			FunctorContext, FieldArgContext, FieldArgNumber,
			FieldUnifyConj, VarSet4, VarSet, Info3, Info),

		{ Goal0 = _ - GoalInfo0 },
		{ goal_to_conj_list(Goal0, GoalList0) },
		{ list__condense([GoalList0, TermUnifyConj, FieldUnifyConj],
			GoalList) },
		{ conj_list_to_goal(GoalList, GoalInfo0, Goal) }
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
			{ make_atomic_unification(X, functor(ConsId, []),
				Context, MainContext, SubContext, Goal0,
				Info0, Info) },
			{ Goal0 = GoalExpr - GoalInfo0 },
			{ add_goal_info_purity_feature(GoalInfo0, Purity,
				GoalInfo) },
			{ Goal = GoalExpr - GoalInfo },
			{ VarSet = VarSet0 }
		;
			{ make_fresh_arg_vars(FunctorArgs, VarSet0,
				HeadVars, VarSet1) },
			{ make_atomic_unification(X,
				functor(ConsId, HeadVars), Context,
				MainContext, SubContext, Goal0,
				Info0, Info1) },
			{ ArgContext = functor(ConsId,
				MainContext, SubContext) },
			% Should this be insert_... rather than append_...?
			% No, because that causes efficiency problems
			% with type-checking :-(
			% But for impure unifications, we need to do
			% this, because mode reordering can't reorder
			% around the functor unification.
			( { Purity = pure } ->
				append_arg_unifications(HeadVars, FunctorArgs,
					FunctorContext, ArgContext, Goal0,
					VarSet1, Goal, VarSet, Info1, Info)
			;
				{ Goal0 = GoalExpr - GoalInfo0 },
				{ add_goal_info_purity_feature(GoalInfo0,
					Purity, GoalInfo) },
				{ Goal1 = GoalExpr - GoalInfo },
				insert_arg_unifications(HeadVars, FunctorArgs,
					FunctorContext, ArgContext, no, Goal1,
					VarSet1, Goal, VarSet, Info1, Info)
			)
		)
	).


	% Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification(term__functor(F, As, FC), term__variable(Y),
		C, MC, SC, VarSet0, Purity, Goal, VarSet, Info0, Info) -->
	unravel_unification(term__variable(Y),
		term__functor(F, As, FC),
		C, MC, SC, VarSet0, Purity, Goal, VarSet, Info0, Info).

	% If we find a unification of the form `f1(...) = f2(...)',
	% then we replace it with `Tmp = f1(...), Tmp = f2(...)',
	% and then process it according to the rule above.
	% Note that we can't simplify it yet, because we might simplify
	% away type errors.

unravel_unification(term__functor(LeftF, LeftAs, LeftC),
			term__functor(RightF, RightAs, RightC),
			Context, MainContext, SubContext, VarSet0,
			Purity, Goal, VarSet, Info0, Info) -->
	{ varset__new_var(VarSet0, TmpVar, VarSet1) },
	unravel_unification(
		term__variable(TmpVar),
		term__functor(LeftF, LeftAs, LeftC),
		Context, MainContext, SubContext,
		VarSet1, Purity, Goal0, VarSet2, Info0, Info1),
	unravel_unification(
		term__variable(TmpVar),
		term__functor(RightF, RightAs, RightC),
		Context, MainContext, SubContext,
		VarSet2, Purity, Goal1, VarSet, Info1, Info),
	{ goal_info_init(GoalInfo) },
	{ goal_to_conj_list(Goal0, ConjList0) },
	{ goal_to_conj_list(Goal1, ConjList1) },
	{ list__append(ConjList0, ConjList1, ConjList) },
	{ conj_list_to_goal(ConjList, GoalInfo, Goal) }.

:- pred make_hlds__qualify_lambda_mode_list(list(mode), list(mode),
		prog_context, transform_info, transform_info,
		io__state, io__state).
:- mode make_hlds__qualify_lambda_mode_list(in, out, in, in, out,
		di, uo) is det.

make_hlds__qualify_lambda_mode_list(Modes0, Modes, Context, Info0, Info) -->
	% The modes in `.opt' files are already fully module qualified.
	( { Info0 ^ qual_info ^ import_status \= opt_imported } ->
		{ qual_info_get_mq_info(Info0 ^ qual_info, MQInfo0) },
		module_qual__qualify_lambda_mode_list(Modes0, Modes,
			Context, MQInfo0, MQInfo1),
		{ qual_info_set_mq_info(Info0 ^ qual_info,
			MQInfo1, QualInfo1) },
		{ Info = Info0 ^ qual_info := QualInfo1 }
	;
		{ Modes = Modes0 },
		{ Info = Info0 }
	).

%-----------------------------------------------------------------------------%

:- pred check_expr_purity(purity, prog_context, transform_info,
	transform_info, io__state, io__state).
:- mode check_expr_purity(in, in, in, out, di, uo) is det.
check_expr_purity(Purity, Context, Info0, Info) -->
		( { Purity \= pure } ->
			impure_unification_expr_error(Context, Purity),
			{ module_info_incr_errors(Info0 ^ module_info,
				ModuleInfo) },
			{ Info = Info0 ^ module_info := ModuleInfo }
		;
			{ Info = Info0 }
		).
%-----------------------------------------------------------------------------%

	% Parse a term of the form `Head :- Body', treating
	% a term not in that form as `Head :- true'.
:- pred parse_rule_term(term__context, term(T), term(T), term(T)).
:- mode parse_rule_term(in, in, out, out) is det.

parse_rule_term(Context, RuleTerm, HeadTerm, GoalTerm) :-
	(
		RuleTerm = term__functor(term__atom(":-"),
			[HeadTerm0, GoalTerm0], _)
	->
		HeadTerm = HeadTerm0,
		GoalTerm = GoalTerm0
	;
		HeadTerm = RuleTerm,
		GoalTerm = term__functor(term__atom("true"), [], Context)
	).

%-----------------------------------------------------------------------------%

:- pred build_lambda_expression(prog_var, pred_or_func, lambda_eval_method,
		list(prog_term), list(mode), determinism, goal, prog_varset,
		prog_context, unify_main_context, unify_sub_contexts,
		hlds_goal, prog_varset, transform_info, transform_info,
		io__state, io__state).
:- mode build_lambda_expression(in, in, in, in, in, in, in, in,
		in, in, in, out, out, in, out, di, uo) is det.

build_lambda_expression(X, PredOrFunc, EvalMethod, Args, Modes, Det,
		ParsedGoal, VarSet0, Context, MainContext, SubContext,
		Goal, VarSet, Info1, Info) -->
	%
	% In the parse tree, the lambda arguments can be any terms.
	% But in the HLDS, they must be distinct variables.  So we introduce
	% fresh variables for the lambda arguments, and add appropriate
	% unifications.
	%
	% For example, we convert from 
	%	X = (func(f(A, B), c) = D :- G)
	% to 
	%	X = (func(H1, H2) = H3 :-
	%		some [A, B] (H1 = f(A, B), H2 = c, H3 = D).
	%
	% Note that the quantification is important here.
	% That's why we need to introduce the explicit `some [...]'.
	% Variables in the argument positions are lambda-quantified,
	% so when we move them to the body, we need to make them
	% explicitly existentially quantified, to avoid capturing
	% any variables of the same name that occur outside this scope.
	%
	% For predicates, all variables occuring in the lambda arguments
	% are locally quantified to the lambda goal. 
	% For functions, we need to be careful because variables in
	% arguments should similarly be quantified, but variables in
	% the function return value term (and not in the arguments)
	% should *not* be locally quantified.
	%

	%
	% Create fresh variables, transform the goal to HLDS,
	% and add unifications with the fresh variables.
	% We use varset__new_vars rather than make_fresh_arg_vars,
	% since for functions we need to ensure that the variable 
	% corresponding to the function result term is a new variable,
	% to avoid the function result term becoming lambda-quantified.
	%
	{ list__length(Args, NumArgs) },
	{ varset__new_vars(VarSet0, NumArgs, LambdaVars, VarSet1) },
	{ map__init(Substitution) },
	transform_goal(ParsedGoal, VarSet1, Substitution,
			HLDS_Goal0, VarSet2, Info1, Info2),
	{ ArgContext = head(PredOrFunc, NumArgs) },
	insert_arg_unifications(LambdaVars, Args, Context, ArgContext,
		no, HLDS_Goal0, VarSet2, HLDS_Goal1, VarSet, Info2, Info3),

	%
	% Now figure out which variables we need to explicitly existentially
	% quantify.
	%
	{
		PredOrFunc = predicate,
		QuantifiedArgs = Args
	;
		PredOrFunc = function,
		pred_args_to_func_args(Args, QuantifiedArgs, _ReturnValTerm)
	},
	{ term__vars_list(QuantifiedArgs, QuantifiedVars0) },
	{ list__sort_and_remove_dups(QuantifiedVars0, QuantifiedVars) },

	{ goal_info_init(Context, GoalInfo) },
	{ HLDS_Goal = some(QuantifiedVars, can_remove, HLDS_Goal1)
			- GoalInfo },

	%
	% We set the lambda nonlocals here to anything that could possibly
	% be nonlocal.  Quantification will reduce this down to
	% the proper set of nonlocal arguments.
	%
	{ goal_util__goal_vars(HLDS_Goal, LambdaGoalVars0) }, 
	{ set__delete_list(LambdaGoalVars0, LambdaVars, LambdaGoalVars1) },
	{ set__delete_list(LambdaGoalVars1, QuantifiedVars, LambdaGoalVars2) },
	{ set__to_sorted_list(LambdaGoalVars2, LambdaNonLocals) },

	{ make_atomic_unification(X,
		lambda_goal(PredOrFunc, EvalMethod, modes_are_ok,
			LambdaNonLocals, LambdaVars, Modes, Det, HLDS_Goal),
		Context, MainContext, SubContext, Goal, Info3, Info) }.

%-----------------------------------------------------------------------------%

:- pred construct_pred_or_func_call(pred_id, pred_or_func, sym_name,
		list(prog_var), hlds_goal_info, hlds_goal,
		transform_info, transform_info).
:- mode construct_pred_or_func_call(in, in, in, in, in, out, in, out) is det.

construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, Goal, Info0, Info) :-
	do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, Goal),
	list__length(Args, Arity),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	record_called_pred_or_func(PredOrFunc, SymName, OrigArity,
		Info0, Info).

:- pred do_construct_pred_or_func_call(pred_id, pred_or_func, sym_name,
		list(prog_var), hlds_goal_info, hlds_goal).
:- mode do_construct_pred_or_func_call(in, in, in, in, in, out) is det.

do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, Goal) :-
	(
		PredOrFunc = predicate,
		invalid_proc_id(DummyProcId),
		Goal = call(PredId, DummyProcId, Args,
			not_builtin, no, SymName) - GoalInfo
	;
		PredOrFunc = function,
		pred_args_to_func_args(Args, FuncArgs, RetArg),
		list__length(FuncArgs, Arity),
		ConsId = cons(SymName, Arity),
		goal_info_get_context(GoalInfo, Context),
		hlds_goal__create_atomic_unification(RetArg,
			functor(ConsId, FuncArgs), Context,
			explicit, [], GoalExpr - _),
		Goal = GoalExpr - GoalInfo
	).

:- pred make_atomic_unification(prog_var, unify_rhs, prog_context,
		unify_main_context, unify_sub_contexts, hlds_goal,
		transform_info, transform_info).
:- mode make_atomic_unification(in, in, in, in, in, out, in, out) is det.

make_atomic_unification(Var, Rhs, Context, MainContext, SubContext,
		Goal, Info0, Info) :-
	(
		Rhs = var(_),
		Info = Info0
	;
		Rhs = lambda_goal(_, _, _, _, _, _, _, _),
		Info = Info0
	;
		Rhs = functor(ConsId, _),
		record_used_functor(ConsId, Info0, Info)
	),
	hlds_goal__create_atomic_unification(Var, Rhs, Context,
		MainContext, SubContext, Goal).

%-----------------------------------------------------------------------------%

	% Process an explicit type qualification.
:- pred process_type_qualification(prog_var, type, tvarset, prog_context,
		transform_info, transform_info, io__state, io__state).
:- mode process_type_qualification(in, in, in, in, in, out, di, uo) is det.

process_type_qualification(Var, Type0, VarSet, Context, Info0, Info) -->
	{ Info0 ^ qual_info = qual_info(EqvMap, TVarSet0, TVarRenaming0,
			TVarNameMap0, VarTypes0, MQInfo0, Status, FoundError) },

	( { Status = opt_imported } ->
		% Types in `.opt' files should already be
		% fully module qualified.
		{ Type1 = Type0 },
		{ MQInfo = MQInfo0 }
	;
		module_qual__qualify_type_qualification(Type0, Type1, 
			Context, MQInfo0, MQInfo)
	),

	{
	% Find any new type variables introduced by this type, and
	% add them to the var-name index and the variable renaming.
	term__vars(Type1, TVars),
	get_new_tvars(TVars, VarSet, TVarSet0, TVarSet1,
		TVarNameMap0, TVarNameMap, TVarRenaming0, TVarRenaming),
			
	% Apply the updated renaming to convert type variables in
	% the clause to type variables in the tvarset.
	term__apply_variable_renaming(Type1, TVarRenaming, Type2),

	% Expand equivalence types.
	equiv_type__replace_in_type(Type2, TVarSet1, EqvMap, Type, TVarSet)
	},
	update_var_types(VarTypes0, Var, Type, Context, VarTypes),	
	{ Info = Info0 ^ qual_info := qual_info(EqvMap, TVarSet, TVarRenaming,
			TVarNameMap, VarTypes, MQInfo, Status, FoundError) }.

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
	tvar_name_map, tvar_name_map, map(tvar, tvar), map(tvar, tvar)).
:- mode get_new_tvars(in, in, in, out, in, out, in, out) is det.

get_new_tvars([], _, T, T, M, M, R, R).
get_new_tvars([TVar | TVars], VarSet, TVarSet0, TVarSet,
		TVarNameMap0, TVarNameMap, TVarRenaming0, TVarRenaming) :-
	( map__contains(TVarRenaming0, TVar) ->
		TVarRenaming1 = TVarRenaming0,
		TVarSet2 = TVarSet0,
		TVarNameMap1 = TVarNameMap0
	;
		( varset__search_name(VarSet, TVar, TVarName) ->
			( map__search(TVarNameMap0, TVarName, TVarSetVar) ->
				map__det_insert(TVarRenaming0, TVar,
						TVarSetVar, TVarRenaming1),
				TVarSet2 = TVarSet0,
				TVarNameMap1 = TVarNameMap0
			;
				varset__new_var(TVarSet0, NewTVar, TVarSet1),
				varset__name_var(TVarSet1, NewTVar,
						TVarName, TVarSet2),
				map__det_insert(TVarNameMap0, TVarName,
						NewTVar, TVarNameMap1),
				map__det_insert(TVarRenaming0, TVar, NewTVar,
						TVarRenaming1)
			)
		;
			TVarNameMap1 = TVarNameMap0,
			varset__new_var(TVarSet0, NewTVar, TVarSet2),
			map__det_insert(TVarRenaming0, TVar, NewTVar,
					TVarRenaming1)
		)
	),
	get_new_tvars(TVars, VarSet, TVarSet2, TVarSet,
		 TVarNameMap1, TVarNameMap, TVarRenaming1, TVarRenaming).
			
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
	list(hlds_goal), prog_varset, transform_info, transform_info,
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
		list(hlds_goal), prog_varset, transform_info, transform_info,
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
		list(hlds_goal), prog_varset, transform_info, transform_info,
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
	---> qual_info(
		eqv_map :: eqv_map,	% Used to expand equivalence types. 
		tvarset :: tvarset,	% All type variables for predicate.
		tvar_renaming :: map(tvar, tvar),
					% Map from clause type variable to
					% actual type variable in tvarset.
		tvar_name_map :: tvar_name_map,
					% Type variables in tvarset occurring
					% in the predicate's argument types
					% indexed by name.
		vartypes :: map(prog_var, type), % Var types
		mq_info :: mq_info,	% Module qualification info.
		import_status :: import_status,
		found_syntax_error :: bool
					% Was there a syntax error
					% in an Aditi update.
	).

:- pred init_qual_info(mq_info, eqv_map, qual_info).
:- mode init_qual_info(in, in, out) is det.

init_qual_info(MQInfo0, EqvMap, QualInfo) :-
	mq_info_set_need_qual_flag(MQInfo0, may_be_unqualified, MQInfo),
	varset__init(TVarSet),
	map__init(Renaming),
	map__init(Index),
	map__init(VarTypes),
	FoundSyntaxError = no,
	QualInfo = qual_info(EqvMap, TVarSet, Renaming,
			Index, VarTypes, MQInfo, local, FoundSyntaxError).

	% Update the qual_info when processing a new clause.
:- pred update_qual_info(qual_info, tvar_name_map, tvarset,
			map(prog_var, type), import_status, qual_info).
:- mode update_qual_info(in, in, in, in, in, out) is det.

update_qual_info(QualInfo0, TVarNameMap, TVarSet,
		VarTypes, Status, QualInfo) :-
	QualInfo0 = qual_info(EqvMap, _TVarSet0, _Renaming0, _TVarNameMap0,
			_VarTypes0, MQInfo, _Status, _FoundError),
	% The renaming for one clause is useless in the others.
	map__init(Renaming),
	QualInfo = qual_info(EqvMap, TVarSet, Renaming, TVarNameMap,
			VarTypes, MQInfo, Status, no).

:- pred qual_info_get_mq_info(qual_info, mq_info).
:- mode qual_info_get_mq_info(in, out) is det.

qual_info_get_mq_info(Info, Info ^ mq_info).

:- pred qual_info_set_mq_info(qual_info, mq_info, qual_info).
:- mode qual_info_set_mq_info(in, in, out) is det.

qual_info_set_mq_info(Info0, MQInfo, Info0 ^ mq_info := MQInfo).

:- pred qual_info_get_var_types(qual_info, map(prog_var, type)).
:- mode qual_info_get_var_types(in, out) is det.

qual_info_get_var_types(Info, Info ^ vartypes).

:- pred qual_info_get_found_syntax_error(qual_info, bool).
:- mode qual_info_get_found_syntax_error(in, out) is det.

qual_info_get_found_syntax_error(Info, Info ^ found_syntax_error).

:- pred qual_info_set_found_syntax_error(bool, qual_info, qual_info).
:- mode qual_info_set_found_syntax_error(in, in, out) is det.

qual_info_set_found_syntax_error(FoundError, Info,
		Info ^ found_syntax_error := FoundError).

:- pred apply_to_recompilation_info(
		pred(recompilation_info, recompilation_info),
		transform_info, transform_info).
:- mode apply_to_recompilation_info(pred(in, out) is det, in, out) is det.

apply_to_recompilation_info(Pred, Info0, Info) :-
	MQInfo0 = Info0 ^ qual_info ^ mq_info,
	mq_info_get_recompilation_info(MQInfo0, MaybeRecompInfo0),
	(
		MaybeRecompInfo0 = yes(RecompInfo0),
		Pred(RecompInfo0, RecompInfo),
		mq_info_set_recompilation_info(MQInfo0,
			yes(RecompInfo), MQInfo),
		Info = Info0 ^ qual_info ^ mq_info := MQInfo
	;
		MaybeRecompInfo0 = no,
		Info = Info0
	).

set_module_recompilation_info(QualInfo, ModuleInfo0, ModuleInfo) :-
	mq_info_get_recompilation_info(QualInfo ^ mq_info, RecompInfo),
	module_info_set_maybe_recompilation_info(ModuleInfo0,
		RecompInfo, ModuleInfo).

:- pred record_called_pred_or_func(pred_or_func, sym_name, arity,
		transform_info, transform_info).
:- mode record_called_pred_or_func(in, in, in, in, out) is det.

record_called_pred_or_func(PredOrFunc, SymName, Arity) -->
	{ Id = SymName - Arity },
	apply_to_recompilation_info(
		recompilation__record_used_item(
			pred_or_func_to_item_type(PredOrFunc), Id, Id)).

:- pred record_used_functor(cons_id, transform_info, transform_info).
:- mode record_used_functor(in, in, out) is det.

record_used_functor(ConsId) -->
	(
		{ ConsId = cons(SymName, Arity) }
	->
		{ Id = SymName - Arity },
		apply_to_recompilation_info(
			recompilation__record_used_item(functor, Id, Id))
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred report_unexpected_decl(string, prog_context, io__state, io__state).
:- mode report_unexpected_decl(in, in, di, uo) is det.

report_unexpected_decl(Descr, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: unexpected or incorrect `"),
	io__write_string(Descr),
	io__write_string("' declaration.\n").

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
	prog_out__write_sym_name_and_arity(Name/Arity),
	io__write_string("\n"),
	prog_out__write_context(Context),
	% This used to say `preceding' instead of `corresponding.'
	% Which is more correct?
	io__write_string("  without corresponding `pred' or `func' declaration.\n").

:- pred pred_method_with_no_modes_error(pred_info, io__state, io__state).
:- mode pred_method_with_no_modes_error(in, di, uo) is det.

pred_method_with_no_modes_error(PredInfo) -->
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_module(PredInfo, Module) },
	{ pred_info_name(PredInfo, Name) },
	{ pred_info_arity(PredInfo, Arity) },
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: no mode declaration for type class method\n"),
	prog_out__write_context(Context),
	io__write_string("  predicate `"),
	prog_out__write_sym_name_and_arity(qualified(Module,Name)/Arity),
	io__write_string("'.\n").

	% Similar to undeclared_mode_error, but gives less information.
	% XXX perhaps we should get rid of this, and change the callers to
	% instead call undeclared_mode_error.
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
	prog_out__write_sym_name_and_arity(Name/Arity),
	io__write_string("' specifies non-existent mode.\n").

	% Similar to undefined_mode_error, but gives more information.
:- pred undeclared_mode_error(list(mode)::in, prog_varset::in,
		pred_id::in, pred_info::in, module_info::in, prog_context::in,
		io__state::di, io__state::uo) is det.

undeclared_mode_error(ModeList, VarSet, PredId, PredInfo, ModuleInfo,
		Context) -->
	prog_out__write_context(Context),
	io__write_string("In clause for "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string(
		"  error: mode annotation specifies undeclared mode\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	{ strip_builtin_qualifiers_from_mode_list(ModeList,
		StrippedModeList) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_name(PredInfo, Name) },
	{ MaybeDet = no },
	mercury_output_mode_subdecl(PredOrFunc,
		varset__coerce(VarSet),
		unqualified(Name), StrippedModeList,
		MaybeDet, Context),
	io__write_string("'\n"),
	prog_out__write_context(Context),
	io__write_string("  of "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(".\n"),
	globals__io_lookup_bool_option(verbose_errors,
		VerboseErrors),
	{ pred_info_all_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		prog_out__write_context(Context),
		io__write_string(
		"  (There are no declared modes for this "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(".)\n")
	; { VerboseErrors = yes } ->
		io__write_string(
		"\tThe declared modes for this "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__write_string(" are the following:\n"),
		{ OutputProc =
		    (pred(ProcId::in, di, uo) is det -->
			io__write_string("\t\t:- mode "),
			output_mode_decl(ProcId, PredInfo),
			io__write_string(".\n")) },
		list__foldl(OutputProc, ProcIds)
	;
		[]
	).

:- pred maybe_undefined_pred_error(sym_name, int, pred_or_func, import_status,
		bool, prog_context, string, io__state, io__state).
:- mode maybe_undefined_pred_error(in, in, in, in, in, in, in, di, uo) is det.

% This is not considered an unconditional error anymore:
% if there is no `:- pred' or `:- func' declaration,
% and the declaration is local, and not a type class method,
% and the `--infer-types' option was specified,
% then we just add an implicit declaration for that predicate or
% function, marking it as one whose type will be inferred.
%
% If this module is for a query generated by the Aditi dbsh
% (--aditi-only is set), allow mode declarations for exported
% predicates with no `:- pred' or `:- func' declaration.
% The predicate will never be called from a compiled Mercury
% procedure. The RL bytecode for the predicate will be called
% directly using information from the generated
% `<module>.derived_schema' file to work out the argument
% types of the output relation.

maybe_undefined_pred_error(Name, Arity, PredOrFunc, Status, IsClassMethod,
		Context, Description) -->
	{ status_defined_in_this_module(Status, DefinedInThisModule) },
	{ status_is_exported(Status, IsExported) },
	globals__io_lookup_bool_option(infer_types, InferTypes),
	globals__io_lookup_bool_option(aditi_only, AditiOnly),
	(
		{
			DefinedInThisModule = yes,
			IsExported = no,
			IsClassMethod = no,
			InferTypes = yes
		;
			AditiOnly = yes
		}		
	->
		[]
	;
		io__set_exit_status(1),
		prog_out__write_context(Context),
		io__write_string("Error: "),
		io__write_string(Description),
		io__write_string(" for "),
		hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
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
	prog_out__write_sym_name_and_arity(ClassName/Arity),
	io__write_string("' without preceding typeclass declaration.\n").

:- pred unspecified_det_for_local(sym_name, arity, pred_or_func, prog_context, 
				io__state, io__state).
:- mode unspecified_det_for_local(in, in, in, in, di, uo) is det.

unspecified_det_for_local(Name, Arity, PredOrFunc, Context) -->
	prog_out__write_context(Context),
	report_warning("Error: no determinism declaration for local\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
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

:- pred unspecified_det_for_method(sym_name, arity, pred_or_func,
			prog_context, io__state, io__state).
:- mode unspecified_det_for_method(in, in, in, in, di, uo) is det.

unspecified_det_for_method(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string(
		"Error: no determinism declaration for type class method\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred unspecified_det_for_exported(sym_name, arity, pred_or_func,
			prog_context, io__state, io__state).
:- mode unspecified_det_for_exported(in, in, in, in, di, uo) is det.

unspecified_det_for_exported(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: no determinism declaration for exported\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred clause_for_imported_pred_error(sym_name, arity, pred_or_func,
				prog_context, io__state, io__state).
:- mode clause_for_imported_pred_error(in, in, in, in, di, uo) is det.

clause_for_imported_pred_error(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: clause for imported "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
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

:- pred pragma_status_error(sym_name, int, prog_context, string,
				io__state, io__state).
:- mode pragma_status_error(in, in, in, in, di, uo) is det.

pragma_status_error(Name, Arity, Context, PragmaName) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: `:- pragma "),
	io__write_string(PragmaName),
	io__write_string("' declaration for exported\n"),
	prog_out__write_context(Context),
	io__write_string("predicate or function "),
	prog_out__write_sym_name_and_arity(Name/Arity),
	io__write_string(" must also be exported.\n").

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
	prog_out__write_sym_name_and_arity(Name/Arity),
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
		{ adjust_func_arity(PredOrFunc, Arity, NumArgs) },

		    % create pragma c_header_code to declare extern variables
		{ module_add_foreign_decl(c, C_HeaderCode, Context,
			Module1, Module2) },

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
		prog_out__write_sym_name_and_arity(Pred/Arity),
		io__write_string("':\n"),
		prog_out__write_context(Context),
		io__write_string(
			"  error: ambiguous predicate/function name.\n"),
		{ Module = Module0 },
		{ Info = Info0 }
	    )
	;
	    undefined_pred_or_func_error(Pred, Arity, Context, 
	    	"`:- pragma fact_table' declaration"),
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
	{ proc_info_argmodes(ProcInfo, Modes) },
	{ fact_table_pragma_vars(Vars, Modes, VarSet, PragmaVars) },
	fact_table_generate_c_code(SymName, PragmaVars, ProcID, PrimaryProcID,
		ProcInfo, ArgTypes, Module0, C_ProcCode, C_ExtraCode),

	% XXX this should be modified to use nondet pragma c_code.
	{ default_attributes(c, Attrs0) },
	{ set_may_call_mercury(Attrs0, will_not_call_mercury, Attrs1) },
	{ set_thread_safe(Attrs1, thread_safe, Attrs2) },
		% fact tables procedures should be considered pure
	{ set_purity(Attrs2, pure, Attrs) },
	module_add_pragma_foreign_proc(Attrs, SymName, PredOrFunc, 
		PragmaVars, VarSet, ordinary(C_ProcCode, no),
		Status, Context, Module0, Module1, Info0, Info),
	{
		C_ExtraCode = ""
	->
		Module2 = Module1
	;
		module_add_foreign_body_code(c, C_ExtraCode, Context,
			Module1, Module2)
	},
	%
	% The C code for fact tables includes C labels;
	% we cannot inline this code, because if we try,
	% the result may be duplicate labels in the generated code.
	% So we must disable inlining for fact_table procedures.
	%
	add_pred_marker(Module2, "fact_table", SymName, Arity,
		Status, Context, no_inline, [], Module).


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
