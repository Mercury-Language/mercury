%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2004 The University of Melbourne.
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

:- module hlds__make_hlds.
:- interface.

:- import_module hlds__hlds_data.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__special_pred.
:- import_module parse_tree__equiv_type.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__prog_data.

:- import_module bool, list, io, std_util, term.

% parse_tree_to_hlds(ParseTree, MQInfo, EqvMap, HLDS, QualInfo,
%		InvalidTypes, InvalidModes):
%	Given MQInfo (returned by module_qual.m) and EqvMap (returned by
%	equiv_type.m), converts ParseTree to HLDS.
%	Any errors found are recorded in the HLDS num_errors field.
%	Returns InvalidTypes = yes if undefined types found.
%	Returns InvalidModes = yes if undefined or cyclic insts or modes found.
%	QualInfo is an abstract type that is then passed back to
%	produce_instance_method_clauses (see below).
:- pred parse_tree_to_hlds(compilation_unit::in, mq_info::in, eqv_map::in,
	module_info::out, qual_info::out, bool::out, bool::out, io::di, io::uo)
	is det.

:- pred add_new_proc(inst_varset::in, arity::in, list(mode)::in,
	maybe(list(mode))::in, maybe(list(is_live))::in,
	maybe(determinism)::in, prog_context::in, is_address_taken::in,
	pred_info::in, pred_info::out, proc_id::out) is det.

	% add_special_pred_for_real(SpecialPredId, TVarSet, Type, TypeCtor,
	% 	TypeBody, TypeContext, TypeStatus, !ModuleInfo).
	%
	% Add declarations and clauses for a special predicate.
	% This is used by unify_proc.m to add a unification predicate
	% for an imported type for which special predicates are being
	% generated only when a unification procedure is requested
	% during mode analysis.
:- pred add_special_pred_for_real(special_pred_id::in, tvarset::in, (type)::in,
	type_ctor::in, hlds_type_body::in, prog_context::in, import_status::in,
	module_info::in, module_info::out) is det.

	% add_special_pred_decl_for_real(SpecialPredId, TVarSet,
	% 	Type, TypeCtor, TypeContext, TypeStatus, !ModuleInfo).
	%
	% Add declarations for a special predicate.
	% This is used by higher_order.m when specializing an in-in
	% unification for an imported type for which unification procedures
	% are generated lazily.
:- pred add_special_pred_decl_for_real(special_pred_id::in,
	tvarset::in, (type)::in, type_ctor::in, prog_context::in,
	import_status::in, module_info::in, module_info::out) is det.

:- type qual_info.

	% Given the definition for a predicate or function from a
	% type class instance declaration, produce the clauses_info
	% for that definition.
:- pred produce_instance_method_clauses(instance_proc_def::in,
	pred_or_func::in, arity::in, list(type)::in, pred_markers::in,
	term__context::in, import_status::in, clauses_info::out,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

	% Move the recompilation_info from the qual_info to the module_info
	% after make_hlds is finished with it and the qual_info is dead.
:- pred set_module_recompilation_info(qual_info::in,
	module_info::in, module_info::out) is det.

:- pred next_mode_id(proc_table::in, maybe(determinism)::in, proc_id::out)
	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs__export.
:- import_module backend_libs__foreign.
:- import_module check_hlds__clause_to_proc.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_errors.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__purity.
:- import_module check_hlds__typecheck.
:- import_module check_hlds__type_util.
:- import_module check_hlds__unify_proc.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_code_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__make_tags.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend.
:- import_module ll_backend__fact_table.
:- import_module ll_backend__llds.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__module_qual.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_dcg.
:- import_module parse_tree__prog_io_goal.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module recompilation.
:- import_module transform_hlds__term_util.

:- import_module string, char, int, set, map, multi_map, require.
:- import_module bag, term, varset, getopt, assoc_list, term_io.

parse_tree_to_hlds(module(Name, Items), MQInfo0, EqvMap, Module, QualInfo,
		InvalidTypes, InvalidModes, !IO) :-
   some [!Module] (
	globals__io_get_globals(Globals, !IO),
	mq_info_get_partial_qualifier_info(MQInfo0, PQInfo),
		module_info_init(Name, Items, Globals, PQInfo, no, !:Module),
		add_item_list_decls_pass_1(Items,
			item_status(local, may_be_unqualified), !Module,
			no, InvalidModes0, !IO),
	globals__io_lookup_bool_option(statistics, Statistics, !IO),
	maybe_report_stats(Statistics, !IO),

	check_for_errors(
		add_item_list_decls_pass_2(Items,
			item_status(local, may_be_unqualified)),
		InvalidTypes1, !Module, !IO),

	% Add constructors and special preds to the HLDS.
	% This must be done after adding all type and
	% `:- pragma foreign_type' declarations.
	% If there were errors in foreign type type declarations,
	% doing this may cause a compiler abort.
	(
		InvalidTypes1 = no,
		module_info_types(!.Module, Types),
		map__foldl3(process_type_defn, Types,
			no, InvalidTypes2, !Module, !IO)
	;
		InvalidTypes1 = yes,
		InvalidTypes2 = yes
	),

	% Add the special preds for the builtin types which don't have a
	% type declaration, hence no hlds_type_defn is generated for them.
	(
		Name = mercury_public_builtin_module,
		compiler_generated_rtti_for_builtins(!.Module)
	->
		varset__init(TVarSet),
		Body = abstract_type(non_solver_type),
		term__context_init(Context),
		Status = local,
		list__foldl(
			(pred(TypeCtor::in, M0::in, M::out) is det :-
				construct_type(TypeCtor, [], Type),
				add_special_preds(TVarSet, Type, TypeCtor,
						Body, Context, Status, M0, M)
			), builtin_type_ctors_with_no_hlds_type_defn, !Module)
	;
		true
	),

	maybe_report_stats(Statistics, !IO),
		% balance the binary trees
	module_info_optimize(!Module),
	maybe_report_stats(Statistics, !IO),
	init_qual_info(MQInfo0, EqvMap, QualInfo0),
	add_item_list_clauses(Items, local, !Module, QualInfo0, QualInfo, !IO),

	qual_info_get_mq_info(QualInfo, MQInfo),
	mq_info_get_type_error_flag(MQInfo, InvalidTypes3),
	InvalidTypes = InvalidTypes1 `or` InvalidTypes2 `or` InvalidTypes3,
	mq_info_get_mode_error_flag(MQInfo, InvalidModes1),
	InvalidModes = InvalidModes0 `or` InvalidModes1,
	mq_info_get_num_errors(MQInfo, MQ_NumErrors),

	module_info_num_errors(!.Module, NumErrors5),
	NumErrors = NumErrors5 + MQ_NumErrors,
	module_info_set_num_errors(NumErrors, !Module),
		% the predid list is constructed in reverse order, for
		% efficiency, so we return it to the correct order here.
	module_info_reverse_predids(!Module),
	Module = !.Module
    ).

:- pred check_for_errors(pred(module_info, module_info, io, io)
	::pred(in, out, di, uo) is det, bool::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_for_errors(P, FoundError, !Module, !IO) :-
	io__get_exit_status(BeforeStatus, !IO),
	io__set_exit_status(0, !IO),
	module_info_num_errors(!.Module, BeforeNumErrors),
	P(!Module, !IO),
	module_info_num_errors(!.Module, AfterNumErrors),
	io__get_exit_status(AfterStatus, !IO),
	(
		AfterStatus = 0,
		BeforeNumErrors = AfterNumErrors
	->
		FoundError = no
	;
		FoundError = yes
	),
	( BeforeStatus \= 0 ->
		io__set_exit_status(BeforeStatus, !IO)
	;
		true
	).

%-----------------------------------------------------------------------------%

	% When adding an item to the HLDS we need to know both its
	% import_status and whether uses of it must be module qualified.
:- type item_status
	---> item_status(import_status, need_qualifier).

%-----------------------------------------------------------------------------%

	% pass 1:
	% Add the declarations one by one to the module,
	% except for type definitions and pragmas.

	% The `InvalidModes' bool records whether we detected
	% any cyclic insts or modes.

:- pred add_item_list_decls_pass_1(item_list::in, item_status::in,
	module_info::in, module_info::out, bool::in, bool::out,
	io::di, io::uo) is det.

add_item_list_decls_pass_1([], _, !Module, !InvalidModes, !IO).
add_item_list_decls_pass_1([Item - Context | Items], Status0, !Module,
		!InvalidModes, !IO) :-
	add_item_decl_pass_1(Item, Context, Status0, Status1, !Module,
		NewInvalidModes, !IO),
	!:InvalidModes = bool__or(!.InvalidModes, NewInvalidModes),
	add_item_list_decls_pass_1(Items, Status1, !Module, !InvalidModes,
		!IO).

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

:- pred add_item_list_decls_pass_2(item_list::in, item_status::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

add_item_list_decls_pass_2([], _, !Module, !IO).
add_item_list_decls_pass_2([Item - Context | Items], Status0, !Module, !IO) :-
	add_item_decl_pass_2(Item, Context, Status0, Status1, !Module, !IO),
	add_item_list_decls_pass_2(Items, Status1, !Module, !IO).

	% pass 3:
	% Add the clauses one by one to the module.
	% (I supposed this could conceivably be folded into pass 2?)
	%
	% Check that the declarations for field extraction
	% and update functions are sensible.

:- pred add_item_list_clauses(item_list::in, import_status::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

add_item_list_clauses([], _Status, !Module, !Info, !IO).
add_item_list_clauses([Item - Context | Items], Status0, !Module, !Info,
		!IO) :-
	add_item_clause(Item, Status0, Status1, Context, !Module, !Info, !IO),
	add_item_list_clauses(Items, Status1, !Module, !Info, !IO).

%-----------------------------------------------------------------------------%

	% The bool records whether any cyclic insts or modes were
	% detected.

:- pred add_item_decl_pass_1(item::in, prog_context::in,
	item_status::in, item_status::out, module_info::in, module_info::out,
	bool::out, io::di, io::uo) is det.

	% Dispatch on the different types of items.

	% skip clauses
add_item_decl_pass_1(clause(_, _, _, _, _), _, !Status, !Module, no, !IO).

	% If this is a solver type then we need to also add the declarations
	% for the compiler generated construction function and deconstruction
	% predicate for the special constrained data constructor.
	%
	% In pass 3 we add the corresponding clauses.
	%
	% Before switch detection, we turn calls to these functions/predicates
	% into ordinary constructions/deconstructions, but preserve the
	% corresponding impurity annotations.
	%
add_item_decl_pass_1(
		type_defn(TVarSet, SymName, TypeParams, TypeDefn, _Cond),
		Context, !Status, !Module, no, !IO) :-
	(
		TypeDefn = solver_type(SolverTypeDetails, _MaybeUserEqComp)
	->
		add_solver_type_decl_items(TVarSet, SymName, TypeParams,
			SolverTypeDetails, Context,
			!Status, !Module, !IO)
	;
		true
	).

add_item_decl_pass_1(inst_defn(VarSet, Name, Params, InstDefn, Cond), Context,
		!Status, !Module, InvalidMode, !IO) :-
	module_add_inst_defn(VarSet, Name, Params, InstDefn, Cond, Context,
		!.Status, !Module, InvalidMode, !IO).

add_item_decl_pass_1(mode_defn(VarSet, Name, Params, ModeDefn, Cond), Context,
		!Status, !Module, InvalidMode, !IO) :-
	module_add_mode_defn(VarSet, Name, Params, ModeDefn,
		Cond, Context, !.Status, !Module, InvalidMode, !IO).

add_item_decl_pass_1(pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
		PredOrFunc, PredName, TypesAndModes, _WithType, _WithInst,
		MaybeDet, _Cond, Purity, ClassContext),
		Context, !Status, !Module, no, !IO) :-
	init_markers(Markers),
	module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
		PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity,
		ClassContext, Markers, Context, !.Status, _, !Module, !IO).

add_item_decl_pass_1(pred_or_func_mode(VarSet, MaybePredOrFunc, PredName,
			Modes, _WithInst, MaybeDet, _Cond),
		Context, !Status, !Module, no, !IO) :-
	( MaybePredOrFunc = yes(PredOrFunc) ->
		!.Status = item_status(ImportStatus, _),
		IsClassMethod = no,
		module_add_mode(VarSet, PredName, Modes, MaybeDet,
			ImportStatus, Context, PredOrFunc, IsClassMethod, _,
			!Module, !IO)
	;
		% equiv_type.m should have either set the pred_or_func
		% or removed the item from the list.
		unexpected(this_file, "add_item_decl_pass_1: " ++
			"no pred_or_func on mode declaration")
	).

add_item_decl_pass_1(pragma(_), _, !Status, !Module, no, !IO).

add_item_decl_pass_1(promise(_, _, _, _), _, !Status, !Module, no, !IO).

add_item_decl_pass_1(module_defn(_VarSet, ModuleDefn), Context,
		!Status, !Module, no, !IO) :-
	( module_defn_update_import_status(ModuleDefn, Status1) ->
		!:Status = Status1
	; ModuleDefn = import(module(Specifiers)) ->
		!.Status = item_status(IStat, _),
		(
			( status_defined_in_this_module(IStat, yes)
			; IStat = imported(ancestor_private_interface)
			)
		->
			module_add_imported_module_specifiers(Specifiers,
				!Module)
		;
			module_add_indirectly_imported_module_specifiers(
				Specifiers, !Module)
		)
	; ModuleDefn = use(module(Specifiers)) ->
		!.Status = item_status(IStat, _),
		(
			( status_defined_in_this_module(IStat, yes)
			; IStat = imported(ancestor)
			)
		->
			module_add_imported_module_specifiers(Specifiers,
				!Module)
		;
			module_add_indirectly_imported_module_specifiers(
				Specifiers, !Module)
		)
	; ModuleDefn = include_module(_) ->
		true
	; ModuleDefn = external(External) ->
		( External = name_arity(Name, Arity) ->
			module_mark_as_external(Name, Arity, Context,
				!Module, !IO)
		;
			prog_out__write_context(Context, !IO),
			report_warning("Warning: `external' declaration " ++
				"requires arity.\n", !IO)
		)
	; ModuleDefn = module(_ModuleName) ->
		report_unexpected_decl("module", Context, !IO)
	; ModuleDefn = end_module(_ModuleName) ->
		report_unexpected_decl("end_module", Context, !IO)
	; ModuleDefn = version_numbers(_, _) ->
		true
	; ModuleDefn = transitively_imported ->
		true
	;
		prog_out__write_context(Context, !IO),
		report_warning("Warning: declaration not yet implemented.\n",
			!IO)
	).

add_item_decl_pass_1(nothing(_), _, !Status, !Module, no, !IO).

add_item_decl_pass_1(typeclass(Constraints, Name, Vars, Interface, VarSet),
		Context, !Status, !Module, no, !IO) :-
	module_add_class_defn(Constraints, Name, Vars, Interface,
		VarSet, Context, !.Status, !Module, !IO).

	% We add instance declarations on the second pass so that we don't add
	% an instance declaration before its class declaration.
add_item_decl_pass_1(instance(_, _, _, _, _,_), _, !Status, !Module, no, !IO).

%-----------------------------------------------------------------------------%

	% A solver type t defined with
	%
	% :- solver type st
	% 	where representation is rt,		% type
	% 	      initialisation is ip,		% pred
	% 	      ground         is gi,		% inst
	% 	      any            is ai, ...		% inst
	%
	% causes the following to be introduced:
	%
	% :- impure func 'representation of ground st'(st::in) =
	% 			(rt::out(gi)) is det.
	% :- impure func 'representation of any st'(st::in(any)) =
	% 			(rt::out(ai)) is det.
	%
	% :- impure func 'representation to ground st'(rt::in(gi)) =
	% 			(st::out) is det.
	% :- impure func 'representation to any st'(rt::in(ai)) =
	% 			(st::out(any)) is det.
	%
:- pred add_solver_type_decl_items(
		tvarset::in, sym_name::in, list(type_param)::in,
		solver_type_details::in, prog_context::in,
		item_status::in, item_status::out,
		module_info::in, module_info::out, io::di, io::uo) is det.

add_solver_type_decl_items(TVarSet, TypeSymName, TypeParams,
		SolverTypeDetails, Context,
		!Status, !Module, !IO) :-

	SolverType        = sym_name_and_args_to_term(TypeSymName,
				TypeParams, Context),
	Arity             = length(TypeParams),

	RepnType          = SolverTypeDetails ^ representation_type,
	AnyInst           = SolverTypeDetails ^ any_inst,
	GroundInst        = SolverTypeDetails ^ ground_inst,

	InAnyMode         = in_mode(AnyInst),
	InGroundMode      = in_mode(GroundInst),

	OutAnyMode        = out_mode(AnyInst),
	OutGroundMode     = out_mode(GroundInst),

	InstVarSet        = varset__init,
	ExistQTVars       = [],

	% Insert the conversion function declarations.

		% The `:- impure
		%	func 'representation of ground st'(st::in(gi)) =
		% 			(rt::out) is det' declaration.
		%
	ToGroundRepnSymName     =
		solver_to_ground_repn_symname(TypeSymName, Arity),
	ToGroundRepnArgTypes    =
		[type_and_mode(SolverType, in_mode      ),
		 type_and_mode(RepnType,   OutGroundMode)],
	ToGroundRepnTypeSigItem =
		pred_or_func(TVarSet, InstVarSet, ExistQTVars,
			function,
			ToGroundRepnSymName,
			ToGroundRepnArgTypes,
			no,	/* no `with_type` ... */
			no,	/* no `with_inst` ... */
			yes(det),
			true,	/* no `where ...' */
			(impure), 
			constraints([], []) /* no type class constraints */
		),
	add_item_decl_pass_1(ToGroundRepnTypeSigItem, Context,
		!Status, !Module, InvalidToGroundRepnMode, !IO),

	( InvalidToGroundRepnMode = yes ->
		error("make_hlds.add_solver_type_decl_items: invalid mode " ++
			"in ToGroundRepn item")
	;
		true
	),

		% The `:- impure
		%	func 'representation of any st'(st::in(ai)) =
		% 			(rt::out(any)) is det' declaration.
		%
	ToAnyRepnSymName     =
		solver_to_any_repn_symname(TypeSymName, Arity),
	ToAnyRepnArgTypes    =
		[type_and_mode(SolverType, in_any_mode ),
		 type_and_mode(RepnType,   OutAnyMode)],
	ToAnyRepnTypeSigItem =
		pred_or_func(TVarSet, InstVarSet, ExistQTVars,
			function,
			ToAnyRepnSymName,
			ToAnyRepnArgTypes,
			no,	/* no `with_type` ... */
			no,	/* no `with_inst` ... */
			yes(det),
			true,	/* no `where ...' */
			(impure), 
			constraints([], []) /* no type class constraints */
		),
	add_item_decl_pass_1(ToAnyRepnTypeSigItem, Context,
		!Status, !Module, InvalidToAnyRepnMode, !IO),

	( InvalidToAnyRepnMode = yes ->
		error("make_hlds.add_solver_type_decl_items: invalid mode " ++
			"in ToAnyRepn item")
	;
		true
	),

		% The `:- impure
		%	func 'representation to ground st'(rt::in(gi)) =
		% 			(st::out) is det' declaration.
		%
	FromGroundRepnSymName     =
		repn_to_ground_solver_symname(TypeSymName, Arity),
	FromGroundRepnArgTypes    =
		[type_and_mode(RepnType,   InGroundMode   ),
		 type_and_mode(SolverType, out_mode       )],
	FromGroundRepnTypeSigItem =
		pred_or_func(TVarSet, InstVarSet, ExistQTVars,
			function,
			FromGroundRepnSymName,
			FromGroundRepnArgTypes,
			no,	/* no `with_type` ... */
			no,	/* no `with_inst` ... */
			yes(det),
			true,	/* no `where ...' */
			(impure), 
			constraints([], []) /* no type class constraints */
		),
	add_item_decl_pass_1(FromGroundRepnTypeSigItem, Context,
		!Status, !Module, InvalidFromGroundRepnMode, !IO),

	( InvalidFromGroundRepnMode = yes ->
		error("make_hlds.add_solver_type_decl_items: invalid mode " ++
			"in FromGroundRepn item")
	;
		true
	),

		% The `:- impure
		%	func 'representation to any st'(rt::in(ai)) =
		% 			(st::out(any)) is det' declaration.
		%
	FromAnyRepnSymName     =
		repn_to_any_solver_symname(TypeSymName, Arity),
	FromAnyRepnArgTypes    =
		[type_and_mode(RepnType,   InAnyMode   ),
		 type_and_mode(SolverType, out_any_mode)],
	FromAnyRepnTypeSigItem =
		pred_or_func(TVarSet, InstVarSet, ExistQTVars,
			function,
			FromAnyRepnSymName,
			FromAnyRepnArgTypes,
			no,	/* no `with_type` ... */
			no,	/* no `with_inst` ... */
			yes(det),
			true,	/* no `where ...' */
			(impure), 
			constraints([], []) /* no type class constraints */
		),
	add_item_decl_pass_1(FromAnyRepnTypeSigItem, Context,
		!Status, !Module, InvalidFromAnyRepnMode, !IO),

	( InvalidFromAnyRepnMode = yes ->
		error("make_hlds.add_solver_type_decl_items: invalid mode " ++
			"in FromAnyRepn item")
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Obtain the solver type conversion function sym_names from
	% the solver type sym_name.
	%
:- func solver_to_ground_repn_symname(sym_name, arity) = sym_name.
solver_to_ground_repn_symname(SymName, Arity) =
	solver_conversion_fn_symname("representation of ground ",
		SymName, Arity).

:- func solver_to_any_repn_symname(sym_name, arity) = sym_name.
solver_to_any_repn_symname(SymName, Arity) =
	solver_conversion_fn_symname("representation of any ",
		SymName, Arity).

:- func repn_to_ground_solver_symname(sym_name, arity) = sym_name.
repn_to_ground_solver_symname(SymName, Arity) =
	solver_conversion_fn_symname("representation to ground ",
		SymName, Arity).

:- func repn_to_any_solver_symname(sym_name, arity) = sym_name.
repn_to_any_solver_symname(SymName, Arity) =
	solver_conversion_fn_symname("representation to any ",
		SymName, Arity).


:- func solver_conversion_fn_symname(string, sym_name, arity) = sym_name.
solver_conversion_fn_symname(Prefix, unqualified(Name), Arity) =
	unqualified(
		Prefix ++ Name ++ "/" ++ int_to_string(Arity)
	).
solver_conversion_fn_symname(Prefix, qualified(ModuleNames, Name), Arity) =
	qualified(ModuleNames,
		Prefix ++ Name ++ "/" ++ int_to_string(Arity)
	).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl_pass_2(item::in, prog_context::in, item_status::in,
	item_status::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

add_item_decl_pass_2(module_defn(_VarSet, ModuleDefn), _Context,
		!Status, !Module, !IO) :-
	( module_defn_update_import_status(ModuleDefn, Status1) ->
		!:Status = Status1
	;
		true
	).

add_item_decl_pass_2(type_defn(VarSet, Name, Args, TypeDefn, Cond),
		Context, !Status, !Module, !IO) :-
	module_add_type_defn(VarSet, Name, Args, TypeDefn,
		Cond, Context, !.Status, !Module, !IO).

add_item_decl_pass_2(pragma(Pragma), Context, !Status, !Module, !IO) :-
	%
	% check for invalid pragmas in the `interface' section
	%
	!.Status = item_status(ImportStatus, _),
	pragma_allowed_in_interface(Pragma, Allowed),
	( Allowed = no ->
		check_not_exported(ImportStatus, Context,
			"`pragma' declaration", !IO)
	;
		true
	),
	%
	% switch on the pragma type
	%
	(
		% ignore `pragma source_file' declarations - they're dealt
		% with elsewhere
		Pragma = source_file(_)
	;
		Pragma = foreign_code(Lang, Body_Code),
		module_add_foreign_body_code(Lang, Body_Code, Context,
			!Module)
	;
		Pragma  = foreign_decl(Lang, IsLocal, C_Header),
		module_add_foreign_decl(Lang, IsLocal, C_Header, Context,
			!Module)
	;
		Pragma  = foreign_import_module(Lang, Import),
		module_add_foreign_import_module(Lang, Import, Context,
			!Module)
	;
		% Handle pragma foreign procs later on (when we process
		% clauses).
		Pragma = foreign_proc(_, _, _, _, _, _)
	;
		% Handle pragma tabled decls later on (when we process
		% clauses).
		Pragma = tabled(_, _, _, _, _)
	;
		Pragma = inline(Name, Arity),
		add_pred_marker("inline", Name, Arity, ImportStatus, Context,
			inline, [no_inline], !Module, !IO)
	;
		Pragma = no_inline(Name, Arity),
		add_pred_marker("no_inline", Name, Arity, ImportStatus,
			Context, no_inline, [inline], !Module, !IO)
	;
		Pragma = obsolete(Name, Arity),
		add_pred_marker("obsolete", Name, Arity, ImportStatus,
			Context, obsolete, [], !Module, !IO)
	;
		% Handle pragma import decls later on (when we process
		% clauses and pragma c_code).
		Pragma = import(_, _, _, _, _)
	;
		Pragma = export(Name, PredOrFunc, Modes, C_Function),
		add_pragma_export(Name, PredOrFunc, Modes, C_Function,
			Context, !Module, !IO)
	;
			% Used for inter-module unused argument elimination.
			% This can only appear in .opt files.
		Pragma = unused_args(PredOrFunc, SymName, Arity, ModeNum,
			UnusedArgs),
		( ImportStatus \= opt_imported ->
			prog_out__write_context(Context, !IO),
			io__write_string("Error: illegal use of pragma " ++
				"`unused_args'.\n", !IO),
			module_info_incr_errors(!Module)
		;
			add_pragma_unused_args(PredOrFunc, SymName, Arity,
				ModeNum, UnusedArgs, Context, !Module, !IO)
		)
	;
		Pragma = exceptions(PredOrFunc, SymName, Arity, ModeNum,
			ThrowStatus),
		( ImportStatus \= opt_imported ->
			prog_out.write_context(Context, !IO),
			io.write_string("Error: illegal use of pragma " ++
				"`exceptions'.\n", !IO),
			module_info_incr_errors(!Module)
		;
			add_pragma_exceptions(PredOrFunc, SymName, Arity,
				ModeNum, ThrowStatus, Context, !Module, !IO)
		)
	;
		% Handle pragma type_spec decls later on (when we process
		% clauses).
		Pragma = type_spec(_, _, _, _, _, _, _, _)
	;
		% Handle pragma fact_table decls later on (when we process
		% clauses -- since these decls take the place of clauses).
		Pragma = fact_table(_, _, _)
	;
		% Handle pragma reserve_tag decls later on (when we process
		% clauses -- they need to be handled after the type definitions
		% have been added).
		Pragma = reserve_tag(_, _)
	;
		Pragma = aditi(PredName, Arity),
		maybe_enable_aditi_compilation(!.Status, Context, !Module,
			!IO),
		add_pred_marker("aditi", PredName, Arity, ImportStatus,
			Context, aditi, [], !Module, !IO),
		add_stratified_pred("aditi", PredName, Arity, Context,
			!Module, !IO)
	;
		Pragma = base_relation(PredName, Arity),
		maybe_enable_aditi_compilation(!.Status, Context, !Module,
			!IO),
		add_pred_marker("aditi", PredName, Arity, ImportStatus,
			Context, aditi, [], !Module, !IO),
		add_pred_marker("base_relation", PredName, Arity, ImportStatus,
			Context, base_relation, [], !Module, !IO),
		module_mark_as_external(PredName, Arity, Context, !Module, !IO)
	;
		Pragma = aditi_index(PredName, Arity, Index),
		add_base_relation_index(PredName, Arity, Index, ImportStatus,
			Context, !Module, !IO)
	;
		Pragma = naive(PredName, Arity),
		add_pred_marker("naive", PredName, Arity, ImportStatus,
			Context, naive, [psn], !Module, !IO)
	;
		Pragma = psn(PredName, Arity),
		add_pred_marker("psn", PredName, Arity, ImportStatus,
			Context, psn, [naive], !Module, !IO)
	;
		Pragma = aditi_memo(Name, Arity),
		add_pred_marker("aditi_memo", Name, Arity, ImportStatus,
			Context, aditi_memo, [aditi_no_memo], !Module, !IO)
	;
		Pragma = aditi_no_memo(PredName, Arity),
		add_pred_marker("aditi_no_memo", PredName, Arity, ImportStatus,
			Context, aditi_no_memo, [aditi_memo], !Module, !IO)
	;
		Pragma = supp_magic(PredName, Arity),
		add_pred_marker("supp_magic", PredName, Arity, ImportStatus,
			Context, supp_magic, [context], !Module, !IO)
	;
		Pragma = context(PredName, Arity),
		add_pred_marker("context", PredName, Arity, ImportStatus,
			Context, context, [supp_magic], !Module, !IO)
	;
		Pragma = owner(PredName, Arity, Owner),
		set_pred_owner(PredName, Arity, Owner, ImportStatus,
			Context, !Module, !IO)
	;
		Pragma = promise_pure(Name, Arity),
		add_pred_marker("promise_pure", Name, Arity, ImportStatus,
			Context, promised_pure, [], !Module, !IO)
	;
		Pragma = promise_semipure(Name, Arity),
		add_pred_marker("promise_semipure", Name, Arity, ImportStatus,
			Context, promised_semipure, [], !Module, !IO)
	;
		% Handle pragma termination_info decls later on, in pass 3 --
		% we need to add function default modes before handling
		% these pragmas
		Pragma = termination_info(_, _, _, _, _)
	;
		Pragma = terminates(Name, Arity),
		add_pred_marker("terminates", Name, Arity,
			ImportStatus, Context, terminates,
			[check_termination, does_not_terminate], !Module, !IO)
	;
		Pragma = does_not_terminate(Name, Arity),
		add_pred_marker("does_not_terminate", Name, Arity,
			ImportStatus, Context, does_not_terminate,
			[check_termination, terminates], !Module, !IO)
	;
		Pragma = check_termination(Name, Arity),
		add_pred_marker("check_termination", Name, Arity,
			ImportStatus, Context, check_termination,
			[terminates, does_not_terminate], !Module, !IO)
	).

add_item_decl_pass_2(pred_or_func(_TypeVarSet, _InstVarSet, _ExistQVars,
		PredOrFunc, SymName, TypesAndModes, _WithType, _WithInst,
		_MaybeDet, _Cond, _Purity, _ClassContext),
		_Context, !Status, !Module, !IO) :-
	%
	% add default modes for function declarations, if necessary
	%
	(
		PredOrFunc = predicate
	;
		PredOrFunc = function,
		list__length(TypesAndModes, Arity),
		adjust_func_arity(function, FuncArity, Arity),
		module_info_get_predicate_table(!.Module, PredTable0),
		(
			predicate_table_search_func_sym_arity(PredTable0,
				is_fully_qualified, SymName,
				FuncArity, PredIds)
		->
			predicate_table_get_preds(PredTable0, Preds0),
			maybe_add_default_func_modes(PredIds, Preds0, Preds),
			predicate_table_set_preds(Preds,
				PredTable0, PredTable),
			module_info_set_predicate_table(PredTable, !Module)
		;
			error("make_hlds.m: can't find func declaration")
		)
	).
add_item_decl_pass_2(promise(_, _, _, _), _, !Status, !Module, !IO).
add_item_decl_pass_2(clause(_, _, _, _, _), _, !Status, !Module, !IO).
add_item_decl_pass_2(inst_defn(_, _, _, _, _), _, !Status, !Module, !IO).
add_item_decl_pass_2(mode_defn(_, _, _, _, _), _, !Status, !Module, !IO).
add_item_decl_pass_2(pred_or_func_mode(_, _, _, _, _, _, _), _,
		!Status, !Module, !IO).
add_item_decl_pass_2(nothing(_), _, !Status, !Module, !IO).
add_item_decl_pass_2(typeclass(_, _, _, _, _) , _, !Status, !Module, !IO).
add_item_decl_pass_2(instance(Constraints, Name, Types, Body, VarSet,
		InstanceModuleName), Context, !Status, !Module, !IO) :-
	!.Status = item_status(ImportStatus, _),
	( Body = abstract ->
		make_status_abstract(ImportStatus, BodyStatus)
	;
		BodyStatus = ImportStatus
	),
	module_add_instance_defn(InstanceModuleName, Constraints, Name, Types,
		Body, VarSet, BodyStatus, Context, !Module, !IO).

%------------------------------------------------------------------------------

	% If a module_defn updates the import_status, return the new
	% status and whether uses of the following items must be module
	% qualified, otherwise fail.
:- pred module_defn_update_import_status(module_defn::in, item_status::out)
	is semidet.

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
module_defn_update_import_status(abstract_imported,
		item_status(abstract_imported, must_be_qualified)).

%-----------------------------------------------------------------------------%

	% If there are any Aditi procedures enable Aditi compilation.
	% If there are only imported Aditi procedures, magic.m still
	% needs to remove the `aditi' and `base_relation' markers
	% so that the procedures are not ignored by the code
	% generation annotation passes (e.g. arg_info.m).
:- pred maybe_enable_aditi_compilation(item_status::in, term__context::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

maybe_enable_aditi_compilation(_Status, Context, !Module, !IO) :-
	globals__io_lookup_bool_option(aditi, Aditi, !IO),
	( Aditi = no ->
		prog_out__write_context(Context, !IO),
		io__write_string("Error: compilation of Aditi procedures\n",
			!IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  requires the `--aditi' option.\n", !IO),
		io__set_exit_status(1, !IO),
		module_info_incr_errors(!Module)
	;
		% There are Aditi procedures - enable Aditi code generation.
		module_info_set_do_aditi_compilation(!Module)
	).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_clause(item::in, import_status::in, import_status::out,
	prog_context::in, module_info::in, module_info::out,
	qual_info::in, qual_info::out, io::di, io::uo) is det.

add_item_clause(clause(VarSet, PredOrFunc, PredName, Args, Body),
		!Status, Context, !Module, !Info, !IO) :-
	check_not_exported(!.Status, Context, "clause", !IO),
	GoalType = none,
	% at this stage we only need know that it's not a promise declaration
	module_add_clause(VarSet, PredOrFunc, PredName, Args, Body, !.Status,
		Context, GoalType, !Module, !Info, !IO).

add_item_clause(type_defn(_TVarSet, SymName, TypeParams, TypeDefn, _Cond),
		!Status, Context, !Module, !Info, !IO) :-
	% If this is a solver type then we need to also add clauses
	% the compiler generated inst cast predicate (the declaration
	% for which was added in pass 1).
	(
		TypeDefn = solver_type(SolverTypeDetails, _MaybeUserEqComp)
	->
		add_solver_type_clause_items(SymName, TypeParams,
			SolverTypeDetails, !Status, Context, !Module, !Info,
			!IO)
	;
		true
	).
add_item_clause(inst_defn(_, _, _, _, _), !Status, _, !Module, !Info, !IO).
add_item_clause(mode_defn(_, _, _, _, _), !Status, _, !Module, !Info, !IO).
add_item_clause(pred_or_func(_, _, _, PredOrFunc, SymName, TypesAndModes,
			_WithType, _WithInst, _, _, _, _),
		!Status, Context, !Module, !Info, !IO) :-
	(
		PredOrFunc = predicate
	;
		PredOrFunc = function,
		list__length(TypesAndModes, PredArity),
		adjust_func_arity(function, FuncArity, PredArity),
		maybe_check_field_access_function(SymName, FuncArity,
			!.Status, Context, !.Module, !IO)
	).

add_item_clause(pred_or_func_mode(_, _, _, _, _, _, _), !Status, _,
		!Module, !Info, !IO).
add_item_clause(module_defn(_, Defn), !Status, _, !Module, !Info, !IO) :-
	( Defn = version_numbers(ModuleName, ModuleVersionNumbers) ->
		%
		% Record the version numbers for each imported module
		% if smart recompilation is enabled.
		%
		apply_to_recompilation_info(
			(pred(RecompInfo0::in, RecompInfo::out) is det :-
				RecompInfo = RecompInfo0 ^ version_numbers ^
					map__elem(ModuleName) :=
					ModuleVersionNumbers
			),
			transform_info(!.Module, !.Info),
			transform_info(!:Module, !:Info))
	; module_defn_update_import_status(Defn, ItemStatus1) ->
		ItemStatus1 = item_status(!:Status, NeedQual),
		qual_info_get_mq_info(!.Info, MQInfo0),
		mq_info_set_need_qual_flag(MQInfo0, NeedQual, MQInfo),
		qual_info_set_mq_info(MQInfo, !Info)
	;
		true
	).
add_item_clause(pragma(Pragma), !Status, Context, !Module, !Info, !IO) :-
	(
		Pragma = foreign_proc(Attributes, Pred, PredOrFunc,
			Vars, VarSet, PragmaImpl)
	->
		module_add_pragma_foreign_proc(Attributes, Pred, PredOrFunc,
			Vars, VarSet, PragmaImpl, !.Status, Context,
			!Module, !Info, !IO)
	;
		Pragma = import(Name, PredOrFunc, Modes, Attributes,
			C_Function)
	->
		module_add_pragma_import(Name, PredOrFunc, Modes, Attributes,
			C_Function, !.Status, Context, !Module, !Info, !IO)
	;
		Pragma = fact_table(Pred, Arity, File)
	->
		module_add_pragma_fact_table(Pred, Arity, File, !.Status,
			Context, !Module, !Info, !IO)
	;
		Pragma = tabled(Type, Name, Arity, PredOrFunc, Mode)
	->
		globals__io_lookup_bool_option(type_layout, TypeLayout, !IO),
		(
			TypeLayout = yes,
			module_add_pragma_tabled(Type, Name, Arity, PredOrFunc,
				Mode, !.Status, Context, !Module, !IO)
		;
			TypeLayout = no,
			module_info_incr_errors(!Module),
			prog_out__write_context(Context, !IO),
			io__write_string("Error: `:- pragma ", !IO),
			EvalMethodS = eval_method_to_string(Type),
			io__write_string(EvalMethodS, !IO),
			io__write_string("' declaration requires the " ++
				"type_ctor_layout\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("    structures. Use " ++
				"the --type-layout flag to enable them.\n",
				!IO)
		)
	;
		Pragma = type_spec(_, _, _, _, _, _, _, _)
	->
		%
		% XXX For the Java back-end, `pragma type_spec' can
		% result in class names that exceed the limits on file
		% name length.  So we ignore these pragmas for the
		% Java back-end.
		%
		globals__io_get_target(Target, !IO),
		( Target = java ->
			true
		;
			add_pragma_type_spec(Pragma, Context, !Module, !Info,
				!IO)
		)
	;
 		Pragma = termination_info(PredOrFunc, SymName, ModeList,
 			MaybeArgSizeInfo, MaybeTerminationInfo)
 	->
 		add_pragma_termination_info(PredOrFunc, SymName, ModeList,
 			MaybeArgSizeInfo, MaybeTerminationInfo, Context,
 			!Module, !IO)
 	;
		Pragma = reserve_tag(TypeName, TypeArity)
	->
		add_pragma_reserve_tag(TypeName, TypeArity, !.Status,
			Context, !Module, !IO)
	;
 		% don't worry about any pragma declarations other than the
 		% clause-like pragmas (c_code, tabling and fact_table),
		% foreign_type and the termination_info pragma here,
		% since they've already been handled earlier, in pass 2
		true
	).

add_item_clause(promise(PromiseType, Goal, VarSet, UnivVars),
		!Status, Context, !Module, !Info, !IO) :-
	%
	% If the outermost universally quantified variables
	% are placed in the head of the dummy predicate, the
	% typechecker will avoid warning about unbound
	% type variables as this implicity adds a universal
	% quantification of the typevariables needed.
	%
	term__var_list_to_term_list(UnivVars, HeadVars),

	% extra error checking for promise ex declarations
	( PromiseType \= true ->
		check_promise_ex_decl(UnivVars, PromiseType, Goal, Context,
			!IO)
	;
		true
	),

	% add as dummy predicate
	add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context,
		!.Status, !Module, !Info, !IO).

add_item_clause(nothing(_), !Status, _, !Module, !Info, !IO).
add_item_clause(typeclass(_, _, _, _, _), !Status, _, !Module, !Info, !IO).
add_item_clause(instance(_, _, _, _, _, _), !Status, _, !Module, !Info, !IO).


:- pred add_solver_type_clause_items(sym_name::in, list(type_param)::in,
		solver_type_details::in,
		import_status::in, import_status::out, prog_context::in,
		module_info::in, module_info::out,
		qual_info::in, qual_info::out, io::di, io::uo) is det.

add_solver_type_clause_items(TypeSymName, TypeParams, SolverTypeDetails,
		!Status, Context, !Module, !Info, !IO) :-

	Arity             = length(TypeParams),

	AnyInst           = SolverTypeDetails ^ any_inst,
	GroundInst        = SolverTypeDetails ^ ground_inst,

	InAnyMode         = in_mode(AnyInst),
	InGroundMode      = in_mode(GroundInst),

	OutAnyMode        = out_mode(AnyInst),
	OutGroundMode     = out_mode(GroundInst),

	VarSet0           = varset__init,
	varset__new_var(VarSet0, X, VarSet1),
	varset__new_var(VarSet1, Y, VarSet),

	Attrs0            = default_attributes(c),
	set_may_call_mercury(will_not_call_mercury, Attrs0, Attrs1),
	set_thread_safe(thread_safe,                Attrs1, Attrs2),
	set_terminates(terminates,                  Attrs2, Attrs),

	Impl              = ordinary("Y = X;", yes(Context)),

		% The `func(in) = out(<i_ground>) is det' mode.
		%
	ToGroundRepnSymName =
		solver_to_ground_repn_symname(TypeSymName, Arity),
	ToGroundRepnArgs = [ pragma_var(X, "X", in_mode      ),
                             pragma_var(Y, "Y", OutGroundMode) ],
	ToGroundRepnForeignProc =
		foreign_proc(
			Attrs,
			ToGroundRepnSymName,
			function,
			ToGroundRepnArgs,
			VarSet,
			Impl
		),
	ToGroundRepnItem = pragma(ToGroundRepnForeignProc),
	add_item_clause(ToGroundRepnItem, !Status, Context, !Module, !Info,
		!IO),

		% The `func(in(any)) = out(<i_any>) is det' mode.
		%
	ToAnyRepnSymName =
		solver_to_any_repn_symname(TypeSymName, Arity),
	ToAnyRepnArgs = [ pragma_var(X, "X", in_any_mode),
                          pragma_var(Y, "Y", OutAnyMode ) ],
	ToAnyRepnForeignProc =
		foreign_proc(
			Attrs,
			ToAnyRepnSymName,
			function,
			ToAnyRepnArgs,
			VarSet,
			Impl
		),
	ToAnyRepnItem = pragma(ToAnyRepnForeignProc),
	add_item_clause(ToAnyRepnItem, !Status, Context, !Module, !Info, !IO),

		% The `func(in(<i_ground>)) = out is det' mode.
		%
	FromGroundRepnSymName =
		repn_to_ground_solver_symname(TypeSymName, Arity),
	FromGroundRepnArgs        = [ pragma_var(X, "X", InGroundMode),
			              pragma_var(Y, "Y", out_mode) ],
	FromGroundRepnForeignProc =
		foreign_proc(
			Attrs,
			FromGroundRepnSymName,
			function,
			FromGroundRepnArgs,
			VarSet,
			Impl
		),
	FromGroundRepnItem = pragma(FromGroundRepnForeignProc),
	add_item_clause(FromGroundRepnItem, !Status, Context, !Module, !Info,
		!IO),

		% The `func(in(<i_any>)) = out(any) is det' mode.
		%
	FromAnyRepnSymName =
		repn_to_any_solver_symname(TypeSymName, Arity),
	FromAnyRepnArgs        = [ pragma_var(X, "X", InAnyMode   ),
			           pragma_var(Y, "Y", out_any_mode) ],
	FromAnyRepnForeignProc =
		foreign_proc(
			Attrs,
			FromAnyRepnSymName,
			function,
			FromAnyRepnArgs,
			VarSet,
			Impl
		),
	FromAnyRepnItem = pragma(FromAnyRepnForeignProc),
	add_item_clause(FromAnyRepnItem, !Status, Context, !Module, !Info,
		!IO).

%-----------------------------------------------------------------------------%

	% We need to "unparse" the sym_name to construct the properly
	% module qualified term.
	%
:- func sym_name_and_args_to_term(sym_name, list(term(T)), prog_context) =
		term(T).

sym_name_and_args_to_term(unqualified(Name), Xs, Context) =
	term__functor(term__atom(Name), Xs, Context).

sym_name_and_args_to_term(qualified(ModuleNames, Name), Xs, Context) =
	sym_name_and_term_to_term(ModuleNames,
		term__functor(term__atom(Name), Xs, Context), Context).


:- func sym_name_and_term_to_term(module_specifier, term(T),
		prog_context) = term(T).

sym_name_and_term_to_term(unqualified(ModuleName), Term, Context) =
	term__functor(
		term__atom("."),
		[ term__functor(term__atom(ModuleName), [], Context),
		  Term ],
		Context
	).

sym_name_and_term_to_term(qualified(ModuleNames, ModuleName), Term,
		Context) =
	term__functor(
		term__atom("."),
		[ sym_name_and_term_to_term(
			ModuleNames,
			term__functor(term__atom(ModuleName), [], Context),
			Context
		  ),
		  Term ],
		Context
	).


:- pred add_promise_clause(promise_type::in, list(term(prog_var_type))::in,
	prog_varset::in, goal::in, prog_context::in, import_status::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context, Status,
		!Module, !Info, !IO) :-
	term__context_line(Context, Line),
	term__context_file(Context, File),
	string__format(prog_out__promise_to_string(PromiseType) ++
		"__%d__%s", [i(Line), s(File)], Name),
		%
		% Promise declarations are recorded as a predicate with a
		% goal_type of promise(X), where X is of promise_type. This
		% allows us to leverage off all the other checks in the
		% compiler that operate on predicates.
		%
		% :- promise all [A,B,R] ( R = A + B <=> R = B + A ).
		%
		% becomes
		%
		% promise__lineno_filename(A, B, R) :-
		% 	( R = A + B <=> R = B + A ).
		%
	GoalType = promise(PromiseType) ,
	module_info_name(!.Module, ModuleName),
	module_add_clause(VarSet, predicate, qualified(ModuleName, Name),
		HeadVars, Goal, Status, Context, GoalType,
		!Module, !Info, !IO).

%-----------------------------------------------------------------------------%

:- pred check_not_exported(import_status::in, prog_context::in, string::in,
	io::di, io::uo) is det.

check_not_exported(Status, Context, Message, !IO) :-
		%
		% check that clauses are not exported
		%
	( Status = exported ->
		prog_out__write_context(Context, !IO),
		string__append_list(
			["Warning: ", Message, " in module interface.\n"],
			WarningMessage),
		report_warning(WarningMessage, !IO)
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_export(sym_name::in, pred_or_func::in, list(mode)::in,
	string::in, prog_context::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

add_pragma_export(Name, PredOrFunc, Modes, C_Function, Context, !Module,
		!IO) :-
	module_info_get_predicate_table(!.Module, PredTable),
	list__length(Modes, Arity),
	(
		predicate_table_search_pf_sym_arity(PredTable,
			may_be_partially_qualified, PredOrFunc, Name,
			Arity, [PredId])
	->
		predicate_table_get_preds(PredTable, Preds),
		map__lookup(Preds, PredId, PredInfo),
		pred_info_procedures(PredInfo, Procs),
		map__to_assoc_list(Procs, ExistingProcs),
		(
			get_procedure_matching_declmodes(ExistingProcs, Modes,
				!.Module, ProcId)
		->
			module_info_get_pragma_exported_procs(!.Module,
				PragmaExportedProcs0),
			NewExportedProc = pragma_exported_proc(PredId,
				ProcId, C_Function, Context),
			PragmaExportedProcs =
				[NewExportedProc | PragmaExportedProcs0],
			module_info_set_pragma_exported_procs(
				PragmaExportedProcs, !Module)
		;
			undefined_mode_error(Name, Arity, Context,
				"`:- pragma export' declaration", !IO),
			module_info_incr_errors(!Module)
		)
	;
		undefined_pred_or_func_error(Name, Arity, Context,
			"`:- pragma export' declaration", !IO),
		module_info_incr_errors(!Module)
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_reserve_tag(sym_name::in, arity::in, import_status::in,
	prog_context::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

add_pragma_reserve_tag(TypeName, TypeArity, PragmaStatus, Context, !Module,
		!IO) :-
	TypeCtor = TypeName - TypeArity,
	module_info_types(!.Module, Types0),
	TypeStr = error_util__describe_sym_name_and_arity(
		TypeName / TypeArity),
	ErrorPieces1 = [
		words("In"),
		fixed("`pragma reserve_tag'"),
		words("declaration for"),
		fixed(TypeStr ++ ":")
	],
	( map__search(Types0, TypeCtor, TypeDefn0) ->
		hlds_data__get_type_defn_body(TypeDefn0, TypeBody0),
		hlds_data__get_type_defn_status(TypeDefn0, TypeStatus),
		(
			not (
				TypeStatus = PragmaStatus
			;
				TypeStatus = abstract_exported,
				( PragmaStatus = local
				; PragmaStatus = exported_to_submodules
				)
			)
		->
			error_util__write_error_pieces(Context, 0,
				ErrorPieces1, !IO),
			ErrorPieces2 = [
				words("error: `reserve_tag' declaration must"),
				words("have the same visibility as the"),
				words("type definition.")
			],
			error_util__write_error_pieces_not_first_line(Context,
				0, ErrorPieces2, !IO),
			io__set_exit_status(1, !IO),
			module_info_incr_errors(!Module)

		;
			TypeBody0 = du_type(Body, _CtorTags0, _IsEnum0,
				MaybeUserEqComp, ReservedTag0, IsForeign)
		->
			(
				ReservedTag0 = yes,
				% make doubly sure that we don't get any
				% spurious warnings with intermodule
				% optimization...
				TypeStatus \= opt_imported
			->
				error_util__write_error_pieces(Context, 0,
					ErrorPieces1, !IO),
				ErrorPieces2 = [
					words("warning: multiple"),
					fixed("`pragma reserved_tag'"),
					words("declarations for the same type.")
				],
				error_util__write_error_pieces_not_first_line(
					Context, 0, ErrorPieces2, !IO)
			;
				true
			),
			%
			% We passed all the semantic checks.
			% Mark the type has having a reserved tag,
			% and recompute the constructor tags.
			%
			ReservedTag = yes,
			module_info_globals(!.Module, Globals),
			assign_constructor_tags(Body, TypeCtor, ReservedTag,
				Globals, CtorTags, IsEnum),
			TypeBody = du_type(Body, CtorTags, IsEnum,
				MaybeUserEqComp, ReservedTag, IsForeign),
			hlds_data__set_type_defn_body(TypeBody,
				TypeDefn0, TypeDefn),
			map__set(Types0, TypeCtor, TypeDefn, Types),
			module_info_set_types(Types, !Module)
		;
			error_util__write_error_pieces(Context, 0,
				ErrorPieces1, !IO),
			ErrorPieces2 = [
				words("error:"),
				fixed(TypeStr),
				words("is not a discriminated union type.")
			],
			error_util__write_error_pieces_not_first_line(Context,
				0, ErrorPieces2, !IO),
			io__set_exit_status(1, !IO),
			module_info_incr_errors(!Module)
		)
	;
		error_util__write_error_pieces(Context, 0, ErrorPieces1, !IO),
		ErrorPieces2 = [
			words("error: undefined type"),
			fixed(TypeStr ++ ".")
		],
		error_util__write_error_pieces_not_first_line(Context,
			0, ErrorPieces2, !IO),
		io__set_exit_status(1, !IO),
		module_info_incr_errors(!Module)
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_unused_args(pred_or_func::in, sym_name::in, arity::in,
	mode_num::in, list(int)::in, prog_context::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

add_pragma_unused_args(PredOrFunc, SymName, Arity, ModeNum, UnusedArgs,
		Context, !Module, !IO) :-
	module_info_get_predicate_table(!.Module, Preds),
	(
		predicate_table_search_pf_sym_arity(Preds,
			is_fully_qualified, PredOrFunc, SymName,
			Arity, [PredId])
	->
		module_info_unused_arg_info(!.Module, UnusedArgInfo0),
		% convert the mode number to a proc_id
		proc_id_to_int(ProcId, ModeNum),
		map__set(UnusedArgInfo0, proc(PredId, ProcId), UnusedArgs,
			UnusedArgInfo),
		module_info_set_unused_arg_info(UnusedArgInfo, !Module)
	;
		prog_out__write_context(Context, !IO),
		io__write_string("Internal compiler error: " ++
			"unknown predicate in `pragma unused_args'.\n", !IO),
		module_info_incr_errors(!Module)
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_exceptions(pred_or_func::in, sym_name::in, arity::in,
	mode_num::in, exception_status::in, prog_context::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

add_pragma_exceptions(PredOrFunc, SymName, Arity, ModeNum, ThrowStatus,
		_Context, !Module, !IO) :-
	module_info_get_predicate_table(!.Module, Preds),
	(
		predicate_table_search_pf_sym_arity(Preds,
			is_fully_qualified, PredOrFunc, SymName,
			Arity, [PredId])
	->
		module_info_exception_info(!.Module, ExceptionsInfo0),
		% convert the mode number to a proc_id
		proc_id_to_int(ProcId, ModeNum),
		map__set(ExceptionsInfo0, proc(PredId, ProcId), ThrowStatus,
			ExceptionsInfo),
		module_info_set_exception_info(ExceptionsInfo, !Module)
	;
		% XXX We'll just ignore this for the time being - 
		% it causes errors with transitive-intermodule optimization. 
		%prog_out__write_context(Context, !IO),
		%io__write_string("Internal compiler error: " ++
		%	"unknown predicate in `pragma exceptions'.\n", !IO),
		%module_info_incr_errors(!Module)
		true	
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_type_spec(pragma_type::in(type_spec), term__context::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

add_pragma_type_spec(Pragma, Context, !Module, !Info, !IO) :-
	Pragma = type_spec(SymName, _, Arity, MaybePredOrFunc, _, _, _, _),
	module_info_get_predicate_table(!.Module, Preds),
	(
		( MaybePredOrFunc = yes(PredOrFunc) ->
			adjust_func_arity(PredOrFunc, Arity, PredArity),
			predicate_table_search_pf_sym_arity(Preds,
				is_fully_qualified, PredOrFunc,
				SymName, PredArity, PredIds)
		;
			predicate_table_search_sym_arity(Preds,
				is_fully_qualified, SymName, Arity, PredIds)
		),
		PredIds \= []
	->
		list__foldl2(add_pragma_type_spec_2(Pragma, Context),
			PredIds, transform_info(!.Module, !.Info),
			transform_info(!:Module, !:Info), !IO)
	;
		undefined_pred_or_func_error(SymName, Arity, Context,
			"`:- pragma type_spec' declaration", !IO),
		module_info_incr_errors(!Module)
	).

:- pred add_pragma_type_spec_2(pragma_type::in(type_spec), prog_context::in,
	pred_id::in, transform_info::in, transform_info::out,
	io::di, io::uo) is det.

add_pragma_type_spec_2(Pragma0, Context, PredId,
		transform_info(ModuleInfo0, Info0), TransformInfo, !IO) :-
	Pragma0 = type_spec(SymName, SpecName, Arity, _, MaybeModes, Subst,
		TVarSet0, ExpandedItems),
	module_info_pred_info(ModuleInfo0, PredId, PredInfo0),
	handle_pragma_type_spec_subst(Context, Subst, PredInfo0,
		TVarSet0, TVarSet, Types, ExistQVars, ClassContext, SubstOk,
		ModuleInfo0, ModuleInfo1, !IO),
	( SubstOk = yes(RenamedSubst) ->
		pred_info_procedures(PredInfo0, Procs0),
		handle_pragma_type_spec_modes(SymName, Arity, Context,
			MaybeModes, ProcIds, Procs0, Procs, ModesOk,
			ModuleInfo1, ModuleInfo2, !IO),
		globals__io_lookup_bool_option(user_guided_type_specialization,
			DoTypeSpec, !IO),
		globals__io_lookup_bool_option(smart_recompilation, Smart, !IO),
		(
			ModesOk = yes,
			% Even if we aren't doing type specialization, we need
			% to create the interface procedures for local
			% predicates to check the type-class correctness of
			% the requested specializations.
			%
			% If we're doing smart recompilation we need to record
			% the pragmas even if we aren't doing type
			% specialization to avoid problems with differing
			% output for the recompilation tests in debugging
			% grades.
			%
			( DoTypeSpec = yes
			; \+ pred_info_is_imported(PredInfo0)
			; Smart = yes
			)
		->
			%
			% Build a clause to call the old predicate with the
			% specified types to force the specialization.
			% For imported predicates this forces the creation
			% of the proper interface.
			%
			PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
			adjust_func_arity(PredOrFunc, Arity, PredArity),
			varset__init(ArgVarSet0),
			make_n_fresh_vars("HeadVar__", PredArity,
				Args, ArgVarSet0, ArgVarSet),
			% XXX We could use explicit type qualifications here
			% for the argument types, but explicit type
			% qualification doesn't work correctly with type
			% inference due to a bug somewhere in typecheck.m
			% -- the explicitly declared types are not kept in
			% sync with the predicate's tvarset after the first
			% pass of type checking.
			% map__from_corresponding_lists(Args, Types, VarTypes0)
			map__init(VarTypes0),
			goal_info_init(GoalInfo0),
			set__list_to_set(Args, NonLocals),
			goal_info_set_nonlocals(GoalInfo0, NonLocals,
				GoalInfo1),
			goal_info_set_context(GoalInfo1, Context, GoalInfo),

			%
			% We don't record the called predicate as used -- it
			% is only used if there is some other call. This call
			% is only used to make higher_order.m generate
			% the interface for the type specialized procedure, and
			% will be removed by higher_order.m after that is done.
			%
			do_construct_pred_or_func_call(PredId, PredOrFunc,
				SymName, Args, GoalInfo, Goal),
			Clause = clause(ProcIds, Goal, mercury, Context),
			map__init(TI_VarMap),
			map__init(TCI_VarMap),
			map__init(TVarNameMap),
			HasForeignClauses = no,
			Clauses = clauses_info(ArgVarSet, VarTypes0,
				TVarNameMap, VarTypes0, Args, [Clause],
				TI_VarMap, TCI_VarMap, HasForeignClauses),
			pred_info_get_markers(PredInfo0, Markers0),
			add_marker(calls_are_fully_qualified,
				Markers0, Markers),
			map__init(Proofs),

			( pred_info_is_imported(PredInfo0) ->
				Status = opt_imported
			;
				pred_info_import_status(PredInfo0, Status)
			),

			ModuleName = pred_info_module(PredInfo0),
			pred_info_get_aditi_owner(PredInfo0, Owner),
			pred_info_init(ModuleName, SpecName, PredArity,
				PredOrFunc, Context, Status, none, Markers,
				Types, TVarSet, ExistQVars,
				ClassContext, Proofs,
				Owner, Clauses, NewPredInfo0),
			pred_info_set_procedures(Procs,
				NewPredInfo0, NewPredInfo),
			module_info_get_predicate_table(ModuleInfo2,
				PredTable0),
			predicate_table_insert(NewPredInfo, NewPredId,
				PredTable0, PredTable),
			module_info_set_predicate_table(PredTable,
				ModuleInfo2, ModuleInfo3),

			%
			% Record the type specialisation in the module_info.
			%
			module_info_type_spec_info(ModuleInfo3, TypeSpecInfo0),
			TypeSpecInfo0 = type_spec_info(ProcsToSpec0,
				ForceVersions0, SpecMap0, PragmaMap0),
			list__map((pred(ProcId::in, PredProcId::out) is det :-
					PredProcId = proc(PredId, ProcId)
				), ProcIds, PredProcIds),
			set__insert_list(ProcsToSpec0, PredProcIds,
				ProcsToSpec),
			set__insert(ForceVersions0, NewPredId, ForceVersions),

			( Status = opt_imported ->
				% For imported predicates dead_proc_elim.m
				% needs to know that if the original predicate
				% is used, the predicate to force the
				% production of the specialised interface is
				% also used.
				multi_map__set(SpecMap0, PredId, NewPredId,
					SpecMap)
			;
				SpecMap = SpecMap0
			),
			Pragma = type_spec(SymName, SpecName, Arity,
				yes(PredOrFunc), MaybeModes,
				map__to_assoc_list(RenamedSubst), TVarSet,
				ExpandedItems),
			multi_map__set(PragmaMap0, PredId, Pragma, PragmaMap),
			TypeSpecInfo = type_spec_info(ProcsToSpec,
				ForceVersions, SpecMap, PragmaMap),
			module_info_set_type_spec_info(TypeSpecInfo,
				ModuleInfo3, ModuleInfo),

			TransformInfo1 = transform_info(ModuleInfo, Info0),
			( status_is_imported(Status, yes) ->
				ItemType =
					pred_or_func_to_item_type(PredOrFunc),
				apply_to_recompilation_info(
				recompilation__record_expanded_items(
					item_id(ItemType, SymName - Arity),
					ExpandedItems),
				TransformInfo1, TransformInfo)
			;
				TransformInfo = TransformInfo1
			)
		;
			TransformInfo = transform_info(ModuleInfo2, Info0)
		)
	;
		TransformInfo = transform_info(ModuleInfo1, Info0)
	).

	% Check that the type substitution for a `:- pragma type_spec'
	% declaration is valid.
	% A type substitution is invalid if:
	%	- it substitutes unknown type variables
	% 	- it substitutes existentially quantified type variables
	% Type substitutions are also invalid if the replacement types are
	% not ground, however this is a (hopefully temporary) limitation
	% of the current implementation, so it only results in a warning.
:- pred handle_pragma_type_spec_subst(prog_context::in,
	assoc_list(tvar, type)::in, pred_info::in, tvarset::in, tvarset::out,
	list(type)::out, existq_tvars::out, class_constraints::out,
	maybe(tsubst)::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

handle_pragma_type_spec_subst(Context, Subst, PredInfo0, TVarSet0, TVarSet,
		Types, ExistQVars, ClassContext, SubstOk, !ModuleInfo, !IO) :-
	assoc_list__keys(Subst, VarsToSub),
	(
		Subst = []
	->
		error("handle_pragma_type_spec_subst: empty substitution")
	;
		find_duplicate_list_elements(VarsToSub, MultiSubstVars0),
		MultiSubstVars0 \= []
	->
		list__sort_and_remove_dups(MultiSubstVars0, MultiSubstVars),
		report_multiple_subst_vars(PredInfo0, Context,
			TVarSet0, MultiSubstVars, !IO),
		module_info_incr_errors(!ModuleInfo),
		io__set_exit_status(1, !IO),
		ExistQVars = [],
		Types = [],
		ClassContext = constraints([], []),
		varset__init(TVarSet),
		SubstOk = no
	;
		pred_info_typevarset(PredInfo0, CalledTVarSet),
		varset__create_name_var_map(CalledTVarSet, NameVarIndex0),
		list__filter((pred(Var::in) is semidet :-
			varset__lookup_name(TVarSet0, Var, VarName),
			\+ map__contains(NameVarIndex0, VarName)
		), VarsToSub, UnknownVarsToSub),
		( UnknownVarsToSub = [] ->
			% Check that the substitution is not recursive.
			set__list_to_set(VarsToSub, VarsToSubSet),

			assoc_list__values(Subst, SubstTypes0),
			term__vars_list(SubstTypes0, TVarsInSubstTypes0),
			set__list_to_set(TVarsInSubstTypes0,
				TVarsInSubstTypes),

			set__intersect(TVarsInSubstTypes, VarsToSubSet,
				RecSubstTVars0),
			set__to_sorted_list(RecSubstTVars0, RecSubstTVars),

			( RecSubstTVars = [] ->
				map__init(TVarRenaming0),
				list__append(VarsToSub, TVarsInSubstTypes0,
					VarsToReplace),

				get_new_tvars(VarsToReplace, TVarSet0,
					CalledTVarSet, TVarSet, NameVarIndex0,
					_, TVarRenaming0, TVarRenaming),

				% Check that none of the existentially
				% quantified variables were substituted.
				map__apply_to_list(VarsToSub, TVarRenaming,
					RenamedVarsToSub),
				pred_info_get_exist_quant_tvars(PredInfo0,
					ExistQVars),
				list__filter((pred(RenamedVar::in) is semidet
						:-
					list__member(RenamedVar, ExistQVars)
				), RenamedVarsToSub, SubExistQVars),
				( SubExistQVars = [] ->
					map__init(TypeSubst0),
					term__apply_variable_renaming_to_list(
						SubstTypes0, TVarRenaming,
						SubstTypes),
					assoc_list__from_corresponding_lists(
						RenamedVarsToSub, SubstTypes,
						SubAL),
					list__foldl((pred((TVar - Type)::in,
							TSubst0::in,
							TSubst::out) is det :-
						map__set(TSubst0, TVar, Type,
							TSubst)
					), SubAL, TypeSubst0, TypeSubst),

					% Apply the substitution.
					pred_info_arg_types(PredInfo0, Types0),
					pred_info_get_class_context(PredInfo0,
						ClassContext0),
					term__apply_rec_substitution_to_list(
						Types0, TypeSubst, Types),
					apply_rec_subst_to_constraints(
						TypeSubst, ClassContext0,
						ClassContext),
					SubstOk = yes(TypeSubst)
				;
					report_subst_existq_tvars(PredInfo0,
						Context, SubExistQVars, !IO),
					io__set_exit_status(1, !IO),
					module_info_incr_errors(!ModuleInfo),
					Types = [],
					ClassContext = constraints([], []),
					SubstOk = no
				)
			;
				report_recursive_subst(PredInfo0, Context,
					TVarSet0, RecSubstTVars, !IO),
				io__set_exit_status(1, !IO),
				module_info_incr_errors(!ModuleInfo),
				ExistQVars = [],
				Types = [],
				ClassContext = constraints([], []),
				varset__init(TVarSet),
				SubstOk = no
			)
		;
			report_unknown_vars_to_subst(PredInfo0, Context,
				TVarSet0, UnknownVarsToSub, !IO),
			module_info_incr_errors(!ModuleInfo),
			io__set_exit_status(1, !IO),
			ExistQVars = [],
			Types = [],
			ClassContext = constraints([], []),
			varset__init(TVarSet),
			SubstOk = no
		)
	).

:- pred find_duplicate_list_elements(list(T)::in, list(T)::out) is det.

find_duplicate_list_elements([], []).
find_duplicate_list_elements([H | T], Vars) :-
	find_duplicate_list_elements(T, Vars0),
	( list__member(H, T) ->
		Vars = [H | Vars0]
	;
		Vars = Vars0
	).

:- pred report_subst_existq_tvars(pred_info::in, prog_context::in,
	list(tvar)::in, io::di, io::uo) is det.

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

:- pred report_recursive_subst(pred_info::in, prog_context::in, tvarset::in,
	list(tvar)::in, io::di, io::uo) is det.

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

:- pred report_multiple_subst_vars(pred_info::in, prog_context::in,
	tvarset::in, list(tvar)::in, io::di, io::uo) is det.

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

:- pred report_unknown_vars_to_subst(pred_info::in, prog_context::in,
	tvarset::in, list(tvar)::in, io::di, io::uo) is det.

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
	{ PredOrFunc = pred_info_is_pred_or_func(PredInfo0) },
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

:- pred report_pragma_type_spec(pred_info::in, term__context::in,
	io::di, io::uo) is det.

report_pragma_type_spec(PredInfo0, Context) -->
	{ Module = pred_info_module(PredInfo0) },
	{ Name = pred_info_name(PredInfo0) },
	{ Arity = pred_info_arity(PredInfo0) },
	{ PredOrFunc = pred_info_is_pred_or_func(PredInfo0) },
	prog_out__write_context(Context),
	io__write_string("In `:- pragma type_spec' declaration for "),
	hlds_out__write_simple_call_id(PredOrFunc,
		qualified(Module, Name)/Arity),
	io__write_string(":\n").

:- pred report_variables(list(tvar)::in, tvarset::in, io::di, io::uo) is det.

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
:- pred handle_pragma_type_spec_modes(sym_name::in, arity::in,
	prog_context::in, maybe(list(mode))::in, list(proc_id)::out,
	proc_table::in, proc_table::out, bool::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

handle_pragma_type_spec_modes(SymName, Arity, Context, MaybeModes, ProcIds,
		!Procs, ModesOk, !ModuleInfo, !IO) :-
	( MaybeModes = yes(Modes) ->
		map__to_assoc_list(!.Procs, ExistingProcs),
		(
			get_procedure_matching_argmodes(ExistingProcs,
				Modes, !.ModuleInfo, ProcId)
		->
			map__lookup(!.Procs, ProcId, ProcInfo),
			map__det_insert(map__init, ProcId, ProcInfo, !:Procs),
			ProcIds = [ProcId],
			ModesOk = yes
		;
			ProcIds = [],
			module_info_incr_errors(!ModuleInfo),
			undefined_mode_error(SymName, Arity, Context,
				"`:- pragma type_spec' declaration", !IO),
			ModesOk = no
		)
	;
		map__keys(!.Procs, ProcIds),
		ModesOk = yes
	).

%-----------------------------------------------------------------------------%

:- pred add_pragma_termination_info(pred_or_func::in, sym_name::in,
	list(mode)::in, maybe(pragma_arg_size_info)::in,
	maybe(pragma_termination_info)::in,
	prog_context::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

add_pragma_termination_info(PredOrFunc, SymName, ModeList,
		MaybePragmaArgSizeInfo, MaybePragmaTerminationInfo,
		Context, !Module, !IO) :-
	module_info_get_predicate_table(!.Module, Preds),
	list__length(ModeList, Arity),
	(
		predicate_table_search_pf_sym_arity(Preds, is_fully_qualified,
			PredOrFunc, SymName, Arity, PredIds),
		PredIds \= []
	->
		( PredIds = [PredId] ->
			module_info_preds(!.Module, PredTable0),
			map__lookup(PredTable0, PredId, PredInfo0),
			pred_info_procedures(PredInfo0, ProcTable0),
			map__to_assoc_list(ProcTable0, ProcList),
			(
				get_procedure_matching_declmodes(ProcList,
					ModeList, !.Module, ProcId)
			->
				add_context_to_arg_size_info(
					MaybePragmaArgSizeInfo,
					Context, MaybeArgSizeInfo),
				add_context_to_termination_info(
					MaybePragmaTerminationInfo, Context,
					MaybeTerminationInfo),
				map__lookup(ProcTable0, ProcId, ProcInfo0),
				proc_info_set_maybe_arg_size_info(
					MaybeArgSizeInfo,
					ProcInfo0, ProcInfo1),
				proc_info_set_maybe_termination_info(
					MaybeTerminationInfo,
					ProcInfo1, ProcInfo),
				map__det_update(ProcTable0, ProcId, ProcInfo,
					ProcTable),
				pred_info_set_procedures(ProcTable,
					PredInfo0, PredInfo),
				map__det_update(PredTable0, PredId, PredInfo,
					PredTable),
				module_info_set_preds(PredTable, !Module)
			;
				module_info_incr_errors(!Module),
				prog_out__write_context(Context, !IO),
				io__write_string("Error: `:- pragma " ++
					"termination_info' ", !IO),
				io__write_string("declaration for " ++
					"undeclared mode of ", !IO),
				hlds_out__write_simple_call_id(PredOrFunc,
					SymName/Arity, !IO),
				io__write_string(".\n", !IO)
			)
		;
			prog_out__write_context(Context, !IO),
			io__write_string("Error: ambiguous predicate name ",
				!IO),
			hlds_out__write_simple_call_id(PredOrFunc,
				SymName/Arity, !IO),
			io__nl(!IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  in `pragma termination_info'.\n",
				!IO),
			module_info_incr_errors(!Module)
		)
	;
		% XXX This happens in `.trans_opt' files sometimes --
		% so just ignore it
		true
		%	undefined_pred_or_func_error(SymName, Arity, Context,
		% 		"`:- pragma termination_info' declaration",
		%		!IO),
		%	module_info_incr_errors(!Module)
	).

%-----------------------------------------------------------------------------%

:- pred add_stratified_pred(string::in, sym_name::in, arity::in,
	term__context::in, module_info::in, module_info::out, io::di, io::uo)
	is det.

add_stratified_pred(PragmaName, Name, Arity, Context, !Module, !IO) :-
	module_info_get_predicate_table(!.Module, PredTable0),
	(
		predicate_table_search_sym_arity(PredTable0,
			is_fully_qualified, Name, Arity, PredIds)
	->
		module_info_stratified_preds(!.Module, StratPredIds0),
		set__insert_list(StratPredIds0, PredIds, StratPredIds),
		module_info_set_stratified_preds(StratPredIds, !Module)
	;
		string__append_list(
			["`:- pragma ", PragmaName, "' declaration"],
			Description),
		undefined_pred_or_func_error(Name, Arity, Context,
			Description, !IO),
		module_info_incr_errors(!Module)
	).

%-----------------------------------------------------------------------------%

	% add_pred_marker(ModuleInfo0, PragmaName, Name, Arity, Status,
	%	Context, Marker, ConflictMarkers, ModuleInfo, IO0, IO)
	%
	% Adds Marker to the marker list of the pred(s) with give Name and
	% Arity, updating the ModuleInfo. If the named pred does not exist,
	% or the pred already has a marker in ConflictMarkers, report
	% an error.
:- pred add_pred_marker(string::in, sym_name::in, arity::in, import_status::in,
	prog_context::in, marker::in, list(marker)::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

add_pred_marker(PragmaName, Name, Arity, Status, Context, Marker,
		ConflictMarkers, !Module, !IO) :-
	( marker_must_be_exported(Marker) ->
		MustBeExported = yes
	;
		MustBeExported = no
	),
	do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported,
		Context, add_marker_pred_info(Marker), !Module, PredIds, !IO),
	module_info_preds(!.Module, Preds),
	pragma_check_markers(Preds, PredIds, ConflictMarkers, Conflict),
	( Conflict = yes ->
		pragma_conflict_error(Name, Arity, Context, PragmaName, !IO),
		module_info_incr_errors(!Module)
	;
		true
	).

:- pred set_pred_owner(sym_name::in, arity::in, string::in, import_status::in,
	prog_context::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

set_pred_owner(Name, Arity, Owner, Status, Context, !Module, !IO) :-
	SetOwner = (pred(PredInfo0::in, PredInfo::out) is det :-
			pred_info_set_aditi_owner(Owner, PredInfo0, PredInfo)
	),
	MarkerMustBeExported = yes,
	do_add_pred_marker("owner", Name, Arity, Status,
		MarkerMustBeExported, Context, SetOwner, !Module, _, !IO).

:- pred add_base_relation_index(sym_name::in, arity::in, index_spec::in,
	import_status::in, prog_context::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

add_base_relation_index(Name, Arity, Index, Status, Context, !Module, !IO) :-
	AddIndex = (pred(PredInfo0::in, PredInfo::out) is det :-
			pred_info_get_indexes(PredInfo0, Indexes0),
			Indexes = [Index | Indexes0],
			pred_info_set_indexes(Indexes, PredInfo0, PredInfo)
	),
	MarkerMustBeExported = yes,
	do_add_pred_marker("aditi_index", Name, Arity, Status,
		MarkerMustBeExported, Context, AddIndex, !Module,
		PredIds, !IO),
	Index = index_spec(_, Attrs),
	list__foldl(check_index_attribute(Name, Arity, Context), Attrs, !IO),
	list__foldl(check_index_attribute_pred(!.Module, Name, Arity, Context,
		Attrs), PredIds, !IO).

	% Check that the index attributes are legal for the predicate's arity.
:- pred check_index_attribute(sym_name::in, arity::in, term__context::in,
	int::in, io::di, io::uo) is det.

check_index_attribute(Name, Arity, Context, Attr, !IO) :-
	( ( Attr > 0, Attr =< Arity ) ->
		true
	;
		prog_out__write_context(Context, !IO),
		io__write_string(
			"In `:- pragma aditi_index' declaration for `", !IO),
		prog_out__write_sym_name_and_arity(Name/Arity, !IO),
		io__write_string("':\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  attribute ", !IO),
		io__write_int(Attr, !IO),
		io__write_string(" is out of range.\n", !IO),
		io__set_exit_status(1, !IO)
	).

	% Check that a relation with an index specified is a base relation
	% and that the indexed attributes do not include aditi__states.
:- pred check_index_attribute_pred(module_info::in, sym_name::in, arity::in,
	term__context::in, list(int)::in, pred_id::in, io::di, io::uo) is det.

check_index_attribute_pred(ModuleInfo, Name, Arity, Context, Attrs, PredId,
		!IO) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_markers(PredInfo, Markers),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	( check_marker(Markers, base_relation) ->
		true
	;
		prog_out__write_context(Context, !IO),
		io__write_string(
			"Error: `:- pragma aditi_index' declaration", !IO),
		io__nl(!IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  for ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, Name/Arity, !IO),
		io__write_string(" without preceding\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string(
			"  `:- pragma base_relation' declaration.\n", !IO),
		io__set_exit_status(1, !IO)
	),

	pred_info_arg_types(PredInfo, ArgTypes),
	AttrIsAditiState = (pred(Attr::in) is semidet :-
		list__index0(ArgTypes, Attr, ArgType),
		type_is_aditi_state(ArgType)
	),
	list__filter(AttrIsAditiState, Attrs, AditiStateAttrs),

	( AditiStateAttrs = [AditiStateAttr | _] ->
		% Indexing on aditi__state attributes is pretty silly,
		% since they're removed by magic.m.
		prog_out__write_context(Context, !IO),
		io__write_string(
			"In `:- pragma aditi_index' declaration for ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, Name/Arity, !IO),
		io__write_string(":\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  attribute ", !IO),
		io__write_int(AditiStateAttr, !IO),
		io__write_string(" is an aditi__state.\n", !IO),
		io__set_exit_status(1, !IO)
	;
		true
	).

:- type add_marker_pred_info == pred(pred_info, pred_info).
:- inst add_marker_pred_info == (pred(in, out) is det).

:- pred do_add_pred_marker(string::in, sym_name::in, arity::in,
	import_status::in, bool::in, term__context::in,
	add_marker_pred_info::in(add_marker_pred_info),
	module_info::in, module_info::out, list(pred_id)::out,
	io::di, io::uo) is det.

do_add_pred_marker(PragmaName, Name, Arity, Status, MustBeExported, Context,
		UpdatePredInfo, !Module, PredIds, !IO) :-
	( get_matching_pred_ids(!.Module, Name, Arity, PredIds0) ->
		PredIds = PredIds0,
		module_info_get_predicate_table(!.Module, PredTable0),
		predicate_table_get_preds(PredTable0, Preds0),

		pragma_add_marker(PredIds, UpdatePredInfo, Status,
			MustBeExported, Preds0, Preds, WrongStatus),
		(
			WrongStatus = yes
		->
			pragma_status_error(Name, Arity, Context, PragmaName,
				!IO),
			module_info_incr_errors(!Module)
		;
			true
		),

		predicate_table_set_preds(Preds, PredTable0, PredTable),
		module_info_set_predicate_table(PredTable, !Module)
	;
		PredIds = [],
		string__append_list(
			["`:- pragma ", PragmaName, "' declaration"],
			Description),
		undefined_pred_or_func_error(Name, Arity, Context,
			Description, !IO),
		module_info_incr_errors(!Module)
	).

:- pred get_matching_pred_ids(module_info::in, sym_name::in, arity::in,
	list(pred_id)::out) is semidet.

get_matching_pred_ids(Module0, Name, Arity, PredIds) :-
	module_info_get_predicate_table(Module0, PredTable0),
	% check that the pragma is module qualified.
	(
		Name = unqualified(_)
	->
		error("get_matching_pred_ids: unqualified name")
	;
		predicate_table_search_sym_arity(PredTable0,
			is_fully_qualified, Name, Arity, PredIds)
	).

%-----------------------------------------------------------------------------%

:- pred module_mark_as_external(sym_name::in, int::in, prog_context::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_mark_as_external(PredName, Arity, Context, !Module, !IO) :-
	% `external' declarations can only apply to things defined
	% in this module, since everything else is already external.
	module_info_get_predicate_table(!.Module, PredicateTable0),
	(
		predicate_table_search_sym_arity(PredicateTable0,
			is_fully_qualified, PredName, Arity, PredIdList)
	->
		module_mark_preds_as_external(PredIdList, !Module)
	;
		undefined_pred_or_func_error(PredName, Arity,
			Context, "`:- external' declaration", !IO),
		module_info_incr_errors(!Module)
	).

:- pred module_mark_preds_as_external(list(pred_id)::in,
	module_info::in, module_info::out) is det.

module_mark_preds_as_external([], !Module).
module_mark_preds_as_external([PredId | PredIds], !Module) :-
	module_info_preds(!.Module, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_mark_as_external(PredInfo0, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Preds, !Module),
	module_mark_preds_as_external(PredIds, !Module).

%-----------------------------------------------------------------------------%

:- pred module_add_inst_defn(inst_varset::in, sym_name::in, list(inst_var)::in,
	inst_defn::in, condition::in, prog_context::in, item_status::in,
	module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

module_add_inst_defn(VarSet, Name, Args, InstDefn, Cond, Context,
		item_status(Status, _NeedQual), !Module, InvalidMode, !IO) :-
	%
	% add the definition of this inst to the HLDS inst table
	%
	module_info_insts(!.Module, InstTable0),
	inst_table_get_user_insts(InstTable0, Insts0),
	insts_add(VarSet, Name, Args, InstDefn, Cond, Context, Status,
		Insts0, Insts, !IO),
	inst_table_set_user_insts(Insts, InstTable0, InstTable),
	module_info_set_insts(InstTable, !Module),
	%
	% check if the inst is infinitely recursive (at the top level)
	%
	Arity = list__length(Args),
	InstId = Name - Arity,
	TestArgs = list__duplicate(Arity, not_reached),
	check_for_cyclic_inst(Insts, InstId, InstId, TestArgs, [], Context,
		InvalidMode, !IO).

:- pred insts_add(inst_varset::in, sym_name::in,
	list(inst_var)::in, inst_defn::in, condition::in, prog_context::in,
	import_status::in, user_inst_table::in, user_inst_table::out,
	io::di, io::uo) is det.

	% XXX handle abstract insts
insts_add(_, _, _, abstract_inst, _, _, _, !Insts, !IO) :-
	error("sorry, abstract insts not implemented").
insts_add(VarSet, Name, Args, eqv_inst(Body), _Cond, Context, Status, !Insts,
		!IO) :-
	list__length(Args, Arity),
	(
		I = hlds_inst_defn(VarSet, Args, eqv_inst(Body),
			Context, Status),
		user_inst_table_insert(Name - Arity, I, !Insts)
	->
		true
	;
		% If abstract insts are implemented, this will need to change
		% to update the hlds_inst_defn to the non-abstract inst.

		% XXX we should record each error using
		%	 module_info_incr_errors
		user_inst_table_get_inst_defns(!.Insts, InstDefns),
		map__lookup(InstDefns, Name - Arity, OrigI),
		OrigI = hlds_inst_defn(_, _, _, OrigContext, _),
		multiple_def_error(Status, Name, Arity, "inst",
			Context, OrigContext, _, !IO)
	).

	%
	% check if the inst is infinitely recursive (at the top level)
	%
:- pred check_for_cyclic_inst(user_inst_table::in, inst_id::in, inst_id::in,
	list(inst)::in, list(inst_id)::in, prog_context::in, bool::out,
	io::di, io::uo) is det.

check_for_cyclic_inst(UserInstTable, OrigInstId, InstId0, Args0, Expansions0,
		Context, InvalidMode, !IO) :-
	( list__member(InstId0, Expansions0) ->
		report_circular_equiv_error("inst", OrigInstId, InstId0,
			Expansions0, Context, !IO),
		InvalidMode = yes
	;
		user_inst_table_get_inst_defns(UserInstTable, InstDefns),
		(
			map__search(InstDefns, InstId0, InstDefn),
			InstDefn = hlds_inst_defn(_, Params, Body, _, _),
			Body = eqv_inst(EqvInst0),
			inst_substitute_arg_list(EqvInst0, Params, Args0,
				EqvInst),
			EqvInst = defined_inst(user_inst(Name, Args))
		->
			Arity = list__length(Args),
			InstId = Name - Arity,
			Expansions = [InstId0 | Expansions0],
			check_for_cyclic_inst(UserInstTable, OrigInstId,
				InstId, Args, Expansions, Context, InvalidMode,
				!IO)
		;
			InvalidMode = no
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_mode_defn(inst_varset::in, sym_name::in, list(inst_var)::in,
	mode_defn::in, condition::in, prog_context::in, item_status::in,
	module_info::in, module_info::out, bool::out, io::di, io::uo) is det.

module_add_mode_defn(VarSet, Name, Params, ModeDefn, Cond,
		Context, item_status(Status, _NeedQual),
		!Module, InvalidMode, !IO) :-
	module_info_modes(!.Module, Modes0),
	modes_add(VarSet, Name, Params, ModeDefn,
		Cond, Context, Status, Modes0, Modes, InvalidMode, !IO),
	module_info_set_modes(Modes, !Module).

:- pred modes_add(inst_varset::in, sym_name::in, list(inst_var)::in,
	mode_defn::in, condition::in, prog_context::in, import_status::in,
	mode_table::in, mode_table::out, bool::out, io::di, io::uo) is det.

modes_add(VarSet, Name, Args, eqv_mode(Body), _Cond, Context, Status,
		!Modes, InvalidMode, !IO) :-
	list__length(Args, Arity),
	ModeId = Name - Arity,
	(
		I = hlds_mode_defn(VarSet, Args, eqv_mode(Body),
			Context, Status),
		mode_table_insert(ModeId, I, !Modes)
	->
		true
	;
		mode_table_get_mode_defns(!.Modes, ModeDefns),
		map__lookup(ModeDefns, ModeId, OrigI),
		OrigI = hlds_mode_defn(_, _, _, OrigContext, _),
		% XXX we should record each error using
		% 	module_info_incr_errors
		multiple_def_error(Status, Name, Arity, "mode",
			Context, OrigContext, _, !IO)
	),
	check_for_cyclic_mode(!.Modes, ModeId, ModeId, [], Context,
		InvalidMode, !IO).

	%
	% check if the mode is infinitely recursive at the top level
	%
:- pred check_for_cyclic_mode(mode_table::in, mode_id::in, mode_id::in,
	list(mode_id)::in, prog_context::in, bool::out, io::di, io::uo) is det.

check_for_cyclic_mode(ModeTable, OrigModeId, ModeId0, Expansions0, Context,
		InvalidMode, !IO) :-
	( list__member(ModeId0, Expansions0) ->
		report_circular_equiv_error("mode", OrigModeId, ModeId0,
			Expansions0, Context, !IO),
		InvalidMode = yes
	;
		mode_table_get_mode_defns(ModeTable, ModeDefns),
		(
			map__search(ModeDefns, ModeId0, ModeDefn),
			ModeDefn = hlds_mode_defn(_, _, Body, _, _),
			Body = eqv_mode(EqvMode),
			EqvMode = user_defined_mode(Name, Args)
		->
			Arity = list__length(Args),
			ModeId = Name - Arity,
			Expansions = [ModeId0 | Expansions0],
			check_for_cyclic_mode(ModeTable, OrigModeId, ModeId,
				Expansions, Context, InvalidMode, !IO)
		;
			InvalidMode = no
		)
	).

:- type id == pair(sym_name, arity).

:- pred report_circular_equiv_error(string::in, id::in, id::in, list(id)::in,
	prog_context::in, io::di, io::uo) is det.

report_circular_equiv_error(Kind, OrigId, Id, Expansions, Context, !IO) :-
	( Id = OrigId ->
		%
		% Report an error message of the form
		%	Error: circular equivalence <kind> foo/0.
		% or
		%	Error: circular equivalence <kind>s foo/0 and bar/1.
		% or
		%	Error: circular equivalence <kind>s foo/0, bar/1,
		%	and baz/2.
		% where <kind> is either "inst" or "mode".
		%
		Kinds = (if Expansions = [_] then Kind else Kind ++ "s"),
		Pieces0 = list__map(
			(func(SymName - Arity) =
				error_util__describe_sym_name_and_arity(
					SymName / Arity)),
			Expansions),
		error_util__list_to_pieces(Pieces0, Pieces1),
		Pieces = append_punctuation(
			[words("Error: circular equivalence"),
				fixed(Kinds) | Pieces1], '.'),
		error_util__write_error_pieces(Context, 0, Pieces, !IO),
		io__set_exit_status(1, !IO)
	;
		% We have an inst `OrigId' which is not itself circular,
		% but which is defined in terms of `Id' which is circular.
		% Don't bother reporting it now -- it have already been
		% reported when we processed the definition of Id.
		true
	).

%-----------------------------------------------------------------------------%

	% We allow more than one "definition" for a given type so
	% long all of them except one are actually just declarations,
	% e.g. `:- type t.', which is parsed as an type definition for
	% t which defines t as an abstract_type.

:- pred module_add_type_defn(tvarset::in, sym_name::in, list(type_param)::in,
	type_defn::in, condition::in, prog_context::in, item_status::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_add_type_defn(TVarSet, Name, Args, TypeDefn, _Cond, Context,
		item_status(Status0, NeedQual), !Module, !IO) :-
	globals__io_get_globals(Globals, !IO),
	list__length(Args, Arity),
	TypeCtor = Name - Arity,
	convert_type_defn(TypeDefn, TypeCtor, Globals, Body0),
	module_info_types(!.Module, Types0),
	(
		(
			Body0 = abstract_type(_)
		;
			Body0 = du_type(_, _, _, _, _, _),
			string__suffix(term__context_file(Context), ".int2")
			% If the type definition comes from a .int2 file then
			% we need to treat it as abstract.  The constructors
			% may only be used by the mode system for comparing
			% `bound' insts to `ground'.
		)
	->
		make_status_abstract(Status0, Status1)
	;
		Status1 = Status0
	),
	(
		% the type is exported if *any* occurrence is exported,
		% even a previous abstract occurrence
		map__search(Types0, TypeCtor, OldDefn0)
	->
		hlds_data__get_type_defn_status(OldDefn0, OldStatus),
		combine_status(Status1, OldStatus, Status),
		hlds_data__get_type_defn_body(OldDefn0, OldBody0),
		combine_is_solver_type(OldBody0, OldBody, Body0, Body),
		( is_solver_type_is_inconsistent(OldBody, Body) ->
			% The existing definition has an is_solver_type
			% annotation which is different to the current
			% definition.
			module_info_incr_errors(!Module),
			Pieces0 = [words("In definition of type"),
				fixed(describe_sym_name_and_arity(
					Name / Arity) ++ ":"), nl,
				words("error: all definitions of a type must"),
				words("have consistent `solver'"),
				words("annotations")],
			error_util__write_error_pieces(Context, 0, Pieces0,
				!IO),
			MaybeOldDefn = no
		;
			hlds_data__set_type_defn_body(OldBody,
				OldDefn0, OldDefn),
			MaybeOldDefn = yes(OldDefn)
		)
	;
		MaybeOldDefn = no,
		Status = Status1,
		Body = Body0
	),
	hlds_data__set_type_defn(TVarSet, Args, Body, Status, no,
		NeedQual, Context, T),
	(
		MaybeOldDefn = no,
		Body = foreign_type(_)
	->
		TypeStr = error_util__describe_sym_name_and_arity(
				Name / Arity),
		ErrorPieces = [
			words("Error: type "),
			fixed(TypeStr),
			words("defined as foreign_type without being declared.")
		],
		error_util__write_error_pieces(Context, 0, ErrorPieces, !IO),
		module_info_incr_errors(!Module)
	;
		MaybeOldDefn = yes(OldDefn1),
		Body = foreign_type(_),
		hlds_data__get_type_defn_status(OldDefn1, OldStatus1),
		hlds_data__get_type_defn_body(OldDefn1, OldBody1),
		OldBody1 = abstract_type(_),
		status_is_exported_to_non_submodules(OldStatus1, no),
		status_is_exported_to_non_submodules(Status0, yes)
	->
		TypeStr = error_util__describe_sym_name_and_arity(
				Name / Arity),
		ErrorPieces = [
			words("Error: pragma foreign_type "),
			fixed(TypeStr),
			words(
		"must have the same visibility as the type declaration.")
		],
		error_util__write_error_pieces(Context, 0, ErrorPieces, !IO),
		module_info_incr_errors(!Module)
	;

		% if there was an existing non-abstract definition for the type
		MaybeOldDefn = yes(T2),
		hlds_data__get_type_defn_tvarset(T2, TVarSet_2),
		hlds_data__get_type_defn_tparams(T2, Params_2),
		hlds_data__get_type_defn_body(T2, Body_2),
		hlds_data__get_type_defn_context(T2, OrigContext),
		hlds_data__get_type_defn_status(T2, OrigStatus),
		hlds_data__get_type_defn_in_exported_eqv(T2,
			OrigInExportedEqv),
		hlds_data__get_type_defn_need_qualifier(T2, OrigNeedQual),
		Body_2 \= abstract_type(_)
	->
		globals__io_get_target(Target, !IO),
		globals__io_lookup_bool_option(make_optimization_interface,
			MakeOptInt, !IO),
		( Body = foreign_type(_) ->
			module_info_contains_foreign_type(!Module)
		;
			true
		),
	  	(
			% then if this definition was abstract, ignore it
			% (but update the status of the old defn if necessary)
			Body = abstract_type(_)
		->
			( Status = OrigStatus ->
				true
			;
				hlds_data__set_type_defn(TVarSet_2, Params_2,
					Body_2, Status, OrigInExportedEqv,
					OrigNeedQual, OrigContext, T3),
				map__det_update(Types0, TypeCtor, T3, Types),
				module_info_set_types(Types, !Module)
			)
		;
			merge_foreign_type_bodies(Target, MakeOptInt,
				Body, Body_2, NewBody)
		->
			(
				check_foreign_type_visibility(OrigStatus,
					Status1)
			->
				hlds_data__set_type_defn(TVarSet_2, Params_2,
					NewBody, Status, OrigInExportedEqv,
					NeedQual, Context, T3),
				map__det_update(Types0, TypeCtor, T3, Types),
				module_info_set_types(Types, !Module)
			;
				module_info_incr_errors(!Module),
				Pieces = [words("In definition of type"),
					fixed(describe_sym_name_and_arity(
						Name / Arity) ++ ":"), nl,
					words("error: all definitions of a"),
					words("type must have the same"),
					words("visibility")],
				error_util__write_error_pieces(Context, 0,
					Pieces, !IO)
			)
		;
			% otherwise issue an error message if the second
			% definition wasn't read while reading .opt files.
			Status = opt_imported
		->
			true
		;
			module_info_incr_errors(!Module),
			multiple_def_error(Status, Name, Arity, "type",
				Context, OrigContext, _, !IO)
		)
	;
		map__set(Types0, TypeCtor, T, Types),
		module_info_set_types(Types, !Module),
		(
			% XXX we can't handle abstract exported
			% polymorphic equivalence types with monomorphic
			% bodies, because the compiler stuffs up the
			% type_info handling -- the caller passes type_infos,
			% but the callee expects no type_infos
			Body = eqv_type(EqvType),
			Status = abstract_exported,
			term__contains_var_list(Args, Var),
			\+ term__contains_var(EqvType, Var)
		->
			prog_out__write_context(Context, !IO),
			io__write_string("Sorry, not implemented: " ++
				"polymorphic equivalence type,\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  with monomorphic definition, " ++
				"exported as abstract type.\n", !IO),
			globals__io_lookup_bool_option(verbose_errors,
				VerboseErrors, !IO),
			( VerboseErrors = yes ->
				io__write_strings([
	"\tA quick work-around is just export the type as a concrete type,\n",
	"\tby putting the type definition in the interface section.\n",
	"\tA better work-around is to use a ""wrapper"" type, with just one\n",
	"\tfunctor that has just one arg, instead of an equivalence type.\n",
	"\t(There's no performance penalty for this -- the compiler will\n",
	"\toptimize the wrapper away.)\n"], !IO)
			;
				true
			),
			io__set_exit_status(1, !IO)
		;
			true
		)
	).

%-----------------------------------------------------------------------------%

	% We do not have syntax for adding `solver' annotations to
	% `:- pragma foreign_type' declarations, so foreign_type bodies
	% default to having an is_solver_type field of `non_solver_type'.
	% If another declaration for the type has a `solver' annotation then
	% we must update the foreign_type body to reflect this.
	%
	% rafe: XXX think it should be an error for foreign types to
	% be solver types.
	%
:- pred combine_is_solver_type(hlds_type_body::in, hlds_type_body::out,
		hlds_type_body::in, hlds_type_body::out) is det.
combine_is_solver_type(OldBody, OldBody, Body, Body).

	% Succeed iff the two type bodies have inconsistent is_solver_type
	% annotations.
:- pred is_solver_type_is_inconsistent(hlds_type_body::in, hlds_type_body::in)
	is semidet.

is_solver_type_is_inconsistent(OldBody, Body) :-
	maybe_get_body_is_solver_type(OldBody, OldIsSolverType),
	maybe_get_body_is_solver_type(Body, IsSolverType),
	OldIsSolverType \= IsSolverType.

:- pred maybe_get_body_is_solver_type(hlds_type_body::in, is_solver_type::out)
	is semidet.

maybe_get_body_is_solver_type(abstract_type(IsSolverType), IsSolverType).
maybe_get_body_is_solver_type(solver_type(_, _), solver_type).

	% check_foreign_type_visibility(OldStatus, NewDefnStatus).
	%
	% Check that the visibility of the new definition for
	% a foreign type matches that of previous definitions.
:- pred check_foreign_type_visibility(import_status::in,
	import_status::in) is semidet.

check_foreign_type_visibility(OldStatus, NewDefnStatus) :-
	( OldStatus = abstract_exported  ->
		% If OldStatus is abstract_exported, the previous
		% definitions were local.
		status_is_exported_to_non_submodules(NewDefnStatus, no)
	; OldStatus = exported ->
		NewDefnStatus = exported
	;
		status_is_exported_to_non_submodules(OldStatus, no),
		status_is_exported_to_non_submodules(NewDefnStatus, no)
	).

	% Add the constructors and special preds for a type to the HLDS.
:- pred process_type_defn(type_ctor::in, hlds_type_defn::in,
	bool::in, bool::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

process_type_defn(TypeCtor, TypeDefn, !FoundError, !Module, !IO) :-
	hlds_data__get_type_defn_context(TypeDefn, Context),
	hlds_data__get_type_defn_tvarset(TypeDefn, TVarSet),
	hlds_data__get_type_defn_tparams(TypeDefn, Args),
	hlds_data__get_type_defn_body(TypeDefn, Body),
	hlds_data__get_type_defn_status(TypeDefn, Status),
	hlds_data__get_type_defn_need_qualifier(TypeDefn, NeedQual),
	(
		ConsList = Body ^ du_type_ctors,
		ReservedTag = Body ^ du_type_reserved_tag,
		module_info_ctors(!.Module, Ctors0),
		module_info_get_partial_qualifier_info(!.Module, PQInfo),
		% ZZZ
		check_for_errors(
			(pred(M0::in, M::out, IO0::di, IO::uo) is det :-
				module_info_ctor_field_table(M0, CtorFields0),
				ctors_add(ConsList, TypeCtor, TVarSet,
					NeedQual, PQInfo, Context, Status,
					CtorFields0, CtorFields, Ctors0, Ctors,
					IO0, IO),
				module_info_set_ctors(Ctors, M0, M1),
				module_info_set_ctor_field_table(CtorFields,
					M1, M)
		), NewFoundError, !Module, !IO),

		globals__io_get_globals(Globals, !IO),
		(
			type_constructors_should_be_no_tag(ConsList,
				ReservedTag, Globals, Name, CtorArgType, _)
		->
			NoTagType = no_tag_type(Args, Name, CtorArgType),
			module_info_no_tag_types(!.Module, NoTagTypes0),
			map__set(NoTagTypes0, TypeCtor, NoTagType, NoTagTypes),
			module_info_set_no_tag_types(NoTagTypes, !Module)
		;
			true
		)
	;
		Body = abstract_type(_),
		NewFoundError = no
	;
		Body = solver_type(_, _),
		NewFoundError = no
	;
		Body = eqv_type(_),
		NewFoundError = no
	;
		Body = foreign_type(ForeignTypeBody),
		check_foreign_type(TypeCtor, ForeignTypeBody, Context,
			NewFoundError, !Module, !IO)
	),
	!:FoundError = !.FoundError `and` NewFoundError,
	(
		!.FoundError = yes
	->
		true
	;
		% Equivalence types are fully expanded on the IL and Java
		% backends, so the special predicates aren't required.
		are_equivalence_types_expanded(!.Module),
		Body = eqv_type(_)
	->
		true
	;
		construct_type(TypeCtor, Args, Type),
		add_special_preds(TVarSet, Type, TypeCtor, Body, Context,
			Status, !Module)
	).

	% check_foreign_type ensures that if we are generating code for
	% a specific backend that the foreign type has a representation
	% on that backend.
:- pred check_foreign_type(type_ctor::in, foreign_type_body::in,
	prog_context::in, bool::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

check_foreign_type(TypeCtor, ForeignTypeBody, Context, FoundError, !Module,
		!IO) :-
	TypeCtor = Name - Arity,
	module_info_globals(!.Module, Globals),
	generating_code(GeneratingCode, !IO),
	globals__get_target(Globals, Target),
	( have_foreign_type_for_backend(Target, ForeignTypeBody, yes) ->
		FoundError = no
	; GeneratingCode = yes ->
		%
		% If we're not generating code the error may only have
		% occurred because the grade options weren't passed.
		%
		io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
		( VeryVerbose = yes ->
			VerboseErrorPieces = [
				nl,
				words("There are representations for"),
				words("this type on other back-ends,"),
				words("but none for this back-end.")
			]
		;
			VerboseErrorPieces = []
		),
		( Target = c, LangStr = "C"
		; Target = il, LangStr = "IL"
		; Target = java, LangStr = "Java"
		; Target = asm, LangStr = "C"
		),
		TypeStr = error_util__describe_sym_name_and_arity(Name/Arity),
		ErrorPieces = [
			words("Error: no"), words(LangStr),
			words("`pragma foreign_type' declaration for"),
			fixed(TypeStr) | VerboseErrorPieces
		],
		error_util__write_error_pieces(Context, 0, ErrorPieces, !IO),
		FoundError = yes,
		module_info_incr_errors(!Module)
	;
		FoundError = yes
	).

	% Do the options imply that we will generate code for a specific
	% back-end?
:- pred generating_code(bool::out, io::di, io::uo) is det.

generating_code(bool__not(NotGeneratingCode)) -->
	io_lookup_bool_option(make_short_interface, MakeShortInterface),
	io_lookup_bool_option(make_interface, MakeInterface),
	io_lookup_bool_option(make_private_interface, MakePrivateInterface),
	io_lookup_bool_option(make_transitive_opt_interface,
			MakeTransOptInterface),
	io_lookup_bool_option(generate_source_file_mapping, GenSrcFileMapping),
	io_lookup_bool_option(generate_dependencies, GenDepends),
	io_lookup_bool_option(convert_to_mercury, ConvertToMercury),
	io_lookup_bool_option(typecheck_only, TypeCheckOnly),
	io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	io_lookup_bool_option(output_grade_string, OutputGradeString),
	{ bool__or_list([MakeShortInterface, MakeInterface,
		MakePrivateInterface, MakeTransOptInterface,
		GenSrcFileMapping, GenDepends, ConvertToMercury,
		TypeCheckOnly, ErrorCheckOnly, OutputGradeString],
		NotGeneratingCode) }.

:- pred merge_foreign_type_bodies(compilation_target::in, bool::in,
	hlds_type_body::in, hlds_type_body::in, hlds_type_body::out)
	is semidet.

	% Ignore Mercury definitions if we've got a foreign type
	% declaration suitable for this back-end and we aren't making the
	% optimization interface.  We need to keep the Mercury definition
	% if we are making the optimization interface so that it gets
	% output in the .opt file.
merge_foreign_type_bodies(Target, MakeOptInterface,
		foreign_type(ForeignTypeBody0), Body1, Body) :-
	MaybeForeignTypeBody1 = Body1 ^ du_type_is_foreign_type,
	(
		MaybeForeignTypeBody1 = yes(ForeignTypeBody1)
	;
		MaybeForeignTypeBody1 = no,
		ForeignTypeBody1 = foreign_type_body(no, no, no)
	),
	merge_foreign_type_bodies_2(ForeignTypeBody0, ForeignTypeBody1,
		ForeignTypeBody),
	(
		have_foreign_type_for_backend(Target, ForeignTypeBody, yes),
		MakeOptInterface = no
	->
		Body = foreign_type(ForeignTypeBody)
	;
		Body = Body1 ^ du_type_is_foreign_type := yes(ForeignTypeBody)
	).
merge_foreign_type_bodies(Target, MakeOptInterface,
		Body0 @ du_type(_, _, _, _, _, _),
		Body1 @ foreign_type(_), Body) :-
	merge_foreign_type_bodies(Target, MakeOptInterface, Body1, Body0, Body).
merge_foreign_type_bodies(_, _, foreign_type(Body0),
		foreign_type(Body1),
		foreign_type(Body)) :-
	merge_foreign_type_bodies_2(Body0, Body1, Body).

:- pred merge_foreign_type_bodies_2(foreign_type_body::in,
	foreign_type_body::in, foreign_type_body::out) is semidet.

merge_foreign_type_bodies_2(foreign_type_body(MaybeILA, MaybeCA, MaybeJavaA),
		foreign_type_body(MaybeILB, MaybeCB, MaybeJavaB),
		foreign_type_body(MaybeIL, MaybeC, MaybeJava)) :-
	merge_maybe(MaybeILA, MaybeILB, MaybeIL),
	merge_maybe(MaybeCA, MaybeCB, MaybeC),
	merge_maybe(MaybeJavaA, MaybeJavaB, MaybeJava).

:- pred merge_maybe(maybe(T)::in, maybe(T)::in, maybe(T)::out) is semidet.

merge_maybe(no, no, no).
merge_maybe(yes(T), no, yes(T)).
merge_maybe(no, yes(T), yes(T)).

:- pred make_status_abstract(import_status::in, import_status::out) is det.

make_status_abstract(Status, AbstractStatus) :-
	( Status = exported ->
		AbstractStatus = abstract_exported
	; Status = imported(_) ->
		AbstractStatus = abstract_imported
	;
		AbstractStatus = Status
	).

:- pred combine_status(import_status::in, import_status::in,
	import_status::out) is det.

combine_status(StatusA, StatusB, Status) :-
	( combine_status_2(StatusA, StatusB, CombinedStatus) ->
		Status = CombinedStatus
	;
		error("unexpected status for type definition")
	).

:- pred combine_status_2(import_status::in, import_status::in,
	import_status::out) is semidet.

combine_status_2(imported(_), Status2, Status) :-
	combine_status_imported(Status2, Status).
combine_status_2(local, Status2, Status) :-
	combine_status_local(Status2, Status).
combine_status_2(exported, _Status2, exported).
combine_status_2(exported_to_submodules, Status2, Status) :-
	combine_status_local(Status2, Status3),
	( Status3 = local ->
		Status = exported_to_submodules
	;
		Status = Status3
	).
combine_status_2(opt_imported, _Status2, opt_imported).
combine_status_2(abstract_imported, Status2, Status) :-
	combine_status_abstract_imported(Status2, Status).
combine_status_2(abstract_exported, Status2, Status) :-
	combine_status_abstract_exported(Status2, Status).

:- pred combine_status_imported(import_status::in, import_status::out)
	is semidet.

combine_status_imported(imported(Section),	imported(Section)).
combine_status_imported(local,			imported(implementation)).
combine_status_local(exported_to_submodules,
					exported_to_submodules).
combine_status_imported(exported,		exported).
combine_status_imported(opt_imported,		opt_imported).
combine_status_imported(abstract_imported,	imported(interface)).
combine_status_imported(abstract_exported,	abstract_exported).

:- pred combine_status_local(import_status::in, import_status::out) is semidet.

combine_status_local(imported(_),	local).
combine_status_local(local,		local).
combine_status_local(exported,		exported).
combine_status_local(opt_imported,	local).
combine_status_local(abstract_imported, local).
combine_status_local(abstract_exported, abstract_exported).

:- pred combine_status_abstract_exported(import_status::in, import_status::out)
	is det.

combine_status_abstract_exported(Status2, Status) :-
	( Status2 = exported ->
		Status = exported
	;
		Status = abstract_exported
	).

:- pred combine_status_abstract_imported(import_status::in, import_status::out)
	is det.

combine_status_abstract_imported(Status2, Status) :-
	( Status2 = imported(Section) ->
		Status = imported(Section)
	;
		Status = abstract_imported
	).

:- pred convert_type_defn(type_defn::in, type_ctor::in, globals::in,
	hlds_type_body::out) is det.

convert_type_defn(du_type(Body, MaybeUserEqComp), TypeCtor, Globals,
		du_type(Body, CtorTags, IsEnum, MaybeUserEqComp,
			ReservedTagPragma, IsForeign)) :-
	% Initially, when we first see the `:- type' definition,
	% we assign the constructor tags assuming that there is no
	% `:- pragma reserve_tag' declaration for this type.
	% (If it turns out that there was one, then we will recompute the
	% constructor tags by calling assign_constructor_tags again,
	% with ReservedTagPragma = yes, when processing the pragma.)
	ReservedTagPragma = no,
	assign_constructor_tags(Body, TypeCtor, ReservedTagPragma, Globals,
		CtorTags, IsEnum),
	IsForeign = no.
convert_type_defn(eqv_type(Body), _, _, eqv_type(Body)).
convert_type_defn(solver_type(SolverTypeDetails, MaybeUserEqComp), _, _,
		solver_type(SolverTypeDetails, MaybeUserEqComp)).
convert_type_defn(abstract_type(IsSolverType), _, _,
		abstract_type(IsSolverType)).
convert_type_defn(foreign_type(ForeignType, MaybeUserEqComp, Assertions),
		_, _, foreign_type(Body)) :-
	(
		ForeignType = il(ILForeignType),
		Data = foreign_type_lang_data(ILForeignType,
				MaybeUserEqComp, Assertions),
		Body = foreign_type_body(yes(Data), no, no)
	;
		ForeignType = c(CForeignType),
		Data = foreign_type_lang_data(CForeignType,
				MaybeUserEqComp, Assertions),
		Body = foreign_type_body(no, yes(Data), no)
	;
		ForeignType = java(JavaForeignType),
		Data = foreign_type_lang_data(JavaForeignType,
				MaybeUserEqComp, Assertions),
		Body = foreign_type_body(no, no, yes(Data))
	).

:- pred ctors_add(list(constructor)::in, type_ctor::in, tvarset::in,
	need_qualifier::in, partial_qualifier_info::in, prog_context::in,
	import_status::in, ctor_field_table::in, ctor_field_table::out,
	cons_table::in, cons_table::out, io::di, io::uo) is det.

ctors_add([], _, _, _, _, _, _, !FieldNameTable, !Ctors, !IO).
ctors_add([Ctor | Rest], TypeCtor, TVarSet, NeedQual, PQInfo, Context,
		ImportStatus, !FieldNameTable, !Ctors, !IO) :-
	Ctor = ctor(ExistQVars, Constraints, Name, Args),
	QualifiedConsId = make_cons_id(Name, Args, TypeCtor),
	ConsDefn = hlds_cons_defn(ExistQVars, Constraints, Args, TypeCtor,
		Context),
	%
	% Insert the fully-qualified version of this cons_id into the
	% cons_table.
	% Also check that there is at most one definition of a given
	% cons_id in each type.
	%
	( map__search(!.Ctors, QualifiedConsId, QualifiedConsDefns0) ->
		QualifiedConsDefns1 = QualifiedConsDefns0
	;
		QualifiedConsDefns1 = []
	),
	(
		list__member(OtherConsDefn, QualifiedConsDefns1),
		OtherConsDefn = hlds_cons_defn(_, _, _, TypeCtor, _)
	->
		% XXX we should record each error using module_info_incr_errors
		prog_out__write_context(Context, !IO),
		io__write_string("Error: constructor `", !IO),
		hlds_out__write_cons_id(QualifiedConsId, !IO),
		io__write_string("' for type `", !IO),
		hlds_out__write_type_ctor(TypeCtor, !IO),
		io__write_string("' multiply defined.\n", !IO),
		io__set_exit_status(1, !IO),
		QualifiedConsDefns = QualifiedConsDefns1
	;
		QualifiedConsDefns = [ConsDefn | QualifiedConsDefns1]
	),
	map__set(!.Ctors, QualifiedConsId, QualifiedConsDefns, !:Ctors),

	( QualifiedConsId = cons(qualified(Module, ConsName), Arity) ->
		% Add unqualified version of the cons_id to the
		% cons_table, if appropriate.
		(
			NeedQual = may_be_unqualified
		->
			UnqualifiedConsId = cons(unqualified(ConsName), Arity),
			multi_map__set(!.Ctors, UnqualifiedConsId, ConsDefn,
				!:Ctors)
		;
			true
		),

		% Add partially qualified versions of the cons_id
		get_partial_qualifiers(Module, PQInfo, PartialQuals),
		list__map_foldl(add_ctor(ConsName, Arity, ConsDefn),
			PartialQuals, _PartiallyQualifiedConsIds, !Ctors),

		assoc_list__keys(Args, FieldNames),
		FirstField = 1,

		add_ctor_field_names(FieldNames, NeedQual, PartialQuals,
			TypeCtor, QualifiedConsId, Context, ImportStatus,
			FirstField, !FieldNameTable, !IO)
	;
		error("ctors_add: cons_id not qualified")
	),
	ctors_add(Rest, TypeCtor, TVarSet, NeedQual, PQInfo, Context,
		ImportStatus, !FieldNameTable, !Ctors, !IO).

:- pred add_ctor(string::in, int::in, hlds_cons_defn::in, module_name::in,
	cons_id::out, cons_table::in, cons_table::out) is det.

add_ctor(ConsName, Arity, ConsDefn, ModuleQual, ConsId, CtorsIn, CtorsOut) :-
	ConsId = cons(qualified(ModuleQual, ConsName), Arity),
	multi_map__set(CtorsIn, ConsId, ConsDefn, CtorsOut).

:- pred add_ctor_field_names(list(maybe(ctor_field_name))::in,
	need_qualifier::in, list(module_name)::in, type_ctor::in, cons_id::in,
	prog_context::in, import_status::in, int::in,
	ctor_field_table::in, ctor_field_table::out, io::di, io::uo) is det.

add_ctor_field_names([], _, _, _, _, _, _, _, !FieldNameTable, !IO).
add_ctor_field_names([MaybeFieldName | FieldNames], NeedQual,
		PartialQuals, TypeCtor, ConsId, Context, ImportStatus,
		FieldNumber, !FieldNameTable, !IO) :-
	(
		MaybeFieldName = yes(FieldName),
		FieldDefn = hlds_ctor_field_defn(Context, ImportStatus,
			TypeCtor, ConsId, FieldNumber),
		add_ctor_field_name(FieldName, FieldDefn, NeedQual,
			PartialQuals, !FieldNameTable, !IO)
	;
		MaybeFieldName = no
	),
	add_ctor_field_names(FieldNames, NeedQual, PartialQuals, TypeCtor,
		ConsId, Context, ImportStatus, FieldNumber + 1,
		!FieldNameTable, !IO).

:- pred add_ctor_field_name(ctor_field_name::in, hlds_ctor_field_defn::in,
	need_qualifier::in, list(module_name)::in,
	ctor_field_table::in, ctor_field_table::out, io::di, io::uo) is det.

add_ctor_field_name(FieldName, FieldDefn, NeedQual, PartialQuals,
		!FieldNameTable, !IO) :-
	( FieldName = qualified(FieldModule0, _) ->
		FieldModule = FieldModule0
	;
		error("add_ctor_field_name: unqualified field name")
	),
	(
		%
		% Field names must be unique within a module, not
		% just within a type because the function names for
		% user-defined override functions for the builtin field
		% access functions must be unique within a module.
		%
		map__search(!.FieldNameTable, FieldName, ConflictingDefns)
	->
		( ConflictingDefns = [ConflictingDefn] ->
			ConflictingDefn =
				hlds_ctor_field_defn(OrigContext, _, _, _, _)
		;
			error(
			"add_ctor_field_name: multiple conflicting fields")
		),

		% XXX we should record each error
		% using module_info_incr_errors
		FieldDefn = hlds_ctor_field_defn(Context, _, _, _, _),
		prog_out__sym_name_to_string(FieldName, FieldString),
		ErrorPieces = [
			words("Error: field"),
			fixed(string__append_list(["`", FieldString, "'"])),
			words("multiply defined.")
		],
		error_util__write_error_pieces(Context, 0, ErrorPieces, !IO),

		% This type of error doesn't fit well with
		% how error_util does things -- error_util.m
		% wants to write everything with a single context.
		prog_out__write_context(OrigContext, !IO),
		io__write_string(
			"  Here is the previous definition of field `", !IO),
		io__write_string(FieldString, !IO),
		io__write_string("'.\n", !IO),
		io__set_exit_status(1, !IO)
	;
		unqualify_name(FieldName, UnqualFieldName),

		% Add an unqualified version of the field name to the
		% table, if appropriate.
		( NeedQual = may_be_unqualified ->
			multi_map__set(!.FieldNameTable,
				unqualified(UnqualFieldName), FieldDefn,
				!:FieldNameTable)
		;
			true
		),

		% Add partially qualified versions of the cons_id
		list__foldl(do_add_ctor_field(UnqualFieldName, FieldDefn),
			[FieldModule | PartialQuals], !FieldNameTable)
	).

:- pred do_add_ctor_field(string::in, hlds_ctor_field_defn::in,
	module_name::in, ctor_field_table::in, ctor_field_table::out) is det.

do_add_ctor_field(FieldName, FieldNameDefn, ModuleName, !FieldNameTable) :-
	multi_map__set(!.FieldNameTable, qualified(ModuleName, FieldName),
		FieldNameDefn, !:FieldNameTable).

%-----------------------------------------------------------------------------%

:- pred module_add_pred_or_func(tvarset::in, inst_varset::in, existq_tvars::in,
	pred_or_func::in, sym_name::in, list(type_and_mode)::in,
	maybe(determinism)::in, purity::in,
	class_constraints::in, pred_markers::in, prog_context::in,
	item_status::in, maybe(pair(pred_id, proc_id))::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_add_pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
		PredOrFunc, PredName, TypesAndModes, MaybeDet, Purity,
		ClassContext, Markers, Context, item_status(Status, NeedQual),
		MaybePredProcId, !Module, !IO) :-
	split_types_and_modes(TypesAndModes, Types, MaybeModes0),
	add_new_pred(TypeVarSet, ExistQVars, PredName, Types, Purity,
		ClassContext, Markers, Context, Status, NeedQual, PredOrFunc,
		!Module, !IO),
	(
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
	),
	(
		MaybeModes = yes(Modes),
		( check_marker(Markers, class_method) ->
			IsClassMethod = yes
		;
			IsClassMethod = no
		),
		module_add_mode(InstVarSet, PredName, Modes, MaybeDet,
			Status, Context, PredOrFunc, IsClassMethod,
			PredProcId, !Module, !IO),
		MaybePredProcId = yes(PredProcId)
	;
		MaybeModes = no,
		MaybePredProcId = no
	).

:- pred module_add_class_defn(list(class_constraint)::in, sym_name::in,
	list(tvar)::in, class_interface::in, tvarset::in, prog_context::in,
	item_status::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

module_add_class_defn(Constraints, Name, Vars, Interface, VarSet, Context,
		Status, !Module, !IO) :-
	module_info_classes(!.Module, Classes0),
	module_info_superclasses(!.Module, SuperClasses0),
	list__length(Vars, ClassArity),
	ClassId = class_id(Name, ClassArity),
	Status = item_status(ImportStatus0, _),
	( Interface = abstract ->
		make_status_abstract(ImportStatus0, ImportStatus1)
	;
		ImportStatus1 = ImportStatus0
	),
	(
		% the typeclass is exported if *any* occurrence is exported,
		% even a previous abstract occurrence
		map__search(Classes0, ClassId, OldDefn)
	->
		OldDefn = hlds_class_defn(OldStatus, OldConstraints, OldVars,
			OldInterface, OldMethods, OldVarSet, OldContext),
		combine_status(ImportStatus1, OldStatus, ImportStatus),
		(
			OldInterface = concrete(_),
			ClassMethods0 = OldMethods,
			ClassInterface = OldInterface
		;
			OldInterface = abstract,
			ClassMethods0 = [],
			ClassInterface = Interface
		),
		(
			\+ superclass_constraints_are_identical(OldVars,
				OldVarSet, OldConstraints, Vars, VarSet,
				Constraints)
		->
			% Always report the error, even in `.opt' files.
			DummyStatus = local,
			multiple_def_error(DummyStatus, Name, ClassArity,
				"typeclass", Context, OldContext, _, !IO),
			prog_out__write_context(Context, !IO),
			io__write_string(
			"  The superclass constraints do not match.\n", !IO),
			io__set_exit_status(1, !IO),
			ErrorOrPrevDef = yes
		;
			Interface = concrete(_),
			OldInterface = concrete(_)
		->
			multiple_def_error(ImportStatus, Name, ClassArity,
				"typeclass", Context, OldContext, _, !IO),
			ErrorOrPrevDef = yes
		;
			ErrorOrPrevDef = no
		),

		IsNewDefn = no
	;
		IsNewDefn = yes `with_type` bool,
		ErrorOrPrevDef = no `with_type` bool,
		ClassMethods0 = [],
		ClassInterface = Interface,
		ImportStatus = ImportStatus1
	),
	( ErrorOrPrevDef = no ->
		(
			Interface = concrete(Methods),
			module_add_class_interface(Name, Vars, Methods,
				Status, PredProcIds0, !Module, !IO),
				% Get rid of the `no's from the list of maybes
			IsYes = (pred(Maybe::in, PredProcId::out) is semidet :-
				Maybe = yes(Pred - Proc),
				PredProcId = hlds_class_proc(Pred, Proc)
			),
			list__filter_map(IsYes, PredProcIds0, PredProcIds1),

				%
				% The list must be sorted on pred_id and then
				% proc_id -- check_typeclass.m assumes this
				% when it is generating the corresponding list
				% of pred_proc_ids for instance definitions.
				%
			list__sort(PredProcIds1, ClassMethods)
		;
			Interface = abstract,
			ClassMethods = ClassMethods0
		),

		Defn = hlds_class_defn(ImportStatus, Constraints, Vars,
			ClassInterface, ClassMethods, VarSet, Context),
		map__set(Classes0, ClassId, Defn, Classes),
		module_info_set_classes(Classes, !Module),

		( IsNewDefn = yes ->
				% insert an entry into the super class table
				% for each super class of this class
			AddSuper = (pred(Super::in, Ss0::in, Ss::out) is det :-
				Super = constraint(SuperName, SuperTypes),
				list__length(SuperTypes, SuperClassArity),
				SuperClassId = class_id(SuperName,
					SuperClassArity),
				SubClassDetails = subclass_details(SuperTypes,
					ClassId, Vars, VarSet),
				multi_map__set(Ss0, SuperClassId,
					SubClassDetails, Ss)
			),
			list__foldl(AddSuper, Constraints,
				SuperClasses0, SuperClasses),

			module_info_set_superclasses(SuperClasses, !Module),

				% When we find the class declaration, make an
				% entry for the instances.
			module_info_instances(!.Module, Instances0),
			map__det_insert(Instances0, ClassId, [], Instances),
			module_info_set_instances(Instances, !Module)
		;
			true
		)
	;
		true
	).

:- pred superclass_constraints_are_identical(list(tvar)::in, tvarset::in,
	list(class_constraint)::in, list(tvar)::in, tvarset::in,
	list(class_constraint)::in) is semidet.

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

:- pred module_add_class_interface(sym_name::in, list(tvar)::in,
	list(class_method)::in, item_status::in,
	list(maybe(pair(pred_id, proc_id)))::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_add_class_interface(Name, Vars, Methods, Status, PredProcIds,
		!Module, !IO) :-
	module_add_class_interface_2(Name, Vars, Methods, Status, PredProcIds0,
		!Module, !IO),
	check_method_modes(Methods, PredProcIds0, PredProcIds,
		!Module, !IO).

:- pred module_add_class_interface_2(sym_name::in, list(tvar)::in,
	list(class_method)::in, item_status::in,
	list(maybe(pair(pred_id, proc_id)))::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_add_class_interface_2(_, _, [], _, [], !Module, !IO).
module_add_class_interface_2(Name, Vars, [M | Ms], Status, [P | Ps],
		!Module, !IO) :-
	module_add_class_method(M, Name, Vars, Status, P, !Module, !IO),
	module_add_class_interface_2(Name, Vars, Ms, Status, Ps, !Module, !IO).

:- pred module_add_class_method(class_method::in, sym_name::in, list(tvar)::in,
	item_status::in, maybe(pair(pred_id, proc_id))::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_add_class_method(Method, Name, Vars, Status, MaybePredIdProcId,
		!Module, !IO) :-
	(
		Method = pred_or_func(TypeVarSet, InstVarSet, ExistQVars,
			PredOrFunc, PredName, TypesAndModes, _WithType,
			_WithInst, MaybeDet, _Cond, Purity, ClassContext,
			Context),
		term__var_list_to_term_list(Vars, VarTerms),
		ClassContext = constraints(UnivCnstrs, ExistCnstrs),
		NewUnivCnstrs = [constraint(Name, VarTerms) | UnivCnstrs],
		NewClassContext = constraints(NewUnivCnstrs, ExistCnstrs),
		init_markers(Markers0),
		add_marker(class_method, Markers0, Markers),
		module_add_pred_or_func(TypeVarSet, InstVarSet,
			ExistQVars, PredOrFunc, PredName, TypesAndModes,
			MaybeDet, Purity, NewClassContext, Markers,
			Context, Status, MaybePredIdProcId, !Module, !IO)
	;
		Method = pred_or_func_mode(VarSet, MaybePredOrFunc, PredName,
			Modes, _WithInst, MaybeDet, _Cond, Context),
		( MaybePredOrFunc = yes(PredOrFunc) ->
			Status = item_status(ImportStatus, _),
			IsClassMethod = yes,
			module_add_mode(VarSet, PredName, Modes, MaybeDet,
				ImportStatus, Context, PredOrFunc,
				IsClassMethod, PredIdProcId, !Module, !IO),
			MaybePredIdProcId = yes(PredIdProcId)
		;
			% equiv_type.m should have either set the
			% pred_or_func or removed the item from the list.
			unexpected(this_file, "module_add_class_method: " ++
				"no pred_or_func on mode declaration")
		)
	).

	% Go through the list of class methods, looking for
	% - functions without mode declarations: add a default mode
	% - predicates without mode declarations: report an error
	% - mode declarations with no determinism: report an error
:- pred check_method_modes(list(class_method)::in,
	list(maybe(pair(pred_id, proc_id)))::in,
	list(maybe(pair(pred_id, proc_id)))::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_method_modes([], !PredProcIds, !Module, !IO).
check_method_modes([Method | Methods], !PredProcIds, !Module, !IO) :-
	(
		Method = pred_or_func(_, _, _, PorF, QualName, TypesAndModes,
			_WithType, _WithInst, _, _, _, _, _)
	->
		( QualName = qualified(ModuleName0, Name0) ->
			ModuleName = ModuleName0,
			Name = Name0
		;
			% The class interface should be fully module qualified
			% by prog_io.m at the time it is read in.
			error("add_default_class_method_func_modes: " ++
				"unqualified func")
		),
		list__length(TypesAndModes, PredArity),
		module_info_get_predicate_table(!.Module, PredTable),
		(
			predicate_table_search_pf_m_n_a(PredTable,
				is_fully_qualified, PorF, ModuleName,
				Name, PredArity, [PredId])
		->
			module_info_pred_info(!.Module, PredId, PredInfo0),
			(
				PorF = function,
				maybe_add_default_func_mode(PredInfo0,
					PredInfo, MaybeProc),
				(
					MaybeProc = no
				;
					MaybeProc = yes(ProcId),
					NewPredProc = yes(PredId - ProcId),
					!:PredProcIds = [NewPredProc |
						!.PredProcIds],
					module_info_set_pred_info(PredId,
						PredInfo, !Module)
				)
			;
				PorF = predicate,
				pred_info_procedures(PredInfo0, Procs),
				( map__is_empty(Procs) ->
					pred_method_with_no_modes_error(
						PredInfo0, !IO)
				;
					true
				)
			)
		;
			error("handle_methods_with_no_modes")
		)
	;
		true
	),
	check_method_modes(Methods, !PredProcIds, !Module, !IO).

:- pred module_add_instance_defn(module_name::in, list(class_constraint)::in,
	sym_name::in, list(type)::in, instance_body::in, tvarset::in,
	import_status::in, prog_context::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

module_add_instance_defn(InstanceModuleName, Constraints, ClassName,
		Types, Body0, VarSet, Status, Context, !Module, !IO) :-
	module_info_classes(!.Module, Classes),
	module_info_instances(!.Module, Instances0),
	list__length(Types, ClassArity),
	ClassId = class_id(ClassName, ClassArity),
	Body = expand_bang_state_var_args_in_instance_method_heads(Body0),
	(
		map__search(Classes, ClassId, _)
	->
		map__init(Empty),
		NewInstanceDefn = hlds_instance_defn(InstanceModuleName,
			Status, Context, Constraints, Types, Body, no,
			VarSet, Empty),
		map__lookup(Instances0, ClassId, InstanceDefns),
		check_for_overlapping_instances(NewInstanceDefn, InstanceDefns,
			ClassId, !IO),
		map__det_update(Instances0, ClassId,
			[NewInstanceDefn | InstanceDefns], Instances),
		module_info_set_instances(Instances, !Module)
	;
		undefined_type_class_error(ClassName, ClassArity, Context,
			"instance declaration", !IO)
	).

:- pred check_for_overlapping_instances(hlds_instance_defn::in,
	list(hlds_instance_defn)::in, class_id::in, io::di, io::uo) is det.

check_for_overlapping_instances(NewInstanceDefn, InstanceDefns, ClassId,
		!IO) :-
	IsOverlapping = (pred((Context - OtherContext)::out) is nondet :-
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
	),
	aggregate(IsOverlapping,
		report_overlapping_instance_declaration(ClassId), !IO).

:- pred report_overlapping_instance_declaration(class_id::in,
	pair(prog_context)::in, io::di, io::uo) is det.

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

:- pred add_new_pred(tvarset::in, existq_tvars::in, sym_name::in,
	list(type)::in, purity::in, class_constraints::in,
	pred_markers::in, prog_context::in, import_status::in,
	need_qualifier::in, pred_or_func::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

% NB.  Predicates are also added in lambda.m, which converts
% lambda expressions into separate predicates, so any changes may need
% to be reflected there too.

add_new_pred(TVarSet, ExistQVars, PredName, Types, Purity, ClassContext,
		Markers0, Context, ItemStatus, NeedQual, PredOrFunc,
		!Module, !IO) :-
	% Only preds with opt_imported clauses are tagged as opt_imported, so
	% that the compiler doesn't look for clauses for other preds read in
	% from optimization interfaces.
	( ItemStatus = opt_imported ->
		Status = imported(interface)
	;
		Status = ItemStatus
	),
	check_tvars_in_constraints(ClassContext, Types, TVarSet,
		PredOrFunc, PredName, Context, !Module, !IO),
	module_info_name(!.Module, ModuleName),
	list__length(Types, Arity),
	(
		PredName = unqualified(_PName),
		module_info_incr_errors(!Module),
		unqualified_pred_error(PredName, Arity, Context, !IO)
		% All predicate names passed into this predicate should have
		% been qualified by prog_io.m, when they were first read.
	;
		PredName = qualified(MNameOfPred, PName),
		module_info_get_predicate_table(!.Module, PredicateTable0),
		clauses_info_init(Arity, ClausesInfo),
		map__init(Proofs),
		purity_to_markers(Purity, PurityMarkers),
		markers_to_marker_list(PurityMarkers, MarkersList),
		list__foldl(add_marker, MarkersList, Markers0, Markers),
		globals__io_lookup_string_option(aditi_user, Owner, !IO),
		pred_info_init(ModuleName, PredName, Arity, PredOrFunc,
			Context, Status, none, Markers,
			Types, TVarSet, ExistQVars, ClassContext, Proofs,
			Owner, ClausesInfo, PredInfo0),
		(
			predicate_table_search_pf_m_n_a(PredicateTable0,
				is_fully_qualified, PredOrFunc, MNameOfPred,
				PName, Arity, [OrigPred|_])
		->
			module_info_pred_info(!.Module, OrigPred,
				OrigPredInfo),
			pred_info_context(OrigPredInfo, OrigContext),
			DeclString = pred_or_func_to_str(PredOrFunc),
			adjust_func_arity(PredOrFunc, OrigArity, Arity),
			multiple_def_error(ItemStatus, PredName, OrigArity,
				DeclString, Context, OrigContext, FoundError,
				!IO),
			( FoundError = yes ->
				module_info_incr_errors(!Module)
			;
				true
			)
		;
			module_info_get_partial_qualifier_info(!.Module,
				PQInfo),
			predicate_table_insert(PredInfo0, NeedQual, PQInfo,
				PredId, PredicateTable0, PredicateTable1),
			( pred_info_is_builtin(PredInfo0) ->
				add_builtin(PredId, Types,
					PredInfo0, PredInfo),
				predicate_table_get_preds(PredicateTable1,
					Preds1),
				map__det_update(Preds1, PredId, PredInfo,
					Preds),
				predicate_table_set_preds(Preds,
					PredicateTable1, PredicateTable)
			;
				PredicateTable = PredicateTable1
			),
			module_info_set_predicate_table(PredicateTable,
				!Module)
		)
	).

%-----------------------------------------------------------------------------%

	%
	% check for type variables which occur in the the class constraints,
	% but which don't occur in the predicate argument types
	%
:- pred check_tvars_in_constraints(class_constraints::in, list(type)::in,
	tvarset::in, pred_or_func::in, sym_name::in, prog_context::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_tvars_in_constraints(ClassContext, ArgTypes, TVarSet,
		PredOrFunc, PredName, Context, !Module, !IO) :-
	solutions(constrained_tvar_not_in_arg_types(ClassContext, ArgTypes),
		UnboundTVars),
	( UnboundTVars = [] ->
		true
	;
		module_info_incr_errors(!Module),
		report_unbound_tvars_in_class_context(UnboundTVars, ArgTypes,
			TVarSet, PredOrFunc, PredName, Context, !IO)
	).

:- pred constrained_tvar_not_in_arg_types(class_constraints::in,
	list(type)::in, tvar::out) is nondet.

constrained_tvar_not_in_arg_types(ClassContext, ArgTypes, TVar) :-
	ClassContext = constraints(UnivCs, ExistCs),
	( Constraints = UnivCs ; Constraints = ExistCs ),
	type_util__constraint_list_get_tvars(Constraints, TVars),
	list__member(TVar, TVars),
	\+ term__contains_var_list(ArgTypes, TVar).

:- pred report_unbound_tvars_in_class_context(list(tvar)::in, list(type)::in,
	tvarset::in, pred_or_func::in, sym_name::in, prog_context::in,
	io::di, io::uo) is det.

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

:- pred maybe_check_field_access_function(sym_name::in, arity::in,
	import_status::in, prog_context::in, module_info::in,
	io::di, io::uo) is det.

maybe_check_field_access_function(FuncName, FuncArity, Status, Context,
		Module, !IO) :-
	(
		is_field_access_function_name(Module, FuncName, FuncArity,
			AccessType, FieldName)
	->
		check_field_access_function(AccessType, FieldName, FuncName,
			FuncArity, Status, Context, Module, !IO)
	;
		true
	).

:- pred check_field_access_function(field_access_type::in, ctor_field_name::in,
	sym_name::in, arity::in, import_status::in, prog_context::in,
	module_info::in, io::di, io::uo) is det.

check_field_access_function(_AccessType, FieldName, FuncName, FuncArity,
		FuncStatus, Context, Module, !IO) :-
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
		report_field_status_mismatch(Context, FuncCallId, !IO)
	;
		true
	).

:- pred report_field_status_mismatch(prog_context::in, simple_call_id::in,
	io::di, io::uo) is det.

report_field_status_mismatch(Context, CallId, !IO) :-
	CallIdString = hlds_out__simple_call_id_to_string(CallId),
	ErrorPieces = [
		words("In declaration of"),
		fixed(string__append(CallIdString, ":")),
		nl,
		words("error: a field access function for an"),
		words("exported field must also be exported.")
	],
	error_util__write_error_pieces(Context, 0, ErrorPieces, !IO),
	io__set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- pred add_builtin(pred_id::in, list(type)::in, pred_info::in, pred_info::out)
	is det.

	% For a builtin predicate, say foo/2, we add a clause
	%
	%	foo(H1, H2) :- foo(H1, H2).
	%
	% This does not generate an infinite loop!
	% Instead, the compiler will generate the usual builtin inline code
	% for foo/2 in the body.  The reason for generating this
	% forwarding code stub is so that things work correctly if
	% you take the address of the predicate.

add_builtin(PredId, Types, !PredInfo) :-
		%
		% lookup some useful info: Module, Name, Context, HeadVars
		%
	Module = pred_info_module(!.PredInfo),
	Name = pred_info_name(!.PredInfo),
	pred_info_context(!.PredInfo, Context),
	pred_info_clauses_info(!.PredInfo, ClausesInfo0),
	clauses_info_varset(ClausesInfo0, VarSet),
	clauses_info_headvars(ClausesInfo0, HeadVars),

		%
		% construct the pseudo-recursive call to Module:Name(HeadVars)
		%
	SymName = qualified(Module, Name),
	ModeId = invalid_proc_id,	% mode checking will figure it out
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
	pred_info_set_clauses_info(ClausesInfo, !PredInfo),

		%
		% It's pointless but harmless to inline these clauses.
		% The main purpose of the `no_inline' marker is to stop
		% constraint propagation creating real infinite loops in
		% the generated code when processing calls to these
		% predicates. The code generator will still generate
		% inline code for calls to these predicates.
		%
	pred_info_get_markers(!.PredInfo, Markers0),
	add_marker(no_inline, Markers0, Markers),
	pred_info_set_markers(Markers, !PredInfo).

%-----------------------------------------------------------------------------%

:- pred add_special_preds(tvarset::in, (type)::in, type_ctor::in,
	hlds_type_body::in, prog_context::in, import_status::in,
	module_info::in, module_info::out) is det.

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

add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status, !Module) :-
	(
		special_pred_is_generated_lazily(!.Module, TypeCtor, Body,
			Status)
	->
		true
	;
		can_generate_special_pred_clauses_for_type(!.Module, TypeCtor,
			Body)
	->
		add_special_pred(unify, TVarSet, Type, TypeCtor, Body, Context,
			Status, !Module),
		(
			status_defined_in_this_module(Status, yes)
		->
			(
				Ctors = Body ^ du_type_ctors,
				Body ^ du_type_is_enum = no,
				Body ^ du_type_usereq = no,
				module_info_globals(!.Module, Globals),
				globals__lookup_int_option(Globals,
					compare_specialization, CompareSpec),
				list__length(Ctors, CtorCount),
				CtorCount > CompareSpec
			->
				SpecialPredIds = [index, compare]
			;
				SpecialPredIds = [compare]
			),
			add_special_pred_list(SpecialPredIds, TVarSet, Type,
				TypeCtor, Body, Context, Status, !Module)
		;
			% Never add clauses for comparison predicates
			% for imported types -- they will never be used.
			module_info_get_special_pred_map(!.Module,
				SpecialPreds),
			( map__contains(SpecialPreds, compare - TypeCtor) ->
				true
			;
				add_special_pred_decl(compare, TVarSet, Type,
					TypeCtor, Body, Context, Status,
					!Module)
			)
		),
		(
			type_util__type_body_is_solver_type(!.Module, Body)
		->
			add_special_pred(initialise, TVarSet, Type,
				TypeCtor, Body, Context, Status, !Module)
		;
			true
 		)
	;
		(
			type_util__type_body_is_solver_type(!.Module, Body)
		->
			SpecialPredIds = [unify, compare, initialise]
		;
			SpecialPredIds = [unify, compare]
		),
		add_special_pred_decl_list(SpecialPredIds, TVarSet, Type,
			TypeCtor, Body, Context, Status, !Module)
	).

:- pred add_special_pred_list(list(special_pred_id)::in, tvarset::in,
	(type)::in, type_ctor::in, hlds_type_body::in, prog_context::in,
	import_status::in, module_info::in, module_info::out) is det.

add_special_pred_list([], _, _, _, _, _, _, !Module).
add_special_pred_list([SpecialPredId | SpecialPredIds], TVarSet, Type,
		TypeCtor, Body, Context, Status, !Module) :-
	add_special_pred(SpecialPredId, TVarSet, Type,
		TypeCtor, Body, Context, Status, !Module),
	add_special_pred_list(SpecialPredIds, TVarSet, Type,
		TypeCtor, Body, Context, Status, !Module).

:- pred add_special_pred(special_pred_id::in, tvarset::in, (type)::in,
	type_ctor::in, hlds_type_body::in, prog_context::in, import_status::in,
	module_info::in, module_info::out) is det.

add_special_pred(SpecialPredId, TVarSet, Type, TypeCtor, TypeBody, Context,
		Status0, !Module) :-
	module_info_globals(!.Module, Globals),
	globals__lookup_bool_option(Globals, special_preds, GenSpecialPreds),
	(
		GenSpecialPreds = yes,
		add_special_pred_for_real(SpecialPredId, TVarSet, Type,
			TypeCtor, TypeBody, Context, Status0, !Module)
	;
		GenSpecialPreds = no,
		(
			SpecialPredId = unify,
			add_special_pred_unify_status(TypeBody, Status0,
				Status),
			add_special_pred_for_real(SpecialPredId, TVarSet, Type,
				TypeCtor, TypeBody, Context, Status, !Module)
		;
			SpecialPredId = index
		;
			SpecialPredId = compare,
			( TypeBody ^ du_type_usereq = yes(_) ->
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
					TVarSet, Type, TypeCtor, TypeBody,
					Context, Status0, !Module)
			;
				true
			)
		;
			SpecialPredId = initialise,
			(
				type_is_solver_type(!.Module, Type)
			->
				add_special_pred_for_real(SpecialPredId,
					TVarSet, Type, TypeCtor, TypeBody,
					Context, Status0, !Module)
			;
				error("make_hlds.add_special_pred: " ++
					"attempt to add initialise pred " ++
					"for non-solver type")
			)
		)
	).

add_special_pred_for_real(SpecialPredId, TVarSet, Type0, TypeCtor,
		TypeBody, Context, Status0, !Module) :-
	Type = adjust_types_with_special_preds_in_private_builtin(Type0),
	adjust_special_pred_status(SpecialPredId, Status0, Status),
	module_info_get_special_pred_map(!.Module, SpecialPredMap0),
	( map__contains(SpecialPredMap0, SpecialPredId - TypeCtor) ->
		true
	;
		add_special_pred_decl_for_real(SpecialPredId, TVarSet,
			Type, TypeCtor, Context, Status, !Module)
	),
	module_info_get_special_pred_map(!.Module, SpecialPredMap1),
	map__lookup(SpecialPredMap1, SpecialPredId - TypeCtor, PredId),
	module_info_preds(!.Module, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% if the type was imported, then the special preds for that
	% type should be imported too
	(
		(Status = imported(_) ; Status = pseudo_imported)
	->
		pred_info_set_import_status(Status, PredInfo0, PredInfo1)
	;
		TypeBody ^ du_type_usereq = yes(_),
		pred_info_import_status(PredInfo0, OldStatus),
		OldStatus = pseudo_imported,
		status_is_imported(Status, no)
	->
		% We can only get here with --no-special-preds if the old
		% status is from an abstract declaration of the type.
		% Since the compiler did not then know that the type definition
		% will specify a user-defined equality predicate, it set up
		% the status as pseudo_imported in order to prevent the
		% generation of code for mode 0 of the unify predicate
		% for the type. However, for types with user-defined equality,
		% we *do* want to generate code for mode 0 of unify,
		% so we fix the status.
		pred_info_set_import_status(Status, PredInfo0, PredInfo1)
	;
		PredInfo1 = PredInfo0
	),
	unify_proc__generate_clause_info(SpecialPredId, Type, TypeBody,
		Context, !.Module, ClausesInfo),
	pred_info_set_clauses_info(ClausesInfo, PredInfo1, PredInfo2),
	pred_info_get_markers(PredInfo2, Markers2),
	add_marker(calls_are_fully_qualified, Markers2, Markers),
	pred_info_set_markers(Markers, PredInfo2, PredInfo3),
	pred_info_set_maybe_special_pred(yes(SpecialPredId - TypeCtor),
		PredInfo3, PredInfo),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Preds, !Module).

	% These types need to have the builtin qualifier removed
	% so that their special predicates type check.
:- func adjust_types_with_special_preds_in_private_builtin(type) = (type).

adjust_types_with_special_preds_in_private_builtin(Type) = NormalizedType :-
	( type_to_ctor_and_args(Type, TypeCtor, []) ->
		( is_builtin_types_special_preds_defined_in_mercury(TypeCtor,
				Name) ->
			construct_type(unqualified(Name) - 0, [],
				NormalizedType)
		;
			NormalizedType = Type
		)
	;
		NormalizedType = Type
	).

:- pred add_special_pred_decl_list(list(special_pred_id)::in, tvarset::in,
	(type)::in, type_ctor::in, hlds_type_body::in, prog_context::in,
	import_status::in, module_info::in, module_info::out) is det.

add_special_pred_decl_list([], _, _, _, _, _, _, !Module).
add_special_pred_decl_list([SpecialPredId | SpecialPredIds], TVarSet, Type,
		TypeCtor, TypeBody, Context, Status, !Module) :-
	add_special_pred_decl(SpecialPredId, TVarSet, Type,
		TypeCtor, TypeBody, Context, Status, !Module),
	add_special_pred_decl_list(SpecialPredIds, TVarSet, Type,
		TypeCtor, TypeBody, Context, Status, !Module).

:- pred add_special_pred_decl(special_pred_id::in, tvarset::in, (type)::in,
	type_ctor::in, hlds_type_body::in, prog_context::in, import_status::in,
	module_info::in, module_info::out) is det.

add_special_pred_decl(SpecialPredId, TVarSet, Type, TypeCtor, TypeBody,
		Context, Status0, !Module) :-
	module_info_globals(!.Module, Globals),
	globals__lookup_bool_option(Globals, special_preds, GenSpecialPreds),
	( GenSpecialPreds = yes ->
		add_special_pred_decl_for_real(SpecialPredId,
			TVarSet, Type, TypeCtor, Context, Status0, !Module)
	; SpecialPredId = unify ->
		add_special_pred_unify_status(TypeBody, Status0, Status),
		add_special_pred_decl_for_real(SpecialPredId, TVarSet, Type,
			TypeCtor, Context, Status, !Module)
	;
		true
	).

add_special_pred_decl_for_real(SpecialPredId, TVarSet, Type, TypeCtor,
		Context, Status0, !Module) :-
	module_info_name(!.Module, ModuleName),
	special_pred_interface(SpecialPredId, Type, ArgTypes, ArgModes, Det),
	Name = special_pred_name(SpecialPredId, TypeCtor),
	(
		SpecialPredId = initialise
	->
		TypeCtor = TypeSymName - _TypeArity,
		sym_name_get_module_name(TypeSymName, ModuleName,
			TypeModuleName),
		PredName = qualified(TypeModuleName, Name)
	;
		PredName = unqualified(Name)
	),
	special_pred_name_arity(SpecialPredId, _, Arity),
	clauses_info_init(Arity, ClausesInfo0),
	adjust_special_pred_status(SpecialPredId, Status0, Status),
	map__init(Proofs),
	init_markers(Markers),
		% XXX If/when we have "comparable" or "unifiable" typeclasses,
		% XXX this context might not be empty
	ClassContext = constraints([], []),
	ExistQVars = [],
	module_info_globals(!.Module, Globals),
	globals__lookup_string_option(Globals, aditi_user, Owner),
	pred_info_init(ModuleName, PredName, Arity, predicate, Context,
		Status, none, Markers, ArgTypes, TVarSet, ExistQVars,
		ClassContext, Proofs, Owner, ClausesInfo0, PredInfo0),
	pred_info_set_maybe_special_pred(yes(SpecialPredId - TypeCtor),
		PredInfo0, PredInfo1),
	ArgLives = no,
	varset__init(InstVarSet),
		% Should not be any inst vars here so it's ok to use a
		% fresh inst_varset.
	add_new_proc(InstVarSet, Arity, ArgModes, yes(ArgModes), ArgLives,
		yes(Det), Context, address_is_not_taken, PredInfo1, PredInfo,
		_),

	module_info_get_predicate_table(!.Module, PredicateTable0),
	predicate_table_insert(PredInfo, PredId,
		PredicateTable0, PredicateTable),
	module_info_set_predicate_table(PredicateTable, !Module),
	module_info_get_special_pred_map(!.Module, SpecialPredMap0),
	map__set(SpecialPredMap0, SpecialPredId - TypeCtor, PredId,
		SpecialPredMap),
	module_info_set_special_pred_map(SpecialPredMap, !Module).

:- pred add_special_pred_unify_status(hlds_type_body::in, import_status::in,
	import_status::out) is det.

add_special_pred_unify_status(TypeBody, Status0, Status) :-
	( TypeBody ^ du_type_usereq = yes(_) ->
			% If the type has user-defined equality,
			% then we create a real unify predicate
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

:- pred adjust_special_pred_status(special_pred_id::in,
	import_status::in, import_status::out) is det.

adjust_special_pred_status(SpecialPredId, !Status) :-
	( ( !.Status = opt_imported ; !.Status = abstract_imported ) ->
		!:Status = imported(interface)
	; !.Status = abstract_exported ->
		!:Status = exported
	;
		true
	),

	% unification predicates are special - they are
	% "pseudo"-imported/exported (only mode 0 is imported/exported).
	( SpecialPredId = unify ->
		( !.Status = imported(_) ->
			!:Status = pseudo_imported
		; !.Status = exported ->
			!:Status = pseudo_exported
		;
			true
		)
	;
		true
	).

add_new_proc(InstVarSet, Arity, ArgModes, MaybeDeclaredArgModes, MaybeArgLives,
		MaybeDet, Context, IsAddressTaken, PredInfo0, PredInfo,
		ModeId) :-
	pred_info_procedures(PredInfo0, Procs0),
	pred_info_arg_types(PredInfo0, ArgTypes),
	next_mode_id(Procs0, MaybeDet, ModeId),
	proc_info_init(Context, Arity, ArgTypes, MaybeDeclaredArgModes,
		ArgModes, MaybeArgLives, MaybeDet, IsAddressTaken, NewProc0),
	proc_info_set_inst_varset(InstVarSet, NewProc0, NewProc),
	map__det_insert(Procs0, ModeId, NewProc, Procs),
	pred_info_set_procedures(Procs, PredInfo0, PredInfo).

%-----------------------------------------------------------------------------%

	% Add a mode declaration for a predicate.

:- pred module_add_mode(inst_varset::in, sym_name::in, list(mode)::in,
	maybe(determinism)::in, import_status::in, prog_context::in,
	pred_or_func::in, bool::in, pair(pred_id, proc_id)::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

	% We should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments.

module_add_mode(InstVarSet, PredName, Modes, MaybeDet, Status, MContext,
		PredOrFunc, IsClassMethod, PredProcId, !ModuleInfo, !IO) :-

		% Lookup the pred or func declaration in the predicate table.
		% If it's not there (or if it is ambiguous), optionally print a
		% warning message and insert an implicit definition for the
		% predicate; it is presumed to be local, and its type
		% will be inferred automatically.

	module_info_name(!.ModuleInfo, ModuleName0),
	sym_name_get_module_name(PredName, ModuleName0, ModuleName),
	list__length(Modes, Arity),
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
	(
		predicate_table_search_pf_sym_arity(PredicateTable0,
			is_fully_qualified, PredOrFunc, PredName, Arity,
			[PredId0])
	->
		PredId = PredId0
	;
		preds_add_implicit_report_error(ModuleName, PredOrFunc,
			PredName, Arity, Status, IsClassMethod, MContext,
			"mode declaration", PredId, !ModuleInfo, !IO)
	),

		% Lookup the pred_info for this predicate
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable1),
	predicate_table_get_preds(PredicateTable1, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),

	module_do_add_mode(InstVarSet, Arity, Modes, MaybeDet,
		IsClassMethod, MContext, PredInfo0, PredInfo, ProcId, !IO),
	map__det_update(Preds0, PredId, PredInfo, Preds),
	predicate_table_set_preds(Preds, PredicateTable1, PredicateTable),
	module_info_set_predicate_table(PredicateTable, !ModuleInfo),
	PredProcId = PredId - ProcId.

:- pred module_do_add_mode(inst_varset::in, arity::in, list(mode)::in,
	maybe(determinism)::in, bool::in, prog_context::in,
	pred_info::in, pred_info::out, proc_id::out, io::di, io::uo) is det.

module_do_add_mode(InstVarSet, Arity, Modes, MaybeDet, IsClassMethod, MContext,
		!PredInfo, ProcId, !IO) :-
		% check that the determinism was specified
	(
		MaybeDet = no
	->
		pred_info_import_status(!.PredInfo, ImportStatus),
		PredOrFunc = pred_info_is_pred_or_func(!.PredInfo),
		PredModule = pred_info_module(!.PredInfo),
		PredName = pred_info_name(!.PredInfo),
		PredSymName = qualified(PredModule, PredName),
		( IsClassMethod = yes ->
			unspecified_det_for_method(PredSymName, Arity,
				PredOrFunc, MContext, !IO)
		; status_is_exported(ImportStatus, yes) ->
			unspecified_det_for_exported(PredSymName, Arity,
				PredOrFunc, MContext, !IO)
		;
			globals__io_lookup_bool_option(infer_det, InferDet,
				!IO),
			(
				InferDet = no
			->
				unspecified_det_for_local(PredSymName, Arity,
					PredOrFunc, MContext, !IO)
			;
				true
			)
		)
	;
		true
	),

		% add the mode declaration to the pred_info for this procedure.
	ArgLives = no,
	add_new_proc(InstVarSet, Arity, Modes, yes(Modes), ArgLives, MaybeDet,
		MContext, address_is_not_taken, !PredInfo, ProcId).

	% Whenever there is a clause or mode declaration for an undeclared
	% predicate, we add an implicit declaration
	%	:- pred p(T1, T2, ..., Tn).
	% for that predicate; the real types will be inferred by
	% type inference.

:- pred preds_add_implicit_report_error(module_name::in, pred_or_func::in,
	sym_name::in, arity::in, import_status::in, bool::in, prog_context::in,
	string::in, pred_id::out, module_info::in, module_info::out,
	io::di, io::uo) is det.

preds_add_implicit_report_error(ModuleName, PredOrFunc, PredName, Arity,
		Status, IsClassMethod, Context, Description, PredId,
		!ModuleInfo, !IO) :-
	maybe_undefined_pred_error(PredName, Arity, PredOrFunc, Status,
		IsClassMethod, Context, Description, !IO),
	( PredOrFunc = function ->
		adjust_func_arity(function, FuncArity, Arity),
		maybe_check_field_access_function(PredName, FuncArity,
			Status, Context, !.ModuleInfo, !IO)
	;
		true
	),
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
	preds_add_implicit(!.ModuleInfo, ModuleName, PredName, Arity, Status,
		Context, PredOrFunc, PredId, PredicateTable0, PredicateTable),
	module_info_set_predicate_table(PredicateTable, !ModuleInfo).

:- pred preds_add_implicit(module_info::in, module_name::in, sym_name::in,
	arity::in, import_status::in, prog_context::in, pred_or_func::in,
	pred_id::out, predicate_table::in, predicate_table::out) is det.

preds_add_implicit(ModuleInfo, ModuleName, PredName, Arity, Status, Context,
		PredOrFunc, PredId, !PredicateTable) :-
	clauses_info_init(Arity, ClausesInfo),
	preds_add_implicit_2(ClausesInfo, ModuleInfo, ModuleName, PredName,
		Arity, Status, Context, PredOrFunc, PredId, !PredicateTable).

:- pred preds_add_implicit_for_assertion(prog_vars::in, module_info::in,
	module_name::in, sym_name::in, arity::in, import_status::in,
	prog_context::in, pred_or_func::in, pred_id::out,
	predicate_table::in, predicate_table::out) is det.

preds_add_implicit_for_assertion(HeadVars, ModuleInfo, ModuleName, PredName,
		Arity, Status, Context, PredOrFunc, PredId, !PredicateTable) :-
	clauses_info_init_for_assertion(HeadVars, ClausesInfo),
	preds_add_implicit_2(ClausesInfo, ModuleInfo, ModuleName, PredName,
		Arity, Status, Context, PredOrFunc, PredId, !PredicateTable).

:- pred preds_add_implicit_2(clauses_info::in, module_info::in,
	module_name::in, sym_name::in, arity::in, import_status::in,
	prog_context::in, pred_or_func::in, pred_id::out,
	predicate_table::in, predicate_table::out) is det.

preds_add_implicit_2(ClausesInfo, ModuleInfo, ModuleName, PredName, Arity,
		Status, Context, PredOrFunc, PredId, !PredicateTable) :-
	varset__init(TVarSet0),
	make_n_fresh_vars("T", Arity, TypeVars, TVarSet0, TVarSet),
	term__var_list_to_term_list(TypeVars, Types),
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
	pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
		Status, none, Markers0, Types, TVarSet, ExistQVars,
		ClassContext, Proofs, Owner, ClausesInfo, PredInfo0),
	add_marker(infer_type, Markers0, Markers),
	pred_info_set_markers(Markers, PredInfo0, PredInfo),
	(
		\+ predicate_table_search_pf_sym_arity(!.PredicateTable,
			is_fully_qualified, PredOrFunc, PredName, Arity, _)
	->
		module_info_get_partial_qualifier_info(ModuleInfo,
			MQInfo),
		predicate_table_insert(PredInfo,
			may_be_unqualified, MQInfo, PredId, !PredicateTable)
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

:- pred module_add_clause(prog_varset::in, pred_or_func::in, sym_name::in,
	list(prog_term)::in, goal::in, import_status::in, prog_context::in,
	goal_type::in, module_info::in, module_info::out,
	qual_info::in, qual_info::out, io::di, io::uo) is det.

module_add_clause(ClauseVarSet, PredOrFunc, PredName, Args0, Body, Status,
		Context, GoalType, !ModuleInfo, !Info, !IO) :-
	( illegal_state_var_func_result(PredOrFunc, Args0, SVar) ->
		IllegalSVarResult = yes(SVar)
	;
		IllegalSVarResult = no
	),
	ArityAdjustment = ( if IllegalSVarResult = yes(_) then -1 else 0 ),
	Args = expand_bang_state_var_args(Args0),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	( VeryVerbose = yes ->
		io__write_string("% Processing clause for ", !IO),
		write_pred_or_func(PredOrFunc, !IO),
		io__write_string(" `", !IO),
		list__length(Args, PredArity0),
		PredArity = PredArity0 + ArityAdjustment,
		adjust_func_arity(PredOrFunc, OrigArity, PredArity),
		prog_out__write_sym_name_and_arity(PredName/OrigArity, !IO),
		io__write_string("'...\n", !IO)
	;
		true
	),

		% Lookup the pred declaration in the predicate table.
		% (If it's not there, call maybe_undefined_pred_error
		% and insert an implicit declaration for the predicate.)
	module_info_name(!.ModuleInfo, ModuleName),
	list__length(Args, Arity0),
	Arity = Arity0 + ArityAdjustment,
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
	(
		predicate_table_search_pf_sym_arity(PredicateTable0,
			is_fully_qualified, PredOrFunc, PredName,
			Arity, [PredId0])
	->
		PredId = PredId0,
		( GoalType = promise(_) ->
			prog_out__sym_name_to_string(PredName, NameString),
			string__format("%s %s %s (%s).\n",
				[s("Attempted to introduce a predicate"),
				s("for a promise with an identical"),
				s("name to an existing predicate"),
				s(NameString)], String),
			error(String)
		;
			true
		)
	;
			% A promise will not have a
			% corresponding pred declaration.
		(
			GoalType = promise(_)
		->
			term__term_list_to_var_list(Args, HeadVars),
			preds_add_implicit_for_assertion(HeadVars,
				!.ModuleInfo, ModuleName, PredName, Arity,
				Status, Context, PredOrFunc, PredId,
				PredicateTable0, PredicateTable1),
			module_info_set_predicate_table(PredicateTable1,
				!ModuleInfo)
		;
			preds_add_implicit_report_error(ModuleName,
				PredOrFunc, PredName, Arity, Status, no,
				Context, "clause", PredId, !ModuleInfo, !IO)
		)
	),
		% Lookup the pred_info for this pred,
		% add the clause to the clauses_info in the pred_info,
		% if there are no modes add an `infer_modes' marker,
		% and then save the pred_info.
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable2),
	predicate_table_get_preds(PredicateTable2, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause for them
	( Status = opt_imported ->
		pred_info_set_import_status(opt_imported,
			PredInfo0, PredInfo0a),
		pred_info_get_markers(PredInfo0a, Markers0),
		add_marker(calls_are_fully_qualified, Markers0, Markers1),
		pred_info_set_markers(Markers1, PredInfo0a, PredInfo1)
	;
		PredInfo1 = PredInfo0
	),
	(
		IllegalSVarResult = yes(StateVar)
	->
		report_illegal_func_svar_result(Context, ClauseVarSet,
			StateVar, !IO)
	;
		%
		% User-supplied clauses for field access functions are
		% not allowed -- the clauses are always generated by the
		% compiler.
		%
		PredOrFunc = function,
		adjust_func_arity(function, FuncArity, Arity),
		is_field_access_function_name(!.ModuleInfo, PredName,
			FuncArity, _, _),

		% Don't report errors for clauses for field access
		% function clauses in `.opt' files.
		Status \= opt_imported
	->
		module_info_incr_errors(!ModuleInfo),
		CallIdString0 = hlds_out__simple_call_id_to_string(
			PredOrFunc - PredName/Arity),
		string__append(CallIdString0, ".", CallIdString),
		ErrorPieces0 = [
			words("Error: clause for automatically generated"),
			words("field access"),
			fixed(CallIdString),
			nl
		],
		globals__io_lookup_bool_option(verbose_errors, Verbose, !IO),
		(
			Verbose = yes,
			ErrorPieces1 = [
				words("Clauses for field access functions"),
				words("are automatically generated by the"),
				words("compiler. To supply your own"),
				words("definition for a field access"),
				words("function, for example to check"),
				words("the input to a field update,"),
				words("give the field of the constructor a"),
				words("different name.")
			],
			list__append(ErrorPieces0, ErrorPieces1,
				ErrorPieces)
		;
			Verbose = no,
			ErrorPieces = ErrorPieces0
		),
		error_util__write_error_pieces(Context, 0, ErrorPieces, !IO)
	;
		% Ignore clauses for builtins. This makes bootstrapping
		% easier when redefining builtins to use normal Mercury code.
		pred_info_is_builtin(PredInfo1)
	->
		prog_out__write_context(Context, !IO),
		report_warning("Warning: clause for builtin.\n", !IO)
	;
		pred_info_clauses_info(PredInfo1, Clauses0),
		pred_info_typevarset(PredInfo1, TVarSet0),
		maybe_add_default_func_mode(PredInfo1, PredInfo2, _),
		select_applicable_modes(Args, ClauseVarSet, Status, Context,
			PredId, PredInfo2, ArgTerms, ProcIdsForThisClause,
			!ModuleInfo, !Info, !IO),
		clauses_info_add_clause(ProcIdsForThisClause,
			ClauseVarSet, TVarSet0, ArgTerms, Body, Context,
			Status, PredOrFunc, Arity, GoalType, Goal,
			VarSet, TVarSet, Clauses0, Clauses, Warnings,
			!ModuleInfo, !Info, !IO),
		pred_info_set_clauses_info(Clauses, PredInfo2, PredInfo3),
		( GoalType = promise(PromiseType) ->
			pred_info_set_goal_type(promise(PromiseType),
				PredInfo3, PredInfo4)
		;
			pred_info_update_goal_type(clauses,
				PredInfo3, PredInfo4)
		),
		pred_info_set_typevarset(TVarSet, PredInfo4, PredInfo5),
		pred_info_arg_types(PredInfo5, _ArgTVarSet,
			ExistQVars, ArgTypes),
		pred_info_set_arg_types(TVarSet, ExistQVars, ArgTypes,
			PredInfo5, PredInfo6),

		%
		% check if there are still no modes for the predicate,
		% and if so, set the `infer_modes' flag for that predicate
		%
		ProcIds = pred_info_all_procids(PredInfo6),
		( ProcIds = [] ->
			pred_info_get_markers(PredInfo6, Markers6),
			add_marker(infer_modes, Markers6, Markers),
			pred_info_set_markers(Markers, PredInfo6, PredInfo)
		;
			PredInfo = PredInfo6
		),
		map__det_update(Preds0, PredId, PredInfo, Preds),
		predicate_table_set_preds(Preds,
			PredicateTable2, PredicateTable),
		module_info_set_predicate_table(PredicateTable, !ModuleInfo),
		( Status \= opt_imported ->
			% warn about singleton variables
			maybe_warn_singletons(VarSet,
				PredOrFunc - PredName/Arity, !.ModuleInfo,
				Goal, !IO),
			% warn about variables with overlapping scopes
			maybe_warn_overlap(Warnings, VarSet,
				PredOrFunc - PredName/Arity, !IO)
		;
			true
		)
	).

	% Extract the mode annotations (if any) from the clause arguments,
	% and determine which mode(s) this clause should apply to.

:- pred select_applicable_modes(list(prog_term)::in, prog_varset::in,
	import_status::in, prog_context::in, pred_id::in, pred_info::in,
	list(prog_term)::out, list(proc_id)::out,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

select_applicable_modes(Args0, VarSet, Status, Context, PredId, PredInfo,
		Args, ProcIds, !ModuleInfo, !Info, !IO) :-
	get_mode_annotations(Args0, Args, empty, ModeAnnotations),
	(
		ModeAnnotations = modes(ModeList0),

		%
		% The user specified some mode annotations on this clause.
		% First module-qualify the mode annotations. The annotations
		% on clauses from `.opt' files will already be fully module
		% qualified.
		%
		( Status = opt_imported ->
			ModeList = ModeList0
		;
			qual_info_get_mq_info(!.Info, MQInfo0),
			module_qual__qualify_clause_mode_list(ModeList0,
				ModeList, Context, MQInfo0, MQInfo, !IO),
			qual_info_set_mq_info(MQInfo, !Info)
		),

		%
		% Now find the procedure which matches these mode annotations.
		%
		pred_info_procedures(PredInfo, Procs),
		map__to_assoc_list(Procs, ExistingProcs),
		(
			get_procedure_matching_declmodes(ExistingProcs,
				ModeList, !.ModuleInfo, ProcId)
		->
			ProcIds = [ProcId]
		;
			module_info_incr_errors(!ModuleInfo),
			undeclared_mode_error(ModeList, VarSet, PredId,
				PredInfo, !.ModuleInfo, Context, !IO),
			% apply the clause to all modes
			% XXX would it be better to apply it to none?
			ProcIds = pred_info_all_procids(PredInfo)
		)
	;
		ModeAnnotations = empty,
		( pred_info_pragma_goal_type(PredInfo) ->
			% We are only allowed to mix foreign procs and
			% mode specific clauses, so make this clause
			% mode specific but apply to all modes.
			ProcIds = pred_info_all_procids(PredInfo)
		;
			% this means the clauses applies to all modes
			ProcIds = []
		)
	;
		ModeAnnotations = none,
		( pred_info_pragma_goal_type(PredInfo) ->
			% We are only allowed to mix foreign procs and
			% mode specific clauses, so make this clause
			% mode specific but apply to all modes.
			ProcIds = pred_info_all_procids(PredInfo)
		;
			% this means the clauses applies to all modes
			ProcIds = []
		)
	;
		ModeAnnotations = mixed,
		module_info_incr_errors(!ModuleInfo),
		io__set_exit_status(1, !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("In clause for ", !IO),
		hlds_out__write_pred_id(!.ModuleInfo, PredId, !IO),
		io__write_string(":\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  syntax error: some but not all " ++
			"arguments have mode annotations.\n", !IO),
		% apply the clause to all modes
		% XXX would it be better to apply it to none?
		ProcIds = pred_info_all_procids(PredInfo)
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

get_mode_annotations([], [], !Annotations).
get_mode_annotations([Arg0 | Args0], [Arg | Args], !Annotations) :-
	get_mode_annotation(Arg0, Arg, MaybeAnnotation),
	add_annotation(MaybeAnnotation, !Annotations),
	get_mode_annotations(Args0, Args, !Annotations).

:- pred add_annotation(maybe(mode)::in,
	mode_annotations::in, mode_annotations::out) is det.

add_annotation(no,        empty, none).
add_annotation(yes(Mode), empty, modes([Mode])).
add_annotation(no,        modes(_ `with_type` list(mode)), mixed).
add_annotation(yes(Mode), modes(Modes), modes(Modes ++ [Mode])).
add_annotation(no,        none, none).
add_annotation(yes(_),    none, mixed).
add_annotation(_,         mixed, mixed).

	% Extract the mode annotations (if any) from a single argument.
:- pred get_mode_annotation(prog_term::in, prog_term::out, maybe(mode)::out)
	is det.

get_mode_annotation(Arg0, Arg, MaybeAnnotation) :-
	(
		Arg0 = term__functor(term__atom("::"), [Arg1, ModeTerm], _),
		convert_mode(allow_constrained_inst_var,
			term__coerce(ModeTerm), Mode)
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
		!ModuleInfo, !QualInfo, !IO) :-

		% Add the body of the introduced pred

		% First the goal info
	goal_info_init(GoalInfo0),
	goal_info_set_context(GoalInfo0, Context, GoalInfo1),
	set__list_to_set(HeadVars, NonLocals),
	goal_info_set_nonlocals(GoalInfo1, NonLocals, GoalInfo2),
	( check_marker(Markers, (impure)) ->
		goal_info_add_feature(GoalInfo2, (impure), GoalInfo)
	; check_marker(Markers, (semipure)) ->
		goal_info_add_feature(GoalInfo2, (semipure), GoalInfo)
	;
		GoalInfo = GoalInfo2
	),

		% Then the goal itself
	varset__init(VarSet0),
	make_n_fresh_vars("HeadVar__", PredArity, HeadVars, VarSet0, VarSet),
	construct_pred_or_func_call(invalid_pred_id, PredOrFunc,
		InstancePredName, HeadVars, GoalInfo, IntroducedGoal,
		transform_info(!.ModuleInfo, !.QualInfo),
		transform_info(!:ModuleInfo, !:QualInfo)),
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
		!ModuleInfo, !QualInfo, !IO) :-
	clauses_info_init(PredArity, ClausesInfo0),
	list__foldl4(
		produce_instance_method_clause(PredOrFunc, Context, Status),
		InstanceClauses, !ModuleInfo, !QualInfo,
		ClausesInfo0, ClausesInfo, !IO).

:- pred produce_instance_method_clause(pred_or_func::in,
	prog_context::in, import_status::in, item::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	clauses_info::in, clauses_info::out, io::di, io::uo) is det.

produce_instance_method_clause(PredOrFunc, Context, Status, InstanceClause,
		!ModuleInfo, !QualInfo, !ClausesInfo, !IO) :-
	(
		InstanceClause = clause(CVarSet, PredOrFunc, PredName,
			HeadTerms0, Body)
	->
		(
			illegal_state_var_func_result(PredOrFunc, HeadTerms0,
				StateVar)
		->
			report_illegal_func_svar_result(Context, CVarSet,
				StateVar, !IO)
		;
			HeadTerms = expand_bang_state_var_args(HeadTerms0),
			PredArity = list__length(HeadTerms),
			adjust_func_arity(PredOrFunc, Arity, PredArity),
			% The tvarset argument is only used for explicit type
			% qualifications, of which there are none in this
			% clause, so it is set to a dummy value.
			varset__init(TVarSet0),

			ProcIds = [], % means this clause applies to _every_
					  % mode of the procedure
			GoalType = none,	% goal is not a promise
			clauses_info_add_clause(ProcIds, CVarSet, TVarSet0,
				HeadTerms, Body, Context, Status, PredOrFunc,
				Arity, GoalType, Goal, VarSet, _TVarSet,
				!ClausesInfo, Warnings, !ModuleInfo, !QualInfo,
				!IO),

			% warn about singleton variables
			maybe_warn_singletons(VarSet,
				PredOrFunc - PredName/Arity, !.ModuleInfo,
				Goal, !IO),

			% warn about variables with overlapping scopes
			maybe_warn_overlap(Warnings, VarSet,
				PredOrFunc - PredName/Arity, !IO)
		)
	;
		error("produce_clause: invalid instance item")
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

:- pred module_add_pragma_import(sym_name::in, pred_or_func::in,
	list(mode)::in, pragma_foreign_proc_attributes::in, string::in,
	import_status::in, prog_context::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

module_add_pragma_import(PredName, PredOrFunc, Modes, Attributes, C_Function,
		Status, Context, !ModuleInfo, !Info, !IO) :-
	module_info_name(!.ModuleInfo, ModuleName),
	list__length(Modes, Arity),

		%
		% print out a progress message
		%
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	( VeryVerbose = yes ->
		io__write_string("% Processing `:- pragma import' for ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity,
			!IO),
		io__write_string("...\n", !IO)
	;
		true
	),

		%
		% Lookup the pred declaration in the predicate table.
		% (If it's not there, print an error message and insert
		% a dummy declaration for the predicate.)
		%
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
	(
		predicate_table_search_pf_sym_arity(PredicateTable0,
			is_fully_qualified, PredOrFunc, PredName,
			Arity, [PredId0])
	->
		PredId = PredId0
	;
		preds_add_implicit_report_error(ModuleName, PredOrFunc,
			PredName, Arity, Status, no, Context,
			"`:- pragma import' declaration", PredId,
			!ModuleInfo, !IO)
	),
		%
		% Lookup the pred_info for this pred,
		% and check that it is valid.
		%
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable2),
	predicate_table_get_preds(PredicateTable2, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause (including
	% a `pragma import' clause) for them
	( Status = opt_imported ->
		pred_info_set_import_status(opt_imported, PredInfo0, PredInfo1)
	;
		PredInfo1 = PredInfo0
	),
	( pred_info_is_imported(PredInfo1) ->
		module_info_incr_errors(!ModuleInfo),
		prog_out__write_context(Context, !IO),
		io__write_string("Error: `:- pragma import' ", !IO),
		io__write_string("declaration for imported ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
		io__write_string(".\n", !IO)
	; pred_info_clause_goal_type(PredInfo1) ->
		module_info_incr_errors(!ModuleInfo),
		prog_out__write_context(Context, !IO),
		io__write_string("Error: `:- pragma import' declaration ", !IO),
		io__write_string("for ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
		io__write_string("\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  with preceding clauses.\n", !IO)
	;
		pred_info_update_goal_type(pragmas, PredInfo1, PredInfo2),
		%
		% add the pragma declaration to the proc_info for this procedure
		%
		pred_info_procedures(PredInfo2, Procs),
		map__to_assoc_list(Procs, ExistingProcs),
		(
			get_procedure_matching_argmodes(ExistingProcs, Modes,
				!.ModuleInfo, ProcId)
		->
			pred_add_pragma_import(PredId, ProcId, Attributes,
				C_Function, Context, PredInfo2, PredInfo,
				!ModuleInfo, !Info, !IO),
			map__det_update(Preds0, PredId, PredInfo, Preds),
			predicate_table_set_preds(Preds,
				PredicateTable2, PredicateTable),
			module_info_set_predicate_table(PredicateTable,
				!ModuleInfo)
		;
			module_info_incr_errors(!ModuleInfo),
			prog_out__write_context(Context, !IO),
			io__write_string("Error: `:- pragma import' ", !IO),
			io__write_string("declaration for undeclared mode ",
				!IO),
			io__write_string("of ", !IO),
			hlds_out__write_simple_call_id(PredOrFunc,
				PredName/Arity, !IO),
			io__write_string(".\n", !IO)
		)
	).

% pred_add_pragma_import:
%	This is a subroutine of module_add_pragma_import which adds
%	the c_code for a `pragma import' declaration to a pred_info.

:- pred pred_add_pragma_import(pred_id::in, proc_id::in,
	pragma_foreign_proc_attributes::in, string::in, prog_context::in,
	pred_info::in, pred_info::out, module_info::in, module_info::out,
	qual_info::in, qual_info::out, io::di, io::uo) is det.

pred_add_pragma_import(PredId, ProcId, Attributes, C_Function, Context,
		!PredInfo, !ModuleInfo, !Info, !IO) :-
	pred_info_procedures(!.PredInfo, Procs),
	map__lookup(Procs, ProcId, ProcInfo),
	foreign__make_pragma_import(!.PredInfo, ProcInfo, C_Function, Context,
		!.ModuleInfo, PragmaImpl, VarSet, PragmaVars, ArgTypes,
		Arity, PredOrFunc),

	%
	% lookup some information we need from the pred_info and proc_info
	%
	PredName = pred_info_name(!.PredInfo),
	PredModule = pred_info_module(!.PredInfo),
	pred_info_clauses_info(!.PredInfo, Clauses0),
	pred_info_get_purity(!.PredInfo, Purity),

	%
	% Add the code for this `pragma import' to the clauses_info
	%
	clauses_info_add_pragma_foreign_proc(Purity, Attributes,
		PredId, ProcId, VarSet, PragmaVars, ArgTypes, PragmaImpl,
		Context, PredOrFunc, qualified(PredModule, PredName),
		Arity, Clauses0, Clauses, !ModuleInfo, !IO),

	%
	% Store the clauses_info etc. back into the pred_info
	%
	pred_info_set_clauses_info(Clauses, !PredInfo).

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_foreign_proc(pragma_foreign_proc_attributes::in,
	sym_name::in, pred_or_func::in, list(pragma_var)::in, prog_varset::in,
	pragma_foreign_code_impl::in, import_status::in, prog_context::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

module_add_pragma_foreign_proc(Attributes, PredName, PredOrFunc, PVars, VarSet,
		PragmaImpl, Status, Context, !ModuleInfo, !Info, !IO) :-
	module_info_name(!.ModuleInfo, ModuleName),
	PragmaForeignLanguage = foreign_language(Attributes),
	list__length(PVars, Arity),
		% print out a progress message
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	( VeryVerbose = yes ->
		io__write_string("% Processing `:- pragma foreign_proc' for ",
			!IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity,
			!IO),
		io__write_string("...\n", !IO)
	;
		true
	),

	globals__io_get_backend_foreign_languages(BackendForeignLangs, !IO),

		% Lookup the pred declaration in the predicate table.
		% (If it's not there, print an error message and insert
		% a dummy declaration for the predicate.)
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
	(
		predicate_table_search_pf_sym_arity(PredicateTable0,
			is_fully_qualified, PredOrFunc, PredName,
			Arity, [PredId0])
	->
		PredId = PredId0
	;
		preds_add_implicit_report_error(ModuleName, PredOrFunc,
			PredName, Arity, Status, no, Context,
			"`:- pragma foreign_proc' declaration",
			PredId, !ModuleInfo, !IO)
	),
		% Lookup the pred_info for this pred,
		% add the pragma to the proc_info in the proc_table in the
		% pred_info, and save the pred_info.
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable1),
	predicate_table_get_preds(PredicateTable1, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	% opt_imported preds are initially tagged as imported and are
	% tagged as opt_imported only if/when we see a clause (including
	% a `pragma c_code' clause) for them
	( Status = opt_imported ->
		pred_info_set_import_status(opt_imported,
			PredInfo0, PredInfo1a)
	;
		PredInfo1a = PredInfo0
	),
	(
		% If this procedure was previously defined as clauses only
		% then we need to turn all the non mode-specific clauses
		% into mode-specific clauses.
		pred_info_clause_goal_type(PredInfo1a)
	->
		pred_info_clauses_info(PredInfo1a, CInfo0),
		clauses_info_clauses(CInfo0, ClauseList0),
		ClauseList = list__map(
			(func(C) =
				( C = clause([], Goal, mercury, Ctxt) ->
					clause(AllProcIds, Goal, mercury, Ctxt)
				;
					C
				) :-
				AllProcIds = pred_info_all_procids(PredInfo1a)
			), ClauseList0),
		clauses_info_set_clauses(ClauseList, CInfo0, CInfo),
		pred_info_set_clauses_info(CInfo, PredInfo1a, PredInfo1)
	;
		PredInfo1 = PredInfo1a
	),
	(
		pred_info_is_imported(PredInfo1)
	->
		module_info_incr_errors(!ModuleInfo),
		prog_out__write_context(Context, !IO),
		io__write_string("Error: `:- pragma foreign_proc' " ++
			"(or `pragma c_code')\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("declaration for imported ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
		io__write_string(".\n", !IO)
	;
			% Don't add clauses for foreign languages other
			% than the ones we can generate code for.
		not list__member(PragmaForeignLanguage, BackendForeignLangs)
	->
		pred_info_update_goal_type(pragmas, PredInfo0, PredInfo),
		module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
	;
		% add the pragma declaration to the proc_info for this procedure
		pred_info_procedures(PredInfo1, Procs),
		map__to_assoc_list(Procs, ExistingProcs),
		pragma_get_modes(PVars, Modes),
		(
			get_procedure_matching_argmodes(ExistingProcs, Modes,
				!.ModuleInfo, ProcId)
		->
			pred_info_clauses_info(PredInfo1, Clauses0),

			pred_info_arg_types(PredInfo1, ArgTypes),
			pred_info_get_purity(PredInfo1, Purity),
			clauses_info_add_pragma_foreign_proc(Purity,
				Attributes, PredId, ProcId, VarSet, PVars,
				ArgTypes, PragmaImpl, Context, PredOrFunc,
				PredName, Arity, Clauses0, Clauses,
				!ModuleInfo, !IO),
			pred_info_set_clauses_info(Clauses,
				PredInfo1, PredInfo2),
			pred_info_update_goal_type(pragmas,
				PredInfo2, PredInfo),
			map__det_update(Preds0, PredId, PredInfo, Preds),
			predicate_table_set_preds(Preds,
				PredicateTable1, PredicateTable),
			module_info_set_predicate_table(PredicateTable,
				!ModuleInfo),
			pragma_get_var_infos(PVars, ArgInfo),
			maybe_warn_pragma_singletons(PragmaImpl,
				PragmaForeignLanguage, ArgInfo,
				Context, PredOrFunc - PredName/Arity,
				!.ModuleInfo, !IO)
		;
			module_info_incr_errors(!ModuleInfo),
			prog_out__write_context(Context, !IO),
			io__write_string("Error: `:- pragma foreign_proc' ",
				!IO),
			io__write_string("declaration for undeclared mode ",
				!IO),
			io__write_string("of ", !IO),
			hlds_out__write_simple_call_id(PredOrFunc,
				PredName/Arity, !IO),
			io__write_string(".\n", !IO)
		)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_pragma_tabled(eval_method::in, sym_name::in, int::in,
	maybe(pred_or_func)::in, maybe(list(mode))::in, import_status::in,
	prog_context::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

module_add_pragma_tabled(EvalMethod, PredName, Arity, MaybePredOrFunc,
		MaybeModes, Status, Context, !ModuleInfo, !IO) :-
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
	EvalMethodS = eval_method_to_string(EvalMethod),

	% Find out if we are tabling a predicate or a function
	(
		MaybePredOrFunc = yes(PredOrFunc0)
	->
		PredOrFunc = PredOrFunc0,

			% Lookup the pred declaration in the predicate table.
			% (If it's not there, print an error message and insert
			% a dummy declaration for the predicate.)
		(
			predicate_table_search_pf_sym_arity(PredicateTable0,
				is_fully_qualified, PredOrFunc,
				PredName, Arity, PredIds0)
		->
			PredIds = PredIds0
		;
			module_info_name(!.ModuleInfo, ModuleName),
			string__format("`:- pragma %s' declaration",
				[s(EvalMethodS)], Message1),

			preds_add_implicit_report_error(ModuleName, PredOrFunc,
				PredName, Arity, Status, no, Context,
				Message1, PredId, !ModuleInfo, !IO),
			PredIds = [PredId]
		)
	;
		(
			predicate_table_search_sym_arity(PredicateTable0,
				is_fully_qualified, PredName,
				Arity, PredIds0)
		->
			PredIds = PredIds0
		;
			module_info_name(!.ModuleInfo, ModuleName),
			string__format("`:- pragma %s' declaration",
				[s(EvalMethodS)], Message1),

			preds_add_implicit_report_error(ModuleName,
				predicate, PredName, Arity, Status, no,
				Context, Message1, PredId,
				!ModuleInfo, !IO),
			PredIds = [PredId]
		)
	),
	list__foldl2(module_add_pragma_tabled_2(EvalMethod, PredName,
		Arity, MaybePredOrFunc, MaybeModes, Context),
		PredIds, !ModuleInfo, !IO).

:- pred module_add_pragma_tabled_2(eval_method::in, sym_name::in, int::in,
	maybe(pred_or_func)::in, maybe(list(mode))::in, prog_context::in,
	pred_id::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

module_add_pragma_tabled_2(EvalMethod0, PredName, Arity0, MaybePredOrFunc,
		MaybeModes, Context, PredId, !ModuleInfo, !IO) :-

	( EvalMethod0 = eval_minimal(_) ->
		globals__io_lookup_bool_option(use_minimal_model_own_stacks,
			OwnStacks, !IO),
		(
			OwnStacks = yes,
			EvalMethod = eval_minimal(own_stacks)
		;
			OwnStacks = no,
			EvalMethod = eval_minimal(stack_copy)
		)
	;
		EvalMethod = EvalMethod0
	),

		% Lookup the pred_info for this pred,
	module_info_get_predicate_table(!.ModuleInfo, PredicateTable),
	predicate_table_get_preds(PredicateTable, Preds),
	map__lookup(Preds, PredId, PredInfo0),

	% Find out if we are tabling a predicate or a function
	(
		MaybePredOrFunc = yes(PredOrFunc0),
		PredOrFunc = PredOrFunc0
	;
		MaybePredOrFunc = no,
		PredOrFunc = pred_info_is_pred_or_func(PredInfo0)
	),
	adjust_func_arity(PredOrFunc, Arity0, Arity),

		% print out a progress message
	EvalMethodS = eval_method_to_string(EvalMethod),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
	( VeryVerbose = yes ->
		io__write_string("% Processing `:- pragma ", !IO),
		io__write_string(EvalMethodS, !IO),
		io__write_string("' for ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
		io__write_string("...\n", !IO)
	;
		true
	),

	% Issue a warning if this predicate/function has a pragma inline
	% declaration.  Tabled procedures cannot be inlined.
	pred_info_get_markers(PredInfo0, Markers),
	globals.io_lookup_bool_option(warn_table_with_inline, WarnInline, !IO),	
	( check_marker(Markers, inline), WarnInline = yes ->
		PredNameStr = hlds_out.simple_call_id_to_string(PredOrFunc,
			PredName/Arity),
		TablePragmaStr = string.format("`:- pragma %s'",
			[s(EvalMethodS)]),
		InlineWarning = [
			words("Warning: "), fixed(PredNameStr), 
			words("has a"), nl, fixed(TablePragmaStr),
			words("declaration but also has a"),
			fixed("`:- pragma inline'"),
			words("declaration."), nl,
			words("This inline pragma will be ignored"),
			words("since tabled predicates cannot be inlined."), nl,
			words("You can use the"),
			fixed("`--no-warn-table-with-inline'"),
			words("option to suppress this warning.")
		],
		error_util.report_warning(Context, 0, InlineWarning, !IO)
	;
		true
	),
	( pred_info_is_imported(PredInfo0) ->
		module_info_incr_errors(!ModuleInfo),
		prog_out__write_context(Context, !IO),
		io__write_string("Error: `:- pragma ", !IO),
		io__write_string(EvalMethodS, !IO),
		io__write_string("' declaration for imported ", !IO),
		hlds_out__write_simple_call_id(PredOrFunc, PredName/Arity, !IO),
		io__write_string(".\n", !IO)
	;
		% do we have to make sure the tabled preds are stratified?
		(
			eval_method_needs_stratification(EvalMethod) = yes
		->
			module_info_stratified_preds(!.ModuleInfo,
				StratPredIds0),
			set__insert(StratPredIds0, PredId, StratPredIds),
			module_info_set_stratified_preds(StratPredIds,
				!ModuleInfo)
		;
			true
		),

		% add the eval model to the proc_info for this procedure
		pred_info_procedures(PredInfo0, Procs0),
		map__to_assoc_list(Procs0, ExistingProcs),
		( MaybeModes = yes(Modes) ->
			(
				get_procedure_matching_argmodes(ExistingProcs,
					Modes, !.ModuleInfo, ProcId)
			->
				map__lookup(Procs0, ProcId, ProcInfo0),
				proc_info_set_eval_method(EvalMethod,
					ProcInfo0, ProcInfo),
				map__det_update(Procs0, ProcId, ProcInfo,
					Procs),
				pred_info_set_procedures(Procs,
					PredInfo0, PredInfo),
				module_info_set_pred_info(PredId, PredInfo,
					!ModuleInfo)
			;
				module_info_incr_errors(!ModuleInfo),
				prog_out__write_context(Context, !IO),
				io__write_string("Error: `:- pragma ", !IO),
				io__write_string(EvalMethodS, !IO),
				io__write_string("' declaration for " ++
					"undeclared mode of ", !IO),
				hlds_out__write_simple_call_id(PredOrFunc,
					PredName/Arity, !IO),
				io__write_string(".\n", !IO)
			)
		; ExistingProcs = [] ->
			module_info_incr_errors(!ModuleInfo),
			prog_out__write_context(Context, !IO),
			io__write_string("Error: `:- pragma ", !IO),
			io__write_string(EvalMethodS, !IO),
			io__write_string("' declaration for\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  ", !IO),
			hlds_out__write_simple_call_id(PredOrFunc,
				PredName/Arity, !IO),
			io__write_string(" with no declared modes.\n", !IO)
		;
			set_eval_method_list(ExistingProcs, Context,
				PredOrFunc, PredName/Arity, EvalMethod,
				Procs0, Procs, !ModuleInfo, !IO),
			pred_info_set_procedures(Procs,
				PredInfo0, PredInfo),
			module_info_set_pred_info(PredId, PredInfo,
				!ModuleInfo)
		)
	).

:- pred set_eval_method_list(assoc_list(proc_id, proc_info)::in,
	prog_context::in, pred_or_func::in, sym_name_and_arity::in,
	eval_method::in, proc_table::in, proc_table::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

set_eval_method_list([], _, _, _, _, !Procs, !ModuleInfo, !IO).
set_eval_method_list([ProcId - ProcInfo0 | Rest], Context, PredOrFunc,
		PredNameAndArity, EvalMethod, !Procs, !ModuleInfo, !IO) :-
	proc_info_eval_method(ProcInfo0, OldEvalMethod),
	% NOTE: We don't bother detecting multiple tabling pragmas
	% of the same type here.
	( 
		OldEvalMethod \= eval_normal,
		OldEvalMethod \= EvalMethod
	->
		% If there are conflicting tabling pragmas then
		% emit an error message and do not bother changing 
		% the evaluation method.
		OldEvalMethodStr = eval_method_to_string(OldEvalMethod),
		EvalMethodStr = eval_method_to_string(EvalMethod),
		Name = hlds_out.simple_call_id_to_string(PredOrFunc,
			PredNameAndArity),
		ErrorMsg = [
			words("Error:"),
			fixed(Name),
			words("has both"),
			fixed(OldEvalMethodStr),
			words("and"),
			fixed(EvalMethodStr),
			words("pragmas specified."),
			words("Only one kind of"),
			words("tabling pragma may be applied to it.")
		],
		module_info_incr_errors(!ModuleInfo),
		error_util.write_error_pieces(Context, 0, ErrorMsg, !IO)
	;
		proc_info_set_eval_method(EvalMethod, ProcInfo0, ProcInfo),
		map__det_update(!.Procs, ProcId, ProcInfo, !:Procs)
	),
	set_eval_method_list(Rest, Context, PredOrFunc, PredNameAndArity,
		EvalMethod, !Procs, !ModuleInfo, !IO).

%-----------------------------------------------------------------------------%

	% from the list of pragma_vars extract the modes.
:- pred pragma_get_modes(list(pragma_var)::in, list(mode)::out) is det.

pragma_get_modes([], []).
pragma_get_modes([PragmaVar | Vars], [Mode | Modes]) :-
	PragmaVar = pragma_var(_Var, _Name, Mode),
	pragma_get_modes(Vars, Modes).

%-----------------------------------------------------------------------------%

	% from the list of pragma_vars , extract the vars.
:- pred pragma_get_vars(list(pragma_var)::in, list(prog_var)::out) is det.

pragma_get_vars([], []).
pragma_get_vars([PragmaVar | PragmaVars], [Var | Vars]) :-
	PragmaVar = pragma_var(Var, _Name, _Mode),
	pragma_get_vars(PragmaVars, Vars).

%---------------------------------------------------------------------------%

	% from the list of pragma_vars, extract the names.

:- pred pragma_get_var_infos(list(pragma_var)::in,
	list(maybe(pair(string, mode)))::out) is det.

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

:- pred pragma_check_markers(pred_table::in, list(pred_id)::in,
	list(marker)::in, bool::out) is det.

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

:- pred pragma_add_marker(list(pred_id)::in,
	add_marker_pred_info::in(add_marker_pred_info), import_status::in,
	bool::in, pred_table::in, pred_table::out, bool::out) is det.

pragma_add_marker([], _, _, _, !PredTable, no).
pragma_add_marker([PredId | PredIds], UpdatePredInfo, Status, MustBeExported,
		!PredTable, WrongStatus) :-
	map__lookup(!.PredTable, PredId, PredInfo0),
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
	map__det_update(!.PredTable, PredId, PredInfo, !:PredTable),
	pragma_add_marker(PredIds, UpdatePredInfo, Status,
		MustBeExported, !PredTable, WrongStatus1),
	bool__or(WrongStatus0, WrongStatus1, WrongStatus).

:- pred add_marker_pred_info(marker::in, pred_info::in, pred_info::out)
	is det.

add_marker_pred_info(Marker, !PredInfo) :-
	pred_info_get_markers(!.PredInfo, Markers0),
	add_marker(Marker, Markers0, Markers),
	pred_info_set_markers(Markers, !PredInfo).

	% Succeed if a marker for an exported procedure must also
	% be exported.
:- pred marker_must_be_exported(marker::in) is semidet.

marker_must_be_exported(aditi).
marker_must_be_exported(base_relation).

%---------------------------------------------------------------------------%

	% Find the procedure with argmodes which match the ones we want.

:- pred get_procedure_matching_argmodes(assoc_list(proc_id, proc_info)::in,
	list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes(Procs, Modes0, ModuleInfo, ProcId) :-
	list__map(constrain_inst_vars_in_mode, Modes0, Modes),
	get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo, ProcId).

:- pred get_procedure_matching_argmodes_2(assoc_list(proc_id, proc_info)::in,
	list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_argmodes_2([P|Procs], Modes, ModuleInfo, OurProcId) :-
	P = ProcId - ProcInfo,
	proc_info_argmodes(ProcInfo, ArgModes),
	( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
		OurProcId = ProcId
	;
		get_procedure_matching_argmodes_2(Procs, Modes, ModuleInfo,
			OurProcId)
	).

	% Find the procedure with declared argmodes which match the ones
	% we want.  If there was no mode declaration, then use the inferred
	% argmodes.
:- pred get_procedure_matching_declmodes(assoc_list(proc_id, proc_info)::in,
	list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_declmodes(Procs, Modes0, ModuleInfo, ProcId) :-
	list__map(constrain_inst_vars_in_mode, Modes0, Modes),
	get_procedure_matching_declmodes_2(Procs, Modes, ModuleInfo, ProcId).

:- pred get_procedure_matching_declmodes_2(assoc_list(proc_id, proc_info)::in,
	list(mode)::in, module_info::in, proc_id::out) is semidet.

get_procedure_matching_declmodes_2([P|Procs], Modes, ModuleInfo, OurProcId) :-
	P = ProcId - ProcInfo,
	proc_info_declared_argmodes(ProcInfo, ArgModes),
	( mode_list_matches(Modes, ArgModes, ModuleInfo) ->
		OurProcId = ProcId
	;
		get_procedure_matching_declmodes_2(Procs, Modes, ModuleInfo,
			OurProcId)
	).

:- pred mode_list_matches(list(mode)::in, list(mode)::in, module_info::in)
	is semidet.

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
:- pred maybe_warn_overlap(list(quant_warning)::in, prog_varset::in,
	simple_call_id::in, io::di, io::uo) is det.

maybe_warn_overlap(Warnings, VarSet, PredCallId) -->
	globals__io_lookup_bool_option(warn_overlapping_scopes,
		WarnOverlappingScopes),
	( { WarnOverlappingScopes = yes } ->
		warn_overlap(Warnings, VarSet, PredCallId)
	;
		[]
	).

:- pred warn_overlap(list(quant_warning)::in, prog_varset::in,
	simple_call_id::in, io::di, io::uo) is det.

warn_overlap([], _, _) --> [].
warn_overlap([Warn|Warns], VarSet, PredCallId) -->
	{ Warn = warn_overlap(Vars, Context) },
	prog_out__write_context(Context),
	io__write_string("In clause for "),
	hlds_out__write_simple_call_id(PredCallId),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	( { Vars = [Var] } ->
		io__write_string("  warning: variable `"),
		mercury_output_var(Var, VarSet, no),
		report_warning("' has overlapping scopes.\n")
	;
		io__write_string("  warning: variables `"),
		mercury_output_vars(Vars, VarSet, no),
		report_warning("'\n"),
		prog_out__write_context(Context),
		report_warning("  each have overlapping scopes.\n")
	),
	warn_overlap(Warns, VarSet, PredCallId).

%-----------------------------------------------------------------------------%

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once, or about variables that do not occur in
	% C code strings when they should.
	%
:- pred maybe_warn_singletons(prog_varset::in, simple_call_id::in,
	module_info::in, hlds_goal::in, io::di, io::uo) is det.

maybe_warn_singletons(VarSet, PredCallId, ModuleInfo, Body) -->
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars),
	( { WarnSingletonVars = yes } ->
		{ set__init(QuantVars) },
		warn_singletons_in_goal(Body, QuantVars, VarSet, PredCallId,
			ModuleInfo)
	;
		[]
	).

:- pred warn_singletons_in_goal(hlds_goal::in, set(prog_var)::in,
	prog_varset::in, simple_call_id::in, module_info::in,
	io::di, io::uo) is det.

warn_singletons_in_goal(Goal - GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_2(Goal, GoalInfo, QuantVars, VarSet,
		PredCallId, MI).

:- pred warn_singletons_in_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	set(prog_var)::in, prog_varset::in, simple_call_id::in,
	module_info::in, io::di, io::uo) is det.

warn_singletons_in_goal_2(conj(Goals), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(par_conj(Goals), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(disj(Goals), _GoalInfo, QuantVars, VarSet,
		PredCallId, MI) -->
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(switch(_Var, _CanFail, Cases),
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
		warn_singletons(Vars, GoalInfo, EmptySet, SubGoalVars, VarSet,
			Context, PredCallId)
	;
		[]
	),
	{ set__insert_list(QuantVars, Vars, QuantVars1) },
	warn_singletons_in_goal(SubGoal, QuantVars1, VarSet, PredCallId, MI).

warn_singletons_in_goal_2(if_then_else(Vars, Cond, Then, Else), GoalInfo,
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
		warn_singletons(Vars, GoalInfo, EmptySet, CondThenVars, VarSet,
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
	warn_singletons(Args, GoalInfo, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(generic_call(GenericCall, Args0, _, _),
		GoalInfo, QuantVars, VarSet, PredCallId, _) -->
	{ goal_util__generic_call_vars(GenericCall, Args1) },
	{ list__append(Args0, Args1, Args) },
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(Args, GoalInfo, NonLocals, QuantVars, VarSet, Context,
		PredCallId).

warn_singletons_in_goal_2(unify(Var, RHS, _, _, _),
		GoalInfo, QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_unify(Var, RHS, GoalInfo, QuantVars, VarSet,
		PredCallId, MI).

warn_singletons_in_goal_2(foreign_proc(Attrs, _, _, Args, _, PragmaImpl),
		GoalInfo, _QuantVars, _VarSet, PredCallId, MI) -->
	{ goal_info_get_context(GoalInfo, Context) },
	{ Lang = foreign_language(Attrs) },
	{ NamesModes = list__map(foreign_arg_maybe_name_mode, Args) },
	warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang,
		NamesModes, Context, PredCallId, MI).

warn_singletons_in_goal_2(shorthand(ShorthandGoal), GoalInfo, QuantVars,
		VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_2_shorthand(ShorthandGoal, GoalInfo,
		QuantVars, VarSet, PredCallId, MI).

:- pred warn_singletons_in_goal_2_shorthand(shorthand_goal_expr::in,
	hlds_goal_info::in, set(prog_var)::in, prog_varset::in,
	simple_call_id::in, module_info::in, io::di, io::uo) is det.

warn_singletons_in_goal_2_shorthand(bi_implication(LHS, RHS), _GoalInfo,
		QuantVars, VarSet, PredCallId, MI) -->
	warn_singletons_in_goal_list([LHS, RHS], QuantVars, VarSet,
		PredCallId, MI).

:- pred warn_singletons_in_goal_list(list(hlds_goal)::in, set(prog_var)::in,
	prog_varset::in, simple_call_id::in, module_info::in,
	io::di, io::uo) is det.

warn_singletons_in_goal_list([], _, _, _, _) --> [].
warn_singletons_in_goal_list([Goal|Goals], QuantVars, VarSet, CallPredId, MI)
		-->
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI),
	warn_singletons_in_goal_list(Goals, QuantVars, VarSet, CallPredId, MI).

:- pred warn_singletons_in_cases(list(case)::in, set(prog_var)::in,
	prog_varset::in, simple_call_id::in, module_info::in,
	io::di, io::uo) is det.

warn_singletons_in_cases([], _, _, _, _) --> [].
warn_singletons_in_cases([Case|Cases], QuantVars, VarSet, CallPredId, MI) -->
	{ Case = case(_ConsId, Goal) },
	warn_singletons_in_goal(Goal, QuantVars, VarSet, CallPredId, MI),
	warn_singletons_in_cases(Cases, QuantVars, VarSet, CallPredId, MI).

:- pred warn_singletons_in_unify(prog_var::in, unify_rhs::in,
	hlds_goal_info::in, set(prog_var)::in, prog_varset::in,
	simple_call_id::in, module_info::in, io::di, io::uo) is det.

warn_singletons_in_unify(X, var(Y), GoalInfo, QuantVars, VarSet, CallPredId, _)
		-->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons([X, Y], GoalInfo, NonLocals, QuantVars, VarSet,
		Context, CallPredId).

warn_singletons_in_unify(X, functor(_ConsId, _, Vars), GoalInfo,
		QuantVars, VarSet, CallPredId, _) -->
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons([X | Vars], GoalInfo, NonLocals, QuantVars, VarSet,
		Context, CallPredId).

warn_singletons_in_unify(X, lambda_goal(_Purity, _PredOrFunc, _Eval, _Fix,
		_NonLocals, LambdaVars, _Modes, _Det, LambdaGoal),
		GoalInfo, QuantVars, VarSet, CallPredId, MI) -->
	%
	% warn if any lambda-quantified variables occur only in the quantifier
	%
	{ LambdaGoal = _ - LambdaGoalInfo },
	{ goal_info_get_nonlocals(LambdaGoalInfo, LambdaNonLocals) },
	{ goal_info_get_context(GoalInfo, Context) },
	warn_singletons(LambdaVars, GoalInfo, LambdaNonLocals, QuantVars,
		VarSet, Context, CallPredId),

	%
	% warn if X (the variable we're unifying the lambda expression with)
	% is singleton
	%
	{ goal_info_get_nonlocals(GoalInfo, NonLocals) },
	warn_singletons([X], GoalInfo, NonLocals, QuantVars, VarSet, Context,
		CallPredId),

	%
	% warn if the lambda-goal contains singletons
	%
	warn_singletons_in_goal(LambdaGoal, QuantVars, VarSet, CallPredId, MI).

%-----------------------------------------------------------------------------%

:- pred maybe_warn_pragma_singletons(pragma_foreign_code_impl::in,
	foreign_language::in, list(maybe(pair(string, mode)))::in,
	prog_context::in, simple_call_id::in, module_info::in,
	io::di, io::uo) is det.

maybe_warn_pragma_singletons(PragmaImpl, Lang, ArgInfo, Context, CallId, MI,
		!IO) :-
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars,
		!IO),
	( WarnSingletonVars = yes ->
		warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang,
			ArgInfo, Context, CallId, MI, !IO)
	;
		true
	).

	% warn_singletons_in_pragma_foreign_proc checks to see if each
	% variable is mentioned at least once in the foreign code
	% fragments that ought to mention it. If not, it gives a
	% warning.
	% (Note that for some foreign languages it might not be
	% appropriate to do this check, or you may need to add a
	% transformation to map Mercury variable names into identifiers
	% for that foreign language).
:- pred warn_singletons_in_pragma_foreign_proc(pragma_foreign_code_impl::in,
	foreign_language::in, list(maybe(pair(string, mode)))::in,
	prog_context::in, simple_call_id::in, module_info::in,
	io::di, io::uo) is det.

warn_singletons_in_pragma_foreign_proc(PragmaImpl, Lang, Args, Context,
		PredOrFuncCallId, ModuleInfo, !IO) :-
	LangStr = foreign_language_string(Lang),
	(
		PragmaImpl = ordinary(C_Code, _),
		c_code_to_name_list(C_Code, C_CodeList),
		Filter = (pred(Name::out) is nondet :-
			list__member(yes(Name - _), Args),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, C_CodeList)
		),
		solutions(Filter, UnmentionedVars),
		( UnmentionedVars = [] ->
			true
		;
			prog_out__write_context(Context, !IO),
			io__write_string("In the " ++ LangStr ++ " code for ",
			!IO),
			hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
			io__write_string(":\n", !IO),
			prog_out__write_context(Context, !IO),
			write_variable_warning_start(UnmentionedVars, !IO),
			io__write_string("not occur in the " ++
				LangStr ++ " code.\n", !IO)
		)
	;
		PragmaImpl = nondet(_, _, FirstCode, _,
			LaterCode, _, _, SharedCode, _),
		c_code_to_name_list(FirstCode, FirstCodeList),
		c_code_to_name_list(LaterCode, LaterCodeList),
		c_code_to_name_list(SharedCode, SharedCodeList),
		InputFilter = (pred(Name::out) is nondet :-
			list__member(yes(Name - Mode), Args),
				mode_is_input(ModuleInfo, Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, FirstCodeList)
		),
		solutions(InputFilter, UnmentionedInputVars),
		( UnmentionedInputVars = [] ->
			true
		;
			prog_out__write_context(Context, !IO),
			io__write_string("In the " ++ LangStr ++ " code for ",
				!IO),
			hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
			io__write_string(":\n", !IO),
			prog_out__write_context(Context, !IO),
			write_variable_warning_start(UnmentionedInputVars, !IO),
			io__write_string("not occur in the first " ++
				LangStr ++ " code.\n ", !IO)
		),
		FirstOutputFilter = (pred(Name::out) is nondet :-
			list__member(yes(Name - Mode), Args),
				mode_is_output(ModuleInfo, Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, FirstCodeList),
				\+ list__member(Name, SharedCodeList)
		),
		solutions(FirstOutputFilter, UnmentionedFirstOutputVars),
		( UnmentionedFirstOutputVars = [] ->
			true
		;
			prog_out__write_context(Context, !IO),
			io__write_string("In the " ++ LangStr ++ " code for ",
				!IO),
			hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
			io__write_string(":\n", !IO),
			prog_out__write_context(Context, !IO),
			write_variable_warning_start(
				UnmentionedFirstOutputVars, !IO),
			io__write_string("not occur in the first " ++
				LangStr ++ " code or the shared " ++ LangStr ++
				" code.\n ", !IO)
		),
		LaterOutputFilter = (pred(Name::out) is nondet :-
			list__member(yes(Name - Mode), Args),
				mode_is_output(ModuleInfo, Mode),
				\+ string__prefix(Name, "_"),
				\+ list__member(Name, LaterCodeList),
				\+ list__member(Name, SharedCodeList)
		),
		solutions(LaterOutputFilter, UnmentionedLaterOutputVars),
		( UnmentionedLaterOutputVars = [] ->
			true
		;
			prog_out__write_context(Context, !IO),
			io__write_string("In the " ++ LangStr ++ " code for ",
				!IO),
			hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
			io__write_string(":\n", !IO),
			prog_out__write_context(Context, !IO),
			write_variable_warning_start(
				UnmentionedLaterOutputVars, !IO),
			io__write_string("not occur in the retry " ++
				LangStr ++ " code or the shared " ++ LangStr ++
				" code.\n ", !IO)
		)
	;
		PragmaImpl = import(_, _, _, _)
	).

:- pred write_variable_warning_start(list(string)::in, io::di, io::uo) is det.

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
:- pred c_code_to_name_list(string::in, list(string)::out) is det.

c_code_to_name_list(Code, List) :-
	string__to_char_list(Code, CharList),
	c_code_to_name_list_2(CharList, List).

:- pred c_code_to_name_list_2(list(char)::in, list(string)::out) is det.

c_code_to_name_list_2(C_Code, List) :-
	get_first_c_name(C_Code, NameCharList, TheRest),
	( NameCharList = [] ->
		% no names left
		List = []
	;
		c_code_to_name_list_2(TheRest, Names),
		string__from_char_list(NameCharList, Name),
		List = [Name|Names]
	).

:- pred get_first_c_name(list(char)::in, list(char)::out, list(char)::out)
	is det.

get_first_c_name([], [], []).
get_first_c_name([C | CodeChars], NameCharList, TheRest) :-
	( char__is_alnum_or_underscore(C) ->
		get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
		NameCharList = [C | NameCharList0]
	;
			% strip off any characters in the C code which
			% don't form part of an identifier.
		get_first_c_name(CodeChars, NameCharList, TheRest)
	).

:- pred get_first_c_name_in_word(list(char)::in, list(char)::out,
	list(char)::out) is det.

get_first_c_name_in_word([], [], []).
get_first_c_name_in_word([C | CodeChars], NameCharList, TheRest) :-
	( char__is_alnum_or_underscore(C) ->
			% There are more characters in the word
		get_first_c_name_in_word(CodeChars, NameCharList0, TheRest),
		NameCharList = [C|NameCharList0]
	;
			% The word is finished
		NameCharList = [],
		TheRest = CodeChars
	).

%-----------------------------------------------------------------------------%

:- pred write_string_list(list(string)::in, io::di, io::uo) is det.

write_string_list([], !IO).
write_string_list([X | Xs], !IO) :-
	io__write_string(X, !IO),
	(
		Xs = []
	;
		Xs = [_ | _],
		io__write_string(", ", !IO),
		write_string_list(Xs, !IO)
	).

%-----------------------------------------------------------------------------%

	% warn_singletons(Vars, GoalInfo, NonLocals, QuantVars, ...):
	% 	Warn if any of the non-underscore variables in Vars don't
	%	occur in NonLocals and don't have the same name as any variable
	%	in QuantVars, or if any of the underscore variables
	%	in Vars do occur in NonLocals.
	%	Omit the warning if GoalInfo says we should.

:- pred warn_singletons(list(prog_var)::in, hlds_goal_info::in,
	set(prog_var)::in, set(prog_var)::in, prog_varset::in,
	prog_context::in, simple_call_id::in, io::di, io::uo) is det.

warn_singletons(GoalVars, GoalInfo, NonLocals, QuantVars, VarSet, Context,
		PredOrFuncCallId, !IO) :-
	% find all the variables in the goal that don't occur outside the
	% goal (i.e. are singleton), have a variable name that doesn't
	% start with "_" or "DCG_", and don't have the same name as any
	% variable in QuantVars (i.e. weren't explicitly quantified).

	solutions((pred(Var::out) is nondet :-
		  	list__member(Var, GoalVars),
			\+ set__member(Var, NonLocals),
			varset__search_name(VarSet, Var, Name),
			\+ string__prefix(Name, "_"),
			\+ string__prefix(Name, "DCG_"),
			\+ (
				set__member(QuantVar, QuantVars),
				varset__search_name(VarSet, QuantVar, Name)
			)
		), SingletonVars),

	% if there were any such variables, issue a warning

	(
		(
			SingletonVars = []
		;
			goal_info_has_feature(GoalInfo, dont_warn_singleton)
		)
	->
		true
	;
		prog_out__write_context(Context, !IO),
		io__write_string("In clause for ", !IO),
		hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
		io__write_string(":\n", !IO),
		prog_out__write_context(Context, !IO),
		( SingletonVars = [_] ->
			io__write_string("  warning: variable `", !IO),
			mercury_output_vars(SingletonVars, VarSet, no, !IO),
			report_warning("' occurs only once in this scope.\n",
				!IO)
		;
			io__write_string("  warning: variables `", !IO),
			mercury_output_vars(SingletonVars, VarSet, no, !IO),
			report_warning("' occur only once in this scope.\n",
				!IO)
		)
	),

	% Find all the variables in the goal that do occur outside the goal
	% (i.e. are not singleton) and have a variable name that starts
	% with "_". If there were any such variables, issue a warning.

	solutions((pred(Var2::out) is nondet :-
		  	list__member(Var2, GoalVars),
			set__member(Var2, NonLocals),
			varset__search_name(VarSet, Var2, Name2),
			string__prefix(Name2, "_")
		), MultiVars),
	(
		MultiVars = []
	;
		MultiVars = [_ | _],
		prog_out__write_context(Context, !IO),
		io__write_string("In clause for ", !IO),
		hlds_out__write_simple_call_id(PredOrFuncCallId, !IO),
		io__write_string(":\n", !IO),
		prog_out__write_context(Context, !IO),
		( MultiVars = [_] ->
			io__write_string("  warning: variable `", !IO),
			mercury_output_vars(MultiVars, VarSet, no, !IO),
			report_warning("' occurs more than once " ++
				"in this scope.\n", !IO)
		;
			io__write_string("  warning: variables `", !IO),
			mercury_output_vars(MultiVars, VarSet, no, !IO),
			report_warning(
				"' occur more than once in this scope.\n", !IO)
		)
	).

%-----------------------------------------------------------------------------

:- pred clauses_info_init_for_assertion(prog_vars::in, clauses_info::out)
	is det.

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
	make_n_fresh_vars("HeadVar__", Arity, HeadVars, VarSet0, VarSet),
	map__init(TI_VarMap),
	map__init(TCI_VarMap),
	HasForeignClauses = no,
	ClausesInfo = clauses_info(VarSet, VarTypes, TVarNameMap, VarTypes,
		HeadVars, [], TI_VarMap, TCI_VarMap, HasForeignClauses).

:- pred clauses_info_add_clause(list(proc_id)::in,
	prog_varset::in, tvarset::in, list(prog_term)::in, goal::in,
	prog_context::in, import_status::in, pred_or_func::in, arity::in,
	goal_type::in, hlds_goal::out, prog_varset::out, tvarset::out,
	clauses_info::in, clauses_info::out, list(quant_warning)::out,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

clauses_info_add_clause(ModeIds0, CVarSet, TVarSet0, Args, Body, Context,
		Status, PredOrFunc, Arity, GoalType, Goal, VarSet, TVarSet,
		!ClausesInfo, Warnings, !Module, !Info, !IO) :-
	!.ClausesInfo = clauses_info(VarSet0, ExplicitVarTypes0,
		TVarNameMap0, InferredVarTypes, HeadVars, ClauseList0,
		TI_VarMap, TCI_VarMap, HasForeignClauses),
	( ClauseList0 = [] ->
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
	),
	update_qual_info(TVarNameMap, TVarSet0, ExplicitVarTypes0, Status,
		!Info),
	varset__merge_subst(VarSet0, CVarSet, VarSet1, Subst),
	transform(Subst, HeadVars, Args, Body, Context, PredOrFunc,
		Arity, GoalType, Goal0, VarSet1, VarSet, Warnings,
		transform_info(!.Module, !.Info),
		transform_info(!:Module, !:Info), !IO),
	TVarSet = !.Info ^ tvarset ,
	qual_info_get_found_syntax_error(!.Info, FoundError),
	qual_info_set_found_syntax_error(no, !Info),
	(
		FoundError = yes,
			% Don't insert clauses containing syntax errors into
			% the clauses_info, because doing that would cause
			% typecheck.m to report spurious type errors.
			% Don't report singleton variable warnings if there
			% were syntax errors.
		true_goal(Goal)
	;
		FoundError = no,
		Goal = Goal0,

			% If we have foreign clauses, we should only
			% add this clause for modes *not* covered by the
			% foreign clauses.
		( HasForeignClauses = yes ->
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
		),
		qual_info_get_var_types(!.Info, ExplicitVarTypes),
		!:ClausesInfo = clauses_info(VarSet, ExplicitVarTypes,
			TVarNameMap, InferredVarTypes, HeadVars,
			ClauseList, TI_VarMap, TCI_VarMap,
			HasForeignClauses)
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

:- pred clauses_info_add_pragma_foreign_proc(purity::in,
	pragma_foreign_proc_attributes::in, pred_id::in, proc_id::in,
	prog_varset::in, list(pragma_var)::in, list(type)::in,
	pragma_foreign_code_impl::in, prog_context::in, pred_or_func::in,
	sym_name::in, arity::in, clauses_info::in, clauses_info::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

clauses_info_add_pragma_foreign_proc(Purity, Attributes0, PredId, ProcId,
		PVarSet, PVars, OrigArgTypes, PragmaImpl0, Context, PredOrFunc,
		PredName, Arity, !ClausesInfo, !ModuleInfo, !IO) :-

	!.ClausesInfo = clauses_info(VarSet0, VarTypes, TVarNameMap,
		VarTypes1, HeadVars, ClauseList, TI_VarMap, TCI_VarMap,
		_HasForeignClauses),

		% Find all the existing clauses for this mode, and
		% extract their implementation language and clause number
		% (that is, their index in the list).
	NewLang = foreign_language(Attributes0),

	globals__io_get_globals(Globals, !IO),
	globals__io_get_target(Target, !IO),

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

	list__foldl2(
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
		), ClauseList, add, FinalAction, 1, _),

	UpdateClauses = (pred(NewCl::in, Cs::out) is det :-
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
	),

	globals__io_get_backend_foreign_languages(BackendForeignLanguages,
		!IO),
	pragma_get_vars(PVars, Args0),
	pragma_get_var_infos(PVars, ArgInfo),

	%
	% If the foreign language not one of the backend
	% languages, we will have to generate an interface to it in a
	% backend language.
	%
	foreign__extrude_pragma_implementation(BackendForeignLanguages,
		PVars, PredName, PredOrFunc, Context, !ModuleInfo,
		Attributes0, Attributes, PragmaImpl0, PragmaImpl),

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
	assoc_list__keys(ArgBagAL, MultipleArgs),

	( MultipleArgs = [_ | _] ->
		prog_out__write_context(Context, !IO),
		io__write_string(
			"In `:- pragma foreign_proc' declaration for ", !IO),
		adjust_func_arity(PredOrFunc, OrigArity, Arity),
		hlds_out__write_simple_call_id(
			PredOrFunc - PredName/OrigArity, !IO),
		io__write_string(":\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  error: ", !IO),
		(
			MultipleArgs = [MultipleArg],
			io__write_string("variable `", !IO),
			mercury_output_var(MultipleArg, PVarSet, no, !IO),
			io__write_string("' occurs multiple times\n", !IO)
		;
			MultipleArgs = [_, _ | _],
			io__write_string("variables `", !IO),
			mercury_output_vars(MultipleArgs, PVarSet, no, !IO),
			io__write_string("' occur multiple times\n", !IO)
		),
		prog_out__write_context(Context, !IO),
		io__write_string("  in the argument list.\n", !IO),
		io__set_exit_status(1, !IO)
	;
			% build the pragma_c_code
		goal_info_init(GoalInfo0),
		goal_info_set_context(GoalInfo0, Context, GoalInfo1),
		% Put the purity in the goal_info in case
		% this foreign code is inlined
		add_goal_info_purity_feature(GoalInfo1, Purity, GoalInfo),
		make_foreign_args(HeadVars, ArgInfo, OrigArgTypes,
			ForeignArgs),
		HldsGoal0 = foreign_proc(Attributes, PredId, ProcId,
			ForeignArgs, [], PragmaImpl)
			- GoalInfo,
		map__init(EmptyVarTypes),
		implicitly_quantify_clause_body(HeadVars, _Warnings, HldsGoal0,
			HldsGoal, VarSet0, VarSet, EmptyVarTypes, _),
		NewClause = clause([ProcId], HldsGoal,
			foreign_language(NewLang), Context),
		UpdateClauses(NewClause, NewClauseList),
		HasForeignClauses = yes,
		!:ClausesInfo =  clauses_info(VarSet, VarTypes, TVarNameMap,
			VarTypes1, HeadVars, NewClauseList,
			TI_VarMap, TCI_VarMap, HasForeignClauses)
	).

:- pred allocate_vars_for_saved_vars(list(string)::in,
	list(pair(prog_var, string))::out,
	prog_varset::in, prog_varset::out) is det.

allocate_vars_for_saved_vars([], [], !VarSet).
allocate_vars_for_saved_vars([Name | Names], [Var - Name | VarNames],
		!VarSet) :-
	varset__new_var(!.VarSet, Var, !:VarSet),
	allocate_vars_for_saved_vars(Names, VarNames, !VarSet).

%-----------------------------------------------------------------------------

:- type transform_info --->
	transform_info(
		module_info	:: module_info,
		qual_info	:: qual_info
	).

:- pred transform(prog_substitution::in, list(prog_var)::in,
	list(prog_term)::in, goal::in, prog_context::in, pred_or_func::in,
	arity::in, goal_type::in, hlds_goal::out,
	prog_varset::in, prog_varset::out, list(quant_warning)::out,
	transform_info::in, transform_info::out, io::di, io::uo) is det.

transform(Subst, HeadVars, Args0, Body0, Context, PredOrFunc, Arity, GoalType,
		Goal, !VarSet, Warnings, !Info, !IO) :-
	prepare_for_head(SInfo0),
	term__apply_substitution_to_list(Args0, Subst, Args1),
	substitute_state_var_mappings(Args1, Args, !VarSet, SInfo0, SInfo1,
		!IO),
	hlds_goal__true_goal(Head0),
	( GoalType = promise(_) ->
		Head    = Head0,
		SInfo2  = SInfo1
	;
		ArgContext = head(PredOrFunc, Arity),
		insert_arg_unifications(HeadVars, Args, Context, ArgContext,
			Head0, Head, !VarSet, !Info, SInfo1, SInfo2, !IO)
	),
	prepare_for_body(FinalSVarMap, !VarSet, SInfo2, SInfo3),
	transform_goal(Body0, Subst, Body, !VarSet, !Info, SInfo3, SInfo, !IO),
	finish_head_and_body(Context, FinalSVarMap, Head, Body, Goal0, SInfo),
	VarTypes0 = !.Info ^ qual_info ^ vartypes,
	implicitly_quantify_clause_body(HeadVars, Warnings, Goal0, Goal,
		!VarSet, VarTypes0, VarTypes),
	!:Info = !.Info ^ qual_info ^ vartypes := VarTypes.

%-----------------------------------------------------------------------------%

	% Convert goals from the prog_data `goal' structure into the
	% hlds `hlds_goal' structure.  At the same time, convert
	% it to super-homogeneous form by unravelling all the complex
	% unifications, and annotate those unifications with a unify_context
	% so that we can still give good error messages.
	% And also at the same time, apply the given substitution to
	% the goal, to rename it apart from the other clauses.

:- pred transform_goal(goal::in, prog_substitution::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_goal(Goal0 - Context, Subst, Goal1 - GoalInfo1, !VarSet,
		!Info, !SInfo, !IO) :-
	transform_goal_2(Goal0, Context, Subst, Goal1 - GoalInfo0,
		!VarSet, !Info, !SInfo, !IO),
	goal_info_set_context(GoalInfo0, Context, GoalInfo1).

:- pred transform_goal_2(goal_expr::in, prog_context::in,
	prog_substitution::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_goal_2(fail, _, _, disj([]) - GoalInfo, !VarSet, !Info, !SInfo,
		!IO) :-
	goal_info_init(GoalInfo),
	prepare_for_next_conjunct(set__init, !VarSet, !SInfo).

transform_goal_2(true, _, _, conj([]) - GoalInfo, !VarSet, !Info, !SInfo,
		!IO) :-
	goal_info_init(GoalInfo),
	prepare_for_next_conjunct(set__init, !VarSet, !SInfo).

	% Convert `all [Vars] Goal' into `not some [Vars] not Goal'.
transform_goal_2(all(Vars0, Goal0), Context, Subst, Goal, !VarSet, !Info,
		!SInfo, !IO) :-
	TransformedGoal = not(some(Vars0, not(Goal0) - Context) - Context),
	transform_goal_2(TransformedGoal, Context, Subst, Goal, !VarSet,
		!Info, !SInfo, !IO).

transform_goal_2(all_state_vars(StateVars, Goal0), Context, Subst,
		Goal, !VarSet, !Info, !SInfo, !IO) :-
	transform_goal_2(
		not(some_state_vars(StateVars, not(Goal0) - Context) - Context),
		Context, Subst, Goal, !VarSet, !Info, !SInfo, !IO).

transform_goal_2(some(Vars0, Goal0), _, Subst,
		some(Vars, can_remove, Goal) - GoalInfo,
		!VarSet, !Info, !SInfo, !IO) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(Goal0, Subst, Goal, !VarSet, !Info, !SInfo, !IO),
	goal_info_init(GoalInfo).

transform_goal_2(some_state_vars(StateVars0, Goal0), _, Subst,
		some(Vars, can_remove, Goal) - GoalInfo,
		!VarSet, !Info, !SInfo, !IO) :-
	BeforeSInfo = !.SInfo,
	substitute_vars(StateVars0, Subst, StateVars),
	prepare_for_local_state_vars(StateVars, !VarSet, !SInfo),
	transform_goal(Goal0, Subst, Goal, !VarSet, !Info, !SInfo, !IO),
	finish_local_state_vars(StateVars, Vars, BeforeSInfo, !SInfo),
	goal_info_init(GoalInfo).

transform_goal_2(if_then_else(Vars0, StateVars0, Cond0, Then0, Else0), Context,
		Subst, if_then_else(Vars, Cond, Then, Else) - GoalInfo,
		!VarSet, !Info, !SInfo, !IO) :-
	BeforeSInfo = !.SInfo,
	substitute_vars(Vars0, Subst, Vars),
	substitute_vars(StateVars0, Subst, StateVars),
	prepare_for_if_then_else_goal(StateVars, !VarSet, !SInfo),
	transform_goal(Cond0, Subst, Cond, !VarSet, !Info, !SInfo, !IO),
	finish_if_then_else_goal_condition(StateVars,
		BeforeSInfo, !.SInfo, AfterCondSInfo, !:SInfo),
	transform_goal(Then0, Subst, Then1, !VarSet, !Info, !SInfo, !IO),
	finish_if_then_else_goal_then_goal(StateVars, BeforeSInfo, !SInfo),
	AfterThenSInfo = !.SInfo,
	transform_goal(Else0, Subst, Else1, !VarSet, !Info,
		BeforeSInfo, !:SInfo, !IO),
	goal_info_init(Context, GoalInfo),
	finish_if_then_else(Context, Then1, Then, Else1, Else,
		BeforeSInfo, AfterCondSInfo, AfterThenSInfo, !SInfo, !VarSet).

transform_goal_2(if_then(Vars0, StateVars, A0, B0), Context, Subst,
		Goal, !VarSet, !Info, !SInfo, !IO) :-
	transform_goal_2(
		if_then_else(Vars0, StateVars, A0, B0, true - Context),
		Context, Subst, Goal, !VarSet, !Info, !SInfo, !IO).

transform_goal_2(not(A0), _, Subst, Goal, !VarSet, !Info, !SInfo, !IO) :-
	BeforeSInfo = !.SInfo,
	transform_goal(A0, Subst, A, !VarSet, !Info, !SInfo, !IO),
	goal_info_init(GoalInfo),
	Goal = not(A) - GoalInfo,
	finish_negation(BeforeSInfo, !SInfo).

transform_goal_2((A0, B0), _, Subst, Goal, !VarSet, !Info, !SInfo, !IO) :-
	get_rev_conj(A0, Subst, [], R0, !VarSet, !Info, !SInfo, !IO),
	get_rev_conj(B0, Subst, R0, R,  !VarSet, !Info, !SInfo, !IO),
	L = list__reverse(R),
	goal_info_init(GoalInfo),
	conj_list_to_goal(L, GoalInfo, Goal).

transform_goal_2((A0 & B0), _, Subst, Goal, !VarSet, !Info, !SInfo, !IO) :-
	get_rev_par_conj(B0, Subst, [], R0, !VarSet, !Info, !SInfo, !IO),
	get_rev_par_conj(A0, Subst, R0, R,  !VarSet, !Info, !SInfo, !IO),
	L = list__reverse(R),
	goal_info_init(GoalInfo),
	par_conj_list_to_goal(L, GoalInfo, Goal).

transform_goal_2((A0 ; B0), Context, Subst, Goal, !VarSet, !Info, !SInfo,
		!IO) :-
	get_disj(B0, Subst, [], L0, !VarSet, !Info, !.SInfo, !IO),
	get_disj(A0, Subst, L0, L1, !VarSet, !Info, !.SInfo, !IO),
	finish_disjunction(Context, !.VarSet, L1, L, !:SInfo),
	goal_info_init(Context, GoalInfo),
	disj_list_to_goal(L, GoalInfo, Goal).

transform_goal_2(implies(P, Q), Context, Subst, Goal, !VarSet, !Info, !SInfo,
		!IO) :-
		% `P => Q' is defined as `not (P, not Q)'
	TransformedGoal = not( (P, not(Q) - Context) - Context ),
	transform_goal_2(TransformedGoal, Context, Subst, Goal, !VarSet,
		!Info, !SInfo, !IO).

transform_goal_2(equivalent(P0, Q0), _, Subst, Goal, !VarSet, !Info, !SInfo,
		!IO) :-
	%
	% `P <=> Q' is defined as `(P => Q), (Q => P)',
	% but that transformation must not be done until
	% after quantification analysis, lest the duplication of
	% the goals concerned affect the implicit quantification
	% of the variables inside them.
	%
	BeforeSInfo = !.SInfo,
	goal_info_init(GoalInfo),
	transform_goal(P0, Subst, P, !VarSet, !Info, !SInfo, !IO),
	transform_goal(Q0, Subst, Q, !VarSet, !Info, !SInfo, !IO),
	Goal = shorthand(bi_implication(P, Q)) - GoalInfo,
	finish_equivalence(BeforeSInfo, !SInfo).

transform_goal_2(call(Name, Args0, Purity), Context, Subst, Goal, !VarSet,
		!Info, !SInfo, !IO) :-
	Args1 = expand_bang_state_var_args(Args0),
	(
		Name = unqualified("\\="),
		Args1 = [LHS, RHS]
	->
		prepare_for_call(!SInfo),
			% `LHS \= RHS' is defined as `not (LHS = RHS)'
		transform_goal_2(not(unify(LHS, RHS, Purity) - Context),
			Context, Subst, Goal, !VarSet, !Info, !SInfo, !IO),
		finish_call(!VarSet, !SInfo)
	;
		% check for a DCG field access goal:
		% get:  Field =^ field
		% set:  ^ field := Field
		( Name = unqualified(Operator) ),
		( Operator = "=^"
		; Operator = ":="
		)
	->
		prepare_for_call(!SInfo),
		term__apply_substitution_to_list(Args1, Subst, Args2),
		transform_dcg_record_syntax(Operator, Args2, Context,
			Goal, !VarSet, !Info, !SInfo, !IO),
		finish_call(!VarSet, !SInfo)
	;
		% check for an Aditi builtin
		Purity = pure,
		Name = unqualified(Name1),
		( Name1 = "aditi_insert"
		; Name1 = "aditi_delete"
		; Name1 = "aditi_bulk_insert"
		; Name1 = "aditi_bulk_delete"
		; Name1 = "aditi_bulk_modify"
		)
	->
		term__apply_substitution_to_list(Args1, Subst, Args2),
		transform_aditi_builtin(Name1, Args2, Context, Goal,
			!VarSet, !Info, !SInfo, !IO)
	;
		prepare_for_call(!SInfo),
		term__apply_substitution_to_list(Args1, Subst, Args),
		make_fresh_arg_vars(Args, HeadVars, !VarSet, !SInfo, !IO),
		list__length(Args, Arity),
		(
			% check for a higher-order call,
			% i.e. a call to either call/N or ''/N.
			( Name = unqualified("call")
			; Name = unqualified("")
			),
			HeadVars = [PredVar | RealHeadVars]
		->
			% initialize some fields to junk
			Modes = [],
			Det = erroneous,

			GenericCall = higher_order(PredVar, Purity,
				predicate, Arity),
			Call = generic_call(GenericCall, RealHeadVars,
				Modes, Det),

			hlds_goal__generic_call_id(GenericCall, CallId)
		;
			% initialize some fields to junk
			PredId = invalid_pred_id,
			ModeId = invalid_proc_id,

			MaybeUnifyContext = no,
			Call = call(PredId, ModeId, HeadVars, not_builtin,
				MaybeUnifyContext, Name),
			CallId = call(predicate - Name/Arity)
		),
		goal_info_init(Context, GoalInfo0),
		add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo),
		Goal0 = Call - GoalInfo,

		record_called_pred_or_func(predicate, Name, Arity, !Info),
		insert_arg_unifications(HeadVars, Args, Context, call(CallId),
			Goal0, Goal, !VarSet, !Info, !SInfo, !IO),
		finish_call(!VarSet, !SInfo)
	).

transform_goal_2(unify(A0, B0, Purity), Context, Subst, Goal, !VarSet,
		!Info, !SInfo, !IO) :-
	% It is an error for the left or right hand side of a
	% unification to be !X (it may be !.X or !:X, however).
	%
	( A0 = functor(atom("!"), [variable(StateVarA)], _) ->
		report_svar_unify_error(Context, !.VarSet, StateVarA, !IO),
		true_goal(Goal)
	; B0 = functor(atom("!"), [variable(StateVarB)], _) ->
		report_svar_unify_error(Context, !.VarSet, StateVarB, !IO),
		true_goal(Goal)
	;
		prepare_for_call(!SInfo),
		term__apply_substitution(A0, Subst, A),
		term__apply_substitution(B0, Subst, B),
		unravel_unification(A, B, Context, explicit, [],
			Purity, Goal, !VarSet, !Info, !SInfo, !IO),
		finish_call(!VarSet, !SInfo)
	).

:- pred report_svar_unify_error(prog_context::in, prog_varset::in, svar::in,
	io::di, io::uo) is det.

report_svar_unify_error(Context, VarSet, StateVar, !IO) :-
	Name = varset__lookup_name(VarSet, StateVar),
	prog_out__write_context(Context, !IO),
	report_warning(string__format("\
Error: !%s cannot appear as a unification argument.\n", [s(Name)]), !IO),
	prog_out__write_context(Context, !IO),
	report_warning(string__format("\
You probably meant !.%s or !:%s.\n", [s(Name), s(Name)]), !IO).

:- inst dcg_record_syntax_op == bound("=^"; ":=").

:- pred transform_dcg_record_syntax(string::in(dcg_record_syntax_op),
	list(prog_term)::in, prog_context::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_dcg_record_syntax(Operator, ArgTerms0, Context, Goal, !VarSet,
		!Info, !SInfo, !IO) :-
	goal_info_init(Context, GoalInfo),
	(
		ArgTerms0 = [LHSTerm, RHSTerm, TermInputTerm, TermOutputTerm],
		(
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
		)
	->
		parse_field_list(FieldNameTerm, MaybeFieldNames),
		(
			MaybeFieldNames = ok(FieldNames),
			ArgTerms = [FieldValueTerm, TermInputTerm,
				TermOutputTerm],
			transform_dcg_record_syntax_2(AccessType,
				FieldNames, ArgTerms, Context, Goal,
				!VarSet, !Info, !SInfo, !IO)
		;
			MaybeFieldNames = error(Msg, ErrorTerm),
			invalid_goal("^", ArgTerms0, GoalInfo, Goal, !VarSet,
				!SInfo, !IO),
			qual_info_set_found_syntax_error(yes,
				!.Info ^ qual_info, QualInfo),
			!:Info = !.Info ^ qual_info := QualInfo,
			io__set_exit_status(1, !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("In DCG field ", !IO),
			(
				AccessType = set,
				io__write_string("update", !IO)
			;
				AccessType = get,
				io__write_string("extraction", !IO)
			),
			io__write_string(" goal:\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  error: ", !IO),
			io__write_string(Msg, !IO),
			io__write_string(" at term `", !IO),
			term_io__write_term(!.VarSet, ErrorTerm, !IO),
			io__write_string("'.\n", !IO)
		)
	;
		invalid_goal("^", ArgTerms0, GoalInfo, Goal, !VarSet,
			!SInfo, !IO),
		qual_info_set_found_syntax_error(yes, !.Info ^ qual_info,
			QualInfo),
		!:Info = !.Info ^ qual_info := QualInfo,
		io__set_exit_status(1, !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("Error: expected " ++
			"`Field =^ field1 ^ ... ^ fieldN'\n", !IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  or `^ field1 ^ ... ^ fieldN := Field'.\n",
			!IO),
		prog_out__write_context(Context, !IO),
		io__write_string("  in DCG field access goal.\n", !IO)
	).

:- pred transform_dcg_record_syntax_2(field_access_type::in, field_list::in,
	list(prog_term)::in, prog_context::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_dcg_record_syntax_2(AccessType, FieldNames, ArgTerms, Context, Goal,
		!VarSet, !Info, !SInfo, !IO) :-
	make_fresh_arg_vars(ArgTerms, ArgVars, !VarSet, !SInfo, !IO),
	( ArgVars = [FieldValueVar, TermInputVar, TermOutputVar] ->
		(
			AccessType = set,
			expand_set_field_function_call(Context, explicit, [],
				FieldNames, FieldValueVar, TermInputVar,
				TermOutputVar, !VarSet, Functor,
				InnermostFunctor - InnermostSubContext, Goal0,
				!Info, !SInfo, !IO),

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
				ArgTerms, ArgContexts, Context, Goal0, Goal,
				!VarSet, !Info, !SInfo, !IO)
		;
			AccessType = get,
			expand_dcg_field_extraction_goal(Context, explicit,
				[], FieldNames, FieldValueVar, TermInputVar,
				TermOutputVar, !VarSet, Functor,
				InnermostFunctor - _InnerSubContext, Goal0,
				!Info, !SInfo, !IO),
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
				ArgTerms, ArgContexts, Context, Goal0, Goal,
				!VarSet, !Info, !SInfo, !IO)
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
:- pred expand_set_field_function_call(prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, field_list::in,
	prog_var::in, prog_var::in, prog_var::in,
	prog_varset::in, prog_varset::out, cons_id::out,
	pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

expand_set_field_function_call(Context, MainContext, SubContext0, FieldNames,
		FieldValueVar, TermInputVar, TermOutputVar, !VarSet, Functor,
		FieldSubContext, Goal, !Info, !SInfo, !IO) :-
	expand_set_field_function_call_2(Context, MainContext,
		SubContext0, FieldNames, FieldValueVar, TermInputVar,
		TermOutputVar, !VarSet, Functor, FieldSubContext, Goals,
		!Info, !SInfo, !IO),
	goal_info_init(Context, GoalInfo),
	conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_set_field_function_call_2(prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, field_list::in,
	prog_var::in, prog_var::in, prog_var::in,
	prog_varset::in, prog_varset::out, cons_id::out,
	pair(cons_id, unify_sub_contexts)::out, list(hlds_goal)::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

expand_set_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _, _, _, _,
		_, _, !IO) :-
	error("expand_set_field_function_call_2: empty list of field names").
expand_set_field_function_call_2(Context, MainContext, SubContext0,
		[FieldName - FieldArgs | FieldNames], FieldValueVar,
		TermInputVar, TermOutputVar, !VarSet, Functor,
		FieldSubContext, Goals, !Info, !SInfo, !IO) :-
	make_fresh_arg_vars(FieldArgs, FieldArgVars, !VarSet, !SInfo, !IO),
	( FieldNames = [_ | _] ->
		varset__new_var(!.VarSet, SubTermInputVar, !:VarSet),
		varset__new_var(!.VarSet, SubTermOutputVar, !:VarSet),
		SetArgs = list__append(FieldArgVars,
			[TermInputVar, SubTermOutputVar]),
		construct_field_access_function_call(set, Context,
			MainContext, SubContext0, FieldName, TermOutputVar,
			SetArgs, Functor, UpdateGoal, !Info),

		% extract the field containing the field to update.
		construct_field_access_function_call(get, Context,
			MainContext, SubContext0, FieldName, SubTermInputVar,
			list__append(FieldArgVars, [TermInputVar]), _,
			GetSubFieldGoal, !Info),

		% recursively update the field.
		SubTermInputArgNumber = 2 + list__length(FieldArgs),
		TermInputContext = Functor - SubTermInputArgNumber,
		SubContext = [TermInputContext | SubContext0],
		expand_set_field_function_call_2(Context, MainContext,
			SubContext, FieldNames, FieldValueVar, SubTermInputVar,
			SubTermOutputVar, !VarSet, _, FieldSubContext, Goals0,
			!Info, !SInfo, !IO),

		list__append([GetSubFieldGoal | Goals0], [UpdateGoal], Goals1)
	;
		SetArgs = list__append(FieldArgVars,
			[TermInputVar, FieldValueVar]),
		construct_field_access_function_call(set, Context,
			MainContext, SubContext0, FieldName, TermOutputVar,
			SetArgs, Functor, Goal, !Info),
		FieldSubContext = Functor - SubContext0,
		Goals1 = [Goal]

	),
	ArgContext = functor(Functor, MainContext, SubContext0),
	goal_info_init(Context, GoalInfo),
	conj_list_to_goal(Goals1, GoalInfo, Conj0),
	insert_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
		Conj0, Conj, !VarSet, !Info, !SInfo, !IO),
	goal_to_conj_list(Conj, Goals).

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
:- pred expand_dcg_field_extraction_goal(prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, field_list::in,
	prog_var::in, prog_var::in, prog_var::in,
	prog_varset::in, prog_varset::out, cons_id::out,
	pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

expand_dcg_field_extraction_goal(Context, MainContext, SubContext,
		FieldNames, FieldValueVar, TermInputVar, TermOutputVar,
		!VarSet, Functor, FieldSubContext, Goal, !Info, !SInfo, !IO) :-
	% unify the DCG input and output variables
	make_atomic_unification(TermOutputVar, var(TermInputVar), Context,
		MainContext, SubContext, UnifyDCG, !Info),

	% process the access function as a get function on
	% the output DCG variable
	expand_get_field_function_call_2(Context, MainContext, SubContext,
		FieldNames, FieldValueVar, TermOutputVar, !VarSet,
		Functor, FieldSubContext, Goals1, !Info, !SInfo, !IO),
	Goals = [UnifyDCG | Goals1],
	goal_info_init(Context, GoalInfo),
	conj_list_to_goal(Goals, GoalInfo, Goal).

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
:- pred expand_get_field_function_call(prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, field_list::in,
	prog_var::in, prog_var::in, prog_varset::in, prog_varset::out,
	cons_id::out, pair(cons_id, unify_sub_contexts)::out, hlds_goal::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

expand_get_field_function_call(Context, MainContext, SubContext0,
		FieldNames, FieldValueVar, TermInputVar, !VarSet,
		Functor, FieldSubContext, Goal, !Info, !SInfo, !IO) :-
	expand_get_field_function_call_2(Context, MainContext, SubContext0,
		FieldNames, FieldValueVar, TermInputVar, !VarSet,
		Functor, FieldSubContext, Goals, !Info, !SInfo, !IO),
	goal_info_init(Context, GoalInfo),
	conj_list_to_goal(Goals, GoalInfo, Goal).

:- pred expand_get_field_function_call_2(prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, field_list::in,
	prog_var::in, prog_var::in, prog_varset::in, prog_varset::out,
	cons_id::out, pair(cons_id, unify_sub_contexts)::out,
	list(hlds_goal)::out, transform_info::in, transform_info::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

expand_get_field_function_call_2(_, _, _, [], _, _, _, _, _, _, _, _, _,
		_, _, !IO) :-
	error("expand_get_field_function_call_2: empty list of field names").
expand_get_field_function_call_2(Context, MainContext, SubContext0,
		[FieldName - FieldArgs | FieldNames], FieldValueVar,
		TermInputVar, !VarSet, Functor, FieldSubContext, Goals,
		!Info, !SInfo, !IO) :-
	make_fresh_arg_vars(FieldArgs, FieldArgVars, !VarSet, !SInfo, !IO),
	GetArgVars = list__append(FieldArgVars, [TermInputVar]),
	( FieldNames = [_ | _] ->
		varset__new_var(!.VarSet, SubTermInputVar, !:VarSet),
		construct_field_access_function_call(get, Context,
			MainContext, SubContext0, FieldName, SubTermInputVar,
			GetArgVars, Functor, Goal, !Info),

		% recursively extract until we run out of field names
		TermInputArgNumber = 1 + list__length(FieldArgVars),
		TermInputContext = Functor - TermInputArgNumber,
		SubContext = [TermInputContext | SubContext0],
		expand_get_field_function_call_2(Context, MainContext,
			SubContext, FieldNames, FieldValueVar, SubTermInputVar,
			!VarSet, _, FieldSubContext, Goals1, !Info, !SInfo,
			!IO),
		Goals2 = [Goal | Goals1]
	;
		FieldSubContext = Functor - SubContext0,
		construct_field_access_function_call(get, Context,
			MainContext, SubContext0, FieldName, FieldValueVar,
			GetArgVars, Functor, Goal, !Info),
		Goals2 = [Goal]
	),
	ArgContext = functor(Functor, MainContext, SubContext0),
	goal_info_init(Context, GoalInfo),
	conj_list_to_goal(Goals2, GoalInfo, Conj0),
	insert_arg_unifications(FieldArgVars, FieldArgs, Context, ArgContext,
		Conj0, Conj, !VarSet, !Info, !SInfo, !IO),
	goal_to_conj_list(Conj, Goals).

:- pred construct_field_access_function_call(field_access_type::in,
	prog_context::in, unify_main_context::in, unify_sub_contexts::in,
	ctor_field_name::in, prog_var::in, list(prog_var)::in, cons_id::out,
	hlds_goal::out, transform_info::in, transform_info::out) is det.

construct_field_access_function_call(AccessType, Context, MainContext,
		SubContext, FieldName, RetArg, Args, Functor, Goal, !Info) :-
	field_access_function_name(AccessType, FieldName, FuncName),
	list__length(Args, Arity),
	Functor = cons(FuncName, Arity),
	make_atomic_unification(RetArg, functor(Functor, no, Args),
		Context, MainContext, SubContext, Goal, !Info).

:- type field_list == assoc_list(ctor_field_name, list(prog_term)).

:- pred parse_field_list(prog_term::in,
	maybe1(field_list, prog_var_type)::out) is det.

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
			MaybeFieldNames = error("expected field name", Term)
		)
	).

%-----------------------------------------------------------------------------%

:- inst aditi_update_str ==
	bound(	"aditi_insert"
	;	"aditi_delete"
	;	"aditi_bulk_insert"
	;	"aditi_bulk_delete"
	;	"aditi_bulk_modify"
	).

	% See the "Aditi update syntax" section of the
	% Mercury Language Reference Manual.
:- pred transform_aditi_builtin(string::in(aditi_update_str),
	list(prog_term)::in, prog_context::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_aditi_builtin(UpdateStr, Args0, Context, Goal, !VarSet,
		!Info, !SInfo, !IO) :-
	(
		( UpdateStr = "aditi_insert", Update = insert
		; UpdateStr = "aditi_delete", Update = delete
		)
	->
		transform_aditi_tuple_update(UpdateStr, Update, Args0,
			Context, Goal, !VarSet, !Info, !SInfo, !IO)
	;
		( UpdateStr = "aditi_bulk_insert", Update = bulk_insert
		; UpdateStr = "aditi_bulk_delete", Update = bulk_delete
		; UpdateStr = "aditi_bulk_modify", Update = bulk_modify
		)
	->
		transform_aditi_bulk_update(UpdateStr, Update, Args0,
			Context, Goal, !VarSet, !Info, !SInfo, !IO)

	;
		error("transform_aditi_builtin")
	).

:- pred transform_aditi_tuple_update(string::in, aditi_tuple_update::in,
	list(prog_term)::in, prog_context::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_aditi_tuple_update(UpdateStr, Update, Args0, Context,
		Goal, !VarSet, !Info, !SInfo, !IO) :-
	% Build an empty goal_info.
	goal_info_init(Context, GoalInfo),

	%
	% Syntax -
	% aditi_insert(p(_DB, X, Y), DB0, DB).
	%
	% `p(_DB, X, Y)' is the tuple to insert, not a higher-order term.
	%
	( Args0 = [InsertTupleTerm, AditiState0Term, AditiStateTerm] ->
		(
			% Parse the tuple to insert.
			parse_pred_or_func_and_args(InsertTupleTerm,
				PredOrFunc, SymName, TupleArgTerms)
		->
			%
			% Make new variables for the arguments.
			% The argument list of the `aditi_insert'
			% goal contains the arguments of the tuple
			% to insert and the `aditi__state' arguments.
			%
			make_fresh_arg_var(AditiState0Term, AditiState0Var, [],
				!VarSet, !SInfo, !IO),
			make_fresh_arg_var(AditiStateTerm, AditiStateVar, [],
				!VarSet, !SInfo, !IO),
			make_fresh_arg_vars(TupleArgTerms, TupleArgVars,
				!VarSet, !SInfo, !IO),
			list__append(TupleArgVars,
				[AditiState0Var, AditiStateVar], AllArgs),
			list__length(TupleArgVars, InsertArity),

			PredId = invalid_pred_id,
			Builtin = aditi_tuple_update(Update, PredId),
			InsertCallId = PredOrFunc - SymName/InsertArity,
			Call = generic_call(
				aditi_builtin(Builtin, InsertCallId),
				AllArgs, [], det),
			Goal0 = Call - GoalInfo,
			CallId = generic_call(aditi_builtin(Builtin,
				InsertCallId)),
			list__append(TupleArgTerms,
				[AditiState0Term, AditiStateTerm],
				AllArgTerms),

			record_called_pred_or_func(PredOrFunc, SymName,
				InsertArity, !Info),
			insert_arg_unifications(AllArgs, AllArgTerms,
				Context, call(CallId), Goal0, Goal,
				!VarSet, !Info, !SInfo, !IO)
		;
			invalid_goal(UpdateStr, Args0, GoalInfo,
				Goal, !VarSet, !SInfo, !IO),
			qual_info_set_found_syntax_error(yes,
				!.Info ^ qual_info, QualInfo),
			!:Info = !.Info ^ qual_info := QualInfo,
			io__set_exit_status(1, !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("Error: expected tuple to ", !IO),
			io__write(Update, !IO),
			io__write_string(" in `", !IO),
			io__write_string(UpdateStr, !IO),
			io__write_string("'.\n", !IO)
		)
	;
		invalid_goal(UpdateStr, Args0, GoalInfo, Goal, !VarSet,
			!SInfo, !IO),
		qual_info_set_found_syntax_error(yes, !.Info ^ qual_info,
			QualInfo),
		!:Info = !.Info ^ qual_info := QualInfo,
		list__length(Args0, Arity),
		aditi_update_arity_error(Context, UpdateStr, Arity, [3], !IO)
	).

	% Parse an `aditi_delete' or `aditi_modify' goal.
:- pred transform_aditi_bulk_update(string::in, aditi_bulk_update::in,
	list(prog_term)::in, prog_context::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

transform_aditi_bulk_update(Descr, Update, Args0, Context, UpdateGoal,
		!VarSet, !Info, !SInfo, !IO) :-
	goal_info_init(Context, GoalInfo),
	(
		list__length(Args0, Arity),
		Arity \= 3,
		Arity \= 4
	->
		invalid_goal(Descr, Args0, GoalInfo,
			UpdateGoal, !VarSet, !SInfo, !IO),
		qual_info_set_found_syntax_error(yes, !.Info ^ qual_info,
			QualInfo),
		!:Info = !.Info ^ qual_info := QualInfo,
		aditi_update_arity_error(Context, Descr, Arity, [3, 4], !IO)
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
		Args0 = [HOTerm, AditiState0Term, AditiStateTerm],
		parse_rule_term(Context, HOTerm, HeadTerm, GoalTerm1),
		(
			Update = bulk_insert,
			parse_pred_or_func_and_args(HeadTerm,
				PredOrFunc, SymName, HeadArgs1),
			list__length(HeadArgs1, PredArity)
		;
			Update = bulk_delete,
			parse_pred_or_func_and_args(HeadTerm,
				PredOrFunc, SymName, HeadArgs1),
			list__length(HeadArgs1, PredArity)
		;
			Update = bulk_modify,
			HeadTerm = term__functor(term__atom("==>"),
				[LeftHeadTerm, RightHeadTerm], _),
			parse_pred_or_func_and_args(LeftHeadTerm,
				PredOrFunc, SymName, LeftHeadArgs),
			parse_pred_or_func_and_args(RightHeadTerm,
				PredOrFunc, SymName, RightHeadArgs),
			list__append(LeftHeadArgs, RightHeadArgs, HeadArgs1),
			list__length(LeftHeadArgs, PredArity),
			list__length(RightHeadArgs, PredArity)
		)
	->
		%
		% This syntax is transformed into a construction of
		% a lambda expression for the modification condition
		% and a call to an update goal with that closure.
		% The transformed code is equivalent to the
		% `sym_name_and_closure' syntax which is parsed below.
		%
		Syntax = pred_term,

		%
		% Parse the modification goal as for a lambda expression.
		%
		make_fresh_arg_vars(HeadArgs1, HeadArgs, !VarSet, !SInfo, !IO),
		term__coerce(GoalTerm1, GoalTerm),
		parse_goal(GoalTerm, ParsedGoal, !VarSet),

		prepare_for_lambda(!SInfo),

		hlds_goal__true_goal(PredHead0),
		ArgContext = head(PredOrFunc, PredArity),
		insert_arg_unifications(HeadArgs, HeadArgs1, Context,
			ArgContext, PredHead0, PredHead, !VarSet,
			!Info, !SInfo, !IO),

		prepare_for_body(FinalSVarMap, !VarSet, !SInfo),

		map__init(Substitution),
		transform_goal(ParsedGoal, Substitution, PredBody,
			!VarSet, !Info, !SInfo, !IO),

		finish_head_and_body(Context, FinalSVarMap, PredHead, PredBody,
			PredGoal0, !.SInfo),

		% Quantification will reduce this down to
		% the proper set of nonlocal arguments.
		goal_util__goal_vars(PredGoal, LambdaGoalVars0),
		set__delete_list(LambdaGoalVars0, HeadArgs, LambdaGoalVars1),
		set__to_sorted_list(LambdaGoalVars1, LambdaNonLocals),
		aditi_bulk_update_goal_info(Update,
			PredOrFunc, SymName, PredArity, HeadArgs,
			LambdaPredOrFunc, EvalMethod, LambdaModes,
			Detism, PredGoal0, PredGoal),
		ModifiedCallId = PredOrFunc - SymName/PredArity,

		PredId = invalid_pred_id,
		Builtin = aditi_bulk_update(Update, PredId, Syntax),
		MainContext =
			call(generic_call(
				aditi_builtin(Builtin, ModifiedCallId)),
			1),
		varset__new_var(!.VarSet, LambdaVar, !:VarSet),

		% Tell purity.m to change the mode of the `aditi__state'
		% arguments of the closure to `unused', to make sure
		% that the closure does not call any Aditi relations.
		% We don't know which argument is the `aditi__state' until
		% after typechecking.
		% The `aditi__state's are passed even though they are not
		% used to make the arguments of the closure match the
		% arguments of the relation being updated.
		FixModes = modes_need_fixing,

		% Build the lambda expression for the modification condition.
		make_atomic_unification(LambdaVar,
			lambda_goal((pure), LambdaPredOrFunc, EvalMethod,
				FixModes, LambdaNonLocals,
				HeadArgs, LambdaModes, Detism, PredGoal),
			Context, MainContext, [], LambdaConstruct, !Info),

		make_fresh_arg_var(AditiState0Term, AditiState0Var, [],
			!VarSet, !SInfo, !IO),
		make_fresh_arg_var(AditiStateTerm, AditiStateVar, [],
			!VarSet, !SInfo, !IO),
		AllArgs = [LambdaVar, AditiState0Var, AditiStateVar],

		% post_typecheck.m will fill this in.
		GenericCallModes = [],

		Call = generic_call(aditi_builtin(Builtin, ModifiedCallId),
			AllArgs, GenericCallModes, det) - GoalInfo,

		%
		% Wrap an explicit quantification around the goal to make
		% sure that the closure construction and the
		% `aditi_delete' or `aditi_modify' call are not separated.
		% Separating the goals would make optimization of the update
		% using indexes more difficult.
		%
		UpdateConj = some([], cannot_remove,
			conj([LambdaConstruct, Call]) - GoalInfo) - GoalInfo,

		CallId = call(generic_call(
			aditi_builtin(Builtin, ModifiedCallId))),

		record_called_pred_or_func(PredOrFunc, SymName, PredArity,
			!Info),
		insert_arg_unifications(AllArgs,
			[term__variable(LambdaVar), AditiState0Term,
				AditiStateTerm],
			Context, CallId, UpdateConj,
			UpdateGoal, !VarSet, !Info, !SInfo, !IO)
	;
		%
		% Second syntax -
		% aditi_bulk_delete(pred p/3,
		%	(aditi_bottom_up pred(..) :- ..), DB0, DB).
		%
		% The `pred_term' syntax parsed above is transformed
		% into the equivalent of this syntax.
		%
		Args0 = [PredCallIdTerm | OtherArgs0],
		OtherArgs0 = [_, _, _],

		parse_pred_or_func_name_and_arity(PredCallIdTerm,
			PredOrFunc, SymName, Arity0),
		adjust_func_arity(PredOrFunc, Arity0, Arity)
	->
		Syntax = sym_name_and_closure,

		make_fresh_arg_vars(OtherArgs0,
			OtherArgs, !VarSet, !SInfo, !IO),
		PredId = invalid_pred_id,

		Builtin = aditi_bulk_update(Update, PredId, Syntax),

		ModifiedCallId = PredOrFunc - SymName/Arity,

		% post_typecheck.m will fill this in.
		GenericCallModes = [],

		Call = generic_call(aditi_builtin(Builtin, ModifiedCallId),
			OtherArgs, GenericCallModes, det) - GoalInfo,
		CallId = call(generic_call(
			aditi_builtin(Builtin, ModifiedCallId))),
		record_called_pred_or_func(PredOrFunc, SymName, Arity,
			!Info),
		insert_arg_unifications(OtherArgs, OtherArgs0, Context, CallId,
			Call, UpdateGoal, !VarSet, !Info, !SInfo, !IO)
	;
		invalid_goal(Descr, Args0, GoalInfo,
			UpdateGoal, !VarSet, !SInfo, !IO),
		qual_info_set_found_syntax_error(yes, !.Info ^ qual_info,
			QualInfo),
		!:Info = !.Info ^ qual_info := QualInfo,
		io__set_exit_status(1, !IO),
		output_expected_aditi_update_syntax(Context, Update, !IO)
	).

:- pred aditi_bulk_update_goal_info(aditi_bulk_update::in, pred_or_func::in,
	sym_name::in, arity::in, list(prog_var)::in, pred_or_func::out,
	lambda_eval_method::out, list(mode)::out, determinism::out,
	hlds_goal::in, hlds_goal::out) is det.

aditi_bulk_update_goal_info(bulk_insert, PredOrFunc, _SymName,
		PredArity, _Args, LambdaPredOrFunc, EvalMethod,
		LambdaModes, Detism, Goal, Goal) :-
	LambdaPredOrFunc = PredOrFunc,
	EvalMethod = (aditi_bottom_up),
	out_mode(OutMode),
	Detism = nondet,
	% Modes for the arguments of the input tuple.
	list__duplicate(PredArity, OutMode, LambdaModes).

aditi_bulk_update_goal_info(bulk_delete, PredOrFunc,
		SymName, PredArity, Args, LambdaPredOrFunc, EvalMethod,
		LambdaModes, Detism, Goal0, Goal) :-
	LambdaPredOrFunc = PredOrFunc,
	EvalMethod = (aditi_bottom_up),
	Detism = nondet,
	out_mode(OutMode),
	list__duplicate(PredArity, OutMode, LambdaModes),

	% Join the result of the deletion goal with the
	% relation to be updated.
	conjoin_aditi_update_goal_with_call(PredOrFunc, SymName,
		Args, Goal0, Goal).

aditi_bulk_update_goal_info(bulk_modify, PredOrFunc,
		SymName, PredArity, Args, LambdaPredOrFunc, EvalMethod,
		LambdaModes, Detism, Goal0, Goal) :-

	% The closure passed to `aditi_modify' and `aditi_bulk_modify'
	% is always a predicate closure.
	LambdaPredOrFunc = predicate,

	out_mode(OutMode),
	EvalMethod = (aditi_bottom_up),
	Detism = nondet,

	% Modes for the arguments corresponding to
	% the input tuple.
	list__duplicate(PredArity, OutMode, DeleteModes),

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
		CallArgs, Goal0, Goal),

	% Modes for the arguments corresponding to
	% the output tuple.
	list__duplicate(PredArity, OutMode, InsertModes),
	list__append(DeleteModes, InsertModes, LambdaModes).

:- pred conjoin_aditi_update_goal_with_call(pred_or_func::in, sym_name::in,
	list(prog_var)::in, hlds_goal::in, hlds_goal::out) is det.

conjoin_aditi_update_goal_with_call(PredOrFunc, SymName, Args, Goal0, Goal) :-
	PredId = invalid_pred_id,
	Goal0 = _ - GoalInfo,

	% The predicate is recorded as used in
	% transform_aditi_tuple_update and
	% transform_aditi_insert_delete_modify
	do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, CallGoal),

	Goal = conj([CallGoal, Goal0]) - GoalInfo.

:- pred output_expected_aditi_update_syntax(prog_context::in,
	aditi_bulk_update::in, io::di, io::uo) is det.

output_expected_aditi_update_syntax(Context, bulk_insert) -->
	output_insert_or_delete_expected_syntax(Context, "aditi_bulk_insert").
output_expected_aditi_update_syntax(Context, bulk_delete) -->
	output_insert_or_delete_expected_syntax(Context, "aditi_bulk_delete").
output_expected_aditi_update_syntax(Context, bulk_modify) -->
	{ Name = "aditi_bulk_modify" },
	prog_out__write_context(Context),
	io__write_string("Error: expected\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	io__write_string(Name),
	io__write_string("(\n"),
	prog_out__write_context(Context),
	io__write_string("    (p(<Args0>) ==> p(<Args>) :- <Goal>),\n"),
	prog_out__write_context(Context),
	io__write_string( "    DB0, DB)'\n"),
	output_aditi_closure_syntax(Context, Name).

:- pred output_insert_or_delete_expected_syntax(prog_context::in, string::in,
	io::di, io::uo) is det.

output_insert_or_delete_expected_syntax(Context, Name) -->
	prog_out__write_context(Context),
	io__write_string("Error: expected `"),
	io__write_string(Name),
	io__write_string("((p(<Args>) :- <Goal>), DB0, DB)'\n"),
	output_aditi_closure_syntax(Context, Name).

:- pred output_aditi_closure_syntax(prog_context::in, string::in,
	io::di, io::uo) is det.

output_aditi_closure_syntax(Context, Name) -->
	prog_out__write_context(Context),
	io__write_string("  or `"),
	io__write_string(Name),
	io__write_string("(PredOrFunc p/N, Closure, DB0, DB)'.\n").

	% Report an error for an Aditi update with the wrong number
	% of arguments.
:- pred aditi_update_arity_error(prog_context::in, string::in, int::in,
	list(int)::in, io::di, io::uo) is det.

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
:- pred invalid_goal(string::in, list(prog_term)::in, hlds_goal_info::in,
	hlds_goal::out, prog_varset::in, prog_varset::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

invalid_goal(UpdateStr, Args0, GoalInfo, Goal, !VarSet, !SInfo, !IO) :-
	make_fresh_arg_vars(Args0, HeadVars, !VarSet, !SInfo, !IO),
	MaybeUnifyContext = no,
	Goal = call(invalid_pred_id, invalid_proc_id, HeadVars, not_builtin,
		MaybeUnifyContext, unqualified(UpdateStr)) - GoalInfo.

%-----------------------------------------------------------------------------

	% `insert_arg_unifications' takes a list of variables,
	% a list of terms to unify them with, and a goal, and
	% inserts the appropriate unifications onto the front of
	% the goal.  It calls `unravel_unification' to ensure
	% that each unification gets reduced to superhomogeneous form.
	% It also gets passed an `arg_context', which indicates
	% where the terms came from.

	% We never insert unifications of the form X = X.

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

:- pred insert_arg_unifications(list(prog_var)::in, list(prog_term)::in,
	prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

insert_arg_unifications(HeadVars, Args0, Context, ArgContext,
		!Goal, !VarSet, !Info, !SInfo, !IO) :-
	( HeadVars = [] ->
		true
	;
		!.Goal = _ - GoalInfo0,
		goal_to_conj_list(!.Goal, List0),
		substitute_state_var_mappings(Args0, Args, !VarSet, !SInfo,
			!IO),
		insert_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			0, List0, List, !VarSet, !Info, !SInfo, !IO),
		goal_info_set_context(GoalInfo0, Context, GoalInfo),
		conj_list_to_goal(List, GoalInfo, !:Goal)
	).

:- pred insert_arg_unifications_2(list(prog_var)::in, list(prog_term)::in,
	prog_context::in, arg_context::in, int::in,
	list(hlds_goal)::in, list(hlds_goal)::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

insert_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, _, _, _, _, !IO) :-
	error("insert_arg_unifications_2: length mismatch").
insert_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, _, _, _, _, !IO) :-
	error("insert_arg_unifications_2: length mismatch").
insert_arg_unifications_2([], [], _, _, _, !List, !VarSet, !Info, !SInfo, !IO).
insert_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext, N0,
		!List, !VarSet, !Info, !SInfo, !IO) :-
	N1 = N0 + 1,
	insert_arg_unification(Var, Arg, Context, ArgContext, N1,
		!VarSet, ArgUnifyConj, !Info, !SInfo, !IO),
	( ArgUnifyConj = [] ->
		insert_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
			!List, !VarSet, !Info, !SInfo, !IO)
	;
		insert_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
			!List, !VarSet, !Info, !SInfo, !IO),
		list__append(ArgUnifyConj, !.List, !:List)
	).

:- pred insert_arg_unifications_with_supplied_contexts(list(prog_var)::in,
	list(prog_term)::in, assoc_list(int, arg_context)::in,
	prog_context::in, hlds_goal::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

insert_arg_unifications_with_supplied_contexts(ArgVars, ArgTerms0, ArgContexts,
		Context, !Goal, !VarSet, !Info, !SInfo, !IO) :-
	( ArgVars = [] ->
		true
	;
		!.Goal = _ - GoalInfo0,
		goal_to_conj_list(!.Goal, GoalList0),
		substitute_state_var_mappings(ArgTerms0, ArgTerms,
			!VarSet, !SInfo, !IO),
		insert_arg_unifications_with_supplied_contexts_2(ArgVars,
			ArgTerms, ArgContexts, Context, GoalList0, GoalList,
			!VarSet, !Info, !SInfo, !IO),
		goal_info_set_context(GoalInfo0, Context, GoalInfo),
		conj_list_to_goal(GoalList, GoalInfo, !:Goal)
	).

:- pred insert_arg_unifications_with_supplied_contexts_2(list(prog_var)::in,
	list(prog_term)::in, assoc_list(int, arg_context)::in,
	prog_context::in, list(hlds_goal)::in, list(hlds_goal)::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

insert_arg_unifications_with_supplied_contexts_2(Vars, Terms, ArgContexts,
		Context, !List, !VarSet, !Info, !SInfo, !IO) :-
	(
		( Vars = [], Terms = [], ArgContexts = [] )
	->
		true
	;
		Vars = [Var | Vars1],
		Terms = [Term | Terms1],
		ArgContexts = [ArgNumber - ArgContext | ArgContexts1]
	->
		insert_arg_unification(Var, Term, Context, ArgContext,
			ArgNumber, !VarSet, UnifyConj, !Info, !SInfo, !IO),
		insert_arg_unifications_with_supplied_contexts_2(Vars1, Terms1,
			ArgContexts1, Context, !List, !VarSet, !Info, !SInfo,
			!IO),
		list__append(UnifyConj, !.List, !:List)
	;
		error("insert_arg_unifications_with_supplied_contexts")
	).

:- pred insert_arg_unification(prog_var::in, prog_term::in, prog_context::in,
	arg_context::in, int::in, prog_varset::in, prog_varset::out,
	list(hlds_goal)::out, transform_info::in, transform_info::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

insert_arg_unification(Var, Arg, Context, ArgContext, N1, !VarSet,
		ArgUnifyConj, !Info, !SInfo, !IO) :-
	( Arg = term__variable(Var) ->
		% Skip unifications of the form `X = X'
		ArgUnifyConj = []
	;
		arg_context_to_unify_context(ArgContext, N1,
			UnifyMainContext, UnifySubContext),
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			pure, Goal, !VarSet, !Info, !SInfo, !IO),
		goal_to_conj_list(Goal, ArgUnifyConj)
	).

	% append_arg_unifications is the same as insert_arg_unifications,
	% except that the unifications are added after the goal rather
	% than before the goal.

:- pred append_arg_unifications(list(prog_var)::in, list(prog_term)::in,
	prog_context::in, arg_context::in, hlds_goal::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

append_arg_unifications(HeadVars, Args0, Context, ArgContext, !Goal, !VarSet,
		!Info, !SInfo, !IO) :-
	( HeadVars = [] ->
		true
	;
		!.Goal = _ - GoalInfo,
		goal_to_conj_list(!.Goal, List0),
		substitute_state_var_mappings(Args0, Args, !VarSet,
			!SInfo, !IO),
		append_arg_unifications_2(HeadVars, Args, Context, ArgContext,
			0, List0, List, !VarSet, !Info, !SInfo, !IO),
		conj_list_to_goal(List, GoalInfo, !:Goal)
	).

:- pred append_arg_unifications_2(list(prog_var)::in, list(prog_term)::in,
	prog_context::in, arg_context::in, int::in,
	list(hlds_goal)::in, list(hlds_goal)::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

append_arg_unifications_2([], [_|_], _, _, _, _, _, _, _, _, _, _, _, !IO) :-
	error("append_arg_unifications_2: length mismatch").
append_arg_unifications_2([_|_], [], _, _, _, _, _, _, _, _, _, _, _, !IO) :-
	error("append_arg_unifications_2: length mismatch").
append_arg_unifications_2([], [], _, _, _, !List, !VarSet, !Info, !SInfo, !IO).
append_arg_unifications_2([Var|Vars], [Arg|Args], Context, ArgContext, N0,
		!List, !VarSet, !Info, !SInfo, !IO) :-
	N1 = N0 + 1,
	append_arg_unification(Var, Arg, Context, ArgContext, N1, ConjList,
		!VarSet, !Info, !SInfo, !IO),
	list__append(!.List, ConjList, !:List),
	append_arg_unifications_2(Vars, Args, Context, ArgContext, N1,
		!List, !VarSet, !Info, !SInfo, !IO).

:- pred append_arg_unification(prog_var::in, prog_term::in, prog_context::in,
	arg_context::in, int::in, list(hlds_goal)::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

append_arg_unification(Var, Arg, Context, ArgContext, N1, ConjList,
		!VarSet, !Info, !SInfo, !IO) :-
	( Arg = term__variable(Var) ->
		% skip unifications of the form `X = X'
		ConjList = []
	;
		arg_context_to_unify_context(ArgContext, N1,
			UnifyMainContext, UnifySubContext),
		unravel_unification(term__variable(Var), Arg,
			Context, UnifyMainContext, UnifySubContext,
			pure, Goal, !VarSet, !Info, !SInfo, !IO),
		goal_to_conj_list(Goal, ConjList)
	).

:- pred arg_context_to_unify_context(arg_context::in, int::in,
	unify_main_context::out, unify_sub_contexts::out) is det.

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

	% make_fresh_arg_vars(Args, VarSet0, Vars, VarSet, !SInfo, !IO):
	%	`Vars' is a list of distinct variables corresponding to
	%	the terms in `Args'.  For each term in `Args', if
	%	the term is a variable V which is distinct from the
	%	variables already produced, then the corresponding
	%	variable in `Vars' is just V, otherwise a fresh variable
	%	is allocated from `VarSet0'.   `VarSet' is the resulting
	%	varset after all the necessary variables have been allocated.
	%	!SInfo and !IO are required to handle state variables.
	%
	%	For efficiency, the list `Vars' is constructed backwards
	%	and then reversed to get the correct order.

:- pred make_fresh_arg_vars(list(prog_term)::in, list(prog_var)::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

make_fresh_arg_vars(Args, Vars, !VarSet, !SInfo, !IO) :-
	make_fresh_arg_vars_2(Args, [], Vars1, !VarSet, !SInfo, !IO),
	list__reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(prog_term)::in, list(prog_var)::in,
	list(prog_var)::out, prog_varset::in,prog_varset::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

make_fresh_arg_vars_2([], Vars, Vars, !VarSet, !SInfo, !IO).
make_fresh_arg_vars_2([Arg | Args], Vars0, Vars, !VarSet, !SInfo, !IO) :-
	make_fresh_arg_var(Arg, Var, Vars0, !VarSet, !SInfo, !IO),
	make_fresh_arg_vars_2(Args, [Var | Vars0], Vars, !VarSet, !SInfo, !IO).

:- pred make_fresh_arg_var(prog_term::in, prog_var::out, list(prog_var)::in,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

make_fresh_arg_var(Arg0, Var, Vars0, !VarSet, !SInfo, !IO) :-
	substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !IO),
	(
		Arg = term__variable(ArgVar),
		\+ list__member(ArgVar, Vars0)
	->
		Var = ArgVar
	;
		varset__new_var(!.VarSet, Var, !:VarSet)
	).

%-----------------------------------------------------------------------------%

	%
	% XXX We could do better on the error messages for
	% lambda expressions and field extraction and update expressions.
	%
:- pred unravel_unification(prog_term::in, prog_term::in, prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, purity::in,
	hlds_goal::out, prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

unravel_unification(LHS0, RHS0, Context, MainContext, SubContext,
		Purity, Goal, !VarSet, !Info, !SInfo, !IO) :-
	substitute_state_var_mapping(LHS0, LHS, !VarSet, !SInfo, !IO),
	substitute_state_var_mapping(RHS0, RHS, !VarSet, !SInfo, !IO),
	unravel_unification_2(LHS, RHS, Context, MainContext, SubContext,
		Purity, Goal, !VarSet, !Info, !SInfo,!IO).

:- pred unravel_unification_2(prog_term::in, prog_term::in, prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, purity::in,
	hlds_goal::out, prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

	% `X = Y' needs no unravelling.

unravel_unification_2(term__variable(X), term__variable(Y), Context,
		MainContext, SubContext, Purity, Goal, !VarSet, !Info, !SInfo,
		!IO) :-
	make_atomic_unification(X, var(Y), Context, MainContext,
		SubContext, Goal, !Info),
	check_expr_purity(Purity, Context, !Info, !IO).

	% If we find a unification of the form
	%	X = f(A1, A2, A3)
	% we replace it with
	%	X = f(NewVar1, NewVar2, NewVar3),
	%	NewVar1 = A1,
	%	NewVar2 = A2,
	%	NewVar3 = A3.
	% In the trivial case `X = c', no unravelling occurs.

unravel_unification_2(term__variable(X), RHS, Context, MainContext, SubContext,
		Purity, Goal, !VarSet, !Info, !SInfo, !IO) :-
	RHS = term__functor(F, Args1, FunctorContext),
	substitute_state_var_mappings(Args1, Args, !VarSet, !SInfo, !IO),
	(
		% Handle explicit type qualification.
		F = term__atom("with_type"),
		Args = [RVal, DeclType0]
	->
		convert_type(DeclType0, DeclType),
		varset__coerce(!.VarSet, DeclVarSet),
		process_type_qualification(X, DeclType, DeclVarSet,
			Context, !Info, !IO),
		unravel_unification(term__variable(X), RVal, Context,
			MainContext, SubContext, Purity, Goal,
			!VarSet, !Info, !SInfo, !IO)
	;
		% Handle unification expressions.
		F = term__atom("@"),
		Args = [LVal, RVal]
	->
		unravel_unification(term__variable(X), LVal, Context,
			MainContext, SubContext, Purity, Goal1,
			!VarSet, !Info, !SInfo, !IO),
		unravel_unification(term__variable(X), RVal, Context,
			MainContext, SubContext, Purity, Goal2,
			!VarSet, !Info, !SInfo, !IO),
		goal_info_init(GoalInfo),
		goal_to_conj_list(Goal1, ConjList1),
		goal_to_conj_list(Goal2, ConjList2),
		list__append(ConjList1, ConjList2, ConjList),
		conj_list_to_goal(ConjList, GoalInfo, Goal)
	;
		(
			% handle lambda expressions
			parse_lambda_eval_method(RHS, EvalMethod0, RHS1),
			RHS1 = term__functor(term__atom("lambda"), Args1, _),
			Args1 = [LambdaExpressionTerm0, GoalTerm0],
			term__coerce(LambdaExpressionTerm0,
				LambdaExpressionTerm),
			parse_lambda_expression(LambdaExpressionTerm,
				Vars0, Modes0, Det0)
		->
			LambdaPurity = (pure),
			PredOrFunc = predicate,
			EvalMethod = EvalMethod0,
			Vars1 = Vars0,
			Modes1 = Modes0,
			Det1 = Det0,
			GoalTerm1 = GoalTerm0,
			WarnDeprecatedLambda = yes
		;
			% handle higher-order pred and func expressions -
			% same semantics as lambda expressions, different
			% syntax (the original lambda expression syntax
			% is now deprecated)
			parse_rule_term(Context, RHS, HeadTerm0, GoalTerm1),
			term__coerce(HeadTerm0, HeadTerm1),
			parse_purity_annotation(HeadTerm1, LambdaPurity,
				HeadTerm),
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
			),
			WarnDeprecatedLambda = no
		)
	->
		( WarnDeprecatedLambda = yes ->
			report_warning(Context, 0,
			[words("Warning: deprecated lambda expression syntax."), nl,
			 words("Lambda expressions with lambda as the top-level functor"),
			 words("are deprecated; please use the form using pred instead.")],
				!IO)
		;
			true
		),
		check_expr_purity(Purity, Context, !Info, !IO),
		make_hlds__qualify_lambda_mode_list(Modes1, Modes, Context,
			!Info, !IO),
		Det = Det1,
		term__coerce(GoalTerm1, GoalTerm),
		parse_goal(GoalTerm, ParsedGoal, !VarSet),
		build_lambda_expression(X, LambdaPurity, PredOrFunc,
			EvalMethod, Vars1, Modes, Det, ParsedGoal,
			Context, MainContext, SubContext, Goal, !VarSet,
			!Info, !.SInfo, !IO)
	;
		% handle higher-order dcg pred expressions -
		% same semantics as higher-order pred expressions,
		% but has two extra arguments, and the goal is expanded
		% as a DCG goal.
		F = term__atom("-->"),
		Args = [PredTerm0, GoalTerm0],
		term__coerce(PredTerm0, PredTerm1),
		parse_purity_annotation(PredTerm1, DCGLambdaPurity, PredTerm),
		parse_dcg_pred_expression(PredTerm, EvalMethod,
			Vars0, Modes0, Det)
	->
		make_hlds__qualify_lambda_mode_list(Modes0, Modes, Context,
			!Info, !IO),
		term__coerce(GoalTerm0, GoalTerm),
		parse_dcg_pred_goal(GoalTerm, ParsedGoal, DCG0, DCGn, !VarSet),
		list__append(Vars0,
			[term__variable(DCG0), term__variable(DCGn)], Vars1),
		build_lambda_expression(X, DCGLambdaPurity, predicate,
			EvalMethod, Vars1, Modes, Det, ParsedGoal,
			Context, MainContext, SubContext, Goal0, !VarSet,
			!Info, !.SInfo, !IO),
		Goal0 = GoalExpr - GoalInfo0,
		add_goal_info_purity_feature(GoalInfo0, Purity, GoalInfo),
		Goal = GoalExpr - GoalInfo
	;
		% handle if-then-else expressions
		(
			F = term__atom("else"),
			Args = [term__functor(term__atom("if"),
					[term__functor(term__atom("then"),
						[IfTerm0, ThenTerm], _)], _),
				ElseTerm]
		;
			F = term__atom(";"),
			Args = [term__functor(term__atom("->"),
				[IfTerm0, ThenTerm], _),
				ElseTerm]
		),
		term__coerce(IfTerm0, IfTerm),
		parse_some_vars_goal(IfTerm, Vars, StateVars,
			IfParseTree, !VarSet)
	->
		BeforeSInfo = !.SInfo,
		prepare_for_if_then_else_expr(StateVars, !VarSet, !SInfo),

		check_expr_purity(Purity, Context, !Info, !IO),
		map__init(EmptySubst),
		transform_goal(IfParseTree, EmptySubst, IfGoal, !VarSet,
			!Info, !SInfo, !IO),

		finish_if_then_else_expr_condition(BeforeSInfo, !SInfo),

		unravel_unification(term__variable(X), ThenTerm,
			Context, MainContext, SubContext, pure, ThenGoal,
			!VarSet, !Info, !SInfo, !IO),

		finish_if_then_else_expr_then_goal(StateVars, BeforeSInfo,
			!SInfo),

		unravel_unification(term__variable(X), ElseTerm,
			Context, MainContext, SubContext, pure,
			ElseGoal, !VarSet, !Info, !SInfo, !IO),

		IfThenElse = if_then_else(StateVars ++ Vars, IfGoal,
			ThenGoal, ElseGoal),
		goal_info_init(Context, GoalInfo),
		Goal = IfThenElse - GoalInfo
	;
		% handle field extraction expressions
		F = term__atom("^"),
		Args = [InputTerm, FieldNameTerm],
		parse_field_list(FieldNameTerm, FieldNameResult),
		FieldNameResult = ok(FieldNames)
	->
		check_expr_purity(Purity, Context, !Info, !IO),
		make_fresh_arg_var(InputTerm, InputTermVar, [], !VarSet,
			!SInfo, !IO),
		expand_get_field_function_call(Context, MainContext,
			SubContext, FieldNames, X, InputTermVar,
			!VarSet, Functor, _, Goal0, !Info, !SInfo, !IO),

		ArgContext = functor(Functor, MainContext, SubContext),
		insert_arg_unifications([InputTermVar], [InputTerm],
			FunctorContext, ArgContext, Goal0,
			Goal, !VarSet, !Info, !SInfo, !IO)
	;
		% handle field update expressions
		F = term__atom(":="),
		Args = [FieldDescrTerm, FieldValueTerm],
		FieldDescrTerm = term__functor(term__atom("^"),
			[InputTerm, FieldNameTerm], _),
		parse_field_list(FieldNameTerm, FieldNameResult),
		FieldNameResult = ok(FieldNames)
	->
		check_expr_purity(Purity, Context, !Info, !IO),
		make_fresh_arg_var(InputTerm, InputTermVar, [], !VarSet,
			!SInfo, !IO),
		make_fresh_arg_var(FieldValueTerm, FieldValueVar,
			[InputTermVar], !VarSet, !SInfo, !IO),

		expand_set_field_function_call(Context, MainContext,
			SubContext, FieldNames, FieldValueVar, InputTermVar, X,
			!VarSet, Functor, InnerFunctor - FieldSubContext,
			Goal0, !Info, !SInfo, !IO),

		TermArgContext = functor(Functor, MainContext, SubContext),
		TermArgNumber = 1,
		FieldArgContext = functor(InnerFunctor,
			MainContext, FieldSubContext),
		FieldArgNumber = 2,
		ArgContexts = [TermArgNumber - TermArgContext,
				FieldArgNumber - FieldArgContext],
		insert_arg_unifications_with_supplied_contexts(
			[InputTermVar, FieldValueVar],
			[InputTerm, FieldValueTerm], ArgContexts,
			Context, Goal0, Goal, !VarSet, !Info, !SInfo, !IO)
	;
		parse_qualified_term(RHS, RHS, "", MaybeFunctor),
		(
			MaybeFunctor = ok(FunctorName, FunctorArgs),
			list__length(FunctorArgs, Arity),
			ConsId = cons(FunctorName, Arity)
		;
			% float, int or string constant
			% 	- any errors will be caught by typechecking
			MaybeFunctor = error(_, _),
			list__length(Args, Arity),
			ConsId = make_functor_cons_id(F, Arity),
			FunctorArgs = Args
		),
		( FunctorArgs = [] ->
			make_atomic_unification(X, functor(ConsId, no, []),
				Context, MainContext, SubContext, Goal0,
				!Info),
			Goal0 = GoalExpr - GoalInfo0,
			add_goal_info_purity_feature(GoalInfo0, Purity,
				GoalInfo),
			Goal = GoalExpr - GoalInfo
		;
			make_fresh_arg_vars(FunctorArgs, HeadVars, !VarSet,
				!SInfo, !IO),
			make_atomic_unification(X,
				functor(ConsId, no, HeadVars), Context,
				MainContext, SubContext, Goal0,
				!Info),
			ArgContext = functor(ConsId, MainContext, SubContext),
			% Should this be insert_... rather than append_...?
			% No, because that causes efficiency problems
			% with type-checking :-(
			% But for impure unifications, we need to do
			% this, because mode reordering can't reorder
			% around the functor unification.
			( Purity = pure ->
				append_arg_unifications(HeadVars, FunctorArgs,
					FunctorContext, ArgContext,
					Goal0, Goal, !VarSet, !Info, !SInfo,
					!IO)
			;
				Goal0 = GoalExpr - GoalInfo0,
				add_goal_info_purity_feature(GoalInfo0,
					Purity, GoalInfo),
				Goal1 = GoalExpr - GoalInfo,
				insert_arg_unifications(HeadVars, FunctorArgs,
					FunctorContext, ArgContext,
					Goal1, Goal, !VarSet, !Info, !SInfo,
					!IO)
			)
		)
	).

	% Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification_2(term__functor(F, As, FC), term__variable(Y),
		C, MC, SC, Purity, Goal, !VarSet, !Info, !SInfo, !IO) :-
	unravel_unification(term__variable(Y), term__functor(F, As, FC),
		C, MC, SC, Purity, Goal, !VarSet, !Info, !SInfo, !IO).

	% If we find a unification of the form `f1(...) = f2(...)',
	% then we replace it with `Tmp = f1(...), Tmp = f2(...)',
	% and then process it according to the rule above.
	% Note that we can't simplify it yet, because we might simplify
	% away type errors.

unravel_unification_2(term__functor(LeftF, LeftAs, LeftC),
		term__functor(RightF, RightAs, RightC),
		Context, MainContext, SubContext,
		Purity, Goal, !VarSet, !Info, !SInfo, !IO) :-
	varset__new_var(!.VarSet, TmpVar, !:VarSet),
	unravel_unification(term__variable(TmpVar),
		term__functor(LeftF, LeftAs, LeftC),
		Context, MainContext, SubContext,
		Purity, Goal0, !VarSet, !Info, !SInfo, !IO),
	unravel_unification(term__variable(TmpVar),
		term__functor(RightF, RightAs, RightC),
		Context, MainContext, SubContext,
		Purity, Goal1, !VarSet, !Info, !SInfo, !IO),
	goal_info_init(GoalInfo),
	goal_to_conj_list(Goal0, ConjList0),
	goal_to_conj_list(Goal1, ConjList1),
	list__append(ConjList0, ConjList1, ConjList),
	conj_list_to_goal(ConjList, GoalInfo, Goal).

:- pred parse_purity_annotation(term(T)::in, purity::out, term(T)::out) is det.

parse_purity_annotation(Term0, Purity, Term) :-
	(
		Term0 = term__functor(term__atom(PurityName), [Term1], _),
		purity_name(Purity0, PurityName)
	->
		Purity = Purity0,
		Term = Term1
	;
		Purity = (pure),
		Term = Term0
	).

:- pred make_hlds__qualify_lambda_mode_list(list(mode)::in, list(mode)::out,
	prog_context::in, transform_info::in, transform_info::out,
	io::di, io::uo) is det.

make_hlds__qualify_lambda_mode_list(Modes0, Modes, Context, !Info, !IO) :-
	% The modes in `.opt' files are already fully module qualified.
	( !.Info ^ qual_info ^ import_status \= opt_imported ->
		qual_info_get_mq_info(!.Info ^ qual_info, MQInfo0),
		module_qual__qualify_lambda_mode_list(Modes0, Modes,
			Context, MQInfo0, MQInfo1, !IO),
		qual_info_set_mq_info(MQInfo1, !.Info ^ qual_info,
			QualInfo1),
		!:Info = !.Info ^ qual_info := QualInfo1
	;
		Modes = Modes0
	).

%-----------------------------------------------------------------------------%

:- pred check_expr_purity(purity::in, prog_context::in,
	transform_info::in, transform_info::out, io::di, io::uo) is det.

check_expr_purity(Purity, Context, !Info, !IO) :-
	( Purity \= pure ->
		impure_unification_expr_error(Context, Purity, !IO),
		module_info_incr_errors(!.Info ^ module_info, ModuleInfo),
		!:Info = !.Info ^ module_info := ModuleInfo
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Parse a term of the form `Head :- Body', treating
	% a term not in that form as `Head :- true'.
:- pred parse_rule_term(term__context::in, term(T)::in, term(T)::out,
	term(T)::out) is det.

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

:- pred build_lambda_expression(prog_var::in, purity::in, pred_or_func::in,
	lambda_eval_method::in, list(prog_term)::in, list(mode)::in,
	determinism::in, goal::in, prog_context::in, unify_main_context::in,
	unify_sub_contexts::in, hlds_goal::out,
	prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out,
	svar_info::in, io::di, io::uo) is det.

build_lambda_expression(X, Purity, PredOrFunc, EvalMethod, Args0, Modes, Det,
		ParsedGoal, Context, MainContext, SubContext, Goal, !VarSet,
		!Info, SInfo0, !IO) :-
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
	(
		illegal_state_var_func_result(PredOrFunc, Args0, StateVar)
	->
		report_illegal_func_svar_result(Context, !.VarSet, StateVar,
			!IO),
		true_goal(Goal)
	;
		lambda_args_contain_bang_state_var(Args0, StateVar)
	->
		report_illegal_bang_svar_lambda_arg(Context, !.VarSet,
			StateVar, !IO),
		true_goal(Goal)
	;
		prepare_for_lambda(SInfo0, SInfo1),
		substitute_state_var_mappings(Args0, Args, !VarSet,
			SInfo1, SInfo2, !IO),

		list__length(Args, NumArgs),
		varset__new_vars(!.VarSet, NumArgs, LambdaVars, !:VarSet),
		map__init(Substitution),
		hlds_goal__true_goal(Head0),
		ArgContext = head(PredOrFunc, NumArgs),

		insert_arg_unifications(LambdaVars, Args, Context, ArgContext,
			Head0, Head, !VarSet, !Info, SInfo2, SInfo3, !IO),

		prepare_for_body(FinalSVarMap, !VarSet, SInfo3, SInfo4),

		transform_goal(ParsedGoal, Substitution,
			Body, !VarSet, !Info, SInfo4, SInfo5, !IO),

		finish_head_and_body(Context, FinalSVarMap,
			Head, Body, HLDS_Goal0, SInfo5),

		%
		% Now figure out which variables we need to
		% explicitly existentially quantify.
		%
		(
			PredOrFunc = predicate,
			QuantifiedArgs = Args
		;
			PredOrFunc = function,
			pred_args_to_func_args(Args, QuantifiedArgs,
				_ReturnValTerm)
		),
		term__vars_list(QuantifiedArgs, QuantifiedVars0),
		list__sort_and_remove_dups(QuantifiedVars0, QuantifiedVars),

		goal_info_init(Context, GoalInfo),
		HLDS_Goal = some(QuantifiedVars, can_remove, HLDS_Goal0) -
			GoalInfo,

		%
		% We set the lambda nonlocals here to anything that
		% could possibly be nonlocal.  Quantification will
		% reduce this down to the proper set of nonlocal arguments.
		%
		goal_util__goal_vars(HLDS_Goal, LambdaGoalVars0),
		set__delete_list(LambdaGoalVars0, LambdaVars,
			LambdaGoalVars1),
		set__delete_list(LambdaGoalVars1, QuantifiedVars,
			LambdaGoalVars2),
		set__to_sorted_list(LambdaGoalVars2, LambdaNonLocals),

		make_atomic_unification(X,
			lambda_goal(Purity, PredOrFunc, EvalMethod,
				modes_are_ok, LambdaNonLocals, LambdaVars,
				Modes, Det, HLDS_Goal),
			Context, MainContext, SubContext, Goal, !Info)
	).

%-----------------------------------------------------------------------------%

:- pred construct_pred_or_func_call(pred_id::in, pred_or_func::in,
	sym_name::in, list(prog_var)::in, hlds_goal_info::in, hlds_goal::out,
	transform_info::in, transform_info::out) is det.

construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args, GoalInfo, Goal,
		!Info) :-
	do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, Goal),
	list__length(Args, Arity),
	adjust_func_arity(PredOrFunc, OrigArity, Arity),
	record_called_pred_or_func(PredOrFunc, SymName, OrigArity, !Info).

:- pred do_construct_pred_or_func_call(pred_id::in, pred_or_func::in,
	sym_name::in, list(prog_var)::in, hlds_goal_info::in, hlds_goal::out)
	is det.

do_construct_pred_or_func_call(PredId, PredOrFunc, SymName, Args,
		GoalInfo, Goal) :-
	(
		PredOrFunc = predicate,
		Goal = call(PredId, invalid_proc_id, Args,
			not_builtin, no, SymName) - GoalInfo
	;
		PredOrFunc = function,
		pred_args_to_func_args(Args, FuncArgs, RetArg),
		list__length(FuncArgs, Arity),
		ConsId = cons(SymName, Arity),
		goal_info_get_context(GoalInfo, Context),
		hlds_goal__create_atomic_unification(RetArg,
			functor(ConsId, no, FuncArgs), Context,
			explicit, [], GoalExpr - _),
		Goal = GoalExpr - GoalInfo
	).

:- pred make_atomic_unification(prog_var::in, unify_rhs::in, prog_context::in,
	unify_main_context::in, unify_sub_contexts::in, hlds_goal::out,
	transform_info::in, transform_info::out) is det.

make_atomic_unification(Var, Rhs, Context, MainContext, SubContext,
		Goal, Info0, Info) :-
	(
		Rhs = var(_),
		Info = Info0
	;
		Rhs = lambda_goal(_, _, _, _, _, _, _, _, _),
		Info = Info0
	;
		Rhs = functor(ConsId, _, _),
		record_used_functor(ConsId, Info0, Info)
	),
	hlds_goal__create_atomic_unification(Var, Rhs, Context,
		MainContext, SubContext, Goal).

%-----------------------------------------------------------------------------%

	% Process an explicit type qualification.
:- pred process_type_qualification(prog_var::in, (type)::in, tvarset::in,
	prog_context::in, transform_info::in, transform_info::out,
	io::di, io::uo) is det.

process_type_qualification(Var, Type0, VarSet, Context, !Info, !IO) :-
	!.Info ^ qual_info = qual_info(EqvMap, TVarSet0, TVarRenaming0,
		TVarNameMap0, VarTypes0, MQInfo0, Status, FoundError),
	( Status = opt_imported ->
		% Types in `.opt' files should already be
		% fully module qualified.
		Type1 = Type0,
		MQInfo = MQInfo0
	;
		module_qual__qualify_type_qualification(Type0, Type1,
			Context, MQInfo0, MQInfo, !IO)
	),

	% Find any new type variables introduced by this type, and
	% add them to the var-name index and the variable renaming.
	term__vars(Type1, TVars),
	get_new_tvars(TVars, VarSet, TVarSet0, TVarSet1,
		TVarNameMap0, TVarNameMap, TVarRenaming0, TVarRenaming),

	% Apply the updated renaming to convert type variables in
	% the clause to type variables in the tvarset.
	term__apply_variable_renaming(Type1, TVarRenaming, Type2),

	% Expand equivalence types.
	% We don't need to record the expanded types for smart recompilation
	% because at the moment no recompilation.item_id can depend on a
	% clause item.
	RecordExpanded = no,
	equiv_type__replace_in_type(EqvMap, Type2, Type, _, TVarSet1, TVarSet,
		RecordExpanded, _),
	update_var_types(Var, Type, Context, VarTypes0, VarTypes, !IO),
	!:Info = !.Info ^ qual_info := qual_info(EqvMap, TVarSet, TVarRenaming,
		TVarNameMap, VarTypes, MQInfo, Status, FoundError).

:- pred update_var_types(prog_var::in, (type)::in, prog_context::in,
	map(prog_var, type)::in, map(prog_var, type)::out, io::di, io::uo)
	is det.

update_var_types(Var, Type, Context, !VarTypes, !IO) :-
	( map__search(!.VarTypes, Var, Type0) ->
		( Type = Type0 ->
			true
		;
			prog_out__write_context(Context, !IO),
			io__write_string("Error: explicit type " ++
				"qualification does\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  not match prior qualification.\n",
				!IO),
			io__set_exit_status(1, !IO)
		)
	;
		map__det_insert(!.VarTypes, Var, Type, !:VarTypes)
	).

	% Add new type variables for those introduced by a type qualification.
:- pred get_new_tvars(list(tvar)::in, tvarset::in, tvarset::in, tvarset::out,
	tvar_name_map::in, tvar_name_map::out,
	map(tvar, tvar)::in, map(tvar, tvar)::out) is det.

get_new_tvars([], _,  !TVarSet, !TVarNameMap, !TVarRenaming).
get_new_tvars([TVar | TVars], VarSet, !TVarSet, !TVarNameMap, !TVarRenaming) :-
	( map__contains(!.TVarRenaming, TVar) ->
		true
	;
		( varset__search_name(VarSet, TVar, TVarName) ->
			( map__search(!.TVarNameMap, TVarName, TVarSetVar) ->
				map__det_insert(!.TVarRenaming,
					TVar, TVarSetVar, !:TVarRenaming)
			;
				varset__new_var(!.TVarSet, NewTVar, !:TVarSet),
				varset__name_var(!.TVarSet, NewTVar, TVarName,
					!:TVarSet),
				map__det_insert(!.TVarNameMap,
					TVarName, NewTVar, !:TVarNameMap),
				map__det_insert(!.TVarRenaming, TVar, NewTVar,
					!:TVarRenaming)
			)
		;
			varset__new_var(!.TVarSet, NewTVar, !:TVarSet),
			map__det_insert(!.TVarRenaming, TVar, NewTVar,
				!:TVarRenaming)
		)
	),
	get_new_tvars(TVars, VarSet, !TVarSet, !TVarNameMap, !TVarRenaming).

%-----------------------------------------------------------------------------%

% substitute_vars(Vars0, Subst, Vars)
%	apply substitiution `Subst' (which must only rename vars) to `Vars0',
%	and return the result in `Vars'.

:- pred substitute_vars(list(var(T))::in, substitution(T)::in,
	list(var(T))::out) is det.

substitute_vars(Vars0, Subst, Vars) :-
	Vars = list__map(substitute_var(Subst), Vars0).

:- func substitute_var(substitution(T), var(T)) = var(T).

substitute_var(Subst, Var0) = Var :-
	term__apply_substitution(term__variable(Var0), Subst, Term),
	( Term = term__variable(Var1) ->
		Var = Var1
	;
		error("substitute_var: invalid substitution")
	).

%-----------------------------------------------------------------------------%

% get_rev_conj(Goal, Subst, RevConj0, RevConj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list (applying Subst),
%	reverse it, append RevConj0, and return the result in RevConj.

:- pred get_rev_conj(goal::in, prog_substitution::in, list(hlds_goal)::in,
	list(hlds_goal)::out, prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

get_rev_conj(Goal, Subst, RevConj0, RevConj, !VarSet, !Info, !SInfo, !IO) :-
	( Goal = (A,B) - _Context ->
		get_rev_conj(A, Subst, RevConj0, RevConj1,
			!VarSet, !Info, !SInfo, !IO),
		get_rev_conj(B, Subst, RevConj1, RevConj,
			!VarSet, !Info, !SInfo, !IO)
	;
		transform_goal(Goal, Subst, Goal1, !VarSet, !Info, !SInfo,
			!IO),
		goal_to_conj_list(Goal1, ConjList),
		RevConj = list__reverse(ConjList) ++ RevConj0
	).

% get_rev_par_conj(Goal, Subst, RevParConj0, RevParConj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list (applying Subst),
%	reverse it, append RevParConj0, and return the result in RevParConj.

:- pred get_rev_par_conj(goal::in, prog_substitution::in, list(hlds_goal)::in,
	list(hlds_goal)::out, prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

get_rev_par_conj(Goal, Subst, RevParConj0, RevParConj, !VarSet, !Info, !SInfo,
		!IO) :-
	( Goal = (A & B) - _Context ->
		get_rev_par_conj(A, Subst, RevParConj0, RevParConj1,
			!VarSet, !Info, !SInfo, !IO),
		get_rev_par_conj(B, Subst, RevParConj1, RevParConj,
			!VarSet, !Info, !SInfo, !IO)
	;
		transform_goal(Goal, Subst, Goal1, !VarSet, !Info, !SInfo,
			!IO),
		goal_to_par_conj_list(Goal1, ParConjList),
		RevParConj = list__reverse(ParConjList) ++ RevParConj0
	).

% get_disj(Goal, Subst, Disj0, Disj) :
% 	Goal is a tree of disjuncts.  Flatten it into a list (applying Subst)
%	append Disj0, and return the result in Disj.

:- pred get_disj(goal::in, prog_substitution::in, hlds_goal_svar_infos::in,
	hlds_goal_svar_infos::out, prog_varset::in, prog_varset::out,
	transform_info::in, transform_info::out, svar_info::in,
	io::di, io::uo) is det.

get_disj(Goal, Subst, Disj0, Disj, !VarSet, !Info, SInfo, !IO) :-
	( Goal = (A;B) - _Context ->
		get_disj(B, Subst, Disj0, Disj1, !VarSet, !Info, SInfo, !IO),
		get_disj(A, Subst, Disj1, Disj, !VarSet, !Info, SInfo, !IO)
	;
		transform_goal(Goal, Subst, Goal1, !VarSet, !Info,
			SInfo, SInfo1, !IO),
		Disj = [{Goal1, SInfo1} | Disj0]
	).

%-----------------------------------------------------------------------------%

	% Information used to process explicit type qualifications.
:- type qual_info
	---> qual_info(
		eqv_map		:: eqv_map,
				% Used to expand equivalence types.
		tvarset		:: tvarset,
				% All type variables for predicate.
		tvar_renaming	:: map(tvar, tvar),
				% Map from clause type variable to
				% actual type variable in tvarset.
		tvar_name_map	:: tvar_name_map,
				% Type variables in tvarset occurring
				% in the predicate's argument types
				% indexed by name.
		vartypes	:: map(prog_var, type), % Var types
		mq_info		:: mq_info,
				% Module qualification info.
		import_status	:: import_status,
		found_syntax_error :: bool
				% Was there a syntax error
				% in an Aditi update.
	).

:- pred init_qual_info(mq_info::in, eqv_map::in, qual_info::out) is det.

init_qual_info(MQInfo0, EqvMap, QualInfo) :-
	mq_info_set_need_qual_flag(MQInfo0, may_be_unqualified, MQInfo),
	varset__init(TVarSet),
	map__init(Renaming),
	map__init(Index),
	map__init(VarTypes),
	FoundSyntaxError = no,
	QualInfo = qual_info(EqvMap, TVarSet, Renaming, Index, VarTypes,
		MQInfo, local, FoundSyntaxError).

	% Update the qual_info when processing a new clause.
:- pred update_qual_info(tvar_name_map::in, tvarset::in,
	map(prog_var, type)::in, import_status::in,
	qual_info::in, qual_info::out) is det.

update_qual_info(TVarNameMap, TVarSet, VarTypes, Status, !QualInfo) :-
	!.QualInfo = qual_info(EqvMap, _TVarSet0, _Renaming0, _TVarNameMap0,
		_VarTypes0, MQInfo, _Status, _FoundError),
	% The renaming for one clause is useless in the others.
	map__init(Renaming),
	!:QualInfo = qual_info(EqvMap, TVarSet, Renaming, TVarNameMap,
		VarTypes, MQInfo, Status, no).

:- pred qual_info_get_mq_info(qual_info::in, mq_info::out) is det.

qual_info_get_mq_info(Info, Info ^ mq_info).

:- pred qual_info_set_mq_info(mq_info::in, qual_info::in, qual_info::out)
	is det.

qual_info_set_mq_info(MQInfo, Info, Info ^ mq_info := MQInfo).

:- pred qual_info_get_var_types(qual_info::in, map(prog_var, type)::out)
	is det.

qual_info_get_var_types(Info, Info ^ vartypes).

:- pred qual_info_get_found_syntax_error(qual_info::in, bool::out) is det.

qual_info_get_found_syntax_error(Info, Info ^ found_syntax_error).

:- pred qual_info_set_found_syntax_error(bool::in,
	qual_info::in, qual_info::out) is det.

qual_info_set_found_syntax_error(FoundError, Info,
	Info ^ found_syntax_error := FoundError).

:- pred apply_to_recompilation_info(
	pred(recompilation_info, recompilation_info)::in(pred(in, out) is det),
	transform_info::in, transform_info::out) is det.

apply_to_recompilation_info(Pred, !Info) :-
	MQInfo0 = !.Info ^ qual_info ^ mq_info,
	mq_info_get_recompilation_info(MQInfo0, MaybeRecompInfo0),
	(
		MaybeRecompInfo0 = yes(RecompInfo0),
		Pred(RecompInfo0, RecompInfo),
		mq_info_set_recompilation_info(MQInfo0,
			yes(RecompInfo), MQInfo),
		!:Info = !.Info ^ qual_info ^ mq_info := MQInfo
	;
		MaybeRecompInfo0 = no
	).

set_module_recompilation_info(QualInfo, !ModuleInfo) :-
	mq_info_get_recompilation_info(QualInfo ^ mq_info, RecompInfo),
	module_info_set_maybe_recompilation_info(RecompInfo, !ModuleInfo).

:- pred record_called_pred_or_func(pred_or_func::in, sym_name::in, arity::in,
	transform_info::in, transform_info::out) is det.

record_called_pred_or_func(PredOrFunc, SymName, Arity) -->
	{ Id = SymName - Arity },
	apply_to_recompilation_info(
		recompilation__record_used_item(
			pred_or_func_to_item_type(PredOrFunc), Id, Id)).

:- pred record_used_functor(cons_id::in,
	transform_info::in, transform_info::out) is det.

record_used_functor(ConsId, !Info) :-
	( ConsId = cons(SymName, Arity) ->
		Id = SymName - Arity,
		apply_to_recompilation_info(
			recompilation__record_used_item(functor, Id, Id),
			!Info)
	;
		true
	).

%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred report_unexpected_decl(string::in, prog_context::in,
	io::di, io::uo) is det.

report_unexpected_decl(Descr, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: unexpected or incorrect `"),
	io__write_string(Descr),
	io__write_string("' declaration.\n").

:- pred multiple_def_error(import_status::in, sym_name::in, int::in,
	string::in, prog_context::in, prog_context::in, bool::out,
	io::di, io::uo) is det.

multiple_def_error(Status, Name, Arity, DefType, Context, OrigContext,
		FoundError) -->
	( { Status \= opt_imported } ->
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
		io__write_string("'.\n"),
		{ FoundError = yes }
	;
		% We don't take care not to read the same declaration
		% from multiple sources with inter-module optimization
		% so ignore multiple definition errors in the items read
		% for inter-module optimization.
		{ FoundError = no }
	).

:- pred undefined_pred_or_func_error(sym_name::in, int::in, prog_context::in,
	string::in, io::di, io::uo) is det.

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
	io__write_string("  without corresponding `pred' or `func' " ++
		"declaration.\n").

:- pred pred_method_with_no_modes_error(pred_info::in, io::di, io::uo) is det.

pred_method_with_no_modes_error(PredInfo) -->
	{ pred_info_context(PredInfo, Context) },
	{ Module = pred_info_module(PredInfo) },
	{ Name = pred_info_name(PredInfo) },
	{ Arity = pred_info_arity(PredInfo) },
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
:- pred undefined_mode_error(sym_name::in, int::in, prog_context::in,
	string::in, io::di, io::uo) is det.

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
	io::di, io::uo) is det.

undeclared_mode_error(ModeList, VarSet, PredId, PredInfo, ModuleInfo,
		Context, !IO) :-
	prog_out__write_context(Context, !IO),
	io__write_string("In clause for ", !IO),
	hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
	io__write_string(":\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string(
		"  error: mode annotation specifies undeclared mode\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  `", !IO),
	strip_builtin_qualifiers_from_mode_list(ModeList, StrippedModeList),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	Name = pred_info_name(PredInfo),
	MaybeDet = no,
	mercury_output_mode_subdecl(PredOrFunc, varset__coerce(VarSet),
		unqualified(Name), StrippedModeList, MaybeDet, Context, !IO),
	io__write_string("'\n", !IO),
	prog_out__write_context(Context, !IO),
	io__write_string("  of ", !IO),
	hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
	io__write_string(".\n", !IO),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
	ProcIds = pred_info_all_procids(PredInfo),
	( ProcIds = [] ->
		prog_out__write_context(Context, !IO),
		io__write_string("  (There are no declared modes for this ",
			!IO),
		write_pred_or_func(PredOrFunc, !IO),
		io__write_string(".)\n", !IO)
	; VerboseErrors = yes ->
		io__write_string("\tThe declared modes for this ", !IO),
		write_pred_or_func(PredOrFunc, !IO),
		io__write_string(" are the following:\n", !IO),
		OutputProc = (pred(ProcId::in, di, uo) is det -->
			io__write_string("\t\t:- mode "),
			output_mode_decl(ProcId, PredInfo),
			io__write_string(".\n")),
		list__foldl(OutputProc, ProcIds, !IO)
	;
		true
	).

:- pred maybe_undefined_pred_error(sym_name::in, int::in, pred_or_func::in,
	import_status::in, bool::in, prog_context::in, string::in,
	io::di, io::uo) is det.

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
		{ DeclString = pred_or_func_to_str(PredOrFunc) },
		io__write_string(DeclString),
		io__write_string("' declaration.\n")
	).

:- pred undefined_type_class_error(sym_name::in, int::in, prog_context::in,
	string::in, io::di, io::uo) is det.

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

:- pred unspecified_det_for_local(sym_name::in, arity::in, pred_or_func::in,
	prog_context::in, io::di, io::uo) is det.

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
		io__write_string("  (This is an error because " ++
			"you specified the `--no-infer-det'"),
		prog_out__write_context(Context),
		io__write_string("  option.  Use the `--infer-det' option " ++
			"if you want the"),
		prog_out__write_context(Context),
		io__write_string("  compiler to automatically infer " ++
			"the determinism of"),
		prog_out__write_context(Context),
		io__write_string("  local predicates.)")
	;
		[]
	).

:- pred unspecified_det_for_method(sym_name::in, arity::in, pred_or_func::in,
	prog_context::in, io::di, io::uo) is det.

unspecified_det_for_method(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string(
		"Error: no determinism declaration for type class method\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred unspecified_det_for_exported(sym_name::in, arity::in, pred_or_func::in,
	prog_context::in, io::di, io::uo) is det.

unspecified_det_for_exported(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: no determinism declaration for exported\n"),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred clause_for_imported_pred_error(sym_name::in, arity::in,
	pred_or_func::in, prog_context::in, io::di, io::uo) is det.

clause_for_imported_pred_error(Name, Arity, PredOrFunc, Context) -->
	io__set_exit_status(1),
	prog_out__write_context(Context),
	io__write_string("Error: clause for imported "),
	hlds_out__write_simple_call_id(PredOrFunc, Name/Arity),
	io__write_string(".\n").

:- pred unqualified_pred_error(sym_name::in, int::in, prog_context::in,
	io::di, io::uo) is det.

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

:- pred pragma_status_error(sym_name::in, int::in, prog_context::in,
	string::in, io::di, io::uo) is det.

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

:- pred pragma_conflict_error(sym_name::in, int::in, prog_context::in,
	string::in, io::di, io::uo) is det.

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

:- pred module_add_pragma_fact_table(sym_name::in, arity::in, string::in,
	import_status::in, prog_context::in, module_info::in, module_info::out,
	qual_info::in, qual_info::out, io::di, io::uo) is det.

module_add_pragma_fact_table(Pred, Arity, FileName, Status, Context,
		!Module, !Info, !IO) :-
	module_info_get_predicate_table(!.Module, PredicateTable),
	(
		predicate_table_search_sym_arity(PredicateTable,
			is_fully_qualified, Pred, Arity, PredIDs0),
		PredIDs0 = [PredID | PredIDs1]
	->
		(
			PredIDs1 = [], 		% only one predicate found
			module_info_pred_info(!.Module, PredID, PredInfo0),

				% compile the fact table into a separate
				% .o file
			fact_table_compile_facts(Pred, Arity, FileName,
				PredInfo0, PredInfo, Context, !.Module,
				C_HeaderCode, PrimaryProcID, !IO),

			module_info_set_pred_info(PredID, PredInfo, !Module),
			pred_info_procedures(PredInfo, ProcTable),
			pred_info_arg_types(PredInfo, ArgTypes),
			ProcIDs = pred_info_procids(PredInfo),
			PredOrFunc = pred_info_is_pred_or_func(PredInfo),
			adjust_func_arity(PredOrFunc, Arity, NumArgs),

				% create foreign_decls to declare
				% extern variables
			module_add_foreign_decl(c, foreign_decl_is_local,
				C_HeaderCode, Context, !Module),

			module_add_fact_table_file(FileName, !Module),

			io__get_exit_status(ExitStatus, !IO),
			( ExitStatus = 1 ->
				true
			;
				% create foreign_procs to access the table
				% in each mode
				module_add_fact_table_procedures(ProcIDs,
					PrimaryProcID, ProcTable, Pred,
					PredOrFunc, NumArgs, ArgTypes, Status,
					Context, !Module, !Info, !IO)
			)
		;
			PredIDs1 = [_ | _],		% >1 predicate found
			io__set_exit_status(1, !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("In pragma fact_table for `", !IO),
			prog_out__write_sym_name_and_arity(Pred/Arity, !IO),
			io__write_string("':\n", !IO),
			prog_out__write_context(Context, !IO),
			io__write_string("  error: " ++
				"ambiguous predicate/function name.\n", !IO)
		)
	;
		undefined_pred_or_func_error(Pred, Arity, Context,
			"`:- pragma fact_table' declaration", !IO)
	).

	% Add a `pragma c_code' for each mode of the fact table lookup to the
	% HLDS.
	% `pragma fact_table's are represented in the HLDS by a
	% `pragma c_code' for each mode of the predicate.

:- pred module_add_fact_table_procedures(list(proc_id)::in, proc_id::in,
	proc_table::in, sym_name::in, pred_or_func::in, arity::in,
	list(type)::in, import_status::in, prog_context::in,
	module_info::in, module_info::out, qual_info::in, qual_info::out,
	io::di, io::uo) is det.

module_add_fact_table_procedures([],_,_,_,_,_,_,_,_, !Module, !Info, !IO).
module_add_fact_table_procedures([ProcID | ProcIDs], PrimaryProcID, ProcTable,
		SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
		!Module, !Info, !IO) :-
	module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName,
		PredOrFunc, Arity, ArgTypes, Status, Context,
		!Module, !Info, !IO),
	module_add_fact_table_procedures(ProcIDs, PrimaryProcID, ProcTable,
		SymName, PredOrFunc, Arity, ArgTypes, Status, Context,
		!Module, !Info, !IO).

:- pred module_add_fact_table_proc(proc_id::in, proc_id::in, proc_table::in,
	sym_name::in, pred_or_func::in, arity::in, list(type)::in,
	import_status::in, prog_context::in, module_info::in, module_info::out,
	qual_info::in, qual_info::out, io::di, io::uo) is det.

module_add_fact_table_proc(ProcID, PrimaryProcID, ProcTable, SymName,
		PredOrFunc, Arity, ArgTypes, Status, Context, !Module,
		!Info, !IO) :-
	map__lookup(ProcTable, ProcID, ProcInfo),
	varset__init(VarSet0),
	varset__new_vars(VarSet0, Arity, Vars, VarSet),
	proc_info_argmodes(ProcInfo, Modes),
	fact_table_pragma_vars(Vars, Modes, VarSet, PragmaVars),
	fact_table_generate_c_code(SymName, PragmaVars, ProcID, PrimaryProcID,
		ProcInfo, ArgTypes, !.Module, C_ProcCode, C_ExtraCode, !IO),

	% XXX this should be modified to use nondet pragma c_code.
	Attrs0 = default_attributes(c),
	set_may_call_mercury(will_not_call_mercury, Attrs0, Attrs1),
	set_thread_safe(thread_safe, Attrs1, Attrs2),
		% fact tables procedures should be considered pure
	set_purity(pure, Attrs2, Attrs),
	module_add_pragma_foreign_proc(Attrs, SymName, PredOrFunc,
		PragmaVars, VarSet, ordinary(C_ProcCode, no),
		Status, Context, !Module, !Info, !IO),
	( C_ExtraCode = "" ->
		true
	;
		module_add_foreign_body_code(c, C_ExtraCode, Context, !Module)
	),
	%
	% The C code for fact tables includes C labels;
	% we cannot inline this code, because if we try,
	% the result may be duplicate labels in the generated code.
	% So we must disable inlining for fact_table procedures.
	%
	add_pred_marker("fact_table", SymName, Arity, Status, Context,
		no_inline, [], !Module, !IO).

	% Create a list(pragma_var) that looks like the ones that are created
	% for pragma c_code in prog_io.m.
	% This is required by module_add_pragma_c_code to add the C code for
	% the procedure to the HLDS.

:- pred fact_table_pragma_vars(list(prog_var)::in, list(mode)::in,
	prog_varset::in, list(pragma_var)::out) is det.

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
%
% promise ex error checking
%
% The following predicates are used to perform extra error checking specific
% to promise ex declarations (see notes/promise_ex.html). Currently, the
% following checks are performed:
% 	* check for universally quantified variables
% 	* check if universal quantification is placed in the wrong
% 	  position (i.e. after the `promise_exclusive' rather than
% 	  before it)
% 	* check that its goal is a disjunction and that each arm of the
% 	  disjunction has at most one call, and otherwise has only unifications

	% perform above checks on a promise ex declaration
:- pred check_promise_ex_decl(prog_vars::in, promise_type::in, goal::in,
	prog_context::in, io::di, io::uo) is det.

check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !IO) :-
		% are universally quantified variables present?
	(
		UnivVars = [],
		promise_ex_error(PromiseType, Context,
			"declaration has no universally quantified variables",
			!IO)
	;
		UnivVars = [_ | _]
	),
	check_promise_ex_goal(PromiseType, Goal, !IO).

	% check for misplaced universal quantification, otherwise find the
	% disjunction, flatten it out into list form and perform further
	% checks
:- pred check_promise_ex_goal(promise_type::in, goal::in, io::di, io::uo)
	is det.

check_promise_ex_goal(PromiseType, GoalExpr - Context, !IO) :-
	( GoalExpr = some(_, Goal) ->
		check_promise_ex_goal(PromiseType, Goal, !IO)
	; GoalExpr =  ( _ ; _ ) ->
		flatten_to_disj_list(GoalExpr - Context, DisjList),
		list__map(flatten_to_conj_list, DisjList, DisjConjList),
		check_disjunction(PromiseType, DisjConjList, !IO)
	; GoalExpr = all(_UnivVars, Goal) ->
		promise_ex_error(PromiseType, Context,
			"universal quantification should come before " ++
			"the declaration name", !IO),
		check_promise_ex_goal(PromiseType, Goal, !IO)
	;
		promise_ex_error(PromiseType, Context,
			"goal in declaration is not a disjunction", !IO)
	).

	% turns the goal of a promise ex declaration into a list of goals,
	% where each goal is an arm of the disjunction
:- pred flatten_to_disj_list(goal::in, goals::out) is det.

flatten_to_disj_list(GoalExpr - Context, GoalList) :-
	( GoalExpr = ( GoalA ; GoalB ) ->
		flatten_to_disj_list(GoalA, GoalListA),
		flatten_to_disj_list(GoalB, GoalListB),
		GoalList = GoalListA ++ GoalListB
	;
		GoalList = [GoalExpr - Context]
	).

	% takes a goal representing an arm of a disjunction and turn it into
	% a list of conjunct goals
:- pred flatten_to_conj_list(goal::in, goals::out) is det.

flatten_to_conj_list(GoalExpr - Context, GoalList) :-
	( GoalExpr = ( GoalA , GoalB ) ->
		flatten_to_conj_list(GoalA, GoalListA),
		flatten_to_conj_list(GoalB, GoalListB),
		GoalList = GoalListA ++ GoalListB
	;
		GoalList = [GoalExpr - Context]
	).

	% taking a list of arms of the disjunction, check each arm
	% individually
:- pred check_disjunction(promise_type::in, list(goals)::in, io::di, io::uo)
	is det.

check_disjunction(PromiseType, DisjConjList, !IO) :-
	(
		DisjConjList = []
	;
		DisjConjList = [ConjList | Rest],
		check_disj_arm(PromiseType, ConjList, no, !IO),
		check_disjunction(PromiseType, Rest, !IO)
	).

	% only one goal in an arm is allowed to be a call, the rest must be
	% unifications
:- pred check_disj_arm(promise_type::in, goals::in, bool::in,
	io::di, io::uo) is det.

check_disj_arm(PromiseType, Goals, CallUsed, !IO) :-
	(
		Goals = []
	;
		Goals = [GoalExpr - Context | Rest],
		( GoalExpr = unify(_, _, _) ->
			check_disj_arm(PromiseType, Rest, CallUsed, !IO)
		; GoalExpr = some(_, Goal) ->
			check_disj_arm(PromiseType, [Goal | Rest], CallUsed,
				!IO)
		; GoalExpr = call(_, _, _) ->
			(
				CallUsed = no
			;
				CallUsed = yes,
				promise_ex_error(PromiseType, Context,
					"disjunct contains more than one call",
					!IO)
			),
			check_disj_arm(PromiseType, Rest, yes, !IO)
		;
			promise_ex_error(PromiseType, Context,
				"disjunct is not a call or unification", !IO),
			check_disj_arm(PromiseType, Rest, CallUsed, !IO)
		)
	).

	% called for any error in the above checks
:- pred promise_ex_error(promise_type::in, prog_context::in, string::in,
	io::di, io::uo) is det.

promise_ex_error(PromiseType, Context, Message, !IO) :-
	ErrorPieces = [
		words("In"),
		fixed("`" ++ prog_out__promise_to_string(PromiseType) ++ "'"),
		words("declaration:"),
		nl,
		words("error:"),
		words(Message)
	],
	error_util__write_error_pieces(Context, 0, ErrorPieces, !IO).

:- func this_file = string.

this_file = "make_hlds.m".

%-----------------------------------------------------------------------------%

	% This synonym improves code legibility.
	%
:- type svar == prog_var.

:- type svars == list(svar).

	% A set of state variables.
	%
:- type svar_set == set(svar).

	% A mapping from state variables to logical variables.
	%
:- type svar_map == map(svar, prog_var).

	% This controls how state variables are dealt with.
	%
:- type svar_ctxt
	--->	in_head		% In the head of a clause or lambda.
	;	in_body		% In the body of a clause or lambda.
	;	in_atom(	% In the context of an atomic goal at the
				% level of the source code.
			had_colon_reference :: svar_set,
				% The set of state variables X that have been
				% referenced as !:X in the parameters of the
				% atomic goal.
			parent_svar_info    :: svar_info
				% The parent svar_info, used to keep track of
				% nesting in subterms of an atomic formula.
		).

:- type svar_info
	--->	svar_info(
			ctxt		::	svar_ctxt,

			num		::	int,
				% This is used to number state variables and
				% is incremented for each source-level
				% conjunct.

			external_dot	::	svar_map,
				% The "read only" state variables in
				% scope (e.g. external state variables
				% visible from within a lambda body or
				% condition of an if-then-else expression.)

			dot		::	svar_map,
			colon		::	svar_map
				% The "read/write" state variables in scope.
		).

	% When collecting the arms of a disjunction we also need to
	% collect the resulting svar_infos.
	%
:- type hlds_goal_svar_info == {hlds_goal, svar_info}.

:- type hlds_goal_svar_infos == list(hlds_goal_svar_info).

	% Create a new svar_info set up to start processing a clause head.
	%
:- func new_svar_info = svar_info.

new_svar_info = svar_info(in_head, 0, map__init, map__init, map__init).

%-----------------------------------------------------------------------------%

	% Obtain the mapping for a !.X state variable reference and
	% update the svar_info.
	%
	% If we are processing the head of a clause or lambda, we
	% incrementally accumulate the mappings.
	%
	% Otherwise, the mapping must already be present for a local
	% or `external' state variable (i.e. one that may be visible,
	% but not updatable, in the current context.)
	%
	% Note that if !.X does not appear in the head then !:X must
	% appear before !.X can be referenced.
	%
:- pred dot(prog_context::in, svar::in, prog_var::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

dot(Context, StateVar, Var, !VarSet, !SInfo, !IO) :-
	( !.SInfo ^ ctxt = in_head ->
		( !.SInfo ^ dot ^ elem(StateVar) = Var0 ->
			Var = Var0
		;
		  	new_dot_state_var(StateVar, Var, !VarSet, !SInfo)
		)
	;
		( !.SInfo ^ dot ^ elem(StateVar) = Var0 ->
			Var = Var0
		; !.SInfo ^ external_dot ^ elem(StateVar) = Var0 ->
			Var = Var0
		; !.SInfo `has_svar_colon_mapping_for` StateVar ->
		  	new_dot_state_var(StateVar, Var, !VarSet, !SInfo),
			report_unitialized_state_var(Context, !.VarSet,
				StateVar, !IO)
		;
		  	Var = StateVar,
			report_non_visible_state_var(".", Context, !.VarSet,
				StateVar, !IO)
		)
	).

:- pred svar_info `has_svar_colon_mapping_for` svar.
:- mode in `has_svar_colon_mapping_for` in is semidet.

SInfo `has_svar_colon_mapping_for` StateVar :-
	SInfo ^ colon `contains` StateVar.

SInfo `has_svar_colon_mapping_for` StateVar :-
	SInfo ^ ctxt = in_atom(_, ParentSInfo),
	ParentSInfo `has_svar_colon_mapping_for` StateVar.

%-----------------------------------------------------------------------------%

	% Obtain the mapping for a !:X state variable reference.
	%
	% If we are processing the head of a clause or lambda, we
	% incrementally accumulate the mappings.
	%
	% Otherwise, the mapping must already be present for a local
	% state variable (`externally' visible state variables cannot
	% be updated.)
	%
	% We also keep track of which state variables have been updated
	% in an atomic context.
	%
:- pred colon(prog_context::in, svar::in, prog_var::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

colon(Context, StateVar, Var, !VarSet, !SInfo, !IO) :-
	( !.SInfo ^ ctxt = in_head ->
		( !.SInfo ^ colon ^ elem(StateVar) = Var0 ->
			Var = Var0
		;
		  	new_final_state_var(StateVar, Var, !VarSet, !SInfo)
		)
	;
		( !.SInfo ^ colon ^ elem(StateVar) = Var0 ->
			Var = Var0,
			!:SInfo = !.SInfo `with_updated_svar` StateVar
		;
		  	Var = StateVar,
			% Set up a dummy mapping: there's no point
			% in mentioning this error twice.
			!:SInfo = ( !.SInfo ^ colon ^ elem(StateVar) := Var ),
			( !.SInfo ^ external_dot `contains` StateVar ->
				PError = report_illegal_state_var_update
			;
				PError = report_non_visible_state_var(":")
			),
			PError(Context, !.VarSet, StateVar, !IO)
		)
	).

:- func svar_info `with_updated_svar` svar = svar_info.

SInfo `with_updated_svar` StateVar =
	( SInfo ^ ctxt =  in_atom(UpdatedStateVars, ParentSInfo) ->
		SInfo ^ ctxt := in_atom(set__insert(UpdatedStateVars, StateVar),
			ParentSInfo)
	;
		SInfo
	).

%-----------------------------------------------------------------------------%

	% Construct the initial and final mappings for a state variable.
	%
:- pred new_local_state_var(svar::in, prog_var::out, prog_var::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

new_local_state_var(StateVar, VarD, VarC, !VarSet, !SInfo) :-
	new_dot_state_var(StateVar, VarD, !VarSet, !SInfo),
	new_final_state_var(StateVar, VarC, !VarSet, !SInfo).

	% Construct the initial and final mappings for a state variable.
	%
:- pred new_dot_state_var(svar::in, prog_var::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

new_dot_state_var(StateVar, VarD, !VarSet, !SInfo) :-
	N     = !.SInfo ^ num,
	Name  = varset__lookup_name(!.VarSet, StateVar),
	NameD = string__format("STATE_VARIABLE_%s_%d", [s(Name), i(N)]),
	varset__new_named_var(!.VarSet, NameD, VarD, !:VarSet),
	!:SInfo = ( !.SInfo ^ dot ^ elem(StateVar) := VarD ).

:- pred new_colon_state_var(svar::in, prog_var::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

new_colon_state_var(StateVar, VarC, !VarSet, !SInfo) :-
	N     = !.SInfo ^ num,
	Name  = varset__lookup_name(!.VarSet, StateVar),
	NameC = string__format("STATE_VARIABLE_%s_%d", [s(Name), i(N)]),
	varset__new_named_var(!.VarSet, NameC, VarC, !:VarSet),
	!:SInfo = ( !.SInfo ^ colon ^ elem(StateVar) := VarC ).

:- pred new_final_state_var(svar::in, prog_var::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

new_final_state_var(StateVar, VarC, !VarSet, !SInfo) :-
	Name  = varset__lookup_name(!.VarSet, StateVar),
	NameC = string__format("STATE_VARIABLE_%s",    [s(Name)]),
	varset__new_named_var(!.VarSet, NameC, VarC, !:VarSet),
	!:SInfo = ( !.SInfo ^ colon ^ elem(StateVar) := VarC ).

%-----------------------------------------------------------------------------%

	% Prepare for the head of a new clause.
	%
:- pred prepare_for_head(svar_info::out) is det.

prepare_for_head(new_svar_info).

%-----------------------------------------------------------------------------%

	% We need to make the current !.Xs external
	% ("read-only") and clear the !.Xs and !:Xs.
	%
	% While processing the head, any state variables therein are
	% implicitly scoped over the body and have !. and !: mappings
	% set up.
	%
:- pred prepare_for_lambda(svar_info::in, svar_info::out) is det.

prepare_for_lambda(!SInfo) :-
	!:SInfo = ( new_svar_info ^ external_dot := !.SInfo ^ dot ).

%-----------------------------------------------------------------------------%

	% Having processed the head of a clause, prepare for the first
	% (source-level) atomic conjunct.  We return the final !:
	% mappings identified while processing the head.
	%
:- pred prepare_for_body(svar_map::out, prog_varset::in, prog_varset::out,
	svar_info::in, svar_info::out) is det.

prepare_for_body(FinalMap, !VarSet, !SInfo) :-
	FinalMap  = !.SInfo ^ colon,
	N         = !.SInfo ^ num + 1,
	StateVars = list__merge_and_remove_dups(map__keys(!.SInfo ^ colon),
						map__keys(!.SInfo ^ dot)),
	next_svar_mappings(N, StateVars, !VarSet, Colon),
	!:SInfo   = ((( !.SInfo ^ ctxt  := in_body )
			       ^ num   := N       )
			       ^ colon := Colon   ).

%-----------------------------------------------------------------------------%

	% We have to conjoin the head and body and add unifiers to tie up all
	% the final values of the state variables to the head variables.
	%
:- pred finish_head_and_body(prog_context::in, svar_map::in,
	hlds_goal::in, hlds_goal::in, hlds_goal::out, svar_info::in) is det.

finish_head_and_body(Context, FinalSVarMap, Head, Body, Goal, SInfo) :-
	goal_info_init(Context, GoalInfo),
	goal_to_conj_list(Head, HeadGoals),
	goal_to_conj_list(Body, BodyGoals),
	Unifiers = svar_unifiers(yes(dont_warn_singleton), Context,
		FinalSVarMap, SInfo ^ dot),
	conj_list_to_goal(HeadGoals ++ BodyGoals ++ Unifiers, GoalInfo, Goal).

:- func svar_unifiers(maybe(goal_feature), prog_context, svar_map, svar_map)
	= hlds_goals.

svar_unifiers(MaybeFeature, Context, LHSMap, RHSMap) =
	map__foldl(add_svar_unifier(MaybeFeature, RHSMap, Context), LHSMap,
		[]).

:- func add_svar_unifier(maybe(goal_feature), svar_map, prog_context,
	svar, prog_var, hlds_goals) = hlds_goals.

add_svar_unifier(MaybeFeature, RHSMap, Context, StateVar, Var, Unifiers0)
		= Unifiers :-
	( RHSVar = RHSMap ^ elem(StateVar) ->
		Unifier = svar_unification(MaybeFeature, Context, Var, RHSVar),
		Unifiers = [Unifier | Unifiers0]
	;
		Unifiers = Unifiers0
	).

%-----------------------------------------------------------------------------%

:- func svar_unification(maybe(goal_feature), prog_context, prog_var, prog_var)
	= hlds_goal.

svar_unification(MaybeFeature, Context, SVar, Var) = Unification :-
	hlds_goal__create_atomic_unification(SVar, var(Var), Context,
		implicit("state variable"), [], Unification0),
	(
		MaybeFeature = no,
		Unification = Unification0
	;
		MaybeFeature = yes(Feature),
		goal_add_feature(Unification0, Feature, Unification)
	).

%-----------------------------------------------------------------------------%

	% Add some local state variables.
	%
:- pred prepare_for_local_state_vars(svars::in,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

prepare_for_local_state_vars(StateVars, !VarSet, !SInfo) :-
	list__foldl2(add_new_local_state_var, StateVars,
		!VarSet, !SInfo).

:- pred add_new_local_state_var(svar::in,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

add_new_local_state_var(StateVar, !VarSet, !SInfo) :-
	new_colon_state_var(StateVar, _, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

	% Remove some local state variables.
	%
:- pred finish_local_state_vars(svars::in, prog_vars::out,
	svar_info::in, svar_info::in, svar_info::out) is det.

finish_local_state_vars(StateVars, Vars, SInfoBefore, !SInfo) :-
	Dots    = svar_mappings(!.SInfo ^ dot  , StateVars),
	Colons  = svar_mappings(!.SInfo ^ colon, StateVars),
	Vars    = list__sort_and_remove_dups(Dots ++ Colons),
	!:SInfo = (( !.SInfo ^ dot   := del_locals(StateVars,
							SInfoBefore ^ dot,
							!.SInfo ^ dot) )
	                   ^ colon := del_locals(StateVars,
							SInfoBefore ^ colon,
							!.SInfo ^ colon) ).

:- func svar_mappings(svar_map, svars) = svars.

svar_mappings(_, []) = [].
svar_mappings(Map, [StateVar | StateVars]) =
	( Map ^ elem(StateVar) = Var ->
		[Var | svar_mappings(Map, StateVars)]
	;
		svar_mappings(Map, StateVars)
	).

:- func del_locals(svars, svar_map, svar_map) = svar_map.

del_locals(StateVars, MapBefore, Map) =
	list__foldl(
		func(K, M) =
			( if   MapBefore ^ elem(K) =  V
			  then M         ^ elem(K) := V
			  else map__delete(M, K)
			),
		StateVars,
		Map
	).

%-----------------------------------------------------------------------------%

	% We have to add unifiers to the Then and Else arms of an
	% if-then-else to make sure all the state variables match up.
	%
	% More to the point, we have to add unifiers to the Then arm
	% for any new state variable mappings produced in the condition.
	%
	% We construct new mappings for the state variables and then
	% add unifiers.
	%
:- pred finish_if_then_else(prog_context::in, hlds_goal::in, hlds_goal::out,
	hlds_goal::in, hlds_goal::out, svar_info::in,
	svar_info::in, svar_info::in, svar_info::in, svar_info::out,
	prog_varset::in, prog_varset::out) is det.

finish_if_then_else(Context, Then0, Then, Else0, Else,
		SInfo0, SInfoC, SInfoT0, SInfoE, SInfo, !VarSet) :-

		% Add unifiers to the Then arm for state variables that
		% acquired new mappings in the condition, but not in the
		% Them arm itself.  This is because the new mappings
		% appear only in a negated context.
		%
	StateVars  = list__merge_and_remove_dups(map__keys(SInfoT0 ^ dot),
						 map__keys(SInfoE  ^ dot)),
	Then0 = _ - GoalInfo,
	goal_to_conj_list(Then0, Thens0),
	add_then_arm_specific_unifiers(Context, StateVars,
		SInfo0, SInfoC, SInfoT0, SInfoT,
		Thens0, Thens, !VarSet),
	conj_list_to_goal(Thens, GoalInfo, Then1),

		% Calculate the svar_info with the highest numbered
		% mappings from each arm.
		%
	DisjSInfos = [{Then1, SInfoT}, {Else0, SInfoE}],
	SInfo      = reconciled_disj_svar_info(!.VarSet, DisjSInfos),

		% Add unifiers to each arm to ensure they both construct
		% the same final state variable mappings.
		%
	Then       = add_disj_unifiers(Context, SInfo, StateVars,
				{Then1, SInfoT}),
	Else       = add_disj_unifiers(Context, SInfo, StateVars,
				{Else0, SInfoE}).

	% If a new mapping was produced for state variable X in the
	% condition-goal (i.e. the condition refers to !:X), but not
	% in the then-goal, then we have to add a new unifier !:X = !.X
	% to the then-goal because the new mapping was created in a
	% negated context.
	%
:- pred add_then_arm_specific_unifiers(prog_context::in, svars::in,
	svar_info::in, svar_info::in, svar_info::in, svar_info::out,
	hlds_goals::in, hlds_goals::out, prog_varset::in, prog_varset::out)
	is det.

add_then_arm_specific_unifiers(_, [], _, _, SInfoT, SInfoT,
		Thens, Thens, VarSet, VarSet).

add_then_arm_specific_unifiers(Context, [StateVar | StateVars],
		SInfo0, SInfoC, !SInfoT, !Thens, !VarSet) :-
	(	% the condition refers to !:X, but the then-goal doesn't
		SInfoC ^ dot ^ elem(StateVar) \= SInfo0 ^ dot ^ elem(StateVar),
		!.SInfoT ^ dot ^ elem(StateVar) = SInfoC ^ dot ^ elem(StateVar)
	->
	  	% add a new unifier !:X = !.X
	  	Dot0 = !.SInfoT ^ dot ^ det_elem(StateVar),
		new_colon_state_var(StateVar, Dot, !VarSet, !SInfoT),
		!:Thens = [svar_unification(yes(dont_warn_singleton), Context,
			Dot, Dot0) | !.Thens],
		prepare_for_next_conjunct(set__make_singleton_set(StateVar),
			!VarSet, !SInfoT)
	;
	  	true
	),
	add_then_arm_specific_unifiers(Context, StateVars,
		SInfo0, SInfoC, !SInfoT, !Thens, !VarSet).

%-----------------------------------------------------------------------------%

:- pred next_svar_mappings(int::in, svars::in,
	prog_varset::in, prog_varset::out, svar_map::out) is det.

next_svar_mappings(N, StateVars, VarSet0, VarSet, Map) :-
	next_svar_mappings_2(N, StateVars, VarSet0, VarSet, map__init, Map).

:- pred next_svar_mappings_2(int::in, svars::in,
	prog_varset::in, prog_varset::out, svar_map::in, svar_map::out) is det.

next_svar_mappings_2(_, [], !VarSet, !Map).
next_svar_mappings_2(N, [StateVar | StateVars], !VarSet, !Map) :-
	next_svar_mapping(N, StateVar, _, !VarSet, !Map),
	next_svar_mappings_2(N, StateVars, !VarSet, !Map).

%-----------------------------------------------------------------------------%

	% We assume that a negation updates all state variables in scope,
	% so we construct new mappings for the state variables and then
	% add unifiers from their pre-negated goal mappings.
	%
:- pred finish_negation(svar_info::in, svar_info::in, svar_info::out) is det.

finish_negation(SInfoBefore, SInfoNeg, SInfo) :-
	SInfo = (( SInfoBefore ^ num   := SInfoNeg ^ num   )
	                       ^ colon := SInfoNeg ^ colon ).

%-----------------------------------------------------------------------------%

	% We have to make sure that all arms of a disjunction produce the
	% same state variable bindings by adding unifiers as necessary.
	%
:- pred finish_disjunction(prog_context::in, prog_varset::in,
	hlds_goal_svar_infos::in, hlds_goals::out, svar_info::out) is det.

finish_disjunction(Context, VarSet, DisjSInfos, Disjs, SInfo) :-
	SInfo      = reconciled_disj_svar_info(VarSet, DisjSInfos),
	StateVars  = map__keys(SInfo ^ dot),
	Disjs      = list__map(
			add_disj_unifiers(Context, SInfo, StateVars),
			DisjSInfos).

	% Each arm of a disjunction may have a different mapping for
	% !.X and/or !:X.  The reconciled svar_info for the disjunction
	% takes the highest numbered mapping for each disjunct (each
	% state variable mapping for !.X or !:X will have a name of
	% the form `STATE_VARIABLE_X_n' for some number `n'.)
	%
:- func reconciled_disj_svar_info(prog_varset, hlds_goal_svar_infos) =
	svar_info.

reconciled_disj_svar_info(_, []) = _ :-
	error("make_hlds__reconciled_disj_svar_info: empty disjunct list").

reconciled_disj_svar_info(VarSet, [{_, SInfo0} | DisjSInfos]) = SInfo :-

		% We compute the set of final !. and !: state variables
		% over the whole disjunction (not all arms will necessarily
		% include !. and !: mappings for all state variables).
		%
	Dots0   = set__sorted_list_to_set(map__keys(SInfo0 ^ dot)),
	Colons0 = set__sorted_list_to_set(map__keys(SInfo0 ^ colon)),
	Dots    = union_dot_svars(Dots0, DisjSInfos),
	Colons  = union_colon_svars(Colons0, DisjSInfos),

		% Then we update SInfo0 to take the highest numbered
		% !. and !: mapping for each state variable.
		%
	SInfo   = list__foldl(
			reconciled_svar_infos(VarSet, Dots, Colons),
			DisjSInfos,
			SInfo0
		  ).

:- func union_dot_svars(svar_set, hlds_goal_svar_infos) = svar_set.

union_dot_svars(Dots, []                       ) = Dots.

union_dot_svars(Dots, [{_, SInfo} | DisjSInfos]) =
	union_dot_svars(
		Dots `union`
			set__sorted_list_to_set(map__keys(SInfo ^ dot)),
		DisjSInfos
	).

:- func union_colon_svars(svar_set, hlds_goal_svar_infos) = svar_set.

union_colon_svars(Colons, []                       ) = Colons.

union_colon_svars(Colons, [{_, SInfo} | DisjSInfos]) =
	union_colon_svars(
		Colons `union`
			set__sorted_list_to_set(map__keys(SInfo ^ colon)),
		DisjSInfos
	).

:- func reconciled_svar_infos(prog_varset, svar_set, svar_set,
	hlds_goal_svar_info, svar_info) = svar_info.

reconciled_svar_infos(VarSet, Dots, Colons,
		{_, SInfoX}, SInfo0) = SInfo :-
	SInfo1 = set__fold(
			reconciled_svar_infos_dots(VarSet, SInfoX),
			Dots,
			SInfo0
		 ),
	SInfo2 = set__fold(
			reconciled_svar_infos_colons(VarSet, SInfoX),
			Colons,
			SInfo1
		 ),
	SInfo  = ( SInfo2 ^ num := max(SInfo0 ^ num, SInfoX ^ num) ).

:- func reconciled_svar_infos_dots(prog_varset, svar_info, svar, svar_info)
	= svar_info.

reconciled_svar_infos_dots(VarSet, SInfoX, StateVar, SInfo0) = SInfo :-
	(
		DotX = SInfoX ^ dot ^ elem(StateVar),
		Dot0 = SInfo0 ^ dot ^ elem(StateVar)
	->
	  	NameX = varset__lookup_name(VarSet, DotX) `with_type` string,
	  	Name0 = varset__lookup_name(VarSet, Dot0) `with_type` string,
		compare_svar_names(RDot, NameX, Name0),
		(
			RDot  = (<),
			SInfo = ( SInfo0 ^ dot ^ elem(StateVar) := Dot0 )
		;
			RDot  = (=),
			SInfo = SInfo0
		;
			RDot  = (>),
			SInfo = ( SInfo0 ^ dot ^ elem(StateVar) := DotX )
		)
	;
	  	SInfo = SInfo0
	).

:- func reconciled_svar_infos_colons(prog_varset, svar_info, svar, svar_info)
	= svar_info.

reconciled_svar_infos_colons(VarSet, SInfoX, StateVar, SInfo0) = SInfo :-
	(
		ColonX = SInfoX ^ colon ^ elem(StateVar),
		Colon0 = SInfo0 ^ colon ^ elem(StateVar)
	->
	  	NameX = varset__lookup_name(VarSet, ColonX) `with_type` string,
	  	Name0 = varset__lookup_name(VarSet, Colon0) `with_type` string,
		compare_svar_names(RColon, NameX, Name0),
		(
			RColon = (<),
			SInfo  = ( SInfo0 ^ colon ^ elem(StateVar) := Colon0 )
		;
			RColon = (=),
			SInfo  = SInfo0
		;
			RColon = (>),
			SInfo  = ( SInfo0 ^ colon ^ elem(StateVar) := ColonX )
		)
	;
	  	SInfo = SInfo0
	).

:- func add_disj_unifiers(prog_context, svar_info, svars, hlds_goal_svar_info)
	= hlds_goal.

add_disj_unifiers(Context, SInfo, StateVars, {GoalX, SInfoX}) = Goal :-
	Unifiers = list__foldl(add_disj_unifier(Context, SInfo, SInfoX),
		StateVars, []),
	GoalX = _ - GoalInfo,
	goal_to_conj_list(GoalX, GoalsX),
	conj_list_to_goal(GoalsX ++ Unifiers, GoalInfo, Goal).

:- func add_disj_unifier(prog_context, svar_info, svar_info, svar, hlds_goals)
	= hlds_goals.

add_disj_unifier(Context, SInfo, SInfoX, StateVar, Unifiers) =
	(
		Dot  = SInfo  ^ dot ^ elem(StateVar),
		DotX = SInfoX ^ dot ^ elem(StateVar),
		Dot \= DotX
	->
	  	[svar_unification(yes(dont_warn_singleton), Context, Dot, DotX)
			| Unifiers]
	;
	  	Unifiers
	).

%-----------------------------------------------------------------------------%

	% We implement a special purpose comparison for state variable
	% names that compares the numbers appended at the right hand
	% ends of the name strings.
	%
	% NOTE state variable names are either "..._X" or "..._X_N"
	% where X is the name of the program variable used for the
	% state variable and N is a decimal number with no leading
	% zeroes.
	%
:- pred compare_svar_names(comparison_result::out, string::in, string::in)
	is det.

compare_svar_names(R, A, B) :-
	compare(R, int_suffix_of(A), int_suffix_of(B)).

	% Find the number suffix at the end of a string as an int.
	%
:- func int_suffix_of(string) = int.

int_suffix_of(S) = int_suffix_2(S, length(S) - 1, 1, 0).

	% int_suffix_2(String, Index, RadixOfIndexDigit, IntSoFar) = IntSuffix
	%
:- func int_suffix_2(string, int, int, int) = int.

int_suffix_2(S, I, R, N) =
	(
		0 =< I,
		digit_to_int(S `unsafe_index` I, D),
		D < 10
	->
		int_suffix_2(S, I - 1, 10 * R, (R * D) + N)
	;
	  	N
	).

%-----------------------------------------------------------------------------%

	% We treat equivalence goals as if they were negations (they are
	% in a negated context after all.)
	%
:- pred finish_equivalence(svar_info::in, svar_info::in, svar_info::out)
	is det.

finish_equivalence(SInfoBefore, SInfoEqv, SInfo) :-
	finish_negation(SInfoBefore, SInfoEqv, SInfo).

%-----------------------------------------------------------------------------%

	% We prepare for a call by setting the ctxt to in_atom.  If we're
	% already in an atom then we inherit the parent's set of "updated"
	% state variables.
	%
:- pred prepare_for_call(svar_info::in, svar_info::out) is det.

prepare_for_call(ParentSInfo, SInfo) :-
	(
		ParentSInfo ^ ctxt =
			in_atom(UpdatedStateVars, _GrandparentSInfo)
	->
		Ctxt = in_atom(UpdatedStateVars, ParentSInfo)
	;
		Ctxt = in_atom(set__init, ParentSInfo)
	),
	SInfo = ParentSInfo ^ ctxt := Ctxt.

%-----------------------------------------------------------------------------%

	% When we finish a call, we're either still inside the
	% atomic formula, in which case we simply propagate the set of
	% "updated" state variables, or we've just emerged, in which case
	% we need to set up the svar_info for the next conjunct.
	%
	% (We can still be in an atomic context if, for example, we've
	% been processing a function call which must appear as an
	% expression and hence occur inside an atomic context.)
	%
:- pred finish_call(prog_varset::in, prog_varset::out,
	svar_info::in, svar_info::out) is det.

finish_call(!VarSet, !SInfo) :-
	( !.SInfo ^ ctxt = in_atom(UpdatedStateVars, ParentSInfo0) ->
		ParentSInfo = ( ParentSInfo0 ^ dot := !.SInfo ^ dot ),
		( ParentSInfo ^ ctxt = in_atom(_, GrandParentSInfo) ->
			!:SInfo  = ( ParentSInfo ^ ctxt :=
				in_atom(UpdatedStateVars, GrandParentSInfo) )
		;
			prepare_for_next_conjunct(UpdatedStateVars,
				!VarSet, ParentSInfo, !:SInfo)
		)
	;
	  	error("make_hlds__finish_call: ctxt is not in_atom")
	).

%-----------------------------------------------------------------------------%

:- pred prepare_for_if_then_else_goal(svars::in,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

prepare_for_if_then_else_goal(StateVars, !VarSet, !SInfo) :-
	prepare_for_local_state_vars(StateVars, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

:- pred finish_if_then_else_goal_condition(svars::in,
	svar_info::in, svar_info::in, svar_info::out, svar_info::out) is det.

finish_if_then_else_goal_condition(StateVars, SInfoBefore, SInfoA0, SInfoA,
		SInfoB) :-
	SInfoB = SInfoA0,
	finish_local_state_vars(StateVars, _, SInfoBefore, SInfoA0, SInfoA).

%-----------------------------------------------------------------------------%

:- pred finish_if_then_else_goal_then_goal(svars::in,
	svar_info::in, svar_info::in, svar_info::out) is det.

finish_if_then_else_goal_then_goal(StateVars, SInfoBefore, SInfoB0, SInfoB) :-
	finish_local_state_vars(StateVars, _, SInfoBefore, SInfoB0, SInfoB).

%-----------------------------------------------------------------------------%

	% The condition of an if-then-else expression is a goal in which
	% only !.X state variables in scope are visible (although the goal
	% may use local state variables introduced via an explicit
	% quantifier.)  The StateVars are local to the condition and then-
	% goal.
	%
:- pred prepare_for_if_then_else_expr(svars::in,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

prepare_for_if_then_else_expr(StateVars, !VarSet, !SInfo) :-
	!:SInfo = ((( new_svar_info ^ ctxt         := in_body      )
	                           ^ external_dot := !.SInfo ^ dot )
				   ^ num          := !.SInfo ^ num ),
	prepare_for_local_state_vars(StateVars, !VarSet, !SInfo).

%-----------------------------------------------------------------------------%

:- pred finish_if_then_else_expr_condition(svar_info::in,
	svar_info::in, svar_info::out) is det.

finish_if_then_else_expr_condition(SInfoBefore, SInfo0, SInfo) :-
	SInfo = (((( SInfo0 ^ external_dot := SInfoBefore ^ external_dot )
	                    ^ dot          := (SInfo0 ^ dot) `overlay`
		 	                         (SInfoBefore ^ dot)  )
	                    ^ colon        := (SInfo0 ^ colon) `overlay`
		 	                         (SInfoBefore ^ colon)  )
	                    ^ ctxt         := SInfoBefore ^ ctxt         ).

%-----------------------------------------------------------------------------%

:- pred finish_if_then_else_expr_then_goal(svars::in,
	svar_info::in, svar_info::in, svar_info::out) is det.

finish_if_then_else_expr_then_goal(StateVars, SInfoBefore, SInfo0, SInfo) :-
	finish_local_state_vars(StateVars, _, SInfoBefore, SInfo0, SInfo).

%-----------------------------------------------------------------------------%

	% Having finished processing one source-level atomic conjunct, prepare
	% for the next.  Note that if !:X was not seen in the conjunct we've
	% just processed, then we can reuse the !.X and !:X mappings.
	%
	% 	p(!.X) where [!.X -> X0, !:X -> X1]
	%
	% can yield
	%
	% 	p(X0) and [!.X -> X0, !:X -> X2]
	%
	% but
	%
	% 	p(!.X, !:X) where [!.X -> X0, !:X -> X1]
	%
	% will yield
	%
	% 	p(X0, X1) and [!.X -> X1, !:X -> X2]
	%
:- pred prepare_for_next_conjunct(svar_set::in,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out)
	is det.

prepare_for_next_conjunct(UpdatedStateVars, !VarSet, !SInfo) :-
	Dot0   = !.SInfo ^ dot,
	Colon0 = !.SInfo ^ colon,
	N      = !.SInfo ^ num + 1,
	map__init(Nil),
	map__foldl(next_dot_mapping(UpdatedStateVars, Dot0, Colon0), Colon0,
		Nil, Dot),
	map__foldl2(next_colon_mapping(UpdatedStateVars, Colon0, N), Colon0,
		!VarSet, Nil, Colon),
	!:SInfo  = (((( !.SInfo ^ ctxt  := in_body )
		             ^ num   := N       )
			     ^ dot   := Dot     )
			     ^ colon := Colon   ).

	% If the state variable has been updated (i.e. there was a !:X
	% reference) then the next !.X mapping will be the current !:X
	% mapping.
	% Otherwise, preserve the current !.X mapping, if any (there
	% may be none if, for example, the head only references !:X
	% and there have been no prior references to !:X in the body.)
	%
:- pred next_dot_mapping(svar_set::in, svar_map::in, svar_map::in, svar::in,
	prog_var::in, svar_map::in, svar_map::out) is det.

next_dot_mapping(UpdatedStateVars, OldDot, OldColon, StateVar, _, Dot0, Dot) :-
	( UpdatedStateVars `contains` StateVar ->
		Var = OldColon ^ det_elem(StateVar),
	  	Dot = ( Dot0 ^ elem(StateVar) := Var )
	; Var = OldDot ^ elem(StateVar) ->
		Dot = ( Dot0 ^ elem(StateVar) := Var )
	;
		Dot = Dot0
	).

	% If the state variable has been updated (i.e. there was a !:X
	% reference) then create a new mapping for the next !:X.
	% Otherwise, the next !:X mapping is the same as the current
	% !:X mapping.
	%
:- pred next_colon_mapping(svar_set::in, svar_map::in, int::in, svar::in,
	prog_var::in, prog_varset::in, prog_varset::out,
	svar_map::in, svar_map::out) is det.

next_colon_mapping(UpdatedStateVars, OldColon, N, StateVar, _,
		!VarSet, !Colon) :-
	( UpdatedStateVars `contains` StateVar ->
		next_svar_mapping(N, StateVar, _Var, !VarSet, !Colon)
	;
		!:Colon = ( !.Colon ^ elem(StateVar) :=
					OldColon ^ det_elem(StateVar) )
	).

:- pred next_svar_mapping(int::in, svar::in, prog_var::out,
	prog_varset::in, prog_varset::out, svar_map::in, svar_map::out) is det.

next_svar_mapping(N, StateVar, Var, !VarSet, !Map) :-
	Name = string__format("STATE_VARIABLE_%s_%d",
		[s(varset__lookup_name(!.VarSet, StateVar)), i(N)]),
	varset__new_named_var(!.VarSet, Name, Var, !:VarSet),
	!:Map  = ( !.Map ^ elem(StateVar) := Var ).

%-----------------------------------------------------------------------------%

	% Replace !X args with two args !.X, !:X in that order.
	%
:- func expand_bang_state_var_args(list(prog_term)) = list(prog_term).

expand_bang_state_var_args(Args) =
	list__foldr(expand_bang_state_var, Args, []).

:- func expand_bang_state_var(prog_term, list(prog_term)) = list(prog_term).

expand_bang_state_var(T @ variable(_), Ts) = [T | Ts].

expand_bang_state_var(T @ functor(Const, Args, Ctxt), Ts) =
	( Const = atom("!"), Args = [variable(_StateVar)] ->
		[ functor(atom("!."), Args, Ctxt),
		  functor(atom("!:"), Args, Ctxt)
		    | Ts ]
	;
	  	[ T | Ts ]
	).

%-----------------------------------------------------------------------------%

:- func expand_bang_state_var_args_in_instance_method_heads(instance_body) =
	instance_body.

expand_bang_state_var_args_in_instance_method_heads(abstract) = abstract.

expand_bang_state_var_args_in_instance_method_heads(concrete(Methods)) =
	concrete(list__map(expand_method_bsvs, Methods)).

:- func expand_method_bsvs(instance_method) = instance_method.

expand_method_bsvs(IM) = IM :-
	IM = instance_method(_, _, name(_), _, _).

expand_method_bsvs(IM0) = IM :-
	IM0 = instance_method(PredOrFunc, Method, clauses(Cs0), Arity0, Ctxt),
	Cs  = list__map(expand_item_bsvs, Cs0),
		% Note that the condition should always succeed...
		%
	( Cs = [clause(_, _, _, Args, _) | _] ->
		adjust_func_arity(PredOrFunc, Arity, list__length(Args))
	;
		Arity = Arity0
	),
	IM  = instance_method(PredOrFunc, Method, clauses(Cs), Arity, Ctxt).

	% The instance method clause items will all be clause items.
	%
:- func expand_item_bsvs(item) = item.

expand_item_bsvs(Item) =
	( Item = clause(VarSet, PredOrFunc, SymName, Args, Body) ->
		clause(VarSet, PredOrFunc, SymName,
			expand_bang_state_var_args(Args), Body)
	;
		Item
	).

%-----------------------------------------------------------------------------%

	% Given a list of argument terms, substitute !.X and !:X with
	% the corresponding state variable mappings.  Any !X should
	% already have been expanded into !.X, !:X via a call to
	% expand_bang_state_var_args/1.
	%
:- pred substitute_state_var_mappings(list(prog_term)::in,
	list(prog_term)::out, prog_varset::in, prog_varset::out,
	svar_info::in, svar_info::out, io::di, io::uo) is det.

substitute_state_var_mappings([], [], !VarSet, !SInfo, !IO).
substitute_state_var_mappings([Arg0 | Args0], [Arg | Args],
		!VarSet, !SInfo, !IO) :-
	substitute_state_var_mapping(Arg0, Arg,
		!VarSet, !SInfo, !IO),
	substitute_state_var_mappings(Args0, Args,
		!VarSet, !SInfo, !IO).

:- pred substitute_state_var_mapping(prog_term::in, prog_term::out,
	prog_varset::in, prog_varset::out, svar_info::in, svar_info::out,
	io::di, io::uo) is det.

substitute_state_var_mapping(Arg0, Arg, !VarSet, !SInfo, !IO) :-
	(
		Arg0 = functor(atom("!."), [variable(StateVar)], Context)
	->
		dot(Context, StateVar, Var, !VarSet, !SInfo, !IO),
		Arg  = variable(Var)
	;
		Arg0 = functor(atom("!:"), [variable(StateVar)], Context)
	->
		colon(Context, StateVar, Var, !VarSet, !SInfo, !IO),
		Arg  = variable(Var)
	;
	  	Arg  = Arg0
	).

%-----------------------------------------------------------------------------%

:- pred illegal_state_var_func_result(pred_or_func::in, list(prog_term)::in,
	svar::out) is semidet.

illegal_state_var_func_result(function, Args, StateVar) :-
	list__last(Args, functor(atom("!"), [variable(StateVar)], _Ctxt)).

%-----------------------------------------------------------------------------%

	% We do not allow !X to appear as a lambda head argument.
	% We might extend the syntax still further to accommodate
	% this as an option, e.g. !IO::(di, uo).
	%
:- pred lambda_args_contain_bang_state_var(list(prog_term)::in, prog_var::out)
	is semidet.

lambda_args_contain_bang_state_var([Arg | Args], StateVar) :-
	( Arg      = functor(atom("!"), [variable(StateVar0)], _) ->
		StateVar = StateVar0
	;
		lambda_args_contain_bang_state_var(Args, StateVar)
	).

%-----------------------------------------------------------------------------%

:- pred report_illegal_state_var_update(prog_context::in, prog_varset::in,
	svar::in, io::di, io::uo) is det.

report_illegal_state_var_update(Context, VarSet, StateVar, !IO) :-
	Name = varset__lookup_name(VarSet, StateVar),
	prog_out__write_context(Context, !IO),
	report_error(string__format("\
cannot use !:%s in this context;", [s(Name)]), !IO),
	prog_out__write_context(Context, !IO),
	io__format("  however !.%s may be used here.\n", [s(Name)], !IO).

%-----------------------------------------------------------------------------%

:- pred report_non_visible_state_var(string::in, prog_context::in,
	prog_varset::in, svar::in, io::di, io::uo) is det.

report_non_visible_state_var(DorC, Context, VarSet, StateVar, !IO) :-
	Name = varset__lookup_name(VarSet, StateVar),
	prog_out__write_context(Context, !IO),
	report_error(string__format(
		"state variable !%s%s is not visible in this context.",
		[s(DorC), s(Name)]), !IO).

%-----------------------------------------------------------------------------%

:- pred report_unitialized_state_var(prog_context::in, prog_varset::in,
	svar::in, io::di, io::uo) is det.

report_unitialized_state_var(Context, VarSet, StateVar, !IO) :-
	Name = varset__lookup_name(VarSet, StateVar),
	prog_out__write_context(Context, !IO),
	report_warning(string__format(
		"Warning: reference to unitialized state variable !.%s.\n",
		[s(Name)]), !IO).

%-----------------------------------------------------------------------------%

:- pred report_illegal_func_svar_result(prog_context::in, prog_varset::in,
	svar::in, io::di, io::uo) is det.

report_illegal_func_svar_result(Context, VarSet, StateVar, !IO) :-
	Name = varset__lookup_name(VarSet, StateVar),
	prog_out__write_context(Context, !IO),
	report_error(string__format("!%s cannot be a function result.",
		[s(Name)]), !IO),
	prog_out__write_context(Context, !IO),
	io__format("  You probably meant !.%s or !:%s.\n", [s(Name), s(Name)],
  		!IO).

%-----------------------------------------------------------------------------%

:- pred report_illegal_bang_svar_lambda_arg(prog_context::in, prog_varset::in,
	svar::in, io::di, io::uo) is det.

report_illegal_bang_svar_lambda_arg(Context, VarSet, StateVar, !IO) :-
	Name = varset__lookup_name(VarSet, StateVar),
	prog_out__write_context(Context, !IO),
	report_error(string__format("!%s cannot be a lambda argument.",
		[s(Name)]), !IO),
	prog_out__write_context(Context, !IO),
	io__format("  Perhaps you meant !.%s or !:%s.\n",
		[s(Name), s(Name)], !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
