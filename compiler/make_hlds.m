%-----------------------------------------------------------------------------%

% File: make_hlds.nl.
% Main author: fjh.

% This module converts from the parse tree structure which is read in by
% prog_io.nl, into the simplified high level data structure defined in
% hlds.nl.  In the parse tree, the program is represented as a list of
% items; we insert each item into the appropriate symbol table, and report
% any duplicate definition errors.  We also transform clause bodies from
% (A,B,C) into conj([A,B,C]) form, convert all unifications into
% super-homogenous form, and introduce implicit quantification.
% 
% XXX we should record each error using module_info_incr_errors.

% WISHLIST - we should handle explicit module quantification

:- module make_hlds.
:- interface.
:- import_module prog_io, hlds, io.

:- pred parse_tree_to_hlds(program, module_info, io__state, io__state).
:- mode parse_tree_to_hlds(in, out, di, uo) is det.

:- pred create_atomic_unification(term, term, unify_main_context,
				unify_sub_contexts, hlds__goal).
:- mode create_atomic_unification(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, int, set, bintree, list, map, require, std_util.
:- import_module term, term_io, varset.
:- import_module prog_util, prog_out, hlds_out.
:- import_module globals, options.
:- import_module make_tags, quantification.
:- import_module code_util, unify_proc, type_util, implication.


parse_tree_to_hlds(module(Name, Items), Module) -->
	{ module_info_init(Name, Module0) },
	add_item_list_decls(Items, local, Module0, Module1),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
		% balance the binary trees
	{ module_info_optimize(Module1, Module2) },
	maybe_report_stats(Statistics),
	add_item_list_clauses(Items, Module2, Module3),
		% the predid list is constructed in reverse order, for
		% efficiency, so we return it to the correct order here.
	{ module_info_reverse_predids(Module3, Module) }.

%-----------------------------------------------------------------------------%

	% add the declarations one by one to the module

:- pred add_item_list_decls(item_list, import_status, module_info, module_info,
				io__state, io__state).
:- mode add_item_list_decls(in, in, in, out, di, uo) is det.

add_item_list_decls([], _, Module, Module) --> [].
add_item_list_decls([Item - Context | Items], Status0, Module0, Module) -->
	add_item_decl(Item, Context, Status0, Module0, Status1, Module1),
	add_item_list_decls(Items, Status1, Module1, Module).

	% add the clauses one by one to the module

:- pred add_item_list_clauses(item_list, module_info, module_info,
				io__state, io__state).
:- mode add_item_list_clauses(in, in, out, di, uo) is det.

add_item_list_clauses([], Module, Module) --> [].
add_item_list_clauses([Item - Context | Items], Module0, Module) -->
	add_item_clause(Item, Context, Module0, Module1),
	add_item_list_clauses(Items, Module1, Module).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl(item, term__context, import_status, module_info,
			import_status, module_info,
			io__state, io__state).
:- mode add_item_decl(in, in, in, in, out, out, di, uo) is det.

	% skip clauses
add_item_decl(clause(_, _, _, _), _, Status, Module, Status, Module) --> [].

add_item_decl(type_defn(VarSet, TypeDefn, Cond), Context, Status, Module0,
		Status, Module) -->
	module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Context, Status,
		Module).

add_item_decl(inst_defn(VarSet, InstDefn, Cond), Context, Status, Module0,
		Status, Module) -->
	module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Context, Module).

add_item_decl(mode_defn(VarSet, ModeDefn, Cond), Context, Status, Module0,
		Status, Module) -->
	module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Context, Module).

add_item_decl(pred(VarSet, PredName, TypesAndModes, Det, Cond), Context,
		Status, Module0, Status, Module) -->
	module_add_pred(Module0, VarSet, PredName, TypesAndModes, Det, Cond,
		Context, Status, Module).

add_item_decl(mode(VarSet, PredName, Modes, Det, Cond), Context, Status,
		Module0, Status, Module) -->
	module_add_mode(Module0, VarSet, PredName, Modes, Det, Cond, Context,
		Module).

add_item_decl(module_defn(_VarSet, ModuleDefn), Context, Status0, Module0,
		Status, Module) -->
	( { ModuleDefn = interface } ->
		{ Status = exported },
		{ Module = Module0 }
	; { ModuleDefn = implementation } ->
		{ Status = local },
		{ Module = Module0 }
	; { ModuleDefn = imported } ->
		{ Status = imported },
		{ Module = Module0 }
	; { ModuleDefn = import(module(_)) } ->
		{ Status = Status0 },
		{ Module = Module0 }
	; { ModuleDefn = external(name_arity(Name, Arity)) } ->
		{ Status = Status0 },
		module_mark_as_external(Name, Arity, Context, Module0, Module)
	;
		{ Status = Status0 },
		{ Module = Module0 },
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("warning: declaration not yet implemented.\n"),
		io__set_output_stream(OldStream, _)
	).

add_item_decl(nothing, _, Status, Module, Status, Module) --> [].

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_clause(item, term__context, module_info, module_info,
			io__state, io__state).
:- mode add_item_clause(in, in, in, out, di, uo) is det.

add_item_clause(clause(VarSet, PredName, Args, Body), Context, Module0,
			Module) -->
	module_add_clause(Module0, VarSet, PredName, Args, Body, Context,
			Module).
add_item_clause(type_defn(_, _, _), _, Module, Module) --> [].
add_item_clause(inst_defn(_, _, _), _, Module, Module) --> [].
add_item_clause(mode_defn(_, _, _), _, Module, Module) --> [].
add_item_clause(pred(_, _, _, _, _), _, Module, Module) --> [].
add_item_clause(mode(_, _, _, _, _), _, Module, Module) --> [].
add_item_clause(module_defn(_, _), _, Module, Module) --> [].
add_item_clause(nothing, _, Module, Module) --> [].

%-----------------------------------------------------------------------------%

:- pred module_mark_as_external(sym_name, int, term__context,
			module_info, module_info, io__state, io__state).
:- mode module_mark_as_external(in, in, in, in, out, di, uo) is det.

module_mark_as_external(PredName, Arity, Context, Module0, Module) -->
	{ module_info_name(Module0, ModuleName) },
	{ module_info_get_predicate_table(Module0, PredicateTable0) },
	{ unqualify_name(PredName, PName) },	% ignore any module qualifier
	(
		{ predicate_table_search_m_n_a(PredicateTable0,
			ModuleName, PName, Arity, PredIdList) }
	->
		{ module_mark_preds_as_external(PredIdList, Module0, Module) }
	;
		{ module_info_incr_errors(Module0, Module) },
		undefined_pred_error(PredName, Arity, Context,	
			"`external' declaration")
	).

:- pred module_mark_preds_as_external(list(pred_id), module_info, module_info).
:- mode module_mark_preds_as_external(in, in, out) is det.

module_mark_preds_as_external([], Module, Module).
module_mark_preds_as_external([PredId | PredIds], Module0, Module) :-
	module_info_preds(Module0, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	pred_info_mark_as_external(PredInfo0, PredInfo),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Module0, Preds, Module1),
	module_mark_preds_as_external(PredIds, Module1, Module).

%-----------------------------------------------------------------------------%

:- pred module_add_inst_defn(module_info, varset, inst_defn, condition,
			term__context, module_info, io__state, io__state).
:- mode module_add_inst_defn(in, in, in, in, in, out, di, uo) is det.

module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Context, Module) -->
	{ module_info_insts(Module0, InstTable0) },
	{ inst_table_get_user_insts(InstTable0, Insts0) },
	insts_add(Insts0, VarSet, InstDefn, Cond, Context, Insts),
	{ inst_table_set_user_insts(InstTable0, Insts, InstTable) },
	{ module_info_set_insts(Module0, InstTable, Module) }.

:- pred insts_add(user_inst_table, varset, inst_defn, condition, term__context,
			user_inst_table, io__state, io__state).
:- mode insts_add(in, in, in, in, in, out, di, uo) is det.

	% XXX handle abstract insts
insts_add(_, _, abstract_inst(_, _), _, _, _) -->
	{ error("sorry, abstract insts not implemented") }.
insts_add(Insts0, VarSet, eqv_inst(Name, Args, Body), Cond, Context, Insts) -->
	{ list__length(Args, Arity) },
	(
	 	{ I = hlds__inst_defn(VarSet, Args, eqv_inst(Body), Cond,
			Context) },
		{ map__insert(Insts0, Name - Arity, I, Insts1) }
	->
		{ Insts = Insts1 }
	;
		{ Insts = Insts0 },
		% XXX we should record each error using module_info_incr_errors
		multiple_def_error(Name, Arity, "inst", Context)
	).

%-----------------------------------------------------------------------------%

:- pred module_add_mode_defn(module_info, varset, mode_defn, condition,
			term__context, module_info, io__state, io__state).
:- mode module_add_mode_defn(in, in, in, in, in, out, di, uo) is det.

module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Context, Module) -->
	{ module_info_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, ModeDefn, Cond, Context, Modes),
	{ module_info_set_modes(Module0, Modes, Module) }.

:- pred modes_add(mode_table, varset, mode_defn, condition, term__context,
			mode_table, io__state, io__state).
:- mode modes_add(in, in, in, in, in, out, di, uo) is det.

modes_add(Modes0, VarSet, eqv_mode(Name, Args, Body), Cond, Context, Modes) -->
	{ list__length(Args, Arity) },
	(
		{ I = hlds__mode_defn(VarSet, Args, eqv_mode(Body), Cond,
			Context) },
		{ map__insert(Modes0, Name - Arity, I, Modes1) }
	->
		{ Modes = Modes1 }
	;
		{ Modes = Modes0 },
		% XXX we should record each error using module_info_incr_errors
		multiple_def_error(Name, Arity, "mode", Context)
	).

:- pred mode_name_args(mode_defn, sym_name, list(inst_param), hlds__mode_body).
:- mode mode_name_args(in, out, out, out) is det.

mode_name_args(eqv_mode(Name, Args, Body), Name, Args, eqv_mode(Body)).

%-----------------------------------------------------------------------------%

	% We allow more than one "definition" for a given type so
	% long all of them except one are actually just declarations,
	% e.g. `:- type t.', which is parsed as an type definition for
	% t which defines t as an abstract_type.

:- pred module_add_type_defn(module_info, tvarset, type_defn, condition,
				term__context, import_status, module_info,
				io__state, io__state).
:- mode module_add_type_defn(in, in, in, in, in, in, out, di, uo) is det.

module_add_type_defn(Module0, TVarSet, TypeDefn, Cond, Context, Status,
		Module) -->
	{ module_info_types(Module0, Types0) },
	globals__io_get_globals(Globals),
	{ convert_type_defn(TypeDefn, Globals, Name, Args, Body) },
	{ list__length(Args, Arity) },
	{ T = hlds__type_defn(TVarSet, Args, Body, Cond, Context) },
	(
		% if there was an existing non-abstract definition for the type
		{ map__search(Types0, Name - Arity, T2) },
		{ T2 = hlds__type_defn(_, _, Body_2, _, _) },
		{ Body_2 \= abstract_type }
	->
	  	(
			% then if this definition was abstract, ignore it
			{ Body = abstract_type }
		->
			{ Module = Module0 }
		;
			% otherwise issue an error message
			{ module_info_incr_errors(Module0, Module) },
			multiple_def_error(Name, Arity, "type", Context)
		)
	;
		{ TypeId = Name - Arity },
		{ map__set(Types0, TypeId, T, Types) },
		(
			{ Body = du_type(ConsList, _, IsEnum) }
		->
			{ module_info_ctors(Module0, Ctors0) },
			ctors_add(ConsList, TypeId, Context, Ctors0, Ctors),
			{ module_info_set_ctors(Module0, Ctors, Module1) },
			( { IsEnum = no } ->
				{ unqualify_name(Name, UnqualifiedName) },
				{ TypeFunctor = term__atom(UnqualifiedName) },
				{ Type = term__functor(TypeFunctor, Args,
					Context) },
				{ add_unify_pred(Module1, TVarSet, Type, TypeId,
					unify_du_type(ConsList), Context,
					Status, Module2) }
			;
				{ Module2 = Module1 }
			)
		;
			{ Body = eqv_type(EqvType) }
		->
			{ unqualify_name(Name, UnqualifiedName) },
			{ TypeFunctor = term__atom(UnqualifiedName) },
			{ Type = term__functor(TypeFunctor, Args,
				Context) },
			{ add_unify_pred(Module0, TVarSet, Type, TypeId,
				unify_eqv_type(EqvType), Context,
				Status, Module2) }
		;
			{ Body = abstract_type }
		->
			{ unqualify_name(Name, UnqualifiedName) },
			{ TypeFunctor = term__atom(UnqualifiedName) },
			{ Type = term__functor(TypeFunctor, Args,
				Context) },
			{ add_unify_pred_decl(Module0, TVarSet, Type, TypeId,
					Context, Status, Module2) }
			
		;
			{ Module2 = Module0 }
		),
		{ module_info_set_types(Module2, Types, Module) },
		( { Body = uu_type(_) } ->
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string(
	"warning: undiscriminated union types (`+') not implemented.\n"),
			io__set_output_stream(OldStream, _)
		;
			[]
		)
	).

:- pred convert_type_defn(type_defn, globals,
			sym_name, list(type_param), hlds__type_body).
:- mode convert_type_defn(in, in, out, out, out) is det.

convert_type_defn(du_type(Name, Args, Body), Globals, Name, Args,
		du_type(Body, CtorTags, IsEnum)) :-
	assign_constructor_tags(Body, Globals, CtorTags, IsEnum).
convert_type_defn(uu_type(Name, Args, Body), _, Name, Args, uu_type(Body)).
convert_type_defn(eqv_type(Name, Args, Body), _, Name, Args, eqv_type(Body)).
convert_type_defn(abstract_type(Name, Args), _, Name, Args, abstract_type).

:- pred ctors_add(list(constructor), type_id, term__context, cons_table,
			cons_table, io__state, io__state).
:- mode ctors_add(in, in, in, in, out, di, uo) is det.

ctors_add([], _TypeId, _Context, Ctors, Ctors) --> [].
ctors_add([Name - Args | Rest], TypeId, Context, Ctors0, Ctors) -->
	{ make_cons_id(Name, Args, TypeId, ConsId) },
	{ ConsDefn = hlds__cons_defn(Args, TypeId, Context) },
	( %%% some [ConsDefns0]
		{ map__search(Ctors0, ConsId, ConsDefns0) }
	->
		{ ConsDefns1 = ConsDefns0 }
	;
		{ ConsDefns1 = [] }
	),
	(
		{ list__member(OtherConsDefn, ConsDefns1) },
		{ OtherConsDefn = hlds__cons_defn(_, TypeId, _) }
	->
		% XXX we should record each error using module_info_incr_errors
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("Error: constructor `"),
		hlds_out__write_cons_id(ConsId),
		io__write_string("' for type `"),
		hlds_out__write_type_id(TypeId),
		io__write_string("' multiply defined.\n"),
		io__set_output_stream(OldStream, _),
		{ ConsDefns2 = ConsDefns1 }
	;
		{ ConsDefns2 = [ConsDefn | ConsDefns1] }
	),
	{ map__set(Ctors0, ConsId, ConsDefns2, Ctors1) },
	ctors_add(Rest, TypeId, Context, Ctors1, Ctors).

%---------------------------------------------------------------------------%

:- pred module_add_pred(module_info, varset, sym_name, list(type_and_mode),
		determinism, condition, term__context, import_status,
		module_info,
		io__state, io__state).
:- mode module_add_pred(in, in, in, in, in, in, in, in, out, di, uo) is det.

module_add_pred(Module0, VarSet, PredName, TypesAndModes, Det, Cond, Context,
		Status, Module) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	preds_add(Module0, VarSet, PredName, Types, Cond, Context, Status,
		Module1),
	(
		% some [Modes]
		{ MaybeModes = yes(Modes) }
	->
		module_add_mode(Module1, VarSet, PredName, Modes, Det, Cond,
			Context, Module)
	;
		{ Module = Module1 }
	).

:- pred preds_add(module_info, tvarset, sym_name, list(type),
		condition, term__context, import_status,
		module_info, io__state, io__state).
:- mode preds_add(in, in, in, in, in, in, in, out, di, uo) is det.

preds_add(Module0, TVarSet, PredName, Types, Cond, Context, Status, Module) -->
	{ module_info_name(Module0, ModuleName) },
	{ module_info_get_predicate_table(Module0, PredicateTable0) },
	{ list__length(Types, Arity) },
	{ clauses_info_init(Arity, ClausesInfo) },
	{ pred_info_init(ModuleName, PredName, Arity, TVarSet, Types, Cond,
		Context, ClausesInfo, Status, PredInfo0) },
	{ unqualify_name(PredName, PName) },	% ignore any module qualifier
	(
		{ \+ predicate_table_search_m_n_a(PredicateTable0,
			ModuleName, PName, Arity, _) }
	->
		( { code_util__predinfo_is_builtin(Module0, PredInfo0) } ->
			{ pred_info_mark_as_external(PredInfo0, PredInfo) }
		;
			{ PredInfo = PredInfo0 }
		),
		{ predicate_table_insert(PredicateTable0, PredInfo, _PredId,
			PredicateTable) },
		{ module_info_set_predicate_table(Module0, PredicateTable,
			Module) }
	;
		{ module_info_incr_errors(Module0, Module) },
		multiple_def_error(PredName, Arity, "pred", Context)
	).

:- pred add_unify_pred(module_info, tvarset, type, type_id, unify_type,
			term__context, import_status,
			module_info).
:- mode add_unify_pred(in, in, in, in, in, in, in, out) is det.

add_unify_pred(Module0, TVarSet, Type, TypeId, UnifyType, Context, Status,
		Module) :-
	module_info_get_unify_pred_map(Module0, UnifyPredMap0),
	( map__contains(UnifyPredMap0, TypeId) ->
		Module1 = Module0
	;
		add_unify_pred_decl(Module0, TVarSet, Type, TypeId, Context,
			Status, Module1)
	),
	module_info_get_unify_pred_map(Module1, UnifyPredMap1),
	map__lookup(UnifyPredMap1, TypeId, PredId),
	module_info_preds(Module1, Preds0),
	map__lookup(Preds0, PredId, PredInfo0),
	unify_proc__generate_clause_info(Type, UnifyType, ClausesInfo),
	pred_info_set_clauses_info(PredInfo0, ClausesInfo, PredInfo),
	map__set(Preds0, PredId, PredInfo, Preds),
	module_info_set_preds(Module1, Preds, Module).

:- pred add_unify_pred_decl(module_info, tvarset, type, type_id,
				term__context, import_status,
				module_info).
:- mode add_unify_pred_decl(in, in, in, in, in, in, out) is det.

add_unify_pred_decl(Module0, TVarSet, Type, TypeId, Context, Status,
			Module) :-
	module_info_name(Module0, ModuleName),
	PredName = unqualified("="),
	Arity = 2,
	Cond = true,
	ArgTypes = [Type, Type],
	clauses_info_init(Arity, ClausesInfo0),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, ArgTypes, Cond,
		Context, ClausesInfo0, Status, PredInfo0),

	ArgModes = [ground -> ground, ground -> ground],
	Det = semidet,
	pred_info_procedures(PredInfo0, Procs0),
	next_mode_id(Procs0, Det, ModeId),
	proc_info_init(Arity, ArgModes, Det, Context, NewProc),
	map__set(Procs0, ModeId, NewProc, Procs),
	pred_info_set_procedures(PredInfo0, Procs, PredInfo),

	module_info_get_predicate_table(Module0, PredicateTable0),
	predicate_table_insert(PredicateTable0, PredInfo, PredId,
		PredicateTable),
	module_info_set_predicate_table(Module0, PredicateTable,
		Module1),
	module_info_get_unify_pred_map(Module1, UnifyPredMap0),
	map__set(UnifyPredMap0, TypeId, PredId, UnifyPredMap),
	module_info_set_unify_pred_map(Module1, UnifyPredMap, Module).

%-----------------------------------------------------------------------------%

	% Add a mode declaration for a predicate.

:- pred module_add_mode(module_info, varset, sym_name, list(mode), determinism,
			condition, term__context, module_info,
			io__state, io__state).
:- mode module_add_mode(in, in, in, in, in, in, in, out, di, uo) is det.

	% We should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments.

module_add_mode(ModuleInfo0, _VarSet, PredName, Modes, Det, _Cond, MContext,
			ModuleInfo) -->
		%
		% Lookup the pred declaration in the predicate table.
		% If it's not there (or if it is ambiguous), print an
		% error message and insert a dummy declaration for the
		% predicate.
		% 
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ unqualify_name(PredName, PName) },	% ignore any module qualifier
	{ list__length(Modes, Arity) },
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_m_n_a(PredicateTable0,
			ModuleName, PName, Arity, [PredId0]) }
	->
		{ PredicateTable1 = PredicateTable0 },
		{ PredId = PredId0 }
	;
		% XXX we should record each error using module_info_incr_errors
		undefined_pred_error(PredName, Arity, MContext,	
			"mode declaration"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, MContext,
				PredId, PredicateTable1) }
		
	),
		%
		% Lookup the pred_info for this predicate
		%
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
		%
		% check that the determinism was specified
		%
	(
		{ Det = unspecified }
	->
		( { pred_info_is_exported(PredInfo0) } ->
			unspecified_det_error(PredName, Arity, MContext)
		;
			globals__io_lookup_bool_option(warn_missing_det_decls,
				ShouldWarn),
			{ ShouldWarn = yes }
		->
			unspecified_det_warning(PredName, Arity, MContext)
		;
			[]
		)
	;
		[]
	),
		%
		% add the mode declaration to the proc_info for this procedure.
		%
	{ pred_info_procedures(PredInfo0, Procs0) },
		% XXX we should check that this mode declaration
		% isn't the same as an existing one
	{ next_mode_id(Procs0, Det, ModeId) },
	{ proc_info_init(Arity, Modes, Det, MContext, NewProc) },
	{ map__set(Procs0, ModeId, NewProc, Procs) },
	{ pred_info_set_procedures(PredInfo0, Procs, PredInfo) },
	{ map__set(Preds0, PredId, PredInfo, Preds) },
	{ predicate_table_set_preds(PredicateTable0, Preds, PredicateTable) },
	{ module_info_set_predicate_table(ModuleInfo0, PredicateTable,
		ModuleInfo) }.

	% Whenever there is a clause or mode declaration for an undeclared
	% predicate, we add an implicit declaration
	%	:- pred p(_, _, ..., _).
	% for that predicate, so that calls to the pred don't get
	% spurious errors.

:- pred preds_add_implicit(predicate_table, module_name, sym_name, arity,
				term__context, pred_id, predicate_table).
:- mode preds_add_implicit(in, in, in, in, in, out, out) is det.

preds_add_implicit(PredicateTable0,
			ModuleName, PredName, Arity, Context,
			PredId, PredicateTable) :-
	varset__init(TVarSet0),
	make_n_fresh_vars(Arity, TVarSet0, TypeVars, TVarSet),
	term__var_list_to_term_list(TypeVars, Types),
	Cond = true,
	clauses_info_init(Arity, ClausesInfo),
	pred_info_init(ModuleName, PredName, Arity, TVarSet, Types, Cond,
		Context, ClausesInfo, local, PredInfo),
	unqualify_name(PredName, PName),	% ignore any module qualifier
	(
		\+ predicate_table_search_m_n_a(PredicateTable0,
			ModuleName, PName, Arity, _)
	->
		predicate_table_insert(PredicateTable0, PredInfo, PredId,
			PredicateTable1),
		predicate_table_remove_predid(PredicateTable1, PredId,
			PredicateTable)
	;	
		error("preds_add_implicit")
	).

	% This is a quick hack, especially the trick with
	% determinism_priority.  Efficiency could be improved -
	% we should probably store the next available ModeId rather
	% than recomputing it all the time.

:- pred next_mode_id(proc_table, determinism, proc_id).
:- mode next_mode_id(in, in, out) is det.

next_mode_id(Procs, Det, ModeId) :-
	map__to_assoc_list(Procs, List),
	list__length(List, ModeId0),
	determinism_priority(Det, Priority),
	ModeId is ModeId0 + Priority.

	% If we can call a predicate in either of two different modes,
	% we should prefer to call it in a deterministic mode
	% rather than a non-deterministic one, and we should prefer
	% to call it in a semideterministic mode rather than a deterministic
	% one.
	% Higher numbers mean lower priority.
	% This works because mode analysis tries each mode in turn,
	% starting with the lowest-numbered modes.

:- pred determinism_priority(determinism, int).
:- mode determinism_priority(in, out) is det.

determinism_priority(semidet, 0).
determinism_priority(failure, 0).
determinism_priority(det, 10000).
determinism_priority(erroneous, 10000).
determinism_priority(unspecified, 20000).
determinism_priority(nondet, 30000).

%-----------------------------------------------------------------------------%

:- pred module_add_clause(module_info, varset, sym_name, list(term), goal,
			term__context, module_info, io__state, io__state).
:- mode module_add_clause(in, in, in, in, in, in, out, di, uo) is det.

module_add_clause(ModuleInfo0, VarSet, PredName, Args, Body, Context,
			ModuleInfo) -->
		%
		% print out a progress message
		%
	{ module_info_name(ModuleInfo0, ModuleName) },
	{ list__length(Args, Arity) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("% Processing clause for pred `"),
		hlds_out__write_pred_call_id(PredName/Arity),
		io__write_string("'...\n")
	;
		[]
	),
		%
		% Lookup the pred declaration in the predicate table.
		% (if it's not there, print an error message and insert
		% a dummy declaration for the predicate.)
		%
	{ unqualify_name(PredName, PName) },	% ignore any module qualifier
	{ module_info_get_predicate_table(ModuleInfo0, PredicateTable0) },
	(
		{ predicate_table_search_m_n_a(PredicateTable0,
			ModuleName, PName, Arity, [PredId0]) }
	->
		{ PredId = PredId0 },
		{ PredicateTable1 = PredicateTable0 },
		{ ModuleInfo1 = ModuleInfo0 }
	;
		{ module_info_incr_errors(ModuleInfo0, ModuleInfo1) },
		undefined_pred_error(PredName, Arity, Context, "clause"),
		{ preds_add_implicit(PredicateTable0,
				ModuleName, PredName, Arity, Context,
				PredId, PredicateTable1) }
	),
		%
		% Lookup the pred_info for this pred,
		% add the clause to the clauses_info in the pred_info,
		% and save the pred_info.
		%
	{ predicate_table_get_preds(PredicateTable1, Preds0) },
	{ map__lookup(Preds0, PredId, PredInfo0) },
	( { pred_info_is_imported(PredInfo0) } ->
		{ module_info_incr_errors(ModuleInfo1, ModuleInfo) },
		clause_for_imported_pred_error(PredName, Arity, Context)
	; {
		pred_info_clauses_info(PredInfo0, Clauses0), 
		pred_info_procedures(PredInfo0, Procs),
		map__keys(Procs, ModeIds),
		clauses_info_add_clause(Clauses0, ModeIds, VarSet, Args,
				Body, Context, Clauses),
		pred_info_set_clauses_info(PredInfo0, Clauses, PredInfo),
		map__set(Preds0, PredId, PredInfo, Preds),
		predicate_table_set_preds(PredicateTable1, Preds,
			PredicateTable),
		module_info_set_predicate_table(ModuleInfo1, PredicateTable,
			ModuleInfo)
	} ),
		%
		% Warn about singleton variables in the clauses.
		%
	maybe_warn_singletons(VarSet, PredName/Arity, Args, Body, Context).

%-----------------------------------------------------------------------------%

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once.
	%
	% XXX This should be based on scopes, not over the entire clause.
	%     We shouldn't warn about variables which start with '_'
	%     unless they occur more than once *in the same scope*.
	%     We should also warn about variables which occur twice
	%     but only occur once in a particular scope.
	%
	% XXX This is O(N*N) in the number of vars per clause

:- pred maybe_warn_singletons(varset, pred_call_id, list(term), goal,
				term__context, io__state, io__state).
:- mode maybe_warn_singletons(in, in, in, in, in, di, uo) is det.

maybe_warn_singletons(VarSet, PredCallId, Args, Body, Context) -->
	globals__io_lookup_bool_option(warn_singleton_vars, WarnSingletonVars),
	( { WarnSingletonVars = yes } ->
		{ term__vars_list(Args, VarList0) },
		{ vars_in_goal(Body, VarList0, VarList) },
		warn_singletons(VarList, VarSet, PredCallId, Context)
	;	
		[]
	).

:- pred warn_singletons(list(var), varset, pred_call_id, term__context,
			io__state, io__state).
:- mode warn_singletons(in, in, in, in, di, uo) is det.

warn_singletons([], _, _, _) --> [].
warn_singletons([Var | Vars0], VarSet, PredCallId, Context) -->
	{ delete_all(Vars0, Var, Vars1, Found) },
	( { varset__lookup_name(VarSet, Var, Name) } ->
		(
			{ string__first_char(Name, '_', _) }
		->
			(
				{ Found = yes }
			->
				prog_out__write_context(Context),
				io__write_string("In clause for predicate `"),
				hlds_out__write_pred_call_id(PredCallId),
				io__write_string("':\n"),
				prog_out__write_context(Context),
				io__write_string("  Warning: variable `"),
				term_io__write_variable(Var, VarSet),
				io__write_string("' occurs more than once.\n")
			;
				[]
			)
		;
			(
				{ Found = no }
			->
				prog_out__write_context(Context),
				io__write_string("In clause for predicate `"),
				hlds_out__write_pred_call_id(PredCallId),
				io__write_string("':\n"),
				prog_out__write_context(Context),
				io__write_string("  Warning: variable `"),
				term_io__write_variable(Var, VarSet),
				io__write_string("' occurs only once.\n")
			;
				[]
			)
		)
	;
		[]
	),
	warn_singletons(Vars1, VarSet, PredCallId, Context).


	% delete_all(List0, Elem, List, Found) is true iff
	% List is List0 with all occurrences of Elem removed,
	% and Found = 'yes' if Elem occurred in List0 and 'no' otherwise.

:- pred delete_all(list(T), T, list(T), bool).
:- mode delete_all(in, in, out, out) is det.

delete_all([], _, [], no).
delete_all([X | Xs], Y, L, Found) :-
	( X = Y ->
		Found = yes,
		delete_all(Xs, Y, L, _)
	;	
		L = [X | Xs1],
		delete_all(Xs, Y, Xs1, Found)
	).

:- pred vars_in_goal(goal, list(var), list(var)).
:- mode vars_in_goal(in, in, out) is det.

vars_in_goal(true) --> [].
vars_in_goal(fail) --> [].
vars_in_goal((A,B)) -->
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal((A;B)) -->
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal(not(G)) -->
	vars_in_goal(G).
vars_in_goal(some(Vs, G)) -->
	list__append(Vs),
	vars_in_goal(G).
vars_in_goal(all(Vs, G)) -->
	list__append(Vs),
	vars_in_goal(G).
vars_in_goal(implies(A,B)) -->
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal(equivalent(A,B)) -->
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal(unify(A, B)) -->
	vars_in_term(A),
	vars_in_term(B).
vars_in_goal(call(Term)) -->
	vars_in_term(Term).
vars_in_goal(if_then(Vars,A,B)) -->
	list__append(Vars),
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal(if_then_else(Vars,A,B,C)) -->
	list__append(Vars),
	vars_in_goal(A),
	vars_in_goal(B),
	vars_in_goal(C).

:- pred vars_in_term(term, list(var), list(var)).
:- mode vars_in_term(in, in, out) is det.

vars_in_term(Term) -->
	term__vars_2(Term).

%-----------------------------------------------------------------------------

:- pred clauses_info_init(int::in, clauses_info::out) is det.

clauses_info_init(Arity, clauses_info(VarSet, VarTypes, HeadVars, [])) :-
	map__init(VarTypes),
	varset__init(VarSet0),
	make_n_fresh_vars(Arity, VarSet0, HeadVars, VarSet).

:- pred clauses_info_add_clause(clauses_info::in,
		list(proc_id)::in, varset::in, list(term)::in, goal::in,
		term__context::in, clauses_info::out) is det.

clauses_info_add_clause(ClausesInfo0, ModeIds, CVarSet, Args, Body,
		Context, ClausesInfo) :-
	ClausesInfo0 = clauses_info(VarSet0, VarTypes, HeadVars, ClauseList0),
	varset__merge_subst(VarSet0, CVarSet, VarSet1, Subst),
	transform(Subst, HeadVars, Args, Body, VarSet1, Goal, VarSet),
		% XXX we should avoid append - this gives O(N*N)
	list__append(ClauseList0, [clause(ModeIds, Goal, Context)], ClauseList),
	ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, ClauseList).

%-----------------------------------------------------------------------------

:- pred transform(substitution, list(var), list(term), goal, varset,
			hlds__goal, varset).
:- mode transform(in, in, in, in, in, out, out) is det.

transform(Subst, HeadVars, Args0, Body, VarSet0, Goal, VarSet) :-
	transform_goal(Body, VarSet0, Subst, Goal1, VarSet1),
	term__apply_substitution_to_list(Args0, Subst, Args),
	insert_arg_unifications(HeadVars, Args, head, Goal1, VarSet1,
		Goal2, VarSet),
	implicitly_quantify_clause_body(HeadVars, Goal2, Goal).

%-----------------------------------------------------------------------------%

	% Convert goals from the prog_io `goal' structure into the
	% hlds `hlds__goal' structure.  At the same time, convert
	% it to super-homogeneous form by unravelling all the complex
	% unifications, and annotate those unifications with a unify_context
	% so that we can still give good error messages.
	% And also at the same time, apply the given substitution to
	% the goal, to rename it apart from the other clauses.

:- pred transform_goal(goal, varset, substitution, hlds__goal, varset).
:- mode transform_goal(in, in, in, out, out) is det.

transform_goal(fail, VarSet, _, disj([]) - GoalInfo, VarSet) :-
	goal_info_init(GoalInfo).

transform_goal(true, VarSet, _, conj([]) - GoalInfo, VarSet) :-
	goal_info_init(GoalInfo).

	% Convert `all [Vars] Goal' into `not some [Vars] not Goal'.

transform_goal(all(Vars0, Goal0), VarSet0, Subst,
		not(some(Vars, not(Goal) - GoalInfo) - GoalInfo) - GoalInfo,
		VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(Goal0, VarSet0, Subst, Goal, VarSet),
	goal_info_init(GoalInfo).

transform_goal(some(Vars0, Goal0), VarSet0, Subst,
		some(Vars, Goal) - GoalInfo, VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(Goal0, VarSet0, Subst, Goal, VarSet),
	goal_info_init(GoalInfo).

transform_goal(if_then_else(Vars0, A0, B0, C0), VarSet0, Subst,
		if_then_else(Vars, A, B, C) - GoalInfo, VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(A0, VarSet0, Subst, A, VarSet1),
	transform_goal(B0, VarSet1, Subst, B, VarSet2),
	transform_goal(C0, VarSet2, Subst, C, VarSet),
	goal_info_init(GoalInfo).

transform_goal(if_then(Vars0, A0, B0), Subst, VarSet0, Goal, VarSet) :-
	transform_goal(if_then_else(Vars0, A0, B0, true), Subst, VarSet0,
		Goal, VarSet).

transform_goal(not(A0), VarSet0, Subst,
		not(A) - GoalInfo, VarSet) :-
	transform_goal(A0, VarSet0, Subst, A, VarSet),
	goal_info_init(GoalInfo).

transform_goal((A0,B0), VarSet0, Subst, Goal, VarSet) :-
	get_conj(B0, Subst, [], VarSet0, L0, VarSet1),
	get_conj(A0, Subst, L0, VarSet1, L, VarSet),
	goal_info_init(GoalInfo),
	conj_list_to_goal(L, GoalInfo, Goal).

transform_goal((A0;B0), VarSet0, Subst, Goal, VarSet) :-
	get_disj(B0, Subst, [], VarSet0, L0, VarSet1),
	get_disj(A0, Subst, L0, VarSet1, L, VarSet),
	goal_info_init(GoalInfo),
	disj_list_to_goal(L, GoalInfo, Goal).

transform_goal(implies(Goal0,Goal1), VarSet0, Subst, Goal, VarSet) :-
	implication__expand_implication(Goal0, Goal1, TransformedGoal),
	transform_goal(TransformedGoal, VarSet0, Subst, Goal, VarSet).

transform_goal(equivalent(Goal0,Goal1), VarSet0, Subst,	Goal, VarSet) :-
	implication__expand_equivalence(Goal0, Goal1, TransformedGoal),
	transform_goal(TransformedGoal, VarSet0, Subst, Goal, VarSet).

transform_goal(call(Goal0), VarSet0, Subst, Goal, VarSet) :-

	% As a special case, transform `A \= B' into `not (A = B)'.

	( Goal0 = term__functor(term__atom("\\="), [LHS, RHS], _Context) ->
		transform_goal(not(unify(LHS, RHS)), VarSet0, Subst, Goal,
			VarSet)
	;
		% fill unused slots with any old junk 
		ModeId = 0,
		Builtin = not_builtin,

		term__apply_substitution(Goal0, Subst, Goal1),
		( Goal1 = term__functor(term__atom(PredName0), Args0, _) ->
			PredName = PredName0,
			Args = Args0
		;
			% If the called term is not an atom, then it is
			% either a variable, or something stupid like a number.
			% In the first case, we want to transform it to a call
			% to builtin:call/1, and in the latter case, we
			% want to report an error.
			% In either case, we transform it to a call to call/1.
			% The error in the latter case will be caught by the
			% type-checker.
			PredName = "call",
			Args = [Goal1]
		),
		list__length(Args, Arity),
			% XXX we should handle module qualifiers properly
		SymName = unqualified(PredName),
		PredCallId = SymName/Arity,
		make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1),
		term__var_list_to_term_list(HeadVars, HeadArgs),
		invalid_pred_id(PredId),
		map__init(Follow),
		Goal2 = call(PredId, ModeId, HeadArgs, Builtin,
							SymName, Follow) -
				GoalInfo,
		goal_info_init(GoalInfo),
		insert_arg_unifications(HeadVars, Args, call(PredCallId),
			Goal2, VarSet1, Goal, VarSet)
	).

transform_goal(unify(A0, B0), VarSet0, Subst, Goal, VarSet) :-
	term__apply_substitution(A0, Subst, A),
	term__apply_substitution(B0, Subst, B),
	unravel_unification(A, B, explicit, [], VarSet0, Goal, VarSet).

%-----------------------------------------------------------------------------

	% `insert_arg_unifications' takes a list of variables,
	% a list of terms to unify them with, and a goal, and
	% inserts the appropriate unifications onto the front of
	% the goal.  It calls `unravel_unification' to ensure
	% that each unification gets reduced to superhomogeneous form.
	% It also gets passed a `arg_context', which indicates
	% where the terms came from.

:- type arg_context
	--->	head		% the arguments in the head of the clause
	;	call(pred_call_id) % the arguments in a call to a predicate
	;	functor(	% the arguments in a functor
			cons_id,
			unify_main_context,
			unify_sub_contexts
		).

		
:- pred insert_arg_unifications(list(var), list(term), arg_context,
				hlds__goal, varset, hlds__goal, varset).
:- mode insert_arg_unifications(in, in, in, in, in, out, out) is det.

insert_arg_unifications(HeadVars, Args, ArgContext, Goal0, VarSet0,
			Goal, VarSet) :-
	( HeadVars = [] ->
		Goal = Goal0,
		VarSet = VarSet0
	;
		Goal0 = _ - GoalInfo,
		goal_to_conj_list(Goal0, List0),
		insert_arg_unifications_2(HeadVars, Args, ArgContext, 0,
			List0, VarSet0, List, VarSet),
		conj_list_to_goal(List, GoalInfo, Goal)
	).

:- pred insert_arg_unifications_2(list(var), list(term), arg_context, int,
				list(hlds__goal), varset,
				list(hlds__goal), varset).
:- mode insert_arg_unifications_2(in, in, in, in, in, in, out, out) is det.

:- insert_arg_unifications_2(A, B, _, _, _, _, _, _) when A and B.

insert_arg_unifications_2([], [_|_], _, _, _, _, _, _) :-
	error("insert_arg_unifications_2: length mismatch").
insert_arg_unifications_2([_|_], [], _, _, _, _, _, _) :-
	error("insert_arg_unifications_2: length mismatch").
insert_arg_unifications_2([], [], _, _, List, VarSet, List, VarSet).
insert_arg_unifications_2([Var|Vars], [Arg|Args], Context, N0, List0, VarSet0,
				List, VarSet) :-
	N1 is N0 + 1,
		% skip unifications of the form `X = X'
	( Arg = term__variable(Var) ->
		insert_arg_unifications_2(Vars, Args, Context, N1,
				List0, VarSet0, List, VarSet)
	;
		arg_context_to_unify_context(Context, N1,
				UnifyMainContext, UnifySubContext),
		unravel_unification(term__variable(Var), Arg, UnifyMainContext,
				UnifySubContext, VarSet0, Goal, VarSet1),
		goal_to_conj_list(Goal, ConjList),
		list__append(ConjList, List1, List),
		insert_arg_unifications_2(Vars, Args, Context, N1,
				List0, VarSet1, List1, VarSet)
	).

:- pred append_arg_unifications(list(var), list(term), arg_context,
				hlds__goal, varset, hlds__goal, varset).
:- mode append_arg_unifications(in, in, in, in, in, out, out) is det.

append_arg_unifications(HeadVars, Args, ArgContext, Goal0, VarSet0,
			Goal, VarSet) :-
	( HeadVars = [] ->
		Goal = Goal0,
		VarSet = VarSet0
	;
		Goal0 = _ - GoalInfo,
		goal_to_conj_list(Goal0, List0),
		append_arg_unifications_2(HeadVars, Args, ArgContext, 0,
			List0, VarSet0, List, VarSet),
		conj_list_to_goal(List, GoalInfo, Goal)
	).

:- pred append_arg_unifications_2(list(var), list(term), arg_context, int,
				list(hlds__goal), varset,
				list(hlds__goal), varset).
:- mode append_arg_unifications_2(in, in, in, in, in, in, out, out) is det.

:- append_arg_unifications_2(A, B, _, _, _, _, _, _) when A and B.

append_arg_unifications_2([], [_|_], _, _, _, _, _, _) :-
	error("append_arg_unifications_2: length mismatch").
append_arg_unifications_2([_|_], [], _, _, _, _, _, _) :-
	error("append_arg_unifications_2: length mismatch").
append_arg_unifications_2([], [], _, _, List, VarSet, List, VarSet).
append_arg_unifications_2([Var|Vars], [Arg|Args], Context, N0, List0, VarSet0,
				List, VarSet) :-
	N1 is N0 + 1,
		% skip unifications of the form `X = X'
	( Arg = term__variable(Var) ->
		append_arg_unifications_2(Vars, Args, Context, N1,
				List0, VarSet0, List, VarSet)
	;
		arg_context_to_unify_context(Context, N1,
				UnifyMainContext, UnifySubContext),
		unravel_unification(term__variable(Var), Arg, UnifyMainContext,
				UnifySubContext, VarSet0, Goal, VarSet1),
		goal_to_conj_list(Goal, ConjList),
		list__append(List0, ConjList, List1),
		append_arg_unifications_2(Vars, Args, Context, N1,
				List1, VarSet1, List, VarSet)
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

:- pred make_fresh_arg_vars(list(term), varset, list(var), varset).
:- mode make_fresh_arg_vars(in, in, out, out) is det.

make_fresh_arg_vars(Args, VarSet0, Vars, VarSet) :-
	make_fresh_arg_vars_2(Args, [], VarSet0, Vars1, VarSet),
	list__reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(term), list(var), varset,
				list(var), varset).
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

:- pred unravel_unification(term, term, unify_main_context,
				unify_sub_contexts, varset, hlds__goal, varset).
:- mode unravel_unification(in, in, in, in, in, out, out) is det.

	% `X = Y' needs no unravelling.

unravel_unification(term__variable(X), term__variable(Y), MainContext,
			SubContext, VarSet0, Goal, VarSet) :-
	create_atomic_unification(term__variable(X), term__variable(Y),
			MainContext, SubContext, Goal),
	VarSet0 = VarSet.

	% If we find a unification of the form
	%	X = f(A1, A2, A3)
	% we replace it with
	%	X = f(NewVar1, NewVar2, NewVar3),
	%	NewVar1 = A1,
	%	NewVar2 = A2,
	%	NewVar3 = A3.
	% In the trivial case `X = c', no unravelling occurs.

unravel_unification(term__variable(X), term__functor(F, Args, C), MainContext,
			SubContext, VarSet0, Goal, VarSet) :-
	( Args = [] ->
		create_atomic_unification(term__variable(X),
				term__functor(F, Args, C),
				MainContext, SubContext, Goal),
		VarSet = VarSet0
	;
		make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1),
		term__var_list_to_term_list(HeadVars, HeadArgs),
		create_atomic_unification(term__variable(X),
				term__functor(F, HeadArgs, C),
				MainContext, SubContext, Goal0),
		list__length(Args, Arity),
		make_functor_cons_id(F, Arity, ConsId),
		ArgContext = functor(ConsId, MainContext, SubContext),
		append_arg_unifications(HeadVars, Args, ArgContext,
					Goal0, VarSet1, Goal, VarSet)
	).

	% Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification(term__functor(F, As, C), term__variable(Y), MC, SC,
			VarSet0, Goal, VarSet) :-
	unravel_unification(term__variable(Y), term__functor(F, As, C), MC, SC,
			VarSet0, Goal, VarSet).

	% If we find a unification of the form `f1(...) = f2(...)',
	% then we replace it with `Tmp = f1(NewVars1), Tmp = f2(NewVars2),
	% NewVars1 = ..., NewVars2 = ...'. 
	% Note that we can't simplify it yet, because we might simplify
	% away type errors.

unravel_unification(term__functor(LeftF, LeftAs, LeftC),
			term__functor(RightF, RightAs, RightC),
			MainContext, SubContext, VarSet0, Goal, VarSet) :-
	make_fresh_arg_vars(LeftAs, VarSet0, LeftHeadVars, VarSet1),
	make_fresh_arg_vars(RightAs, VarSet1, RightHeadVars, VarSet2),
	term__var_list_to_term_list(LeftHeadVars, LeftHeadArgs),
	term__var_list_to_term_list(RightHeadVars, RightHeadArgs),
	list__length(LeftAs, LeftArity),
	list__length(RightAs, RightArity),
	make_functor_cons_id(LeftF, LeftArity, LeftConsId),
	make_functor_cons_id(RightF, RightArity, RightConsId),
	LeftArgContext = functor(LeftConsId, MainContext, SubContext),
	RightArgContext = functor(RightConsId, MainContext, SubContext),
	varset__new_var(VarSet2, TmpVar, VarSet3),
	create_atomic_unification(term__variable(TmpVar),
			term__functor(LeftF, LeftHeadArgs, LeftC),
			MainContext, SubContext, Goal0),
	create_atomic_unification(term__variable(TmpVar),
			term__functor(RightF, RightHeadArgs, RightC),
			MainContext, SubContext, Goal1),
	goal_info_init(GoalInfo),
	goal_to_conj_list(Goal0, ConjList0),
	goal_to_conj_list(Goal1, ConjList1),
	list__append(ConjList0, ConjList1, ConjList),
	conj_list_to_goal(ConjList, GoalInfo, Goal2),
	append_arg_unifications(RightHeadVars, RightAs, RightArgContext,
				Goal2, VarSet3, Goal3, VarSet4),
	append_arg_unifications(LeftHeadVars, LeftAs, LeftArgContext, Goal3,
				VarSet4, Goal, VarSet).

	% create the hlds__goal for a unification which cannot be
	% further simplified, filling in all the as yet
	% unknown slots with dummy values

create_atomic_unification(A, B, UnifyMainContext, UnifySubContext, Goal) :-
	UMode = ((free - free) -> (free - free)),
	Mode = ((free -> free) - (free -> free)),
	map__init(Follow),
	UnifyInfo = complicated_unify(UMode, nondeterministic, Follow),
	UnifyC = unify_context(UnifyMainContext, UnifySubContext),
	goal_info_init(GoalInfo),
	Goal = unify(A, B, Mode, UnifyInfo, UnifyC) - GoalInfo.

%-----------------------------------------------------------------------------%

% substitute_vars(Vars0, Subst, Vars)
%	apply substitiution `Subst' (which must only rename vars) to `Vars0',
%	and return the result in `Vars'.

:- pred substitute_vars(list(var), substitution, list(var)).
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

:- pred get_conj(goal, substitution, list(hlds__goal), varset,
		list(hlds__goal), varset).
:- mode get_conj(in, in, in, in, out, out) is det.

get_conj(Goal, Subst, Conj0, VarSet0, Conj, VarSet) :-
	(
		%some [A,B]
		Goal = (A,B)
	->
		get_conj(B, Subst, Conj0, VarSet0, Conj1, VarSet1),
		get_conj(A, Subst, Conj1, VarSet1, Conj, VarSet)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet),
		goal_to_conj_list(Goal1, ConjList),
		list__append(ConjList, Conj0, Conj)
	).

% get_disj(Goal, Subst, Disj0, Disj) :
% 	Goal is a tree of disjuncts.  Flatten it into a list (applying Subst)
%	append Disj0, and return the result in Disj.

:- pred get_disj(goal, substitution, list(hlds__goal), varset,
		list(hlds__goal), varset).
:- mode get_disj(in, in, in, in, out, out) is det.

get_disj(Goal, Subst, Disj0, VarSet0, Disj, VarSet) :-
	(
		%some [A,B]
		Goal = (A;B)
	->
		get_disj(B, Subst, Disj0, VarSet0, Disj1, VarSet1),
		get_disj(A, Subst, Disj1, VarSet1, Disj, VarSet)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet),
		Disj = [Goal1 | Disj0]
	).

%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred multiple_def_error(sym_name, int, string, term__context,
				io__state, io__state).
:- mode multiple_def_error(in, in, in, in, di, uo) is det.

multiple_def_error(Name, Arity, DefType, Context) -->
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' multiply defined\n").

:- pred undefined_pred_error(sym_name, int, term__context, string,
				io__state, io__state).
:- mode undefined_pred_error(in, in, in, in, di, uo) is det.

undefined_pred_error(Name, Arity, Context, Description) -->
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' without preceding pred declaration\n").

:- pred unspecified_det_warning(sym_name, int, term__context, 
				io__state, io__state).
:- mode unspecified_det_warning(in, in, in, di, uo) is det.

unspecified_det_warning(Name, Arity, Context) -->
	prog_out__write_context(Context),
	io__write_string("Warning: no determinism declaration for local pred\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n"),
	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	( { VerboseErrors = yes } ->
		prog_out__write_context(Context),
		io__write_string("  (The determinism of this predicate will be automatically\n"),
		io__write_string("  inferred by the compiler.)")
	;
		[]
	).

:- pred unspecified_det_error(sym_name, int, term__context, 
				io__state, io__state).
:- mode unspecified_det_error(in, in, in, di, uo) is det.

unspecified_det_error(Name, Arity, Context) -->
	prog_out__write_context(Context),
	io__write_string("Error: no determinism declaration for exported pred\n"),
	prog_out__write_context(Context),
	io__write_string("  `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'\n").

:- pred clause_for_imported_pred_error(sym_name, int, term__context, 
				io__state, io__state).
:- mode clause_for_imported_pred_error(in, in, in, di, uo) is det.

clause_for_imported_pred_error(Name, Arity, Context) -->
	prog_out__write_context(Context),
	io__write_string("Error: clause for imported pred `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'.\n").

%-----------------------------------------------------------------------------%
