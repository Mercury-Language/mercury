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
% XXX we should record each error using moduleinfo_incr_errors.

% WISHLIST - we should handle explicit module quantification

:- module make_hlds.
:- interface.
:- import_module prog_io, hlds.

:- pred parse_tree_to_hlds(program, module_info, io__state, io__state).
:- mode parse_tree_to_hlds(input, output, di, uo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_util, prog_out, set, require, globals, options.
:- import_module hlds_out.

parse_tree_to_hlds(module(Name, Items), Module) -->
	{ moduleinfo_init(Name, Module0) },
	add_item_list_decls(Items, Module0, Module1),
	lookup_option(statistics, bool(Statistics)),
	maybe_report_stats(Statistics),
	{ module_balance(Module1, Module2) },
	lookup_option(statistics, bool(Statistics)),
	maybe_report_stats(Statistics),
	add_item_list_clauses(Items, Module2, Module3),
		% the predid list is constructed in reverse order, for
		% effiency, so we return it to the correct order here.
	{ moduleinfo_predids(Module3, RevPredIds) },
	{ reverse(RevPredIds, PredIds) },
	{ moduleinfo_set_predids(Module3, PredIds, Module) }.

	% After we have finished constructing the symbol tables,
	% we balance all the binary trees, to improve performance
	% in later stages of the compiler.

:- pred module_balance(module_info, module_info).
:- mode module_balance(in, out).

module_balance(ModuleInfo0, ModuleInfo) :-

	moduleinfo_preds(ModuleInfo0, Preds0),
	bintree__balance(Preds0, Preds),
	moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo1),

	moduleinfo_pred_name_index(ModuleInfo1, PredNameIndex0),
	bintree__balance(PredNameIndex0, PredNameIndex),
	moduleinfo_set_pred_name_index(ModuleInfo1, PredNameIndex, ModuleInfo2),

	moduleinfo_types(ModuleInfo2, Types0),
	bintree__balance(Types0, Types),
	moduleinfo_set_types(ModuleInfo2, Types, ModuleInfo3),

	moduleinfo_insts(ModuleInfo3, Insts0),
	bintree__balance(Insts0, Insts),
	moduleinfo_set_insts(ModuleInfo3, Insts, ModuleInfo4),

	moduleinfo_modes(ModuleInfo4, Modes0),
	bintree__balance(Modes0, Modes),
	moduleinfo_set_modes(ModuleInfo4, Modes, ModuleInfo5),

	moduleinfo_ctors(ModuleInfo5, Ctors0),
	bintree__balance(Ctors0, Ctors),
	moduleinfo_set_ctors(ModuleInfo5, Ctors, ModuleInfo).

%-----------------------------------------------------------------------------%

	% add the declarations one by one to the module

:- pred add_item_list_decls(item_list, module_info, module_info,
				io__state, io__state).
:- mode add_item_list_decls(input, input, output, di, uo).

add_item_list_decls([], Module, Module) --> [].
add_item_list_decls([Item - Context | Items], Module0, Module) -->
	(add_item_decl(Item, Context, Module0, Module1) ->
		{ true }
	;
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("Internal error in make_hlds.\n"),
		io__write_string("Failed to process the following item:\n"),
		io__write_anything(Item),
		io__write_string("\n"),
		io__set_output_stream(OldStream, _),
		{ moduleinfo_incr_errors(Module0, Module1) }
	),
	add_item_list_decls(Items, Module1, Module).

	% add the clauses one by one to the module

:- pred add_item_list_clauses(item_list, module_info, module_info,
				io__state, io__state).
:- mode add_item_list_clauses(input, input, output, di, uo).

add_item_list_clauses([], Module, Module) --> [].
add_item_list_clauses([Item - Context | Items], Module0, Module) -->
	(add_item_clause(Item, Context, Module0, Module1) ->
		{ true }
	;
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		io__write_string("\n"),
		prog_out__write_context(Context),
		io__write_string("Internal error in make_hlds.\n"),
		io__write_string("Failed to process the following clause:\n"),
		io__write_anything(Item),
		io__write_string("\n"),
		io__set_output_stream(OldStream, _),
		{ moduleinfo_incr_errors(Module0, Module1) }
	),
	add_item_list_clauses(Items, Module1, Module).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_decl(item, term__context, module_info, module_info,
			io__state, io__state).
:- mode add_item_decl(input, input, input, output, di, uo).

add_item_decl(clause(_, _, _, _), _, Module, Module) --> [].	% skip clauses

add_item_decl(type_defn(VarSet, TypeDefn, Cond), Context, Module0, Module) -->
	module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Context, Module).

add_item_decl(inst_defn(VarSet, InstDefn, Cond), Context, Module0, Module) -->
	module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Context, Module).

add_item_decl(mode_defn(VarSet, ModeDefn, Cond), Context, Module0, Module) -->
	module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Context, Module).

add_item_decl(pred(VarSet, PredName, TypesAndModes, Det, Cond), Context,
		Module0, Module) -->
	module_add_pred(Module0, VarSet, PredName, TypesAndModes, Det, Cond,
		Context, Module).

add_item_decl(mode(VarSet, PredName, Modes, Det, Cond), Context, Module0,
		Module) -->
	module_add_mode(Module0, VarSet, PredName, Modes, Det, Cond, Context,
		Module).

add_item_decl(module_defn(_VarSet, ModuleDefn), Context, Module, Module) -->
	( { ModuleDefn = interface } ->
		[]
	; { ModuleDefn = implementation } ->
		[]
	; { ModuleDefn = import(module(_)) } ->
		[]
	;
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("warning: declaration not yet implemented.\n"),
		io__set_output_stream(OldStream, _)
	).

add_item_decl(nothing, _, Module, Module) -->
	[].

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

:- pred add_item_clause(item, term__context, module_info, module_info,
			io__state, io__state).
:- mode add_item_clause(input, input, input, output, di, uo).

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


:- pred module_add_inst_defn(module_info, varset, inst_defn, condition,
			term__context, module_info, io__state, io__state).
:- mode module_add_inst_defn(input, input, input, input, input, output, di, uo).

module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Context, Module) -->
	{ moduleinfo_insts(Module0, Insts0) },
	insts_add(Insts0, VarSet, InstDefn, Cond, Context, Insts),
	{ moduleinfo_set_insts(Module0, Insts, Module) }.

	% XXX handle abstract insts

:- pred insts_add(inst_table, varset, inst_defn, condition, term__context,
			inst_table, io__state, io__state).
:- mode insts_add(input, input, input, input, input, output, di, uo).
insts_add(Insts0, VarSet, eqv_inst(Name, Args, Body), Cond, Context, Insts) -->
	{ length(Args, Arity),
	  I = hlds__inst_defn(VarSet, Args, eqv_inst(Body), Cond, Context) },
	(
		% some [I2]		% NU-Prolog inconsistency
		{ map__search(Insts0, Name - Arity, I2) }
	->
		{ Insts = Insts0 },
		(
			{ inst_is_compat(I, I2) }
		->
			duplicate_def_warning(Name, Arity, "inst", Context)
		;
			multiple_def_error(Name, Arity, "inst", Context)
		)
	;
		{ map__insert(Insts0, Name - Arity, I, Insts) }
	).

	% Two different inst definitions are compatible if
	% their mode parameters and their bodies are identical.
	% (This is perhaps more strict than it need be.)

:- pred inst_is_compat(hlds__inst_defn, hlds__inst_defn).
:- mode inst_is_compat(input, input).

inst_is_compat(hlds__inst_defn(_, Args, Body, _, _),
		hlds__inst_defn(_, Args, Body, _, _)).

%-----------------------------------------------------------------------------%

:- pred module_add_mode_defn(module_info, varset, mode_defn, condition,
			term__context, module_info, io__state, io__state).
:- mode module_add_mode_defn(input, input, input, input, input, output, di, uo).

module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Context, Module) -->
	{ moduleinfo_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, ModeDefn, Cond, Context, Modes),
	{ moduleinfo_set_modes(Module0, Modes, Module) }.

:- pred modes_add(mode_table, varset, mode_defn, condition, term__context,
			mode_table, io__state, io__state).
:- mode modes_add(input, input, input, input, input, output, di, uo).

modes_add(Modes0, VarSet, eqv_mode(Name, Args, Body), Cond, Context, Modes) -->
	{ length(Args, Arity),
	  I = hlds__mode_defn(VarSet, Args, eqv_mode(Body), Cond, Context) },
	(
		% some [I2]		% NU-Prolog inconsistency
		{ map__search(Modes0, Name - Arity, I2) }
	->
		{ Modes = Modes0 },
		(
			{ mode_is_compat(I, I2) }
		->
			duplicate_def_warning(Name, Arity, "mode", Context)
		;
			multiple_def_error(Name, Arity, "mode", Context)
		)
	;
		{ map__insert(Modes0, Name - Arity, I, Modes) }
	).

:- pred mode_name_args(mode_defn, sym_name, list(inst_param), hlds__mode_body).
:- mode mode_name_args(input, output, output, output).

mode_name_args(eqv_mode(Name, Args, Body), Name, Args, eqv_mode(Body)).

:- pred mode_is_compat(hlds__mode_defn, hlds__mode_defn).
:- mode mode_is_compat(input, input).

mode_is_compat(hlds__mode_defn(_, Args, Body, _, _),
		hlds__mode_defn(_, Args, Body, _, _)).

%-----------------------------------------------------------------------------%

	% We allow more than one "definition" for a given type so
	% long all of them except one are actually just declarations,
	% e.g. `:- type t.', which is parsed as an type definition for
	% t which defines t as an abstract_type.

:- pred module_add_type_defn(module_info, varset, type_defn, condition,
			term__context, module_info, io__state, io__state).
:- mode module_add_type_defn(input, input, input, input, input, output, di, uo).

module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Context, Module) -->
	{ moduleinfo_types(Module0, Types0) },
	{ type_name_args(TypeDefn, Name, Args, Body),
	  length(Args, Arity),
	  T = hlds__type_defn(VarSet, Args, Body, Cond, Context) },
	(
		% if there was an existing non-abstract definition for the type
		{ map__search(Types0, Name - Arity, T2) },
		{ T2 = hlds__type_defn(_, _, Body_2, _, _) },
		{ Body_2 \= abstract_type }
	->
		{ Module = Module0 },
	  	(
			% then if this definition was abstract, ignore it
			{ Body = abstract_type }
		->
			[]
		;
			% otherwise give a warning or an error
			{ type_is_compat(T, T2) }
		->
			duplicate_def_warning(Name, Arity, "type", Context)
		;
			multiple_def_error(Name, Arity, "type", Context)
		)
	;
		{ 
		  TypeId = Name - Arity,
		  map__set(Types0, TypeId, T, Types)
		},
		( %%% some [ConsList]
			{ Body = du_type(ConsList) }
		->
			{ moduleinfo_ctors(Module0, Ctors0) },
			ctors_add(ConsList, TypeId, Context, Ctors0, Ctors),
			{ moduleinfo_set_ctors(Module0, Ctors, Module1) }
		;
			{ Module1 = Module0 }
		),
		{ moduleinfo_set_types(Module1, Types, Module) },
		( { Body = uu_type(_) } ->
			io__stderr_stream(StdErr),
			io__set_output_stream(StdErr, OldStream),
			prog_out__write_context(Context),
			io__write_string(
		"warning: undiscriminated union types not yet implemented.\n"),
			io__set_output_stream(OldStream, _)
		;
			[]
		)
	).

:- pred type_name_args(type_defn, sym_name, list(type_param), hlds__type_body).
:- mode type_name_args(input, output, output, output).

type_name_args(du_type(Name, Args, Body), Name, Args, du_type(Body)).
type_name_args(uu_type(Name, Args, Body), Name, Args, uu_type(Body)).
type_name_args(eqv_type(Name, Args, Body), Name, Args, eqv_type(Body)).
type_name_args(abstract_type(Name, Args), Name, Args, abstract_type).

	% Two type definitions are compatible if they have exactly the
	% same argument lists and bodies.

:- pred type_is_compat(hlds__type_defn, hlds__type_defn).
:- mode type_is_compat(input, input).

type_is_compat( hlds__type_defn(_, Args, Body, _, _),
		hlds__type_defn(_, Args, Body, _, _)).

:- pred ctors_add(list(constructor), type_id, term__context, cons_table,
			cons_table, io__state, io__state).
:- mode ctors_add(input, input, input, input, output, di, uo).


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
	( { member(hlds__cons_defn(_, TypeId, _), ConsDefns1) } ->
		io__stderr_stream(StdErr),
		io__set_output_stream(StdErr, OldStream),
		prog_out__write_context(Context),
		io__write_string("error: constructor `"),
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

%-----------------------------------------------------------------------------%

:- pred module_add_pred(module_info, varset, sym_name, list(type_and_mode),
		determinism, condition, term__context, module_info,
		io__state, io__state).
:- mode module_add_pred(input, input, input, input, input, input, input, output,
		di, uo).

module_add_pred(Module0, VarSet, PredName, TypesAndModes, Det, Cond, Context,
		Module) -->
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	preds_add(Module0, VarSet, PredName, Types, Cond, Context, Module1),
	(
		% some [Modes]
		{ MaybeModes = yes(Modes) }
	->
		module_add_mode(Module1, VarSet, PredName, Modes, Det, Cond,
			Context, Module)
	;
		{ Module = Module1 }
	).

:- pred preds_add(module_info, varset, sym_name, list(type),
		condition, term__context, module_info, io__state, io__state).
:- mode preds_add(input, input, input, input, input, input, output,
		di, uo).

preds_add(Module0, VarSet, Name, Types, Cond, Context, Module) -->
	{ moduleinfo_name(Module0, ModuleName) },
	{ moduleinfo_preds(Module0, Preds0) },
	{ length(Types, Arity),
	  map__init(Procs),
	  make_predid(ModuleName, Name, Arity, PredId),
	  clauses_info_init(Arity, ClausesInfo),
	  PredContainsError = no,
	  P = predicate(VarSet, Types, Cond, ClausesInfo, Procs, Context,
			PredContainsError) },
	(
		% some [P2]
		{ map__search(Preds0, PredId, P2) }
	->
		{ Module = Module0 },
		(
			{ pred_is_compat(P, P2) }
		->
			duplicate_def_warning(Name, Arity, "pred", Context)
		;
			multiple_def_error(Name, Arity, "pred", Context)
		)
	;
		{ map__insert(Preds0, PredId, P, Preds) },
		{ moduleinfo_set_preds(Module0, Preds, Module1) },
		{ moduleinfo_predids(Module1, PredIds0) },
		{ moduleinfo_set_predids(Module1, [PredId | PredIds0],
				Module2) },
		{ moduleinfo_pred_name_index(Module2, PredNameIndex0) },
		{ unqualify_name(Name, UnqualifiedName) },
		{
			map__search(PredNameIndex0, UnqualifiedName, PredIdList)
		->
			map__set(PredNameIndex0, UnqualifiedName,
				[PredId | PredIdList], PredNameIndex)
		;
			map__insert(PredNameIndex0, UnqualifiedName,
				[PredId], PredNameIndex)
		},
		{ moduleinfo_set_pred_name_index(Module2, PredNameIndex,
				Module) }
	).

	% XXX This doesn't work.

:- pred pred_is_compat(pred_info, pred_info).
:- mode pred_is_compat(input, input).

pred_is_compat(PredInfo1, PredInfo2) :-
	predinfo_arg_types(PredInfo1, _, ArgTypes1),
	predinfo_arg_types(PredInfo2, _, ArgTypes2),
	ArgTypes1 = ArgTypes2.
 
%-----------------------------------------------------------------------------%

	% Add a mode declaration for a predicate.

:- pred module_add_mode(module_info, varset, sym_name, list(mode), determinism,
			condition, term__context, module_info,
			io__state, io__state).
:- mode module_add_mode(input, input, input, input, input, input, input, output,
			di, uo).

module_add_mode(Module0, VarSet, PredName, Modes, Det, Cond, Context, Module)
		-->
	{ moduleinfo_preds(Module0, Preds0) },
	{ moduleinfo_name(Module0, ModuleName) },
	pred_modes_add(Preds0, ModuleName, VarSet, PredName, Modes, Det, Cond,
			Context, Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module) }.

:- pred pred_modes_add(pred_table, module_name, varset, sym_name, list(mode),
		determinism, condition, term__context, pred_table,
		io__state, io__state).
:- mode pred_modes_add(input, input, input, input, input, input, input,
		input, output, di, uo).

	% XXX we should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments

pred_modes_add(Preds0, ModuleName, VarSet, PredName, Modes, Det, Cond,
		MContext, Preds) -->
	{ length(Modes, Arity),
	  make_predid(ModuleName, PredName, Arity, PredId) },
	(
		% some [P0]
		{ map__search(Preds0, PredId, P0) }
	->
		{ predinfo_procedures(P0, Procs0) },
			% XXX we should check that this mode declaration
			% isn't the same as an existing one
		{ next_mode_id(Procs0, ModeId) },
		{ procinfo_init(Modes, Det, MContext, NewProc) },
		{ map__insert(Procs0, ModeId, NewProc, Procs) },
		{ predinfo_set_procedures(P0, Procs, P) },
		{ map__set(Preds0, PredId, P, Preds) }
	;
		undefined_pred_error(PredName, Arity, MContext,	
			"mode declaration"),
		{ preds_add_implicit(Preds0, PredId, MContext, Preds1) },
		pred_modes_add(Preds1, ModuleName, VarSet, PredName,
				Modes, Det, Cond, MContext, Preds)
	).

	% Whenever there is a clause or mode declaration for an undeclared
	% predicate, we add an implicit declaration for that predicate.

:- pred preds_add_implicit(pred_table, pred_id, term__context, pred_table).
:- mode preds_add_implicit(input, input, input, output).

preds_add_implicit(Preds0, PredId, Context, Preds) :-
	predicate_arity(PredId, Arity),
	clauses_info_init(Arity, ClausesInfo),
	map__init(Procs),
	varset__init(TVarSet0),
	make_n_fresh_vars(Arity, TVarSet0, TypeVars, TVarSet),
	var_list_to_term_list(TypeVars, Types),
	P = predicate(TVarSet, Types, true, ClausesInfo, Procs, Context, no),
	map__set(Preds0, PredId, P, Preds).

:- pred var_list_to_term_list(list(var), list(term)).
:- mode var_list_to_term_list(input, output) is det.
:- mode var_list_to_term_list(output, input).

var_list_to_term_list([], []).
var_list_to_term_list([V | Vs0], [term_variable(V) | Vs]) :-
	var_list_to_term_list(Vs0, Vs).

	% XXX efficiency could be improved
	% we should probably store the next available ModeId rather
	% than recomputing it all the time

:- pred next_mode_id(proc_table, proc_id).
:- mode next_mode_id(input, output).

next_mode_id(Procs, ModeId) :-
	map__to_assoc_list(Procs, List),
	length(List, ModeId).

%-----------------------------------------------------------------------------%

:- pred module_add_clause(module_info, varset, sym_name, list(term), goal,
			term__context, module_info, io__state, io__state).
:- mode module_add_clause(input, input, input, input, input, input, output,
			di, uo).

module_add_clause(Module0, VarSet, PredName, Args, Body, Context, Module) -->
	{ moduleinfo_preds(Module0, Preds0) },
	{ moduleinfo_name(Module0, ModuleName) },
	clauses_add(Preds0, ModuleName, VarSet, PredName, Args, Body, Context,
		Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module) }.

:- pred clauses_add(pred_table, module_name, varset, sym_name, list(term),
			goal, term__context, pred_table, io__state, io__state).
:- mode clauses_add(input, input, input, input, input, input, input, output,
			di, uo).

clauses_add(Preds0, ModuleName, VarSet, PredName, Args, Body, Context,
		Preds) -->
	{ length(Args, Arity) },
	{ make_predid(ModuleName, PredName, Arity, PredId) },
	lookup_option(very_verbose, bool(VeryVerbose)),
	( { VeryVerbose = yes } ->
		io__write_string("% Processing clause for pred `"),
		{ predicate_name(PredId, PName) },
		io__write_string(PName),
		io__write_string("/"),
		io__write_int(Arity),
		io__write_string("'...\n")
	;
		[]
	),
	maybe_warn_singletons(VarSet, PredId, Args, Body, Context),
	(
		% some [PredInfo0]
		{ map__search(Preds0, PredId, PredInfo0) }
	->
			% XXX abstract predicate/4
		{ predinfo_clauses_info(PredInfo0, Clauses0), 
		  predinfo_procedures(PredInfo0, Procs),
		  map__keys(Procs, ModeIds),
		  clauses_info_add_clause(Clauses0, ModeIds, VarSet, Args,
				Body, Context, Clauses),
		  predinfo_set_clauses_info(PredInfo0, Clauses, PredInfo),
		  map__set(Preds0, PredId, PredInfo, Preds) }
	;
		undefined_pred_error(PredName, Arity, Context, "clause"),
		{ preds_add_implicit(Preds0, PredId, Context, Preds1) },
		clauses_add(Preds1, ModuleName, VarSet, PredName, Args,
			Body, Context, Preds)
	).

%-----------------------------------------------------------------------------

	% Warn about variables which occur only once but don't start with
	% an underscore, or about variables which do start with an underscore
	% but occur more than once.
	%
	% XXX We shouldn't warn about variables which start with '_'
	%     unless they occur more than once *in the same scope*.
	%
	% XXX This is O(N*N) in the number of vars per clause

:- pred maybe_warn_singletons(varset, pred_id, list(term), goal, term__context,
					io__state, io__state).
:- mode maybe_warn_singletons(in, in, in, in, in, di, uo).

maybe_warn_singletons(VarSet, PredId, Args, Body, Context) -->
	lookup_option(warn_singleton_vars, bool(WarnSingletonVars)),
	( { WarnSingletonVars = yes } ->
		{ term__vars_list(Args, VarList0) },
		{ vars_in_goal(Body, VarList0, VarList) },
		warn_singletons(VarList, VarSet, PredId, Context)
	;	
		[]
	).

:- pred warn_singletons(list(var), varset, pred_id, term__context,
			io__state, io__state).
:- mode warn_singletons(in, in, in, in, di, uo).

warn_singletons([], _, _, _) --> [].
warn_singletons([Var | Vars0], VarSet, PredId, Context) -->
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
				hlds_out__write_pred_id(PredId),
				io__write_string("':\n"),
				prog_out__write_context(Context),
				io__write_string("  Warning: variable `"),
				io__write_variable(Var, VarSet),
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
				hlds_out__write_pred_id(PredId),
				io__write_string("':\n"),
				prog_out__write_context(Context),
				io__write_string("  Warning: variable `"),
				io__write_variable(Var, VarSet),
				io__write_string("' occurs only once.\n")
			;
				[]
			)
		)
	;
		[]
	),
	warn_singletons(Vars1, VarSet, PredId, Context).


	% delete_all(List0, Elem, List, Found) is true iff
	% List is List0 with all occurrences of Elem removed,
	% and Found = 'yes' if Elem occurred in List0 and 'no' otherwise.

:- pred delete_all(list(T), T, list(T), bool).
:- mode delete_all(in, in, out, out).

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
vars_in_goal((A;B)) -->
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal(not(Vs, G)) -->
	append(Vs),
	vars_in_goal(G).
vars_in_goal(some(Vs, G)) -->
	append(Vs),
	vars_in_goal(G).
vars_in_goal(all(Vs, G)) -->
	append(Vs),
	vars_in_goal(G).
vars_in_goal(unify(A, B)) -->
	vars_in_term(A),
	vars_in_term(B).
vars_in_goal(call(Term)) -->
	vars_in_term(Term).
vars_in_goal(if_then(Vars,A,B)) -->
	append(Vars),
	vars_in_goal(A),
	vars_in_goal(B).
vars_in_goal(if_then_else(Vars,A,B,C)) -->
	append(Vars),
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
	append(ClauseList0, [clause(ModeIds, Goal, Context)], ClauseList),
	ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, ClauseList).

%-----------------------------------------------------------------------------

:- pred transform(substitution, list(var), list(term), goal, varset,
			hlds__goal, varset).
:- mode transform(input, input, input, input, input, output, output) is det.

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
:- mode transform_goal(input, input, input, output, output).

transform_goal(fail, VarSet, _, disj([]) - GoalInfo, VarSet) :-
	goalinfo_init(GoalInfo).

transform_goal(true, VarSet, _, conj([]) - GoalInfo, VarSet) :-
	goalinfo_init(GoalInfo).

transform_goal(some(Vars0, Goal0), VarSet0, Subst,
		some(Vars, Goal) - GoalInfo, VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(Goal0, VarSet0, Subst, Goal, VarSet),
	goalinfo_init(GoalInfo).

transform_goal(all(Vars0, Goal0), VarSet0, Subst,
		all(Vars, Goal) - GoalInfo, VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(Goal0, VarSet0, Subst, Goal, VarSet),
	goalinfo_init(GoalInfo).

transform_goal(if_then_else(Vars0, A0, B0, C0), VarSet0, Subst,
		if_then_else(Vars, A, B, C) - GoalInfo, VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(A0, VarSet0, Subst, A, VarSet1),
	transform_goal(B0, VarSet1, Subst, B, VarSet2),
	transform_goal(C0, VarSet2, Subst, C, VarSet),
	goalinfo_init(GoalInfo).

transform_goal(if_then(Vars0, A0, B0), Subst, VarSet0, Goal, VarSet) :-
	transform_goal(if_then_else(Vars0, A0, B0, fail), Subst, VarSet0,
		Goal, VarSet).

transform_goal(not(Vars0, A0), VarSet0, Subst,
		not(Vars, A) - GoalInfo, VarSet) :-
	substitute_vars(Vars0, Subst, Vars),
	transform_goal(A0, VarSet0, Subst, A, VarSet),
	goalinfo_init(GoalInfo).

transform_goal((A0,B0), VarSet0, Subst, conj(L) - GoalInfo, VarSet) :-
	get_conj(B0, Subst, [], VarSet0, L0, VarSet1),
	get_conj(A0, Subst, L0, VarSet1, L, VarSet),
	goalinfo_init(GoalInfo).

transform_goal((A0;B0), VarSet0, Subst, disj(L) - GoalInfo, VarSet) :-
	get_disj(B0, Subst, [], VarSet0, L0, VarSet1),
	get_disj(A0, Subst, L0, VarSet1, L, VarSet),
	goalinfo_init(GoalInfo).

transform_goal(call(Goal0), VarSet0, Subst, Goal, VarSet) :-

	% fill unused slots with any old junk 
	ModeId = 0,
	Builtin = not_builtin,

	% the module name will be determined by typecheck.nl
	% when it resolves predicate overloading
	ModuleName = "",

	term__apply_substitution(Goal0, Subst, Goal1),
	( Goal1 = term_functor(term_atom(PredName0), Args0, _) ->
		PredName = PredName0,
		Args = Args0
	;
		% If the called term is not an atom, then it is
		% either a variable, or something stupid like a number.
		% In the first case, we want to transform it to a call
		% to builtin:call/1, and in the latter case, we
		% In either case, we transform it to a call to call/1.
		% The latter case will will be caught by the type-checker.
		PredName = "call",
		Args = [Goal1]
	),
	length(Args, Arity),
	make_predid(ModuleName, unqualified(PredName), Arity, PredId),
	make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1),
	var_list_to_term_list(HeadVars, HeadArgs),
	Goal2 = call(PredId, ModeId, HeadArgs, Builtin) - GoalInfo,
	goalinfo_init(GoalInfo),
	insert_arg_unifications(HeadVars, Args, call(PredId), Goal2,
	 	VarSet1, Goal, VarSet).

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
	;	call(pred_id)	% the arguments in a call to a predicate
	;	functor(	% the arguments in a functor
			cons_id,
			unify_main_context,
			unify_sub_contexts
		).

:- pred insert_arg_unifications(list(var), list(term), arg_context,
				hlds__goal, varset, hlds__goal, varset).
:- mode insert_arg_unifications(input, input, input, input, input, 
				output, output).

insert_arg_unifications(HeadVars, Args, ArgContext, Goal0, VarSet0,
			Goal, VarSet) :-
	( HeadVars = [] ->
		Goal = Goal0,
		VarSet = VarSet0
	;
		goal_to_conj_list(Goal0, List0),
		insert_arg_unifications_2(HeadVars, Args, ArgContext, 0,
			List0, VarSet0, List, VarSet),
		goalinfo_init(GoalInfo),
		Goal = conj(List) - GoalInfo
	).

:- pred insert_arg_unifications_2(list(var), list(term), arg_context, int,
				list(hlds__goal), varset,
				list(hlds__goal), varset).
:- mode insert_arg_unifications_2(input, input, input, input, input, input,
				output, output) is det.

insert_arg_unifications_2([], [], _, _, List, VarSet, List, VarSet).
insert_arg_unifications_2([Var|Vars], [Arg|Args], Context, N0, List0, VarSet0,
				List, VarSet) :-
	N1 is N0 + 1,
	( Arg = term_variable(Var) ->
		% just skip unifications of the form `X = X'
		List = List1,
		VarSet1 = VarSet0
	;
		arg_context_to_unify_context(Context, N1,
				UnifyMainContext, UnifySubContext),
		unravel_unification(term_variable(Var), Arg, UnifyMainContext,
				UnifySubContext, VarSet0, Goal, VarSet1),
		( Goal = (conj(ConjList) - _) ->
			append(ConjList, List1, List)
		;
			List = [Goal | List1]
		)
	),
	insert_arg_unifications_2(Vars, Args, Context, N1, List0, VarSet1,
				List1, VarSet).

:- pred arg_context_to_unify_context(arg_context, int,
				unify_main_context, unify_sub_contexts).
:- mode arg_context_to_unify_context(input, input, output, output) is det.

arg_context_to_unify_context(head, N, head(N), []).
arg_context_to_unify_context(call(PredId), N, call(PredId, N), []).
arg_context_to_unify_context(functor(ConsId, MainContext, SubContexts), N,
			MainContext, [ConsId - N | SubContexts]).

%-----------------------------------------------------------------------------%

	% make_n_fresh_vars(N, VarSet0, Vars, VarSet):
	%	`Vars' is a list of `N' fresh variables allocated from
	%	`VarSet0'.  `VarSet' is the resulting varset.

:- pred make_n_fresh_vars(int, varset, list(var), varset).
:- mode make_n_fresh_vars(input, input, output, output).

make_n_fresh_vars(N, VarSet0, Vars, VarSet) :-
	make_n_fresh_vars_2(0, N, VarSet0, Vars, VarSet).

:- pred make_n_fresh_vars_2(int, int, varset, list(var), varset).
:- mode make_n_fresh_vars_2(input, input, input, output, output).

make_n_fresh_vars_2(N, Max, VarSet0, Vars, VarSet) :-
	(N = Max ->
		VarSet = VarSet0,
		Vars = []
	;
		N1 is N + 1,
		varset__new_var(VarSet0, Var, VarSet1),
		string__int_to_string(N1, Num),
		string__append("HeadVar__", Num, VarName),
		varset__name_var(VarSet1, Var, VarName, VarSet2),
		Vars = [Var | Vars1],
		make_n_fresh_vars_2(N1, Max, VarSet2, Vars1, VarSet)
	).

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
:- mode make_fresh_arg_vars(input, input, output, output).

make_fresh_arg_vars(Args, VarSet0, Vars, VarSet) :-
	make_fresh_arg_vars_2(Args, [], VarSet0, Vars1, VarSet),
	reverse(Vars1, Vars).

:- pred make_fresh_arg_vars_2(list(term), list(var), varset,
				list(var), varset).
:- mode make_fresh_arg_vars_2(input, input, input, output, output).

make_fresh_arg_vars_2([], Vars, VarSet, Vars, VarSet).
make_fresh_arg_vars_2([Arg | Args], Vars0, VarSet0, Vars, VarSet) :-
	( Arg = term_variable(ArgVar), \+ member(ArgVar, Vars0) ->
		Var = ArgVar,
		VarSet1 = VarSet0
	;
		varset__new_var(VarSet0, Var, VarSet1)
	),
	make_fresh_arg_vars_2(Args, [Var | Vars0], VarSet1, Vars, VarSet).

%-----------------------------------------------------------------------------%

:- pred goal_to_conj_list(hlds__goal, list(hlds__goal)).
:- mode goal_to_conj_list(in, out).

goal_to_conj_list(Goal, ConjList) :-
	( Goal = (conj(List) - _) ->
		ConjList = List
	;
		ConjList = [Goal]
	).

%-----------------------------------------------------------------------------%

	% Make implicit quantification explicit.
	% For the rules on implicit quantification, see the
	% file compiler/notes/IMPLICIT_QUANTIFICATION.
	%
	% Rather than making implicit quantification explicit by
	% inserting additional existential quantifiers in the form of
	% `some/2' goals, we instead record existential quantification
	% in the goalinfo for each goal.  In fact we could (should?)
	% even remove any explicit existential quantifiers that were
	% present in the source code, since the information they convey
	% will be stored in the goalinfo, although currently we don't
	% do that.
	% 
	% The important piece of information that later stages of the
	% compilation process want to know is "Does this goal bind any
	% of its non-local variables?".  So, rather than storing a list
	% of the variables which _are_ existentially quantified in the
	% goalinfo, we store the set of variables which are _not_
	% quantified.
	%
	% XXX we ought to rename variables with overlapping scopes
	% caused by explicit quantifiers here (and issue a warning
	% at the same time).

:- pred implicitly_quantify_clause_body(list(var), hlds__goal, hlds__goal).
:- mode implicitly_quantify_clause_body(input, input, output).

implicitly_quantify_clause_body(HeadVars, Goal0, Goal) :-
	set__init(Set0),
	set__insert_list(Set0, HeadVars, Set),
	implicitly_quantify_goal(Goal0, Set, Goal, _).

:- pred implicitly_quantify_goal(hlds__goal, set(var), hlds__goal, set(var)).
:- mode implicitly_quantify_goal(in, in, out, out).

implicitly_quantify_goal(Goal0 - GoalInfo0, OutsideVars,
			Goal - GoalInfo, NonLocalVars) :-
	implicitly_quantify_goal_2(Goal0, OutsideVars, Goal, NonLocalVars),
	goalinfo_set_nonlocals(GoalInfo0, NonLocalVars, GoalInfo).

:- pred implicitly_quantify_goal_2(hlds__goal_expr, set(var),
				   hlds__goal_expr, set(var)).
:- mode implicitly_quantify_goal_2(in, in, out, out).

implicitly_quantify_goal_2(some(Vars, Goal0), OutsideVars,
			   some(Vars, Goal), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals).

/********
	% perhaps we should instead remove explicit quantifiers
	% as follows (and also for not/2):
implicitly_quantify_goal_2(some(Vars, Goal0), OutsideVars, Goal, NonLocals) :-
	set__remove_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal1, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals),
	Goal1 = G - GoalInfo1,
	goalinfo_set_nonlocals(GoalInfo1, NonLocals, GoalInfo).
	Goal = G - GoalInfo.
*********/

implicitly_quantify_goal_2(all(Vars, Goal0), OutsideVars,
			   all(Vars, Goal), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals).

implicitly_quantify_goal_2(conj(List0), OutsideVars,
			   conj(List), NonLocalVars) :-
	implicitly_quantify_conj(List0, OutsideVars, List, NonLocalVars).

implicitly_quantify_goal_2(disj(Goals0), OutsideVars,
			   disj(Goals), NonLocalVars) :-
	implicitly_quantify_disj(Goals0, OutsideVars, Goals, NonLocalVars).

implicitly_quantify_goal_2(not(Vars, Goal0), OutsideVars,
		    not(Vars, Goal), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocals0),
	set__remove_list(NonLocals0, Vars, NonLocals).

implicitly_quantify_goal_2(if_then_else(Vars, A0, B0, C0), OutsideVars,
		if_then_else(Vars, A, B, C), NonLocals) :-
	set__insert_list(OutsideVars, Vars, OutsideVars0),
	goal_vars(B0, VarsB),
	set__union(OutsideVars0, VarsB, OutsideVars1),
	implicitly_quantify_goal(A0, OutsideVars1, A, NonLocalsA),
	set__union(OutsideVars, NonLocalsA, OutsideVars2),
	implicitly_quantify_goal(B0, OutsideVars2, B, NonLocalsB),
	implicitly_quantify_goal(C0, OutsideVars, C, NonLocalsC),
	set__union(NonLocalsA, NonLocalsB, NonLocalsSuccess),
	set__union(NonLocalsSuccess, NonLocalsC, NonLocalsIfThenElse),
	set__intersect(NonLocalsIfThenElse, OutsideVars, NonLocals).

implicitly_quantify_goal_2(call(PredId, ModeId, HeadArgs, Builtin), OutsideVars,
		call(PredId, ModeId, HeadArgs, Builtin), NonLocalVars) :-
	term__vars_list(HeadArgs, HeadVars),
	set__list_to_set(HeadVars, GoalVars),
	set__intersect(GoalVars, OutsideVars, NonLocalVars).

implicitly_quantify_goal_2(unify(A, B, X, Y, Z), OutsideVars,
			unify(A, B, X, Y, Z), NonLocalVars) :-
	term__vars(A, VarsA),
	term__vars(B, VarsB),
	append(VarsA, VarsB, Vars),
	set__list_to_set(Vars, GoalVars),
	set__intersect(GoalVars, OutsideVars, NonLocalVars).

:- pred implicitly_quantify_conj(list(hlds__goal), set(var),
				 list(hlds__goal), set(var)).
:- mode implicitly_quantify_conj(in, in, out, out).

implicitly_quantify_conj(Goals0, OutsideVars, Goals, NonLocalVars) :-
	get_vars(Goals0, FollowingVarsList),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList, OutsideVars,
				Goals, NonLocalVars).

:- pred implicitly_quantify_conj_2(list(hlds__goal), list(set(var)), set(var),
			list(hlds__goal), set(var)).
:- mode implicitly_quantify_conj_2(in, in, in, out, out).

implicitly_quantify_conj_2([], _, _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_conj_2([Goal0 | Goals0],
			[FollowingVars | FollowingVarsList],
			OutsideVars,
			[Goal | Goals], NonLocalVars) :-
	set__union(OutsideVars, FollowingVars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocalVars1),
	set__union(OutsideVars, NonLocalVars1, OutsideVars2),
	implicitly_quantify_conj_2(Goals0, FollowingVarsList, OutsideVars2,
				Goals, NonLocalVars2),
	set__union(NonLocalVars1, NonLocalVars2, NonLocalVarsConj),
	set__intersect(NonLocalVarsConj, OutsideVars, NonLocalVars).

/********** OLD
implicitly_quantify_conj([], _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_conj([Goal0 | Goals0], OutsideVars,
			[Goal | Goals], NonLocalVars) :-
	goal_list_vars(Goals0, FollowingVars),
	set__union(OutsideVars, FollowingVars, OutsideVars1),
	implicitly_quantify_goal(Goal0, OutsideVars1, Goal, NonLocalVars1),
	set__union(OutsideVars, NonLocalVars1, OutsideVars2),
	implicitly_quantify_conj(Goals0, OutsideVars2, Goals, NonLocalVars2),
	set__union(NonLocalVars1, NonLocalVars2, NonLocalVars).

****************/

:- pred implicitly_quantify_disj(list(hlds__goal), set(var),
				 list(hlds__goal), set(var)).
:- mode implicitly_quantify_disj(in, in, out, out).

implicitly_quantify_disj([], _, [], NonLocalVars) :-
	set__init(NonLocalVars).
implicitly_quantify_disj([Goal0 | Goals0], OutsideVars,
			[Goal | Goals], NonLocalVars) :-
	implicitly_quantify_goal(Goal0, OutsideVars, Goal, NonLocalVars0),
	implicitly_quantify_disj(Goals0, OutsideVars, Goals, NonLocalVars1),
	set__union(NonLocalVars0, NonLocalVars1, NonLocalVars).

%-----------------------------------------------------------------------------%

	% Given a list of goals, produce a corresponding list of sets
	% of following variables, where is the set of following variables
	% for each goal is those variables which occur in any of the
	% following goals in the list.

:- pred get_vars(list(hlds__goal), list(set(var))).
:- mode get_vars(in, out) is det.

:- get_vars([], _) when ever.
:- get_vars([_], _) when ever.
:- get_vars([_,_|_], _) when ever.

get_vars([], []).
get_vars([_Goal], [Set]) :-
	set__init(Set).
get_vars([_Goal1, Goal2 | Goals], SetList) :-
	get_vars([Goal2 | Goals], SetList0),
	SetList0 = [Set0 | _],
	goal_vars(Goal2, Set1),
	set__union(Set0, Set1, Set),
	SetList = [Set | SetList0].

	% `goal_list_vars(Goal, Vars)' is true iff 
	% `Vars' is the set of unquantified variables in Goal.

:- pred goal_list_vars(list(hlds__goal), set(var)).
:- mode goal_list_vars(in, out) is det.

goal_list_vars(Goals, S) :-	
	set__init(S0),
	goal_list_vars_2(Goals, S0, S).


:- pred goal_list_vars_2(list(hlds__goal), set(var), set(var)).
:- mode goal_list_vars_2(in, in, out) is det.

goal_list_vars_2([], Set, Set).
goal_list_vars_2([Goal - _GoalInfo| Goals], Set0, Set) :-
	goal_vars_2(Goal, Set0, Set1),
	goal_list_vars_2(Goals, Set1, Set).

:- pred goal_vars(hlds__goal, set(var)).
:- mode goal_vars(in, out) is det.

goal_vars(Goal - _GoalInfo, Set) :-
	set__init(Set0),
	goal_vars_2(Goal, Set0, Set).

:- pred goal_vars_2(hlds__goal_expr, set(var), set(var)).
:- mode goal_vars_2(in, in, out) is det.

goal_vars_2(unify(A, B, _, _, _), Set0, Set) :-
	term__vars(A, VarsA),
	set__insert_list(Set0, VarsA, Set1),
	term__vars(B, VarsB),
	set__insert_list(Set1, VarsB, Set).

goal_vars_2(call(_, _, Args, _), Set0, Set) :-
	term__vars_list(Args, Vars),
	set__insert_list(Set0, Vars, Set).

goal_vars_2(conj(Goals), Set0, Set) :-
	goal_list_vars_2(Goals, Set0, Set).

goal_vars_2(disj(Goals), Set0, Set) :-
	goal_list_vars_2(Goals, Set0, Set).

goal_vars_2(some(Vars, Goal), Set0, Set) :-
	goal_vars(Goal, Set1),
	set__remove_list(Set1, Vars, Set2),
	set__union(Set0, Set2, Set).

goal_vars_2(all(Vars, Goal), Set0, Set) :-
	goal_vars(Goal, Set1),
	set__remove_list(Set1, Vars, Set2),
	set__union(Set0, Set2, Set).

goal_vars_2(not(Vars, Goal), Set0, Set) :-
	goal_vars(Goal, Set1),
	set__remove_list(Set1, Vars, Set2),
	set__union(Set0, Set2, Set).

goal_vars_2(if_then_else(Vars, A, B, C), Set0, Set) :-
		% Let
		%	Set = Set0 + ( (vars(A) + vars(B)) \ Vars ) + vars(C)
		%
		% where `+' is set union and `-' is relative complement.
		%
		% (It's times like this you wish you had a functional
		% notation ;-)
	goal_vars(A, Set1),
	goal_vars(B, Set2),
	set__union(Set1, Set2, Set3),
	set__remove_list(Set3, Vars, Set4),
	set__union(Set0, Set4, Set5),
	goal_vars(C, Set6),
	set__union(Set5, Set6, Set).

%-----------------------------------------------------------------------------%

:- pred unravel_unification(term, term, unify_main_context,
				unify_sub_contexts, varset, hlds__goal, varset).
:- mode unravel_unification(in, in, in, in, in, out, out).

	% `X = Y' needs no unravelling.

unravel_unification(term_variable(X), term_variable(Y), MainContext,
			SubContext, VarSet0, Goal, VarSet) :-
	create_atomic_unification(term_variable(X), term_variable(Y),
			MainContext, SubContext, Goal),
	VarSet0 = VarSet.

	% If we find a unification of the form
	%	X = f(A1, A2, A3)
	% we replace it with
	%	NewVar1 = A1,
	%	NewVar2 = A2,
	%	NewVar3 = A3,
	%	X = f(NewVar1, NewVar2, NewVar3).
	% In the trivial case `X = c', no unravelling occurs.

unravel_unification(term_variable(X), term_functor(F, Args, C), MainContext,
			SubContext, VarSet0, Goal, VarSet) :-
	( Args = [] ->
		create_atomic_unification(term_variable(X),
				term_functor(F, Args, C),
				MainContext, SubContext, Goal),
		VarSet = VarSet0
	;
		make_fresh_arg_vars(Args, VarSet0, HeadVars, VarSet1),
		var_list_to_term_list(HeadVars, HeadArgs),
		create_atomic_unification(term_variable(X),
				term_functor(F, HeadArgs, C),
				MainContext, SubContext, Goal0),
		length(Args, Arity),
		make_functor_cons_id(F, Arity, ConsId),
		ArgContext = functor(ConsId, MainContext, SubContext),
		insert_arg_unifications(HeadVars, Args, ArgContext,
					Goal0, VarSet1, Goal, VarSet)
	).

	% Handle `f(...) = X' in the same way as `X = f(...)'.

unravel_unification(term_functor(F, As, C), term_variable(Y), MC, SC,
			VarSet0, Goal, VarSet) :-
	unravel_unification(term_variable(Y), term_functor(F, As, C), MC, SC,
			VarSet0, Goal, VarSet).

	% If we find a unification of the form `f1(...) = f2(...)',
	% then we replace it with `NewVars1 = ..., NewVars2 = ...,
	% Tmp = f1(NewVars1), Tmp = f2(NewVars2)'. 
	% Note that we can't simplify it yet, because we might simplify
	% away type errors.

unravel_unification(term_functor(LeftF, LeftAs, LeftC),
			term_functor(RightF, RightAs, RightC),
			MainContext, SubContext, VarSet0, Goal, VarSet) :-
	make_fresh_arg_vars(LeftAs, VarSet0, LeftHeadVars, VarSet1),
	make_fresh_arg_vars(RightAs, VarSet1, RightHeadVars, VarSet2),
	var_list_to_term_list(LeftHeadVars, LeftHeadArgs),
	var_list_to_term_list(RightHeadVars, RightHeadArgs),
	length(LeftAs, LeftArity),
	length(RightAs, RightArity),
	make_functor_cons_id(LeftF, LeftArity, LeftConsId),
	make_functor_cons_id(RightF, RightArity, RightConsId),
	LeftArgContext = functor(LeftConsId, MainContext, SubContext),
	RightArgContext = functor(RightConsId, MainContext, SubContext),
	varset__new_var(VarSet2, TmpVar, VarSet3),
	create_atomic_unification(term_variable(TmpVar),
			term_functor(LeftF, LeftHeadArgs, LeftC),
			MainContext, SubContext, Goal0),
	create_atomic_unification(term_variable(TmpVar),
			term_functor(RightF, RightHeadArgs, RightC),
			MainContext, SubContext, Goal1),
	goalinfo_init(GoalInfo),
	Goal2 = conj([Goal0, Goal1]) - GoalInfo,
	insert_arg_unifications(RightHeadVars, RightAs, RightArgContext,
				Goal2, VarSet3, Goal3, VarSet4),
	insert_arg_unifications(LeftHeadVars, LeftAs, LeftArgContext, Goal3,
				VarSet4, Goal, VarSet).

	% create the hlds__goal for a unification which cannot be
	% further simplified, filling in all the as yet
	% unknown slots with dummy values

:- pred create_atomic_unification(term, term, unify_main_context,
				unify_sub_contexts, hlds__goal).
:- mode create_atomic_unification(in, in, in, in, out).

create_atomic_unification(A, B, UnifyMainContext, UnifySubContext, Goal) :-
	Mode = ((free -> free) - (free -> free)),
	UnifyInfo = complicated_unify(Mode, A, B),
	UnifyC = unify_context(UnifyMainContext, UnifySubContext),
	goalinfo_init(GoalInfo),
	Goal = unify(A, B, Mode, UnifyInfo, UnifyC) - GoalInfo.

%-----------------------------------------------------------------------------%

% substitute_vars(Vars0, Subst, Vars)
%	apply substitiution `Subst' (which must only rename vars) to `Vars0',
%	and return the result in `Vars'.

:- pred substitute_vars(list(var), substitution, list(var)).
:- mode substitute_vars(input, input, output) is det.

substitute_vars([], _, []).
substitute_vars([Var0 | Vars0], Subst, [Var | Vars]) :-
	term__apply_substitution(term_variable(Var0), Subst, Term),
	Term = term_variable(Var),
	substitute_vars(Vars0, Subst, Vars).

%-----------------------------------------------------------------------------%

% get_conj(Goal, Conj0, Subst, Conj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list (applying Subst),
%	append Conj0, and return the result in Conj.

:- pred get_conj(goal, substitution, list(hlds__goal), varset,
		list(hlds__goal), varset).
:- mode get_conj(input, input, input, input, output, output).

get_conj(Goal, Subst, Conj0, VarSet0, Conj, VarSet) :-
	(
		%some [A,B]
		Goal = (A,B)
	->
		get_conj(B, Subst, Conj0, VarSet0, Conj1, VarSet1),
		get_conj(A, Subst, Conj1, VarSet1, Conj, VarSet)
	;
		transform_goal(Goal, VarSet0, Subst, Goal1, VarSet),
		Conj = [Goal1 | Conj0]
	).

% get_disj(Goal, Subst, Disj0, Disj) :
% 	Goal is a tree of disjuncts.  Flatten it into a list (applying Subst)
%	append Disj0, and return the result in Disj.

:- pred get_disj(goal, substitution, list(hlds__goal), varset,
		list(hlds__goal), varset).
:- mode get_disj(input, input, input, input, output, output).

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

:- pred duplicate_def_warning(sym_name, int, string, term__context,
				io__state, io__state).
:- mode duplicate_def_warning(input, input, input, input, di, uo).

duplicate_def_warning(Name, Arity, DefType, Context) -->
	prog_out__write_context(Context),
	io__write_string("Warning: duplicate definition for "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'\n").

:- pred multiple_def_error(sym_name, int, string, term__context,
				io__state, io__state).
:- mode multiple_def_error(input, input, input, input, di, uo).

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
:- mode undefined_pred_error(input, input, input, input, di, uo).

undefined_pred_error(Name, Arity, Context, Description) -->
	prog_out__write_context(Context),
	io__write_string("Error: "),
	io__write_string(Description),
	io__write_string(" for `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' without preceding pred declaration\n").

%-----------------------------------------------------------------------------%
