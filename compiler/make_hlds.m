%-----------------------------------------------------------------------------%

% File: make_hlds.nl.
% Main author: fjh.

% This module converts from the parse tree structure which is
% read in by prog_io.nl, into the simplified high level data structure
% defined in hlds.nl.  In the parse tree, the program is represented
% as a list of items; we insert each item into the appropriate symbol
% table, and report any duplicate definition errors.  We also
% transform clause bodies from (A,B,C) into conj([A,B,C]) form.

:- module make_hlds.
:- interface.
:- import_module prog_io, hlds.

:- pred parse_tree_to_hlds(program, module_info, io__state, io__state).
:- mode parse_tree_to_hlds(input, output, di, uo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_out, require.

parse_tree_to_hlds(module(Name, Items), Module) -->
	{ moduleinfo_init(Name, Module0) },
	add_item_list_decls(Items, Module0, Module1),
	%%% { report_stats },
	add_item_list_clauses(Items, Module1, Module2),
	{ moduleinfo_predids(Module2, RevPredIds),
	  reverse(RevPredIds, PredIds),
	  moduleinfo_set_predids(Module2, PredIds, Module) }.

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
		io__write_string("internal error in make_hlds.\n"),
		io__write_string("Failed to process the following item:\n"),
		io__write_anything(Item),
		io__write_string("\n"),
		io__set_output_stream(OldStream, _)
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
		io__write_string("internal error in make_hlds.\n"),
		io__write_string("Failed to process the following clause:\n"),
		io__write_anything(Item),
		io__write_string("\n"),
		io__set_output_stream(OldStream, _)
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

:- pred insts_add(inst_table, varset, inst_defn, condition, term__context,
			inst_table, io__state, io__state).
:- mode insts_add(input, input, input, input, input, output, di, uo).
insts_add(Insts0, VarSet, inst_defn(Name, Args, Body), Cond, Context, Insts) -->
	{ length(Args, Arity),
	  I = hlds__inst_defn(VarSet, Args, Body, Cond, Context) },
	(if %%% some [I2]		% NU-Prolog inconsistency
		{ map__search(Insts0, Name - Arity, I2) }
	then
		{ Insts = Insts0 },
		(if
			{ inst_is_compat(I, I2) }
		then
			duplicate_def_warning(Name, Arity, "inst", Context)
		else
			multiple_def_error(Name, Arity, "inst", Context)
		)
	else
		{ map__insert(Insts0, Name - Arity, I, Insts) }
	).

	% Two different inst definitions are compatible if
	% their mode parameters and their bodies are identical.
	% (This is perhaps more strict than it need be.)

:- pred inst_is_compat(hlds__inst_defn, hlds__inst_defn).
:- mode inst_is_compat(input, input).

inst_is_compat(hlds__inst_defn(_, Args, Body, _, _),
		hlds__inst_defn(_, Args, Body, _, _)).

	% XXX should be in hlds.nl.

:- pred make_predid(string, sym_name, int, pred_id).
:- mode make_predid(input, input, input, output).

make_predid(ModName, unqualified(Name), Arity, pred(ModName, Name, Arity)).
make_predid(_, qualified(ModName, Name), Arity, pred(ModName, Name, Arity)).

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

modes_add(Modes0, VarSet, mode_defn(Name, Args, Body), Cond, Context, Modes) -->
	{ length(Args, Arity),
	  I = hlds__mode_defn(VarSet, Args, Body, Cond, Context) },
	(if %%% some [I2]		% NU-Prolog inconsistency
		{ map__search(Modes0, Name - Arity, I2) }
	then
		{ Modes = Modes0 },
		(if { mode_is_compat(I, I2) } then
			duplicate_def_warning(Name, Arity, "mode", Context)
		else
			multiple_def_error(Name, Arity, "mode", Context)
		)
	else
		{ map__insert(Modes0, Name - Arity, I, Modes) }
	).

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
		{ \+ (Body_2 = abstract_type) }
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
		  map__set(Types0, TypeId, T, Types),
		  (if some [ConsList]
			Body = du_type(ConsList)
		  then
			moduleinfo_ctors(Module0, Ctors0),
			ctors_add(ConsList, TypeId, Context, Ctors0, Ctors),
			moduleinfo_set_ctors(Module0, Ctors, Module1)
		  else
			Module1 = Module0
		  ),
		  moduleinfo_set_types(Module1, Types, Module)
		},
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
			cons_table).
:- mode ctors_add(input, input, input, input, output).

ctors_add([], _TypeId, _Context, Ctors, Ctors).
ctors_add([Name - Args | Rest], TypeId, Context, Ctors0, Ctors) :-
	make_cons_id(Name, Args, TypeId, ConsId),
	ConsDefn = hlds__cons_defn(Args, TypeId, Context),
	(if some [ConsDefns0]
		map__search(Ctors0, ConsId, ConsDefns0)
	then
		ConsDefns1 = ConsDefns0
	else
		ConsDefns1 = []
	),
	map__set(Ctors0, ConsId, [ConsDefn | ConsDefns1], Ctors1),
	ctors_add(Rest, TypeId, Context, Ctors1, Ctors).

:- pred make_cons_id(sym_name, list(type), type_id, cons_id).
:- mode make_cons_id(input, input, input, output).

make_cons_id(qualified(_Module, Name), Args, _TypeId, cons(Name, Arity)) :-
	length(Args, Arity).
make_cons_id(unqualified(Name), Args, _TypeId, cons(Name, Arity)) :-
	length(Args, Arity).

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
	(if %%% some [Modes]
		{ MaybeModes = yes(Modes) }
	then
		module_add_mode(Module1, VarSet, PredName, Modes, Det, Cond,
			Context, Module)
	else
		{ Module = Module1 }
	).


% A pred declaration may contains just types, as in
%	:- pred append(list(T), list(T), list(T)).
% or it may contain both types and modes, as in
%	:- pred append(list(T)::input, list(T)::input, list(T)::output).
%
% This predicate takes the argument list of a pred declaration, splits
% it into two separate lists for the types and (if present) the modes.

:- type maybe_modes ---> yes(list(mode)) ; no.
:- pred split_types_and_modes(list(type_and_mode), list(type), maybe_modes).
:- mode split_types_and_modes(input, output, output).

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
	split_types_and_modes_2(TypesAndModes, no, Types, Modes, Result),
	(if Result = yes then
		MaybeModes = yes(Modes)
	else
		MaybeModes = no
	).

:- type maybe ---> yes ; no.
:- pred split_types_and_modes_2(list(type_and_mode), maybe,
				list(type), list(mode), maybe).
:- mode split_types_and_modes_2(input, input, output, output, output).

	% T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
	split_type_and_mode(TM, Result0, T, M, Result1),
	split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the mode for the
	% other arguments defaults to free->free.
	% - should this be an error instead?

:- pred split_type_and_mode(type_and_mode, maybe, type, mode, maybe).
:- mode split_type_and_mode(input, input, output, output, output).

split_type_and_mode(type_only(T), _, T, (free -> free), yes).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

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
	  P = predicate(VarSet, Types, Cond, [], Procs, Context) },
	(if %%% some [P2]
		{ map__search(Preds0, PredId, P2) }
	then
		{ Module = Module0 },
		(if 
			{ pred_is_compat(P, P2) }
		then
			duplicate_def_warning(Name, Arity, "pred", Context)
		else
			multiple_def_error(Name, Arity, "pred", Context)
		)
	else
		{ map__insert(Preds0, PredId, P, Preds) },
		{ moduleinfo_set_preds(Module0, Preds, Module1) },
		{ moduleinfo_predids(Module1, PredIds0) },
		{ moduleinfo_set_predids(Module1, [PredId | PredIds0], Module) }
	).

:- pred pred_is_compat(pred_info, pred_info).
:- mode pred_is_compat(input, input).

pred_is_compat(predicate(_, Types, _, _, _, _),
	       predicate(_, Types, _, _, _, _)).

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
pred_modes_add(Preds0, ModuleName, _VarSet, PredName, Modes, Det, _Cond,
		MContext, Preds) -->
	{ length(Modes, Arity),
	  make_predid(ModuleName, PredName, Arity, PredId) },
	(if %%% some [P0]
		{ map__search(Preds0, PredId, P0) }
	then
		{ P0 = predicate(TVarSet, ArgTypes, TCond, Clauses, Procs0,
			TContext) },
			% XXX we should check that this mode declaration
			% isn't the same as an existing one
		
			% some parts of the procedure aren't known yet
			% we initialize them to any old garbage which
			% we will later throw away
		{ next_mode_id(Procs0, ModeId) },
		{ map__init(BodyTypes) },
		{ goalinfo_init(GoalInfo) },
		{ varset__init(BodyVarSet) },
		{ HeadVars = [] },
		{ determinism_to_category(Det, Category) },
		{ map__init(CallInfo) },
		{ NewProc = procedure(Category, BodyVarSet, BodyTypes,
			HeadVars, Modes, conj([]) - GoalInfo, MContext,
			CallInfo) },
		{ map__insert(Procs0, ModeId, NewProc, Procs) },
		{ P = predicate(TVarSet, ArgTypes, TCond, Clauses, Procs,
			TContext) },
		{ map__set(Preds0, PredId, P, Preds) }
	else
		undefined_pred_error(PredName, Arity, MContext,	
			"mode declaration"),
		{ Preds = Preds0 }
	).

:- pred determinism_to_category(determinism, category).
:- mode determinism_to_category(input, output).
determinism_to_category(det, deterministic(declared)).
determinism_to_category(semidet, deterministic(declared)).
determinism_to_category(nondet, deterministic(declared)).
determinism_to_category(unspecified, unspecified).

	% XXX efficiency could be improved
	% we should probably store the next available ModeId rather
	% than recomputing it all the time

:- pred next_mode_id(proc_table, pred_mode_id).
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
	(if %%% some [PredInfo0]
		{ map__search(Preds0, PredId, PredInfo0) }
	then
			% XXX abstract predicate/4
		{ PredInfo0 = predicate(TVarSet, Types, Cond, Clauses0, Procs,
				TContext),
		  map__keys(Procs, ModeIds),
		  transform(VarSet, Args, Body, NewVarSet, HeadVars, Goal),
		  map__init(VarTypes),
		  % XXX we should avoid append - this gives O(N*N)
		  append(Clauses0, [clause(ModeIds, NewVarSet, VarTypes,
				HeadVars, Goal, Context)], Clauses),
		  PredInfo = predicate(TVarSet, Types, Cond, Clauses, Procs,
				TContext),
		  map__set(Preds0, PredId, PredInfo, Preds) }
	else
		undefined_pred_error(PredName, Arity, Context, "clause"),
		{ Preds = Preds0 }
	).

:- pred transform(varset, list(term), goal, varset, list(var), hlds__goal).
:- mode transform(input, input, input, output, output, output).

transform(VarSet0, Args, Body, VarSet, HeadVars, Goal) :-
	length(Args, NumArgs),
	make_n_fresh_vars(NumArgs, VarSet0, HeadVars, VarSet),
	insert_head_unifications(Args, HeadVars, Body, Body2),
	transform_goal(Body2, Goal).

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

:- pred insert_head_unifications(list(term), list(var), goal, goal).
:- mode insert_head_unifications(input, input, input, output).

insert_head_unifications([], [], Body, Body).
insert_head_unifications([Arg|Args], [Var|Vars], Body0, Body) :-
	Body = (unify(Arg, term_variable(Var)), Body1),
	insert_head_unifications(Args, Vars, Body0, Body1).

:- pred transform_goal(goal, hlds__goal).
:- mode transform_goal(input, output).

transform_goal(fail, conj([]) - GoalInfo) :-
	goalinfo_init(GoalInfo).

transform_goal(true, disj([]) - GoalInfo) :-
	goalinfo_init(GoalInfo).

transform_goal(some(Vars, Goal0), some(Vars, Goal) - GoalInfo) :-
	transform_goal(Goal0, Goal),
	goalinfo_init(GoalInfo).

transform_goal(all(Vars, Goal0), all(Vars, Goal) - GoalInfo) :-
	transform_goal(Goal0, Goal),
	goalinfo_init(GoalInfo).

transform_goal(if_then_else(Vars, A0, B0, C0),
		if_then_else(Vars, A, B, C) - GoalInfo) :-
	transform_goal(A0, A),
	transform_goal(B0, B),
	transform_goal(C0, C),
	goalinfo_init(GoalInfo).

transform_goal(if_then(Vars, A0, B0),
		if_then_else(Vars, A, B, C) - GoalInfo) :-
	transform_goal(A0, A),
	transform_goal(B0, B),
	transform_goal(fail, C),
	goalinfo_init(GoalInfo).

transform_goal(not(Vars, A0), not(Vars, A) - GoalInfo) :-
	transform_goal(A0, A),
	goalinfo_init(GoalInfo).

transform_goal((A0,B0), conj(L) - GoalInfo) :-
	get_conj(B0, [], L0),
	get_conj(A0, L0, L),
	goalinfo_init(GoalInfo).

transform_goal((A0;B0), disj(L) - GoalInfo) :-
	get_disj(B0, [], L0),
	get_disj(A0, L0, L),
	goalinfo_init(GoalInfo).

transform_goal(call(Goal), call(PredId, ModeId, Args, Builtin) - GoalInfo) :-
	% fill unused slots with any old junk 
	ModeId = 0,
	Builtin = not_builtin,

	% XXX serious design flaw
	% XXX we need to know the module name!!!
	ModuleName = "xxx",

	( Goal = term_functor(term_atom(PredName), Args, _) ->
		true
	;
		error("fatal error: called term is not an atom")
	),
	length(Args, Arity),
	make_predid(ModuleName, unqualified(PredName), Arity, PredId),

	goalinfo_init(GoalInfo).

transform_goal(unify(A, B), unify(A, B, Mode, UnifyInfo) - GoalInfo) :-
	goalinfo_init(GoalInfo),
		% fill in unused slots with garbage values
	Mode = ((free -> free) - (free -> free)),
	UnifyInfo = complicated_unify(Mode, A, B).

% get_conj(Goal, Conj0, Conj) :
% 	Goal is a tree of conjuncts.  Flatten it into a list,
%	append Conj0, and return the result in Conj.

:- pred get_conj(goal, list(hlds__goal), list(hlds__goal)).
:- mode get_conj(input, input, output).

get_conj(Goal, Conj0, Conj) :-
	(if some [A,B]
		Goal = (A,B)
	then
		get_conj(B, Conj0, Conj1),
		get_conj(A, Conj1, Conj)
	else
		transform_goal(Goal, Goal1),
		Conj = [Goal1 | Conj0]
	).

% get_disj(Goal, Disj0, Disj) :
% 	Goal is a tree of disjuncts.  Flatten it into a list,
%	append Disj0, and return the result in Disj.

:- pred get_disj(goal, list(hlds__goal), list(hlds__goal)).
:- mode get_disj(input, input, output).

get_disj(Goal, Disj0, Disj) :-
	(if some [A,B]
		Goal = (A;B)
	then
		get_disj(B, Disj0, Disj1),
		get_disj(A, Disj1, Disj)
	else
		transform_goal(Goal, Goal1),
		Disj = [Goal1 | Disj0]
	).

%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred duplicate_def_warning(sym_name, int, string, term__context,
				io__state, io__state).
:- mode duplicate_def_warning(input, input, input, input, di, uo).

duplicate_def_warning(Name, Arity, DefType, Context) -->
	prog_out__write_context(Context),
	io__write_string("warning: duplicate definition for "),
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
	io__write_string("error: "),
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
	io__write_string("error: "),
	io__write_string(Description),
	io__write_string(" for `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' without preceding pred declaration\n").

%-----------------------------------------------------------------------------%
