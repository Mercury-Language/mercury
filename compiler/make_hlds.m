%-----------------------------------------------------------------------------%
:- module make_hlds.
:- import_module prog_io, hlds.

% This module converts from the parse tree structure which is
% read in by prog_io.nl, into the simplified high level data structure
% defined in hlds.nl.  In the parse tree, the program is represented
% as a list of items; we insert each item into the appropriate symbol
% table, and report any duplicate definition errors.  We also
% transform clause bodies from (A,B,C) into conj([A,B,C]) form.

%-----------------------------------------------------------------------------%
:- pred parse_tree_to_hlds(program, module_info).
:- mode parse_tree_to_hlds(input, output).

parse_tree_to_hlds(module(Name, Items),  Module) -->
	{ moduleinfo_init(Name, Module0) },
	% XXX this is commmented out until it works
	%%%% add_item_list_decls(Items, Module0, Module1),
	%%%% add_item_list_clauses(Items, Module1, Module).
	% instead we just use this stub
	add_item_list_decls(Items, Module0, Module).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

	% add the declarations one by one to the module

add_item_list_decls([], Module, Module) --> [].
add_item_list_decls([Item|Items], Module0, Module) -->
	add_item_decl(Item, Module0, Module1),
	add_item_list_decls(Items, Module1, Module).

	% add the clauses one by one to the module

add_item_list_clauses([], Module, Module) --> [].
add_item_list_clauses([Item|Items], Module0, Module) -->
	add_item_clause(Item, Module0, Module1),
	add_item_list_clauses(Items, Module1, Module).

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

add_item_decl(clause(_, _, _, _), Module, Module) --> [].	% skip clauses

add_item_decl(type_defn(VarSet, TypeDefn, Cond), Module0, Module) -->
	module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Module).

add_item_decl(inst_defn(VarSet, InstDefn, Cond), Module0, Module) -->
	module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Module).

add_item_decl(mode_defn(VarSet, ModeDefn, Cond), Module0, Module) -->
	module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Module).

add_item_decl(pred(VarSet, PredName, TypesAndModes, Cond), Module0, Module) -->
	module_add_pred(Module0, VarSet, PredName, TypesAndModes, Cond, Module).

add_item_decl(mode(VarSet, PredName, Modes, Cond), Module0, Module) -->
	module_add_mode(Module0, VarSet, PredName, Modes, Cond, Module).

add_item_decl(module_defn(_VarSet, _ModuleDefn), Module, Module) -->
	io__write_string("warning: module declarations not yet implemented\n").

%-----------------------------------------------------------------------------%

	% dispatch on the different types of items

add_item_clause(clause(VarSet, PredName, Args, Body), Module0, Module) -->
	module_add_clause(Module0, VarSet, PredName, Args, Body, Module).
add_item_clause(type_defn(_, _, _), Module, Module) --> [].
add_item_clause(inst_defn(_, _, _), Module, Module) --> [].
add_item_clause(mode_defn(_, _, _), Module, Module) --> [].
add_item_clause(pred(_, _, _, _), Module, Module) --> [].
add_item_clause(mode(_, _, _, _), Module, Module) --> [].
add_item_clause(module_defn(_, _), Module, Module) --> [].

%-----------------------------------------------------------------------------%

module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Module) -->
	{ moduleinfo_insts(Module0, Insts0) },
	insts_add(Insts0, VarSet, InstDefn, Cond, Insts),
	{ moduleinfo_set_insts(Module0, Insts, Module) }.

insts_add(Insts0, VarSet, inst_defn(Name, Args, Body), Cond, Insts) -->
	{ length(Args, Arity),
	  I = hlds__inst_defn(VarSet, Args, Body, Cond) },
	(if %%% some [I2]		% NU-Prolog inconsistency
		{ map__search(Insts0, Name - Arity, I2) }
	then
		{ Insts = Insts0 },
		(if
			{ inst_is_compat(I, I2) }
		then
			duplicate_def_warning(Name, Arity, "inst")
		else
			multiple_def_error(Name, Arity, "inst")
		)
	else
		{ map__insert(Insts0, Name - Arity, I, Insts) }
	).

inst_is_compat(I1, I2) :-
	I1 = I2.

	% XXX should be in hlds.nl.
make_predid(ModName, unqualified(Name), Arity, pred(ModName, Name, Arity)).
make_predid(_, qualified(ModName, Name), Arity, pred(ModName, Name, Arity)).

%-----------------------------------------------------------------------------%

module_add_mode_defn(Module0, VarSet, TypeDefn, Cond, Module) -->
	{ moduleinfo_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, ModeDefn, Cond, Modes),
	{ moduleinfo_set_modes(Module0, Modes, Module) }.

modes_add(Modes0, VarSet, mode_defn(Name, Args, Body), Cond, Modes) -->
	{ length(Args, Arity),
	  I = hlds__mode_defn(VarSet, Args, Body, Cond) },
	(if %%% some [I2]		% NU-Prolog inconsistency
		{ map__search(Modes0, Name - Arity, I2) }
	then
		{ Modes = Modes0 },
		(if mode_is_compat(I, I2) then
			duplicate_def_warning(Name, Arity, "mode")
		else
			multiple_def_error(Name, Arity, "mode")
		)
	else
		{ map__insert(Modes0, Name - Arity, I, Modes) }
	).

mode_is_compat(M1, M2) :-
	M1 = M2.

%-----------------------------------------------------------------------------%

module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Module) -->
	{ moduleinfo_types(Module0, Types0) },
	types_add(Types0, VarSet, TypeDefn, Cond, Types),
	{ moduleinfo_set_types(Module0, Types, Module) }.

type_name_args(du_type(Name, Args, Body), Name, Args, du_type(Body)).
type_name_args(uu_type(Name, Args, Body), Name, Args, uu_type(Body)).
type_name_args(eqv_type(Name, Args, Body), Name, Args, eqv_type(Body)).

types_add(Types0, VarSet, TypeDefn, Cond, Types) -->
	{ type_name_args(TypeDefn, Name, Args, Body),
	  length(Args, Arity),
	  I = hlds__type_defn(VarSet, Args, Body, Cond) },
	(if %%% some [I2]
		{ map__search(Types0, Name - Arity, I2) }
	then
		{ Types = Types0 },
		(if 
			{ type_is_compat(I, I2) }
		then
			duplicate_def_warning(Name, Arity, "type")
		else
			multiple_def_error(Name, Arity, "type")
		)
	else
		{ map__insert(Types0, Name - Arity, I, Types) }
	).

type_is_compat(T1, T2) :-
	T1 = T2.

%-----------------------------------------------------------------------------%

module_add_pred(Module0, VarSet, PredName, TypesAndModes, Cond, Module) -->
	{ moduleinfo_preds(Module0, Preds0) },
	{ split_types_and_modes(TypesAndModes, Types, MaybeModes) },
	{ moduleinfo_name(Module0, ModuleName) },
	preds_add(Preds0, ModuleName, VarSet, PredName, Types, Cond, Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module1) },
	(if %%% some [Modes]
		{ MaybeModes = yes(Modes) }
	then
		module_add_mode(Module1, VarSet, PredName, Modes, Cond, Module)
	else
		{ Module = Module1 }
	).

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
	split_types_and_modes_2(TypesAndModes, Types, Modes, no, R),
	(if R = yes then
		MaybeModes = yes(Modes)
	else
		MaybeModes = no
	).

split_types_and_modes_2([], [], [], R, R).
split_types_and_modes_2([TM|TMs], [T|Ts], [M|Ms], R0, R) :-
	split_type_and_mode(TM, T, M),
	split_types_and_modes_2(TMs, Ts, Ms, R0, R).

	% if a pred declaration specifies modes for some but
	% not all of the arguments, then the mode for the
	% other arguments defaults to free->free.
	% - should this be an error instead?

split_type_and_mode(type_only(T), T, free -> free, _, yes).
split_type_and_mode(type_and_mode(T,M), T, M, R, R).

preds_add(Preds0, ModNm, VarSet, Name, Types, Cond, Preds) -->
	{ length(Types, Arity),
	  map__init(Procs),
	  make_predid(ModNm, Name, Arity, PredId) },
	  I = predicate(VarSet, Types, Cond, [], Procs) },
	(if %%% some [I2]
		{ map__search(Preds0, PredId, I2) }
	then
		{ Types = Types0 },
		(if 
			{ pred_is_compat(I, I2) }
		then
			duplicate_def_warning(Name, Arity, "pred")
		else
			multiple_def_error(Name, Arity, "pred")
		)
	else
		{ map__insert(Preds0, PredId, I, Preds) }
	).

pred_is_compat(predicate(VarSet, Types, Cond, _, _),
	       predicate(VarSet, Types, Cond, _, _)).

%-----------------------------------------------------------------------------%

module_add_mode(Module0, VarSet, PredName, Modes, Cond, Module) -->
	{ moduleinfo_preds(Module0, Preds0) },
	{ moduleinfo_name(Module0, ModuleName) },
	pred_modes_add(Preds0, VarSet, PredName, Modes, Cond, Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module) }.

pred_modes_add(Preds0, ModuleName, VarSet, PredName, Modes, Cond, Preds) --->
	{ length(Modes, Arity),
	  make_predid(ModuleName, PredName, Arity, PredId) },
	(if %%% some [P0]
		{ map__search(Preds0, PredId, P0) }
	then
		{ P0 = predicate(VarSet, ArgTypes, Cond, Clauses, Procs0) },
			% XXX we should check that this mode declaration
			% isn't the same as an existing one
		
			% some parts of the procedure aren't known yet
			% we initialize them to any old garbage which
			% we will later throw away
		{ next_mode_id(Procs0, ModeId) },
		{ map__init(BodyTypes) },
		{ map__init(GoalInfo1) },
		{ GoalInfo = goal_info(GoalInfo1) },
		{ varset__init(BodyVarSet) },
		{ HeadVars = [] },
		{ NewProc = procedure(nondeterministic, BodyVarSet,
			BodyTypes, HeadVars, Modes, conj([]) - GoalInfo) },
		{ map__insert(Procs0, ModeId, NewProc, Procs) }
		{ P = predicate(VarSet, ArgTypes, Cond, Clauses, Procs) },
		{ map__set(Preds0, PredId, P) }
	else
		undefined_pred_error(PredName, Arity),
		Preds = Preds0
	).

	% XXX efficiency could be improved
	% we should probably store the next available ModeId rather
	% than recomputing it all the time
next_mode_id(Procs, ModeId) :-
	map__to_assoc_list(Procs, List),
	length(List, ModeId).

%-----------------------------------------------------------------------------%

module_add_clause(Module0, VarSet, PredName, Args, Body, Module) -->
	{ moduleinfo_preds(Module0, Preds0) },
	{ moduleinfo_name(Module0, ModuleName) },
	clauses_add(Preds0, ModuleName, VarSet, PredName, Args, Body, Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module) }.

clauses_add(Preds0, ModuleName, VarSet, PredName, Args, Body, Preds) -->
	{ length(Args, Arity) },
	{ make_predid(ModuleName, PredName, Arity, PredId) },
	(if %%% some [PredInfo0]
		{ map__search(Preds0, PredId, PredInfo0) }
	then
			% XXX abstract predicate/4
		{ PredInfo0 = predicate(TVarSet, Types, Cond, Clauses0, Procs),
		  map__keys(Procs, ModeIds),
		  transform(VarSet, Args, Body, NewVarSet, HeadVars, Goal),
		  map__init(VarTypes),
		  Clauses = [clause(ModeIds, NewVarSet, VarTypes,
				HeadVars, Goal) | Clauses0]
		  PredInfo = predicate(TVarSet, Type, Cond, Clauses, Procs),
		  map__set(Preds0, PredId, PredInfo, Preds) }
	else
		undefined_pred_error(PredName, Arity, "clause")
	).

transform(VarSet0, Args, Body, VarSet, HeadVars, Goal) :-
	length(Args, NumArgs),
	make_n_fresh_vars(NumArgs, VarSet0, HeadVars, VarSet),
	insert_head_unifications(Args, HeadVars, Body, Body2),
	transform_goal(Body2, Goal).

make_n_fresh_vars(N, VarSet0, Vars, VarSet) :-
	(if N = 0 then
		VarSet = VarSet0,
		Vars = []
	else
		N1 is N - 1,
		varset__new_var(VarSet0, Var, VarSet1),
		Vars = [Var | Vars1],
		make_n_fresh_vars(N1, VarSet1, Vars1, VarSet)
	).

insert_head_unifications([], [], Body, Body).
insert_head_unifications([Arg|Args], [Var|Vars], Body0, Body) :-
	Body = (unify(Arg, Var), Body1),
	insert_head_unifications(Args, Vars, Body0, Body1).

:- type goal		--->	(goal,goal)
			;	fail	
					% could use conj(goals) instead 
			;	{goal;goal}	% {...} quotes ';'/2.
			;	true	
					% could use disj(goals) instead
			;	not(vars,goal)
			;	some(vars,goal)
			;	all(vars,goal)
			;	if_then(vars,goal,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	call(term)
			;	unify(term, term).


	% XXX unfinished
transform_goal(fail, conj([]) - GoalInfo) :-
	goalinfo_init(GoalInfo).

transform_goal(true, disj([]) - GoalInfo) :-
	goalinfo_init(GoalInfo).

transform_goal(some(Vars, Goal), some(Vars, Goal) - GoalInfo) :-
	goalinfo_init(GoalInfo).

transform_goal(all(Vars, Goal), all(Vars, Goal) - GoalInfo) :-
	goalinfo_init(GoalInfo).

transform_goal((A0,B0), conj(L) - GoalInfo) :-
	get_conj(A0, [], L0),
	get_conj(B0, L0, L),
	goalinfo_init(GoalInfo).

transform_goal((A0;B0), disj(L) - GoalInfo) :-
	get_disj(A0, [], L0),
	get_disj(B0, L0, L),
	goalinfo_init(GoalInfo).

%-----------------------------------------------------------------------------%

duplicate_def_warning(Name, Arity, DefType) -->
	io__write_string("warning: duplicate definition for "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'\n").

multiple_def_error(Name, Arity, DefType) -->
	io__write_string("error: "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' multiply defined\n").

undefined_pred_error(Name, Arity, Description) -->
	io__write_string("error: "),
	io__write_string(Description),
	io__write_string(" for `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' without preceding pred declaration\n").

%-----------------------------------------------------------------------------%
