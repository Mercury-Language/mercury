%-----------------------------------------------------------------------------%
:- module make_hlds.
:- import_module prog_io, hlds.

% Main author: fjh.

% This module converts from the parse tree structure which is
% read in by prog_io.nl, into the simplified high level data structure
% defined in hlds.nl.  In the parse tree, the program is represented
% as a list of items; we insert each item into the appropriate symbol
% table, and report any duplicate definition errors.  We also
% transform clause bodies from (A,B,C) into conj([A,B,C]) form.

%-----------------------------------------------------------------------------%
:- pred parse_tree_to_hlds(program, module_info, io__state, io__state).
:- mode parse_tree_to_hlds(input, output, di, uo).

parse_tree_to_hlds(module(Name, Items), Module) -->
	{ moduleinfo_init(Name, Module0) },
	% XXX this is commmented out until it works
	add_item_list_decls(Items, Module0, Module1),
	{ statistics },
	add_item_list_clauses(Items, Module1, Module).
	% instead we just use this stub
	%%%% add_item_list_decls(Items, Module0, Module).

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%

	% add the declarations one by one to the module

add_item_list_decls([], Module, Module) --> [].
add_item_list_decls([Item|Items], Module0, Module) -->
	(add_item_decl(Item, Module0, Module1) ->
		{ true }
	;
		{ write('failed: '),
		  write(Item),
		  nl
		}
	),
	add_item_list_decls(Items, Module1, Module).

	% add the clauses one by one to the module

add_item_list_clauses([], Module, Module) --> [].
add_item_list_clauses([Item|Items], Module0, Module) -->
	(add_item_clause(Item, Module0, Module1) ->
		{ true }
	;
		{ write('failed: '),
		  write(Item),
		  nl
		}
	),
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


:- pred module_add_inst_defn(module_info, varset, inst_defn, condition,
				module_info, io__state, io__state).
:- mode module_add_inst_defn(input, input, input, input, output, di, uo).

module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Module) -->
	{ moduleinfo_insts(Module0, Insts0) },
	insts_add(Insts0, VarSet, InstDefn, Cond, Insts),
	{ moduleinfo_set_insts(Module0, Insts, Module) }.

:- pred insts_add(inst_table, varset, hlds__inst_defn, condition, inst_table,
			io__state, io__state).
:- mode insts_add(input, input, input, input, output, di, uo).
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

:- pred inst_is_compat(hlds__inst_defn, hlds__inst_defn).
:- mode inst_is_compat(input, input).

inst_is_compat(I1, I2) :-
	I1 = I2.

	% XXX should be in hlds.nl.

:- pred make_predid(string, sym_name, int, pred_id).
:- mode make_predid(input, input, input, output).

make_predid(ModName, unqualified(Name), Arity, pred(ModName, Name, Arity)).
make_predid(_, qualified(ModName, Name), Arity, pred(ModName, Name, Arity)).

%-----------------------------------------------------------------------------%

:- pred module_add_mode_defn(module_info, varset, mode_defn, condition,
				module_info, io__state, io__state).
:- mode module_add_mode_defn(input, input, input, input, output, di, uo).

module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Module) -->
	{ moduleinfo_modes(Module0, Modes0) },
	modes_add(Modes0, VarSet, ModeDefn, Cond, Modes),
	{ moduleinfo_set_modes(Module0, Modes, Module) }.

:- pred modes_add(mode_table, varset, hlds__mode_defn, condition, mode_table,
			io__state, io__state).
:- mode modes_add(input, input, input, input, output, di, uo).

modes_add(Modes0, VarSet, mode_defn(Name, Args, Body), Cond, Modes) -->
	{ length(Args, Arity),
	  I = hlds__mode_defn(VarSet, Args, Body, Cond) },
	(if %%% some [I2]		% NU-Prolog inconsistency
		{ map__search(Modes0, Name - Arity, I2) }
	then
		{ Modes = Modes0 },
		(if { mode_is_compat(I, I2) } then
			duplicate_def_warning(Name, Arity, "mode")
		else
			multiple_def_error(Name, Arity, "mode")
		)
	else
		{ map__insert(Modes0, Name - Arity, I, Modes) }
	).

:- pred mode_is_compat(hlds__mode_defn, hlds__mode_defn).
:- mode mode_is_compat(input, input).

mode_is_compat(M1, M2) :-
	M1 = M2.

%-----------------------------------------------------------------------------%

:- pred module_add_type_defn(module_info, varset, hlds__type_defn, condition,
				module_info, io__state, io__state).
:- mode module_add_type_defn(input, input, input, input, output, di, uo).
module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Module) -->
	{ moduleinfo_types(Module0, Types0) },
	types_add(Types0, VarSet, TypeDefn, Cond, Types),
	{ moduleinfo_set_types(Module0, Types, Module) }.

:- pred type_name_args(type_defn, sym_name, list(type_param), hlds__type_body).
:- mode type_name_args(input, output, output, output).

type_name_args(du_type(Name, Args, Body), Name, Args, du_type(Body)).
type_name_args(uu_type(Name, Args, Body), Name, Args, uu_type(Body)).
type_name_args(eqv_type(Name, Args, Body), Name, Args, eqv_type(Body)).

:- pred types_add(type_table, varset, type_defn, condition, type_table,
		io__state, io__state).
:- mode types_add(input, input, input, input, output, di, uo).

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

:- pred type_is_compat(hlds__type_defn, hlds__type_defn).
:- mode type_is_compat(input, input).

type_is_compat(T1, T2) :-
	T1 = T2.

%-----------------------------------------------------------------------------%

:- pred module_add_pred(module_info, varset, sym_name, list(type_and_mode),
			condition, module_info, io__state, io__state).
:- mode module_add_pred(input, input, input, input, input, output, di, uo).

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

:- pred preds_add(pred_table, module_name, varset, sym_name, list(type),
		condition, pred_table, io__state, io__state).
:- mode preds_add(input, input, input, input, input, input, output, di, uo).

preds_add(Preds0, ModNm, VarSet, Name, Types, Cond, Preds) -->
	{ length(Types, Arity),
	  map__init(Procs),
	  make_predid(ModNm, Name, Arity, PredId),
	  I = predicate(VarSet, Types, Cond, [], Procs) },
	(if %%% some [I2]
		{ map__search(Preds0, PredId, I2) }
	then
		{ Preds = Preds0 },
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

:- pred pred_is_compat(pred_info, pred_info).
:- mode pred_is_compat(input, input).

pred_is_compat(predicate(VarSet, Types, Cond, _, _),
	       predicate(VarSet, Types, Cond, _, _)).

%-----------------------------------------------------------------------------%

:- pred module_add_mode(module_info, varset, sym_name, list(mode), condition,
			module_info, io__state, io__state).
:- mode module_add_mode(input, input, input, input, input, output, di, uo).

module_add_mode(Module0, VarSet, PredName, Modes, Cond, Module) -->
	{ moduleinfo_preds(Module0, Preds0) },
	{ moduleinfo_name(Module0, ModuleName) },
	pred_modes_add(Preds0, ModuleName, VarSet, PredName, Modes, Cond,
			Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module) }.

:- pred pred_modes_add(pred_table, module_name, varset, sym_name, list(mode),
			condition, pred_table, io__state, io__state).
:- mode pred_modes_add(input, input, input, input, input, input, output,
			di, uo).

	% XXX we should store the mode varset and the mode condition
	% in the hlds - at the moment we just ignore those two arguments
pred_modes_add(Preds0, ModuleName, _VarSet, PredName, Modes, _Cond, Preds) -->
	{ length(Modes, Arity),
	  make_predid(ModuleName, PredName, Arity, PredId) },
	(if %%% some [P0]
		{ map__search(Preds0, PredId, P0) }
	then
		{ P0 = predicate(TVarSet, ArgTypes, TCond, Clauses, Procs0) },
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
		{ NewProc = procedure(nondeterministic, BodyVarSet,
			BodyTypes, HeadVars, Modes, conj([]) - GoalInfo) },
		{ map__insert(Procs0, ModeId, NewProc, Procs) },
		{ P = predicate(TVarSet, ArgTypes, TCond, Clauses, Procs) },
		{ map__set(Preds0, PredId, P, Preds) }
	else
		undefined_pred_error(PredName, Arity, "mode declaration"),
		{ Preds = Preds0 }
	).

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
				module_info, io__state, io__state).
:- mode module_add_clause(input, input, input, input, input, output, di, uo).

module_add_clause(Module0, VarSet, PredName, Args, Body, Module) -->
	{ moduleinfo_preds(Module0, Preds0) },
	{ moduleinfo_name(Module0, ModuleName) },
	clauses_add(Preds0, ModuleName, VarSet, PredName, Args, Body, Preds),
	{ moduleinfo_set_preds(Module0, Preds, Module) }.

:- pred clauses_add(pred_table, module_name, varset, sym_name, list(term),
			goal, pred_table, io__state, io__state).
:- mode clauses_add(input, input, input, input, input, input, output, di, uo).

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
				HeadVars, Goal) | Clauses0],
		  PredInfo = predicate(TVarSet, Types, Cond, Clauses, Procs),
		  map__set(Preds0, PredId, PredInfo, Preds) }
	else
		undefined_pred_error(PredName, Arity, "clause")
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
	(if N = 0 then
		VarSet = VarSet0,
		Vars = []
	else
		N1 is N - 1,
		varset__new_var(VarSet0, Var, VarSet1),
		Vars = [Var | Vars1],
		make_n_fresh_vars(N1, VarSet1, Vars1, VarSet)
	).

:- pred insert_head_unifications(list(term), list(term), goal, goal).

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
	ModuleName = "test",

	require(Goal = term_functor(term_atom(PredName), Args),
		"fatal error: called term is not an atom"),
	length(Args, Arity),
	make_predid(ModuleName, unqualified(PredName), Arity, PredId),

	goalinfo_init(GoalInfo).

transform_goal(unify(A, B), unify(A, B, Mode, UnifyInfo) - GoalInfo) :-
	goalinfo_init(GoalInfo),
		% fill in unused slots with garbage values
	Mode = (free -> free),
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
		Conj = [Goal | Conj0]
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
		Disj = [Goal | Disj0]
	).

%-----------------------------------------------------------------------------%

	% Predicates to write out the different warning and error messages.

:- pred duplicate_def_warning(sym_name, int, string, io__state, io__state).
:- mode duplicate_def_warning(input, input, input, di, uo).

duplicate_def_warning(Name, Arity, DefType) -->
	io__write_string("warning: duplicate definition for "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'\n").

:- pred multiple_def_error(sym_name, int, string, io__state, io__state).
:- mode multiple_def_error(input, input, input, di, uo).

multiple_def_error(Name, Arity, DefType) -->
	io__write_string("error: "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' multiply defined\n").

:- pred undefined_pred_error(sym_name, int, string, io__state, io__state).
:- mode undefined_pred_error(input, input, input, di, uo).

undefined_pred_error(Name, Arity, Description) -->
	io__write_string("error: "),
	io__write_string(Description),
	io__write_string(" for `"),
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' without preceding pred declaration\n").

%-----------------------------------------------------------------------------%
