%-----------------------------------------------------------------------------%
:- module make_hlds.
:- import prog_io, hlds.

%-----------------------------------------------------------------------------%
:- pred parse_tree_to_hlds(prog_io:program, hlds:module_info).
:- mode parse_tree_to_hlds(input, output).

parse_tree_to_hlds(module(Name, Items),  Module) -->
	{ module_init(Module0, Name) },
	add_item_list(Items, Module0, Module).

%-----------------------------------------------------------------------------%

module_init(module(Name, Preds, Types, Insts, Modes), Name) :-
	map__init(Preds),
	map__init(Types),
	map__init(Insts),
	map__init(Modes).

%-----------------------------------------------------------------------------%

add_item_list([], Module, Module) --> [].
add_items([Item|Items], Module0, Module) -->
	add_item(Item, Module0, Module1),
	add_item_list(Items, Module0, Module).

%-----------------------------------------------------------------------------%

add_item(clause(VarSet, PredName, Args, Body), Module0, Module) -->
	module_add_clause(Module0, VarSet, PredName, Args, Body, Module).

add_item(type_defn(VarSet, TypeDefn, Cond), Module0, Module) -->
	module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Module).

add_item(inst_defn(VarSet, InstDefn, Cond), Module0, Module) -->
	module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Module).

add_item(mode_defn(VarSet, ModeDefn, Cond), Module0, Module) -->
	module_add_mode_defn(Module0, VarSet, ModeDefn, Cond, Module).

add_item(pred(VarSet, PredName, TypesAndModes, Cond), Module0, Module) -->
	module_add_pred(Module0, VarSet, PredName, TypesAndModes, Cond, Module).

add_item(mode(VarSet, PredName, Modes, Cond), Module0, Module) -->
	module_add_mode(Module0, VarSet, PredName, Modes, Cond, Module).

add_item(module_defn(_VarSet, _ModuleDefn), Module, Module) -->
	io__write_string("warning: module declarations not yet implemented\n").

%-----------------------------------------------------------------------------%

module_add_inst_defn(Module0, VarSet, InstDefn, Cond, Module) -->
	Module0 = module(Name, Preds, Types, Insts0, Modes),
	insts_add(Insts0, VarSet, InstDefn, Cond, Insts),
	Module =  module(Name, Preds, Types, Insts, Modes).

insts_add(Insts0, VarSet, inst_defn(Name, Args, Body), Cond, Insts) -->
	{ length(Args, Arity),
	  I = hlds__inst_defn(VarSet, Args, Body, Cond) },
	(if some [I2]
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
		map__insert(Insts0, Name - Arity, I, Insts)
	).

%-----------------------------------------------------------------------------%

module_add_mode_defn(Module0, VarSet, TypeDefn, Cond, Module) -->
	Module0 = module(Name, Preds, Types, Insts, Modes0),
	modes_add(Modes0, VarSet, ModeDefn, Cond, Modes),
	Module =  module(Name, Preds, Types, Insts, Modes).

modes_add(Modes0, VarSet, mode_defn(Name, Args, Body), Cond, Modes) -->
	length(Args, Arity),
	I = hlds__mode_defn(VarSet, Args, Body, Cond),
	(if some [I2]
		map__search(Modes0, Name - Arity, I2)
	then
		Modes = Modes0,
		(if mode_is_compat(I, I2) then
			duplicate_def_warning(Name, Arity, "mode")
		else
			multiple_def_error(Name, Arity, "mode")
		)
	else
		map__insert(Modes0, Name - Arity, I, Modes)
	).

%-----------------------------------------------------------------------------%

module_add_type_defn(Module0, VarSet, TypeDefn, Cond, Module) -->
	Module0 = module(Name, Preds, Types0, Insts, Modes),
	types_add(Types0, VarSet, TypeDefn, Cond, Types),
	Module =  module(Name, Preds, Types, Insts, Modes).

type_name_args(du_type(Name, Args, Body), Name, Args, du_type(Body)).
type_name_args(uu_type(Name, Args, Body), Name, Args, uu_type(Body)).
type_name_args(eqv_type(Name, Args, Body), Name, Args, eqv_type(Body)).

types_add(Types0, VarSet, TypeDefn, Cond, Types) -->
	{ type_name_args(TypeDefn, Name, Args, Body),
	  length(Args, Arity),
	  I = hlds__type_defn(VarSet, Args, Body, Cond) },
	(if some [I2]
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

%-----------------------------------------------------------------------------%

module_add_pred(Module0, VarSet, PredName, TypesAndModes, Cond, Module) --> ...
	Module0 = module(Name, Preds0, Types, Insts, Modes),
	preds_add(Preds0, VarSet, PredName, TypesAndModes, Cond, Preds),
	Module = module(Name, Preds, Types, Insts, Modes).

preds_add(Preds0, VarSet, Name, TypesAndModes, Cond, Preds) -->
	{ length(TypesAndModes, Arity),
	  I = predicate(VarSet, Args, Body, Cond) },
	(if some [I2]
		{ map__search(Types0, Name - Arity, I2) }
	then
		{ Types = Types0 },
		(if 
			{ type_is_compat(I, I2) }
		then
			duplicate_def_warning(Name, Arity, "pred")
		else
			multiple_def_error(Name, Arity, "pred")
		)
	else
		{ map__insert(Preds0, Name - Arity, I, Preds) }
	).

%-----------------------------------------------------------------------------%

module_add_mode(Module0, VarSet, PredName, Modes, Cond, Module) --> ...

module_add_clause(Module0, VarSet, PredName, Args, Body, Module) --> ...

%-----------------------------------------------------------------------------%

duplicate_def_warning(Name, Arity, DefType) -->
	io__write_string("warning: duplicate definition for "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("'\n").

multiple_def_error(Name, Arity, DefType) -->
	io__write_string("error: "),
	io__write_string(DefType),
	io__write_string(" `"),
	prog_out__write_sym(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string("' multiply defined\n").

%-----------------------------------------------------------------------------%
