%-----------------------------------------------------------------------------%

:- module hlds.
:- import_module string, list, varset, term, io, map, tree, llds.
:- interface.

:- type module_info	--->	module(
					string,		% module name
					map(pred_id, list(mode_id)),
					map(pair(pred_id, mode_id), clause),
					map(pair(pred_id, mode_id), category),
					map(type_id, type_body)
				).
:- type pred_id 	=	pred(string, string, int).
			%	module, predname, arity
:- type mode_id		=	int.
:- type category	--->	deterministic		% functional & total
			;	semideterministic	% just functional
			;	nondeterministic.	% neither
:- type clause		--->	clause(var_info, list(var), goal).
			%	variable types, head variables, body

:- type var_info	--->	var_info(
					map(var, string),	% var names
					map(var, type)		% var types
				).

:- type liveness_info	=	map(var_id, is_live)
:- type is_live		--->	live ; dead.

:- type register_info	--->	reginfo(
					int,	% number of stack slots used
					int,	% current clause
					int,	% current label within clause
					map(pseudo_reg, reg_status), 
					map(var, pseudo_reg)
				).
:- type pseudo_reg	--->	r(int)
			;	stack(int).
:- type reg_status	--->	var(var)
			;	const(int).

:- type mode_info	=	map(var, instantiatedness).

:- type goal		--->	goal2 - liveness_info.

:- type goal2		--->	(goal,goal)
					% could use conj(goals) instead 
			;	case(var_id, list(pair(term, goal)))
			;	true	
			;	call(pred_id, mode_id, list(term))
			% not used:
			;	{goal;goal}	% {...} quotes ';'/2.
			;	fail	
					% could use disj(goals) instead
			;	not(vars,goal)
					% could use if_then_else instead
			;	all(vars,goal)
			;	some(vars,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	error.

:- type goals		==	list(goal).
:- type vars		==	list(variable).

:- type int		==	integer.

:- code_tree		==	tree(instructions).

:- pred module_name(module_info, string).
:- mode module_name(input,output).

:- pred module_predicates(module_info, list(pred_id)).
:- mode module_predicates(input, output).

:- pred predicate_module(module_info, pred_id, string).
:- mode predicate_module(input, input, output).

:- pred predicate_name(module_info, pred_id, string).
:- mode predicate_name(input, input, output).

:- pred predicate_arity(module_info, pred_id, int).
:- mode predicate_arity(input, input, output).

:- pred predicate_modes(module_info, pred_id, list(mode_id)).
:- mode predicate_modes(input, input, output).

:- pred predicate_clauses(module_info, pred_id, mode_id, list(clause)).
:- mode predicate_clauses(input, input, output).

:- pred predicate_category(module_info, pred_id, mode_id, category).
:- mode predicate_category(input, input, input, output).

:- pred var_is_live(module_info, liveness_info, var_is, is_live).
:- mode var_is_live(input, input, input, output).

:- pred var_type(module_info, var_info, var_id, type).
:- mode var_type(input, input, input, output).

:- pred generate_code(module_info, c_file).
:- mode generate_code(input, output).

%-----------------------------------------------------------------------------%

	% OK, this is how types are represented.

:- type type_defn	--->	du_type(string, list(term), list(constructor))
			;	uu_type(string, list(term), list(type_body))
			;	eqv_type(string, list(term), type_body).
:- type constructor	==	term.
:- type type_head	==	term.
:- type type_body	==	term.
:- type (type)		==	term.

:- type condition	--->	true
			;	where(term).
%-----------------------------------------------------------------------------%

:- implementation.

generate_code(ModuleInfo, c_file(Name, [c_module(Mod_Name, Instructions)])) :-
	module_name(ModuleInfo, Name),
	string__append(Name,"_module",Mod_Name),
	module_predicates(ModuleInfo, PredList),
	generate_pred_list_code(ModuleInfo, PredList, InstrTree),
	tree__flatten(InstrTree,Instructions).

generate_pred_list_code(ModuleInfo, [], empty).
generate_pred_list_code(ModuleInfo, [ PredId | PredIds ], Instr) :-
	generate_pred_code(ModuleInfo, PredId, Instr1),
	generate_pred_list_code(ModuleInfo, PredIds, Instr2),
	Instr = tree(Instr1,Instr2).

generate_pred_code(ModuleInfo, PredId, Instr) :-
	predicate_modes(ModuleInfo, PredId, Modes),
	generate_mode_list_code(ModuleInfo, PredId, Modes, Instr).

generate_mode_list_code(ModuleInfo, PredId, [], empty).
generate_mode_list_code(ModuleInfo, PredId, [ Mode | Modes ], Instr) :-
	predicate_category(ModuleInfo, PredId, Mode, Category),
	generate_category_code(ModuleInfo, PredId, Mode, Category, Instr).

generate_category_code(ModuleInfo, PredId, Mode, deterministic, Instr) :-
	predicate_clauses(ModuleInfo, PredId, Mode, Clauses),
	Clauses = [ clause(VarInfo, HeadVars, Body) | ClauseTail ],
	require( ClauseTail = [], "Deterministic code isn't"),
	module_get_modes(ModuleInfo, PredId, Mode, HeadVars, ModeInfo),
	init_reg_info(ModuleInfo, ModeInfo, VarInfo, HeadVars, RegInfo0),
	generate_det_goal(ModuleInfo, VarInfo, Body, RegInfo0, 
		RegInfo1, Instr1),
	generate_det_epilog(ModuleInfo, VarInfo, ModeInfo, HeadVars, 
		RegInfo1, Instr2),
	Instr = tree(Instr1,Instr2).

generate_category_code(ModuleInfo, PredId, Mode, semideterministic, Instr) :-
	require(fail,"Unimplemented").

generate_category_code(ModuleInfo, PredId, Mode, nondeterministic, Instr) :-
	require(fail,"Unimplemented").

init_reg_info(ModuleInfo, ModeInfo, VarInfo, HeadVars, reginfo(0,1,0,Regs)) :-
	map__init(Regs0),
	init_reg_info_2(ModuleInfo, HeadVars, ModeInfo, 1, Regs0, Regs).

init_reg_info_2(_ModuleInfo, [], _ModeInfo, _, Registers, Registers).
init_reg_info_2(ModuleInfo, [ V | Vs ], ModeInfo, RegNum, Registers0, Registers) :-
	map__search(ModeInfo, V, V_Mode),
	(if mode_is_input(ModeInfo, V_Mode) then
		map__insert(Registers0, r(RegNum), var(V), Registers1)
	else
		Registers1 = Registers
	),
	RegNum1 is RegNum + 1,
	init_reg_info_2(ModuleInfo, Vs, ModeInfo, RegNum1, Registers1, Registers).

generate_det_goal(ModuleInfo, VarInfo, Goal - Liveness, RegInfo0, Instr0, 
		RegInfo, Instr) :-
	generate_det_goal_2(Goal, ModuleInfo, VarInfo, Liveness, RegInfo0,
		Instr0, RegInfo, Instr).

:- mode generate_det_goal_2(input, input, input, input, input, output, output).

generate_det_goal_2(true, _ModuleInfo, _VarInfo, _Liveness, RegInfo, 
		RegInfo, empty).
generate_det_goal_2((A, B), ModuleInfo, VarInfo, _Liveness, RegInfo0, 
		RegInfo, tree(Instr1, Instr2)) :-
	generate_det_goal(ModuleInfo, VarInfo, A, RegInfo0, RegInfo1, Instr1),
	generate_det_goal(ModuleInfo, VarInfo, B, RegInfo1, RegInfo, Instr2).
generate_det_goal_2(call(PredId, ModeId, Args), ModuleInfo, VarInfo, 
		Liveness, RegInfo0, RegInfo, Instr) :-
	module_get_modes(ModuleInfo, PredId, ModeId, Args, ModeInfo),
		% Save and restore succip.
	PushSuccip = node([
			assign(reg(stackvar(0)), lval(reg(succip))) - "push(succip)"
			incr_sp(1) - "",
		]),
	PopSuccip = node([
			decr_sp(1) - "succip := pop()",
			assign(reg(succip), lval(reg(stackvar(0)))) - ""
		])
	setup_det_args(Args, ModeInfo, VarInfo, Liveness, RegInfo0,
		RegInfo1, Instr1),
	get_predicate_address(ModuleInfo, PredId, ModeId, PredAddress),
	generate_label(ModuleInfo, PredId, RegInfo1, LocalLabel, RegInfo2),
	CallInstr = node([
			call(PredAddress, LocalLabel) - "Call",
			label(LocalLabel) - "Return Label"
		]),
	Instr = tree( PushSuccip, tree(Instr1, tree(CallInst, PopSuccip))).

%-----------------------------------------------------------------------------%

setup_det_args(Args, ModeInfo, VarInfo, Liveness, RegInfo0, RegInfo, Instr) :-
	save_det_vars(ModeInfo, VarInfo, Liveness, RegInfo0, RegInfo1, Instr1),
	setup_det_args_2(Args, ModeInfo, Liveness, RegInfo1, RegInfo, Instr2),
	Instr = tree(Instr1,Instr2).

%-----------------------------------------------------------------------------%

	% Save all the live variables which only reside in registers.

:- pred save_det_vars(mode_info, liveness_info, register_info,
		register_info, code_tree).
:- mode save_det_vars(input, input, input, output, output).

save_det_vars(ModeInfo, VarInfo, Liveness, RegInfo0, RegInfo, Instr) :-
	map__keys(Liveness, VarIds),
	save_det_vars_2(VarIds, ModeInfo, VarInfo, Liveness, RegInfo0,
			RegInfo, Instr).

:- pred save_det_vars_2(list(var), mode_info, var_info, liveness_info,
		register_info, register_info, code_tree).
:- mode save_det_vars_2(input, input, input, input, input, output, output).

save_det_vars_2([], _ModeInfo, _VarInfo, _Liveness, RegInfo, RegInfo, empty).
save_det_vars_2([ VarId | VarIds ], ModeInfo, VarInfo, Liveness, RegInfo0, 
		RegInfo, Instr) :-
	map__search(Liveness, VarId, Mortality),
	RegInfo0 = reginfo(_,_,_,Regs),
	map__to_assoc_list(Regs, RegList),
	(if
		% if the variable is live and it hasn't already been
		% saved in a stack slot
		Mortality = live, 
		not_on_stack(RegList, VarId),
		first_reg(RegList, VarId, Reg)
	then
		% save it!
		allocate_stack_slot(RegInfo0, Slot, RegInfo1),
		varinfo_get_var_name(VarInfo, VarId, VarName),
		string__append("save variable ", VarName, Comment),
		Instr1 = node([
			assign(reg(stackvar(0)), lval(reg(Reg))) - Comment
			incr_sp(1) - "",
		]),

	else

:- pred not_on_stack(list(map__pair(pseudo_reg, var)), var).
:- mode not_on_stack(input, input).
not_on_stack(RegList, VarId) :-
	stack_vars(RegList, Vars),
	not member(VarId, Vars).

:- pred stack_vars(list(map__pair(pseudo_reg, var)), list(var)).
:- mode stack_vars(input, output).
stack_vars([], []).
stack_vars([ R - V | Vs ], L) :-
	stack_vars(Vs, L1),
	(if
		R = stack(_)
	then
		L = L1
	else
		L = [ V | L1 ]
	).

:- pred first_reg(list(map__pair(pseudo_reg, var)), var, int).
:- mode first_reg(input, input, output).
first_reg([ stackvar(_) - _ | Vs ], VarId, Reg) :-
	first_reg(Vs, VarId, Reg).
first_reg([ reg(Reg1) - V | _ ], VarId, Reg) :-
	(if
		V = VarId
	then
		Reg = Reg1
	else
		first_reg(Vs, VarId, Reg)
	).

%-----------------------------------------------------------------------------%

setup_det_args_2([], _ModeInfo, _Liveness, RegInfo, RegInfo, empty).
setup_det_args_2([ Arg | Args ], ModeInfo, Liveness, RegInfo0, 
		RegInfo1, Instr) :-
	


generate_label(ModuleInfo, PredId, Mode, RegInfo0, Label, RegInfo) :-
	RegInfo0 = reginfo(StackPos, ClauseNum, LabelNum, Regs),
	LableNum1 is LabelNum + 1,
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = label(ModuleName, PredName, Arity, Mode, ClauseNum, LabelNum),
	RegInfo = reginfo(StackPos, ClauseNum, LabelNum1, Regs).

generate_entrylabel(ModuleInfo, PredId, Mode, Label) :-
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, Arity),
	Label = entrylabel(ModuleName, PredName, Arity, Mode).

get_predicate_address(ModuleInfo, PredId, ModeId, PredAddress) :-
	module_name(ModuleInfo, ModuleName),
	predicate_module(ModuleInfo, PredId, PredModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	predicate_arity(ModuleInfo, PredId, PredArity),
	(if
		ModuleName = PredModuleName
	then
		PredAddress = local(entrylabel(ModuleName, PredName, PredArity,
				ModeId))
	else
		PredAddress = nonlocal(PredModuleName, PredName, PredArity,
				ModeId))
	).

generate_det_epilog(ModuleInfo, VarInfo, ModeInfo, HeadVars, RegInfo1, Instr1, Instr).
generate_det_goal(...) :-

generate_det_epilog(...) :-

mode_is_input(...) :-

module_get_modes(...) :-

module_predicates(module(_,PredInfo,_,_,_), PredIds) :-
	map__keys(PredInfo,PredIds).

predicate_module(_, pred(Module,_Name,_Arity), Module).

predicate_name(_, pred(_Module,Name,_Arity), Name).

predicate_arity(_, pred(_Module,_Name,Arity), Arity).

predicate_modes(module(_,PredInfo,_,_,_), PredId, Modes) :-
	map__search(PredInfo, PredId, Modes).

predicate_clauses(module(_,_,ClauseInfo,_,_), PredId, ModeId, Clauses) :-
	map__search(ClauseInfo, PredId - ModeId, Clauses).

predicate_category(module(_,_,_,CategoryInfo,_), PredId, ModeId, Category) :-
	map__search(CategoryInfo, PredId - ModeId, Category).

module_name(module(Name,_,_,_,_),Name).
