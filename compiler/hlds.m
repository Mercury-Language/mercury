%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% HLDS - The High-Level Data Structure.

:- module hlds.
:- import_module integer, string, list, varset, term, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- interface.

% :- adt module_info.

% :- adt pred_id.

% :- adt pred_mode_id.

% :- adt inst_id.

% :- adt mode_id.

:- type category	--->	deterministic		% functional & total
			;	semideterministic	% just functional
			;	nondeterministic.	% neither

:- type goal		--->	goal_expr - goal_info.

:- type goal_expr    	--->	conj(goals)

				% Initially only the pred_id and arguments
				% are filled in.  Mode analysis fills in the
				% pred_mode_id.  Just before code generation,
				% we do a pass over the hlds which recognizes
				% the builtins and fills in the is_builtin
				% field.
			;	call(pred_id, pred_mode_id, list(term), is_builtin)

				% Deterministic disjunctions are converted
				% into case statements by the determinism
				% analysis.
				% Variable, functor-args-goal, followvars
			;	switch(var_id, list(case), vars)

				% Initially only the two terms are filled
				% in.  Mode analysis fills in the last
				% two fields.
			;	unify(term, term, unify_mode, unification)

			% The remainder aren't used as yet, since
			% we only handle deterministic code.

			;	disj(goals)
			;	not(vars,goal)
					% could use if_then_else instead
			;	all(vars,goal)
			;	some(vars,goal)
			;	if_then_else(vars,goal,goal,goal)
			;	error.

	% Record whether a call is a builtin or not, and if so, which one.
:- type is_builtin	--->	not_builtin
			;	is_builtin(builtin).

:- type builtin		--->	plus
			;	times.
				% etc.

:- type case		--->	case(const, list(var), goal).
			%	functor to match with, arguments to extract,
			%	goal to execute if match succeeds.

	% Initially all unifications are represented as
	% unify(term, term, _, _), but mode analysis replaces
	% these with various special cases.

:- type unification	--->	
				% Y = f(X) where the top node of Y is output,
				% written as Y := f(X).
				construct(var, const, list(var), list(mode))

				% Y = f(X) where the top node of Y is input,
				% written Y == f(X).
			;	deconstruct(var, const, list(var), list(mode))

				% Y = X where the top node of Y is output,
				% written Y := X.
			;	assign(var, var)

				% Y = X where the type of X and Y is an atomic
				% type and they are both input, written X == Y.
			;	simple_test(var, var)	

				% Y = X where the type of Y and X is not an
				% atomic type, and where the top-level node
				% of both Y and X is input.  May involve
				% bi-directional data flow.  Implemented
				% using out-of-line call to  a compiler
				% generated unification predicate for that
				% type & mode.
			;	complicated_unify(unify_mode, var, var).

:- type goals		==	list(goal).
:- type vars		==	list(variable).

%-----------------------------------------------------------------------------%

	% This is how type and modes are represented.
	% The parts that are not defined here (eg. type_param, constructor,
	% type) are represented in the same way as in prog_io.nl,
	% and are defined there.

:- type hlds__type_defn	--->	hlds__type_defn(varset, list(type_param),
					hlds__type_body, condition).

:- type hlds__type_body	--->	du_type(string, list(term), list(constructor))
			;	uu_type(string, list(term), list(type))
			;	eqv_type(string, list(term), (type)).


:- type hlds__inst_defn --->	hlds__inst_defn(varset, list(inst_param),
					inst, condition).

:- type hlds__mode_defn --->	hlds__inst_defn(varset, list(inst_param),
					inst, condition).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- pred module_name(module_info, string).
:- mode module_name(input,output).

:- pred module_predicates(module_info, list(pred_id)).
:- mode module_predicates(input, output).

:- pred predicate_module(pred_id, string).
:- mode predicate_module(input, output).

:- pred predicate_name(pred_id, string).
:- mode predicate_name(input, output).

:- pred predicate_arity(pred_id, int).
:- mode predicate_arity(input, output).

:- pred predicate_modes(module_info, pred_id, list(pred_mode_id)).
:- mode predicate_modes(input, input, output).

:- pred predicate_clauses(module_info, pred_id, pred_mode_id, list(clause)).
:- mode predicate_clauses(input, input, output).

:- pred predicate_category(module_info, pred_id, pred_mode_id, category).
:- mode predicate_category(input, input, input, output).

:- pred var_is_live(module_info, goal_info, var_id, liveness).
:- mode var_is_live(input, input, input, output).

:- pred var_type(module_info, var_info, var_id, type).
:- mode var_type(input, input, input, output).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type xxx		= int.

:- type module_info	--->	module(
					string,		% module name
					pred_table,
					type_table,
					inst_table,
					mode_table
				).

%-----------------------------------------------------------------------------%

	% The symbol table for predicates.

:- type pred_info	--->	predicate(
					varset,		% names of _type_ vars
							% in the type decl
					list(type),	% argument types
					condition,	% formal specification
							% (not used)
					proc_table
				).

:- type proc_table	=	map(pred_mode_id, proc_info).

:- type proc_info	--->	procedure(
					category,
					varset,		% variable names
					map(var_id, type), % variable types
					list(var_id),	% head vars
					mode_info,
					goal  % Body
				).

:- type pred_id 	=	pred(string, string, int).
			%	module, predname, arity

:- type pred_table	=	map(pred_id, pred_info).

:- type proc_id		=	proc(pred_id, pred_mode_id).

	% a pred_mode_id is a mode number within a particular predicate -
	% not to be confused with a mode_id, which is the name of a
	% user-defined mode.
:- type pred_mode_id	=	int.

%-----------------------------------------------------------------------------%

	% The symbol table for types.

:- type type_id		= 	pair(sym_name, int).
				% name, arity

:- type type_table	=	map(type_id, type_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for modes.

:- type mode_id		=	pair(sym_name, int).
				% name, arity

:- type mode_table	=	map(mode_id, mode).

%-----------------------------------------------------------------------------%

	% The symbol table for insts.

:- type inst_id		=	pair(sym_name, int).
				% name, arity.

:- type inst_table	=	map(inst_id, inst_body).

%-----------------------------------------------------------------------------%

:- type var_info	--->	var_info(
					string, % name
					type_id
				).

:- type goal_info	--->	goalinfo(
					map(var_id, is_live)
		% maybe			map(var_id, inst_id)
				).

:- type is_live		--->	live ; dead.

:- type mode_info	=	map(var_id, mode_id). % initial-final

%-----------------------------------------------------------------------------%

	% Various access predicates for the module_info data structure

moduleinfo_name(ModuleInfo, Name) :-
	ModuleInfo = module(Name, _Preds, _Types, _Insts, _Modes).
moduleinfo_preds(ModuleInfo, Preds) :-
	ModuleInfo = module(_Name, Preds, _Types, _Insts, _Modes).
moduleinfo_predids(ModuleInfo, PredIDs) :-
	ModuleInfo = module(_Name, Preds, _Types, _Insts, _Modes),
	map__keys(Preds, PredIDs).
moduleinfo_types(ModuleInfo, Types) :-
	ModuleInfo = module(_Name, _Preds, Types, _Insts, _Modes).
moduleinfo_typeids(ModuleInfo, TypeIDs) :-
	ModuleInfo = module(_Name, _Preds, Types, _Insts, _Modes),
	map__keys(Types, TypeIDs).
moduleinfo_insts(ModuleInfo, Insts) :-
	ModuleInfo = module(_Name, _Preds, _Types, Insts, _Modes).
moduleinfo_instids(ModuleInfo, InstIDs) :-
	ModuleInfo = module(_Name, _Preds, _Types, Insts, _Modes),
	map__keys(Insts, InstIDs).
moduleinfo_modes(ModuleInfo, Modes) :-
	ModuleInfo = module(_Name, _Preds, _Types, _Insts, Modes).
moduleinfo_modeids(ModuleInfo, ModeIDs) :-
	ModuleInfo = module(_Name, _Preds, _Types, _Insts, Modes),
	map__keys(Modes, ModeIDs).

%-----------------------------------------------------------------------------%

predicate_module(pred(Module,_Name,_Arity), Module).

predicate_name(pred(_Module,Name,_Arity), Name).

predicate_arity(pred(_Module,_Name,Arity), Arity).

predinfo_modes(PredInfo, Modes) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, Procs),
	map__keys(Procs, Modes).

predinfo_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, Procs).

%-----------------------------------------------------------------------------%

procinfo_category(ProcInfo, Category) :-
	ProcInfo = procedure(Category, _Names, _Types, _HeadVars, _ModeInfo, _Goal).
procinfo_variables(ProcInfo, VarSet) :-
	ProcInfo = procedure(_Category, VarSet, _Types, _HeadVars, _ModeInfo, _Goal).
procinfo_vartypes(ProcInfo, VarTypes) :-
	ProcInfo = procedure(_Category, _Names, VarTypes, _HeadVars, _ModeInfo, _Goal).
procinfo_headvars(ProcInfo, HeadVars) :-
	ProcInfo = procedure(_Category, _Names, _Types, HeadVars, _ModeInfo, _Goal).
procinfo_modeinfo(ProcInfo, ModeInfo) :-
	ProcInfo = procedure(Category, _Names, _Types, _HeadVars, ModeInfo, _Goal).
procinfo_goal(ProcInfo, Goal) :-
	ProcInfo = procedure(_Category, _Names, _Types, _HeadVars, _ModeInfo, Goal).

%-----------------------------------------------------------------------------%

goalinfo_liveness(GoalInfo, Liveness) :-
	GoalInfo = goalinfo(Liveness).

%-----------------------------------------------------------------------------%

:- pred mode_is_input(mode_table, mode_id).

mode_is_input(ModuleInfo, ModeID) :-
	moduleinfo_modes(ModuleInfo, Modes),
	moduleinfo_insts(ModuleInfo, Insts),
	map__search(Modes, ModeID, Mode),
	Mode = Inst1 - _Inst2.
	map__search(Insts, Inst1, ground).

:- pred mode_is_output(mode_table, mode_id).

mode_is_output(ModuleInfo, ModeID) :-
	moduleinfo_modes(ModuleInfo, Modes),
	moduleinfo_insts(ModuleInfo, Insts),
	map__search(Modes, ModeID, Mode),
	Mode = Inst1 - Inst2,
	map__search(Insts, Inst1, free),
	map__search(Insts, Inst2, ground).

mode_id_to_int(X, X).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
