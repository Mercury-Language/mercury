%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% HLDS - The High-Level Data Structure.

% Main authors: fjh, conway, zs.

% This file contains the data types for the high-level data structure.
% The file is arranged as follows: first all the data structures are
% listed.  Most of these are private to this module, with access
% predicates provided.  Those that are not are explicitly exported
% using a `:- export_type' directive.
% Then for each data structure, we give the interface and then
% the implementation for the access predicates for that data structure.
%
% Although most of the data structures are private, it is a quite thin
% layer of abstraction.

:- module hlds.
:- import_module int, string, list, varset, term, map.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type xxx		= int.

:- type module_info	--->	module(
					string,		% module name
					pred_table,
					type_table,
					inst_table,
					mode_table,
					cons_table
				).

%-----------------------------------------------------------------------------%

	% The symbol table for predicates.

:- type pred_info	--->	predicate(
					varset,		% names of _type_ vars
							% in the pred type decl
					list(type),	% argument types
					condition,	% formal specification
							% (not used)

					clause_list,

					proc_table,

					term__context	% the location (line #)
							% of the :- pred decl.
				).

:- type clause_list	=	list(clause).

:- type clause		--->	clause(
					list(pred_mode_id),
							% modes for which
							% this clause applies
					varset,		% variable names
					map(var_id, type), % variable types
					list(var_id),	% head vars
					hlds__goal,	% Body
					term__context
				).

:- export_type proc_table.
:- type proc_table	=	map(pred_mode_id, proc_info).

:- type proc_info	--->	procedure(
					category,
					varset,		% variable names
					map(var_id, type), % variable types
					list(var_id),	% head vars
					list(mode),
					hlds__goal,	% Body
					term__context,	% The context of
							% the :- mode decl,
							% not the clause.
					call_info	% stack allocations
				).

:- export_type category.
:- type category	--->	deterministic(det_source) % functional & total
			;	semideterministic(det_source) % just functional
			;	nondeterministic(det_source) % neither
			;	unspecified.

:- export_type det_source.
:- type det_source	--->	declared
			;	infered.

:- type pred_id 	--->	pred(module_name, string, int).
			%	module, predname, arity

:- export_type pred_table.
:- type pred_table	=	map(pred_id, pred_info).

:- type proc_id		--->	proc(pred_id, pred_mode_id).

	% a pred_mode_id is a mode number within a particular predicate -
	% not to be confused with a mode_id, which is the name of a
	% user-defined mode.
:- type pred_mode_id	=	int.

%-----------------------------------------------------------------------------%

	% The symbol table for types.

:- type type_id		= 	pair(sym_name, int).
				% name, arity

:- export_type type_table.
:- type type_table	=	map(type_id, hlds__type_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for modes.

:- type mode_id		=	pair(sym_name, int).
				% name, arity

:- export_type mode_table.
:- type mode_table	=	map(mode_id, hlds__mode_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for insts.

:- type inst_id		=	pair(sym_name, int).
				% name, arity.

:- export_type inst_table.
:- type inst_table	=	map(inst_id, hlds__inst_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for constructors.
	% This table is used by the type-checker to look
	% up the type of functors/constants.

:- type cons_id		--->	cons(string, int).
				% name, arity

:- export_type cons_table.
:- type cons_table	=	map(cons_id, hlds__cons_defn).

%-----------------------------------------------------------------------------%

	% Here's how goals are represented

:- export_type hlds__goal.
:- type hlds__goal		--->	hlds__goal_expr - hlds__goal_info.

:- export_type hlds__goal_expr.
:- type hlds__goal_expr    	--->	conj(hlds__goals)

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
			;	switch(var_id, list(case), follow_vars)

				% Initially only the two terms are filled
				% in.  Mode analysis fills in the last
				% two fields.
			;	unify(term, term, unify_mode, unification)

			% The remainder aren't used as yet, since
			% we only handle deterministic code.

			;	disj(hlds__goals)
			;	not(list(var), hlds__goal)
					% could use if_then_else instead
			;	all(list(var), hlds__goal)
			;	some(list(var), hlds__goal)
			;	if_then_else(list(var), hlds__goal,
					hlds__goal, hlds__goal)
			;	error.

	% Record whether a call is a builtin or not, and if so, which one.
:- export_type is_builtin.
:- type is_builtin	--->	not_builtin
			;	is_builtin(builtin).

:- export_type builtin.
:- type builtin		--->	plus
			;	times.
				% etc.	XXX

:- export_type call_info.
:- type call_info	--->	map(var_id, int).

:- export_type case.
:- type case		--->	case(cons_id, list(var), hlds__goal).
			%	functor to match with, arguments to extract,
			%	goal to execute if match succeeds.

	% Initially all unifications are represented as
	% unify(term, term, _, _), but mode analysis replaces
	% these with various special cases.

:- export_type follow_vars.
:- type follow_vars	--->	map(var_id, register).

:- export_type unification.
:- type unification	--->	
				% Y = f(X) where the top node of Y is output,
				% written as Y := f(X).
				construct(var, cons_id, list(var), list(mode))

				% Y = f(X) where the top node of Y is input,
				% written Y == f(X).
			;	deconstruct(var, cons_id, list(var), list(mode))

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

:- export_type hlds__goals.
:- type hlds__goals		==	list(hlds__goal).

:- type hlds__goal_info	--->	goalinfo(
					map(var_id, is_live), 
					category,
					map(var_id, inst)
				).

:- export_type is_live.
:- type is_live		--->	live ; dead.

%-----------------------------------------------------------------------------%

	% This is how type, modes and constructors are represented.
	% The parts that are not defined here (eg. type_param, constructor,
	% type) are represented in the same way as in prog_io.nl,
	% and are defined there.

:- type hlds__type_defn	--->	hlds__type_defn(
						% names of type vars (empty 
						% except for polymorphic types)
					varset,	
						% formal type parameters
					list(type_param),
						% the definition of the type
					hlds__type_body,
						% a class invariant for the
						% type (not used)
					condition,
					term__context
				).

	% du = discriminated union, uu = undiscriminated union,
	% eqv_type = equivalence type (a type defined to be eqv to some
	% other type)

:- type hlds__type_body	--->	du_type(list(constructor))
			;	uu_type(list(type))
			;	eqv_type(type).


:- type hlds__inst_defn --->	hlds__inst_defn(varset, list(inst_param),
					inst, condition, term__context).

:- type hlds__mode_defn --->	hlds__mode_defn(varset, list(inst_param),
					mode, condition, term__context).

:- type hlds__cons_defn	--->	hlds__cons_defn(
					%%% maybe: varset,
					list(type),	% arg types
					type_id,	% result type
					term__context
				).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for manipulating the module_info data structure

:- interface.

:- pred moduleinfo_init(string, module_info).
:- mode moduleinfo_init(input, output).

:- pred moduleinfo_name(module_info, string).
:- mode moduleinfo_name(input, output).

:- pred moduleinfo_preds(module_info, pred_table).
:- mode moduleinfo_preds(input, output).

:- pred moduleinfo_predids(module_info, list(pred_id)).
:- mode moduleinfo_predids(input, output).

:- pred moduleinfo_types(module_info, type_table).
:- mode moduleinfo_types(input, output).

:- pred moduleinfo_typeids(module_info, list(type_id)).
:- mode moduleinfo_typeids(input, output).

:- pred moduleinfo_insts(module_info, inst_table).
:- mode moduleinfo_insts(input, output).

:- pred moduleinfo_instids(module_info, list(inst_id)).
:- mode moduleinfo_instids(input, output).

:- pred moduleinfo_modes(module_info, mode_table).
:- mode moduleinfo_modes(input, output).

:- pred moduleinfo_modeids(module_info, list(mode_id)).
:- mode moduleinfo_modeids(input, output).

:- pred moduleinfo_ctors(module_info, mode_table).
:- mode moduleinfo_ctors(input, output).

:- pred moduleinfo_consids(module_info, list(cons_id)).
:- mode moduleinfo_consids(input, output).

:- pred moduleinfo_set_name(module_info, string, module_info).
:- mode moduleinfo_set_name(input, input, output).

:- pred moduleinfo_set_preds(module_info, pred_table, module_info).
:- mode moduleinfo_set_preds(input, input, output).

:- pred moduleinfo_set_types(module_info, type_table, module_info).
:- mode moduleinfo_set_types(input, input, output).

:- pred moduleinfo_set_insts(module_info, inst_table, module_info).
:- mode moduleinfo_set_insts(input, input, output).

:- pred moduleinfo_set_modes(module_info, mode_table, module_info).
:- mode moduleinfo_set_modes(input, input, output).

:- pred moduleinfo_set_ctors(module_info, cons_table, module_info).
:- mode moduleinfo_set_ctors(input, input, output).

%-----------------------------------------------------------------------------%

:- implementation.

	% A predicate which creates an empty module

moduleinfo_init(Name, module(Name, Preds, Types, Insts, Modes, Ctors)) :-
	map__init(Preds),
	map__init(Types),
	map__init(Insts),
	map__init(Modes),
	map__init(Ctors).

	% Various access predicates which extract different pieces
	% of info from the module_info data structure.

moduleinfo_name(ModuleInfo, Name) :-
	ModuleInfo = module(Name, _Preds, _Types, _Insts, _Modes, _Ctors).

moduleinfo_preds(ModuleInfo, Preds) :-
	ModuleInfo = module(_Name, Preds, _Types, _Insts, _Modes, _Ctors).

moduleinfo_predids(ModuleInfo, PredIDs) :-
	ModuleInfo = module(_Name, Preds, _Types, _Insts, _Modes, _Ctors),
	map__keys(Preds, PredIDs).

moduleinfo_types(ModuleInfo, Types) :-
	ModuleInfo = module(_Name, _Preds, Types, _Insts, _Modes, _Ctors).

moduleinfo_typeids(ModuleInfo, TypeIDs) :-
	ModuleInfo = module(_Name, _Preds, Types, _Insts, _Modes, _Ctors),
	map__keys(Types, TypeIDs).

moduleinfo_insts(ModuleInfo, Insts) :-
	ModuleInfo = module(_Name, _Preds, _Types, Insts, _Modes, _Ctors).

moduleinfo_instids(ModuleInfo, InstIDs) :-
	ModuleInfo = module(_Name, _Preds, _Types, Insts, _Modes, _Ctors),
	map__keys(Insts, InstIDs).

moduleinfo_modes(ModuleInfo, Modes) :-
	ModuleInfo = module(_Name, _Preds, _Types, _Insts, Modes, _Ctors).

moduleinfo_modeids(ModuleInfo, ModeIDs) :-
	ModuleInfo = module(_Name, _Preds, _Types, _Insts, Modes, _Ctors),
	map__keys(Modes, ModeIDs).

moduleinfo_ctors(ModuleInfo, Ctors) :-
	ModuleInfo = module(_Name, _Preds, _Types, _Insts, _Modes, Ctors).

moduleinfo_consids(ModuleInfo, ConsIDs) :-
	ModuleInfo = module(_Name, _Preds, _Types, _Insts, _Modes, Ctors),
	map__keys(Ctors, ConsIDs).

	% Various predicates which modify the module_info data structure.

moduleinfo_set_name(ModuleInfo0, Name, ModuleInfo) :-
	ModuleInfo0 = module(_, Preds, Types, Insts, Modes, Ctors),
	ModuleInfo = module(Name, Preds, Types, Insts, Modes, Ctors).

moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo) :-
	ModuleInfo0 = module(Name, _, Types, Insts, Modes, Ctors),
	ModuleInfo = module(Name, Preds, Types, Insts, Modes, Ctors).

moduleinfo_set_types(ModuleInfo0, Types, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, _, Insts, Modes, Ctors),
	ModuleInfo = module(Name, Preds, Types, Insts, Modes, Ctors).

moduleinfo_set_insts(ModuleInfo0, Insts, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, Types, _, Modes, Ctors),
	ModuleInfo = module(Name, Preds, Types, Insts, Modes, Ctors).

moduleinfo_set_modes(ModuleInfo0, Modes, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, Types, Insts, _, Ctors),
	ModuleInfo = module(Name, Preds, Types, Insts, Modes, Ctors).

moduleinfo_set_ctors(ModuleInfo0, Ctors, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, Types, Insts, Modes, _),
	ModuleInfo = module(Name, Preds, Types, Insts, Modes, Ctors).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the information stored in the
	% pred_id and pred_info data structures.

:- interface.

:- pred predicate_module(pred_id, module_name).
:- mode predicate_module(input, output).

:- pred predicate_name(pred_id, string).
:- mode predicate_name(input, output).

:- pred predicate_arity(pred_id, int).
:- mode predicate_arity(input, output).

:- pred predinfo_modes(pred_info, list(pred_mode_id)).
:- mode predinfo_modes(input, output).

:- pred predinfo_arg_types(pred_info, varset, list(type)).
:- mode predinfo_arg_types(input, output, output).

:- pred predinfo_clauses(pred_info, clause_list).
:- mode predinfo_clauses(input, output).

:- mode predinfo_procedures(input, output).
:- mode predinfo_procedures(input, output).

%-----------------------------------------------------------------------------%

:- implementation.

predicate_module(pred(Module,_Name,_Arity), Module).

predicate_name(pred(_Module,Name,_Arity), Name).

predicate_arity(pred(_Module,_Name,Arity), Arity).

predinfo_modes(PredInfo, Modes) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, _Clauses, Procs, _),
	map__keys(Procs, Modes).

predinfo_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, _Clauses, Procs, _).

predinfo_clauses(PredInfo, Clauses) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, Clauses, _Procs, _).

predinfo_arg_types(PredInfo, TypeVars, ArgTypes) :-
	PredInfo = predicate(TypeVars, ArgTypes, _Cond, _Clauses, _Procs, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the proc_info data structure.

:- interface.

:- pred procinfo_category(proc_info, category).
:- mode procinfo_category(input, output).

:- pred procinfo_variables(proc_info, varset).
:- mode procinfo_variables(input, output).

:- pred procinfo_vartypes(proc_info, map(var, type)).
:- mode procinfo_vartypes(input, output).

:- pred procinfo_headvars(proc_info, list(var)).
:- mode procinfo_headvars(input, output).

:- pred procinfo_modeinfo(proc_info, list(mode)).
:- mode procinfo_modeinfo(input, output).

:- pred procinfo_goal(proc_info, hlds__goal).
:- mode procinfo_goal(input, output).

:- pred procinfo_context(proc_info, term__context).
:- mode procinfo_context(input, output).

:- pred procinfo_callinfo(proc_info, call_info).
:- mode procinfo_callinfo(input, output).

:- pred procinfo_arg_registers(proc_info, list(var_id), map(var_id, int)).
:- mode procinfo_arg_registers(input, input, output).

:- implementation.

procinfo_category(ProcInfo, Category) :-
	ProcInfo = procedure(Category, _Names, _Types, _HeadVars,
				_ModeInfo, _Goal, _Context, _CallInfo).
procinfo_variables(ProcInfo, VarSet) :-
	ProcInfo = procedure(_Category, VarSet, _Types, _HeadVars,
				_ModeInfo, _Goal, _Context, _CallInfo).
procinfo_vartypes(ProcInfo, VarTypes) :-
	ProcInfo = procedure(_Category, _Names, VarTypes, _HeadVars,
				_ModeInfo, _Goal, _Context, _CallInfo).
procinfo_headvars(ProcInfo, HeadVars) :-
	ProcInfo = procedure(_Category, _Names, _Types, HeadVars,
				_ModeInfo, _Goal, _Context, _CallInfo).
procinfo_modeinfo(ProcInfo, ModeInfo) :-
	ProcInfo = procedure(_Category, _Names, _Types, _HeadVars,
				ModeInfo, _Goal, _Context, _CallInfo).
procinfo_goal(ProcInfo, Goal) :-
	ProcInfo = procedure(_Category, _Names, _Types, _HeadVars,
				_ModeInfo, Goal, _Context, _CallInfo).
procinfo_context(ProcInfo, Context) :-
	ProcInfo = procedure(_Category, _Names, _Types, _HeadVars,
				_ModeInfo, _Goal, Context, _CallInfo).
procinfo_callinfo(ProcInfo, CallInfo) :-
	ProcInfo = procedure(_Category, _Names, _Types, _HeadVars,
				_ModeInfo, _Goal, _Context, CallInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Access predicates for the hlds__goal_info data structure.

:- interface.

:- pred goalinfo_init(hlds__goal_info).
:- mode goalinfo_init(output).

:- pred goalinfo_liveness(hlds__goal_info, map(var_id, is_live)).
:- mode goalinfo_liveness(input, output).

%-----------------------------------------------------------------------------%

:- implementation.

goalinfo_init(goalinfo(Liveness, unspecified, InstMap)) :-
	map__init(Liveness),
	map__init(InstMap).

goalinfo_liveness(GoalInfo, Liveness) :-
	GoalInfo = goalinfo(Liveness, _Detism, _InstMap).

goalinfo_set_liveness(GoalInfo0, Liveness, GoalInfo) :-
	GoalInfo0 = goalinfo(_, Detism, InstMap),
	GoalInfo = goalinfo(Liveness, Detism, InstMap).

goalinfo_category(GoalInfo, Detism) :-
	GoalInfo = goalinfo(_Liveness, Detism, _InstMap).

goalinfo_set_category(GoalInfo0, Detism, GoalInfo) :-
	GoalInfo0 = goalinfo(Liveness, _, InstMap),
	GoalInfo = goalinfo(Liveness, Detism, InstMap).

goalinfo_instmap(GoalInfo, InstMap) :-
	GoalInfo = goalinfo(_Liveness, _Detism, InstMap).

goalinfo_set_instmap(GoalInfo0, InstMap, GoalInfo) :-
	GoalInfo0 = goalinfo(Liveness, Detism, _),
	GoalInfo = goalinfo(Liveness, Detism, InstMap).

liveness_livevars(Liveness, LiveVars) :-
	findall([X], map__search(Liveness, X, live), LiveVars).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates which are used by codegen to
	% access the mode data structure.

:- interface.

:- pred mode_is_input(mode_table, mode).
:- mode mode_is_input(input, input).

:- pred mode_is_output(mode_table, mode).
:- mode mode_is_output(input, input).

:- pred pred_mode_id_to_int(pred_mode_id, int).
:- mode pred_mode_id_to_int(input, output).

%-----------------------------------------------------------------------------%

:- implementation.

	% A mode is considered an input mode if the top-level
	% node is input.

mode_is_input(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
	inst_is_bound(ModuleInfo, InitialInst).

	% A mode is considered an output mode if the top-level
	% node is output.

mode_is_output(ModuleInfo, Mode) :-
	mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
	inst_is_free(ModuleInfo, InitialInst),
	inst_is_bound(ModuleInfo, FinalInst).

	% inst_is_free succeeds iff the inst passed is `free'
	% or is a user-defined inst which is defined as `free'.

:- pred inst_is_free(module_info, inst).
:- mode inst_is_free(input, input).

inst_is_free(_, free).
inst_is_free(_, inst_var(_)) :-
	error("internal error: uninstantiated inst parameter").
inst_is_free(ModuleInfo, user_defined_inst(Name, Args)) :-
	inst_lookup(ModuleInfo, Name, Args, Inst),
	inst_is_free(ModuleInfo, Inst).

	% inst_is_bound succeeds iff the inst passed is not `free'
	% or is a user-defined inst which is not defined as `free'.

:- pred inst_is_bound(module_info, inst).
:- mode inst_is_bound(input, input).

inst_is_bound(_, ground).
inst_is_bound(_, bound(_)).
inst_is_bound(_, inst_var(_)) :-
	error("internal error: uninstantiated inst parameter").
inst_is_bound(ModuleInfo, user_defined_inst(Name, Args)) :-
	inst_lookup(ModuleInfo, Name, Args, Inst),
	inst_is_bound(ModuleInfo, Inst).

:- pred inst_lookup(module_info, string, list(inst), inst).
:- mode inst_lookup(input, input, input, output).

inst_lookup(ModuleInfo, Name, Args, Inst) :-
	length(Args, Arity),
	moduleinfo_modes(ModuleInfo, Modes),
	map__search(Modes, Name - Arity, ModeDefn),
	ModeDefn = hlds__inst_defn(_VarSet, Params, Inst0, _Cond, _Context),
	inst_substitute_args(Inst0, Args, Params, Inst).

	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode.

:- pred mode_get_insts(module_info, mode, inst, inst).
:- mode mode_get_insts(input, input, output, output).

mode_get_insts(_ModuleInfo, InitialInst -> FinalInst, InitialInst, FinalInst).
mode_get_insts(ModuleInfo, user_defined_mode(Name, Args), Initial, Final) :-
	length(Args, Arity),
	moduleinfo_modes(ModuleInfo, Modes),
	map__search(Modes, Name - Arity, HLDS_Mode),
	HLDS_Mode = hlds__mode_defn(_VarSet, Params, Mode0, _Cond),
	mode_substitute_args(Args, Params, Mode0, Mode),
	mode_get_insts(ModuleInfo, Mode, Initial, Final).

	% mode_substute_args(Args, Params, Mode0, Mode) is true
	% iff Mode is the mode that results from substituting all
	% occurrences of Params in Mode0 with the corresponding
	% value in Args.

:- pred mode_substitute_args(list(inst), list(inst_param), mode, mode).
:- mode mode_substitute_args(input, input, input, output).

mode_substitute_args([], [], Mode, Mode).
mode_substitute_args([Arg|Args], [Param|Params], Mode0, Mode) :-
	mode_substitute_arg(Mode0, Arg, Param, Mode1),
	mode_substitute_args(Args, Params, Mode1, Mode).

	% mode_substitute_arg(Mode0, Arg, Param, Mode) is true
	% iff Mode is the mode that results from substituting all
	% occurrences of Param in Mode0 with Arg.

:- pred mode_substitute_arg(mode, inst_param, mode, mode).
:- mode mode_substitute_arg(input, input, input, output).

mode_substitute_arg(I0 -> F0, Arg, Param, I -> F) :-
	inst_substitute_arg(I0, Arg, Param, I),
	inst_substitute_arg(F0, Arg, Param, F).
mode_substitute_arg(user_defined_mode(Name, Args0), Arg, Param,
		    user_defined_mode(Name, Args)) :-
	inst_substitute_args(Args0, Arg, Param, Args).

	% inst_substitute_args(Inst0, Args, Params, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Param in Inst0 with Arg.

:- pred inst_substitute_args(list(inst), inst_param, mode, list(inst)).
:- mode inst_substitute_args(input, input, input, output).

inst_substitute_args([], _, _, []).
inst_substitute_args([A0|As0], Arg, Param, [A|As]) :-
	inst_substitute_arg(A0, Arg, Param, A),
	inst_substitute_args(As0, Arg, Param, As).

	% inst_substitute_arg(Inst0, Arg, Param, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Param in Inst0 with Arg.

:- pred inst_substitute_arg(inst, inst_param, mode, inst).
:- mode inst_substitute_arg(input, input, input, output).

inst_substitute_arg(free, _, _, free).
inst_substitute_arg(ground, _, _, ground).
inst_substitute_arg(bound(Alts0), Arg, Param, bound(Alts)) :-
	inst_substitute_alts(Alts0, Arg, Param, Alts).
inst_substitute_arg(inst_var(Var), Arg, Param, Result) :-
	Param = term_variable(ParamVar),	% XXX params should be vars!
	(if Var = ParamVar then
		Result = Arg
	else
		Result = inst_var(Var)
	).
inst_substitute_arg(user_defined_inst(Name, Args0), Arg, Param,
		    user_defined_inst(Name, Args)) :-
	inst_substitute_args(Args0, Arg, Param, Args).

:- pred inst_substitute_alts(list(bound_inst), inst, inst_param,
				list(bound_inst)).
:- mode inst_substitute_alts(input, input, input, output).

inst_substitute_alts([], _, _, []).
inst_substitute_alts([Alt0|Alts0], Arg, Param, [Alt|Alts]) :-
	Alt0 = functor(Name, Args0),
	inst_substitute_args(Args0, Arg, Param, Args),
	Alt = functor(Name, Args),
	inst_substitute_alts(Alts0, Arg, Param, Alts).

	% In case we later decided to change the representation
	% of mode_ids.

pred_mode_id_to_int(X, X).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
