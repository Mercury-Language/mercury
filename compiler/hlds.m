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
%
% WARNING: changes here will probably require changes in make_hlds.nl
% and elsewhere.

:- module hlds.
:- interface.
:- import_module int, string, list, set, varset, term, map, prog_io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type module_info	--->	module(
					string,		% module name
					pred_table,
					list(pred_id),
					pred_name_index,
					type_table,
					inst_table,
					mode_table,
					cons_table,
					int,		% number of errors
					int		% number of warnings
				).

%-----------------------------------------------------------------------------%

	% The symbol table for predicates.

:- type pred_info	--->	predicate(
					varset,		% names of _type_ vars
							% in the pred type decl
					list(type),	% argument types
					condition,	% formal specification
							% (not used)

					clauses_info,

					proc_table,

					term__context	% the location (line #)
							% of the :- pred decl.
				).

:- type clauses_info	--->	clauses_info(
					varset,		% variable names
					map(var, type), % variable types
					list(var),	% head vars
					list(clause)
				).

:- type clause		--->	clause(
					list(proc_id),  % modes for which
							% this clause applies
					hlds__goal,	% Body
					term__context
				).

%%% :- export_type proc_table.
:- type proc_table	==	map(proc_id, proc_info).

:- type proc_info	--->	procedure(
					determinism,	% _declared_ determism
					varset,		% variable names
					map(var, type),	% variable types
					list(var),	% head vars
					list(mode), % modes of args
					hlds__goal,	% Body
					term__context,	% The context of
							% the :- mode decl,
							% not the clause.	
					call_info,	% stack allocations
					category	% _inferred_ det'ism
				).

%%% :- export_type category.
:- type category	--->	deterministic		% functional & total
			;	semideterministic	% just functional
			;	nondeterministic.	% neither

%%% :- export_type det_source.
:- type det_source	--->	declared
			;	inferred.

:- type pred_id 	--->	pred(module_name, string, int).
			%	module, predname, arity

%%% :- export_type pred_table.
:- type pred_table	==	map(pred_id, pred_info).

%%% :- export_type pred_name_index.
:- type pred_name_index	==	map(string, list(pred_id)).

:- type procedure_id	--->	proc(pred_id, proc_id).

	% a proc_id is a mode number within a particular predicate -
	% not to be confused with a mode_id, which is the name of a
	% user-defined mode.
:- type proc_id		==	int.

%-----------------------------------------------------------------------------%

	% The symbol table for types.

:- type type_id		== 	pair(sym_name, int).
				% name, arity

%%% :- export_type type_table.
:- type type_table	==	map(type_id, hlds__type_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for modes.

:- type mode_id		==	pair(sym_name, int).
				% name, arity

%%% :- export_type mode_table.
:- type mode_table	==	map(mode_id, hlds__mode_defn).

:- type mode_info	==	map(var, mode).

%-----------------------------------------------------------------------------%

	% The symbol table for insts.

:- type inst_id		==	pair(sym_name, int).
				% name, arity.

%%% :- export_type inst_table.
:- type inst_table	==	map(inst_id, hlds__inst_defn).

%-----------------------------------------------------------------------------%

	% The symbol table for constructors.
	% This table is used by the type-checker to look
	% up the type of functors/constants.

:- type cons_id		--->	cons(string, int).
				% name, arity

%%% :- export_type cons_table.
:- type cons_table	==	map(cons_id, list(hlds__cons_defn)).

%-----------------------------------------------------------------------------%

	% Here's how goals are represented

%%% :- export_type hlds__goal.
:- type hlds__goal		== pair(hlds__goal_expr, hlds__goal_info).

%%% :- export_type hlds__goal_expr.
:- type hlds__goal_expr    	--->	
				% A conjunction
				conj(hlds__goals)

				% Initially only the pred_id and arguments
				% are filled in.  Mode analysis fills in the
				% proc_id.  Just before code generation,
				% we do a pass over the hlds which recognizes
				% the builtins and fills in the is_builtin
				% field.
			;	call(pred_id, proc_id, list(term), is_builtin)

				% Deterministic disjunctions are converted
				% into case statements by the switch
				% detection pass.
				% Variable, functor-args-goal, followvars
			;	switch(var, list(case), follow_vars)

				% Initially only the terms and the context
				% are know.  Mode analysis fills in the
				% missing information.
			;	unify(term, term, unify_mode, unification,
								unify_context)
			;	disj(hlds__goals)
			;	not(list(var), hlds__goal)
			;	all(list(var), hlds__goal)
			;	some(list(var), hlds__goal)
			;	if_then_else(list(var), hlds__goal,
					hlds__goal, hlds__goal).

	% Record whether a call is a builtin or not, and if so, which one.
%%% :- export_type is_builtin.
:- type is_builtin	--->	not_builtin
			;	is_builtin.

%%% :- export_type call_info.
:- type call_info	==	map(var, int).

%%% :- export_type case.
:- type case		--->	case(cons_id, list(var), hlds__goal).
			%	functor to match with, arguments to extract,
			%	goal to execute if match succeeds.

	% Initially all unifications are represented as
	% unify(term, term, _, _), but mode analysis replaces
	% these with various special cases.

%%% :- export_type follow_vars.
:- type follow_vars	==	map(var, register_slot).
%%% :- export_type register_slot.
:- type register_slot		==	int.

%%% :- export_type unification.
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
			;	complicated_unify(unify_mode, term, term).

:- type unify_context	--->	unify_context(
					unify_main_context,
					unify_sub_contexts
				).

:- type unify_main_context --->	explicit
			;	head(int)
			;	call(pred_id, int).

:- type unify_sub_context ==	pair(cons_id, int).

:- type unify_sub_contexts ==	list(unify_sub_context).

%%% :- export_type hlds__goals.
:- type hlds__goals		==	list(hlds__goal).

:- type hlds__goal_info
	---> goalinfo(
		map(var, is_live),	% XXX this is O(N*N)
		determinism,	% the declared determinism
				% (current always unspecified, since
				% there's no way to declare the determinism
				% of a goal.)
		category, 	% the inferred determinism
		instmap_delta,
		term__context,
		set(var)	% the non-local vars in the goal
	).

%%% :- export_type is_live.
:- type is_live		--->	live ; dead.

:- type unify_mode	==	pair(mode, mode).

%-----------------------------------------------------------------------------%

	% This is how type, modes and constructors are represented.
	% The parts that are not defined here (i.e. type_param, constructor,
	% type, inst, mode, condition) are represented in the same way as
	% in prog_io.nl, and are defined there.

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
			;	eqv_type(type)
			;	abstract_type.

:- type hlds__inst_defn --->	hlds__inst_defn(varset, list(inst_param),
					hlds__inst_body,
					condition, term__context).

:- type hlds__inst_body	--->	eqv_inst(inst)
			;	abstract_inst.

:- type hlds__mode_defn --->	hlds__mode_defn(varset, list(inst_param),
					hlds__mode_body,
					condition, term__context).

:- type hlds__mode_body --->	eqv_mode(mode).

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

:- pred moduleinfo_pred_name_index(module_info, pred_name_index).
:- mode moduleinfo_pred_name_index(input, output).

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

:- pred moduleinfo_ctors(module_info, cons_table).
:- mode moduleinfo_ctors(input, output).

:- pred moduleinfo_num_errors(module_info, int).
:- mode moduleinfo_num_errors(input, output).

:- pred moduleinfo_num_warnings(module_info, int).
:- mode moduleinfo_num_warnings(input, output).

:- pred moduleinfo_consids(module_info, list(cons_id)).
:- mode moduleinfo_consids(input, output).

:- pred moduleinfo_set_name(module_info, string, module_info).
:- mode moduleinfo_set_name(input, input, output).

:- pred moduleinfo_set_preds(module_info, pred_table, module_info).
:- mode moduleinfo_set_preds(input, input, output).

:- pred moduleinfo_set_predids(module_info, list(pred_id), module_info).
:- mode moduleinfo_set_predids(input, input, output).

:- pred moduleinfo_set_pred_name_index(module_info, pred_name_index,
					module_info).
:- mode moduleinfo_set_pred_name_index(input, input, output).

:- pred moduleinfo_set_types(module_info, type_table, module_info).
:- mode moduleinfo_set_types(input, input, output).

:- pred moduleinfo_set_insts(module_info, inst_table, module_info).
:- mode moduleinfo_set_insts(input, input, output).

:- pred moduleinfo_set_modes(module_info, mode_table, module_info).
:- mode moduleinfo_set_modes(input, input, output).

:- pred moduleinfo_set_ctors(module_info, cons_table, module_info).
:- mode moduleinfo_set_ctors(input, input, output).

:- pred moduleinfo_incr_errors(module_info, module_info).
:- mode moduleinfo_incr_errors(input, output).

:- pred moduleinfo_incr_warnings(module_info, module_info).
:- mode moduleinfo_incr_warnings(input, output).

%-----------------------------------------------------------------------------%

:- implementation.

	% A predicate which creates an empty module

moduleinfo_init(Name, module(Name, Preds, [], PredNameIndex, Types, Insts,
		Modes, Ctors, 0, 0)) :-
	map__init(Preds),
	map__init(PredNameIndex),
	map__init(Types),
	map__init(Insts),
	map__init(Modes),
	map__init(Ctors).

	% Various access predicates which extract different pieces
	% of info from the module_info data structure.

moduleinfo_name(ModuleInfo, Name) :-
	ModuleInfo = module(Name, _, _, _, _, _, _, _, _, _).

moduleinfo_preds(ModuleInfo, Preds) :-
	ModuleInfo = module(_, Preds, _, _, _, _, _, _, _, _).

moduleinfo_predids(ModuleInfo, PredIDs) :-
	ModuleInfo = module(_, _, PredIDs, _, _, _, _, _, _, _).

moduleinfo_pred_name_index(ModuleInfo, PredNameIndex) :-
	ModuleInfo = module(_, _, _, PredNameIndex, _, _, _, _, _, _).

moduleinfo_types(ModuleInfo, Types) :-
	ModuleInfo = module(_, _, _, _, Types, _, _, _, _, _).

moduleinfo_typeids(ModuleInfo, TypeIDs) :-
	ModuleInfo = module(_, _, _, _, Types, _, _, _, _, _),
	map__keys(Types, TypeIDs).

moduleinfo_insts(ModuleInfo, Insts) :-
	ModuleInfo = module(_, _, _, _, _, Insts, _, _, _, _).

moduleinfo_instids(ModuleInfo, InstIDs) :-
	ModuleInfo = module(_, _, _, _, _, Insts, _, _, _, _),
	map__keys(Insts, InstIDs).

moduleinfo_modes(ModuleInfo, Modes) :-
	ModuleInfo = module(_, _, _, _, _, _, Modes, _, _, _).

moduleinfo_modeids(ModuleInfo, ModeIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, Modes, _, _, _),
	map__keys(Modes, ModeIDs).

moduleinfo_ctors(ModuleInfo, Ctors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, Ctors, _, _).

moduleinfo_consids(ModuleInfo, ConsIDs) :-
	ModuleInfo = module(_, _, _, _, _, _, _, Ctors, _, _),
	map__keys(Ctors, ConsIDs).

moduleinfo_num_errors(ModuleInfo, NumErrors) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, NumErrors, _).

moduleinfo_num_warnings(ModuleInfo, NumWarnings) :-
	ModuleInfo = module(_, _, _, _, _, _, _, _, _, NumWarnings).

	% Various predicates which modify the module_info data structure.

moduleinfo_set_name(ModuleInfo0, Name, ModuleInfo) :-
	ModuleInfo0 = module(_, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_preds(ModuleInfo0, Preds, ModuleInfo) :-
	ModuleInfo0 = module(Name, _, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_predids(ModuleInfo0, PredIDs, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, _, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_pred_name_index(ModuleInfo0, PredNameIndex, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, _, Types,
				Insts, Modes, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_types(ModuleInfo0, Types, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, PredNameIndex, _,
				Insts, Modes, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_insts(ModuleInfo0, Insts, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, PredNameIndex, Types,
				_, Modes, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_modes(ModuleInfo0, Modes, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, _, Ctors, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_set_ctors(ModuleInfo0, Ctors, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, _, Errs, Warns),
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_incr_errors(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs0, Warns),
	Errs is Errs0 + 1,
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

moduleinfo_incr_warnings(ModuleInfo0, ModuleInfo) :-
	ModuleInfo0 = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns0),
	Warns0 is Warns + 1,
	ModuleInfo = module(Name, Preds, PredIDs, PredNameIndex, Types,
				Insts, Modes, Ctors, Errs, Warns).

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

:- pred make_predid(string, sym_name, int, pred_id).
:- mode make_predid(input, input, input, output).


:- pred predinfo_proc_ids(pred_info, list(proc_id)).
:- mode predinfo_proc_ids(input, output).

:- pred predinfo_arg_types(pred_info, varset, list(type)).
:- mode predinfo_arg_types(input, output, output).

:- pred predinfo_clauses_info(pred_info, clauses_info).
:- mode predinfo_clauses_info(input, output).

:- pred predinfo_set_clauses_info(pred_info, clauses_info, pred_info).
:- mode predinfo_set_clauses_info(input, input, output).

:- pred predinfo_procedures(pred_info, proc_table).
:- mode predinfo_procedures(input, output).

:- pred predinfo_set_procedures(pred_info, proc_table, pred_info).
:- mode predinfo_set_procedures(input, input, output).

:- pred predinfo_procids(pred_info, list(proc_id)).
:- mode predinfo_procids(input, output).

:- pred predinfo_context(pred_info, term__context).
:- mode predinfo_context(input, output).

:- pred predinfo_is_imported(pred_info::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

predicate_module(pred(Module,_Name,_Arity), Module).

predicate_name(pred(_Module,Name,_Arity), Name).

predicate_arity(pred(_Module,_Name,Arity), Arity).

make_predid(ModName, unqualified(Name), Arity, pred(ModName, Name, Arity)).
make_predid(_, qualified(ModName, Name), Arity, pred(ModName, Name, Arity)).

predinfo_proc_ids(PredInfo, ProcIds) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, _Clauses, Procs, _),
	map__keys(Procs, ProcIds).

predinfo_clauses_info(PredInfo, Clauses) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, Clauses, _Procs, _).

predinfo_set_clauses_info(PredInfo0, Clauses, PredInfo) :-
	PredInfo0 = predicate(TypeVars, ArgTypes, Cond, _, Procs, C),
	PredInfo = predicate(TypeVars, ArgTypes, Cond, Clauses, Procs, C).

predinfo_arg_types(PredInfo, TypeVars, ArgTypes) :-
	PredInfo = predicate(TypeVars, ArgTypes, _Cond, _Clauses, _Procs, _).

predinfo_procedures(PredInfo, Procs) :-
	PredInfo = predicate(_TypeVars, _ArgTypes, _Cond, _Clauses, Procs, _).

predinfo_set_procedures(PredInfo0, Procedures, PredInfo) :-
	PredInfo0 = predicate(A, B, C, D, _, F),
	PredInfo = predicate(A, B, C, D, Procedures, F).

predinfo_procids(PredInfo, ProcIds) :-
	predinfo_procedures(PredInfo, Procedures),
	map__keys(Procedures, ProcIds).

predinfo_context(PredInfo, Context) :-
	PredInfo = predicate(_, _, _, _, _, Context).

predinfo_is_imported(PredInfo) :-
	predinfo_clauses_info(PredInfo, ClauseInfo),
	ClauseInfo = clauses_info(_, _, _, []).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates for accessing the proc_info data structure.

:- interface.

:- pred procinfo_init(list(mode), determinism, term__context, proc_info).
:- mode procinfo_init(input, input, input, output).

:- pred determinism_to_category(determinism, category).
:- mode determinism_to_category(input, output).

:- pred procinfo_declared_determinism(proc_info, determinism).
:- mode procinfo_declared_determinism(input, output).

:- pred procinfo_inferred_determinism(proc_info, category).
:- mode procinfo_inferred_determinism(input, output).

:- pred procinfo_variables(proc_info, varset).
:- mode procinfo_variables(input, output).

:- pred procinfo_vartypes(proc_info, map(var, type)).
:- mode procinfo_vartypes(input, output).

:- pred procinfo_headvars(proc_info, list(var)).
:- mode procinfo_headvars(input, output).

:- pred procinfo_argmodes(proc_info, list(mode)).
:- mode procinfo_argmodes(input, output).

:- pred procinfo_goal(proc_info, hlds__goal).
:- mode procinfo_goal(input, output).

:- pred procinfo_context(proc_info, term__context).
:- mode procinfo_context(input, output).

:- pred procinfo_callinfo(proc_info, call_info).
:- mode procinfo_callinfo(input, output).

:- pred procinfo_arg_registers(proc_info, list(var), map(var, int)).
:- mode procinfo_arg_registers(input, input, output).

:- pred procinfo_set_inferred_determinism(proc_info, category, proc_info).
:- mode procinfo_set_inferred_determinism(input, input, output).

:- pred procinfo_set_goal(proc_info, hlds__goal, proc_info).
:- mode procinfo_set_goal(input, input, output).

:- implementation.

	% Some parts of the procedure aren't known yet.  We initialize
	% them to any old garbage which we will later throw away

procinfo_init(Modes, Det, MContext, NewProc) :-
	map__init(BodyTypes),
	goalinfo_init(GoalInfo),
	varset__init(BodyVarSet),
	HeadVars = [],
	determinism_to_category(Det, Category),
	map__init(CallInfo),
	NewProc = procedure(
		Det, BodyVarSet, BodyTypes, HeadVars, Modes,
		conj([]) - GoalInfo, MContext, CallInfo, Category
	).

determinism_to_category(det, deterministic).
determinism_to_category(semidet, semideterministic).
determinism_to_category(nondet, nondeterministic).
	% The inferred determinism gets initialized to `deterministic'.
	% This is what `det_analysis.nl' wants.  If it turns out
	% that the procedure wasn't deterministic, then det_analysis.nl
	% provide the correct inferred determinism for it.
determinism_to_category(unspecified, deterministic).

procinfo_declared_determinism(ProcInfo, Determinism) :-
	ProcInfo = procedure(Determinism, _, _, _, _, _, _, _, _).
procinfo_variables(ProcInfo, VarSet) :-
	ProcInfo = procedure(_, VarSet, _, _, _, _, _, _, _).
procinfo_vartypes(ProcInfo, VarTypes) :-
	ProcInfo = procedure(_, _, VarTypes, _, _, _, _, _, _).
procinfo_headvars(ProcInfo, HeadVars) :-
	ProcInfo = procedure(_, _, _, HeadVars, _, _, _, _, _).
procinfo_argmodes(ProcInfo, ModeInfo) :-
	ProcInfo = procedure(_, _, _, _, ModeInfo, _, _, _, _).
procinfo_goal(ProcInfo, Goal) :-
	ProcInfo = procedure(_, _, _, _, _, Goal, _, _, _).
procinfo_context(ProcInfo, Context) :-
	ProcInfo = procedure(_, _, _, _, _, _, Context, _, _).
procinfo_callinfo(ProcInfo, CallInfo) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, CallInfo, _).
procinfo_inferred_determinism(ProcInfo, Category) :-
	ProcInfo = procedure(_, _, _, _, _, _, _, _, Category).

procinfo_set_inferred_determinism(ProcInfo0, Category, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, F, G, H, _),
	ProcInfo = procedure(A, B, C, D, E, F, G, H, Category).

procinfo_set_goal(ProcInfo0, Goal, ProcInfo) :-
	ProcInfo0 = procedure(A, B, C, D, E, _, G, H, I),
	ProcInfo = procedure(A, B, C, D, E, Goal, G, H, I).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Access predicates for the hlds__goal_info data structure.

:- interface.

:- pred goalinfo_init(hlds__goal_info).
:- mode goalinfo_init(output).

:- type liveness == map(var, is_live).

:- pred goalinfo_liveness(hlds__goal_info, liveness).
:- mode goalinfo_liveness(input, output).

:- pred goalinfo_set_liveness(hlds__goal_info, liveness, hlds__goal_info).
:- mode goalinfo_set_liveness(input, input, output).

:- pred goalinfo_declared_determinism(hlds__goal_info, determinism).
:- mode goalinfo_declared_determinism(input, output).

:- pred goalinfo_inferred_determinism(hlds__goal_info, category).
:- mode goalinfo_inferred_determinism(input, output).

:- pred goalinfo_set_inferred_determinism(hlds__goal_info, category,
					  hlds__goal_info).
:- mode goalinfo_set_inferred_determinism(input, input, output).

:- pred goalinfo_get_nonlocals(hlds__goal_info, set(var)).
:- mode goalinfo_get_nonlocals(input, output).

:- pred goalinfo_set_nonlocals(hlds__goal_info, set(var), hlds__goal_info).
:- mode goalinfo_set_nonlocals(input, input, output).

	% The instmap delta stores the final instantiatedness
	% of the non-local variables whose instantiatedness
	% changed.

:- type instmap_delta == assoc_list(var, inst).

:- pred goalinfo_get_instmap_delta(hlds__goal_info, instmap_delta).
:- mode goalinfo_get_instmap_delta(input, output).

:- pred goalinfo_set_instmap_delta(hlds__goal_info, instmap_delta,
				hlds__goal_info).
:- mode goalinfo_set_instmap_delta(input, input, output).

/***** currently not used and not implemented
:- pred liveness_livevars(map(var, is_live), list(var)).
:- mode liveness_livevars(input, output).
*****/

:- pred goalinfo_context(hlds__goal_info, term__context).
:- mode goalinfo_context(input, output).

:- pred goalinfo_set_context(hlds__goal_info, term__context, hlds__goal_info).
:- mode goalinfo_set_context(input, input, output).

%-----------------------------------------------------------------------------%

:- implementation.

goalinfo_init(GoalInfo) :-
	DeclaredDet = unspecified,
	InferredDet = nondeterministic, 
	map__init(Liveness),
	InstMapDelta = [],
	set__init(NonLocals),
	term__context_init("", 0, Context),
	GoalInfo = goalinfo(Liveness, DeclaredDet, InferredDet,
				InstMapDelta, Context, NonLocals).

goalinfo_liveness(GoalInfo, Liveness) :-
	GoalInfo = goalinfo(Liveness, _, _, _, _, _).

goalinfo_set_liveness(GoalInfo0, Liveness, GoalInfo) :-
	GoalInfo0 = goalinfo(_, B, C, D, E, F),
	GoalInfo = goalinfo(Liveness, B, C, D, E, F).

goalinfo_declared_determinism(GoalInfo, DeclaredDeterminism) :-
	GoalInfo = goalinfo(_, DeclaredDeterminism, _, _, _, _).

goalinfo_inferred_determinism(GoalInfo, InferredDeterminism) :-
	GoalInfo = goalinfo(_, _, InferredDeterminism, _, _, _).

goalinfo_set_inferred_determinism(GoalInfo0, InferredDeterminism, GoalInfo) :-
	GoalInfo0 = goalinfo(A, B, _, D, E, F),
	GoalInfo = goalinfo(A, B, InferredDeterminism, D, E, F).

goalinfo_get_instmap_delta(GoalInfo, InstMapDelta) :-
	GoalInfo = goalinfo(_, _, _, InstMapDelta, _, _).

goalinfo_set_instmap_delta(GoalInfo0, InstMapDelta, GoalInfo) :-
	GoalInfo0 = goalinfo(A, B, C, _, E, F),
	GoalInfo = goalinfo(A, B, C, InstMapDelta, E, F).

/*** This is a specification, not an implementation.
     It's not mode-correct.
liveness_livevars(Liveness, LiveVars) :-
	solutions(X, map__search(Liveness, X, live), LiveVars).
***/

goalinfo_context(GoalInfo, Context) :-
	GoalInfo = goalinfo(_, _, _, _, Context, _).

goalinfo_set_context(GoalInfo0, Context, GoalInfo) :-
	GoalInfo0 = goalinfo(A, B, C, D, _, F),
	GoalInfo = goalinfo(A, B, C, D, Context, F).

goalinfo_get_nonlocals(GoalInfo, NonLocals) :-
	GoalInfo = goalinfo(_, _, _, _, _, NonLocals).

goalinfo_set_nonlocals(GoalInfo0, NonLocals, GoalInfo) :-
	GoalInfo0 = goalinfo(A, B, C, D, E, _),
	GoalInfo  = goalinfo(A, B, C, D, E, NonLocals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Various predicates which are used by codegen to
	% access the mode data structure.

:- interface.

	% XXX how should these predicates handle abstract insts?
	% In that case, we don't _know_ whether the mode is input
	% or not!

:- pred mode_is_input(module_info, mode).
:- mode mode_is_input(input, input) is semidet.

:- pred mode_is_output(module_info, mode).
:- mode mode_is_output(input, input) is semidet.

:- pred mode_id_to_int(mode_id, int).
:- mode mode_id_to_int(input, output) is det.

:- pred mode_list_get_final_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_final_insts(input, input, output) is det.

:- pred mode_list_get_initial_insts(list(mode), module_info, list(inst)).
:- mode mode_list_get_initial_insts(input, input, output) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

mode_list_get_final_insts([], _ModuleInfo, []).
mode_list_get_final_insts([Mode | Modes], ModuleInfo, [Inst | Insts]) :-
	mode_get_insts(ModuleInfo, Mode, _, Inst),
	mode_list_get_final_insts(Modes, ModuleInfo, Insts).

mode_list_get_initial_insts([], _ModuleInfo, []).
mode_list_get_initial_insts([Mode | Modes], ModuleInfo, [Inst | Insts]) :-
	mode_get_insts(ModuleInfo, Mode, Inst, _),
	mode_list_get_initial_insts(Modes, ModuleInfo, Insts).

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

:- inst_is_free(_, X) when X.		% NU-Prolog indexing.

inst_is_free(_, free).
inst_is_free(_, inst_var(_)) :-
	error("internal error: uninstantiated inst parameter").
inst_is_free(ModuleInfo, user_defined_inst(Name, Args)) :-
	inst_lookup(ModuleInfo, Name, Args, Inst),
	inst_is_free(ModuleInfo, Inst).
inst_is_free(_, abstract_inst(_, _)) :-
	error("internal error: abstract insts not yet implemented").

	% inst_is_bound succeeds iff the inst passed is not `free'
	% or is a user-defined inst which is not defined as `free'.

:- pred inst_is_bound(module_info, inst).
:- mode inst_is_bound(input, input).

:- inst_is_bound(_, X) when X.		% NU-Prolog indexing.

inst_is_bound(_, ground).
inst_is_bound(_, bound(_)).
inst_is_bound(_, inst_var(_)) :-
	error("internal error: uninstantiated inst parameter").
inst_is_bound(ModuleInfo, user_defined_inst(Name, Args)) :-
	inst_lookup(ModuleInfo, Name, Args, Inst),
	inst_is_bound(ModuleInfo, Inst).
inst_is_bound(_, abstract_inst(_, _)) :-
	error("internal error: abstract insts not yet implemented").

:- pred inst_lookup(module_info, sym_name, list(inst), inst).
:- mode inst_lookup(input, input, input, output).

inst_lookup(ModuleInfo, Name, Args, Inst) :-
	length(Args, Arity),
	moduleinfo_insts(ModuleInfo, Insts),
	map__search(Insts, Name - Arity, InstDefn),
	InstDefn = hlds__inst_defn(_VarSet, Params, Inst0, _Cond, _Context),
	inst_lookup_2(Inst0, Params, Name, Args, Inst).

:- pred inst_lookup_2(hlds__inst_body, list(inst_param), sym_name, list(inst),
			inst).
:- mode inst_lookup_2(input, input, input, input, output).

inst_lookup_2(eqv_inst(Inst0), Params, _Name, Args, Inst) :-
	inst_substitute_arg_list(Inst0, Params, Args, Inst).
inst_lookup_2(abstract_inst, _Params, Name, Args,
		abstract_inst(Name, Args)).

	% mode_get_insts returns the initial instantiatedness and
	% the final instantiatedness for a given mode.

:- pred mode_get_insts(module_info, mode, inst, inst).
:- mode mode_get_insts(input, input, output, output).

mode_get_insts(_ModuleInfo, InitialInst -> FinalInst, InitialInst, FinalInst).
mode_get_insts(ModuleInfo, user_defined_mode(Name, Args), Initial, Final) :-
	length(Args, Arity),
	moduleinfo_modes(ModuleInfo, Modes),
	map__search(Modes, Name - Arity, HLDS_Mode),
	HLDS_Mode = hlds__mode_defn(_VarSet, Params, ModeDefn, _Cond, _Context),
	ModeDefn = eqv_mode(Mode0),
	mode_substitute_arg_list(Mode0, Params, Args, Mode),
	mode_get_insts(ModuleInfo, Mode, Initial, Final).

	% mode_substitute_arg_list(Mode0, Params, Args, Mode) is true
	% iff Mode is the mode that results from substituting all
	% occurrences of Params in Mode0 with the corresponding
	% value in Args.

:- pred mode_substitute_arg_list(mode, list(inst_param), list(inst), mode).
:- mode mode_substitute_arg_list(input, input, input, output).

mode_substitute_arg_list(Mode0, Params, Args, Mode) :-
	( Params = [] ->
		Mode = Mode0	% optimize common case
	;
		map__from_corresponding_lists(Params, Args, Subst),
		mode_apply_substitution(Mode0, Subst, Mode)
	).
		
	% inst_substitute_arg_list(Inst0, Params, Args, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Params in Inst0 with the corresponding
	% value in Args.

:- pred inst_substitute_arg_list(inst, list(inst_param), list(inst), inst).
:- mode inst_substitute_arg_list(input, input, input, output).

inst_substitute_arg_list(Inst0, Params, Args, Inst) :-
	( Params = [] ->
		Inst = Inst0	% optimize common case
	;
		map__from_corresponding_lists(Params, Args, Subst),
		inst_apply_substitution(Inst0, Subst, Inst)
	).

	% mode_apply_substitution(Mode0, Subst, Mode) is true iff
	% Mode is the mode that results from apply Subst to Mode0.

:- type inst_subst == map(inst_param, inst).

:- pred mode_apply_substitution(mode, inst_subst, mode).
:- mode mode_apply_substitution(input, input, output).

mode_apply_substitution(I0 -> F0, Subst, I -> F) :-
	inst_apply_substitution(I0, Subst, I),
	inst_apply_substitution(F0, Subst, F).
mode_apply_substitution(user_defined_mode(Name, Args0), Subst,
		    user_defined_mode(Name, Args)) :-
	inst_list_apply_substitution(Args0, Subst, Args).

	% inst_list_apply_substitution(Insts0, Subst, Insts) is true
	% iff Inst is the inst that results from applying Subst to Insts0.

:- pred inst_list_apply_substitution(list(inst), inst_subst, list(inst)).
:- mode inst_list_apply_substitution(input, input, output).

inst_list_apply_substitution([], _, []).
inst_list_apply_substitution([A0 | As0], Subst, [A | As]) :-
	inst_apply_substitution(A0, Subst, A),
	inst_list_apply_substitution(As0, Subst, As).

	% inst_substitute_arg(Inst0, Subst, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Param in Inst0 with Arg.

:- pred inst_apply_substitution(inst, inst_subst, inst).
:- mode inst_apply_substitution(input, input, output).

inst_apply_substitution(free, _, free).
inst_apply_substitution(ground, _, ground).
inst_apply_substitution(bound(Alts0), Subst, bound(Alts)) :-
	alt_list_apply_substitution(Alts0, Subst, Alts).
inst_apply_substitution(inst_var(Var), Subst, Result) :-
	(if some [Replacement]
		% XXX should params be vars?
		map__search(Subst, term_variable(Var), Replacement)
	then
		Result = Replacement
	else
		Result = inst_var(Var)
	).
inst_apply_substitution(user_defined_inst(Name, Args0), Subst,
		    user_defined_inst(Name, Args)) :-
	inst_list_apply_substitution(Args0, Subst, Args).
inst_apply_substitution(abstract_inst(Name, Args0), Subst,
		    abstract_inst(Name, Args)) :-
	inst_list_apply_substitution(Args0, Subst, Args).

:- pred alt_list_apply_substitution(list(bound_inst), inst_subst,
				list(bound_inst)).
:- mode alt_list_apply_substitution(input, input, output).

alt_list_apply_substitution([], _, []).
alt_list_apply_substitution([Alt0|Alts0], Subst, [Alt|Alts]) :-
	Alt0 = functor(Name, Args0),
	inst_list_apply_substitution(Args0, Subst, Args),
	Alt = functor(Name, Args),
	alt_list_apply_substitution(Alts0, Subst, Alts).

%-----------------------------------------------------------------------------%

	% In case we later decided to change the representation
	% of mode_ids.

mode_id_to_int(_ - X, X).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
