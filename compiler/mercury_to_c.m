%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury_to_c.m
% Main author: fjh

% This module is an alternative to the original code generator.
% It generates much higher-level C than the original code generator.

% XXX This is very incomplete!!!
% The only part that works is the generation of function prototypes.

%-----------------------------------------------------------------------------%

:- module mercury_to_c.
:- interface.
:- import_module int, io, hlds, llds.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% print out an entire hlds structure.

:- pred c__gen_hlds(int, module_info, io__state, io__state).
:- mode c__gen_hlds(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module string, map, list, require, std_util, term, term_io, prog_out.
:- import_module mercury_to_mercury, prog_io, globals, options, set, varset.
:- import_module prog_util, mode_util, hlds_out.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% print out an hlds goal.

:- pred c__gen_goal(hlds__goal, module_info, varset, int,
				io__state, io__state).
:- mode c__gen_goal(in, in, in, in, di, uo) is det.

:- type c__gen_info
	--->	c__gen_info(
			code_model,
			module_info,
			pred_id,
			proc_id,
			list(var) % output variables
				% these must be prefixed with `*', since they
				% are pointers
		).

c__gen_hlds(Indent, Module) -->
	c__gen_header(Indent, Module),
	io__write_string("\n"),
	{ module_info_types(Module, TypeTable) },
	c__gen_types(Indent, TypeTable),
	io__write_string("\n"),
	{ module_info_preds(Module, PredTable) },
	c__gen_preds(Indent, Module, PredTable),
	io__write_string("\n"),
	c__gen_footer(Indent, Module).

:- pred c__gen_header(int, module_info, io__state, io__state).
:- mode c__gen_header(in, in, di, uo) is det.

c__gen_header(Indent, Module) -->
	{ module_info_name(Module, Name) },
	c__gen_indent(Indent),
	io__write_string("/* :- module "),
	io__write_string(Name),
	io__write_string(". */\n\n"),
	c__gen_indent(Indent),
	io__write_string("#include ""imp.h""\n\n").

:- pred c__gen_footer(int, module_info, io__state, io__state).
:- mode c__gen_footer(in, in, di, uo) is det.

c__gen_footer(Indent, Module) -->
	{ module_info_name(Module, Name) },
	c__gen_indent(Indent),
	io__write_string("/* :- end_module "),
	io__write_string(Name),
	io__write_string(". */\n").

:- pred c__gen_preds(int, module_info, pred_table,
				io__state, io__state).
:- mode c__gen_preds(in, in, in, di, uo) is det.

c__gen_preds(Indent, ModuleInfo, PredTable) -->
	{ map__keys(PredTable, PredIds) },
	c__gen_preds_2(Indent, ModuleInfo, PredIds, PredTable).

:- pred c__gen_preds_2(int, module_info, list(pred_id), pred_table,
			io__state, io__state).
:- mode c__gen_preds_2(in, in, in, in, di, uo) is det.

c__gen_preds_2(Indent, ModuleInfo, PredIds0, PredTable) --> 
	(
		{ PredIds0 = [PredId|PredIds] }
	->
		{ map__lookup(PredTable, PredId, PredInfo) },
		( { pred_info_is_imported(PredInfo) } ->
			[]
		;
			c__gen_pred(Indent, ModuleInfo, PredId,
				PredInfo)
		),
		c__gen_preds_2(Indent, ModuleInfo, PredIds, PredTable)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred c__gen_pred(int, module_info, pred_id, pred_info,
				io__state, io__state).
:- mode c__gen_pred(in, in, in, in, di, uo) is det.

c__gen_pred(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_arg_types(PredInfo, TVarSet, ArgTypes) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		[]
	;
		c__gen_indent(Indent),
		io__write_string("/****\n"),
		mercury_output_pred_type(TVarSet, unqualified(PredName),
			ArgTypes, no, Context),

		{ pred_info_clauses_info(PredInfo, ClausesInfo) },
		{ ClausesInfo = clauses_info(VarSet, _VarTypes, HeadVars,
			Clauses) },

		globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
		globals__io_set_option(verbose_dump_hlds, bool(no)),
		hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet,
			HeadVars, Clauses),
		globals__io_set_option(verbose_dump_hlds, bool(Verbose)),

		io__write_string("****/\n"),

		c__gen_procs(Indent, ModuleInfo, PredId, PredInfo)
	).

:- pred c__gen_type(type, io__state, io__state).
:- mode c__gen_type(in, di, uo) is det.

c__gen_type(Type) -->
	( { Type = term__functor(term__atom("character"), [], _) } ->
		io__write_string("char")
	; { Type = term__functor(term__atom("int"), [], _) } ->
		io__write_string("int")
	; { Type = term__functor(term__atom("string"), [], _) } ->
		io__write_string("String")
	; { Type = term__functor(term__atom("float"), [], _) } ->
		io__write_string("Float")
	;
		io__write_string("Word")
	).

:- pred c__gen_procs(int, module_info, pred_id, pred_info,
				io__state, io__state).
:- mode c__gen_procs(in, in, in, in, di, uo) is det.

c__gen_procs(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	c__gen_procs_2(ProcIds, ModuleInfo, Indent, PredId, PredInfo).

:- pred c__gen_procs_2(list(proc_id), module_info, int, pred_id,
				pred_info, io__state, io__state).
:- mode c__gen_procs_2(in, in, in, in, in, di, uo) is det.

c__gen_procs_2([], _ModuleInfo, _Indent, _PredId, _PredInfo) -->
	[].
c__gen_procs_2([ProcId | ProcIds], ModuleInfo, Indent, PredId, PredInfo) --> 
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	c__gen_proc(Indent, ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
	c__gen_procs_2(ProcIds, ModuleInfo, Indent, PredId, PredInfo).

:- pred c__gen_proc(int, module_info, pred_id, proc_id, pred_info,
				proc_info, io__state, io__state).
:- mode c__gen_proc(in, in, in, in, in, in, di, uo) is det.

c__gen_proc(Indent, ModuleInfo, PredId, ProcId, Pred, Proc) -->
	{ proc_info_interface_determinism(Proc, InterfaceDeterminism) },
	{ proc_info_variables(Proc, VarSet) },
	{ proc_info_headvars(Proc, HeadVars) },
	{ pred_info_name(Pred, PredName) },
	{ proc_info_vartypes(Proc, VarTypes) },
	{ proc_info_argmodes(Proc, HeadModes) },
	{ proc_info_goal(Proc, Goal) },
	{ proc_info_context(Proc, ModeContext) },
	{ Indent1 is Indent + 1 },

	c__gen_indent(Indent),
	io__write_string("/*\n"),
	c__gen_indent(Indent),
	io__write_string("** "),
	{ varset__init(ModeVarSet) },
	mercury_output_mode_decl(ModeVarSet, unqualified(PredName), 
			HeadModes, yes(InterfaceDeterminism), ModeContext),
	c__gen_indent(Indent),
	io__write_string("*/\n"),

	c__gen_indent(Indent),
	c__gen_prototype(ModuleInfo, PredId, ProcId),
	io__write_string("\n"),
	c__gen_indent(Indent),
	io__write_string("{\n"),
	c__gen_local_var_decls(Indent1, VarSet, VarTypes, HeadVars),
	%%%   c__info_init(ModuleInfo, PredId, ProcId, C_Info),
	io__write_string("/* "),
	c__gen_indent(Indent1),
	%%%   c__gen_goal(Goal, C_Info, Indent1),
	%%%   { determinism_to_code_model(InterfaceDeterminism, CodeModel) },
	c__gen_goal(Goal, ModuleInfo, VarSet, Indent1),
	io__write_string("\n"),
	io__write_string(" */\n"),
	c__gen_indent(Indent),
	io__write_string("}\n").

%-----------------------------------------------------------------------------%

	%
	% generate the function prototype for a procedure
	%
:- pred c__gen_prototype(module_info, pred_id, proc_id, io__state, io__state).
:- mode c__gen_prototype(in, in, in, di, uo) is det.

c__gen_prototype(ModuleInfo, PredId, ProcId) -->
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },

	{ proc_info_interface_determinism(ProcInfo, Detism) },
	{ proc_info_variables(ProcInfo, VarSet) },
	{ proc_info_headvars(ProcInfo, HeadVars) },
	{ pred_info_arg_types(PredInfo, _HeadTypeVarSet, HeadTypes) },
	{ proc_info_argmodes(ProcInfo, HeadModes) },

	( { Detism = semidet } ->
		io__write_string("bool")
	;
		io__write_string("void")
	),
	io__write_string(" "),
	c__gen_proc_name(ModuleInfo, PredId, ProcId),
	io__write_string("("),
	( { HeadVars = [] } ->
		( { Detism = nondet } ->
			io__write_string("Cont cont")
		;
			io__write_string("void")
		)
	;
		c__gen_arg_decls(ModuleInfo, HeadVars, HeadTypes, HeadModes,
			VarSet),
		( { Detism = nondet } ->
			io__write_string(", Cont cont")
		;
			[]
		)
	),
	io__write_string(")").

:- pred c__gen_proc_name(module_info, pred_id, proc_id, io__state, io__state).
:- mode c__gen_proc_name(in, in, in, di, uo) is det.

	% XXX need to handle special_preds
	% Also need to handle main/2 specially
c__gen_proc_name(ModuleInfo, PredId, ProcId) -->
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
	{ llds__name_mangle(PredName, MangledPredName) },
	io__write_string("MP_"),
	io__write_string("_"),
	io__write_string(ModuleName),
	io__write_string("__"),
	io__write_string(MangledPredName),
	io__write_string("_"),
	io__write_int(Arity),
	io__write_string("_"),
        { ModeNum is ProcId mod 10000 },      % strip off the priority
	io__write_int(ModeNum).

:- pred c__gen_arg_decls(module_info, list(var), list(type), list(mode),
			varset, io__state, io__state).
:- mode c__gen_arg_decls(in, in, in, in, in, di, uo) is det.

c__gen_arg_decls(ModuleInfo, HeadVars, HeadTypes, HeadModes, VarSet) -->
	(
		{ HeadVars = [], HeadTypes = [], HeadModes = [] }
	->
		[]
	;	
		{ HeadVars = [Var|Vars] },
		{ HeadTypes = [Type|Types] },
		{ HeadModes = [Mode|Modes] }
	->
		c__gen_arg_decl(ModuleInfo, Var, Type, Mode, VarSet),
		( { Vars \= [] } ->
			io__write_string(", ")
		;
			[]
		),
		c__gen_arg_decls(ModuleInfo, Vars, Types, Modes, VarSet)
	;
		{ error("c__gen_arg_decls: length mismatch") }
	).

:- pred c__gen_arg_decl(module_info, var, type, mode, varset,
			io__state, io__state).
:- mode c__gen_arg_decl(in, in, in, in, in, di, uo) is det.

c__gen_arg_decl(ModuleInfo, Var, Type, Mode, VarSet) -->
	c__gen_type(Type),
	( { mode_is_output(ModuleInfo, Mode) } ->
		io__write_string("* ")
	;
		io__write_string(" ")
	),
	mercury_output_var(Var, VarSet).

:- pred c__gen_local_var_decls(int, varset, map(var, type), list(var),
				io__state, io__state).
:- mode c__gen_local_var_decls(in, in, in, in, di, uo) is det.

c__gen_local_var_decls(Indent, VarSet, VarTypes, HeadVars) -->
	{ varset__vars(VarSet, Vars) },
	{ list__delete_elems(Vars, HeadVars, LocalVars) },
	c__gen_local_var_decls_2(LocalVars, VarSet, VarTypes, Indent).

:- pred c__gen_local_var_decls_2(list(var), varset, map(var, type), int,
				io__state, io__state).
:- mode c__gen_local_var_decls_2(in, in, in, in, di, uo) is det.

c__gen_local_var_decls_2([], _, _, _) --> [].
c__gen_local_var_decls_2([Var|Vars], VarSet, VarTypes, Indent) -->
	c__gen_indent(Indent),
	{ map__lookup(VarTypes, Var, Type) },
	c__gen_type(Type),
	io__write_string(" "),
	mercury_output_var(Var, VarSet),
	io__write_string(";\n"),
	c__gen_local_var_decls_2(Vars, VarSet, VarTypes, Indent).

%-----------------------------------------------------------------------------%

c__gen_goal(_Goal, _ModuleInfo, _VarSet, _Indent) -->
	[].

/***

:- pred c__gen_goal(hlds__goal, module_info, varset, int, code_model,
				io__state, io__state).
:- mode c__gen_goal(in, in, in, in, di, uo) is det.

c__gen_goal(Goal - GoalInfo, ModuleInfo, VarSet, Indent, CodeModel) -->
	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	( { LineNumbers = yes } ->
		{ goal_info_context(GoalInfo, Context) },
		{ term__context_file(Context, FileName) },
		{ term__context_line(Context, LineNumber) },
		( { FileName \= "" } ->
			io__write_string("#line """),
			io__write_string(FileName),
			io__write_string(" "),
			io__write_int(LineNumber),
			mercury_output_newline(Indent)
		;
			[]
		)
	;
		[]
	),
	c__gen_goal_2(Goal, ModuleInfo, VarSet, Indent, CodeModel).

:- pred c__gen_goal_2(hlds__goal_expr, module_info, varset, int, code_model,
					io__state, io__state).
:- mode c__gen_goal_2(in, in, in, in, in, di, uo) is det.

c__gen_goal_2(switch(Var, CanFail, CasesList), ModuleInfo, VarSet, Indent, _)
		-->
	io__write_string("( % "),
	c__gen_can_fail(CanFail), 
	io__write_string(" switch on `"),
	mercury_output_var(Var, VarSet),
	io__write_string("'"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	( { CasesList = [Case | Cases] } ->
		c__gen_case(Case, Var, ModuleInfo, VarSet, Indent1),
		c__gen_cases(Cases, Var, ModuleInfo, VarSet, Indent)
	;
		io__write_string("fail")
	),
	mercury_output_newline(Indent),
	io__write_string(")").

c__gen_goal_2(some(Vars, Goal), ModuleInfo, VarSet, Indent, _) -->
	io__write_string("some ["),
	mercury_output_vars(Vars, VarSet),
	io__write_string("] ("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	c__gen_goal(Goal, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

c__gen_goal_2(if_then_else(Vars, A, B, C), ModuleInfo, VarSet, Indent, _)
		-->
	io__write_string("if"),
	c__gen_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	c__gen_goal(A, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	c__gen_goal(B, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("else"),
	globals__io_lookup_bool_option(verbose_dump_hlds, Verbose),
	(
		{ Verbose = no },
		{ C = if_then_else(_, _, _, _) - _ }
	->
		io__write_string(" "),
		c__gen_goal(C, ModuleInfo, VarSet, Indent)
	;
		mercury_output_newline(Indent1),
		c__gen_goal(C, ModuleInfo, VarSet, Indent1),
		mercury_output_newline(Indent)
	),
	io__write_string(")").

c__gen_goal_2(not(Goal), ModuleInfo, VarSet, Indent, _) -->
	io__write_string("\\+ ("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	c__gen_goal(Goal, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

c__gen_goal_2(conj(List), ModuleInfo, VarSet, Indent, CodeModel) -->
	( 	{ List = [] }
	;	{ List = [Goal | Goals] },
		c_gen_goal(Goal, ModuleInfo, VarSet, Indent, CodeModel),
		c_gen_goal(Goals, ModuleInfo, VarSet, Indent, CodeModel)
	).

c__gen_goal_2(disj(List), ModuleInfo, VarSet, Indent) -->
	( { List = [Goal | Goals] } ->
		io__write_string("( % disjunction"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		c__gen_goal(Goal, ModuleInfo, VarSet, Indent1),
		c__gen_disj(Goals, ModuleInfo, VarSet, Indent),
		mercury_output_newline(Indent),
		io__write_string(")")
	;
		io__write_string("fail")
	).

c__gen_goal_2(call(_PredId, _ProcId, ArgVars, _, _, PredName, _Follow),
					_ModuleInfo, VarSet, _Indent) -->
		% XXX we should print more info here
	(	{ PredName = qualified(ModuleName, Name) }, 
		c__gen_qualified_functor(ModuleName, term__atom(Name), ArgVars, VarSet)
	;
		{ PredName = unqualified(Name) },
		c__gen_functor(term__atom(Name), ArgVars, VarSet)
	).

c__gen_goal_2(unify(A, B, _, Unification, _), ModuleInfo, VarSet,
		Indent) -->
	mercury_output_var(A, VarSet),
	c__gen_unification(Unification),
	c__gen_unify_rhs(B, ModuleInfo, VarSet, Indent).

c__gen_goal_2(pragma_c_code(C_Code, _, _, _, ArgNameMap), _, _, _) -->
	{ map__values(ArgNameMap, Names) },
	io__write_string("$pragma(c_code, ["),
	c__gen_string_list(Names),
	io__write_string("], """),
	io__write_string(C_Code),
	io__write_string(""" )").

:- pred c__gen_string_list(list(string), io__state, io__state).
:- mode c__gen_string_list(in, di, uo) is det.

c__gen_string_list([]) --> [].
c__gen_string_list([Name]) -->
	io__write_string(Name).
c__gen_string_list([Name1, Name2|Names]) -->
	io__write_string(Name1),
	io__write_string(", "),
	c__gen_string_list([Name2|Names]).

:- pred c__gen_unification(unification, io__state, io__state).
:- mode c__gen_unification(in, di, uo) is det.

c__gen_unification(assign(Var1, Var2)) -->
	c__gen_var(Var1),
	io__write_string(" = "),
	c__gen_var(Var2),
	io__write_string(";").
c__gen_unification(simple_test(_, _)) -->
	io__write_string("if (").
	c__gen_var(Var1),
	io__write_string(" == "),
	c__gen_var(Var2),
	io__write_string(") "),
	c__gen__generate_failure. % XXX
c__gen_unification(construct(_, _, _, _)) -->
	io__write_string(" :=: ").
c__gen_unification(deconstruct(_, _, _, _, _)) -->
	io__write_string(" == ").
c__gen_unification(complicated_unify(_, _, _)) -->
	io__write_string(" = ").

:- pred c__gen_var_modes(list(var), list(mode), varset,
					io__state, io__state).
:- mode c__gen_var_modes(in, in, in, di, uo) is det.

c__gen_var_modes([], [], _) --> [].
c__gen_var_modes([Var|Vars], [Mode|Modes], VarSet) -->
	mercury_output_var(Var, VarSet),
	io__write_string("::"),
	mercury_output_mode(Mode, VarSet),
	( { Vars \= [] } ->
		io__write_string(", ")
	;
		[]
	),
	c__gen_var_modes(Vars, Modes, VarSet).
c__gen_var_modes([], [_|_], _) -->
	{ error("c__gen_var_modes: length mis-match") }.
c__gen_var_modes([_|_], [], _) -->
	{ error("c__gen_var_modes: length mis-match") }.

:- pred c__gen_conj(list(hlds__goal), module_info, varset, int,
				io__state, io__state).
:- mode c__gen_conj(in, in, in, in, di, uo) is det.

c__gen_conj(GoalList, ModuleInfo, VarSet, Indent) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		io__write_string(","),
		mercury_output_newline(Indent),
		c__gen_goal(Goal, ModuleInfo, VarSet, Indent),
		c__gen_conj(Goals, ModuleInfo, VarSet, Indent)
	;
		[]
	).

:- pred c__gen_disj(list(hlds__goal), module_info, varset, int,
				io__state, io__state).
:- mode c__gen_disj(in, in, in, in, di, uo) is det.

c__gen_disj(GoalList, ModuleInfo, VarSet, Indent) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		c__gen_goal(Goal, ModuleInfo, VarSet, Indent1),
		c__gen_disj(Goals, ModuleInfo, VarSet, Indent)
	;
		[]
	).

:- pred c__gen_case(case, var, module_info, varset, int,
				io__state, io__state).
:- mode c__gen_case(in, in, in, in, in, di, uo) is det.

c__gen_case(case(ConsId, Goal), Var, ModuleInfo, VarSet, Indent) -->
	mercury_output_var(Var, VarSet),
	io__write_string(" has functor "),
	c__gen_cons_id(ConsId),
	io__write_string(","),
	mercury_output_newline(Indent),
	c__gen_goal(Goal, ModuleInfo, VarSet, Indent).

:- pred c__gen_cases(list(case), var, module_info, varset, int,
				io__state, io__state).
:- mode c__gen_cases(in, in, in, in, in, di, uo) is det.

c__gen_cases(CasesList, Var, ModuleInfo, VarSet, Indent) -->
	(
		{ CasesList = [Case | Cases] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		c__gen_case(Case, Var, ModuleInfo, VarSet, Indent1),
		c__gen_cases(Cases, Var, ModuleInfo, VarSet, Indent)
	;
		[]
	).

***/

:- pred c__gen_var_types(int, varset, map(var, type), varset,
					io__state, io__state).
:- mode c__gen_var_types(in, in, in, in, di, uo) is det.

c__gen_var_types(Indent, VarSet, VarTypes, TVarSet) -->
	{ map__keys(VarTypes, Vars) },
	c__gen_var_types_2(Vars, Indent, VarSet, VarTypes, TVarSet).

:- pred c__gen_var_types_2(list(var), int, varset, map(var, type),
					varset, io__state, io__state).
:- mode c__gen_var_types_2(in, in, in, in, in, di, uo) is det.

c__gen_var_types_2([], _, _, _, _) --> [].
c__gen_var_types_2([Var | Vars], Indent, VarSet, VarTypes, TypeVarSet)
		-->
	{ map__lookup(VarTypes, Var, Type) },
	c__gen_indent(Indent),
	c__gen_type(Type),
	io__write_string(" "),
	mercury_output_var(Var, VarSet),
	io__write_string(";\t/* "),
	mercury_output_term(Type, TypeVarSet),
	io__write_string(" */\n"),
	c__gen_var_types_2(Vars, Indent, VarSet, VarTypes, TypeVarSet).

:- pred c__gen_types(int, type_table, io__state, io__state).
:- mode c__gen_types(in, in, di, uo) is det.

c__gen_types(Indent, _X) -->
	c__gen_indent(Indent),
	io__write_string("/* types */\n").

:- pred c__gen_indent(int, io__state, io__state).
:- mode c__gen_indent(in, di, uo) is det.

c__gen_indent(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		c__gen_indent(Indent1)
	).

:- pred c__gen_failure(c__gen_info, c__gen_info, io__state, io__state).
:- mode c__gen_failure(in, out, di, uo) is det.

c__gen_failure(X, X) --> []. % XXX

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
