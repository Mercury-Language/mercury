%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury_to_c.m
% Main author: fjh

% This module is an alternative to the original code generator.
% It generates much higher-level C than the original code generator.

% XXX This is still very incomplete!!!
% Done:
%	- function prototypes
%	- code generation for det & semidet predicates:
%		- conjunctions
%		- negation
%		- if-then-else
%		- predicate calls
%		- unifications
%			- assignment
%			- simple test on integers
% TODO:
%	Everything else, including
%	- code generation for nondet predicates
%	- disjunctions
%	- switches
%	- c_code pragmas
%	- construct/deconstruct/complicated unifications
%	- calls to builtin predicates
%	- type declarations for user-defined types

%-----------------------------------------------------------------------------%

:- module mercury_to_c.

:- interface.

:- import_module hlds_module.
:- import_module io.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% print out an entire hlds structure.

:- pred mercury_to_c__gen_hlds(int, module_info, io__state, io__state).
:- mode mercury_to_c__gen_hlds(in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, prog_data.
:- import_module llds, llds_out, prog_out, prog_io, mercury_to_mercury.
:- import_module prog_util, mode_util, hlds_out, stack, quantification.
:- import_module globals, options.
:- import_module string, map, list, require, std_util, term, term_io, getopt.
:- import_module bool, set, varset, int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type c_gen_info
	--->	c_gen_info(
			code_model,
			module_info,
			pred_id,
			proc_id,
			varset,
			list(var), % output variables
				% these must be prefixed with `*', since they
				% are pointers
			stack(c_failure_cont),
			c_label,	% label counter
			c_label_func	% label function counter
		).

:- type c_failure_cont
	--->	semidet_fail	% `return FALSE'
	;	nondet_fail	% `return'
	;	goto(c_label)	% `goto Label'
	;	call(c_label_func).	% `LabelFunc()'

:- type c_label == int.		% A number corresponding to a C label
:- type c_label_func == int.	% A number corresponding to a GNU C
				% nested function which serves as a label

:- type c_success_cont
	--->	semidet_succeed	% `return TRUE'
	;	nondet_succeed	% `cont()'
	;	goto(c_label).	% `goto Label'

mercury_to_c__gen_hlds(Indent, Module) -->
	c_gen_header(Indent, Module),
	io__write_string("\n"),
	{ module_info_types(Module, TypeTable) },
	c_gen_types(Indent, TypeTable),
	io__write_string("\n"),
	{ module_info_preds(Module, PredTable) },
	c_gen_preds(Indent, Module, PredTable),
	io__write_string("\n"),
	c_gen_footer(Indent, Module).

:- pred c_gen_header(int, module_info, io__state, io__state).
:- mode c_gen_header(in, in, di, uo) is det.

c_gen_header(Indent, Module) -->
	{ module_info_name(Module, Name) },
	c_gen_indent(Indent),
	io__write_string("/* :- module "),
	io__write_string(Name),
	io__write_string(". */\n\n"),
	c_gen_indent(Indent),
	io__write_string("#include ""imp.h""\n\n").

:- pred c_gen_footer(int, module_info, io__state, io__state).
:- mode c_gen_footer(in, in, di, uo) is det.

c_gen_footer(Indent, Module) -->
	{ module_info_name(Module, Name) },
	c_gen_indent(Indent),
	io__write_string("/* :- end_module "),
	io__write_string(Name),
	io__write_string(". */\n").

:- pred c_gen_preds(int, module_info, pred_table,
				io__state, io__state).
:- mode c_gen_preds(in, in, in, di, uo) is det.

c_gen_preds(Indent, ModuleInfo, PredTable) -->
	{ map__keys(PredTable, PredIds) },
	c_gen_preds_2(Indent, ModuleInfo, PredIds, PredTable).

:- pred c_gen_preds_2(int, module_info, list(pred_id), pred_table,
			io__state, io__state).
:- mode c_gen_preds_2(in, in, in, in, di, uo) is det.

c_gen_preds_2(Indent, ModuleInfo, PredIds0, PredTable) --> 
	(
		{ PredIds0 = [PredId|PredIds] }
	->
		{ map__lookup(PredTable, PredId, PredInfo) },
		( { pred_info_is_imported(PredInfo) } ->
			[]
		;
			c_gen_pred(Indent, ModuleInfo, PredId,
				PredInfo)
		),
		c_gen_preds_2(Indent, ModuleInfo, PredIds, PredTable)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred c_gen_pred(int, module_info, pred_id, pred_info,
				io__state, io__state).
:- mode c_gen_pred(in, in, in, in, di, uo) is det.

c_gen_pred(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_arg_types(PredInfo, TVarSet, ArgTypes) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		[]
	;
		c_gen_indent(Indent),
		io__write_string("/****\n"),
		mercury_output_pred_type(TVarSet, unqualified(PredName),
			ArgTypes, no, Context),

		{ pred_info_clauses_info(PredInfo, ClausesInfo) },
		{ ClausesInfo = clauses_info(VarSet, _VarTypes, _, HeadVars,
			Clauses) },

		globals__io_lookup_string_option(verbose_dump_hlds, Verbose),
		globals__io_set_option(verbose_dump_hlds, string("")),
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet, no,
			HeadVars, PredOrFunc, Clauses, no),
		globals__io_set_option(verbose_dump_hlds, string(Verbose)),

		io__write_string("****/\n"),

		c_gen_procs(Indent, ModuleInfo, PredId, PredInfo)
	).

:- pred c_gen_type(type, io__state, io__state).
:- mode c_gen_type(in, di, uo) is det.

c_gen_type(Type) -->
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

:- pred c_gen_procs(int, module_info, pred_id, pred_info,
				io__state, io__state).
:- mode c_gen_procs(in, in, in, in, di, uo) is det.

c_gen_procs(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	c_gen_procs_2(ProcIds, ModuleInfo, Indent, PredId, PredInfo).

:- pred c_gen_procs_2(list(proc_id), module_info, int, pred_id,
				pred_info, io__state, io__state).
:- mode c_gen_procs_2(in, in, in, in, in, di, uo) is det.

c_gen_procs_2([], _ModuleInfo, _Indent, _PredId, _PredInfo) -->
	[].
c_gen_procs_2([ProcId | ProcIds], ModuleInfo, Indent, PredId, PredInfo) --> 
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	c_gen_proc(Indent, ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
	c_gen_procs_2(ProcIds, ModuleInfo, Indent, PredId, PredInfo).

:- pred c_gen_proc(int, module_info, pred_id, proc_id, pred_info,
				proc_info, io__state, io__state).
:- mode c_gen_proc(in, in, in, in, in, in, di, uo) is det.

c_gen_proc(Indent, ModuleInfo, PredId, ProcId, Pred, Proc) -->
	{ proc_info_interface_determinism(Proc, InterfaceDeterminism) },
	{ proc_info_variables(Proc, VarSet) },
	{ proc_info_headvars(Proc, HeadVars) },
	{ pred_info_name(Pred, PredName) },
	{ proc_info_vartypes(Proc, VarTypes) },
	{ proc_info_argmodes(Proc, HeadModes) },
	{ proc_info_goal(Proc, Goal) },
	{ proc_info_context(Proc, ModeContext) },
	{ Indent1 is Indent + 1 },

	c_gen_indent(Indent),
	io__write_string("/*\n"),
	c_gen_indent(Indent),
	io__write_string("** "),
	{ varset__init(ModeVarSet) },
	mercury_output_pred_mode_decl(ModeVarSet, unqualified(PredName), 
			HeadModes, yes(InterfaceDeterminism), ModeContext),
	c_gen_indent(Indent),
	io__write_string("*/\n"),

	c_gen_indent(Indent),
	c_gen_prototype(ModuleInfo, PredId, ProcId),
	io__write_string("\n"),
	c_gen_indent(Indent),
	io__write_string("{\n"),
	c_gen_local_var_decls(Indent1, Goal, VarSet, VarTypes, HeadVars),
	io__write_string("\n"),
	{ determinism_to_code_model(InterfaceDeterminism, CodeModel) },
	{ c_gen_select_output_vars(ModuleInfo, HeadVars, HeadModes,
		OutputVars) },
	{ c_gen_info_init(ModuleInfo, PredId, ProcId, VarSet, CodeModel,
		OutputVars, CGenInfo0) },
	( { CodeModel = model_non } ->
		c_gen_predeclare_labels(Goal, CGenInfo0),
		c_gen_indent(Indent1),
		io__write_string("void MNL_0(void) {\n")
	;
		[]
	),
	c_gen_goal(Goal, Indent1, CGenInfo0, _CGenInfo),
	( { CodeModel = model_non } ->
		{ Indent2 is Indent1 + 1 },
		c_gen_indent(Indent2),
		io__write_string("cont();\n"),
		c_gen_indent(Indent1),
		io__write_string("}\n\n"),
		c_gen_indent(Indent1),
		io__write_string("MNL_0();\n")
	; { CodeModel = model_semi } ->
		c_gen_indent(Indent1),
		io__write_string("return TRUE;\n")
	;
		[]
	),
	c_gen_indent(Indent),
	io__write_string("}\n").

%-----------------------------------------------------------------------------%

:- pred c_gen_predeclare_labels(hlds_goal, c_gen_info, io__state, io__state).
:- mode c_gen_predeclare_labels(in, in, di, uo) is det.

	% XXX this should traverse the goal and count how many function labels
	% will be needed.

c_gen_predeclare_labels(_Goal, CGenInfo) -->
	c_gen_predeclare_label(1, CGenInfo),
	c_gen_predeclare_label(2, CGenInfo),
	c_gen_predeclare_label(3, CGenInfo),
	c_gen_predeclare_label(4, CGenInfo),
	c_gen_predeclare_label(5, CGenInfo),
	c_gen_predeclare_label(6, CGenInfo),
	c_gen_predeclare_label(7, CGenInfo),
	c_gen_predeclare_label(8, CGenInfo),
	c_gen_predeclare_label(9, CGenInfo),
	c_gen_predeclare_label(10, CGenInfo),
	c_gen_predeclare_label(11, CGenInfo),
	c_gen_predeclare_label(12, CGenInfo).	
	% XXX
	
:- pred c_gen_predeclare_label(int, c_gen_info, io__state, io__state).
:- mode c_gen_predeclare_label(in, in, di, uo) is det.

c_gen_predeclare_label(Label, CGenInfo) -->
	io__write_string("\tauto void "),
	c_gen_write_label_func(Label, CGenInfo),
	io__write_string("(void);\n").

:- pred c_gen_write_label_func(int, c_gen_info, io__state, io__state).
:- mode c_gen_write_label_func(in, in, di, uo) is det.

c_gen_write_label_func(Label, _CGenInfo) -->
	io__write_string("MNL_"),
	io__write_int(Label).

:- pred c_gen_insert_label_func(c_label_func, int, c_gen_info,
				io__state, io__state).
:- mode c_gen_insert_label_func(in, in, in, di, uo) is det.

c_gen_insert_label_func(Label, Indent, CGenInfo) -->
	c_gen_indent(Indent),
	io__write_string("}\n"),
	c_gen_indent(Indent),
	io__write_string("void "),
	c_gen_write_label_func(Label, CGenInfo),
	io__write_string("(void) {\n").

	%
	% generate the function prototype for a procedure
	%
:- pred c_gen_prototype(module_info, pred_id, proc_id, io__state, io__state).
:- mode c_gen_prototype(in, in, in, di, uo) is det.

c_gen_prototype(ModuleInfo, PredId, ProcId) -->
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		PredInfo, ProcInfo) },

	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
	{ proc_info_variables(ProcInfo, VarSet) },
	{ proc_info_headvars(ProcInfo, HeadVars) },
	{ pred_info_arg_types(PredInfo, _HeadTypeVarSet, HeadTypes) },
	{ proc_info_argmodes(ProcInfo, HeadModes) },

	( { CodeModel = model_semi } ->
		io__write_string("bool")
	;
		io__write_string("void")
	),
	io__write_string(" "),
	c_gen_proc_name(ModuleInfo, PredId, ProcId),
	io__write_string("("),
	( { HeadVars = [] } ->
		( { CodeModel = model_non } ->
			io__write_string("Cont cont")
		;
			io__write_string("void")
		)
	;
		c_gen_arg_decls(ModuleInfo, HeadVars, HeadTypes, HeadModes,
			VarSet),
		( { CodeModel = model_non } ->
			io__write_string(", Cont cont")
		;
			[]
		)
	),
	io__write_string(")").

:- pred c_gen_proc_name(module_info, pred_id, proc_id, io__state, io__state).
:- mode c_gen_proc_name(in, in, in, di, uo) is det.

	% XXX need to handle special_preds
	% Also need to handle main/2 specially
c_gen_proc_name(ModuleInfo, PredId, ProcId) -->
	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
	{ llds_out__name_mangle(PredName, MangledPredName) },
	io__write_string("MP_"),
	io__write_string("_"),
	io__write_string(ModuleName),
	io__write_string("__"),
	io__write_string(MangledPredName),
	io__write_string("_"),
	io__write_int(Arity),
	io__write_string("_"),
	{ proc_id_to_int(ProcId, ProcInt) },
	{ ModeNum is ProcInt mod 10000 },	% strip off the priority
	io__write_int(ModeNum).

:- pred c_gen_select_output_vars(module_info, list(var), list(mode), list(var)).
:- mode c_gen_select_output_vars(in, in, in, out) is det.

c_gen_select_output_vars(ModuleInfo, HeadVars, HeadModes, OutputVars) :-
	(
		HeadVars = [], HeadModes = []
	->
		OutputVars = []
	;	
		HeadVars = [Var|Vars],
		HeadModes = [Mode|Modes]
	->
		( mode_is_output(ModuleInfo, Mode) ->
			OutputVars = [Var|OutputVars1],
			c_gen_select_output_vars(ModuleInfo, Vars, Modes,
				OutputVars1)
		;
			c_gen_select_output_vars(ModuleInfo, Vars, Modes,
				OutputVars)
		)
	;
		error("c_gen_select_output_vars: length mismatch")
	).

:- pred c_gen_arg_decls(module_info, list(var), list(type), list(mode),
			varset, io__state, io__state).
:- mode c_gen_arg_decls(in, in, in, in, in, di, uo) is det.

c_gen_arg_decls(ModuleInfo, HeadVars, HeadTypes, HeadModes, VarSet) -->
	(
		{ HeadVars = [], HeadTypes = [], HeadModes = [] }
	->
		[]
	;	
		{ HeadVars = [Var|Vars] },
		{ HeadTypes = [Type|Types] },
		{ HeadModes = [Mode|Modes] }
	->
		c_gen_arg_decl(ModuleInfo, Var, Type, Mode, VarSet),
		( { Vars \= [] } ->
			io__write_string(", ")
		;
			[]
		),
		c_gen_arg_decls(ModuleInfo, Vars, Types, Modes, VarSet)
	;
		{ error("c_gen_arg_decls: length mismatch") }
	).

:- pred c_gen_arg_decl(module_info, var, type, mode, varset,
			io__state, io__state).
:- mode c_gen_arg_decl(in, in, in, in, in, di, uo) is det.

c_gen_arg_decl(ModuleInfo, Var, Type, Mode, VarSet) -->
	c_gen_type(Type),
	( { mode_is_output(ModuleInfo, Mode) } ->
		io__write_string("* ")
	;
		io__write_string(" ")
	),
	mercury_output_var(Var, VarSet, no).

:- pred c_gen_local_var_decls(int, hlds_goal, varset, map(var, type),
				list(var),
				io__state, io__state).
:- mode c_gen_local_var_decls(in, in, in, in, in, di, uo) is det.

c_gen_local_var_decls(Indent, Goal, VarSet, VarTypes, HeadVars) -->
	{ quantification__goal_vars(Goal, Vars0) },
	{ set__to_sorted_list(Vars0, Vars) },
	{ list__delete_elems(Vars, HeadVars, LocalVars) },
	c_gen_local_var_decls_2(LocalVars, VarSet, VarTypes, Indent).

:- pred c_gen_local_var_decls_2(list(var), varset, map(var, type), int,
				io__state, io__state).
:- mode c_gen_local_var_decls_2(in, in, in, in, di, uo) is det.

c_gen_local_var_decls_2([], _, _, _) --> [].
c_gen_local_var_decls_2([Var|Vars], VarSet, VarTypes, Indent) -->
	c_gen_indent(Indent),
	{ map__lookup(VarTypes, Var, Type) },
	c_gen_type(Type),
	io__write_string(" "),
	mercury_output_var(Var, VarSet, no),
	io__write_string(";\n"),
	c_gen_local_var_decls_2(Vars, VarSet, VarTypes, Indent).

%-----------------------------------------------------------------------------%

:- pred c_gen_goal(hlds_goal, int, c_gen_info, c_gen_info,
				io__state, io__state).
:- mode c_gen_goal(in, in, in, out, di, uo) is det.

c_gen_goal(Goal - GoalInfo, Indent, CGenInfo0, CGenInfo) -->
	globals__io_lookup_bool_option(line_numbers, LineNumbers),
	( { LineNumbers = yes } ->
		{ goal_info_get_context(GoalInfo, Context) },
		{ term__context_file(Context, FileName) },
		{ term__context_line(Context, LineNumber) },
		( { FileName \= "" } ->
			io__write_string("#line "),
			io__write_int(LineNumber),
			io__write_string(" """),
			io__write_string(FileName),
			io__write_string("""\n")
		;
			[]
		)
	;
		[]
	),
	c_gen_goal_2(Goal, Indent, CGenInfo0, CGenInfo).

:- pred c_gen_goal_2(hlds_goal_expr, int, c_gen_info, c_gen_info,
					io__state, io__state).
:- mode c_gen_goal_2(in, in, in, out, di, uo) is det.

c_gen_goal_2(switch(Var, _CanFail, CasesList, _), Indent,
		CGenInfo0, CGenInfo) -->
	{ sorry(7) },
	io__write_string("/* "),
	% c_gen_can_fail(CanFail), 
	io__write_string(" switch on `"),
	mercury_output_var(Var, _VarSet, no),
	io__write_string("' */"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	( { CasesList = [Case | Cases] } ->
		c_gen_case(Case, Var, Indent1, CGenInfo0, CGenInfo1),
		c_gen_cases(Cases, Var, Indent, CGenInfo1, CGenInfo)
	;
		io__write_string("fail"),
		{ CGenInfo0 = CGenInfo }
	),
	mercury_output_newline(Indent),
	io__write_string(")").

c_gen_goal_2(some(Vars, Goal), Indent, CGenInfo0, CGenInfo) -->
	{ sorry(8) },
	io__write_string("some ["),
	mercury_output_vars(Vars, _VarSet, no),
	io__write_string("] ("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	c_gen_goal(Goal, Indent1, CGenInfo0, CGenInfo),
	mercury_output_newline(Indent),
	io__write_string(")").

c_gen_goal_2(if_then_else(_Vars, A, B, C, _), Indent, CGenInfo0, CGenInfo)
		-->
	% XXX need to handle nondet
	{ c_gen_info_new_label(ElseLabel, CGenInfo0, CGenInfo1) },
	{ c_gen_info_get_failconts(CGenInfo1, FailureConts0) },
	{ stack__push(FailureConts0, goto(ElseLabel), FailureConts) },
	{ c_gen_info_set_failconts(CGenInfo1, FailureConts, CGenInfo2) },

	c_gen_indent(Indent),
	io__write_string("/* if */\n"),
	{ Indent1 is Indent + 1 },
	c_gen_goal(A, Indent1, CGenInfo2, CGenInfo3),
	{ c_gen_info_set_failconts(CGenInfo3, FailureConts0, CGenInfo4) },

	c_gen_indent(Indent),
	io__write_string("/* then */\n"),
	c_gen_goal(B, Indent1, CGenInfo4, CGenInfo5),
	c_gen_indent(Indent1),
	{ c_gen_info_new_label(EndIfLabel, CGenInfo5, CGenInfo6) },
	io__write_string("goto ML_"),
	io__write_int(EndIfLabel),
	io__write_string(";\n"),
	c_gen_label(ElseLabel, Indent1),
	c_gen_indent(Indent),
	io__write_string("/* else */\n"),
	(
		{ C = if_then_else(_, _, _, _, _) - _ }
	->
		c_gen_goal(C, Indent, CGenInfo6, CGenInfo)
	;
		c_gen_goal(C, Indent1, CGenInfo6, CGenInfo)
	),
	c_gen_label(EndIfLabel, Indent1),
	c_gen_indent(Indent),
	io__write_string("/* end if */\n").

c_gen_goal_2(not(Goal), Indent, CGenInfo0, CGenInfo) -->
	{ c_gen_info_new_label(SuccessLabel, CGenInfo0, CGenInfo1) },
	{ c_gen_info_get_failconts(CGenInfo1, FailureConts0) },
	{ stack__push(FailureConts0, goto(SuccessLabel), FailureConts) },
	{ c_gen_info_set_failconts(CGenInfo1, FailureConts, CGenInfo2) },

	c_gen_indent(Indent),
	io__write_string("/* not */\n"),
	{ Indent1 is Indent + 1 },
	c_gen_goal(Goal, Indent1, CGenInfo2, CGenInfo3),
	{ c_gen_info_set_failconts(CGenInfo3, FailureConts0, CGenInfo4) },
	c_gen_failure(Indent1, CGenInfo4, CGenInfo),
	c_gen_label(SuccessLabel, Indent1),
	c_gen_indent(Indent),
	io__write_string("/* end not */\n").

c_gen_goal_2(conj(Goals), Indent, CGenInfo0, CGenInfo) -->
	c_gen_conj(Goals, Indent, CGenInfo0, CGenInfo).

c_gen_goal_2(disj(List, _), Indent, CGenInfo0, CGenInfo) -->
	{ c_gen_info_get_code_model(CGenInfo0, CodeModel) },
	( { CodeModel = model_non } ->
		{ true }
	;
		{ sorry(5) }
	),
	( { List = [_Goal] } ->
		{ sorry(12) }
	; { List = [Goal | Goals] } ->
		c_gen_indent(Indent),
		io__write_string("/* disjunction */\n"),
		% XXX need to fix failure conts
		{ c_gen_info_new_label_func(Label, CGenInfo0, CGenInfo1) },
		c_gen_disj([Goal | Goals], Label, Indent, CGenInfo1, CGenInfo),
		c_gen_insert_label_func(Label, Indent, CGenInfo),
		c_gen_indent(Indent),
		io__write_string("/* end disjunction */\n")
	;
		c_gen_failure(Indent, CGenInfo0, CGenInfo)
	).

c_gen_goal_2(higher_order_call(_, _, _, _, _), _, _, _) -->
	{ error("mercury_to_c: higher_order_call not implemented") }.
c_gen_goal_2(call(PredId, ProcId, ArgVars, _, _, _PredName),
					Indent, CGenInfo0, CGenInfo) -->
	{ c_gen_info_get_module_info(CGenInfo0, ModuleInfo) },
	{ module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
		_PredInfo, ProcInfo) },
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
	{ proc_info_argmodes(ProcInfo, ArgModes) },
	c_gen_indent(Indent),
	( { CodeModel = model_non } ->
		{ c_gen_info_new_label_func(Label, CGenInfo0, CGenInfo1) }
	; { CodeModel = model_semi } ->
		io__write_string("if (!"),
		{ CGenInfo1 = CGenInfo0 },
		{ Label = 0 }
	;
		{ CGenInfo1 = CGenInfo0 },
		{ Label = 0 }
	),
	c_gen_proc_name(ModuleInfo, PredId, ProcId),
	io__write_string("("),
	c_gen_arg_list(ArgVars, ArgModes, CGenInfo1, CGenInfo2),
	( { CodeModel = model_non } ->
		( { ArgVars \= [] } ->
			io__write_string(", ")
		;
			[]
		),
		c_gen_write_label_func(Label, CGenInfo2),
		io__write_string(");\n"),
		c_gen_failure(Indent, CGenInfo2, CGenInfo),
		c_gen_insert_label_func(Label, Indent, CGenInfo)
	; { CodeModel = model_semi } ->
		io__write_string("))\n"),
		{ Indent1 is Indent + 1 },
		c_gen_failure(Indent1, CGenInfo2, CGenInfo)
	;
		{ CGenInfo = CGenInfo2 },
		io__write_string(");\n")
	).

c_gen_goal_2(unify(_A, _B, _, Unification, _), Indent, CGenInfo0, CGenInfo) -->
	c_gen_unification(Unification, Indent, CGenInfo0, CGenInfo).

c_gen_goal_2(pragma_c_code(C_Code, _, _, _, _, ArgNames, _, _), _, _, _) -->
	{ sorry(4) },
	{ get_pragma_c_var_names(ArgNames, Names) },
	io__write_string("$pragma(c_code, ["),
	c_gen_string_list(Names),
	io__write_string("], """),
	io__write_string(C_Code),
	io__write_string(""" )").

:- pred c_gen_string_list(list(string), io__state, io__state).
:- mode c_gen_string_list(in, di, uo) is det.

c_gen_string_list([]) --> [].
c_gen_string_list([Name]) -->
	io__write_string(Name).
c_gen_string_list([Name1, Name2|Names]) -->
	io__write_string(Name1),
	io__write_string(", "),
	c_gen_string_list([Name2|Names]).

:- pred c_gen_unification(unification, int, c_gen_info, c_gen_info,
			io__state, io__state).
:- mode c_gen_unification(in, in, in, out, di, uo) is det.

c_gen_unification(assign(Var1, Var2), Indent, CGenInfo0, CGenInfo) -->
	c_gen_indent(Indent),
	c_gen_var(Var1, CGenInfo0, CGenInfo1),
	io__write_string(" = "),
	c_gen_var(Var2, CGenInfo1, CGenInfo),
	io__write_string(";\n").
c_gen_unification(simple_test(Var1, Var2), Indent, CGenInfo0, CGenInfo) -->
	c_gen_indent(Indent),
	io__write_string("if ("),
	c_gen_var(Var1, CGenInfo0, CGenInfo1),
	io__write_string(" == "),	% XXX string equality
	c_gen_var(Var2, CGenInfo1, CGenInfo2),
	io__write_string(")\n"),
	{ Indent1 is Indent + 1 },
	c_gen_failure(Indent1, CGenInfo2, CGenInfo).
c_gen_unification(construct(_, _, _, _), _Indent, CGenInfo, CGenInfo) -->
	{ sorry(1) },
	io__write_string(" :=: ").
c_gen_unification(deconstruct(_, _, _, _, _), _Indent, CGenInfo, CGenInfo) -->
	{ sorry(2) },
	io__write_string(" == ").
c_gen_unification(complicated_unify(_, _), _Indent, CGenInfo, CGenInfo) -->
	{ sorry(3) },
	io__write_string(" = ").

:- pred c_gen_arg_list(list(var), list(mode), c_gen_info, c_gen_info,
			io__state, io__state).
:- mode c_gen_arg_list(in, in, in, out, di, uo) is det.

c_gen_arg_list(Vars, Modes, CGenInfo0, CGenInfo) -->
	( { Vars = [], Modes = [] } ->
		{ CGenInfo = CGenInfo0 }
	; { Vars = [Var|Vars1], Modes = [Mode|Modes1] } ->
		{ c_gen_info_get_module_info(CGenInfo0, ModuleInfo) },
		{ c_gen_info_get_varset(CGenInfo0, VarSet) },
		{ c_gen_info_get_output_vars(CGenInfo0, OutputVars) },
		(
			{ list__member(Var, OutputVars) },
			{ \+ mode_is_output(ModuleInfo, Mode) }
		->
			io__write_char('*')
		;
			{ mode_is_output(ModuleInfo, Mode) },
			{ \+ list__member(Var, OutputVars) }
		->
			io__write_char('&')
		;
			[]
		),
		mercury_output_var(Var, VarSet, no),	% XXX name mangling
		( { Vars1 = [] } ->
			[]
		;
			io__write_string(", ")
		),
		c_gen_arg_list(Vars1, Modes1, CGenInfo0, CGenInfo)
	;
		{ error("c_gen_arg_list: length mismatch") }
	).

:- pred c_gen_var(var, c_gen_info, c_gen_info, io__state, io__state).
:- mode c_gen_var(in, in, out, di, uo) is det.

c_gen_var(Var, CGenInfo, CGenInfo) -->
	{ c_gen_info_get_varset(CGenInfo, VarSet) },
	{ c_gen_info_get_output_vars(CGenInfo, OutputVars) },
	( { list__member(Var, OutputVars) } ->
		io__write_char('*')
	;
		[]
	),
	mercury_output_var(Var, VarSet, no).	% XXX name mangling

:- pred sorry(int::in) is erroneous.

sorry(N) :-
	string__format("Sorry, not implemented [%d]", [i(N)], ErrorMessage),
	error(ErrorMessage).

:- pred c_gen_var_modes(list(var), list(mode), varset,
					io__state, io__state).
:- mode c_gen_var_modes(in, in, in, di, uo) is det.

c_gen_var_modes([], [], _) --> [].
c_gen_var_modes([Var|Vars], [Mode|Modes], VarSet) -->
	mercury_output_var(Var, VarSet, no),
	io__write_string("::"),
	mercury_output_mode(Mode, VarSet),
	( { Vars \= [] } ->
		io__write_string(", ")
	;
		[]
	),
	c_gen_var_modes(Vars, Modes, VarSet).
c_gen_var_modes([], [_|_], _) -->
	{ error("c_gen_var_modes: length mis-match") }.
c_gen_var_modes([_|_], [], _) -->
	{ error("c_gen_var_modes: length mis-match") }.

:- pred c_gen_conj(list(hlds_goal), int, c_gen_info, c_gen_info,
				io__state, io__state).
:- mode c_gen_conj(in, in, in, out, di, uo) is det.

c_gen_conj([], _Indent, CGenInfo, CGenInfo) --> [].
c_gen_conj([Goal | Goals], Indent, CGenInfo0, CGenInfo) -->
	c_gen_goal(Goal, Indent, CGenInfo0, CGenInfo1),
	c_gen_conj(Goals, Indent, CGenInfo1, CGenInfo).

:- pred c_gen_disj(list(hlds_goal), c_label_func, int, c_gen_info, c_gen_info,
				io__state, io__state).
:- mode c_gen_disj(in, in, in, in, out, di, uo) is det.

c_gen_disj([], _Label, _Indent, CGenInfo, CGenInfo) --> [].
c_gen_disj([Goal|Goals], Label, Indent, CGenInfo0, CGenInfo) -->
	c_gen_goal(Goal, Indent, CGenInfo0, CGenInfo1),
	io__write_string("MNL_"),
	io__write_int(Label),
	io__write_string("();\n"),
	c_gen_disj(Goals, Label, Indent, CGenInfo1, CGenInfo).

:- pred c_gen_case(case, var, int, c_gen_info, c_gen_info,
				io__state, io__state).
:- mode c_gen_case(in, in, in, in, out, di, uo) is erroneous.

c_gen_case(case(_ConsId, Goal), Var, Indent, CGenInfo0, CGenInfo) -->
	{ sorry(10) },
	mercury_output_var(Var, _VarSet, no),
	io__write_string(" has functor "),
	% c_gen_cons_id(ConsId),
	io__write_string(","),
	mercury_output_newline(Indent),
	c_gen_goal(Goal, Indent, CGenInfo0, CGenInfo).

:- pred c_gen_cases(list(case), var, int, c_gen_info, c_gen_info,
				io__state, io__state).
:- mode c_gen_cases(in, in, in, in, out, di, uo) is erroneous.

c_gen_cases(CasesList, Var, Indent, CGenInfo0, CGenInfo) -->
	{ sorry(11) },
	(
		{ CasesList = [Case | Cases] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		c_gen_case(Case, Var, Indent1, CGenInfo0, CGenInfo1),
		c_gen_cases(Cases, Var, Indent, CGenInfo1, CGenInfo)
	;
		[]
	).

:- pred c_gen_var_types(int, varset, map(var, type), varset,
					io__state, io__state).
:- mode c_gen_var_types(in, in, in, in, di, uo) is det.

c_gen_var_types(Indent, VarSet, VarTypes, TVarSet) -->
	{ map__keys(VarTypes, Vars) },
	c_gen_var_types_2(Vars, Indent, VarSet, VarTypes, TVarSet).

:- pred c_gen_var_types_2(list(var), int, varset, map(var, type),
					varset, io__state, io__state).
:- mode c_gen_var_types_2(in, in, in, in, in, di, uo) is det.

c_gen_var_types_2([], _, _, _, _) --> [].
c_gen_var_types_2([Var | Vars], Indent, VarSet, VarTypes, TypeVarSet)
		-->
	{ map__lookup(VarTypes, Var, Type) },
	c_gen_indent(Indent),
	c_gen_type(Type),
	io__write_string(" "),
	mercury_output_var(Var, VarSet, no),
	io__write_string(";\t/* "),
	mercury_output_term(Type, TypeVarSet, no),
	io__write_string(" */\n"),
	c_gen_var_types_2(Vars, Indent, VarSet, VarTypes, TypeVarSet).

:- pred c_gen_types(int, type_table, io__state, io__state).
:- mode c_gen_types(in, in, di, uo) is det.

c_gen_types(_Indent, _X) -->
	[].
	% c_gen_indent(Indent),
	% io__write_string("/* types */\n").

:- pred c_gen_indent(int, io__state, io__state).
:- mode c_gen_indent(in, di, uo) is det.

c_gen_indent(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		c_gen_indent(Indent1)
	).

%-----------------------------------------------------------------------------%

:- pred c_gen_label(c_label, int, io__state, io__state).
:- mode c_gen_label(in, in, di, uo) is det.

c_gen_label(Label, Indent) -->
	{ UnIndent is Indent - 1 },
	c_gen_indent(UnIndent),
	io__write_string("ML_"),
	io__write_int(Label),
	io__write_string(":\n").

:- pred c_gen_failure(int, c_gen_info, c_gen_info, io__state, io__state).
:- mode c_gen_failure(in, in, out, di, uo) is det.

c_gen_failure(Indent, CGenInfo, CGenInfo) -->
	{ c_gen_info_get_failconts(CGenInfo, FailContStack) },
	( { stack__top(FailContStack, FailCont) } ->
		c_gen_indent(Indent),
		c_gen_failure_2(FailCont)
	;
		{ error("c_gen_failure: missing failure continuation") }
	).

:- pred c_gen_failure_2(c_failure_cont, io__state, io__state).
:- mode c_gen_failure_2(in, di, uo) is det.

c_gen_failure_2(nondet_fail) -->
	io__write_string("return;\n").
c_gen_failure_2(semidet_fail) -->
	io__write_string("return FALSE;\n").
c_gen_failure_2(goto(Label)) -->
	io__write_string("goto ML_"),
	io__write_int(Label),
	io__write_string(";\n").
c_gen_failure_2(call(Label)) -->
	io__write_string("MNL_"),
	io__write_int(Label),
	io__write_string("();\n").

%-----------------------------------------------------------------------------%

:- pred c_gen_info_init(module_info, pred_id, proc_id, varset, code_model,
			list(var), c_gen_info).
:- mode c_gen_info_init(in, in, in, in, in, in, out) is det.

c_gen_info_init(ModuleInfo, PredId, ProcId, VarSet, CodeModel, OutputVars,
		CGenInfo) :-
	stack__init(FailureContStack0),
	( CodeModel = model_non ->
		stack__push(FailureContStack0, nondet_fail, FailureContStack)
	; CodeModel = model_semi ->
		stack__push(FailureContStack0, semidet_fail, FailureContStack)
	;
		FailureContStack = FailureContStack0
	),
	LabelCounter = 0,
	LabelFuncCounter = 0,
	CGenInfo = c_gen_info(
			CodeModel,
			ModuleInfo,
			PredId,
			ProcId,
			VarSet,
			OutputVars,
			FailureContStack,
			LabelCounter,
			LabelFuncCounter
		).

:- pred c_gen_info_get_code_model(c_gen_info, code_model).
:- mode c_gen_info_get_code_model(in, out) is det.

c_gen_info_get_code_model(c_gen_info(CodeModel, _, _, _, _, _, _, _, _),
	CodeModel).

:- pred c_gen_info_get_module_info(c_gen_info, module_info).
:- mode c_gen_info_get_module_info(in, out) is det.

c_gen_info_get_module_info(c_gen_info(_, ModuleInfo, _, _, _, _, _, _, _),
	ModuleInfo).

:- pred c_gen_info_get_varset(c_gen_info, varset).
:- mode c_gen_info_get_varset(in, out) is det.

c_gen_info_get_varset(c_gen_info(_, _, _, _, VarSet, _, _, _, _), VarSet).

:- pred c_gen_info_get_output_vars(c_gen_info, list(var)).
:- mode c_gen_info_get_output_vars(in, out) is det.

c_gen_info_get_output_vars(c_gen_info(_, _, _, _, _, OutputVars, _, _, _),
	OutputVars).

:- pred c_gen_info_get_failconts(c_gen_info, stack(c_failure_cont)).
:- mode c_gen_info_get_failconts(in, out) is det.

c_gen_info_get_failconts(c_gen_info(_, _, _, _, _, _, FailConts, _, _),
	FailConts).

:- pred c_gen_info_set_failconts(c_gen_info, stack(c_failure_cont), c_gen_info).
:- mode c_gen_info_set_failconts(in, in, out) is det.

c_gen_info_set_failconts(c_gen_info(A, B, C, D, E, F, _, H, I), FailConts,
			 c_gen_info(A, B, C, D, E, F, FailConts, H, I)).

:- pred c_gen_info_new_label(c_label, c_gen_info, c_gen_info).
:- mode c_gen_info_new_label(out, in, out) is det.

c_gen_info_new_label(Label, c_gen_info(A, B, C, D, E, F, G, Label0, I),
			    c_gen_info(A, B, C, D, E, F, G, Label, I)) :-
	Label is Label0 + 1.

:- pred c_gen_info_new_label_func(c_label_func, c_gen_info, c_gen_info).
:- mode c_gen_info_new_label_func(out, in, out) is det.

c_gen_info_new_label_func(Label, c_gen_info(A, B, C, D, E, F, G, H, Label0),
			    c_gen_info(A, B, C, D, E, F, G, H, Label)) :-
	Label is Label0 + 1.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
