%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% hlds_out.nl

% Main author: conway

%-----------------------------------------------------------------------------%

:- module hlds_out.
:- interface.
:- import_module int, io, hlds.

% print out an hlds structure.

:- pred hlds_out__write_hlds(int, module_info, io__state, io__state).
:- mode hlds_out__write_hlds(in, in, in, out).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module term_io, require.

hlds_out__write_hlds(Indent, Module) -->
	hlds_out__write_header(Indent, Module),
	{
		Indent1 is Indent + 1,
		moduleinfo_preds(Module, PredTable),
		moduleinfo_types(Module, TypeTable),
		moduleinfo_insts(Module, InstTable),
		moduleinfo_modes(Module, ModeTable)
	},
	hlds_out__write_preds(Indent1, PredTable),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_types(Indent1, TypeTable),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_insts(Indent1, InstTable),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_modes(Indent1, ModeTable),
	hlds_out__write_footer(Indent, Module).

:- pred hlds_out__write_header(int, module_info, io__state, io__state).
:- mode hlds_out__write_header(in, in, in, out).

hlds_out__write_header(Indent, Module) -->
	{ moduleinfo_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string("% HLDS structure for module: "),
	io__write_string(Name),
	io__write_string("\n\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_indent(Indent1),
	io__write_string("module(\n").

:- pred hlds_out__write_footer(int, module_info, io__state, io__state).
:- mode hlds_out__write_footer(in, in, in, out).

hlds_out__write_footer(Indent, Module) -->
	{ moduleinfo_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(")\n"),
	hlds_out__write_indent(Indent),
	io__write_string("% End of module: "),
	io__write_string(Name),
	io__write_string(".\n").

:- pred hlds_out__write_preds(int, pred_table, io__state, io__state).
:- mode hlds_out__write_preds(in, in, in, out).

hlds_out__write_preds(Indent, PredTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("% Table of Predicates\n"),
	hlds_out__write_indent(Indent),
	io__write_string("[\n"),
	{ map__keys(PredTable, PredIds) },
	{ Indent1 is Indent + 1 },
	hlds_out__write_preds_2(Indent1, PredIds, PredTable),
	hlds_out__write_indent(Indent),
	io__write_string("]\n").

:- pred hlds_out__write_preds_2(int, list(pred_id), pred_table,
			io__state, io__state).
:- mode hlds_out__write_preds_2(in, in, in, in, out).

hlds_out__write_preds_2(Indent, PredIds0, PredTable) --> 
	(
		{ PredIds0 = [] }
	->
		[]
	;
		{ PredIds0 = [PredId] }
	->
		{
			map__search(PredTable, PredId, PredInfo),
			Indent1 is Indent + 1
		},
		hlds_out__write_pred(Indent1, PredId, PredInfo)
	;
		{
			PredIds0 = [PredId|PredIds],
			map__search(PredTable, PredId, PredInfo),
			Indent1 is Indent + 1
		},
		hlds_out__write_pred(Indent1, PredId, PredInfo),
		io__write_string(",\n"),
		hlds_out__write_preds_2(Indent, PredIds, PredTable)
	).

:- pred hlds_out__write_pred(int, pred_id, pred_info, io__state, io__state).
:- mode hlds_out__write_pred(in, in, in, in, out).

hlds_out__write_pred(Indent, PredId, PredInfo) -->
	{
		PredInfo = predicate(
			Varset,
			ArgTypes,
			_Condition,
			ClausesInfo, % source level
			ProcTable,
			_Context
		),
		Indent1 is Indent + 1
	},
	hlds_out__write_indent(Indent),
	io__write_string("predicate(\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Type Variables\n"),
	hlds_out__write_varset(Indent1, Varset),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Argument Types\n"),
	hlds_out__write_argtypes(Indent1, ArgTypes),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	{ ClausesInfo = clauses_info(Varset, VarTypes, HeadVars, Clauses) },
	io__write_string("% The variables in this pred\n"),
	hlds_out__write_varset(Indent1, Varset),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% The types of the variables in this pred\n"),
	hlds_out__write_vartypes(Indent1, VarTypes),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% The head variables of this pred\n"),
	hlds_out__write_anything(Indent1, HeadVars),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Clauses of the predicate\n"),
	hlds_out__write_clauses(Indent1, Clauses),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Procedures for the predicate\n"),
	hlds_out__write_procs(Indent1, PredId, ProcTable),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").

:- pred hlds_out__write_clauses(int, list(clause), io__state, io__state).
:- mode hlds_out__write_clauses(in, in, in, out).

hlds_out__write_clauses(Indent, Clauses0) -->
	(
		{ Clauses0 = [] }
	->
		[]
	;
		{ Clauses0 = [Clause] }
	->
		{ Indent1 is Indent + 1 },
		hlds_out__write_clause(Indent1, Clause)
	;
		{
			Clauses0 = [Clause|Clauses],
			Indent1 is Indent + 1
		},
		hlds_out__write_clause(Indent1, Clause),
		hlds_out__write_indent(Indent),
		io__write_string(",\n"),
		hlds_out__write_clauses(Indent, Clauses)
	).

:- pred hlds_out__write_clause(int, clause, io__state, io__state).
:- mode hlds_out__write_clause(in, in, in, out).

hlds_out__write_clause(Indent, Clause) -->
	{
		Clause = clause(
			Modes,
			Goal,
			_Context
		),
		Indent1 is Indent + 1
	},
	hlds_out__write_indent(Indent),
	io__write_string("clause(\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Modes for which this clause applies:\n\t\t"),
	hlds_out__write_anything(Indent1, Modes),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% The goal of this clause\n"),
	hlds_out__write_hlds__goal(Indent, Goal),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").

:- pred hlds_out__write_intlist(int, list(int), io__state, io__state).
:- mode hlds_out__write_intlist(in, in, in, out).

hlds_out__write_intlist(Indent, IntList) -->
	(
		{ IntList = [] }
	->
		hlds_out__write_indent(Indent),
		io__write_string("[]")
	;
		hlds_out__write_indent(Indent),
		io__write_string("[ "),
		hlds_out__write_intlist_2(IntList),
		io__write_string("]")
	).

:- pred hlds_out__write_intlist_2(list(int), io__state, io__state).
:- mode hlds_out__write_intlist_2(in, in, out).

hlds_out__write_intlist_2(Ns0) -->
	(
		{ Ns0 = [] }
	->
		{ error("This should be unreachable.") }
	;
		{ Ns0 = [N] }
	->
		io__write_int(N)
	;
		{ Ns0 = [N|Ns] },
		io__write_int(N),
		io__write_string(", "),
		hlds_out__write_intlist_2(Ns)
	).

:- pred hlds_out__write_hlds__goal(int, hlds__goal, io__state, io__state).
:- mode hlds_out__write_hlds__goal(in, in, in, out).

hlds_out__write_hlds__goal(Indent, GoalExprn - GoalInfo) -->
	hlds_out__write_goal(Indent, GoalExprn),
	{ Indent1 is Indent + 1 },
	hlds_out__write_indent(Indent1),
	io__write_string("-\n"),
	hlds_out__write_goalinfo(Indent1, GoalInfo).

:- pred hlds_out__write_goal(int, hlds__goal_expr, io__state, io__state).
:- mode hlds_out__write_goal(in, in, in, out).

hlds_out__write_goal(Indent, conj(Goals)) -->
	hlds_out__write_indent(Indent),
	io__write_string("conj(\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_indent(Indent1),
	io__write_string("[\n"),
	hlds_out__write_conj_goals(Indent1, Goals),
	hlds_out__write_indent(Indent1),
	io__write_string("]\n"),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").
hlds_out__write_goal(Indent, disj(Goals)) -->
	hlds_out__write_indent(Indent),
	io__write_string("disj(\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_indent(Indent1),
	io__write_string("[\n"),
	hlds_out__write_disj_goals(Indent1, Goals),
	hlds_out__write_indent(Indent1),
	io__write_string("]\n"),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").
hlds_out__write_goal(Indent, call(PredId, PredModeId, Args, Builtin)) -->
	hlds_out__write_indent(Indent),
	io__write_string("call(\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_predid(Indent1, PredId),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_predmodeid(Indent1, PredModeId),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_termlist(Indent1, Args),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_builtin(Indent1, Builtin),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").
hlds_out__write_goal(Indent, switch(VarId, Cases, FollowVars)) -->
	hlds_out__write_indent(Indent),
	io__write_string("switch(\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_indent(Indent1),
	io__write_anything(VarId),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_cases(Indent1, Cases),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_followvars(Indent1, FollowVars),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").
hlds_out__write_goal(Indent, unify(LTerm, RTerm, Mode, Uni, _)) -->
	hlds_out__write_indent(Indent),
	io__write_string("unify(\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_term(Indent1, LTerm),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_term(Indent1, RTerm),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_unimode(Indent1, Mode),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_unification(Indent1, Uni),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").

	% XXX this output is terrible:

hlds_out__write_goal(_Indent, if_then_else(Vars, A, B, C)) -->
	io__write_anything(if_then_else(Vars, A, B, C)).
hlds_out__write_goal(_Indent, not(Vars, G)) -->
	io__write_anything(not(Vars, G)).
hlds_out__write_goal(_Indent, some(Vars, G)) -->
	io__write_anything(some(Vars, G)).
hlds_out__write_goal(_Indent, all(Vars, G)) -->
	io__write_anything(all(Vars, G)).

:- pred hlds_out__write_argtypes(int, list(term), io__state, io__state).
:- mode hlds_out__write_argtypes(in, in, in, out).

hlds_out__write_argtypes(Indent, ArgTypes) -->
	hlds_out__write_termlist(Indent, ArgTypes),
	io__write_string("\n").

:- pred hlds_out__write_builtin(int, is_builtin, io__state, io__state).
:- mode hlds_out__write_builtin(in, in, in, out).

hlds_out__write_builtin(Indent, is_builtin) -->
	hlds_out__write_indent(Indent),
	io__write_string("is_builtin").
hlds_out__write_builtin(Indent, not_builtin) -->
	hlds_out__write_indent(Indent),
	io__write_string("not_builtin").
	
:- pred hlds_out__write_cases(int, list(case), io__state, io__state).
:- mode hlds_out__write_cases(in, in, in, out).

hlds_out__write_cases(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_followvars(int, follow_vars, io__state, io__state).
:- mode hlds_out__write_followvars(in, in, in, out).

hlds_out__write_followvars(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_goalinfo(int, hlds__goal_info, io__state, io__state).
:- mode hlds_out__write_goalinfo(in, in, in, out).

hlds_out__write_goalinfo(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_conj_goals(int, list(hlds__goal), io__state, io__state).
:- mode hlds_out__write_conj_goals(in, in, in, out).

hlds_out__write_conj_goals(Indent, Goals0) -->
	(
		{ Goals0 = [] }
	->
		{ Indent1 is Indent + 1 },
		hlds_out__write_indent(Indent1),
		io__write_string("true\n")
	;
		{
			Goals0 = [Goal | Goals],
			Indent1 is Indent + 1
		},
		hlds_out__write_hlds__goal(Indent1, Goal),
		hlds_out__write_indent(Indent),
		io__write_string(",\n"),
		hlds_out__write_conj_goals(Indent, Goals)
	).

:- pred hlds_out__write_disj_goals(int, list(hlds__goal), io__state, io__state).
:- mode hlds_out__write_disj_goals(in, in, in, out).

hlds_out__write_disj_goals(Indent, Goals0) -->
	(
		{ Goals0 = [] }
	->
		{ Indent1 is Indent + 1 },
		hlds_out__write_indent(Indent1),
		io__write_string("fail\n")
	;
		{
			Goals0 = [Goal | Goals],
			Indent1 is Indent + 1
		},
		hlds_out__write_hlds__goal(Indent1, Goal),
		hlds_out__write_indent(Indent),
		io__write_string(",\n"),
		hlds_out__write_disj_goals(Indent, Goals)
	).

:- pred hlds_out__write_insts(int, inst_table, io__state, io__state).
:- mode hlds_out__write_insts(in, in, in, out).

hlds_out__write_insts(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_modes(int, mode_table, io__state, io__state).
:- mode hlds_out__write_modes(in, in, in, out).

hlds_out__write_modes(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_mode_list(int, list(mode), io__state, io__state).
:- mode hlds_out__write_mode_list(in, in, in, out).

hlds_out__write_mode_list(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_predid(int, pred_id, io__state, io__state).
:- mode hlds_out__write_predid(in, in, in, out).

hlds_out__write_predid(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_predmodeid(int, proc_id, io__state, io__state).
:- mode hlds_out__write_predmodeid(in, in, in, out).

hlds_out__write_predmodeid(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_procs(int, pred_id, proc_table, io__state, io__state).
:- mode hlds_out__write_procs(in, in, in, in, out).

hlds_out__write_procs(Indent, PredId, ProcTable) -->
	hlds_out__write_indent(Indent),
	io__write_string("[\n"),
	{ map__keys(ProcTable, ProcIds) },
	{ Indent1 is Indent + 1 },
	(
		{ ProcIds = [] }
	->
		hlds_out__write_indent(Indent),
		io__write_string("[]")
	;
		hlds_out__write_procs_2(Indent1, PredId, ProcIds, ProcTable),
		hlds_out__write_indent(Indent),
		io__write_string("]\n")
	).

:- pred hlds_out__write_procs_2(int, pred_id, list(proc_id),
					proc_table, io__state, io__state).
:- mode hlds_out__write_procs_2(in, in, in, in, in, out).

hlds_out__write_procs_2(Indent, PredId, ProcIds0, ProcTable) --> 
	(
		{ ProcIds0 = [] }
	->
		{ error("This should never happen") }
	;
		{ ProcIds0 = [ProcId] }
	->
		{
			map__search(ProcTable, ProcId, ProcInfo),
			Indent1 is Indent + 1
		},
		hlds_out__write_proc(Indent1, PredId, ProcId, ProcInfo)
	;
		{
			ProcIds0 = [ProcId|ProcIds],
			map__search(ProcTable, ProcId, ProcInfo),
			Indent1 is Indent + 1
		},
		hlds_out__write_proc(Indent1, PredId, ProcId, ProcInfo),
		io__write_string(",\n"),
		hlds_out__write_procs_2(Indent, PredId, ProcIds, ProcTable)
	).

:- pred hlds_out__write_proc(int, pred_id, proc_id, proc_info,
							io__state, io__state).
:- mode hlds_out__write_proc(in, in, in, in, in, out).

hlds_out__write_proc(Indent, PredId, PredModeId, Proc) -->
	{
		PredId = pred(Module, Name, Arity),
		Proc = procedure(
			_DeclaredDeterminism,
			VarNames,
			VarTypes,
			HeadVars,
			HeadModes,
			Goal,
			_ModeContext,
			_CallInfo,
			Category
		),
		Indent1 is Indent + 1
	},
	hlds_out__write_indent(Indent),
	io__write_string("procedure( % "),
	io__write_string(Module),
	io__write_string(":"),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(", "),
	io__write_int(PredModeId),
	io__write_string("\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Category\n"),
	hlds_out__write_category(Indent1, Category),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Variable names\n"),
	hlds_out__write_varset(Indent1, VarNames),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Variable Types\n"),
	hlds_out__write_vartypes(Indent1, VarTypes),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Argument Variables\n"),
	hlds_out__write_anything(Indent1, HeadVars),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Argument Modes\n"),
	hlds_out__write_modelist(Indent1, HeadModes),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Procedue goal\n"),
	hlds_out__write_hlds__goal(Indent, Goal),
%%%	io__write_string(",\n"),
%%%	hlds_out__write_indent(Indent1),
%%%	io__write_string("% Mode declaration context\n"),
%%%	hlds_out__write_context(Indent, Context),
%%%	io__write_string(",\n"),
%%%	hlds_out__write_indent(Indent1),
%%%	io__write_string("% CallInfo\n"),
%%%	hlds_out__write_callinfo(Indent, CallInfo),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").

:- pred hlds_out__write_term(int, term, io__state, io__state).
:- mode hlds_out__write_term(in, in, in, out).

hlds_out__write_term(Indent, term_functor(Const, Terms, Context)) -->
	hlds_out__write_indent(Indent),
	io__write_string("term_functor(\n"),
	{ Indent1 is Indent + 1 },
	hlds_out__write_const(Indent1, Const),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_termlist(Indent1, Terms),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_anything(Indent1, Context),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").
hlds_out__write_term(Indent, term_variable(VarId)) -->
	hlds_out__write_indent(Indent),
	io__write_string("term_variable("),
	io__write_anything(VarId),
	io__write_string(")\n").

:- pred hlds_out__write_termlist(int, list(term), io__state, io__state).
:- mode hlds_out__write_termlist(in, in, in, out).

hlds_out__write_termlist(Indent, Terms) -->
	(
		{ Terms = [] }
	->
		hlds_out__write_indent(Indent),
		io__write_string("[]\n")
	;
		hlds_out__write_indent(Indent),
		io__write_string("[\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_termlist_2(Indent1, Terms),
		hlds_out__write_indent(Indent),
		io__write_string("]\n")
	).

:- pred hlds_out__write_termlist_2(int, list(term), io__state, io__state).
:- mode hlds_out__write_termlist_2(in, in, in, out).

hlds_out__write_termlist_2(Indent, Terms0) -->
	(
		{ Terms0 = [] }
	->
		{ error("This cannot happen") }
	;
		{ Terms0 = [Term] }
	->
		{Indent1 is Indent + 1},
		hlds_out__write_term(Indent1, Term)
	;
		{
			Terms0 = [Term|Terms],
			Indent1 is Indent + 1
		},
		hlds_out__write_term(Indent1, Term),
		hlds_out__write_indent(Indent),
		io__write_string(",\n"),
		hlds_out__write_termlist_2(Indent, Terms)
	).

:- pred hlds_out__write_const(int, const, io__state, io__state).
:- mode hlds_out__write_const(in, in, in, out).

hlds_out__write_const(Indent, term_atom(Str)) -->
	hlds_out__write_indent(Indent),
	io__write_string("term_atom("),
	io__write_string(Str),
	io__write_string(")\n").
hlds_out__write_const(Indent, term_integer(Int)) -->
	hlds_out__write_indent(Indent),
	io__write_string("term_integer("),
	io__write_int(Int),
	io__write_string(")\n").
hlds_out__write_const(Indent, term_string(Str)) -->
	hlds_out__write_indent(Indent),
	io__write_string("term_string("),
	io__write_string(Str),
	io__write_string(")\n").

:- pred hlds_out__write_types(int, type_table, io__state, io__state).
:- mode hlds_out__write_types(in, in, in, out).

hlds_out__write_types(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_unification(int, unification, io__state, io__state).
:- mode hlds_out__write_unification(in, in, in, out).

hlds_out__write_unification(Indent, construct(VarId, ConsId, Vars, Modes)) -->
	hlds_out__write_indent(Indent),
	io__write_string("construct(\n"),
	{ Indent1 is Indent + 1 },
	io__write_anything(VarId),
	hlds_out__write_consid(Indent1, ConsId),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_anything(Indent1, Vars),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_mode_list(Indent1, Modes),
	io__write_string(")\n").
hlds_out__write_unification(Indent, deconstruct(VarId, ConsId, Vars, Modes)) -->
	hlds_out__write_indent(Indent),
	io__write_string("deconstruct(\n"),
	{ Indent1 is Indent + 1 },
	io__write_anything(VarId),
	hlds_out__write_consid(Indent1, ConsId),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_anything(Indent1, Vars),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_mode_list(Indent1, Modes),
	io__write_string(")\n").
hlds_out__write_unification(Indent, assign(VarId0, VarId1)) -->
	hlds_out__write_indent(Indent),
	io__write_string("assign("),
	io__write_anything(VarId0),
	io__write_string(", "),
	io__write_anything(VarId1),
	io__write_string(")\n").
hlds_out__write_unification(Indent, simple_test(VarId0, VarId1)) -->
	hlds_out__write_indent(Indent),
	io__write_string("simple_test("),
	io__write_anything(VarId0),
	io__write_string(", "),
	io__write_anything(VarId1),
	io__write_string(")\n").
hlds_out__write_unification(Indent, complicated_unify(Mode, VarId0, VarId1)) -->
	hlds_out__write_indent(Indent),
	io__write_string("complicated_unify(\n"),
	{ Mode = (Mode1 - Mode2) },
	hlds_out__write_mode(Indent, Mode1),
	io__write_string(" - "),
	hlds_out__write_mode(Indent, Mode2),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent),
	io__write_anything(VarId0),
	io__write_string("\n"),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent),
	io__write_anything(VarId1),
	io__write_string("\n"),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").

:- pred hlds_out__write_anything(int, _AnyType, io__state, io__state).
:- mode hlds_out__write_anything(in, in, di, uo).

hlds_out__write_anything(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_unimode(int, unify_mode, io__state, io__state).
:- mode hlds_out__write_unimode(in, in, in, out).

hlds_out__write_unimode(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_varset(int, varset, io__state, io__state).
:- mode hlds_out__write_varset(in, in, in, out).

/*************
	% XXX this violates modularity - varset is an adt.

hlds_out__write_varset(Indent, varset(Id, VarNames, VarTerms)) -->
	hlds_out__write_indent(Indent),
	io__write_string("varset(\n"),
	{Indent1 is Indent + 1},
	hlds_out__write_indent(Indent1),
	io__write_int(Id),
	io__write_string("\n"),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Variable Id - Name bindings.\n"),
	hlds_out__write_varnames(Indent1, VarNames),
	hlds_out__write_indent(Indent),
	io__write_string(",\n"),
	hlds_out__write_indent(Indent1),
	io__write_string("% Variable Id - Value bindings.\n"),
	hlds_out__write_varterms(Indent1, VarTerms),
	hlds_out__write_indent(Indent),
	io__write_string(")\n").
*************/
hlds_out__write_varset(Indent, VarSet) -->
	hlds_out__write_indent(Indent),
	io__write_anything(VarSet),
	io__write_string("\n").

:- pred hlds_out__write_varnames(int, map(var, string), io__state, io__state).
:- mode hlds_out__write_varnames(in, in, in, out).

hlds_out__write_varnames(Indent, VarNames) -->
	{ map__to_assoc_list(VarNames, VarNameList) },
	(
		{ VarNameList = [] }
	->
		hlds_out__write_indent(Indent),
		io__write_string("[]\n")
	;
		hlds_out__write_indent(Indent),
		io__write_string("[\n"),
		{Indent1 is Indent + 1},
		hlds_out__write_varnames_2(Indent1, VarNameList),
		hlds_out__write_indent(Indent),
		io__write_string("]\n")
	).

:- pred hlds_out__write_varnames_2(int, list(pair(var, string)),
							io__state, io__state).
:- mode hlds_out__write_varnames_2(in, in, in, out).

hlds_out__write_varnames_2(Indent, VarNameList0) -->
	(
		{ VarNameList0 = [] }
	->
		{ error("This cannot happen") }
	;
		{ VarNameList0 = [VarId - Name] }
	->
		{Indent1 is Indent + 1},
		hlds_out__write_indent(Indent1),
		io__write_anything(VarId),
		io__write_string(" - "),
		io__write_string(Name),
		io__write_string("\n")
	;
		{
			VarNameList0 = [VarId - Name|VarNameList],
			Indent1 is Indent + 1
		},
		hlds_out__write_indent(Indent1),
		io__write_anything(VarId),
		io__write_string(" - "),
		io__write_string(Name),
		io__write_string("\n"),
		io__write_string(",\n"),
		hlds_out__write_varnames_2(Indent, VarNameList)
	).

:- pred hlds_out__write_varterms(int, map(var, term), io__state, io__state).
:- mode hlds_out__write_varterms(in, in, in, out).

hlds_out__write_varterms(Indent, VarTerms) -->
	{ map__to_assoc_list(VarTerms, VarTermList) },
	(
		{ VarTermList = [] }
	->
		hlds_out__write_indent(Indent),
		io__write_string("[]\n")
	;
		hlds_out__write_indent(Indent),
		io__write_string("[\n"),
		{ Indent1 is Indent + 1 },
		hlds_out__write_varterms_2(Indent1, VarTermList),
		hlds_out__write_indent(Indent),
		io__write_string("]\n")
	).

:- pred hlds_out__write_varterms_2(int, list(pair(var, term)),
					io__state, io__state).
:- mode hlds_out__write_varterms_2(in, in, in, out).

hlds_out__write_varterms_2(Indent, VarTermList0) -->
	(
		{ VarTermList0 = [] }
	->
		{ error("This cannot happen") }
	;
		{ VarTermList0 = [VarId - Term] }
	->
		{Indent1 is Indent + 1},
		{Indent2 is Indent1 + 1},
		hlds_out__write_indent(Indent1),
		io__write_anything(VarId),
		io__write_string(" - "),
		hlds_out__write_term(Indent2, Term)
	;
		{
			VarTermList0 = [VarId - Term|VarTermList],
			Indent1 is Indent + 1,
			Indent2 is Indent1 + 1
		},
		hlds_out__write_indent(Indent1),
		io__write_anything(VarId),
		io__write_string(" - "),
		hlds_out__write_term(Indent2, Term),
		io__write_string(",\n"),
		hlds_out__write_varterms_2(Indent, VarTermList)
	).

:- pred hlds_out__write_vartypes(int, map(var, type), io__state, io__state).
:- mode hlds_out__write_vartypes(in, in, di, uo).

hlds_out__write_vartypes(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_modelist(int, list(mode), io__state, io__state).
:- mode hlds_out__write_modelist(in, in, di, uo).

hlds_out__write_modelist(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_category(int, category, io__state, io__state).
:- mode hlds_out__write_category(in, in, di, uo).

:- hlds_out__write_category(_, X, _, _) when X.	% NU-Prolog indexing.

hlds_out__write_category(Indent, deterministic) -->
	hlds_out__write_indent(Indent),
	io__write_string("deterministic\n").
hlds_out__write_category(Indent, semideterministic) -->
	hlds_out__write_indent(Indent),
	io__write_string("semideterministic\n").
hlds_out__write_category(Indent, nondeterministic) -->
	hlds_out__write_indent(Indent),
	io__write_string("nondeterministic\n").

:- pred hlds_out__write_indent(int, io__state, io__state).
:- mode hlds_out__write_indent(in, di, uo).

hlds_out__write_indent(X) -->
	(
		{ X = 0 }
	->
		[]
	;
		{ X1 is X - 1 },
		io__write_string("  "),
		hlds_out__write_indent(X1)
	).

:- pred hlds_out__write_mode(int, mode, io__state, io__state).
:- mode hlds_out__write_mode(in, in, di, uo).

hlds_out__write_mode(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_consid(int, cons_id, io__state, io__state).
:- mode hlds_out__write_consid(in, in, di, uo).

hlds_out__write_consid(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

