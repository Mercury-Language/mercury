%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% hlds_out.nl

% Main authors: conway, fjh.

% There is quite a bit of overlap between the following modules:
%
%	hlds_out.nl
%	mercury_to_mercury.nl
%	term_io.nl
%
% mercury_to_mercury.nl prints the parse tree data structure defined
% in prog_io.nl.  hlds_out.nl does a similar task, but for the data
% structure defined in hlds.nl.  term_io.nl prints terms.

% There are two different ways of printing variables.
% One way uses the names Var', Var'', etc. which are generated
% by the compiler.  The other way converts all names back into
% a format allowed as source code.  Currently this module calls
% mercury_to_mercury.nl, which uses the second method, rather
% than term_io.nl, which uses the first method.  We should 
% think about using an option to specify which method is used.

%-----------------------------------------------------------------------------%

:- module hlds_out.
:- interface.
:- import_module int, io, hlds.

%-----------------------------------------------------------------------------%

:- pred hlds_out__write_type_id(type_id, io__state, io__state).
:- mode hlds_out__write_type_id(in, di, uo) is det.

:- pred hlds_out__write_cons_id(cons_id, io__state, io__state).
:- mode hlds_out__write_cons_id(in, di, uo) is det.

:- pred hlds_out__write_pred_id(module_info, pred_id, io__state, io__state).
:- mode hlds_out__write_pred_id(in, in, di, uo) is det.

:- pred hlds_out__write_pred_call_id(pred_call_id, io__state, io__state).
:- mode hlds_out__write_pred_call_id(in, di, uo) is det.

:- pred hlds_out__write_unify_context(unify_context, term__context,
				io__state, io__state).
:- mode hlds_out__write_unify_context(in, in, di, uo) is det.

:- pred hlds_out__write_category(category, io__state, io__state).
:- mode hlds_out__write_category(in, di, uo) is det.

%-----------------------------------------------------------------------------%

	% print out an entire hlds structure.

:- pred hlds_out__write_hlds(int, module_info, io__state, io__state).
:- mode hlds_out__write_hlds(in, in, in, out) is det.

	% print out an hlds goal.

:- pred hlds_out__write_goal(hlds__goal, module_info, varset, int,
				io__state, io__state).
:- mode hlds_out__write_goal(in, in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, list, require, std_util, term, term_io, prog_out.
:- import_module mercury_to_mercury, prog_io, globals, options, set, varset.
:- import_module prog_util.

hlds_out__write_type_id(Name - Arity) -->
	prog_out__write_sym_name(Name),
	io__write_string("/"),
	io__write_int(Arity).

hlds_out__write_cons_id(cons(Name, Arity)) -->
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity).
hlds_out__write_cons_id(int_const(Int)) -->
	io__write_int(Int).
hlds_out__write_cons_id(string_const(String)) -->
	io__write_char('"'),
	io__write_string(String),
	io__write_char('"').
hlds_out__write_cons_id(float_const(Float)) -->
	io__write_float(Float).

hlds_out__write_pred_id(ModuleInfo, PredId) -->
	% XXX module name
	%%% { predicate_module(ModuleInfo, PredId, Module) },
	{ predicate_name(ModuleInfo, PredId, Name) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
	%%% io__write_string(Module),
	%%% io__write_string(":"),
	io__write_string(Name),
	io__write_string("/"),
	io__write_int(Arity).

hlds_out__write_pred_call_id(Name / Arity) -->
	( { Name = qualified(ModuleName, _) } ->
		io__write_string(ModuleName),
		io__write_char(':')
	;
		[]
	),
	{ unqualify_name(Name, PredName) },
	io__write_string(PredName),
	io__write_char('/'),
	io__write_int(Arity).

%-----------------------------------------------------------------------------%

hlds_out__write_unify_context(unify_context(MainContext, RevSubContexts),
		Context) -->
	hlds_out__write_unify_main_context(MainContext, Context),
	{ list__reverse(RevSubContexts, SubContexts) },
	hlds_out__write_unify_sub_contexts(SubContexts, Context).

:- pred hlds_out__write_unify_main_context(unify_main_context, term__context,
						io__state, io__state).
:- mode hlds_out__write_unify_main_context(in, in, di, uo) is det.

hlds_out__write_unify_main_context(explicit, _) -->
	[].
hlds_out__write_unify_main_context(head(ArgNum), Context) -->
	prog_out__write_context(Context),
	io__write_string("  in argument "),
	io__write_int(ArgNum),
	io__write_string(" of clause head:\n").
hlds_out__write_unify_main_context(call(PredId, ArgNum), Context) -->
	prog_out__write_context(Context),
	io__write_string("  in argument "),
	io__write_int(ArgNum),
	io__write_string(" of call to predicate `"),
	hlds_out__write_pred_call_id(PredId),
	io__write_string("':\n").

:- pred hlds_out__write_unify_sub_contexts(unify_sub_contexts, term__context,
				io__state, io__state).
:- mode hlds_out__write_unify_sub_contexts(in, in, di, uo) is det.

hlds_out__write_unify_sub_contexts([], _) -->
	[].
hlds_out__write_unify_sub_contexts([ConsId - ArgNum | SubContexts], Context) -->
	prog_out__write_context(Context),
	io__write_string("  in argument "),
	io__write_int(ArgNum),
	io__write_string(" of functor `"),
	hlds_out__write_cons_id(ConsId),
	io__write_string("':\n"),
	hlds_out__write_unify_sub_contexts(SubContexts, Context).

%-----------------------------------------------------------------------------%

hlds_out__write_hlds(Indent, Module) -->
	{
		module_info_preds(Module, PredTable),
		module_info_types(Module, TypeTable),
		module_info_insts(Module, InstTable),
		module_info_modes(Module, ModeTable)
	},
	hlds_out__write_header(Indent, Module),
	io__write_string("\n"),
	hlds_out__write_types(Indent, TypeTable),
	io__write_string("\n"),
	hlds_out__write_insts(Indent, InstTable),
	io__write_string("\n"),
	hlds_out__write_modes(Indent, ModeTable),
	io__write_string("\n"),
	hlds_out__write_preds(Indent, Module, PredTable),
	io__write_string("\n"),
	hlds_out__write_footer(Indent, Module).

:- pred hlds_out__write_header(int, module_info, io__state, io__state).
:- mode hlds_out__write_header(in, in, in, out) is det.

hlds_out__write_header(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- module "),
	io__write_string(Name),
	io__write_string(".\n").

:- pred hlds_out__write_footer(int, module_info, io__state, io__state).
:- mode hlds_out__write_footer(in, in, di, uo) is det.

hlds_out__write_footer(Indent, Module) -->
	{ module_info_name(Module, Name) },
	hlds_out__write_indent(Indent),
	io__write_string(":- end_module "),
	io__write_string(Name),
	io__write_string(".\n").

:- pred hlds_out__write_preds(int, module_info, pred_table,
				io__state, io__state).
:- mode hlds_out__write_preds(in, in, in, di, uo) is det.

hlds_out__write_preds(Indent, ModuleInfo, PredTable) -->
	io__write_string("% predicates\n"),
	hlds_out__write_indent(Indent),
	{ map__keys(PredTable, PredIds) },
	hlds_out__write_preds_2(Indent, ModuleInfo, PredIds, PredTable).

:- pred hlds_out__write_preds_2(int, module_info, list(pred_id), pred_table,
			io__state, io__state).
:- mode hlds_out__write_preds_2(in, in, in, in, in, out) is det.

hlds_out__write_preds_2(Indent, ModuleInfo, PredIds0, PredTable) --> 
	(
		{ PredIds0 = [] }
	->
		[]
	;
		{ PredIds0 = [PredId|PredIds] },
		{ map__lookup(PredTable, PredId, PredInfo) },
		hlds_out__write_pred(Indent, ModuleInfo, PredId, PredInfo),
		hlds_out__write_preds_2(Indent, ModuleInfo, PredIds, PredTable)
	).

:- pred hlds_out__write_pred(int, module_info, pred_id, pred_info,
				io__state, io__state).
:- mode hlds_out__write_pred(in, in, in, in, in, out) is det.

hlds_out__write_pred(Indent, ModuleInfo, PredId, PredInfo) -->
	{ pred_info_arg_types(PredInfo, TVarSet, ArgTypes) },
	{ pred_info_clauses_info(PredInfo, ClausesInfo) },
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ pred_info_context(PredInfo, Context) },
	{ pred_info_name(PredInfo, PredName) },
	mercury_output_pred_type(TVarSet, unqualified(PredName), ArgTypes,
		unspecified, Context),
	{ pred_info_is_imported(PredInfo) ->
		Imported = yes
	;
		Imported = no
	},
	{ ClausesInfo = clauses_info(VarSet, VarTypes, HeadVars, Clauses) },
	hlds_out__write_var_types(Indent, VarSet, VarTypes, TVarSet),
	hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet, HeadVars,
			Clauses),
	hlds_out__write_procs(Indent, ModuleInfo, PredId, Imported, ProcTable),
	io__write_string("\n").

:- pred hlds_out__write_clauses(int, module_info, pred_id, varset, list(var),
				list(clause),
				io__state, io__state).
:- mode hlds_out__write_clauses(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet, HeadVars, Clauses0)
		-->
	(
		{ Clauses0 = [] }
	->
		[]
	;
		{ Clauses0 = [Clause|Clauses] },
		hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet,
			HeadVars, Clause),
		hlds_out__write_clauses(Indent, ModuleInfo, PredId, VarSet,
			HeadVars, Clauses)
	).

:- pred hlds_out__write_clause(int, module_info, pred_id, varset, list(var),
				clause, io__state, io__state).
:- mode hlds_out__write_clause(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_clause(Indent, ModuleInfo, PredId, VarSet, HeadVars, Clause) -->
	{
		Clause = clause(
			Modes,
			Goal,
			_Context
		),
		Indent1 is Indent + 1
	},
	globals__lookup_option(very_verbose, bool(VeryVerbose)),
	( { VeryVerbose = yes } ->
		hlds_out__write_indent(Indent),
		io__write_string("% Modes for which this clause applies: "),
		hlds_out__write_intlist(Modes),
		io__write_string("\n")
	;
		[]
	),
	hlds_out__write_clause_head(ModuleInfo, PredId, VarSet, HeadVars),
	( { Goal = conj([]) - _GoalInfo } ->
		[]
	;
		io__write_string(" :-\n"),
		hlds_out__write_indent(Indent1),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1)
	),
	io__write_string(".\n").

:- pred hlds_out__write_intlist(list(int), io__state, io__state).
:- mode hlds_out__write_intlist(in, in, out) is det.

hlds_out__write_intlist(IntList) -->
	(
		{ IntList = [] }
	->
		io__write_string("[]")
	;
		io__write_string("[ "),
		hlds_out__write_intlist_2(IntList),
		io__write_string("]")
	).

:- pred hlds_out__write_intlist_2(list(int), io__state, io__state).
:- mode hlds_out__write_intlist_2(in, in, out) is det.

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

:- pred hlds_out__write_clause_head(module_info, pred_id, varset, list(var),
					io__state, io__state).
:- mode hlds_out__write_clause_head(in, in, in, in, di, uo) is det.

hlds_out__write_clause_head(ModuleInfo, PredId, VarSet, HeadVars) -->
	{
		predicate_name(ModuleInfo, PredId, PredName),
		term__context_init(0, Context),
		term__var_list_to_term_list(HeadVars, HeadTerms),
		Term = term__functor(term__atom(PredName), HeadTerms, Context)
	},
	mercury_output_term(Term, VarSet).

hlds_out__write_goal(Goal - GoalInfo, ModuleInfo, VarSet, Indent) -->
	globals__lookup_option(verbose_dump_hlds, bool(Verbose)),
	( { Verbose = yes } ->
		{ goal_info_get_nonlocals(GoalInfo, NonLocalsSet) },
		{ set__to_sorted_list(NonLocalsSet, NonLocalsList) },
		io__write_string("% nonlocals: "),
		mercury_output_vars(NonLocalsList, VarSet),
		mercury_output_newline(Indent),
		{ goal_info_delta_liveness(GoalInfo, Births - Deaths) },
		{ set__to_sorted_list(Births, BirthList) },
		{ set__to_sorted_list(Deaths, DeathList) },
		io__write_string("% births: "),
		mercury_output_vars(BirthList, VarSet),
		mercury_output_newline(Indent),
		io__write_string("% deaths: "),
		mercury_output_vars(DeathList, VarSet),
		mercury_output_newline(Indent),
		io__write_string("% determinism: "),
		{ goal_info_inferred_determinism(GoalInfo, Category) },
		hlds_out__write_category(Category),
		mercury_output_newline(Indent),
		io__write_string("% new insts: "),
		{ goal_info_get_instmap_delta(GoalInfo, InstMapDelta) },
		hlds_out__write_instmap(InstMapDelta, VarSet),
		mercury_output_newline(Indent)
	;
		[]
	),
	hlds_out__write_goal_2(Goal, ModuleInfo, VarSet, Indent).

:- pred hlds_out__write_goal_2(hlds__goal_expr, module_info, varset, int,
					io__state, io__state).
:- mode hlds_out__write_goal_2(in, in, in, in, di, uo) is det.

hlds_out__write_goal_2(switch(Var, CasesList, _), ModuleInfo, VarSet, Indent)
		-->
	io__write_string("( % switch on `"),
	mercury_output_var(Var, VarSet),
	io__write_string("'"),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	( { CasesList = [Case | Cases] } ->
		hlds_out__write_case(Case, Var, ModuleInfo, VarSet, Indent1),
		hlds_out__write_cases(Cases, Var, ModuleInfo, VarSet, Indent)
	;
		io__write_string("fail")
	),
	mercury_output_newline(Indent),
	io__write_string(")").

hlds_out__write_goal_2(some(Vars, Goal), ModuleInfo, VarSet, Indent) -->
	( { Vars = [] } ->
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent)
	;
		io__write_string("some ["),
		mercury_output_vars(Vars, VarSet),
		io__write_string("] ("),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1),
		mercury_output_newline(Indent),
		io__write_string(")")
	).

hlds_out__write_goal_2(if_then_else(Vars, A, B, C), ModuleInfo, VarSet, Indent)
		-->
	io__write_string("(if"),
	hlds_out__write_some(Vars, VarSet),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	hlds_out__write_goal(A, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("then"),
	mercury_output_newline(Indent1),
	hlds_out__write_goal(B, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string("else"),
	mercury_output_newline(Indent1),
	hlds_out__write_goal(C, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

hlds_out__write_goal_2(not(Vars, Goal), ModuleInfo, VarSet, Indent) -->
	io__write_string("\+"),
	hlds_out__write_some(Vars, VarSet),
	io__write_string(" ("),
	{ Indent1 is Indent + 1 },
	mercury_output_newline(Indent1),
	hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1),
	mercury_output_newline(Indent),
	io__write_string(")").

hlds_out__write_goal_2(conj(List), ModuleInfo, VarSet, Indent) -->
	( { List = [Goal | Goals] } ->
		globals__lookup_option(verbose_dump_hlds, bool(Verbose)),
		( { Verbose = yes } ->
			{ Indent1 is Indent + 1 },
			io__write_string("( % conjunction"),
			mercury_output_newline(Indent1),
			hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1),
			hlds_out__write_conj(Goals, ModuleInfo, VarSet,
				Indent1),
			mercury_output_newline(Indent),
			io__write_string(")")
		;
			hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent),
			hlds_out__write_conj(Goals, ModuleInfo, VarSet, Indent)
		)
	;
		io__write_string("true")
	).

hlds_out__write_goal_2(disj(List), ModuleInfo, VarSet, Indent) -->
	( { List = [Goal | Goals] } ->
		io__write_string("("),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1),
		hlds_out__write_disj(Goals, ModuleInfo, VarSet, Indent),
		mercury_output_newline(Indent),
		io__write_string(")")
	;
		io__write_string("fail")
	).

hlds_out__write_goal_2(call(_PredId, _ProcId, Args, _, PredName), _ModuleInfo,
		VarSet, _Indent) -->
		% XXX we should print more info here
	{ unqualify_name(PredName, Name) },
	{ term__context_init(0, Context) },
	mercury_output_term(term__functor(term__atom(Name), Args, Context),
				VarSet).

hlds_out__write_goal_2(unify(A, B, _, _, _), _ModuleInfo, VarSet, _Indent) -->
	mercury_output_term(A, VarSet),
	io__write_string(" = "),
	mercury_output_term(B, VarSet).

:- pred hlds_out__write_conj(list(hlds__goal), module_info, varset, int,
				io__state, io__state).
:- mode hlds_out__write_conj(in, in, in, in, di, uo) is det.

hlds_out__write_conj(GoalList, ModuleInfo, VarSet, Indent) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		io__write_string(","),
		mercury_output_newline(Indent),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent),
		hlds_out__write_conj(Goals, ModuleInfo, VarSet, Indent)
	;
		[]
	).

:- pred hlds_out__write_disj(list(hlds__goal), module_info, varset, int,
				io__state, io__state).
:- mode hlds_out__write_disj(in, in, in, in, di, uo) is det.

hlds_out__write_disj(GoalList, ModuleInfo, VarSet, Indent) -->
	(
		{ GoalList = [Goal | Goals] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1),
		hlds_out__write_disj(Goals, ModuleInfo, VarSet, Indent)
	;
		[]
	).

:- pred hlds_out__write_case(case, var, module_info, varset, int,
				io__state, io__state).
:- mode hlds_out__write_case(in, in, in, in, in, di, uo) is det.

hlds_out__write_case(case(ConsId, Goal), Var, ModuleInfo, VarSet, Indent) -->
	mercury_output_var(Var, VarSet),
	io__write_string(" has functor "),
	hlds_out__write_cons_id(ConsId),
	io__write_string(","),
	mercury_output_newline(Indent),
	hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent).

:- pred hlds_out__write_cases(list(case), var, module_info, varset, int,
				io__state, io__state).
:- mode hlds_out__write_cases(in, in, in, in, in, di, uo) is det.

hlds_out__write_cases(CasesList, Var, ModuleInfo, VarSet, Indent) -->
	(
		{ CasesList = [Case | Cases] }
	->
		mercury_output_newline(Indent),
		io__write_string(";"),
		{ Indent1 is Indent + 1 },
		mercury_output_newline(Indent1),
		hlds_out__write_case(Case, Var, ModuleInfo, VarSet, Indent1),
		hlds_out__write_cases(Cases, Var, ModuleInfo, VarSet, Indent)
	;
		[]
	).

:- pred hlds_out__write_some(list(var), varset, io__state, io__state).
:- mode hlds_out__write_some(in, in, di, uo) is det.

	% quantification is all implicit by the time we get to the hlds.

hlds_out__write_some(_Vars, _VarSet) --> [].

:- pred hlds_out__write_builtin(is_builtin, io__state, io__state).
:- mode hlds_out__write_builtin(in, in, out) is det.

hlds_out__write_builtin(is_builtin) -->
	io__write_string("is_builtin").
hlds_out__write_builtin(not_builtin) -->
	io__write_string("not_builtin").

:- pred hlds_out__write_instmap(instmap, varset, io__state, io__state).
:- mode hlds_out__write_instmap(in, in, di, uo) is det.

hlds_out__write_instmap(unreachable, _) -->
	io__write_string("unreachable").
hlds_out__write_instmap(reachable(InstMapping), VarSet) -->
	{ map__to_assoc_list(InstMapping, AssocList) },
	hlds_out__write_instmap_2(AssocList, VarSet).

:- pred hlds_out__write_instmap_2(assoc_list(var, inst), varset,
					io__state, io__state).
:- mode hlds_out__write_instmap_2(in, in, di, uo) is det.

hlds_out__write_instmap_2([], _) --> [].
hlds_out__write_instmap_2([Var - Inst | Rest], VarSet) -->
	mercury_output_var(Var, VarSet),
	io__write_string(" -> "),
	{ varset__init(InstVarSet) },
	mercury_output_inst(Inst, InstVarSet),
	( { Rest = [] } ->
		[]
	;
		io__write_string(", "),
		hlds_out__write_instmap_2(Rest, VarSet)
	).

:- pred hlds_out__write_var_types(int, varset, map(var, type), varset,
					io__state, io__state).
:- mode hlds_out__write_var_types(in, in, in, in, di, uo) is det.

hlds_out__write_var_types(Indent, VarSet, VarTypes, TVarSet) -->
	{ map__keys(VarTypes, Vars) },
	hlds_out__write_var_types_2(Vars, Indent, VarSet, VarTypes, TVarSet).

:- pred hlds_out__write_var_types_2(list(var), int, varset, map(var, type),
					varset, io__state, io__state).
:- mode hlds_out__write_var_types_2(in, in, in, in, in, di, uo) is det.

hlds_out__write_var_types_2([], _, _, _, _) --> [].
hlds_out__write_var_types_2([Var | Vars], Indent, VarSet, VarTypes, TypeVarSet)
		-->
	{ map__lookup(VarTypes, Var, Type) },
	hlds_out__write_indent(Indent),
	io__write_string("% "),
	mercury_output_var(Var, VarSet),
	io__write_string(" :: "),
	mercury_output_term(Type, TypeVarSet),
	io__write_string("\n"),
	hlds_out__write_var_types_2(Vars, Indent, VarSet, VarTypes, TypeVarSet).
	
:- pred hlds_out__write_types(int, type_table, io__state, io__state).
:- mode hlds_out__write_types(in, in, in, out) is det.

hlds_out__write_types(Indent, _X) -->
	hlds_out__write_indent(Indent),
	io__write_string("% types (sorry, output of types not implemented)\n").

:- pred hlds_out__write_insts(int, inst_table, io__state, io__state).
:- mode hlds_out__write_insts(in, in, in, out) is det.

hlds_out__write_insts(Indent, _X) -->
	hlds_out__write_indent(Indent),
	io__write_string("% insts (sorry, output of insts not implemented)\n").

:- pred hlds_out__write_modes(int, mode_table, io__state, io__state).
:- mode hlds_out__write_modes(in, in, in, out) is det.

hlds_out__write_modes(Indent, _X) -->
	hlds_out__write_indent(Indent),
	io__write_string("% modes (sorry, output of modes not implemented)\n").

:- pred hlds_out__write_mode_list(int, list(mode), io__state, io__state).
:- mode hlds_out__write_mode_list(in, in, in, out) is det.

hlds_out__write_mode_list(Indent, _X) -->
	hlds_out__write_indent(Indent),
	% XXX
	io__write_string("\n").

:- pred hlds_out__write_procs(int, module_info, pred_id, bool, proc_table,
				io__state, io__state).
:- mode hlds_out__write_procs(in, in, in, in, in, di, uo) is det.

hlds_out__write_procs(Indent, ModuleInfo, PredId, IsImported, ProcTable) -->
	{ map__keys(ProcTable, ProcIds) },
	hlds_out__write_procs_2(ProcIds, ModuleInfo, Indent, PredId,
		IsImported, ProcTable).

:- pred hlds_out__write_procs_2(list(proc_id), module_info, int, pred_id,
				bool, proc_table, io__state, io__state).
:- mode hlds_out__write_procs_2(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_procs_2([], _ModuleInfo, _Indent, _PredId, _, _ProcTable) -->
	[].
hlds_out__write_procs_2([ProcId | ProcIds], ModuleInfo, Indent, PredId,
		IsImported,
		ProcTable) --> 
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	hlds_out__write_proc(Indent, ModuleInfo, PredId, ProcId,
		IsImported, ProcInfo),
	hlds_out__write_procs_2(ProcIds, ModuleInfo, Indent, PredId,
		IsImported, ProcTable).

:- pred hlds_out__write_proc(int, module_info, pred_id, proc_id, bool,
				proc_info, io__state, io__state).
:- mode hlds_out__write_proc(in, in, in, in, in, in, di, uo) is det.

hlds_out__write_proc(Indent, ModuleInfo, PredId, ProcId, IsImported, Proc) -->
	{ proc_info_declared_determinism(Proc, DeclaredDeterminism) },
	{ proc_info_inferred_determinism(Proc, Category) },
	{ proc_info_variables(Proc, VarSet) },
	{ proc_info_headvars(Proc, HeadVars) },
	{ proc_info_argmodes(Proc, HeadModes) },
	{ proc_info_goal(Proc, Goal) },
	{ proc_info_context(Proc, ModeContext) },
	{ Indent1 is Indent + 1 },

	hlds_out__write_indent(Indent1),
	io__write_string("% mode number "),
	io__write_int(ProcId),
	io__write_string(" of "),
	hlds_out__write_pred_id(ModuleInfo, PredId),
	io__write_string(" ("),
	hlds_out__write_category(Category),
	io__write_string("):\n"),

	hlds_out__write_indent(Indent),
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ varset__init(ModeVarSet) },
	mercury_output_mode_decl(ModeVarSet, unqualified(PredName), HeadModes,
		DeclaredDeterminism, ModeContext),

	( { IsImported = no } ->
		hlds_out__write_indent(Indent),
		hlds_out__write_clause_head(ModuleInfo, PredId, VarSet,
						HeadVars),
		io__write_string(" :-\n"),

		hlds_out__write_indent(Indent1),
		hlds_out__write_goal(Goal, ModuleInfo, VarSet, Indent1),
		io__write_string(".\n")
	;
		[]
	).

/*********

:- pred hlds_out__write_unification(int, unification, io__state, io__state).
:- mode hlds_out__write_unification(in, in, di, uo) is det.

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

:- pred hlds_out__write_unimode(int, unify_mode, io__state, io__state).
:- mode hlds_out__write_unimode(in, in, in, out) is det.

hlds_out__write_unimode(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

***********/

:- pred hlds_out__write_anything(int, _AnyType, io__state, io__state).
:- mode hlds_out__write_anything(in, in, di, uo) is det.

hlds_out__write_anything(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

:- pred hlds_out__write_varnames(int, map(var, string), io__state, io__state).
:- mode hlds_out__write_varnames(in, in, in, out) is det.

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
:- mode hlds_out__write_varnames_2(in, in, in, out) is det.

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

:- pred hlds_out__write_vartypes(int, map(var, type), io__state, io__state).
:- mode hlds_out__write_vartypes(in, in, di, uo) is det.

hlds_out__write_vartypes(Indent, X) -->
	hlds_out__write_indent(Indent),
	io__write_anything(X),
	io__write_string("\n").

hlds_out__write_category(deterministic) -->
	io__write_string("det").
hlds_out__write_category(semideterministic) -->
	io__write_string("semidet").
hlds_out__write_category(nondeterministic) -->
	io__write_string("nondet").

:- pred hlds_out__write_indent(int, io__state, io__state).
:- mode hlds_out__write_indent(in, di, uo) is det.

hlds_out__write_indent(Indent) -->
	(
		{ Indent = 0 }
	->
		[]
	;
		io__write_char('\t'),
		{ Indent1 is Indent - 1 },
		hlds_out__write_indent(Indent1)
	).

:- pred hlds_out__write_consid(int, cons_id, io__state, io__state).
:- mode hlds_out__write_consid(in, in, di, uo) is det.

hlds_out__write_consid(Indent, X) -->
	hlds_out__write_indent(Indent),
	hlds_out__write_cons_id(X),
	io__write_string("\n").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
