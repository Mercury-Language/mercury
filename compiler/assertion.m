%-----------------------------------------------------------------------------%
% Copyright (C) 1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Module: assertion
%
% Main authors: petdr
%
% This module is an abstract interface to the assertion table.
% Note that this is a first design and will probably change
% substantially in the future.
%
%-----------------------------------------------------------------------------%

:- module (assertion).

:- interface.

:- import_module hlds_data, hlds_goal, hlds_module, hlds_pred, prog_data.
:- import_module io, std_util.

	%
	% assertion__goal
	%
	% Get the hlds_goal which represents the assertion.
	%
:- pred assertion__goal(assert_id::in, module_info::in, hlds_goal::out) is det.

	%
	% assertion__record_preds_used_in
	%
	% Record into the pred_info of each pred used in the assertion
	% the assert_id.
	%
:- pred assertion__record_preds_used_in(hlds_goal::in, assert_id::in,
		module_info::in, module_info::out) is det.

	% 
	% assertion__is_commutativity_assertion(Id, MI, Vs, CVs)
	%
	% Does the assertion represented by the assertion id, Id,
	% state the commutativity of a pred/func?
	% We extend the usual definition of commutativity to apply to
	% predicates or functions with more than two arguments as
	% follows by allowing extra arguments which must be invariant.
	% If so, this predicate returns (in CVs) the two variables which
	% can be swapped in order if it was a call to Vs.
	%
	% The assertion must be in a form similar to this
	% 	all [Is,A,B,C] ( p(Is,A,B,C) <=> p(Is,B,A,C) )
	% for the predicate to return true (note that the invariant
	% arguments, Is, can be any where providing they are in
	% identical locations on both sides of the equivalence).
	%
:- pred assertion__is_commutativity_assertion(assert_id::in, module_info::in,
		prog_vars::in, pair(prog_var)::out) is semidet.

	%
	% assertion__in_interface_check
	%
	% Ensure that an assertion which is defined in an interface
	% doesn't refer to any constructors, functions and predicates
	% defined in the implementation of that module.
	%
:- pred assertion__in_interface_check(hlds_goal::in, pred_info::in,
		module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals, goal_util, hlds_out, options, prog_out, type_util.
:- import_module bool, list, map, require, set, std_util.

:- type subst == map(prog_var, prog_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

assertion__is_commutativity_assertion(AssertId, Module, CallVars,
		CommutativeVars) :-
	assertion__goal(AssertId, Module, Goal),
	equivalent(Goal, P, Q),
	P = call(PredId, _, VarsP, _, _, _) - _,
	Q = call(PredId, _, VarsQ, _, _, _) - _,
	commutative_var_ordering(VarsP, VarsQ, CallVars, CommutativeVars).

	%
	% commutative_var_ordering(Ps, Qs, Vs, CommutativeVs)
	%
	% Check that the two list of variables are identical except that
	% the position of two variables has been swapped.
	% e.g [A,B,C] and [B,A,C] is true.
	% It also takes a list of variables, Vs, to a call and returns
	% the two variables in that list that can be swapped, ie [A,B].
	%
:- pred commutative_var_ordering(prog_vars::in, prog_vars::in,
		prog_vars::in, pair(prog_var)::out) is semidet.

commutative_var_ordering([P|Ps], [Q|Qs], [V|Vs], CommutativeVars) :-
	(
		P = Q
	->
		commutative_var_ordering(Ps, Qs, Vs, CommutativeVars)
	;
		commutative_var_ordering_2(P, Q, Ps, Qs, Vs, CallVarB),
		CommutativeVars = V - CallVarB
	).

:- pred commutative_var_ordering_2(prog_var::in, prog_var::in, prog_vars::in,
		prog_vars::in, prog_vars::in, prog_var::out) is semidet.

commutative_var_ordering_2(VarP, VarQ, [P|Ps], [Q|Qs], [V|Vs], CallVarB) :-
	(
		P = Q
	->
		commutative_var_ordering_2(VarP, VarQ, Ps, Qs, Vs, CallVarB)
	;
		CallVarB = V,
		P = VarQ,
		Q = VarP,
		Ps = Qs
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

assertion__goal(AssertId, Module, Goal) :-
	module_info_assertion_table(Module, AssertTable),
	assertion_table_lookup(AssertTable, AssertId, PredId),
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_clauses_info(PredInfo, ClausesInfo),
	clauses_info_clauses(ClausesInfo, Clauses),
	(
		Clauses = [clause(_ProcIds, Goal0, _Context)]
	->
		assertion__normalise_goal(Goal0, Goal)
	;
		error("assertion__goal: not an assertion")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred implies(hlds_goal::in, hlds_goal::out, hlds_goal::out) is semidet.

implies(Goal, P, Q) :-
		% Goal = (P => Q)
	Goal = not(conj([P, NotQ]) - _) - _,
	NotQ = not(Q) - _.

:- pred equivalent(hlds_goal::in, hlds_goal::out, hlds_goal::out) is semidet.

equivalent(Goal, P, Q) :-
		% Goal = P <=> Q
	Goal = conj([A, B]) - _GoalInfo,
	map__init(Subst),
	implies(A, PA, QA),
	implies(B, QB, PB),
	equal_goals(PA, PB, Subst, _),
	equal_goals(QA, QB, Subst, _),
	P = PA,
	Q = QA.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% equal_goals(GA, GB)
	%
	% Do these two goals represent the same hlds_goal modulo
	% renaming.
	%
:- pred equal_goals(hlds_goal::in, hlds_goal::in,
		subst::in, subst::out) is semidet.

equal_goals(conj(GoalAs) - _, conj(GoalBs) - _, Subst0, Subst) :-
	equal_goals_list(GoalAs, GoalBs, Subst0, Subst).
equal_goals(call(PredId, _, VarsA, _, _, _) - _,
		call(PredId, _, VarsB, _, _, _) - _, Subst0, Subst) :-
	equal_vars(VarsA, VarsB, Subst0, Subst).
equal_goals(generic_call(Type, VarsA, _, _) - _,
		generic_call(Type, VarsB, _, _) - _, Subst0, Subst) :-
	equal_vars(VarsA, VarsB, Subst0, Subst).
equal_goals(switch(Var, CanFail, CasesA, _) - _,
		switch(Var, CanFail, CasesB, _) - _, Subst0, Subst) :-
	equal_goals_cases(CasesA, CasesB, Subst0, Subst).
equal_goals(unify(VarA, RHSA, _, _, _) - _, unify(VarB, RHSB, _, _, _) - _,
		Subst0, Subst) :-
	equal_vars([VarA], [VarB], Subst0, Subst1),
	equal_unification(RHSA, RHSB, Subst1, Subst).
equal_goals(disj(GoalAs, _) - _, disj(GoalBs, _) - _, Subst0, Subst) :-
	equal_goals_list(GoalAs, GoalBs, Subst0, Subst).
equal_goals(not(GoalA) - _, not(GoalB) - _, Subst0, Subst) :-
	equal_goals(GoalA, GoalB, Subst0, Subst).
equal_goals(some(VarsA, _, GoalA) - _, some(VarsB, _, GoalB) - _,
		Subst0, Subst) :-
	equal_vars(VarsA, VarsB, Subst0, Subst1),
	equal_goals(GoalA, GoalB, Subst1, Subst).
equal_goals(if_then_else(VarsA, IfA, ThenA, ElseA, _) - _,
		if_then_else(VarsB, IfB, ThenB, ElseB, _) - _, Subst0, Subst) :-
	equal_vars(VarsA, VarsB, Subst0, Subst1),
	equal_goals(IfA, IfB, Subst1, Subst2),
	equal_goals(ThenA, ThenB, Subst2, Subst3),
	equal_goals(ElseA, ElseB, Subst3, Subst).
equal_goals(pragma_c_code(Attribs, PredId, _, VarsA, _, _, _) - _,
		pragma_c_code(Attribs, PredId, _, VarsB, _, _, _) - _,
		Subst0, Subst) :-
	equal_vars(VarsA, VarsB, Subst0, Subst).
equal_goals(par_conj(GoalAs, _) - _, par_conj(GoalBs, _) - _, Subst0, Subst) :-
	equal_goals_list(GoalAs, GoalBs, Subst0, Subst).
equal_goals(bi_implication(LeftGoalA, RightGoalA) - _,
	    bi_implication(LeftGoalB, RightGoalB) - _, Subst0, Subst) :-
	equal_goals(LeftGoalA, LeftGoalB, Subst0, Subst1),
	equal_goals(RightGoalA, RightGoalB, Subst1, Subst).

:- pred equal_vars(prog_vars::in, prog_vars::in, subst::in,
		subst::out) is semidet.

equal_vars([], [], Subst, Subst).
equal_vars([VA | VAs], [VB | VBs], Subst0, Subst) :-
	(
		map__search(Subst0, VA, SubstVA)
	->
		SubstVA = VB,
		equal_vars(VAs, VBs, Subst0, Subst)
	;
		map__insert(Subst0, VA, VB, Subst1),
		equal_vars(VAs, VBs, Subst1, Subst)
	).

:- pred equal_unification(unify_rhs::in, unify_rhs::in, subst::in,
		subst::out) is semidet.

equal_unification(var(A), var(B), Subst0, Subst) :-
	equal_vars([A], [B], Subst0, Subst).
equal_unification(functor(ConsId, VarsA), functor(ConsId, VarsB),
		Subst0, Subst) :-
	equal_vars(VarsA, VarsB, Subst0, Subst).
equal_unification(lambda_goal(PredOrFunc, EvalMethod, FixModes, NLVarsA, LVarsA,
			Modes, Det, GoalA),
		lambda_goal(PredOrFunc, EvalMethod, FixModes, NLVarsB, LVarsB,
			Modes, Det, GoalB),
		Subst0, Subst) :-
	equal_vars(NLVarsA, NLVarsB, Subst0, Subst1),
	equal_vars(LVarsA, LVarsB, Subst1, Subst2),
	equal_goals(GoalA, GoalB, Subst2, Subst).


:- pred equal_goals_list(hlds_goals::in, hlds_goals::in, subst::in,
		subst::out) is semidet.

equal_goals_list([], [], Subst, Subst).
equal_goals_list([GoalA | GoalAs], [GoalB | GoalBs], Subst0, Subst) :-
	equal_goals(GoalA, GoalB, Subst0, Subst1),
	equal_goals_list(GoalAs, GoalBs, Subst1, Subst).

:- pred equal_goals_cases(list(case)::in, list(case)::in, subst::in,
		subst::out) is semidet.

equal_goals_cases([], [], Subst, Subst).
equal_goals_cases([CaseA | CaseAs], [CaseB | CaseBs], Subst0, Subst) :-
	CaseA = case(ConsId, GoalA),
	CaseB = case(ConsId, GoalB),
	equal_goals(GoalA, GoalB, Subst0, Subst1),
	equal_goals_cases(CaseAs, CaseBs, Subst1, Subst).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

assertion__record_preds_used_in(Goal, AssertId, Module0, Module) :-
		% Explicit lambda expression needed since
		% goal_calls_pred_id has multiple modes.
	P = (pred(PredId::out) is nondet :- goal_calls_pred_id(Goal, PredId)),
	solutions(P, PredIds),
	list__foldl(update_pred_info(AssertId), PredIds, Module0, Module).

%-----------------------------------------------------------------------------%

	%
	% update_pred_info(Id, A, M0, M)
	%
	% Record in the pred_info pointed to by Id that that predicate
	% is used in the assertion pointed to by A.
	%
:- pred update_pred_info(assert_id::in, pred_id::in, module_info::in,
		module_info::out) is det.

update_pred_info(AssertId, PredId, Module0, Module) :-
	module_info_pred_info(Module0, PredId, PredInfo0),
	pred_info_get_assertions(PredInfo0, Assertions0),
	set__insert(Assertions0, AssertId, Assertions),
	pred_info_set_assertions(PredInfo0, Assertions, PredInfo),
	module_info_set_pred_info(Module0, PredId, PredInfo, Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	%
	% assertion__normalise_goal
	%
	% Place a hlds_goal into a standard form.  Currently all the
	% code does is replace conj([G]) with G.
	%
:- pred assertion__normalise_goal(hlds_goal::in, hlds_goal::out) is det.

assertion__normalise_goal(call(A,B,C,D,E,F) - GI, call(A,B,C,D,E,F) - GI).
assertion__normalise_goal(generic_call(A,B,C,D) - GI, generic_call(A,B,C,D)-GI).
assertion__normalise_goal(unify(A,B,C,D,E) - GI, unify(A,B,C,D,E) - GI).
assertion__normalise_goal(pragma_c_code(A,B,C,D,E,F,G) - GI,
		pragma_c_code(A,B,C,D,E,F,G) - GI).
assertion__normalise_goal(conj(Goals0) - GI, conj(Goals) - GI) :-
	assertion__normalise_conj(Goals0, Goals).
assertion__normalise_goal(switch(A,B,Case0s,D) - GI, switch(A,B,Cases,D)-GI) :-
	assertion__normalise_cases(Case0s, Cases).
assertion__normalise_goal(disj(Goal0s,B) - GI, disj(Goals,B) - GI) :-
	assertion__normalise_goals(Goal0s, Goals).
assertion__normalise_goal(not(Goal0) - GI, not(Goal) - GI) :-
	assertion__normalise_goal(Goal0, Goal).
assertion__normalise_goal(some(A,B,Goal0) - GI, some(A,B,Goal) - GI) :-
	assertion__normalise_goal(Goal0, Goal).
assertion__normalise_goal(if_then_else(A, If0, Then0, Else0, E) - GI,
		if_then_else(A, If, Then, Else, E) - GI) :-
	assertion__normalise_goal(If0, If),
	assertion__normalise_goal(Then0, Then),
	assertion__normalise_goal(Else0, Else).
assertion__normalise_goal(par_conj(Goal0s,B) - GI, par_conj(Goals,B) - GI) :-
	assertion__normalise_goals(Goal0s, Goals).
assertion__normalise_goal(bi_implication(LHS0, RHS0) - GI,
		bi_implication(LHS, RHS) - GI) :-
	assertion__normalise_goal(LHS0, LHS),
	assertion__normalise_goal(RHS0, RHS).

%-----------------------------------------------------------------------------%

:- pred assertion__normalise_conj(hlds_goals::in, hlds_goals::out) is det.

assertion__normalise_conj([], []).
assertion__normalise_conj([Goal0 | Goal0s], Goals) :-
	goal_to_conj_list(Goal0, ConjGoals),
	assertion__normalise_conj(Goal0s, Goal1s),
	list__append(ConjGoals, Goal1s, Goals).

:- pred assertion__normalise_cases(list(case)::in, list(case)::out) is det.

assertion__normalise_cases([], []).
assertion__normalise_cases([Case0 | Case0s], [Case | Cases]) :-
	Case0 = case(ConsId, Goal0),
	assertion__normalise_goal(Goal0, Goal),
	Case = case(ConsId, Goal),
	assertion__normalise_cases(Case0s, Cases).

:- pred assertion__normalise_goals(hlds_goals::in, hlds_goals::out) is det.

assertion__normalise_goals([], []).
assertion__normalise_goals([Goal0 | Goal0s], [Goal | Goals]) :-
	assertion__normalise_goal(Goal0, Goal),
	assertion__normalise_goals(Goal0s, Goals).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

assertion__in_interface_check(call(PredId,_,_,_,_,SymName) - GoalInfo,
		_PredInfo, Module0, Module) -->
	{ module_info_pred_info(Module0, PredId, CallPredInfo)  },
	{ pred_info_import_status(CallPredInfo, ImportStatus) },
	(
		{ is_defined_in_implementation_section(ImportStatus, yes) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		{ pred_info_get_is_pred_or_func(CallPredInfo, PredOrFunc) },
		{ pred_info_arity(CallPredInfo, Arity) },
		write_assertion_interface_error(Context,
				call(PredOrFunc, SymName, Arity),
				Module0, Module)
	;
		{ Module = Module0 }
	).
assertion__in_interface_check(generic_call(_,_,_,_) - _, _,
		Module, Module) --> [].
assertion__in_interface_check(unify(Var,RHS,_,_,_) - GoalInfo,
		PredInfo, Module0, Module) -->
	{ goal_info_get_context(GoalInfo, Context) },
	assertion__in_interface_check_unify_rhs(RHS, Var, Context,
			PredInfo, Module0, Module).
assertion__in_interface_check(pragma_c_code(_,PredId,_,_,_,_,_) - GoalInfo,
		_PredInfo, Module0, Module) -->
	{ module_info_pred_info(Module0, PredId, PragmaPredInfo) },
	{ pred_info_import_status(PragmaPredInfo, ImportStatus) },
	(
		{ is_defined_in_implementation_section(ImportStatus, yes) }
	->
		{ goal_info_get_context(GoalInfo, Context) },
		{ pred_info_get_is_pred_or_func(PragmaPredInfo, PredOrFunc) },
		{ pred_info_name(PragmaPredInfo, Name) },
		{ SymName = unqualified(Name) },
		{ pred_info_arity(PragmaPredInfo, Arity) },
		write_assertion_interface_error(Context,
				call(PredOrFunc, SymName, Arity),
				Module0, Module)
	;
		{ Module = Module0 }
	).
assertion__in_interface_check(conj(Goals) - _, PredInfo, Module0, Module) -->
	assertion__in_interface_check_list(Goals, PredInfo, Module0, Module).
assertion__in_interface_check(switch(_,_,_,_) - _, _, _, _) -->
	{ error("assertion__in_interface_check: assertion contains switch.") }.
assertion__in_interface_check(disj(Goals,_) - _, PredInfo, Module0, Module) -->
	assertion__in_interface_check_list(Goals, PredInfo, Module0, Module).
assertion__in_interface_check(not(Goal) - _, PredInfo, Module0, Module) -->
	assertion__in_interface_check(Goal, PredInfo, Module0, Module).
assertion__in_interface_check(some(_,_,Goal) - _, PredInfo, Module0, Module) -->
	assertion__in_interface_check(Goal, PredInfo, Module0, Module).
assertion__in_interface_check(if_then_else(_, If, Then, Else, _) - _,
		PredInfo, Module0, Module) -->
	assertion__in_interface_check(If, PredInfo, Module0, Module1),
	assertion__in_interface_check(Then, PredInfo, Module1, Module2),
	assertion__in_interface_check(Else, PredInfo, Module2, Module).
assertion__in_interface_check(par_conj(Goals,_) - _, PredInfo,
		Module0, Module) -->
	assertion__in_interface_check_list(Goals, PredInfo, Module0, Module).
assertion__in_interface_check(bi_implication(LHS, RHS) - _, PredInfo,
		Module0, Module) -->
	assertion__in_interface_check(LHS, PredInfo, Module0, Module1),
	assertion__in_interface_check(RHS, PredInfo, Module1, Module).

%-----------------------------------------------------------------------------%

:- pred assertion__in_interface_check_unify_rhs(unify_rhs::in, prog_var::in,
		prog_context::in, pred_info::in, module_info::in,
		module_info::out, io__state::di, io__state::uo) is det.

assertion__in_interface_check_unify_rhs(var(_), _, _, _, Module, Module) --> [].
assertion__in_interface_check_unify_rhs(functor(ConsId, _), Var, Context,
		PredInfo, Module0, Module) -->
	{ pred_info_clauses_info(PredInfo, ClausesInfo) },
	{ clauses_info_vartypes(ClausesInfo, VarTypes) },
	{ map__lookup(VarTypes, Var, Type) },
	(
		{ type_to_type_id(Type, TypeId, _) }
	->
		{ module_info_types(Module0, Types) },
		{ map__lookup(Types, TypeId, TypeDefn) },
		{ hlds_data__get_type_defn_status(TypeDefn, TypeStatus) },
		(
			{ is_defined_in_implementation_section(TypeStatus,
					yes) }
		->
			write_assertion_interface_error(Context,
					cons(ConsId), Module0, Module)
		;
			{ Module = Module0 }
		)
	;
		{ error("assertion__in_interface_check_unify_rhs: type_to_type_id failed.") }
	).
assertion__in_interface_check_unify_rhs(lambda_goal(_,_,_,_,_,_,_,Goal),
		_Var, _Context, PredInfo, Module0, Module) -->
	assertion__in_interface_check(Goal, PredInfo, Module0, Module).

%-----------------------------------------------------------------------------%

:- pred assertion__in_interface_check_list(hlds_goals::in, pred_info::in,
		module_info::in, module_info::out,
		io__state::di, io__state::uo)is det.

assertion__in_interface_check_list([], _, Module, Module) --> [].
assertion__in_interface_check_list([Goal0 | Goal0s], PredInfo,
		Module0, Module) -->
	assertion__in_interface_check(Goal0, PredInfo, Module0, Module1),
	assertion__in_interface_check_list(Goal0s, PredInfo, Module1, Module).

%-----------------------------------------------------------------------------%

	%
	% is_defined_in_implementation_section
	%
	% Returns yes if the import_status indicates the item came form
	% the implementation section.
	%
:- pred is_defined_in_implementation_section(import_status::in,
		bool::out) is det.

is_defined_in_implementation_section(abstract_exported, yes).
is_defined_in_implementation_section(exported_to_submodules, yes).
is_defined_in_implementation_section(local, yes).
is_defined_in_implementation_section(imported(implementation), yes).

is_defined_in_implementation_section(imported(interface), no).
is_defined_in_implementation_section(opt_imported, no).
is_defined_in_implementation_section(abstract_imported, no).
is_defined_in_implementation_section(pseudo_imported, no).
is_defined_in_implementation_section(exported, no).
is_defined_in_implementation_section(pseudo_exported, no).

%-----------------------------------------------------------------------------%

:- type call_or_consid
	--->	call(pred_or_func, sym_name, arity)
	;	cons(cons_id).

:- pred write_assertion_interface_error(prog_context::in,
		call_or_consid::in, module_info::in, module_info::out,
		io__state::di, io__state::uo) is det.

write_assertion_interface_error(Context, Type, Module0, Module) -->
	{ module_info_incr_errors(Module0, Module) },
	{ module_info_name(Module, ModuleName) },
	prog_out__write_context(Context),
	io__write_string("In interface for module `"),
	prog_out__write_sym_name(ModuleName),
	io__write_string("':\n"),

	prog_out__write_context(Context),
	io__write_string("  error: exported promise refers to "),
	(
		{ Type = call(PredOrFunc, SymName, Arity) },
		hlds_out__write_simple_call_id(PredOrFunc, SymName, Arity),
		io__nl
	;
		{ Type = cons(ConsId) },
		io__write_string("constructor `"),
		hlds_out__write_cons_id(ConsId),
		io__write_string("'\n")
	),

	prog_out__write_context(Context),
	io__write_string("  which is defined in the implementation "),
	io__write_string("of module `"),
	prog_out__write_sym_name(ModuleName),
	io__write_string("'.\n"),

	globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
	(
		{ VerboseErrors = yes }
	->
		prog_out__write_context(Context),
		io__write_string("  Either move the promise into the "),
		io__write_string("implementation section\n"),

		prog_out__write_context(Context),
		io__write_string("  or move the definition "),
		io__write_string("into the interface.\n")
	;
		[]
	).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
