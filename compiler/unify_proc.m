%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% unify_proc.nl: 
%
%	This module encapsulates access to the unify_requests table,
%	and constructs the clauses for out-of-line complicated
%	unification procedures.
%
% During mode analysis, we notice each different complicated unification and 
% record in the `unify_requests' table that we need to eventually generate
% code for that out-of-line unification procedure.
%
% After we've done mode analysis for all the ordinary predicates, we then
% do mode analysis for the out-of-line unification procedures.  Note that
% unification procedures may call other unification procedures which have
% not yet been enountered, causing new entries to be added to the
% unify_requests table.  We store the entries in a queue and continue the
% process until the queue is empty.
%
% Each time we come to generate code for a complicated unification, the
% compiler just generates a call to the out-of-line unification procedure
% (this is done in call_gen.nl).
%
% Currently if the same complicated unification procedure is called by
% different modules, each module will end up with a copy of the code for
% that procedure.  In the long run it would be desireable to either delay
% generation of complicated unification procedures until link time (like
% Cfront does with C++ templates) or to have a smart linker which could
% merge duplicate definitions (like Borland C++).  However the amount of
% code duplication involved is probably very small, so it's definitely not
% worth worrying about right now.

%-----------------------------------------------------------------------------%

:- module unify_proc.
:- interface.
:- import_module std_util, io, list.
:- import_module prog_io, hlds, llds.

:- type unify_requests.

:- type unify_proc_id == pair(type_id, uni_mode).

:- type unify_proc_num == int.

	% Initialize the unify_requests table.

:- pred unify_proc__init_requests(unify_requests).
:- mode unify_proc__init_requests(out) is det.

	% Add a new request to the unify_requests table.

:- pred unify_proc__request_unify(unify_proc_id, unify_requests,
				unify_requests).
:- mode unify_proc__request_unify(in, in, out) is det.

	% Generate code for the unification procedures which have been
	% requested.

:- pred modecheck_unify_procs(module_info, module_info, io__state, io__state).
:- mode modecheck_unify_procs(in, out, di, uo) is det.

:- pred unify_proc__lookup_num(unify_requests, type_id, uni_mode,
				unify_proc_num).
:- mode unify_proc__lookup_num(in, in, in, out) is det.

:- pred unify_proc__generate_clause_info(type, hlds__type_body, clauses_info).
:- mode unify_proc__generate_clause_info(in, in, out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module tree, map, queue, int, require.
:- import_module code_util, code_info, type_util, varset.
:- import_module mercury_to_mercury, hlds_out.
:- import_module make_hlds, term, prog_util.
:- import_module quantification.
:- import_module globals, options.

	% We keep track of all the complicated unification procs we need
	% by storing them in the unify_requests structure.
	% We assign a unique unify_proc_num to each one.

:- type req_map == map(unify_proc_id, unify_proc_num).

:- type req_queue == queue(unify_proc_id).

:- type unify_requests --->
		unify_requests(
			unify_proc_num,		% next unused number
			req_map,		% the assignment of numbers
						% to unify_proc_ids
			req_queue		% queue of procs we still need
						% to generate code for
		).

%-----------------------------------------------------------------------------%

unify_proc__init_requests(Requests) :-
	map__init(ReqMap),
	queue__init(ReqQueue),
	Requests = unify_requests(1, ReqMap, ReqQueue).

%-----------------------------------------------------------------------------%

	% Boring access predicates

:- pred unify_proc__get_num(unify_requests, unify_proc_num).
:- mode unify_proc__get_num(in, out) is det.

:- pred unify_proc__get_req_map(unify_requests, req_map).
:- mode unify_proc__get_req_map(in, out) is det.

:- pred unify_proc__get_req_queue(unify_requests, req_queue).
:- mode unify_proc__get_req_queue(in, out) is det.

:- pred unify_proc__set_num(unify_requests, unify_proc_num, unify_requests).
:- mode unify_proc__set_num(in, in, out) is det.

:- pred unify_proc__set_req_map(unify_requests, req_map, unify_requests).
:- mode unify_proc__set_req_map(in, in, out) is det.

:- pred unify_proc__set_req_queue(unify_requests, req_queue, unify_requests).
:- mode unify_proc__set_req_queue(in, in, out) is det.

unify_proc__get_num(unify_requests(Num, _, _), Num).

unify_proc__get_req_map(unify_requests(_, ReqMap, _), ReqMap).

unify_proc__get_req_queue(unify_requests(_, _, ReqQueue), ReqQueue).

unify_proc__set_num(unify_requests(_, B, C), Num,
			unify_requests(Num, B, C)).

unify_proc__set_req_map(unify_requests(A, _, C), ReqMap,
			unify_requests(A, ReqMap, C)).

unify_proc__set_req_queue(unify_requests(A, B, _), ReqQueue,
			unify_requests(A, B, ReqQueue)).

%-----------------------------------------------------------------------------%

unify_proc__lookup_num(Requests, TypeId, UniMode, Num) :-
	unify_proc__get_req_map(Requests, ReqMap),
	( map__search(ReqMap, TypeId - UniMode, Num0) ->
		Num = Num0
	;
		error("unify_proc.nl: sorry, complicated unifications with partially instantiated modes not implemented")
	).

%-----------------------------------------------------------------------------%

unify_proc__request_unify(UnifyId, Requests0, Requests) :-
	unify_proc__get_req_map(Requests0, ReqMap0),
	( map__contains(ReqMap0, UnifyId) ->
		Requests = Requests0
	;
		unify_proc__get_num(Requests0, Num0),
		map__set(ReqMap0, UnifyId, Num0, ReqMap),
		unify_proc__set_req_map(Requests0, ReqMap, Requests1),

		unify_proc__get_num(Requests1, Num1),
		Num is Num1 + 1,
		unify_proc__set_num(Requests1, Num, Requests2),

		unify_proc__get_req_queue(Requests1, ReqQueue1),
		queue__put(ReqQueue1, UnifyId, ReqQueue),
		unify_proc__set_req_queue(Requests2, ReqQueue, Requests)
	).

%-----------------------------------------------------------------------------%

	% XXX these belong in modes.nl

modecheck_unify_procs(ModuleInfo0, ModuleInfo) -->
	{ module_info_get_unify_requests(ModuleInfo0, Requests0) },
	{ unify_proc__get_req_queue(Requests0, RequestQueue0) },
	(
		{ queue__get(RequestQueue0, UnifyProcId, RequestQueue1) }
	->
		{ unify_proc__set_req_queue(Requests0, RequestQueue1,
			Requests1) },
		{ module_info_set_unify_requests(ModuleInfo0, Requests1,
			ModuleInfo1) },
		globals__io_lookup_bool_option(very_verbose, VeryVerbose),
		( { VeryVerbose = yes } ->
			io__write_string("% Generating "),
			unify_proc__write_unify_proc_id(UnifyProcId),
			io__write_string("\n")
		;
			[]
		),
		{ modecheck_generate_unification(UnifyProcId, ModuleInfo1,
			ModuleInfo2) },
		modecheck_unify_procs(ModuleInfo2, ModuleInfo)
	;
		{ ModuleInfo = ModuleInfo0 }
	).

:- pred modecheck_generate_unification(unify_proc_id, module_info, module_info).
:- mode modecheck_generate_unification(in, in, out) is det.

modecheck_generate_unification(_UnifyProcId, ModuleInfo, ModuleInfo).
	% XXX stub only!!!
	% currently we don't handle complicated unifications in
	% partially instantiated modes
/*
modecheck_generate_unification(UnifyProcId, ModuleInfo0, ModuleInfo) :-
	module_info_get_unify_requests(ModuleInfo0, Requests0),
	unify_proc__get_req_map(Requests0, ReqMap),
	map__lookup(ReqMap, UnifyProcId, UnifyModeNum),
	UnifyProcId = TypeId - UnifyMode,
*/

%-----------------------------------------------------------------------------%

:- pred unify_proc__write_unify_proc_id(unify_proc_id, io__state, io__state).
:- mode unify_proc__write_unify_proc_id(in, di, uo) is det.

unify_proc__write_unify_proc_id(TypeId - UniMode) -->
	io__write_string("unification procedure for type `"),
	hlds_out__write_type_id(TypeId),
	io__write_string("' with initial insts `"),
	{ UniMode = ((InstA - InstB) -> _FinalInst) },
	{ varset__init(InstVarSet) },
	mercury_output_inst(InstA, InstVarSet),
	io__write_string("', `"),
	mercury_output_inst(InstB, InstVarSet),
	io__write_string("'").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

/*
	For a type such as

		type t(X) ---> a ; b(int) ; c(X); d(int, X, t)
	
	we want to generate code

		eq(H1, H2) :-
			(
				H1 = a,
				H2 = a
			;
				H1 = b(X1),
				H2 = b(X2),
				X1 = X2,
			;
				H1 = c(Y1),
				H2 = c(Y2),
				Y1 = Y2,
			;
				H1 = d(A1, B1, C1),
				H2 = c(A2, B2, C2),
				A1 = A2,
				B1 = B2,
				C1 = C2
			).
*/


:- unify_proc__generate_clause_info(_, X, _) when X. % NU-Prolog indexing.

unify_proc__generate_clause_info(Type, TypeBody, ClauseInfo) :-
	var_type_info__init(VarTypeInfo0),
	( TypeBody = eqv_type(EqvType) ->
		HeadVarType = EqvType
	;
		HeadVarType = Type
	),
	unify_proc__generate_head_vars(HeadVarType, H1, H2,
					VarTypeInfo0, VarTypeInfo1),
	unify_proc__generate_clauses(TypeBody, H1, H2, Clauses,
					VarTypeInfo1, VarTypeInfo),
	var_type_info__extract(VarTypeInfo, VarSet, Types),
	ClauseInfo = clauses_info(VarSet, Types, [H1, H2], Clauses).

:- pred unify_proc__generate_head_vars(type, var, var,
					var_type_info, var_type_info).
:- mode unify_proc__generate_head_vars(in, out, out, in, out) is det.

unify_proc__generate_head_vars(Type, H1, H2) -->
	var_type_info__new_var(Type, H1),
	var_type_info__new_var(Type, H2).

:- pred unify_proc__generate_clauses(hlds__type_body, var, var, list(clause),
					var_type_info, var_type_info).
:- mode unify_proc__generate_clauses(in, in, in, out, in, out) is det.

unify_proc__generate_clauses(TypeBody, H1, H2, Clauses) -->
	( { TypeBody = du_type(Ctors, _, IsEnum), IsEnum = no } ->
		unify_proc__generate_du_clauses(Ctors, H1, H2, Clauses)
	;
		{ create_atomic_unification(term__variable(H1),
			term__variable(H2), explicit, [], Goal) },
		{ implicitly_quantify_clause_body([H1, H2], Goal, Body) },
		{ term__context_init(Context) },
		{ Clauses = [clause([], Body, Context)] }
	).

:- pred unify_proc__generate_du_clauses(list(constructor), var, var,
				list(clause), var_type_info, var_type_info).
:- mode unify_proc__generate_du_clauses(in, in, in, out, in, out) is det.

unify_proc__generate_du_clauses([], _H1, _H2, []) --> [].
unify_proc__generate_du_clauses([Ctor | Ctors], H1, H2, [Clause | Clauses]) -->
	{ Ctor = FunctorName - ArgTypes },
	{ unqualify_name(FunctorName, UnqualifiedFunctorName) },
	{ Functor = term__atom(UnqualifiedFunctorName) },
	{ term__context_init(Context) },
	unify_proc__make_fresh_vars(ArgTypes, Vars1),
	unify_proc__make_fresh_vars(ArgTypes, Vars2),
	{ term__var_list_to_term_list(Vars1, VarTerms1) },
	{ term__var_list_to_term_list(Vars2, VarTerms2) },
	{ create_atomic_unification(
		term__variable(H1), term__functor(Functor, VarTerms1, Context), 
		explicit, [], 
		UnifyH1_Goal) },
	{ create_atomic_unification(
		term__variable(H2), term__functor(Functor, VarTerms2, Context),
		explicit, [], 
		UnifyH2_Goal) },
	{ unify_proc__unify_var_lists(Vars1, Vars2, UnifyArgs_Goal) },
	{ GoalList = [UnifyH1_Goal, UnifyH2_Goal | UnifyArgs_Goal] },
	{ goal_info_init(GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) },
	{ implicitly_quantify_clause_body([H1, H2], Goal, Body) },
	{ Clause = clause([], Body, Context) },
	unify_proc__generate_du_clauses(Ctors, H1, H2, Clauses).

:- pred unify_proc__make_fresh_vars(list(type), list(var),
					var_type_info, var_type_info).
:- mode unify_proc__make_fresh_vars(in, out, in, out) is det.

unify_proc__make_fresh_vars([], []) --> [].
unify_proc__make_fresh_vars([Type | Types], [Var | Vars]) -->
	var_type_info__new_var(Type, Var),
	unify_proc__make_fresh_vars(Types, Vars).

:- pred unify_proc__unify_var_lists(list(var), list(var), list(hlds__goal)).
:- mode unify_proc__unify_var_lists(in, in, out) is det.

unify_proc__unify_var_lists([], [_|_], _) :-
	error("unify_proc__unify_var_lists: length mismatch").
unify_proc__unify_var_lists([_|_], [], _) :-
	error("unify_proc__unify_var_lists: length mismatch").
unify_proc__unify_var_lists([], [], []).
unify_proc__unify_var_lists([Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) :-
	create_atomic_unification(
		term__variable(Var1), term__variable(Var2),
		explicit, [], 
		Goal),
	unify_proc__unify_var_lists(Vars1, Vars2, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% It's a pity that we don't have nested modules.

% :- begin_module var_type_info.
% :- interface.

:- type var_type_info.

:- pred var_type_info__init(var_type_info).
:- mode var_type_info__init(out) is det.

:- pred var_type_info__new_var(type, var, var_type_info, var_type_info).
:- mode var_type_info__new_var(in, out, in, out) is det.

:- pred var_type_info__extract(var_type_info, varset, map(var, type)).
:- mode var_type_info__extract(in, out, out) is det.

%-----------------------------------------------------------------------------%

% :- implementation

:- type var_type_info == pair(varset, map(var, type)).

var_type_info__init(VarSet - Types) :-
	varset__init(VarSet),
	map__init(Types).

var_type_info__new_var(Type, Var, VarSet0 - Types0, VarSet - Types) :-
	varset__new_var(VarSet0, Var, VarSet),
	map__set(Types0, Var, Type, Types).

var_type_info__extract(VarSet - Types, VarSet, Types).

% :- end_module var_type_info.

%-----------------------------------------------------------------------------%
