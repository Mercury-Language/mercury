%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% unify_proc.m: 
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
% (this is done in call_gen.m).
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

:- pred unify_proc__generate_clause_info(special_pred_id, type,
			hlds__type_body, module_info, clauses_info).
:- mode unify_proc__generate_clause_info(in, in, in, in, out) is det.

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
		error("unify_proc.m: sorry, complicated unifications with partially instantiated modes not implemented")
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

	% XXX these belong in modes.m

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


unify_proc__generate_clause_info(SpecialPredId, Type, TypeBody, ModuleInfo,
				ClauseInfo) :-
	unify_proc_info__init(ModuleInfo, VarTypeInfo0),
	( TypeBody = eqv_type(EqvType) ->
		HeadVarType = EqvType
	;
		HeadVarType = Type
	),
	special_pred_info(SpecialPredId, HeadVarType,
			_PredName, ArgTypes, _Modes, _Det),
	unify_proc__make_fresh_vars(ArgTypes, Args, VarTypeInfo0, VarTypeInfo1),
	( SpecialPredId = unify, Args = [H1, H2] ->
		unify_proc__generate_unify_clauses(TypeBody, H1, H2,
					Clauses, VarTypeInfo1, VarTypeInfo)
	; SpecialPredId = index, Args = [X, Index] ->
		unify_proc__generate_index_clauses(TypeBody, X, Index,
					Clauses, VarTypeInfo1, VarTypeInfo)
	; SpecialPredId = compare, Args = [Res, X, Y] ->
		unify_proc__generate_compare_clauses(TypeBody, Res, X, Y,
					Clauses, VarTypeInfo1, VarTypeInfo)
	;
		error("unknown special pred")
	),
	unify_proc_info__extract(VarTypeInfo, VarSet, Types),
	ClauseInfo = clauses_info(VarSet, Types, Args, Clauses).

:- pred unify_proc__generate_unify_clauses(hlds__type_body, var, var,
				list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_unify_clauses(in, in, in, out, in, out) is det.

unify_proc__generate_unify_clauses(TypeBody, H1, H2, Clauses) -->
	( { TypeBody = du_type(Ctors, _, IsEnum), IsEnum = no } ->
		unify_proc__generate_du_unify_clauses(Ctors, H1, H2, Clauses)
	;
		{ term__context_init(Context) },
		{ create_atomic_unification(H1, var(H2), Context, explicit, [],
			Goal) },
		{ implicitly_quantify_clause_body([H1, H2], Goal, Body) },
		{ Clauses = [clause([], Body, Context)] }
	).

:- pred unify_proc__generate_index_clauses(hlds__type_body, var, var,
				list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_index_clauses(in, in, in, out, in, out) is det.

unify_proc__generate_index_clauses(TypeBody, X, Index, Clauses) -->
	( { TypeBody = du_type(Ctors, _, IsEnum), IsEnum = no } ->
		unify_proc__generate_du_index_clauses(Ctors, X, Index, 0,
			Clauses)
	;
		{ ArgVars = [X, Index] },
		unify_proc__build_call("index", ArgVars, Goal),
		{ implicitly_quantify_clause_body(ArgVars, Goal, Body) },
		{ term__context_init(Context) },
		{ Clauses = [clause([], Body, Context)] }
	).

:- pred unify_proc__generate_compare_clauses(hlds__type_body, var, var, var,
				list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_compare_clauses(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_compare_clauses(TypeBody, Res, H1, H2, Clauses) -->
	( { TypeBody = du_type(Ctors, _, IsEnum), IsEnum = no } ->
		unify_proc__generate_du_compare_clauses(Ctors, Res, H1, H2,
			Clauses)
	;
		{ ArgVars = [Res, H1, H2] },
		unify_proc__build_call("compare", ArgVars, Goal),
		{ implicitly_quantify_clause_body(ArgVars, Goal, Body) },
		{ term__context_init(Context) },
		{ Clauses = [clause([], Body, Context)] }
	).


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

:- pred unify_proc__generate_du_unify_clauses(list(constructor), var, var,
				list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_du_unify_clauses(in, in, in, out, in, out) is det.

unify_proc__generate_du_unify_clauses([], _H1, _H2, []) --> [].
unify_proc__generate_du_unify_clauses([Ctor | Ctors], H1, H2,
		[Clause | Clauses]) -->
	{ Ctor = FunctorName - ArgTypes },
	{ unqualify_name(FunctorName, UnqualifiedFunctorName) },
	{ Functor = term__atom(UnqualifiedFunctorName) },
	{ term__context_init(Context) },
	unify_proc__make_fresh_vars(ArgTypes, Vars1),
	unify_proc__make_fresh_vars(ArgTypes, Vars2),
	{ create_atomic_unification(
		H1, functor(Functor, Vars1), Context, explicit, [], 
		UnifyH1_Goal) },
	{ create_atomic_unification(
		H2, functor(Functor, Vars2), Context, explicit, [], 
		UnifyH2_Goal) },
	{ unify_proc__unify_var_lists(Vars1, Vars2, UnifyArgs_Goal) },
	{ GoalList = [UnifyH1_Goal, UnifyH2_Goal | UnifyArgs_Goal] },
	{ goal_info_init(GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) },
	{ implicitly_quantify_clause_body([H1, H2], Goal, Body) },
	{ Clause = clause([], Body, Context) },
	unify_proc__generate_du_unify_clauses(Ctors, H1, H2, Clauses).

%-----------------------------------------------------------------------------%

/*
	For a type such as 

		:- type foo ---> f ; g(a, b, c) ; h(foo).
	
	we want to generate code

		index(X, Index) :-
			(
				X = f,
				Index = 0
			;
				X = g(_, _, _),
				Index = 1
			;
				X = h(_),
				Index = 2
			).
*/

:- pred unify_proc__generate_du_index_clauses(list(constructor), var, var, int,
				list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_du_index_clauses(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_du_index_clauses([], _X, _Index, _N, []) --> [].
unify_proc__generate_du_index_clauses([Ctor | Ctors], X, Index, N,
		[Clause | Clauses]) -->
	{ Ctor = FunctorName - ArgTypes },
	{ unqualify_name(FunctorName, UnqualifiedFunctorName) },
	{ Functor = term__atom(UnqualifiedFunctorName) },
	{ term__context_init(Context) },
	unify_proc__make_fresh_vars(ArgTypes, ArgVars),
	{ create_atomic_unification(
		X, functor(Functor, ArgVars), Context, explicit, [], 
		UnifyX_Goal) },
	{ create_atomic_unification(
		Index, functor(term__integer(N), []), Context, explicit, [], 
		UnifyIndex_Goal) },
	{ GoalList = [UnifyX_Goal, UnifyIndex_Goal] },
	{ goal_info_init(GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Goal) },
	{ implicitly_quantify_clause_body([X, Index], Goal, Body) },
	{ Clause = clause([], Body, Context) },
	{ N1 is N + 1 },
	unify_proc__generate_du_index_clauses(Ctors, X, Index, N1, Clauses).

%-----------------------------------------------------------------------------%

/*	For a type such as 

		:- type foo ---> f ; g(a) ; h(b, foo).

   	we want to generate code

		compare(Res, X, Y) :-
			index(X, X_Index),	% Call_X_Index
			index(Y, Y_Index),	% Call_Y_Index
			( X_Index < Y_Index ->	% Call_Less_Than
				Res = (<)	% Return_Less_Than
			; X_Index > Y_Index ->	% Call_Greater_Than
				Res = (>)	% Return_Greater_Than
			;
				% This disjunction is generated by
				% unify_proc__generate_compare_cases, below.
				(
					X = f, Y = f,
					R = (=)
				;
					X = g(X1), Y = g(Y1),
					compare(R, X1, Y1)
				;
					X = h(X1, X2), Y = h(Y1, Y2),
					( compare(R1, X1, Y1), R1 \= (=) ->
						R = R1
					; 
						compare(R, X2, Y2)
					)
				)
			->
				Res = R		% Return_R
			;
				compare_error 	% Abort
			).
*/

:- pred unify_proc__generate_du_compare_clauses(
			list(constructor), var, var, var,
			list(clause), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_du_compare_clauses(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_du_compare_clauses(Ctors, Res, X, Y, [Clause]) -->
	( { Ctors = [SingleCtor] } ->
		unify_proc__generate_compare_case(SingleCtor, Res, X, Y, Goal)
	;
		unify_proc__generate_du_compare_clauses_2(Ctors, Res, X, Y,
			Goal)
	),
	{ ArgVars = [Res, X, Y] },
	{ implicitly_quantify_clause_body(ArgVars, Goal, Body) },
	{ term__context_init(Context) },
	{ Clause = clause([], Body, Context) }.

:- pred unify_proc__generate_du_compare_clauses_2(
			list(constructor), var, var, var,
			hlds__goal, unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_du_compare_clauses_2(in, in, in, in, out, in, out)
	is det.

unify_proc__generate_du_compare_clauses_2(Ctors, Res, X, Y, Goal) -->
	{ term__context_init(Context) },
	{ IntType = term__functor(term__atom("int"), [], Context) },
	{ ResType = term__functor(term__atom("comparison_result"), [],
			Context) },
	unify_proc_info__new_var(IntType, X_Index),
	unify_proc_info__new_var(IntType, Y_Index),
	unify_proc_info__new_var(ResType, R),

	{ goal_info_init(GoalInfo) },

	unify_proc__build_call("index", [X, X_Index], Call_X_Index),

	unify_proc__build_call("index", [Y, Y_Index], Call_Y_Index),

	unify_proc__build_call("<", [X_Index, Y_Index], Call_Less_Than),

	unify_proc__build_call(">", [X_Index, Y_Index], Call_Greater_Than),

	{ create_atomic_unification(
		Res, functor(term__atom("<"), []), Context, explicit, [], 
		Return_Less_Than) },

	{ create_atomic_unification(
		Res, functor(term__atom(">"), []), Context, explicit, [], 
		Return_Greater_Than) },

	{ create_atomic_unification(Res, var(R), Context, explicit, [],
		Return_R) },

	unify_proc__generate_compare_cases(Ctors, R, X, Y, Cases),
	{ CasesGoal = disj(Cases) - GoalInfo },

	unify_proc__build_call("compare_error", [], Abort),

	{ Goal = conj([
		Call_X_Index,
		Call_Y_Index, 
		if_then_else([], Call_Less_Than, Return_Less_Than,
		    if_then_else([], Call_Greater_Than, Return_Greater_Than,
		        if_then_else([], CasesGoal, Return_R,
		            Abort
		        ) - GoalInfo
		    ) - GoalInfo
		) - GoalInfo
	]) - GoalInfo }.

/*	
	unify_proc__generate_compare_cases: for a type such as 

		:- type foo ---> f ; g(a) ; h(b, foo).

   	we want to generate code
		(
			X = f,		% UnifyX_Goal
			Y = f,		% UnifyY_Goal
			R = (=)		% CompareArgs_Goal
		;
			X = g(X1),	
			Y = g(Y1),
			compare(R, X1, Y1)
		;
			X = h(X1, X2),
			Y = h(Y1, Y2),
			( compare(R1, X1, Y1), R1 \= (=) ->
				R = R1
			; 
				compare(R, X2, Y2)
			)
		)
*/

:- pred unify_proc__generate_compare_cases(list(constructor), var, var, var,
			list(hlds__goal), unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_compare_cases(in, in, in, in, out, in, out) is det.

unify_proc__generate_compare_cases([], _R, _X, _Y, []) --> [].
unify_proc__generate_compare_cases([Ctor | Ctors], R, X, Y, [Case | Cases]) -->
	unify_proc__generate_compare_case(Ctor, R, X, Y, Case),
	unify_proc__generate_compare_cases(Ctors, R, X, Y, Cases).

:- pred unify_proc__generate_compare_case(constructor, var, var, var,
			hlds__goal, unify_proc_info, unify_proc_info).
:- mode unify_proc__generate_compare_case(in, in, in, in, out, in, out) is det.

unify_proc__generate_compare_case(Ctor, R, X, Y, Case) -->
	{ Ctor = FunctorName - ArgTypes },
	{ unqualify_name(FunctorName, UnqualifiedFunctorName) },
	{ Functor = term__atom(UnqualifiedFunctorName) },
	{ term__context_init(Context) },
	unify_proc__make_fresh_vars(ArgTypes, Vars1),
	unify_proc__make_fresh_vars(ArgTypes, Vars2),
	{ create_atomic_unification(
		X, functor(Functor, Vars1), Context, explicit, [], 
		UnifyX_Goal) },
	{ create_atomic_unification(
		Y, functor(Functor, Vars2), Context, explicit, [], 
		UnifyY_Goal) },
	unify_proc__compare_args(Vars1, Vars2, R, CompareArgs_Goal),
	{ GoalList = [UnifyX_Goal, UnifyY_Goal, CompareArgs_Goal] },
	{ goal_info_init(GoalInfo) },
	{ conj_list_to_goal(GoalList, GoalInfo, Case) }.

/*	unify_proc__compare_args: for a constructor such as

		h(list(int), foo, string)
	
	we want to generate code

		(
			compare(R1, X1, Y1),	% Do_Comparison
			R1 \= (=)		% Check_Not_Equal
		->
			R = R1			% Return_R1
		;
			compare(R2, X2, Y2),
			R2 \= (=)
		->
			R = R2
		; 
			compare(R, X3, Y3)	% Return_Comparison
		)

	For a constructor with no arguments, we want to generate code

		R = (=)		% Return_Equal

*/

:- pred unify_proc__compare_args(list(var), list(var), var, hlds__goal,
				unify_proc_info, unify_proc_info).
:- mode unify_proc__compare_args(in, in, in, out, in, out) is det.

unify_proc__compare_args([], [], R, Return_Equal) -->
	{ term__context_init(Context) },
	{ create_atomic_unification(
		R, functor(term__atom("="), []), Context, explicit, [], 
		Return_Equal) }.
unify_proc__compare_args([X|Xs], [Y|Ys], R, Goal) -->
	{ goal_info_init(GoalInfo) },
	( { Xs = [], Ys = [] } ->
		unify_proc__build_call("compare", [R, X, Y], Goal)
	;
		{ term__context_init(Context) },
		{ ResType = term__functor(term__atom("comparison_result"), [],
				Context) },
		unify_proc_info__new_var(ResType, R1),

		unify_proc__build_call("compare", [R1, X, Y], Do_Comparison),

		{ create_atomic_unification(
			R1, functor(term__atom("="), []), Context, explicit, [],
			Check_Equal) },
		{ Check_Not_Equal = not(Check_Equal) - GoalInfo },

		{ create_atomic_unification(
			R, var(R1), Context, explicit, [], Return_R1) },
		{ Condition = conj([Do_Comparison, Check_Not_Equal])
					- GoalInfo },
		{ Goal = if_then_else([], Condition, Return_R1, ElseCase)
					- GoalInfo},
		unify_proc__compare_args(Xs, Ys, R, ElseCase)
	).
unify_proc__compare_args([], [_|_], _, _) -->
	{ error("unify_proc__compare_args: length mismatch") }.
unify_proc__compare_args([_|_], [], _, _) -->
	{ error("unify_proc__compare_args: length mismatch") }.

%-----------------------------------------------------------------------------%

:- pred unify_proc__build_call(string, list(var), hlds__goal,
				unify_proc_info, unify_proc_info).
:- mode unify_proc__build_call(in, in, out, in, out) is det.

unify_proc__build_call(Name, ArgVars, Goal) -->
	unify_proc_info__get_module_info(ModuleInfo),
	{ module_info_get_predicate_table(ModuleInfo, PredicateTable) },
	{ list__length(ArgVars, Arity) },
	{
		predicate_table_search_name_arity(PredicateTable,
			Name, Arity, [PredId])
	->
		IndexPredId = PredId
	;
		error("unify_proc__build_call: invalid/ambiguous pred")
	},
	{ ModeId = 0 },
	{ map__init(Follow) },
	{ is_builtin__make_builtin(no, no, Builtin) },
	{ Call = call(IndexPredId, ModeId, ArgVars, Builtin,
			unqualified(Name), Follow) },
	{ goal_info_init(GoalInfo) },
	{ Goal = Call - GoalInfo }.

:- pred unify_proc__make_fresh_vars(list(type), list(var),
					unify_proc_info, unify_proc_info).
:- mode unify_proc__make_fresh_vars(in, out, in, out) is det.

unify_proc__make_fresh_vars([], []) --> [].
unify_proc__make_fresh_vars([Type | Types], [Var | Vars]) -->
	unify_proc_info__new_var(Type, Var),
	unify_proc__make_fresh_vars(Types, Vars).

:- pred unify_proc__unify_var_lists(list(var), list(var), list(hlds__goal)).
:- mode unify_proc__unify_var_lists(in, in, out) is det.

unify_proc__unify_var_lists([], [_|_], _) :-
	error("unify_proc__unify_var_lists: length mismatch").
unify_proc__unify_var_lists([_|_], [], _) :-
	error("unify_proc__unify_var_lists: length mismatch").
unify_proc__unify_var_lists([], [], []).
unify_proc__unify_var_lists([Var1 | Vars1], [Var2 | Vars2], [Goal | Goals]) :-
	term__context_init(Context),
	create_atomic_unification(Var1, var(Var2), Context, explicit, [],
		Goal),
	unify_proc__unify_var_lists(Vars1, Vars2, Goals).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% It's a pity that we don't have nested modules.

% :- begin_module unify_proc_info.
% :- interface.

:- type unify_proc_info.

:- pred unify_proc_info__init(module_info, unify_proc_info).
:- mode unify_proc_info__init(in, out) is det.

:- pred unify_proc_info__new_var(type, var, unify_proc_info, unify_proc_info).
:- mode unify_proc_info__new_var(in, out, in, out) is det.

:- pred unify_proc_info__extract(unify_proc_info, varset, map(var, type)).
:- mode unify_proc_info__extract(in, out, out) is det.

:- pred unify_proc_info__get_module_info(module_info,
					unify_proc_info, unify_proc_info).
:- mode unify_proc_info__get_module_info(out, in, out) is det.

%-----------------------------------------------------------------------------%

% :- implementation

:- type unify_proc_info
	--->	unify_proc_info(
			varset,
			map(var, type),
			module_info
		).

unify_proc_info__init(ModuleInfo, VarTypeInfo) :-
	varset__init(VarSet),
	map__init(Types),
	VarTypeInfo = unify_proc_info(VarSet, Types, ModuleInfo).

unify_proc_info__new_var(Type, Var,
		unify_proc_info(VarSet0, Types0, ModuleInfo),
		unify_proc_info(VarSet, Types, ModuleInfo)) :-
	varset__new_var(VarSet0, Var, VarSet),
	map__set(Types0, Var, Type, Types).

unify_proc_info__extract(unify_proc_info(VarSet, Types, _ModuleInfo),
		VarSet, Types).

unify_proc_info__get_module_info(ModuleInfo, VarTypeInfo, VarTypeInfo) :-
	VarTypeInfo = unify_proc_info(_VarSet, _Types, ModuleInfo).

% :- end_module unify_proc_info.

%-----------------------------------------------------------------------------%
