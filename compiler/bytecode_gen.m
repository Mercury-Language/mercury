%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates the bytecode used by the debugger.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module bytecode_gen.

:- interface.

:- import_module hlds_module, bytecode.
:- import_module io.

:- pred bytecode_gen__module(module_info::in, list(byte_code)::out,
	io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, hlds_goal, hlds_data, prog_data, llds.
:- import_module passes_aux, call_gen, mode_util, code_util, goal_util, tree.

:- import_module bool, int, string, list, assoc_list, set, map, varset.
:- import_module std_util, require.

bytecode_gen__module(ModuleInfo, Code) -->
	{ module_info_predids(ModuleInfo, PredIds) },
	bytecode_gen__preds(PredIds, ModuleInfo, CodeTree),
	{ tree__flatten(CodeTree, CodeList) },
	{ list__condense(CodeList, Code) }.

:- pred bytecode_gen__preds(list(pred_id)::in, module_info::in,
	byte_tree::out, io__state::di, io__state::uo) is det.

bytecode_gen__preds([], _ModuleInfo, empty) --> [].
bytecode_gen__preds([PredId | PredIds], ModuleInfo, Code) -->
	{ module_info_preds(ModuleInfo, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		{ PredCode = empty }
	;
		bytecode_gen__pred(PredId, ProcIds, PredInfo, ModuleInfo,
			ProcsCode),
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ list__length(ProcIds, ProcsCount) },
		{ PredCode = tree(
			tree(
				node([enter_pred(PredName, ProcsCount)]),
				ProcsCode),
			node([endof_pred])) }
	),
	bytecode_gen__preds(PredIds, ModuleInfo, OtherCode),
	{ Code = tree(PredCode, OtherCode) }.

:- pred bytecode_gen__pred(pred_id::in, list(proc_id)::in, pred_info::in,
	module_info::in, byte_tree::out, io__state::di, io__state::uo) is det.

bytecode_gen__pred(_PredId, [], _PredInfo, _ModuleInfo, empty) --> [].
bytecode_gen__pred(PredId, [ProcId | ProcIds], PredInfo, ModuleInfo, Code) -->
	write_proc_progress_message("% Generating bytecode for ",
		PredId, ProcId, ModuleInfo),
	{ bytecode_gen__proc(ProcId, PredInfo, ModuleInfo, ProcCode) },
	bytecode_gen__pred(PredId, ProcIds, PredInfo, ModuleInfo, ProcsCode),
	{ Code = tree(ProcCode, ProcsCode) }.

:- pred bytecode_gen__proc(proc_id::in, pred_info::in,
	module_info::in, byte_tree::out) is det.

bytecode_gen__proc(ProcId, PredInfo, ModuleInfo, Code) :-
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),

	proc_info_goal(ProcInfo, Goal),
	proc_info_vartypes(ProcInfo, VarTypes),
	proc_info_variables(ProcInfo, VarSet),
	proc_info_interface_determinism(ProcInfo, Detism),

	goal_util__goal_vars(Goal, Vars),
	set__to_sorted_list(Vars, VarList),
	map__init(VarMap0),
	bytecode_gen__create_varmap(VarList, VarSet, VarTypes, 0,
		VarMap0, VarMap, VarInfos),

	bytecode_gen__init_byte_info(ModuleInfo, VarMap, VarTypes, ByteInfo),

	proc_info_headvars(ProcInfo, ArgVars),
	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, Args),

	call_gen__input_arg_locs(Args, InputArgs),
	bytecode_gen__gen_pickups(InputArgs, ByteInfo, PickupCode),

	call_gen__output_arg_locs(Args, OutputArgs),
	bytecode_gen__gen_places(OutputArgs, ByteInfo, PlaceCode),

	bytecode_gen__goal(Goal, ByteInfo, 1, N, GoalCode),

	BodyTree = tree(
			tree(
				PickupCode,
				node([label(0)])),
			tree(
				GoalCode,
				PlaceCode)),
	tree__flatten(BodyTree, BodyList),
	list__condense(BodyList, BodyCode0),
	( list__member(not_supported, BodyCode0) ->
		BodyCode = node([not_supported])
	;
		BodyCode = node(BodyCode0)
	),
	Code = tree(
			node([enter_proc(ProcId, Detism, N, VarInfos)]),
			tree(
				BodyCode,
				node([endof_proc]))).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__goal(hlds__goal::in, byte_info::in, int::in, int::out,
	byte_tree::out) is det.

bytecode_gen__goal(GoalExpr - GoalInfo, ByteInfo, N0, N, Code) :-
	bytecode_gen__goal_expr(GoalExpr, ByteInfo, N0, N, GoalCode),
	goal_info_context(GoalInfo, term__context(_, Line)),
	Code = tree(node([context(Line)]), GoalCode).

:- pred bytecode_gen__goal_expr(hlds__goal_expr::in, byte_info::in,
	int::in, int::out, byte_tree::out) is det.

bytecode_gen__goal_expr(GoalExpr, ByteInfo, N0, N, Code) :-
	(
		GoalExpr = higher_order_call(_, _, _, _, _, _),
		error("we do not handle higher order calls yet")
	;
		GoalExpr = call(PredId, ProcId, Args, IsBuiltin, _, _, _),
		( hlds__is_builtin_is_internal(IsBuiltin) ->
			bytecode_gen__builtin(PredId, ProcId, Args, ByteInfo,
				Code)
		;
			bytecode_gen__call(PredId, ProcId, Args, ByteInfo,
				Code)
		),
		N = N0
	;
		GoalExpr = unify(Var, RHS, _Mode, Unification, _),
		bytecode_gen__unify(Unification, Var, RHS, ByteInfo, Code),
		N = N0
	;
		GoalExpr = not(Goal),
		bytecode_gen__goal(Goal, ByteInfo, N0, N1, SomeCode),
		N is N1 + 1,
		Code = tree(
			tree(
				node([enter_negation(N1)]),
				SomeCode),
			node([endof_negation]))
	;
		GoalExpr = some(_, Goal),
		bytecode_gen__goal(Goal, ByteInfo, N0, N, SomeCode),
		Code = tree(
			tree(
				node([enter_commit]),
				SomeCode),
			node([endof_commit]))
	;
		GoalExpr = conj(GoalList),
		bytecode_gen__conj(GoalList, ByteInfo, N0, N, Code)
	;
		GoalExpr = disj(GoalList, _),
		bytecode_gen__disj(GoalList, ByteInfo, N0, N1, DisjCode),
		N is N1 + 1,
		Code = tree(
			tree(
				node([enter_disjunction(N1)]),
				DisjCode),
			node([endof_disjunction, label(N1)]))
	;
		GoalExpr = switch(Var, _, CasesList, _),
		bytecode_gen__switch(CasesList, Var, ByteInfo, N0, N1,
			SwitchCode),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		N is N1 + 1,
		Code = tree(
			tree(
				node([enter_switch(ByteVar, N1)]),
				SwitchCode),
			node([endof_switch, label(N1)]))
	;
		GoalExpr = if_then_else(_Vars, Cond, Then, Else, _),
		bytecode_gen__goal(Cond, ByteInfo, N0, N1, CondCode),
		bytecode_gen__goal(Then, ByteInfo, N1, N2, ThenCode),
		N3 is N2 + 1,
		bytecode_gen__goal(Else, ByteInfo, N3, N4, ElseCode),
		N is N4 + 1,
		Code = tree(
			tree(
				tree(
					node([enter_if(N2, N4)]),
					CondCode),
				tree(
					node([enter_then]),
					ThenCode)),
			tree(
				node([endof_then, label(N2)]),
				tree(
					ElseCode,
					node([endof_else, label(N4)]))))
	;
		GoalExpr = pragma_c_code(_, _, _, _, _, _),
		Code = node([not_supported]),
		N = N0
	).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__gen_places(list(pair(var, arg_loc))::in, byte_info::in,
	byte_tree::out) is det.

bytecode_gen__gen_places([], _, empty).
bytecode_gen__gen_places([Var - Loc | OutputArgs], ByteInfo, Code) :-
	bytecode_gen__gen_places(OutputArgs, ByteInfo, OtherCode),
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	Code = tree(node([place_arg(r(Loc), ByteVar)]), OtherCode).

:- pred bytecode_gen__gen_pickups(list(pair(var, arg_loc))::in, byte_info::in,
	byte_tree::out) is det.

bytecode_gen__gen_pickups([], _, empty).
bytecode_gen__gen_pickups([Var - Loc | OutputArgs], ByteInfo, Code) :-
	bytecode_gen__gen_pickups(OutputArgs, ByteInfo, OtherCode),
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	Code = tree(node([pickup_arg(r(Loc), ByteVar)]), OtherCode).

%---------------------------------------------------------------------------%

	% Generate bytecode for an ordinary call.

:- pred bytecode_gen__call(pred_id::in, proc_id::in, list(var)::in,
	byte_info::in, byte_tree::out) is det.

bytecode_gen__call(PredId, ProcId, ArgVars, ByteInfo, Code) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, Args),

	call_gen__input_arg_locs(Args, InputArgs),
	bytecode_gen__gen_places(InputArgs, ByteInfo, PlaceArgs),

	call_gen__output_arg_locs(Args, OutputArgs),
	bytecode_gen__gen_pickups(OutputArgs, ByteInfo, PickupArgs),

	predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),
	Call = node([call(ModuleName, PredName, Arity, ProcId)]),
	Code = tree(PlaceArgs, tree(Call, PickupArgs)).

	% Generate bytecode for a call to a builtin.

:- pred bytecode_gen__builtin(pred_id::in, proc_id::in, list(var)::in,
	byte_info::in, byte_tree::out) is det.

bytecode_gen__builtin(PredId, ProcId, Args, ByteInfo, Code) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	(
		code_util__translate_builtin(ModuleName, PredName, ProcId,
			Args, MaybeTest, MaybeAssign)
	->
		( MaybeTest = yes(Test) ->
			bytecode_gen__map_test(ByteInfo, Test, TestCode)
		;
			TestCode = empty
		),
		( MaybeAssign = yes(Var - Rval) ->
			bytecode_gen__map_assign(ByteInfo, Var, Rval,
				AssignCode)
		;
			AssignCode = empty
		),
		Code = tree(TestCode, AssignCode)
	;
		string__append("unknown builtin predicate ", PredName, Msg),
		error(Msg)
	).

:- pred bytecode_gen__map_test(byte_info::in, rval::in, byte_tree::out) is det.

bytecode_gen__map_test(ByteInfo, Rval, Code) :-
	( Rval = binop(Binop, X, Y) ->
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		bytecode_gen__map_arg(ByteInfo, Y, ByteY),
		Code = node([builtin_bintest(Binop, ByteX, ByteY)])
	; Rval = unop(Unop, X) ->
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		Code = node([builtin_untest(Unop, ByteX)])
	;
		error("builtin test is not a unary or binary operator")
	).

:- pred bytecode_gen__map_assign(byte_info::in, var::in, rval::in,
	byte_tree::out) is det.

bytecode_gen__map_assign(ByteInfo, Var, Rval, Code) :-
	( Rval = binop(Binop, X, Y) ->
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		bytecode_gen__map_arg(ByteInfo, Y, ByteY),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([builtin_binop(Binop, ByteX, ByteY, ByteVar)])
	; Rval = unop(Unop, X) ->
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([builtin_unop(Unop, ByteX, ByteVar)])
	;
		error("builtin assignment is not a unary or binary operator")
	).

:- pred bytecode_gen__map_arg(byte_info::in, rval::in, byte_arg::out) is det.

bytecode_gen__map_arg(ByteInfo, Rval, ByteArg) :-
	( Rval = var(Var) ->
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		ByteArg = var(ByteVar)
	; Rval = const(int_const(IntVal)) ->
		ByteArg = int_const(IntVal)
	; Rval = const(float_const(FloatVal)) ->
		ByteArg = float_const(FloatVal)
	;
		error("unknown kind of builtin argument")
	).

%---------------------------------------------------------------------------%

	% Generate bytecode for a unification.

:- pred bytecode_gen__unify(unification::in, var::in, unify_rhs::in,
	byte_info::in, byte_tree::out) is det.

bytecode_gen__unify(construct(Var, ConsId, Args, UniModes), _, _, ByteInfo,
		Code) :-
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	bytecode_gen__map_vars(ByteInfo, Args, ByteArgs),
	bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
	(
		bytecode_gen__map_uni_modes(UniModes, ByteInfo, Dirs),
		bytecode_gen__all_dirs_same(Dirs, to_var)
	->
		Code = node([construct(ByteVar, ByteConsId, ByteArgs)])
	;
		error("invalid mode for construction unification")
	).
bytecode_gen__unify(deconstruct(Var, ConsId, Args, UniModes, _), _, _, ByteInfo,
		Code) :-
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	bytecode_gen__map_vars(ByteInfo, Args, ByteArgs),
	bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
	bytecode_gen__map_uni_modes(UniModes, ByteInfo, Dirs),
	(
		bytecode_gen__all_dirs_same(Dirs, to_arg)
	->
		Code = node([deconstruct(ByteVar, ByteConsId, ByteArgs)])
	;
		assoc_list__from_corresponding_lists(ByteArgs, Dirs, Pairs),
		Code = node([complex_deconstruct(ByteVar, ByteConsId, Pairs)])
	).
bytecode_gen__unify(assign(Target, Source), _, _, ByteInfo, Code) :-
	bytecode_gen__map_var(ByteInfo, Target, ByteTarget),
	bytecode_gen__map_var(ByteInfo, Source, ByteSource),
	Code = node([assign(ByteTarget, ByteSource)]).
bytecode_gen__unify(simple_test(Var1, Var2), _, _, ByteInfo, Code) :-
	bytecode_gen__map_var(ByteInfo, Var1, ByteVar1),
	bytecode_gen__map_var(ByteInfo, Var2, ByteVar2),
	Code = node([test(ByteVar1, ByteVar2)]).
bytecode_gen__unify(complicated_unify(_, _, _), _Var, _RHS, _ByteInfo, _Code) :-
	error("we do not handle complicated unifications yet").

:- pred bytecode_gen__map_uni_modes(list(uni_mode)::in, byte_info::in,
	list(byte_dir)::out) is det.

bytecode_gen__map_uni_modes([], _, []).
bytecode_gen__map_uni_modes([UniMode | UniModes], ByteInfo, [Dir | Dirs]) :-
	UniMode = ((VarInitial - ArgInitial) -> (VarFinal - ArgFinal)),
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	( mode_is_input(ModuleInfo, (VarInitial -> VarFinal)) ->
		VarMode = top_in
	; mode_is_output(ModuleInfo, (VarInitial -> VarFinal)) ->
		VarMode = top_out
	;
		VarMode = top_unused
	),
	( mode_is_input(ModuleInfo, (ArgInitial -> ArgFinal)) ->
		ArgMode = top_in
	; mode_is_output(ModuleInfo, (ArgInitial -> ArgFinal)) ->
		ArgMode = top_out
	;
		ArgMode = top_unused
	),
	(
		VarMode = top_in,
		ArgMode = top_out
	->
		Dir = to_arg
	;
		VarMode = top_out,
		ArgMode = top_in
	->
		Dir = to_var
	;
		VarMode = top_out,
		ArgMode = top_in
	->
		Dir = to_none
	;
		error("invalid mode for deconstruct unification")
	),
	bytecode_gen__map_uni_modes(UniModes, ByteInfo, Dirs).

:- pred bytecode_gen__all_dirs_same(list(byte_dir)::in, byte_dir::in)
	is semidet.

bytecode_gen__all_dirs_same([], _).
bytecode_gen__all_dirs_same([Dir | Dirs], Dir) :-
	bytecode_gen__all_dirs_same(Dirs, Dir).

%---------------------------------------------------------------------------%

	% Generate bytecode for a conjunction

:- pred bytecode_gen__conj(list(hlds__goal)::in, byte_info::in,
	int::in, int::out, byte_tree::out) is det.

bytecode_gen__conj([], _, N, N, empty).
bytecode_gen__conj([Goal | Goals], ByteInfo, N0, N, Code) :-
	bytecode_gen__goal(Goal, ByteInfo, N0, N1, ThisCode),
	bytecode_gen__conj(Goals, ByteInfo, N1, N, OtherCode),
	Code = tree(ThisCode, OtherCode).

%---------------------------------------------------------------------------%

	% Generate bytecode for each disjunct of a disjunction.

:- pred bytecode_gen__disj(list(hlds__goal)::in, byte_info::in,
	int::in, int::out, byte_tree::out) is det.

bytecode_gen__disj([], _, N, N, empty).
bytecode_gen__disj([Disjunct | Disjuncts], ByteInfo, N0, N, Code) :-
	bytecode_gen__goal(Disjunct, ByteInfo, N0, N1, ThisCode),
	N2 is N1 + 1,
	bytecode_gen__disj(Disjuncts, ByteInfo, N2, N, OtherCode),
	Code = tree(
		tree(
			node([enter_disjunct(N1)]),
			ThisCode),
		tree(
			node([endof_disjunct, label(N1)]),
			OtherCode)).

%---------------------------------------------------------------------------%

	% Generate bytecode for each arm of a switch.

:- pred bytecode_gen__switch(list(case)::in, var::in, byte_info::in,
	int::in, int::out, byte_tree::out) is det.

bytecode_gen__switch([], _, _, N, N, empty).
bytecode_gen__switch([case(ConsId, Goal) | Cases], Var, ByteInfo, N0, N, Code)
		:-
	bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
	bytecode_gen__goal(Goal, ByteInfo, N0, N1, ThisCode),
	N2 is N1 + 1,
	bytecode_gen__switch(Cases, Var, ByteInfo, N2, N, OtherCode),
	Code = tree(
		tree(
			node([enter_switch_arm(ByteConsId, N1)]),
			ThisCode),
		tree(
			node([endof_switch_arm, label(N1)]),
			OtherCode)).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__map_cons_id(byte_info::in, var::in, cons_id::in,
	byte_cons_id::out) is det.

bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	(
		ConsId = cons(Functor, Arity),
		(
			Functor = unqualified(FunctorName)
		;
			% Should have been transformed into a function call
			% or pred_const.
			Functor = qualified(_, _),
			error("bytecode_gen__map_cons_id: qualified cons_id")
		),
		bytecode_gen__get_var_type(ByteInfo, Var, Type),
		code_util__cons_id_to_tag(ConsId, Type, ModuleInfo, ConsTag),
		bytecode_gen__map_cons_tag(ConsTag, ByteConsTag),
		ByteConsId = cons(FunctorName, Arity, ByteConsTag)
	;
		ConsId = int_const(IntVal),
		ByteConsId = int_const(IntVal)
	;
		ConsId = string_const(StringVal),
		ByteConsId = string_const(StringVal)
	;
		ConsId = float_const(FloatVal),
		ByteConsId = float_const(FloatVal)
	;
		ConsId = pred_const(PredId, ProcId),
		predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),
		ByteConsId = pred_const(ModuleName, PredName, Arity, ProcId)
	;
		ConsId = code_addr_const(PredId, ProcId),
		predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),
		ByteConsId = code_addr_const(ModuleName, PredName, Arity,
			ProcId)
	;
		ConsId = base_type_info_const(ModuleName, TypeName, TypeArity),
		ByteConsId = base_type_info_const(ModuleName, TypeName,
			TypeArity)
	).

:- pred bytecode_gen__map_cons_tag(cons_tag::in, byte_cons_tag::out) is det.

bytecode_gen__map_cons_tag(no_tag, no_tag).
bytecode_gen__map_cons_tag(simple_tag(Primary), simple_tag(Primary)).
bytecode_gen__map_cons_tag(complicated_tag(Primary, Secondary),
	complicated_tag(Primary, Secondary)).
bytecode_gen__map_cons_tag(complicated_constant_tag(Primary, Secondary),
	complicated_constant_tag(Primary, Secondary)).
bytecode_gen__map_cons_tag(string_constant(_), _) :-
	error("string_constant cons tag for non-string_constant cons id").
bytecode_gen__map_cons_tag(int_constant(_), _) :-
	error("int_constant cons tag for non-int_constant cons id").
bytecode_gen__map_cons_tag(float_constant(_), _) :-
	error("float_constant cons tag for non-float_constant cons id").
bytecode_gen__map_cons_tag(pred_closure_tag(_, _), _) :-
	error("pred_closure_tag cons tag for non-pred_const cons id").
bytecode_gen__map_cons_tag(code_addr_constant(_, _), _) :-
	error("code_addr_constant cons tag for non-address_const cons id").
bytecode_gen__map_cons_tag(base_type_info_constant(_, _, _), _) :-
	error("base_type_info_constant cons tag for non-base_type_info_constant cons id").

%---------------------------------------------------------------------------%

:- pred bytecode_gen__create_varmap(list(var)::in, varset::in,
	map(var, type)::in, int::in, map(var, byte_var)::in,
	map(var, byte_var)::out, list(byte_var_info)::out) is det.

bytecode_gen__create_varmap([], _, _, _, VarMap, VarMap, []).
bytecode_gen__create_varmap([Var | VarList], VarSet, VarTypes, N0,
		VarMap0, VarMap, VarInfos) :-
	map__det_insert(VarMap0, Var, N0, VarMap1),
	N1 is N0 + 1,
	varset__lookup_name(VarSet, Var, VarName),
	map__lookup(VarTypes, Var, VarType),
	bytecode_gen__create_varmap(VarList, VarSet, VarTypes, N1,
		VarMap1, VarMap, VarInfos1),
	VarInfos = [var_info(VarName, VarType) | VarInfos1].

%---------------------------------------------------------------------------%(

:- type byte_info	--->	byte_info(
					map(var, byte_var),
					map(var, type),
					module_info).

:- pred bytecode_gen__init_byte_info(module_info::in, map(var, byte_var)::in,
	map(var, type)::in, byte_info::out) is det.

bytecode_gen__init_byte_info(ModuleInfo, VarMap, VarTypes, ByteInfo) :-
	ByteInfo = byte_info(VarMap, VarTypes, ModuleInfo).

:- pred bytecode_gen__get_module_info(byte_info::in, module_info::out) is det.

bytecode_gen__get_module_info(byte_info(_, _, ModuleInfo), ModuleInfo).

:- pred bytecode_gen__map_vars(byte_info::in,
	list(var)::in, list(byte_var)::out) is det.

bytecode_gen__map_vars(byte_info(VarMap, _, _), Vars, ByteVars) :-
	bytecode_gen__map_vars_2(VarMap, Vars, ByteVars).

:- pred bytecode_gen__map_vars_2(map(var, byte_var)::in,
	list(var)::in, list(byte_var)::out) is det.

bytecode_gen__map_vars_2(_VarMap, [], []).
bytecode_gen__map_vars_2(VarMap, [Var | Vars], [ByteVar | ByteVars]) :-
	map__lookup(VarMap, Var, ByteVar),
	bytecode_gen__map_vars_2(VarMap, Vars, ByteVars).

:- pred bytecode_gen__map_var(byte_info::in, var::in, byte_var::out) is det.

bytecode_gen__map_var(byte_info(VarMap, _, _), Var, ByteVar) :-
	map__lookup(VarMap, Var, ByteVar).

:- pred bytecode_gen__get_var_type(byte_info::in, var::in, (type)::out) is det.

bytecode_gen__get_var_type(byte_info(_, VarTypes, _), Var, Type) :-
	map__lookup(VarTypes, Var, Type).

%---------------------------------------------------------------------------%
