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

:- import_module hlds_pred, hlds_goal, hlds_data, llds.
:- import_module call_gen, code_util, goal_util, tree.

:- import_module bool, int, list, assoc_list, set, map, std_util, require.

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
		{ Code = empty }
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

bytecode_gen__pred(_PredId, [], _Predinfo, _ModuleInfo, empty) --> [].
bytecode_gen__pred(PredId, [ProcId | ProcIds], Predinfo, ModuleInfo, Code) -->
	bytecode_gen__proc(PredId, ProcId, Predinfo, ModuleInfo, ProcCode),
	bytecode_gen__pred(PredId, ProcIds, Predinfo, ModuleInfo, ProcsCode),
	Code = tree(ProcCode, ProcsCode).

:- pred bytecode_gen__proc(pred_id::in, proc_id::in, pred_info::in,
	module_info::in, byte_tree::out, io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- pred bytecode_gen__goal(hlds__goal::in, byte_info::in, int::in, int::out,
	byte_tree::out) is det.

bytecode_gen__goal(GoalExpr - _GoalInfo, ByteInfo, N0, N, Code) :-
	bytecode_gen__goal_expr(GoalExpr, ByteInfo, N0, N, Code).

:- pred bytecode_gen__goal_expr(hlds__goal_expr::in, byte_info::in,
	int::in, int::out, byte_tree::out) is det.

bytecode_gen__goal_expr(GoalExpr, ByteInfo, N0, N, Code) :-
	(
		GoalExpr = call(PredId, ProcId, Args, IsBuiltin, _, _, _),
		( IsBuiltin = yes ->
			bytecode_gen__builtin(PredId, ProcId, Args, ByteInfo,
				Code)
		;
			bytecode_gen__call(PredId, ProcId, Args, ByteInfo,
				Code)
		),
		N = N0
	;
		GoalExpr = unify(Var, RHS, _Mode, Unification, _),
		bytecode_gen__unify(Unification, Var, RHS, ByteInfo, Code)
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
	).

	% Generate bytecode for an ordinary call.

:- pred bytecode_gen__call(pred_id::in, proc_id::in, list(var), byte_info::in,
	byte_tree::out) is det.

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

	% Generate bytecode for a call to a builtin.

:- pred bytecode_gen__builtin(pred_id::in, proc_id::in, list(var)::in,
	byte_info::in, byte_tree::out) is det.

bytecode_gen__builtin(PredId, _ProcId, Args, ByteInfo, Code) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	(
		code_util__builtin_binop(ModuleName, PredName, 3, Binop),
		Args = [X, Y, Var]
	->
		bytecode_gen__map_var(ByteInfo, X, ByteX),
		bytecode_gen__map_var(ByteInfo, Y, ByteY),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([builtin_binop(Binop, ByteX, ByteY, ByteVar)])
	;
		code_util__builtin_unop(ModuleName, PredName, 2, Unop),
		Args = [X, Var]
	->
		bytecode_gen__map_var(ByteInfo, X, ByteX),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([builtin_unop(Unop, ByteX, ByteVar)])
	;
		error("unknown builtin predicate")
	).

	% Generate bytecode for a unification.

:- pred bytecode_gen__unify(unification::in, var::in, unify_rhs::in, byte_info::in, byte_tree::out)
	is det.

% bytecode_gen__unify(construct(Var, ConsId, Args, Modes), _, _, ByteInfo, Code) :-
% bytecode_gen__unify(deconstruct(Var, ConsId, Args, Modes, _), _, _, ByteInfo, Code) :-
% bytecode_gen__unify(assign(Target, Source), _, _, ByteInfo, Code) :-
% 	Code = 
% bytecode_gen__unify(simple_test(Var1, Var2), _, _, ByteInfo, Code) :-
% bytecode_gen__unify(complicated_unify(Var1, Var2), Var, RHS, ByteInfo, Code) :-

	% Generate bytecode for each disjunct of a disjunction.

:- pred bytecode_gen__conj(list(hlds__goal)::in, byte_info::in,
	int::in, int::out, byte_tree::out) is det.

bytecode_gen__conj([], _, N, N, empty).
bytecode_gen__conj([Goal | Goals], ByteInfo, N0, N, Code) :-
	bytecode_gen__goal(Goal, ByteInfo, N0, N1, ThisCode),
	bytecode_gen__conj(Goals, ByteInfo, N1, N, OtherCode),
	Code = tree(ThisCode, OtherCode).

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

:- type byte_info	--->	byte_info(
					map(var, byte_var),
					module_info).

:- pred bytecode_gen__init_byte_info(module_info::in, hlds__goal::in,
	byte_info::out) is det.

bytecode_gen__init_byte_info(ModuleInfo, Goal, ByteInfo) :-
	map__init(VarMap0),
	goal_util__goal_vars(Goal, Vars),
	set__to_sorted_list(Vars, VarList),
	bytecode_gen__create_varmap(VarList, 0, VarMap0, VarMap),
	ByteInfo = byte_info(VarMap, ModuleInfo).

:- pred bytecode_gen__get_module_info(byte_info::in, module_info::out) is det.

bytecode_gen__get_module_info(byte_info(_, ModuleInfo), ModuleInfo).

:- pred bytecode_gen__map_var(byte_info::in, var::in, byte_var::out) is det.

bytecode_gen__map_var(byte_info(VarMap, _), Var, ByteVar) :-
	map__lookup(VarMap, Var, ByteVar).

:- pred bytecode_gen__create_varmap(list(var)::in, int::in,
	map(var, byte_var)::in, map(var, byte_var)::out) is det.

bytecode_gen__create_varmap([], _, VarMap, VarMap).
bytecode_gen__create_varmap([Var | VarList], N0, VarMap0, VarMap) :-
	map__det_insert(VarMap0, Var, N0, VarMap1),
	N1 is N0 + 1,
	bytecode_gen__create_varmap(VarList, N1, VarMap1, VarMap).
