%---------------------------------------------------------------------------%
% Copyright (C) 1996-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates bytecode, which is intended to be used by a
% (not yet implemented) bytecode interpreter/debugger.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module bytecode_backend__bytecode_gen.

:- interface.

:- import_module hlds__hlds_module, bytecode_backend__bytecode.
:- import_module io, list.

:- pred bytecode_gen__module(module_info::in, list(byte_code)::out,
	io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% We make use of some stuff from the LLDS back-end, in particular the stuff
% relating to the argument passing convention in arg_info.m and call_gen.m.
% The intent here is to use the same argument passing convention as for
% the LLDS, to allow interoperability between code compiled to bytecode
% and code compiled to machine code.
%
% XXX It might be nice to move the argument passing related stuff
% in call_gen.m that we use here into arg_info.m, and to then rework
% arg_info.m so that it didn't depend on the LLDS.

:- import_module ll_backend__arg_info.
:- import_module ll_backend__call_gen. % XXX for arg passing convention
:- import_module ll_backend__code_util.	% XXX for cons_id_to_tag

:- import_module parse_tree__prog_data.
:- import_module hlds__hlds_pred, hlds__hlds_goal, hlds__hlds_data.
:- import_module check_hlds__type_util, check_hlds__mode_util, hlds__goal_util.
:- import_module backend_libs__builtin_ops, backend_libs__code_model.
:- import_module hlds__passes_aux, hlds__error_util.
:- import_module libs__globals, libs__tree.
:- import_module parse_tree__prog_out.

:- import_module bool, int, string, list, assoc_list, set, map, varset.
:- import_module std_util, require, term.

%---------------------------------------------------------------------------%

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
		{ pred_info_arity(PredInfo, Arity) },
		{ bytecode_gen__get_is_func(PredInfo, IsFunc) },
		{ EnterCode = node([enter_pred(PredName, Arity, IsFunc,
			ProcsCount)]) },
		{ EndofCode = node([endof_pred]) },
		{ PredCode = tree(EnterCode, tree(ProcsCode, EndofCode)) }
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
	proc_info_varset(ProcInfo, VarSet),
	proc_info_interface_determinism(ProcInfo, Detism),
	determinism_to_code_model(Detism, CodeModel),

	goal_util__goal_vars(Goal, GoalVars),
	proc_info_headvars(ProcInfo, ArgVars),
	set__insert_list(GoalVars, ArgVars, Vars),
	set__to_sorted_list(Vars, VarList),
	map__init(VarMap0),
	bytecode_gen__create_varmap(VarList, VarSet, VarTypes, 0,
		VarMap0, VarMap, VarInfos),

	bytecode_gen__init_byte_info(ModuleInfo, VarMap, VarTypes, ByteInfo0),
	bytecode_gen__get_next_label(ByteInfo0, ZeroLabel, ByteInfo1),

	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, Args),

	call_gen__input_arg_locs(Args, InputArgs),
	bytecode_gen__gen_pickups(InputArgs, ByteInfo, PickupCode),

	call_gen__output_arg_locs(Args, OutputArgs),
	bytecode_gen__gen_places(OutputArgs, ByteInfo, PlaceCode),

	% If semideterministic, reserve temp slot 0 for the return value
	( CodeModel = model_semi ->
		bytecode_gen__get_next_temp(ByteInfo1, _FrameTemp, ByteInfo2)
	;
		ByteInfo2 = ByteInfo1
	),

	bytecode_gen__goal(Goal, ByteInfo2, ByteInfo3, GoalCode),

	bytecode_gen__get_next_label(ByteInfo3, EndLabel, ByteInfo),
	
	bytecode_gen__get_counts(ByteInfo, LabelCount, TempCount),

	ZeroLabelCode = node([label(ZeroLabel)]),
	BodyTree =
		tree(PickupCode,
		tree(ZeroLabelCode,
		tree(GoalCode,
		     PlaceCode))),
	tree__flatten(BodyTree, BodyList),
	list__condense(BodyList, BodyCode0),
	( list__member(not_supported, BodyCode0) ->
		BodyCode = node([not_supported])
	;
		BodyCode = node(BodyCode0)
	),
	proc_id_to_int(ProcId, ProcInt),
	EnterCode = node([enter_proc(ProcInt, Detism, LabelCount, EndLabel,
		TempCount, VarInfos)]),
	( CodeModel = model_semi ->
		EndofCode = node([semidet_succeed, label(EndLabel), endof_proc])
	;
		EndofCode = node([label(EndLabel), endof_proc])
	),
	Code = tree(EnterCode, tree(BodyCode, EndofCode)).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__goal(hlds_goal::in, byte_info::in, byte_info::out,
	byte_tree::out) is det.

bytecode_gen__goal(GoalExpr - GoalInfo, ByteInfo0, ByteInfo, Code) :-
	bytecode_gen__goal_expr(GoalExpr, GoalInfo, ByteInfo0, ByteInfo,
		GoalCode),
	goal_info_get_context(GoalInfo, Context),
	term__context_line(Context, Line),
	Code = tree(node([context(Line)]), GoalCode).

:- pred bytecode_gen__goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
	byte_info::in, byte_info::out, byte_tree::out) is det.

bytecode_gen__goal_expr(GoalExpr, GoalInfo, ByteInfo0, ByteInfo, Code) :-
	(
		GoalExpr = generic_call(GenericCallType, 
			ArgVars, ArgModes, Detism),
		( GenericCallType = higher_order(PredVar, _, _) ->
			bytecode_gen__higher_order_call(PredVar, ArgVars,
				ArgModes, Detism, ByteInfo0, Code),
			ByteInfo = ByteInfo0
		;
			% XXX
			functor(GenericCallType, _GenericCallFunctor, _),
			/*string__append_list([
				"bytecode for ", GenericCallFunctor, " calls"], Msg),
			sorry(this_file, Msg)*/
			Code = node([not_supported]),
			ByteInfo = ByteInfo0
		)
	;
		GoalExpr = call(PredId, ProcId, ArgVars, BuiltinState, _, _),
		( BuiltinState = not_builtin ->
			goal_info_get_determinism(GoalInfo, Detism),
			bytecode_gen__call(PredId, ProcId, ArgVars, Detism,
				ByteInfo0, Code)
		;
			bytecode_gen__builtin(PredId, ProcId, ArgVars,
				ByteInfo0, Code)
		),
		ByteInfo = ByteInfo0
	;
		GoalExpr = unify(Var, RHS, _Mode, Unification, _),
		bytecode_gen__unify(Unification, Var, RHS, ByteInfo0, Code),
		ByteInfo = ByteInfo0
	;
		GoalExpr = not(Goal),
		bytecode_gen__goal(Goal, ByteInfo0, ByteInfo1, SomeCode),
		bytecode_gen__get_next_label(ByteInfo1, EndLabel, ByteInfo2),
		bytecode_gen__get_next_temp(ByteInfo2, FrameTemp, ByteInfo),
		EnterCode = node([enter_negation(FrameTemp, EndLabel)]),
		EndofCode = node([endof_negation_goal(FrameTemp),
			label(EndLabel), endof_negation]),
		Code =  tree(EnterCode,
			tree(SomeCode,
			     EndofCode))
	;
		GoalExpr = some(_, _, Goal),
		bytecode_gen__goal(Goal, ByteInfo0, ByteInfo1, SomeCode),
		bytecode_gen__get_next_temp(ByteInfo1, Temp, ByteInfo),
		EnterCode = node([enter_commit(Temp)]),
		EndofCode = node([endof_commit(Temp)]),
		Code = tree(EnterCode, tree(SomeCode, EndofCode))
	;
		GoalExpr = conj(GoalList),
		bytecode_gen__conj(GoalList, ByteInfo0, ByteInfo, Code)
	;
		GoalExpr = par_conj(_GoalList, _SM),
		sorry(this_file, "bytecode_gen of parallel conjunction")
	;
		GoalExpr = disj(GoalList, _),
		( GoalList = [] ->
			Code = node([fail]),
			ByteInfo = ByteInfo0
		;
			bytecode_gen__get_next_label(ByteInfo0, EndLabel,
				ByteInfo1),
			bytecode_gen__disj(GoalList, ByteInfo1, EndLabel,
				ByteInfo, DisjCode),
			EnterCode = node([enter_disjunction(EndLabel)]),
			EndofCode = node([endof_disjunction, label(EndLabel)]),
			Code = tree(EnterCode, tree(DisjCode, EndofCode))
		)
	;
		GoalExpr = switch(Var, _, CasesList, _),
		bytecode_gen__get_next_label(ByteInfo0, EndLabel, ByteInfo1),
		bytecode_gen__switch(CasesList, Var, ByteInfo1, EndLabel,
			ByteInfo, SwitchCode),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		EnterCode = node([enter_switch(ByteVar, EndLabel)]),
		EndofCode = node([endof_switch, label(EndLabel)]),
		Code = tree(EnterCode, tree(SwitchCode, EndofCode))
	;
		GoalExpr = if_then_else(_Vars, Cond, Then, Else, _),
		bytecode_gen__get_next_label(ByteInfo0, EndLabel, ByteInfo1),
		bytecode_gen__get_next_label(ByteInfo1, ElseLabel, ByteInfo2),
		bytecode_gen__get_next_temp(ByteInfo2, FrameTemp, ByteInfo3),
		bytecode_gen__goal(Cond, ByteInfo3, ByteInfo4, CondCode),
		bytecode_gen__goal(Then, ByteInfo4, ByteInfo5, ThenCode),
		bytecode_gen__goal(Else, ByteInfo5, ByteInfo, ElseCode),
		EnterIfCode = node([enter_if(ElseLabel, EndLabel, FrameTemp)]),
		EnterThenCode = node([enter_then(FrameTemp)]),
		EndofThenCode = node([endof_then(EndLabel), label(ElseLabel),
			enter_else(FrameTemp)]),
		EndofIfCode = node([endof_if, label(EndLabel)]),
		Code =
			tree(EnterIfCode,
			tree(CondCode,
			tree(EnterThenCode,
			tree(ThenCode,
			tree(EndofThenCode,
			tree(ElseCode,
			     EndofIfCode))))))
	;
		GoalExpr = foreign_proc(_, _, _, _, _, _, _),
		Code = node([not_supported]),
		ByteInfo = ByteInfo0
	;
		GoalExpr = shorthand(_),
		% these should have been expanded out by now
		unexpected(this_file, "bytecode_gen__goal_expr: unexpected shorthand")
	).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__gen_places(list(pair(prog_var, arg_loc))::in,
		byte_info::in, byte_tree::out) is det.

bytecode_gen__gen_places([], _, empty).
bytecode_gen__gen_places([Var - Loc | OutputArgs], ByteInfo, Code) :-
	bytecode_gen__gen_places(OutputArgs, ByteInfo, OtherCode),
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	Code = tree(node([place_arg(r, Loc, ByteVar)]), OtherCode).

:- pred bytecode_gen__gen_pickups(list(pair(prog_var, arg_loc))::in,
		byte_info::in, byte_tree::out) is det.

bytecode_gen__gen_pickups([], _, empty).
bytecode_gen__gen_pickups([Var - Loc | OutputArgs], ByteInfo, Code) :-
	bytecode_gen__gen_pickups(OutputArgs, ByteInfo, OtherCode),
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	Code = tree(node([pickup_arg(r, Loc, ByteVar)]), OtherCode).

%---------------------------------------------------------------------------%

	% Generate bytecode for a higher order call.

:- pred bytecode_gen__higher_order_call(prog_var::in, list(prog_var)::in,
	list(mode)::in, determinism::in, byte_info::in, byte_tree::out) is det.

bytecode_gen__higher_order_call(PredVar, ArgVars, ArgModes, Detism,
		ByteInfo, Code) :-
	determinism_to_code_model(Detism, CodeModel),
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	list__map(bytecode_gen__get_var_type(ByteInfo), ArgVars, ArgTypes),
	make_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgVarsInfos),

	arg_info__partition_args(ArgVarsInfos, InVars, OutVars),
	list__length(InVars, NInVars),
	list__length(OutVars, NOutVars),

	call_gen__input_arg_locs(ArgVarsInfos, InputArgs),
	bytecode_gen__gen_places(InputArgs, ByteInfo, PlaceArgs),

	call_gen__output_arg_locs(ArgVarsInfos, OutputArgs),
	bytecode_gen__gen_pickups(OutputArgs, ByteInfo, PickupArgs),

	bytecode_gen__map_var(ByteInfo, PredVar, BytePredVar),
	Call = node([higher_order_call(BytePredVar, NInVars, NOutVars,
		Detism)]),
	( CodeModel = model_semi ->
		Check = node([semidet_success_check])
	;
		Check = empty
	),
	Code = tree(PlaceArgs, tree(Call, tree(Check, PickupArgs))).

	% Generate bytecode for an ordinary call.

:- pred bytecode_gen__call(pred_id::in, proc_id::in, list(prog_var)::in,
	determinism::in, byte_info::in, byte_tree::out) is det.

bytecode_gen__call(PredId, ProcId, ArgVars, Detism, ByteInfo, Code) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_arg_info(ProcInfo, ArgInfo),
	assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgVarsInfos),

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	bytecode_gen__get_is_func(PredInfo, IsFunc),

	call_gen__input_arg_locs(ArgVarsInfos, InputArgs),
	bytecode_gen__gen_places(InputArgs, ByteInfo, PlaceArgs),

	call_gen__output_arg_locs(ArgVarsInfos, OutputArgs),
	bytecode_gen__gen_pickups(OutputArgs, ByteInfo, PickupArgs),

	predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),
	proc_id_to_int(ProcId, ProcInt),
	Call = node([call(ModuleName, PredName, Arity, IsFunc, ProcInt)]),
	determinism_to_code_model(Detism, CodeModel),
	( CodeModel = model_semi ->
		Check = node([semidet_success_check])
	;
		Check = empty
	),
	Code = tree(PlaceArgs, tree(Call, tree(Check, PickupArgs))).

	% Generate bytecode for a call to a builtin.

:- pred bytecode_gen__builtin(pred_id::in, proc_id::in, list(prog_var)::in,
	byte_info::in, byte_tree::out) is det.

bytecode_gen__builtin(PredId, ProcId, Args, ByteInfo, Code) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	predicate_module(ModuleInfo, PredId, ModuleName),
	predicate_name(ModuleInfo, PredId, PredName),
	(
		builtin_ops__translate_builtin(ModuleName, PredName, ProcId,
			Args, SimpleCode)
	->
		(
			SimpleCode = test(Test),
			bytecode_gen__map_test(ByteInfo, Test, Code)
		;
			SimpleCode = assign(Var, Expr),
			bytecode_gen__map_assign(ByteInfo, Var, Expr,
				Code)
		)
	;
		string__append("unknown builtin predicate ", PredName, Msg),
		unexpected(this_file, Msg)
	).

:- pred bytecode_gen__map_test(byte_info::in,
		simple_expr(prog_var)::in(simple_test_expr),
		byte_tree::out) is det.

bytecode_gen__map_test(ByteInfo, TestExpr, Code) :-
	(
		TestExpr = binary(Binop, X, Y),
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		bytecode_gen__map_arg(ByteInfo, Y, ByteY),
		Code = node([builtin_bintest(Binop, ByteX, ByteY)])
	;
		TestExpr = unary(Unop, X),
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		Code = node([builtin_untest(Unop, ByteX)])
	).

:- pred bytecode_gen__map_assign(byte_info::in, prog_var::in,
		simple_expr(prog_var)::in(simple_assign_expr),
		byte_tree::out) is det.

bytecode_gen__map_assign(ByteInfo, Var, Expr, Code) :-
	(
		Expr = binary(Binop, X, Y),
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		bytecode_gen__map_arg(ByteInfo, Y, ByteY),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([builtin_binop(Binop, ByteX, ByteY, ByteVar)])
	;
		Expr = unary(Unop, X),
		bytecode_gen__map_arg(ByteInfo, X, ByteX),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([builtin_unop(Unop, ByteX, ByteVar)])
	;
		Expr = leaf(X),
		bytecode_gen__map_var(ByteInfo, X, ByteX),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		Code = node([assign(ByteVar, ByteX)])
	).

:- pred bytecode_gen__map_arg(byte_info::in,
		simple_expr(prog_var)::in(simple_arg_expr),
		byte_arg::out) is det.

bytecode_gen__map_arg(ByteInfo, Expr, ByteArg) :-
	(
		Expr = leaf(Var),
		bytecode_gen__map_var(ByteInfo, Var, ByteVar),
		ByteArg = var(ByteVar)
	;
		Expr = int_const(IntVal),
		ByteArg = int_const(IntVal)
	;
		Expr = float_const(FloatVal),
		ByteArg = float_const(FloatVal)
	).

%---------------------------------------------------------------------------%

	% Generate bytecode for a unification.

:- pred bytecode_gen__unify(unification::in, prog_var::in, unify_rhs::in,
	byte_info::in, byte_tree::out) is det.

bytecode_gen__unify(construct(Var, ConsId, Args, UniModes, _, _, _),
		_, _, ByteInfo, Code) :-
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	bytecode_gen__map_vars(ByteInfo, Args, ByteArgs),
	bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
	(
		ByteConsId = pred_const(_, _, _, _, _)
	->
		Code = node([construct(ByteVar, ByteConsId, ByteArgs)])
	;
		% Don't call bytecode_gen__map_uni_modes until after
		% the pred_const test fails, since the arg-modes on
		% unifications that create closures aren't like other arg-modes.
		bytecode_gen__map_uni_modes(UniModes, Args, ByteInfo, Dirs),
		(
			bytecode_gen__all_dirs_same(Dirs, to_var)
		->
			Code = node([construct(ByteVar, ByteConsId, ByteArgs)])
		;
			assoc_list__from_corresponding_lists(ByteArgs, Dirs,
				Pairs),
			Code = node([complex_construct(ByteVar, ByteConsId,
				Pairs)])
		)
	).
bytecode_gen__unify(deconstruct(Var, ConsId, Args, UniModes, _, _), _, _,
		ByteInfo, Code) :-
	bytecode_gen__map_var(ByteInfo, Var, ByteVar),
	bytecode_gen__map_vars(ByteInfo, Args, ByteArgs),
	bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
	bytecode_gen__map_uni_modes(UniModes, Args, ByteInfo, Dirs),
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
	bytecode_gen__get_var_type(ByteInfo, Var1, Var1Type),
	bytecode_gen__get_var_type(ByteInfo, Var2, Var2Type),

	(
		type_to_ctor_and_args(Var1Type, TypeCtor1, _),
		type_to_ctor_and_args(Var2Type, TypeCtor2, _)
	->
		( TypeCtor2 = TypeCtor1 ->
			TypeCtor = TypeCtor1
		;	unexpected(this_file,
				"simple_test between different types")
		)
	;
		unexpected(this_file, "failed lookup of type id")
	),

	ByteInfo = byte_info(_, _, ModuleInfo, _, _),

	classify_type_ctor(ModuleInfo, TypeCtor, BuiltinType),

	(
		BuiltinType = int_type,
		TestId = int_test
	
	;	BuiltinType = char_type,
		TestId = char_test
	
	;	BuiltinType = str_type,
		TestId = string_test
	
	;	BuiltinType = float_type,
		TestId = float_test 
	
	;	BuiltinType = enum_type,
		TestId = enum_test 

	;	BuiltinType = pred_type,
		unexpected(this_file, "pred_type in simple_test")
	
	;	BuiltinType = tuple_type,
		unexpected(this_file, "tuple_type in simple_test")

	;	BuiltinType = user_type,
		unexpected(this_file, "user_type in simple_test")

	;	BuiltinType = polymorphic_type,
		unexpected(this_file, "polymorphic_type in simple_test")

	),
	Code = node([test(ByteVar1, ByteVar2, TestId)]).
bytecode_gen__unify(complicated_unify(_,_,_), _Var, _RHS, _ByteInfo, _Code) :-
	unexpected(this_file,
		"complicated unifications should have been handled by polymorphism.m").

:- pred bytecode_gen__map_uni_modes(list(uni_mode)::in, list(prog_var)::in,
	byte_info::in, list(byte_dir)::out) is det.

bytecode_gen__map_uni_modes([], [], _, []).
bytecode_gen__map_uni_modes([UniMode | UniModes], [Arg | Args], ByteInfo,
		[Dir | Dirs]) :-
	UniMode = ((VarInitial - ArgInitial) -> (VarFinal - ArgFinal)),
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	bytecode_gen__get_var_type(ByteInfo, Arg, Type),
	mode_to_arg_mode(ModuleInfo, (VarInitial -> VarFinal), Type, VarMode),
	mode_to_arg_mode(ModuleInfo, (ArgInitial -> ArgFinal), Type, ArgMode),
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
		VarMode = top_unused,
		ArgMode = top_unused
	->
		Dir = to_none
	;
		unexpected(this_file, "invalid mode for (de)construct unification")
	),
	bytecode_gen__map_uni_modes(UniModes, Args, ByteInfo, Dirs).
bytecode_gen__map_uni_modes([], [_|_], _, _) :-
	unexpected(this_file, "bytecode_gen__map_uni_modes: length mismatch").
bytecode_gen__map_uni_modes([_|_], [], _, _) :-
	unexpected(this_file, "bytecode_gen__map_uni_modes: length mismatch").

:- pred bytecode_gen__all_dirs_same(list(byte_dir)::in, byte_dir::in)
	is semidet.

bytecode_gen__all_dirs_same([], _).
bytecode_gen__all_dirs_same([Dir | Dirs], Dir) :-
	bytecode_gen__all_dirs_same(Dirs, Dir).

%---------------------------------------------------------------------------%

	% Generate bytecode for a conjunction

:- pred bytecode_gen__conj(list(hlds_goal)::in, byte_info::in, byte_info::out,
	byte_tree::out) is det.

bytecode_gen__conj([], ByteInfo, ByteInfo, empty).
bytecode_gen__conj([Goal | Goals], ByteInfo0, ByteInfo, Code) :-
	bytecode_gen__goal(Goal, ByteInfo0, ByteInfo1, ThisCode),
	bytecode_gen__conj(Goals, ByteInfo1, ByteInfo, OtherCode),
	Code = tree(ThisCode, OtherCode).

%---------------------------------------------------------------------------%

	% Generate bytecode for each disjunct of a disjunction.

:- pred bytecode_gen__disj(list(hlds_goal)::in, byte_info::in,
	int::in, byte_info::out,  byte_tree::out) is det.

bytecode_gen__disj([], _, _, _, _) :-
	unexpected(this_file, "empty disjunction in bytecode_gen__disj").
bytecode_gen__disj([Disjunct | Disjuncts], ByteInfo0, EndLabel,
		ByteInfo, Code) :-
	bytecode_gen__goal(Disjunct, ByteInfo0, ByteInfo1, ThisCode),
	( Disjuncts = [] ->
		EnterCode = node([enter_disjunct(-1)]),
		EndofCode = node([endof_disjunct(EndLabel)]),
		Code = tree(EnterCode, tree(ThisCode, EndofCode)),
		ByteInfo = ByteInfo1
	;
		bytecode_gen__disj(Disjuncts, ByteInfo1, EndLabel,
			ByteInfo2, OtherCode),
		bytecode_gen__get_next_label(ByteInfo2, NextLabel, ByteInfo),
		EnterCode = node([enter_disjunct(NextLabel)]),
		EndofCode = node([endof_disjunct(EndLabel), label(NextLabel)]),
		Code =
			tree(EnterCode,
			tree(ThisCode,
			tree(EndofCode,
			     OtherCode)))
	).

%---------------------------------------------------------------------------%

	% Generate bytecode for each arm of a switch.

:- pred bytecode_gen__switch(list(case)::in, prog_var::in, byte_info::in,
	int::in, byte_info::out, byte_tree::out) is det.

bytecode_gen__switch([], _, ByteInfo, _, ByteInfo, empty).
bytecode_gen__switch([case(ConsId, Goal) | Cases], Var, ByteInfo0, EndLabel,
		ByteInfo, Code) :-
	bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
	bytecode_gen__goal(Goal, ByteInfo0, ByteInfo1, ThisCode),
	bytecode_gen__switch(Cases, Var, ByteInfo1, EndLabel,
		ByteInfo2, OtherCode),
	bytecode_gen__get_next_label(ByteInfo2, NextLabel, ByteInfo),
	EnterCode = node([enter_switch_arm(ByteConsId, NextLabel)]),
	EndofCode = node([endof_switch_arm(EndLabel), label(NextLabel)]),
	Code = tree(EnterCode, tree(ThisCode, tree(EndofCode, OtherCode))).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__map_cons_id(byte_info::in, prog_var::in, cons_id::in,
	byte_cons_id::out) is det.

bytecode_gen__map_cons_id(ByteInfo, Var, ConsId, ByteConsId) :-
	bytecode_gen__get_module_info(ByteInfo, ModuleInfo),
	(
		ConsId = cons(Functor, Arity),
		bytecode_gen__get_var_type(ByteInfo, Var, Type),
		(
			% Everything other than characters and tuples should
			% be module qualified.
			Functor = unqualified(FunctorName),
			\+ type_is_tuple(Type, _)
		->
			string__to_char_list(FunctorName, FunctorList),
			( FunctorList = [Char] ->
				ByteConsId = char_const(Char)
			;	
				unexpected(this_file,
					"map_cons_id: unqualified cons_id is not a char_const")
			)
		;
			(
				Functor = unqualified(FunctorName),
				ModuleName = unqualified("builtin")
			;
				Functor = qualified(ModuleName, FunctorName)
			),
			code_util__cons_id_to_tag(ConsId,
				Type, ModuleInfo, ConsTag),
			bytecode_gen__map_cons_tag(ConsTag, ByteConsTag),
			ByteConsId = cons(ModuleName, FunctorName,
				Arity, ByteConsTag)
		)
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
		ConsId = pred_const(PredId, ProcId, EvalMethod),
		( EvalMethod = normal ->
			predicate_id(ModuleInfo, PredId,
				ModuleName, PredName, Arity),

			module_info_pred_info(ModuleInfo, PredId, PredInfo),
			bytecode_gen__get_is_func(PredInfo, IsFunc),
				
			proc_id_to_int(ProcId, ProcInt),
			ByteConsId = pred_const(ModuleName,
				PredName, Arity, IsFunc, ProcInt)
		;
			% XXX
			sorry(this_file, "bytecode for Aditi lambda expressions")
		)
	;
		ConsId = code_addr_const(PredId, ProcId),
		predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),
		proc_id_to_int(ProcId, ProcInt),
		ByteConsId = code_addr_const(ModuleName, PredName, Arity,
			ProcInt)
	;
		ConsId = type_ctor_info_const(ModuleName, TypeName, TypeArity),
		ByteConsId = type_ctor_info_const(ModuleName, TypeName,
			TypeArity)
	;
		ConsId = base_typeclass_info_const(ModuleName, ClassId,
			_, Instance),
		ByteConsId = base_typeclass_info_const(ModuleName, ClassId,
			Instance)
	;
		ConsId = tabling_pointer_const(_, _),
		sorry(this_file, "bytecode cannot implement tabling")
	;
		ConsId = table_io_decl(_),
		sorry(this_file, "bytecode cannot implement table io decl")
	;
		ConsId = deep_profiling_proc_static(_),
		sorry(this_file, "bytecode cannot implement deep profiling")
	).

:- pred bytecode_gen__map_cons_tag(cons_tag::in, byte_cons_tag::out) is det.

bytecode_gen__map_cons_tag(no_tag, no_tag).
	% `single_functor' is just an optimized version of `unshared_tag(0)'
	% this optimization is not important for the bytecode
bytecode_gen__map_cons_tag(single_functor, unshared_tag(0)).
bytecode_gen__map_cons_tag(unshared_tag(Primary), unshared_tag(Primary)).
bytecode_gen__map_cons_tag(shared_remote_tag(Primary, Secondary),
	shared_remote_tag(Primary, Secondary)).
bytecode_gen__map_cons_tag(shared_local_tag(Primary, Secondary),
	shared_local_tag(Primary, Secondary)).
bytecode_gen__map_cons_tag(string_constant(_), _) :-
	unexpected(this_file, "string_constant cons tag for non-string_constant cons id").
bytecode_gen__map_cons_tag(int_constant(IntVal), enum_tag(IntVal)).
bytecode_gen__map_cons_tag(float_constant(_), _) :-
	unexpected(this_file, "float_constant cons tag for non-float_constant cons id").
bytecode_gen__map_cons_tag(pred_closure_tag(_, _, _), _) :-
	unexpected(this_file, "pred_closure_tag cons tag for non-pred_const cons id").
bytecode_gen__map_cons_tag(code_addr_constant(_, _), _) :-
	unexpected(this_file, "code_addr_constant cons tag for non-address_const cons id").
bytecode_gen__map_cons_tag(type_ctor_info_constant(_, _, _), _) :-
	unexpected(this_file, "type_ctor_info_constant cons tag for non-type_ctor_info_constant cons id").
bytecode_gen__map_cons_tag(base_typeclass_info_constant(_, _, _), _) :-
	unexpected(this_file, "base_typeclass_info_constant cons tag for non-base_typeclass_info_constant cons id").
bytecode_gen__map_cons_tag(tabling_pointer_constant(_, _), _) :-
	unexpected(this_file, "tabling_pointer_constant cons tag for non-tabling_pointer_constant cons id").
bytecode_gen__map_cons_tag(deep_profiling_proc_static_tag(_), _) :-
	unexpected(this_file, "deep_profiling_proc_static_tag cons tag for non-deep_profiling_proc_static cons id").
bytecode_gen__map_cons_tag(table_io_decl_tag(_), _) :-
	unexpected(this_file, "table_io_decl_tag cons tag for non-table_io_decl cons id").
bytecode_gen__map_cons_tag(reserved_address(_), _) :-
	% These should only be generated if the --num-reserved-addresses
	% or --num-reserved-objects options are used.
	sorry(this_file,
	   "bytecode with --num-reserved-addresses or --num-reserved-objects").
bytecode_gen__map_cons_tag(shared_with_reserved_addresses(_, _), _) :-
	% These should only be generated if the --num-reserved-addresses
	% or --num-reserved-objects options are used.
	sorry(this_file,
	   "bytecode with --num-reserved-addresses or --num-reserved-objects").

%---------------------------------------------------------------------------%

:- pred bytecode_gen__create_varmap(list(prog_var)::in, prog_varset::in,
	map(prog_var, type)::in, int::in, map(prog_var, byte_var)::in,
	map(prog_var, byte_var)::out, list(byte_var_info)::out) is det.

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

:- type byte_info
	--->	byte_info(
			map(prog_var, byte_var),
			map(prog_var, type),
			module_info,
			int,		% next label number to use
			int		% next temp number to use
		).

:- pred bytecode_gen__init_byte_info(module_info::in,
	map(prog_var, byte_var)::in, map(prog_var, type)::in,
	byte_info::out) is det.  
bytecode_gen__init_byte_info(ModuleInfo, VarMap, VarTypes, ByteInfo) :-
	ByteInfo = byte_info(VarMap, VarTypes, ModuleInfo, 0, 0).

:- pred bytecode_gen__get_module_info(byte_info::in, module_info::out) is det.

bytecode_gen__get_module_info(byte_info(_, _, ModuleInfo, _, _), ModuleInfo).

:- pred bytecode_gen__map_vars(byte_info::in,
	list(prog_var)::in, list(byte_var)::out) is det.

bytecode_gen__map_vars(byte_info(VarMap, _, _, _, _), Vars, ByteVars) :-
	bytecode_gen__map_vars_2(VarMap, Vars, ByteVars).

:- pred bytecode_gen__map_vars_2(map(prog_var, byte_var)::in,
	list(prog_var)::in, list(byte_var)::out) is det.

bytecode_gen__map_vars_2(_VarMap, [], []).
bytecode_gen__map_vars_2(VarMap, [Var | Vars], [ByteVar | ByteVars]) :-
	map__lookup(VarMap, Var, ByteVar),
	bytecode_gen__map_vars_2(VarMap, Vars, ByteVars).

:- pred bytecode_gen__map_var(byte_info::in, prog_var::in,
	byte_var::out) is det.

bytecode_gen__map_var(byte_info(VarMap, _, _, _, _), Var, ByteVar) :-
	map__lookup(VarMap, Var, ByteVar).

:- pred bytecode_gen__get_var_type(byte_info::in, prog_var::in,
	(type)::out) is det.

bytecode_gen__get_var_type(byte_info(_, VarTypes, _, _, _), Var, Type) :-
	map__lookup(VarTypes, Var, Type).

:- pred bytecode_gen__get_next_label(byte_info::in, int::out, byte_info::out)
	is det.

bytecode_gen__get_next_label(ByteInfo0, Label0, ByteInfo) :-
	ByteInfo0 = byte_info(VarMap, VarTypes, ModuleInfo, Label0, Temp),
	Label is Label0 + 1,
	ByteInfo  = byte_info(VarMap, VarTypes, ModuleInfo, Label,  Temp).

:- pred bytecode_gen__get_next_temp(byte_info::in, int::out, byte_info::out)
	is det.

bytecode_gen__get_next_temp(ByteInfo0, Temp0, ByteInfo) :-
	ByteInfo0 = byte_info(VarMap, VarTypes, ModuleInfo, Label, Temp0),
	Temp is Temp0 + 1,
	ByteInfo  = byte_info(VarMap, VarTypes, ModuleInfo, Label, Temp).

:- pred bytecode_gen__get_counts(byte_info::in, int::out, int::out) is det.

bytecode_gen__get_counts(byte_info(_, _, _, Label, Temp), Label, Temp).

%---------------------------------------------------------------------------%

:- pred bytecode_gen__get_is_func(pred_info, byte_is_func).
:- mode bytecode_gen__get_is_func(in, out) is det.

bytecode_gen__get_is_func(PredInfo, IsFunc) :-
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	(	PredOrFunc = predicate
	->	IsFunc = 0
	;	IsFunc = 1
	).

%---------------------------------------------------------------------------%

:- func this_file = string.
this_file = "bytecode_gen.m".

:- end_module bytecode_gen.

%---------------------------------------------------------------------------%
