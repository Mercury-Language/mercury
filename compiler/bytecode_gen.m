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

:- import_module hlds, io.

:- pred byte_gen__module(module_info::in, list(byte_code)::out,
	io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

byte_gen__module(ModuleInfo, MLDS) -->
	{ module_info_predids(ModuleInfo0, PredIds) },
	byte_gen__preds(PredIds, ModuleInfo, MLDS).

:- pred byte_gen__preds(list(pred_id)::in, module_info::in,
	list(byte)::out, io__state::di, io__state::uo) is det.

byte_gen__preds([], _ModuleInfo, empty) --> [].
byte_gen__preds([PredId | PredIds], ModuleInfo, MLDS) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		{ MLDS1 = empty }
	;
		byte_gen__pred(PredId, ProcIds, PredInfo, ModuleInfo,
			MLDS1)
	),
	byte_gen__preds(PredIds, ModuleInfo, MLDS2),
	{ MLDS = tree(MLDS1, MLDS2) }.

:- pred byte_gen__pred(pred_id::in, list(proc_id)::in, pred_info::in,
	module_info::in, io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%

:- pred byte_gen__goal_list(hlds__goal_expr::in, byte_tree::out) is det.

byte_gen__goal_list([], empty).
byte_gen__goal_list([Goal | Goals], Code) :-
	byte_gen__goal(Goal, Code1),
	byte_gen__goal_list(Goals, Code2),
	Code = tree(Code1, Code2).

:- pred byte_gen__goal(hlds__goal::in, byte_tree::out) is det.

byte_gen__goal(GoalExpr - _GoalInfo, Code) :-
	byte_gen__goal_expr(GoalExpr, Code).

:- pred byte_gen__goal_expr(hlds__goal_expr::in, byte_tree::out) is det.

byte_gen__goal_expr(GoalExpr, Code) :-
	(
		GoalExpr = call(PredId, ProcId, Args, IsBuiltin, _, SymName, _),
		( IsBuiltin = yes ->
			byte_gen__builtin()
		;
			byte_gen__call(PredId, )
		)
	;
		GoalExpr = conj(GoalList),
		byte_gen__goal_list(GoalList, Code)
	).

:- pred byte_gen__call(pred_id::in, proc_id::in, byte_tree::out) is det.

byte_gen__goal_call(PredId, ProcId, Code) :-
	GoalExpr = call(PredId, ProcId, Args, _, _, SymName, _),

	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
        { assoc_list__from_corresponding_lists(Arguments, ArgInfo, Args) },
        { call_gen__select_out_args(Args, OutArgs) },
        call_gen__save_variables(OutArgs, CodeA),
        code_info__setup_call(Args, caller, CodeB),
        code_info__get_next_label(ReturnLabel),
        code_info__get_module_info(ModuleInfo),
        { call_gen__input_args(ArgInfo, InputArguments) },
        call_gen__generate_call_livevals(OutArgs, InputArguments, CodeC0),
        { call_gen__output_args(Args, OutputArguments) },

