%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% file: code_util.m.
%
% various utilities routines for code generation and recognition
% of builtins.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module ll_backend__code_util.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module ll_backend__llds.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module list, assoc_list, std_util.

	% Create a code address which holds the address of the specified
	% procedure.
	% The `immed' argument should be `no' if the the caller wants the
	% returned address to be valid from everywhere in the program.
	% If being valid from within the current procedure is enough,
	% this argument should be `yes' wrapped around the value of the
	% --procs-per-c-function option and the current procedure id.
	% Using an address that is only valid from within the current
	% procedure may make jumps more efficient.

:- type immed == maybe(pair(int, pred_proc_id)).

:- pred code_util__make_entry_label(module_info::in, pred_id::in, proc_id::in,
	immed::in, code_addr::out) is det.

:- pred code_util__make_entry_label_from_rtti(rtti_proc_label::in, immed::in,
	code_addr::out) is det.

	% Create a label which holds the address of the specified procedure,
	% which must be defined in the current module (procedures that are
	% imported from other modules have representations only as code_addrs,
	% not as labels, since their address is not known at C compilation
	% time).
	% The fourth argument has the same meaning as for
	% code_util__make_entry_label.

:- pred code_util__make_local_entry_label(module_info::in,
	pred_id::in, proc_id::in, immed::in, label::out) is det.

	% Create a label internal to a Mercury procedure.
:- pred code_util__make_internal_label(module_info::in,
	pred_id::in, proc_id::in, int::in, label::out) is det.

:- pred code_util__extract_proc_label_from_code_addr(code_addr::in,
	proc_label::out) is det.

:- pred code_util__arg_loc_to_register(arg_loc::in, lval::out) is det.

:- pred code_util__max_mentioned_reg(list(lval)::in, int::out) is det.
:- pred code_util__max_mentioned_abs_reg(list(abs_locn)::in, int::out) is det.

:- pred code_util__goal_may_alloc_temp_frame(hlds_goal::in) is semidet.

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

:- pred code_util__neg_rval(rval::in, rval::out) is det.

:- pred code_util__negate_the_test(list(instruction)::in,
	list(instruction)::out) is det.

	% These predicates return the set of lvals referenced in an rval
	% and an lval respectively. Lvals referenced indirectly through
	% lvals of the form var(_) are not counted.

:- pred code_util__lvals_in_rval(rval::in, list(lval)::out) is det.
:- pred code_util__lvals_in_lval(lval::in, list(lval)::out) is det.
:- pred code_util__lvals_in_lvals(list(lval)::in, list(lval)::out) is det.

	% Given a procedure that already has its arg_info field filled in,
	% return a list giving its input variables and their initial locations.

:- pred build_input_arg_list(proc_info::in,
	assoc_list(prog_var, lval)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module backend_libs__proc_label.
:- import_module backend_libs__rtti.
:- import_module hlds__code_model.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__prog_util.

:- import_module bool, char, int, string, set, term, varset, require.

%---------------------------------------------------------------------------%

code_util__make_entry_label(ModuleInfo, PredId, ProcId, Immed, ProcAddr) :-
	RttiProcLabel = rtti__make_rtti_proc_label(ModuleInfo, PredId, ProcId),
	code_util__make_entry_label_from_rtti(RttiProcLabel, Immed, ProcAddr).

code_util__make_entry_label_from_rtti(RttiProcLabel, Immed, ProcAddr) :-
	( RttiProcLabel ^ proc_is_imported = yes ->
		ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
		ProcAddr = imported(ProcLabel)
	;
		code_util__make_local_entry_label_from_rtti(RttiProcLabel,
			Immed, Label),
		ProcAddr = label(Label)
	).

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Immed, Label) :-
	RttiProcLabel = rtti__make_rtti_proc_label(ModuleInfo, PredId, ProcId),
	code_util__make_local_entry_label_from_rtti(RttiProcLabel,
		Immed, Label).

:- pred code_util__make_local_entry_label_from_rtti(rtti_proc_label::in,
	immed::in, label::out) is det.

code_util__make_local_entry_label_from_rtti(RttiProcLabel, Immed, Label) :-
	ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
	(
		Immed = no,
		% If we want to define the label or use it to put it
		% into a data structure, a label that is usable only
		% within the current C module won't do.
		( RttiProcLabel ^ proc_is_exported = yes ->
			EntryType = exported
		;
			EntryType = local
		),
		Label = entry(EntryType, ProcLabel)
	;
		Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)),
		choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
			RttiProcLabel^pred_id, RttiProcLabel^proc_id,
			ProcLabel, Label)
	).

:- pred choose_local_label_type(int::in, pred_id::in, proc_id::in,
	pred_id::in, proc_id::in, proc_label::in, label::out) is det.

choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
		PredId, ProcId, ProcLabel, Label) :-
	(
		% If we want to branch to the label now,
		% we prefer a form that are usable only within
		% the current C module, since it is likely
		% to be faster.
		(
			ProcsPerFunc = 0
		;
			PredId = CurPredId,
			ProcId = CurProcId
		)
	->
		EntryType = c_local
	;
		EntryType = local
	),
	Label = entry(EntryType, ProcLabel).

%-----------------------------------------------------------------------------%

code_util__make_internal_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
	Label = internal(LabelNum, ProcLabel).

code_util__extract_proc_label_from_code_addr(CodeAddr, ProcLabel) :-
	( CodeAddr = label(Label) ->
		ProcLabel = get_proc_label(Label)
	; CodeAddr = imported(ProcLabelPrime) ->
		ProcLabel = ProcLabelPrime
	;
		error("code_util__extract_label_from_code_addr failed")
	).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, reg(r, ArgLoc)).

%-----------------------------------------------------------------------------%

code_util__max_mentioned_reg(Lvals, MaxRegNum) :-
	code_util__max_mentioned_reg_2(Lvals, 0, MaxRegNum).

:- pred code_util__max_mentioned_reg_2(list(lval)::in, int::in, int::out)
	is det.

code_util__max_mentioned_reg_2([], !MaxRegNum).
code_util__max_mentioned_reg_2([Lval | Lvals], !MaxRegNum) :-
	( Lval = reg(r, N) ->
		int__max(N, !MaxRegNum)
	;
		true
	),
	code_util__max_mentioned_reg_2(Lvals, !MaxRegNum).

code_util__max_mentioned_abs_reg(Lvals, MaxRegNum) :-
	code_util__max_mentioned_abs_reg_2(Lvals, 0, MaxRegNum).

:- pred code_util__max_mentioned_abs_reg_2(list(abs_locn)::in,
	int::in, int::out) is det.

code_util__max_mentioned_abs_reg_2([], !MaxRegNum).
code_util__max_mentioned_abs_reg_2([Lval | Lvals], !MaxRegNum) :-
	( Lval = abs_reg(N) ->
		int__max(N, !MaxRegNum)
	;
		true
	),
	code_util__max_mentioned_abs_reg_2(Lvals, !MaxRegNum).

%-----------------------------------------------------------------------------%

code_util__goal_may_alloc_temp_frame(Goal) :-
	code_util__goal_may_alloc_temp_frame(Goal, yes).

:- pred code_util__goal_may_alloc_temp_frame(hlds_goal::in, bool::out) is det.

code_util__goal_may_alloc_temp_frame(Goal - _GoalInfo, May) :-
	code_util__goal_may_alloc_temp_frame_2(Goal, May).

:- pred code_util__goal_may_alloc_temp_frame_2(hlds_goal_expr::in, bool::out)
	is det.

code_util__goal_may_alloc_temp_frame_2(generic_call(_, _, _, _), no).
code_util__goal_may_alloc_temp_frame_2(call(_, _, _, _, _, _), no).
code_util__goal_may_alloc_temp_frame_2(unify(_, _, _, _, _), no).
	% We cannot safely say that a foreign code fragment does not allocate
	% temporary nondet frames without knowing all the #defined macros
	% that expand to mktempframe and variants thereof. The performance
	% impact of being too conservative is probably not too bad.
code_util__goal_may_alloc_temp_frame_2(foreign_proc(_, _, _, _, _, _), yes).
code_util__goal_may_alloc_temp_frame_2(some(_Vars, _, Goal), May) :-
	Goal = _ - GoalInfo,
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		May = yes
	;
		code_util__goal_may_alloc_temp_frame(Goal, May)
	).
code_util__goal_may_alloc_temp_frame_2(not(Goal), May) :-
	code_util__goal_may_alloc_temp_frame(Goal, May).
code_util__goal_may_alloc_temp_frame_2(conj(Goals), May) :-
	code_util__goal_list_may_alloc_temp_frame(Goals, May).
code_util__goal_may_alloc_temp_frame_2(par_conj(Goals), May) :-
	code_util__goal_list_may_alloc_temp_frame(Goals, May).
code_util__goal_may_alloc_temp_frame_2(disj(Goals), May) :-
	code_util__goal_list_may_alloc_temp_frame(Goals, May).
code_util__goal_may_alloc_temp_frame_2(switch(_Var, _Det, Cases), May) :-
	code_util__cases_may_alloc_temp_frame(Cases, May).
code_util__goal_may_alloc_temp_frame_2(if_then_else(_Vars, C, T, E), May) :-
	( code_util__goal_may_alloc_temp_frame(C, yes) ->
		May = yes
	; code_util__goal_may_alloc_temp_frame(T, yes) ->
		May = yes
	;
		code_util__goal_may_alloc_temp_frame(E, May)
	).
code_util__goal_may_alloc_temp_frame_2(shorthand(ShorthandGoal), May) :-
	code_util__goal_may_alloc_temp_frame_2_shorthand(ShorthandGoal,May).

:- pred code_util__goal_may_alloc_temp_frame_2_shorthand(
		shorthand_goal_expr::in, bool::out) is det.

code_util__goal_may_alloc_temp_frame_2_shorthand(bi_implication(G1, G2),
		May) :-
	( code_util__goal_may_alloc_temp_frame(G1, yes) ->
		May = yes
	;
		code_util__goal_may_alloc_temp_frame(G2, May)
	).

:- pred code_util__goal_list_may_alloc_temp_frame(list(hlds_goal)::in,
	bool::out) is det.

code_util__goal_list_may_alloc_temp_frame([], no).
code_util__goal_list_may_alloc_temp_frame([Goal | Goals], May) :-
	( code_util__goal_may_alloc_temp_frame(Goal, yes) ->
		May = yes
	;
		code_util__goal_list_may_alloc_temp_frame(Goals, May)
	).

:- pred code_util__cases_may_alloc_temp_frame(list(case)::in, bool::out)
	is det.

code_util__cases_may_alloc_temp_frame([], no).
code_util__cases_may_alloc_temp_frame([case(_, Goal) | Cases], May) :-
	( code_util__goal_may_alloc_temp_frame(Goal, yes) ->
		May = yes
	;
		code_util__cases_may_alloc_temp_frame(Cases, May)
	).

%-----------------------------------------------------------------------------%

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

code_util__neg_rval(Rval, NegRval) :-
	( code_util__neg_rval_2(Rval, NegRval0) ->
		NegRval = NegRval0
	;
		NegRval = unop(not, Rval)
	).

:- pred code_util__neg_rval_2(rval::in, rval::out) is semidet.

code_util__neg_rval_2(const(Const), const(NegConst)) :-
	(
		Const = true, NegConst = false
	;
		Const = false, NegConst = true
	).
code_util__neg_rval_2(unop(not, Rval), Rval).
code_util__neg_rval_2(binop(Op, X, Y), binop(NegOp, X, Y)) :-
	code_util__neg_op(Op, NegOp).

:- pred code_util__neg_op(binary_op::in, binary_op::out) is semidet.

code_util__neg_op(eq, ne).
code_util__neg_op(ne, eq).
code_util__neg_op(<, >=).
code_util__neg_op(<=, >).
code_util__neg_op(>, <=).
code_util__neg_op(>=, <).
code_util__neg_op(str_eq, str_ne).
code_util__neg_op(str_ne, str_eq).
code_util__neg_op(str_lt, str_ge).
code_util__neg_op(str_le, str_gt).
code_util__neg_op(str_gt, str_le).
code_util__neg_op(str_ge, str_lt).
code_util__neg_op(float_eq, float_ne).
code_util__neg_op(float_ne, float_eq).
code_util__neg_op(float_lt, float_ge).
code_util__neg_op(float_le, float_gt).
code_util__neg_op(float_gt, float_le).
code_util__neg_op(float_ge, float_lt).

code_util__negate_the_test([], _) :-
	error("code_util__negate_the_test on empty list").
code_util__negate_the_test([Instr0 | Instrs0], Instrs) :-
	( Instr0 = if_val(Test, Target) - Comment ->
		code_util__neg_rval(Test, NewTest),
		Instrs = [if_val(NewTest, Target) - Comment]
	;
		code_util__negate_the_test(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

code_util__lvals_in_lvals([], []).
code_util__lvals_in_lvals([First | Rest], Lvals) :-
	code_util__lvals_in_lval(First, FirstLvals),
	code_util__lvals_in_lvals(Rest, RestLvals),
	list__append(FirstLvals, RestLvals, Lvals).

code_util__lvals_in_rval(lval(Lval), [Lval | Lvals]) :-
	code_util__lvals_in_lval(Lval, Lvals).
code_util__lvals_in_rval(var(_), []).
code_util__lvals_in_rval(mkword(_, Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_rval(const(_), []).
code_util__lvals_in_rval(unop(_, Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_rval(binop(_, Rval1, Rval2), Lvals) :-
	code_util__lvals_in_rval(Rval1, Lvals1),
	code_util__lvals_in_rval(Rval2, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).
code_util__lvals_in_rval(mem_addr(MemRef), Lvals) :-
	code_util__lvals_in_mem_ref(MemRef, Lvals).

code_util__lvals_in_lval(reg(_, _), []).
code_util__lvals_in_lval(stackvar(_), []).
code_util__lvals_in_lval(framevar(_), []).
code_util__lvals_in_lval(succip, []).
code_util__lvals_in_lval(maxfr, []).
code_util__lvals_in_lval(curfr, []).
code_util__lvals_in_lval(succip(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(redofr(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(redoip(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(succfr(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(prevfr(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(hp, []).
code_util__lvals_in_lval(sp, []).
code_util__lvals_in_lval(field(_, Rval1, Rval2), Lvals) :-
	code_util__lvals_in_rval(Rval1, Lvals1),
	code_util__lvals_in_rval(Rval2, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).
code_util__lvals_in_lval(lvar(_), []).
code_util__lvals_in_lval(temp(_, _), []).
code_util__lvals_in_lval(mem_ref(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).

:- pred code_util__lvals_in_mem_ref(mem_ref::in, list(lval)::out) is det.

code_util__lvals_in_mem_ref(stackvar_ref(_), []).
code_util__lvals_in_mem_ref(framevar_ref(_), []).
code_util__lvals_in_mem_ref(heap_ref(Rval, _, _), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).

%-----------------------------------------------------------------------------%

build_input_arg_list(ProcInfo, VarLvals) :-
	proc_info_headvars(ProcInfo, HeadVars),
	proc_info_arg_info(ProcInfo, ArgInfos),
	assoc_list__from_corresponding_lists(HeadVars, ArgInfos, VarArgInfos),
	build_input_arg_list_2(VarArgInfos, VarLvals).

:- pred build_input_arg_list_2(assoc_list(prog_var, arg_info)::in,
	assoc_list(prog_var, lval)::out) is det.

build_input_arg_list_2([], []).
build_input_arg_list_2([V - Arg | Rest0], VarArgs) :-
	Arg = arg_info(Loc, Mode),
	( Mode = top_in ->
		code_util__arg_loc_to_register(Loc, Reg),
		VarArgs = [V - Reg | VarArgs0]
	;
		VarArgs = VarArgs0
	),
	build_input_arg_list_2(Rest0, VarArgs0).

%-----------------------------------------------------------------------------%
