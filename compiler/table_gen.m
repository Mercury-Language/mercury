%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: ohutch, zs.
%
% This module transforms HLDS code to implement loop detection, memoing,
% minimal model evaluation, or I/O idempotence. The transformation involves
% adding calls to predicates defined in library/table_builtin.m and in
% runtime/mercury_minimal_model.c.
%
% The loop detection transformation adds code to a procedure that allows
% early detection of infinite loops. If such loops are detected the program
% will terminate with a helpful error message.
%
% The memo transformation adds code that allows a procedure to "memo"
% (remember) answers once they have been generated using program clause
% resolution.
%
% The minimal model transformation changes the semantics of the procedure
% being transformed. See the PhD thesis of K. Sagonas: `The SLG-WAM: A
% Search-Efficient Engine for Well-Founded Evaluation of Normal Logic
% Programs' from SUNY at Stony Brook in 1996 for a description of
% the semantics behind the transformation. Currently only SLGd is
% implemented.
%
% The current implementation attempts to detect cases where tabling has
% undesirable interactions with if-then-else, solutions, and (possibly)
% negated contexts in general. However, the detection is done at runtime,
% since there is no known way of doing this compile time.

%-----------------------------------------------------------------------------%

:- module transform_hlds__table_gen.

:- interface.

:- import_module hlds__hlds_module.

:- import_module io.

:- pred table_gen__process_module(module_info::in, module_info::out,
	io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs__rtti.
:- import_module check_hlds__det_analysis.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__modes.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module hlds__code_model.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend.
:- import_module ll_backend__continuation_info.
:- import_module parse_tree__error_util.
:- import_module parse_tree__inst.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__const_prop.

:- import_module term, varset.
:- import_module bool, int, string, list, assoc_list.
:- import_module set, map, require, std_util.

%-----------------------------------------------------------------------------%

	% NOTE: following preds seem to duplicate the code in passes_aux.m.
	% The reason for this duplication is that this module needs a variant
	% of this code that is able to handle passing a module_info to
	% polymorphism and getting an updated module_info back.
table_gen__process_module(ModuleInfo0, ModuleInfo, S0, S) :-
	module_info_preds(ModuleInfo0, Preds0),
	map__keys(Preds0, PredIds),
	table_gen__process_preds(PredIds, ModuleInfo0, ModuleInfo, S0, S).

:- pred table_gen__process_preds(list(pred_id)::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

table_gen__process_preds([], ModuleInfo, ModuleInfo, S, S).
table_gen__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo, S0, S) :-
	table_gen__process_pred(PredId, ModuleInfo0, ModuleInfo1, S0, S1),
	table_gen__process_preds(PredIds, ModuleInfo1, ModuleInfo, S1, S).

:- pred table_gen__process_pred(pred_id::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

table_gen__process_pred(PredId, !ModuleInfo, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
	ProcIds = pred_info_procids(PredInfo),
	table_gen__process_procs(PredId, ProcIds, !ModuleInfo, !IO).

:- pred table_gen__process_procs(pred_id::in, list(proc_id)::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

table_gen__process_procs(_PredId, [], !ModuleInfo, !IO).
table_gen__process_procs(PredId, [ProcId | ProcIds], !ModuleInfo, !IO) :-
	module_info_preds(!.ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo0),
	table_gen__process_proc(PredId, ProcId, ProcInfo0, PredInfo,
		!ModuleInfo, !IO),
	table_gen__process_procs(PredId, ProcIds, !ModuleInfo, !IO).

:- pred table_gen__process_proc(pred_id::in, proc_id::in, proc_info::in,
	pred_info::in, module_info::in, module_info::out, io::di, io::uo)
	is det.

table_gen__process_proc(PredId, ProcId, ProcInfo0, PredInfo0,
		!ModuleInfo, !IO) :-
	proc_info_eval_method(ProcInfo0, EvalMethod),
	( eval_method_requires_tabling_transform(EvalMethod) = yes ->
		table_gen__transform_proc(EvalMethod, PredId, ProcId,
			ProcInfo0, _, PredInfo0, _, !ModuleInfo, !IO)
	;
		module_info_globals(!.ModuleInfo, Globals),
		globals__lookup_bool_option(Globals, trace_table_io, yes),
		proc_info_has_io_state_pair(!.ModuleInfo, ProcInfo0,
			_InArgNum, _OutArgNum)
	->
		proc_info_interface_code_model(ProcInfo0, CodeModel),
		( CodeModel = model_det ->
			true
		;
			pred_id_to_int(PredId, PredIdInt),
			Msg = string__format(
				"I/O procedure pred id %d not model_det",
				[i(PredIdInt)]),
			error(Msg)
		),
		globals__lookup_bool_option(Globals, trace_table_io_all,
			TransformAll),
		globals__lookup_bool_option(Globals, trace_table_io_require,
			Require),
		proc_info_goal(ProcInfo0, BodyGoal),
		predicate_module(!.ModuleInfo, PredId, PredModuleName),
		should_io_procedure_be_transformed(TransformAll, Require,
			BodyGoal, PredModuleName, AnnotationIsMissing,
			TransformPrimitive),
		(
			AnnotationIsMissing = yes,
			report_missing_tabled_for_io(!.ModuleInfo, PredInfo0,
				PredId, ProcId, !IO),
			module_info_incr_errors(!ModuleInfo)
		;
			AnnotationIsMissing = no
		),
		(
			TransformPrimitive = no
		;
			TransformPrimitive = yes(Unitize),
			globals__lookup_bool_option(Globals,
				trace_table_io_only_retry,
				TraceTableIoOnlyRetry),
			(
				TraceTableIoOnlyRetry = no,
				Decl = table_io_decl
			;
				TraceTableIoOnlyRetry = yes,
				Decl = table_io_proc
			),
			TableIoMethod = eval_table_io(Decl, Unitize),
			proc_info_set_eval_method(TableIoMethod,
				ProcInfo0, ProcInfo1),
			table_gen__transform_proc(TableIoMethod,
				PredId, ProcId, ProcInfo1, _, PredInfo0, _,
				!ModuleInfo, !IO)
		)
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred should_io_procedure_be_transformed(bool::in, bool::in, hlds_goal::in,
	sym_name::in, bool::out, maybe(table_io_is_unitize)::out) is det.

should_io_procedure_be_transformed(TransformAll, Require, BodyGoal,
		PredModuleName, AnnotationIsMissing, TransformInfo) :-
	tabled_for_io_attributes(BodyGoal, TabledForIoAttrs),
	( TabledForIoAttrs = [] ->
		AnnotationIsMissing = no,
		TransformInfo = no
	; TabledForIoAttrs = [TabledForIoAttr] ->
		(
			TabledForIoAttr = not_tabled_for_io,
			(
				Require = yes,
				\+ any_mercury_builtin_module(PredModuleName)
			->
				AnnotationIsMissing = yes,
				TransformInfo = no
			;
				AnnotationIsMissing = no,
				(
					TransformAll = no,
					TransformInfo = no
				;
					TransformAll = yes,
					may_call_mercury_attributes(BodyGoal,
						MayCallMercuryAttrs),
					(
						MayCallMercuryAttrs =
							[may_call_mercury]
					->
						TransformInfo = no
					;
						TransformInfo =
							yes(table_io_alone)
					)
				)
			)
		;
			TabledForIoAttr = tabled_for_descendant_io,
			AnnotationIsMissing = no,
			% The procedure itself doesn't do any I/O, so don't
			% transform it.
			TransformInfo = no
		;
			TabledForIoAttr = tabled_for_io,
			AnnotationIsMissing = no,
			TransformInfo = yes(table_io_alone)
		;
			TabledForIoAttr = tabled_for_io_unitize,
			AnnotationIsMissing = no,
			TransformInfo = yes(table_io_unitize)
		)
	;
		% Since table_gen is run before inlining, each procedure
		% should contain at most one foreign_proc goal.
		error("should_io_procedure_be_transformed: " ++
			"different tabled_for_io attributes in one procedure")
	).

:- pred may_call_mercury_attributes(hlds_goal::in, list(may_call_mercury)::out)
	is det.

may_call_mercury_attributes(Goal, MayCallMercuryAttrs) :-
	solutions(subgoal_may_call_mercury_attribute(Goal),
		MayCallMercuryAttrs).

:- pred subgoal_may_call_mercury_attribute(hlds_goal::in,
	may_call_mercury::out) is nondet.

subgoal_may_call_mercury_attribute(Goal, MayCallMercuryAttr) :-
	some [SubGoal,Attrs] (
		goal_contains_goal(Goal, SubGoal),
		SubGoal = foreign_proc(Attrs, _, _, _, _, _, _) - _,
		MayCallMercuryAttr = may_call_mercury(Attrs)
	).

:- pred tabled_for_io_attributes(hlds_goal::in, list(tabled_for_io)::out)
	is det.

tabled_for_io_attributes(Goal, TabledForIoAttrs) :-
	solutions(subgoal_tabled_for_io_attribute(Goal), TabledForIoAttrs).

:- pred subgoal_tabled_for_io_attribute(hlds_goal::in, tabled_for_io::out)
	is nondet.

subgoal_tabled_for_io_attribute(Goal, TabledForIoAttr) :-
	some [SubGoal,Attrs] (
		goal_contains_goal(Goal, SubGoal),
		SubGoal = foreign_proc(Attrs, _,_,_,_,_,_) - _,
		TabledForIoAttr = tabled_for_io(Attrs),
		\+ TabledForIoAttr = not_tabled_for_io
	).

:- pred report_missing_tabled_for_io(module_info::in, pred_info::in,
	pred_id::in, proc_id::in, io::di, io::uo) is det.

report_missing_tabled_for_io(ModuleInfo, PredInfo, PredId, ProcId, !IO) :-
	pred_info_context(PredInfo, Context),
	describe_one_proc_name(ModuleInfo, should_module_qualify,
		proc(PredId, ProcId), Name),
	Msg = [fixed(Name), words("contains untabled I/O primitive.")],
	error_util__write_error_pieces(Context, 0, Msg, !IO).

:- pred report_bad_mode_for_tabling(module_info::in, pred_info::in,
	pred_id::in, proc_id::in, prog_varset::in, list(prog_var)::in,
	io::di, io::uo) is det.

report_bad_mode_for_tabling(ModuleInfo, PredInfo, PredId, ProcId, VarSet, Vars,
		!IO) :-
	pred_info_context(PredInfo, Context),
	describe_one_proc_name(ModuleInfo, should_module_qualify,
		proc(PredId, ProcId), Name),
	lookup_var_names(VarSet, Vars, VarNames),
	Msg = [fixed(Name), words("contains arguments"),
		words("whose mode is incompatible with tabling;"), nl,
		words("these arguments are"), words(VarNames)],
	error_util__write_error_pieces(Context, 0, Msg, !IO).

:- pred lookup_var_names(prog_varset::in, list(prog_var)::in, string::out)
	is det.

lookup_var_names(_, [], "").
lookup_var_names(VarSet, [Var | Vars], Description) :-
	varset__lookup_name(VarSet, Var, Name),
	(
		Vars = [],
		Description = Name
	;
		Vars = [_ | _],
		lookup_var_names(VarSet, Vars, Description0),
		Description = Name ++ ", " ++ Description0
	).

%-----------------------------------------------------------------------------%

:- pred table_gen__transform_proc(eval_method::in, pred_id::in, proc_id::in,
	proc_info::in, proc_info::out, pred_info::in, pred_info::out,
	module_info::in, module_info::out, io::di, io::uo) is det.

table_gen__transform_proc(EvalMethod, PredId, ProcId, !ProcInfo, !PredInfo,
		!ModuleInfo, !IO) :-

	% grab the appropriate fields from the pred_info and proc_info
	proc_info_interface_determinism(!.ProcInfo, Detism),
	determinism_to_code_model(Detism, CodeModel),
	proc_info_headvars(!.ProcInfo, HeadVars),
	proc_info_varset(!.ProcInfo, VarSet0),
	proc_info_vartypes(!.ProcInfo, VarTypes0),
	proc_info_goal(!.ProcInfo, OrigGoal),
	proc_info_argmodes(!.ProcInfo, ArgModes),

	get_input_output_vars(HeadVars, ArgModes, !.ModuleInfo, BadVars,
		InputVars, OutputVars),
	(
		BadVars = []
	;
		BadVars = [_ | _],
		report_bad_mode_for_tabling(!.ModuleInfo, !.PredInfo,
			PredId, ProcId, VarSet0, BadVars, !IO),
		% We continue the transformation as best we can,
		% but prevent the creation of an executable.
		module_info_incr_errors(!ModuleInfo)
	),
	table_info_init(!.ModuleInfo, PredId, ProcId, !.PredInfo, !.ProcInfo,
		TableInfo0),

	(
		EvalMethod = eval_normal,
		% This should have been caught by our caller.
		error("table_gen__transform_proc: eval_normal")
	;
		EvalMethod = eval_table_io(Decl, Unitize),
		module_info_globals(!.ModuleInfo, Globals),
		globals__lookup_bool_option(Globals, trace_table_io_states,
			TableIoStates),
		table_gen__create_new_io_goal(OrigGoal, Decl, Unitize,
			TableIoStates, HeadVars, InputVars, OutputVars,
			VarTypes0, VarTypes, VarSet0, VarSet,
			TableInfo0, TableInfo, Goal, MaybeProcTableInfo),
		MaybeCallTableTip = no
	;
		EvalMethod = eval_loop_check,
		table_gen__create_new_loop_goal(Detism, OrigGoal,
			PredId, ProcId, HeadVars, InputVars, OutputVars,
			VarTypes0, VarTypes, VarSet0, VarSet,
			TableInfo0, TableInfo, CallTableTip, Goal, Steps),
		generate_gen_proc_table_info(TableInfo, Steps,
			InputVars, OutputVars, ProcTableInfo),
		MaybeCallTableTip = yes(CallTableTip),
		MaybeProcTableInfo = yes(ProcTableInfo)
	;
		EvalMethod = eval_memo,
		table_gen__create_new_memo_goal(Detism, OrigGoal,
			PredId, ProcId, HeadVars, InputVars, OutputVars,
			VarTypes0, VarTypes, VarSet0, VarSet,
			TableInfo0, TableInfo, CallTableTip, Goal, Steps),
		generate_gen_proc_table_info(TableInfo, Steps,
			InputVars, OutputVars, ProcTableInfo),
		MaybeCallTableTip = yes(CallTableTip),
		MaybeProcTableInfo = yes(ProcTableInfo)
	;
		EvalMethod = eval_minimal,
		(
			CodeModel = model_det,
			error("table_gen__transform_proc: minimal det")
		;
			CodeModel = model_semi,
			error("table_gen__transform_proc: minimal semi")
		;
			CodeModel = model_non,
			table_gen__create_new_mm_goal(Detism, OrigGoal,
				PredId, ProcId, HeadVars,
				InputVars, OutputVars, VarTypes0, VarTypes,
				VarSet0, VarSet, TableInfo0, TableInfo,
				CallTableTip, Goal, Steps)
		),
		generate_gen_proc_table_info(TableInfo, Steps,
			InputVars, OutputVars, ProcTableInfo),
		MaybeCallTableTip = yes(CallTableTip),
		MaybeProcTableInfo = yes(ProcTableInfo)
	),

	table_info_extract(TableInfo, !:ModuleInfo, _, _,
		!:PredInfo, !:ProcInfo),

	% set the new values of the fields in proc_info and pred_info
	% and save in the module info
	proc_info_set_goal(Goal, !ProcInfo),
	proc_info_set_varset(VarSet, !ProcInfo),
	proc_info_set_vartypes(VarTypes, !ProcInfo),
	proc_info_set_call_table_tip(MaybeCallTableTip, !ProcInfo),
	proc_info_set_maybe_proc_table_info(MaybeProcTableInfo, !ProcInfo),

	% Some of the instmap_deltas generated in this module
	% are pretty dodgy (especially those for if-then-elses), so
	% recompute them here.

	RecomputeAtomic = no,
	recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo,
		!ModuleInfo),

	pred_info_procedures(!.PredInfo, ProcTable1),
	map__det_update(ProcTable1, ProcId, !.ProcInfo, ProcTable),
	pred_info_set_procedures(ProcTable, !PredInfo),

	%
	% The transformation doesn't pay attention to the purity
	% of compound goals, so recompute the purity here.
	%
	repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
	module_info_preds(!.ModuleInfo, PredTable1),
	map__det_update(PredTable1, PredId, !.PredInfo, PredTable),
	module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

% Example of transformation for model_det loopcheck:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
% 	<original code>.
%
% The transformed code would be :
%
% p(A, B) :-
%	T0 = <table pointer for p/2>,
%	impure table_lookup_insert_int(T0, A, T),
%	impure table_loop_setup(T, Status),
%	(
%		Status = loop_active,
%		error("detected infinite recursion in ...")
%	;
%		Status = loop_inactive,
%		% status has been changed to active by the setup predicate
%		<original code>,
%		impure table_loop_mark_as_inactive(T)
%	).
%
% Example of transformation for model_semi loopcheck:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
% 	<original code>.
%
% The transformed code would be :
%
% p(A, B) :-
%	T0 = <table pointer for p/2>,
%	impure table_lookup_insert_int(T0, A, T),
%	impure table_loop_setup(T, Status),
%	(
%		Status = loop_active,
%		error("detected infinite recursion in ...")
%	;
%		Status = loop_inactive,
%		% status has been changed to active by the setup predicate
%	   	(if
%			<original code>, with B replaced by C
%		then
%			B = C,
%			impure table_loop_mark_as_inactive(T)
%		else
%			impure table_loop_mark_as_inactive(T),
%			fail
%		)
%	).

:- pred create_new_loop_goal(determinism::in, hlds_goal::in,
	pred_id::in, proc_id::in,
	list(prog_var)::in, list(prog_var)::in, list(prog_var)::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::out, hlds_goal::out,
	list(table_trie_step)::out) is det.

create_new_loop_goal(Detism, OrigGoal, PredId, ProcId,
		HeadVars, InputVars, OutputVars, !VarTypes, !VarSet,
		!TableInfo, TableTipVar, Goal, Steps) :-
	% even if the original goal doesn't use all of the headvars,
	% the code generated by the tabling transformation does,
	% so we need to compute the nonlocals from the headvars rather
	% than getting it from the nonlocals field in the original goal
	set__list_to_set(HeadVars, OrigNonLocals),
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
	goal_info_get_context(OrigGoalInfo, Context),

	ModuleInfo = !.TableInfo ^ table_module_info,
	generate_simple_call_table_lookup_goal(loop_status_type,
		"table_loop_setup", InputVars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, StatusVar,
		LookUpGoal, Steps),

	generate_error_goal(!.TableInfo, Context, infinite_recursion_msg,
		!VarTypes, !VarSet, ActiveGoal),

	generate_call("table_loop_mark_as_inactive", [TableTipVar], det,
		yes(impure), [], ModuleInfo, Context, MarkInactiveGoal),

	determinism_to_code_model(Detism, CodeModel),
	set__list_to_set([TableTipVar | HeadVars], InactiveNonLocals),
	InactiveInstmapDelta = bind_vars(OutputVars),
	(
		CodeModel = model_det,
		InactiveGoalExpr = conj([OrigGoal, MarkInactiveGoal])
	;
		CodeModel = model_semi,

		create_renaming(OutputVars, !VarTypes, !VarSet,
			Unifies, NewVars, Renaming),
		rename_vars_in_goal(OrigGoal, Renaming, RenamedOrigGoal),

		ThenGoalExpr = conj(list__append(Unifies, [MarkInactiveGoal])),
		list__append([TableTipVar | OutputVars], NewVars, ThenVars),
		set__list_to_set(ThenVars, ThenNonLocals),
		goal_info_init_hide(ThenNonLocals, InactiveInstmapDelta,
			Detism, impure, Context, ThenGoalInfo),
		ThenGoal = ThenGoalExpr - ThenGoalInfo,

		append_fail(MarkInactiveGoal, ElseGoal),

		InactiveGoalExpr = if_then_else([], RenamedOrigGoal,
			ThenGoal, ElseGoal)
	;
		CodeModel = model_non,
		error("create_new_loop_goal: model_non")
	),
	goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
		Detism, impure, Context, InactiveGoalInfo),
	InactiveGoal = InactiveGoalExpr - InactiveGoalInfo,

	mercury_table_builtin_module(TB),
	SwitchArms = [
		case(cons(qualified(TB, "loop_active"), 0),
			ActiveGoal),
		case(cons(qualified(TB, "loop_inactive"), 0),
			InactiveGoal)
	],
	SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
	goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
		Detism, impure, Context, SwitchGoalInfo),
	SwitchGoal = SwitchExpr - SwitchGoalInfo,

	GoalExpr = conj([LookUpGoal, SwitchGoal]),
	goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism,
		impure, Context, GoalInfo),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

% Example of transformation for model_det memo:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
% 	<original code>.
%
% The transformed code would be :
%
% p(A, B) :-
%	T0 = <table pointer for p/2>,
%	impure table_lookup_insert_int(T0, A, T),
%	impure table_memo_setup(T, Status),
%	(
%		Status = memo_det_succeeded,
%		semipure table_memo_get_answer_block(T, AnswerBlock),
%		semipure table_restore_int_answer(AnswerBlock, 0, B)
%	;
%		Status = memo_det_active,
%		error("detected infinite recursion in ...")
%	;
%		Status = memo_det_inactive,
%		% status has been changed to active by the setup predicate
%		<original code>
%		impure table_memo_create_answer_block(T, 1, AnswerBlock),
%		impure table_save_int_answer(AnswerBlock, 0, B)
%	).
%
% Example of transformation for model_semi memo:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
% 	<original code>.
%
% The transformed code would be :
%
% p(A, B) :-
%	T0 = <table pointer for p/2>,
%	impure table_lookup_insert_int(T0, A, T),
%	impure table_memo_setup(T, Status),
%	(
%		Status = memo_semi_failed,
%		fail
%	;
%		Status = memo_semi_succeeded,
%		semipure table_memo_get_answer_block(T, AnswerBlock),
%		semipure table_restore_int_answer(AnswerBlock, 0, B)
%	;
%		Status = memo_semi_active,
%		error("detected infinite recursion in ...")
%	;
%		Status = memo_semi_inactive,
%		% status has been changed to active by the setup predicate
%	   	(if
%			<original code>, with B replaced by C
%		then
%			B = C,
%			impure table_memo_create_answer_block(T, 1,
%				AnswerBlock),
%			impure table_save_int_answer(AnswerBlock, 0, B)
%		else
%			impure table_memo_mark_as_failed(T),
%			fail
%		)
%	).
%
% If there are no output variables, then instead of creating an answer block
% and filling it in, we call table_memo_mark_as_succeeded.

:- pred create_new_memo_goal(determinism::in, hlds_goal::in,
	pred_id::in, proc_id::in,
	list(prog_var)::in, list(prog_var)::in, list(prog_var)::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::out, hlds_goal::out,
	list(table_trie_step)::out) is det.

create_new_memo_goal(Detism, OrigGoal, PredId, ProcId,
		HeadVars, InputVars, OutputVars, !VarTypes, !VarSet,
		!TableInfo, TableTipVar, Goal, Steps) :-
	% even if the original goal doesn't use all of the headvars,
	% the code generated by the tabling transformation does,
	% so we need to compute the nonlocals from the headvars rather
	% than getting it from the nonlocals field in the original goal
	set__list_to_set(HeadVars, OrigNonLocals),
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
	goal_info_get_context(OrigGoalInfo, Context),

	ModuleInfo = !.TableInfo ^ table_module_info,
	determinism_to_code_model(Detism, CodeModel),
	(
		CodeModel = model_det,
		StatusType = memo_det_status_type,
		SetupPred = "table_memo_det_setup"
	;
		CodeModel = model_semi,
		StatusType = memo_semi_status_type,
		SetupPred = "table_memo_semi_setup"
	;
		CodeModel = model_non,
		error("create_new_memo_goal: model_non")
	),
	generate_simple_call_table_lookup_goal(StatusType, SetupPred,
		InputVars, PredId, ProcId, Context, !VarTypes, !VarSet,
		!TableInfo, TableTipVar, StatusVar, LookUpGoal, Steps),

	generate_error_goal(!.TableInfo, Context, infinite_recursion_msg,
		!VarTypes, !VarSet, ActiveGoal),

	allocate_slot_numbers(OutputVars, 0, NumberedOutputVars),
	list__length(NumberedOutputVars, BlockSize),
	generate_memo_save_goals(NumberedOutputVars, TableTipVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnswerGoals),
	generate_memo_restore_goal(NumberedOutputVars, TableTipVar, ModuleInfo,
		Context, !VarTypes, !VarSet, RestoreAnswerGoal),
	SucceededGoal = RestoreAnswerGoal,

	set__list_to_set([TableTipVar | HeadVars], InactiveNonLocals),
	InactiveInstmapDelta = bind_vars(OutputVars),
	(
		CodeModel = model_det,
		InactiveGoalExpr = conj([OrigGoal | SaveAnswerGoals]),
		goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
			Detism, impure, Context, InactiveGoalInfo),
		InactiveGoal = InactiveGoalExpr - InactiveGoalInfo,

		mercury_table_builtin_module(TB),
		SwitchArms = [
			case(cons(qualified(TB, "memo_det_active"), 0),
				ActiveGoal),
			case(cons(qualified(TB, "memo_det_inactive"), 0),
				InactiveGoal),
			case(cons(qualified(TB, "memo_det_succeeded"), 0),
				SucceededGoal)
		]
	;
		CodeModel = model_semi,
		create_renaming(OutputVars, !VarTypes, !VarSet,
			Unifies, NewVars, Renaming),
		rename_vars_in_goal(OrigGoal, Renaming, RenamedOrigGoal),

		ThenGoalExpr = conj(list__append(Unifies, SaveAnswerGoals)),
		list__append([TableTipVar | OutputVars], NewVars, ThenVars),
		set__list_to_set(ThenVars, ThenNonLocals),
		goal_info_init_hide(ThenNonLocals, InactiveInstmapDelta,
			det, impure, Context, ThenGoalInfo),
		ThenGoal = ThenGoalExpr - ThenGoalInfo,

		generate_call("table_memo_mark_as_failed", [TableTipVar],
			failure, yes(impure), [], ModuleInfo, Context,
			ElseGoal),
		% XXX make table_memo_mark_as_failed have detism failure
		% append_fail(ElseGoal0, ElseGoal),

		InactiveGoalExpr = if_then_else([], RenamedOrigGoal,
			ThenGoal, ElseGoal),
		goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
			Detism, impure, Context, InactiveGoalInfo),
		InactiveGoal = InactiveGoalExpr - InactiveGoalInfo,
		fail_goal(FailedGoal),

		mercury_table_builtin_module(TB),
		SwitchArms = [
			case(cons(qualified(TB, "memo_semi_active"), 0),
				ActiveGoal),
			case(cons(qualified(TB, "memo_semi_inactive"), 0),
				InactiveGoal),
			case(cons(qualified(TB, "memo_semi_succeeded"), 0),
				SucceededGoal),
			case(cons(qualified(TB, "memo_semi_failed"), 0),
				FailedGoal)
		]
	;
		CodeModel = model_non,
		error("create_new_memo_goal: model_non")
	),

	SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
	goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
		Detism, impure, Context, SwitchGoalInfo),
	SwitchGoal = SwitchExpr - SwitchGoalInfo,

	GoalExpr = conj([LookUpGoal, SwitchGoal]),
	goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism,
		impure, Context, GoalInfo),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

% Example of transformation for tabling I/O, for I/O primitives (i.e.
% predicates defined by pragma c_code that take an input/output pair of
% io_state arguments) that have the tabled_for_io feature:
%
% :- pred p(int, string, io, io).
% :- mode p(in, out, di, uo) is det.
%
% p(A, B, S0, S) :-
%	<original code>
%
% The transformed code would be :
%
% p(A, B, S0, S) :-
%	(if
%			% Get the global I/O table, the global I/O
%			% counter, and the starting point for tabling
%			% I/O actions, if we are in the tabled range.
%		table_io_in_range(T0, Counter, Start)
%	then
%			% Look up the input arguments.
%		impure table_lookup_insert_start_int(T0, Counter,
%			Start, T),
%		(if
%			semipure table_io_has_occurred(T)
%		then
%			impure table_restore_string_answer(T, 0, B)
%			table_io_copy_io_state(S0, S)
%		else
%			<original code>
%				% Save the answers in the table.
%			impure table_io_create_answer_block(T, 1, AnswerBlock),
%			impure table_save_string_answer(AnswerBlock, 0, B)
%		)
%	else
%		<original code>
%	).
%
% Note that copying the I/O state works only because variables of type
% io__state don't actually contain any information; the information is actually
% stored in global variables. However, if this ever changes, the transformation
% can be fixed simply by changing the value of --trace-table-io-states to yes,
% which will cause such values to be tabled along with the other output
% arguments.
%
% For I/O primitives that do not have tabled_for_io, we should require that
% they do not do any I/O in their own code, meaning that all their I/O is
% inside any Mercury code they call. We can then leave such primitives
% untransformed; the I/O primitives called from the inner Mercury engine
% will do the right thing. For now, this requirement is not enforced,
% which means that enabling I/O tabling (with --trace-table-io) does not
% guarantee that *all* I/O actions are tabled. This can cause inconsistent
% behavior after retry commands in mdb. This is the reason why retry across
% I/O is experimental for now.
%
% The reason why we require I/O primitives to be marked manually by a
% programmer with the tabled_for_io feature is to get the programmer to make
% sure that the primitive meets the requirement. Unfortunately, this cannot be
% automated, since automation would require analysis of arbitrary C code.
%
% The transformation for tabling I/O for declarative debugging is a variant
% of the usual transformation of I/O primitives. In this variant, the answer
% block contains not only the answers, but also a pointer to the primitive's
% proc layout structure and the values of the input aruments. The code we
% generate will fill in the slots containing this extra information before
% it executes the original goal.

:- pred table_gen__create_new_io_goal(hlds_goal::in, table_io_is_decl::in,
	table_io_is_unitize::in, bool::in, list(prog_var)::in,
	list(prog_var)::in, list(prog_var)::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	hlds_goal::out, maybe(proc_table_info)::out) is det.

table_gen__create_new_io_goal(OrigGoal, TableDecl, Unitize, TableIoStates,
		HeadVars, InputVars, OutputVars, !VarTypes, !VarSet,
		!TableInfo, Goal, MaybeProcTableInfo) :-
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_nonlocals(OrigGoalInfo, OrigNonLocals),
	goal_info_get_context(OrigGoalInfo, Context),
	ModuleInfo = !.TableInfo ^ table_module_info,
	(
		TableIoStates = yes,
		IoStateAssignToVars = [],
		IoStateAssignFromVars = [],
		SavedOutputVars = OutputVars,
		SavedHeadVars = HeadVars
	;
		TableIoStates = no,
		list__filter(table_gen__var_is_io_state(!.VarTypes),
			OutputVars, IoStateAssignToVars, SavedOutputVars),
		list__filter(table_gen__var_is_io_state(!.VarTypes),
			InputVars, IoStateAssignFromVars, _SavedInputVars),
		list__filter(table_gen__var_is_io_state(!.VarTypes),
			HeadVars, _, SavedHeadVars)
	),
	generate_new_table_var("TableVar", trie_node_type, !VarTypes, !VarSet,
		TableVar),
	generate_new_table_var("CounterVar", int_type, !VarTypes, !VarSet,
		CounterVar),
	generate_new_table_var("StartVar", int_type, !VarTypes, !VarSet,
		StartVar),
	generate_call("table_io_in_range", [TableVar, CounterVar, StartVar],
		semidet, yes(impure), [TableVar - ground(shared, none),
		CounterVar - ground(shared, none),
		StartVar - ground(shared, none)],
		ModuleInfo, Context, InRangeGoal),
	generate_new_table_var("TipVar", trie_node_type, !VarTypes, !VarSet,
		TipVar),
	generate_call("table_lookup_insert_start_int",
		[TableVar, StartVar, CounterVar, TipVar],
		det, yes(impure), [TipVar - ground(unique, none)],
		ModuleInfo, Context, LookupGoal),
	generate_call("table_io_has_occurred", [TipVar], semidet,
		yes(semipure), [], ModuleInfo, Context, OccurredGoal),
	(
		TableDecl = table_io_decl,
		PredId = !.TableInfo ^ table_cur_pred_id,
		ProcId = !.TableInfo ^ table_cur_proc_id,
		RttiProcLabel = rtti__make_rtti_proc_label(ModuleInfo,
			PredId, ProcId),
		TableIoDeclConsId = table_io_decl(RttiProcLabel),
		make_const_construction(TableIoDeclConsId, c_pointer_type,
			yes("TableIoDeclPtr"), TableIoDeclGoal,
			TableIoDeclPtrVar, !VarTypes, !VarSet),
		allocate_slot_numbers(SavedHeadVars, 1, NumberedSavedHeadVars),
		NumberedSaveVars = [TableIoDeclPtrVar - 0 |
			NumberedSavedHeadVars],
		list__filter(key_belong_to_list(SavedOutputVars),
			NumberedSaveVars, NumberedSavedOutputVars),
		NumberedRestoreVars = NumberedSavedOutputVars,

		ProcInfo0 = !.TableInfo ^ table_cur_proc_info,
		continuation_info__generate_table_arg_type_info(ProcInfo0,
			NumberedSavedHeadVars, TableArgTypeInfo),
		ProcTableInfo = table_io_decl_info(TableArgTypeInfo),
		MaybeProcTableInfo = yes(ProcTableInfo)
	;
		TableDecl = table_io_proc,
		true_goal(TableIoDeclGoal),
		allocate_slot_numbers(SavedOutputVars, 0,
			NumberedSavedOutputVars),
		NumberedRestoreVars = NumberedSavedOutputVars,
		NumberedSaveVars = NumberedSavedOutputVars,
		MaybeProcTableInfo = no
	),
	list__length(NumberedSaveVars, BlockSize),
	generate_memo_restore_goal(NumberedRestoreVars, TipVar, ModuleInfo,
		Context, !VarTypes, !VarSet, RestoreAnswerGoal0),
	(
		TableIoStates = yes,
		RestoreAnswerGoal = RestoreAnswerGoal0
	;
		TableIoStates = no,
		(
			IoStateAssignFromVars = [IoStateAssignFromVarPrime],
			IoStateAssignToVars = [IoStateAssignToVarPrime]
		->
			IoStateAssignFromVar = IoStateAssignFromVarPrime,
			IoStateAssignToVar = IoStateAssignToVarPrime
		;
			% The call to proc_info_has_io_state_pair in
			% table_gen__process_procs should ensure that we
			% never get here.
			error("create_new_io_goal: one in / one out violation")
		),
		generate_call("table_io_copy_io_state",
			[IoStateAssignFromVar, IoStateAssignToVar], det, no,
			[IoStateAssignFromVar - ground(clobbered, none),
			IoStateAssignToVar - ground(unique, none)],
			ModuleInfo, Context, IoStateAssignGoal),
		RestoreAnswerGoalExpr = conj([RestoreAnswerGoal0,
			IoStateAssignGoal]),
		create_instmap_delta([RestoreAnswerGoal0, IoStateAssignGoal],
			RestoreAnswerInstMapDelta0),
		RestoreAnswerGoal0 = _ - RestoreAnswerGoal0Info,
		goal_info_get_nonlocals(RestoreAnswerGoal0Info,
			RestoreAnswer0NonLocals),
		set__insert_list(RestoreAnswer0NonLocals,
			[IoStateAssignFromVar, IoStateAssignToVar],
			RestoreAnswerNonLocals),
		instmap_delta_restrict(RestoreAnswerInstMapDelta0,
			RestoreAnswerNonLocals, RestoreAnswerInstMapDelta),
		goal_info_init_hide(RestoreAnswerNonLocals,
			RestoreAnswerInstMapDelta, det, semipure, Context,
			RestoreAnswerGoalInfo),
		RestoreAnswerGoal = RestoreAnswerGoalExpr
			- RestoreAnswerGoalInfo
	),
	generate_memo_save_goals(NumberedSaveVars, TipVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnswerGoals),
	(
		Unitize = table_io_alone,
		CallSaveAnswerGoalList = [OrigGoal, TableIoDeclGoal
			| SaveAnswerGoals]
	;
		Unitize = table_io_unitize,
		generate_new_table_var("SavedTraceEnabled", int_type,
			!VarTypes, !VarSet, SavedTraceEnabledVar),
		generate_call("table_io_left_bracket_unitized_goal",
			[SavedTraceEnabledVar], det, yes(impure),
			[SavedTraceEnabledVar - ground(unique, none)],
			ModuleInfo, Context, LeftBracketGoal),
		generate_call("table_io_right_bracket_unitized_goal",
			[SavedTraceEnabledVar], det, yes(impure), [],
			ModuleInfo, Context, RightBracketGoal),
		CallSaveAnswerGoalList = [LeftBracketGoal, OrigGoal,
			RightBracketGoal, TableIoDeclGoal | SaveAnswerGoals]
	),
	CallSaveAnswerGoalExpr = conj(CallSaveAnswerGoalList),
	create_instmap_delta(CallSaveAnswerGoalList,
		CallSaveAnswerInstMapDelta0),
	set__insert(OrigNonLocals, TipVar, CallSaveAnswerNonLocals),
	instmap_delta_restrict(CallSaveAnswerInstMapDelta0,
		CallSaveAnswerNonLocals, CallSaveAnswerInstMapDelta),
	goal_info_init_hide(CallSaveAnswerNonLocals,
		CallSaveAnswerInstMapDelta, det, impure, Context,
		CallSaveAnswerGoalInfo0),
	goal_info_add_feature(CallSaveAnswerGoalInfo0, hide_debug_event,
		CallSaveAnswerGoalInfo),
	CallSaveAnswerGoal = CallSaveAnswerGoalExpr - CallSaveAnswerGoalInfo,

	GenIfNecGoalExpr = if_then_else([], OccurredGoal,
		RestoreAnswerGoal, CallSaveAnswerGoal),
	create_instmap_delta([OccurredGoal, RestoreAnswerGoal,
		CallSaveAnswerGoal], GenIfNecInstMapDelta0),
	set__insert(OrigNonLocals, TipVar, GenIfNecNonLocals),
	instmap_delta_restrict(GenIfNecInstMapDelta0, GenIfNecNonLocals,
		GenIfNecInstMapDelta),
	goal_info_init_hide(GenIfNecNonLocals, GenIfNecInstMapDelta, det,
		impure, Context, GenIfNecGoalInfo),
	GenIfNecGoal = GenIfNecGoalExpr - GenIfNecGoalInfo,

	CheckAndGenAnswerGoalExpr = conj([LookupGoal, GenIfNecGoal]),
	create_instmap_delta([LookupGoal, GenIfNecGoal],
		CheckAndGenAnswerInstMapDelta0),
	set__insert_list(OrigNonLocals, [TableVar, CounterVar, StartVar],
		CheckAndGenAnswerNonLocals),
	instmap_delta_restrict(CheckAndGenAnswerInstMapDelta0,
		CheckAndGenAnswerNonLocals, CheckAndGenAnswerInstMapDelta),
	goal_info_init_hide(CheckAndGenAnswerNonLocals,
		CheckAndGenAnswerInstMapDelta, det, impure, Context,
		CheckAndGenAnswerGoalInfo),
	CheckAndGenAnswerGoal = CheckAndGenAnswerGoalExpr
		- CheckAndGenAnswerGoalInfo,

	BodyGoalExpr = if_then_else([], InRangeGoal, CheckAndGenAnswerGoal,
		OrigGoal),
	create_instmap_delta([InRangeGoal, CheckAndGenAnswerGoal, OrigGoal],
		BodyInstMapDelta0),
	instmap_delta_restrict(BodyInstMapDelta0, OrigNonLocals,
		BodyInstMapDelta),
	goal_info_init_hide(OrigNonLocals, BodyInstMapDelta, det, impure,
		Context, BodyGoalInfo),
	Goal = BodyGoalExpr - BodyGoalInfo.

%-----------------------------------------------------------------------------%

% Example of transformation for nondet minimal_model :
%
% :- pred p(int, int).
% :- mode p(in, out) is nondet.
%
% p(A, B) :-
%	<original code>.
%
% The transformed code would be :
%
% p(A, B) :-
% 		% Get a handle on the table.
% 	CT0 = <table pointer for p/2>,
%
%		% Look up the input arguments, and set up the table.
%	impure table_lookup_insert_int(CT0, A, CT1),
%	impure table_mm_setup(CT1, SubGoal, Status),
%	(
%		Status = complete,
%			% Return all the answers from the complete table.
%		semipure table_mm_return_all_nondet(SubGoal, Answer),
%		semipure table_restore_int_answer(Answer, 0, B)
%	;
%		Status = active,
%			% Suspend the current computational branch.
%			% Resume when the generator has computed some answers.
%		impure table_mm_suspend_consumer(SubGoal, Answer),
%		semipure table_restore_int_answer(Answer, 0, B)
%	;
%		Status = inactive,
%	   	(
%			<original code>,
%
%				% Check for duplicate answers.
%			semipure table_mm_get_answer_table(SubGoal, AT0),
%			impure table_lookup_insert_int(AT0, B, AT1),
%				% Fail if the answer is already in the table;
%				% otherwise, put it into the table.
%			impure table_mm_answer_is_not_duplicate(AT1),
%
%				% Save the new answer in the table.
%			impure table_mm_create_answer_block(Subgoal, 1, Block),
%			impure table_save_int_answer(Block, 0, B)
%		;
%				% Mark this subgoal as completely evaluated,
%				% modulo any dependencies on other subgoals.
%			impure table_mm_completion(SubGoal),
%			fail
%		)
%	).

:- pred table_gen__create_new_mm_goal(determinism::in,
	hlds_goal::in, pred_id::in, proc_id::in, list(prog_var)::in,
	list(prog_var)::in, list(prog_var)::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, hlds_goal::out, list(table_trie_step)::out) is det.

table_gen__create_new_mm_goal(Detism, OrigGoal, PredId, ProcId,
		HeadVars, InputVars, OutputVars, !VarTypes, !VarSet,
		!TableInfo, TableTipVar, Goal, Steps) :-
	% even if the original goal doesn't use all of the headvars,
	% the code generated by the tabling transformation does,
	% so we need to compute the nonlocals from the headvars rather
	% than getting it from the nonlocals field in the original goal
	set__list_to_set(HeadVars, OrigNonLocals),
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
	goal_info_get_context(OrigGoalInfo, Context),

	ModuleInfo = !.TableInfo ^ table_module_info,
	allocate_slot_numbers(OutputVars, 0, NumberedOutputVars),
	list__length(NumberedOutputVars, BlockSize),
	generate_non_call_table_lookup_goal(InputVars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, SubgoalVar,
		StatusVar, LookUpGoal, Steps),
	generate_mm_save_goals(NumberedOutputVars, SubgoalVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnswerGoals),
	generate_mm_restore_goal(Detism, NumberedOutputVars, SubgoalVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreAllAnswerGoal),

	generate_mm_suspend_goal(NumberedOutputVars, SubgoalVar,
		ModuleInfo, Context, !VarTypes, !VarSet, SuspendGoal),

	MainExpr = conj([OrigGoal | SaveAnswerGoals]),
	set__insert(OrigNonLocals, SubgoalVar, MainNonLocals),
	create_instmap_delta([OrigGoal | SaveAnswerGoals], MainIMD0),
	instmap_delta_restrict(MainIMD0, MainNonLocals, MainIMD),
	goal_info_init_hide(MainNonLocals, MainIMD, nondet, impure, Context,
		MainGoalInfo),
	MainGoal = MainExpr - MainGoalInfo,

	generate_call("table_mm_completion", [SubgoalVar], det,
		yes(impure), [], ModuleInfo, Context, ResumeGoal0),
	append_fail(ResumeGoal0, ResumeGoal),
	InactiveExpr = disj([MainGoal, ResumeGoal]),
	InactiveGoal = InactiveExpr - MainGoalInfo,

	mercury_table_builtin_module(TB),
	SwitchArms = [
		case(cons(qualified(TB, "mm_inactive"), 0),
			InactiveGoal),
		case(cons(qualified(TB, "mm_complete"), 0),
			RestoreAllAnswerGoal),
		case(cons(qualified(TB, "mm_active"), 0),
			SuspendGoal)
	],
	SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
	goal_info_add_feature(MainGoalInfo, hide_debug_event, SwitchGoalInfo),
	SwitchGoal = SwitchExpr - SwitchGoalInfo,

	GoalExpr = conj([LookUpGoal, SwitchGoal]),
	goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, nondet, impure,
		Context, GoalInfo),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

:- pred create_renaming(list(prog_var)::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, list(hlds_goal)::out,
	list(prog_var)::out, map(prog_var, prog_var)::out) is det.

create_renaming(OrigVars, !VarTypes, !VarSet, Unifies, NewVars, Renaming) :-
	create_renaming_2(OrigVars, !VarTypes, !VarSet, [], RevUnifies,
		[], RevNewVars, map__init, Renaming),
	list__reverse(RevNewVars, NewVars),
	list__reverse(RevUnifies, Unifies).

:- pred create_renaming_2(list(prog_var)::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out,
	list(hlds_goal)::in, list(hlds_goal)::out,
	list(prog_var)::in, list(prog_var)::out,
	map(prog_var, prog_var)::in, map(prog_var, prog_var)::out) is det.

create_renaming_2([], !VarTypes, !VarSet, !RevUnifies, !RevNewVars, !Renaming).
create_renaming_2([OrigVar | OrigVars], !VarTypes, !VarSet, !RevUnifies,
		!RevNewVars, !Renaming) :-
	varset__new_var(!.VarSet, NewVar, !:VarSet),
	map__lookup(!.VarTypes, OrigVar, Type),
	map__det_insert(!.VarTypes, NewVar, Type, !:VarTypes),
	Ground = ground(shared, none),
	Mode = ((Ground -> Ground) - (free -> Ground)),
	UnifyInfo = assign(OrigVar, NewVar),
	UnifyContext = unify_context(explicit, []),
	GoalExpr = unify(OrigVar, var(NewVar), Mode, UnifyInfo, UnifyContext),
	set__list_to_set([OrigVar, NewVar], NonLocals),
	goal_info_init_hide(NonLocals, bind_vars([OrigVar]), det, pure,
		term__context_init, GoalInfo),
	Goal = GoalExpr - GoalInfo,
	!:RevUnifies = [Goal | !.RevUnifies],
	map__det_insert(!.Renaming, OrigVar, NewVar, !:Renaming),
	!:RevNewVars = [NewVar | !.RevNewVars],
	create_renaming_2(OrigVars, !VarTypes, !VarSet, !RevUnifies,
		!RevNewVars, !Renaming).

%-----------------------------------------------------------------------------%

:- pred table_gen__var_is_io_state(vartypes::in, prog_var::in) is semidet.

table_gen__var_is_io_state(VarTypes, Var) :-
	map__lookup(VarTypes, Var, VarType),
	type_util__type_is_io_state(VarType).

%-----------------------------------------------------------------------------%

:- pred generate_gen_proc_table_info(table_info::in, list(table_trie_step)::in,
	list(prog_var)::in, list(prog_var)::in, proc_table_info::out) is det.

generate_gen_proc_table_info(TableInfo, Steps, InputVars, OutputVars,
		ProcTableInfo) :-
	ProcInfo = TableInfo ^ table_cur_proc_info,
	list__append(InputVars, OutputVars, InOutHeadVars),
	allocate_slot_numbers(InOutHeadVars, 1, NumberedInOutHeadVars),
	continuation_info__generate_table_arg_type_info(ProcInfo,
		NumberedInOutHeadVars, TableArgTypeInfo),
	NumInputs = list__length(InputVars),
	NumOutputs = list__length(OutputVars),
	ProcTableInfo = table_gen_info(NumInputs, NumOutputs, Steps,
		TableArgTypeInfo).

%-----------------------------------------------------------------------------%

:- pred generate_table_lookup_goals(list(prog_var)::in, string::in, int::in,
	term__context::in, prog_var::in, prog_var::out,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, list(hlds_goal)::out,
	list(table_trie_step)::out) is det.

generate_table_lookup_goals([], _, _, _, !TableVar, !VarTypes, !VarSet,
		!TableInfo, [], []).
generate_table_lookup_goals([Var | Vars], Prefix, VarSeqNum, Context,
		!TableVar, !VarTypes, !VarSet, !TableInfo,
		[Goal | RestGoals], [Step | Steps]) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_lookup_call_for_type(TypeCat, VarType, Var, Prefix, VarSeqNum,
		Context, !VarTypes, !VarSet, !TableInfo, !TableVar,
		Goal, Step),
	generate_table_lookup_goals(Vars, Prefix, VarSeqNum + 1, Context,
		!TableVar, !VarTypes, !VarSet, !TableInfo, RestGoals, Steps).

%-----------------------------------------------------------------------------%

:- type loopcheck_or_memo
	--->	loopcheck
	;	memo.

:- pred generate_get_table_goal(pred_id::in, proc_id::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	prog_var::out, hlds_goal::out) is det.

generate_get_table_goal(PredId, ProcId, !VarTypes, !VarSet, PredTableVar,
		Goal) :-
	generate_new_table_var("PredTable", trie_node_type,
		!VarTypes, !VarSet, PredTableVar),
	ConsId = tabling_pointer_const(PredId, ProcId),
	make_const_construction(PredTableVar, ConsId, GoalExpr - GoalInfo0),
	goal_info_add_feature(GoalInfo0, impure, GoalInfo),
	Goal = GoalExpr - GoalInfo.

:- pred generate_call_table_lookup_goals(list(prog_var)::in,
	pred_id::in, proc_id::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::out,
	list(hlds_goal)::out, list(table_trie_step)::out) is det.

generate_call_table_lookup_goals(Vars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, Goals, Steps) :-
	generate_get_table_goal(PredId, ProcId, !VarTypes, !VarSet,
		PredTableVar, GetTableGoal),
	generate_table_lookup_goals(Vars, "CallTableNode", 1, Context,
		PredTableVar, TableTipVar, !VarTypes, !VarSet,
		!TableInfo, LookupGoals, Steps),
	Goals = [GetTableGoal | LookupGoals].

:- pred generate_simple_call_table_lookup_goal((type)::in, string::in,
	list(prog_var)::in, pred_id::in, proc_id::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::out, prog_var::out,
	hlds_goal::out, list(table_trie_step)::out) is det.

generate_simple_call_table_lookup_goal(StatusType, SetupPred, Vars,
		PredId, ProcId, Context, !VarTypes, !VarSet, !TableInfo,
		TableTipVar, StatusVar, Goal, Steps) :-
	generate_call_table_lookup_goals(Vars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, LookupGoals,
		Steps),
	generate_new_table_var("Status", StatusType, !VarTypes, !VarSet,
		StatusVar),
	ModuleInfo = !.TableInfo ^ table_module_info,
	generate_call(SetupPred, [TableTipVar, StatusVar],
		det, yes(impure), ground_vars([StatusVar]),
		ModuleInfo, Context, SetupGoal0),
	attach_call_table_tip(SetupGoal0, SetupGoal),
	GoalExpr = conj(list__append(LookupGoals, [SetupGoal])),
	set__list_to_set([StatusVar, TableTipVar | Vars], NonLocals),
	goal_info_init_hide(NonLocals, bind_vars([TableTipVar, StatusVar]),
		det, impure, Context, GoalInfo),
	Goal = GoalExpr - GoalInfo.

:- pred generate_non_call_table_lookup_goal(list(prog_var)::in,
	pred_id::in, proc_id::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::out, prog_var::out,
	prog_var::out, hlds_goal::out, list(table_trie_step)::out) is det.

generate_non_call_table_lookup_goal(Vars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo,
		TableTipVar, SubgoalVar, StatusVar, Goal, Steps) :-
	generate_call_table_lookup_goals(Vars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, LookupGoals,
		Steps),
	ModuleInfo = !.TableInfo ^ table_module_info,
	generate_new_table_var("Subgoal", subgoal_type, !VarTypes, !VarSet,
		SubgoalVar),
	generate_new_table_var("Status", mm_status_type,
		!VarTypes, !VarSet, StatusVar),
	generate_call("table_mm_setup", [TableTipVar, SubgoalVar, StatusVar],
		det, yes(impure), ground_vars([SubgoalVar, StatusVar]),
		ModuleInfo, Context, SetupGoal0),
	attach_call_table_tip(SetupGoal0, SetupGoal),
	GoalExpr = conj(list__append(LookupGoals, [SetupGoal])),
	set__list_to_set([StatusVar, TableTipVar, SubgoalVar | Vars],
		NonLocals),
	goal_info_init_hide(NonLocals,
		bind_vars([SubgoalVar, TableTipVar, StatusVar]), det,
		impure, Context, GoalInfo),
	Goal = GoalExpr - GoalInfo.

:- pred attach_call_table_tip(hlds_goal::in, hlds_goal::out) is det.

attach_call_table_tip(GoalExpr - GoalInfo0, GoalExpr - GoalInfo) :-
	goal_info_get_features(GoalInfo0, Features0),
	set__insert(Features0, call_table_gen, Features),
	goal_info_set_features(GoalInfo0, Features, GoalInfo).

:- pred gen_lookup_call_for_type(type_category::in, (type)::in,
	prog_var::in, string::in, int::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::in, prog_var::out,
	hlds_goal::out, table_trie_step::out) is det.

gen_lookup_call_for_type(TypeCat, Type, ArgVar, Prefix, VarSeqNum, Context,
		!VarTypes, !VarSet, !TableInfo, TableVar, NextTableVar,
		Goal, Step) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	VarName = Prefix ++ int_to_string(VarSeqNum),
	generate_new_table_var(VarName, trie_node_type, !VarTypes, !VarSet,
		NextTableVar),
	BindNextTableVar = ground_vars([NextTableVar]),
	( TypeCat = enum_type ->
		( type_to_ctor_and_args(Type, TypeCtor, _) ->
			module_info_types(ModuleInfo, TypeDefnTable),
			map__lookup(TypeDefnTable, TypeCtor, TypeDefn),
			hlds_data__get_type_defn_body(TypeDefn, TypeBody),
			(
				Ctors = TypeBody ^ du_type_ctors,
				TypeBody ^ du_type_is_enum = yes,
				TypeBody ^ du_type_usereq = no
			->
				list__length(Ctors, EnumRange)
			;
				error("gen_lookup_call_for_type: " ++
					"enum type is not du_type?")
			),
			gen_int_construction("RangeVar", EnumRange, !VarTypes,
				!VarSet, RangeVar, RangeUnifyGoal),
			generate_call("table_lookup_insert_enum",
				[TableVar, RangeVar, ArgVar, NextTableVar],
				det, yes(impure), BindNextTableVar,
				ModuleInfo, Context, LookupGoal),
			set__list_to_set([TableVar, ArgVar], NonLocals),
			goal_info_init_hide(NonLocals,
				bind_vars([NextTableVar]), det, impure,
				Context, GoalInfo),
			Goal = conj([RangeUnifyGoal, LookupGoal]) - GoalInfo,
			Step = table_trie_step_enum(EnumRange)
		;
			error("gen_lookup_call_for_type: unexpected enum type")
		)
	;
		lookup_tabling_category(TypeCat, MaybeCatStringStep),
		(
			MaybeCatStringStep = no,
			( type_util__vars(Type, []) ->
				LookupPredName = "table_lookup_insert_user",
				Step = table_trie_step_user(Type)
			;
				LookupPredName = "table_lookup_insert_poly",
				Step = table_trie_step_poly
			),
			make_type_info_var(Type, Context, !VarTypes, !VarSet,
				!TableInfo, TypeInfoVar, ExtraGoals),
			generate_call(LookupPredName,
				[TypeInfoVar, TableVar, ArgVar, NextTableVar],
				det, yes(impure), BindNextTableVar,
				ModuleInfo, Context, CallGoal),
			list__append(ExtraGoals, [CallGoal], ConjList),
			CallGoal = _ - GoalInfo,
			conj_list_to_goal(ConjList, GoalInfo, Goal)
		;
			MaybeCatStringStep = yes(CatString - Step),
			string__append("table_lookup_insert_", CatString,
				LookupPredName),
			generate_call(LookupPredName,
				[TableVar, ArgVar, NextTableVar],
				det, yes(impure), BindNextTableVar, ModuleInfo,
				Context, Goal)
		)
	).

%-----------------------------------------------------------------------------%

:- pred generate_memo_save_goals(assoc_list(prog_var, int)::in,
	prog_var::in, int::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, list(hlds_goal)::out) is det.

generate_memo_save_goals(NumberedSaveVars, TableVar, BlockSize, Context,
		!VarTypes, !VarSet, !TableInfo, Goals) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	( BlockSize > 0 ->
		gen_int_construction("BlockSize", BlockSize, !VarTypes,
			!VarSet, BlockSizeVar, BlockSizeVarUnifyGoal),
		generate_new_table_var("AnswerBlock", answer_block_type,
			!VarTypes, !VarSet, AnswerBlockVar),
		generate_call("table_memo_create_answer_block",
			[TableVar, BlockSizeVar, AnswerBlockVar], det,
			yes(impure), ground_vars([AnswerBlockVar]),
			ModuleInfo, Context, CreateAnswerBlockGoal),
		generate_save_goals(NumberedSaveVars, AnswerBlockVar, Context,
			!VarTypes, !VarSet, !TableInfo, SaveGoals),
		Goals = [BlockSizeVarUnifyGoal, CreateAnswerBlockGoal |
			SaveGoals]
	;
		generate_call("table_memo_mark_as_succeeded", [TableVar],
			det, yes(impure), [], ModuleInfo, Context, Goal),
		Goals = [Goal]
	).

:- pred generate_mm_save_goals(assoc_list(prog_var, int)::in,
	prog_var::in, int::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, list(hlds_goal)::out) is det.

generate_mm_save_goals(NumberedOutputVars, TableVar, BlockSize, Context,
		!VarTypes, !VarSet, !TableInfo, Goals) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	generate_new_table_var("AnswerTableVar", trie_node_type,
		!VarTypes, !VarSet, AnswerTableVar),
	generate_call("table_mm_get_answer_table", [TableVar, AnswerTableVar],
		det, yes(semipure), ground_vars([AnswerTableVar]),
		ModuleInfo, Context, GetAnswerTableGoal),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	generate_table_lookup_goals(OutputVars, "AnswerTableNode", 1, Context,
		AnswerTableVar, AnswerTableTipVar, !VarTypes, !VarSet,
		!TableInfo, LookupAnswerGoals, _),
	generate_call("table_mm_answer_is_not_duplicate",
		[AnswerTableTipVar], semidet, yes(impure), [], ModuleInfo,
		Context, DuplicateCheckGoal),
	gen_int_construction("BlockSize", BlockSize, !VarTypes, !VarSet,
		BlockSizeVar, BlockSizeVarUnifyGoal),
	generate_new_table_var("AnswerBlock", answer_block_type,
		!VarTypes, !VarSet, AnswerBlockVar),
	generate_call("table_mm_create_answer_block",
		[TableVar, BlockSizeVar, AnswerBlockVar], det, yes(impure),
		ground_vars([AnswerBlockVar]), ModuleInfo, Context,
		NewAnswerBlockGoal),
	generate_save_goals(NumberedOutputVars, AnswerBlockVar, Context,
		!VarTypes, !VarSet, !TableInfo, SaveGoals),
	list__append([GetAnswerTableGoal | LookupAnswerGoals],
		[DuplicateCheckGoal, BlockSizeVarUnifyGoal, NewAnswerBlockGoal
		| SaveGoals], Goals).

:- pred generate_save_goals(assoc_list(prog_var, int)::in, prog_var::in,
	term__context::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	list(hlds_goal)::out) is det.

generate_save_goals([], _, _, !VarTypes, !VarSet, !TableInfo, []).
generate_save_goals([NumberedVar | NumberedRest], TableVar, Context,
		!VarTypes, !VarSet, !TableInfo, Goals) :-
	NumberedVar = Var - Offset,
	gen_int_construction("OffsetVar", Offset, !VarTypes,
		!VarSet, OffsetVar, OffsetUnifyGoal),
	ModuleInfo = !.TableInfo ^ table_module_info,
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_save_call_for_type(TypeCat, VarType, TableVar, Var, OffsetVar,
		Context, !VarTypes, !VarSet, !TableInfo, CallGoal),
	generate_save_goals(NumberedRest, TableVar, Context,
		!VarTypes, !VarSet, !TableInfo, RestGoals),
	Goals =	[OffsetUnifyGoal, CallGoal | RestGoals].

:- pred gen_save_call_for_type(type_category::in, (type)::in,
	prog_var::in, prog_var::in, prog_var::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, hlds_goal::out) is det.

gen_save_call_for_type(TypeCat, Type, TableVar, Var, OffsetVar, Context,
		!VarTypes, !VarSet, !TableInfo, Goal) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	( type_util__type_is_io_state(Type) ->
		LookupPredName = "table_save_io_state_answer",
		generate_call(LookupPredName, [TableVar, OffsetVar, Var],
			det, yes(impure), [], ModuleInfo, Context, Goal)
	; builtin_type(TypeCat) = no ->
		make_type_info_var(Type, Context, !VarTypes, !VarSet,
			!TableInfo, TypeInfoVar, ExtraGoals),

		generate_call("table_save_any_answer",
			[TypeInfoVar, TableVar, OffsetVar, Var],
			det, yes(impure), [], ModuleInfo, Context, CallGoal),

		list__append(ExtraGoals, [CallGoal], ConjList),
		CallGoal = _ - GoalInfo,
		conj_list_to_goal(ConjList, GoalInfo, Goal)
	;
		type_save_category(TypeCat, CatString),
		string__append_list(["table_save_", CatString, "_answer"],
			LookupPredName),
		generate_call(LookupPredName, [TableVar, OffsetVar, Var],
			det, yes(impure), [], ModuleInfo, Context, Goal)
	).

%-----------------------------------------------------------------------------%

:- pred generate_memo_restore_goal(assoc_list(prog_var, int)::in,
	prog_var::in, module_info::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	hlds_goal::out) is det.

generate_memo_restore_goal(NumberedOutputVars, TipVar, ModuleInfo, Context,
		!VarTypes, !VarSet, Goal) :-
	(
		NumberedOutputVars = [_ | _],
		generate_new_table_var("RestoreBlockVar", answer_block_type,
			!VarTypes, !VarSet, RestoreBlockVar),
		generate_call("table_memo_get_answer_block",
			[TipVar, RestoreBlockVar], det, yes(semipure),
			ground_vars([RestoreBlockVar]),
			ModuleInfo, Context, GetBlockGoal),
		generate_restore_goals(NumberedOutputVars, RestoreBlockVar,
			ModuleInfo, Context, !VarTypes, !VarSet, RestoreGoals),
		GoalExpr = conj([GetBlockGoal | RestoreGoals]),
		assoc_list__keys(NumberedOutputVars, OutputVars),
		set__list_to_set([TipVar | OutputVars], NonLocals),
		goal_info_init_hide(NonLocals, bind_vars(OutputVars),
			det, semipure, Context, GoalInfo),
		Goal = GoalExpr - GoalInfo
	;
		NumberedOutputVars = [],
		true_goal(Goal)
	).

:- pred generate_mm_restore_goal(determinism::in,
	assoc_list(prog_var, int)::in, prog_var::in,
	module_info::in, term__context::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, hlds_goal::out) is det.

generate_mm_restore_goal(Detism, NumberedOutputVars, SubgoalVar,
		ModuleInfo, Context, !VarTypes, !VarSet, Goal) :-
	generate_new_table_var("AnswerTable", answer_block_type,
		!VarTypes, !VarSet, AnswerTableVar),
	( Detism = multidet ->
		ReturnAllAns = "table_mm_return_all_multi"
	; Detism = nondet ->
		ReturnAllAns = "table_mm_return_all_nondet"
	;
		error("generate_mm_restore_goal: invalid determinism")
	),
	generate_call(ReturnAllAns, [SubgoalVar, AnswerTableVar],
		Detism, yes(semipure), [AnswerTableVar - ground(unique, none)],
		ModuleInfo, Context, ReturnAnswerBlocksGoal),
	generate_restore_goals(NumberedOutputVars, AnswerTableVar, ModuleInfo,
		Context, !VarTypes, !VarSet, RestoreGoals),
	GoalExpr = conj([ReturnAnswerBlocksGoal | RestoreGoals]),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	set__list_to_set([SubgoalVar | OutputVars], NonLocals),
	create_instmap_delta([ReturnAnswerBlocksGoal | RestoreGoals],
		InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init_hide(NonLocals, InstMapDelta, nondet, semipure, Context,
		GoalInfo),
	Goal = GoalExpr - GoalInfo.

:- pred generate_restore_goals(assoc_list(prog_var, int)::in, prog_var::in,
	module_info::in, term__context::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, list(hlds_goal)::out) is det.

generate_restore_goals([], _, _, _, !VarTypes, !VarSet, []).
generate_restore_goals([NumberedVar | NumberedRest], AnswerBlockVar,
		ModuleInfo, Context, !VarTypes, !VarSet,
		[OffsetUnifyGoal, CallGoal | RestGoals]) :-
	NumberedVar = Var - Offset,
	gen_int_construction("OffsetVar", Offset, !VarTypes, !VarSet,
		OffsetVar, OffsetUnifyGoal),
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_restore_call_for_type(TypeCat, VarType, AnswerBlockVar, Var,
		OffsetVar, ModuleInfo, Context, CallGoal),
	generate_restore_goals(NumberedRest, AnswerBlockVar, ModuleInfo,
		Context, !VarTypes, !VarSet, RestGoals).

:- pred gen_restore_call_for_type(type_category::in, (type)::in,
	prog_var::in, prog_var::in, prog_var::in, module_info::in,
	term__context::in, hlds_goal::out) is det.

gen_restore_call_for_type(TypeCat, Type, TableVar, Var, OffsetVar,
		ModuleInfo, Context, Goal) :-
	( type_util__type_is_io_state(Type) ->
		LookupPredName = "table_restore_io_state_answer"
	; builtin_type(TypeCat) = no ->
		LookupPredName = "table_restore_any_answer"
	;
		type_save_category(TypeCat, CatString),
		string__append_list(["table_restore_", CatString, "_answer"],
			LookupPredName)
	),
	generate_call(LookupPredName, [TableVar, OffsetVar, Var], det,
		yes(semipure), [Var - ground(shared, none)],
		ModuleInfo, Context, Goal).

%-----------------------------------------------------------------------------%

:- pred generate_mm_suspend_goal(assoc_list(prog_var, int)::in, prog_var::in,
	module_info::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	hlds_goal::out) is det.

generate_mm_suspend_goal(NumberedOutputVars, TableVar, ModuleInfo, Context,
		!VarTypes, !VarSet, Goal) :-
	generate_new_table_var("AnswerTable", answer_block_type,
		!VarTypes, !VarSet, AnswerTableVar),
	generate_call("table_mm_suspend_consumer", [TableVar, AnswerTableVar],
		nondet, yes(semipure), ground_vars([AnswerTableVar]),
		ModuleInfo, Context, ReturnAnswerBlocksGoal),
	generate_restore_goals(NumberedOutputVars, AnswerTableVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreGoals),
	GoalExpr = conj([ReturnAnswerBlocksGoal | RestoreGoals]),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	set__list_to_set([TableVar | OutputVars], NonLocals),
	create_instmap_delta([ReturnAnswerBlocksGoal | RestoreGoals],
		InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init_hide(NonLocals, InstMapDelta, nondet, impure, Context,
		GoalInfo),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

:- func infinite_recursion_msg = string.

infinite_recursion_msg = "detected infinite recursion".

:- func bad_fail_msg = string.

bad_fail_msg = "failed subgoal in model_det predicate".

:- pred generate_error_goal(table_info::in, term__context::in,
	string::in, vartypes::in, vartypes::out,
	prog_varset::in, prog_varset::out, hlds_goal::out) is det.

generate_error_goal(TableInfo, Context, Msg, !VarTypes, !VarSet, Goal) :-
	ModuleInfo = TableInfo ^ table_module_info,
	PredInfo = TableInfo ^ table_cur_pred_info,

	Module = pred_info_module(PredInfo),
	Name = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	PredOrFuncStr = hlds_out__pred_or_func_to_str(PredOrFunc),
	prog_out__sym_name_to_string(qualified(Module, Name), NameStr),
	string__int_to_string(Arity, ArityStr),
	string__append_list([Msg, " in ", PredOrFuncStr, " ", NameStr,
		"/", ArityStr], Message),

	gen_string_construction("Message", Message, !VarTypes, !VarSet,
		MessageVar, MessageStrGoal),
	generate_call("table_error", [MessageVar], erroneous,
		no, [], ModuleInfo, Context, CallGoal),

	GoalExpr = conj([MessageStrGoal, CallGoal]),
	goal_info_init_hide(set__init, bind_vars([]), erroneous, impure,
		Context, GoalInfo),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

:- pred generate_new_table_var(string::in, (type)::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	prog_var::out) is det.

generate_new_table_var(Name, Type, !VarTypes, !VarSet, Var) :-
	varset__new_named_var(!.VarSet, Name, Var, !:VarSet),
	map__set(!.VarTypes, Var, Type, !:VarTypes).

:- pred generate_call(string::in, list(prog_var)::in, determinism::in,
	maybe(goal_feature)::in, assoc_list(prog_var, inst)::in,
	module_info::in, term__context::in, hlds_goal::out) is det.

generate_call(PredName, Args, Detism, MaybeFeature, InstMapSrc,
		ModuleInfo, Context, CallGoal) :-
	mercury_table_builtin_module(BuiltinModule),
	goal_util__generate_simple_call(BuiltinModule, PredName, predicate,
		Args, only_mode, Detism, MaybeFeature, InstMapSrc, ModuleInfo,
		Context, CallGoal).

:- pred append_fail(hlds_goal::in, hlds_goal::out) is det.

append_fail(Goal, GoalAndThenFail) :-
	Goal = _ - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	goal_info_get_context(GoalInfo, Context),
	instmap_delta_init_unreachable(UnreachInstMapDelta),
	goal_info_init_hide(NonLocals, UnreachInstMapDelta, failure, impure,
		Context, ConjGoalInfo),
	fail_goal(FailGoal),
	GoalAndThenFail = conj([Goal, FailGoal]) - ConjGoalInfo.

:- pred gen_int_construction(string::in, int::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	prog_var::out, hlds_goal::out) is det.

gen_int_construction(VarName, VarValue, !VarTypes, !VarSet, Var, Goal) :-
	make_int_const_construction(VarValue, yes(VarName), Goal, Var,
		!VarTypes, !VarSet).

:- pred gen_string_construction(string::in, string::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	prog_var::out, hlds_goal::out) is det.

gen_string_construction(VarName, VarValue, !VarTypes, !VarSet, Var, Goal) :-
	make_string_const_construction(VarValue, yes(VarName), Goal, Var,
		!VarTypes, !VarSet).

:- func trie_node_type = (type).
:- func subgoal_type = (type).
:- func answer_block_type = (type).
:- func loop_status_type = (type).
:- func memo_det_status_type = (type).
:- func memo_semi_status_type = (type).
:- func mm_status_type = (type).

trie_node_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "ml_trie_node") - 0, [], Type).

subgoal_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "ml_subgoal") - 0, [], Type).

answer_block_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "ml_answer_block") - 0, [], Type).

loop_status_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "loop_status") - 0, [], Type).

memo_det_status_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "memo_det_status") - 0, [], Type).

memo_semi_status_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "memo_semi_status") - 0, [], Type).

mm_status_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "mm_status") - 0, [], Type).

%-----------------------------------------------------------------------------%

:- pred get_input_output_vars(list(prog_var)::in, list(mode)::in,
	module_info::in, list(prog_var)::out,
	list(prog_var)::out, list(prog_var)::out) is det.

get_input_output_vars([], [], _, [], [], []).
get_input_output_vars([_|_], [], _, _, _, _) :-
	error("get_input_output_vars: lists not same length").
get_input_output_vars([], [_|_], _, _, _, _) :-
	error("get_input_output_vars: lists not same length").
get_input_output_vars([Var | RestV], [Mode | RestM], ModuleInfo,
		BadVars, InVars, OutVars) :-
	% XXX We should check not just the boundedness of Var, but also
	% its sharing state: tabled arguments should not share.
	( mode_is_fully_input(ModuleInfo, Mode) ->
		get_input_output_vars(RestV, RestM, ModuleInfo,
			BadVars, InVars0, OutVars),
		InVars = [Var | InVars0]
	; mode_is_fully_output(ModuleInfo, Mode) ->
		get_input_output_vars(RestV, RestM, ModuleInfo,
			BadVars, InVars, OutVars0),
		OutVars = [Var | OutVars0]
	;
		get_input_output_vars(RestV, RestM, ModuleInfo,
			BadVars0, InVars, OutVars),
		BadVars = [Var | BadVars0]
	).

%-----------------------------------------------------------------------------%

:- pred create_instmap_delta(hlds_goals::in, instmap_delta::out) is det.

create_instmap_delta([], IMD) :-
	instmap_delta_from_assoc_list([], IMD).
create_instmap_delta([Goal | Rest], IMD) :-
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, IMD0),
	create_instmap_delta(Rest, IMD1),
	instmap_delta_apply_instmap_delta(IMD0, IMD1, IMD).

%-----------------------------------------------------------------------------%

:- pred allocate_slot_numbers(list(prog_var)::in, int::in,
	assoc_list(prog_var, int)::out) is det.

allocate_slot_numbers([], _, []).
allocate_slot_numbers([Var | Vars], Offset0, [Var - Offset0 | NumberedVars]) :-
	allocate_slot_numbers(Vars, Offset0 + 1, NumberedVars).

:- pred key_belong_to_list(list(T)::in, pair(T, U)::in) is semidet.

key_belong_to_list(List, Key - _Value) :-
	list__member(Key, List).

:- func bind_vars(list(prog_var)) = instmap_delta.

bind_vars(Vars) = InstMapDelta :-
	VarsAndGround = ground_vars(Vars),
	instmap_delta_from_assoc_list(VarsAndGround, InstMapDelta).

:- func ground_vars(list(prog_var)) = assoc_list(prog_var, inst).

ground_vars(Vars) = VarsAndGround :-
	VarsAndGround = list__map(pair_with_ground, Vars).

:- func pair_with_ground(prog_var) = pair(prog_var, inst).

pair_with_ground(Var) = Var - ground(shared, none).

:- pred goal_info_init_hide(set(prog_var)::in, instmap_delta::in,
	determinism::in, purity::in, prog_context::in, hlds_goal_info::out)
	is det.

goal_info_init_hide(NonLocals, InstmapDelta, Detism, Purity, Context,
		GoalInfo) :-
	goal_info_init(NonLocals, InstmapDelta, Detism, Purity, Context,
		GoalInfo0),
	goal_info_add_feature(GoalInfo0, hide_debug_event, GoalInfo).

%-----------------------------------------------------------------------------%

:- func builtin_type(type_category) = bool.

% For backward compatibility, we treat type_info_type as user_type. However,
% this makes the tabling of type_infos more expensive than necessary, since
% we essentially table the information in the type_info twice, once by tabling
% the type represented by the type_info (since this is the value of the type
% argument of the type constructor private_builtin.type_info/1, and then
% tabling the type_info itself.

builtin_type(int_type) = yes.
builtin_type(char_type) = yes.
builtin_type(str_type) = yes.
builtin_type(float_type) = yes.
builtin_type(void_type) = yes.
builtin_type(type_info_type) = no.
builtin_type(type_ctor_info_type) = yes.
builtin_type(typeclass_info_type) = yes.
builtin_type(base_typeclass_info_type) = yes.
builtin_type(higher_order_type) = no.
builtin_type(enum_type) = no.
builtin_type(variable_type) = no.
builtin_type(tuple_type) = no.
builtin_type(user_ctor_type) = no.

% Figure out what kind of data structure implements the lookup table for values
% of a given builtin type.

:- pred lookup_tabling_category(type_category::in,
	maybe(pair(string, table_trie_step))::out) is det.

lookup_tabling_category(int_type,   yes("int" -    table_trie_step_int)).
lookup_tabling_category(char_type,  yes("char" -   table_trie_step_char)).
lookup_tabling_category(str_type,   yes("string" - table_trie_step_string)).
lookup_tabling_category(float_type, yes("float" -  table_trie_step_float)).
lookup_tabling_category(void_type, _) :-
	error("lookup_tabling_category: void").
lookup_tabling_category(type_info_type, no).
lookup_tabling_category(type_ctor_info_type, no).
lookup_tabling_category(typeclass_info_type, no).
lookup_tabling_category(base_typeclass_info_type, no).
lookup_tabling_category(enum_type, no).
lookup_tabling_category(higher_order_type, no).
lookup_tabling_category(tuple_type, no).
lookup_tabling_category(variable_type, no).
lookup_tabling_category(user_ctor_type, no).

% Figure out which save and restore predicates in library/table_builtin.m
% we need to use for values of types belonging the type category given by
% the first argument. The returned value replaces CAT in table_save_CAT_answer
% and table_restore_CAT_answer.

:- pred type_save_category(type_category::in, string::out) is det.

type_save_category(enum_type,		"enum").
type_save_category(int_type,		"int").
type_save_category(char_type,		"char").
type_save_category(str_type,		"string").
type_save_category(float_type,		"float").
type_save_category(higher_order_type,	"pred").
type_save_category(tuple_type,		"any").
type_save_category(user_ctor_type,	"any").		% could do better
type_save_category(variable_type,	"any").		% could do better
type_save_category(void_type, _) :-
	error("type_save_category: void").
type_save_category(type_info_type, "any").		% could do better
type_save_category(type_ctor_info_type, _) :-
	error("type_save_category: type_ctor_info").
type_save_category(typeclass_info_type, _) :-
	error("type_save_category: typeclass_info").
type_save_category(base_typeclass_info_type, _) :-
	error("type_save_category: base_typeclass_info").

%-----------------------------------------------------------------------------%

:- pred table_gen__make_type_info_var((type)::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, prog_var::out,
	list(hlds_goal)::out) is det.

table_gen__make_type_info_var(Type, Context, !VarTypes, !VarSet, !TableInfo,
		TypeInfoVar, TypeInfoGoals) :-
	table_gen__make_type_info_vars([Type], Context, !VarTypes, !VarSet,
		!TableInfo, TypeInfoVars, TypeInfoGoals),
	( TypeInfoVars = [TypeInfoVar0] ->
		TypeInfoVar = TypeInfoVar0
	;
		error("table_gen__make_type_info_var: list length != 1")
	).

:- pred table_gen__make_type_info_vars(list(type)::in, term__context::in,
	vartypes::in, vartypes::out, prog_varset::in, prog_varset::out,
	table_info::in, table_info::out, list(prog_var)::out,
	list(hlds_goal)::out) is det.

table_gen__make_type_info_vars(Types, Context, !VarTypes, !VarSet, !TableInfo,
		TypeInfoVars, TypeInfoGoals) :-
	%
	% Extract the information from table_info
	%
	table_info_extract(!.TableInfo, ModuleInfo0, PredId, ProcId,
		PredInfo0, ProcInfo0),

	%
	% Put the varset and vartypes from the simplify_info
	% back in the proc_info
	%
	proc_info_set_vartypes(!.VarTypes, ProcInfo0, ProcInfo1),
	proc_info_set_varset(!.VarSet, ProcInfo1, ProcInfo2),

	%
	% Call polymorphism.m to create the type_infos
	%
	create_poly_info(ModuleInfo0, PredInfo0, ProcInfo2, PolyInfo0),
	polymorphism__make_type_info_vars(Types, Context,
		TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo),
	poly_info_extract(PolyInfo, PredInfo0, PredInfo,
		ProcInfo0, ProcInfo, ModuleInfo),

	%
	% Get the new varset and vartypes from the proc_info
	%
	proc_info_vartypes(ProcInfo, !:VarTypes),
	proc_info_varset(ProcInfo, !:VarSet),

	%
	% Put the new module_info, pred_info, and proc_info back in the
	% table_info.
	%
	table_info_init(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
		!:TableInfo).

%-----------------------------------------------------------------------------%

:- type table_info
	---> table_info(
			table_module_info	:: module_info,
			table_cur_pred_id	:: pred_id,
			table_cur_proc_id	:: proc_id,
			table_cur_pred_info	:: pred_info,
			table_cur_proc_info	:: proc_info
		).

:- pred table_info_init(module_info::in, pred_id::in, proc_id::in,
	pred_info::in, proc_info::in, table_info::out) is det.

:- pred table_info_extract(table_info::in, module_info::out,
	pred_id::out, proc_id::out, pred_info::out, proc_info::out) is det.

table_info_init(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo, TableInfo) :-
	TableInfo = table_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo).

table_info_extract(TableInfo, ModuleInfo, PredId, ProcId, PredInfo, ProcInfo)
		:-
	TableInfo = table_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo).

%-----------------------------------------------------------------------------%
