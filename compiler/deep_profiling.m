%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: conway.
%
% This module applies the deep profiling transformation described in the paper
% ``Engineering a profiler for a logic programming language'' by Thomas Conway
% and Zoltan Somogyi.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__deep_profiling.

:- interface.

:- import_module hlds__hlds_module, ll_backend__layout.
:- import_module io, list.

:- pred apply_deep_profiling_transformation(module_info::in, module_info::out,
	list(layout_data)::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module (parse_tree__inst), hlds__instmap, hlds__hlds_data.
:- import_module hlds__hlds_pred, hlds__hlds_goal, parse_tree__prog_data.
:- import_module backend_libs__code_model, ll_backend__code_util.
:- import_module parse_tree__prog_util, check_hlds__type_util.
:- import_module check_hlds__mode_util.
:- import_module hlds__quantification, transform_hlds__dependency_graph.
:- import_module backend_libs__rtti, ll_backend__trace.
:- import_module libs__options, libs__globals.
:- import_module bool, int, list, assoc_list, map, require, set.
:- import_module std_util, string, term, varset, counter.

apply_deep_profiling_transformation(ModuleInfo0, ModuleInfo, ProcStatics) -->
	{ module_info_globals(ModuleInfo0, Globals) },
	{ globals__lookup_bool_option(Globals, deep_profile_tail_recursion,
		TailRecursion) },
	(
		{ TailRecursion = yes },
		{ apply_tail_recursion_transformation(ModuleInfo0,
			ModuleInfo1) }
	;
		{ TailRecursion = no },
		{ ModuleInfo1 = ModuleInfo0 }
	),
	{ module_info_predids(ModuleInfo1, PredIds) },
	{ module_info_get_predicate_table(ModuleInfo1, PredTable0) },
	{ predicate_table_get_preds(PredTable0, PredMap0) },
	{ list__foldl2(transform_predicate(ModuleInfo1),
		PredIds, PredMap0, PredMap, [], MaybeProcStatics) },
		% Remove any duplicates that resulted from
		% references in inner tail recursive procedures
	{ list__filter_map(
		(pred(MaybeProcStatic::in, ProcStatic::out) is semidet :-
			MaybeProcStatic = yes(ProcStatic)
	), MaybeProcStatics, ProcStatics) },
	{ predicate_table_set_preds(PredTable0, PredMap, PredTable) },
	{ module_info_set_predicate_table(ModuleInfo1, PredTable, ModuleInfo) }.

%-----------------------------------------------------------------------------%

:- pred apply_tail_recursion_transformation(module_info::in, module_info::out)
	is det.

apply_tail_recursion_transformation(ModuleInfo0, ModuleInfo) :-
	module_info_ensure_dependency_info(ModuleInfo0, ModuleInfo1),
	module_info_dependency_info(ModuleInfo1, DepInfo),
	hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
	list__foldl(apply_tail_recursion_to_scc, SCCs,
		ModuleInfo1, ModuleInfo).

:- pred apply_tail_recursion_to_scc(list(pred_proc_id)::in,
	module_info::in, module_info::out) is det.

apply_tail_recursion_to_scc(SCC, ModuleInfo0, ModuleInfo) :-
	% For the time being, we only look for self-tail-recursive calls.
	list__foldl(apply_tail_recursion_to_proc, SCC,
		ModuleInfo0, ModuleInfo).

:- pred apply_tail_recursion_to_proc(pred_proc_id::in,
	module_info::in, module_info::out) is det.

apply_tail_recursion_to_proc(PredProcId, ModuleInfo0, ModuleInfo) :-
	PredProcId = proc(PredId, ProcId),
	module_info_preds(ModuleInfo0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_arg_types(PredInfo0, Types),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	proc_info_interface_determinism(ProcInfo0, Detism),
	(
		determinism_components(Detism, _CanFail, SolnCount),
		SolnCount \= at_most_many,
		proc_info_headvars(ProcInfo0, HeadVars),
		proc_info_argmodes(ProcInfo0, Modes),
		find_list_of_output_args(HeadVars, Modes, Types, ModuleInfo0,
			Outputs),
		clone_proc_id(ProcTable0, ProcId, CloneProcId),
		ClonePredProcId = proc(PredId, CloneProcId),
		ApplyInfo = apply_tail_recursion_info(ModuleInfo0,
			[PredProcId - ClonePredProcId], Detism, Outputs),
		apply_tail_recursion_to_goal(Goal0, ApplyInfo,
			Goal, no, FoundTailCall, _),
		FoundTailCall = yes
	->
		proc_info_set_goal(ProcInfo0, Goal, ProcInfo1),
		figure_out_rec_call_numbers(Goal, 0, _N, [], TailCallSites),
		OrigDeepProfileInfo = deep_profile_proc_info(
			outer_proc(ClonePredProcId),
			[visible_scc_data(PredProcId, ClonePredProcId,
				TailCallSites)]),
		CloneDeepProfileInfo = deep_profile_proc_info(
			inner_proc(PredProcId),
			[visible_scc_data(PredProcId, ClonePredProcId,
				TailCallSites)]),
		proc_info_set_maybe_deep_profile_info(ProcInfo1,
			yes(OrigDeepProfileInfo), ProcInfo),
		proc_info_set_maybe_deep_profile_info(ProcInfo1,
			yes(CloneDeepProfileInfo), CloneProcInfo),
		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable1),
		map__det_insert(ProcTable1, CloneProcId, CloneProcInfo,
			ProcTable),
		pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
		map__det_update(PredTable0, PredId, PredInfo, PredTable),
		module_info_set_preds(ModuleInfo0, PredTable, ModuleInfo)
	;
		ModuleInfo = ModuleInfo0
	).

:- pred find_list_of_output_args(list(prog_var)::in, list(mode)::in,
	list(type)::in, module_info::in, list(prog_var)::out) is det.

find_list_of_output_args(Vars, Modes, Types, ModuleInfo, Outputs) :-
	(
		find_list_of_output_args_2(Vars, Modes, Types, ModuleInfo,
			OutputsPrime)
	->
		Outputs = OutputsPrime
	;
		error("find_list_of_output_args: list length mismatch")
	).

:- pred find_list_of_output_args_2(list(prog_var)::in, list(mode)::in,
	list(type)::in, module_info::in, list(prog_var)::out) is semidet.

find_list_of_output_args_2([], [], [], _, []).
find_list_of_output_args_2([Var | Vars], [Mode | Modes], [Type | Types],
		ModuleInfo, Outputs) :-
	find_list_of_output_args_2(Vars, Modes, Types, ModuleInfo, Outputs1),
	mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
	( ArgMode = top_in ->
		Outputs = Outputs1
	;
		Outputs = [Var | Outputs1]
	).

%-----------------------------------------------------------------------------%

:- type apply_tail_recursion_info
	--->	apply_tail_recursion_info(
			moduleinfo	:: module_info,
			scc_ppids	:: assoc_list(pred_proc_id),
			detism		:: determinism,
			outputs		:: list(prog_var)
		).

:- pred apply_tail_recursion_to_goal(hlds_goal::in,
	apply_tail_recursion_info::in, hlds_goal::out, bool::in, bool::out,
	maybe(list(prog_var))::out) is det.

apply_tail_recursion_to_goal(Goal0, ApplyInfo, Goal,
		FoundTailCall0, FoundTailCall, Continue) :-
	Goal0 = GoalExpr0 - GoalInfo0,
	(
		GoalExpr0 = foreign_proc(_, _, _, _, _, _, _),
		Goal = Goal0,
		FoundTailCall = FoundTailCall0,
		Continue = no
	;
		GoalExpr0 = call(PredId, ProcId, Args,
			Builtin, UnifyContext, SymName),
		(
			PredProcId = proc(PredId, ProcId),
			assoc_list__search(ApplyInfo ^ scc_ppids, PredProcId,
				ClonePredProcId),
			module_info_pred_proc_info(ApplyInfo ^ moduleinfo,
				PredId, ProcId, PredInfo, ProcInfo),
			proc_info_interface_determinism(ProcInfo, CallDetism),
			CallDetism = ApplyInfo ^ detism,
			pred_info_arg_types(PredInfo, Types),
			proc_info_argmodes(ProcInfo, Modes),
			find_list_of_output_args(Args, Modes, Types,
				ApplyInfo ^ moduleinfo, CallOutputs),
			CallOutputs = ApplyInfo ^ outputs,
			Builtin = not_builtin
		->
			ClonePredProcId = proc(ClonePredId, CloneProcId),
			GoalExpr = call(ClonePredId, CloneProcId, Args,
				Builtin, UnifyContext, SymName),
			goal_info_add_feature(GoalInfo0, tailcall, GoalInfo),
			Goal = GoalExpr - GoalInfo,
			FoundTailCall = yes
		;
			Goal = Goal0,
			FoundTailCall = FoundTailCall0
		),
		Continue = no
	;
		GoalExpr0 = generic_call(_, _, _, _),
		Goal = Goal0,
		FoundTailCall = FoundTailCall0,
		Continue = no
	;
		GoalExpr0 = unify(_, _, _, Unify0, _),
		Goal = Goal0,
		FoundTailCall = FoundTailCall0,
		(
			Unify0 = assign(ToVar, FromVar)
		->
			apply_tail_recursion_process_assign(
				ApplyInfo ^ outputs, ToVar, FromVar, Outputs),
			Continue = yes(Outputs)
		;
			Continue = no
		)
	;
		GoalExpr0 = conj(Goals0),
		apply_tail_recursion_to_conj(Goals0, ApplyInfo,
			Goals, FoundTailCall0, FoundTailCall, Continue),
		GoalExpr = conj(Goals),
		Goal = GoalExpr - GoalInfo0
	;
		GoalExpr0 = disj(Goals0),
		apply_tail_recursion_to_disj(Goals0, ApplyInfo,
			Goals, FoundTailCall0, FoundTailCall),
		GoalExpr = disj(Goals),
		Goal = GoalExpr - GoalInfo0,
		Continue = no
	;
		GoalExpr0 = switch(Var, CanFail, Cases0),
		apply_tail_recursion_to_cases(Cases0, ApplyInfo,
			Cases, FoundTailCall0, FoundTailCall),
		GoalExpr = switch(Var, CanFail, Cases),
		Goal = GoalExpr - GoalInfo0,
		Continue = no
	;
		GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
		apply_tail_recursion_to_goal(Then0, ApplyInfo,
			Then, FoundTailCall0, FoundTailCall1, _),
		apply_tail_recursion_to_goal(Else0, ApplyInfo,
			Else, FoundTailCall1, FoundTailCall, _),
		GoalExpr = if_then_else(Vars, Cond, Then, Else),
		Goal = GoalExpr - GoalInfo0,
		Continue = no
	;
		GoalExpr0 = par_conj(_),
		Goal = Goal0,
		FoundTailCall = FoundTailCall0,
		Continue = no
	;
		GoalExpr0 = some(_, _, _),
		Goal = Goal0,
		FoundTailCall = FoundTailCall0,
		Continue = no
	;
		GoalExpr0 = not(_),
		Goal = Goal0,
		FoundTailCall = FoundTailCall0,
		Continue = no
	;
		GoalExpr0 = shorthand(_),
		error("shorthand in apply_tail_recursion_to_goal")
	).

:- pred apply_tail_recursion_process_assign(list(prog_var)::in,
	prog_var::in, prog_var::in, list(prog_var)::out) is det.

apply_tail_recursion_process_assign([], _, _, []).
apply_tail_recursion_process_assign([Output0 | Outputs0], ToVar, FromVar,
		[Output | Outputs]) :-
	( ToVar = Output0 ->
		Output = FromVar
	;
		Output = Output0
	),
	apply_tail_recursion_process_assign(Outputs0, ToVar, FromVar, Outputs).

:- pred apply_tail_recursion_to_conj(list(hlds_goal)::in,
	apply_tail_recursion_info::in, list(hlds_goal)::out,
	bool::in, bool::out, maybe(list(prog_var))::out) is det.

apply_tail_recursion_to_conj([], ApplyInfo, [],
		FoundTailCall, FoundTailCall, yes(ApplyInfo ^ outputs)).
apply_tail_recursion_to_conj([Goal0 | Goals0], ApplyInfo0, [Goal | Goals],
		FoundTailCall0, FoundTailCall, Continue) :-
	apply_tail_recursion_to_conj(Goals0, ApplyInfo0, Goals,
		FoundTailCall0, FoundTailCall1, Continue1),
	(
		Continue1 = yes(Outputs),
		apply_tail_recursion_to_goal(Goal0,
			ApplyInfo0 ^ outputs := Outputs, Goal,
			FoundTailCall1, FoundTailCall, Continue)
	;
		Continue1 = no,
		Goal = Goal0,
		FoundTailCall = FoundTailCall1,
		Continue = no
	).

:- pred apply_tail_recursion_to_disj(list(hlds_goal)::in,
	apply_tail_recursion_info::in, list(hlds_goal)::out,
	bool::in, bool::out) is det.

apply_tail_recursion_to_disj([], _, [], FoundTailCall, FoundTailCall).
apply_tail_recursion_to_disj([Goal0], ApplyInfo, [Goal],
		FoundTailCall0, FoundTailCall) :-
	apply_tail_recursion_to_goal(Goal0, ApplyInfo, Goal,
		FoundTailCall0, FoundTailCall, _).
apply_tail_recursion_to_disj([Goal0 | Goals0], ApplyInfo, [Goal0 | Goals],
		FoundTailCall0, FoundTailCall) :-
	Goals0 = [_ | _],
	apply_tail_recursion_to_disj(Goals0, ApplyInfo, Goals,
		FoundTailCall0, FoundTailCall).

:- pred apply_tail_recursion_to_cases(list(case)::in,
	apply_tail_recursion_info::in, list(case)::out,
	bool::in, bool::out) is det.

apply_tail_recursion_to_cases([], _,
		[], FoundTailCall, FoundTailCall).
apply_tail_recursion_to_cases([case(ConsId, Goal0) | Cases0], ApplyInfo,
		[case(ConsId, Goal) | Cases], FoundTailCall0, FoundTailCall) :-
	apply_tail_recursion_to_goal(Goal0, ApplyInfo, Goal,
		FoundTailCall0, FoundTailCall1, _),
	apply_tail_recursion_to_cases(Cases0, ApplyInfo, Cases,
		FoundTailCall1, FoundTailCall).

%-----------------------------------------------------------------------------%

:- pred figure_out_rec_call_numbers(hlds_goal, int, int, list(int), list(int)).
:- mode figure_out_rec_call_numbers(in, in, out, in, out) is det.

figure_out_rec_call_numbers(Goal, N0, N, TailCallSites0, TailCallSites) :-
	Goal = GoalExpr - GoalInfo,
	(
		GoalExpr = foreign_proc(Attrs, _, _, _, _, _, _),
		( may_call_mercury(Attrs, may_call_mercury) ->
			N = N0 + 1
		;
			N = N0
		),
		TailCallSites = TailCallSites0
	;
		GoalExpr = call(_, _, _, BuiltinState, _, _),
		goal_info_get_features(GoalInfo, Features),
		( BuiltinState \= inline_builtin ->
			N = N0 + 1
		;
			N = N0
		),
		( set__member(tailcall, Features) ->
			TailCallSites = [N0|TailCallSites0]
		;
			TailCallSites = TailCallSites0
		)
	;
		GoalExpr = generic_call(_, _, _, _),
		N = N0 + 1,
		TailCallSites = TailCallSites0
	;
		GoalExpr = unify(_, _, _, _, _),
		N = N0,
		TailCallSites = TailCallSites0
	;
		GoalExpr = conj(Goals),
		figure_out_rec_call_numbers_in_goal_list(Goals, N0, N,
			TailCallSites0, TailCallSites)
	;
		GoalExpr = disj(Goals),
		figure_out_rec_call_numbers_in_goal_list(Goals, N0, N,
			TailCallSites0, TailCallSites)
	;
		GoalExpr = switch(_, _, Cases),
		figure_out_rec_call_numbers_in_case_list(Cases, N0, N,
			TailCallSites0, TailCallSites)
	;
		GoalExpr = if_then_else(_, Cond, Then, Else),
		figure_out_rec_call_numbers(Cond, N0, N1,
			TailCallSites0, TailCallSites1),
		figure_out_rec_call_numbers(Then, N1, N2,
			TailCallSites1, TailCallSites2),
		figure_out_rec_call_numbers(Else, N2, N,
			TailCallSites2, TailCallSites)
	;
		GoalExpr = par_conj(Goals),
		figure_out_rec_call_numbers_in_goal_list(Goals, N0, N,
			TailCallSites0, TailCallSites)
	;
		GoalExpr = some(_, _, Goal1),
		figure_out_rec_call_numbers(Goal1, N0, N,
			TailCallSites0, TailCallSites)
	;
		GoalExpr = not(Goal1),
		figure_out_rec_call_numbers(Goal1, N0, N,
			TailCallSites0, TailCallSites)
	;
		GoalExpr = shorthand(_),
		error("shorthand in apply_tail_recursion_to_goal")
	).

:- pred figure_out_rec_call_numbers_in_goal_list(list(hlds_goal), int, int,
		list(int), list(int)).
:- mode figure_out_rec_call_numbers_in_goal_list(in, in, out, in, out) is det.

figure_out_rec_call_numbers_in_goal_list([], N, N,
		TailCallSites, TailCallSites).
figure_out_rec_call_numbers_in_goal_list([Goal|Goals], N0, N,
		TailCallSites0, TailCallSites) :-
	figure_out_rec_call_numbers(Goal, N0, N1,
		TailCallSites0, TailCallSites1),
	figure_out_rec_call_numbers_in_goal_list(Goals, N1, N,
		TailCallSites1, TailCallSites).

:- pred figure_out_rec_call_numbers_in_case_list(list(case), int, int,
		list(int), list(int)).
:- mode figure_out_rec_call_numbers_in_case_list(in, in, out, in, out) is det.

figure_out_rec_call_numbers_in_case_list([], N, N,
		TailCallSites, TailCallSites).
figure_out_rec_call_numbers_in_case_list([Case|Cases], N0, N,
		TailCallSites0, TailCallSites) :-
	Case = case(_, Goal),
	figure_out_rec_call_numbers(Goal, N0, N1,
		TailCallSites0, TailCallSites1),
	figure_out_rec_call_numbers_in_case_list(Cases, N1, N,
		TailCallSites1, TailCallSites).

%-----------------------------------------------------------------------------%

:- pred transform_predicate(module_info::in, pred_id::in,
	pred_table::in, pred_table::out,
	list(maybe(layout_data))::in, list(maybe(layout_data))::out) is det.

transform_predicate(ModuleInfo, PredId, PredMap0, PredMap,
		ProcStatics0, ProcStatics) :-
	map__lookup(PredMap0, PredId, PredInfo0),
	pred_info_non_imported_procids(PredInfo0, ProcIds),
	pred_info_procedures(PredInfo0, ProcTable0),
	list__foldl2(maybe_transform_procedure(ModuleInfo, PredId),
		ProcIds, ProcTable0, ProcTable, ProcStatics0, ProcStatics),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__det_update(PredMap0, PredId, PredInfo, PredMap).

:- pred maybe_transform_procedure(module_info::in, pred_id::in, proc_id::in,
	proc_table::in, proc_table::out,
	list(maybe(layout_data))::in, list(maybe(layout_data))::out) is det.

maybe_transform_procedure(ModuleInfo, PredId, ProcId, ProcTable0, ProcTable,
		ProcStatics0, ProcStatics) :-
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_goal(ProcInfo0, Goal0),
	predicate_module(ModuleInfo, PredId, PredModuleName),
	(
		% XXX We need to eliminate nondet C code...
		Goal0 = foreign_proc(_,_,_,_,_,_, Impl) - _,
		Impl = nondet(_, _, _, _, _, _, _, _, _)
	->
		error("deep profiling is incompatible with nondet foreign code")
	;
		% We don't want to transform the procedures for
		% managing the deep profiling call graph, or we'd get
		% infinite recursion.
		mercury_profiling_builtin_module(PredModuleName)
	->
		ProcTable = ProcTable0,
		ProcStatics = ProcStatics0
	;
		transform_procedure2(ModuleInfo, proc(PredId, ProcId),
			ProcInfo0, ProcInfo, ProcStatics0, ProcStatics),
		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable)
	).

:- pred transform_procedure2(module_info::in, pred_proc_id::in,
	proc_info::in, proc_info::out,
	list(maybe(layout_data))::in, list(maybe(layout_data))::out) is det.

transform_procedure2(ModuleInfo, PredProcId, Proc0, Proc,
		ProcStaticList0, ProcStaticList) :-
	proc_info_get_maybe_deep_profile_info(Proc0, MaybeRecInfo),
	proc_info_interface_code_model(Proc0, CodeModel),
	(
		CodeModel = model_det,
		(
			MaybeRecInfo = yes(RecInfo),
			RecInfo ^ role = inner_proc(_)
		->
			transform_inner_proc(ModuleInfo, PredProcId, Proc0,
				Proc, MaybeProcStatic)
		;
			transform_det_proc(ModuleInfo, PredProcId, Proc0,
				Proc, MaybeProcStatic)
		)
	;
		CodeModel = model_semi,
		(
			MaybeRecInfo = yes(RecInfo),
			RecInfo ^ role = inner_proc(_)
		->
			transform_inner_proc(ModuleInfo, PredProcId, Proc0,
				Proc, MaybeProcStatic)
		;
			transform_semi_proc(ModuleInfo, PredProcId, Proc0,
				Proc, MaybeProcStatic)
		)
	;
		CodeModel = model_non,
		transform_non_proc(ModuleInfo, PredProcId, Proc0,
			Proc, MaybeProcStatic)
	),
	ProcStaticList = [MaybeProcStatic | ProcStaticList0].

%-----------------------------------------------------------------------------%

:- type deep_info --->
	deep_info(
		module_info		:: module_info,
		pred_proc_id		:: pred_proc_id,
		current_csd		:: prog_var,
		site_num_counter	:: counter,
		call_sites		:: list(call_site_static_data),
		vars			:: prog_varset,
		var_types		:: vartypes,
		proc_filename		:: string,
		maybe_rec_info		:: maybe(deep_profile_proc_info)
	).

:- pred transform_det_proc(module_info::in, pred_proc_id::in,
	proc_info::in, proc_info::out, maybe(layout_data)::out) is det.

transform_det_proc(ModuleInfo, PredProcId, Proc0, Proc, yes(ProcStatic)) :-
	proc_info_goal(Proc0, Goal0),
	Goal0 = _ - GoalInfo0,
	proc_info_varset(Proc0, Vars0),
	proc_info_vartypes(Proc0, VarTypes0),
	CPointerType = c_pointer_type,
	varset__new_named_var(Vars0, "TopCSD", TopCSD, Vars1),
	map__set(VarTypes0, TopCSD, CPointerType, VarTypes1),
	varset__new_named_var(Vars1, "MiddleCSD", MiddleCSD, Vars2),
	map__set(VarTypes1, MiddleCSD, CPointerType, VarTypes2),
	varset__new_named_var(Vars2, "ProcStatic", ProcStaticVar, Vars3),
	map__set(VarTypes2, ProcStaticVar, CPointerType, VarTypes3),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, use_activation_counts,
		UseActivationCounts),
	(
		UseActivationCounts = no,
		varset__new_named_var(Vars3, "ActivationPtr", ActivationPtr0,
			Vars5),
		map__set(VarTypes3, ActivationPtr0, CPointerType, VarTypes5),
		MaybeActivationPtr = yes(ActivationPtr0)
	;
		UseActivationCounts = yes,
		Vars5 = Vars3,
		VarTypes5 = VarTypes3,
		MaybeActivationPtr = no
	),
	proc_info_context(Proc0, Context),
	FileName = term__context_file(Context),
	LineNumber = term__context_line(Context),

	proc_info_get_maybe_deep_profile_info(Proc0, MaybeRecInfo),
	DeepInfo0 = deep_info(ModuleInfo, PredProcId, MiddleCSD,
		counter__init(0), [], Vars5, VarTypes5,
		FileName, MaybeRecInfo),

	transform_goal([], Goal0, TransformedGoal, _, DeepInfo0, DeepInfo),

	Vars = DeepInfo ^ vars,
	VarTypes = DeepInfo ^ var_types,
	CallSites = DeepInfo ^ call_sites,

	(
		MaybeRecInfo = yes(RecInfo),
		RecInfo ^ role = inner_proc(OuterPredProcId)
	->
		OuterPredProcId = proc(PredId, ProcId)
	;
		PredProcId = proc(PredId, ProcId)
	),

	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	IsInInterface = is_proc_in_interface(ModuleInfo, PredId, ProcId),
	ProcStatic = proc_static_data(RttiProcLabel, FileName, LineNumber,
		IsInInterface, CallSites),
	ProcStaticConsId = deep_profiling_proc_static(RttiProcLabel),
	generate_unify(ProcStaticConsId, ProcStaticVar, BindProcStaticVarGoal),

	(
		MaybeActivationPtr = yes(ActivationPtr1),
		generate_call(ModuleInfo, "det_call_port_code_sr", 4,
			[ProcStaticVar, TopCSD, MiddleCSD, ActivationPtr1],
			[TopCSD, MiddleCSD, ActivationPtr1], CallPortCode),
		generate_call(ModuleInfo, "det_exit_port_code_sr", 3,
			[TopCSD, MiddleCSD, ActivationPtr1], [], ExitPortCode)
	;
		MaybeActivationPtr = no,
		generate_call(ModuleInfo, "det_call_port_code_ac", 3,
			[ProcStaticVar, TopCSD, MiddleCSD],
			[TopCSD, MiddleCSD], CallPortCode),
		generate_call(ModuleInfo, "det_exit_port_code_ac", 2,
			[TopCSD, MiddleCSD], [], ExitPortCode)
	),

	goal_info_add_feature(GoalInfo0, impure, GoalInfo),
	Goal = conj([
		BindProcStaticVarGoal,
		CallPortCode,
		TransformedGoal,
		ExitPortCode
	]) - GoalInfo,
	proc_info_set_varset(Proc0, Vars, Proc1),
	proc_info_set_vartypes(Proc1, VarTypes, Proc2),
	proc_info_set_goal(Proc2, Goal, Proc).

:- pred transform_semi_proc(module_info::in, pred_proc_id::in,
	proc_info::in, proc_info::out, maybe(layout_data)::out) is det.

transform_semi_proc(ModuleInfo, PredProcId, Proc0, Proc, yes(ProcStatic)) :-
	proc_info_goal(Proc0, Goal0),
	Goal0 = _ - GoalInfo0,
	proc_info_varset(Proc0, Vars0),
	proc_info_vartypes(Proc0, VarTypes0),
	CPointerType = c_pointer_type,
	varset__new_named_var(Vars0, "TopCSD", TopCSD, Vars1),
	map__set(VarTypes0, TopCSD, CPointerType, VarTypes1),
	varset__new_named_var(Vars1, "MiddleCSD", MiddleCSD, Vars2),
	map__set(VarTypes1, MiddleCSD, CPointerType, VarTypes2),
	varset__new_named_var(Vars2, "ProcStatic", ProcStaticVar, Vars3),
	map__set(VarTypes2, ProcStaticVar, CPointerType, VarTypes3),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, use_activation_counts,
		UseActivationCounts),
	(
		UseActivationCounts = no,
		varset__new_named_var(Vars3, "ActivationPtr", ActivationPtr0,
			Vars5),
		map__set(VarTypes3, ActivationPtr0, CPointerType, VarTypes5),
		MaybeActivationPtr = yes(ActivationPtr0)
	;
		UseActivationCounts = yes,
		Vars5 = Vars3,
		VarTypes5 = VarTypes3,
		MaybeActivationPtr = no
	),
	proc_info_context(Proc0, Context),
	FileName = term__context_file(Context),
	LineNumber = term__context_line(Context),

	proc_info_get_maybe_deep_profile_info(Proc0, MaybeRecInfo),
	DeepInfo0 = deep_info(ModuleInfo, PredProcId, MiddleCSD,
		counter__init(0), [], Vars5, VarTypes5,
		FileName, MaybeRecInfo),

	transform_goal([], Goal0, TransformedGoal, _, DeepInfo0, DeepInfo),

	Vars = DeepInfo ^ vars,
	VarTypes = DeepInfo ^ var_types,
	CallSites = DeepInfo ^ call_sites,

	(
		MaybeRecInfo = yes(RecInfo),
		RecInfo ^ role = inner_proc(OuterPredProcId)
	->
		OuterPredProcId = proc(PredId, ProcId)
	;
		PredProcId = proc(PredId, ProcId)
	),

	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	IsInInterface = is_proc_in_interface(ModuleInfo, PredId, ProcId),
	ProcStatic = proc_static_data(RttiProcLabel, FileName, LineNumber,
		IsInInterface, CallSites),
	ProcStaticConsId = deep_profiling_proc_static(RttiProcLabel),
	generate_unify(ProcStaticConsId, ProcStaticVar, BindProcStaticVarGoal),

	(
		MaybeActivationPtr = yes(ActivationPtr1),
		generate_call(ModuleInfo, "semi_call_port_code_sr", 4,
			[ProcStaticVar, TopCSD, MiddleCSD, ActivationPtr1],
			[TopCSD, MiddleCSD, ActivationPtr1], CallPortCode),
		generate_call(ModuleInfo, "semi_exit_port_code_sr", 3,
			[TopCSD, MiddleCSD, ActivationPtr1], [], ExitPortCode),
		generate_call(ModuleInfo, "semi_fail_port_code_sr", 3,
			[TopCSD, MiddleCSD, ActivationPtr1], no, failure,
			FailPortCode),
		NewNonlocals = list_to_set([TopCSD, MiddleCSD, ActivationPtr1])
	;
		MaybeActivationPtr = no,
		generate_call(ModuleInfo, "semi_call_port_code_ac", 3,
			[ProcStaticVar, TopCSD, MiddleCSD],
			[TopCSD, MiddleCSD], CallPortCode),
		generate_call(ModuleInfo, "semi_exit_port_code_ac", 2,
			[TopCSD, MiddleCSD], [], ExitPortCode),
		generate_call(ModuleInfo, "semi_fail_port_code_ac", 2,
			[TopCSD, MiddleCSD], no, failure, FailPortCode),
		NewNonlocals = list_to_set([TopCSD, MiddleCSD])
	),

	ExitConjGoalInfo = goal_info_add_nonlocals_make_impure(GoalInfo0,
		NewNonlocals),

	goal_info_add_feature(GoalInfo0, impure, GoalInfo),
	Goal = conj([
		BindProcStaticVarGoal,
		CallPortCode,
		disj([
			conj([
				TransformedGoal,
				ExitPortCode
			]) - ExitConjGoalInfo,
			FailPortCode
		]) - ExitConjGoalInfo
	]) - GoalInfo,
	proc_info_set_varset(Proc0, Vars, Proc1),
	proc_info_set_vartypes(Proc1, VarTypes, Proc2),
	proc_info_set_goal(Proc2, Goal, Proc).

:- pred transform_non_proc(module_info::in, pred_proc_id::in,
	proc_info::in, proc_info::out, maybe(layout_data)::out) is det.

transform_non_proc(ModuleInfo, PredProcId, Proc0, Proc, yes(ProcStatic)) :-
	proc_info_goal(Proc0, Goal0),
	Goal0 = _ - GoalInfo0,
	proc_info_varset(Proc0, Vars0),
	proc_info_vartypes(Proc0, VarTypes0),
	CPointerType = c_pointer_type,
	varset__new_named_var(Vars0, "TopCSD", TopCSD, Vars1),
	map__set(VarTypes0, TopCSD, CPointerType, VarTypes1),
	varset__new_named_var(Vars1, "MiddleCSD", MiddleCSD, Vars2),
	map__set(VarTypes1, MiddleCSD, CPointerType, VarTypes2),
	varset__new_named_var(Vars2, "ProcStatic", ProcStaticVar, Vars3),
	map__set(VarTypes2, ProcStaticVar, CPointerType, VarTypes3),
	module_info_globals(ModuleInfo, Globals),
	globals__lookup_bool_option(Globals, use_activation_counts,
		UseActivationCounts),
	(
		UseActivationCounts = no,
		varset__new_named_var(Vars3, "OldOutermost",
			OldOutermostProcDyn0, Vars4),
		map__set(VarTypes3, OldOutermostProcDyn0, CPointerType,
			VarTypes4),
		varset__new_named_var(Vars4, "NewOutermost",
			NewOutermostProcDyn, Vars5),
		map__set(VarTypes4, NewOutermostProcDyn, CPointerType,
			VarTypes5),
		MaybeOldActivationPtr = yes(OldOutermostProcDyn0)
	;
		UseActivationCounts = yes,
		varset__new_named_var(Vars3, "NewOutermost",
			NewOutermostProcDyn, Vars5),
		map__set(VarTypes3, NewOutermostProcDyn, CPointerType,
			VarTypes5),
		MaybeOldActivationPtr = no
	),
	proc_info_context(Proc0, Context),
	FileName = term__context_file(Context),
	LineNumber = term__context_line(Context),

	proc_info_get_maybe_deep_profile_info(Proc0, MaybeRecInfo),
	DeepInfo0 = deep_info(ModuleInfo, PredProcId, MiddleCSD,
		counter__init(0), [], Vars5, VarTypes5,
		FileName, MaybeRecInfo),

	transform_goal([], Goal0, TransformedGoal, _, DeepInfo0, DeepInfo),

	Vars = DeepInfo ^ vars,
	VarTypes = DeepInfo ^ var_types,
	CallSites = DeepInfo ^ call_sites,

	PredProcId = proc(PredId, ProcId),
	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	IsInInterface = is_proc_in_interface(ModuleInfo, PredId, ProcId),
	ProcStatic = proc_static_data(RttiProcLabel, FileName, LineNumber,
		IsInInterface, CallSites),
	ProcStaticConsId = deep_profiling_proc_static(RttiProcLabel),
	generate_unify(ProcStaticConsId, ProcStaticVar, BindProcStaticVarGoal),

	(
		MaybeOldActivationPtr = yes(OldOutermostProcDyn2),
		generate_call(ModuleInfo, "non_call_port_code_sr", 5,
			[ProcStaticVar, TopCSD, MiddleCSD,
			OldOutermostProcDyn2, NewOutermostProcDyn],
			[TopCSD, MiddleCSD,
				OldOutermostProcDyn2, NewOutermostProcDyn],
			CallPortCode),
		generate_call(ModuleInfo, "non_exit_port_code_sr", 3,
			[TopCSD, MiddleCSD, OldOutermostProcDyn2], [],
			ExitPortCode),
		generate_call(ModuleInfo, "non_fail_port_code_sr", 3,
			[TopCSD, MiddleCSD, OldOutermostProcDyn2], no,
			failure, FailPortCode),
		generate_call(ModuleInfo, "non_redo_port_code_sr", 2,
			[MiddleCSD, NewOutermostProcDyn], no,
			failure, RedoPortCode),
		NewNonlocals = list_to_set(
			[TopCSD, MiddleCSD, OldOutermostProcDyn2])
	;
		MaybeOldActivationPtr = no,
		generate_call(ModuleInfo, "non_call_port_code_ac", 4,
			[ProcStaticVar, TopCSD, MiddleCSD, NewOutermostProcDyn],
			[TopCSD, MiddleCSD, NewOutermostProcDyn],
			CallPortCode),
		generate_call(ModuleInfo, "non_exit_port_code_ac", 2,
			[TopCSD, MiddleCSD], [], ExitPortCode),
		generate_call(ModuleInfo, "non_fail_port_code_ac", 2,
			[TopCSD, MiddleCSD], no, failure, FailPortCode),
		generate_call(ModuleInfo, "non_redo_port_code_ac", 2,
			[MiddleCSD, NewOutermostProcDyn], no,
			failure, RedoPortCode),
		NewNonlocals = list_to_set([TopCSD, MiddleCSD])
	),

	% Even though the procedure has a model_non interface determinism,
	% the actual determinism of its original body goal may have been
	% at_most once. However, the exit/redo disjunction we insert into
	% the procedure body means that the procedure body does actually leave
	% a nondet stack frame when it succeeds, and its determinism must be
	% adjusted accordingly.
	goal_info_get_determinism(GoalInfo0, Detism0),
	determinism_components(Detism0, CanFail, _),
	determinism_components(Detism, CanFail, at_most_many),
	goal_info_set_determinism(GoalInfo0, Detism, GoalInfo1),

	ExitRedoNonLocals = set__union(NewNonlocals,
		list_to_set([NewOutermostProcDyn])),
	ExitRedoGoalInfo = impure_reachable_init_goal_info(ExitRedoNonLocals,
		multidet),

	CallExitRedoGoalInfo = goal_info_add_nonlocals_make_impure(GoalInfo1,
		ExitRedoNonLocals),

	goal_info_add_feature(GoalInfo1, impure, GoalInfo),
	Goal = conj([
		BindProcStaticVarGoal,
		CallPortCode,
		disj([
			conj([
				TransformedGoal,
				disj([
					ExitPortCode,
					RedoPortCode
				]) - ExitRedoGoalInfo
			]) - CallExitRedoGoalInfo,
			FailPortCode
		]) - CallExitRedoGoalInfo
	]) - GoalInfo,
	proc_info_set_varset(Proc0, Vars, Proc1),
	proc_info_set_vartypes(Proc1, VarTypes, Proc2),
	proc_info_set_goal(Proc2, Goal, Proc).

:- pred transform_inner_proc(module_info::in, pred_proc_id::in,
	proc_info::in, proc_info::out, maybe(layout_data)::out) is det.

transform_inner_proc(ModuleInfo, PredProcId, Proc0, Proc, no) :-
	proc_info_goal(Proc0, Goal0),
	Goal0 = _ - GoalInfo0,
	proc_info_varset(Proc0, Vars0),
	proc_info_vartypes(Proc0, VarTypes0),
	CPointerType = c_pointer_type,
	% MiddleCSD should be unused
	varset__new_named_var(Vars0, "MiddleCSD", MiddleCSD, Vars1),
	map__set(VarTypes0, MiddleCSD, CPointerType, VarTypes1),

	goal_info_get_context(GoalInfo0, Context),
	FileName = term__context_file(Context),

	proc_info_get_maybe_deep_profile_info(Proc0, MaybeRecInfo),
	DeepInfo0 = deep_info(ModuleInfo, PredProcId, MiddleCSD,
		counter__init(0), [], Vars1, VarTypes1,
		FileName, MaybeRecInfo),

	transform_goal([], Goal0, TransformedGoal, _, DeepInfo0, DeepInfo),

	Vars = DeepInfo ^ vars,
	VarTypes = DeepInfo ^ var_types,

	proc_info_set_varset(Proc0, Vars, Proc1),
	proc_info_set_vartypes(Proc1, VarTypes, Proc2),
	proc_info_set_goal(Proc2, TransformedGoal, Proc).

%-----------------------------------------------------------------------------%

:- func is_proc_in_interface(module_info, pred_id, proc_id) = bool.

is_proc_in_interface(ModuleInfo, PredId, _ProcId) = IsInInterface :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	(
		( pred_info_is_exported(PredInfo)
		; pred_info_is_pseudo_exported(PredInfo)
		)
	->
		IsInInterface = yes
	;
		IsInInterface = no
	).

%-----------------------------------------------------------------------------%

:- pred add_impurity_if_needed(bool::in, hlds_goal_info::in,
	hlds_goal_info::out) is det.

add_impurity_if_needed(AddedImpurity, GoalInfo0, GoalInfo) :-
	(
		AddedImpurity = no,
		GoalInfo = GoalInfo0
	;
		AddedImpurity = yes,
		goal_info_add_feature(GoalInfo0, impure, GoalInfo)
	).

%-----------------------------------------------------------------------------%

:- pred transform_goal(goal_path::in, hlds_goal::in, hlds_goal::out, bool::out,
	deep_info::in, deep_info::out) is det.

transform_goal(Path, conj(Goals0) - Info0, conj(Goals) - Info,
		AddedImpurity) -->
	transform_conj(0, Path, Goals0, Goals, AddedImpurity),
	{ add_impurity_if_needed(AddedImpurity, Info0, Info) }.

transform_goal(Path, par_conj(Goals0) - Info0,
		par_conj(Goals) - Info, AddedImpurity) -->
	transform_conj(0, Path, Goals0, Goals, AddedImpurity),
	{ add_impurity_if_needed(AddedImpurity, Info0, Info) }.

transform_goal(Path, switch(Var, CF, Cases0) - Info0,
		switch(Var, CF, Cases) - Info, AddedImpurity) -->
	transform_switch(list__length(Cases0), 0, Path, Cases0, Cases,
		AddedImpurity),
	{ add_impurity_if_needed(AddedImpurity, Info0, Info) }.

transform_goal(Path, disj(Goals0) - Info0, disj(Goals) - Info,
		AddedImpurity) -->
	transform_disj(0, Path, Goals0, Goals, AddedImpurity),
	{ add_impurity_if_needed(AddedImpurity, Info0, Info) }.

transform_goal(Path, not(Goal0) - Info0, not(Goal) - Info, AddedImpurity) -->
	transform_goal([neg | Path], Goal0, Goal, AddedImpurity),
	{ add_impurity_if_needed(AddedImpurity, Info0, Info) }.

transform_goal(Path, some(QVars, CanRemove, Goal0) - Info0,
		some(QVars, CanRemove, Goal) - Info, AddedImpurity) -->
	{ Goal0 = _ - InnerInfo },
	{ goal_info_get_determinism(Info0, OuterDetism) },
	{ goal_info_get_determinism(InnerInfo, InnerDetism) },
	{ InnerDetism = OuterDetism ->
		Info1 = Info0,
		MaybeCut = no_cut
	;
		% Given a subgoal containing both nondet code and impure code, 
		% determinism analysis will remove the `some' wrapped around
		% that subgoal if it is allowed to. If we get here, then the
		% subgoal inside the `some' contains nondet code, and the deep
		% profiling transformation will make it impure as well.

		goal_info_add_feature(Info0, keep_this_commit, Info1),
		MaybeCut = cut
	},
	transform_goal([exist(MaybeCut) | Path], Goal0, Goal, AddedImpurity),
	{ add_impurity_if_needed(AddedImpurity, Info1, Info) }.

transform_goal(Path, if_then_else(IVars, Cond0, Then0, Else0) - Info0,
		if_then_else(IVars, Cond, Then, Else) - Info,
		AddedImpurity) -->
	transform_goal([ite_cond | Path], Cond0, Cond, AddedImpurityC),
	transform_goal([ite_then | Path], Then0, Then, AddedImpurityT),
	transform_goal([ite_else | Path], Else0, Else, AddedImpurityE),
	{
		( AddedImpurityC = yes
		; AddedImpurityT = yes
		; AddedImpurityE = yes
		)
	->
		AddedImpurity = yes
	;
		AddedImpurity = no
	},
	{ add_impurity_if_needed(AddedImpurity, Info0, Info) }.

transform_goal(_, shorthand(_) - _, _, _) -->
	{ error("transform_goal/6: shorthand should have gone by now") }.

transform_goal(Path0, Goal0 - Info0, GoalAndInfo, AddedImpurity) -->
	{ Goal0 = foreign_proc(Attrs, _, _, _, _, _, _) },
	( { may_call_mercury(Attrs, may_call_mercury) } ->
		{ reverse(Path0, Path) },
		wrap_foreign_code(Path, Goal0 - Info0, GoalAndInfo),
		{ AddedImpurity = yes }
	;
		{ GoalAndInfo = Goal0 - Info0 },
		{ AddedImpurity = no }
	).

transform_goal(_Path, Goal - Info, Goal - Info, no) -->
	{ Goal = unify(_, _, _, _, _) }.

transform_goal(Path0, Goal0 - Info0, GoalAndInfo, yes) -->
	{ Goal0 = call(_, _, _, BuiltinState, _, _) },
	( { BuiltinState \= inline_builtin } ->
		{ reverse(Path0, Path) },
		wrap_call(Path, Goal0 - Info0, GoalAndInfo)
	;
		{ GoalAndInfo = Goal0 - Info0 }
	).

transform_goal(Path0, Goal0 - Info0, GoalAndInfo, yes) -->
	{ Goal0 = generic_call(_, _, _, _) },
	{ reverse(Path0, Path) },
	wrap_call(Path, Goal0 - Info0, GoalAndInfo).

:- pred transform_conj(int::in, goal_path::in,
	list(hlds_goal)::in, list(hlds_goal)::out, bool::out,
	deep_info::in, deep_info::out) is det.

transform_conj(_, _, [], [], no) --> [].
transform_conj(N, Path, [Goal0 | Goals0], [Goal | Goals], AddedImpurity) -->
	transform_goal([conj(N) | Path], Goal0, Goal, AddedImpurityFirst),
	transform_conj(N + 1, Path, Goals0, Goals, AddedImpurityLater),
	{ bool__or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity) }.

:- pred transform_disj(int::in, goal_path::in,
	list(hlds_goal)::in, list(hlds_goal)::out, bool::out,
	deep_info::in, deep_info::out) is det.

transform_disj(_, _, [], [], no) --> [].
transform_disj(N, Path, [Goal0 | Goals0], [Goal | Goals], AddedImpurity) -->
	transform_goal([disj(N) | Path], Goal0, Goal, AddedImpurityFirst),
	transform_disj(N + 1, Path, Goals0, Goals, AddedImpurityLater),
	{ bool__or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity) }.

:- pred transform_switch(int::in, int::in, goal_path::in,
	list(case)::in, list(case)::out, bool::out,
	deep_info::in, deep_info::out) is det.

transform_switch(_, _, _, [], [], no) --> [].
transform_switch(NumCases, N, Path, [case(Id, Goal0) | Goals0],
		[case(Id, Goal) | Goals], AddedImpurity) -->
	transform_goal([switch(NumCases, N) | Path], Goal0, Goal,
		AddedImpurityFirst),
	transform_switch(NumCases, N + 1, Path, Goals0, Goals,
		AddedImpurityLater),
	{ bool__or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity) }.

:- pred wrap_call(goal_path::in, hlds_goal::in, hlds_goal::out,
	deep_info::in, deep_info::out) is det.

wrap_call(GoalPath, Goal0, Goal, DeepInfo0, DeepInfo) :-
	Goal0 = GoalExpr - GoalInfo0,
	ModuleInfo = DeepInfo0 ^ module_info,
	goal_info_get_features(GoalInfo0, GoalFeatures),
	goal_info_remove_feature(GoalInfo0, tailcall, GoalInfo1),
	goal_info_add_feature(GoalInfo1, impure, GoalInfo),

	SiteNumCounter0 = DeepInfo0 ^ site_num_counter,
	counter__allocate(SiteNum, SiteNumCounter0, SiteNumCounter),
	varset__new_named_var(DeepInfo0 ^ vars, "SiteNum", SiteNumVar, Vars1),
	IntType = int_type,
	map__set(DeepInfo0 ^ var_types, SiteNumVar, IntType, VarTypes1),
	generate_unify(int_const(SiteNum), SiteNumVar, SiteNumVarGoal),
	DeepInfo1 = (((DeepInfo0 ^ vars := Vars1)
		^ var_types := VarTypes1)
		^ site_num_counter := SiteNumCounter),

	goal_info_get_context(GoalInfo0, Context),
	FileName0 = term__context_file(Context),
	LineNumber = term__context_line(Context),
	compress_filename(DeepInfo1, FileName0, FileName),
	classify_call(ModuleInfo, GoalExpr, CallKind),
	(
		CallKind = normal(PredProcId),
		( set__member(tailcall, GoalFeatures) ->
			generate_call(ModuleInfo, "prepare_for_tail_call", 1,
				[SiteNumVar], [], PrepareGoal)
		;
			generate_call(ModuleInfo, "prepare_for_normal_call", 1,
				[SiteNumVar], [], PrepareGoal)
		),
		PredProcId = proc(PredId, ProcId),
		TypeSubst = compute_type_subst(GoalExpr, DeepInfo1),
		MaybeRecInfo = DeepInfo1 ^ maybe_rec_info,
		(
			MaybeRecInfo = yes(RecInfo1),
			RecInfo1 ^ role = inner_proc(OuterPredProcId),
			PredProcId = DeepInfo1 ^ pred_proc_id
		->
			OuterPredProcId = proc(OuterPredId, OuterProcId),
			RttiProcLabel = rtti__make_proc_label(ModuleInfo,
				OuterPredId, OuterProcId)
		;
			MaybeRecInfo = yes(RecInfo2),
			RecInfo2 ^ role = outer_proc(InnerPredProcId),
			PredProcId = InnerPredProcId
		->
			OuterPredProcId = DeepInfo1 ^ pred_proc_id,
			OuterPredProcId = proc(OuterPredId, OuterProcId),
			RttiProcLabel = rtti__make_proc_label(ModuleInfo,
				OuterPredId, OuterProcId)
		;
			RttiProcLabel = rtti__make_proc_label(ModuleInfo,
				PredId, ProcId)
		),
		CallSite = normal_call(RttiProcLabel, TypeSubst,
			FileName, LineNumber, GoalPath),
		Goal1 = Goal0,
		DeepInfo3 = DeepInfo1
	;
		CallKind = special(_PredProcId, TypeInfoVar),
		generate_call(ModuleInfo, "prepare_for_special_call", 2,
			[SiteNumVar, TypeInfoVar], [], PrepareGoal),
		CallSite = special_call(FileName, LineNumber, GoalPath),
		Goal1 = Goal0,
		DeepInfo3 = DeepInfo1
	;
		CallKind = generic(Generic),
		(
			Generic = higher_order(ClosureVar, _, _),
			generate_call(ModuleInfo, "prepare_for_ho_call", 2,
				[SiteNumVar, ClosureVar], [], PrepareGoal),
			CallSite = higher_order_call(FileName, LineNumber,
				GoalPath),
			DeepInfo2 = DeepInfo1
		;
			Generic = class_method(TypeClassInfoVar, MethodNum,
				_, _),
			varset__new_named_var(DeepInfo1 ^ vars, "MethodNum",
				MethodNumVar, Vars2),
			map__set(DeepInfo1 ^ var_types, MethodNumVar, IntType,
				VarTypes2),
			generate_unify(int_const(MethodNum), MethodNumVar,
				MethodNumVarGoal),
			DeepInfo2 = ((DeepInfo1 ^ vars := Vars2)
				^ var_types := VarTypes2),
			generate_call(ModuleInfo, "prepare_for_method_call", 3,
				[SiteNumVar, TypeClassInfoVar, MethodNumVar],
				[], PrepareCallGoal),
			PrepareCallGoal = _ - PrepareCallGoalInfo,
			PrepareGoal = conj([
				MethodNumVarGoal,
				PrepareCallGoal
			]) - PrepareCallGoalInfo,
			CallSite = method_call(FileName, LineNumber, GoalPath)
		;
			Generic = aditi_builtin(_, _),
			error("deep profiling and aditi do not mix")
		),
		goal_info_get_code_model(GoalInfo0, GoalCodeModel),
		module_info_globals(ModuleInfo, Globals),
		globals__lookup_bool_option(Globals,
			use_zeroing_for_ho_cycles, UseZeroing),
		( UseZeroing = yes ->
			transform_higher_order_call(Globals, GoalCodeModel,
				Goal0, Goal1, DeepInfo2, DeepInfo3)
		;
			Goal1 = Goal0,
			DeepInfo3 = DeepInfo2
		)
	),
	DeepInfo4 = DeepInfo3 ^ call_sites :=
		(DeepInfo3 ^ call_sites ++ [CallSite]),
	(
		set__member(tailcall, GoalFeatures),
		DeepInfo4 ^ maybe_rec_info = yes(RecInfo),
		RecInfo ^ role = outer_proc(_)
	->
		VisSCC = RecInfo ^ visible_scc,
		MiddleCSD = DeepInfo4 ^ current_csd,
		(
			VisSCC = [],
			CallGoals = [],
			ExitGoals = [],
			FailGoals = [],
			SaveRestoreVars = [],
			DeepInfo = DeepInfo4
		;
			VisSCC = [SCCmember],
			generate_recursion_counter_saves_and_restores(
				SCCmember ^ rec_call_sites, MiddleCSD,
				CallGoals, ExitGoals, FailGoals,
				SaveRestoreVars, DeepInfo4, DeepInfo)
		;
			VisSCC = [_, _ | _],
			error("wrap_call: multi-procedure SCCs not yet implemented")
		),

		goal_info_get_code_model(GoalInfo0, CodeModel),
		( CodeModel = model_det ->
			list__condense([
				CallGoals,
				[SiteNumVarGoal, PrepareGoal, Goal1],
				ExitGoals
			], Goals),
			Goal = conj(Goals) - GoalInfo
		;
			
			ExtraVars = list_to_set([MiddleCSD | SaveRestoreVars]),
			WrappedGoalGoalInfo =
				goal_info_add_nonlocals_make_impure(GoalInfo,
					ExtraVars),

			ReturnFailsGoalInfo =
				impure_unreachable_init_goal_info(
					ExtraVars, failure),

			FailGoalInfo = fail_goal_info,
			FailGoal = disj([]) - FailGoalInfo,

			list__append(FailGoals, [FailGoal], FailGoalsAndFail),

			list__condense([
				CallGoals,
				[disj([
					conj([
						SiteNumVarGoal,
						PrepareGoal,
						Goal1 |
						ExitGoals
					]) - WrappedGoalGoalInfo,
					conj(
						FailGoalsAndFail
					) - ReturnFailsGoalInfo
				]) - WrappedGoalGoalInfo]
			], Goals),
			Goal = conj(Goals) - GoalInfo
		)
	;
		Goal = conj([
			SiteNumVarGoal,
			PrepareGoal,
			Goal1
		]) - GoalInfo,
		DeepInfo = DeepInfo4
	).

:- pred transform_higher_order_call(globals::in, code_model::in,
	hlds_goal::in, hlds_goal::out, deep_info::in, deep_info::out) is det.

transform_higher_order_call(Globals, CodeModel, Goal0, Goal,
		DeepInfo0, DeepInfo) :-
	Vars0 = DeepInfo0 ^ vars,
	VarTypes0 = DeepInfo0 ^ var_types, 

	CPointerType = c_pointer_type,
	varset__new_named_var(Vars0, "SavedPtr", SavedPtrVar, Vars1),
	map__set(VarTypes0, SavedPtrVar, CPointerType, VarTypes1),

	globals__lookup_bool_option(Globals, use_activation_counts,
		UseActivationCounts),
	(
		UseActivationCounts = yes,

		IntType = int_type,
		varset__new_named_var(Vars1, "SavedCounter", SavedCountVar,
			Vars),
		map__set(VarTypes1, SavedCountVar, IntType, VarTypes),

		DeepInfo1 = DeepInfo0 ^ vars := Vars,
		DeepInfo = DeepInfo1 ^ var_types := VarTypes,
		ExtraNonLocals = set__list_to_set(
			[SavedCountVar, SavedPtrVar]),

		generate_call(DeepInfo ^ module_info,
			"save_and_zero_activation_info_ac", 2,
			[SavedCountVar, SavedPtrVar],
			[SavedCountVar, SavedPtrVar], SaveStuff),
		generate_call(DeepInfo ^ module_info,
			"reset_activation_info_ac", 2,
			[SavedCountVar, SavedPtrVar], [], RestoreStuff),
		generate_call(DeepInfo ^ module_info,
			"rezero_activation_info_ac", 0,
			[], [], ReZeroStuff)
	;
		UseActivationCounts = no,

		DeepInfo1 = DeepInfo0 ^ vars := Vars1,
		DeepInfo = DeepInfo1 ^ var_types := VarTypes1,
		ExtraNonLocals = set__list_to_set([SavedPtrVar]),

		generate_call(DeepInfo ^ module_info,
			"save_and_zero_activation_info_sr", 1,
			[SavedPtrVar], [SavedPtrVar], SaveStuff),
		generate_call(DeepInfo ^ module_info,
			"reset_activation_info_sr", 1,
			[SavedPtrVar], [], RestoreStuff),
		generate_call(DeepInfo ^ module_info,
			"rezero_activation_info_sr", 0,
			[], [], ReZeroStuff)
	),

	Goal0 = _ - GoalInfo0,
	ExtGoalInfo = goal_info_add_nonlocals_make_impure(GoalInfo0,
		ExtraNonLocals),

	% XXX We should build up NoBindExtGoalInfo from scratch.
	instmap_delta_init_reachable(EmptyDelta),
	goal_info_set_instmap_delta(ExtGoalInfo, EmptyDelta,
		NoBindExtGoalInfo),

	FailGoalInfo = fail_goal_info,
	FailGoal = disj([]) - FailGoalInfo,

	RestoreFailGoalInfo = impure_unreachable_init_goal_info(ExtraNonLocals,
		failure),

	RezeroFailGoalInfo = impure_unreachable_init_goal_info(set__init,
		failure),

	goal_info_add_feature(GoalInfo0, impure, GoalInfo),
	(
		CodeModel = model_det,
		Goal = conj([
			SaveStuff,
			Goal0,
			RestoreStuff
		]) - GoalInfo
	;
		CodeModel = model_semi,
		Goal = conj([
			SaveStuff,
			disj([
				conj([
					Goal0,
					RestoreStuff
				]) - ExtGoalInfo,
				conj([
					RestoreStuff,
					FailGoal
				]) - RestoreFailGoalInfo
			]) - ExtGoalInfo
		]) - GoalInfo
	;
		CodeModel = model_non,
		Goal = conj([
			SaveStuff,
			disj([
				conj([
					Goal0,
					disj([
						RestoreStuff,
						conj([
							ReZeroStuff,
							FailGoal
						]) - RezeroFailGoalInfo
					]) - NoBindExtGoalInfo
				]) - ExtGoalInfo,
				conj([
					RestoreStuff,
					FailGoal
				]) - RestoreFailGoalInfo
			]) - ExtGoalInfo
		]) - GoalInfo
	).

:- pred wrap_foreign_code(goal_path::in, hlds_goal::in, hlds_goal::out,
	deep_info::in, deep_info::out) is det.

wrap_foreign_code(GoalPath, Goal0, Goal, DeepInfo0, DeepInfo) :-
	Goal0 = _ - GoalInfo0,
	ModuleInfo = DeepInfo0 ^ module_info,

	SiteNumCounter0 = DeepInfo0 ^ site_num_counter,
	counter__allocate(SiteNum, SiteNumCounter0, SiteNumCounter),
	varset__new_named_var(DeepInfo0 ^ vars, "SiteNum", SiteNumVar, Vars),
	map__set(DeepInfo0 ^ var_types, SiteNumVar, int_type, VarTypes),
	generate_unify(int_const(SiteNum), SiteNumVar, SiteNumVarGoal),

	generate_call(ModuleInfo, "prepare_for_callback", 1,
		[SiteNumVar], [], PrepareGoal),

	goal_info_get_context(GoalInfo0, Context),
	LineNumber = term__context_line(Context),
	FileName0 = term__context_file(Context),
	compress_filename(DeepInfo0, FileName0, FileName),
	CallSite = callback(FileName, LineNumber, GoalPath),

	goal_info_add_feature(GoalInfo0, impure, GoalInfo),
	Goal = conj([
		SiteNumVarGoal,
		PrepareGoal,
		Goal0
	]) - GoalInfo,
	DeepInfo = ((((DeepInfo0 ^ site_num_counter := SiteNumCounter)
		^ vars := Vars)
		^ var_types := VarTypes)
		^ call_sites := DeepInfo0 ^ call_sites ++ [CallSite]).

:- pred compress_filename(deep_info::in, string::in, string::out) is det.

compress_filename(Deep, FileName0, FileName) :-
	( FileName0 = Deep ^ proc_filename ->
		FileName = ""
	;
		FileName = FileName0
	).

:- type call_class
			% For normal first order calls
	--->	normal(pred_proc_id)
			% For calls to unify/2 and compare/3
	;	special(pred_proc_id, prog_var)
			% For higher order and typeclass method calls
	;	generic(generic_call).

:- pred classify_call(module_info::in, hlds_goal_expr::in,
	call_class::out) is det.

classify_call(ModuleInfo, Expr, Class) :-
	( Expr = call(PredId, ProcId, Args, _, _, _) ->
		module_info_get_predicate_table(ModuleInfo, PredTable),
		mercury_public_builtin_module(MercuryBuiltin),
		(
			predicate_table_search_pred_m_n_a(PredTable,
				MercuryBuiltin, "unify", 2, [PredId]),
			Args = [TypeInfoVar | _]
		->
			Class = special(proc(PredId, ProcId), TypeInfoVar)
		;
			predicate_table_search_pred_m_n_a(PredTable,
				MercuryBuiltin, "compare", 3, [PredId]),
			Args = [TypeInfoVar | _]
		->
			Class = special(proc(PredId, ProcId), TypeInfoVar)
		;
			Class = normal(proc(PredId, ProcId))
		)
	; Expr = generic_call(Generic, _, _, _) ->
		Class = generic(Generic)
	;
		error("unexpected goal type in classify_call/2")
	).

:- func compute_type_subst(hlds_goal_expr, deep_info) = string.

% XXX we don't compute type substitution strings yet.
compute_type_subst(_, _) = "".

	% The maximum value of N for which save_recursion_depth_N,
	% restore_recursion_depth_exit_N and restore_recursion_depth_fail_N
	% exist in library/profiling_builtin.m.
:- func max_save_restore_vector_size = int.

max_save_restore_vector_size = 9.

:- pred generate_recursion_counter_saves_and_restores(list(int)::in,
	prog_var::in, list(hlds_goal)::out, list(hlds_goal)::out,
	list(hlds_goal)::out, list(prog_var)::out,
	deep_info::in, deep_info::out) is det.

generate_recursion_counter_saves_and_restores(CSNs, CSDVar, CallGoals,
		ExitGoals, FailGoals, ExtraVars, DeepInfo0, DeepInfo) :-
	list__chunk(CSNs, max_save_restore_vector_size, CSNChunks),
	generate_recursion_counter_saves_and_restores_2(CSNChunks, CSDVar,
		CallGoals, ExitGoals, FailGoals, ExtraVars,
		DeepInfo0, DeepInfo).

:- pred generate_recursion_counter_saves_and_restores_2(list(list(int))::in,
	prog_var::in, list(hlds_goal)::out, list(hlds_goal)::out,
	list(hlds_goal)::out, list(prog_var)::out,
	deep_info::in, deep_info::out) is det.

generate_recursion_counter_saves_and_restores_2([], _, [], [], [], [],
		DeepInfo, DeepInfo).
generate_recursion_counter_saves_and_restores_2([Chunk | Chunks], CSDVar,
		CallGoals, ExitGoals, FailGoals, ExtraVars,
		DeepInfo0, DeepInfo) :-

	list__map_foldl(generate_depth_var, Chunk, DepthVars,
		DeepInfo0, DeepInfo1),

	% We generate three separate variables to hold the constant CSN vector.
	% If we used only one, the code generator would have to save its value
	% on the stack when we enter the disjunction that wraps the goal.
	list__length(Chunk, Length),
	generate_csn_vector(Length, Chunk, CallVars1, CallGoals1, CallCellVar,
		DeepInfo1, DeepInfo2),
	generate_csn_vector(Length, Chunk, ExitVars1, ExitGoals1, ExitCellVar,
		DeepInfo2, DeepInfo3),
	generate_csn_vector(Length, Chunk, FailVars1, FailGoals1, FailCellVar,
		DeepInfo3, DeepInfo4),
	list__condense([CallVars1, ExitVars1, FailVars1], ExtraVars1),

	CallPredName = string__format("save_recursion_depth_%d",
		[i(Length)]),
	ExitPredName = string__format("restore_recursion_depth_exit_%d",
		[i(Length)]),
	FailPredName = string__format("restore_recursion_depth_fail_%d",
		[i(Length)]),
	ModuleInfo = DeepInfo4 ^ module_info,
	generate_call(ModuleInfo, CallPredName, Length + 2,
		[CSDVar, CallCellVar | DepthVars], DepthVars, CallCellGoal),
	generate_call(ModuleInfo, ExitPredName, Length + 2,
		[CSDVar, ExitCellVar | DepthVars], [], ExitCellGoal),
	generate_call(ModuleInfo, FailPredName, Length + 2,
		[CSDVar, FailCellVar | DepthVars], [], FailCellGoal),

	generate_recursion_counter_saves_and_restores_2(Chunks, CSDVar,
		CallGoals2, ExitGoals2, FailGoals2, ExtraVars2,
		DeepInfo4, DeepInfo),

	list__append(CallGoals1, [CallCellGoal | CallGoals2], CallGoals),
	list__append(ExitGoals1, [ExitCellGoal | ExitGoals2], ExitGoals),
	list__append(FailGoals1, [FailCellGoal | FailGoals2], FailGoals),
	list__append(ExtraVars1, ExtraVars2, ExtraVars).

:- pred generate_depth_var(int::in, prog_var::out,
	deep_info::in, deep_info::out) is det.

generate_depth_var(CSN, DepthVar, DeepInfo0, DeepInfo) :-
	Vars0 = DeepInfo0 ^ vars,
	VarTypes0 = DeepInfo0 ^ var_types,
	IntType = int_type,
	VarName = string__format("Depth%d", [i(CSN)]),
	varset__new_named_var(Vars0, VarName, DepthVar, Vars),
	map__set(VarTypes0, DepthVar, IntType, VarTypes),
	DeepInfo = (DeepInfo0 ^ vars := Vars) ^ var_types := VarTypes.

:- pred generate_csn_vector(int::in, list(int)::in, list(prog_var)::out,
	list(hlds_goal)::out, prog_var::out,
	deep_info::in, deep_info::out) is det.

generate_csn_vector(Length, CSNs, CSNVars, UnifyGoals, CellVar,
		DeepInfo0, DeepInfo) :-
	( CSNs = [CSN] ->
		generate_single_csn_unify(CSN, CSNVar - UnifyGoal,
			DeepInfo0, DeepInfo),
		CSNVars = [CSNVar],
		UnifyGoals = [UnifyGoal],
		CellVar = CSNVar
	;
		require(Length =< max_save_restore_vector_size,
			"generate_csn_vector_unifies: too long"),
		list__map_foldl(generate_single_csn_unify, CSNs, CSNVarsGoals,
			DeepInfo0, DeepInfo1),
		InnerVars = assoc_list__keys(CSNVarsGoals),
		InnerGoals = assoc_list__values(CSNVarsGoals),
		generate_csn_vector_cell(Length, InnerVars, CellVar, CellGoal,
			DeepInfo1, DeepInfo),
		CSNVars = [CellVar | InnerVars],
		UnifyGoals = list__append(InnerGoals, [CellGoal])
	).

:- pred generate_csn_vector_cell(int::in, list(prog_var)::in,
	prog_var::out, hlds_goal::out, deep_info::in, deep_info::out) is det.

generate_csn_vector_cell(Length, CSNVars, CellVar, CellGoal,
		DeepInfo0, DeepInfo) :-
	Vars0 = DeepInfo0 ^ vars,
	VarTypes0 = DeepInfo0 ^ var_types,
	varset__new_named_var(Vars0, "CSNCell", CellVar, Vars),
	mercury_profiling_builtin_module(ProfilingBuiltin),
	CellTypeName = string__format("call_site_nums_%d", [i(Length)]),
	CellTypeId = qualified(ProfilingBuiltin, CellTypeName) - Length,
	construct_type(CellTypeId, [], CellType),
	map__set(VarTypes0, CellVar, CellType, VarTypes),
	DeepInfo = (DeepInfo0 ^ vars := Vars) ^ var_types := VarTypes,
	ConsId = cons(qualified(ProfilingBuiltin, CellTypeName), Length),
	generate_cell_unify(Length, ConsId, CSNVars, CellVar, CellGoal).

:- pred generate_single_csn_unify(int::in,
	pair(prog_var, hlds_goal)::out, deep_info::in, deep_info::out) is det.

generate_single_csn_unify(CSN, CSNVar - UnifyGoal, DeepInfo0, DeepInfo) :-
	Vars0 = DeepInfo0 ^ vars,
	VarTypes0 = DeepInfo0 ^ var_types,
	VarName = string__format("CSN%d", [i(CSN)]),
	varset__new_named_var(Vars0, VarName, CSNVar, Vars),
	map__set(VarTypes0, CSNVar, int_type, VarTypes),
	DeepInfo = (DeepInfo0 ^ vars := Vars) ^ var_types := VarTypes,
	generate_unify(int_const(CSN), CSNVar, UnifyGoal).

:- pred generate_call(module_info::in, string::in, int::in,
	list(prog_var)::in, list(prog_var)::in, hlds_goal::out) is det.

generate_call(ModuleInfo, Name, Arity, ArgVars, OutputVars, Goal) :-
	generate_call(ModuleInfo, Name, Arity, ArgVars, yes(OutputVars),
		det, Goal).

:- pred generate_call(module_info::in, string::in, int::in, list(prog_var)::in,
	maybe(list(prog_var))::in, determinism::in, hlds_goal::out) is det.

generate_call(ModuleInfo, Name, Arity, ArgVars, MaybeOutputVars, Detism,
		Goal) :-
	get_deep_profile_builtin_ppid(ModuleInfo, Name, Arity, PredId, ProcId),
	NonLocals = list_to_set(ArgVars),
	Ground = ground(shared, none),
	(
		MaybeOutputVars = yes(OutputVars),
		map((pred(V::in, P::out) is det :-
			P = V - Ground
		), OutputVars, OutputInsts),
		instmap_delta_from_assoc_list(OutputInsts, InstMapDelta)
	;
		MaybeOutputVars = no,
		instmap_delta_init_unreachable(InstMapDelta)
	),
	GoalInfo = impure_init_goal_info(NonLocals, InstMapDelta, Detism),
	Goal = call(PredId, ProcId, ArgVars, not_builtin, no,
		unqualified(Name)) - GoalInfo.

:- pred generate_unify(cons_id::in, prog_var::in, hlds_goal::out) is det.

generate_unify(ConsId, Var, Goal) :-
	Ground = ground(shared, none),
	NonLocals = set__make_singleton_set(Var),
	instmap_delta_from_assoc_list([Var - ground(shared, none)],
		InstMapDelta),
	Determinism = det,
	goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo),
	Goal = unify(Var, functor(ConsId, []),
    		(free -> Ground) - (Ground -> Ground),
		construct(Var, ConsId, [], [], construct_statically([]),
			cell_is_shared, no),
		unify_context(explicit, [])) - GoalInfo.

:- pred generate_cell_unify(int::in, cons_id::in, list(prog_var)::in,
	prog_var::in, hlds_goal::out) is det.

generate_cell_unify(Length, ConsId, Args, Var, Goal) :-
	Ground = ground(shared, none),
	NonLocals = set__list_to_set([Var | Args]),
	instmap_delta_from_assoc_list([Var - Ground], InstMapDelta),
	Determinism = det,
	goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo),
	ArgMode = ((free - Ground) -> (Ground - Ground)),
	list__duplicate(Length, ArgMode, ArgModes),
	Goal = unify(Var, functor(ConsId, Args),
    		(free -> Ground) - (Ground -> Ground),
		construct(Var, ConsId, Args, ArgModes,
			construct_statically([]), cell_is_shared, no),
		unify_context(explicit, [])) - GoalInfo.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred get_deep_profile_builtin_ppid(module_info::in, string::in, int::in,
	pred_id::out, proc_id::out) is det.

get_deep_profile_builtin_ppid(ModuleInfo, Name, Arity, PredId, ProcId) :-
	mercury_profiling_builtin_module(ModuleName),
	module_info_get_predicate_table(ModuleInfo, PredTable),
	(
		predicate_table_search_pred_m_n_a(PredTable,
			ModuleName, Name, Arity, PredIds)
	->
		(
			PredIds = [],
			error("get_deep_profile_builtin_ppid: no pred_id")
		;
			PredIds = [PredId],
			predicate_table_get_preds(PredTable, Preds),
			lookup(Preds, PredId, PredInfo),
			pred_info_procids(PredInfo, ProcIds),
			(
				ProcIds = [],
				error("get_deep_profile_builtin_ppid: no proc_id")
			;
				ProcIds = [ProcId]
			;
				ProcIds = [_, _ | _],
				error("get_deep_profile_builtin_ppid: proc_id not unique")
			)
		;
			PredIds = [_, _ | _],
			error("get_deep_profile_builtin_ppid: pred_id not unique")
		)
	;
		format("couldn't find pred_id for `%s'/%d",
			[s(Name), i(Arity)], Msg),
		error(Msg)
	).

%-----------------------------------------------------------------------------%

:- func impure_init_goal_info(set(prog_var), instmap_delta, determinism)
	= hlds_goal_info.

impure_init_goal_info(NonLocals, InstMapDelta, Determinism) = GoalInfo :-
	goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo0),
	goal_info_add_feature(GoalInfo0, impure, GoalInfo).

:- func impure_reachable_init_goal_info(set(prog_var), determinism)
	= hlds_goal_info.

impure_reachable_init_goal_info(NonLocals, Determinism) = GoalInfo :-
	instmap_delta_init_reachable(InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo0),
	goal_info_add_feature(GoalInfo0, impure, GoalInfo).

:- func impure_unreachable_init_goal_info(set(prog_var), determinism)
	= hlds_goal_info.

impure_unreachable_init_goal_info(NonLocals, Determinism) = GoalInfo :-
	instmap_delta_init_unreachable(InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, Determinism, GoalInfo0),
	goal_info_add_feature(GoalInfo0, impure, GoalInfo).

:- func goal_info_add_nonlocals_make_impure(hlds_goal_info, set(prog_var))
	= hlds_goal_info.

goal_info_add_nonlocals_make_impure(GoalInfo0, NewNonLocals) = GoalInfo :-
	goal_info_get_nonlocals(GoalInfo0, NonLocals0),
	NonLocals = set__union(NonLocals0, NewNonLocals),
	goal_info_set_nonlocals(GoalInfo0, NonLocals, GoalInfo1),
	goal_info_add_feature(GoalInfo1, impure, GoalInfo).

:- func fail_goal_info = hlds_goal_info.

fail_goal_info = GoalInfo :-
	instmap_delta_init_unreachable(InstMapDelta),
	goal_info_init(set__init, InstMapDelta, failure, GoalInfo).

%-----------------------------------------------------------------------------%
