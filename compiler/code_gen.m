%---------------------------------------------------------------------------%
% Copyright (C) 1994-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Code generation - convert from HLDS to LLDS.
%
% Main authors: conway, zs.
%
% The two main tasks of this module are
%
% 1	to look after the aspects of generating code for a procedure
%	that do not involve generating code for a specific goal, and
%
% 2	to provide a generic predicate that can be called from anywhere in
%	the code generator to generate code for a goal.
%
% Code_gen forwards most of the actual construction of code for particular
% goals to other modules. The generation of code for unifications is done
% by unify_gen, for calls, higher-order calls and method calls by call_gen,
% for commits by commit_gen, for if-then-elses and negations by ite_gen,
% for switches by switch_gen and its subsidiary modules, for disjunctions
% by disj_gen, and for pragma_c_codes by pragma_c_gen. The only kind of goal
% handled directly by code_gen is the conjunction.
%
%---------------------------------------------------------------------------%

:- module ll_backend__code_gen.

:- interface.

:- import_module hlds__code_model.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module ll_backend__code_info.
:- import_module ll_backend__global_data.
:- import_module ll_backend__llds.

:- import_module list, io.

		% Translate a HLDS module to LLDS.

:- pred generate_code(module_info::in, global_data::in, global_data::out,
	list(c_procedure)::out, io::di, io::uo) is det.

		% Translate a HLDS procedure to LLDS, threading through
		% the data structure that records information about layout
		% structures.

:- pred generate_proc_code(pred_info::in, proc_info::in,
	proc_id::in, pred_id::in, module_info::in,
	global_data::in, global_data::out, c_procedure::out) is det.

		% Translate a HLDS goal to LLDS.

:- pred code_gen__generate_goal(code_model::in, hlds_goal::in, code_tree::out,
	code_info::in, code_info::out) is det.

		% Return the message that identifies the procedure to pass to
		% the incr_sp_push_msg macro in the generated C code.
:- func code_gen__push_msg(module_info, pred_id, proc_id) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Parse tree modules
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

% HLDS modules
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_llds.
:- import_module hlds__hlds_out.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module hlds__special_pred.

% LLDS code generator modules.
:- import_module ll_backend__call_gen.
:- import_module ll_backend__code_aux.
:- import_module ll_backend__code_util.
:- import_module ll_backend__commit_gen.
:- import_module ll_backend__continuation_info.
:- import_module ll_backend__disj_gen.
:- import_module ll_backend__ite_gen.
:- import_module ll_backend__layout.
:- import_module ll_backend__llds_out.
:- import_module ll_backend__middle_rec.
:- import_module ll_backend__par_conj_gen.
:- import_module ll_backend__pragma_c_gen.
:- import_module ll_backend__switch_gen.
:- import_module ll_backend__trace.
:- import_module ll_backend__unify_gen.

% Misc compiler modules
:- import_module backend_libs__builtin_ops.
:- import_module backend_libs__proc_label.
:- import_module backend_libs__rtti.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module libs__trace_params.

% Standard library modules
:- import_module bool, char, int, string.
:- import_module map, assoc_list, set, term, libs__tree, std_util, require.
:- import_module counter, varset.

%---------------------------------------------------------------------------%

generate_code(ModuleInfo0, !GlobalData, Procedures, !IO) :-
		% get a list of all the predicate ids
		% for which we are going to generate code.
	module_info_predids(ModuleInfo0, PredIds),
		% now generate the code for each predicate
	generate_pred_list_code(ModuleInfo0, !GlobalData, PredIds,
		Procedures, !IO).

	% Translate a list of HLDS predicates to LLDS.

:- pred generate_pred_list_code(module_info::in,
	global_data::in, global_data::out,
	list(pred_id)::in, list(c_procedure)::out,
	io::di, io::uo) is det.

generate_pred_list_code(_ModuleInfo, !GlobalData, [], [], !IO).
generate_pred_list_code(ModuleInfo, !GlobalData, [PredId | PredIds],
		Predicates, !IO) :-
	generate_maybe_pred_code(ModuleInfo, !GlobalData, PredId,
		Predicates0, !IO),
	generate_pred_list_code(ModuleInfo, !GlobalData, PredIds,
		Predicates1, !IO),
	list__append(Predicates0, Predicates1, Predicates).

:- pred generate_maybe_pred_code(module_info::in,
	global_data::in, global_data::out, pred_id::in,
	list(c_procedure)::out, io::di, io::uo) is det.

	% Note that some of the logic of generate_maybe_pred_code is duplicated
	% by mercury_compile__backend_pass_by_preds, so modifications here may
	% also need to be repeated there.

generate_maybe_pred_code(ModuleInfo, !GlobalData, PredId, Predicates, !IO) :-
	module_info_preds(ModuleInfo, PredInfos),
		% get the pred_info structure for this predicate
	map__lookup(PredInfos, PredId, PredInfo),
		% extract a list of all the procedure ids for this
		% predicate and generate code for them
	ProcIds = pred_info_non_imported_procids(PredInfo),
	(
		( ProcIds = []
		; hlds_pred__pred_info_is_aditi_relation(PredInfo)
		)
	->
		Predicates = []
	;
		module_info_globals(ModuleInfo, Globals),
		globals__lookup_bool_option(Globals, very_verbose,
			VeryVerbose),
		( VeryVerbose = yes ->
			io__write_string("% Generating code for ", !IO),
			hlds_out__write_pred_id(ModuleInfo, PredId, !IO),
			io__write_string("\n", !IO),
			globals__lookup_bool_option(Globals,
				statistics, Statistics),
			maybe_report_stats(Statistics, !IO)
		;
			true
		),
		generate_pred_code(ModuleInfo, !GlobalData,
			PredId, PredInfo, ProcIds, Predicates)
	).

	% Translate a HLDS predicate to LLDS.

:- pred generate_pred_code(module_info::in, global_data::in, global_data::out,
	pred_id::in, pred_info::in, list(proc_id)::in, list(c_procedure)::out)
	is det.

generate_pred_code(ModuleInfo, !GlobalData, PredId, PredInfo, ProcIds, Code) :-
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo,
		!GlobalData, [], Code).

	% Translate all the procedures of a HLDS predicate to LLDS.

:- pred generate_proc_list_code(list(proc_id)::in, pred_id::in, pred_info::in,
	module_info::in, global_data::in, global_data::out,
	list(c_procedure)::in, list(c_procedure)::out) is det.

generate_proc_list_code([], _PredId, _PredInfo, _ModuleInfo,
		!GlobalData, !Procs).
generate_proc_list_code([ProcId | ProcIds], PredId, PredInfo, ModuleInfo0,
		!GlobalData, !Procs) :-
	pred_info_procedures(PredInfo, ProcInfos),
	map__lookup(ProcInfos, ProcId, ProcInfo),
	generate_proc_code(PredInfo, ProcInfo, ProcId, PredId, ModuleInfo0,
		!GlobalData, Proc),
	!:Procs = [Proc | !.Procs],
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		!GlobalData, !Procs).

%---------------------------------------------------------------------------%

	% Values of this type hold information about stack frames that is
	% generated when generating prologs and is used in generating epilogs
	% and when massaging the code generated for the procedure.

:- type frame_info
	--->	frame(
			int,		% Number of slots in frame.

			maybe(int),	% Slot number of succip if succip is
					% present in a general slot.

			bool		% Is this the frame of a model_non
					% proc defined via pragma C code?
		).

%---------------------------------------------------------------------------%

generate_proc_code(PredInfo, ProcInfo, ProcId, PredId, ModuleInfo0,
		!GlobalData, Proc) :-

		% The module_info with a modified trace level is discarded
		% on return from generate_proc_code.
	maybe_set_trace_level(PredInfo, ModuleInfo0, ModuleInfo),

	proc_info_interface_determinism(ProcInfo, Detism),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	proc_info_goal(ProcInfo, Goal),
	Goal = _ - GoalInfo,
	goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
	(
		MaybeFollowVars = yes(FollowVars)
	;
		MaybeFollowVars = no,
		map__init(FollowVarsMap),
		FollowVars = abs_follow_vars(FollowVarsMap, 1)
	),
	module_info_globals(ModuleInfo, Globals),
	continuation_info__basic_stack_layout_for_proc(PredInfo, Globals,
		BasicStackLayout, ForceProcId),
	( BasicStackLayout = yes ->
		SaveSuccip = yes
	;
		SaveSuccip = no
	),

		% Initialise the code_info structure. Generate_category_code
		% below will use the returned OutsideResumePoint as the
		% entry to the code that handles the failure of the procedure,
		% if such code is needed. It is never needed for model_det
		% procedures, always needed for model_semi procedures, and
		% needed for model_non procedures only if we are doing
		% execution tracing.
	global_data_get_static_cell_info(!.GlobalData, StaticCellInfo0),
	code_info__init(SaveSuccip, Globals, PredId, ProcId, PredInfo,
		ProcInfo, FollowVars, ModuleInfo, StaticCellInfo0,
		OutsideResumePoint, TraceSlotInfo, CodeInfo0),

		% Generate code for the procedure.
	generate_category_code(CodeModel, Goal, OutsideResumePoint,
		TraceSlotInfo, CodeTree, MaybeTraceCallLabel, FrameInfo,
		CodeInfo0, CodeInfo),
	code_info__get_max_reg_in_use_at_trace(CodeInfo, MaxTraceReg),
	code_info__get_static_cell_info(CodeInfo, StaticCellInfo),
	global_data_set_static_cell_info(StaticCellInfo, !GlobalData),

	globals__get_trace_level(Globals, TraceLevel),
	code_info__get_created_temp_frame(CodeInfo, CreatedTempFrame),

	EffTraceIsNone = eff_trace_level_is_none(PredInfo, ProcInfo,
		TraceLevel),
	(
		EffTraceIsNone = no,
		CreatedTempFrame = yes,
		CodeModel \= model_non
	->
			% If tracing is enabled, the procedure lives on
			% the det stack and the code created any temporary
			% nondet stack frames, then we must have reserved a
			% stack slot for storing the value of maxfr; if we
			% didn't, a retry command in the debugger from a point
			% in the middle of this procedure will do the wrong
			% thing.
		proc_info_get_need_maxfr_slot(ProcInfo, HaveMaxfrSlot),
		require(unify(HaveMaxfrSlot, yes),
			"should have reserved a slot for maxfr, but didn't")
	;
		true
	),

		% Turn the code tree into a list.
	tree__flatten(CodeTree, FragmentList),
		% Now the code is a list of code fragments (== list(instr)),
		% so we need to do a level of unwinding to get a flat list.
	list__condense(FragmentList, Instructions0),
	FrameInfo = frame(TotalSlots, MaybeSuccipSlot, _),
	(
		MaybeSuccipSlot = yes(SuccipSlot)
	->
			% The set of recorded live values at calls (for value
			% numbering) and returns (for accurate gc and execution
			% tracing) do not yet record the stack slot holding the
			% succip, so add it to those sets.
		code_gen__add_saved_succip(Instructions0,
			SuccipSlot, Instructions)
	;
		Instructions = Instructions0
	),

	proc_info_get_maybe_proc_table_info(ProcInfo, MaybeTableInfo),
	(
		( BasicStackLayout = yes
		; MaybeTableInfo = yes(table_io_decl_info(_TableIoDeclInfo))
		)
	->
			% Create the procedure layout structure.
		RttiProcLabel = rtti__make_rtti_proc_label(ModuleInfo,
			PredId, ProcId),
		code_info__get_layout_info(CodeInfo, InternalMap),
		code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
			no, EntryLabel),
		proc_info_eval_method(ProcInfo, EvalMethod),
		proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap0),
		proc_info_headvars(ProcInfo, HeadVars),
		proc_info_varset(ProcInfo, VarSet),
		proc_info_vartypes(ProcInfo, VarTypes),
		globals__get_trace_suppress(Globals, TraceSuppress),
		(
			eff_trace_needs_proc_body_reps(PredInfo, ProcInfo,
				TraceLevel, TraceSuppress) = yes
		->
			MaybeGoal = yes(Goal)
		;
			MaybeGoal = no
		),
		IsBeingTraced = bool__not(EffTraceIsNone),
		NeedsAllNames = eff_trace_needs_all_var_names(PredInfo,
			ProcInfo, TraceLevel, TraceSuppress),
		proc_info_get_maybe_deep_profile_info(ProcInfo,
			MaybeHLDSDeepInfo),
		(
			MaybeHLDSDeepInfo = yes(HLDSDeepInfo),
			DeepProfInfo = generate_deep_prof_info(ProcInfo,
				HLDSDeepInfo),
			MaybeDeepProfInfo = yes(DeepProfInfo)
		;
			MaybeHLDSDeepInfo = no,
			MaybeDeepProfInfo = no
		),
		ProcLayout = proc_layout_info(RttiProcLabel, EntryLabel,
			Detism, TotalSlots, MaybeSuccipSlot, EvalMethod,
			MaybeTraceCallLabel, MaxTraceReg, HeadVars, MaybeGoal,
			InstMap0, TraceSlotInfo, ForceProcId, VarSet, VarTypes,
			InternalMap, MaybeTableInfo, IsBeingTraced,
			NeedsAllNames, MaybeDeepProfInfo),
		global_data_add_new_proc_layout(proc(PredId, ProcId),
			ProcLayout, !GlobalData)
	;
		true
	),

	code_info__get_closure_layouts(CodeInfo, ClosureLayouts),
	global_data_add_new_closure_layouts(ClosureLayouts, !GlobalData),
	ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
	maybe_add_tabling_pointer_var(ModuleInfo, PredId, ProcId, ProcInfo,
		ProcLabel, !GlobalData),

	Name = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),

	code_info__get_label_counter(CodeInfo, LabelCounter),
	(
		EffTraceIsNone = yes,
		MayAlterRtti = may_alter_rtti
	;
		EffTraceIsNone = no,
		MayAlterRtti = must_not_alter_rtti
	),

	globals__lookup_bool_option(Globals, generate_bytecode, GenBytecode),
	(
		% XXX: There is a mass of calls above that the bytecode
		% doesn't need; work out which is and isn't needed and put
		% inside the else case below
		GenBytecode = yes,
		% We don't generate bytecode for unify and compare preds.
		% The automatically generated unify and compare predicates
		% are correct by construction; for user-defined unify and
		% compare predicates, we *assume* their correctness for now
		% (perhaps not wisely).
		\+ is_unify_or_compare_pred(PredInfo),
		% Don't generate bytecode for procs with foreign code
		goal_has_foreign(Goal) = no
	->
		EmptyLabelCounter = counter__init(0),
		code_gen__bytecode_stub(ModuleInfo, PredId, ProcId,
			BytecodeInstructions),
		Proc = c_procedure(Name, Arity, proc(PredId, ProcId),
			BytecodeInstructions, ProcLabel, EmptyLabelCounter,
			MayAlterRtti)
	;
		Proc = c_procedure(Name, Arity, proc(PredId, ProcId),
			Instructions, ProcLabel, LabelCounter,
			MayAlterRtti)
	).

:- pred maybe_set_trace_level(pred_info::in,
	module_info::in, module_info::out) is det.

maybe_set_trace_level(PredInfo, !ModuleInfo) :-
	module_info_globals(!.ModuleInfo, Globals0),
	(
		PredModule = pred_info_module(PredInfo),
		PredName = pred_info_name(PredInfo),
		PredArity = pred_info_arity(PredInfo),
		no_type_info_builtin(PredModule, PredName, PredArity)
	->
			% These predicates should never be traced,
			% since they do not obey typeinfo_liveness.
			% Since they may be opt_imported into other
			% modules, we must switch off the tracing
			% of such preds on a pred-by-pred basis.
		globals__set_trace_level_none(Globals0, Globals1),
		module_info_set_globals(Globals1, !ModuleInfo)
	;
		pred_info_get_maybe_special_pred(PredInfo, yes(_)),
		globals__get_trace_level(Globals0, TraceLevel),
		UC_TraceLevel =
			trace_level_for_unify_compare(TraceLevel)
	->
		globals__set_trace_level(UC_TraceLevel,
			Globals0, Globals1),
		module_info_set_globals(Globals1, !ModuleInfo)
	;
		true
	).

:- func generate_deep_prof_info(proc_info, deep_profile_proc_info)
	= proc_layout_proc_static.

generate_deep_prof_info(ProcInfo, HLDSDeepInfo) = DeepProfInfo :-
	HLDSDeepInfo ^ deep_layout = MaybeHLDSDeepLayout,
	(
		MaybeHLDSDeepLayout = yes(HLDSDeepLayout)
	;
		MaybeHLDSDeepLayout = no,
		error("generate_deep_prof_info: " ++
			"no HLDS deep profiling layout info")
	),
	HLDSDeepLayout = hlds_deep_layout(HLDSProcStatic, HLDSExcpVars),
	HLDSExcpVars = hlds_deep_excp_vars(TopCSDVar, MiddleCSDVar,
		MaybeOldOutermostVar),
	proc_info_stack_slots(ProcInfo, StackSlots),
	( map__search(StackSlots, TopCSDVar, TopCSDSlot) ->
		TopCSDSlotNum = stack_slot_num(TopCSDSlot),
		map__lookup(StackSlots, MiddleCSDVar, MiddleCSDSlot),
		MiddleCSDSlotNum = stack_slot_num(MiddleCSDSlot),
		(
			MaybeOldOutermostVar = yes(OldOutermostVar),
			map__lookup(StackSlots, OldOutermostVar,
				OldOutermostSlot),
			OldOutermostSlotNum = stack_slot_num(OldOutermostSlot)
		;
			MaybeOldOutermostVar = no,
			OldOutermostSlotNum = -1
		)
	;
		TopCSDSlotNum = -1,
		MiddleCSDSlotNum = -1,
		OldOutermostSlotNum = -1
	),
	DeepExcpSlots = deep_excp_slots(TopCSDSlotNum, MiddleCSDSlotNum,
		OldOutermostSlotNum),
	DeepProfInfo = proc_layout_proc_static(HLDSProcStatic, DeepExcpSlots).

:- pred maybe_add_tabling_pointer_var(module_info::in,
	pred_id::in, proc_id::in, proc_info::in, proc_label::in,
	global_data::in, global_data::out) is det.

maybe_add_tabling_pointer_var(ModuleInfo, PredId, ProcId, ProcInfo, ProcLabel,
		!GlobalData) :-
	proc_info_eval_method(ProcInfo, EvalMethod),
	( eval_method_has_per_proc_tabling_pointer(EvalMethod) = yes ->
		module_info_name(ModuleInfo, ModuleName),
		Var = tabling_pointer_var(ModuleName, ProcLabel),
		global_data_add_new_proc_var(proc(PredId, ProcId), Var,
			!GlobalData)
	;
		true
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

	% Generate_category_code generates code for an entire procedure.
	% Its algorithm has three or four main stages:
	%
	%	- generate code for the body goal
	%	- generate code for the procedure entry
	%	- generate code for the procedure exit
	%	- generate code for the procedure fail (if needed)
	%
	% The first three tasks are forwarded to other procedures.
	% The fourth task, if needed, is done by generate_category_code.
	%
	% The only caller of generate_category_code, generate_proc_code,
	% has set up the code generator state to reflect what the machine
	% state will be on entry to the procedure. Ensuring that the
	% machine state at exit will conform to the expectation
	% of the caller is the job of code_gen__generate_exit.
	%
	% The reason why we generate the entry code after the body is that
	% information such as the total number of stack slots needed,
	% which is needed in the procedure entry prologue, cannot be
	% conveniently obtained before generating the body, since the
	% code generator may allocate temporary variables to hold values
	% such as saved heap and trail pointers.
	%
	% Code_gen__generate_entry cannot depend on the code generator
	% state, since when it is invoked this state is not appropriate
	% for the procedure entry. Nor can it change the code generator state,
	% since that would confuse code_gen__generate_exit.
	%
	% Generating CALL trace events is done by generate_category_code,
	% since only on entry to generate_category_code is the code generator
	% state set up right. Generating EXIT trace events is done by
	% code_gen__generate_exit. Generating FAIL trace events is done
	% by generate_category_code, since this requires modifying how
	% we generate code for the body of the procedure (failures must
	% now branch to a different place). Since FAIL trace events are
	% part of the failure continuation, generate_category_code takes
	% care of the failure continuation as well. (Model_det procedures
	% of course have no failure continuation. Model_non procedures have
	% a failure continuation, but in the absence of tracing this
	% continuation needs no code. Only model_semi procedures need code
	% for the failure continuation at all times.)

:- pred generate_category_code(code_model::in, hlds_goal::in,
	resume_point_info::in, trace_slot_info::in, code_tree::out,
	maybe(label)::out, frame_info::out, code_info::in, code_info::out)
	is det.

generate_category_code(model_det, Goal, ResumePoint, TraceSlotInfo, Code,
		MaybeTraceCallLabel, FrameInfo, !CI) :-
		% generate the code for the body of the clause
	(
		code_info__get_globals(!.CI, Globals),
		globals__lookup_bool_option(Globals, middle_rec, yes),
		middle_rec__match_and_generate(Goal, MiddleRecCode, !CI)
	->
		Code = MiddleRecCode,
		MaybeTraceCallLabel = no,
		FrameInfo = frame(0, no, no)
	;
		Goal = _ - GoalInfo,
		goal_info_get_context(GoalInfo, BodyContext),
		code_info__get_maybe_trace_info(!.CI, MaybeTraceInfo),
		( MaybeTraceInfo = yes(TraceInfo) ->
			trace__generate_external_event_code(call, TraceInfo,
				BodyContext, MaybeCallExternalInfo, !CI),
			(
				MaybeCallExternalInfo = yes(CallExternalInfo),
				CallExternalInfo = external_event_info(
					TraceCallLabel, _, TraceCallCode)
			;
				MaybeCallExternalInfo = no,
				error("generate_category_code: " ++
					"call events suppressed")
			),
			MaybeTraceCallLabel = yes(TraceCallLabel)
		;
			TraceCallCode = empty,
			MaybeTraceCallLabel = no
		),
		code_gen__generate_goal(model_det, Goal, BodyCode, !CI),
		code_gen__generate_entry(!.CI, model_det, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_det, FrameInfo, TraceSlotInfo,
			BodyContext, _, ExitCode, !CI),
		Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			     ExitCode)))
	).

generate_category_code(model_semi, Goal, ResumePoint, TraceSlotInfo, Code,
		MaybeTraceCallLabel, FrameInfo, !CI) :-
	set__singleton_set(FailureLiveRegs, reg(r, 1)),
	FailCode = node([
		assign(reg(r, 1), const(false)) - "Fail",
		livevals(FailureLiveRegs) - "",
		goto(succip) - "Return from procedure call"
	]),
	Goal = _ - GoalInfo,
	goal_info_get_context(GoalInfo, BodyContext),
	code_info__get_maybe_trace_info(!.CI, MaybeTraceInfo),
	( MaybeTraceInfo = yes(TraceInfo) ->
		trace__generate_external_event_code(call, TraceInfo,
			BodyContext, MaybeCallExternalInfo, !CI),
		(
			MaybeCallExternalInfo = yes(CallExternalInfo),
			CallExternalInfo = external_event_info(
				TraceCallLabel, _, TraceCallCode)
		;
			MaybeCallExternalInfo = no,
			error("generate_category_code: call events suppressed")
		),
		MaybeTraceCallLabel = yes(TraceCallLabel),
		code_gen__generate_goal(model_semi, Goal, BodyCode, !CI),
		code_gen__generate_entry(!.CI, model_semi, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_semi, FrameInfo, TraceSlotInfo,
			BodyContext, RestoreDeallocCode, ExitCode, !CI),

		code_info__generate_resume_point(ResumePoint, ResumeCode, !CI),
		code_info__resume_point_vars(ResumePoint, ResumeVarList),
		set__list_to_set(ResumeVarList, ResumeVars),
		code_info__set_forward_live_vars(ResumeVars, !CI),
			% XXX A context that gives the end of the procedure
			% definition would be better than BodyContext.
		trace__generate_external_event_code(fail, TraceInfo,
			BodyContext, MaybeFailExternalInfo, !CI),
		(
			MaybeFailExternalInfo = yes(FailExternalInfo),
			FailExternalInfo = external_event_info(
				_, _, TraceFailCode)
		;
			MaybeFailExternalInfo = no,
			TraceFailCode = empty
		),
		Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(TraceFailCode,
			tree(RestoreDeallocCode,
			     FailCode)))))))
	;
		MaybeTraceCallLabel = no,
		code_gen__generate_goal(model_semi, Goal, BodyCode, !CI),
		code_gen__generate_entry(!.CI, model_semi, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_semi, FrameInfo, TraceSlotInfo,
			BodyContext, RestoreDeallocCode, ExitCode, !CI),
		code_info__generate_resume_point(ResumePoint, ResumeCode, !CI),
		Code =
			tree(EntryCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(RestoreDeallocCode,
			     FailCode)))))
	).

generate_category_code(model_non, Goal, ResumePoint, TraceSlotInfo, Code,
		MaybeTraceCallLabel, FrameInfo, !CI) :-
	code_info__get_maybe_trace_info(!.CI, MaybeTraceInfo),
	Goal = _ - GoalInfo,
	goal_info_get_context(GoalInfo, BodyContext),
	( MaybeTraceInfo = yes(TraceInfo) ->
		trace__generate_external_event_code(call, TraceInfo,
			BodyContext, MaybeCallExternalInfo, !CI),
		(
			MaybeCallExternalInfo = yes(CallExternalInfo),
			CallExternalInfo = external_event_info(
				TraceCallLabel, _, TraceCallCode)
		;
			MaybeCallExternalInfo = no,
			error("generate_category_code: call events suppressed")
		),
		MaybeTraceCallLabel = yes(TraceCallLabel),
		code_gen__generate_goal(model_non, Goal, BodyCode, !CI),
		code_gen__generate_entry(!.CI, model_non, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_non, FrameInfo, TraceSlotInfo,
			BodyContext, _, ExitCode, !CI),

		code_info__generate_resume_point(ResumePoint, ResumeCode, !CI),
		code_info__resume_point_vars(ResumePoint, ResumeVarList),
		set__list_to_set(ResumeVarList, ResumeVars),
		code_info__set_forward_live_vars(ResumeVars, !CI),
			% XXX A context that gives the end of the procedure
			% definition would be better than BodyContext.
		trace__generate_external_event_code(fail, TraceInfo,
			BodyContext, MaybeFailExternalInfo, !CI),
		(
			MaybeFailExternalInfo = yes(FailExternalInfo),
			FailExternalInfo = external_event_info(
				_, _, TraceFailCode)
		;
			MaybeFailExternalInfo = no,
			TraceFailCode = empty
		),
		( TraceSlotInfo ^ slot_trail = yes(_) ->
			( TraceSlotInfo ^ slot_from_full =
				yes(FromFullSlot)
			->
				%
				% Generate code which discards the ticket
				% only if it was allocated, i.e. only if
				% MR_trace_from_full was true on entry.
				%
				FromFullSlotLval =
					llds__stack_slot_num_to_lval(
						model_non, FromFullSlot),
				code_info__get_next_label(SkipLabel, !CI),
				DiscardTraceTicketCode = node([
					if_val(unop(not,
						lval(FromFullSlotLval)),
						label(SkipLabel)) - "",
					discard_ticket - "discard retry ticket",
					label(SkipLabel) - ""
				])
			;
				DiscardTraceTicketCode = node([
					discard_ticket - "discard retry ticket"
				])
			)
		;
			DiscardTraceTicketCode = empty
		),
		FailCode = node([
			goto(do_fail) - "fail after fail trace port"
		]),
		Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(TraceFailCode,
			tree(DiscardTraceTicketCode,
			     FailCode)))))))
	;
		MaybeTraceCallLabel = no,
		code_gen__generate_goal(model_non, Goal, BodyCode, !CI),
		code_gen__generate_entry(!.CI, model_non, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_non, FrameInfo, TraceSlotInfo,
			BodyContext, _, ExitCode, !CI),
		Code =
			tree(EntryCode,
			tree(BodyCode,
			     ExitCode))
	).

%---------------------------------------------------------------------------%

	% Generate the prologue for a procedure.
	%
	% The prologue will contain
	%
	%	a comment to mark prologue start
	%	a comment explaining the stack layout
	%	the procedure entry label
	%	code to allocate a stack frame
	%	code to fill in some special slots in the stack frame
	%	a comment to mark prologue end
	%
	% At the moment the only special slots are the succip slot, and
	% the slots holding the call number and call depth for tracing.
	%
	% Not all frames will have all these components. For example, the code
	% to allocate a stack frame will be missing if the procedure doesn't
	% need a stack frame, and if the procedure is nondet, then the code
	% to fill in the succip slot is subsumed by the mkframe.

:- pred code_gen__generate_entry(code_info::in, code_model::in, hlds_goal::in,
	resume_point_info::in, frame_info::out, code_tree::out) is det.

code_gen__generate_entry(CI, CodeModel, Goal, OutsideResumePoint, FrameInfo,
		EntryCode) :-
	code_info__get_stack_slots(CI, StackSlots),
	code_info__get_varset(CI, VarSet),
	code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment),
	StartComment = node([
		comment("Start of procedure prologue") - "",
		comment(SlotsComment) - ""
	]),
	code_info__get_total_stackslot_count(CI, MainSlots),
	code_info__get_pred_id(CI, PredId),
	code_info__get_proc_id(CI, ProcId),
	code_info__get_module_info(CI, ModuleInfo),
	code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry),
	LabelCode = node([
		label(Entry) - "Procedure entry point"
	]),
	code_info__get_succip_used(CI, Used),
	(
		% Do we need to save the succip across calls?
		Used = yes,
		% Do we need to use a general slot for storing succip?
		CodeModel \= model_non
	->
		SuccipSlot = MainSlots + 1,
		SaveSuccipCode = node([
			assign(stackvar(SuccipSlot), lval(succip)) -
				"Save the success ip"
		]),
		TotalSlots = SuccipSlot,
		MaybeSuccipSlot = yes(SuccipSlot)
	;
		SaveSuccipCode = empty,
		TotalSlots = MainSlots,
		MaybeSuccipSlot = no
	),
	code_info__get_maybe_trace_info(CI, MaybeTraceInfo),
	( MaybeTraceInfo = yes(TraceInfo) ->
		trace__generate_slot_fill_code(CI, TraceInfo, TraceFillCode)
	;
		TraceFillCode = empty
	),
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	ModuleName = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),

	PushMsg = code_gen__push_msg(ModuleInfo, PredId, ProcId),
	( CodeModel = model_non ->
		code_info__resume_point_stack_addr(OutsideResumePoint,
			OutsideResumeAddress),
		(
			Goal = foreign_proc(_, _, _, _, _, PragmaCode) - _,
			PragmaCode = nondet(Fields, FieldsContext,
				_,_,_,_,_,_,_)
		->
			pragma_c_gen__struct_name(ModuleName, PredName,
				Arity, ProcId, StructName),
			Struct = pragma_c_struct(StructName,
				Fields, FieldsContext),
			string__format("#define\tMR_ORDINARY_SLOTS\t%d\n",
				[i(TotalSlots)], DefineStr),
			DefineComponents = [pragma_c_raw_code(DefineStr,
				live_lvals_info(set__init))],
			NondetFrameInfo = ordinary_frame(PushMsg, TotalSlots,
				yes(Struct)),
			AllocCode = node([
				mkframe(NondetFrameInfo,
					yes(OutsideResumeAddress))
					- "Allocate stack frame",
				pragma_c([], DefineComponents,
					will_not_call_mercury, no, no, no, no,
					no, no)
					- ""
			]),
			NondetPragma = yes
		;
			NondetFrameInfo = ordinary_frame(PushMsg, TotalSlots,
				no),
			AllocCode = node([
				mkframe(NondetFrameInfo,
					yes(OutsideResumeAddress))
					- "Allocate stack frame"
			]),
			NondetPragma = no
		)
	; TotalSlots > 0 ->
		AllocCode = node([
			incr_sp(TotalSlots, PushMsg) -
				"Allocate stack frame"
		]),
		NondetPragma = no
	;
		AllocCode = empty,
		NondetPragma = no
	),
	FrameInfo = frame(TotalSlots, MaybeSuccipSlot, NondetPragma),
	EndComment = node([
		comment("End of procedure prologue") - ""
	]),
	EntryCode =
		tree(StartComment,
		tree(LabelCode,
		tree(AllocCode,
		tree(SaveSuccipCode,
		tree(TraceFillCode,
		     EndComment))))).

%---------------------------------------------------------------------------%

	% Generate the success epilogue for a procedure.
	%
	% The success epilogue will contain
	%
	%	a comment to mark epilogue start
	%	code to place the output arguments where their caller expects
	%	code to restore registers from some special slots
	%	code to deallocate the stack frame
	%	code to set r1 to MR_TRUE (for semidet procedures only)
	%	a jump back to the caller, including livevals information
	%	a comment to mark epilogue end
	%
	% The parts of this that restore registers and deallocate the stack
	% frame are also part of the failure epilog, which is handled by
	% our caller; this is why we return RestoreDeallocCode.
	%
	% At the moment the only special slots are the succip slot, and
	% the tracing slots (holding the call sequence number, call event
	% number, call depth, from-full indication, and trail state).
	%
	% Not all frames will have all these components. For example, for
	% nondet procedures we don't deallocate the stack frame before
	% success.
	%
	% Epilogues for procedures defined by nondet pragma C codes do not
	% follow the rules above. For such procedures, the normal functions
	% of the epilogue are handled when traversing the pragma C code goal;
	% we need only #undef a macro defined by the procedure prologue.

:- pred code_gen__generate_exit(code_model::in, frame_info::in,
	trace_slot_info::in, prog_context::in, code_tree::out, code_tree::out,
	code_info::in, code_info::out) is det.

code_gen__generate_exit(CodeModel, FrameInfo, TraceSlotInfo, BodyContext,
		RestoreDeallocCode, ExitCode, !CI) :-
	StartComment = node([
		comment("Start of procedure epilogue") - ""
	]),
	EndComment = node([
		comment("End of procedure epilogue") - ""
	]),
	FrameInfo = frame(TotalSlots, MaybeSuccipSlot, NondetPragma),
	( NondetPragma = yes ->
		UndefStr = "#undef\tMR_ORDINARY_SLOTS\n",
		UndefComponents = [pragma_c_raw_code(UndefStr,
			live_lvals_info(set__init))],
		UndefCode = node([
			pragma_c([], UndefComponents,
				will_not_call_mercury, no, no, no, no, no, no)
				- ""
		]),
		RestoreDeallocCode = empty,	% always empty for nondet code
		ExitCode =
			tree(StartComment,
			tree(UndefCode,
			     EndComment))
	;
		code_info__get_instmap(!.CI, Instmap),
		ArgModes = code_info__get_arginfo(!.CI),
		HeadVars = code_info__get_headvars(!.CI),
		assoc_list__from_corresponding_lists(HeadVars, ArgModes,
			Args),
		( instmap__is_unreachable(Instmap) ->
			OutLvals = set__init,
			FlushCode = empty
		;
			code_info__setup_return(Args, OutLvals, FlushCode, !CI)
		),
		( MaybeSuccipSlot = yes(SuccipSlot) ->
			RestoreSuccipCode = node([
				assign(succip, lval(stackvar(SuccipSlot))) -
					"restore the success ip"
			])
		;
			RestoreSuccipCode = empty
		),
		( ( TotalSlots = 0 ; CodeModel = model_non ) ->
			DeallocCode = empty
		;
			DeallocCode = node([
				decr_sp(TotalSlots) - "Deallocate stack frame"
			])
		),
		(
			TraceSlotInfo ^ slot_trail = yes(_),
			CodeModel \= model_non
		->
			( TraceSlotInfo ^ slot_from_full = yes(FromFullSlot) ->
				%
				% Generate code which prunes the ticket
				% only if it was allocated, i.e. only if
				% MR_trace_from_full was true on entry.
				%
				% Note that to avoid duplicating label names,
				% we need to generate two different copies
				% of this with different labels; this is
				% needed for semidet code, which will get one
				% copy in the success epilogue and one copy
				% in the failure epilogue
				%
				FromFullSlotLval =
					llds__stack_slot_num_to_lval(
						CodeModel, FromFullSlot),
				code_info__get_next_label(SkipLabel, !CI),
				code_info__get_next_label(SkipLabelCopy, !CI),
				PruneTraceTicketCode = node([
					if_val(unop(not,
						lval(FromFullSlotLval)),
						label(SkipLabel)) - "",
					prune_ticket - "prune retry ticket",
					label(SkipLabel) - ""
				]),
				PruneTraceTicketCodeCopy = node([
					if_val(unop(not,
						lval(FromFullSlotLval)),
						label(SkipLabelCopy)) - "",
					prune_ticket - "prune retry ticket",
					label(SkipLabelCopy) - ""
				])
			;
				PruneTraceTicketCode = node([
					prune_ticket - "prune retry ticket"
				]),
				PruneTraceTicketCodeCopy =
					PruneTraceTicketCode
			)
		;
			PruneTraceTicketCode = empty,
			PruneTraceTicketCodeCopy = empty
		),

		RestoreDeallocCode =
			tree(RestoreSuccipCode,
			tree(PruneTraceTicketCode,
			     DeallocCode)),
		RestoreDeallocCodeCopy =
			tree(RestoreSuccipCode,
			tree(PruneTraceTicketCodeCopy,
			     DeallocCode)),

		code_info__get_maybe_trace_info(!.CI, MaybeTraceInfo),
		(
			MaybeTraceInfo = yes(TraceInfo),
				% XXX A context that gives the end of the
				% procedure definition would be better than
				% CallContext.
			trace__generate_external_event_code(exit, TraceInfo,
				BodyContext, MaybeExitExternalInfo, !CI),
			(
				MaybeExitExternalInfo = yes(ExitExternalInfo),
				ExitExternalInfo = external_event_info(
					_, TypeInfoDatas, TraceExitCode)
			;
				MaybeExitExternalInfo = no,
				TypeInfoDatas = map__init,
				TraceExitCode = empty
			),
			map__values(TypeInfoDatas, TypeInfoLocnSets),
			FindBaseLvals = (pred(Lval::out) is nondet :-
				list__member(LocnSet, TypeInfoLocnSets),
				set__member(Locn, LocnSet),
				(
					Locn = direct(Lval)
				;
					Locn = indirect(Lval, _)
				)
			),
			solutions(FindBaseLvals, TypeInfoLvals),
			set__insert_list(OutLvals, TypeInfoLvals,
				LiveLvals)
		;
			MaybeTraceInfo = no,
			TraceExitCode = empty,
			LiveLvals = OutLvals
		),

		(
			CodeModel = model_det,
			SuccessCode = node([
				livevals(LiveLvals) - "",
				goto(succip) - "Return from procedure call"
			]),
			AllSuccessCode =
				tree(TraceExitCode,
				tree(RestoreDeallocCodeCopy,
				     SuccessCode))
		;
			CodeModel = model_semi,
			set__insert(LiveLvals, reg(r, 1), SuccessLiveRegs),
			SuccessCode = node([
				assign(reg(r, 1), const(true)) - "Succeed",
				livevals(SuccessLiveRegs) - "",
				goto(succip) - "Return from procedure call"
			]),
			AllSuccessCode =
				tree(TraceExitCode,
				tree(RestoreDeallocCodeCopy,
				     SuccessCode))
		;
			CodeModel = model_non,
			( MaybeTraceInfo = yes(TraceInfo2) ->
				trace__maybe_setup_redo_event(TraceInfo2,
					SetupRedoCode)
			;
				SetupRedoCode = empty
			),
			SuccessCode = node([
				livevals(LiveLvals) - "",
				goto(do_succeed(no))
					- "Return from procedure call"
			]),
			AllSuccessCode =
				tree(SetupRedoCode,
				tree(TraceExitCode,
				     SuccessCode))
		),
		ExitCode =
			tree(StartComment,
			tree(FlushCode,
			tree(AllSuccessCode,
			     EndComment)))
	).

%---------------------------------------------------------------------------%

% Generate a goal. This predicate arranges for the necessary updates of
% the generic data structures before and after the actual code generation,
% which is delegated to goal-specific predicates.

code_gen__generate_goal(ContextModel, Goal - GoalInfo, Code, !CI) :-
		% Make any changes to liveness before Goal
	( goal_is_atomic(Goal) ->
		IsAtomic = yes
	;
		IsAtomic = no
	),
	code_info__pre_goal_update(GoalInfo, IsAtomic, !CI),
	code_info__get_instmap(!.CI, Instmap),
	( instmap__is_reachable(Instmap) ->
		goal_info_get_code_model(GoalInfo, CodeModel),

			% sanity check: code of some code models
			% should occur only in limited contexts
		(
			CodeModel = model_det
		;
			CodeModel = model_semi,
			( ContextModel \= model_det ->
				true
			;
				error("semidet model in det context")
			)
		;
			CodeModel = model_non,
			( ContextModel = model_non ->
				true
			;
				error("nondet model in det/semidet context")
			)
		),

		code_gen__generate_goal_2(Goal, GoalInfo, CodeModel, GoalCode,
			!CI),
		goal_info_get_features(GoalInfo, Features),
		code_info__get_proc_info(!.CI, ProcInfo),

			% If the predicate's evaluation method is memo,
			% loopcheck or minimal model, the goal generated
			% the variable that represents the call table tip,
			% *and* tracing is enabled, then we save this variable
			% to its stack slot. This is necessary to enable
			% retries across this procedure to reset the call table
			% entry to uninitialized, effectively removing the
			% call table entry.
			%
			% If tracing is not enabled, then CallTableVar isn't
			% guaranteed to have a stack slot.
		(
			set__member(call_table_gen, Features),
			code_info__get_proc_info(!.CI, ProcInfo),
			proc_info_get_call_table_tip(ProcInfo,
				MaybeCallTableVar),
			MaybeCallTableVar = yes(CallTableVar),
			code_info__get_maybe_trace_info(!.CI, yes(_))
		->
			code_info__save_variables_on_stack([CallTableVar],
				TipSaveCode, !CI),
			CodeUptoTip = tree(GoalCode, TipSaveCode)
		;
			CodeUptoTip = GoalCode
		),
			% After the goal that generates the variables needed
			% at the exception port, on which deep_profiling.m puts
			% the save_deep_excp_vars feature, save those variables
			% in their stack slots. The procedure layout structure
			% gives the identity of their slots, and exception.m
			% expects to find the variables in their stack slots.
			%
			% These variables are computed by the call port code
			% and are needed by the exit and fail port codes, so
			% their lifetime is the entire procedure invocation.
			% If the procedure makes any calls other than the ones
			% inserted by deep profiling, then all the variables
			% will have stack slots, and we save them all on the
			% stack. If the procedure doesn't make any such calls,
			% then the variables won't have stack slots, but they
			% won't *need* stack slots either, since there is no
			% way for such a leaf procedure to throw an exception.
			% (Throwing requires calling exception__throw, directly
			% or indirectly.)
		(
			set__member(save_deep_excp_vars, Features)
		->
			DeepSaveVars = compute_deep_save_excp_vars(ProcInfo),
			code_info__save_variables_on_stack(DeepSaveVars,
				DeepSaveCode, !CI),
			Code = tree(CodeUptoTip, DeepSaveCode)
		;
			Code = CodeUptoTip
		),

			% Make live any variables which subsequent goals
			% will expect to be live, but were not generated
		code_info__set_instmap(Instmap, !CI),
		code_info__post_goal_update(GoalInfo, !CI)
	;
		Code = empty
	).

:- func compute_deep_save_excp_vars(proc_info) = list(prog_var).

compute_deep_save_excp_vars(ProcInfo) = DeepSaveVars :-
	proc_info_get_maybe_deep_profile_info(ProcInfo, MaybeDeepProfInfo),
	(
		MaybeDeepProfInfo = yes(DeepProfInfo),
		MaybeDeepLayout = DeepProfInfo ^ deep_layout,
		MaybeDeepLayout = yes(DeepLayout)
	->
		ExcpVars = DeepLayout ^ deep_layout_excp,
		ExcpVars = hlds_deep_excp_vars(TopCSDVar, MiddleCSDVar,
			MaybeOldOutermostVar),
		proc_info_stack_slots(ProcInfo, StackSlots),
		( map__search(StackSlots, TopCSDVar, _) ->
			% If one of these variables has a stack
			% slot, the others must have one too.
			(
				MaybeOldOutermostVar = yes(OldOutermostVar),
				DeepSaveVars = [TopCSDVar, MiddleCSDVar,
					OldOutermostVar]
			;
				MaybeOldOutermostVar = no,
				DeepSaveVars = [TopCSDVar, MiddleCSDVar]
			)
		;
			DeepSaveVars = []
		)
	;
		error("compute_deep_save_excp_vars: inconsistent proc_info")
	).

%---------------------------------------------------------------------------%

:- pred code_gen__generate_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	code_model::in, code_tree::out, code_info::in, code_info::out) is det.

code_gen__generate_goal_2(unify(_, _, _, Uni, _), GoalInfo, CodeModel, Code,
		!CI) :-
	unify_gen__generate_unification(CodeModel, Uni, GoalInfo, Code, !CI).
code_gen__generate_goal_2(conj(Goals), _GoalInfo, CodeModel, Code, !CI) :-
	code_gen__generate_goals(Goals, CodeModel, Code, !CI).
code_gen__generate_goal_2(par_conj(Goals), GoalInfo, CodeModel, Code, !CI) :-
	par_conj_gen__generate_par_conj(Goals, GoalInfo, CodeModel, Code, !CI).
code_gen__generate_goal_2(disj(Goals), GoalInfo, CodeModel, Code, !CI) :-
	disj_gen__generate_disj(CodeModel, Goals, GoalInfo, Code, !CI).
code_gen__generate_goal_2(not(Goal), GoalInfo, CodeModel, Code, !CI) :-
	ite_gen__generate_negation(CodeModel, Goal, GoalInfo, Code, !CI).
code_gen__generate_goal_2(if_then_else(_Vars, Cond, Then, Else),
		GoalInfo, CodeModel, Code, !CI) :-
	ite_gen__generate_ite(CodeModel, Cond, Then, Else, GoalInfo, Code,
		!CI).
code_gen__generate_goal_2(switch(Var, CanFail, CaseList),
		GoalInfo, CodeModel, Code, !CI) :-
	switch_gen__generate_switch(CodeModel, Var, CanFail, CaseList,
		GoalInfo, Code, !CI).
code_gen__generate_goal_2(some(_Vars, _, Goal), _GoalInfo, CodeModel, Code,
		!CI) :-
	commit_gen__generate_commit(CodeModel, Goal, Code, !CI).
code_gen__generate_goal_2(generic_call(GenericCall, Args, Modes, Det),
		GoalInfo, CodeModel, Code, !CI) :-
	call_gen__generate_generic_call(CodeModel, GenericCall, Args,
		Modes, Det, GoalInfo, Code, !CI).
code_gen__generate_goal_2(call(PredId, ProcId, Args, BuiltinState, _,_),
		GoalInfo, CodeModel, Code, !CI) :-
	( BuiltinState = not_builtin ->
		call_gen__generate_call(CodeModel, PredId, ProcId, Args,
			GoalInfo, Code, !CI)
	;
		call_gen__generate_builtin(CodeModel, PredId, ProcId, Args,
			Code, !CI)
	).
code_gen__generate_goal_2(foreign_proc(Attributes, PredId, ProcId,
		Args, ExtraArgs, PragmaCode),
		GoalInfo, CodeModel, Code, !CI) :-
	( c = foreign_language(Attributes) ->
		pragma_c_gen__generate_pragma_c_code(CodeModel, Attributes,
			PredId, ProcId, Args, ExtraArgs, GoalInfo, PragmaCode,
			Code, !CI)
	;
		error("code_gen__generate_goal_2: " ++
			"foreign code other than C unexpected")
	).
code_gen__generate_goal_2(shorthand(_), _, _, _, !CI) :-
	% these should have been expanded out by now
	error("code_gen__generate_goal_2: unexpected shorthand").

%---------------------------------------------------------------------------%

% Generate a conjoined series of goals.
% Note of course, that with a conjunction, state information
% flows directly from one conjunct to the next.

:- pred code_gen__generate_goals(hlds_goals::in, code_model::in,
	code_tree::out, code_info::in, code_info::out) is det.

code_gen__generate_goals([], _, empty, !CI).
code_gen__generate_goals([Goal | Goals], CodeModel, Instr, !CI) :-
	code_gen__generate_goal(CodeModel, Goal, Instr1, !CI),
	code_info__get_instmap(!.CI, Instmap),
	(
		instmap__is_unreachable(Instmap)
	->
		Instr = Instr1
	;
		code_gen__generate_goals(Goals, CodeModel, Instr2, !CI),
		Instr = tree(Instr1, Instr2)
	).

%---------------------------------------------------------------------------%

:- pred code_gen__select_args_with_mode(assoc_list(prog_var, arg_info)::in,
	arg_mode::in, list(prog_var)::out, list(lval)::out) is det.

code_gen__select_args_with_mode([], _, [], []).
code_gen__select_args_with_mode([Var - ArgInfo | Args], DesiredMode, Vs, Ls) :-
	code_gen__select_args_with_mode(Args, DesiredMode, Vs0, Ls0),
	ArgInfo = arg_info(Loc, Mode),
	(
		Mode = DesiredMode
	->
		code_util__arg_loc_to_register(Loc, Reg),
		Vs = [Var | Vs0],
		Ls = [Reg | Ls0]
	;
		Vs = Vs0,
		Ls = Ls0
	).

%---------------------------------------------------------------------------%

% Add the succip to the livevals before and after calls.
% Traverses the list of instructions looking for livevals and calls,
% adding succip in the stackvar number given as an argument.

:- pred code_gen__add_saved_succip(list(instruction)::in, int::in,
	list(instruction)::out) is det.

code_gen__add_saved_succip([], _StackLoc, []).
code_gen__add_saved_succip([Instrn0 - Comment | Instrns0 ], StackLoc,
		[Instrn - Comment | Instrns]) :-
	(
		Instrn0 = livevals(LiveVals0),
		Instrns0 \= [goto(succip) - _ | _]
		% XXX We should also test for tailcalls
		% once we start generating them directly.
	->
		set__insert(LiveVals0, stackvar(StackLoc), LiveVals1),
		Instrn = livevals(LiveVals1)
        ;
		Instrn0 = call(Target, ReturnLabel, LiveVals0, Context, GP, CM)
	->
		map__init(Empty),
		LiveVals  = [live_lvalue(direct(stackvar(StackLoc)),
				succip, Empty) | LiveVals0],
		Instrn = call(Target, ReturnLabel, LiveVals, Context, GP, CM)
	;
		Instrn = Instrn0
	),
	code_gen__add_saved_succip(Instrns0, StackLoc, Instrns).

%---------------------------------------------------------------------------%

:- pred code_gen__bytecode_stub(module_info::in, pred_id::in, proc_id::in,
	list(instruction)::out) is det.

code_gen__bytecode_stub(ModuleInfo, PredId, ProcId, BytecodeInstructions) :-

	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	ModuleSymName = pred_info_module(PredInfo),

	prog_out__sym_name_to_string(ModuleSymName, "__", ModuleName),

	code_util__make_local_entry_label(ModuleInfo, PredId,
		ProcId, no, Entry),

	PredName = pred_info_name(PredInfo),
	proc_id_to_int(ProcId, ProcNum),
	string__int_to_string(ProcNum, ProcStr),
	Arity = pred_info_arity(PredInfo),
	int_to_string(Arity, ArityStr),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),

	CallStructName = "bytecode_call_info",

	append_list([
		"\t\tstatic MB_Call ", CallStructName, " = {\n",
		"\t\t\t(MB_Word)NULL,\n",
		"\t\t\t""", ModuleName, """,\n",
		"\t\t\t""", PredName, """,\n",
		"\t\t\t", ProcStr, ",\n",
		"\t\t\t", ArityStr, ",\n",
		"\t\t\t", (PredOrFunc = function -> "MR_TRUE" ; "MR_FALSE"),
			"\n",
		"\t\t};\n"
		], CallStruct),

	append_list([
		"\t\tMB_Native_Addr return_addr;\n",
		"\t\tMR_save_registers();\n",
		"\t\treturn_addr = MB_bytecode_call_entry(",
			"&",CallStructName,");\n",
		"\t\tMR_restore_registers();\n",
		"\t\tMR_GOTO(return_addr);\n"
		], BytecodeCall),

	BytecodeInstructionsComponents = [
		pragma_c_raw_code("\t{\n", live_lvals_info(set__init)),
		pragma_c_raw_code(CallStruct, live_lvals_info(set__init)),
		pragma_c_raw_code(BytecodeCall, no_live_lvals_info),
		pragma_c_raw_code("\t}\n", live_lvals_info(set__init))
	],

	BytecodeInstructions = [
		label(Entry) - "Procedure entry point",
		pragma_c([], BytecodeInstructionsComponents,
			may_call_mercury, no, no, no, no, no, no)
			- "Entry stub"
	].

%---------------------------------------------------------------------------%

:- type type_giving_arg --->	last_arg ; last_but_one_arg.

code_gen__push_msg(ModuleInfo, PredId, ProcId) = PushMsg :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	ModuleName = pred_info_module(PredInfo),
	PredName = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	pred_info_get_maybe_special_pred(PredInfo, MaybeSpecial),
	( MaybeSpecial = yes(SpecialId - TypeCtor) ->
		code_gen__find_arg_type_ctor_name(TypeCtor, TypeName),
		special_pred_name_arity(SpecialId, SpecialPredName, _),
		string__append_list([SpecialPredName, "_for_", TypeName],
			FullPredName)
	;
		FullPredName = PredName
	),
	PredOrFuncString = pred_or_func_to_str(PredOrFunc),
	prog_out__sym_name_to_string(ModuleName, ModuleNameString),
	string__int_to_string(Arity, ArityStr),
	proc_id_to_int(ProcId, ProcNum),
	string__int_to_string(ProcNum, ProcNumStr),
		% XXX if ModuleNameString ends with [0-9] and/or
		% FullPredName starts with [0-9] then ideally we
		% should use "'.'" rather than just ".".
		%
	string__append_list([PredOrFuncString, " ", ModuleNameString, ".",
		FullPredName, "/", ArityStr, "-", ProcNumStr], PushMsg).

:- pred code_gen__find_arg_type_ctor_name((type_ctor)::in, string::out) is det.

code_gen__find_arg_type_ctor_name(TypeCtor, TypeName) :-
	TypeCtor = TypeCtorSymName - TypeCtorArity,
	prog_out__sym_name_to_string(TypeCtorSymName, TypeCtorName),
	string__int_to_string(TypeCtorArity, ArityStr),
	string__append_list([TypeCtorName, "_", ArityStr], TypeName).

%---------------------------------------------------------------------------%
