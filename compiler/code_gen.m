%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
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

:- module code_gen.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, llds, code_info.
:- import_module continuation_info, globals.
:- import_module list, io.

		% Translate a HLDS module to LLDS.

:- pred generate_code(module_info::in, module_info::out,
	list(c_procedure)::out, io__state::di, io__state::uo) is det.

		% Translate a HLDS procedure to LLDS, threading through
		% the data structure that records information about layout
		% structures and the counter for ensuring the uniqueness
		% of cell numbers.

:- pred generate_proc_code(pred_info::in, proc_info::in,
	proc_id::in, pred_id::in, module_info::in, globals::in,
	continuation_info::in, continuation_info::out, int::in, int::out,
	c_procedure::out) is det.

		% Translate a HLDS goal to LLDS.

:- pred code_gen__generate_goal(code_model::in, hlds_goal::in, code_tree::out,
	code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module call_gen, unify_gen, ite_gen, switch_gen, disj_gen.
:- import_module par_conj_gen, pragma_c_gen, commit_gen.
:- import_module trace, options, hlds_out.
:- import_module code_aux, middle_rec, passes_aux, llds_out.
:- import_module code_util, type_util, mode_util.
:- import_module prog_data, prog_out, instmap.
:- import_module bool, char, int, string.
:- import_module map, assoc_list, set, term, tree, std_util, require, varset.

%---------------------------------------------------------------------------%

generate_code(ModuleInfo0, ModuleInfo, Procedures) -->
		% get a list of all the predicate ids
		% for which we are going to generate code.
	{ module_info_predids(ModuleInfo0, PredIds) },
		% now generate the code for each predicate
	generate_pred_list_code(ModuleInfo0, ModuleInfo, PredIds, Procedures).

	% Translate a list of HLDS predicates to LLDS.

:- pred generate_pred_list_code(module_info::in, module_info::out,
	list(pred_id)::in, list(c_procedure)::out,
	io__state::di, io__state::uo) is det.

generate_pred_list_code(ModuleInfo, ModuleInfo, [], []) --> [].
generate_pred_list_code(ModuleInfo0, ModuleInfo, [PredId | PredIds],
				Predicates) -->
	{ module_info_preds(ModuleInfo0, PredInfos) },
		% get the pred_info structure for this predicate
	{ map__lookup(PredInfos, PredId, PredInfo) },
		% extract a list of all the procedure ids for this
		% predicate and generate code for them
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		{ Predicates0 = [] },
		{ ModuleInfo1 = ModuleInfo0 } 
	;
		generate_pred_code(ModuleInfo0, ModuleInfo1, PredId,
					PredInfo, ProcIds, Predicates0) 
	),
	{ list__append(Predicates0, Predicates1, Predicates) },
		% and generate the code for the rest of the predicates
	generate_pred_list_code(ModuleInfo1, ModuleInfo, PredIds, Predicates1).

	% Translate a HLDS predicate to LLDS.

:- pred generate_pred_code(module_info::in, module_info::out,
	pred_id::in, pred_info::in, list(proc_id)::in, list(c_procedure)::out,
	io__state::di, io__state::uo) is det.

generate_pred_code(ModuleInfo0, ModuleInfo, PredId, PredInfo, ProcIds, Code) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string("% Generating code for "),
		hlds_out__write_pred_id(ModuleInfo0, PredId),
		io__write_string("\n"),
		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics)
	;
		[]
	),
	{ module_info_get_continuation_info(ModuleInfo0, ContInfo0) },
	{ module_info_get_cell_count(ModuleInfo0, CellCount0) },
	globals__io_get_globals(Globals),
	{ generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Globals, ContInfo0, ContInfo, CellCount0, CellCount,
		[], Code) },
	{ module_info_set_cell_count(ModuleInfo0, CellCount, ModuleInfo1) },
	{ module_info_set_continuation_info(ModuleInfo1, ContInfo, 
		ModuleInfo) }.

	% Translate all the procedures of a HLDS predicate to LLDS.

:- pred generate_proc_list_code(list(proc_id)::in, pred_id::in, pred_info::in,
	module_info::in, globals::in,
	continuation_info::in, continuation_info::out, int::in, int::out,
	list(c_procedure)::in, list(c_procedure)::out) is det.

generate_proc_list_code([], _PredId, _PredInfo, _ModuleInfo, _Globals,
		ContInfo, ContInfo, CellCount, CellCount, Procs, Procs).
generate_proc_list_code([ProcId | ProcIds], PredId, PredInfo, ModuleInfo0,
		Globals, ContInfo0, ContInfo, CellCount0, CellCount,
		Procs0, Procs) :-
	pred_info_procedures(PredInfo, ProcInfos),
	map__lookup(ProcInfos, ProcId, ProcInfo),
	generate_proc_code(PredInfo, ProcInfo, ProcId, PredId, ModuleInfo0,
		Globals, ContInfo0, ContInfo1, CellCount0, CellCount1, Proc),
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Globals, ContInfo1, ContInfo, CellCount1, CellCount,
		[Proc | Procs0], Procs).

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

generate_proc_code(PredInfo, ProcInfo, ProcId, PredId, ModuleInfo, Globals,
		ContInfo0, ContInfo, CellCount0, CellCount, Proc) :-
	proc_info_interface_determinism(ProcInfo, Detism),
	proc_info_interface_code_model(ProcInfo, CodeModel),
	proc_info_goal(ProcInfo, Goal),
	proc_info_varset(ProcInfo, VarSet),
	proc_info_liveness_info(ProcInfo, Liveness),
	proc_info_stack_slots(ProcInfo, StackSlots),
	proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InitialInst),
	Goal = _ - GoalInfo,
	goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
	(
		MaybeFollowVars = yes(FollowVars)
	;
		MaybeFollowVars = no,
		map__init(FollowVars)
	),
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
	code_info__init(VarSet, Liveness, StackSlots, SaveSuccip, Globals,
		PredId, ProcId, ProcInfo, InitialInst, FollowVars,
		ModuleInfo, CellCount0, OutsideResumePoint, TraceSlotInfo,
		CodeInfo0),

		% Generate code for the procedure.
	generate_category_code(CodeModel, Goal, OutsideResumePoint,
		CodeTree, MaybeTraceCallLabel, FrameInfo, CodeInfo0, CodeInfo),
	code_info__get_cell_count(CellCount, CodeInfo, _),

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
	( BasicStackLayout = yes ->
			% Create the procedure layout structure.
		code_info__get_layout_info(LayoutInfo, CodeInfo, _),
		code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
			no, EntryLabel),
		continuation_info__add_proc_info(proc(PredId, ProcId),
			EntryLabel, TotalSlots, Detism, MaybeSuccipSlot,
			MaybeTraceCallLabel, TraceSlotInfo,
			ForceProcId, LayoutInfo, ContInfo0, ContInfo)
	;
		ContInfo = ContInfo0
	),

	predicate_name(ModuleInfo, PredId, Name),
	predicate_arity(ModuleInfo, PredId, Arity),
		% Construct a c_procedure structure with all the information.
	Proc = c_procedure(Name, Arity, proc(PredId, ProcId), Instructions).

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
	resume_point_info::in, code_tree::out, maybe(label)::out,
	frame_info::out, code_info::in, code_info::out) is det.

generate_category_code(model_det, Goal, ResumePoint, Code,
		MaybeTraceCallLabel, FrameInfo) -->
		% generate the code for the body of the clause
	(
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, middle_rec, yes) },
		middle_rec__match_and_generate(Goal, MiddleRecCode)
	->
		{ Code = MiddleRecCode },
		{ MaybeTraceCallLabel = no },
		{ FrameInfo = frame(0, no, no) }
	;
		code_info__get_maybe_trace_info(MaybeTraceInfo),
		( { MaybeTraceInfo = yes(TraceInfo) } ->
			trace__generate_external_event_code(call, TraceInfo,
				TraceCallLabel, _TypeInfos, TraceCallCode),
			{ MaybeTraceCallLabel = yes(TraceCallLabel) }
		;
			{ TraceCallCode = empty },
			{ MaybeTraceCallLabel = no }
		),
		code_gen__generate_goal(model_det, Goal, BodyCode),
		code_gen__generate_entry(model_det, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_det, FrameInfo, _, ExitCode),
		{ Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			     ExitCode)))
		}
	).

generate_category_code(model_semi, Goal, ResumePoint, Code,
		MaybeTraceCallLabel, FrameInfo) -->
	{ set__singleton_set(FailureLiveRegs, reg(r, 1)) },
	{ FailCode = node([
		assign(reg(r, 1), const(false)) - "Fail",
		livevals(FailureLiveRegs) - "",
		goto(succip) - "Return from procedure call"
	]) },
	code_info__get_maybe_trace_info(MaybeTraceInfo),
	( { MaybeTraceInfo = yes(TraceInfo) } ->
		trace__generate_external_event_code(call, TraceInfo,
			TraceCallLabel, _TypeInfos, TraceCallCode),
		{ MaybeTraceCallLabel = yes(TraceCallLabel) },
		code_gen__generate_goal(model_semi, Goal, BodyCode),
		code_gen__generate_entry(model_semi, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_semi, FrameInfo,
			RestoreDeallocCode, ExitCode),

		code_info__generate_resume_point(ResumePoint, ResumeCode),
		{ code_info__resume_point_vars(ResumePoint, ResumeVarList) },
		{ set__list_to_set(ResumeVarList, ResumeVars) },
		code_info__set_forward_live_vars(ResumeVars),
		trace__generate_external_event_code(fail, TraceInfo, _, _,
			TraceFailCode),
		{ Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(TraceFailCode,
			tree(RestoreDeallocCode,
			     FailCode)))))))
		}
	;
		{ MaybeTraceCallLabel = no },
		code_gen__generate_goal(model_semi, Goal, BodyCode),
		code_gen__generate_entry(model_semi, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_semi, FrameInfo,
			RestoreDeallocCode, ExitCode),
		code_info__generate_resume_point(ResumePoint, ResumeCode),
		{ Code =
			tree(EntryCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(RestoreDeallocCode,
			     FailCode)))))
		}
	).

generate_category_code(model_non, Goal, ResumePoint, Code,
		MaybeTraceCallLabel, FrameInfo) -->
	code_info__get_maybe_trace_info(MaybeTraceInfo),
	( { MaybeTraceInfo = yes(TraceInfo) } ->
		trace__generate_external_event_code(call, TraceInfo,
			TraceCallLabel, _TypeInfos, TraceCallCode),
		{ MaybeTraceCallLabel = yes(TraceCallLabel) },
		code_gen__generate_goal(model_non, Goal, BodyCode),
		code_gen__generate_entry(model_non, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_non, FrameInfo, _, ExitCode),

		code_info__generate_resume_point(ResumePoint, ResumeCode),
		{ code_info__resume_point_vars(ResumePoint, ResumeVarList) },
		{ set__list_to_set(ResumeVarList, ResumeVars) },
		code_info__set_forward_live_vars(ResumeVars),
		trace__generate_external_event_code(fail, TraceInfo, _, _,
			TraceFailCode),
		{ FailCode = node([
			goto(do_fail) - "fail after fail trace port"
		]) },
		{ Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(TraceFailCode,
			     FailCode))))))
		}
	;
		{ MaybeTraceCallLabel = no },
		code_gen__generate_goal(model_non, Goal, BodyCode),
		code_gen__generate_entry(model_non, Goal, ResumePoint,
			FrameInfo, EntryCode),
		code_gen__generate_exit(model_non, FrameInfo, _, ExitCode),
		{ Code =
			tree(EntryCode,
			tree(BodyCode,
			     ExitCode))
		}
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

:- pred code_gen__generate_entry(code_model::in, hlds_goal::in,
	resume_point_info::in, frame_info::out, code_tree::out,
	code_info::in, code_info::out) is det.

code_gen__generate_entry(CodeModel, Goal, OutsideResumePoint,
		FrameInfo, EntryCode) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_varset(VarSet),
	{ code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment) },
	{ StartComment = node([
		comment("Start of procedure prologue") - "",
		comment(SlotsComment) - ""
	]) },
	code_info__get_total_stackslot_count(MainSlots),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ LabelCode = node([
		label(Entry) - "Procedure entry point"
	]) },
	code_info__get_succip_used(Used),
	(
		% Do we need to save the succip across calls?
		{ Used = yes },
		% Do we need to use a general slot for storing succip?
		{ CodeModel \= model_non }
	->
		{ SuccipSlot is MainSlots + 1 },
		{ SaveSuccipCode = node([
			assign(stackvar(SuccipSlot), lval(succip)) -
				"Save the success ip"
		]) },
		{ TotalSlots = SuccipSlot },
		{ MaybeSuccipSlot = yes(SuccipSlot) }
	;
		{ SaveSuccipCode = empty },
		{ TotalSlots = MainSlots },
		{ MaybeSuccipSlot = no }
	),
	code_info__get_maybe_trace_info(MaybeTraceInfo),
	( { MaybeTraceInfo = yes(TraceInfo) } ->
		trace__generate_slot_fill_code(TraceInfo, TraceFillCode)
	;
		{ TraceFillCode = empty }
	),

	{ predicate_module(ModuleInfo, PredId, ModuleName) },
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameString) },
	{ string__int_to_string(Arity, ArityStr) },
	{ string__append_list([ModuleNameString, ":", PredName, "/", ArityStr],
		PushMsg) },
	(
		{ CodeModel = model_non }
	->
		{ code_info__resume_point_stack_addr(OutsideResumePoint,
			OutsideResumeAddress) },
		(
			{ Goal = pragma_c_code(_,_,_,_,_,_, PragmaCode) - _},
			{ PragmaCode = nondet(Fields, FieldsContext,
				_,_,_,_,_,_,_) }
		->
			{ pragma_c_gen__struct_name(ModuleName, PredName,
				Arity, ProcId, StructName) },
			{ Struct = pragma_c_struct(StructName,
				Fields, FieldsContext) },
			{ string__format("#define\tMR_ORDINARY_SLOTS\t%d\n",
				[i(TotalSlots)], DefineStr) },
			{ DefineComponents = [pragma_c_raw_code(DefineStr)] },
			{ NondetFrameInfo = ordinary_frame(PushMsg, TotalSlots,
				yes(Struct)) },
			{ AllocCode = node([
				mkframe(NondetFrameInfo, OutsideResumeAddress)
					- "Allocate stack frame",
				pragma_c([], DefineComponents,
					will_not_call_mercury, no, no)
					- ""
			]) },
			{ NondetPragma = yes }
		;
			{ NondetFrameInfo = ordinary_frame(PushMsg, TotalSlots,
				no) },
			{ AllocCode = node([
				mkframe(NondetFrameInfo, OutsideResumeAddress)
					- "Allocate stack frame"
			]) },
			{ NondetPragma = no }
		)
	;
		{ TotalSlots > 0 }
	->
		{ AllocCode = node([
			incr_sp(TotalSlots, PushMsg) -
				"Allocate stack frame"
		]) },
		{ NondetPragma = no }
	;
		{ AllocCode = empty },
		{ NondetPragma = no }
	),
	{ FrameInfo = frame(TotalSlots, MaybeSuccipSlot, NondetPragma) },
	{ EndComment = node([
		comment("End of procedure prologue") - ""
	]) },
	{ EntryCode =
		tree(StartComment,
		tree(LabelCode,
		tree(AllocCode,
		tree(SaveSuccipCode,
		tree(TraceFillCode,
		     EndComment)))))
	}.

%---------------------------------------------------------------------------%

	% Generate the success epilogue for a procedure.
	%
	% The success epilogue will contain
	%
	%	a comment to mark epilogue start
	%	code to place the output arguments where their caller expects
	%	code to restore registers from some special slots
	%	code to deallocate the stack frame
	%	code to set r1 to TRUE (for semidet procedures only)
	%	a jump back to the caller, including livevals information
	%	a comment to mark epilogue end
	%
	% The parts of this that restore registers and deallocate the stack
	% frame are also part of the failure epilog, which is handled by
	% our caller; this is why we return RestoreDeallocCode.
	%
	% At the moment the only special slots are the succip slot, and
	% the slots holding the call number and call depth for tracing.
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
	code_tree::out, code_tree::out, code_info::in, code_info::out) is det.

code_gen__generate_exit(CodeModel, FrameInfo, RestoreDeallocCode, ExitCode) -->
	{ StartComment = node([
		comment("Start of procedure epilogue") - ""
	]) },
	{ EndComment = node([
		comment("End of procedure epilogue") - ""
	]) },
	{ FrameInfo = frame(TotalSlots, MaybeSuccipSlot, NondetPragma) },
	( { NondetPragma = yes } ->
		{ UndefStr = "#undef\tMR_ORDINARY_SLOTS\n" },
		{ UndefComponents = [pragma_c_raw_code(UndefStr)] },
		{ UndefCode = node([
			pragma_c([], UndefComponents,
				will_not_call_mercury, no, no)
				- ""
		]) },
		{ RestoreDeallocCode = empty },	% always empty for nondet code
		{ ExitCode =
			tree(StartComment,
			tree(UndefCode,
			     EndComment))
		}
	;
		code_info__get_instmap(Instmap),
		code_info__get_arginfo(ArgModes),
		code_info__get_headvars(HeadVars),
		{ assoc_list__from_corresponding_lists(HeadVars, ArgModes,
			Args)},
		(
			{ instmap__is_unreachable(Instmap) }
		->
			{ FlushCode = empty }
		;
			code_info__setup_call(Args, callee, FlushCode)
		),
		(
			{ MaybeSuccipSlot = yes(SuccipSlot) }
		->
			{ RestoreSuccipCode = node([
				assign(succip, lval(stackvar(SuccipSlot))) -
					"restore the success ip"
			]) }
		;
			{ RestoreSuccipCode = empty }
		),
		(
			{ TotalSlots = 0 ; CodeModel = model_non }
		->
			{ DeallocCode = empty }
		;
			{ DeallocCode = node([
				decr_sp(TotalSlots) - "Deallocate stack frame"
			]) }
		),
		{ RestoreDeallocCode = tree(RestoreSuccipCode, DeallocCode ) },

		code_info__get_maybe_trace_info(MaybeTraceInfo),
		( { MaybeTraceInfo = yes(TraceInfo) } ->
			trace__generate_external_event_code(exit, TraceInfo,
				_, TypeInfoDatas, TraceExitCode),
			{ map__values(TypeInfoDatas, TypeInfoLocnSets) },
			{ FindBaseLvals = lambda([Lval::out] is nondet, (
				list__member(LocnSet, TypeInfoLocnSets),
				set__member(Locn, LocnSet),
				(
					Locn = direct(Lval)
				;
					Locn = indirect(Lval, _)
				)
			)) },
			{ solutions(FindBaseLvals, TypeInfoLvals) }
		;
			{ TraceExitCode = empty },
			{ TypeInfoLvals = [] }
		),

			% Find out which locations should be mentioned
			% in the success path livevals(...) annotation,
			% so that value numbering doesn't optimize them away.
		{ code_gen__select_args_with_mode(Args, top_out, _OutVars,
			OutLvals) },
		{ list__append(TypeInfoLvals, OutLvals, LiveArgLvals) },
		{ set__list_to_set(LiveArgLvals, LiveArgs) },

		(
			{ CodeModel = model_det },
			{ SuccessCode = node([
				livevals(LiveArgs) - "",
				goto(succip) - "Return from procedure call"
			]) },
			{ AllSuccessCode =
				tree(TraceExitCode,
				tree(RestoreDeallocCode,
				     SuccessCode))
			}
		;
			{ CodeModel = model_semi },
			{ set__insert(LiveArgs, reg(r, 1), SuccessLiveRegs) },
			{ SuccessCode = node([
				assign(reg(r, 1), const(true)) - "Succeed",
				livevals(SuccessLiveRegs) - "",
				goto(succip) - "Return from procedure call"
			]) },
			{ AllSuccessCode =
				tree(TraceExitCode,
				tree(RestoreDeallocCode,
				     SuccessCode))
			}
		;
			{ CodeModel = model_non },
			{ MaybeTraceInfo = yes(TraceInfo2) ->
				trace__maybe_setup_redo_event(TraceInfo2,
					SetupRedoCode)
			;
				SetupRedoCode = empty
			},
			{ SuccessCode = node([
				livevals(LiveArgs) - "",
				goto(do_succeed(no))
					- "Return from procedure call"
			]) },
			{ AllSuccessCode =
				tree(SetupRedoCode,
				tree(TraceExitCode,
				     SuccessCode))
			}
		),
		{ ExitCode =
			tree(StartComment,
			tree(FlushCode,
			tree(AllSuccessCode,
			     EndComment)))
		}
	).

%---------------------------------------------------------------------------%

% Generate a goal. This predicate arranges for the necessary updates of
% the generic data structures before and after the actual code generation,
% which is delegated to goal-specific predicates.

code_gen__generate_goal(ContextModel, Goal - GoalInfo, Code) -->
		% Make any changes to liveness before Goal
	{ goal_is_atomic(Goal) ->
		IsAtomic = yes
	;
		IsAtomic = no
	},
	code_info__pre_goal_update(GoalInfo, IsAtomic),
	code_info__get_instmap(Instmap),
	(
		{ instmap__is_reachable(Instmap) }
	->
		{ goal_info_get_code_model(GoalInfo, CodeModel) },

			% sanity check: code of some code models
			% should occur only in limited contexts
		{
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
		},

		code_gen__generate_goal_2(Goal, GoalInfo, CodeModel, Code),

			% Make live any variables which subsequent goals
			% will expect to be live, but were not generated
		code_info__set_instmap(Instmap),
		code_info__post_goal_update(GoalInfo)
	;
		{ Code = empty }
	),
	!.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
	code_model::in, code_tree::out, code_info::in, code_info::out) is det.

code_gen__generate_goal_2(unify(_, _, _, Uni, _), _, CodeModel, Code) -->
	unify_gen__generate_unification(CodeModel, Uni, Code).
code_gen__generate_goal_2(conj(Goals), _GoalInfo, CodeModel, Code) -->
	code_gen__generate_goals(Goals, CodeModel, Code).
code_gen__generate_goal_2(par_conj(Goals, _SM), GoalInfo, CodeModel, Code) -->
	par_conj_gen__generate_par_conj(Goals, GoalInfo, CodeModel, Code).
code_gen__generate_goal_2(disj(Goals, StoreMap), _, CodeModel, Code) -->
	disj_gen__generate_disj(CodeModel, Goals, StoreMap, Code).
code_gen__generate_goal_2(not(Goal), _GoalInfo, CodeModel, Code) -->
	ite_gen__generate_negation(CodeModel, Goal, Code).
code_gen__generate_goal_2(if_then_else(_Vars, Cond, Then, Else, StoreMap),
		_GoalInfo, CodeModel, Code) -->
	ite_gen__generate_ite(CodeModel, Cond, Then, Else, StoreMap, Code).
code_gen__generate_goal_2(switch(Var, CanFail, CaseList, StoreMap),
		GoalInfo, CodeModel, Code) -->
	switch_gen__generate_switch(CodeModel, Var, CanFail, CaseList,
		StoreMap, GoalInfo, Code).
code_gen__generate_goal_2(some(_Vars, Goal), _GoalInfo, CodeModel, Code) -->
	commit_gen__generate_commit(CodeModel, Goal, Code).
code_gen__generate_goal_2(higher_order_call(PredVar, Args, Types,
		Modes, Det, _PredOrFunc), GoalInfo, CodeModel, Code) -->
	call_gen__generate_higher_order_call(CodeModel, PredVar, Args,
		Types, Modes, Det, GoalInfo, Code).
code_gen__generate_goal_2(class_method_call(TCVar, Num, Args, Types,
		Modes, Det), GoalInfo, CodeModel, Code) -->
	call_gen__generate_class_method_call(CodeModel, TCVar, Num, Args,
		Types, Modes, Det, GoalInfo, Code).
code_gen__generate_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
		GoalInfo, CodeModel, Code) -->
	(
		{ BuiltinState = not_builtin }
	->
		call_gen__generate_call(CodeModel, PredId, ProcId, Args,
			GoalInfo, Code)
	;
		call_gen__generate_builtin(CodeModel, PredId, ProcId, Args,
			Code)
	).
code_gen__generate_goal_2(pragma_c_code(Attributes,
		PredId, ModeId, Args, ArgNames, OrigArgTypes, PragmaCode),
		GoalInfo, CodeModel, Instr) -->
	pragma_c_gen__generate_pragma_c_code(CodeModel, Attributes,
		PredId, ModeId, Args, ArgNames, OrigArgTypes, GoalInfo,
		PragmaCode, Instr).

%---------------------------------------------------------------------------%

% Generate a conjoined series of goals.
% Note of course, that with a conjunction, state information
% flows directly from one conjunct to the next.

:- pred code_gen__generate_goals(hlds_goals::in, code_model::in,
	code_tree::out, code_info::in, code_info::out) is det.

code_gen__generate_goals([], _, empty) --> [].
code_gen__generate_goals([Goal | Goals], CodeModel, Instr) -->
	code_gen__generate_goal(CodeModel, Goal, Instr1),
	code_info__get_instmap(Instmap),
	(
		{ instmap__is_unreachable(Instmap) }
	->
		{ Instr = Instr1 }
	;
		code_gen__generate_goals(Goals, CodeModel, Instr2),
		{ Instr = tree(Instr1, Instr2) }
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
		Instrn0 = call(Target, ReturnLabel, LiveVals0, CM)
	->
		map__init(Empty),
		Instrn  = call(Target, ReturnLabel, 
			[live_lvalue(direct(stackvar(StackLoc)),
				succip, Empty) |
			LiveVals0], CM)
	;
		Instrn = Instrn0
	),
	code_gen__add_saved_succip(Instrns0, StackLoc, Instrns).

%---------------------------------------------------------------------------%
