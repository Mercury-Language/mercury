%---------------------------------------------------------------------------%
% Copyright (C) 1996-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Code generation - convert from HLDS to LLDS.
%
% Main author: conway.
%
% Notes:
%
%	code_gen forwards most of the actual construction of intruction
%	sequences to code_info, and other modules. The generation of
%	calls is done by call_gen, switches by switch_gen, if-then-elses
%	by ite_gen, unifications by unify_gen, disjunctions by disj_gen,
%	and pragma_c_codes by pragma_c_gen.
%
%	The general scheme for generating semideterministic code is
%	to treat it as deterministic code, and have a fall-through
%	point for failure.  Semideterministic procedures leave a 'true'
%	in register r(1) to indicate success, and 'false' to indicate
%	failure.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_gen.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, llds, code_info.
:- import_module continuation_info, globals.
:- import_module set, list, assoc_list, term, io.

		% Translate a HLDS structure into an LLDS

:- pred generate_code(module_info, module_info, list(c_procedure),
						io__state, io__state).
:- mode generate_code(in, out, out, di, uo) is det.

:- pred generate_proc_code(proc_info, proc_id, pred_id, module_info, globals,
	continuation_info, int, continuation_info, int, c_procedure).
:- mode generate_proc_code(in, in, in, in, in, in, in, out, out, out) is det.

		% This predicate generates code for a goal.

:- pred code_gen__generate_goal(code_model, hlds_goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_goal(in, in, out, in, out) is det.

:- pred code_gen__output_args(assoc_list(var, arg_info), set(lval)).
:- mode code_gen__output_args(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module call_gen, unify_gen, ite_gen, switch_gen, disj_gen.
:- import_module par_conj_gen, pragma_c_gen, trace, options, hlds_out.
:- import_module code_aux, middle_rec, passes_aux, llds_out.
:- import_module code_util, type_util, mode_util.
:- import_module prog_data, prog_out, instmap.
:- import_module bool, char, int, string.
:- import_module map, tree, std_util, require, varset.

%---------------------------------------------------------------------------%

% For a set of high level data structures and associated data, given in
% ModuleInfo, generate a list of c_procedure structures.

generate_code(ModuleInfo0, ModuleInfo, Procedures) -->
		% get a list of all the predicate ids
		% for which we are going to generate code.
	{ module_info_predids(ModuleInfo0, PredIds) },
		% now generate the code for each predicate
	generate_pred_list_code(ModuleInfo0, ModuleInfo, PredIds, Procedures).

% Generate a list of c_procedure structures for each mode of each
% predicate given in ModuleInfo

:- pred generate_pred_list_code(module_info, module_info, list(pred_id), 
				list(c_procedure), io__state, io__state).
:- mode generate_pred_list_code(in, out, in, out, di, uo) is det.

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

% For the predicate identified by PredId, with the the associated
% data in ModuleInfo, generate a code_tree.

:- pred generate_pred_code(module_info, module_info, pred_id, pred_info,
		list(proc_id), list(c_procedure), io__state, io__state).
:- mode generate_pred_code(in, out, in, in, in, out, di, uo) is det.

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
		% generate all the procedures for this predicate
	{ module_info_get_continuation_info(ModuleInfo0, ContInfo0) },
	{ module_info_get_cell_count(ModuleInfo0, CellCount0) },
	globals__io_get_globals(Globals),
	{ generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Globals, ContInfo0, ContInfo, CellCount0, CellCount,
		[], Code) },
	{ module_info_set_cell_count(ModuleInfo0, CellCount, ModuleInfo1) },
	{ module_info_set_continuation_info(ModuleInfo1, ContInfo, 
		ModuleInfo) }.

% For all the modes of predicate PredId, generate the appropriate
% code (deterministic, semideterministic, or nondeterministic).

:- pred generate_proc_list_code(list(proc_id), pred_id, pred_info, module_info,
	globals, continuation_info, continuation_info, int, int,
	list(c_procedure), list(c_procedure)).
% :- mode generate_proc_list_code(in, in, in, in, in, di, uo, di, uo)
%	is det.
:- mode generate_proc_list_code(in, in, in, in, in, in, out, in, out, in, out)
	is det.

generate_proc_list_code([], _PredId, _PredInfo, _ModuleInfo, _Globals,
		ContInfo, ContInfo, CellCount, CellCount, Procs, Procs).
generate_proc_list_code([ProcId | ProcIds], PredId, PredInfo, ModuleInfo0,
		Globals, ContInfo0, ContInfo, CellCount0, CellCount,
		Procs0, Procs) :-
	pred_info_procedures(PredInfo, ProcInfos),
		% locate the proc_info structure for this mode of the predicate
	map__lookup(ProcInfos, ProcId, ProcInfo),
	generate_proc_code(ProcInfo, ProcId, PredId, ModuleInfo0, Globals,
		ContInfo0, CellCount0, ContInfo1, CellCount1, Proc),
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Globals, ContInfo1, ContInfo, CellCount1, CellCount,
		[Proc | Procs0], Procs).

%---------------------------------------------------------------------------%

	% Values of this type hold information about stack frames that is
	% generated when generating prologs and is used in generating epilogs
	% and when massaging the code generated for the procedure.

:- type frame_info	--->	frame(
					int, 	    % Number of slots in frame.

					maybe(int), % Slot number of succip
						    % if succip is present
						    % in a general slot.

					bool	    % Is this the frame of a
						    % model_non proc defined
						    % via pragma C code?
				).

%---------------------------------------------------------------------------%

generate_proc_code(ProcInfo, ProcId, PredId, ModuleInfo, Globals,
		ContInfo0, CellCount0, ContInfo, CellCount, Proc) :-
		% find out if the proc is deterministic/etc
	proc_info_interface_determinism(ProcInfo, Detism),
	proc_info_interface_code_model(ProcInfo, CodeModel),
		% get the goal for this procedure
	proc_info_goal(ProcInfo, Goal),
		% get the information about this procedure that we need.
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
	globals__lookup_bool_option(Globals, basic_stack_layout,
		BasicStackLayout),
	( BasicStackLayout = yes ->
		SaveSuccip = yes
	;
		SaveSuccip = no
	),
		% initialise the code_info structure 
	code_info__init(VarSet, Liveness, StackSlots, SaveSuccip, Globals,
		PredId, ProcId, ProcInfo, InitialInst, FollowVars,
		ModuleInfo, CellCount0, CodeInfo0),
		% generate code for the procedure
	globals__get_trace_level(Globals, TraceLevel),
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	(
		( TraceLevel = interface ; TraceLevel = full )
	->
		trace__setup(TraceLevel, CodeInfo0, CodeInfo1)
	;
		CodeInfo1 = CodeInfo0
	),
	generate_category_code(CodeModel, Goal, ProcInfo, CodeTree,
		MaybeTraceCallLabel, FrameInfo, CodeInfo1, CodeInfo),
		% extract the new continuation_info and cell count
	code_info__get_cell_count(CellCount, CodeInfo, _CodeInfo1),

		% turn the code tree into a list
	tree__flatten(CodeTree, FragmentList),
		% now the code is a list of code fragments (== list(instr)),
		% so we need to do a level of unwinding to get a flat list.
	list__condense(FragmentList, Instructions0),
	FrameInfo = frame(TotalSlots, MaybeSuccipSlot, _),
	(
		MaybeSuccipSlot = yes(SuccipSlot)
	->
		code_gen__add_saved_succip(Instructions0,
			SuccipSlot, Instructions)
	;
		Instructions = Instructions0
	),
	( BasicStackLayout = yes ->
		code_info__get_layout_info(LayoutInfo, CodeInfo, _CodeInfo2),
		continuation_info__add_proc_info(proc(PredId, ProcId),
			ProcLabel, TotalSlots, Detism, MaybeSuccipSlot,
			MaybeTraceCallLabel, LayoutInfo, ContInfo0, ContInfo)
	;
		ContInfo = ContInfo0
	),

		% get the name and arity of this predicate
	predicate_name(ModuleInfo, PredId, Name),
	predicate_arity(ModuleInfo, PredId, Arity),
		% construct a c_procedure structure with all the information
	Proc = c_procedure(Name, Arity, proc(PredId, ProcId), Instructions).

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

:- pred generate_category_code(code_model, hlds_goal, proc_info, code_tree,
	maybe(label), frame_info, code_info, code_info).
:- mode generate_category_code(in, in, in, out, out, out, in, out) is det.

generate_category_code(model_det, Goal, ProcInfo, Code,
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
		% make a new failure cont (not model_non);
		% this continuation is never actually used,
		% but is a place holder
		code_info__manufacture_failure_cont(no),

		code_info__get_maybe_trace_info(MaybeTraceInfo),
		( { MaybeTraceInfo = yes(TraceInfo) } ->
			code_info__get_module_info(ModuleInfo),
			{ trace__fail_vars(ModuleInfo, ProcInfo, ResumeVars) },
				% Protect these vars from being forgotten,
				% so they will be around for the exit trace.
			code_info__push_resume_point_vars(ResumeVars),
			trace__generate_external_event_code(call, TraceInfo,
				TraceCallLabel, _TypeInfos, TraceCallCode),
			{ MaybeTraceCallLabel = yes(TraceCallLabel) }
		;
			{ TraceCallCode = empty },
			{ MaybeTraceCallLabel = no }
		),
		code_gen__generate_goal(model_det, Goal, BodyCode),
		code_gen__generate_entry(model_det, Goal, FrameInfo,
			EntryCode),
		code_gen__generate_exit(model_det, FrameInfo, _, ExitCode),
		{ Code =
			tree(EntryCode,
			tree(TraceCallCode,
			tree(BodyCode,
			     ExitCode)))
		}
	).

generate_category_code(model_semi, Goal, ProcInfo, Code,
		MaybeTraceCallLabel, FrameInfo) -->
		% make a new failure cont (not model_non)
	code_info__manufacture_failure_cont(no),
	code_info__get_maybe_trace_info(MaybeTraceInfo),
	{ set__singleton_set(FailureLiveRegs, reg(r, 1)) },
	{ FailCode = node([
		assign(reg(r, 1), const(false)) - "Fail",
		livevals(FailureLiveRegs) - "",
		goto(succip) - "Return from procedure call"
	]) },
	( { MaybeTraceInfo = yes(TraceInfo) } ->
		code_info__get_module_info(ModuleInfo),
		{ trace__fail_vars(ModuleInfo, ProcInfo, ResumeVars) },
		code_info__make_known_failure_cont(ResumeVars, orig_and_stack,
			no, SetupCode),
		code_info__push_resume_point_vars(ResumeVars),
		trace__generate_external_event_code(call, TraceInfo,
			TraceCallLabel, _TypeInfos, TraceCallCode),
		{ MaybeTraceCallLabel = yes(TraceCallLabel) },
		code_gen__generate_goal(model_semi, Goal, BodyCode),
		code_gen__generate_entry(model_semi, Goal, FrameInfo,
			EntryCode),
		code_gen__generate_exit(model_semi, FrameInfo,
			RestoreDeallocCode, ExitCode),
		code_info__pop_resume_point_vars,
		code_info__restore_failure_cont(ResumeCode),
		code_info__set_forward_live_vars(ResumeVars),
		trace__generate_external_event_code(fail, TraceInfo, _, _,
			TraceFailCode),
		{ Code =
			tree(EntryCode,
			tree(SetupCode,
			tree(TraceCallCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(TraceFailCode,
			tree(RestoreDeallocCode,
			     FailCode))))))))
		}
	;
		{ MaybeTraceCallLabel = no },
		code_gen__generate_goal(model_semi, Goal, BodyCode),
		code_gen__generate_entry(model_semi, Goal, FrameInfo,
			EntryCode),
		code_gen__generate_exit(model_semi, FrameInfo,
			RestoreDeallocCode, ExitCode),
		code_info__restore_failure_cont(ResumeCode),
		{ Code =
			tree(EntryCode,
			tree(BodyCode,
			tree(ExitCode,
			tree(ResumeCode,
			tree(RestoreDeallocCode,
			     FailCode)))))
		}
	).

generate_category_code(model_non, Goal, ProcInfo, Code,
		MaybeTraceCallLabel, FrameInfo) -->
		% make a new failure cont (yes, it is model_non)
	code_info__manufacture_failure_cont(yes),
		% we must arrange the tracing of failure out of this proc
	code_info__get_maybe_trace_info(MaybeTraceInfo),
	( { MaybeTraceInfo = yes(TraceInfo) } ->
		code_info__get_module_info(ModuleInfo),
		{ trace__fail_vars(ModuleInfo, ProcInfo, ResumeVars) },
		code_info__make_known_failure_cont(ResumeVars, orig_and_stack,
			yes, SetupCode),
		code_info__push_resume_point_vars(ResumeVars),
		trace__generate_external_event_code(call, TraceInfo,
			TraceCallLabel, _TypeInfos, TraceCallCode),
		{ MaybeTraceCallLabel = yes(TraceCallLabel) },
		code_gen__generate_goal(model_non, Goal, BodyCode),
		code_gen__generate_entry(model_non, Goal, FrameInfo,
			PrologCode),
		code_gen__generate_exit(model_non, FrameInfo, _, EpilogCode),

		code_info__pop_resume_point_vars,
		code_info__restore_failure_cont(RestoreCode),
		code_info__set_forward_live_vars(ResumeVars),
		trace__generate_external_event_code(fail, TraceInfo, _, _,
			TraceFailCode),
		code_info__generate_failure(FailCode),
		{ Code =
			tree(PrologCode,
			tree(SetupCode,
			tree(TraceCallCode,
			tree(BodyCode,
			tree(EpilogCode,
			tree(RestoreCode,
			tree(TraceFailCode,
			     FailCode)))))))
		}
	;
		{ MaybeTraceCallLabel = no },
		code_gen__generate_goal(model_non, Goal, BodyCode),
		code_gen__generate_entry(model_non, Goal, FrameInfo,
			PrologCode),
		code_gen__generate_exit(model_non, FrameInfo, _, EpilogCode),
		{ Code =
			tree(PrologCode,
			tree(BodyCode,
			     EpilogCode))
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

:- pred code_gen__generate_entry(code_model, hlds_goal, frame_info,
	code_tree, code_info, code_info).
:- mode code_gen__generate_entry(in, in, out, out, in, out) is det.

code_gen__generate_entry(CodeModel, Goal, FrameInfo, PrologCode) -->
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
		{ trace__generate_slot_fill_code(TraceInfo, TraceFillCode) }
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
			{ AllocCode = node([
				mkframe(PushMsg, TotalSlots, yes(Struct),
					do_fail)
					- "Allocate stack frame",
				pragma_c([], DefineComponents,
					will_not_call_mercury, no, no)
					- ""
			]) },
			{ NondetPragma = yes }
		;
			{ AllocCode = node([
				mkframe(PushMsg, TotalSlots, no, do_fail) -
					"Allocate stack frame"
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
	{ PrologCode =
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

:- pred code_gen__generate_exit(code_model, frame_info, code_tree, code_tree,
	code_info, code_info).
:- mode code_gen__generate_exit(in, in, out, out, in, out) is det.

code_gen__generate_exit(CodeModel, FrameInfo, RestoreDeallocCode, EpilogCode)
		-->
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
		{ EpilogCode =
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
			{ assoc_list__values(TypeInfoDatas, TypeInfoLvals) }
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
			{ SuccessCode = node([
				livevals(LiveArgs) - "",
				goto(do_succeed(no))
					- "Return from procedure call"
			]) },
			{ AllSuccessCode =
				tree(TraceExitCode,
				     SuccessCode)
			}
		),
		{ EpilogCode =
			tree(StartComment,
			tree(FlushCode,
			tree(AllSuccessCode,
			     EndComment)))
		}
	).

%---------------------------------------------------------------------------%

% Generate a goal. This predicate arranges for the necessary updates of
% the generic data structures before and after the actual code generation,
% which is delegated to context-specific predicates.

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
		(
			{ CodeModel = model_det },
			code_gen__generate_det_goal_2(Goal, GoalInfo, Code)
		;
			{ CodeModel = model_semi },
			( { ContextModel \= model_det } ->
				code_gen__generate_semi_goal_2(Goal, GoalInfo,
					Code)
			;
				{ error("semidet model in det context") }
			)
		;
			{ CodeModel = model_non },
			( { ContextModel = model_non } ->
				code_gen__generate_non_goal_2(Goal, GoalInfo,
					Code)
			;
				{ error("nondet model in det/semidet context") }
			)
		),
			% Make live any variables which subsequent goals
			% will expect to be live, but were not generated
		code_info__set_instmap(Instmap),
		code_info__post_goal_update(GoalInfo)
	;
		{ Code = empty }
	),
	!.

%---------------------------------------------------------------------------%

% Generate a conjoined series of goals.
% Note of course, that with a conjunction, state information
% flows directly from one conjunct to the next.

:- pred code_gen__generate_goals(hlds_goals, code_model, code_tree,
							code_info, code_info).
:- mode code_gen__generate_goals(in, in, out, in, out) is det.

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

:- pred code_gen__generate_det_goal_2(hlds_goal_expr, hlds_goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_det_goal_2(in, in, out, in, out) is det.

code_gen__generate_det_goal_2(conj(Goals), _GoalInfo, Instr) -->
	code_gen__generate_goals(Goals, model_det, Instr).
code_gen__generate_det_goal_2(par_conj(Goals, _StoreMap), GoalInfo, Instr) -->
	par_conj_gen__generate_det_par_conj(Goals, GoalInfo, Instr).
code_gen__generate_det_goal_2(some(_Vars, Goal), _GoalInfo, Instr) -->
	{ Goal = _ - InnerGoalInfo },
	{ goal_info_get_code_model(InnerGoalInfo, CodeModel) },
	(
		{ CodeModel = model_det },
		code_gen__generate_goal(model_det, Goal, Instr)
	;
		{ CodeModel = model_semi },
		{ error("semidet model in det context") }
	;
		{ CodeModel = model_non },
		code_info__generate_det_pre_commit(Slots, PreCommit),
		code_gen__generate_goal(model_non, Goal, GoalCode),
		code_info__generate_det_commit(Slots, Commit),
		{ Instr = tree(PreCommit, tree(GoalCode, Commit)) }
	).
code_gen__generate_det_goal_2(disj(Goals, StoreMap), _GoalInfo, Instr) -->
	disj_gen__generate_det_disj(Goals, StoreMap, Instr).
code_gen__generate_det_goal_2(not(Goal), _GoalInfo, Instr) -->
	code_gen__generate_negation(model_det, Goal, Instr).
code_gen__generate_det_goal_2(higher_order_call(PredVar, Args, Types,
		Modes, Det, _PredOrFunc),
		GoalInfo, Instr) -->
	call_gen__generate_higher_order_call(model_det, PredVar, Args,
		Types, Modes, Det, GoalInfo, Instr).
code_gen__generate_det_goal_2(class_method_call(TCVar, Num, Args, Types,
		ArgModes, Det),
		GoalInfo, Instr) -->
	call_gen__generate_class_method_call(model_det, TCVar, Num, Args,
		Types, ArgModes, Det, GoalInfo, Instr).
code_gen__generate_det_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
		GoalInfo, Instr) -->
	(
		{ BuiltinState = not_builtin }
	->
		code_info__succip_is_used,
		call_gen__generate_call(model_det, PredId, ProcId, Args,
			GoalInfo, Instr)
	;
		call_gen__generate_det_builtin(PredId, ProcId, Args, Instr)
	).
code_gen__generate_det_goal_2(switch(Var, CanFail, CaseList, StoreMap),
		GoalInfo, Instr) -->
	switch_gen__generate_switch(model_det, Var, CanFail, CaseList,
		StoreMap, GoalInfo, Instr).
code_gen__generate_det_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal, StoreMap),
							_GoalInfo, Instr) -->
	ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, StoreMap,
		Instr).
code_gen__generate_det_goal_2(unify(_L, _R, _U, Uni, _C), _GoalInfo, Instr) -->
	(
		{ Uni = assign(Left, Right) },
		unify_gen__generate_assignment(Left, Right, Instr)
	;
		{ Uni = construct(Var, ConsId, Args, Modes) },
		unify_gen__generate_construction(Var, ConsId, Args,
								Modes, Instr)
	;
		{ Uni = deconstruct(Var, ConsId, Args, Modes, _Det) },
		unify_gen__generate_det_deconstruction(Var, ConsId, Args,
								Modes, Instr)
	;
		% These should have been transformed into calls by
		% polymorphism.m.
		{ Uni = complicated_unify(_UniMode, _CanFail) },
		{ error("code_gen__generate_det_goal_2 - complicated unify") }
	;
		{ Uni = simple_test(_, _) },
		{ error("generate_det_goal_2: cannot have det simple_test") }
	).

code_gen__generate_det_goal_2(pragma_c_code(MayCallMercury,
		PredId, ModeId, Args, ArgNames, OrigArgTypes, PragmaCode),
		GoalInfo, Instr) -->
	{ ArgNames = pragma_c_code_arg_info(_InstTable, Names) },
	pragma_c_gen__generate_pragma_c_code(model_det, MayCallMercury,
		PredId, ModeId, Args, Names, OrigArgTypes, GoalInfo,
		PragmaCode, Instr).

%---------------------------------------------------------------------------%

:- pred code_gen__generate_semi_goal_2(hlds_goal_expr, hlds_goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_semi_goal_2(in, in, out, in, out) is det.

code_gen__generate_semi_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_goals(Goals, model_semi, Code).
code_gen__generate_semi_goal_2(par_conj(_Goals, _SM), _GoalInfo, _Code) -->
	% Determinism analysis will report a determinism error if the
	% parallel conj is not det.
	{ error("sorry, semidet parallel conjunction not implemented") }.
code_gen__generate_semi_goal_2(some(_Vars, Goal), _GoalInfo, Code) -->
	{ Goal = _ - InnerGoalInfo },
	{ goal_info_get_code_model(InnerGoalInfo, CodeModel) },
	(
		{ CodeModel = model_det },
		code_gen__generate_goal(model_det, Goal, Code)
	;
		{ CodeModel = model_semi },
		code_gen__generate_goal(model_semi, Goal, Code)
	;
		{ CodeModel = model_non },
		code_info__generate_semi_pre_commit(Label, Slots, PreCommit),
		code_gen__generate_goal(model_non, Goal, GoalCode),
		code_info__generate_semi_commit(Label, Slots, Commit),
		{ Code = tree(PreCommit, tree(GoalCode, Commit)) }
	).
code_gen__generate_semi_goal_2(disj(Goals, StoreMap), _GoalInfo, Code) -->
	disj_gen__generate_semi_disj(Goals, StoreMap, Code).
code_gen__generate_semi_goal_2(not(Goal), _GoalInfo, Code) -->
	code_gen__generate_negation(model_semi, Goal, Code).
code_gen__generate_semi_goal_2(higher_order_call(PredVar, Args, Types, Modes,
		Det, _PredOrFunc), GoalInfo, Code) -->
	call_gen__generate_higher_order_call(model_semi, PredVar, Args,
		Types, Modes, Det, GoalInfo, Code).
code_gen__generate_semi_goal_2(class_method_call(TCVar, Num, Args, Types,
		ArgModes, Det), GoalInfo, Code) -->
	call_gen__generate_class_method_call(model_semi, TCVar, Num, Args,
		Types, ArgModes, Det, GoalInfo, Code).
code_gen__generate_semi_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
							GoalInfo, Code) -->
	(
		{ BuiltinState = not_builtin }
	->
		code_info__succip_is_used,
		call_gen__generate_call(model_semi, PredId, ProcId, Args,
			GoalInfo, Code)
	;
		call_gen__generate_semidet_builtin(PredId, ProcId, Args, Code)
	).
code_gen__generate_semi_goal_2(switch(Var, CanFail, CaseList, StoreMap),
		GoalInfo, Instr) -->
	switch_gen__generate_switch(model_semi, Var, CanFail,
		CaseList, StoreMap, GoalInfo, Instr).
code_gen__generate_semi_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal, StoreMap),
							_GoalInfo, Instr) -->
	ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, StoreMap,
		Instr).
code_gen__generate_semi_goal_2(unify(_L, _R, _U, Uni, _C),
							_GoalInfo, Code) -->
	(
		{ Uni = assign(Left, Right) },
		unify_gen__generate_assignment(Left, Right, Code)
	;
		{ Uni = construct(Var, ConsId, Args, Modes) },
		unify_gen__generate_construction(Var, ConsId, Args,
								Modes, Code)
	;
		{ Uni = deconstruct(Var, ConsId, Args, Modes, _) },
		unify_gen__generate_semi_deconstruction(Var, ConsId, Args,
								Modes, Code)
	;
		{ Uni = simple_test(Var1, Var2) },
		unify_gen__generate_test(Var1, Var2, Code)
	;
		{ Uni = complicated_unify(_UniMode, _CanFail) },
		{ error("code_gen__generate_semi_goal_2 - complicated_unify") }
	).

code_gen__generate_semi_goal_2(pragma_c_code(MayCallMercury,
		PredId, ModeId, Args, ArgNames, OrigArgTypes, PragmaCode),
		GoalInfo, Instr) -->
	{ ArgNames = pragma_c_code_arg_info(_InstTable, Names) },
	pragma_c_gen__generate_pragma_c_code(model_semi, MayCallMercury,
		PredId, ModeId, Args, Names, OrigArgTypes, GoalInfo,
		PragmaCode, Instr).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_gen__generate_negation(code_model, hlds_goal, code_tree,
	code_info, code_info).
:- mode code_gen__generate_negation(in, in, out, in, out) is det.

code_gen__generate_negation(CodeModel, Goal0, Code) -->
	{ Goal0 = GoalExpr - GoalInfo0 },
	{ goal_info_get_resume_point(GoalInfo0, Resume) },
	(
		{ Resume = resume_point(ResumeVarsPrime, ResumeLocsPrime) }
	->
		{ ResumeVars = ResumeVarsPrime},
		{ ResumeLocs = ResumeLocsPrime}
	;
		{ error("negated goal has no resume point") }
	),
	code_info__push_resume_point_vars(ResumeVars),
		% The next line is to enable Goal to pass the
		% pre_goal_update sanity check
	{ goal_info_set_resume_point(GoalInfo0, no_resume_point, GoalInfo) },
	{ Goal = GoalExpr - GoalInfo },

		% for a negated simple test, we can generate better code
		% than the general mechanism, because we don't have to
		% flush the cache.
	(
		{ CodeModel = model_semi },
		{ GoalExpr = unify(_, _, _, simple_test(L, R), _) },
		code_info__failure_is_direct_branch(CodeAddr),
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, simple_neg, yes) }
	->
			% Because we're generating a goal
			% (special-cased, though it may be)
			% we need to apply the pre- and post-
			% updates.
		code_info__pre_goal_update(GoalInfo, yes),
		code_info__produce_variable(L, CodeL, ValL),
		code_info__produce_variable(R, CodeR, ValR),
		code_info__variable_type(L, Type),
		{ Type = term__functor(term__atom("string"), [], _) ->
			Op = str_eq
		; Type = term__functor(term__atom("float"), [], _) ->
			Op = float_eq
		;
			Op = eq
		},
		{ TestCode = node([
			if_val(binop(Op, ValL, ValR), CodeAddr) -
				"test inequality"
		]) },
		code_info__post_goal_update(GoalInfo),
		{ Code = tree(tree(CodeL, CodeR), TestCode) }
	;
		code_gen__generate_negation_general(CodeModel, Goal,
			ResumeVars, ResumeLocs, Code)
	),
	code_info__pop_resume_point_vars.

:- pred code_gen__generate_negation_general(code_model, hlds_goal,
	set(var), resume_locs, code_tree, code_info, code_info).
:- mode code_gen__generate_negation_general(in, in, in, in, out, in, out)
	is det.

code_gen__generate_negation_general(CodeModel, Goal, ResumeVars, ResumeLocs,
		Code) -->
		% This code is a cut-down version of the code for semidet
		% if-then-elses.

	code_info__make_known_failure_cont(ResumeVars, ResumeLocs, no,
		ModContCode),

		% Maybe save the heap state current before the condition;
		% this ought to be after we make the failure continuation
		% because that causes the cache to get flushed
	code_info__get_globals(Globals),
	{
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(Goal)
	->
		ReclaimHeap = yes
	;
		ReclaimHeap = no
	},
	code_info__maybe_save_hp(ReclaimHeap, SaveHpCode, MaybeHpSlot),

	{ globals__lookup_bool_option(Globals, use_trail, UseTrail) },
	code_info__maybe_save_ticket(UseTrail, SaveTicketCode,
		MaybeTicketSlot),

		% Generate the condition as a semi-deterministic goal;
		% it cannot be nondet, since mode correctness requires it
		% to have no output vars
	code_gen__generate_goal(model_semi, Goal, GoalCode),

		% XXX we should check for delayed goals
		% (e.g. delayed non-linear constraints) here

	( { CodeModel = model_det } ->
		{ DiscardTicketCode = empty },
		{ FailCode = empty }
	;
		code_info__grab_code_info(CodeInfo),
		code_info__pop_failure_cont,
		% The call to reset_ticket(..., commit) here is necessary
		% in order to properly detect floundering.
		code_info__maybe_reset_and_discard_ticket(MaybeTicketSlot,
			commit, DiscardTicketCode),
		code_info__generate_failure(FailCode),
		code_info__slap_code_info(CodeInfo)
	),
	code_info__restore_failure_cont(RestoreContCode),
	code_info__maybe_reset_and_discard_ticket(MaybeTicketSlot, undo,
		RestoreTicketCode),
	code_info__maybe_restore_and_discard_hp(MaybeHpSlot, RestoreHpCode),
	{ Code = tree(ModContCode,
		 tree(SaveHpCode,
		 tree(SaveTicketCode,
		 tree(GoalCode,
		 tree(DiscardTicketCode, % is this necessary?
		 tree(FailCode,
		 tree(RestoreContCode,
		 tree(RestoreTicketCode,
		      RestoreHpCode)))))))) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_gen__generate_non_goal_2(hlds_goal_expr, hlds_goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_non_goal_2(in, in, out, in, out) is det.

code_gen__generate_non_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_goals(Goals, model_non, Code).
code_gen__generate_non_goal_2(par_conj(_Goals, _SM), _GoalInfo, _Code) -->
		% Determinism analysis will report a determinism error if the
		% parallel conj is not det.
	{ error("sorry, nondet parallel conjunction not implemented") }.
code_gen__generate_non_goal_2(some(_Vars, Goal), _GoalInfo, Code) -->
	{ Goal = _ - InnerGoalInfo },
	{ goal_info_get_code_model(InnerGoalInfo, CodeModel) },
	code_gen__generate_goal(CodeModel, Goal, Code).
code_gen__generate_non_goal_2(disj(Goals, StoreMap), _GoalInfo, Code) -->
	disj_gen__generate_non_disj(Goals, StoreMap, Code).
code_gen__generate_non_goal_2(not(_Goal), _GoalInfo, _Code) -->
	{ error("Cannot have a nondet negation.") }.
code_gen__generate_non_goal_2(higher_order_call(PredVar, Args, Types, Modes,
		Det, _PredOrFunc),
		GoalInfo, Code) -->
	call_gen__generate_higher_order_call(model_non, PredVar, Args, Types,
		Modes, Det, GoalInfo, Code).
code_gen__generate_non_goal_2(class_method_call(TCVar, Num, Args, Types,
		ArgModes, Det),
		GoalInfo, Code) -->
	call_gen__generate_class_method_call(model_non, TCVar, Num, Args, Types,
		ArgModes, Det, GoalInfo, Code).
code_gen__generate_non_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
							GoalInfo, Code) -->
	(
		{ BuiltinState = not_builtin }
	->
		code_info__succip_is_used,
		call_gen__generate_call(model_non, PredId, ProcId, Args,
			GoalInfo, Code)
	;
		call_gen__generate_nondet_builtin(PredId, ProcId, Args, Code)
	).
code_gen__generate_non_goal_2(switch(Var, CanFail, CaseList, StoreMap),
		GoalInfo, Instr) -->
	switch_gen__generate_switch(model_non, Var, CanFail,
		CaseList, StoreMap, GoalInfo, Instr).
code_gen__generate_non_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal, StoreMap),
							_GoalInfo, Instr) -->
	ite_gen__generate_nondet_ite(CondGoal, ThenGoal, ElseGoal,
		StoreMap, Instr).
code_gen__generate_non_goal_2(unify(_L, _R, _U, _Uni, _C),
							_GoalInfo, _Code) -->
	{ error("Cannot have a nondet unification.") }.
code_gen__generate_non_goal_2(pragma_c_code(MayCallMercury,
		PredId, ModeId, Args, ArgNames, OrigArgTypes, PragmaCode),
		GoalInfo, Instr) -->
	{ ArgNames = pragma_c_code_arg_info(_InstTable, Names) },
	pragma_c_gen__generate_pragma_c_code(model_non, MayCallMercury,
		PredId, ModeId, Args, Names, OrigArgTypes, GoalInfo,
		PragmaCode, Instr).

%---------------------------------------------------------------------------%

code_gen__output_args(Args, Vs) :-
	code_gen__select_args_with_mode(Args, top_out, _, Lvals),
	set__list_to_set(Lvals, Vs).

:- pred code_gen__select_args_with_mode(assoc_list(var, arg_info), 
	arg_mode, list(var), list(lval)).
:- mode code_gen__select_args_with_mode(in, in, out, out) is det.

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

:- pred code_gen__add_saved_succip(list(instruction), int, list(instruction)).
:- mode code_gen__add_saved_succip(in, in, out) is det.

code_gen__add_saved_succip([], _StackLoc, []).
code_gen__add_saved_succip([Instrn0 - Comment | Instrns0 ], StackLoc, 
		[Instrn - Comment | Instrns]) :-
	(
		Instrn0 = livevals(LiveVals0),
		Instrns0 \= [goto(succip) - _ | _]
		% XXX We should also test for tailcalls
		% if we ever start generating them directly.
	->
		set__insert(LiveVals0, stackvar(StackLoc), LiveVals1),
		Instrn = livevals(LiveVals1)
        ;
		Instrn0 = call(Target, ReturnLabel, LiveVals0, CM)
	->
		Instrn  = call(Target, ReturnLabel, 
			[live_lvalue(stackvar(StackLoc), succip, "", []) |
			LiveVals0], CM)
	;
		Instrn = Instrn0
	),
	code_gen__add_saved_succip(Instrns0, StackLoc, Instrns).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
