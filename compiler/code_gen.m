%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
:- import_module continuation_info.
:- import_module list, assoc_list, io.

		% Translate a HLDS structure into an LLDS

:- pred generate_code(module_info, module_info, list(c_procedure),
						io__state, io__state).
:- mode generate_code(in, out, out, di, uo) is det.

:- pred generate_proc_code(proc_info, proc_id, pred_id, module_info, 
	continuation_info, int, continuation_info, int, c_procedure, 
	io__state, io__state).
:- mode generate_proc_code(in, in, in, in, in, in, out, out, out,
	di, uo) is det.

		% This predicate generates code for a goal.

:- pred code_gen__generate_goal(code_model, hlds_goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_goal(in, in, out, in, out) is det.

:- pred code_gen__output_args(assoc_list(var, arg_info), set(lval)).
:- mode code_gen__output_args(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module call_gen, unify_gen, ite_gen, switch_gen.
:- import_module disj_gen, pragma_c_gen, globals, options, hlds_out.
:- import_module code_aux, middle_rec, passes_aux.
:- import_module code_util, type_util, mode_util.
:- import_module prog_data, instmap.
:- import_module bool, char, int, string, list, term.
:- import_module map, tree, std_util, require, set, varset.

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
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		ContInfo0, ContInfo, CellCount0, CellCount, [], Code),
	{ module_info_set_cell_count(ModuleInfo0, CellCount, ModuleInfo1) },
	{ module_info_set_continuation_info(ModuleInfo1, ContInfo, 
		ModuleInfo) }.

% For all the modes of predicate PredId, generate the appropriate
% code (deterministic, semideterministic, or nondeterministic).

:- pred generate_proc_list_code(list(proc_id), pred_id, pred_info, module_info,
	continuation_info, continuation_info, int, int,
	list(c_procedure), list(c_procedure), io__state, io__state).
% :- mode generate_proc_list_code(in, in, in, in, di, uo, di, uo, di, uo)
%	is det.
:- mode generate_proc_list_code(in, in, in, in, in, out, in, out, in, out,
	di, uo) is det.

generate_proc_list_code([], _PredId, _PredInfo, _ModuleInfo,
		ContInfo, ContInfo, CellCount, CellCount, Procs, Procs) --> [].
generate_proc_list_code([ProcId | ProcIds], PredId, PredInfo, ModuleInfo0,
		ContInfo0, ContInfo, CellCount0, CellCount, Procs0, Procs) -->
	{ pred_info_procedures(PredInfo, ProcInfos) },
		% locate the proc_info structure for this mode of the predicate
	{ map__lookup(ProcInfos, ProcId, ProcInfo) },
		% find out if the proc is deterministic/etc
	generate_proc_code(ProcInfo, ProcId, PredId, ModuleInfo0,
		ContInfo0, CellCount0, ContInfo1, CellCount1, Proc),
	{ Procs1 = [Proc | Procs0] },
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		ContInfo1, ContInfo, CellCount1, CellCount, Procs1, Procs).

generate_proc_code(ProcInfo, ProcId, PredId, ModuleInfo,
		ContInfo0, CellCount0, ContInfo, CellCount, Proc) -->
		% find out if the proc is deterministic/etc
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
		% get the goal for this procedure
	{ proc_info_goal(ProcInfo, Goal) },
		% get the information about this procedure that we need.
	{ proc_info_variables(ProcInfo, VarInfo) },
	{ proc_info_liveness_info(ProcInfo, Liveness) },
	{ proc_info_stack_slots(ProcInfo, StackSlots) },
	{ proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InitialInst) },
	{ Goal = _ - GoalInfo },
	{ goal_info_get_follow_vars(GoalInfo, MaybeFollowVars) },
	{
		MaybeFollowVars = yes(FollowVars)
	;
		MaybeFollowVars = no,
		map__init(FollowVars)
	},
	globals__io_get_gc_method(GC_Method),
	{ GC_Method = accurate ->
		SaveSuccip = yes
	;
		SaveSuccip = no
	},
	globals__io_get_globals(Globals),
		% initialise the code_info structure 
	{ code_info__init(VarInfo, Liveness, StackSlots, SaveSuccip, Globals,
		PredId, ProcId, ProcInfo, InitialInst, FollowVars,
		ModuleInfo, CellCount0, ContInfo0, CodeInfo0) },
		% generate code for the procedure
	{ generate_category_code(CodeModel, Goal, CodeTree, SUsed, CodeInfo0,
		CodeInfo) },
		% extract the new continuation_info and cell count
	{ code_info__get_continuation_info(ContInfo1, CodeInfo, _CodeInfo1) },
	{ code_info__get_cell_count(CellCount, CodeInfo, _CodeInfo2) },


		% turn the code tree into a list
	{ tree__flatten(CodeTree, FragmentList) },
		% now the code is a list of code fragments (== list(instr)),
		% so we need to do a level of unwinding to get a flat list.
	{ list__condense(FragmentList, Instructions0) },
	{
		SUsed = yes(SlotNum)
	->
		% XXX Do we need to still do this?
		code_gen__add_saved_succip(Instructions0,
			SlotNum, Instructions),

		( GC_Method = accurate ->
			code_info__get_total_stackslot_count(StackSize,
				CodeInfo, _),
			code_util__make_proc_label(ModuleInfo, 
				PredId, ProcId, ProcLabel),
			continuation_info__add_proc_info(Instructions, 
				ProcLabel, StackSize, CodeModel,
				SlotNum, ContInfo1, ContInfo)
		;
			ContInfo = ContInfo1
		)
	;
		ContInfo = ContInfo1,
		Instructions = Instructions0
	},

		% get the name and arity of this predicate
	{ predicate_name(ModuleInfo, PredId, Name) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
		% construct a c_procedure structure with all the information
	{ proc_id_to_int(ProcId, LldsProcId) },
	{ Proc = c_procedure(Name, Arity, LldsProcId, Instructions) }.

:- pred generate_category_code(code_model, hlds_goal, code_tree, maybe(int),
				code_info, code_info).
:- mode generate_category_code(in, in, out, out, in, out) is det.

generate_category_code(model_det, Goal, Instrs, Used) -->
		% generate the code for the body of the clause
	(
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, middle_rec, yes) },
		middle_rec__match_and_generate(Goal, MiddleRecInstrs)
	->
		{ Instrs = MiddleRecInstrs },
		{ Used = no }
	;
		% Make a new failure cont (not model_non)
		% This continuation is never actually used,
		% but is a place holder.
		code_info__manufacture_failure_cont(no),

		code_gen__generate_goal(model_det, Goal, Instr1),
		code_info__get_instmap(Instmap),

		% generate the prolog for the clause, which for deterministic
		% procedures creates a label, increments the
		% stack pointer to reserve space for local variables and
		% the succip, and saves the succip.

		code_gen__generate_det_prolog(Instr0, Used),

		% generate a procedure epilog
		% This needs information based on what variables are
		% live at the end of the goal - that is, those that
		% are output parameters which are known from goal_info,
		% and decrement the stack pointer to free local variables,
		% and restore the succip.

		(
			{ instmap__is_reachable(Instmap) }
		->
			code_gen__generate_det_epilog(Instr2)
		;
			{ Instr2 = empty }
		),

		% combine the prolog, body and epilog
		{ Instrs = tree(Instr0, tree(Instr1, Instr2)) }
	).

generate_category_code(model_semi, Goal, Instrs, Used) -->
		% Make a new failure cont (not model_non)
	code_info__manufacture_failure_cont(no),

		% generate the code for the body of the clause
	code_gen__generate_goal(model_semi, Goal, Instr1),
	code_gen__generate_semi_prolog(Instr0, Used),
	code_gen__generate_semi_epilog(Instr2),

		% combine the prolog, body and epilog
	{ Instrs = tree(Instr0, tree(Instr1, Instr2)) }.

generate_category_code(model_non, Goal, Instrs, Used) -->
		% Make a failure continuation, we lie and
		% say that it is nondet, and then unset it
		% so that it points to do_fail
	code_info__manufacture_failure_cont(yes),

		% generate the code for the body of the clause
	code_gen__generate_goal(model_non, Goal, Instr1),
	code_gen__generate_non_prolog(Instr0, Used),
	code_gen__generate_non_epilog(Instr2),

		% combine the prolog, body and epilog
	{ Instrs = tree(Instr0, tree(Instr1, Instr2)) }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_det_prolog(code_tree, maybe(int),
	code_info, code_info).
:- mode code_gen__generate_det_prolog(out, out, in, out) is det.

code_gen__generate_det_prolog(EntryCode, SUsed) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_varset(VarSet),
	{ code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment) },
	code_info__get_total_stackslot_count(NS0),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_succip_used(Used),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ CodeA = node([
		comment(SlotsComment) - "",
		label(Entry) - "Procedure entry point"
	]) },
	(
		{ Used = yes }
	->
		{ NS is NS0 + 1 },
		{ CodeC = node([
			assign(stackvar(NS), lval(succip)) -
					"save the success ip"
		]) },
		{ SUsed = yes(NS) }
	;
		{ NS = NS0 },
		{ CodeC = empty },
		{ SUsed = no }
	),
	(
		{ NS = 0 }
	->
		{ CodeB = CodeA }
	;
		{ predicate_module(ModuleInfo, PredId, ModuleName) },
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ string__append_list([ModuleName, ":", PredName], PushMsg) },
		{ CodeB = tree(
			CodeA,
			node([incr_sp(NS, PushMsg) - "Allocate stack frame"])
		) }
	),
	{ PStart = node([comment("Start of procedure prologue") - ""]) },
	{ PEnd = node([comment("End of procedure prologue") - ""]) },
	{ EntryCode = tree(tree(PStart, CodeB), tree(CodeC, PEnd)) }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_det_epilog(code_tree, code_info, code_info).
:- mode code_gen__generate_det_epilog(out, in, out) is det.

code_gen__generate_det_epilog(ExitCode) -->
	code_info__get_instmap(Instmap),
	code_info__get_arginfo(ArgModes),
	code_info__get_headvars(HeadVars),
	{ assoc_list__from_corresponding_lists(HeadVars, ArgModes, Args)},
	(
		{ instmap__is_unreachable(Instmap) }
	->
		{ CodeA = empty }
	;
		code_info__setup_call(Args, callee, CodeA)
	),
	code_info__get_succip_used(Used),
	code_info__get_total_stackslot_count(NS0),
	(
		{ Used = yes }
	->
		{ NS is NS0 + 1 },
		{ CodeC = node([
			assign(succip, lval(stackvar(NS))) -
					"restore the success ip"
		]) }
	;
		{ NS = NS0 },
		{ CodeC = empty }
	),
	{ CodeB1 = node([ goto(succip) - "Return from procedure call"]) },
	(
		{ NS = 0 }
	->
		{ CodeB0 = empty }
	;
		{ CodeB0 = node([
			decr_sp(NS) - "Deallocate stack frame"
		]) }
	),
	{ code_gen__output_args(Args, LiveArgs) },
	{ LiveValCode = node([
		livevals(LiveArgs) - ""
	]) },
	{ CodeB = tree(CodeB0, tree(LiveValCode, CodeB1)) },
	{ EStart = node([comment("Start of procedure epilogue") - ""]) },
	{ EEnd = node([comment("End of procedure epilogue") - ""]) },
	{ ExitCode = tree(tree(EStart, CodeA),
					tree(CodeC, tree(EEnd, CodeB))) }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_semi_prolog(code_tree, maybe(int),
	code_info, code_info).
:- mode code_gen__generate_semi_prolog(out, out, in, out) is det.

code_gen__generate_semi_prolog(EntryCode, SUsed) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_varset(VarSet),
	{ code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment) },
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_succip_used(Used),
	code_info__get_total_stackslot_count(NS0),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ CodeA = node([
		comment(SlotsComment) - "",
		label(Entry) - "Procedure entry point"
	]) },
	(
		{ Used = yes }
	->
		{ NS is NS0 + 1 },
		{ CodeC = node([
			assign(stackvar(NS), lval(succip)) -
					"save the success ip"
		]) },
		{ SUsed = yes(NS) }
	;
		{ NS = NS0 },
		{ CodeC = empty },
		{ SUsed = no }
	),
	(
		{ NS = 0 }
	->
		{ CodeB = CodeA }
	;
		{ predicate_module(ModuleInfo, PredId, ModuleName) },
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ string__append_list([ModuleName, ":", PredName], PushMsg) },
		{ CodeB = tree(
			CodeA,
			node([incr_sp(NS, PushMsg) - "Allocate stack frame"])
		) }
	),
	{ PStart = node([comment("Start of procedure prologue") - ""]) },
	{ PEnd = node([comment("End of procedure prologue") - ""]) },
	{ EntryCode = tree(tree(PStart, CodeB), tree(CodeC, PEnd)) }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_semi_epilog(code_tree, code_info, code_info).
:- mode code_gen__generate_semi_epilog(out, in, out) is det.

code_gen__generate_semi_epilog(Instr) -->
	code_info__get_instmap(Instmap),
	code_info__get_arginfo(ArgModes),
	code_info__get_headvars(HeadVars),
	{assoc_list__from_corresponding_lists(HeadVars, ArgModes, Args) },
	(
		{ instmap__is_unreachable(Instmap) }
	->
		{ CodeA = empty }
	;
		code_info__setup_call(Args, callee, CodeA)
	),
	code_info__restore_failure_cont(FailureCont),
	code_info__get_succip_used(Used),
	code_info__get_total_stackslot_count(NS0),
	{ code_gen__output_args(Args, LiveArgs0) },
	{ set__insert(LiveArgs0, reg(r, 1), LiveArgs) },
	{ SLiveValCode = node([
		livevals(LiveArgs) - ""
	]) },
	{ set__singleton_set(LiveArg, reg(r, 1)) },
	{ FLiveValCode = node([
		livevals(LiveArg) - ""
	]) },
	(
		{ Used = yes }
	->
		{ NS is NS0 + 1 },
		{ CodeC = node([
			assign(succip, lval(stackvar(NS))) -
					"restore the success ip"
		]) }
	;
		{ NS = NS0 },
		{ CodeC = empty }
	),
	(
		{ NS = 0 }
	->
		{ UnLink = CodeC }
	;
		{ UnLink = tree(
			CodeC,
			node([
				decr_sp(NS) - "Deallocate stack frame"
			])
		) }
	),
	{ Success = tree(
		UnLink,
		node([ assign(reg(r, 1), const(true)) - "Succeed" ])
	) },
	{ Failure = tree(
		UnLink,
		node([ assign(reg(r, 1), const(false)) - "Fail" ])
	) },
	{ ExitCode = tree(
		tree(
			tree(Success, SLiveValCode),
			node([ goto(succip) - "Return from procedure call" ])
		),
		tree(
			FailureCont,
			tree(
				tree(Failure, FLiveValCode),
				node([ goto(succip) -
					"Return from procedure call" ])
			)
		)
	) },
	{ EStart = node([comment("Start of procedure epilogue") - ""]) },
	{ EEnd = node([comment("End of procedure epilogue") - ""]) },
	{ Instr = tree(tree(EStart, CodeA), tree(ExitCode, EEnd)) }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_non_prolog(code_tree, maybe(int),
	code_info, code_info).
:- mode code_gen__generate_non_prolog(out, out, in, out) is det.

code_gen__generate_non_prolog(EntryCode, no) -->
	code_info__get_stack_slots(StackSlots),
	code_info__get_varset(VarSet),
	{ code_aux__explain_stack_slots(StackSlots, VarSet, SlotsComment) },
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_total_stackslot_count(NS),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ CodeA = node([
		comment(SlotsComment) - "",
		label(Entry) - "Procedure entry point"
	]) },
		% The `name' argument to mkframe() is just for
		% debugging purposes.  We construct it as "predname/arity".
	{ predicate_name(ModuleInfo, PredId, PredName) },
	{ predicate_arity(ModuleInfo, PredId, PredArity) },
	{ string__int_to_string(PredArity, PredArityString) },
	{ string__append(PredName, "/", Tmp) },
	{ string__append(Tmp, PredArityString, Name) },
	{ CodeB = node([
		mkframe(Name, NS, do_fail) - "Nondet stackframe"
	]) },
	{ PStart = node([comment("Start of procedure prologue") - ""]) },
	{ PEnd = node([comment("End of procedure prologue") - ""]) },
	{ EntryCode = tree(tree(PStart, CodeA), tree(CodeB, PEnd)) }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_non_epilog(code_tree, code_info, code_info).
:- mode code_gen__generate_non_epilog(out, in, out) is det.

code_gen__generate_non_epilog(Instr) -->
	code_info__get_instmap(Instmap),
	code_info__get_arginfo(ArgModes),
	code_info__get_headvars(HeadVars),
	{assoc_list__from_corresponding_lists(HeadVars, ArgModes, Args) },
	(
		{ instmap__is_unreachable(Instmap) }
	->
		{ CodeA = empty }
	;
		code_info__setup_call(Args, callee, CodeA)
	),
	{ code_gen__output_args(Args, LiveArgs) },
	{ LiveValCode = node([
		livevals(LiveArgs) - ""
	]) },
	{ ExitCode = tree(LiveValCode, node([
		goto(do_succeed(no)) - "Succeed"
	])) },
	{ EStart = node([comment("Start of procedure epilogue") - ""]) },
	{ EEnd = node([comment("End of procedure epilogue") - ""]) },
	{ Instr = tree(tree(EStart, CodeA), tree(ExitCode, EEnd)) }.

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
			code_gen__generate_det_goal_2(Goal, GoalInfo, Code0)
		;
			{ CodeModel = model_semi },
			( { ContextModel \= model_det } ->
				code_gen__generate_semi_goal_2(Goal, GoalInfo,
					Code0)
			;
				{ error("semidet model in det context") }
			)
		;
			{ CodeModel = model_non },
			( { ContextModel = model_non } ->
				code_gen__generate_non_goal_2(Goal, GoalInfo,
					Code0)
			;
				{ error("nondet model in det/semidet context") }
			)
		),
			% Make live any variables which subsequent goals
			% will expect to be live, but were not generated
		code_info__set_instmap(Instmap),
		code_info__post_goal_update(GoalInfo),
		code_info__get_globals(Options),
		(
			{ globals__lookup_bool_option(Options, lazy_code, yes) }
		->
			{ Code1 = empty }
		;
			{ error("Eager code unavailable") }
%%%			code_info__generate_eager_flush(Code1)
		),
		{ Code = tree(Code0, Code1) }
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
		Modes, Det),
		GoalInfo, Instr) -->
	call_gen__generate_higher_order_call(model_det, PredVar, Args,
		Types, Modes, Det, GoalInfo, Instr).
code_gen__generate_det_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
		GoalInfo, Instr) -->
	(
		{ BuiltinState = not_builtin }
	->
		code_info__succip_is_used,
		call_gen__generate_det_call(PredId, ProcId, Args, GoalInfo,
			Instr)
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

code_gen__generate_det_goal_2(pragma_c_code(C_Code, MayCallMercury,
		PredId, ModeId, Args, ArgNames, OrigArgTypes, Extra),
		GoalInfo, Instr) -->
	(
		{ Extra = none },
		pragma_c_gen__generate_pragma_c_code(model_det, C_Code,
			MayCallMercury, PredId, ModeId, Args, ArgNames,
			OrigArgTypes, GoalInfo, Instr)
	;
		{ Extra = extra_pragma_info(_, _) },
		{ error("det pragma has non-empty extras field") }
	).

%---------------------------------------------------------------------------%

:- pred code_gen__generate_semi_goal_2(hlds_goal_expr, hlds_goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_semi_goal_2(in, in, out, in, out) is det.

code_gen__generate_semi_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_goals(Goals, model_semi, Code).
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
		Det), GoalInfo, Code) -->
	call_gen__generate_higher_order_call(model_semi, PredVar, Args,
		Types, Modes, Det, GoalInfo, Code).
code_gen__generate_semi_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
							GoalInfo, Code) -->
	(
		{ BuiltinState = not_builtin }
	->
		code_info__succip_is_used,
		call_gen__generate_semidet_call(PredId, ProcId, Args, GoalInfo,
			Code)
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

code_gen__generate_semi_goal_2(pragma_c_code(C_Code, MayCallMercury,
		PredId, ModeId, Args, ArgNameMap, OrigArgTypes, Extra),
		GoalInfo, Instr) -->
	(
		{ Extra = none },
		pragma_c_gen__generate_pragma_c_code(model_semi, C_Code,
			MayCallMercury, PredId, ModeId, Args, ArgNameMap,
			OrigArgTypes, GoalInfo, Instr)
	;
		{ Extra = extra_pragma_info(_, _) },
		{ error("semidet pragma has non-empty extras field") }
	).

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

	{ globals__lookup_bool_option(Globals, constraints, Constraints) },
	code_info__maybe_save_ticket(Constraints, SaveTicketCode,
		MaybeTicketSlot),

		% Generate the condition as a semi-deterministic goal;
		% it cannot be nondet, since mode correctness requires it
		% to have no output vars
	code_gen__generate_goal(model_semi, Goal, GoalCode),

	( { CodeModel = model_det } ->
		{ FailCode = empty }
	;
		code_info__grab_code_info(CodeInfo),
		code_info__pop_failure_cont,
		code_info__generate_failure(FailCode),
		code_info__slap_code_info(CodeInfo)
	),
	code_info__restore_failure_cont(RestoreContCode),
	code_info__maybe_restore_and_discard_ticket(MaybeTicketSlot,
		RestoreTicketCode),
	code_info__maybe_restore_and_discard_hp(MaybeHpSlot, RestoreHpCode),
	{ Code = tree(ModContCode,
		 tree(SaveHpCode,
		 tree(SaveTicketCode,
		 tree(GoalCode,
		 tree(FailCode,
		 tree(RestoreContCode,
		 tree(RestoreTicketCode,
		      RestoreHpCode))))))) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_gen__generate_non_goal_2(hlds_goal_expr, hlds_goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_non_goal_2(in, in, out, in, out) is det.

code_gen__generate_non_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_goals(Goals, model_non, Code).
code_gen__generate_non_goal_2(some(_Vars, Goal), _GoalInfo, Code) -->
	{ Goal = _ - InnerGoalInfo },
	{ goal_info_get_code_model(InnerGoalInfo, CodeModel) },
	code_gen__generate_goal(CodeModel, Goal, Code).
code_gen__generate_non_goal_2(disj(Goals, StoreMap), _GoalInfo, Code) -->
	disj_gen__generate_non_disj(Goals, StoreMap, Code).
code_gen__generate_non_goal_2(not(_Goal), _GoalInfo, _Code) -->
	{ error("Cannot have a nondet negation.") }.
code_gen__generate_non_goal_2(higher_order_call(PredVar, Args, Types, Modes,
		Det),
		GoalInfo, Code) -->
	call_gen__generate_higher_order_call(model_non, PredVar, Args, Types,
		Modes, Det, GoalInfo, Code).
code_gen__generate_non_goal_2(call(PredId, ProcId, Args, BuiltinState, _, _),
							GoalInfo, Code) -->
	(
		{ BuiltinState = not_builtin }
	->
		code_info__succip_is_used,
		call_gen__generate_nondet_call(PredId, ProcId, Args, GoalInfo,
			Code)
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
code_gen__generate_non_goal_2(pragma_c_code(C_Code, MayCallMercury,
		PredId, ModeId, Args, ArgNameMap, OrigArgTypes, Extra),
		GoalInfo, Instr) -->
	(
		{ Extra = none },
		% Error disabled for bootstrapping. string.m uses this form,
		% and we can't change it to the new form until the new form
		% is completed, and even then we must wait until that compiler
		% is installed on all our machines.
		% { error("nondet pragma has empty extras field") }
		pragma_c_gen__generate_pragma_c_code(model_semi, C_Code,
			MayCallMercury, PredId, ModeId, Args, ArgNameMap,
			OrigArgTypes, GoalInfo, Instr)
	;
		{ Extra = extra_pragma_info(SavedVars, LabelNames) },
		pragma_c_gen__generate_backtrack_pragma_c_code(model_semi,
			C_Code, MayCallMercury, PredId, ModeId, Args,
			ArgNameMap, OrigArgTypes, SavedVars, LabelNames,
			GoalInfo, Instr)
	).

%---------------------------------------------------------------------------%

code_gen__output_args([], LiveVals) :-
	set__init(LiveVals).
code_gen__output_args([_V - arg_info(Loc, Mode) | Args], Vs) :-
	code_gen__output_args(Args, Vs0),
	(
		Mode = top_out
	->
		code_util__arg_loc_to_register(Loc, Reg),
		set__insert(Vs0, Reg, Vs)
	;
		Vs = Vs0
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
		% XXX we should also test for tailcalls
		% once we start generating them directly
	->
		set__insert(LiveVals0, stackvar(StackLoc), LiveVals1),
		Instrn = livevals(LiveVals1)
        ;
		Instrn0 = call(Target, ReturnLabel, LiveVals0, CM)
	->
		Instrn  = call(Target, ReturnLabel, 
			[live_lvalue(stackvar(StackLoc), succip, []) |
			LiveVals0], CM)
	;
		Instrn = Instrn0
	),
	code_gen__add_saved_succip(Instrns0, StackLoc, Instrns).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
