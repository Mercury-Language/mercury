%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Code generation - convert from HLDS to LLDS.
% Main author: conway.
%
% Notes:
%	code_gen forwards most of the actual construction of intruction
%	sequences to code_info, and other modules. The generation of
%	calls is done by call_gen, switches by switch_gen, if-then-elses
%	by ite_gen, unifications by unify_gen, and disjunctions by disj_gen.
%
%	The general scheme for generating semideterministic code is
%	to treat it as deterministic code, and have a fall-through
%	point for failure.  Semideterministic procedures leave a 'true'
%	in register r(1) to indicate success, and 'fail' to indicate
%	failure.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module code_gen.

:- interface.

:- import_module hlds_module, hlds_pred, hlds_goal, llds, code_info, shapes.
:- import_module list, assoc_list, io.

		% Translate a HLDS structure into an LLDS

:- pred generate_code(module_info, module_info, list(c_procedure),
						io__state, io__state).
:- mode generate_code(in, out, out, di, uo) is det.

:- pred generate_proc_code(proc_info, proc_id, pred_id, module_info, 
	shape_table, shape_table, c_procedure, io__state, io__state).
:- mode generate_proc_code(in, in, in, in, in, out, out, di, uo) is det.
		% N.B. could use unique mode for `shape_table'

		% This predicate generates code for a goal.

:- pred code_gen__generate_goal(code_model, hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_goal(in, in, out, in, out) is det.

		% This predicate generates code for a goal
		% and leaves all live values in locations
		% determined by the call_info structure.

:- pred code_gen__generate_forced_goal(code_model, hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_forced_goal(in, in, out, in, out) is det.

:- pred code_gen__output_args(assoc_list(var, arg_info), set(lval)).
:- mode code_gen__output_args(in, out) is det.

:- pred code_gen__ensure_vars_are_saved(list(var), code_tree,
						code_info, code_info).
:- mode code_gen__ensure_vars_are_saved(in, out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool, char, string, list, varset, term, map, tree, require.
:- import_module type_util, mode_util, std_util, int, set.
:- import_module code_util, call_gen, unify_gen, ite_gen, switch_gen.
:- import_module disj_gen, globals, options, hlds_out.
:- import_module code_aux, middle_rec.
:- import_module prog_data, instmap.

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
#if NU_PROLOG
	{ module_info_get_shapes(ModuleInfo1, Shape_Table) },
	{ putprop(codegen, codegen, Predicates0 - Shape_Table ), fail }.
generate_pred_list_code(ModuleInfo0, ModuleInfo, [PredId | PredIds], 
			Predicates) -->
	{ getprop(codegen, codegen, Predicates0 - Shape_Table, Ref),
	  erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
	{ module_info_set_shapes(ModuleInfo0, Shape_Table, ModuleInfo1) },
#endif
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
		( { Statistics = yes } ->
			io__report_stats
		;
			[]
		)
	;
		[]
	),
		% generate all the procedures for this predicate
	{ module_info_get_shapes(ModuleInfo0, Shapes0) },
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Shapes0, Shapes, [], Code),
	{ module_info_set_shapes(ModuleInfo0, Shapes, ModuleInfo) }.

% For all the modes of predicate PredId, generate the appropriate
% code (deterministic, semideterministic, or nondeterministic).

:- pred generate_proc_list_code(list(proc_id), pred_id, pred_info, module_info,
	shape_table, shape_table, list(c_procedure), list(c_procedure),
	io__state, io__state).
% :- mode generate_proc_list_code(in, in, in, in, di, uo, di, uo, di, uo)
%	is det.
:- mode generate_proc_list_code(in, in, in, in, in, out, in, out, di, uo)
	is det.

generate_proc_list_code([], _PredId, _PredInfo, _ModuleInfo,
		Shapes, Shapes, Procs, Procs) --> [].
generate_proc_list_code([ProcId | ProcIds], PredId, PredInfo, ModuleInfo0,
		Shapes0, Shapes, Procs0, Procs) -->
	{ pred_info_procedures(PredInfo, ProcInfos) },
		% locate the proc_info structure for this mode of the predicate
	{ map__lookup(ProcInfos, ProcId, ProcInfo) },
		% find out if the proc is deterministic/etc
	generate_proc_code(ProcInfo, ProcId, PredId, ModuleInfo0,
		Shapes0, Shapes1, Proc),
	{ Procs1 = [Proc | Procs0] },
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Shapes1, Shapes, Procs1, Procs).

generate_proc_code(ProcInfo, ProcId, PredId, ModuleInfo,
		Shapes0, Shapes, Proc) -->
		% find out if the proc is deterministic/etc
	{ proc_info_interface_code_model(ProcInfo, CodeModel) },
		% get the goal for this procedure
	{ proc_info_goal(ProcInfo, Goal) },
		% get the information about this procedure that we need.
	{ proc_info_variables(ProcInfo, VarInfo) },
	{ proc_info_liveness_info(ProcInfo, Liveness) },
	{ proc_info_follow_vars(ProcInfo, FollowVars) },
	{ proc_info_call_info(ProcInfo, CallInfo) },
	{ proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InitialInst) },
	globals__io_get_gc_method(GC_Method),
	{ GC_Method = accurate ->
		SaveSuccip = yes
	;
		SaveSuccip = no
	},
	globals__io_get_globals(Globals),
		% initialise the code_info structure 
	{ code_info__init(VarInfo, Liveness, CallInfo, SaveSuccip, Globals,
		PredId, ProcId, ProcInfo, CodeModel, InitialInst, FollowVars,
		ModuleInfo, Shapes0, CodeInfo0) },
		% generate code for the procedure
	{ generate_category_code(CodeModel, Goal, CodeTree, SUsed, CodeInfo0,
		CodeInfo) },
		% extract the new shape table
	{ code_info__get_shapes(Shapes, CodeInfo, _CodeInfo1) },

		% turn the code tree into a list
	{ tree__flatten(CodeTree, FragmentList) },
		% now the code is a list of code fragments (== list(instr)),
		% so we need to do a level of unwinding to get a flat list.
	{ list__condense(FragmentList, Instructions0) },
	(
		{ SUsed = yes(SlotNum) }
	->
		{ code_gen__add_saved_succip(Instructions0,
			SlotNum, Instructions) }
	;
		{ Instructions = Instructions0 }
	),
		% get the name and arity of this predicate
	{ predicate_name(ModuleInfo, PredId, Name) },
	{ predicate_arity(ModuleInfo, PredId, Arity) },
		% construct a c_procedure structure with all the information
	{ Proc = c_procedure(Name, Arity, ProcId, Instructions) }.

:- pred generate_category_code(code_model, hlds__goal, code_tree, maybe(int),
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
		code_info__get_instmap(InstMap),

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
			{ InstMap \= unreachable }
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
	code_info__get_call_info(CallInfo),
	code_info__get_varset(VarSet),
	{ code_aux__explain_call_info(CallInfo, VarSet, CallInfoComment) },
	code_info__get_total_stackslot_count(NS0),
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_succip_used(Used),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ CodeA = node([
		comment(CallInfoComment) - "",
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
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ CodeB = tree(
			CodeA,
			node([incr_sp(NS, PredName) - "Allocate stack frame"])
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
		{ Instmap = unreachable }
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

:- pred code_gen__generate_semi_prolog(code_tree, maybe(int), code_info, code_info).
:- mode code_gen__generate_semi_prolog(out, out, in, out) is det.

code_gen__generate_semi_prolog(EntryCode, SUsed) -->
	code_info__get_call_info(CallInfo),
	code_info__get_varset(VarSet),
	{ code_aux__explain_call_info(CallInfo, VarSet, CallInfoComment) },
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_succip_used(Used),
	code_info__get_total_stackslot_count(NS0),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ CodeA = node([
		comment(CallInfoComment) - "",
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
		{ predicate_name(ModuleInfo, PredId, PredName) },
		{ CodeB = tree(
			CodeA,
			node([incr_sp(NS, PredName) - "Allocate stack frame"])
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
		{ Instmap = unreachable }
	->
		{ CodeA = empty }
	;
		code_info__setup_call(Args, callee, CodeA)
	),
	code_info__restore_failure_cont(FailureCont),
	code_info__get_succip_used(Used),
	code_info__get_total_stackslot_count(NS0),
	{ code_gen__output_args(Args, LiveArgs0) },
	{ set__insert(LiveArgs0, reg(r(1)), LiveArgs) },
	{ SLiveValCode = node([
		livevals(LiveArgs) - ""
	]) },
	{ set__singleton_set(LiveArg, reg(r(1))) },
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
		node([ assign(reg(r(1)), const(true)) - "Succeed" ])
	) },
	{ Failure = tree(
		UnLink,
		node([ assign(reg(r(1)), const(false)) - "Fail" ])
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

:- pred code_gen__generate_non_prolog(code_tree, maybe(int), code_info, code_info).
:- mode code_gen__generate_non_prolog(out, out, in, out) is det.

code_gen__generate_non_prolog(EntryCode, no) -->
	code_info__get_call_info(CallInfo),
	code_info__get_varset(VarSet),
	{ code_aux__explain_call_info(CallInfo, VarSet, CallInfoComment) },
	code_info__get_pred_id(PredId),
	code_info__get_proc_id(ProcId),
	code_info__get_total_stackslot_count(NS),
	code_info__get_module_info(ModuleInfo),
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, no,
		Entry) },
	{ CodeA = node([
		comment(CallInfoComment) - "",
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
		{ Instmap = unreachable }
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

code_gen__generate_forced_goal(Det, Goal, Code) -->
	code_gen__generate_goal(Det, Goal, CodeA),
	code_info__generate_forced_saves(CodeB),
	{ Code = tree(CodeA, CodeB) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

% Generate a goal. This predicate arranges for the necessary updates of
% the generic data structures before and after the actual code generation,
% which is delegated to context-specific predicates.

code_gen__generate_goal(ContextModel, Goal - GoalInfo, Code) -->
		% Make any changes to liveness before Goal
	code_aux__pre_goal_update(GoalInfo),
	code_info__get_instmap(InstMap),
	(
		{ InstMap \= unreachable }
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
					Code0),
				% the nondet goal may have created choice
				% points, so we must set the current failure
				% continuation to `unknown', which means
				% "on failure, just do a redo()".
				code_info__unset_failure_cont
			;
				{ error("nondet model in det/semidet context") }
			)
		),
			% Make live any variables which subsequent goals
			% will expect to be live, but were not generated
		code_info__set_instmap(InstMap),
		code_aux__post_goal_update(GoalInfo),
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
% flows directly from one to the next atom.

:- pred code_gen__generate_goals(hlds__goals, code_model, code_tree,
							code_info, code_info).
:- mode code_gen__generate_goals(in, in, out, in, out) is det.

code_gen__generate_goals([], _, empty) --> [].
code_gen__generate_goals([Goal | Goals], CodeModel, Instr) -->
	code_gen__generate_goal(CodeModel, Goal, Instr1),
	code_info__get_instmap(InstMap),
	(
		{ InstMap = unreachable }
	->
		{ Instr = Instr1 }
	;
		code_gen__generate_goals(Goals, CodeModel, Instr2),
		{ Instr = tree(Instr1, Instr2) }
	).

%---------------------------------------------------------------------------%

:- pred code_gen__generate_det_goal_2(hlds__goal_expr, hlds__goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_det_goal_2(in, in, out, in, out) is det.

code_gen__generate_det_goal_2(conj(Goals), _GoalInfo, Instr) -->
	code_gen__generate_goals(Goals, model_det, Instr).
code_gen__generate_det_goal_2(some(Vars, Goal), _GoalInfo, Instr) -->
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
		code_info__generate_det_pre_commit(PreCommit),
		code_gen__generate_goal(model_non, Goal, GoalCode),
		code_info__generate_det_commit(Commit),
		{ Instr = tree(PreCommit, tree(GoalCode, Commit)) }
	),
		% Any variables that became nondet live
		% during the quantified goal that are
		% quantified to the scope of that goal
		% are no longer nondet live.
	code_info__get_nondet_lives(NondetLives0),
	{ set__delete_list(NondetLives0, Vars, NondetLives) },
	code_info__set_nondet_lives(NondetLives).
code_gen__generate_det_goal_2(disj(Goals, FV), _GoalInfo, Instr) -->
	code_info__push_store_map(FV),
	disj_gen__generate_det_disj(Goals, Instr),
	code_info__pop_store_map.
code_gen__generate_det_goal_2(not(Goal), _GoalInfo, Instr) -->
	code_gen__generate_negation_general(model_det, Goal, Instr).
code_gen__generate_det_goal_2(higher_order_call(PredVar, Args, Types,
		Modes, Det, _Follow),
		_CodeInfo, Instr) -->
	call_gen__generate_higher_order_call(model_det, PredVar, Args,
		Types, Modes, Det, Instr).
code_gen__generate_det_goal_2(
		call(PredId, ProcId, Args, Builtin, _, _, _Follow),
							_GoalInfo, Instr) -->
	(
		{ hlds__is_builtin_is_internal(Builtin) }
	->
		call_gen__generate_det_builtin(PredId, ProcId, Args, Instr)
	;
		code_info__set_succip_used(yes),
		call_gen__generate_det_call(PredId, ProcId, Args, Instr)
	).
code_gen__generate_det_goal_2(switch(Var, CanFail, CaseList, FV), GoalInfo,
		Instr) -->
	code_info__push_store_map(FV),
	switch_gen__generate_switch(model_det, Var, CanFail, CaseList, FV,
		GoalInfo, Instr),
	code_info__pop_store_map.
code_gen__generate_det_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal, FV),
							_GoalInfo, Instr) -->
	code_info__push_store_map(FV),
	ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, FV, Instr),
	code_info__pop_store_map.
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
		{ Uni = complicated_unify(_UniMode, _CanFail, _Follow) },
		{ error("code_gen__generate_det_goal_2 - complicated unify") }
	;
		{ Uni = simple_test(_, _) },
		{ error("generate_det_goal_2: cannot have det simple_test") }
	).

code_gen__generate_det_goal_2(
		pragma_c_code(C_Code, IsRecursive, PredId, ModeId, Args,
				ArgNames), GoalInfo, Instr) -->
	code_gen__generate_pragma_c_code(model_det, C_Code, IsRecursive,
		PredId, ModeId, Args, ArgNames, GoalInfo, Instr).

%---------------------------------------------------------------------------%

:- type c_arg
	--->	c_arg(var, maybe(string)).

:- pred code_gen__generate_pragma_c_code(code_model, string, c_is_recursive,
		pred_id, proc_id, list(var), list(maybe(string)),
		hlds__goal_info, code_tree, code_info, code_info).
:- mode code_gen__generate_pragma_c_code(in, in, in, in, in, in, in, in, out,
		in, out) is det.

code_gen__generate_pragma_c_code(CodeModel, C_Code, IsRecursive,
		PredId, ModeId, Args, Names, _GoalInfo, Instr) -->
	% First we need to get a list of input and output arguments
	code_info__get_pred_proc_arginfo(PredId, ModeId, ArgInfo),
	{ make_c_arg_list(Args, Names, ArgNames) },
	{ assoc_list__from_corresponding_lists(ArgNames, ArgInfo, ArgModes) },
	{ pragma_select_in_args(ArgModes, InArgs) },
	{ pragma_select_out_args(ArgModes, OutArgs) },

% The code generated for pragma_c_code is of the following form:
%
% <save live variables onto the stack> /* see note (1) below */
% {
%	<declaration of one local variable for each arg>
%	<assignment of input values from registers to local variables>
%	save_registers(); /* see notes (1) and (2) below */
%	{ <the c code itself> }
%	#ifndef CONSERVATIVE_GC
%	  restore_registers(); /* see notes (1) and (3) below */
%	#endif
%	<assignment of the output values from local variables to registers>
% }
%
% In the case of a semidet pragma c_code, this is followed by
%
%	if (r1) goto label;
%	<code to fail>
%	label:
%
% Notes:
%
% (1)      These parts are only emitted if the C code may be recursive.
%	   If a pragma c_code(non_recursive, ...) declaration was used,
%	   they will not be emitted.
%
% (2)	   The call to save_registers() is needed so that if the
%	   C code calls Mercury code, we can call restore_registers()
%	   on entry to the Mercury code (see export.m) to get the
%	   right values of `sp', `hp', `curfr' and `maxfr' for the
%	   recursive invocation of Mercury.
%
% (3)	   The call to restore_registers() is needed in case the
%	   C code calls Mercury code which allocates some data
%	   on the heap, and this data is returned from Mercury
%	   through C back to Mercury.  In that case, we need to
%	   keep the value of `hp' that was set by the recursive
%	   invocation of Mercury.  The Mercury calling convention
%	   guarantees that the values of `sp', `curfr', and `maxfr'
%	   will be preserved, so if we're using conservative gc,
%	   there is nothing that needs restoring.

	( { IsRecursive = non_recursive } ->
		{ SaveVarsCode = empty }
	;
		% the C code might call back Mercury code which clobbers the
		% succip
		code_info__set_succip_used(yes),

		% the C code might call back Mercury code which clobbers the
		% other registers, so we need to save any live variables
		% (other than the output args) onto the stack
		{ get_c_arg_list_vars(OutArgs, OutArgs1) },
		{ set__list_to_set(OutArgs1, OutArgsSet) },
		call_gen__save_variables(OutArgsSet, SaveVarsCode)
	),

	make_pragma_decls(ArgNames, Decls),
	get_pragma_input_vars(InArgs, Inputs, InputVarsCode),
	( { CodeModel = model_semi } ->
		% We have to clear r1 for C code that gets inlined
		% so that it is safe to assign to SUCCESS_INDICATOR.
		code_info__clear_r1(ShuffleR1_Code),

		% C code goes here

		( { IsRecursive = non_recursive } ->
			[]
		;
			% the C code may call Mercury code which clobbers
			% the regs
			code_info__clear_all_registers
		),

		code_info__get_next_label(SkipLab),
		code_info__generate_failure(FailCode),
		{ CheckFailureCode = tree(node([
			if_val(lval(reg(r(1))), label(SkipLab)) -
				"Test for success of pragma_c_code"
			]), tree(FailCode, node([ label(SkipLab) - "" ])))
		},

		code_info__lock_reg(r(1)),
		pragma_acquire_regs(OutArgs, Regs),
		code_info__unlock_reg(r(1))
	;
		{ ShuffleR1_Code = empty },

		% c code goes here

		( { IsRecursive = non_recursive } ->
			[]
		;
			% the C code may call Mercury code which clobbers
			% the regs
			code_info__clear_all_registers
		),

		{ CheckFailureCode = empty },

		pragma_acquire_regs(OutArgs, Regs)
	),
	place_pragma_output_args_in_regs(OutArgs, Regs, Outputs),

	( { IsRecursive = non_recursive } ->
		{ Wrapped_C_Code = C_Code }
	;
		{ string__append_list([
				"\tsave_registers();\n{\n",
				C_Code, "\n}\n",
				"#ifndef CONSERVATIVE_GC\n",
				"\trestore_registers();\n",
				"#endif\n"
			], Wrapped_C_Code) }
	),
	{ PragmaCode = node([pragma_c(Decls, Inputs, Wrapped_C_Code, Outputs) - 
			"Pragma C inclusion"]) },
	{ Instr = tree(tree(tree(SaveVarsCode, InputVarsCode), ShuffleR1_Code), 
			tree(PragmaCode, CheckFailureCode)) }.

%---------------------------------------------------------------------------%

:- pred make_c_arg_list(list(var), list(maybe(string)), list(c_arg)).
:- mode make_c_arg_list(in, in, out) is det.

make_c_arg_list(Vars, Names, ArgNames) :-
	make_c_arg_list_2(Vars, Names, [], ArgNames0),
	list__reverse(ArgNames0, ArgNames).

:- pred make_c_arg_list_2(list(var), list(maybe(string)),
				list(c_arg), list(c_arg)).
:- mode make_c_arg_list_2(in, in, in, out) is det.

make_c_arg_list_2([], [], ArgNames, ArgNames).
make_c_arg_list_2([Var | Vars], [Name | Names], ArgNames0, ArgNames) :-
	make_c_arg_list_2(Vars, Names, [c_arg(Var, Name) | ArgNames0],
						ArgNames).
make_c_arg_list_2([], [_|_], _, _) :-
	error("code_gen:make_c_arg_list_2 - length mismatch").
make_c_arg_list_2([_|_], [], _, _) :-
	error("code_gen:make_c_arg_list_2 - length mismatch").

:- pred get_c_arg_list_vars(list(c_arg)::in, list(var)::out) is det.

get_c_arg_list_vars([], []).
get_c_arg_list_vars([c_arg(Var, _) | Args], [Var | Vars1]) :-
	get_c_arg_list_vars(Args, Vars1).


% pragma_select_out_args returns the list of variables which are outputs for
% a procedure

:- pred pragma_select_out_args(assoc_list(c_arg, arg_info), list(c_arg)).
:- mode pragma_select_out_args(in, out) is det.

pragma_select_out_args([], []).
pragma_select_out_args([V - arg_info(_Loc, Mode)|Rest], Out) :-
        pragma_select_out_args(Rest, Out0),
        (
                Mode = top_out
        ->
                Out = [V|Out0]
        ;
                Out = Out0
        ).

%---------------------------------------------------------------------------%

% pragma_select_in_args returns the list of variables which are inputs for
% a procedure

:- pred pragma_select_in_args(assoc_list(c_arg, arg_info), list(c_arg)).
:- mode pragma_select_in_args(in, out) is det.

pragma_select_in_args([], []).
pragma_select_in_args([V - arg_info(_Loc, Mode)|Rest], In) :-
        pragma_select_in_args(Rest, In0),
        (
                Mode = top_in
        ->
		In = [V|In0]
        ;
                In = In0
        ).

%---------------------------------------------------------------------------%

% make_pragma_decls fills returns the list of pragma_decls for the pragma_c
% data structure in the llds. It is essentially a list of pairs of type and
% variable name, so that declarations of the form "Type Name;" can be made.

:- pred make_pragma_decls(list(c_arg), list(pragma_c_decl),
				code_info, code_info).
:- mode make_pragma_decls(in, out, in, out) is det.

make_pragma_decls([], []) --> [].
make_pragma_decls([c_arg(Arg, ArgName) | ArgNames], Decls) -->
	( { ArgName = yes(Name) } ->
		code_info__variable_type(Arg, Type),
		{ Decl = pragma_c_decl(Type, Name) },
		{ Decls = [Decl | Decls1] },
		make_pragma_decls(ArgNames, Decls1)
	;
		% if the variable doesn't occur in the ArgNameMap,
		% it can't be used, so we just ignore it
		make_pragma_decls(ArgNames, Decls)
	).

%---------------------------------------------------------------------------%

% get_pragma_input_vars returns a list of pragma_c_inputs for the pragma_c
% data structure in the llds. It is essentially a list of the input variables,
% and the corresponding rvals assigned to those (C) variables.

:- pred get_pragma_input_vars(list(c_arg), list(pragma_c_input),
			code_tree, code_info, code_info).
:- mode get_pragma_input_vars(in, out, out, in, out) is det.

get_pragma_input_vars([], [], empty) --> [].
get_pragma_input_vars([c_arg(Arg, MaybeName) | Args], Inputs, Code) -->
	( { MaybeName = yes(Name) } ->
		code_info__variable_type(Arg, Type),
		code_info__produce_variable(Arg, Code0, Rval),
		{ Input = pragma_c_input(Name, Type, Rval) },
		{ Inputs = [Input | Inputs1] },
		{ Code = tree(Code0, Code1) },
		get_pragma_input_vars(Args, Inputs1, Code1)
	;
		% if the variable doesn't occur in the ArgNameMap,
		% it can't be used, so we just ignore it
		get_pragma_input_vars(Args, Inputs, Code)
	).

%---------------------------------------------------------------------------%

% pragma_acquire_regs acquires a list of registers in which to place each
% of the given variables.

:- pred pragma_acquire_regs(list(c_arg), list(reg), code_info, code_info).
:- mode pragma_acquire_regs(in, out, in, out) is det.

pragma_acquire_regs([], []) --> [].
pragma_acquire_regs([_V|Vars], [Reg|Regs]) -->
	code_info__acquire_reg(Reg),
	pragma_acquire_regs(Vars, Regs).

%---------------------------------------------------------------------------%

% place_pragma_output_args_in_regs returns a list of pragma_c_outputs, which
% are pairs of names of output registers and (C) variables which hold the
% output value.

:- pred place_pragma_output_args_in_regs(list(c_arg), list(reg),
			list(pragma_c_output), code_info, code_info).
:- mode place_pragma_output_args_in_regs(in, in, out, in, out) is det.

place_pragma_output_args_in_regs([], [], []) --> [].
place_pragma_output_args_in_regs([_X|_Xs], [], []) --> 
	{ error("place_pragma_output_args_in_regs: list length mismatch") }.
place_pragma_output_args_in_regs([], [_X|_Xs], []) -->
	{ error("place_pragma_output_args_in_regs: list length mismatch") }.
place_pragma_output_args_in_regs([Arg|Args], [Reg|Regs], [O|Outputs]) -->
	( { Arg = c_arg(A, yes(Name)) } ->
		code_info__variable_type(A, Type),
		code_info__release_reg(Reg),
		code_info__set_var_location(A, reg(Reg)),
		{ O = pragma_c_output(reg(Reg), Type, Name) },
		place_pragma_output_args_in_regs(Args, Regs, Outputs)
	;
		{ error("code_gen:place_pragma_output_args_in_regs") }
	).

%---------------------------------------------------------------------------%


:- pred code_gen__generate_semi_goal_2(hlds__goal_expr, hlds__goal_info,
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
		code_info__generate_semi_pre_commit(Label, PreCommit),
		code_gen__generate_goal(model_non, Goal, GoalCode),
		code_info__generate_semi_commit(Label, Commit),
		{ Code = tree(PreCommit, tree(GoalCode, Commit)) }
	).
code_gen__generate_semi_goal_2(disj(Goals, FV), _GoalInfo, Code) -->
	code_info__push_store_map(FV),
	disj_gen__generate_semi_disj(Goals, FV, Code),
	code_info__pop_store_map.
code_gen__generate_semi_goal_2(not(Goal), _GoalInfo, Code) -->
	code_gen__generate_negation(Goal, Code).
code_gen__generate_semi_goal_2(higher_order_call(PredVar, Args, Types, Modes,
		Det, _Follow), _CodeInfo, Code) -->
	call_gen__generate_higher_order_call(model_semi, PredVar, Args,
		Types, Modes, Det, Code).
code_gen__generate_semi_goal_2(
		call(PredId, ProcId, Args, Builtin, _, _, _Follow),
							_GoalInfo, Code) -->
	(
		{ hlds__is_builtin_is_internal(Builtin) }
	->
		call_gen__generate_semidet_builtin(PredId, ProcId, Args, Code)
	;
		code_info__set_succip_used(yes),
		call_gen__generate_semidet_call(PredId, ProcId, Args, Code)
	).
code_gen__generate_semi_goal_2(switch(Var, CanFail, CaseList, FV), GoalInfo,
		Instr) -->
	code_info__push_store_map(FV),
	switch_gen__generate_switch(model_semi, Var, CanFail,
		CaseList, FV, GoalInfo, Instr),
	code_info__pop_store_map.
code_gen__generate_semi_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal, FV),
							_GoalInfo, Instr) -->
	code_info__push_store_map(FV),
	ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, FV, Instr),
	code_info__pop_store_map.
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
		{ Uni = complicated_unify(_UniMode, _CanFail, _Follow) },
		{ error("code_gen__generate_semi_goal_2 - complicated_unify") }
	).

code_gen__generate_semi_goal_2(pragma_c_code(C_Code, IsRecursive,
		PredId, ModeId, Args, ArgNameMap), GoalInfo, Instr) -->
	code_gen__generate_pragma_c_code(model_semi, C_Code, IsRecursive,
			PredId, ModeId, Args, ArgNameMap, GoalInfo, Instr).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_gen__generate_negation(hlds__goal, code_tree,
					code_info, code_info).
:- mode code_gen__generate_negation(in, out, in, out) is det.

code_gen__generate_negation(Goal, Code) -->
		% for a negated simple test, we see if
		% we can do a more efficient mechanism that
		% doesn't require a cache flush.
	(
		{ Goal = unify(_, _, _, simple_test(L, R), _) - GoalInfo },
		code_info__can_generate_direct_branch(CodeAddr),
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, simple_neg, yes) }
	->
			% Because we're generating a goal
			% (special-cased, though it may be)
			% we need to apply the pre- and post-
			% updates.
		code_aux__pre_goal_update(GoalInfo),
		code_info__produce_variable(L, Code0, ValA),
		code_info__produce_variable(R, Code1, ValB),
		code_aux__post_goal_update(GoalInfo),
		code_info__variable_type(L, Type),
		{ Type = term__functor(term__atom("string"), [], _) ->
			Op = str_eq
		; Type = term__functor(term__atom("float"), [], _) ->
			Op = float_eq
		;
			Op = eq
		},
		{ TestCode = node([
			if_val(binop(Op, ValA, ValB), CodeAddr) -
				"test inequality"
		]) },
		{ Code = tree(Code0, tree(Code1, TestCode)) }
	;
		code_gen__generate_negation_general(model_semi, Goal, Code)
	).

:- pred code_gen__generate_negation_general(code_model, hlds__goal, code_tree,
					code_info, code_info).
:- mode code_gen__generate_negation_general(in, in, out, in, out) is det.

code_gen__generate_negation_general(CodeModel, Goal, Code) -->
		% make sure that any variables that became
		% nondet live during the negated goal are
		% no longer considered nondet live by reinstating
		% the initial set of nondet live variables.
	code_info__get_nondet_lives(NondetLives),
	code_info__get_globals(Globals),
	{ Goal = _NotGoal - GoalInfo },
	{ goal_info_cont_lives(GoalInfo, Lives) },
	{
		Lives = yes(Vars0)
	->
		Vars = Vars0
	;
		error("code_gen__generate_negation: no cont_lives!")
	},
		% make the failure cont before saving the heap
		% pointer because the cache gets flushed.
	code_info__make_known_failure_cont(Vars, no, ModContCode),
	{
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(Goal)
	->
		Reclaim = yes
	;
		Reclaim = no
	},
	code_info__maybe_save_hp(Reclaim, SaveHeapCode),
		% The contained goal cannot be nondet, because if it's
		% mode-correct, it won't have any output vars, and so
		% it will be semi-det.
	code_gen__generate_goal(model_semi, Goal, GoalCode),
	( { CodeModel = model_det } ->
		{ FailCode = empty }
	;
		code_info__generate_under_failure(FailCode)
	),
	code_info__restore_failure_cont(RestoreContCode),
	code_info__maybe_restore_hp(Reclaim, RestoreHeapCode),
	code_info__set_nondet_lives(NondetLives),
	{ Code = tree(tree(tree(ModContCode, SaveHeapCode), GoalCode),
			tree(FailCode, tree(RestoreContCode,
						RestoreHeapCode))) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_gen__generate_non_goal_2(hlds__goal_expr, hlds__goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_non_goal_2(in, in, out, in, out) is det.

code_gen__generate_non_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_goals(Goals, model_non, Code).
code_gen__generate_non_goal_2(some(_Vars, Goal), _GoalInfo, Code) -->
	{ Goal = _ - InnerGoalInfo },
	{ goal_info_get_code_model(InnerGoalInfo, CodeModel) },
	code_gen__generate_goal(CodeModel, Goal, Code).
code_gen__generate_non_goal_2(disj(Goals, FV), _GoalInfo, Code) -->
	code_info__push_store_map(FV),
	disj_gen__generate_non_disj(Goals, FV, Code),
	code_info__pop_store_map.
code_gen__generate_non_goal_2(not(_Goal), _GoalInfo, _Code) -->
	{ error("Cannot have a nondet negation.") }.
code_gen__generate_non_goal_2(higher_order_call(PredVar, Args, Types, Modes,
		Det, _Follow),
		_CodeInfo, Code) -->
	call_gen__generate_higher_order_call(model_non, PredVar, Args, Types,
		Modes, Det, Code).
code_gen__generate_non_goal_2(
		call(PredId, ProcId, Args, Builtin, _, _, _Follow),
							_GoalInfo, Code) -->
	(
		{ hlds__is_builtin_is_internal(Builtin) }
	->
		call_gen__generate_nondet_builtin(PredId, ProcId, Args, Code)
	;
		code_info__set_succip_used(yes),
		call_gen__generate_nondet_call(PredId, ProcId, Args, Code)
	).
code_gen__generate_non_goal_2(switch(Var, CanFail, CaseList, FV), GoalInfo,
		Instr) -->
	code_info__push_store_map(FV),
	switch_gen__generate_switch(model_non, Var, CanFail,
		CaseList, FV, GoalInfo, Instr),
	code_info__pop_store_map.
code_gen__generate_non_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal, FV),
							_GoalInfo, Instr) -->
	code_info__push_store_map(FV),
	ite_gen__generate_nondet_ite(CondGoal, ThenGoal, ElseGoal,
		FV, Instr),
	code_info__pop_store_map.
code_gen__generate_non_goal_2(unify(_L, _R, _U, _Uni, _C),
							_GoalInfo, _Code) -->
	{ error("Cannot have a nondet unification.") }.
code_gen__generate_non_goal_2(pragma_c_code(A, B, C, D, E, F), G, H) -->
	% it would make sense to abort, but we need to handle string__append,
	% which is implemented using pragma c_code, and whose reverse mode
	% is multidet (see library/string.m)
	code_gen__generate_det_goal_2(pragma_c_code(A, B, C, D, E, F), G, H).

%---------------------------------------------------------------------------%

code_gen__output_args([], LiveVals) :-
	set__init(LiveVals).
code_gen__output_args([_V - arg_info(Loc, Mode)|Args], Vs) :-
	code_gen__output_args(Args, Vs0),
	(
		Mode = top_out
	->
		code_util__arg_loc_to_register(Loc, Reg),
		set__insert(Vs0, reg(Reg), Vs)
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
		[Instrn - Comment| Instrns]) :-
	(
		Instrn0 = livevals(LiveVals0),
		Instrns0 \= [goto(succip) - _|_]
		% XXX we should also test for tailcalls
		% once we start generating them directly
	->
		set__insert(LiveVals0, stackvar(StackLoc), LiveVals1),
		Instrn = livevals(LiveVals1)
        ;
		Instrn0 = call(Target, ReturnLabel, LiveVals0, CM)
	->
		Instrn  = call(Target, ReturnLabel, 
			[live_lvalue(stackvar(StackLoc), succip, no) |
			LiveVals0], CM)
	;
		Instrn = Instrn0
	),
	code_gen__add_saved_succip(Instrns0, StackLoc, Instrns).

%---------------------------------------------------------------------------%

code_gen__ensure_vars_are_saved([], empty) --> [].
code_gen__ensure_vars_are_saved([V|Vs], Code) -->
	code_info__get_call_info(CallInfo),
	{ map__lookup(CallInfo, V, Slot) },
	code_info__place_var(V, Slot, Code0),
	code_gen__ensure_vars_are_saved(Vs, Code1),
	{ Code = tree(Code0, Code1) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
