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
:- import_module hlds, llds, code_info, io.

		% Translate a HLDS structure into an LLDS

:- pred generate_code(module_info, module_info, list(c_procedure),
						io__state, io__state).
:- mode generate_code(in, out, out, di, uo) is det.

:- pred generate_proc_code(proc_info, proc_id, pred_id, module_info, 
	shape_table, shape_table, c_procedure, io__state, io__state).
:- mode generate_proc_code(in, in, in, in, di, uo, out, di, uo) is det.

		% These predicates generate code for a goal

:- pred code_gen__generate_goal(code_model, hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_goal(in, in, out, in, out) is det.

:- pred code_gen__generate_det_goal(hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_det_goal(in, out, in, out) is det.

:- pred code_gen__generate_semi_goal(hlds__goal, code_tree,
							code_info, code_info).
:- mode code_gen__generate_semi_goal(in, out, in, out) is det.

:- pred code_gen__generate_non_goal(hlds__goal, code_tree,
							code_info, code_info).
:- mode code_gen__generate_non_goal(in, out, in, out) is det.

		% These predicates generate code for a goal
		% and leave all live values in locations
		% determined by the call_info structure.

:- pred code_gen__generate_forced_goal(code_model, hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_forced_goal(in, in, out, in, out) is det.

:- pred code_gen__generate_forced_det_goal(hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_forced_det_goal(in, out, in, out) is det.

:- pred code_gen__generate_forced_semi_goal(hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_forced_semi_goal(in, out, in, out) is det.

:- pred code_gen__generate_forced_non_goal(hlds__goal, code_tree,
						code_info, code_info).
:- mode code_gen__generate_forced_non_goal(in, out, in, out) is det.

:- pred code_gen__output_args(assoc_list(var, arg_info), set(lval)).
:- mode code_gen__output_args(in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char, string, list, varset, term, map, tree, require.
:- import_module type_util, mode_util, std_util, int, set.
:- import_module code_util, call_gen, unify_gen, ite_gen, switch_gen.
:- import_module disj_gen, globals, options, hlds_out.
:- import_module code_aux, middle_rec.

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
	( { pred_info_is_imported(PredInfo) }
	->
		{ Predicates0 = [] },
		{ ModuleInfo1 = ModuleInfo0 } 
	;
			% now generate code for this predicate.
		generate_pred_code(ModuleInfo0, ModuleInfo1, PredId,
					PredInfo, Predicates0) 
	),
#if NU_PROLOG
	{ module_info_shapes(ModuleInfo1, Shape_Table) },
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
		list(c_procedure), io__state, io__state).
:- mode generate_pred_code(in, out, in, in, out, di, uo) is det.

generate_pred_code(ModuleInfo0, ModuleInfo, PredId, PredInfo, Code) -->
		% extract a list of all the procedure ids for this predicate
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
	{ pred_info_proc_ids(PredInfo, ProcIds) },
		% generate all the procedures for this predicate
	{ module_info_shapes(ModuleInfo0, Shapes0) },
	generate_proc_list_code(ProcIds, PredId, PredInfo, ModuleInfo0,
		Shapes0, Shapes, [], Code),
	{ module_info_set_shapes(ModuleInfo0, Shapes, ModuleInfo) }.

% For all the modes of predicate PredId, generate the appropriate
% code (deterministic, semideterministic, or nondeterministic).

:- pred generate_proc_list_code(list(proc_id), pred_id, pred_info, module_info,
	shape_table, shape_table, list(c_procedure), list(c_procedure),
	io__state, io__state).
:- mode generate_proc_list_code(in, in, in, in, di, uo, di, uo, di, uo) is det.

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
	{ generate_category_code_2(CodeModel, Goal, CodeTree, SUsed, CodeInfo0,
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

:- pred generate_category_code_2(code_model, hlds__goal, code_tree, maybe(int),
				code_info, code_info).
:- mode generate_category_code_2(in, in, out, out, in, out) is det.

generate_category_code_2(model_det, Goal, Instrs, Used) -->
		% generate the code for the body of the clause
	(
		code_info__get_globals(Globals),
		{ globals__lookup_bool_option(Globals, middle_rec, yes) },
		middle_rec__match_det(Goal, Switch)
	->
		middle_rec__gen_det(Switch, Instrs),
		{ Used = no }
	;
		code_gen__generate_det_goal(Goal, Instr1),
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
		{ Instrs = tree(Instr0, tree(Instr1,Instr2)) }
	).

generate_category_code_2(model_semi, Goal, Instrs, Used) -->
		% Create a label for fall through on failure.
	code_info__get_next_label(FallThrough, no),
	code_info__push_failure_cont(known(FallThrough)),

		% generate the code for the body of the clause
	code_gen__generate_semi_goal(Goal, Instr1),
	code_gen__generate_semi_prolog(Instr0, Used),
	code_gen__generate_semi_epilog(Instr2),
	code_info__pop_failure_cont,

		% combine the prolog, body and epilog
	{ Instrs = tree(Instr0, tree(Instr1,Instr2)) }.

generate_category_code_2(model_non, Goal, Instrs, Used) -->
		% Ensure that on failure we do a `fail()'
	code_info__push_failure_cont(do_fail),

		% generate the code for the body of the clause
	code_gen__generate_non_goal(Goal, Instr1),
	code_gen__generate_non_prolog(Instr0, Used),
	code_gen__generate_non_epilog(Instr2),
	code_info__pop_failure_cont,	% just for symmetry ;-)

		% combine the prolog, body and epilog
	{ Instrs = tree(Instr0, tree(Instr1,Instr2)) }.

%---------------------------------------------------------------------------%

code_gen__generate_goal(model_det, Goal, Code) -->
	code_gen__generate_det_goal(Goal, Code).
code_gen__generate_goal(model_semi, Goal, Code) -->
	code_gen__generate_semi_goal(Goal, Code).
code_gen__generate_goal(model_non, Goal, Code) -->
	code_gen__generate_non_goal(Goal, Code).

%---------------------------------------------------------------------------%

code_gen__generate_forced_goal(Det, Goal, Code) -->
	code_gen__generate_goal(Det, Goal, CodeA),
	code_info__generate_forced_saves(CodeB),
	{ Code = tree(CodeA, CodeB) },
	code_info__remake_with_store_map.

%---------------------------------------------------------------------------%

code_gen__generate_forced_det_goal(Goal, Code) -->
	code_gen__generate_forced_goal(model_det, Goal, Code).

code_gen__generate_forced_semi_goal(Goal, Code) -->
	code_gen__generate_forced_goal(model_semi, Goal, Code).

code_gen__generate_forced_non_goal(Goal, Code) -->
	code_gen__generate_forced_goal(model_non, Goal, Code).

%---------------------------------------------------------------------------%

% generate a deterministic goal - this predicate really just
% arranges the information a bit more conveniently

code_gen__generate_det_goal(Goal - GoalInfo, Instr) -->
		% Make any changes to liveness before Goal
	code_aux__pre_goal_update(GoalInfo),
	code_info__get_instmap(InstMap),
	(
		{ InstMap \= unreachable }
	->
			% generate goal
		code_gen__generate_det_goal_2(Goal, GoalInfo, Instr0),
			% Make live any variables which subsequent goals
			% will expect to be live, but were not generated
		code_info__set_instmap(InstMap),
		code_aux__post_goal_update(GoalInfo),
		code_info__get_globals(Options),
		(
			{ globals__lookup_bool_option(Options, lazy_code, yes) }
		->
			{ Instr1 = empty }
		;
			{ error("Eager code unavailable") }
%%%			code_info__generate_eager_flush(Instr1)
		),
		{ Instr = tree(Instr0, Instr1) }
	;
		{ Instr = empty }
	).

:- pred code_gen__generate_det_goal_2(hlds__goal_expr, hlds__goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_det_goal_2(in, in, out, in, out) is det.

code_gen__generate_det_goal_2(conj(Goals), _GoalInfo, Instr) -->
	code_gen__generate_det_goals(Goals, Instr).
code_gen__generate_det_goal_2(some(_Vars, Goal), _GoalInfo, Instr) -->
	code_gen__generate_det_goal(Goal, Instr).
code_gen__generate_det_goal_2(disj(_Goals), _GoalInfo, _Instr) -->
	{ error("Disjunction cannot occur in deterministic code.") }.
code_gen__generate_det_goal_2(not(_), _GoalInfo, _Instr) -->
	{ error("Negation cannot occur in deterministic code.") }.
code_gen__generate_det_goal_2(
		call(PredId, ProcId, Args, Builtin, _, _Follow),
							_GoalInfo, Instr) -->
	(
		{ is_builtin__is_internal(Builtin) }
	->
		call_gen__generate_det_builtin(PredId, ProcId, Args, Instr)
	;
		code_info__set_succip_used(yes),
		call_gen__generate_det_call(PredId, ProcId, Args, Instr)
	).
code_gen__generate_det_goal_2(switch(Var, CanFail, CaseList), GoalInfo, Instr) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		switch_gen__generate_switch(model_det,
						Var, CanFail, CaseList, Instr),
		code_info__pop_store_map
	;
		switch_gen__generate_switch(model_det,
						Var, CanFail, CaseList, Instr)
	).
code_gen__generate_det_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
							GoalInfo, Instr) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, Instr),
		code_info__pop_store_map
	;
		ite_gen__generate_det_ite(CondGoal, ThenGoal, ElseGoal, Instr)
	).
code_gen__generate_det_goal_2(unify(L, R, _U, Uni, _C), _GoalInfo, Instr) -->
	(
		{ Uni = assign(Left, Right) }
	->
		unify_gen__generate_assignment(Left, Right, Instr)
	;
		{ Uni = construct(Var, ConsId, Args, Modes) }
	->
		unify_gen__generate_construction(Var, ConsId, Args,
								Modes, Instr)
	;
		{ Uni = deconstruct(Var, ConsId, Args, Modes, _Det) }
	->
		unify_gen__generate_det_deconstruction(Var, ConsId, Args,
								Modes, Instr)
	;
		{ L = term__variable(Var1) },
		{ R = term__variable(Var2) },
		{ Uni = complicated_unify(UniMode, CanFail, _Follow) }
	->
		call_gen__generate_complicated_unify(Var1, Var2, UniMode,
			CanFail, Instr)
	;
		{ error("Cannot generate det code for semidet unifications") }
	).

%---------------------------------------------------------------------------%

% Generate a conjoined series of goals.
% Note of course, that with a [deterministic] conjunction, state information
% flows directly from one to the next atom.

:- pred code_gen__generate_det_goals(hlds__goals, code_tree,
							code_info, code_info).
:- mode code_gen__generate_det_goals(in, out, in, out) is det.

		% generating a deterministic
		% conjunction is straight forward.
code_gen__generate_det_goals([], empty) --> [].
code_gen__generate_det_goals([Goal | Goals], Instr) -->
		% generate this goal
	code_gen__generate_det_goal(Goal, Instr1),
	code_info__get_instmap(InstMap),
	(
		{ InstMap = unreachable }
	->
		{ Instr = Instr1 }
	;
			% generate the rest of the goals
		code_gen__generate_det_goals(Goals, Instr2),
		{ Instr = tree(Instr1, Instr2) }
	).

%---------------------------------------------------------------------------%

:- pred code_gen__generate_det_prolog(code_tree, maybe(int), code_info, code_info).
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
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
		Entry) },
	{ CodeA = node([
		label(Entry) - "Procedure entry point",
		comment(CallInfoComment) - ""
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
		{ CodeB = tree(
			CodeA,
			node([ incr_sp(NS) - "Allocate stack frame" ])
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
	{ CodeB1 = node([ goto(succip, succip) -
		"Return from procedure call"]) },
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
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
		Entry) },
	{ CodeA = node([
		label(Entry) - "Procedure entry point",
		comment(CallInfoComment) - ""
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
		{ CodeB = tree(
			CodeA,
			node([ incr_sp(NS) - "Allocate stack frame" ])
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
	{assoc_list__from_corresponding_lists(HeadVars,ArgModes,Args) },
	(
		{ Instmap = unreachable }
	->
		{ CodeA = empty }
	;
		code_info__setup_call(Args, callee, CodeA)
	),
	code_info__get_succip_used(Used),
	code_info__get_total_stackslot_count(NS0),
	code_info__failure_cont(FailCont),
	{ code_gen__output_args(Args, LiveArgs0) },
	{ set__insert(LiveArgs0, reg(r(1)), LiveArgs) },
	{ SLiveValCode = node([
		livevals(LiveArgs) - ""
	]) },
	{ set__singleton_set(LiveArg, reg(r(1))) },
	{ FLiveValCode = node([
		livevals(LiveArg) - ""
	]) },
	{ FailCont = known(FallThrough0) ->
		FallThrough = FallThrough0
	;
		error("semi_epilogue: invalid failure cont")
	},
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
			node([ goto(succip, succip)
				- "Return from procedure call" ])
		),
		tree(
			node([
				label(FallThrough) - "FallThrough"
			]),
			tree(
				tree(Failure, FLiveValCode),
				node([ goto(succip, succip) -
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
	{ code_util__make_local_entry_label(ModuleInfo, PredId, ProcId,
		Entry) },
	{ CodeA = node([
		label(Entry) - "Procedure entry point",
		comment(CallInfoComment) - ""
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
	{assoc_list__from_corresponding_lists(HeadVars,ArgModes,Args) },
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
		goto(do_succeed(no), do_succeed(no)) - "Succeed"
	])) },
	{ EStart = node([comment("Start of procedure epilogue") - ""]) },
	{ EEnd = node([comment("End of procedure epilogue") - ""]) },
	{ Instr = tree(tree(EStart, CodeA), tree(ExitCode, EEnd)) }.

%---------------------------------------------------------------------------%

code_gen__generate_semi_goal(Goal - GoalInfo, Instr) -->
	code_aux__pre_goal_update(GoalInfo),
	code_info__get_instmap(InstMap),
	(
		{ InstMap \= unreachable }
	->
		{ goal_info_get_internal_code_model(GoalInfo, CodeModel) },
		(
			{ CodeModel = model_det },
			code_gen__generate_det_goal_2(Goal, GoalInfo, Instr0)
		;
			{ CodeModel = model_semi },
			code_gen__generate_semi_goal_2(Goal, GoalInfo, Instr0)
		;
			{ CodeModel = model_non },
			code_info__generate_pre_commit(PreCommit, FailLabel),
			code_gen__generate_non_goal_2(Goal, GoalInfo, GoalCode),
			code_info__generate_commit(FailLabel, Commit),
			{ Instr0 = tree(PreCommit, tree(GoalCode, Commit)) }
		),
		code_info__set_instmap(InstMap),
		code_aux__post_goal_update(GoalInfo),
		code_info__get_globals(Options),
		(
			{ globals__lookup_bool_option(Options, lazy_code, yes) }
		->
			{ Instr1 = empty }
		;
			{ error("Eager code unavailable") }
%%%			code_info__generate_eager_flush(Instr1)
		),
		{ Instr = tree(Instr0, Instr1) }
	;
		{ Instr = empty }
	),
	!.

:- pred code_gen__generate_semi_goal_2(hlds__goal_expr, hlds__goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_semi_goal_2(in, in, out, in, out) is det.

code_gen__generate_semi_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_semi_goals(Goals, Code).
code_gen__generate_semi_goal_2(some(_Vars, Goal), _GoalInfo, Code) -->
	code_gen__generate_semi_goal(Goal, Code).
code_gen__generate_semi_goal_2(disj(Goals), GoalInfo, Code) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		disj_gen__generate_semi_disj(Goals, Code),
		code_info__pop_store_map
	;
		disj_gen__generate_semi_disj(Goals, Code)
	).
code_gen__generate_semi_goal_2(not(Goal), _GoalInfo, Code) -->
	code_gen__generate_negation(Goal, Code).
code_gen__generate_semi_goal_2(
		call(PredId, ProcId, Args, Builtin, _, _Follow),
							_GoalInfo, Code) -->
	(
		{ is_builtin__is_internal(Builtin) }
	->
		call_gen__generate_semidet_builtin(PredId, ProcId, Args, Code)
	;
		code_info__set_succip_used(yes),
		call_gen__generate_semidet_call(PredId, ProcId, Args, Code)
	).
code_gen__generate_semi_goal_2(switch(Var, CanFail, CaseList), GoalInfo, Instr) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		switch_gen__generate_switch(model_semi,
						Var, CanFail, CaseList, Instr),
		code_info__pop_store_map
	;
		switch_gen__generate_switch(model_semi,
						Var, CanFail, CaseList, Instr)
	).
code_gen__generate_semi_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
							GoalInfo, Instr) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, Instr),
		code_info__pop_store_map
	;
		ite_gen__generate_semidet_ite(CondGoal, ThenGoal, ElseGoal, Instr)
	).
code_gen__generate_semi_goal_2(unify(L, R, _U, Uni, _C),
							_GoalInfo, Code) -->
	(
		{ Uni = assign(Left, Right) }
	->
		unify_gen__generate_assignment(Left, Right, Code)
	;
		{ Uni = construct(Var, ConsId, Args, Modes) }
	->
		unify_gen__generate_construction(Var, ConsId, Args,
								Modes, Code)
	;
		{ Uni = deconstruct(Var, ConsId, Args, Modes, _) }
	->
		unify_gen__generate_semi_deconstruction(Var, ConsId, Args,
								Modes, Code)
	;
		{ Uni = simple_test(Var1, Var2) }
	->
		unify_gen__generate_test(Var1, Var2, Code)
	;
		{ L = term__variable(Var1) },
		{ R = term__variable(Var2) },
		{ Uni = complicated_unify(UniMode, CanFail, _Follow) }
	->
		call_gen__generate_complicated_unify(Var1, Var2, UniMode,
			CanFail, Code)
	;
		{ error("code_gen__generate_semi_goal_2: unify") }
	).

%---------------------------------------------------------------------------%

:- pred code_gen__generate_semi_goals(hlds__goals, code_tree,
							code_info, code_info).
:- mode code_gen__generate_semi_goals(in, out, in, out) is det.

		% generating a deterministic
		% conjunction is straight forward.
code_gen__generate_semi_goals([], empty) --> [].
code_gen__generate_semi_goals([Goal | Goals], Instr) -->
		% generate this goal
	code_gen__generate_semi_goal(Goal, Instr1),
		% generate the rest of the goals
	code_info__get_instmap(InstMap),
	(
		{ InstMap = unreachable }
	->
		{ Instr = Instr1 }
	;
			% generate the rest of the goals
		code_gen__generate_semi_goals(Goals, Instr2),
		{ Instr = tree(Instr1, Instr2) }
	).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred code_gen__generate_negation(hlds__goal, code_tree,
					code_info, code_info).
:- mode code_gen__generate_negation(in, out, in, out) is det.

code_gen__generate_negation(Goal, Code) -->
	code_info__get_globals(Globals),
	{
		globals__lookup_bool_option(Globals,
			reclaim_heap_on_semidet_failure, yes),
		code_util__goal_may_allocate_heap(Goal)
	->
		Reclaim = yes
	;
		Reclaim = no
	},
	code_info__get_next_label(SuccLab, no),
	code_info__push_failure_cont(known(SuccLab)),
	code_info__maybe_save_hp(Reclaim, SaveHeapCode),
	code_info__generate_nondet_saves(SaveCode),
		% The contained goal cannot be nondet, because if it's
		% mode-correct, it won't have any output vars, and so
		% it will be semi-det.
	code_gen__generate_semi_goal(Goal, GoalCode),
	code_info__remake_with_call_info,
	code_info__maybe_restore_hp(Reclaim, RestoreHeapCode),
	code_info__pop_failure_cont,
	code_info__generate_failure(FailCode),
	{ SuccessCode = node([
		label(SuccLab) - "negated goal failed, so proceed"
	]) },
	{ Code = tree(tree(tree(SaveHeapCode, SaveCode), GoalCode),
			tree(FailCode, tree(SuccessCode, RestoreHeapCode))) }.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

code_gen__generate_non_goal(Goal - GoalInfo, Instr) -->
	code_aux__pre_goal_update(GoalInfo),
	code_info__get_instmap(InstMap),
	(
		{ InstMap \= unreachable }
	->
		{ goal_info_get_internal_code_model(GoalInfo, CodeModel) },
		(
			{ CodeModel = model_det },
			code_gen__generate_det_goal_2(Goal, GoalInfo, Instr0)
		;
			{ CodeModel = model_semi },
			code_gen__generate_semi_goal_2(Goal, GoalInfo, Instr0)
		;
			{ CodeModel = model_non },
			code_gen__generate_non_goal_2(Goal, GoalInfo, Instr0)
		),
		code_info__set_instmap(InstMap),
		code_aux__post_goal_update(GoalInfo),
		code_info__get_globals(Options),
		(
			{ globals__lookup_bool_option(Options, lazy_code, yes) }
		->
			{ Instr1 = empty }
		;
			{ error("Eager code unavailable") }
%%%		       code_info__generate_eager_flush(Instr1)
		),
		{ Instr = tree(Instr0, Instr1) }
	;
		{ Instr = empty }
	).

:- pred code_gen__generate_non_goal_2(hlds__goal_expr, hlds__goal_info,
					code_tree, code_info, code_info).
:- mode code_gen__generate_non_goal_2(in, in, out, in, out) is det.

code_gen__generate_non_goal_2(conj(Goals), _GoalInfo, Code) -->
	code_gen__generate_non_goals(Goals, Code).
code_gen__generate_non_goal_2(some(_Vars, Goal), _GoalInfo, Code) -->
	code_gen__generate_non_goal(Goal, Code).
code_gen__generate_non_goal_2(disj(Goals), GoalInfo, Code) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		disj_gen__generate_non_disj(Goals, Code),
		code_info__pop_store_map
	;
		disj_gen__generate_non_disj(Goals, Code)
	).
code_gen__generate_non_goal_2(not(_Goal), _GoalInfo, _Code) -->
	{ error("Cannot have a nondet negation.") }.
code_gen__generate_non_goal_2(
		call(PredId, ProcId, Args, Builtin, _, _Follow),
							_GoalInfo, Code) -->
	(
		{ is_builtin__is_internal(Builtin) }
	->
		call_gen__generate_nondet_builtin(PredId, ProcId, Args, Code)
	;
		code_info__set_succip_used(yes),
		call_gen__generate_nondet_call(PredId, ProcId, Args, Code)
	).
code_gen__generate_non_goal_2(switch(Var, CanFail, CaseList), GoalInfo, Instr) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		switch_gen__generate_switch(model_non,
						Var, CanFail, CaseList, Instr),
		code_info__pop_store_map
	;
		switch_gen__generate_switch(model_non,
						Var, CanFail, CaseList, Instr)
	).
code_gen__generate_non_goal_2(
		if_then_else(_Vars, CondGoal, ThenGoal, ElseGoal),
							GoalInfo, Instr) -->
	{ goal_info_store_map(GoalInfo, StoreMap0) },
	(
		{ StoreMap0 = yes(StoreMap) }
	->
		code_info__push_store_map(StoreMap),
		ite_gen__generate_nondet_ite(CondGoal, ThenGoal, ElseGoal, Instr),
		code_info__pop_store_map
	;
		ite_gen__generate_nondet_ite(CondGoal, ThenGoal, ElseGoal, Instr)
	).
code_gen__generate_non_goal_2(unify(_L, _R, _U, _Uni, _C),
							_GoalInfo, _Code) -->
	{ error("Cannot have a nondet unification.") }.

%---------------------------------------------------------------------------%

:- pred code_gen__generate_non_goals(hlds__goals, code_tree,
							code_info, code_info).
:- mode code_gen__generate_non_goals(in, out, in, out) is det.

		% generating a deterministic
		% conjunction is straight forward.
code_gen__generate_non_goals([], empty) --> [].
code_gen__generate_non_goals([Goal | Goals], Instr) -->
		% generate this goal
	code_gen__generate_non_goal(Goal, Instr1),
		% generate the rest of the goals
	code_info__get_instmap(InstMap),
	(
		{ InstMap = unreachable }
	->
		{ Instr = Instr1 }
	;
			% generate the rest of the goals
		code_gen__generate_non_goals(Goals, Instr2),
		{ Instr = tree(Instr1, Instr2) }
	).

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

:- pred code_gen__add_saved_succip(list(instruction), int, list(instruction)).
:- mode code_gen__add_saved_succip(in, in, out) is det.

code_gen__add_saved_succip([], _N, []).
code_gen__add_saved_succip([I0-S|Is0], N, [I-S|Is]) :-
	(
		I0 = livevals(L0),
		Is0 \= [goto(succip, succip) - _|_]
		% XXX we should also test for tailcalls
		% once we start generating them directly
	->
		set__insert(L0, stackvar(N), L1),
		I = livevals(L1)
        ;
		I0 = call(T, R, C, LV0)
	->
		I = call(T, R, C, [live_lvalue(stackvar(N), -1)|LV0])
	;
		I = I0
	),
	code_gen__add_saved_succip(Is0, N, Is).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
