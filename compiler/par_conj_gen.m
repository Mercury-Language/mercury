%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: par_conj.m:
%
% Main authors: conway.
%
% The predicates of this module generate code for parallel conjunctions.
%
%---------------------------------------------------------------------------%
%
% Notes on parallel conjunction:
%
% A parallel conjunction (A & B) denotes that the goals `A' and `B' should
% be executed concurrently. Parallel conjunction has exactly the same
% declarative semantics as normal conjunction, but it has different (stricter)
% rules for mode-correctness and determinism-correctness, and it has different
% operational semantics.
%	[Operational semantics]
%	- `,'/2 gives some operational guarantees that `&'/2 does not:
%	  if `--no-reorder-conj' is set, there is an implied ordering
%	  in the code:  conjunctions must not be reordered beyond the
%	  minimum necessary for mode correctness.
%	  This is justified for reasons performance modeling and ensuring
%	  predicatable termination properties.
%	  Parallel conjunction does not of itself suggest any information
%	  about which order two goals should be executed, however if
%	  coroutining (not currently implemented) is being used, then the
%	  data dependancies between the two goals will constrain the order
%	  of execution at runtime.
%	[Mode correctness]
%	- `,'/2 has a *sequential* behaviour `A, B' proves `A' *then*
%	  proves `B'. Mode analysis only allows unidirectional data-
%	  dependancies for conjunction. In independant and-parallelism,
%	  for the goal `A & B', mode analysis requires that `A' and `B'
%	  bind disjoint sets of free variables (or when mode analysis
%	  supports it properly, disjoint sets of type-nodes), and that
%	  `A' does not require any bindings made in `B' and vice versa.
%	  In dependant and-parallelism, mode analysis requires that each
%	  variable (or type-node) have a unique producer (as in independant
%	  and-parallelism), but an and-parallel goal may use bindings made
%	  in conjoined goals which may lead to coroutining.
%
% The current implementation only supports independant and-parallelism.
% The syntax for parallel conjunction is `&'/2 which behaves like `,'/2
% in that sequences get flattened (ie A & (B & C) <=> (A & B) & C).
%
% Type checking works exactly the same for parallel conjunction as it does
% for sequential conjunction.
%
% Mode analysis schedules a parallel conjunction if all the conjuncts can
% be scheduled independantly, and they bind disjoint sets of variables
% (type-nodes). This is done by mode checking each conjunct with the same
% initial instmap and `locking' (as is done for the nonlocal variables of a
% negation[1]) any variables that get bound in that conjunct before
% recursively processing the rest of the parallel conjunction. At the end of
% the conjunction the final instmaps from the conjuncts are merged by unifying
% them. Since the variable `locking' ensures that the variables bound by each
% conjunct are distinct from those bound by the other conjuncts, the
% unification of the instmaps is guarenteed to succeed.
%
% In principal, the determinism of a parallel conjunction is derived from
% its conjuncts in the same way as the determinism of a conjunction but
% because the current runtime implementation only allows model_det parallel
% conjunction, determinism analysis works by inferring the determinism of
% each conjunct and reporting an error if it is not a model_det determinism.
%
% We conservatively require that any variable that is nonlocal to more
% than one parallel conjunct become shared at the start of the parallel
% conjunction. This avoids problems where one conjunct has a use in a
% di mode and another in a ui mode. This would introduce an implicit
% dependency between the two conjuncts, which at present is illegal,
% since parallel conjunction is currently *independent* parallel
% conjunction only.
%
% The code generated for a parallel conjunction consists of a piece of
% initialization code which creates a term on the heap to be used for
% controlling the synchronization of the conjuncts and the code for the
% conjuncts each proceeded by a command to start the conjunct as a new
% thead of execution (except the last which executes in the "parent"
% thread), and each succeeded by a command that signals that the execution
% of the conjunct has completed and terminates the thread (except for
% the "parent" thread which suspends till all the other parallel conjuncts
% have terminated, when it will be woken up). The synchronization terms
% are refered to in the code as 'sync_term's.
%
% The runtime support for parallel conjunction is documented in the runtime
% directory in mercury_context.{c,h}.
%
%---------------------------------------------------------------------------%

:- module ll_backend__par_conj_gen.

:- interface.

:- import_module hlds__hlds_goal, backend_libs__code_model, ll_backend__llds.
:- import_module ll_backend__code_info.
:- import_module list.

:- pred par_conj_gen__generate_par_conj(list(hlds_goal)::in,
	hlds_goal_info::in, code_model::in, code_tree::out,
	code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_data, parse_tree__inst. 
:- import_module hlds__hlds_module, hlds__hlds_data, hlds__instmap.
:- import_module check_hlds__mode_util.
:- import_module ll_backend__code_gen, ll_backend__code_util.
:- import_module ll_backend__code_info, ll_backend__continuation_info.
:- import_module libs__options, libs__globals, libs__tree.

:- import_module bool, int, list, set, map, std_util, require.

%---------------------------------------------------------------------------%

par_conj_gen__generate_par_conj(Goals, GoalInfo, CodeModel, Code) -->
	{
		CodeModel = model_det
	;
		CodeModel = model_semi,
		error("sorry, semidet parallel conjunction not implemented")
	;
		CodeModel = model_non,
		error("sorry, nondet parallel conjunction not implemented")
	},
	code_info__get_globals(Globals),
	{ globals__lookup_int_option(Globals, sync_term_size, STSize) },
	code_info__get_known_variables(Vars),
	code_info__save_variables_on_stack(Vars, SaveCode),
	{ goal_info_get_code_gen_nonlocals(GoalInfo, Nonlocals) },
	{ set__to_sorted_list(Nonlocals, Variables) },
	code_info__get_instmap(Initial),
	{ goal_info_get_instmap_delta(GoalInfo, Delta) },
	{ instmap__apply_instmap_delta(Initial, Delta, Final) },
	code_info__get_module_info(ModuleInfo),
	{ par_conj_gen__find_outputs(Variables, Initial, Final, ModuleInfo,
			[], Outputs) },
	{ list__length(Goals, NumGoals) },
	code_info__acquire_reg(r, RegLval),
	code_info__acquire_temp_slot(sync_term, SyncSlot),
	code_info__acquire_temp_slot(lval(sp), SpSlot),
	{ MakeTerm = node([
		assign(SpSlot, lval(sp))
			- "save the parent stack pointer",
		incr_hp(RegLval, no, const(int_const(STSize)),
			"synchronization vector")
			- "allocate a synchronization vector",
		init_sync_term(RegLval, NumGoals)
			- "initialize sync term",
		assign(SyncSlot, lval(RegLval))
			- "store the sync-term on the stack"
	]) },
	code_info__release_reg(RegLval),
	code_info__clear_all_registers(no),
	par_conj_gen__generate_det_par_conj_2(Goals, 0, SyncSlot, SpSlot,
		Initial, no, GoalCode),
	code_info__release_temp_slot(SyncSlot),
	{ Code = tree(tree(SaveCode, MakeTerm), GoalCode) },
	code_info__clear_all_registers(no),
	par_conj_gen__place_all_outputs(Outputs).

:- pred par_conj_gen__generate_det_par_conj_2(list(hlds_goal), int, lval, lval,
		instmap, branch_end, code_tree, code_info, code_info).
:- mode par_conj_gen__generate_det_par_conj_2(in, in, in, in,
		in, in, out, in, out) is det.

par_conj_gen__generate_det_par_conj_2([], _N, _SyncTerm, _SpSlot, _Initial,
		_, empty) --> [].
par_conj_gen__generate_det_par_conj_2([Goal|Goals], N, SyncTerm, SpSlot,
		Initial, MaybeEnd0, Code) -->
	code_info__remember_position(StartPos),
	code_info__get_next_label(ThisConjunct),
	code_info__get_next_label(NextConjunct),
	code_gen__generate_goal(model_det, Goal, ThisGoalCode),
	code_info__get_stack_slots(AllSlots),
	code_info__get_known_variables(Variables),
	{ set__list_to_set(Variables, LiveVars) },
	{ map__select(AllSlots, LiveVars, StoreMap) },
	code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
		SaveCode),
	{ Goal = _GoalExpr - GoalInfo },
	{ goal_info_get_instmap_delta(GoalInfo, Delta) },
	{ instmap__apply_instmap_delta(Initial, Delta, Final) },
	code_info__get_module_info(ModuleInfo),
	{ par_conj_gen__find_outputs(Variables, Initial, Final, ModuleInfo,
			[], TheseOutputs) },
	par_conj_gen__copy_outputs(TheseOutputs, SpSlot, CopyCode),
	(
		{ Goals = [_|_] }
	->
		code_info__reset_to_position(StartPos),
		code_info__get_total_stackslot_count(NumSlots),
		{ ForkCode = node([
			fork(ThisConjunct, NextConjunct, NumSlots)
				- "fork off a child",
			label(ThisConjunct)
				- "child thread"
		]) },
		{ JoinCode = node([
			join_and_terminate(SyncTerm)
				- "finish",
			label(NextConjunct)
				- "start of the next conjunct"
		]) }
	;
		code_info__get_next_label(ContLab),
		{ ForkCode = empty },
		{ JoinCode = node([
			join_and_continue(SyncTerm, ContLab)
				- "sync with children then continue",
			label(ContLab)
				- "end of parallel conjunction"
		]) }
	),
	{ ThisCode = tree(
		ForkCode,
		tree(ThisGoalCode, tree(tree(SaveCode, CopyCode), JoinCode))
	) },
	{ N1 is N + 1 },
	par_conj_gen__generate_det_par_conj_2(Goals, N1, SyncTerm, SpSlot,
			Initial, MaybeEnd, RestCode),
	{ Code = tree(ThisCode, RestCode) }.

:- pred par_conj_gen__find_outputs(list(prog_var), instmap, instmap,
		module_info, list(prog_var), list(prog_var)).
:- mode par_conj_gen__find_outputs(in, in, in, in, in, out) is det.

par_conj_gen__find_outputs([], _Initial, _Final, _ModuleInfo,
		Outputs, Outputs).
par_conj_gen__find_outputs([Var|Vars],  Initial, Final, ModuleInfo,
		Outputs0, Outputs) :-
	instmap__lookup_var(Initial, Var, InitialInst),
	instmap__lookup_var(Final, Var, FinalInst),
	(
		mode_is_output(ModuleInfo, (InitialInst -> FinalInst))
	->
		Outputs1 = [Var|Outputs0]
	;
		Outputs1 = Outputs0
	),
	par_conj_gen__find_outputs(Vars, Initial, Final, ModuleInfo,
			Outputs1, Outputs).

:- pred par_conj_gen__copy_outputs(list(prog_var), lval, code_tree,
		code_info, code_info).
:- mode par_conj_gen__copy_outputs(in, in, out, in, out) is det.

par_conj_gen__copy_outputs([], _, empty) --> [].
par_conj_gen__copy_outputs([Var|Vars], SpSlot, Code) -->
	code_info__get_variable_slot(Var, SrcSlot),
	(
		{ SrcSlot = stackvar(SlotNum) }
	->
		{ NegSlotNum is (- SlotNum) },
		{ DestSlot = field(yes(0), lval(SpSlot),
			const(int_const(NegSlotNum))) }
	;
		{ error("par conj in model non procedure!") }
	),
	{ ThisCode = node([
		assign(DestSlot, lval(SrcSlot))
			- "copy result to parent stackframe"
	]) },
	{ Code = tree(ThisCode, RestCode) },
	par_conj_gen__copy_outputs(Vars, SpSlot, RestCode).

:- pred par_conj_gen__place_all_outputs(list(prog_var), code_info, code_info).
:- mode par_conj_gen__place_all_outputs(in, in, out) is det.

par_conj_gen__place_all_outputs([]) --> [].
par_conj_gen__place_all_outputs([Var|Vars]) -->
	code_info__variable_locations(VarLocations),
	code_info__get_variable_slot(Var, Slot),
	(
		{ map__search(VarLocations, Var, Locations) },
		{ set__member(Slot, Locations) }
	->
		[]
	;
		code_info__set_var_location(Var, Slot)
	),
	par_conj_gen__place_all_outputs(Vars).
