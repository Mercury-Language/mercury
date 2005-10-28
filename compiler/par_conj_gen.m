%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002-2005 University of Melbourne.
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
%
%   [Operational semantics]
%   - `,'/2 gives some operational guarantees that `&'/2 does not:
%     if `--no-reorder-conj' is set, there is an implied ordering
%     in the code:  conjunctions must not be reordered beyond the
%     minimum necessary for mode correctness.
%     This is justified for reasons performance modeling and ensuring
%     predicatable termination properties.
%     Parallel conjunction does not of itself suggest any information
%     about which order two goals should be executed, however if
%     coroutining (not currently implemented) is being used, then the
%     data dependancies between the two goals will constrain the order
%     of execution at runtime.
%
%   [Mode correctness]
%   - `,'/2 has a *sequential* behaviour `A, B' proves `A' *then*
%     proves `B'. Mode analysis only allows unidirectional data-
%     dependancies for conjunction. In independant and-parallelism,
%     for the goal `A & B', mode analysis requires that `A' and `B'
%     bind disjoint sets of free variables (or when mode analysis
%     supports it properly, disjoint sets of type-nodes), and that
%     `A' does not require any bindings made in `B' and vice versa.
%     In dependant and-parallelism, mode analysis requires that each
%     variable (or type-node) have a unique producer (as in independant
%     and-parallelism), but an and-parallel goal may use bindings made
%     in conjoined goals which may lead to coroutining.
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
% In principle, the determinism of a parallel conjunction is derived from
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

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.

:- import_module list.

:- pred generate_par_conj(list(hlds_goal)::in, hlds_goal_info::in,
    code_model::in, code_tree::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.

%---------------------------------------------------------------------------%

generate_par_conj(Goals, GoalInfo, CodeModel, Code, !CI) :-
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi,
        error("sorry, semidet parallel conjunction not implemented")
    ;
        CodeModel = model_non,
        error("sorry, nondet parallel conjunction not implemented")
    ),
    code_info__get_globals(!.CI, Globals),
    globals__lookup_int_option(Globals, sync_term_size, STSize),
    code_info__get_known_variables(!.CI, Vars),
    code_info__save_variables_on_stack(Vars, SaveCode, !CI),
    goal_info_get_code_gen_nonlocals(GoalInfo, Nonlocals),
    set__to_sorted_list(Nonlocals, Variables),
    code_info__get_instmap(!.CI, Initial),
    goal_info_get_instmap_delta(GoalInfo, Delta),
    instmap__apply_instmap_delta(Initial, Delta, Final),
    code_info__get_module_info(!.CI, ModuleInfo),
    find_outputs(Variables, Initial, Final, ModuleInfo, [], Outputs),
    list__length(Goals, NumGoals),
    code_info__acquire_reg(r, RegLval, !CI),
    code_info__acquire_temp_slot(sync_term, SyncSlot, !CI),
    code_info__acquire_temp_slot(lval(sp), SpSlot, !CI),
    MakeTerm = node([
        assign(SpSlot, lval(sp))
            - "save the parent stack pointer",
        incr_hp(RegLval, no, no, const(int_const(STSize)),
            "synchronization vector")
            - "allocate a synchronization vector",
        init_sync_term(RegLval, NumGoals)
            - "initialize sync term",
        assign(SyncSlot, lval(RegLval))
            - "store the sync-term on the stack"
    ]),
    code_info__release_reg(RegLval, !CI),
    code_info__clear_all_registers(no, !CI),
    generate_det_par_conj_2(Goals, 0, SyncSlot, SpSlot, Initial, no,
        GoalCode, !CI),
    code_info__release_temp_slot(SyncSlot, !CI),
    Code = tree(tree(SaveCode, MakeTerm), GoalCode),
    code_info__clear_all_registers(no, !CI),
    place_all_outputs(Outputs, !CI).

:- pred generate_det_par_conj_2(list(hlds_goal)::in, int::in,
    lval::in, lval::in, instmap::in, branch_end::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_det_par_conj_2([], _N, _SyncTerm, _SpSlot, _Initial, _, empty, !CI).
generate_det_par_conj_2([Goal | Goals], N, SyncTerm, SpSlot,
        Initial, MaybeEnd0, Code, !CI) :-
    code_info__remember_position(!.CI, StartPos),
    code_info__get_next_label(ThisConjunct, !CI),
    code_info__get_next_label(NextConjunct, !CI),
    code_gen__generate_goal(model_det, Goal, ThisGoalCode, !CI),
    code_info__get_stack_slots(!.CI, AllSlots),
    code_info__get_known_variables(!.CI, Variables),
    set__list_to_set(Variables, LiveVars),
    map__select(AllSlots, LiveVars, StoreMap0),
    StoreMap = map__map_values(key_stack_slot_to_abs_locn, StoreMap0),
    code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
        SaveCode, !CI),
    Goal = _GoalExpr - GoalInfo,
    goal_info_get_instmap_delta(GoalInfo, Delta),
    instmap__apply_instmap_delta(Initial, Delta, Final),
    code_info__get_module_info(!.CI, ModuleInfo),
    find_outputs(Variables, Initial, Final, ModuleInfo, [], TheseOutputs),
    copy_outputs(!.CI, TheseOutputs, SpSlot, CopyCode),
    (
        Goals = [_ | _],
        code_info__reset_to_position(StartPos, !CI),
        code_info__get_total_stackslot_count(!.CI, NumSlots),
        ForkCode = node([
            fork(ThisConjunct, NextConjunct, NumSlots)
                - "fork off a child",
            label(ThisConjunct)
                - "child thread"
        ]),
        JoinCode = node([
            join_and_terminate(SyncTerm)
                - "finish",
            label(NextConjunct)
                - "start of the next conjunct"
        ])
    ;
        Goals = [],
        code_info__get_next_label(ContLab, !CI),
        ForkCode = empty,
        JoinCode = node([
            join_and_continue(SyncTerm, ContLab)
                - "sync with children then continue",
            label(ContLab)
                - "end of parallel conjunction"
        ])
    ),
    ThisCode = tree_list([ForkCode, ThisGoalCode, SaveCode, CopyCode,
        JoinCode]),
    N1 = N + 1,
    generate_det_par_conj_2(Goals, N1, SyncTerm, SpSlot, Initial, MaybeEnd,
        RestCode, !CI),
    Code = tree(ThisCode, RestCode).

:- pred find_outputs(list(prog_var)::in, instmap::in, instmap::in,
    module_info::in, list(prog_var)::in, list(prog_var)::out) is det.

find_outputs([], _Initial, _Final, _ModuleInfo, !Outputs).
find_outputs([Var | Vars],  Initial, Final, ModuleInfo, !Outputs) :-
    instmap__lookup_var(Initial, Var, InitialInst),
    instmap__lookup_var(Final, Var, FinalInst),
    ( mode_is_output(ModuleInfo, (InitialInst -> FinalInst)) ->
        !:Outputs = [Var | !.Outputs]
    ;
        !:Outputs = !.Outputs
    ),
    find_outputs(Vars, Initial, Final, ModuleInfo, !Outputs).

:- pred copy_outputs(code_info::in, list(prog_var)::in, lval::in,
    code_tree::out) is det.

copy_outputs(_, [], _, empty).
copy_outputs(CI, [Var | Vars], SpSlot, Code) :-
    code_info__get_variable_slot(CI, Var, SrcSlot),
    ( SrcSlot = stackvar(SlotNum) ->
        NegSlotNum = (- SlotNum),
        DestSlot = field(yes(0), lval(SpSlot), const(int_const(NegSlotNum)))
    ;
        error("par conj in model non procedure!")
    ),
    ThisCode = node([
        assign(DestSlot, lval(SrcSlot))
            - "copy result to parent stackframe"
    ]),
    Code = tree(ThisCode, RestCode),
    copy_outputs(CI, Vars, SpSlot, RestCode).

:- pred place_all_outputs(list(prog_var)::in, code_info::in, code_info::out)
    is det.

place_all_outputs([], !CI).
place_all_outputs([Var | Vars], !CI) :-
    code_info__variable_locations(!.CI, VarLocations),
    code_info__get_variable_slot(!.CI, Var, Slot),
    (
        map__search(VarLocations, Var, Locations),
        set__member(Slot, Locations)
    ->
        true
    ;
        code_info__set_var_location(Var, Slot, !CI)
    ),
    place_all_outputs(Vars, !CI).
