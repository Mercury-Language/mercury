%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2000,2002-2007 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: par_conj.m.
% Main authors: conway, wangp.
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
%     predictable termination properties.
%     Parallel conjunction does not of itself suggest any information
%     about which order two goals should be executed, however if
%     coroutining is being used, then the data dependencies between
%     the two goals will constrain the order of execution at runtime.
%
%   [Mode correctness]
%   - `,'/2 has a *sequential* behaviour `A, B' proves `A' *then*
%     proves `B'. Mode analysis only allows unidirectional data-
%     dependencies for conjunction. In independent and-parallelism,
%     for the goal `A & B', mode analysis requires that `A' and `B'
%     bind disjoint sets of free variables (or when mode analysis
%     supports it properly, disjoint sets of type-nodes), and that
%     `A' does not require any bindings made in `B' and vice versa.
%     In dependant and-parallelism, mode analysis requires that each
%     variable (or type-node) have a unique producer (as in independent
%     and-parallelism), but an and-parallel goal may use bindings made
%     in conjoined goals which may lead to coroutining.
%
% The current implementation mainly independent and-parallelism and
% a subset of dependent and-parallelism (see dep_par_conj.m).
% The syntax for parallel conjunction is `&'/2 which behaves like `,'/2
% in that sequences get flattened (ie A & (B & C) <=> (A & B) & C).
%
% Type checking and mode analysis work exactly the same for parallel
% conjunction as for sequential conjunction.
%
% In principle, the determinism of a parallel conjunction is derived from
% its conjuncts in the same way as the determinism of a conjunction but
% because the current runtime implementation only allows model_det parallel
% conjunction, determinism analysis works by inferring the determinism of
% each conjunct and reporting an error if it is not a model_det determinism.
%
% The code generated for a parallel conjunction consists of a piece of
% initialization code which creates a term on the heap to be used for
% controlling the synchronization of the conjuncts and the code for the
% conjuncts.  The synchronization terms are referred to in the code as
% 'sync_term's.  Conjuncts are executed "left to right".  At the start of
% the i'th conjunct is a command to "spark" the i+1'th conjunct, i.e.
% record enough information to begin executing the next conjunct either
% in parallel, or to return to it after the current conjunct ends.
%
% At the end of each conjunct is a call to an join_and_continue instruction.
% It executes the next parallel conjunct or, if the parallel conjunction is
% finished, causes the code following the parallel conjunction to execute in
% the context that originated the parallel conjunction.  If the originating
% context can't execute the next conjunct and the parallel conjunction isn't
% finished, it must suspend and store its address in the sync term.  When a
% non-originating context later finds that the parallel conjunction _is_
% finished, it will then cause the originating context to resume execution
% at the join point.  Please see the implementation of MR_join_and_continue()
% for the details.
%
% The runtime support for parallel conjunction is documented in the runtime
% directory in mercury_context.{c,h}.
%
%---------------------------------------------------------------------------%

:- module ll_backend.par_conj_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_par_conj(list(hlds_goal)::in, hlds_goal_info::in,
    code_model::in, code_tree::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.code_info.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.exprn_aux.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module unit.

%---------------------------------------------------------------------------%

generate_par_conj(Goals, GoalInfo, CodeModel, Code, !CI) :-
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi,
        sorry(this_file, "semidet parallel conjunction not implemented")
    ;
        CodeModel = model_non,
        sorry(this_file, "nondet parallel conjunction not implemented")
    ),

    code_info.get_globals(!.CI, Globals),
    globals.lookup_int_option(Globals, sync_term_size, STSize),

    % When entering a parallel conjunctions at the shallowest level in
    % the procedure, we have to set the parent_sp register to the value
    % of the sp register, and restore it when the parallel conjunction
    % finishes.
    code_info.get_par_conj_depth(!.CI, Depth),
    ( Depth = 0 ->
        code_info.acquire_temp_slot(lval(parent_sp), ParentSpSlot, !CI),
        MaybeSetParentSpCode = node([
            llds_instr(assign(ParentSpSlot, lval(parent_sp)),
                "save the old parent stack pointer"),
            llds_instr(assign(parent_sp, lval(sp)),
                "set the parent stack pointer")
        ]),
        MaybeRestoreParentSpCode = node([
            llds_instr(assign(parent_sp, lval(ParentSpSlot)),
                "restore old parent stack pointer")
        ]),
        MaybeReleaseParentSpSlot = yes(ParentSpSlot)
    ;
        MaybeSetParentSpCode = empty,
        MaybeRestoreParentSpCode = empty,
        MaybeReleaseParentSpSlot = no
    ),

    code_info.get_known_variables(!.CI, Vars),
    code_info.save_variables_on_stack(Vars, SaveCode, !CI),

    goal_info_get_code_gen_nonlocals(GoalInfo, Nonlocals),
    set.to_sorted_list(Nonlocals, Variables),
    code_info.get_instmap(!.CI, Initial),
    goal_info_get_instmap_delta(GoalInfo, Delta),
    instmap.apply_instmap_delta(Initial, Delta, Final),
    code_info.get_module_info(!.CI, ModuleInfo),
    find_outputs(Variables, Initial, Final, ModuleInfo, [], Outputs),

    list.length(Goals, NumGoals),
    code_info.acquire_reg(reg_r, RegLval, !CI),
    code_info.acquire_persistent_temp_slot(sync_term, SyncSlot, !CI),
    ( SyncSlot = stackvar(SlotNum) ->
        ParentSyncSlot = parent_stackvar(SlotNum)
    ;
        unexpected(this_file, "generate_par_conj")
    ),

    MakeSyncTermCode = node([
        % The may_not_use_atomic here is conservative.
        llds_instr(incr_hp(RegLval, no, no, const(llconst_int(STSize)),
            "sync term", may_not_use_atomic_alloc),
            "allocate a sync term"),
        llds_instr(init_sync_term(RegLval, NumGoals),
            "initialize sync term"),
        llds_instr(assign(SyncSlot, lval(RegLval)),
            "store the sync term on the stack")
    ]),
    code_info.release_reg(RegLval, !CI),

    code_info.set_par_conj_depth(Depth+1, !CI),
    code_info.get_next_label(EndLabel, !CI),
    code_info.clear_all_registers(no, !CI),
    generate_det_par_conj_2(Goals, ParentSyncSlot, EndLabel, Initial, no,
        GoalCode, !CI),
    code_info.set_par_conj_depth(Depth, !CI),

    EndLabelCode = node([
        llds_instr(label(EndLabel), "end of parallel conjunction")
    ]),
    Code = tree_list([
        MaybeSetParentSpCode, SaveCode, MakeSyncTermCode,
        GoalCode, EndLabelCode, MaybeRestoreParentSpCode
    ]),

    % We can't release the sync slot right now, in case we are in a
    % nested parallel conjunction.  Consider:
    %
    %   (
    %       (A & B)   % inner1
    %   &
    %       (C & D)   % inner2
    %   )
    %
    % If inner1 released its sync slot now then it might end up being reused
    % by inner2.  But inner1 and inner2 could be executing simultaneously.
    % In general we can't release the sync slot of any parallel conjunction
    % until we leave the shallowest parallel conjunction, i.e. at depth 0.
    % For now we only release the sync slots of parallel conjunctions at the
    % top level.
    %
    % XXX release sync slots of nested parallel conjunctions
    %
    ( Depth = 0 ->
        code_info.release_persistent_temp_slot(SyncSlot, !CI)
    ;
        true
    ),
    (
        MaybeReleaseParentSpSlot = yes(ParentSpSlot1),
        code_info.release_temp_slot(ParentSpSlot1, !CI)
    ;
        MaybeReleaseParentSpSlot = no
    ),
    code_info.clear_all_registers(no, !CI),
    place_all_outputs(Outputs, !CI).

:- pred generate_det_par_conj_2(list(hlds_goal)::in,
    lval::in, label::in, instmap::in, branch_end::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_det_par_conj_2([], _ParentSyncTerm, _EndLabel,
        _Initial, _, empty, !CI).
generate_det_par_conj_2([Goal | Goals], ParentSyncTerm, EndLabel,
        Initial, MaybeEnd0, Code, !CI) :-
    code_info.remember_position(!.CI, StartPos),
    code_gen.generate_goal(model_det, Goal, ThisGoalCode0, !CI),
    replace_stack_vars_by_parent_sv(ThisGoalCode0, ThisGoalCode),

    code_info.get_stack_slots(!.CI, AllSlots),
    code_info.get_known_variables(!.CI, Variables),
    set.list_to_set(Variables, LiveVars),
    map.select(AllSlots, LiveVars, StoreMap0),
    StoreMap = map.map_values(key_stack_slot_to_abs_locn, StoreMap0),
    code_info.generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
        SaveCode0, !CI),
    replace_stack_vars_by_parent_sv(SaveCode0, SaveCode),

    (
        Goals = [_ | _],
        code_info.get_next_label(NextConjunct, !CI),
        code_info.reset_to_position(StartPos, !CI),
        ForkCode = node([
            llds_instr(fork(NextConjunct), "fork off a child")
        ]),
        JoinCode = node([
            llds_instr(join_and_continue(ParentSyncTerm, EndLabel), "finish"),
            llds_instr(label(NextConjunct), "start of the next conjunct")
        ])
    ;
        Goals = [],
        ForkCode = empty,
        JoinCode = node([
            llds_instr(join_and_continue(ParentSyncTerm, EndLabel), "finish")
        ])
    ),
    ThisCode = tree_list([ForkCode, ThisGoalCode, SaveCode, JoinCode]),
    generate_det_par_conj_2(Goals, ParentSyncTerm, EndLabel, Initial, MaybeEnd,
        RestCode, !CI),
    Code = tree(ThisCode, RestCode).

%-----------------------------------------------------------------------------%

    % In the code of parallel conjuncts we have to refer to stack slots in
    % the procedure's stack frame via the `parent_sp' register instead of the
    % usual `sp' register, as the conjunct could be running in a different
    % context.
    %
:- pred replace_stack_vars_by_parent_sv(code_tree::in, code_tree::out) is det.
:- pred replace_stack_vars_by_parent_sv_instrs(list(instruction)::in,
    list(instruction)::out) is det.
:- pred replace_stack_vars_by_parent_sv_lval(lval::in, lval::out,
    unit::in, unit::out) is det.

replace_stack_vars_by_parent_sv(!Code) :-
    tree.map(replace_stack_vars_by_parent_sv_instrs, !Code).

replace_stack_vars_by_parent_sv_instrs(!Instrs) :-
    list.map_foldl(
        transform_lval_in_instr(replace_stack_vars_by_parent_sv_lval),
        !Instrs, unit, _).

replace_stack_vars_by_parent_sv_lval(Lval0, Lval, !Acc) :-
    TransformRval = replace_stack_vars_by_parent_sv_lval,
    (
        ( Lval0 = stackvar(SlotNum)
        ; Lval0 = parent_stackvar(SlotNum)
        ),
        Lval = parent_stackvar(SlotNum)
    ;
        ( Lval0 = reg(_Type, _RegNum)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = temp(_Type, _TmpNum)
        ; Lval0 = framevar(_SlotNum)
        ; Lval0 = lvar(_Var)
        ; Lval0 = global_var_ref(_GlobalVarName)
        ),
        Lval = Lval0
    ;
        Lval0 = succip_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = succip_slot(Rval)
    ;
        Lval0 = redoip_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = redoip_slot(Rval)
    ;
        Lval0 = redofr_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = redofr_slot(Rval)
    ;
        Lval0 = succfr_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = succfr_slot(Rval)
    ;
        Lval0 = prevfr_slot(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = prevfr_slot(Rval)
    ;
        Lval0 = field(Tag, Rval1, Rval2),
        transform_lval_in_rval(TransformRval, Rval1, Rval3, !Acc),
        transform_lval_in_rval(TransformRval, Rval2, Rval4, !Acc),
        Lval = field(Tag, Rval3, Rval4)
    ;
        Lval0 = mem_ref(Rval0),
        transform_lval_in_rval(TransformRval, Rval0, Rval, !Acc),
        Lval = mem_ref(Rval)
    ).

%-----------------------------------------------------------------------------%

:- pred find_outputs(list(prog_var)::in, instmap::in, instmap::in,
    module_info::in, list(prog_var)::in, list(prog_var)::out) is det.

find_outputs([], _Initial, _Final, _ModuleInfo, !Outputs).
find_outputs([Var | Vars],  Initial, Final, ModuleInfo, !Outputs) :-
    instmap.lookup_var(Initial, Var, InitialInst),
    instmap.lookup_var(Final, Var, FinalInst),
    ( mode_is_output(ModuleInfo, (InitialInst -> FinalInst)) ->
        !:Outputs = [Var | !.Outputs]
    ;
        !:Outputs = !.Outputs
    ),
    find_outputs(Vars, Initial, Final, ModuleInfo, !Outputs).

:- pred place_all_outputs(list(prog_var)::in, code_info::in, code_info::out)
    is det.

place_all_outputs([], !CI).
place_all_outputs([Var | Vars], !CI) :-
    code_info.variable_locations(!.CI, VarLocations),
    code_info.get_variable_slot(!.CI, Var, Slot),
    (
        map.search(VarLocations, Var, Locations),
        set.member(Slot, Locations)
    ->
        true
    ;
        code_info.set_var_location(Var, Slot, !CI)
    ),
    place_all_outputs(Vars, !CI).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "par_conj_gen.m".

%----------------------------------------------------------------------------%
:- end_module par_conj_gen.
%----------------------------------------------------------------------------%
