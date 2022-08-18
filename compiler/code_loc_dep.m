%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: code_loc_dep.m.
% Main authors: conway, zs.
%
% This file defines the code_loc_dep type and various operations on it.
% The codeloc_dep structure is the location-dependent part of the state
% of the code generator. The other part of the code generator state,
% the persistent part, is in code_info.m.
%
% This file is organized into ten submodules:
%
%   - the code_loc_dep structure and its access predicates
%   - simple wrappers around access predicates
%   - handling branched control structures
%   - handling failure continuations
%   - handling liveness issues
%   - saving and restoring heap pointers, trail tickets etc
%   - interfacing to var_locn
%   - managing info about variable liveness around calls, as needed by
%     e.g. garbage collection
%   - managing stack slots
%   - support for debugging the code generator itself.
%
%---------------------------------------------------------------------------%

:- module ll_backend.code_loc_dep.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.options.
:- import_module ll_backend.code_info.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.trace_params.
:- import_module ll_backend.code_util.
:- import_module ll_backend.opt_debug.
:- import_module ll_backend.trace_gen.
:- import_module ll_backend.var_locn.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module stack.
:- import_module string.

%---------------------------------------------------------------------------%

    % Submodule for the code_loc_dep type and its access predicates.
    %
    % This submodule has the following components:
    %
    %   declarations for exported access predicates
    %   declarations for non-exported access predicates
    %   the definition of the type and the init predicate
    %   the definition of the get access predicates
    %   the definition of the set access predicates
    %
    % Please keep the order of mention of the various fields
    % consistent in each of these five components.

:- interface.

:- type code_loc_dep.

    % Create a new code_loc_dep structure. Return the outermost resumption
    % point, and info about the non-fixed stack slots used for exec tracing.
    %
:- pred code_loc_dep_init(abs_follow_vars::in, resume_point_info::out,
    code_info::in, code_info::out, code_loc_dep::out) is det.

:- pred get_forward_live_vars(code_loc_dep::in, set_of_progvar::out) is det.
:- pred get_instmap(code_loc_dep::in, instmap::out) is det.
:- pred get_par_conj_depth(code_loc_dep::in, int::out) is det.

:- pred set_forward_live_vars(set_of_progvar::in,
    code_loc_dep::in, code_loc_dep::out) is det.
:- pred set_instmap(instmap::in,
    code_loc_dep::in, code_loc_dep::out) is det.
:- pred set_par_conj_depth(int::in,
    code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred get_zombies(code_loc_dep::in, set_of_progvar::out) is det.
:- pred get_var_locn_info(code_loc_dep::in, var_locn_info::out) is det.
:- pred get_temps_in_use(code_loc_dep::in, set(lval)::out) is det.
:- pred get_fail_info(code_loc_dep::in, fail_info::out) is det.

:- pred set_zombies(set_of_progvar::in,
    code_loc_dep::in, code_loc_dep::out) is det.
:- pred set_var_locn_info(var_locn_info::in,
    code_loc_dep::in, code_loc_dep::out) is det.
:- pred set_temps_in_use(set(lval)::in,
    code_loc_dep::in, code_loc_dep::out) is det.
:- pred set_fail_info(fail_info::in,
    code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

    % This type records information about the state of the code generator
    % at a particular location in the HLDS code of the current procedure.
    % At the start of a branched control structure, the code generator
    % remembers the values of these fields (in a value of type position_info),
    % and starts generating code for each branch from the same
    % location-dependent state.

:- type code_loc_dep
    --->    code_loc_dep(
                % Variables that are forward live after this goal.
                cld_forward_live_vars   :: set_of_progvar,

                % Current insts of the live variables.
                cld_instmap             :: instmap,

                % Zombie variables; variables that are not forward live
                % but which are protected by an enclosing resume point.
                cld_zombies             :: set_of_progvar,

                % A map storing the information about the status of each known
                % variable. (Known vars = forward live vars + zombies.)
                cld_var_locn_info       :: var_locn_info,

                % The set of temporary locations currently in use. These lvals
                % must be all be keys in the map of temporary locations ever
                % used, which is one of the persistent fields below. Any keys
                % in that map which are not in this set are free for reuse.
                cld_temps_in_use        :: set(lval),

                % Information about how to manage failures.
                cld_fail_info           :: fail_info,

                % How deep in a nested parallel conjunction we are.
                % This is zero at the beginning of a procedure and
                % is incremented as we enter parallel conjunctions.
                cld_par_conj_depth      :: int
            ).

%---------------------------------------------------------------------------%

code_loc_dep_init(FollowVars, ResumePoint, !CI, !:CLD) :-
    get_module_info(!.CI, ModuleInfo),
    get_proc_info(!.CI, ProcInfo),
    module_info_get_globals(ModuleInfo, Globals),
    proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap),
    proc_info_get_liveness_info(ProcInfo, Liveness),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    build_input_arg_list(ProcInfo, ArgList),
    get_var_table(!.CI, VarTable),
    proc_info_get_stack_slots(ProcInfo, StackSlots),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    (
        UseFloatRegs = yes,
        FloatRegType = reg_f
    ;
        UseFloatRegs = no,
        FloatRegType = reg_r
    ),
    get_eff_trace_level(!.CI, EffTraceLevel),
    TraceEnabled = is_exec_trace_enabled_at_eff_trace_level(EffTraceLevel),
    (
        TraceEnabled = exec_trace_is_enabled,
        trace_fail_vars(ModuleInfo, ProcInfo, FailVars),
        MaybeFailVars = yes(FailVars),
        set_of_var.union(Liveness, FailVars, EffLiveness)
    ;
        TraceEnabled = exec_trace_is_not_enabled,
        MaybeFailVars = no,
        EffLiveness = Liveness
    ),
    init_var_locn_state(VarTable, FloatRegType, StackSlots, FollowVars,
        EffLiveness, ArgList, VarLocnInfo),
    stack.init(ResumePoints),
    globals.get_opt_tuple(Globals, OptTuple),
    AllowHijack = OptTuple ^ ot_allow_hijacks,
    DummyFailInfo = fail_info(ResumePoints, resume_point_unknown,
        may_be_different, not_inside_non_condition, AllowHijack),
    set.init(TempsInUse),
    Zombies = set_of_var.init,
    NestedParConjDepth = 0,
    !:CLD = code_loc_dep(
        Liveness,
        InstMap,
        Zombies,
        VarLocnInfo,
        TempsInUse,
        DummyFailInfo,  % init_fail_info will override this dummy value
        NestedParConjDepth
    ),
    init_fail_info(CodeModel, MaybeFailVars, ResumePoint, !CI, !CLD).

%---------------------------------------------------------------------------%

get_forward_live_vars(CLD, CLD ^ cld_forward_live_vars).
get_instmap(CLD, CLD ^ cld_instmap).
get_zombies(CLD, CLD ^ cld_zombies).
get_var_locn_info(CLD, CLD ^ cld_var_locn_info).
get_temps_in_use(CLD, CLD ^ cld_temps_in_use).
get_fail_info(CLD, CLD ^ cld_fail_info).
get_par_conj_depth(CLD, CLD ^ cld_par_conj_depth).

set_forward_live_vars(LV, !CLD) :-
    !CLD ^ cld_forward_live_vars := LV.
set_instmap(IM, !CLD) :-
    !CLD ^ cld_instmap := IM.
set_zombies(Zs, !CLD) :-
    !CLD ^ cld_zombies := Zs.
set_var_locn_info(EI, !CLD) :-
    !CLD ^ cld_var_locn_info := EI.
set_temps_in_use(TI, !CLD) :-
    !CLD ^ cld_temps_in_use := TI.
set_fail_info(FI, !CLD) :-
    !CLD ^ cld_fail_info := FI.
set_par_conj_depth(N, !CLD) :-
    !CLD ^ cld_par_conj_depth := N.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for simple wrappers around access predicates.

:- interface.

    % Get the table that contains advice about where variables should be put.
    %
:- pred get_follow_var_map(code_loc_dep::in, abs_follow_vars_map::out) is det.

    % Get the integer that gives the number of the next non-reserved register.
    %
:- pred get_next_non_reserved(code_loc_dep::in, reg_type::in, int::out) is det.

    % Set the table that contains advice about where variables should be put.
    %
:- pred set_follow_vars(abs_follow_vars::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred pre_goal_update(hlds_goal_info::in, has_subgoals::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred post_goal_update(hlds_goal_info::in,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

    % Get the set of variables currently needed by the resume points
    % of enclosing goals.
    %
:- func current_resume_point_vars(code_loc_dep) = set_of_progvar.

%---------------------------------------------------------------------------%

:- implementation.

get_follow_var_map(CLD, FollowVarMap) :-
    get_var_locn_info(CLD, VarLocnInfo),
    var_locn_get_follow_var_map(VarLocnInfo, FollowVarMap).

get_next_non_reserved(CLD, RegType, NextNonReserved) :-
    get_var_locn_info(CLD, VarLocnInfo),
    var_locn_get_next_non_reserved(VarLocnInfo, RegType, NextNonReserved).

set_follow_vars(FollowVars, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_set_follow_vars(FollowVars, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

%-----------------------------------------------------------------------------%

pre_goal_update(GoalInfo, HasSubGoals, !CLD) :-
    % The liveness pass puts resume_point annotations on some kinds of goals.
    % The parts of the code generator that handle those kinds of goals
    % should handle the resume point annotation as well; when they do,
    % they remove the annotation. The following code is a sanity check
    % to make sure that this has in fact been done.
    goal_info_get_resume_point(GoalInfo, ResumePoint),
    (
        ResumePoint = no_resume_point
    ;
        ResumePoint = resume_point(_, _),
        unexpected($pred, "pre_goal_update with resume point")
    ),
    goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
    (
        MaybeFollowVars = yes(FollowVars),
        set_follow_vars(FollowVars, !CLD)
    ;
        MaybeFollowVars = no
    ),
    % NOTE: We must be careful to apply deaths before births.
    goal_info_get_pre_deaths(GoalInfo, PreDeaths),
    rem_forward_live_vars(PreDeaths, !CLD),
    maybe_make_vars_forward_dead(PreDeaths, no, !CLD),
    goal_info_get_pre_births(GoalInfo, PreBirths),
    add_forward_live_vars(PreBirths, !CLD),
    (
        HasSubGoals = does_not_have_subgoals,
        goal_info_get_post_deaths(GoalInfo, PostDeaths),
        rem_forward_live_vars(PostDeaths, !CLD)
    ;
        HasSubGoals = has_subgoals
    ).

post_goal_update(GoalInfo, CI, !CLD) :-
    % note: we must be careful to apply deaths before births
    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    rem_forward_live_vars(PostDeaths, !CLD),
    maybe_make_vars_forward_dead(PostDeaths, no, !CLD),
    goal_info_get_post_births(GoalInfo, PostBirths),
    add_forward_live_vars(PostBirths, !CLD),
    make_vars_forward_live(PostBirths, CI, !CLD),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    get_instmap(!.CLD, InstMap0),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
    set_instmap(InstMap, !CLD).

%---------------------------------------------------------------------------%

current_resume_point_vars(CLD) = ResumeVars :-
    get_fail_info(CLD, FailInfo),
    FailInfo = fail_info(ResumePointStack, _, _, _, _),
    stack.det_top(ResumePointStack, ResumePointInfo),
    pick_first_resume_point(ResumePointInfo, ResumeMap, _),
    map.keys_as_set(ResumeMap, ResumeVarsSet),
    ResumeVars = set_of_var.set_to_bitset(ResumeVarsSet).

%---------------------------------------------------------------------------%

:- pred get_active_temps_data(code_info::in, code_loc_dep::in,
    assoc_list(lval, slot_contents)::out) is det.

get_active_temps_data(CI, CLD, Temps) :-
    get_temps_in_use(CLD, TempsInUse),
    get_temp_content_map(CI, TempContentMap),
    map.select(TempContentMap, TempsInUse, TempsInUseContentMap),
    map.to_assoc_list(TempsInUseContentMap, Temps).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for handling branched control structures.

:- interface.

:- type position_info.
:- type branch_end_info.

:- type branch_end == maybe(branch_end_info).

:- pred remember_position(code_loc_dep::in, position_info::out) is det.

:- pred reset_to_position(position_info::in,
    code_info::in, code_loc_dep::out) is det.

:- pred reset_resume_known(position_info::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_branch_end(abs_store_map::in, branch_end::in, branch_end::out,
    llds_code::out, code_loc_dep::in) is det.

:- pred after_all_branches(abs_store_map::in, branch_end::in,
    code_info::in, code_loc_dep::out) is det.

:- pred save_hp_in_branch(llds_code::out, lval::out,
    position_info::in, position_info::out, code_info::in, code_info::out)
    is det.

:- implementation.

:- type position_info
    --->    position_info(
                % The location-dependent part of the code_info
                % at a given position.
                code_loc_dep
            ).

:- type branch_end_info
    --->    branch_end_info(
                % The code_info at the end of a branch.
                % code_info
                code_loc_dep
            ).

remember_position(CLD, position_info(CLD)).

reset_to_position(position_info(!.CLD), CurCI, !:CLD) :-
    get_persistent_temps(CurCI, PersistentTemps),
    get_temps_in_use(!.CLD, TempsInUse0),
    set.union(PersistentTemps, TempsInUse0, TempsInUse),
    set_temps_in_use(TempsInUse, !CLD).

reset_resume_known(BranchStart, !CLD) :-
    BranchStart = position_info(BranchStartCLD),
    get_fail_info(BranchStartCLD, BranchStartFailInfo),
    get_fail_info(!.CLD, CurFailInfo),
    BranchStartFailInfo = fail_info(_, BSResumeKnown, _, _, _),
    CurFailInfo = fail_info(CurFailStack, _, CurCurfMaxfr, CurCondEnv,
        CurHijack),
    NewFailInfo = fail_info(CurFailStack, BSResumeKnown, CurCurfMaxfr,
        CurCondEnv, CurHijack),
    set_fail_info(NewFailInfo, !CLD).

generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, Code, !.CLD) :-
    % The code generator generates better code if it knows in advance where
    % each variable should go. We don't need to reset the follow_vars
    % afterwards, since every goal following a branched control structure
    % must in any case be annotated with its own follow_var set.
    map.to_assoc_list(StoreMap, AbsVarLocs),
    assoc_list.values(AbsVarLocs, AbsLocs),
    code_util.max_mentioned_abs_regs(AbsLocs, MaxRegR, MaxRegF),
    set_follow_vars(abs_follow_vars(StoreMap, MaxRegR + 1, MaxRegF + 1), !CLD),
    get_instmap(!.CLD, InstMap),
    ( if instmap_is_reachable(InstMap) then
        VarLocs = assoc_list.map_values_only(abs_locn_to_lval, AbsVarLocs),
        place_vars(VarLocs, Code, !CLD)
    else
        % With --opt-no-return-call, the variables that we would have saved
        % across a call that cannot return have had the last of their
        % code generation state destroyed, so calling place_vars would cause
        % a code generator abort. However, pretending that all the variables
        % are where the store map says they should be is perfectly fine,
        % since we can never reach the end of *this* branch anyway.
        remake_with_store_map(StoreMap, !CLD),
        Code = empty
    ),
    EndCLD1 = !.CLD,
    (
        MaybeEnd0 = no,
        MaybeEnd = yes(branch_end_info(EndCLD1))
    ;
        MaybeEnd0 = yes(branch_end_info(EndCLD0)),

        % Make sure the left context we leave the branched structure with
        % is valid for all branches.
        get_fail_info(EndCLD0, FailInfo0),
        get_fail_info(EndCLD1, FailInfo1),
        FailInfo0 = fail_info(_, ResumeKnown0, CurfrMaxfr0, CondEnv0, Hijack0),
        FailInfo1 = fail_info(R, ResumeKnown1, CurfrMaxfr1, CondEnv1, Hijack1),
        ( if
            ResumeKnown0 = resume_point_known(Redoip0),
            ResumeKnown1 = resume_point_known(Redoip1)
        then
            ResumeKnown = resume_point_known(Redoip0),
            expect(unify(Redoip0, Redoip1), $pred, "redoip mismatch")
        else
            ResumeKnown = resume_point_unknown
        ),
        ( if
            CurfrMaxfr0 = must_be_equal,
            CurfrMaxfr1 = must_be_equal
        then
            CurfrMaxfr = must_be_equal
        else
            CurfrMaxfr = may_be_different
        ),
        ( if
            Hijack0 = allow_hijacks,
            Hijack1 = allow_hijacks
        then
            Hijack = allow_hijacks
        else
            Hijack = do_not_allow_hijacks
        ),
        expect(unify(CondEnv0, CondEnv1), $pred,
            "some but not all branches inside a non condition"),
        FailInfo = fail_info(R, ResumeKnown, CurfrMaxfr, CondEnv0, Hijack),
        set_fail_info(FailInfo, EndCLD1, EndCLDA),

        % Make sure the "temps in use" set at the end of the branched control
        % structure includes every slot in use at the end of any branch.
        get_temps_in_use(EndCLD0, TempsInUse0),
        get_temps_in_use(EndCLD1, TempsInUse1),
        set.union(TempsInUse0, TempsInUse1, TempsInUse),
        set_temps_in_use(TempsInUse, EndCLDA, EndCLD),

        MaybeEnd = yes(branch_end_info(EndCLD))
    ).

after_all_branches(StoreMap, MaybeEnd, CI, !:CLD) :-
    (
        MaybeEnd = yes(BranchEnd),
        BranchEnd = branch_end_info(BranchEndCLD),
        reset_to_position(position_info(BranchEndCLD), CI, !:CLD),
        remake_with_store_map(StoreMap, !CLD)
    ;
        MaybeEnd = no,
        unexpected($pred, "no branches in branched control structure")
    ).

    % remake_with_store_map throws away the var_info data structure, forgetting
    % the current locations of all variables, and rebuilds it from scratch
    % based on the given store map. The new var_info will know about only
    % the variables present in the store map, and will believe they are
    % where the store map says they are.
    %
:- pred remake_with_store_map(abs_store_map::in,
    code_loc_dep::in, code_loc_dep::out) is det.

remake_with_store_map(StoreMap, !CLD) :-
    map.to_assoc_list(StoreMap, VarLocns),
    VarLvals = assoc_list.map_values_only(abs_locn_to_lval, VarLocns),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    reinit_var_locn_state(VarLvals, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

save_hp_in_branch(Code, Slot, Pos0, Pos, !CI) :-
    Pos0 = position_info(CLD0),
    save_hp(Code, Slot, !CI, CLD0, CLD),
    Pos = position_info(CLD).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for the handling of failure continuations.

    % The principles underlying this submodule of code_info.m are
    % documented in the file compiler/notes/failure.html, which also
    % defines terms such as "quarter hijack"). Some parts of the submodule
    % also require knowledge of compiler/notes/allocation.html.

:- interface.

:- type resume_map.

:- type resume_point_info.

    % `prepare_for_disj_hijack' should be called before entering
    % a disjunction. It saves the values of any nondet stack slots
    % the disjunction may hijack, and if necessary, sets the redofr
    % slot of the top frame to point to this frame. The code at the
    % start of the individual disjuncts will override the redoip slot.
    %
    % `undo_disj_hijack' should be called before entering the last disjunct
    % of a disjunction. It undoes the effects of `prepare_for_disj_hijack'.
    %
:- type disj_hijack_info.

:- pred prepare_for_disj_hijack(code_model::in,
    disj_hijack_info::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred undo_disj_hijack(disj_hijack_info::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

    % `prepare_for_ite_hijack' should be called before entering
    % an if-then-else. It saves the values of any nondet stack slots
    % the if-then-else may hijack, and if necessary, sets the redofr
    % slot of the top frame to point to this frame. Our caller
    % will then override the redoip slot to point to the start of
    % the else part before generating the code of the condition.
    % The maybe(lval) argument, if set to `yes', specifies the slot
    % holding the success record to use in deciding whether to execute
    % a region_ite_nondet_cond_fail operation at the start of the else branch.
    %
    % `ite_enter_then', which should be called after generating code for
    % the condition, sets up the failure state of the code generator
    % for generating the then-part, and returns the code sequences
    % to be used at the starts of the then-part and the else-part
    % to undo the effects of any hijacking.
    %
:- type ite_hijack_info.

:- pred prepare_for_ite_hijack(code_model::in,
    maybe(embedded_stack_frame_id)::in, ite_hijack_info::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred ite_enter_then(ite_hijack_info::in, resume_point_info::in,
    llds_code::out, llds_code::out, code_info::in, code_info::out,
    code_loc_dep::in, code_loc_dep::out) is det.

    % `enter_simple_neg' and `leave_simple_neg' should be called before
    % and after generating the code for a negated unification, in
    % situations where failure is a direct branch. We handle this case
    % specially, because it occurs frequently and should not require
    % a flushing of the expression cache, whereas the general way of
    % handling negations does require a flush. These two predicates
    % handle all aspects of the negation except for the unification itself.
    %
:- type simple_neg_info.

:- pred enter_simple_neg(list(prog_var)::in, hlds_goal_info::in,
    simple_neg_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred leave_simple_neg(hlds_goal_info::in, simple_neg_info::in,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

    % `prepare_for_det_commit' and `generate_det_commit' should be
    % called before and after generating the code for the multi goal
    % being cut across. If the goal succeeds, the commit will cut away
    % any choice points generated in the goal.
    %
    % The set_of_progvar should be the set of variables live before
    % the scope goal.
    %
:- type det_commit_info.

:- pred prepare_for_det_commit(add_trail_ops::in, add_region_ops::in,
    set_of_progvar::in, hlds_goal_info::in, det_commit_info::out,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_det_commit(det_commit_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

    % `prepare_for_semi_commit' and `generate_semi_commit' should be
    % called before and after generating the code for the nondet goal
    % being cut across. If the goal succeeds, the commit will cut
    % any choice points generated in the goal.
    %
    % The set_of_progvar should be the set of variables live before
    % the scope goal.
    %
:- type semi_commit_info.

:- pred prepare_for_semi_commit(add_trail_ops::in, add_region_ops::in,
    set_of_progvar::in, hlds_goal_info::in, semi_commit_info::out,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_semi_commit(semi_commit_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

    % Put the given resume point into effect, by pushing it on to
    % the resume point stack, and if necessary generating code to
    % override the redoip of the top nondet stack frame.
    %
:- pred effect_resume_point(resume_point_info::in, code_model::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred pop_resume_point(code_loc_dep::in, code_loc_dep::out) is det.

    % Return the details of the resume point currently on top of the
    % failure continuation stack.
    %
:- pred top_resume_point(code_loc_dep::in, resume_point_info::out) is det.

    % Call this predicate to say "we have just left a disjunction;
    % we don't know what address the following code will need to backtrack to".
    %
:- pred set_resume_point_to_unknown(code_loc_dep::in, code_loc_dep::out)
    is det.

    % Call this predicate to say "we have just returned from a model_non
    % call; we don't know what address the following code will need to
    % backtrack to, and there may now be nondet frames on top of ours
    % that do not have their redofr slots pointing to our frame".
    %
:- pred set_resume_point_and_frame_to_unknown(
    code_loc_dep::in, code_loc_dep::out) is det.

    % Generate code for executing a failure that is appropriate for the
    % current failure environment.
    %
:- pred generate_failure(llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

    % Generate code that checks if the given rval is false, and if yes,
    % executes a failure that is appropriate for the current failure
    % environment. The returned code_loc_dep will reflect where things are
    % if the fail path is NOT taken.
    %
:- pred fail_if_rval_is_false(rval::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

    % Checks whether the appropriate code for failure in the current
    % failure environment is a direct branch.
    %
:- pred failure_is_direct_branch(code_loc_dep::in, code_addr::out) is semidet.

    % Checks under what circumstances the current failure environment
    % would allow a model_non call at this point to be turned into a tail call,
    % provided of course that the return from the call is followed immediately
    % by succeed().
    %
:- pred may_use_nondet_tailcall(code_loc_dep::in, nondet_tail_call::out)
    is det.

    % Materialize the given variables into registers or stack slots.
    %
:- pred produce_vars(list(prog_var)::in, resume_map::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

    % Put the variables needed in enclosing failure continuations
    % into their stack slots.
    %
:- pred flush_resume_vars_to_stack(llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

    % Set up the resume_point_info structure.
    % The ResumeVars passed as the first arguments should be a sorted list
    % without duplicates.
    %
:- pred make_resume_point(set_of_progvar::in, resume_locs::in, resume_map::in,
    resume_point_info::out, code_info::in, code_info::out) is det.

    % Generate the code for a resume point.
    %
:- pred generate_resume_point(resume_point_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

    % List the variables that need to be preserved for the given resume point.
    %
:- pred resume_point_vars(resume_point_info::in, list(prog_var)::out) is det.

    % See whether the given resume point includes a code address that presumes
    % all the resume point variables to be in their stack slots. If yes,
    % return that code address; otherwise, abort the compiler.
    %
:- pred resume_point_stack_addr(resume_point_info::in, code_addr::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

    % The part of the code generator state that says how failures at the
    % current location should be handled. It is also called the failure
    % continuation stack.
    %
:- type fail_info
    --->    fail_info(
                stack(resume_point_info),
                resume_point_known,
                curfr_vs_maxfr,
                condition_env,
                maybe_allow_hijacks
            ).

    % A resumption point has one or two labels associated with it.
    % Backtracking can arrive at either label. The code following
    % each label will assume that the variables needed at the resumption
    % point are in the locations given by the resume_map associated with
    % the given label and nowhere else. Any code that can cause
    % backtracking to a label must make sure that those variables are
    % in the positions expected by the label.
    %
    % The only time when a code_addr in a resume_point info is not a label
    % is when the code_addr is do_redo or do_fail, which indicate that
    % the resumption point is either unknown or not in (this invocation of)
    % this procedure.
    %
:- type resume_point_info
    --->    orig_only(resume_map, code_addr)
    ;       stack_only(resume_map, code_addr)
    ;       orig_then_stack(resume_map, code_addr, resume_map, code_addr)
    ;       stack_then_orig(resume_map, code_addr, resume_map, code_addr).

    % A resume map maps the variables that will be needed at a resumption
    % point to the locations in which they will be.
    %
:- type resume_map == map(prog_var, set(lval)).

:- type redoip_update
    --->    has_been_done
    ;       wont_be_done.

:- type resume_point_known
    --->    resume_point_known(redoip_update)
    ;       resume_point_unknown.

:- type curfr_vs_maxfr
    --->    must_be_equal
    ;       may_be_different.

:- type condition_env
    --->    inside_non_condition
    ;       not_inside_non_condition.

%---------------------------------------------------------------------------%

:- type disj_hijack_info
    --->    disj_no_hijack
    ;       disj_temp_frame
    ;       disj_quarter_hijack
    ;       disj_half_hijack(
                % The stack slot in which we saved the value
                % of the hijacked redoip.
                lval
            )
    ;       disj_full_hijack(
                % The stack slot in which we saved the value
                % of the hijacked redoip.
                lval,

                % The stack slot in which we saved the value
                % of the hijacked redofr.
                lval
            ).

prepare_for_disj_hijack(CodeModel, HijackInfo, Code, !CI, !CLD) :-
    get_fail_info(!.CLD, FailInfo),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, CondEnv,
        Allow),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        HijackInfo = disj_no_hijack,
        Code = singleton(
            llds_instr(comment("disj no hijack"), "")
        )
    ;
        CodeModel = model_non,
        (
            CondEnv = inside_non_condition,
            HijackInfo = disj_temp_frame,
            create_temp_frame(do_fail, "prepare for disjunction", Code,
                !CI, !CLD)
        ;
            CondEnv = not_inside_non_condition,
            (
                Allow = do_not_allow_hijacks,
                ( if
                    CurfrMaxfr = must_be_equal,
                    ResumeKnown = resume_point_known(has_been_done),
                    stack.pop(TopResumePoint, ResumePoints, RestResumePoints),
                    stack.is_empty(RestResumePoints),
                    TopResumePoint = stack_only(_, do_fail)
                then
                    HijackInfo = disj_quarter_hijack,
                    Code = singleton(
                        llds_instr(comment("disj quarter hijack of do_fail"),
                            "")
                    )
                else
                    HijackInfo = disj_temp_frame,
                    create_temp_frame(do_fail, "prepare for disjunction", Code,
                        !CI, !CLD)
                )
            ;
                Allow = allow_hijacks,
                (
                    CurfrMaxfr = must_be_equal,
                    (
                        ResumeKnown = resume_point_known(has_been_done),
                        HijackInfo = disj_quarter_hijack,
                        Code = singleton(
                            llds_instr(comment("disj quarter hijack"), "")
                        )
                    ;
                        ( ResumeKnown = resume_point_known(wont_be_done)
                        ; ResumeKnown = resume_point_unknown
                        ),
                        acquire_temp_slot(slot_lval(redoip_slot(lval(curfr))),
                            non_persistent_temp_slot, RedoipSlot, !CI, !CLD),
                        HijackInfo = disj_half_hijack(RedoipSlot),
                        Code = singleton(
                            llds_instr(assign(RedoipSlot,
                                lval(redoip_slot(lval(curfr)))),
                                "prepare for half disj hijack")
                        )
                    )
                ;
                    CurfrMaxfr = may_be_different,
                    acquire_temp_slot(slot_lval(redoip_slot(lval(maxfr))),
                        non_persistent_temp_slot, RedoipSlot, !CI, !CLD),
                    acquire_temp_slot(slot_lval(redofr_slot(lval(maxfr))),
                        non_persistent_temp_slot, RedofrSlot, !CI, !CLD),
                    HijackInfo = disj_full_hijack(RedoipSlot, RedofrSlot),
                    Code = from_list([
                        llds_instr(
                            assign(RedoipSlot, lval(redoip_slot(lval(maxfr)))),
                            "prepare for full disj hijack"),
                        llds_instr(
                            assign(RedofrSlot, lval(redofr_slot(lval(maxfr)))),
                            "prepare for full disj hijack"),
                        llds_instr(
                            assign(redofr_slot(lval(maxfr)), lval(curfr)),
                            "prepare for full disj hijack")
                    ])
                )
            )
        )
    ).

undo_disj_hijack(HijackInfo, Code, !CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, CondEnv,
        Allow),
    (
        HijackInfo = disj_no_hijack,
        Code = empty
    ;
        HijackInfo = disj_temp_frame,
        Code = singleton(
            llds_instr(assign(maxfr, lval(prevfr_slot(lval(maxfr)))),
                "restore maxfr for temp frame disj")
        )
    ;
        HijackInfo = disj_quarter_hijack,
        expect(unify(CurfrMaxfr, must_be_equal), $pred,
            "maxfr may differ from curfr in disj_quarter_hijack"),
        stack.det_top(ResumePoints, ResumePoint),
        pick_stack_resume_point(ResumePoint, _, StackLabel),
        LabelConst = const(llconst_code_addr(StackLabel)),
        % peephole.m looks for the "curfr==maxfr" pattern in the comment.
        Code = singleton(
            llds_instr(assign(redoip_slot(lval(curfr)), LabelConst),
                "restore redoip for quarter disj hijack (curfr==maxfr)")
        )
    ;
        HijackInfo = disj_half_hijack(RedoipSlot),
        expect(unify(ResumeKnown, resume_point_unknown), $pred,
            "resume point known in disj_half_hijack"),
        expect(unify(CurfrMaxfr, must_be_equal), $pred,
            "maxfr may differ from curfr in disj_half_hijack"),
        % peephole.m looks for the "curfr==maxfr" pattern in the comment.
        Code = singleton(
            llds_instr(assign(redoip_slot(lval(curfr)), lval(RedoipSlot)),
                "restore redoip for half disj hijack (curfr==maxfr)")
        )
    ;
        HijackInfo = disj_full_hijack(RedoipSlot, RedofrSlot),
        expect(unify(CurfrMaxfr, may_be_different), $pred,
            "maxfr same as curfr in disj_full_hijack"),
        Code = from_list([
            llds_instr(assign(redoip_slot(lval(maxfr)), lval(RedoipSlot)),
                "restore redoip for full disj hijack"),
            llds_instr(assign(redofr_slot(lval(maxfr)), lval(RedofrSlot)),
                "restore redofr for full disj hijack")
        ])
    ),
    ( if
        % HijackInfo \= disj_no_hijack if and only if the disjunction
        % is model_non.
        HijackInfo \= disj_no_hijack,
        CondEnv = inside_non_condition
    then
        FailInfo = fail_info(ResumePoints, resume_point_unknown, CurfrMaxfr,
            CondEnv, Allow),
        set_fail_info(FailInfo, !CLD)
    else
        true
    ).

%---------------------------------------------------------------------------%

    % For model_non if-then-elses, we need to clean up the embedded stack frame
    % we create for the if-then-else when the condition fails after succeeding.
    % For such if-then-elses, we record the id of the embedded frame we need to
    % clean up, and the id of the slot that is initialized to false, and set to
    % true each time the condition succeeds.
:- type ite_region_info
    --->    ite_region_info(
                embedded_stack_frame_id,
                lval
            ).

:- type ite_hijack_info
    --->    ite_info(
                resume_point_known,
                condition_env,
                ite_hijack_type,
                maybe(ite_region_info)
            ).

:- type ite_hijack_type
    --->    ite_no_hijack
    ;       ite_temp_frame(
                % The stack slot in which we saved the value of maxfr.
                lval
            )
    ;       ite_quarter_hijack
    ;       ite_half_hijack(
                % The stack slot in which we saved the value
                % of the hijacked redoip.
                lval
            )
    ;       ite_full_hijack(
                % The stack slot in which we saved the value
                % of the hijacked redoip.
                lval,

                % The stack slot in which we saved the value
                % of the hijacked redofr.
                lval,

                % The stack slot in which we saved the value of maxfr.
                lval
            ).

prepare_for_ite_hijack(CondCodeModel, MaybeEmbeddedFrameId, HijackInfo, Code,
        !CI, !CLD) :-
    get_fail_info(!.CLD, FailInfo),
    FailInfo = fail_info(_, ResumeKnown, CurfrMaxfr, CondEnv, Allow),
    (
        % It is possible for a negated goal (which is the "Condition" of the
        % equivalent if-then-else) to be det, if it is also impure.
        ( CondCodeModel = model_det
        ; CondCodeModel = model_semi
        ),
        expect(unify(MaybeEmbeddedFrameId, no), $pred,
            "MaybeEmbeddedFrameId in model_semi"),
        HijackType = ite_no_hijack,
        Code = singleton(
            llds_instr(comment("ite no hijack"), "")
        ),
        MaybeRegionInfo = no
    ;
        CondCodeModel = model_non,
        ( if
            ( Allow = do_not_allow_hijacks
            ; CondEnv = inside_non_condition
            ; MaybeEmbeddedFrameId = yes(_)
            )
        then
            acquire_temp_slot(slot_lval(maxfr), non_persistent_temp_slot,
                MaxfrSlot, !CI, !CLD),
            HijackType = ite_temp_frame(MaxfrSlot),
            create_temp_frame(do_fail, "prepare for ite", TempFrameCode,
                !CI, !CLD),
            MaxfrCode = singleton(
                llds_instr(assign(MaxfrSlot, lval(maxfr)), "prepare for ite")
            ),
            (
                MaybeEmbeddedFrameId = yes(EmbeddedFrameId),
                % Note that this slot is intentionally not released anywhere.
                acquire_temp_slot(slot_success_record, persistent_temp_slot,
                    SuccessRecordSlot, !CI, !CLD),
                InitSuccessCode = singleton(
                    llds_instr(
                        assign(SuccessRecordSlot, const(llconst_false)),
                        "record no success of the condition yes")
                ),
                MaybeRegionInfo =
                    yes(ite_region_info(EmbeddedFrameId, SuccessRecordSlot))
            ;
                MaybeEmbeddedFrameId = no,
                InitSuccessCode = empty,
                MaybeRegionInfo = no
            ),
            Code = TempFrameCode ++ MaxfrCode ++ InitSuccessCode
        else
            (
                CurfrMaxfr = must_be_equal,
                (
                    ResumeKnown = resume_point_known(_),
                    HijackType = ite_quarter_hijack,
                    Code = singleton(
                        llds_instr(comment("ite quarter hijack"), "")
                    )
                ;
                    ResumeKnown = resume_point_unknown,
                    acquire_temp_slot(slot_lval(redoip_slot(lval(curfr))),
                        non_persistent_temp_slot, RedoipSlot, !CI, !CLD),
                    HijackType = ite_half_hijack(RedoipSlot),
                    Code = singleton(
                        llds_instr(
                            assign(RedoipSlot, lval(redoip_slot(lval(curfr)))),
                            "prepare for half ite hijack")
                    )
                )
            ;
                CurfrMaxfr = may_be_different,
                acquire_temp_slot(slot_lval(redoip_slot(lval(maxfr))),
                    non_persistent_temp_slot, RedoipSlot, !CI, !CLD),
                acquire_temp_slot(slot_lval(redofr_slot(lval(maxfr))),
                    non_persistent_temp_slot, RedofrSlot, !CI, !CLD),
                acquire_temp_slot(slot_lval(maxfr),
                    non_persistent_temp_slot, MaxfrSlot, !CI, !CLD),
                HijackType = ite_full_hijack(RedoipSlot, RedofrSlot,
                    MaxfrSlot),
                Code = from_list([
                    llds_instr(
                        assign(MaxfrSlot, lval(maxfr)),
                        "prepare for full ite hijack"),
                    llds_instr(
                        assign(RedoipSlot, lval(redoip_slot(lval(maxfr)))),
                        "prepare for full ite hijack"),
                    llds_instr(
                        assign(RedofrSlot, lval(redofr_slot(lval(maxfr)))),
                        "prepare for full ite hijack"),
                    llds_instr(
                        assign(redofr_slot(lval(maxfr)), lval(curfr)),
                        "prepare for full ite hijack")
                ])
            ),
            MaybeRegionInfo = no
        ),
        set_inside_non_condition(!CLD)
    ),
    HijackInfo = ite_info(ResumeKnown, CondEnv, HijackType, MaybeRegionInfo).

ite_enter_then(HijackInfo, ITEResumePoint, ThenCode, ElseCode, !CI, !CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, ResumeKnown0, CurfrMaxfr, _, Allow),
    stack.det_pop(_, ResumePoints0, ResumePoints),
    HijackInfo = ite_info(HijackResumeKnown, OldCondEnv, HijackType,
        MaybeRegionInfo),
    (
        HijackType = ite_no_hijack,
        expect(unify(MaybeRegionInfo, no), $pred,
            "MaybeRegionInfo ite_no_hijack"),
        ThenCode = empty,
        ElseCode = empty
    ;
        HijackType = ite_temp_frame(MaxfrSlot),
        (
            MaybeRegionInfo = no,
            ThenCode = singleton(
                % We can't remove the frame, it may not be on top.
                llds_instr(assign(redoip_slot(lval(MaxfrSlot)),
                    const(llconst_code_addr(do_fail))),
                    "soft cut for temp frame ite")
            ),
            ElseCode = singleton(
                % XXX search for assignments to maxfr
                llds_instr(assign(maxfr, lval(prevfr_slot(lval(MaxfrSlot)))),
                    "restore maxfr for temp frame ite")
            )
        ;
            MaybeRegionInfo = yes(RegionInfo),
            RegionInfo = ite_region_info(EmbeddedStackFrameId,
                SuccessRecordSlot),
            % XXX replace do_fail with ref to ResumePoint stack label
            resume_point_stack_addr(ITEResumePoint, ITEStackResumeCodeAddr),
            ThenCode = from_list([
                llds_instr(assign(SuccessRecordSlot, const(llconst_true)),
                    "record success of the condition"),
                llds_instr(assign(redoip_slot(lval(MaxfrSlot)),
                    const(llconst_code_addr(ITEStackResumeCodeAddr))),
                    "redirect to cut for temp frame ite")
            ]),
            get_next_label(AfterRegionOp, !CI),
            ElseCode = from_list([
                llds_instr(assign(maxfr, lval(prevfr_slot(lval(MaxfrSlot)))),
                    "restore maxfr for temp frame ite"),
                llds_instr(if_val(unop(logical_not, lval(SuccessRecordSlot)),
                    code_label(AfterRegionOp)),
                    "jump around if the condition never succeeded"),
                llds_instr(use_and_maybe_pop_region_frame(
                    region_ite_nondet_cond_fail, EmbeddedStackFrameId),
                    "cleanup after the post-success failure of the condition"),
                llds_instr(goto(do_fail),
                    "the condition succeeded, so don't execute else branch"),
                llds_instr(label(AfterRegionOp),
                    "after region op")
            ])
        )
    ;
        HijackType = ite_quarter_hijack,
        expect(unify(MaybeRegionInfo, no), $pred,
            "MaybeRegionInfo ite_quarter_hijack"),
        stack.det_top(ResumePoints, ResumePoint),
        ( if maybe_pick_stack_resume_point(ResumePoint, _, StackLabel) then
            LabelConst = const(llconst_code_addr(StackLabel)),
            ThenCode = singleton(
                llds_instr(assign(redoip_slot(lval(curfr)), LabelConst),
                    "restore redoip for quarter ite hijack")
            )
        else
            % This can happen only if ResumePoint is unreachable from here.
            ThenCode = empty
        ),
        ElseCode = ThenCode
    ;
        HijackType = ite_half_hijack(RedoipSlot),
        expect(unify(MaybeRegionInfo, no), $pred,
            "MaybeRegionInfo ite_half_hijack"),
        ThenCode = singleton(
            llds_instr(assign(redoip_slot(lval(curfr)), lval(RedoipSlot)),
                "restore redoip for half ite hijack")
        ),
        ElseCode = ThenCode
    ;
        HijackType = ite_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
        expect(unify(MaybeRegionInfo, no), $pred,
            "MaybeRegionInfo ite_full_hijack"),
        ThenCode = from_list([
            llds_instr(assign(redoip_slot(lval(MaxfrSlot)), lval(RedoipSlot)),
                "restore redoip for full ite hijack"),
            llds_instr(assign(redofr_slot(lval(MaxfrSlot)), lval(RedofrSlot)),
                "restore redofr for full ite hijack")
        ]),
        ElseCode = from_list([
            llds_instr(assign(redoip_slot(lval(maxfr)), lval(RedoipSlot)),
                "restore redoip for full ite hijack"),
            llds_instr(assign(redofr_slot(lval(maxfr)), lval(RedofrSlot)),
                "restore redofr for full ite hijack")
        ])
    ),
    (
        ResumeKnown0 = resume_point_unknown,
        ResumeKnown = resume_point_unknown
    ;
        ResumeKnown0 = resume_point_known(_),
        ResumeKnown = HijackResumeKnown
    ),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, OldCondEnv,
        Allow),
    set_fail_info(FailInfo, !CLD).

%---------------------------------------------------------------------------%

:- type simple_neg_info == fail_info.

enter_simple_neg(ResumeVars, GoalInfo, FailInfo0, !CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    % The only reason why we push a resume point at all is to protect
    % the variables in ResumeVars from becoming unknown; by including them
    % in the domain of the resume point, we guarantee that they will become
    % zombies instead of unknown if they die in the pre- or post-goal updates.
    % Therefore the only part of ResumePoint that matters is the set of
    % variables in the resume map; the other parts of ResumePoint
    % (the locations, the code address) will not be referenced.
    map.init(ResumeMap0),
    make_fake_resume_map(ResumeVars, ResumeMap0, ResumeMap),
    ResumePoint = orig_only(ResumeMap, do_redo),
    effect_resume_point(ResumePoint, model_semi, Code, !CLD),
    expect(is_empty(Code), $pred, "nonempty code for simple neg"),
    pre_goal_update(GoalInfo, does_not_have_subgoals, !CLD).

leave_simple_neg(GoalInfo, FailInfo, CI, !CLD) :-
    post_goal_update(GoalInfo, CI, !CLD),
    set_fail_info(FailInfo, !CLD).

:- pred make_fake_resume_map(list(prog_var)::in,
    map(prog_var, set(lval))::in, map(prog_var, set(lval))::out) is det.

make_fake_resume_map([], !ResumeMap).
make_fake_resume_map([Var | Vars], !ResumeMap) :-
    % A visibly fake location.
    Locns = set.make_singleton_set(reg(reg_r, -1)),
    map.det_insert(Var, Locns, !ResumeMap),
    make_fake_resume_map(Vars, !ResumeMap).

%---------------------------------------------------------------------------%

:- type det_commit_info
    --->    det_commit_info(
                % Location of saved maxfr.
                maybe(lval),

                % Location of saved ticket % counter and trail pointer.
                maybe(pair(lval)),

                maybe(region_commit_stack_frame)
            ).

:- type region_commit_stack_frame
    --->    region_commit_stack_frame(
                % The id of the region commit stack frame, which is emdedded
                % in the current procedure's stack frame, and whose layout is:

                % saved region_commit_stack_pointer
                % saved region sequence number
                % number of live nonprotected regions
                % space reserved for the ids of live nonprotected regions

                embedded_stack_frame_id,

                % The list of temporary slots that constitute
                % this embedded stack frame.
                list(lval)
            ).

prepare_for_det_commit(AddTrailOps, AddRegionOps, ForwardLiveVarsBeforeGoal,
        CommitGoalInfo, DetCommitInfo, Code, !CI, !CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(_, _, CurfrMaxfr, _, _),
    (
        CurfrMaxfr = may_be_different,
        acquire_temp_slot(slot_lval(maxfr), non_persistent_temp_slot,
            MaxfrSlot, !CI, !CLD),
        SaveMaxfrCode = singleton(
            llds_instr(save_maxfr(MaxfrSlot), "save the value of maxfr")
        ),
        MaybeMaxfrSlot = yes(MaxfrSlot)
    ;
        CurfrMaxfr = must_be_equal,
        SaveMaxfrCode = empty,
        MaybeMaxfrSlot = no
    ),
    maybe_save_trail_info(AddTrailOps, MaybeTrailSlots, SaveTrailCode,
        !CI, !CLD),
    maybe_save_region_commit_frame(AddRegionOps, ForwardLiveVarsBeforeGoal,
        CommitGoalInfo, MaybeRegionCommitFrameInfo, SaveRegionCommitFrameCode,
        !CI, !CLD),
    DetCommitInfo = det_commit_info(MaybeMaxfrSlot, MaybeTrailSlots,
        MaybeRegionCommitFrameInfo),
    Code = SaveMaxfrCode ++ SaveTrailCode ++ SaveRegionCommitFrameCode.

generate_det_commit(DetCommitInfo, Code, !CI, !CLD) :-
    DetCommitInfo = det_commit_info(MaybeMaxfrSlot, MaybeTrailSlots,
        MaybeRegionCommitFrameInfo),
    (
        MaybeMaxfrSlot = yes(MaxfrSlot),
        RestoreMaxfrCode = singleton(
            llds_instr(restore_maxfr(MaxfrSlot),
                "restore the value of maxfr - perform commit")
        ),
        release_temp_slot(MaxfrSlot, non_persistent_temp_slot, !CI, !CLD)
    ;
        MaybeMaxfrSlot = no,
        RestoreMaxfrCode = singleton(
            llds_instr(assign(maxfr, lval(curfr)),
                "restore the value of maxfr - perform commit")
        )
    ),
    maybe_restore_trail_info(MaybeTrailSlots, CommitTrailCode, _, !CI, !CLD),
    maybe_restore_region_commit_frame(MaybeRegionCommitFrameInfo,
        SuccessRegionCode, _FailureRegionCode, !CI, !CLD),
    Code = RestoreMaxfrCode ++ CommitTrailCode ++ SuccessRegionCode.

%---------------------------------------------------------------------------%

:- type semi_commit_info
    --->    semi_commit_info(
                % Fail_info on entry.
                fail_info,

                resume_point_info,
                commit_hijack_info,

                % Location of saved ticket counter and trail pointer.
                maybe(pair(lval)),

                maybe(region_commit_stack_frame)
            ).

:- type commit_hijack_info
    --->    commit_temp_frame(
                % The stack slot in which we saved the old value of maxfr.
                lval,

                % Do we bracket the goal with MR_commit_mark and MR_commit_cut?
                bool
            )
    ;       commit_quarter_hijack
    ;       commit_half_hijack(
                % The stack slot in which we saved the value
                % of the hijacked redoip.
                lval
            )
    ;       commit_full_hijack(
                % The stack slot in which we saved the value
                % of the hijacked redoip.
                lval,

                % The stack slot in which we saved the value
                % of the hijacked redofr.
                lval,

                % The stack slot in which we saved the value of maxfr.
                lval
            ).

prepare_for_semi_commit(AddTrailOps, AddRegionOps, ForwardLiveVarsBeforeGoal,
        CommitGoalInfo, SemiCommitInfo, Code, !CI, !CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, ResumeKnown, CurfrMaxfr, CondEnv,
        Allow),
    stack.det_top(ResumePoints0, TopResumePoint),
    clone_resume_point(TopResumePoint, NewResumePoint, !CI),
    stack.push(NewResumePoint, ResumePoints0, ResumePoints),
    FailInfo = fail_info(ResumePoints, resume_point_known(has_been_done),
        CurfrMaxfr, CondEnv, Allow),
    set_fail_info(FailInfo, !CLD),

    pick_stack_resume_point(NewResumePoint, _, StackLabel),
    StackLabelConst = const(llconst_code_addr(StackLabel)),
    ( if
        ( Allow = do_not_allow_hijacks ; CondEnv = inside_non_condition )
    then
        acquire_temp_slot(slot_lval(maxfr), non_persistent_temp_slot,
            MaxfrSlot, !CI, !CLD),
        MaxfrCode = singleton(
            llds_instr(save_maxfr(MaxfrSlot),
                "prepare for temp frame commit")
        ),
        create_temp_frame(StackLabel,
            "prepare for temp frame commit", TempFrameCode, !CI, !CLD),
        get_globals(!.CI, Globals),
        globals.lookup_bool_option(Globals, use_minimal_model_stack_copy_cut,
            UseMinimalModelStackCopyCut),
        HijackInfo = commit_temp_frame(MaxfrSlot, UseMinimalModelStackCopyCut),
        (
            UseMinimalModelStackCopyCut = yes,
            % If the code we are committing across starts but does not complete
            % the evaluation of a tabled subgoal, the cut will remove the
            % generator's choice point, so that the evaluation of the subgoal
            % will never be completed. We handle such "dangling" generators
            % by removing them from the subgoal trie of the tabled procedure.
            % This requires knowing what tabled subgoals are started inside
            % commits, which is why we wrap the goal being committed across
            % inside MR_commit_{mark,cut}.
            Components = [
                foreign_proc_raw_code(cannot_branch_away,
                    proc_affects_liveness, live_lvals_info(set.init),
                    "\t\tMR_save_transient_registers();\n"),
                foreign_proc_raw_code(cannot_branch_away,
                    proc_does_not_affect_liveness, live_lvals_info(set.init),
                    "\t\tMR_commit_mark();\n"),
                foreign_proc_raw_code(cannot_branch_away,
                    proc_affects_liveness, live_lvals_info(set.init),
                    "\t\tMR_restore_transient_registers();\n")
            ],
            MD = proc_may_duplicate,
            MarkCode = singleton(
                llds_instr(foreign_proc_code([], Components,
                    proc_will_not_call_mercury, no, no, no, no, no, no, MD),
                    "")
            )
        ;
            UseMinimalModelStackCopyCut = no,
            MarkCode = empty
        ),
        HijackCode = MaxfrCode ++ TempFrameCode ++ MarkCode
    else
        (
            CurfrMaxfr = must_be_equal,
            (
                ResumeKnown = resume_point_known(has_been_done),
                HijackInfo = commit_quarter_hijack,
                HijackCode = singleton(
                    llds_instr(assign(redoip_slot(lval(curfr)),
                        StackLabelConst),
                        "hijack the redofr slot")
                )
            ;
                ( ResumeKnown = resume_point_known(wont_be_done)
                ; ResumeKnown = resume_point_unknown
                ),
                acquire_temp_slot(slot_lval(redoip_slot(lval(curfr))),
                    non_persistent_temp_slot, RedoipSlot, !CI, !CLD),
                HijackInfo = commit_half_hijack(RedoipSlot),
                HijackCode = from_list([
                    llds_instr(assign(RedoipSlot,
                        lval(redoip_slot(lval(curfr)))),
                        "prepare for half commit hijack"),
                    llds_instr(assign(redoip_slot(lval(curfr)),
                        StackLabelConst),
                        "hijack the redofr slot")
                ])
            )
        ;
            CurfrMaxfr = may_be_different,
            acquire_temp_slot(slot_lval(redoip_slot(lval(maxfr))),
                non_persistent_temp_slot, RedoipSlot, !CI, !CLD),
            acquire_temp_slot(slot_lval(redofr_slot(lval(maxfr))),
                non_persistent_temp_slot, RedofrSlot, !CI, !CLD),
            acquire_temp_slot(slot_lval(maxfr),
                non_persistent_temp_slot, MaxfrSlot, !CI, !CLD),
            HijackInfo = commit_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
            HijackCode = from_list([
                llds_instr(assign(RedoipSlot, lval(redoip_slot(lval(maxfr)))),
                    "prepare for full commit hijack"),
                llds_instr(assign(RedofrSlot, lval(redofr_slot(lval(maxfr)))),
                    "prepare for full commit hijack"),
                llds_instr(save_maxfr(MaxfrSlot),
                    "prepare for full commit hijack"),
                llds_instr(assign(redofr_slot(lval(maxfr)), lval(curfr)),
                    "hijack the redofr slot"),
                llds_instr(assign(redoip_slot(lval(maxfr)), StackLabelConst),
                    "hijack the redoip slot")
            ])
        )
    ),
    maybe_save_trail_info(AddTrailOps, MaybeTrailSlots, SaveTrailCode,
        !CI, !CLD),
    maybe_save_region_commit_frame(AddRegionOps, ForwardLiveVarsBeforeGoal,
        CommitGoalInfo, MaybeRegionCommitFrameInfo, SaveRegionCommitFrameCode,
        !CI, !CLD),
    SemiCommitInfo = semi_commit_info(FailInfo0, NewResumePoint,
        HijackInfo, MaybeTrailSlots, MaybeRegionCommitFrameInfo),
    Code = HijackCode ++ SaveTrailCode ++ SaveRegionCommitFrameCode.

generate_semi_commit(SemiCommitInfo, Code, !CI, !CLD) :-
    SemiCommitInfo = semi_commit_info(FailInfo, ResumePoint,
        HijackInfo, MaybeTrailSlots, MaybeRegionCommitFrameInfo),

    set_fail_info(FailInfo, !CLD),
    % XXX Should release the temp slots in each arm of the switch.
    (
        HijackInfo = commit_temp_frame(MaxfrSlot, UseMinimalModel),
        MaxfrCode = singleton(
            llds_instr(restore_maxfr(MaxfrSlot),
                "restore maxfr for temp frame hijack")
        ),
        (
            UseMinimalModel = yes,
            % See the comment in prepare_for_semi_commit above.
            Components = [
                foreign_proc_raw_code(cannot_branch_away,
                    proc_does_not_affect_liveness, live_lvals_info(set.init),
                    "\t\tMR_commit_cut();\n")
            ],
            MD = proc_may_duplicate,
            CutCode = singleton(
                llds_instr(foreign_proc_code([], Components,
                    proc_will_not_call_mercury, no, no, no, no, no, no, MD),
                    "commit for temp frame hijack")
            )
        ;
            UseMinimalModel = no,
            CutCode = empty
        ),
        SuccessUndoCode = MaxfrCode ++ CutCode,
        FailureUndoCode = MaxfrCode ++ CutCode
    ;
        HijackInfo = commit_quarter_hijack,
        FailInfo = fail_info(ResumePoints, _, _, _, _),
        stack.det_top(ResumePoints, TopResumePoint),
        pick_stack_resume_point(TopResumePoint, _, StackLabel),
        StackLabelConst = const(llconst_code_addr(StackLabel)),
        SuccessUndoCode = from_list([
            llds_instr(assign(maxfr, lval(curfr)),
                "restore maxfr for quarter commit hijack"),
            llds_instr(assign(redoip_slot(lval(maxfr)), StackLabelConst),
                "restore redoip for quarter commit hijack")
        ]),
        FailureUndoCode = singleton(
            llds_instr(assign(redoip_slot(lval(maxfr)), StackLabelConst),
                "restore redoip for quarter commit hijack")
        )
    ;
        HijackInfo = commit_half_hijack(RedoipSlot),
        SuccessUndoCode = from_list([
            llds_instr(assign(maxfr, lval(curfr)),
                "restore maxfr for half commit hijack"),
            llds_instr(assign(redoip_slot(lval(maxfr)), lval(RedoipSlot)),
                "restore redoip for half commit hijack")
        ]),
        FailureUndoCode = singleton(
            llds_instr(assign(redoip_slot(lval(maxfr)), lval(RedoipSlot)),
                "restore redoip for half commit hijack")
        )
    ;
        HijackInfo = commit_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
        SuccessUndoCode = from_list([
            llds_instr(restore_maxfr(MaxfrSlot),
                "restore maxfr for full commit hijack"),
            llds_instr(assign(redoip_slot(lval(maxfr)), lval(RedoipSlot)),
                "restore redoip for full commit hijack"),
            llds_instr(assign(redofr_slot(lval(maxfr)), lval(RedofrSlot)),
                "restore redofr for full commit hijack")
        ]),
        FailureUndoCode = from_list([
            llds_instr(assign(redoip_slot(lval(maxfr)), lval(RedoipSlot)),
                "restore redoip for full commit hijack"),
            llds_instr(assign(redofr_slot(lval(maxfr)), lval(RedofrSlot)),
                "restore redofr for full commit hijack")
        ])
    ),

    remember_position(!.CLD, AfterCommit),
    generate_resume_point(ResumePoint, ResumePointCode, !CI, !CLD),
    generate_failure(FailCode, !CI, !.CLD),
    reset_to_position(AfterCommit, !.CI, !:CLD),

    maybe_restore_trail_info(MaybeTrailSlots, CommitTrailCode,
        RestoreTrailCode, !CI, !CLD),
    maybe_restore_region_commit_frame(MaybeRegionCommitFrameInfo,
        SuccessRegionCode, FailureRegionCode, !CI, !CLD),

    get_next_label(SuccLabel, !CI),
    GotoSuccLabel = singleton(
        llds_instr(goto(code_label(SuccLabel)), "Jump to success continuation")
    ),
    SuccLabelCode = singleton(
        llds_instr(label(SuccLabel), "Success continuation")
    ),
    SuccessCode = SuccessUndoCode ++ CommitTrailCode ++ SuccessRegionCode,
    FailureCode = ResumePointCode ++ FailureUndoCode ++ RestoreTrailCode ++
        FailureRegionCode ++ FailCode,
    Code = SuccessCode ++ GotoSuccLabel ++ FailureCode ++ SuccLabelCode.

%---------------------------------------------------------------------------%

:- pred maybe_save_region_commit_frame(add_region_ops::in, set_of_progvar::in,
    hlds_goal_info::in, maybe(region_commit_stack_frame)::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_save_region_commit_frame(AddRegionOps, _ForwardLiveVarsBeforeGoal,
        CommitGoalInfo, MaybeRegionCommitFrameInfo, Code, !CI, !CLD) :-
    (
        AddRegionOps = do_not_add_region_ops,
        MaybeRegionCommitFrameInfo = no,
        Code = empty
    ;
        AddRegionOps = add_region_ops,
        MaybeRbmmInfo = goal_info_get_maybe_rbmm(CommitGoalInfo),
        (
            MaybeRbmmInfo = no,
            MaybeRegionCommitFrameInfo = no,
            Code = empty
        ;
            MaybeRbmmInfo = yes(RbmmInfo),
            RbmmInfo = rbmm_goal_info(_, CommitRemovedRegionVars, _, _, _),

            RemovedRegionVarList = set.to_sorted_list(CommitRemovedRegionVars),

            NumRemovedRegionVars = list.length(RemovedRegionVarList),

            code_info.get_globals(!.CI, Globals),
            globals.lookup_int_option(Globals, size_region_commit_fixed,
                FixedSize),
            globals.lookup_int_option(Globals, size_region_commit_entry,
                EntrySize),
            FrameSize = FixedSize + EntrySize * NumRemovedRegionVars,
            Items = list.duplicate(FrameSize, slot_region_commit),
            acquire_several_temp_slots(Items, non_persistent_temp_slot,
                StackVars, MainStackId, FirstSlotNum, LastSlotNum, !CI, !CLD),
            EmbeddedStackFrame = embedded_stack_frame_id(MainStackId,
                FirstSlotNum, LastSlotNum),
            FirstSavedRegionAddr = first_nonfixed_embedded_slot_addr(
                EmbeddedStackFrame, FixedSize),
            acquire_reg(reg_r, NumRegLval, !CLD),
            acquire_reg(reg_r, AddrRegLval, !CLD),
            PushInitCode = from_list([
                llds_instr(
                    push_region_frame(region_stack_commit, EmbeddedStackFrame),
                    "Save stack pointer of embedded region commit stack"),
                llds_instr(
                    assign(NumRegLval, const(llconst_int(0))),
                    "Initialize number of unprotected live regions"),
                llds_instr(
                    assign(AddrRegLval, FirstSavedRegionAddr),
                    "Initialize pointer to the next unprotected live" ++
                    " region slot")
            ]),
            save_unprotected_live_regions(NumRegLval, AddrRegLval,
                EmbeddedStackFrame, RemovedRegionVarList, FillCode, !CLD),
            SetCode = singleton(
                llds_instr(
                    region_set_fixed_slot(region_set_commit_num_entries,
                        EmbeddedStackFrame, lval(NumRegLval)),
                    "Store the number of unprotected live regions")
            ),
            release_reg(NumRegLval, !CLD),
            release_reg(AddrRegLval, !CLD),

            RegionCommitFrameInfo =
                region_commit_stack_frame(EmbeddedStackFrame, StackVars),
            MaybeRegionCommitFrameInfo = yes(RegionCommitFrameInfo),

            Code = PushInitCode ++ FillCode ++ SetCode
        )
    ).

:- pred save_unprotected_live_regions(lval::in, lval::in,
    embedded_stack_frame_id::in, list(prog_var)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

save_unprotected_live_regions(_, _, _, [], cord.empty, !CLD).
save_unprotected_live_regions(NumLval, AddrLval, EmbeddedStackFrame,
        [RegionVar | RegionVars], Code ++ Codes, !CLD) :-
    produce_variable(RegionVar, ProduceVarCode, RegionVarRval, !CLD),
    SaveCode = singleton(
        llds_instr(
            region_fill_frame(region_fill_commit, EmbeddedStackFrame,
                RegionVarRval, NumLval, AddrLval),
            "Save the region in the commit stack frame if it is unprotected")
    ),
    Code = ProduceVarCode ++ SaveCode,
    save_unprotected_live_regions(NumLval, AddrLval, EmbeddedStackFrame,
        RegionVars, Codes, !CLD).

:- pred maybe_restore_region_commit_frame(maybe(region_commit_stack_frame)::in,
    llds_code::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_restore_region_commit_frame(MaybeRegionCommitFrameInfo,
        SuccessCode, FailureCode, !CI, !CLD) :-
    (
        MaybeRegionCommitFrameInfo = no,
        SuccessCode = empty,
        FailureCode = empty
    ;
        MaybeRegionCommitFrameInfo = yes(RegionCommitFrameInfo),
        RegionCommitFrameInfo = region_commit_stack_frame(EmbeddedStackFrame,
            StackVars),
        SuccessCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(region_commit_success,
                    EmbeddedStackFrame),
                "Destroy removed regions protected by cut away disjunctions")
        ),
        FailureCode = singleton(
            llds_instr(
                use_and_maybe_pop_region_frame(region_commit_failure,
                    EmbeddedStackFrame),
                "Undo the creation of the commit frame")
        ),
        release_several_temp_slots(StackVars, non_persistent_temp_slot,
            !CI, !CLD)
    ).

%---------------------------------------------------------------------------%

:- pred set_inside_non_condition(code_loc_dep::in, code_loc_dep::out) is det.

set_inside_non_condition(!CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, _, Allow),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
        inside_non_condition, Allow),
    set_fail_info(FailInfo, !CLD).

:- pred create_temp_frame(code_addr::in, string::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

create_temp_frame(Redoip, Comment, Code, !CI, !CLD) :-
    CodeModel = get_proc_model(!.CI),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        Kind = det_stack_proc
    ;
        CodeModel = model_non,
        Kind = nondet_stack_proc
    ),
    Code = singleton(
        llds_instr(mkframe(temp_frame(Kind), yes(Redoip)), Comment)
    ),
    set_created_temp_frame(yes, !CI),
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, ResumeKnown, _, CondEnv, Allow),
    FailInfo = fail_info(ResumePoints, ResumeKnown, may_be_different,
        CondEnv, Allow),
    set_fail_info(FailInfo, !CLD).

%---------------------------------------------------------------------------%

effect_resume_point(ResumePoint, CodeModel, Code, !CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, _ResumeKnown, CurfrMaxfr,
        CondEnv, Allow),
    ( if stack.top(ResumePoints0, OldResumePoint) then
        pick_first_resume_point(OldResumePoint, OldMap, _),
        pick_first_resume_point(ResumePoint, NewMap, _),
        map.keys_as_set(OldMap, OldKeys),
        map.keys_as_set(NewMap, NewKeys),
        expect(set.subset(OldKeys, NewKeys), $pred,
            "non-nested resume point variable sets")
    else
        true
    ),
    stack.push(ResumePoint, ResumePoints0, ResumePoints),
    (
        CodeModel = model_non,
        pick_stack_resume_point(ResumePoint, _, StackLabel),
        LabelConst = const(llconst_code_addr(StackLabel)),
        Code = singleton(
            llds_instr(assign(redoip_slot(lval(maxfr)), LabelConst),
                "hijack redoip to effect resume point")
        ),
        RedoipUpdate = has_been_done
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        Code = empty,
        RedoipUpdate = wont_be_done
    ),
    FailInfo = fail_info(ResumePoints, resume_point_known(RedoipUpdate),
        CurfrMaxfr, CondEnv, Allow),
    set_fail_info(FailInfo, !CLD).

pop_resume_point(!CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, ResumeKnown, CurfrMaxfr,
        CondEnv, Allow),
    stack.det_pop(_, ResumePoints0, ResumePoints),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
        CondEnv, Allow),
    set_fail_info(FailInfo, !CLD).

%---------------------------------------------------------------------------%

top_resume_point(CLD, ResumePoint) :-
    get_fail_info(CLD, FailInfo),
    FailInfo = fail_info(ResumePoints, _, _, _, _),
    stack.det_top(ResumePoints, ResumePoint).

set_resume_point_to_unknown(!CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, _, CurfrMaxfr, CondEnv, Allow),
    FailInfo = fail_info(ResumePoints, resume_point_unknown,
        CurfrMaxfr, CondEnv, Allow),
    set_fail_info(FailInfo, !CLD).

set_resume_point_and_frame_to_unknown(!CLD) :-
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, _, _, CondEnv, Allow),
    FailInfo = fail_info(ResumePoints, resume_point_unknown, may_be_different,
        CondEnv, Allow),
    set_fail_info(FailInfo, !CLD).

%---------------------------------------------------------------------------%

generate_failure(Code, !CI, !.CLD) :-
    get_fail_info(!.CLD, FailInfo),
    FailInfo = fail_info(ResumePoints, ResumeKnown, _, _, _),
    (
        ResumeKnown = resume_point_known(_),
        stack.det_top(ResumePoints, TopResumePoint),
        ( if
            pick_matching_resume_addr(!.CLD, TopResumePoint, FailureAddress0)
        then
            FailureAddress = FailureAddress0,
            PlaceCode = empty
        else
            pick_first_resume_point(TopResumePoint, Map, FailureAddress),
            map.to_assoc_list(Map, AssocList),
            pick_and_place_vars(AssocList, _, PlaceCode, !.CLD, _AfterPlaceCLD)
        ),
        BranchCode = singleton(llds_instr(goto(FailureAddress), "fail")),
        Code = PlaceCode ++ BranchCode
    ;
        ResumeKnown = resume_point_unknown,
        Code = singleton(llds_instr(goto(do_redo), "fail"))
    ),
    trace [compiletime(flag("codegen_goal")), io(!IO)] (
        ( if should_trace_code_gen(!.CI) then
            io.output_stream(Stream, !IO),
            io.write_string(Stream, "failure code\n", !IO),
            Instrs = cord.list(Code),
            write_instrs(Stream, Instrs, no, auto_comments, !IO),
            io.write_string(Stream, "end failure code\n", !IO),
            io.flush_output(Stream, !IO)
        else
            true
        )
    ).

fail_if_rval_is_false(Rval0, Code, !CI, !CLD) :-
    get_fail_info(!.CLD, FailInfo),
    FailInfo = fail_info(ResumePoints, ResumeKnown, _, _, _),
    (
        ResumeKnown = resume_point_known(_),
        stack.det_top(ResumePoints, TopResumePoint),
        ( if
            pick_matching_resume_addr(!.CLD, TopResumePoint, FailureAddress0)
        then
            % We branch away if the test *fails*
            code_util.neg_rval(Rval0, Rval),
            Code = singleton(
                llds_instr(if_val(Rval, FailureAddress0), "Test for failure")
            )
        else
            pick_first_resume_point(TopResumePoint, Map, FailureAddress),
            map.to_assoc_list(Map, AssocList),
            get_next_label(SuccessLabel, !CI),
            remember_position(!.CLD, CurPos),
            pick_and_place_vars(AssocList, _, PlaceCode,
                !.CLD, _AfterPlaceCLD),
            reset_to_position(CurPos, !.CI, !:CLD),
            SuccessAddress = code_label(SuccessLabel),
            % We branch away if the test *fails*, therefore if the test
            % succeeds, we branch around the code that moves variables to
            % their failure locations and branches away to the failure
            % continuation.
            TestCode = singleton(
                llds_instr(if_val(Rval0, SuccessAddress), "Test for failure")
            ),
            TailCode = from_list([
                llds_instr(goto(FailureAddress), "Goto failure"),
                llds_instr(label(SuccessLabel), "Success continuation")
            ]),
            Code = TestCode ++ PlaceCode ++ TailCode
        )
    ;
        ResumeKnown = resume_point_unknown,
        % We branch away if the test *fails*
        code_util.neg_rval(Rval0, Rval),
        Code = singleton(
            llds_instr(if_val(Rval, do_redo), "Test for failure")
        )
    ).

%---------------------------------------------------------------------------%

failure_is_direct_branch(CLD, CodeAddr) :-
    get_fail_info(CLD, FailInfo),
    FailInfo = fail_info(ResumePoints, resume_point_known(_), _, _, _),
    stack.top(ResumePoints, TopResumePoint),
    pick_matching_resume_addr(CLD, TopResumePoint, CodeAddr).

may_use_nondet_tailcall(CLD, TailCallStatus) :-
    get_fail_info(CLD, FailInfo),
    FailInfo = fail_info(ResumePoints0, ResumeKnown, _, _, _),
    ( if
        stack.pop(ResumePoint1, ResumePoints0, ResumePoints1),
        stack.is_empty(ResumePoints1),
        ResumePoint1 = stack_only(_, do_fail)
    then
        (
            ResumeKnown = resume_point_known(_),
            TailCallStatus = unchecked_tail_call
        ;
            ResumeKnown = resume_point_unknown,
            TailCallStatus = checked_tail_call
        )
    else
        TailCallStatus = no_tail_call
    ).

%---------------------------------------------------------------------------%

    % See whether the current locations of variables match the locations
    % associated with any of the options in the given failure map.
    % If yes, return the code_addr of that option.
    %
:- pred pick_matching_resume_addr(code_loc_dep::in,
    resume_point_info::in, code_addr::out) is semidet.

pick_matching_resume_addr(CLD, ResumeMaps, Addr) :-
    variable_locations(CLD, CurLocs),
    (
        ResumeMaps = orig_only(OrigMap, OrigAddr),
        ( if match_resume_loc(OrigMap, CurLocs) then
            Addr = OrigAddr
        else
            fail
        )
    ;
        ResumeMaps = stack_only(StackMap, StackAddr),
        ( if match_resume_loc(StackMap, CurLocs) then
            Addr = StackAddr
        else
            fail
        )
    ;
        ResumeMaps = orig_then_stack(OrigMap, OrigAddr, StackMap, StackAddr),
        ( if match_resume_loc(OrigMap, CurLocs) then
            Addr = OrigAddr
        else if match_resume_loc(StackMap, CurLocs) then
            Addr = StackAddr
        else
            fail
        )
    ;
        ResumeMaps = stack_then_orig(StackMap, StackAddr, OrigMap, OrigAddr),
        % XXX Why do we pick StackAddr even if we could pick OrigAddr?
        ( if match_resume_loc(StackMap, CurLocs) then
            Addr = StackAddr
        else if match_resume_loc(OrigMap, CurLocs) then
            Addr = OrigAddr
        else
            fail
        )
    ).

:- pred match_resume_loc(resume_map::in, resume_map::in) is semidet.

match_resume_loc(ResumeMap, ActualLocationMap) :-
    map.keys_as_set(ResumeMap, ResumeVars),
    map.select(ActualLocationMap, ResumeVars, ResumeVarActualLocationMap),
    map.to_assoc_list(ResumeVarActualLocationMap, ResumeVarActualLocations),
    all_vars_match_resume_map(ResumeMap, ResumeVarActualLocations).

:- pred all_vars_match_resume_map(resume_map::in,
    assoc_list(prog_var, set(lval))::in) is semidet.

all_vars_match_resume_map(_ResumeMap, []).
all_vars_match_resume_map(ResumeMap, [Var - ActualLvals | VarsActualLvals]) :-
    map.search(ResumeMap, Var, ResumeLvals),
    set.subset(ResumeLvals, ActualLvals),
    all_vars_match_resume_map(ResumeMap, VarsActualLvals).

:- pred pick_first_resume_point(resume_point_info::in,
    resume_map::out, code_addr::out) is det.

pick_first_resume_point(orig_only(Map, Addr), Map, Addr).
pick_first_resume_point(stack_only(Map, Addr), Map, Addr).
pick_first_resume_point(orig_then_stack(Map, Addr, _, _), Map, Addr).
pick_first_resume_point(stack_then_orig(Map, Addr, _, _), Map, Addr).

:- pred pick_stack_resume_point(resume_point_info::in,
    resume_map::out, code_addr::out) is det.

pick_stack_resume_point(ResumePoint, Map, Addr) :-
    ( if maybe_pick_stack_resume_point(ResumePoint, MapPrime, AddrPrime) then
        Map = MapPrime,
        Addr = AddrPrime
    else
        unexpected($pred, "no stack resume point")
    ).

:- pred maybe_pick_stack_resume_point(resume_point_info::in,
    resume_map::out, code_addr::out) is semidet.

maybe_pick_stack_resume_point(stack_only(Map, Addr), Map, Addr).
maybe_pick_stack_resume_point(orig_then_stack(_, _, Map, Addr), Map, Addr).
maybe_pick_stack_resume_point(stack_then_orig(Map, Addr, _, _), Map, Addr).

%---------------------------------------------------------------------------%

produce_vars([], Map, empty, !CLD) :-
    map.init(Map).
produce_vars([Var | Vars], Map, Code, !CLD) :-
    produce_vars(Vars, Map0, CodeVars, !CLD),
    produce_variable_in_reg_or_stack(Var, CodeVar, Lval, !CLD),
    Lvals = set.make_singleton_set(Lval),
    map.det_insert(Var, Lvals, Map0, Map),
    Code = CodeVars ++ CodeVar.

flush_resume_vars_to_stack(Code, !CLD) :-
    compute_resume_var_stack_locs(!.CLD, VarLocs),
    place_vars(VarLocs, Code, !CLD).

:- pred compute_resume_var_stack_locs(code_loc_dep::in,
    assoc_list(prog_var, lval)::out) is det.

compute_resume_var_stack_locs(CLD, VarLocs) :-
    get_fail_info(CLD, FailInfo),
    FailInfo = fail_info(ResumePointStack, _, _, _, _),
    stack.det_top(ResumePointStack, ResumePoint),
    pick_stack_resume_point(ResumePoint, StackMap, _),
    map.to_assoc_list(StackMap, VarLocSets),
    pick_var_places(VarLocSets, VarLocs).

%---------------------------------------------------------------------------%

:- pred init_fail_info(code_model::in, maybe(set_of_progvar)::in,
    resume_point_info::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

init_fail_info(CodeModel, MaybeFailVars, ResumePoint, !CI, !CLD) :-
    (
        CodeModel = model_det,
        get_next_label(ResumeLabel, !CI),
        ResumeAddress = code_label(ResumeLabel),
        ResumeKnown = resume_point_unknown,
        CurfrMaxfr = may_be_different
    ;
        CodeModel = model_semi,
        % The resume point for this label will be part of the procedure epilog.
        get_next_label(ResumeLabel, !CI),
        ResumeAddress = code_label(ResumeLabel),
        ResumeKnown = resume_point_known(wont_be_done),
        CurfrMaxfr = may_be_different
    ;
        CodeModel = model_non,
        (
            MaybeFailVars = yes(_),
            get_next_label(ResumeLabel, !CI),
            ResumeAddress = code_label(ResumeLabel)
        ;
            MaybeFailVars = no,
            ResumeAddress = do_fail
        ),
        ResumeKnown = resume_point_known(has_been_done),
        CurfrMaxfr = must_be_equal
    ),
    (
        MaybeFailVars = yes(FailVars),
        get_stack_slots(!.CI, StackSlots),
        map.select_sorted_list(StackSlots, set_of_var.to_sorted_list(FailVars),
            AbsStackMap),
        map.to_assoc_list(AbsStackMap, AbsStackList),
        StackList0 = assoc_list.map_values_only(stack_slot_to_lval,
            AbsStackList),
        make_singleton_sets(StackList0, StackList),
        map.from_sorted_assoc_list(StackList, StackMap)
    ;
        MaybeFailVars = no,
        map.init(StackMap)
    ),
    ResumePoint = stack_only(StackMap, ResumeAddress),
    stack.init(ResumeStack0),
    stack.push(ResumePoint, ResumeStack0, ResumeStack),
    get_fail_info(!.CLD, FailInfo0),
    FailInfo0 = fail_info(_, _, _, _, Allow),
    FailInfo = fail_info(ResumeStack, ResumeKnown, CurfrMaxfr,
        not_inside_non_condition, Allow),
    set_fail_info(FailInfo, !CLD).

%---------------------------------------------------------------------------%

make_resume_point(ResumeVars, ResumeLocs, FullMap, ResumePoint, !CI) :-
    get_stack_slots(!.CI, StackSlots),
    map.select(FullMap, set_of_var.bitset_to_set(ResumeVars), OrigMap),
    (
        ResumeLocs = resume_locs_orig_only,
        get_next_label(OrigLabel, !CI),
        OrigAddr = code_label(OrigLabel),
        ResumePoint = orig_only(OrigMap, OrigAddr),
        trace [compiletime(flag("codegen_goal")), io(!IO)] (
            ( if should_trace_code_gen(!.CI) then
                io.output_stream(Stream, !IO),
                code_info.get_var_table(!.CI, VarTable),
                io.write_string(Stream, "make_resume_point orig_only\n", !IO),
                output_resume_map(Stream, VarTable, "orig:",
                    OrigMap, OrigLabel, !IO),
                io.flush_output(Stream, !IO)
            else
                true
            )
        )
    ;
        ResumeLocs = resume_locs_stack_only,
        make_stack_resume_map(ResumeVars, StackSlots, StackMap),
        get_next_label(StackLabel, !CI),
        StackAddr = code_label(StackLabel),
        ResumePoint = stack_only(StackMap, StackAddr),
        trace [compiletime(flag("codegen_goal")), io(!IO)] (
            ( if should_trace_code_gen(!.CI) then
                io.output_stream(Stream, !IO),
                code_info.get_var_table(!.CI, VarTable),
                io.write_string(Stream,
                    "make_resume_point stack_only\n", !IO),
                output_resume_map(Stream, VarTable, "stack:",
                    StackMap, StackLabel, !IO),
                io.flush_output(Stream, !IO)
            else
                true
            )
        )
    ;
        ResumeLocs = resume_locs_orig_then_stack,
        make_stack_resume_map(ResumeVars, StackSlots, StackMap),
        get_next_label(OrigLabel, !CI),
        OrigAddr = code_label(OrigLabel),
        get_next_label(StackLabel, !CI),
        StackAddr = code_label(StackLabel),
        ResumePoint = orig_then_stack(OrigMap, OrigAddr, StackMap, StackAddr),
        trace [compiletime(flag("codegen_goal")), io(!IO)] (
            ( if should_trace_code_gen(!.CI) then
                io.output_stream(Stream, !IO),
                code_info.get_var_table(!.CI, VarTable),
                io.write_string(Stream,
                    "make_resume_point orig_then_stack\n", !IO),
                output_resume_map(Stream, VarTable, "orig:",
                    OrigMap, OrigLabel, !IO),
                output_resume_map(Stream, VarTable, "stack:",
                    StackMap, StackLabel, !IO),
                io.flush_output(Stream, !IO)
            else
                true
            )
        )
    ;
        ResumeLocs = resume_locs_stack_then_orig,
        make_stack_resume_map(ResumeVars, StackSlots, StackMap),
        get_next_label(StackLabel, !CI),
        StackAddr = code_label(StackLabel),
        get_next_label(OrigLabel, !CI),
        OrigAddr = code_label(OrigLabel),
        ResumePoint = stack_then_orig(StackMap, StackAddr, OrigMap, OrigAddr),
        trace [compiletime(flag("codegen_goal")), io(!IO)] (
            ( if should_trace_code_gen(!.CI) then
                code_info.get_var_table(!.CI, VarTable),
                io.output_stream(Stream, !IO),
                io.write_string(Stream,
                    "make_resume_point stack_then_orig\n", !IO),
                output_resume_map(Stream, VarTable, "stack:",
                    StackMap, StackLabel, !IO),
                output_resume_map(Stream, VarTable, "orig:",
                    OrigMap, OrigLabel, !IO),
                io.flush_output(Stream, !IO)
            else
                true
            )
        )
    ).

:- pred make_stack_resume_map(set_of_progvar::in, stack_slots::in,
    map(prog_var, set(lval))::out) is det.

make_stack_resume_map(ResumeVars, StackSlots, StackMap) :-
    map.select(StackSlots, set_of_var.bitset_to_set(ResumeVars), StackMap0),
    map.to_assoc_list(StackMap0, AbsStackList),
    StackList0 = assoc_list.map_values_only(stack_slot_to_lval, AbsStackList),
    make_singleton_sets(StackList0, StackList),
    map.from_sorted_assoc_list(StackList, StackMap).

:- pred make_singleton_sets(assoc_list(prog_var, lval)::in,
    assoc_list(prog_var, set(lval))::out) is det.

make_singleton_sets([], []).
make_singleton_sets([Var - Lval | Tail], [Var - Lvals | SetTail]) :-
    Lvals = set.make_singleton_set(Lval),
    make_singleton_sets(Tail, SetTail).

%---------------------------------------------------------------------------%

:- pred clone_resume_point(resume_point_info::in,
    resume_point_info::out, code_info::in, code_info::out) is det.

clone_resume_point(ResumePoint0, ResumePoint, !CI) :-
    (
        ResumePoint0 = orig_only(_, _),
        unexpected($pred, "cloning orig_only resume point")
    ;
        ResumePoint0 = stack_only(Map1, _),
        get_next_label(Label1, !CI),
        Addr1 = code_label(Label1),
        ResumePoint = stack_only(Map1, Addr1)
    ;
        ResumePoint0 = stack_then_orig(Map1, _, Map2, _),
        get_next_label(Label1, !CI),
        Addr1 = code_label(Label1),
        get_next_label(Label2, !CI),
        Addr2 = code_label(Label2),
        ResumePoint = stack_then_orig(Map1, Addr1, Map2, Addr2)
    ;
        ResumePoint0 = orig_then_stack(Map1, _, Map2, _),
        get_next_label(Label2, !CI),
        Addr2 = code_label(Label2),
        get_next_label(Label1, !CI),
        Addr1 = code_label(Label1),
        ResumePoint = stack_then_orig(Map2, Addr2, Map1, Addr1)
    ).

%---------------------------------------------------------------------------%

generate_resume_point(ResumePoint, Code, !CI, !CLD) :-
    % The code we generate for a resumption point looks like this:
    %
    %   label(StackLabel)
    %       <assume variables are where StackMap says they are>
    %       <copy variables to their locations according to OrigMap>
    %   label(OrigLabel)
    %       <assume variables are where OrigMap says they are>
    %
    % Failures at different points may cause control to arrive at
    % the resumption point via either label, which is why the last
    % line is necessary.
    %
    % The idea is that failures from other procedures will go to
    % StackLabel, and that failures from this procedure while
    % everything is in its original place will go to OrigLabel.
    % Failures from this procedure where not everything is in its
    % original place can go to either, after moving the resume variables
    % to the places where the label expects them.
    %
    % The above layout (stack, then orig) is the most common. However,
    % liveness.m may decide that one or other of the two labels will
    % never be referred to (e.g. because there are no calls inside
    % the range of effect of the resumption point or because a call
    % follows immediately after the establishment of the resumption
    % point), or that it would be more efficient to put the two labels
    % in the other order (e.g. because the code after the resumption point
    % needs most of the variables in their stack slots).

    (
        ResumePoint = orig_only(Map1, Addr1),
        extract_label_from_code_addr(Addr1, Label1),
        Code = singleton(
            llds_instr(label(Label1), "orig only failure continuation")
        ),
        set_var_locations(Map1, !CLD)
    ;
        ResumePoint = stack_only(Map1, Addr1),
        extract_label_from_code_addr(Addr1, Label1),
        Code = singleton(
            llds_instr(label(Label1), "stack only failure continuation")
        ),
        set_var_locations(Map1, !CLD),
        maybe_generate_resume_layout(Label1, Map1, !CI, !.CLD)
    ;
        ResumePoint = stack_then_orig(Map1, Addr1, Map2, Addr2),
        extract_label_from_code_addr(Addr1, Label1),
        extract_label_from_code_addr(Addr2, Label2),
        Label1Code = singleton(
            llds_instr(label(Label1), "stack failure continuation before orig")
        ),
        set_var_locations(Map1, !CLD),
        maybe_generate_resume_layout(Label1, Map1, !CI, !.CLD),
        map.to_assoc_list(Map2, AssocList2),
        place_resume_vars(AssocList2, PlaceCode, !CLD),
        Label2Code = singleton(
            llds_instr(label(Label2), "orig failure continuation after stack")
        ),
        set_var_locations(Map2, !CLD),
        Code = Label1Code ++ PlaceCode ++ Label2Code
    ;
        ResumePoint = orig_then_stack(Map1, Addr1, Map2, Addr2),
        extract_label_from_code_addr(Addr1, Label1),
        extract_label_from_code_addr(Addr2, Label2),
        Label1Code = singleton(
            llds_instr(label(Label1), "orig failure continuation before stack")
        ),
        set_var_locations(Map1, !CLD),
        map.to_assoc_list(Map2, AssocList2),
        place_resume_vars(AssocList2, PlaceCode, !CLD),
        Label2Code = singleton(
            llds_instr(label(Label2), "stack failure continuation after orig")
        ),
        set_var_locations(Map2, !CLD),
        maybe_generate_resume_layout(Label2, Map2, !CI, !.CLD),
        Code = Label1Code ++ PlaceCode ++ Label2Code
    ).

:- pred extract_label_from_code_addr(code_addr::in, label::out) is det.

extract_label_from_code_addr(CodeAddr, Label) :-
    ( if CodeAddr = code_label(Label0) then
        Label = Label0
    else
        unexpected($pred, "non-label")
    ).

:- pred place_resume_vars(assoc_list(prog_var, set(lval))::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

place_resume_vars([], cord.empty, !CLD).
place_resume_vars([Var - TargetSet | Rest], Code, !CLD) :-
    set.to_sorted_list(TargetSet, Targets),
    place_resume_var(Var, Targets, FirstCode, !CLD),
    place_resume_vars(Rest, RestCode, !CLD),
    Code = FirstCode ++ RestCode.

:- pred place_resume_var(prog_var::in, list(lval)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

place_resume_var(_Var, [], cord.empty, !CLD).
place_resume_var(Var, [Target | Targets], Code, !CLD) :-
    place_var(Var, Target, FirstCode, !CLD),
    place_resume_var(Var, Targets, RestCode, !CLD),
    Code = FirstCode ++ RestCode.

    % Reset the code generator's database of what is where.
    % Remember that the variables in the map are available in their
    % associated rvals; forget about all other variables.
    %
:- pred set_var_locations(resume_map::in,
    code_loc_dep::in, code_loc_dep::out) is det.

set_var_locations(Map, !CLD) :-
    map.to_assoc_list(Map, LvalList0),
    flatten_varlval_list(LvalList0, LvalList),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    reinit_var_locn_state(LvalList, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

:- pred flatten_varlval_list(assoc_list(prog_var, set(lval))::in,
    assoc_list(prog_var, lval)::out) is det.

flatten_varlval_list([], []).
flatten_varlval_list([V - Rvals | Rest0], All) :-
    flatten_varlval_list(Rest0, Rest),
    set.to_sorted_list(Rvals, RvalList),
    flatten_varlval_list_2(RvalList, V, Rest1),
    list.append(Rest1, Rest, All).

:- pred flatten_varlval_list_2(list(lval)::in, prog_var::in,
    assoc_list(prog_var, lval)::out) is det.

flatten_varlval_list_2([], _V, []).
flatten_varlval_list_2([R | Rs], V, [V - R | Rest]) :-
    flatten_varlval_list_2(Rs, V, Rest).

resume_point_vars(ResumePoint, Vars) :-
    pick_first_resume_point(ResumePoint, ResumeMap, _),
    map.keys(ResumeMap, Vars).

resume_point_stack_addr(ResumePoint, StackAddr) :-
    pick_stack_resume_point(ResumePoint, _, StackAddr).

%---------------------------------------------------------------------------%

:- pred maybe_save_trail_info(add_trail_ops::in, maybe(pair(lval))::out,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_save_trail_info(AddTrailOps, MaybeTrailSlots, SaveTrailCode,
        !CI, !CLD) :-
    (
        AddTrailOps = add_trail_ops,
        acquire_temp_slot(slot_ticket_counter, non_persistent_temp_slot,
            CounterSlot, !CI, !CLD),
        acquire_temp_slot(slot_ticket, non_persistent_temp_slot,
            TrailPtrSlot, !CI, !CLD),
        MaybeTrailSlots = yes(CounterSlot - TrailPtrSlot),
        SaveTrailCode = from_list([
            llds_instr(mark_ticket_stack(CounterSlot),
                "save the ticket counter"),
            llds_instr(store_ticket(TrailPtrSlot),
                "save the trail pointer")
        ])
    ;
        AddTrailOps = do_not_add_trail_ops,
        MaybeTrailSlots = no,
        SaveTrailCode = empty
    ).

:- pred maybe_restore_trail_info(maybe(pair(lval))::in,
    llds_code::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

maybe_restore_trail_info(MaybeTrailSlots, CommitCode, RestoreCode,
        !CI, !CLD) :-
    (
        MaybeTrailSlots = no,
        CommitCode = empty,
        RestoreCode = empty
    ;
        MaybeTrailSlots = yes(CounterSlot - TrailPtrSlot),
        CommitCode = from_list([
            llds_instr(reset_ticket(lval(TrailPtrSlot), reset_reason_commit),
                "discard trail entries and restore trail ptr"),
            llds_instr(prune_tickets_to(lval(CounterSlot)),
                "restore ticket counter (but not high water mark)")
        ]),
        RestoreCode = from_list([
            llds_instr(reset_ticket(lval(TrailPtrSlot), reset_reason_undo),
                "apply trail entries and restore trail ptr"),
            llds_instr(discard_ticket,
                "restore ticket counter and high water mark")
        ]),
        release_temp_slot(CounterSlot, non_persistent_temp_slot, !CI, !CLD),
        release_temp_slot(TrailPtrSlot, non_persistent_temp_slot, !CI, !CLD)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule to deal with liveness issues.

    % The principles underlying this submodule of code_info.m are
    % documented in the file compiler/notes/allocation.html.

:- interface.

:- pred get_known_variables(code_loc_dep::in, list(prog_var)::out) is det.

:- pred variable_is_forward_live(code_loc_dep::in, prog_var::in) is semidet.

:- pred add_forward_live_vars(set_of_progvar::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred make_vars_forward_dead(set_of_progvar::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_make_vars_forward_dead(set_of_progvar::in, bool::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred pickup_zombies(set_of_progvar::out,
    code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred rem_forward_live_vars(set_of_progvar::in,
    code_loc_dep::in, code_loc_dep::out) is det.

get_known_variables(CLD, VarList) :-
    get_forward_live_vars(CLD, ForwardLiveVars),
    ResumeVars = current_resume_point_vars(CLD),
    set_of_var.union(ForwardLiveVars, ResumeVars, Vars),
    VarList = set_of_var.to_sorted_list(Vars).

variable_is_forward_live(CLD, Var) :-
    get_forward_live_vars(CLD, Liveness),
    set_of_var.member(Liveness, Var).

add_forward_live_vars(Births, !CLD) :-
    get_forward_live_vars(!.CLD, Liveness0),
    set_of_var.union(Liveness0, Births, Liveness),
    set_forward_live_vars(Liveness, !CLD).

rem_forward_live_vars(Deaths, !CLD) :-
    get_forward_live_vars(!.CLD, Liveness0),
    set_of_var.difference(Liveness0, Deaths, Liveness),
    set_forward_live_vars(Liveness, !CLD).

    % Make these variables appear magically live.
    % We don't care where they are put.
    %
:- pred make_vars_forward_live(set_of_progvar::in,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

make_vars_forward_live(Vars, CI, !CLD) :-
    get_stack_slots(CI, StackSlots),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    VarList = set_of_var.to_sorted_list(Vars),
    do_make_vars_forward_live(VarList, StackSlots, 1,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

:- pred do_make_vars_forward_live(list(prog_var)::in,
    stack_slots::in, int::in, var_locn_info::in, var_locn_info::out) is det.

do_make_vars_forward_live([], _, _, !VarLocnInfo).
do_make_vars_forward_live([Var | Vars], StackSlots, N0, !VarLocnInfo) :-
    do_make_var_forward_live(Var, StackSlots, N0, N1, !VarLocnInfo),
    do_make_vars_forward_live(Vars, StackSlots, N1, !VarLocnInfo).

:- pred do_make_var_forward_live(prog_var::in, stack_slots::in,
    int::in, int::out, var_locn_info::in, var_locn_info::out) is det.

do_make_var_forward_live(Var, StackSlots, N0, N1, !VarLocnInfo) :-
    ( if map.search(StackSlots, Var, Slot) then
        Lval = stack_slot_to_lval(Slot),
        N1 = N0
    else
        % reg_r is fine since we don't care where the variables are put.
        RegType = reg_r,
        find_unused_reg(!.VarLocnInfo, RegType, N0, N1),
        Lval = reg(RegType, N1)
    ),
    var_locn_set_magic_var_location(Var, Lval, !VarLocnInfo).

:- pred find_unused_reg(var_locn_info::in, reg_type::in, int::in, int::out)
    is det.

find_unused_reg(VLI, RegType, N0, N) :-
    ( if var_locn_lval_in_use(VLI, reg(RegType, N0)) then
        find_unused_reg(VLI, RegType, N0 + 1, N)
    else
        N = N0
    ).

make_vars_forward_dead(Vars, !CLD) :-
    maybe_make_vars_forward_dead(Vars, yes, !CLD).

maybe_make_vars_forward_dead(Vars0, FirstTime, !CLD) :-
    ResumeVars = current_resume_point_vars(!.CLD),
    set_of_var.intersect(Vars0, ResumeVars, FlushVars),
    get_zombies(!.CLD, Zombies0),
    set_of_var.union(Zombies0, FlushVars, Zombies),
    set_zombies(Zombies, !CLD),
    set_of_var.difference(Vars0, Zombies, Vars),
    VarList = set_of_var.to_sorted_list(Vars),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    maybe_make_vars_forward_dead_2(VarList, FirstTime,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

:- pred maybe_make_vars_forward_dead_2(list(prog_var)::in, bool::in,
    var_locn_info::in, var_locn_info::out) is det.

maybe_make_vars_forward_dead_2([], _, !VLI).
maybe_make_vars_forward_dead_2([Var | Vars], FirstTime, !VLI) :-
    var_locn_var_becomes_dead(Var, FirstTime, !VLI),
    maybe_make_vars_forward_dead_2(Vars, FirstTime, !VLI).

pickup_zombies(Zombies, !CLD) :-
    get_zombies(!.CLD, Zombies),
    set_zombies(set_of_var.init, !CLD).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for handling the saving and restoration
    % of trail tickets, heap pointers, stack pointers etc.

:- interface.

:- pred save_hp(llds_code::out, lval::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred restore_hp(lval::in, llds_code::out) is det.

:- pred release_hp(lval::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred restore_and_release_hp(lval::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred ite_maybe_save_hp(option::in, hlds_goal::in,
    llds_code::out, maybe(lval)::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_save_hp(bool::in, llds_code::out, maybe(lval)::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_restore_hp(maybe(lval)::in, llds_code::out) is det.

:- pred maybe_release_hp(maybe(lval)::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_restore_and_release_hp(maybe(lval)::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred save_ticket(llds_code::out, lval::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred reset_ticket(lval::in, reset_trail_reason::in, llds_code::out) is det.

:- pred release_ticket(lval::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred reset_and_prune_ticket(lval::in, reset_trail_reason::in,
    llds_code::out) is det.

:- pred reset_prune_and_release_ticket(lval::in, reset_trail_reason::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred reset_and_discard_ticket(lval::in, reset_trail_reason::in,
    llds_code::out) is det.

:- pred reset_discard_and_release_ticket(lval::in, reset_trail_reason::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred discard_and_release_ticket(lval::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_save_ticket(add_trail_ops::in, llds_code::out, maybe(lval)::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_reset_ticket(maybe(lval)::in, reset_trail_reason::in,
    llds_code::out) is det.

:- pred maybe_release_ticket(maybe(lval)::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_reset_and_prune_ticket(maybe(lval)::in,
    reset_trail_reason::in, llds_code::out) is det.

:- pred maybe_reset_prune_and_release_ticket(maybe(lval)::in,
    reset_trail_reason::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_reset_and_discard_ticket(maybe(lval)::in,
    reset_trail_reason::in, llds_code::out) is det.

:- pred maybe_reset_discard_and_release_ticket(maybe(lval)::in,
    reset_trail_reason::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred maybe_discard_and_release_ticket(maybe(lval)::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

save_hp(Code, HpSlot, !CI, !CLD) :-
    acquire_temp_slot(slot_lval(hp), non_persistent_temp_slot, HpSlot,
        !CI, !CLD),
    Code = singleton(
        llds_instr(mark_hp(HpSlot), "Save heap pointer")
    ).

restore_hp(HpSlot, Code) :-
    Code = singleton(
        llds_instr(restore_hp(lval(HpSlot)), "Restore heap pointer")
    ).

release_hp(HpSlot, !CI, !CLD) :-
    release_temp_slot(HpSlot, non_persistent_temp_slot, !CI, !CLD).

restore_and_release_hp(HpSlot, Code, !CI, !CLD) :-
    restore_hp(HpSlot, Code),
    release_hp(HpSlot, !CI, !CLD).

%---------------------------------------------------------------------------%

ite_maybe_save_hp(ReclaimOption, CondGoal, SaveHpCode, MaybeHpSlot,
        !CI, !CLD) :-
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, ReclaimOption, ReclaimOptionValue),
    (
        ReclaimOptionValue = no,
        SaveHpCode = cord.empty,
        MaybeHpSlot = no
    ;
        ReclaimOptionValue = yes,
        ( if goal_may_allocate_heap(CondGoal) then
            save_hp(SaveHpCode, HpSlot, !CI, !CLD),
            MaybeHpSlot = yes(HpSlot)
        else
            SaveHpCode = cord.empty,
            MaybeHpSlot = no
        )
    ).

maybe_save_hp(Maybe, Code, MaybeHpSlot, !CI, !CLD) :-
    (
        Maybe = yes,
        save_hp(Code, HpSlot, !CI, !CLD),
        MaybeHpSlot = yes(HpSlot)
    ;
        Maybe = no,
        Code = empty,
        MaybeHpSlot = no
    ).

maybe_restore_hp(MaybeHpSlot, Code) :-
    (
        MaybeHpSlot = yes(HpSlot),
        restore_hp(HpSlot, Code)
    ;
        MaybeHpSlot = no,
        Code = empty
    ).

maybe_release_hp(MaybeHpSlot, !CI, !CLD) :-
    (
        MaybeHpSlot = yes(HpSlot),
        release_hp(HpSlot, !CI, !CLD)
    ;
        MaybeHpSlot = no
    ).

maybe_restore_and_release_hp(MaybeHpSlot, Code, !CI, !CLD) :-
    (
        MaybeHpSlot = yes(HpSlot),
        restore_and_release_hp(HpSlot, Code, !CI, !CLD)
    ;
        MaybeHpSlot = no,
        Code = empty
    ).

%---------------------------------------------------------------------------%

save_ticket(Code, TicketSlot, !CI, !CLD) :-
    acquire_temp_slot(slot_ticket, non_persistent_temp_slot, TicketSlot,
        !CI, !CLD),
    Code = singleton(
        llds_instr(store_ticket(TicketSlot), "Save trail state")
    ).

reset_ticket(TicketSlot, Reason, Code) :-
    Code = singleton(
        llds_instr(reset_ticket(lval(TicketSlot), Reason), "Reset trail")
    ).

release_ticket(TicketSlot, !CI, !CLD) :-
    release_temp_slot(TicketSlot, non_persistent_temp_slot, !CI, !CLD).

reset_and_prune_ticket(TicketSlot, Reason, Code) :-
    Code = from_list([
        llds_instr(reset_ticket(lval(TicketSlot), Reason), "Restore trail"),
        llds_instr(prune_ticket, "Prune ticket stack")
    ]).

reset_prune_and_release_ticket(TicketSlot, Reason, Code, !CI, !CLD) :-
    Code = from_list([
        llds_instr(reset_ticket(lval(TicketSlot), Reason), "Release trail"),
        llds_instr(prune_ticket, "Prune ticket stack")
    ]),
    release_temp_slot(TicketSlot, non_persistent_temp_slot, !CI, !CLD).

reset_and_discard_ticket(TicketSlot, Reason, Code) :-
    Code = from_list([
        llds_instr(reset_ticket(lval(TicketSlot), Reason), "Restore trail"),
        llds_instr(discard_ticket, "Pop ticket stack")
    ]).

reset_discard_and_release_ticket(TicketSlot, Reason, Code, !CI, !CLD) :-
    Code = from_list([
        llds_instr(reset_ticket(lval(TicketSlot), Reason), "Release trail"),
        llds_instr(discard_ticket, "Pop ticket stack")
    ]),
    release_temp_slot(TicketSlot, non_persistent_temp_slot, !CI, !CLD).

discard_and_release_ticket(TicketSlot, Code, !CI, !CLD) :-
    Code = singleton(
        llds_instr(discard_ticket, "Pop ticket stack")
    ),
    release_temp_slot(TicketSlot, non_persistent_temp_slot, !CI, !CLD).

%---------------------------------------------------------------------------%

maybe_save_ticket(AddTrailOps, Code, MaybeTicketSlot, !CI, !CLD) :-
    (
        AddTrailOps = add_trail_ops,
        save_ticket(Code, TicketSlot, !CI, !CLD),
        MaybeTicketSlot = yes(TicketSlot)
    ;
        AddTrailOps = do_not_add_trail_ops,
        Code = empty,
        MaybeTicketSlot = no
    ).

maybe_reset_ticket(MaybeTicketSlot, Reason, Code) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_ticket(TicketSlot, Reason, Code)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_release_ticket(MaybeTicketSlot, !CI, !CLD) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        release_ticket(TicketSlot, !CI, !CLD)
    ;
        MaybeTicketSlot = no
    ).

maybe_reset_and_prune_ticket(MaybeTicketSlot, Reason, Code) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_and_prune_ticket(TicketSlot, Reason, Code)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_reset_prune_and_release_ticket(MaybeTicketSlot, Reason, Code,
        !CI, !CLD) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_prune_and_release_ticket(TicketSlot, Reason, Code, !CI, !CLD)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_reset_and_discard_ticket(MaybeTicketSlot, Reason, Code) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_and_discard_ticket(TicketSlot, Reason, Code)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_reset_discard_and_release_ticket(MaybeTicketSlot, Reason, Code,
        !CI, !CLD) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_discard_and_release_ticket(TicketSlot, Reason, Code, !CI, !CLD)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_discard_and_release_ticket(MaybeTicketSlot, Code, !CI, !CLD) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        discard_and_release_ticket(TicketSlot, Code, !CI, !CLD)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule to deal with var_locn.

    % Most of these procedures just forward to the var_locn module.
    % See var_locn for documentation.

:- interface.

:- pred variable_locations(code_loc_dep::in,
    map(prog_var, set(lval))::out) is det.

:- pred set_var_location(prog_var::in, lval::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred assign_var_to_var(prog_var::in, prog_var::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred assign_lval_to_var(prog_var::in, lval::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

:- pred assign_const_to_var(prog_var::in, rval::in,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

:- pred assign_expr_to_var(prog_var::in, rval::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred reassign_mkword_hole_var(prog_var::in, ptag::in, rval::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred reassign_tagword_var(prog_var::in, uint::in, rval::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred assign_field_lval_expr_to_var(prog_var::in, list(lval)::in, rval::in,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

    % assign_cell_to_var(Var, ReserveWordAtStart, Ptag, MaybeRvals,
    %   AllFilled, MaybeSize, FieldAddrs, TypeMsg, MayUseAtomic, Where,
    %   Code, !CLD).
    %
:- pred assign_cell_to_var(prog_var::in, bool::in, ptag::in,
    list(cell_arg)::in, how_to_construct::in, maybe(term_size_value)::in,
    maybe(alloc_site_id)::in, may_use_atomic_alloc::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred save_reused_cell_fields(prog_var::in, lval::in,
    llds_code::out, list(lval)::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred place_var(prog_var::in, lval::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred produce_variable(prog_var::in, llds_code::out, rval::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred produce_variable_in_reg(prog_var::in, llds_code::out, lval::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred produce_variable_in_reg_or_stack(prog_var::in,
    llds_code::out, lval::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred materialize_vars_in_lval(lval::in, lval::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred materialize_vars_in_rval(rval::in, rval::out,
    llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred acquire_reg_for_var(prog_var::in, reg_type::in, lval::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred acquire_reg_not_in_storemap(abs_store_map::in, reg_type::in, lval::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred acquire_reg(reg_type::in, lval::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred release_reg(lval::in, code_loc_dep::in, code_loc_dep::out) is det.

:- pred reserve_r1(llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred clear_r1(llds_code::out, code_loc_dep::in, code_loc_dep::out) is det.

:- type call_direction
    --->    caller
    ;       callee.

    % Move variables to where they need to be at the time of the call:
    %
    % - The variables that need to be saved across the call (either because
    %   they are forward live after the call or because they are protected
    %   by an enclosing resumption point) will be saved on the stack.
    %   Note that if the call cannot succeed and the trace level is none,
    %   then no variables need to be saved across the call. (If the call
    %   cannot succeed but the trace level is not none, then we still
    %   save the usual variables on the stack to make them available
    %   for up-level printing in the debugger.)
    %
    % - The input arguments will be moved to their registers.
    %
:- pred setup_call(hlds_goal_info::in, assoc_list(prog_var, arg_info)::in,
    set(lval)::out, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

    % Move the output arguments of the current procedure to where
    % they need to be at return.
    %
:- pred setup_return(assoc_list(prog_var, arg_info)::in,
    set(lval)::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred lock_regs(int::in, int::in, assoc_list(prog_var, lval)::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred unlock_regs(code_loc_dep::in, code_loc_dep::out) is det.

    % Record the fact that all the registers have been clobbered (as by a
    % call). If the bool argument is true, then the call cannot return, and
    % thus it is OK for this action to delete the last record of the state
    % of a variable.
    %
:- pred clear_all_registers(bool::in, code_loc_dep::in, code_loc_dep::out)
    is det.

:- pred clobber_reg(lval::in, code_loc_dep::in, code_loc_dep::out) is det.

:- pred clobber_regs(list(lval)::in,
    code_loc_dep::in, code_loc_dep::out) is det.

:- pred save_variables(set_of_progvar::in, set(lval)::out, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

:- pred save_variables_on_stack(list(prog_var)::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

:- pred max_reg_in_use(code_loc_dep::in, int::out, int::out) is det.

:- pred magically_put_var_in_unused_reg(prog_var::in,
    code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

variable_locations(CLD, Lvals) :-
    get_var_locn_info(CLD, VarLocnInfo),
    var_locn_get_var_locations(VarLocnInfo, Lvals).

set_var_location(Var, Lval, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_check_and_set_magic_var_location(Var, Lval,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

assign_var_to_var(Var, AssignedVar, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_assign_var_to_var(Var, AssignedVar, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

assign_lval_to_var(Var, Lval, Code, CI, !CLD) :-
    get_static_cell_info(CI, StaticCellInfo),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_assign_lval_to_var(Var, Lval, StaticCellInfo, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

assign_const_to_var(Var, ConstRval, CI, !CLD) :-
    get_exprn_opts(CI, ExprnOpts),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_assign_const_to_var(ExprnOpts, Var, ConstRval,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

assign_expr_to_var(Var, Rval, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    Lvals = lvals_in_rval(Rval),
    (
        Lvals = [],
        var_locn_assign_expr_to_var(Var, Rval, Code,
            VarLocnInfo0, VarLocnInfo)
    ;
        Lvals = [_ | _],
        unexpected($pred, "non-var lvals")
    ),
    set_var_locn_info(VarLocnInfo, !CLD).

reassign_mkword_hole_var(Var, Ptag, Rval, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    Lvals = lvals_in_rval(Rval),
    (
        Lvals = [],
        var_locn_reassign_mkword_hole_var(Var, Ptag, Rval, Code,
            VarLocnInfo0, VarLocnInfo)
    ;
        Lvals = [_ | _],
        unexpected($pred, "non-var lvals")
    ),
    set_var_locn_info(VarLocnInfo, !CLD).

reassign_tagword_var(Var, ToOrMask, ToOrRval, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    Lvals = lvals_in_rval(ToOrRval),
    (
        Lvals = [],
        var_locn_reassign_tagword_var(Var, ToOrMask, ToOrRval, Code,
            VarLocnInfo0, VarLocnInfo)
    ;
        Lvals = [_ | _],
        unexpected($pred, "non-var lvals")
    ),
    set_var_locn_info(VarLocnInfo, !CLD).

assign_field_lval_expr_to_var(Var, FieldLvals, Rval, Code, !CLD) :-
    ( if
        FieldLvals = [field(MaybeTag, var(BaseVar), _) | RestFieldLvals],
        list.all_true(is_var_field(MaybeTag, BaseVar), RestFieldLvals)
    then
        ( if
            Lvals = lvals_in_rval(Rval),
            all [Lval] (
                list.member(Lval, Lvals)
            =>
                list.member(Lval, FieldLvals)
            )
        then
            get_var_locn_info(!.CLD, VarLocnInfo0),
            var_locn_assign_field_lval_expr_to_var(Var, BaseVar, Rval, Code,
                VarLocnInfo0, VarLocnInfo),
            set_var_locn_info(VarLocnInfo, !CLD)
        else
            unexpected($pred, "rval contains unexpected lval")
        )
    else
        unexpected($pred,
            "FieldLvals not all fields of the same base variable")
    ).

:- pred is_var_field(maybe(ptag)::in, prog_var::in, lval::in) is semidet.

is_var_field(MaybeTag, Var, field(MaybeTag, var(Var), _)).

assign_cell_to_var(Var, ReserveWordAtStart, Ptag, CellArgs, HowToConstruct,
        MaybeSize, MaybeAllocId, MayUseAtomic, Code, !CI, !CLD) :-
    get_next_label(Label, !CI),
    get_static_cell_info(!.CI, StaticCellInfo0),
    get_exprn_opts(!.CI, ExprnOpts),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_assign_cell_to_var(ExprnOpts, Var, ReserveWordAtStart,
        Ptag, CellArgs, HowToConstruct, MaybeSize, MaybeAllocId, MayUseAtomic,
        Label, Code, StaticCellInfo0, StaticCellInfo,
        VarLocnInfo0, VarLocnInfo),
    set_static_cell_info(StaticCellInfo, !CI),
    set_var_locn_info(VarLocnInfo, !CLD).

save_reused_cell_fields(Var, Lval, Code, Regs, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_save_cell_fields(Var, Lval, Code, Regs,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

place_var(Var, Lval, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_place_var(Var, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

:- pred pick_and_place_vars(assoc_list(prog_var, set(lval))::in,
    set(lval)::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

pick_and_place_vars(VarLocSets, LiveLocs, Code, !CLD) :-
    pick_var_places(VarLocSets, VarLocs),
    assoc_list.values(VarLocs, Locs),
    set.list_to_set(Locs, LiveLocs),
    place_vars(VarLocs, Code, !CLD).

:- pred pick_var_places(assoc_list(prog_var, set(lval))::in,
    assoc_list(prog_var, lval)::out) is det.

pick_var_places([], []).
pick_var_places([Var - LvalSet | VarLvalSets], VarLvals) :-
    pick_var_places(VarLvalSets, VarLvals0),
    ( if
        set.to_sorted_list(LvalSet, LvalList),
        LvalList = [Lval | _]
    then
        VarLvals = [Var - Lval | VarLvals0]
    else
        VarLvals = VarLvals0
    ).

:- pred place_vars(assoc_list(prog_var, lval)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

place_vars(VarLocs, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_place_vars(VarLocs, Code, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

produce_variable(Var, Code, Rval, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_produce_var(Var, Rval, Code, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

produce_variable_in_reg(Var, Code, Lval, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_produce_var_in_reg(Var, Lval, Code, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

produce_variable_in_reg_or_stack(Var, Code, Lval, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_produce_var_in_reg_or_stack(Var, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

materialize_vars_in_lval(Lval0, Lval, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_materialize_vars_in_lval(Lval0, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

materialize_vars_in_rval(Rval0, Rval, Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_materialize_vars_in_rval(Rval0, Rval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

acquire_reg_for_var(Var, RegType, Lval, !CLD) :-
    get_follow_var_map(!.CLD, FollowVarsMap),
    get_next_non_reserved(!.CLD, RegType, NextNonReserved),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    ( if
        map.search(FollowVarsMap, Var, PrefLocn),
        PrefLocn = abs_reg(RegType, PrefRegNum),
        PrefRegNum >= 1
    then
        var_locn_acquire_reg_prefer_given(RegType, PrefRegNum, Lval,
            VarLocnInfo0, VarLocnInfo)
    else
        % XXX We should only get a register if the map.search succeeded;
        % otherwise we should put the var in its stack slot.
        var_locn_acquire_reg_start_at_given(RegType, NextNonReserved, Lval,
            VarLocnInfo0, VarLocnInfo)
    ),
    set_var_locn_info(VarLocnInfo, !CLD).

acquire_reg_not_in_storemap(StoreMap, RegType, Lval, !CLD) :-
    map.foldl2(record_highest_used_reg, StoreMap, 0, HighestUsedRegR,
        0, HighestUsedRegF),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    (
        RegType = reg_r,
        NextRegNum = HighestUsedRegR + 1
    ;
        RegType = reg_f,
        NextRegNum = HighestUsedRegF + 1
    ),
    var_locn_acquire_reg_start_at_given(RegType, NextRegNum, Lval,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

:- pred record_highest_used_reg(prog_var::in, abs_locn::in, int::in, int::out,
    int::in, int::out) is det.

record_highest_used_reg(_, AbsLocn, !HighestUsedRegR, !HighestUsedRegF) :-
    (
        AbsLocn = abs_reg(reg_r, N),
        int.max(N, !HighestUsedRegR)
    ;
        AbsLocn = abs_reg(reg_f, N),
        int.max(N, !HighestUsedRegF)
    ;
        ( AbsLocn = any_reg
        ; AbsLocn = abs_stackvar(_, _)
        ; AbsLocn = abs_parent_stackvar(_, _)
        ; AbsLocn = abs_framevar(_)
        )
    ).

acquire_reg(Type, Lval, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_acquire_reg(Type, Lval, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

release_reg(Lval, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_release_reg(Lval, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

reserve_r1(Code, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_clear_r1(Code, VarLocnInfo0, VarLocnInfo1),
    var_locn_acquire_reg_require_given(reg(reg_r, 1),
        VarLocnInfo1, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

clear_r1(empty, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_release_reg(reg(reg_r, 1), VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

%---------------------------------------------------------------------------%

setup_call(GoalInfo, ArgInfos, LiveLocs, Code, CI, !CLD) :-
    partition_args(ArgInfos, InArgInfos, OutArgInfos, _UnusedArgInfos),
    assoc_list.keys(OutArgInfos, OutVars),
    set.list_to_set(OutVars, OutVarSet),
    Detism = goal_info_get_determinism(GoalInfo),
    get_opt_no_return_calls(CI, OptNoReturnCalls),
    get_var_table(CI, VarTable),
    ( if
        Detism = detism_erroneous,
        OptNoReturnCalls = yes
    then
        RealStackVarLocs = [],
        DummyStackVarLocs = []
    else
        compute_forward_live_var_saves(CI, !.CLD, set_to_bitset(OutVarSet),
            ForwardVarLocs),
        CodeModel = goal_info_get_code_model(GoalInfo),
        (
            CodeModel = model_non,
            % Save variables protected by the nearest resumption point on the
            % stack.
            % XXX This should be unnecessary; with the current setup, the code
            % that established the resume point should have saved those
            % variables on the stack already. However, later we should arrange
            % things so that this saving of the resume vars on the stack
            % is delayed until the first call after the setup of the
            % resume point.
            compute_resume_var_stack_locs(!.CLD, ResumeVarLocs),
            list.append(ResumeVarLocs, ForwardVarLocs, StackVarLocs)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),
            StackVarLocs = ForwardVarLocs
        ),
        list.filter(valid_stack_slot(VarTable), StackVarLocs,
            RealStackVarLocs, DummyStackVarLocs)
    ),
    list.filter(key_var_is_of_non_dummy_type(VarTable),
        InArgInfos, RealInArgInfos),
    var_arg_info_to_lval(RealInArgInfos, RealInArgLocs),
    AllRealLocs = RealStackVarLocs ++ RealInArgLocs,
    AllLocs = DummyStackVarLocs ++ AllRealLocs,
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_place_vars(AllLocs, Code, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD),
    assoc_list.values(AllRealLocs, LiveLocList),
    set.list_to_set(LiveLocList, LiveLocs).

setup_return(VarArgInfos, OutLocs, Code, !CLD) :-
    setup_call_args(VarArgInfos, callee, OutLocs, Code, !CLD).

:- pred key_var_is_of_non_dummy_type(var_table::in,
    pair(prog_var, arg_info)::in) is semidet.

key_var_is_of_non_dummy_type(VarTable, Var - _ArgInfo) :-
    var_has_non_dummy_type(VarTable, Var).

:- pred valid_stack_slot(var_table::in, pair(prog_var, lval)::in) is semidet.

valid_stack_slot(VarTable, Var - Lval) :-
    lookup_var_entry(VarTable, Var, Entry),
    Entry ^ vte_is_dummy = is_not_dummy_type,
    ( if
        ( Lval = stackvar(N)
        ; Lval = parent_stackvar(N)
        ; Lval = framevar(N)
        ),
        N < 0
    then
        unexpected($pred, "nondummy var in dummy stack slot")
    else
        true
    ).

:- pred setup_call_args(assoc_list(prog_var, arg_info)::in,
    call_direction::in, set(lval)::out, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

setup_call_args(AllArgsInfos, Direction, LiveLocs, Code, !CLD) :-
    list.filter(call_arg_in_selected_dir(Direction), AllArgsInfos, ArgsInfos),
    var_arg_info_to_lval(ArgsInfos, ArgsLocns),
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_place_vars(ArgsLocns, Code, VarLocnInfo0, VarLocnInfo1),
    set_var_locn_info(VarLocnInfo1, !CLD),
    assoc_list.values(ArgsLocns, LiveLocList),
    set.list_to_set(LiveLocList, LiveLocs),
    assoc_list.keys(ArgsLocns, ArgVars),
    which_variables_are_forward_live(!.CLD, ArgVars,
        set_of_var.init, DeadVars),
    make_vars_forward_dead(DeadVars, !CLD).

:- pred var_arg_info_to_lval(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, lval)::out) is det.

var_arg_info_to_lval([], []).
var_arg_info_to_lval([Var - ArgInfo | RestInfos], [Var - Lval | RestLvals]) :-
    ArgInfo = arg_info(Loc, _Mode),
    code_util.arg_loc_to_register(Loc, Lval),
    var_arg_info_to_lval(RestInfos, RestLvals).

:- pred which_variables_are_forward_live(code_loc_dep::in,
    list(prog_var)::in, set_of_progvar::in, set_of_progvar::out) is det.

which_variables_are_forward_live(_, [], !DeadVars).
which_variables_are_forward_live(CLD, [Var | Vars], !DeadVars) :-
    ( if variable_is_forward_live(CLD, Var) then
        true
    else
        set_of_var.insert(Var, !DeadVars)
    ),
    which_variables_are_forward_live(CLD, Vars, !DeadVars).

:- pred call_arg_in_selected_dir(call_direction::in,
    pair(prog_var, arg_info)::in) is semidet.

call_arg_in_selected_dir(Direction, _ - arg_info(_, Mode)) :-
    (
        Mode = top_in,
        Direction = caller
    ;
        Mode = top_out,
        Direction = callee
    ).

lock_regs(R, F, Exceptions, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_lock_regs(R, F, Exceptions, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

unlock_regs(!CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_unlock_regs(VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

clear_all_registers(OkToDeleteAny, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_clobber_all_regs(OkToDeleteAny, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

clobber_reg(Reg, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_clobber_reg(Reg, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

clobber_regs(Regs, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    var_locn_clobber_regs(Regs, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

save_variables(OutArgs, SavedLocs, Code, CI, !CLD) :-
    compute_forward_live_var_saves(CI, !.CLD, OutArgs, VarLocs),
    assoc_list.values(VarLocs, SavedLocList),
    set.list_to_set(SavedLocList, SavedLocs),
    place_vars(VarLocs, Code, !CLD).

save_variables_on_stack(Vars, Code, CI, !CLD) :-
    list.map(associate_stack_slot(CI), Vars, VarLocs),
    place_vars(VarLocs, Code, !CLD).

:- pred compute_forward_live_var_saves(code_info::in, code_loc_dep::in,
    set_of_progvar::in, assoc_list(prog_var, lval)::out) is det.

compute_forward_live_var_saves(CI, CLD, OutArgs, VarLocs) :-
    get_known_variables(CLD, Variables0),
    Vars0 = set_of_var.list_to_set(Variables0),
    TypeInfoLiveness = body_typeinfo_liveness(CI),
    get_var_table(CI, VarTable),
    get_proc_info(CI, ProcInfo),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    maybe_complete_with_typeinfo_vars(VarTable, RttiVarMaps,
        TypeInfoLiveness, Vars0, Vars1),
    set_of_var.difference(Vars1, OutArgs, Vars),
    Variables = set_of_var.to_sorted_list(Vars),
    list.map(associate_stack_slot(CI), Variables, VarLocs).

:- pred associate_stack_slot(code_info::in, prog_var::in,
    pair(prog_var, lval)::out) is det.

associate_stack_slot(CI, Var, Var - Slot) :-
    get_variable_slot(CI, Var, Slot).

max_reg_in_use(CLD, MaxR, MaxF) :-
    get_var_locn_info(CLD, VarLocnInfo),
    var_locn_max_reg_in_use(VarLocnInfo, MaxR, MaxF).

magically_put_var_in_unused_reg(Var, !CLD) :-
    get_var_locn_info(!.CLD, VarLocnInfo0),
    map.init(StackSlots), % This is a lie, but it doesn't matter.
    do_make_var_forward_live(Var, StackSlots, 1, _, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CLD).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for dealing with the recording of variable liveness
    % information around calls.
    %
    % Value numbering needs to know what locations are live before calls;
    % the garbage collector and the debugger need to know what locations
    % are live containing what types of values after calls.

:- interface.

:- pred generate_call_vn_livevals(code_info::in, code_loc_dep::in,
    list(arg_loc)::in, set_of_progvar::in, set(lval)::out) is det.

:- pred generate_return_live_lvalues(code_info::in, code_loc_dep::in,
    assoc_list(prog_var, arg_loc)::in, instmap::in, bool::in,
    list(liveinfo)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

generate_call_vn_livevals(CI, CLD, InputArgLocs, OutputArgs, LiveVals) :-
    generate_call_stack_vn_livevals(CI, CLD, OutputArgs, StackLiveVals),
    generate_input_var_vn(InputArgLocs, StackLiveVals, LiveVals).

:- pred generate_call_stack_vn_livevals(code_info::in, code_loc_dep::in,
    set_of_progvar::in, set(lval)::out) is det.

generate_call_stack_vn_livevals(CI, CLD, OutputArgs, LiveVals) :-
    get_known_variables(CLD, KnownVarList0),
    get_var_table(CI, VarTable),
    list.filter(var_has_non_dummy_type(VarTable), KnownVarList0, KnownVarList),
    set_of_var.list_to_set(KnownVarList, KnownVars),
    set_of_var.difference(KnownVars, OutputArgs, LiveVars),
    set_of_var.to_sorted_list(LiveVars, LiveVarList),
    generate_stack_var_vn(CI, LiveVarList, set.init, LiveVals1),
    get_active_temps_data(CI, CLD, Temps),
    generate_call_temp_vn(Temps, LiveVals1, LiveVals).

:- pred generate_stack_var_vn(code_info::in, list(prog_var)::in,
    set(lval)::in, set(lval)::out) is det.

generate_stack_var_vn(_, [], !Vals).
generate_stack_var_vn(CI, [V | Vs], !Vals) :-
    get_variable_slot(CI, V, Lval),
    set.insert(Lval, !Vals),
    generate_stack_var_vn(CI, Vs, !Vals).

:- pred generate_call_temp_vn(assoc_list(lval, slot_contents)::in,
    set(lval)::in, set(lval)::out) is det.

generate_call_temp_vn([], !Vals).
generate_call_temp_vn([Lval - _ | Temps], !Vals) :-
    set.insert(Lval, !Vals),
    generate_call_temp_vn(Temps, !Vals).

:- pred generate_input_var_vn(list(arg_loc)::in,
    set(lval)::in, set(lval)::out) is det.

generate_input_var_vn([], !Vals).
generate_input_var_vn([InputArgLoc | InputArgLocs], !Vals) :-
    code_util.arg_loc_to_register(InputArgLoc, Lval),
    set.insert(Lval, !Vals),
    generate_input_var_vn(InputArgLocs, !Vals).

%---------------------------------------------------------------------------%

generate_return_live_lvalues(CI, CLD, OutputArgLocs, ReturnInstMap,
        OkToDeleteAny, LiveLvalues) :-
    get_globals(CI, Globals),
    get_proc_info(CI, ProcInfo),
    get_eff_trace_level(CI, EffTraceLevel),
    variable_locations(CLD, VarLocs),
    get_known_variables(CLD, Vars0),
    get_var_table(CI, VarTable),
    list.filter(var_has_non_dummy_type(VarTable), Vars0, Vars),
    get_active_temps_data(CI, CLD, Temps),
    cont_info_generate_return_live_lvalues(Globals, ProcInfo, EffTraceLevel,
        OutputArgLocs, ReturnInstMap, Vars, VarLocs, Temps,
        OkToDeleteAny, LiveLvalues).

:- pred var_has_non_dummy_type(var_table::in, prog_var::in) is semidet.

var_has_non_dummy_type(VarTable, Var) :-
    lookup_var_entry(VarTable, Var, Entry),
    Entry ^ vte_is_dummy = is_not_dummy_type.

:- pred maybe_generate_resume_layout(label::in, resume_map::in,
    code_info::in, code_info::out, code_loc_dep::in) is det.

maybe_generate_resume_layout(Label, ResumeMap, !CI, CLD) :-
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, agc_stack_layout, AgcStackLayout),
    (
        AgcStackLayout = yes,
        get_active_temps_data(!.CI, CLD, Temps),
        get_instmap(CLD, InstMap),
        get_proc_info(!.CI, ProcInfo),
        continuation_info.generate_resume_layout(ProcInfo, InstMap,
            ResumeMap, Temps, Layout),
        add_resume_layout_for_label(Label, Layout, !CI)
    ;
        AgcStackLayout = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for managing stack slots.

    % Det stack frames are organized as follows.
    %
    %       ... unused ...
    %   sp ---> <first unused slot>
    %       <space for local var 1>
    %       ... local vars ...
    %       <space for local var n>
    %       <space for temporary 1>
    %       ... temporaries ...
    %       <space for temporary n>
    %       <space for saved succip, if needed>
    %
    % The stack pointer points to the first free location at the
    % top of the stack.
    %
    % `succip_is_used' determines whether we need a slot to
    % hold the succip.
    %
    % Nondet stack frames also have the local variables above the
    % temporaries, but contain several fixed slots on top, and the
    % saved succip is stored in one of these.
    %
    % For both kinds of stack frames, the slots holding variables
    % are allocated during the live_vars pass, while the slots holding
    % temporaries are acquired (and if possible, released) on demand
    % during code generation.

:- interface.

    % If a stack slot is persistent, then the stack slot is not implicitly
    % released when the code generator resets its location-dependent state,
    % usually when entering the next arm of a disjunction, switch, etc.

:- type temp_slot_persistence
    --->    persistent_temp_slot
    ;       non_persistent_temp_slot.

    % Acquire a stack slot for storing a temporary. The slot_contents
    % description is for accurate gc.
    %
:- pred acquire_temp_slot(slot_contents::in, temp_slot_persistence::in,
    lval::out, code_info::in, code_info::out,
    code_loc_dep::in, code_loc_dep::out) is det.

    % acquire_several_temp_slots(Items, Persistence, StackVars,
    %   StackId, N, M, !Info):
    %
    % Perform an acquire_temp_slot operation for each element of the
    % input list, all with the same persistence.
    %
    % The slots will be the ones from stack_slot_num_to_lval(StackId, N)
    % consecutively to stack_slot_num_to_lval(StackId, M), with N < M.
    % These will also be returned as StackVars.
    %
:- pred acquire_several_temp_slots(list(slot_contents)::in,
    temp_slot_persistence::in, list(lval)::out,
    main_stack::out, int::out, int::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

    % Release a stack slot acquired earlier for a temporary value.
    % The persistence argument should match the acquire operation.
    %
:- pred release_temp_slot(lval::in, temp_slot_persistence::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

    % Release the stack slots acquired by an earlier acquire_several_temp_slots
    % operation. The persistence argument should match the acquire operation.
    %
:- pred release_several_temp_slots(list(lval)::in, temp_slot_persistence::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

acquire_temp_slot(Item, Persistence, StackVar, !CI, !CLD) :-
    get_temp_content_map(!.CI, TempContentMap0),
    map.to_assoc_list(TempContentMap0, TempContentList),
    get_temps_in_use(!.CLD, TempsInUse0),
    ( if
        find_unused_slot_for_item(TempContentList, Item, TempsInUse0,
            ChosenStackVar, _)
    then
        StackVar = ChosenStackVar
    else
        new_temp_slot(Item, StackVar, !CI)
    ),
    set.insert(StackVar, TempsInUse0, TempsInUse),
    set_temps_in_use(TempsInUse, !CLD),
    (
        Persistence = persistent_temp_slot,
        get_persistent_temps(!.CI, PersistentTemps0),
        set.insert(StackVar, PersistentTemps0, PersistentTemps),
        set_persistent_temps(PersistentTemps, !CI)
    ;
        Persistence = non_persistent_temp_slot
    ).

acquire_several_temp_slots([], _, _, _, _, _, !CI, !CLD) :-
    % We could return an empty list of stack vars for StackVars, but there is
    % nothing meaningful we can return for the other outputs.
    unexpected($pred, "[]").
acquire_several_temp_slots([HeadItem | TailItems], Persistence, StackVars,
        StackId, FirstSlotNum, LastSlotNum, !CI, !CLD) :-
    get_temp_content_map(!.CI, TempContentMap0),
    map.to_assoc_list(TempContentMap0, TempContentList),
    get_temps_in_use(!.CLD, TempsInUse0),
    ( if
        find_unused_slots_for_items(TempContentList, HeadItem, TailItems,
            TempsInUse0, StackVarsPrime,
            StackIdPrime, FirstSlotNumPrime, LastSlotNumPrime)
    then
        StackVars = StackVarsPrime,
        StackId = StackIdPrime,
        FirstSlotNum = FirstSlotNumPrime,
        LastSlotNum = LastSlotNumPrime
    else
        new_temp_slots(HeadItem, TailItems, StackVars,
            StackId, FirstSlotNum, LastSlotNum, !CI)
    ),
    set.insert_list(StackVars, TempsInUse0, TempsInUse),
    set_temps_in_use(TempsInUse, !CLD),
    (
        Persistence = persistent_temp_slot,
        get_persistent_temps(!.CI, PersistentTemps0),
        set.insert_list(StackVars, PersistentTemps0, PersistentTemps),
        set_persistent_temps(PersistentTemps, !CI)
    ;
        Persistence = non_persistent_temp_slot
    ).

:- pred new_temp_slot(slot_contents::in, lval::out,
    code_info::in, code_info::out) is det.

new_temp_slot(Item, StackVar, !CI) :-
    CodeModel = get_proc_model(!.CI),
    StackId = code_model_to_main_stack(CodeModel),

    get_var_slot_count(!.CI, VarSlotCount),
    get_max_temp_slot_count(!.CI, TempSlotCount0),
    FirstSlotNum = VarSlotCount + TempSlotCount0 + 1,
    get_temp_content_map(!.CI, TempContentMap0),
    record_new_temp_slot(Item, StackId, FirstSlotNum, _FirstUnusedSlotNum,
        TempSlotCount0, TempSlotCount, TempContentMap0, TempContentMap,
        StackVar),
    set_max_temp_slot_count(TempSlotCount, !CI),
    set_temp_content_map(TempContentMap, !CI).

:- pred new_temp_slots(slot_contents::in, list(slot_contents)::in,
    list(lval)::out, main_stack::out, int::out, int::out,
    code_info::in, code_info::out) is det.

new_temp_slots(HeadItem, TailItems, StackVars, StackId,
        FirstSlotNum, LastSlotNum, !CI) :-
    CodeModel = get_proc_model(!.CI),
    StackId = code_model_to_main_stack(CodeModel),

    get_var_slot_count(!.CI, VarSlotCount),
    get_max_temp_slot_count(!.CI, TempSlotCount0),
    FirstSlotNum = VarSlotCount + TempSlotCount0 + 1,
    get_temp_content_map(!.CI, TempContentMap0),
    record_new_temp_slot(HeadItem, StackId, FirstSlotNum, NextSlotNum,
        TempSlotCount0, TempSlotCount1, TempContentMap0, TempContentMap1,
        HeadStackVar),
    record_new_temp_slots(TailItems, StackId, NextSlotNum, FirstUnusedSlotNum,
        TempSlotCount1, TempSlotCount, TempContentMap1, TempContentMap,
        TailStackVars),
    StackVars = [HeadStackVar | TailStackVars],
    LastSlotNum = FirstUnusedSlotNum - 1,
    set_max_temp_slot_count(TempSlotCount, !CI),
    set_temp_content_map(TempContentMap, !CI).

:- pred record_new_temp_slots(list(slot_contents)::in,
    main_stack::in, int::in, int::out, int::in, int::out,
    map(lval, slot_contents)::in, map(lval, slot_contents)::out,
    list(lval)::out) is det.

record_new_temp_slots([], _, !CurSlotNum, !TempSlotCount, !TempContentMap, []).
record_new_temp_slots([Item | Items], StackId, !CurSlotNum,
        !TempSlotCount, !TempContentMap, [StackVar | StackVars]) :-
    record_new_temp_slot(Item, StackId, !CurSlotNum,
        !TempSlotCount, !TempContentMap, StackVar),
    record_new_temp_slots(Items, StackId, !CurSlotNum,
        !TempSlotCount, !TempContentMap, StackVars).

:- pred record_new_temp_slot(slot_contents::in,
    main_stack::in, int::in, int::out, int::in, int::out,
    map(lval, slot_contents)::in, map(lval, slot_contents)::out,
    lval::out) is det.

record_new_temp_slot(Item, StackId, !CurSlotNum,
        !TempSlotCount, !TempContentMap, StackVar) :-
    StackVar = stack_slot_num_to_lval(StackId, !.CurSlotNum),
    map.det_insert(StackVar, Item, !TempContentMap),
    !:CurSlotNum = !.CurSlotNum + 1,
    !:TempSlotCount = !.TempSlotCount + 1.

:- pred find_unused_slot_for_item(assoc_list(lval, slot_contents)::in,
    slot_contents::in, set(lval)::in, lval::out,
    assoc_list(lval, slot_contents)::out) is semidet.

find_unused_slot_for_item([Head | Tail], Item, TempsInUse,
        ChosenStackVar, Remainder) :-
    Head = HeadStackVar - HeadSlotType,
    ( if
        HeadSlotType = Item,
        \+ set.member(HeadStackVar, TempsInUse)
    then
        ChosenStackVar = HeadStackVar,
        Remainder = Tail
    else
        find_unused_slot_for_item(Tail, Item, TempsInUse,
            ChosenStackVar, Remainder)
    ).

:- pred find_unused_slots_for_items(assoc_list(lval, slot_contents)::in,
    slot_contents::in, list(slot_contents)::in, set(lval)::in, list(lval)::out,
    main_stack::out, int::out, int::out) is semidet.

find_unused_slots_for_items([Head | Tail], HeadItem, TailItems, TempsInUse,
        ChosenStackVars, StackId, FirstSlotNum, LastSlotNum) :-
    ( if
        find_unused_slot_for_item([Head | Tail], HeadItem, TempsInUse,
            ChosenHeadStackVar, Remainder),
        ( if ChosenHeadStackVar = stackvar(N) then
            StackId0 = det_stack,
            FirstSlotNum0 = N
        else if ChosenHeadStackVar = framevar(N) then
            StackId0 = nondet_stack,
            FirstSlotNum0 = N
        else
            unexpected($pred, "not stackvar or framevar")
        ),
        StackId1 = StackId0,
        FirstSlotNum1 = FirstSlotNum0,
        find_next_slots_for_items(Remainder, TailItems, TempsInUse,
            ChosenTailStackVars, StackId1, FirstSlotNum1, LastSlotNum1)
    then
        ChosenStackVars = [ChosenHeadStackVar | ChosenTailStackVars],
        StackId = StackId1,
        FirstSlotNum = FirstSlotNum1,
        LastSlotNum = LastSlotNum1
    else
        find_unused_slots_for_items(Tail, HeadItem, TailItems, TempsInUse,
            ChosenStackVars, StackId, FirstSlotNum, LastSlotNum)
    ).

:- pred find_next_slots_for_items(assoc_list(lval, slot_contents)::in,
    list(slot_contents)::in, set(lval)::in, list(lval)::out,
    main_stack::in, int::in, int::out) is semidet.

find_next_slots_for_items([], [], _, [], _, !SlotNum).
find_next_slots_for_items([Head | Tail], [HeadItem | TailItems], TempsInUse,
        [HeadStackVar | TailStackVars], StackId, !SlotNum) :-
    Head = HeadStackVar - HeadSlotType,
    !:SlotNum = !.SlotNum + 1,
    HeadStackVar = stack_slot_num_to_lval(StackId, !.SlotNum),
    HeadSlotType = HeadItem,
    \+ set.member(HeadStackVar, TempsInUse),
    find_next_slots_for_items(Tail, TailItems, TempsInUse,
        TailStackVars, StackId, !SlotNum).

release_temp_slot(StackVar, Persistence, !CI, !CLD) :-
    get_temps_in_use(!.CLD, TempsInUse0),
    set.delete(StackVar, TempsInUse0, TempsInUse),
    set_temps_in_use(TempsInUse, !CLD),

    get_persistent_temps(!.CI, PersistentTemps0),
    set.is_member(StackVar, PersistentTemps0, IsInPersistentTemps0),
    (
        Persistence = persistent_temp_slot,
        expect(unify(IsInPersistentTemps0, yes), $pred,
            "released stack slot should be persistent"),
        set.delete(StackVar, PersistentTemps0, PersistentTemps),
        set_persistent_temps(PersistentTemps, !CI)
    ;
        Persistence = non_persistent_temp_slot,
        expect(unify(IsInPersistentTemps0, no), $pred,
            "released stack slot should not be persistent")
    ).

release_several_temp_slots([], _Persistence, !CI, !CLD).
release_several_temp_slots([StackVar | StackVars], Persistence, !CI, !CLD) :-
    release_temp_slot(StackVar, Persistence, !CI, !CLD),
    release_several_temp_slots(StackVars, Persistence, !CI, !CLD).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for debugging the code generator itself.

:- interface.

    % Should we trace the operation of the code generator?
    %
:- pred should_trace_code_gen(code_info::in) is semidet.

:- type code_info_component
    --->    cic_forward_live_vars
    ;       cic_zombies
    ;       cic_temps_in_use
    ;       cic_par_conj_depth.

    % Print the selected parts of the code_loc_dep.
    %
    % If you need to print a part that is not currently selectable, make it
    % selectable.
    %
:- pred output_code_info(io.text_output_stream::in,
    list(code_info_component)::in,
    code_info::in, code_loc_dep::in, io::di, io::uo) is det.

:- implementation.

should_trace_code_gen(CI) :-
    get_pred_id(CI, PredId),
    pred_id_to_int(PredId, PredIdInt),
    get_module_info(CI, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, debug_code_gen_pred_id, DebugPredIdInt),
    PredIdInt = DebugPredIdInt.

output_code_info(Stream, Components, CI, CLD, !IO) :-
    get_var_table(CI, VarTable),
    CLD = code_loc_dep(ForwardLiveVars, _InstMap, Zombies,
        _VarLocnInfo, TempsInUse, _FailInfo, ParConjDepth),
    ( if list.member(cic_forward_live_vars, Components) then
        ForwardLiveVarNames = list.map(
            var_table_entry_name_and_number(VarTable),
            set_of_var.to_sorted_list(ForwardLiveVars)),
        io.format(Stream, "forward live vars: %s\n",
            [s(string.join_list(", ", ForwardLiveVarNames))], !IO)
    else
        true
    ),
    ( if list.member(cic_zombies, Components) then
        ZombieVarNames = list.map(
            var_table_entry_name_and_number(VarTable),
            set_of_var.to_sorted_list(Zombies)),
        io.format(Stream, "zombies: %s\n",
            [s(string.join_list(", ", ZombieVarNames))], !IO)
    else
        true
    ),
    ( if list.member(cic_temps_in_use, Components) then
        io.write_string(Stream, "temps_in_use: ", !IO),
        io.write_string(Stream,
            dump_lvals(no, set.to_sorted_list(TempsInUse)), !IO),
        io.nl(Stream, !IO)
    else
        true
    ),
    ( if list.member(cic_par_conj_depth, Components) then
        io.format(Stream, "par_conj_depth: %d\n", [i(ParConjDepth)], !IO)
    else
        true
    ).

:- pred output_resume_map(io.text_output_stream::in, var_table::in,
    string::in, map(prog_var, set(lval))::in, label::in,
    io::di, io::uo) is det.

output_resume_map(Stream, VarTable, Desc, ResumeMap, ResumeLabel, !IO) :-
    (
        ResumeLabel = internal_label(LabelNumber, _ProcLabel)
    ;
        ResumeLabel = entry_label(_, _),
        unexpected($pred, "resume label is an entry label")
    ),
    io.format(Stream, "  %-6s local label %d\n",
        [s(Desc), i(LabelNumber)], !IO),
    map.to_assoc_list(ResumeMap, ResumeAssocList),
    list.foldl(output_resume_map_element(Stream, VarTable),
        ResumeAssocList, !IO).

:- pred output_resume_map_element(io.text_output_stream::in, var_table::in,
    pair(prog_var, set(lval))::in, io::di, io::uo) is det.

output_resume_map_element(Stream, VarTable, Var - LvalSet, !IO) :-
    lookup_var_entry(VarTable, Var, Entry),
    VarDesc = var_entry_name_and_number(Var, Entry),
    Lvals = set.to_sorted_list(LvalSet),
    LvalDescs = list.map(dump_lval(no), Lvals),
    LvalsDesc = string.join_list(" ", LvalDescs),
    io.format(Stream, "    %s: %s\n", [s(VarDesc), s(LvalsDesc)], !IO).

%---------------------------------------------------------------------------%
:- end_module ll_backend.code_loc_dep.
%---------------------------------------------------------------------------%
