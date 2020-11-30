%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: instmap.m.
% Main author: bromage.
%
% This module contains the instmap and instmap_delta ADTs.
%
% An instmap maps variables to their instantiation states (if the relevant
% program point is reachable).
%
% An instmap_delta stores how variables' instantiation states
% change across a goal.
%
%---------------------------------------------------------------------------%

:- module hlds.instmap.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.mode_info.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

:- type instmap.
:- type instmap_delta.

%---------------------------------------------------------------------------%

    % Initialize an empty instmap.
    %
:- pred init_reachable(instmap::out) is det.

    % Initialize an empty unreachable instmap.
    %
:- pred init_unreachable(instmap::out) is det.

    % Initialize an empty reachable instmap_delta.
    %
:- pred instmap_delta_init_reachable(instmap_delta::out) is det.

    % Initialize an empty unreachable instmap_delta.
    %
:- pred instmap_delta_init_unreachable(instmap_delta::out) is det.

%---------------------------------------------------------------------------%

    % For any instmap InstMap, exactly one of
    % is_reachable(InstMap) and
    % is_unreachable(InstMap) holds.

    % Is the instmap reachable?
    %
:- pred instmap_is_reachable(instmap::in) is semidet.

    % Is the instmap unreachable?
    %
:- pred instmap_is_unreachable(instmap::in) is semidet.

    % For any instmap InstMapDelta, exactly one of
    % instmap_delta_is_reachable(InstMapDelta) and
    % instmap_delta_is_unreachable(InstMapDelta) holds.

    % Is the instmap_delta reachable?
    %
:- pred instmap_delta_is_reachable(instmap_delta::in) is semidet.

    % Is the instmap_delta unreachable?
    %
:- pred instmap_delta_is_unreachable(instmap_delta::in) is semidet.

%---------------------------------------------------------------------------%

:- func instmap_from_assoc_list(assoc_list(prog_var, mer_inst)) =
    instmap.

:- pred instmap_to_assoc_list(instmap::in,
    assoc_list(prog_var, mer_inst)::out) is det.

:- func instmap_delta_from_assoc_list(assoc_list(prog_var, mer_inst)) =
    instmap_delta.

:- pred instmap_delta_to_assoc_list(instmap_delta::in,
    assoc_list(prog_var, mer_inst)::out) is det.

%---------------------------------------------------------------------------%

:- type var_init_final_insts
    --->    var_init_final_insts(
                vifi_var        :: prog_var,
                vifi_init_inst  :: mer_inst,
                vifi_final_inst :: mer_inst
            ).

:- pred instmap_delta_from_mode_list(module_info::in,
    list(prog_var)::in, list(mer_mode)::in, instmap_delta::out) is det.
:- pred instmap_delta_from_var_init_final_insts(module_info::in,
    list(var_init_final_insts)::in, instmap_delta::out) is det.

%---------------------------------------------------------------------------%

    % Return the set of variables in an instmap.
    %
:- pred instmap_vars(instmap::in, set_of_progvar::out) is det.

    % Return the list of variables in an instmap.
    %
:- pred instmap_vars_list(instmap::in, list(prog_var)::out) is det.

%---------------------------------------------------------------------------%

    % Given an instmap and a variable, determine the inst of that variable.
    %
:- pred instmap_lookup_var(instmap::in, prog_var::in, mer_inst::out) is det.

    % Given an instmap and a list of variables, return a list
    % containing the insts of those variable.
    %
:- pred instmap_lookup_vars(instmap::in, list(prog_var)::in,
    list(mer_inst)::out) is det.

    % Given an instmap_delta and a variable, determine the new inst
    % of that variable (if any).
    %
:- pred instmap_delta_search_var(instmap_delta::in, prog_var::in,
    mer_inst::out) is semidet.

    % Given an instmap_delta and a variable, determine the new inst
    % of that variable (which must have one).
    %
:- pred instmap_delta_lookup_var(instmap_delta::in, prog_var::in,
    mer_inst::out) is det.

%---------------------------------------------------------------------------%
% ZZZ instmap_insert_var
% ZZZ instmap_delta_set_vars
% ZZZ instmap_delta_set_vars_corresponding

    % Set an entry in an instmap.
    %
:- pred instmap_set_var(prog_var::in, mer_inst::in, instmap::in, instmap::out)
    is det.

    % Set multiple entries in an instmap. None of the insts should be
    % `not_reached'.
    %
:- pred instmap_set_vars(assoc_list(prog_var, mer_inst)::in,
    instmap::in, instmap::out) is det.
:- pred instmap_set_vars_corresponding(list(prog_var)::in, list(mer_inst)::in,
    instmap::in, instmap::out) is det.
:- pred instmap_set_vars_same(mer_inst::in, list(prog_var)::in,
    instmap::in, instmap::out) is det.

    % Insert an entry into an instmap_delta. Note that you cannot call
    % instmap_delta_insert for a variable already present.
    %
:- pred instmap_delta_insert_var(prog_var::in, mer_inst::in,
    instmap_delta::in, instmap_delta::out) is det.

:- pred instmap_delta_set_var(prog_var::in, mer_inst::in,
    instmap_delta::in, instmap_delta::out) is det.

:- pred instmap_delta_set_vars_same(mer_inst::in, list(prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

%---------------------------------------------------------------------------%

    % instmap_restrict takes an instmap and a set of vars and returns
    % an instmap with its domain restricted to those vars.
    %
:- pred instmap_restrict(set_of_progvar::in, instmap::in, instmap::out) is det.

    % instmap_delta_restrict takes an instmap and a set of vars and returns
    % an instmap_delta with its domain restricted to those vars.
    %
:- pred instmap_delta_restrict(set_of_progvar::in,
    instmap_delta::in, instmap_delta::out) is det.

    % instmap_delta_delete_vars takes an instmap_delta and a list of vars
    % and returns an instmap_delta with those vars removed from its domain.
    %
:- pred instmap_delta_delete_vars(list(prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

%---------------------------------------------------------------------------%

:- pred var_is_ground_in_instmap(module_info::in, instmap::in, prog_var::in)
    is semidet.

:- pred var_is_any_in_instmap(module_info::in, instmap::in, prog_var::in)
    is semidet.

    % var_is_bound_in_instmap_delta(ModuleInfo, InstMap, InstMapDelta, Var)
    %
    % Succeed if Var is a variable bound between InstMap and
    % InstMap+InstMapDelta. Fail if either InstMap or InstMapDelta are
    % unreachable.
    %
:- pred var_is_bound_in_instmap_delta(module_info::in,
    instmap::in, instmap_delta::in, prog_var::in) is semidet.

%---------------------------------------------------------------------------%

    % Return the set of variables in the instmap whose instantiatedness
    % passes the inst_is_bound test.
    %
:- pred instmap_bound_vars(module_info::in, instmap::in, set_of_progvar::out)
    is det.

    % instmap_delta_no_output_vars(ModuleInfo, VarTypes, InstMap,
    %   InstMapDelta, Vars)
    % is true if none of the vars in Vars can become more instantiated
    % when InstMapDelta is applied to InstMap.
    %
:- pred instmap_delta_no_output_vars(module_info::in, vartypes::in,
    instmap::in, instmap_delta::in, set_of_progvar::in) is semidet.

    % instmap_changed_vars(ModuleInfo, VarTypes, IMA, IMB, CV)
    %
    % Given an earlier instmap, IMA, and a later instmap, IMB, determine
    % what variables, CV, have had their instantiatedness information changed.
    %
    % This predicate is meant to be equivalent to instmap_delta_changed_vars,
    % where the instmap_delta is simply the one to take IMA to IMB.
    %
:- pred instmap_changed_vars(module_info::in, vartypes::in,
    instmap::in, instmap::in, set_of_progvar::out) is det.

    % Return the set of variables whose instantiations have changed
    % (or our knowledge about them has changed) across an instmap_delta.
    %
:- pred instmap_delta_changed_vars(instmap_delta::in, set_of_progvar::out)
    is det.

%---------------------------------------------------------------------------%
% ZZZ

:- func instmap_delta_bind_no_var = instmap_delta.
:- func instmap_delta_bind_var(prog_var) = instmap_delta.
:- func instmap_delta_bind_vars(list(prog_var)) = instmap_delta.

%---------------------------------------------------------------------------%
%
% Bind a variable in an instmap to a functor or functors at the beginning
% of a case in a switch. Aborts on compiler generated cons_ids.
%

:- pred bind_var_to_functor(prog_var::in, mer_type::in, cons_id::in,
    instmap::in, instmap::out, module_info::in, module_info::out) is det.
:- pred bind_var_to_functors(prog_var::in, mer_type::in,
    cons_id::in, list(cons_id)::in, instmap::in, instmap::out,
    module_info::in, module_info::out) is det.

:- pred instmap_delta_bind_var_to_functor(prog_var::in, mer_type::in,
    cons_id::in, instmap::in, instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.
:- pred instmap_delta_bind_var_to_functors(prog_var::in, mer_type::in,
    cons_id::in, list(cons_id)::in, instmap::in,
    instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % Update the given instmap to include the initial insts of the
    % lambda variables.
    %
:- pred pre_lambda_update(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, instmap::in, instmap::out) is det.

%---------------------------------------------------------------------------%

    % Given two instmaps and a set of variables, compute an instmap delta
    % which records the change in the instantiation state of those variables.
    %
:- pred compute_instmap_delta(instmap::in, instmap::in, set_of_progvar::in,
    instmap_delta::out) is det.

%---------------------------------------------------------------------------%

:- type overlay_how
    --->    large_base
    ;       large_overlay
    ;       test_size.

    % Given an instmap and an instmap_delta, overlay the entries in the
    % instmap_delta on top of those in the instmap to produce a new instmap.
    %
:- pred apply_instmap_delta(instmap_delta::in, instmap::in, instmap::out)
    is det.

    % Given two instmap_deltas, overlay the entries in the second instmap_delta
    % on top of those in the first to produce a new instmap_delta.
    %
:- pred instmap_delta_apply_instmap_delta(instmap_delta::in, instmap_delta::in,
    overlay_how::in, instmap_delta::out) is det.

%---------------------------------------------------------------------------%

:- type arm_instmap
    --->    arm_instmap(
                % The context of the arm goal.
                prog_context,

                % The instmap at the point at the end of the arm.
                instmap
            ).

:- pred make_arm_instmaps_for_goals(list(hlds_goal)::in, list(instmap)::in,
    list(arm_instmap)::out) is det.
:- pred make_arm_instmaps_for_cases(list(case)::in, list(instmap)::in,
    list(arm_instmap)::out) is det.

%---------------------------------------------------------------------------%

    % instmap_merge(NonLocalVars, ArmInstMaps, MergeContext, !ModeInfo):
    %
    % Merge the instmaps resulting from different branches of a disjunction
    % or if-then-else, and update the instantiatedness of all the nonlocal
    % variables, checking that it is the same for every branch.
    %
:- pred instmap_merge(set_of_progvar::in, list(arm_instmap)::in,
    merge_context::in, mode_info::in, mode_info::out) is det.

%---------------------------------------------------------------------------%

    % merge_instmap_delta(InitialInstMap, NonLocals,
    %   InstMapDeltaA, InstMapDeltaB, !ModuleInfo):
    %
    % Merge the instmap_deltas of different branches of an if-then-else,
    % disj or switch.
    %
:- pred merge_instmap_delta(instmap::in, set_of_progvar::in, vartypes::in,
    instmap_delta::in, instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

    % merge_instmap_deltas(Vars, InstMapDeltas, MergedInstMapDelta,
    %   !ModuleInfo):
    %
    % Takes a list of instmap deltas from the branches of an if-then-else,
    % switch, or disj and merges them. This is used in situations
    % where the bindings are known to be compatible.
    %
:- pred merge_instmap_deltas(instmap::in, set_of_progvar::in, vartypes::in,
    list(instmap_delta)::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % unify_instmap_delta(InitialInstMap, NonLocals,
    %   InstMapDeltaA, InstMapDeltaB, !ModuleInfo)
    %
    % Unify the instmap_deltas of different branches of a parallel
    % conjunction.
    %
:- pred unify_instmap_delta(instmap::in, set_of_progvar::in, instmap_delta::in,
    instmap_delta::in, instmap_delta::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

:- pred instmap_apply_sub(must_rename::in, map(prog_var, prog_var)::in,
    instmap::in, instmap::out) is det.

    % instmap_delta_apply_sub(Must, Renaming, InstMapDelta0, InstMapDelta):
    %
    % Apply the variable renaming Renaming to InstMapDelta0 to get the new
    % instmap_delta InstMapDelta. If there is a variable in InstMapDelta0
    % which does not appear in Renaming, it is ignored if Must is set to
    % need_not_rename, otherwise it is an error.
    %
:- pred instmap_delta_apply_sub(must_rename::in, map(prog_var, prog_var)::in,
    instmap_delta::in, instmap_delta::out) is det.

%---------------------------------------------------------------------------%

    % Apply the specified procedure to all insts in an instmap_delta.
    %
:- pred instmap_delta_map_foldl(
    pred(prog_var, mer_inst, mer_inst, T, T)::
        in(pred(in, in, out, in, out) is det),
    instmap_delta::in, instmap_delta::out, T::in, T::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.

:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- type instmap
    --->    reachable(instmapping)
    ;       unreachable.

:- type instmap_delta == instmap.

:- type instmapping == map(prog_var, mer_inst).

%---------------------------------------------------------------------------%

init_reachable(reachable(InstMapping)) :-
    map.init(InstMapping).

init_unreachable(unreachable).

instmap_delta_init_reachable(reachable(InstMapping)) :-
    map.init(InstMapping).

instmap_delta_init_unreachable(unreachable).

%---------------------------------------------------------------------------%

instmap_is_reachable(reachable(_)).

instmap_is_unreachable(unreachable).

instmap_delta_is_reachable(reachable(_)).

instmap_delta_is_unreachable(unreachable).

%---------------------------------------------------------------------------%

instmap_from_assoc_list(AL) = reachable(Instmapping) :-
    map.from_assoc_list(AL, Instmapping).

instmap_to_assoc_list(unreachable, []).
instmap_to_assoc_list(reachable(InstMapping), AL) :-
    map.to_assoc_list(InstMapping, AL).

instmap_delta_from_assoc_list(AL) = reachable(Instmapping) :-
    map.from_assoc_list(AL, Instmapping).

instmap_delta_to_assoc_list(unreachable, []).
instmap_delta_to_assoc_list(reachable(InstMapping), AL) :-
    map.to_assoc_list(InstMapping, AL).

%---------------------------------------------------------------------------%

instmap_delta_from_mode_list(ModuleInfo, Var, Modes, InstMapDelta) :-
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_from_mode_list_loop(ModuleInfo, Var, Modes,
        InstMapDelta0, InstMapDelta).

:- pred instmap_delta_from_mode_list_loop(module_info::in,
    list(prog_var)::in, list(mer_mode)::in,
    instmap_delta::in, instmap_delta::out) is det.

instmap_delta_from_mode_list_loop(_, [], [], !InstMapDelta).
instmap_delta_from_mode_list_loop(_, [], [_ | _], !InstMapDelta) :-
    unexpected($pred, "length mismatch").
instmap_delta_from_mode_list_loop(_, [_ | _], [], !InstMapDelta) :-
    unexpected($pred, "length mismatch").
instmap_delta_from_mode_list_loop(ModuleInfo, [Var | Vars], [Mode | Modes],
        !InstMapDelta) :-
    mode_get_insts(ModuleInfo, Mode, InitInst, FinalInst),
    ( if InitInst = FinalInst then
        true
    else
        instmap_delta_set_var(Var, FinalInst, !InstMapDelta)
    ),
    instmap_delta_from_mode_list_loop(ModuleInfo, Vars, Modes, !InstMapDelta).

%---------------------%

instmap_delta_from_var_init_final_insts(ModuleInfo, VarsInsts, InstMapDelta) :-
    instmap_delta_init_reachable(InstMapDelta0),
    instmap_delta_from_var_init_final_insts_loop(ModuleInfo, VarsInsts,
        InstMapDelta0, InstMapDelta).

:- pred instmap_delta_from_var_init_final_insts_loop(module_info::in,
    list(var_init_final_insts)::in,
    instmap_delta::in, instmap_delta::out) is det.

instmap_delta_from_var_init_final_insts_loop(_, [], !InstMapDelta).
instmap_delta_from_var_init_final_insts_loop(ModuleInfo,
        [VarInsts | VarsInsts], !InstMapDelta) :-
    VarInsts = var_init_final_insts(Var, InitInst, FinalInst),
    ( if InitInst = FinalInst then
        true
    else
        instmap_delta_set_var(Var, FinalInst, !InstMapDelta)
    ),
    instmap_delta_from_var_init_final_insts_loop(ModuleInfo, VarsInsts,
        !InstMapDelta).

%---------------------------------------------------------------------------%

instmap_vars(Instmap, Vars) :-
    instmap_vars_list(Instmap, VarsList),
    set_of_var.list_to_set(VarsList, Vars).

instmap_vars_list(unreachable, []).
instmap_vars_list(reachable(InstMapping), VarsList) :-
    map.keys(InstMapping, VarsList).

%---------------------------------------------------------------------------%

instmap_lookup_var(unreachable, _Var, not_reached).
instmap_lookup_var(reachable(InstMap), Var, Inst) :-
    instmapping_lookup_var(InstMap, Var, Inst).

:- pred instmapping_lookup_var(instmapping::in, prog_var::in, mer_inst::out)
    is det.

instmapping_lookup_var(InstMap, Var, Inst) :-
    ( if map.search(InstMap, Var, VarInst) then
        Inst = VarInst
    else
        Inst = free
    ).

instmap_lookup_vars(_InstMap, [], []).
instmap_lookup_vars(InstMap, [Arg | Args], [Inst | Insts]) :-
    instmap_lookup_var(InstMap, Arg, Inst),
    instmap_lookup_vars(InstMap, Args, Insts).

instmap_delta_search_var(unreachable, _, not_reached).
instmap_delta_search_var(reachable(InstMap), Var, Inst) :-
    map.search(InstMap, Var, Inst).

instmap_delta_lookup_var(InstMapDelta, Var, Inst) :-
    ( if instmap_delta_search_var(InstMapDelta, Var, InstPrime) then
        Inst = InstPrime
    else
        unexpected($pred, "var not in instmap")
    ).

%---------------------------------------------------------------------------%

instmap_set_var(_Var, _Inst, unreachable, unreachable).
instmap_set_var(Var, Inst, reachable(InstMapping0), reachable(InstMapping)) :-
    map.set(Var, Inst, InstMapping0, InstMapping).

instmap_set_vars(VarsInsts, !InstMap) :-
    (
        !.InstMap = unreachable
        % Leave the instmap as it is.
    ;
        !.InstMap = reachable(InstMapping0),
        instmapping_set_vars(VarsInsts, InstMapping0, InstMapping),
        !:InstMap = reachable(InstMapping)
    ).

instmap_set_vars_corresponding(Vars, Insts, !InstMap) :-
    (
        !.InstMap = unreachable
        % Leave the instmap as it is.
    ;
        !.InstMap = reachable(InstMapping0),
        instmapping_set_vars_corresponding(Vars, Insts,
            InstMapping0, InstMapping),
        !:InstMap = reachable(InstMapping)
    ).

instmap_set_vars_same(Inst, Vars, !InstMap) :-
    (
        !.InstMap = unreachable
        % Leave the instmap as it is.
    ;
        !.InstMap = reachable(InstMapping0),
        expect(negate(unify(Inst, not_reached)), $pred, "not_reached"),
        instmapping_set_vars_same(Inst, Vars, InstMapping0, InstMapping),
        !:InstMap = reachable(InstMapping)
    ).

%---------------------%

instmap_delta_insert_var(_Var, _Inst, unreachable, unreachable).
instmap_delta_insert_var(Var, Inst, reachable(InstMapping0), Instmap) :-
    ( if Inst = not_reached then
        Instmap = unreachable
    else
        map.det_insert(Var, Inst, InstMapping0, InstMapping),
        Instmap = reachable(InstMapping)
    ).

instmap_delta_set_var(_Var, _Inst, unreachable, unreachable).
instmap_delta_set_var(Var, Inst, reachable(InstMapping0), Instmap) :-
    ( if Inst = not_reached then
        Instmap = unreachable
    else
        map.set(Var, Inst, InstMapping0, InstMapping),
        Instmap = reachable(InstMapping)
    ).

instmap_delta_set_vars_same(Inst, Vars, !InstMapDelta) :-
    (
        !.InstMapDelta = unreachable
        % Leave the instmap as it is.
    ;
        !.InstMapDelta = reachable(InstMapping0),
        expect(negate(unify(Inst, not_reached)), $pred, "not_reached"),
        instmapping_set_vars_same(Inst, Vars, InstMapping0, InstMapping),
        !:InstMapDelta = reachable(InstMapping)
    ).

%---------------------%

:- pred instmapping_set_vars(assoc_list(prog_var, mer_inst)::in,
    instmapping::in, instmapping::out) is det.

instmapping_set_vars([], !InstMapping).
instmapping_set_vars([Var - Inst | VarsInsts], !InstMapping) :-
    expect_not(unify(Inst, not_reached), $pred, "not_reached"),
    map.set(Var, Inst, !InstMapping),
    instmapping_set_vars(VarsInsts, !InstMapping).

:- pred instmapping_set_vars_corresponding(
    list(prog_var)::in, list(mer_inst)::in,
    instmapping::in, instmapping::out) is det.

instmapping_set_vars_corresponding([], [], !InstMapping).
instmapping_set_vars_corresponding([Var | Vars], [Inst | Insts],
        !InstMapping) :-
    expect(negate(unify(Inst, not_reached)), $pred, "not_reached"),
    map.set(Var, Inst, !InstMapping),
    instmapping_set_vars_corresponding(Vars, Insts, !InstMapping).
instmapping_set_vars_corresponding([_ | _], [], !InstMapping) :-
    unexpected($pred, "length mismatch (1)").
instmapping_set_vars_corresponding([], [_ | _], !InstMapingp) :-
    unexpected($pred, "length mismatch (2)").

:- pred instmapping_set_vars_same(mer_inst::in, list(prog_var)::in,
    instmapping::in, instmapping::out) is det.

instmapping_set_vars_same(_, [], !InstMapping).
instmapping_set_vars_same(Inst, [Var | Vars], !InstMapping) :-
    map.set(Var, Inst, !InstMapping),
    instmapping_set_vars_same(Inst, Vars, !InstMapping).

%---------------------------------------------------------------------------%

instmap_restrict(_, unreachable, unreachable).
instmap_restrict(Vars, reachable(InstMapping0), reachable(InstMapping)) :-
    map.select_sorted_list(InstMapping0, set_of_var.to_sorted_list(Vars),
        InstMapping).

instmap_delta_restrict(_, unreachable, unreachable).
instmap_delta_restrict(Vars,
        reachable(InstMapping0), reachable(InstMapping)) :-
    map.select_sorted_list(InstMapping0, set_of_var.to_sorted_list(Vars),
        InstMapping).

instmap_delta_delete_vars(_, unreachable, unreachable).
instmap_delta_delete_vars(Vars,
        reachable(InstMapping0), reachable(InstMapping)) :-
    map.delete_list(Vars, InstMapping0, InstMapping).

%---------------------------------------------------------------------------%

var_is_ground_in_instmap(ModuleInfo, InstMap, Var) :-
    instmap_lookup_var(InstMap, Var, Inst),
    inst_is_ground(ModuleInfo, Inst).

var_is_any_in_instmap(ModuleInfo, InstMap, Var) :-
    instmap_lookup_var(InstMap, Var, Inst),
    inst_is_any(ModuleInfo, Inst).

var_is_bound_in_instmap_delta(ModuleInfo, InstMap, InstMapDelta, Var) :-
    instmap_is_reachable(InstMap),
    instmap_delta_is_reachable(InstMapDelta),
    instmap_lookup_var(InstMap, Var, OldVarInst),
    inst_is_free(ModuleInfo, OldVarInst),
    instmap_delta_search_var(InstMapDelta, Var, VarInst),
    inst_is_bound(ModuleInfo, VarInst).

%---------------------------------------------------------------------------%

instmap_bound_vars(_ModuleInfo, unreachable, set_of_var.init).
instmap_bound_vars(ModuleInfo, reachable(InstMapping), BoundVars) :-
    map.foldl(instmap_bound_vars_2(ModuleInfo), InstMapping,
        set_of_var.init, BoundVars).

:- pred instmap_bound_vars_2(module_info::in, prog_var::in, mer_inst::in,
    set_of_progvar::in, set_of_progvar::out) is det.

instmap_bound_vars_2(ModuleInfo, Var, Inst, !BoundVars) :-
    ( if inst_is_bound(ModuleInfo, Inst) then
        set_of_var.insert(Var, !BoundVars)
    else
        true
    ).

instmap_delta_no_output_vars(ModuleInfo, VarTypes, InstMap0, InstMapDelta,
        Vars) :-
    (
        InstMapDelta = unreachable
    ;
        InstMapDelta = reachable(InstMapDeltaMap),
        Test = var_is_not_output(ModuleInfo, VarTypes, InstMap0,
            InstMapDeltaMap),
        set_of_var.all_true(Test, Vars)
    ).

:- pred var_is_not_output(module_info::in, vartypes::in,
    instmap::in, instmapping::in, prog_var::in) is semidet.

var_is_not_output(ModuleInfo, VarTypes, InstMap0, InstMapDeltaMap, Var) :-
    instmap_lookup_var(InstMap0, Var, OldInst),
    ( if map.search(InstMapDeltaMap, Var, NewInst) then
        % We use `inst_matches_binding' to check that the new inst has only
        % added information or lost uniqueness, not bound anything.
        % If the instmap delta contains the variable, the variable may still
        % not be output, if the change is just an increase in information
        % rather than an increase in instantiatedness.
        %
        % XXX Inlining can result in cases where OldInst (from procedure 1)
        % can decide that Var must be bound to one set of function symbols,
        % while NewInst (from a later unification inlined into procedure 1
        % from procedure 2) can say that it is bound to a different,
        % non-overlapping set of function symbols. In such cases,
        % inst_matches_binding will fail, even though we want to succeed.
        % The right fix for this would be to generalize inst_matches_binding,
        % to allow the caller to specify what kinds of deviations from an exact
        % syntactic match are ok.
        lookup_var_type(VarTypes, Var, Type),
        inst_matches_binding(NewInst, OldInst, Type, ModuleInfo)
    else
        % If the instmap delta doesn't contain the variable, it may still
        % have been (partially) output, if its inst is (or contains) `any'.
        not inst_contains_any(ModuleInfo, OldInst)
    ).

%---------------------%

instmap_changed_vars(ModuleInfo, VarTypes, InstMapA, InstMapB, ChangedVars) :-
    instmap_vars_list(InstMapB, VarsB),
    instmap_changed_vars_loop(ModuleInfo, VarTypes, VarsB, InstMapA, InstMapB,
        ChangedVars).

:- pred instmap_changed_vars_loop(module_info::in, vartypes::in,
    prog_vars::in, instmap::in, instmap::in, set_of_progvar::out) is det.

instmap_changed_vars_loop(_ModuleInfo, _VarTypes, [],
        _InstMapA, _InstMapB, ChangedVars) :-
    set_of_var.init(ChangedVars).
instmap_changed_vars_loop(ModuleInfo, VarTypes, [VarB | VarBs],
        InstMapA, InstMapB, ChangedVars) :-
    instmap_changed_vars_loop(ModuleInfo, VarTypes, VarBs,
        InstMapA, InstMapB, ChangedVars0),
    instmap_lookup_var(InstMapA, VarB, InitialInst),
    instmap_lookup_var(InstMapB, VarB, FinalInst),
    lookup_var_type(VarTypes, VarB, Type),
    ( if
        inst_matches_final_typed(InitialInst, FinalInst, Type, ModuleInfo)
    then
        ChangedVars = ChangedVars0
    else
        set_of_var.insert(VarB, ChangedVars0, ChangedVars)
    ).

instmap_delta_changed_vars(unreachable, ChangedVars) :-
    set_of_var.init(ChangedVars).
instmap_delta_changed_vars(reachable(InstMapping), ChangedVars) :-
    map.keys(InstMapping, ChangedVarsList),
    set_of_var.sorted_list_to_set(ChangedVarsList, ChangedVars).

%---------------------------------------------------------------------------%

instmap_delta_bind_no_var = InstMapDelta :-
    InstMapDelta = instmap_delta_from_assoc_list([]).

instmap_delta_bind_var(Var) = InstMapDelta :-
    InstMapDelta = instmap_delta_from_assoc_list([pair_with_ground(Var)]).

instmap_delta_bind_vars(Vars) = InstMapDelta :-
    VarsAndGround = ground_vars(Vars),
    InstMapDelta = instmap_delta_from_assoc_list(VarsAndGround).

:- func ground_vars(list(prog_var)) = assoc_list(prog_var, mer_inst).

ground_vars(Vars) = VarsAndGround :-
    VarsAndGround = list.map(pair_with_ground, Vars).

:- func pair_with_ground(prog_var) = pair(prog_var, mer_inst).

pair_with_ground(Var) = Var - ground(shared, none_or_default_func).

%---------------------------------------------------------------------------%

bind_var_to_functor(Var, Type, ConsId, !InstMap, !ModuleInfo) :-
    instmap_lookup_var(!.InstMap, Var, Inst0),
    bind_inst_to_functor(Type, ConsId, Inst0, Inst, !ModuleInfo),
    instmap_set_var(Var, Inst, !InstMap).

bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        !InstMap, !ModuleInfo) :-
    instmap_lookup_var(!.InstMap, Var, Inst0),
    bind_inst_to_functors(Type, MainConsId, OtherConsIds, Inst0, Inst,
        !ModuleInfo),
    instmap_set_var(Var, Inst, !InstMap).

:- pred bind_inst_to_functor(mer_type::in, cons_id::in,
    mer_inst::in, mer_inst::out, module_info::in, module_info::out) is det.

bind_inst_to_functor(Type, ConsId, !Inst, !ModuleInfo) :-
    Arity = cons_id_adjusted_arity(!.ModuleInfo, Type, ConsId),
    list.duplicate(Arity, is_dead, ArgLives),
    list.duplicate(Arity, free, ArgInsts),
    ( if
        abstractly_unify_inst_functor(is_dead, !.Inst, ConsId, ArgInsts,
            ArgLives, real_unify, Type, !:Inst, _Det, !ModuleInfo)
    then
        true
    else
        unexpected($pred, "mode error")
    ).

:- pred bind_inst_to_functors(mer_type::in, cons_id::in, list(cons_id)::in,
    mer_inst::in, mer_inst::out, module_info::in, module_info::out) is det.

bind_inst_to_functors(Type, MainConsId, OtherConsIds, InitInst, FinalInst,
        !ModuleInfo) :-
    bind_inst_to_functor(Type, MainConsId, InitInst,
        MainFinalInst, !ModuleInfo),
    bind_inst_to_functors_others(Type, OtherConsIds, InitInst,
        OtherFinalInsts, !ModuleInfo),
    merge_var_insts(yes(Type), MainFinalInst, OtherFinalInsts, MaybeMergedInst,
        !ModuleInfo),
    (
        MaybeMergedInst = yes(FinalInst)
    ;
        MaybeMergedInst = no,
        % bind_inst_to_functors should be called only when multi-cons-id
        % switches are being or have been introduced into the HLDS, which
        % should come only after mode checking has been done without finding
        % any errors. Finding an error now would mean that some compiler pass
        % executed between mode checking and now has screwed up.
        unexpected($pred, "no MaybeMergedInst")
    ).

:- pred bind_inst_to_functors_others(mer_type::in, list(cons_id)::in,
    mer_inst::in, list(mer_inst)::out, module_info::in, module_info::out)
    is det.

bind_inst_to_functors_others(_Type, [], _InitInst, [], !ModuleInfo).
bind_inst_to_functors_others(Type, [ConsId | ConsIds], InitInst,
        [FinalInst | FinalInsts], !ModuleInfo) :-
    bind_inst_to_functor(Type, ConsId, InitInst, FinalInst, !ModuleInfo),
    bind_inst_to_functors_others(Type, ConsIds, InitInst, FinalInsts,
        !ModuleInfo).

%---------------------%

instmap_delta_bind_var_to_functor(Var, Type, ConsId, InstMap, !InstmapDelta,
        !ModuleInfo) :-
    (
        !.InstmapDelta = unreachable
    ;
        !.InstmapDelta = reachable(InstmappingDelta0),

        % Get the initial inst from the InstMap.
        instmap_lookup_var(InstMap, Var, OldInst),

        % Compute the new inst by taking the old inst, applying the instmap
        % delta to it, and then unifying with bound(ConsId, ...).
        ( if map.search(InstmappingDelta0, Var, NewInst0) then
            NewInst1 = NewInst0
        else
            NewInst1 = OldInst
        ),
        bind_inst_to_functor(Type, ConsId, NewInst1, NewInst, !ModuleInfo),

        % Add `Var :: OldInst -> NewInst' to the instmap delta.
        ( if NewInst = OldInst then
            true
        else
            instmap_delta_set_var(Var, NewInst, !InstmapDelta)
        )
    ).

instmap_delta_bind_var_to_functors(Var, Type, MainConsId, OtherConsIds,
        InstMap, !InstmapDelta, !ModuleInfo) :-
    (
        !.InstmapDelta = unreachable
    ;
        !.InstmapDelta = reachable(InstmappingDelta0),

        % Get the initial inst from the InstMap.
        instmap_lookup_var(InstMap, Var, OldInst),

        % Compute the new inst by taking the old inst, applying the instmap
        % delta to it, and then unifying with bound(MainConsId, ...).
        ( if map.search(InstmappingDelta0, Var, NewInst0) then
            NewInst1 = NewInst0
        else
            NewInst1 = OldInst
        ),
        bind_inst_to_functors(Type, MainConsId, OtherConsIds,
            NewInst1, NewInst, !ModuleInfo),

        % Add `Var :: OldInst -> NewInst' to the instmap delta.
        ( if NewInst = OldInst then
            true
        else
            instmap_delta_set_var(Var, NewInst, !InstmapDelta)
        )
    ).

%---------------------------------------------------------------------------%

pre_lambda_update(ModuleInfo, Vars, Modes, InstMap0, InstMap) :-
    mode_list_get_initial_insts(ModuleInfo, Modes, Insts),
    assoc_list.from_corresponding_lists(Vars, Insts, VarInsts),
    InstMapDelta = instmap_delta_from_assoc_list(VarInsts),
    apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

%---------------------------------------------------------------------------%

compute_instmap_delta(InstMapA, InstMapB, NonLocals, InstMap) :-
    (
        InstMapA = unreachable,
        InstMap = unreachable
    ;
        InstMapA = reachable(_),
        InstMapB = unreachable,
        InstMap = unreachable
    ;
        InstMapA = reachable(InstMappingA),
        InstMapB = reachable(InstMappingB),
        set_of_var.to_sorted_list(NonLocals, NonLocalsList),
        compute_instmap_delta_for_vars(NonLocalsList,
            InstMappingA, InstMappingB, AssocList),
        map.from_sorted_assoc_list(AssocList, DeltaInstMap),
        InstMap = reachable(DeltaInstMap)
    ).

:- pred compute_instmap_delta_for_vars(list(prog_var)::in, instmapping::in,
    instmapping::in, assoc_list(prog_var, mer_inst)::out) is det.

compute_instmap_delta_for_vars([], _, _, []).
compute_instmap_delta_for_vars([Var | Vars], InstMappingA, InstMappingB,
        AssocList) :-
    compute_instmap_delta_for_vars(Vars, InstMappingA, InstMappingB,
        AssocListTail),
    instmapping_lookup_var(InstMappingA, Var, InstA),
    instmapping_lookup_var(InstMappingB, Var, InstB),
    ( if InstA = InstB then
        AssocList = AssocListTail
    else
        AssocList = [Var - InstB | AssocListTail]
    ).

%---------------------------------------------------------------------------%

apply_instmap_delta(_, unreachable, unreachable).
apply_instmap_delta(unreachable, reachable(_), unreachable).
apply_instmap_delta(reachable(InstMappingDelta),
        reachable(InstMapping0), reachable(InstMapping)) :-
    map.overlay(InstMapping0, InstMappingDelta, InstMapping).

instmap_delta_apply_instmap_delta(InstMap1, InstMap2, How, InstMap) :-
    (
        InstMap1 = unreachable,
        InstMap = unreachable
    ;
        InstMap1 = reachable(_),
        InstMap2 = unreachable,
        InstMap = unreachable
    ;
        InstMap1 = reachable(InstMappingDelta1),
        InstMap2 = reachable(InstMappingDelta2),
        (
            How = large_base,
            map.overlay(InstMappingDelta1, InstMappingDelta2,
                InstMappingDelta)
        ;
            How = large_overlay,
            map.overlay_large_map(InstMappingDelta1,
                InstMappingDelta2, InstMappingDelta)
        ;
            How = test_size,
            ( if
                map.count(InstMappingDelta1, Count1),
                map.count(InstMappingDelta2, Count2),
                Count1 >= Count2
            then
                map.overlay(InstMappingDelta1,
                    InstMappingDelta2, InstMappingDelta)
            else
                map.overlay_large_map(InstMappingDelta1,
                    InstMappingDelta2, InstMappingDelta)
            )
        ),
        InstMap = reachable(InstMappingDelta)
    ).

%---------------------------------------------------------------------------%

make_arm_instmaps_for_goals([], [], []).
make_arm_instmaps_for_goals([], [_ | _], _) :-
    unexpected($pred, "mismatched lists").
make_arm_instmaps_for_goals([_ | _], [], _) :-
    unexpected($pred, "mismatched lists").
make_arm_instmaps_for_goals([Goal | Goals], [InstMap | InstMaps],
        [ArmInfo | ArmInfos]) :-
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    ArmInfo = arm_instmap(Context, InstMap),
    make_arm_instmaps_for_goals(Goals, InstMaps, ArmInfos).

make_arm_instmaps_for_cases([], [], []).
make_arm_instmaps_for_cases([], [_ | _], _) :-
    unexpected($pred, "mismatched lists").
make_arm_instmaps_for_cases([_ | _], [], _) :-
    unexpected($pred, "mismatched lists").
make_arm_instmaps_for_cases([Case | Cases], [InstMap | InstMaps],
        [ArmInfo | ArmInfos]) :-
    Case = case(_, _, Goal),
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    ArmInfo = arm_instmap(Context, InstMap),
    make_arm_instmaps_for_cases(Cases, InstMaps, ArmInfos).

%---------------------------------------------------------------------------%

instmap_merge(NonLocals, ArmInstMaps, MergeContext, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),
    get_reachable_instmaps(ArmInstMaps, ReachableInstMappingList),
    ( if
        % We can reach the code after the branched control structure only if
        % (a) we can reach its start, and (b) some branch can reach the end.
        InstMap0 = reachable(InstMapping0),
        ReachableInstMappingList = [_ | _]
    then
        set_of_var.to_sorted_list(NonLocals, NonLocalsList),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        merge_insts_of_vars(NonLocalsList, ArmInstMaps, VarTypes,
            InstMapping0, InstMapping, ModuleInfo0, ModuleInfo, ErrorList),
        mode_info_set_module_info(ModuleInfo, !ModeInfo),
        (
            ErrorList = [FirstError | _],
            FirstError = merge_error(Var, _),
            WaitingVars = set_of_var.make_singleton(Var),
            mode_info_error(WaitingVars,
                mode_error_disj(MergeContext, ErrorList), !ModeInfo)
        ;
            ErrorList = []
        ),
        InstMap = reachable(InstMapping)
    else
        InstMap = unreachable
    ),
    mode_info_set_instmap(InstMap, !ModeInfo).

:- pred get_reachable_instmaps(list(arm_instmap)::in,
    list(instmapping)::out) is det.

get_reachable_instmaps([], []).
get_reachable_instmaps([ArmInstMap | ArmInstMaps], Reachables) :-
    ArmInstMap = arm_instmap(_, InstMap),
    (
        InstMap = reachable(InstMapping),
        get_reachable_instmaps(ArmInstMaps, ReachablesTail),
        Reachables = [InstMapping | ReachablesTail]
    ;
        InstMap = unreachable,
        get_reachable_instmaps(ArmInstMaps, Reachables)
    ).

%---------------------%

    % merge_insts_of_vars(Vars, ArmInstMaps, VarTypes, !InstMapping,
    %   !ModuleInfo, Errors):
    %
    % Given Vars, a list of variables, and ArmInstMaps, a list containing
    % instmaps giving the insts of those variables (and possibly others)
    % at the ends of a branched control structure such as a disjunction or
    % if-then-else, update !InstMapping, which initially gives the insts of
    % variables at the start of the branched control structure, to reflect
    % their insts at its end.
    %
    % For variables mentioned in Vars, merge their insts and put the merged
    % inst into !:InstMapping. For variables not in Vars, leave their insts in
    % !.InstMapping alone.
    %
    % If some variables in Vars have incompatible insts in two or more instmaps
    % in InstMapList, return them in Errors.
    %
:- pred merge_insts_of_vars(list(prog_var)::in, list(arm_instmap)::in,
    vartypes::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out, list(merge_error)::out) is det.

merge_insts_of_vars([], _, _, !InstMap, !ModuleInfo, []).
merge_insts_of_vars([Var | Vars], ArmInstMaps, VarTypes, !InstMapping,
        !ModuleInfo, !:ErrorList) :-
    merge_insts_of_vars(Vars, ArmInstMaps, VarTypes, !InstMapping,
        !ModuleInfo, !:ErrorList),
    lookup_var_type(VarTypes, Var, VarType),
    list.map(lookup_var_in_arm_instmap(Var), ArmInstMaps, VarInsts),
    det_head_tail(VarInsts, HeadVarInst, TailVarInsts), 
    merge_var_insts(yes(VarType), HeadVarInst, TailVarInsts, MaybeInst,
        !ModuleInfo),
    (
        MaybeInst = no,
        list.map(arm_instmap_project_context, ArmInstMaps, Contexts),
        assoc_list.from_corresponding_lists(Contexts, VarInsts, ContextsInsts),
        !:ErrorList = [merge_error(Var, ContextsInsts) | !.ErrorList],
        map.set(Var, not_reached, !InstMapping)
    ;
        MaybeInst = yes(Inst),
        map.set(Var, Inst, !InstMapping)
    ).

:- pred lookup_var_in_arm_instmap(prog_var::in, arm_instmap::in,
    mer_inst::out) is det.

lookup_var_in_arm_instmap(Var, ArmInstMap, Inst) :-
    ArmInstMap = arm_instmap(_, InstMap),
    instmap_lookup_var(InstMap, Var, Inst).

:- pred arm_instmap_project_context(arm_instmap::in, prog_context::out) is det.

arm_instmap_project_context(ArmErrorInfo, Context) :-
    ArmErrorInfo = arm_instmap(Context, _InstMap).

    % merge_var_insts(Insts, Type, !ModuleInfo, MaybeMergedInst):
    %
    % Given a list of insts of a given variable that reflect the inst of that
    % variable at the ends of a branched control structure such as a
    % disjunction or if-then-else, return either `yes(MergedInst)' where
    % MergedInst is the final inst of that variable after the branched control
    % structure as a whole, or `no' if some of the insts are not compatible.
    %
    % We used to use a straightforward algorithm that, given a list of N insts,
    % merged the tail N-1 insts, and merged the result with the head inst.
    % While this is simple and efficient for small N, it has very bad
    % performance for large N. The reason is that its complexity can be N^2,
    % since in many cases each arm of the branched control structure binds
    % the variable to a different function symbol, and this means that the
    % merged inst evolves like this:
    %
    %   bound(f)
    %   bound(f; g)
    %   bound(f; g; h)
    %   bound(f; g; h; i)
    %
    % Our current algorithm uses a number of passes, each of which divides the
    % number of insts by four by merging groups of four adjacent insts.
    % The overall complexity is thus closer to N log N than N^2.
    %
:- pred merge_var_insts(maybe(mer_type)::in, mer_inst::in, list(mer_inst)::in,
    maybe(mer_inst)::out, module_info::in, module_info::out) is det.

merge_var_insts(MaybeType, HeadInst, TailInsts, MaybeMergedInst,
        !ModuleInfo) :-
    merge_var_insts_pass(MaybeType, HeadInst, TailInsts,
        [], MergedInsts, no, Error, !ModuleInfo),
    (
        Error = yes,
        MaybeMergedInst = no
    ;
        Error = no,
        (
            MergedInsts = [],
            % merge_var_insts_pass can return MergedInsts = [], but only
            % together with Error = yes.
            unexpected($pred, "MergedInsts = []")
        ;
            MergedInsts = [MergedInst1 | MergedInsts2Plus],
            (
                MergedInsts2Plus = [],
                MaybeMergedInst = yes(MergedInst1)
            ;
                MergedInsts2Plus = [_ | _],
                merge_var_insts(MaybeType, MergedInst1, MergedInsts2Plus,
                    MaybeMergedInst, !ModuleInfo)
            )
        )
    ).

:- pred merge_var_insts_pass(maybe(mer_type)::in,
    mer_inst::in, list(mer_inst)::in,
    list(mer_inst)::in, list(mer_inst)::out,
    bool::in, bool::out, module_info::in, module_info::out) is det.

merge_var_insts_pass(MaybeType, Inst1, Insts2Plus, !MergedInsts, !Error,
        !ModuleInfo) :-
    (
        Insts2Plus = [],
        !:MergedInsts = [Inst1 | !.MergedInsts]
    ;
        Insts2Plus = [Inst2],
        ( if
            inst_merge(Inst1, Inst2, MaybeType, Inst12, !ModuleInfo)
        then
            !:MergedInsts = [Inst12 | !.MergedInsts]
        else
            !:Error = yes
        )
    ;
        Insts2Plus = [Inst2, Inst3],
        ( if
            inst_merge(Inst1, Inst2, MaybeType, Inst12, !ModuleInfo),
            inst_merge(Inst12, Inst3, MaybeType, Inst123, !ModuleInfo)
        then
            !:MergedInsts = [Inst123 | !.MergedInsts]
        else
            !:Error = yes
        )
    ;
        Insts2Plus = [Inst2, Inst3, Inst4 | Insts5Plus],
        ( if
            inst_merge(Inst1, Inst2, MaybeType, Inst12, !ModuleInfo),
            inst_merge(Inst3, Inst4, MaybeType, Inst34, !ModuleInfo),
            inst_merge(Inst12, Inst34, MaybeType, Inst1234, !ModuleInfo)
        then
            !:MergedInsts = [Inst1234 | !.MergedInsts],
            (
                Insts5Plus = []
            ;
                Insts5Plus = [Inst5 | Insts6Plus],
                merge_var_insts_pass(MaybeType, Inst5, Insts6Plus,
                    !MergedInsts, !Error, !ModuleInfo)
            )
        else
            !:Error = yes
        )
    ).

%---------------------------------------------------------------------------%

merge_instmap_delta(InstMap, NonLocals, VarTypes,
        MaybeInstMappingA, MaybeInstMappingB, MaybeInstMapping, !ModuleInfo) :-
    (
        MaybeInstMappingA = unreachable,
        MaybeInstMapping = MaybeInstMappingB
    ;
        MaybeInstMappingA = reachable(InstMappingA),
        (
            MaybeInstMappingB = unreachable,
            MaybeInstMapping = MaybeInstMappingA
        ;
            MaybeInstMappingB = reachable(InstMappingB),
            merge_instmapping_delta(InstMap, NonLocals, VarTypes,
                InstMappingA, InstMappingB, InstMapping, !ModuleInfo),
            MaybeInstMapping = reachable(InstMapping)
        )
    ).

:- pred merge_instmapping_delta(instmap::in, set_of_progvar::in, vartypes::in,
    instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

merge_instmapping_delta(InstMap, NonLocals, VarTypes,
        InstMappingA, InstMappingB, InstMapping, !ModuleInfo) :-
    map.keys(InstMappingA, VarsInA),
    map.keys(InstMappingB, VarsInB),
    set_of_var.sorted_list_to_set(VarsInA, SetofVarsInA),
    set_of_var.sorted_list_to_set(VarsInB, SetofVarsInB),
    set_of_var.union(SetofVarsInA, SetofVarsInB, SetofVars0),
    set_of_var.intersect(SetofVars0, NonLocals, SetofVars),
    set_of_var.to_sorted_list(SetofVars, ListofVars),
    merge_instmapping_delta_vars(ListofVars, InstMap, VarTypes,
        InstMappingA, InstMappingB, map.init, InstMapping, !ModuleInfo).

:- pred merge_instmapping_delta_vars(list(prog_var)::in, instmap::in,
    vartypes::in, instmapping::in, instmapping::in,
    instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

merge_instmapping_delta_vars([], _, _, _, _, !InstMapping, !ModuleInfo).
merge_instmapping_delta_vars([Var | Vars], InstMap, VarTypes,
        InstMappingA, InstMappingB, !InstMapping, !ModuleInfo) :-
    ( if map.search(InstMappingA, Var, InstInA) then
        InstA = InstInA
    else
        instmap_lookup_var(InstMap, Var, InstA)
    ),
    ( if map.search(InstMappingB, Var, InstInB) then
        InstB = InstInB
    else
        instmap_lookup_var(InstMap, Var, InstB)
    ),
    lookup_var_type(VarTypes, Var, VarType),
    ( if inst_merge(InstA, InstB, yes(VarType), InstAB, !ModuleInfo) then
        % XXX Given instmap_lookup_var(InstMap, Var, OldInst),
        % we should probably set Inst not directly from InstAB, but
        % from a conjunction of OldInst and InstAB. If OldInst says that
        % Var is bound to f, and InstAB says that it is bound to g,
        % Inst should be `unreachable', not bound(g). If OldInst says
        % that Var is bound to f or g, and InstAB says that it is bound
        % to g or h, Inst should say that it is bound(g).
        %
        % If there is an invariant to the effect that such situations
        % are not supposed to arise, then it is being broken, due to
        % the XXX in recompute_instmap_delta_unify in mode_util.m.
        %
        % At present, I believe that the cases we mishandle here can
        % arise only after inlining, as in puzzle_detism_bug.m in
        % tests/hard_coded. -zs

        Inst = InstAB,
        map.det_insert(Var, Inst, !InstMapping)
    else
        term.var_to_int(Var, VarInt),
        string.format("error merging var %i", [i(VarInt)], Msg),
        unexpected($pred, Msg)
    ),
    merge_instmapping_delta_vars(Vars, InstMap, VarTypes,
        InstMappingA, InstMappingB, !InstMapping, !ModuleInfo).

%---------------------------------------------------------------------------%

merge_instmap_deltas(InstMap, NonLocals, VarTypes, Deltas,
        MergedDelta, !ModuleInfo) :-
    % We use the same technique for merge_instmap_deltas as for merge_var,
    % and for the same reason.
    merge_instmap_deltas_2(InstMap, NonLocals, VarTypes, Deltas,
        [], MergedDeltas, !ModuleInfo),
    (
        MergedDeltas = [],
        unexpected($pred, "empty instmap_delta list.")
    ;
        MergedDeltas = [MergedDelta]
    ;
        MergedDeltas = [_, _ | _],
        merge_instmap_deltas(InstMap, NonLocals, VarTypes, MergedDeltas,
            MergedDelta, !ModuleInfo)
    ).

:- pred merge_instmap_deltas_2(instmap::in, set_of_progvar::in, vartypes::in,
    list(instmap_delta)::in, list(instmap_delta)::in, list(instmap_delta)::out,
    module_info::in, module_info::out) is det.

merge_instmap_deltas_2(InstMap, NonLocals, VarTypes, Deltas,
        !MergedDeltas, !ModuleInfo) :-
    (
        Deltas = []
    ;
        Deltas = [Delta1],
        !:MergedDeltas = [Delta1 | !.MergedDeltas]
    ;
        Deltas = [Delta1, Delta2],
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta1, Delta2,
            Delta12, !ModuleInfo),
        !:MergedDeltas = [Delta12 | !.MergedDeltas]
    ;
        Deltas = [Delta1, Delta2, Delta3],
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta1, Delta2,
            Delta12, !ModuleInfo),
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta12, Delta3,
            Delta123, !ModuleInfo),
        !:MergedDeltas = [Delta123 | !.MergedDeltas]
    ;
        Deltas = [Delta1, Delta2, Delta3, Delta4 | MoreDeltas],
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta1, Delta2,
            Delta12, !ModuleInfo),
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta3, Delta4,
            Delta34, !ModuleInfo),
        merge_instmap_delta(InstMap, NonLocals, VarTypes, Delta12, Delta34,
            Delta1234, !ModuleInfo),
        !:MergedDeltas = [Delta1234 | !.MergedDeltas],
        merge_instmap_deltas_2(InstMap, NonLocals, VarTypes, MoreDeltas,
            !MergedDeltas, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%

unify_instmap_delta(_, _, unreachable, InstMapDelta, InstMapDelta,
        !ModuleInfo).
unify_instmap_delta(_, _, reachable(InstMapping), unreachable,
        reachable(InstMapping), !ModuleInfo).
unify_instmap_delta(InstMap, NonLocals, reachable(InstMappingA),
        reachable(InstMappingB), reachable(InstMapping), !ModuleInfo) :-
    unify_instmapping_delta(InstMap, NonLocals, InstMappingA, InstMappingB,
        InstMapping, !ModuleInfo).

:- pred unify_instmapping_delta(instmap::in, set_of_progvar::in,
    instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

unify_instmapping_delta(InstMap, NonLocals, InstMappingA, InstMappingB,
        InstMapping, !ModuleInfo) :-
    map.keys(InstMappingA, VarsInA),
    map.keys(InstMappingB, VarsInB),
    set_of_var.sorted_list_to_set(VarsInA, SetofVarsInA),
    set_of_var.insert_list(VarsInB, SetofVarsInA, SetofVars0),
    set_of_var.intersect(SetofVars0, NonLocals, SetofVars),
    set_of_var.to_sorted_list(SetofVars, ListofVars),
    unify_instmapping_delta_2(ListofVars, InstMap, InstMappingA, InstMappingB,
        map.init, InstMapping, !ModuleInfo).

:- pred unify_instmapping_delta_2(list(prog_var)::in, instmap::in,
    instmapping::in, instmapping::in, instmapping::in, instmapping::out,
    module_info::in, module_info::out) is det.

unify_instmapping_delta_2([], _, _, _, !InstMapping, !ModuleInfo).
unify_instmapping_delta_2([Var | Vars], InstMap, InstMappingA, InstMappingB,
        !InstMapping, !ModuleInfo) :-
    ( if map.search(InstMappingA, Var, InstA) then
        ( if map.search(InstMappingB, Var, InstB) then
            ( if
                % We can ignore the determinism of the unification: if it
                % isn't det, then there will be a mode error or a determinism
                % error in one of the parallel conjuncts.

                abstractly_unify_inst(is_live, InstA, InstB,
                    fake_unify, Inst, _Det, !ModuleInfo)
            then
                map.det_insert(Var, Inst, !InstMapping)
            else
                unexpected($pred, "unexpected error")
            )
        else
            map.det_insert(Var, InstA, !InstMapping)
        )
    else
        ( if map.search(InstMappingB, Var, InstB) then
            map.det_insert(Var, InstB, !InstMapping)
        else
            true
        )
    ),
    unify_instmapping_delta_2(Vars, InstMap, InstMappingA, InstMappingB,
        !InstMapping, !ModuleInfo).

%---------------------------------------------------------------------------%

instmap_apply_sub(Must, Renaming, InstMap0, InstMap) :-
    instmap_delta_apply_sub(Must, Renaming, InstMap0, InstMap).

instmap_delta_apply_sub(_Must, _Renaming, unreachable, unreachable).
instmap_delta_apply_sub(Must, Renaming,
        reachable(OldInstMapping), reachable(InstMapping)) :-
    map.to_assoc_list(OldInstMapping, InstMappingAL),
    instmapping_apply_sub_loop(InstMappingAL, Must, Renaming,
        map.init, InstMapping).

:- pred instmapping_apply_sub_loop(assoc_list(prog_var, mer_inst)::in,
    must_rename::in, map(prog_var, prog_var)::in,
    instmapping::in, instmapping::out) is det.

instmapping_apply_sub_loop([], _Must, _Renaming, !Instmapping).
instmapping_apply_sub_loop([Var0 - Inst | VarInsts0], Must, Renaming,
        !Instmapping) :-
    rename_var(Must, Renaming, Var0, Var),
    % XXX temporary hack alert XXX
    % This should be a call to map.det_insert, rather than to map.set.
    % However, if we do that, then the compiler breaks, due to a problem
    % with excess.m not preserving super-homogenous form.
    map.set(Var, Inst, !Instmapping),
    instmapping_apply_sub_loop(VarInsts0, Must, Renaming, !Instmapping).

%---------------------------------------------------------------------------%

instmap_delta_map_foldl(_, unreachable, unreachable, !T).
instmap_delta_map_foldl(P, reachable(Instmapping0), reachable(Instmapping),
        !T) :-
    map.map_foldl(P, Instmapping0, Instmapping, !T).

%---------------------------------------------------------------------------%
:- end_module hlds.instmap.
%---------------------------------------------------------------------------%
