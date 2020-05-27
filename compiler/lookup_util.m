%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: lookup_util.m.
% Author: zs.
%
% Utility predicates used by both lookup_switch.m and disj.m. These utility
% predicates help in the implementation of switches and disjunctions in which
% the code of each arm consists only of looking up the values of the output
% variables in a table.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.lookup_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module list.

    % Figure out which variables are bound in the goal.
    % We do this by using the current instmap and the instmap delta in the
    % goal info to work out which variables are [further] bound by the goal.
    %
:- pred figure_out_output_vars(code_info::in, code_loc_dep::in,
    hlds_goal_info::in, list(prog_var)::out) is det.

    % To figure out if the outputs are constants, we
    %
    % - check whether the determinism and structure of the goal are right,
    % - generate code for the case,
    % - check to see if each of the output vars is a constant,
    % - check to see that no actual code was generated.
    %
    % For large goals, step 2 can be expensive. Step 1 is a cheap way of
    % finding out whether steps 3 and 4 would fail without first running
    % step 2. Therefore the caller should call goal_is_conj_of_unify (which
    % does step 1) on Goal before calling generate_constants_for_arm (which
    % does steps 2, 3 and 4).
    %
:- pred generate_constants_for_arm(position_info::in, hlds_goal::in,
    list(prog_var)::in, abs_store_map::in, list(rval)::out,
    branch_end::in, branch_end::out, set_of_progvar::out,
    code_info::in, code_info::out) is semidet.

:- pred generate_constants_for_disjunct(position_info::in,
    hlds_goal::in, list(prog_var)::in, abs_store_map::in,
    list(rval)::out, branch_end::in, branch_end::out, set_of_progvar::out,
    code_info::in, code_info::out) is semidet.

:- pred generate_constants_for_disjuncts(position_info::in,
    list(hlds_goal)::in, list(prog_var)::in, abs_store_map::in,
    list(list(rval))::out, branch_end::in, branch_end::out,
    code_info::in, code_info::out) is semidet.

    % set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd, Code,
    %   !CI, !.CLD):
    %
    % Set the liveness to Liveness, move all the variables listed in StoreMap
    % to their indicated locations, and end the current branch, updating
    % !MaybeEnd in the process.
    %
:- pred set_liveness_and_end_branch(abs_store_map::in, set_of_progvar::in,
    branch_end::in, branch_end::out, llds_code::out,
    code_info::in, code_loc_dep::in) is det.

:- pred generate_offset_assigns(list(prog_var)::in, int::in, lval::in,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.exprn_aux.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module require.

figure_out_output_vars(CI, CLD, GoalInfo, OutVars) :-
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    ( if instmap_delta_is_unreachable(InstMapDelta) then
        OutVars = []
    else
        get_instmap(CLD, CurrentInstMap),
        get_module_info(CI, ModuleInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        apply_instmap_delta(InstMapDelta, CurrentInstMap, InstMapAfter),
        list.filter(is_output_var(ModuleInfo, CurrentInstMap, InstMapAfter),
            set_of_var.to_sorted_list(ChangedVars), OutVars)
    ).

:- pred is_output_var(module_info::in, instmap::in, instmap::in, prog_var::in)
    is semidet.

is_output_var(ModuleInfo, CurrentInstMap, InstMapAfter, Var) :-
    % If a variable has a final inst, then it changed
    % instantiatedness during the switch.
    instmap_lookup_var(CurrentInstMap, Var, Initial),
    instmap_lookup_var(InstMapAfter, Var, Final),
    mode_is_output(ModuleInfo, from_to_mode(Initial, Final)).

generate_constants_for_arm(BranchStart, Goal, Vars, StoreMap, !MaybeEnd,
        CaseRvals, Liveness, !CI) :-
    do_generate_constants_for_arm(BranchStart, Goal, Vars, StoreMap,
        no, !MaybeEnd, CaseRvals, Liveness, !CI).

:- pred do_generate_constants_for_arm(position_info::in, hlds_goal::in,
    list(prog_var)::in, abs_store_map::in, bool::in, list(rval)::out,
    branch_end::in, branch_end::out, set_of_progvar::out,
    code_info::in, code_info::out) is semidet.

do_generate_constants_for_arm(BranchStart, Goal, Vars, StoreMap, SetToUnknown,
        CaseRvals, !MaybeEnd, Liveness, !CI) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    CodeModel = goal_info_get_code_model(GoalInfo),
    some [!CLD] (
        reset_to_position(BranchStart, !.CI, !:CLD),
        code_gen.generate_goal(CodeModel, Goal, Code, !CI, !CLD),
        cord.is_empty(Code),
        get_forward_live_vars(!.CLD, Liveness),

        get_exprn_opts(!.CI, ExprnOpts),
        get_arm_rvals(Vars, CaseRvals, !.CI, !CLD, ExprnOpts),
        (
            SetToUnknown = no
        ;
            SetToUnknown = yes,
            set_resume_point_to_unknown(!CLD)
        ),
        % EndCode code may contain instructions that place Vars in the
        % locations dictated by StoreMap, and thus does not have to be empty.
        % (The array lookup code will put those variables in those locations
        % directly.)
        generate_branch_end(StoreMap, !MaybeEnd, _EndCode, !.CI, !.CLD)
    ).

generate_constants_for_disjunct(BranchStart, Disjunct0, Vars, StoreMap, Soln,
        !MaybeEnd, Liveness, !CI) :-
    % The pre_goal_update sanity check insists on no_resume_point, to ensure
    % that all resume points have been handled by surrounding code.
    Disjunct0 = hlds_goal(DisjunctGoalExpr, DisjunctGoalInfo0),
    goal_info_set_resume_point(no_resume_point,
        DisjunctGoalInfo0, DisjunctGoalInfo),
    Disjunct = hlds_goal(DisjunctGoalExpr, DisjunctGoalInfo),
    do_generate_constants_for_arm(BranchStart, Disjunct, Vars, StoreMap,
        yes, Soln, !MaybeEnd, Liveness, !CI).

generate_constants_for_disjuncts(_StartPos, [], _Vars,
        _StoreMap, [], !MaybeEnd, !CI).
generate_constants_for_disjuncts(StartPos, [Disjunct0 | Disjuncts0], Vars,
        StoreMap, [Soln | Solns], !MaybeEnd, !CI) :-
    generate_constants_for_disjunct(StartPos, Disjunct0, Vars,
        StoreMap, Soln, !MaybeEnd, _Liveness, !CI),
    generate_constants_for_disjuncts(StartPos, Disjuncts0, Vars,
        StoreMap, Solns, !MaybeEnd, !CI).

%---------------------------------------------------------------------------%

:- pred get_arm_rvals(list(prog_var)::in, list(rval)::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out, exprn_opts::in)
    is semidet.

get_arm_rvals([], [], _CI, !CLD, _ExprnOpts).
get_arm_rvals([Var | Vars], [Rval | Rvals], CI, !CLD, ExprnOpts) :-
    produce_variable(Var, Code, Rval, CI, !CLD),
    cord.is_empty(Code),
    rval_is_constant(Rval, ExprnOpts),
    get_arm_rvals(Vars, Rvals, CI, !CLD, ExprnOpts).

    % rval_is_constant(Rval, ExprnOpts) is true iff Rval is a constant.
    % This depends on the options governing nonlocal gotos, asm labels enabled
    % and static ground terms, etc.
    %
:- pred rval_is_constant(rval::in, exprn_opts::in) is semidet.

rval_is_constant(const(Const), ExprnOpts) :-
    exprn_aux.const_is_constant(Const, ExprnOpts, yes).
rval_is_constant(unop(_, Exprn), ExprnOpts) :-
    rval_is_constant(Exprn, ExprnOpts).
rval_is_constant(binop(_, Exprn0, Exprn1), ExprnOpts) :-
    rval_is_constant(Exprn0, ExprnOpts),
    rval_is_constant(Exprn1, ExprnOpts).
rval_is_constant(mkword(_, Exprn0), ExprnOpts) :-
    rval_is_constant(Exprn0, ExprnOpts).

%---------------------------------------------------------------------------%

set_liveness_and_end_branch(StoreMap, Liveness, !MaybeEnd, BranchEndCode,
        CI, !.CLD) :-
    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail' slot
    % last would yield the wrong liveness. Also, by killing the variables
    % that are not live anymore, we avoid generating code that moves their
    % values aside.
    get_forward_live_vars(!.CLD, OldLiveness),
    set_forward_live_vars(Liveness, !CLD),
    set_of_var.difference(OldLiveness, Liveness, DeadVars),
    maybe_make_vars_forward_dead(DeadVars, no, !CLD),
    generate_branch_end(StoreMap, !MaybeEnd, BranchEndCode, CI, !.CLD).

generate_offset_assigns([], _, _, _CI, !CLD).
generate_offset_assigns([Var | Vars], Offset, BaseReg, CI, !CLD) :-
    LookupLval = field(yes(ptag(0u8)), lval(BaseReg),
        const(llconst_int(Offset))),
    assign_lval_to_var(Var, LookupLval, Code, CI, !CLD),
    expect(cord.is_empty(Code), $pred, "nonempty code"),
    generate_offset_assigns(Vars, Offset + 1, BaseReg, CI, !CLD).

%---------------------------------------------------------------------------%
:- end_module ll_backend.lookup_util.
%---------------------------------------------------------------------------%
