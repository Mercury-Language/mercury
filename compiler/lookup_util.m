%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
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

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module set.

    % Figure out which variables are bound in the goal.
    % We do this by using the current instmap and the instmap delta in the
    % goal info to work out which variables are [further] bound by the goal.
    %
:- pred figure_out_output_vars(code_info::in, hlds_goal_info::in,
    list(prog_var)::out) is det.

    % Is the input goal a conjunction of unifications?
    %
:- pred goal_is_conj_of_unify(hlds_goal::in) is semidet.

    % Run goal_is_conj_of_unify on each goal in the list.
    %
:- pred all_disjuncts_are_conj_of_unify(list(hlds_goal)::in) is semidet.

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
:- pred generate_constants_for_arm(hlds_goal::in, list(prog_var)::in,
    abs_store_map::in, list(rval)::out, branch_end::in, branch_end::out,
    set(prog_var)::out, code_info::in, code_info::out) is semidet.

:- pred generate_constants_for_disjuncts(list(hlds_goal)::in,
    list(prog_var)::in, abs_store_map::in, list(list(rval))::out,
    branch_end::in, branch_end::out, maybe(set(prog_var))::out,
    code_info::in, code_info::out) is semidet.

:- pred set_liveness_and_end_branch(abs_store_map::in, branch_end::in,
    set(prog_var)::in, code_tree::out, code_info::in, code_info::out) is det.

:- pred generate_offset_assigns(list(prog_var)::in, int::in, lval::in,
    code_info::in, code_info::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.tree.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.exprn_aux.

:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module solutions.

figure_out_output_vars(CI, GoalInfo, OutVars) :-
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    ( instmap_delta_is_unreachable(InstMapDelta) ->
        OutVars = []
    ;
        code_info.get_instmap(CI, CurrentInstMap),
        code_info.get_module_info(CI, ModuleInfo),
        instmap_delta_changed_vars(InstMapDelta, ChangedVars),
        instmap.apply_instmap_delta(CurrentInstMap, InstMapDelta,
            InstMapAfter),
        Lambda = (pred(Var::out) is nondet :-
            % If a variable has a final inst, then it changed
            % instantiatedness during the switch.
            set.member(Var, ChangedVars),
            instmap.lookup_var(CurrentInstMap, Var, Initial),
            instmap.lookup_var(InstMapAfter, Var, Final),
            mode_is_output(ModuleInfo, (Initial -> Final))
        ),
        solutions.solutions(Lambda, OutVars)
    ).

goal_is_conj_of_unify(Goal) :-
    Goal = _GoalExpr - GoalInfo,
    goal_info_get_code_model(GoalInfo, CodeModel),
    CodeModel = model_det,
    goal_to_conj_list(Goal, Conj),
    only_constant_goals(Conj).

all_disjuncts_are_conj_of_unify([]).
all_disjuncts_are_conj_of_unify([Disjunct | Disjuncts]) :-
    goal_is_conj_of_unify(Disjunct),
    all_disjuncts_are_conj_of_unify(Disjuncts).

:- pred only_constant_goals(list(hlds_goal)::in) is semidet.

only_constant_goals([]).
only_constant_goals([Goal | Goals]) :-
    Goal = GoalExpr - _,
    % We could allow calls as well. Some procedures have an output inst
    % that fixes the value of the output variable, which is thus a constant.
    % However, calls to such procedures should have been inlined by now.
    GoalExpr = unify(_, _, _, _, _),
    only_constant_goals(Goals).

generate_constants_for_arm(Goal, Vars, StoreMap, !MaybeEnd, CaseRvals,
        Liveness, !CI) :-
    do_generate_constants_for_arm(Goal, Vars, StoreMap, no, !MaybeEnd,
        CaseRvals, Liveness, !CI).

:- pred do_generate_constants_for_arm(hlds_goal::in, list(prog_var)::in,
    abs_store_map::in, bool::in, list(rval)::out,
    branch_end::in, branch_end::out, set(prog_var)::out,
    code_info::in, code_info::out) is semidet.

do_generate_constants_for_arm(Goal, Vars, StoreMap, SetToUnknown, CaseRvals,
        !MaybeEnd, Liveness, !CI) :-
    code_info.remember_position(!.CI, BranchStart),
    Goal = _GoalExpr - GoalInfo,
    goal_info_get_code_model(GoalInfo, CodeModel),
    code_gen.generate_goal(CodeModel, Goal, Code, !CI),
    tree.tree_of_lists_is_empty(Code),
    code_info.get_forward_live_vars(!.CI, Liveness),

    code_info.get_globals(!.CI, Globals),
    globals.get_options(Globals, Options),
    exprn_aux.init_exprn_opts(Options, ExprnOpts),
    get_arm_rvals(Vars, CaseRvals, !CI, ExprnOpts),
    (
        SetToUnknown = no
    ;
        SetToUnknown = yes,
        code_info.set_resume_point_to_unknown(!CI)
    ),
    % EndCode code may contain instructions that place Vars in the locations
    % dictated by StoreMap, and thus does not have to be empty. (The array
    % lookup code will put those variables in those locations directly.)
    code_info.generate_branch_end(StoreMap, !MaybeEnd, _EndCode, !CI),
    code_info.reset_to_position(BranchStart, !CI).

generate_constants_for_disjuncts([], _Vars, _StoreMap, [], !MaybeEnd,
        no, !CI).
generate_constants_for_disjuncts([Disjunct0 | Disjuncts], Vars, StoreMap,
        [Soln | Solns], !MaybeEnd, yes(Liveness), !CI) :-
    % The pre_goal_update sanity check insists on no_resume_point, to ensure
    % that all resume points have been handled by surrounding code.
    Disjunct0 = DisjunctGoalExpr - DisjunctGoalInfo0,
    goal_info_set_resume_point(no_resume_point,
        DisjunctGoalInfo0, DisjunctGoalInfo),
    Disjunct = DisjunctGoalExpr - DisjunctGoalInfo,
    do_generate_constants_for_arm(Disjunct, Vars, StoreMap, yes, Soln,
        !MaybeEnd, Liveness, !CI),
    generate_constants_for_disjuncts(Disjuncts, Vars, StoreMap, Solns,
        !MaybeEnd, _, !CI).

%---------------------------------------------------------------------------%

:- pred get_arm_rvals(list(prog_var)::in, list(rval)::out,
    code_info::in, code_info::out, exprn_opts::in) is semidet.

get_arm_rvals([], [], !CI, _ExprnOpts).
get_arm_rvals([Var | Vars], [Rval | Rvals], !CI, ExprnOpts) :-
    code_info.produce_variable(Var, Code, Rval, !CI),
    tree.tree_of_lists_is_empty(Code),
    rval_is_constant(Rval, ExprnOpts),
    get_arm_rvals(Vars, Rvals, !CI, ExprnOpts).

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

set_liveness_and_end_branch(StoreMap, MaybeEnd0, Liveness, BranchEndCode,
        !CI) :-
    % We keep track of what variables are supposed to be live at the end
    % of cases. We have to do this explicitly because generating a `fail' slot
    % last would yield the wrong liveness.
    code_info.set_forward_live_vars(Liveness, !CI),
    code_info.generate_branch_end(StoreMap, MaybeEnd0, _MaybeEnd,
        BranchEndCode, !CI).

generate_offset_assigns([], _, _, !CI).
generate_offset_assigns([Var | Vars], Offset, BaseReg, !CI) :-
    LookupLval = field(yes(0), lval(BaseReg), const(llconst_int(Offset))),
    code_info.assign_lval_to_var(Var, LookupLval, Code, !CI),
    expect(tree.is_empty(Code), this_file,
        "generate_offset_assigns: nonempty code"),
    generate_offset_assigns(Vars, Offset + 1, BaseReg, !CI).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "lookup_util.m".

%---------------------------------------------------------------------------%
