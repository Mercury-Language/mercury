%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: commit_gen.m.
% Main authors: conway, fjh, zs.
%
% The predicates of this module generate code for performing commits.
%
%---------------------------------------------------------------------------%

:- module ll_backend.commit_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.set_of_var.

%---------------------------------------------------------------------------%

:- pred generate_scope(scope_reason::in, code_model::in, hlds_goal_info::in,
    set_of_progvar::in, hlds_goal::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module ll_backend.code_gen.
% :- import_module parse_tree.prog_data.
% :- import_module set.

:- import_module cord.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

generate_scope(Reason, OuterCodeModel, OuterGoalInfo,
        ForwardLiveVarsBeforeGoal, Goal, Code, !CI, !CLD) :-
    ( if
        Reason = trace_goal(_, MaybeTraceRuntimeCond, _, _, _),
        MaybeTraceRuntimeCond = yes(_)
    then
        % These goals should have been transformed into other forms of goals
        % by simplify.m at the end of semantic analysis.
        unexpected($pred, "trace_goal")
    else
        generate_commit(OuterCodeModel, OuterGoalInfo,
            ForwardLiveVarsBeforeGoal, Goal, Code, !CI, !CLD)
    ).

:- pred generate_commit(code_model::in, hlds_goal_info::in, set_of_progvar::in,
    hlds_goal::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_commit(OuterCodeModel, OuterGoalInfo, ForwardLiveVarsBeforeGoal,
        Goal, Code, !CI, !CLD) :-
    AddTrailOps = should_add_trail_ops(!.CI, OuterGoalInfo),
    AddRegionOps = should_add_region_ops(!.CI, OuterGoalInfo),

    Goal = hlds_goal(_, InnerGoalInfo),
    InnerCodeModel = goal_info_get_code_model(InnerGoalInfo),
    (
        OuterCodeModel = model_det,
        (
            InnerCodeModel = model_det,
            code_gen.generate_goal(InnerCodeModel, Goal, Code, !CI, !CLD)
        ;
            InnerCodeModel = model_semi,
            unexpected($pred, "semidet model in det context")
        ;
            InnerCodeModel = model_non,
            prepare_for_det_commit(AddTrailOps, AddRegionOps,
                ForwardLiveVarsBeforeGoal, InnerGoalInfo, CommitInfo,
                PreCommit, !CI, !CLD),
            code_gen.generate_goal(InnerCodeModel, Goal, GoalCode, !CI, !CLD),
            generate_det_commit(CommitInfo, Commit, !CI, !CLD),
            Code = PreCommit ++ GoalCode ++ Commit
        )
    ;
        OuterCodeModel = model_semi,
        (
            InnerCodeModel = model_det,
            code_gen.generate_goal(InnerCodeModel, Goal, Code, !CI, !CLD)
        ;
            InnerCodeModel = model_semi,
            code_gen.generate_goal(InnerCodeModel, Goal, Code, !CI, !CLD)
        ;
            InnerCodeModel = model_non,
            prepare_for_semi_commit(AddTrailOps, AddRegionOps,
                ForwardLiveVarsBeforeGoal, InnerGoalInfo, CommitInfo,
                PreCommit, !CI, !CLD),
            code_gen.generate_goal(InnerCodeModel, Goal, GoalCode, !CI, !CLD),
            generate_semi_commit(CommitInfo, Commit, !CI, !CLD),
            Code = PreCommit ++ GoalCode ++ Commit
        )
    ;
        OuterCodeModel = model_non,
        code_gen.generate_goal(InnerCodeModel, Goal, Code, !CI, !CLD)
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.commit_gen.
%---------------------------------------------------------------------------%
