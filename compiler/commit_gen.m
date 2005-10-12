%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-1998, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: commit_gen.m
%
% Main authors: conway, fjh, zs.
%
% The predicates of this module generate code for performing commits.
%
%---------------------------------------------------------------------------%

:- module ll_backend__commit_gen.

:- interface.

:- import_module hlds__code_model.
:- import_module hlds__hlds_goal.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.

:- pred commit_gen__generate_commit(code_model::in, hlds_goal::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- implementation.

:- import_module libs__tree.
:- import_module ll_backend__code_gen.
:- import_module parse_tree__error_util.

:- import_module require.
:- import_module std_util.

commit_gen__generate_commit(OuterCodeModel, Goal, Code, !Info) :-
    Goal = _ - InnerGoalInfo,
    goal_info_get_code_model(InnerGoalInfo, InnerCodeModel),
    (
        OuterCodeModel = model_det,
        (
            InnerCodeModel = model_det,
            code_gen__generate_goal(InnerCodeModel, Goal, Code, !Info)
        ;
            InnerCodeModel = model_semi,
            unexpected(this_file, "semidet model in det context")
        ;
            InnerCodeModel = model_non,
            code_info__prepare_for_det_commit(CommitInfo, PreCommit, !Info),
            code_gen__generate_goal(InnerCodeModel, Goal, GoalCode, !Info),
            code_info__generate_det_commit(CommitInfo, Commit, !Info),
            Code = tree(PreCommit, tree(GoalCode, Commit))
        )
    ;
        OuterCodeModel = model_semi,
        (
            InnerCodeModel = model_det,
            code_gen__generate_goal(InnerCodeModel, Goal, Code, !Info)
        ;
            InnerCodeModel = model_semi,
            code_gen__generate_goal(InnerCodeModel, Goal, Code, !Info)
        ;
            InnerCodeModel = model_non,
            code_info__prepare_for_semi_commit(CommitInfo, PreCommit, !Info),
            code_gen__generate_goal(InnerCodeModel, Goal, GoalCode, !Info),
            code_info__generate_semi_commit(CommitInfo, Commit, !Info),
            Code = tree(PreCommit, tree(GoalCode, Commit))
        )
    ;
        OuterCodeModel = model_non,
        code_gen__generate_goal(InnerCodeModel, Goal, Code, !Info)
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "commit_gen.m".

%---------------------------------------------------------------------------%
