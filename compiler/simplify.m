%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% The two jobs of the simplification package are
%
% - to find and exploit opportunities for simplifying the internal form
%   of the program, both to optimize the code and to massage the code
%   into a form the code generator will accept, and
%
% - to warn the programmer about any constructs that look suspicious.
%
% Originally, we warned only about code that was so simple that
% it should not have been included in the program in the first place
% (such as if-then-elses whose conditions cannot fail), but we now
% also warn about other things as well, such as mismatches between
% format strings and the values supplied to be printed.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.simplify.
:- interface.

% We export simplify_tasks because it defines the type that
% clients of simplify use to tell simplify what tasks to perform.
:- include_module check_hlds.simplify.simplify_tasks.
% We export simplify_proc because it exports the predicates that
% clients of simplify call to invoke simplify.
:- include_module check_hlds.simplify.simplify_proc.
% We export format_call because the presence of calls in a procedure
% that format_call needs to check (and may be able to optimize) is noticed
% by code outside the simplify package, in determinism analysis.
% (We could have simplify look for such calls itself, but that would require
% an extra traversal of every procedure.)
:- include_module check_hlds.simplify.format_call.

:- implementation.

:- include_module check_hlds.simplify.common.
:- include_module check_hlds.simplify.simplify_goal.
:- include_module check_hlds.simplify.simplify_goal_call.
:- include_module check_hlds.simplify.simplify_goal_conj.
:- include_module check_hlds.simplify.simplify_goal_disj.
:- include_module check_hlds.simplify.simplify_goal_ite.
:- include_module check_hlds.simplify.simplify_goal_scope.
:- include_module check_hlds.simplify.simplify_goal_switch.
:- include_module check_hlds.simplify.simplify_goal_unify.
:- include_module check_hlds.simplify.simplify_info.
:- include_module check_hlds.simplify.split_switch_arms.

%-----------------------------------------------------------------------------%
:- end_module check_hlds.simplify.
%-----------------------------------------------------------------------------%
