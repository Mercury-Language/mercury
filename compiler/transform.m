%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2003-2007, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: transform.m
% Main author: bromage.
%
% This module defines the primitive operations that may be performed
% on a logic program. These include:
%
%   - unfold (NYI)
%     Replaces a goal with its possible expansions.
%
%   - fold (NYI)
%     Opposite of unfold (not surprisingly).
%
%   - definition (NYI)
%     Define a new predicate with a given goal.
%
%   - identity (NYI)
%     Apply an identity (such as the associative law for addition) to a goal.
%
% These operations form the basis of most high-level transformations.
%
% Also included is a conjunction rescheduler. Useful just in case
% your transformer upsets the ordering in a conjunction.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.transform.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred reschedule_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.delay_info.
:- import_module hlds.instmap.

%-----------------------------------------------------------------------------%

reschedule_conj([], [], !ModeInfo).
reschedule_conj([Goal0 | Goals0], Goals, !ModeInfo) :-
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    mode_info_get_delay_info(!.ModeInfo, DelayInfo0),

    delay_info_wakeup_goals(WokenGoals, DelayInfo0, DelayInfo1),
    mode_info_set_delay_info(DelayInfo1, !ModeInfo),
    (
        WokenGoals = [_ | _],
        list.append(WokenGoals, [Goal0 | Goals0], Goals1),
        reschedule_conj(Goals1, Goals, !ModeInfo)
    ;
        WokenGoals = [],
        Goal0 = hlds_goal(_Goal0Goal, Goal0Info),
        InstMapDelta = goal_info_get_instmap_delta(Goal0Info),
        apply_instmap_delta(InstMapDelta, InstMap0, InstMap1),
        mode_info_set_instmap(InstMap1, !ModeInfo),
        reschedule_conj(Goals0, Goals1, !ModeInfo),
        Goals = [Goal0 | Goals1]
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.transform.
%-----------------------------------------------------------------------------%
