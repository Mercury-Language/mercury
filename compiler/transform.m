%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: transform.m
% Main author: bromage.
%
% This module defines the primitive operations that may be performed
% on a logic program.  These include:
%
%	- unfold (NYI)
%	  Replaces a goal with its possible expansions.
%
%	- fold (NYI)
%	  Opposite of unfold (not surprisingly).
%
%	- definition (NYI)
%	  Define a new predicate with a given goal.
%
%	- identity (NYI)
%	  Apply an identity (such as the associative law for
%	  addition) to a goal.
%
% These operations form the basis of most high-level transformations.
%
% Also included is a conjunction rescheduler.  Useful just in case
% your transformer upset the ordering in a conjunction.
%-----------------------------------------------------------------------------%

:- module transform.
:- interface.
:- import_module hlds_goal, mode_info.

%:- pred unfold__in_proc(pred_id, proc_id, hlds_goal_expr,
%			mode_info, mode_info).
%:- mode unfold__in_proc(in, in, out, mode_info_di, module_info_uo) is det.

:- pred transform__reschedule_conj(list(hlds_goal), list(hlds_goal), 
			mode_info, mode_info).
:- mode transform__reschedule_conj(in, out, mode_info_di, mode_info_uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module list, map, set, std_util.
:- import_module mode_util, delay_info, term, require.
:- import_module varset, code_aux, prog_data, instmap.

%-----------------------------------------------------------------------------%

% unfold__in_proc(

%-----------------------------------------------------------------------------%

transform__reschedule_conj([], []) --> [].
transform__reschedule_conj([Goal0 | Goals0], Goals) -->
	=(ModeInfo0),
	{ mode_info_get_instmap(ModeInfo0, InstMap0) },
	{ mode_info_get_delay_info(ModeInfo0, DelayInfo0) },

	{ delay_info__wakeup_goals(DelayInfo0, WokenGoals, DelayInfo1) },
	mode_info_set_delay_info(DelayInfo1),
	( { WokenGoals \= [] } ->
	    { list__append(WokenGoals, [Goal0 | Goals0], Goals1) },
	    transform__reschedule_conj(Goals1, Goals)
	;
	    { Goal0 = _Goal0Goal - Goal0Info },
	    { goal_info_get_instmap_delta(Goal0Info, InstMapDelta) },
	    { instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1) },
	    mode_info_set_instmap(InstMap1),
	    transform__reschedule_conj(Goals0, Goals1),
	    { Goals = [Goal0 | Goals1] }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
