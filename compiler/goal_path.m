%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% This module looks after goal paths, which associate each goal
% with its position in a procedure definition,

% Main author: zs.

:- module goal_path.

:- interface.

:- import_module hlds_pred, hlds_module.

:- pred goal_path__fill_slots(proc_info::in, module_info::in, proc_info::out)
	is det.

:- implementation.

:- import_module hlds_goal.
:- import_module int, list, std_util.

goal_path__fill_slots(Proc0, _ModuleInfo, Proc) :-
		% The ModuleInfo argument is there just for passes_aux
	proc_info_goal(Proc0, Goal0),
	fill_goal_slots(Goal0, [], Goal),
	proc_info_set_goal(Proc0, Goal, Proc).

:- pred fill_goal_slots(hlds_goal::in, goal_path::in, hlds_goal::out) is det.

fill_goal_slots(Expr0 - Info0, Path0, Expr - Info) :-
	goal_info_set_goal_path(Info0, Path0, Info),
	fill_expr_slots(Expr0, Path0, Expr).

:- pred fill_expr_slots(hlds_goal_expr::in, goal_path::in,
	hlds_goal_expr::out) is det.

fill_expr_slots(conj(Goals0), Path0, conj(Goals)) :-
	fill_conj_slots(Goals0, Path0, 0, Goals).
fill_expr_slots(par_conj(Goals0, SM), Path0, par_conj(Goals, SM)) :-
	fill_conj_slots(Goals0, Path0, 0, Goals).
fill_expr_slots(disj(Goals0, B), Path0, disj(Goals, B)) :-
	fill_disj_slots(Goals0, Path0, 0, Goals).
fill_expr_slots(switch(A, B, Cases0, D), Path0, switch(A, B, Cases, D)) :-
	fill_switch_slots(Cases0, Path0, 0, Cases).
fill_expr_slots(not(Goal0), Path0, not(Goal)) :-
	fill_goal_slots(Goal0, [neg | Path0], Goal).
fill_expr_slots(some(A, Goal0), Path0, some(A, Goal)) :-
	fill_goal_slots(Goal0, [exist | Path0], Goal).
fill_expr_slots(if_then_else(A, Cond0, Then0, Else0, E), Path0,
		if_then_else(A, Cond, Then, Else, E)) :-
	fill_goal_slots(Cond0, [ite_cond | Path0], Cond),
	fill_goal_slots(Then0, [ite_then | Path0], Then),
	fill_goal_slots(Else0, [ite_else | Path0], Else).
fill_expr_slots(call(A,B,C,D,E,F), _Path0, call(A,B,C,D,E,F)).
fill_expr_slots(higher_order_call(A,B,C,D,E,F), _Path0,
		higher_order_call(A,B,C,D,E,F)).
fill_expr_slots(class_method_call(A,B,C,D,E,F), _Path0,
		class_method_call(A,B,C,D,E,F)).
fill_expr_slots(unify(A,B,C,D,E), _Path0, unify(A,B,C,D,E)).
fill_expr_slots(pragma_c_code(A,B,C,D,E,F,G), _Path0,
		pragma_c_code(A,B,C,D,E,F,G)).

:- pred fill_conj_slots(list(hlds_goal)::in, goal_path::in, int::in,
	list(hlds_goal)::out) is det.

fill_conj_slots([], _, _, []).
fill_conj_slots([Goal0 | Goals0], Path0, N0, [Goal | Goals]) :-
	N1 is N0 + 1,
	fill_goal_slots(Goal0, [conj(N1) | Path0], Goal),
	fill_conj_slots(Goals0, Path0, N1, Goals).

:- pred fill_disj_slots(list(hlds_goal)::in, goal_path::in, int::in,
	list(hlds_goal)::out) is det.

fill_disj_slots([], _, _, []).
fill_disj_slots([Goal0 | Goals0], Path0, N0, [Goal | Goals]) :-
	N1 is N0 + 1,
	fill_goal_slots(Goal0, [disj(N1) | Path0], Goal),
	fill_disj_slots(Goals0, Path0, N1, Goals).

:- pred fill_switch_slots(list(case)::in, goal_path::in, int::in,
	list(case)::out) is det.

fill_switch_slots([], _, _, []).
fill_switch_slots([case(A, Goal0) | Cases0], Path0, N0,
		[case(A, Goal) | Cases]) :-
	N1 is N0 + 1,
	fill_goal_slots(Goal0, [switch(N1) | Path0], Goal),
	fill_switch_slots(Cases0, Path0, N1, Cases).
