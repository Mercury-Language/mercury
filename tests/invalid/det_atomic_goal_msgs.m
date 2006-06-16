%
% rotd-2006-06-15 printed out a badly formed error message for
% det_calls_nondet/2.
%
:- module det_atomic_goal_msgs.
:- interface.

:- pred det_calls_failure(int::in, int::out) is det.
:- pred det_calls_semidet(int::in, int::out) is det.
:- pred det_calls_multi(int::out) is det.
:- pred det_calls_nondet(int::in, int::out) is det.

:- implementation.

det_calls_failure(X, 4)   :- failure_pred(X).
det_calls_semidet(X, 4)   :- semidet_pred(X).
det_calls_multi(X)        :- multi_pred(X).
det_calls_nondet(X, Y)    :- nondet_pred(X, Y).

:- pred nondet_pred(int::in, int::out) is nondet.

nondet_pred(1, 2).
nondet_pred(1, 3).
nondet_pred(2, 2).
nondet_pred(3, 2).

:- pred multi_pred(int::out) is multi.

multi_pred(1).
multi_pred(2).
multi_pred(3).
multi_pred(4).

:- pred semidet_pred(int::in) is semidet.

semidet_pred(3).

:- pred failure_pred(int::in) is failure.

failure_pred(_).
