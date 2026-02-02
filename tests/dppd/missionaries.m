%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "missionaries" Benchmark
% Part of the DPPD Library.
%
% A program for the missionaries and cannibals problem without using
% builtins, but with negation.
%

:- module missionaries.

:- interface.

:- pred missionaries is semidet.

:- implementation.

:- import_module list.
:- import_module missionaries_impl.
:- import_module run.

missionaries :-
    missionaries_query(s(s(s(zero))), s(s(s(zero))), Res),
    use(Res).

% The partial deduction query
%
% :- search(X, Y, west, [state(X, Y, west)], Res).
%
% The run-time queries
%
% :- search(s(s(s(zero))), s(s(s(zero))), west,
%     [state(s(s(s(zero))), s(s(s(zero))), west)], Res).
%
% Example solution
%
% The best solution so far, using the ECCE partial deduction system
% runs almost 2 times faster than the original. Mixtus (0.3.3) and Paddy
% (Eclipse 3.5.1) did not terminate on this example. The following solution
% is not optimal (but is a bit smaller). Its relative execution is 0.69.
%
% search_1(zero, zero, X1, [state(zero, zero, west) | X1]).
% search_1(X1, X2, X3, X4) :-
%     move_boat_east_conj_2(X1, X2, X5, X6),
%     not(loop_3(X5, X6, east, X1, X2, west, X3)),
%     search_4(X5, X6, X1, X2, X3, X4).
%
% move_boat_east_conj_2(s(s(X1)), X2, X1, X2) :- safe_11(X1, X2).
% move_boat_east_conj_2(s(X1), X2, X1, X2) :- safe_11(X1, X2).
% move_boat_east_conj_2(s(X1), s(X2), X1, X2) :- safe_11(X1, X2).
% move_boat_east_conj_2(X1, s(X2), X1, X2) :- safe_11(X1, X2).
% move_boat_east_conj_2(X1, s(s(X2)), X1, X2) :- safe_11(X1, X2).
%
% loop_3(X1, X2, X3, X4, X5, X6, [state(X1, X2, X3) | X7]).
% loop_3(X1, X2, X3, X4, X5, X6, [X7, X8 | X9]) :-
%     mymember_1zero(X1, X2, X3, X8, X9).
%
% search_4(zero, zero, X1, X2, X3,
%         [state(zero, zero, east), state(X1, X2, west) | X3]).
% search_4(X1, X2, X3, X4, X5, X6) :-
%     move_boat_west_conj_5(X1, X2, X7, X8),
%     not(loop_3(X7, X8, west, X1, X2, east, [state(X3, X4, west) | X5])),
%     search_1(X7, X8, [state(X1, X2, east), state(X3, X4, west) | X5], X6).
%
% move_boat_west_conj_5(X1, X2, s(s(X1)), X2) :- safe_6(s(X1), X2).
% move_boat_west_conj_5(X1, X2, s(X1), X2) :- safe_6(X1, X2).
% move_boat_west_conj_5(X1, X2, s(X1), s(X2)) :- safe_6(X1, s(X2)).
% move_boat_west_conj_5(X1, X2, X1, s(X2)) :-  safe_7(X1, X2).
% move_boat_west_conj_5(X1, X2, X1, s(s(X2))) :- safe_7(X1, s(X2)).
%
% safe_6(s(s(zero)), X1) :- not(gt_8(X1)).
% safe_6(X1, s(X1)) :- not(ge_9(s(X1))).
%
% safe_7(s(s(s(zero))), X1) :- not(gt_8(s(X1))).
% safe_7(zero, X1) :- not(gt_8(s(X1))).
% safe_7(s(X1), X1) :- not(ge_9(s(X1))).
%
% gt_8(s(s(s(s(X1))))).
%
% ge_9(s(s(s(zero)))).
% ge_9(s(s(s(s(X1))))).
%
% mymember_1zero(X1, X2, X3, state(X1, X2, X3), X4).
% mymember_1zero(X1, X2, X3, X4, [X5 | X6]) :-
%     mymember_1zero(X1, X2, X3, X5, X6).
%
% safe_11(s(s(s(zero))), X1) :- not(gt_8(X1)).
% safe_11(zero, X1) :- not(gt_8(X1)).
% safe_11(X1, X1) :- not(ge_12(X1)), not(ge_9(X1)).
%
% ge_12(zero).
%
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
