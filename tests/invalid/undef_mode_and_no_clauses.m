% This is a regression test for a bug that showed up in
% version rotd-2000-08-12.
% After correctly reporting the errors in this code,
% the compiler threw an exception.

:- module undef_mode_and_no_clauses.
:- interface.
:- type result(T) ---> ok(T).
:- implementation.

:- pred p(pred(T), result(T)).
:- mode p(pred(out) is det, result(T)) is det.	% mode result/1 not defined

% no clauses for p/2
