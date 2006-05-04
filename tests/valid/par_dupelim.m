% Uncaught Mercury exception:
% Software Error: dupelim.m: Unexpected: blocks with same standard 
%   form don't antiunify

:- module par_dupelim.
:- interface.

:- type t ---> t1 ; t2.
:- pred p(t::in, int::out) is det.

:- implementation.

p(T, X) :-
    (
	T = t1,
	( X = 1
	& true
	% This conjunct is the same as a later conjunct.
	)
    ;
	T = t2,
	( X = 2
	& true
	)
    ).
