:- module ack.

:- interface.

:- type nat	--->	zero ; s(nat).

:- pred ack(nat, nat, nat).
:- mode ack(in, in, out) is det.

:- implementation.

% int(zero).
% int(s(X)) :- int(X).

ack(zero, N, s(N)). % :- int(N).
ack(s(M), zero, A) :-
	ack(M, s(zero), A).
ack(s(M), s(N), A) :-
	ack(s(M), N, A1),
	ack(M, A1, A).
