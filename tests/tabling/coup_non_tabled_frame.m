% This is yet another variant of the coup test case. This one includes
% a non-tabled model_non procedure in the nondet stack segment that needs
% to be saved and restored, checking that the frames of such procedures
% are handled correctly.

:- module coup_non_tabled_frame.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module std_util.

main --> 
	{ solutions(p, Solns) },
	io__write(Solns),
	io__write_string("\n").

:- pragma minimal_model(p/1).
:- pred p(int).
:- mode p(out) is nondet.

p(X) :-
	r(X).
p(X) :-
	X = 1.

:- pred r(int).
:- mode r(out) is multi.

r(X) :- q(X).
r(6).

:- pragma minimal_model(q/1).
:- pred q(int).
:- mode q(out) is nondet.

q(3) :- q(_).
q(4) :- p(_).
q(5).
