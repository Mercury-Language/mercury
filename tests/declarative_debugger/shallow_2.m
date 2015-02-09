:- module shallow_2.
:- interface.

:- pred p(string::in, int::in, int::out) is det.
:- pred q(string::in, int::in, int::out) is det.
:- pred r(string::in, int::in, int::out) is det.

:- implementation.
:- import_module shallow_3.

p(S, M, N) :-
	(
		pp(S, 1, M)
	->
		N = 1
	;
		N = -11
	).

:- pred pp(string::in, int::in, int::out) is multi.

pp(S, M, N) :- a(S, M, N).  
pp(S, M, N) :- b(S, M, N).

q(S, M, N) :-
	(
		a(S, M, -1)
	->
		N = 11
	;
		N = 2
	).

r(S, M, N) :-
	(
		\+ a(S, M, -3),
		b(S, M, 5)
	->
		N = 23
	;
		N = 0
	).

% shallow_3 defines:
%
% :- pred a(string::in, int::in, int::out) is multi.
%
% a(_, X, X).
% a(_, _, 0).
%
% :- pred b(string::in, int::in, int::out) is det.
%
% b(_, _, 5).

