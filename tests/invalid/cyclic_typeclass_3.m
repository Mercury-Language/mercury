:- module cyclic_typeclass_3.
:- interface.

% The cycles are:
%	`a/1' <= `b/1' <= `g/1' <= `a/1'
%	`a/1' <= `c/1' <= `e/1' <= `g/1' <= `a/1'
%	`c/1' <= `e/1' <= `i/1' <= `c/1'
%
% The second of these is not reported, however, since a cycle for `a/1'
% will have already been detected and reported.

:- typeclass a(T) <= (b(T), c(T))	where [].
:- typeclass b(T) <= g(T)		where [].
:- typeclass c(T) <= (d(T), e(T), f(T))	where [].
:- typeclass d(T) 			where [].
:- typeclass e(T) <= (g(T), h(T), i(T))	where [].
:- typeclass f(T) 			where [].
:- typeclass g(T) <= a(T)		where [].
:- typeclass h(T) <= f(T)		where [].
:- typeclass i(T) <= c(T)		where [].

