% An example module to illustrate committed choice nondeterminism in Mercury.
% This program should print out _either_
%
%	Hello, World
% or
%	Goodbye, World
%
% Which one it prints out is unspecified.
% The implementation can pick either.

% Note that the behaviour of the current Mercury implementation is to print
% out _both_, which is incorrect.  We needed to have some way of getting all
% solutions to a predicate, before we had implemented any all-solutions
% predicates.  We'll fix this bug as soon as we have fully implemented
% solutions/2.

:- module committed_choice.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is multidet.

:- implementation.

main --> io__write_string("Hello, world\n").
main --> io__write_string("Goodbye, world\n").
