% An example module to illustrate committed choice nondeterminism in Mercury.
% This program should print out _either_
%
%	Hello, World
% or
%	Goodbye, World
%
% Which one it prints out is unspecified.
% The implementation can pick either.

:- module committed_choice.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is multidet.

:- implementation.

main --> io__write_string("Hello, world\n").
main --> io__write_string("Goodbye, world\n").
