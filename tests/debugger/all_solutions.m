% An example program to illustrate the use of all-solutions predicates
% in Mercury.  This program just prints out all solutions to the
% predicate hello/1.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module all_solutions.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module solutions.

main --> 
	{ solutions(hello, List) },
	io__write_strings(List).

:- pred hello(string::out) is multi.

hello("Hello, world\n").
hello("Hello again, world\n").

