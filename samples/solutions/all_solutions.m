% An example program to illustrate the use of all-solutions predicates
% in Mercury.  This program just prints out all solutions to the
% predicate hello/1.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module all_solutions.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module solutions.

main(!IO) :-
	solutions(hello, List),
	io.write_strings(List, !IO).

:- pred hello(string::out) is multi.

hello("Hello, world\n").
hello("Hello again, world\n").

