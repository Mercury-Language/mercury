% An example program to illustrate the use of all-solutions predicates
% in Mercury.

:- module all_solutions.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util.

main --> 
	{ solutions(hello, List) },
	io__write_strings(List).

:- pred hello(string::out) is multidet.

hello("Hello, world\n").
hello("Hello again, world\n").

