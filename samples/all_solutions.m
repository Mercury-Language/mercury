% An example program to illustrate the use of all-solutions predicates
% in Mercury.

% Note that the current implementation of solutions/2 is incomplete -
% it only works if the predicate passed is monomorphic (doesn't have
% any type parameters).  Also mode and determinism checking of higher-order
% predicates are not yet implemented, so if you pass an incorrect argument
% to solutions/2, you will probably get a core dump.

:- module all_solutionns.
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

