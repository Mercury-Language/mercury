% This is a regression test - a previous version of the compiler
% got an internal compiler error when compiling this file.
% (Thanks to Bart Demoen for this test.)

/* Running this program yields
213
4


*** Mercury runtime: caught segmentation violation ***
cause: address not mapped to object
PC at signal: 120476 (1d69c)
address involved: 8
exiting from signal handler
*/

:- module partition.
:- interface.
:- import_module io, int, list.

:- pred main(io__state::di, io__state::uo) is multidet.

:- implementation.

main -->
        ( { part(3,[4,2,1,3], Le, Gr) } ->
		print_intlist(Le),
		print_intlist(Gr),
		nl
	;
		io__write_string("No solution\n")
	).

:- pred part(int,list(int),list(int),list(int)).
:- mode part(in,in,out,out) is nondet.
part(_X, [], [], []).
part(X, [Y|L], [Y|Le], Gr):-
        Y =< X, part(X, L, Le, Gr).
part(X, [Y|L], Le, [Y|Gr]):-
        Y > X, part(X, L, Le, Gr).

:- pred nl(io__state::di, io__state::uo) is det.
nl --> io__write_string("\n").

:- pred print_intlist(list(int)::in,io__state::di, io__state::uo) is det.
print_intlist([])--> nl.
print_intlist([X|L])--> io__write_int(X), print_intlist(L).

:- end_module partition.
