:- module change_search.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, std_util.

main(!IO) :-
	mylast(1 .. 1000, L),
	io.write(L, !IO),
	io.nl(!IO).

:- pred mylast(list(T)::in, maybe(T)::out) is det.

mylast([], no).
mylast([_], no). % Bug
mylast([_, H2 | T], L) :- mylast([H2 | T], L).
