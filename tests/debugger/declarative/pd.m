:- module pd.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list.

main -->
	io__write(rev([1, 2, 3])),
	io__nl.

:- func rev(list(int)) = list(int).

rev(As) = rev_2(As, []).

:- func rev_2(list(int), list(int)) = list(int).

rev_2([], _) = [].	% oops
rev_2([A | As], Bs) = rev_2(As, [A | Bs]).

