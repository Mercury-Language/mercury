% The compiler of 23/8/1999 had a bug which caused a mode error
% if the clauses in a `.opt' file contained a call to an impure predicate.
% The problem was that mode analysis was not reordering the head
% unifications with the impure call because the head unifications
% had been expanded into separate unifications twice - once
% when reading the `.m' file and once when reading the `.opt' file.
% The fix was to avoid putting goals which call impure predicates
% in the `.opt' files.
:- module intermod_impure.

:- interface.

:- import_module io.

:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module intermod_impure2.

main -->
	{ impure intermod_impure(Int) },
	io__write_int(Int),
	io__nl.

