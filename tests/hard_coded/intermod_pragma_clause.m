% Ensure that the foreign_decl is placed into the .opt file for procedures
% which are defined by both a foreign proc and a mercury clause.

:- module intermod_pragma_clause.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module intermod_pragma_clause_sub.

main -->
	{ f(X) },
	io__write_int(X),
	io__nl,
	{ g(Y) },
	io__write_int(Y),
	io__nl.
