:- module builtin_inst_rename.

:- interface.

:- import_module builtin_inst_rename2.
:- import_module io.

:- pred p(builtin_inst_rename2.my_int, builtin_inst_rename2.my_int).
:- mode p(builtin_inst_rename2__ground >> builtin_inst_rename2__ground,
	  builtin_inst_rename2__free   >> builtin_inst_rename2__ground)
	is det.

:- pred main(io::di, io::uo) is det.

:- implementation.

p(X, X).

main -->
	{ p(42, X) },
	io__write_int(X),
	io__nl.
