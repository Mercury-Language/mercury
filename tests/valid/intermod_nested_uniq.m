%-----------------------------------------------------------------------------%
:- module intermod_nested_uniq.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- import_module intermod_nested_uniq2.

main -->
	{ init(1, 1, Matrix) },
	{ lookup(1, 1, Matrix, _) }.

