:- module record_syntax_bug_4.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

:- type info
	---> info(
		field :: int
	).
		
main -->
	{ List = list__map(field(info(1)), [1, 2, 3]) },
	io__write(List),
	io__nl.

:- func field(info, int) = int.

field(_Info, Int) = Int.

