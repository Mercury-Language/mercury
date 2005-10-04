:- module impure_init_and_final.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- initialise init/0.
:- finalise final/0.

main(!IO) :- io.write_string("This is main...\n", !IO).

:- pragma foreign_decl("C", "#include <stdio.h>").

:- impure pred puts(string::in) is det.
:- pragma foreign_proc("C",
	puts(S::in),
	[will_not_call_mercury],
"
	puts(S);
").

:- impure pred init is det.

init :- impure puts("This is init...").

:- impure pred final is det.

final :- impure puts("This is final...").
