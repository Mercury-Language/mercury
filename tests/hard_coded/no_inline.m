
:- module no_inline.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.
:- pred baz(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_string("I'm going to see whether `bar'/2 is inlined.\n"),
	io__write_string("(Remember kids, don't try this one at home.)\n\n"),
	bar,
	io__write_string("It wasn't.\n").

:- pragma no_inline(bar/2).

:- pred bar(io__state::di, io__state::uo) is det.

:- pragma c_header_code("
#include <stdio.h>
").

:- pragma c_code(bar(IO0::di, IO::uo), will_not_call_mercury, "
	/* START INLINING */
{
	static int i = 0;
	IO = IO0;

	/* Check if I get run again - I'm supposed to only execute once! */

	if (i > 0) { 
		printf(""bar/2: You inlined me!\n""
			""Read my lips, `no_inline'\n\n"");
		    fatal_error(""pragma no_inline ignored."");
	}
	i++;

	tailcall(ENTRY(mercury__no_inline__baz_2_0),
			ENTRY(mercury__no_inline__bar_2_0));
	/* END INLINING */
}
").


:- pragma c_code(baz(IO0::di, IO::uo), will_not_call_mercury, "
	IO = IO0;
	proceed();
").

