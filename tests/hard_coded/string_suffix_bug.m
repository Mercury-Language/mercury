:- module string_suffix_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main -->
	( { string__suffix("testing string__suffix", "suffix") } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).
