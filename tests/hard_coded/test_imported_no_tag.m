% Test handling of no_tag types without inlining.
:- module test_imported_no_tag.
:- interface.

:- import_module imported_no_tag, io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- pred test_int(int::in) is semidet.

test_int(99).

main -->
 ( { pwrite(class(test_int), 99) } ->
	io__write_string("ok\n")	
;
	io__write_string("uh oh\n")
).

