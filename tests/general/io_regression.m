:- module io_regression.

%
% io__read_file_as_string stopped working one day, and it wasn't noticed
% because it wasn't in any of the test cases.  So now it is.
%

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main --> 
	io__read_file_as_string(Res),
	( { Res = ok(Str) } ->
		io__write_string(Str)
	;
		io__write_string("Error reading file.\n")
	).

