% All this program does is echo the input by recursively calling main to read
% and then write one char.
%
% It is useful both for ensuring that this recursion works ok, and as an IO
% test.

:- module recursive_main.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module io.

main -->
	read_char(Result),
	(
		{ Result = ok(Char) },
		write_char(Char),
		main
	;
		{ Result = eof }
	;
		{ Result = error(Error) },
		write_string(error_message(Error))
	).
