:- module read_line_as_string.

:- interface.
:- import_module io.

:- pred main(io__state :: di, io__state :: uo) is det.

:- implementation.

main -->
	io__open_input("read_line_as_string.exp", Result),
	( { Result = ok(Stream) } ->
		io__set_input_stream(Stream, _),
		io__read_line_as_string(Result2),
		cat(Result2)
	;
		io__write_string("Error opening file!")
	).

:- pred cat(io__result(string)::in, io__state::di, io__state::uo) is det.

cat(Result) -->
	( { Result = ok(Line) },
		io__write_string(Line),
		io__read_line_as_string(NextResult),
		cat(NextResult)
	; { Result = eof }
	; { Result = error(_Error) },
		io__write_string("Error reading file!")
	).

