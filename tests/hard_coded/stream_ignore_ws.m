:- module stream_ignore_ws.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module stream.

main(!IO) :-
	io.stdin_stream(Stdin, !IO),
	stream.ignore_whitespace(Stdin, IgnoreResult, !IO),
	( IgnoreResult = ok ->
		io.read_file_as_string(Stdin, MaybePartialRes, !IO),
		( MaybePartialRes = ok(String) ->
		    io.write_string(String, !IO)
		;
		    io.write_string("io.read_file_as_string FAILED\n", !IO)
		)
	;
		io.write_string("stream.ignore_whitespace FAILED\n", !IO)
	).
