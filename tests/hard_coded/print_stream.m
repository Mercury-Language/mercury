:- module print_stream.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	io.stdout_stream(StdOut, !IO),
	io.stdin_stream(StdIn, !IO),
	io.stderr_stream(StdErr, !IO),
	io.stdin_binary_stream(StdInBin, !IO),
	io.stdout_binary_stream(StdOutBin, !IO),
	io.write(StdIn, !IO),     io.nl(!IO),
	io.write(StdOut, !IO),    io.nl(!IO),
	io.write(StdErr, !IO),    io.nl(!IO),
	io.write(StdInBin, !IO),  io.nl(!IO),
	io.write(StdOutBin, !IO), io.nl(!IO).
