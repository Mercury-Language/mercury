:- module list_series_int.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int.

main(!IO) :-
	io.write(1 .. 10, !IO),
	io.nl(!IO),
	io.write(-10 .. 0, !IO),
	io.nl(!IO),
	io.write(2 * 3 .. 5 + 5 - 1, !IO),
	io.nl(!IO),
	io.write(10..2, !IO),
	io.nl(!IO),
	io.write(0..0, !IO),
	io.nl(!IO),
	io.write((1..4) ++ (1..4), !IO),
	io.nl(!IO).
