% This is a test to check the proper functioning of the code in switch_detect.m
% that looks for switch unifications inside nested disjunctions.

:- module switch_detect.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.

:- type t
	--->	f
	;	g(int)
	;	h(int, float).

main(!IO) :-
	read_t(X, !IO),
	(
		X = f,
		io__write_string("f\n", !IO)
	;
		( X = g(_) ; X = h(_, _) ),
		io__write(X, !IO),
		io__nl(!IO)
	),
	read_t(Y, !IO),
	(
		Y = f,
		Num = 42
	;
		Z = Y,
		(
			Z = g(Num)
		;
			W = Z,
			W = h(Num0, _),
			Num = Num0 + 5
		)
	),
	io__write_int(Num, !IO),
	io__nl(!IO).

:- pred read_t(t::out, io::di, io::uo) is det.

read_t(X, !IO) :-
	io__read(Res, !IO),
	(
		Res = ok(X)
	;
		Res = error(_, _),
		error("cannot read")
	;
		Res = eof,
		error("eof")
	).
