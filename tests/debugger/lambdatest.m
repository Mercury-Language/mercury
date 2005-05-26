:- module lambdatest.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
	Z = polycall(
		(func(X) = X + 1),
		(func(Y) = Y + 2)),
	io.write_int(Z, !IO),
	nl(!IO).

:- func polycall(func(int) = int, func(int) = int) = int.

polycall(F, G) = F(1) + G(2).
