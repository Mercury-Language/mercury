% Check that loopcheck isn't overzealous.

:- module loopcheck_no_loop.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
	sum(3, Sum3),
	io__write_int(Sum3, !IO),
	io__write_string("\n", !IO),
	sum(2, Sum2),
	io__write_int(Sum2, !IO),
	io__write_string("\n", !IO),
	( semisum(3, Semisum3) ->
		io__write_int(Semisum3, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("semisum(3) failed\n", !IO)
	),
	( semisum(2, Semisum2) ->
		io__write_int(Semisum2, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("semisum(2) failed\n", !IO)
	),
	( semisum(-2, SemisumN2) ->
		io__write_int(SemisumN2, !IO),
		io__write_string("\n", !IO)
	;
		io__write_string("semisum(-2) failed\n", !IO)
	),
	( count(3) ->
		io__write_string("count(3) succeeded\n", !IO)
	;
		io__write_string("count(3) failed\n", !IO)
	),
	( count(2) ->
		io__write_string("count(2) succeeded\n", !IO)
	;
		io__write_string("count(2) failed\n", !IO)
	),
	( count(-2) ->
		io__write_string("count(-2) succeeded\n", !IO)
	;
		io__write_string("count(-2) failed\n", !IO)
	).

:- pred sum(int::in, int::out) is det.
:- pragma loop_check(sum/2).

sum(N, SumN) :-
	( N = 0 ->
		SumN = 0
	;
		sum(N - 1, Sum1),
		SumN = Sum1 + N
	).

:- pred semisum(int::in, int::out) is semidet.
:- pragma loop_check(semisum/2).

semisum(N, SumN) :-
	( N < 0 ->
		fail
	; N = 0 ->
		SumN = 0
	;
		semisum(N - 1, Sum1),
		SumN = Sum1 + N
	).

:- pred count(int::in) is semidet.
:- pragma loop_check(count/1).

count(N) :-
	( N < 0 ->
		fail
	; N = 0 ->
		true
	;
		count(N - 1)
	).
