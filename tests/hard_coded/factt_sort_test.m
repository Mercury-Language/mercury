% This is a regression test.
% It 

:- module factt_sort_test.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.
:- import_module list, string.

main(!IO) :-
	test(0x044E, !IO),
	test(0x044F, !IO),
	test(0x0450, !IO),
	test(0x0451, !IO).

:- pred test(int, io, io).
:- mode test(in, di, uo) is det.

test(X, !IO) :-
	( unicode_to_big5(X, Y) ->
		format("%d => %d\n", [i(X), i(Y)], !IO)
	;
		format("%d => fail\n", [i(X)], !IO)
	).

:- pred unicode_to_big5(int, int).
:- mode unicode_to_big5(in, out) is semidet.

:- pragma fact_table(unicode_to_big5/2, "factt_sort_test.facts").
