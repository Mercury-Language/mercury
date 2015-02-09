:- module trust.

:- interface.

:- import_module io.

:- pred main(io::di,io::uo) is cc_multi.

:- implementation.

:- import_module trust_1, trust_2.

main(!IO) :- 
	dostuff(w(S), R),
	write_string(S, !IO),
	nl(!IO),
	write(R, !IO),
	nl(!IO).

:- pred dostuff(w::out, comparison_result::uo) is cc_multi.	
	
dostuff(W, R) :-
	compare(R, w("aaB"), w("aAB")),
	concat(w("aaa"),w("bbb"),W).
	
