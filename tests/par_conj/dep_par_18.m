% cut out of from primes.m

:- module dep_par_18.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    integers(0, 5, R),
    io.print(R, !IO),
    io.nl(!IO).

:- pred integers(int::in, int::in, list(int)::out) is det.

integers(Low, High, Result) :- 
    ( Low =< High ->
	Result = [Low | Rest] &
	integers(Low+1, High, Rest)
    ;
	Result = []
    ).
