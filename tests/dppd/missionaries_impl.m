:- module missionaries_impl.

:- interface.

:- import_module list.

:- type nat
	--->	zero
	;	s(nat).

:- type state 
	--->	state(nat, nat, side).

:- type side
	--->	west
	;	east.

:- pred missionaries_query(nat::in, nat::in, list(state)::out) is nondet.

:- implementation.

missionaries_query(X, Y, Res) :- search(X,Y,west,[state(X,Y,west)],Res).

:- pred go(list(state)::out) is nondet.

go(Res) :- search(s(s(s(zero))),s(s(s(zero))),west,
        [state(s(s(s(zero))),s(s(s(zero))),west)],Res).

:- pred search(nat::in, nat::in, side::in, 
		list(state)::in, list(state)::out) is nondet.

search(zero,zero,_Boat,Result,Result).
search(WM,WC,east,History,Result) :-
        move_boat_west(WM,WC,NWM,NWC),
        safe(NWM,NWC),
        \+(loop(NWM,NWC,west,History)),
        search(NWM,NWC,west,[state(NWM,NWC,west)|History],Result).
search(WM,WC,west,History,Result) :-
        move_boat_east(WM,WC,NWM,NWC),
        safe(NWM,NWC),
        \+(loop(NWM,NWC,east,History)),
        search(NWM,NWC,east,[state(NWM,NWC,east)|History],Result).

:- pred safe(nat::in, nat::in) is semidet.

safe(s(s(s(zero))),WestCann) :- \+(gt(WestCann,s(s(s(zero))) )).
safe(zero,WestCann) :- \+(gt(WestCann,s(s(s(zero))) )).
safe(W,W) :-  \+(ge(zero,W)), \+(ge(W,s(s(s(zero))) )).

:- pred loop(nat::in, nat::in, side::in, list(state)::in) is semidet.

loop(WestMiss,WestCann,Boat,History) :-
        mymember(state(WestMiss,WestCann,Boat),History).

:- pred move_boat_east(nat::in, nat::in, nat::out, nat::out) is nondet.

move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus2(NewWestMiss,WestMiss), NewWestCann = WestCann.
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(NewWestMiss,WestMiss), NewWestCann = WestCann.
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(NewWestMiss,WestMiss),
        plus1(NewWestCann,WestCann).
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus1(NewWestCann,WestCann).
move_boat_east(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus2(NewWestCann,WestCann).

:- pred move_boat_west(nat::in, nat::in, nat::out, nat::out) is multidet.

move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus2(WestMiss,NewWestMiss), NewWestCann = WestCann.
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(WestMiss,NewWestMiss), NewWestCann = WestCann.
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        plus1(WestMiss,NewWestMiss), plus1(WestCann,NewWestCann).
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus1(WestCann,NewWestCann).
move_boat_west(WestMiss,WestCann,NewWestMiss,NewWestCann) :-
        NewWestMiss = WestMiss, plus2(WestCann,NewWestCann).

:- pred mymember(T, list(T)).
:- mode mymember(in, in) is semidet.
:- mode mymember(out, in) is nondet.

mymember(X,[X|_]).
mymember(X,[_|T]) :- mymember(X,T).

:- pred gt(nat::in, nat::in) is semidet.

gt(s(_),zero).
gt(s(X),s(Y)) :- gt(X,Y).

:- pred ge(nat::in, nat::in) is semidet.

ge(zero,zero).
ge(s(_),zero).
ge(s(X),s(Y)) :- ge(X,Y).


:- pred plus1(nat, nat).
:- mode plus1(in, out) is det.
:- mode plus1(out, in) is semidet.

plus1(X,s(X)).

:- pred plus2(nat, nat).
:- mode plus2(in, out) is det.
:- mode plus2(out, in) is semidet.
plus2(X,s(s(X))).

