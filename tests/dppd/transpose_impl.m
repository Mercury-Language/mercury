
:- module transpose_impl.

:- interface.

:- import_module list.

:- pred transpose(list(list(T)), list(list(T))).
:- mode transpose(in(matrix9), out) is nondet.
:- mode transpose(in, out) is nondet.

:- inst matrix9 = bound([list9, ground, ground]).
:- inst list9 = bound([ground, ground, ground, ground, ground, ground, ground,
			ground, ground]).


:- inst [A | B]
	--->	[A | B].
:- inst []
	--->	[].


:- implementation.

  
transpose(Xs,[]) :-
        nullrows(Xs).
transpose(Xs,[Y|Ys]) :-
        makerow(Xs,Y,Zs),
        transpose(Zs,Ys).

:- pred makerow(list(list(T)), list(T), list(list(T))).
:- mode makerow(in, out, out) is semidet.
:- mode makerow(in(matrix9), out, out) is semidet.

makerow([],[],[]).
makerow([[X|Xs]|Ys],[X|Xs1],[Xs|Zs]) :-
        makerow(Ys,Xs1,Zs).

:- pred nullrows(list(list(T))) is semidet.
:- mode nullrows(in) is semidet.
:- mode nullrows(in(list9)) is semidet.

nullrows([]).
nullrows([[]|Ns]) :-
        nullrows(Ns).

