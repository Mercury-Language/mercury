:- module doubleapp_impl.

:- interface.

:- import_module list.

:- pred double_app(list(T), list(T), list(T), list(T)).
:- mode double_app(in, in, in, out) is det.

:- implementation.

double_app(X,Y,Z,Res) :-
        app(X,Y,Int),
        app(Int,Z,Res).

:- pred app(list(T), list(T), list(T)).
:- mode app(in, in, out) is det.

app([],L,L).
app([H|X],Y,[H|Z]) :-
        app(X,Y,Z).

