:- module unused_args_test2.
:- interface.
:- type foo ---> foo(int).
:- pred bug(foo::out) is det.
:- implementation.
bug(Y) :-
	Y = foo(_),
	Z = 42,
	Y = foo(Z).
