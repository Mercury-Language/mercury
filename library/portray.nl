
portray(Term) :-
	compound(Term),
	Term =.. [F|As],
	write(F),
	write('('),
	portray_args(As),
	write(')').

portray_args([]).
portray_args(X.Xs) :-
	portray2(X),
	portray_args_2(Xs).

portray_args_2([]).
portray_args_2(X.Xs) :-
	write(', '),
	portray2(X),
	portray_args_2(Xs).

portray2(Term) :-
	( compound(Term) ->
		functor(Term,F,N),
		format("<~a/~d>", [F,N])
	;
		write(Term)
	).

spyHook(_,Term) :-
	write(Term), writeln(.).

