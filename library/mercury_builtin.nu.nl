
builtin_strcmp(Res, S1, S2) :-
	compare(R, S1, S2),
	builtin_strcmp_2(R, Result).

builtin_strcmp_2(<, -1).
builtin_strcmp_2(=, 0).
builtin_strcmp_2(>, 1).

unify(X, X).

index(F, I) :-
	error("mercury_builtin.nu.nl: index/2 called").

