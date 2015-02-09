:- module ignore_1.

:- interface.

:- import_module list.

:- func fold(func(T, T) = T, list(T), T) = T.

:- implementation.

fold(_, [], X) = X.
fold(F, [H|T], X0) = fold(F, T, F(H, X0)).
