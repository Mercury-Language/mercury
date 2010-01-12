% Explicit type qualification didn't work with methods defined directly in the
% instance declaration. Bug #123.

:- module instance_typequal.

:- interface.

:- type e(E) ---> e(E).

:- type y(A) ---> y(A).

:- typeclass tc(S, T) where [
    pred x1(S::in, T::in) is det,
    pred x2(S::in, T::in) is det
].

:- implementation.

:- instance tc(e(E), y(A)) <= tc(E, A) where [
    ( x1(e(E), y(A : _)) :-
        x1(E, A)
    ),

    % Explicit type variables in different clauses are distinct.
    ( x2(e(E), y(A : T)) :-
        x2(E, A : T)
    ),
    ( x2(e(E : T), y(A)) :-
        x2(E : T, A)
    )
].
