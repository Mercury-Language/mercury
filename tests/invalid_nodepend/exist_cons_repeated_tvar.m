% Check that we reject existentially qunatified data constructors with repeated
% type variables.

:- module exist_cons_repeated_tvar.
:- interface.

:- type nowarn ---> nowarn.

:- type foo
    ---> some [T, T] foo(T).

:- type bar ---> some [T, T, U, U, V] bar(T, U, V).
