% The compiler of 3/5/2000 aborted on this test case because it
% didn't properly handle overloading of field access functions
% and constructors.
:- module record_syntax_bug.
:- interface.

:- type foo ---> debug(string).
:- type foo2 ---> some [T] debug(T).
:- type bar ---> bar( debug :: int ).

:- func dest(foo) = int.

:- pred baz(int, bar).
:- mode baz(in, in) is semidet.

:- implementation.

dest(debug(_)) = 42.

baz(debug(X), X).
