:- module lambda_syntax_error.
:- interface.

:- some [T] func baz(int) =  T.
:- some [T] func baz2(int) =  T.
:- some [T] func baz3(int) =  T.
:- some [T] func baz4(int) =  T.

:- implementation.
:- import_module int.

baz(X) = (pred(Y) :- X > Y).	% modes & determinism not specified
baz2(X) = (pred(Y::in) :- X > Y).	% determinism not specified
baz3(X) = (pred(Y) is semidet :- X > Y). % mode not specified
baz4(X) = (pred(A) = B :- X = A + B).	% mixing `func' and `pred' notation
