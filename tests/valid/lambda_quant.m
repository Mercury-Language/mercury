% Test quantification of local variables in lambda expressions.
% Also test use of variables as goals (without *explicitly* using call/1).

:- module lambda_quant.
:- interface.

:- pred test(pred).
:- mode test(out((pred) is semidet)) is nondet.

:- implementation.
:- import_module list, int.

:- pred t is semidet.
t.

:- pred f is semidet.
f :- fail.

test(t).
test(f).
test(G) :-
	G1 = ((pred) is semidet :- X = 0, X \= 1),
	G2 = ((pred) is det :- X = [], X \= [_|_]),
	G = ((pred) is semidet :- G1, G2).
test(G) :-
	G1 = ((pred) is semidet :- Y = 0),
	G2 = ((pred) is det :- Y = []),
	G = ((pred) is semidet :- G1, G2).

