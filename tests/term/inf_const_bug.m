% Regression test - rotd 2003-12-14 and before caused a software error
% in term_pass2.m.
% 
% Symptom: 
% 	Uncaught Mercury exception:
% 	Software Error: can_loop detected in pass2 but not pass1

:- module inf_const_bug.

:- interface.

:- type list(T) ---> [] ; [T | list(T)].

:- type pair(T1, T2) ---> (T1 - T2).

:- type a == pair(b, nat).

:- type nat ---> zero ; s(nat). 

:- type b ---> b(list(a)).

:- func foo(a) = a.

:- implementation.

foo(B - N) = bar(B) - square(N).

	% We need to ensure that pass 1 also considers this function 
	% otherwise we will miss the fact that the SCC is nonterminating.
:- func bar(b) = b.

bar(b(As)) = b(map(foo, As)).

:- func square(nat) = nat.

square(A) = multiply(A, A). 

:- func multiply(nat, nat) = nat.

multiply(zero, _) = zero.
multiply(s(X), Y) = add(multiply(X, Y), Y).

:- func add(nat, nat) = nat.

add(zero, Y) = Y.
add(s(X), Y) = s(add(X, Y)).

:- func map(func(X) = Y, list(X)) = list(Y).

map(_, []) = [].
map(P, [X | Xs]) = [ P(X) | map(P, Xs) ].

:- end_module inf_const_bug.
