:- module merge_ground_any.
:- interface.

:- type hpair(A,B)  ---> (A-B) ; (A+B).
:- inst hpair(A,B)  = bound(A-B).

:- pred pass(hpair(A,B),hpair(A,B),hpair(A,B)).
:- mode pass(in,in(hpair(ground,any)),out(hpair(ground,any))) is multi.

:- implementation.

pass(X,_,X).
pass(_,X,X).
