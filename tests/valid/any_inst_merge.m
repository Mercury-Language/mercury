% A regression test -- Mercury 0.9.1 and earlier failed this test case,
% due to a bug in inst_merge.

:- module any_inst_merge.

:- interface.

:- type hpair(A,B)  ---> (A-B).

:- inst hpair(A,B)  == bound(A-B).

:- pred pass(hpair(A,B),hpair(A,B),hpair(A,B)).
:- mode pass(in,in(hpair(ground,any)),out(any)) is multi.

:- implementation.

pass(X,_,X).
pass(_,X,X).
