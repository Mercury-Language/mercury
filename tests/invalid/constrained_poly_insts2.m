%-----------------------------------------------------------------------------%

:- module constrained_poly_insts2.
:- interface.

:- pred test1 is det.
:- pred test2 is det.
:- pred test3 is det.
:- pred test4 is det.

:- pred test5 is det.
:- pred test6 is det.
:- pred test7 is det.
:- pred test8 is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type fruit --->  apple ; orange.
:- inst apple --->  apple.
:- inst orange ---> orange.

:- pred apple1(fruit).
:- mode apple1(in(apple)) is det.

apple1(_).

:- pred apple2(fruit).
:- mode apple2(in(I =< apple)) is det.

apple2(_).

test1 :- p(apple1, apple).
test2 :- p(apple1, orange).
test3 :- p(apple2, apple).
test4 :- p(apple2, orange).

test5 :- q(apple1, apple).
test6 :- q(apple1, orange). % error
test7 :- q(apple2, apple).
test8 :- q(apple2, orange). % error

:- pred p(pred(T), T).
:- mode p(pred(in(I =< ground)) is det, in) is det.

p(P, X) :- P(X). % error

:- pred p2(pred(T), T).
:- mode p2(pred(in(I =< ground)) is det, in(J =< apple)) is det.

p2(P, X) :- P(X). % error

:- pred p3(pred(T), T).
:- mode p3(pred(in(I =< apple)) is det, in(orange)) is det.

p3(P, X) :- P(X). % error

:- pred p4(pred(T), T).
:- mode p4(pred(in(I =< apple)) is det, in(J =< apple)) is det.

p4(P, X) :- P(X). % error

:- pred p5(pred(T), T).
:- mode p5(pred(in(I =< apple)) is det, in(apple)) is det.

p5(P, X) :- P(X). % error

:- pred q(pred(T), T).
:- mode q(pred(in(I =< ground)) is det, in(I =< ground)) is det.

q(P, X) :- P(X).

:- pred q2(pred(T), T).
:- mode q2(pred(in(I =< apple)) is det, in(I =< apple)) is det.

q2(P, X) :- P(X).

:- pred q3(pred(T), T).
:- mode q3(pred(in(apple)) is det, in(I =< apple)) is det.

q3(P, X) :- P(X).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
