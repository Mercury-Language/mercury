% A regression test.
% Mercury 0.9.1 and earlier reported a spurious mode error
% for this code.

:- module mostly_uniq_bug.
:- interface.

:- pred test is det.

:- implementation.

:- type foo ---> f(int).

:- pred p(foo::mostly_unique >> dead) is det.
:- pred q(foo::unique(f(unique(42))) >> dead) is det.

p(_).
q(X) :- p(X).
test :- q(f(42)).
