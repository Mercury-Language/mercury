%-----------------------------------------------------------------------------%

:- module constrained_poly_multi.
:- interface.

:- pred test is det.

:- pred test2 is failure.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type either ---> left ; right.
:- inst left ---> left.
:- inst right ---> right.

:- type fruit ---> apple ; orange ; pear.
:- inst apple ---> apple.
:- inst orange ---> orange.

:- pred either(either, fruit, fruit, fruit).
:- mode either(in(left), in(I =< apple), in(J =< orange), out(I =< apple))
    is det.
:- mode either(in(right), in(I =< apple), in(J =< orange), out(J =< orange))
    is det.

either(left, X, _, X).
either(right, _, X, X).

test :-
    either(left, apple, orange, apple),
    either(right, apple, orange, orange).

test2 :-
    either(left, apple, orange, orange).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
