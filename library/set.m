%--------------------------------------------------------------------------%
%--------------------------------------------------------------------------%

:- module set.

:- interface.

:- type set(T).

:- pred set__init(set(T)).
:- mode set__init(out).

:- pred set__equal(set(T), set(T)).
:- mode set__equal(in, in).

:- pred set__subset(set(T), set(T)).
:- mode set__subset(in, in).

:- pred set__superset(set(T), set(T)).
:- mode set__superset(in, in).

:- pred set__member(T, set(T)).
:- mode set__member(in, in) is det.
:- mode set__member(out, in) is nondet.

:- pred set__insert(set(T), T, set(T)).
:- mode set__insert(in, in, out) is det.

:- pred set__delete(set(T), T, set(T)).
:- mode set__delete(in, in, out) is semidet.

:- pred set__union(set(T), set(T), set(T)).
:- mode set__union(in, in, out) is det.

:- pred set__intersect(set(T), set(T), set(T)).
:- mode set__intersect(in, in, out).

:- implementation.

:- import_module list.

:- type set(T)          ==      list(T).

set__init([]).

set__equal([], []).
set__equal([E|S0], S1) :-
    set__delete(S1, E, S2),
    set__equal(S0, S2).

set__subset([], S).
set__subset([E|S0], S1) :-
    set__member(E, S1),
    set__subset(S0, S1).

set__superset(S0, S1) :-
    set__subset(S1, S0).

set__member(E, S) :-
    member(E, S).

set__insert(S0, E, [E|S0]).

set__delete(S0, E, S) :-
    delete(S0, E, S).

set__union([], S, S).
set__union([E|S0], S1, S) :-
    (
	member(E, S1)
    ->
	S2 = S1
    ;
	S2 = [E|S1]
    ),
    set__union(S0, S2, S).

set__intersect(S0, S1, S) :-
    set__intersect_2(S0, S1, [], S).

:- pred set__intersect_2(set(T), set(T), set(T), set(T)).
:- mode set__intersect_2(in, in, in, out).

set__intersect_2([], S1, S, S).
set__intersect_2([E|S0], S1, S2, S) :-
    (
	member(E, S1)
    ->
	S3 = [E|S2]
    ;
	S3 = S2
    ),
    set__intersect_2(S0, S1, S3, S).

