%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module eqv_type_bug_helper_1.

:- interface.

:- type cqueue(T).

:- pred cqueue(cqueue(T)).
:- mode cqueue(out) is det.

:- pred insert(cqueue(T), T, cqueue(T)).
:- mode insert(in, in, out) is det.

:- pred append(cqueue(T), T, cqueue(T)).
:- mode append(in, in, out) is det.

:- pred this(cqueue(T), T).
:- mode this(in, out) is semidet.

:- pred next(cqueue(T), cqueue(T)).
:- mode next(in, out) is det.

:- pred prev(cqueue(T), cqueue(T)).
:- mode prev(in, out) is det.

:- implementation.

:- import_module list.
:- import_module pair.

:- type cqueue(T) == pair(list(T)).

cqueue([] - []).

insert([Thing | Before] - After, New, [Thing, New | Before] - After).
insert([] - After0, New, Before - After) :-
    list.reverse(After0, Before0),
    (
        Before0 = [],
        Before = [New],
        After = []
    ;
        Before0 = [Thing | Before1],
        Before = [Thing, New | Before1],
        After = []
    ).

append(Before - After, New, Before - [New | After]).

this([This | _Before] - _After, This).
this([] - After, This) :-
    list.reverse(After, [This | _Before]).

next(Before - [Thing | After], [Thing | Before] - After).
next(Before0 - [], Before - After) :-
    list.reverse(Before0, After0),
    (
        After0 = [],
        Before = [],
        After = []
    ;
        After0 = [Thing | After],
        Before = [Thing]
    ).

prev([Thing | Before] - After, Before - [Thing | After]).
prev([] - After0, Before - After) :-
    list.reverse(After0, Before0),
    (
        Before0 = [],
        After = [],
        Before = []
    ;
        Before0 = [Thing | Before],
        After = [Thing]
    ).
