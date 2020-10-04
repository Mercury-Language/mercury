%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module cqueue.

:- interface.

:- type cqueue(T).

:- pred cqueue.cqueue(cqueue(T)).
:- mode cqueue.cqueue(out) is det.

:- pred cqueue.insert(cqueue(T), T, cqueue(T)).
:- mode cqueue.insert(in, in, out) is det.

:- pred cqueue.append(cqueue(T), T, cqueue(T)).
:- mode cqueue.append(in, in, out) is det.

:- pred cqueue.this(cqueue(T), T).
:- mode cqueue.this(in, out) is semidet.

:- pred cqueue.next(cqueue(T), cqueue(T)).
:- mode cqueue.next(in, out) is det.

:- pred cqueue.prev(cqueue(T), cqueue(T)).
:- mode cqueue.prev(in, out) is det.

:- implementation.

:- import_module list.
:- import_module pair.

:- type cqueue(T) == pair(list(T)).

cqueue.cqueue([] - []).

cqueue.insert([Thing | Before] - After, New, [Thing, New | Before] - After).
cqueue.insert([] - After0, New, Before - After) :-
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

cqueue.append(Before - After, New, Before - [New | After]).

cqueue.this([This | _Before] - _After, This).
cqueue.this([] - After, This) :-
    list.reverse(After, [This | _Before]).

cqueue.next(Before - [Thing | After], [Thing | Before] - After).
cqueue.next(Before0 - [], Before - After) :-
    list.reverse(Before0, After0),
    (
        After0 = [],
        Before = [],
        After = []
    ;
        After0 = [Thing | After],
        Before = [Thing]
    ).

cqueue.prev([Thing | Before] - After, Before - [Thing | After]).
cqueue.prev([] - After0, Before - After) :-
    list.reverse(After0, Before0),
    (
        Before0 = [],
        After = [],
        Before = []
    ;
        Before0 = [Thing | Before],
        After = [Thing]
    ).
