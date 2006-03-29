:- module cqueue.

:- interface.

:- type cqueue(T).

:- pred cqueue__cqueue(cqueue(T)).
:- mode cqueue__cqueue(out) is det.

:- pred cqueue__insert(cqueue(T), T, cqueue(T)).
:- mode cqueue__insert(in, in, out) is det.

:- pred cqueue__append(cqueue(T), T, cqueue(T)).
:- mode cqueue__append(in, in, out) is det.

:- pred cqueue__this(cqueue(T), T).
:- mode cqueue__this(in, out) is semidet.

:- pred cqueue__next(cqueue(T), cqueue(T)).
:- mode cqueue__next(in, out) is det.

:- pred cqueue__prev(cqueue(T), cqueue(T)).
:- mode cqueue__prev(in, out) is det.

:- implementation.

:- import_module list, pair.

:- type cqueue(T) ==	pair(list(T)).


cqueue__cqueue([] - []).

cqueue__insert([Thing|Before] - After, New, [Thing,New|Before] - After).
cqueue__insert([] - After0, New, Before - After) :-
	list__reverse(After0, Before0),
	(
		Before0 = [],
		Before = [New],
		After = []
	;
		Before0 = [Thing|Before1],
		Before = [Thing, New|Before1],
		After = []
	).

cqueue__append(Before - After, New, Before - [New|After]).

cqueue__this([This|_Before] - _After, This).
cqueue__this([] - After, This) :-
	list__reverse(After, [This|_Before]).

cqueue__next(Before - [Thing|After], [Thing|Before] - After).
cqueue__next(Before0 - [], Before - After) :-
	list__reverse(Before0, After0),
	(
		After0 = [],
		Before = [],
		After = []
	;
		After0 = [Thing|After],
		Before = [Thing]
	).

cqueue__prev([Thing|Before] - After, Before - [Thing|After]).
cqueue__prev([] - After0, Before - After) :-
	list__reverse(After0, Before0),
	(
		Before0 = [],
		After = [],
		Before = []
	;
		Before0 = [Thing|Before],
		After = [Thing]
	).

