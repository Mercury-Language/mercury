:- module table.

:- interface.

:- import_module int, list.

:- type table(T).

:- pred table__mktable(list(T), table(T)).
:- mode table__mktable(in, out) is det.

:- pred table__lookup(table(T), int, T).
:- mode table__lookup(in, in, out) is det.

:- implementation.

:- import_module array.

:- type table(T) == array(T).

table__mktable(Ts, Table) :-
	Table = array(Ts).

table__lookup(Table, I, V) :- array__lookup(Table, I, V).

