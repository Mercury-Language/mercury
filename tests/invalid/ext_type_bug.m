:- module ext_type_bug.
:- interface.
:- import_module int.

:- type bar(T) ---> bar(T).

:- pred foo(bar(int) :: out) is det.

:- implementation.

foo(Bar) :- make_bar(Bar).

:- some[T] pred make_bar(bar(T) :: out) is det.
make_bar(bar(42)).
