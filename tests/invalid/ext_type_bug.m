%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ext_type_bug.
:- interface.
:- import_module int.

:- type bar(T)
    --->    bar(T).

:- pred foo(bar(int)::out) is det.

:- pred foo_tuple({bar(int), bar(float)}::out) is det.

:- implementation.

%---------------------------------------------------------------------------%

foo(Bar) :-
    make_bar(Bar).

:- some [T] pred make_bar(bar(T)::out) is det.

make_bar(bar(42)).

%---------------------------------------------------------------------------%

foo_tuple(BarTuple) :-
    make_bar_tuple(BarTuple).

:- some [T, U] pred make_bar_tuple({bar(T), bar(U)}::out) is det.

make_bar_tuple({bar("a"), bar(21)}).

%---------------------------------------------------------------------------%
