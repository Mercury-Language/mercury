%
% Test the require tail recursion pragma with the --warn-non-tail-recursion
% option.  These tests do not raise an error, the tests that do raise errors
% are in invalid/
%

:- module require_tailrec_1.

:- interface.

:- import_module int.
:- import_module list.

:- pred qsortapp(list(int)::in, list(int)::out) is det.

:- implementation.

:- pragma require_tail_recursion(qsortapp/2, [none]).

qsortapp([], []).
qsortapp([Pivot | T], List) :-
    partition(Pivot, T, [], Left0, [], Right0),
    qsortapp(Left0, Left),
    qsortapp(Right0, Right),
    append(Left, [Pivot | Right], List).

:- pred partition(int::in, list(int)::in, list(int)::in, list(int)::out,
    list(int)::in, list(int)::out) is det.
:- pragma require_tail_recursion(partition/6).

partition(_Pivot, [], Left, Left, Right, Right).
partition(Pivot, [H | T], Left0, Left, Right0, Right) :-
    ( if H < Pivot then
        partition(Pivot, T, [H | Left0], Left, Right0, Right)
    else
        partition(Pivot, T, Left0, Left, [H | Right0], Right)
    ).

