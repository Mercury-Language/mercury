:- module double_underscore.

:- interface.



:- pred p(int::in, int::out) is det.

:- implementation.

p(_X, _X).
