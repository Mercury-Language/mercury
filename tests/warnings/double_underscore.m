:- module double_underscore.

:- interface.

:- import_module int.

:- pred p(int::in, int::out) is det.

:- implementation.

p(_X, _X).
