:- module double_vn.

:- interface.

:- pred p(int::out) is det.

:- implementation.

:- import_module int.

p(X) :-
	X is 1 \/ 1 \/ 1.
