:- module pragma_c_code_no_det.

:- interface.

:- pred test(int::out) is det.

:- implementation.

test(Int) :-
	c_code(Int).

:- pred c_code(int::out).
:- pragma c_code(c_code(X::out), "X = 1").
