:- module ext_type.

:- interface.
:- import_module bool.

:- some [PT] pred p(bool, PT).
:- mode p(in, out) is det.

:- pred r is semidet.

:- implementation.

p(yes, Y) :-
	Y = q(42).
p(no, Z) :-
	Z = q(43).

r :-
	q(1) = q(2).

:- some [QT] func q(int) = QT.
q(X) = X.

