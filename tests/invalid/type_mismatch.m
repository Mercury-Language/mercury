:- module type_mismatch.

:- interface.

:- type t1		--->	f1(int, int, int).
:- type t2(T2)		--->	f2(T2, int, int).
:- type t3(T3a, T3b)	--->	f3(T3a, T3b, int).
:- type t4(T4)		--->	f4(T4, int, int).

:- pred p1(t1::in, float::out) is semidet.
:- pred p2(t2(F2)::in, float::out) is semidet.
:- pred p3(t3(F3a, F3b)::in, F3c::out) is semidet.
:- pred p4a(t4(F4)::in, float::out) is semidet.
:- pred p4b(t4(F4)::in, float::out, float::out) is semidet.

:- implementation.

p1(Tuple, Field) :-
	Tuple = f1(_, Field, 5).

p2(Tuple, Field) :-
	Tuple = f2(_, Field, 5).

p3(Tuple, Field) :-
	Tuple = f3(_, Field, 5).

% The error message for p4a could be improved. At the moment we only get
% an error for argument 1, since the transformation to superhomogeneous form
% replaces the second occurrence of Field inside f4 with another variable.

p4a(Tuple, Field) :-
	Tuple = f4(Field, Field, 5).

p4b(Tuple, Field1, Field2) :-
	Tuple = f4(Field1, Field2, 5).
