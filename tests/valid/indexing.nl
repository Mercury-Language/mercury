:- module indexing.

:- type t5
	--->	a ; b ; c ; d ; e.

:- type t32
	--->	c0
	;	c1
	;	c2
	;	c3
	;	c4
	;	c5
	;	c6
	;	c7
	;	c8
	;	c9
	;	c10
	;	c11
	;	c12
	;	c13
	;	c14
	;	c15
	;	c16
	;	c17
	;	c18
	;	c19
	;	c20
	;	c21
	;	c22
	;	c23
	;	c24
	;	c25
	;	c26
	;	c27
	;	c28
	;	c29
	;	c30
	;	c31.

:- pred fully_dense(t5::in, int::out) is det.
fully_dense(a, 1).
fully_dense(b, 2).
fully_dense(c, 3).
fully_dense(d, 4).
fully_dense(e, 5).

:- pred semi_dense(t5::in, int::out) is semidet.
semi_dense(a, 1).
semi_dense(c, 3).
semi_dense(d, 4).

:- pred sparse(t32::in, int::out) is semidet.
sparse(c0, 0).
sparse(c1, 1).


:- pred semi_dense_int(int::in, int::out) is semidet.
semi_dense_int(2, 1).
semi_dense_int(4, 3).
semi_dense_int(6, 4).

