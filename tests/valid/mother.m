:- module mother.

:- interface.

:- import_module int.

:- type vec
	---> vec(int, int, int, int, int, int, int, int, int, int).

:- pred set(vec, int, int, vec).
:- mode set(in, in, in, out) is det.
:- mode set(in, in(bound(0)), in, out) is det.
:- mode set(in, in(bound(1)), in, out) is det.
:- mode set(in, in(bound(2)), in, out) is det.
:- mode set(in, in(bound(3)), in, out) is det.
:- mode set(in, in(bound(4)), in, out) is det.
:- mode set(in, in(bound(5)), in, out) is det.
:- mode set(in, in(bound(6)), in, out) is det.
:- mode set(in, in(bound(7)), in, out) is det.
:- mode set(in, in(bound(8)), in, out) is det.
:- mode set(in, in(bound(9)), in, out) is det.

:- implementation.

:- import_module require.

set(Vec0, Ind, V, Vec) :-
	Vec0 = vec(A, B, C, D, E, F, G, H, I, J),
	(
		( Ind = 0, Vec1 = vec(V, B, C, D, E, F, G, H, I, J)
		; Ind = 1, Vec1 = vec(A, V, C, D, E, F, G, H, I, J)
		; Ind = 2, Vec1 = vec(A, B, V, D, E, F, G, H, I, J)
		; Ind = 3, Vec1 = vec(A, B, C, V, E, F, G, H, I, J)
		; Ind = 4, Vec1 = vec(A, B, C, D, V, F, G, H, I, J)
		; Ind = 5, Vec1 = vec(A, B, C, D, E, V, G, H, I, J)
		; Ind = 6, Vec1 = vec(A, B, C, D, E, F, V, H, I, J)
		; Ind = 7, Vec1 = vec(A, B, C, D, E, F, G, V, I, J)
		; Ind = 8, Vec1 = vec(A, B, C, D, E, F, G, H, V, J)
		; Ind = 9, Vec1 = vec(A, B, C, D, E, F, G, H, I, V)
		)
	->
		Vec = Vec1
	;
		error("set: out of range")
	).

