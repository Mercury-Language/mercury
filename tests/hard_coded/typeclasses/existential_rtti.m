% Test cases for rtti for existential types.

:- module existential_rtti.

:- interface.

:- import_module io, list.

:- typeclass c(T) where [].

:- typeclass c2(T1, T2) where [].

:- typeclass c3(T1, T2) where [].

:- type f(X) ---> some [T] f(int, T, list(X), int).

:- type myf  ---> some [T] myf(T) => c(T).

:- type f 
	---> 	some [T] f(int, T, int)
	; 	some [X] g(float, X, float)
	.

:- type g 
	---> 	some [T] g(T).

:- type g2 ---> g2(int).

:- type foo ---> foo(string, string).
:- type goo ---> goo ; hoo.

:- type u(X) ---> u(X).

:- type f2(X, Y) ---> some [T1, T2] f2(int, T1, u(X), T2, u(Y), int).

:- type multi  ---> some [T1, T2] multi(T1, T2) => c2(T1, T2).

:- type multi2  ---> some [T1,T2,T3] multi2(T1, T2, T3) 
			=> (c2(T1, T2), c3(T1, T3)).

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util.

:- instance c(int) where [].
:- instance c2(int, string) where [].
:- instance c3(int, float) where [].

main -->
	io__write_string("Writing copies of terms:\n"),
	{ A = 'new myf'(1) },
	{ copy(A, ACopy) },
	io__write(ACopy), io__nl,
		% different types inside
	{ B = 'new f'(1, "hello", 42) },
	{ copy(B, BCopy) },
	io__write(BCopy), io__nl,
	{ C = 'new f'(2, 'w', 42) },
	{ copy(C, CCopy) },
	io__write(CCopy), io__nl,
		% an enum
	{ D = 'new f'(3, goo, 42) },
	{ copy(D, DCopy) },
	io__write(DCopy), io__nl,
		% existential inside an existential
	{ E = 'new f'(4, 'new g'("hello"), 42) },
	{ copy(E, ECopy) },
	io__write(ECopy), io__nl,
		% A no-tag inside
	{ F = 'new f'(5, g2(12), 42) },
	{ copy(F, FCopy) },
	io__write(FCopy), io__nl,
	{ G = 'new f'(6, foo("hello", "world"), 42) },
	{ copy(G, GCopy) },
	io__write(GCopy), io__nl,
	{ H = 'new g'(7.0, 'new g'("hello"), 42.0) },
	{ copy(H, HCopy) },
	io__write(HCopy), io__nl,
		% universally quantified argument.
	{ I = 'new f'(8, u("hello"), 42) },
	{ copy(I, ICopy) },
	io__write(ICopy), io__nl,
		% multiple existentially and universally quantified arguments
	{ J = 'new f2'(9, "hello", u("hello"), 432.1, u("world"), 42) },
	{ copy(J, JCopy) },
	io__write(JCopy), io__nl,
		% multi parameter type class
	{ K = 'new multi'(10, "multiparameter") },
	{ copy(K, KCopy) },
	io__write(KCopy), io__nl,
		% multi parameter type class, multiple constraints
	{ L = 'new multi2'(11, "multiparameter", 42.0) },
	{ copy(L, LCopy) },
	io__write(LCopy), io__nl,

	io__nl,
	io__write_string("Writing some terms:\n"),
	io__write(A), io__nl,
	io__write(B), io__nl,
	io__write(C), io__nl,
	io__write(D), io__nl,
	io__write(E), io__nl,
	io__write(F), io__nl,
	io__write(G), io__nl,
	io__write(H), io__nl,
	io__write(I), io__nl,
	io__write(J), io__nl,
	io__write(K), io__nl,
	io__write(L), io__nl,

	io__nl,
	io__write_string("Writing copies of terms again:\n"),
	io__write(ACopy), io__nl,
	io__write(BCopy), io__nl,
	io__write(CCopy), io__nl,
	io__write(DCopy), io__nl,
	io__write(ECopy), io__nl,
	io__write(FCopy), io__nl,
	io__write(GCopy), io__nl,
	io__write(HCopy), io__nl,
	io__write(ICopy), io__nl,
	io__write(JCopy), io__nl,
	io__write(KCopy), io__nl,
	io__write(LCopy), io__nl,

	io__write_string("Writing deconstructed terms:\n"),
	deconstruct_test(A),
	deconstruct_test(B),
	deconstruct_test(C),
	deconstruct_test(D),
	deconstruct_test(E),
	deconstruct_test(F),
	deconstruct_test(G),
	deconstruct_test(H),
	deconstruct_test(I),
	deconstruct_test(J),
	deconstruct_test(K),
	deconstruct_test(L).

:- pred deconstruct_test(T::in, io__state::di, io__state::uo) is det.

deconstruct_test(Term) -->
	{ deconstruct(Term, Functor, Arity, Args) },
	io__write_string(Functor),
	io__write_string("/"),
	io__write_int(Arity),
	io__nl,
	io__write_list(Args, ", ", io__write),
	io__nl.
