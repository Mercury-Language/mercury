:- module cond.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int, maybe.

:- type t
	--->	empty
	;	node(t, int, t).

main(!IO) :-
	test_maybe(!IO),
	test_maybe(!IO),
	test_maybe(!IO),
	test_maybe(!IO),
	test_string(!IO),
	test_string(!IO),
	test_tree(!IO),
	test_tree(!IO),
	test_tree(!IO),
	test_tree(!IO),
	test_tree(!IO).

:- pred test_maybe(io::di, io::uo) is det.

test_maybe(!IO) :-
	p(no, A),
	p(yes(2), B),
	p(yes(3), C),
	io__write([A, B, C], !IO),
	io__nl(!IO).

:- pred test_string(io::di, io::uo) is det.

test_string(!IO) :-
	q("abc", A),
	io__write_string(A, !IO),
	io__nl(!IO),
	q("def", B),
	io__write_string(B, !IO),
	io__nl(!IO),
	q("ghi", C),
	io__write_string(C, !IO),
	io__nl(!IO).

:- pred test_tree(io::di, io::uo) is det.

test_tree(!IO) :-
	r(1, A),
	s(A, AA),
	io__write(AA, !IO),
	io__nl(!IO),
	r(2, B),
	s(B, BB),
	io__write(BB, !IO),
	io__nl(!IO).

:- pred p(maybe(int)::in, maybe(int)::out) is det.

p(X, Y) :-
	(
		X = no,
		Y = no
	;
		X = yes(Z),
		Y = yes(Z + 1)
	).

:- pred q(string::in, string::out) is det.

q(X, Y) :-
	( X = "abc" ->
		Y = "xabcx"
	; X = "def" ->
		Y = "ydefy"
	;
		Y = "else"
	).

:- pred r(int::in, t::out) is det.

r(X, Y) :-
	( X = 0 ->
		Y = empty
	;
		r(X - 1, S),
		Y = node(S, X, S)
	).

:- pred s(t::in, t::out) is det.

s(X, X).
