% This tests the case of committing across a nondet goal in a nondet
% context. There was a bug in this, which this test case exercised.

:- module commit_bug_2.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list, std_util.

main -->
	{ solutions(test, List) },
	print_intlist(List).

:- pred print_intlist(list(int)::in, io__state::di, io__state::uo) is det.

print_intlist([]) --> [].
print_intlist([X | L]) -->
	io__write_int(X),
	io__write_string("\n"),
	print_intlist(L).

:- pred test(int::out) is nondet.

test(A) :-
	p(A),
	some [B] (
		q(A, B),
		C is B + 1,
		some [D] (
			r(C, D),
			D > 25
		)
	).

:- pred p(int).
:- mode p(out) is multi.

p(0).
p(1).
p(2).
p(3).

:- pred q(int, int).
:- mode q(in, out) is nondet.

q(0, 1).
q(1, 2).
q(1, 3).
q(2, 4).
q(3, 5).

:- pred r(int, int).
:- mode r(in, out) is nondet.

r(2, 10).
r(3, 20).
r(3, 20).
r(4, 20).
r(4, 20).
r(5, 40).
r(6, 50).
