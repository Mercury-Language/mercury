:- module stable_foreign.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
	init(Base),
	offset(Base, 10, First),
	offset(Base, 42, Second),
	offset(Base, 77, Third),
	test(First, First, !IO),
	test(First, Second, !IO),
	test(First, Third, !IO),
	test(Second, Second, !IO),
	test(Second, First, !IO),
	test(Second, Third, !IO),
	test(Third, First, !IO),
	test(Third, Second, !IO),
	test(Third, Third, !IO).

:- pred test(ptr::in, ptr::in, io::di, io::uo) is cc_multi.

test(P1, P2, !IO) :-
	rep(P1, V1),
	rep(P2, V2),

	io__write_int(V1, !IO),
	( unify(P1, P2) ->
		io__write_string(" u= ", !IO)
	;
		io__write_string(" u!= ", !IO)
	),
	io__write_int(V2, !IO),
	io__write_string("\n", !IO),

	compare(R, P1, P2),
	io__write_int(V1, !IO),
	(
		R = (<),
		io__write_string(" c< ", !IO)
	;
		R = (=),
		io__write_string(" c= ", !IO)
	;
		R = (>),
		io__write_string(" c> ", !IO)
	),
	io__write_int(V2, !IO),
	io__write_string("\n", !IO),

	compare_representation(RR, P1, P2),
	io__write_int(V1, !IO),
	(
		RR = (<),
		io__write_string(" r< ", !IO)
	;
		RR = (=),
		io__write_string(" r= ", !IO)
	;
		RR = (>),
		io__write_string(" r> ", !IO)
	),
	io__write_int(V2, !IO),
	io__write_string("\n", !IO).

:- pragma foreign_decl(c, "
#define	STABLE_FOREIGN_MAX 100
static int	stable_foreign_array[STABLE_FOREIGN_MAX];
").

:- type ptr.
:- pragma foreign_type(c, ptr, "int *", [can_pass_as_mercury_type, stable]).

:- pred init(ptr::out) is det.

:- pragma foreign_proc(c,
	init(Ptr::out),
	[will_not_call_mercury, promise_pure],
"
	int	i;

	for (i = 0; i < STABLE_FOREIGN_MAX ; i++) {
		stable_foreign_array[i] = i/2;
	}

	Ptr = &stable_foreign_array[0];
").

:- pred offset(ptr::in, int::in, ptr::out) is det.

:- pragma foreign_proc(c,
	offset(Base::in, N::in, Ptr::out),
	[will_not_call_mercury, promise_pure],
"
	/* Base */
	if (0 <= N && N < STABLE_FOREIGN_MAX) {
		Ptr = &stable_foreign_array[N];
	} else {
		MR_fatal_error(""bad offset"");
	}
").

:- pred rep(ptr::in, int::out) is det.

:- pragma foreign_proc(c,
	rep(Ptr::in, Val::out),
	[will_not_call_mercury, promise_pure],
"
	Val = *Ptr;
").
