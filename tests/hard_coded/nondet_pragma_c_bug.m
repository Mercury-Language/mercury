:- module nondet_pragma_c_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module std_util.

main -->
	print("foo:\n"),
	unsorted_aggregate(foo, print_soln),
	print("bar:\n"),
	unsorted_aggregate(bar, print_soln).

:- pred print_soln(int::in, io::di, io::uo) is det.
print_soln(Soln) --> write(Soln), nl.

:- pred foo(int).
:- mode foo(out) is nondet.
:- pragma c_code(foo(X::out), [may_call_mercury],
        local_vars("
                int state;
        "),
        first_code("
                LOCALS->state = 1;
        "),
        retry_code("
                LOCALS->state++;
        "),
        common_code("
                switch (LOCALS->state) {
                        case 1: X = 20; SUCCEED; break;
                        case 2: X = 10; SUCCEED; break;
                        case 3: X = 42; SUCCEED; break;
                        case 4: X = 99; SUCCEED; break;
                        case 5: FAIL; break;
                }
        ")
).

:- pred bar(int).
:- mode bar(out) is nondet.
:- pragma c_code(bar(X::out), [may_call_mercury],
        local_vars("
                int state;
        "),
        first_code("
                LOCALS->state = 1;
        "),
        retry_code("
                LOCALS->state++;
        "),
        common_code("
                switch (LOCALS->state) {
                        case 1: X = 20; SUCCEED; break;
                        case 2: X = 10; SUCCEED; break;
                        case 3: X = 42; SUCCEED; break;
                        case 4: X = 99; SUCCEED; break;
                        case 5: X = 123; FAIL; break;
                }
        ")
).

