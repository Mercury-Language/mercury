        :- module copy_pred.
        :- interface.
        :- import_module io.

        :- pred main(io__state::di, io__state::uo) is det.

        :- implementation.

        :- import_module list, map.
        :- import_module std_util, string.

        main -->
                { F = foo(10, 20) },
                { copy(F, F2) },
                io__set_globals(univ(F2)),
		io__get_globals(Univ),
		{ det_univ_to_type(Univ, F3) },
		{ inst_cast(F3, F4) },
		{ F4("blah", S) },
		print(S), nl.

	:- pred inst_cast(pred(string, string), pred(string, string)).
	:- mode inst_cast(in, out(pred(in, out) is det)) is det.
	:- pragma c_code(inst_cast(X::in, Y::out(pred(in, out) is det)),
		[will_not_call_mercury, thread_safe], "Y = X").
	:- pragma foreign_proc("C#",
		inst_cast(X::in, Y::out(pred(in, out) is det)),
		[will_not_call_mercury, thread_safe, promise_pure], "Y = X;").
	:- pragma foreign_proc("Java",
		inst_cast(X::in, Y::out(pred(in, out) is det)),
		[will_not_call_mercury, thread_safe, promise_pure], "Y = X;").

        :- pred foo(int, int, string, string) is det.
        :- mode foo(in, in, in, out) is det.
        foo(A, B, S0, S) :-
		string__format("%d, %d, %s", [i(A), i(B), s(S0)], S).

