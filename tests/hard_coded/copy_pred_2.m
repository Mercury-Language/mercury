	:- module copy_pred_2.
	:- interface.
	:- import_module io.

	:- pred main(io__state::di, io__state::uo) is det.

	:- implementation.

	:- import_module list, map.
	:- import_module std_util, string.

	main -->
		{ make_closure(10, 20, P0) },
		io__write_string("copying\n"),
		{ copy(P0, P1) },
		{ inst_cast(P1, P) },
		io__write_string("calling\n"),
		{ P("blah", S) },
		io__write_string("printing\n"),
		print(S), nl.

	:- pred make_closure(T, T, pred(string, string)).
	:- mode make_closure(in, in, out(pred(in, out) is det)) is det.
	:- pragma no_inline(make_closure/3).

	make_closure(A, B, foo(A, B)).

	:- pred inst_cast(pred(string, string), pred(string, string)).
	:- mode inst_cast(in, out(pred(in, out) is det)) is det.
	:- pragma c_code(inst_cast(X::in, Y::out(pred(in, out) is det)),
		[will_not_call_mercury, thread_safe], "Y = X").

	:- pred foo(T, T, string, string).
	:- mode foo(in, in, in, out) is det.
	foo(A, B, S0, S) :-
		functor(A, FA, _),
		functor(B, FB, _),
		string__format("%s, %s, %s",
			[s(FA), s(FB), s(S0)], S).
