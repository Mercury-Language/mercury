:- module mode_choice.
:- interface.
:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.

main -->
	( { test1("foo", T0, T0b) } ->
		print("T0: "), print(T0), nl,
		print("T0b: "), print(T0b), nl
	;
		print("test1: failed")
	),
	( { test1("foo", "fooie", T1) } ->
		print("T1: "), print(T1), nl
	;
		print("test1: failed")
	),
	{ test2("bar", T2) },
	print("T2: "), print(T2), nl,
	{ Z = "z" },
	{ test2(Z, T3) },
	print("T3: "), print(T3), nl,
	{ test2(Z, T4) },
	print("T4: "), print(T4), nl,
	{ mkany(Any) },
	{ test3(Any, T5) },
	print("T5: "), print(T5), nl,
	{ test3(_, T6) },
	print("T6: "), print(T6), nl,
	( { test4("", "", T7) } ->
		print("T7: "), print(T7), nl
	;
		print("T7 failed\n")
	),
	{ test4("", T8, T9) },
	print("T8: "), print(T8), nl,
	print("T9: "), print(T9), nl,
	{ test4(T10, "", T11) },
	print("T10: "), print(T10), nl,
	print("T11: "), print(T11), nl,
	{ test5("a", "b", T12) },
	print("T12: "), print(T12), nl,
	{ test5("a", "a", T13) },
	print("T13: "), print(T13), nl,
	{ test5("b", "b", T14) },
	print("T14: "), print(T14), nl.

% prefer `in' to `out'

:- pred test1(string, string, string).
:- mode test1(in, out, out) is semidet.
:- mode test1(in, in, out) is semidet.

:- pragma c_code(test1(_A::in, B::out, C::out), will_not_call_mercury, "
	B = C = ""test1(in, out, out)"";
	SUCCESS_INDICATOR = MR_TRUE;
").

:- pragma c_code(test1(_A::in, _B::in, C::out), will_not_call_mercury, "
	C = ""test1(in, in, out)"";
	SUCCESS_INDICATOR = MR_TRUE;
"). 

% prefer `di' to `uo'

:- pred test2(string, string).
:- mode test2(in, out) is det.
:- mode test2(di, uo) is det.

:- pragma c_code(test2(_A::in, B::out), will_not_call_mercury, "
	B = ""test2(in, out)"";
").

:- pragma c_code(test2(_A::di, B::uo), will_not_call_mercury, "
	B = ""test2(di, uo)"";
").

/******* `ui' modes not yet supported
% prefer `ui' to `in'

:- pred test2b(string, string).
:- mode test2b(in, out) is det.
:- mode test2b(ui, uo) is det.

:- pragma c_code(test2b(_A::in, B::out), will_not_call_mercury, "
	B = ""test2b(in, out)"";
").

:- pragma c_code(test2(_A::ui, B::out), will_not_call_mercury, "
	B = ""test2b(ui, out)"";
").
*******/

:- pred mkany(string::out(any)) is det.
:- pragma c_code(mkany(S::out(any)), will_not_call_mercury, "
	S = NULL;
").

% prefer in(any) over out(any)
% [i.e. any -> any beats free -> any]

:- pred test3(string, string).
:- mode test3(in(any), out) is det.
:- mode test3(out(any), out) is det.

:- pragma c_code(test3(_A::in(any), B::out), will_not_call_mercury, "
	B = ""test3(in(any), out)"";
").

:- pragma c_code(test3(A::out(any), B::out), will_not_call_mercury, "
	A = NULL;
	B = ""test3(out(any), out)"";
").

% for non-comparable modes, pick the first one

:- pred test4(string, string, string).
:- mode test4(in, out, out) is det.
:- mode test4(out, in, out) is det.

:- pragma c_code(test4(_A::in, B::out, C::out), will_not_call_mercury, "
	B = """";
	C = ""test4(in, out, out)"";
").

:- pragma c_code(test4(A::out, _B::in, C::out), will_not_call_mercury, "
	A = """";
	C = ""test4(out, in, out)"";
").

% for non-comparable modes, pick the first one

:- inst a == bound("a").
:- inst b == bound("b").
:- inst ab == bound("a" ; "b").

:- pred test5(string, string, string).
:- mode test5(in(a), in(ab), out) is det.
:- mode test5(in(ab), in(b), out) is det.

:- pragma c_code(test5(_A::in(a), _B::in(ab), C::out), will_not_call_mercury, "
	C = ""test5(in(a), in(ab), out)"";
").

:- pragma c_code(test5(_A::in(ab), _B::in(b), C::out), will_not_call_mercury, "
	C = ""test5(in(ab), in(b), out)"";
").

