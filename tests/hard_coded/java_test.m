% A test of the Java interface.

:- module java_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int.

:- func foo(int) = int.
/*
XXX `pragma export' not yet supported for Java
:- pragma export(foo(in) = out, "foo").
*/
foo(X) = X + 1.

main -->
	java_write_string("Hello, world\n"),
	( { java_semidet_succeed } ->
		[]
	;
		java_write_string("java_semidet_succeed failed\n")
	),
	( { java_semidet_fail } ->
		java_write_string("java_semidet_fail succeeded\n")
	;
		[]
	).

:- pragma(foreign_decl, "Java", "
	// some Java top-level declarations
	class Foo {}
").

:- pragma(foreign_code, "Java", "
	// some Java in-class declarations
	static void bar() {
/*
XXX `pragma export' not yet supported for Java
		// test `pragma export' functions
		if (foo(42) != 43) {
			throw new java.lang.Error(""bar: foo failed"");
		}
*/
	}
").

:- pred java_write_string(string::in, io__state::di, io__state::uo) is det.

:- pragma(foreign_proc, "Java",
	java_write_string(Message::in, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure],
"
	// a Java procedure
	System.out.print(Message);
	// test that foreign_decl declarations are visible
	Foo f;
	// test that foreign_code declarations are visible
	bar();
").

:- pred java_semidet_succeed is semidet.
:- pred java_semidet_fail is semidet.
:- pragma(foreign_proc, "Java", java_semidet_succeed,
	[will_not_call_mercury, promise_pure], "succeeded = true;").
:- pragma(foreign_proc, "Java", java_semidet_fail,
	[will_not_call_mercury, promise_pure], "succeeded = false;").
