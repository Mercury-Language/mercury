:- module export_test2.

:- interface.

:- import_module int, io.

:- pred main(io__state::di, io__state::uo) is det.

:- pred foo(io__output_stream::in, io__output_stream::out,
		int::in, int::out) is det.

:- pred bar(io__output_stream::in, io__output_stream::out,
		int::in, int::out) is det.


:- implementation.

main -->
	io__stdout_stream(Stream0),
	{ bar(Stream0, Stream, 41, X) },
	io__write(Stream, X),
	io__write_char(Stream, '\n').

foo(S, S, X, X+1).

:- pragma foreign_decl("C",
"#include ""mercury_library_types.h""

/*
** Make sure the foreign type definition of io__input_stream
** is available here.  If it is not, the automatically generated
** definition of foo() will be
**	void foo(MR_Word, MR_Word *, MR_Integer, MR_Integer *);
*/
void foo(MercuryFilePtr, MercuryFilePtr *, MR_Integer, MR_Integer *);

").


:- pragma export(foo(in, out, in, out), "foo").

:- pragma foreign_proc("C", bar(S::in, T::out, X::in, Y::out),
		[may_call_mercury, promise_pure],
"
	foo(S, &T, X, &Y);
").
:- pragma foreign_proc("C#", bar(S::in, T::out, X::in, Y::out),
		[may_call_mercury, promise_pure], "
	export_test2.mercury_code.foo(S, ref T, X, ref Y);
").
