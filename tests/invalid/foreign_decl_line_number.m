% Check that we report the correct line number for the error in
% foreign_decl.
:- module foreign_decl_line_number.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main --> foo(_).

:- pragma foreign_decl("C", "
#error Error in foreign decl
").

:- pragma foreign_code("C", "
#error Error in foreign code
").

:- type my_foreign_type.
:- pragma foreign_type("C", my_foreign_type, "
#error Error in foreign type
").

:- pragma export(bar(out,di,uo), "bar").
:- pred bar(my_foreign_type::out, io::di,io::uo) is det.
bar(X) --> foo(X).

:- pred foo(my_foreign_type::out, io::di,io::uo) is det.
:- pragma foreign_proc("C", foo(_output::out, _io0::di, _io::uo),
	[will_not_call_mercury, promise_pure, thread_safe],
"
#error Error in foreign proc
").
