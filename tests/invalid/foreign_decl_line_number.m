%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that we report the correct line number for errors in
% pragma foreign_decl, pragma foreign_code, and pragma foreign_proc.
%
% Note that we can't use #error in pragma foreign_type,
% since C preprocessor declarations are not permitted there.
% So that case is tested separately in foreign_type_line_number.m.

:- module foreign_decl_line_number.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main -->
    foo(_).

:- pragma foreign_decl("C", "
#error Error in foreign decl
").

:- pragma foreign_code("C", "
#error Error in foreign code
").

:- type my_foreign_type.
:- pragma foreign_type("C", my_foreign_type, "int").

:- pragma foreign_export("C", bar(out, di, uo), "bar").
:- pred bar(my_foreign_type::out, io::di, io::uo) is det.
bar(X, !IO) :-
    foo(X, !IO).

:- pred foo(my_foreign_type::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    foo(_output::out, _io0::di, _io::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#error Error in foreign proc
").
