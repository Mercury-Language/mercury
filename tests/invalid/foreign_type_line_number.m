%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that we report the correct line number for the error in
% foreign_type.
%
:- module foreign_type_line_number.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    foo(_, !IO).

:- type my_foreign_type.
:- pragma foreign_type("C", my_foreign_type, "
/* We can't use #error here, since C preprocessor directives are
   not permitted in pragma foreign_type.  So instead we just use
   an invalid type name. */
long short int
").

:- pragma foreign_export("C", bar(out, di, uo), "bar").
:- pred bar(my_foreign_type::out, io::di, io::uo) is det.
bar(X, !IO) :-
    foo(X, !IO).

:- pred foo(my_foreign_type::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    foo(_output::out, _io0::di, _io::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
").
