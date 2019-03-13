%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test io.print etc with foreign types in the Java grades.
%
%---------------------------------------------------------------------------%

:- module java_print_foreign.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
   io.print_line(get_foreign_bool, !IO),
   io.print_line(get_foreign_char, !IO),
   io.print_line(get_foreign_int, !IO),
   io.print_line(get_foreign_long, !IO),
   io.print_line(get_foreign_float, !IO),
   io.print_line(get_foreign_double, !IO),
   io.print_line(get_uuid, !IO),
   io.print_line(null_uuid, !IO),
   io.print_line(get_local_date, !IO),
   io.print_line(null_local_date, !IO).

:- type foreign_bool.
:- pragma foreign_type("Java", foreign_bool, "boolean").
:- func get_foreign_bool = foreign_bool.
:- pragma foreign_proc("Java",
    get_foreign_bool = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = true;
").

:- type foreign_int.
:- pragma foreign_type("Java", foreign_int, "int").
:- func get_foreign_int = foreign_int.
:- pragma foreign_proc("Java",
    get_foreign_int = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = 561;
").

:- type foreign_char.
:- pragma foreign_type("Java", foreign_char, "char").
:- func get_foreign_char = foreign_char.
:- pragma foreign_proc("Java",
    get_foreign_char = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = 'A';
").

:- type foreign_long.
:- pragma foreign_type("Java", foreign_long, "long").
:- func get_foreign_long = foreign_long.
:- pragma foreign_proc("Java",
    get_foreign_long = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = 561L;
").

:- type foreign_float.
:- pragma foreign_type("Java", foreign_float, "float").
:- func get_foreign_float = foreign_float.
:- pragma foreign_proc("Java",
    get_foreign_float = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = java.lang.Float.MAX_VALUE;
").

:- type foreign_double.
:- pragma foreign_type("Java", foreign_double, "double").
:- func get_foreign_double = foreign_double.
:- pragma foreign_proc("Java",
    get_foreign_double = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = java.lang.Double.MAX_VALUE;
").

:- type uuid.
:- pragma foreign_type("Java", uuid, "java.util.UUID").
:- func get_uuid = uuid.
:- pragma foreign_proc("Java",
    get_uuid = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = java.util.UUID.fromString(""5f4fe224-2d69-49f0-9c72-acb9ec8400ef"");
").

:- func null_uuid = uuid.
:- pragma foreign_proc("Java",
    null_uuid = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = null;
").

:- type local_date.
:- pragma foreign_type("Java", local_date, "java.time.LocalDate").
:- func get_local_date = local_date.
:- pragma foreign_proc("Java",
    get_local_date = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = java.time.LocalDate.MAX;
").

:- func null_local_date = local_date.
:- pragma foreign_proc("Java",
    null_local_date = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = null;
").


