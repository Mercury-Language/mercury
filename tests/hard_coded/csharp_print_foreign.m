%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% Test io.print etc with foreign types in the C# grades.
%
%---------------------------------------------------------------------------%

:- module csharp_print_foreign.
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
   io.print_line(null_object, !IO).

:- type foreign_bool.
:- pragma foreign_type("C#", foreign_bool, "bool").
:- func get_foreign_bool = foreign_bool.
:- pragma foreign_proc("C#",
    get_foreign_bool = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = true;
").

:- type foreign_int.
:- pragma foreign_type("C#", foreign_int, "int").
:- func get_foreign_int = foreign_int.
:- pragma foreign_proc("C#",
    get_foreign_int = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = 561;
").

:- type foreign_char.
:- pragma foreign_type("C#", foreign_char, "char").
:- func get_foreign_char = foreign_char.
:- pragma foreign_proc("C#",
    get_foreign_char = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = 'A';
").

:- type foreign_long.
:- pragma foreign_type("C#", foreign_long, "long").
:- func get_foreign_long = foreign_long.
:- pragma foreign_proc("C#",
    get_foreign_long = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = 561L;
").

:- type foreign_float.
:- pragma foreign_type("C#", foreign_float, "float").
:- func get_foreign_float = foreign_float.
:- pragma foreign_proc("C#",
    get_foreign_float = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = System.Single.MaxValue;
").

:- type foreign_double.
:- pragma foreign_type("C#", foreign_double, "double").
:- func get_foreign_double = foreign_double.
:- pragma foreign_proc("C#",
    get_foreign_double = (V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = System.Double.MaxValue;
").

:- type uuid.
:- pragma foreign_type("C#", uuid, "System.Guid").
:- func get_uuid = uuid.
:- pragma foreign_proc("C#",
    get_uuid = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = System.Guid.Empty;
").

:- type object.
:- pragma foreign_type("C#", object, "System.Object").
:- func null_object = object.
:- pragma foreign_proc("C#",
    null_object = (U::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = null;
").
