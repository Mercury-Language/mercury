%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case tests using `pragma foreign_export' on a procedure defined
% in a different module (in this case, one from the standard library).
% Previously the MLDS back-end was failing this test case.

:- module pragma_export.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    my_write_string("pragma_export test succeeded\n", !IO).

:- pred my_write_string(string::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    my_write_string(Str::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe],
"
    write_str(Str);
").

:- pragma foreign_proc("C#",
    my_write_string(Str::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe],
"
    write_str(Str);
").

:- pragma foreign_proc("Java",
    my_write_string(Str::in, _IO0::di, _IO::uo),
    [promise_pure, may_call_mercury, thread_safe],
"
    write_str(Str);
").

:- pragma foreign_export("C", io.write_string(in, di, uo), "write_str").
:- pragma foreign_export("C#", io.write_string(in, di, uo), "write_str").
:- pragma foreign_export("Java", io.write_string(in, di, uo), "write_str").
