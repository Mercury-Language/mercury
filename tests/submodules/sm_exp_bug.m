%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Modules compiled by rotd-2007-02-27 and before sometimes did not
% implicitly foreign_import themselves.  This meant that procedures
% exported via a pragma foreign_export were not visible to foreign
% code in the same module.  (This only showed up with the LLDS backend
% since the MLDS->C code generator always inserted the necessary foreign
% import regardless of whether it was present in the HLDS or not.)

:- module sm_exp_bug.
:- interface.

:- include_module sm_exp_bug.child.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    call_foreign(!IO).

:- pred call_foreign(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    call_foreign(IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    WRITE_HELLO();
    IO = IO0;
").

:- pragma foreign_proc("C#",
    call_foreign(IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    WRITE_HELLO();
    IO = IO0;
").

:- pragma foreign_proc("Java",
    call_foreign(IO0::di, IO::uo),
    [may_call_mercury, promise_pure],
"
    WRITE_HELLO();
    IO = IO0;
").

:- pragma foreign_export("C", write_hello(di, uo), "WRITE_HELLO").
:- pragma foreign_export("C#", write_hello(di, uo), "WRITE_HELLO").
:- pragma foreign_export("Java", write_hello(di, uo), "WRITE_HELLO").
:- pred write_hello(io::di, io::uo) is det.

write_hello(!IO) :-
    io.write_string("Hello World!\n", !IO).
