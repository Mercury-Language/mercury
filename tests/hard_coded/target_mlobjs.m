%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module target_mlobjs.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    c_write_string("Hello, world\n", !IO).

:- pragma foreign_decl("C", "#include ""target_mlobjs_c.h""").

:- pred c_write_string(string::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    c_write_string(Message::in, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure],
"
    c_write_string(Message);
    IO = IO0;
").
c_write_string(Str, !IO) :-
    io.write_string(Str, !IO).
