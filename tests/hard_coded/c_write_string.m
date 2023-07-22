%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module c_write_string.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

%---------------------------------------------------------------------------%

main(!IO) :-
    c_write_string("Hello, world\n", !IO),
    c_write_string("I am 8 today!\n", !IO),
    c_write_string(X, !IO),
    X = "fred\n".

:- pragma foreign_decl("C", "#include <stdio.h>").

:- pred c_write_string(string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    c_write_string(Str::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    fputs(Str, stdout);
").

c_write_string(Str, !IO) :-
    % For the non-C backends.
    io.write_string(Str, !IO).
