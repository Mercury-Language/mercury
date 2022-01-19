% This is a simple example of using the foreign language interface to call the
% Java method System.out.println().

% This source file is hereby placed in the public domain.

:- module short_example.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    java_write_line("Hello, world", !IO).

:- pred java_write_line(string::in, io::di, io::uo) is det.
:- pragma foreign_proc("JAva",
    java_write_line(S::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    System.out.println(S);
").
