% This is a simple example of using the foreign language interface to call the
% C# method Console.WriteLine().

% This source file is hereby placed in the public domain.

:- module short_example.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    csharp_write_line("Hello, world", !IO).

:- pragma foreign_decl("C#", "using System;").

:- pred csharp_write_line(string::in, io::di, io::uo) is det.
:- pragma foreign_proc("C#",
    csharp_write_line(S::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Console.WriteLine(S);
").
