%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module csharp_hello.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pragma foreign_proc("C#",
    main(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    System.Console.WriteLine(""Hello Mercury/C# world"");
").
