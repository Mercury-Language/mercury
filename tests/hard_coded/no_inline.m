%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%

:- module no_inline.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- pragma promise_pure(main/2).
main(!IO) :-
    impure bar(A),
    impure bar(B),
    impure bar(C),
    impure bar(D),
    io.write_line([A, B, C, D], !IO).

:- pragma no_inline(bar/1).
:- impure pred bar(int::out) is det.

:- pragma foreign_proc("C",
    bar(Value::out),
    [will_not_call_mercury],
"
{
    static int counter = 0;

    Value = counter++;
}
").

:- pragma foreign_code("C#",
"
    static int counter = 0;
").
:- pragma foreign_proc("C#",
    bar(Value::out),
    [],
"
    Value = counter++;
").

:- pragma foreign_code("Java",
"
    static int counter = 0;
").
:- pragma foreign_proc("Java",
    bar(Value::out),
    [],
"
    Value = counter++;
").
