%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for bugs in the handling of purity by the optimization passes.

:- module purity_opt.
:- interface.
:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.
:- import_module std_util.

main(!IO) :-
    ( if impure test1(1), impure test1(2), impure test1(3) then
        semipure get(C1),
        io.write_int(C1, !IO),
        io.nl(!IO)
    else
        error("test1 failed")
    ).

:- impure pred test1(int::in) is semidet.
:- pragma no_inline(test1/1).

test1(X) :-
    (
        impure incr(_),
        fail
    ;
        true
    ),
    ( if X > 1 then
        Z = X - 1
    else
        Z = 3
    ),
    Z < 4.

:- pragma foreign_decl("C", "int counter;").
:- pragma foreign_code("C", "int counter = 1;").

:- pragma foreign_code("C#", "static int counter = 1;").
:- pragma foreign_code("Java", "static int counter = 1;").

:- impure pred incr(int::out) is det.

:- pragma foreign_proc("C",
    incr(Val::out),
    [will_not_call_mercury],
"
    counter++; Val = counter;
").
:- pragma foreign_proc("C#",
    incr(Val::out),
    [will_not_call_mercury],
"
    counter++; Val = counter;
").
:- pragma foreign_proc("Java",
    incr(Val::out),
    [will_not_call_mercury],
"
    counter++; Val = counter;
").

:- semipure pred get(int::out) is det.

:- pragma foreign_proc("C",
    get(Val::out),
    [will_not_call_mercury, promise_semipure],
"
    Val = counter
").
:- pragma foreign_proc("C#",
    get(Val::out),
    [will_not_call_mercury, promise_semipure],
"
    Val = counter;
").
:- pragma foreign_proc("Java",
    get(Val::out),
    [will_not_call_mercury, promise_semipure],
"
    Val = counter;
").
