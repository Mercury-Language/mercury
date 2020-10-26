%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test for bugs in the handling of purity by the optimization passes.

:- module purity_opt.
:- interface.
:- import_module io.

:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.
:- import_module std_util.

main -->
    ( { impure test1(1), impure test1(2), impure test1(3) } ->
        { semipure get(C1) },
        io__write_int(C1),
        io__nl
    ;
        { error("test1 failed") }
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
    ( X > 1 ->
        Z = X - 1
    ;
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
