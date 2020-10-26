%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% foreign proc

:- module dep_par_10.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
        X = fp(1)
    &
        Y = fp(X)
    ),
    io.print(X*Y, !IO),
    io.nl(!IO).

:- func fp(int) = int.

:- pragma foreign_proc("C",
    fp(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = X+1;
").
:- pragma foreign_proc("C#",
    fp(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = X+1;
").
:- pragma foreign_proc("Java",
    fp(X::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = X+1;
").
