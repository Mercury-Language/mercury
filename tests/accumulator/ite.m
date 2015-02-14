%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that if-then-elses are handled correctly.

:- module ite.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.

main -->
    io__write_string("factorial: "),
    { Factorial = factorial(7) },
    io__write(Factorial),
    io__nl,
    io__write_string("sort_of_factorial: "),
    { sort_of_factorial(3, Factorial2) },
    io__write(Factorial2),
    io__nl.

:- func factorial(int) = int.

factorial(Num)
    = ( Num = 0 -> 1 ; Num * factorial(Num - 1)).

:- pred sort_of_factorial(int, int).

sort_of_factorial(Num, Fac) :-
    % Here we bind a value in the condition and use it in the then part,
    % in an attempt to confuse the compiler.
    (
        (Num \= 0, X = 2)
    ->
        sort_of_factorial(Num - 1, Fac0),
        Fac = X * Num * Fac0
    ;
        Fac = 1
    ).
