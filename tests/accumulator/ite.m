%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

% Tests that if-then-elses are handled correctly.

:- module ite.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    io.write_string("factorial: ", !IO),
    Factorial = factorial(7),
    io.write_line(Factorial, !IO),
    io.write_string("sort_of_factorial: ", !IO),
    sort_of_factorial(3, Factorial2),
    io.write_line(Factorial2, !IO).

:- func factorial(int) = int.

factorial(Num)
    = ( if Num = 0 then 1 else Num * factorial(Num - 1)).

:- pred sort_of_factorial(int, int).

sort_of_factorial(Num, Fac) :-
    % Here we bind a value in the condition and use it in the then part,
    % in an attempt to confuse the compiler.
    ( if
        Num \= 0,
        X = 2
    then
        sort_of_factorial(Num - 1, Fac0),
        Fac = X * Num * Fac0
    else
        Fac = 1
    ).
