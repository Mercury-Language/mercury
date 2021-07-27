%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that we report an error for procedures with no mode declarations.
% This is a regression test; Mercury versions prior to 18 Jun 2003
% failed this test.

:- module undeclared_mode.

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
% ERROR: no mode declaration for sort_of_factorial/2

    % Here we bind a value in the Cond goal and use it in the Then goal,
    % in an attempt to confuse the compiler.
sort_of_factorial(Num, Fac) :-
    ( if
        (Num \= 0, X = 2)
    then
        sort_of_factorial(Num - 1, Fac0),
        Fac = X * Num * Fac0
    else
        Fac = 1
    ).
