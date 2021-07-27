%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug361.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    print_line(get_n_happy_numbers(8, 1), !IO).

:- func get_n_happy_numbers(int, int) = list(int).

get_n_happy_numbers(NumToFind, N) =
    ( if NumToFind > 0 then
       ( if is_happy(N) then
            [N | get_n_happy_numbers(NumToFind - 1, N + 1)]
       else
            get_n_happy_numbers(NumToFind, N + 1)
       )
    else
       []
    ).

:- pred is_happy(int::in) is nondet.
:- pragma loop_check(is_happy/1).

is_happy(1).
is_happy(N) :- is_happy(sum_sqr_digits(N)).

:- func sum_sqr_digits(int) = int.

sum_sqr_digits(N) =
   ( if N < 10 then sqr(N) else sqr(N mod 10) + sum_sqr_digits(N div 10) ).

:- func sqr(int) = int.

sqr(X) = X * X.
