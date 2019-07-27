%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order_func_test.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    L1 = [1, 2, 3],
    L2 = my_map((func(X::in) = (Y::out) is det :- Y = 2*X), L1),
    L3 = my_map((func(X2) = Y2 :- Y2 = 5*X2), L2),
    L = my_map(func(X3) = 10*X3, L3),
    list.foldl(output_int_and_string(" "), L, !IO),
    io.nl(!IO).

:- func my_map(func(X) = Y, list(X)) = list(Y).
:- mode my_map(func(in) = out is det, in) = out is det.
:- mode my_map(func(in) = out is semidet, in) = out is semidet.

my_map(_, []) = [].
my_map(F, [H0 | T0]) = [apply(F, H0) | my_map(F, T0)].

:- pred output_int_and_string(string::in, int::in, io::di, io::uo) is det.

output_int_and_string(Str, N, !IO) :-
    io.write_int(N, !IO),
    io.write_string(Str, !IO).
