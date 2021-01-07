%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the use of existential types,
% including type inference,
% but not including type class constraints.
% This test is designed to test boxing/unboxing
% of types with non-word size, i.e. chars and floats.

:- module existential_float.
:- interface.

:- import_module io.
:- import_module univ.

:- type foo
    --->    left
    ;       right.

:- some [T] func call_univ_value(univ) = T.

:- some [T] func my_exist_c = T.
:- some [T] func my_exist_f = T.
:- some [T] func my_exist_fn = T.

:- some [T] pred my_exist_p_multi(T::out) is multi.
:- some [T] pred my_exist_p_semi(foo::in, T::out) is semidet.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.
:- import_module int.
:- import_module solutions.

main(!IO) :-
    foo(univ(my_exist_c), !IO),
    foo(univ(my_exist_f), !IO),
    foo(univ(my_exist_fn), !IO),
    foo(univ(call_my_exist_c), !IO),
    foo(univ(call_my_exist_f), !IO),
    foo(univ(call_my_exist_fn), !IO),
    io.write_line(my_exist_c, !IO),
    io.write_line(my_exist_f, !IO),
    io.write_line(my_exist_fn, !IO),
    io.write_line(call_my_exist_c, !IO),
    io.write_line(call_my_exist_f, !IO),
    io.write_line(call_my_exist_fn, !IO),
    ( if call_my_exist_p_semi(left, X1) then
        io.write_line(X1, !IO)
    else
        io.print_line("no.", !IO)
    ),
    ( if call_my_exist_p_semi(right, X2) then
        io.write_line(X2, !IO)
    else
        io.print_line("no.", !IO)
    ),
    ( if my_exist_p_semi(left, X3) then
        io.write_line(X3, !IO)
    else
        io.print_line("no.", !IO)
    ),
    ( if my_exist_p_semi(right, X4) then
        io.write_line(X4, !IO)
    else
        io.print_line("no.", !IO)
    ),
    unsorted_solutions(my_univ_p_multi, List),
    io.write_line(List, !IO).

my_exist_c = 'c'.

my_exist_f = 42.0.

my_exist_fn = (func(X) = 2 * X).

my_exist_p_multi(1.0).
my_exist_p_multi(2.0).

my_exist_p_semi(left, 33.3).

call_my_exist_c = my_exist_c.

call_my_exist_f = my_exist_f.

call_my_exist_fn = my_exist_fn.

call_my_exist_p_multi(X) :-
    my_exist_p_multi(X).

call_my_exist_p_semi(A, B) :-
    my_exist_p_semi(A, B).

:- mode my_univ_p_multi(out) is multi.

my_univ_p_multi(univ(X)) :-
    call_my_exist_p_multi(X).

:- pred foo(univ::in, io::di, io::uo) is det.

foo(X, !IO) :-
    io.write_line(univ_value(X), !IO),
    io.write_line(call_univ_value(X), !IO).

call_univ_value(Univ) = univ_value(Univ).
