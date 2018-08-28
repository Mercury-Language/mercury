%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module implied_instance.

:- interface.

:- pred main(io::di, io::uo) is det.

:- import_module io.

:- implementation.

:- import_module int.
:- import_module list.

:- typeclass sumable(A) where [
    pred p(A::in, int::out) is det
].

:- instance sumable(int) where [
    pred(p/2) is copy_int
].

:- instance sumable(list(T)) <= sumable(T) where [
    pred(p/2) is sum_int_list
].

main(!IO) :-
    p(2, SumA),
    p([42, 24, 1, 2, 3], SumB),
    io.write_int(SumA, !IO),
    io.write_string("\n", !IO),
    io.write_int(SumB, !IO),
    io.write_string("\n", !IO).

:- pred copy_int(int, int).
:- mode copy_int(in, out) is det.

copy_int(N, N).

:- pred sum_int_list(list(T), int) <= sumable(T).
:- mode sum_int_list(in, out) is det.

sum_int_list([], 0).
sum_int_list([X | Xs], Sum) :-
    p(X, SumA),
    sum_int_list(Xs, SumB),
    Sum = SumA + SumB.
