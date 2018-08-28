%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module higher_order.

:- interface.
:- import_module io.

:- pred main(io::di, state::uo) is det.

:- implementation.

:- import_module int.
:- import_module float.
:- import_module list.

main(!IO) :-
    IntIn = [1, 2, 3, 4, 5],
    FloatIn = [1.0, 2.0, 3.0, 4.0, 5.0],
    IntListIn = [[1, 2], [3, 4, 5]],
    StringListIn = [["one", "two"], ["three", "four", "five"]],

    domap(float_add2(3.0), FloatIn, FloatAdd2Out),
    domap(float_op3(4.0, 5.0), FloatIn, FloatOp3Out),
    domap(int.max(3), IntIn, IntMaxOut),
    domap(do_append([6]), IntListIn, IntAppendOut),
    domap(do_append(["a"]), StringListIn, StringAppendOut),

    io.write_line(FloatAdd2Out, !IO),
    io.write_line(FloatOp3Out, !IO),
    io.write_line(IntMaxOut, !IO),
    io.write_line(IntAppendOut, !IO),
    io.write_line(StringAppendOut, !IO).

:- pred domap(pred(X, Y), list(X), list(Y)).
:- mode domap(pred(in, out) is det, in, out) is det.

domap(_, [],  []).
domap(P, [H0 | T0], [H | T]) :-
    P(H0, H),
    domap(P, T0, T).

:- pred float_add2(float::in, float::in, float::out) is det.

float_add2(A, B, A + B).

:- pred float_op3(float::in, float::in, float::in, float::out) is det.

float_op3(A, B, C, A + B * C).

:- pred do_append(list(T)::in, list(T)::in, list(T)::out) is det.

do_append(A, B, C) :-
    list.append(A, B, C).
