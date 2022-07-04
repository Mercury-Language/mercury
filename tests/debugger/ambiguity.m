%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ambiguity.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module ambiguity_helper.

:- import_module float.
:- import_module int.
:- import_module list.

%---------------------%

:- type t
    --->    t1
    ;       t2.

:- type t2
    --->    t1(int)
    ;       t1(int, float)
    ;       u1(t2).

:- type t(T)
    --->    u1
    ;       u2(T).

%---------------------%

main(!IO) :-
    io.write_line(p(2.5), !IO),
    io.write_line(p(1, t1), !IO),
    io.write_line(p(0, 1, u2(42)), !IO),
    p(1, X),
    io.write_line(X, !IO),

    ListA = [1, 2],
    ListB = [3.0, 4.0, 5.0],
    get_length_sum(ListA, SumA, LenA),
    get_length_sum_via_acc(ListB, _SumB, LenB),
    add_int(SumA, add_int(LenA, LenB), TotalInt),
    io.write_line(TotalInt, !IO),

    add_float(3.3, add_float(4.4, 5.5), TotalFloat),
    io.write_line(TotalFloat, !IO).

%---------------------%

:- func p(float) = float.

p(F) = F.

:- func p(int, t) = t.

p(_, T) = T.

:- func p(int, int, t(T)) = t(T).

p(_, _, T) = T.

:- pred p(int::in, int::out) is det.

p(I, I).
