%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .exp file is for when int is 64-bit.
% The .exp2 file is for when int is 32-bit.
%---------------------------------------------------------------------------%

:- module unsigned_lt_le.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    F1 = f1,
    F2 = f2,
    F3 = f3,
    test(F1, F1, !IO),
    test(F1, F2, !IO),
    test(F1, F3, !IO),
    test(F2, F1, !IO),
    test(F2, F2, !IO),
    test(F2, F3, !IO),
    test(F3, F1, !IO),
    test(F3, F2, !IO),
    test(F3, F3, !IO).

:- pred test(int::in, int::in, io::di, io::uo) is det.

test(A, B, !IO) :-
    ( if A `private_builtin.unsigned_lt` B then
        LT = "true"
    else
        LT = "false"
    ),
    ( if A `private_builtin.unsigned_le` B then
        LE = "true"
    else
        LE = "false"
    ),
    io.format("%16x unsigned_lt %16x = %s\n", [i(A), i(B), s(LT)], !IO),
    io.format("%16x unsigned_le %16x = %s\n", [i(A), i(B), s(LE)], !IO).

:- func f1 = int.
:- func f2 = int.
:- func f3 = int.

f1 = -42.
f2 = 42.
f3 = 18.
