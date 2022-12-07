%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether the encoding of ints as uints in the uenum(int) instance
% of the uenum typeclass works.
%
% The .exp file is for 64 bit targets.
% The .exp2 file is for 32 bit targets.
%

:- module int_uenum.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module enum.
:- import_module int.
:- import_module uint.
:- import_module list.
:- import_module string.

main(!IO) :-
    test(0, !IO),
    test(-1, !IO),
    test(1, !IO),
    test(-63, !IO),
    test(-64, !IO),
    test(63, !IO),
    test(64, !IO),
    test(int.min_int + 1, !IO),
    test(int.min_int, !IO),
    test(int.max_int - 1, !IO),
    test(int.max_int, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(I, !IO) :-
    U = to_uint(I),
    ( if from_uint(U, I2) then
        ( if I = I2 then
            io.format("%d %x -> %u %x\n",
                [i(I), i(I), u(U), u(U)], !IO)
        else
            io.format("%d %x -> %u %x, wrong reverse %d %x\n",
                [i(I), i(I), u(U), u(U), i(I2), i(I2)], !IO)
        )
    else
        io.format("%d %x -> %u %x, reverse failed\n",
            [i(I), i(I), u(U), u(U)], !IO)
    ).
