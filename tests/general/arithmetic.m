%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A very basic check of integer arithmetic.
%
% Note: this test makes use of Mercury-specific features (specifically
% the use of "`xor`" rather than "^" for the exclusive or operator,
% and the use of the reverse modes of xor) so it really belongs in
% the `tests/hard_coded' directory, rather than the `tests/general'
% directory... but that distinction is pretty much obsolete now that we
% don't support compiling things with Prolog.

:- module arithmetic.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    test(3, 4, !IO).

:- pred test(int::in, int::in, io::di, io::uo) is det.

test(X, Y, !IO) :-
    Plus = X + Y,
    Times = X * Y,
    Minus = X - Y,
    Div = X // Y,
    Mod = X mod Y,
    LeftShift = X << Y,
    RightShift = X >> Y,
    BitAnd = X /\ Y,
    BitOr = X \/ Y,
    BitXor = X `xor` Y,
    X = BitXor2 `xor` Y,
    Y = X `xor` BitXor3,
    BitNeg = \ X,

    write_message("X", X, !IO),
    write_message("Y", Y, !IO),
    write_message("X + Y", Plus, !IO),
    write_message("X * Y", Times, !IO),
    write_message("X - Y", Minus, !IO),
    write_message("X / Y", Div, !IO),
    write_message("X mod Y", Mod, !IO),
    write_message("X << Y", LeftShift, !IO),
    write_message("X >> Y", RightShift, !IO),
    write_message("X /\\ Y", BitAnd, !IO),
    write_message("X \\/ Y", BitOr, !IO),
    write_message("X `xor` Y", BitXor, !IO),
    write_message("Z such that X = Z `xor` Y", BitXor2, !IO),
    write_message("Z such that Y = X `xor` Z", BitXor3, !IO),
    write_message("\\ X", BitNeg, !IO).

:- pred write_message(string::in, int::in, io::di, io::uo) is det.

write_message(Msg, N, !IO) :-
    io.format("%s: %d\n", [s(Msg), i(N)], !IO).
