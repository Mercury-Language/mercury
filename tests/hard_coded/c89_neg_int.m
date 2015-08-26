%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module c89_neg_int.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    show(-2147483647, !IO),
    show(-2147483648, !IO),
    % Disabled as they will be rejected by a compiler with 32-bit ints.
    % show(-2147483649, !IO),
    % show(-9223372036854775807, !IO),
    % show(-9223372036854775808, !IO),
    true.

:- pred show(int::in, io::di, io::uo) is det.

show(N, !IO) :-
    io.write_int(N, !IO),
    io.nl(!IO).
