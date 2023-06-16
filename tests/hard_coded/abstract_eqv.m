%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_eqv.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module abstract_eqv_helper_1.

main(!IO) :-
    test(val1, val2, !IO),
    test(val1, val3, !IO),
    test(val2, val3, !IO).

:- pred test(t_abs::in, t_abs::in, io::di, io::uo) is det.

test(A, B, !IO) :-
    io.write(A, !IO),
    io.write_string(" = ", !IO),
    io.write(B, !IO),
    io.write_string(": ", !IO),
    ( if A = B then
        io.write_string("true\n", !IO)
    else
        io.write_string("false\n", !IO)
    ).
