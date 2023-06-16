%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lookup_switch_simple_bitvec.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pair.
:- import_module string.

main(!IO) :-
    test(1, !IO),
    test(2, !IO),
    test(3, !IO),
    test(4, !IO),
    test(5, !IO),
    test(6, !IO),
    test(7, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    ( if p(N, A, B, C, D) then
        io.format("N = %d: ", [i(N)], !IO),
        io.write(A, !IO),
        io.write_string(" ", !IO),
        io.write(B, !IO),
        io.write_string(" ", !IO),
        io.write(C, !IO),
        io.write_string(" ", !IO),
        io.write(D, !IO),
        io.write_string("\n", !IO)
    else
        io.format("N = %d: no solution\n", [i(N)], !IO)
    ).

:- pred p(int::in, int::out, float::out, string::out, pair(int)::out)
    is semidet.

p(2, 22, 2.2, "two",   222 - 222).
p(3, 33, 3.3, "three", 222 - 223).
p(5, 55, 5.5, "five",  222 - 225).
p(6, 66, 6.6, "six",   222 - 226).
p(7, 77, 7.7, "seven", 222 - 227).
