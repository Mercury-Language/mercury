%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lookup_switch_simple_cond.

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
    test(4, !IO).

:- pred test(int::in, io::di, io::uo) is det.

test(N, !IO) :-
    io.format("N = %d: ", [i(N)], !IO),
    ( if p(N, A, B, C, D) then
        io.write(A, !IO),
        io.write_string(" ", !IO),
        io.write(B, !IO),
        io.write_string(" ", !IO),
        io.write(C, !IO),
        io.write_string(" ", !IO),
        io.write(D, !IO),
        io.write_string("\n", !IO)
    else
        io.format("no solution for %d\n", [i(N)], !IO)
    ).

:- pragma inline(p/5).
:- pred p(int::in, int::out, float::out, string::out, pair(int)::out)
    is semidet.

p(2, 22, 2.2, "two",    222 - 223).
p(3, 33, 3.3, "three",  222 - 224).
p(4, 44, 4.4, "four",   222 - 225).
p(5, 55, 5.5, "five",   222 - 226).
