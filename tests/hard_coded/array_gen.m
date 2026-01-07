%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test array generators.
%

:- module array_gen.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module string.
:- import_module uint.

main(!IO) :-
    ArrayI0 = array.generate(10, (func(I) = I * 2)),
    io.write_string("ArrayI0 = ", !IO),
    io.write_line(ArrayI0, !IO),
    array.generate_foldl(10, gen_elem, ArrayI, !IO),
    io.write_string("ArrayI = ", !IO),
    io.write_line(ArrayI, !IO),

    ArrayU0 = array.ugenerate(10u, (func(I) = I * 4u)),
    io.write_string("ArrayU0 = ", !IO),
    io.write_line(ArrayU0, !IO),
    array.ugenerate_foldl(10u, ugen_elem, ArrayU, !IO),
    io.write_string("ArrayU = ", !IO),
    io.write_line(ArrayU, !IO).

:- pred gen_elem(int::in, int::out, io::di, io::uo) is det.

gen_elem(Index, Result, !IO) :-
    Result = Index * 3,
    io.format("ArrayI[%d] = %d\n", [i(Index), i(Result)], !IO).

:- pred ugen_elem(uint::in, uint::out, io::di, io::uo) is det.

ugen_elem(Index, Result, !IO) :-
    Result = Index * 5u,
    io.format("ArrayU[%u] = %u\n", [u(Index), u(Result)], !IO).
