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

main(!IO) :-
    Array = array.generate(10, (func(I) = I * 2)),
    io.write(Array, !IO),
    io.nl(!IO),
    array.generate_foldl(10, gen_elem, Array2, !IO),
    io.write(Array2, !IO),
    io.nl(!IO).

:- pred gen_elem(int::in, int::out, io::di, io::uo) is det.

gen_elem(Index, Result, !IO) :-
    Result = Index * 3,
    io.format("Array2[%d] = %d\n", [i(Index), i(Result)], !IO).
