
:- module dep_par_27.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    List = [1, 2, 3],
    (
        mylength(List, Len)
    &
        square(Len, Len2)
    ),
    io.write_int(Len2, !IO),
    io.nl(!IO).

:- pred mylength(list(_T), int).
:- mode mylength(in, out) is det.
:- pragma no_inline(mylength/2).

mylength(L, N) :-
    mylength_2(L, 0, N).

:- pred mylength_2(list(T), int, int).
:- mode mylength_2(in, in, out) is det.

mylength_2([], N, N).
mylength_2([_ | L1], N0, N) :-
    N1 = N0 + 1,
    mylength_2(L1, N1, N).

:- pred square(int::in, int::out) is det.
:- pragma no_inline(square/2).

square(X, X*X).
