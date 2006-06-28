% There were problems with this and deforestation and
% --optimise-saved-vars-const.

:- module dep_par_14.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    R = [1, 5, 3, 4, 7, 8, 6, 9, 2, 0],
    p(R, 1, S),
    io.print(S, !IO),
    io.nl(!IO).

:- pred p(list(int)::in, int::in, int::out) is det.

p([], A, A).
p([H|T], A0, A) :-
    (if H = A0 then
        ( p(T, A0, A1)
        & p(T, A1, A)
        )
    else
        A = A0
    ).
