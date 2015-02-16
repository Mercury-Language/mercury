% vim: ts=4 sw=4 ts ft=mercury
%
% Test case to test the par_loop_control.m's handling of right recursion.
% The code is a simplified version of map_foldl.

:- module rr.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    mf([1, 2, 3], 0, Result),
    io.write_int(Result, !IO),
    nl(!IO).

:- pred mf(list(int)::in, int::in, int::out) is det.

mf([], !Acc).
mf([X | Xs], !Acc) :-
    (
        Y = X + 1,
        !:Acc = !.Acc + Y
    &
        mf(Xs, !Acc)
    ).

