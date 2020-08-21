%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The call in the compose section isn't assocative.
%

:- module simple.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_string("foldr: ", !IO),
    foldr([1, 10, 100], 0, Ans),
    io.write_line(Ans, !IO).

:- pred foldr(list(int)::in, int::in, int::out) is det.

foldr([], Acc, Acc).
foldr(X, Acc0, Acc) :-
    X = [H | T],
    foldr(T, Acc0, Acc1),
    Acc = H - Acc1.
