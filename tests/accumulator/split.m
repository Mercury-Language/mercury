%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that all the output variables must be related to
% a variable in the recursive call.
%

:- module split.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    p([1, 7, 4], S),
    io.write_string("p: ", !IO),
    io.write_line(S, !IO).

:- pred p(list(int)::in, int::out) is det.

p([], 0).
p([H | T], S) :-
    p(T, _),
    Tmp = 0,
    S = H + Tmp.
