%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test that accumulators are introduced into functions.
%

:- module (func).

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_string("sumlist: ", !IO),
    Sum = sumlist([5, 6, 7]),
    io.write_line(Sum, !IO).

:- func sumlist(list(int)) = int.

sumlist([]) = 0.
sumlist([H | T]) = H + sumlist(T).
