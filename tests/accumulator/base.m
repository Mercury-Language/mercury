%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that we handle the case of a dynamic value for the base
% case coming from the previous call.
%

:- module base.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_string("l: ", !IO),
    p([1, 10, 100], 5, Length),
    io.write_line(Length, !IO).

:- pred p(list(int)::in, int::in, int::out) is det.

p([], L, L).
p([H | T], _, L) :-
    p(T, H, L0),
    L = L0 + 1.
