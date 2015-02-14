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

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io__write_string("l: ", !IO),
    p([1, 10, 100], 5, Length),
    io__write(Length, !IO),
    io__nl(!IO).

:- pred p(list(int)::in, int::in, int::out) is det.

p([], L, L).
p([H | T], _, L) :-
    p(T, H, L0),
    L is L0 + 1.
