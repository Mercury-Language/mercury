%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that if there is a call in the base case that we still
% are able to introduce an accumulator.
%

:- module call_in_base.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io__write_string("l: ", !IO),
    l([1, 10, 100], Length),
    io__write(Length, !IO),
    io__nl(!IO).

:- pred l(list(T)::in, int::out) is det.

l([], Init) :-
    init(Init).
l([_ | T], L) :-
    l(T, L0),
    L is L0 + 1.

:- pred init(int::out) is det.
:- pragma no_inline(init/1).

init(0).
