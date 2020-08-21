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

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_string("l: ", !IO),
    l([1, 10, 100], Length),
    io.write_line(Length, !IO).

:- pred l(list(T)::in, int::out) is det.

l([], Init) :-
    init(Init).
l([_ | T], L) :-
    l(T, L0),
    L = L0 + 1.

:- pred init(int::out) is det.
:- pragma no_inline(init/1).

init(0).
