%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tabled_io.

:- interface.

:- import_module io.

:- pred p(io::di, io::uo) is det.
:- pred q(io::di, io::uo) is det.

:- implementation.

:- import_module exception.

p(S0, _) :-
    nasty_fail_p(S0, S0).

q(S0, _) :-
    nasty_fail_q(S0, S0).

:- pred nasty_fail_p(io::in, io::in) is erroneous.
:- pred nasty_fail_q(io::in(any), io::in(any)) is erroneous.

nasty_fail_p(_, _) :-
    throw("ouch").

nasty_fail_q(_, _) :-
    throw("ouch").
