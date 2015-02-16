%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tricky_assert2.

:- interface.

:- pred tricky_assert2__local is semidet.
:- promise tricky_assert2__local.

:- implementation.
:- external(tricky_assert2__local/0).
:- pragma foreign_code("Erlang", "local_0_p_0() -> void.").
