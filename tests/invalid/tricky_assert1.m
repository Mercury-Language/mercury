%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is invalid because the interface refers to stuff defined
% only in the implementation.

:- module tricky_assert1.
:- interface.
:- promise tricky_assert1__local.

:- implementation.
:- pred tricky_assert1__local is semidet.
:- external(tricky_assert1__local/0).
