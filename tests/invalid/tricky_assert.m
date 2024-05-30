%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is invalid because the interface refers to stuff defined
% only in the implementation.

:- module tricky_assert1.
:- interface.
:- promise tricky_assert1.local.

:- implementation.
:- pred tricky_assert1.local is semidet.
:- pragma external_pred(tricky_assert1.local/0).
