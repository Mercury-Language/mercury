%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is invalid because the interface refers to stuff defined
% only in the implementation.

:- module tricky_assert.
:- interface.
:- promise tricky_assert.local.

:- implementation.
:- pred tricky_assert.local is semidet.
:- pragma external_pred(tricky_assert.local/0).
