%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tricky_assert.

:- interface.

:- pred tricky_assert.local is semidet.
:- promise tricky_assert.local.

:- implementation.
:- pragma external_pred(tricky_assert.local/0).
