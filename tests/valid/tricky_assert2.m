%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tricky_assert2.

:- interface.

:- pred tricky_assert2.local is semidet.
:- promise tricky_assert2.local.

:- implementation.
:- pragma external_pred(tricky_assert2.local/0).
