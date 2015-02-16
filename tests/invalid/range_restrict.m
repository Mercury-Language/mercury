%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module range_restrict.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- implementation.
:- import_module list.
:- import_module map.

    % Error: range-restrictedness
:- instance foo(map(W, X), map(Y, Z)) where [].

    % This is also a range-restrictedness error, but since we don't
    % support duplicated type vars in instance declarations yet, we
    % can't test for it.
% :- instance foo(list(X), map(X, Y)) where [].
