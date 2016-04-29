%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Same as ext.m but tests trail usage analysis separately.

:- module ext2.
:- interface.

:- pred foo(t::in, t::out) is det.

:- pred bar(t::in, t::out) is det.

:- type t
    --->    t(int, int).

%---------------------------------------------------------------------------%

:- implementation.

:- pragma external_pred(foo/2).

% For comparison.
:- pragma no_inline(bar/2).
bar(X, X).
