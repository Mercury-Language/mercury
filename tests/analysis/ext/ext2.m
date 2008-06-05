% Same as ext.m but tests trail usage analysis separately.

:- module ext2.
:- interface.

:- pred foo(t::in, t::out) is det.

:- pred bar(t::in, t::out) is det.

:- type t
    --->    t(int, int).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- external(foo/2).

% For comparison.
:- pragma no_inline(bar/2).
bar(X, X).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sts=4 sw=4 et
