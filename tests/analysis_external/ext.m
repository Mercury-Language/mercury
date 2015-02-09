% Analysis results should be written out for exported `:- external' predicates.
% Importing modules don't care how those procedures are implemented, so results
% should exist.

:- module ext.
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
