%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module duplicate_modes.

:- pred p.
:- mode p is det.
p.

:- pred q(int, int).
:- mode q(ground >> ground, free >> ground) is det.
:- mode q(in, out) is det.
:- mode q(in, out) is det.
q(X, X).

:- pred r(int, int).
:- mode r(in, in) is det.
:- mode r(in, in) is semidet.

r(_, _).

% this one is legal (albeit not yet supported)
:- pred s(int, int).
:- mode s(in, out) is multi.
:- mode s(in, out) is cc_multi.

s(_, 42).
s(_, 43).

% this one is legal (albeit not yet supported)
:- pred t(int, int).
:- mode t(in(bound(1)), out) is multi.
:- mode t(in(bound(2)), out) is cc_multi.

t(_, 42).
t(_, 43).
