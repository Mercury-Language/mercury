%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constrained_poly_insts.
:- interface.

% Test that inconsistent declarations are not allowed.

:- pred p(T, T).
:- mode p(in(I), out(I =< any)) is det.

:- pred q(T::in(I =< free), T::out(I =< bound(c))) is det.

:- func r(T) = T.
:- mode r(in(I)) = out(I =< free) is det.

:- func s(T::in(I =< ground)) = (T::out(I =< unique)) is det.

:- pred t(I::in(I =< ground), T::out(I =< unique),
    U::in(J =< ground), U::out(J =< any)) is det.

% Test that mode errors are detected correctly.
:- pred u(int::in(I), int::out(I)) is det.

:- implementation.

p(X, X).
q(X, X).
r(X) = X.
s(X) = X.
t(X, X, Y, Y).
u(_, 42).
