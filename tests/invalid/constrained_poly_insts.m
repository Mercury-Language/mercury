:- module constrained_poly_insts.
:- interface.

% Test that inconsistent declarations are not allowed.

:- pred p(T, T).
:- mode p(in(I), out(I =< any)) is det.

:- pred q(T::in(I =< free), T::out(I =< bound(c))) is det.

:- func r(T) = T.
:- mode r(in(I)) = out(I =< free) is det.

:- func s(T::in(I =< ground)) = (T::out(I =< unique)) is det.

% Test that mode errors are detected correctly.

:- pred t(int::in(I), int::out(I)) is det.

:- implementation.

p(X, X).
q(X, X).
r(X) = X.
s(X) = X.

t(_, 42).
