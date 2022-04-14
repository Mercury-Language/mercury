%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mpj5.
:- interface.

:- import_module bool.
:- import_module list.

:- typeclass coll(E, C) <= (C -> E) where [
    func e = C,
    func i(E, C) = C,
    pred m(E::in, C::in) is semidet
].

:- type w(T)
    --->    w(T).
:- type cf(T)
    --->    cf(func(T) = bool).

:- instance coll(int, int).

% We don't support duplicated type vars in instances yet, so these cases
% are disabled for the moment.
% :- instance coll(w(T), list(T)).
% :- instance coll(w(T), cf(T)).

:- implementation.
:- import_module int.

:- instance coll(int, int) where [
    (e = 0),
    (i(N, B) = B \/ (1 << N)),
    (m(N, B) :- B /\ (1 << N) \= 0)
].

% :- instance coll(w(T), list(T)) where [
%   (e = []),
%   (i(w(E), L) = [E | L]),
%   (m(w(E), L) :- list.member(E, L))
% ].

% :- instance coll(w(T), cf(T)) where [
%   (e = cf(func(_) = no)),
%   (i(w(E), cf(F)) = cf(func(X) = ( X = E -> yes ; F(X) ))),
%   (m(w(E), cf(F)) :- F(E) = yes)
% ].
