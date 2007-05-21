:- module incompatible_instance_constraints.
:- interface.

:- type t1(T) ---> t1(T).
:- type t2(T) ---> t2(T).
:- type t3(T, U) ---> t3(T, U).
:- type t4(T, U) ---> t4(T, U).

:- typeclass tc1(T) where [].
:- typeclass tc2(T) where [].
:- typeclass tc3(T, U) where [].

    % Missing constraints
:- instance tc1(t1(T)).

    % Constraints in a different order
:- instance tc1(t3(A, B)) <= (tc2(B), tc1(A)).
:- instance tc1(t4(T, U)) <= (tc1(U), tc1(T)).

    % Valid - same order just different type variables
:- instance tc2(t4(A, B)) <= (tc1(A), tc1(B)).

:- implementation.

    % Incompatible abstract definitions
:- instance tc1(t1(T)) <= tc1(T) where [].
:- instance tc1(t3(T, U)) <= (tc1(T), tc2(U)) where [].
:- instance tc1(t4(T, U)) <= (tc1(T), tc1(U)) where [].

    % Compatible abstract definitions
:- instance tc2(t4(T, U)) <= (tc1(T), tc1(U)) where [].

