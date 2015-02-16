%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module invalid_typeclass.

:- interface.

:- import_module enum.

:- typeclass class(T).

:- typeclass class2(T) <= enum(T).

:- typeclass class3(T, T) where [
    func f(T) = T
].

:- implementation.

:- typeclass class2(T) where [
    func add1(T) = T
].
