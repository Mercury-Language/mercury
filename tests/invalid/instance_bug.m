%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module instance_bug.

:- interface.

:- typeclass foo(T) where [
    func id(T) = T
].
    % Compiler should complain about this.
:- instance foo(int) where [
    id(X) = X
].

:- implementation.

    % Compiler shouldn't complain about this.
:- instance foo(float) where [
    id(X) = X
].
