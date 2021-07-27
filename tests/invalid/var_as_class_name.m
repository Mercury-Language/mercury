%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The term parser turns "X(a, b)" into "`'(X, a, b)". This would leads us to
% generate confusing error messages for invalid declarations such as those
% below if we didn't detect them, and handle them specially.
%

:- module var_as_class_name.
:- interface.

:- typeclass foo(T) where [].

:- typeclass A(T).
:- typeclass B(T) where [].
:- typeclass C(T) <= foo(T).
:- typeclass D(T) <= foo(T) where [].

:- instance A(int).
:- instance B(bar(T)) <= foo(T).
