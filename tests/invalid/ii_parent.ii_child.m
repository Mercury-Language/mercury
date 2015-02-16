%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ii_parent.ii_child.
:- interface.

:- instance foo(int, string).

:- implementation.

:- instance foo(int, string) where [].
