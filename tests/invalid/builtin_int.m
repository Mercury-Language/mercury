%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that compiler/module_qual.m does not add a spurious
% "(the module `builtin' has not been imported)" to the end of the
% undefined type error message.

:- module builtin_int.

:- interface.

:- pred foo(builtin.ints::in) is semidet.

:- implementation.

foo(1).
