%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2009-04-03 and before failed to detect the overlapping instance
% between this parent module and its child module unless the option
% --intermodule-optimization was enabled, despite the fact that the
% information needed for doing that check is contained in the parent module's
% private interface (.int0) file.
%
% The .err_exp file is for C, the .err_exp2 file is for Java and C#.
%
%---------------------------------------------------------------------------%

:- module instances_pc.
:- interface.

:- typeclass foo(A, B) <= (A -> B) where [].

:- instance foo(int, float).

:- include_module instances_pc_helper_1.

:- implementation.

:- instance foo(int, float) where [].
