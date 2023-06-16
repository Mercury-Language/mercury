%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test is designed to detect inconsistent instance declarations where
% one declaration is in a parent module and the other is in its child module.
% This is the child module. The error is detected when compiling this module,
% by comparing its instance declaration with the instance declaration
% in the parent module's .int0 file.
%

:- module instances_pc.instances_pc_helper_1.
:- interface.

:- instance foo(int, string).

:- implementation.

:- instance foo(int, string) where [].
