%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is part of the test_nested.m test case.

:- module parent.
:- interface.

:- include_module public_child.

:- implementation.

:- include_module private_child.
