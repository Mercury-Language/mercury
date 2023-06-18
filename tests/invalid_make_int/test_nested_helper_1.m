%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is part of the test_nested.m test case.

:- module test_nested_helper_1.
:- interface.

:- include_module test_nested_helper_3.

:- implementation.

:- include_module test_nested_helper_4.
