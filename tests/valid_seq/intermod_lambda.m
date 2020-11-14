%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for resolving overloading of higher-order terms
% exported using inter-module optimization.
% This is also a regression test to check that local modes are put
% in the .opt files.
%

:- module intermod_lambda.

:- interface.

:- type foo.    % not used

:- implementation.

:- type foo
    --->    foo.

:- import_module intermod_lambda2.
