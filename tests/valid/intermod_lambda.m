% Regression test for resolving overloading of higher-order terms
% exported using inter-module optimization.
:- module intermod_lambda.

:- interface.

:- type foo.	% not used

:- import_module intermod_lambda2.

:- interface.
