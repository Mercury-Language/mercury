%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test to ensure that terms are properly quoted in the .opt file.
%
% This test was originally called intermod_quote.
%

:- module opt_file_quote.
:- interface.

:- type foo. % Not used.

:- implementation.
:- import_module opt_file_quote_helper_1.

:- type foo
    --->    foo.
