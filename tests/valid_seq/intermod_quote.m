%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test to ensure that terms are properly quoted in the intermodule
% .opt file.
%

:- module intermod_quote.
:- interface.

:- type foo. % Not used.

:- implementation.
:- import_module intermod_quote2.

:- type foo
    --->    foo.
