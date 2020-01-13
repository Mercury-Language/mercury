%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This file is deliberately named to potentially be confused for
% a standard library module.

:- module bug489.lexer.
:- interface.

:- type token
    --->    tok1
    ;       tok2.

:- implementation.

:- import_module bug489.other.
