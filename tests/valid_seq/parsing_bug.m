%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test was originally called parsing_bug_main.
%

:- module parsing_bug.
:- interface.
:- import_module parsing_bug_helper_1.

:- type blah == int.    % Just to avoid warning about exporting nothing.
