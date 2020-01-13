% vim: ts=4 sw=4 et ft=mercury
%
% The fact that the module name in this source file does not match
% the module name expected from its file name caused mmc to crash
% with a map lookup failure when invoked with --generate-dependencies
% on bad_module_name.m.

:- module wrong_module_name.
:- interface.

:- type u
    --->    g1
    ;       g2.
