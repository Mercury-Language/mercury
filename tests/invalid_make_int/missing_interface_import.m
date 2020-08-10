%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test: the Mercury compiler of Mon Mar 2, 1998 failed to report
% an error for this test case. The .int_err_exp2 file is for --use-subdirs.

:- module missing_interface_import.
:- interface.

:- type bar == map(int, int).

:- pred p(univ__univ::in) is det.
:- pred q(list(int)::in) is det.

:- implementation.

% These import_module and use_module declarations should be in the
% interface section.
:- import_module list.
:- import_module map.
:- use_module univ.

p(_).
q(_).
