% Regression test: the Mercury compiler of Mon Mar 2, 1998
% failed to report an error for this test case.

:- module missing_interface_import.
:- interface.

:- type bar == map(int, int).

:- pred p(std_util__univ::in) is det.
:- pred q(list(int)::in) is det.

:- implementation.

% These import_module and use_module declarations should be in the
% interface section.
:- import_module list, map.
:- use_module std_util.

p(_).
q(_).
