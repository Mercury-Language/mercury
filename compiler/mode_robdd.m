%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: mode_robdd.m.
% Main author: dmo
% 
% This module contains the package for mode_robbds, data structures built
% around reduced ordered binary decision diagrams (ROBDDs) to help implement
% mode checking via constraints.
%
%---------------------------------------------------------------------------%

:- module mode_robdd.
:- interface.

% These modules contain the alternative implementations of mode_robdds.
% :- include_module r.
% :- include_module tfr.
% :- include_module tfer.
% :- include_module tfeir.
:- include_module tfeirn.

% This module allows different alternatives implementations to be compared.
% :- include_module check.

:- include_module equiv_vars.
:- include_module implications.

% The default implementation of mode_robdd's stores as much information
% out of the robdd itself as possible, for performance.
:- import_module mode_robdd.tfeirn.
:- type mode_robdd(T) == tfeirn(T).

% You may wish to use this type instead, for comparing to mode_robdd
% implementations, which can help in debugging.
% :- import_module mode_robdd__check.
% :- type mode_robdd(T) == check_robdd(T).

%---------------------------------------------------------------------------%
:- end_module mode_robdd.
%---------------------------------------------------------------------------%
