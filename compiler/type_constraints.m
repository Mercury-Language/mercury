%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: type_constraints.m
%
% Typecheck all the predicates in the program using constraints.
%
%---------------------------------------------------------------------------%

:- module check_hlds.type_constraints.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

    % Typecheck the module using constraints.
    %
:- pred typecheck_constraints(module_info::in, module_info::out,
    list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

typecheck_constraints(!HLDS, Specs) :-
    Specs = [].

%---------------------------------------------------------------------------%
:- end_module check_hlds.type_constraints.
%---------------------------------------------------------------------------%
