%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: region_builtin.m.
% Main authors: qph, juliensf.
% Stability: low.
%
% This file is automatically imported into every module when region-based
% memory management is enabled.  It contains support predicates used for
% RBMM.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module region_builtin.
:- interface.

%-----------------------------------------------------------------------------%

    % A pointer to a memory region.
    %
:- type region.

    % Create a new region.
    %
:- impure pred create_region(region::out) is det.

    % Remove a region.
    %
:- impure pred remove_region(region::in) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% XXX the following definitions are just placeholders.  When runtime
% support for RBMM is introduced they will be changed.

:- type region == c_pointer.

:- pragma foreign_proc("C",
    create_region(Region::out),
    [will_not_call_mercury],
"
    /* Region */
    MR_fatal_error(\"region_builtin.create_region/1 NYI.\");
").

:- pragma foreign_proc("C",
    remove_region(Region::in),
    [will_not_call_mercury],
"
    /* Region */
    MR_fatal_error(\"region_builtin.remove_region/1 NYI.\");
").

%-----------------------------------------------------------------------------%
:- end_module region_builtin.
%-----------------------------------------------------------------------------%
