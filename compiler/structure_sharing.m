%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_sharing.m.
% Main author: nancy.
%
% Package grouping all the modules related to the data structure sharing
% analysis needed for data structure reuse.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.
:- interface.

:- include_module analysis.
:- include_module domain.

:- import_module hlds.
    % Structure sharing information can be optimised using some
    % of the liveness information as used in liveness.m.
    % This explains the import of ll_backend here.
:- import_module ll_backend.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_sharing.
%-----------------------------------------------------------------------------%
