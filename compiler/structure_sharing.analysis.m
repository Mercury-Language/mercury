%-----------------------------------------------------------------------------%
% Copyright (C) 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_sharing.analysis.m
% Main authors: nancy
%
% Implementation of the data structure analysis. 
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.analysis.

:- interface.

:- import_module hlds__hlds_module.

:- import_module io.

:- pred data_structure_sharing_analysis(module_info::in, module_info::out, 
	io__state::di, io__state::uo) is det.

:- implementation. 

:- import_module io. 

data_structure_sharing_analysis(!HLDS, !IO). 

:- end_module transform_hlds.ctgc.structure_sharing.analysis.
