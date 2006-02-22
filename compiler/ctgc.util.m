%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.util.m
% Main authors: nancy
%
% Utility operations for the CTGC-system.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.util.

:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

	% Check if some of the predicates are "special" predicates (as in
	% "special_pred_map" known from module_info) or not defined in the
	% current module, as these predicates are not analysed by the CTGC
	% system.
	%
:- pred preds_requiring_no_analysis(module_info::in, 
	list(pred_proc_id)::in) is semidet.

:- implementation.

:- import_module bool.
:- import_module map.

preds_requiring_no_analysis(ModuleInfo, PredProcIds) :- 
	module_info_get_special_pred_map(ModuleInfo, SpecialPredMap), 
	map.values(SpecialPredMap, SpecialPreds), 
	(

		list.filter(pred_id_in(SpecialPreds), PredProcIds,
				SpecialPredProcs),
		SpecialPredProcs = [_|_]

	; 
		% or some of the predicates are not defined in this
		% module. 
		list.filter(not_defined_in_this_module(ModuleInfo), 	
			PredProcIds, FilteredPredProcIds), 
		FilteredPredProcIds = [_|_]
	).

:- pred pred_id_in(list(pred_id)::in, pred_proc_id::in) is semidet.
pred_id_in(PredIds, PredProcId):-
	PredProcId = proc(PredId, _),
	list.member(PredId, PredIds). 

:- pred not_defined_in_this_module(module_info::in, 
	pred_proc_id::in) is semidet.
not_defined_in_this_module(ModuleInfo, proc(PredId, _)):-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_import_status(PredInfo, Status),
	status_defined_in_this_module(Status, no).

:- end_module transform_hlds.ctgc.util.
