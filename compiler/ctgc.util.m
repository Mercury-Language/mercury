%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ctgc.util.m.
% Main author: nancy.

% Utility operations for the CTGC-system.

%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.util.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module list.

%-----------------------------------------------------------------------------%

    % Check if some of the predicates are "special" predicates (as in
    % "special_pred_map" known from module_info) or not defined in the
    % current module, as these predicates are not analysed by the CTGC
    % system.
    %
:- pred preds_requiring_no_analysis(module_info::in, list(pred_proc_id)::in)
    is semidet.

:- pred pred_requires_no_analysis(module_info::in, pred_id::in) is semidet.
:- pred pred_requires_analysis(module_info::in, pred_id::in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module map.


%-----------------------------------------------------------------------------%

pred_requires_no_analysis(ModuleInfo, PredId) :- 
    module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
    map.values(SpecialPredMap, SpecialPreds),
    (
        list.member(PredId, SpecialPreds)
    ;   
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_import_status(PredInfo, Status),
        status_defined_in_this_module(Status, no)
    ).

pred_requires_analysis(ModuleInfo, PredId) :- 
    \+ pred_requires_no_analysis(ModuleInfo, PredId).

:- func get_pred_id(pred_proc_id) = pred_id. 
get_pred_id(proc(PredId, _)) = PredId. 

preds_requiring_no_analysis(ModuleInfo, PPIds) :-
    list.takewhile(pred_requires_analysis(ModuleInfo),
        list.map(get_pred_id, PPIds), _RequiresAnalysis, RequiresNoAnalysis),
    RequiresNoAnalysis = [_|_].

:- pred not_defined_in_this_module(module_info::in, pred_proc_id::in)
    is semidet.

not_defined_in_this_module(ModuleInfo, proc(PredId, _)):-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, Status),
    status_defined_in_this_module(Status, no).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.util.
%-----------------------------------------------------------------------------%
