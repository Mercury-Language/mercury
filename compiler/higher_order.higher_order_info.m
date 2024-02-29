%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module transform_hlds.higher_order.higher_order_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.higher_order.higher_order_global_info.

%---------------------------------------------------------------------------%

    % Returned by ho_traverse_proc_body.
    %
:- type ho_changed
    --->    hoc_changed     % Need to requantify goal + check other procs.
    ;       hoc_request     % Need to check other procs.
    ;       hoc_unchanged.  % Do nothing more for this predicate.

    % Used while traversing goals.
    %
:- type higher_order_info.

:- func hoi_init(higher_order_global_info, pred_id, proc_id)
    = higher_order_info.

:- pred hoi_results(higher_order_info::in, higher_order_global_info::out,
    pred_info::out, proc_info::out) is det.

:- func hoi_get_global_info(higher_order_info) = higher_order_global_info.
:- func hoi_get_pred_proc_id(higher_order_info) = pred_proc_id.
:- func hoi_get_pred_info(higher_order_info) = pred_info.
:- func hoi_get_proc_info(higher_order_info) = proc_info.
:- func hoi_get_known_var_map(higher_order_info) = known_var_map.
:- func hoi_get_changed(higher_order_info) = ho_changed.

:- pred hoi_set_global_info(higher_order_global_info::in,
    higher_order_info::in, higher_order_info::out) is det.
:- pred hoi_set_pred_info(pred_info::in,
    higher_order_info::in, higher_order_info::out) is det.
:- pred hoi_set_proc_info(proc_info::in,
    higher_order_info::in, higher_order_info::out) is det.
:- pred hoi_set_known_var_map(known_var_map::in,
    higher_order_info::in, higher_order_info::out) is det.
:- pred hoi_set_changed(ho_changed::in,
    higher_order_info::in, higher_order_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_module.

:- import_module map.

%---------------------------------------------------------------------------%

    % Used while traversing goals.
    %
:- type higher_order_info
    --->    higher_order_info(
                % The hoi_pred_proc_id field is read-only. All the others
                % are read-write.

                hoi_global_info         :: higher_order_global_info,

                % The pred_proc_id, pred_info and proc_info of the procedure
                % whose body is being traversed.
                hoi_pred_proc_id        :: pred_proc_id,
                hoi_pred_info           :: pred_info,
                hoi_proc_info           :: proc_info,

                % Higher order variables with unique known values.
                hoi_known_var_map       :: known_var_map,

                hoi_changed             :: ho_changed
            ).

hoi_init(GlobalInfo0, PredId, ProcId) = Info :-
    ModuleInfo0 = hogi_get_module_info(GlobalInfo0),
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId,
        PredInfo0, ProcInfo0),
    PredProcId = proc(PredId, ProcId),
    map.init(KnownVarMap0),
    Info = higher_order_info(GlobalInfo0, PredProcId, PredInfo0, ProcInfo0,
        KnownVarMap0, hoc_unchanged).

hoi_results(Info, GlobalInfo, PredInfo, ProcInfo) :-
    Info = higher_order_info(GlobalInfo, _, PredInfo, ProcInfo, _, _).

%---------------------------------------------------------------------------%

hoi_get_global_info(Info) = X :-
    X = Info ^ hoi_global_info.
hoi_get_pred_proc_id(Info) = X :-
    X = Info ^ hoi_pred_proc_id.
hoi_get_pred_info(Info) = X :-
    X = Info ^ hoi_pred_info.
hoi_get_proc_info(Info) = X :-
    X = Info ^ hoi_proc_info.
hoi_get_known_var_map(Info) = X :-
    X = Info ^ hoi_known_var_map.
hoi_get_changed(Info) = X :-
    X = Info ^ hoi_changed.

hoi_set_global_info(X, !Info) :-
    !Info ^ hoi_global_info := X.
hoi_set_pred_info(X, !Info) :-
    !Info ^ hoi_pred_info := X.
hoi_set_proc_info(X, !Info) :-
    !Info ^ hoi_proc_info := X.
hoi_set_known_var_map(X, !Info) :-
    !Info ^ hoi_known_var_map := X.
hoi_set_changed(X, !Info) :-
    !Info ^ hoi_changed := X.

%---------------------------------------------------------------------------%
:- end_module transform_hlds.higher_order.higher_order_info.
%---------------------------------------------------------------------------%
