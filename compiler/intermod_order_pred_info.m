%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module contains a type, order_pred_info, and a way to create values
% of that type. It is in a module of its own because this functionality
% is needed by both intermod.m and intermod_analysis.m.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.intermod_order_pred_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Information that intermod.m and intermod_analysis.m may need
    % about a predicate, in a form that, when sorted, generates a standard
    % lexicographic order on the names and arities of the predicates involved.
    %
:- type order_pred_info
    --->    order_pred_info(
                opi_name            :: string,
                opi_user_arity      :: user_arity,
                opi_pred_or_fun     :: pred_or_func,
                opi_pred_id         :: pred_id,
                opi_pred_info       :: pred_info
            ).

    % Construct an order_pred_info for each predicate whose pred_id is
    % given to us, and return those order_pred_infos in lexicographic order.
    %
:- pred generate_order_pred_infos(module_info::in, list(pred_id)::in,
    list(order_pred_info)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

%---------------------------------------------------------------------------%

generate_order_pred_infos(ModuleInfo, PredIds, SortedOrderPredInfos) :-
    generate_order_pred_infos_acc(ModuleInfo, PredIds, [], OrderPredInfos),
    list.sort(OrderPredInfos, SortedOrderPredInfos).

:- pred generate_order_pred_infos_acc(module_info::in, list(pred_id)::in,
    list(order_pred_info)::in, list(order_pred_info)::out) is det.

generate_order_pred_infos_acc(_, [], !OrderPredInfos).
generate_order_pred_infos_acc(ModuleInfo, [PredId | PredIds],
        !OrderPredInfos) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_orig_arity(PredInfo),
    user_arity_pred_form_arity(PredOrFunc, UserArity,
        pred_form_arity(PredFormArity)),
    OrderPredInfo = order_pred_info(PredName, UserArity, PredOrFunc,
        PredId, PredInfo),
    !:OrderPredInfos = [OrderPredInfo | !.OrderPredInfos],
    generate_order_pred_infos_acc(ModuleInfo, PredIds, !OrderPredInfos).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.intermod_order_pred_info.
%---------------------------------------------------------------------------%
