%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.default_func_mode.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Before copying the clauses to the procs, we need to add
    % a default mode of `:- mode foo(in, in, ..., in) = out is det.'
    % for functions that don't have an explicit mode declaration.
    %
:- pred maybe_add_default_func_modes(list(pred_id)::in,
    pred_table::in, pred_table::out) is det.

:- pred maybe_add_default_func_mode(pred_info::in, pred_info::out,
    maybe(proc_id)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.add_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.

:- import_module int.
:- import_module map.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

maybe_add_default_func_modes([], !PredTable).
maybe_add_default_func_modes([PredId | PredIds], !PredTable) :-
    map.lookup(!.PredTable, PredId, PredInfo0),
    maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeNewProcId),
    (
        MaybeNewProcId = no
    ;
        MaybeNewProcId = yes(_),
        map.det_update(PredId, PredInfo, !PredTable)
    ),

    maybe_add_default_func_modes(PredIds, !PredTable).

maybe_add_default_func_mode(PredInfo0, PredInfo, MaybeProcId) :-
    pred_info_get_proc_table(PredInfo0, Procs0),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    ( if
        % Is this a function with no modes?
        PredOrFunc = pf_function,
        map.is_empty(Procs0)
    then
        % If so, add a default mode of
        %
        %   :- mode foo(in, in, ..., in) = out is det.
        %
        % for this function. (N.B. functions which can fail must be
        % explicitly declared as semidet.)

        SeqNum = item_no_seq_num,
        PredArity = pred_info_orig_arity(PredInfo0),
        FuncArity = PredArity - 1,
        in_mode(InMode),
        out_mode(OutMode),
        list.duplicate(FuncArity, InMode, FuncArgModes),
        FuncRetMode = OutMode,
        list.append(FuncArgModes, [FuncRetMode], PredArgModes),
        Determinism = detism_det,
        pred_info_get_context(PredInfo0, Context),
        MaybePredArgLives = no,
        % No inst_vars in default func mode.
        varset.init(InstVarSet),
        % Before the simplification pass, HasParallelConj is not meaningful.
        HasParallelConj = has_no_parallel_conj,
        add_new_proc(Context, SeqNum, PredArity,
            InstVarSet, PredArgModes, yes(PredArgModes),
            MaybePredArgLives, detism_decl_implicit, yes(Determinism),
            address_is_not_taken, HasParallelConj,
            PredInfo0, PredInfo, ProcId),
        MaybeProcId = yes(ProcId)
    else
        PredInfo = PredInfo0,
        MaybeProcId = no
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.default_func_mode.
%-----------------------------------------------------------------------------%
