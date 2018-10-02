%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: proc_requests.m.
%
% This module encapsulates access to the proc_requests table, and constructs
% the clauses for out-of-line complicated unification procedures.
% It also generates the code for other compiler-generated type-specific
% predicates such as compare/3.
%
% During mode analysis, we notice each different complicated unification
% that occurs. For each one we add a new mode to the out-of-line
% unification predicate for that type, and we record in the `proc_requests'
% table that we need to eventually modecheck that mode of the unification
% procedure.
%
% After we have done mode analysis for all the ordinary predicates, we then
% do mode analysis for the out-of-line unification procedures. Note that
% unification procedures may call other unification procedures which have
% not yet been encountered, causing new entries to be added to the
% proc_requests table. We store the entries in a queue and continue the
% process until the queue is empty.
%
% The same queuing mechanism is also used for procedures created by
% mode inference during mode analysis and unique mode analysis.
%
% Currently if the same complicated unification procedure is called by
% different modules, each module will end up with a copy of the code for
% that procedure. In the long run it would be desireable to either delay
% generation of complicated unification procedures until link time (like
% Cfront does with C++ templates) or to have a smart linker which could
% merge duplicate definitions (like Borland C++). However the amount of
% code duplication involved is probably very small, so it is definitely not
% worth worrying about right now.
%
% XXX What about complicated unification of an abstract type in a partially
% instantiated mode? Currently we don't implement it correctly. Probably
% it should be disallowed, but we should issue a proper error message.
%
%---------------------------------------------------------------------------%

:- module check_hlds.proc_requests.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.
:- import_module queue.

%---------------------------------------------------------------------------%

:- type proc_requests.
:- type req_queue == queue(pred_proc_id).

:- pred get_req_queue(proc_requests::in, req_queue::out) is det.
:- pred set_req_queue(req_queue::in,
    proc_requests::in, proc_requests::out) is det.

%---------------------------------------------------------------------------%

:- type unify_proc_id
    --->    unify_proc_id(type_ctor, unify_mode).

    % Initialize the proc_requests table.
    %
:- pred init_requests(proc_requests::out) is det.

    % Add a new request for a unification procedure to the proc_requests table.
    %
:- pred request_unify(unify_proc_id::in, inst_varset::in, determinism::in,
    prog_context::in, module_info::in, module_info::out) is det.

    % Add a new request for a procedure (not necessarily a unification)
    % to the request queue. Return the procedure's newly allocated proc_id.
    % (This is used by mode inference and unique_modes.m.)
    %
:- pred request_proc(pred_id::in, list(mer_mode)::in, inst_varset::in,
    maybe(list(is_live))::in, maybe(determinism)::in, prog_context::in,
    proc_id::out, module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

    % Given the type and mode of a unification, look up the mode number
    % for the unification proc.
    %
:- pred lookup_mode_num(module_info::in, type_ctor::in, unify_mode::in,
    determinism::in, proc_id::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.clause_to_proc.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.add_pred.
:- import_module hlds.add_special_pred.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.special_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module recompilation.

:- import_module map.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % We keep track of all the complicated unification procs we need
    % by storing them in the proc_requests structure.
    % For each unify_proc_id (i.e. type & mode), we store the proc_id
    % (mode number) of the unification procedure which corresponds to
    % that mode.
    %
:- type unify_req_map == map(unify_proc_id, proc_id).

:- type proc_requests
    --->    proc_requests(
                unify_req_map   :: unify_req_map,
                % The assignment of proc_id numbers to unify_proc_ids.

                req_queue       :: req_queue
                % The queue of procs we still to generate code for.
            ).

:- pred get_unify_req_map(proc_requests::in, unify_req_map::out) is det.
:- pred set_unify_req_map(unify_req_map::in,
    proc_requests::in, proc_requests::out) is det.

get_unify_req_map(PR, X) :-
    X = PR ^ unify_req_map.
get_req_queue(PR, X) :-
    X = PR ^ req_queue.

set_unify_req_map(X, !PR) :-
    !PR ^ unify_req_map := X.
set_req_queue(X, !PR) :-
    !PR ^ req_queue := X.

%---------------------------------------------------------------------------%

init_requests(Requests) :-
    map.init(UnifyReqMap),
    queue.init(ReqQueue),
    Requests = proc_requests(UnifyReqMap, ReqQueue).

%---------------------------------------------------------------------------%

request_unify(UnifyId, InstVarSet, Determinism, Context, !ModuleInfo) :-
    UnifyId = unify_proc_id(TypeCtor, UnifyMode),

    % Generating a unification procedure for a type uses its body.
    module_info_get_maybe_recompilation_info(!.ModuleInfo, MaybeRecompInfo0),
    (
        MaybeRecompInfo0 = yes(RecompInfo0),
        TypeCtorItem = type_ctor_to_item_name(TypeCtor),
        recompilation.record_used_item(type_body_item,
            TypeCtorItem, TypeCtorItem, RecompInfo0, RecompInfo),
        module_info_set_maybe_recompilation_info(yes(RecompInfo), !ModuleInfo)
    ;
        MaybeRecompInfo0 = no
    ),

    % Check if this unification has already been requested, or
    % if the proc is hand defined.
    ( if
        (
            search_mode_num(!.ModuleInfo, TypeCtor, UnifyMode, Determinism, _)
        ;
            module_info_get_type_table(!.ModuleInfo, TypeTable),
            search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            (
                TypeCtor = type_ctor(TypeName, _TypeArity),
                TypeName = qualified(TypeModuleName, _),
                module_info_get_name(!.ModuleInfo, ModuleName),
                ModuleName = TypeModuleName,
                TypeBody = hlds_abstract_type(_)
            ;
                type_ctor_has_hand_defined_rtti(TypeCtor, TypeBody)
            )
        )
    then
        true
    else
        % Lookup the pred_id for the unification procedure that we are
        % going to generate.
        module_info_get_special_pred_maps(!.ModuleInfo, SpecialPredMaps),
        UnifyMap = SpecialPredMaps ^ spm_unify_map,
        ( if map.search(UnifyMap, TypeCtor, PredIdPrime) then
            PredId = PredIdPrime
        else
            % We generate unification predicates for most imported types
            % lazily, so add the declarations and clauses now.
            add_lazily_generated_unify_pred(TypeCtor, PredId, !ModuleInfo)
        ),

        % Convert from `uni_mode' to `list(mer_mode)'.
        UnifyMode = unify_modes_lhs_rhs(LHSInsts, RHSInsts),
        LHSInsts = from_to_insts(LHSInit, LHSFinal),
        RHSInsts = from_to_insts(RHSInit, RHSFinal),
        LHSMode = from_to_mode(LHSInit, LHSFinal),
        RHSMode = from_to_mode(RHSInit, RHSFinal),
        ArgModes0 = [LHSMode, RHSMode],

        % For polymorphic types, add extra modes for the type_infos.
        in_mode(InMode),
        TypeCtor = type_ctor(_, TypeArity),
        list.duplicate(TypeArity, InMode, TypeInfoModes),
        ArgModes = TypeInfoModes ++ ArgModes0,

        ArgLives = no,  % XXX ArgLives should be part of the UnifyId

        request_proc(PredId, ArgModes, InstVarSet, ArgLives, yes(Determinism),
            Context, ProcId, !ModuleInfo),

        % Save the proc_id for this unify_proc_id.
        module_info_get_proc_requests(!.ModuleInfo, Requests0),
        get_unify_req_map(Requests0, UnifyReqMap0),
        map.set(UnifyId, ProcId, UnifyReqMap0, UnifyReqMap),
        set_unify_req_map(UnifyReqMap, Requests0, Requests),
        module_info_set_proc_requests(Requests, !ModuleInfo)
    ).

request_proc(PredId, ArgModes, InstVarSet, ArgLives, MaybeDet, Context, ProcId,
        !ModuleInfo) :-
    some [!PredInfo, !ProcInfo, !PredMap, !ProcMap, !Goal] (
        % Create a new proc_info for this procedure.
        module_info_get_preds(!.ModuleInfo, !:PredMap),
        map.lookup(!.PredMap, PredId, !:PredInfo),
        ItemNumber = -1,
        list.length(ArgModes, Arity),
        DeclaredArgModes = no,
        % Before the simplification pass, HasParallelConj is not meaningful.
        HasParallelConj = has_no_parallel_conj,
        add_new_proc(Context, ItemNumber, Arity,
            InstVarSet, ArgModes, DeclaredArgModes, ArgLives,
            detism_decl_implicit, MaybeDet, address_is_not_taken,
            HasParallelConj, !PredInfo, ProcId),

        % Copy the clauses for the procedure from the pred_info
        % to the proc_info, and mark the procedure as one that
        % cannot be processed yet. (The mark will be changed to
        % `can_process_now' by modecheck_queued_proc.)
        pred_info_get_proc_table(!.PredInfo, !:ProcMap),
        map.lookup(!.ProcMap, ProcId, !:ProcInfo),
        proc_info_set_can_process(cannot_process_yet, !ProcInfo),

        copy_clauses_to_proc_in_proc_info(!.ModuleInfo, !.PredInfo, ProcId,
            !ProcInfo),

        proc_info_get_goal(!.ProcInfo, !:Goal),
        set_goal_contexts(Context, !Goal),

        % The X == Y pretest on unifications makes sense only for in-in
        % unifications, and if the initial insts are incompatible, then
        % casts in the pretest would prevent mode analysis from discovering
        % this fact.
        pred_info_get_origin(!.PredInfo, Origin),
        ( if
            Origin = origin_special_pred(spec_pred_unify, _TypeCtor),
            all [ArgMode] (
                list.member(ArgMode, ArgModes)
            =>
                mode_is_fully_input(!.ModuleInfo, ArgMode)
            ),
            not MaybeDet = yes(detism_failure)
        then
            true
        else
            !:Goal = maybe_strip_equality_pretest(!.Goal)
        ),
        proc_info_set_goal(!.Goal, !ProcInfo),

        map.det_update(ProcId, !.ProcInfo, !ProcMap),
        pred_info_set_proc_table(!.ProcMap, !PredInfo),
        map.det_update(PredId, !.PredInfo, !PredMap),
        module_info_set_preds(!.PredMap, !ModuleInfo),

        % Insert the pred_proc_id into the request queue.
        module_info_get_proc_requests(!.ModuleInfo, Requests0),
        get_req_queue(Requests0, ReqQueue0),
        queue.put(proc(PredId, ProcId), ReqQueue0, ReqQueue),
        set_req_queue(ReqQueue, Requests0, Requests),
        module_info_set_proc_requests(Requests, !ModuleInfo)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

lookup_mode_num(ModuleInfo, TypeCtor, UniMode, Det, Num) :-
    ( if search_mode_num(ModuleInfo, TypeCtor, UniMode, Det, NumPrime) then
        Num = NumPrime
    else
        unexpected($pred, "search_num failed")
    ).

    % Given the type, mode, and determinism of a unification, look up the
    % mode number for the unification proc.
    % We handle semidet unifications with mode (in, in) specially - they
    % are always mode zero. Similarly for unifications of `any' insts.
    % (It should be safe to use the `in, in' mode for any insts, since
    % we assume that `ground' and `any' have the same representation.)
    % For unreachable unifications, we also use mode zero.
    %
:- pred search_mode_num(module_info::in, type_ctor::in, unify_mode::in,
    determinism::in, proc_id::out) is semidet.

search_mode_num(ModuleInfo, TypeCtor, UnifyMode, Determinism, ProcId) :-
    UnifyMode = unify_modes_lhs_rhs(
        from_to_insts(InitInstX, _FinalInstX),
        from_to_insts(InitInstY, _FinalInstY)),
    ( if
        Determinism = detism_semi,
        inst_is_ground_or_any(ModuleInfo, InitInstX),
        inst_is_ground_or_any(ModuleInfo, InitInstY)
    then
        hlds_pred.in_in_unification_proc_id(ProcId)
    else if
        InitInstX = not_reached
    then
        hlds_pred.in_in_unification_proc_id(ProcId)
    else if
        InitInstY = not_reached
    then
        hlds_pred.in_in_unification_proc_id(ProcId)
    else
        module_info_get_proc_requests(ModuleInfo, Requests),
        get_unify_req_map(Requests, UnifyReqMap),
        map.search(UnifyReqMap, unify_proc_id(TypeCtor, UnifyMode), ProcId)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.proc_requests.
%---------------------------------------------------------------------------%
