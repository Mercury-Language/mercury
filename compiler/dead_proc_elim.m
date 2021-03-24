%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: dead_proc_elim.m.
% Main author: zs.
%
% The job of this module is to delete dead predicates, procedures and
% type_ctor_gen_info structures from the HLDS.
%
% It also computes the usage counts that inlining.m uses for the
% `--inline-single-use' option.
%
% It also issues warnings about unused procedures.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.dead_proc_elim.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

:- type needed_map == map(entity, maybe_needed).

:- type entity
    --->    entity_proc(pred_id, proc_id)
    ;       entity_table_struct(pred_id, proc_id)
    ;       entity_type_ctor(module_name, string, int)
    ;       entity_const_struct(int)
    ;       entity_mutable(module_name, string, mutable_pred_kind).

:- type maybe_needed
    --->    not_eliminable
    ;       maybe_eliminable(num_references :: int).

    % Analyze which entities are needed, and for those entities which are
    % needed, record how many times they are referenced (this information
    % is used by our inlining heuristics).
    %
:- pred dead_proc_analyze(module_info::in, needed_map::out) is det.

%-----------------------------------------------------------------------------%

:- type maybe_elim_opt_imported
    --->    elim_opt_imported
    ;       do_not_elim_opt_imported.

    % Eliminate dead procedures and/or constant structures.
    % If the first argument is `elim_opt_imported', also eliminate
    % any opt_imported procedures.
    %
:- pred dead_proc_elim(maybe_elim_opt_imported::in,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%

    % Generate a warning for every user-defined dead procedure in the module.
    %
    % XXX At the moment, there are several situations in which we generate
    % warnings that (at least arguably) we shouldn't.
    %
    % - If a predicate has definitions in both Mercury and a foreign language,
    %   and we are generating code for a backend that targets that foreign
    %   language, then we don't use the Mercury clauses for the predicate.
    %   This causes us to miss calls from those clauses. If such calls
    %   are the only calls to a predicate, we will wrongly believe that
    %   predicate to never be called.
    %
    %   To fix this, we will need to (a) preserve and semantically check
    %   any Mercury clauses even for predicates which can be, and are,
    %   implemented by foreign code on the current backend, and (b)
    %   record the ids of the procedures called from these Mercury clauses
    %   in the proc_info in the field that currently holds the ids of
    %   procedures called from deleted trace goal scopes.
    %
    % - Some predicates exist to map between a (finite) enum type and another,
    %   infinite type such as "string". The intended mode of use of the
    %   predicate is as a semidet predicate to translate from the infinite type
    %   to the finite type, but the programmer may add another mode to the
    %   predicate to translate from the finite type to the infinite type.
    %   By making this predicate det, the programmer can ensure that all
    %   values in the finite type are in the range of *some* value of the
    %   infinite type. This makes this mode of the predicate useful even if
    %   it is never called.
    %
    %   We could fix this by adding an option that would ask this predicate
    %   to suppress warnings for unused det procedures of predicates which
    %   do have used semidet procedures, or simply to suppress warnings for
    %   an unused procedure unless *all* its sibling procedures are unused
    %   as well.
    %
:- pred dead_proc_warn(module_info::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

    % Optimize away any dead predicates. This is performed immediately after
    % building the HLDS to avoid doing semantic checking and optimization
    % on predicates from `.opt' files which are not used in the current module.
    %
    % This predicate assumes that the clauses_info is still valid, and
    % therefore it cannot be run after mode analysis.
    %
:- pred dead_pred_elim(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.try_expand.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module transform_hlds.direct_arg_in_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set_tree234.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

dead_proc_analyze(ModuleInfo, Needed) :-
    AnalyzeLinks = analyze_links(do_not_analyze_link_deleted_calls,
        do_not_analyze_link_type_ctor, do_not_analyze_link_const_struct),
    do_dead_proc_analyze(ModuleInfo, AnalyzeLinks, Needed).

%-----------------------------------------------------------------------------%

dead_proc_elim(ElimOptImported, !ModuleInfo) :-
    AnalyzeLinks = analyze_links(do_not_analyze_link_deleted_calls,
        analyze_link_type_ctor, analyze_link_const_struct),
    do_dead_proc_analyze(!.ModuleInfo, AnalyzeLinks, Needed),
    do_dead_proc_eliminate(ElimOptImported, Needed, !ModuleInfo).

%-----------------------------------------------------------------------------%

dead_proc_warn(ModuleInfo, Specs) :-
    AnalyzeLinks = analyze_links(analyze_link_deleted_calls,
        analyze_link_type_ctor, analyze_link_const_struct),
    do_dead_proc_analyze(ModuleInfo, AnalyzeLinks, Needed),
    do_dead_proc_warn(ModuleInfo, Needed, Specs).

%-----------------------------------------------------------------------------%

% We deal with three kinds of entities, procedures, type_ctor_info structures
% and constant structures.
%
% The algorithm has three main data structures:
%
%   - a map of entities known to be needed to either "no" (if they
%     cannot possibly be eliminated) or to "yes" and the number of their
%     uses (if they are a candidate for elimination after inlining)
%
%   - a queue of entities to be examined,
%
%   - a set of entities that have been examined.
%
% The needed map and the queue are both initialized with the ids of the
% procedures and type_ctor_info structures exported from the module.
% The algorithm then takes the ids of entities from the queue one at a time,
% and if the entity hasn't been examined before, examines the entity
% definition to find all mention of other entities. Their ids are then
% put into both the needed map and the queue.
%
% The final pass of the algorithm deletes from the HLDS any procedure,
% type_ctor_info structure or constant structure whose id is not in the
% needed map.

:- type entity_queue    ==  queue(entity).
:- type examined_set    ==  set_tree234(entity).

%-----------------------------------------------------------------------------%

:- type analyze_links
    --->    analyze_links(
                al_deleted_calls    :: maybe_analyze_link_deleted_calls,
                al_type_ctor        :: maybe_analyze_link_type_ctor,
                al_const_struct     :: maybe_analyze_link_const_struct
            ).

:- type maybe_analyze_link_deleted_calls
    --->    do_not_analyze_link_deleted_calls
    ;       analyze_link_deleted_calls.

:- type maybe_analyze_link_type_ctor
    --->    do_not_analyze_link_type_ctor
    ;       analyze_link_type_ctor.

:- type maybe_analyze_link_const_struct
    --->    do_not_analyze_link_const_struct
    ;       analyze_link_const_struct.

:- pred do_dead_proc_analyze(module_info::in, analyze_links::in,
    needed_map::out) is det.

do_dead_proc_analyze(ModuleInfo, AnalyzeLinks, !:Needed) :-
    Examined0 = set_tree234.init,
    dead_proc_initialize(ModuleInfo, Queue0, !:Needed),
    dead_proc_examine(Queue0, Examined0, AnalyzeLinks, ModuleInfo, !Needed).

    % Add all exported entities to the queue and map.
    % NOTE: changes here are likely to require changes to dead_pred_elim
    % as well.
    %
:- pred dead_proc_initialize(module_info::in,
    entity_queue::out, needed_map::out) is det.

dead_proc_initialize(ModuleInfo, !:Queue, !:Needed) :-
    !:Queue = queue.init,
    !:Needed = map.init,
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    module_info_get_preds(ModuleInfo, PredTable),
    dead_proc_initialize_preds(PredIds, PredTable, !Queue, !Needed),

    module_info_get_pragma_exported_procs(ModuleInfo, PragmaExports),
    dead_proc_initialize_pragma_exports(cord.list(PragmaExports),
        !Queue, !Needed),

    module_info_user_init_pred_procs(ModuleInfo, InitProcs),
    dead_proc_initialize_init_fn_procs(InitProcs, !Queue, !Needed),

    module_info_user_final_pred_procs(ModuleInfo, FinalPreds),
    dead_proc_initialize_init_fn_procs(FinalPreds, !Queue, !Needed),

    module_info_get_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
    dead_proc_initialize_type_ctor_infos(TypeCtorGenInfos, !Queue, !Needed),

    module_info_get_class_table(ModuleInfo, Classes),
    module_info_get_instance_table(ModuleInfo, Instances),
    dead_proc_initialize_class_methods(Classes, Instances, !Queue, !Needed).

    % Add all normally exported procedures within the listed predicates
    % to the queue and map.
    %
:- pred dead_proc_initialize_preds(list(pred_id)::in, pred_table::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_preds([], _PredTable, !Queue, !Needed).
dead_proc_initialize_preds([PredId | PredIds], PredTable, !Queue, !Needed) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, PredMarkers),
    ( if check_marker(PredMarkers, marker_consider_used) then
        LiveProcIds = pred_info_valid_procids(PredInfo)
    else
        LiveProcIds = pred_info_valid_exported_procids(PredInfo)
    ),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, Procs),
    dead_proc_initialize_procs(PredId, Procs, LiveProcIds, !Queue, !Needed),
    dead_proc_initialize_preds(PredIds, PredTable, !Queue, !Needed).

    % Add the listed procedures to the queue and map.
    %
:- pred dead_proc_initialize_procs(pred_id::in,
    assoc_list(proc_id, proc_info)::in, list(proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_procs(_PredId, [], _LiveProcIds, !Queue, !Needed).
dead_proc_initialize_procs(PredId, [Proc | Procs], LiveProcIds,
        !Queue, !Needed) :-
    Proc = ProcId - ProcInfo,
    ( if
        proc_info_is_valid_mode(ProcInfo),
        (
            list.member(ProcId, LiveProcIds)
        ;
            proc_info_get_has_any_foreign_exports(ProcInfo,
                has_foreign_exports)
        )
    then
        Entity = entity_proc(PredId, ProcId),
        queue.put(Entity, !Queue),
        map.set(Entity, not_eliminable, !Needed)
    else
        true
    ),
    dead_proc_initialize_procs(PredId, Procs, LiveProcIds, !Queue, !Needed).

    % Add procedures exported to foreign language by a `:- pragma
    % foreign_export(...)' declaration to the queue and map.
    %
:- pred dead_proc_initialize_pragma_exports(list(pragma_exported_proc)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_pragma_exports([], !Queue, !Needed).
dead_proc_initialize_pragma_exports([PragmaProc | PragmaProcs],
        !Queue, !Needed) :-
    PragmaProc = pragma_exported_proc(_Lang, PredId, ProcId,
        _ExportName, _Ctxt),
    Entity = entity_proc(PredId, ProcId),
    queue.put(Entity, !Queue),
    map.set(Entity, not_eliminable, !Needed),
    dead_proc_initialize_pragma_exports(PragmaProcs, !Queue, !Needed).

    % Add module initialisation/finalisation procedures to the queue and map
    % as they cannot be removed.
    %
:- pred dead_proc_initialize_init_fn_procs(list(pred_proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_init_fn_procs([], !Queue, !Needed).
dead_proc_initialize_init_fn_procs([PPId | PPIds], !Queue, !Needed) :-
    PPId = proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),
    queue.put(Entity, !Queue),
    map.set(Entity, not_eliminable, !Needed),
    dead_proc_initialize_init_fn_procs(PPIds, !Queue, !Needed).

:- pred dead_proc_initialize_type_ctor_infos(list(type_ctor_gen_info)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_type_ctor_infos([], !Queue, !Needed).
dead_proc_initialize_type_ctor_infos([TypeCtorGenInfo | TypeCtorGenInfos],
        !Queue, !Needed) :-
    TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName, TypeName,
        Arity, _Status, _HldsDefn, _Unify, _Compare),
    ( if
        % XXX: We'd like to do this, but there are problems.
        % status_is_exported(Status, yes)
        %
        % We need to do more thorough analysis of the reachability of the
        % special predicates, in general, because using arg/3 allows us
        % to get at type_ctor_info via the type_ctor_layout.
        % The type_ctor_infos of arguments of functors may have had their
        % special preds eliminated, but they can still be called. In addition,
        % it would be nice for pragma C code to have some support for using
        % compiler generated data structures and preds, so that they aren't
        % just eliminated.
        %
        % So presently, all type_ctor_infos will be treated as exported,
        % and hence no special preds will be eliminated.
        semidet_succeed
    then
        Entity = entity_type_ctor(ModuleName, TypeName, Arity),
        queue.put(Entity, !Queue),
        map.set(Entity, not_eliminable, !Needed)
    else
        true
    ),
    dead_proc_initialize_type_ctor_infos(TypeCtorGenInfos, !Queue, !Needed).

:- pred dead_proc_initialize_class_methods(class_table::in, instance_table::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_class_methods(Classes, Instances,
        !Queue, !Needed) :-
    map.values(Instances, InstanceDefnsLists),
    list.condense(InstanceDefnsLists, InstanceDefns),
    list.foldl2(get_instance_pred_procs, InstanceDefns, !Queue, !Needed),
    map.values(Classes, ClassDefns),
    list.foldl2(get_class_pred_procs, ClassDefns, !Queue, !Needed).

:- pred get_instance_pred_procs(hlds_instance_defn::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_instance_pred_procs(Instance, !Queue, !Needed) :-
    Instance = hlds_instance_defn(_, _, _, _, _, _, _, PredProcIds, _, _),
    % We need to keep the instance methods for all instances
    % for optimization of method lookups.
    (
        % This should never happen.
        PredProcIds = no
    ;
        PredProcIds = yes(Ids),
        list.foldl2(get_class_interface_pred_proc, Ids, !Queue, !Needed)
    ).

:- pred get_class_pred_procs(hlds_class_defn::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_class_pred_procs(Class, !Queue, !Needed) :-
    Methods = Class ^ classdefn_hlds_interface,
    list.foldl2(get_class_interface_pred_proc, Methods, !Queue, !Needed).

:- pred get_class_interface_pred_proc(pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_class_interface_pred_proc(ClassProc, !Queue, !Needed) :-
    ClassProc = proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),
    queue.put(Entity, !Queue),
    map.set(Entity, not_eliminable, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine(entity_queue::in, examined_set::in,
    analyze_links::in, module_info::in, needed_map::in, needed_map::out)
    is det.

dead_proc_examine(!.Queue, !.Examined, AnalyzeLinks, ModuleInfo, !Needed) :-
    % See if the queue is empty.
    ( if queue.get(Entity, !Queue) then
        % See if the next element has been examined before.
        ( if set_tree234.insert_new(Entity, !Examined) then
            (
                Entity = entity_proc(PredId, ProcId),
                PredProcId = proc(PredId, ProcId),
                AnalyzeTraceGoalProcs = AnalyzeLinks ^ al_deleted_calls,
                dead_proc_examine_proc(PredProcId, AnalyzeTraceGoalProcs,
                    ModuleInfo, !Queue, !Needed)
            ;
                ( Entity = entity_table_struct(_PredId, _ProcId)
                ; Entity = entity_mutable(_ModuleName, _Name, _PredKind)
                )
                % Nothing further to examine.
            ;
                Entity = entity_type_ctor(Module, Type, Arity),
                AnalyzeTypeCtor = AnalyzeLinks ^ al_type_ctor,
                (
                    AnalyzeTypeCtor = do_not_analyze_link_type_ctor
                ;
                    AnalyzeTypeCtor = analyze_link_type_ctor,
                    dead_proc_examine_type_ctor_info(Module, Type, Arity,
                        ModuleInfo, !Queue, !Needed)
                )
            ;
                Entity = entity_const_struct(ConstNum),
                AnalyzeConstStruct = AnalyzeLinks ^ al_const_struct,
                (
                    AnalyzeConstStruct = do_not_analyze_link_const_struct
                ;
                    AnalyzeConstStruct = analyze_link_const_struct,
                    dead_proc_examine_const_struct(ModuleInfo, ConstNum,
                        !Queue, !Needed)
                )
            )
        else
            true
        ),
        disable_warning [suspicious_recursion] (
            dead_proc_examine(!.Queue, !.Examined, AnalyzeLinks,
                ModuleInfo, !Needed)
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_type_ctor_info(module_name::in, string::in,
    arity::in, module_info::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_type_ctor_info(ModuleName, TypeName, Arity, ModuleInfo,
        !Queue, !Needed) :-
    module_info_get_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
    ( if
        find_type_ctor_info(ModuleName, TypeName, Arity, TypeCtorGenInfos,
            Refs)
    then
        dead_proc_examine_refs(Refs, !Queue, !Needed)
    else
        true
    ).

:- pred find_type_ctor_info(module_name::in, string::in,
    arity::in, list(type_ctor_gen_info)::in, list(pred_proc_id)::out)
    is semidet.

find_type_ctor_info(ModuleName, TypeName, TypeArity,
        [TypeCtorGenInfo | TypeCtorGenInfos], Refs) :-
    ( if
        TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName,
            TypeName, TypeArity, _Status, _HldsDefn, Unify, Compare)
    then
        Refs = [Unify, Compare]
    else
        find_type_ctor_info(ModuleName, TypeName, TypeArity, TypeCtorGenInfos,
            Refs)
    ).

:- pred dead_proc_examine_refs(list(pred_proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_refs([], !Queue, !Needed).
dead_proc_examine_refs([Ref | Refs], !Queue, !Needed) :-
    Ref = proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),
    queue.put(Entity, !Queue),
    map.set(Entity, not_eliminable, !Needed),
    dead_proc_examine_refs(Refs, !Queue, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_const_struct(module_info::in, int::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_const_struct(ModuleInfo, ConstNum, !Queue, !Needed) :-
    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    lookup_const_struct_num(ConstStructDb, ConstNum, ConstStruct),
    ConstStruct = const_struct(ConsId, Args, _, _, _),
    ( if ConsId = type_ctor_info_const(Module, TypeName, Arity) then
        Entity = entity_type_ctor(Module, TypeName, Arity),
        queue.put(Entity, !Queue),
        map.set(Entity, not_eliminable, !Needed)
    else
        true
    ),
    dead_proc_examine_const_struct_args(Args, !Queue, !Needed).

:- pred dead_proc_examine_const_struct_args(list(const_struct_arg)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_const_struct_args([], !Queue, !Needed).
dead_proc_examine_const_struct_args([Arg | Args], !Queue, !Needed) :-
    (
        Arg = csa_const_struct(ConstNum),
        Entity = entity_const_struct(ConstNum),
        queue.put(Entity, !Queue),
        map.set(Entity, not_eliminable, !Needed)
    ;
        Arg = csa_constant(ConsId, _),
        ( if ConsId = type_ctor_info_const(Module, TypeName, Arity) then
            Entity = entity_type_ctor(Module, TypeName, Arity),
            queue.put(Entity, !Queue),
            map.set(Entity, not_eliminable, !Needed)
        else
            true
        )
    ),
    dead_proc_examine_const_struct_args(Args, !Queue, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_proc(pred_proc_id::in,
    maybe_analyze_link_deleted_calls::in, module_info::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_proc(proc(PredId, ProcId), AnalyzeTraceGoalProcs,
        ModuleInfo, !Queue, !Needed) :-
    ( if
        module_info_get_preds(ModuleInfo, PredTable),
        map.lookup(PredTable, PredId, PredInfo),
        ProcIds = pred_info_valid_non_imported_procids(PredInfo),
        list.member(ProcId, ProcIds),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo)
    then
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream, "examining proc %d %d\n",
                [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))], !IO)
        ),
        proc_info_get_goal(ProcInfo, Goal),
        dead_proc_examine_goal(Goal, proc(PredId, ProcId),
            ModuleInfo, !Queue, !Needed),
        (
            AnalyzeTraceGoalProcs = do_not_analyze_link_deleted_calls
        ;
            AnalyzeTraceGoalProcs = analyze_link_deleted_calls,
            proc_info_get_deleted_call_callees(ProcInfo, DeletedCallCallees),
            set.foldl2(need_trace_goal_proc, DeletedCallCallees,
                !Queue, !Needed)
        ),

        proc_info_get_eval_method(ProcInfo, EvalMethod),
        HasPerProcTablingPtr =
            eval_method_has_per_proc_tabling_pointer(EvalMethod),
        (
            HasPerProcTablingPtr = no
        ;
            HasPerProcTablingPtr = yes,
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.format(DebugStream, "need table struct for proc %d %d\n",
                    [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                    !IO)
            ),
            TableStructEntity = entity_table_struct(PredId, ProcId),
            map.set(TableStructEntity, not_eliminable, !Needed)
        ),
        pred_info_get_origin(PredInfo, Origin),
        ( if Origin = origin_mutable(ModuleName, MutableName, PredKind) then
            MutableEntity = entity_mutable(ModuleName, MutableName, PredKind),
            map.set(MutableEntity, not_eliminable, !Needed)
        else
            true
        )
    else
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream, "not examining proc %d %d\n",
                [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))], !IO)
        )
    ).

:- pred need_trace_goal_proc(pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

need_trace_goal_proc(TraceGoalProc, !Queue, !Needed) :-
    TraceGoalProc = proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),
    % If we are following links that correspond to eliminated calls,
    % then aren't actually trying to eliminate procedures, so whether
    % we record the callee of that eliminated call as eliminable or not
    % does not matter.
    map.set(Entity, not_eliminable, !Needed),
    queue.put(Entity, !Queue).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_goals(list(hlds_goal)::in, pred_proc_id::in,
    module_info::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_goals([], _, _, !Queue, !Needed).
dead_proc_examine_goals([Goal | Goals], CurrProc, ModuleInfo,
        !Queue, !Needed) :-
    dead_proc_examine_goal(Goal, CurrProc, ModuleInfo, !Queue, !Needed),
    dead_proc_examine_goals(Goals, CurrProc, ModuleInfo, !Queue, !Needed).

:- pred dead_proc_examine_cases(list(case)::in, pred_proc_id::in,
    module_info::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_cases([], _, _, !Queue, !Needed).
dead_proc_examine_cases([case(_, _, Goal) | Cases], CurrProc,
        ModuleInfo, !Queue, !Needed) :-
    dead_proc_examine_goal(Goal, CurrProc, ModuleInfo, !Queue, !Needed),
    dead_proc_examine_cases(Cases, CurrProc, ModuleInfo, !Queue, !Needed).

:- pred dead_proc_examine_goal(hlds_goal::in, pred_proc_id::in,
    module_info::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_goal(Goal, CurrProc, ModuleInfo, !Queue, !Needed) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        dead_proc_examine_goals(Goals, CurrProc, ModuleInfo, !Queue, !Needed)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        dead_proc_examine_cases(Cases, CurrProc, ModuleInfo, !Queue, !Needed)
    ;
        GoalExpr = negation(SubGoal),
        dead_proc_examine_goal(SubGoal, CurrProc, ModuleInfo, !Queue, !Needed)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % The scope has no references to procedures at all.
            true
        else
            dead_proc_examine_goal(SubGoal, CurrProc, ModuleInfo,
                !Queue, !Needed)
        )
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        dead_proc_examine_goal(Cond, CurrProc, ModuleInfo, !Queue, !Needed),
        dead_proc_examine_goal(Then, CurrProc, ModuleInfo, !Queue, !Needed),
        dead_proc_examine_goal(Else, CurrProc, ModuleInfo, !Queue, !Needed)
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _,_,_,_),
        Entity = entity_proc(PredId, ProcId),
        queue.put(Entity, !Queue),
        ( if proc(PredId, ProcId) = CurrProc then
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.format(DebugStream, "plain_call recursive %d %d\n",
                    [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                    !IO)
            ),
            % If it is reachable and recursive, then we cannot eliminate it
            % or inline it.
            NewNotation = not_eliminable,
            map.set(Entity, NewNotation, !Needed)
        else if map.search(!.Needed, Entity, OldNotation) then
            (
                OldNotation = not_eliminable,
                NewNotation = not_eliminable,
                trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                    get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                    io.format(DebugStream,
                        "plain_call old not_eliminable %d %d\n",
                        [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                        !IO)
                )
            ;
                OldNotation = maybe_eliminable(Count),
                NewNotation = maybe_eliminable(Count + 1),
                trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                    get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                    io.format(DebugStream,
                        "plain_call incr maybe_eliminable %d %d\n",
                        [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                        !IO)
                )
            ),
            map.det_update(Entity, NewNotation, !Needed)
        else
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.format(DebugStream,
                    "plain_call init maybe_eliminable %d %d\n",
                    [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                    !IO)
            ),
            NewNotation = maybe_eliminable(1),
            map.det_insert(Entity, NewNotation, !Needed)
        )
    ;
        GoalExpr = call_foreign_proc(_, PredId, ProcId, _, _, _, _),
        Entity = entity_proc(PredId, ProcId),
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream, "foreign_proc %d %d\n",
                [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))], !IO)
        ),
        queue.put(Entity, !Queue),
        map.set(Entity, not_eliminable, !Needed)
    ;
        GoalExpr = unify(_LHS, _RHS, _UniModes, Unification, _UnifyContext),
        (
            Unification = construct(_, ConsId, _, _, _, _, _),
            (
                (
                    ConsId = closure_cons(ShroudedPredProcId, _),
                    proc(PredId, ProcId) =
                        unshroud_pred_proc_id(ShroudedPredProcId),
                    Entity = entity_proc(PredId, ProcId),
                    trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                        get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                        io.format(DebugStream, "pred_const %d %d\n",
                            [i(pred_id_to_int(PredId)),
                            i(proc_id_to_int(ProcId))], !IO)
                    )
                ;
                    ConsId = type_ctor_info_const(Module, TypeName, Arity),
                    Entity = entity_type_ctor(Module, TypeName, Arity)
                ;
                    ConsId = tabling_info_const(ShroudedPredProcId),
                    proc(PredId, ProcId) =
                        unshroud_pred_proc_id(ShroudedPredProcId),
                    Entity = entity_table_struct(PredId, ProcId),
                    trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                        get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                        io.format(DebugStream, "table struct const %d %d\n",
                            [i(pred_id_to_int(PredId)),
                            i(proc_id_to_int(ProcId))], !IO)
                    )
                ),
                queue.put(Entity, !Queue),
                map.set(Entity, not_eliminable, !Needed)
            ;
                ( ConsId = type_info_const(ConstNum)
                ; ConsId = typeclass_info_const(ConstNum)
                ; ConsId = ground_term_const(ConstNum, _)
                ),
                Entity = entity_const_struct(ConstNum),
                queue.put(Entity, !Queue),
                map.set(Entity, not_eliminable, !Needed)
            ;
                ( ConsId = cons(_, _, _)
                ; ConsId = tuple_cons(_)
                ; ConsId = int_const(_)
                ; ConsId = uint_const(_)
                ; ConsId = int8_const(_)
                ; ConsId = uint8_const(_)
                ; ConsId = int16_const(_)
                ; ConsId = uint16_const(_)
                ; ConsId = int32_const(_)
                ; ConsId = uint32_const(_)
                ; ConsId = int64_const(_)
                ; ConsId = uint64_const(_)
                ; ConsId = float_const(_)
                ; ConsId = char_const(_)
                ; ConsId = string_const(_)
                ; ConsId = impl_defined_const(_)
                ; ConsId = base_typeclass_info_const(_, _, _, _)
                ; ConsId = type_info_cell_constructor(_)
                ; ConsId = typeclass_info_cell_constructor
                ; ConsId = deep_profiling_proc_layout(_)
                ; ConsId = table_io_entry_desc(_)
                )
                % Do nothing.
            )
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
            % Do nothing. These kinds of unifications cannot include
            % the kinds of cons_ids that we are looking for.
        ;
            Unification = complicated_unify(_, _, _),
            % These should have been replaced with calls by now.
            unexpected($pred, "complicated_unify")
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

        % Information used during the procedure elimination phase.

:- type proc_elim_info
    --->    proc_elim_info(
                % Collected usage counts.
                proc_elim_needed_map    :: needed_map,

                proc_elim_module_info   :: module_info,

                % Table of predicates in this module: preds and procs
                % in this table may be eliminated.
                proc_elim_pred_table    :: pred_table,

                % Has anything changed that could affect dependency_info.
                proc_elim_changed       :: bool
            ).

    % Given the information about which entities are needed,
    % eliminate entities which are *not* needed.
    %
:- pred do_dead_proc_eliminate(maybe_elim_opt_imported::in, needed_map::in,
    module_info::in, module_info::out) is det.

do_dead_proc_eliminate(ElimOptImported, !.Needed, !ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    Changed0 = no,
    ProcElimInfo0 = proc_elim_info(!.Needed, !.ModuleInfo, PredTable0,
        Changed0),
    list.foldl(dead_proc_eliminate_pred(ElimOptImported), PredIds,
        ProcElimInfo0, ProcElimInfo),
    ProcElimInfo = proc_elim_info(!:Needed, !:ModuleInfo, PredTable,
        Changed),
    module_info_set_preds(PredTable, !ModuleInfo),

    module_info_get_type_ctor_gen_infos(!.ModuleInfo, TypeCtorGenInfos0),
    dead_proc_eliminate_type_ctor_infos(TypeCtorGenInfos0, !.Needed,
        TypeCtorGenInfos),
    module_info_set_type_ctor_gen_infos(TypeCtorGenInfos, !ModuleInfo),

    % We could also eliminate table structs, but we don't do that
    % yet, because some references to such structs are currently visible
    % only in C code embedded in compiler-generated foreign_procs, and
    % therefore we might accidentally create dangling references.

    module_info_get_const_struct_db(!.ModuleInfo, ConstStructDb0),
    const_struct_db_get_structs(ConstStructDb0, ConstNumStructs0),
    dead_proc_eliminate_const_structs(ConstNumStructs0, !.Needed,
        ConstStructDb0, ConstStructDb),
    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    (
        Changed = yes,
        % The dependency graph will still contain references to the eliminated
        % procedures, so it must be rebuilt if it will be used later.
        module_info_clobber_dependency_info(!ModuleInfo)
    ;
        Changed = no
    ).

%-----------------------------------------------------------------------------%

:- pred dead_proc_eliminate_type_ctor_infos(list(type_ctor_gen_info)::in,
    needed_map::in, list(type_ctor_gen_info)::out) is det.

dead_proc_eliminate_type_ctor_infos([], _Needed, []).
dead_proc_eliminate_type_ctor_infos([TypeCtorGenInfo0 | TypeCtorGenInfos0],
        Needed, TypeCtorGenInfos) :-
    dead_proc_eliminate_type_ctor_infos(TypeCtorGenInfos0, Needed,
        TypeCtorGenInfos1),
    TypeCtorGenInfo0 = type_ctor_gen_info(_TypeCtor, ModuleName,
        TypeName, Arity, _Status, _HldsDefn, _Unify, _Compare),
    ( if
        Entity = entity_type_ctor(ModuleName, TypeName, Arity),
        map.search(Needed, Entity, _)
    then
        TypeCtorGenInfos = [TypeCtorGenInfo0 | TypeCtorGenInfos1]
    else
        TypeCtorGenInfos = TypeCtorGenInfos1
    ).

%-----------------------------------------------------------------------------%

:- pred dead_proc_eliminate_const_structs(assoc_list(int, const_struct)::in,
    needed_map::in, const_struct_db::in, const_struct_db::out) is det.

dead_proc_eliminate_const_structs([], _Needed, !ConstStructDb).
dead_proc_eliminate_const_structs([ConstNum - _ | ConstNumStructs], Needed,
        !ConstStructDb) :-
    Entity = entity_const_struct(ConstNum),
    ( if map.search(Needed, Entity, _) then
        true
    else
        delete_const_struct(ConstNum, !ConstStructDb)
    ),
    dead_proc_eliminate_const_structs(ConstNumStructs, Needed,
        !ConstStructDb).

%-----------------------------------------------------------------------------%

    % Eliminate any unused procedures for this pred.
    %
:- pred dead_proc_eliminate_pred(maybe_elim_opt_imported::in, pred_id::in,
    proc_elim_info::in, proc_elim_info::out) is det.

dead_proc_eliminate_pred(ElimOptImported, PredId, !ProcElimInfo) :-
    !.ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable0, Changed0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_status(PredInfo0, PredStatus),
    ( if
        % Find out if the predicate is defined in this module.
        % If yes, find out also whether any of its procedures must be kept.
        (
            PredStatus = pred_status(status_local),
            Keep = no
        ;
            PredStatus = pred_status(status_pseudo_imported),
            Keep = no
        ;
            PredStatus = pred_status(status_pseudo_exported),
            hlds_pred.in_in_unification_proc_id(InitProcId),
            Keep = yes(InitProcId)
        )
    then
        ProcIds = pred_info_valid_procids(PredInfo0),
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        list.foldl2(dead_proc_eliminate_proc(PredId, Keep, !.ProcElimInfo),
            ProcIds, ProcTable0, ProcTable, Changed0, Changed),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, PredTable0, PredTable),
        !:ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable, Changed)
    else if
        % Don't generate code in the current module for unoptimized
        % opt_imported preds (that is, for opt_imported preds which we have not
        % by this point managed to inline or specialize; this code should be
        % called with `ElimOptImported = elim_opt_imported' only after inlining
        % and specialization is complete).
        ElimOptImported = elim_opt_imported,
        PredStatus = pred_status(status_opt_imported)
    then
        Changed = yes,
        ProcIds = pred_info_valid_procids(PredInfo0),
        pred_info_get_proc_table(PredInfo0, ProcTable0),

        % Reduce memory usage by replacing the goals with "true".
        DestroyGoal =
            ( pred(Id::in, PTable0::in, PTable::out) is det :-
                map.lookup(ProcTable0, Id, ProcInfo0),
                proc_info_set_goal(true_goal, ProcInfo0, ProcInfo),
                map.det_update(Id, ProcInfo, PTable0, PTable)
            ),
        list.foldl(DestroyGoal, ProcIds, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo1),
        pred_info_set_status(
            pred_status(status_imported(import_locn_interface)),
            PredInfo1, PredInfo),
        map.det_update(PredId, PredInfo, PredTable0, PredTable),

        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                write_pred_progress_message(
                    "% Eliminated opt_imported predicate ",
                    PredId, ModuleInfo, !IO)
            )
        ;
            VeryVerbose = no
        ),
        !:ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable, Changed)
    else
        % This predicate is not defined in this module.
        true
    ).

    % Eliminate a procedure, if unused.
    %
:- pred dead_proc_eliminate_proc(pred_id::in, maybe(proc_id)::in,
    proc_elim_info::in, proc_id::in, proc_table::in, proc_table::out,
    bool::in, bool::out) is det.

dead_proc_eliminate_proc(PredId, Keep, ProcElimInfo, ProcId,
        !ProcTable, !Changed) :-
    Needed = ProcElimInfo ^ proc_elim_needed_map,
    ModuleInfo = ProcElimInfo ^ proc_elim_module_info,
    ( if
        (
            % Keep the procedure if it is in the needed map.
            map.contains(Needed, entity_proc(PredId, ProcId))
        ;
            % Or if it is to be kept because it is exported.
            Keep = yes(ProcId)
        )
    then
        true
    else
        map.delete(ProcId, !ProcTable),
        !:Changed = yes,

        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                write_proc_progress_message(
                    "% Eliminated the dead procedure ",
                    PredId, ProcId, ModuleInfo, !IO)
            )
        ;
            VeryVerbose = no
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Given the information about which entities are needed,
    % eliminate procedures which are not needed.
    %
:- pred do_dead_proc_warn(module_info::in, needed_map::in,
    list(error_spec)::out) is det.

do_dead_proc_warn(ModuleInfo, Needed, Specs) :-
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    module_info_get_preds(ModuleInfo, PredTable),
    module_info_get_globals(ModuleInfo, Globals),
    % We get called only if either warn_dead_procs or warn_dead_preds is set.
    % We warn about procedures of entirely dead predicates if either is set
    % (or if both are set); the only difference is whether we warn about
    % procedures with live siblings.
    globals.lookup_bool_option(Globals, warn_dead_procs, WarnWithLiveSiblings),
    list.foldl(
        dead_proc_warn_pred(ModuleInfo, PredTable, WarnWithLiveSiblings,
            Needed),
        PredIds, [], Specs).

    % Warn about any unused procedures for this pred.
    %
:- pred dead_proc_warn_pred(module_info::in, pred_table::in, bool::in,
    needed_map::in, pred_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

dead_proc_warn_pred(ModuleInfo, PredTable, WarnWithLiveSiblings, Needed,
        PredId, !Specs) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    ( if
        % Generate a warning only if the predicate is defined in this module,
        % and is not exported.
        PredStatus = pred_status(status_local),

        % Don't warn for unify or comparison preds,
        % since they may be automatically generated.
        not is_unify_index_or_compare_pred(PredInfo),

        % Don't warn for procedures introduced from lambda expressions.
        % The only time those procedures will be unused is if the
        % procedure containing the lambda expression is unused,
        % and in that case, we already warn for that containing
        % procedure if appropriate.
        PredName = pred_info_name(PredInfo),
        not string.prefix(PredName, "IntroducedFrom__"),

        % Likewise, don't warn for procedures introduced for
        % type specialization.
        not string.prefix(PredName, "TypeSpecOf__")
    then
        ProcIds = pred_info_valid_procids(PredInfo),
        pred_info_get_proc_table(PredInfo, ProcTable),
        list.foldl(
            dead_proc_maybe_warn_proc(ModuleInfo, Needed, PredId, PredInfo,
                WarnWithLiveSiblings, ProcIds, ProcTable),
            ProcIds, !Specs)
    else
        true
    ).

    % If WarnWithLiveSiblings = yes:
    %   warn about a procedure only if the procedure is unused.
    % If WarnWithLiveSiblings = no:
    %   warn about a procedure only if the procedure is unused,
    %   and none of the OTHER procedures in the predicate is used.
    %
    % Regardless of the value of WarnWithLiveSiblings, if the procedure
    % is an access predicate for a mutable, warn only if the user can do
    % something to cause the unused access predicate to not be generated,
    % while getting the access predicates that *are* used to be generated.
    % This may mean changing the mutable's attributes, or (if the mutable
    % is not used at all) deleting the mutable. The key consideration is that
    % we don't want to generate a warning if the user cannot do anything
    % useful to shut up that warning.
    %
:- pred dead_proc_maybe_warn_proc(module_info::in, needed_map::in, pred_id::in,
    pred_info::in, bool::in, list(proc_id)::in, proc_table::in, proc_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

dead_proc_maybe_warn_proc(ModuleInfo, Needed, PredId, PredInfo,
        WarnWithLiveSiblings, AllProcsInPred, ProcTable, ProcId, !Specs) :-
    ( if
        (
            % Don't warn about the procedure being unused
            % if it is in the needed map.
            map.contains(Needed, entity_proc(PredId, ProcId))
        ;
            WarnWithLiveSiblings = no,
            some [OtherProcId] (
                list.member(OtherProcId, AllProcsInPred),
                map.contains(Needed, entity_proc(PredId, OtherProcId))
            )
        ;
            pred_info_get_origin(PredInfo, Origin),
            Origin = origin_mutable(MutableModuleName, MutableName, PredKind),
            suppress_unused_mutable_access_pred(PredKind, SuppressPredKinds),
            some [SuppressPredKind] (
                list.member(SuppressPredKind, SuppressPredKinds),
                SuppressMutableEntity = entity_mutable(MutableModuleName,
                    MutableName, SuppressPredKind),
                map.contains(Needed, SuppressMutableEntity)
            )
        )
    then
        true
    else
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_context(ProcInfo, Context),
        Spec = warn_dead_proc(ModuleInfo, PredId, ProcId, Context),
        !:Specs = [Spec | !.Specs]
    ).

    % Given the id of an unused access predicate for a mutable,
    % return the set of other access predicates that, if any one of them
    % *is* used, should suppress the warning for the unused predicate.
    %
:- pred suppress_unused_mutable_access_pred(mutable_pred_kind::in,
    list(mutable_pred_kind)::out) is det.

suppress_unused_mutable_access_pred(Unused, Suppress) :-
    % Note that having Unused in Suppress is harmless,
    % and it lets us share the same code for some Unuseds.
    (
        Unused = mutable_pred_std_get,
        % If the mutable is read via the get predicate that uses the I/O state,
        % then the mutable is needed.
        Suppress = [mutable_pred_io_get]
    ;
        Unused = mutable_pred_std_set,
        % If the mutable is read, then the mutable is needed. Maybe it does not
        % need to be writeable, but it *is* needed.
        Suppress = [mutable_pred_std_get, mutable_pred_io_get]
    ;
        Unused = mutable_pred_constant_get,
        % This is the only user visible access predicate for a constant
        % mutable. If this is not used, the mutable is not needed.
        Suppress = []
    ;
        Unused = mutable_pred_constant_secret_set,
        % The secret set is used during initialization of the mutable.
        % This is needed if the mutable is ever read. If there is a constant
        % set predicate, the reading can be done only via the constant get
        % access predicate.
        Suppress = [mutable_pred_constant_get]
    ;
        ( Unused = mutable_pred_io_get
        ; Unused = mutable_pred_io_set
        ),
        % If either of the io get and io set predicates is used, then
        % the attach_to_io_state attribute is needed, which means that
        % the *other* predicate of the pair will also be generated.
        % If neither of the two io state access predicates is used,
        % then the mutable itself may be needed, but its attachment to
        % the I/O state is not.
        Suppress = [mutable_pred_io_get, mutable_pred_io_set]
    ;
        ( Unused = mutable_pred_unsafe_get
        ; Unused = mutable_pred_unsafe_set
        ),
        % If the unsafe get and unsafe set predicates exist, then they
        % were created to implement one of the user-visible access predicates.
        Suppress =
            [mutable_pred_std_get, mutable_pred_std_set,
            mutable_pred_io_get, mutable_pred_io_set,
            mutable_pred_unsafe_get, mutable_pred_unsafe_set,
            mutable_pred_constant_get]
    ;
        ( Unused = mutable_pred_pre_init
        ; Unused = mutable_pred_init
        ; Unused = mutable_pred_lock
        ; Unused = mutable_pred_unlock
        ),
        % The init predicate is needed if any of the user-visible access
        % predicates are used.
        %
        % If any of the other three "infrastructure" predicates is actually
        % generated (which depends on the target platform), then the same
        % is true for them.
        Suppress =
            [mutable_pred_std_get, mutable_pred_std_set,
            mutable_pred_io_get, mutable_pred_io_set,
            mutable_pred_unsafe_get, mutable_pred_unsafe_set,
            mutable_pred_constant_get]
    ).

:- func warn_dead_proc(module_info, pred_id, proc_id, prog_context)
    = error_spec.

warn_dead_proc(ModuleInfo, PredId, ProcId, Context) = Spec :-
    ProcPieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("Warning:")] ++ ProcPieces ++
        [words("is never called."), nl],
    Spec = simplest_spec($pred, severity_warning, phase_dead_code,
        Context, Pieces).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type pred_elim_info
    --->    pred_elim_info(
                pred_elim_module_info   :: module_info,
                pred_elim_queue         :: queue(pred_id), % preds to examine.

                % preds examined.
                pred_elim_examined      :: set_tree234(pred_id),

                % needed pred_ids.
                pred_elim_needed_ids    :: set_tree234(pred_id),

                % pred names needed.
                pred_elim_needed_named  :: set_tree234(sym_name)
            ).

dead_pred_elim(!ModuleInfo) :-
    queue.init(Queue0),
    map.init(Needed0),
    module_info_get_pragma_exported_procs(!.ModuleInfo, PragmaExports),
    dead_proc_initialize_pragma_exports(cord.list(PragmaExports), Queue0, _,
        Needed0, Needed1),

    % The goals for the class method procs need to be examined because
    % they contain calls to the actual method implementations.

    module_info_get_instance_table(!.ModuleInfo, Instances),
    module_info_get_class_table(!.ModuleInfo, Classes),
    dead_proc_initialize_class_methods(Classes, Instances, Queue0, _,
        Needed1, Needed),
    map.keys(Needed, Entities),
    queue.init(Queue1),
    NeededPreds0 = set_tree234.init,
    list.foldl2(dead_pred_elim_add_entity, Entities, Queue1, Queue,
        NeededPreds0, NeededPreds1),

    module_info_get_type_table(!.ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorDefns),
    list.foldl(dead_pred_initialize_referred_preds(!.ModuleInfo),
        TypeCtorDefns, NeededPreds1, NeededPreds2),

    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),

    Preds0 = set_tree234.init,
    Names0 = set_tree234.init,
    DeadInfo0 = pred_elim_info(!.ModuleInfo, Queue, Preds0, NeededPreds2,
        Names0),
    list.foldl(dead_pred_elim_initialize, PredIds, DeadInfo0, DeadInfo1),
    dead_pred_elim_analyze(DeadInfo1, DeadInfo),
    DeadInfo = pred_elim_info(!:ModuleInfo, _, _, NeededPreds3, _),

    % If a predicate is not needed, predicates which were added in make_hlds.m
    % to force type specialization are also not needed. Here we add in those
    % which are needed.

    module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo0),
    TypeSpecInfo0 = type_spec_info(TypeSpecProcs0, TypeSpecForcePreds0,
        SpecMap0, PragmaMap0),
    set_tree234.to_sorted_list(NeededPreds3) = NeededPredList3,
    list.foldl(
        ( pred(NeededPred::in, AllPreds0::in, AllPreds::out) is det :-
            ( if map.search(SpecMap0, NeededPred, NewNeededPreds) then
                set_tree234.insert_list(NewNeededPreds, AllPreds0, AllPreds)
            else
                AllPreds = AllPreds0
            )
        ), NeededPredList3, NeededPreds3, NeededPreds),
    % We expect TypeSpecForcePreds0 to have very few elements, and NeededPreds
    % to have many. Therefore doing an individual O(log N) lookup in
    % NeededPreds for each element of TypeSpecForcePreds0 is a better approach
    % than an intersection operation that would have to traverse all of
    % NeededPreds.
    TypeSpecForcePreds =
        set.filter(set_tree234.contains(NeededPreds), TypeSpecForcePreds0),

    TypeSpecInfo = type_spec_info(TypeSpecProcs0, TypeSpecForcePreds,
        SpecMap0, PragmaMap0),
    module_info_set_type_spec_info(TypeSpecInfo, !ModuleInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    module_info_get_partial_qualifier_info(!.ModuleInfo, PartialQualInfo),
    predicate_table_restrict(PartialQualInfo,
        set_tree234.to_sorted_list(NeededPreds), PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

:- pred dead_pred_initialize_referred_preds(module_info::in,
    pair(type_ctor, hlds_type_defn)::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out) is det.

dead_pred_initialize_referred_preds(ModuleInfo, _TypeCtor - TypeDefn,
        !NeededPreds) :-
    get_type_defn_body(TypeDefn, TypeDefnBody),
    (
        TypeDefnBody = hlds_du_type(_Ctors, _MaybeSuperType, MaybeCanon, _Repn,
            _IsForeign),
        dead_pred_initialize_maybe_canonical(ModuleInfo, MaybeCanon,
            !NeededPreds)
    ;
        TypeDefnBody = hlds_foreign_type(ForeignTypeBody),
        ForeignTypeBody = foreign_type_body(C, Java, CSharp),
        dead_pred_initialize_forein_type_lang_body(ModuleInfo, C,
            !NeededPreds),
        dead_pred_initialize_forein_type_lang_body(ModuleInfo, Java,
            !NeededPreds),
        dead_pred_initialize_forein_type_lang_body(ModuleInfo, CSharp,
            !NeededPreds)
    ;
        TypeDefnBody = hlds_solver_type(DetailsSolver),
        DetailsSolver = type_details_solver(_SolverTypeDetails, MaybeCanon),
        dead_pred_initialize_maybe_canonical(ModuleInfo, MaybeCanon,
            !NeededPreds)
    ;
        ( TypeDefnBody = hlds_eqv_type(_)
        ; TypeDefnBody = hlds_abstract_type(_)
        )
    ).

:- pred dead_pred_initialize_forein_type_lang_body(module_info::in,
    foreign_type_lang_body(_)::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out) is det.

dead_pred_initialize_forein_type_lang_body(ModuleInfo, ForeignTypeLangBody,
        !NeededPreds) :-
    (
        ForeignTypeLangBody = no
    ;
        ForeignTypeLangBody = yes(type_details_foreign(_, MaybeCanon, _)),
        dead_pred_initialize_maybe_canonical(ModuleInfo, MaybeCanon,
            !NeededPreds)
    ).

:- pred dead_pred_initialize_maybe_canonical(module_info::in,
    maybe_canonical::in,
    set_tree234(pred_id)::in, set_tree234(pred_id)::out) is det.

dead_pred_initialize_maybe_canonical(ModuleInfo, MaybeCanon, !NeededPreds) :-
    (
        MaybeCanon = canon
    ;
        MaybeCanon = noncanon(NonCanonical),
        module_info_get_predicate_table(ModuleInfo, PredTable),
        (
            NonCanonical = noncanon_uni_cmp(UniPredSymName, CmpPredSymName),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                UniPredSymName, 2, UniPredIds),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                CmpPredSymName, 3, CmpPredIds),
            set_tree234.insert_list(UniPredIds, !NeededPreds),
            set_tree234.insert_list(CmpPredIds, !NeededPreds)
        ;
            NonCanonical = noncanon_uni_only(UniPredSymName),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                UniPredSymName, 2, UniPredIds),
            set_tree234.insert_list(UniPredIds, !NeededPreds)
        ;
            NonCanonical = noncanon_cmp_only(CmpPredSymName),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                CmpPredSymName, 3, CmpPredIds),
            set_tree234.insert_list(CmpPredIds, !NeededPreds)
        ;
            NonCanonical = noncanon_abstract(_IsSolverType)
        )
    ).

:- pred dead_pred_elim_add_entity(entity::in, queue(pred_id)::in,
    queue(pred_id)::out, set_tree234(pred_id)::in, set_tree234(pred_id)::out)
    is det.

dead_pred_elim_add_entity(Entity, !Queue, !Preds) :-
    (
        Entity = entity_proc(PredId, _),
        queue.put(PredId, !Queue),
        set_tree234.insert(PredId, !Preds)
    ;
        ( Entity = entity_table_struct(_, _)
        ; Entity = entity_type_ctor(_, _, _)
        ; Entity = entity_const_struct(_)
        ; Entity = entity_mutable(_, _, _)
        )
    ).

:- pred dead_pred_elim_initialize(pred_id::in,
    pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_elim_initialize(PredId, DeadInfo0, DeadInfo) :-
    some [!Queue, !NeededNames] (
        DeadInfo0 = pred_elim_info(ModuleInfo, !:Queue, Examined, NeededIds,
            !:NeededNames),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ( if
            PredModule = pred_info_module(PredInfo),
            PredName = pred_info_name(PredInfo),
            PredArity = pred_info_orig_arity(PredInfo),
            (
                % Don't eliminate special preds since they won't be actually
                % called from the HLDS until after polymorphism.
                is_unify_index_or_compare_pred(PredInfo)
            ;
                % Don't eliminate preds from builtin modules, since later
                % passes of the compiler may introduce calls to them
                % (e.g. polymorphism.m needs unify/2 and friends).
                % XXX This is too broad. The later disjuncts here try to do
                % a much more precise job.
                any_mercury_builtin_module(PredModule)
            ;
                % Simplify can't introduce calls to this predicate or function
                % if we eliminate it here.
                is_std_lib_module_name(PredModule, PredModuleName),
                (
                    simplify_may_introduce_calls(PredModuleName, PredName,
                        PredArity)
                ;
                    daio_may_introduce_calls(PredModuleName, PredName,
                        PredArity)
                )
            ;
                % Try-goal expansion may introduce calls to predicates in
                % `exception'.
                % XXX it should actually be calling predicates in a new
                % exception_builtin module, which would obviate the need for
                % this check
                PredModule = mercury_exception_module,
                try_expand_may_introduce_calls(PredName, PredArity)
            ;
                % Don't attempt to eliminate local preds here, since we want
                % to do semantic checking on those even if they aren't used.
                not pred_info_is_imported(PredInfo),
                pred_info_get_status(PredInfo, PredStatus),
                PredStatus \= pred_status(status_opt_imported)
            ;
                % Don't eliminate predicates declared in this module with a
                % `:- pragma external_{pred/func}'.
                module_info_get_name(ModuleInfo, PredModule)
            ;
                % Don't eliminate <foo>_init_any/1 predicates; modes.m may
                % insert calls to them to initialize variables from inst `free'
                % to inst `any'.
                string.remove_suffix(PredName, "_init_any", _),
                PredArity = 1
            ;
                % Don't eliminate the clauses for promises.
                pred_info_is_promise(PredInfo, _)
            )
        then
            set_tree234.insert(qualified(PredModule, PredName), !NeededNames),
            queue.put(PredId, !Queue)
        else
            true
        ),
        DeadInfo = pred_elim_info(ModuleInfo, !.Queue, Examined, NeededIds,
            !.NeededNames)
    ).

:- pred dead_pred_elim_analyze(pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_elim_analyze(!DeadInfo) :-
    some [!Queue, !Examined, !Needed] (
        !.DeadInfo = pred_elim_info(ModuleInfo, !:Queue, !:Examined,
            !:Needed, NeededNames),
        ( if queue.get(PredId, !Queue) then
            ( if set_tree234.contains(!.Examined, PredId) then
                !:DeadInfo = pred_elim_info(ModuleInfo, !.Queue, !.Examined,
                    !.Needed, NeededNames)
            else
                set_tree234.insert(PredId, !Needed),
                set_tree234.insert(PredId, !Examined),
                !:DeadInfo = pred_elim_info(ModuleInfo, !.Queue, !.Examined,
                    !.Needed, NeededNames),
                module_info_pred_info(ModuleInfo, PredId, PredInfo),
                pred_info_get_clauses_info(PredInfo, ClausesInfo),
                clauses_info_get_clauses_rep(ClausesInfo, ClausesRep,
                    _ItemNumbers),
                get_clause_list_maybe_repeated(ClausesRep, Clauses),
                list.foldl(dead_pred_elim_process_clause, Clauses, !DeadInfo)
            ),
            disable_warning [suspicious_recursion] (
                dead_pred_elim_analyze(!DeadInfo)
            )
        else
            true
        )
    ).

:- pred dead_pred_elim_process_clause(clause::in,
    pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_elim_process_clause(Clause, !DeadInfo) :-
    pre_modecheck_examine_goal(Clause ^ clause_body, !DeadInfo).

:- pred pre_modecheck_examine_case(case::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_case(Case, !DeadInfo) :-
    Case = case(_, _, Goal),
    pre_modecheck_examine_goal(Goal, !DeadInfo).

:- pred pre_modecheck_examine_goal(hlds_goal::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_goal(Goal, !DeadInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    pre_modecheck_examine_goal_expr(GoalExpr, !DeadInfo).

:- pred pre_modecheck_examine_goal_expr(hlds_goal_expr::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_goal_expr(GoalExpr, !DeadInfo) :-
    (
        GoalExpr = conj(_ConjType, Goals),
        list.foldl(pre_modecheck_examine_goal, Goals, !DeadInfo)
    ;
        GoalExpr = disj(Goals),
        list.foldl(pre_modecheck_examine_goal, Goals, !DeadInfo)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        pre_modecheck_examine_goal(Cond, !DeadInfo),
        pre_modecheck_examine_goal(Then, !DeadInfo),
        pre_modecheck_examine_goal(Else, !DeadInfo)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl(pre_modecheck_examine_case, Cases, !DeadInfo)
    ;
        GoalExpr = negation(SubGoal),
        pre_modecheck_examine_goal(SubGoal, !DeadInfo)
    ;
        GoalExpr = scope(_, SubGoal),
        % The invariants that would allow us to optimize
        % from_ground_term_construct scopes haven't been established yet,
        % which is why we must always scan SubGoal.
        pre_modecheck_examine_goal(SubGoal, !DeadInfo)
    ;
        GoalExpr = plain_call(_, _, _, _, _, PredName),
        dead_pred_info_add_pred_name(PredName, !DeadInfo)
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
    ;
        GoalExpr = unify(_, RHS, _, _, _),
        pre_modecheck_examine_unify_rhs(RHS, !DeadInfo)
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_GoalType, _Outer, _Inner,
                _MaybeOutputVars, MainGoal, OrElseGoals, _OrElseInners),
            pre_modecheck_examine_goal(MainGoal, !DeadInfo),
            list.foldl(pre_modecheck_examine_goal, OrElseGoals, !DeadInfo)
        ;
            ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
            pre_modecheck_examine_goal(SubGoal, !DeadInfo)
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "unexpected bi_implication")
        )
    ).

:- pred pre_modecheck_examine_unify_rhs(unify_rhs::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_unify_rhs(RHS, !DeadInfo) :-
    (
        RHS = rhs_var(_)
    ;
        RHS = rhs_functor(Functor, _, _),
        ( if Functor = cons(Name, _, _) then
            dead_pred_info_add_pred_name(Name, !DeadInfo)
        else
            true
        )
    ;
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, Goal),
        pre_modecheck_examine_goal(Goal, !DeadInfo)
    ).

:- pred dead_pred_info_add_pred_name(sym_name::in,
    pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_info_add_pred_name(Name, !DeadInfo) :-
    some [!Queue, !NeededNames] (
        !.DeadInfo = pred_elim_info(ModuleInfo, !:Queue, Examined,
            Needed, !:NeededNames),
        ( if set_tree234.contains(!.NeededNames, Name) then
            true
        else
            module_info_get_predicate_table(ModuleInfo, PredicateTable),
            set_tree234.insert(Name, !NeededNames),
            predicate_table_lookup_sym(PredicateTable,
                may_be_partially_qualified, Name, PredIds),
            queue.put_list(PredIds, !Queue),
            !:DeadInfo = pred_elim_info(ModuleInfo, !.Queue, Examined,
                Needed, !.NeededNames)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.dead_proc_elim.
%-----------------------------------------------------------------------------%
