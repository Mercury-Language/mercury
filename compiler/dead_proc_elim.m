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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module set_ctree234.

%-----------------------------------------------------------------------------%

:- type needed_map == map(entity, maybe_needed).

:- type entity
    --->    entity_proc(pred_proc_id)
    ;       entity_table_struct(pred_proc_id)
    ;       entity_type_ctor(module_name, string, int)
    ;       entity_const_struct(int)
    ;       entity_mutable(module_name, string, mutable_pred_kind).

:- type maybe_needed
    --->    not_eliminable(set_ctree234(needed_reason))
    ;       maybe_eliminable(num_references :: int).

:- type needed_reason
    --->    nr_exported
    ;       nr_foreign_exported
    ;       nr_initial_final
    ;       nr_type_ctor
    ;       nr_const_struct
    ;       nr_class_interface_proc
    ;       nr_used_by(entity).

    % Analyze which entities are needed, and for those entities which are
    % needed, record how many times they are referenced (this information
    % is used by our inlining heuristics).
    %
:- pred dead_proc_analyze(module_info::in, needed_map::out) is det.

%-----------------------------------------------------------------------------%

:- type maybe_elim_opt_imported
    --->    elim_opt_imported
    ;       do_not_elim_opt_imported.

    % Values of this type map each predicate that dead_proc_elim has modified
    % to information about what was deleted from it.
:- type eliminated_map == map(pred_id, eliminated_what).

:- type eliminated_what
    --->    elim_whole_pred(eliminated_how)
            % We have eliminated, in the manner given by the argument,
            % all the procedures of this predicate.
    ;       elim_procs(set(proc_id)).
            % We have eliminated a strict subset of the procedures of
            % this predicate. The manner is implicitly elim_deleted_proc.

:- type eliminated_how
    --->    elim_deleted_proc
            % We have deleted the procedure from its pred_info.
            % Note that we never delete the pred_info from the predicate table
            % (and its pred_id from the set of valid pred_ids) even if
            % all its procedures get deleted. A trial which did so found that
            % a bootcheck *almost* works, with the exception being that
            % the tests in the recompilation directory fail. Apparently,
            % smart recompilation needs the pred_infos of otherwise-deleted
            % predicates.
    ;       elim_deleted_goal.
            % We have kept the procedure, but replaced its body with "true"
            % to reduce its memory consumption.
            %
            % We do this for opt_imported predicates that we will not generate
            % code for (the module we are importing them from will do that),
            % but for which we cannot delete the procedure itself, because
            % live code still contains calls (or other references) to it.
            % Typically, the pre-code-generation simplification pass,
            % when operating on other procedures which have such references
            % to this procedure, will access this procedure's proc_info,
            % as part of e.g. recomputing the instmap delta of a call,
            % or finding out whether the call can loop forever or throw
            % an exception.

    % Eliminate dead procedures and/or constant structures.
    % If the first argument is `elim_opt_imported', also eliminate
    % any opt_imported procedures.
    %
:- pred dead_proc_elim(maybe_elim_opt_imported::in, needed_map::out,
    eliminated_map::out, module_info::in, module_info::out) is det.

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

:- pred output_needed_map(io.text_output_stream::in, module_info::in,
    needed_map::in, io::di, io::uo) is det.

:- pred output_elimination_msgs(io.text_output_stream::in, module_info::in,
    eliminated_map::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.introduced_call_table.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_name.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.      % undesirable dependency

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module queue.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%-----------------------------------------------------------------------------%

dead_proc_analyze(ModuleInfo, Needed) :-
    AnalyzeLinks = analyze_links(do_not_analyze_link_deleted_calls,
        do_not_analyze_link_type_ctor, do_not_analyze_link_const_struct),
    do_dead_proc_analyze(ModuleInfo, AnalyzeLinks, Needed).

%-----------------------------------------------------------------------------%

dead_proc_elim(ElimOptImported, Needed, ElimMap, !ModuleInfo) :-
    AnalyzeLinks = analyze_links(do_not_analyze_link_deleted_calls,
        analyze_link_type_ctor, analyze_link_const_struct),
    do_dead_proc_analyze(!.ModuleInfo, AnalyzeLinks, Needed),
    do_dead_proc_eliminate(ElimOptImported, Needed, ElimMap, !ModuleInfo).

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
    dead_proc_examine(ModuleInfo, AnalyzeLinks, Queue0, Examined0, !Needed).

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
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    dead_proc_initialize_preds(PredIdTable, PredIds, !Queue, !Needed),

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
:- pred dead_proc_initialize_preds(pred_id_table::in, list(pred_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_preds(_PredTable, [], !Queue, !Needed).
dead_proc_initialize_preds(PredTable, [PredId | PredIds], !Queue, !Needed) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_markers(PredInfo, PredMarkers),
    ( if check_marker(PredMarkers, marker_consider_used) then
        LiveProcIds = pred_info_all_procids(PredInfo)
    else
        LiveProcIds = pred_info_all_exported_procids(PredInfo)
    ),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, Procs),
    dead_proc_initialize_procs(PredId, Procs, LiveProcIds, !Queue, !Needed),
    dead_proc_initialize_preds(PredTable, PredIds, !Queue, !Needed).

    % Add the listed procedures to the queue and map.
    %
:- pred dead_proc_initialize_procs(pred_id::in,
    assoc_list(proc_id, proc_info)::in, list(proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_procs(_PredId, [], _LiveProcIds, !Queue, !Needed).
dead_proc_initialize_procs(PredId, [ProcIdInfo | ProcIdInfos], LiveProcIds,
        !Queue, !Needed) :-
    ProcIdInfo = ProcId - ProcInfo,
    ( if
        (
            list.member(ProcId, LiveProcIds)
        ;
            proc_info_get_has_any_foreign_exports(ProcInfo,
                has_foreign_exports)
        )
    then
        Entity = entity_proc(proc(PredId, ProcId)),
        queue.put(Entity, !Queue),
        record_entity_is_needed(Entity, nr_exported, !Needed)
    else
        true
    ),
    dead_proc_initialize_procs(PredId, ProcIdInfos, LiveProcIds,
        !Queue, !Needed).

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
    Entity = entity_proc(proc(PredId, ProcId)),
    queue.put(Entity, !Queue),
    record_entity_is_needed(Entity, nr_foreign_exported, !Needed),
    dead_proc_initialize_pragma_exports(PragmaProcs, !Queue, !Needed).

    % Add module initialisation/finalisation procedures to the queue and map
    % as they cannot be removed.
    %
:- pred dead_proc_initialize_init_fn_procs(list(pred_proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_init_fn_procs([], !Queue, !Needed).
dead_proc_initialize_init_fn_procs([PPId | PPIds], !Queue, !Needed) :-
    Entity = entity_proc(PPId),
    queue.put(Entity, !Queue),
    record_entity_is_needed(Entity, nr_initial_final, !Needed),
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
        record_entity_is_needed(Entity, nr_type_ctor, !Needed)
    else
        true
    ),
    dead_proc_initialize_type_ctor_infos(TypeCtorGenInfos, !Queue, !Needed).

:- pred dead_proc_initialize_class_methods(class_table::in, instance_table::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_class_methods(Classes, Instances, !Queue, !Needed) :-
    map.values(Instances, InstanceDefnsLists),
    list.condense(InstanceDefnsLists, InstanceDefns),
    list.foldl2(get_instance_pred_procs, InstanceDefns, !Queue, !Needed),
    map.values(Classes, ClassDefns),
    list.foldl2(get_class_pred_procs, ClassDefns, !Queue, !Needed).

:- pred get_instance_pred_procs(hlds_instance_defn::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_instance_pred_procs(Instance, !Queue, !Needed) :-
    MaybeMethodInfos = Instance ^ instdefn_maybe_method_infos,
    % We need to keep the instance methods for all instances
    % for optimization of method lookups.
    (
        % This should never happen.
        MaybeMethodInfos = no
    ;
        MaybeMethodInfos = yes(MethodInfos),
        MethodPredProcIds = method_infos_to_pred_proc_ids(MethodInfos),
        list.foldl2(get_class_interface_pred_proc, MethodPredProcIds,
            !Queue, !Needed)
    ).

:- pred get_class_pred_procs(hlds_class_defn::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_class_pred_procs(Class, !Queue, !Needed) :-
    MethodInfos = Class ^ classdefn_method_infos,
    MethodPredProcIds = method_infos_to_pred_proc_ids(MethodInfos),
    list.foldl2(get_class_interface_pred_proc, MethodPredProcIds,
        !Queue, !Needed).

:- pred get_class_interface_pred_proc(pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_class_interface_pred_proc(ClassProc, !Queue, !Needed) :-
    Entity = entity_proc(ClassProc),
    queue.put(Entity, !Queue),
    % NOTE If the typeclass is exported, then marking all the predicates
    % implementing its methods is obviously correct. If the typeclass
    % is private to the current module, then it, or some of its methods
    % *could* be unused, but
    %
    % - that is *very* unlikely; and
    % - due to this fact, we have not written code that could prove
    %   that a private class or one of its methods is unused.
    record_entity_is_needed(Entity, nr_class_interface_proc, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine(module_info::in, analyze_links::in,
    entity_queue::in, examined_set::in, needed_map::in, needed_map::out)
    is det.

dead_proc_examine(ModuleInfo, AnalyzeLinks, !.Queue, !.Examined, !Needed) :-
    % See if the queue is empty.
    ( if queue.get(Entity, !Queue) then
        % See if the next element has been examined before.
        ( if set_tree234.insert_new(Entity, !Examined) then
            % No, this is the first time we are looking at Entity.
            (
                Entity = entity_proc(PredProcId),
                AnalyzeTraceGoalProcs = AnalyzeLinks ^ al_deleted_calls,
                dead_proc_examine_proc(ModuleInfo, AnalyzeTraceGoalProcs,
                    PredProcId, !Queue, !Needed)
            ;
                ( Entity = entity_table_struct(_PredProcId)
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
                    dead_proc_examine_type_ctor_info(ModuleInfo,
                        Module, Type, Arity, !Queue, !Needed)
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
            % We have processed Entity previously.
            true
        ),
        disable_warning [suspicious_recursion] (
            dead_proc_examine(ModuleInfo, AnalyzeLinks, !.Queue, !.Examined,
                !Needed)
        )
    else
        true
    ).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_type_ctor_info(module_info::in,
    module_name::in, string::in, arity::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_type_ctor_info(ModuleInfo, ModuleName, TypeName, Arity,
        !Queue, !Needed) :-
    module_info_get_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
    ( if
        find_type_ctor_info(ModuleName, TypeName, Arity, TypeCtorGenInfos,
            Refs)
    then
        dead_proc_examine_type_ctor_info_refs(Refs, !Queue, !Needed)
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

:- pred dead_proc_examine_type_ctor_info_refs(list(pred_proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_type_ctor_info_refs([], !Queue, !Needed).
dead_proc_examine_type_ctor_info_refs([PPId | PPIds], !Queue, !Needed) :-
    Entity = entity_proc(PPId),
    queue.put(Entity, !Queue),
    record_entity_is_needed(Entity, nr_type_ctor, !Needed),
    dead_proc_examine_type_ctor_info_refs(PPIds, !Queue, !Needed).

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
        record_entity_is_needed(Entity, nr_type_ctor, !Needed)
    else
        true
    ),
    dead_proc_examine_const_struct_args(Args, !Queue, !Needed).

:- pred dead_proc_examine_const_struct_args(list(const_struct_arg)::in,
    entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_const_struct_args([], !Queue, !Needed).
dead_proc_examine_const_struct_args([Arg | Args], !Queue, !Needed) :-
    (
        Arg = csa_const_struct(ConstNum),
        Entity = entity_const_struct(ConstNum),
        queue.put(Entity, !Queue),
        record_entity_is_needed(Entity, nr_const_struct, !Needed)
    ;
        Arg = csa_constant(ConsId, _),
        ( if ConsId = type_ctor_info_const(Module, TypeName, Arity) then
            Entity = entity_type_ctor(Module, TypeName, Arity),
            queue.put(Entity, !Queue),
            record_entity_is_needed(Entity, nr_type_ctor, !Needed)
        else
            true
        )
    ),
    dead_proc_examine_const_struct_args(Args, !Queue, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_proc(module_info::in,
    maybe_analyze_link_deleted_calls::in, pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_proc(ModuleInfo, AnalyzeDeletedCalls, PredProcId,
        !Queue, !Needed) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_non_imported_procids(PredInfo),
    ( if
        list.member(ProcId, ProcIds),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo)
    then
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream, "examining proc %d %d\n",
                [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))], !IO)
        ),
        proc_info_get_eval_method(ProcInfo, EvalMethod),
        NeededReason = nr_used_by(entity_proc(PredProcId)),

        proc_info_get_goal(ProcInfo, Goal),
        dead_proc_examine_goal(ModuleInfo, PredProcId, Goal,
            !Queue, !Needed),
        (
            AnalyzeDeletedCalls = do_not_analyze_link_deleted_calls
        ;
            AnalyzeDeletedCalls = analyze_link_deleted_calls,
            proc_info_get_deleted_call_callees(ProcInfo, DeletedCallCallees),
            set.foldl2(need_trace_goal_proc(NeededReason), DeletedCallCallees,
                !Queue, !Needed)
        ),

        ( if
            EvalMethod = eval_tabled(TabledMethod),
            tabled_eval_method_has_per_proc_tabling_pointer(TabledMethod) = yes
        then
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.format(DebugStream, "need table struct for proc %d %d\n",
                    [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                    !IO)
            ),
            TableStructEntity = entity_table_struct(PredProcId),
            record_entity_is_needed(TableStructEntity, NeededReason, !Needed)
        else
            true
        ),
        pred_info_get_origin(PredInfo, Origin),
        ( if
            Origin = origin_compiler(
                made_for_mutable(ModuleName, MutableName, PredKind))
        then
            % XXX Why do we record this, when we do not ever delete
            % unused mutables?
            MutableEntity = entity_mutable(ModuleName, MutableName, PredKind),
            record_entity_is_needed(MutableEntity, NeededReason, !Needed)
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

:- pred need_trace_goal_proc(needed_reason::in, pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

need_trace_goal_proc(NeededReason, TraceGoalPPId, !Queue, !Needed) :-
    Entity = entity_proc(TraceGoalPPId),
    % If we are following links that correspond to eliminated calls,
    % then aren't actually trying to eliminate procedures, so whether
    % we record the callee of that eliminated call as eliminable or not
    % does not matter.
    record_entity_is_needed(Entity, NeededReason, !Needed),
    queue.put(Entity, !Queue).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_goals(module_info::in, pred_proc_id::in,
    list(hlds_goal)::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_goals(_, _, [], !Queue, !Needed).
dead_proc_examine_goals(ModuleInfo, CurPredProcId, [Goal | Goals],
        !Queue, !Needed) :-
    dead_proc_examine_goal(ModuleInfo, CurPredProcId, Goal, !Queue, !Needed),
    dead_proc_examine_goals(ModuleInfo, CurPredProcId, Goals, !Queue, !Needed).

:- pred dead_proc_examine_cases(module_info::in, pred_proc_id::in,
    list(case)::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_cases(_, _, [], !Queue, !Needed).
dead_proc_examine_cases(ModuleInfo, CurPredProcId, [case(_, _, Goal) | Cases],
        !Queue, !Needed) :-
    dead_proc_examine_goal(ModuleInfo, CurPredProcId, Goal, !Queue, !Needed),
    dead_proc_examine_cases(ModuleInfo, CurPredProcId, Cases, !Queue, !Needed).

:- pred dead_proc_examine_goal(module_info::in, pred_proc_id::in,
    hlds_goal::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_goal(ModuleInfo, CurPredProcId, Goal, !Queue, !Needed) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        dead_proc_examine_goals(ModuleInfo, CurPredProcId, Goals,
            !Queue, !Needed)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        dead_proc_examine_cases(ModuleInfo, CurPredProcId, Cases,
            !Queue, !Needed)
    ;
        GoalExpr = negation(SubGoal),
        dead_proc_examine_goal(ModuleInfo, CurPredProcId, SubGoal,
            !Queue, !Needed)
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
            dead_proc_examine_goal(ModuleInfo, CurPredProcId, SubGoal,
                !Queue, !Needed)
        )
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        dead_proc_examine_goal(ModuleInfo, CurPredProcId, Cond,
            !Queue, !Needed),
        dead_proc_examine_goal(ModuleInfo, CurPredProcId, Then,
            !Queue, !Needed),
        dead_proc_examine_goal(ModuleInfo, CurPredProcId, Else,
            !Queue, !Needed)
    ;
        GoalExpr = generic_call(_, _, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _,_,_,_),
        CalleePredProcId = proc(PredId, ProcId),
        CalleeEntity = entity_proc(CalleePredProcId),
        queue.put(CalleeEntity, !Queue),
        ( if CalleePredProcId = CurPredProcId then
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.format(DebugStream, "plain_call recursive %d %d\n",
                    [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                    !IO)
            ),
            % If it is reachable and recursive, then we cannot eliminate it
            % or inline it.
            CurProcEntity = entity_proc(CurPredProcId),
            record_entity_is_used(CalleeEntity, CurProcEntity, !Needed)
        else if map.search(!.Needed, CalleeEntity, OldNotation) then
            (
                OldNotation = not_eliminable(_),
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
                map.det_update(CalleeEntity, NewNotation, !Needed),
                trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                    get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                    io.format(DebugStream,
                        "plain_call incr maybe_eliminable %d %d\n",
                        [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                        !IO)
                )
            )
        else
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                io.format(DebugStream,
                    "plain_call init maybe_eliminable %d %d\n",
                    [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))],
                    !IO)
            ),
            NewNotation = maybe_eliminable(1),
            map.det_insert(CalleeEntity, NewNotation, !Needed)
        )
    ;
        GoalExpr = call_foreign_proc(_, PredId, ProcId, _, _, _, _),
        CalleeEntity = entity_proc(proc(PredId, ProcId)),
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.format(DebugStream, "foreign_proc %d %d\n",
                [i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId))], !IO)
        ),
        queue.put(CalleeEntity, !Queue),
        CurProcEntity = entity_proc(CurPredProcId),
        record_entity_is_used(CalleeEntity, CurProcEntity, !Needed)
    ;
        GoalExpr = unify(_LHS, _RHS, _UniModes, Unification, _UnifyContext),
        (
            Unification = construct(_, ConsId, _, _, _, _, _),
            (
                (
                    ConsId = closure_cons(ShroudedPredProcId, _),
                    proc(PredId, ProcId) =
                        unshroud_pred_proc_id(ShroudedPredProcId),
                    Entity = entity_proc(proc(PredId, ProcId)),
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
                    Entity = entity_table_struct(proc(PredId, ProcId)),
                    trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                        get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                        io.format(DebugStream, "table struct const %d %d\n",
                            [i(pred_id_to_int(PredId)),
                            i(proc_id_to_int(ProcId))], !IO)
                    )
                ),
                queue.put(Entity, !Queue),
                CurProcEntity = entity_proc(CurPredProcId),
                record_entity_is_used(Entity, CurProcEntity, !Needed)
            ;
                ( ConsId = type_info_const(ConstNum)
                ; ConsId = typeclass_info_const(ConstNum)
                ; ConsId = ground_term_const(ConstNum, _)
                ),
                Entity = entity_const_struct(ConstNum),
                queue.put(Entity, !Queue),
                CurProcEntity = entity_proc(CurPredProcId),
                record_entity_is_used(Entity, CurProcEntity, !Needed)
            ;
                ( ConsId = cons(_, _, _)
                ; ConsId = tuple_cons(_)
                ; ConsId = some_int_const(_)
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

:- pred record_entity_is_used(entity::in, entity::in,
    needed_map::in, needed_map::out) is det.

record_entity_is_used(Entity, User, !Needed) :-
    record_entity_is_needed(Entity, nr_used_by(User), !Needed).

:- pred record_entity_is_needed(entity::in, needed_reason::in,
    needed_map::in, needed_map::out) is det.

record_entity_is_needed(Entity, NeededReason, !Needed) :-
    ( if map.search(!.Needed, Entity, MaybeEliminable0) then
        (
            MaybeEliminable0 = maybe_eliminable(_NumRefs),
            NeededReasons = set_ctree234.make_singleton_set(NeededReason),
            map.det_update(Entity, not_eliminable(NeededReasons), !Needed)
        ;
            MaybeEliminable0 = not_eliminable(NeededReasons0),
            NumReasons0 = count(NeededReasons0),
            ( if NumReasons0 < num_reasons_to_keep then
                set_ctree234.insert(NeededReason,
                    NeededReasons0, NeededReasons),
                map.det_update(Entity, not_eliminable(NeededReasons), !Needed)
            else
                % We have enough reasons; don't pile on any others.
                true
            )
        )
    else
        NeededReasons = set_ctree234.make_singleton_set(NeededReason),
        map.det_insert(Entity, not_eliminable(NeededReasons), !Needed)
    ).

:- func num_reasons_to_keep = int.

num_reasons_to_keep = 3.

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
                proc_elim_pred_id_table :: pred_id_table,

                % The set of predicates and/or procedures that we have
                % - either deleted from the predicate table,
                % - or whose body we have replaced with "true".
                proc_elim_map           :: eliminated_map
            ).

    % Given the information about which entities are needed,
    % eliminate entities which are *not* needed.
    %
:- pred do_dead_proc_eliminate(maybe_elim_opt_imported::in, needed_map::in,
    eliminated_map::out, module_info::in, module_info::out) is det.

do_dead_proc_eliminate(ElimOptImported, !.Needed, !:ElimMap, !ModuleInfo) :-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    ProcElimInfo0 = proc_elim_info(!.Needed, !.ModuleInfo, PredIdTable0,
        map.init),
    list.foldl(dead_proc_eliminate_pred(ElimOptImported), PredIds,
        ProcElimInfo0, ProcElimInfo),
    ProcElimInfo = proc_elim_info(!:Needed, !:ModuleInfo, PredIdTable,
        !:ElimMap),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo),

    module_info_get_type_ctor_gen_infos(!.ModuleInfo, TypeCtorGenInfos0),
    dead_proc_eliminate_type_ctor_infos(TypeCtorGenInfos0, !.Needed,
        TypeCtorGenInfos),
    module_info_set_type_ctor_gen_infos(TypeCtorGenInfos, !ModuleInfo),

    % We could also eliminate table structs, but we don't do that yet,
    % because some references to such structs are currently visible only
    % in C code embedded in compiler-generated foreign_procs, and
    % therefore we might accidentally create dangling references.

    module_info_get_const_struct_db(!.ModuleInfo, ConstStructDb0),
    const_struct_db_get_structs(ConstStructDb0, ConstNumStructs0),
    dead_proc_eliminate_const_structs(ConstNumStructs0, !.Needed,
        ConstStructDb0, ConstStructDb),
    module_info_set_const_struct_db(ConstStructDb, !ModuleInfo),

    ( if map.is_empty(!.ElimMap) then
        true
    else
        % The dependency graph will still contain references to the eliminated
        % procedures, so it must be rebuilt if it will be used later.
        module_info_clobber_dependency_info(!ModuleInfo)
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
    !.ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable0, ElimMap0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_status(PredInfo0, PredStatus),
    ( if
        % Find out if the predicate is defined in this module.
        % If yes, find out also whether any of its procedures must be kept.
        (
            PredStatus = pred_status(status_local),
            KeepAnyProc = no
        ;
            PredStatus = pred_status(status_pseudo_imported),
            KeepAnyProc = no
        ;
            PredStatus = pred_status(status_pseudo_exported),
            hlds_pred.in_in_unification_proc_id(InitProcId),
            KeepAnyProc = yes(InitProcId)
        )
    then
        ProcIds = pred_info_all_procids(PredInfo0),
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        list.foldl2(
            dead_proc_eliminate_proc(!.ProcElimInfo, KeepAnyProc, PredId),
            ProcIds, ProcTable0, ProcTable, set.init, DeletedProcIds),
        ( if set.is_empty(DeletedProcIds) then
            % There is no change.
            true
        else
            ( if map.is_empty(ProcTable) then
                ElimWhat = elim_whole_pred(elim_deleted_proc)
            else
                ElimWhat = elim_procs(DeletedProcIds)
            ),
            map.det_insert(PredId, ElimWhat, ElimMap0, ElimMap),
            pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredId, PredInfo, PredTable0, PredTable),
            !:ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable,
                ElimMap)
        )
    else if
        % Don't generate code in the current module for unoptimized
        % opt_imported preds (that is, for opt_imported preds which we have not
        % by this point managed to inline or specialize; this code should be
        % called with `ElimOptImported = elim_opt_imported' only after inlining
        % and specialization is complete).
        ElimOptImported = elim_opt_imported,
        PredStatus = pred_status(status_opt_imported)
    then
        ProcIds = pred_info_all_procids(PredInfo0),
        pred_info_get_proc_table(PredInfo0, ProcTable0),

        % Reduce memory usage by replacing the goals with "true".
        DestroyGoal =
            ( pred(Id::in, PTable0::in, PTable::out) is det :-
                map.lookup(ProcTable0, Id, ProcInfo0),
                proc_info_set_goal(true_goal, ProcInfo0, ProcInfo),
                map.det_update(Id, ProcInfo, PTable0, PTable)
            ),
        ElimWhat = elim_whole_pred(elim_deleted_goal),
        map.det_insert(PredId, ElimWhat, ElimMap0, ElimMap),
        list.foldl(DestroyGoal, ProcIds, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo1),
        pred_info_set_status(
            pred_status(status_imported(import_locn_interface)),
            PredInfo1, PredInfo),
        map.det_update(PredId, PredInfo, PredTable0, PredTable),
        !:ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable, ElimMap)
    else
        % This predicate is not defined in this module.
        true
    ).

    % Eliminate a procedure, if unused.
    %
:- pred dead_proc_eliminate_proc(proc_elim_info::in, maybe(proc_id)::in,
    pred_id::in, proc_id::in, proc_table::in, proc_table::out,
    set(proc_id)::in, set(proc_id)::out) is det.

dead_proc_eliminate_proc(ProcElimInfo, KeepAnyProc, PredId, ProcId,
        !ProcTable, !DeletedProcIds) :-
    Needed = ProcElimInfo ^ proc_elim_needed_map,
    ( if
        (
            % Keep the procedure if it is in the needed map.
            %
            % If its associated value is not_eliminable, the reason for this
            % is obvious.
            %
            % If its associated value is maybe_eliminable, the reason for this
            % is that the procedure *could* be eliminated if we inlined
            % all calls to it, and we count how many such calls there are
            % to "encourage" inlining to inline at such call sites. However,
            % if there is such an entry in Needed, then either inlining
            % hasn't beeen run yet, or it has been run but it left some
            % calls to this procedure. Neither case allows the deletion
            % of this procedure.
            map.contains(Needed, entity_proc(proc(PredId, ProcId)))
        ;
            % Or if it is to be kept because it is exported.
            KeepAnyProc = yes(ProcId)
        )
    then
        true
    else
        map.delete(ProcId, !ProcTable),
        set.insert(ProcId, !DeletedProcIds)
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
    module_info_get_pred_id_table(ModuleInfo, PredIdTable),
    module_info_get_globals(ModuleInfo, Globals),
    % We get called only if either warn_dead_procs or warn_dead_preds is set.
    % We warn about procedures of entirely dead predicates if either is set
    % (or if both are set); the only difference is whether we warn about
    % procedures with live siblings.
    globals.lookup_bool_option(Globals, warn_dead_procs, WarnWithLiveSiblings),
    list.foldl(
        dead_proc_warn_pred(ModuleInfo, PredIdTable, WarnWithLiveSiblings,
            Needed),
        PredIds, [], Specs).

    % Warn about any unused procedures for this pred.
    %
:- pred dead_proc_warn_pred(module_info::in, pred_id_table::in, bool::in,
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
        ProcIds = pred_info_all_procids(PredInfo),
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
            map.contains(Needed, entity_proc(proc(PredId, ProcId)))
        ;
            WarnWithLiveSiblings = no,
            some [OtherProcId] (
                list.member(OtherProcId, AllProcsInPred),
                map.contains(Needed, entity_proc(proc(PredId, OtherProcId)))
            )
        ;
            pred_info_get_origin(PredInfo, Origin),
            Origin = origin_compiler(made_for_mutable(MutableModuleName,
                MutableName, PredKind)),
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

                % preds to examine.
                pred_elim_queue         :: queue(pred_id),

                % preds examined.
                pred_elim_examined      :: set_tree234(pred_id),

                % needed pred_ids.
                pred_elim_needed_ids    :: set_tree234(pred_id),

                % pred names needed.
                pred_elim_needed_named  :: set_tree234(sym_name)
            ).

dead_pred_elim(!ModuleInfo) :-
    map.init(NeededMap0),
    module_info_get_pragma_exported_procs(!.ModuleInfo, PragmaExports),
    dead_proc_initialize_pragma_exports(cord.list(PragmaExports),
        queue.init, _, NeededMap0, NeededMap1),

    % The goals for the class method procs need to be examined because
    % they contain calls to the actual method implementations.

    module_info_get_instance_table(!.ModuleInfo, Instances),
    module_info_get_class_table(!.ModuleInfo, Classes),
    dead_proc_initialize_class_methods(Classes, Instances,
        queue.init, _, NeededMap1, NeededMap),
    % All entities in NeededMap should be mapped to not_eliminable.
    map.keys(NeededMap, NeededEntities),
    NeededPreds0 = set_tree234.init,
    list.foldl2(dead_pred_elim_add_entity, NeededEntities,
        queue.init, Queue, NeededPreds0, NeededPreds1),

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
    NeededPredList3 = set_tree234.to_sorted_list(NeededPreds3),
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
        TypeDefnBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_Ctors, _MaybeSuperType, MaybeCanon, _Repn,
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
                UniPredSymName, user_arity(2), UniPredIds),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                CmpPredSymName, user_arity(3), CmpPredIds),
            set_tree234.insert_list(UniPredIds, !NeededPreds),
            set_tree234.insert_list(CmpPredIds, !NeededPreds)
        ;
            NonCanonical = noncanon_uni_only(UniPredSymName),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                UniPredSymName, user_arity(2), UniPredIds),
            set_tree234.insert_list(UniPredIds, !NeededPreds)
        ;
            NonCanonical = noncanon_cmp_only(CmpPredSymName),
            predicate_table_lookup_sym_arity(PredTable, is_fully_qualified,
                CmpPredSymName, user_arity(3), CmpPredIds),
            set_tree234.insert_list(CmpPredIds, !NeededPreds)
        ;
            NonCanonical = noncanon_abstract(_IsSolverType)
        ;
            NonCanonical = noncanon_subtype
        )
    ).

:- pred dead_pred_elim_add_entity(entity::in, queue(pred_id)::in,
    queue(pred_id)::out, set_tree234(pred_id)::in, set_tree234(pred_id)::out)
    is det.

dead_pred_elim_add_entity(Entity, !Queue, !Preds) :-
    (
        Entity = entity_proc(proc(PredId, _)),
        queue.put(PredId, !Queue),
        set_tree234.insert(PredId, !Preds)
    ;
        ( Entity = entity_table_struct(_)
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
            PredModuleName = pred_info_module(PredInfo),
            PredName = pred_info_name(PredInfo),
            (
                % Don't eliminate unify or compare predicates, for two reasons.
                %
                % First, because higher_order.m may introduce calls to them
                % when it specializes calls to builtin.unify or builtin.compare
                % respectively in situations when it knows the types of the
                % values to be unified or compared. Until that pass has been
                % run, we don't know which otherwise-dead unify or compare
                % predicates it will "bring back to life".
                %
                % Second, we don't want to delete even a never-called
                % unification or compare predicate while that predicate
                % has a pointer to it from its type_ctor's type_ctor_info
                % structure, and until this writing (2022 may 13), dead
                % procedure elimination has never had code to eliminate
                % unused type_ctor_info structures. The reason for that is
                %
                % - many places can refer to type_ctor_infos, so proving that
                %   one is unused requires significant programming effort, but
                %
                % - virtually all type_ctor_infos that get created are actually
                %   used, so that effort, and the time required to run the
                %   resulting code, would be wasted in the vast majority of
                %   compilations.
                % 
                is_unify_index_or_compare_pred(PredInfo)
            ;
                is_std_lib_module_name(PredModuleName, PredModuleNameStr),
                % Don't eliminate preds from standard library modules
                % if later passes of the compiler may introduce calls to them.
                PredOrFunc = pred_info_is_pred_or_func(PredInfo),
                PredFormArity = pred_info_pred_form_arity(PredInfo),
                may_introduce_calls_to(PredOrFunc,
                    PredModuleNameStr, PredName, PredFormArity)
            ;
                % Don't attempt to eliminate local preds here, since we want
                % to do semantic checking on those, even if they are not used.
                pred_info_get_status(PredInfo, PredStatus),
                PredStatus = pred_status(OldStatus),
                ( OldStatus = status_external(_)
                ; OldStatus = status_abstract_imported
                ; OldStatus = status_pseudo_imported
                ; OldStatus = status_exported
                ; OldStatus = status_exported_to_submodules
                ; OldStatus = status_abstract_exported
                ; OldStatus = status_opt_exported
                ; OldStatus = status_pseudo_exported
                ; OldStatus = status_local
                )
            ;
                % Don't eliminate the clauses for promises.
                pred_info_is_promise(PredInfo, _)
            )
        then
            PredSymName = qualified(PredModuleName, PredName),
            set_tree234.insert(PredSymName, !NeededNames),
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
        RHS = rhs_lambda_goal(_, _, _, _, _, _, _, Goal),
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
%-----------------------------------------------------------------------------%

output_needed_map(Stream, ModuleInfo, Needed, !IO) :-
    map.to_sorted_assoc_list(Needed, NeededAL),
    io.write_string(Stream, "% Needed map:\n", !IO),
    list.foldl(output_needed_map_entry(Stream, ModuleInfo), NeededAL, !IO),
    io.write_string(Stream, "%\n", !IO),
    io.write_string(Stream, "% End of needed map\n", !IO).

:- pred output_needed_map_entry(io.text_output_stream::in, module_info::in,
    pair(entity, maybe_needed)::in, io::di, io::uo) is det.

output_needed_map_entry(Stream, ModuleInfo, Entity - MaybeNeeded, !IO) :-
    EntityStr = entity_to_string(ModuleInfo, Entity),
    (
        MaybeNeeded = maybe_eliminable(N),
        io.format(Stream, "%%\n%% entity: %s: %d refs\n",
            [s(EntityStr), i(N)], !IO)
    ;
        MaybeNeeded = not_eliminable(NeededReasonsSet),
        NeededReasons = set_ctree234.to_sorted_list(NeededReasonsSet),
        io.format(Stream, "%%\n%% entity: %s: needed\n", [s(EntityStr)], !IO),
        list.foldl2(output_needed_reason(Stream, ModuleInfo),
            NeededReasons, 1, NumReasons, !IO),
        ( if NumReasons = num_reasons_to_keep then
            io.write_string(Stream, "%%   ... and possibly more\n", !IO)
        else
            true
        )
    ).

:- func entity_to_string(module_info, entity) = string.

entity_to_string(ModuleInfo, Entity) = Str :-
    (
        Entity = entity_proc(PredProcId),
        ProcDesc = describe_proc_from_id(include_module_name,
            ModuleInfo, PredProcId),
        string.format("%s", [s(ProcDesc)], Str)
    ;
        Entity = entity_table_struct(PredProcId),
        ProcDesc = describe_proc_from_id(include_module_name,
            ModuleInfo, PredProcId),
        string.format("table_struct %s", [s(ProcDesc)], Str)
    ;
        Entity = entity_type_ctor(ModuleName, TypeName, TypeArity),
        string.format("type_ctor %s.%s/%d",
            [s(sym_name_to_string(ModuleName)), s(TypeName), i(TypeArity)],
            Str)
    ;
        Entity = entity_const_struct(N),
        string.format("const struct #%d", [i(N)], Str)
    ;
        Entity = entity_mutable(ModuleName, MutableName, Kind),
        ( Kind = mutable_pred_std_get, KindStr = "std get"
        ; Kind = mutable_pred_std_set, KindStr = "std set"
        ; Kind = mutable_pred_io_get, KindStr = "io get"
        ; Kind = mutable_pred_io_set, KindStr = "io set"
        ; Kind = mutable_pred_constant_get, KindStr = "constant get"
        ; Kind = mutable_pred_constant_secret_set, KindStr = "constant set"
        ; Kind = mutable_pred_unsafe_get, KindStr = "unsafe get"
        ; Kind = mutable_pred_unsafe_set, KindStr = "unsafe set"
        ; Kind = mutable_pred_lock, KindStr = "lock"
        ; Kind = mutable_pred_unlock, KindStr = "unlock"
        ; Kind = mutable_pred_init, KindStr = "init"
        ; Kind = mutable_pred_pre_init, KindStr = "pre_init"
        ),
        string.format("%s pred for mutable %s.%s",
            [s(KindStr), s(sym_name_to_string(ModuleName)), s(MutableName)],
            Str)
    ).

:- pred output_needed_reason(io.text_output_stream::in, module_info::in,
    needed_reason::in, int::in, int::out, io::di, io::uo) is det.

output_needed_reason(Stream, ModuleInfo, Reason, !CurReasonNum, !IO) :-
    ReasonStr = needed_reason_to_string(ModuleInfo, Reason),
    io.format(Stream, "%%   #%d: %s\n",
        [i(!.CurReasonNum), s(ReasonStr)], !IO),
    !:CurReasonNum = !.CurReasonNum + 1.

:- func needed_reason_to_string(module_info, needed_reason) = string.

needed_reason_to_string(ModuleInfo, Reason) = Str :-
    (
        Reason = nr_exported,
        Str = "exported"
    ;
        Reason = nr_foreign_exported,
        Str = "exported to foreign code"
    ;
        Reason = nr_initial_final,
        Str = "is initialization or finalization procedure"
    ;
        Reason = nr_type_ctor,
        Str = "type constructor"
    ;
        Reason = nr_const_struct,
        Str = "const struct"
    ;
        Reason = nr_class_interface_proc,
        Str = "class interface procedure"
    ;
        Reason = nr_used_by(Entity),
        EntityStr = entity_to_string(ModuleInfo, Entity),
        string.format("used by %s", [s(EntityStr)], Str)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

output_elimination_msgs(Stream, ModuleInfo, ElimMap, !IO) :-
    map.to_sorted_assoc_list(ElimMap, ElimMapAL),
    list.foldl(acc_pred_elimination_msg(ModuleInfo), ElimMapAL, [], Msgs),
    list.sort(Msgs, SortedMsgs),
    list.foldl(io.write_string(Stream), SortedMsgs, !IO).

:- pred acc_pred_elimination_msg(module_info::in,
    pair(pred_id, eliminated_what)::in,
    list(string)::in, list(string)::out) is det.

acc_pred_elimination_msg(ModuleInfo, PredId - ElimWhat, !Msgs) :-
    (
        ElimWhat = elim_whole_pred(ElimHow),
        PredStr = pred_id_to_user_string(ModuleInfo, PredId),
        (
            ElimHow = elim_deleted_proc,
            string.format("%% Eliminated dead %s\n", [s(PredStr)], Msg)
        ;
            ElimHow = elim_deleted_goal,
            string.format("%% Eliminated opt_imported %s\n", [s(PredStr)], Msg)
        ),
        !:Msgs = [Msg | !.Msgs]
    ;
        ElimWhat = elim_procs(ProcIds),
        list.foldl(acc_proc_elimination_msg(ModuleInfo, PredId),
            set.to_sorted_list(ProcIds), !Msgs)
    ).

:- pred acc_proc_elimination_msg(module_info::in, pred_id::in, proc_id::in,
    list(string)::in, list(string)::out) is det.

acc_proc_elimination_msg(ModuleInfo, PredId, ProcId, !Msgs) :-
    ProcStr = pred_proc_id_pair_to_user_string(ModuleInfo, PredId, ProcId),
    string.format("%% Eliminated dead %s\n", [s(ProcStr)], Msg),
    !:Msgs = [Msg | !.Msgs].

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.dead_proc_elim.
%-----------------------------------------------------------------------------%
