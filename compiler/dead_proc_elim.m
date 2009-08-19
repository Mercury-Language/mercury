%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2009 The University of Melbourne.
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

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.

:- import_module list.
:- import_module map.

%-----------------------------------------------------------------------------%

:- type maybe_elim_opt_imported
    --->    elim_opt_imported
    ;       do_not_elim_opt_imported.

    % Eliminate dead procedures. If the first argument is `elim_opt_imported',
    % also eliminate any opt_imported procedures.
    %
    % The last argument will be a list of warnings about any user-defined
    % procedures that are dead, but the caller is free to ignore this list.
    %
:- pred dead_proc_elim(maybe_elim_opt_imported::in,
    module_info::in, module_info::out, list(error_spec)::out) is det.

:- type needed_map == map(entity, maybe_needed).

:- type entity
    --->    entity_proc(pred_id, proc_id)
    ;       entity_table_struct(pred_id, proc_id)
    ;       entity_type_ctor(module_name, string, int).

:- type maybe_needed
    --->    not_eliminable
    ;       maybe_eliminable(num_references :: int).

    % Analyze which entities are needed, and for those entities which are
    % needed, record how many times they are referenced (this information
    % is used by our inlining heuristics).
    %
:- pred dead_proc_analyze(module_info::in, module_info::out, needed_map::out)
    is det.

    % Optimize away any dead predicates. This is performed immediately after
    % building the HLDS to avoid doing semantic checking and optimization
    % on predicates from `.opt' files which are not used in the current module.
    % This assumes that the clauses_info is still valid, so it cannot be run
    % after mode analysis.
    %
:- pred dead_pred_elim(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.simplify.
:- import_module check_hlds.try_expand.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module pair.
:- import_module queue.
:- import_module set_tree234.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svqueue.

%-----------------------------------------------------------------------------%

% We deal with two kinds of entities, procedures and type_ctor_info structures.
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
% The final pass of the algorithm deletes from the HLDS any procedure
% or type_ctor_info structure whose id is not in the needed map.

:- type entity_queue    ==  queue(entity).
:- type examined_set    ==  set_tree234(entity).

dead_proc_elim(ElimOptImported, !ModuleInfo, Specs) :-
    dead_proc_analyze(!ModuleInfo, Needed),
    dead_proc_eliminate(ElimOptImported, Needed, !ModuleInfo, Specs).

%-----------------------------------------------------------------------------%

dead_proc_analyze(!ModuleInfo, !:Needed) :-
    Examined0 = set_tree234.init,
    dead_proc_initialize(!ModuleInfo, Queue0, !:Needed),
    dead_proc_examine(Queue0, Examined0, !.ModuleInfo, !Needed).

    % Add all exported entities to the queue and map.
    % NOTE: changes here are likely to require changes to dead_pred_elim
    % as well.
    %
:- pred dead_proc_initialize(module_info::in,module_info::out,
    entity_queue::out, needed_map::out) is det.

dead_proc_initialize(!ModuleInfo, !:Queue, !:Needed) :-
    !:Queue = queue.init,
    !:Needed = map.init,
    module_info_predids(PredIds, !ModuleInfo),
    module_info_preds(!.ModuleInfo, PredTable),
    dead_proc_initialize_preds(PredIds, PredTable, !Queue, !Needed),

    module_info_get_pragma_exported_procs(!.ModuleInfo, PragmaExports),
    dead_proc_initialize_pragma_exports(PragmaExports, !Queue, !Needed),

    module_info_user_init_pred_procs(!.ModuleInfo, InitProcs),
    dead_proc_initialize_init_fn_procs(InitProcs, !Queue, !Needed),

    module_info_user_final_pred_procs(!.ModuleInfo, FinalPreds),
    dead_proc_initialize_init_fn_procs(FinalPreds, !Queue, !Needed),

    module_info_get_type_ctor_gen_infos(!.ModuleInfo, TypeCtorGenInfos),
    dead_proc_initialize_type_ctor_infos(TypeCtorGenInfos, !Queue, !Needed),

    module_info_get_class_table(!.ModuleInfo, Classes),
    module_info_get_instance_table(!.ModuleInfo, Instances),
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
    ProcIds = pred_info_exported_procids(PredInfo),
    dead_proc_initialize_procs(PredId, ProcIds, !Queue, !Needed),
    dead_proc_initialize_preds(PredIds, PredTable, !Queue, !Needed).

    % Add the listed procedures to the queue and map.
    %
:- pred dead_proc_initialize_procs(pred_id::in, list(proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_procs(_PredId, [], !Queue, !Needed).
dead_proc_initialize_procs(PredId, [ProcId | ProcIds], !Queue, !Needed) :-
    Entity = entity_proc(PredId, ProcId),
    svqueue.put(Entity, !Queue),
    svmap.set(Entity, not_eliminable, !Needed),
    dead_proc_initialize_procs(PredId, ProcIds, !Queue, !Needed).

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
    svqueue.put(Entity, !Queue),
    svmap.set(Entity, not_eliminable, !Needed),
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
    svqueue.put(Entity, !Queue),
    svmap.set(Entity, not_eliminable, !Needed),
    dead_proc_initialize_init_fn_procs(PPIds, !Queue, !Needed).

:- pred dead_proc_initialize_type_ctor_infos(list(type_ctor_gen_info)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_initialize_type_ctor_infos([], !Queue, !Needed).
dead_proc_initialize_type_ctor_infos([TypeCtorGenInfo | TypeCtorGenInfos],
        !Queue, !Needed) :-
    TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName, TypeName,
        Arity, _Status, _HldsDefn, _Unify, _Compare),
    (
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
    ->
        Entity = entity_type_ctor(ModuleName, TypeName, Arity),
        svqueue.put(Entity, !Queue),
        svmap.set(Entity, not_eliminable, !Needed)
    ;
        true
    ),
    dead_proc_initialize_type_ctor_infos(TypeCtorGenInfos, !Queue, !Needed).

:- pred dead_proc_initialize_class_methods(class_table::in,
    instance_table::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

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
    Instance = hlds_instance_defn(_, _, _, _, _, _, PredProcIds, _, _),
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
    Methods = Class ^ class_hlds_interface,
    list.foldl2(get_class_interface_pred_proc, Methods, !Queue, !Needed).

:- pred get_class_interface_pred_proc(hlds_class_proc::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

get_class_interface_pred_proc(ClassProc, !Queue, !Needed) :-
    ClassProc = hlds_class_proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),
    svqueue.put(Entity, !Queue),
    svmap.set(Entity, not_eliminable, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine(entity_queue::in, examined_set::in,
    module_info::in, needed_map::in, needed_map::out) is det.

dead_proc_examine(!.Queue, !.Examined, ModuleInfo, !Needed) :-
    % See if the queue is empty.
    ( svqueue.get(Entity, !Queue) ->
        % See if the next element has been examined before.
        ( set_tree234.member(!.Examined, Entity) ->
            dead_proc_examine(!.Queue, !.Examined, ModuleInfo, !Needed)
        ;
            set_tree234.insert(Entity, !Examined),
            (
                Entity = entity_proc(PredId, ProcId),
                PredProcId = proc(PredId, ProcId),
                dead_proc_examine_proc(PredProcId, ModuleInfo, !Queue, !Needed)
            ;
                Entity = entity_table_struct(_PredId, _ProcId)
                % Nothing further to examine.
            ;
                Entity = entity_type_ctor(Module, Type, Arity),
                dead_proc_examine_type_ctor_info(Module, Type, Arity,
                    ModuleInfo, !Queue, !Needed)
            ),
            dead_proc_examine(!.Queue, !.Examined, ModuleInfo, !Needed)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_type_ctor_info(module_name::in, string::in,
    arity::in, module_info::in, entity_queue::in, entity_queue::out,
    needed_map::in, needed_map::out) is det.

dead_proc_examine_type_ctor_info(ModuleName, TypeName, Arity, ModuleInfo,
        !Queue, !Needed) :-
    module_info_get_type_ctor_gen_infos(ModuleInfo, TypeCtorGenInfos),
    (
        find_type_ctor_info(ModuleName, TypeName, Arity, TypeCtorGenInfos,
            Refs)
    ->
        dead_proc_examine_refs(Refs, !Queue, !Needed)
    ;
        true
    ).

:- pred find_type_ctor_info(module_name::in, string::in,
    arity::in, list(type_ctor_gen_info)::in, list(pred_proc_id)::out)
    is semidet.

find_type_ctor_info(ModuleName, TypeName, TypeArity,
        [TypeCtorGenInfo | TypeCtorGenInfos], Refs) :-
    (
        TypeCtorGenInfo = type_ctor_gen_info(_TypeCtor, ModuleName,
            TypeName, TypeArity, _Status, _HldsDefn, Unify, Compare)
    ->
        Refs = [Unify, Compare]
    ;
        find_type_ctor_info(ModuleName, TypeName, TypeArity, TypeCtorGenInfos,
            Refs)
    ).

:- pred maybe_add_ref(maybe(pred_proc_id)::in,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

maybe_add_ref(no, Refs, Refs).
maybe_add_ref(yes(Ref), Refs, [Ref | Refs]).

:- pred dead_proc_examine_refs(list(pred_proc_id)::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_refs([], !Queue, !Needed).
dead_proc_examine_refs([Ref | Refs], !Queue, !Needed) :-
    Ref = proc(PredId, ProcId),
    Entity = entity_proc(PredId, ProcId),
    svqueue.put(Entity, !Queue),
    svmap.set(Entity, not_eliminable, !Needed),
    dead_proc_examine_refs(Refs, !Queue, !Needed).

%-----------------------------------------------------------------------------%

:- pred dead_proc_examine_proc(pred_proc_id::in, module_info::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_proc(proc(PredId, ProcId), ModuleInfo,
        !Queue, !Needed) :-
    (
        module_info_preds(ModuleInfo, PredTable),
        map.lookup(PredTable, PredId, PredInfo),
        ProcIds = pred_info_non_imported_procids(PredInfo),
        list.member(ProcId, ProcIds),
        pred_info_get_procedures(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo)
    ->
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            io.write_string("examining proc ", !IO),
            io.write_int(pred_id_to_int(PredId), !IO),
            io.write_string(" ", !IO),
            io.write_int(proc_id_to_int(ProcId), !IO),
            io.nl(!IO)
        ),
        proc_info_get_goal(ProcInfo, Goal),
        dead_proc_examine_goal(Goal, proc(PredId, ProcId), !Queue, !Needed),

        proc_info_get_eval_method(ProcInfo, EvalMethod),
        HasPerProcTablingPtr =
            eval_method_has_per_proc_tabling_pointer(EvalMethod),
        (
            HasPerProcTablingPtr = no
        ;
            HasPerProcTablingPtr = yes,
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                io.write_string("need table struct for proc ", !IO),
                io.write_int(pred_id_to_int(PredId), !IO),
                io.write_string(" ", !IO),
                io.write_int(proc_id_to_int(ProcId), !IO),
                io.nl(!IO)
            ),
            TableStructEntity = entity_table_struct(PredId, ProcId),
            svmap.set(TableStructEntity, not_eliminable, !Needed)
        )
    ;
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            io.write_string("not examining proc ", !IO),
            io.write_int(pred_id_to_int(PredId), !IO),
            io.write_string(" ", !IO),
            io.write_int(proc_id_to_int(ProcId), !IO),
            io.nl(!IO)
        )
    ).

:- pred dead_proc_examine_goals(list(hlds_goal)::in, pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_goals([], _, !Queue, !Needed).
dead_proc_examine_goals([Goal | Goals], CurrProc, !Queue, !Needed) :-
    dead_proc_examine_goal(Goal, CurrProc, !Queue, !Needed),
    dead_proc_examine_goals(Goals, CurrProc, !Queue, !Needed).

:- pred dead_proc_examine_cases(list(case)::in, pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_cases([], _CurrProc, !Queue, !Needed).
dead_proc_examine_cases([case(_, _, Goal) | Cases], CurrProc,
        !Queue, !Needed) :-
    dead_proc_examine_goal(Goal, CurrProc, !Queue, !Needed),
    dead_proc_examine_cases(Cases, CurrProc, !Queue, !Needed).

:- pred dead_proc_examine_goal(hlds_goal::in, pred_proc_id::in,
    entity_queue::in, entity_queue::out, needed_map::in, needed_map::out)
    is det.

dead_proc_examine_goal(Goal, CurrProc, !Queue, !Needed) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = conj(_ConjType, Goals)
        ; GoalExpr = disj(Goals)
        ),
        dead_proc_examine_goals(Goals, CurrProc, !Queue, !Needed)
    ;
        GoalExpr = switch(_Var, _CanFail, Cases),
        dead_proc_examine_cases(Cases, CurrProc, !Queue, !Needed)
    ;
        GoalExpr = negation(SubGoal),
        dead_proc_examine_goal(SubGoal, CurrProc, !Queue, !Needed)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( Reason = from_ground_term(_, from_ground_term_construct) ->
            % The scope has no references to procedures at all.
            true
        ;
            dead_proc_examine_goal(SubGoal, CurrProc, !Queue, !Needed)
        )
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        dead_proc_examine_goal(Cond, CurrProc, !Queue, !Needed),
        dead_proc_examine_goal(Then, CurrProc, !Queue, !Needed),
        dead_proc_examine_goal(Else, CurrProc, !Queue, !Needed)
    ;
        GoalExpr = generic_call(_, _, _, _)
    ;
        GoalExpr = plain_call(PredId, ProcId, _,_,_,_),
        Entity = entity_proc(PredId, ProcId),
        queue.put(!.Queue, Entity, !:Queue),
        ( proc(PredId, ProcId) = CurrProc ->
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                io.write_string("plain_call recursive ", !IO),
                io.write_int(pred_id_to_int(PredId), !IO),
                io.write_string(" ", !IO),
                io.write_int(proc_id_to_int(ProcId), !IO),
                io.nl(!IO)
            ),
            % If it's reachable and recursive, then we can't eliminate it
            % or inline it.
            NewNotation = not_eliminable,
            svmap.set(Entity, NewNotation, !Needed)
        ; map.search(!.Needed, Entity, OldNotation) ->
            (
                OldNotation = not_eliminable,
                NewNotation = not_eliminable,
                trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                    io.write_string("plain_call old not_eliminable ", !IO),
                    io.write_int(pred_id_to_int(PredId), !IO),
                    io.write_string(" ", !IO),
                    io.write_int(proc_id_to_int(ProcId), !IO),
                    io.nl(!IO)
                )
            ;
                OldNotation = maybe_eliminable(Count),
                NewNotation = maybe_eliminable(Count + 1),
                trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                    io.write_string("plain_call incr maybe_eliminable ", !IO),
                    io.write_int(pred_id_to_int(PredId), !IO),
                    io.write_string(" ", !IO),
                    io.write_int(proc_id_to_int(ProcId), !IO),
                    io.nl(!IO)
                )
            ),
            svmap.det_update(Entity, NewNotation, !Needed)
        ;
            trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
                io.write_string("plain_call init maybe_eliminable ", !IO),
                io.write_int(pred_id_to_int(PredId), !IO),
                io.write_string(" ", !IO),
                io.write_int(proc_id_to_int(ProcId), !IO),
                io.nl(!IO)
            ),
            NewNotation = maybe_eliminable(1),
            svmap.set(Entity, NewNotation, !Needed)
        )
    ;
        GoalExpr = call_foreign_proc(_, PredId, ProcId, _, _, _, _),
        Entity = entity_proc(PredId, ProcId),
        trace [io(!IO), compile_time(flag("dead_proc_elim"))] (
            io.write_string("foreign_proc ", !IO),
            io.write_int(pred_id_to_int(PredId), !IO),
            io.write_string(" ", !IO),
            io.write_int(proc_id_to_int(ProcId), !IO),
            io.nl(!IO)
        ),
        svqueue.put(Entity, !Queue),
        svmap.set(Entity, not_eliminable, !Needed)
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
                        io.write_string("pred_const ", !IO),
                        io.write_int(pred_id_to_int(PredId), !IO),
                        io.write_string(" ", !IO),
                        io.write_int(proc_id_to_int(ProcId), !IO),
                        io.nl(!IO)
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
                        io.write_string("table struct const ", !IO),
                        io.write_int(pred_id_to_int(PredId), !IO),
                        io.write_string(" ", !IO),
                        io.write_int(proc_id_to_int(ProcId), !IO),
                        io.nl(!IO)
                    )
                ),
                svqueue.put(Entity, !Queue),
                svmap.set(Entity, not_eliminable, !Needed)
            ;
                ( ConsId = cons(_, _, _)
                ; ConsId = tuple_cons(_)
                ; ConsId = int_const(_)
                ; ConsId = float_const(_)
                ; ConsId = char_const(_)
                ; ConsId = string_const(_)
                ; ConsId = impl_defined_const(_)
                ; ConsId = base_typeclass_info_const(_, _, _, _)
                ; ConsId = type_info_cell_constructor(_)
                ; ConsId = typeclass_info_cell_constructor
                ; ConsId = deep_profiling_proc_layout(_)
                ; ConsId = table_io_decl(_)
                )
                % Do nothing.
            )
        ;
            ( Unification = deconstruct(_, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            )
            % Do nothing.
        ;
            Unification = complicated_unify(_, _, _),
            % These should have been replaced with calls by now.
            unexpected(this_file, "dead_proc_examine_goal: complicated_unify")
        )
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "dead_proc_examine_goal: shorthand")
    ).

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

                % Has anything changed.
                proc_elim_changed       :: bool,

                % A list of warning messages.
                proc_elim_warnings      :: list(error_spec)
            ).

    % Given the information about which entities are needed,
    % eliminate procedures which are not needed.
    %
:- pred dead_proc_eliminate(maybe_elim_opt_imported::in, needed_map::in,
    module_info::in, module_info::out, list(error_spec)::out) is det.

dead_proc_eliminate(ElimOptImported, !.Needed, !ModuleInfo, Specs) :-
    module_info_predids(PredIds, !ModuleInfo),
    module_info_preds(!.ModuleInfo, PredTable0),

    Changed0 = no,
    ProcElimInfo0 = proc_elim_info(!.Needed, !.ModuleInfo, PredTable0,
        Changed0, []),
    list.foldl(dead_proc_eliminate_pred(ElimOptImported), PredIds,
        ProcElimInfo0, ProcElimInfo),
    ProcElimInfo = proc_elim_info(!:Needed, !:ModuleInfo, PredTable,
        Changed, Specs),

    module_info_set_preds(PredTable, !ModuleInfo),
    module_info_get_type_ctor_gen_infos(!.ModuleInfo, TypeCtorGenInfos0),
    dead_proc_eliminate_type_ctor_infos(TypeCtorGenInfos0, !.Needed,
        TypeCtorGenInfos),
    module_info_set_type_ctor_gen_infos(TypeCtorGenInfos, !ModuleInfo),

    % We could also eliminate eliminate table structs, but we don't do that
    % yet, because some references to such structs are currently visible
    % only in C code embedded in compiler-generated foreign_procs, and
    % therefore we might accidentally create dangling references.

    (
        Changed = yes,
        % The dependency graph will still contain references to the eliminated
        % procedures, so it must be rebuilt if it will be used later.
        module_info_clobber_dependency_info(!ModuleInfo)
    ;
        Changed = no
    ).

    % Eliminate any unused procedures for this pred.
    %
:- pred dead_proc_eliminate_pred(maybe_elim_opt_imported::in, pred_id::in,
    proc_elim_info::in, proc_elim_info::out) is det.

dead_proc_eliminate_pred(ElimOptImported, PredId, !ProcElimInfo) :-
    !.ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable0, Changed0,
        Specs0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_import_status(PredInfo0, Status),
    (
        % Find out if the predicate is defined in this module.
        % If yes, find out also whether any of its procedures must be kept.
        (
            Status = status_local,
            Keep = no,
            (
                % Don't warn for unify or comparison preds,
                % since they may be automatically generated
                is_unify_or_compare_pred(PredInfo0)
            ->
                WarnForThisProc = no
            ;
                % Don't warn for procedures introduced from lambda expressions.
                % The only time those procedures will be unused is if the
                % procedure containing the lambda expression is unused,
                % and in that case, we already warn for that containing
                % procedure if appropriate. Likewise, don't warn for procedures
                % introduced for type specialization.
                PredName = pred_info_name(PredInfo0),
                ( string.prefix(PredName, "IntroducedFrom__")
                ; string.prefix(PredName, "TypeSpecOf__")
                )
            ->
                WarnForThisProc = no
            ;
                WarnForThisProc = yes
            )
        ;
            Status = status_pseudo_imported,
            Keep = no,
            WarnForThisProc = no
        ;
            Status = status_pseudo_exported,
            hlds_pred.in_in_unification_proc_id(InitProcId),
            Keep = yes(InitProcId),
            WarnForThisProc = no
        )
    ->
        ProcIds = pred_info_procids(PredInfo0),
        pred_info_get_procedures(PredInfo0, ProcTable0),
        list.foldl3(dead_proc_eliminate_proc(PredId, Keep, WarnForThisProc,
            !.ProcElimInfo),
            ProcIds, ProcTable0, ProcTable, Changed0, Changed, Specs0, Specs),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        map.det_update(PredTable0, PredId, PredInfo, PredTable),
        !:ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable, Changed,
            Specs)
    ;
        % Don't generate code in the current module for unoptimized
        % opt_imported preds (that is, for opt_imported preds which we have not
        % by this point managed to inline or specialize; this code should be
        % called with `ElimOptImported = elim_opt_imported' only after inlining
        % and specialization is complete).
        ElimOptImported = elim_opt_imported,
        Status = status_opt_imported
    ->
        Changed = yes,
        ProcIds = pred_info_procids(PredInfo0),
        pred_info_get_procedures(PredInfo0, ProcTable0),

        % Reduce memory usage by replacing the goals with "true".
        DestroyGoal =
            (pred(Id::in, PTable0::in, PTable::out) is det :-
                map.lookup(ProcTable0, Id, ProcInfo0),
                proc_info_set_goal(true_goal, ProcInfo0, ProcInfo),
                map.det_update(PTable0, Id, ProcInfo, PTable)
            ),
        list.foldl(DestroyGoal, ProcIds, ProcTable0, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo1),
        pred_info_set_import_status(status_imported(import_locn_interface),
            PredInfo1, PredInfo),
        map.det_update(PredTable0, PredId, PredInfo, PredTable),

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
        !:ProcElimInfo = proc_elim_info(Needed, ModuleInfo, PredTable, Changed,
            Specs0)
    ;
        % This predicate is not defined in this module.
        true
    ).

    % Eliminate a procedure, if unused.
    %
:- pred dead_proc_eliminate_proc(pred_id::in, maybe(proc_id)::in, bool::in,
    proc_elim_info::in, proc_id::in, proc_table::in, proc_table::out,
    bool::in, bool::out, list(error_spec)::in, list(error_spec)::out) is det.

dead_proc_eliminate_proc(PredId, Keep, WarnForThisProc, ProcElimInfo,
        ProcId, !ProcTable, !Changed, !Specs) :-
    Needed = ProcElimInfo ^ proc_elim_needed_map,
    ModuleInfo = ProcElimInfo ^ proc_elim_module_info,
    (
        (
            % Keep the procedure if it is in the needed map.
            map.search(Needed, entity_proc(PredId, ProcId), _)
        ;
            % Or if it is to be kept because it is exported.
            Keep = yes(ProcId)
        )
    ->
        true
    ;
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
        ),
        map.lookup(!.ProcTable, ProcId, ProcInfo0),
        (
            WarnForThisProc = yes,
            proc_info_get_context(ProcInfo0, Context),
            Spec = warn_dead_proc(PredId, ProcId, Context, ModuleInfo),
            !:Specs = [Spec | !.Specs]
        ;
            WarnForThisProc = no
        ),
        svmap.delete(ProcId, !ProcTable)
    ).

:- func warn_dead_proc(pred_id, proc_id, prog_context, module_info)
    = error_spec.

warn_dead_proc(PredId, ProcId, Context, ModuleInfo) = Spec :-
    ProcPieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = [words("Warning:")] ++ ProcPieces ++
        [words("is never called."), nl],
    Msg = simple_msg(Context,
        [option_is_set(warn_dead_procs, yes, [always(Pieces)])]),
    Severity = severity_conditional(warn_dead_procs, yes,
        severity_warning, no),
    Spec = error_spec(Severity, phase_dead_code, [Msg]).

:- pred dead_proc_eliminate_type_ctor_infos(list(type_ctor_gen_info)::in,
    needed_map::in, list(type_ctor_gen_info)::out) is det.

dead_proc_eliminate_type_ctor_infos([], _Needed, []).
dead_proc_eliminate_type_ctor_infos([TypeCtorGenInfo0 | TypeCtorGenInfos0],
        Needed, TypeCtorGenInfos) :-
    dead_proc_eliminate_type_ctor_infos(TypeCtorGenInfos0, Needed,
        TypeCtorGenInfos1),
    TypeCtorGenInfo0 = type_ctor_gen_info(_TypeCtor, ModuleName,
        TypeName, Arity, _Status, _HldsDefn, _Unify, _Compare),
    (
        Entity = entity_type_ctor(ModuleName, TypeName, Arity),
        map.search(Needed, Entity, _)
    ->
        TypeCtorGenInfos = [TypeCtorGenInfo0 | TypeCtorGenInfos1]
    ;
        TypeCtorGenInfos = TypeCtorGenInfos1
    ).

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
    dead_proc_initialize_pragma_exports(PragmaExports, Queue0, _,
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

    module_info_predids(PredIds, !ModuleInfo),

    Preds0 = set_tree234.init,
    Names0 = set_tree234.init,
    DeadInfo0 = pred_elim_info(!.ModuleInfo, Queue, Preds0, NeededPreds1,
        Names0),
    list.foldl(dead_pred_elim_initialize, PredIds, DeadInfo0, DeadInfo1),
    dead_pred_elim_analyze(DeadInfo1, DeadInfo),
    DeadInfo = pred_elim_info(!:ModuleInfo, _, _, NeededPreds2, _),

    % If a predicate is not needed, predicates which were added in make_hlds.m
    % to force type specialization are also not needed. Here we add in those
    % which are needed.

    module_info_get_type_spec_info(!.ModuleInfo, TypeSpecInfo0),
    TypeSpecInfo0 = type_spec_info(TypeSpecProcs0, TypeSpecForcePreds0,
        SpecMap0, PragmaMap0),
    set_tree234.to_sorted_list(NeededPreds2) = NeededPredList2,
    list.foldl((pred(NeededPred::in, AllPreds0::in, AllPreds::out) is det :-
        ( map.search(SpecMap0, NeededPred, NewNeededPreds) ->
            set_tree234.insert_list(NewNeededPreds, AllPreds0, AllPreds)
        ;
            AllPreds = AllPreds0
        )
    ), NeededPredList2, NeededPreds2, NeededPreds),
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

:- pred dead_pred_elim_add_entity(entity::in, queue(pred_id)::in,
    queue(pred_id)::out, set_tree234(pred_id)::in, set_tree234(pred_id)::out)
    is det.

dead_pred_elim_add_entity(entity_type_ctor(_, _, _), !Queue, !Preds).
dead_pred_elim_add_entity(entity_table_struct(_, _), !Queue, !Preds).
dead_pred_elim_add_entity(entity_proc(PredId, _), !Queue, !Preds) :-
    svqueue.put(PredId, !Queue),
    set_tree234.insert(PredId, !Preds).

:- pred dead_pred_elim_initialize(pred_id::in,
    pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_elim_initialize(PredId, DeadInfo0, DeadInfo) :-
    some [!Queue, !NeededNames] (
        DeadInfo0 = pred_elim_info(ModuleInfo, !:Queue, Examined, NeededIds,
            !:NeededNames),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        (
            PredModule = pred_info_module(PredInfo),
            PredName = pred_info_name(PredInfo),
            PredArity = pred_info_orig_arity(PredInfo),
            (
                % Don't eliminate special preds since they won't be actually
                % called from the HLDS until after polymorphism.
                is_unify_or_compare_pred(PredInfo)
            ;
                % Don't eliminate preds from builtin modules, since later
                % passes of the compiler may introduce calls to them
                % (e.g. polymorphism.m needs unify/2 and friends).
                any_mercury_builtin_module(PredModule)
            ;
                % Simplify can't introduce calls to this predicate or function
                % if we eliminate it here.
                is_std_lib_module_name(PredModule, PredModuleName),
                simplify_may_introduce_calls(PredModuleName, PredName,
                    PredArity)
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
                \+ pred_info_is_imported(PredInfo),
                \+ pred_info_get_import_status(PredInfo, status_opt_imported)
            ;
                % Don't eliminate predicates declared in this module with a
                % `:- external' declaration.
                module_info_get_name(ModuleInfo, PredModule)
            ;
                % Don't eliminate <foo>_init_any/1 predicates; modes.m may
                % insert calls to them to initialize variables from inst `free'
                % to inst `any'.
                string.remove_suffix(PredName, "_init_any", _),
                PredArity = 1
            ;
                % Don't eliminate the clauses for promises.
                pred_info_get_goal_type(PredInfo, goal_type_promise(_))
            )
        ->
            set_tree234.insert(qualified(PredModule, PredName), !NeededNames),
            svqueue.put(PredId, !Queue)
        ;
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
        ( svqueue.get(PredId, !Queue) ->
            ( set_tree234.member(!.Examined, PredId) ->
                !:DeadInfo = pred_elim_info(ModuleInfo, !.Queue, !.Examined,
                    !.Needed, NeededNames)
            ;
                set_tree234.insert(PredId, !Needed),
                set_tree234.insert(PredId, !Examined),
                !:DeadInfo = pred_elim_info(ModuleInfo, !.Queue, !.Examined,
                    !.Needed, NeededNames),
                module_info_pred_info(ModuleInfo, PredId, PredInfo),
                pred_info_get_clauses_info(PredInfo, ClausesInfo),
                clauses_info_get_clauses_rep(ClausesInfo, ClausesRep),
                get_clause_list_any_order(ClausesRep, Clauses),
                list.foldl(dead_pred_elim_process_clause, Clauses, !DeadInfo)
            ),
            dead_pred_elim_analyze(!DeadInfo)
        ;
            true
        )
    ).

:- pred dead_pred_elim_process_clause(clause::in,
    pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_elim_process_clause(clause(_, Goal, _, _), !DeadInfo) :-
    pre_modecheck_examine_goal(Goal, !DeadInfo).

:- pred pre_modecheck_examine_goal(hlds_goal::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_goal(Goal, !DeadInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    pre_modecheck_examine_goal_expr(GoalExpr, !DeadInfo).

:- pred pre_modecheck_examine_goal_expr(hlds_goal_expr::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_goal_expr(conj(_ConjType, Goals), !DeadInfo) :-
    list.foldl(pre_modecheck_examine_goal, Goals, !DeadInfo).
pre_modecheck_examine_goal_expr(disj(Goals), !DeadInfo) :-
    list.foldl(pre_modecheck_examine_goal, Goals, !DeadInfo).
pre_modecheck_examine_goal_expr(if_then_else(_, If, Then, Else), !DeadInfo) :-
    list.foldl(pre_modecheck_examine_goal, [If, Then, Else], !DeadInfo).
pre_modecheck_examine_goal_expr(switch(_, _, Cases), !DeadInfo) :-
    ExamineCase = (pred(Case::in, Info0::in, Info::out) is det :-
        Case = case(_, _, Goal),
        pre_modecheck_examine_goal(Goal, Info0, Info)
    ),
    list.foldl(ExamineCase, Cases, !DeadInfo).
pre_modecheck_examine_goal_expr(generic_call(_,_,_,_), !DeadInfo).
pre_modecheck_examine_goal_expr(negation(SubGoal), !DeadInfo) :-
    pre_modecheck_examine_goal(SubGoal, !DeadInfo).
pre_modecheck_examine_goal_expr(scope(_, SubGoal), !DeadInfo) :-
    % The invariants that would allow us to optimize from_ground_term_construct
    % scopes haven't been established yet, which is why we must always scan
    % SubGoal.
    pre_modecheck_examine_goal(SubGoal, !DeadInfo).
pre_modecheck_examine_goal_expr(plain_call(_, _, _, _, _, PredName),
        !DeadInfo) :-
    dead_pred_info_add_pred_name(PredName, !DeadInfo).
pre_modecheck_examine_goal_expr(call_foreign_proc(_, _, _, _, _, _, _),
        !DeadInfo).
pre_modecheck_examine_goal_expr(unify(_, Rhs, _, _, _), !DeadInfo) :-
    pre_modecheck_examine_unify_rhs(Rhs, !DeadInfo).
pre_modecheck_examine_goal_expr(shorthand(ShortHand), !DeadInfo) :-
    (
        ShortHand = atomic_goal(_GoalType, _Outer, _Inner, _MaybeOutputVars,
            MainGoal, OrElseGoals, _OrElseInners),
        pre_modecheck_examine_goal(MainGoal, !DeadInfo),
        list.foldl(pre_modecheck_examine_goal, OrElseGoals, !DeadInfo)
    ;
        ShortHand = try_goal(_MaybeIO, _ResultVar, SubGoal),
        pre_modecheck_examine_goal(SubGoal, !DeadInfo)
    ;
        ShortHand = bi_implication(_, _),
        % These should have been expanded out by now.
        unexpected(this_file,
            "pre_modecheck_examine_goal_expr: unexpected bi_implication")
    ).

:- pred pre_modecheck_examine_unify_rhs(unify_rhs::in,
    pred_elim_info::in, pred_elim_info::out) is det.

pre_modecheck_examine_unify_rhs(rhs_var(_), !DeadInfo).
pre_modecheck_examine_unify_rhs(rhs_functor(Functor, _, _), !DeadInfo) :-
    ( Functor = cons(Name, _, _) ->
        dead_pred_info_add_pred_name(Name, !DeadInfo)
    ;
        true
    ).
pre_modecheck_examine_unify_rhs(rhs_lambda_goal(_, _, _, _, _, _, _, _, Goal),
        !DeadInfo) :-
    pre_modecheck_examine_goal(Goal, !DeadInfo).

:- pred dead_pred_info_add_pred_name(sym_name::in,
    pred_elim_info::in, pred_elim_info::out) is det.

dead_pred_info_add_pred_name(Name, !DeadInfo) :-
    some [!Queue, !NeededNames] (
        !.DeadInfo = pred_elim_info(ModuleInfo, !:Queue, Examined,
            Needed, !:NeededNames),
        ( set_tree234.member(!.NeededNames, Name) ->
            true
        ;
            module_info_get_predicate_table(ModuleInfo, PredicateTable),
            set_tree234.insert(Name, !NeededNames),
            (
                predicate_table_search_sym(PredicateTable,
                    may_be_partially_qualified, Name, PredIds)
            ->
                svqueue.put_list(PredIds, !Queue)
            ;
                true
            ),
            !:DeadInfo = pred_elim_info(ModuleInfo, !.Queue, Examined,
                Needed, !.NeededNames)
        )
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "dead_proc_elim.m".

%-----------------------------------------------------------------------------%
:- end_module dead_proc_elim.
%-----------------------------------------------------------------------------%
