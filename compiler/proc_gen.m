%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: code_gen.m.
% Main authors: conway, zs.
%
% Code generation - convert from HLDS to LLDS.
%
% The two main tasks of this module are
%
% 1 to look after the aspects of generating code for a procedure
%   that do not involve generating code for a specific goal, and
%
% 2 to provide a generic predicate that can be called from anywhere in
%   the code generator to generate code for a goal.
%
% Code_gen forwards most of the actual construction of code for particular
% goals to other modules. The generation of code for unifications is done
% by unify_gen, for calls, higher-order calls and method calls by call_gen,
% for commits by commit_gen, for if-then-elses and negations by ite_gen,
% for switches by switch_gen and its subsidiary modules, for disjunctions
% by disj_gen, and for pragma_c_codes by pragma_c_gen. The only kind of goal
% handled directly by code_gen is the conjunction.
%
%---------------------------------------------------------------------------%

:- module ll_backend.proc_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.

:- import_module list.

%---------------------------------------------------------------------------%

    % Translate a HLDS module to LLDS.
    %
:- pred generate_module_code(module_info::in, list(c_procedure)::out,
    global_data::in, global_data::out) is det.

    % Translate a HLDS procedure to LLDS, threading through the data structure
    % that records information about layout structures.
    %
:- pred generate_proc_code(module_info::in, const_struct_map::in,
    pred_id::in, pred_info::in, proc_id::in, proc_info::in,
    c_procedure::out, global_data::in, global_data::out) is det.

    % Return the message that identifies the procedure to pass to
    % the incr_sp_push_msg macro in the generated C code.
    %
:- func push_msg(module_info, pred_id, proc_id) = string.

    % Add all the global variables required for tabling by the procedures
    % of the module.
    %
:- pred add_all_tabling_info_structs(module_info::in,
    global_data::in, global_data::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module hlds.code_model.
:- import_module hlds.goal_form.
:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.code_gen.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.layout.
:- import_module ll_backend.middle_rec.
:- import_module ll_backend.stack_layout.
:- import_module ll_backend.trace_gen.
:- import_module ll_backend.unify_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

generate_module_code(ModuleInfo, CProcs, !GlobalData) :-
    % Get a list of all the predicate ids for which we will generate code.
    module_info_get_valid_pred_ids(ModuleInfo, PredIds),
    % Check if we want to use parallel code generation.
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, parallel_code_gen, ParallelCodeGen),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),

    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.write_string("% Generating constant structures\n", !IO)
        ),
        generate_const_structs(ModuleInfo, ConstStructMap, !GlobalData),
        trace [io(!IO)] (
            maybe_report_stats(Statistics, !IO)
        )
    ;
        VeryVerbose = no,
        generate_const_structs(ModuleInfo, ConstStructMap, !GlobalData)
    ),

    ( if
        ParallelCodeGen = yes,
        % Can't do parallel code generation if I/O is required.
        VeryVerbose = no,
        Statistics = no
    then
        generate_module_code_par(ModuleInfo, ConstStructMap, PredIds,
            CProcsCord, !GlobalData)
    else
        generate_module_code_seq(ModuleInfo, VeryVerbose, Statistics,
            ConstStructMap, PredIds, CProcsCord, !GlobalData)
    ),
    CProcs = cord.list(CProcsCord).

:- pred generate_module_code_seq(module_info::in, bool::in, bool::in,
    const_struct_map::in, list(pred_id)::in, cord(c_procedure)::out,
    global_data::in, global_data::out) is det.

generate_module_code_seq(ModuleInfo, VeryVerbose, Statistics, ConstStructMap,
        PredIds, CProcsCord, !GlobalData) :-
    list.foldl2(
        generate_pred_code_seq(ModuleInfo, VeryVerbose, Statistics,
            ConstStructMap),
        PredIds, cord.init, CProcsCord, !GlobalData).

%-----------------------------------------------------------------------------%

:- pred generate_module_code_par(module_info::in, const_struct_map::in,
    list(pred_id)::in, cord(c_procedure)::out,
    global_data::in, global_data::out) is det.

generate_module_code_par(ModuleInfo, ConstStructMap, PredIds, CProcsCord,
        !GlobalData) :-
    % Split up the list of predicates into pieces for processing in parallel.
    % Splitting the list in the middle does not work well as the load will be
    % unbalanced. Splitting the list in any other way (as we do) does mean
    % that the generated code will be slightly different due to the static
    % data being reordered.
    %
    % We only try to make use of two processors (threads) for now. Using more
    % processors efficiently probably requires knowing how many processors are
    % available, so we can divide the pred list whilst minimise the time
    % merging global_datas and updating static cell references.
    %
    list.chunk(PredIds, pred_list_chunk_size, ListsOfPredIds),
    interleave(ListsOfPredIds, ListsOfPredIdsA, ListsOfPredIdsB),
    GlobalData0 = !.GlobalData,
    (
        list.condense(ListsOfPredIdsA, PredIdsA),
        list.foldl2(generate_pred_code_par(ModuleInfo, ConstStructMap),
            PredIdsA, cord.init, CProcsCordA, GlobalData0, GlobalDataA)
    % XXX the following should be a parallel conjunction
    ,
        list.condense(ListsOfPredIdsB, PredIdsB),
        bump_type_num_counter(type_num_skip, GlobalData0, GlobalData1),
        list.foldl2(generate_pred_code_par(ModuleInfo, ConstStructMap),
            PredIdsB, cord.init, CProcsCordB0, GlobalData1, GlobalDataB)
    ),
    merge_global_datas(GlobalDataA, GlobalDataB, !:GlobalData, Remap),
    cord.map_pred(remap_references_to_global_data(Remap),
        CProcsCordB0, CProcsCordB),
    CProcsCord = CProcsCordA ++ CProcsCordB.

    % These numbers are rather arbitrary.
    %
:- func pred_list_chunk_size = int.
pred_list_chunk_size = 50.

:- func type_num_skip = int.
type_num_skip = 10000.

:- pred interleave(list(T)::in, list(T)::out, list(T)::out) is det.
:- pred interleave_2(list(T)::in, list(T)::in, list(T)::out,
    list(T)::in, list(T)::out) is det.

interleave(L, reverse(As), reverse(Bs)) :-
    interleave_2(L, [], As, [], Bs).

interleave_2([], !As, !Bs).
interleave_2([H | T], As0, As, Bs0, Bs) :-
    interleave_2(T, Bs0, Bs, [H | As0], As).

%-----------------------------------------------------------------------------%

:- pred generate_pred_code_seq(module_info::in, bool::in, bool::in,
    const_struct_map::in, pred_id::in,
    cord(c_procedure)::in, cord(c_procedure)::out,
    global_data::in, global_data::out) is det.

generate_pred_code_seq(ModuleInfo, VeryVerbose, Statistics, ConstStructMap,
        PredId, !CProcsCord, !GlobalData) :-
    % Note that some of the logic of generate_maybe_pred_code is duplicated
    % by mercury_compile.backend_pass_by_preds, so modifications here may
    % also need to be repeated there.

    module_info_get_preds(ModuleInfo, PredInfos),
    map.lookup(PredInfos, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    (
        ProcIds = []
    ;
        ProcIds = [_ | _],
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                io.write_string("% Generating code for ", !IO),
                write_pred_id(ModuleInfo, PredId, !IO),
                io.write_string("\n", !IO)
            ),
            generate_pred_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
                ProcIds, !CProcsCord, !GlobalData),
            trace [io(!IO)] (
                maybe_report_stats(Statistics, !IO)
            )
        ;
            VeryVerbose = no,
            generate_pred_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
                ProcIds, !CProcsCord, !GlobalData)
        )
    ).

:- pred generate_pred_code_par(module_info::in, const_struct_map::in,
    pred_id::in, cord(c_procedure)::in, cord(c_procedure)::out,
    global_data::in, global_data::out) is det.

generate_pred_code_par(ModuleInfo, ConstStructMap, PredId,
        !CProcsCord, !GlobalData) :-
    module_info_get_preds(ModuleInfo, PredInfos),
    map.lookup(PredInfos, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    generate_pred_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
        ProcIds, !CProcsCord, !GlobalData).

    % Translate a HLDS predicate to LLDS.
    %
:- pred generate_pred_code(module_info::in, const_struct_map::in,
    pred_id::in, pred_info::in, list(proc_id)::in,
    cord(c_procedure)::in, cord(c_procedure)::out,
    global_data::in, global_data::out) is det.

generate_pred_code(ModuleInfo, ConstStructMap, PredId, PredInfo, ProcIds,
        !CProcsCord, !GlobalData) :-
    generate_proc_list_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
        ProcIds, !CProcsCord, !GlobalData).

    % Translate all the procedures of a HLDS predicate to LLDS.
    %
:- pred generate_proc_list_code(module_info::in, const_struct_map::in,
    pred_id::in, pred_info::in, list(proc_id)::in,
    cord(c_procedure)::in, cord(c_procedure)::out,
    global_data::in, global_data::out) is det.

generate_proc_list_code(_, _, _, _, [], !CProcsCord, !GlobalData).
generate_proc_list_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
        [ProcId | ProcIds], !CProcsCord, !GlobalData) :-
    pred_info_get_proc_table(PredInfo, ProcInfos),
    map.lookup(ProcInfos, ProcId, ProcInfo),
    generate_proc_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
        ProcId, ProcInfo, CProc, !GlobalData),
    !:CProcsCord = cord.snoc(!.CProcsCord, CProc),
    generate_proc_list_code(ModuleInfo, ConstStructMap, PredId, PredInfo,
        ProcIds, !CProcsCord, !GlobalData).

%---------------------------------------------------------------------------%

    % A value of this type holds information about a procedure's stack frame.
    % It is generated when generating a procedure's prologue, and is used both
    % when generating the procedure's epilogue, and when massaging its code
    % to add the slot containing the succip to the sets of lvals live across
    % calls.
    %
:- type proc_frame_slots
    --->    proc_frame_slots(
                % Number of slots in frame.
                int,

                % Slot number of succip if succip is present in a general slot.
                maybe(int)
            ).

%---------------------------------------------------------------------------%

generate_proc_code(ModuleInfo0, ConstStructMap, PredId, PredInfo,
        ProcId, ProcInfo0, CProc, !GlobalData) :-
    % The modified module_info and proc_info are both discarded
    % on return from generate_proc_code.
    maybe_set_trace_level(PredInfo, ModuleInfo0, ModuleInfo),
    ensure_all_headvars_are_named(ProcInfo0, ProcInfo1),

    module_info_get_globals(ModuleInfo, Globals),
    globals.get_trace_level(Globals, TraceLevel),
    proc_info_get_has_parallel_conj(ProcInfo1, HasParConj),
    globals.lookup_bool_option(Globals, parallel, Parallel),
    ( if
        % Make the containing goal map available if we need it.
        % It is needed for execution tracing, and for parallel conjunctions.
        ( given_trace_level_is_none(TraceLevel) = no
        ; HasParConj = has_parallel_conj
        )
    then
        (
            HasParConj = has_parallel_conj,
            % In sequential grades, any parallel conjunctions should have been
            % converted to sequential conjunctions by parallel_to_plain_conj.m.
            expect(unify(Parallel, yes), $module, $pred,
                "found parallel conjunction in non-parallel grade")
        ;
            HasParConj = has_no_parallel_conj
        ),
        fill_goal_id_slots_in_proc(ModuleInfo, ContainingGoalMap,
            ProcInfo1, ProcInfo),
        MaybeContainingGoalMap = yes(ContainingGoalMap)
    else
        MaybeContainingGoalMap = no,
        ProcInfo = ProcInfo1
    ),

    % Find out the appropriate context for the predicate's interface events.
    pred_info_get_clauses_info(PredInfo, ClausesInfo),
    ( if get_first_clause(ClausesInfo ^ cli_rep, FirstClause) then
        ProcContext = FirstClause ^ clause_context
    else
        % This predicate must have been created by the compiler. In that case,
        % the context of the body goal is the best we can do.
        ProcContext = goal_info_get_context(GoalInfo)
    ),

    proc_info_interface_determinism(ProcInfo, Detism),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(_, GoalInfo),
    goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
    (
        MaybeFollowVars = yes(FollowVars)
    ;
        MaybeFollowVars = no,
        map.init(FollowVarsMap),
        FollowVars = abs_follow_vars(FollowVarsMap, 1, 1)
    ),
    basic_stack_layout_for_proc(PredInfo, Globals, BasicStackLayout,
        ForceProcId),
    SaveSuccip = BasicStackLayout,

    % Initialise the code_info structure. Generate_category_code below will use
    % the returned OutsideResumePoint as the entry to the code that handles
    % the failure of the procedure, if such code is needed. It is never needed
    % for model_det procedures, always needed for model_semi procedures, and
    % needed for model_non procedures only if we are doing execution tracing.
    global_data_get_static_cell_info(!.GlobalData, StaticCellInfo0),
    global_data_get_threadscope_rev_string_table(!.GlobalData,
        TSRevStringTable0, TSStringTableSize0),

    code_info_init(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        SaveSuccip, StaticCellInfo0, ConstStructMap, MaybeContainingGoalMap,
        TSRevStringTable0, TSStringTableSize0, TraceSlotInfo, CodeInfo0),
    code_loc_dep_init(FollowVars, OutsideResumePoint,
        CodeInfo0, CodeInfo1, CodeLocDep0),

    % Generate code for the procedure.
    generate_category_code(CodeModel, ProcContext, Goal, OutsideResumePoint,
        TraceSlotInfo, CodeTree0, MaybeTraceCallLabel, ProcFrameSlots,
        CodeInfo1, CodeInfo, CodeLocDep0),
    get_out_of_line_code(CodeInfo, OutOfLineCode),
    CodeTree = CodeTree0 ++ OutOfLineCode,
    get_max_regs_in_use_at_trace(CodeInfo, MaxTraceRegR, MaxTraceRegF),
    get_static_cell_info(CodeInfo, StaticCellInfo),
    global_data_set_static_cell_info(StaticCellInfo, !GlobalData),

    get_threadscope_rev_string_table(CodeInfo,
        TSRevStringTable, TSStringTableSize),
    global_data_set_threadscope_rev_string_table(TSRevStringTable,
        TSStringTableSize, !GlobalData),

    get_created_temp_frame(CodeInfo, CreatedTempFrame),
    get_proc_trace_events(CodeInfo, ProcTraceEvents),
    % You can have user trace events even if the effective trace level is none.
    ( if
        ProcTraceEvents = yes,
        CreatedTempFrame = yes,
        CodeModel \= model_non
    then
        % If tracing is enabled, the procedure lives on the det stack and the
        % code created any temporary nondet stack frames, then we must have
        % reserved a stack slot for storing the value of maxfr; if we didn't,
        % a retry command in the debugger from a point in the middle of this
        % procedure will do the wrong thing.
        proc_info_get_needs_maxfr_slot(ProcInfo, NeedsMaxfrSlot),
        expect(unify(NeedsMaxfrSlot, needs_maxfr_slot), $module, $pred,
            "should have reserved a slot for maxfr, but didn't")
    else
        true
    ),

    Instructions0 = cord.list(CodeTree),
    ProcFrameSlots = proc_frame_slots(TotalSlots, MaybeSuccipSlot),
    (
        MaybeSuccipSlot = yes(SuccipSlot),
        % The set of recorded live values at calls (for value numbering)
        % and returns (for accurate gc and execution tracing) do not yet record
        % the stack slot holding the succip, so add it to those sets.
        add_saved_succip(Instructions0, SuccipSlot, Instructions)
    ;
        MaybeSuccipSlot = no,
        Instructions = Instructions0
    ),

    proc_info_get_maybe_proc_table_io_info(ProcInfo, MaybeTableIOInfo),
    ( if
        ( BasicStackLayout = yes
        ; MaybeTableIOInfo = yes(_TableIODeclInfo)
        )
    then
        % Create the procedure layout structure.
        RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
        get_layout_info(CodeInfo, InternalMap),
        EntryLabel = make_local_entry_label(ModuleInfo, PredId, ProcId, no),
        proc_info_get_eval_method(ProcInfo, EvalMethod),
        proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap0),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_varset(ProcInfo, VarSet),
        proc_info_get_argmodes(ProcInfo, ArgModes),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        globals.get_trace_suppress(Globals, TraceSuppress),
        NeedBodyReps = eff_trace_needs_proc_body_reps(ModuleInfo,
            PredInfo, ProcInfo, TraceLevel, TraceSuppress),
        (
            NeedBodyReps = yes,
            NeedGoalRep = trace_needs_body_rep
        ;
            NeedBodyReps = no,
            NeedGoalRep = trace_does_not_need_body_rep
        ),
        NeedsAllNames = eff_trace_needs_all_var_names(ModuleInfo, PredInfo,
            ProcInfo, TraceLevel, TraceSuppress),
        proc_info_get_maybe_deep_profile_info(ProcInfo, MaybeHLDSDeepInfo),
        (
            MaybeHLDSDeepInfo = yes(HLDSDeepInfo),
            MaybeDeepProfInfo =
                maybe_generate_deep_prof_info(ProcInfo, HLDSDeepInfo)
        ;
            MaybeHLDSDeepInfo = no,
            MaybeDeepProfInfo = no
        ),
        EffTraceLevel = eff_trace_level(ModuleInfo, PredInfo, ProcInfo,
            TraceLevel),
        module_info_get_table_struct_map(ModuleInfo, TableStructMap),
        PredProcId = proc(PredId, ProcId),
        (
            MaybeTableIOInfo = no,
            ( if map.search(TableStructMap, PredProcId, TableStructInfo) then
                TableStructInfo = table_struct_info(ProcTableStructInfo,
                    _Attributes),
                MaybeTableInfo = yes(proc_table_struct(ProcTableStructInfo))
            else
                MaybeTableInfo = no
            )
        ;
            MaybeTableIOInfo = yes(TableIOInfo),
            ( if map.search(TableStructMap, PredProcId, _TableStructInfo) then
                unexpected($module, $pred, "conflicting kinds of tabling")
            else
                MaybeTableInfo = yes(proc_table_io_entry(TableIOInfo))
            )
        ),
        proc_info_get_oisu_kind_fors(ProcInfo, OISUKindFors),
        ProcLayout = proc_layout_info(RttiProcLabel, EntryLabel,
            Detism, TotalSlots, MaybeSuccipSlot, EvalMethod,
            EffTraceLevel, MaybeTraceCallLabel, MaxTraceRegR, MaxTraceRegF,
            HeadVars, ArgModes, Goal, NeedGoalRep, InstMap0,
            TraceSlotInfo, ForceProcId, VarSet, VarTypes,
            InternalMap, MaybeTableInfo, NeedsAllNames, OISUKindFors,
            MaybeDeepProfInfo),
        global_data_add_new_proc_layout(proc(PredId, ProcId), ProcLayout,
            !GlobalData)
    else
        true
    ),

    get_closure_layouts(CodeInfo, ClosureLayouts),
    global_data_add_new_closure_layouts(ClosureLayouts, !GlobalData),
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),

    get_alloc_sites(CodeInfo, AllocSites),
    global_data_add_new_alloc_sites(AllocSites, !GlobalData),

    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),

    get_label_counter(CodeInfo, LabelCounter),
    % You can have user trace events even if the effective trace level is none.
    (
        ProcTraceEvents = no,
        MayAlterRtti = may_alter_rtti
    ;
        ProcTraceEvents = yes,
        MayAlterRtti = must_not_alter_rtti
    ),

    globals.lookup_bool_option(Globals, generate_bytecode, GenBytecode),
    ( if
        % XXX: There is a mass of calls above that the bytecode doesn't need;
        % work out which is and isn't needed and put inside the else case
        % below.
        GenBytecode = yes,
        % We don't generate bytecode for unify and compare preds.
        % The automatically generated unify and compare predicates
        % are correct by construction; for user-defined unify and
        % compare predicates, we *assume* their correctness for now
        % (perhaps not wisely).
        not is_unify_or_compare_pred(PredInfo),
        % Don't generate bytecode for procs with foreign code.
        goal_has_foreign(Goal) = no
    then
        bytecode_stub(ModuleInfo, PredId, ProcId, ProcInstructions),
        ProcLabelCounter = counter.init(0)
    else
        ProcInstructions = Instructions,
        ProcLabelCounter = LabelCounter
    ),
    get_used_env_vars(CodeInfo, UsedEnvVars),
    CProc = c_procedure(Name, Arity, proc(PredId, ProcId), ProcLabel,
        CodeModel, ProcInstructions, ProcLabelCounter, MayAlterRtti,
        UsedEnvVars).

:- pred maybe_set_trace_level(pred_info::in,
    module_info::in, module_info::out) is det.

maybe_set_trace_level(PredInfo, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals0),
    ( if
        PredModule = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        PredArity = pred_info_orig_arity(PredInfo),
        no_type_info_builtin(PredModule, PredName, PredArity)
    then
        % These predicates should never be traced, since they do not obey
        % typeinfo_liveness. Since they may be opt_imported into other
        % modules, we must switch off the tracing of such preds on a
        % pred-by-pred basis.
        globals.set_trace_level_none(Globals0, Globals1),
        module_info_set_globals(Globals1, !ModuleInfo)
    else if
        pred_info_get_origin(PredInfo, origin_special_pred(_, _))
    then
        globals.get_trace_level(Globals0, TraceLevel),
        UC_TraceLevel = trace_level_for_unify_compare(TraceLevel),
        globals.set_trace_level(UC_TraceLevel, Globals0, Globals1),
        module_info_set_globals(Globals1, !ModuleInfo)
    else
        true
    ).

:- func maybe_generate_deep_prof_info(proc_info, deep_profile_proc_info)
    = maybe(proc_deep_prof_info).

maybe_generate_deep_prof_info(ProcInfo, HLDSDeepInfo) = MaybeDeepProfInfo :-
    HLDSDeepInfo ^ deep_layout = MaybeHLDSDeepLayout,
    (
        MaybeHLDSDeepLayout = yes(HLDSDeepLayout),
        HLDSDeepLayout = hlds_deep_layout(HLDSProcStatic, HLDSExcpVars),
        HLDSDeepInfo ^ deep_orig_body = OriginalProcBody,
        HLDSExcpVars = hlds_deep_excp_vars(TopCSDVar, MiddleCSDVar,
            MaybeOldOutermostVar),
        proc_info_get_stack_slots(ProcInfo, StackSlots),
        ( if map.search(StackSlots, TopCSDVar, TopCSDSlot) then
            TopCSDSlotNum = stack_slot_num(TopCSDSlot),
            map.lookup(StackSlots, MiddleCSDVar, MiddleCSDSlot),
            MiddleCSDSlotNum = stack_slot_num(MiddleCSDSlot),
            (
                MaybeOldOutermostVar = yes(OldOutermostVar),
                map.lookup(StackSlots, OldOutermostVar, OldOutermostSlot),
                OldOutermostSlotNum = stack_slot_num(OldOutermostSlot)
            ;
                MaybeOldOutermostVar = no,
                OldOutermostSlotNum = -1
            )
        else
            TopCSDSlotNum = -1,
            MiddleCSDSlotNum = -1,
            OldOutermostSlotNum = -1
        ),
        DeepExcpSlots = deep_excp_slots(TopCSDSlotNum, MiddleCSDSlotNum,
            OldOutermostSlotNum),
        DeepProfInfo = proc_deep_prof_info(HLDSProcStatic, DeepExcpSlots,
            OriginalProcBody),
        MaybeDeepProfInfo = yes(DeepProfInfo)
    ;
        MaybeHLDSDeepLayout = no,
        MaybeDeepProfInfo = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Generate_category_code generates code for an entire procedure.
    % Its algorithm has three or four main stages:
    %
    %   - generate code for the body goal
    %   - generate code for the procedure entry
    %   - generate code for the procedure exit
    %   - generate code for the procedure fail (if needed)
    %
    % The first three tasks are forwarded to other procedures.
    % The fourth task, if needed, is done by generate_category_code.
    %
    % The only caller of generate_category_code, generate_proc_code,
    % has set up the code generator state to reflect what the machine
    % state will be on entry to the procedure. Ensuring that the
    % machine state at exit will conform to the expectation
    % of the caller is the job of generate_exit.
    %
    % The reason why we generate the entry code after the body is that
    % information such as the total number of stack slots needed,
    % which is needed in the procedure entry prologue, cannot be
    % conveniently obtained before generating the body, since the
    % code generator may allocate temporary variables to hold values
    % such as saved heap and trail pointers.
    %
    % Code_gen.generate_entry cannot depend on the code generator
    % state, since when it is invoked this state is not appropriate
    % for the procedure entry. Nor can it change the code generator state,
    % since that would confuse generate_exit.
    %
    % Generating CALL trace events is done by generate_category_code,
    % since only on entry to generate_category_code is the code generator
    % state set up right. Generating EXIT trace events is done by
    % generate_exit. Generating FAIL trace events is done
    % by generate_category_code, since this requires modifying how
    % we generate code for the body of the procedure (failures must
    % now branch to a different place). Since FAIL trace events are
    % part of the failure continuation, generate_category_code takes
    % care of the failure continuation as well. (Model_det procedures
    % of course have no failure continuation. Model_non procedures have
    % a failure continuation, but in the absence of tracing this
    % continuation needs no code. Only model_semi procedures need code
    % for the failure continuation at all times.)
    %
:- pred generate_category_code(code_model::in, prog_context::in, hlds_goal::in,
    resume_point_info::in, trace_slot_info::in, llds_code::out,
    maybe(label)::out, proc_frame_slots::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_category_code(CodeModel, ProcContext, Goal, ResumePoint,
        TraceSlotInfo, Code, MaybeTraceCallLabel, ProcFrameSlots,
        !CI, !.CLD) :-
    (
        CodeModel = model_det,
        % Generate the code for the body of the procedure.
        get_globals(!.CI, Globals),
        globals.lookup_bool_option(Globals, middle_rec, MiddleRec),
        ( if
            MiddleRec = yes,
            middle_rec.match_and_generate(Goal, MiddleRecCode, !CI, !CLD)
        then
            Code = MiddleRecCode,
            MaybeTraceCallLabel = no,
            ProcFrameSlots = proc_frame_slots(0, no)
        else
            get_maybe_trace_info(!.CI, MaybeTraceInfo),
            (
                MaybeTraceInfo = yes(TraceInfo),
                generate_call_event(TraceInfo, ProcContext,
                    MaybeTraceCallLabel, TraceCallCode, !CI, !CLD),
                get_trace_maybe_tail_rec_info(TraceInfo, MaybeTailRecInfo),
                (
                    MaybeTailRecInfo = yes(_TailRecLval - TailRecLabel),
                    TailRecLabelCode = singleton(
                        llds_instr(label(TailRecLabel),
                            "tail recursion label, nofulljump")
                    )
                ;
                    MaybeTailRecInfo = no,
                    TailRecLabelCode = empty
                )
            ;
                MaybeTraceInfo = no,
                MaybeTraceCallLabel = no,
                TraceCallCode = empty,
                TailRecLabelCode = empty
            ),
            generate_goal(model_det, Goal, BodyCode, !CI, !CLD),
            generate_entry(!.CI, model_det, Goal, ResumePoint, ProcFrameSlots,
                EntryCode),
            generate_exit(model_det, ProcFrameSlots, TraceSlotInfo,
                ProcContext, _, ExitCode, !CI, !.CLD),
            Code = EntryCode ++ TraceCallCode ++ TailRecLabelCode ++
                BodyCode ++ ExitCode
        )
    ;
        CodeModel = model_semi,
        FailureLiveRegs = set.make_singleton_set(reg(reg_r, 1)),
        FailCode = from_list([
            llds_instr(assign(reg(reg_r, 1), const(llconst_false)), "Fail"),
            llds_instr(livevals(FailureLiveRegs), ""),
            llds_instr(goto(code_succip), "Return from procedure call")
        ]),
        get_maybe_trace_info(!.CI, MaybeTraceInfo),
        (
            MaybeTraceInfo = yes(TraceInfo),
            remember_position(!.CLD, BeforeBody),
            generate_call_event(TraceInfo, ProcContext, MaybeTraceCallLabel,
                TraceCallCode, !CI, !CLD),
            get_trace_maybe_tail_rec_info(TraceInfo, MaybeTailRecInfo),
            (
                MaybeTailRecInfo = yes(_TailRecLval - TailRecLabel),
                TailRecLabelCode = singleton(
                    llds_instr(label(TailRecLabel), "tail recursion label")
                )
            ;
                MaybeTailRecInfo = no,
                TailRecLabelCode = empty
            ),
            generate_goal(model_semi, Goal, BodyCode, !CI, !CLD),
            generate_entry(!.CI, model_semi, Goal, ResumePoint,
                ProcFrameSlots, EntryCode),
            generate_exit(model_semi, ProcFrameSlots, TraceSlotInfo,
                ProcContext, RestoreDeallocCode, ExitCode, !CI, !.CLD),

            reset_to_position(BeforeBody, !.CI, !:CLD),
            generate_resume_point(ResumePoint, ResumeCode, !CI, !CLD),
            resume_point_vars(ResumePoint, ResumeVarList),
            ResumeVars = set_of_var.list_to_set(ResumeVarList),
            set_forward_live_vars(ResumeVars, !CLD),
            % XXX A context that gives the end of the procedure definition
            % would be better than ProcContext.
            generate_external_event_code(external_port_fail, TraceInfo,
                ProcContext, MaybeFailExternalInfo,
                !CI, !.CLD, _CLDAfterEvent),
            (
                MaybeFailExternalInfo = yes(FailExternalInfo),
                FailExternalInfo = external_event_info(_, _, TraceFailCode)
            ;
                MaybeFailExternalInfo = no,
                TraceFailCode = empty
            ),
            Code = EntryCode ++ TraceCallCode ++ TailRecLabelCode ++
                BodyCode ++ ExitCode ++ ResumeCode ++ TraceFailCode ++
                RestoreDeallocCode ++ FailCode
        ;
            MaybeTraceInfo = no,
            MaybeTraceCallLabel = no,
            remember_position(!.CLD, BeforeBody),
            generate_goal(model_semi, Goal, BodyCode, !CI, !CLD),
            generate_entry(!.CI, model_semi, Goal, ResumePoint,
                ProcFrameSlots, EntryCode),
            generate_exit(model_semi, ProcFrameSlots, TraceSlotInfo,
                ProcContext, RestoreDeallocCode, ExitCode, !CI, !.CLD),
            reset_to_position(BeforeBody, !.CI, !:CLD),
            generate_resume_point(ResumePoint, ResumeCode,
                !CI, !.CLD, _CLDAfterResume),
            Code = EntryCode ++ BodyCode ++ ExitCode ++ ResumeCode ++
                RestoreDeallocCode ++ FailCode
        )
    ;
        CodeModel = model_non,
        get_maybe_trace_info(!.CI, MaybeTraceInfo),
        (
            MaybeTraceInfo = yes(TraceInfo),
            generate_call_event(TraceInfo, ProcContext, MaybeTraceCallLabel,
                TraceCallCode, !CI, !CLD),
            get_trace_maybe_tail_rec_info(TraceInfo, MaybeTailRecInfo),
            expect(unify(MaybeTailRecInfo, no), $module, $pred,
                "tail recursive call in model_non code"),
            remember_position(!.CLD, BeforeBody),
            generate_goal(model_non, Goal, BodyCode, !CI, !CLD),
            generate_entry(!.CI, model_non, Goal, ResumePoint,
                ProcFrameSlots, EntryCode),
            generate_exit(model_non, ProcFrameSlots, TraceSlotInfo,
                ProcContext, _, ExitCode, !CI, !.CLD),

            reset_to_position(BeforeBody, !.CI, !:CLD),
            generate_resume_point(ResumePoint, ResumeCode, !CI, !CLD),
            resume_point_vars(ResumePoint, ResumeVarList),
            ResumeVars = set_of_var.list_to_set(ResumeVarList),
            set_forward_live_vars(ResumeVars, !CLD),
            % XXX A context that gives the end of the procedure definition
            % would be better than ProcContext.
            generate_external_event_code(external_port_fail, TraceInfo,
                ProcContext, MaybeFailExternalInfo,
                !CI, !.CLD, _CLDAfterEvent),
            (
                MaybeFailExternalInfo = yes(FailExternalInfo),
                FailExternalInfo = external_event_info(_, _, TraceFailCode)
            ;
                MaybeFailExternalInfo = no,
                TraceFailCode = empty
            ),
            MaybeTrailSlot = TraceSlotInfo ^ slot_trail,
            (
                MaybeTrailSlot = yes(_),
                MaybeFromFull = TraceSlotInfo ^ slot_from_full,
                (
                    MaybeFromFull = yes(FromFullSlot),
                    % Generate code which discards the ticket only if it was
                    % allocated, i.e. only if MR_trace_from_full was true
                    % on entry.
                    FromFullSlotLval =
                        llds.stack_slot_num_to_lval(nondet_stack,
                            FromFullSlot),
                    get_next_label(SkipLabel, !CI),
                    DiscardTraceTicketCode = from_list([
                        llds_instr(
                            if_val(unop(logical_not, lval(FromFullSlotLval)),
                                code_label(SkipLabel)), ""),
                        llds_instr(discard_ticket, "discard retry ticket"),
                        llds_instr(label(SkipLabel), "")
                    ])
                ;
                    MaybeFromFull = no,
                    DiscardTraceTicketCode = singleton(
                        llds_instr(discard_ticket, "discard retry ticket")
                    )
                )
            ;
                MaybeTrailSlot = no,
                DiscardTraceTicketCode = empty
            ),
            FailCode = singleton(
                llds_instr(goto(do_fail), "fail after fail trace port")
            ),
            Code = EntryCode ++ TraceCallCode ++ BodyCode ++ ExitCode ++
                ResumeCode ++ TraceFailCode ++ DiscardTraceTicketCode ++
                FailCode
        ;
            MaybeTraceInfo = no,
            MaybeTraceCallLabel = no,
            generate_goal(model_non, Goal, BodyCode, !CI, !CLD),
            generate_entry(!.CI, model_non, Goal, ResumePoint,
                ProcFrameSlots, EntryCode),
            generate_exit(model_non, ProcFrameSlots, TraceSlotInfo,
                ProcContext, _, ExitCode, !CI, !.CLD),
            Code = EntryCode ++ BodyCode ++ ExitCode
        )
    ).

:- pred generate_call_event(trace_info::in, prog_context::in,
    maybe(label)::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_call_event(TraceInfo, ProcContext, MaybeTraceCallLabel, TraceCallCode,
        !CI, !CLD) :-
    generate_external_event_code(external_port_call, TraceInfo,
        ProcContext, MaybeCallExternalInfo, !CI, !CLD),
    (
        MaybeCallExternalInfo = yes(CallExternalInfo),
        CallExternalInfo = external_event_info(TraceCallLabel, _,
            TraceCallCode),
        MaybeTraceCallLabel = yes(TraceCallLabel)
    ;
        MaybeCallExternalInfo = no,
        % This can happen for procedures containing user events
        % in shallow traced modules.
        TraceCallCode = empty,
        MaybeTraceCallLabel = no
    ).

%---------------------------------------------------------------------------%

    % Generate the prologue for a procedure.
    %
    % The prologue will contain
    %
    %   a comment to mark prologue start
    %   a comment explaining the stack layout
    %   the procedure entry label
    %   code to allocate a stack frame
    %   code to fill in some special slots in the stack frame
    %   a comment to mark prologue end
    %
    % At the moment the only special slots are the succip slot, and
    % the slots holding the call number and call depth for tracing.
    %
    % Not all frames will have all these components. For example, the code
    % to allocate a stack frame will be missing if the procedure doesn't
    % need a stack frame, and if the procedure is nondet, then the code
    % to fill in the succip slot is subsumed by the mkframe.

:- pred generate_entry(code_info::in, code_model::in, hlds_goal::in,
    resume_point_info::in, proc_frame_slots::out, llds_code::out) is det.

generate_entry(CI, CodeModel, Goal, OutsideResumePoint, ProcFrameSlots,
        EntryCode) :-
    get_stack_slots(CI, StackSlots),
    get_varset(CI, VarSet),
    SlotsComment = explain_stack_slots(StackSlots, VarSet),
    StartComment = from_list([
        llds_instr(comment("Start of procedure prologue"), ""),
        llds_instr(comment(SlotsComment), "")
    ]),
    get_total_stackslot_count(CI, MainSlots),
    get_pred_id(CI, PredId),
    get_proc_id(CI, ProcId),
    get_module_info(CI, ModuleInfo),
    EntryLabel = make_local_entry_label(ModuleInfo, PredId, ProcId, no),
    LabelCode = singleton(
        llds_instr(label(EntryLabel), "Procedure entry point")
    ),
    get_succip_used(CI, Used),
    ( if
        % Do we need to save the succip across calls?
        Used = yes,
        % Do we need to use a general slot for storing succip?
        CodeModel \= model_non
    then
        SuccipSlot = maybe_round_frame_size(CI, CodeModel, MainSlots + 1),
        SaveSuccipCode = singleton(
            llds_instr(assign(stackvar(SuccipSlot), lval(succip)),
                "Save the success ip")
        ),
        TotalSlots = SuccipSlot,
        MaybeSuccipSlot = yes(SuccipSlot)
    else
        SaveSuccipCode = empty,
        TotalSlots = maybe_round_frame_size(CI, CodeModel, MainSlots),
        MaybeSuccipSlot = no
    ),
    get_maybe_trace_info(CI, MaybeTraceInfo),
    (
        MaybeTraceInfo = yes(TraceInfo),
        generate_slot_fill_code(CI, TraceInfo, TraceFillCode)
    ;
        MaybeTraceInfo = no,
        TraceFillCode = empty
    ),

    PushMsg = push_msg(ModuleInfo, PredId, ProcId),
    (
        CodeModel = model_non,
        resume_point_stack_addr(OutsideResumePoint, OutsideResumeAddress),
        NondetFrameInfo = ordinary_frame(PushMsg, TotalSlots),
        AllocCode = singleton(
            llds_instr(mkframe(NondetFrameInfo, yes(OutsideResumeAddress)),
                "Allocate stack frame")
        )
    ;
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        IsLeaf = proc_body_is_leaf(Goal),
        (
            IsLeaf = is_not_leaf,
            StackIncrKind = stack_incr_nonleaf
        ;
            IsLeaf = is_leaf,
            StackIncrKind = stack_incr_leaf
        ),
        ( if TotalSlots > 0 then
            AllocCode = singleton(
                llds_instr(incr_sp(TotalSlots, PushMsg, StackIncrKind),
                    "Allocate stack frame")
            )
        else
            AllocCode = empty
        )
    ),
    ProcFrameSlots = proc_frame_slots(TotalSlots, MaybeSuccipSlot),
    EndComment = singleton(
        llds_instr(comment("End of procedure prologue"), "")
    ),
    EntryCode = StartComment ++ LabelCode ++ AllocCode ++
        SaveSuccipCode ++ TraceFillCode ++ EndComment.

:- func maybe_round_frame_size(code_info, code_model, int) = int.

maybe_round_frame_size(CI, CodeModel, NumSlots0) = NumSlots :-
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        NumSlots = round_det_stack_frame_size(CI, NumSlots0)
    ;
        CodeModel = model_non,
        NumSlots = NumSlots0
    ).

%---------------------------------------------------------------------------%

    % Generate the success epilogue for a procedure.
    %
    % The success epilogue will contain
    %
    %   a comment to mark epilogue start
    %   code to place the output arguments where their caller expects
    %   code to restore registers from some special slots
    %   code to deallocate the stack frame
    %   code to set r1 to MR_TRUE (for semidet procedures only)
    %   a jump back to the caller, including livevals information
    %   a comment to mark epilogue end
    %
    % The parts of this that restore registers and deallocate the stack
    % frame are also part of the failure epilog, which is handled by
    % our caller; this is why we return RestoreDeallocCode.
    %
    % At the moment the only special slots are the succip slot, and
    % the tracing slots (holding the call sequence number, call event
    % number, call depth, from-full indication, and trail state).
    %
    % Not all frames will have all these components. For example, for
    % nondet procedures we don't deallocate the stack frame before
    % success.
    %
    % Epilogues for procedures defined by nondet pragma C codes do not
    % follow the rules above. For such procedures, the normal functions
    % of the epilogue are handled when traversing the pragma C code goal;
    % we need only #undef a macro defined by the procedure prologue.

:- pred generate_exit(code_model::in, proc_frame_slots::in,
    trace_slot_info::in, prog_context::in, llds_code::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in) is det.

generate_exit(CodeModel, ProcFrameSlots, TraceSlotInfo, ProcContext,
        RestoreDeallocCode, ExitCode, !CI, !.CLD) :-
    StartComment = singleton(
        llds_instr(comment("Start of procedure epilogue"), "")
    ),
    EndComment = singleton(
        llds_instr(comment("End of procedure epilogue"), "")
    ),
    ProcFrameSlots = proc_frame_slots(TotalSlots, MaybeSuccipSlot),
    get_instmap(!.CLD, InstMap),
    ArgModes = get_arginfo(!.CI),
    HeadVars = get_headvars(!.CI),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, Args),
    ( if instmap_is_unreachable(InstMap) then
        OutLvals = set.init,
        FlushCode = empty
    else
        setup_return(Args, OutLvals, FlushCode, !.CI, !CLD)
    ),
    (
        MaybeSuccipSlot = yes(SuccipSlot),
        RestoreSuccipCode = singleton(
            llds_instr(assign(succip, lval(stackvar(SuccipSlot))),
                "restore the success ip")
        )
    ;
        MaybeSuccipSlot = no,
        RestoreSuccipCode = empty
    ),
    ( if
        ( TotalSlots = 0
        ; CodeModel = model_non
        )
    then
        DeallocCode = empty
    else
        DeallocCode = singleton(
           llds_instr(decr_sp(TotalSlots), "Deallocate stack frame")
        )
    ),
    ( if
        TraceSlotInfo ^ slot_trail = yes(_),
        CodeModel \= model_non
    then
        MaybeFromFull = TraceSlotInfo ^ slot_from_full,
        (
            MaybeFromFull = yes(FromFullSlot),
            % Generate code which prunes the ticket only if it was allocated,
            % i.e. only if MR_trace_from_full was true on entry.
            %
            % Note that to avoid duplicating label names, we need to generate
            % two different copies of this with different labels; this is
            % needed for semidet code, which will get one copy in the
            % success epilogue and one copy in the failure epilogue.

            StackId = code_model_to_main_stack(CodeModel),
            FromFullSlotLval =
                llds.stack_slot_num_to_lval(StackId, FromFullSlot),
            get_next_label(SkipLabel, !CI),
            get_next_label(SkipLabelCopy, !CI),
            PruneTraceTicketCode = from_list([
                llds_instr(
                    if_val(unop(logical_not, lval(FromFullSlotLval)),
                    code_label(SkipLabel)), ""),
                llds_instr(prune_ticket, "prune retry ticket"),
                llds_instr(label(SkipLabel), "")
            ]),
            PruneTraceTicketCodeCopy = from_list([
                llds_instr(
                    if_val(unop(logical_not, lval(FromFullSlotLval)),
                    code_label(SkipLabelCopy)), ""),
                llds_instr(prune_ticket, "prune retry ticket"),
                llds_instr(label(SkipLabelCopy), "")
            ])
        ;
            MaybeFromFull = no,
            PruneTraceTicketCode = singleton(
                llds_instr(prune_ticket, "prune retry ticket")
            ),
            PruneTraceTicketCodeCopy = PruneTraceTicketCode
        )
    else
        PruneTraceTicketCode = empty,
        PruneTraceTicketCodeCopy = empty
    ),

    RestoreDeallocCode = RestoreSuccipCode ++
        PruneTraceTicketCode ++ DeallocCode,
    RestoreDeallocCodeCopy = RestoreSuccipCode ++
        PruneTraceTicketCodeCopy ++ DeallocCode,

    get_maybe_trace_info(!.CI, MaybeTraceInfo),
    (
        MaybeTraceInfo = yes(TraceInfo),
        % XXX A context that gives the end of the procedure definition
        % would be better than CallContext.
        generate_external_event_code(external_port_exit, TraceInfo,
            ProcContext, MaybeExitExternalInfo, !CI, !.CLD, _CLDAfterExit),
        (
            MaybeExitExternalInfo = yes(ExitExternalInfo),
            ExitExternalInfo = external_event_info(_, TypeInfoDatas,
                TraceExitCode),
            map.foldl(add_type_info_lvals, TypeInfoDatas, OutLvals, LiveLvals)
        ;
            MaybeExitExternalInfo = no,
            LiveLvals = OutLvals,
            TraceExitCode = empty
        )
    ;
        MaybeTraceInfo = no,
        TraceExitCode = empty,
        LiveLvals = OutLvals
    ),

    get_proc_info(!.CI, ProcInfo),
    proc_info_get_maybe_special_return(ProcInfo, MaybeSpecialReturn),
    (
        CodeModel = model_det,
        expect(unify(MaybeSpecialReturn, no), $module, $pred,
            "det special_return"),
        SuccessCode = from_list([
            llds_instr(livevals(LiveLvals), ""),
            llds_instr(goto(code_succip), "Return from procedure call")
        ]),
        AllSuccessCode = TraceExitCode ++ RestoreDeallocCodeCopy ++ SuccessCode
    ;
        CodeModel = model_semi,
        expect(unify(MaybeSpecialReturn, no), $module, $pred,
            "semi special_return"),
        set.insert(reg(reg_r, 1), LiveLvals, SuccessLiveRegs),
        SuccessCode = from_list([
            llds_instr(assign(reg(reg_r, 1), const(llconst_true)),
                "Succeed"),
            llds_instr(livevals(SuccessLiveRegs), ""),
            llds_instr(goto(code_succip), "Return from procedure call")
        ]),
        AllSuccessCode = TraceExitCode ++ RestoreDeallocCodeCopy ++ SuccessCode
    ;
        CodeModel = model_non,
        (
            MaybeTraceInfo = yes(TraceInfo2),
            maybe_setup_redo_event(TraceInfo2, SetupRedoCode)
        ;
            MaybeTraceInfo = no,
            SetupRedoCode = empty
        ),
        (
            MaybeSpecialReturn = yes(SpecialReturn),
            SpecialReturn = generator_return(GeneratorLocnStr, DebugStr),
            ReturnMacroName = "MR_tbl_mmos_return_answer",
            ReturnCodeStr = "\t" ++ ReturnMacroName ++ "(" ++
                DebugStr ++ ", " ++ GeneratorLocnStr ++ ");\n",
            Component = foreign_proc_user_code(no,
                proc_does_not_affect_liveness, ReturnCodeStr),
            MD = proc_may_not_duplicate,
            SuccessCode = from_list([
                llds_instr(livevals(LiveLvals), ""),
                llds_instr(foreign_proc_code([], [Component],
                    proc_may_call_mercury, no, no, no, no, no, no, MD), "")
            ])
        ;
            MaybeSpecialReturn = no,
            SuccessCode = from_list([
                llds_instr(livevals(LiveLvals), ""),
                llds_instr(goto(do_succeed(no)),
                    "Return from procedure call")
            ])
        ),
        AllSuccessCode = SetupRedoCode ++ TraceExitCode ++ SuccessCode
    ),
    ExitCode = StartComment ++ FlushCode ++ AllSuccessCode ++ EndComment.

:- pred add_type_info_lvals(tvar::in, set(layout_locn)::in,
    set(lval)::in, set(lval)::out) is det.

add_type_info_lvals(_TVar, TypeInfoLocnSets, !LiveLvals) :-
    TypeInfoLvals = set.map(project_layout_locn_lval, TypeInfoLocnSets),
    set.union(TypeInfoLvals, !LiveLvals).

:- func project_layout_locn_lval(layout_locn) = lval.

project_layout_locn_lval(locn_direct(Lval)) = Lval.
project_layout_locn_lval(locn_indirect(Lval, _)) = Lval.

%---------------------------------------------------------------------------%

    % Add the succip to the livevals before and after calls. Traverses the list
    % of instructions looking for livevals and calls, adding succip in the
    % stackvar number given as an argument.
    %
:- pred add_saved_succip(list(instruction)::in, int::in,
    list(instruction)::out) is det.

add_saved_succip([], _StackLoc, []).
add_saved_succip([Instr0 | Instrs0], StackLoc, [Instr | Instrs]) :-
    Instr0 = llds_instr(Uinstr0, Comment),
    ( if
        Uinstr0 = livevals(LiveVals0),
        Instrs0 \= [llds_instr(goto(code_succip), _) | _]
        % XXX We should also test for tailcalls
        % once we start generating them directly.
    then
        set.insert(stackvar(StackLoc), LiveVals0, LiveVals1),
        Uinstr = livevals(LiveVals1),
        Instr = llds_instr(Uinstr, Comment)
    else if
        Uinstr0 = llcall(Target, ReturnLabel, LiveVals0, Context, GP, CM)
    then
        map.init(Empty),
        LiveVals = [live_lvalue(locn_direct(stackvar(StackLoc)),
            live_value_succip, Empty) | LiveVals0],
        Uinstr = llcall(Target, ReturnLabel, LiveVals, Context, GP, CM),
        Instr = llds_instr(Uinstr, Comment)
    else
        Instr = Instr0
    ),
    add_saved_succip(Instrs0, StackLoc, Instrs).

%---------------------------------------------------------------------------%

:- pred bytecode_stub(module_info::in, pred_id::in, proc_id::in,
    list(instruction)::out) is det.

bytecode_stub(ModuleInfo, PredId, ProcId, BytecodeInstructions) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleSymName = pred_info_module(PredInfo),

    ModuleName = sym_name_to_string_sep(ModuleSymName, "__"),

    EntryLabel = make_local_entry_label(ModuleInfo, PredId, ProcId, no),

    PredName = pred_info_name(PredInfo),
    proc_id_to_int(ProcId, ProcNum),
    string.int_to_string(ProcNum, ProcStr),
    Arity = pred_info_orig_arity(PredInfo),
    int_to_string(Arity, ArityStr),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),

    CallStructName = "bytecode_call_info",

    (
        PredOrFunc = pf_function,
        IsFuncStr = "MR_TRUE"
    ;
        PredOrFunc = pf_predicate,
        IsFuncStr = "MR_FALSE"
    ),
    string.append_list([
        "\t\tstatic MB_Call ", CallStructName, " = {\n",
        "\t\t\t(MB_Word)NULL,\n",
        "\t\t\t""", ModuleName, """,\n",
        "\t\t\t""", PredName, """,\n",
        "\t\t\t", ProcStr, ",\n",
        "\t\t\t", ArityStr, ",\n",
        "\t\t\t", IsFuncStr, "\n",
        "\t\t};\n"
        ], CallStruct),

    string.append_list([
        "\t\tMB_Native_Addr return_addr;\n",
        "\t\tMR_save_registers();\n",
        "\t\treturn_addr = MB_bytecode_call_entry(", "&",CallStructName,");\n",
        "\t\tMR_restore_registers();\n",
        "\t\tMR_GOTO(return_addr);\n"
        ], BytecodeCall),

    BytecodeInstructionsComponents = [
        foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), "\t{\n"),
        foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init),
            CallStruct),
        foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, no_live_lvals_info, BytecodeCall),
        foreign_proc_raw_code(cannot_branch_away,
            proc_does_not_affect_liveness, live_lvals_info(set.init), "\t}\n")
    ],

    MD = proc_may_not_duplicate,
    BytecodeInstructions = [
        llds_instr(label(EntryLabel), "Procedure entry point"),
        llds_instr(foreign_proc_code([], BytecodeInstructionsComponents,
            proc_may_call_mercury, no, no, no, no, no, no, MD), "Entry stub")
    ].

%---------------------------------------------------------------------------%

push_msg(ModuleInfo, PredId, ProcId) = PushMsg :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PushMsg = describe_proc(PredInfo, ProcId).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_all_tabling_info_structs(ModuleInfo, !GlobalData) :-
    module_info_get_table_struct_map(ModuleInfo, TableStructMap),
    map.to_assoc_list(TableStructMap, TableStructs),
    list.foldl(add_tabling_info_struct, TableStructs, !GlobalData).

:- pred add_tabling_info_struct(pair(pred_proc_id, table_struct_info)::in,
    global_data::in, global_data::out) is det.

add_tabling_info_struct(PredProcId - TableStructInfo, !GlobalData) :-
    TableStructInfo = table_struct_info(ProcTableStructInfo, TableAttributes),
    ProcTableStructInfo = proc_table_struct_info(RttiProcLabel, _TVarSet,
        _Context, NumInputs, NumOutputs, InputSteps, MaybeOutputSteps,
        ArgInfos, EvalMethod),

    global_data_get_static_cell_info(!.GlobalData, StaticCellInfo0),
    convert_table_arg_info(ArgInfos, NumPTIs, PTIVectorRval,
        TVarVectorRval, StaticCellInfo0, StaticCellInfo),
    global_data_set_static_cell_info(StaticCellInfo, !GlobalData),
    NumArgs = NumInputs + NumOutputs,
    expect(unify(NumArgs, NumPTIs), $module, $pred, "args mismatch"),

    MaybeSizeLimit = TableAttributes ^ table_attr_size_limit,
    Statistics = TableAttributes ^ table_attr_statistics,
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    Var = tabling_info_struct(ProcLabel, EvalMethod,
        NumInputs, NumOutputs, InputSteps, MaybeOutputSteps, PTIVectorRval,
        TVarVectorRval, MaybeSizeLimit, Statistics),
    global_data_add_new_proc_var(PredProcId, Var, !GlobalData).

%---------------------------------------------------------------------------%
:- end_module ll_backend.proc_gen.
%---------------------------------------------------------------------------%
