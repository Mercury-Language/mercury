%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: table_gen.m.
% Main authors: zs, ohutch.
%
% This module transforms HLDS code to implement loop detection, memoing,
% minimal model evaluation, or I/O idempotence. The transformation involves
% adding calls to predicates defined in library/table_builtin.m and in
% runtime/mercury_minimal_model.c.
%
% The loop detection transformation adds code to a procedure that allows
% early detection of infinite loops. If such loops are detected the program
% will terminate with a helpful error message.
%
% The memo transformation adds code that allows a procedure to "memo"
% (remember) answers once they have been generated using program clause
% resolution.
%
% The minimal model transformation changes the semantics of the procedure
% being transformed. See the PhD thesis of K. Sagonas: `The SLG-WAM: A
% Search-Efficient Engine for Well-Founded Evaluation of Normal Logic
% Programs' from SUNY at Stony Brook in 1996 for a description of
% the semantics behind the transformation. Currently only SLGd is
% implemented.
%
% The current implementation attempts to detect cases where tabling has
% undesirable interactions with if-then-else, solutions, and (possibly)
% negated contexts in general. However, the detection is done at runtime,
% since there is no known way of doing this compile time.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.table_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

%-----------------------------------------------------------------------------%

:- pred table_gen_process_module(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.purity.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.continuation_info.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module table_builtin.
:- import_module term.
:- import_module unit.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Values of this type map the pred_id of a minimal_model tabled
    % predicate to the pred_id of its generator variant.
    %
:- type generator_map   ==  map(pred_id, pred_id).

    % NOTE: following preds seem to duplicate the code in passes_aux.m.
    % The reason for this duplication is that this module needs a variant
    % of this code that is able to handle passing a module_info to
    % polymorphism and getting an updated module_info back.
    %
table_gen_process_module(!ModuleInfo, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, trace_table_io, TraceTableIO),
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.keys(Preds0, PredIds),
    map.init(GenMap0),
    table_gen_process_preds(TraceTableIO, PredIds,
        !ModuleInfo, GenMap0, _, !Specs).

:- pred table_gen_process_preds(bool::in, list(pred_id)::in,
    module_info::in, module_info::out,
    generator_map::in, generator_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

table_gen_process_preds(_, [], !ModuleInfo, !GenMap, !Specs).
table_gen_process_preds(TraceTableIO, [PredId | PredIds],
        !ModuleInfo, !GenMap, !Specs) :-
    table_gen_process_pred(TraceTableIO, PredId,
        !ModuleInfo, !GenMap, !Specs),
    table_gen_process_preds(TraceTableIO, PredIds,
        !ModuleInfo, !GenMap, !Specs).

:- pred table_gen_process_pred(bool::in, pred_id::in,
    module_info::in, module_info::out, generator_map::in, generator_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

table_gen_process_pred(TraceTableIO, PredId, !ModuleInfo, !GenMap, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    table_gen_process_procs(TraceTableIO, PredId, ProcIds,
        !ModuleInfo, !GenMap, !Specs).

:- pred table_gen_process_procs(bool::in, pred_id::in, list(proc_id)::in,
    module_info::in, module_info::out,
    generator_map::in, generator_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

table_gen_process_procs(_, _, [], !ModuleInfo, !GenMap, !Specs).
table_gen_process_procs(TraceTableIO, PredId, [ProcId | ProcIds],
        !ModuleInfo, !GenMap, !Specs) :-
    module_info_get_preds(!.ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo0),
    table_gen_process_proc(TraceTableIO, PredId, ProcId, ProcInfo0, PredInfo,
        !ModuleInfo, !GenMap, !Specs),
    table_gen_process_procs(TraceTableIO, PredId, ProcIds,
        !ModuleInfo, !GenMap, !Specs).

:- pred table_gen_process_proc(bool::in,
    pred_id::in, proc_id::in, proc_info::in, pred_info::in,
    module_info::in, module_info::out, generator_map::in, generator_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

table_gen_process_proc(TraceTableIO, PredId, ProcId, ProcInfo0, PredInfo0,
        !ModuleInfo, !GenMap, !Specs) :-
    proc_info_get_eval_method(ProcInfo0, EvalMethod),
    RequiresTablingTransform =
        eval_method_requires_tabling_transform(EvalMethod),
    (
        RequiresTablingTransform = yes,
        table_gen_transform_proc_if_possible(EvalMethod, PredId,
            ProcId, ProcInfo0, _, PredInfo0, _, !ModuleInfo, !GenMap, !Specs)
    ;
        RequiresTablingTransform = no,
        ( if
            TraceTableIO = yes,
            proc_info_has_io_state_pair(!.ModuleInfo, ProcInfo0,
                _InArgNum, _OutArgNum)
        then
            table_gen_process_io_proc(PredId, ProcId, ProcInfo0, PredInfo0,
                !ModuleInfo, !GenMap, !Specs)
        else
            true
        )
    ).

%-----------------------------------------------------------------------------%

:- pred table_gen_process_io_proc(pred_id::in, proc_id::in, proc_info::in,
    pred_info::in, module_info::in, module_info::out,
    generator_map::in, generator_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

table_gen_process_io_proc(PredId, ProcId, ProcInfo0, PredInfo0,
        !ModuleInfo, !GenMap, !Specs) :-
    CodeModel = proc_info_interface_code_model(ProcInfo0),
    (
        CodeModel = model_det
    ;
        ( CodeModel = model_semi
        ; CodeModel = model_non
        ),
        pred_id_to_int(PredId, PredIdInt),
        Msg = string.format("I/O procedure pred id %d not model_det",
            [i(PredIdInt)]),
        unexpected($module, $pred, Msg)
    ),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, trace_table_io_all, TransformAll),
    globals.lookup_bool_option(Globals, trace_table_io_require, Require),
    proc_info_get_goal(ProcInfo0, BodyGoal),
    PredModuleName = predicate_module(!.ModuleInfo, PredId),
    should_io_procedure_be_transformed(TransformAll, Require, BodyGoal,
        PredModuleName, AnnotationIsMissing, TransformPrimitive),
    (
        AnnotationIsMissing = yes,
        Spec = report_missing_tabled_for_io(PredInfo0, PredId, ProcId,
            !.ModuleInfo),
        !:Specs = [Spec | !.Specs]
    ;
        AnnotationIsMissing = no
    ),
    (
        TransformPrimitive = no
    ;
        TransformPrimitive = yes(Unitize),
        globals.lookup_bool_option(Globals, trace_table_io_only_retry,
            TraceTableIoOnlyRetry),
        (
            TraceTableIoOnlyRetry = no,
            pred_info_get_class_context(PredInfo0, TypeClassConstraints),
            TypeClassConstraints =
                constraints(UnivConstraints, ExistConstraints),
            ( if
                UnivConstraints = [],
                ExistConstraints = []
            then
                EntryKind = entry_stores_procid_inputs_outputs
            else
                EntryKind = entry_stores_procid_outputs
            )
        ;
            TraceTableIoOnlyRetry = yes,
            EntryKind = entry_stores_outputs
        ),
        TableIoMethod = eval_table_io(EntryKind, Unitize),
        proc_info_set_eval_method(TableIoMethod, ProcInfo0, ProcInfo1),
        table_gen_transform_proc_if_possible(TableIoMethod,
            PredId, ProcId, ProcInfo1, _, PredInfo0, _, !ModuleInfo,
            !GenMap, !Specs)
    ).

:- pred should_io_procedure_be_transformed(bool::in, bool::in, hlds_goal::in,
    sym_name::in, bool::out, maybe(table_io_is_unitize)::out) is det.

should_io_procedure_be_transformed(TransformAll, Require, BodyGoal,
        PredModuleName, AnnotationIsMissing, TransformInfo) :-
    tabled_for_io_attributes(BodyGoal, TabledForIoAttrs),
    (
        TabledForIoAttrs = [],
        AnnotationIsMissing = no,
        TransformInfo = no
    ;
        TabledForIoAttrs = [TabledForIoAttr],
        (
            TabledForIoAttr = proc_not_tabled_for_io,
            ( if
                Require = yes,
                not any_mercury_builtin_module(PredModuleName)
            then
                AnnotationIsMissing = yes,
                TransformInfo = no
            else
                AnnotationIsMissing = no,
                (
                    TransformAll = no,
                    TransformInfo = no
                ;
                    TransformAll = yes,
                    may_call_mercury_attributes(BodyGoal, MayCallMercuryAttrs),
                    ( if MayCallMercuryAttrs = [proc_may_call_mercury] then
                        TransformInfo = no
                    else
                        TransformInfo = yes(table_io_alone)
                    )
                )
            )
        ;
            TabledForIoAttr = proc_tabled_for_descendant_io,
            AnnotationIsMissing = no,
            % The procedure itself doesn't do any I/O, so don't transform it.
            TransformInfo = no
        ;
            TabledForIoAttr = proc_tabled_for_io,
            AnnotationIsMissing = no,
            TransformInfo = yes(table_io_alone)
        ;
            TabledForIoAttr = proc_tabled_for_io_unitize,
            AnnotationIsMissing = no,
            TransformInfo = yes(table_io_unitize)
        )
    ;
        TabledForIoAttrs = [_, _ | _],
        % Since table_gen is run before inlining, each procedure
        % should contain at most one foreign_proc goal.
        unexpected($module, $pred,
            "different tabled_for_io attributes in one procedure")
    ).

:- pred may_call_mercury_attributes(hlds_goal::in,
    list(proc_may_call_mercury)::out) is det.

may_call_mercury_attributes(Goal, MayCallMercuryAttrs) :-
    solutions.solutions(subgoal_may_call_mercury_attribute(Goal),
        MayCallMercuryAttrs).

:- pred subgoal_may_call_mercury_attribute(hlds_goal::in,
    proc_may_call_mercury::out) is nondet.

subgoal_may_call_mercury_attribute(Goal, MayCallMercuryAttr) :-
    some [SubGoal, Attrs] (
        goal_contains_goal(Goal, SubGoal),
        SubGoal = hlds_goal(call_foreign_proc(Attrs, _, _, _, _, _, _), _),
        MayCallMercuryAttr = get_may_call_mercury(Attrs)
    ).

:- pred tabled_for_io_attributes(hlds_goal::in, list(proc_tabled_for_io)::out)
    is det.

tabled_for_io_attributes(Goal, TabledForIoAttrs) :-
    solutions.solutions(subgoal_tabled_for_io_attribute(Goal),
        TabledForIoAttrs).

:- pred subgoal_tabled_for_io_attribute(hlds_goal::in, proc_tabled_for_io::out)
    is nondet.

subgoal_tabled_for_io_attribute(Goal, TabledForIoAttr) :-
    some [SubGoal, Attrs] (
        goal_contains_goal(Goal, SubGoal),
        SubGoal = hlds_goal(call_foreign_proc(Attrs, _, _, _, _, _, _), _),
        TabledForIoAttr = get_tabled_for_io(Attrs),
        not TabledForIoAttr = proc_not_tabled_for_io
    ).

:- func report_missing_tabled_for_io(pred_info, pred_id, proc_id, module_info)
    = error_spec.

report_missing_tabled_for_io(PredInfo, PredId, ProcId, ModuleInfo) = Spec :-
    pred_info_get_context(PredInfo, Context),
    ProcPieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = ProcPieces ++ [words("contains untabled I/O primitive."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_code_gen, [Msg]).

%-----------------------------------------------------------------------------%

:- pred table_gen_transform_proc_if_possible(eval_method::in,
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    generator_map::in, generator_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

table_gen_transform_proc_if_possible(EvalMethod, PredId, ProcId,
        !ProcInfo, !PredInfo, !ModuleInfo, !GenMap, !Specs) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    current_grade_supports_tabling(Globals, IsTablingSupported),
    (
        IsTablingSupported = yes,
        table_gen_transform_proc(EvalMethod, PredId, ProcId,
            !ProcInfo, !PredInfo, !ModuleInfo, !GenMap)
    ;
        IsTablingSupported = no,
        pred_info_get_context(!.PredInfo, Context),
        ProcPieces = describe_one_proc_name(!.ModuleInfo,
            should_module_qualify, proc(PredId, ProcId)),
        EvalMethodStr = eval_method_to_string(EvalMethod),
        Pieces = [words("Ignoring the pragma"), fixed(EvalMethodStr),
            words("for")] ++ ProcPieces ++
            [words("due to lack of support on this back end."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        % We don't want to increment the error count, since that would combine
        % with --halt-at-warn to prevent the clean compilation of the library.
        Spec = error_spec(severity_informational, phase_code_gen, [Msg]),
        !:Specs = [Spec | !.Specs],

        % XXX We set the evaluation method to eval_normal here to prevent
        % problems in the ml code generator if we are compiling in a grade
        % that does not support tabling. (See ml_gen_maybe_add_table_var/6
        % in ml_code_gen.m for further details.)
        %
        % We do this here rather than when processing the tabling pragmas
        % (in add_pragma.m) so that we can still generate error messages
        % for misuses of the tabling pragmas.

        proc_info_set_eval_method(eval_normal, !ProcInfo),
        module_info_set_pred_proc_info(PredId, ProcId, !.PredInfo,
            !.ProcInfo, !ModuleInfo)
    ).

:- pred table_gen_transform_proc(eval_method::in, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, pred_info::in, pred_info::out,
    module_info::in, module_info::out,
    generator_map::in, generator_map::out) is det.

table_gen_transform_proc(EvalMethod, PredId, ProcId, !ProcInfo, !PredInfo,
        !ModuleInfo, !GenMap) :-
    table_info_init(!.ModuleInfo, !.PredInfo, !.ProcInfo, TableInfo0),

    % Grab the appropriate fields from the pred_info and proc_info.
    proc_info_interface_determinism(!.ProcInfo, Detism),
    determinism_to_code_model(Detism, CodeModel),
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_goal(!.ProcInfo, OrigGoal),
    proc_info_get_argmodes(!.ProcInfo, ArgModes),
    proc_info_get_table_attributes(!.ProcInfo, MaybeAttributes),
    (
        MaybeAttributes = yes(Attributes)
    ;
        MaybeAttributes = no,
        Attributes = default_memo_table_attributes
    ),

    (
        EvalMethod = eval_normal,
        % This should have been caught by our caller.
        unexpected($module, $pred, "eval_normal")
    ;
        EvalMethod = eval_table_io(_, _),
        expect(unify(MaybeAttributes, no), $pred,
            "eval_table_io and Attributes"),
        % Since we don't actually create a call table for I/O tabled
        % procedures, the value of MaybeSpecMethod doesn't really matter.
        MaybeSpecMethod = msm_all_same(arg_value),
        Statistics = table_dont_gather_statistics,
        MaybeSizeLimit = no
    ;
        ( EvalMethod = eval_loop_check
        ; EvalMethod = eval_memo
        ; EvalMethod = eval_minimal(_)
        ),
        CallStrictness = Attributes ^ table_attr_strictness,
        Statistics = Attributes ^ table_attr_statistics,
        MaybeSizeLimit = Attributes ^ table_attr_size_limit,
        (
            CallStrictness = cts_all_strict,
            MaybeSpecMethod = msm_all_same(arg_value)
        ;
            CallStrictness = cts_all_fast_loose,
            MaybeSpecMethod = msm_all_same(arg_addr)
        ;
            CallStrictness = cts_specified(ArgMethods, HiddenArgMethod),
            MaybeSpecMethod = msm_specified(ArgMethods, HiddenArgMethod)
        ),
        (
            EvalMethod = eval_loop_check
        ;
            EvalMethod = eval_memo
        ;
            EvalMethod = eval_minimal(_),
            expect(unify(MaybeSizeLimit, no), $pred,
                "eval_minimal with size limit"),
            expect(unify(MaybeSpecMethod, msm_all_same(arg_value)), $pred,
                "eval_minimal without all_strict")
        )
    ),
    get_input_output_vars(HeadVars, ArgModes, !.ModuleInfo, MaybeSpecMethod, _,
        InputVarModeMethods, OutputVarModeMethods),
    allocate_slot_numbers(InputVarModeMethods, 0, NumberedInputVars),
    allocate_slot_numbers(OutputVarModeMethods, 0, NumberedOutputVars),
    % The case EvalMethod = eval_normal was caught by the code above.
    (
        EvalMethod = eval_table_io(Decl, Unitize),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, trace_table_io_states,
            TableIoStates),
        assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
        create_new_io_goal(OrigGoal, Decl, Unitize, TableIoStates,
            PredId, ProcId, HeadVarModes, NumberedInputVars,
            NumberedOutputVars, VarSet0, VarSet, VarTypes0, VarTypes,
            TableInfo0, TableInfo, Goal, MaybeProcTableIOInfo),
        MaybeCallTableTip = no,
        MaybeProcTableStructInfo = no
    ;
        EvalMethod = eval_loop_check,
        create_new_loop_goal(OrigGoal, Statistics,
            PredId, ProcId, HeadVars, NumberedInputVars, NumberedOutputVars,
            VarSet0, VarSet, VarTypes0, VarTypes,
            TableInfo0, TableInfo, CallTableTip, Goal, InputSteps),
        MaybeOutputSteps = no,
        generate_gen_proc_table_info(TableInfo, PredId, ProcId,
            InputSteps, MaybeOutputSteps,
            InputVarModeMethods, OutputVarModeMethods, ProcTableStructInfo),
        MaybeCallTableTip = yes(CallTableTip),
        MaybeProcTableIOInfo = no,
        MaybeProcTableStructInfo = yes(ProcTableStructInfo)
    ;
        EvalMethod = eval_memo,
        (
            CodeModel = model_non,
            create_new_memo_non_goal(Detism, OrigGoal, Statistics,
                MaybeSizeLimit, PredId, ProcId,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes, TableInfo0, TableInfo,
                CallTableTip, Goal, InputSteps, OutputSteps),
            MaybeOutputSteps = yes(OutputSteps)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_semi
            ),
            create_new_memo_goal(Detism, OrigGoal, Statistics, MaybeSizeLimit,
                PredId, ProcId,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes,
                TableInfo0, TableInfo, CallTableTip, Goal, InputSteps),
            MaybeOutputSteps = no
        ),
        generate_gen_proc_table_info(TableInfo, PredId, ProcId, InputSteps,
            MaybeOutputSteps, InputVarModeMethods, OutputVarModeMethods,
            ProcTableStructInfo),
        MaybeCallTableTip = yes(CallTableTip),
        MaybeProcTableIOInfo = no,
        MaybeProcTableStructInfo = yes(ProcTableStructInfo)
    ;
        EvalMethod = eval_minimal(MinimalMethod),
        expect(unify(CodeModel, model_non), $pred,
            "table_gen_transform_proc: minimal model but not model_non"),
        (
            MinimalMethod = stack_copy,
            create_new_mm_goal(Detism, OrigGoal, Statistics, PredId, ProcId,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes, TableInfo0, TableInfo,
                CallTableTip, Goal, InputSteps, OutputSteps),
            MaybeCallTableTip = yes(CallTableTip),
            MaybeOutputSteps = yes(OutputSteps),
            generate_gen_proc_table_info(TableInfo, PredId, ProcId, InputSteps,
                MaybeOutputSteps, InputVarModeMethods, OutputVarModeMethods,
                ProcTableStructInfo),
            MaybeProcTableStructInfo = yes(ProcTableStructInfo)
        ;
            MinimalMethod = own_stacks_consumer,
            do_own_stack_transform(Detism, OrigGoal, Statistics,
                PredId, ProcId, !.PredInfo, !.ProcInfo,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes, TableInfo0, TableInfo,
                !GenMap, Goal, _InputSteps, _OutputSteps),
            MaybeCallTableTip = no,
            MaybeProcTableStructInfo = no
        ;
            MinimalMethod = own_stacks_generator,
            % The own_stacks_generator minimal_method is only ever introduced
            % by the transformation in this module; a procedure that hasn't
            % been transformed yet should not have this eval_method.
            unexpected($module, $pred, "own stacks generator")
        ),
        MaybeProcTableIOInfo = no
    ),

    table_info_extract(TableInfo, !:ModuleInfo, !:PredInfo, !:ProcInfo),

    % Set the new values of the fields in proc_info and pred_info
    % and save in the module info.
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_call_table_tip(MaybeCallTableTip, !ProcInfo),

    (
        MaybeProcTableIOInfo = no
    ;
        MaybeProcTableIOInfo = yes(FinalProcTableIOInfo),
        proc_info_set_maybe_proc_table_io_info(yes(FinalProcTableIOInfo),
            !ProcInfo)
    ),

    (
        MaybeProcTableStructInfo = no
    ;
        MaybeProcTableStructInfo = yes(FinalProcTableStructInfo),
        PredProcId = proc(PredId, ProcId),
        add_proc_table_struct(PredProcId, FinalProcTableStructInfo, !.ProcInfo,
            !ModuleInfo)
    ),

    % Some of the instmap_deltas generated in this module are pretty dodgy
    % (especially those for if-then-elses), so recompute them here.
    % XXX Fix this: generate correct-by-construction instmap_deltas.
    recompute_instmap_delta_proc(do_not_recompute_atomic_instmap_deltas,
        !ProcInfo, !ModuleInfo),

    pred_info_get_proc_table(!.PredInfo, ProcTable1),
    map.det_update(ProcId, !.ProcInfo, ProcTable1, ProcTable),
    pred_info_set_proc_table(ProcTable, !PredInfo),

    % The transformation doesn't pay attention to the purity of compound goals,
    % so recompute the purity here.
    % XXX Fix this: generate correct-by-construction purity information.
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_get_preds(!.ModuleInfo, PredTable1),
    map.det_update(PredId, !.PredInfo, PredTable1, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

% Example of transformation for model_det loopcheck:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%   T0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(T0, A, T),
%   impure table_loop_setup(T, Status),
%   (
%       Status = loop_active,
%       error("detected infinite recursion in ...")
%   ;
%       Status = loop_inactive,
%       % status has been changed to active by the setup predicate
%       <original code>,
%       impure table_loop_mark_as_inactive(T)
%   ).
%
% Example of transformation for model_semi loopcheck:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%   T0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(T0, A, T),
%   impure table_loop_setup(T, Status),
%   (
%       Status = loop_active,
%       error("detected infinite recursion in ...")
%   ;
%       Status = loop_inactive,
%       % status has been changed to active by the setup predicate
%       ( if
%           <original code>, with B replaced by C
%       then
%           B = C,
%           impure table_loop_mark_as_inactive(T)
%       else
%           impure table_loop_mark_as_inactive_and_fail(T),
%       )
%   ).
%
% Example of transformation for model_non loopcheck:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%   T0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(T0, A, T),
%   impure table_loop_setup(T, Status),
%   (
%       Status = loop_active,
%       error("detected infinite recursion in ...")
%   ;
%       Status = loop_inactive,
%       % status has been changed to active by the setup predicate
%       (
%           <original code>,
%           (
%               impure table_loop_mark_as_inactive(T)
%           ;
%               impure table_loop_mark_as_active_and_fail(T),
%               fail
%           )
%       ;
%           impure table_loop_mark_as_inactive_and_fail(T)
%       )
%   ).

:- pred create_new_loop_goal(hlds_goal::in,
    table_attr_statistics::in, pred_id::in, proc_id::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, hlds_goal::out,
    list(table_step_desc)::out) is det.

create_new_loop_goal(OrigGoal, Statistics, PredId, ProcId,
        HeadVars, NumberedInputVars, NumberedOutputVars, !VarSet, !VarTypes,
        !TableInfo, TableTipVar, Goal, Steps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set_of_var.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    Context = goal_info_get_context(OrigGoalInfo),

    ModuleInfo = !.TableInfo ^ table_module_info,
    generate_simple_call_table_lookup_goal(loop_status_type,
        "table_loop_setup_shortcut", "MR_tbl_loop_setup",
        NumberedInputVars, PredId, ProcId, Statistics, Context,
        !VarSet, !VarTypes, !TableInfo, TableTipVar, StatusVar,
        LookUpGoal, Steps),

    generate_error_goal(!.TableInfo, Context, infinite_recursion_msg,
        !VarSet, !VarTypes, ActiveGoal),

    TableTipArg = foreign_arg(TableTipVar,
        yes(foreign_arg_name_mode(cur_table_node_name, in_mode)),
        trie_node_type, bp_native_if_possible),

    MarkInactivePredName = "table_loop_mark_as_inactive",
    MarkInactiveMacroName = "MR_tbl_loop_mark_as_inactive",
    MarkInactiveFailPredName = "table_loop_mark_as_inactive_and_fail",
    MarkInactiveFailMacroName = "MR_tbl_loop_mark_as_inactive_and_fail",
    MarkActiveFailPredName = "table_loop_mark_as_active_and_fail",
    MarkActiveFailMacroName = "MR_tbl_loop_mark_as_active_and_fail",

    DebugArgStr = get_debug_arg_string(!.TableInfo),
    MarkInactiveCode = "\t" ++ MarkInactiveMacroName ++
        "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");\n",
    MarkInactiveFailCode = "\t" ++ MarkInactiveFailMacroName ++
        "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");\n",
    MarkActiveFailCode = "\t" ++ MarkActiveFailMacroName ++
        "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");\n",

    table_generate_foreign_proc(MarkInactivePredName, detism_det,
        tabling_c_attributes_dupl, [TableTipArg], [],
        MarkInactiveCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, MarkInactiveGoal),
    table_generate_foreign_proc(MarkInactiveFailPredName, detism_failure,
        tabling_c_attributes_dupl, [TableTipArg], [],
        MarkInactiveFailCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, MarkInactiveFailGoal),
    table_generate_foreign_proc(MarkActiveFailPredName, detism_failure,
        tabling_c_attributes_dupl, [TableTipArg], [],
        MarkActiveFailCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, MarkActiveFailGoal),

    % The actual determinism of a predicate can be tighter than its
    % declared determinism. This can lead to problems, such as mantis bug 361.
    %
    % When we add code to the end of the procedure body to mark that the
    % current subgoal is now inactive, we keep the original procedure body's
    % determinism. This will cause a code generator abort (unexpected
    % determinism) if the "mark inactive" goal has a looser determinism
    % than the original body. This can happen by adding model_non "mark
    % inactive" code to the end of a model_det or model_semi goal,
    % or by adding model_semi "mark inactive" code to the end of a
    % model_det goal.
    %
    % We could fix this in one of two ways.
    %
    % - We can set the determinism of the compound goal that includes
    %   both the original body and the "mark inactive" goal based on the
    %   code model of the "mark inactive goal, which should always be
    %   the looser code model.
    %
    % - We can choose which transformation to apply based on the actual
    %   determinism of the procedure body, not its declared determinism.
    %
    % We implement the second approach.

    Detism = goal_info_get_determinism(OrigGoalInfo),
    determinism_to_code_model(Detism, CodeModel),
    set_of_var.list_to_set([TableTipVar | HeadVars], InactiveNonLocals),
    OutputVars = list.map(project_var, NumberedOutputVars),
    InactiveInstmapDelta = instmap_delta_bind_vars(OutputVars),
    (
        CodeModel = model_det,
        InactiveGoalExpr = conj(plain_conj, [OrigGoal, MarkInactiveGoal])
    ;
        CodeModel = model_semi,
        InstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
        create_renaming(OutputVars, InstMapDelta, !VarSet, !VarTypes,
            Unifies, NewVars, Renaming),
        rename_some_vars_in_goal(Renaming, OrigGoal, RenamedOrigGoal),

        ThenGoalExpr = conj(plain_conj, Unifies ++ [MarkInactiveGoal]),
        ThenVars = [TableTipVar | OutputVars] ++ NewVars,
        set_of_var.list_to_set(ThenVars, ThenNonLocals),
        goal_info_init_hide(ThenNonLocals, InactiveInstmapDelta, Detism,
            purity_impure, Context, ThenGoalInfo),
        ThenGoal = hlds_goal(ThenGoalExpr, ThenGoalInfo),

        InactiveGoalExpr = if_then_else([], RenamedOrigGoal,
            ThenGoal, MarkInactiveFailGoal)
    ;
        CodeModel = model_non,
        AfterGoalExpr = disj([MarkInactiveGoal, MarkActiveFailGoal]),
        instmap_delta_init_reachable(AfterInstMapDelta),
        goal_info_init_hide(set_of_var.make_singleton(TableTipVar),
            AfterInstMapDelta, detism_multi, purity_impure, Context,
            AfterGoalInfo),
        AfterGoal = hlds_goal(AfterGoalExpr, AfterGoalInfo),
        FirstGoalExpr = conj(plain_conj, [OrigGoal, AfterGoal]),
        OrigGINonLocals = goal_info_get_nonlocals(OrigGoalInfo),
        set_of_var.insert(TableTipVar, OrigGINonLocals, FirstNonlocals),
        goal_info_set_nonlocals(FirstNonlocals, OrigGoalInfo, FirstGoalInfo),
        FirstGoal = hlds_goal(FirstGoalExpr, FirstGoalInfo),
        InactiveGoalExpr = disj([FirstGoal, MarkInactiveFailGoal])
    ),
    goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta, Detism,
        purity_impure, Context, InactiveGoalInfo),
    InactiveGoal = hlds_goal(InactiveGoalExpr, InactiveGoalInfo),

    SwitchArms = [
        case(loop_active_cons_id, [], ActiveGoal),
        case(loop_inactive_cons_id, [], InactiveGoal)
    ],
    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    set_of_var.insert_list([StatusVar, TableTipVar],
        InactiveNonLocals, SwitchNonLocals),
    goal_info_init_hide(SwitchNonLocals, InactiveInstmapDelta, Detism,
        purity_impure, Context, SwitchGoalInfo),
    SwitchGoal = hlds_goal(SwitchExpr, SwitchGoalInfo),

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism,
        purity_impure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

% Example of transformation for model_det memo:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%   T0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(T0, A, T),
%   impure table_memo_setup(T, Status),
%   (
%       Status = memo_det_succeeded,
%       semipure table_memo_get_answer_block(T, Block),
%       semipure table_restore_int_answer(Block, 0, B)
%   ;
%       Status = memo_det_active,
%       error("detected infinite recursion in ...")
%   ;
%       Status = memo_det_inactive,
%       % status has been changed to active by the setup predicate
%       <original code>
%       impure table_memo_create_answer_block(T, 1, Block),
%       impure table_save_int_answer(Block, 0, B)
%   ).
%
% Example of transformation for model_semi memo:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%   T0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(T0, A, T),
%   impure table_memo_setup(T, Status),
%   (
%       Status = memo_semi_failed,
%       fail
%   ;
%       Status = memo_semi_succeeded,
%       semipure table_memo_get_answer_block(T, Block),
%       semipure table_restore_int_answer(Block, 0, B)
%   ;
%       Status = memo_semi_active,
%       error("detected infinite recursion in ...")
%   ;
%       Status = memo_semi_inactive,
%       % status has been changed to active by the setup predicate
%       ( if
%           <original code>, with B replaced by C
%       then
%           B = C,
%           impure table_memo_create_answer_block(T, 1, Block),
%           impure table_save_int_answer(Block, 0, B)
%       else
%           impure table_memo_mark_as_failed(T),
%           fail
%       )
%   ).
%
% Example of transformation for model_non memo:
%
% :- pred p(int::in, int::out) is semidet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%   CT0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(CT0, A, CT1),
%   impure table_memo_non_setup(CT1, Record, Status),
%   (
%       Status = memo_non_complete,
%       semipure table_memo_return_all_nondet(T, Block),
%       semipure table_restore_int_answer(Block, 0, B)
%   ;
%       Status = memo_non_incomplete,
%       error("detected need for minimal model tabling in ...")
%   ;
%       Status = memo_non_active,
%       error("detected infinite recursion in ...")
%   ;
%       Status = memo_non_inactive,
%       (
%           <original code>,
%
%               % Check for duplicate answers.
%           semipure table_memo_get_answer_table(Record, AT0),
%           impure table_lookup_insert_int(AT0, B, AT1),
%               % Fail if the answer is already in the table;
%               % otherwise, put it into the table.
%           impure table_mm_answer_is_not_duplicate(AT1),
%
%               % Save the new answer in the table.
%           impure table_memo_create_answer_block(Record, 1, Block),
%           impure table_save_int_answer(Block, 0, B),
%           (
%               impure table_memo_mark_as_incomplete(R)
%           ;
%               impure table_memo_mark_as_active_and_fail(R),
%               fail
%           )
%       ;
%           impure table_memo_mark_as_complete_and_fail(R)
%       )
%   ).
%
% If there are no output variables, then instead of creating an answer block
% and filling it in, we call table_memo_mark_as_succeeded.

:- pred create_new_memo_goal(determinism::in, hlds_goal::in,
    table_attr_statistics::in, maybe(int)::in,
    pred_id::in, proc_id::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, hlds_goal::out,
    list(table_step_desc)::out) is det.

create_new_memo_goal(Detism, OrigGoal, Statistics, _MaybeSizeLimit,
        PredId, ProcId, HeadVars, NumberedInputVars, NumberedOutputVars,
        !VarSet, !VarTypes, !TableInfo, TableTipVar, Goal, Steps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set_of_var.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    Context = goal_info_get_context(OrigGoalInfo),

    ModuleInfo = !.TableInfo ^ table_module_info,
    determinism_to_code_model(Detism, CodeModel),
    (
        CodeModel = model_det,
        StatusType = memo_det_status_type,
        SetupPredName = "table_memo_det_setup_shortcut",
        SetupMacroName = "MR_tbl_memo_det_setup"
    ;
        CodeModel = model_semi,
        StatusType = memo_semi_status_type,
        SetupPredName = "table_memo_semi_setup_shortcut",
        SetupMacroName = "MR_tbl_memo_semi_setup"
    ;
        CodeModel = model_non,
        unexpected($module, $pred, "model_non")
    ),
    generate_simple_call_table_lookup_goal(StatusType,
        SetupPredName, SetupMacroName, NumberedInputVars,
        PredId, ProcId, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, TableTipVar, StatusVar, LookUpGoal, Steps),

    generate_error_goal(!.TableInfo, Context, infinite_recursion_msg,
        !VarSet, !VarTypes, ActiveGoal),

    list.length(NumberedOutputVars, BlockSize),
    generate_memo_save_goal(NumberedOutputVars, TableTipVar, BlockSize,
        Context, !VarSet, !VarTypes, !TableInfo, SaveAnswerGoals),
    generate_memo_restore_goal(NumberedOutputVars, OrigInstMapDelta,
        TableTipVar, Context, !VarSet, !VarTypes, !.TableInfo,
        RestoreAnswerGoal),
    SucceededGoal = RestoreAnswerGoal,

    set_of_var.list_to_set([TableTipVar | HeadVars], InactiveNonLocals),
    OutputVars = list.map(project_var, NumberedOutputVars),
    InactiveInstmapDelta = instmap_delta_bind_vars(OutputVars),

    % The case CodeModel = model_non was caught by the code above.
    (
        CodeModel = model_det,
        InactiveGoalExpr = conj(plain_conj, [OrigGoal | SaveAnswerGoals]),
        goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
            Detism, purity_impure, Context, InactiveGoalInfo),
        InactiveGoal = hlds_goal(InactiveGoalExpr, InactiveGoalInfo),

        SwitchArms = [
            case(memo_det_active_cons_id, [], ActiveGoal),
            case(memo_det_inactive_cons_id, [], InactiveGoal),
            case(memo_det_succeeded_cons_id, [], SucceededGoal)
        ]
    ;
        CodeModel = model_semi,
        create_renaming(OutputVars, OrigInstMapDelta, !VarSet, !VarTypes,
            Unifies, NewVars, Renaming),
        rename_some_vars_in_goal(Renaming, OrigGoal, RenamedOrigGoal),

        ThenGoalExpr = conj(plain_conj, Unifies ++ SaveAnswerGoals),
        ThenVars = [TableTipVar | OutputVars] ++ NewVars,
        set_of_var.list_to_set(ThenVars, ThenNonLocals),
        goal_info_init_hide(ThenNonLocals, InactiveInstmapDelta,
            detism_det, purity_impure, Context, ThenGoalInfo),
        ThenGoal = hlds_goal(ThenGoalExpr, ThenGoalInfo),

        MarkAsFailedPredName = "table_memo_mark_as_failed",
        MarkAsFailedMacroName = "MR_tbl_memo_mark_as_failed",
        TableTipArg = foreign_arg(TableTipVar,
            yes(foreign_arg_name_mode(cur_table_node_name, in_mode)),
            trie_node_type, bp_native_if_possible),
        DebugArgStr = get_debug_arg_string(!.TableInfo),
        MarkAsFailedCode = MarkAsFailedMacroName ++
            "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");",
        table_generate_foreign_proc(MarkAsFailedPredName, detism_failure,
            tabling_c_attributes_dupl, [TableTipArg], [],
            MarkAsFailedCode, purity_impure, instmap_delta_bind_no_var,
            ModuleInfo, Context, ElseGoal),
        InactiveGoalExpr = if_then_else([], RenamedOrigGoal,
            ThenGoal, ElseGoal),
        goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta, Detism,
            purity_impure, Context, InactiveGoalInfo),
        InactiveGoal = hlds_goal(InactiveGoalExpr, InactiveGoalInfo),
        FailedGoal = fail_goal,

        SwitchArms = [
            case(memo_semi_active_cons_id, [], ActiveGoal),
            case(memo_semi_inactive_cons_id, [], InactiveGoal),
            case(memo_semi_succeeded_cons_id, [], SucceededGoal),
            case(memo_semi_failed_cons_id, [], FailedGoal)
        ]
    ),

    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    set_of_var.insert(StatusVar, InactiveNonLocals, SwitchNonLocals),
    goal_info_init_hide(SwitchNonLocals, InactiveInstmapDelta,
        Detism, purity_impure, Context, SwitchGoalInfo),
    SwitchGoal = hlds_goal(SwitchExpr, SwitchGoalInfo),

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred create_new_memo_non_goal(determinism::in, hlds_goal::in,
    table_attr_statistics::in, maybe(int)::in,
    pred_id::in, proc_id::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, hlds_goal::out,
    list(table_step_desc)::out, list(table_step_desc)::out) is det.

create_new_memo_non_goal(Detism, OrigGoal, Statistics, _MaybeSizeLimit,
        PredId, ProcId, HeadVars, NumberedInputVars, NumberedOutputVars,
        !VarSet, !VarTypes, !TableInfo, RecordVar, Goal,
        InputSteps, OutputSteps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set_of_var.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    Context = goal_info_get_context(OrigGoalInfo),

    ModuleInfo = !.TableInfo ^ table_module_info,
    list.length(NumberedOutputVars, BlockSize),

    generate_error_goal(!.TableInfo, Context, infinite_recursion_msg,
        !VarSet, !VarTypes, InfiniteRecursionGoal),
    generate_error_goal(!.TableInfo, Context, need_minimal_model_msg,
        !VarSet, !VarTypes, NeedMinModelGoal),

    generate_memo_non_call_table_lookup_goal(NumberedInputVars,
        PredId, ProcId, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, RecordVar, StatusVar, LookUpGoal, InputSteps),
    generate_memo_non_save_goals(NumberedOutputVars, PredId, ProcId,
        RecordVar, BlockSize, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, OutputSteps, SaveAnswerGoals),

    generate_memo_non_restore_goal(Detism, NumberedOutputVars,
        OrigInstMapDelta, RecordVar, Context,
        !VarSet, !VarTypes, !.TableInfo, RestoreAllAnswerGoal),

    RecordVarName = memo_non_record_name,
    RecordArg = foreign_arg(RecordVar,
        yes(foreign_arg_name_mode(RecordVarName, in_mode)),
        memo_non_record_type, bp_native_if_possible),

    MarkIncompletePredName = "table_memo_mark_as_incomplete",
    MarkIncompleteMacroName = "MR_tbl_memo_mark_as_incomplete",
    MarkActivePredName = "table_memo_mark_as_active_and_fail",
    MarkActiveMacroName = "MR_tbl_memo_mark_as_active_and_fail",
    MarkCompletePredName = "table_memo_mark_as_complete_and_fail",
    MarkCompleteMacroName = "MR_tbl_memo_mark_as_complete_and_fail",

    DebugArgStr = get_debug_arg_string(!.TableInfo),
    MarkIncompleteCode = MarkIncompleteMacroName ++
        "(" ++ DebugArgStr ++ ", " ++ RecordVarName ++ ");\n",
    MarkActiveCode = MarkActiveMacroName ++
        "(" ++ DebugArgStr ++ ", " ++ RecordVarName ++ ");\n",
    MarkCompleteCode = MarkCompleteMacroName ++
        "(" ++ DebugArgStr ++ ", " ++ RecordVarName ++ ");\n",

    table_generate_foreign_proc(MarkIncompletePredName, detism_det,
        tabling_c_attributes_dupl, [RecordArg], [],
        MarkIncompleteCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, MarkIncompleteGoal),
    table_generate_foreign_proc(MarkActivePredName, detism_failure,
        tabling_c_attributes_dupl, [RecordArg], [],
        MarkActiveCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, MarkActiveGoal),
    table_generate_foreign_proc(MarkCompletePredName, detism_failure,
        tabling_c_attributes_dupl, [RecordArg], [],
        MarkCompleteCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, MarkCompleteGoal),

    OrigSaveExpr = conj(plain_conj, [OrigGoal | SaveAnswerGoals]),
    set_of_var.insert(RecordVar, OrigNonLocals, OrigSaveNonLocals),
    create_instmap_delta([OrigGoal | SaveAnswerGoals], OrigSaveIMD0),
    instmap_delta_restrict(OrigSaveNonLocals, OrigSaveIMD0, OrigSaveIMD),
    goal_info_init_hide(OrigSaveNonLocals, OrigSaveIMD, detism_non,
        purity_impure, Context, OrigSaveGoalInfo),
    OrigSaveGoal = hlds_goal(OrigSaveExpr, OrigSaveGoalInfo),

    AfterExpr = disj([MarkIncompleteGoal, MarkActiveGoal]),
    AfterNonLocals = set_of_var.make_singleton(RecordVar),
    create_instmap_delta([], AfterInstMapDelta),
    goal_info_init_hide(AfterNonLocals, AfterInstMapDelta, detism_non,
        purity_impure, Context, AfterGoalInfo),
    AfterGoal = hlds_goal(AfterExpr, AfterGoalInfo),

    OrigSaveAfterExpr = conj(plain_conj, [OrigSaveGoal, AfterGoal]),
    OrigSaveAfterGoal = hlds_goal(OrigSaveAfterExpr, OrigSaveGoalInfo),

    InactiveExpr = disj([OrigSaveAfterGoal, MarkCompleteGoal]),
    InactiveGoal = hlds_goal(InactiveExpr, OrigSaveGoalInfo),

    set_of_var.list_to_set([RecordVar | HeadVars], InactiveNonLocals),
    OutputVars = list.map(project_var, NumberedOutputVars),
    InactiveInstmapDelta = instmap_delta_bind_vars(OutputVars),

    SwitchArms = [
        case(memo_non_active_cons_id, [], InfiniteRecursionGoal),
        case(memo_non_inactive_cons_id, [], InactiveGoal),
        case(memo_non_incomplete_cons_id, [], NeedMinModelGoal),
        case(memo_non_complete_cons_id, [], RestoreAllAnswerGoal)
    ],

    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    set_of_var.insert(StatusVar, InactiveNonLocals, SwitchNonLocals),
    goal_info_init_hide(SwitchNonLocals, InactiveInstmapDelta, Detism,
        purity_impure, Context, SwitchGoalInfo),
    SwitchGoal = hlds_goal(SwitchExpr, SwitchGoalInfo),

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

% Example of transformation for tabling I/O, for I/O primitives (i.e.
% predicates defined by foreign_procs that take an input/output pair of
% io_state arguments) that have the tabled_for_io feature:
%
% :- pred p(int, string, io, io).
% :- mode p(in, out, di, uo) is det.
%
% p(A, B, S0, S) :-
%   <original code>
%
% The transformed code would be:
%
% p(A, B, S0, S) :-
%   ( if
%           % Get the global I/O table, the global I/O
%           % counter, and the starting point for tabling
%           % I/O actions, if we are in the tabled range.
%       table_io_in_range(T0, Counter, Start)
%   then
%           % Look up the input arguments.
%       impure table_lookup_insert_start_int(T0, Counter,
%           Start, T),
%       ( if
%           semipure table_io_has_occurred(T)
%       then
%           semipure table_memo_get_answer_block(T, Block),
%           impure table_restore_string_answer(Block, 0, B),
%           table_io_copy_io_state(S0, S)
%       else
%           <original code>
%               % Save the answers in the table.
%           impure table_io_create_answer_block(T, 1, Block),
%           impure table_save_string_answer(Block, 0, B)
%       )
%   else
%       <original code>
%   ).
%
% Note that copying the I/O state works only because variables of type
% io.state don't actually contain any information; the information is actually
% stored in global variables. However, if this ever changes, the transformation
% can be fixed simply by changing the value of --trace-table-io-states to yes,
% which will cause such values to be tabled along with the other output
% arguments.
%
% For I/O primitives that do not have tabled_for_io, we should require that
% they do not do any I/O in their own code, meaning that all their I/O is
% inside any Mercury code they call. We can then leave such primitives
% untransformed; the I/O primitives called from the inner Mercury engine
% will do the right thing. For now, this requirement is not enforced,
% which means that enabling I/O tabling (with --trace-table-io) does not
% guarantee that *all* I/O actions are tabled. This can cause inconsistent
% behavior after retry commands in mdb. This is the reason why retry across
% I/O is experimental for now.
%
% The reason why we require I/O primitives to be marked manually by a
% programmer with the tabled_for_io feature is to get the programmer to make
% sure that the primitive meets the requirement. Unfortunately, this cannot be
% automated, since automation would require analysis of arbitrary C code.
%
% The transformation for tabling I/O for declarative debugging is a variant
% of the usual transformation of I/O primitives. In this variant, the answer
% block contains not only the answers, but also a pointer to the primitive's
% proc layout structure and the values of the input aruments. The code we
% generate will fill in the slots containing this extra information before
% it executes the original goal.

:- pred create_new_io_goal(hlds_goal::in, table_io_entry_kind::in,
    table_io_is_unitize::in, bool::in, pred_id::in, proc_id::in,
    assoc_list(prog_var, mer_mode)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out,
    hlds_goal::out, maybe(proc_table_io_info)::out) is det.

create_new_io_goal(OrigGoal, TableIoEntryKind, Unitize, TableIoStates,
        PredId, ProcId, HeadVarModes, OrigInputVars, OrigOutputVars,
        !VarSet, !VarTypes, !TableInfo, Goal, MaybeProcTableIOInfo) :-
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    ModuleInfo0 = !.TableInfo ^ table_module_info,
    module_info_pred_info(ModuleInfo0, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if check_marker(Markers, marker_user_marked_no_inline) then
        %
        % If the predicate should not be inlined, then we create a new
        % predicate with the same body as the original predicate, which is
        % called wherever the original goal would appear in the transformed
        % code.  This is necessary when the original goal is foreign C code
        % that uses labels.  The original goal would otherwise be duplicated
        % by the transformation, resulting in duplicate label errors from
        % the C compiler.
        %
        clone_proc_and_create_call(PredInfo, ProcId, CallExpr, ModuleInfo0,
            ModuleInfo),
        NewGoal = hlds_goal(CallExpr, OrigGoalInfo),
        !TableInfo ^ table_module_info := ModuleInfo
    else
        NewGoal = OrigGoal,
        ModuleInfo = ModuleInfo0
    ),
    OrigNonLocals = goal_info_get_nonlocals(OrigGoalInfo),
    Context = goal_info_get_context(OrigGoalInfo),

    (
        TableIoStates = yes,
        IoStateAssignToVars = [],
        IoStateAssignFromVars = [],
        SavedOutputVars = OrigOutputVars,
        SavedHeadVars = HeadVarModes
    ;
        TableIoStates = no,
        list.filter(var_mode_pos_is_io_state(!.VarTypes),
            OrigOutputVars, IoStateAssignToVars, MisNumberedSavedOutputVars),
        reallocate_slot_numbers(MisNumberedSavedOutputVars, 0,
            SavedOutputVars),
        list.filter(var_mode_pos_is_io_state(!.VarTypes),
            OrigInputVars, IoStateAssignFromVars, _MisNumberedSavedInputVars),
        list.filter(var_mode_is_io_state(!.VarTypes),
            HeadVarModes, _, SavedHeadVars)
    ),
    generate_new_table_var("TableVar", trie_node_type, !VarSet, !VarTypes,
        TableVar),
    generate_new_table_var("CounterVar", int_type, !VarSet, !VarTypes,
        CounterVar),
    generate_new_table_var("StartVar", int_type, !VarSet, !VarTypes,
        StartVar),
    table_generate_call("table_io_in_range", detism_semi,
        [TableVar, CounterVar, StartVar], purity_impure,
        instmap_delta_bind_vars([TableVar, CounterVar, StartVar]),
        ModuleInfo, Context, InRangeGoal),
    generate_new_table_var("TipVar", trie_node_type, !VarSet, !VarTypes,
        TipVar),
    table_generate_call("table_lookup_insert_start_int", detism_det,
        [TableVar, StartVar, CounterVar, TipVar], purity_impure,
        instmap_delta_bind_var(TipVar), ModuleInfo, Context, LookupGoal),
    table_generate_call("table_io_has_occurred", detism_semi, [TipVar],
        purity_impure, instmap_delta_bind_no_var, ModuleInfo, Context,
        OccurredGoal),
    (
        TableIoEntryKind = entry_stores_procid_inputs_outputs,
        ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
        TableIoEntryConsId = table_io_entry_desc(ShroudedPredProcId),
        make_const_construction_alloc(TableIoEntryConsId, c_pointer_type,
            yes("TableIoEntryDescPtr"), TableIoEntryDescGoal,
            TableIoEntryDescPtrVar, !VarSet, !VarTypes),
        allocate_plain_slot_numbers(SavedHeadVars, 1, NumberedSavedHeadVars),
        NumberedSaveVars = [
            var_mode_pos_method(TableIoEntryDescPtrVar, in_mode, 0, unit)
            | NumberedSavedHeadVars],
        UnnumberedSavedOutputVars = list.map(project_var, SavedOutputVars),
        list.filter(var_belong_to_list(UnnumberedSavedOutputVars),
            NumberedSaveVars, NumberedSavedOutputVars),
        NumberedRestoreVars = NumberedSavedOutputVars,

        ProcInfo0 = !.TableInfo ^ table_cur_proc_info,
        continuation_info.generate_table_arg_type_info(ProcInfo0,
            list.map(project_var_pos, NumberedSavedHeadVars),
            TableArgTypeInfo),
        ProcTableIOInfo = proc_table_io_info(yes(TableArgTypeInfo)),
        MaybeProcTableIOInfo = yes(ProcTableIOInfo)
    ;
        TableIoEntryKind = entry_stores_procid_outputs,
        ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
        TableIoEntryConsId = table_io_entry_desc(ShroudedPredProcId),
        make_const_construction_alloc(TableIoEntryConsId, c_pointer_type,
            yes("TableIoEntryDescPtr"), TableIoEntryDescGoal,
            TableIoEntryDescPtrVar, !VarSet, !VarTypes),

        NumberedRestoreVars0 =
            list.map(project_out_arg_method, SavedOutputVars),
        reallocate_slot_numbers(NumberedRestoreVars0, 1,
            NumberedRestoreVars),
        NumberedSaveVars = [
            var_mode_pos_method(TableIoEntryDescPtrVar, in_mode, 0, unit)
            | NumberedRestoreVars],

        ProcTableIOInfo = proc_table_io_info(no),
        MaybeProcTableIOInfo = yes(ProcTableIOInfo)
    ;
        TableIoEntryKind = entry_stores_outputs,
        TableIoEntryDescGoal = true_goal,
        NumberedRestoreVars =
            list.map(project_out_arg_method, SavedOutputVars),
        NumberedSaveVars = NumberedRestoreVars,
        MaybeProcTableIOInfo = no
    ),
    list.length(NumberedSaveVars, BlockSize),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    generate_memo_restore_goal(NumberedRestoreVars, OrigInstMapDelta, TipVar,
        Context, !VarSet, !VarTypes, !.TableInfo, RestoreAnswerGoal0),
    (
        TableIoStates = yes,
        RestoreAnswerGoal = RestoreAnswerGoal0
    ;
        TableIoStates = no,
        ( if
            IoStateAssignFromVars = [IoStateAssignFromVarPrime],
            IoStateAssignToVars = [IoStateAssignToVarPrime]
        then
            IoStateAssignFromVar = project_var(IoStateAssignFromVarPrime),
            IoStateAssignToVar = project_var(IoStateAssignToVarPrime)
        else
            % The call to proc_info_has_io_state_pair in
            % table_gen_process_procs should ensure that we never get here.
            unexpected($module, $pred, "one in / one out violation")
        ),
        table_generate_call("table_io_copy_io_state", detism_det,
            [IoStateAssignFromVar, IoStateAssignToVar], purity_pure,
            instmap_delta_from_assoc_list([
                IoStateAssignFromVar - ground(clobbered, none_or_default_func),
                IoStateAssignToVar - ground(unique, none_or_default_func)]),
            ModuleInfo, Context, IoStateAssignGoal),
        RestoreAnswerGoalExpr = conj(plain_conj,
            [RestoreAnswerGoal0, IoStateAssignGoal]),
        create_instmap_delta([RestoreAnswerGoal0, IoStateAssignGoal],
            RestoreAnswerInstMapDelta0),
        RestoreAnswerGoal0 = hlds_goal(_, RestoreAnswerGoal0Info),
        RestoreAnswer0NonLocals =
            goal_info_get_nonlocals(RestoreAnswerGoal0Info),
        set_of_var.insert_list([IoStateAssignFromVar, IoStateAssignToVar],
            RestoreAnswer0NonLocals, RestoreAnswerNonLocals),
        instmap_delta_restrict(RestoreAnswerNonLocals,
            RestoreAnswerInstMapDelta0, RestoreAnswerInstMapDelta),
        goal_info_init_hide(RestoreAnswerNonLocals,
            RestoreAnswerInstMapDelta, detism_det, purity_semipure, Context,
            RestoreAnswerGoalInfo),
        RestoreAnswerGoal = hlds_goal(RestoreAnswerGoalExpr,
            RestoreAnswerGoalInfo)
    ),
    generate_memo_save_goal(NumberedSaveVars, TipVar, BlockSize,
        Context, !VarSet, !VarTypes, !TableInfo, SaveAnswerGoals),
    (
        Unitize = table_io_alone,
        CallSaveAnswerGoalList =
            [NewGoal, TableIoEntryDescGoal | SaveAnswerGoals]
    ;
        Unitize = table_io_unitize,
        generate_new_table_var("SavedTraceEnabled", int_type,
            !VarSet, !VarTypes, SavedTraceEnabledVar),
        table_generate_call("table_io_left_bracket_unitized_goal", detism_det,
            [SavedTraceEnabledVar], purity_impure,
            instmap_delta_bind_var(SavedTraceEnabledVar),
            ModuleInfo, Context, LeftBracketGoal),
        table_generate_call("table_io_right_bracket_unitized_goal", detism_det,
            [SavedTraceEnabledVar], purity_impure, instmap_delta_bind_no_var,
            ModuleInfo, Context, RightBracketGoal),
        CallSaveAnswerGoalList = [LeftBracketGoal, NewGoal,
            RightBracketGoal, TableIoEntryDescGoal | SaveAnswerGoals]
    ),
    CallSaveAnswerGoalExpr = conj(plain_conj, CallSaveAnswerGoalList),
    create_instmap_delta(CallSaveAnswerGoalList, CallSaveAnswerInstMapDelta0),
    set_of_var.insert(TipVar, OrigNonLocals, CallSaveAnswerNonLocals),
    instmap_delta_restrict(CallSaveAnswerNonLocals,
        CallSaveAnswerInstMapDelta0, CallSaveAnswerInstMapDelta),
    goal_info_init_hide(CallSaveAnswerNonLocals, CallSaveAnswerInstMapDelta,
        detism_det, purity_impure, Context, CallSaveAnswerGoalInfo0),
    goal_info_add_feature(feature_hide_debug_event,
        CallSaveAnswerGoalInfo0, CallSaveAnswerGoalInfo),
    CallSaveAnswerGoal = hlds_goal(CallSaveAnswerGoalExpr,
        CallSaveAnswerGoalInfo),

    GenIfNecGoalExpr = if_then_else([], OccurredGoal,
        RestoreAnswerGoal, CallSaveAnswerGoal),
    create_instmap_delta([OccurredGoal, RestoreAnswerGoal,
        CallSaveAnswerGoal], GenIfNecInstMapDelta0),
    set_of_var.insert(TipVar, OrigNonLocals, GenIfNecNonLocals),
    instmap_delta_restrict(GenIfNecNonLocals,
        GenIfNecInstMapDelta0, GenIfNecInstMapDelta),
    goal_info_init_hide(GenIfNecNonLocals, GenIfNecInstMapDelta, detism_det,
        purity_impure, Context, GenIfNecGoalInfo),
    GenIfNecGoal = hlds_goal(GenIfNecGoalExpr, GenIfNecGoalInfo),

    CheckAndGenAnswerGoalExpr = conj(plain_conj, [LookupGoal, GenIfNecGoal]),
    create_instmap_delta([LookupGoal, GenIfNecGoal],
        CheckAndGenAnswerInstMapDelta0),
    set_of_var.insert_list([TableVar, CounterVar, StartVar],
        OrigNonLocals, CheckAndGenAnswerNonLocals),
    instmap_delta_restrict(CheckAndGenAnswerNonLocals,
        CheckAndGenAnswerInstMapDelta0, CheckAndGenAnswerInstMapDelta),
    goal_info_init_hide(CheckAndGenAnswerNonLocals,
        CheckAndGenAnswerInstMapDelta, detism_det, purity_impure, Context,
        CheckAndGenAnswerGoalInfo),
    CheckAndGenAnswerGoal = hlds_goal(CheckAndGenAnswerGoalExpr,
        CheckAndGenAnswerGoalInfo),

    BodyGoalExpr = if_then_else([], InRangeGoal, CheckAndGenAnswerGoal,
        NewGoal),
    create_instmap_delta([InRangeGoal, CheckAndGenAnswerGoal, NewGoal],
        BodyInstMapDelta0),
    instmap_delta_restrict(OrigNonLocals, BodyInstMapDelta0, BodyInstMapDelta),
    goal_info_init_hide(OrigNonLocals, BodyInstMapDelta, detism_det,
        purity_impure, Context, BodyGoalInfo),
    Goal = hlds_goal(BodyGoalExpr, BodyGoalInfo).

%-----------------------------------------------------------------------------%

% Example of transformation for nondet minimal_model:
%
% :- pred p(int, int).
% :- mode p(in, out) is nondet.
%
% p(A, B) :-
%   <original code>.
%
% The transformed code would be:
%
% p(A, B) :-
%       % Get a handle on the table.
%   CT0 = <table pointer for p/2>,
%
%       % Look up the input arguments, and set up the table.
%   impure table_lookup_insert_int(CT0, A, CT1),
%   impure table_mm_setup(CT1, Subgoal, Status),
%   (
%       Status = complete,
%           % Return all the answers from the complete table.
%       semipure table_mm_return_all_nondet(Subgoal, Block),
%       semipure table_restore_int_answer(Block, 0, B)
%   ;
%       Status = active,
%           % Suspend the current computational branch.
%           % Resume when the generator has computed some answers.
%       impure table_mm_suspend_consumer(Subgoal, Block),
%       semipure table_restore_int_answer(Block, 0, B)
%   ;
%       Status = inactive,
%       (
%           <original code>,
%
%               % Check for duplicate answers.
%           semipure table_mm_get_answer_table(Subgoal, AT0),
%           impure table_lookup_insert_int(AT0, B, AT1),
%               % Fail if the answer is already in the table;
%               % otherwise, put it into the table.
%           impure table_mm_answer_is_not_duplicate(AT1),
%
%               % Save the new answer in the table.
%           impure table_mm_create_answer_block(Subgoal, 1, Block),
%           impure table_save_int_answer(Block, 0, B)
%       ;
%               % Mark this subgoal as completely evaluated,
%               % modulo any dependencies on other subgoals.
%           impure table_mm_completion(Subgoal),
%           fail
%       )
%   ).

:- pred create_new_mm_goal(determinism::in, hlds_goal::in,
    table_attr_statistics::in, pred_id::in, proc_id::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, hlds_goal::out,
    list(table_step_desc)::out, list(table_step_desc)::out) is det.

create_new_mm_goal(Detism, OrigGoal, Statistics, PredId, ProcId,
        HeadVars, NumberedInputVars, NumberedOutputVars, !VarSet, !VarTypes,
        !TableInfo, SubgoalVar, Goal, InputSteps, OutputSteps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set_of_var.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    Context = goal_info_get_context(OrigGoalInfo),

    ModuleInfo = !.TableInfo ^ table_module_info,
    list.length(NumberedOutputVars, BlockSize),
    generate_mm_call_table_lookup_goal(NumberedInputVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo,
        SubgoalVar, StatusVar, LookUpGoal, InputSteps),
    generate_mm_save_goals(NumberedOutputVars, SubgoalVar, PredId, ProcId,
        BlockSize, Statistics, Context, !VarSet, !VarTypes, !TableInfo,
        OutputSteps, SaveAnswerGoals),
    generate_mm_restore_goal(Detism, NumberedOutputVars, OrigInstMapDelta,
        SubgoalVar, Context, !VarSet, !VarTypes, !.TableInfo,
        RestoreAllAnswerGoal),
    generate_mm_suspend_goal(NumberedOutputVars, OrigInstMapDelta,
        SubgoalVar, Context, !VarSet, !VarTypes, !.TableInfo, SuspendGoal),

    MainExpr = conj(plain_conj, [OrigGoal | SaveAnswerGoals]),
    set_of_var.insert_list([SubgoalVar, StatusVar],
        OrigNonLocals, MainNonLocals),
    create_instmap_delta([OrigGoal | SaveAnswerGoals], MainIMD0),
    instmap_delta_restrict(MainNonLocals, MainIMD0, MainIMD),
    goal_info_init_hide(MainNonLocals, MainIMD, detism_non, purity_impure,
        Context, MainGoalInfo),
    MainGoal = hlds_goal(MainExpr, MainGoalInfo),

    table_generate_call("table_mm_completion", detism_det, [SubgoalVar],
        purity_impure, instmap_delta_bind_no_var, ModuleInfo, Context,
        ResumeGoal0),
    append_fail(ResumeGoal0, ResumeGoal),
    InactiveExpr = disj([MainGoal, ResumeGoal]),
    InactiveGoal = hlds_goal(InactiveExpr, MainGoalInfo),

    SwitchArms = [
        case(mm_inactive_cons_id, [], InactiveGoal),
        case(mm_active_cons_id, [], SuspendGoal),
        case(mm_complete_cons_id, [], RestoreAllAnswerGoal)
    ],
    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    goal_info_add_feature(feature_hide_debug_event,
        MainGoalInfo, SwitchGoalInfo),
    SwitchGoal = hlds_goal(SwitchExpr, SwitchGoalInfo),

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, detism_non,
        purity_impure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

% Example of transformation for nondet minimal_model_own_stack:
%
% :- pred p(int, int).
% :- mode p(in, out) is nondet
%
% p(A, B) :- e(A, B).
% p(A, B) :- p(A, C), e(C, B).
%
% The transformed code would be something like this; see also forest.ps
% in the tabling directory in the papers CVS module.
%
% p2_gen(A, B) :-
%   impure table_mmos_pickup_generator(Generator),
%   (
%       (
%           %
%           % Original goals
%           %
%       ),
%
%       % Check for duplicate answers.
%       semipure table_mmos_get_answer_table(Generator, AT0),
%       impure table_lookup_insert_int(AT0, B, AT1),
%
%       % Fail if the answer is already in the table;
%       % otherwise, put it into the table.
%       impure table_mmos_answer_is_not_duplicate(AT1),
%
%       % Save the new answer in the table.
%       impure table_mmos_create_answer_block(Generator, 1, AnswerBlock),
%       impure table_save_int_ans(AnswerBlock, 0, B),
%   ;
%       impure table_mmos_completion(Generator)
%   ).
%   MR_succeed is replaced by table_mmos_return_answer(Generator).
%
% p(A, B) :-
%   CT0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(CT0, A, CT1),
%   ( if CT1 = NULL then
%       table_mmos_save_inputs(0, A),   % into global var MR_mmos_input_arg
%       Generator = table_mmos_setup_generator(CT1, 1, p2_gen, "p"),
%   then
%       Generator = trie_node_to_generator(CT1)
%   ),
%   impure table_mmos_setup_consumer(Generator, Consumer),
%   impure table_mmos_consume_next_answer_nondet(Consumer, AnswerBlock),
%   impure table_restore_int_ans(AnswerBlock, 0, B).

:- pred do_own_stack_transform(determinism::in, hlds_goal::in,
    table_attr_statistics::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, generator_map::in, generator_map::out,
    hlds_goal::out, list(table_step_desc)::out, list(table_step_desc)::out)
    is det.

do_own_stack_transform(Detism, OrigGoal, Statistics, PredId, ProcId,
        PredInfo0, ProcInfo0, HeadVars, NumberedInputVars, NumberedOutputVars,
        !VarSet, !VarTypes, !TableInfo, !GenMap, Goal,
        InputSteps, OutputSteps) :-
    PredName = pred_info_name(PredInfo0),
    ( if map.search(!.GenMap, PredId, GeneratorPredIdPrime) then
        GeneratorPredId = GeneratorPredIdPrime
    else
        clone_pred_info(PredId, PredInfo0, HeadVars, NumberedOutputVars,
            GeneratorPredId, !TableInfo),
        map.det_insert(PredId, GeneratorPredId, !GenMap)
    ),

    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set_of_var.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    Context = goal_info_get_context(OrigGoalInfo),

    list.length(NumberedInputVars, NumInputVars),
    ModuleInfo = !.TableInfo ^ table_module_info,

    % The type is a lie, but a safe one: the variable we create
    % is handled only by C code. The reason why we lie is that
    % creating the right type would be quite complicated.
    GeneratorPredType = c_pointer_type,

    generate_new_table_var("GeneratorPredVar", GeneratorPredType,
        !VarSet, !VarTypes, GeneratorPredVar),
    generate_new_table_var("Consumer", consumer_type,
        !VarSet, !VarTypes, ConsumerVar),

    ShroudedPredProcId = shroud_pred_proc_id(proc(GeneratorPredId, ProcId)),
    GeneratorConsId = closure_cons(ShroudedPredProcId, lambda_normal),
    make_const_construction(Context, GeneratorPredVar, GeneratorConsId,
        MakeGeneratorVarGoal),

    generate_call_table_lookup_goals(NumberedInputVars,
        GeneratorPredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, InputSteps,
        _TableTipVar, _TableTipArg, InfoArg, LookupForeignArgs,
        LookupPrefixGoals, LookupCodeStr, _CallTableTipAssignStr),

    InputVarModes = list.map(project_mode, NumberedInputVars),
    assoc_list.from_corresponding_lists(LookupForeignArgs, InputVarModes,
        LookupForeignArgModes),
    generate_save_input_vars_code(LookupForeignArgModes, ModuleInfo, 0,
        PickupForeignArgs, SaveInputVarCode, PickupInputVarCode),

    GeneratorPredVarName = generator_pred_name,
    ConsumerVarName = consumer_name,

    GeneratorPredArg = foreign_arg(GeneratorPredVar,
        yes(foreign_arg_name_mode(generator_pred_name, in_mode)),
        GeneratorPredType, bp_native_if_possible),
    ConsumerArg = foreign_arg(ConsumerVar,
        yes(foreign_arg_name_mode(ConsumerVarName, out_mode)),
        consumer_type, bp_native_if_possible),

    LookupDeclCodeStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n" ++
        "\tMR_GeneratorPtr " ++ generator_name ++ ";\n\n" ++
        LookupCodeStr,

    SetupPredName = "table_mmos_setup_consumer",
    SetupCode = "\t" ++ generator_name ++ " = " ++
        cur_table_node_name ++ "->MR_generator;\n" ++
        "\tif (" ++ generator_name ++ " == NULL) {\n" ++
            SaveInputVarCode ++
            "\t\t" ++ generator_name ++ " = MR_table_mmos_setup_generator(" ++
                cur_table_node_name ++ ",\n\t\t\t"
                ++ int_to_string(NumInputVars) ++ ", "
                ++ GeneratorPredVarName ++ ", " ++
                """" ++ PredName ++ """);\n" ++
            "\t\tMR_mmos_new_generator = " ++ generator_name ++ ";\n" ++
        "\t}\n" ++
        "\t" ++ consumer_name ++ " = " ++
            "MR_table_mmos_setup_consumer(" ++ generator_name ++
            ", """ ++ PredName ++ """);\n",
    table_generate_foreign_proc(SetupPredName, detism_det,
        make_generator_c_attributes,
        [InfoArg, GeneratorPredArg, ConsumerArg], LookupForeignArgs,
        LookupDeclCodeStr ++ SetupCode, purity_impure,
        instmap_delta_bind_var(ConsumerVar), ModuleInfo, Context,
        SetupGoal),
    % We don't attach the call_table_tip attribute to the setup goal, since
    % retrying across the creation of the generator should not require undoing
    % the creation of the generator. Of course, for this to work *properly*,
    % the runtime system and the debugger will need to cooperate to effectively
    % present to the user a distinct sequence of trace events for each context.
    % attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = [MakeGeneratorVarGoal | LookupPrefixGoals]
        ++ [SetupGoal],

    generate_new_table_var("AnswerBlock", answer_block_type,
        !VarSet, !VarTypes, AnswerBlockVar),
    ( if Detism = detism_multi then
        ConsumePredName = "table_mmos_consume_next_answer_multi"
    else if Detism = detism_non then
        ConsumePredName = "table_mmos_consume_next_answer_nondet"
    else
        unexpected($module, $pred, "invalid determinism")
    ),
    % XXX consider inlining the predicate being called
    table_generate_call(ConsumePredName, Detism, [ConsumerVar, AnswerBlockVar],
        purity_impure, instmap_delta_bind_var(AnswerBlockVar), ModuleInfo,
        Context, GetNextAnswerGoal),
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, RestoreInstMapDeltaSrc,
        RestoreArgs, RestoreCodeStr),
    AnswerBlockArg = foreign_arg(AnswerBlockVar,
        yes(foreign_arg_name_mode(answer_block_name, in_mode)),
        answer_block_type, bp_native_if_possible),
    RestoreAllPredName = "table_mmos_restore_answers",
    table_generate_foreign_proc(RestoreAllPredName, detism_det,
        tabling_c_attributes_no_dupl, [AnswerBlockArg],
        RestoreArgs, RestoreCodeStr, purity_semipure,
        instmap_delta_from_assoc_list(RestoreInstMapDeltaSrc),
        ModuleInfo, Context, RestoreGoal),

    GoalExpr = conj(plain_conj,
        LookupSetupGoals ++ [GetNextAnswerGoal, RestoreGoal]),
    goal_info_init(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    module_info_pred_info(ModuleInfo, GeneratorPredId, GeneratorPredInfo),
    table_info_init(ModuleInfo, GeneratorPredInfo, ProcInfo0,
        GeneratorTableInfo0),
    do_own_stack_create_generator(GeneratorPredId, ProcId, GeneratorPredInfo,
        ProcInfo0, Statistics, Context, GeneratorPredVar, DebugArgStr,
        PickupInputVarCode, PickupForeignArgs,
        NumberedInputVars, NumberedOutputVars,
        OrigNonLocals, OrigInstMapDelta, !.VarTypes, !.VarSet,
        GeneratorTableInfo0, GeneratorTableInfo, InputSteps, OutputSteps),
    !TableInfo ^ table_module_info := GeneratorTableInfo ^ table_module_info.

:- pred generate_save_input_vars_code(assoc_list(foreign_arg, mer_mode)::in,
    module_info::in, int::in, list(foreign_arg)::out, string::out, string::out)
    is det.

generate_save_input_vars_code([], _, _, [], "", "").
generate_save_input_vars_code([InputArg - Mode | InputArgModes], ModuleInfo,
        Pos, [PickupArg | PickupArgs], SaveVarCode ++ SaveVarCodes,
        PickupVarCode ++ PickupVarCodes) :-
    InputArg = foreign_arg(InputVar, MaybeArgNameMode, Type, _),
    (
        MaybeArgNameMode = yes(foreign_arg_name_mode(InputVarName, _InMode))
    ;
        MaybeArgNameMode = no,
        unexpected($module, $pred, "no InputVarName")
    ),
    mode_get_insts(ModuleInfo, Mode, InitInst, _FinalInst),
    PickupMode = from_to_mode(free, InitInst),
    PickupArg = foreign_arg(InputVar,
        yes(foreign_arg_name_mode(InputVarName, PickupMode)),
        Type, bp_native_if_possible),
    SaveVarCode = "\t\tMR_table_mmos_save_input_arg(" ++
        int_to_string(Pos) ++ ", " ++ InputVarName ++ ");\n",
    PickupVarCode = "\t\tMR_table_mmos_pickup_input_arg(" ++
        int_to_string(Pos) ++ ", " ++ InputVarName ++ ");\n",
    generate_save_input_vars_code(InputArgModes, ModuleInfo, Pos + 1,
        PickupArgs, SaveVarCodes, PickupVarCodes).

:- pred do_own_stack_create_generator(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in,
    table_attr_statistics::in, term.context::in,
    prog_var::in, string::in, string::in, list(foreign_arg)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    set_of_progvar::in, instmap_delta::in,
    vartypes::in, prog_varset::in, table_info::in, table_info::out,
    list(table_step_desc)::in, list(table_step_desc)::out) is det.

do_own_stack_create_generator(PredId, ProcId, !.PredInfo, !.ProcInfo,
        Statistics, Context, GeneratorVar, DebugArgStr, PickupVarCode,
        PickupForeignArgs, NumberedInputVars, NumberedOutputVars,
        OrigNonLocals, OrigInstMapDelta, !.VarTypes, !.VarSet, !TableInfo,
        InputSteps, OutputSteps) :-
    ModuleInfo0 = !.TableInfo ^ table_module_info,

    proc_info_set_headvars(list.map(project_var, NumberedOutputVars),
        !ProcInfo),
    proc_info_set_argmodes(list.map(project_mode, NumberedOutputVars),
        !ProcInfo),
    PickupInstMapDeltaSrc0 = list.map(project_var_init_inst(ModuleInfo0),
        NumberedInputVars),
    PickupInstMapDeltaSrc = [GeneratorVar - ground_inst
        | PickupInstMapDeltaSrc0],
    PickupGeneratorCode = "\t\t" ++ generator_name ++
        " = MR_mmos_new_generator;\n",
    PickupGeneratorArg = foreign_arg(GeneratorVar,
        yes(foreign_arg_name_mode(generator_name, out_mode)),
        generator_type, bp_native_if_possible),
    table_generate_foreign_proc("table_mmos_pickup_inputs", detism_det,
        tabling_c_attributes_no_dupl, [PickupGeneratorArg], PickupForeignArgs,
        PickupGeneratorCode ++ PickupVarCode, purity_impure,
        instmap_delta_from_assoc_list(PickupInstMapDeltaSrc),
        ModuleInfo0, Context, PickupGoal),

    list.length(NumberedOutputVars, BlockSize),
    generate_own_stack_save_return_goal(NumberedOutputVars, GeneratorVar,
        PredId, ProcId, BlockSize, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, OutputSteps, SaveReturnAnswerGoals),

    proc_info_get_goal(!.ProcInfo, OrigGoal),
    OrigGoal = hlds_goal(_, OrigGoalInfo),

    MainGoalExpr = conj(plain_conj, [OrigGoal | SaveReturnAnswerGoals]),
    Detism = goal_info_get_determinism(OrigGoalInfo),
    set_of_var.insert(GeneratorVar, OrigNonLocals, NonLocals),
    goal_info_init(NonLocals, OrigInstMapDelta, Detism, purity_impure, Context,
        MainGoalInfo0),
    goal_info_add_feature(feature_hide_debug_event,
        MainGoalInfo0, MainGoalInfo),
    MainGoal = hlds_goal(MainGoalExpr, MainGoalInfo),

    CompletionCode = "\t\t" ++ "MR_tbl_mmos_completion(" ++
        DebugArgStr ++ ", " ++ generator_name ++ ");\n",
    CompletionArg = foreign_arg(GeneratorVar,
        yes(foreign_arg_name_mode(generator_name, in_mode)),
        generator_type, bp_native_if_possible),
    table_generate_foreign_proc("table_mmos_completion", detism_failure,
        tabling_c_attributes_no_dupl, [CompletionArg], [],
        CompletionCode, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo0, Context, CompletionGoal),

    DisjGoalExpr = disj([MainGoal, CompletionGoal]),
    DisjGoal = hlds_goal(DisjGoalExpr, MainGoalInfo),

    GoalExpr = conj(plain_conj, [PickupGoal, DisjGoal]),
    Goal = hlds_goal(GoalExpr, OrigGoalInfo),
    proc_info_set_goal(Goal, !ProcInfo),

    proc_info_set_vartypes(!.VarTypes, !ProcInfo),
    proc_info_set_varset(!.VarSet, !ProcInfo),

    InputVarModeMethods = list.map(project_out_pos, NumberedInputVars),
    OutputVarModeMethods = list.map(project_out_pos, NumberedOutputVars),
    generate_gen_proc_table_info(!.TableInfo, PredId, ProcId, InputSteps,
        yes(OutputSteps), InputVarModeMethods, OutputVarModeMethods,
        ProcTableStructInfo),

    PredProcId = proc(PredId, ProcId),
    add_proc_table_struct(PredProcId, ProcTableStructInfo, !.ProcInfo,
        ModuleInfo0, ModuleInfo1),

    SpecialReturn = generator_return(returning_generator_locn, DebugArgStr),
    proc_info_set_maybe_special_return(yes(SpecialReturn), !ProcInfo),
    proc_info_set_eval_method(eval_minimal(own_stacks_generator), !ProcInfo),

    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    map.det_insert(ProcId, !.ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, !PredInfo),

    module_info_get_preds(ModuleInfo1, PredTable0),
    map.det_update(PredId, !.PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, ModuleInfo1, ModuleInfo),
    !TableInfo ^ table_module_info := ModuleInfo.

:- pred clone_pred_info(pred_id::in, pred_info::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, pred_id::out,
    table_info::in, table_info::out) is det.

clone_pred_info(OrigPredId, PredInfo0, HeadVars, NumberedOutputVars,
        GeneratorPredId, !TableInfo) :-
    % We don't have any procedures for the generator yet. We will copy
    % the consumers' procedures later, one by one, as they are transformed.

    ModuleName = pred_info_module(PredInfo0),
    PredName0 = pred_info_name(PredInfo0),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo0),
    pred_info_get_context(PredInfo0, Context),
    % The generator is local even if the original predicate is exported.
    PredStatus = pred_status(status_local),
    pred_info_get_goal_type(PredInfo0, GoalType),
    pred_info_get_markers(PredInfo0, Markers0),
    pred_info_get_arg_types(PredInfo0, ArgTypes0),
    pred_info_get_typevarset(PredInfo0, TypeVarSet),
    pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
    pred_info_get_class_context(PredInfo0, ClassContext),
    pred_info_get_constraint_proof_map(PredInfo0, ClassProofMap),
    pred_info_get_constraint_map(PredInfo0, ClassConstraintMap),
    pred_info_get_origin(PredInfo0, OrigOrigin),
    pred_info_get_clauses_info(PredInfo0, ClausesInfo),
    pred_info_get_var_name_remap(PredInfo0, VarNameRemap),

    PredName = qualified(ModuleName, "GeneratorFor_" ++ PredName0),
    assoc_list.from_corresponding_lists(HeadVars, ArgTypes0, HeadVarTypes),
    keep_only_output_arg_types(HeadVarTypes, NumberedOutputVars, ArgTypes),
    Arity = list.length(ArgTypes),

    markers_to_marker_list(Markers0, MarkerList0),
    list.filter(filter_marker, MarkerList0, MarkerList),
    marker_list_to_markers(MarkerList, Markers),

    Origin = origin_transformed(transform_table_generator,
        OrigOrigin, OrigPredId),
    CurUserDecl = maybe.no,
    pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
        Origin, PredStatus, CurUserDecl, GoalType, Markers,
        ArgTypes, TypeVarSet, ExistQVars, ClassContext, ClassProofMap,
        ClassConstraintMap, ClausesInfo, VarNameRemap, PredInfo),

    ModuleInfo0 = !.TableInfo ^ table_module_info,
    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(PredInfo, GeneratorPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo),
    !TableInfo ^ table_module_info := ModuleInfo.

    % clone_proc_and_create_call(PredInfo, ProcId, CallExpr, !ModuleInfo).
    % This predicate creates a new procedure with the same body as the
    % procedure with ProcId in PredInfo.  It then creates a call goal
    % expression which calls the new procedure with its formal arguments as the
    % actual arguments.
    %
:- pred clone_proc_and_create_call(pred_info::in, proc_id::in,
    hlds_goal_expr::out, module_info::in, module_info::out) is det.

clone_proc_and_create_call(PredInfo, ProcId, CallExpr, !ModuleInfo) :-
    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    proc_info_get_context(ProcInfo, ProcContext),
    proc_info_get_varset(ProcInfo, ProcVarSet),
    proc_info_get_vartypes(ProcInfo, ProcVarTypes),
    proc_info_get_headvars(ProcInfo, ProcHeadVars),
    proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
    proc_info_get_argmodes(ProcInfo, ProcHeadModes),
    proc_info_get_inferred_determinism(ProcInfo, ProcDetism),
    proc_info_get_goal(ProcInfo, ProcGoal),
    proc_info_get_rtti_varmaps(ProcInfo, ProcRttiVarMaps),
    proc_info_get_has_parallel_conj(ProcInfo, HasParallelConj),
    proc_info_get_var_name_remap(ProcInfo, VarNameRemap),
    ItemNumber = -1,
    proc_info_create(ProcContext, ItemNumber,
        ProcVarSet, ProcVarTypes, ProcHeadVars,
        ProcInstVarSet, ProcHeadModes, detism_decl_none, ProcDetism, ProcGoal,
        ProcRttiVarMaps, address_is_not_taken, HasParallelConj, VarNameRemap,
        NewProcInfo),
    ModuleName = pred_info_module(PredInfo),
    OrigPredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_context(PredInfo, PredContext),
    NewPredName = qualified(ModuleName, "OutlinedForIOTablingFrom_" ++
        OrigPredName),
    pred_info_get_arg_types(PredInfo, PredArgTypes),
    pred_info_get_typevarset(PredInfo, PredTypeVarSet),
    pred_info_get_exist_quant_tvars(PredInfo, PredExistQVars),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_assertions(PredInfo, PredAssertions),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_create(ModuleName, NewPredName, PredOrFunc, PredContext,
        origin_created(created_by_io_tabling), pred_status(status_local),
        Markers, PredArgTypes, PredTypeVarSet, PredExistQVars,
        PredClassContext, PredAssertions, VarNameRemap,
        NewProcInfo, NewProcId, NewPredInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_insert(NewPredInfo, NewPredId,
        PredicateTable0, PredicateTable),
    module_info_set_predicate_table(PredicateTable, !ModuleInfo),
    CallExpr = plain_call(NewPredId, NewProcId, ProcHeadVars, not_builtin, no,
        NewPredName).

:- pred keep_only_output_arg_types(assoc_list(prog_var, mer_type)::in,
    list(var_mode_pos_method)::in, list(mer_type)::out) is det.

keep_only_output_arg_types([], _, []).
keep_only_output_arg_types([_ | _], [], []).
keep_only_output_arg_types([Var - Type | VarTypes], [Out | Outs], OutTypes) :-
    Out = var_mode_pos_method(OutVar, _, _, _),
    ( if Var = OutVar then
        keep_only_output_arg_types(VarTypes, Outs, OutTypesTail),
        OutTypes = [Type | OutTypesTail]
    else
        keep_only_output_arg_types(VarTypes, [Out | Outs], OutTypes)
    ).

:- pred filter_marker(pred_marker::in) is semidet.

filter_marker(Marker) :-
    keep_marker(Marker) = yes.

:- func keep_marker(pred_marker) = bool.

keep_marker(marker_stub) = no.
keep_marker(marker_builtin_stub) = no.
keep_marker(marker_infer_type) = no.
keep_marker(marker_infer_modes) = no.
keep_marker(marker_no_pred_decl) = no.
keep_marker(marker_obsolete) = no.
keep_marker(marker_no_detism_warning) = no.
keep_marker(marker_user_marked_inline) = no.
keep_marker(marker_user_marked_no_inline) = no.
keep_marker(marker_heuristic_inline) = no.
keep_marker(marker_consider_used) = no.
keep_marker(marker_class_method) = no.
keep_marker(marker_class_instance_method) = no.
keep_marker(marker_named_class_instance_method) = no.
keep_marker(marker_is_impure) = yes.
keep_marker(marker_is_semipure) = yes.
keep_marker(marker_promised_pure) = yes.
keep_marker(marker_promised_semipure) = yes.
keep_marker(marker_promised_equivalent_clauses) = yes.
keep_marker(marker_terminates) = yes.
keep_marker(marker_does_not_terminate) = yes.
keep_marker(marker_check_termination) = no.
keep_marker(marker_calls_are_fully_qualified) = yes.
keep_marker(marker_mode_check_clauses) = yes.
keep_marker(marker_mutable_access_pred) = yes.
keep_marker(marker_has_require_scope) = yes.
keep_marker(marker_has_incomplete_switch) = yes.
keep_marker(marker_has_format_call) = yes.

%-----------------------------------------------------------------------------%

:- pred generate_gen_proc_table_info(table_info::in, pred_id::in, proc_id::in,
    list(table_step_desc)::in, maybe(list(table_step_desc))::in,
    list(var_mode_method)::in, list(var_mode_method)::in,
    proc_table_struct_info::out) is det.

generate_gen_proc_table_info(TableInfo, PredId, ProcId,
        InputSteps, MaybeOutputSteps, InputVars, OutputVars,
        ProcTableStructInfo) :-
    ModuleInfo = TableInfo ^ table_module_info,
    RTTIProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),

    PredInfo = TableInfo ^ table_cur_pred_info,
    pred_info_get_typevarset(PredInfo, TVarSet),
    ProcInfo = TableInfo ^ table_cur_proc_info,
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    proc_info_get_context(ProcInfo, Context),

    InOutHeadVars = InputVars ++ OutputVars,
    allocate_slot_numbers(InOutHeadVars, 1, NumberedInOutHeadVars),
    ArgInfos = list.map(project_var_pos, NumberedInOutHeadVars),
    continuation_info.generate_table_arg_type_info(ProcInfo, ArgInfos,
        TableArgTypeInfo),
    NumInputs = list.length(InputVars),
    NumOutputs = list.length(OutputVars),

    ProcTableStructInfo = proc_table_struct_info(RTTIProcLabel, TVarSet,
        Context, NumInputs, NumOutputs, InputSteps, MaybeOutputSteps,
        TableArgTypeInfo, EvalMethod).

%-----------------------------------------------------------------------------%

    % Generate a goal for doing lookups in call tables for
    % loopcheck and memo predicates.
    %
:- pred generate_simple_call_table_lookup_goal(mer_type::in,
    string::in, string::in, list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, prog_var::out,
    hlds_goal::out, list(table_step_desc)::out) is det.

generate_simple_call_table_lookup_goal(StatusType, PredName,
        SetupMacroName, NumberedVars, PredId, ProcId, Statistics, Context,
        !VarSet, !VarTypes, !TableInfo, TableTipVar, StatusVar, Goal, Steps) :-
    generate_call_table_lookup_goals(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, Steps,
        TableTipVar, TableTipArg, InfoArg, LookupForeignArgs,
        LookupPrefixGoals, LookupCodeStr, CallTableTipAssignStr),
    generate_new_table_var("Status", StatusType, !VarSet, !VarTypes,
        StatusVar),
    ModuleInfo = !.TableInfo ^ table_module_info,

    StatusVarName = status_name,
    StatusArg = foreign_arg(StatusVar,
        yes(foreign_arg_name_mode(StatusVarName, out_mode)),
        StatusType, bp_native_if_possible),
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = get_back_arg_string(!.TableInfo),
    MainPredCodeStr = "\t" ++ SetupMacroName ++ "(" ++
        DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
        cur_table_node_name ++ ", " ++ StatusVarName ++ ");\n",
    Args = [InfoArg, TableTipArg, StatusArg],
    BoundVars = [TableTipVar, StatusVar],
    CodeStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n\n" ++
        LookupCodeStr ++
        CallTableTipAssignStr ++
        MainPredCodeStr,
    table_generate_foreign_proc(PredName, detism_det,
        tabling_c_attributes_no_dupl,
        Args, LookupForeignArgs, CodeStr, purity_impure,
        instmap_delta_bind_vars(BoundVars), ModuleInfo, Context, SetupGoal0),
    attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = LookupPrefixGoals ++ [SetupGoal],

    GoalExpr = conj(plain_conj, LookupSetupGoals),
    Vars = list.map(project_var, NumberedVars),
    set_of_var.list_to_set([StatusVar, TableTipVar | Vars], NonLocals),
    goal_info_init_hide(NonLocals,
        instmap_delta_bind_vars([TableTipVar, StatusVar]),
        detism_det, purity_impure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Generate a goal for doing lookups in call tables for
    % model_non memo predicates.
    %
:- pred generate_memo_non_call_table_lookup_goal(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, prog_var::out,
    hlds_goal::out, list(table_step_desc)::out) is det.

generate_memo_non_call_table_lookup_goal(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo,
        RecordVar, StatusVar, Goal, Steps) :-
    generate_call_table_lookup_goals(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, Steps,
        _TableTipVar, _TableTipArg, InfoArg, LookupForeignArgs,
        LookupPrefixGoals, LookupCodeStr, _CallTableTipAssignStr),
    ModuleInfo = !.TableInfo ^ table_module_info,
    generate_new_table_var("Record", memo_non_record_type, !VarSet, !VarTypes,
        RecordVar),
    generate_new_table_var("Status", memo_non_status_type, !VarSet, !VarTypes,
        StatusVar),
    SetupPredName = "table_memo_non_setup",
    SetupMacroName = "MR_tbl_memo_non_setup",
    BoundVars = [RecordVar, StatusVar],
    RecordVarName = memo_non_record_name,
    StatusVarName = status_name,
    RecordArg = foreign_arg(RecordVar,
        yes(foreign_arg_name_mode(RecordVarName, out_mode)),
        memo_non_record_type, bp_native_if_possible),
    StatusArg = foreign_arg(StatusVar,
        yes(foreign_arg_name_mode(StatusVarName, out_mode)),
        memo_non_status_type, bp_native_if_possible),
    Args = [InfoArg, RecordArg, StatusArg],
    LookupDeclCodeStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n\n" ++
        LookupCodeStr,
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = get_back_arg_string(!.TableInfo),
    PredCodeStr = "\t" ++ SetupMacroName ++ "(" ++
        DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
        cur_table_node_name ++ ", " ++ RecordVarName ++ ", " ++
        StatusVarName ++ ");\n",
    table_generate_foreign_proc(SetupPredName, detism_det,
        tabling_c_attributes_no_dupl, Args, LookupForeignArgs,
        LookupDeclCodeStr ++ PredCodeStr, purity_impure,
        instmap_delta_bind_vars(BoundVars), ModuleInfo, Context, SetupGoal0),
    attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = LookupPrefixGoals ++ [SetupGoal],

    GoalExpr = conj(plain_conj, LookupSetupGoals),
    Vars = list.map(project_var, NumberedVars),
    set_of_var.list_to_set([StatusVar, RecordVar | Vars], NonLocals),
    goal_info_init_hide(NonLocals,
        instmap_delta_bind_vars([RecordVar, StatusVar]),
        detism_det, purity_impure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Generate a goal for doing lookups in call tables for
    % minimal model predicates.
    %
:- pred generate_mm_call_table_lookup_goal(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, prog_var::out,
    hlds_goal::out, list(table_step_desc)::out) is det.

generate_mm_call_table_lookup_goal(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo,
        SubgoalVar, StatusVar, Goal, Steps) :-
    generate_call_table_lookup_goals(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, Steps,
        _TableTipVar, _TableTipArg, InfoArg, LookupForeignArgs,
        LookupPrefixGoals, LookupCodeStr, _CallTableTipAssignStr),
    generate_new_table_var("Subgoal", subgoal_type, !VarSet, !VarTypes,
        SubgoalVar),
    generate_new_table_var("Status", mm_status_type, !VarSet, !VarTypes,
        StatusVar),
    SetupPredName = "table_mm_setup",
    SetupMacroName = "MR_tbl_mm_setup",
    BoundVars = [SubgoalVar, StatusVar],

    SubgoalVarName = subgoal_name,
    StatusVarName = status_name,
    SubgoalArg = foreign_arg(SubgoalVar,
        yes(foreign_arg_name_mode(SubgoalVarName, out_mode)),
        subgoal_type, bp_native_if_possible),
    StatusArg = foreign_arg(StatusVar,
        yes(foreign_arg_name_mode(StatusVarName, out_mode)),
        mm_status_type, bp_native_if_possible),
    Args = [InfoArg, SubgoalArg, StatusArg],
    LookupDeclStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n\n",
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = get_back_arg_string(!.TableInfo),
    SetupCodeStr = "\t" ++ SetupMacroName ++ "(" ++
        DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
        cur_table_node_name ++ ", " ++ SubgoalVarName ++ ", " ++
        StatusVarName ++ ");\n",
    CodeStr = LookupDeclStr ++ LookupCodeStr ++ SetupCodeStr,
    ModuleInfo = !.TableInfo ^ table_module_info,
    table_generate_foreign_proc(SetupPredName, detism_det,
        tabling_c_attributes_no_dupl, Args, LookupForeignArgs, CodeStr,
        purity_impure, instmap_delta_bind_vars(BoundVars), ModuleInfo, Context,
        SetupGoal0),
    attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = LookupPrefixGoals ++ [SetupGoal],

    GoalExpr = conj(plain_conj, LookupSetupGoals),
    Vars = list.map(project_var, NumberedVars),
    set_of_var.list_to_set([StatusVar, SubgoalVar | Vars], NonLocals),
    goal_info_init_hide(NonLocals,
        instmap_delta_bind_vars([SubgoalVar, StatusVar]),
        detism_det, purity_impure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

% Utility predicates used when creating table lookup goals.

:- pred generate_call_table_lookup_goals(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_step_desc)::out,
    prog_var::out, foreign_arg::out, foreign_arg::out, list(foreign_arg)::out,
    list(hlds_goal)::out, string::out, string::out) is det.

generate_call_table_lookup_goals(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, InputSteps,
        CallTableTipVar, CallTableTipArg, InfoArg, LookupArgs,
        PrefixGoals, MainCodeStr, CallTableTipVarCodeStr) :-
    InfoToPtrCodeStr = "\t" ++ cur_table_node_name ++ " = " ++
        "&" ++ proc_table_info_name ++ "->MR_pt_tablenode;\n",
    generate_get_table_info_goal(PredId, ProcId, Context, !VarSet, !VarTypes,
        proc_table_info_name, InfoArg, GetTableInfoGoal),
    MaybeStatsRef = stats_ref(Statistics, call_table),
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = get_back_arg_string(!.TableInfo),
    generate_table_lookup_goals(NumberedVars, MaybeStatsRef,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        InputSteps, LookupArgs, LookupPrefixGoals, LookupCodeStr),
    PrefixGoals = [GetTableInfoGoal] ++ LookupPrefixGoals,
    % We ignore _StatsPrefixGoals and _StatsExtraArgs because we always
    % include ProcTableInfoVar in the arguments.
    maybe_record_overall_stats(PredId, ProcId, Context,
        proc_table_info_name, cur_table_node_name,
        MaybeStatsRef, !VarSet, !VarTypes,
        _StatsPrefixGoals, _StatsExtraArgs, StatsCodeStr),
    MainCodeStr = InfoToPtrCodeStr ++ LookupCodeStr ++ StatsCodeStr,
    CallTableTipVarName = "CallTableTipVar",
    generate_new_table_var(CallTableTipVarName, trie_node_type,
        !VarSet, !VarTypes, CallTableTipVar),
    CallTableTipArg = foreign_arg(CallTableTipVar,
        yes(foreign_arg_name_mode(CallTableTipVarName, out_mode)),
        trie_node_type, bp_native_if_possible),
    CallTableTipVarCodeStr =
        "\t" ++ CallTableTipVarName ++ " = " ++ cur_table_node_name ++ ";\n".

:- pred generate_answer_table_lookup_goals(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_step_desc)::out,
    list(foreign_arg)::out, list(hlds_goal)::out, string::out) is det.

generate_answer_table_lookup_goals(NumberedVars, PredId, ProcId, Statistics,
        Context, !VarSet, !VarTypes, !TableInfo, OutputSteps, ForeignArgs,
        PrefixGoals, CodeStr) :-
    MaybeStatsRef = stats_ref(Statistics, answer_table),
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = "MR_FALSE",
    generate_table_lookup_goals(NumberedVars, MaybeStatsRef,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        OutputSteps, LookupArgs, LookupPrefixGoals, LookupCodeStr),
    maybe_record_overall_stats(PredId, ProcId, Context,
        proc_table_info_name, cur_table_node_name,
        MaybeStatsRef, !VarSet, !VarTypes,
        StatsPrefixGoals, StatsExtraArgs, StatsCodeStr),
    CodeStr = LookupCodeStr ++ StatsCodeStr,
    ForeignArgs = StatsExtraArgs ++ LookupArgs,
    PrefixGoals = StatsPrefixGoals ++ LookupPrefixGoals.

:- pred maybe_record_overall_stats(pred_id::in, proc_id::in, prog_context::in,
    string::in, string::in, maybe(string)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    list(hlds_goal)::out, list(foreign_arg)::out, string::out) is det.

maybe_record_overall_stats(PredId, ProcId, Context, InfoVarName, TipVarName,
        MaybeStatsRef, !VarSet, !VarTypes, PrefixGoals, Args, StatsCodeStr) :-
    (
        MaybeStatsRef = no,
        PrefixGoals = [],
        Args = [],
        StatsCodeStr = ""
    ;
        MaybeStatsRef = yes(StatsRef),
        generate_get_table_info_goal(PredId, ProcId, Context,
            !VarSet, !VarTypes, InfoVarName, Arg, Goal),
        PrefixGoals = [Goal],
        Args = [Arg],
        StatsCodeStr =
            "\t" ++ StatsRef ++ ".MR_ts_num_lookups++;\n" ++
            "\t" ++ "if (MR_trie_node_seen_before(" ++ TipVarName ++ ")) " ++
                "{\n" ++
            "\t\t" ++ StatsRef ++ ".MR_ts_num_lookups_is_dupl++;\n" ++
            "\t" ++ "}\n"
    ).

:- pred generate_get_table_info_goal(pred_id::in, proc_id::in,
    prog_context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    string::in, foreign_arg::out, hlds_goal::out) is det.

generate_get_table_info_goal(PredId, ProcId, Context, !VarSet, !VarTypes,
        InfoVarName, Arg, Goal) :-
    generate_new_table_var("ProcTableInfo", proc_table_info_type,
        !VarSet, !VarTypes, ProcTableInfoVar),
    Arg = foreign_arg(ProcTableInfoVar,
        yes(foreign_arg_name_mode(InfoVarName, in_mode)),
        proc_table_info_type, bp_native_if_possible),
    ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
    InfoConsId = tabling_info_const(ShroudedPredProcId),
    make_const_construction(Context, ProcTableInfoVar, InfoConsId,
        hlds_goal(GoalExpr, GoalInfo0)),
    goal_info_set_purity(purity_impure, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred attach_call_table_tip(hlds_goal::in, hlds_goal::out) is det.

attach_call_table_tip(hlds_goal(GoalExpr, GoalInfo0),
        hlds_goal(GoalExpr, GoalInfo)) :-
    Features0 = goal_info_get_features(GoalInfo0),
    set.insert(feature_call_table_gen, Features0, Features),
    goal_info_set_features(Features, GoalInfo0, GoalInfo).

%-----------------------------------------------------------------------------%

    % Generate a sequence of lookup goals for the given variables.
    % The generated code is used for lookups in both call tables
    % and answer tables.
    %
:- pred generate_table_lookup_goals(list(var_mode_pos_method)::in,
    maybe(string)::in, string::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_step_desc)::out,
    list(foreign_arg)::out, list(hlds_goal)::out, string::out) is det.

generate_table_lookup_goals([], _, _, _, _, !VarSet, !VarTypes, !TableInfo,
        [], [], [], "").
generate_table_lookup_goals([VarModePos | NumberedVars], MaybeStatsRef,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        [StepDesc | StepDescs], ForeignArgs ++ RestForeignArgs,
        PrefixGoals ++ RestPrefixGoals, CodeStr ++ RestCodeStr) :-
    VarModePos = var_mode_pos_method(Var, _, VarSeqNum, ArgMethod),
    varset.lookup_name(!.VarSet, Var, VarName),
    ModuleInfo = !.TableInfo ^ table_module_info,
    lookup_var_type(!.VarTypes, Var, VarType),
    CtorCat = classify_type(ModuleInfo, VarType),
    (
        ArgMethod = arg_promise_implied,
        Step = table_trie_step_promise_implied,
        ForeignArgs = [],
        PrefixGoals = [],
        CodeStr = "\t/* promise_implied for " ++ arg_name(VarSeqNum) ++ " */\n"
    ;
        ( ArgMethod = arg_value
        ; ArgMethod = arg_addr
        ),
        gen_lookup_call_for_type(ArgMethod, CtorCat, VarType, Var,
            VarSeqNum, MaybeStatsRef, DebugArgStr, BackArgStr, Context,
            !VarSet, !VarTypes, !TableInfo, Step, ForeignArgs,
            PrefixGoals, CodeStr)
    ),
    StepDesc = table_step_desc(VarName, Step),
    generate_table_lookup_goals(NumberedVars, MaybeStatsRef,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        StepDescs, RestForeignArgs, RestPrefixGoals, RestCodeStr).

:- pred gen_lookup_call_for_type(arg_tabling_method::in,
    type_ctor_category::in, mer_type::in, prog_var::in, int::in,
    maybe(string)::in, string::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out,
    table_trie_step::out, list(foreign_arg)::out, list(hlds_goal)::out,
    string::out) is det.

gen_lookup_call_for_type(ArgTablingMethod0, CtorCat, Type, ArgVar, VarSeqNum,
        MaybeStatsRef, DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes,
        !TableInfo, Step, ExtraArgs, PrefixGoals, CodeStr) :-
    ModuleInfo = !.TableInfo ^ table_module_info,
    ArgName = arg_name(VarSeqNum),
    ForeignArg = foreign_arg(ArgVar,
        yes(foreign_arg_name_mode(ArgName, in_mode)),
        Type, bp_native_if_possible),
    (
        ( CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin(cat_builtin_int(_))
        ; CtorCat = ctor_cat_builtin(cat_builtin_char)
        ; CtorCat = ctor_cat_void
        ),
        % Values in these type categories don't have an address.
        ( if ArgTablingMethod0 = arg_addr then
            ArgTablingMethod = arg_value
        else
            ArgTablingMethod = ArgTablingMethod0
        )
    ;
        ( CtorCat = ctor_cat_builtin(cat_builtin_string)
        ; CtorCat = ctor_cat_builtin(cat_builtin_float)
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        ArgTablingMethod = ArgTablingMethod0
    ),
    MaybeStepStatsArgStr = maybe_step_stats_arg_addr(MaybeStatsRef, VarSeqNum),
    (
        ArgTablingMethod = arg_value,
        (
            CtorCat = ctor_cat_enum(cat_enum_mercury),
            type_to_ctor_det(Type, TypeCtor),
            module_info_get_type_table(ModuleInfo, TypeTable),
            lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            ( if
                TypeBody = hlds_du_type(Ctors, MaybeCanonical, MaybeRepn,
                    _MaybeForeign),
                MaybeCanonical = canon,
                MaybeRepn = yes(Repn),
                Repn ^ dur_kind = du_type_kind_mercury_enum
            then
                list.length(Ctors, EnumRange)
            else
                unexpected($module, $pred, "enum type is not du_type?")
            ),
            LookupMacroName = "MR_tbl_lookup_insert_enum",
            Step = table_trie_step_enum(EnumRange),
            PrefixGoals = [],
            ExtraArgs = [ForeignArg],
            LookupCodeStr = "\t" ++ LookupMacroName ++ "(" ++
                MaybeStepStatsArgStr ++ ", " ++ DebugArgStr ++ ", " ++
                BackArgStr ++ ", " ++ cur_table_node_name ++ ", " ++
                int_to_string(EnumRange) ++ ", " ++ ArgName ++ ", " ++
                next_table_node_name ++ ");\n"
        ;
            % Mercury doesn't know the specific values of the foreign
            % enums, so we cannot use an array as a trie (since we don't
            % know how big the array would have to be). However, hashing
            % the foreign enum will work.
            %
            % XXX The code of this case is the same as the code of the case
            % shared by the builtin types below. The only reason why it is
            % here is that switch detection cannot yet look three levels deep.

            CtorCat = ctor_cat_enum(cat_enum_foreign),
            CatString = "foreign_enum",
            Step = table_trie_step_foreign_enum,
            LookupMacroName = "MR_tbl_lookup_insert_" ++ CatString,
            PrefixGoals = [],
            ExtraArgs = [ForeignArg],
            LookupCodeStr = "\t" ++ LookupMacroName ++ "(" ++
                MaybeStepStatsArgStr ++ ", " ++ DebugArgStr ++ ", " ++
                BackArgStr ++ ", " ++ cur_table_node_name ++ ", " ++
                ArgName ++ ", " ++ next_table_node_name ++ ");\n"
        ;
            % XXX The code of this case is the same as the code of the case
            % shared by the builtin types below. The only reason why it is
            % here is that switch detection cannot yet look three levels deep.

            ( CtorCat = ctor_cat_system(cat_system_type_info)
            ; CtorCat = ctor_cat_system(cat_system_type_ctor_info)
            ),
            CatString = "typeinfo",
            Step = table_trie_step_typeinfo,
            LookupMacroName = "MR_tbl_lookup_insert_" ++ CatString,
            PrefixGoals = [],
            ExtraArgs = [ForeignArg],
            LookupCodeStr = "\t" ++ LookupMacroName ++ "(" ++
                MaybeStepStatsArgStr ++ ", " ++ DebugArgStr ++ ", " ++
                BackArgStr ++ ", " ++ cur_table_node_name ++ ", " ++
                ArgName ++ ", " ++ next_table_node_name ++ ");\n"
        ;
            (
                CtorCat = ctor_cat_builtin(cat_builtin_int(IntType)),
                int_type_to_string(IntType, CatString),
                Step = table_trie_step_int(IntType)
            ;
                CtorCat = ctor_cat_builtin(cat_builtin_char),
                CatString = "char",
                Step = table_trie_step_char
            ;
                CtorCat = ctor_cat_builtin(cat_builtin_string),
                CatString = "string",
                Step = table_trie_step_string
            ;
                CtorCat = ctor_cat_builtin(cat_builtin_float),
                CatString = "float",
                Step = table_trie_step_float
            ),
            LookupMacroName = "MR_tbl_lookup_insert_" ++ CatString,
            PrefixGoals = [],
            ExtraArgs = [ForeignArg],
            LookupCodeStr = "\t" ++ LookupMacroName ++ "(" ++
                MaybeStepStatsArgStr ++ ", " ++ DebugArgStr ++ ", " ++
                BackArgStr ++ ", " ++ cur_table_node_name ++ ", " ++
                ArgName ++ ", " ++ next_table_node_name ++ ");\n"
        ;
            ( CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_user(_)
            ),
            MaybeAddrString = "",
            IsAddr = table_value,
            gen_general_lookup_call(IsAddr, MaybeAddrString, Type, ForeignArg,
                ArgName, VarSeqNum, MaybeStatsRef, DebugArgStr, BackArgStr,
                Context, !VarSet, !VarTypes, !TableInfo, Step, ExtraArgs,
                PrefixGoals, LookupCodeStr)
        ;
            CtorCat = ctor_cat_builtin_dummy,
            Step = table_trie_step_dummy,
            PrefixGoals = [],
            ExtraArgs = [],
            LookupCodeStr = "\t" ++ next_table_node_name ++ " = " ++
                cur_table_node_name ++ ";\n"
        ;
            CtorCat = ctor_cat_void,
            unexpected($module, $pred, "void")
        ;
            CtorCat = ctor_cat_system(cat_system_typeclass_info),
            unexpected($module, $pred, "typeclass_info_type")
        ;
            CtorCat = ctor_cat_system(cat_system_base_typeclass_info),
            unexpected($module, $pred, "base_typeclass_info_type")
        )
    ;
        ArgTablingMethod = arg_addr,
        (
            CtorCat = ctor_cat_enum(_),
            unexpected($module, $pred, "tabling enums by addr")
        ;
            CtorCat = ctor_cat_builtin(cat_builtin_int(_)),
            unexpected($module, $pred, "tabling integer type by addr")
        ;
            CtorCat = ctor_cat_builtin(cat_builtin_char),
            unexpected($module, $pred, "tabling chars by addr")
        ;
            ( CtorCat = ctor_cat_builtin(cat_builtin_string)
            ; CtorCat = ctor_cat_builtin(cat_builtin_float)
            ; CtorCat = ctor_cat_system(_)
            ; CtorCat = ctor_cat_higher_order
            ; CtorCat = ctor_cat_tuple
            ; CtorCat = ctor_cat_variable
            ; CtorCat = ctor_cat_user(_)
            ),
            MaybeAddrString = "_addr",
            IsAddr = table_addr,
            gen_general_lookup_call(IsAddr, MaybeAddrString, Type, ForeignArg,
                ArgName, VarSeqNum, MaybeStatsRef, DebugArgStr, BackArgStr,
                Context, !VarSet, !VarTypes, !TableInfo, Step, ExtraArgs,
                PrefixGoals, LookupCodeStr)
        ;
            CtorCat = ctor_cat_builtin_dummy,
            unexpected($module, $pred, "tabling dummies by addr")
        ;
            CtorCat = ctor_cat_void,
            unexpected($module, $pred, "void")
        )
    ;
        ArgTablingMethod = arg_promise_implied,
        unexpected($module, $pred, "arg_promise_implied")
    ),
    UpdateCurNodeCodeStr = "\t" ++ cur_table_node_name ++ " = " ++
        next_table_node_name ++ ";\n",
    (
        MaybeStatsRef = no,
        CodeStr = LookupCodeStr ++ UpdateCurNodeCodeStr
    ;
        MaybeStatsRef = yes(StatsRef),
        StepStatsArgStr = step_stats_arg_addr(StatsRef, VarSeqNum),
        NextVarName = next_table_node_name,
        LookupStatsCodeStr =
            "\t" ++ StepStatsArgStr ++ ".MR_tss_num_lookups++;\n" ++
            "\t" ++ "if (MR_trie_node_seen_before(" ++ NextVarName ++ "))" ++
                "{\n" ++
            "\t\t" ++ StepStatsArgStr ++ ".MR_tss_num_lookups_is_dupl++;\n" ++
            "\t" ++ "}\n",
        CodeStr = LookupCodeStr ++ LookupStatsCodeStr ++ UpdateCurNodeCodeStr
    ).

:- pred gen_general_lookup_call(table_value_or_addr::in, string::in,
    mer_type::in, foreign_arg::in, string::in, int::in, maybe(string)::in,
    string::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out,
    table_trie_step::out, list(foreign_arg)::out, list(hlds_goal)::out,
    string::out) is det.

gen_general_lookup_call(IsAddr, MaybeAddrString, Type, ForeignArg, ArgName,
        VarSeqNum, MaybeStatsRef, DebugArgStr, BackArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Step, ExtraArgs, PrefixGoals,
        LookupCodeStr) :-
    type_vars(Type, TypeVars),
    (
        TypeVars = [],
        MaybePolyString = "",
        IsPoly = table_is_mono
    ;
        TypeVars = [_ | _],
        MaybePolyString = "_poly",
        IsPoly = table_is_poly
    ),
    Step = table_trie_step_general(Type, IsPoly, IsAddr),
    LookupMacroName = "MR_tbl_lookup_insert_gen" ++
        MaybePolyString ++ MaybeAddrString,
    table_gen_make_type_info_var(Type, Context, !VarSet, !VarTypes,
        !TableInfo, TypeInfoVar, PrefixGoals),
    TypeInfoArgName = "input_typeinfo" ++ int_to_string(VarSeqNum),
    lookup_var_type(!.VarTypes, TypeInfoVar, TypeInfoType),
    ForeignTypeInfoArg = foreign_arg(TypeInfoVar,
        yes(foreign_arg_name_mode(TypeInfoArgName, in_mode)),
        TypeInfoType, bp_native_if_possible),
    ExtraArgs = [ForeignTypeInfoArg, ForeignArg],
    StepStatsArgStr = maybe_step_stats_arg_addr(MaybeStatsRef, VarSeqNum),
    LookupCodeStr = "\t" ++ LookupMacroName ++ "(" ++
        StepStatsArgStr ++ ", " ++ DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
        cur_table_node_name ++ ", " ++ TypeInfoArgName ++ ", " ++
        ArgName ++ ", " ++ next_table_node_name ++ ");\n".

%-----------------------------------------------------------------------------%

    % Generate a goal for saving the output arguments in an answer block
    % in memo predicates.
    %
:- pred generate_memo_save_goal(list(var_mode_pos_method(T))::in,
    prog_var::in, int::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(hlds_goal)::out) is det.

generate_memo_save_goal(NumberedSaveVars, TableTipVar, BlockSize,
        Context, !VarSet, !VarTypes, !TableInfo, Goals) :-
    ModuleInfo = !.TableInfo ^ table_module_info,
    TipVarName = cur_table_node_name,
    TableArg = foreign_arg(TableTipVar,
        yes(foreign_arg_name_mode(TipVarName, in_mode)),
        trie_node_type, bp_native_if_possible),
    ( if BlockSize > 0 then
        CreatePredName = "table_memo_fill_answer_block_shortcut",
        CreateMacroName = "MR_tbl_memo_create_answer_block",
        generate_all_save_goals(NumberedSaveVars, TipVarName,
            BlockSize, CreateMacroName, Context, !VarSet, !VarTypes,
            !TableInfo, SaveArgs, SavePrefixGoals, SaveDeclCode, SaveCode),
        table_generate_foreign_proc(CreatePredName, detism_det,
            tabling_c_attributes_dupl, [TableArg], SaveArgs,
            SaveDeclCode ++ SaveCode, purity_impure, instmap_delta_bind_no_var,
            ModuleInfo, Context, SaveGoal),
        Goals = SavePrefixGoals ++ [SaveGoal]
    else
        DebugArgStr = get_debug_arg_string(!.TableInfo),
        MarkAsSucceededPredName = "table_memo_mark_as_succeeded",
        MarkAsSucceededMacroName = "MR_tbl_memo_mark_as_succeeded",
        MarkAsSucceededCode = MarkAsSucceededMacroName ++
            "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");",
        table_generate_foreign_proc(MarkAsSucceededPredName, detism_det,
            tabling_c_attributes_dupl, [TableArg], [],
            MarkAsSucceededCode, purity_impure, instmap_delta_bind_no_var,
            ModuleInfo, Context, SaveGoal),
        Goals = [SaveGoal]
    ).

    % Generate a goal for saving the output arguments in an answer block
    % in model_non memo predicates.
    %
:- pred generate_memo_non_save_goals(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, prog_var::in, int::in,
    table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_step_desc)::out,
    list(hlds_goal)::out) is det.

generate_memo_non_save_goals(NumberedSaveVars, PredId, ProcId,
        RecordVar, BlockSize, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, OutputSteps, Goals) :-
    ModuleInfo = !.TableInfo ^ table_module_info,
    RecordName = memo_non_record_name,
    RecordArg = foreign_arg(RecordVar,
        yes(foreign_arg_name_mode(RecordName, in_mode)),
        memo_non_record_type, bp_native_if_possible),

    generate_answer_table_lookup_goals(NumberedSaveVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, OutputSteps,
        LookupForeignArgs, LookupPrefixGoals, LookupCodeStr),

    CreateAnswerBlockMacroName = "MR_tbl_memo_non_create_answer_block",
    generate_all_save_goals(NumberedSaveVars, memo_non_record_name, BlockSize,
        CreateAnswerBlockMacroName, Context, !VarSet, !VarTypes, !TableInfo,
        _SaveForeignArgs, _SavePrefixGoals, SaveDeclCodeStr, CreateSaveCode),

    GetMacroName = "MR_tbl_memo_non_get_answer_table",
    DuplCheckPredName = "table_memo_non_answer_is_not_duplicate_shortcut",
    DuplCheckMacroName = "MR_tbl_memo_non_answer_is_not_duplicate",

    DebugArgStr = get_debug_arg_string(!.TableInfo),
    SuccName = "succeeded",
    LookupDeclCodeStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n" ++
        "\tMR_bool " ++ SuccName ++ ";\n",
    GetCodeStr =
        "\t" ++ GetMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            RecordName ++ ", " ++ cur_table_node_name ++ ");\n" ++
        LookupCodeStr,
    DuplCheckCodeStr =
        "\t" ++ DuplCheckMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            cur_table_node_name ++ ", " ++ SuccName ++ ");\n",
    AssignSuccessCodeStr =
        "\t" ++ success_indicator_name ++ " = " ++ SuccName ++ ";\n",
    CodeStr = LookupDeclCodeStr ++ SaveDeclCodeStr ++ "\n" ++
        GetCodeStr ++ LookupCodeStr ++
        DuplCheckCodeStr ++
        "\tif (" ++ SuccName ++ ") {\n" ++ CreateSaveCode ++ "\t}\n" ++
        AssignSuccessCodeStr,
    table_generate_foreign_proc(DuplCheckPredName, detism_semi,
        tabling_c_attributes_dupl, [RecordArg], LookupForeignArgs, CodeStr,
        purity_impure, instmap_delta_bind_no_var, ModuleInfo, Context,
        DuplicateCheckSaveGoal),
    Goals = LookupPrefixGoals ++ [DuplicateCheckSaveGoal].

    % Generate a goal for saving the output arguments in an answer block
    % in minimal model predicates.
    %
:- pred generate_mm_save_goals(list(var_mode_pos_method)::in,
    prog_var::in, pred_id::in, proc_id::in, int::in,
    table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_step_desc)::out,
    list(hlds_goal)::out) is det.

generate_mm_save_goals(NumberedSaveVars, SubgoalVar, PredId, ProcId, BlockSize,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, OutputSteps,
        Goals) :-
    ModuleInfo = !.TableInfo ^ table_module_info,
    DebugArgStr = get_debug_arg_string(!.TableInfo),

    generate_answer_table_lookup_goals(NumberedSaveVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, OutputSteps,
        LookupForeignArgs, LookupPrefixGoals, LookupCodeStr),

    GetMacroName = "MR_tbl_mm_get_answer_table",
    CreateMacroName = "MR_tbl_mm_create_answer_block",
    DuplCheckPredName = "table_mm_answer_is_not_duplicate_shortcut",
    DuplCheckMacroName = "MR_tbl_mm_answer_is_not_duplicate",

    generate_all_save_goals(NumberedSaveVars, subgoal_name, BlockSize,
        CreateMacroName, Context, !VarSet, !VarTypes, !TableInfo,
        _SaveArgs, _SavePrefixGoals, SaveDeclCode, CreateSaveCode),

    SubgoalName = subgoal_name,
    Args = [foreign_arg(SubgoalVar,
        yes(foreign_arg_name_mode(SubgoalName, in_mode)),
        subgoal_type, bp_native_if_possible)],
    SuccName = "succeeded",
    LookupDeclCodeStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n" ++
        "\tMR_bool " ++ SuccName ++ ";\n",
    GetCodeStr =
        "\t" ++ GetMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            SubgoalName ++ ", " ++ cur_table_node_name ++ ");\n",
    DuplCheckCodeStr =
        "\t" ++ DuplCheckMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            cur_table_node_name ++ ", " ++ SuccName ++ ");\n",
    CondSaveStr = "\tif (" ++ SuccName ++ ") {\n" ++
        CreateSaveCode ++ "\t}\n",
    AssignSuccessCodeStr =
        "\t" ++ success_indicator_name ++ " = " ++ SuccName ++ ";\n",
    CodeStr = LookupDeclCodeStr ++ SaveDeclCode ++
        GetCodeStr ++ LookupCodeStr ++ DuplCheckCodeStr ++
        CondSaveStr ++ AssignSuccessCodeStr,
    table_generate_foreign_proc(DuplCheckPredName, detism_semi,
        tabling_c_attributes_dupl, Args, LookupForeignArgs,
        CodeStr, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, DuplicateCheckSaveGoal),
    Goals = LookupPrefixGoals ++ [DuplicateCheckSaveGoal].

    % Generate a save goal for the given variables.
    %
:- pred generate_all_save_goals(list(var_mode_pos_method(T))::in,
    string::in, int::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(foreign_arg)::out,
    list(hlds_goal)::out, string::out, string::out) is det.

generate_all_save_goals(NumberedSaveVars, BaseVarName, BlockSize,
        CreateMacroName, Context, !VarSet, !VarTypes, !TableInfo,
        SaveArgs, SavePrefixGoals, SaveDeclCodeStr, CreateSaveCodeStr) :-
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    generate_save_goals(NumberedSaveVars, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, SaveArgs,
        SavePrefixGoals, SaveCodeStr),
    SaveDeclCodeStr = "\tMR_AnswerBlock " ++ answer_block_name ++ ";\n",
    CreateCodeStr = "\t" ++ CreateMacroName ++ "(" ++ DebugArgStr ++ ", " ++
        BaseVarName ++ ", " ++ int_to_string(BlockSize) ++ ", " ++
        answer_block_name ++ ");\n",
    CreateSaveCodeStr = CreateCodeStr ++ SaveCodeStr.

%-----------------------------------------------------------------------------%

    % Generate a sequence of save goals for the given variables.
    %
:- pred generate_own_stack_save_return_goal(list(var_mode_pos_method)::in,
    prog_var::in, pred_id::in, proc_id::in, int::in,
    table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_step_desc)::out,
    list(hlds_goal)::out) is det.

generate_own_stack_save_return_goal(NumberedOutputVars, GeneratorVar,
        PredId, ProcId, BlockSize, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, OutputSteps, Goals) :-
    GeneratorName = generator_name,
    GeneratorArg = foreign_arg(GeneratorVar,
        yes(foreign_arg_name_mode(GeneratorName, in_mode)),
        generator_type, bp_native_if_possible),
    DebugArgStr = get_debug_arg_string(!.TableInfo),

    generate_answer_table_lookup_goals(NumberedOutputVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, OutputSteps,
        LookupForeignArgs, LookupPrefixGoals, LookupCodeStr),

    generate_save_goals(NumberedOutputVars, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, _SaveArgs,
        SavePrefixGoals, SaveCodeStr),

    GetMacroName = "MR_table_mmos_get_answer_table",
    CreateMacroName = "MR_tbl_mmos_create_answer_block",
    DuplCheckPredName = "table_mmos_answer_is_not_duplicate_shortcut",
    DuplCheckMacroName = "MR_tbl_mmos_answer_is_not_duplicate",

    Args = [GeneratorArg],
    SuccName = "succeeded",
    LookupSaveDeclCodeStr =
        "\tMR_TrieNode " ++ cur_table_node_name ++ ";\n" ++
        "\tMR_TrieNode " ++ next_table_node_name ++ ";\n" ++
        "\tMR_AnswerBlock " ++ answer_block_name ++ ";\n" ++
        "\tMR_bool " ++ SuccName ++ ";\n\n",
    GetCodeStr = "\t" ++ cur_table_node_name ++ " = " ++
        GetMacroName ++ "(" ++ GeneratorName ++ ");\n",
    DuplCheckCodeStr =
        "\t" ++ DuplCheckMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            cur_table_node_name ++ ", " ++ SuccName ++ ");\n",
    AssignSuccessCodeStr =
        "\t" ++ success_indicator_name ++ " = " ++ SuccName ++ ";\n",
    CreateCodeStr = "\t" ++ CreateMacroName ++ "(" ++ DebugArgStr ++ ", " ++
        GeneratorName ++ ", " ++ int_to_string(BlockSize) ++ ", " ++
        answer_block_name ++ ");\n",
    SetupReturnCodeStr = "\t" ++ returning_generator_locn ++ " = " ++
        GeneratorName ++ ";\n",
    CreateSaveSetupReturnCodeStr = CreateCodeStr ++ SaveCodeStr ++
        SetupReturnCodeStr,
    CondSaveCodeStr = "\tif (" ++ SuccName ++ ") {\n" ++
        CreateSaveSetupReturnCodeStr ++ "\t}\n",
    CodeStr = LookupSaveDeclCodeStr ++ GetCodeStr ++ LookupCodeStr ++
        DuplCheckCodeStr ++ CondSaveCodeStr ++ AssignSuccessCodeStr,
    ModuleInfo = !.TableInfo ^ table_module_info,
    table_generate_foreign_proc(DuplCheckPredName, detism_semi,
        tabling_c_attributes_dupl, Args, LookupForeignArgs,
        CodeStr, purity_impure, instmap_delta_bind_no_var,
        ModuleInfo, Context, DuplicateCheckSaveGoal),
    Goals = LookupPrefixGoals ++ SavePrefixGoals ++
        [DuplicateCheckSaveGoal].

:- pred generate_save_goals(list(var_mode_pos_method(T))::in, string::in,
    term.context::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, table_info::in, table_info::out,
    list(foreign_arg)::out, list(hlds_goal)::out, string::out) is det.

generate_save_goals([], _, _, !VarSet, !VarTypes, !TableInfo, [], [], "").
generate_save_goals([NumberedVar | NumberedRest], DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Args ++ RestArgs,
        PrefixGoals ++ RestPrefixGoals, CodeStr ++ RestCodeStr) :-
    NumberedVar = var_mode_pos_method(Var, _Mode, Offset, _),
    ModuleInfo = !.TableInfo ^ table_module_info,
    lookup_var_type(!.VarTypes, Var, VarType),
    CtorCat = classify_type(ModuleInfo, VarType),
    gen_save_call_for_type(CtorCat, VarType, Var, Offset, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Args, PrefixGoals, CodeStr),
    generate_save_goals(NumberedRest, DebugArgStr, Context, !VarSet, !VarTypes,
        !TableInfo, RestArgs, RestPrefixGoals, RestCodeStr).

:- pred gen_save_call_for_type(type_ctor_category::in, mer_type::in,
    prog_var::in, int::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(foreign_arg)::out,
    list(hlds_goal)::out, string::out) is det.

gen_save_call_for_type(CtorCat, Type, Var, Offset, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Args, PrefixGoals, CodeStr) :-
    Name = arg_name(Offset),
    ForeignArg = foreign_arg(Var, yes(foreign_arg_name_mode(Name, in_mode)),
        Type, bp_native_if_possible),
    ( if type_is_io_state(Type) then
        SaveMacroName = "MR_tbl_save_io_state_answer",
        Args = [ForeignArg],
        PrefixGoals = [],
        CodeStr = "\t" ++ SaveMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            answer_block_name ++ ", " ++ int_to_string(Offset) ++ ", "
            ++ Name ++ ");\n"
    else if builtin_type(CtorCat) = no then
        % If we used ForeignArg instead of GenericForeignArg, then
        % Var would be unboxed when assigned to Name, which we don't want.
        GenericForeignArg = foreign_arg(Var,
            yes(foreign_arg_name_mode(Name, in_mode)),
            dummy_type_var, bp_native_if_possible),
        table_gen_make_type_info_var(Type, Context, !VarSet, !VarTypes,
            !TableInfo, TypeInfoVar, PrefixGoals),
        TypeInfoName = "save_arg_typeinfo" ++ int_to_string(Offset),
        lookup_var_type(!.VarTypes, TypeInfoVar, TypeInfoType),
        TypeInfoForeignArg = foreign_arg(TypeInfoVar,
            yes(foreign_arg_name_mode(TypeInfoName, in_mode)),
            TypeInfoType, bp_native_if_possible),
        SaveMacroName = "MR_tbl_save_any_answer",
        Args = [GenericForeignArg, TypeInfoForeignArg],
        CodeStr = "\t" ++ SaveMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            answer_block_name ++ ", " ++ int_to_string(Offset) ++ ", " ++
            TypeInfoName ++ ", " ++ Name ++ ");\n"
    else
        type_save_category(CtorCat, CatString),
        SaveMacroName = "MR_tbl_save_" ++ CatString ++ "_answer",
        Args = [ForeignArg],
        PrefixGoals = [],
        CodeStr = "\t" ++ SaveMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            answer_block_name ++ ", " ++ int_to_string(Offset) ++ ", "
            ++ Name ++ ");\n"
    ).

%-----------------------------------------------------------------------------%

    % Generate a goal for restoring the output arguments from
    % an answer block in memo predicates.
    %
:- pred generate_memo_restore_goal(list(var_mode_pos_method(T))::in,
    instmap_delta::in, prog_var::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, hlds_goal::out) is det.

generate_memo_restore_goal(NumberedOutputVars, OrigInstMapDelta, TipVar,
        Context, !VarSet, !VarTypes, TableInfo, Goal) :-
    (
        NumberedOutputVars = [_ | _],
        DebugArgStr = get_debug_arg_string(TableInfo),
        ModuleInfo = TableInfo ^ table_module_info,
        generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
            DebugArgStr, ModuleInfo, !VarSet, !VarTypes,
            RestoreInstMapDeltaSrc, RestoreArgs, RestoreCodeStr),
        BaseVarName = base_name,
        Arg = foreign_arg(TipVar,
            yes(foreign_arg_name_mode(BaseVarName, in_mode)),
            trie_node_type, bp_native_if_possible),
        Args = [Arg],
        GetPredName = "table_memo_get_answer_block_shortcut",
        GetMacroName = "MR_tbl_memo_get_answer_block",
        DeclCodeStr = "\tMR_AnswerBlock " ++ answer_block_name ++ ";\n",
        GetRestoreCodeStr = "\t" ++ GetMacroName ++ "(" ++
            DebugArgStr ++ ", " ++ BaseVarName ++ ", " ++
            answer_block_name ++ ");\n" ++
            RestoreCodeStr,
        table_generate_foreign_proc(GetPredName, detism_det,
            tabling_c_attributes_dupl, Args, RestoreArgs,
            DeclCodeStr ++ GetRestoreCodeStr, purity_semipure,
            instmap_delta_from_assoc_list(RestoreInstMapDeltaSrc),
            ModuleInfo, Context, ShortcutGoal),
        Goal = ShortcutGoal
    ;
        NumberedOutputVars = [],
        Goal = true_goal
    ).

    % Generate a goal for restoring the output arguments from
    % an answer block in model_non memo predicates.
    %
:- pred generate_memo_non_restore_goal(determinism::in,
    list(var_mode_pos_method)::in, instmap_delta::in, prog_var::in,
    term.context::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, table_info::in, hlds_goal::out) is det.

generate_memo_non_restore_goal(Detism, NumberedOutputVars, OrigInstMapDelta,
        RecordVar, Context, !VarSet, !VarTypes, TableInfo, Goal) :-
    ( if Detism = detism_multi then
        ReturnAllAns = "table_memo_return_all_answers_multi"
    else if Detism = detism_non then
        ReturnAllAns = "table_memo_return_all_answers_nondet"
    else
        unexpected($module, $pred, "invalid determinism")
    ),
    generate_new_table_var("AnswerBlock", answer_block_type,
        !VarSet, !VarTypes, AnswerBlockVar),
    ModuleInfo = TableInfo ^ table_module_info,
    table_generate_call(ReturnAllAns, Detism, [RecordVar, AnswerBlockVar],
        purity_semipure, instmap_delta_bind_var(AnswerBlockVar), ModuleInfo,
        Context, ReturnAnswerBlocksGoal),
    DebugArgStr = get_debug_arg_string(TableInfo),
    generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, RestoreInstMapDeltaSrc,
        RestoreArgs, RestoreCodeStr),
    OutputVars = list.map(project_var, NumberedOutputVars),
    Arg = foreign_arg(AnswerBlockVar,
        yes(foreign_arg_name_mode(answer_block_name, in_mode)),
        answer_block_type, bp_native_if_possible),
    Args = [Arg],
    PredName = "table_memo_non_return_all_shortcut",
    table_generate_foreign_proc(PredName, detism_det,
        tabling_c_attributes_no_dupl, Args, RestoreArgs, RestoreCodeStr,
        purity_semipure, instmap_delta_from_assoc_list(RestoreInstMapDeltaSrc),
        ModuleInfo, Context, ShortcutGoal),

    GoalExpr = conj(plain_conj, [ReturnAnswerBlocksGoal, ShortcutGoal]),
    set_of_var.list_to_set([RecordVar | OutputVars], NonLocals),
    goal_info_init_hide(NonLocals, OrigInstMapDelta, Detism, purity_semipure,
        Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Generate a goal for restoring the output arguments from
    % an answer block in minimal model predicates without a suspension.
    %
:- pred generate_mm_restore_goal(determinism::in,
    list(var_mode_pos_method)::in, instmap_delta::in, prog_var::in,
    term.context::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, table_info::in, hlds_goal::out) is det.

generate_mm_restore_goal(Detism, NumberedOutputVars, OrigInstMapDelta,
        SubgoalVar, Context, !VarSet, !VarTypes, TableInfo, Goal) :-
    ( if Detism = detism_multi then
        ReturnAllAns = "table_mm_return_all_multi"
    else if Detism = detism_non then
        ReturnAllAns = "table_mm_return_all_nondet"
    else
        unexpected($module, $pred, "invalid determinism")
    ),
    generate_mm_restore_or_suspend_goal(ReturnAllAns, Detism, purity_semipure,
        NumberedOutputVars, OrigInstMapDelta, SubgoalVar, Context,
        !VarSet, !VarTypes, TableInfo, Goal).

    % Generate a goal for restoring the output arguments from
    % an answer block in minimal model predicates after a suspension.
    %
:- pred generate_mm_suspend_goal(list(var_mode_pos_method)::in,
    instmap_delta::in, prog_var::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, hlds_goal::out) is det.

generate_mm_suspend_goal(NumberedOutputVars, OrigInstMapDelta, SubgoalVar,
        Context, !VarSet, !VarTypes, TableInfo, Goal) :-
    generate_mm_restore_or_suspend_goal("table_mm_suspend_consumer",
        detism_non, purity_impure, NumberedOutputVars, OrigInstMapDelta,
        SubgoalVar, Context, !VarSet, !VarTypes, TableInfo, Goal).

    % Generate a goal for restoring the output arguments from
    % an answer block in minimal model predicates. Whether the restore
    % is after a suspension depends on the arguments.
    %
:- pred generate_mm_restore_or_suspend_goal(string::in, determinism::in,
    purity::in, list(var_mode_pos_method)::in, instmap_delta::in,
    prog_var::in, term.context::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, table_info::in, hlds_goal::out) is det.

generate_mm_restore_or_suspend_goal(PredName, Detism, Purity,
        NumberedOutputVars, OrigInstMapDelta, SubgoalVar, Context,
        !VarSet, !VarTypes, TableInfo, Goal) :-
    generate_new_table_var("AnswerBlock", answer_block_type,
        !VarSet, !VarTypes, AnswerBlockVar),
    ModuleInfo = TableInfo ^ table_module_info,
    table_generate_call(PredName, Detism, [SubgoalVar, AnswerBlockVar],
        Purity, instmap_delta_bind_var(AnswerBlockVar), ModuleInfo,
        Context, ReturnAnswerBlocksGoal),
    DebugArgStr = get_debug_arg_string(TableInfo),
    generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, RestoreInstMapDeltaSrc,
        RestoreArgs, RestoreCodeStr),
    OutputVars = list.map(project_var, NumberedOutputVars),

    Arg = foreign_arg(AnswerBlockVar,
        yes(foreign_arg_name_mode(answer_block_name, in_mode)),
        answer_block_type, bp_native_if_possible),
    Args = [Arg],
    ReturnAllPredName = "table_mm_return_all_shortcut",
    table_generate_foreign_proc(ReturnAllPredName, detism_det,
        tabling_c_attributes_no_dupl, Args, RestoreArgs, RestoreCodeStr,
        purity_semipure, instmap_delta_from_assoc_list(RestoreInstMapDeltaSrc),
        ModuleInfo, Context, ReturnAllGoal),
    GoalExpr = conj(plain_conj, [ReturnAnswerBlocksGoal, ReturnAllGoal]),

    set_of_var.list_to_set([SubgoalVar | OutputVars], NonLocals),
    goal_info_init_hide(NonLocals, OrigInstMapDelta, Detism, Purity,
        Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

    % Generate a sequence of restore goals for the given variables.
    %
:- pred generate_restore_goals(list(var_mode_pos_method(T))::in,
    instmap_delta::in, string::in, module_info::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    assoc_list(prog_var, mer_inst)::out, list(foreign_arg)::out,
    string::out) is det.

generate_restore_goals([], _, _, _, !VarSet, !VarTypes, [], [], "").
generate_restore_goals([NumberedVar | NumberedRest], OrigInstmapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, [VarInst | VarInsts],
        [Arg | Args], CodeStr ++ RestCodeStr) :-
    NumberedVar = var_mode_pos_method(Var, _Mode, Offset, _),
    lookup_var_type(!.VarTypes, Var, VarType),
    CtorCat = classify_type(ModuleInfo, VarType),
    gen_restore_call_for_type(DebugArgStr, CtorCat, VarType, OrigInstmapDelta,
        Var, Offset, VarInst, Arg, CodeStr),
    generate_restore_goals(NumberedRest, OrigInstmapDelta, DebugArgStr,
        ModuleInfo, !VarSet, !VarTypes, VarInsts, Args, RestCodeStr).

:- pred gen_restore_call_for_type(string::in, type_ctor_category::in,
    mer_type::in, instmap_delta::in, prog_var::in, int::in,
    pair(prog_var, mer_inst)::out, foreign_arg::out, string::out) is det.

gen_restore_call_for_type(DebugArgStr, CtorCat, Type, OrigInstmapDelta, Var,
        Offset, Var - Inst, Arg, CodeStr) :-
    Name = "restore_arg" ++ int_to_string(Offset),
    ( if type_is_io_state(Type) then
        RestoreMacroName = "MR_tbl_restore_io_state_answer",
        ArgType = Type
    else if builtin_type(CtorCat) = no then
        RestoreMacroName = "MR_tbl_restore_any_answer",
        ArgType = dummy_type_var
    else
        type_save_category(CtorCat, CatString),
        RestoreMacroName = "MR_tbl_restore_" ++ CatString ++ "_answer",
        ArgType = Type
    ),
    ( if instmap_delta_search_var(OrigInstmapDelta, Var, InstPrime) then
        Inst = InstPrime
    else
        unexpected($module, $pred, "no inst")
    ),
    Arg = foreign_arg(Var,
        yes(foreign_arg_name_mode(Name, from_to_mode(free, Inst))),
        ArgType, bp_native_if_possible),
    CodeStr = "\t" ++ RestoreMacroName ++ "(" ++ DebugArgStr ++ ", " ++
        answer_block_name ++ ", " ++ int_to_string(Offset) ++ ", " ++
        Name ++ ");\n".

%-----------------------------------------------------------------------------%

:- func infinite_recursion_msg = string.

infinite_recursion_msg = "detected infinite recursion".

:- func need_minimal_model_msg = string.

need_minimal_model_msg = "detected need for minimal model".

:- pred generate_error_goal(table_info::in, term.context::in, string::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    hlds_goal::out) is det.

generate_error_goal(TableInfo, Context, Msg, !VarSet, !VarTypes, Goal) :-
    ModuleInfo = TableInfo ^ table_module_info,
    PredInfo = TableInfo ^ table_cur_pred_info,

    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredOrFuncStr = pred_or_func_to_str(PredOrFunc),
    NameStr = sym_name_to_string(qualified(Module, Name)),
    string.int_to_string(Arity, ArityStr),
    Message = Msg ++ " in " ++ PredOrFuncStr ++ " " ++ NameStr
        ++ "/" ++ ArityStr,

    make_string_const_construction_alloc(Message, yes("Message"),
        MessageStrGoal, MessageVar, !VarSet, !VarTypes),

    table_generate_call("table_error", detism_erroneous, [MessageVar],
        purity_pure, instmap_delta_bind_no_var, ModuleInfo, Context, CallGoal),

    GoalExpr = conj(plain_conj, [MessageStrGoal, CallGoal]),
    goal_info_init_hide(set_of_var.init, instmap_delta_bind_no_var,
        detism_erroneous, purity_impure, Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred generate_new_table_var(string::in, mer_type::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var::out) is det.

generate_new_table_var(Name, Type, !VarSet, !VarTypes, Var) :-
    varset.new_named_var(Name, Var, !VarSet),
    add_var_type(Var, Type, !VarTypes).

:- pred table_generate_call(string::in, determinism::in, list(prog_var)::in,
    purity::in, instmap_delta::in, module_info::in, term.context::in,
    hlds_goal::out) is det.

table_generate_call(PredName, Detism, Args, Purity, InstMapDelta, ModuleInfo,
        Context, Goal) :-
    BuiltinModule = mercury_table_builtin_module,
    (
        Purity = purity_pure,
        Features0 = []
    ;
        ( Purity = purity_semipure
        ; Purity = purity_impure
        ),
        Features0 = [feature_not_impure_for_determinism]
    ),
    ( if Detism = detism_failure then
        Features = [feature_preserve_backtrack_into | Features0]
    else
        Features = Features0
    ),
    goal_util.generate_simple_call(BuiltinModule, PredName, pf_predicate,
        only_mode, Detism, Purity, Args, Features, InstMapDelta, ModuleInfo,
        Context, Goal).

:- pred table_generate_foreign_proc(string::in, determinism::in,
    pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, list(foreign_arg)::in, string::in,
    purity::in, instmap_delta::in,
    module_info::in, term.context::in, hlds_goal::out) is det.

table_generate_foreign_proc(PredName, Detism, Attributes, Args, ExtraArgs,
        Code, Purity, InstMapDelta, ModuleInfo, Context, Goal) :-
    (
        Purity = purity_pure,
        Features0 = []
    ;
        ( Purity = purity_semipure
        ; Purity = purity_impure
        ),
        Features0 = [feature_not_impure_for_determinism]
    ),
    ( if Detism = detism_failure then
        Features = [feature_preserve_backtrack_into | Features0]
    else
        Features = Features0
    ),
    BuiltinModule = mercury_table_builtin_module,
    MaybeTraceRuntimCond = no,
    goal_util.generate_foreign_proc(BuiltinModule, PredName, pf_predicate,
        only_mode, Detism, Purity, Attributes, Args, ExtraArgs,
        MaybeTraceRuntimCond, Code, Features, InstMapDelta, ModuleInfo,
        Context, Goal).

:- pred append_fail(hlds_goal::in, hlds_goal::out) is det.

append_fail(Goal, GoalAndThenFail) :-
    Goal = hlds_goal(_, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    instmap_delta_init_unreachable(UnreachInstMapDelta),
    goal_info_init_hide(NonLocals, UnreachInstMapDelta, detism_failure,
        purity_impure, Context, ConjGoalInfo),
    GoalAndThenFail =
        hlds_goal(conj(plain_conj, [Goal, fail_goal]), ConjGoalInfo).

%-----------------------------------------------------------------------------%

:- func consumer_type = mer_type.

consumer_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_consumer"), 0), [], Type).

:- func generator_type = mer_type.

generator_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_generator"), 0), [], Type).

:- type maybe_specified_method
    --->    msm_all_same(arg_tabling_method)
    ;       msm_specified(
                list(maybe(arg_tabling_method)),
                hidden_arg_tabling_method
            ).

:- pred get_input_output_vars(list(prog_var)::in, list(mer_mode)::in,
    module_info::in, maybe_specified_method::in, maybe_specified_method::out,
    list(var_mode_method)::out, list(var_mode_method)::out) is det.

get_input_output_vars([], [], _, !MaybeSpecMethod, [], []).
get_input_output_vars([_ | _], [], _, !MaybeSpecMethod, _, _) :-
    unexpected($module, $pred, "lists not same length").
get_input_output_vars([], [_ | _], _, !MaybeSpecMethod, _, _) :-
    unexpected($module, $pred, "lists not same length").
get_input_output_vars([Var | Vars], [Mode | Modes], ModuleInfo,
        !MaybeSpecMethod, InVarModes, OutVarModes) :-
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        get_input_output_vars(Vars, Modes, ModuleInfo, !MaybeSpecMethod,
            InVarModes0, OutVarModes),
        (
            !.MaybeSpecMethod = msm_all_same(ArgMethod)
        ;
            !.MaybeSpecMethod =
                msm_specified(MaybeArgMethods0, HiddenArgMethod),
            ( if
                list.split_last(MaybeArgMethods0, MaybeArgMethods,
                    LastMaybeArgMethod)
            then
                (
                    LastMaybeArgMethod = yes(ArgMethod)
                ;
                    LastMaybeArgMethod = no,
                    unexpected($module, $pred, "bad method for input var")
                ),
                !:MaybeSpecMethod = msm_specified(MaybeArgMethods,
                    HiddenArgMethod)
            else
                % We have run out of specified arg_methods, which means the
                % variable we are looking at right now is one that was added
                % by the polymorphism transformation.
                (
                    HiddenArgMethod = table_hidden_arg_value,
                    ArgMethod = arg_value
                ;
                    HiddenArgMethod = table_hidden_arg_addr,
                    ArgMethod = arg_addr
                ),
                !:MaybeSpecMethod = msm_all_same(ArgMethod)
            )
        ),
        InVarModes = [var_mode_method(Var, Mode, ArgMethod) | InVarModes0]
    else if mode_is_fully_output(ModuleInfo, Mode) then
        get_input_output_vars(Vars, Modes, ModuleInfo, !MaybeSpecMethod,
            InVarModes, OutVarModes0),
        (
            !.MaybeSpecMethod = msm_all_same(_ArgMethod)
            % The tabling methods that use answer tables always use arg_value
            % to look up computed output arguments in them. The argument of
            % all_same refers only to the treatment of input arguments.
        ;
            !.MaybeSpecMethod =
                msm_specified(MaybeArgMethods0, HiddenArgMethod),
            ( if
                list.split_last(MaybeArgMethods0, MaybeArgMethods,
                    LastMaybeArgMethod)
            then
                expect(unify(LastMaybeArgMethod, no), $pred,
                    "bad method for output var"),
                !:MaybeSpecMethod = msm_specified(MaybeArgMethods,
                    HiddenArgMethod)
            else
                % We have run out of specified arg_methods, which means the
                % variable we are looking at right now is one that was added
                % by the polymorphism transformation.
                (
                    HiddenArgMethod = table_hidden_arg_value,
                    ArgMethod = arg_value
                ;
                    HiddenArgMethod = table_hidden_arg_addr,
                    ArgMethod = arg_addr
                ),
                !:MaybeSpecMethod = msm_all_same(ArgMethod)
            )
        ),
        OutVarModes = [var_mode_method(Var, Mode, arg_value) | OutVarModes0]
    else
        % We should have caught this when we added the tabling pragma
        % to the proc_info.
        unexpected($module, $pred, "bad var")
    ).

%-----------------------------------------------------------------------------%

:- pred create_instmap_delta(hlds_goals::in, instmap_delta::out) is det.

create_instmap_delta([], IMD) :-
    IMD = instmap_delta_bind_no_var.
create_instmap_delta([Goal | Rest], IMD) :-
    Goal = hlds_goal(_, GoalInfo),
    IMD0 = goal_info_get_instmap_delta(GoalInfo),
    create_instmap_delta(Rest, IMD1),
    instmap_delta_apply_instmap_delta(IMD0, IMD1, test_size, IMD).

%-----------------------------------------------------------------------------%

:- pred add_proc_table_struct(pred_proc_id::in, proc_table_struct_info::in,
    proc_info::in, module_info::in, module_info::out) is det.

add_proc_table_struct(PredProcId, ProcTableStructInfo, ProcInfo,
        !ModuleInfo) :-
    module_info_get_table_struct_map(!.ModuleInfo, TableStructMap0),
    proc_info_get_table_attributes(ProcInfo, MaybeTableAttributes),
    (
        MaybeTableAttributes = yes(TableAttributes)
    ;
        MaybeTableAttributes = no,
        TableAttributes = default_memo_table_attributes
    ),
    TableStructInfo = table_struct_info(ProcTableStructInfo, TableAttributes),
    map.det_insert(PredProcId, TableStructInfo,
        TableStructMap0, TableStructMap),
    module_info_set_table_struct_map(TableStructMap, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- type var_mode_method
    --->    var_mode_method(
                % The head variable.
                prog_var,

                % The mode of the head variable.
                mer_mode,

                % For input arguments, this is the arg method to use in
                % looking up the argument in the call table. For output
                % arguments, this is the arg method to use in looking up
                % the argument in the answer table (if any).
                arg_tabling_method
            ).

:- type var_mode_pos_method == var_mode_pos_method(arg_tabling_method).

:- type var_mode_pos_method(T)
    --->    var_mode_pos_method(
                % The head variable.
                prog_var,

                % The mode of the head variable.
                mer_mode,

                % The offset of the head variable in the answer block;
                % the first slot is at offset 0.
                int,

                % For input arguments, this is the arg method to use in
                % looking up the argument in the call table. For output
                % arguments, this is the arg method to use in looking up
                % the argument in the answer table (if any), which for now
                % is always arg_value.
                %
                % When T is unit, there is no info about how to look up
                % the variable in a call or return table. This is useful
                % for recording the structure of answer blocks and sequences
                % of arguments for I/O tabling.
                T
            ).

:- func project_var(var_mode_pos_method(T)) = prog_var.

project_var(var_mode_pos_method(Var, _, _, _)) = Var.

:- func project_var_pos(var_mode_pos_method(T)) = pair(prog_var, int).

project_var_pos(var_mode_pos_method(Var, _, Pos, _)) = Var - Pos.

:- func project_var_init_inst(module_info, var_mode_pos_method(T)) =
    pair(prog_var, mer_inst).

project_var_init_inst(ModuleInfo, var_mode_pos_method(Var, Mode, _, _))
        = Var - Inst :-
    mode_get_insts(ModuleInfo, Mode, Inst, _).

:- func project_mode(var_mode_pos_method(T)) = mer_mode.

project_mode(var_mode_pos_method(_, Mode, _, _)) = Mode.

:- func project_out_arg_method(var_mode_pos_method) =
    var_mode_pos_method(unit).

project_out_arg_method(var_mode_pos_method(Var, Mode, Pos, _)) =
    var_mode_pos_method(Var, Mode, Pos, unit).

:- func project_out_pos(var_mode_pos_method) = var_mode_method.

project_out_pos(var_mode_pos_method(Var, Mode, _Pos, ArgMethod)) =
    var_mode_method(Var, Mode, ArgMethod).

:- pred allocate_slot_numbers(list(var_mode_method)::in,
    int::in, list(var_mode_pos_method)::out) is det.

allocate_slot_numbers([], _, []).
allocate_slot_numbers([var_mode_method(Var, Mode, ArgMethod) | VarModes],
        Offset0, [VarModePos | VarModePoss]) :-
    VarModePos = var_mode_pos_method(Var, Mode, Offset0, ArgMethod),
    allocate_slot_numbers(VarModes, Offset0 + 1, VarModePoss).

:- pred allocate_plain_slot_numbers(assoc_list(prog_var, mer_mode)::in,
    int::in, list(var_mode_pos_method(unit))::out) is det.

allocate_plain_slot_numbers([], _, []).
allocate_plain_slot_numbers([Var - Mode | VarModes],
        Offset0, [VarModePos | VarModePoss]) :-
    VarModePos = var_mode_pos_method(Var, Mode, Offset0, unit),
    allocate_plain_slot_numbers(VarModes, Offset0 + 1, VarModePoss).

:- pred reallocate_slot_numbers(list(var_mode_pos_method(T))::in, int::in,
    list(var_mode_pos_method(T))::out) is det.

reallocate_slot_numbers([], _, []).
reallocate_slot_numbers([VarModePos0 | VarModes], Offset0,
        [VarModePos | VarModePoss]) :-
    VarModePos0 = var_mode_pos_method(Var, Mode, _, ArgMethod),
    VarModePos = var_mode_pos_method(Var, Mode, Offset0, ArgMethod),
    reallocate_slot_numbers(VarModes, Offset0 + 1, VarModePoss).

:- pred var_belong_to_list(list(prog_var)::in, var_mode_pos_method(T)::in)
    is semidet.

var_belong_to_list(List, var_mode_pos_method(Var, _, _, _)) :-
    list.member(Var, List).

:- pred goal_info_init_hide(set_of_progvar::in, instmap_delta::in,
    determinism::in, purity::in, prog_context::in, hlds_goal_info::out)
    is det.

goal_info_init_hide(NonLocals, InstmapDelta, Detism, Purity, Context,
        GoalInfo) :-
    goal_info_init(NonLocals, InstmapDelta, Detism, Purity, Context,
        GoalInfo0),
    goal_info_add_feature(feature_hide_debug_event, GoalInfo0, GoalInfo).

%-----------------------------------------------------------------------------%

    % For backward compatibility, we treat type_info_type as user_type.
    % This used to make the tabling of type_infos more expensive than
    % necessary, since we essentially tabled the information in the type_info
    % twice, once by tabling the type represented by the type_info (since this
    % was the value of the type argument of the type constructor
    % private_builtin.type_info/1), and then tabling the type_info itself.
    % However, since we made type_info have arity zero, this overhead
    % should be gone.
    %
:- func builtin_type(type_ctor_category) = bool.

builtin_type(CtorCat) = Builtin :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(cat_system_type_ctor_info)
        ; CtorCat = ctor_cat_system(cat_system_typeclass_info)
        ; CtorCat = ctor_cat_system(cat_system_base_typeclass_info)
        ),
        Builtin = yes
    ;
        ( CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_system(cat_system_type_info)
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_higher_order
        ),
        Builtin = no
    ).

    % Figure out which save and restore predicates in library/table_builtin.m
    % we need to use for values of types belonging the type category given by
    % the first argument. The returned value replaces CAT in
    % table_save_CAT_answer and table_restore_CAT_answer.
    %
:- pred type_save_category(type_ctor_category::in, string::out) is det.

type_save_category(CtorCat, Name) :-
    (
        CtorCat = ctor_cat_enum(cat_enum_mercury),
        Name = "enum"
    ;
        CtorCat = ctor_cat_enum(cat_enum_foreign),
        sorry($module, $pred, "tabling and foreign enumerations NYI.")
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int)),
        Name = "int"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint)),
        Name = "uint"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int8)),
        Name = "int8"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint8)),
        Name = "uint8"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int16)),
        Name = "int16"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint16)),
        Name = "uint16"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int32)),
        Name = "int32"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint32)),
        Name = "uint32"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_int64)),
        Name = "int64"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_int(int_type_uint64)),
        Name = "uint64"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_float),
        Name = "float"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_char),
        Name = "char"
    ;
        CtorCat = ctor_cat_builtin(cat_builtin_string),
        Name = "string"
    ;
        CtorCat = ctor_cat_higher_order,
        Name = "pred"
    ;
        % Could do better.
        ( CtorCat = ctor_cat_user(_)
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_system(cat_system_type_info)
        ),
        Name = "any"
    ;
        CtorCat = ctor_cat_tuple,
        Name = "any"
    ;
        ( CtorCat = ctor_cat_system(cat_system_type_ctor_info)
        ; CtorCat = ctor_cat_system(cat_system_typeclass_info)
        ; CtorCat = ctor_cat_system(cat_system_base_typeclass_info)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ),
        unexpected($module, $pred, "unexpected category")
    ).

%-----------------------------------------------------------------------------%

:- func get_debug_arg_string(table_info) = string.

get_debug_arg_string(TableInfo) = DebugArgStr :-
    ModuleInfo = TableInfo ^ table_module_info,
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, table_debug, TableDebug),
    (
        TableDebug = yes,
        DebugArgStr = "MR_TRUE"
    ;
        TableDebug = no,
        DebugArgStr = "MR_FALSE"
    ).

:- func get_back_arg_string(table_info) = string.

get_back_arg_string(TableInfo) = BackArgStr :-
    ProcInfo = TableInfo ^ table_cur_proc_info,
    proc_info_get_table_attributes(ProcInfo, MaybeAttributes),
    (
        MaybeAttributes = yes(Attributes),
        MaybeSizeLimit = Attributes ^ table_attr_size_limit,
        (
            MaybeSizeLimit = yes(_),
            BackArgStr = "MR_TRUE"
        ;
            MaybeSizeLimit = no,
            BackArgStr = "MR_FALSE"
        )
    ;
        MaybeAttributes = no,
        BackArgStr = "MR_FALSE"
    ).

:- type call_or_answer_table
    --->    call_table
    ;       answer_table.

:- func stats_ref(table_attr_statistics, call_or_answer_table) = maybe(string).

stats_ref(Statistics, Kind) = MaybeStatsRef :-
    (
        Statistics = table_dont_gather_statistics,
        MaybeStatsRef = no
    ;
        Statistics = table_gather_statistics,
        (
            Kind = call_table,
            KindStr = "MR_TABLE_CALL_TABLE"
        ;
            Kind = answer_table,
            KindStr = "MR_TABLE_ANSWER_TABLE"
        ),
        StatsRef = proc_table_info_name ++ "->MR_pt_stats" ++
            "[" ++ KindStr ++ "][MR_TABLE_STATS_CURR]",
        MaybeStatsRef = yes(StatsRef)
    ).

:- func maybe_step_stats_arg_addr(maybe(string), int) = string.

maybe_step_stats_arg_addr(MaybeStatsRef, SeqNum) = ArgStr :-
    (
        MaybeStatsRef = no,
        ArgStr = "NULL"
    ;
        MaybeStatsRef = yes(StatsRef),
        ArgStr = "&(" ++ step_stats_arg_addr(StatsRef, SeqNum) ++ ")"
    ).

:- func step_stats_arg_addr(string, int) = string.

step_stats_arg_addr(StatsRef, SeqNum) = ArgStr :-
    ArgStr = StatsRef ++ ".MR_ts_steps" ++ "[" ++ int_to_string(SeqNum) ++ "]".

%-----------------------------------------------------------------------------%

:- pred table_gen_make_type_info_var(mer_type::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out,
    list(hlds_goal)::out) is det.

table_gen_make_type_info_var(Type, Context, !VarSet, !VarTypes, !TableInfo,
        TypeInfoVar, TypeInfoGoals) :-
    table_gen_make_type_info_vars([Type], Context, !VarSet, !VarTypes,
        !TableInfo, TypeInfoVars, TypeInfoGoals),
    ( if TypeInfoVars = [TypeInfoVar0] then
        TypeInfoVar = TypeInfoVar0
    else
        unexpected($module, $pred, "list length != 1")
    ).

:- pred table_gen_make_type_info_vars(list(mer_type)::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(prog_var)::out,
    list(hlds_goal)::out) is det.

table_gen_make_type_info_vars(Types, Context, !VarSet, !VarTypes,
        !TableInfo, TypeInfoVars, TypeInfoGoals) :-
    % Extract the information from table_info.
    table_info_extract(!.TableInfo, ModuleInfo0, PredInfo0, ProcInfo0),

    % Put the varset and vartypes from the simplify_info
    % back in the proc_info.
    proc_info_set_vartypes(!.VarTypes, ProcInfo0, ProcInfo1),
    proc_info_set_varset(!.VarSet, ProcInfo1, ProcInfo2),

    % Call polymorphism.m to create the type_infos.
    create_poly_info(ModuleInfo0, PredInfo0, ProcInfo2, PolyInfo0),
    polymorphism_make_type_info_vars(Types, Context,
        TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo),
    poly_info_extract(PolyInfo, PolySpecs, PredInfo0, PredInfo,
        ProcInfo0, ProcInfo, ModuleInfo),
    expect(unify(PolySpecs, []), $module, $pred,
        "got errors while making type_info_vars"),

    % Get the new varset and vartypes from the proc_info.
    proc_info_get_vartypes(ProcInfo, !:VarTypes),
    proc_info_get_varset(ProcInfo, !:VarSet),

    % Put the new module_info, pred_info, and proc_info back in the table_info.
    table_info_init(ModuleInfo, PredInfo, ProcInfo, !:TableInfo).

%-----------------------------------------------------------------------------%

:- pred var_mode_pos_is_io_state(vartypes::in, var_mode_pos_method::in)
    is semidet.

var_mode_pos_is_io_state(VarTypes, VarModePosMethod) :-
    var_is_io_state(VarTypes, project_var(VarModePosMethod)).

:- pred var_mode_is_io_state(vartypes::in, pair(prog_var, mer_mode)::in)
    is semidet.

var_mode_is_io_state(VarTypes, Var - _) :-
    var_is_io_state(VarTypes, Var).

:- pred var_is_io_state(vartypes::in, prog_var::in) is semidet.

var_is_io_state(VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, VarType),
    type_is_io_state(VarType).

%-----------------------------------------------------------------------------%

:- func tabling_c_attributes_dupl = pragma_foreign_proc_attributes.

tabling_c_attributes_dupl = Attrs :-
    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs0, Attrs1),
    set_affects_liveness(proc_does_not_affect_liveness, Attrs1, Attrs2),
    set_may_duplicate(yes(proc_may_duplicate), Attrs2, Attrs).

:- func tabling_c_attributes_no_dupl = pragma_foreign_proc_attributes.

tabling_c_attributes_no_dupl = Attrs :-
    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs0, Attrs1),
    set_affects_liveness(proc_does_not_affect_liveness, Attrs1, Attrs).

:- func make_generator_c_attributes = pragma_foreign_proc_attributes.

make_generator_c_attributes = Attrs :-
    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_may_call_mercury, Attrs0, Attrs).

:- func dummy_type_var = mer_type.

dummy_type_var = Type :-
    varset.init(DummyTVarSet0),
    varset.new_var(DummyTVar, DummyTVarSet0, _),
    Type = type_variable(DummyTVar, kind_star).

%-----------------------------------------------------------------------------%

:- func loop_inactive_cons_id = cons_id.
:- func loop_active_cons_id = cons_id.

loop_inactive_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "loop_inactive"),
    TypeCtor = loop_status_type_ctor.
loop_active_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "loop_active"),
    TypeCtor = loop_status_type_ctor.

:- func memo_det_inactive_cons_id = cons_id.
:- func memo_det_active_cons_id = cons_id.
:- func memo_det_succeeded_cons_id = cons_id.

memo_det_inactive_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_det_inactive"),
    TypeCtor = memo_det_status_type_ctor.
memo_det_active_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_det_active"),
    TypeCtor = memo_det_status_type_ctor.
memo_det_succeeded_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_det_succeeded"),
    TypeCtor = memo_det_status_type_ctor.

:- func memo_semi_inactive_cons_id = cons_id.
:- func memo_semi_active_cons_id = cons_id.
:- func memo_semi_succeeded_cons_id = cons_id.
:- func memo_semi_failed_cons_id = cons_id.

memo_semi_inactive_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_semi_inactive"),
    TypeCtor = memo_semi_status_type_ctor.
memo_semi_active_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_semi_active"),
    TypeCtor = memo_semi_status_type_ctor.
memo_semi_succeeded_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_semi_succeeded"),
    TypeCtor = memo_semi_status_type_ctor.
memo_semi_failed_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_semi_failed"),
    TypeCtor = memo_semi_status_type_ctor.

:- func memo_non_inactive_cons_id = cons_id.
:- func memo_non_active_cons_id = cons_id.
:- func memo_non_incomplete_cons_id = cons_id.
:- func memo_non_complete_cons_id = cons_id.

memo_non_inactive_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_non_inactive"),
    TypeCtor = memo_non_status_type_ctor.
memo_non_active_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_non_active"),
    TypeCtor = memo_non_status_type_ctor.
memo_non_incomplete_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_non_incomplete"),
    TypeCtor = memo_non_status_type_ctor.
memo_non_complete_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "memo_non_complete"),
    TypeCtor = memo_non_status_type_ctor.

:- func mm_inactive_cons_id = cons_id.
:- func mm_active_cons_id = cons_id.
:- func mm_complete_cons_id = cons_id.

mm_inactive_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "mm_inactive"),
    TypeCtor = mm_status_type_ctor.
mm_active_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "mm_active"),
    TypeCtor = mm_status_type_ctor.
mm_complete_cons_id = cons(SymName, 0, TypeCtor) :-
    SymName = qualified(mercury_table_builtin_module, "mm_complete"),
    TypeCtor = mm_status_type_ctor.

:- func loop_status_type_ctor = type_ctor.

loop_status_type_ctor = TypeCtor :-
    TypeModule = mercury_table_builtin_module,
    TypeSymName = qualified(TypeModule, "loop_status"),
    TypeCtor = type_ctor(TypeSymName, 0).

:- func memo_det_status_type_ctor = type_ctor.

memo_det_status_type_ctor = TypeCtor :-
    TypeModule = mercury_table_builtin_module,
    TypeSymName = qualified(TypeModule, "memo_det_status"),
    TypeCtor = type_ctor(TypeSymName, 0).

:- func memo_semi_status_type_ctor = type_ctor.

memo_semi_status_type_ctor = TypeCtor :-
    TypeModule = mercury_table_builtin_module,
    TypeSymName = qualified(TypeModule, "memo_semi_status"),
    TypeCtor = type_ctor(TypeSymName, 0).

:- func memo_non_status_type_ctor = type_ctor.

memo_non_status_type_ctor = TypeCtor :-
    TypeModule = mercury_table_builtin_module,
    TypeSymName = qualified(TypeModule, "memo_non_status"),
    TypeCtor = type_ctor(TypeSymName, 0).

:- func mm_status_type_ctor = type_ctor.

mm_status_type_ctor = TypeCtor :-
    TypeModule = mercury_table_builtin_module,
    TypeSymName = qualified(TypeModule, "mm_status"),
    TypeCtor = type_ctor(TypeSymName, 0).

%-----------------------------------------------------------------------------%

:- func proc_table_info_type = mer_type.
:- func trie_node_type = mer_type.
:- func memo_non_record_type = mer_type.
:- func subgoal_type = mer_type.
:- func answer_block_type = mer_type.
:- func loop_status_type = mer_type.
:- func memo_det_status_type = mer_type.
:- func memo_semi_status_type = mer_type.
:- func memo_non_status_type = mer_type.
:- func mm_status_type = mer_type.

proc_table_info_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_proc_table_info"), 0),
        [], Type).

trie_node_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_trie_node"), 0), [], Type).

memo_non_record_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "memo_non_record"), 0), [], Type).

subgoal_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_subgoal"), 0), [], Type).

answer_block_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_answer_block"), 0), [], Type).

loop_status_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "loop_status"), 0), [], Type).

memo_det_status_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "memo_det_status"), 0), [], Type).

memo_semi_status_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "memo_semi_status"), 0), [], Type).

memo_non_status_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "memo_non_status"), 0), [], Type).

mm_status_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "mm_status"), 0), [], Type).

%-----------------------------------------------------------------------------%

:- func proc_table_info_name = string.
:- func cur_table_node_name = string.
:- func next_table_node_name = string.
:- func table_tip_node_name = string.
:- func base_name = string.
:- func memo_non_record_name = string.
:- func subgoal_name = string.
:- func status_name = string.
:- func answer_block_name = string.
:- func success_indicator_name = string.
:- func arg_name(int) = string.
:- func num_input_args_name = string.
:- func pred_name_var_name = string.
:- func answer_table_name = string.
:- func consumer_name = string.
:- func generator_name = string.
:- func generator_pred_name = string.
:- func returning_generator_locn = string.

:- pragma consider_used(table_tip_node_name/0).
:- pragma consider_used(num_input_args_name/0).
:- pragma consider_used(pred_name_var_name/0).
:- pragma consider_used(answer_table_name/0).

proc_table_info_name = "proc_table_info".
cur_table_node_name = "cur_node".
next_table_node_name = "next_node".
table_tip_node_name = "table_tip".
base_name = "base".
memo_non_record_name = "record".
subgoal_name = "subgoal".
status_name = "status".
answer_block_name = "answerblock".
success_indicator_name = "SUCCESS_INDICATOR".
arg_name(VarSeqNum) = "arg" ++ int_to_string(VarSeqNum).
num_input_args_name = "num_input_args".
pred_name_var_name = "pred_name".
answer_table_name = "answer_table".
consumer_name = "consumer".
generator_name = "generator".
generator_pred_name = "generator_pred".
returning_generator_locn = "MR_mmos_returning_generator".

%-----------------------------------------------------------------------------%

:- type table_info
    --->    table_info(
                table_module_info   :: module_info,
                table_cur_pred_info :: pred_info,
                table_cur_proc_info :: proc_info
            ).

:- pred table_info_init(module_info::in,
    pred_info::in, proc_info::in, table_info::out) is det.

:- pred table_info_extract(table_info::in, module_info::out,
    pred_info::out, proc_info::out) is det.

table_info_init(ModuleInfo, PredInfo, ProcInfo, TableInfo) :-
    TableInfo = table_info(ModuleInfo, PredInfo, ProcInfo).

table_info_extract(TableInfo, ModuleInfo, PredInfo, ProcInfo) :-
    TableInfo = table_info(ModuleInfo, PredInfo, ProcInfo).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.table_gen.
%-----------------------------------------------------------------------------%
