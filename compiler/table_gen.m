%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 The University of Melbourne.
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

:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred table_gen_process_module(module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.modes.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.purity.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.continuation_info.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.const_prop.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
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
table_gen_process_module(!ModuleInfo, !IO) :-
    module_info_preds(!.ModuleInfo, Preds0),
    map.keys(Preds0, PredIds),
    map.init(GenMap0),
    table_gen_process_preds(PredIds, !ModuleInfo, GenMap0, _, !IO).

:- pred table_gen_process_preds(list(pred_id)::in,
    module_info::in, module_info::out,
    generator_map::in, generator_map::out, io::di, io::uo) is det.

table_gen_process_preds([], !ModuleInfo, !GenMap, !IO).
table_gen_process_preds([PredId | PredIds], !ModuleInfo, !GenMap, !IO) :-
    table_gen_process_pred(PredId, !ModuleInfo, !GenMap, !IO),
    table_gen_process_preds(PredIds, !ModuleInfo, !GenMap, !IO).

:- pred table_gen_process_pred(pred_id::in, module_info::in, module_info::out,
    generator_map::in, generator_map::out, io::di, io::uo) is det.

table_gen_process_pred(PredId, !ModuleInfo, !GenMap, !IO) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    table_gen_process_procs(PredId, ProcIds, !ModuleInfo, !GenMap, !IO).

:- pred table_gen_process_procs(pred_id::in, list(proc_id)::in,
    module_info::in, module_info::out,
    generator_map::in, generator_map::out, io::di, io::uo) is det.

table_gen_process_procs(_PredId, [], !ModuleInfo, !GenMap, !IO).
table_gen_process_procs(PredId, [ProcId | ProcIds], !ModuleInfo, !GenMap,
        !IO) :-
    module_info_preds(!.ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_procedures(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo0),
    table_gen_process_proc(PredId, ProcId, ProcInfo0, PredInfo,
        !ModuleInfo, !GenMap, !IO),
    table_gen_process_procs(PredId, ProcIds, !ModuleInfo, !GenMap, !IO).

:- pred table_gen_process_proc(pred_id::in, proc_id::in, proc_info::in,
    pred_info::in, module_info::in, module_info::out,
    generator_map::in, generator_map::out, io::di, io::uo) is det.

table_gen_process_proc(PredId, ProcId, ProcInfo0, PredInfo0, !ModuleInfo,
        !GenMap, !IO) :-
    proc_info_get_eval_method(ProcInfo0, EvalMethod),
    ( eval_method_requires_tabling_transform(EvalMethod) = yes ->
        table_gen_transform_proc_if_possible(EvalMethod, PredId,
            ProcId, ProcInfo0, _, PredInfo0, _, !ModuleInfo, !GenMap, !IO)
    ;
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, trace_table_io, yes),
        proc_info_has_io_state_pair(!.ModuleInfo, ProcInfo0,
            _InArgNum, _OutArgNum)
    ->
        proc_info_interface_code_model(ProcInfo0, CodeModel),
        ( CodeModel = model_det ->
            true
        ;
            pred_id_to_int(PredId, PredIdInt),
            Msg = string.format("I/O procedure pred id %d not model_det",
                [i(PredIdInt)]),
            unexpected(this_file, Msg)
        ),
        globals.lookup_bool_option(Globals, trace_table_io_all, TransformAll),
        globals.lookup_bool_option(Globals, trace_table_io_require, Require),
        proc_info_get_goal(ProcInfo0, BodyGoal),
        PredModuleName = predicate_module(!.ModuleInfo, PredId),
        should_io_procedure_be_transformed(TransformAll, Require, BodyGoal,
            PredModuleName, AnnotationIsMissing, TransformPrimitive),
        (
            AnnotationIsMissing = yes,
            report_missing_tabled_for_io(PredInfo0, PredId, ProcId,
                !ModuleInfo, !IO)
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
                Decl = table_io_decl
            ;
                TraceTableIoOnlyRetry = yes,
                Decl = table_io_proc
            ),
            TableIoMethod = eval_table_io(Decl, Unitize),
            proc_info_set_eval_method(TableIoMethod, ProcInfo0, ProcInfo1),
            table_gen_transform_proc_if_possible(TableIoMethod,
                PredId, ProcId, ProcInfo1, _, PredInfo0, _, !ModuleInfo,
                !GenMap, !IO)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%

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
            (
                Require = yes,
                \+ any_mercury_builtin_module(PredModuleName)
            ->
                AnnotationIsMissing = yes,
                TransformInfo = no
            ;
                AnnotationIsMissing = no,
                (
                    TransformAll = no,
                    TransformInfo = no
                ;
                    TransformAll = yes,
                    may_call_mercury_attributes(BodyGoal, MayCallMercuryAttrs),
                    ( MayCallMercuryAttrs = [proc_may_call_mercury] ->
                        TransformInfo = no
                    ;
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
        unexpected(this_file, "should_io_procedure_be_transformed: " ++
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
        SubGoal = call_foreign_proc(Attrs, _, _, _, _, _, _) - _,
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
        SubGoal = call_foreign_proc(Attrs, _, _, _, _, _, _) - _,
        TabledForIoAttr = get_tabled_for_io(Attrs),
        \+ TabledForIoAttr = proc_not_tabled_for_io
    ).

:- pred report_missing_tabled_for_io(pred_info::in, pred_id::in, proc_id::in,
    module_info::in, module_info::out, io::di, io::uo) is det.

report_missing_tabled_for_io(PredInfo, PredId, ProcId, !ModuleInfo, !IO) :-
    pred_info_context(PredInfo, Context),
    ProcPieces = describe_one_proc_name(!.ModuleInfo,
        should_not_module_qualify, proc(PredId, ProcId)),
    Pieces = ProcPieces ++ [words("contains untabled I/O primitive."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_code_gen, [Msg]),
    module_info_get_globals(!.ModuleInfo, Globals),
    write_error_spec(Spec, Globals, 0, _NumWarnings, 0, NumErrors, !IO),
    module_info_incr_num_errors(NumErrors, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred table_gen_transform_proc_if_possible(eval_method::in,
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    pred_info::in, pred_info::out, module_info::in, module_info::out,
    generator_map::in, generator_map::out, io::di, io::uo) is det.

table_gen_transform_proc_if_possible(EvalMethod, PredId, ProcId,
        !ProcInfo, !PredInfo, !ModuleInfo, !GenMap, !IO) :-
    globals.io_get_target(Target, !IO),
    globals.io_get_gc_method(GC_Method, !IO),
    ( Target = target_c, GC_Method \= gc_accurate ->
        table_gen_transform_proc(EvalMethod, PredId, ProcId,
            !ProcInfo, !PredInfo, !ModuleInfo, !GenMap, !IO)
    ;
        pred_info_context(!.PredInfo, Context),
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
        module_info_get_globals(!.ModuleInfo, Globals),
        write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),

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
    generator_map::in, generator_map::out, io::di, io::uo) is det.

table_gen_transform_proc(EvalMethod, PredId, ProcId, !ProcInfo, !PredInfo,
        !ModuleInfo, !GenMap, !IO) :-
    table_info_init(!.ModuleInfo, !.PredInfo, !.ProcInfo, TableInfo0),

    % grab the appropriate fields from the pred_info and proc_info
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
        unexpected(this_file, "table_gen_transform_proc: eval_normal")
    ;
        EvalMethod = eval_table_io(_, _),
        expect(unify(MaybeAttributes, no), this_file,
            "table_gen_transform_proc: eval_table_io and Attributes"),
        % Since we don't actually create a call table for I/O tabled
        % procedures, the value of MaybeSpecMethod doesn't really matter.
        MaybeSpecMethod = all_same(arg_value),
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
            CallStrictness = all_strict,
            MaybeSpecMethod = all_same(arg_value)
        ;
            CallStrictness = all_fast_loose,
            MaybeSpecMethod = all_same(arg_addr)
        ;
            CallStrictness = specified(ArgMethods),
            MaybeSpecMethod = specified(ArgMethods)
        ),
        ( EvalMethod = eval_minimal(_) ->
            expect(unify(MaybeSizeLimit, no), this_file,
                "eval_minimal with size limit"),
            expect(unify(MaybeSpecMethod, all_same(arg_value)), this_file,
                "eval_minimal without all_strict")
        ;
            true
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
            TableInfo0, TableInfo, Goal, MaybeProcTableInfo),
        MaybeCallTableTip = no
    ;
        EvalMethod = eval_loop_check,
        create_new_loop_goal(Detism, OrigGoal, Statistics,
            PredId, ProcId, HeadVars, NumberedInputVars, NumberedOutputVars,
            VarSet0, VarSet, VarTypes0, VarTypes,
            TableInfo0, TableInfo, CallTableTip, Goal, InputSteps),
        generate_gen_proc_table_info(TableInfo, InputSteps, no,
            InputVarModeMethods, OutputVarModeMethods, ProcTableInfo),
        MaybeCallTableTip = yes(CallTableTip),
        MaybeProcTableInfo = yes(ProcTableInfo)
    ;
        EvalMethod = eval_memo,
        ( CodeModel = model_non ->
            create_new_memo_non_goal(Detism, OrigGoal, Statistics,
                MaybeSizeLimit, PredId, ProcId,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes, TableInfo0, TableInfo,
                CallTableTip, Goal, InputSteps, OutputSteps),
                MaybeOutputSteps = yes(OutputSteps)
        ;
            create_new_memo_goal(Detism, OrigGoal, Statistics, MaybeSizeLimit,
                PredId, ProcId,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes,
                TableInfo0, TableInfo, CallTableTip, Goal, InputSteps),
                MaybeOutputSteps = no
        ),
        generate_gen_proc_table_info(TableInfo, InputSteps, MaybeOutputSteps,
            InputVarModeMethods, OutputVarModeMethods, ProcTableInfo),
        MaybeCallTableTip = yes(CallTableTip),
        MaybeProcTableInfo = yes(ProcTableInfo)
    ;
        EvalMethod = eval_minimal(MinimalMethod),
        (
            CodeModel = model_det,
            unexpected(this_file, "table_gen_transform_proc: minimal det")
        ;
            CodeModel = model_semi,
            unexpected(this_file, "table_gen_transform_proc: minimal semi")
        ;
            CodeModel = model_non,
            MinimalMethod = stack_copy,
            create_new_mm_goal(Detism, OrigGoal, Statistics, PredId, ProcId,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes, TableInfo0, TableInfo,
                CallTableTip, Goal, InputSteps, OutputSteps),
            MaybeCallTableTip = yes(CallTableTip)
        ;
            CodeModel = model_non,
            MinimalMethod = own_stacks,
            do_own_stack_transform(Detism, OrigGoal, Statistics,
                PredId, ProcId, !.PredInfo, !.ProcInfo,
                HeadVars, NumberedInputVars, NumberedOutputVars,
                VarSet0, VarSet, VarTypes0, VarTypes, TableInfo0, TableInfo,
                !GenMap, Goal, InputSteps, OutputSteps),
            MaybeCallTableTip = no
        ),
        MaybeOutputSteps = yes(OutputSteps),
        generate_gen_proc_table_info(TableInfo, InputSteps, MaybeOutputSteps,
            InputVarModeMethods, OutputVarModeMethods, ProcTableInfo),
        MaybeProcTableInfo = yes(ProcTableInfo)
    ),

    table_info_extract(TableInfo, !:ModuleInfo, !:PredInfo, !:ProcInfo),

    % set the new values of the fields in proc_info and pred_info
    % and save in the module info
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_call_table_tip(MaybeCallTableTip, !ProcInfo),
    proc_info_set_maybe_proc_table_info(MaybeProcTableInfo, !ProcInfo),

    % Some of the instmap_deltas generated in this module
    % are pretty dodgy (especially those for if-then-elses), so
    % recompute them here.
    RecomputeAtomic = no,
    recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo),

    pred_info_get_procedures(!.PredInfo, ProcTable1),
    map.det_update(ProcTable1, ProcId, !.ProcInfo, ProcTable),
    pred_info_set_procedures(ProcTable, !PredInfo),

    % The transformation doesn't pay attention to the purity
    % of compound goals, so recompute the purity here.
    repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
    module_info_preds(!.ModuleInfo, PredTable1),
    map.det_update(PredTable1, PredId, !.PredInfo, PredTable),
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
%       (if
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

:- pred create_new_loop_goal(determinism::in, hlds_goal::in,
    table_attr_statistics::in, pred_id::in, proc_id::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, hlds_goal::out,
    list(table_trie_step)::out) is det.

create_new_loop_goal(Detism, OrigGoal, Statistics, PredId, ProcId,
        HeadVars, NumberedInputVars, NumberedOutputVars, !VarSet, !VarTypes,
        !TableInfo, TableTipVar, Goal, Steps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    goal_info_get_context(OrigGoalInfo, Context),

    ModuleInfo = !.TableInfo ^ table_module_info,
    generate_simple_call_table_lookup_goal(loop_status_type,
        "table_loop_setup_shortcut", "MR_tbl_loop_setup",
        NumberedInputVars, PredId, ProcId, Statistics, Context,
        !VarSet, !VarTypes, !TableInfo, TableTipVar, StatusVar,
        LookUpGoal, Steps),

    generate_error_goal(!.TableInfo, Context, infinite_recursion_msg,
        !VarSet, !VarTypes, ActiveGoal),

    TableTipArg = foreign_arg(TableTipVar,
        yes(cur_table_node_name - in_mode), trie_node_type,
        native_if_possible),

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
        tabling_c_attributes, [TableTipArg], [],
        MarkInactiveCode, purity_impure, [],
        ModuleInfo, Context, MarkInactiveGoal),
    table_generate_foreign_proc(MarkInactiveFailPredName, detism_failure,
        tabling_c_attributes, [TableTipArg], [],
        MarkInactiveFailCode, purity_impure, [],
        ModuleInfo, Context, MarkInactiveFailGoal),
    table_generate_foreign_proc(MarkActiveFailPredName, detism_failure,
        tabling_c_attributes, [TableTipArg], [],
        MarkActiveFailCode, purity_impure, [],
        ModuleInfo, Context, MarkActiveFailGoal),

    determinism_to_code_model(Detism, CodeModel),
    set.list_to_set([TableTipVar | HeadVars], InactiveNonLocals),
    OutputVars = list.map(project_var, NumberedOutputVars),
    InactiveInstmapDelta = bind_vars(OutputVars),
    (
        CodeModel = model_det,
        InactiveGoalExpr = conj(plain_conj, [OrigGoal, MarkInactiveGoal])
    ;
        CodeModel = model_semi,
        goal_info_get_instmap_delta(OrigGoalInfo, InstMapDelta),
        create_renaming(OutputVars, InstMapDelta, !VarSet, !VarTypes,
            Unifies, NewVars, Renaming),
        rename_some_vars_in_goal(Renaming, OrigGoal, RenamedOrigGoal),

        ThenGoalExpr = conj(plain_conj, Unifies ++ [MarkInactiveGoal]),
        ThenVars = [TableTipVar | OutputVars] ++ NewVars,
        set.list_to_set(ThenVars, ThenNonLocals),
        goal_info_init_hide(ThenNonLocals, InactiveInstmapDelta, Detism,
            purity_impure, Context, ThenGoalInfo),
        ThenGoal = ThenGoalExpr - ThenGoalInfo,

        InactiveGoalExpr = if_then_else([], RenamedOrigGoal,
            ThenGoal, MarkInactiveFailGoal)
    ;
        CodeModel = model_non,
        AfterGoalExpr = disj([MarkInactiveGoal, MarkActiveFailGoal]),
        instmap_delta_init_reachable(AfterInstMapDelta),
        goal_info_init_hide(set.make_singleton_set(TableTipVar),
            AfterInstMapDelta, detism_multi, purity_impure, Context,
            AfterGoalInfo),
        AfterGoal = AfterGoalExpr - AfterGoalInfo,
        FirstGoalExpr = conj(plain_conj, [OrigGoal, AfterGoal]),
        goal_info_get_nonlocals(OrigGoalInfo, OrigGINonLocals),
        set.insert(OrigGINonLocals, TableTipVar, FirstNonlocals),
        goal_info_set_nonlocals(FirstNonlocals, OrigGoalInfo, FirstGoalInfo),
        FirstGoal = FirstGoalExpr - FirstGoalInfo,
        InactiveGoalExpr = disj([FirstGoal, MarkInactiveFailGoal])
    ),
    goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta, Detism,
        purity_impure, Context, InactiveGoalInfo),
    InactiveGoal = InactiveGoalExpr - InactiveGoalInfo,

    TB = mercury_table_builtin_module,
    SwitchArms = [
        case(cons(qualified(TB, "loop_active"), 0), ActiveGoal),
        case(cons(qualified(TB, "loop_inactive"), 0), InactiveGoal)
    ],
    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    set.insert_list(InactiveNonLocals, [StatusVar, TableTipVar],
        SwitchNonLocals),
    goal_info_init_hide(SwitchNonLocals, InactiveInstmapDelta, Detism,
        purity_impure, Context, SwitchGoalInfo),
    SwitchGoal = SwitchExpr - SwitchGoalInfo,

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism,
        purity_impure, Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

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
%       (if
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
    list(table_trie_step)::out) is det.

create_new_memo_goal(Detism, OrigGoal, Statistics, _MaybeSizeLimit,
        PredId, ProcId, HeadVars, NumberedInputVars, NumberedOutputVars,
        !VarSet, !VarTypes, !TableInfo, TableTipVar, Goal, Steps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    goal_info_get_context(OrigGoalInfo, Context),

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
        unexpected(this_file, "create_new_memo_goal: model_non")
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

    set.list_to_set([TableTipVar | HeadVars], InactiveNonLocals),
    OutputVars = list.map(project_var, NumberedOutputVars),
    InactiveInstmapDelta = bind_vars(OutputVars),

    % The case CodeModel = model_non was caught by the code above.
    (
        CodeModel = model_det,
        InactiveGoalExpr = conj(plain_conj, [OrigGoal | SaveAnswerGoals]),
        goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta,
            Detism, purity_impure, Context, InactiveGoalInfo),
        InactiveGoal = InactiveGoalExpr - InactiveGoalInfo,

        TB = mercury_table_builtin_module,
        SwitchArms = [
            case(cons(qualified(TB, "memo_det_active"), 0), ActiveGoal),
            case(cons(qualified(TB, "memo_det_inactive"), 0), InactiveGoal),
            case(cons(qualified(TB, "memo_det_succeeded"), 0), SucceededGoal)
        ]
    ;
        CodeModel = model_semi,
        create_renaming(OutputVars, OrigInstMapDelta, !VarSet, !VarTypes,
            Unifies, NewVars, Renaming),
        rename_some_vars_in_goal(Renaming, OrigGoal, RenamedOrigGoal),

        ThenGoalExpr = conj(plain_conj, Unifies ++ SaveAnswerGoals),
        ThenVars = [TableTipVar | OutputVars] ++ NewVars,
        set.list_to_set(ThenVars, ThenNonLocals),
        goal_info_init_hide(ThenNonLocals, InactiveInstmapDelta,
            detism_det, purity_impure, Context, ThenGoalInfo),
        ThenGoal = ThenGoalExpr - ThenGoalInfo,

        MarkAsFailedPredName = "table_memo_mark_as_failed",
        MarkAsFailedMacroName = "MR_tbl_memo_mark_as_failed",
        TableTipArg = foreign_arg(TableTipVar,
            yes(cur_table_node_name - in_mode), trie_node_type,
            native_if_possible),
        DebugArgStr = get_debug_arg_string(!.TableInfo),
        MarkAsFailedCode = MarkAsFailedMacroName ++
            "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");",
        table_generate_foreign_proc(MarkAsFailedPredName, detism_failure,
            tabling_c_attributes, [TableTipArg], [],
            MarkAsFailedCode, purity_impure, [],
            ModuleInfo, Context, ElseGoal),
        InactiveGoalExpr = if_then_else([], RenamedOrigGoal,
            ThenGoal, ElseGoal),
        goal_info_init_hide(InactiveNonLocals, InactiveInstmapDelta, Detism,
            purity_impure, Context, InactiveGoalInfo),
        InactiveGoal = InactiveGoalExpr - InactiveGoalInfo,
        FailedGoal = fail_goal,

        TB = mercury_table_builtin_module,
        SwitchArms = [
            case(cons(qualified(TB, "memo_semi_active"), 0), ActiveGoal),
            case(cons(qualified(TB, "memo_semi_inactive"), 0), InactiveGoal),
            case(cons(qualified(TB, "memo_semi_succeeded"), 0), SucceededGoal),
            case(cons(qualified(TB, "memo_semi_failed"), 0), FailedGoal)
        ]
    ),

    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    set.insert(InactiveNonLocals, StatusVar, SwitchNonLocals),
    goal_info_init_hide(SwitchNonLocals, InactiveInstmapDelta,
        Detism, purity_impure, Context, SwitchGoalInfo),
    SwitchGoal = SwitchExpr - SwitchGoalInfo,

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

:- pred create_new_memo_non_goal(determinism::in, hlds_goal::in,
    table_attr_statistics::in, maybe(int)::in,
    pred_id::in, proc_id::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, hlds_goal::out,
    list(table_trie_step)::out, list(table_trie_step)::out) is det.

create_new_memo_non_goal(Detism, OrigGoal, Statistics, _MaybeSizeLimit,
        PredId, ProcId, HeadVars, NumberedInputVars, NumberedOutputVars,
        !VarSet, !VarTypes, !TableInfo, RecordVar, Goal,
        InputSteps, OutputSteps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    goal_info_get_context(OrigGoalInfo, Context),

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
        yes(RecordVarName - in_mode), memo_non_record_type,
        native_if_possible),

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
        tabling_c_attributes, [RecordArg], [],
        MarkIncompleteCode, purity_impure, [],
        ModuleInfo, Context, MarkIncompleteGoal),
    table_generate_foreign_proc(MarkActivePredName, detism_failure,
        tabling_c_attributes, [RecordArg], [],
        MarkActiveCode, purity_impure, [],
        ModuleInfo, Context, MarkActiveGoal),
    table_generate_foreign_proc(MarkCompletePredName, detism_failure,
        tabling_c_attributes, [RecordArg], [],
        MarkCompleteCode, purity_impure, [],
        ModuleInfo, Context, MarkCompleteGoal),

    OrigSaveExpr = conj(plain_conj, [OrigGoal | SaveAnswerGoals]),
    set.insert(OrigNonLocals, RecordVar, OrigSaveNonLocals),
    create_instmap_delta([OrigGoal | SaveAnswerGoals], OrigSaveIMD0),
    instmap_delta_restrict(OrigSaveNonLocals, OrigSaveIMD0, OrigSaveIMD),
    goal_info_init_hide(OrigSaveNonLocals, OrigSaveIMD, detism_non,
        purity_impure, Context, OrigSaveGoalInfo),
    OrigSaveGoal = OrigSaveExpr - OrigSaveGoalInfo,

    AfterExpr = disj([MarkIncompleteGoal, MarkActiveGoal]),
    AfterNonLocals = set.make_singleton_set(RecordVar),
    create_instmap_delta([], AfterInstMapDelta),
    goal_info_init_hide(AfterNonLocals, AfterInstMapDelta, detism_non,
        purity_impure, Context, AfterGoalInfo),
    AfterGoal = AfterExpr - AfterGoalInfo,

    OrigSaveAfterExpr = conj(plain_conj, [OrigSaveGoal, AfterGoal]),
    OrigSaveAfterGoal = OrigSaveAfterExpr - OrigSaveGoalInfo,

    InactiveExpr = disj([OrigSaveAfterGoal, MarkCompleteGoal]),
    InactiveGoal = InactiveExpr - OrigSaveGoalInfo,

    set.list_to_set([RecordVar | HeadVars], InactiveNonLocals),
    OutputVars = list.map(project_var, NumberedOutputVars),
    InactiveInstmapDelta = bind_vars(OutputVars),

    TB = mercury_table_builtin_module,
    SwitchArms = [
        case(cons(qualified(TB, "memo_non_active"), 0), InfiniteRecursionGoal),
        case(cons(qualified(TB, "memo_non_inactive"), 0), InactiveGoal),
        case(cons(qualified(TB, "memo_non_incomplete"), 0), NeedMinModelGoal),
        case(cons(qualified(TB, "memo_non_complete"), 0), RestoreAllAnswerGoal)
    ],

    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    set.insert(InactiveNonLocals, StatusVar, SwitchNonLocals),
    goal_info_init_hide(SwitchNonLocals, InactiveInstmapDelta, Detism,
        purity_impure, Context, SwitchGoalInfo),
    SwitchGoal = SwitchExpr - SwitchGoalInfo,

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

% Example of transformation for tabling I/O, for I/O primitives (i.e.
% predicates defined by pragma c_code that take an input/output pair of
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
%   (if
%           % Get the global I/O table, the global I/O
%           % counter, and the starting point for tabling
%           % I/O actions, if we are in the tabled range.
%       table_io_in_range(T0, Counter, Start)
%   then
%           % Look up the input arguments.
%       impure table_lookup_insert_start_int(T0, Counter,
%           Start, T),
%       (if
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

:- pred create_new_io_goal(hlds_goal::in, table_io_is_decl::in,
    table_io_is_unitize::in, bool::in, pred_id::in, proc_id::in,
    assoc_list(prog_var, mer_mode)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out,
    hlds_goal::out, maybe(proc_table_info)::out) is det.

create_new_io_goal(OrigGoal, TableDecl, Unitize, TableIoStates,
        PredId, ProcId, HeadVarModes, OrigInputVars, OrigOutputVars,
        !VarSet, !VarTypes, !TableInfo, Goal, MaybeProcTableInfo) :-
    OrigGoal = _ - OrigGoalInfo,
    ModuleInfo0 = !.TableInfo ^ table_module_info,
    module_info_pred_info(ModuleInfo0, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( check_marker(Markers, marker_user_marked_no_inline) ->
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
        NewGoal = CallExpr - OrigGoalInfo,
        !:TableInfo = !.TableInfo ^ table_module_info := ModuleInfo
    ;
        NewGoal = OrigGoal,
        ModuleInfo = ModuleInfo0
    ),
    goal_info_get_nonlocals(OrigGoalInfo, OrigNonLocals),
    goal_info_get_context(OrigGoalInfo, Context),

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
        ground_vars([TableVar, CounterVar, StartVar]),
        ModuleInfo, Context, InRangeGoal),
    generate_new_table_var("TipVar", trie_node_type, !VarSet, !VarTypes,
        TipVar),
    table_generate_call("table_lookup_insert_start_int", detism_det,
        [TableVar, StartVar, CounterVar, TipVar], purity_impure,
        ground_vars([TipVar]), ModuleInfo, Context, LookupGoal),
    table_generate_call("table_io_has_occurred", detism_semi, [TipVar],
        purity_impure, [], ModuleInfo, Context, OccurredGoal),
    (
        TableDecl = table_io_decl,
        ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
        TableIoDeclConsId = table_io_decl(ShroudedPredProcId),
        make_const_construction_alloc(TableIoDeclConsId, c_pointer_type,
            yes("TableIoDeclPtr"), TableIoDeclGoal, TableIoDeclPtrVar,
            !VarSet, !VarTypes),
        allocate_plain_slot_numbers(SavedHeadVars, 1, NumberedSavedHeadVars),
        NumberedSaveVars = [
            var_mode_pos_method(TableIoDeclPtrVar, in_mode, 0, unit)
            | NumberedSavedHeadVars],
        UnnumberedSavedOutputVars = list.map(project_var, SavedOutputVars),
        list.filter(var_belong_to_list(UnnumberedSavedOutputVars),
            NumberedSaveVars, NumberedSavedOutputVars),
        NumberedRestoreVars = NumberedSavedOutputVars,

        ProcInfo0 = !.TableInfo ^ table_cur_proc_info,
        continuation_info.generate_table_arg_type_info(ProcInfo0,
            list.map(project_var_pos, NumberedSavedHeadVars),
            TableArgTypeInfo),
        ProcTableInfo = table_io_decl_info(TableArgTypeInfo),
        MaybeProcTableInfo = yes(ProcTableInfo)
    ;
        TableDecl = table_io_proc,
        TableIoDeclGoal = true_goal,
        NumberedRestoreVars =
            list.map(project_out_arg_method, SavedOutputVars),
        NumberedSaveVars = list.map(project_out_arg_method, SavedOutputVars),
        MaybeProcTableInfo = no
    ),
    list.length(NumberedSaveVars, BlockSize),
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    generate_memo_restore_goal(NumberedRestoreVars, OrigInstMapDelta, TipVar,
        Context, !VarSet, !VarTypes, !.TableInfo, RestoreAnswerGoal0),
    (
        TableIoStates = yes,
        RestoreAnswerGoal = RestoreAnswerGoal0
    ;
        TableIoStates = no,
        (
            IoStateAssignFromVars = [IoStateAssignFromVarPrime],
            IoStateAssignToVars = [IoStateAssignToVarPrime]
        ->
            IoStateAssignFromVar = project_var(IoStateAssignFromVarPrime),
            IoStateAssignToVar = project_var(IoStateAssignToVarPrime)
        ;
            % The call to proc_info_has_io_state_pair in
            % table_gen_process_procs should ensure that we never get here.
            unexpected(this_file,
                "create_new_io_goal: one in / one out violation")
        ),
        table_generate_call("table_io_copy_io_state", detism_det,
            [IoStateAssignFromVar, IoStateAssignToVar], purity_pure,
            [IoStateAssignFromVar - ground(clobbered, none),
            IoStateAssignToVar - ground(unique, none)],
            ModuleInfo, Context, IoStateAssignGoal),
        RestoreAnswerGoalExpr = conj(plain_conj,
            [RestoreAnswerGoal0, IoStateAssignGoal]),
        create_instmap_delta([RestoreAnswerGoal0, IoStateAssignGoal],
            RestoreAnswerInstMapDelta0),
        RestoreAnswerGoal0 = _ - RestoreAnswerGoal0Info,
        goal_info_get_nonlocals(RestoreAnswerGoal0Info,
            RestoreAnswer0NonLocals),
        set.insert_list(RestoreAnswer0NonLocals,
            [IoStateAssignFromVar, IoStateAssignToVar],
            RestoreAnswerNonLocals),
        instmap_delta_restrict(RestoreAnswerNonLocals,
            RestoreAnswerInstMapDelta0, RestoreAnswerInstMapDelta),
        goal_info_init_hide(RestoreAnswerNonLocals,
            RestoreAnswerInstMapDelta, detism_det, purity_semipure, Context,
            RestoreAnswerGoalInfo),
        RestoreAnswerGoal = RestoreAnswerGoalExpr - RestoreAnswerGoalInfo
    ),
    generate_memo_save_goal(NumberedSaveVars, TipVar, BlockSize,
        Context, !VarSet, !VarTypes, !TableInfo, SaveAnswerGoals),
    (
        Unitize = table_io_alone,
        CallSaveAnswerGoalList = [NewGoal, TableIoDeclGoal | SaveAnswerGoals]
    ;
        Unitize = table_io_unitize,
        generate_new_table_var("SavedTraceEnabled", int_type,
            !VarSet, !VarTypes, SavedTraceEnabledVar),
        table_generate_call("table_io_left_bracket_unitized_goal", detism_det,
            [SavedTraceEnabledVar], purity_impure,
            ground_vars([SavedTraceEnabledVar]),
            ModuleInfo, Context, LeftBracketGoal),
        table_generate_call("table_io_right_bracket_unitized_goal", detism_det,
            [SavedTraceEnabledVar], purity_impure, [],
            ModuleInfo, Context, RightBracketGoal),
        CallSaveAnswerGoalList = [LeftBracketGoal, NewGoal,
            RightBracketGoal, TableIoDeclGoal | SaveAnswerGoals]
    ),
    CallSaveAnswerGoalExpr = conj(plain_conj, CallSaveAnswerGoalList),
    create_instmap_delta(CallSaveAnswerGoalList, CallSaveAnswerInstMapDelta0),
    set.insert(OrigNonLocals, TipVar, CallSaveAnswerNonLocals),
    instmap_delta_restrict(CallSaveAnswerNonLocals,
        CallSaveAnswerInstMapDelta0, CallSaveAnswerInstMapDelta),
    goal_info_init_hide(CallSaveAnswerNonLocals, CallSaveAnswerInstMapDelta,
        detism_det, purity_impure, Context, CallSaveAnswerGoalInfo0),
    goal_info_add_feature(feature_hide_debug_event,
        CallSaveAnswerGoalInfo0, CallSaveAnswerGoalInfo),
    CallSaveAnswerGoal = CallSaveAnswerGoalExpr - CallSaveAnswerGoalInfo,

    GenIfNecGoalExpr = if_then_else([], OccurredGoal,
        RestoreAnswerGoal, CallSaveAnswerGoal),
    create_instmap_delta([OccurredGoal, RestoreAnswerGoal,
        CallSaveAnswerGoal], GenIfNecInstMapDelta0),
    set.insert(OrigNonLocals, TipVar, GenIfNecNonLocals),
    instmap_delta_restrict(GenIfNecNonLocals,
        GenIfNecInstMapDelta0, GenIfNecInstMapDelta),
    goal_info_init_hide(GenIfNecNonLocals, GenIfNecInstMapDelta, detism_det,
        purity_impure, Context, GenIfNecGoalInfo),
    GenIfNecGoal = GenIfNecGoalExpr - GenIfNecGoalInfo,

    CheckAndGenAnswerGoalExpr = conj(plain_conj, [LookupGoal, GenIfNecGoal]),
    create_instmap_delta([LookupGoal, GenIfNecGoal],
        CheckAndGenAnswerInstMapDelta0),
    set.insert_list(OrigNonLocals, [TableVar, CounterVar, StartVar],
        CheckAndGenAnswerNonLocals),
    instmap_delta_restrict(CheckAndGenAnswerNonLocals,
        CheckAndGenAnswerInstMapDelta0, CheckAndGenAnswerInstMapDelta),
    goal_info_init_hide(CheckAndGenAnswerNonLocals,
        CheckAndGenAnswerInstMapDelta, detism_det, purity_impure, Context,
        CheckAndGenAnswerGoalInfo),
    CheckAndGenAnswerGoal = CheckAndGenAnswerGoalExpr
        - CheckAndGenAnswerGoalInfo,

    BodyGoalExpr = if_then_else([], InRangeGoal, CheckAndGenAnswerGoal,
        NewGoal),
    create_instmap_delta([InRangeGoal, CheckAndGenAnswerGoal, NewGoal],
        BodyInstMapDelta0),
    instmap_delta_restrict(OrigNonLocals, BodyInstMapDelta0, BodyInstMapDelta),
    goal_info_init_hide(OrigNonLocals, BodyInstMapDelta, detism_det,
        purity_impure, Context, BodyGoalInfo),
    Goal = BodyGoalExpr - BodyGoalInfo.

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
    list(table_trie_step)::out, list(table_trie_step)::out) is det.

create_new_mm_goal(Detism, OrigGoal, Statistics, PredId, ProcId,
        HeadVars, NumberedInputVars, NumberedOutputVars, !VarSet, !VarTypes,
        !TableInfo, SubgoalVar, Goal, InputSteps, OutputSteps) :-
    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    goal_info_get_context(OrigGoalInfo, Context),

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
    set.insert_list(OrigNonLocals, [SubgoalVar, StatusVar], MainNonLocals),
    create_instmap_delta([OrigGoal | SaveAnswerGoals], MainIMD0),
    instmap_delta_restrict(MainNonLocals, MainIMD0, MainIMD),
    goal_info_init_hide(MainNonLocals, MainIMD, detism_non, purity_impure,
        Context, MainGoalInfo),
    MainGoal = MainExpr - MainGoalInfo,

    table_generate_call("table_mm_completion", detism_det, [SubgoalVar],
        purity_impure, [], ModuleInfo, Context, ResumeGoal0),
    append_fail(ResumeGoal0, ResumeGoal),
    InactiveExpr = disj([MainGoal, ResumeGoal]),
    InactiveGoal = InactiveExpr - MainGoalInfo,

    TB = mercury_table_builtin_module,
    SwitchArms = [
        case(cons(qualified(TB, "mm_inactive"), 0), InactiveGoal),
        case(cons(qualified(TB, "mm_complete"), 0), RestoreAllAnswerGoal),
        case(cons(qualified(TB, "mm_active"), 0), SuspendGoal)
    ],
    SwitchExpr = switch(StatusVar, cannot_fail, SwitchArms),
    goal_info_add_feature(feature_hide_debug_event,
        MainGoalInfo, SwitchGoalInfo),
    SwitchGoal = SwitchExpr - SwitchGoalInfo,

    GoalExpr = conj(plain_conj, [LookUpGoal, SwitchGoal]),
    goal_info_init_hide(OrigNonLocals, OrigInstMapDelta, detism_non,
        purity_impure, Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

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
%           % Check for duplicate answers.
%       semipure table_mmos_get_answer_table(Generator, AT0),
%       impure table_lookup_insert_int(AT0, B, AT1),
%           Fail if the answer is already in the table;
%           otherwise, put it into the table.
%       impure table_mmos_answer_is_not_duplicate(AT1),
%
%           % Save the new answer in the table.
%       impure table_mmos_create_answer_block(Generator, 1,
%           AnswerBlock),
%       impure table_save_int_ans(AnswerBlock, 0, B),
%       impure table_mmos_return_answer(Generator, AnswerBlock)
%   ;
%       impure table_mmos_completion(Generator)
%   ).
%
% p(A, B) :-
%   table_mmos_save_inputs(1, A),       % into global variable
%   CT0 = <table pointer for p/2>,
%   impure table_lookup_insert_int(CT0, A, CT1),
%   impure table_mmos_setup_consumer(CT1, 1, p2_gen, "p/2", Consumer),
%   impure table_mmos_consume_next_answer_nondet(Consumer, AnswerBlock),
%   impure table_restore_int_ans(AnswerBlock, 0, B).

:- pred do_own_stack_transform(determinism::in, hlds_goal::in,
    table_attr_statistics::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, list(prog_var)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, generator_map::in, generator_map::out,
    hlds_goal::out, list(table_trie_step)::out, list(table_trie_step)::out)
    is det.

do_own_stack_transform(Detism, OrigGoal, Statistics, PredId, ProcId,
        PredInfo0, ProcInfo0, HeadVars, NumberedInputVars, NumberedOutputVars,
        !VarSet, !VarTypes, !TableInfo, !GenMap, Goal,
        InputSteps, OutputSteps) :-
    PredName = pred_info_name(PredInfo0),
    ( map.search(!.GenMap, PredId, GeneratorPredIdPrime) ->
        GeneratorPredId = GeneratorPredIdPrime
    ;
        clone_pred_info(PredId, PredInfo0, HeadVars, NumberedOutputVars,
            GeneratorPredId, !TableInfo),
        map.det_insert(!.GenMap, PredId, GeneratorPredId, !:GenMap)
    ),

    % Even if the original goal doesn't use all of the headvars,
    % the code generated by the tabling transformation does,
    % so we need to compute the nonlocals from the headvars rather
    % than getting it from the nonlocals field in the original goal.
    set.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    goal_info_get_context(OrigGoalInfo, Context),

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
    GeneratorConsId = pred_const(ShroudedPredProcId, lambda_normal),
    make_const_construction(GeneratorPredVar, GeneratorConsId,
        MakeGeneratorVarGoal),

    generate_call_table_lookup_goals(NumberedInputVars, PredId, ProcId,
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
        yes(generator_pred_name - in_mode), GeneratorPredType,
        native_if_possible),
    ConsumerArg = foreign_arg(ConsumerVar,
        yes(ConsumerVarName - out_mode), consumer_type, native_if_possible),

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
            "\t\t" ++ generator_name ++ " = MR_tbl_mmos_setup_generator(" ++
                cur_table_node_name ++ ",\n\t\t\t"
                ++ int_to_string(NumInputVars) ++ ", "
                ++ GeneratorPredVarName ++ ", " ++
                """" ++ PredName ++ """);\n" ++
            "\t\tMR_mmos_new_generator = " ++ generator_name ++ ";\n" ++
        "\t}\n" ++
        "\t" ++ consumer_name ++ " = " ++
            "MR_tbl_mmos_setup_consumer(" ++ generator_name ++
            ", """ ++ PredName ++ """);\n",
    table_generate_foreign_proc(SetupPredName, detism_det,
        make_generator_c_attributes,
        [InfoArg, GeneratorPredArg, ConsumerArg], LookupForeignArgs,
        LookupDeclCodeStr ++ SetupCode, purity_impure,
        ground_vars([ConsumerVar]), ModuleInfo, Context, SetupGoal),
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
    ( Detism = detism_multi ->
        ConsumePredName = "table_mmos_consume_next_answer_multi"
    ; Detism = detism_non ->
        ConsumePredName = "table_mmos_consume_next_answer_nondet"
    ;
        unexpected(this_file, "do_own_stack_transform: invalid determinism")
    ),
    % XXX consider inlining the predicate being called
    table_generate_call(ConsumePredName, Detism, [ConsumerVar, AnswerBlockVar],
        purity_impure, ground_vars([AnswerBlockVar]), ModuleInfo, Context,
        GetNextAnswerGoal),
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, RestoreInstMapDeltaSrc,
        RestoreArgs, RestoreCodeStr),
    AnswerBlockArg = foreign_arg(AnswerBlockVar,
        yes(answer_block_name - in_mode), answer_block_type,
        native_if_possible),
    RestoreAllPredName = "table_mmos_restore_answers",
    table_generate_foreign_proc(RestoreAllPredName, detism_det,
        tabling_c_attributes, [AnswerBlockArg], RestoreArgs, RestoreCodeStr,
        purity_impure, RestoreInstMapDeltaSrc, ModuleInfo, Context,
        RestoreGoal),

    GoalExpr = conj(plain_conj,
        LookupSetupGoals ++ [GetNextAnswerGoal, RestoreGoal]),
    goal_info_init(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = GoalExpr - GoalInfo,

    module_info_pred_info(ModuleInfo, GeneratorPredId, GeneratorPredInfo),
    table_info_init(ModuleInfo, GeneratorPredInfo, ProcInfo0,
        GeneratorTableInfo0),
    do_own_stack_create_generator(GeneratorPredId, ProcId, GeneratorPredInfo,
        ProcInfo0, Statistics, Context, GeneratorPredVar,
        PickupInputVarCode, PickupForeignArgs,
        NumberedInputVars, NumberedOutputVars,
        OrigNonLocals, OrigInstMapDelta, !.VarTypes, !.VarSet,
        GeneratorTableInfo0, GeneratorTableInfo, OutputSteps),
    !:TableInfo = !.TableInfo ^ table_module_info :=
        GeneratorTableInfo ^ table_module_info.

:- pred generate_save_input_vars_code(assoc_list(foreign_arg, mer_mode)::in,
    module_info::in, int::in, list(foreign_arg)::out, string::out, string::out)
    is det.

generate_save_input_vars_code([], _, _, [], "", "").
generate_save_input_vars_code([InputArg - Mode | InputArgModes], ModuleInfo,
        Pos, [PickupArg | PickupArgs], SaveVarCode ++ SaveVarCodes,
        PickupVarCode ++ PickupVarCodes) :-
    InputArg = foreign_arg(InputVar, MaybeArgNameMode, Type, _),
    (
        MaybeArgNameMode = yes(InputVarName - _InMode)
    ;
        MaybeArgNameMode = no,
        unexpected(this_file, "generate_save_input_vars_code: no InputVarName")
    ),
    mode_get_insts(ModuleInfo, Mode, InitInst, _FinalInst),
    PickupMode = (free -> InitInst),
    PickupArg = foreign_arg(InputVar, yes(InputVarName - PickupMode), Type,
        native_if_possible),
    SaveVarCode = "\t\tMR_mmos_save_input_arg(" ++
        int_to_string(Pos) ++ ", " ++ InputVarName ++ ");\n",
    PickupVarCode = "\t\tMR_mmos_pickup_input_arg(" ++
        int_to_string(Pos) ++ ", " ++ InputVarName ++ ");\n",
    generate_save_input_vars_code(InputArgModes, ModuleInfo, Pos + 1,
        PickupArgs, SaveVarCodes, PickupVarCodes).

:- pred do_own_stack_create_generator(pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, table_attr_statistics::in, term.context::in,
    prog_var::in, string::in, list(foreign_arg)::in,
    list(var_mode_pos_method)::in, list(var_mode_pos_method)::in,
    set(prog_var)::in, instmap_delta::in,
    vartypes::in, prog_varset::in, table_info::in, table_info::out,
    list(table_trie_step)::out) is det.

do_own_stack_create_generator(PredId, ProcId, !.PredInfo, !.ProcInfo,
        Statistics, Context, GeneratorVar, PickupVarCode,
        PickupForeignArgs, NumberedInputVars, NumberedOutputVars,
        OrigNonLocals, OrigInstMapDelta, !.VarTypes, !.VarSet, !TableInfo,
        OutputSteps) :-
    ModuleInfo0 = !.TableInfo ^ table_module_info,

    proc_info_set_headvars(list.map(project_var, NumberedOutputVars),
        !ProcInfo),
    proc_info_set_argmodes(list.map(project_mode, NumberedOutputVars),
        !ProcInfo),
    PickupInstMapDeltaSrc0 = list.map(project_var_init_inst(ModuleInfo0),
        NumberedInputVars),
    PickupInstMapDeltaSrc = [pair_with_ground(GeneratorVar)
        | PickupInstMapDeltaSrc0],
    PickupGeneratorCode = "\t\t" ++ generator_name ++
        " = MR_mmos_new_generator;\n",
    PickupGeneratorArg = foreign_arg(GeneratorVar,
        yes(generator_name - out_mode), generator_type, native_if_possible),
    table_generate_foreign_proc("table_mmos_pickup_inputs", detism_det,
        tabling_c_attributes, [PickupGeneratorArg], PickupForeignArgs,
        PickupGeneratorCode ++ PickupVarCode, purity_semipure,
        PickupInstMapDeltaSrc, ModuleInfo0, Context, PickupGoal),

    list.length(NumberedOutputVars, BlockSize),
    generate_own_stack_save_goal(NumberedOutputVars, GeneratorVar,
        PredId, ProcId, BlockSize, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, OutputSteps, SaveAnswerGoals),

    proc_info_get_goal(!.ProcInfo, OrigGoal),
    GoalExpr = conj(plain_conj, [PickupGoal, OrigGoal | SaveAnswerGoals]),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_determinism(OrigGoalInfo, Detism),
    set.insert(OrigNonLocals, GeneratorVar, NonLocals),
    goal_info_init(NonLocals, OrigInstMapDelta, Detism, purity_impure, Context,
        GoalInfo0),
    goal_info_add_feature(feature_hide_debug_event, GoalInfo0, GoalInfo),
    Goal = GoalExpr - GoalInfo,
    proc_info_set_goal(Goal, !ProcInfo),

    proc_info_set_vartypes(!.VarTypes, !ProcInfo),
    proc_info_set_varset(!.VarSet, !ProcInfo),
    pred_info_get_procedures(!.PredInfo, ProcTable0),
    map.det_insert(ProcTable0, ProcId, !.ProcInfo, ProcTable),
    pred_info_set_procedures(ProcTable, !PredInfo),

    module_info_preds(ModuleInfo0, PredTable0),
    map.det_update(PredTable0, PredId, !.PredInfo, PredTable),
    module_info_set_preds(PredTable, ModuleInfo0, ModuleInfo),
    !:TableInfo = !.TableInfo ^ table_module_info := ModuleInfo.

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
    pred_info_context(PredInfo0, Context),
    % The generator is local even if the original predicate is exported.
    Status = status_local,
    pred_info_get_goal_type(PredInfo0, GoalType),
    pred_info_get_markers(PredInfo0, Markers0),
    pred_info_get_arg_types(PredInfo0, ArgTypes0),
    pred_info_get_typevarset(PredInfo0, TypeVarSet),
    pred_info_get_exist_quant_tvars(PredInfo0, ExistQVars),
    pred_info_get_class_context(PredInfo0, ClassContext),
    pred_info_get_constraint_proofs(PredInfo0, ClassProofs),
    pred_info_get_constraint_map(PredInfo0, ClassConstraintMap),
    pred_info_get_origin(PredInfo0, OrigOrigin),
    pred_info_clauses_info(PredInfo0, ClausesInfo),

    PredName = qualified(ModuleName, "GeneratorFor_" ++ PredName0),
    assoc_list.from_corresponding_lists(HeadVars, ArgTypes0, HeadVarTypes),
    keep_only_output_arg_types(HeadVarTypes, NumberedOutputVars, ArgTypes),
    Arity = list.length(ArgTypes),

    markers_to_marker_list(Markers0, MarkerList0),
    list.filter(filter_marker, MarkerList0, MarkerList),
    marker_list_to_markers(MarkerList, Markers),

    Origin = origin_transformed(transform_table_generator,
        OrigOrigin, OrigPredId),
    pred_info_init(ModuleName, PredName, Arity, PredOrFunc, Context,
        Origin, Status, GoalType, Markers, ArgTypes, TypeVarSet,
        ExistQVars, ClassContext, ClassProofs, ClassConstraintMap,
        ClausesInfo, PredInfo),

    ModuleInfo0 = !.TableInfo ^ table_module_info,
    module_info_get_predicate_table(ModuleInfo0, PredTable0),
    predicate_table_insert(PredInfo, GeneratorPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, ModuleInfo0, ModuleInfo),
    !:TableInfo = !.TableInfo ^ table_module_info := ModuleInfo.

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
    proc_info_create(ProcContext, ProcVarSet, ProcVarTypes, ProcHeadVars,
        ProcInstVarSet, ProcHeadModes, ProcDetism, ProcGoal, ProcRttiVarMaps,
        address_is_not_taken, NewProcInfo),
    ModuleName = pred_info_module(PredInfo),
    OrigPredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_context(PredInfo, PredContext),
    NewPredName = qualified(ModuleName, "OutlinedForIOTablingFrom_" ++
        OrigPredName),
    pred_info_get_arg_types(PredInfo, PredArgTypes),
    pred_info_get_typevarset(PredInfo, PredTypeVarSet),
    pred_info_get_exist_quant_tvars(PredInfo, PredExistQVars),
    pred_info_get_class_context(PredInfo, PredClassContext),
    pred_info_get_assertions(PredInfo, PredAssertions),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_create(ModuleName, NewPredName, PredOrFunc, PredContext,
        origin_created(io_tabling), status_local, Markers, PredArgTypes,
        PredTypeVarSet, PredExistQVars, PredClassContext, PredAssertions,
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
    ( Var = OutVar ->
        keep_only_output_arg_types(VarTypes, Outs, OutTypesTail),
        OutTypes = [Type | OutTypesTail]
    ;
        keep_only_output_arg_types(VarTypes, [Out | Outs], OutTypes)
    ).

:- pred filter_marker(marker::in) is semidet.

filter_marker(Marker) :-
    keep_marker(Marker) = yes.

:- func keep_marker(marker) = bool.

keep_marker(marker_stub) = no.
keep_marker(marker_infer_type) = no.
keep_marker(marker_infer_modes) = no.
keep_marker(marker_obsolete) = no.
keep_marker(marker_user_marked_inline) = no.
keep_marker(marker_user_marked_no_inline) = no.
keep_marker(marker_heuristic_inline) = no.
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
keep_marker(marker_may_have_parallel_conj) = yes.
keep_marker(marker_mutable_access_pred) = yes.

%-----------------------------------------------------------------------------%

:- pred generate_gen_proc_table_info(table_info::in,
    list(table_trie_step)::in, maybe(list(table_trie_step))::in,
    list(var_mode_method)::in, list(var_mode_method)::in,
    proc_table_info::out) is det.

generate_gen_proc_table_info(TableInfo, InputSteps, MaybeOutputSteps,
        InputVars, OutputVars, ProcTableInfo) :-
    ProcInfo = TableInfo ^ table_cur_proc_info,
    InOutHeadVars = InputVars ++ OutputVars,
    allocate_slot_numbers(InOutHeadVars, 1, NumberedInOutHeadVars),
    ArgInfos = list.map(project_var_pos, NumberedInOutHeadVars),
    continuation_info.generate_table_arg_type_info(ProcInfo, ArgInfos,
        TableArgTypeInfo),
    NumInputs = list.length(InputVars),
    NumOutputs = list.length(OutputVars),
    ProcTableInfo = table_gen_info(NumInputs, NumOutputs, InputSteps,
        MaybeOutputSteps, TableArgTypeInfo).

%-----------------------------------------------------------------------------%

    % Generate a goal for doing lookups in call tables for
    % loopcheck and memo predicates.
    %
:- pred generate_simple_call_table_lookup_goal(mer_type::in,
    string::in, string::in, list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, prog_var::out,
    hlds_goal::out, list(table_trie_step)::out) is det.

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
        yes(StatusVarName - out_mode), StatusType, native_if_possible),
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
    table_generate_foreign_proc(PredName, detism_det, tabling_c_attributes,
        Args, LookupForeignArgs, CodeStr, purity_impure,
        ground_vars(BoundVars), ModuleInfo, Context, SetupGoal0),
    attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = LookupPrefixGoals ++ [SetupGoal],

    GoalExpr = conj(plain_conj, LookupSetupGoals),
    Vars = list.map(project_var, NumberedVars),
    set.list_to_set([StatusVar, TableTipVar | Vars], NonLocals),
    goal_info_init_hide(NonLocals, bind_vars([TableTipVar, StatusVar]),
        detism_det, purity_impure, Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

    % Generate a goal for doing lookups in call tables for
    % model_non memo predicates.
    %
:- pred generate_memo_non_call_table_lookup_goal(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, prog_var::out,
    hlds_goal::out, list(table_trie_step)::out) is det.

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
        yes(RecordVarName - out_mode), memo_non_record_type,
        native_if_possible),
    StatusArg = foreign_arg(StatusVar,
        yes(StatusVarName - out_mode), memo_non_status_type,
        native_if_possible),
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
        tabling_c_attributes, Args, LookupForeignArgs,
        LookupDeclCodeStr ++ PredCodeStr, purity_impure,
        ground_vars(BoundVars), ModuleInfo, Context, SetupGoal0),
    attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = LookupPrefixGoals ++ [SetupGoal],

    GoalExpr = conj(plain_conj, LookupSetupGoals),
    Vars = list.map(project_var, NumberedVars),
    set.list_to_set([StatusVar, RecordVar | Vars], NonLocals),
    goal_info_init_hide(NonLocals, bind_vars([RecordVar, StatusVar]),
        detism_det, purity_impure, Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

    % Generate a goal for doing lookups in call tables for
    % minimal model predicates.
    %
:- pred generate_mm_call_table_lookup_goal(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out, prog_var::out,
    hlds_goal::out, list(table_trie_step)::out) is det.

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
        yes(SubgoalVarName - out_mode), subgoal_type, native_if_possible),
    StatusArg = foreign_arg(StatusVar,
        yes(StatusVarName - out_mode), mm_status_type, native_if_possible),
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
        tabling_c_attributes, Args, LookupForeignArgs, CodeStr, purity_impure,
        ground_vars(BoundVars), ModuleInfo, Context, SetupGoal0),
    attach_call_table_tip(SetupGoal0, SetupGoal),
    LookupSetupGoals = LookupPrefixGoals ++ [SetupGoal],

    GoalExpr = conj(plain_conj, LookupSetupGoals),
    Vars = list.map(project_var, NumberedVars),
    set.list_to_set([StatusVar, SubgoalVar | Vars], NonLocals),
    goal_info_init_hide(NonLocals, bind_vars([SubgoalVar, StatusVar]),
        detism_det, purity_impure, Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

% Utility predicates used when creating table lookup goals.

:- pred generate_call_table_lookup_goals(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_trie_step)::out,
    prog_var::out, foreign_arg::out, foreign_arg::out, list(foreign_arg)::out,
    list(hlds_goal)::out, string::out, string::out) is det.

generate_call_table_lookup_goals(NumberedVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, InputSteps,
        CallTableTipVar, CallTableTipArg, InfoArg, LookupArgs,
        PrefixGoals, MainCodeStr, CallTableTipVarCodeStr) :-
    InfoToPtrCodeStr = "\t" ++ cur_table_node_name ++ " = " ++
        "&" ++ proc_table_info_name ++ "->MR_pt_tablenode;\n",
    generate_get_table_info_goal(PredId, ProcId, !VarSet, !VarTypes,
        proc_table_info_name, InfoArg, GetTableInfoGoal),
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = get_back_arg_string(!.TableInfo),
    generate_table_lookup_goals(NumberedVars, Statistics, call_table,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        InputSteps, LookupArgs, LookupPrefixGoals, LookupCodeStr),
    PrefixGoals = [GetTableInfoGoal] ++ LookupPrefixGoals,
    % We ignore _StatsPrefixGoals and _StatsExtraArgs because we always
    % include ProcTableInfoVar in the arguments.
    maybe_lookup_not_dupl_code_args(PredId, ProcId,
        proc_table_info_name, cur_table_node_name,
        Statistics, call_table, !VarSet, !VarTypes,
        _StatsPrefixGoals, _StatsExtraArgs, StatsCodeStr),
    MainCodeStr = InfoToPtrCodeStr ++ LookupCodeStr ++ StatsCodeStr,
    CallTableTipVarName = "CallTableTipVar",
    generate_new_table_var(CallTableTipVarName, trie_node_type,
        !VarSet, !VarTypes, CallTableTipVar),
    CallTableTipArg = foreign_arg(CallTableTipVar,
        yes(CallTableTipVarName - out_mode), trie_node_type,
        native_if_possible),
    CallTableTipVarCodeStr =
        "\t" ++ CallTableTipVarName ++ " = " ++ cur_table_node_name ++ ";\n".

:- pred generate_answer_table_lookup_goals(list(var_mode_pos_method)::in,
    pred_id::in, proc_id::in, table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_trie_step)::out,
    list(foreign_arg)::out, list(hlds_goal)::out, string::out) is det.

generate_answer_table_lookup_goals(NumberedVars, PredId, ProcId, Statistics,
        Context, !VarSet, !VarTypes, !TableInfo, OutputSteps, ForeignArgs,
        PrefixGoals, CodeStr) :-
    DebugArgStr = get_debug_arg_string(!.TableInfo),
    BackArgStr = "MR_FALSE",
    generate_table_lookup_goals(NumberedVars, Statistics, answer_table,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        OutputSteps, LookupArgs, LookupPrefixGoals, LookupCodeStr),
    maybe_lookup_not_dupl_code_args(PredId, ProcId,
        proc_table_info_name, cur_table_node_name,
        Statistics, call_table, !VarSet, !VarTypes,
        StatsPrefixGoals, StatsExtraArgs, StatsCodeStr),
    CodeStr = LookupCodeStr ++ StatsCodeStr,
    ForeignArgs = StatsExtraArgs ++ LookupArgs,
    PrefixGoals = StatsPrefixGoals ++ LookupPrefixGoals.

:- pred maybe_lookup_not_dupl_code_args(pred_id::in, proc_id::in,
    string::in, string::in, table_attr_statistics::in,
    call_or_answer_table::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    list(hlds_goal)::out, list(foreign_arg)::out, string::out) is det.

maybe_lookup_not_dupl_code_args(PredId, ProcId, InfoVarName, TipVarName,
        Statistics, Kind, !VarSet, !VarTypes, PrefixGoals, Args, CodeStr) :-
    (
        Statistics = table_dont_gather_statistics,
        PrefixGoals = [],
        Args = [],
        CodeStr = ""
    ;
        Statistics = table_gather_statistics,
        generate_get_table_info_goal(PredId, ProcId, !VarSet, !VarTypes,
            InfoVarName, Arg, Goal),
        PrefixGoals = [Goal],
        Args = [Arg],
        CodeStr = lookup_not_dupl_code(InfoVarName, TipVarName, Kind)
    ).

:- func lookup_not_dupl_code(string, string, call_or_answer_table) = string.

lookup_not_dupl_code(InfoVar, TipVar, call_table) =
    "\t" ++ InfoVar ++ "->MR_pt_call_table_lookups++;\n" ++
    "\tif (" ++ TipVar ++ "->MR_integer != 0) {\n" ++
    "\t\t" ++ InfoVar ++ "->MR_pt_call_table_not_dupl++;\n" ++
    "\t}\n".
lookup_not_dupl_code(InfoVar, TipVar, answer_table) =
    "\t" ++ InfoVar ++ "->MR_pt_answer_table_lookups++;\n" ++
    "\tif (" ++ TipVar ++ "->MR_integer != 0) {\n" ++
    "\t\t" ++ InfoVar ++ "->MR_pt_answer_table_not_dupl++;\n" ++
    "\t}\n".

:- pred generate_get_table_info_goal(pred_id::in, proc_id::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    string::in, foreign_arg::out, hlds_goal::out) is det.

generate_get_table_info_goal(PredId, ProcId, !VarSet, !VarTypes,
        InfoVarName, Arg, Goal) :-
    generate_new_table_var("ProcTableInfo", proc_table_info_type,
        !VarSet, !VarTypes, ProcTableInfoVar),
    Arg = foreign_arg(ProcTableInfoVar,
        yes(InfoVarName - in_mode), proc_table_info_type,
        native_if_possible),
    ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
    InfoConsId = tabling_info_const(ShroudedPredProcId),
    make_const_construction(ProcTableInfoVar, InfoConsId,
        GoalExpr - GoalInfo0),
    goal_info_set_purity(purity_impure, GoalInfo0, GoalInfo),
    Goal = GoalExpr - GoalInfo.

:- pred attach_call_table_tip(hlds_goal::in, hlds_goal::out) is det.

attach_call_table_tip(GoalExpr - GoalInfo0, GoalExpr - GoalInfo) :-
    goal_info_get_features(GoalInfo0, Features0),
    set.insert(Features0, feature_call_table_gen, Features),
    goal_info_set_features(Features, GoalInfo0, GoalInfo).

%-----------------------------------------------------------------------------%

    % Generate a sequence of lookup goals for the given variables.
    % The generated code is used for lookups in both call tables
    % and answer tables.
    %
:- pred generate_table_lookup_goals(list(var_mode_pos_method)::in,
    table_attr_statistics::in, call_or_answer_table::in,
    string::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_trie_step)::out,
    list(foreign_arg)::out, list(hlds_goal)::out, string::out) is det.

generate_table_lookup_goals([], _, _, _, _, _, !VarSet, !VarTypes, !TableInfo,
        [], [], [], "").
generate_table_lookup_goals([VarModePos | NumberedVars], Statistics,
        Kind, DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        [Step | Steps], ForeignArgs ++ RestForeignArgs,
        PrefixGoals ++ RestPrefixGoals, CodeStr ++ RestCodeStr) :-
    VarModePos = var_mode_pos_method(Var, _, VarSeqNum, ArgMethod),
    ModuleInfo = !.TableInfo ^ table_module_info,
    map.lookup(!.VarTypes, Var, VarType),
    classify_type(ModuleInfo, VarType) = TypeCat,
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
        gen_lookup_call_for_type(ArgMethod, TypeCat, VarType, Var,
            VarSeqNum, Statistics, Kind, DebugArgStr, BackArgStr, Context,
            !VarSet, !VarTypes, !TableInfo, Step, ForeignArgs,
            PrefixGoals, CodeStr)
    ),
    generate_table_lookup_goals(NumberedVars, Statistics, Kind,
        DebugArgStr, BackArgStr, Context, !VarSet, !VarTypes, !TableInfo,
        Steps, RestForeignArgs, RestPrefixGoals, RestCodeStr).

:- pred gen_lookup_call_for_type(arg_tabling_method::in, type_category::in,
    mer_type::in, prog_var::in, int::in, table_attr_statistics::in,
    call_or_answer_table::in, string::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out,
    table_trie_step::out, list(foreign_arg)::out, list(hlds_goal)::out,
    string::out) is det.

gen_lookup_call_for_type(ArgTablingMethod, TypeCat, Type, ArgVar,
        VarSeqNum, Statistics, Kind, DebugArgStr, BackArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Step, ExtraArgs, PrefixGoals,
        CodeStr) :-
    ModuleInfo = !.TableInfo ^ table_module_info,
    ArgName = arg_name(VarSeqNum),
    ForeignArg = foreign_arg(ArgVar, yes(ArgName - in_mode), Type,
        native_if_possible),
    ( TypeCat = type_cat_enum ->
        ( type_to_ctor_and_args(Type, TypeCtor, _) ->
            module_info_get_type_table(ModuleInfo, TypeDefnTable),
            map.lookup(TypeDefnTable, TypeCtor, TypeDefn),
            hlds_data.get_type_defn_body(TypeDefn, TypeBody),
            (
                Ctors = TypeBody ^ du_type_ctors,
                TypeBody ^ du_type_is_enum = is_enum,
                TypeBody ^ du_type_usereq  = no
            ->
                list.length(Ctors, EnumRange)
            ;
                unexpected(this_file,
                    "gen_lookup_call_for_type: enum type is not du_type?")
            ),
            LookupMacroName = "MR_tbl_lookup_insert_enum",
            Step = table_trie_step_enum(EnumRange),
            PrefixGoals = [],
            ExtraArgs = [ForeignArg],
            StatsArgStr = stats_arg(Statistics, Kind, VarSeqNum),
            CodeStr0 = "\t" ++ LookupMacroName ++ "(" ++ StatsArgStr ++ ", " ++
                DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
                cur_table_node_name ++ ", " ++ int_to_string(EnumRange) ++
                ", " ++ ArgName ++ ", " ++ next_table_node_name ++ ");\n"
        ;
            unexpected(this_file,
                "gen_lookup_call_for_type: unexpected enum type")
        )
    ; TypeCat = type_cat_dummy ->
        Step = table_trie_step_dummy,
        PrefixGoals = [],
        ExtraArgs = [],
        CodeStr0 = next_table_node_name ++ " = "
            ++ cur_table_node_name ++ ";\n "
    ;
        lookup_tabling_category(TypeCat, MaybeCatStringStep),
        (
            MaybeCatStringStep = no,
            type_vars(Type, TypeVars),
            (
                ArgTablingMethod = arg_value,
                (
                    TypeVars = [],
                    LookupMacroName = "MR_tbl_lookup_insert_user",
                    Step = table_trie_step_user(Type)
                ;
                    TypeVars = [_ | _],
                    LookupMacroName = "MR_tbl_lookup_insert_poly",
                    Step = table_trie_step_poly
                )
            ;
                ArgTablingMethod = arg_addr,
                (
                    TypeVars = [],
                    LookupMacroName = "MR_tbl_lookup_insert_user_addr",
                    Step = table_trie_step_user_fast_loose(Type)
                ;
                    TypeVars = [_ | _],
                    LookupMacroName = "MR_tbl_lookup_insert_poly_addr",
                    Step = table_trie_step_poly_fast_loose
                )
            ;
                ArgTablingMethod = arg_promise_implied,
                unexpected(this_file,
                    "gen_lookup_call_for_type: arg_promise_implied")
            ),
            table_gen_make_type_info_var(Type, Context, !VarSet, !VarTypes,
                !TableInfo, TypeInfoVar, PrefixGoals),
            TypeInfoArgName = "input_typeinfo" ++ int_to_string(VarSeqNum),
            map.lookup(!.VarTypes, TypeInfoVar, TypeInfoType),
            ForeignTypeInfoArg = foreign_arg(TypeInfoVar,
                yes(TypeInfoArgName - in_mode), TypeInfoType,
                native_if_possible),
            ExtraArgs = [ForeignTypeInfoArg, ForeignArg],
            StatsArgStr = stats_arg(Statistics, Kind, VarSeqNum),
            CodeStr0 = "\t" ++ LookupMacroName ++ "(" ++ StatsArgStr ++ ", " ++
                DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
                cur_table_node_name ++ ", " ++ TypeInfoArgName ++ ", " ++
                ArgName ++ ", " ++ next_table_node_name ++ ");\n"
        ;
            MaybeCatStringStep = yes(CatString - Step),
            LookupMacroName = "MR_tbl_lookup_insert_" ++ CatString,
            PrefixGoals = [],
            ExtraArgs = [ForeignArg],
            StatsArgStr = stats_arg(Statistics, Kind, VarSeqNum),
            CodeStr0 = "\t" ++ LookupMacroName ++ "(" ++ StatsArgStr ++ ", " ++
                DebugArgStr ++ ", " ++ BackArgStr ++ ", " ++
                cur_table_node_name ++ ", " ++ ArgName ++ ", " ++
                next_table_node_name ++ ");\n"
        )
    ),
    CodeStr = CodeStr0 ++ "\t" ++ cur_table_node_name ++ " = " ++
        next_table_node_name ++ ";\n".

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
    TableArg = foreign_arg(TableTipVar, yes(TipVarName - in_mode),
        trie_node_type, native_if_possible),
    ( BlockSize > 0 ->
        CreatePredName = "table_memo_fill_answer_block_shortcut",
        CreateMacroName = "MR_tbl_memo_create_answer_block",
        generate_all_save_goals(NumberedSaveVars, TipVarName,
            BlockSize, CreateMacroName, Context, !VarSet, !VarTypes,
            !TableInfo, SaveArgs, SavePrefixGoals, SaveDeclCode, SaveCode),
        table_generate_foreign_proc(CreatePredName, detism_det,
            tabling_c_attributes, [TableArg], SaveArgs,
            SaveDeclCode ++ SaveCode, purity_impure, [],
            ModuleInfo, Context, SaveGoal),
        Goals = SavePrefixGoals ++ [SaveGoal]
    ;
        DebugArgStr = get_debug_arg_string(!.TableInfo),
        MarkAsSucceededPredName = "table_memo_mark_as_succeeded",
        MarkAsSucceededMacroName = "MR_tbl_memo_mark_as_succeeded",
        MarkAsSucceededCode = MarkAsSucceededMacroName ++
            "(" ++ DebugArgStr ++ ", " ++ cur_table_node_name ++ ");",
        table_generate_foreign_proc(MarkAsSucceededPredName, detism_det,
            tabling_c_attributes, [TableArg], [],
            MarkAsSucceededCode, purity_impure, [],
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
    table_info::in, table_info::out, list(table_trie_step)::out,
    list(hlds_goal)::out) is det.

generate_memo_non_save_goals(NumberedSaveVars, PredId, ProcId,
        RecordVar, BlockSize, Statistics, Context, !VarSet, !VarTypes,
        !TableInfo, OutputSteps, Goals) :-
    ModuleInfo = !.TableInfo ^ table_module_info,
    RecordName = memo_non_record_name,
    RecordArg = foreign_arg(RecordVar,
        yes(RecordName - in_mode), memo_non_record_type, native_if_possible),

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
        tabling_c_attributes, [RecordArg], LookupForeignArgs, CodeStr,
        purity_impure, [], ModuleInfo, Context, DuplicateCheckSaveGoal),
    Goals = LookupPrefixGoals ++ [DuplicateCheckSaveGoal].

    % Generate a goal for saving the output arguments in an answer block
    % in minimal model predicates.
    %
:- pred generate_mm_save_goals(list(var_mode_pos_method)::in,
    prog_var::in, pred_id::in, proc_id::in, int::in,
    table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_trie_step)::out,
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
    Args = [foreign_arg(SubgoalVar, yes(SubgoalName - in_mode),
        subgoal_type, native_if_possible)],
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
        tabling_c_attributes, Args, LookupForeignArgs,
        CodeStr, purity_impure, [],
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
:- pred generate_own_stack_save_goal(list(var_mode_pos_method)::in,
    prog_var::in, pred_id::in, proc_id::in, int::in,
    table_attr_statistics::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(table_trie_step)::out,
    list(hlds_goal)::out) is det.

generate_own_stack_save_goal(NumberedOutputVars, GeneratorVar, PredId, ProcId,
        BlockSize, Statistics, Context, !VarSet, !VarTypes, !TableInfo,
        OutputSteps, Goals) :-
    GeneratorName = generator_name,
    GeneratorArg = foreign_arg(GeneratorVar, yes(GeneratorName - in_mode),
        generator_type, native_if_possible),
    DebugArgStr = get_debug_arg_string(!.TableInfo),

    generate_answer_table_lookup_goals(NumberedOutputVars, PredId, ProcId,
        Statistics, Context, !VarSet, !VarTypes, !TableInfo, OutputSteps,
        LookupForeignArgs, LookupPrefixGoals, LookupCodeStr),

    generate_save_goals(NumberedOutputVars, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, _SaveArgs,
        SavePrefixGoals, SaveCodeStr),

    GetMacroName = "MR_tbl_mmos_get_answer_table",
    CreateMacroName = "MR_tbl_mm_create_answer_block",
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
        GetMacroName ++ "(" ++ DebugArgStr ++ ", " ++
        GeneratorName ++ ");\n",
    DuplCheckCodeStr =
        "\t" ++ DuplCheckMacroName ++ "(" ++ cur_table_node_name ++ ", " ++
            SuccName ++ ");\n",
    AssignSuccessCodeStr =
        "\t" ++ success_indicator_name ++ " = " ++ SuccName ++ ";\n",
    CreateCodeStr = "\t" ++ CreateMacroName ++ "(" ++ DebugArgStr ++ ", " ++
        GeneratorName ++ ", " ++ int_to_string(BlockSize) ++ ", " ++
        answer_block_name ++ ");\n",
    CreateSaveCodeStr = CreateCodeStr ++ SaveCodeStr,
    CondSaveCodeStr = "\tif (" ++ SuccName ++ ") {\n" ++
        CreateSaveCodeStr ++ "\t}\n",
    CodeStr = LookupSaveDeclCodeStr ++ GetCodeStr ++ LookupCodeStr ++
        DuplCheckCodeStr ++ CondSaveCodeStr ++ AssignSuccessCodeStr,
    ModuleInfo = !.TableInfo ^ table_module_info,
    table_generate_foreign_proc(DuplCheckPredName, detism_semi,
        tabling_c_attributes, Args, LookupForeignArgs,
        CodeStr, purity_impure, [],
        ModuleInfo, Context, DuplicateCheckSaveGoal),
    Goals = LookupPrefixGoals ++ SavePrefixGoals ++ [DuplicateCheckSaveGoal].

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
    map.lookup(!.VarTypes, Var, VarType),
    classify_type(ModuleInfo, VarType) = TypeCat,
    gen_save_call_for_type(TypeCat, VarType, Var, Offset, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Args, PrefixGoals, CodeStr),
    generate_save_goals(NumberedRest, DebugArgStr, Context, !VarSet, !VarTypes,
        !TableInfo, RestArgs, RestPrefixGoals, RestCodeStr).

:- pred gen_save_call_for_type(type_category::in, mer_type::in,
    prog_var::in, int::in, string::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(foreign_arg)::out,
    list(hlds_goal)::out, string::out) is det.

gen_save_call_for_type(TypeCat, Type, Var, Offset, DebugArgStr, Context,
        !VarSet, !VarTypes, !TableInfo, Args, PrefixGoals, CodeStr) :-
    Name = arg_name(Offset),
    ForeignArg = foreign_arg(Var, yes(Name - in_mode), Type,
        native_if_possible),
    ( type_is_io_state(Type) ->
        SaveMacroName = "MR_tbl_save_io_state_answer",
        Args = [ForeignArg],
        PrefixGoals = [],
        CodeStr = "\t" ++ SaveMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            answer_block_name ++ ", " ++ int_to_string(Offset) ++ ", "
            ++ Name ++ ");\n"
    ; builtin_type(TypeCat) = no ->
        % If we used ForeignArg instead of GenericForeignArg, then
        % Var would be unboxed when assigned to Name, which we don't want.
        GenericForeignArg = foreign_arg(Var, yes(Name - in_mode),
            dummy_type_var, native_if_possible),
        table_gen_make_type_info_var(Type, Context, !VarSet, !VarTypes,
            !TableInfo, TypeInfoVar, PrefixGoals),
        TypeInfoName = "save_arg_typeinfo" ++ int_to_string(Offset),
        map.lookup(!.VarTypes, TypeInfoVar, TypeInfoType),
        TypeInfoForeignArg = foreign_arg(TypeInfoVar,
            yes(TypeInfoName - in_mode), TypeInfoType, native_if_possible),
        SaveMacroName = "MR_tbl_save_any_answer",
        Args = [GenericForeignArg, TypeInfoForeignArg],
        CodeStr = "\t" ++ SaveMacroName ++ "(" ++ DebugArgStr ++ ", " ++
            answer_block_name ++ ", " ++ int_to_string(Offset) ++ ", " ++
            TypeInfoName ++ ", " ++ Name ++ ");\n"
    ;
        type_save_category(TypeCat, CatString),
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
        Arg = foreign_arg(TipVar, yes(BaseVarName - in_mode),
            trie_node_type, native_if_possible),
        Args = [Arg],
        GetPredName = "table_memo_get_answer_block_shortcut",
        GetMacroName = "MR_tbl_memo_get_answer_block",
        DeclCodeStr = "\tMR_AnswerBlock " ++ answer_block_name ++ ";\n",
        GetRestoreCodeStr = "\t" ++ GetMacroName ++ "(" ++
            DebugArgStr ++ ", " ++ BaseVarName ++ ", " ++
            answer_block_name ++ ");\n" ++
            RestoreCodeStr,
        table_generate_foreign_proc(GetPredName, detism_det,
            tabling_c_attributes, Args, RestoreArgs,
            DeclCodeStr ++ GetRestoreCodeStr, purity_semipure,
            RestoreInstMapDeltaSrc, ModuleInfo, Context, ShortcutGoal),
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
    ( Detism = detism_multi ->
        ReturnAllAns = "table_memo_return_all_answers_multi"
    ; Detism = detism_non ->
        ReturnAllAns = "table_memo_return_all_answers_nondet"
    ;
        unexpected(this_file, "generate_mm_restore_goal: invalid determinism")
    ),
    generate_new_table_var("AnswerBlock", answer_block_type,
        !VarSet, !VarTypes, AnswerBlockVar),
    ModuleInfo = TableInfo ^ table_module_info,
    table_generate_call(ReturnAllAns, Detism, [RecordVar, AnswerBlockVar],
        purity_semipure, ground_vars([AnswerBlockVar]), ModuleInfo,
        Context, ReturnAnswerBlocksGoal),
    DebugArgStr = get_debug_arg_string(TableInfo),
    generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, RestoreInstMapDeltaSrc,
        RestoreArgs, RestoreCodeStr),
    OutputVars = list.map(project_var, NumberedOutputVars),
    Arg = foreign_arg(AnswerBlockVar, yes(answer_block_name - in_mode),
        answer_block_type, native_if_possible),
    Args = [Arg],
    PredName = "table_memo_non_return_all_shortcut",
    table_generate_foreign_proc(PredName, detism_det, tabling_c_attributes,
        Args, RestoreArgs, RestoreCodeStr, purity_semipure,
        RestoreInstMapDeltaSrc, ModuleInfo, Context, ShortcutGoal),

    GoalExpr = conj(plain_conj, [ReturnAnswerBlocksGoal, ShortcutGoal]),
    set.list_to_set([RecordVar | OutputVars], NonLocals),
    goal_info_init_hide(NonLocals, OrigInstMapDelta, Detism, purity_semipure,
        Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

    % Generate a goal for restoring the output arguments from
    % an answer block in minimal model predicates without a suspension.
    %
:- pred generate_mm_restore_goal(determinism::in,
    list(var_mode_pos_method)::in, instmap_delta::in, prog_var::in,
    term.context::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, table_info::in, hlds_goal::out) is det.

generate_mm_restore_goal(Detism, NumberedOutputVars, OrigInstMapDelta,
        SubgoalVar, Context, !VarSet, !VarTypes, TableInfo, Goal) :-
    ( Detism = detism_multi ->
        ReturnAllAns = "table_mm_return_all_multi"
    ; Detism = detism_non ->
        ReturnAllAns = "table_mm_return_all_nondet"
    ;
        unexpected(this_file, "generate_mm_restore_goal: invalid determinism")
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
        Purity, ground_vars([AnswerBlockVar]), ModuleInfo,
        Context, ReturnAnswerBlocksGoal),
    DebugArgStr = get_debug_arg_string(TableInfo),
    generate_restore_goals(NumberedOutputVars, OrigInstMapDelta,
        DebugArgStr, ModuleInfo, !VarSet, !VarTypes, RestoreInstMapDeltaSrc,
        RestoreArgs, RestoreCodeStr),
    OutputVars = list.map(project_var, NumberedOutputVars),

    Arg = foreign_arg(AnswerBlockVar, yes(answer_block_name - in_mode),
        answer_block_type, native_if_possible),
    Args = [Arg],
    ReturnAllPredName = "table_mm_return_all_shortcut",
    table_generate_foreign_proc(ReturnAllPredName, detism_det,
        tabling_c_attributes, Args, RestoreArgs, RestoreCodeStr,
        purity_semipure, RestoreInstMapDeltaSrc, ModuleInfo, Context,
        ReturnAllGoal),
    GoalExpr = conj(plain_conj, [ReturnAnswerBlocksGoal, ReturnAllGoal]),

    set.list_to_set([SubgoalVar | OutputVars], NonLocals),
    goal_info_init_hide(NonLocals, OrigInstMapDelta, Detism, Purity,
        Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

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
    map.lookup(!.VarTypes, Var, VarType),
    classify_type(ModuleInfo, VarType) = TypeCat,
    gen_restore_call_for_type(DebugArgStr, TypeCat, VarType, OrigInstmapDelta,
        Var, Offset, VarInst, Arg, CodeStr),
    generate_restore_goals(NumberedRest, OrigInstmapDelta, DebugArgStr,
        ModuleInfo, !VarSet, !VarTypes, VarInsts, Args, RestCodeStr).

:- pred gen_restore_call_for_type(string::in, type_category::in, mer_type::in,
    instmap_delta::in, prog_var::in, int::in,
    pair(prog_var, mer_inst)::out, foreign_arg::out, string::out) is det.

gen_restore_call_for_type(DebugArgStr, TypeCat, Type, OrigInstmapDelta, Var,
        Offset, Var - Inst, Arg, CodeStr) :-
    Name = "restore_arg" ++ int_to_string(Offset),
    ( type_is_io_state(Type) ->
        RestoreMacroName = "MR_tbl_restore_io_state_answer",
        ArgType = Type
    ; builtin_type(TypeCat) = no ->
        RestoreMacroName = "MR_tbl_restore_any_answer",
        ArgType = dummy_type_var
    ;
        type_save_category(TypeCat, CatString),
        RestoreMacroName = "MR_tbl_restore_" ++ CatString ++ "_answer",
        ArgType = Type
    ),
    ( instmap_delta_search_var(OrigInstmapDelta, Var, InstPrime) ->
        Inst = InstPrime
    ;
        unexpected(this_file, "gen_restore_call_for_type: no inst")
    ),
    Arg = foreign_arg(Var, yes(Name - (free -> Inst)), ArgType,
        native_if_possible),
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

    gen_string_construction("Message", Message, !VarSet, !VarTypes,
        MessageVar, MessageStrGoal),
    table_generate_call("table_error", detism_erroneous, [MessageVar],
        purity_pure, [], ModuleInfo, Context, CallGoal),

    GoalExpr = conj(plain_conj, [MessageStrGoal, CallGoal]),
    goal_info_init_hide(set.init, bind_vars([]), detism_erroneous,
        purity_impure, Context, GoalInfo),
    Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

:- pred generate_new_table_var(string::in, mer_type::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var::out) is det.

generate_new_table_var(Name, Type, !VarSet, !VarTypes, Var) :-
    varset.new_named_var(!.VarSet, Name, Var, !:VarSet),
    map.set(!.VarTypes, Var, Type, !:VarTypes).

:- pred table_generate_call(string::in, determinism::in, list(prog_var)::in,
    purity::in, assoc_list(prog_var, mer_inst)::in,
    module_info::in, term.context::in, hlds_goal::out) is det.

table_generate_call(PredName, Detism, Args, Purity, InstMapSrc, ModuleInfo,
        Context, Goal) :-
    BuiltinModule = mercury_table_builtin_module,
    ( Purity = purity_pure ->
        Features0 = []
    ;
        Features0 = [feature_not_impure_for_determinism]
    ),
    ( Detism = detism_failure ->
        Features = [feature_preserve_backtrack_into | Features0]
    ;
        Features = Features0
    ),
    goal_util.generate_simple_call(BuiltinModule, PredName, predicate,
        only_mode, Detism, Purity, Args, Features, InstMapSrc, ModuleInfo,
        Context, Goal).

:- pred table_generate_foreign_proc(string::in, determinism::in,
    pragma_foreign_proc_attributes::in,
    list(foreign_arg)::in, list(foreign_arg)::in, string::in,
    purity::in, assoc_list(prog_var, mer_inst)::in,
    module_info::in, term.context::in, hlds_goal::out) is det.

table_generate_foreign_proc(PredName, Detism, Attributes, Args, ExtraArgs,
        Code, Purity, InstMapSrc, ModuleInfo, Context, Goal) :-
    ( Purity = purity_pure ->
        Features0 = []
    ;
        Features0 = [feature_not_impure_for_determinism]
    ),
    ( Detism = detism_failure ->
        Features = [feature_preserve_backtrack_into | Features0]
    ;
        Features = Features0
    ),
    BuiltinModule = mercury_table_builtin_module,
    MaybeTraceRuntimCond = no,
    goal_util.generate_foreign_proc(BuiltinModule, PredName, predicate,
        only_mode, Detism, Purity, Attributes, Args, ExtraArgs,
        MaybeTraceRuntimCond, Code, Features, InstMapSrc, ModuleInfo,
        Context, Goal).

:- pred append_fail(hlds_goal::in, hlds_goal::out) is det.

append_fail(Goal, GoalAndThenFail) :-
    Goal = _ - GoalInfo,
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    goal_info_get_context(GoalInfo, Context),
    instmap_delta_init_unreachable(UnreachInstMapDelta),
    goal_info_init_hide(NonLocals, UnreachInstMapDelta, detism_failure,
        purity_impure, Context, ConjGoalInfo),
    GoalAndThenFail = conj(plain_conj, [Goal, fail_goal]) - ConjGoalInfo.

:- pred gen_int_construction(string::in, int::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var::out, hlds_goal::out) is det.

gen_int_construction(VarName, VarValue, !VarSet, !VarTypes, Var, Goal) :-
    make_int_const_construction_alloc(VarValue, yes(VarName), Goal, Var,
        !VarSet, !VarTypes).

:- pred gen_string_construction(string::in, string::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    prog_var::out, hlds_goal::out) is det.

gen_string_construction(VarName, VarValue, !VarSet, !VarTypes, Var, Goal) :-
    make_string_const_construction_alloc(VarValue, yes(VarName), Goal, Var,
        !VarSet, !VarTypes).

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

:- func consumer_type = mer_type.

consumer_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_consumer"), 0), [], Type).

:- func generator_type = mer_type.

generator_type = Type :-
    TB = mercury_table_builtin_module,
    construct_type(type_ctor(qualified(TB, "ml_generator"), 0), [], Type).

:- type maybe_specified_method
    --->    all_same(arg_tabling_method)
    ;       specified(list(maybe(arg_tabling_method))).

:- pred get_input_output_vars(list(prog_var)::in, list(mer_mode)::in,
    module_info::in, maybe_specified_method::in, maybe_specified_method::out,
    list(var_mode_method)::out, list(var_mode_method)::out) is det.

get_input_output_vars([], [], _, !MaybeSpecMethod, [], []).
get_input_output_vars([_ | _], [], _, !MaybeSpecMethod, _, _) :-
    unexpected(this_file, "get_input_output_vars: lists not same length").
get_input_output_vars([], [_ | _], _, !MaybeSpecMethod, _, _) :-
    unexpected(this_file, "get_input_output_vars: lists not same length").
get_input_output_vars([Var | Vars], [Mode | Modes], ModuleInfo,
        !MaybeSpecMethod, InVarModes, OutVarModes) :-
    ( mode_is_fully_input(ModuleInfo, Mode) ->
        get_input_output_vars(Vars, Modes, ModuleInfo, !MaybeSpecMethod,
            InVarModes0, OutVarModes),
        (
            !.MaybeSpecMethod = all_same(ArgMethod)
        ;
            !.MaybeSpecMethod = specified(MaybeArgMethods0),
            (
                list.split_last(MaybeArgMethods0, MaybeArgMethods,
                    LastMaybeArgMethod)
            ->
                (
                    LastMaybeArgMethod = yes(ArgMethod)
                ;
                    LastMaybeArgMethod = no,
                    unexpected(this_file,
                        "get_input_output_vars: bad method for input var")
                ),
                !:MaybeSpecMethod = specified(MaybeArgMethods)
            ;
                % We have run out of specified arg_methods, which means the
                % variable we are looking at right now is one that was added
                % by the polymorphism transformation.
                ArgMethod = arg_value,
                !:MaybeSpecMethod = all_same(arg_value)
            )
        ),
        InVarModes = [var_mode_method(Var, Mode, ArgMethod) | InVarModes0]
    ; mode_is_fully_output(ModuleInfo, Mode) ->
        get_input_output_vars(Vars, Modes, ModuleInfo, !MaybeSpecMethod,
            InVarModes, OutVarModes0),
        (
            !.MaybeSpecMethod = all_same(_ArgMethod)
            % The tabling methods that use answer tables always use arg_value
            % to look up computed output arguments in them. The argument of
            % all_same refers only to the treatment of input arguments.
        ;
            !.MaybeSpecMethod = specified(MaybeArgMethods0),
            (
                list.split_last(MaybeArgMethods0, MaybeArgMethods,
                    LastMaybeArgMethod)
            ->
                expect(unify(LastMaybeArgMethod, no), this_file,
                    "get_input_output_vars: bad method for output var"),
                !:MaybeSpecMethod = specified(MaybeArgMethods)
            ;
                % We have run out of specified arg_methods, which means the
                % variable we are looking at right now is one that was added
                % by the polymorphism transformation.
                !:MaybeSpecMethod = all_same(arg_value)
            )
        ),
        OutVarModes = [var_mode_method(Var, Mode, arg_value) | OutVarModes0]
    ;
        % We should have caught this when we added the tabling pragma
        % to the proc_info.
        unexpected(this_file, "get_input_output_vars: bad var")
    ).

%-----------------------------------------------------------------------------%

:- pred create_instmap_delta(hlds_goals::in, instmap_delta::out) is det.

create_instmap_delta([], IMD) :-
    instmap_delta_from_assoc_list([], IMD).
create_instmap_delta([Goal | Rest], IMD) :-
    Goal = _ - GoalInfo,
    goal_info_get_instmap_delta(GoalInfo, IMD0),
    create_instmap_delta(Rest, IMD1),
    instmap_delta_apply_instmap_delta(IMD0, IMD1, test_size, IMD).

%-----------------------------------------------------------------------------%

:- type var_mode_method
    --->    var_mode_method(
                prog_var,   % The head variable.
                mer_mode,   % The mode of the head variable.
                arg_tabling_method
                            % For input arguments, this is the arg method to
                            % use in looking up the argument in the call table.
                            % For output arguments, this is the arg method to
                            % use in looking up the argument in the answer
                            % table (if any).
            ).

:- type var_mode_pos_method == var_mode_pos_method(arg_tabling_method).

:- type var_mode_pos_method(T)
    --->    var_mode_pos_method(
                prog_var,   % The head variable.
                mer_mode,   % The mode of the head variable.
                int,        % The position of the head variable in the list of
                            % inputs or outputs; first element is in the
                            % position numbered 0.
                T
                            % For input arguments, this is the arg method to
                            % use in looking up the argument in the call table.
                            % For output arguments, this is the arg method to
                            % use in looking up the argument in the answer
                            % table (if any), which for now is always
                            % arg_value.
                            %
                            % When T is unit, there is no info about how to
                            % look up the variable in a call or return table.
                            % This is useful for recording the structure of
                            % answer blocks and sequences of arguments
                            % for I/O tabling.
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

:- func bind_vars(list(prog_var)) = instmap_delta.

bind_vars(Vars) = InstMapDelta :-
    VarsAndGround = ground_vars(Vars),
    instmap_delta_from_assoc_list(VarsAndGround, InstMapDelta).

:- func ground_vars(list(prog_var)) = assoc_list(prog_var, mer_inst).

ground_vars(Vars) = VarsAndGround :-
    VarsAndGround = list.map(pair_with_ground, Vars).

:- func pair_with_ground(prog_var) = pair(prog_var, mer_inst).

pair_with_ground(Var) = Var - ground(shared, none).

:- pred goal_info_init_hide(set(prog_var)::in, instmap_delta::in,
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
:- func builtin_type(type_category) = bool.

builtin_type(type_cat_int) = yes.
builtin_type(type_cat_char) = yes.
builtin_type(type_cat_string) = yes.
builtin_type(type_cat_float) = yes.
builtin_type(type_cat_void) = yes.
builtin_type(type_cat_type_info) = no.
builtin_type(type_cat_type_ctor_info) = yes.
builtin_type(type_cat_typeclass_info) = yes.
builtin_type(type_cat_base_typeclass_info) = yes.
builtin_type(type_cat_higher_order) = no.
builtin_type(type_cat_enum) = no.
builtin_type(type_cat_dummy) = no.
builtin_type(type_cat_variable) = no.
builtin_type(type_cat_tuple) = no.
builtin_type(type_cat_user_ctor) = no.

    % Figure out what kind of data structure implements the lookup table
    % for values of a given builtin type.
    %
:- pred lookup_tabling_category(type_category::in,
    maybe(pair(string, table_trie_step))::out) is det.

lookup_tabling_category(type_cat_int,
        yes("int" -    table_trie_step_int)).
lookup_tabling_category(type_cat_char,
        yes("char" -   table_trie_step_char)).
lookup_tabling_category(type_cat_string,
        yes("string" - table_trie_step_string)).
lookup_tabling_category(type_cat_float,
        yes("float" -  table_trie_step_float)).
lookup_tabling_category(type_cat_void, _) :-
    unexpected(this_file, "lookup_tabling_category: void").
lookup_tabling_category(type_cat_dummy, _) :-
    unexpected(this_file, "lookup_tabling_category: dummy_type").
lookup_tabling_category(type_cat_type_info,
        yes("typeinfo" - table_trie_step_typeinfo)).
lookup_tabling_category(type_cat_type_ctor_info,
        yes("typeinfo" - table_trie_step_typeinfo)).
lookup_tabling_category(type_cat_typeclass_info, _) :-
    unexpected(this_file, "lookup_tabling_category: typeclass_info_type").
lookup_tabling_category(type_cat_base_typeclass_info, _) :-
    unexpected(this_file, "lookup_tabling_category: base_typeclass_info_type").
lookup_tabling_category(type_cat_enum, no).
lookup_tabling_category(type_cat_higher_order, no).
lookup_tabling_category(type_cat_tuple, no).
lookup_tabling_category(type_cat_variable, no).
lookup_tabling_category(type_cat_user_ctor, no).

    % Figure out which save and restore predicates in library/table_builtin.m
    % we need to use for values of types belonging the type category given by
    % the first argument. The returned value replaces CAT in
    % table_save_CAT_answer and table_restore_CAT_answer.

:- pred type_save_category(type_category::in, string::out) is det.

type_save_category(type_cat_enum,         "enum").
type_save_category(type_cat_int,          "int").
type_save_category(type_cat_char,         "char").
type_save_category(type_cat_string,       "string").
type_save_category(type_cat_float,        "float").
type_save_category(type_cat_higher_order, "pred").
type_save_category(type_cat_tuple,        "any").
type_save_category(type_cat_user_ctor,    "any").       % could do better
type_save_category(type_cat_variable,     "any").       % could do better
type_save_category(type_cat_dummy, _) :-
    unexpected(this_file, "type_save_category: dummy").
type_save_category(type_cat_void, _) :-
    unexpected(this_file, "type_save_category: void").
type_save_category(type_cat_type_info, "any").          % could do better
type_save_category(type_cat_type_ctor_info, _) :-
    unexpected(this_file, "type_save_category: type_ctor_info").
type_save_category(type_cat_typeclass_info, _) :-
    unexpected(this_file, "type_save_category: typeclass_info").
type_save_category(type_cat_base_typeclass_info, _) :-
    unexpected(this_file, "type_save_category: base_typeclass_info").

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

:- func stats_arg(table_attr_statistics, call_or_answer_table, int) = string.

stats_arg(Statistics, Kind, SeqNum) = ArgStr :-
    (
        Statistics = table_dont_gather_statistics,
        ArgStr = "NULL"
    ;
        Statistics = table_gather_statistics,
        (
            Kind = call_table,
            ArgStr = "&" ++ proc_table_info_name ++
                "->MR_pt_call_table_stats[" ++ int_to_string(SeqNum) ++ "]"
        ;
            Kind = answer_table,
            ArgStr = "&" ++ proc_table_info_name ++
                "->MR_pt_answer_table_stats[" ++ int_to_string(SeqNum) ++ "]"
        )
    ).

%-----------------------------------------------------------------------------%

:- pred table_gen_make_type_info_var(mer_type::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, prog_var::out,
    list(hlds_goal)::out) is det.

table_gen_make_type_info_var(Type, Context, !VarSet, !VarTypes, !TableInfo,
        TypeInfoVar, TypeInfoGoals) :-
    table_gen_make_type_info_vars([Type], Context, !VarSet, !VarTypes,
        !TableInfo, TypeInfoVars, TypeInfoGoals),
    ( TypeInfoVars = [TypeInfoVar0] ->
        TypeInfoVar = TypeInfoVar0
    ;
        unexpected(this_file,
            "table_gen_make_type_info_var: list length != 1")
    ).

:- pred table_gen_make_type_info_vars(list(mer_type)::in, term.context::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    table_info::in, table_info::out, list(prog_var)::out,
    list(hlds_goal)::out) is det.

table_gen_make_type_info_vars(Types, Context, !VarSet, !VarTypes, !TableInfo,
        TypeInfoVars, TypeInfoGoals) :-
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
    poly_info_extract(PolyInfo, PredInfo0, PredInfo,
        ProcInfo0, ProcInfo, ModuleInfo),

    % Get the new varset and vartypes from the proc_info.
    proc_info_get_vartypes(ProcInfo, !:VarTypes),
    proc_info_get_varset(ProcInfo, !:VarSet),

    % Put the new module_info, pred_info, and proc_info back in the
    % table_info.
    table_info_init(ModuleInfo, PredInfo, ProcInfo, !:TableInfo).

%-----------------------------------------------------------------------------%

:- pred var_mode_pos_is_io_state(vartypes::in,
    var_mode_pos_method::in) is semidet.

var_mode_pos_is_io_state(VarTypes, VarModePosMethod) :-
    var_is_io_state(VarTypes, project_var(VarModePosMethod)).

:- pred var_mode_is_io_state(vartypes::in, pair(prog_var, mer_mode)::in)
    is semidet.

var_mode_is_io_state(VarTypes, Var - _) :-
    var_is_io_state(VarTypes, Var).

:- pred var_is_io_state(vartypes::in, prog_var::in) is semidet.

var_is_io_state(VarTypes, Var) :-
    map.lookup(VarTypes, Var, VarType),
    type_is_io_state(VarType).

%-----------------------------------------------------------------------------%

:- func tabling_c_attributes = pragma_foreign_proc_attributes.

tabling_c_attributes = Attrs :-
    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs0, Attrs).

:- func make_generator_c_attributes = pragma_foreign_proc_attributes.

make_generator_c_attributes = Attrs :-
    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_may_call_mercury, Attrs0, Attrs).

:- func dummy_type_var = mer_type.

dummy_type_var = Type :-
    varset.init(DummyTVarSet0),
    varset.new_var(DummyTVarSet0, DummyTVar, _),
    Type = type_variable(DummyTVar, kind_star).

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

%-----------------------------------------------------------------------------%

:- type table_info
    ---> table_info(
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

:- func this_file = string.

this_file = "table_gen.m".

%-----------------------------------------------------------------------------%
