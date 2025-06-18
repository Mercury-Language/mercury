%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2013-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: det_check_proc.m.
% Author: zs.
%
% This module handles reporting of errors and warnings about procedures
% having a determinism that is incompatible with some other aspects
% of their declarations. It subcontracts diagnosis of problems with the
% determinisms of goal goals to det_check_goal.m, which in turn subcontracts
% some part of its work dealing with switches (the goal kind with the
% most kinds of errors applicable to it) to det_check_switch.m.
%
%---------------------------------------------------------------------------%

:- module check_hlds.det_check_proc.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Check the determinism declarations of the specified procedures,
    % which should be all the valid procedures in this module.
    %
    % This is the main predicate exported by this module.
    %
:- pred check_determinism_of_procs(io.text_output_stream::in,
    list(pred_proc_id)::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % As above, but for just a single procedure. Used when a change in
    % the body of a procedure (by e.g. cse_detection.m) requires redoing
    % the procedure's mode- and determinism-analysis, in order to update
    % the relevant fields of hlds_goal_infos.
    %
:- pred check_determinism_of_proc(io.text_output_stream::in,
    pred_proc_id::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Check the determinism declarations of the specified procedures
    % whose bodies det_analysis.m did not analyze because they are
    % imported from another module (and whose bodies are therefore
    % not available, at least not without intermodule optimization).
    % However, we can still perform some checks on the mode declarations
    % themselves.
    %
    % Note that any errors with those declarations should also be caught
    % when the module that we are importing the declaration from is compiled.
    % The point of doing those checks *here* is to prevent later passes
    % of *this* compiler invocation being given data that violates
    % our rules of static semantics.
    % (See tests/invalid_manual/gh118.m for an example.)
    %
:- pred check_determinism_of_imported_procs(io.text_output_stream::in,
    module_info::in, list(pred_proc_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_check_goal.
:- import_module check_hlds.det_check_switch.
:- import_module check_hlds.det_util.
:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.instmap.
:- import_module hlds.pred_name.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_type_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_table.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module cord.
:- import_module maybe.
:- import_module require.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

check_determinism_of_procs(_, [], !ModuleInfo, !Specs).
check_determinism_of_procs(ProgressStream, [PredProcId | PredProcIds],
        !ModuleInfo, !Specs) :-
    check_determinism_of_proc(ProgressStream, PredProcId, !ModuleInfo, !Specs),
    check_determinism_of_procs(ProgressStream, PredProcIds,
        !ModuleInfo, !Specs).

check_determinism_of_proc(ProgressStream, PredProcId, !ModuleInfo, !Specs) :-
    module_info_pred_proc_info(!.ModuleInfo, PredProcId, PredInfo, ProcInfo),

    trace [compiletime(flag("debug-check-detism-progress")), io(!IO)] (
        PredModePieces = describe_one_proc_name_maybe_argmodes(!.ModuleInfo,
            output_mercury, no, should_not_module_qualify, [], PredProcId),
        PredStr = error_pieces_to_one_line_string(PredModePieces),
        io.format(ProgressStream, "check_determinism_of_proc %s\n",
            [s(PredStr)], !IO)
    ),

    % Report any mismatches between the declared and the actual determinisms
    % of the procedure.
    check_for_too_tight_or_loose_declared_determinism(PredProcId,
        PredInfo, ProcInfo, !ModuleInfo, !Specs),

    % Some determinisms are invalid for some kinds of predicates.
    % Check for and report any violations of these rules.
    make_reqscope_checks_if_needed(!.ModuleInfo, PredProcId,
        PredInfo, ProcInfo, !Specs),
    check_determinism_for_eval_method(ProcInfo, !Specs),
    check_determinism_if_pred_is_main(PredInfo, ProcInfo, !Specs),
    check_function_semantics(!.ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs),
    check_io_state_proc_detism(!.ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs),
    check_exported_proc_detism(PredProcId, ProcInfo, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

check_determinism_of_imported_procs(_, _, [], !Specs).
check_determinism_of_imported_procs(ProgressStream, ModuleInfo,
        [PredProcId | PredProcIds], !Specs) :-
    check_determinism_of_imported_proc(ProgressStream, ModuleInfo,
        PredProcId, !Specs),
    check_determinism_of_imported_procs(ProgressStream, ModuleInfo,
        PredProcIds, !Specs).

:- pred check_determinism_of_imported_proc(io.text_output_stream::in,
    module_info::in, pred_proc_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_of_imported_proc(ProgressStream, ModuleInfo, PredProcId,
        !Specs) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),

    trace [compiletime(flag("debug-check-detism-progress")), io(!IO)] (
        PredModePieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
            output_mercury, no, should_not_module_qualify, [], PredProcId),
        PredStr = error_pieces_to_one_line_string(PredModePieces),
        io.format(ProgressStream, "check_determinism_of_imported_proc %s\n",
            [s(PredStr)], !IO)
    ),

    check_function_semantics(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs),
    check_io_state_proc_detism(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred check_for_too_tight_or_loose_declared_determinism(
    pred_proc_id::in, pred_info::in, proc_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_for_too_tight_or_loose_declared_determinism(PredProcId,
        PredInfo, ProcInfo, !ModuleInfo, !Specs) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),
    (
        MaybeDetism = no
    ;
        MaybeDetism = yes(DeclaredDetism),
        compare_determinisms(DeclaredDetism, InferredDetism, Cmp),
        (
            Cmp = first_detism_same_as
        ;
            Cmp = first_detism_looser_than,
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, warn_det_decls_too_lax,
                ShouldIssueWarning),
            globals.lookup_bool_option(Globals, warn_inferred_erroneous,
                WarnAboutInferredErroneous),
            pred_info_get_markers(PredInfo, Markers),
            ( if
                ShouldIssueWarning = yes,

                % Don't report warnings for class method implementations --
                % the determinism in the `:- typeclass' declaration will be
                % the loosest of all possible instances. This is similar to
                % the reason we don't report warnings for lambda expressions.
                not marker_is_present(Markers, marker_class_instance_method),

                % Don't report warnings for procedures with no clauses.
                not marker_is_present(Markers, marker_stub),

                % Don't report warnings for predicates for which the user
                % has written a pragma requesting no warnings.
                not marker_is_present(Markers, marker_no_detism_warning),

                % Don't report warnings for compiler-generated Unify, Compare
                % or Index procedures, since the user has no way to shut
                % these up. These can happen for the Unify pred for the unit
                % type, if such types are not boxed (as they are not
                % boxed for the IL backend).
                not is_unify_index_or_compare_pred(PredInfo),

                % Don't warn about predicates which are inferred erroneous
                % when the appropriate option is set. This is to avoid warnings
                % about unimplemented predicates.
                (
                    WarnAboutInferredErroneous = yes
                ;
                    WarnAboutInferredErroneous = no,
                    InferredDetism \= detism_erroneous
                ),

                % Only warn about predicates that are defined in this module.
                % This avoids warnings being emitted for opt_imported
                % predicates.
                pred_info_get_status(PredInfo, Status),
                pred_status_defined_in_this_module(Status) = yes
            then
                % The returned ReportMsg will contain the actual determinism
                % colored as incorrect. XXX Is this ok for a warning?
                report_determinism_problem(!.ModuleInfo, PredProcId,
                    "Warning", "could be tighter", [],
                    DeclaredDetism, InferredDetism, ReportMsg),
                ReportSpec = error_spec($pred, severity_warning,
                    phase_detism_check, [ReportMsg]),
                !:Specs = [ReportSpec | !.Specs]
            else
                true
            )
        ;
            ( Cmp = first_detism_tighter_than
            ; Cmp = first_detism_incomparable
            ),
            proc_info_get_goal(ProcInfo, Goal),
            proc_info_get_var_table(ProcInfo, VarTable),
            proc_info_get_initial_instmap(!.ModuleInfo, ProcInfo, InstMap0),
            det_info_init(!.ModuleInfo, PredProcId, VarTable,
                pess_extra_vars_report, [], DetInfo0),
            det_diagnose_goal_get_msgs(InstMap0, DeclaredDetism, Goal,
                GoalMsgs, DetInfo0, DetInfo),
            det_info_get_module_info(DetInfo, !:ModuleInfo),
            cse_nopull_msgs(ProcInfo, CseMsgs),
            DetailMsgs = GoalMsgs ++ CseMsgs,
            (
                DetailMsgs = [],
                ReasonPieces = []
            ;
                DetailMsgs = [_],
                ReasonPieces = [words("The reason for the difference"),
                    words("is the following."), nl]
            ;
                DetailMsgs = [_, _ | _],
                ReasonPieces = [words("The reasons for the difference"),
                    words("are the following."), nl]
            ),
            report_determinism_problem(!.ModuleInfo, PredProcId,
                "Error", "is not satisfied", ReasonPieces,
                DeclaredDetism, InferredDetism, ReportMsg),
            ReportSpec = error_spec($pred, severity_error, phase_detism_check,
                [ReportMsg | start_each_msg_with_blank_line(DetailMsgs)]),
            !:Specs = [ReportSpec | !.Specs]
        )
    ).

:- pred cse_nopull_msgs(proc_info::in, list(error_msg)::out) is det.

cse_nopull_msgs(ProcInfo, Msgs) :-
    proc_info_get_cse_nopull_contexts(ProcInfo, CseNoPullContexts),
    list.sort(CseNoPullContexts, SortedCseNoPullContexts),
    (
        SortedCseNoPullContexts = [],
        Msgs = []
    ;
        SortedCseNoPullContexts = [FirstNoPullContext | _],
        Msgs = [msg(FirstNoPullContext, cse_nopull_pieces)]
    ).

:- func cse_nopull_pieces = list(format_piece).

cse_nopull_pieces =
    [words("It is possible that"),
    words("the cause of the declared determinism not being satisfied"),
    words("is the inability of determinism analysis to recognize that"),
    words("a disjunction (usually created by the compiler for a switch arm)"),
    words("is a switch on a *subterm* of a variable"),
    words("when the instantiation state of that variable"),
    words("is at least partially unique."),
    words("This is because converting such a disjunction to a switch"),
    words("requires replacing several unifications,"),
    words("one in each arm of the disjunction,"),
    words("that each unify the variable representing the subterm"),
    words("(e.g. the tail of a list) with the same function symbol,"),
    words("with just one unification before the disjunction,"),
    words("but due to limitations of the current modechecker,"),
    words("this transformation could destroy the uniqueness."), nl,
    words("In cases where this uniqueness is not needed,"),
    words("the programmer can fix the determinism error"),
    words("by performing this transformation manually."), nl].

%---------------------------------------------------------------------------%

:- pred make_reqscope_checks_if_needed(module_info::in,
    pred_proc_id::in, pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

make_reqscope_checks_if_needed(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs) :-
    pred_info_get_markers(PredInfo, Markers),
    some [!NeedReqScope] (
        ( if
            marker_is_present(Markers, marker_has_incomplete_switch),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, inform_incomplete_switch, yes)
        then
            % If this option is specified, it acts as an implicit
            % require_complete_switch scope around all switches.
            !:NeedReqScope = yes,
            InformIncompleteSwitches = inform_incomplete_switches
        else
            !:NeedReqScope = no,
            InformIncompleteSwitches = do_not_inform_incomplete_switches
        ),
        ( if marker_is_present(Markers, marker_has_require_scope) then
            !:NeedReqScope = yes
        else
            true
        ),
        ( if marker_is_present(Markers, marker_req_sw_arms_type_order) then
            !:NeedReqScope = yes,
            ReqArmsTypeOrder = req_arms_in_type_order
        else
            ReqArmsTypeOrder = no_req_arms_in_type_order
        ),
        NeedReqScope = !.NeedReqScope
    ),
    (
        NeedReqScope = yes,
        proc_info_get_goal(ProcInfo, Goal),
        proc_info_get_var_table(ProcInfo, VarTable),
        proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap0),
        det_info_init(ModuleInfo, PredProcId, VarTable,
            pess_extra_vars_ignore, [], DetInfo0),
        Params = reqscope_params(InformIncompleteSwitches, ReqArmsTypeOrder),
        reqscope_check_goal(Params, InstMap0, no, [], Goal, DetInfo0, DetInfo),
        det_info_get_error_specs(DetInfo, RCSSpecs),
        !:Specs = RCSSpecs ++ !.Specs
    ;
        NeedReqScope = no
    ).

:- func detism_decl_name(detism_decl) = string.

detism_decl_name(DetismDecl) = Name :-
    (
        DetismDecl = detism_decl_explicit,
        Name = "determinism declaration"
    ;
        DetismDecl = detism_decl_implicit,
        Name = "implicit determinism declaration"
    ;
        DetismDecl = detism_decl_none,
        Name = "nonexistent determinism declaration"
        % This shouldn't happen, but if it does, it is better to get an
        % error message that puts you on the right track than to get
        % a compiler abort.
        % unexpected($pred, "detism_decl_name: detism_decl_none")
    ).

:- pred get_valid_determinisms(tabled_eval_method::in, determinism::out)
    is nondet.

get_valid_determinisms(TabledMethod, Detism) :-
    determinism(Detism),
    valid_determinism_for_tabled_eval_method(TabledMethod, Detism) = yes.

    % Generate all the possible determinisms.
    %
:- pred determinism(determinism).
:- mode determinism(out) is multi.
:- mode determinism(in) is det.         % To ensure we don't forget any.

determinism(detism_det).
determinism(detism_semi).
determinism(detism_multi).
determinism(detism_non).
determinism(detism_cc_multi).
determinism(detism_cc_non).
determinism(detism_erroneous).
determinism(detism_failure).

%---------------------------------------------------------------------------%

:- pred check_determinism_for_eval_method(proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_for_eval_method(ProcInfo, !Specs) :-
    proc_info_get_inferred_determinism(ProcInfo, InferredDetism),
    proc_info_get_eval_method(ProcInfo, EvalMethod),
    ( if
        EvalMethod = eval_tabled(TabledMethod),
        valid_determinism_for_tabled_eval_method(TabledMethod, InferredDetism)
            = no
    then
        PragmaName = tabled_eval_method_to_pragma_name(TabledMethod),
        InferredDetismStr = determinism_to_string(InferredDetism),
        proc_info_get_context(ProcInfo, Context),
        MainPieces = [words("Error:")] ++
            color_as_subject([pragma_decl(PragmaName),
                words("declarations")]) ++
            [words("are")] ++
            color_as_incorrect([words("not allowed")]) ++
            [words("for procedures with determinism")] ++
            color_as_incorrect([quote(InferredDetismStr), suffix(".")]) ++
            [nl],

        solutions.solutions(get_valid_determinisms(TabledMethod), Detisms),
        DetismStrs = list.map(determinism_to_string, Detisms),
        list.sort(DetismStrs, SortedDetismStrs),
        DetismPieces = fixed_list_to_color_pieces(color_correct, "and",
            [suffix(".")], SortedDetismStrs),
        VerbosePieces =
            [words("This pragma is valid only for the following"),
            words(choose_number(Detisms, "determinism", "determinisms")),
            suffix(":") | DetismPieces] ++ [nl],
        ValidSpec = error_spec($pred, severity_error, phase_detism_check,
            [simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)])]),
        !:Specs = [ValidSpec | !.Specs]
    else
        true
    ).

    % Check if the given evaluation method is allowed with
    % the given determinism.
    %
:- func valid_determinism_for_tabled_eval_method(tabled_eval_method,
    determinism) = bool.

valid_determinism_for_tabled_eval_method(TabledMethod, Detism) = Valid :-
    (
        ( TabledMethod = tabled_loop_check
        ; TabledMethod = tabled_memo(_)
        ),
        determinism_components(Detism, _, MaxSoln),
        (
            MaxSoln = at_most_zero,
            Valid = no
        ;
            ( MaxSoln = at_most_one
            ; MaxSoln = at_most_many
            ; MaxSoln = at_most_many_cc
            ),
            Valid = yes
        )
    ;
        TabledMethod = tabled_io(_, _),
        unexpected($pred, "called after tabling phase")
    ;
        TabledMethod = tabled_minimal(_),
        % The following reasons specify why a particular determinism is
        % incompatible with minimal model tabling.
        %
        % Reason 1:
        % Determinism analysis isn't yet smart enough to know whether
        % a cannot_fail execution path is guaranteed not to go through a call
        % to a predicate that is mutually recursive with this one, which
        % (if this predicate is minimal model) is the only way that the
        % predicate can be properly cannot_fail. The problem is that
        % in general, the mutually recursive predicate may be in
        % another module.
        %
        % Reason 2:
        % The transformation, as currently implemented, assumes that
        % it is possible to reach the call table tip, and generates HLDS
        % that refers to the introduced variable representing this tip.
        % This variable however, will be optimized away if the code
        % cannot succeed, causing a code generator abort.
        %
        % Reason 3:
        % Minimal model semantics requires computing to a fixpoint, and
        % this is incompatible with the notion of committed choice.
        %
        % Reason 4:
        % Doing the analysis required to ensure that a predicate can't have
        % more than one solution is much harder if the predicate concerned is
        % minimal_model. In theory, this analysis could be done, but it would
        % take a lot of programming, and since there is a simple workaround
        % (make the predicate nondet, and check the number of solutions at the
        % caller), this would not be cost-effective.
        (
            Detism = detism_det,
            Valid = no                  % Reason 1
        ;
            Detism = detism_semi,
            Valid = no                  % Reason 4
        ;
            Detism = detism_multi,      % Reason 1
            Valid = yes
        ;
            Detism = detism_non,
            Valid = yes
        ;
            Detism = detism_cc_multi,   % Reason 3
            Valid = no
        ;
            Detism = detism_cc_non,     % Reason 3
            Valid = no
        ;
            Detism = detism_erroneous,  % Reason 2
            Valid = no
        ;
            Detism = detism_failure,    % Reason 2
            Valid = no
        )
    ).

%---------------------------------------------------------------------------%

:- pred check_determinism_if_pred_is_main(pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_determinism_if_pred_is_main(PredInfo, ProcInfo, !Specs) :-
    % Check that `main/2' has determinism `det' or `cc_multi',
    % as required by the language reference manual.
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    ( if
        pred_info_name(PredInfo) = "main",
        pred_info_pred_form_arity(PredInfo) = pred_form_arity(2)
    then
        MainSNA = sym_name_arity(unqualified("main"), 2),
        MainSNAPiece = unqual_sym_name_arity(MainSNA),
        proc_info_get_context(ProcInfo, ProcContext),
        ( if pred_info_is_exported(PredInfo) then
            true
        else
            ExportPieces =
                [words("Error:")] ++ color_as_subject([MainSNAPiece]) ++
                [words("must be")] ++ color_as_correct([words("exported,")]) ++
                [words("but")] ++ color_as_incorrect([words("it is not.")]) ++
                [nl],
            ExportSpec = spec($pred, severity_error, phase_detism_check,
                ProcContext, ExportPieces),
            !:Specs = [ExportSpec | !.Specs]
        ),
        (
            MaybeDetism = no,
            DetismPieces =
                [words("Error:")] ++ color_as_subject([MainSNAPiece]) ++
                [words("must have a")] ++
                color_as_correct([words("declared determinism,")]) ++
                [words("but")] ++ color_as_correct([words("it does not.")]) ++
                [nl],
            DetismSpec = spec($pred, severity_error, phase_detism_check,
                ProcContext, DetismPieces),
            !:Specs = [DetismSpec | !.Specs]
        ;
            MaybeDetism = yes(DeclaredDetism),
            (
                ( DeclaredDetism = detism_det
                ; DeclaredDetism = detism_cc_multi
                )
            ;
                ( DeclaredDetism = detism_semi
                ; DeclaredDetism = detism_multi
                ; DeclaredDetism = detism_non
                ; DeclaredDetism = detism_cc_non
                ; DeclaredDetism = detism_erroneous
                ; DeclaredDetism = detism_failure
                ),
                DetismStr = determinism_to_string(DeclaredDetism),
                DetismPieces =
                    [words("Error:")] ++ color_as_subject([MainSNAPiece]) ++
                    [words("must be either")] ++
                    color_as_correct([quote("det")]) ++ [words("or")] ++
                    color_as_correct([quote("cc_multi"), suffix(";")]) ++
                    [words("it may not be")] ++
                    color_as_incorrect([quote(DetismStr), suffix(".")]) ++
                    [nl],
                DetismSpec = spec($pred, severity_error, phase_detism_check,
                    ProcContext, DetismPieces),
                !:Specs = [DetismSpec | !.Specs]
            )
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred check_function_semantics(module_info::in, pred_proc_id::in,
    pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_function_semantics(ModuleInfo, PredProcId, PredInfo, ProcInfo, !Specs) :-
    % Is this procedure the primary mode of a function?
    % (Primary mode meaning "all arguments other than the return value
    % being input".)
    ( if
        pred_info_is_pred_or_func(PredInfo) = pf_function,
        proc_info_get_argmodes(ProcInfo, PredArgModes),
        pred_info_get_arg_types(PredInfo, PredArgTypes),
        pred_args_to_func_args(PredArgTypes, FuncArgTypes, FuncResultType),
        pred_args_to_func_args(PredArgModes, FuncArgModes, _FuncResultMode),
        all_modes_are_fully_input(ModuleInfo, FuncArgTypes, FuncArgModes)
    then
        proc_info_get_inferred_determinism(ProcInfo, InferredDetism),
        determinism_components(InferredDetism, CanFail, NumSolns),
        (
            ( NumSolns = at_most_zero
            ; NumSolns = at_most_one
            )
        ;
            ( NumSolns = at_most_many
            ; NumSolns = at_most_many_cc
            ),
            % Having a function that can succeed more than once
            % in its primary mode violates referential transparency.
            % This is because if you see two calls to the function
            % with the same input arguments, you can't be sure that
            % they represent the same value. This can happen in C
            % with e.g. the `rand()' function; we don't want to allow it
            % in Mercury.
            MultiSolnSpec = report_multisoln_func(ModuleInfo, PredProcId,
                ProcInfo, InferredDetism),
            !:Specs = [MultiSolnSpec | !.Specs]
        ),
        (
            CanFail = cannot_fail
        ;
            CanFail = can_fail,
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, warn_can_fail_function,
                WarnCanFailFunction),
            pred_info_get_status(PredInfo, PredStatus),
            pred_info_get_origin(PredInfo, Origin),
            pred_info_get_obsolete_in_favour_of(PredInfo, MaybeObsolete),
            ( if
                WarnCanFailFunction = yes,
                pred_status_defined_in_this_module(PredStatus) = yes,
                Origin = origin_user(_),
                MaybeObsolete = no
            then
                CanFailSpec = report_can_fail_func(ModuleInfo, PredProcId,
                    PredInfo, ProcInfo, FuncResultType, InferredDetism),
                !:Specs = [CanFailSpec | !.Specs]
            else
                true
            )
        )
    else
        true
    ).

:- func report_multisoln_func(module_info, pred_proc_id, proc_info,
    determinism) = error_spec.

report_multisoln_func(ModuleInfo, PredProcId, ProcInfo, InferredDetism)
        = Spec :-
    proc_info_get_context(ProcInfo, Context),
    PredModePieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
        output_mercury, no, should_not_module_qualify, [], PredProcId),
    InferredDetismStr = mercury_det_to_string(InferredDetism),
    MainPieces = [words("Error: invalid determinism for")] ++
        color_as_subject(PredModePieces ++ [suffix(":")]) ++ [nl,
        words("the primary mode of a function cannot be")] ++
        color_as_incorrect([quote((InferredDetismStr)), suffix(".")]) ++
        [nl],
    VerbosePieces = func_primary_mode_det_msg,
    Spec = error_spec($pred, severity_error, phase_detism_check,
        [simple_msg(Context,
            [always(MainPieces),
            verbose_only(verbose_once, VerbosePieces)])]).

:- func func_primary_mode_det_msg = list(format_piece).

func_primary_mode_det_msg = [words("In Mercury,"),
    words("a function is supposed to be a true mathematical function"),
    words("of its arguments; that is, the value of the function's result"),
    words("should be determined only by the values of its arguments."),
    words("(Allowing functions to have more than one result for the same"),
    words("arguments would break referential transparency.)"),
    words("Most likely, this procedure should be a predicate,"),
    words("not a function."), nl].

:- func report_can_fail_func(module_info, pred_proc_id, pred_info, proc_info,
    mer_type, determinism) = error_spec.

report_can_fail_func(ModuleInfo, PredProcId, PredInfo, ProcInfo, ResultType0,
        InferredDetism) = Spec :-
    proc_info_get_context(ProcInfo, FuncContext),
    PredProcId = proc(PredId, _ProcId),
    PredNamePieces = describe_unqual_pred_name(ModuleInfo, PredId),
    determinism_components(InferredDetism, _, MaxSolns),
    determinism_components(ProposedDetism, cannot_fail, MaxSolns),
    InferredDetismStr = mercury_det_to_string(InferredDetism),
    ProposedDetismStr = mercury_det_to_string(ProposedDetism),
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    strip_module_names_from_type(strip_all_module_names, set_default_func,
        ResultType0, ResultType),
    % Even if the predicate has existentially quantified type variables,
    % we don't want to print them in this message.
    ExistQTVars = [],
    ResultTypePieces = type_to_pieces(TypeVarSet, InstVarSet,
        print_name_only, add_quotes, ExistQTVars, ResultType),
    NewResultType =
        defined_type(unqualified("maybe"), [ResultType], kind_star),
    NewResultTypePieces = type_to_pieces(TypeVarSet, InstVarSet,
        print_name_only, add_quotes, ExistQTVars, NewResultType),
    Pieces = [words("Warning: the primary mode of the")] ++
        % NOTE PredNamePieces will start with "function".
        color_as_subject(PredNamePieces) ++
        color_as_incorrect([words("can fail."), nl]) ++
        [words("Consider turning this"),
        words(InferredDetismStr), words("function"),
        words("either into a"), words(InferredDetismStr), words("predicate,"),
        words("or into a"), words(ProposedDetismStr), words("function"),
        words("by changing the return type from")] ++ ResultTypePieces ++
        [words("to")] ++ NewResultTypePieces ++ [suffix("."), nl],
    Spec = spec($pred, severity_warning, phase_detism_check,
        FuncContext, Pieces).

%---------------------------------------------------------------------------%

:- pred check_io_state_proc_detism(module_info::in, pred_proc_id::in,
    pred_info::in, proc_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_io_state_proc_detism(ModuleInfo, PredProcId, PredInfo, ProcInfo,
        !Specs) :-
    ( if
        proc_info_has_io_state_pair(ModuleInfo, ProcInfo, _InArg, _OutArg),
        proc_info_get_inferred_determinism(ProcInfo, ActualDetism),
        proc_info_get_declared_determinism(ProcInfo, MaybeDeclaredDetism),
        is_detism_ok_for_io(ActualDetism) = no,
        (
            MaybeDeclaredDetism = no,
            BadDetismPieces = [words("the inferred determinism")] ++
                color_as_incorrect(
                    [quote(determinism_to_string(ActualDetism))])
        ;
            MaybeDeclaredDetism = yes(DeclaredDetism),
            DeclaredDetismOk = is_detism_ok_for_io(DeclaredDetism),
            (
                DeclaredDetismOk = yes,
                % Do not generate the message below. The actual error
                % the programmer should fix is whatever issue is causing
                % the mismatch between DeclaredDetism and ActualDetism;
                % a reminder about ActualDetism not being allowed with I/O
                % would just be a nuisance, rather than being helpful.
                fail
            ;
                DeclaredDetismOk = no,
                % In this case, the programmer has the problems that
                % even the declared detism is not allowed with I/O.
                % This is what we report here. If the actual and declared
                % determinisms differ, that is a *different* problem,
                % which would get its own separate error message.
                BadDetismPieces = color_as_incorrect(
                    [quote(determinism_to_string(DeclaredDetism))])
            )
        )
    then
        pred_info_get_module_name(PredInfo, PredModuleName),
        module_info_get_name(ModuleInfo, ModuleName),
        % Almost all error messages this predicate will generate will refer
        % to a procedure that is local to the module being compiled,
        % and for these, printing the module name would only be clutter.
        % However, in rare cases such as the tests/invalid_manual/gh118
        % test case, we may need to generate a message about an imported
        % predicate, and for this case, the name of the module containing
        % the predicate that we complain about is crucial information.
        ( if ModuleName = PredModuleName then
            ShouldModuleQual = should_not_module_qualify
        else
            ShouldModuleQual = should_module_qualify
        ),
        ProcColonPieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
            output_mercury, yes(color_subject), ShouldModuleQual,
            [suffix(":")], PredProcId),
        GoodDetismPieces = color_as_correct([quote("det"), suffix(",")]) ++
            color_as_correct([quote("cc_multi")]) ++ [words("and")] ++
            color_as_correct([quote("erroneous"), suffix(",")]),
        % We used to print the second sentence (actually, only its first half)
        % only if verbose error messages were enabled, but anyone who makes
        % this error will probably need that extra help, so now we don't
        % make them ask for it with -E.
        Pieces = [words("In")] ++ ProcColonPieces ++ [nl,
            words("error:")] ++ BadDetismPieces ++
            [words("is not a valid determinism"),
            words("for a predicate that has I/O state arguments."),
            words("The valid determinisms for such predicates are")] ++
            GoodDetismPieces ++
            [words("since the I/O state can be neither duplicated"),
            words("nor destroyed."), nl],
        proc_info_get_context(ProcInfo, ProcContext),
        Spec = spec($pred, severity_error, phase_detism_check,
            ProcContext, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- func is_detism_ok_for_io(determinism) = bool.

is_detism_ok_for_io(Detism) = Ok :-
    (
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        ; Detism = detism_erroneous
        ),
        Ok = yes
    ;
        ( Detism = detism_semi
        ; Detism = detism_multi
        ; Detism = detism_non
        ; Detism = detism_cc_non
        ; Detism = detism_failure
        ),
        Ok = no
    ).

%---------------------------------------------------------------------------%

    % Check to make sure that if this procedure is exported via a pragma
    % foreign_export declaration, then the determinism is not multi or nondet.
    % Pragma exported procs that have been declared to have these determinisms
    % should have been picked up in make_hlds, so this is just to catch those
    % whose determinisms need to be inferred.
    %
:- pred check_exported_proc_detism(pred_proc_id::in, proc_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_exported_proc_detism(PredProcId, ProcInfo, !ModuleInfo, !Specs) :-
    module_info_get_pragma_exported_procs(!.ModuleInfo, ExportedProcsCord0),
    ExportedProcs = cord.list(ExportedProcsCord0),
    ExportedProcsCord = cord.from_list(ExportedProcs),
    % So that any later conversion from cord to list will do nothing
    % except take off the cord wrapper.
    module_info_set_pragma_exported_procs(ExportedProcsCord, !ModuleInfo),
    proc_info_get_inferred_determinism(ProcInfo, Detism),
    PredProcId = proc(PredId, ProcId),
    ( if
        is_proc_pragma_exported(ExportedProcs, PredId, ProcId, ExportContext),
        ( Detism = detism_multi
        ; Detism = detism_non
        )
    then
        DetismStr = determinism_to_string(Detism),
        Pieces = [words("Error:"),
            pragma_decl("foreign_export"), words("declaration"),
            words("for a procedure whose determinism is")] ++
            color_as_incorrect([quote(DetismStr), suffix(".")]) ++ [nl],
        Spec = spec($pred, severity_error, phase_detism_check,
            ExportContext, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- pred is_proc_pragma_exported(list(pragma_exported_proc)::in,
    pred_id::in, proc_id::in, prog_context::out) is semidet.

is_proc_pragma_exported([ExportProc | ExportProcs], PredId, ProcId, Context) :-
    ( if ExportProc = pragma_exported_proc(_, PredId, ProcId, _, Context0) then
        Context = Context0
    else
        is_proc_pragma_exported(ExportProcs, PredId, ProcId, Context)
    ).

%---------------------------------------------------------------------------%

:- pred report_determinism_problem(module_info::in, pred_proc_id::in,
    string::in, string::in, list(format_piece)::in,
    determinism::in, determinism::in, error_msg::out) is det.

report_determinism_problem(ModuleInfo, PredProcId, ErrorOrWarn, ProblemStr,
        ReasonPieces, DeclaredDetism, InferredDetism, Msg) :-
    module_info_proc_info(ModuleInfo, PredProcId, ProcInfo),
    proc_info_get_detism_decl(ProcInfo, DetismDecl),
    proc_info_get_context(ProcInfo, Context),
    ProcPieces = describe_one_proc_name_maybe_argmodes(ModuleInfo,
        output_mercury, yes(color_subject), should_not_module_qualify, [],
        PredProcId),
    DeclaredStr = determinism_to_string(DeclaredDetism),
    InferredStr = determinism_to_string(InferredDetism),
    DeclaredPieces = color_as_correct([quote(DeclaredStr), suffix(",")]),
    InferredPieces = color_as_incorrect([quote(InferredStr), suffix(".")]),
    Pieces = [words(ErrorOrWarn), suffix(":"), words("the"),
        words(detism_decl_name(DetismDecl)), words("for")] ++ ProcPieces ++
        color_as_incorrect([words(ProblemStr), suffix(".")]) ++ [nl] ++
        [words("Declared")] ++ DeclaredPieces ++
        [words("inferred")] ++ InferredPieces ++ [nl] ++
        ReasonPieces,
    Msg = msg(Context, Pieces).

%---------------------------------------------------------------------------%
:- end_module check_hlds.det_check_proc.
%---------------------------------------------------------------------------%
