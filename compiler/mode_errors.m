%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mode_errors.m.
% Main author: fjh.
%
% This module contains all the error-reporting routines for the mode-checker.
%
%---------------------------------------------------------------------------%

:- module check_hlds.mode_errors.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type merge_context
    --->    merge_disj
    ;       merge_if_then_else
    ;       merge_stm_atomic.

:- type merge_error
    --->    merge_error(prog_var, assoc_list(prog_context, mer_inst)).

:- type merge_errors == list(merge_error).

:- type delayed_goal
    --->    delayed_goal(
                set_of_progvar,     % The vars the goal is waiting on.
                mode_error_info,    % The reason the goal can't be scheduled.
                hlds_goal           % The goal itself.
            ).

:- type var_multimode_pred_error
    --->    no_matching_mode(list(prog_var))
    ;       more_than_one_matching_mode(list(prog_var))
    ;       some_ho_args_non_ground(list(prog_var)).

:- type pred_var_multimode_pred_error
    --->    pred_var_multimode_pred_error(pred_id, var_multimode_pred_error).

:- type mode_error
    --->    mode_error_disj(merge_context, merge_errors)
            % Different arms of a disjunction result in different insts
            % for some non-local variables.

    ;       mode_error_par_conj(merge_errors)
            % Different arms of a parallel conj result in mutually exclusive
            % bindings - i.e. the process of unifying the instmaps from the end
            % of each branch failed.

    ;       mode_error_higher_order_pred_var(pred_or_func, prog_var, mer_inst,
                arity)
            % The predicate variable in a higher-order predicate or function
            % call didn't have a higher-order predicate or function inst
            % of the appropriate arity.

    ;       mode_error_poly_unify(prog_var, mer_inst)
            % A variable in a polymorphic unification with unknown
            % type has inst other than `ground' or `any'.

    ;       mode_error_var_is_live(prog_var)
            % Call to a predicate which will clobber its argument,
            % but the argument is still live.

    ;       mode_error_var_has_inst(prog_var, mer_inst, mer_inst,
                maybe(pred_var_multimode_pred_error))
            % Call to a predicate with an insufficiently
            % instantiated variable (for preds with one mode).

    ;       mode_error_unify_pred(prog_var, mode_error_unify_rhs, mer_type,
                pred_or_func)
            % An attempt was made to unify two higher-order
            % predicate or function variables.

    ;       mode_error_implied_mode(prog_var, mer_inst, mer_inst)
            % A call to a predicate with an overly instantiated variable
            % would use an implied mode of the predicate, but we can't
            % introduce a simple unification after calling the predicate in a
            % principal mode because the relevant variable has complex inst
            % (such as any).

    ;       mode_error_no_mode_decl
            % A call to a predicate for which there are no mode declarations
            % (and mode inference is not enabled).

    ;       mode_error_no_matching_mode(list(prog_var), list(mer_inst),
                list(list(mer_inst)))
            % Call to a predicate with an insufficiently instantiated variable
            % (for preds with >1 mode).
            %
            % The first two arguments give the argument vars of the call
            % and their insts at the time of the call.
            %
            % If the third argument is a nonempty list, then every member
            % of that list gives the list of the required initial insts
            % of the argument vars.
            %
            % All the lists of insts must have exactly one inst for
            % each var in the list of vars.

    ;       mode_error_in_callee(list(prog_var), list(mer_inst),
                pred_id, proc_id, list(mode_error_info))
            % Call to a predicate with initial argument insts for which mode
            % inference gave a mode error in the callee.

    ;       mode_error_bind_var(var_lock_reason, prog_var, mer_inst, mer_inst)
            % Attempt to bind a non-local variable inside a negated context,
            % or attempt to re-bind a variable in a parallel conjunct.

    ;       mode_error_non_local_lambda_var(prog_var, mer_inst)
            % Attempt to pass a live non-ground var as a non-local variable
            % to a lambda goal.

    ;       mode_error_unify_var_var(prog_var, prog_var, mer_inst, mer_inst)
            % Attempt to unify two free variables.

    ;       mode_error_unify_var_functor(prog_var, cons_id,
                list(prog_var), mer_inst, list(mer_inst))
            % Attempt to unify a free var with a functor containing
            % free arguments.

    ;       mode_error_unify_var_lambda(prog_var, mer_inst, mer_inst)
            % Some sort of error in attempt to unify a variable with lambda
            % expression.

    ;       mode_error_unify_var_multimode_pred(prog_var, pred_id,
                var_multimode_pred_error)
            % Some sort of error in attempt to take address of a multi-moded
            % predicate.

    ;       mode_error_conj(list(delayed_goal), schedule_culprit)
            % A conjunction contains one or more unscheduleable goals;
            % schedule_culprit gives the reason why they couldn't be scheduled.

    ;       mode_error_final_inst(int, prog_var, mer_inst, mer_inst,
                final_inst_error)
            % One of the head variables did not have the expected final inst
            % on exit from the proc.

    ;       purity_error_should_be_in_promise_purity_scope(
                negated_context_desc, prog_var)
            % The condition of an if-then-else or the body of a negation
            % contained an inst any non-local, but was not inside a
            % promise_purity scope.

    ;       purity_error_lambda_should_be_any(list(prog_var)).
            % A ground lambda term contains inst any non-locals, and is not
            % marked impure.

:- type negated_context_desc
    --->    if_then_else
    ;       negation.

:- type schedule_culprit
    --->    goal_itself_was_impure
    ;       goals_followed_by_impure_goal(hlds_goal)
    ;       conj_floundered.        % We have reached the end of a conjunction
                                    % and there were still delayed goals.

:- type final_inst_error
    --->    too_instantiated
    ;       not_instantiated_enough
    ;       wrongly_instantiated.   % A catchall for anything that does not
                                    % fit into the above two categories.

:- type mode_error_unify_rhs
    --->    error_at_var(prog_var)
    ;       error_at_functor(cons_id, list(prog_var))
    ;       error_at_lambda(list(prog_var), list(from_to_insts)).

:- type mode_error_info
    --->    mode_error_info(
                set_of_progvar,     % The variables which caused the error
                                    % (we will attempt to reschedule the goal
                                    % if one of these variables becomes
                                    % more instantiated).
                mode_error,         % The nature of the error.
                prog_context,       % Where the error occurred.
                mode_context        % Where the error occurred.
            ).

:- type mode_warning
    --->    cannot_succeed_var_var(prog_var, prog_var, mer_inst, mer_inst)
    ;       cannot_succeed_var_functor(prog_var, mer_inst, cons_id)
    ;       cannot_succeed_ground_occur_check(prog_var, cons_id).

:- type mode_warning_info
    --->    mode_warning_info(
                mode_warning,       % The nature of the error.
                prog_context,       % Where the error occurred.
                mode_context        % Where the error occurred.
            ).

%---------------------------------------------------------------------------%

    % Initialize the mode_context.
    %
:- pred mode_context_init(mode_context::out) is det.

    % Return an error for a predicate with no mode declarations
    % unless mode inference is enabled and the predicate is local.
    % XXX This predicate should be included in the types above.
    %
:- func maybe_report_error_no_modes(module_info, pred_id, pred_info)
    = list(error_spec).

    % Report an error for the case when two mode declarations
    % declare indistinguishable modes.
    % XXX This predicate should be included in the types above.
    %
:- func report_indistinguishable_modes_error(module_info, proc_id, proc_id,
    pred_id, pred_info) = error_spec.

:- type include_detism_on_modes
    --->    include_detism_on_modes
    ;       do_not_include_detism_on_modes.

    % Generate the inferred `mode' declarations for a list of pred_ids.
    % The include_detism_on_modes argument indicates whether or not
    % to write out determinism annotations on the modes. (It should only
    % be set to `include_detism_on_modes' _after_ determinism analysis.)
    %
:- pred report_mode_inference_messages_for_preds(module_info::in,
    include_detism_on_modes::in, list(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- func mode_decl_to_string(output_lang, proc_id, pred_info) = string.

:- func mode_error_info_to_spec(mode_info, mode_error_info) = error_spec.

:- func mode_warning_info_to_spec(mode_info, mode_warning_info) = error_spec.

:- func should_report_mode_warning_for_pred_origin(pred_origin) = bool.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_util.
:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module io.            % used only for a typeclass instance
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

mode_context_init(mode_context_uninitialized).

%---------------------------------------------------------------------------%

maybe_report_error_no_modes(ModuleInfo, PredId, PredInfo) = Specs :-
    pred_info_get_status(PredInfo, PredStatus),
    % XXX STATUS
    ( if PredStatus = pred_status(status_local) then
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, infer_modes, InferModesOpt),
        (
            InferModesOpt = yes,
            Specs = []
        ;
            InferModesOpt = no,
            pred_info_get_markers(PredInfo, Markers),
            ( if check_marker(Markers, marker_no_pred_decl) then
                % Generate an error_spec that prints nothing.
                % While we don't want the user to see the error message,
                % we need the severity_error to stop the compiler
                % from proceeding to process this predicate further.
                % For example, to determinism analysis, where it could
                % generate misleading errors about the determinism declaration
                % (added implicitly by the compiler) being wrong.
                % There is no risk of the compilation failing without
                % *any* error indication, since we generated an error message
                % when we added the marker.
                Msgs = []
            else
                PredDesc = describe_one_pred_name(ModuleInfo,
                    should_not_module_qualify, PredId),
                MainPieces = [words("Error: no mode declaration for")] ++
                    PredDesc ++ [suffix("."), nl],
                VerbosePieces =
                    [words("(Use"), quote("--infer-modes"),
                    words("to enable mode inference.)"), nl],
                Msgs =
                    [simple_msg(Context,
                        [always(MainPieces),
                        verbose_only(verbose_once, VerbosePieces)])]
            ),
            pred_info_get_context(PredInfo, Context),
            Spec = error_spec($pred, severity_error,
                phase_mode_check(report_in_any_mode), Msgs),
            Specs = [Spec]
        )
    else
        pred_info_get_context(PredInfo, Context),
        Pieces = [words("Error: no mode declaration for exported")] ++
            describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
            ++ [suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_mode_check(report_in_any_mode), Context, Pieces),
        Specs = [Spec]
    ).

%---------------------------------------------------------------------------%

report_indistinguishable_modes_error(ModuleInfo, OldProcId, NewProcId,
        PredId, PredInfo) = Spec :-
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, OldProcId, OldProcInfo),
    map.lookup(Procs, NewProcId, NewProcInfo),
    proc_info_get_context(OldProcInfo, OldContext),
    proc_info_get_context(NewProcInfo, NewContext),

    MainPieces = [words("In mode declarations for ")] ++
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
        ++ [suffix(":"), nl, words("error: duplicate mode declaration."), nl],
    VerbosePieces = [words("Modes"),
        words_quote(mode_decl_to_string(output_mercury, OldProcId, PredInfo)),
        words("and"),
        words_quote(mode_decl_to_string(output_mercury, NewProcId, PredInfo)),
        words("are indistinguishable.")],
    OldPieces = [words("Here is the conflicting mode declaration.")],
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(NewContext,
            [always(MainPieces), verbose_only(verbose_always, VerbosePieces)]),
        simple_msg(OldContext, [always(OldPieces)])]).

%---------------------------------------------------------------------------%

report_mode_inference_messages_for_preds(_, _, [], !Specs).
report_mode_inference_messages_for_preds(ModuleInfo, OutputDetism,
        [PredId | PredIds], !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if check_marker(Markers, marker_infer_modes) then
        ProcIds = pred_info_all_procids(PredInfo),
        pred_info_get_proc_table(PredInfo, Procs),
        report_mode_inference_messages_for_procs(ModuleInfo, OutputDetism,
            PredInfo, Procs, ProcIds, !Specs)
    else
        true
    ),
    report_mode_inference_messages_for_preds(ModuleInfo, OutputDetism,
        PredIds, !Specs).

    % Generate the inferred `mode' declarations for a list of proc_ids.
    %
:- pred report_mode_inference_messages_for_procs(module_info::in,
    include_detism_on_modes::in, pred_info::in, proc_table::in,
    list(proc_id)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_mode_inference_messages_for_procs(_, _, _, _, [], !Specs).
report_mode_inference_messages_for_procs(ModuleInfo, OutputDetism,
        PredInfo, Procs, [ProcId | ProcIds], !Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    map.lookup(Procs, ProcId, ProcInfo),
    ( if
        (
            % We always output `Inferred :- mode ...'
            proc_info_is_valid_mode(ProcInfo)
        ;
            % We only output `REJECTED :- mode ...'
            % if --verbose-errors is enabled
            VerboseErrors = yes
        )
    then
        Spec = report_mode_inference_message(ModuleInfo, OutputDetism,
            PredInfo, ProcInfo),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    report_mode_inference_messages_for_procs(ModuleInfo,
        OutputDetism, PredInfo, Procs, ProcIds, !Specs).

    % Return a description of the inferred mode declaration for the given
    % predicate or function.
    %
:- func report_mode_inference_message(module_info, include_detism_on_modes,
    pred_info, proc_info) = error_spec.

report_mode_inference_message(ModuleInfo, OutputDetism, PredInfo, ProcInfo)
        = Spec :-
    PredName = pred_info_name(PredInfo),
    Name = unqualified(PredName),
    pred_info_get_context(PredInfo, Context),
    PredArity = pred_info_orig_arity(PredInfo),
    some [!ArgModes, !MaybeDet] (
        proc_info_get_argmodes(ProcInfo, !:ArgModes),

        % We need to strip off the extra type_info arguments inserted at the
        % front by polymorphism.m - we only want the last `PredArity' of them.
        %
        list.length(!.ArgModes, NumArgModes),
        NumToDrop = NumArgModes - PredArity,
        ( if list.drop(NumToDrop, !ArgModes) then
            true
        else
            unexpected($pred, "list.drop failed")
        ),

        varset.init(VarSet),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        (
            OutputDetism = include_detism_on_modes,
            proc_info_get_inferred_determinism(ProcInfo, Detism),
            !:MaybeDet = yes(Detism)
        ;
            OutputDetism = do_not_include_detism_on_modes,
            !:MaybeDet = no
        ),
        ( if proc_info_is_valid_mode(ProcInfo) then
            Verb = "Inferred"
        else
            Verb = "REJECTED",
            % Replace the final insts with dummy insts '...', since they
            % won't be valid anyway -- they are just the results of whatever
            % partial inference we did before detecting the error.
            mode_list_get_initial_insts(ModuleInfo, !.ArgModes, InitialInsts),
            DummyInst = defined_inst(user_inst(unqualified("..."), [])),
            list.duplicate(PredArity, DummyInst, FinalInsts),
            !:ArgModes = list.map(func(I - F) = from_to_mode(I, F),
                assoc_list.from_corresponding_lists(InitialInsts, FinalInsts)),
            % Likewise delete the determinism.
            !:MaybeDet = no
        ),
        strip_builtin_qualifiers_from_mode_list(!ArgModes),
        (
            PredOrFunc = pf_predicate,
            MaybeWithInst = maybe.no,
            Detail = mercury_pred_mode_decl_to_string(output_debug, VarSet,
                Name, !.ArgModes, MaybeWithInst, !.MaybeDet)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(!.ArgModes, FuncArgModes, RetMode),
            Detail = mercury_func_mode_decl_to_string(output_debug, VarSet,
                Name, FuncArgModes, RetMode, !.MaybeDet)
        ),
        Pieces = [words(Verb), words(Detail), nl],
        Spec = conditional_spec($pred, inform_inferred_modes, yes,
            severity_informational, phase_mode_check(report_in_any_mode),
            [simplest_msg(Context, Pieces)])
    ).

%---------------------------------------------------------------------------%

mode_decl_to_string(Lang, ProcId, PredInfo) = String :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name0 = pred_info_name(PredInfo),
    Name = unqualified(Name0),
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    proc_info_get_declared_determinism(ProcInfo, MaybeDet),
    varset.init(InstVarSet),
    strip_builtin_qualifiers_from_mode_list(Modes0, Modes),
    String = mercury_mode_subdecl_to_string(Lang, PredOrFunc,
        InstVarSet, Name, Modes, MaybeDet).

%---------------------------------------------------------------------------%

mode_error_info_to_spec(ModeInfo0, ModeErrorInfo) = Spec :-
    some [!ModeInfo] (
        !:ModeInfo = ModeInfo0,
        ModeErrorInfo = mode_error_info(_, ModeError, Context, ModeContext),
        mode_info_set_context(Context, !ModeInfo),
        mode_info_set_mode_context(ModeContext, !ModeInfo),
        Spec = mode_error_to_spec(!.ModeInfo, ModeError)
    ).

:- func mode_error_to_spec(mode_info, mode_error) = error_spec.

mode_error_to_spec(ModeInfo, ModeError) = Spec :-
    (
        ModeError = mode_error_disj(MergeContext, MergeErrors),
        Spec = mode_error_disj_to_spec(ModeInfo, MergeContext, MergeErrors)
    ;
        ModeError = mode_error_par_conj(MergeErrors),
        Spec = mode_error_par_conj_to_spec(ModeInfo, MergeErrors)
    ;
        ModeError = mode_error_higher_order_pred_var(PredOrFunc, Var, Inst,
            Arity),
        Spec = mode_error_higher_order_pred_var_to_spec(ModeInfo, PredOrFunc,
            Var, Inst, Arity)
    ;
        ModeError = mode_error_poly_unify(Var, Inst),
        Spec = mode_error_poly_unify_to_spec(ModeInfo, Var, Inst)
    ;
        ModeError = mode_error_var_is_live(Var),
        Spec = mode_error_var_is_live_to_spec(ModeInfo, Var)
    ;
        ModeError = mode_error_var_has_inst(Var, InstA, InstB, MaybeMultiMode),
        Spec = mode_error_var_has_inst_to_spec(ModeInfo, Var, InstA, InstB,
            MaybeMultiMode)
    ;
        ModeError = mode_error_unify_pred(Var, RHS, Type, PredOrFunc),
        Spec = mode_error_unify_pred_to_spec(ModeInfo, Var, RHS, Type,
            PredOrFunc)
    ;
        ModeError = mode_error_implied_mode(Var, InstA, InstB),
        Spec = mode_error_implied_mode_to_spec(ModeInfo, Var, InstA, InstB)
    ;
        ModeError = mode_error_no_mode_decl,
        Spec = mode_error_no_mode_decl_to_spec(ModeInfo)
    ;
        ModeError = mode_error_bind_var(Reason, Var, InstA, InstB),
        Spec = mode_error_bind_var_to_spec(ModeInfo, Reason, Var,
            InstA, InstB)
    ;
        ModeError = mode_error_non_local_lambda_var(Var, Inst),
        Spec = mode_error_non_local_lambda_var_to_spec(ModeInfo, Var, Inst)
    ;
        ModeError = mode_error_unify_var_var(VarA, VarB, InstA, InstB),
        Spec = mode_error_unify_var_var_to_spec(ModeInfo, VarA, VarB,
            InstA, InstB)
    ;
        ModeError = mode_error_unify_var_lambda(VarA, InstA, InstB),
        Spec = mode_error_unify_var_lambda_to_spec(ModeInfo, VarA,
            InstA, InstB)
    ;
        ModeError = mode_error_unify_var_multimode_pred(VarA, PredId,
            MultiModeError),
        Spec = mode_error_unify_var_multimode_pred_to_spec(ModeInfo, VarA,
            PredId, MultiModeError)
    ;
        ModeError = mode_error_unify_var_functor(Var, Name, Args, Inst,
            ArgInsts),
        Spec = mode_error_unify_var_functor_to_spec(ModeInfo, Var, Name,
            Args, Inst, ArgInsts)
    ;
        ModeError = mode_error_conj(Errors, Culprit),
        Spec = mode_error_conj_to_spec(ModeInfo, Errors, Culprit)
    ;
        ModeError = mode_error_no_matching_mode(Vars, Insts, InitialInsts),
        Spec = mode_error_no_matching_mode_to_spec(ModeInfo, Vars, Insts,
            InitialInsts)
    ;
        ModeError = mode_error_in_callee(Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors),
        Spec = mode_error_in_callee_to_spec(ModeInfo, Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors)
    ;
        ModeError = mode_error_final_inst(ArgNum, Var, VarInst, Inst, Reason),
        Spec = mode_error_final_inst_to_spec(ModeInfo, ArgNum, Var, VarInst,
            Inst, Reason)
    ;
        ModeError = purity_error_should_be_in_promise_purity_scope(NegCtxt,
            Var),
        Spec = purity_error_should_be_in_promise_purity_scope_to_spec(NegCtxt,
            ModeInfo, Var)
    ;
        ModeError = purity_error_lambda_should_be_any(Vars),
        Spec = purity_error_lambda_should_be_any_to_spec(ModeInfo, Vars)
    ).

mode_warning_info_to_spec(!.ModeInfo, Warning) = Spec :-
    Warning = mode_warning_info(ModeWarning, Context, ModeContext),
    mode_info_set_context(Context, !ModeInfo),
    mode_info_set_mode_context(ModeContext, !ModeInfo),
    (
        ModeWarning = cannot_succeed_var_var(VarA, VarB, InstA, InstB),
        Spec = mode_warning_cannot_succeed_var_var(!.ModeInfo, VarA, VarB,
            InstA, InstB)
    ;
        ModeWarning = cannot_succeed_var_functor(Var, Inst, ConsId),
        Spec = mode_warning_cannot_succeed_var_functor(!.ModeInfo,
            Var, Inst, ConsId)
    ;
        ModeWarning = cannot_succeed_ground_occur_check(Var, ConsId),
        Spec = mode_warning_cannot_succeed_ground_occur_check(!.ModeInfo,
            Var, ConsId)
    ).

%---------------------------------------------------------------------------%

:- func mode_error_conj_to_spec(mode_info, list(delayed_goal),
    schedule_culprit) = error_spec.

mode_error_conj_to_spec(ModeInfo, Errors, Culprit) = Spec :-
    mode_info_get_context(ModeInfo, Context),
    (
        Errors = [],
        unexpected($pred, "no errors")
    ;
        Errors = [Error],
        Msgs1 = mode_error_conjunct_to_msgs(Context, ModeInfo, Error)
    ;
        Errors = [_, _ | _],
        % If there is more than one error, we use the setting of
        % --verbose-errors to decide between reporting just one and
        % reporting them all. Unfortunately, We can't use the
        % verbose_and_nonverbose functor of the error_msg_component type
        % to package up the two cases, because we need to package up
        % multiple messages, each with its own context.
        list.filter(is_error_important, Errors, ImportantErrors, OtherErrors),
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = no,
            % In the absence of --verbose-errors, report only one error.
            % We prefer that this be an important error.
            (
                ImportantErrors = [FirstImportantError | _],
                ConjMsgs = mode_error_conjunct_to_msgs(Context, ModeInfo,
                    FirstImportantError)
            ;
                ImportantErrors = [],
                (
                    OtherErrors = [FirstOtherError | _],
                    ConjMsgs = mode_error_conjunct_to_msgs(Context, ModeInfo,
                        FirstOtherError)
                ;
                    OtherErrors = [],
                    unexpected($pred, "no errors of any kind")
                )
            ),
            % MoreMsg is there to indicate that --verbose-errors would yield
            % more information.
            MoreMsg = simple_msg(Context, [verbose_only(verbose_always, [])]),
            Msgs1 = ConjMsgs ++ [MoreMsg]
        ;
            VerboseErrors = yes,
            Preamble = mode_info_context_preamble(ModeInfo),
            ConjPieces = [words("mode error in conjunction. The next"),
                fixed(int_to_string(list.length(Errors))),
                words("error messages indicate"),
                words("possible causes of this error.")],
            Msgs1Start = [simplest_msg(Context, Preamble ++ ConjPieces)],
            Msgs1Rest0 = list.map(
                mode_error_conjunct_to_msgs(Context, ModeInfo),
                ImportantErrors ++ OtherErrors),
            Msgs1Rest = list.map(prefix_with_blank_line(Context), Msgs1Rest0),
            Msgs1 = Msgs1Start ++ list.condense(Msgs1Rest)
        )
    ),

    % If the goal(s) couldn't be scheduled because we couldn't reorder things
    % past an impure goal, then report that.
    (
        Culprit = conj_floundered,
        % We have already reported everything we can.
        Msgs2 = []
    ;
        Culprit = goal_itself_was_impure,
        Pieces = [words("The goal could not be reordered,"),
            words("because it was impure.")],
        Msgs2 = [simplest_msg(Context, Pieces)]
    ;
        Culprit = goals_followed_by_impure_goal(ImpureGoal),
        ImpureGoal = hlds_goal(_, ImpureGoalInfo),
        ImpureGoalContext = goal_info_get_context(ImpureGoalInfo),
        Pieces1 = [words("The goal could not be reordered,"),
            words("because it was followed by an impure goal.")],
        Pieces2 = [words("This is the location of the impure goal.")],
        Msgs2 = [simplest_msg(Context, Pieces1),
            simplest_msg(ImpureGoalContext, Pieces2)]
    ),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Msgs1 ++ Msgs2).

:- func prefix_with_blank_line(prog_context, list(error_msg))
    = list(error_msg).

prefix_with_blank_line(Context, Msgs) = [BlankMsg | Msgs] :-
    BlankMsg = simplest_msg(Context, [blank_line]).

:- pred is_error_important(delayed_goal::in) is semidet.

is_error_important(Error) :-
    Error = delayed_goal(_, mode_error_info(_, ModeError, _, ModeContext), _),
    ( if
        % An error is important unless it is a non-explicit unification,
        % i.e. a head unification or a call argument unification.
        ModeContext = mode_context_unify(unify_context(UnifyContext, _), _),
        UnifyContext \= umc_explicit,

        % Except that errors in lambda goals are important even if the
        % unification that creates the lambda goal is an implicit one.
        ModeError \= mode_error_non_local_lambda_var(_, _)
    then
        fail
    else
        true
    ).

:- func mode_error_conjunct_to_msgs(prog_context, mode_info, delayed_goal)
    = list(error_msg).

mode_error_conjunct_to_msgs(Context, !.ModeInfo, DelayedGoal) = Msgs :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),

    DelayedGoal = delayed_goal(Vars, Error, Goal),
    Error = mode_error_info(_, ModeError, ErrorContext, ModeContext),
    mode_info_set_context(ErrorContext, !ModeInfo),
    mode_info_set_mode_context(ModeContext, !ModeInfo),

    SubSpec0 = mode_error_to_spec(!.ModeInfo, ModeError),
    extract_spec_msgs(Globals, SubSpec0, SubMsgs),

    globals.lookup_bool_option(Globals, debug_modes, DebugModes),
    (
        DebugModes = no,
        Msgs = SubMsgs
    ;
        DebugModes = yes,
        set_of_var.to_sorted_list(Vars, VarList),
        mode_info_get_varset(!.ModeInfo, VarSet),
        VarNames = mercury_vars_to_name_only(VarSet, VarList),
        Pieces1 = [words("Floundered goal, waiting on {"),
            words(VarNames), words("}:"), nl],
        Msg1 = simplest_msg(Context, Pieces1),
        % XXX Shouldn't we check debug_modes_verbose instead of very_verbose?
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        (
            VeryVerbose = no,
            Msgs = [Msg1] ++ SubMsgs
        ;
            VeryVerbose = yes,
            Pieces2 =
                [always([nl]),
                'new print_anything'(
                    write_indented_goal(ModuleInfo, VarSet, Goal))],
            Msg2 = error_msg(no, do_not_treat_as_first, 0, Pieces2),
            Msgs = [Msg1, Msg2] ++ SubMsgs
        )
    ).

:- type write_indented_goal
    --->    write_indented_goal(module_info, prog_varset, hlds_goal).

:- instance error_util.print_anything(write_indented_goal) where [
    ( print_anything(write_indented_goal(ModuleInfo, VarSet, Goal), !IO) :-
        io.write_string("\t\t", !IO),
        module_info_get_globals(ModuleInfo, Globals),
        OutInfo = init_hlds_out_info(Globals, output_debug),
        write_goal(OutInfo, ModuleInfo, VarSet, print_name_only, 2,
            ".\n", Goal, !IO)
    )
].

%---------------------------------------------------------------------------%

:- func mode_error_disj_to_spec(mode_info, merge_context, merge_errors)
    = error_spec.

mode_error_disj_to_spec(ModeInfo, MergeContext, MergeErrors) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    MainPieces = [words("mode mismatch in "),
        words(merge_context_to_string(MergeContext)), suffix("."), nl],
    MergeMsgLists = list.map(merge_error_to_msgs(ModeInfo, Context, yes),
        MergeErrors),
    list.condense(MergeMsgLists, MergeMsgs),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simplest_msg(Context, Preamble ++ MainPieces) | MergeMsgs]).

:- func mode_error_par_conj_to_spec(mode_info, merge_errors) = error_spec.

mode_error_par_conj_to_spec(ModeInfo, MergeErrors) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    MainPieces = [words("mode error: mutually exclusive bindings"),
        words("in parallel conjunction."),
        words("(The current implementation does not permit"),
        words("parallel conjunctions to fail.)"), nl],
    MergeMsgLists = list.map(merge_error_to_msgs(ModeInfo, Context, no),
        MergeErrors),
    list.condense(MergeMsgLists, MergeMsgs),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simplest_msg(Context, Preamble ++ MainPieces) | MergeMsgs]).

:- func merge_context_to_string(merge_context) = string.

merge_context_to_string(merge_disj) = "disjunction".
merge_context_to_string(merge_if_then_else) = "if-then-else".
merge_context_to_string(merge_stm_atomic) = "atomic".

:- func merge_error_to_msgs(mode_info, prog_context, bool, merge_error)
    = list(error_msg).

merge_error_to_msgs(ModeInfo, MainContext, IsDisjunctive, MergeError) = Msgs :-
    MergeError = merge_error(Var, ContextsInsts0),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_varset(ModeInfo, VarSet),
    VarNamePiece = quote(mercury_var_to_name_only(VarSet, Var)),
    list.sort(ContextsInsts0, ContextsInsts),
    count_ground_insts(ModuleInfo, ContextsInsts,
        0, NumGroundInsts, 0, NumAllInsts),
    ( if
        IsDisjunctive = yes,
        NumGroundInsts < NumAllInsts,
        NumGroundInsts * 2 > NumAllInsts
    then
        % More than half the insts are ground, so it is likely that they
        % were *all* intended to be ground, but not all actually *are* ground,
        % which is likely to be the bug.
        CommonPieces = [words("The variable"), VarNamePiece,
            words("is ground"), words("in"), int_fixed(NumGroundInsts),
            words("out of"), int_fixed(NumAllInsts), words("branches."), nl],
        VerbosePieces =
            [words("It has the following instantiation states."), nl],
        ( if NumAllInsts - NumGroundInsts > 1 then
            NonVerbosePieces =
                [words("It has non-ground instantiation states"),
                words("in the following branches."), nl]
        else
            % The message for the non-ground branch will *say*
            % it is for such a branch.
            NonVerbosePieces = []
        ),
        VarMsg = simple_msg(MainContext,
            [always(CommonPieces),
            verbose_and_nonverbose(VerbosePieces, NonVerbosePieces)]),
        InstMsgs = list.map(
            report_inst_in_context(ModeInfo, VarNamePiece,
                report_is_ground_v_and_nonv),
            ContextsInsts),
        Msgs = [VarMsg | InstMsgs]
    else
        VarPieces = [words("The variable"), VarNamePiece,
            words("has the following instantiation states."), nl],
        VarMsg = simple_msg(MainContext, [always(VarPieces)]),
        InstMsgs = list.map(
            report_inst_in_context(ModeInfo, VarNamePiece,
                dont_report_is_ground),
            ContextsInsts),
        Msgs = [VarMsg | InstMsgs]
    ).

:- pred count_ground_insts(module_info::in,
    assoc_list(prog_context, mer_inst)::in,
    int::in, int::out, int::in, int::out) is det.

count_ground_insts(_ModuleInfo, [], !NumGroundInsts, !NumAllInsts).
count_ground_insts(ModuleInfo, [ContextInst | ContextsInsts],
        !NumGroundInsts, !NumAllInsts) :-
    ContextInst = _Context - Inst,
    ( if inst_is_ground(ModuleInfo, Inst) then
        !:NumGroundInsts = !.NumGroundInsts + 1
    else
        true
    ),
    !:NumAllInsts = !.NumAllInsts + 1,
    count_ground_insts(ModuleInfo, ContextsInsts,
        !NumGroundInsts, !NumAllInsts).

%---------------------------------------------------------------------------%

:- func mode_error_bind_var_to_spec(mode_info, var_lock_reason,
    prog_var, mer_inst, mer_inst) = error_spec.

mode_error_bind_var_to_spec(ModeInfo, Reason, Var, VarInst, Inst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        Reason = var_lock_negation,
        ReasonStr = "attempt to bind a non-local variable inside a negation."
    ;
        Reason = var_lock_if_then_else,
        ReasonStr = "attempt to bind a non-local variable" ++
            " inside the condition of an if-then-else."
    ;
        Reason = var_lock_lambda(PredOrFunc),
        PredOrFuncS = prog_out.pred_or_func_to_str(PredOrFunc),
        ReasonStr = "attempt to bind a non-local variable inside" ++
            " a " ++ PredOrFuncS ++ " lambda goal."
    ;
        Reason = var_lock_trace_goal,
        ReasonStr = "attempt to bind a non-local variable inside a trace goal."
    ;
        Reason = var_lock_atomic_goal,
        ReasonStr = "attempt to bind outer state variables inside an " ++
            "atomic goal."
    ;
        Reason = var_lock_par_conj,
        ReasonStr = "attempt to bind a non-local variable" ++
            " inside more than one parallel conjunct."
    ),
    MainPieces = [words("scope error:"), words(ReasonStr), nl,
        words("Variable"), quote(mercury_var_to_name_only(VarSet, Var)) |
        has_inst_expected_inst_was(ModeInfo, VarInst, Inst)],
    (
        Reason = var_lock_negation,
        VerbosePieces =
            [words("A negation is only allowed to bind variables"),
            words("which are local to the negation, i.e. those which are"),
            words("implicitly existentially quantified"),
            words("inside the scope of the negation."), nl]
    ;
        Reason = var_lock_if_then_else,
        VerbosePieces =
            [words("The condition of an if-then-else is only"),
            words("allowed to bind variables which are local to the"),
            words("condition or which occur only in the condition"),
            words("and the"), quote("then"), words("part."), nl]
    ;
        Reason = var_lock_lambda(_),
        VerbosePieces =
            [words("A lambda goal is only allowed to bind"),
            words("its arguments and variables local to the "),
            words("lambda expression."), nl]
    ;
        Reason = var_lock_trace_goal,
        VerbosePieces =
            [words("A trace goal is only allowed to bind variables"),
            words("which are local to the trace goal."), nl]
    ;
        Reason = var_lock_atomic_goal,
        VerbosePieces =
            [words("An atomic goal may not use the state variables"),
            words("belonging to the outer scope."), nl]
    ;
        Reason = var_lock_par_conj,
        VerbosePieces =
            [words("A nonlocal variable of a parallel conjunction"),
            words("may be bound in at most one conjunct."), nl]
    ),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces),
            verbose_only(verbose_always, VerbosePieces)])]).

%---------------------------------------------------------------------------%

:- func mode_error_non_local_lambda_var_to_spec(mode_info, prog_var, mer_inst)
    = error_spec.

mode_error_non_local_lambda_var_to_spec(ModeInfo, Var, VarInst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error: variable"),
        quote(mercury_var_to_name_only(VarSet, Var)) |
        has_instantiatedness(ModeInfo, VarInst, ",")] ++
        [words("expected instantiatedness for non-local variables"),
        words("of lambda goals is"), quote("ground"), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_in_callee_to_spec(mode_info, list(prog_var), list(mer_inst),
    pred_id, proc_id, list(mode_error_info)) = error_spec.

mode_error_in_callee_to_spec(!.ModeInfo, Vars, Insts,
        CalleePredId, CalleeProcId, CalleeModeErrors) = Spec :-
    Preamble = mode_info_context_preamble(!.ModeInfo),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_varset(!.ModeInfo, VarSet),
    (
        Vars = [],
        unexpected($pred, "Vars = []")
    ;
        Vars = [Var],
        MainPieces = [words("mode error: argument"),
            quote(mercury_var_to_name_only(VarSet, Var)),
            words("has the following inst:"), nl_indent_delta(1)]
    ;
        Vars = [_, _ | _],
        MainPieces = [words("mode error: arguments"),
            quote(mercury_vars_to_name_only(VarSet, Vars)),
            words("have the following insts:"), nl_indent_delta(1)]
    ),
    NoMatchPieces = inst_list_to_sep_lines(!.ModeInfo, Insts) ++
        [words("which does not match any of the valid modes for")],

    CalleePredIdPieces = describe_one_pred_name(ModuleInfo,
        should_module_qualify, CalleePredId),
    VerboseCalleePieces = [words("the callee"), prefix("(")] ++
        CalleePredIdPieces ++
        [suffix(")"), nl, words("because of the following error."), nl],
    VerbosePieces = MainPieces ++ NoMatchPieces ++ VerboseCalleePieces,
    NonVerboseCalleePieces =
        [words("the callee, because of the following error."), nl],
    NonVerbosePieces = MainPieces ++ NoMatchPieces ++ NonVerboseCalleePieces,

    InitMsg = simple_msg(Context,
        [always(Preamble),
        verbose_and_nonverbose(VerbosePieces, NonVerbosePieces)]),

    (
        CalleeModeErrors = [First | _],
        First = mode_error_info(_, CalleeModeError,
            CalleeContext, CalleeModeContext),
        mode_info_set_pred_id(CalleePredId, !ModeInfo),
        mode_info_set_proc_id(CalleeProcId, !ModeInfo),
        mode_info_set_context(CalleeContext, !ModeInfo),
        mode_info_set_mode_context(CalleeModeContext, !ModeInfo),
        CalleeModeErrorSpec0 = mode_error_to_spec(!.ModeInfo, CalleeModeError),
        module_info_get_globals(ModuleInfo, Globals),
        extract_spec_msgs(Globals, CalleeModeErrorSpec0, LaterMsgs0),
        (
            LaterMsgs0 = [],
            LaterMsgs = []
        ;
            LaterMsgs0 = [LaterHead0 | LaterTail],
            (
                LaterHead0 = simplest_msg(LaterContext, Pieces),
                LaterHead = error_msg(yes(LaterContext), treat_as_first,
                    0, [always(Pieces)])
            ;
                LaterHead0 = simple_msg(LaterContext, Components),
                LaterHead = error_msg(yes(LaterContext), treat_as_first,
                    0, Components)
            ;
                LaterHead0 = error_msg(MaybeLaterContext, _, Indent,
                    Components),
                LaterHead = error_msg(MaybeLaterContext, treat_as_first,
                    Indent, Components)
            ),
            LaterMsgs = [LaterHead | LaterTail]
        ),
        Spec = error_spec($pred, severity_error,
            phase_mode_check(report_in_any_mode), [InitMsg | LaterMsgs])
    ;
        CalleeModeErrors = [],
        unexpected($pred, "no error")
    ).

%---------------------------------------------------------------------------%

:- func mode_error_no_matching_mode_to_spec(mode_info, list(prog_var),
    list(mer_inst), list(list(mer_inst))) = error_spec.

mode_error_no_matching_mode_to_spec(ModeInfo, Vars, Insts, InitialInsts)
        = Spec :-
    list.length(Vars, NumVars),
    list.length(Insts, NumInsts),
    expect(unify(NumVars, NumInsts), $pred, "NumVars != NumInsts"),

    PrefixPieces = mode_info_context_preamble(ModeInfo) ++
        [words("mode error:")],
    mode_info_get_mode_context(ModeInfo, ModeContext),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    (
        ModeContext = mode_context_call(ModeCallId, _),
        (
            ModeCallId = mode_call_generic(GenericCallId),
            CallId = generic_call_id(GenericCallId),
            (
                GenericCallId = gcid_higher_order(_, PredOrFunc, _)
            ;
                GenericCallId = gcid_class_method(_, PFSymNameArity),
                PFSymNameArity = pf_sym_name_arity(PredOrFunc, _, _)
            ;
                ( GenericCallId = gcid_event_call(_)
                ; GenericCallId = gcid_cast(_)
                ),
                PredOrFunc = pf_predicate
            ),
            % XXX Setting NumExtra to 0 means that we do not separate out
            % any arguments added by polymorphism.m. Since most higher order
            % values are monomorphic, this should not be too much of a loss.
            % If and when it becomes one, this should be fixed.
            NumExtra = 0
        ;
            ModeCallId = mode_call_plain(PredId),
            mode_info_get_pf_sym_name_arity(ModeInfo, PredId, PFSymNameArity),
            CallId = plain_call_id(PFSymNameArity),

            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_orig_arity(PredInfo, OrigArity),
            PredOrFunc = pred_info_is_pred_or_func(PredInfo),
            NumExtra = NumVars - OrigArity
        )
    ;
        ( ModeContext = mode_context_unify(_, _)
        ; ModeContext = mode_context_uninitialized
        ),
        unexpected($pred, "invalid context")
    ),
    ( if NumExtra > 0 then
        % The callee's argument list contains extra typeinfo and/or
        % typeclass_info arguments added by polymorphism.m. Usually,
        % these extra arguments are all already ground, which means that
        % they cannot be insufficiently instantiated. And since users cannot
        % require them to have any inst more specific than ground, they
        % cannot be the cause of any mode errors. This is why we do not
        % report their instantiations.
        %
        % In the unusual case that some extra variable is *not* ground,
        % they *could* possibly be the cause of a mode error, so we *do* list
        % their instantiation states, but we do so separately, in an effort
        % to avoid confusing users.
        list.det_split_list(NumExtra, Vars, ExtraVars, UserVars),
        list.det_split_list(NumExtra, Insts, ExtraInsts, UserInsts),
        UserArgPieces = [words("argument")],
        UserVarInstPieces = arg_inst_mismatch_pieces(ModeInfo, UserArgPieces,
            PredOrFunc, UserVars, UserInsts),
        ( if list.all_true(inst_is_ground(ModuleInfo), ExtraInsts) then
            VarListInstPieces = UserVarInstPieces
        else
            ExtraArgPieces = [words("the compiler-generated argument")],
            ExtraVarInstPieces = arg_inst_mismatch_pieces(ModeInfo,
                ExtraArgPieces, pf_predicate, ExtraVars, ExtraInsts),
            VarListInstPieces =  ExtraVarInstPieces ++ UserVarInstPieces
        )
    else
        UserArgPieces = [words("argument")],
        VarListInstPieces = arg_inst_mismatch_pieces(ModeInfo, UserArgPieces,
            PredOrFunc, Vars, Insts)
    ),
    NoMatchPieces =
        [words("which does not match any of the modes for"),
        words(call_id_to_string(CallId)), suffix("."), nl],
    mode_info_get_var_types(ModeInfo, VarTypes),
    construct_argnum_var_type_inst_tuples(VarTypes, Vars, Insts, 1, ArgTuples),
    find_satisfied_initial_insts_in_procs(ModuleInfo, ArgTuples, InitialInsts,
        0, map.init, ArgNumMatchedProcs),
    report_any_never_matching_args(ModeInfo, ArgNumMatchedProcs, NumExtra,
        ArgTuples, BadArgPieces),
    Pieces = PrefixPieces ++ VarListInstPieces ++ NoMatchPieces ++
        BadArgPieces,
    mode_info_get_context(ModeInfo, Context),
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Pieces).

%---------------------%

:- func arg_inst_mismatch_pieces(mode_info, list(format_component),
    pred_or_func, list(prog_var), list(mer_inst)) = list(format_component).

arg_inst_mismatch_pieces(ModeInfo, ArgPieces, PredOrFunc, Vars, Insts)
        = Pieces :-
    mode_info_get_varset(ModeInfo, VarSet),
    (
        Vars = [],
        Pieces = []
    ;
        Vars = [HeadVar | TailVars],
        (
            PredOrFunc = pf_predicate,
            (
                TailVars = [],
                Pieces = ArgPieces ++
                    [quote(mercury_var_to_name_only(VarSet, HeadVar)),
                    words("has the following inst:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            ;
                TailVars = [_ | _],
                Pieces = ArgPieces ++ [suffix("s"),
                    quote(mercury_vars_to_name_only(VarSet, Vars)),
                    words("have the following insts:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            )
        ;
            PredOrFunc = pf_function,
            list.det_split_last(Vars, ArgVars, ReturnVar),
            (
                ArgVars = [],
                Pieces = [words("the return value"),
                    quote(mercury_var_to_name_only(VarSet, ReturnVar)),
                    words("has the following inst:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            ;
                (
                    ArgVars = [_],
                    SuffixPieces = []
                ;
                    ArgVars = [_, _ | _],
                    SuffixPieces = [suffix("s")]
                ),
                Pieces = ArgPieces ++ SuffixPieces ++
                    [quote(mercury_vars_to_name_only(VarSet, ArgVars)),
                    words("and the return value"),
                    quote(mercury_var_to_name_only(VarSet, ReturnVar)),
                    words("have the following insts:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            )
        )
    ).

%---------------------%

:- type argnum_var_type_inst
    --->    argnum_var_type_inst(int, prog_var, mer_type, mer_inst).

:- pred construct_argnum_var_type_inst_tuples(vartypes::in,
    list(prog_var)::in, list(mer_inst)::in, int::in,
    list(argnum_var_type_inst)::out) is det.

construct_argnum_var_type_inst_tuples(_VarTypes, [], [], _ArgNum, []).
construct_argnum_var_type_inst_tuples(_VarTypes, [], [_ | _], _ArgNum, _) :-
    unexpected($pred, "length mismatch").
construct_argnum_var_type_inst_tuples(_VarTypes, [_ | _], [], _ArgNum, _) :-
    unexpected($pred, "length mismatch").
construct_argnum_var_type_inst_tuples(VarTypes, [Var | Vars], [Inst | Insts],
        ArgNum, [ArgTuple | ArgTuples]) :-
    lookup_var_type(VarTypes, Var, Type),
    ArgTuple = argnum_var_type_inst(ArgNum, Var, Type, Inst),
    construct_argnum_var_type_inst_tuples(VarTypes, Vars, Insts,
        ArgNum + 1, ArgTuples).

:- pred find_satisfied_initial_insts_in_procs(module_info::in,
    list(argnum_var_type_inst)::in, list(list(mer_inst))::in, int::in,
    multi_map(int, int)::in, multi_map(int, int)::out) is det.

find_satisfied_initial_insts_in_procs(_ModuleInfo, _ArgTuples,
        [], _ProcNum, !ArgNumMatchedProcs).
find_satisfied_initial_insts_in_procs(ModuleInfo, ArgTuples,
        [Proc | Procs], ProcNum, !ArgNumMatchedProcs) :-
    find_satisfied_initial_insts_in_proc(ModuleInfo, ArgTuples, Proc,
        ProcNum, !ArgNumMatchedProcs),
    find_satisfied_initial_insts_in_procs(ModuleInfo, ArgTuples,
        Procs, ProcNum + 1, !ArgNumMatchedProcs).

:- pred find_satisfied_initial_insts_in_proc(module_info::in,
    list(argnum_var_type_inst)::in, list(mer_inst)::in, int::in,
    multi_map(int, int)::in, multi_map(int, int)::out) is det.

find_satisfied_initial_insts_in_proc(_ModuleInfo, [], [],
        _ProcNum, !ArgNumMatchedProcs).
find_satisfied_initial_insts_in_proc(_ModuleInfo, [], [_ | _],
        _ProcNum, !ArgNumMatchedProcs) :-
    unexpected($pred, "length mismatch").
find_satisfied_initial_insts_in_proc(_ModuleInfo, [_ | _], [],
        _ProcNum, !ArgNumMatchedProcs) :-
    unexpected($pred, "length mismatch").
find_satisfied_initial_insts_in_proc(ModuleInfo,
        [ArgTuple | ArgTuples], [ProcInitialInst | ProcInitialInsts],
        ProcNum, !ArgNumMatchedProcs) :-
    ArgTuple = argnum_var_type_inst(ArgNum, _Var, VarType, VarInst),
    ( if
        inst_matches_initial_sub(VarInst, ProcInitialInst, VarType,
            ModuleInfo, _UpdatedModuleInfo, map.init, _Subst)
    then
        multi_map.add(ArgNum, ProcNum, !ArgNumMatchedProcs)
    else
        true
    ),
    find_satisfied_initial_insts_in_proc(ModuleInfo,
        ArgTuples, ProcInitialInsts, ProcNum, !ArgNumMatchedProcs).

:- pred report_any_never_matching_args(mode_info::in,
    multi_map(int, int)::in, int::in, list(argnum_var_type_inst)::in,
    list(format_component)::out) is det.

report_any_never_matching_args(_ModeInfo, _ArgNumMatchedProcs, _NumExtra,
        [], []).
report_any_never_matching_args(ModeInfo, ArgNumMatchedProcs, NumExtra,
        [ArgTuple | ArgTuples], BadArgPieces) :-
    report_any_never_matching_args(ModeInfo, ArgNumMatchedProcs, NumExtra,
        ArgTuples, BadArgPiecesTail),
    ArgTuple = argnum_var_type_inst(ArgNum, Var, _VarType, VarInst),
    ( if map.search(ArgNumMatchedProcs, ArgNum, _) then
        BadArgPieces = BadArgPiecesTail
    else
        ( if ArgNum =< NumExtra then
            ArgNumPieces = [words("The compiler-generated"), nth_fixed(ArgNum),
                words("argument")]
        else
            ArgNumPieces = [words("The"), nth_fixed(ArgNum - NumExtra),
                words("argument")]
        ),
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        ( if inst_contains_higher_order(ModuleInfo, VarInst) then
            HOPieces = [words("(For higher order insts like this,"),
                words("the mismatch is sometimes caused by"),
                words("the arity of the predicate or function"),
                words("being different in the inst than in the type.)"), nl]
        else
            HOPieces = []
        ),
        mode_info_get_varset(ModeInfo, VarSet),
        BadArgPieces = ArgNumPieces ++
            [quote(mercury_var_to_name_only(VarSet, Var)),
            words("has inst")] ++
            report_inst(ModeInfo, quote_short_inst, [suffix(",")],
                [nl_indent_delta(1)], [suffix(","), nl_indent_delta(-1)],
                VarInst) ++
            [words("which does not match any of those modes."), nl] ++
            HOPieces ++
            BadArgPiecesTail
    ).

%---------------------------------------------------------------------------%

:- func mode_error_higher_order_pred_var_to_spec(mode_info, pred_or_func,
    prog_var, mer_inst, arity) = error_spec.

mode_error_higher_order_pred_var_to_spec(ModeInfo, PredOrFunc, Var, VarInst,
        Arity) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    % Don't let the specification of the expected arity be broken up,
    % since that would make the error message harder to read.
    (
        PredOrFunc = pf_predicate,
        ExpectingPieces = [words("expecting higher-order pred inst"),
            fixed("of arity " ++ int_to_string(Arity) ++ "."), nl]
    ;
        PredOrFunc = pf_function,
        ExpectingPieces = [words("expecting higher-order func inst"),
            fixed("of arity " ++ int_to_string(Arity - 1) ++ "."), nl]
    ),
    Pieces = [words("mode error: variable"),
        quote(mercury_var_to_name_only(VarSet, Var)) |
        has_instantiatedness(ModeInfo, VarInst, ",")] ++
        ExpectingPieces,
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

:- func mode_error_poly_unify_to_spec(mode_info, prog_var, mer_inst)
    = error_spec.

mode_error_poly_unify_to_spec(ModeInfo, Var, VarInst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    MainPieces = [words("in polymorphically-typed unification:"), nl,
        words("mode error: variable"),
        quote(mercury_var_to_name_only(VarSet, Var)) |
        has_instantiatedness(ModeInfo, VarInst, ",")] ++
        [words("expected instantiatedness was"), quote("ground"),
        words("or"), quote("any"), suffix("."), nl],
    VerbosePieces = [words("When unifying two variables whose type"),
        words("will not be known until runtime, the variables must both"),
        words("be ground (or have inst"), quote("any"), suffix(")."),
        words("Unifications of polymorphically-typed variables with"),
        words("partially instantiated modes are not allowed.")],
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces),
            verbose_only(verbose_once, VerbosePieces)])]).

:- func mode_error_var_is_live_to_spec(mode_info, prog_var) = error_spec.

mode_error_var_is_live_to_spec(ModeInfo, Var) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("unique-mode error: the called procedure"),
        words("would clobber its argument, but variable"),
        quote(mercury_var_to_name_only(VarSet, Var)),
        words("is still live."), nl],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

:- func mode_error_var_has_inst_to_spec(mode_info, prog_var,
    mer_inst, mer_inst, maybe(pred_var_multimode_pred_error)) = error_spec.

mode_error_var_has_inst_to_spec(ModeInfo, Var, VarInst, Inst,
        MaybeMultiModeError) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    MainPieces = [words("mode error: variable"),
        quote(mercury_var_to_name_only(VarSet, Var)) |
        has_inst_expected_inst_was(ModeInfo, VarInst, Inst)],
    MainMsgs = [simplest_msg(Context, Preamble ++ MainPieces)],
    (
        MaybeMultiModeError = no,
        Spec = error_spec($pred, severity_error,
            phase_mode_check(report_in_any_mode), MainMsgs)
    ;
        MaybeMultiModeError = yes(MultiModeError),
        ConnectPieces = [words("This may have been caused by"),
            words("the following error."), nl],
        ConnectMsgs = [simplest_msg(Context, ConnectPieces)],
        MultiModeError = pred_var_multimode_pred_error(PredId, MultiMode),
        SubSpec0 = mode_error_unify_var_multimode_pred_to_spec(ModeInfo,
            Var, PredId, MultiMode),
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        extract_spec_msgs(Globals, SubSpec0, SubMsgs),
        Spec = error_spec($pred, severity_error,
            phase_mode_check(report_in_any_mode),
            MainMsgs ++ ConnectMsgs ++ SubMsgs)
    ).

:- func mode_error_implied_mode_to_spec(mode_info, prog_var,
    mer_inst, mer_inst) = error_spec.

mode_error_implied_mode_to_spec(ModeInfo, Var, VarInst, Inst) = Spec :-
    % This "error" message is really a "sorry, not implemented" message.
    % We only print the message if we will actually generating code.
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_op_mode(Globals, OpMode),
    mode_info_get_context(ModeInfo, Context),
    ( if OpMode = opm_top_args(opma_augment(opmau_generate_code(_))) then
        Preamble = mode_info_context_preamble(ModeInfo),
        mode_info_get_varset(ModeInfo, VarSet),
        Pieces = [words("sorry, implied modes not implemented."), nl,
            words("Variable"), quote(mercury_var_to_name_only(VarSet, Var)) |
            has_inst_expected_inst_was(ModeInfo, VarInst, Inst)],
        Spec = simplest_spec($pred, severity_error,
            phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces)
    else
        Spec = error_spec($pred, severity_informational,
            phase_mode_check(report_in_any_mode),
            [simple_msg(Context, [])])
    ).

:- func mode_error_no_mode_decl_to_spec(mode_info) = error_spec.

mode_error_no_mode_decl_to_spec(ModeInfo) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    Pieces = [words("no mode declaration for called predicate."), nl],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

:- func mode_error_unify_pred_to_spec(mode_info, prog_var,
    mode_error_unify_rhs, mer_type, pred_or_func) = error_spec.

mode_error_unify_pred_to_spec(ModeInfo, X, RHS, Type, PredOrFunc) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    (
        RHS = error_at_var(Y),
        RHSStr = mercury_var_to_name_only(VarSet, Y)
    ;
        RHS = error_at_functor(ConsId, ArgVars),
        RHSStr = functor_cons_id_to_string(ModuleInfo, VarSet, print_name_only,
            ConsId, ArgVars)
    ;
        RHS = error_at_lambda(ArgVars, ArgFromToInsts),
        ArgModes = list.map(from_to_insts_to_mode, ArgFromToInsts),
        RHSStr = "lambda(["
            ++ var_modes_to_string(VarSet, InstVarSet, print_name_only,
                ArgVars, ArgModes)
            ++ "] ... )"
    ),
    varset.init(TypeVarSet),
    MainPieces = [words("In unification of"),
        quote(mercury_var_to_name_only(VarSet, X)),
        words("with"), quote(RHSStr), suffix(":"), nl,
        words("mode error: attempt at higher-order unification."), nl,
        words("Cannot unify two terms of type"),
        quote(mercury_type_to_string(TypeVarSet, print_name_only, Type)),
        suffix("."), nl],
    VerbosePieces = [words("Your code is trying to test whether two "),
        words(prog_out.pred_or_func_to_full_str(PredOrFunc) ++ "s"),
        words("are equal, by unifying them."),
        words("In the general case, testing equivalence of"),
        words(prog_out.pred_or_func_to_full_str(PredOrFunc) ++ "s"),
        words("is an undecidable problem,"),
        words("and so this is not allowed by the Mercury mode system."),
        words("In some cases, you can achieve the same effect by"),
        words("writing an explicit universal quantification, e.g."),
        quote("all [X] call(P, X) <=> call(Q, X)"), suffix(","),
        words("instead of"), quote("P = Q"), suffix(".")],
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces),
            verbose_only(verbose_once, VerbosePieces)])]).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_var_to_spec(mode_info, prog_var,
    prog_var, mer_inst, mer_inst) = error_spec.

mode_error_unify_var_var_to_spec(ModeInfo, X, Y, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarSet, X)), words("and"),
        quote(mercury_var_to_name_only(VarSet, Y)), suffix("."), nl,
        words("Variable"), quote(mercury_var_to_name_only(VarSet, X)) |
        has_instantiatedness(ModeInfo, InstX, ",")] ++
        [words("variable"), quote(mercury_var_to_name_only(VarSet, Y)) |
        has_instantiatedness(ModeInfo, InstY, ".")],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_lambda_to_spec(mode_info, prog_var,
    mer_inst, mer_inst) = error_spec.

mode_error_unify_var_lambda_to_spec(ModeInfo, X, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarSet, X)),
        words("and lambda expression."), nl,
        words("Variable"), quote(mercury_var_to_name_only(VarSet, X)) |
        has_instantiatedness(ModeInfo, InstX, ",")] ++
        [words("lambda expression") |
        has_instantiatedness(ModeInfo, InstY, ".")],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_multimode_pred_to_spec(mode_info, prog_var,
    pred_id, var_multimode_pred_error) = error_spec.

mode_error_unify_var_multimode_pred_to_spec(ModeInfo, X, PredId,
        MultiModeError) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    QualifiedName = qualified(PredModule, PredName),
    Arity = pred_info_orig_arity(PredInfo),
    adjust_func_arity(PredOrFunc, FuncArity, Arity),
    StartPieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarSet, X)),
        words("and higher-order term based on multi-moded"),
        p_or_f(PredOrFunc),
        qual_sym_name_arity(sym_name_arity(QualifiedName, FuncArity)),
        suffix("."), nl],
    (
        MultiModeError = some_ho_args_non_ground(NonGroundArgVars),
        VarOrVars = choose_number(NonGroundArgVars, "variable", "variables"),
        NonGroundArgVarPieces =
            named_and_unnamed_vars_to_pieces(VarSet, NonGroundArgVars),
        DetailPieces = [words("The higher order argument"),
            words(VarOrVars)] ++ NonGroundArgVarPieces ++
            [words("should be ground, but are not."), nl]
    ;
        (
            MultiModeError = no_matching_mode(ArgVars),
            MatchPieces = [words(choose_number(ArgVars, "does", "do")),
                words("not match any")]
        ;
            MultiModeError = more_than_one_matching_mode(ArgVars),
            MatchPieces = [words(choose_number(ArgVars, "matches", "match")),
                words("more than one")]
        ),
        ModeOrModes = choose_number(ArgVars, "mode", "modes"),
        VarOrVars = choose_number(ArgVars, "variable", "variables"),
        ArgVarPieces = named_and_unnamed_vars_to_pieces(VarSet, ArgVars),
        DetailPieces = [words("The"), words(ModeOrModes),
            words("of the argument"), words(VarOrVars)] ++ ArgVarPieces ++
            MatchPieces ++ [words("of the called"),
            p_or_f(PredOrFunc), suffix("'s"), words("modes."), nl]
    ),
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        Context, Preamble ++ StartPieces ++ DetailPieces).

:- func named_and_unnamed_vars_to_pieces(prog_varset, list(prog_var)) =
    list(format_component).

named_and_unnamed_vars_to_pieces(VarSet, Vars) = Pieces :-
    list.filter_map(varset.search_name(VarSet), Vars,
        NamedVarNames, UnnamedVars),
    (
        NamedVarNames = [],
        Pieces = []
    ;
        NamedVarNames = [_ | _],
        NamedVarPieces = list_to_pieces(NamedVarNames),
        (
            UnnamedVars = [],
            Pieces = NamedVarPieces
        ;
            UnnamedVars = [_ | _],
            Pieces = [words("including") | NamedVarPieces]
        )
    ).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_functor_to_spec(mode_info, prog_var,
    cons_id, list(prog_var), mer_inst, list(mer_inst)) = error_spec.

mode_error_unify_var_functor_to_spec(ModeInfo, X, ConsId, Args,
        InstX, ArgInsts) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    FunctorConsIdStr = functor_cons_id_to_string(ModuleInfo, VarSet,
        print_name_only, ConsId, Args),
    ConsIdStr = mercury_cons_id_to_string(does_not_need_brackets, ConsId),
    FakeTermInst = defined_inst(user_inst(unqualified(ConsIdStr), ArgInsts)),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarSet, X)),
        words("and"), words_quote(FunctorConsIdStr), suffix("."), nl,
        words("Variable"), quote(mercury_var_to_name_only(VarSet, X)) |
        has_instantiatedness(ModeInfo, InstX, ",")] ++
        [words("term"), words_quote(FunctorConsIdStr) |
        has_instantiatedness(ModeInfo, FakeTermInst, ".")],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_warning_cannot_succeed_var_var(mode_info,
    prog_var, prog_var, mer_inst, mer_inst) = error_spec.

mode_warning_cannot_succeed_var_var(ModeInfo, X, Y, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        quote(mercury_var_to_name_only(VarSet, X)),
        words("and"), quote(mercury_var_to_name_only(VarSet, Y)),
        words("cannot succeed."), nl,
        quote(mercury_var_to_name_only(VarSet, X)) |
        has_instantiatedness(ModeInfo, InstX, ",")] ++
        [quote(mercury_var_to_name_only(VarSet, Y)) |
        has_instantiatedness(ModeInfo, InstY, ".")],
    Spec = simplest_spec($pred, severity_warning,
        phase_mode_check(report_only_if_in_all_modes),
        Context, Preamble ++ Pieces).

:- func mode_warning_cannot_succeed_var_functor(mode_info, prog_var, mer_inst,
    cons_id) = error_spec.

mode_warning_cannot_succeed_var_functor(ModeInfo, X, InstX, ConsId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        quote(mercury_var_to_name_only(VarSet, X)), words("and"),
        words(mercury_cons_id_to_string(does_not_need_brackets, ConsId)),
        words("cannot succeed."), nl,
        quote(mercury_var_to_name_only(VarSet, X)) |
        has_instantiatedness(ModeInfo, InstX, ".")],
    Spec = simplest_spec($pred, severity_warning,
        phase_mode_check(report_only_if_in_all_modes),
        Context, Preamble ++ Pieces).

:- func mode_warning_cannot_succeed_ground_occur_check(mode_info, prog_var,
    cons_id) = error_spec.

mode_warning_cannot_succeed_ground_occur_check(ModeInfo, X, ConsId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        quote(mercury_var_to_name_only(VarSet, X)), words("and"),
        words(mercury_cons_id_to_string(does_not_need_brackets, ConsId)),
        words("cannot succeed, because"),
        quote(mercury_var_to_name_only(VarSet, X)),
        words("cannot be equal to a term containing itself."), nl],
    Spec = simplest_spec($pred, severity_warning,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_info_context_preamble(mode_info) = list(format_component).

mode_info_context_preamble(ModeInfo) = Pieces :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_pred_id(ModeInfo, PredId),
    mode_info_get_proc_id(ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    pred_info_get_origin(PredInfo, PredOrigin),
    ( if PredOrigin = origin_instance_method(MethodName, _) then
        Name0 = unqualify_name(MethodName),
        ExtraMethodPieces = [words("type class method implementation for")]
    else
        Name0 = pred_info_name(PredInfo),
        ExtraMethodPieces = []
    ),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    Name = unqualified(Name0),
    pred_info_get_markers(PredInfo, PredMarkers),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    strip_builtin_qualifiers_from_mode_list(Modes0, Modes),
    MaybeDet = no,
    ModeSubDeclStr = mercury_mode_subdecl_to_string(output_debug, PredOrFunc,
        InstVarSet, Name, Modes, MaybeDet),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    ModeContextPieces =
        mode_context_to_pieces(ModeInfo, ModeContext, PredMarkers),
    Pieces = [words("In clause for")] ++
        ExtraMethodPieces ++
        [words_quote(ModeSubDeclStr), suffix(":"), nl] ++
        ModeContextPieces.

%---------------------------------------------------------------------------%

    % XXX some parts of the mode context never get set up

:- func mode_context_to_pieces(mode_info, mode_context, pred_markers)
    = list(format_component).

mode_context_to_pieces(ModeInfo, ModeContext, Markers) = Pieces :-
    (
        ModeContext = mode_context_uninitialized,
        Pieces = []
    ;
        ModeContext = mode_context_call(ModeCallId, ArgNum),
        (
            ModeCallId = mode_call_plain(PredId),
            mode_info_get_pf_sym_name_arity(ModeInfo, PredId, PFSymNameArity),
            CallId = plain_call_id(PFSymNameArity)
        ;
            ModeCallId = mode_call_generic(GenericCallId),
            CallId = generic_call_id(GenericCallId)
        ),
        Pieces = [words("in"),
            words(call_arg_id_to_string(CallId, ArgNum, Markers)),
            suffix(":"), nl]
    ;
        ModeContext = mode_context_unify(UnifyContext, _Side),
        unify_context_first_to_pieces(is_not_first, _, UnifyContext,
            [], Pieces)
    ).

%---------------------------------------------------------------------------%

:- func mode_error_final_inst_to_spec(mode_info, int, prog_var,
    mer_inst, mer_inst, final_inst_error) = error_spec.

mode_error_final_inst_to_spec(ModeInfo, ArgNum, Var, VarInst, ExpInst, Reason)
        = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        Reason = too_instantiated,
        Problem = "became too instantiated."
    ;
        Reason = not_instantiated_enough,
        Problem = "did not get sufficiently instantiated."
    ;
        Reason = wrongly_instantiated,
        % I don't think this can happen. But just in case...
        Problem = "had the wrong instantiatedness."
    ),
    Pieces = [words("mode error: argument"), fixed(int_to_string(ArgNum)),
        words(Problem), nl,
        words("Final instantiatedness of"),
        quote(mercury_var_to_name_only(VarSet, Var)), words("was") |
        report_inst(ModeInfo, quote_short_inst, [suffix(","), nl],
            [nl_indent_delta(1)], [suffix(","), nl_indent_delta(-1)],
            VarInst)] ++
        [words("expected final instantiatedness was") |
        report_inst(ModeInfo, quote_short_inst, [suffix("."), nl],
            [nl_indent_delta(1)], [suffix("."), nl_indent_delta(-1)],
            ExpInst)],
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func purity_error_should_be_in_promise_purity_scope_to_spec(
    negated_context_desc, mode_info, prog_var) = error_spec.

purity_error_should_be_in_promise_purity_scope_to_spec(NegCtxtDesc,
        ModeInfo, Var) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        NegCtxtDesc = if_then_else,
        Pieces = [words("purity error: if-then-else should be inside"),
            words("a promise_purity scope because non-local variable"),
            quote(mercury_var_to_name_only(VarSet, Var)),
            words("has inst"), quote("any"),
            words("and appears in the condition."), nl]
    ;
        NegCtxtDesc = negation,
        Pieces = [words("purity error: negation should be inside"),
            words("a promise_purity scope because non-local variable"),
            quote(mercury_var_to_name_only(VarSet, Var)),
            words("has inst"), quote("any"),
            words("and appears in the body."), nl]
    ),
    Spec = simplest_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func purity_error_lambda_should_be_any_to_spec(mode_info, list(prog_var))
    = error_spec.

purity_error_lambda_should_be_any_to_spec(ModeInfo, Vars) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("purity error: lambda is"), quote("ground"),
        words("but contains the following non-local variables"),
        words("whose insts contain"), quote("any"), suffix(":"),
        words(mercury_vars_to_name_only(VarSet, Vars)),
        suffix("."), nl],
    Always = always(Preamble ++ Pieces),
    VerboseOnly = verbose_only(verbose_once, [words("Predicate expressions"),
        words("with inst"), quote("any"), words("can be written"),
        quote("any_pred(Args) is det :- ..."), suffix("."),
        words("Function expressions with inst"), quote("any"),
        words("can be written"),
        quote("any_func(Args) = Result is det :- ..."), suffix("."), nl]),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [Always, VerboseOnly])]).

%---------------------------------------------------------------------------%

should_report_mode_warning_for_pred_origin(origin_special_pred(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_instance_method(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_class_method(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_transformed(_, _, _)) = no.
should_report_mode_warning_for_pred_origin(origin_created(_)) = no.
should_report_mode_warning_for_pred_origin(origin_assertion(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_lambda(_, _, _)) = yes.
should_report_mode_warning_for_pred_origin(origin_solver_type(_, _, _)) = no.
should_report_mode_warning_for_pred_origin(origin_tabling(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_mutable(_, _, _)) = no.
should_report_mode_warning_for_pred_origin(origin_initialise) = no.
should_report_mode_warning_for_pred_origin(origin_finalise) = no.
should_report_mode_warning_for_pred_origin(origin_user(_)) = yes.

%---------------------------------------------------------------------------%

:- func inst_list_to_sep_lines(mode_info, list(mer_inst))
    = list(format_component).

inst_list_to_sep_lines(_ModeInfo, []) = [].
inst_list_to_sep_lines(ModeInfo, [Inst | Insts]) = Pieces :-
    (
        Insts = [],
        Pieces = report_inst(ModeInfo, fixed_short_inst, [nl_indent_delta(-1)],
            [], [nl_indent_delta(-1)], Inst)
    ;
        Insts = [_ | _],
        HeadPieces = report_inst(ModeInfo, fixed_short_inst, [suffix(","), nl],
            [], [suffix(","), nl], Inst),
        TailPieces = inst_list_to_sep_lines(ModeInfo, Insts),
        Pieces = HeadPieces ++ TailPieces
    ).

%---------------------------------------------------------------------------%

:- func has_inst_expected_inst_was(mode_info, mer_inst, mer_inst)
    = list(format_component).

has_inst_expected_inst_was(ModeInfo, ActualInst, ExpectedInst) =
    has_instantiatedness(ModeInfo, ActualInst, ",") ++
    expected_inst_was(ModeInfo, ExpectedInst).

:- func has_instantiatedness(mode_info, mer_inst, string)
    = list(format_component).

has_instantiatedness(ModeInfo, Inst, Suffix) =
    [words("has instantiatedness") |
        report_inst(ModeInfo, quote_short_inst, [suffix(Suffix), nl],
            [nl_indent_delta(1)], [suffix(Suffix), nl_indent_delta(-1)],
            Inst)].

:- func expected_inst_was(mode_info, mer_inst) = list(format_component).

expected_inst_was(ModeInfo, Inst) =
    [words("expected instantiatedness was") |
        report_inst(ModeInfo, quote_short_inst, [suffix("."), nl],
            [nl_indent_delta(1)], [suffix("."), nl_indent_delta(-1)], Inst)].

%---------------------------------------------------------------------------%

:- type maybe_report_is_ground
    --->    dont_report_is_ground
    ;       report_is_ground_v_and_nonv.

:- func report_inst_in_context(mode_info, format_component,
    maybe_report_is_ground, pair(prog_context, mer_inst)) = error_msg.

report_inst_in_context(ModeInfo, VarNamePiece, ReportIsGround, Context - Inst)
        = Msg :-
    (
        ReportIsGround = dont_report_is_ground,
        IntroPieces = [words("In this branch,"), VarNamePiece,
            words("has instantiatedness")],
        Pieces = report_inst_in_branch(ModeInfo, IntroPieces, Inst),
        Msg = simple_msg(Context, [always(Pieces)])
    ;
        ReportIsGround = report_is_ground_v_and_nonv,
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        ( if inst_is_ground(ModuleInfo, Inst) then
            ( if Inst = ground(shared, none_or_default_func) then
                Pieces = [words("In this branch,"), VarNamePiece,
                    words("is ground."), nl]
            else
                IntroPieces = [words("In this branch,"), VarNamePiece,
                    words("has the ground instantiatedness")],
                Pieces = report_inst_in_branch(ModeInfo, IntroPieces, Inst)
            ),
            Msg = simple_msg(Context, [verbose_and_nonverbose(Pieces, [])])
        else
            ( if ( Inst = free ; Inst = free(_) ) then
                Pieces = [words("In this branch,"), VarNamePiece,
                    words("is free."), nl]
            else
                IntroPieces = [words("In this branch,"), VarNamePiece,
                    words("has the non-ground instantiatedness")],
                Pieces = report_inst_in_branch(ModeInfo, IntroPieces, Inst)
            ),
            Msg = simple_msg(Context, [always(Pieces)])
        )
    ).

:- func report_inst_in_branch(mode_info, list(format_component), mer_inst)
    = list(format_component).

report_inst_in_branch(ModeInfo, IntroPieces, Inst) = Pieces :-
    Pieces = [nl_indent_delta(1) | IntroPieces] ++ [nl_indent_delta(1) |
        report_inst(ModeInfo, fixed_short_inst,
            [suffix("."), nl_indent_delta(-2)],
            [], [suffix("."), nl_indent_delta(-2)], Inst)].

:- func report_inst(mode_info, short_inst,
    list(format_component), list(format_component), list(format_component),
    mer_inst) = list(format_component).

report_inst(ModeInfo, ShortInstQF, ShortInstSuffix,
        LongInstPrefix, LongInstSuffix, Inst0) = Pieces :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    Pieces = error_msg_inst(ModuleInfo, InstVarSet, expand_named_insts,
        ShortInstQF, ShortInstSuffix, LongInstPrefix, LongInstSuffix, Inst0).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_errors.
%---------------------------------------------------------------------------%
