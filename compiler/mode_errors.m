%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_errors.m.
% Main author: fjh.
%
% This module contains all the error-reporting routines for the mode-checker.
%
%-----------------------------------------------------------------------------%

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
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

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

    ;       mode_error_var_has_inst(prog_var, mer_inst, mer_inst)
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

    ;       mode_error_no_matching_mode(list(prog_var), list(mer_inst))
            % Call to a predicate with an insufficiently instantiated variable
            % (for preds with >1 mode).

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

    ;       mode_error_unify_var_multimode_pred(prog_var, pred_id)
            % Some sort of error in attempt to take address of a multi-moded
            % predicate.

    ;       mode_error_unify_var_multimode_pred_undetermined(prog_var,
                pred_id)
            % Attempt to take address of a multi-moded predicate where we
            % could not uniquely determine the mode to use.

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
    ;       conj_floundered.        % We've reached the end of a conjunction
                                    % and there were still delayed goals.

:- type final_inst_error
    --->    too_instantiated
    ;       not_instantiated_enough
    ;       wrongly_instantiated.   % A catchall for anything that doesn't
                                    % fit into the above two categories.

:- type mode_error_unify_rhs
    --->    error_at_var(prog_var)
    ;       error_at_functor(cons_id, list(prog_var))
    ;       error_at_lambda(list(prog_var), list(mer_mode)).

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

%-----------------------------------------------------------------------------%

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

    % Write out the inferred `mode' declarations for a list of pred_ids.
    % The bool indicates whether or not to write out determinism
    % annotations on the modes (it should only be set to `yes' _after_
    % determinism analysis).
    %
:- func report_mode_inference_messages(module_info, include_detism_on_modes,
    list(pred_id)) = list(error_spec).

:- func mode_decl_to_string(proc_id, pred_info) = string.

:- func mode_error_info_to_spec(mode_info, mode_error_info) = error_spec.

:- func mode_warning_info_to_spec(mode_info, mode_warning_info) = error_spec.

:- func should_report_mode_warning_for_pred_origin(pred_origin) = bool.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_info.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module io.            % used only for a typeclass instance
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

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
        ModeError = mode_error_disj(MergeContext, ErrorList),
        Spec = mode_error_disj_to_spec(ModeInfo, MergeContext, ErrorList)
    ;
        ModeError = mode_error_par_conj(ErrorList),
        Spec = mode_error_par_conj_to_spec(ModeInfo, ErrorList)
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
        ModeError = mode_error_var_has_inst(Var, InstA, InstB),
        Spec = mode_error_var_has_inst_to_spec(ModeInfo, Var, InstA, InstB)
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
        ModeError = mode_error_unify_var_multimode_pred(VarA, PredId),
        Spec = mode_error_unify_var_multimode_pred_to_spec(ModeInfo, VarA,
            PredId)
    ;
        ModeError = mode_error_unify_var_multimode_pred_undetermined(VarA,
            PredId),
        Spec = mode_error_unify_var_multimode_pred_undetermined_to_spec(
            ModeInfo, VarA, PredId)
    ;
        ModeError = mode_error_unify_var_functor(Var, Name, Args, Inst,
            ArgInsts),
        Spec = mode_error_unify_var_functor_to_spec(ModeInfo, Var, Name,
            Args, Inst, ArgInsts)
    ;
        ModeError = mode_error_conj(Errors, Culprit),
        Spec = mode_error_conj_to_spec(ModeInfo, Errors, Culprit)
    ;
        ModeError = mode_error_no_matching_mode(Vars, Insts),
        Spec = mode_error_no_matching_mode_to_spec(ModeInfo, Vars, Insts)
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

%-----------------------------------------------------------------------------%

:- func mode_error_conj_to_spec(mode_info, list(delayed_goal),
    schedule_culprit) = error_spec.

mode_error_conj_to_spec(ModeInfo, Errors, Culprit) = Spec :-
    mode_info_get_context(ModeInfo, Context),
    (
        Errors = [],
        unexpected($module, $pred, "no errors")
    ;
        Errors = [Error],
        Msgs1 = mode_error_conjunct_to_msgs(Context, ModeInfo, Error)
    ;
        Errors = [_, _ | _],
        % If there's more than one error, we use the setting of
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
                    unexpected($module, $pred, "no errors of any kind")
                )
            ),
            % MoreMsg is there to indicate that --verbose-errors would yield
            % more information.
            MoreMsg = simple_msg(Context, [verbose_only([])]),
            Msgs1 = ConjMsgs ++ [MoreMsg]
        ;
            VerboseErrors = yes,
            Preamble = mode_info_context_preamble(ModeInfo),
            ConjPieces = [words("mode error in conjunction. The next"),
                fixed(int_to_string(list.length(Errors))),
                words("error messages indicate"),
                words("possible causes of this error.")],
            Msgs1Start = [simple_msg(Context,
                [always(Preamble ++ ConjPieces)])],
            Msgs1Rest = list.map(
                mode_error_conjunct_to_msgs(Context, ModeInfo),
                ImportantErrors ++ OtherErrors),
            Msgs1 = Msgs1Start ++ list.condense(Msgs1Rest)
        )
    ),

    % If the goal(s) couldn't be scheduled because we couldn't reorder things
    % past an impure goal, then report that.
    (
        Culprit = conj_floundered,
        % We've already reported everything we can.
        Msgs2 = []
    ;
        Culprit = goal_itself_was_impure,
        Pieces = [words("The goal could not be reordered,"),
            words("because it was impure.")],
        Msgs2 = [simple_msg(Context, [always(Pieces)])]
    ;
        Culprit = goals_followed_by_impure_goal(ImpureGoal),
        ImpureGoal = hlds_goal(_, ImpureGoalInfo),
        ImpureGoalContext = goal_info_get_context(ImpureGoalInfo),
        Pieces1 = [words("The goal could not be reordered,"),
            words("because it was followed by an impure goal.")],
        Pieces2 = [words("This is the location of the impure goal.")],
        Msgs2 = [
            simple_msg(Context, [always(Pieces1)]),
            simple_msg(ImpureGoalContext, [always(Pieces2)])
        ]
    ),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        Msgs1 ++ Msgs2).

:- pred is_error_important(delayed_goal::in) is semidet.

is_error_important(Error) :-
    Error = delayed_goal(_, mode_error_info(_, ModeError, _, ModeContext), _),
    (
        % An error is important unless it is a non-explicit unification,
        % i.e. a head unification or a call argument unification.
        ModeContext = mode_context_unify(unify_context(UnifyContext, _), _),
        UnifyContext \= umc_explicit,

        % Except that errors in lambda goals are important even if the
        % unification that creates the lambda goal is an implicit one.
        ModeError \= mode_error_non_local_lambda_var(_, _)
    ->
        fail
    ;
        true
    ).

:- func mode_error_conjunct_to_msgs(prog_context, mode_info, delayed_goal)
    = list(error_msg).

mode_error_conjunct_to_msgs(Context, !.ModeInfo, DelayedGoal) = Msgs :-
    DelayedGoal = delayed_goal(Vars, Error, Goal),
    set_of_var.to_sorted_list(Vars, VarList),
    mode_info_get_varset(!.ModeInfo, VarSet),
    Pieces1 = [words("Floundered goal, waiting on {"),
        words(mercury_vars_to_string(VarSet, no, VarList)),
        words("}:"), nl],
    Msg1 = simple_msg(Context,
        [option_is_set(debug_modes, yes, [always(Pieces1)])]),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    Msg2 = error_msg(no, do_not_treat_as_first, 0,
        [option_is_set(very_verbose, yes,
            [always([nl]),
             'new print_anything'(
                write_indented_goal(ModuleInfo, VarSet, Goal))])]),
    Error = mode_error_info(_, ModeError, ErrorContext, ModeContext),
    mode_info_set_context(ErrorContext, !ModeInfo),
    mode_info_set_mode_context(ModeContext, !ModeInfo),
    SubSpec = mode_error_to_spec(!.ModeInfo, ModeError),
    SubSpec = error_spec(_, _, SubMsgs),
    Msgs = [Msg1, Msg2] ++ SubMsgs.

:- type write_indented_goal
    --->    write_indented_goal(module_info, prog_varset, hlds_goal).

:- instance error_util.print_anything(write_indented_goal) where [
    ( print_anything(write_indented_goal(ModuleInfo, VarSet, Goal), !IO) :-
        io.write_string("\t\t", !IO),
        module_info_get_globals(ModuleInfo, Globals),
        OutInfo = init_hlds_out_info(Globals),
        write_goal(OutInfo, Goal, ModuleInfo, VarSet, no, 2, ".\n", !IO)
    )
].

%-----------------------------------------------------------------------------%

:- func mode_error_disj_to_spec(mode_info, merge_context, merge_errors)
    = error_spec.

mode_error_disj_to_spec(ModeInfo, MergeContext, ErrorList) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    MainPieces = [words("mode mismatch in "),
        words(merge_context_to_string(MergeContext)), suffix("."), nl],
    MergePieceLists = list.map(merge_error_to_pieces(ModeInfo), ErrorList),
    list.condense(MergePieceLists, MergePieces),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces ++ MergePieces)])]).

:- func mode_error_par_conj_to_spec(mode_info, merge_errors) = error_spec.

mode_error_par_conj_to_spec(ModeInfo, ErrorList) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    Pieces = [words("mode error: mutually exclusive bindings"),
        words("in parallel conjunction."),
        words("(The current implementation does not permit"),
        words("parallel conjunctions to fail.)"), nl],
    MergePieceLists = list.map(merge_error_to_pieces(ModeInfo), ErrorList),
    list.condense(MergePieceLists, MergePieces),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ Pieces ++ MergePieces)])]).

:- func merge_context_to_string(merge_context) = string.

merge_context_to_string(merge_disj) = "disjunction".
merge_context_to_string(merge_if_then_else) = "if-then-else".
merge_context_to_string(merge_stm_atomic) = "atomic".

:- func merge_error_to_pieces(mode_info, merge_error) = list(format_component).

merge_error_to_pieces(ModeInfo, MergeError) = Pieces :-
    mode_info_get_varset(ModeInfo, VarSet),
    VarPieces = [words("The variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("has these instantiation states:"), nl_indent_delta(1)],
    MergeError = merge_error(Var, ContextsInsts0),
    list.sort(ContextsInsts0, ContextsInsts),
    (
        ContextsInsts = [],
        unexpected($module, $pred, "ContextsInsts0 = []")
    ;
        ContextsInsts = [HeadContext - _HeadInst | TailContextInsts],
        HeadContext = term.context(FileName, _),
        ( all_in_same_file(FileName, TailContextInsts) ->
            InclFileName = no
        ;
            InclFileName = yes
        )
    ),
    InstPiecesList = list.map(context_inst_to_string(ModeInfo, InclFileName),
        ContextsInsts),
    InstPieces = list.condense(InstPiecesList),
    SuffixPieces = [nl_indent_delta(-1)],
    Pieces = VarPieces ++ InstPieces ++ SuffixPieces.

:- pred all_in_same_file(string::in, assoc_list(prog_context, mer_inst)::in)
    is semidet.

all_in_same_file(_, []).
all_in_same_file(FileName, [Pair | Pairs]) :-
    Pair = Context - _Inst,
    Context = term.context(FileName, _),
    all_in_same_file(FileName, Pairs).

:- func context_inst_to_string(mode_info, bool, pair(prog_context, mer_inst))
    = list(format_component).

context_inst_to_string(ModeInfo, InclFileName, Context - Inst) = Pieces :-
    Context = context(FileName, LineNum),
    LineNumStr = string.int_to_string(LineNum),
    (
        InclFileName = no,
        ContextStr = "line " ++ LineNumStr ++ ":"
    ;
        InclFileName = yes,
        ContextStr = FileName ++ ":" ++ LineNumStr ++ ":"
    ),
    Pieces = [fixed(ContextStr), words(inst_to_string(ModeInfo, Inst)), nl].

%-----------------------------------------------------------------------------%

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
        words("Variable"), quote(mercury_var_to_string(VarSet, no, Var)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, VarInst)), suffix(","), nl,
        words("expected instantiatedness was"),
        words_quote(inst_to_string(ModeInfo, Inst)), suffix("."), nl],
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
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces), verbose_only(VerbosePieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_error_non_local_lambda_var_to_spec(mode_info, prog_var, mer_inst)
    = error_spec.

mode_error_non_local_lambda_var_to_spec(ModeInfo, Var, VarInst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error: variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, VarInst)),
        suffix(","), nl,
        words("expected instantiatedness for non-local variables"),
        words("of lambda goals is"), quote("ground"), suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_error_in_callee_to_spec(mode_info, list(prog_var), list(mer_inst),
    pred_id, proc_id, list(mode_error_info)) = error_spec.

mode_error_in_callee_to_spec(!.ModeInfo, Vars, Insts,
        CalleePredId, CalleeProcId, CalleeModeErrors) = Spec :-
    Preamble = mode_info_context_preamble(!.ModeInfo),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_varset(!.ModeInfo, VarSet),
    MainPieces = [words("mode error: arguments"),
        quote(mercury_vars_to_string(VarSet, no, Vars)),
        words("have the following insts:"), nl_indent_delta(1)] ++
        inst_list_to_sep_lines(!.ModeInfo, Insts) ++
        [words("which does not match any of the valid modes for")],

    CalleePredIdPieces = describe_one_pred_name(ModuleInfo,
        should_module_qualify, CalleePredId),
    VerboseCalleePieces = [words("the callee"), prefix("(")] ++
        CalleePredIdPieces ++
        [suffix(")"), nl, words("because of the following error."), nl],
    VerbosePieces = MainPieces ++ VerboseCalleePieces,
    NonVerboseCalleePieces =
        [words("the callee, because of the following error."), nl],
    NonVerbosePieces = MainPieces ++ NonVerboseCalleePieces,

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
        CalleeModeErrorSpec = mode_error_to_spec(!.ModeInfo, CalleeModeError),
        CalleeModeErrorSpec = error_spec(_, _, LaterMsgs0),
        (
            LaterMsgs0 = [],
            LaterMsgs = []
        ;
            LaterMsgs0 = [LaterHead0 | LaterTail],
            (
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
        Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
            [InitMsg | LaterMsgs])
    ;
        CalleeModeErrors = [],
        unexpected($module, $pred, "no error")
    ).

:- func mode_error_no_matching_mode_to_spec(mode_info, list(prog_var),
    list(mer_inst)) = error_spec.

mode_error_no_matching_mode_to_spec(ModeInfo, Vars, Insts) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    (
        ModeContext = mode_context_call(CallId, _),
        CallIdStr = call_id_to_string(CallId)
    ;
        ( ModeContext = mode_context_unify(_, _)
        ; ModeContext = mode_context_uninitialized
        ),
        unexpected($module, $pred, "invalid context")
    ),
    Pieces = [words("mode error: arguments"),
        quote(mercury_vars_to_string(VarSet, no, Vars)),
        words("have the following insts:"), nl_indent_delta(1)] ++
        inst_list_to_sep_lines(ModeInfo, Insts) ++
        [words("which does not match any of the modes for"),
        words(CallIdStr), suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_error_higher_order_pred_var_to_spec(mode_info, pred_or_func,
    prog_var, mer_inst, arity) = error_spec.

mode_error_higher_order_pred_var_to_spec(ModeInfo, PredOrFunc, Var, VarInst,
        Arity) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        PredOrFunc = pf_predicate,
        Expecting = "expecting higher-order pred inst (of arity " ++
            int_to_string(Arity) ++ ")."
    ;
        PredOrFunc = pf_function,
        Expecting = "expecting higher-order func inst (of arity " ++
            int_to_string(Arity - 1) ++ ")."
    ),
    Pieces = [words("mode error: variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, VarInst)), suffix(","), nl,
        words(Expecting), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_error_poly_unify_to_spec(mode_info, prog_var, mer_inst)
    = error_spec.

mode_error_poly_unify_to_spec(ModeInfo, Var, VarInst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    MainPieces = [words("in polymorphically-typed unification:"), nl,
        words("mode error: variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, VarInst)), suffix(","), nl,
        words("expected instantiatedness was"), quote("ground"),
        words("or"), quote("any"), suffix("."), nl],
    VerbosePieces = [words("When unifying two variables whose type"),
        words("will not be known until runtime, the variables must both"),
        words("be ground (or have inst"), quote("any"), suffix(")."),
        words("Unifications of polymorphically-typed variables with"),
        words("partially instantiated modes are not allowed.")],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces), verbose_only(VerbosePieces)])]).

:- func mode_error_var_is_live_to_spec(mode_info, prog_var) = error_spec.

mode_error_var_is_live_to_spec(ModeInfo, Var) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("unique-mode error: the called procedure"),
        words("would clobber its argument, but variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("is still live."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_error_var_has_inst_to_spec(mode_info, prog_var,
    mer_inst, mer_inst) = error_spec.

mode_error_var_has_inst_to_spec(ModeInfo, Var, VarInst, Inst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error: variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, VarInst)), suffix(","), nl,
        words("expected instantiatedness was"),
        words_quote(inst_to_string(ModeInfo, Inst)), suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_error_implied_mode_to_spec(mode_info, prog_var,
    mer_inst, mer_inst) = error_spec.

mode_error_implied_mode_to_spec(ModeInfo, Var, VarInst, Inst) = Spec :-
    % This "error" message is really a "sorry, not implemented" message.
    % We only print the message if we will actually generating code.
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("sorry, implied modes not implemented."), nl,
        words("Variable"),
        quote(mercury_var_to_string(VarSet, no, Var)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, VarInst)), suffix(","), nl,
        words("expected instantiatedness was"),
        words_quote(inst_to_string(ModeInfo, Inst)), suffix("."), nl],
    Severity = severity_conditional(errorcheck_only, no, severity_error, no),
    Spec = error_spec(Severity, phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [option_is_set(errorcheck_only, no,
                [always(Preamble ++ Pieces)])])]).

:- func mode_error_no_mode_decl_to_spec(mode_info) = error_spec.

mode_error_no_mode_decl_to_spec(ModeInfo) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    Pieces = [words("no mode declaration for called predicate."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

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
        RHSStr = mercury_var_to_string(VarSet, no, Y)
    ;
        RHS = error_at_functor(ConsId, ArgVars),
        RHSStr = functor_cons_id_to_string(ConsId, ArgVars, VarSet,
            ModuleInfo, no)
    ;
        RHS = error_at_lambda(ArgVars, ArgModes),
        RHSStr = "lambda(["
            ++ var_modes_to_string(ArgVars, ArgModes, VarSet, InstVarSet, no)
            ++ "] ... )"
    ),
    varset.init(TypeVarSet),
    MainPieces = [words("In unification of"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("with"), quote(RHSStr), suffix(":"), nl,
        words("mode error: attempt at higher-order unification."), nl,
        words("Cannot unify two terms of type"),
        quote(mercury_type_to_string(TypeVarSet, no, Type)), suffix("."), nl],
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
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces), verbose_only(VerbosePieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_var_to_spec(mode_info, prog_var,
    prog_var, mer_inst, mer_inst) = error_spec.

mode_error_unify_var_var_to_spec(ModeInfo, X, Y, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_string(VarSet, no, X)), words("and"),
        quote(mercury_var_to_string(VarSet, no, Y)), suffix("."), nl,
        words("Variable"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstX)), suffix(","), nl,
        words("variable"),
        quote(mercury_var_to_string(VarSet, no, Y)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstY)), suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_lambda_to_spec(mode_info, prog_var,
    mer_inst, mer_inst) = error_spec.

mode_error_unify_var_lambda_to_spec(ModeInfo, X, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("and lambda expression."), nl,
        words("Variable"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstX)), suffix(","), nl,
        words("lambda expression has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstY)), suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_multimode_pred_to_spec(mode_info, prog_var,
    pred_id) = error_spec.

mode_error_unify_var_multimode_pred_to_spec(ModeInfo, X, PredId) = Spec :-
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
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("and higher-order term based on multi-moded"),
        p_or_f(PredOrFunc), sym_name_and_arity(QualifiedName / FuncArity),
        suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_error_unify_var_multimode_pred_undetermined_to_spec(mode_info,
    prog_var, pred_id) = error_spec.

mode_error_unify_var_multimode_pred_undetermined_to_spec(ModeInfo, X, PredId)
        = Spec :-
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
    Pieces = [words("In unification of"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("and higher-order term."),
        words("Could not determine the mode of"),
        p_or_f(PredOrFunc), sym_name_and_arity(QualifiedName / FuncArity),
        words("by the insts of the higher-order arguments only."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_functor_to_spec(mode_info, prog_var,
    cons_id, list(prog_var), mer_inst, list(mer_inst)) = error_spec.

mode_error_unify_var_functor_to_spec(ModeInfo, X, ConsId, Args,
        InstX, ArgInsts) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    FunctorConsIdStr =
        functor_cons_id_to_string(ConsId, Args, VarSet, ModuleInfo, no),
    Pieces1 = [words("mode error in unification of"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("and"), words_quote(FunctorConsIdStr), suffix("."), nl,
        words("Variable"), quote(mercury_var_to_string(VarSet, no, X)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstX)), suffix(","), nl,
        words("term"),
        words_quote(FunctorConsIdStr)],
    ConsIdStr = mercury_cons_id_to_string(does_not_need_brackets, ConsId),
    (
        Args = [_ | _],
        Pieces2 = [words("has instantiatedness"),
            prefix("`"), words(ConsIdStr), suffix("("), nl_indent_delta(1)] ++
            inst_list_to_sep_lines(ModeInfo, ArgInsts) ++
            [fixed(")'.")]
    ;
        Args = [],
        Pieces2 = [words("has instantiatedness"),
            words(ConsIdStr), suffix("."), nl]
    ),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces1 ++ Pieces2)])]).

%-----------------------------------------------------------------------------%

:- func mode_warning_cannot_succeed_var_var(mode_info,
    prog_var, prog_var, mer_inst, mer_inst) = error_spec.

mode_warning_cannot_succeed_var_var(ModeInfo, X, Y, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("and"), quote(mercury_var_to_string(VarSet, no, Y)),
        words("cannot succeed."), nl,
        quote(mercury_var_to_string(VarSet, no, X)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstX)), suffix(","), nl,
        quote(mercury_var_to_string(VarSet, no, Y)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstY)), suffix("."), nl],
    Spec = error_spec(severity_warning,
        phase_mode_check(report_only_if_in_all_modes),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_warning_cannot_succeed_var_functor(mode_info, prog_var, mer_inst,
    cons_id) = error_spec.

mode_warning_cannot_succeed_var_functor(ModeInfo, X, InstX, ConsId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        quote(mercury_var_to_string(VarSet, no, X)), words("and"),
        words(mercury_cons_id_to_string(does_not_need_brackets, ConsId)),
        words("cannot succeed."), nl,
        quote(mercury_var_to_string(VarSet, no, X)),
        words("has instantiatedness"),
        words_quote(inst_to_string(ModeInfo, InstX)), suffix("."), nl],
    Spec = error_spec(severity_warning,
        phase_mode_check(report_only_if_in_all_modes),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

:- func mode_warning_cannot_succeed_ground_occur_check(mode_info, prog_var,
    cons_id) = error_spec.

mode_warning_cannot_succeed_ground_occur_check(ModeInfo, X, ConsId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        quote(mercury_var_to_string(VarSet, no, X)), words("and"),
        words(mercury_cons_id_to_string(does_not_need_brackets, ConsId)),
        words("cannot succeed, because"),
        quote(mercury_var_to_string(VarSet, no, X)),
        words("cannot be equal to a term containing itself."), nl],
    Spec = error_spec(severity_warning,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func mode_info_context_preamble(mode_info) = list(format_component).

mode_info_context_preamble(ModeInfo) = Pieces :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_pred_id(ModeInfo, PredId),
    mode_info_get_proc_id(ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_markers(PredInfo, PredMarkers),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    strip_builtin_qualifiers_from_mode_list(Modes0, Modes),
    Name0 = pred_info_name(PredInfo),
    Name = unqualified(Name0),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    MaybeDet = no,

    ModeSubDeclStr = mercury_mode_subdecl_to_string(PredOrFunc, InstVarSet,
        Name, Modes, MaybeDet, Context),
    Pieces1 = [words("In clause for"), words_quote(ModeSubDeclStr),
        suffix(":"), nl],
    mode_info_get_mode_context(ModeInfo, ModeContext),
    Pieces2 = mode_context_to_pieces(ModeContext, PredMarkers),
    Pieces = Pieces1 ++ Pieces2.

%-----------------------------------------------------------------------------%

    % XXX some parts of the mode context never get set up

:- func mode_context_to_pieces(mode_context, pred_markers)
    = list(format_component).

mode_context_to_pieces(mode_context_uninitialized, _Markers) = [].
mode_context_to_pieces(mode_context_call(CallId, ArgNum), Markers) =
    [words("in"),
        words(call_arg_id_to_string(CallId, ArgNum, Markers)),
        suffix(":"), nl].
mode_context_to_pieces(mode_context_unify(UnifyContext, _Side), _Markers)
        = Pieces :-
    unify_context_first_to_pieces(is_not_first, _, UnifyContext, [], Pieces).

%-----------------------------------------------------------------------------%

mode_context_init(mode_context_uninitialized).

%-----------------------------------------------------------------------------%

:- func mode_error_final_inst_to_spec(mode_info, int, prog_var,
    mer_inst, mer_inst, final_inst_error) = error_spec.

mode_error_final_inst_to_spec(ModeInfo, ArgNum, Var, VarInst, Inst, Reason)
        = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        Reason = too_instantiated,
        Problem = " became too instantiated."
    ;
        Reason = not_instantiated_enough,
        Problem = "did not get sufficiently instantiated."
    ;
        Reason = wrongly_instantiated,
        % I don't think this can happen.  But just in case...
        Problem = "had the wrong instantiatedness."
    ),
    Pieces = [words("mode error: argument"), fixed(int_to_string(ArgNum)),
        words(Problem), nl,
        words("Final instantiatedness of"),
        quote(mercury_var_to_string(VarSet, no, Var)), words("was"),
        words_quote(inst_to_string(ModeInfo, VarInst)), suffix(","), nl,
        words("expected final instantiatedness was"),
        words_quote(inst_to_string(ModeInfo, Inst)), suffix("."), nl],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func purity_error_should_be_in_promise_purity_scope_to_spec(
    negated_context_desc, mode_info, prog_var) = error_spec.

purity_error_should_be_in_promise_purity_scope_to_spec(NegCtxtDesc,
        ModeInfo, Var) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        NegCtxtDesc = if_then_else,
        Pieces = [
            words("purity error: if-then-else"),
            words("should be inside a promise_purity"),
            words("scope because non-local variable"),
            quote(mercury_var_to_string(VarSet, no, Var)),
            words("has inst"), quote("any"),
            words("and appears in the condition.")
        ]
    ;
        NegCtxtDesc = negation,
        Pieces = [
            words("purity error: negation"),
            words("should be inside a promise_purity"),
            words("scope because non-local variable"),
            quote(mercury_var_to_string(VarSet, no, Var)),
            words("has inst"), quote("any"),
            words("and appears in the body.")
        ]
    ),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [always(Preamble ++ Pieces)])]).

%-----------------------------------------------------------------------------%

:- func purity_error_lambda_should_be_any_to_spec(mode_info, list(prog_var))
    = error_spec.

purity_error_lambda_should_be_any_to_spec(ModeInfo, Vars) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [
        words("purity error: lambda is"), quote("ground"),
        words("but contains the following non-local variables"),
        words("whose insts contain"), quote("any"), suffix(":"),
        words(mercury_vars_to_string(VarSet, no, Vars)), suffix("."), nl
    ],
    Always = always(Preamble ++ Pieces),
    VerboseOnly = verbose_only([
        words("Predicate expressions with inst"), quote("any"),
        words("can be written"), quote("any_pred(Args) is det :- ..."),
        suffix("."),
        words("Function expressions with inst"), quote("any"),
        words("can be written"),
        quote("any_func(Args) = Result is det :- ..."), suffix(".")
    ]),
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(Context, [Always, VerboseOnly])]).

%-----------------------------------------------------------------------------%

should_report_mode_warning_for_pred_origin(origin_special_pred(_)) = no.
should_report_mode_warning_for_pred_origin(origin_instance_method(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_transformed(_, _, _)) = no.
should_report_mode_warning_for_pred_origin(origin_created(_)) = no.
should_report_mode_warning_for_pred_origin(origin_assertion(_, _)) = no.
should_report_mode_warning_for_pred_origin(origin_lambda(_, _, _)) = yes.
should_report_mode_warning_for_pred_origin(origin_user(_)) = yes.

%-----------------------------------------------------------------------------%

maybe_report_error_no_modes(ModuleInfo, PredId, PredInfo) = Specs :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    module_info_get_globals(ModuleInfo, Globals),
    ( ImportStatus = status_local ->
        globals.lookup_bool_option(Globals, infer_modes, InferModesOpt),
        (
            InferModesOpt = yes,
            Specs = []
        ;
            InferModesOpt = no,
            pred_info_get_context(PredInfo, Context),
            MainPieces = [words("Error: no mode declaration for")] ++
                describe_one_pred_name(ModuleInfo, should_not_module_qualify,
                    PredId) ++ [suffix("."), nl],
            VerbosePieces =
                [words("(Use"), quote("--infer-modes"),
                words("to enable mode inference.)"), nl],
            Spec = error_spec(severity_error,
                phase_mode_check(report_in_any_mode),
                [simple_msg(Context,
                    [always(MainPieces), verbose_only(VerbosePieces)])]),
            Specs = [Spec]
        )
    ;
        pred_info_get_context(PredInfo, Context),
        Pieces = [words("Error: no mode declaration for exported")] ++
            describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
            ++ [suffix("."), nl],
        Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
            [simple_msg(Context, [always(Pieces)])]),
        Specs = [Spec]
    ).

%-----------------------------------------------------------------------------%

    % Write out the inferred `mode' declarations for a list of pred_ids.
    %
report_mode_inference_messages(_, _, []) = [].
report_mode_inference_messages(ModuleInfo, OutputDetism, [PredId | PredIds]) =
        Specs :-
    TailSpecs = report_mode_inference_messages(ModuleInfo, OutputDetism,
        PredIds),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( check_marker(Markers, marker_infer_modes) ->
        ProcIds = pred_info_all_procids(PredInfo),
        pred_info_get_procedures(PredInfo, Procs),
        HeadSpecs = report_mode_inference_messages_2(ModuleInfo, OutputDetism,
            PredInfo, Procs, ProcIds),
        Specs = HeadSpecs ++ TailSpecs
    ;
        Specs = TailSpecs
    ).

    % Write out the inferred `mode' declarations for a list of proc_ids.
    %
:- func report_mode_inference_messages_2(module_info, include_detism_on_modes,
    pred_info, proc_table, list(proc_id)) = list(error_spec).

report_mode_inference_messages_2(_, _, _, _, []) = [].
report_mode_inference_messages_2(ModuleInfo, OutputDetism, PredInfo, Procs,
        [ProcId | ProcIds]) = Specs :-
    TailSpecs = report_mode_inference_messages_2(ModuleInfo, OutputDetism,
        PredInfo, Procs, ProcIds),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    map.lookup(Procs, ProcId, ProcInfo),
    (
        (
            % We always output `Inferred :- mode ...'
            proc_info_is_valid_mode(ProcInfo)
        ;
            % We only output `REJECTED :- mode ...'
            % if --verbose-errors is enabled
            VerboseErrors = yes
        )
    ->
        HeadSpec = report_mode_inference_message(ModuleInfo, OutputDetism,
            PredInfo, ProcInfo),
        Specs = [HeadSpec | TailSpecs]
    ;
        Specs = TailSpecs
    ).

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
        ( list.drop(NumToDrop, !ArgModes) ->
            true
        ;
            unexpected($module, $pred, "list.drop failed")
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
        ( proc_info_is_valid_mode(ProcInfo) ->
            Verb = "Inferred"
        ;
            Verb = "REJECTED",
            % Replace the final insts with dummy insts '...',
            % since they won't be valid anyway -- they are just
            % the results of whatever partial inference we did
            % before detecting the error.
            mode_list_get_initial_insts(ModuleInfo, !.ArgModes, InitialInsts),
            DummyInst = defined_inst(user_inst(unqualified("..."), [])),
            list.duplicate(PredArity, DummyInst, FinalInsts),
            !:ArgModes = list.map(func(I - F) = (I -> F),
                assoc_list.from_corresponding_lists(InitialInsts, FinalInsts)),
            % Likewise delete the determinism.
            !:MaybeDet = no
        ),
        strip_builtin_qualifiers_from_mode_list(!ArgModes),
        (
            PredOrFunc = pf_predicate,
            Detail = mercury_pred_mode_decl_to_string(VarSet, Name,
                !.ArgModes, !.MaybeDet, Context)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(!.ArgModes, FuncArgModes, RetMode),
            Detail = mercury_func_mode_decl_to_string(VarSet, Name,
                FuncArgModes, RetMode, !.MaybeDet, Context)
        ),
        Pieces = [words(Verb), words(Detail), nl],
        Msg = simple_msg(Context,
            [option_is_set(inform_inferred_modes, yes, [always(Pieces)])]),
        Severity = severity_conditional(inform_inferred_modes, yes,
            severity_informational, no),
        Spec = error_spec(Severity,
            phase_mode_check(report_in_any_mode), [Msg])
    ).

%-----------------------------------------------------------------------------%

report_indistinguishable_modes_error(ModuleInfo, OldProcId, NewProcId,
        PredId, PredInfo) = Spec :-
    pred_info_get_procedures(PredInfo, Procs),
    map.lookup(Procs, OldProcId, OldProcInfo),
    map.lookup(Procs, NewProcId, NewProcInfo),
    proc_info_get_context(OldProcInfo, OldContext),
    proc_info_get_context(NewProcInfo, NewContext),

    MainPieces = [words("In mode declarations for ")] ++
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
        ++ [suffix(":"), nl, words("error: duplicate mode declaration."), nl],
    VerbosePieces = [words("Modes"),
        words_quote(mode_decl_to_string(OldProcId, PredInfo)),
        words("and"),
        words_quote(mode_decl_to_string(NewProcId, PredInfo)),
        words("are indistinguishable.")],
    OldPieces = [words("Here is the conflicting mode declaration.")],
    Spec = error_spec(severity_error, phase_mode_check(report_in_any_mode),
        [simple_msg(NewContext,
            [always(MainPieces), verbose_only(VerbosePieces)]),
        simple_msg(OldContext, [always(OldPieces)])]).

%-----------------------------------------------------------------------------%

mode_decl_to_string(ProcId, PredInfo) = String :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name0 = pred_info_name(PredInfo),
    Name = unqualified(Name0),
    pred_info_get_procedures(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    proc_info_get_declared_determinism(ProcInfo, MaybeDet),
    proc_info_get_context(ProcInfo, Context),
    varset.init(InstVarSet),
    strip_builtin_qualifiers_from_mode_list(Modes0, Modes),
    String = mercury_mode_subdecl_to_string(PredOrFunc, InstVarSet, Name,
        Modes, MaybeDet, Context).

:- func inst_to_string(mode_info, mer_inst) = string.

inst_to_string(ModeInfo, Inst0) = Str :-
    strip_builtin_qualifiers_from_inst(Inst0, Inst),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    Str = mercury_expanded_inst_to_string(Inst, InstVarSet, ModuleInfo).

:- func inst_list_to_string(mode_info, list(mer_inst)) = string.

inst_list_to_string(ModeInfo, Insts) =
    string.join_list(", ", list.map(inst_to_string(ModeInfo), Insts)).

:- func inst_list_to_sep_lines(mode_info, list(mer_inst))
    = list(format_component).

inst_list_to_sep_lines(_ModeInfo, []) = [].
inst_list_to_sep_lines(ModeInfo, [Inst | Insts]) = Pieces ++ MorePieces :-
    (
        Insts = [],
        Comma = [],
        Newline = nl_indent_delta(-1)
    ;
        Insts = [_ | _],
        Comma = [suffix(",")],
        Newline = nl
    ),
    Pieces = [words(inst_to_string(ModeInfo, Inst))] ++ Comma ++ [Newline],
    MorePieces = inst_list_to_sep_lines(ModeInfo, Insts).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.mode_errors.
%-----------------------------------------------------------------------------%
