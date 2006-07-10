%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
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
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_errors.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_goal.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module check_hlds.mode_info.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type merge_context
    --->    disj
    ;       if_then_else.

:- type merge_error  == pair(prog_var, list(mer_inst)).
:- type merge_errors == list(merge_error).

:- type delayed_goal
    --->    delayed_goal(
                set(prog_var),      % The vars it's waiting on.
                mode_error_info,    % The reason it can't be scheduled.
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

    ;       mode_error_unify_var_functor(prog_var, cons_id, list(prog_var),
                mer_inst, list(mer_inst))
            % Attempt to unify a free var with a functor containing
            % free arguments.

    ;       mode_error_unify_var_lambda(prog_var, mer_inst, mer_inst)
            % Some sort of error in attempt to unify a variable with lambda
            % expression.

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

    ;       purity_error_lambda_should_be_impure(list(prog_var)).
            % A lambda term containing inst any non-locals should have been
            % declared impure, but hasn't been (executing such a lambda may
            % further constrain the inst any variables, thereby violating
            % referential transparency).

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
                set(prog_var),      % The variables which caused the error
                                    % (we will attempt to reschedule the goal
                                    % if the one of these variables becomes
                                    % more instantiated).
                mode_error,         % The nature of the error.
                prog_context,       % Where the error occurred.
                mode_context        % Where the error occurred.
            ).

:- type mode_warning
    --->    cannot_succeed_var_var(prog_var, prog_var, mer_inst, mer_inst)
    ;       cannot_succeed_var_functor(prog_var, mer_inst, cons_id).

:- type mode_warning_info
    --->    mode_warning_info(
                mode_warning,       % The nature of the error.
                prog_context,       % Where the error occurred.
                mode_context        % Where the error occurred.
            ).

%-----------------------------------------------------------------------------%

    % If there were any errors recorded in the mode_info,
    % report them to the user now.
    %
:- pred report_mode_errors(mode_info::in, mode_info::out,
    io::di, io::uo) is det.

    % Print an error message describing a mode error.
    %
:- pred report_mode_error(mode_error::in, mode_info::in,
    io::di, io::uo) is det.

    % Report an error for a predicate with no mode declarations
    % unless mode inference is enabled and the predicate is local.
    %
:- pred maybe_report_error_no_modes(pred_id::in, pred_info::in,
    module_info::in, io::di, io::uo) is det.

    % If there were any warnings recorded in the mode_info,
    % report them to the user now.
    %
:- pred report_mode_warnings(mode_info::in, mode_info::out,
    io::di, io::uo) is det.

    % Print a warning message.
    %
:- pred report_mode_warning(mode_info::in, mode_warning_info::in,
    io::di, io::uo) is det.

    % Initialize the mode_context.
    %
:- pred mode_context_init(mode_context::out) is det.

    % Write out the inferred `mode' declarations for a list of pred_ids.
    % The bool indicates whether or not to write out determinism
    % annotations on the modes (it should only be set to `yes' _after_
    % determinism analysis).
    %
:- pred write_mode_inference_messages(list(pred_id)::in, bool::in,
    module_info::in, io::di, io::uo) is det.

    % Report an error for the case when two mode declarations
    % declare indistinguishable modes.
    %
:- pred report_indistinguishable_modes_error(proc_id::in, proc_id::in,
    pred_id::in, pred_info::in, module_info::in, io::di, io::uo) is det.

:- pred output_mode_decl(proc_id::in, pred_info::in, io::di, io::uo) is det.
:- func mode_decl_to_string(proc_id, pred_info) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_info.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

report_mode_error(ModeError, ModeInfo, !IO) :-
    ExtendedSpecs = mode_error_to_specs(ModeError, ModeInfo),
    % XXX This module needs some rearrangement for the global extra erro info
    % flag to be respected properly.  In the meantime we just set it to yes
    % because that was the original behaviour for this module was.
    globals.io_set_extra_error_info(yes, !IO),
    write_extended_error_specs(ExtendedSpecs, !IO).

report_mode_warning(ModeInfo, Warning, !IO) :-
    Specs = mode_warning_to_specs(ModeInfo, Warning),
    write_error_specs(Specs, !IO),
    record_warning(!IO).

:- func mode_error_to_specs(mode_error::in, mode_info::in)
    = (list(extended_error_msg_spec)::out(extended_error_msg_specs)) is det.

mode_error_to_specs(ModeError, ModeInfo) = ExtendedSpecs :-
    (
        ModeError = mode_error_disj(MergeContext, ErrorList),
        Specs = mode_error_disj_to_specs(ModeInfo, MergeContext, ErrorList),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_par_conj(ErrorList),
        Specs = mode_error_par_conj_to_specs(ModeInfo, ErrorList),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_higher_order_pred_var(PredOrFunc, Var, Inst,
            Arity),
        Specs = mode_error_higher_order_pred_var_to_specs(ModeInfo, PredOrFunc,
            Var, Inst, Arity),
            extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_poly_unify(Var, Inst),
        Specs = mode_error_poly_unify_to_specs(ModeInfo, Var, Inst),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_var_is_live(Var),
        Specs = mode_error_var_is_live_to_specs(ModeInfo, Var),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_var_has_inst(Var, InstA, InstB),
        Specs = mode_error_var_has_inst_to_specs(ModeInfo, Var, InstA, InstB),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_unify_pred(Var, RHS, Type, PredOrFunc),
        Specs = mode_error_unify_pred_to_specs(ModeInfo, Var, RHS, Type,
            PredOrFunc),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_implied_mode(Var, InstA, InstB),
        Specs = mode_error_implied_mode_to_specs(ModeInfo, Var, InstA, InstB),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_no_mode_decl,
        Specs = mode_error_no_mode_decl_to_specs(ModeInfo),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_bind_var(Reason, Var, InstA, InstB),
        Specs = mode_error_bind_var_to_specs(ModeInfo, Reason, Var,
            InstA, InstB),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_non_local_lambda_var(Var, Inst),
        Specs = mode_error_non_local_lambda_var_to_specs(ModeInfo, Var, Inst),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_unify_var_var(VarA, VarB, InstA, InstB),
        Specs = mode_error_unify_var_var_to_specs(ModeInfo, VarA, VarB,
            InstA, InstB),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_unify_var_lambda(VarA, InstA, InstB),
        Specs = mode_error_unify_var_lambda_to_specs(ModeInfo, VarA,
            InstA, InstB),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_unify_var_functor(Var, Name, Args, Inst,
            ArgInsts),
        Specs = mode_error_unify_var_functor_to_specs(ModeInfo, Var, Name,
            Args, Inst, ArgInsts),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_conj(Errors, Culprit),
        ExtendedSpecs = mode_error_conj_to_specs(ModeInfo, Errors, Culprit)
    ;
        ModeError = mode_error_no_matching_mode(Vars, Insts),
        Specs = mode_error_no_matching_mode_to_specs(ModeInfo, Vars, Insts),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = mode_error_in_callee(Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors),
        ExtendedSpecs = mode_error_in_callee_to_specs(ModeInfo, Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors)
    ;
        ModeError = mode_error_final_inst(ArgNum, Var, VarInst, Inst, Reason),
        Specs = mode_error_final_inst_to_specs(ModeInfo, ArgNum, Var, VarInst,
            Inst, Reason),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = purity_error_should_be_in_promise_purity_scope(NegCtxt,
            Var),
        Specs = purity_error_should_be_in_promise_purity_scope_to_specs(
            NegCtxt, ModeInfo, Var),
        extend_specs(Specs, ExtendedSpecs)
    ;
        ModeError = purity_error_lambda_should_be_impure(Vars),
        Specs = purity_error_lambda_should_be_impure_to_specs(ModeInfo, Vars),
        extend_specs(Specs, ExtendedSpecs)
    ).

:- func mode_warning_to_specs(mode_info, mode_warning_info)
    = list(error_msg_spec).

mode_warning_to_specs(!.ModeInfo, Warning) = Specs :-
    Warning = mode_warning_info(ModeWarning, Context, ModeContext),
    mode_info_set_context(Context, !ModeInfo),
    mode_info_set_mode_context(ModeContext, !ModeInfo),
    (
        ModeWarning = cannot_succeed_var_var(VarA, VarB, InstA, InstB),
        Specs = mode_warning_cannot_succeed_var_var(!.ModeInfo, VarA, VarB,
            InstA, InstB)
    ;
        ModeWarning = cannot_succeed_var_functor(Var, Inst, ConsId),
        Specs = mode_warning_cannot_succeed_var_functor(!.ModeInfo,
            Var, Inst, ConsId)
    ).

%-----------------------------------------------------------------------------%

:- func mode_error_conj_to_specs(mode_info::in, list(delayed_goal)::in,
    schedule_culprit::in)
    = (list(extended_error_msg_spec)::out(extended_error_msg_specs)) is det.

mode_error_conj_to_specs(ModeInfo, Errors, Culprit) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    list.filter(is_error_important, Errors, ImportantErrors, OtherErrors),

    % If there's more than one error, and we have verbose-errors enabled,
    % report them all.
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    (
        VerboseErrors = yes,
        Errors = [_, _ | _]
    ->
        PiecesA = [words("mode error in conjunction. The next"),
            fixed(int_to_string(list.length(Errors))),
            words("error messages indicate possible causes of this error.")],
        Specs1Start = [plain_spec(mode_info_context_to_spec(ModeInfo)),
            plain_spec(error_msg_spec(no, Context, 0, PiecesA))],
        Specs1Rest = list.map(
            mode_error_conjunct_to_specs(VarSet, Context, ModeInfo),
            ImportantErrors ++ OtherErrors),
        Specs1 = Specs1Start ++ list.condense(Specs1Rest)
    ;
        % In the normal case, only report the first error.
        ImportantErrors = [FirstImportantError | _]
    ->
        Specs1 = mode_error_conjunct_to_specs(VarSet, Context, ModeInfo,
            FirstImportantError)
    ;
        OtherErrors = [FirstOtherError | _]
    ->
        Specs1 = mode_error_conjunct_to_specs(VarSet, Context, ModeInfo,
            FirstOtherError)
    ;
        % There wasn't any error to report!  This can't happen.
        unexpected(this_file, "report_mode_error_conj")
    ),

    % If the goal(s) couldn't be scheduled because we couldn't reorder things
    % past an impure goal, then report that.
    (
        Culprit = conj_floundered,
        % We've already reported everything we can.
        Specs2 = []
    ;
        Culprit = goal_itself_was_impure,
        Pieces = [words("The goal could not be reordered,"),
            words("because it was impure.")],
        Specs2 = [plain_spec(error_msg_spec(no, Context, 0, Pieces))]
    ;
        Culprit = goals_followed_by_impure_goal(ImpureGoal),
        ImpureGoal = _ - ImpureGoalInfo,
        goal_info_get_context(ImpureGoalInfo, ImpureGoalContext),
        Pieces1 = [words("The goal could not be reordered,"),
            words("because it was followed by an impure goal.")],
        Pieces2 = [words("This is the location of the impure goal.")],
        Specs2 = [plain_spec(error_msg_spec(no, Context, 0, Pieces1)),
            plain_spec(error_msg_spec(no, ImpureGoalContext, 0, Pieces2))]
    ),
    Specs = Specs1 ++ Specs2.

:- pred is_error_important(delayed_goal::in) is semidet.

is_error_important(Error) :-
    Error = delayed_goal(_, mode_error_info(_, ModeError, _, ModeContext), _),
    (
        % An error is important unless it is a non-explicit unification,
        % i.e. a head unification or a call argument unification.
        ModeContext = unify(unify_context(UnifyContext, _), _),
        UnifyContext \= explicit,

        % Except that errors in lambda goals are important even if the
        % unification that creates the lambda goal is an implicit one.
        ModeError \= mode_error_non_local_lambda_var(_, _)
    ->
        fail
    ;
        true
    ).

:- func mode_error_conjunct_to_specs(prog_varset::in, prog_context::in,
    mode_info::in, delayed_goal::in)
    = (list(extended_error_msg_spec)::out(extended_error_msg_specs)) is det.

mode_error_conjunct_to_specs(VarSet, Context, !.ModeInfo, DelayedGoal)
        = Specs :-
    DelayedGoal = delayed_goal(Vars, Error, Goal),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_modes, Debug),
    (
        Debug = yes,
        set.to_sorted_list(Vars, VarList),
        Pieces1 = [words("Floundered goal, waiting on { "),
            words(mercury_vars_to_string(VarList, VarSet, no)),
            words(" } :"), nl],
        Specs1 = [plain_spec(error_msg_spec(no, Context, 0, Pieces1))]
    ;
        Debug = no,
        Specs1 = []
    ),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        Specs2 = [anything(write_indented_goal(Goal, ModuleInfo, VarSet))]
    ;
        VeryVerbose = no,
        Specs2 = []
    ),
    Error = mode_error_info(_, ModeError, ErrorContext, ModeContext),
    mode_info_set_context(ErrorContext, !ModeInfo),
    mode_info_set_mode_context(ModeContext, !ModeInfo),
    Specs = Specs1 ++ Specs2 ++ mode_error_to_specs(ModeError, !.ModeInfo).

:- pred write_indented_goal(hlds_goal::in, module_info::in, prog_varset::in,
    io::di, io::uo) is det.

write_indented_goal(Goal, ModuleInfo, VarSet, !IO) :-
    io.write_string("\t\t", !IO),
    hlds_out.write_goal(Goal, ModuleInfo, VarSet, no, 2, ".\n", !IO).

%-----------------------------------------------------------------------------%

:- func mode_error_disj_to_specs(mode_info, merge_context, merge_errors)
    = list(error_msg_spec).

mode_error_disj_to_specs(ModeInfo, MergeContext, ErrorList) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    Pieces = [words("mode mismatch in "),
        words(merge_context_to_string(MergeContext)), suffix(".")],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces) |
        list.map(merge_error_to_spec(ModeInfo), ErrorList)].

:- func mode_error_par_conj_to_specs(mode_info, merge_errors)
    = list(error_msg_spec).

mode_error_par_conj_to_specs(ModeInfo, ErrorList) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    Pieces = [words("mode error: mutually exclusive bindings"),
        words("in parallel conjunction."),
        words("(The current implementation does not permit"),
        words("parallel conjunctions to fail.)"), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces) |
        list.map(merge_error_to_spec(ModeInfo), ErrorList)].

:- func merge_error_to_spec(mode_info, merge_error) = error_msg_spec.

merge_error_to_spec(ModeInfo, Var - Insts) = Spec :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        fixed("::"),
        words(inst_list_to_string(ModeInfo, Insts)), suffix("."), nl],
    Spec = error_msg_spec(no, Context, 0, Pieces).

:- func merge_context_to_string(merge_context) = string.

merge_context_to_string(disj) = "disjunction".
merge_context_to_string(if_then_else) = "if-then-else".

%-----------------------------------------------------------------------------%

:- func mode_error_bind_var_to_specs(mode_info, var_lock_reason,
    prog_var, mer_inst, mer_inst) = list(error_msg_spec).

mode_error_bind_var_to_specs(ModeInfo, Reason, Var, VarInst, Inst) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        Reason = negation,
        ReasonStr = "attempt to bind a variable inside a negation."
    ;
        Reason = if_then_else,
        ReasonStr = "attempt to bind a non-local variable" ++
            " inside the condition of an if-then-else."
    ;
        Reason = lambda(PredOrFunc),
        PredOrFuncS = prog_out.pred_or_func_to_str(PredOrFunc),
        ReasonStr = "attempt to bind a non-local variable inside" ++
            " a " ++ PredOrFuncS ++ " lambda goal."
    ;
        Reason = par_conj,
        ReasonStr = "attempt to bind a non-local variable" ++
            "inside more than one parallel conjunct."
    ),
    Pieces1 = [words("scope error:"), words(ReasonStr), nl,
        words("Variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))), suffix(","), nl,
        words("expected instantiatedness was"),
        words(add_quotes(inst_to_string(ModeInfo, Inst))), suffix("."), nl],
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    (
        VerboseErrors = yes,
        (
            Reason = negation,
            Pieces2 = [words("A negation is only allowed to bind variables"),
                words("which are local to the negation, i.e. those which are"),
                words("implicitly existentially quantified"),
                words("inside the scope of the negation."), nl]
        ;
            Reason = if_then_else,
            Pieces2 = [words("The condition of an if-then-else is only"),
                words("allowed to bind variables which are local to the"),
                words("condition or which occur only in the condition"),
                words("and the `then' part."), nl]
        ;
            Reason = lambda(_),
            Pieces2 = [words("A lambda goal is only allowed to bind"),
                words("its arguments and variables local to the "),
                words("lambda expression."), nl]
        ;
            Reason = par_conj,
            Pieces2 = [words("A nonlocal variable of a parallel conjunction"),
                words("may be bound in at most one conjunct."), nl]
        )
    ;
        VerboseErrors = no,
        % XXX We need to set the extra error flag here.
        Pieces2 = []
    ),
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces1 ++ Pieces2)].

%-----------------------------------------------------------------------------%

:- func mode_error_non_local_lambda_var_to_specs(mode_info, prog_var, mer_inst)
    = list(error_msg_spec).

mode_error_non_local_lambda_var_to_specs(ModeInfo, Var, VarInst) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error: variable"),
        fixed(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))),
        suffix(","), nl,
        words("expected instantiatedness for non-local variables"),
        words("of lambda goals is `ground'."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

%-----------------------------------------------------------------------------%

:- func mode_error_in_callee_to_specs(mode_info::in, list(prog_var)::in,
    list(mer_inst)::in, pred_id::in, proc_id::in, list(mode_error_info)::in)
    = (list(extended_error_msg_spec)::out(extended_error_msg_specs)) is det.

mode_error_in_callee_to_specs(!.ModeInfo, Vars, Insts,
        CalleePredId, CalleeProcId, CalleeModeErrors) = Specs :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_varset(!.ModeInfo, VarSet),
    Pieces1 = [words("mode error: arguments"),
        words(add_quotes(mercury_vars_to_string(Vars, VarSet, no))),
        words("have the following insts:"), nl_indent_delta(1)] ++
        inst_list_to_sep_lines(!.ModeInfo, Insts) ++
        [words("which does not match any of the valid modes for")],
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    (
        VerboseErrors = yes,
        Pieces2 = [words("the callee"), prefix("(")] ++
            describe_one_pred_name(ModuleInfo, should_module_qualify,
                CalleePredId) ++ [suffix(")"), nl,
            words("because of the following error."), nl]
    ;
        VerboseErrors = no,
        Pieces2 = [words("the callee, because of the following error."), nl]
    ),
    InitSpecs = [plain_spec(mode_info_context_to_spec(!.ModeInfo)),
        plain_spec(error_msg_spec(no, Context, 0, Pieces1 ++ Pieces2))],
    (
        CalleeModeErrors = [First | _],
        First = mode_error_info(_, CalleeModeError,
            CalleeContext, CalleeModeContext),
        mode_info_set_predid(CalleePredId, !ModeInfo),
        mode_info_set_procid(CalleeProcId, !ModeInfo),
        mode_info_set_context(CalleeContext, !ModeInfo),
        mode_info_set_mode_context(CalleeModeContext, !ModeInfo),
        LaterSpecs0 = mode_error_to_specs(CalleeModeError, !.ModeInfo),
        (
            LaterSpecs0 = [],
            LaterSpecs = []
        ;
            LaterSpecs0 = [LaterSpecsHead0 | LaterSpecsTail],
            (
                LaterSpecsHead0 = plain_spec(Spec0),
                Spec = Spec0 ^ spec_treat_as_first := yes,
                LaterSpecsHead = plain_spec(Spec)
            ;
                LaterSpecsHead0 = anything(_),
                LaterSpecsHead = LaterSpecsHead0
            ),
            LaterSpecs = [LaterSpecsHead | LaterSpecsTail]
        ),
        Specs = InitSpecs ++ LaterSpecs
    ;
        CalleeModeErrors = [],
        unexpected(this_file, "report_mode_error_in_callee: no error")
    ).

:- func mode_error_no_matching_mode_to_specs(mode_info, list(prog_var),
    list(mer_inst)) = list(error_msg_spec).

mode_error_no_matching_mode_to_specs(ModeInfo, Vars, Insts) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    ( ModeContext = call(CallId, _) ->
        CallIdStr = hlds_out.call_id_to_string(CallId)
    ;
        unexpected(this_file,
            "report_mode_error_no_matching_mode: invalid context")
    ),
    Pieces = [words("mode error: arguments"),
        fixed(add_quotes(mercury_vars_to_string(Vars, VarSet, no))),
        words("have the following insts:"), nl_indent_delta(1)] ++
        inst_list_to_sep_lines(ModeInfo, Insts) ++
        [words("which does not match any of the modes for"),
        words(CallIdStr), suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

:- func mode_error_higher_order_pred_var_to_specs(mode_info, pred_or_func,
    prog_var, mer_inst, arity) = list(error_msg_spec).

mode_error_higher_order_pred_var_to_specs(ModeInfo, PredOrFunc, Var, VarInst,
        Arity) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        PredOrFunc = predicate,
        Expecting = "expecting higher-order pred inst (of arity " ++
            int_to_string(Arity) ++ ")."
    ;
        PredOrFunc = function,
        Expecting = "expecting higher-order func inst (of arity " ++
            int_to_string(Arity - 1) ++ ")."
    ),
    Pieces = [words("mode error: variable"),
        fixed(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))),
        suffix(","), nl,
        words(Expecting), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

:- func mode_error_poly_unify_to_specs(mode_info, prog_var, mer_inst)
    = list(error_msg_spec).

mode_error_poly_unify_to_specs(ModeInfo, Var, VarInst) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces1 = [words("in polymorphically-typed unification:"), nl,
        words("mode error: variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))), suffix(","), nl,
        words("expected instantiatedness was `ground' or `any'."), nl],
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    (
        VerboseErrors = yes,
        Pieces2 = [words("When unifying two variables whose type"),
            words("will not be known until runtime, the variables must both"),
            words("be ground (or have inst `any'). Unifications of"),
            words("polymorphically-typed variables with partially"),
            words("instantiated modes are not allowed.")]
    ;
        VerboseErrors = no,
        Pieces2 = []
    ),
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces1 ++ Pieces2)].

:- func mode_error_var_is_live_to_specs(mode_info, prog_var)
    = list(error_msg_spec).

mode_error_var_is_live_to_specs(ModeInfo, Var) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("unique-mode error: the called procedure"),
        words("would clobber its argument, but variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("is still live."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

:- func mode_error_var_has_inst_to_specs(mode_info, prog_var,
    mer_inst, mer_inst) = list(error_msg_spec).

mode_error_var_has_inst_to_specs(ModeInfo, Var, VarInst, Inst) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error: variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))), suffix(","), nl,
        words("expected instantiatedness was"),
        words(add_quotes(inst_to_string(ModeInfo, Inst))), suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

:- func mode_error_implied_mode_to_specs(mode_info, prog_var,
    mer_inst, mer_inst) = list(error_msg_spec).

mode_error_implied_mode_to_specs(ModeInfo, Var, VarInst, Inst) = Specs :-
        % This "error" message is really a "sorry, not implemented" message.
        % We only print the message if we will actually generating code.
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, errorcheck_only, ErrorcheckOnly),
    (
        ErrorcheckOnly = no,
        mode_info_get_context(ModeInfo, Context),
        mode_info_get_varset(ModeInfo, VarSet),
        Pieces = [words("sorry, implied modes not implemented."), nl,
            words("Variable"),
            words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
            words("has instantiatedness"),
            words(add_quotes(inst_to_string(ModeInfo, VarInst))),
            suffix(","), nl,
            words("expected instantiatedness was"),
            words(add_quotes(inst_to_string(ModeInfo, Inst))),
            suffix("."), nl],
        Specs = [mode_info_context_to_spec(ModeInfo),
            error_msg_spec(no, Context, 0, Pieces)]
    ;
        ErrorcheckOnly = yes,
        Specs = []
    ).

:- func mode_error_no_mode_decl_to_specs(mode_info::in)
    = (list(error_msg_spec)::out) is det.

mode_error_no_mode_decl_to_specs(ModeInfo) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    Pieces = [words("no mode declaration for called predicate."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

:- func mode_error_unify_pred_to_specs(mode_info, prog_var,
    mode_error_unify_rhs, mer_type, pred_or_func) = list(error_msg_spec).

mode_error_unify_pred_to_specs(ModeInfo, X, RHS, Type, PredOrFunc) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    (
        RHS = error_at_var(Y),
        RHSStr = mercury_var_to_string(Y, VarSet, no)
    ;
        RHS = error_at_functor(ConsId, ArgVars),
        RHSStr = hlds_out.functor_cons_id_to_string(ConsId, ArgVars, VarSet,
            ModuleInfo, no)
    ;
        RHS = error_at_lambda(ArgVars, ArgModes),
        RHSStr = "lambda(["
            ++ hlds_out.var_modes_to_string(ArgVars, ArgModes, VarSet,
                InstVarSet, no)
            ++ "] ... )"
    ),
    varset.init(TypeVarSet),
    Pieces1 = [words("In unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("with"), words(add_quotes(RHSStr)), suffix(":"), nl,
        words("mode error: attempt at higher-order unification."), nl,
        words("Cannot unify two terms of type"),
        words(add_quotes(mercury_type_to_string(TypeVarSet, no, Type))),
        suffix("."), nl],
    globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
    (
        VerboseErrors = yes,
        Pieces2 = [words("Your code is trying to test whether two "),
            words(prog_out.pred_or_func_to_full_str(PredOrFunc) ++ "s"),
            words("are equal, by unifying them."),
            words("In the general case, testing equivalence of"),
            words(prog_out.pred_or_func_to_full_str(PredOrFunc) ++ "s"),
            words("is an undecidable problem,"),
            words("and so this is not allowed by the Mercury mode system."),
            words("In some cases, you can achieve the same effect by"),
            words("writing an explicit universal quantification, e.g."),
            fixed("`all [X] call(P, X) <=> call(Q, X)',"),
            words("instead of"),
            fixed("`P = Q'.")]
    ;
        VerboseErrors = no,
        Pieces2 = []
    ),
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces1 ++ Pieces2)].

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_var_to_specs(mode_info, prog_var,
    prog_var, mer_inst, mer_inst) = list(error_msg_spec).

mode_error_unify_var_var_to_specs(ModeInfo, X, Y, InstX, InstY) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error in unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("and"),
        words(add_quotes(mercury_var_to_string(Y, VarSet, no))),
        suffix("."), nl,
        words("Variable"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstX))), suffix(","), nl,
        words("variable"),
        words(add_quotes(mercury_var_to_string(Y, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstY))), suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_lambda_to_specs(mode_info, prog_var,
    mer_inst, mer_inst) = list(error_msg_spec).

mode_error_unify_var_lambda_to_specs(ModeInfo, X, InstX, InstY) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("mode error in unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("and lambda expression."), nl,
        words("Variable"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstX))), suffix(","), nl,
        words("lambda expression has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstY))), suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

%-----------------------------------------------------------------------------%

:- func mode_error_unify_var_functor_to_specs(mode_info, prog_var,
    cons_id, list(prog_var), mer_inst, list(mer_inst)) = list(error_msg_spec).

mode_error_unify_var_functor_to_specs(ModeInfo, X, ConsId, Args,
        InstX, ArgInsts) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    Pieces1 = [words("mode error in unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("and"),
        words(add_quotes(hlds_out.functor_cons_id_to_string(ConsId, Args,
            VarSet, ModuleInfo, no))), suffix("."), nl,
        words("Variable"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstX))), suffix(","), nl,
        words("term"),
        words(add_quotes(hlds_out.functor_cons_id_to_string(ConsId, Args,
            VarSet, ModuleInfo, no)))],
    (
        Args = [_ | _],
        Pieces2 = [words("has instantiatedness"),
            prefix("`"),
            words(mercury_cons_id_to_string(ConsId, does_not_need_brackets)),
            suffix("("), nl_indent_delta(1)] ++
            inst_list_to_sep_lines(ModeInfo, ArgInsts) ++
            [fixed(")'.")]
    ;
        Args = [],
        Pieces2 = [words("has instantiatedness"),
            words(add_quotes(mercury_cons_id_to_string(ConsId,
                does_not_need_brackets))), suffix("."), nl]
    ),
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces1 ++ Pieces2)].

%-----------------------------------------------------------------------------%

:- func mode_warning_cannot_succeed_var_var(mode_info,
    prog_var, prog_var, mer_inst, mer_inst) = list(error_msg_spec).

mode_warning_cannot_succeed_var_var(ModeInfo, X, Y, InstX, InstY) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("and"),
        words(add_quotes(mercury_var_to_string(Y, VarSet, no))),
        words("cannot succeed"), nl,
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstX))), suffix(","), nl,
        words(add_quotes(mercury_var_to_string(Y, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstY))), suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

:- func mode_warning_cannot_succeed_var_functor(mode_info, prog_var, mer_inst,
    cons_id) = list(error_msg_spec).

mode_warning_cannot_succeed_var_functor(ModeInfo, X, InstX, ConsId) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words("warning: unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("and"),
        words(mercury_cons_id_to_string(ConsId, does_not_need_brackets)),
        words("cannot succeed"), nl,
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstX))), suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

%-----------------------------------------------------------------------------%

:- func mode_info_context_to_spec(mode_info) = error_msg_spec.

mode_info_context_to_spec(ModeInfo) = Spec :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_predid(ModeInfo, PredId),
    mode_info_get_procid(ModeInfo, ProcId),
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
    Pieces1 = [words("In clause for"),
        words(add_quotes(ModeSubDeclStr)), suffix(":"), nl],
    mode_info_get_mode_context(ModeInfo, ModeContext),
    Pieces2 = mode_context_to_pieces(ModeContext, PredMarkers),
    Spec = error_msg_spec(no, Context, 0, Pieces1 ++ Pieces2).

%-----------------------------------------------------------------------------%

    % XXX some parts of the mode context never get set up

:- func mode_context_to_pieces(mode_context, pred_markers)
    = list(format_component).

mode_context_to_pieces(uninitialized, _Markers) = [].
mode_context_to_pieces(call(CallId, ArgNum), Markers) =
    [words("in"),
        words(hlds_out.call_arg_id_to_string(CallId, ArgNum, Markers)),
        suffix(":"), nl].
mode_context_to_pieces(unify(UnifyContext, _Side), _Markers) = Pieces :-
    hlds_out.unify_context_to_pieces(no, _, UnifyContext, [], Pieces).

%-----------------------------------------------------------------------------%

mode_context_init(uninitialized).

%-----------------------------------------------------------------------------%

:- func mode_error_final_inst_to_specs(mode_info, int, prog_var,
    mer_inst, mer_inst, final_inst_error) = list(error_msg_spec).

mode_error_final_inst_to_specs(ModeInfo, ArgNum, Var, VarInst, Inst, Reason)
        = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    ( Reason = too_instantiated ->
        Problem = " became too instantiated."
    ; Reason = not_instantiated_enough ->
        Problem = "did not get sufficiently instantiated."
    ;
        % I don't think this can happen.  But just in case...
        Problem = "had the wrong instantiatedness."
    ),
    Pieces = [words("mode error: argument"), fixed(int_to_string(ArgNum)),
        words(Problem), nl,
        words("Final instantiatedness of"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("was"), words(add_quotes(inst_to_string(ModeInfo, VarInst))),
        suffix(","), nl,
        words("expected final instantiatedness was"),
        words(add_quotes(inst_to_string(ModeInfo, Inst))),
        suffix("."), nl],
    Specs = [mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)].

%-----------------------------------------------------------------------------%

:- func purity_error_should_be_in_promise_purity_scope_to_specs(
    negated_context_desc, mode_info, prog_var) = list(error_msg_spec).

purity_error_should_be_in_promise_purity_scope_to_specs(NegCtxtDesc,
        ModeInfo, Var) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    (
        NegCtxtDesc = if_then_else,
        Pieces = [
            words("purity error: if-then-else"),
            words("should be inside a promise_purity"),
            words("scope because non-local variable"),
            words(mercury_var_to_string(Var, VarSet, no)),
            words("has inst any and appears in the condition.")
        ]
    ;
        NegCtxtDesc = negation,
        Pieces = [
            words("purity error: negation"),
            words("should be inside a promise_purity"),
            words("scope because non-local variable"),
            words(mercury_var_to_string(Var, VarSet, no)),
            words("has inst any and appears in the body.")
        ]
    ),
    Specs = [
        mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)
    ].

%-----------------------------------------------------------------------------%

:- func purity_error_lambda_should_be_impure_to_specs(mode_info,
    list(prog_var)) = list(error_msg_spec).

purity_error_lambda_should_be_impure_to_specs(ModeInfo, Vars) = Specs :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [
        words("purity error: lambda should be impure because it"),
        words("contains the following non-local variables"),
        words("whose insts contain `any':"),
        words(mercury_vars_to_string(Vars, VarSet, no)),
        suffix("."), nl
    ],
    Specs = [
        mode_info_context_to_spec(ModeInfo),
        error_msg_spec(no, Context, 0, Pieces)
    ].

%-----------------------------------------------------------------------------%

maybe_report_error_no_modes(PredId, PredInfo, ModuleInfo, !IO) :-
    pred_info_get_import_status(PredInfo, ImportStatus),
    ( ImportStatus = local ->
        globals.io_lookup_bool_option(infer_modes, InferModesOpt, !IO),
        (
            InferModesOpt = yes
        ;
            InferModesOpt = no,
            io.set_exit_status(1, !IO),
            pred_info_context(PredInfo, Context),
            Pieces1 = [words("Error: no mode declaration for")] ++
                describe_one_pred_name(ModuleInfo, should_module_qualify,
                    PredId) ++ [suffix("."), nl],
            globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
            (
                VerboseErrors = yes,
                Pieces2 = [words("(Use `--infer-modes'"),
                    words("to enable mode inference.)"), nl]
            ;
                VerboseErrors = no,
                Pieces2 = []
            ),
            write_error_pieces(Context, 0, Pieces1 ++ Pieces2, !IO)
        )
    ;
        io.set_exit_status(1, !IO),
        pred_info_context(PredInfo, Context),
        Pieces = [words("Error: no mode declaration for exported")] ++
            describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
            ++ [suffix("."), nl],
        write_error_pieces(Context, 0, Pieces, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Write out the inferred `mode' declarations for a list of pred_ids.
    %
write_mode_inference_messages([], _, _, !IO).
write_mode_inference_messages([PredId | PredIds], OutputDetism, ModuleInfo,
        !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( check_marker(Markers, infer_modes) ->
        ProcIds = pred_info_all_procids(PredInfo),
        pred_info_get_procedures(PredInfo, Procs),
        write_mode_inference_messages_2(ProcIds, Procs, PredInfo,
            OutputDetism, ModuleInfo, !IO)
    ;
        true
    ),
    write_mode_inference_messages(PredIds, OutputDetism, ModuleInfo, !IO).

    % Write out the inferred `mode' declarations for a list of proc_ids.
    %
:- pred write_mode_inference_messages_2(list(proc_id)::in, proc_table::in,
    pred_info::in, bool::in, module_info::in, io::di, io::uo) is det.

write_mode_inference_messages_2([], _, _, _, _, !IO).
write_mode_inference_messages_2([ProcId | ProcIds], Procs, PredInfo,
        OutputDetism, ModuleInfo, !IO) :-
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
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
        write_mode_inference_message(PredInfo, ProcInfo, OutputDetism,
            ModuleInfo, !IO)
    ;
        true
    ),
    write_mode_inference_messages_2(ProcIds, Procs, PredInfo, OutputDetism,
        ModuleInfo, !IO).

    % Write out the inferred `mode' declaration for a single function
    % or predicate..
    %
:- pred write_mode_inference_message(pred_info::in, proc_info::in, bool::in,
    module_info::in, io::di, io::uo) is det.

write_mode_inference_message(PredInfo, ProcInfo, OutputDetism, ModuleInfo,
        !IO) :-
    PredName = pred_info_name(PredInfo),
    Name = unqualified(PredName),
    pred_info_context(PredInfo, Context),
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
            unexpected(this_file, "report_pred_proc_id: list.drop failed")
        ),

        varset.init(VarSet),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        (
            OutputDetism = yes,
            proc_info_get_inferred_determinism(ProcInfo, Detism),
            !:MaybeDet = yes(Detism)
        ;
            OutputDetism = no,
            !:MaybeDet = no
        ),
        ( proc_info_is_valid_mode(ProcInfo) ->
            Msg = "Inferred"
        ;
            Msg = "REJECTED",
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
            PredOrFunc = predicate,
            Detail = mercury_pred_mode_decl_to_string(VarSet, Name,
                !.ArgModes, !.MaybeDet, Context)
        ;
            PredOrFunc = function,
            pred_args_to_func_args(!.ArgModes, FuncArgModes, RetMode),
            Detail = mercury_func_mode_decl_to_string(VarSet, Name,
                FuncArgModes, RetMode, !.MaybeDet, Context)
        ),
        Pieces = [words(Msg), words(Detail), nl],
        write_error_pieces(Context, 0, Pieces, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

report_mode_errors(!ModeInfo, !IO) :-
    mode_info_get_errors(!.ModeInfo, Errors),
    (
        Errors = [FirstError | _],
        % XXX Document exactly why we only report the first.
        FirstError = mode_error_info(_, ModeError, Context, ModeContext),
        mode_info_set_context(Context, !ModeInfo),
        mode_info_set_mode_context(ModeContext, !ModeInfo),
        report_mode_error(ModeError, !.ModeInfo, !IO)
    ;
        Errors = []
    ).

report_mode_warnings(!ModeInfo, !IO) :-
    mode_info_get_warnings(!.ModeInfo, Warnings),
    list.foldl(report_mode_warning(!.ModeInfo), Warnings, !IO).

%-----------------------------------------------------------------------------%

report_indistinguishable_modes_error(OldProcId, NewProcId, PredId, PredInfo,
        ModuleInfo, !IO) :-
    io.set_exit_status(1, !IO),

    pred_info_get_procedures(PredInfo, Procs),
    map.lookup(Procs, OldProcId, OldProcInfo),
    map.lookup(Procs, NewProcId, NewProcInfo),
    proc_info_get_context(OldProcInfo, OldContext),
    proc_info_get_context(NewProcInfo, NewContext),

    Pieces1 = [words("In mode declarations for ")] ++
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
        ++ [suffix(":"), nl, words("error: duplicate mode declaration."), nl],
    Specs1 = [error_msg_spec(no, NewContext, 0, Pieces1)],

    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Pieces2 = [words("Modes"),
            fixed(add_quotes(mode_decl_to_string(OldProcId, PredInfo))),
            words("and"),
            fixed(add_quotes(mode_decl_to_string(NewProcId, PredInfo))),
            words("are indistinguishable.")],
        Specs2 = [error_msg_spec(no, NewContext, 0, Pieces2)]
    ;
        VerboseErrors = no,
        Specs2 = []
    ),

    Pieces3 = [words("Here is the conflicting mode declaration.")],
    Specs3 = [error_msg_spec(no, OldContext, 0, Pieces3)],
    write_error_specs(Specs1 `do_append` Specs2 `do_append` Specs3, !IO).

:- func do_append(list(T)::in(list_skel(I =< ground)),
    list(T)::in(list_skel(I =< ground))) =
    (list(T)::out(list_skel(I =< ground))) is det.

do_append([], L) = L.
do_append([H | T], L) = [H | NT] :-
    do_append(T, L) = NT.

%-----------------------------------------------------------------------------%

output_mode_decl(ProcId, PredInfo, !IO) :-
    io.write_string(mode_decl_to_string(ProcId, PredInfo), !IO).

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

:- pred output_inst(mer_inst::in, mode_info::in, io::di, io::uo) is det.

output_inst(Inst0, ModeInfo, !IO) :-
    io.write_string(inst_to_string(ModeInfo, Inst0), !IO).

:- func inst_to_string(mode_info, mer_inst) = string.

inst_to_string(ModeInfo, Inst0) = Str :-
    strip_builtin_qualifiers_from_inst(Inst0, Inst),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    Str = mercury_expanded_inst_to_string(Inst, InstVarSet, ModuleInfo).

:- pred output_inst_list(list(mer_inst)::in, mode_info::in, io::di, io::uo)
    is det.

output_inst_list(Insts, ModeInfo, !IO) :-
    io.write_string(inst_list_to_string(ModeInfo, Insts), !IO).

:- func inst_list_to_string(mode_info, list(mer_inst)) = string.

inst_list_to_string(ModeInfo, Insts) =
    string.join_list(", ", list.map(inst_to_string(ModeInfo), Insts)).

:- pred output_inst_list_sep_lines(prog_context::in, list(mer_inst)::in,
    mode_info::in, io::di, io::uo) is det.

output_inst_list_sep_lines(_Context, [], _, !IO).
output_inst_list_sep_lines(Context, [Inst | Insts], ModeInfo, !IO) :-
    prog_out.write_context(Context, !IO),
    io.write_string("    ", !IO),
    output_inst(Inst, ModeInfo, !IO),
    (
        Insts = []
    ;
        Insts = [_ | _],
        io.write_string(",", !IO)
    ),
    io.nl(!IO),
    output_inst_list_sep_lines(Context, Insts, ModeInfo, !IO).

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

:- func this_file = string.

this_file = "mode_errors.m".

%-----------------------------------------------------------------------------%
