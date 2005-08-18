%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mode_errors.m.
% Main author: fjh.

% This module contains all the error-reporting routines for the mode-checker.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module check_hlds__mode_errors.

:- interface.

:- import_module hlds__hlds_pred.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_goal.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.
:- import_module check_hlds__mode_info.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- type merge_context
    --->    disj
    ;       if_then_else.

:- type merge_error  == pair(prog_var, list(inst)).
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
            % bindings - ie the process of unifying the instmaps from the end
            % of each branch failed.

    ;       mode_error_higher_order_pred_var(pred_or_func, prog_var, inst,
                arity)
            % The predicate variable in a higher-order predicate or function
            % call didn't have a higher-order predicate or function inst
            % of the appropriate arity.

    ;       mode_error_poly_unify(prog_var, inst)
            % A variable in a polymorphic unification with unknown
            % type has inst other than `ground' or `any'.

    ;       mode_error_var_is_live(prog_var)
            % Call to a predicate which will clobber its argument,
            % but the argument is still live.

    ;       mode_error_var_has_inst(prog_var, inst, inst)
            % Call to a predicate with an insufficiently
            % instantiated variable (for preds with one mode).

    ;       mode_error_unify_pred(prog_var, mode_error_unify_rhs, type,
                pred_or_func)
            % An attempt was made to unify two higher-order
            % predicate or function variables.

    ;       mode_error_implied_mode(prog_var, inst, inst)
            % A call to a predicate with an overly instantiated variable
            % would use an implied mode of the predicate, but we can't
            % introduce a simple unification after calling the predicate in a
            % principal mode because the relevant variable has complex inst
            % (such as any).

    ;       mode_error_no_mode_decl
            % A call to a predicate for which there are no mode declarations
            % (and mode inference is not enabled).

    ;       mode_error_no_matching_mode(list(prog_var), list(inst))
            % Call to a predicate with an insufficiently instantiated variable
            % (for preds with >1 mode).

    ;       mode_error_in_callee(list(prog_var), list(inst),
                pred_id, proc_id, list(mode_error_info))
            % Call to a predicate with initial argument insts for which mode
            % inference gave a mode error in the callee.

    ;       mode_error_bind_var(var_lock_reason, prog_var, inst, inst)
            % Attempt to bind a non-local variable inside a negated context,
            % or attempt to re-bind a variable in a parallel conjunct.

    ;       mode_error_non_local_lambda_var(prog_var, inst)
            % Attempt to pass a live non-ground var as a non-local variable
            % to a lambda goal.

    ;       mode_error_unify_var_var(prog_var, prog_var, inst, inst)
            % Attempt to unify two free variables.

    ;       mode_error_unify_var_functor(prog_var, cons_id, list(prog_var),
                inst, list(inst))
            % Attempt to unify a free var with a functor containing
            % free arguments.

    ;       mode_error_unify_var_lambda(prog_var, inst, inst)
            % Some sort of error in attempt to unify a variable with lambda
            % expression.

    ;       mode_error_conj(list(delayed_goal), schedule_culprit)
            % A conjunction contains one or more unscheduleable goals;
            % schedule_culprit gives the reason why they couldn't be scheduled.

    ;       mode_error_final_inst(int, prog_var, inst, inst, final_inst_error).
            % One of the head variables did not have the expected final inst
            % on exit from the proc.

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
    ;       error_at_lambda(list(prog_var), list(mode)).

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

:- import_module check_hlds__mode_info.
:- import_module check_hlds__mode_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_out.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_mode.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

report_mode_error(ModeError, ModeInfo, !IO) :-
    (
        ModeError = mode_error_disj(MergeContext, ErrorList),
        report_mode_error_disj(ModeInfo, MergeContext, ErrorList, !IO)
    ;
        ModeError = mode_error_par_conj(ErrorList),
        report_mode_error_par_conj(ModeInfo, ErrorList, !IO)
    ;
        ModeError = mode_error_higher_order_pred_var(PredOrFunc, Var, Inst,
            Arity),
        report_mode_error_higher_order_pred_var(ModeInfo, PredOrFunc, Var,
            Inst, Arity, !IO)
    ;
        ModeError = mode_error_poly_unify(Var, Inst),
        report_mode_error_poly_unify(ModeInfo, Var, Inst, !IO)
    ;
        ModeError = mode_error_var_is_live(Var),
        report_mode_error_var_is_live(ModeInfo, Var, !IO)
    ;
        ModeError = mode_error_var_has_inst(Var, InstA, InstB),
        report_mode_error_var_has_inst(ModeInfo, Var, InstA, InstB, !IO)
    ;
        ModeError = mode_error_unify_pred(Var, RHS, Type, PredOrFunc),
        report_mode_error_unify_pred(ModeInfo, Var, RHS, Type, PredOrFunc, !IO)
    ;
        ModeError = mode_error_implied_mode(Var, InstA, InstB),
        report_mode_error_implied_mode(ModeInfo, Var, InstA, InstB, !IO)
    ;
        ModeError = mode_error_no_mode_decl,
        report_mode_error_no_mode_decl(ModeInfo, !IO)
    ;
        ModeError = mode_error_bind_var(Reason, Var, InstA, InstB),
        report_mode_error_bind_var(ModeInfo, Reason, Var, InstA, InstB, !IO)
    ;
        ModeError = mode_error_non_local_lambda_var(Var, Inst),
        report_mode_error_non_local_lambda_var(ModeInfo, Var, Inst, !IO)
    ;
        ModeError = mode_error_unify_var_var(VarA, VarB, InstA, InstB),
        report_mode_error_unify_var_var(ModeInfo, VarA, VarB, InstA, InstB,
            !IO)
    ;
        ModeError = mode_error_unify_var_lambda(VarA, InstA, InstB),
        report_mode_error_unify_var_lambda(ModeInfo, VarA, InstA, InstB, !IO)
    ;
        ModeError = mode_error_unify_var_functor(Var, Name, Args, Inst,
            ArgInsts),
        report_mode_error_unify_var_functor(ModeInfo, Var, Name, Args, Inst,
            ArgInsts, !IO)
    ;
        ModeError = mode_error_conj(Errors, Culprit),
        report_mode_error_conj(ModeInfo, Errors, Culprit, !IO)
    ;
        ModeError = mode_error_no_matching_mode(Vars, Insts),
        report_mode_error_no_matching_mode(ModeInfo, Vars, Insts, !IO)
    ;
        ModeError = mode_error_in_callee(Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors),
        report_mode_error_in_callee(ModeInfo, Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors, !IO)
    ;
        ModeError = mode_error_final_inst(ArgNum, Var, VarInst, Inst, Reason),
        report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst,
            Reason, !IO)
    ).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_conj(mode_info::in, list(delayed_goal)::in,
    schedule_culprit::in, io::di, io::uo) is det.

report_mode_error_conj(ModeInfo, Errors, Culprit, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    list__filter(is_error_important, Errors, ImportantErrors, OtherErrors),

    % If there's more than one error, and we have verbose-errors enabled,
    % report them all.
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Errors = [_, _ | _]
    ->
        mode_info_write_context(ModeInfo, !IO),
        PiecesA = [words("mode error in conjunction. The next"),
            fixed(int_to_string(list__length(Errors))),
            words("error messages indicate possible causes of this error.")],
        write_error_pieces_not_first_line(Context, 0, PiecesA, !IO),
        list__foldl(report_mode_error_conjunct(VarSet, Context, ModeInfo),
            ImportantErrors ++ OtherErrors, !IO)
    ;
        % In the normal case, only report the first error.
        ImportantErrors = [FirstImportantError | _]
    ->
        report_mode_error_conjunct(VarSet, Context, ModeInfo,
            FirstImportantError, !IO)
    ;
        OtherErrors = [FirstOtherError | _]
    ->
        report_mode_error_conjunct(VarSet, Context, ModeInfo, FirstOtherError,
            !IO)
    ;
        % There wasn't any error to report!  This can't happen.
        unexpected(this_file, "report_mode_error_conj")
    ),

    % If the goal(s) couldn't be scheduled because we couldn't reorder things
    % past an impure goal, then report that.
    (
        Culprit = conj_floundered
        % We've already reported everything we can.
    ;
        Culprit = goal_itself_was_impure,
        Pieces = [words("The goal could not be reordered,"),
            words("because it was impure.")],
        write_error_pieces_not_first_line(Context, 0, Pieces, !IO)
    ;
        Culprit = goals_followed_by_impure_goal(ImpureGoal),
        Pieces1 = [words("The goal could not be reordered,"),
            words("becaise it was followed by an impure goal.")],
        write_error_pieces_not_first_line(Context, 0, Pieces1, !IO),
        ImpureGoal = _ - ImpureGoalInfo,
        goal_info_get_context(ImpureGoalInfo, ImpureGoalContext),
        Pieces2 = [words("This is the location of the impure goal.")],
        write_error_pieces_not_first_line(ImpureGoalContext, 0, Pieces2, !IO)
    ).

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

:- pred report_mode_error_conjunct(prog_varset::in, prog_context::in,
    mode_info::in, delayed_goal::in, io::di, io::uo) is det.

report_mode_error_conjunct(VarSet, Context, !.ModeInfo,
        delayed_goal(Vars, Error, Goal), !IO) :-
    globals__io_lookup_bool_option(debug_modes, Debug, !IO),
    (
        Debug = yes,
        prog_out__write_context(Context, !IO),
        io__write_string("Floundered goal, waiting on { ", !IO),
        set__to_sorted_list(Vars, VarList),
        mercury_output_vars(VarList, VarSet, no, !IO),
        io__write_string(" } :\n", !IO)
    ;
        Debug = no
    ),
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        io__write_string("\t\t", !IO),
        hlds_out__write_goal(Goal, ModuleInfo, VarSet, no, 2, ".\n", !IO)
    ;
        VeryVerbose = no
    ),
    Error = mode_error_info(_, ModeError, ErrorContext, ModeContext),
    mode_info_set_context(ErrorContext, !ModeInfo),
    mode_info_set_mode_context(ModeContext, !ModeInfo),
    report_mode_error(ModeError, !.ModeInfo, !IO).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_disj(mode_info::in, merge_context::in,
    merge_errors::in, io::di, io::uo) is det.

report_mode_error_disj(ModeInfo, MergeContext, ErrorList, !IO) :-
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("mode mismatch in "),
        words(merge_context_to_string(MergeContext)), suffix(".")],
    mode_info_get_context(ModeInfo, Context),
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO),
    io__write_list(ErrorList, "", write_merge_error(ModeInfo), !IO).

:- pred report_mode_error_par_conj(mode_info::in, merge_errors::in,
    io::di, io::uo) is det.

report_mode_error_par_conj(ModeInfo, ErrorList, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("mode error: mutually exclusive bindings"),
        words("in parallel conjunction."),
        words("(The current implementation does not permit"),
        words("parallel conjunctions to fail.)"), nl],
    write_error_pieces_not_first_line(Context, 1, Pieces, !IO),
    io__write_list(ErrorList, "", write_merge_error(ModeInfo), !IO).

:- pred write_merge_error(mode_info::in, merge_error::in, io::di, io::uo)
    is det.

write_merge_error(ModeInfo, Var - Insts, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    Pieces = [words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        fixed("::"),
        words(inst_list_to_string(ModeInfo, Insts)), suffix("."), nl],
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- func merge_context_to_string(merge_context) = string.

merge_context_to_string(disj) = "disjunction".
merge_context_to_string(if_then_else) = "if-then-else".

%-----------------------------------------------------------------------------%

:- pred report_mode_error_bind_var(mode_info::in, var_lock_reason::in,
    prog_var::in, (inst)::in, (inst)::in, io::di, io::uo) is det.

report_mode_error_bind_var(ModeInfo, Reason, Var, VarInst, Inst, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
    (
        Reason = negation,
        ReasonStr = "attempt to bind a variable inside a negation."
    ;
        Reason = if_then_else,
        ReasonStr = "attempt to bind a non-local variable" ++
            " inside the condition of an if-then-else."
    ;
        Reason = lambda(PredOrFunc),
        PredOrFuncS = prog_out__pred_or_func_to_str(PredOrFunc),
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
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
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
        Pieces2 = []
    ),
    write_error_pieces_not_first_line(Context, 0, Pieces1 ++ Pieces2, !IO).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_non_local_lambda_var(mode_info::in, prog_var::in,
    (inst)::in, io::di, io::uo) is det.

report_mode_error_non_local_lambda_var(ModeInfo, Var, VarInst, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("mode error: variable"),
        fixed(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))),
        suffix(","), nl,
        words("expected instantiatedness for non-local variables"),
        words("of lambda goals is `ground'."), nl],
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_in_callee(mode_info::in, list(prog_var)::in,
    list(inst)::in, pred_id::in, proc_id::in, list(mode_error_info)::in,
    io::di, io::uo) is det.

report_mode_error_in_callee(!.ModeInfo, Vars, Insts,
        CalleePredId, CalleeProcId, CalleeModeErrors, !IO) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_varset(!.ModeInfo, VarSet),
    mode_info_write_context(!.ModeInfo, !IO),
    Pieces1 = [words("mode error: arguments"),
        words(add_quotes(mercury_vars_to_string(Vars, VarSet, no))),
        words("have the following insts:"), nl_indent_delta(1)] ++
        inst_list_to_sep_lines(!.ModeInfo, Insts) ++
        [words("which does not match any of the valid modes for")],
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
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
    write_error_pieces_not_first_line(Context, 0, Pieces1 ++ Pieces2, !IO),
    (
        CalleeModeErrors = [First | _],
        First = mode_error_info(_, CalleeModeError,
            CalleeContext, CalleeModeContext),
        mode_info_set_predid(CalleePredId, !ModeInfo),
        mode_info_set_procid(CalleeProcId, !ModeInfo),
        mode_info_set_context(CalleeContext, !ModeInfo),
        mode_info_set_mode_context(CalleeModeContext, !ModeInfo),
        report_mode_error(CalleeModeError, !.ModeInfo, !IO)
    ;
        CalleeModeErrors = [],
        error("report_mode_error_in_callee: no error")
    ).

:- pred report_mode_error_no_matching_mode(mode_info::in, list(prog_var)::in,
    list(inst)::in, io::di, io::uo) is det.

report_mode_error_no_matching_mode(ModeInfo, Vars, Insts, !IO) :-
    mode_info_write_context(ModeInfo, !IO),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    ( ModeContext = call(CallId, _) ->
        CallIdStr = hlds_out__call_id_to_string(CallId)
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
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_mode_error_higher_order_pred_var(mode_info::in, pred_or_func::in,
    prog_var::in, (inst)::in, arity::in, io::di, io::uo) is det.

report_mode_error_higher_order_pred_var(ModeInfo, PredOrFunc, Var, VarInst,
        Arity, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
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
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_mode_error_poly_unify(mode_info::in, prog_var::in, (inst)::in,
    io::di, io::uo) is det.

report_mode_error_poly_unify(ModeInfo, Var, VarInst, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
    Pieces1 = [words("in polymorphically-typed unification:"), nl,
        words("mode error: variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))), suffix(","), nl,
        words("expected instantiatedness was `ground' or `any'."), nl],
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
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
    write_error_pieces_not_first_line(Context, 0, Pieces1 ++ Pieces2, !IO).

:- pred report_mode_error_var_is_live(mode_info::in, prog_var::in,
    io::di, io::uo) is det.

report_mode_error_var_is_live(ModeInfo, Var, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("unique-mode error: the called procedure"),
        words("would clobber its argument, but variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("is still live."), nl],
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_mode_error_var_has_inst(mode_info::in, prog_var::in,
    (inst)::in, (inst)::in, io::di, io::uo) is det.

report_mode_error_var_has_inst(ModeInfo, Var, VarInst, Inst, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("mode error: variable"),
        words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, VarInst))), suffix(","), nl,
        words("expected instantiatedness was"),
        words(add_quotes(inst_to_string(ModeInfo, Inst))), suffix("."), nl],
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_mode_error_implied_mode(mode_info::in, prog_var::in,
    (inst)::in, (inst)::in, io::di, io::uo) is det.

report_mode_error_implied_mode(ModeInfo, Var, VarInst, Inst, !IO) :-
        % This "error" message is really a "sorry, not implemented" message.
        % We only print the message if we will actually generating code.
    globals__io_lookup_bool_option(errorcheck_only, ErrorcheckOnly, !IO),
    (
        ErrorcheckOnly = no,
        mode_info_get_context(ModeInfo, Context),
        mode_info_get_varset(ModeInfo, VarSet),
        mode_info_write_context(ModeInfo, !IO),
        Pieces = [words("sorry, implied modes not implemented."), nl,
            words("Variable"),
            words(add_quotes(mercury_var_to_string(Var, VarSet, no))),
            words("has instantiatedness"),
            words(add_quotes(inst_to_string(ModeInfo, VarInst))),
            suffix(","), nl,
            words("expected instantiatedness was"),
            words(add_quotes(inst_to_string(ModeInfo, Inst))),
            suffix("."), nl],
        write_error_pieces_not_first_line(Context, 0, Pieces, !IO)
    ;
        ErrorcheckOnly = yes
    ).

:- pred report_mode_error_no_mode_decl(mode_info::in, io::di, io::uo) is det.

report_mode_error_no_mode_decl(ModeInfo, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("no mode declaration for called predicate."), nl],
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_mode_error_unify_pred(mode_info::in, prog_var::in,
    mode_error_unify_rhs::in, (type)::in, pred_or_func::in,
    io::di, io::uo) is det.

report_mode_error_unify_pred(ModeInfo, X, RHS, Type, PredOrFunc) -->
    { mode_info_get_context(ModeInfo, Context) },
    { mode_info_get_varset(ModeInfo, VarSet) },
    { mode_info_get_instvarset(ModeInfo, InstVarSet) },
    mode_info_write_context(ModeInfo),
    prog_out__write_context(Context),
    io__write_string("  In unification of `"),
    mercury_output_var(X, VarSet, no),
    io__write_string("' with `"),
    (
        { RHS = error_at_var(Y) },
        mercury_output_var(Y, VarSet, no)
    ;
        { RHS = error_at_functor(ConsId, ArgVars) },
        { mode_info_get_module_info(ModeInfo, ModuleInfo) },
        hlds_out__write_functor_cons_id(ConsId, ArgVars, VarSet,
            ModuleInfo, no)
    ;
        { RHS = error_at_lambda(ArgVars, ArgModes) },
        io__write_string("lambda(["),
        hlds_out__write_var_modes(ArgVars, ArgModes, VarSet,
            InstVarSet, no),
        io__write_string("] ... )")
    ),
    io__write_string("':\n"),
    prog_out__write_context(Context),
    io__write_string("  mode error: attempt at higher-order unification.\n"),
    prog_out__write_context(Context),
    io__write_string("  Cannot unify two terms of type `"),
    { varset__init(TypeVarSet) },
    mercury_output_term(Type, TypeVarSet, no),
    io__write_string("'.\n"),
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
    ( { VerboseErrors = yes } ->
        io__write_string("\tYour code is trying to test whether two "),
        prog_out__write_pred_or_func(PredOrFunc),
        io__write_string("s are equal,\n"),
        io__write_string("\tby unifying them.  In the general " ++
                    "case, testing equivalence\n"),
        io__write_string("\tof "),
        prog_out__write_pred_or_func(PredOrFunc),
        io__write_string("s is an undecidable problem,\n"),
        io__write_string("\tand so this is not allowed by the Mercury mode system.\n"),
        io__write_string("\tIn some cases, you can achieve the same effect by\n"),
        io__write_string("\twriting an explicit universal quantification,\n"),
        io__write_string("\te.g. `all [X] call(P, X) <=> call(Q, X)',"),
        io__write_string(" instead of `P = Q'.\n")
    ;
        []
    ).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_var(mode_info::in, prog_var::in,
    prog_var::in, (inst)::in, (inst)::in, io::di, io::uo) is det.

report_mode_error_unify_var_var(ModeInfo, X, Y, InstX, InstY, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
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
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_lambda(mode_info::in, prog_var::in,
    (inst)::in, (inst)::in, io::di, io::uo) is det.

report_mode_error_unify_var_lambda(ModeInfo, X, InstX, InstY, !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
    Pieces = [words("mode error in unification of"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("and lambda expression."), nl,
        words("Variable"),
        words(add_quotes(mercury_var_to_string(X, VarSet, no))),
        words("has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstX))), suffix(","), nl,
        words("lambda expression has instantiatedness"),
        words(add_quotes(inst_to_string(ModeInfo, InstY))), suffix("."), nl],
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_unify_var_functor(mode_info::in, prog_var::in,
    cons_id::in, list(prog_var)::in, (inst)::in, list(inst)::in,
    io::di, io::uo) is det.

report_mode_error_unify_var_functor(ModeInfo, X, ConsId, Args, InstX, ArgInsts)
        -->
    { mode_info_get_context(ModeInfo, Context) },
    { mode_info_get_varset(ModeInfo, VarSet) },
    { mode_info_get_module_info(ModeInfo, ModuleInfo) },
    mode_info_write_context(ModeInfo),
    prog_out__write_context(Context),
    io__write_string("  mode error in unification of `"),
    mercury_output_var(X, VarSet, no),
    io__write_string("' and `"),
    hlds_out__write_functor_cons_id(ConsId, Args, VarSet, ModuleInfo, no),
    io__write_string("'.\n"),
    prog_out__write_context(Context),
    io__write_string("  Variable `"),
    mercury_output_var(X, VarSet, no),
    io__write_string("' has instantiatedness `"),
    output_inst(InstX, ModeInfo),
    io__write_string("',\n"),
    prog_out__write_context(Context),
    io__write_string("  term `"),
    hlds_out__write_functor_cons_id(ConsId, Args, VarSet, ModuleInfo, no),
    (
        { Args = [_ | _] },
        io__write_string("'\n"),
        prog_out__write_context(Context),
        io__write_string("  has instantiatedness `"),
        mercury_output_cons_id(ConsId, does_not_need_brackets),
        io__write_string("(\n"),
        output_inst_list_sep_lines(Context, ArgInsts, ModeInfo),
        prog_out__write_context(Context),
        io__write_string("  )")
    ;
        { Args = [] },
        io__write_string("' has instantiatedness `"),
        mercury_output_cons_id(ConsId, does_not_need_brackets)
    ),
    io__write_string("'.\n").

%-----------------------------------------------------------------------------%

:- pred mode_info_write_context(mode_info::in, io::di, io::uo) is det.

mode_info_write_context(ModeInfo, !IO) :-
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

    prog_out__write_context(Context, !IO),
    io__write_string("In clause for `", !IO),
    mercury_output_mode_subdecl(PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, Context, !IO),
    io__write_string("':\n", !IO),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    write_mode_context(ModeContext, Context, PredMarkers, !IO).

%-----------------------------------------------------------------------------%

:- pred report_mode_error_final_inst(mode_info::in, int::in, prog_var::in,
    (inst)::in, (inst)::in, final_inst_error::in, io::di, io::uo) is det.

report_mode_error_final_inst(ModeInfo, ArgNum, Var, VarInst, Inst, Reason,
        !IO) :-
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_varset(ModeInfo, VarSet),
    mode_info_write_context(ModeInfo, !IO),
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
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

%-----------------------------------------------------------------------------%

mode_context_init(uninitialized).

%-----------------------------------------------------------------------------%

    % XXX some parts of the mode context never get set up

:- pred write_mode_context(mode_context::in, prog_context::in,
    pred_markers::in, io::di, io::uo) is det.

write_mode_context(uninitialized, _Context, _Markers, !IO).
write_mode_context(call(CallId, ArgNum), Context, Markers, !IO) :-
    prog_out__write_context(Context, !IO),
    io__write_string("  in ", !IO),
    hlds_out__write_call_arg_id(CallId, ArgNum, Markers, !IO),
    io__write_string(":\n", !IO).
write_mode_context(unify(UnifyContext, _Side), Context, _Markers, !IO) :-
    hlds_out__write_unify_context(UnifyContext, Context, !IO).

%-----------------------------------------------------------------------------%

maybe_report_error_no_modes(PredId, PredInfo, ModuleInfo, !IO) :-
    pred_info_import_status(PredInfo, ImportStatus),
    ( ImportStatus = local ->
        globals__io_lookup_bool_option(infer_modes, InferModesOpt, !IO),
        (
            InferModesOpt = yes
        ;
            InferModesOpt = no,
            io__set_exit_status(1, !IO),
            pred_info_context(PredInfo, Context),
            Pieces1 = [words("Error: no mode declaration for")] ++
                describe_one_pred_name(ModuleInfo, should_module_qualify,
                    PredId) ++ [suffix("."), nl],
            globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
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
        io__set_exit_status(1, !IO),
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
        pred_info_procedures(PredInfo, Procs),
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
    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    map__lookup(Procs, ProcId, ProcInfo),
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
        proc_info_argmodes(ProcInfo, !:ArgModes),

        % We need to strip off the extra type_info arguments inserted at the
        % front by polymorphism.m - we only want the last `PredArity' of them.
        %
        list__length(!.ArgModes, NumArgModes),
        NumToDrop = NumArgModes - PredArity,
        ( list__drop(NumToDrop, !ArgModes) ->
            true
        ;
            error("report_pred_proc_id: list__drop failed")
        ),

        varset__init(VarSet),
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        (
            OutputDetism = yes,
            proc_info_inferred_determinism(ProcInfo, Detism),
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
            mode_list_get_initial_insts(!.ArgModes, ModuleInfo, InitialInsts),
            DummyInst = defined_inst(user_inst(unqualified("..."), [])),
            list__duplicate(PredArity, DummyInst, FinalInsts),
            !:ArgModes = list__map(func(I - F) = (I -> F),
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
        % XXX Why do we only report the first?
        FirstError = mode_error_info(_, ModeError, Context, ModeContext),
        mode_info_set_context(Context, !ModeInfo),
        mode_info_set_mode_context(ModeContext, !ModeInfo),
        report_mode_error(ModeError, !.ModeInfo, !IO)
    ;
        Errors = []
    ).

%-----------------------------------------------------------------------------%

report_indistinguishable_modes_error(OldProcId, NewProcId, PredId, PredInfo,
        ModuleInfo, !IO) :-
    io__set_exit_status(1, !IO),

    pred_info_procedures(PredInfo, Procs),
    map__lookup(Procs, OldProcId, OldProcInfo),
    map__lookup(Procs, NewProcId, NewProcInfo),
    proc_info_context(OldProcInfo, OldContext),
    proc_info_context(NewProcInfo, NewContext),

    Pieces1 = [words("In mode declarations for ")] ++
        describe_one_pred_name(ModuleInfo, should_module_qualify, PredId)
        ++ [suffix(":"), nl, words("error: duplicate mode declaration."), nl],
    write_error_pieces(NewContext, 0, Pieces1, !IO),

    globals__io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Pieces2 = [words("Modes"),
            fixed(add_quotes(mode_decl_to_string(OldProcId, PredInfo))),
            words("and"),
            fixed(add_quotes(mode_decl_to_string(NewProcId, PredInfo))),
            words("are indistinguishable.")],
        write_error_pieces_not_first_line(NewContext, 0, Pieces2, !IO)
    ;
        VerboseErrors = no
    ),

    Pieces3 = [words("Here is the conflicting mode declaration.")],
    write_error_pieces_not_first_line(OldContext, 0, Pieces3, !IO).

%-----------------------------------------------------------------------------%

output_mode_decl(ProcId, PredInfo, !IO) :-
    io__write_string(mode_decl_to_string(ProcId, PredInfo), !IO).

mode_decl_to_string(ProcId, PredInfo) = String :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name0 = pred_info_name(PredInfo),
    Name = unqualified(Name0),
    pred_info_procedures(PredInfo, Procs),
    map__lookup(Procs, ProcId, ProcInfo),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    proc_info_declared_determinism(ProcInfo, MaybeDet),
    proc_info_context(ProcInfo, Context),
    varset__init(InstVarSet),
    strip_builtin_qualifiers_from_mode_list(Modes0, Modes),
    String = mercury_mode_subdecl_to_string(PredOrFunc, InstVarSet, Name,
        Modes, MaybeDet, Context).

:- pred output_inst((inst)::in, mode_info::in, io::di, io::uo) is det.

output_inst(Inst0, ModeInfo, !IO) :-
    io__write_string(inst_to_string(ModeInfo, Inst0), !IO).

:- func inst_to_string(mode_info, (inst)) = string.

inst_to_string(ModeInfo, Inst0) = Str :-
    strip_builtin_qualifiers_from_inst(Inst0, Inst),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    Str = mercury_expanded_inst_to_string(Inst, InstVarSet, ModuleInfo).

:- pred output_inst_list(list(inst)::in, mode_info::in, io::di, io::uo) is det.

output_inst_list(Insts, ModeInfo, !IO) :-
    io__write_string(inst_list_to_string(ModeInfo, Insts), !IO).

:- func inst_list_to_string(mode_info, list(inst)) = string.

inst_list_to_string(ModeInfo, Insts) =
    string__join_list(", ", list__map(inst_to_string(ModeInfo), Insts)).

:- pred output_inst_list_sep_lines(prog_context::in, list(inst)::in,
    mode_info::in, io::di, io::uo) is det.

output_inst_list_sep_lines(_Context, [], _, !IO).
output_inst_list_sep_lines(Context, [Inst | Insts], ModeInfo, !IO) :-
    prog_out__write_context(Context, !IO),
    io__write_string("    ", !IO),
    output_inst(Inst, ModeInfo, !IO),
    (
        Insts = []
    ;
        Insts = [_ | _],
        io__write_string(",", !IO)
    ),
    io__nl(!IO),
    output_inst_list_sep_lines(Context, Insts, ModeInfo, !IO).

:- func inst_list_to_sep_lines(mode_info, list(inst)) = list(format_component).

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
