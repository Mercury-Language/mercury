%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2024  The Mercury team.
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
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_name.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module one_or_more.

%---------------------------------------------------------------------------%

:- type mode_error_info
    --->    mode_error_info(
                % The variables which caused the error (we will attempt
                % to reschedule the goal if one of these variables becomes
                % more instantiated).
                set_of_progvar,

                % The nature of the error.
                mode_error,

                % Where the error occurred.
                prog_context,
                mode_context
            ).

%---------------------%

:- type mode_error
    % Mode errors in var/var unifications.

    --->    mode_error_unify_var_var(prog_var, prog_var, mer_inst, mer_inst)
            % Attempt to unify two variables whose initial insts *cannot*
            % be unified.
            % XXX But see also the comment on the code that constructs these
            % kinds of errors.

    ;       mode_error_unify_var_poly(prog_var, mer_inst)
            % A variable in a polymorphic unification with unknown type
            % has inst other than `ground' or `any'.

    % Mode errors in var/functor unifications.

    ;       mode_error_unify_var_functor(prog_var, cons_id, list(prog_var),
                mer_inst, list(mer_inst))
            % Attempt to unify a var with a structured term, where their
            % initial insts *cannot* be unified.
            % XXX But see also the comment in modecheck_unify_functor.

    % Mode errors in var/lambda unifications.

    ;       mode_error_unify_var_lambda(prog_var, mer_inst, mer_inst)
            % Attempt to unify a var with a lambda expression, where their
            % initial insts *cannot* be unified.

    ;       mode_error_unify_var_multimode_pf(prog_var,
                pred_id_var_multimode_error)
            % Attempt to construct a closure using a multi-moded predicate or
            % function.

    ;       mode_error_non_ground_non_local_lambda_var(prog_var, mer_inst)
            % Attempt to pass a live non-ground var as a non-local variable
            % to a lambda goal.

    % Mode errors in any kind of unification.

    ;       mode_error_higher_order_unify(prog_var, mode_error_unify_rhs,
                mer_type, pred_or_func)
            % An attempt to unify two higher-order terms. The first term
            % is a variable, the second term may be anything that can occur
            % on a right hand side of a unification.

    % Mode errors in plain first order calls.

    ;       mode_error_var_is_not_sufficiently_instantiated(prog_var,
                mer_inst, mer_inst, maybe(pred_id_var_multimode_error))
            % An argument in a call to a predicate or function with one mode
            % is not sufficiently instantiated.
            % The first inst is the var's actual inst, the second is its
            % expected inst.
            % XXX The "with one mode" part may be a lie.
            % XXX Document the last arg.

    ;       mode_error_clobbered_var_is_live(prog_var)
            % A call will clobber this variable, but it is still live.

    ;       mode_error_callee_pred_has_no_mode_decl(pred_id)
            % A call to a predicate for which there are no mode declarations
            % (and mode inference is not enabled). Note that there are no
            % *functions* without mode declarations: if the user does not
            % provide one, the compiler will.

    ;       mode_error_no_matching_mode(match_what, instmap, list(prog_var),
                list(list(mer_inst)))
            % Call to a predicate with an insufficiently instantiated variable
            % (for preds with >1 mode).
            %
            % The secord argument gives the argument vars of the call,
            % and lookup them in the instmap gives their insts at the time
            % of the call.
            %
            % If the third argument is a nonempty list, then every member
            % of that list gives the list of the required initial insts
            % of the argument vars.
            % XXX And if it is empty?
            %
            % All the lists of insts must have exactly one inst for
            % each var in the list of vars.

    % Mode errors in higher order calls.

    ;       mode_error_bad_higher_order_inst(prog_var, mer_inst,
                pred_or_func, user_arity, higher_order_mismatch_info)
            % The variable has the given inst, which does not match
            % the expected higher order inst with the given pred_or_func
            % and the given arity. The last argument specifies the
            % nature of the mismatch.

    % Mode errors in conjunctions.

    ;       mode_error_unschedulable_conjuncts(one_or_more(delayed_goal),
                schedule_culprit)
            % A conjunction contains one or more unscheduleable goals;
            % schedule_culprit gives the reason why they couldn't be scheduled.

    ;       mode_error_merge_par_conj(one_or_more(merge_error))
            % Different arms of a parallel conj result in mutually exclusive
            % bindings, which means that the process of unifying the instmaps
            % from the end of each conjunct has failed.

    % Mode errors in disjunctions (of various kinds).

    ;       mode_error_merge_disj(merge_context, one_or_more(merge_error))
            % Different arms of a disjunction result in different insts
            % for some non-local variables.

    % Mode errors in coerce expressions.

    ;       mode_error_coerce_error(list(coerce_error))
            % Mode error in coerce expression.

    % Mode errors that can happen in more than one kind of goal.

    ;       mode_error_bind_locked_var(var_lock_reason, prog_var,
                mer_inst, mer_inst)
            % Attempt to bind a non-local variable inside a negated context,
            % or attempt to re-bind a variable in a parallel conjunct.
            % The two insts are the insts of the variable before and after
            % *something*, but the code that creates this error is not clear
            % on what that "something" is :-( Presumably, the identity of that
            % "something" may depend on the reason for the lock.

    % Mode errors in procedure as a whole.

    ;       mode_error_unexpected_final_inst(int, prog_var, mer_inst, mer_inst,
                final_inst_error)
            % The head variable in the given argument position did not have
            % the expected final inst on exit from the proc.
            % The first inst gives its actual final inst,
            % the second inst gives its expected final inst.
            % The last argument specifies the nature of the discrepancy
            % between these two insts.

    % Mode errors caused by limitations of the current mode analyzer.

    ;       mode_error_in_callee(list(prog_var), list(mer_inst),
                pred_id, proc_id, list(mode_error_info))
            % Call to a predicate with initial argument insts for which mode
            % inference gave a mode error in the callee.
            % XXX I (zs) see nothing in either the code that generates
            % this kind of error, or in the message we generate for it,
            % that restricts it to occur only in the presence of mode
            % inference.

    ;       mode_error_cannot_create_implied_mode(cannot_create_reason,
                prog_var, mer_inst, mer_inst)
            % A call to a predicate with an overly instantiated variable
            % would use an implied mode of the predicate, but we can't
            % introduce a simple unification after calling the predicate
            % for the reason given by the first argument.
            % The first inst is the current inst of the variable, while
            % the second is the expected initial inst in the original,
            % non-implied mode.

    % Purity errors.

    ;       purity_error_should_be_in_promise_purity_scope(
                negated_context_desc, prog_var)
            % The condition of an if-then-else or the body of a negation
            % contains a nonlocal variable with inst `any', but is not
            % inside a promise_purity scope.

    ;       purity_error_lambda_should_be_any(one_or_more(prog_var)).
            % A ground lambda term contains the given nonlocal variables
            % that have inst `any', but is not marked impure.

:- type higher_order_mismatch_info
    --->    mismatch_not_higher_order_type
    ;       mismatch_no_higher_order_inst_info
    ;       mismatch_pred_vs_func(pred_or_func)
            % actual PorF (expected is in enclosing term)
    ;       mismatch_on_arity(user_arity).
            % actual arity (expected is in enclosing term)

:- type match_what
    --->    match_plain_call(pred_id)
    ;       match_higher_order_call(generic_call_id)
    ;       match_unify
    ;       match_cast
    ;       match_event.

%---------------------%

:- type pred_id_var_multimode_error
    --->    pred_id_var_multimode_error(pred_id, var_multimode_error).

:- type var_multimode_error
    --->    no_matching_mode(
                % The modes of these arguments match no mode of the callee.
                list(prog_var)
            )
    ;       more_than_one_matching_mode(
                % The modes of these arguments match more than one mode
                % of the callee ...
                list(prog_var),
                % ... specifically, these modes.
                proc_id, proc_id, list(proc_id)
            )
    ;       some_ho_args_non_ground(
                % These higher order arguments of the call are not ground.
                list(prog_var)
            ).

%---------------------%

:- type mode_error_unify_rhs
    --->    error_at_var(prog_var)
    ;       error_at_functor(cons_id, list(prog_var))
    ;       error_at_lambda(list(prog_var), list(from_to_insts)).

%---------------------%

:- type delayed_goal
    --->    delayed_goal(
                set_of_progvar,     % The vars the goal is waiting on.
                mode_error_info,    % The reason the goal can't be scheduled.
                hlds_goal           % The goal itself.
            ).

:- type schedule_culprit
    --->    goal_itself_was_impure
    ;       goals_followed_by_impure_goal(hlds_goal)
    ;       conj_floundered.        % We have reached the end of a conjunction
                                    % and there were still delayed goals.

%---------------------%

:- type merge_error
    --->    merge_error(prog_var, assoc_list(prog_context, mer_inst)).
            % The given variable had incompatible insts in different branches.
            % The second arg effectively maps the context of each branch
            % with inst of the variable in that branch.

:- type merge_context
    --->    merge_disj
    ;       merge_if_then_else
    ;       merge_stm_atomic.

%---------------------%

:- type coerce_error
    --->    coerce_error(
                % Path to subterm where the mode error was detected.
                list(coerce_error_term_path_step),
                % Type of the subterm.
                mer_type,
                % Target type of the conversion.
                mer_type,
                coerce_error_reason
            ).

:- type coerce_error_term_path_step
    --->    coerce_error_term_path_step(cons_id, int).

:- type coerce_error_reason
    --->    input_inst_not_ground(mer_inst)
    ;       cons_id_errors(mer_inst,
                bound_inst_cons_id_error, list(bound_inst_cons_id_error))
    ;       has_inst_expect_upcast(mer_inst).

:- type bound_inst_cons_id_error
    --->    bad_cons_id_input(cons_id)
            % The cons_id does not exist in the input type.
    ;       bad_cons_id_input_inst_arity(cons_id, arity, arity)
            % The cons_id exists in the input type, but the inst specifies
            % the wrong arity for it.
            % XXX This should not happen, since such errors *should* be
            % detected when pushing types into insts, but we prepare for it
            % happening anyway, just in case that process lets through
            % something it shouldn't have let through.
            % The cons_id, its arity in the inst of X, and the expected arity.
    ;       bad_cons_id_result(cons_id).
            % The cons_id does not exist in the result type.

%---------------------%

:- type final_inst_error
    --->    too_instantiated
    ;       not_instantiated_enough
    ;       wrongly_instantiated.
            % A catchall for anything that does not fit into
            % the above two categories.

%---------------------%

:- type cannot_create_reason
    --->    cannot_init_any
    ;       cannot_deep_copy_partial_term.

%---------------------%

:- type negated_context_desc
    --->    if_then_else
    ;       negation.

%---------------------------------------------------------------------------%

:- type mode_warning_info
    --->    mode_warning_info(
                mode_warning,       % The nature of the error.
                prog_context,       % Where the error occurred.
                mode_context        % Where the error occurred.
            ).

:- type mode_warning
    --->    cannot_succeed_var_var(prog_var, prog_var, mer_inst, mer_inst)
    ;       cannot_succeed_var_functor(prog_var, mer_inst, cons_id)
    ;       cannot_succeed_ground_occur_check(prog_var, cons_id).

%---------------------------------------------------------------------------%

    % Generate a message for the given mode error in the context
    % described by the mode_info.
    %
:- func mode_error_info_to_spec(mode_info, mode_error_info) = error_spec.

    % Generate a message for the given mode warning in the context
    % described by the mode_info.
    %
:- func mode_warning_info_to_spec(mode_info, mode_warning_info) = error_spec.

    % What it says on the tin.
    %
:- func should_report_mode_warning_for_pred_origin(pred_origin) = bool.

%---------------------------------------------------------------------------%

    % XXX This utility predicate does not really fit in this module,
    % being completely independent of the rest of the module,
    % but it doesn't fit any better in any other modules either :-(,
    % and it cannot be moved to its caller's module, because it is
    % called from more than one other module.
    %
:- func mode_decl_to_string(output_lang, proc_id, pred_info) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.error_msg_inst.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_proc_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module int.
:- import_module map.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% The structure of the implementation section is as follows.
%
% - The code that switches on the kinds of mode errors, and invokes
%   each one's handler. The switch arms are in the same order as the
%   errors' function symbols in the mode_error type above.
%
% - The code specific to each kind of mode error handler. These follow
%   the same order.
%
% - The utility predicates needed by two or more of these error handlers.
%
% - The code that switches on the kinds of mode warnings, and invokes
%   each one's handler. The switch arms are in the same order as the
%   warnings' function symbols in the mode_warning type above.
%
% - The code specific to each kind of mode error handler. These follow
%   the same order.
%
% - The utility predicates needed by some warning handlers, and maybe
%   by some error handlers.
%
% - The code of should_report_mode_warning_for_pred_origin.
%
% - The code of mode_decl_to_string.
%
%---------------------------------------------------------------------------%
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
        ModeError = mode_error_unify_var_var(VarA, VarB, InstA, InstB),
        Spec = mode_error_unify_var_var_to_spec(ModeInfo, VarA, VarB,
            InstA, InstB)
    ;
        ModeError = mode_error_unify_var_poly(Var, Inst),
        Spec = mode_error_unify_var_poly_to_spec(ModeInfo, Var, Inst)
    ;
        ModeError = mode_error_unify_var_functor(Var, Name, Args, Inst,
            ArgInsts),
        Spec = mode_error_unify_var_functor_to_spec(ModeInfo, Var, Name,
            Args, Inst, ArgInsts)
    ;
        ModeError = mode_error_unify_var_lambda(VarA, InstA, InstB),
        Spec = mode_error_unify_var_lambda_to_spec(ModeInfo, VarA,
            InstA, InstB)
    ;
        ModeError = mode_error_unify_var_multimode_pf(Var,
            PredMultiModeError),
        Spec = mode_error_unify_var_multimode_pf_to_spec(ModeInfo, Var,
            PredMultiModeError)
    ;
        ModeError = mode_error_non_ground_non_local_lambda_var(Var, Inst),
        Spec = mode_error_non_ground_non_local_lambda_var_to_spec(ModeInfo,
            Var, Inst)
    ;
        ModeError = mode_error_higher_order_unify(Var, RHS, Type, PredOrFunc),
        Spec = mode_error_higher_order_unify_to_spec(ModeInfo, Var, RHS, Type,
            PredOrFunc)
    ;
        ModeError = mode_error_var_is_not_sufficiently_instantiated(Var,
            ActualInst, ExpectedInst, MaybeMultiMode),
        Spec = mode_error_var_is_not_sufficiently_instantiated_to_spec(
            ModeInfo, Var, ActualInst, ExpectedInst, MaybeMultiMode)
    ;
        ModeError = mode_error_clobbered_var_is_live(Var),
        Spec = mode_error_clobbered_var_is_live_to_spec(ModeInfo, Var)
    ;
        ModeError = mode_error_callee_pred_has_no_mode_decl(PredId),
        Spec = mode_error_callee_pred_has_no_mode_decl_to_spec(ModeInfo,
            PredId)
    ;
        ModeError = mode_error_no_matching_mode(MatchWhat, InstMap, ArgVars,
            ProcInitialInsts),
        Spec = mode_error_no_matching_mode_to_spec(ModeInfo, MatchWhat,
            InstMap, ArgVars, ProcInitialInsts)
    ;
        ModeError = mode_error_bad_higher_order_inst(Var, Inst,
            ExpectedPredOrFunc, ExpectedUserArity, Mismatch),
        Spec = mode_error_bad_higher_order_inst_to_spec(ModeInfo, Var, Inst,
            ExpectedPredOrFunc, ExpectedUserArity, Mismatch)
    ;
        ModeError = mode_error_unschedulable_conjuncts(OoMErrors, Culprit),
        Spec = mode_error_unschedulable_conjuncts_to_spec(ModeInfo, OoMErrors,
            Culprit)
    ;
        ModeError = mode_error_merge_par_conj(MergeErrors),
        Spec = mode_error_merge_par_conj_to_spec(ModeInfo, MergeErrors)
    ;
        ModeError = mode_error_merge_disj(MergeContext, MergeErrors),
        Spec = mode_error_merge_disj_to_spec(ModeInfo, MergeContext,
            MergeErrors)
    ;
        ModeError = mode_error_coerce_error(CoerceErrors),
        Spec = mode_error_coerce_error_to_spec(ModeInfo, CoerceErrors)
    ;
        ModeError = mode_error_bind_locked_var(Reason, Var, InstA, InstB),
        Spec = mode_error_bind_locked_var_to_spec(ModeInfo, Reason, Var,
            InstA, InstB)
    ;
        ModeError = mode_error_unexpected_final_inst(ArgNum, Var,
            ActualInst, ExpectedInst, Reason),
        Spec = mode_error_unexpected_final_inst_to_spec(ModeInfo, ArgNum, Var,
            ActualInst, ExpectedInst, Reason)
    ;
        ModeError = mode_error_in_callee(Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors),
        Spec = mode_error_in_callee_to_spec(ModeInfo, Vars, Insts,
            CalleePredId, CalleeProcId, CalleeErrors)
    ;
        ModeError = mode_error_cannot_create_implied_mode(Reason,
            Var, ActualInst, NonImpliedInitialInst),
        Spec = mode_error_cannot_create_implied_mode_to_spec(ModeInfo, Reason,
            Var, ActualInst, NonImpliedInitialInst)
    ;
        ModeError = purity_error_should_be_in_promise_purity_scope(NegCtxt,
            Var),
        Spec = purity_error_should_be_in_promise_purity_scope_to_spec(NegCtxt,
            ModeInfo, Var)
    ;
        ModeError = purity_error_lambda_should_be_any(Vars),
        Spec = purity_error_lambda_should_be_any_to_spec(ModeInfo, Vars)
    ).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_var_to_spec(mode_info, prog_var,
    prog_var, mer_inst, mer_inst) = error_spec.

mode_error_unify_var_var_to_spec(ModeInfo, X, Y, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarTable, X)), words("and"),
        quote(mercury_var_to_name_only(VarTable, Y)), suffix("."), nl,
        words("Variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, X))]) ++
        has_instantiatedness(ModeInfo, yes(color_inconsistent), InstX, ",") ++
        [words("variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, Y))]) ++
        has_instantiatedness(ModeInfo, yes(color_inconsistent), InstY, "."),
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_poly_to_spec(mode_info, prog_var, mer_inst)
    = error_spec.

mode_error_unify_var_poly_to_spec(ModeInfo, Var, VarInst) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    ExpectedInstPieces =
        color_as_correct([quote("ground")]) ++ [words("or")] ++
        color_as_correct([quote("any"), suffix(".")]),
    MainPieces = [words("in polymorphically-typed unification:"), nl,
        words("mode error: variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, Var))]) ++
        has_instantiatedness(ModeInfo, yes(color_incorrect), VarInst, ",") ++
        [words("expected instantiatedness was")] ++
        ExpectedInstPieces ++ [nl],
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

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_functor_to_spec(mode_info, prog_var,
    cons_id, list(prog_var), mer_inst, list(mer_inst)) = error_spec.

mode_error_unify_var_functor_to_spec(ModeInfo, X, ConsId, ArgVars,
        InstX, ArgInsts) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    FunctorConsIdStr = functor_cons_id_to_string(ModuleInfo,
        vns_var_table(VarTable), print_name_only, ConsId, ArgVars),
    ConsIdStr = mercury_cons_id_to_string(output_mercury,
        does_not_need_brackets, ConsId),
    % inst_name_to_pieces and inst_name_to_inline_pieces look for this
    % specific fake module qualifier, and allow the lookup of a user_inst
    % with this qualifier to fail. They will then treat ConsIdStr as we
    % want it to be treated: as a wrapper around ArgInsts.
    FakeTermInstModuleName = unqualified("FAKE_CONS_ID"),
    FakeTermInstSymName = qualified(FakeTermInstModuleName, ConsIdStr),
    FakeTermInst = defined_inst(user_inst(FakeTermInstSymName, ArgInsts)),
    InstColor = yes(color_inconsistent),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarTable, X)),
        words("and"), words_quote(FunctorConsIdStr), suffix("."), nl,
        words("Variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, X))]) ++
        has_instantiatedness(ModeInfo, InstColor, InstX, ",") ++
        [words("term")] ++
        color_as_subject([words_quote(FunctorConsIdStr)]) ++
        has_instantiatedness(ModeInfo, InstColor, FakeTermInst, "."),
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_lambda_to_spec(mode_info, prog_var,
    mer_inst, mer_inst) = error_spec.

mode_error_unify_var_lambda_to_spec(ModeInfo, X, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    InstColor = yes(color_inconsistent),
    Pieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarTable, X)),
        words("and lambda expression."), nl,
        words("Variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, X))]) ++
        has_instantiatedness(ModeInfo, InstColor, InstX, ",") ++
        color_as_subject([words("lambda expression")]) ++
        has_instantiatedness(ModeInfo, InstColor, InstY, "."),
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_unify_var_multimode_pf_to_spec(mode_info, prog_var,
    pred_id_var_multimode_error) = error_spec.

mode_error_unify_var_multimode_pf_to_spec(ModeInfo, X, PredMultiModeError)
        = Spec :-
    PredMultiModeError = pred_id_var_multimode_error(PredId, MultiModeError),
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    SymName = qualified(PredModule, PredName),
    PFSNA = pf_sym_name_arity(PredOrFunc, SymName, PredFormArity),
    % XXX What should we color as incorrect or as possible cause here?
    % The variable X? PFSNA, the name of the predicate or function?
    % Both? Neither? Something else?
    StartPieces = [words("mode error in unification of"),
        quote(mercury_var_to_name_only(VarTable, X)),
        words("and higher-order term based on multi-moded"),
        qual_pf_sym_name_pred_form_arity(PFSNA), suffix("."), nl],
    (
        MultiModeError = some_ho_args_non_ground(NonGroundArgVars),
        VarOrVars = choose_number(NonGroundArgVars, "variable", "variables"),
        NonGroundArgVarPieces = named_and_unnamed_vars_to_pieces(VarTable,
            color_subject, NonGroundArgVars),
        DetailPieces = [words("The higher order argument"),
            words(VarOrVars)] ++ NonGroundArgVarPieces ++
            color_as_correct([words("should be ground,")]) ++
            [words("but")] ++
            color_as_incorrect([words("are not.")]) ++
            [nl]
    ;
        (
            MultiModeError = no_matching_mode(ArgVars),
            MatchPieces = [words(choose_number(ArgVars, "does", "do")),
                words("not match any")],
            EndPieces = [suffix(".")]
        ;
            MultiModeError = more_than_one_matching_mode(ArgVars,
                ProcA, ProcB, ProcCs),
            MatchPieces = [words(choose_number(ArgVars, "matches", "match")),
                words("more than one")],
            ProcIdToPiece =
                ( func(ProcId) = nth_fixed(ModeNum) :-
                    % ProcIds start at zero, mode numbers start at 1.
                    ModeNum = proc_id_to_int(ProcId) + 1
                ),
            ModeNumberPieces =
                list.map(ProcIdToPiece, [ProcA, ProcB | ProcCs]),
            EndPieces = [suffix(","), words("specifically the")] ++
                piece_list_to_pieces("and", ModeNumberPieces) ++ [suffix(".")]
        ),
        ModeOrModes = choose_number(ArgVars, "mode", "modes"),
        VarOrVars = choose_number(ArgVars, "variable", "variables"),
        ArgVarPieces = named_and_unnamed_vars_to_pieces(VarTable,
            color_subject, ArgVars),
        DetailPieces = [words("The"), words(ModeOrModes),
            words("of the argument"), words(VarOrVars)] ++ ArgVarPieces ++
            color_as_incorrect(MatchPieces) ++ [words("of the called"),
            p_or_f(PredOrFunc), suffix("'s"), words("modes")] ++
            EndPieces ++ [nl]
    ),
    Spec = spec($pred, severity_error, phase_mode_check(report_in_any_mode),
        Context, Preamble ++ StartPieces ++ DetailPieces).

:- func named_and_unnamed_vars_to_pieces(var_table, color_name,
    list(prog_var)) = list(format_piece).

named_and_unnamed_vars_to_pieces(VarTable, Color, Vars) = Pieces :-
    list.filter_map(
        ( pred(V::in, N::out) is semidet :-
            lookup_var_entry(VarTable, V, E),
            E = vte(N, _, _),
            N \= ""
        ), Vars, NamedVarNames, UnnamedVars),
    (
        NamedVarNames = [],
        Pieces = []
    ;
        NamedVarNames = [_ | _],
        NamedVarNamePieces = list.map((func(N) = quote(N)), NamedVarNames),
        NamedVarPieces = piece_list_to_color_pieces(Color, "and", [],
            NamedVarNamePieces),
        (
            UnnamedVars = [],
            Pieces = NamedVarPieces
        ;
            UnnamedVars = [_ | _],
            Pieces = [words("including") | NamedVarPieces]
        )
    ).

%---------------------------------------------------------------------------%

:- func mode_error_non_ground_non_local_lambda_var_to_spec(mode_info, prog_var,
    mer_inst) = error_spec.

mode_error_non_ground_non_local_lambda_var_to_spec(ModeInfo, Var, VarInst)
        = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    Pieces = [words("mode error: variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, Var))]) ++
        has_instantiatedness(ModeInfo, yes(color_incorrect), VarInst, ",") ++
        [words("expected instantiatedness for non-local variables"),
        words("of lambda goals is")] ++
        color_as_correct([quote("ground"), suffix(".")]) ++
        [nl],
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_higher_order_unify_to_spec(mode_info, prog_var,
    mode_error_unify_rhs, mer_type, pred_or_func) = error_spec.

mode_error_higher_order_unify_to_spec(ModeInfo, LHSVar, RHS, Type, PredOrFunc)
        = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    (
        RHS = error_at_var(Y),
        RHSStr = mercury_var_to_name_only(VarTable, Y)
    ;
        RHS = error_at_functor(ConsId, ArgVars),
        RHSStr = functor_cons_id_to_string(ModuleInfo, vns_var_table(VarTable),
            print_name_only, ConsId, ArgVars)
    ;
        RHS = error_at_lambda(ArgVars, ArgFromToInsts),
        ArgModes = list.map(from_to_insts_to_mode, ArgFromToInsts),
        assoc_list.from_corresponding_lists(ArgVars, ArgModes, ArgVarsModes),
        RHSStr = "lambda(["
            ++ var_modes_to_string(output_debug, vns_var_table(VarTable),
                InstVarSet, print_name_only, ArgVarsModes)
            ++ "] ... )"
    ),
    varset.init(TypeVarSet),
    LHSVarName = mercury_var_to_name_only(VarTable, LHSVar),
    MainPieces = [words("In unification of")] ++
        color_as_subject([quote(LHSVarName)]) ++ [words("with")] ++
        color_as_subject([quote(RHSStr), suffix(":")]) ++ [nl,
        words("mode error: you cannot unify two higher-order values."), nl,
        words("These two terms have type"),
        quote(mercury_type_to_string(TypeVarSet, print_name_only, Type)),
        suffix("."), nl],
    VerbosePieces = [words("Your code is trying to test whether two"),
        words(pred_or_func_to_full_str(PredOrFunc) ++ "s"),
        words("are equal, by unifying them."),
        words("In the general case, testing equivalence of"),
        words(pred_or_func_to_full_str(PredOrFunc) ++ "s"),
        words("is an undecidable problem,"),
        words("and so this is not allowed by the Mercury mode system."),
        words("In some cases, you can achieve the same effect by"),
        words("writing an explicit universal quantification, e.g."),
        quote("all [X] call(PredA, X) <=> call(PredB, X)"), suffix(","),
        words("instead of"), quote("PredA = PredB"), suffix("."), nl],
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces),
            verbose_only(verbose_once, VerbosePieces)])]).

%---------------------------------------------------------------------------%

:- func mode_error_var_is_not_sufficiently_instantiated_to_spec(mode_info,
    prog_var, mer_inst, mer_inst, maybe(pred_id_var_multimode_error))
    = error_spec.

mode_error_var_is_not_sufficiently_instantiated_to_spec(ModeInfo, Var,
        VarInst, ExpectedInst, MaybeMultiModeError) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    MainPieces = [words("mode error: variable")] ++
        color_as_subject([quote(mercury_var_to_name_only(VarTable, Var))]) ++
        has_inst_expected_inst_was(ModeInfo, VarInst, ExpectedInst),
    MainMsgs = [msg(Context, Preamble ++ MainPieces)],
    Phase = phase_mode_check(report_in_any_mode),
    ( if
        inst_has_uniqueness(VarInst, mostly_unique),
        inst_has_uniqueness(ExpectedInst, unique)
    then
        UniqPieces = [words("This kind of uniqueness mismatch"),
            words("is usually caused by doing")] ++
            color_as_hint([words("input/output")]) ++
            [words("or")] ++
            color_as_hint([words("some other kind of"),
                words("destructive update")]) ++
            color_as_incorrect([words("in a context where"),
                words("it can be backtracked over,")]) ++
            [words("such as the condition of an if-then-else."), nl],
        UniqMsgs = [msg(Context, UniqPieces)]
    else
        UniqMsgs = []
    ),
    (
        MaybeMultiModeError = no,
        MultiModeMsgs = []
    ;
        MaybeMultiModeError = yes(PredMultiModeError),
        ConnectPieces = [words("This may have been caused by"),
            words("the following error."), nl],
        ConnectMsgs = [msg(Context, ConnectPieces)],
        SubSpec0 = mode_error_unify_var_multimode_pf_to_spec(ModeInfo, Var,
            PredMultiModeError),
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        extract_spec_msgs(Globals, SubSpec0, SubMsgs),
        MultiModeMsgs = ConnectMsgs ++ SubMsgs
    ),
    AllMsgs = MainMsgs ++ UniqMsgs ++ MultiModeMsgs,
    Spec = error_spec($pred, severity_error, Phase, AllMsgs).

:- pred inst_has_uniqueness(mer_inst::in, uniqueness::in) is semidet.

inst_has_uniqueness(Inst, SearchUniq) :-
    require_complete_switch [Inst]
    (
        ( Inst = free
        ; Inst = any(_, _)
        ; Inst = not_reached
        ; Inst = inst_var(_)
        ),
        fail
    ;
        Inst = defined_inst(_),
        % We could expand the defined inst, but the novice Mercury programmers
        % that the error message addendum this code helps to construct
        % will virtually never encounter defined insts that expand out
        % to unique or mostly_unique.
        fail
    ;
        ( Inst = ground(Uniq, _)
        ; Inst = bound(Uniq, _, _)
        ),
        Uniq = SearchUniq
    ;
        Inst = constrained_inst_vars(_, SubInst),
        inst_has_uniqueness(SubInst, SearchUniq)
    ).

%---------------------------------------------------------------------------%

:- func mode_error_clobbered_var_is_live_to_spec(mode_info, prog_var)
    = error_spec.

mode_error_clobbered_var_is_live_to_spec(ModeInfo, Var) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    VarName = mercury_var_to_name_only(VarTable, Var),
    Pieces = [words("unique-mode error: the called procedure"),
        words("would clobber its argument, but variable")] ++
        color_as_subject([quote(VarName)]) ++
        [words("is")] ++
        color_as_incorrect([words("still live.")]) ++
        [nl],
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_callee_pred_has_no_mode_decl_to_spec(mode_info, pred_id)
    = error_spec.

mode_error_callee_pred_has_no_mode_decl_to_spec(ModeInfo, PredId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    PredDotPieces = describe_one_pred_name(ModuleInfo, yes(color_subject),
        should_module_qualify, [suffix(".")], PredId),
    mode_info_get_context(ModeInfo, Context),
    % Functions have a default mode, so they do not need a mode declaration,
    Pieces = [words("error: there is")] ++
        color_as_incorrect([words("no mode declaration")]) ++
        [words("for")] ++ PredDotPieces ++ [nl],
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_no_matching_mode_to_spec(mode_info, match_what, instmap,
    list(prog_var), list(list(mer_inst))) = error_spec.

mode_error_no_matching_mode_to_spec(ModeInfo, MatchWhat, InstMap, Vars,
        ProcInitialInsts) = Spec :-
    PrefixPieces = mode_info_context_preamble(ModeInfo) ++
        [words("mode error:")],
    mode_info_get_mode_context(ModeInfo, ModeContext),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    (
        ModeContext = mode_context_call(ModeCallId, _),
        (
            ModeCallId = mode_call_generic(GenericCallId0),
            (
                GenericCallId0 = gcid_higher_order(_, PredOrFunc, _)
            ;
                GenericCallId0 = gcid_class_method(_, PFSymNameArity),
                PFSymNameArity = pf_sym_name_arity(PredOrFunc, _, _)
            ;
                ( GenericCallId0 = gcid_event_call(_)
                ; GenericCallId0 = gcid_cast(_)
                ),
                PredOrFunc = pf_predicate
            ),
            % XXX Setting NumExtra to 0 means that we do not separate out
            % any arguments added by polymorphism.m. Since most higher order
            % values are monomorphic, this should not be too much of a loss.
            % If and when it becomes one, this should be fixed.
            NumExtra = 0
        ;
            ModeCallId = mode_call_plain(PredId0),
            some [PredInfo, PFSymNameArity] (
                module_info_pred_info(ModuleInfo, PredId0, PredInfo),
                pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
                PredOrFunc = pred_info_is_pred_or_func(PredInfo),
                pred_info_get_orig_arity(PredInfo, pred_form_arity(OrigArity))
            ),
            list.length(Vars, NumVars),
            NumExtra = NumVars - OrigArity
        )
    ;
        ( ModeContext = mode_context_unify(_, _)
        ; ModeContext = mode_context_not_call_or_unify
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
        UserVarInstPieces = arg_inst_mismatch_pieces(ModeInfo, [],
            PredOrFunc, InstMap, UserVars),
        ( if
            var_insts_are_all_ground(ModuleInfo, VarTable, InstMap, ExtraVars)
        then
            VarListInstPieces = UserVarInstPieces
        else
            ExtraArgPieces = [words("the compiler-generated")],
            ExtraVarInstPieces = arg_inst_mismatch_pieces(ModeInfo,
                ExtraArgPieces, pf_predicate, InstMap, ExtraVars),
            VarListInstPieces =  ExtraVarInstPieces ++ UserVarInstPieces
        )
    else
        VarListInstPieces = arg_inst_mismatch_pieces(ModeInfo, [],
            PredOrFunc, InstMap, Vars)
    ),
    % Note that MatchWhat *almost* duplicates the information contained in
    % ModeContext, which we process above. However, despite the call to
    % unexpected above for mode_context_unify, this predicate *can* get called
    % for errors discovered during unifications (whose processing presumably
    % did not set the mode context), and we want to generate an error message
    % that does not generate confusing wording in this eventuality.
    NoMatchPieces = [words("which")] ++
        color_as_incorrect([words("does not match")]),
    (
        MatchWhat = match_plain_call(PredId),
        some [PredInfo, PFSymNameArity] (
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
            CallIdStr = pf_sym_name_pred_form_arity_to_string(PFSymNameArity),
            pred_info_get_proc_table(PredInfo, ProcTable)
        ),
        ( if map.count(ProcTable) > 1 then
            MatchWhatPieces =
                [words("any of the modes for"),
                words(CallIdStr), suffix("."), nl]
        else
            MatchWhatPieces =
                [words("the only mode of"),
                words(CallIdStr), suffix("."), nl]
        )
    ;
        MatchWhat = match_higher_order_call(GenericCallId),
        WhatStr = generic_call_id_to_string(GenericCallId),
        MatchWhatPieces =
            [words("the mode of this"),
            words(WhatStr), suffix("."), nl]
    ;
        MatchWhat = match_event,
        MatchWhatPieces =
            [words("the input mode required for event arguments."), nl]
    ;
        MatchWhat = match_cast,
        MatchWhatPieces =
            [words("the input mode required for event values being cast."), nl]
    ;
        MatchWhat = match_unify,
        MatchWhatPieces =
            [words("the input mode required for unifications."), nl]
    ),
    mode_info_get_var_table(ModeInfo, VarTable),
    construct_argnum_var_type_inst_tuples(VarTable, InstMap, 1,
        Vars, ArgTuples),
    find_satisfied_initial_insts_in_procs(ModuleInfo, ArgTuples,
        ProcInitialInsts, 0, map.init, ArgNumMatchedProcs),
    % XXX If ArgTuples has length 1, which happens reasonably often,
    % the information reported by BadArgPieces will exactly duplicate
    % the information reported by NoMatchPieces, just with different wording.
    % The same is true if BadArgPieces has a component for every var in Vars.
    % In such cases, we should include just one of the two in Pieces.
    % But which one? One consideration: BadArgPieces actually names
    % the arguments.
    report_any_never_matching_args(ModeInfo, ArgNumMatchedProcs, NumExtra,
        ArgTuples, BadArgPieces),
    Pieces = PrefixPieces ++ VarListInstPieces ++
        NoMatchPieces ++ MatchWhatPieces ++ BadArgPieces,
    Phase = phase_mode_check(report_in_any_mode),
    mode_info_get_context(ModeInfo, Context),
    Spec = spec($pred, severity_error, Phase, Context, Pieces).

:- pred var_insts_are_all_ground(module_info::in, var_table::in, instmap::in,
    list(prog_var)::in) is semidet.

var_insts_are_all_ground(_, _, _, []).
var_insts_are_all_ground(ModuleInfo, VarTable, InstMap, [Var | Vars]) :-
    lookup_var_type(VarTable, Var, VarType),
    instmap_lookup_var(InstMap, Var, VarInst),
    inst_is_ground(ModuleInfo, VarType, VarInst),
    var_insts_are_all_ground(ModuleInfo, VarTable, InstMap, Vars).

:- func arg_inst_mismatch_pieces(mode_info, list(format_piece),
    pred_or_func, instmap, list(prog_var)) = list(format_piece).

arg_inst_mismatch_pieces(ModeInfo, CompGenPieces, PredOrFunc, InstMap, Vars)
        = Pieces :-
    (
        Vars = [],
        Pieces = []
    ;
        Vars = [HeadVar | TailVars],
        mode_info_get_var_table(ModeInfo, VarTable),
        instmap_lookup_vars(InstMap, Vars, Insts),
        (
            PredOrFunc = pf_predicate,
            HeadVarPiece = var_in_table_to_quote_piece(VarTable, HeadVar),
            TailVarPieces = list.map(var_in_table_to_quote_piece(VarTable),
                TailVars),
            (
                TailVars = [],
                Pieces = CompGenPieces ++ [words("argument")] ++
                    color_as_subject([HeadVarPiece]) ++
                    [words("has the following inst:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            ;
                TailVars = [_ | _],
                Pieces = CompGenPieces ++ [words("arguments")] ++
                    piece_list_to_color_pieces(color_subject, "and", [],
                        [HeadVarPiece | TailVarPieces]) ++
                    [words("have the following insts:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            )
        ;
            PredOrFunc = pf_function,
            list.det_split_last(Vars, ArgVars, ReturnVar),
            ReturnVarName = mercury_var_to_name_only(VarTable, ReturnVar),
            ReturnVarPieces = [words("return value"), quote(ReturnVarName)],
            (
                ArgVars = [],
                % Ignore CompGenPieces, since the result arg of a function
                % will never be compiler generated.
                Pieces = [words("the")] ++
                    color_as_subject(ReturnVarPieces) ++
                    [words("has the following inst:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            ;
                ArgVars = [_ | _],
                ArgVarPieces = list.map(var_in_table_to_quote_piece(VarTable),
                    ArgVars),
                Pieces = CompGenPieces ++
                    [words(choose_number(ArgVars, "argument", "arguments"))] ++
                    piece_strict_list_to_color_pieces(color_subject, [],
                        ArgVarPieces) ++
                    [words("and the")] ++ color_as_subject(ReturnVarPieces) ++
                    [words("have the following insts:"), nl_indent_delta(1)] ++
                    inst_list_to_sep_lines(ModeInfo, Insts)
                % inst_list_to_sep_lines does nl_indent_delta(-1).
            )
        )
    ).

:- type argnum_var_type_inst
    --->    argnum_var_type_inst(int, prog_var, mer_type, mer_inst).

:- pred construct_argnum_var_type_inst_tuples(var_table::in, instmap::in,
    int::in, list(prog_var)::in, list(argnum_var_type_inst)::out) is det.

construct_argnum_var_type_inst_tuples(_, _, _, [], []).
construct_argnum_var_type_inst_tuples(VarTable, InstMap, ArgNum,
        [Var | Vars], [ArgTuple | ArgTuples]) :-
    lookup_var_type(VarTable, Var, Type),
    instmap_lookup_var(InstMap, Var, Inst),
    ArgTuple = argnum_var_type_inst(ArgNum, Var, Type, Inst),
    construct_argnum_var_type_inst_tuples(VarTable, InstMap, ArgNum + 1,
        Vars, ArgTuples).

%---------------------%

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
        inst_matches_initial_sub(VarType, VarInst, ProcInitialInst,
            ModuleInfo, _UpdatedModuleInfo, map.init, _Subst)
    then
        multi_map.add(ArgNum, ProcNum, !ArgNumMatchedProcs)
    else
        true
    ),
    find_satisfied_initial_insts_in_proc(ModuleInfo,
        ArgTuples, ProcInitialInsts, ProcNum, !ArgNumMatchedProcs).

%---------------------%

:- pred report_any_never_matching_args(mode_info::in,
    multi_map(int, int)::in, int::in, list(argnum_var_type_inst)::in,
    list(format_piece)::out) is det.

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
            ArgNumPieces = [words("compiler-generated"), nth_fixed(ArgNum),
                words("argument")]
        else
            ArgNumPieces = [nth_fixed(ArgNum - NumExtra), words("argument")]
        ),
        VarName = mercury_var_to_name_only(VarTable, Var),
        ArgNumVarPieces = [words("The")] ++
            color_as_subject(ArgNumPieces ++ [quote(VarName)]),
        % XXX We should consider generating the diff between VarInst
        % and the expected initial inst of the corresponding arg mode
        % in each vector of expected modes. This would require extending
        % the argnum_var_type_inst structure to include those initial insts.
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        ( if inst_contains_higher_order(ModuleInfo, VarInst) then
            HOPieces = [words("(For higher order insts like this,"),
                words("the mismatch is sometimes caused by"),
                words("the arity of the predicate or function"),
                words("being different in the inst than in the type.)"), nl]
        else
            HOPieces = []
        ),
        mode_info_get_var_table(ModeInfo, VarTable),
        % XXX Most of the situations for which are generating diagnostics
        % here has just one expected mode for each argument, so the wording
        % "any of those modes" here is wrong. But should the fix for this
        % include special-casing the handling of casts and events,
        % where the expected mode is universal for all their uses,
        % and maybe for unifications?
        BadArgPieces = ArgNumVarPieces ++
            [words("has inst")] ++
            color_as_incorrect(report_inst(ModeInfo, quote_short_inst,
                [suffix(",")], [nl_indent_delta(1)],
                [suffix(","), nl_indent_delta(-1)], VarInst)) ++
            [words("which does not match any of those modes."), nl] ++
            HOPieces ++
            BadArgPiecesTail
    ).

%---------------------------------------------------------------------------%

:- func mode_error_bad_higher_order_inst_to_spec(mode_info, prog_var, mer_inst,
    pred_or_func, user_arity, higher_order_mismatch_info) = error_spec.

mode_error_bad_higher_order_inst_to_spec(ModeInfo, PredVar, PredVarInst,
        ExpectedPredOrFunc, ExpectedUserArity, Mismatch) = Spec :-
    PreamblePieces = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    PredVarName = mercury_var_to_name_only(VarTable, PredVar),
    PredVarNamePieces = color_as_subject([quote(PredVarName)]),
    ExpPFStr = pred_or_func_to_full_str(ExpectedPredOrFunc),
    ExpectedUserArity = user_arity(ExpUserArityInt),
    % Don't let the specification of the expected arity be broken up,
    % since that would make the error message harder to read.
    ExpArityPiece = fixed("arity " ++ int_name_str(ExpUserArityInt)),
    ExpPieces0 = [words(ExpPFStr), words("of"), ExpArityPiece, suffix(",")],
    ExpPieces = color_as_correct(ExpPieces0),
    (
        Mismatch = mismatch_not_higher_order_type,
        ActPieces =
            [words("but the type of")] ++ PredVarNamePieces ++ [words("is")] ++
            color_as_incorrect([words("not a higher order type.")]) ++ [nl]
    ;
        Mismatch = mismatch_no_higher_order_inst_info,
        ( if PredVarInst = free then
            FreeVarPieces = [words("free variable.")],
            ActPieces = [words("but")] ++ PredVarNamePieces ++
                [words("is a")] ++ color_as_incorrect(FreeVarPieces) ++ [nl]
        else
            mode_info_get_module_info(ModeInfo, ModuleInfo),
            mode_info_get_instvarset(ModeInfo, InstVarSet),
            PredVarInstPieces = error_msg_inst(ModuleInfo, InstVarSet,
                dont_expand_named_insts, uod_user, quote_short_inst, [], [],
                [nl_indent_delta(1)], [nl_indent_delta(-1)], PredVarInst),
            ActPieces =
                [words("and the type of")] ++ PredVarNamePieces ++
                [words("does match that expectation."),
                words("However, to check the correctness of the call,"),
                words("the compiler also needs to know"),
                words("the modes of the arguments and the determinism"),
                words("of the"), words(ExpPFStr),
                words("that"), quote(PredVarName), words("represents."),
                words("The insts of higher order values"),
                words("should contain this information, but")] ++
                color_as_incorrect(PredVarInstPieces ++ [suffix(",")]) ++
                [words("which is the inst of"), quote(PredVarName),
                words("at this point,")] ++
                color_as_incorrect([words("does not.")]) ++
                [blank_line,
                words("The fix for this error is to add this information."),
                words("For example, given a higher order type such as"),
                nl_indent_delta(1),
                fixed(":- type callback_t == (pred(world, world, io, io))."),
                nl_indent_delta(-1),
                words("you would define a corresponding inst, such as"),
                nl_indent_delta(1),
                fixed(":- inst callback_i == (pred(in, out, di, uo) is det)."),
                nl_indent_delta(-1),
                words("This inst, which is usually called"),
                words("a higher order inst, specifies both"),
                words("the modes of the arguments"),
                words("and the determinism of a predicate."),
                words("Search for `higher order inst'"),
                words("in the Mercury language reference manual's"),
                words("chapter for higher order programming"),
                words("for a corresponding example for functions."), nl,
                blank_line,
                words("You can then tell the compiler that"),
                words("a value of type callback_t has inst callback_i"),
                words("by specifying"),
                words("either the mode"), quote("in(callback_i)"),
                words("(when taking a value of type callback_t as input)"),
                words("or the mode"), quote("out(callback_i)"),
                words("(when returning a value of type callback_t as output)"),
                suffix("."), nl]
        )
    ;
        Mismatch = mismatch_pred_vs_func(ActualPredOrFunc),
        ActPFStr = pred_or_func_to_full_str(ActualPredOrFunc),
        ActPieces = [words("but")] ++ PredVarNamePieces ++ [words("is a")] ++
            color_as_incorrect([words(ActPFStr), words("variable.")]) ++ [nl]
    ;
        Mismatch = mismatch_on_arity(ActualUserArity),
        ActualUserArity = user_arity(ActUserArityInt),
        ActArityPiece = fixed("arity " ++ int_name_str(ActUserArityInt)),
        ActPieces = [words("but")] ++ PredVarNamePieces ++ [words("has")] ++
            color_as_incorrect([ActArityPiece, suffix(".")]) ++ [nl]
    ),
    Pieces = PreamblePieces ++ [words("mode error: context requires a")]
        ++ ExpPieces ++ ActPieces,
    Phase = phase_mode_check(report_in_any_mode),
    Spec = spec($pred, severity_error, Phase, Context, Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_unschedulable_conjuncts_to_spec(mode_info,
    one_or_more(delayed_goal), schedule_culprit) = error_spec.

mode_error_unschedulable_conjuncts_to_spec(ModeInfo, OoMErrors, Culprit)
        = Spec :-
    mode_info_get_context(ModeInfo, Context),
    OoMErrors = one_or_more(HeadError, TailErrors),
    (
        TailErrors = [],
        Msgs1 = mode_error_conjunct_to_msgs(Context, ModeInfo, HeadError)
    ;
        TailErrors = [_ | _],
        % If there is more than one error, we use the setting of
        % --verbose-errors to decide between reporting just one,
        % and reporting them all. Unfortunately, we can't use the
        % verbose_and_nonverbose functor of the error_msg_component type
        % to package up the two cases, because we need to package up
        % multiple messages, each with its own context.
        Errors = [HeadError | TailErrors],
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
            NumErrors = list.length(Errors),
            ConjPieces = [words("mode error in conjunction. The next"),
                int_name(NumErrors), words("error messages"),
                words("indicate possible causes of this error."), nl],
            Msgs1Start = [msg(Context, Preamble ++ ConjPieces)],
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
            words("because it was impure."), nl],
        Msgs2 = [msg(Context, Pieces)]
    ;
        Culprit = goals_followed_by_impure_goal(ImpureGoal),
        ImpureGoal = hlds_goal(_, ImpureGoalInfo),
        ImpureGoalContext = goal_info_get_context(ImpureGoalInfo),
        Pieces1 = [words("The goal could not be reordered,"),
            words("because it was followed by an impure goal."), nl],
        Pieces2 = [words("This is the location of the impure goal."), nl],
        Msgs2 = [msg(Context, Pieces1),
            msg(ImpureGoalContext, Pieces2)]
    ),
    Phase = phase_mode_check(report_in_any_mode),
    Spec = error_spec($pred, severity_error, Phase, Msgs1 ++ Msgs2).

:- func prefix_with_blank_line(prog_context, list(error_msg))
    = list(error_msg).

prefix_with_blank_line(Context, Msgs) = [BlankMsg | Msgs] :-
    BlankMsg = msg(Context, [blank_line]).

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
        ModeError \= mode_error_non_ground_non_local_lambda_var(_, _)
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
        mode_info_get_var_table(!.ModeInfo, VarTable),
        VarNames = mercury_vars_to_name_only(VarTable, VarList),
        Pieces1 = [words("Floundered goal, waiting on {"),
            words(VarNames), words("}:"), nl],
        Msg1 = msg(Context, Pieces1),
        % XXX Shouldn't we check debug_modes_verbose instead of very_verbose?
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        (
            VeryVerbose = no,
            Msgs = [Msg1] ++ SubMsgs
        ;
            VeryVerbose = yes,

            OutInfo = init_hlds_out_info(Globals, output_debug),
            VarNameSrc = vns_var_table(VarTable),
            % XXX If we ever need this info again, we should print
            % the current pred_proc_id as well.
            varset.init(TVarSet),
            varset.init(InstVarSet),
            StringBuilder0 = string.builder.init,
            string.builder.append_string("\t\t",
                StringBuilder0, StringBuilder1),
            format_goal_nl(OutInfo, ModuleInfo, VarNameSrc,
                print_name_only, TVarSet, InstVarSet, 2u, ".\n", Goal,
                StringBuilder1, StringBuilder),
            GoalStr = string.builder.to_string(StringBuilder),
            % Note that GoalStr can be far, far longer than fits on one line,
            % but it will be formatted *with* newlines embedded within it.
            % It is generated by the same code as we use to generate HLDS
            % dumps, so it will look like that in the error message as well.
            % The nl's before and after ensure that GoalStr is on its own line
            % even if it turns out to be very short (which can happen e.g.
            % for unifications).
            Components2 = [always([nl, fixed(GoalStr), nl])],
            Msg2 = error_msg(no, treat_based_on_posn, 0u, Components2),
            Msgs = [Msg1, Msg2] ++ SubMsgs
        )
    ).

%---------------------------------------------------------------------------%

:- func mode_error_merge_par_conj_to_spec(mode_info, one_or_more(merge_error))
    = error_spec.

mode_error_merge_par_conj_to_spec(ModeInfo, MergeErrors) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    MainPieces = [words("mode error: mutually exclusive bindings"),
        words("in parallel conjunction."),
        words("(The current implementation does not permit"),
        words("parallel conjunctions to fail.)"), nl],
    MergeMsgLists = list.map(
        merge_error_to_msgs(ModeInfo, Context, is_not_disjunctive),
        one_or_more_to_list(MergeErrors)),
    list.condense(MergeMsgLists, MergeMsgs),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [msg(Context, Preamble ++ MainPieces) | MergeMsgs]).

%---------------------------------------------------------------------------%

:- func mode_error_merge_disj_to_spec(mode_info, merge_context,
    one_or_more(merge_error)) = error_spec.

mode_error_merge_disj_to_spec(ModeInfo, MergeContext, MergeErrors) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    MergeContextStr = merge_context_to_string(MergeContext),
    mode_info_get_context(ModeInfo, Context),
    MainPieces = [words("error:")] ++
        color_as_incorrect([words("mode mismatch")]) ++
        [words("in"), words(MergeContextStr), suffix("."), nl],
    MergeMsgLists = list.map(
        merge_error_to_msgs(ModeInfo, Context, is_disjunctive),
        one_or_more_to_list(MergeErrors)),
    list.condense(MergeMsgLists, MergeMsgs),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [msg(Context, Preamble ++ MainPieces) | MergeMsgs]).

:- func merge_context_to_string(merge_context) = string.

merge_context_to_string(merge_disj) = "disjunction".
merge_context_to_string(merge_if_then_else) = "if-then-else".
merge_context_to_string(merge_stm_atomic) = "atomic".

%---------------------------------------------------------------------------%

:- func mode_error_coerce_error_to_spec(mode_info, list(coerce_error))
    = error_spec.

mode_error_coerce_error_to_spec(ModeInfo, Errors) = Spec :-
    Error = list.det_head(Errors),
    Error = coerce_error(TermPath, FromType, ToType, Reason),
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    varset.init(TypeVarSet),
    (
        TermPath = [],
        TermPathPieces = [],
        CoercedTermOrTerms = words("coerced term")
    ;
        TermPath = [_ | _],
        TermPathPieces =
            [words("in the coerced term:"), nl] ++
            list.condense(list.map(make_term_path_piece, TermPath)),
        CoercedTermOrTerms = words("coerced subterm")
    ),
    TheTermPieces = [words("the")] ++ color_as_subject([CoercedTermOrTerms]),
    FromTypeStr =
        mercury_type_to_string(TypeVarSet, print_name_only, FromType),
    ToTypeStr =
        mercury_type_to_string(TypeVarSet, print_name_only, ToType),
    (
        Reason = input_inst_not_ground(Inst),
        ReasonPieces =
            TheTermPieces ++ [words("has instantiatedness")] ++
            color_as_incorrect(
                report_inst(ModeInfo, quote_short_inst, [suffix(",")],
                    [nl_indent_delta(1)], [suffix(","), nl_indent_delta(-1)],
                    Inst)) ++
            [words("but it must have a")] ++
            color_as_correct([words("ground")]) ++ [words("inst."), nl]
    ;
        Reason = cons_id_errors(_Inst, HeadConsError, TailConsErrors),
        classify_bound_inst_cons_id_errors([HeadConsError | TailConsErrors],
            set.init, InputBadConsIdSet,
            set.init, InputBadInstArityConsIdSet,
            set.init, ResultBadConsIdSet),
        InputBadConsIds = set.to_sorted_list(InputBadConsIdSet),
        InputBadInstArityConsIds =
            set.to_sorted_list(InputBadInstArityConsIdSet),
        ResultBadConsIds = set.to_sorted_list(ResultBadConsIdSet),
        % We print one to three blocks of text, with one block for each
        % kind of error that is present, in this order:
        %
        % - cons_ids that do not occur in the input type;
        % - cons_ids that do occur in the input type but have the wrong
        %   arity in the input inst; and
        % - cons_ids that do not occur in the result type.
        %
        % Because of the mode error, we cannot construct a final inst
        % for the coerce goal, which means that there is no analogue
        % of the second category for the result type. (Our test cases often
        % contain an inst on an output argument that *should* describe
        % the term that result of the coerce operation, but that is not
        % the same thing as an inst that *does* describe that result.)
        %
        % We construct these blocks in reverse order, so that we can put
        % the right suffix (a period or a semicolon) at the end of the
        % previous block (if any).
        (
            ResultBadConsIds = [],
            ResultBadConsIdMsgPieces = [],
            BadAritySuffix = "."
        ;
            ResultBadConsIds = [_ | _],
            ResultWrapUnqual = (func(C) = qual_cons_id_and_maybe_arity(C)),
            ResultBadConsIdPieces =
                list.map(ResultWrapUnqual, ResultBadConsIds),
            ResultBadConsIdMsgPieces =
                [words("the following function"),
                words(choose_number(ResultBadConsIds,
                    "symbol in the input term's instantiatedness is",
                    "symbols in the input term's instantiatedness are"))] ++
                color_as_incorrect([words("not part of the result type:")]) ++
                [nl_indent_delta(1)] ++
                piece_list_to_color_line_pieces(color_incorrect,
                    [suffix(".")], ResultBadConsIdPieces) ++
                [nl_indent_delta(-1)],
            BadAritySuffix = ";"
        ),
        (
            InputBadInstArityConsIds = [],
            InputBadInstArityConsIdMsgPieces = [],
            BadInputSuffix = "."
        ;
            InputBadInstArityConsIds = [_ | _],
            InputBadInstArityConsIdPieces =
                list.map(report_bad_arity_pieces, InputBadInstArityConsIds),
            InputBadInstArityConsIdMsgPieces =
                [words("the following function"),
                words(choose_number(InputBadInstArityConsIds,
                    "symbol is used with an incorrect arity",
                    "symbols are used with incorrect arities")),
                words("in the input term's instantiatedness:"),
                nl_indent_delta(1)] ++
                pieces_list_to_color_line_pieces(color_incorrect,
                    [suffix(BadAritySuffix)], InputBadInstArityConsIdPieces) ++
                [nl_indent_delta(-1)],
            BadInputSuffix = ";"
        ),
        (
            InputBadConsIds = [],
            InputBadConsIdMsgPieces = []
        ;
            InputBadConsIds = [_ | _],
            InputWrapUnqual = (func(C) = qual_cons_id_and_maybe_arity(C)),
            InputBadConsIdPieces = list.map(InputWrapUnqual, InputBadConsIds),
            InputBadConsIdMsgPieces =
                [words("the following function"),
                words(choose_number(InputBadConsIds,
                    "symbol in the input term's instantiatedness is",
                    "symbols in the input term's instantiatedness are"))] ++
                color_as_incorrect([words("not part of the input type:")]) ++
                [nl_indent_delta(1)] ++
                piece_list_to_color_line_pieces(color_incorrect,
                    [suffix(BadInputSuffix)], InputBadConsIdPieces) ++
                [nl_indent_delta(-1)]
        ),
        ReasonPieces =
            [words("cannot convert")] ++ TheTermPieces ++
            [words("from type"), quote(FromTypeStr),
            words("to"), quote(ToTypeStr), words("because")] ++
            InputBadConsIdMsgPieces ++
            InputBadInstArityConsIdMsgPieces ++
            ResultBadConsIdMsgPieces
    ;
        Reason = has_inst_expect_upcast(Inst),
        ReasonPieces =
            [words("cannot convert")] ++ TheTermPieces ++
            [words("from type"), quote(FromTypeStr),
            words("to"), quote(ToTypeStr),
            words("because it has instantiatedness")] ++
            color_as_incorrect(
                report_inst(ModeInfo, quote_short_inst, [suffix(",")],
                    [nl_indent_delta(1)], [suffix(","), nl_indent_delta(-1)],
                    Inst)) ++
            [words("and")] ++ color_as_incorrect([quote(FromTypeStr)]) ++
            [words("is not a subtype of"), quote(ToTypeStr), suffix("."), nl]
    ),
    Pieces = [words("mode error:")] ++ TermPathPieces ++ ReasonPieces,
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

:- func make_term_path_piece(coerce_error_term_path_step) =
    list(format_piece).

make_term_path_piece(Step) = Pieces :-
    Step = coerce_error_term_path_step(ConsId, ArgNum),
    ( if
        ConsId = du_data_ctor(du_ctor(_SymName, 1, _)),
        ArgNum = 1
    then
        ArgPieces = [words("in the argument")]
    else
        ArgPieces = [words("in the"), nth_fixed(ArgNum), words("argument")]
    ),
    Pieces = ArgPieces ++ [words("of function symbol"),
        unqual_cons_id_and_maybe_arity(ConsId), suffix(":"), nl].

:- pred classify_bound_inst_cons_id_errors(list(bound_inst_cons_id_error)::in,
    set(cons_id)::in, set(cons_id)::out,
    set({cons_id, arity, arity})::in, set({cons_id, arity, arity})::out,
    set(cons_id)::in, set(cons_id)::out) is det.

classify_bound_inst_cons_id_errors([],
        !InputBadConsIds, !InputBadInstArityConsIds, !ResultBadConsIds).
classify_bound_inst_cons_id_errors([Error | Errors],
        !InputBadConsIds, !InputBadInstArityConsIds, !ResultBadConsIds) :-
    (
        Error = bad_cons_id_input(ConsId),
        set.insert(ConsId, !InputBadConsIds)
    ;
        Error = bad_cons_id_input_inst_arity(ConsId, InstArity, ExpectedArity),
        set.insert({ConsId, InstArity, ExpectedArity},
            !InputBadInstArityConsIds)
    ;
        Error = bad_cons_id_result(ConsId),
        set.insert(ConsId, !ResultBadConsIds)
    ),
    classify_bound_inst_cons_id_errors(Errors,
        !InputBadConsIds, !InputBadInstArityConsIds, !ResultBadConsIds).

:- func report_bad_arity_pieces({cons_id, arity, arity}) = list(format_piece).

report_bad_arity_pieces({ConsId, InstArity, ExpectedArity}) = Pieces :-
    Pieces = [unqual_cons_id_and_maybe_arity(ConsId), prefix("(")] ++
        color_as_incorrect([int_fixed(InstArity)]) ++
        [suffix(";"), words("should be")] ++
        color_as_correct([int_fixed(ExpectedArity)]) ++
        [suffix(")")].

%---------------------------------------------------------------------------%

:- func mode_error_bind_locked_var_to_spec(mode_info, var_lock_reason,
    prog_var, mer_inst, mer_inst) = error_spec.

mode_error_bind_locked_var_to_spec(ModeInfo, Reason, Var, VarInst, Inst)
        = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    (
        Reason = var_lock_negation,
        ReasonStr = "attempt to bind a non-local variable inside a negation."
    ;
        Reason = var_lock_if_then_else,
        ReasonStr = "attempt to bind a non-local variable" ++
            " inside the condition of an if-then-else."
    ;
        Reason = var_lock_lambda(PredOrFunc),
        PredOrFuncS = pred_or_func_to_str(PredOrFunc),
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
    VarName = mercury_var_to_name_only(VarTable, Var),
    MainPieces = [words("scope error:"), words(ReasonStr), nl] ++
        [words("Variable")] ++ color_as_subject([quote(VarName)]) ++
        has_inst_expected_inst_was(ModeInfo, VarInst, Inst),
    (
        Reason = var_lock_negation,
        VerbosePieces =
            [words("A negation is allowed to bind only variables"),
            words("which are")] ++
            color_as_correct([words("local to the negation,")]) ++
            [words("i.e. those which are"),
            words("implicitly existentially quantified"),
            words("inside the scope of the negation."), nl]
    ;
        Reason = var_lock_if_then_else,
        VerbosePieces =
            [words("The condition of an if-then-else is"),
            words("allowed to bind only variables which are")] ++
            color_as_correct([words("local to the condition,")]) ++
            [words("or which occur")] ++
            color_as_correct([words("only in the condition"),
                words("and the"), quote("then"), words("part.")]) ++
            [nl]
    ;
        Reason = var_lock_lambda(_),
        VerbosePieces =
            [words("A lambda goal is allowed to bind only")] ++
            color_as_correct([words("its arguments")]) ++
            [words("and")] ++
            color_as_correct([words("variables local to the"),
                words("lambda expression.")]) ++
            [nl]
    ;
        Reason = var_lock_trace_goal,
        VerbosePieces =
            [words("A trace goal is allowed to bind only")] ++
            color_as_correct([words("variables which are"),
                words("local to the trace goal.")]) ++
            [nl]
    ;
        Reason = var_lock_atomic_goal,
        VerbosePieces =
            [words("An atomic goal")] ++
            color_as_incorrect([words("may not use")]) ++
            [words("the state variables")] ++
            color_as_subject([words("belonging to the outer scope.")]) ++
            [nl]
    ;
        Reason = var_lock_par_conj,
        VerbosePieces =
            [words("A nonlocal variable of a parallel conjunction"),
            words("may not be bound in")] ++
            color_as_incorrect([words("two or more conjuncts.")]) ++
            [nl]
    ),
    Spec = error_spec($pred, severity_error,
        phase_mode_check(report_in_any_mode),
        [simple_msg(Context,
            [always(Preamble ++ MainPieces),
            verbose_only(verbose_always, VerbosePieces)])]).

%---------------------------------------------------------------------------%

:- func mode_error_unexpected_final_inst_to_spec(mode_info, int, prog_var,
    mer_inst, mer_inst, final_inst_error) = error_spec.

mode_error_unexpected_final_inst_to_spec(ModeInfo, RawArgNum, Var,
        ActualInst, ExpectedInst, Reason) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
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
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_pred_id(ModeInfo, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_polymorphism_added_args(PredInfo, NumPolyAddedArgs),
    ArgNum = RawArgNum - NumPolyAddedArgs,
    ( if ArgNum >= 1 then
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        pred_form_arity(PredFormArityInt) =
            pred_info_pred_form_arity(PredInfo),
        ( if
            PredOrFunc = pf_function,
            ArgNum = PredFormArityInt
        then
            ArgNumPieces = [words("the function result")]
        else
            ArgNumPieces = [words("argument"), int_fixed(ArgNum)]
        )
    else
        % This should not happen. All code that passes around
        % compiler-generated arguments is of course also compiler generated,
        % and that code is supposed to be "correct by construction".
        % This is here in case that supposition turns out to be mistaken.
        ArgNumPieces = [words("compiler-generated argument"),
            int_fixed(RawArgNum)]
    ),
    VarName = mercury_var_to_name_only(VarTable, Var),
    Pieces = [words("mode error:")] ++ ArgNumPieces ++ [words(Problem), nl,
        words("Final instantiatedness of")] ++
        color_as_subject([quote(VarName)]) ++ [words("was")] ++
        color_as_incorrect(
            report_inst(ModeInfo, quote_short_inst, [suffix(","), nl],
                [nl_indent_delta(1)], [suffix(","), nl_indent_delta(-1)],
                ActualInst)) ++
        [words("expected final instantiatedness was")] ++
        color_as_correct(
            report_inst(ModeInfo, quote_short_inst, [suffix("."), nl],
                [nl_indent_delta(1)], [suffix("."), nl_indent_delta(-1)],
                ExpectedInst)),
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_error_in_callee_to_spec(mode_info, list(prog_var), list(mer_inst),
    pred_id, proc_id, list(mode_error_info)) = error_spec.

mode_error_in_callee_to_spec(!.ModeInfo, Vars, Insts,
        CalleePredId, CalleeProcId, CalleeModeErrors) = Spec :-
    Preamble = mode_info_context_preamble(!.ModeInfo),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_context(!.ModeInfo, Context),
    mode_info_get_var_table(!.ModeInfo, VarTable),
    (
        Vars = [],
        unexpected($pred, "Vars = []")
    ;
        Vars = [Var],
        VarPiece = var_in_table_to_quote_piece(VarTable, Var),
        MainPieces = [words("mode error: argument")] ++
            color_as_subject([VarPiece]) ++
            [words("has the following inst:"), nl_indent_delta(1)]
    ;
        Vars = [_, _ | _],
        VarPieces = list.map(var_in_table_to_quote_piece(VarTable), Vars),
        MainPieces = [words("mode error: arguments")] ++
            piece_list_to_color_pieces(color_subject, "and", [], VarPieces) ++
            [words("have the following insts:"), nl_indent_delta(1)]
    ),
    NoMatchPieces = inst_list_to_sep_lines(!.ModeInfo, Insts) ++
        [words("which does not match any of the valid modes for")],

    CalleePredIdPieces = describe_qual_pred_name(ModuleInfo, CalleePredId),
    VerboseCalleePieces = [words("the callee"),
        prefix("(")] ++ CalleePredIdPieces ++ [suffix(")"), nl,
        words("because of the following error."), nl],
    VerbosePieces = MainPieces ++ NoMatchPieces ++ VerboseCalleePieces,
    NonVerboseCalleePieces =
        [words("the callee, because of the following error."), nl],
    NonVerbosePieces = MainPieces ++ NoMatchPieces ++ NonVerboseCalleePieces,

    InitMsg = simple_msg(Context,
        [always(Preamble),
        verbose_and_nonverbose(VerbosePieces, NonVerbosePieces)]),
    (
        CalleeModeErrors = [FirstModeError | _],
        FirstModeError = mode_error_info(_, CalleeModeError,
            CalleeContext, CalleeModeContext),
        mode_info_set_pred_id(CalleePredId, !ModeInfo),
        mode_info_set_proc_id(CalleeProcId, !ModeInfo),
        mode_info_set_context(CalleeContext, !ModeInfo),
        mode_info_set_mode_context(CalleeModeContext, !ModeInfo),
        % Any variables in CalleeModeErrors have their names in
        % the var table of the callee, not in the var table of the procedure
        % we are modechecking now.
        module_info_proc_info(ModuleInfo, CalleePredId, CalleeProcId,
            CalleeProcInfo),
        proc_info_get_var_table(CalleeProcInfo, CalleeVarTable),
        mode_info_set_var_table(CalleeVarTable, !ModeInfo),
        CalleeModeErrorSpec0 = mode_error_to_spec(!.ModeInfo, CalleeModeError),
        module_info_get_globals(ModuleInfo, Globals),
        extract_spec_msgs(Globals, CalleeModeErrorSpec0, LaterMsgs0),
        (
            LaterMsgs0 = [],
            LaterMsgs = []
        ;
            LaterMsgs0 = [LaterHead0 | LaterTail],
            (
                LaterHead0 = msg(LaterContext, Pieces),
                LaterHead = error_msg(yes(LaterContext), always_treat_as_first,
                    0u, [always(Pieces)])
            ;
                LaterHead0 = no_ctxt_msg(Pieces),
                LaterHead = error_msg(no, always_treat_as_first,
                    0u, [always(Pieces)])
            ;
                LaterHead0 = simple_msg(LaterContext, Components),
                LaterHead = error_msg(yes(LaterContext), always_treat_as_first,
                    0u, Components)
            ;
                LaterHead0 = error_msg(MaybeLaterContext, _,
                    Indent, Components),
                LaterHead = error_msg(MaybeLaterContext, always_treat_as_first,
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

:- func mode_error_cannot_create_implied_mode_to_spec(mode_info,
    cannot_create_reason, prog_var, mer_inst, mer_inst) = error_spec.

mode_error_cannot_create_implied_mode_to_spec(ModeInfo, Reason, Var, VarInst,
        NonImpliedInitialInst) = Spec :-
    % This "error" message is really a "sorry, not implemented" message.
    % We only print the message if we will actually generate code.
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_op_mode(Globals, OpMode),
    mode_info_get_context(ModeInfo, Context),
    Phase = phase_mode_check(report_in_any_mode),
    ( if OpMode = opm_top_args(opma_augment(opmau_generate_code(_)), _) then
        Preamble = mode_info_context_preamble(ModeInfo),
        mode_info_get_var_table(ModeInfo, VarTable),
        (
            Reason = cannot_init_any,
            ReasonPieces = [words("initializing a solver variable")]
        ;
            Reason = cannot_deep_copy_partial_term,
            ReasonPieces = [words("copying a partially instantiated term")]
        ),
        VarName = mercury_var_to_name_only(VarTable, Var),
        Pieces = [words("sorry, the compiler currently"),
            words("cannot implement implied modes that require")] ++
            ReasonPieces ++ [suffix("."), nl,
            words("Variable")] ++
            color_as_subject([quote(VarName)]) ++
            has_inst_expected_inst_was(ModeInfo, VarInst,
                NonImpliedInitialInst),
        Spec = spec($pred, severity_error, Phase, Context, Preamble ++ Pieces)
    else
        Spec = error_spec($pred, severity_informational, Phase,
            [simple_msg(Context, [])])
    ).

%---------------------------------------------------------------------------%

:- func purity_error_should_be_in_promise_purity_scope_to_spec(
    negated_context_desc, mode_info, prog_var) = error_spec.

purity_error_should_be_in_promise_purity_scope_to_spec(NegCtxtDesc,
        ModeInfo, Var) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    VarName = mercury_var_to_name_only(VarTable, Var),
    (
        NegCtxtDesc = if_then_else,
        Pieces = [words("purity error: if-then-else should be inside"),
            words("a promise_purity scope, because non-local variable")] ++
            color_as_subject([quote(VarName)]) ++
            [words("has inst")] ++ color_as_incorrect([quote("any")]) ++
            [words("and appears in the condition."), nl]
    ;
        NegCtxtDesc = negation,
        Pieces = [words("purity error: negation should be inside"),
            words("a promise_purity scope, because non-local variable")] ++
            color_as_subject([quote(VarName)]) ++
            [words("has inst")] ++ color_as_incorrect([quote("any")]) ++
            [words("and appears in the body."), nl]
    ),
    Spec = spec($pred, severity_error,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func purity_error_lambda_should_be_any_to_spec(mode_info,
    one_or_more(prog_var)) = error_spec.

purity_error_lambda_should_be_any_to_spec(ModeInfo, OoMVars) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    OoMVars = one_or_more(HeadVar, TailVars),
    (
        TailVars = [],
        VarsWhoseInstsContain = [words("variable"),
            words("whose inst contains")],
        HeadVarPiece = var_in_table_to_quote_piece(VarTable, HeadVar),
        VarsDotPieces = color_as_incorrect([HeadVarPiece, suffix(".")])
    ;
        TailVars = [_ | _],
        VarsWhoseInstsContain = [words("variables"),
            words("whose insts contain")],
        Vars = [HeadVar | TailVars],
        VarPieces = list.map(var_in_table_to_quote_piece(VarTable), Vars),
        VarsDotPieces = piece_list_to_color_pieces(color_incorrect, "and",
            [suffix(".")], VarPieces)
    ),
    Pieces = [words("purity error: lambda is"), quote("ground"),
        words("but contains the following non-local")] ++
        VarsWhoseInstsContain ++ [quote("any"), suffix(":")] ++
        VarsDotPieces ++ [nl],
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
%---------------------------------------------------------------------------%
%
% Utility predicates needed to print more than one kind of error.
%

:- type maybe_is_disjunctive
    --->    is_not_disjunctive
    ;       is_disjunctive.

:- func merge_error_to_msgs(mode_info, prog_context, maybe_is_disjunctive,
    merge_error) = list(error_msg).

merge_error_to_msgs(ModeInfo, MainContext, IsDisjunctive, MergeError) = Msgs :-
    MergeError = merge_error(Var, ContextsInsts0),
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_var_table(ModeInfo, VarTable),
    VarNamePiece = var_in_table_to_quote_piece(VarTable, Var),
    list.sort(ContextsInsts0, ContextsInsts),
    lookup_var_type(VarTable, Var, VarType),
    count_ground_insts(ModuleInfo, VarType, ContextsInsts,
        0, NumGroundInsts, 0, NumAllInsts),
    ( if
        IsDisjunctive = is_disjunctive,
        NumGroundInsts < NumAllInsts,
        NumGroundInsts * 2 > NumAllInsts
    then
        % More than half the insts are ground, so it is likely that they
        % were *all* intended to be ground, but not all actually *are* ground,
        % which is likely to be the bug.
        CommonPieces = [words("The variable")] ++
            color_as_subject([VarNamePiece]) ++
            [words("is")] ++ color_as_incorrect([words("ground")]) ++
            [words("in"), int_fixed(NumGroundInsts), words("out of"),
            int_fixed(NumAllInsts), words("branches."), nl],
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
            report_inst_in_context(ModeInfo, report_inst_and_groundness,
                yes(color_inconsistent), VarNamePiece, VarType),
            ContextsInsts),
        Msgs = [VarMsg | InstMsgs]
    else if
        IsDisjunctive = is_disjunctive,
        0 < NumGroundInsts,
        NumGroundInsts < NumAllInsts
    then
        VarPieces = [words("The variable")] ++
            color_as_subject([VarNamePiece]) ++
            [words("is ground in some branches but not others."), nl],
        VarMsg = msg(MainContext, VarPieces),
        InstMsgs = list.map(
            report_inst_in_context(ModeInfo, report_groundness_only,
                yes(color_inconsistent), VarNamePiece, VarType),
            ContextsInsts),
        Msgs = [VarMsg | InstMsgs]
    else
        VarPieces = [words("The variable")] ++
            color_as_subject([VarNamePiece]) ++
            [words("has the following instantiation states."), nl],
        VarMsg = msg(MainContext, VarPieces),
        InstMsgs = list.map(
            report_inst_in_context(ModeInfo, report_inst_only,
                yes(color_inconsistent), VarNamePiece, VarType),
            ContextsInsts),
        Msgs = [VarMsg | InstMsgs]
    ).

%---------------------%

:- pred count_ground_insts(module_info::in, mer_type::in,
    assoc_list(prog_context, mer_inst)::in,
    int::in, int::out, int::in, int::out) is det.

count_ground_insts(_ModuleInfo, _Type, [], !NumGroundInsts, !NumAllInsts).
count_ground_insts(ModuleInfo, Type, [ContextInst | ContextsInsts],
        !NumGroundInsts, !NumAllInsts) :-
    ContextInst = _Context - Inst,
    ( if inst_is_ground(ModuleInfo, Type, Inst) then
        !:NumGroundInsts = !.NumGroundInsts + 1
    else
        true
    ),
    !:NumAllInsts = !.NumAllInsts + 1,
    count_ground_insts(ModuleInfo, Type, ContextsInsts,
        !NumGroundInsts, !NumAllInsts).

%---------------------%

:- type report_inst_how
    --->    report_inst_only
    ;       report_groundness_only
    ;       report_inst_and_groundness.

:- func report_inst_in_context(mode_info, report_inst_how, maybe(color_name),
    format_piece, mer_type, pair(prog_context, mer_inst)) = error_msg.

report_inst_in_context(ModeInfo, ReportIsGround, MaybeColor, VarNamePiece,
        Type, Context - Inst) = Msg :-
    (
        ReportIsGround = report_inst_only,
        Pieces = report_inst_in_branch(ModeInfo, MaybeColor, VarNamePiece,
            no, Inst),
        Msg = msg(Context, Pieces)
    ;
        ReportIsGround = report_groundness_only,
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        ( if inst_is_ground(ModuleInfo, Type, Inst) then
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "ground")
        else if Inst = free then
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "free")
        else
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "not ground")
        ),
        Msg = msg(Context, Pieces)
    ;
        ReportIsGround = report_inst_and_groundness,
        mode_info_get_module_info(ModeInfo, ModuleInfo),
        ( if inst_is_ground(ModuleInfo, Type, Inst) then
            Pieces = report_inst_in_branch(ModeInfo, MaybeColor, VarNamePiece,
                yes(ground), Inst),
            Msg = simple_msg(Context, [verbose_and_nonverbose(Pieces, [])])
        else
            Pieces = report_inst_in_branch(ModeInfo, MaybeColor, VarNamePiece,
                yes(nonground), Inst),
            Msg = msg(Context, Pieces)
        )
    ).

:- type ground_or_nonground
    --->    ground
    ;       nonground.

:- func report_inst_in_branch(mode_info, maybe(color_name), format_piece,
    maybe(ground_or_nonground), mer_inst) = list(format_piece).

report_inst_in_branch(ModeInfo, MaybeColor, VarNamePiece,
        MaybeGroundOrNonGround, Inst) = Pieces :-
    (
        MaybeGroundOrNonGround = no,
        ( if Inst = ground(shared, none_or_default_func) then
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "ground")
        else if Inst = free then
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "free")
        else
            IntroPieces = [words("In this branch,"), VarNamePiece,
                words("has instantiatedness")],
            Pieces = report_inst_in_branch_detail(ModeInfo, IntroPieces,
                MaybeColor, Inst)
        )
    ;
        MaybeGroundOrNonGround = yes(ground),
        ( if Inst = ground(shared, none_or_default_func) then
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "ground")
        else
            IntroPieces = [words("In this branch,"), VarNamePiece,
                words("has the ground instantiatedness")],
            Pieces = report_inst_in_branch_detail(ModeInfo, IntroPieces,
                MaybeColor, Inst)
        )
    ;
        MaybeGroundOrNonGround = yes(nonground),
        ( if Inst = free then
            Pieces = report_inst_in_branch_simple(MaybeColor, VarNamePiece,
                "free")
        else
            IntroPieces = [words("In this branch,"), VarNamePiece,
                words("has the non-ground instantiatedness")],
            Pieces = report_inst_in_branch_detail(ModeInfo, IntroPieces,
                MaybeColor, Inst)
        )
    ).

:- func report_inst_in_branch_simple(maybe(color_name), format_piece, string)
    = list(format_piece).

report_inst_in_branch_simple(MaybeColor, VarNamePiece, FreeOrGround)
        = Pieces :-
    MainPieces = [words("In this branch,"), VarNamePiece, words("is")] ++
        maybe_color_pieces(MaybeColor, [words(FreeOrGround), suffix(".")]) ++
        [nl],
    Pieces = [nl_indent_delta(1) | MainPieces] ++ [nl_indent_delta(-1)].

:- func report_inst_in_branch_detail(mode_info, list(format_piece),
    maybe(color_name), mer_inst) = list(format_piece).

report_inst_in_branch_detail(ModeInfo, IntroPieces, MaybeColor, Inst)
        = Pieces :-
    Pieces = [nl_indent_delta(1) | IntroPieces] ++ [nl_indent_delta(1)] ++
        maybe_color_pieces(MaybeColor,
            report_inst(ModeInfo, fixed_short_inst,
                [suffix("."), nl_indent_delta(-2)],
                [], [suffix("."), nl_indent_delta(-2)], Inst)).

%------------%

:- func inst_list_to_sep_lines(mode_info, list(mer_inst))
    = list(format_piece).

inst_list_to_sep_lines(_ModeInfo, []) = [].
inst_list_to_sep_lines(ModeInfo, [Inst | Insts]) = Pieces :-
    (
        Insts = [],
        Pieces = report_inst(ModeInfo, fixed_short_inst, [], [], [], Inst) ++
            [nl_indent_delta(-1)]
    ;
        Insts = [_ | _],
        HeadPieces = report_inst(ModeInfo, fixed_short_inst, [suffix(",")],
            [], [suffix(",")], Inst) ++ [nl],
        TailPieces = inst_list_to_sep_lines(ModeInfo, Insts),
        Pieces = HeadPieces ++ TailPieces
    ).

%------------%

:- func has_inst_expected_inst_was(mode_info, mer_inst, mer_inst)
    = list(format_piece).

has_inst_expected_inst_was(ModeInfo, ActualInst, ExpectedInst) =
    has_instantiatedness(ModeInfo, yes(color_incorrect), ActualInst, ",") ++
    expected_inst_was(ModeInfo, ExpectedInst).

:- func has_instantiatedness(mode_info, maybe(color_name), mer_inst, string)
    = list(format_piece).

has_instantiatedness(ModeInfo, MaybeColor, Inst, Suffix) = Pieces :-
    InstPieces0 = report_inst(ModeInfo, quote_short_inst, [suffix(Suffix), nl],
        [nl_indent_delta(1)], [suffix(Suffix), nl_indent_delta(-1)], Inst),
    InstPieces = maybe_color_pieces(MaybeColor, InstPieces0),
    Pieces = [words("has instantiatedness") | InstPieces].

:- func expected_inst_was(mode_info, mer_inst) = list(format_piece).

expected_inst_was(ModeInfo, Inst) = Pieces :-
    InstPieces0 = report_inst(ModeInfo, quote_short_inst, [suffix("."), nl],
        [nl_indent_delta(1)], [suffix("."), nl_indent_delta(-1)], Inst),
    InstPieces = color_as_correct(InstPieces0),
    Pieces = [words("expected instantiatedness was") | InstPieces].

:- func report_inst(mode_info, short_inst,
    list(format_piece), list(format_piece), list(format_piece),
    mer_inst) = list(format_piece).

report_inst(ModeInfo, ShortInstQF, ShortInstSuffix,
        LongInstPrefix, LongInstSuffix, Inst0) = Pieces :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    Pieces = error_msg_inst(ModuleInfo, InstVarSet, expand_named_insts,
        uod_user, ShortInstQF, [], ShortInstSuffix,
        LongInstPrefix, LongInstSuffix, Inst0).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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

:- func mode_warning_cannot_succeed_var_var(mode_info,
    prog_var, prog_var, mer_inst, mer_inst) = error_spec.

mode_warning_cannot_succeed_var_var(ModeInfo, X, Y, InstX, InstY) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    NameX = mercury_var_to_name_only(VarTable, X),
    NameY = mercury_var_to_name_only(VarTable, Y),
    MaybeColor = yes(color_inconsistent),
    % XXX It would make sense to color as subject only the last reference
    % to each of NameX and NameY, but this would introduce an asymmetry
    % with respect to mode_warning_cannot_succeed_var_functor, which has
    % only NameX.
    Pieces = [words("warning: unification of")] ++
        color_as_subject([quote(NameX)]) ++ [words("and")] ++
        color_as_subject([quote(NameY)]) ++ [words("cannot succeed."), nl] ++
        color_as_subject([quote(NameX)]) ++
        has_instantiatedness(ModeInfo, MaybeColor, InstX, ",") ++
        color_as_subject([quote(NameY)]) ++
        has_instantiatedness(ModeInfo, MaybeColor, InstY, "."),
    Spec = spec($pred, severity_warning,
        phase_mode_check(report_only_if_in_all_modes),
        Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_warning_cannot_succeed_var_functor(mode_info, prog_var, mer_inst,
    cons_id) = error_spec.

mode_warning_cannot_succeed_var_functor(ModeInfo, X, InstX, ConsId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    NameX = mercury_var_to_name_only(VarTable, X),
    ConsIdStr = mercury_cons_id_to_string(output_mercury,
        does_not_need_brackets, ConsId),
    % XXX The term that X is being unified is ConsId *only* if ConsId
    % has arity zero; otherwise, it is a term in which ConsId is only
    % the principal functor. Can we find a wording here that
    %
    % - is not misleading on this issue, and
    % - is reasonably short and easily understandable?
    %
    % I (zs) feel that "unification of X and the term whose principal functor
    % is ConsId" would fail on that last point, even if we used that wording
    % only ConsId is not a constant.
    %
    % XXX See also the XXX in mode_warning_cannot_succeed_var_var.
    Pieces = [words("warning: unification of")] ++
        color_as_subject([quote(NameX)]) ++ [words("and")] ++
        color_as_subject([words(ConsIdStr)]) ++
        [words("cannot succeed."), nl] ++
        color_as_subject([quote(NameX)]) ++
        has_instantiatedness(ModeInfo, yes(color_incorrect), InstX, "."),
    Spec = spec($pred, severity_warning,
        phase_mode_check(report_only_if_in_all_modes),
        Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%

:- func mode_warning_cannot_succeed_ground_occur_check(mode_info, prog_var,
    cons_id) = error_spec.

mode_warning_cannot_succeed_ground_occur_check(ModeInfo, X, ConsId) = Spec :-
    Preamble = mode_info_context_preamble(ModeInfo),
    mode_info_get_context(ModeInfo, Context),
    mode_info_get_var_table(ModeInfo, VarTable),
    NameX = mercury_var_to_name_only(VarTable, X),
    ConsIdStr = mercury_cons_id_to_string(output_mercury,
        does_not_need_brackets, ConsId),
    Pieces = [words("warning: unification of")] ++
        color_as_subject([quote(NameX)]) ++ [words("and")] ++
        color_as_subject([words(ConsIdStr)]) ++
        [words("cannot succeed, because")] ++
        color_as_incorrect([quote(NameX), words("cannot be equal"),
            words("to a term containing itself.")]) ++
        [nl],
    Spec = spec($pred, severity_warning,
        phase_mode_check(report_in_any_mode), Context, Preamble ++ Pieces).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates needed to print both errors and warnings.
%

:- func mode_info_context_preamble(mode_info) = list(format_piece).

mode_info_context_preamble(ModeInfo) = Pieces :-
    mode_info_get_module_info(ModeInfo, ModuleInfo),
    mode_info_get_pred_id(ModeInfo, PredId),
    mode_info_get_proc_id(ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo),
    pred_info_get_origin(PredInfo, PredOrigin),
    ( if
        PredOrigin = origin_user(OriginUser),
        OriginUser = user_made_instance_method(PFMethodSymNameArity, _),
        PFMethodSymNameArity = pred_pf_name_arity(_, MethodSymName, _)
    then
        Name = unqualify_name(MethodSymName),
        ExtraMethodPieces = [words("type class method implementation for")]
    else
        Name = pred_info_name(PredInfo),
        ExtraMethodPieces = []
    ),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    mode_info_get_instvarset(ModeInfo, InstVarSet),
    SymName = unqualified(Name),
    pred_info_get_markers(PredInfo, PredMarkers),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    strip_module_names_from_mode_list(strip_builtin_module_name,
        do_not_set_default_func, Modes0, Modes),
    MaybeDet = no,
    ModeSubDeclStr = mercury_mode_subdecl_to_string(output_debug, PredOrFunc,
        InstVarSet, SymName, Modes, MaybeDet),
    mode_info_get_mode_context(ModeInfo, ModeContext),
    ModeContextPieces =
        mode_context_to_pieces(ModeInfo, ModeContext, PredMarkers),
    Pieces = [words("In clause for")] ++ ExtraMethodPieces ++
        [words_quote(ModeSubDeclStr), suffix(":"), nl] ++
        ModeContextPieces.

    % XXX Some parts of the mode context never get set up.
    %
:- func mode_context_to_pieces(mode_info, mode_context, pred_markers)
    = list(format_piece).

mode_context_to_pieces(ModeInfo, ModeContext, Markers) = Pieces :-
    (
        ModeContext = mode_context_not_call_or_unify,
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
            _LastContextWord, [], Pieces)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

should_report_mode_warning_for_pred_origin(Origin) = Report :-
    (
        Origin = origin_user(OriginUser),
        (
            ( OriginUser = user_made_pred(_, _, _)
            ; OriginUser = user_made_lambda(_, _, _)
            ),
            Report = yes
        ;
            ( OriginUser = user_made_class_method(_, _)
            ; OriginUser = user_made_instance_method(_, _)
            ; OriginUser = user_made_assertion(_, _, _)
            ),
            Report = no
        )
    ;
        ( Origin = origin_compiler(_)
        ; Origin = origin_pred_transform(_, _, _)
        ; Origin = origin_proc_transform(_, _, _, _)
        ),
        Report = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

mode_decl_to_string(Lang, ProcId, PredInfo) = String :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = pred_info_name(PredInfo),
    SymName = unqualified(Name),
    pred_info_get_proc_table(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),
    proc_info_declared_argmodes(ProcInfo, Modes0),
    proc_info_get_declared_determinism(ProcInfo, MaybeDet),
    varset.init(InstVarSet),
    strip_module_names_from_mode_list(strip_builtin_module_name,
        do_not_set_default_func, Modes0, Modes),
    String = mercury_mode_subdecl_to_string(Lang, PredOrFunc,
        InstVarSet, SymName, Modes, MaybeDet).

%---------------------------------------------------------------------------%
:- end_module check_hlds.mode_errors.
%---------------------------------------------------------------------------%
