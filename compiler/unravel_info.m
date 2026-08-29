%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2011 The University of Melbourne.
% Copyright (C) 2014-2016, 2018-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: state_var.m.
% Main author of original version: rafe.
% Main author of the current version, rewritten in 2011: zs.
%
% This module defines the unravel_info type, which is the representation
% of the state of the transformation to superhomogeneous form, and some of
% the operations on it. These consist mostly of creating new variables,
% generating diagnostics, and recording them in the unravel_info.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.unravel_info.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.make_hlds.state_var.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module one_or_more.

%---------------------------------------------------------------------------%

    % This type describes the state of the code that converts goals
    % from their parse tree form to their HLDS form. Almost all the code
    % in all the modules of the make_hlds package that handle goals
    % pass around values of this type as effectively global state,
    % with persistent updates. (The state of the state var transformation
    % itself is threaded through that code in a different manner;
    % see the definition of the svar_state type below.)
    %
    % With one exception, all of the fields are writeable.
:- type unravel_info
    --->    unravel_info(
                % The module_info, which we use for several purposes.
                % Most uses are readonly, including getting the globals
                % for option lookup, and the module name for creating
                % debug output streams.
                %
                % The only situation in which we update the module_info field
                % is when processing disable_warning scopes that disable
                % the warning for occurs check violations. In that case,
                % we set the option controlling that warning to "no"
                % while processing the goal in the scope, and reset it
                % afterwards. Such scopes are rare enough that storing the
                % value of that option as a separate field in this structure
                % would not be worthwhile.
                ui_module_info      :: module_info,

                % The value of the from_ground_term_threshold option.
                % This field duplicates the value stored in the globals
                % structure, but it is needed often enough that a separate
                % fast-access copy is worthwhile.
                % This field is read-only.
                ui_fgt_threshold    :: int,

                % The store where we record information about what entities
                % imported from other modules are used. We use that info
                % to generate warnings about unused imports.
                ui_qual_info        :: qual_info,

                % The varset of the clause whose goal we are converting.
                % New instances of state variables are allocated from here.
                ui_varset           :: prog_varset,

                % The part of the state of the state var transformation
                % that is updated persistently (meaning, that once we create
                % a new version, we don't go back to look at previous
                % versions.)
                ui_state_var_store  :: svar_store,

                % The errors and warnings that we definitely want to print.
                % (The svar_store also contains warn_specs, but we print those
                % only as hints *if and when* we later find certain other kinds
                % of errors.)
                ui_err_specs       :: list(err_spec),
                ui_warn_specs      :: list(warn_spec)
            ).

%---------------------------------------------------------------------------%

:- pred create_new_unravel_var(prog_var::out,
    unravel_info::in, unravel_info::out) is det.

:- pred create_new_named_unravel_var(string::in, prog_var::out,
    unravel_info::in, unravel_info::out) is det.

:- pred record_unravel_found_syntax_error(
    unravel_info::in, unravel_info::out) is det.

:- pred add_unravel_err(err_spec::in,
    unravel_info::in, unravel_info::out) is det.
:- pred add_unravel_errs(list(err_spec)::in,
    unravel_info::in, unravel_info::out) is det.
:- pred add_unravel_oom_errs(one_or_more(err_spec)::in,
    unravel_info::in, unravel_info::out) is det.

:- pred add_unravel_warn(warn_spec::in,
    unravel_info::in, unravel_info::out) is det.
:- pred add_unravel_warns(list(warn_spec)::in,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%

    % Does the given argument list have a function result term
    % that tries to use state var notation to refer to *two* terms?
    %
    % If yes, return the state variable involved, and the context of the
    % reference.
    %
:- pred illegal_state_var_func_result(pred_or_func::in, list(prog_term)::in,
    svar::out, prog_context::out) is semidet.

    % Does the given term have the form a !X, i.e. does it represent
    % *two* arguments? This is not acceptable in some contexts, such as
    % function results and lambda expression arguments.
    %
    % If yes, return the state variable involved, and the context of the
    % reference.
    %
:- pred is_term_a_bang_state_pair(prog_term::in,
    svar::out, prog_context::out) is semidet.

%---------------------------------------------------------------------------%

:- pred report_illegal_state_var_update(prog_context::in,
    string::in, prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_illegal_func_svar_result(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.
:- func report_illegal_func_svar_result_raw(prog_context,
    prog_varset, svar) = err_spec.

:- pred report_illegal_bang_svar_lambda_arg(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.
:- func report_illegal_bang_svar_lambda_arg_raw(prog_context,
    prog_varset, svar) = err_spec.

:- pred report_non_visible_state_var(string::in, prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_uninitialized_state_var(option::in, prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_repeated_head_state_var(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_state_var_shadow(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_missing_inits_in_ite(prog_context::in, list(string)::in,
    string::in, string::in, warn_spec::out) is det.

:- pred report_missing_inits_in_disjunct(prog_context::in, list(string)::in,
    list(warn_spec)::in, list(warn_spec)::out) is det.

:- pred report_svar_unify_error(prog_context::in, svar::in,
    unravel_info::in, unravel_info::out) is det.

:- pred report_any_unneeded_svars_in_lambda(prog_context::in,
    list(mer_mode)::in, goal::in, hlds_goal::in, unused_statevar_arg_map::in,
    unravel_info::in, unravel_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_vars.
:- import_module hlds.mode_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

create_new_unravel_var(Var, !UrInfo) :-
    VarSet0 = !.UrInfo ^ ui_varset,
    varset.new_var(Var, VarSet0, VarSet),
    !UrInfo ^ ui_varset := VarSet.

create_new_named_unravel_var(Name, Var, !UrInfo) :-
    VarSet0 = !.UrInfo ^ ui_varset,
    varset.new_named_var(Name, Var, VarSet0, VarSet),
    !UrInfo ^ ui_varset := VarSet.

record_unravel_found_syntax_error(!UrInfo) :-
    QualInfo0 = !.UrInfo ^ ui_qual_info,
    qual_info_set_found_syntax_error(yes, QualInfo0, QualInfo),
    !UrInfo ^ ui_qual_info := QualInfo.

add_unravel_err(NewSpec, !UrInfo) :-
    Specs0 = !.UrInfo ^ ui_err_specs,
    Specs = [NewSpec | Specs0],
    !UrInfo ^ ui_err_specs := Specs.

add_unravel_errs(NewSpecs, !UrInfo) :-
    (
        NewSpecs = []
    ;
        NewSpecs = [_ | _],
        Specs0 = !.UrInfo ^ ui_err_specs,
        Specs = NewSpecs ++ Specs0,
        !UrInfo ^ ui_err_specs := Specs
    ).

add_unravel_oom_errs(one_or_more(HeadSpec, TailSpecs), !UrInfo) :-
    Specs0 = !.UrInfo ^ ui_err_specs,
    Specs = [HeadSpec | TailSpecs] ++ Specs0,
    !UrInfo ^ ui_err_specs := Specs.

add_unravel_warn(NewSpec, !UrInfo) :-
    Specs0 = !.UrInfo ^ ui_warn_specs,
    Specs = [NewSpec | Specs0],
    !UrInfo ^ ui_warn_specs := Specs.

add_unravel_warns(NewSpecs, !UrInfo) :-
    Specs0 = !.UrInfo ^ ui_warn_specs,
    Specs = NewSpecs ++ Specs0,
    !UrInfo ^ ui_warn_specs := Specs.

%---------------------------------------------------------------------------%
%
% Test for various kinds of errors.
%

illegal_state_var_func_result(pf_function, ArgTerms, StateVar, Context) :-
    list.last(ArgTerms, LastArgTerm),
    is_term_a_bang_state_pair(LastArgTerm, StateVar, Context).

is_term_a_bang_state_pair(ArgTerm, StateVar, Context) :-
    ArgTerm = functor(atom("!"), [variable(StateVar, Context)], _).

%---------------------------------------------------------------------------%
%
% Report various kinds of errors.
%

report_illegal_state_var_update(Context, RO_Construct, RO_Context,
        StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces1 = [words("Error: you cannot use")] ++
        color_as_incorrect([quote("!:" ++ Name)]) ++
        [words("here due to the surrounding"), words(RO_Construct),
            suffix(";"),
        words("you may only refer to")] ++
        color_as_correct([quote("!." ++ Name), suffix(".")]) ++ [nl],
    Msg1 = msg(Context, Pieces1),
    Pieces2 = [words("Here is the surrounding context that makes"),
        words("state variable"), quote(Name), words("readonly."), nl],
    Msg2 = msg(RO_Context, Pieces2),
    Spec = gen_spec($pred, severity_error, phase_pt2h, [Msg1, Msg2]),
    add_unravel_err(Spec, !UrInfo).

%---------------------------------------------------------------------------%

report_illegal_func_svar_result(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Spec = report_illegal_func_svar_result_raw(Context, VarSet, StateVar),
    add_unravel_err(Spec, !UrInfo).

report_illegal_func_svar_result_raw(Context, VarSet, StateVar) = Spec :-
    Name = varset.lookup_name(VarSet, StateVar),
    % While having !.Var appear as a function argument is quite ordinary,
    % having it appear as a function *result* is not. We therefore do not
    % suggest it as a likely correction.
    Pieces = [words("Error: since it represents two arguments, not one,")] ++
        color_as_incorrect([quote("!" ++ Name)]) ++
        [words("cannot be a function result. You probably meant")] ++
        color_as_correct([fixed("!:" ++ Name), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces).

%---------------------------------------------------------------------------%

report_illegal_bang_svar_lambda_arg(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Spec = report_illegal_bang_svar_lambda_arg_raw(Context, VarSet, StateVar),
    add_unravel_err(Spec, !UrInfo).

report_illegal_bang_svar_lambda_arg_raw(Context, VarSet, StateVar) = Spec :-
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:")] ++
        color_as_incorrect([quote("!" ++ Name)]) ++
        [words("cannot be a lambda argument."), nl,
        words("Perhaps you meant")] ++
        color_as_correct([quote("!." ++ Name)]) ++
        [words("or")] ++
        color_as_correct([quote("!:" ++ Name), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces).

%---------------------------------------------------------------------------%

report_non_visible_state_var(DorC, Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error: state variable")] ++
        color_as_incorrect([quote("!" ++ DorC ++ Name)]) ++
        [words("is not visible in this context."), nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    add_unravel_err(Spec, !UrInfo).

%---------------------------------------------------------------------------%

report_uninitialized_state_var(WarnOption, Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: you cannot refer to")] ++
        color_as_subject([quote("!." ++ Name)]) ++
        [words("here, because that state variable has")] ++
        color_as_incorrect([words("not been initialized")]) ++
        [words("yet."), nl],
    Spec = spec($pred, severity_warning(WarnOption), phase_pt2h,
        Context, Pieces),
    add_unravel_warn(Spec, !UrInfo).

%---------------------------------------------------------------------------%

report_repeated_head_state_var(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: clause head introduces")] ++
        color_as_incorrect([words("state variable"), quote(Name)]) ++
        [words("more than once."), nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    add_unravel_err(Spec, !UrInfo).

%---------------------------------------------------------------------------%

report_state_var_shadow(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Warning: new state variable")] ++
        color_as_subject([quote(Name)]) ++
        color_as_incorrect([words("shadows old one.")]) ++ [nl],
    Spec = spec($pred, severity_warning(warn_state_var_shadowing), phase_pt2h,
        Context, Pieces),
    add_unravel_warn(Spec, !UrInfo).

%---------------------------------------------------------------------------%

report_missing_inits_in_ite(Context, NextStateVars,
        WhenMissing, WhenNotMissing, Spec) :-
    NextStateVarsPieces = quote_list_to_color_pieces(color_subject, "and",
        [suffix(",")], NextStateVars),
    Pieces = [words("When the condition"), words(WhenNotMissing), suffix(","),
        words("the if-then-else")] ++
        color_as_inconsistent([words("defines")]) ++
        NextStateVarsPieces ++
        [words("but when the condition"), words(WhenMissing), suffix(",")] ++
        color_as_inconsistent([words("it does not.")]) ++ [nl],
    Spec = spec($pred, severity_warning(warn_missing_state_var_init),
        phase_pt2h, Context, Pieces).

report_missing_inits_in_disjunct(Context, NextStateVars, !Specs) :-
    Pieces = [words("Other disjuncts define")] ++
        quote_list_to_color_pieces(color_subject, "and", [suffix(",")],
            NextStateVars) ++
        color_as_incorrect([words("but not this one.")]) ++ [nl],
    Spec = spec($pred, severity_warning(warn_missing_state_var_init),
        phase_pt2h, Context, Pieces),
    % The intention is that our caller got !.Specs from the state var store's
    % store_missing_init_specs field, and will put the updated list back there.
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

report_svar_unify_error(Context, StateVar, !UrInfo) :-
    VarSet = !.UrInfo ^ ui_varset,
    Name = varset.lookup_name(VarSet, StateVar),
    Pieces = [words("Error:")] ++
        color_as_incorrect([fixed("!" ++ Name)]) ++
        [words("cannot appear as a unification argument."), nl,
        words("You probably meant")] ++
        color_as_correct([fixed("!." ++ Name)]) ++ [words("or")] ++
        color_as_correct([fixed("!:" ++ Name), suffix(".")]) ++ [nl],
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    add_unravel_err(Spec, !UrInfo).

%---------------------------------------------------------------------------%

report_any_unneeded_svars_in_lambda(Context, Modes, ParseTreeGoal, Goal,
        UnusedSVarArgMap, !UrInfo) :-
    ( if map.is_empty(UnusedSVarArgMap) then
        true
    else
        VarSet = !.UrInfo ^ ui_varset,
        non_svar_copy_vars_in_goal(Goal, GoalVarsSet),
        set_of_var.to_sorted_list(GoalVarsSet, GoalVars),
        list.filter_map(is_prog_var_for_some_state_var(VarSet),
            GoalVars, GoalVarSVarNames),
        map.foldl(
            report_unneeded_svar_in_lambda(Context, Modes,
                ParseTreeGoal, GoalVarSVarNames),
            UnusedSVarArgMap, !UrInfo)
    ).

:- pred report_unneeded_svar_in_lambda(prog_context::in, list(mer_mode)::in,
    goal::in, list(string)::in, uint::in, statevar_arg_desc::in,
    unravel_info::in, unravel_info::out) is det.

report_unneeded_svar_in_lambda(Context, Modes, ParseTreeGoal, GoalVarSVarNames,
        ArgNum, SVarArgDesc, !UrInfo) :-
    SVarArgDesc = statevar_arg_desc(InitOrFinal, SVarName),
    % Please keep the wording of the three warnings generated here
    % in sync with the code of the following predicates in pre_typecheck.m:
    % - warn_about_any_unneeded_initial_statevars
    % - warn_about_unneeded_final_statevar
    % - warn_about_unneeded_initial_final_statevar.
    (
        ( InitOrFinal = init_arg_only,  Prefix = "!."
        ; InitOrFinal = final_arg_only, Prefix = "!:"
        ),
        Pieces = [words("Warning: the state variable")] ++
            color_as_subject([quote(Prefix ++ SVarName)]) ++ [words("is")] ++
            color_as_incorrect([words("never updated")]) ++
            [words("in this lambda expressions, so it should be"),
            words("replaced with an ordinary variable."), nl],
        Severity = severity_warning(warn_unneeded_initial_statevars_lambda),
        Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
        add_unravel_warn(Spec, !UrInfo)
    ;
        InitOrFinal = init_and_final_arg(_),
        % Please keep this wording in sync with the code of the
        % warn_about_unneeded_final_statevar predicate in pre_typecheck.m.
        InitOrFinal = init_and_final_arg(FinalArgNum),
        ( if list.member(SVarName, GoalVarSVarNames) then
            % The initial version of SVarName is used by user-written code
            % in the lambda goal, so only the final version of SVarName
            % is unneeded.
            ModuleInfo = !.UrInfo ^ ui_module_info,
            FinalArgNumI = uint.cast_to_int(FinalArgNum),
            InitArgNumI = uint.cast_to_int(ArgNum),
            list.det_index1(Modes, InitArgNumI, InitArgMode),
            list.det_index1(Modes, FinalArgNumI, FinalArgMode),
            ( if
                % See the comments in warn_about_any_unneeded_statevars
                % for the reasoning behind this test.
                %
                % Note that we cannot test the HLDS goal from which our caller
                % derived GoalVarSVarNames, because that contains the
                % unifications implicitly added by the state variable
                % transformation itself. We need the goal from *before*
                % that transformation.
                not ( ParseTreeGoal = true_expr(_) ),
                % See the comments in maybe_warn_about_unneeded_final_statevar
                % for the reasoning behind this test.
                mode_is_free_of_uniqueness(ModuleInfo, InitArgMode),
                mode_is_free_of_uniqueness(ModuleInfo, FinalArgMode)
            then
                Pieces = [words("Warning: the argument")] ++
                    color_as_subject([quote("!:" ++ SVarName)]) ++
                    [words("in this lambda expression")] ++
                    color_as_incorrect([words("could be deleted,")]) ++
                    [words("because its value"),
                    words("is always the same as its initial value."), nl],
                Severity =
                    severity_warning(warn_unneeded_final_statevars_lambda),
                Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
                add_unravel_warn(Spec, !UrInfo)
            else
                true
            )
        else
            % The initial version of SVarName is NOT used by user-written code
            % in the lambda goal, so both the initial and final versions
            % of SVarName are unneeded.
            Pieces = [words("Warning: the arguments")] ++
                color_as_subject([quote("!." ++ SVarName)]) ++
                [words("and")] ++
                color_as_subject([quote("!:" ++ SVarName)]) ++
                [words("in this lambda expression")] ++
                color_as_incorrect([words("could be deleted,")]) ++
                [words("because they are not used in the lambda goal,"),
                words("and because the final value"),
                words("is always the same as the initial value."), nl],
            Severity = severity_warning(warn_unneeded_final_statevars_lambda),
            Spec = spec($pred, Severity, phase_pt2h, Context, Pieces),
            add_unravel_warn(Spec, !UrInfo)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.unravel_info.
%---------------------------------------------------------------------------%
