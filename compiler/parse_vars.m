%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines predicates that parse lists of variables.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_vars.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.

:- import_module cord.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % parse_possibly_repeated_vars(Term, VarSet, ContextPieces, MaybeVars):
    %
    % Parse Term as a list of quantified variables Vars. If successful,
    % return ok1(Vars). If not, return an error message that has
    % ContextPieces as a prefix.
    %
:- pred parse_possibly_repeated_vars(term(T)::in, varset(T)::in,
    cord(format_component)::in, maybe1(list(var(T)))::out) is det.

    % parse_vars(Term, VarSet, ContextPieces, MaybeVars):
    %
    % The same as parse_possibly_repeated_vars, but generate an error
    % if a variable is in the list more than once.
    %
:- pred parse_vars(term(T)::in, varset(T)::in,
    cord(format_component)::in, maybe1(list(var(T)))::out) is det.

:- type plain_state_vars(T)
    --->    plain_state_vars(
                list(var(T)),   % plain variables
                list(var(T))    % !V state variables
            ).

    % parse_vars_state_vars(Term, VarSet, ContextPieces, MaybeVars):
    %
    % The same as parse_vars, but parse not just ordinary variables V,
    % but also state variables !SV.
    %
:- pred parse_vars_state_vars(term(T)::in, varset(T)::in,
    cord(format_component)::in, maybe1(plain_state_vars(T))::out) is det.

:- type plain_state_dot_colon_vars(T)
    --->    plain_state_dot_colon_vars(
                list(var(T)),   % plain variables
                list(var(T)),   % !V state variables
                list(var(T)),   % !.V state variables
                list(var(T))    % !:V state variables
            ).

    % parse_vars_state_dot_colon_vars(Term, VarSet, ContextPieces, MaybeVars):
    %
    % The same as parse_vars, but parse not just ordinary variables V,
    % but also state variables !SV, !.SV and !:SV.
    %
:- pred parse_vars_state_dot_colon_vars(term(T)::in, varset(T)::in,
    cord(format_component)::in, maybe1(plain_state_dot_colon_vars(T))::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_term.

%---------------------------------------------------------------------------%

parse_possibly_repeated_vars(Term, VarSet, ContextPieces, MaybeVars) :-
    ( if Term = term.functor(term.atom("[]"), [], _) then
        MaybeVars = ok1([])
    else if Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _) then
        (
            HeadTerm = term.variable(HeadVar0, _),
            MaybeHeadVar = ok1(HeadVar0)
        ;
            HeadTerm = term.functor(_, _, _),
            generate_unexpected_term_message(ContextPieces, VarSet,
                "a variable", HeadTerm, Spec),
            MaybeHeadVar = error1([Spec])
        ),
        parse_possibly_repeated_vars(TailTerm, VarSet, ContextPieces,
            MaybeTailVars),
        ( if
            MaybeHeadVar = ok1(HeadVar),
            MaybeTailVars = ok1(TailVars)
        then
            MaybeVars = ok1([HeadVar | TailVars])
        else
            Specs = get_any_errors1(MaybeHeadVar) ++
                get_any_errors1(MaybeTailVars),
            MaybeVars = error1(Specs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "a list of variables", Term, Spec),
        MaybeVars = error1([Spec])
    ).

parse_vars(Term, VarSet, ContextPieces, MaybeVars) :-
    ( if Term = functor(atom("[]"), [], _) then
        MaybeVars = ok1([])
    else if Term = functor(atom("[|]"), [HeadTerm, TailTerm], _) then
        (
            HeadTerm = variable(HeadVar0, _),
            MaybeHeadVar = ok1(HeadVar0)
        ;
            HeadTerm = functor(_, _, _),
            generate_unexpected_term_message(ContextPieces, VarSet,
                "a variable", HeadTerm, HeadSpec),
            MaybeHeadVar = error1([HeadSpec])
        ),
        parse_vars(TailTerm, VarSet, ContextPieces, MaybeTailVars),
        ( if
            MaybeHeadVar = ok1(HeadVar),
            MaybeTailVars = ok1(TailVars)
        then
            ( if list.member(HeadVar, TailVars) then
                generate_repeated_var_msg(ContextPieces, VarSet,
                    HeadTerm, Spec),
                MaybeVars = error1([Spec])
            else
                Vars = [HeadVar | TailVars],
                MaybeVars = ok1(Vars)
            )
        else
            Specs = get_any_errors1(MaybeHeadVar) ++
                get_any_errors1(MaybeTailVars),
            MaybeVars = error1(Specs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "a list of variables", Term, Spec),
        MaybeVars = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- type ordinary_state_var(T)
    --->    os_ordinary_var(var(T))
    ;       os_state_var(var(T)).

parse_vars_state_vars(Term, VarSet, ContextPieces, MaybeVars) :-
    ( if Term = functor(atom("[]"), [], _) then
        MaybeVars = ok1(plain_state_vars([], []))
    else if Term = functor(atom("[|]"), [HeadTerm, TailTerm], _) then
        ( if
            (
                HeadTerm = variable(V0, _),
                VarKind0 = os_ordinary_var(V0)
            ;
                HeadTerm = functor(atom("!"), [variable(SV0, _)], _),
                VarKind0 = os_state_var(SV0)
            )
        then
            MaybeHeadVar = ok1(VarKind0)
        else
            generate_unexpected_term_message(ContextPieces, VarSet,
                "a variable or state variable", HeadTerm, HeadSpec),
            MaybeHeadVar = error1([HeadSpec])
        ),
        parse_vars_state_vars(TailTerm, VarSet, ContextPieces, MaybeTailVars),
        ( if
            MaybeHeadVar = ok1(VarKind),
            MaybeTailVars = ok1(plain_state_vars(TailVars, TailStateVars))
        then
            (
                VarKind = os_ordinary_var(V),
                ( if list.member(V, TailVars) then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error1([Spec])
                else
                    Vars = [V | TailVars],
                    MaybeVars = ok1(plain_state_vars(Vars, TailStateVars))
                )
            ;
                VarKind = os_state_var(SV),
                ( if list.member(SV, TailStateVars) then
                    generate_repeated_state_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error1([Spec])
                else
                    StateVars = [SV | TailStateVars],
                    MaybeVars = ok1(plain_state_vars(TailVars, StateVars))
                )
            )
        else
            Specs = get_any_errors1(MaybeHeadVar) ++
                get_any_errors1(MaybeTailVars),
            MaybeVars = error1(Specs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "a list of variables and/or state variables", Term, Spec),
        MaybeVars = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- type ordinary_state_dot_colon_var(T)
    --->    osdc_ordinary_var(var(T))
    ;       osdc_state_var(var(T))
    ;       osdc_dot_var(var(T))
    ;       osdc_colon_var(var(T)).

parse_vars_state_dot_colon_vars(Term, VarSet, ContextPieces, MaybeVars) :-
    ( if Term = functor(atom("[]"), [], _) then
        MaybeVars = ok1(plain_state_dot_colon_vars([], [], [], []))
    else if Term = functor(atom("[|]"), [HeadTerm, Tail], _) then
        ( if
            (
                HeadTerm = variable(V0, _),
                VarKind0 = osdc_ordinary_var(V0)
            ;
                HeadTerm = functor(atom("!"), [variable(SV0, _)], _),
                VarKind0 = osdc_state_var(SV0)
            ;
                HeadTerm = functor(atom("!."), [variable(SV0, _)], _),
                VarKind0 = osdc_dot_var(SV0)
            ;
                HeadTerm = functor(atom("!:"), [variable(SV0, _)], _),
                VarKind0 = osdc_colon_var(SV0)
            )
        then
            MaybeHeadVar = ok1(VarKind0)
        else
            generate_unexpected_term_message(ContextPieces, VarSet,
                "a variable or state variable", HeadTerm, HeadSpec),
            MaybeHeadVar = error1([HeadSpec])
        ),
        parse_vars_state_dot_colon_vars(Tail, VarSet, ContextPieces,
            MaybeTailVars),
        ( if
            MaybeHeadVar = ok1(VarKind),
            MaybeTailVars = ok1(plain_state_dot_colon_vars(TailVars,
                TailStateVars, TailDotVars, TailColonVars))
        then
            (
                VarKind = osdc_ordinary_var(V),
                ( if list.member(V, TailVars) then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error1([Spec])
                else
                    Vars = [V | TailVars],
                    MaybeVars = ok1(plain_state_dot_colon_vars(Vars,
                        TailStateVars, TailDotVars, TailColonVars))
                )
            ;
                VarKind = osdc_state_var(SV),
                ( if
                    ( list.member(SV, TailStateVars )
                    ; list.member(SV, TailDotVars )
                    ; list.member(SV, TailColonVars )
                    )
                then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error1([Spec])
                else
                    StateVars = [SV | TailStateVars],
                    MaybeVars = ok1(plain_state_dot_colon_vars(TailVars,
                        StateVars, TailDotVars, TailColonVars))
                )
            ;
                VarKind = osdc_dot_var(SV),
                ( if
                    ( list.member(SV, TailStateVars )
                    ; list.member(SV, TailDotVars )
                    ; list.member(SV, TailColonVars )
                    )
                then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error1([Spec])
                else
                    DotVars = [SV | TailDotVars],
                    MaybeVars = ok1(plain_state_dot_colon_vars(TailVars,
                        TailStateVars, DotVars, TailColonVars))
                )
            ;
                VarKind = osdc_colon_var(SV),
                ( if
                    ( list.member(SV, TailStateVars )
                    ; list.member(SV, TailDotVars )
                    ; list.member(SV, TailColonVars )
                    )
                then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error1([Spec])
                else
                    ColonVars = [SV | TailColonVars],
                    MaybeVars = ok1(plain_state_dot_colon_vars(TailVars,
                        TailStateVars, TailDotVars, ColonVars))
                )
            )
        else
            HeadSpecs = get_any_errors1(MaybeHeadVar),
            TailSpecs = get_any_errors1(MaybeTailVars),
            MaybeVars = error1(HeadSpecs ++ TailSpecs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "a list of variables and/or state variables", Term, Spec),
        MaybeVars = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred generate_repeated_var_msg(cord(format_component)::in,
    varset(T)::in, term(T)::in, error_spec::out) is det.

generate_repeated_var_msg(ContextPieces, VarSet, Term, Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
        words("Repeated variable"), words(TermStr), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces).

:- pred generate_repeated_state_var_msg(cord(format_component)::in,
    varset(T)::in, term(T)::in, error_spec::out) is det.

generate_repeated_state_var_msg(ContextPieces, VarSet, Term, Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
        words("Repeated state variable"), words(TermStr), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces).

:- pred generate_unexpected_term_message(cord(format_component)::in,
    varset(T)::in, string::in, term(T)::in, error_spec::out) is det.

generate_unexpected_term_message(ContextPieces, VarSet, Expected, Term,
        Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
        words("Expected"), words(Expected), suffix(","),
        words("got"), quote(TermStr), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_vars.
%---------------------------------------------------------------------------%
