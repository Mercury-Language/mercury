%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_type_defn.m.
%
% This module parses inst and mode definitions.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_mode_defn.

:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module term.
:- import_module varset.

    % Parse a `:- inst <InstDefn>.' declaration.
    %
:- pred parse_inst_defn(module_name::in, varset::in, term::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

    % Parse a `:- mode <ModeDefn>.' declaration.
    %
:- pred parse_mode_defn(module_name::in, varset::in, term::in, term::in,
    condition::in, prog_context::in, int::in, maybe1(item)::out) is det.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_sym_name.

:- import_module list.

parse_inst_defn(ModuleName, VarSet, Term, Context, SeqNum, MaybeItem) :-
    % XXX Some of the tests here could be factored out.
    (
        Term = term.functor(term.atom("=="), [HeadTerm, BodyTerm], _)
    ->
        parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
        parse_inst_defn_base(ModuleName, VarSet, HeadTerm, BeforeCondTerm,
            Condition, Context, SeqNum, MaybeItem)
    ;
        % XXX This is for `abstract inst' declarations,
        % which are not really supported.
        Term = term.functor(term.atom("is"), Args, _),
        Args = [HeadTerm, term.functor(term.atom("private"), [], _)]
    ->
        Condition = cond_true,
        parse_abstract_inst_defn(ModuleName, VarSet, HeadTerm,
            Condition, Context, SeqNum, MaybeItem)
    ;
        Term = term.functor(term.atom("--->"), [HeadTerm, BodyTerm], _)
    ->
        parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
        BoundBeforeCondTerm =
            term.functor(term.atom("bound"), [BeforeCondTerm], Context),
        parse_inst_defn_base(ModuleName, VarSet, HeadTerm, BoundBeforeCondTerm,
            Condition, Context, SeqNum, MaybeItem)
    ;
        Pieces = [words("Error:"), quote("=="), words("expected in"),
            decl("inst"), words("definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_inst_defn_base(module_name::in, varset::in, term::in, term::in,
    condition::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_inst_defn_base(ModuleName, VarSet, HeadTerm, BodyTerm, Condition,
        Context, SeqNum, MaybeItem) :-
    ContextPieces = [words("In inst definition:")],
    parse_implicitly_qualified_sym_name_and_args(ModuleName, HeadTerm,
        VarSet, ContextPieces, MaybeNameAndArgs),
    (
        MaybeNameAndArgs = error2(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeNameAndArgs = ok2(Name, ArgTerms),
        (
            % Check that all the head args are variables.
            term.term_list_to_var_list(ArgTerms, Args)
        ->
            (
                % Check that all the head arg variables are distinct.
                list.member(Arg2, Args, [Arg2 | OtherArgs]),
                list.member(Arg2, OtherArgs)
            ->
                % XXX Should improve the error message here.
                Pieces = [words("Error: repeated inst parameters"),
                    words("in LHS of inst definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                % Check that all the variables in the body occur in the head.
                term.contains_var(BodyTerm, Var2),
                \+ list.member(Var2, Args)
            ->
                Pieces = [words("Error: free inst parameter"),
                    words("in RHS of inst definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(BodyTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                % Check that the inst is a valid user-defined inst, i.e.
                % that it does not have the form of one of the builtin insts.
                \+ (
                    convert_inst(no_allow_constrained_inst_var, HeadTerm,
                        UserInst),
                    UserInst = defined_inst(user_inst(_, _))
                )
            ->
                % XXX Name the builtin inst.
                Pieces =
                    [words("Error: attempt to redefine builtin inst."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                % Should improve the error message here.
                (
                    convert_inst(no_allow_constrained_inst_var, BodyTerm, Inst)
                ->
                    varset.coerce(VarSet, InstVarSet),
                    list.map(term.coerce_var, Args, InstArgs),
                    InstDefn = eqv_inst(Inst),
                    ItemInstDefn = item_inst_defn_info(InstVarSet, Name,
                        InstArgs, InstDefn, Condition, Context, SeqNum),
                    Item = item_inst_defn(ItemInstDefn),
                    MaybeItem = ok1(Item)
                ;
                    BodyTermStr = describe_error_term(VarSet, BodyTerm),
                    Pieces = [words("Error: syntax error in inst body at"),
                        words(BodyTermStr), suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(BodyTerm),
                            [always(Pieces)])]),
                    MaybeItem = error1([Spec])
                )
            )
        ;
            % XXX If term_list_to_var_list returned the non-var's term
            % or context, we could use it here.
            Pieces = [words("Error: inst parameters must be variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

:- pred parse_abstract_inst_defn(module_name::in, varset::in, term::in,
    condition::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_abstract_inst_defn(ModuleName, VarSet, HeadTerm, Condition, Context,
        SeqNum, MaybeItem) :-
    ContextPieces = [words("In inst definition:")],
    parse_implicitly_qualified_sym_name_and_args(ModuleName, HeadTerm,
        VarSet, ContextPieces, MaybeNameAndArgs),
    (
        MaybeNameAndArgs = error2(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeNameAndArgs = ok2(Name, ArgTerms),
        (
            % Check that all the head args are variables.
            term.term_list_to_var_list(ArgTerms, Args)
        ->
            (
                % Check that all the head arg variables are distinct.
                list.member(Arg2, Args, [Arg2 | OtherArgs]),
                list.member(Arg2, OtherArgs)
            ->
                % XXX We should we list the repeated parameters.
                Pieces = [words("Error: repeated inst parameters"),
                    words("in abstract inst definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                varset.coerce(VarSet, InstVarSet),
                list.map(term.coerce_var, Args, InstArgs),
                InstDefn = abstract_inst,
                ItemInstDefn = item_inst_defn_info(InstVarSet, Name,
                    InstArgs, InstDefn, Condition, Context, SeqNum),
                Item = item_inst_defn(ItemInstDefn),
                MaybeItem = ok1(Item)
            )
        ;
            % XXX If term_list_to_var_list returned the non-var's term
            % or context, we could use it here.
            Pieces = [words("Error: inst parameters must be variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------%

:- type processed_mode_body
    --->    processed_mode_body(
                sym_name,
                list(inst_var),
                mode_defn
            ).

parse_mode_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Condition, Context,
        SeqNum, MaybeItem) :-
    ContextPieces = [words("In mode definition:")],
    parse_implicitly_qualified_sym_name_and_args(ModuleName, HeadTerm,
        VarSet, ContextPieces, MaybeModeNameAndArgs),
    (
        MaybeModeNameAndArgs = error2(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeModeNameAndArgs = ok2(Name, ArgTerms),
        % Check that all the head args are variables.
        ( term.term_list_to_var_list(ArgTerms, Args) ->
            (
                % Check that all the head arg variables are distinct.
                list.member(Arg2, Args, [Arg2 | OtherArgs]),
                list.member(Arg2, OtherArgs)
            ->
                % Check that all the head arg variables are distinct.
                % XXX We should list the duplicated head arg variables.
                Pieces = [words("Error: repeated parameters"),
                    words("in LHS of mode definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                % Check that all the variables in the body occur in the head.
                term.contains_var(BodyTerm, Var2),
                \+ list.member(Var2, Args)
            ->
                % XXX Shouldn't we be using the BodyTerm's context?
                % XXX We should list the Var2s for which the condition holds.
                Pieces = [words("Error: free inst parameter"),
                    words("in RHS of mode definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                (
                    convert_mode(no_allow_constrained_inst_var, BodyTerm, Mode)
                ->
                    varset.coerce(VarSet, InstVarSet),
                    list.map(term.coerce_var, Args, ModeArgs),
                    ModeDefn = eqv_mode(Mode),
                    ItemModeDefn = item_mode_defn_info(InstVarSet, Name,
                        ModeArgs, ModeDefn, Condition, Context, SeqNum),
                    Item = item_mode_defn(ItemModeDefn),
                    MaybeItem = ok1(Item)
                ;
                    % XXX We should improve the error message here.
                    Pieces = [words("Error: syntax error"),
                        words("in mode definition body."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(BodyTerm),
                            [always(Pieces)])]),
                    MaybeItem = error1([Spec])
                )
            )
        ;
            % XXX If term_list_to_var_list returned the non-var's term
            % or context, we could use it here.
            Pieces = [words("Error: mode parameters must be variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.prog_io_mode_defn.
%-----------------------------------------------------------------------------e
