%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_inst_mode_defn.m.
%
% This module parses inst and mode definitions.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_inst_mode_defn.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module term.
:- import_module varset.

    % Parse a `:- inst <InstDefn>.' declaration.
    %
:- pred parse_inst_defn_item(module_name::in, varset::in, list(term)::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

    % Parse an `:- abstract_inst <AbstractInstDefn>.' declaration.
    %
:- pred parse_abstract_inst_defn_item(module_name::in, varset::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

    % Parse a `:- mode <ModeDefn>.' declaration.
    %
:- pred parse_mode_defn(module_name::in, varset::in, term::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

    % Parse an `:- abstract_mode <AbstractModeDefn>.' declaration.
    %
:- pred parse_abstract_mode_defn_item(module_name::in, varset::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

%-----------------------------------------------------------------------------e

:- implementation.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module maybe.
:- import_module set.
:- import_module term_vars.

%-----------------------------------------------------------------------------e

parse_inst_defn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if ArgTerms = [InstDefnTerm] then
        % XXX Some of the tests here could be factored out.
        ( if
            InstDefnTerm =
                term.functor(term.atom("=="), [HeadTerm, BodyTerm], _)
        then
            parse_inst_defn_eqv(ModuleName, VarSet, HeadTerm, BodyTerm,
                Context, SeqNum, MaybeIOM)
        else if
            InstDefnTerm =
                term.functor(term.atom("--->"), [HeadTerm, BodyTerm], _)
        then
            BoundBodyTerm =
                term.functor(term.atom("bound"), [BodyTerm], Context),
            parse_inst_defn_eqv(ModuleName, VarSet, HeadTerm, BoundBodyTerm,
                Context, SeqNum, MaybeIOM)
        else
            Pieces = [words("Error: expected either"),
                quote("=="), words("or"), quote("--->"),
                words("at start of"), decl("inst"), words("definition."), nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(InstDefnTerm), Pieces),
            MaybeIOM = error1([Spec])
        )
    else
        Pieces = [words("Error: an")] ++
            color_as_subject([decl("inst"), words("declaration")]) ++
            color_as_incorrect([words("should have just one argument,")]) ++
            [words("which should be the definition of an inst."), nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

:- pred parse_inst_defn_eqv(module_name::in, varset::in, term::in, term::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

parse_inst_defn_eqv(ModuleName, VarSet, HeadTerm, BodyTerm, Context, SeqNum,
        MaybeIOM) :-
    ContextPieces = cord.singleton(words("In inst definition:")),
    ( if
        HeadTerm = term.functor(term.atom("for"),
            [NameTermPrime, ForTypeTerm], _)
    then
        NameTerm = NameTermPrime,
        ( if
            parse_sym_name_and_arity(ForTypeTerm,
                TypeSymName, TypeArity)
        then
            MaybeForType = yes(type_ctor(TypeSymName, TypeArity)),
            ForTypeSpecs = []
        else
            MaybeForType = no,
            ForTypeTermStr = describe_error_term(VarSet, ForTypeTerm),
            ForTypePieces = [words("Error: expected")] ++
                color_as_correct([words("type constructor name/arity,")]) ++
                [words("got")] ++
                color_as_incorrect([quote(ForTypeTermStr), suffix(".")]) ++
                [nl],
            ForTypeSpec = spec($pred, severity_error, phase_t2pt,
                get_term_context(ForTypeTerm), ForTypePieces),
            ForTypeSpecs = [ForTypeSpec]
        )
    else
        NameTerm = HeadTerm,
        MaybeForType = no,
        ForTypeSpecs = []
    ),
    parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
        ContextPieces, NameTerm, MaybeSymNameAndArgs),
    (
        MaybeSymNameAndArgs = error2(SymNameAndArgSpecs),
        Specs = SymNameAndArgSpecs ++ ForTypeSpecs,
        MaybeIOM = error1(Specs)
    ;
        MaybeSymNameAndArgs = ok2(SymName, ArgTerms),

        HeadTermContext = get_term_context(HeadTerm),
        check_user_inst_name(SymName, HeadTermContext, NameSpecs),
        check_inst_mode_defn_args("an", "inst definition", VarSet,
            ArgTerms, yes(BodyTerm), MaybeInstArgVars),
        NamedContextPieces = cord.from_list(
            [words("In the definition of the inst"),
            unqual_sym_name(SymName), suffix(":")]),
        parse_inst(no_allow_constrained_inst_var(wnciv_eqv_inst_defn_rhs),
            VarSet, NamedContextPieces, BodyTerm, MaybeInst),
        ( if
            NameSpecs = [],
            ForTypeSpecs = [],
            MaybeInstArgVars = ok1(InstArgVars),
            MaybeInst = ok1(Inst)
        then
            varset.coerce(VarSet, InstVarSet),
            MaybeAbstractInstDefn = nonabstract_inst_defn(eqv_inst(Inst)),
            ItemInstDefn = item_inst_defn_info(SymName, InstArgVars,
                MaybeForType, MaybeAbstractInstDefn, InstVarSet,
                Context, SeqNum),
            Item = item_inst_defn(ItemInstDefn),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = NameSpecs
                ++ ForTypeSpecs
                ++ get_any_errors1(MaybeInstArgVars)
                ++ get_any_errors1(MaybeInst),
            MaybeIOM = error1(Specs)
        )
    ).

parse_abstract_inst_defn_item(ModuleName, VarSet, HeadTerms, Context, SeqNum,
        MaybeIOM) :-
    (
        HeadTerms = [HeadTerm],
        ContextPieces = cord.singleton(words("In inst definition:")),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
            ContextPieces, HeadTerm, MaybeNameAndArgs),
        (
            MaybeNameAndArgs = error2(Specs),
            MaybeIOM = error1(Specs)
        ;
            MaybeNameAndArgs = ok2(SymName, ArgTerms),
            HeadTermContext = get_term_context(HeadTerm),
            check_user_inst_name(SymName, HeadTermContext, NameSpecs),
            check_inst_mode_defn_args("an", "abstract_inst definition", VarSet,
                ArgTerms, no, MaybeInstArgVars),
            ( if
                NameSpecs = [],
                MaybeInstArgVars = ok1(InstArgVars)
            then
                varset.coerce(VarSet, InstVarSet),
                MaybeForType = no,
                MaybeAbstractInstDefn = abstract_inst_defn,
                ItemInstDefn = item_inst_defn_info(SymName, InstArgVars,
                    MaybeForType, MaybeAbstractInstDefn, InstVarSet,
                    Context, SeqNum),
                Item = item_inst_defn(ItemInstDefn),
                MaybeIOM = ok1(iom_item(Item))
            else
                Specs = NameSpecs ++ get_any_errors1(MaybeInstArgVars),
                MaybeIOM = error1(Specs)
            )
        )
    ;
        ( HeadTerms = []
        ; HeadTerms = [_, _ | _]
        ),
        Pieces = [words("Error:")] ++
            color_as_subject([decl("abstract_inst")]) ++
            color_as_incorrect([words("should have exactly one argument.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- type processed_mode_body
    --->    processed_mode_body(
                sym_name,
                list(inst_var),
                mode_defn
            ).

parse_mode_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Context, SeqNum,
        MaybeIOM) :-
    ContextPieces = cord.singleton(words("In mode definition:")),
    parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
        ContextPieces, HeadTerm, MaybeSymNameAndArgs),
    (
        MaybeSymNameAndArgs = error2(Specs),
        MaybeIOM = error1(Specs)
    ;
        MaybeSymNameAndArgs = ok2(SymName, ArgTerms),
        HeadTermContext = get_term_context(HeadTerm),
        check_user_mode_name(SymName, HeadTermContext, NameSpecs),
        check_inst_mode_defn_args("a", "mode definition", VarSet,
            ArgTerms, yes(BodyTerm), MaybeInstArgVars),
        NamedContextPieces = cord.from_list(
            [words("In the definition of the mode"),
            unqual_sym_name(SymName), suffix(":")]),
        parse_mode(no_allow_constrained_inst_var(wnciv_mode_defn_rhs), VarSet,
            NamedContextPieces, BodyTerm, MaybeMode),
        ( if
            NameSpecs = [],
            MaybeInstArgVars = ok1(InstArgVars),
            MaybeMode = ok1(Mode)
        then
            varset.coerce(VarSet, InstVarSet),
            MaybeAbstractModeDefn = nonabstract_mode_defn(eqv_mode(Mode)),
            ItemModeDefn = item_mode_defn_info(SymName, InstArgVars,
                MaybeAbstractModeDefn, InstVarSet, Context, SeqNum),
            Item = item_mode_defn(ItemModeDefn),
            MaybeIOM = ok1(iom_item(Item))
        else
            Specs = NameSpecs ++
                get_any_errors1(MaybeInstArgVars) ++
                get_any_errors1(MaybeMode),
            MaybeIOM = error1(Specs)
        )
    ).

parse_abstract_mode_defn_item(ModuleName, VarSet, HeadTerms, Context, SeqNum,
        MaybeIOM) :-
    (
        HeadTerms = [HeadTerm],
        ContextPieces = cord.singleton(words("In abstract_mode definition:")),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
            ContextPieces, HeadTerm, MaybeSymNameAndArgs),
        (
            MaybeSymNameAndArgs = error2(Specs),
            MaybeIOM = error1(Specs)
        ;
            MaybeSymNameAndArgs = ok2(SymName, ArgTerms),
            HeadTermContext = get_term_context(HeadTerm),
            check_user_mode_name(SymName, HeadTermContext, NameSpecs),
            check_inst_mode_defn_args("an", "abstract_mode definition", VarSet,
                ArgTerms, no, MaybeInstArgVars),
            ( if
                NameSpecs = [],
                MaybeInstArgVars = ok1(InstArgVars)
            then
                varset.coerce(VarSet, InstVarSet),
                MaybeAbstractModeDefn = abstract_mode_defn,
                ItemModeDefn = item_mode_defn_info(SymName, InstArgVars,
                    MaybeAbstractModeDefn, InstVarSet, Context, SeqNum),
                Item = item_mode_defn(ItemModeDefn),
                MaybeIOM = ok1(iom_item(Item))
            else
                Specs = NameSpecs ++ get_any_errors1(MaybeInstArgVars),
                MaybeIOM = error1(Specs)
            )
        )
    ;
        ( HeadTerms = []
        ; HeadTerms = [_, _ | _]
        ),
        Pieces = [words("Error:")] ++
            color_as_subject([decl("abstract_mode")]) ++
            color_as_incorrect([words("should have exactly one argument.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%-----------------------------------------------------------------------------e

    % Check that the inst name is available to users.
    %
:- pred check_user_inst_name(sym_name::in, term.context::in,
    list(error_spec)::out) is det.

check_user_inst_name(SymName, Context, NameSpecs) :-
    Name = unqualify_name(SymName),
    ( if is_known_inst_name(Name) then
        NamePieces = [words("Error: the inst name")] ++
            color_as_subject([quote(Name)]) ++
            [words("is")] ++
            color_as_incorrect([words("reserved")]) ++
            [words("for the Mercury implementation."), nl],
        NameSpec = spec($pred, severity_error, phase_t2pt,
            Context, NamePieces),
        NameSpecs = [NameSpec]
    else
        NameSpecs = []
    ).

    % Check that the mode name is available to users.
    %
:- pred check_user_mode_name(sym_name::in, term.context::in,
    list(error_spec)::out) is det.

check_user_mode_name(SymName, Context, NameSpecs) :-
    % Check that the mode name is available to users.
    Name = unqualify_name(SymName),
    ( if is_known_mode_name(Name) then
        NamePieces = [words("Error: the mode name")] ++
            color_as_subject([quote(Name)]) ++
            [words("is")] ++
            color_as_incorrect([words("reserved")]) ++
            [words("for the Mercury implementation."), nl],
        NameSpec = spec($pred, severity_error, phase_t2pt,
            Context, NamePieces),
        NameSpecs = [NameSpec]
    else
        NameSpecs = []
    ).

:- pred check_inst_mode_defn_args(string::in, string::in, varset::in,
    list(term)::in, maybe(term)::in, maybe1(list(inst_var))::out) is det.

check_inst_mode_defn_args(AAn, DefnKind, VarSet, ArgTerms, MaybeBodyTerm,
        MaybeArgVars) :-
    % Check that all the head arguments are variables.
    terms_to_distinct_vars(VarSet, AAn, DefnKind, ArgTerms, MaybeArgVars0),
    (
        MaybeArgVars0 = ok1(ArgVars),
        some [!Specs] (
            !:Specs = [],
            % Check that all the variables in the body occur in the head.
            % The common case is BodyVars = []; fail fast in that case.
            ( if
                MaybeBodyTerm = yes(BodyTerm),
                term_vars.vars_in_term(BodyTerm, BodyVars),
                BodyVars = [_ | _],
                set.list_to_set(BodyVars, BodyVarsSet),
                set.list_to_set(ArgVars, ArgVarsSet),
                set.difference(BodyVarsSet, ArgVarsSet, FreeVarsSet),
                set.to_sorted_list(FreeVarsSet, FreeVars),
                FreeVars = [_ | _]
            then
                FreeVarPieces =
                    list.map(var_to_quote_piece(VarSet), FreeVars),
                FreePieces = [words("Error:"),
                    words(choose_number(FreeVars,
                        "a free inst parameter", "free inst parameters")),
                    words("such as")] ++
                    component_list_to_color_pieces(yes(color_subject),
                        "and", [], FreeVarPieces) ++
                    color_as_incorrect([words("may not occur")]) ++
                    [words("on the right hand side of"),
                    words(DefnKind), suffix("."), nl],
                FreeSpec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(BodyTerm), FreePieces),
                !:Specs = [FreeSpec | !.Specs]
            else
                true
            ),
            (
                !.Specs = [],
                list.map(term.coerce_var, ArgVars, InstArgVars),
                MaybeArgVars = ok1(InstArgVars)
            ;
                !.Specs = [_ | _],
                MaybeArgVars = error1(!.Specs)
            )
        )
    ;
        MaybeArgVars0 = error1(Specs),
        MaybeArgVars = error1(Specs)
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.parse_inst_mode_defn.
%-----------------------------------------------------------------------------e
