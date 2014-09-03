%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module handles the top level parsing of items.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_item.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module term.
:- import_module varset.

    % parse_item(ModuleName, VarSet, Term, SeqNum, MaybeItem):
    %
    % Parse Term. If successful, bind MaybeItem to the parsed item,
    % otherwise bind it to an appropriate error message. Qualify
    % appropriate parts of the item, with ModuleName as the module name.
    % Use SeqNum as the item's sequence number.
    %
:- pred parse_item(module_name::in, varset::in, term::in, int::in,
    maybe1(item)::out) is det.

    % parse_decl(ModuleName, VarSet, Term, SeqNum, MaybeItem):
    %
    % Parse Term as a declaration. If successful, bind MaybeItem to the
    % parsed item, otherwise it is bound to an appropriate error message.
    % Qualify appropriate parts of the item, with ModuleName as the module
    % name. Use SeqNum as the item's sequence number.
    %
:- pred parse_decl(module_name::in, varset::in, term::in, int::in,
    maybe1(item)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io_dcg.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_io_mode_defn.
:- import_module parse_tree.prog_io_mutable.
:- import_module parse_tree.prog_io_pragma.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_type_defn.
:- import_module parse_tree.prog_io_typeclass.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

parse_item(ModuleName, VarSet, Term, SeqNum, Result) :-
    (
        Term = term.functor(term.atom(":-"), [DeclTerm], _DeclContext)
    ->
        % Term is a declaration.
        parse_decl(ModuleName, VarSet, DeclTerm, SeqNum, Result)
    ;
        Term = term.functor(term.atom("-->"), [DCG_H_Term, DCG_B_Term],
            DCG_Context)
    ->
        % Term is a DCG clause.
        parse_dcg_clause(ModuleName, VarSet, DCG_H_Term, DCG_B_Term,
            DCG_Context, SeqNum, Result)
    ;
        % Term is a clause; either a fact or a rule.
        (
            Term = term.functor(term.atom(":-"),
                [HeadTermPrime, BodyTermPrime], TermContext)
        ->
            % Term is a rule.
            HeadTerm = HeadTermPrime,
            BodyTerm = BodyTermPrime,
            ClauseContext = TermContext
        ;
            % Term is a fact.
            HeadTerm = Term,
            ClauseContext = get_term_context(HeadTerm),
            BodyTerm = term.functor(term.atom("true"), [], ClauseContext)
        ),
        varset.coerce(VarSet, ProgVarSet),
        parse_clause(ModuleName, HeadTerm, BodyTerm, ProgVarSet,
            ClauseContext, SeqNum, Result)
    ).

:- pred parse_clause(module_name::in, term::in, term::in,
    prog_varset::in, term.context::in, int::in, maybe1(item)::out) is det.

parse_clause(ModuleName, HeadTerm, BodyTerm0, ProgVarSet0, Context,
        SeqNum, MaybeItem) :-
    GoalContextPieces = [],
    parse_goal(BodyTerm0, GoalContextPieces, MaybeBodyGoal,
        ProgVarSet0, ProgVarSet),
    (
        MaybeBodyGoal = ok1(BodyGoal),
        varset.coerce(ProgVarSet, VarSet),
        (
            HeadTerm = term.functor(term.atom("="),
                [FuncHeadTerm0, FuncResultTerm], _),
            FuncHeadTerm = desugar_field_access(FuncHeadTerm0)
        ->
            HeadContextPieces = [words("In equation head:")],
            parse_implicitly_qualified_sym_name_and_args(ModuleName,
                FuncHeadTerm, VarSet, HeadContextPieces, MaybeFunctor),
            (
                MaybeFunctor = ok2(Name, ArgTerms0),
                list.map(term.coerce, ArgTerms0 ++ [FuncResultTerm],
                    ProgArgTerms),
                ItemClause = item_clause_info(user, ProgVarSet, pf_function,
                    Name, ProgArgTerms, BodyGoal, Context, SeqNum),
                Item = item_clause(ItemClause),
                MaybeItem = ok1(Item)
            ;
                MaybeFunctor = error2(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            HeadContextPieces = [words("In clause head:")],
            parse_implicitly_qualified_sym_name_and_args(ModuleName, HeadTerm,
                VarSet, HeadContextPieces, MaybeFunctor),
            (
                MaybeFunctor = ok2(Name, ArgTerms),
                list.map(term.coerce, ArgTerms, ProgArgTerms),
                ItemClause = item_clause_info(user, ProgVarSet, pf_predicate,
                    Name, ProgArgTerms, BodyGoal, Context, SeqNum),
                Item = item_clause(ItemClause),
                MaybeItem = ok1(Item)
            ;
                MaybeFunctor = error2(Specs),
                MaybeItem = error1(Specs)
            )
        )
    ;
        MaybeBodyGoal = error1(Specs),
        MaybeItem = error1(Specs)
    ).

%-----------------------------------------------------------------------------%

parse_decl(ModuleName, VarSet, Term, SeqNum, MaybeItem) :-
    parse_attrs_and_decl(ModuleName, VarSet, Term, [], SeqNum, MaybeItem).

    % parse_attrs_and_decl(ModuleName, VarSet, Term, Attributes, SeqNum,
    %   MaybeItem):
    %
    % Succeeds if Term is a declaration and binds Result to a representation
    % of that declaration. Attributes is a list of enclosing declaration
    % attributes, in the order innermost to outermost.
    %
:- pred parse_attrs_and_decl(module_name::in, varset::in, term::in,
    decl_attrs::in, int::in, maybe1(item)::out) is det.

parse_attrs_and_decl(ModuleName, VarSet, Term, !.Attributes, SeqNum,
        MaybeItem) :-
    ( Term = term.functor(term.atom(Functor), Args, Context) ->
        (
            parse_decl_attribute(Functor, Args, Attribute, SubTerm)
        ->
            !:Attributes = [Attribute - Context | !.Attributes],
            parse_attrs_and_decl(ModuleName, VarSet, SubTerm, !.Attributes,
                SeqNum, MaybeItem)
        ;
            parse_attributed_decl(ModuleName, VarSet, Functor, Args,
                !.Attributes, Context, SeqNum, MaybeItemPrime)
        ->
            MaybeItemPrime = MaybeItem
        ;
            TermStr = mercury_term_to_string(VarSet, no, Term),
            Pieces = [words("Error: unrecognized declaration:"), nl,
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        Context = get_term_context(Term),
        Pieces = [words("Error: atom expected after"), quote(":-"),
            suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

    % parse_attributed_decl(ModuleName, VarSet, Functor, Args, Attributes,
    %   Context, SeqNum, MaybeItem):
    %
    % If Atom(Args) is a declaration, succeed and bind MaybeItem to a
    % representation of that declaration. Attributes is a list of
    % enclosing declaration attributes, in the order outermost to innermost.
    %
:- pred parse_attributed_decl(module_name::in, varset::in, string::in,
    list(term)::in, decl_attrs::in, prog_context::in, int::in,
    maybe1(item)::out) is semidet.

parse_attributed_decl(ModuleName, VarSet, Functor, ArgTerms, Attributes,
        Context, SeqNum, MaybeItem) :-
    (
        Functor = "type",
        ArgTerms = [TypeDefnTerm],
        parse_type_defn(ModuleName, VarSet, TypeDefnTerm, Attributes, Context,
            SeqNum, MaybeItem)
    ;
        Functor = "inst",
        ArgTerms = [InstDeclTerm],
        parse_inst_defn(ModuleName, VarSet, InstDeclTerm, Context,
            SeqNum, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "mode",
        ArgTerms = [SubTerm],
        ( SubTerm = term.functor(term.atom("=="), [HeadTerm, BodyTerm], _) ->
            % This is the definition of a mode.
            parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
            parse_mode_defn(ModuleName, VarSet, HeadTerm, BeforeCondTerm,
                Condition, Context, SeqNum, MaybeItem)
        ;
            % This is the declaration of one mode of a predicate or function.
            parse_mode_decl(ModuleName, VarSet, SubTerm, Attributes,
                Context, SeqNum, MaybeItem)
        )
    ;
        (
            Functor = "pred",
            PredOrFunc = pf_predicate
        ;
            Functor = "func",
            PredOrFunc = pf_function
        ),
        ArgTerms = [DeclTerm],
        parse_pred_or_func_decl(PredOrFunc, ModuleName, VarSet, DeclTerm,
            Attributes, Context, SeqNum, MaybeItem)
    ;
        (
            Functor = "import_module",
            Maker = make_import
        ;
            Functor = "use_module",
            Maker = make_use
        ;
            Functor = "export_module",
            Maker = make_export
        ),
        ArgTerms = [ModuleSpecTerm],
        parse_symlist_decl(parse_module_specifier(VarSet), Maker,
            ModuleSpecTerm, Attributes, Context, SeqNum, MaybeItem)
    ;
        (
            Functor = "interface",
            ModuleDefn = md_interface
        ;
            Functor = "implementation",
            ModuleDefn = md_implementation
        ),
        ArgTerms = [],
        ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
        Item = item_module_defn(ItemModuleDefn),
        MaybeItem0 = ok1(Item),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "external",
        (
            ArgTerms = [PredSpecTerm],
            MaybeBackEnd = no
        ;
            ArgTerms = [BackEndArgTerm, PredSpecTerm],
            BackEndArgTerm = term.functor(term.atom(BackEndFunctor), [], _),
            (
                BackEndFunctor = "high_level_backend",
                BackEnd = high_level_backend
            ;
                BackEndFunctor = "low_level_backend",
                BackEnd = low_level_backend
            ),
            MaybeBackEnd = yes(BackEnd)
        ),
        parse_implicitly_qualified_symbol_name_specifier(ModuleName, VarSet,
            PredSpecTerm, MaybeSymSpec),
        process_maybe1(make_external(MaybeBackEnd, Context, SeqNum),
            MaybeSymSpec, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "module",
        ArgTerms = [ModuleNameTerm],
        parse_module_name(ModuleName, VarSet, ModuleNameTerm,
            MaybeModuleNameSym),
        (
            MaybeModuleNameSym = ok1(ModuleNameSym),
            ItemModuleStart =
                item_module_start_info(ModuleNameSym, Context, SeqNum),
            Item = item_module_start(ItemModuleStart),
            MaybeItem0 = ok1(Item)
        ;
            MaybeModuleNameSym = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "end_module",
        ArgTerms = [ModuleNameTerm],
        % The name in an `end_module' declaration is NOT inside the scope
        % of the module being ended, so the default module name here
        % (ModuleName) is the PARENT of the previous default module name.

        sym_name_get_module_name_default(ModuleName, root_module_name,
            ParentOfModuleName),
        parse_module_name(ParentOfModuleName, VarSet, ModuleNameTerm,
            MaybeModuleNameSym),
        (
            MaybeModuleNameSym = ok1(ModuleNameSym),
            ItemModuleEnd =
                item_module_end_info(ModuleNameSym, Context, SeqNum),
            Item = item_module_end(ItemModuleEnd),
            MaybeItem0 = ok1(Item)
        ;
            MaybeModuleNameSym = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "include_module",
        ArgTerms = [ModuleNamesTerm],
        parse_list(parse_module_name(ModuleName, VarSet), ModuleNamesTerm,
            MaybeModuleNameSyms),
        (
            MaybeModuleNameSyms = ok1(ModuleNameSyms),
            ModuleDefn = md_include_module(ModuleNameSyms),
            ItemModuleDefn =
                item_module_defn_info(ModuleDefn, Context, SeqNum),
            Item = item_module_defn(ItemModuleDefn),
            MaybeItem0 = ok1(Item)
        ;
            MaybeModuleNameSyms = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "pragma",
        parse_pragma(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        (
            Functor = "promise",
            PromiseType = promise_type_true
        ;
            Functor = "promise_exclusive",
            PromiseType = promise_type_exclusive
        ;
            Functor = "promise_exhaustive",
            PromiseType = promise_type_exhaustive
        ;
            Functor = "promise_exclusive_exhaustive",
            PromiseType = promise_type_exclusive_exhaustive
        ),
        parse_promise(ModuleName, PromiseType, VarSet, ArgTerms, Attributes,
            Context, SeqNum, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "typeclass",
        parse_typeclass(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeItemTypeClass),
        (
            MaybeItemTypeClass = ok1(ItemTypeClass),
            MaybeItem0 = ok1(item_typeclass(ItemTypeClass))
        ;
            MaybeItemTypeClass = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "instance",
        parse_instance(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeItemInstance),
        (
            MaybeItemInstance = ok1(ItemInstance),
            MaybeItem0 = ok1(item_instance(ItemInstance))
        ;
            MaybeItemInstance = error1(Specs),
            MaybeItem0 = error1(Specs)
        ),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        ( Functor = "initialise"
        ; Functor = "initialize"
        ),
        ArgTerms = [SubTerm],
        parse_initialise_decl(ModuleName, VarSet, SubTerm, Context,
            SeqNum, MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        ( Functor = "finalise"
        ; Functor = "finalize"
        ),
        ArgTerms = [SubTerm],
        parse_finalise_decl(ModuleName, VarSet, SubTerm, Context, SeqNum,
            MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "mutable",
        parse_mutable_decl(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            MaybeItem0),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ;
        Functor = "version_numbers",
        process_version_numbers(ModuleName, VarSet, ArgTerms, Attributes,
            Context, SeqNum, MaybeItem)
    ).

:- pred parse_symlist_decl(parser(module_specifier)::parser,
    maker(list(module_specifier), module_defn)::maker, term::in,
    decl_attrs::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_symlist_decl(ParserPred, MakeModuleDefnPred, Term, Attributes, Context,
        SeqNum, MaybeItem) :-
    parse_list(ParserPred, Term, MaybeModuleSpecs),
    process_maybe1(make_module_defn(MakeModuleDefnPred, Context, SeqNum),
        MaybeModuleSpecs, MaybeItem0),
    check_no_attributes(MaybeItem0, Attributes, MaybeItem).

:- pred process_version_numbers(module_name::in, varset::in, list(term)::in,
    decl_attrs::in, prog_context::in, int::in, maybe1(item)::out) is semidet.

process_version_numbers(ModuleName, _VarSet, ArgTerms, Attributes, Context,
        SeqNum, MaybeItem) :-
    ArgTerms = [VersionNumberTerm, ModuleNameTerm, VersionNumbersTerm],
    (
        VersionNumberTerm = term.functor(term.integer(VersionNumber), [], _),
        VersionNumber = version_numbers_version_number
    ->
        ( try_parse_symbol_name(ModuleNameTerm, ModuleName) ->
            recompilation.version.parse_version_numbers(VersionNumbersTerm,
                MaybeItem0),
            (
                MaybeItem0 = ok1(VersionNumbers),
                ModuleDefn = md_version_numbers(ModuleName, VersionNumbers),
                ItemModuleDefn = item_module_defn_info(ModuleDefn, Context,
                    SeqNum),
                Item = item_module_defn(ItemModuleDefn),
                MaybeItem1 = ok1(Item),
                check_no_attributes(MaybeItem1, Attributes, MaybeItem)
            ;
                MaybeItem0 = error1(Specs),
                MaybeItem = error1(Specs)
            )
        ;
            Pieces = [words("Error: invalid module name in"),
                quote(":- version_numbers"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ModuleNameTerm),
                    [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ;
        (
            VersionNumberTerm = term.functor(_, _, _VersionNumberContext),
            Msg = "interface file needs to be recreated, " ++
                "the version numbers are out of date",
            dummy_term_with_context(Context, DummyTerm),
            Warning = item_warning(yes(warn_smart_recompilation),
                Msg, DummyTerm),
            ItemNothing = item_nothing_info(yes(Warning), Context, SeqNum),
            Item = item_nothing(ItemNothing),
            MaybeItem = ok1(Item)
        ;
            VersionNumberTerm = term.variable(_, VersionNumberContext),
            Pieces = [words("Error: invalid version number in"),
                quote(":- version_numbers"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(VersionNumberContext, [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

%-----------------------------------------------------------------------------%
%
% Parsing ":- pred" and ":- func" declarations.
%

    % parse_pred_or_func_decl parses a predicate or function declaration.
    %
:- pred parse_pred_or_func_decl(pred_or_func::in, module_name::in, varset::in,
    term::in, decl_attrs::in, prog_context::in, int::in, maybe1(item)::out)
    is det.

parse_pred_or_func_decl(PredOrFunc, ModuleName, VarSet, Term, Attributes,
        Context, SeqNum, MaybeItem) :-
    parse_condition_suffix(Term, BeforeCondTerm, Condition),
    parse_determinism_suffix(VarSet, BeforeCondTerm, BeforeDetismTerm,
        MaybeMaybeDetism),
    parse_with_inst_suffix(BeforeDetismTerm, BeforeWithInstTerm,
        MaybeWithInst),
    parse_with_type_suffix(VarSet, BeforeWithInstTerm, BeforeWithTypeTerm,
        MaybeWithType),
    BaseTerm = BeforeWithTypeTerm,
    (
        MaybeMaybeDetism = ok1(MaybeDetism),
        MaybeWithInst = ok1(WithInst),
        MaybeWithType = ok1(WithType)
    ->
        (
            MaybeDetism = yes(_),
            WithInst = yes(_)
        ->
            Pieces = [words("Error:"), quote("with_inst"),
                words("and determinism both specified."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BaseTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            WithInst = yes(_),
            WithType = no
        ->
            Pieces = [words("Error:"), quote("with_inst"), words("specified"),
                words("without"), quote("with_type"), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BaseTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            (
                % Function declarations with `with_type` annotations
                % have the same form as predicate declarations.
                PredOrFunc = pf_function,
                WithType = no
            ->
                parse_func_decl_base(ModuleName, VarSet, BaseTerm, Condition,
                    MaybeDetism, Attributes, Context, SeqNum, MaybeItem)
            ;
                parse_pred_decl_base(PredOrFunc, ModuleName, VarSet, BaseTerm,
                    Condition, WithType, WithInst, MaybeDetism,
                    Attributes, Context, SeqNum, MaybeItem)
            )
        )
    ;
        Specs = get_any_errors1(MaybeMaybeDetism)
            ++ get_any_errors1(MaybeWithInst)
            ++ get_any_errors1(MaybeWithType),
        MaybeItem = error1(Specs)
    ).

    % parse a `:- pred p(...)' declaration or a
    % `:- func f(...) `with_type` t' declaration
    %
:- pred parse_pred_decl_base(pred_or_func::in, module_name::in, varset::in,
    term::in, condition::in, maybe(mer_type)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, decl_attrs::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_pred_decl_base(PredOrFunc, ModuleName, VarSet, PredTypeTerm, Condition,
        WithType, WithInst, MaybeDet, Attributes0, Context, SeqNum,
        MaybeItem) :-
    get_class_context_and_inst_constraints(ModuleName, VarSet,
        Attributes0, Attributes1, MaybeExistClassInstContext),
    (
        MaybeExistClassInstContext = error3(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeExistClassInstContext =
            ok3(ExistQVars, Constraints, InstConstraints),
        ContextPieces = [words("In")] ++ pred_or_func_decl_pieces(PredOrFunc)
            ++ [suffix(":")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, PredTypeTerm,
            VarSet, ContextPieces, MaybePredNameAndArgs),
        (
            MaybePredNameAndArgs = error2(Specs),
            MaybeItem = error1(Specs)
        ;
            MaybePredNameAndArgs = ok2(Functor, ArgTerms),
            ( parse_type_and_mode_list(InstConstraints, ArgTerms, Args) ->
                ( type_and_mode_list_is_consistent(Args) ->
                    (
                        WithInst = yes(_),
                        Args = [type_only(_) | _]
                    ->
                        Pieces = [words("Error:"), quote("with_inst"),
                            words("specified without argument modes."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(PredTypeTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    ;
                        WithInst = no,
                        WithType = yes(_),
                        Args = [type_and_mode(_, _) | _]
                    ->
                        Pieces = [words("Error: arguments have modes but"),
                            quote("with_inst"), words("not specified."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(PredTypeTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    ;
                        inst_var_constraints_types_modes_self_consistent(Args)
                    ->
                        get_purity(Purity, Attributes1, Attributes),
                        varset.coerce(VarSet, TVarSet),
                        varset.coerce(VarSet, IVarSet),
                        Origin = user,
                        ItemPredDecl = item_pred_decl_info(Origin,
                            TVarSet, IVarSet, ExistQVars, PredOrFunc,
                            Functor, Args, WithType, WithInst, MaybeDet,
                            Condition, Purity, Constraints, Context, SeqNum),
                        Item = item_pred_decl(ItemPredDecl),
                        MaybeItem0 = ok1(Item),
                        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
                    ;
                        PredTypeTermStr =
                            describe_error_term(VarSet, PredTypeTerm),
                        Pieces = [words("Error: inconsistent constraints on"),
                            words("inst variables in")] ++
                            pred_or_func_decl_pieces(PredOrFunc) ++
                            [suffix(":"), nl,
                            words(PredTypeTermStr), suffix("."), nl],
                        Spec = error_spec(severity_error,
                            phase_term_to_parse_tree,
                            [simple_msg(get_term_context(PredTypeTerm),
                                [always(Pieces)])]),
                        MaybeItem = error1([Spec])
                    )
                ;
                    Pieces = [words("Error: some but not all arguments"),
                        words("have modes."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(PredTypeTerm),
                            [always(Pieces)])]),
                    MaybeItem = error1([Spec])
                )
            ;
                PredTypeTermStr = describe_error_term(VarSet, PredTypeTerm),
                Pieces = [words("Error: syntax error in")] ++
                    pred_or_func_decl_pieces(PredOrFunc) ++
                    [words("at"), words(PredTypeTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(PredTypeTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        )
    ).

    % Parse a `:- func p(...)' declaration *without* a with_type clause.
    %
:- pred parse_func_decl_base(module_name::in, varset::in, term::in,
    condition::in, maybe(determinism)::in, decl_attrs::in, prog_context::in,
    int::in, maybe1(item)::out) is det.

parse_func_decl_base(ModuleName, VarSet, Term, Condition, MaybeDet,
        Attributes0, Context, SeqNum, MaybeItem) :-
    get_class_context_and_inst_constraints(ModuleName, VarSet,
        Attributes0, Attributes, MaybeContext),
    (
        MaybeContext = error3(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeContext = ok3(ExistQVars, Constraints, InstConstraints),
        (
            Term = term.functor(term.atom("="),
                [MaybeSugaredFuncTerm, ReturnTerm], _)
        ->
            FuncTerm = desugar_field_access(MaybeSugaredFuncTerm),
            ContextPieces = [words("In"), decl("func"),
                words("declaration")],
            parse_implicitly_qualified_sym_name_and_args(ModuleName, FuncTerm,
                VarSet, ContextPieces, MaybeFuncNameAndArgs),
            (
                MaybeFuncNameAndArgs = error2(Specs),
                MaybeItem = error1(Specs)
            ;
                MaybeFuncNameAndArgs = ok2(FuncName, ArgTerms),
                (
                    parse_type_and_mode_list(InstConstraints, ArgTerms,
                        ArgsPrime)
                ->
                    MaybeArgs = ok1(ArgsPrime)
                ;
                    FuncTermStr = describe_error_term(VarSet, FuncTerm),
                    ArgsPieces = [words("Error: syntax error in arguments of"),
                        decl("func"), words("declaration at"),
                        words(FuncTermStr), suffix("."), nl],
                    ArgsSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(FuncTerm),
                            [always(ArgsPieces)])]),
                    MaybeArgs = error1([ArgsSpec])
                ),
                (
                    parse_type_and_mode(InstConstraints, ReturnTerm,
                        ReturnArgPrime)
                ->
                    MaybeReturnArg = ok1(ReturnArgPrime)
                ;
                    ReturnPieces = [words("Error: syntax error"),
                        words("in return type of"), decl("func"),
                        words("declaration."), nl],
                    ReturnSpec = error_spec(severity_error,
                        phase_term_to_parse_tree,
                        [simple_msg(get_term_context(ReturnTerm),
                            [always(ReturnPieces)])]),
                    MaybeReturnArg = error1([ReturnSpec])
                ),
                (
                    MaybeArgs = ok1(Args),
                    MaybeReturnArg = ok1(ReturnArg)
                ->
                    % We use an auxiliary predicate because the code is just
                    % too deeply indented here.
                    parse_func_decl_base_2(FuncName, Args, ReturnArg,
                        FuncTerm, Term, VarSet, MaybeDet, Condition,
                        ExistQVars, Constraints, Attributes,
                        Context, SeqNum, MaybeItem)
                ;
                    Specs = get_any_errors1(MaybeArgs) ++
                        get_any_errors1(MaybeReturnArg),
                    MaybeItem = error1(Specs)
                )
            )
        ;
            Pieces = [words("Error:"), quote("="), words("expected in"),
                decl("func"), words("declaration."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

:- pred parse_func_decl_base_2(sym_name::in, list(type_and_mode)::in,
    type_and_mode::in, term::in, term::in, varset::in, maybe(determinism)::in,
    condition::in, existq_tvars::in, prog_constraints::in, decl_attrs::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_func_decl_base_2(FuncName, Args, ReturnArg, FuncTerm, Term,
        VarSet, MaybeDet, Condition, ExistQVars, Constraints, Attributes0,
        Context, SeqNum, MaybeItem) :-
    (
        type_and_mode_list_is_consistent(Args)
    ->
        ConsistentArgsSpecs = []
    ;
        ConsistentPieces =
            [words("Error: some but not all arguments have modes."), nl],
        ConsistentSpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncTerm),
                [always(ConsistentPieces)])]),
        ConsistentArgsSpecs = [ConsistentSpec]
    ),
    (
        Args = [type_and_mode(_, _) | _],
        ReturnArg = type_only(_)
    ->
        ArgsOnlyPieces = [words("Error: function arguments have modes,"),
            words("but function result does not."), nl],
        ArgsOnlySpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncTerm),
                [always(ArgsOnlyPieces)])]),
        ArgsOnlySpecs = [ArgsOnlySpec]
    ;
        ArgsOnlySpecs = []
    ),
    (
        Args = [type_only(_) | _],
        ReturnArg = type_and_mode(_, _)
    ->
        ReturnOnlyPieces = [words("Error: function result has mode,"),
            words("but function arguments do not."), nl],
        ReturnOnlySpec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncTerm),
                [always(ReturnOnlyPieces)])]),
        ReturnOnlySpecs = [ReturnOnlySpec]
    ;
        ReturnOnlySpecs = []
    ),
    ConsistencySpecs = ConsistentArgsSpecs ++ ArgsOnlySpecs ++ ReturnOnlySpecs,
    (
        ConsistencySpecs = [_ | _],
        MaybeItem = error1(ConsistencySpecs)
    ;
        ConsistencySpecs = [],
        get_purity(Purity, Attributes0, Attributes),
        varset.coerce(VarSet, TVarSet),
        varset.coerce(VarSet, IVarSet),
        AllArgs = Args ++ [ReturnArg],
        ( inst_var_constraints_types_modes_self_consistent(AllArgs) ->
            Origin = user,
            ItemPredDecl = item_pred_decl_info(Origin, TVarSet, IVarSet,
                ExistQVars, pf_function, FuncName, AllArgs, no, no,
                MaybeDet, Condition, Purity, Constraints, Context, SeqNum),
            Item = item_pred_decl(ItemPredDecl),
            MaybeItem0 = ok1(Item),
            check_no_attributes(MaybeItem0, Attributes, MaybeItem)
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: inconsistent constraints"),
                words("on inst variables in function declaration:"), nl,
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(Term), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

:- pred parse_type_and_mode_list(inst_var_sub::in, list(term)::in,
    list(type_and_mode)::out) is semidet.

parse_type_and_mode_list(_, [], []).
parse_type_and_mode_list(InstConstraints, [H0 | T0], [H | T]) :-
    parse_type_and_mode(InstConstraints, H0, H),
    parse_type_and_mode_list(InstConstraints, T0, T).

:- pred parse_type_and_mode(inst_var_sub::in, term::in, type_and_mode::out)
    is semidet.

parse_type_and_mode(InstConstraints, Term, MaybeTypeAndMode) :-
    ( Term = term.functor(term.atom("::"), [TypeTerm, ModeTerm], _Context) ->
        maybe_parse_type(TypeTerm, Type),
        convert_mode(allow_constrained_inst_var, ModeTerm, Mode0),
        constrain_inst_vars_in_mode_sub(InstConstraints, Mode0, Mode),
        MaybeTypeAndMode = type_and_mode(Type, Mode)
    ;
        maybe_parse_type(Term, Type),
        MaybeTypeAndMode = type_only(Type)
    ).

    % Verify that among the arguments of a :- pred or :- func declaration,
    % either all arguments specify a mode or none of them do.
    %
:- pred type_and_mode_list_is_consistent(list(type_and_mode)::in) is semidet.

type_and_mode_list_is_consistent([]).
type_and_mode_list_is_consistent([Head | Tail]) :-
    (
        Head = type_only(_),
        type_and_mode_list_is_consistent_type_only(Tail)
    ;
        Head = type_and_mode(_, _),
        type_and_mode_list_is_consistent_type_and_mode(Tail)
    ).

:- pred type_and_mode_list_is_consistent_type_only(list(type_and_mode)::in)
    is semidet.

type_and_mode_list_is_consistent_type_only([]).
type_and_mode_list_is_consistent_type_only([Head | Tail]) :-
    Head = type_only(_),
    type_and_mode_list_is_consistent_type_only(Tail).

:- pred type_and_mode_list_is_consistent_type_and_mode(list(type_and_mode)::in)
    is semidet.

type_and_mode_list_is_consistent_type_and_mode([]).
type_and_mode_list_is_consistent_type_and_mode([Head | Tail]) :-
    Head = type_and_mode(_, _),
    type_and_mode_list_is_consistent_type_and_mode(Tail).

%-----------------------------------------------------------------------------%
%
% Parsing mode declarations for predicates and functions.
%

:- pred parse_mode_decl(module_name::in, varset::in, term::in,
    decl_attrs::in, prog_context::in, int::in, maybe1(item)::out) is det.

parse_mode_decl(ModuleName, VarSet, Term, Attributes, Context, SeqNum,
        MaybeItem) :-
    parse_condition_suffix(Term, BeforeCondTerm, Condition),
    parse_determinism_suffix(VarSet, BeforeCondTerm, BeforeDetismTerm,
        MaybeMaybeDetism),
    parse_with_inst_suffix(BeforeDetismTerm, BeforeWithInstTerm,
        MaybeWithInst),
    BaseTerm = BeforeWithInstTerm,
    (
        MaybeMaybeDetism = ok1(MaybeDetism),
        MaybeWithInst = ok1(WithInst)
    ->
        (
            MaybeDetism = yes(_),
            WithInst = yes(_)
        ->
            Pieces = [words("Error:"), quote("with_inst"),
                words("and determinism both specified."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BeforeCondTerm),
                    [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            parse_mode_decl_base(ModuleName, VarSet, BaseTerm, Condition,
                Attributes, WithInst, MaybeDetism, Context, SeqNum, MaybeItem)
        )
    ;
        Specs = get_any_errors1(MaybeMaybeDetism)
            ++ get_any_errors1(MaybeWithInst),
        MaybeItem = error1(Specs)
    ).

:- pred parse_mode_decl_base(module_name::in, varset::in, term::in,
    condition::in, decl_attrs::in, maybe(mer_inst)::in, maybe(determinism)::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

parse_mode_decl_base(ModuleName, VarSet, Term, Condition, Attributes, WithInst,
        MaybeDet, Context, SeqNum, MaybeItem) :-
    (
        WithInst = no,
        Term = term.functor(term.atom("="),
            [MaybeSugaredFuncTerm, ReturnTypeTerm], _)
    ->
        FuncTerm = desugar_field_access(MaybeSugaredFuncTerm),
        ContextPieces = [words("In function"), decl("mode"),
            words("declaration")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, FuncTerm,
            VarSet, ContextPieces, MaybeFunctorArgs),
        (
            MaybeFunctorArgs = error2(Specs),
            MaybeItem = error1(Specs)
        ;
            MaybeFunctorArgs = ok2(Functor, ArgTerms),
            parse_func_mode_decl(Functor, ArgTerms, ModuleName,
                FuncTerm, ReturnTypeTerm, Term, VarSet, MaybeDet, Condition,
                Attributes, Context, SeqNum, MaybeItem)
        )
    ;
        ContextPieces = [words("In"), decl("mode"), words("declaration")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, Term,
            VarSet, ContextPieces, MaybeFunctorArgs),
        (
            MaybeFunctorArgs = error2(Specs),
            MaybeItem = error1(Specs)
        ;
            MaybeFunctorArgs = ok2(Functor, ArgTerms),
            parse_pred_mode_decl(Functor, ArgTerms, ModuleName, Term,
                VarSet, WithInst, MaybeDet, Condition,
                Attributes, Context, SeqNum, MaybeItem)
        )
    ).

:- pred parse_pred_mode_decl(sym_name::in, list(term)::in, module_name::in,
    term::in, varset::in, maybe(mer_inst)::in, maybe(determinism)::in,
    condition::in, decl_attrs::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_pred_mode_decl(Functor, ArgTerms, ModuleName, PredModeTerm, VarSet,
        WithInst, MaybeDet, Condition, Attributes0, Context, SeqNum,
        MaybeItem) :-
    ( convert_mode_list(allow_constrained_inst_var, ArgTerms, ArgModes0) ->
        get_class_context_and_inst_constraints(ModuleName, VarSet,
            Attributes0, Attributes, MaybeConstraints),
        (
            MaybeConstraints = ok3(_, _, InstConstraints),
            list.map(constrain_inst_vars_in_mode_sub(InstConstraints),
                ArgModes0, ArgModes),
            varset.coerce(VarSet, ProgVarSet),
            ( inst_var_constraints_are_self_consistent_in_modes(ArgModes) ->
                (
                    WithInst = no,
                    PredOrFunc = yes(pf_predicate)
                ;
                    WithInst = yes(_),
                    % We don't know whether it's a predicate or a function
                    % until we expand out the inst.
                    PredOrFunc = no
                ),
                ItemModeDecl = item_mode_decl_info(ProgVarSet, PredOrFunc,
                    Functor, ArgModes, WithInst, MaybeDet, Condition, Context,
                    SeqNum),
                Item = item_mode_decl(ItemModeDecl),
                MaybeItem0 = ok1(Item),
                check_no_attributes(MaybeItem0, Attributes, MaybeItem)
            ;
                PredModeTermStr = describe_error_term(VarSet, PredModeTerm),
                Pieces = [words("Error: inconsistent constraints"),
                    words("on inst variables"),
                    words("in predicate mode declaration:"), nl,
                    words(PredModeTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(PredModeTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        ;
            MaybeConstraints = error3(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        PredModeTermStr = describe_error_term(VarSet, PredModeTerm),
        Pieces = [words("Error: syntax error in"), decl("mode"),
            words("declaration at"), words(PredModeTermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(PredModeTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

:- pred parse_func_mode_decl(sym_name::in, list(term)::in, module_name::in,
    term::in, term::in, term::in, varset::in, maybe(determinism)::in,
    condition::in, decl_attrs::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_func_mode_decl(Functor, ArgTerms, ModuleName, FuncMode, RetModeTerm,
        FullTerm, VarSet, MaybeDet, Condition, Attributes0, Context, SeqNum,
        MaybeItem) :-
    ( convert_mode_list(allow_constrained_inst_var, ArgTerms, ArgModes0) ->
        get_class_context_and_inst_constraints(ModuleName, VarSet,
            Attributes0, Attributes, MaybeConstraints),
        (
            MaybeConstraints = ok3(_, _, InstConstraints),
            list.map(constrain_inst_vars_in_mode_sub(InstConstraints),
                ArgModes0, ArgModes),
            (
                convert_mode(allow_constrained_inst_var, RetModeTerm, RetMode0)
            ->
                constrain_inst_vars_in_mode_sub(InstConstraints,
                    RetMode0, RetMode),
                varset.coerce(VarSet, InstVarSet),
                ArgReturnModes = ArgModes ++ [RetMode],
                (
                    inst_var_constraints_are_self_consistent_in_modes(
                        ArgReturnModes)
                ->
                    ItemModeDecl = item_mode_decl_info(InstVarSet,
                        yes(pf_function), Functor, ArgReturnModes, no,
                        MaybeDet, Condition, Context, SeqNum),
                    Item = item_mode_decl(ItemModeDecl),
                    MaybeItem0 = ok1(Item),
                    check_no_attributes(MaybeItem0, Attributes, MaybeItem)
                ;
                    FullTermStr = describe_error_term(VarSet, FullTerm),
                    Pieces = [words("Error: inconsistent constraints"),
                        words("on inst variables"),
                        words("in function mode declaration:"), nl,
                        words(FullTermStr), suffix("."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(get_term_context(FullTerm),
                            [always(Pieces)])]),
                    MaybeItem = error1([Spec])
                )
            ;
                Pieces = [words("Error: syntax error in return mode"),
                    words("of function mode declaration."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(RetModeTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        ;
            MaybeConstraints = error3(Specs),
            MaybeItem = error1(Specs)
        )
    ;
        % XXX Should say which argument.
        FuncModeStr = describe_error_term(VarSet, FuncMode),
        Pieces = [words("Error: syntax error in arguments of"),
            words("function mode declaration at"),
            words(FuncModeStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(FuncMode), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%-----------------------------------------------------------------------------%

    % We could perhaps get rid of some code duplication between here and
    % prog_io_typeclass.m?

    % XXX This documentation is out of date.
    % get_class_context_and_inst_constraints(ModuleName, Attributes0,
    %   Attributes, MaybeContext, MaybeInstConstraints):
    %
    % Parse type quantifiers, type class constraints and inst constraints
    % from the declaration attributes in Attributes0.
    % MaybeContext is either bound to the correctly parsed context, or
    % an appropriate error message (if there was a syntax error).
    % MaybeInstConstraints is either bound to a map containing the inst
    % constraints or an appropriate error message (if there was a syntax
    % error).
    % Attributes is bound to the remaining attributes.
    %
:- pred get_class_context_and_inst_constraints(module_name::in, varset::in,
    decl_attrs::in, decl_attrs::out,
    maybe3(existq_tvars, prog_constraints, inst_var_sub)::out) is det.

get_class_context_and_inst_constraints(ModuleName, VarSet, RevAttributes0,
        RevAttributes, MaybeExistClassInstContext) :-
    % Constraints and quantifiers should occur in the following order
    % (outermost to innermost):
    %
    %                               operator        precedence
    %                               --------        ----------
    %   1. universal quantifiers    all             950
    %   2. existential quantifiers  some            950
    %   3. universal constraints    <=              920
    %   4. existential constraints  =>              920 [*]
    %   5. the decl itself          pred or func    800
    %
    % When we reach here, Attributes0 contains declaration attributes
    % in the opposite order -- innermost to outermost -- so we reverse
    % them before we start.
    %
    % [*] Note that the semantic meaning of `=>' is not quite the same
    % as implication; logically speaking it's more like conjunction.
    % Oh well, at least it has the right precedence.
    %
    % In theory it could make sense to allow the order of 2 & 3 to be
    % swapped, or (in the case of multiple constraints & multiple
    % quantifiers) to allow arbitrary interleaving of 2 & 3, but in
    % practice it seems there would be little benefit in allowing that
    % flexibility, so we don't.
    %
    % Universal quantification is the default, so we just ignore
    % universal quantifiers.  (XXX It might be a good idea to check
    % that any universally quantified type variables do actually
    % occur somewhere in the type declaration, and are not also
    % existentially quantified, and if not, issue a warning or
    % error message.)

    list.reverse(RevAttributes0, Attributes0),
    get_quant_vars(quant_type_univ, ModuleName, Attributes0, Attributes1,
        [], _UnivQVars),
    get_quant_vars(quant_type_exist, ModuleName, Attributes1, Attributes2,
        [], ExistQVars0),
    list.map(term.coerce_var, ExistQVars0, ExistQVars),
    get_constraints(quant_type_univ, ModuleName, VarSet, Attributes2,
        Attributes3, MaybeUnivConstraints),
    get_constraints(quant_type_exist, ModuleName, VarSet, Attributes3,
        Attributes, MaybeExistConstraints),
    list.reverse(Attributes, RevAttributes),

    (
        MaybeUnivConstraints = ok2(UnivConstraints, UnivInstConstraints),
        MaybeExistConstraints = ok2(ExistConstraints, ExistInstConstraints)
    ->
        ClassConstraints = constraints(UnivConstraints, ExistConstraints),
        InstConstraints =
            map.old_merge(UnivInstConstraints, ExistInstConstraints),
        MaybeExistClassInstContext = ok3(ExistQVars, ClassConstraints,
            InstConstraints)
    ;
        Specs = get_any_errors2(MaybeUnivConstraints) ++
            get_any_errors2(MaybeExistConstraints),
        MaybeExistClassInstContext = error3(Specs)
    ).

:- pred get_constraints(quantifier_type::in, module_name::in, varset::in,
    decl_attrs::in, decl_attrs::out, maybe_class_and_inst_constraints::out)
    is det.

get_constraints(QuantType, ModuleName, VarSet, !Attributes,
        MaybeClassInstConstraints) :-
    (
        !.Attributes = [
            decl_attr_constraints(QuantType, ConstraintsTerm) - _Term
            | !:Attributes]
    ->
        parse_class_and_inst_constraints(ModuleName, VarSet, ConstraintsTerm,
            MaybeHeadConstraints),
        % There may be more constraints of the same type;
        % collect them all and combine them.
        get_constraints(QuantType, ModuleName, VarSet, !Attributes,
            MaybeTailConstraints),
        (
            MaybeHeadConstraints =
                ok2(HeadClassConstraints, HeadInstConstraint),
            MaybeTailConstraints =
                ok2(TailClassConstraints, TailInstConstraint)
        ->
            ClassConstraints = HeadClassConstraints ++ TailClassConstraints,
            InstConstraints =
                map.old_merge(HeadInstConstraint, TailInstConstraint),
            MaybeClassInstConstraints = ok2(ClassConstraints, InstConstraints)
        ;
            Specs = get_any_errors2(MaybeHeadConstraints) ++
                get_any_errors2(MaybeTailConstraints),
            MaybeClassInstConstraints = error2(Specs)
        )
    ;
        MaybeClassInstConstraints = ok2([], map.init)
    ).

%-----------------------------------------------------------------------------%

:- pred parse_promise(module_name::in, promise_type::in, varset::in,
    list(term)::in, decl_attrs::in, prog_context::in, int::in,
    maybe1(item)::out) is semidet.

parse_promise(ModuleName, PromiseType, VarSet, [Term], Attributes, Context,
        SeqNum, MaybeItem) :-
    varset.coerce(VarSet, ProgVarSet0),
    ContextPieces = [],
    parse_goal(Term, ContextPieces, MaybeGoal0, ProgVarSet0, ProgVarSet),
    (
        MaybeGoal0 = ok1(Goal0),
        % Get universally quantified variables.
        (
            PromiseType = promise_type_true,
            ( Goal0 = all_expr(UnivVars0, AllGoal) - _Context ->
                UnivVars0 = UnivVars,
                Goal = AllGoal
            ;
                UnivVars = [],
                Goal = Goal0
            )
        ;
            ( PromiseType = promise_type_exclusive
            ; PromiseType = promise_type_exhaustive
            ; PromiseType = promise_type_exclusive_exhaustive
            ),
            get_quant_vars(quant_type_univ, ModuleName, Attributes, _,
                [], UnivVars0),
            list.map(term.coerce_var, UnivVars0, UnivVars),
            Goal0 = Goal
        ),
        ItemPromise = item_promise_info(PromiseType, Goal, ProgVarSet,
            UnivVars, Context, SeqNum),
        Item = item_promise(ItemPromise),
        MaybeItem = ok1(Item)
    ;
        MaybeGoal0 = error1(Specs),
        MaybeItem = error1(Specs)
    ).

%-----------------------------------------------------------------------------%

    % parse_determinism_suffix(VarSet, BodyTerm, BeforeDetismTerm,
    %   MaybeMaybeDetism):
    %
    % Look for a suffix of the form "is <detism>" in Term. If we find one,
    % bind MaybeMaybeDetism to ok1(yes()) wrapped around the determinism,
    % and bind BeforeDetismTerm to the other part of Term. If we don't
    % find, one, then bind MaybeMaybeDetism to ok1(no).
    %
:- pred parse_determinism_suffix(varset::in, term::in, term::out,
    maybe1(maybe(determinism))::out) is det.

parse_determinism_suffix(VarSet, Term, BeforeDetismTerm, MaybeMaybeDetism) :-
    (
        Term = term.functor(term.atom("is"), Args, _),
        Args = [BeforeDetismTermPrime, DetismTerm]
    ->
        BeforeDetismTerm = BeforeDetismTermPrime,
        (
            DetismTerm = term.functor(term.atom(DetismFunctor), [], _),
            standard_det(DetismFunctor, Detism)
        ->
            MaybeMaybeDetism = ok1(yes(Detism))
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: invalid determinism category"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(DetismTerm), [always(Pieces)])]),
            MaybeMaybeDetism = error1([Spec])
        )
    ;
        BeforeDetismTerm = Term,
        MaybeMaybeDetism = ok1(no)
    ).

    % Process the `with_type type` suffix part of a declaration.
    %
:- pred parse_with_type_suffix(varset::in, term::in, term::out,
    maybe1(maybe(mer_type))::out) is det.

parse_with_type_suffix(VarSet, Term, BeforeWithTypeTerm, MaybeWithType) :-
    (
        Term = term.functor(TypeQualifier,
            [BeforeWithTypeTermPrime, TypeTerm], _),
        (
            TypeQualifier = term.atom("with_type")
        ;
            TypeQualifier = term.atom(":")
        )
    ->
        BeforeWithTypeTerm = BeforeWithTypeTermPrime,
        % XXX Should supply more correct ContextPieces.
        ContextPieces = [],
        parse_type(TypeTerm, VarSet, ContextPieces, MaybeType),
        (
            MaybeType = ok1(Type),
            MaybeWithType = ok1(yes(Type))
        ;
            MaybeType = error1(Specs),
            MaybeWithType = error1(Specs)
        )
    ;
        BeforeWithTypeTerm = Term,
        MaybeWithType = ok1(no)
    ).

    % Process the `with_inst inst` suffix part of a declaration.
    %
:- pred parse_with_inst_suffix(term::in, term::out,
    maybe1(maybe(mer_inst))::out) is det.

parse_with_inst_suffix(Term, BeforeWithInstTerm, MaybeWithInst) :-
    (
        Term = term.functor(term.atom("with_inst"),
            [BeforeWithInstTermPrime, InstTerm], _)
    ->
        BeforeWithInstTerm = BeforeWithInstTermPrime,
        ( convert_inst(allow_constrained_inst_var, InstTerm, Inst) ->
            MaybeWithInst = ok1(yes(Inst))
        ;
            Pieces = [words("Error: invalid inst in"), quote("with_inst"),
                suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(InstTerm), [always(Pieces)])]),
            MaybeWithInst = error1([Spec])
        )
    ;
        BeforeWithInstTerm = Term,
        MaybeWithInst = ok1(no)
    ).

%-----------------------------------------------------------------------------%

:- pred get_quant_vars(quantifier_type::in, module_name::in,
    decl_attrs::in, decl_attrs::out, list(var)::in, list(var)::out) is det.

get_quant_vars(QuantType, ModuleName, !Attributes, !Vars) :-
    (
        !.Attributes = [decl_attr_quantifier(QuantType, QuantVars) - _
            | !:Attributes]
    ->
        !:Vars = !.Vars ++ QuantVars,
        get_quant_vars(QuantType, ModuleName, !Attributes, !Vars)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % Perform one of the following field-access syntax rewrites if possible:
    %
    %   A ^ f(B, ...)       --->    f(B, ..., A)
    %   (A ^ f(B, ...) := X)    --->    'f :='(B, ..., A, X)
    %
:- func desugar_field_access(term) = term.

desugar_field_access(Term) = DesugaredTerm :-
    (
        Term = functor(atom("^"), [A, RHS], Context),
        RHS  = functor(atom(FieldName), Bs, _)
    ->
        DesugaredTerm = functor(atom(FieldName), Bs ++ [A], Context)
    ;
        Term = functor(atom(":="), [LHS, X], _),
        LHS  = functor(atom("^"), [A, RHS], Context),
        RHS  = functor(atom(FieldName), Bs, _)
    ->
        FunctionName = FieldName ++ " :=",
        DesugaredTerm = functor(atom(FunctionName), Bs ++ [A, X], Context)
    ;
        DesugaredTerm = Term
    ).

%-----------------------------------------------------------------------------%

    % A ModuleSpecifier is just an sym_name.
    %
:- pred parse_module_specifier(varset::in, term::in,
    maybe1(module_specifier)::out) is det.

parse_module_specifier(VarSet, Term, MaybeModuleSpecifier) :-
    parse_symbol_name(VarSet, Term, MaybeModuleSpecifier).

    % A ModuleName is an implicitly-quantified sym_name.
    %
    % We check for module names starting with capital letters as a special
    % case, so that we can report a better error message for that case.
    %
:- pred parse_module_name(module_name::in, varset::in, term::in,
    maybe1(module_name)::out) is det.

parse_module_name(DefaultModuleName, VarSet, Term, MaybeModule) :-
    (
        Term = term.variable(_, Context),
        Pieces = [words("Error: module names starting with capital letters"),
            words("must be quoted using single quotes"),
            words("(e.g. "":- module 'Foo'."")."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeModule = error1([Spec])
    ;
        Term = term.functor(_, _, _),
        parse_implicitly_qualified_symbol_name(DefaultModuleName, VarSet,
            Term, MaybeModule)
    ).

%-----------------------------------------------------------------------------%

:- pred get_purity(purity::out, decl_attrs::in, decl_attrs::out) is det.

get_purity(Purity, !Attributes) :-
    ( !.Attributes = [decl_attr_purity(Purity0) - _ | !:Attributes] ->
        Purity = Purity0
    ;
        Purity = purity_pure
    ).

%-----------------------------------------------------------------------------%

:- func pred_or_func_decl_pieces(pred_or_func) = list(format_component).

pred_or_func_decl_pieces(pf_function) =
    [decl("func"), words("declaration")].
pred_or_func_decl_pieces(pf_predicate) =
    [decl("pred"), words("declaration")].

%-----------------------------------------------------------------------------%

:- type maker(T1, T2) == pred(T1, T2).
:- mode maker == (pred(in, out) is det).

:- pred process_maybe1(maker(T1, T2)::maker, maybe1(T1)::in, maybe1(T2)::out)
    is det.

process_maybe1(Maker, ok1(X), ok1(Y)) :-
    call(Maker, X, Y).
process_maybe1(_, error1(Specs), error1(Specs)).

:- pred process_maybe1_to_t(maker(T1, maybe1(T2))::maker,
    maybe1(T1)::in, maybe1(T2)::out) is det.

process_maybe1_to_t(Maker, ok1(X), Y) :-
    call(Maker, X, Y).
process_maybe1_to_t(_, error1(Specs), error1(Specs)).

%-----------------------------------------------------------------------------%

:- pred make_use(list(module_specifier)::in, module_defn::out) is det.

make_use(Syms, md_use(Syms)).

:- pred make_import(list(module_specifier)::in, module_defn::out) is det.

make_import(Syms, md_import(Syms)).

:- pred make_export(list(module_specifier)::in, module_defn::out) is det.

make_export(Syms, md_export(Syms)).

%-----------------------------------------------------------------------------%

:- pred make_module_defn(maker(list(module_specifier), module_defn)::maker,
    prog_context::in, int::in, list(module_specifier)::in, item::out) is det.

make_module_defn(MakeModuleDefnPred, Context, SeqNum, ModuleSpecs, Item) :-
    call(MakeModuleDefnPred, ModuleSpecs, ModuleDefn),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
    Item = item_module_defn(ItemModuleDefn).

:- pred make_external(maybe(backend)::in, prog_context::in, int::in,
    sym_name_specifier::in, item::out) is det.

make_external(MaybeBackend, Context, SeqNum, SymSpec, Item) :-
    ModuleDefn = md_external(MaybeBackend, SymSpec),
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, SeqNum),
    Item = item_module_defn(ItemModuleDefn).

%-----------------------------------------------------------------------------%

    % Create a dummy term with the specified context.
    % Used for error messages that are associated with some specific
    % context, but for which we don't want to print out the term
    % (or for which the term isn't available to be printed out).
    %
:- pred dummy_term_with_context(term.context::in, term::out) is det.

dummy_term_with_context(Context, Term) :-
    Term = term.functor(term.atom(""), [], Context).

%-----------------------------------------------------------------------------%
%
% You can uncomment this section for debugging.
%

% :- interface.
%
% :- pred write_item_to_stream(io.output_stream::in, item::in, io::di, io::uo)
%     is det.
%
% :- pred write_item_to_stdout(item::in, io::di, io::uo) is det.
%
% :- pred write_items_to_file(string::in, list(item)::in, io::di, io::uo)
%     is det.
%
% :- implementation.
%
% :- import_module pretty_printer.
%
% write_item_to_stream(Stream, Item, !IO) :-
%     write_doc(Stream, format(Item), !IO),
%     io.nl(Stream, !IO).
%
% write_item_to_stdout(Item, !IO) :-
%     write_item_to_stream(io.stdout_stream, Item, !IO).
%
% write_items_to_file(FileName, Items, !IO) :-
%     io.open_output(FileName, Result, !IO),
%     (
%         Result = ok(Stream),
%         list.foldl(write_item_to_stream(Stream), Items, !IO)
%     ;
%         Result = error(_)
%     ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_item.
%-----------------------------------------------------------------------------%
