%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2011 The University of Melbourne.
% Copyright (C) 2016-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_type_defn.m.
%
% This module parses type definitions.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_type_defn.

:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module term.
:- import_module varset.

    % Parse the definition of a solver type, or of a non-solver type.
    % (The is_solver_type argument of parse_type_defn_item says whether
    % we are parsing the definition of non-solver type, or the part of
    % the definition of a solver type that comes after the "solver" keyword.)
    %
    % The syntax of type definitions allows them to include components
    % that their semantics does not permit, such as a specification of
    % type-specific equality and comparison predicates for subtypes.
    % In such cases, we return a message about the error, but, by ignoring
    % the unexpected component, we can still return a meaningful type
    % definition item. We can return both using iom_item_and_specs.
    %
:- pred parse_solver_type_defn_item(module_name::in, varset::in,
    list(term)::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.
:- pred parse_type_defn_item(module_name::in, varset::in,
    list(term)::in, prog_context::in, item_seq_num::in, is_solver_type::in,
    maybe1(item_or_marker)::out) is det.

    % Parses the attributes in a "where" clause. It looks for and processes
    % only the attributes that can occur on foreign_type pragmas. This includes
    % only the specification of equality and/or comparison predicates,
    % or the assertion that they exist (but that they are visible only
    % in another module).
    %
    % Exported to parse_pragma.m for use when parsing foreign type pragmas.
    %
:- pred parse_where_unify_compare(module_name::in, varset::in, term::in,
    maybe1(maybe_canonical)::out) is det.

    % parse_type_defn_head(ParseContext, ModuleName, VarSet, Head, HeadResult):
    %
    % Check the head of a type definition for errors.
    %
    % Exported to parse_pragma.m for use when parsing foreign type pragmas.
    %
:- pred parse_type_defn_head(cord(format_piece)::in,
    module_name::in, varset::in, term::in,
    maybe2(sym_name, list(type_param))::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_class.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_mutable.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.parse_vars.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module bag.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module term_context.
:- import_module term_int.
:- import_module uint32.
:- import_module unit.

%---------------------------------------------------------------------------%

parse_solver_type_defn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        MaybeIOM) :-
    ( if
        ArgTerms = [ArgTerm],
        ArgTerm = term.functor(term.atom("type"), SubArgTerms, SubContext)
    then
        parse_type_defn_item(ModuleName, VarSet, SubArgTerms,
            SubContext, SeqNum, solver_type, MaybeIOM)
    else
        Pieces = [words("Error: the"), decl("solver"), words("keyword"),
            words("should be followed by a type definition."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

parse_type_defn_item(ModuleName, VarSet, ArgTerms, Context, SeqNum,
        IsSolverType, MaybeIOM) :-
    ( if ArgTerms = [TypeDefnTerm] then
        ( if
            TypeDefnTerm = term.functor(term.atom(Name), TypeDefnArgTerms, _),
            TypeDefnArgTerms = [HeadTerm, BodyTerm],
            ( Name = "--->"
            ; Name = "=="
            ; Name = "where"
            )
        then
            (
                Name = "--->",
                parse_du_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm,
                    Context, SeqNum, IsSolverType, MaybeIOM)
            ;
                Name = "==",
                parse_eqv_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm,
                    Context, SeqNum, IsSolverType, MaybeIOM)
            ;
                Name = "where",
                parse_where_block_type_defn(ModuleName, VarSet,
                    HeadTerm, BodyTerm, Context, SeqNum, IsSolverType,
                    MaybeIOM)
            )
        else
            parse_abstract_type_defn(ModuleName, VarSet, TypeDefnTerm,
                Context, SeqNum, IsSolverType, MaybeIOM)
        )
    else
        Pieces = [words("Error: a"), decl("type"), words("declaration"),
            words("should have just one argument,"),
            words("which should be the definition of a type."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%
%
% Code dealing with definitions of discriminated union types.
%

    % parse_du_type_defn parses the definition of a discriminated union type.
    %
:- pred parse_du_type_defn(module_name::in, varset::in, term::in, term::in,
    prog_context::in, item_seq_num::in, is_solver_type::in,
    maybe1(item_or_marker)::out) is det.

parse_du_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Context, SeqNum,
        IsSolverType, MaybeIOM) :-
    % XXX We should consider which errors should prevent us from returning
    % a type definition item, and which should not.
    (
        IsSolverType = solver_type,
        SolverPieces = [words("Error: a solver type"),
            words("cannot have data constructors."), nl],
        SolverSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree,
            get_term_context(HeadTerm), SolverPieces),
        SolverSpecs = [SolverSpec]
    ;
        IsSolverType = non_solver_type,
        SolverSpecs = []
    ),

    ContextPieces =
        cord.from_list([words("On the left hand side of type definition:")]),
    ( if
        HeadTerm = functor(atom("=<"), HeadArgs, _HeadContext),
        HeadArgs = [SubTypeTerm, SuperTypeTerm]
    then
        parse_type_defn_head(ContextPieces, ModuleName, VarSet, SubTypeTerm,
            MaybeTypeCtorAndArgs),
        SuperTypeContextPieces =
            cord.from_list([words("In the supertype part of a"),
                words("subtype definition:")]),
        parse_supertype(VarSet, SuperTypeContextPieces, SuperTypeTerm,
            MaybeSuperType0),
        SuperTypeContext = get_term_context(SuperTypeTerm)
    else
        parse_type_defn_head(ContextPieces, ModuleName, VarSet, HeadTerm,
            MaybeTypeCtorAndArgs),
        MaybeSuperType0 = ok1(not_a_subtype),
        SuperTypeContext = dummy_context
    ),
    du_type_rhs_ctors_and_where_terms(BodyTerm, CtorsTerm, MaybeWhereTerm),
    parse_maybe_exist_quant_constructors(ModuleName, VarSet, CtorsTerm,
        MaybeOneOrMoreCtors),
    (
        MaybeWhereTerm = no,
        MaybeWhere = ok3(no, canon, no)
    ;
        MaybeWhereTerm = yes(WhereTerm),
        parse_type_decl_where_term(non_solver_type, ModuleName, SeqNum,
            VarSet, WhereTerm, MaybeWhere)
    ),
    ( if
        SolverSpecs = [],
        MaybeTypeCtorAndArgs = ok2(TypeSymName, Params),
        MaybeSuperType0 = ok1(MaybeSuperType),
        MaybeOneOrMoreCtors = ok1(OneOrMoreCtors),
        MaybeWhere = ok3(SolverTypeDetails, MaybeCanonical, MaybeDirectArgIs)
    then
        % We asked parse_type_decl_where_term to return an error if
        % WhereTerm contains solver attributes, so we shouldn't get here
        % if SolverTypeDetails is yes(...).
        expect(unify(SolverTypeDetails, no), $pred,
            "discriminated union type has solver type details"),
        OneOrMoreCtors = one_or_more(HeadCtor, TailCtors),
        Ctors = [HeadCtor | TailCtors],
        process_du_ctors(Params, VarSet, BodyTerm, Ctors,
            [], ErrorSpecs0),
        (
            MaybeSuperType = subtype_of(SuperType),
            DetailsSub = type_details_sub(SuperType, OneOrMoreCtors),
            TypeDefn = parse_tree_sub_type(DetailsSub),
            check_supertype_vars(Params, VarSet, SuperType, SuperTypeContext,
                ErrorSpecs0, ErrorSpecs),
            % By returning a meaningful type definition when we read in
            % a "where equality/comparison is" or "where direct_arg is" clause
            % for a subtype definition, we allow the compiler to find other
            % errors in the module *without* generating misleading error
            % messages about TypeSymName being undefined. This is the case
            % e.g. with tests/invalid/subtype_user_compare.m.
            (
                MaybeCanonical = canon,
                RecoverableSpecs0 = []
            ;
                MaybeCanonical = noncanon(_),
                CanonTypeCtor = type_ctor(TypeSymName, list.length(Params)),
                CanonPieces = [words("Error: the subtype"),
                    unqual_type_ctor(CanonTypeCtor),
                    words("is not allowed to have its own"),
                    words("user-defined equality or comparison;"),
                    words("it must inherit any user-defined"),
                    words("equality and comparison predicates"),
                    words("from its supertype."), nl],
                CanonSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, CanonPieces),
                RecoverableSpecs0 = [CanonSpec]
            ),
            (
                MaybeDirectArgIs = no,
                RecoverableSpecs = RecoverableSpecs0
            ;
                MaybeDirectArgIs = yes(_),
                DirectArgTypeCtor =
                    type_ctor(TypeSymName, list.length(Params)),
                DirectArgPieces = [words("Error: the subtype"),
                    unqual_type_ctor(DirectArgTypeCtor),
                    words("is not allowed to have its own"),
                    quote("where direct_arg is"), words("annotation;"),
                    words("it must inherit its representation"),
                    words("from its supertype."), nl],
                DirectArgSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, DirectArgPieces),
                RecoverableSpecs = [DirectArgSpec | RecoverableSpecs0]
            )
        ;
            MaybeSuperType = not_a_subtype,
            DetailsDu = type_details_du(OneOrMoreCtors, MaybeCanonical,
                MaybeDirectArgIs),
            TypeDefn = parse_tree_du_type(DetailsDu),
            (
                MaybeDirectArgIs = yes(DirectArgCtors),
                check_direct_arg_ctors(Ctors, DirectArgCtors, BodyTerm,
                    ErrorSpecs0, ErrorSpecs)
            ;
                MaybeDirectArgIs = no,
                ErrorSpecs = ErrorSpecs0
            ),
            RecoverableSpecs = []
        ),
        (
            ErrorSpecs = [],
            varset.coerce(VarSet, TypeVarSet),
            ItemTypeDefn = item_type_defn_info(TypeSymName, Params, TypeDefn,
                TypeVarSet, Context, SeqNum),
            Item = item_type_defn(ItemTypeDefn),
            (
                RecoverableSpecs = [],
                IOM = iom_item(Item)
            ;
                RecoverableSpecs = [_ | _],
                IOM = iom_item_and_error_specs(Item, RecoverableSpecs)
            ),
            MaybeIOM = ok1(IOM)
        ;
            ErrorSpecs = [_ | _],
            MaybeIOM = error1(ErrorSpecs)
        )
    else
        Specs = SolverSpecs ++
            get_any_errors2(MaybeTypeCtorAndArgs) ++
            get_any_errors1(MaybeSuperType0) ++
            get_any_errors1(MaybeOneOrMoreCtors) ++
            get_any_errors3(MaybeWhere),
        MaybeIOM = error1(Specs)
    ).

:- pred du_type_rhs_ctors_and_where_terms(term::in,
    term::out, maybe(term)::out) is det.

du_type_rhs_ctors_and_where_terms(Term, CtorsTerm, MaybeWhereTerm) :-
    ( if
        Term = term.functor(term.atom("where"), Args, _Context),
        Args = [CtorsTermPrime, WhereTerm]
    then
        CtorsTerm = CtorsTermPrime,
        MaybeWhereTerm = yes(WhereTerm)
    else
        CtorsTerm = Term,
        MaybeWhereTerm = no
    ).

    % Convert a list of terms separated by semicolons (known as a
    % "disjunction", even thought the terms aren't goals in this case)
    % into a list of constructors.
    %
:- pred parse_maybe_exist_quant_constructors(module_name::in, varset::in,
    term::in, maybe1(one_or_more(constructor))::out) is det.

parse_maybe_exist_quant_constructors(ModuleName, VarSet, Term,
        MaybeConstructors) :-
    disjunction_to_one_or_more(Term, one_or_more(HeadBodyTerm, TailBodyTerms)),
    parse_maybe_exist_quant_constructors_loop(ModuleName, VarSet, 0u32,
        HeadBodyTerm, TailBodyTerms, MaybeConstructors).

    % Try to parse the term as a list of constructors.
    %
:- pred parse_maybe_exist_quant_constructors_loop(module_name::in, varset::in,
    uint32::in, term::in, list(term)::in,
    maybe1(one_or_more(constructor))::out) is det.

parse_maybe_exist_quant_constructors_loop(ModuleName, VarSet, CurOrdinal,
        HeadTerm, TailTerms, MaybeConstructors) :-
    parse_maybe_exist_quant_constructor(ModuleName, VarSet, CurOrdinal,
        HeadTerm, MaybeHeadConstructor),
    (
        TailTerms = [],
        (
            MaybeHeadConstructor = ok1(HeadConstructor),
            MaybeConstructors = ok1(one_or_more(HeadConstructor, []))
        ;
            MaybeHeadConstructor = error1(Specs),
            MaybeConstructors = error1(Specs)
        )
    ;
        TailTerms = [HeadTailTerm | TailTailTerms],
        parse_maybe_exist_quant_constructors_loop(ModuleName, VarSet,
            CurOrdinal + 1u32, HeadTailTerm, TailTailTerms,
            MaybeTailConstructors),
        ( if
            MaybeHeadConstructor = ok1(HeadConstructor),
            MaybeTailConstructors = ok1(TailConstructors)
        then
            MaybeConstructors =
                ok1(one_or_more.cons(HeadConstructor, TailConstructors))
        else
            Specs = get_any_errors1(MaybeHeadConstructor) ++
                get_any_errors1(MaybeTailConstructors),
            MaybeConstructors = error1(Specs)
        )
    ).

:- pred parse_maybe_exist_quant_constructor(module_name::in, varset::in,
    uint32::in, term::in, maybe1(constructor)::out) is det.

parse_maybe_exist_quant_constructor(ModuleName, VarSet, Ordinal, Term,
        MaybeConstructor) :-
    ( if Term = term.functor(term.atom("some"), [VarsTerm, SubTerm], _) then
        ContextPieces = cord.from_list([words("in first argument of"),
            quote("some"), suffix(":")]),
        parse_possibly_repeated_vars(VarsTerm, VarSet, ContextPieces,
            MaybeExistQVars),
        (
            MaybeExistQVars = ok1(ExistQVars),
            list.map(term.coerce_var, ExistQVars, ExistQTVars),
            parse_constructor(ModuleName, VarSet, Ordinal, ExistQTVars,
                SubTerm, MaybeConstructor)
        ;
            MaybeExistQVars = error1(Specs),
            MaybeConstructor = error1(Specs)
        )
    else
        ExistQVars = [],
        parse_constructor(ModuleName, VarSet, Ordinal, ExistQVars, Term,
            MaybeConstructor)
    ).

:- pred parse_constructor(module_name::in, varset::in, uint32::in,
    list(tvar)::in, term::in, maybe1(constructor)::out) is det.

parse_constructor(ModuleName, VarSet, Ordinal, ExistQVars, Term,
        MaybeConstructor) :-
    get_existential_constraints_from_term(ModuleName, VarSet, Term,
        BeforeConstraintsTerm, MaybeConstraints),
    (
        MaybeConstraints = error1(Specs),
        MaybeConstructor = error1(Specs)
    ;
        MaybeConstraints = ok1(Constraints),
        (
            ExistQVars = [],
            (
                Constraints = [],
                MaybeMaybeExistConstraints = ok1(no_exist_constraints)
            ;
                Constraints = [_ | _],
                MCPieces = [words("Error: since there are no"),
                    words("existentially quantified arguments,"),
                    words("there should be no constraints on them."), nl],
                MCSpec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(Term), MCPieces),
                MaybeMaybeExistConstraints = error1([MCSpec])
            )
        ;
            ExistQVars = [_ | _],
            GetConstraintArgTypes = (func(constraint(_, Ts)) = Ts),
            ConstrainedTypeLists =
                list.map(GetConstraintArgTypes, Constraints),
            list.condense(ConstrainedTypeLists, ConstrainedTypes),
            % We compute ConstrainedQVars in this roundabout way to give it
            % the same ordering as ExistQVars. Also, the list returned
            % by type_vars_in_types may contain duplicates.
            type_vars_in_types(ConstrainedTypes, ConstrainedQVars0),
            list.delete_elems(ExistQVars, ConstrainedQVars0,
                UnconstrainedQVars),
            list.delete_elems(ExistQVars, UnconstrainedQVars,
                ConstrainedQVars),
            ExistConstraints = cons_exist_constraints(ExistQVars,
                Constraints, UnconstrainedQVars, ConstrainedQVars),
            MaybeMaybeExistConstraints =
                ok1(exist_constraints(ExistConstraints))
        ),
        ( if
            % Note that as a special case, one level of curly braces around
            % the constructor are ignored. This is to allow you to define
            % ';'/2 and 'some'/2 constructors.
            % XXX I (zs) don't think that this is a good idea; I think such
            % constructors are much more likely to be confusing than useful.
            BeforeConstraintsTerm = term.functor(term.atom("{}"),
                [InsideBracesTerm], _Context)
        then
            MainTerm = InsideBracesTerm
        else
            MainTerm = BeforeConstraintsTerm
        ),
        ContextPieces = cord.singleton(words("In constructor definition:")),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
            ContextPieces, MainTerm, MaybeFunctorAndArgTerms),
        (
            MaybeFunctorAndArgTerms = error2(FAASpecs),
            Functor = unqualified(""),  % won't be used due to the other errors
            MaybeConstructorArgs = error1(FAASpecs)
        ;
            MaybeFunctorAndArgTerms = ok2(Functor, ArgTerms),
            MaybeConstructorArgs = convert_constructor_arg_list(ModuleName,
                VarSet, ArgTerms)
        ),
        MainTermContext = get_term_context(MainTerm),
        ( if
            MaybeMaybeExistConstraints = ok1(exist_constraints(_)),
            MaybeConstructorArgs = ok1([])
        then
            NoArgsPieces = [words("Error: since there are no arguments,"),
                words("(existentially quantified or otherwise),"),
                words("there should be no constraints on them."), nl],
            NoArgsSpecs = [simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(MainTerm),
                NoArgsPieces)]
        else
            NoArgsSpecs = []
        ),
        ( if
            MaybeMaybeExistConstraints = ok1(MaybeExistConstraints),
            MaybeConstructorArgs = ok1(ConstructorArgs),
            NoArgsSpecs = []
        then
            list.length(ConstructorArgs, Arity),
            Ctor = ctor(Ordinal, MaybeExistConstraints,
                Functor, ConstructorArgs, Arity, MainTermContext),
            MaybeConstructor = ok1(Ctor)
        else
            Specs = get_any_errors1(MaybeMaybeExistConstraints) ++
                get_any_errors1(MaybeConstructorArgs) ++ NoArgsSpecs,
            MaybeConstructor = error1(Specs)
        )
    ).

:- pred get_existential_constraints_from_term(module_name::in, varset::in,
    term::in, term::out, maybe1(list(prog_constraint))::out) is det.

get_existential_constraints_from_term(ModuleName, VarSet, !PredTypeTerm,
        MaybeExistentialConstraints) :-
    ( if
        !.PredTypeTerm = term.functor(term.atom("=>"),
            [!:PredTypeTerm, ExistentialConstraints], _)
    then
        parse_class_constraints(ModuleName, VarSet, ExistentialConstraints,
            MaybeExistentialConstraints)
    else
        MaybeExistentialConstraints = ok1([])
    ).

:- func convert_constructor_arg_list(module_name, varset, list(term)) =
    maybe1(list(constructor_arg)).

convert_constructor_arg_list(_, _, []) = ok1([]).
convert_constructor_arg_list(ModuleName, VarSet, [Term | Terms])
        = MaybeConstructorArgs :-
    ( if Term = term.functor(term.atom("::"), [NameTerm, TypeTerm], _) then
        ContextPieces = cord.singleton(words("In field name:")),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
            ContextPieces, NameTerm, MaybeSymNameAndArgs),
        (
            MaybeSymNameAndArgs = error2(Specs),
            MaybeConstructorArgs = error1(Specs)
        ;
            MaybeSymNameAndArgs = ok2(SymName, SymNameArgs),
            (
                SymNameArgs = [_ | _],
                % XXX Should we add "... at function symbol ..."?
                Pieces = [words("Error: syntax error in constructor name."),
                    nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybeConstructorArgs = error1([Spec])
            ;
                SymNameArgs = [],
                NameCtxt = get_term_context(NameTerm),
                MaybeCtorFieldName = yes(ctor_field_name(SymName, NameCtxt)),
                MaybeConstructorArgs =
                    convert_constructor_arg_list_2(ModuleName,
                        VarSet, MaybeCtorFieldName, TypeTerm, Terms)
            )
        )
    else
        MaybeCtorFieldName = no,
        TypeTerm = Term,
        MaybeConstructorArgs = convert_constructor_arg_list_2(ModuleName,
            VarSet, MaybeCtorFieldName, TypeTerm, Terms)
    ).

:- func convert_constructor_arg_list_2(module_name, varset,
    maybe(ctor_field_name), term, list(term)) = maybe1(list(constructor_arg)).

convert_constructor_arg_list_2(ModuleName, VarSet, MaybeCtorFieldName,
        TypeTerm, Terms) = MaybeArgs :-
    ContextPieces = cord.singleton(words("In type definition:")),
    parse_type(allow_ho_inst_info, VarSet, ContextPieces, TypeTerm, MaybeType),
    (
        MaybeType = ok1(Type),
        Context = get_term_context(TypeTerm),
        Arg = ctor_arg(MaybeCtorFieldName, Type, Context),
        MaybeTailArgs =
            convert_constructor_arg_list(ModuleName, VarSet, Terms),
        (
            MaybeTailArgs = error1(Specs),
            MaybeArgs  = error1(Specs)
        ;
            MaybeTailArgs = ok1(Args),
            MaybeArgs  = ok1([Arg | Args])
        )
    ;
        MaybeType = error1(Specs),
        MaybeArgs = error1(Specs)
    ).

:- pred check_supertype_vars(list(type_param)::in, varset::in, mer_type::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

check_supertype_vars(Params, VarSet, SuperType, Context, !Specs) :-
    type_vars_in_type(SuperType, VarsInSuperType0),
    list.sort_and_remove_dups(VarsInSuperType0, VarsInSuperType),
    list.delete_elems(VarsInSuperType, Params, FreeVars),
    (
        FreeVars = []
    ;
        FreeVars = [_ | _],
        varset.coerce(VarSet, GenericVarSet),
        FreeVarsStr = mercury_vars_to_name_only_vs(GenericVarSet, FreeVars),
        Pieces = [words("Error: free type"),
            words(choose_number(FreeVars, "parameter", "parameters")),
            words(FreeVarsStr),
            words("in supertype part of subtype definition."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    ).

:- pred process_du_ctors(list(type_param)::in, varset::in, term::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

process_du_ctors(_Params, _, _, [], !Specs).
process_du_ctors(Params, VarSet, BodyTerm, [Ctor | Ctors], !Specs) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, _CtorName, CtorArgs, _Arity,
        _Context),
    (
        MaybeExistConstraints = no_exist_constraints,
        ExistQVars = [],
        Constraints = []
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            _UnconstrainedExistQVars, _ConstrainedExistQVars)
    ),
    ( if
        % Check that all type variables in the ctor are either explicitly
        % existentially quantified, or occur in the head of the type.

        CtorArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        type_vars_in_types(CtorArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        ExistQVarsParams = ExistQVars ++ Params,
        list.filter(list.contains(ExistQVarsParams), VarsInCtorArgTypes,
            _ExistQOrParamVars, NotExistQOrParamVars),
        NotExistQOrParamVars = [_ | _]
    then
        % There should be no duplicate names to remove.
        varset.coerce(VarSet, GenericVarSet),
        NotExistQOrParamVarsStr =
            mercury_vars_to_name_only_vs(GenericVarSet, NotExistQOrParamVars),
        Pieces = [words("Error: free type"),
            words(choose_number(NotExistQOrParamVars,
                "parameter", "parameters")),
            words(NotExistQOrParamVarsStr),
            words("in right hand side of type definition."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(BodyTerm), Pieces),
        !:Specs = [Spec | !.Specs]
    else if
        % Check that no type variables in existential quantifiers occur
        % in the head. (Maybe this should just be a warning, not an error?
        % If we were to allow it, we would need to rename them apart.)

        set.list_to_set(ExistQVars, ExistQVarsSet),
        set.list_to_set(Params, ParamsSet),
        set.intersect(ExistQVarsSet, ParamsSet, ExistQParamsSet),
        set.is_non_empty(ExistQParamsSet)
    then
        % There should be no duplicate names to remove.
        set.to_sorted_list(ExistQParamsSet, ExistQParams),
        varset.coerce(VarSet, GenericVarSet),
        ExistQParamVarsStrs =
            list.map(mercury_var_to_name_only_vs(GenericVarSet), ExistQParams),
        Pieces = [words("Error:"),
            words(choose_number(ExistQParams,
                "type variable", "type variables"))] ++
            list_to_quoted_pieces(ExistQParamVarsStrs) ++
            [words(choose_number(ExistQParams, "has", "have")),
            words("overlapping scopes"),
            words("(the explicit existential type quantifier shadows"),
            words("the universal quantification implicit in"),
            words(choose_number(ExistQParams,
                "it being a type parameter", "them being type parameters")),
                suffix(")."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(BodyTerm), Pieces),
        !:Specs = [Spec | !.Specs]
    else if
        % Check that all type variables in existential quantifiers occur
        % somewhere in the constructor argument types or constraints.
        % XXX The actual check we would *want* to do is that they occur
        % somewhere in the constructor argument types or *in the range of
        % a functional dependency* in a constraint, since both of those
        % would bind a concrete type to the existential type variable,
        % but obviously this predicate has no access to the definitions
        % of the typeclasses mentioned in the constraints.

        CtorArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        type_vars_in_types(CtorArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        constraint_list_get_tvars(Constraints, ConstraintTVars),
        list.filter(list.contains(VarsInCtorArgTypes ++ ConstraintTVars),
            ExistQVars, _OccursExistQVars, NotOccursExistQVars),
        NotOccursExistQVars = [_ | _]
    then
        % There should be no duplicate names to remove.
        varset.coerce(VarSet, GenericVarSet),
        NotOccursExistQVarStrs =
            list.map(mercury_var_to_name_only_vs(GenericVarSet),
            NotOccursExistQVars),
        Pieces = [words("Error: the existentially quantified"),
            words(choose_number(NotOccursExistQVars,
                "type variable", "type variables"))] ++
            list_to_quoted_pieces(NotOccursExistQVarStrs) ++
            [words(choose_number(NotOccursExistQVars,
                "does not occur", "do not occur")),
            words("either in the arguments or in the constraints"),
            words("of the constructor."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(BodyTerm), Pieces),
        !:Specs = [Spec | !.Specs]
    else if
        % Check that all type variables in existential constraints occur in
        % the existential quantifiers.

        ConstraintArgTypeLists =
            list.map(prog_constraint_get_arg_types, Constraints),
        list.condense(ConstraintArgTypeLists, ConstraintArgTypes),
        type_vars_in_types(ConstraintArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        list.filter(list.contains(ExistQVars), VarsInCtorArgTypes,
            _ExistQArgTypes, NotExistQArgTypes),
        NotExistQArgTypes = [_ | _]
    then
        varset.coerce(VarSet, GenericVarSet),
        NotExistQArgTypeStrs = list.map(
            mercury_var_to_name_only_vs(GenericVarSet), NotExistQArgTypes),
        Pieces = [words("Error: the"),
            words(choose_number(NotExistQArgTypes,
                "type variable", "type variables"))]
            ++ list_to_quoted_pieces(NotExistQArgTypeStrs) ++
            [words(choose_number(NotExistQArgTypeStrs, "occurs", "occur")),
            words("in a class constraint"),
            words("without being explicitly existentially quantified"),
            words("using"), quote("some"), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(BodyTerm), Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    process_du_ctors(Params, VarSet, BodyTerm, Ctors, !Specs).

:- pred check_direct_arg_ctors(list(constructor)::in,
    list(sym_name_arity)::in, term::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_direct_arg_ctors(_Ctors, [], _ErrorTerm, !Specs).
check_direct_arg_ctors(Ctors, [DirectArgCtor | DirectArgCtors], ErrorTerm,
        !Specs) :-
    DirectArgCtor = sym_name_arity(SymName, Arity),
    ( if find_constructor(Ctors, SymName, Arity, Ctor) then
        Ctor = ctor(_Ordinal, MaybeExistConstraints, _SymName, _Args, _Arity,
            _Context),
        ( if Arity \= 1 then
            Pieces = [words("Error: the"), quote("direct_arg"),
                words("attribute contains a function symbol whose arity"),
                words("is not 1."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(ErrorTerm), Pieces),
            !:Specs = [Spec | !.Specs]
        else
            (
                MaybeExistConstraints = no_exist_constraints
            ;
                MaybeExistConstraints = exist_constraints(_),
                Pieces = [words("Error: the"), quote("direct_arg"),
                    words("attribute contains a function symbol"),
                    unqual_sym_name_arity(DirectArgCtor),
                    words("with existentially quantified type variables."),
                    nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(ErrorTerm), Pieces),
                !:Specs = [Spec | !.Specs]
            )
        )
    else
        Pieces = [words("Error: the"), quote("direct_arg"),
            words("attribute lists the function symbol"),
            unqual_sym_name_arity(DirectArgCtor),
            words("which is not in the type definition."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(ErrorTerm), Pieces),
        !:Specs = [Spec | !.Specs]
    ),
    check_direct_arg_ctors(Ctors, DirectArgCtors, ErrorTerm, !Specs).

:- pred find_constructor(list(constructor)::in, sym_name::in, arity::in,
    constructor::out) is semidet.

find_constructor([Ctor | Ctors], SymName, Arity, NamedCtor) :-
    ( if Ctor = ctor(_, _, SymName, _, Arity, _) then
        NamedCtor = Ctor
    else
        find_constructor(Ctors, SymName, Arity, NamedCtor)
    ).

%---------------------------------------------------------------------------%

    % parse_eqv_type_defn parses the definition of an equivalence type.
    %
:- pred parse_eqv_type_defn(module_name::in, varset::in, term::in, term::in,
    prog_context::in, item_seq_num::in, is_solver_type::in,
    maybe1(item_or_marker)::out) is det.

parse_eqv_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Context, SeqNum,
        IsSolverType, MaybeIOM) :-
    (
        IsSolverType = non_solver_type,
        SolverSpecs = []
    ;
        IsSolverType = solver_type,
        SolverPieces = [words("Error: a solver type cannot be defined"),
            words("to be equivalent to another type."), nl],
        SolverSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree,
            get_term_context(HeadTerm), SolverPieces),
        SolverSpecs = [SolverSpec]
    ),
    HeadContextPieces =
        cord.from_list([words("On the left hand side of type definition:")]),
    parse_type_defn_head(HeadContextPieces, ModuleName, VarSet, HeadTerm,
        MaybeNameAndParams),
    % XXX Should pass more correct BodyContextPieces.
    BodyContextPieces = cord.init,
    parse_type(no_allow_ho_inst_info(wnhii_eqv_type_defn_body), VarSet,
        BodyContextPieces, BodyTerm, MaybeType),
    ( if
        SolverSpecs = [],
        MaybeNameAndParams = ok2(Name, ParamTVars),
        MaybeType = ok1(Type)
    then
        varset.coerce(VarSet, TVarSet),
        check_no_free_body_vars(TVarSet, ParamTVars, Type,
            get_term_context(BodyTerm), FreeSpecs),
        (
            FreeSpecs = [],
            TypeDefn = parse_tree_eqv_type(DetailsEqv),
            DetailsEqv = type_details_eqv(Type),
            ItemTypeDefn = item_type_defn_info(Name, ParamTVars, TypeDefn,
                TVarSet, Context, SeqNum),
            Item = item_type_defn(ItemTypeDefn),
            MaybeIOM = ok1(iom_item(Item))
        ;
            FreeSpecs = [_ | _],
            MaybeIOM = error1(FreeSpecs)
        )
    else
        Specs = SolverSpecs ++
            get_any_errors2(MaybeNameAndParams) ++
            get_any_errors1(MaybeType),
        MaybeIOM = error1(Specs)
    ).

%---------------------------------------------------------------------------%

    % Parse a type definition which consists only of a `where' block.
    % This can be
    %
    % - an abstract enumeration type,
    % - an abstract dummy type (NYI),
    % - an abstract notag type (NYI),
    % - an abstract subtype, or
    % - a solver type.
    %
:- pred parse_where_block_type_defn(module_name::in, varset::in, term::in,
    term::in, prog_context::in, item_seq_num::in, is_solver_type::in,
    maybe1(item_or_marker)::out) is det.

parse_where_block_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm,
        Context, SeqNum, IsSolverType, MaybeIOM) :-
    (
        IsSolverType = non_solver_type,
        parse_where_type_is_abstract(ModuleName, VarSet, HeadTerm,
            BodyTerm, Context, SeqNum, MaybeIOM)
    ;
        IsSolverType = solver_type,
        parse_type_decl_where_term(solver_type, ModuleName, SeqNum,
            VarSet, BodyTerm, MaybeWhere),
        (
            MaybeWhere = error3(Specs),
            MaybeIOM = error1(Specs)
        ;
            MaybeWhere = ok3(MaybeSolverTypeDetails, MaybeCanonical,
                MaybeDirectArgCtors),
            (
                MaybeDirectArgCtors = yes(_),
                Pieces = [words("Error: solver type definitions"),
                    words("cannot have a"), quote("direct_arg"),
                    words("attribute."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(HeadTerm), Pieces),
                MaybeIOM = error1([Spec])
            ;
                MaybeDirectArgCtors = no,
                parse_solver_type_base(ModuleName, VarSet, HeadTerm,
                    MaybeSolverTypeDetails, MaybeCanonical, Context, SeqNum,
                    MaybeIOM)
            )
        )
    ).

:- pred parse_where_type_is_abstract(module_name::in, varset::in,
    term::in, term::in, prog_context::in, item_seq_num::in,
    maybe1(item_or_marker)::out) is det.

parse_where_type_is_abstract(ModuleName, VarSet, HeadTerm, BodyTerm,
        Context, SeqNum, MaybeIOM) :-
    ContextPieces =
        cord.from_list([words("On the left hand side of type definition:")]),
    varset.coerce(VarSet, TypeVarSet),
    parse_type_defn_head(ContextPieces, ModuleName, VarSet, HeadTerm,
        MaybeNameParams),
    ( if
        BodyTerm = term.functor(term.atom(AttrName), Args, _),
        ( AttrName = "type_is_abstract_enum"
        ; AttrName = "type_is_representable_in_n_bits"
        )
    then
        ( if Args = [Arg] then
            ( if term_int.decimal_term_to_int(Arg, NumBits) then
                TypeDefn0 = parse_tree_abstract_type(
                    abstract_type_fits_in_n_bits(NumBits)),
                MaybeTypeDefn = ok1(TypeDefn0)
            else
                Pieces = [words("Error: the argument of"), quote(AttrName),
                    words("is not a positive integer."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, Context, Pieces),
                MaybeTypeDefn = error1([Spec])
            )
        else
            Pieces = [words("Error:"), quote(AttrName),
                words("should have exactly one argument."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            MaybeTypeDefn = error1([Spec])
        )
    else if
        BodyTerm = term.functor(term.atom(AttrName), Args, _),
        AttrName = "type_is_abstract_subtype"
    then
        ( if Args = [Arg] then
            ( if parse_sym_name_and_arity(Arg, SymName, Arity) then
                TypeCtor = type_ctor(SymName, Arity),
                TypeDefn0 = parse_tree_abstract_type(
                    abstract_subtype(TypeCtor)),
                MaybeTypeDefn = ok1(TypeDefn0)
            else
                Pieces = [words("Error: the argument of"), quote(AttrName),
                    words("is not a symbol name and arity."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, Context, Pieces),
                MaybeTypeDefn = error1([Spec])
            )
        else
            Pieces = [words("Error:"), quote(AttrName),
                words("should have exactly one argument."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            MaybeTypeDefn = error1([Spec])
        )
    else
        Pieces = [words("Error: invalid"), quote("where ..."),
            words("attribute for abstract non-solver type."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeTypeDefn = error1([Spec])
    ),
    ( if
        MaybeNameParams = ok2(Name, Params),
        MaybeTypeDefn = ok1(TypeDefn)
    then
        ItemTypeDefn = item_type_defn_info(Name, Params, TypeDefn,
            TypeVarSet, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn),
        MaybeIOM = ok1(iom_item(Item))
    else
        Specs = get_any_errors2(MaybeNameParams) ++
            get_any_errors1(MaybeTypeDefn),
        MaybeIOM = error1(Specs)
    ).

:- pred parse_solver_type_base(module_name::in, varset::in, term::in,
    maybe(solver_type_details)::in, maybe_canonical::in,
    prog_context::in, item_seq_num::in, maybe1(item_or_marker)::out) is det.

parse_solver_type_base(ModuleName, VarSet, HeadTerm,
        MaybeSolverTypeDetails, MaybeCanonical, Context, SeqNum, MaybeIOM) :-
    ContextPieces = cord.from_list([words("On the left hand side of"),
        words("solver type definition:")]),
    varset.coerce(VarSet, TVarSet),
    parse_type_defn_head(ContextPieces, ModuleName, VarSet, HeadTerm,
        MaybeNameParams),
    (
        MaybeSolverTypeDetails = yes(_),
        SolverSpecs = []
    ;
        MaybeSolverTypeDetails = no,
        Pieces = [words("Solver type with no solver_type_details."), nl],
        SolverSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(HeadTerm), Pieces),
        SolverSpecs = [SolverSpec]
    ),
    ( if
        MaybeNameParams = ok2(_SymName, ParamTVars0),
        MaybeSolverTypeDetails = yes(SolverTypeDetails0)
    then
        RepType = SolverTypeDetails0 ^ std_representation_type,
        check_no_free_body_vars(TVarSet, ParamTVars0, RepType, Context,
            FreeSpecs)
    else
        FreeSpecs = []
    ),
    ( if
        MaybeNameParams = ok2(SymName, ParamTVars),
        MaybeSolverTypeDetails = yes(SolverTypeDetails),
        FreeSpecs = []
    then
        TypeDefn = parse_tree_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, MaybeCanonical),
        ItemTypeDefn = item_type_defn_info(SymName, ParamTVars, TypeDefn,
            TVarSet, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn),
        MaybeIOM = ok1(iom_item(Item))
    else
        Specs = SolverSpecs ++ get_any_errors2(MaybeNameParams) ++ FreeSpecs,
        MaybeIOM = error1(Specs)
    ).

%---------------------------------------------------------------------------%
%
% Parse an abstract type definition.
%

:- pred parse_abstract_type_defn(module_name::in, varset::in, term::in,
    prog_context::in, item_seq_num::in, is_solver_type::in,
    maybe1(item_or_marker)::out) is det.

parse_abstract_type_defn(ModuleName, VarSet, HeadTerm, Context, SeqNum,
        IsSolverType, MaybeIOM) :-
    ContextPieces =
        cord.from_list([words("On the left hand side of type definition:")]),
    parse_type_defn_head(ContextPieces, ModuleName, VarSet, HeadTerm,
        MaybeTypeCtorAndArgs),
    (
        MaybeTypeCtorAndArgs = error2(Specs),
        MaybeIOM = error1(Specs)
    ;
        MaybeTypeCtorAndArgs = ok2(Name, Params),
        varset.coerce(VarSet, TypeVarSet),
        (
            IsSolverType = non_solver_type,
            % XXX TYPE_REPN This looks wrong to me (zs), because there are
            % non-solver types for whose abstract versions can be described
            % by more specific values of AbstractTypeDetails.
            AbstractTypeDetails = abstract_type_general
        ;
            IsSolverType = solver_type,
            AbstractTypeDetails = abstract_solver_type
        ),
        TypeDefn = parse_tree_abstract_type(AbstractTypeDetails),
        ItemTypeDefn = item_type_defn_info(Name, Params, TypeDefn,
            TypeVarSet, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn),
        MaybeIOM = ok1(iom_item(Item))
    ).

%---------------------------------------------------------------------------%
%
% Parse ... where ... clauses in type definitions. These clauses can specify
% type-specific unify and/or compare predicates for discriminated union types
% and solver type details for solver types.
%

:- pred parse_type_decl_where_term(is_solver_type::in, module_name::in,
    item_seq_num::in, varset::in, term::in,
    maybe3(maybe(solver_type_details), maybe_canonical,
        maybe(list(sym_name_arity)))::out) is det.

parse_type_decl_where_term(IsSolverType, ModuleName, SeqNum, VarSet, Term0,
        MaybeWhereDetails) :-
    GroundContextPieces = cord.singleton(
        words("the ground inst of a solver type")),
    AnyContextPieces = cord.singleton(
        words("the any inst of a solver type")),
    some [!MaybeTerm] (
        !:MaybeTerm = yes(Term0),
        parse_where_attribute(parse_where_type_is_abstract_noncanonical,
            MaybeTypeIsAbstractNoncanonical, !MaybeTerm),
        parse_where_attribute(parse_where_is("representation",
                parse_where_type_is(ModuleName, VarSet)),
            MaybeRepresentationIs, !MaybeTerm),
        parse_where_attribute(parse_where_is("ground",
                parse_where_inst_is(ModuleName, VarSet, GroundContextPieces)),
            MaybeGroundIs, !MaybeTerm),
        parse_where_attribute(parse_where_is("any",
                parse_where_inst_is(ModuleName, VarSet, AnyContextPieces)),
            MaybeAnyIs, !MaybeTerm),
        parse_where_attribute(parse_where_is("constraint_store",
                parse_where_mutable_is(ModuleName, SeqNum, VarSet)),
            MaybeCStoreIs, !MaybeTerm),
        parse_where_attribute(parse_where_is("equality",
                parse_where_pred_is(ModuleName, VarSet)),
            MaybeEqualityIs, !MaybeTerm),
        parse_where_attribute(parse_where_is("comparison",
                parse_where_pred_is(ModuleName, VarSet)),
            MaybeComparisonIs, !MaybeTerm),
        % XXX TYPE_REPN We should stop accepting direct_arg information
        % in where clauses on the type definition itself after switch to
        % including direct_arg information in separate type_repn items instead.
        parse_where_attribute(parse_where_is("direct_arg",
                parse_where_direct_arg_is(ModuleName, VarSet)),
            MaybeDirectArgIs, !MaybeTerm),
        (
            !.MaybeTerm = no,
            MaybeEndSpec = ok1(unit)
        ;
            !.MaybeTerm = yes(EndTerm),
            Pieces = [words("Error: attributes are either badly ordered"),
                words("or contain an unrecognised attribute."), nl],
            EndSpec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(EndTerm), Pieces),
            MaybeEndSpec = error1([EndSpec])
        )
    ),
    MaybeWhereDetails = make_maybe_where_details(
        IsSolverType,
        MaybeTypeIsAbstractNoncanonical,
        MaybeRepresentationIs,
        MaybeGroundIs,
        MaybeAnyIs,
        MaybeCStoreIs,
        MaybeEqualityIs,
        MaybeComparisonIs,
        MaybeDirectArgIs,
        MaybeEndSpec,
        Term0
    ).

parse_where_unify_compare(ModuleName, VarSet, Term0, MaybeMaybeCanonical) :-
    some [!MaybeTerm] (
        !:MaybeTerm = yes(Term0),
        parse_where_attribute(parse_where_type_is_abstract_noncanonical,
            MaybeTypeIsAbstractNoncanonical, !MaybeTerm),
        parse_where_attribute(
            parse_where_is("equality",
                parse_where_pred_is(ModuleName, VarSet)),
            MaybeEqualityIs, !MaybeTerm),
        parse_where_attribute(
            parse_where_is("comparison",
                parse_where_pred_is(ModuleName, VarSet)),
            MaybeComparisonIs, !MaybeTerm),
        (
            !.MaybeTerm = no,
            MaybeWhereEnd = ok1(unit)
        ;
            !.MaybeTerm = yes(EndTerm),
            EndTermStr = describe_error_term(VarSet, EndTerm),
            Pieces = [
                words("In"), pragma_decl("foreign_type"),
                words("declaration: error: unrecognized"),
                quote("where"), words("attribute"), quote(EndTermStr),
                    suffix(".")
            ],
            VerbosePieces = [
                words("Recognized"), quote("where"),
                words("attributes have the form"),
                quote("equality is <<equality pred name>>"), words("and"),
                quote("comparison is <<comparison pred name>>"), suffix(".")
            ],
            EndSpec = error_spec($pred, severity_error,
                phase_term_to_parse_tree,
                [simple_msg(get_term_context(EndTerm),
                    [always(Pieces),
                    verbose_only(verbose_always, VerbosePieces)])]),
            MaybeWhereEnd = error1([EndSpec])
        )
    ),
    ( if
        MaybeTypeIsAbstractNoncanonical = ok1(TypeIsAbstractNoncanonical),
        MaybeEqualityIs = ok1(EqualityIs),
        MaybeComparisonIs = ok1(ComparisonIs),
        MaybeWhereEnd = ok1(_)
    then
        (
            TypeIsAbstractNoncanonical = yes(_),
            ( if
                EqualityIs = no,
                ComparisonIs = no
            then
                MaybeMaybeCanonical =
                    ok1(noncanon(noncanon_abstract(non_solver_type)))
            else
                Spec = abstract_noncanonical_excludes_others(Term0),
                MaybeMaybeCanonical = error1([Spec])
            )
        ;
            TypeIsAbstractNoncanonical = no,
            MaybeCanonical = maybe_unify_compare(EqualityIs, ComparisonIs),
            MaybeMaybeCanonical = ok1(MaybeCanonical)
        )
    else
        Specs =
            get_any_errors1(MaybeEqualityIs) ++
            get_any_errors1(MaybeComparisonIs) ++
            get_any_errors1(MaybeWhereEnd),
        MaybeMaybeCanonical = error1(Specs)
    ).

    % parse_where_attribute(Parser, Result, MaybeTerm, MaybeTailTerm) handles
    % - where MaybeTerm may contain nothing
    % - where MaybeTerm may be a comma-separated pair
    % - applies Parser to the appropriate (sub)term to obtain Result
    % - sets MaybeTailTerm depending upon whether the Result is an error or not
    %   and whether there is more to parse because MaybeTerm was a
    %   comma-separated pair.
    %
:- pred parse_where_attribute((func(term) = maybe1(maybe(T)))::in,
    maybe1(maybe(T))::out, maybe(term)::in, maybe(term)::out) is det.

parse_where_attribute(Parser, Result, MaybeTerm, MaybeTailTerm) :-
    (
        MaybeTerm = no,
        MaybeTailTerm = no,
        Result = ok1(no)
    ;
        MaybeTerm = yes(Term),
        ( if
            Term = term.functor(term.atom(","), [HeadTerm, TailTerm], _)
        then
            Result = Parser(HeadTerm),
            MaybeTailTermIfYes = yes(TailTerm)
        else
            Result = Parser(Term),
            MaybeTailTermIfYes = no
        ),
        (
            Result = error1(_),
            MaybeTailTerm = no
        ;
            Result = ok1(no),
            MaybeTailTerm = yes(Term)
        ;
            Result = ok1(yes(_)),
            MaybeTailTerm = MaybeTailTermIfYes
        )
    ).

    % Parser for `where ...' attributes of the form
    % `attributename is attributevalue'.
    %
:- func parse_where_is(string, func(term) = maybe1(T), term) =
    maybe1(maybe(T)).

parse_where_is(Name, Parser, Term) = Result :-
    ( if Term = term.functor(term.atom("is"), [LHS, RHS], _) then
        ( if LHS = term.functor(term.atom(Name), [], _) then
            RHSResult = Parser(RHS),
            (
                RHSResult = ok1(ParsedRHS),
                Result    = ok1(yes(ParsedRHS))
            ;
                RHSResult = error1(Specs),
                Result    = error1(Specs)
            )
        else
            Result = ok1(no)
        )
    else
        Pieces = [words("Error: expected"), quote("is"), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ).

:- func parse_where_type_is_abstract_noncanonical(term) = maybe1(maybe(unit)).

parse_where_type_is_abstract_noncanonical(Term) =
    ( if
        Term = term.functor(term.atom("type_is_abstract_noncanonical"), [], _)
    then
        ok1(yes(unit))
    else
        ok1(no)
    ).

:- func parse_where_pred_is(module_name, varset, term) = maybe1(sym_name).

parse_where_pred_is(ModuleName, VarSet, Term) = MaybeSymName :-
    parse_implicitly_qualified_symbol_name(ModuleName, VarSet, Term,
        MaybeSymName).

:- func parse_where_inst_is(module_name, varset, cord(format_piece), term)
    = maybe1(mer_inst).

parse_where_inst_is(_ModuleName, VarSet, ContextPieces, Term) = MaybeInst :-
    parse_inst(no_allow_constrained_inst_var(wnciv_solver_type_inst),
        VarSet, ContextPieces, Term, MaybeInst0),
    (
        MaybeInst0 = error1(Specs),
        MaybeInst = error1(Specs)
    ;
        MaybeInst0 = ok1(Inst),
        ( if inst_contains_unconstrained_var(Inst) then
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error:"), quote(TermStr),
                words("is not a ground, unconstrained inst."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, get_term_context(Term), Pieces),
            MaybeInst = error1([Spec])
        else
            MaybeInst = ok1(Inst)
        )
    ).

:- func parse_where_type_is(module_name, varset, term) = maybe1(mer_type).

parse_where_type_is(_ModuleName, VarSet, Term) = MaybeType :-
    % XXX We should pass meaningful ContextPieces.
    ContextPieces = cord.init,
    parse_type(no_allow_ho_inst_info(wnhii_solver_type_defn),
        VarSet, ContextPieces, Term, MaybeType).

:- func parse_where_mutable_is(module_name, item_seq_num, varset, term) =
    maybe1(list(item_mutable_info)).

parse_where_mutable_is(ModuleName, SeqNum, VarSet, Term) = MaybeItems :-
    ( if Term = term.functor(term.atom("mutable"), _, _) then
        parse_mutable_decl_term(ModuleName, SeqNum, VarSet, Term, MaybeItem),
        (
            MaybeItem = ok1(Mutable),
            MaybeItems  = ok1([Mutable])
        ;
            MaybeItem = error1(Specs),
            MaybeItems  = error1(Specs)
        )
    else if list_term_to_term_list(Term, Terms) then
        map_parser(parse_mutable_decl_term(ModuleName, SeqNum, VarSet),
            Terms, MaybeItems)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a mutable declaration"),
            words("or a list of mutable declarations, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeItems = error1([Spec])
    ).

:- pred parse_mutable_decl_term(module_name::in, item_seq_num::in,
    varset::in, term::in, maybe1(item_mutable_info)::out) is det.

parse_mutable_decl_term(ModuleName, SeqNum, VarSet, Term,
        MaybeItemMutableInfo) :-
    ( if
        Term = term.functor(term.atom("mutable"), ArgTerms, Context)
    then
        parse_mutable_decl_info(ModuleName, VarSet, ArgTerms, Context, SeqNum,
            mutable_locn_in_solver_type, MaybeItemMutableInfo)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected a mutable declaration, got"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeItemMutableInfo = error1([Spec])
    ).

:- func parse_where_direct_arg_is(module_name, varset, term) =
    maybe1(list(sym_name_arity)).

parse_where_direct_arg_is(ModuleName, VarSet, Term) = MaybeDirectArgCtors :-
    ( if list_term_to_term_list(Term, FunctorsTerms) then
        map_parser(parse_direct_arg_functor(ModuleName, VarSet),
            FunctorsTerms, MaybeDirectArgCtors)
    else
        Pieces = [words("Error: malformed functors list in"),
            quote("direct_arg"), words("attribute."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeDirectArgCtors = error1([Spec])
    ).

:- pred parse_direct_arg_functor(module_name::in, varset::in, term::in,
    maybe1(sym_name_arity)::out) is det.

parse_direct_arg_functor(ModuleName, VarSet, Term, MaybeFunctor) :-
    ( if parse_sym_name_and_arity(Term, SymName0, Arity) then
        implicitly_qualify_sym_name(ModuleName, Term, SymName0, MaybeSymName),
        (
            MaybeSymName = ok1(SymName),
            MaybeFunctor = ok1(sym_name_arity(SymName, Arity))
        ;
            MaybeSymName = error1(Specs),
            MaybeFunctor = error1(Specs)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected functor name/arity for"),
            quote("direct_arg"), words("attribute, not"),
            quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybeFunctor = error1([Spec])
    ).

:- func make_maybe_where_details(is_solver_type, maybe1(maybe(unit)),
    maybe1(maybe(mer_type)), maybe1(maybe(mer_inst)), maybe1(maybe(mer_inst)),
    maybe1(maybe(list(item_mutable_info))),
    maybe1(maybe(equality_pred)), maybe1(maybe(comparison_pred)),
    maybe1(maybe(list(sym_name_arity))),
    maybe1(unit), term)
    = maybe3(maybe(solver_type_details), maybe_canonical,
        maybe(list(sym_name_arity))).

make_maybe_where_details(IsSolverType, MaybeTypeIsAbstractNoncanonical,
        MaybeRepresentationIs, MaybeGroundIs, MaybeAnyIs, MaybeCStoreIs,
        MaybeEqualityIs, MaybeComparisonIs, MaybeDirectArgIs,
        MaybeWhereEnd, WhereTerm) = MaybeWhereDetails :-
    ( if
        MaybeTypeIsAbstractNoncanonical = ok1(TypeIsAbstractNoncanonical),
        MaybeRepresentationIs = ok1(RepresentationIs),
        MaybeGroundIs = ok1(GroundIs),
        MaybeAnyIs = ok1(AnyIs),
        MaybeCStoreIs = ok1(CStoreIs),
        MaybeEqualityIs = ok1(EqualityIs),
        MaybeComparisonIs = ok1(ComparisonIs),
        MaybeDirectArgIs = ok1(DirectArgIs),
        MaybeWhereEnd = ok1(_WhereEnd)
    then
        MaybeWhereDetails = make_maybe_where_details_2(IsSolverType,
            TypeIsAbstractNoncanonical, RepresentationIs,
            GroundIs, AnyIs, CStoreIs, EqualityIs, ComparisonIs, DirectArgIs,
            WhereTerm)
    else
        Specs =
            get_any_errors1(MaybeTypeIsAbstractNoncanonical) ++
            get_any_errors1(MaybeRepresentationIs) ++
            get_any_errors1(MaybeGroundIs) ++
            get_any_errors1(MaybeAnyIs) ++
            get_any_errors1(MaybeCStoreIs) ++
            get_any_errors1(MaybeEqualityIs) ++
            get_any_errors1(MaybeComparisonIs) ++
            get_any_errors1(MaybeDirectArgIs) ++
            get_any_errors1(MaybeWhereEnd),
        MaybeWhereDetails = error3(Specs)
    ).

:- func make_maybe_where_details_2(is_solver_type, maybe(unit),
    maybe(mer_type), maybe(mer_inst), maybe(mer_inst),
    maybe(list(item_mutable_info)),
    maybe(equality_pred), maybe(comparison_pred),
    maybe(list(sym_name_arity)), term)
    = maybe3(maybe(solver_type_details), maybe_canonical,
        maybe(list(sym_name_arity))).

make_maybe_where_details_2(IsSolverType, TypeIsAbstractNoncanonical,
        RepresentationIs, GroundIs, AnyIs, CStoreIs,
        EqualityIs, ComparisonIs, DirectArgIs, WhereTerm)
        = MaybeWhereDetails :-
    (
        TypeIsAbstractNoncanonical = yes(_),
        % rafe: XXX I think this is wrong. There isn't a problem with having
        % the solver_type_details and type_is_abstract_noncanonical.
        ( if
            RepresentationIs = maybe.no,
            GroundIs         = maybe.no,
            AnyIs            = maybe.no,
            EqualityIs       = maybe.no,
            ComparisonIs     = maybe.no,
            CStoreIs         = maybe.no,
            DirectArgIs      = maybe.no
        then
            MaybeWhereDetails =
                ok3(no, noncanon(noncanon_abstract(IsSolverType)), no)
        else
            Spec = abstract_noncanonical_excludes_others(WhereTerm),
            MaybeWhereDetails = error3([Spec])
        )
    ;
        TypeIsAbstractNoncanonical = maybe.no,
        (
            IsSolverType = solver_type,
            ( if
                DirectArgIs = yes(_)
            then
                Pieces = [words("Error: solver type definitions cannot have"),
                    quote("direct_arg"), words("attributes."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(WhereTerm), Pieces),
                MaybeWhereDetails = error3([Spec])
            else if
                RepresentationIs = yes(RepnType),
                GroundIs         = MaybeGroundInst,
                AnyIs            = MaybeAnyInst,
                EqualityIs       = MaybeEqPred,
                ComparisonIs     = MaybeCmpPred,
                CStoreIs         = MaybeMutableInfos
            then
                (
                    MaybeGroundInst = yes(GroundInst)
                ;
                    MaybeGroundInst = no,
                    GroundInst = ground_inst
                ),
                (
                    MaybeAnyInst = yes(AnyInst)
                ;
                    MaybeAnyInst = no,
                    AnyInst = ground_inst
                ),
                (
                    MaybeMutableInfos = yes(MutableInfos)
                ;
                    MaybeMutableInfos = no,
                    MutableInfos = []
                ),
                SolverTypeDetails = solver_type_details(RepnType,
                    GroundInst, AnyInst, MutableInfos),
                MaybeSolverTypeDetails = yes(SolverTypeDetails),
                MaybeCanonical =
                    maybe_unify_compare(MaybeEqPred, MaybeCmpPred),
                MaybeWhereDetails = ok3(MaybeSolverTypeDetails,
                    MaybeCanonical, no)
            else if
                RepresentationIs = no
            then
                Pieces = [words("Error: solver type definitions must have a"),
                    quote("representation"), words("attribute."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(WhereTerm), Pieces),
                MaybeWhereDetails = error3([Spec])
            else
               unexpected($pred, "shouldn't have reached this point! (1)")
            )
        ;
            IsSolverType = non_solver_type,
            ( if
                ( RepresentationIs = yes(_)
                ; GroundIs         = yes(_)
                ; AnyIs            = yes(_)
                ; CStoreIs         = yes(_)
                )
            then
                Pieces = [words("Error: solver type attribute given"),
                    words("for non-solver type."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree,
                    get_term_context(WhereTerm), Pieces),
                MaybeWhereDetails = error3([Spec])
            else
                MaybeCanonical = maybe_unify_compare(EqualityIs, ComparisonIs),
                MaybeWhereDetails = ok3(no, MaybeCanonical, DirectArgIs)
            )
        )
    ).

:- func abstract_noncanonical_excludes_others(term) = error_spec.

abstract_noncanonical_excludes_others(Term) = Spec :-
    Pieces = [words("Error:"),
        quote("where type_is_abstract_noncanonical"),
        words("excludes other"), quote("where ..."),
        words("attributes."), nl],
    Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
        get_term_context(Term), Pieces).

:- func maybe_unify_compare(maybe(equality_pred), maybe(comparison_pred))
    = maybe_canonical.

maybe_unify_compare(MaybeUniPred, MaybeCmpPred) = MaybeCanonical :-
    (
        MaybeUniPred = no,
        MaybeCmpPred = no,
        MaybeCanonical = canon
    ;
        MaybeUniPred = no,
        MaybeCmpPred = yes(CmpPred),
        MaybeCanonical = noncanon(noncanon_cmp_only(CmpPred))
    ;
        MaybeUniPred = yes(UniPred),
        MaybeCmpPred = yes(CmpPred),
        MaybeCanonical = noncanon(noncanon_uni_cmp(UniPred, CmpPred))
    ;
        MaybeUniPred = yes(UniPred),
        MaybeCmpPred = no,
        MaybeCanonical = noncanon(noncanon_uni_only(UniPred))
    ).

%---------------------------------------------------------------------------%
%
% Predicates useful for parsing several kinds of type definitions.
%

parse_type_defn_head(ContextPieces, ModuleName, VarSet, Term,
        MaybeTypeCtorAndArgs) :-
    (
        Term = term.variable(_Var, Context),
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++
            [lower_case_next_if_not_first,
            words("Error: expected a type constructor"),
            words("and zero or more type variables as arguments,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        MaybeTypeCtorAndArgs = error2([Spec])
    ;
        Term = term.functor(_Functor, _ArgTerms, Context),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, VarSet,
            ContextPieces, Term, MaybeSymNameArgs),
        (
            MaybeSymNameArgs = error2(Specs),
            MaybeTypeCtorAndArgs = error2(Specs)
        ;
            MaybeSymNameArgs = ok2(SymName, ArgTerms),
            % Check that SymName is allowed to be a type constructor name.
            check_user_type_name(SymName, Context, NameSpecs),
            ( if
                unqualify_name(SymName) = "=<",
                list.length(ArgTerms, 2)
            then
                % This looks like an incorrect subtype definition so do not
                % suggest that the arguments must be variables.
                MaybeTypeCtorAndArgs = error2(NameSpecs)
            else
                % Check that all the ArgTerms are variables.
                term_list_to_var_list_and_nonvars(ArgTerms, ParamVars,
                    NonVarArgTerms),
                (
                    NonVarArgTerms = [],
                    % Check that all the ParamVars are distinct.
                    bag.from_list(ParamVars, ParamsBag),
                    bag.to_list_only_duplicates(ParamsBag, DupParamVars),
                    (
                        DupParamVars = [],
                        (
                            NameSpecs = [],
                            list.map(term.coerce_var, ParamVars, PrgParamVars),
                            MaybeTypeCtorAndArgs = ok2(SymName, PrgParamVars)
                        ;
                            NameSpecs = [_ | _],
                            MaybeTypeCtorAndArgs = error2(NameSpecs)
                        )
                    ;
                        DupParamVars = [_ | _],
                        DupParamVarNames = list.map(
                            mercury_var_to_name_only_vs(VarSet), DupParamVars),
                        Params = choose_number(DupParamVars,
                            "the parameter", "the parameters"),
                        IsOrAre = is_or_are(DupParamVars),
                        Pieces = cord.list(ContextPieces) ++
                            [lower_case_next_if_not_first,
                            words("Error: type parameters must be unique,"),
                            words("but"), words(Params)] ++
                            list_to_pieces(DupParamVarNames) ++
                            [words(IsOrAre), words("duplicated."), nl],
                        Spec = simplest_spec($pred, severity_error,
                            phase_term_to_parse_tree, Context, Pieces),
                        MaybeTypeCtorAndArgs = error2([Spec | NameSpecs])
                    )
                ;
                    NonVarArgTerms = [_ | _],
                    NonVarArgTermStrs = list.map(describe_error_term(VarSet),
                        NonVarArgTerms),
                    IsOrAre = is_or_are(NonVarArgTermStrs),
                    Pieces = cord.list(ContextPieces) ++
                        [lower_case_next_if_not_first,
                        words("Error: type parameters must be variables,"),
                        words("but")] ++
                        list_to_quoted_pieces(NonVarArgTermStrs) ++
                        [words(IsOrAre), words("not."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_term_to_parse_tree, Context, Pieces),
                    MaybeTypeCtorAndArgs = error2([Spec | NameSpecs])
                )
            )
        )
    ).

:- pred term_list_to_var_list_and_nonvars(list(term(T))::in,
    list(var(T))::out, list(term(T))::out) is det.

term_list_to_var_list_and_nonvars([], [], []).
term_list_to_var_list_and_nonvars([Term | Terms], Vars, NonVars) :-
    term_list_to_var_list_and_nonvars(Terms, TailVars, TailNonVars),
    (
        Term = variable(Var, _),
        Vars = [Var | TailVars],
        NonVars = TailNonVars
    ;
        Term = functor(_, _, _),
        Vars = TailVars,
        NonVars = [Term | TailNonVars]
    ).

    % Check that the type name is available to users.
    %
:- pred check_user_type_name(sym_name::in, term.context::in,
    list(error_spec)::out) is det.

check_user_type_name(SymName, Context, NameSpecs) :-
    % Check that the mode name is available to users.
    Name = unqualify_name(SymName),
    ( if Name = "=" then
        NamePieces = [words("Error: in definitions of equivalence types,"),
            words("the type name must be followed by"),
            quote("=="), suffix(","),
            words("not"), quote("="), suffix("."), nl],
        NameSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, Context, NamePieces),
        NameSpecs = [NameSpec]
    else if is_known_type_name(Name) then
        NamePieces = [words("Error: the type name"), quote(Name),
            words("is reserved for the Mercury implementation."), nl],
        NameSpec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, Context, NamePieces),
        NameSpecs = [NameSpec]
    else
        NameSpecs = []
    ).

    % Check that all the variables in the body occur in the head.
    % Return a nonempty list of error specs if some do.
    %
:- pred check_no_free_body_vars(tvarset::in, list(tvar)::in, mer_type::in,
    prog_context::in, list(error_spec)::out) is det.

check_no_free_body_vars(TVarSet, ParamTVars, BodyType, BodyContext, Specs) :-
    % Check that all the variables in the body occur in the head.
    set_of_type_vars_in_type(BodyType, BodyTVarSet),
    set.list_to_set(ParamTVars, ParamTVarSet),
    set.difference(BodyTVarSet, ParamTVarSet, OnlyBodyTVarSet),
    set.to_sorted_list(OnlyBodyTVarSet, OnlyBodyTVars),
    (
        OnlyBodyTVars = [],
        Specs = []
    ;
        OnlyBodyTVars = [_ | _],
        OnlyBodyTVarNames = list.map(mercury_var_to_name_only_vs(TVarSet),
            OnlyBodyTVars),
        VarWord = choose_number(OnlyBodyTVars,
            "the type variable", "the type variables"),
        OccurWord = choose_number(OnlyBodyTVars,
            "occurs", "occur"),
        Pieces = [words("Error:"), words(VarWord)] ++
            list_to_pieces(OnlyBodyTVarNames) ++ [words(OccurWord),
            words("only in the right hand side of this type definition."),
            nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            BodyContext, Pieces),
        Specs = [Spec]
    ).

%-----------------------------------------------------------------------------e

:- pred parse_supertype(varset::in, cord(format_piece)::in, term::in,
    maybe1(maybe_subtype)::out) is det.

parse_supertype(VarSet, ContextPieces, Term, Result) :-
    parse_type(no_allow_ho_inst_info(wnhii_supertype), VarSet,
        ContextPieces, Term, MaybeType),
    (
        MaybeType = ok1(Type),
        ( if type_to_ctor_and_args(Type, _TypeCtor, _Args) then
            Result = ok1(subtype_of(Type))
        else
            Context = get_term_context(Term),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: expected a type constructor,"),
                words("got"), quote(TermStr), suffix("."), nl],
            Spec = simplest_spec($pred, severity_error,
                phase_term_to_parse_tree, Context, Pieces),
            Result = error1([Spec])
        )
    ;
        MaybeType = error1(Specs),
        Result = error1(Specs)
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.parse_type_defn.
%-----------------------------------------------------------------------------e
