%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2008-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_type_defn.m.
%
% This module parses type definitions.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_type_defn.

:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

    % Parse the definition of a type.
    %
:- pred parse_type_defn(module_name::in, varset::in, term::in, decl_attrs::in,
    prog_context::in, int::in, maybe1(item)::out) is det.

    % parse_type_defn_head(ModuleName, VarSet, Head, HeadResult):
    %
    % Check the head of a type definition for errors.
    %
:- pred parse_type_defn_head(module_name::in, varset::in, term::in,
    maybe2(sym_name, list(type_param))::out) is det.

    % parse_type_decl_where_part_if_present(TypeSymName, Arity,
    %   IsSolverType, Inst, ModuleName, Term0, Term, Result):
    %
    % Checks if Term0 is a term of the form `<body> where <attributes>'.
    % If so, returns the `<body>' in Term and the parsed `<attributes>'
    % in Result. If not, returns Term = Term0 and Result = no.
    %
:- pred parse_type_decl_where_part_if_present(is_solver_type::in,
    module_name::in, varset::in, term::in, term::out,
    maybe3(maybe(solver_type_details), maybe(unify_compare),
        maybe(list(sym_name_and_arity)))::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_mutable.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_io_typeclass.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module unit.

parse_type_defn(ModuleName, VarSet, TypeDefnTerm, Attributes, Context,
        SeqNum, MaybeItem) :-
    (
        TypeDefnTerm = term.functor(term.atom(Name), ArgTerms, _),
        ArgTerms = [HeadTerm, BodyTerm],
        ( Name = "--->"
        ; Name = "=="
        ; Name = "where"
        )
    ->
        parse_condition_suffix(BodyTerm, BeforeCondTerm, Condition),
        (
            Name = "--->",
            parse_du_type_defn(ModuleName, VarSet,
                HeadTerm, BeforeCondTerm, Attributes,
                Condition, Context, SeqNum, MaybeItem)
        ;
            Name = "==",
            parse_eqv_type_defn(ModuleName, VarSet,
                HeadTerm, BeforeCondTerm, Attributes,
                Condition, Context, SeqNum, MaybeItem)
        ;
            Name = "where",
            parse_where_block_type_defn(ModuleName, VarSet,
                HeadTerm, BeforeCondTerm, Attributes,
                Condition, Context, SeqNum, MaybeItem)
        )
    ;
        parse_abstract_type_defn(ModuleName, VarSet, TypeDefnTerm, Attributes,
            Condition, Context, SeqNum, MaybeItem),
        Condition = cond_true
    ).

%-----------------------------------------------------------------------------%
%
% Code dealing with definitions of discriminated union types.
%

    % parse_du_type_defn parses the definition of a discriminated union type.
    %
:- pred parse_du_type_defn(module_name::in, varset::in, term::in, term::in,
    decl_attrs::in, condition::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_du_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Attributes0,
        Condition, Context, SeqNum, MaybeItem) :-
    get_is_solver_type(IsSolverType, Attributes0, Attributes),
    (
        IsSolverType = solver_type,
        Pieces = [words("Error: a solver type"),
            words("cannot have data constructors."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ;
        IsSolverType = non_solver_type,
        parse_type_defn_head(ModuleName, VarSet, HeadTerm,
            MaybeTypeCtorAndArgs),
        du_type_rhs_ctors_and_where_terms(BodyTerm, CtorsTerm, MaybeWhereTerm),
        MaybeCtors = parse_constructors(ModuleName, VarSet, CtorsTerm),
        MaybeWhere = parse_type_decl_where_term(non_solver_type,
            ModuleName, VarSet, MaybeWhereTerm),
        % The code to process `where' attributes will return an error
        % if solver attributes are given for a non-solver type. Because
        % this is a du type, if the unification with MaybeWhere succeeds
        % then _NoSolverTypeDetails is guaranteed to be `no'.
        (
            MaybeTypeCtorAndArgs = ok2(Name, Params),
            MaybeCtors = ok1(Ctors),
            MaybeWhere = ok3(_NoSolverTypeDetails, MaybeUserEqComp,
                MaybeDirectArgIs)
        ->
            process_du_ctors(Params, VarSet, BodyTerm, Ctors, [], CtorsSpecs),
            (
                MaybeDirectArgIs = yes(DirectArgCtors),
                check_direct_arg_ctors(Ctors, DirectArgCtors, BodyTerm,
                    CtorsSpecs, ErrorSpecs)
            ;
                MaybeDirectArgIs = no,
                ErrorSpecs = CtorsSpecs
            ),
            (
                ErrorSpecs = [],
                varset.coerce(VarSet, TypeVarSet),
                TypeDefn = parse_tree_du_type(Ctors, MaybeUserEqComp,
                    MaybeDirectArgIs),
                ItemTypeDefn = item_type_defn_info(TypeVarSet, Name,
                    Params, TypeDefn, Condition, Context, SeqNum),
                Item = item_type_defn(ItemTypeDefn),
                MaybeItem0 = ok1(Item),
                check_no_attributes(MaybeItem0, Attributes, MaybeItem)
            ;
                ErrorSpecs = [_ | _],
                MaybeItem = error1(ErrorSpecs)
            )
        ;
            Specs = get_any_errors2(MaybeTypeCtorAndArgs) ++
                get_any_errors1(MaybeCtors) ++ get_any_errors3(MaybeWhere),
            MaybeItem = error1(Specs)
        )
    ).

:- pred du_type_rhs_ctors_and_where_terms(term::in,
    term::out, maybe(term)::out) is det.

du_type_rhs_ctors_and_where_terms(Term, CtorsTerm, MaybeWhereTerm) :-
    (
        Term = term.functor(term.atom("where"), Args, _Context),
        Args = [CtorsTerm0, WhereTerm]
    ->
        CtorsTerm      = CtorsTerm0,
        MaybeWhereTerm = yes(WhereTerm)
    ;
        CtorsTerm      = Term,
        MaybeWhereTerm = no
    ).

    % Convert a list of terms separated by semi-colons (known as a
    % "disjunction", even thought the terms aren't goals in this case)
    % into a list of constructors.
    %
:- func parse_constructors(module_name, varset, term) =
    maybe1(list(constructor)).

parse_constructors(ModuleName, VarSet, Term) = MaybeConstructors :-
    disjunction_to_list(Term, BodyTermList),
    MaybeConstructors = parse_constructors_2(ModuleName, VarSet, BodyTermList).

    % True if the term is a valid list of constructors.
    %
:- func parse_constructors_2(module_name, varset, list(term)) =
    maybe1(list(constructor)).

parse_constructors_2(_ModuleName, _, []) = ok1([]).
parse_constructors_2(ModuleName, VarSet, [Head | Tail]) = MaybeConstructors :-
    MaybeHeadConstructor = parse_constructor(ModuleName, VarSet, Head),
    MaybeTailConstructors = parse_constructors_2(ModuleName, VarSet, Tail),
    (
        MaybeHeadConstructor = ok1(HeadConstructor),
        MaybeTailConstructors = ok1(TailConstructors)
    ->
        Constructors = [HeadConstructor | TailConstructors],
        MaybeConstructors = ok1(Constructors)
    ;
        Specs = get_any_errors1(MaybeHeadConstructor) ++
            get_any_errors1(MaybeTailConstructors),
        MaybeConstructors = error1(Specs)
    ).

:- func parse_constructor(module_name, varset, term) = maybe1(constructor).

parse_constructor(ModuleName, VarSet, Term) = MaybeConstructor :-
    ( Term = term.functor(term.atom("some"), [VarsTerm, SubTerm], _) ->
        ( parse_list_of_vars(VarsTerm, ExistQVars) ->
            list.map(term.coerce_var, ExistQVars, ExistQTVars),
            MaybeConstructor = parse_constructor_2(ModuleName, VarSet,
                ExistQTVars, SubTerm)
        ;
            TermStr = describe_error_term(VarSet, Term),
            Pieces = [words("Error: syntax error in variable list at"),
                words(TermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(VarsTerm), [always(Pieces)])]),
            MaybeConstructor = error1([Spec])
        )
    ;
        ExistQVars = [],
        MaybeConstructor = parse_constructor_2(ModuleName, VarSet, ExistQVars,
            Term)
    ).

:- func parse_constructor_2(module_name, varset, list(tvar), term) =
    maybe1(constructor).

parse_constructor_2(ModuleName, VarSet, ExistQVars, Term) = MaybeConstructor :-
    get_existential_constraints_from_term(ModuleName, VarSet, Term,
        BeforeConstraintsTerm, MaybeConstraints),
    (
        MaybeConstraints = error1(Specs),
        MaybeConstructor = error1(Specs)
    ;
        MaybeConstraints = ok1(Constraints),
        (
            % Note that as a special case, one level of curly braces around
            % the constructor are ignored. This is to allow you to define
            % ';'/2 and 'some'/2 constructors.
            BeforeConstraintsTerm = term.functor(term.atom("{}"),
                [InsideBracesTerm], _Context)
        ->
            MainTerm = InsideBracesTerm
        ;
            MainTerm = BeforeConstraintsTerm
        ),
        ContextPieces = [words("In constructor definition:")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, MainTerm,
            VarSet, ContextPieces, MaybeFunctorAndArgTerms),
        (
            MaybeFunctorAndArgTerms = error2(Specs),
            MaybeConstructor  = error1(Specs)
        ;
            MaybeFunctorAndArgTerms = ok2(Functor, ArgTerms),
            MaybeConstructorArgs = convert_constructor_arg_list(ModuleName,
                VarSet, ArgTerms),
            (
                MaybeConstructorArgs = error1(Specs),
                MaybeConstructor = error1(Specs)
            ;
                MaybeConstructorArgs = ok1(ConstructorArgs),
                Ctor = ctor(ExistQVars, Constraints, Functor, ConstructorArgs,
                    get_term_context(MainTerm)),
                MaybeConstructor = ok1(Ctor)
            )
        )
    ).

:- pred get_existential_constraints_from_term(module_name::in, varset::in,
    term::in, term::out, maybe1(list(prog_constraint))::out) is det.

get_existential_constraints_from_term(ModuleName, VarSet, !PredTypeTerm,
        MaybeExistentialConstraints) :-
    (
        !.PredTypeTerm = term.functor(term.atom("=>"),
            [!:PredTypeTerm, ExistentialConstraints], _)
    ->
        parse_class_constraints(ModuleName, VarSet, ExistentialConstraints,
            MaybeExistentialConstraints)
    ;
        MaybeExistentialConstraints = ok1([])
    ).

:- func convert_constructor_arg_list(module_name, varset, list(term)) =
    maybe1(list(constructor_arg)).

convert_constructor_arg_list(_, _, []) = ok1([]).
convert_constructor_arg_list(ModuleName, VarSet, [Term | Terms])
        = MaybeConstructorArgs :-
    ( Term = term.functor(term.atom("::"), [NameTerm, TypeTerm], _) ->
        ContextPieces = [words("In field name:")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, NameTerm,
            VarSet, ContextPieces, MaybeSymNameAndArgs),
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
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
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
    ;
        MaybeCtorFieldName = no,
        TypeTerm = Term,
        MaybeConstructorArgs = convert_constructor_arg_list_2(ModuleName,
            VarSet, MaybeCtorFieldName, TypeTerm, Terms)
    ).

:- func convert_constructor_arg_list_2(module_name, varset,
    maybe(ctor_field_name), term, list(term)) = maybe1(list(constructor_arg)).

convert_constructor_arg_list_2(ModuleName, VarSet, MaybeCtorFieldName,
        TypeTerm, Terms) = MaybeArgs :-
    ContextPieces = [words("In type definition:")],
    parse_type(TypeTerm, VarSet, ContextPieces, MaybeType),
    (
        MaybeType = ok1(Type),
        Context = get_term_context(TypeTerm),
        % Initially every argument is assumed to occupy one word.
        Arg = ctor_arg(MaybeCtorFieldName, Type, full_word, Context),
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

:- pred process_du_ctors(list(type_param)::in, varset::in, term::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

process_du_ctors(_Params, _, _, [], !Specs).
process_du_ctors(Params, VarSet, BodyTerm, [Ctor | Ctors], !Specs) :-
    Ctor = ctor(ExistQVars, Constraints, _CtorName, CtorArgs, _Context),
    (
        % Check that all type variables in the ctor are either explicitly
        % existentially quantified or occur in the head of the type.

        CtorArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        type_vars_list(CtorArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        list.filter(list.contains(ExistQVars ++ Params), VarsInCtorArgTypes,
            _ExistQOrParamVars, NotExistQOrParamVars),
        NotExistQOrParamVars = [_ | _]
    ->
        % There should be no duplicate names to remove.
        varset.coerce(VarSet, GenericVarSet),
        NotExistQOrParamVarsStr =
            mercury_vars_to_string(GenericVarSet, no, NotExistQOrParamVars),
        Pieces = [words("Error: free type"),
            words(choose_number(NotExistQOrParamVars,
                "parameter", "parameters")),
            words(NotExistQOrParamVarsStr),
            words("in RHS of type definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        % Check that all type variables in existential quantifiers do not
        % occur in the head (maybe this should just be a warning, not an error?
        % If we were to allow it, we would need to rename them apart.)

        set.list_to_set(ExistQVars, ExistQVarsSet),
        set.list_to_set(Params, ParamsSet),
        set.intersect(ExistQVarsSet, ParamsSet, ExistQParamsSet),
        set.is_non_empty(ExistQParamsSet)
    ->
        % There should be no duplicate names to remove.
        set.to_sorted_list(ExistQParamsSet, ExistQParams),
        varset.coerce(VarSet, GenericVarSet),
        ExistQParamVarsStrs = list.map(mercury_var_to_string(GenericVarSet, no),
            ExistQParams),
        Pieces = [words("Error:"),
            words(choose_number(ExistQParams,
                "type variable", "type variables"))] ++
            list_to_quoted_pieces(ExistQParamVarsStrs) ++
            [words(choose_number(ExistQParams, "has", "have")),
            words("overlapping scopes"),
            words("(explicit type quantifier shadows argument type)."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        % Check that all type variables in existential quantifiers occur
        % somewhere in the constructor argument types or constraints.

        CtorArgTypes = list.map(func(C) = C ^ arg_type, CtorArgs),
        type_vars_list(CtorArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        constraint_list_get_tvars(Constraints, ConstraintTVars),
        list.filter(list.contains(VarsInCtorArgTypes ++ ConstraintTVars),
            ExistQVars, _OccursExistQVars, NotOccursExistQVars),
        NotOccursExistQVars = [_ | _]
    ->
        % There should be no duplicate names to remove.
        varset.coerce(VarSet, GenericVarSet),
        NotOccursExistQVarsStr =
            mercury_vars_to_string(GenericVarSet, no, NotOccursExistQVars),
        Pieces = [words("Error:"),
            words(choose_number(NotOccursExistQVars,
                "type variable", "type variables")),
            words(NotOccursExistQVarsStr),
            words("in existential quantifier"),
            words(choose_number(NotOccursExistQVars,
                "does not occur", "do not occur")),
            words("in arguments or constraints of constructor."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        % Check that all type variables in existential constraints occur in
        % the existential quantifiers.

        ConstraintArgTypeLists =
            list.map(prog_constraint_get_arg_types, Constraints),
        list.condense(ConstraintArgTypeLists, ConstraintArgTypes),
        type_vars_list(ConstraintArgTypes, VarsInCtorArgTypes0),
        list.sort_and_remove_dups(VarsInCtorArgTypes0, VarsInCtorArgTypes),
        list.filter(list.contains(ExistQVars), VarsInCtorArgTypes,
            _ExistQArgTypes, NotExistQArgTypes),
        NotExistQArgTypes = [_ | _]
    ->
        varset.coerce(VarSet, GenericVarSet),
        NotExistQArgTypesStr =
            mercury_vars_to_string(GenericVarSet, no, NotExistQArgTypes),
        Pieces = [words("Error:"),
            words(choose_number(NotExistQArgTypes,
                "type variable", "type variables")),
            words(NotExistQArgTypesStr),
            words("in class constraints,"),
            words(choose_number(NotExistQArgTypes,
                "which was", "which were")),
            words("introduced with"), quote("=>"),
            words("must be explicitly existentially quantified"),
            words("using"), quote("some"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ;
        true
    ),
    process_du_ctors(Params, VarSet, BodyTerm, Ctors, !Specs).

:- pred check_direct_arg_ctors(list(constructor)::in,
    list(sym_name_and_arity)::in, term::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_direct_arg_ctors(_Ctors, [], _ErrorTerm, !Specs).
check_direct_arg_ctors(Ctors, [DirectArgCtor | DirectArgCtors], ErrorTerm,
        !Specs) :-
    DirectArgCtor = SymName / Arity,
    ( find_constructor(Ctors, SymName, Arity, Ctor) ->
        Ctor = ctor(ExistQVars, _Constraints, _SymName, _Args, _Context),
        ( Arity \= 1 ->
            Pieces = [words("Error: the"), quote("direct_arg"),
                words("attribute contains a function symbol whose arity"),
                words("is not 1."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        ; ExistQVars = [_ | _] ->
            Pieces = [words("Error: the"), quote("direct_arg"),
                words("attribute contains a function symbol"),
                sym_name_and_arity(DirectArgCtor),
                words("with existentially quantified type variables."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
            !:Specs = [Spec | !.Specs]
        ;
            true
        )
    ;
        Pieces = [words("Error: the"), quote("direct_arg"),
            words("attribute lists the function symbol"),
            sym_name_and_arity(DirectArgCtor),
            words("which is not in the type definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ErrorTerm), [always(Pieces)])]),
        !:Specs = [Spec | !.Specs]
    ),
    check_direct_arg_ctors(Ctors, DirectArgCtors, ErrorTerm, !Specs).

:- pred find_constructor(list(constructor)::in, sym_name::in, arity::in,
    constructor::out) is semidet.

find_constructor([H | T], SymName, Arity, Ctor) :-
    (
        H = ctor(_, _, SymName, Args, _),
        list.length(Args, Arity)
    ->
        Ctor = H
    ;
        find_constructor(T, SymName, Arity, Ctor)
    ).

%-----------------------------------------------------------------------------%

    % parse_eqv_type_defn parses the definition of an equivalence type.
    %
:- pred parse_eqv_type_defn(module_name::in, varset::in, term::in, term::in,
    decl_attrs::in, condition::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_eqv_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm, Attributes,
        Condition, Context, SeqNum, MaybeItem) :-
    parse_type_defn_head(ModuleName, VarSet, HeadTerm,
        MaybeNameAndParams),
    (
        MaybeNameAndParams = error2(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeNameAndParams = ok2(Name, Params),
        % Check that all the variables in the body occur in the head.
        (
            term.contains_var(BodyTerm, Var),
            term.coerce_var(Var, TVar),
            not list.member(TVar, Params)
        ->
            BodyTermStr = describe_error_term(VarSet, BodyTerm),
            Pieces = [words("Error: free type parameter"),
                words("in RHS of type definition:"),
                words(BodyTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(BodyTerm), [always(Pieces)])]),
            MaybeItem = error1([Spec])
        ;
            % XXX Should pass more correct ContextPieces.
            ContextPieces = [],
            parse_type(BodyTerm, VarSet, ContextPieces, MaybeType),
            (
                MaybeType = ok1(Type),
                varset.coerce(VarSet, TypeVarSet),
                TypeDefn = parse_tree_eqv_type(Type),
                ItemTypeDefn = item_type_defn_info(TypeVarSet, Name, Params,
                    TypeDefn, Condition, Context, SeqNum),
                Item = item_type_defn(ItemTypeDefn),
                MaybeItem0 = ok1(Item),
                check_no_attributes(MaybeItem0, Attributes, MaybeItem)
            ;
                MaybeType = error1(Specs),
                MaybeItem = error1(Specs)
            )
        )
    ).

%-----------------------------------------------------------------------------%

    % Parse a type definition which consists only of a `where' block.
    % This is either an abstract enumeration type, or a solver type.
    %
:- pred parse_where_block_type_defn(module_name::in, varset::in, term::in,
    term::in, decl_attrs::in, condition::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_where_block_type_defn(ModuleName, VarSet, HeadTerm, BodyTerm,
        Attributes0, Condition, Context, SeqNum, MaybeItem) :-
    get_is_solver_type(IsSolverType, Attributes0, Attributes),
    (
        IsSolverType = non_solver_type,
        parse_where_type_is_abstract_enum(ModuleName, VarSet, HeadTerm,
            BodyTerm, Condition, Context, SeqNum, MaybeItem)
    ;
        IsSolverType = solver_type,
        MaybeWhere = parse_type_decl_where_term(solver_type, ModuleName,
            VarSet, yes(BodyTerm)),
        (
            MaybeWhere = error3(Specs),
            MaybeItem = error1(Specs)
        ;
            MaybeWhere = ok3(MaybeSolverTypeDetails, MaybeUserEqComp,
                MaybeDirectArgCtors),
            (
                MaybeDirectArgCtors = yes(_),
                Pieces = [words("Error: solver type definitions"),
                    words("cannot have a"), quote("direct_arg"),
                    words("attribute."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                MaybeDirectArgCtors = no,
                parse_solver_type_base(ModuleName, VarSet, HeadTerm,
                    MaybeSolverTypeDetails, MaybeUserEqComp, Attributes,
                    Condition, Context, SeqNum, MaybeItem)
            )
        )
    ).

:- pred parse_where_type_is_abstract_enum(module_name::in, varset::in,
    term::in, term::in, condition::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_where_type_is_abstract_enum(ModuleName, VarSet, HeadTerm, BodyTerm,
        Condition, Context, SeqNum, MaybeItem) :-
    parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeNameParams),
    (
        MaybeNameParams = error2(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeNameParams = ok2(Name, Params),
        (
            BodyTerm = term.functor(term.atom("type_is_abstract_enum"),
                Args, _)
        ->
            (
                Args = [Arg],
                Arg = term.functor(integer(NumBits), [], _)
            ->
                varset.coerce(VarSet, TypeVarSet),
                TypeDefn = parse_tree_abstract_type(
                    abstract_enum_type(NumBits)),
                ItemTypeDefn = item_type_defn_info(TypeVarSet, Name, Params,
                    TypeDefn, Condition, Context, SeqNum),
                Item = item_type_defn(ItemTypeDefn),
                MaybeItem = ok1(Item)
            ;
                Pieces = [words("Error: invalid argument for"),
                    words("type_is_abstract_enum."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeItem = error1([Spec])
            )
        ;
            Pieces = [words("Error: invalid"), quote("where ..."),
                words("attributes for abstract non-solver type."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(Context, [always(Pieces)])]),
            MaybeItem = error1([Spec])
        )
    ).

:- pred parse_solver_type_base(module_name::in, varset::in, term::in,
    maybe(solver_type_details)::in, maybe(unify_compare)::in,
    decl_attrs::in, condition::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_solver_type_base(ModuleName, VarSet, HeadTerm,
        MaybeSolverTypeDetails, MaybeUserEqComp, Attributes, Condition,
        Context, SeqNum, MaybeItem) :-
    (
        MaybeSolverTypeDetails = yes(SolverTypeDetails),
        parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeNameParams),
        (
            MaybeNameParams = error2(Specs),
            MaybeItem = error1(Specs)
        ;
            MaybeNameParams = ok2(Name, Params),
            (
                RepnType = SolverTypeDetails ^ std_representation_type,
                type_contains_var(RepnType, Var),
                not list.member(Var, Params)
            ->
                HeadTermStr = describe_error_term(VarSet, HeadTerm),
                Pieces = [words("Error: free type variable"),
                    words("in representation type:"),
                    words(HeadTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(HeadTerm),
                        [always(Pieces)])]),
                MaybeItem = error1([Spec])
            ;
                varset.coerce(VarSet, TypeVarSet),
                TypeDefn = parse_tree_solver_type(SolverTypeDetails,
                    MaybeUserEqComp),
                ItemTypeDefn = item_type_defn_info(TypeVarSet, Name, Params,
                    TypeDefn, Condition, Context, SeqNum),
                Item = item_type_defn(ItemTypeDefn),
                MaybeItem0 = ok1(Item),
                check_no_attributes(MaybeItem0, Attributes, MaybeItem)
            )
        )
    ;
        MaybeSolverTypeDetails = no,
        Pieces = [words("Solver type with no solver_type_details."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(HeadTerm), [always(Pieces)])]),
        MaybeItem = error1([Spec])
    ).

%-----------------------------------------------------------------------------%
%
% Parse an abstract type definition.
%

:- pred parse_abstract_type_defn(module_name::in, varset::in, term::in,
    decl_attrs::in, condition::in, prog_context::in, int::in,
    maybe1(item)::out) is det.

parse_abstract_type_defn(ModuleName, VarSet, HeadTerm, Attributes0,
        Condition, Context, SeqNum, MaybeItem) :-
    parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeTypeCtorAndArgs),
    get_is_solver_type(IsSolverType, Attributes0, Attributes),
    (
        MaybeTypeCtorAndArgs = error2(Specs),
        MaybeItem = error1(Specs)
    ;
        MaybeTypeCtorAndArgs = ok2(Name, Params),
        varset.coerce(VarSet, TypeVarSet),
        (
            IsSolverType = non_solver_type,
            TypeDefn = parse_tree_abstract_type(abstract_type_general)
        ;
            IsSolverType = solver_type,
            TypeDefn = parse_tree_abstract_type(abstract_solver_type)
        ),
        ItemTypeDefn = item_type_defn_info(TypeVarSet, Name, Params, TypeDefn,
            Condition, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn),
        MaybeItem0 = ok1(Item),
        check_no_attributes(MaybeItem0, Attributes, MaybeItem)
    ).

%-----------------------------------------------------------------------------%
%
% Parse ... where ... clauses in type definitions. These clauses can specify
% type-specific unify and/or compare predicates for discriminated union types
% and solver type details for solver types.
%

    % The optional `where ...' part of the type definition syntax
    % is a comma separated list of special type `attributes'.
    %
    % The possible attributes (in this order) are either
    % - `type_is_abstract_noncanonical' on its own appears only in .int2
    %   files and indicates that the type has user-defined equality and/or
    %   comparison, but that what these predicates are is not known at
    %   this point
    % or
    % - `representation is <<type name>>' (required for solver types)
    % - `initialisation is <<pred name>>' (required for solver types)
    % - `ground is <<inst>>' (required for solver types)
    % - `any is <<inst>>' (required for solver types)
    % - `equality is <<pred name>>' (optional)
    % - `comparison is <<pred name>>' (optional).
    %
parse_type_decl_where_part_if_present(IsSolverType, ModuleName, VarSet,
        Term, BeforeWhereTerm, MaybeWhereDetails) :-
    (
        Term = term.functor(term.atom("where"),
            [BeforeWhereTermPrime, WhereTerm], _)
    ->
        BeforeWhereTerm = BeforeWhereTermPrime,
        MaybeWhereDetails = parse_type_decl_where_term(IsSolverType,
            ModuleName, VarSet, yes(WhereTerm))
    ;
        BeforeWhereTerm = Term,
        MaybeWhereDetails = ok3(no, no, no)
    ).

    % The maybe2 wrapper allows us to return an error code or a pair
    % of results. Either result half may be empty, hence the maybe
    % wrapper around each of those.
    %
:- func parse_type_decl_where_term(is_solver_type, module_name, varset,
    maybe(term)) = maybe3(maybe(solver_type_details), maybe(unify_compare),
        maybe(list(sym_name_and_arity))).

parse_type_decl_where_term(IsSolverType, ModuleName, VarSet, MaybeTerm0) =
        MaybeWhereDetails :-
    (
        MaybeTerm0 = no,
        MaybeWhereDetails = ok3(no, no, no)
    ;
        MaybeTerm0 = yes(Term0),
        some [!MaybeTerm] (
            !:MaybeTerm = MaybeTerm0,
            parse_where_attribute(parse_where_type_is_abstract_noncanonical,
                MaybeTypeIsAbstractNoncanonical, !MaybeTerm),
            parse_where_attribute(parse_where_is("representation",
                    parse_where_type_is(ModuleName, VarSet)),
                MaybeRepresentationIs, !MaybeTerm),
            parse_where_attribute(parse_where_initialisation_is(ModuleName,
                    VarSet),
                MaybeInitialisationIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("ground",
                    parse_where_inst_is(ModuleName)),
                MaybeGroundIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("any",
                    parse_where_inst_is(ModuleName)),
                MaybeAnyIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("constraint_store",
                    parse_where_mutable_is(ModuleName)),
                MaybeCStoreIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("equality",
                    parse_where_pred_is(ModuleName, VarSet)),
                MaybeEqualityIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("comparison",
                    parse_where_pred_is(ModuleName, VarSet)),
                MaybeComparisonIs, !MaybeTerm),
            parse_where_attribute(parse_where_is("direct_arg",
                    parse_where_direct_arg_is(ModuleName, VarSet)),
                MaybeDirectArgIs, !MaybeTerm),
            parse_where_end(!.MaybeTerm, MaybeWhereEnd)
        ),
        MaybeWhereDetails = make_maybe_where_details(
            IsSolverType,
            MaybeTypeIsAbstractNoncanonical,
            MaybeRepresentationIs,
            MaybeInitialisationIs,
            MaybeGroundIs,
            MaybeAnyIs,
            MaybeCStoreIs,
            MaybeEqualityIs,
            MaybeComparisonIs,
            MaybeDirectArgIs,
            MaybeWhereEnd,
            Term0
        )
    ).

    % parse_where_attribute(Parser, Result, MaybeTerm, MaybeTailTerm)
    % handles
    % - where MaybeTerm may contain nothing
    % - where MaybeTerm may be a comma-separated pair
    % - applies Parser to the appropriate (sub)term to obtain Result
    % - sets MaybeTailTerm depending upon whether the Result is an error
    % or not and whether there is more to parse because MaybeTerm
    % was a comma-separated pair.
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
        (
            Term = term.functor(term.atom(","), [HeadTerm, TailTerm], _)
        ->
            Result = Parser(HeadTerm),
            MaybeTailTermIfYes = yes(TailTerm)
        ;
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
    ( Term = term.functor(term.atom("is"), [LHS, RHS], _) ->
        ( LHS = term.functor(term.atom(Name), [], _) ->
            RHSResult = Parser(RHS),
            (
                RHSResult = ok1(ParsedRHS),
                Result    = ok1(yes(ParsedRHS))
            ;
                RHSResult = error1(Specs),
                Result    = error1(Specs)
            )
        ;
            Result = ok1(no)
        )
    ;
        Pieces = [words("Error: expected"), quote("is"), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- func parse_where_type_is_abstract_noncanonical(term) = maybe1(maybe(unit)).

parse_where_type_is_abstract_noncanonical(Term) =
    ( Term = term.functor(term.atom("type_is_abstract_noncanonical"), [], _) ->
        ok1(yes(unit))
    ;
        ok1(no)
    ).

:- func parse_where_initialisation_is(module_name, varset, term) =
    maybe1(maybe(sym_name)).

parse_where_initialisation_is(ModuleName, VarSet, Term) = Result :-
    Result0 = parse_where_is("initialisation",
        parse_where_pred_is(ModuleName, VarSet), Term),
    (
        Result0 = ok1(no)
    ->
        Result1 = parse_where_is("initialization",
            parse_where_pred_is(ModuleName, VarSet), Term)
    ;
        Result1 = Result0
    ),
    promise_pure (
        (
            Result1 = ok1(yes(_)),
            semipure
                semipure_get_solver_auto_init_supported(AutoInitSupported),
            (
                AutoInitSupported = yes,
                Result = Result1
            ;
                AutoInitSupported = no,
                Pieces = [words("Error: unknown attribute"),
                    words("in solver type definition."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(Term), [always(Pieces)])]),
                Result = error1([Spec])
            )
        ;
            ( Result1 = ok1(no)
            ; Result1 = error1(_)
            ),
            Result = Result1
        )
    ).

:- func parse_where_pred_is(module_name, varset, term) = maybe1(sym_name).

parse_where_pred_is(ModuleName, VarSet, Term) = MaybeSymName :-
    parse_implicitly_qualified_symbol_name(ModuleName, VarSet, Term,
        MaybeSymName).

:- func parse_where_inst_is(module_name, term) = maybe1(mer_inst).

parse_where_inst_is(_ModuleName, Term) = MaybeInst :-
    (
        convert_inst(no_allow_constrained_inst_var, Term, Inst),
        not inst_contains_unconstrained_var(Inst)
    ->
        MaybeInst = ok1(Inst)
    ;
        Pieces = [words("Error: expected a ground, unconstrained inst."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeInst = error1([Spec])
    ).

:- func parse_where_type_is(module_name, varset, term) = maybe1(mer_type).

parse_where_type_is(_ModuleName, VarSet, Term) = MaybeType :-
    % XXX We should pass meaningful ContextPieces.
    ContextPieces = [],
    parse_type(Term, VarSet, ContextPieces, MaybeType).

:- func parse_where_mutable_is(module_name, term) =
    maybe1(list(item_mutable_info)).

parse_where_mutable_is(ModuleName, Term) = MaybeItems :-
    ( Term = term.functor(term.atom("mutable"), _, _) ->
        parse_mutable_decl_term(ModuleName, Term, MaybeItem),
        (
            MaybeItem = ok1(Mutable),
            MaybeItems  = ok1([Mutable])
        ;
            MaybeItem = error1(Specs),
            MaybeItems  = error1(Specs)
        )
    ; list_term_to_term_list(Term, Terms) ->
        map_parser(parse_mutable_decl_term(ModuleName), Terms, MaybeItems)
    ;
        Pieces = [words("Error: expected a mutable declaration"),
            words("or a list of mutable declarations."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeItems = error1([Spec])
    ).

:- pred parse_mutable_decl_term(module_name::in, term::in,
    maybe1(item_mutable_info)::out) is det.

parse_mutable_decl_term(ModuleName, Term, MaybeMutableInfo) :-
    (
        Term = term.functor(term.atom("mutable"), Args, Context),
        varset.init(VarSet),
        parse_mutable_decl_info(ModuleName, VarSet, Args, Context, -1,
            MaybeMutableInfoPrime)
    ->
        MaybeMutableInfo = MaybeMutableInfoPrime
    ;
        Pieces = [words("Error: expected a mutable declaration."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeMutableInfo = error1([Spec])
    ).

:- func parse_where_direct_arg_is(module_name, varset, term) =
    maybe1(list(sym_name_and_arity)).

parse_where_direct_arg_is(ModuleName, VarSet, Term) = MaybeDirectArgCtors :-
    ( list_term_to_term_list(Term, FunctorsTerms) ->
        map_parser(parse_direct_arg_functor(ModuleName, VarSet),
            FunctorsTerms, MaybeDirectArgCtors)
    ;
        Pieces = [words("Error: malformed functors list in"),
            quote("direct_arg"), words("attribute."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term),
            [always(Pieces)])]),
        MaybeDirectArgCtors = error1([Spec])
    ).

:- pred parse_direct_arg_functor(module_name::in, varset::in, term::in,
    maybe1(sym_name_and_arity)::out) is det.

parse_direct_arg_functor(ModuleName, VarSet, Term, MaybeFunctor) :-
    ( parse_name_and_arity(ModuleName, Term, Name, Arity) ->
        MaybeFunctor = ok1(Name / Arity)
    ;
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error: expected functor"),
            words("name/arity for"), quote("direct_arg"),
            words("attribute, not"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        MaybeFunctor = error1([Spec])
    ).

:- pred parse_where_end(maybe(term)::in, maybe1(maybe(unit))::out) is det.

parse_where_end(no, ok1(yes(unit))).
parse_where_end(yes(Term), error1([Spec])) :-
    Pieces = [words("Error: attributes are either badly ordered"),
        words("or contain an unrecognised attribute."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]).

:- func make_maybe_where_details(is_solver_type, maybe1(maybe(unit)),
    maybe1(maybe(mer_type)), maybe1(maybe(init_pred)),
    maybe1(maybe(mer_inst)), maybe1(maybe(mer_inst)),
    maybe1(maybe(list(item_mutable_info))),
    maybe1(maybe(equality_pred)), maybe1(maybe(comparison_pred)),
    maybe1(maybe(list(sym_name_and_arity))),
    maybe1(maybe(unit)), term)
    = maybe3(maybe(solver_type_details), maybe(unify_compare),
        maybe(list(sym_name_and_arity))).

make_maybe_where_details(IsSolverType, MaybeTypeIsAbstractNoncanonical,
        MaybeRepresentationIs, MaybeInitialisationIs,
        MaybeGroundIs, MaybeAnyIs, MaybeCStoreIs,
        MaybeEqualityIs, MaybeComparisonIs, MaybeDirectArgIs,
        MaybeWhereEnd, WhereTerm) = MaybeWhereDetails :-
    (
        MaybeTypeIsAbstractNoncanonical = ok1(TypeIsAbstractNoncanonical),
        MaybeRepresentationIs = ok1(RepresentationIs),
        MaybeInitialisationIs = ok1(InitialisationIs),
        MaybeGroundIs = ok1(GroundIs),
        MaybeAnyIs = ok1(AnyIs),
        MaybeCStoreIs = ok1(CStoreIs),
        MaybeEqualityIs = ok1(EqualityIs),
        MaybeComparisonIs = ok1(ComparisonIs),
        MaybeDirectArgIs = ok1(DirectArgIs),
        MaybeWhereEnd = ok1(WhereEnd)
    ->
        MaybeWhereDetails = make_maybe_where_details_2(IsSolverType,
            TypeIsAbstractNoncanonical, RepresentationIs, InitialisationIs,
            GroundIs, AnyIs, CStoreIs, EqualityIs, ComparisonIs, DirectArgIs,
            WhereEnd, WhereTerm)
    ;
        Specs =
            get_any_errors1(MaybeTypeIsAbstractNoncanonical) ++
            get_any_errors1(MaybeRepresentationIs) ++
            get_any_errors1(MaybeInitialisationIs) ++
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
    maybe(mer_type), maybe(init_pred), maybe(mer_inst), maybe(mer_inst),
    maybe(list(item_mutable_info)),
    maybe(equality_pred), maybe(comparison_pred),
    maybe(list(sym_name_and_arity)), maybe(unit), term)
    = maybe3(maybe(solver_type_details), maybe(unify_compare),
        maybe(list(sym_name_and_arity))).

make_maybe_where_details_2(IsSolverType, TypeIsAbstractNoncanonical,
        RepresentationIs, InitialisationIs, GroundIs, AnyIs, CStoreIs,
        EqualityIs, ComparisonIs, DirectArgIs, _WhereEnd, WhereTerm)
        = MaybeWhereDetails :-
    (
        TypeIsAbstractNoncanonical = yes(_),
        % rafe: XXX I think this is wrong. There isn't a problem with having
        % the solver_type_details and type_is_abstract_noncanonical.
        (
            RepresentationIs = maybe.no,
            InitialisationIs = maybe.no,
            GroundIs         = maybe.no,
            AnyIs            = maybe.no,
            EqualityIs       = maybe.no,
            ComparisonIs     = maybe.no,
            CStoreIs         = maybe.no,
            DirectArgIs      = maybe.no
        ->
            MaybeWhereDetails =
                ok3(no, yes(abstract_noncanonical_type(IsSolverType)), no)
        ;
            Pieces = [words("Error:"),
                quote("where type_is_abstract_noncanonical"),
                words("excludes other"), quote("where ..."),
                words("attributes."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(WhereTerm), [always(Pieces)])]),
            MaybeWhereDetails = error3([Spec])
        )
    ;
        TypeIsAbstractNoncanonical = maybe.no,
        (
            IsSolverType = solver_type,
            (
                DirectArgIs = yes(_)
            ->
                Pieces = [words("Error: solver type definitions cannot have"),
                    quote("direct_arg"), words("attributes."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(WhereTerm),
                        [always(Pieces)])]),
                MaybeWhereDetails = error3([Spec])
            ;
                RepresentationIs = yes(RepnType),
                InitialisationIs = MaybeInitialisation,
                GroundIs         = MaybeGroundInst,
                AnyIs            = MaybeAnyInst,
                EqualityIs       = MaybeEqPred,
                ComparisonIs     = MaybeCmpPred,
                CStoreIs         = MaybeMutableInfos
            ->
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
                (
                    MaybeInitialisation = yes(InitPred),
                    HowToInit = solver_init_automatic(InitPred)
                ;
                    MaybeInitialisation = no,
                    HowToInit = solver_init_explicit
                ),
                SolverTypeDetails = solver_type_details(
                    RepnType, HowToInit, GroundInst, AnyInst, MutableInfos),
                MaybeSolverTypeDetails = yes(SolverTypeDetails),
                (
                    MaybeEqPred = no,
                    MaybeCmpPred = no
                ->
                    MaybeUnifyCompare = no
                ;
                    MaybeUnifyCompare = yes(unify_compare(
                        MaybeEqPred, MaybeCmpPred))
                ),
                MaybeWhereDetails = ok3(MaybeSolverTypeDetails,
                    MaybeUnifyCompare, no)
            ;
                RepresentationIs = no
            ->
                Pieces = [words("Error: solver type definitions must have a"),
                    quote("representation"), words("attribute."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(WhereTerm),
                        [always(Pieces)])]),
                MaybeWhereDetails = error3([Spec])
            ;
               unexpected($module, $pred, "make_maybe_where_details_2: " ++
                    "shouldn't have reached this point! (1)")
            )
        ;
            IsSolverType = non_solver_type,
            (
                ( RepresentationIs = yes(_)
                ; InitialisationIs = yes(_)
                ; GroundIs         = yes(_)
                ; AnyIs            = yes(_)
                ; CStoreIs         = yes(_)
                )
            ->
                Pieces = [words("Error: solver type attribute given"),
                    words("for non-solver type."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(WhereTerm),
                        [always(Pieces)])]),
                MaybeWhereDetails = error3([Spec])
            ;
                MaybeUC = maybe_unify_compare(EqualityIs, ComparisonIs),
                MaybeWhereDetails = ok3(no, MaybeUC, DirectArgIs)
            )
        )
    ).

:- func maybe_unify_compare(maybe(equality_pred), maybe(comparison_pred))
    = maybe(unify_compare).

maybe_unify_compare(MaybeEqPred, MaybeCmpPred) =
    (
        MaybeEqPred = no,
        MaybeCmpPred = no
    ->
        no
    ;
        yes(unify_compare(MaybeEqPred, MaybeCmpPred))
    ).

%-----------------------------------------------------------------------------%
%
% Predicates useful for parsing several kinds of type definitions.
%

parse_type_defn_head(ModuleName, VarSet, HeadTerm, MaybeTypeCtorAndArgs) :-
    (
        HeadTerm = term.variable(_, Context),
        Pieces = [words("Error: variable on LHS of type definition."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        MaybeTypeCtorAndArgs = error2([Spec])
    ;
        HeadTerm = term.functor(_, _, HeadContext),
        ContextPieces = [words("In type definition:")],
        parse_implicitly_qualified_sym_name_and_args(ModuleName, HeadTerm,
            VarSet, ContextPieces, HeadResult),
        (
            HeadResult = error2(Specs),
            MaybeTypeCtorAndArgs = error2(Specs)
        ;
            HeadResult = ok2(Name, ArgTerms),
            % Check that all the head args are variables.
            ( term_list_to_var_list(ArgTerms, Params0) ->
                % Check that all the head arg variables are distinct.
                (
                    list.member(_, Params0, [Param | OtherParams]),
                    list.member(Param, OtherParams)
                ->
                    Pieces = [words("Error: repeated type parameters"),
                        words("in LHS of type definition."), nl],
                    Spec = error_spec(severity_error, phase_term_to_parse_tree,
                        [simple_msg(HeadContext, [always(Pieces)])]),
                    MaybeTypeCtorAndArgs = error2([Spec])
                ;
                    list.map(term.coerce_var, Params0, Params),
                    MaybeTypeCtorAndArgs = ok2(Name, Params)
                )
            ;
                HeadTermStr = describe_error_term(VarSet, HeadTerm),
                Pieces = [words("Error: type parameters must be variables:"),
                    words(HeadTermStr), suffix(".") ,nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(HeadContext, [always(Pieces)])]),
                MaybeTypeCtorAndArgs = error2([Spec])
            )
        )
    ).

%-----------------------------------------------------------------------------e

:- pred get_is_solver_type(is_solver_type::out,
    decl_attrs::in, decl_attrs::out) is det.

get_is_solver_type(IsSolverType, !Attributes) :-
    ( !.Attributes = [decl_attr_solver_type - _ | !:Attributes] ->
        IsSolverType = solver_type
    ;
        IsSolverType = non_solver_type
    ).

%-----------------------------------------------------------------------------e
:- end_module parse_tree.prog_io_type_defn.
%-----------------------------------------------------------------------------e
