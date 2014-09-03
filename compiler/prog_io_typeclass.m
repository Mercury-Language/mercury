%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2011 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_typeclass.m.
% Main authors: dgj.
%
% This module handles the parsing of typeclass declarations.
% Perhaps some of this should go into prog_io_util.m?
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_typeclass.
:- interface.

:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module list.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Parse a typeclass declaration.
    %
:- pred parse_typeclass(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_typeclass_info)::out) is semidet.

    % Parse an instance declaration.
    %
:- pred parse_instance(module_name::in, varset::in, list(term)::in,
    prog_context::in, int::in, maybe1(item_instance_info)::out) is semidet.

    % Parse constraints on a pred or func declaration, or on an existentially
    % quantified type definition. Currently all such constraints must be
    % simple.
    %
:- pred parse_class_constraints(module_name::in, varset::in, term::in,
    maybe1(list(prog_constraint))::out) is det.

    % Parse a list of class and inst constraints.
    %
:- pred parse_class_and_inst_constraints(module_name::in, varset::in, term::in,
    maybe_class_and_inst_constraints::out) is det.

:- type maybe_class_and_inst_constraints ==
    maybe2(list(prog_constraint), inst_var_sub).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_item.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module map.
:- import_module require.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

parse_typeclass(ModuleName, VarSet, TypeClassTerm, Context, SeqNum,
        MaybeItemTypeClass) :-
    % XXX We should return an error if we get more than one arg, instead of
    % failing.
    TypeClassTerm = [Arg],
    ( Arg = term.functor(term.atom("where"), [Name, Methods], _) ->
        parse_non_empty_class(ModuleName, Name, Methods, VarSet, Context,
            SeqNum, MaybeItemTypeClass)
    ;
        parse_class_head(ModuleName, Arg, VarSet, Context, SeqNum,
            MaybeItemTypeClass)
    ).

:- pred parse_non_empty_class(module_name::in, term::in, term::in, varset::in,
    prog_context::in, int::in, maybe1(item_typeclass_info)::out) is det.

parse_non_empty_class(ModuleName, Name, Methods, VarSet, Context, SeqNum,
        MaybeItemTypeClass) :-
    varset.coerce(VarSet, TVarSet),
    parse_class_methods(ModuleName, Methods, VarSet,
        MaybeParsedMethods),
    (
        MaybeParsedMethods = ok1(MethodList),
        parse_class_head(ModuleName, Name, VarSet, Context, SeqNum,
            MaybeParsedNameAndVars),
        (
            MaybeParsedNameAndVars = error1(Specs),
            MaybeItemTypeClass = error1(Specs)
        ;
            MaybeParsedNameAndVars = ok1(ParsedNameAndVars),
            MaybeItemTypeClass = ok1((ParsedNameAndVars
                ^ tc_class_methods := class_interface_concrete(MethodList))
                ^ tc_varset := TVarSet)
        )
    ;
        MaybeParsedMethods = error1(Specs),
        MaybeItemTypeClass = error1(Specs)
    ).

:- pred parse_class_head(module_name::in, term::in, varset::in,
    prog_context::in, int::in, maybe1(item_typeclass_info)::out) is det.

parse_class_head(ModuleName, Arg, VarSet, Context, SeqNum,
        MaybeItemTypeClass) :-
    ( Arg = term.functor(term.atom("<="), [Name, Constraints], _) ->
        parse_constrained_class(ModuleName, Name, Constraints, VarSet, Context,
            SeqNum, MaybeItemTypeClass)
    ;
        varset.coerce(VarSet, TVarSet),
        parse_unconstrained_class(ModuleName, Arg, TVarSet, Context,
            SeqNum, MaybeItemTypeClass)
    ).

:- pred parse_constrained_class(module_name::in, term::in, term::in,
    varset::in, prog_context::in, int::in, maybe1(item_typeclass_info)::out)
    is det.

parse_constrained_class(ModuleName, Decl, ConstraintsTerm, VarSet, Context,
        SeqNum, MaybeItemTypeClass) :-
    varset.coerce(VarSet, TVarSet),
    parse_superclass_constraints(ModuleName, VarSet, ConstraintsTerm,
        MaybeParsedConstraints),
    (
        MaybeParsedConstraints = ok2(ConstraintList, FunDeps),
        parse_unconstrained_class(ModuleName, Decl, TVarSet, Context, SeqNum,
            MaybeItemTypeClass0),
        (
            MaybeItemTypeClass0 = error1(_),
            MaybeItemTypeClass = MaybeItemTypeClass0
        ;
            MaybeItemTypeClass0 = ok1(ItemTypeClass0),
            % Check for type variables in the constraints which do not
            % occur in the type class parameters.

            constraint_list_get_tvars(ConstraintList, ConstraintVars),
            list.sort_and_remove_dups(ConstraintVars, SortedConstraintVars),
            FunDepVars = tvars_in_fundeps(FunDeps),
            list.sort_and_remove_dups(FunDepVars, SortedFunDepVars),

            Params = ItemTypeClass0 ^ tc_class_params,
            list.filter(is_in_list(Params), SortedConstraintVars,
                _ConstraintInParams, ConstraintNotInParams),
            list.filter(is_in_list(Params), SortedFunDepVars,
                _FunDepInParams, FunDepNotInParams),
            (
                ConstraintNotInParams = [_ | _],
                ( list.length(ConstraintList) = 1 ->
                    ConstraintErrorContext =
                        [words("in the superclass constraint")]
                ;
                    ConstraintErrorContext =
                        [words("in superclass constraints")]
                )
            ;
                ConstraintNotInParams = [],
                ConstraintErrorContext = []
            ),
            (
                FunDepNotInParams = [_ | _],
                ( list.length(FunDeps) = 1 ->
                    FunDepErrorContext =
                        [words("in the functional dependency")]
                ;
                    FunDepErrorContext =
                        [words("in functional dependencies")]
                )
            ;
                FunDepNotInParams = [],
                FunDepErrorContext = []
            ),
            NotInParams = ConstraintNotInParams ++ FunDepNotInParams,
            (
                NotInParams = [],
                ItemTypeClass = ((ItemTypeClass0
                    ^ tc_constraints := ConstraintList)
                    ^ tc_fundeps := FunDeps),
                MaybeItemTypeClass = ok1(ItemTypeClass)
            ;
                NotInParams = [_ | _],
                ClassTVarSet = ItemTypeClass0 ^ tc_varset,
                ConstraintNotInParamsStrs =
                    list.map(mercury_var_to_string(ClassTVarSet, no),
                        ConstraintNotInParams),
                FunDepNotInParamsStrs =
                    list.map(mercury_var_to_string(ClassTVarSet, no),
                        FunDepNotInParams),
                ConstraintNotInParamsPieces =
                    list_to_pieces(ConstraintNotInParamsStrs),
                FunDepNotInParamsPieces =
                    list_to_pieces(FunDepNotInParamsStrs),
                ( list.length(NotInParams) = 1 ->
                    Prefix = [words("Error: type variable")],
                    Suffix = [words("is not a parameter of this type class.")]
                ;
                    Prefix = [words("Error: type variables")],
                    Suffix = [words("are not parameters of this type class.")]
                ),
                (
                    ConstraintNotInParams = [],
                    FunDepNotInParams = [],
                    unexpected($module, $pred, "no NotInParams")
                ;
                    ConstraintNotInParams = [],
                    FunDepNotInParams = [_ | _],
                    Middle =
                        FunDepNotInParamsPieces ++ FunDepErrorContext
                ;
                    ConstraintNotInParams = [_ | _],
                    FunDepNotInParams = [],
                    Middle =
                        ConstraintNotInParamsPieces ++ ConstraintErrorContext
                ;
                    ConstraintNotInParams = [_ | _],
                    FunDepNotInParams = [_ | _],
                    Middle =
                        ConstraintNotInParamsPieces ++ ConstraintErrorContext
                        ++ [words("and")] ++
                        FunDepNotInParamsPieces ++ FunDepErrorContext
                ),
                Pieces = Prefix ++ Middle ++ Suffix ++ [nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(Context, [always(Pieces)])]),
                MaybeItemTypeClass = error1([Spec])
            )
        )
    ;
        MaybeParsedConstraints = error2(Specs),
        MaybeItemTypeClass = error1(Specs)
    ).

:- func tvars_in_fundeps(list(prog_fundep)) = list(tvar).

tvars_in_fundeps(FunDeps) = list.condense(list.map(tvars_in_fundep, FunDeps)).

:- func tvars_in_fundep(prog_fundep) = list(tvar).

tvars_in_fundep(fundep(Domain, Range)) = Domain ++ Range.

:- pred parse_superclass_constraints(module_name::in, varset::in, term::in,
    maybe2(list(prog_constraint), list(prog_fundep))::out) is det.

parse_superclass_constraints(_ModuleName, VarSet, ConstraintsTerm, Result) :-
    parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result0),
    (
        Result0 = ok1(ArbitraryConstraints),
        (
            collect_simple_and_fundep_constraints(ArbitraryConstraints,
                Constraints, FunDeps)
        ->
            Result = ok2(Constraints, FunDeps)
        ;
            Pieces = [words("Error: constraints on class declarations"),
                words("may only constrain type variables and ground types."),
                nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ConstraintsTerm),
                    [always(Pieces)])]),
            Result = error2([Spec])
        )
    ;
        Result0 = error1(Specs),
        Result = error2(Specs)
    ).

:- pred collect_simple_and_fundep_constraints(list(arbitrary_constraint)::in,
    list(prog_constraint)::out, list(prog_fundep)::out) is semidet.

collect_simple_and_fundep_constraints([], [], []).
collect_simple_and_fundep_constraints([Constraint | Constraints],
        SimpleConstraints, FunDeps) :-
    collect_simple_and_fundep_constraints(Constraints, SimpleConstraints0,
        FunDeps0),
    (
        Constraint = simple(SimpleConstraint),
        SimpleConstraints = [SimpleConstraint | SimpleConstraints0],
        FunDeps = FunDeps0
    ;
        Constraint = fundep(FunDep),
        FunDeps = [FunDep | FunDeps0],
        SimpleConstraints = SimpleConstraints0
    ).

:- pred parse_unconstrained_class(module_name::in, term::in, tvarset::in,
    prog_context::in, int::in, maybe1(item_typeclass_info)::out) is det.

parse_unconstrained_class(ModuleName, NameTerm, TVarSet, Context, SeqNum,
        MaybeTypeClassInfo) :-
    ContextPieces = [words("In typeclass declaration:")],
    varset.coerce(TVarSet, VarSet),
    parse_implicitly_qualified_sym_name_and_args(ModuleName, NameTerm,
        VarSet, ContextPieces, MaybeClassName),
    (
        MaybeClassName = ok2(ClassName, TermVars0),
        list.map(term.coerce, TermVars0, TermVars),
        (
            term.term_list_to_var_list(TermVars, Vars),
            list.sort_and_remove_dups(TermVars, SortedTermVars),
            list.length(SortedTermVars) = list.length(TermVars) : int
        ->
            % XXX Would this be a better context?
            % Context = get_term_context(NameTerm),
            TypeClassInfo = item_typeclass_info([], [], ClassName, Vars,
                class_interface_abstract, TVarSet, Context, SeqNum),
            MaybeTypeClassInfo = ok1(TypeClassInfo)
        ;
            Pieces = [words("Error: expected distinct variables"),
                words("as class parameters."), nl],
            % XXX Would Context be better than get_term_context(NameTerm)?
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(NameTerm), [always(Pieces)])]),
            MaybeTypeClassInfo = error1([Spec])
        )
    ;
        MaybeClassName = error2(Specs),
        MaybeTypeClassInfo = error1(Specs)
    ).

:- pred parse_class_methods(module_name::in, term::in, varset::in,
    maybe1(class_methods)::out) is det.

parse_class_methods(ModuleName, MethodsTerm, VarSet, Result) :-
    (
        % Convert the list of terms into a list of maybe1(class_method)s.
        list_term_to_term_list(MethodsTerm, MethodList)
    ->
        list.map(
            (pred(MethodTerm::in, Method::out) is det :-
                % Turn the term into an item.
                parse_decl(ModuleName, VarSet, MethodTerm, -1, Item),
                % Turn the item into a class_method.
                item_to_class_method(Item, MethodTerm, Method)
            ), MethodList, Interface),
        find_errors(Interface, Result)
    ;
        Pieces = [words("Error: expected list of class methods."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(MethodsTerm), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- pred item_to_class_method(maybe1(item)::in, term::in,
    maybe1(class_method)::out) is det.

item_to_class_method(error1(Specs), _, error1(Specs)).
item_to_class_method(ok1(Item), Term, Result) :-
    ( Item = item_pred_decl(ItemPredDecl) ->
        ItemPredDecl = item_pred_decl_info(_Origin, A, B, C, D, E, F, G, H, I,
            J, K, L, Context, _SeqNum),
        ClassMethod = method_pred_or_func(A, B, C, D, E, F, G, H, I,
            J, K, L, Context),
        Result = ok1(ClassMethod)
    ; Item = item_mode_decl(ItemModeDecl) ->
        ItemModeDecl = item_mode_decl_info(A, B, C, D, E, F, G,
            Context, _SeqNum),
        ClassMethod = method_pred_or_func_mode(A, B, C, D, E, F, G,
            Context),
        Result = ok1(ClassMethod)
    ;
        Pieces = [words("Error: only pred, func and mode declarations"),
            words("are allowed in class interfaces."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

    % From a list of maybe1s, search through until you find an error.
    % If some errors are found, error1(their union).
    % If no error is found, return ok1(the original elements).
    %
:- pred find_errors(list(maybe1(T))::in, maybe1(list(T))::out) is det.

find_errors(Xs, Result) :-
    find_errors_2(Xs, [], Methods, [], Specs),
    (
        Specs = [],
        Result = ok1(Methods)
    ;
        Specs = [_ | _],
        Result = error1(Specs)
    ).

:- pred find_errors_2(list(maybe1(T))::in, list(T)::in, list(T)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

find_errors_2([], !Methods, !Specs).
find_errors_2([X | Xs], !Methods, !Specs) :-
    find_errors_2(Xs, !Methods, !Specs),
    (
        X = ok1(CurMethod),
        !:Methods = [CurMethod | !.Methods]
    ;
        X = error1(CurSpecs),
        !:Specs = CurSpecs ++ !.Specs
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for parsing various kinds of constraints.
%

parse_class_constraints(ModuleName, VarSet, ConstraintsTerm, Result) :-
    Pieces = [words("Sorry, not implemented:"),
        words("constraints may only constrain type variables"),
        words("and ground types"), nl],
    parse_simple_class_constraints(ModuleName, VarSet, ConstraintsTerm, Pieces,
        Result).

:- pred parse_simple_class_constraints(module_name::in, varset::in, term::in,
    list(format_component)::in, maybe1(list(prog_constraint))::out) is det.

parse_simple_class_constraints(_ModuleName, VarSet, ConstraintsTerm, Pieces,
        Result) :-
    parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result0),
    (
        Result0 = ok1(ArbitraryConstraints),
        (
            % Fail if any of the constraints aren't simple.
            list.map(get_simple_constraint, ArbitraryConstraints, Constraints)
        ->
            Result = ok1(Constraints)
        ;
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ConstraintsTerm),
                    [always(Pieces)])]),
            Result = error1([Spec])
        )
    ;
        Result0 = error1(Specs),
        Result = error1(Specs)
    ).

:- pred get_simple_constraint(arbitrary_constraint::in, prog_constraint::out)
    is semidet.

get_simple_constraint(simple(Constraint), Constraint).

parse_class_and_inst_constraints(_ModuleName, VarSet, ConstraintsTerm,
        Result) :-
    parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result0),
    (
        Result0 = ok1(ArbitraryConstraints),
        (
            collect_class_and_inst_constraints(ArbitraryConstraints,
                ProgConstraints, InstVarSub)
        ->
            Result = ok2(ProgConstraints, InstVarSub)
        ;
            Pieces = [words("Error: functional dependencies are only allowed"),
                words("in typeclass declarations."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(ConstraintsTerm),
                    [always(Pieces)])]),
            Result = error2([Spec])
        )
    ;
        Result0 = error1(Specs),
        Result = error2(Specs)
    ).

:- pred collect_class_and_inst_constraints(list(arbitrary_constraint)::in,
    list(prog_constraint)::out, inst_var_sub::out) is semidet.

collect_class_and_inst_constraints([], [], map.init).
collect_class_and_inst_constraints([Constraint | Constraints],
        ProgConstraints, InstVarSub) :-
    collect_class_and_inst_constraints(Constraints, ProgConstraints0,
        InstVarSub0),
    (
        Constraint = simple(SimpleConstraint),
        ProgConstraints = [SimpleConstraint | ProgConstraints0],
        InstVarSub = InstVarSub0
    ;
        Constraint = non_simple(ClassConstraint),
        ProgConstraints = [ClassConstraint | ProgConstraints0],
        InstVarSub = InstVarSub0
    ;
        Constraint = inst_constraint(InstVar, Inst),
        map.set(InstVar, Inst, InstVarSub0, InstVarSub),
        ProgConstraints = ProgConstraints0
    ).

:- type arbitrary_constraint
    --->    simple(prog_constraint)
            % A class constraint whose arguments are either variables
            % or ground terms.

    ;       non_simple(prog_constraint)
            % An arbitrary class constraint not matching the description
            % of "simple".

    ;       inst_constraint(inst_var, mer_inst)
            % A constraint on an inst variable (that is, one whose head
            % is '=<'/2).

    ;       fundep(prog_fundep).
            % A functional dependency (that is, one whose head is '->'/2)
            % and whose arguments are comma-separated variables.

:- type arbitrary_constraints == list(arbitrary_constraint).

:- pred parse_arbitrary_constraints(varset::in, term::in,
    maybe1(arbitrary_constraints)::out) is det.

parse_arbitrary_constraints(VarSet, ConstraintsTerm, Result) :-
    conjunction_to_list(ConstraintsTerm, ConstraintList),
    parse_arbitrary_constraint_list(VarSet, ConstraintList, Result).

:- pred parse_arbitrary_constraint_list(varset::in, list(term)::in,
    maybe1(arbitrary_constraints)::out) is det.

parse_arbitrary_constraint_list(_, [], ok1([])).
parse_arbitrary_constraint_list(VarSet, [Term | Terms], Result) :-
    parse_arbitrary_constraint(VarSet, Term, Result0),
    parse_arbitrary_constraint_list(VarSet, Terms, Result1),
    Result = combine_parse_results(Result0, Result1).

:- func combine_parse_results(maybe1(arbitrary_constraint),
    maybe1(arbitrary_constraints)) = maybe1(arbitrary_constraints).

combine_parse_results(error1(HeadSpecs), error1(TailSpecs)) =
    error1(HeadSpecs ++ TailSpecs).
combine_parse_results(error1(Specs), ok1(_)) = error1(Specs).
combine_parse_results(ok1(_), error1(Specs)) = error1(Specs).
combine_parse_results(ok1(Constraint), ok1(Constraints)) =
    ok1([Constraint | Constraints]).

:- pred parse_arbitrary_constraint(varset::in, term::in,
    maybe1(arbitrary_constraint)::out) is det.

parse_arbitrary_constraint(VarSet, ConstraintTerm, Result) :-
    (
        parse_inst_constraint(ConstraintTerm, InstVar, Inst)
    ->
        Result = ok1(inst_constraint(InstVar, Inst))
    ;
        parse_fundep(ConstraintTerm, Result0)
    ->
        Result = Result0
    ;
        try_parse_sym_name_and_args(ConstraintTerm, ClassName, Args0)
    ->
        % XXX ArgsResultContextPieces = [words("In typeclass constraint:")]
        ArgsResultContextPieces = [],
        parse_types(Args0, VarSet, ArgsResultContextPieces, ArgsResult),
        (
            ArgsResult = ok1(Args),
            Constraint = constraint(ClassName, Args),
            ( constraint_is_not_simple(Constraint) ->
                Result = ok1(non_simple(Constraint))
            ;
                Result = ok1(simple(Constraint))
            )
        ;
            ArgsResult = error1(Specs),
            Result = error1(Specs)
        )
    ;
        Pieces = [words("Error: expected atom"),
            words("as class name or inst constraint."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(ConstraintTerm), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- pred parse_inst_constraint(term::in, inst_var::out, mer_inst::out)
    is semidet.

parse_inst_constraint(Term, InstVar, Inst) :-
    Term = term.functor(term.atom("=<"), [Arg1, Arg2], _),
    Arg1 = term.variable(InstVar0, _),
    term.coerce_var(InstVar0, InstVar),
    convert_inst(no_allow_constrained_inst_var, Arg2, Inst).

:- pred parse_fundep(term::in, maybe1(arbitrary_constraint)::out) is semidet.

parse_fundep(Term, Result) :-
    Term = term.functor(term.atom("->"), [DomainTerm, RangeTerm], _),
    (
        parse_fundep_2(DomainTerm, Domain),
        parse_fundep_2(RangeTerm, Range)
    ->
        Result = ok1(fundep(fundep(Domain, Range)))
    ;
        Pieces = [words("Error: the domain and range"),
            words("of a functional dependency"),
            words("must be comma-separated lists of variables."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ).

:- pred parse_fundep_2(term::in, list(tvar)::out) is semidet.

parse_fundep_2(Term, TVars) :-
    TypeTerm = term.coerce(Term),
    conjunction_to_list(TypeTerm, List),
    term.term_list_to_var_list(List, TVars).

:- pred constraint_is_not_simple(prog_constraint::in) is semidet.

constraint_is_not_simple(constraint(_Name, Types)) :-
    some [Type] (
        list.member(Type, Types),
        type_is_nonvar(Type),
        type_is_nonground(Type)
    ).

%-----------------------------------------------------------------------------%

parse_instance(ModuleName, VarSet, TypeClassTerm, Context, SeqNum, Result) :-
    % XXX We should return an error if we get more than one arg,
    % instead of failing.
    TypeClassTerm = [Arg],
    varset.coerce(VarSet, TVarSet),
    ( Arg = term.functor(term.atom("where"), [Name, Methods], _) ->
        parse_non_empty_instance(ModuleName, Name, Methods, VarSet, TVarSet,
            Context, SeqNum, Result)
    ;
        parse_instance_name(ModuleName, Arg, TVarSet, Context, SeqNum, Result)
    ).

:- pred parse_instance_name(module_name::in, term::in, tvarset::in,
    prog_context::in, int::in, maybe1(item_instance_info)::out) is det.

parse_instance_name(ModuleName, Arg, TVarSet, Context, SeqNum,
        MaybeItemInstance) :-
    ( Arg = term.functor(term.atom("<="), [Name, Constraints], _) ->
        parse_derived_instance(ModuleName, Name, Constraints, TVarSet, Context,
            SeqNum, MaybeItemInstance)
    ;
        parse_underived_instance(ModuleName, Arg, TVarSet, Context,
            SeqNum, MaybeItemInstance)
    ).

:- pred parse_derived_instance(module_name::in, term::in, term::in,
    tvarset::in, prog_context::in, int::in, maybe1(item_instance_info)::out)
    is det.

parse_derived_instance(ModuleName, Decl, Constraints, TVarSet, Context,
        SeqNum, MaybeItemInstance) :-
    varset.coerce(TVarSet, VarSet),
    parse_instance_constraints(ModuleName, VarSet, Constraints,
        MaybeParsedConstraints),
    (
        MaybeParsedConstraints = ok1(ConstraintList),
        parse_underived_instance(ModuleName, Decl, TVarSet, Context, SeqNum,
            MaybeItemInstance0),
        (
            MaybeItemInstance0 = error1(_),
            MaybeItemInstance = MaybeItemInstance0
        ;
            MaybeItemInstance0 = ok1(ItemInstance0),
            ItemInstance0 = item_instance_info(_ConstraintList0, Name,
                Types, OriginalTypes, Body, InstanceVarSet, ModName,
                InstanceContext, ItemSeqNum),
            % XXX Should we keep InstanceContext, or should we replace it
            % with Context? Or will they always be the same?
            ItemInstance = item_instance_info(ConstraintList, Name,
                Types, OriginalTypes, Body, InstanceVarSet, ModName,
                InstanceContext, ItemSeqNum),
            MaybeItemInstance = ok1(ItemInstance)
        )
    ;
        MaybeParsedConstraints = error1(Specs),
        MaybeItemInstance = error1(Specs)
    ).

:- pred parse_instance_constraints(module_name::in, varset::in, term::in,
    maybe1(list(prog_constraint))::out) is det.

parse_instance_constraints(ModuleName, VarSet, ConstraintsTerm, Result) :-
    Pieces = [words("Error: constraints on instance declarations"),
        words("may only constrain type variables and ground types."), nl],
    parse_simple_class_constraints(ModuleName, VarSet, ConstraintsTerm, Pieces,
        Result).

:- pred parse_underived_instance(module_name::in, term::in, tvarset::in,
    prog_context::in, int::in, maybe1(item_instance_info)::out) is det.

parse_underived_instance(ModuleName, NameTerm, TVarSet, Context, SeqNum,
        MaybeItemInstance) :-
    % We don't give a default module name here since the instance declaration
    % could well be for a typeclass defined in another module.
    NameContextPieces = [words("In instance declaration:")],
    varset.coerce(TVarSet, VarSet),
    parse_sym_name_and_args(NameTerm, VarSet, NameContextPieces,
        MaybeClassName),
    (
        MaybeClassName = ok2(ClassName, TermTypes),
        % XXX Give better TypesContextPieces.
        TypesContextPieces = [],
        parse_types(TermTypes, VarSet, TypesContextPieces, MaybeTypes),
        (
            MaybeTypes = ok1(Types),
            ItemInstance = item_instance_info([], ClassName, Types, Types,
                instance_body_abstract, TVarSet, ModuleName, Context, SeqNum),
            MaybeItemInstance = ok1(ItemInstance)
        ;
            MaybeTypes = error1(Specs),
            MaybeItemInstance = error1(Specs)
        )
    ;
        MaybeClassName = error2(Specs),
        MaybeItemInstance = error1(Specs)
    ).

:- pred parse_non_empty_instance(module_name::in, term::in, term::in,
    varset::in, tvarset::in, prog_context::in, int::in,
    maybe1(item_instance_info)::out) is det.

parse_non_empty_instance(ModuleName, Name, Methods, VarSet, TVarSet, Context,
        SeqNum, MaybeItemInstance) :-
    parse_instance_methods(ModuleName, Methods, VarSet, MaybeParsedMethods),
    (
        MaybeParsedMethods = ok1(MethodList),
        parse_instance_name(ModuleName, Name, TVarSet, Context, SeqNum,
            MaybeItemInstance0),
        (
            MaybeItemInstance0 = error1(Specs),
            MaybeItemInstance = error1(Specs)
        ;
            MaybeItemInstance0 = ok1(ItemInstance0),
            % XXX Should we keep InstanceContext, or should we replace it
            % with Context? Or will they always be the same?
            ItemInstance0 = item_instance_info(Constraints, NameString,
                Types, OriginalTypes, _, _,
                ModName, InstanceContext, ItemSeqNum),
            ItemInstance = item_instance_info(Constraints, NameString,
                Types, OriginalTypes,
                instance_body_concrete(MethodList), TVarSet,
                ModName, InstanceContext, ItemSeqNum),
            MaybeItemInstance1 = ok1(ItemInstance),
            check_tvars_in_instance_constraint(MaybeItemInstance1, Name,
                MaybeItemInstance)
        )
    ;
        MaybeParsedMethods = error1(Specs),
        MaybeItemInstance = error1(Specs)
    ).

:- pred check_tvars_in_instance_constraint(maybe1(item_instance_info)::in,
    term::in, maybe1(item_instance_info)::out) is det.

check_tvars_in_instance_constraint(error1(Specs), _, error1(Specs)).
check_tvars_in_instance_constraint(ok1(ItemInstance), InstanceTerm, Result) :-
    % XXX
    ItemInstance = item_instance_info(Constraints, _Name, Types,
        _OriginalTypes, _Methods, TVarSet, _ModName, _Context, _SeqNum),
    % Check that all of the type variables in the constraints on the instance
    % declaration also occur in the type class argument types in the instance
    % declaration.
    (
        prog_type.constraint_list_get_tvars(Constraints, TVars),
        type_vars_list(Types, TypesVars),
        list.filter(is_in_list(TypesVars), TVars, _BoundTVars, UnboundTVars),
        UnboundTVars = [_ | _]
    ->
        UnboundTVarStrs =
            list.map(mercury_var_to_string(TVarSet, no), UnboundTVars),
        UnboundTVarPieces = list_to_pieces(UnboundTVarStrs),
        ( list.length(UnboundTVars) = 1 ->
            Prefix = [words("Error: unbound type variable")]
        ;
            Prefix = [words("Error: unbound type variables")]
        ),
        Pieces = Prefix ++ UnboundTVarPieces ++
            [words("in constraints on instance declaration."), nl],
        % XXX Would _Context be better than get_term_context(InstanceTerm)?
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(InstanceTerm), [always(Pieces)])]),
        Result = error1([Spec])
    ;
        Result = ok1(ItemInstance)
    ).

:- pred parse_instance_methods(module_name::in, term::in, varset::in,
    maybe1(instance_methods)::out) is det.

parse_instance_methods(ModuleName, MethodsTerm, VarSet, Result) :-
    ( list_term_to_term_list(MethodsTerm, MethodList) ->
        % Convert the list of terms into a list of maybe1(class_method)s.
        list.map(term_to_instance_method(ModuleName, VarSet), MethodList,
            Interface),
        find_errors(Interface, Result)
    ;
        Pieces = [words("Error: expected list of instance methods."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(MethodsTerm), [always(Pieces)])]),
        Result = error1([Spec])
    ).

    % Turn the term into a method instance.
    %
:- pred term_to_instance_method(module_name::in, varset::in, term::in,
    maybe1(instance_method)::out) is det.

term_to_instance_method(_ModuleName, VarSet, MethodTerm,
        MaybeInstanceMethod) :-
    (
        MethodTerm = term.functor(term.atom("is"),
            [ClassMethodTerm, InstanceMethodTerm], TermContext)
    ->
        % Note that the codes for 'pred(...)' and 'func(...)' are very similar.
        % Unfortunately, factoring out the common code would not really
        % simplify things.
        (
            ClassMethodTerm = term.functor(term.atom("pred"), [SlashTerm], _),
            SlashTerm = term.functor(term.atom("/"),
                [PredNameTerm, ArityTerm], _)
        ->
            (
                try_parse_sym_name_and_no_args(PredNameTerm, PredName),
                ArityTerm = term.functor(term.integer(ArityInt), [], _),
                try_parse_sym_name_and_no_args(InstanceMethodTerm,
                    InstanceMethodName)
            ->
                InstanceMethod = instance_method(pf_predicate, PredName,
                    instance_proc_def_name(InstanceMethodName), ArityInt,
                    TermContext),
                MaybeInstanceMethod = ok1(InstanceMethod)
            ;
                MethodTermStr = describe_error_term(VarSet, MethodTerm),
                Pieces = [words("Error: expected"),
                    quote("pred(<Name> / <Arity>) is <InstanceMethod>"),
                    suffix(","),
                    words("not"), words(MethodTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(MethodTerm),
                        [always(Pieces)])]),
                MaybeInstanceMethod = error1([Spec])
            )
        ;
            ClassMethodTerm = term.functor(term.atom("func"), [SlashTerm], _),
            SlashTerm = term.functor(term.atom("/"),
                [FuncNameTerm, ArityTerm], _)
        ->
            (
                try_parse_sym_name_and_no_args(FuncNameTerm, FuncName),
                ArityTerm = term.functor(term.integer(ArityInt), [], _),
                try_parse_sym_name_and_no_args(InstanceMethodTerm,
                    InstanceMethodName)
            ->
                InstanceMethod = instance_method(pf_function, FuncName,
                    instance_proc_def_name(InstanceMethodName), ArityInt,
                    TermContext),
                MaybeInstanceMethod = ok1(InstanceMethod)
            ;
                MethodTermStr = describe_error_term(VarSet, MethodTerm),
                Pieces = [words("Error: expected"),
                    quote("func(<Name> / <Arity>) is <InstanceMethod>"),
                    suffix(","),
                    words("not"), words(MethodTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(MethodTerm),
                        [always(Pieces)])]),
                MaybeInstanceMethod = error1([Spec])
            )
        ;
            MethodTermStr = describe_error_term(VarSet, MethodTerm),
            Pieces = [words("Error: expected"),
                quote("pred(<Name> / <Arity>) is <InstanceName>"),
                words("or"),
                quote("func(<Name> / <Arity>) is <InstanceName>"),
                suffix(","),
                words("not"), words(MethodTermStr), suffix("."), nl],
            Spec = error_spec(severity_error, phase_term_to_parse_tree,
                [simple_msg(get_term_context(MethodTerm), [always(Pieces)])]),
            MaybeInstanceMethod = error1([Spec])
        )
    ;
        % For the clauses in an instance declaration, the default module name
        % for the clause heads is the module name of the class that this is an
        % instance declaration for, but we don't necessarily know what module
        % that is at this point, since the class name hasn't been fully
        % qualified yet. So here we give the special module name "" as the
        % default, which means that there is no default. (If the module
        % qualifiers in the clauses don't match the module name of the class,
        % we will pick that up later, in check_typeclass.m.)

        DefaultModuleName = unqualified(""),
        parse_item(DefaultModuleName, VarSet, MethodTerm, -1, MaybeItem0),
        (
            MaybeItem0 = error1(Specs),
            MaybeInstanceMethod = error1(Specs)
        ;
            MaybeItem0 = ok1(Item),
            ( Item = item_clause(ItemClause) ->
                ItemClause = item_clause_info(_Origin, _VarNames, PredOrFunc,
                    ClassMethodName, HeadArgs, _ClauseBody, Context, _SeqNum),
                adjust_func_arity(PredOrFunc, ArityInt, list.length(HeadArgs)),
                InstanceMethod = instance_method(PredOrFunc, ClassMethodName,
                    instance_proc_def_clauses([ItemClause]), ArityInt,
                    Context),
                MaybeInstanceMethod = ok1(InstanceMethod)
            ;
                MethodTermStr = describe_error_term(VarSet, MethodTerm),
                Pieces = [words("Error: expected clause or"),
                    quote("pred(<Name> / <Arity>) is <InstanceName>"),
                    words("or"),
                    quote("func(<Name> / <Arity>) is <InstanceName>"),
                    suffix(","),
                    words("not"), words(MethodTermStr), suffix("."), nl],
                Spec = error_spec(severity_error, phase_term_to_parse_tree,
                    [simple_msg(get_term_context(MethodTerm),
                        [always(Pieces)])]),
                MaybeInstanceMethod = error1([Spec])
            )
        )
    ).

%----------------------------------------------------------------------------%

:- pred is_in_list(list(T)::in, T::in) is semidet.

is_in_list(List, Element) :-
    list.member(Element, List).

%----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_typeclass.
%----------------------------------------------------------------------------%
