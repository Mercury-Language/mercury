%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2006 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_io_typeclass.m.
% Main authors: dgj.

% This module handles the parsing of typeclass declarations.
% Perhaps some of this should go into prog_io_util.m?

%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_typeclass.
:- interface.

:- import_module mdbcomp.prim_data.
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
    maybe1(item)::out) is semidet.

    % Parse an instance declaration.
    %
:- pred parse_instance(module_name::in, varset::in, list(term)::in,
    maybe1(item)::out) is semidet.

    % Parse a list of class constraints.
    %
:- pred parse_class_constraints(module_name::in, term::in,
    maybe1(list(prog_constraint))::out) is det.

    % Parse a list of class and inst constraints.
    %
:- pred parse_class_and_inst_constraints(module_name::in, term::in,
    maybe_class_and_inst_constraints::out) is det.

:- type maybe_class_and_inst_constraints ==
    maybe2(list(prog_constraint), inst_var_sub).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

parse_typeclass(ModuleName, VarSet, TypeClassTerm, Result) :-
    % XXX We should return an error if we get more than one arg, instead of
    % failing.
    TypeClassTerm = [Arg],
    ( Arg = term.functor(term.atom("where"), [Name, Methods], _) ->
        parse_non_empty_class(ModuleName, Name, Methods, VarSet, Result)
    ;
        parse_class_head(ModuleName, Arg, VarSet, Result)
    ).

:- pred parse_non_empty_class(module_name::in, term::in, term::in, varset::in,
    maybe1(item)::out) is det.

parse_non_empty_class(ModuleName, Name, Methods, VarSet, Result) :-
    varset.coerce(VarSet, TVarSet),
    parse_class_methods(ModuleName, Methods, VarSet, MaybeParsedMethods),
    (
        MaybeParsedMethods = ok1(MethodList),
        parse_class_head(ModuleName, Name, VarSet, MaybeParsedNameAndVars),
        (
            MaybeParsedNameAndVars = error1(Errors),
            Result = error1(Errors)
        ;
            MaybeParsedNameAndVars = ok1(ParsedNameAndVars),
            ( ParsedNameAndVars = item_typeclass(_, _, _, _, _, _) ->
                Result = ok1((ParsedNameAndVars
                    ^ tc_class_methods := class_interface_concrete(MethodList))
                    ^ tc_varset := TVarSet)
            ;
                % If the item we get back isn't a typeclass,
                % something has gone wrong...
                unexpected(this_file, "item should be a typeclass")
            )
        )
    ;
        MaybeParsedMethods = error1(Errors),
        Result = error1(Errors)
    ).

:- pred parse_class_head(module_name::in, term::in, varset::in,
    maybe1(item)::out) is det.

parse_class_head(ModuleName, Arg, VarSet, Result) :-
    ( Arg = term.functor(term.atom("<="), [Name, Constraints], _) ->
        parse_constrained_class(ModuleName, Name, Constraints, VarSet, Result)
    ;
        varset.coerce(VarSet, TVarSet),
        parse_unconstrained_class(ModuleName, Arg, TVarSet, Result)
    ).

:- pred parse_constrained_class(module_name::in, term::in, term::in,
    varset::in, maybe1(item)::out) is det.

parse_constrained_class(ModuleName, Decl, Constraints, VarSet, Result) :-
    varset.coerce(VarSet, TVarSet),
    parse_superclass_constraints(ModuleName, Constraints,
        MaybeParsedConstraints),
    (
        MaybeParsedConstraints = ok2(ConstraintList, FunDeps),
        parse_unconstrained_class(ModuleName, Decl, TVarSet, Result0),
        (
            Result0 = error1(_),
            Result = Result0
        ;
            Result0 = ok1(Item),
            ( Item = item_typeclass(_, _, _, _, _, _) ->
                (
                    % Check for type variables in the constraints which do not
                    % occur in the type class parameters.
                    prog_type.constraint_list_get_tvars(ConstraintList,
                        ConstrainedVars),
                    list.member(Var, ConstrainedVars),
                    \+ list.member(Var, Item ^ tc_class_params)
                ->
                    Msg = "type variable in superclass constraint " ++
                        "is not a parameter of this type class",
                    Result = error1([Msg - Constraints])
                ;
                    % Check for type variables in the fundeps which do not
                    % occur in the type class parameters.
                    list.member(FunDep, FunDeps),
                    FunDep = fundep(Domain, Range),
                    (
                        list.member(Var, Domain)
                    ;
                        list.member(Var, Range)
                    ),
                    \+ list.member(Var, Item ^ tc_class_params)
                ->
                    Msg = "type variable in functional dependency " ++
                        "is not a parameter of this type class",
                    Result = error1([Msg - Constraints])
                ;
                    Result = ok1((Item
                        ^ tc_constraints := ConstraintList)
                        ^ tc_fundeps := FunDeps)
                )
            ;
                % If the item we get back isn't a typeclass,
                % something has gone wrong...
                unexpected(this_file, "item should be a typeclass")
            )
        )
    ;
        MaybeParsedConstraints = error2(Errors),
        Result = error1(Errors)
    ).

:- pred parse_superclass_constraints(module_name::in, term::in,
    maybe2(list(prog_constraint), list(prog_fundep))::out) is det.

parse_superclass_constraints(_ModuleName, ConstraintsTerm, Result) :-
    parse_arbitrary_constraints(ConstraintsTerm, Result0),
    (
        Result0 = ok1(ArbitraryConstraints),
        (
            collect_simple_and_fundep_constraints(ArbitraryConstraints,
                Constraints, FunDeps)
        ->
            Result = ok2(Constraints, FunDeps)
        ;
            Msg = "constraints on class declarations" ++
                " may only constrain type variables and ground types",
            Result = error2([Msg - ConstraintsTerm])
        )
    ;
        Result0 = error1(Errors),
        Result = error2(Errors)
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
    maybe1(item)::out) is det.

parse_unconstrained_class(ModuleName, Name, TVarSet, Result) :-
    parse_implicitly_qualified_term(ModuleName,
        Name, Name, "typeclass declaration", MaybeClassName),
    (
        MaybeClassName = ok2(ClassName, TermVars0),
        list.map(term.coerce, TermVars0, TermVars),
        (
            term.var_list_to_term_list(Vars, TermVars),
            list.sort_and_remove_dups(TermVars, SortedTermVars),
            list.length(SortedTermVars) = list.length(TermVars) : int
        ->
            Result = ok1(item_typeclass([], [], ClassName, Vars,
                class_interface_abstract, TVarSet))
        ;
            Msg = "expected distinct variables as class parameters",
            Result = error1([Msg - Name])
        )
    ;
        MaybeClassName = error2(Errors),
        Result = error1(Errors)
    ).

:- pred parse_class_methods(module_name::in, term::in, varset::in,
    maybe1(class_methods)::out) is det.

parse_class_methods(ModuleName, Methods, VarSet, Result) :-
    (
        % Convert the list of terms into a list of maybe1(class_method)s.
        list_term_to_term_list(Methods, MethodList)
    ->
        list.map(
            (pred(MethodTerm::in, Method::out) is det :-
                % Turn the term into an item.
                parse_decl(ModuleName, VarSet, MethodTerm, Item),
                % Turn the item into a class_method.
                item_to_class_method(Item, MethodTerm, Method)
            ), MethodList, Interface),
        find_errors(Interface, Result)
    ;
        Result = error1(["expected list of class methods" - Methods])
    ).


:- pred item_to_class_method(maybe2(item, prog_context)::in, term::in,
    maybe1(class_method)::out) is det.

item_to_class_method(error2(Errors), _, error1(Errors)).
item_to_class_method(ok2(Item, Context), Term, Result) :-
    ( Item = item_pred_or_func(_Origin, A, B, C, D, E, F, G, H, I, J, K, L) ->
        Result = ok1(method_pred_or_func(A, B, C, D, E, F, G, H, I, J, K, L,
            Context))
    ; Item = item_pred_or_func_mode(A, B, C, D, E, F, G) ->
        Result = ok1(method_pred_or_func_mode(A, B, C, D, E, F, G, Context))
    ;
        Msg = "Only pred, func and mode declarations " ++
            "allowed in class interface",
        Result = error1([Msg - Term])
    ).

    % From a list of maybe1s, search through until you find an error.
    % If some errors are found, error1(their union).
    % If no error is found, return ok1(the original elements).
    %
:- pred find_errors(list(maybe1(T))::in, maybe1(list(T))::out) is det.

find_errors(Xs, Result) :-
    find_errors_2(Xs, [], Methods, [], Errors),
    (
        Errors = [],
        Result = ok1(Methods)
    ;
        Errors = [_ | _],
        Result = error1(Errors)
    ).

:- pred find_errors_2(list(maybe1(T))::in, list(T)::in, list(T)::out,
    assoc_list(string, term)::in, assoc_list(string, term)::out) is det.

find_errors_2([], !Methods, !Errors).
find_errors_2([X | Xs], !Methods, !Errors) :-
    find_errors_2(Xs, !Methods, !Errors),
    (
        X = ok1(CurMethod),
        !:Methods = [CurMethod | !.Methods]
    ;
        X = error1(CurErrors),
        !:Errors = CurErrors ++ !.Errors
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for parsing various kinds of constraints.
%

    % Parse constraints on a pred or func declaration, or on an existentially
    % quantified type definition. Currently all such constraints must be
    % simple.
    %
parse_class_constraints(ModuleName, ConstraintsTerm, Result) :-
    ErrorMessage = "sorry, not implemented:" ++
        " constraints may only constrain type variables" ++
        " and ground types",
    parse_simple_class_constraints(ModuleName, ConstraintsTerm,
        ErrorMessage, Result).

:- pred parse_simple_class_constraints(module_name::in, term::in, string::in,
    maybe1(list(prog_constraint))::out) is det.

parse_simple_class_constraints(_ModuleName, ConstraintsTerm, ErrorMessage,
        Result) :-
    parse_arbitrary_constraints(ConstraintsTerm, Result0),
    (
        Result0 = ok1(ArbitraryConstraints),
        (
            % Fail if any of the constraints aren't simple.
            list.map(get_simple_constraint, ArbitraryConstraints, Constraints)
        ->
            Result = ok1(Constraints)
        ;
            Result = error1([ErrorMessage - ConstraintsTerm])
        )
    ;
        Result0 = error1(Errors),
        Result = error1(Errors)
    ).

:- pred get_simple_constraint(arbitrary_constraint::in, prog_constraint::out)
    is semidet.

get_simple_constraint(simple(Constraint), Constraint).

parse_class_and_inst_constraints(_ModuleName, ConstraintsTerm, Result) :-
    parse_arbitrary_constraints(ConstraintsTerm, Result0),
    (
        Result0 = ok1(ArbitraryConstraints),
        (
            collect_class_and_inst_constraints(ArbitraryConstraints,
                ProgConstraints, InstVarSub)
        ->
            Result = ok2(ProgConstraints, InstVarSub)
        ;
            ErrorMessage = "functional dependencies are only allowed " ++
                "in typeclass declarations",
            Result = error2([ErrorMessage - ConstraintsTerm])
        )
    ;
        Result0 = error1(Errors),
        Result = error2(Errors)
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
        map.set(InstVarSub0, InstVar, Inst, InstVarSub),
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

:- pred parse_arbitrary_constraints(term::in,
    maybe1(arbitrary_constraints)::out) is det.

parse_arbitrary_constraints(ConstraintsTerm, Result) :-
    conjunction_to_list(ConstraintsTerm, ConstraintList),
    parse_arbitrary_constraint_list(ConstraintList, Result).

:- pred parse_arbitrary_constraint_list(list(term)::in,
    maybe1(arbitrary_constraints)::out) is det.

parse_arbitrary_constraint_list([], ok1([])).
parse_arbitrary_constraint_list([Term | Terms], Result) :-
    parse_arbitrary_constraint(Term, Result0),
    parse_arbitrary_constraint_list(Terms, Result1),
    Result = combine_parse_results(Result0, Result1).

:- func combine_parse_results(maybe1(arbitrary_constraint),
    maybe1(arbitrary_constraints)) = maybe1(arbitrary_constraints).

combine_parse_results(error1(Errors1), error1(Errors2)) =
    error1(Errors1 ++ Errors2).
combine_parse_results(error1(Errors), ok1(_)) = error1(Errors).
combine_parse_results(ok1(_), error1(Errors)) = error1(Errors).
combine_parse_results(ok1(Constraint), ok1(Constraints)) =
    ok1([Constraint | Constraints]).

:- pred parse_arbitrary_constraint(term::in, maybe1(arbitrary_constraint)::out)
    is det.

parse_arbitrary_constraint(ConstraintTerm, Result) :-
    (
        parse_inst_constraint(ConstraintTerm, InstVar, Inst)
    ->
        Result = ok1(inst_constraint(InstVar, Inst))
    ;
        parse_fundep(ConstraintTerm, Result0)
    ->
        Result = Result0
    ;
        parse_qualified_term(ConstraintTerm, ConstraintTerm,
            "class constraint", ok2(ClassName, Args0))
    ->
        parse_types(Args0, ArgsResult),
        (
            ArgsResult = ok1(Args),
            Constraint = constraint(ClassName, Args),
            ( constraint_is_not_simple(Constraint) ->
                Result = ok1(non_simple(Constraint))
            ;
                Result = ok1(simple(Constraint))
            )
        ;
            ArgsResult = error1(Errors),
            Result = error1(Errors)
        )
    ;
        Msg = "expected atom as class name or inst constraint",
        Result = error1([Msg - ConstraintTerm])
    ).

:- pred parse_inst_constraint(term::in, inst_var::out, mer_inst::out)
    is semidet.

parse_inst_constraint(Term, InstVar, Inst) :-
    Term = term.functor(term.atom("=<"), [Arg1, Arg2], _),
    Arg1 = term.variable(InstVar0),
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
        ErrorMessage = "domain and range of functional dependency " ++
            "must be comma-separated lists of variables",
        Result = error1([ErrorMessage - Term])
    ).

:- pred parse_fundep_2(term::in, list(tvar)::out) is semidet.

parse_fundep_2(Term, TVars) :-
    TypeTerm = term.coerce(Term),
    conjunction_to_list(TypeTerm, List),
    term.var_list_to_term_list(TVars, List).

:- pred constraint_is_not_simple(prog_constraint::in) is semidet.

constraint_is_not_simple(constraint(_Name, Types)) :-
    some [Type] (
        list.member(Type, Types),
        type_is_nonvar(Type),
        type_is_nonground(Type)
    ).

%-----------------------------------------------------------------------------%

parse_instance(ModuleName, VarSet, TypeClassTerm, Result) :-
    % XXX We should return an error if we get more than one arg,
    % instead of failing.
    TypeClassTerm = [Arg],
    varset.coerce(VarSet, TVarSet),
    ( Arg = term.functor(term.atom("where"), [Name, Methods], _) ->
        parse_non_empty_instance(ModuleName, Name, Methods, VarSet, TVarSet,
            Result)
    ;
        parse_instance_name(ModuleName, Arg, TVarSet, Result)
    ).

:- pred parse_instance_name(module_name::in, term::in, tvarset::in,
    maybe1(item)::out) is det.

parse_instance_name(ModuleName, Arg, TVarSet, Result) :-
    ( Arg = term.functor(term.atom("<="), [Name, Constraints], _) ->
        parse_derived_instance(ModuleName, Name, Constraints, TVarSet, Result)
    ;
        parse_underived_instance(ModuleName, Arg, TVarSet, Result)
    ).

:- pred parse_derived_instance(module_name::in, term::in, term::in,
    tvarset::in, maybe1(item)::out) is det.

parse_derived_instance(ModuleName, Decl, Constraints, TVarSet, Result) :-
    parse_instance_constraints(ModuleName, Constraints,
        MaybeParsedConstraints),
    (
        MaybeParsedConstraints = ok1(ConstraintList),
        parse_underived_instance(ModuleName, Decl, TVarSet, Result0),
        (
            Result0 = error1(_),
            Result = Result0
        ;
            Result0 = ok1(Item),
            ( Item = item_instance(_, Name, Types, Body, VarSet, ModName) ->
                Result = ok1(item_instance(ConstraintList, Name, Types, Body,
                    VarSet, ModName))
            ;
                % If the item we get back isn't an instance,
                % something has gone wrong...
                % Maybe we should use cleverer inst decls to avoid
                % this call to error.
                unexpected(this_file, "item should be an instance")
            )
        )
    ;
        MaybeParsedConstraints = error1(Errors),
        Result = error1(Errors)
    ).

:- pred parse_instance_constraints(module_name::in, term::in,
    maybe1(list(prog_constraint))::out) is det.

parse_instance_constraints(ModuleName, Constraints, Result) :-
    parse_simple_class_constraints(ModuleName, Constraints,
        "constraints on instance declarations may only constrain " ++
        "type variables and ground types", Result).

:- pred parse_underived_instance(module_name::in, term::in, tvarset::in,
    maybe1(item)::out) is det.

parse_underived_instance(ModuleName, Name, TVarSet, Result) :-
    % We don't give a default module name here since the instance declaration
    % could well be for a typeclass defined in another module.
    parse_qualified_term(Name, Name, "instance declaration", MaybeClassName),
    (
        MaybeClassName = ok2(ClassName, TermTypes),
        parse_types(TermTypes, TypesResult),
        parse_underived_instance_2(Name, ClassName, TypesResult, TVarSet,
            ModuleName, Result)
    ;
        MaybeClassName = error2(Errors),
        Result = error1(Errors)
    ).

:- pred parse_underived_instance_2(term::in, class_name::in,
    maybe1(list(mer_type))::in, tvarset::in, module_name::in,
    maybe1(item)::out) is det.

parse_underived_instance_2(_, _, error1(Errors), _, _, error1(Errors)).
parse_underived_instance_2(ErrorTerm, ClassName, ok1(Types), TVarSet,
        ModuleName, Result) :-
    (
        % Check that each type in the arguments of the instance decl
        % is a functor with vars as args...
        all [Type] (
            list.member(Type, Types)
        =>
            type_is_functor_and_vars(Type)
        ),
        % ...and that the vars are distinct across the entire arg list.
        type_vars_are_distinct(Types)
    ->
        Result = ok1(item_instance([], ClassName, Types,
            instance_body_abstract, TVarSet, ModuleName))
    ;
        Msg = "types in instance declarations must be functors " ++
            "with distinct variables as arguments",
        Result = error1([Msg - ErrorTerm])
    ).

:- pred type_is_functor_and_vars(mer_type::in) is semidet.

type_is_functor_and_vars(defined_type(_, Args, _)) :-
    functor_args_are_variables(Args).
type_is_functor_and_vars(builtin_type(_)).
type_is_functor_and_vars(higher_order_type(Args, MaybeRet, Purity,
        EvalMethod)) :-
    % XXX We currently allow pred types to be instance arguments, but not
    % func types. Even then, the pred type must be pure and have a
    % lambda_eval_method of normal. We keep this behaviour basically
    % for backwards compatibility -- there is little point fixing this
    % now without fixing the more general problem of having these
    % restrictions in the first place.
    MaybeRet = no,
    Purity = purity_pure,
    EvalMethod = lambda_normal,
    functor_args_are_variables(Args).
type_is_functor_and_vars(tuple_type(Args, _)) :-
    functor_args_are_variables(Args).
type_is_functor_and_vars(kinded_type(Type, _)) :-
    type_is_functor_and_vars(Type).

:- pred functor_args_are_variables(list(mer_type)::in) is semidet.

functor_args_are_variables(Args) :-
    all [Arg] (
        list.member(Arg, Args)
    =>
        type_is_var(Arg)
    ).

:- pred type_vars_are_distinct(list(mer_type)::in) is semidet.

type_vars_are_distinct(Types) :-
    promise_equivalent_solutions [NumVars, VarsWithoutDups] (
        solutions.unsorted_solutions(type_list_contains_var(Types), Vars),
        list.length(Vars, NumVars),
        list.sort_and_remove_dups(Vars, VarsWithoutDups)
    ),
    list.length(VarsWithoutDups, NumVars).

:- pred parse_non_empty_instance(module_name::in, term::in, term::in,
    varset::in, tvarset::in, maybe1(item)::out) is det.

parse_non_empty_instance(ModuleName, Name, Methods, VarSet, TVarSet, Result) :-
    parse_instance_methods(ModuleName, Methods, VarSet, MaybeParsedMethods),
    (
        MaybeParsedMethods = ok1(MethodList),
        parse_instance_name(ModuleName, Name, TVarSet,
            MaybeParsedNameAndTypes),
        (
            MaybeParsedNameAndTypes = error1(Errors),
            Result = error1(Errors)
        ;
            MaybeParsedNameAndTypes = ok1(ParsedNameAndTypes),
            (
                ParsedNameAndTypes = item_instance(Constraints, NameString,
                    Types, _, _, ModName)
            ->
                Result0 = ok1(item_instance(Constraints, NameString, Types,
                    instance_body_concrete(MethodList), TVarSet, ModName)),
                check_tvars_in_instance_constraint(Result0, Name, Result)
            ;
                % If the item we get back isn't a typeclass,
                % something has gone wrong...
                unexpected(this_file, "item should be an instance")
            )
        )
    ;
        MaybeParsedMethods = error1(Errors),
        Result = error1(Errors)
    ).

:- pred check_tvars_in_instance_constraint(maybe1(item)::in, term::in,
    maybe1(item)::out) is det.

check_tvars_in_instance_constraint(error1(Errors), _, error1(Errors)).
check_tvars_in_instance_constraint(ok1(Item), InstanceTerm, Result) :-
    (
        Item = item_instance(Constraints, _Name, Types, _Methods, _TVarSet,
            _ModName)
    ->
        % Check that all of the type variables in the constraints
        % on the instance declaration also occur in the type class
        % argument types in the instance declaration.
        (
            prog_type.constraint_list_get_tvars(Constraints, TVars),
            list.member(TVar, TVars),
            \+ type_list_contains_var(Types, TVar)
        ->
            Msg = "unbound type variable(s) " ++
                "in constraints on instance declaration",
            Result = error1([Msg - InstanceTerm])
        ;
            Result = ok1(Item)
        )
    ;
        unexpected(this_file,
            "check_tvars_in_constraint: expecting instance item")
    ).

:- pred parse_instance_methods(module_name::in, term::in, varset::in,
    maybe1(instance_methods)::out) is det.

parse_instance_methods(ModuleName, Methods, VarSet, Result) :-
    ( list_term_to_term_list(Methods, MethodList) ->
        % Convert the list of terms into a list of maybe1(class_method)s.
        list.map(term_to_instance_method(ModuleName, VarSet), MethodList,
            Interface),
        find_errors(Interface, Result)
    ;
        Result = error1(["expected list of instance methods" - Methods])
    ).

    % Turn the term into a method instance.
    %
:- pred term_to_instance_method(module_name::in, varset::in, term::in,
    maybe1(instance_method)::out) is det.

term_to_instance_method(_ModuleName, VarSet, MethodTerm, Result) :-
    (
        MethodTerm = term.functor(term.atom("is"),
            [ClassMethodTerm, InstanceMethod], TermContext)
    ->
        (
            ClassMethodTerm = term.functor(term.atom("pred"),
                [term.functor(term.atom("/"), [ClassMethod, Arity], _)], _)
        ->
            (
                parse_qualified_term(ClassMethod, ClassMethod,
                    "instance method", ok2(ClassMethodName, [])),
                Arity = term.functor(term.integer(ArityInt), [], _),
                parse_qualified_term(InstanceMethod, InstanceMethod,
                    "instance method", ok2(InstanceMethodName, []))
            ->
                Result = ok1(instance_method(predicate, ClassMethodName,
                    instance_proc_def_name(InstanceMethodName), ArityInt,
                    TermContext))
            ;
                Msg = "expected `pred(<Name> / <Arity>) is <InstanceMethod>'",
                Result = error1([Msg - MethodTerm])
            )
        ;
            ClassMethodTerm = term.functor(term.atom("func"),
                [term.functor(term.atom("/"), [ClassMethod, Arity], _)], _)
        ->
            (
                parse_qualified_term(ClassMethod, ClassMethod,
                    "instance method", ok2(ClassMethodName, [])),
                Arity = term.functor(term.integer(ArityInt), [], _),
                parse_qualified_term(InstanceMethod, InstanceMethod,
                    "instance method", ok2(InstanceMethodName, []))
            ->
                Result = ok1(instance_method(function, ClassMethodName,
                    instance_proc_def_name(InstanceMethodName), ArityInt,
                    TermContext))
            ;
                Msg = "expected `func(<Name> / <Arity>) is <InstanceMethod>'",
                Result = error1([Msg - MethodTerm])
            )
        ;
            Msg = "expected `pred(<Name> / <Arity>) is <InstanceName>'",
            Result = error1([Msg - MethodTerm])
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
        parse_item(DefaultModuleName, VarSet, MethodTerm, Result0),
        (
            Result0 = error2(Errors),
            Result = error1(Errors)
        ;
            Result0 = ok2(Item, Context),
            (
                Item = item_clause(_Origin, _VarNames, PredOrFunc,
                    ClassMethodName, HeadArgs, _ClauseBody)
            ->
                adjust_func_arity(PredOrFunc, ArityInt, list.length(HeadArgs)),
                Result = ok1(instance_method(PredOrFunc, ClassMethodName,
                    instance_proc_def_clauses([Item]), ArityInt, Context))
            ;
                Msg = "expected clause or " ++
                    "`pred(<Name> / <Arity>) is <InstanceName>' or " ++
                    "`func(<Name> / <Arity>) is <InstanceName>')",
                Result = error1([Msg - MethodTerm])
            )
        )
    ).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_io_typeclass.m".

%----------------------------------------------------------------------------%
