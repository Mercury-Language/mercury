%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_util.m.
% Main author: fjh.
%
% This module defines the types used by prog_io and its subcontractors
% to return the results of parsing, and some utility predicates needed
% by several of prog_io's submodules.
%
% Most parsing predicates must check for errors. They return either the
% item(s) they were looking for, or an error indication.
%
% Most of the parsing predicates return a `maybe1(T)' or a `maybe2(T1, T2)',
% which will either be the `ok(ParseTree)' (or `ok(ParseTree1, ParseTree2)'),
% if the parse is successful, or `error(Message, Term)' if it is not.
% The `Term' there should be the term which is syntactically incorrect.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_util.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- type maybe_functor    ==  maybe_functor(generic).
:- type maybe_functor(T) ==  maybe2(sym_name, list(term(T))).

    % ok(SymName, Args - MaybeFuncRetArg) ; error(Msg, Term).
:- type maybe_pred_or_func(T) == maybe2(sym_name, pair(list(T), maybe(T))).

:- type var2tvar == map(var, tvar).
:- type var2pvar == map(var, prog_var).

:- type parser(T) == pred(term, maybe1(T)).
:- mode parser == (pred(in, out) is det).

% Various predicates to parse small bits of syntax.
% Some of these predicates fail if they encounter a syntax error; others
% return an error indication.

:- pred parse_list_of_vars(term(T)::in, list(var(T))::out) is semidet.

    % Parse a list of quantified variables.
    % The other input argument is a prefix for any error messages.
    %
:- pred parse_vars(term(T)::in, varset(T)::in, list(format_component)::in,
    maybe1(list(var(T)))::out) is det.

    % Parse a list of quantified variables, splitting it into
    % ordinary logic variables and state variables respectively.
    % The other input argument is a prefix for any error messages.
    %
:- pred parse_quantifier_vars(term(T)::in, varset(T)::in,
    list(format_component)::in, maybe2(list(var(T)), list(var(T)))::out)
    is det.

    % Similar to parse_vars, but also allow state variables to appear
    % in the list. The outputs separate the parsed variables into ordinary
    % variables, state variables listed as !.X, and state variables
    % listed as !:X.
    %
:- pred parse_vars_and_state_vars(term(T)::in, varset(T)::in,
    list(format_component)::in,
    maybe4(list(var(T)), list(var(T)), list(var(T)), list(var(T)))::out)
    is det.

:- pred parse_name_and_arity(module_name::in, term(T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_name_and_arity_unqualified(term(T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_name_and_arity(term(T)::in,
    pred_or_func::out, sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_and_args(term(_T)::in,
    pred_or_func::out, sym_name::out, list(term(_T))::out) is semidet.

:- pred parse_pred_or_func_and_args_general(maybe(module_name)::in,
    term(_T)::in, varset(_T)::in, list(format_component)::in,
    maybe_pred_or_func(term(_T))::out) is det.

:- pred maybe_parse_type(term::in, mer_type::out) is semidet.

:- pred parse_type(term::in, varset::in, list(format_component)::in,
    maybe1(mer_type)::out) is det.

:- pred maybe_parse_types(list(term)::in, list(mer_type)::out) is semidet.

:- pred parse_types(list(term)::in, varset::in, list(format_component)::in,
    maybe1(list(mer_type))::out) is det.

:- pred is_known_type_name(string::in) is semidet.

:- pred unparse_type(mer_type::in, term::out) is det.

:- pred parse_purity_annotation(term(T)::in, purity::out, term(T)::out) is det.

:- type allow_constrained_inst_var
    --->    allow_constrained_inst_var
    ;       no_allow_constrained_inst_var.

:- pred convert_mode_list(allow_constrained_inst_var::in, list(term)::in,
    list(mer_mode)::out) is semidet.

:- pred convert_mode(allow_constrained_inst_var::in, term::in, mer_mode::out)
    is semidet.

:- pred convert_inst_list(allow_constrained_inst_var::in, list(term)::in,
    list(mer_inst)::out) is semidet.

:- pred convert_inst(allow_constrained_inst_var::in, term::in, mer_inst::out)
    is semidet.

:- pred is_known_mode_name(string::in) is semidet.
:- pred is_known_inst_name(string::in) is semidet.

:- pred standard_det(string::in, determinism::out) is semidet.

    % Convert a "disjunction" (bunch of terms separated by ';'s) to a list.
    %
:- pred disjunction_to_one_or_more(term(T)::in, one_or_more(term(T))::out)
    is det.
:- pred disjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % Convert a "conjunction" (bunch of terms separated by ','s) to a list.
    %
:- pred conjunction_to_one_or_more(term(T)::in, one_or_more(term(T))::out)
    is det.
:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % one_or_more_to_conjunction(Context, List, Term):
    %
    % Convert a nonempty list to a "conjunction", i.e. a bunch of terms
    % separated by commas.
    %
:- pred one_or_more_to_conjunction(prog_context::in, one_or_more(term(T))::in,
    term(T)::out) is det.

    % Convert a "sum", i.e. a bunch of terms separated by '+' operators
    % to a nonempty list.
    %
:- pred sum_to_one_or_more(term(T)::in, one_or_more(term(T))::out) is det.
:- pred sum_to_list(term(T)::in, list(term(T))::out) is det.

    % Parse a comma-separated list (misleading described as a "conjunction")
    % of things.
    %
:- pred parse_one_or_more(parser(T)::parser, term::in,
    maybe1(one_or_more(T))::out) is det.
:- pred parse_list(parser(T)::parser, term::in,
    maybe1(list(T))::out) is det.

:- pred map_parser(parser(T)::parser, list(term)::in, maybe1(list(T))::out)
    is det.

:- pred list_term_to_term_list(term::in, list(term)::out) is semidet.

%-----------------------------------------------------------------------------%

:- type decl_attribute
    --->    decl_attr_purity(purity)
    ;       decl_attr_quantifier(quantifier_type, list(var))
    ;       decl_attr_constraints(quantifier_type, term)
            % the term here is the (not yet parsed) list of constraints
    ;       decl_attr_solver_type.

:- type quantifier_type
    --->    quant_type_exist
    ;       quant_type_univ.

    % The term associated with each decl_attribute is the term containing
    % both the attribute and the declaration that that attribute modifies;
    % this term is used when printing out error messages for cases when
    % attributes are used on declarations where they are not allowed.
:- type decl_attrs == assoc_list(decl_attribute, term.context).

:- pred parse_decl_attribute(string::in, list(term)::in, decl_attribute::out,
    term::out) is semidet.

:- pred check_no_attributes(maybe1(T)::in, decl_attrs::in, maybe1(T)::out)
    is det.

:- func attribute_description(decl_attribute) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

parse_name_and_arity(ModuleName, PredAndArityTerm, SymName, Arity) :-
    PredAndArityTerm = term.functor(term.atom("/"),
        [PredNameTerm, ArityTerm], _),
    try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
        PredNameTerm, SymName),
    ArityTerm = term.functor(term.integer(Arity), [], _).

parse_name_and_arity_unqualified(PredAndArityTerm, SymName, Arity) :-
    parse_name_and_arity(unqualified(""), PredAndArityTerm, SymName, Arity).

parse_pred_or_func_name_and_arity(PorFPredAndArityTerm,
        PredOrFunc, SymName, Arity) :-
    PorFPredAndArityTerm = term.functor(term.atom(PredOrFuncStr), Args, _),
    ( PredOrFuncStr = "pred", PredOrFunc = pf_predicate
    ; PredOrFuncStr = "func", PredOrFunc = pf_function
    ),
    Args = [Arg],
    ModuleName = unqualified(""),
    parse_name_and_arity(ModuleName, Arg, SymName, Arity).

parse_pred_or_func_and_args(PredAndArgsTerm, PredOrFunc, SymName, ArgTerms) :-
    ( if
        PredAndArgsTerm = term.functor(term.atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    then
        try_parse_sym_name_and_args(FuncAndArgsTerm, SymName, ArgTerms0),
        PredOrFunc = pf_function,
        ArgTerms = ArgTerms0 ++ [FuncResultTerm]
    else
        try_parse_sym_name_and_args(PredAndArgsTerm, SymName, ArgTerms),
        PredOrFunc = pf_predicate
    ).

parse_pred_or_func_and_args_general(MaybeModuleName, PredAndArgsTerm,
        VarSet, ContextPieces, PredAndArgsResult) :-
    ( if
        PredAndArgsTerm = term.functor(term.atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    then
        FunctorTerm = FuncAndArgsTerm,
        MaybeFuncResult = yes(FuncResultTerm)
    else
        FunctorTerm = PredAndArgsTerm,
        MaybeFuncResult = no
    ),
    varset.coerce(VarSet, GenericVarSet),
    (
        MaybeModuleName = yes(ModuleName),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, FunctorTerm,
            GenericVarSet, ContextPieces, Result)
    ;
        MaybeModuleName = no,
        parse_sym_name_and_args(FunctorTerm, GenericVarSet, ContextPieces,
            Result)
    ),
    (
        Result = ok2(SymName, Args),
        PredAndArgsResult = ok2(SymName, Args - MaybeFuncResult)
    ;
        Result = error2(Specs),
        PredAndArgsResult = error2(Specs)
    ).

maybe_parse_type(Term, Type) :-
    % The values of VarSet and ContextPieces do not matter since we succeed
    % only if they aren't used.
    VarSet = varset.init,
    ContextPieces = [],
    parse_type(Term, VarSet, ContextPieces, ok1(Type)).

parse_type(Term, VarSet, ContextPieces, Result) :-
    % XXX kind inference: We currently give all types kind `star'.
    % This will be different when we have a kind system.
    (
        Term = term.variable(Var0, _),
        term.coerce_var(Var0, Var),
        Result = ok1(type_variable(Var, kind_star))
    ;
        Term = term.functor(Functor, ArgTerms, _),
        (
            ( Functor = term.integer(_)
            ; Functor = term.big_integer(_, _)
            ; Functor = term.float(_)
            ; Functor = term.string(_)
            ; Functor = term.implementation_defined(_)
            ),
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        ;
            Functor = term.atom(Name),
            % XXX We *could* generate a specific error message for each kind
            % of "ill-typed" term.
            ( if is_known_type_name_args(Name, ArgTerms, KnownTypeKind) then
                (
                    KnownTypeKind = known_type_simple(Type),
                    Result = ok1(Type)
                ;
                    KnownTypeKind = known_type_compound(CompoundTypeKind),
                    parse_compound_type(Term, VarSet, ContextPieces,
                        CompoundTypeKind, Result)
                ;
                    KnownTypeKind = known_type_bad_arity,
                    Result = ill_formed_type_result(ContextPieces, VarSet,
                        Term)
                )
            else
                % We don't support kind annotations yet, and we don't report
                % an error either. Perhaps we should?
                parse_sym_name_and_args(Term, VarSet, ContextPieces,
                    NameResult),
                (
                    NameResult = ok2(SymName, SymNameArgTerms),
                    parse_types(SymNameArgTerms, VarSet, ContextPieces,
                        SymNameArgsResult),
                    (
                        SymNameArgsResult = ok1(ArgTypes),
                        Result = ok1(defined_type(SymName, ArgTypes,
                            kind_star))
                    ;
                        SymNameArgsResult = error1(Specs),
                        Result = error1(Specs)
                    )
                ;
                    NameResult = error2(Specs),
                    Result = error1(Specs)
                )
            )
        )
    ).

:- pred parse_compound_type(term::in, varset::in, list(format_component)::in,
    known_compound_type_kind(term)::in, maybe1(mer_type)::out) is det.

parse_compound_type(Term, VarSet, ContextPieces, CompoundTypeKind, Result) :-
    (
        CompoundTypeKind = kctk_tuple(Args),
        parse_types(Args, VarSet, ContextPieces, ArgsResult),
        (
            ArgsResult = ok1(ArgTypes),
            Result = ok1(tuple_type(ArgTypes, kind_star))
        ;
            ArgsResult = error1(Specs),
            Result = error1(Specs)
        )
    ;
        CompoundTypeKind = kctk_apply(_),
        % We don't support apply/N types yet, so we just detect them
        % and report an error message.
        TermStr = describe_error_term(VarSet, Term),
        Pieces = ContextPieces ++ [lower_case_next_if_not_first,
            words("Error: ill-formed type"), words(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ;
        CompoundTypeKind = kctk_pure_pred(Args),
        ( if
            maybe_parse_types(Args, ArgTypes)
        then
            Result = ok1(higher_order_type(ArgTypes, no, purity_pure,
                lambda_normal))
        else
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        )
    ;
        CompoundTypeKind = kctk_pure_func(Arg1, Arg2),
        ( if
            Arg1 = term.functor(term.atom("func"), FuncArgs, _),
            maybe_parse_types(FuncArgs, ArgTypes),
            maybe_parse_type(Arg2, RetType)
        then
            Result = ok1(higher_order_type(ArgTypes,
                yes(RetType), purity_pure, lambda_normal))
        else
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        )
    ;
        CompoundTypeKind = kctk_purity(Purity, SubTerm),
        ( if
            SubTerm = term.functor(term.atom(Name), Args, _),
            (
                Name = "=",
                Args = [Arg1, Arg2],
                Arg1 = term.functor(term.atom("func"), FuncArgs, _),
                maybe_parse_types(FuncArgs, ArgTypes),
                maybe_parse_type(Arg2, RetType),
                ResultPrime = ok1(higher_order_type(ArgTypes,
                    yes(RetType), Purity, lambda_normal))
            ;
                Name = "pred",
                maybe_parse_types(Args, ArgTypes),
                ResultPrime = ok1(higher_order_type(ArgTypes, no, Purity,
                    lambda_normal))
            )
        then
            Result = ResultPrime
        else
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        )
    ).

is_known_type_name(Name) :-
    (
        is_known_type_name_args(Name, [] : list(mer_type), _)
    ;
        % The first disjunct succeeds for "pred", and the fact that
        % a higher order function type's top level functor is "=" and
        % not "func" is merely an implementation detail. Reserving "pred"
        % but not "func" as a type constructor name because of this detail
        % would be wrong, so we reserve "func" here as well.
        Name = "func"
    ).

:- type known_compound_type_kind(T)
    --->    kctk_tuple(list(T))
    ;       kctk_pure_func(T, T)
    ;       kctk_pure_pred(list(T))
    ;       kctk_purity(purity, T)
    ;       kctk_apply(list(T)).

:- type known_type_kind(T)
    --->    known_type_simple(mer_type)
    ;       known_type_compound(known_compound_type_kind(T))
    ;       known_type_bad_arity.

    % is_known_type_name_args(TypeName, TypeArgs, KnownTypeKind):
    %
    % If Name is a known type name and Name(TypeArgs) is a valid type
    % structure, then return its kind in KnownTypeKind.
    %
    % If Name is a known type name but Name(TypeArgs) is NOT a valid type
    % structure, then return known_type_bad_arity in KnownTypeKind.
    %
    % If Name is not a known type name, fail.
    %
:- pred is_known_type_name_args(string::in, list(T)::in,
    known_type_kind(T)::out) is semidet.

is_known_type_name_args(Name, Args, KnownType) :-
    (
        % Known types which are always simple.
        (
            Name = "int",
            BuiltinType = builtin_type_int
        ;
            Name = "float",
            BuiltinType = builtin_type_float
        ;
            Name = "character",
            BuiltinType = builtin_type_char
        ;
            % The type "char" is defined in library/char.m as equivalent
            % to the builtin type "character". However, if we recognize it
            % here, then imports of the char module won't be seen to be used.
%           Name = "char",
%           BuiltinType = builtin_type_char
%       ;
            Name = "string",
            BuiltinType = builtin_type_string
        ),
        (
            Args = [],
            KnownType = known_type_simple(builtin_type(BuiltinType))
        ;
            Args = [_ | _],
            KnownType = known_type_bad_arity
        )
    ;
        Name = "{}",
        KnownType = known_type_compound(kctk_tuple(Args))
    ;
        Name = "=",
        (
            ( Args = []
            ; Args = [_]
            ; Args = [_, _, _ | _]
            ),
            KnownType = known_type_bad_arity
        ;
            Args = [Arg1, Arg2],
            KnownType = known_type_compound(kctk_pure_func(Arg1, Arg2))
        )
    ;
        Name = "pred",
        KnownType = known_type_compound(kctk_pure_pred(Args))
    ;
        (
            Name = "pure",
            Purity = purity_pure
        ;
            Name = "semipure",
            Purity = purity_semipure
        ;
            Name = "impure",
            Purity = purity_impure
        ),
        (
            ( Args = []
            ; Args = [_, _ | _]
            ),
            KnownType = known_type_bad_arity
        ;
            Args = [Arg1],
            KnownType = known_type_compound(kctk_purity(Purity, Arg1))
        )
    ;
        Name = "",
        KnownType = known_type_compound(kctk_apply(Args))
    ).

:- func ill_formed_type_result(list(format_component), varset, term)
    = maybe1(mer_type).

ill_formed_type_result(ContextPieces, VarSet, Term) = Result :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = ContextPieces ++ [lower_case_next_if_not_first,
        words("Error: ill-formed type"), words(TermStr), suffix("."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]),
    Result = error1([Spec]).

maybe_parse_types(Term, Types) :-
    % The values of VarSet and ContextPieces do not matter since we succeed
    % only if they aren't used.
    VarSet = varset.init,
    ContextPieces = [],
    parse_types(Term, VarSet, ContextPieces, ok1(Types)).

parse_types(Terms, VarSet, ContextPieces, Result) :-
    parse_types_acc(Terms, VarSet, ContextPieces, [], RevTypes, [], Specs),
    (
        Specs = [],
        Result = ok1(list.reverse(RevTypes))
    ;
        Specs = [_ | _],
        Result = error1(Specs)
    ).

:- pred parse_types_acc(list(term)::in, varset::in, list(format_component)::in,
    list(mer_type)::in, list(mer_type)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_types_acc([], _, _, !RevTypes, !Specs).
parse_types_acc([Term | Terms], VarSet, ContextPieces, !RevTypes, !Specs) :-
    % XXX We should pass a ContextPieces updated as the "nth type in ...".
    parse_type(Term, VarSet, ContextPieces, TermResult),
    (
        TermResult = ok1(Type),
        !:RevTypes = [Type | !.RevTypes]
    ;
        TermResult = error1(TermSpecs),
        !:Specs = TermSpecs ++ !.Specs
    ),
    parse_types_acc(Terms, VarSet, ContextPieces, !RevTypes, !Specs).

parse_purity_annotation(Term0, Purity, Term) :-
    ( if
        Term0 = term.functor(term.atom(PurityName), [Term1], _),
        purity_name(Purity0, PurityName)
    then
        Purity = Purity0,
        Term = Term1
    else
        Purity = purity_pure,
        Term = Term0
    ).

unparse_type(Type, Term) :-
    Context = term.context_init,
    (
        Type = type_variable(TVar, _),
        Var = term.coerce_var(TVar),
        Term = term.variable(Var, Context)
    ;
        Type = defined_type(SymName, Args, _),
        unparse_type_list(Args, ArgTerms),
        unparse_qualified_term(SymName, ArgTerms, Term)
    ;
        Type = builtin_type(BuiltinType),
        builtin_type_to_string(BuiltinType, Name),
        Term = term.functor(term.atom(Name), [], Context)
    ;
        Type = higher_order_type(Args, MaybeRet, Purity, EvalMethod),
        unparse_type_list(Args, ArgTerms),
        (
            MaybeRet = yes(Ret),
            Term0 = term.functor(term.atom("func"), ArgTerms, Context),
            maybe_add_lambda_eval_method(EvalMethod, Term0, Term1),
            unparse_type(Ret, RetTerm),
            Term2 = term.functor(term.atom("="), [Term1, RetTerm], Context)
        ;
            MaybeRet = no,
            Term0 = term.functor(term.atom("pred"), ArgTerms, Context),
            maybe_add_lambda_eval_method(EvalMethod, Term0, Term2)
        ),
        maybe_add_purity_annotation(Purity, Term2, Term)
    ;
        Type = tuple_type(Args, _),
        unparse_type_list(Args, ArgTerms),
        Term = term.functor(term.atom("{}"), ArgTerms, Context)
    ;
        Type = apply_n_type(TVar, Args, _),
        Var = term.coerce_var(TVar),
        unparse_type_list(Args, ArgTerms),
        Term = term.functor(term.atom(""),
            [term.variable(Var, Context) | ArgTerms], Context)
    ;
        Type = kinded_type(_, _),
        unexpected($module, $pred, "kind annotation")
    ).

:- pred unparse_type_list(list(mer_type)::in, list(term)::out) is det.

unparse_type_list(Types, Terms) :-
    list.map(unparse_type, Types, Terms).

:- pred unparse_qualified_term(sym_name::in, list(term)::in, term::out) is det.

unparse_qualified_term(unqualified(Name), Args, Term) :-
    Context = term.context_init,
    Term = term.functor(term.atom(Name), Args, Context).
unparse_qualified_term(qualified(Qualifier, Name), Args, Term) :-
    Context = term.context_init,
    unparse_qualified_term(Qualifier, [], QualTerm),
    Term0 = term.functor(term.atom(Name), Args, Context),
    Term = term.functor(term.atom("."), [QualTerm, Term0], Context).

:- pred maybe_add_lambda_eval_method(lambda_eval_method::in, term::in,
    term::out) is det.

maybe_add_lambda_eval_method(lambda_normal, Term, Term).

:- pred maybe_add_purity_annotation(purity::in, term::in, term::out) is det.

maybe_add_purity_annotation(purity_pure, Term, Term).
maybe_add_purity_annotation(purity_semipure, Term0, Term) :-
    Context = term.context_init,
    Term = term.functor(term.atom("semipure"), [Term0], Context).
maybe_add_purity_annotation(purity_impure, Term0, Term) :-
    Context = term.context_init,
    Term = term.functor(term.atom("impure"), [Term0], Context).

convert_mode_list(_, [], []).
convert_mode_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_mode(AllowConstrainedInstVar, H0, H),
    convert_mode_list(AllowConstrainedInstVar, T0, T).

convert_mode(AllowConstrainedInstVar, Term, Mode) :-
    Term = term.functor(TermFunctor, TermArgs, _),
    % The is_known_mode_name predicate should succeed for exactly
    % the set of functor atoms recognized here.
    ( if
        TermFunctor = term.atom(">>")
    then
        TermArgs = [InstTermA, InstTermB],
        convert_inst(AllowConstrainedInstVar, InstTermA, InstA),
        convert_inst(AllowConstrainedInstVar, InstTermB, InstB),
        Mode = (InstA -> InstB)
    else if
        TermFunctor = term.atom("is")
    then
        TermArgs = [BeforeIsTerm, DetTerm],
        convert_higher_order_mode(AllowConstrainedInstVar,
            BeforeIsTerm, DetTerm, Mode)
    else
        % If the sym_name_and_args fails, we should report the error
        % (we would need to call parse_qualified_term instead).
        try_parse_sym_name_and_args_from_f_args(TermFunctor, TermArgs,
            Name, Args),
        convert_inst_list(AllowConstrainedInstVar, Args, ConvertedArgs),
        Mode = user_defined_mode(Name, ConvertedArgs)
    ).

is_known_mode_name(">>").
is_known_mode_name("is").

:- pred convert_higher_order_mode(allow_constrained_inst_var::in,
    term::in, term::in, mer_mode::out) is semidet.

convert_higher_order_mode(AllowConstrainedInstVar, BeforeIsTerm, DetTerm,
        Mode) :-
    BeforeIsTerm =
        term.functor(term.atom(BeforeIsFunctor), BeforeIsArgTerms, _),
    (
        ( BeforeIsFunctor = "pred"
        ; BeforeIsFunctor = "any_pred"
        ),
        % XXX We should improve switch detection to make this duplication
        % unnecessary.
        (
            BeforeIsFunctor = "pred",
            % Handle higher-order predicate modes:
            % a mode of the form
            %   pred(<Mode1>, <Mode2>, ...) is <Det>
            % is an abbreviation for the inst mapping
            %   (  pred(<Mode1>, <Mode2>, ...) is <Det>
            %   -> pred(<Mode1>, <Mode2>, ...) is <Det>
            %   )
            IsAny = no
        ;
            BeforeIsFunctor = "any_pred",
            % Handle higher-order predicate modes:
            % a mode of the form
            %   any_pred(<Mode1>, <Mode2>, ...) is <Det>
            % is an abbreviation for the inst mapping
            %   (  any_pred(<Mode1>, <Mode2>, ...) is <Det>
            %   -> any_pred(<Mode1>, <Mode2>, ...) is <Det>
            %   )
            IsAny = yes
        ),
        convert_mode_list(AllowConstrainedInstVar, BeforeIsArgTerms, ArgModes),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        PredInstInfo = pred_inst_info(pf_predicate, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(PredInstInfo))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(PredInstInfo))
        ),
        Mode = (Inst -> Inst)
    ;
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncTerm, RetModeTerm],
        FuncTerm = term.functor(term.atom(FuncTermFunctor), ArgModesTerms, _),
        (
            FuncTermFunctor = "func",
            % Handle higher-order function modes:
            % a mode of the form
            %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            % is an abbreviation for the inst mapping
            %   (  func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   -> func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   )
            IsAny = no
        ;
            FuncTermFunctor = "any_func",
            % Handle higher-order function modes:
            % a mode of the form
            %   any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            % is an abbreviation for the inst mapping
            %   (  any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   -> any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
            %   )
            IsAny = yes
        ),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerms, ArgModes0),
        convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
        list.append(ArgModes0, [RetMode], ArgModes),
        FuncInstInfo = pred_inst_info(pf_function, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(FuncInstInfo))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(FuncInstInfo))
        ),
        Mode = (Inst -> Inst)
    ).

convert_inst_list(_, [], []).
convert_inst_list(AllowConstrainedInstVar, [Term | Terms], [Inst | Insts]) :-
    convert_inst(AllowConstrainedInstVar, Term, Inst),
    convert_inst_list(AllowConstrainedInstVar, Terms, Insts).

convert_inst(_, Term, Inst) :-
    Term = term.variable(V0, _),
    term.coerce_var(V0, V),
    Inst = inst_var(V).
convert_inst(AllowConstrainedInstVar, Term, Inst) :-
    Term = term.functor(Functor, Args0, _Context),
    Functor = term.atom(Name),
    % XXX It would be nice if we could merge the switch on Name inside
    % is_simple_builtin_inst with the explicit switch below, *without*
    % duplicating the code of is_simple_builtin_inst.
    ( if is_known_inst_name_args(Name, Args0, KnownInstKind) then
        (
            KnownInstKind = known_inst_bad_arity,
            fail
        ;
            KnownInstKind = known_inst_simple(Inst)
        ;
            KnownInstKind = known_inst_compound(CompoundInstKind),
            (
                CompoundInstKind = kcik_is(BeforeIsTerm, DetTerm),
                convert_higher_order_inst(AllowConstrainedInstVar,
                    BeforeIsTerm, DetTerm, Inst)
            ;
                CompoundInstKind = kcik_bound(DisjTerm),
                parse_bound_inst_list(AllowConstrainedInstVar, DisjTerm,
                    shared, Inst)
            ;
                CompoundInstKind = kcik_unique(DisjTerm),
                parse_bound_inst_list(AllowConstrainedInstVar, DisjTerm,
                    unique, Inst)
            ;
                CompoundInstKind = kcik_mostly_unique(DisjTerm),
                parse_bound_inst_list(AllowConstrainedInstVar, DisjTerm,
                    mostly_unique, Inst)
            ;
                CompoundInstKind = kcik_constrained(VarTerm, SubInstTerm),
                AllowConstrainedInstVar = allow_constrained_inst_var,
                VarTerm = term.variable(Var, _),
                % Do not allow nested constrained_inst_vars.
                convert_inst(no_allow_constrained_inst_var, SubInstTerm,
                    SubInst),
                Inst = constrained_inst_vars(set.make_singleton_set(
                    term.coerce_var(Var)), SubInst)
            )
        )
    else
        % Anything else must be a user-defined inst.
        try_parse_sym_name_and_args_from_f_args(Functor, Args0,
            QualifiedName, Args1),
        ( if
            BuiltinModule = mercury_public_builtin_module,
            sym_name_get_module_name_default(QualifiedName, unqualified(""),
                BuiltinModule),
            % If the term is qualified with the `builtin' module,
            % then it may be one of the simple builtin insts.
            UnqualifiedName = unqualify_name(QualifiedName),
            is_known_inst_name_args(UnqualifiedName, Args1, KnownInstKind),
            KnownInstKind = known_inst_simple(InstPrime),

            % However, if the inst is a user_inst defined inside
            % the `builtin' module then we need to make sure it is
            % properly module-qualified.
            InstPrime \= defined_inst(user_inst(_, _))
        then
            Inst = InstPrime
        else
            convert_inst_list(AllowConstrainedInstVar, Args1, Args),
            Inst = defined_inst(user_inst(QualifiedName, Args))
        )
    ).

:- type known_compound_inst_kind(T)
    --->    kcik_is(T, T)
    ;       kcik_constrained(T, T)
    ;       kcik_bound(T)
    ;       kcik_unique(T)
    ;       kcik_mostly_unique(T).

:- type known_inst_kind(T)
    --->    known_inst_simple(mer_inst)
    ;       known_inst_compound(known_compound_inst_kind(T))
    ;       known_inst_bad_arity.

is_known_inst_name(Name) :-
    is_known_inst_name_args(Name, [] : list(mer_inst), _).

    % is_known_inst_name_args(InstName, InstArgs, KnownInstKind):
    %
    % If Name is a known inst name and Name(InstArgs) is a valid inst
    % structure, then return its kind in KnownInstKind.
    %
    % If Name is a known inst name but Name(InstArgs) is NOT a valid inst
    % structure, then return known_inst_bad_arity in KnownInstKind.
    %
    % If Name is not a known inst name, fail.
    %
:- pred is_known_inst_name_args(string::in, list(T)::in,
    known_inst_kind(T)::out) is semidet.

is_known_inst_name_args(Name, Args, KnownInst) :-
    (
        % Known insts which are always simple.
        (
            Name = "free",
            SimpleInst = free
        ;
            Name = "ground",
            SimpleInst = ground(shared, none_or_default_func)
        ;
            Name = "clobbered",
            SimpleInst = ground(clobbered, none_or_default_func)
        ;
            Name = "mostly_clobbered",
            SimpleInst = ground(mostly_clobbered, none_or_default_func)
        ;
            Name = "any",
            SimpleInst = any(shared, none_or_default_func)
        ;
            Name = "unique_any",
            SimpleInst = any(unique, none_or_default_func)
        ;
            Name = "mostly_unique_any",
            SimpleInst = any(mostly_unique, none_or_default_func)
        ;
            Name = "clobbered_any",
            SimpleInst = any(clobbered, none_or_default_func)
        ;
            Name = "mostly_clobbered_any",
            SimpleInst = any(mostly_clobbered, none_or_default_func)
        ;
            Name = "not_reached",
            SimpleInst = not_reached
        ),
        (
            Args = [],
            KnownInst = known_inst_simple(SimpleInst)
        ;
            Args = [_ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "unique",
        (
            Args = [],
            KnownInst = known_inst_simple(ground(unique, none_or_default_func))
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "mostly_unique",
        (
            Args = [],
            KnownInst = known_inst_simple(
                ground(mostly_unique, none_or_default_func))
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_mostly_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "is",
        (
            ( Args = []
            ; Args = [_]
            ),
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1, Arg2],
            KnownInst = known_inst_compound(kcik_is(Arg1, Arg2))
        ;
            Args = [_, _, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "=<",
        (
            ( Args = []
            ; Args = [_]
            ),
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1, Arg2],
            KnownInst = known_inst_compound(kcik_constrained(Arg1, Arg2))
        ;
            Args = [_, _, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "bound",
        (
            Args = [],
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_bound(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ;
        Name = "bound_unique",
        % `bound_unique' is for backwards compatibility - use `unique'
        % instead.
        (
            Args = [],
            KnownInst = known_inst_bad_arity
        ;
            Args = [Arg1],
            KnownInst = known_inst_compound(kcik_unique(Arg1))
        ;
            Args = [_, _ | _],
            KnownInst = known_inst_bad_arity
        )
    ).

:- pred convert_higher_order_inst(allow_constrained_inst_var::in,
    term::in, term::in, mer_inst::out) is semidet.

convert_higher_order_inst(AllowConstrainedInstVar, BeforeIsTerm, DetTerm,
        Inst) :-
    BeforeIsTerm =
        term.functor(term.atom(BeforeIsFunctor), BeforeIsArgTerms, _),
    (
        ( BeforeIsFunctor = "pred"
        ; BeforeIsFunctor = "any_pred"
        ),
        (
            BeforeIsFunctor = "pred",
            % The syntax for a ground higher-order pred inst is
            %
            %   pred(<Mode1>, <Mode2>, ...) is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % and <Detism> is a determinism.
            IsAny = no
        ;
            BeforeIsFunctor = "any_pred",
            % The syntax for an `any' higher-order pred inst is
            %
            %   any_pred(<Mode1>, <Mode2>, ...) is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % and <Detism> is a determinism.
            IsAny = yes
        ),

        convert_mode_list(AllowConstrainedInstVar, BeforeIsArgTerms, ArgModes),
        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        PredInst = pred_inst_info(pf_predicate, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(PredInst))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(PredInst))
        )
    ;
        BeforeIsFunctor = "=",
        BeforeIsArgTerms = [FuncTerm, RetModeTerm],
        FuncTerm = term.functor(term.atom(FuncTermFunctor), ArgModesTerms, _),
        (
            FuncTermFunctor = "func",
            % The syntax for a ground higher-order func inst is
            %
            %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % <RetMode> is a mode, and <Detism> is a determinism.
            IsAny = no
        ;
            FuncTermFunctor = "any_func",
            % The syntax for an `any' higher-order func inst is
            %
            %   any_func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
            %
            % where <Mode1>, <Mode2>, ... are a list of modes,
            % <RetMode> is a mode, and <Detism> is a determinism.
            IsAny = yes
        ),

        DetTerm = term.functor(term.atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerms, ArgModes0),
        convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
        list.append(ArgModes0, [RetMode], ArgModes),
        FuncInst = pred_inst_info(pf_function, ArgModes,
            arg_reg_types_unset, Detism),
        (
            IsAny = no,
            Inst = ground(shared, higher_order(FuncInst))
        ;
            IsAny = yes,
            Inst = any(shared, higher_order(FuncInst))
        )
    ).

standard_det("det",       detism_det).
standard_det("cc_nondet", detism_cc_non).
standard_det("cc_multi",  detism_cc_multi).
standard_det("nondet",    detism_non).
standard_det("multi",     detism_multi).
standard_det("multidet",  detism_multi).
standard_det("semidet",   detism_semi).
standard_det("erroneous", detism_erroneous).
standard_det("failure",   detism_failure).

:- pred parse_bound_inst_list(allow_constrained_inst_var::in, term::in,
    uniqueness::in, mer_inst::out) is semidet.

parse_bound_inst_list(AllowConstrainedInstVar, Disj, Uniqueness, Inst) :-
    disjunction_to_list(Disj, Disjuncts),
    convert_bound_inst_list(AllowConstrainedInstVar, Disjuncts, Functors0),
    list.sort(Functors0, Functors),
    % Check that the list doesn't specify the same functor twice.
    not (
        list.append(_, SubList, Functors),
        SubList = [F1, F2 | _],
        F1 = bound_functor(ConsId, _),
        F2 = bound_functor(ConsId, _)
    ),
    Inst = bound(Uniqueness, inst_test_no_results, Functors).

:- pred convert_bound_inst_list(allow_constrained_inst_var::in, list(term)::in,
    list(bound_inst)::out) is semidet.

convert_bound_inst_list(_, [], []).
convert_bound_inst_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_bound_inst(AllowConstrainedInstVar, H0, H),
    convert_bound_inst_list(AllowConstrainedInstVar, T0, T).

:- pred convert_bound_inst(allow_constrained_inst_var::in, term::in,
    bound_inst::out) is semidet.

convert_bound_inst(AllowConstrainedInstVar, InstTerm, BoundInst) :-
    InstTerm = term.functor(Functor, Args0, _),
    require_complete_switch [Functor]
    (
        Functor = term.atom(_),
        try_parse_sym_name_and_args_from_f_args(Functor, Args0,
            SymName, Args1),
        list.length(Args1, Arity),
        ConsId = cons(SymName, Arity, cons_id_dummy_type_ctor)
    ;
        Functor = term.implementation_defined(_),
        % Implementation-defined literals should not appear in inst
        % definitions.
        fail
    ;
        ( Functor = term.integer(_)
        ; Functor = term.big_integer(_, _)
        ; Functor = term.float(_)
        ; Functor = term.string(_)
        ),
        Args1 = Args0,
        list.length(Args1, Arity),
        make_functor_cons_id(Functor, Arity, ConsId)
    ),
    convert_inst_list(AllowConstrainedInstVar, Args1, Args),
    BoundInst = bound_functor(ConsId, Args).

disjunction_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more(";", Term, OneOrMore).

disjunction_to_list(Term, List) :-
    binop_term_to_one_or_more(";", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

conjunction_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more(",", Term, OneOrMore).

conjunction_to_list(Term, List) :-
    binop_term_to_one_or_more(",", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

one_or_more_to_conjunction(_, one_or_more(Term, []), Term).
one_or_more_to_conjunction(Context, one_or_more(First, [Second | Rest]),
        Term) :-
    one_or_more_to_conjunction(Context, one_or_more(Second, Rest), Tail),
    Term = term.functor(term.atom(","), [First, Tail], Context).

sum_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more("+", Term, OneOrMore).

sum_to_list(Term, List) :-
    binop_term_to_one_or_more("+", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

    % General predicate to convert terms separated by any specified operator
    % into a list.
    %
:- pred binop_term_to_one_or_more(string::in, term(T)::in,
    one_or_more(term(T))::out) is det.

binop_term_to_one_or_more(Op, Term, OneOrMore) :-
    binop_term_to_one_or_more_loop(Op, Term, [], OneOrMore).

:- pred binop_term_to_one_or_more_loop(string::in, term(T)::in,
    list(term(T))::in, one_or_more(term(T))::out) is det.

binop_term_to_one_or_more_loop(Op, Term, List0, OneOrMore) :-
    ( if Term = term.functor(term.atom(Op), [L, R], _Context) then
        binop_term_to_one_or_more_loop(Op, R, List0,
            one_or_more(RHead, RTail)),
        binop_term_to_one_or_more_loop(Op, L, [RHead | RTail], OneOrMore)
    else
        OneOrMore = one_or_more(Term, List0)
    ).

parse_one_or_more(Parser, Term, Result) :-
    conjunction_to_one_or_more(Term, one_or_more(Head, Tail)),
    map_parser_one_or_more(Parser, Head, Tail, Result).

parse_list(Parser, Term, Result) :-
    conjunction_to_list(Term, List),
    map_parser(Parser, List, Result).

:- pred map_parser_one_or_more(parser(T)::parser, term::in, list(term)::in,
    maybe1(one_or_more(T))::out) is det.

map_parser_one_or_more(Parser, Head, Tail, Result) :-
    call(Parser, Head, HeadResult),
    (
        Tail = [],
        (
            HeadResult = error1(Specs),
            Result = error1(Specs)
        ;
            HeadResult = ok1(Item),
            Result = ok1(one_or_more(Item, []))
        )
    ;
        Tail = [HeadTail | TailTail],
        map_parser_one_or_more(Parser, HeadTail, TailTail, TailResult),
        (
            HeadResult = error1(HeadSpecs),
            TailResult = error1(TailSpecs),
            Result = error1(HeadSpecs ++ TailSpecs)
        ;
            HeadResult = error1(Specs),
            TailResult = ok1(_),
            Result = error1(Specs)
        ;
            HeadResult = ok1(_),
            TailResult = error1(Specs),
            Result = error1(Specs)
        ;
            HeadResult = ok1(HeadItem),
            TailResult = ok1(TailItems),
            Result = ok1(one_or_more_cons(HeadItem, TailItems))
        )
    ).

map_parser(_, [], ok1([])).
map_parser(Parser, [Head | Tail], Result) :-
    call(Parser, Head, HeadResult),
    map_parser(Parser, Tail, TailResult),
    (
        HeadResult = error1(HeadSpecs),
        TailResult = error1(TailSpecs),
        Result = error1(HeadSpecs ++ TailSpecs)
    ;
        HeadResult = error1(Specs),
        TailResult = ok1(_),
        Result = error1(Specs)
    ;
        HeadResult = ok1(_),
        TailResult = error1(Specs),
        Result = error1(Specs)
    ;
        HeadResult = ok1(HeadItem),
        TailResult = ok1(TailItems),
        Result = ok1([HeadItem | TailItems])
    ).

%-----------------------------------------------------------------------------%

parse_list_of_vars(term.functor(term.atom("[]"), [], _), []).
parse_list_of_vars(term.functor(term.atom("[|]"), [Head, Tail], _),
        [Var | Vars]) :-
    Head = term.variable(Var, _),
    parse_list_of_vars(Tail, Vars).

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
                "variable", HeadTerm, HeadSpec),
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
            HeadSpecs = get_any_errors1(MaybeHeadVar),
            TailSpecs = get_any_errors1(MaybeTailVars),
            MaybeVars = error1(HeadSpecs ++ TailSpecs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "list of variables", Term, Spec),
        MaybeVars = error1([Spec])
    ).

:- type ordinary_state_var(T)
    --->    os_ordinary_var(var(T))
    ;       os_state_var(var(T)).

parse_quantifier_vars(Term, VarSet, ContextPieces, MaybeVars) :-
    ( if Term = functor(atom("[]"), [], _) then
        MaybeVars = ok2([], [])
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
                "variable or state variable", HeadTerm, HeadSpec),
            MaybeHeadVar = error1([HeadSpec])
        ),
        parse_quantifier_vars(TailTerm, VarSet, ContextPieces,
            MaybeTailVars),
        ( if
            MaybeHeadVar = ok1(VarKind),
            MaybeTailVars = ok2(TailVars, TailStateVars)
        then
            (
                VarKind = os_ordinary_var(V),
                ( if list.member(V, TailVars) then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error2([Spec])
                else
                    Vars = [V | TailVars],
                    MaybeVars = ok2(Vars, TailStateVars)
                )
            ;
                VarKind = os_state_var(SV),
                ( if list.member(SV, TailStateVars) then
                    generate_repeated_state_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error2([Spec])
                else
                    StateVars = [SV | TailStateVars],
                    MaybeVars = ok2(TailVars, StateVars)
                )
            )
        else
            HeadSpecs = get_any_errors1(MaybeHeadVar),
            TailSpecs = get_any_errors2(MaybeTailVars),
            MaybeVars = error2(HeadSpecs ++ TailSpecs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "list of variables and/or state variables", Term, Spec),
        MaybeVars = error2([Spec])
    ).

:- type ordinary_state_dot_colon_var(T)
    --->    osdc_ordinary_var(var(T))
    ;       osdc_state_var(var(T))
    ;       osdc_dot_var(var(T))
    ;       osdc_colon_var(var(T)).

parse_vars_and_state_vars(Term, VarSet, ContextPieces, MaybeVars) :-
    ( if Term = functor(atom("[]"), [], _) then
        MaybeVars = ok4([], [], [], [])
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
                "variable or state variable", HeadTerm, HeadSpec),
            MaybeHeadVar = error1([HeadSpec])
        ),
        parse_vars_and_state_vars(Tail, VarSet, ContextPieces,
            MaybeTailVars),
        ( if
            MaybeHeadVar = ok1(VarKind),
            MaybeTailVars = ok4(TailVars, TailStateVars,
                TailDotVars, TailColonVars)
        then
            (
                VarKind = osdc_ordinary_var(V),
                ( if list.member(V, TailVars) then
                    generate_repeated_var_msg(ContextPieces, VarSet,
                        HeadTerm, Spec),
                    MaybeVars = error4([Spec])
                else
                    Vars = [V | TailVars],
                    MaybeVars = ok4(Vars, TailStateVars,
                        TailDotVars, TailColonVars)
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
                    MaybeVars = error4([Spec])
                else
                    StateVars = [SV | TailStateVars],
                    MaybeVars = ok4(TailVars, StateVars,
                        TailDotVars, TailColonVars)
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
                    MaybeVars = error4([Spec])
                else
                    DotVars = [SV | TailDotVars],
                    MaybeVars = ok4(TailVars, TailStateVars,
                        DotVars, TailColonVars)
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
                    MaybeVars = error4([Spec])
                else
                    ColonVars = [SV | TailColonVars],
                    MaybeVars = ok4(TailVars, TailStateVars,
                        TailDotVars, ColonVars)
                )
            )
        else
            HeadSpecs = get_any_errors1(MaybeHeadVar),
            TailSpecs = get_any_errors4(MaybeTailVars),
            MaybeVars = error4(HeadSpecs ++ TailSpecs)
        )
    else
        generate_unexpected_term_message(ContextPieces, VarSet,
            "list of variables and/or state variables", Term, Spec),
        MaybeVars = error4([Spec])
    ).

:- pred generate_repeated_var_msg(list(format_component)::in,
    varset(T)::in, term(T)::in, error_spec::out) is det.

generate_repeated_var_msg(ContextPieces, VarSet, Term, Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = ContextPieces ++ [lower_case_next_if_not_first,
        words("Repeated variable"), words(TermStr), suffix("."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]).

:- pred generate_repeated_state_var_msg(list(format_component)::in,
    varset(T)::in, term(T)::in, error_spec::out) is det.

generate_repeated_state_var_msg(ContextPieces, VarSet, Term, Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = ContextPieces ++ [lower_case_next_if_not_first,
        words("Repeated state variable"), words(TermStr), suffix("."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]).

:- pred generate_unexpected_term_message(list(format_component)::in,
    varset(T)::in, string::in, term(T)::in, error_spec::out) is det.

generate_unexpected_term_message(ContextPieces, VarSet, Expected, Term,
        Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = ContextPieces ++ [lower_case_next_if_not_first,
        words("Expected"), words(Expected), suffix(","),
        words("not"), words(TermStr), suffix("."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]).

%-----------------------------------------------------------------------------%

list_term_to_term_list(Term, Terms) :-
    (
        Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _),
        list_term_to_term_list(TailTerm, TailTerms),
        Terms = [HeadTerm | TailTerms]
    ;
        Term = term.functor(term.atom("[]"), [], _),
        Terms = []
    ).

%-----------------------------------------------------------------------------%

parse_decl_attribute(Functor, ArgTerms, Attribute, SubTerm) :-
    (
        Functor = "impure",
        ArgTerms = [SubTerm],
        Attribute = decl_attr_purity(purity_impure)
    ;
        Functor = "semipure",
        ArgTerms = [SubTerm],
        Attribute = decl_attr_purity(purity_semipure)
    ;
        Functor = "<=",
        ArgTerms = [SubTerm, ConstraintsTerm],
        Attribute = decl_attr_constraints(quant_type_univ, ConstraintsTerm)
    ;
        Functor = "=>",
        ArgTerms = [SubTerm, ConstraintsTerm],
        Attribute = decl_attr_constraints(quant_type_exist, ConstraintsTerm)
    ;
        Functor = "some",
        ArgTerms = [TVarsTerm, SubTerm],
        parse_list_of_vars(TVarsTerm, TVars),
        Attribute = decl_attr_quantifier(quant_type_exist, TVars)
    ;
        Functor = "all",
        ArgTerms = [TVarsTerm, SubTerm],
        parse_list_of_vars(TVarsTerm, TVars),
        Attribute = decl_attr_quantifier(quant_type_univ, TVars)
    ;
        Functor = "solver",
        ArgTerms = [SubTerm],
        Attribute = decl_attr_solver_type
    ).

check_no_attributes(Result0, Attributes, Result) :-
    ( if
        Result0 = ok1(_),
        Attributes = [Attr - Context | _]
    then
        % XXX Shouldn't we mention EVERY element of Attributes?
        Pieces = [words("Error:"), words(attribute_description(Attr)),
            words("not allowed here."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(Context, [always(Pieces)])]),
        Result = error1([Spec])
    else
        Result = Result0
    ).

attribute_description(decl_attr_purity(_)) = "purity specifier".
attribute_description(decl_attr_quantifier(quant_type_univ, _)) =
    "universal quantifier (`all')".
attribute_description(decl_attr_quantifier(quant_type_exist, _)) =
    "existential quantifier (`some')".
attribute_description(decl_attr_constraints(quant_type_univ, _)) =
    "type class constraint (`<=')".
attribute_description(decl_attr_constraints(quant_type_exist, _)) =
    "existentially quantified type class constraint (`=>')".
attribute_description(decl_attr_solver_type) = "solver type specifier".

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_util.
%-----------------------------------------------------------------------------%
