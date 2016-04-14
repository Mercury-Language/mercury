%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines predicates that parse and unparse type names.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_type_name.
:- interface.

:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type allow_ho_inst_info
    --->    allow_ho_inst_info
    ;       no_allow_ho_inst_info.

:- pred maybe_parse_type(allow_ho_inst_info::in, term::in, mer_type::out)
    is semidet.

:- pred parse_type(allow_ho_inst_info::in, varset::in,
    cord(format_component)::in, term::in, maybe1(mer_type)::out) is det.

:- pred maybe_parse_types(allow_ho_inst_info::in, list(term)::in,
    list(mer_type)::out) is semidet.

:- pred parse_types(allow_ho_inst_info::in, varset::in,
    cord(format_component)::in, list(term)::in,
    maybe1(list(mer_type))::out) is det.

:- pred is_known_type_name(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_type.

:- import_module bool.

%---------------------------------------------------------------------------%

maybe_parse_type(AllowHOInstInfo, Term, Type) :-
    % The values of VarSet and ContextPieces do not matter since we succeed
    % only if they aren't used.
    VarSet = varset.init,
    ContextPieces = cord.init,
    parse_type(AllowHOInstInfo, VarSet, ContextPieces, Term, ok1(Type)).

parse_type(AllowHOInstInfo, VarSet, ContextPieces, Term, Result) :-
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
                    parse_compound_type(AllowHOInstInfo, Term, VarSet,
                        ContextPieces, CompoundTypeKind, Result)
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
                    parse_types(no_allow_ho_inst_info, VarSet, ContextPieces,
                        SymNameArgTerms, SymNameArgsResult),
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

:- pred parse_compound_type(allow_ho_inst_info::in, term::in, varset::in,
    cord(format_component)::in, known_compound_type_kind(term)::in,
    maybe1(mer_type)::out) is det.

parse_compound_type(AllowHOInstInfo, Term, VarSet, ContextPieces,
        CompoundTypeKind, Result) :-
    (
        CompoundTypeKind = kctk_tuple(Args),
        parse_types(no_allow_ho_inst_info, VarSet, ContextPieces,
            Args, ArgsResult),
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
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: ill-formed type"), quote(TermStr), suffix("."), nl],
        Spec = error_spec(severity_error, phase_term_to_parse_tree,
            [simple_msg(get_term_context(Term), [always(Pieces)])]),
        Result = error1([Spec])
    ;
        CompoundTypeKind = kctk_pure_pred(Args),
        ( if
            maybe_parse_types(no_allow_ho_inst_info, Args, ArgTypes)
        then
            construct_higher_order_pred_type(purity_pure, lambda_normal,
                ArgTypes, PredType),
            Result = ok1(PredType)
        else
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        )
    ;
        CompoundTypeKind = kctk_pure_func(Arg1, Arg2),
        ( if
            Arg1 = term.functor(term.atom("func"), FuncArgs, _),
            maybe_parse_types(no_allow_ho_inst_info, FuncArgs, ArgTypes),
            maybe_parse_type(no_allow_ho_inst_info, Arg2, RetType)
        then
            construct_higher_order_func_type(purity_pure, lambda_normal,
                ArgTypes, RetType, FuncType),
            Result = ok1(FuncType)
        else
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        )
    ;
        CompoundTypeKind = kctk_is(Arg1, Arg2),
        ( if
            AllowHOInstInfo = allow_ho_inst_info,
            maybe_parse_ho_type_and_inst(Arg1, Arg2, purity_pure, Type)
        then
            Result = ok1(Type)
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
                maybe_parse_types(no_allow_ho_inst_info, FuncArgs, ArgTypes),
                maybe_parse_type(no_allow_ho_inst_info, Arg2, RetType),
                construct_higher_order_func_type(Purity, lambda_normal,
                    ArgTypes, RetType, Type)
            ;
                Name = "pred",
                maybe_parse_types(no_allow_ho_inst_info, Args, ArgTypes),
                construct_higher_order_pred_type(Purity, lambda_normal,
                    ArgTypes, Type)
            ;
                Name = "is",
                AllowHOInstInfo = allow_ho_inst_info,
                Args = [Arg1, Arg2],
                maybe_parse_ho_type_and_inst(Arg1, Arg2, Purity, Type)
            )
        then
            Result = ok1(Type)
        else
            Result = ill_formed_type_result(ContextPieces, VarSet, Term)
        )
    ).

:- pred maybe_parse_ho_type_and_inst(term::in, term::in, purity::in,
    mer_type::out) is semidet.

maybe_parse_ho_type_and_inst(Arg1, Arg2, Purity, Type) :-
    Arg2 = term.functor(term.atom(DetString), [], _),
    standard_det(DetString, Detism),
    (
        Arg1 = term.functor(term.atom("="), [FuncTerm, RetTerm], _),
        FuncTerm = term.functor(term.atom("func"), ArgTerms, _),
        maybe_parse_types_and_modes(ArgTerms, ArgTypes, ArgModes),
        maybe_parse_type_and_mode(RetTerm, RetType, RetMode),
        construct_higher_order_func_type(Purity, lambda_normal, ArgTypes,
            RetType, ArgModes, RetMode, Detism, Type)
    ;
        Arg1 = term.functor(term.atom("pred"), ArgTerms, _),
        maybe_parse_types_and_modes(ArgTerms, ArgTypes, ArgModes),
        construct_higher_order_pred_type(Purity, lambda_normal, ArgTypes,
            ArgModes, Detism, Type)
    ).

:- pred maybe_parse_types_and_modes(list(term)::in, list(mer_type)::out,
    list(mer_mode)::out) is semidet.

maybe_parse_types_and_modes(ArgTerms, ArgTypes, ArgModes) :-
    list.reverse(ArgTerms, RevArgTerms),
    maybe_parse_types_and_modes_acc(RevArgTerms, [], ArgTypes, [], ArgModes).

:- pred maybe_parse_types_and_modes_acc(list(term)::in,
    list(mer_type)::in, list(mer_type)::out,
    list(mer_mode)::in, list(mer_mode)::out) is semidet.

maybe_parse_types_and_modes_acc([], !ArgTypes, !ArgModes).
maybe_parse_types_and_modes_acc([ArgTerm | ArgTerms], ArgTypes0, ArgTypes,
        ArgModes0, ArgModes) :-
    maybe_parse_type_and_mode(ArgTerm, ArgType, ArgMode),
    maybe_parse_types_and_modes_acc(ArgTerms, [ArgType | ArgTypes0], ArgTypes,
        [ArgMode | ArgModes0], ArgModes).

:- pred maybe_parse_type_and_mode(term::in, mer_type::out, mer_mode::out)
    is semidet.

maybe_parse_type_and_mode(Term, Type, Mode) :-
    Term = term.functor(term.atom("::"), [TypeTerm, ModeTerm], _),
    maybe_parse_type(no_allow_ho_inst_info, TypeTerm, Type),
    convert_mode(no_allow_constrained_inst_var, ModeTerm, Mode).

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
    ;       kctk_is(T, T)
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
        Name = "is",
        (
            ( Args = []
            ; Args = [_]
            ; Args = [_, _, _ | _]
            ),
            KnownType = known_type_bad_arity
        ;
            Args = [Arg1, Arg2],
            KnownType = known_type_compound(kctk_is(Arg1, Arg2))
        )
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

:- func ill_formed_type_result(cord(format_component), varset, term)
    = maybe1(mer_type).

ill_formed_type_result(ContextPieces, VarSet, Term) = Result :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
        words("Error: ill-formed type"), quote(TermStr), suffix("."), nl],
    Spec = error_spec(severity_error, phase_term_to_parse_tree,
        [simple_msg(get_term_context(Term), [always(Pieces)])]),
    Result = error1([Spec]).

%---------------------------------------------------------------------------%

maybe_parse_types(AllowHOInstInfo, Term, Types) :-
    % The values of VarSet and ContextPieces do not matter since we succeed
    % only if they aren't used.
    VarSet = varset.init,
    ContextPieces = cord.init,
    parse_types(AllowHOInstInfo, VarSet, ContextPieces, Term, ok1(Types)).

parse_types(AllowHOInstInfo, VarSet, ContextPieces, Terms, Result) :-
    parse_types_acc(AllowHOInstInfo, VarSet, ContextPieces, Terms,
        [], RevTypes, [], Specs),
    (
        Specs = [],
        Result = ok1(list.reverse(RevTypes))
    ;
        Specs = [_ | _],
        Result = error1(Specs)
    ).

:- pred parse_types_acc(allow_ho_inst_info::in, varset::in,
    cord(format_component)::in, list(term)::in,
    list(mer_type)::in, list(mer_type)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_types_acc(_, _, _, [], !RevTypes, !Specs).
parse_types_acc(AllowHOInstInfo, VarSet, ContextPieces, [Term | Terms],
        !RevTypes, !Specs) :-
    % XXX We should pass a ContextPieces updated as the "nth type in ...".
    parse_type(AllowHOInstInfo, VarSet, ContextPieces, Term, TermResult),
    (
        TermResult = ok1(Type),
        !:RevTypes = [Type | !.RevTypes]
    ;
        TermResult = error1(TermSpecs),
        !:Specs = TermSpecs ++ !.Specs
    ),
    parse_types_acc(AllowHOInstInfo, VarSet, ContextPieces, Terms,
        !RevTypes, !Specs).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_type_name.
%---------------------------------------------------------------------------%
