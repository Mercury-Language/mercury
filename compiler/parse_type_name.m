%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016-2024 The Mercury team.
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

:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- type allow_ho_inst_info
    --->    allow_ho_inst_info
    ;       no_allow_ho_inst_info(why_no_ho_inst_info).

    % The contexts in which you are not allowed to specify
    % higher order inst information. Some of these contexts *should*
    % allow you to specify such information, but the support needed
    % has not yet been implemented (NYI).
:- type why_no_ho_inst_info
    --->    wnhii_type_ctor_arg         % NYI
    ;       wnhii_tuple_arg
    ;       wnhii_pred_arg
    ;       wnhii_func_arg
    ;       wnhii_func_return_arg
    ;       wnhii_type_qual             % NYI
    ;       wnhii_supertype
    ;       wnhii_solver_type_defn
    ;       wnhii_class_constraint
    ;       wnhii_user_struct_sharing
    ;       wnhii_ctgc_type_selector
    ;       wnhii_pragma_struct_sharing
    ;       wnhii_pragma_struct_reuse
    ;       wnhii_pragma_type_spec_constr
    ;       wnhii_pragma_type_spec.

:- pred maybe_parse_type(allow_ho_inst_info::in, term::in, mer_type::out)
    is semidet.

:- pred parse_type(allow_ho_inst_info::in, varset::in,
    cord(format_piece)::in, term::in, maybe1(mer_type)::out) is det.

:- pred maybe_parse_types(allow_ho_inst_info::in, list(term)::in,
    list(mer_type)::out) is semidet.

:- pred parse_types(allow_ho_inst_info::in, varset::in,
    cord(format_piece)::in, list(term)::in,
    maybe1(list(mer_type))::out) is det.

%---------------------%

:- type maybe_constrain_inst_vars
    --->    do_not_constrain_inst_vars
    ;       constrain_some_inst_vars(inst_var_sub).

:- type maybe_require_tm_mode
    --->    do_not_require_tm_mode
    ;       require_tm_mode.

:- type arg_context_func == (func(int) = cord(format_piece)).

:- type type_and_maybe_mode
    --->    type_only(mer_type)
    ;       type_and_mode(mer_type, mer_mode).

:- pred parse_types_and_maybe_modes(maybe_constrain_inst_vars::in,
    maybe_require_tm_mode::in, why_no_ho_inst_info::in,
    varset::in, arg_context_func::in,
    list(term)::in, int::in, list(type_and_maybe_mode)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred parse_type_and_maybe_mode(maybe_constrain_inst_vars::in,
    maybe_require_tm_mode::in, why_no_ho_inst_info::in,
    varset::in, cord(format_piece)::in,
    term::in, maybe1(type_and_maybe_mode)::out) is det.

%---------------------%

:- pred is_known_type_name(string::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type_construct.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- func arg_context_pieces(cord(format_piece), pred_or_func, int) =
    cord(format_piece).

arg_context_pieces(ContextPieces, PorF, ArgNum) =
    ContextPieces ++ cord.from_list([lower_case_next_if_not_first,
        words("In the"), nth_fixed(ArgNum),
        words("argument of higher-order"), p_or_f(PorF), words("type:"), nl]).

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
        Term = term.functor(Functor, ArgTerms, FunctorContext),
        (
            ( Functor = term.integer(_, _, _, _)
            ; Functor = term.float(_)
            ; Functor = term.string(_)
            ; Functor = term.implementation_defined(_)
            ),
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error:")] ++
                color_as_subject([quote(TermStr)]) ++
                [words("is")] ++
                color_as_incorrect([words("not a type.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                FunctorContext, Pieces),
            Result = error1([Spec])
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
                    KnownTypeKind = known_type_bad_arity(ExpectedArity),
                    Pieces = cord.list(ContextPieces) ++
                        [lower_case_next_if_not_first, words("Error:")] ++
                        color_as_subject([quote(Name)]) ++
                        [words("should not be used")] ++
                        color_as_incorrect([words("with any arity other than"),
                            int_fixed(ExpectedArity), suffix(".")]) ++
                        [nl],
                    Spec = spec($pred, severity_error, phase_t2pt,
                        FunctorContext, Pieces),
                    Result = error1([Spec])
                )
            else
                % We don't support kind annotations yet, and we don't report
                % an error either. Perhaps we should?
                parse_sym_name_and_args(VarSet, ContextPieces, Term,
                    NameResult),
                (
                    NameResult = ok2(SymName, SymNameArgTerms),
                    parse_types(
                        no_allow_ho_inst_info(wnhii_type_ctor_arg),
                        VarSet, ContextPieces,
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
    cord(format_piece)::in, known_compound_type_kind(term)::in,
    maybe1(mer_type)::out) is det.

parse_compound_type(AllowHOInstInfo, Term, VarSet, ContextPieces,
        CompoundTypeKind, Result) :-
    (
        CompoundTypeKind = kctk_tuple(Args),
        parse_types(no_allow_ho_inst_info(wnhii_tuple_arg),
            VarSet, ContextPieces, Args, ArgsResult),
        (
            ArgsResult = ok1(ArgTypes),
            Result = ok1(tuple_type(ArgTypes, kind_star))
        ;
            ArgsResult = error1(Specs),
            Result = error1(Specs)
        )
    ;
        CompoundTypeKind = kctk_empty_name(ArgTypes),
        % The are types whose top level function symbol is a variable,
        % which the parser records as a term whose function symbol
        % is the empty string, with the formerly-top-level variable
        % put in front of the actual argument list (if any).
        ( if ArgTypes = [term.variable(FunctorVarName, _) | _] then
            FunctorPiece = var_to_quote_piece(VarSet, FunctorVarName),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: expected a")] ++
                color_as_correct([words("symbol name")]) ++
                [words("as type constructor, got")] ++
                color_as_incorrect([FunctorPiece, suffix(".")]) ++
                [nl]
        else
            % The original comment here is:
            %   We don't support apply/N types yet, so we just detect them
            %   and report an error message.
            % I (zs) don't know exactly what syntax Fergus intended for
            % apply/N types, but the test above should catch most of them,
            % and explain the error in a more understandable way.
            TermStr = describe_error_term(VarSet, Term),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error:")] ++
                color_as_incorrect([words("ill-formed type")]) ++
                color_as_subject([quote(TermStr), suffix(".")]) ++
                [nl]
        ),
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        Result = error1([Spec])
    ;
        CompoundTypeKind = kctk_pure_pred(Args),
        ArgContextFunc = arg_context_pieces(ContextPieces, pf_predicate),
        parse_types_no_modes(wnhii_pred_arg, VarSet, ArgContextFunc,
            Args, 1, ArgTypes, [], Specs),
        (
            Specs = [],
            construct_higher_order_pred_type(purity_pure, ArgTypes, PredType),
            Result = ok1(PredType)
        ;
            Specs = [_ | _],
            Result = error1(Specs)
        )
    ;
        CompoundTypeKind = kctk_pure_func(BeforeEqTerm, AfterEqTerm),
        ( if BeforeEqTerm = term.functor(term.atom("func"), FuncArgs, _) then
            ArgContextFunc = arg_context_pieces(ContextPieces, pf_function),
            parse_types_no_modes(wnhii_func_arg, VarSet, ArgContextFunc,
                FuncArgs, 1, ArgTypes, [], ArgSpecs),
            RetContextPieces = ContextPieces ++ cord.from_list([
                words("in the return value of higher-order function type:"),
                nl]),
            parse_type_no_mode(wnhii_func_return_arg, VarSet,
                RetContextPieces, AfterEqTerm, MaybeRetType),
            ( if
                ArgSpecs = [],
                MaybeRetType = ok1(RetType)
            then
                construct_higher_order_func_type(purity_pure,
                    ArgTypes, RetType, FuncType),
                Result = ok1(FuncType)
            else
                Specs = ArgSpecs ++ get_any_errors1(MaybeRetType),
                Result = error1(Specs)
            )
        else
            % XXX Should be more specific.
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: in a function type declaration,"),
                words("the operator")] ++
                color_as_subject([quote("=")]) ++
                color_as_incorrect([words("should be preceded")]) ++
                [words("by")] ++
                color_as_subject([quote("func(<arguments>)"), suffix(".")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(BeforeEqTerm), Pieces),
            Result = error1([Spec])
        )
    ;
        CompoundTypeKind = kctk_is(BeforeIsTerm, AfterIsTerm),
        parse_ho_type_and_inst(VarSet, ContextPieces,
            BeforeIsTerm, AfterIsTerm, purity_pure, MaybeType),
        (
            MaybeType = error1(Specs),
            Result = error1(Specs)
        ;
            MaybeType = ok1(Type),
            (
                AllowHOInstInfo = allow_ho_inst_info,
                Result = ok1(Type)
            ;
                AllowHOInstInfo = no_allow_ho_inst_info(Why),
                Result = no_ho_inst_allowed_result(ContextPieces, Why,
                    VarSet, Term)
            )
        )
    ;
        CompoundTypeKind = kctk_purity(PurityName, Purity, SubTerm),
        ( if
            SubTerm = term.functor(term.atom(Name), Args, _),
            Name = "=",
            Args = [BeforeEqTerm, AfterEqTerm],
            BeforeEqTerm = term.functor(term.atom("func"), FuncArgs, _)
        then
            ArgContextFunc = arg_context_pieces(ContextPieces, pf_function),
            parse_types_no_modes(wnhii_func_arg, VarSet, ArgContextFunc,
                FuncArgs, 1, ArgTypes, [], ArgSpecs),
            RetContextPieces = ContextPieces ++ cord.from_list([
                words("in the return value of higher-order function type:"),
                nl]),
            parse_type_no_mode(wnhii_func_return_arg, VarSet,
                RetContextPieces, AfterEqTerm, MaybeRetType),
            ( if
                ArgSpecs = [],
                MaybeRetType = ok1(RetType)
            then
                construct_higher_order_func_type(Purity, ArgTypes, RetType,
                    Type),
                Result = ok1(Type)
            else
                Specs = ArgSpecs ++ get_any_errors1(MaybeRetType),
                Result = error1(Specs)
            )
        else if
            SubTerm = term.functor(term.atom(Name), Args, _),
            Name = "pred"
        then
            ArgContextFunc = arg_context_pieces(ContextPieces, pf_predicate),
            parse_types_no_modes(wnhii_pred_arg, VarSet, ArgContextFunc,
                Args, 1, ArgTypes, [], Specs),
            (
                Specs = [],
                construct_higher_order_pred_type(Purity, ArgTypes, Type),
                Result = ok1(Type)
            ;
                Specs = [_ | _],
                Result = error1(Specs)
            )
        else if
            SubTerm = term.functor(term.atom(Name), Args, _),
            Name = "is",
            Args = [BeforeIsTerm, AfterIsTerm]
        then
            parse_ho_type_and_inst(VarSet, ContextPieces,
                BeforeIsTerm, AfterIsTerm, Purity, MaybeType),
            (
                MaybeType = error1(Specs),
                Result = error1(Specs)
            ;
                MaybeType = ok1(Type),
                (
                    AllowHOInstInfo = allow_ho_inst_info,
                    Result = ok1(Type)
                ;
                    AllowHOInstInfo = no_allow_ho_inst_info(Why),
                    Result = no_ho_inst_allowed_result(ContextPieces, Why,
                        VarSet, Term)
                )
            )
        else
            AlwaysPieces =
                [quote("pred(<arg_types>)"), nl,
                quote("func(<arg_types>) = <ret_type>")],
            AllowPieces =
                [quote("pred(<arg_types_modes>) is <detism>"), nl,
                quote("func(<arg_types_modes>) = " ++
                    "<ret_type_mode> is <detism>")],
            (
                AllowHOInstInfo = allow_ho_inst_info,
                FormPieces = AlwaysPieces ++ [nl | AllowPieces]
            ;
                AllowHOInstInfo = no_allow_ho_inst_info(_Why),
                FormPieces = AlwaysPieces
            ),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: the purity marker"), quote(PurityName)] ++
                color_as_incorrect([words("should be followed")]) ++
                [words("by a higher order type,"),
                words("which should be of one of the following forms:"),
                nl_indent_delta(1)] ++
                color_as_correct(FormPieces ++ [suffix(".")]) ++
                [nl_indent_delta(-1)],
            Spec = spec($pred, severity_error, phase_t2pt,
                get_term_context(SubTerm), Pieces),
            Result = error1([Spec])
        )
    ).

:- pred parse_ho_type_and_inst(varset::in, cord(format_piece)::in,
    term::in, term::in, purity::in, maybe1(mer_type)::out) is det.

parse_ho_type_and_inst(VarSet, ContextPieces, BeforeIsTerm, AfterIsTerm,
        Purity, MaybeType) :-
    parse_determinism(VarSet, AfterIsTerm, MaybeDetism),
    ( if
        BeforeIsTerm = term.functor(term.atom("="), [FuncTerm, RetTerm], _),
        FuncTerm = term.functor(term.atom("func"), ArgTerms, _)
    then
        % XXX We pass require_tm_mode to the predicates that parse
        % the arguments, but should we *require* this for functions?
        ArgContextFunc = arg_context_pieces(ContextPieces, pf_function),
        parse_types_and_maybe_modes(do_not_constrain_inst_vars,
            require_tm_mode, wnhii_func_arg, VarSet, ArgContextFunc,
            ArgTerms, 1, ArgTypeAndMaybeModes, [], ArgTMSpecs),
        RetContextPieces = ContextPieces ++ cord.from_list([
            words("in the return value of higher-order function type:"),
            nl]),
        parse_type_and_maybe_mode(do_not_constrain_inst_vars, require_tm_mode,
            wnhii_func_return_arg, VarSet, RetContextPieces,
            RetTerm, MaybeRetTypeAndMaybeMode),
        parse_ho_type_and_inst_2(VarSet, ContextPieces, Purity,
            ArgTypeAndMaybeModes, ArgTMSpecs, yes(MaybeRetTypeAndMaybeMode),
            MaybeDetism, MaybeType)
    else if
        BeforeIsTerm = term.functor(term.atom("pred"), ArgTerms, _)
    then
        ArgContextFunc = arg_context_pieces(ContextPieces, pf_predicate),
        parse_types_and_maybe_modes(do_not_constrain_inst_vars,
            require_tm_mode, wnhii_pred_arg, VarSet, ArgContextFunc,
            ArgTerms, 1, ArgTypeAndMaybeModes, [], ArgTMSpecs),
        parse_ho_type_and_inst_2(VarSet, ContextPieces, Purity,
            ArgTypeAndMaybeModes, ArgTMSpecs, no, MaybeDetism, MaybeType)
    else
        Form1 = "pred(<arguments>) is det",
        Form2 = "func(<arguments>) = <return_argument> is det",
        BeforeIsTermStr = describe_error_term(VarSet, BeforeIsTerm),
        HOPieces = [words("Error: expected either"),
            nl_indent_delta(1)] ++
            color_as_correct([quote(Form1)]) ++ [words("or"), nl] ++
            color_as_correct([quote(Form2)]) ++
            [nl_indent_delta(-1),
            words("as a higher order type, got")] ++
            color_as_incorrect([quote(BeforeIsTermStr), suffix(".")]) ++
            [nl],
        HOSpec = spec($pred, severity_error, phase_t2pt,
            get_term_context(AfterIsTerm), HOPieces),
        MaybeType = error1([HOSpec])
    ).

:- pred parse_ho_type_and_inst_2(varset::in, cord(format_piece)::in,
    purity::in, list(type_and_maybe_mode)::in, list(error_spec)::in,
    maybe(maybe1(type_and_maybe_mode))::in, maybe1(determinism)::in,
    maybe1(mer_type)::out) is det.

parse_ho_type_and_inst_2(_VarSet, _ContextPieces, Purity, ArgTypeAndModes,
        ArgSpecs, MaybeMaybeRetTypeAndMode, MaybeDetism, MaybeType) :-
    % XXX _VarSet
    list.map2(project_tm_type_and_mode, ArgTypeAndModes, ArgTypes0, ArgModes0),
    ( if
        ArgSpecs = [],
        (
            MaybeMaybeRetTypeAndMode = no,
            PredOrFunc = pf_predicate,
            ArgTypes = ArgTypes0,
            ArgModes = ArgModes0,
            PredInstInfo = pred_inst_info(PredOrFunc, ArgModes,
                arg_reg_types_unset, Detism)
        ;
            MaybeMaybeRetTypeAndMode = yes(MaybeRetTypeAndMode),
            MaybeRetTypeAndMode = ok1(RetTypeAndMode),
            PredOrFunc = pf_function,
            project_tm_type_and_mode(RetTypeAndMode, RetType, RetMode),
            ArgTypes = ArgTypes0 ++ [RetType],
            ArgModes = ArgModes0 ++ [RetMode],
            PredInstInfo = pred_inst_info(PredOrFunc, ArgModes,
                arg_reg_types_unset, Detism)
        ),
        MaybeDetism = ok1(Detism)
    then
        Type = higher_order_type(PredOrFunc, ArgTypes,
            higher_order(PredInstInfo), Purity),
        MaybeType = ok1(Type)
    else
        (
            MaybeMaybeRetTypeAndMode = no,
            RetSpecs = []
        ;
            MaybeMaybeRetTypeAndMode = yes(ok1(_)),
            RetSpecs = []
        ;
            MaybeMaybeRetTypeAndMode = yes(error1(RetSpecs))
        ),
        Specs = ArgSpecs ++ RetSpecs ++
            get_any_errors1(MaybeDetism),
        MaybeType = error1(Specs)
    ).

:- pred project_tm_type_and_mode(type_and_maybe_mode::in,
    mer_type::out, mer_mode::out) is det.

project_tm_type_and_mode(type_and_mode(Type, Mode), Type, Mode).
project_tm_type_and_mode(type_only(_), _, _) :-
    % Since our ancestor should have passed require_tm_mode to
    % parse_type_and_mode, this should not happen.
    unexpected($pred, "type_only").

%---------------------------------------------------------------------------%

:- pred parse_types_no_modes(why_no_ho_inst_info::in, varset::in,
    arg_context_func::in, list(term)::in, int::in, list(mer_type)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

parse_types_no_modes(_, _, _, [], _, [], !Specs).
parse_types_no_modes(WhyNotHOInstInfo, VarSet, ArgContextFunc, [Term | Terms],
        ArgNum, Types, !Specs) :-
    parse_types_no_modes(WhyNotHOInstInfo, VarSet, ArgContextFunc, Terms,
        ArgNum + 1, TypesTail, !Specs),
    parse_type_no_mode(WhyNotHOInstInfo, VarSet, ArgContextFunc(ArgNum),
        Term, MaybeType),
    (
        MaybeType = ok1(Type),
        Types = [Type | TypesTail]
    ;
        MaybeType = error1(TSpecs),
        Types = TypesTail,
        !:Specs = TSpecs ++ !.Specs
    ).

:- pred parse_type_no_mode(why_no_ho_inst_info::in, varset::in,
    cord(format_piece)::in, term::in, maybe1(mer_type)::out) is det.

parse_type_no_mode(WhyNotHOInstInfo, VarSet, ContextPieces, Term, MaybeType) :-
    ( if Term = term.functor(term.atom("::"), [TypeTerm, _], _) then
        TypeTermStr = describe_error_term(VarSet, TypeTerm),
        no_ho_inst_allowed_desc(WhyNotHOInstInfo, Place, WhyNot),
        ( WhyNot = wna_by_design, NotAllowed = "not allowed"
        ; WhyNot = wna_nyi,       NotAllowed = "not (yet) allowed"
        ),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: the type")] ++
            color_as_subject([quote(TypeTermStr)]) ++
            [words("is followed by a mode, but this is")] ++
            color_as_incorrect([words(NotAllowed)]) ++
            [words("in"), words(Place), suffix("."), nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeType = error1([Spec])
    else
        AllowHOInstInfo = no_allow_ho_inst_info(WhyNotHOInstInfo),
        parse_type(AllowHOInstInfo, VarSet, ContextPieces, Term, MaybeType)
    ).

%---------------------------------------------------------------------------%

maybe_parse_types(AllowHOInstInfo, Terms, Types) :-
    % The values of VarSet and ContextPieces do not matter since we succeed
    % only if they aren't used.
    VarSet = varset.init,
    ContextPieces = cord.init,
    parse_types(AllowHOInstInfo, VarSet, ContextPieces, Terms, ok1(Types)).

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
    cord(format_piece)::in, list(term)::in,
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

parse_types_and_maybe_modes(_, _, _, _, _, [], _, [], !Specs).
parse_types_and_maybe_modes(MaybeInstConstraints, MaybeRequireMode, Why,
        VarSet, ArgContextFunc, [Term | Terms], ArgNum, TypesAndMaybeModes,
        !Specs) :-
    parse_types_and_maybe_modes(MaybeInstConstraints, MaybeRequireMode, Why,
        VarSet, ArgContextFunc, Terms, ArgNum + 1, TypesAndMaybeModesTail,
        !Specs),
    parse_type_and_maybe_mode(MaybeInstConstraints, MaybeRequireMode, Why,
        VarSet, ArgContextFunc(ArgNum), Term, MaybeTypeAndMaybeMode),
    (
        MaybeTypeAndMaybeMode = ok1(TypeAndMaybeMode),
        TypesAndMaybeModes = [TypeAndMaybeMode | TypesAndMaybeModesTail]
    ;
        MaybeTypeAndMaybeMode = error1(TMSpecs),
        TypesAndMaybeModes = TypesAndMaybeModesTail,
        !:Specs = TMSpecs ++ !.Specs
    ).

parse_type_and_maybe_mode(MaybeInstConstraints, MaybeRequireMode, Why, VarSet,
        ContextPieces, Term, MaybeTypeAndMode) :-
    ( if
        Term = term.functor(term.atom(Colons), [TypeTerm, ModeTerm], _Context),
        ( Colons = "::"
        ; Colons = ":"
        )
    then
        parse_type(no_allow_ho_inst_info(Why), VarSet, ContextPieces,
            TypeTerm, MaybeType),
        (
            MaybeInstConstraints = constrain_some_inst_vars(InstConstraints),
            parse_mode(allow_constrained_inst_var, VarSet, ContextPieces,
                ModeTerm, MaybeMode0),
            (
                MaybeMode0 = ok1(Mode0),
                constrain_inst_vars_in_mode_sub(InstConstraints, Mode0, Mode1),
                MaybeMode = ok1(Mode1)
            ;
                MaybeMode0 = error1(ModeSpecs),
                MaybeMode = error1(ModeSpecs)
            )
        ;
            MaybeInstConstraints = do_not_constrain_inst_vars,
            parse_mode(no_allow_constrained_inst_var(wnciv_type_and_mode),
                VarSet, ContextPieces, ModeTerm, MaybeMode)
        ),
        (
            Colons = "::",
            ColonSpecs = []
        ;
            Colons = ":",
            ColonPieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: the type and mode are separated by")] ++
                color_as_incorrect([words("one colon (\":\"),")]) ++
                [words("not")] ++
                color_as_correct([words("two (\"::\").")]) ++
                [nl],
            ColonSpecs = [spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), ColonPieces)]
        ),
        ( if
            MaybeType = ok1(Type),
            MaybeMode = ok1(Mode),
            ColonSpecs = []
        then
            MaybeTypeAndMode = ok1(type_and_mode(Type, Mode))
        else
            Specs = get_any_errors1(MaybeType) ++ get_any_errors1(MaybeMode) ++
                ColonSpecs,
            MaybeTypeAndMode = error1(Specs)
        )
    else
        (
            MaybeRequireMode = require_tm_mode,
            % XXX In tests/invalid_nodepend/combined_ho_type_inst_2.err_exp,
            % there are several examples of the diagnostics we generate here
            % reporting that ::mode suffixes are *required* after types,
            % being just next to other diagnostics that *forbid* ::mode
            % suffixes after types.
            TermStr = describe_error_term(VarSet, Term),
            MissingPieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error:")] ++
                color_as_incorrect([words("there should be a"),
                    quote("::mode"), words("suffix")]) ++
                [words("after")] ++
                color_as_subject([quote(TermStr), suffix(".")]) ++
                [nl],
            MissingSpec = spec($pred, severity_error, phase_t2pt,
                get_term_context(Term), MissingPieces),
            MaybeTypeAndMode = error1([MissingSpec])
        ;
            MaybeRequireMode = do_not_require_tm_mode,
            parse_type(no_allow_ho_inst_info(Why), VarSet, ContextPieces, Term,
                MaybeType),
            (
                MaybeType = ok1(Type),
                MaybeTypeAndMode = ok1(type_only(Type))
            ;
                MaybeType = error1(Specs),
                MaybeTypeAndMode = error1(Specs)
            )
        )
    ).

%---------------------------------------------------------------------------%

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
    ;       kctk_purity(string, purity, T)
    ;       kctk_empty_name(list(T)).

:- type known_type_kind(T)
    --->    known_type_simple(mer_type)
    ;       known_type_compound(known_compound_type_kind(T))
    ;       known_type_bad_arity(int).  % The expected arity.

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
            BuiltinType = builtin_type_int(int_type_int)
        ;
            Name = "uint",
            BuiltinType = builtin_type_int(int_type_uint)
        ;
            Name = "int8",
            BuiltinType = builtin_type_int(int_type_int8)
        ;
            Name = "uint8",
            BuiltinType = builtin_type_int(int_type_uint8)
        ;
            Name = "int16",
            BuiltinType = builtin_type_int(int_type_int16)
        ;
            Name = "uint16",
            BuiltinType = builtin_type_int(int_type_uint16)
        ;
            Name = "int32",
            BuiltinType = builtin_type_int(int_type_int32)
        ;
            Name = "uint32",
            BuiltinType = builtin_type_int(int_type_uint32)
        ;
            Name = "int64",
            BuiltinType = builtin_type_int(int_type_int64)
        ;
            Name = "uint64",
            BuiltinType = builtin_type_int(int_type_uint64)
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
            KnownType = known_type_bad_arity(0)
        )
    ;
        Name = "{}",
        KnownType = known_type_compound(kctk_tuple(Args))
    ;
        ( Name = "="
        ; Name = "=<"
        ),
        (
            ( Args = []
            ; Args = [_]
            ; Args = [_, _, _ | _]
            ),
            KnownType = known_type_bad_arity(2)
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
            KnownType = known_type_bad_arity(2)
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
            KnownType = known_type_bad_arity(1)
        ;
            Args = [Arg1],
            KnownType = known_type_compound(kctk_purity(Name, Purity, Arg1))
        )
    ;
        Name = "",
        KnownType = known_type_compound(kctk_empty_name(Args))
    ).

:- func no_ho_inst_allowed_result(cord(format_piece), why_no_ho_inst_info,
    varset, term) = maybe1(mer_type).

no_ho_inst_allowed_result(ContextPieces, WNHII, VarSet, Term) = Result :-
    no_ho_inst_allowed_desc(WNHII, Place, WhyNot),
    ( WhyNot = wna_by_design, NotAllowed = "not allowed"
    ; WhyNot = wna_nyi,       NotAllowed = "not (yet) allowed"
    ),
    TermStr = describe_error_term(VarSet, Term),
    Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
        words("Error: the type")] ++
        color_as_subject([quote(TermStr)]) ++
        [words("contains higher order inst information, but this is")] ++
        color_as_incorrect([words(NotAllowed)]) ++
        [words("in"), words(Place), suffix("."), nl],
    Spec = spec($pred, severity_error, phase_t2pt,
        get_term_context(Term), Pieces),
    Result = error1([Spec]).

:- type why_not_allowed
    --->    wna_by_design
    ;       wna_nyi.

:- pred no_ho_inst_allowed_desc(why_no_ho_inst_info::in,
    string::out, why_not_allowed::out) is det.

no_ho_inst_allowed_desc(WNHII, Place, WhyNot) :-
    (
        (
            WNHII = wnhii_type_ctor_arg,
            Place = "a type constructor's argument"
        ;
            WNHII = wnhii_type_qual,
            Place = "a type used for type qualification"
        ),
        WhyNot = wna_nyi
    ;
        (
            WNHII = wnhii_tuple_arg,
            Place = "a tuple type constructor's argument"
        ;
            WNHII = wnhii_pred_arg,
            Place = "a predicate's argument"
        ;
            WNHII = wnhii_func_arg,
            Place = "a function's argument"
        ;
            WNHII = wnhii_func_return_arg,
            Place = "a function's return value"
        ;
            WNHII = wnhii_supertype,
            Place = "a supertype of a subtype"
        ;
            WNHII = wnhii_solver_type_defn,
            Place = "the definition of a solver type"
        ;
            WNHII = wnhii_class_constraint,
            Place = "a class constraint"
        ;
            WNHII = wnhii_ctgc_type_selector,
            % Used only in error messages we won't print.
            Place = "type selector"
        ;
            WNHII = wnhii_user_struct_sharing,
            Place = "structure sharing annotation"
        ;
            WNHII = wnhii_pragma_struct_sharing,
            Place = "a structure_sharing pragma"
        ;
            WNHII = wnhii_pragma_struct_reuse,
            Place = "a structure_reuse pragma"
        ;
            WNHII = wnhii_pragma_type_spec_constr,
            Place = "a type_spec_constrained_preds pragma"
        ;
            WNHII = wnhii_pragma_type_spec,
            Place = "a type_spec pragma"
        ),
        WhyNot = wna_by_design
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_type_name.
%---------------------------------------------------------------------------%
