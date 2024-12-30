%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2016, 2019-2021, 2023-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the parse tree structure representations of
% predicate and function declarations back into Mercury source text.
%
% Many of the predicates in this module are have two slightly different
% versions. Either one is for functions and the other is for predicates,
% or one is for known functions and the other for predicates *and* for
% functions that are not known to be functions due to the use of a
% `with_type` annotation, which hides the return type.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_pred_decl.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

    % XXX Document me.
    %
:- pred mercury_format_pred_or_func_decl(output_lang::in, var_name_print::in,
    tvarset::in, inst_varset::in, pred_or_func::in, existq_tvars::in,
    sym_name::in, types_and_maybe_modes::in,
    maybe(mer_type)::in, maybe(mer_inst)::in, maybe(determinism)::in,
    purity::in, univ_exist_constraints::in, string::in, string::in, string::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

    % XXX Document me.
    %
:- pred mercury_format_func_decl(output_lang::in, var_name_print::in,
    tvarset::in, inst_varset::in, existq_tvars::in, sym_name::in,
    types_and_maybe_modes::in, maybe(determinism)::in, purity::in,
    univ_exist_constraints::in, string::in, string::in, string::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Output a `:- pred' declaration.
    %
:- func mercury_pred_type_to_string(tvarset, var_name_print,
    existq_tvars, sym_name, list(mer_type), maybe(determinism),
    purity, univ_exist_constraints) = string.
:- pred mercury_output_pred_type(io.text_output_stream::in, tvarset::in,
    var_name_print::in, existq_tvars::in, sym_name::in, list(mer_type)::in,
    maybe(determinism)::in, purity::in, univ_exist_constraints::in,
    io::di, io::uo) is det.
:- pred mercury_format_pred_type(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in,
    maybe(determinism)::in, purity::in, univ_exist_constraints::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

    % Output a `:- func' declaration.
    %
:- func mercury_func_type_to_string(tvarset, var_name_print,
    existq_tvars, sym_name, list(mer_type), mer_type, maybe(determinism),
    purity, univ_exist_constraints) = string.
:- pred mercury_output_func_type(io.text_output_stream::in,
    tvarset::in, var_name_print::in, existq_tvars::in, sym_name::in,
    list(mer_type)::in, mer_type::in, maybe(determinism)::in, purity::in,
    univ_exist_constraints::in, io::di, io::uo) is det.
:- pred mercury_format_func_type(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, mer_type::in,
    maybe(determinism)::in, purity::in, univ_exist_constraints::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % XXX Document me.
    %
:- func mercury_pred_mode_decl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), maybe(mer_inst), maybe(determinism)) = string.
:- pred mercury_output_pred_mode_decl(io.text_output_stream::in,
    output_lang::in, inst_varset::in, sym_name::in,
    list(mer_mode)::in, maybe(mer_inst)::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- pred mercury_format_pred_mode_decl(output_lang::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_pred_or_func_mode_decl_gen(output_lang::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, string::in, string::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

    % XXX Document me.
    %
:- func mercury_func_mode_decl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), mer_mode, maybe(determinism)) = string.
:- pred mercury_output_func_mode_decl(io.text_output_stream::in,
    output_lang::in, inst_varset::in, sym_name::in,
    list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- pred mercury_format_func_mode_decl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_func_mode_decl_gen(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    string::in, string::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

    % XXX Document me.
    %
:- func mercury_mode_subdecl_to_string(output_lang, pred_or_func, inst_varset,
    sym_name, list(mer_mode), maybe(determinism)) = string.
:- pred mercury_output_mode_subdecl(io.text_output_stream::in,
    output_lang::in, pred_or_func::in, inst_varset::in, sym_name::in,
    list(mer_mode)::in, maybe(determinism)::in, io::di, io::uo) is det.
:- pred mercury_format_mode_subdecl(output_lang::in, pred_or_func::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(determinism)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

    % XXX Document me.
    %
:- func mercury_pred_mode_subdecl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), maybe(determinism)) = string.
:- pred mercury_output_pred_mode_subdecl(io.text_output_stream::in,
    output_lang::in, inst_varset::in, sym_name::in,
    list(mer_mode)::in, maybe(determinism)::in, io::di, io::uo) is det.
:- pred mercury_format_pred_or_func_mode_subdecl(output_lang::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

    % XXX Document me.
    %
:- func mercury_func_mode_subdecl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), mer_mode, maybe(determinism)) = string.
:- pred mercury_output_func_mode_subdecl(io.text_output_stream::in,
    output_lang::in, inst_varset::in, sym_name::in,
    list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- pred mercury_format_func_mode_subdecl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pragma type_spec_constrained_preds([pt_output(Stream, State)],
    apply_to_superclasses,
    [subst([Stream => io.text_output_stream, State = io.state]),
    subst([Stream => string.builder.handle, State = string.builder.state])]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_util.

:- import_module require.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_format_pred_or_func_decl(Lang, VarNamePrint, TypeVarSet, InstVarSet,
        PredOrFunc, ExistQVars, PredName, TypesAndMaybeModes,
        MaybeWithType, MaybeWithInst, MaybeDet0, Purity, ClassContext,
        StartString, Separator, Terminator, S, !U) :-
    get_declared_types_and_maybe_modes(TypesAndMaybeModes, MaybeWithInst,
        MaybeDet0, Types, MaybeModes),
    (
        MaybeModes = yes(Modes),
        MaybeDet1 = maybe.no,
        mercury_format_pred_or_func_type_decl_2(TypeVarSet, VarNamePrint,
            PredOrFunc, ExistQVars, PredName, Types, MaybeWithType, MaybeDet1,
            Purity, ClassContext, StartString, Separator, S, !U),
        mercury_format_pred_or_func_mode_decl_gen(Lang, InstVarSet, PredName,
            Modes, MaybeWithInst, MaybeDet0, StartString, Terminator, S, !U)
    ;
        MaybeModes = no,
        mercury_format_pred_or_func_type_decl_2(TypeVarSet, VarNamePrint,
            PredOrFunc, ExistQVars, PredName, Types, MaybeWithType, MaybeDet0,
            Purity, ClassContext, StartString, Terminator, S, !U)
    ).

%---------------------%

mercury_format_func_decl(Lang, VarNamePrint, TypeVarSet, InstVarSet,
        ExistQVars, FuncName, TypesAndMaybeModes,
        MaybeDet, Purity, ClassContext, StartString, Separator, Terminator,
        S, !U) :-
    (
        TypesAndMaybeModes = no_types_arity_zero,
        % There should be at least a type for the return value.
        unexpected($pred, "no_types_arity_zero")
    ;
        TypesAndMaybeModes = types_only(Types),
        pred_args_to_func_args(Types, ArgTypes, RetType),
        mercury_format_func_type_2(TypeVarSet, VarNamePrint,
            ExistQVars, FuncName, ArgTypes, RetType, MaybeDet, Purity,
            ClassContext, StartString, Terminator, S, !U)
    ;
        TypesAndMaybeModes = types_and_modes(TypesAndModes),
        split_types_and_modes(TypesAndModes, Types, Modes),
        pred_args_to_func_args(Types, ArgTypes, RetType),
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        mercury_format_func_type_2(TypeVarSet, VarNamePrint,
            ExistQVars, FuncName, ArgTypes, RetType, no, Purity,
            ClassContext, StartString, Separator, S, !U),
        mercury_format_func_mode_decl_gen(Lang, InstVarSet, FuncName,
            ArgModes, RetMode, MaybeDet, StartString, Terminator, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_pred_type_to_string(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, MaybeDet, Purity, ClassContext) = Str :-
    State0 = string.builder.init,
    mercury_format_pred_type(TypeVarSet, VarNamePrint, ExistQVars,
        PredName, Types, MaybeDet, Purity, ClassContext,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_pred_type(Stream, TypeVarSet, VarNamePrint, ExistQVars,
        PredName, Types, MaybeDet, Purity, ClassContext, !IO) :-
    mercury_format_pred_type(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, MaybeDet, Purity, ClassContext, Stream, !IO).

mercury_format_pred_type(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, MaybeDet, Purity, ClassContext, S, !U) :-
    MaybeWithType = maybe.no,
    mercury_format_pred_or_func_type_decl_2(TypeVarSet, VarNamePrint,
        pf_predicate, ExistQVars, PredName, Types, MaybeWithType, MaybeDet,
        Purity, ClassContext, ":- ", ".\n", S, !U).

%---------------------%

mercury_func_type_to_string(TypeVarSet, VarNamePrint, ExistQVars, FuncName,
        ArgTypes, RetType, MaybeDet, Purity, ClassContext) = Str :-
    State0 = string.builder.init,
    mercury_format_func_type(TypeVarSet, VarNamePrint, ExistQVars,
        FuncName, ArgTypes, RetType, MaybeDet, Purity, ClassContext,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_func_type(Stream, VarSet, ExistQVars, FuncName,
        ArgTypes, RetType, MaybeDet, Purity, ClassContext, VarNamePrint,
        !IO) :-
    mercury_format_func_type(VarSet, ExistQVars, FuncName, ArgTypes, RetType,
        MaybeDet, Purity, ClassContext, VarNamePrint, Stream, !IO).

mercury_format_func_type(TypeVarSet, VarNamePrint, ExistQVars, FuncName,
        ArgTypes, RetType, MaybeDet, Purity, ClassContext, S, !U) :-
    mercury_format_func_type_2(TypeVarSet, VarNamePrint, ExistQVars, FuncName,
        ArgTypes, RetType, MaybeDet, Purity, ClassContext, ":- ", ".\n",
        S, !U).

%---------------------------------------------------------------------------%
%
% Predicates used in the implementation of both
%   mercury_format_pred_or_func_decl/mercury_format_func_decl
% and
%   mercury_format_pred_type/mercury_format_func_type.
%

:- pred mercury_format_pred_or_func_type_decl_2( tvarset::in,
    var_name_print::in, pred_or_func::in, existq_tvars::in,
    sym_name::in, list(mer_type)::in, maybe(mer_type)::in,
    maybe(determinism)::in, purity::in, univ_exist_constraints::in,
    string::in, string::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pred_or_func_type_decl_2(TypeVarSet, VarNamePrint, PredOrFunc,
        ExistQVars, PredName, Types, MaybeWithType, MaybeDet, Purity,
        Constraints, StartString, Separator, S, !U) :-
    add_string(StartString, S, !U),
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, S, !U),
    Constraints = univ_exist_constraints(UnivConstraints, ExistConstraints),
    ( if
        ExistQVars = [],
        ExistConstraints = []
    then
        MaybeExistConstraints = no_exist_constraints
    else
        MaybeExistConstraints =
            have_exist_constraints_print_paren(ExistConstraints),
        add_string("(", S, !U)
    ),
    add_purity_prefix(Purity, S, !U),
    PredOrFuncStr = pred_or_func_to_str(PredOrFunc),
    add_string(PredOrFuncStr, S, !U),
    add_string(" ", S, !U),
    (
        Types = [_ | _],
        % The following left parenthesis is a graphic token.
        mercury_format_sym_name_ngt(next_to_graphic_token, PredName, S, !U),
        add_string("(", S, !U),
        add_list(mercury_format_type(TypeVarSet, VarNamePrint), ", ", Types,
            S, !U),
        add_string(")", S, !U)
    ;
        Types = [],
        % In a zero arity predicate declaration containing no determinism
        % information, the next token could be the period that signifies
        % the end of the declaration.
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
            S, !U)
    ),
    (
        MaybeWithType = yes(WithType),
        add_string(" `with_type` (", S, !U),
        mercury_format_type(TypeVarSet, VarNamePrint, WithType, S, !U),
        add_string(")", S, !U)
    ;
        MaybeWithType = no
    ),
    % We need to handle is/2 specially, because it is used for determinism
    % annotations (`... is det'), and so the compiler will misinterpret
    % a bare `:- pred is(int, int_expr)' as % `:- pred int is int_expr'
    % and then report some very confusing error message. Thus you _have_
    % to give a determinism annotation in the pred declaration for is/2,
    % e.g. `:- pred is(int, int_expr) is det.' (Yes, this made me puke too.)
    %
    % The alternative is a term traversal in parse_determinism_suffix in
    % parse_item.m. That alternative is nicer, but it is less efficient.
    ( if
        PredOrFunc = pf_predicate,
        MaybeDet = no,
        unqualify_name(PredName) = "is",
        list.length(Types, 2)
    then
        % This determinism will be ignored.
        mercury_format_det_annotation(yes(detism_det), S, !U)
    else
        mercury_format_det_annotation(MaybeDet, S, !U)
    ),
    mercury_format_class_context(TypeVarSet, VarNamePrint,
        UnivConstraints, MaybeExistConstraints, S, !U),
    add_string(Separator, S, !U).

%---------------------%

:- pred mercury_format_func_type_2(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, mer_type::in,
    maybe(determinism)::in, purity::in, univ_exist_constraints::in,
    string::in, string::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_func_type_2(VarSet, VarNamePrint, ExistQVars, FuncName, Types,
        RetType, MaybeDet, Purity, Constraints, StartString, Separator,
        S, !U) :-
    add_string(StartString, S, !U),
    mercury_format_quantifier(VarSet, VarNamePrint, ExistQVars, S, !U),
    Constraints = univ_exist_constraints(UnivConstraints, ExistConstraints),
    ( if
        ExistQVars = [],
        ExistConstraints = []
    then
        MaybeExistConstraints = no_exist_constraints
    else
        MaybeExistConstraints =
            have_exist_constraints_print_paren(ExistConstraints),
        add_string("(", S, !U)
    ),
    add_purity_prefix(Purity, S, !U),
    add_string("func ", S, !U),
    (
        Types = [_ | _],
        % The following left parenthesis is a graphic token.
        mercury_format_sym_name_ngt(next_to_graphic_token, FuncName, S, !U),
        add_string("(", S, !U),
        add_list(mercury_format_type(VarSet, VarNamePrint), ", ", Types,
            S, !U),
        add_string(")", S, !U)
    ;
        Types = [],
        % In a zero arity predicate declaration containing no determinism
        % information, the next token could be the period that signifies
        % the end of the declaration. For functions, that cannot happen,
        % but treating this call as being next to a graphic token is
        % consistent with the corresponding code for predicates above.
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, FuncName,
            S, !U)
    ),
    add_string(" = ", S, !U),
    mercury_format_type(VarSet, VarNamePrint, RetType, S, !U),
    mercury_format_det_annotation(MaybeDet, S, !U),
    mercury_format_class_context(VarSet, VarNamePrint,
        UnivConstraints, MaybeExistConstraints, S, !U),
    add_string(Separator, S, !U).

%---------------------------------------------------------------------------%

mercury_pred_mode_decl_to_string(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet) = Str :-
    State0 = string.builder.init,
    mercury_format_pred_mode_decl(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_pred_mode_decl(Stream, Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, !IO) :-
    mercury_format_pred_mode_decl(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, Stream, !IO).

mercury_format_pred_mode_decl(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, S, !U) :-
    mercury_format_pred_or_func_mode_decl_gen(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, ":- ", ".\n", S, !U).

mercury_format_pred_or_func_mode_decl_gen(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, StartString, Separator, S, !U) :-
    add_string(StartString, S, !U),
    add_string("mode ", S, !U),
    mercury_format_pred_or_func_mode_subdecl(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, S, !U),
    add_string(Separator, S, !U).

%---------------------%

mercury_func_mode_decl_to_string(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet) = Str :-
    State0 = string.builder.init,
    mercury_format_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_func_mode_decl(Stream, Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, !IO) :-
    mercury_format_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, Stream, !IO).

mercury_format_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, S, !U) :-
    mercury_format_func_mode_decl_gen(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, ":- ", ".\n", S, !U).

mercury_format_func_mode_decl_gen(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, StartString, Separator, S, !U) :-
    add_string(StartString, S, !U),
    add_string("mode ", S, !U),
    mercury_format_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, S, !U),
    add_string(Separator, S, !U).

%---------------------------------------------------------------------------%

mercury_mode_subdecl_to_string(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet) = Str :-
    State0 = string.builder.init,
    mercury_format_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_mode_subdecl(Stream, Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, !IO) :-
    mercury_format_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, Stream, !IO).

mercury_format_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, S, !U) :-
    (
        PredOrFunc = pf_predicate,
        mercury_format_pred_or_func_mode_subdecl(Lang, InstVarSet, Name,
            Modes, no, MaybeDet, S, !U)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        mercury_format_func_mode_subdecl(Lang, InstVarSet, Name, ArgModes,
            RetMode, MaybeDet, S, !U)
    ).

%---------------------%

mercury_pred_mode_subdecl_to_string(Lang, VarSet, PredName, Modes, MaybeDet)
        = Str :-
    State0 = string.builder.init,
    mercury_format_pred_or_func_mode_subdecl(Lang, VarSet, PredName, Modes, no,
        MaybeDet, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_pred_mode_subdecl(Stream, Lang, VarSet, PredName, Modes,
        MaybeDet, !IO) :-
    mercury_format_pred_or_func_mode_subdecl(Lang, VarSet, PredName,
        Modes, no, MaybeDet, Stream, !IO).

mercury_format_pred_or_func_mode_subdecl(Lang, InstVarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, S, !U) :-
    (
        Modes = [_ | _],
        mercury_format_sym_name_ngt(next_to_graphic_token, PredName, S, !U),
        add_string("(", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, Modes, S, !U),
        add_string(")", S, !U)
    ;
        Modes = [],
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, PredName,
            S, !U)
    ),
    (
        MaybeWithInst = yes(WithInst),
        add_string(" `with_inst` (", S, !U),
        mercury_format_inst(Lang, InstVarSet, WithInst, S, !U),
        add_string(")", S, !U)
    ;
        MaybeWithInst = no
    ),
    mercury_format_det_annotation(MaybeDet, S, !U).

%---------------------%

mercury_func_mode_subdecl_to_string(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet) = Str :-
    State0 = string.builder.init,
    mercury_format_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_func_mode_subdecl(Stream, Lang, VarSet, FuncName,
        Modes, RetMode, MaybeDet, !IO) :-
    mercury_format_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, Stream, !IO).

mercury_format_func_mode_subdecl(Lang, InstVarSet, FuncName, Modes, RetMode,
        MaybeDet, S, !U) :-
    (
        Modes = [_ | _],
        mercury_format_sym_name_ngt(next_to_graphic_token, FuncName, S, !U),
        add_string("(", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, Modes, S, !U),
        add_string(")", S, !U)
    ;
        Modes = [],
        mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, FuncName,
            S, !U)
    ),
    add_string(" = ", S, !U),
    mercury_format_mode(Lang, InstVarSet, RetMode, S, !U),
    mercury_format_det_annotation(MaybeDet, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_det_annotation(maybe(determinism)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_det_annotation(MaybeDet, S, !U) :-
    (
        MaybeDet = no
    ;
        MaybeDet = yes(Det),
        add_string(" is ", S, !U),
        add_string(mercury_det_to_string(Det), S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_pred_decl.
%---------------------------------------------------------------------------%
