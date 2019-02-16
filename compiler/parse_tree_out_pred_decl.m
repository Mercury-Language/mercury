%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
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
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % XXX Document me.
    %
:- pred mercury_format_pred_or_func_decl(output_lang::in, tvarset::in,
    inst_varset::in, pred_or_func::in, existq_tvars::in, sym_name::in,
    list(type_and_mode)::in, maybe(mer_type)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, purity::in, prog_constraints::in,
    string::in, string::in, string::in, U::di, U::uo) is det <= output(U).

    % XXX Document me.
    %
:- pred mercury_format_func_decl(output_lang::in, tvarset::in, inst_varset::in,
    existq_tvars::in, sym_name::in, list(type_and_mode)::in,
    type_and_mode::in, maybe(determinism)::in, purity::in,
    prog_constraints::in, string::in, string::in, string::in, U::di, U::uo)
    is det <= output(U).

%---------------------------------------------------------------------------%

    % Output a `:- pred' declaration.
    %
:- pred mercury_output_pred_type(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, maybe(determinism)::in,
    purity::in, prog_constraints::in, io::di, io::uo) is det.
:- func mercury_pred_type_to_string(tvarset, var_name_print,
    existq_tvars, sym_name, list(mer_type), maybe(determinism),
    purity, prog_constraints) = string.

    % Output a `:- func' declaration.
    %
:- pred mercury_output_func_type(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, mer_type::in,
    maybe(determinism)::in, purity::in, prog_constraints::in,
    io::di, io::uo) is det.
:- func mercury_func_type_to_string(tvarset, var_name_print,
    existq_tvars, sym_name, list(mer_type), mer_type, maybe(determinism),
    purity, prog_constraints) = string.

%---------------------------------------------------------------------------%

    % XXX Document me.
    %
:- pred mercury_output_pred_mode_decl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, io::di, io::uo) is det.
:- func mercury_pred_mode_decl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), maybe(mer_inst), maybe(determinism)) = string.
:- pred mercury_format_pred_or_func_mode_decl(output_lang::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, string::in, string::in,
    U::di, U::uo) is det <= output(U).

    % XXX Document me.
    %
:- pred mercury_output_func_mode_decl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- func mercury_func_mode_decl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), mer_mode, maybe(determinism)) = string.
:- pred mercury_format_func_mode_decl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    string::in, string::in, U::di, U::uo) is det <= output(U).

    % XXX Document me.
    %
:- pred mercury_output_mode_subdecl(output_lang::in, pred_or_func::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- func mercury_mode_subdecl_to_string(output_lang, pred_or_func, inst_varset,
    sym_name, list(mer_mode), maybe(determinism)) = string.

    % XXX Document me.
    %
:- pred mercury_output_pred_mode_subdecl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- func mercury_pred_mode_subdecl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), maybe(determinism)) = string.

    % XXX Document me.
    %
:- pred mercury_output_func_mode_subdecl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    io::di, io::uo) is det.
:- func mercury_func_mode_subdecl_to_string(output_lang, inst_varset, sym_name,
    list(mer_mode), mer_mode, maybe(determinism)) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module varset.

%---------------------------------------------------------------------------%

mercury_format_pred_or_func_decl(Lang, TypeVarSet, InstVarSet,
        PredOrFunc, ExistQVars, PredName, TypesAndModes, WithType, WithInst,
        MaybeDet, Purity, ClassContext, StartString,
        Separator, Terminator, !IO) :-
    split_types_and_modes(TypesAndModes, Types, MaybeModes),
    ( if
        MaybeModes = yes(Modes),
        ( Modes = [_ | _]
        ; WithInst = yes(_)
        )
    then
        mercury_format_pred_or_func_type_2(TypeVarSet, print_name_only,
            PredOrFunc, ExistQVars, PredName, Types, WithType, no,
            Purity, ClassContext, StartString, Separator, !IO),
        mercury_format_pred_or_func_mode_decl(Lang, InstVarSet, PredName,
            Modes, WithInst, MaybeDet, StartString, Terminator, !IO)
    else
        mercury_format_pred_or_func_type_2(TypeVarSet, print_name_only,
            PredOrFunc, ExistQVars, PredName, Types, WithType, MaybeDet,
            Purity, ClassContext, StartString, Terminator, !IO)
    ).

%---------------------%

mercury_format_func_decl(Lang, TypeVarSet, InstVarSet, ExistQVars, FuncName,
        TypesAndModes, RetTypeAndMode, MaybeDet, Purity, ClassContext,
        StartString, Separator, Terminator, !U) :-
    split_types_and_modes(TypesAndModes, Types, MaybeModes),
    split_type_and_mode(RetTypeAndMode, RetType, MaybeRetMode),
    ( if
        MaybeModes = yes(Modes),
        MaybeRetMode = yes(RetMode)
    then
        mercury_format_func_type_2(TypeVarSet, print_name_only,
            ExistQVars, FuncName, Types, RetType, no, Purity,
            ClassContext, StartString, Separator, !U),
        mercury_format_func_mode_decl(Lang, InstVarSet, FuncName, Modes,
            RetMode, MaybeDet, StartString, Terminator, !U)
    else
        mercury_format_func_type_2(TypeVarSet, print_name_only,
            ExistQVars, FuncName, Types, RetType, MaybeDet, Purity,
            ClassContext, StartString, Terminator, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_pred_type(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, MaybeDet, Purity, ClassContext, !IO) :-
    mercury_format_pred_type(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, no, MaybeDet, Purity, ClassContext, !IO).

mercury_pred_type_to_string(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, MaybeDet, Purity, ClassContext) = String :-
    mercury_format_pred_type(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, no, MaybeDet, Purity, ClassContext, "", String).

:- pred mercury_format_pred_type(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, maybe(mer_type)::in,
    maybe(determinism)::in, purity::in, prog_constraints::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pred_type(TypeVarSet, VarNamePrint, ExistQVars, PredName,
        Types, WithType, MaybeDet, Purity, ClassContext, !U) :-
    mercury_format_pred_or_func_type_2(TypeVarSet, VarNamePrint,
        pf_predicate, ExistQVars, PredName, Types, WithType, MaybeDet,
        Purity, ClassContext, ":- ", ".\n", !U).

%---------------------%

mercury_output_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
        MaybeDet, Purity, ClassContext, VarNamePrint, !IO) :-
    mercury_format_func_type(VarSet, ExistQVars, FuncName, Types, RetType,
        MaybeDet, Purity, ClassContext, VarNamePrint, !IO).

mercury_func_type_to_string(TypeVarSet, VarNamePrint, ExistQVars,
        FuncName, Types, RetType, MaybeDet, Purity, ClassContext) = String :-
    mercury_format_func_type(TypeVarSet, VarNamePrint, ExistQVars,
        FuncName, Types, RetType, MaybeDet, Purity, ClassContext,
        "", String).

:- pred mercury_format_func_type(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, mer_type::in,
    maybe(determinism)::in, purity::in, prog_constraints::in,
    U::di, U::uo) is det <= output(U).

mercury_format_func_type(TypeVarSet, VarNamePrint, ExistQVars, FuncName,
        Types, RetType, MaybeDet, Purity, ClassContext, !U) :-
    mercury_format_func_type_2(TypeVarSet, VarNamePrint, ExistQVars, FuncName,
        Types, RetType, MaybeDet, Purity, ClassContext, ":- ", ".\n", !U).

%---------------------------------------------------------------------------%
%
% Predicates used in the implementation of both
%   mercury_format_pred_or_func_decl/mercury_format_func_decl
% and
%   mercury_format_pred_type/mercury_format_func_type.
%

:- pred mercury_format_pred_or_func_type_2( tvarset::in, var_name_print::in,
    pred_or_func::in, existq_tvars::in, sym_name::in, list(mer_type)::in,
    maybe(mer_type)::in, maybe(determinism)::in, purity::in,
    prog_constraints::in, string::in, string::in,
    U::di, U::uo) is det <= output(U).

mercury_format_pred_or_func_type_2(TypeVarSet, VarNamePrint, PredOrFunc,
        ExistQVars, PredName, Types, MaybeWithType, MaybeDet, Purity,
        ClassContext, StartString, Separator, !U) :-
    add_string(StartString, !U),
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, !U),
    ( if
        ExistQVars = [],
        ClassContext = constraints(_, [])
    then
        true
    else
        add_string("(", !U)
    ),
    add_purity_prefix(Purity, !U),
    PredOrFuncStr = pred_or_func_to_str(PredOrFunc),
    add_string(PredOrFuncStr, !U),
    add_string(" ", !U),
    (
        Types = [_ | _],
        mercury_format_sym_name(PredName, !U),
        add_string("(", !U),
        add_list(Types, ", ", mercury_format_type(TypeVarSet, VarNamePrint),
            !U),
        add_string(")", !U)
    ;
        Types = [],
        mercury_format_bracketed_sym_name(PredName, !U)
    ),
    (
        MaybeWithType = yes(WithType),
        add_string(" `with_type` (", !U),
        mercury_format_type(TypeVarSet, VarNamePrint, WithType, !U),
        add_string(")", !U)
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
        mercury_format_det_annotation(yes(detism_det), !U)
    else
        mercury_format_det_annotation(MaybeDet, !U)
    ),
    mercury_format_class_context(TypeVarSet, VarNamePrint,
        ClassContext, ExistQVars, !U),
    add_string(Separator, !U).

%---------------------%

:- pred mercury_format_func_type_2(tvarset::in, var_name_print::in,
    existq_tvars::in, sym_name::in, list(mer_type)::in, mer_type::in,
    maybe(determinism)::in, purity::in, prog_constraints::in,
    string::in, string::in, U::di, U::uo) is det <= output(U).

mercury_format_func_type_2(VarSet, VarNamePrint, ExistQVars, FuncName, Types,
        RetType, MaybeDet, Purity, ClassContext, StartString, Separator, !U) :-
    add_string(StartString, !U),
    mercury_format_quantifier(VarSet, VarNamePrint, ExistQVars, !U),
    ( if
        ExistQVars = [],
        ClassContext = constraints(_, [])
    then
        true
    else
        add_string("(", !U)
    ),
    add_purity_prefix(Purity, !U),
    add_string("func ", !U),
    (
        Types = [_ | _],
        mercury_format_sym_name(FuncName, !U),
        add_string("(", !U),
        add_list(Types, ", ", mercury_format_type(VarSet, VarNamePrint), !U),
        add_string(")", !U)
    ;
        Types = [],
        mercury_format_bracketed_sym_name(FuncName, !U)
    ),
    add_string(" = ", !U),
    mercury_format_type(VarSet, VarNamePrint, RetType, !U),
    mercury_format_det_annotation(MaybeDet, !U),
    mercury_format_class_context(VarSet, VarNamePrint,
        ClassContext, ExistQVars, !U),
    add_string(Separator, !U).

%---------------------------------------------------------------------------%

mercury_output_pred_mode_decl(Lang, VarSet, PredName, Modes, WithInst,
        MaybeDet, !IO) :-
    mercury_format_pred_or_func_mode_decl(Lang, VarSet, PredName, Modes,
        WithInst, MaybeDet, ":- ", ".\n", !IO).

mercury_pred_mode_decl_to_string(Lang, VarSet, PredName, Modes, MaybeWithInst,
        MaybeDet) = String :-
    mercury_format_pred_or_func_mode_decl(Lang, VarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, ":- ", ".\n", "", String).

mercury_format_pred_or_func_mode_decl(Lang, VarSet, PredName, Modes,
        WithInst, MaybeDet, StartString, Separator, !U) :-
    add_string(StartString, !U),
    add_string("mode ", !U),
    mercury_format_pred_or_func_mode_subdecl(Lang, VarSet, PredName, Modes,
        WithInst, MaybeDet, !U),
    add_string(Separator, !U).

%---------------------%

mercury_output_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode, MaybeDet,
        !IO) :-
    mercury_format_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, ":- ", ".\n", !IO).

mercury_func_mode_decl_to_string(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet) = String :-
    mercury_format_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, ":- ", ".\n", "", String).

mercury_format_func_mode_decl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, StartString, Separator, !U) :-
    add_string(StartString, !U),
    add_string("mode ", !U),
    mercury_format_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, !U),
    add_string(Separator, !U).

%---------------------------------------------------------------------------%

mercury_output_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, !IO) :-
    mercury_format_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, !IO).

mercury_mode_subdecl_to_string(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet) = String :-
    mercury_format_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, "", String).

:- pred mercury_format_mode_subdecl(output_lang::in, pred_or_func::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(determinism)::in,
    U::di, U::uo) is det <= output(U).

mercury_format_mode_subdecl(Lang, PredOrFunc, InstVarSet, Name, Modes,
        MaybeDet, !U) :-
    (
        PredOrFunc = pf_predicate,
        mercury_format_pred_or_func_mode_subdecl(Lang, InstVarSet, Name,
            Modes, no, MaybeDet, !U)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        mercury_format_func_mode_subdecl(Lang, InstVarSet, Name, ArgModes,
            RetMode, MaybeDet, !U)
    ).

%---------------------%

mercury_output_pred_mode_subdecl(Lang, VarSet, PredName, Modes, MaybeDet,
        !IO) :-
    mercury_format_pred_or_func_mode_subdecl(Lang, VarSet, PredName,
        Modes, no, MaybeDet, !IO).

mercury_pred_mode_subdecl_to_string(Lang, VarSet, PredName, Modes, MaybeDet)
        = String :-
    mercury_format_pred_or_func_mode_subdecl(Lang, VarSet, PredName,
        Modes, no, MaybeDet, "", String).

:- pred mercury_format_pred_or_func_mode_subdecl(output_lang::in,
    inst_varset::in, sym_name::in, list(mer_mode)::in, maybe(mer_inst)::in,
    maybe(determinism)::in, U::di, U::uo) is det <= output(U).

mercury_format_pred_or_func_mode_subdecl(Lang, InstVarSet, PredName, Modes,
        MaybeWithInst, MaybeDet, !U) :-
    (
        Modes = [_ | _],
        mercury_format_sym_name(PredName, !U),
        add_string("(", !U),
        mercury_format_mode_list(Lang, InstVarSet, Modes, !U),
        add_string(")", !U)
    ;
        Modes = [],
        mercury_format_bracketed_sym_name(PredName, !U)
    ),
    (
        MaybeWithInst = yes(WithInst),
        add_string(" `with_inst` (", !U),
        mercury_format_inst(Lang, InstVarSet, WithInst, !U),
        add_string(")", !U)
    ;
        MaybeWithInst = no
    ),
    mercury_format_det_annotation(MaybeDet, !U).

%---------------------%

mercury_output_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, !IO) :-
    mercury_format_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, !IO).

mercury_func_mode_subdecl_to_string(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet) = String :-
    mercury_format_func_mode_subdecl(Lang, VarSet, FuncName, Modes, RetMode,
        MaybeDet, "", String).

:- pred mercury_format_func_mode_subdecl(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_mode)::in, mer_mode::in, maybe(determinism)::in,
    U::di, U::uo) is det <= output(U).

mercury_format_func_mode_subdecl(Lang, InstVarSet, FuncName, Modes, RetMode,
        MaybeDet, !U) :-
    (
        Modes = [_ | _],
        mercury_format_sym_name(FuncName, !U),
        add_string("(", !U),
        mercury_format_mode_list(Lang, InstVarSet, Modes, !U),
        add_string(")", !U)
    ;
        Modes = [],
        mercury_format_bracketed_sym_name(FuncName, !U)
    ),
    add_string(" = ", !U),
    mercury_format_mode(Lang, InstVarSet, RetMode, !U),
    mercury_format_det_annotation(MaybeDet, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_det_annotation(maybe(determinism)::in, U::di, U::uo)
    is det <= output(U).

mercury_format_det_annotation(MaybeDet, !U) :-
    (
        MaybeDet = no
    ;
        MaybeDet = yes(Det),
        add_string(" is ", !U),
        add_string(mercury_det_to_string(Det), !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_pred_decl.
%---------------------------------------------------------------------------%
