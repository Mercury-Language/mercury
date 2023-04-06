%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_out_mode.m.
% Main authors: conway, fjh.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_mode.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- func instmap_to_string(var_table, var_name_print, int, instmap) = string.

%---------------------------------------------------------------------------%

:- type incl_addr
    --->    do_not_incl_addr
    ;       do_incl_addr.

    % Output a list of insts in a format that makes them easy to read
    % but may not be valid Mercury.
    %
:- pred mercury_output_structured_inst_list(io.text_output_stream::in,
    output_lang::in, inst_varset::in, incl_addr::in, int::in,
    list(mer_inst)::in, io::di, io::uo) is det.
:- func mercury_structured_inst_list_to_string(output_lang, inst_varset,
    incl_addr, int, list(mer_inst)) = string.

    % Output an inst in a format that makes it easy to read
    % but may not be valid Mercury.
    % The `int' argument specifies the indentation level.
    % (These routines are used with `--debug-modes'.)
    %
:- pred mercury_output_structured_inst(io.text_output_stream::in,
    output_lang::in, inst_varset::in, incl_addr::in, int::in, mer_inst::in,
    io::di, io::uo) is det.
:- func mercury_structured_inst_to_string(output_lang, inst_varset, incl_addr,
    int, mer_inst) = string.

%---------------------------------------------------------------------------%

:- pred mercury_output_structured_unify_mode_list(io.text_output_stream::in,
    output_lang::in, inst_varset::in, incl_addr::in, int::in,
    list(unify_mode)::in, io::di, io::uo) is det.
:- func mercury_structured_unify_mode_list_to_string(output_lang, inst_varset,
    incl_addr, int, list(unify_mode)) = string.

:- pred mercury_output_structured_unify_mode(io.text_output_stream::in,
    output_lang::in, inst_varset::in, incl_addr::in, int::in, unify_mode::in,
    io::di, io::uo) is det.
:- func mercury_structured_unify_mode_to_string(output_lang, inst_varset,
    incl_addr, int, unify_mode) = string.

%---------------------------------------------------------------------------%

:- pred mercury_output_unify_mode_list(io.text_output_stream::in,
    inst_varset::in, list(unify_mode)::in, io::di, io::uo) is det.
:- func mercury_unify_mode_list_to_string(inst_varset, list(unify_mode))
    = string.

:- pred mercury_output_unify_mode(io.text_output_stream::in,
    inst_varset::in, unify_mode::in, io::di, io::uo) is det.
:- func mercury_unify_mode_to_string(inst_varset, unify_mode) = string.

:- implementation.

:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_tree_to_term.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module term_context.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%

instmap_to_string(VarTable, VarNamePrint, Indent, InstMap) = Str :-
    ( if instmap_is_unreachable(InstMap) then
        Str = "unreachable"
    else
        instmap_to_assoc_list(InstMap, AssocList),
        Str = var_inst_list_to_string(VarTable, VarNamePrint, Indent,
            AssocList)
    ).

:- func var_inst_list_to_string(var_table, var_name_print, int,
    assoc_list(prog_var, mer_inst)) = string.

var_inst_list_to_string(_, _, _, []) = "".
var_inst_list_to_string(VarTable, VarNamePrint, Indent,
        [Var - Inst | VarsInsts]) = Str :-
    VarStr = mercury_var_to_string(VarTable, VarNamePrint, Var),
    varset.init(InstVarSet),
    InstStr = mercury_inst_to_string(output_debug, InstVarSet, Inst),
    string.format("%s -> %s", [s(VarStr), s(InstStr)], VarInstStr),
    (
        VarsInsts = [],
        Str = VarInstStr
    ;
        VarsInsts = [_ | _],
        VarsInstsStr = var_inst_list_to_string(VarTable, VarNamePrint,
            Indent, VarsInsts),
        string.duplicate_char('\t', Indent, IndentStr),
        Prefix= "%            ",
        string.format("%s\n%s%s%s",
            [s(VarInstStr), s(IndentStr), s(Prefix), s(VarsInstsStr)], Str)
    ).

%---------------------------------------------------------------------------%

mercury_output_structured_inst_list(Stream, Lang, InstVarSet, InclAddr, Indent,
        Insts, !IO) :-
    mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr, Indent,
        Insts, Stream, !IO).

mercury_structured_inst_list_to_string(Lang, InstVarSet, InclAddr, Indent,
        Insts) = String :-
    mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr, Indent,
        Insts, unit, "", String).

:- pred mercury_format_structured_inst_list(output_lang::in,
    inst_varset::in, incl_addr::in, int::in, list(mer_inst)::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_structured_inst_list(_, _, _, _, [], _, !U).
mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr, Indent,
        [Inst | Insts], S, !U) :-
    (
        Insts = [],
        Suffix = "\n"
    ;
        Insts = [_ | _],
        Suffix = ",\n"
    ),
    mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent, Suffix,
        Inst, S, !U),
    mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr, Indent,
        Insts, S, !U).

%---------------------------------------------------------------------------%

mercury_output_structured_inst(Stream, Lang, InstVarSet, InclAddr, Indent,
        Inst, !IO) :-
    mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
        "\n", Inst, Stream, !IO).

mercury_structured_inst_to_string(Lang, InstVarSet, InclAddr, Indent, Inst)
        = String :-
    mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
        "\n", Inst, unit, "", String).

:- pred mercury_format_structured_inst(output_lang::in, inst_varset::in,
    incl_addr::in, int::in, string::in, mer_inst::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
        Suffix, Inst, S, !U) :-
    mercury_format_tabs(Indent, S, !U),
    (
        InclAddr = do_not_incl_addr
    ;
        InclAddr = do_incl_addr,
        get_inst_addr(Inst, InstAddr),
        InstAddrStr = string.format("%x", [i(InstAddr)]),
        add_string(InstAddrStr, S, !U),
        add_string(": ", S, !U)
    ),
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_any_pred_inst_info(output_debug, InstVarSet,
                Uniq, PredInstInfo, S, !U)
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, S, !U)
        ),
        add_string(Suffix, S, !U)
    ;
        Inst = free,
        add_string("free", S, !U),
        add_string(Suffix, S, !U)
    ;
        Inst = free(_T),
        add_string("free(with some type)", S, !U),
        add_string(Suffix, S, !U)
    ;
        Inst = bound(Uniq, InstResults, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", S, !U),
        add_string("(\n", S, !U),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            InstResultsTerm =
                inst_test_results_to_term(dummy_context, InstResults),
            InstResultsStr = mercury_term_to_string_vs(varset.init,
                print_num_only, InstResultsTerm),
            mercury_format_tabs(Indent + 1, S, !U),
            add_string(InstResultsStr, S, !U),
            add_string(",\n", S, !U)
        ),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            mercury_format_tabs(Indent + 1, S, !U),
            add_string("[\n", S, !U)
        ),
        mercury_format_structured_bound_insts(Lang, InstVarSet, InclAddr,
            Indent + 1, BoundInsts, S, !U),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            mercury_format_tabs(Indent + 1, S, !U),
            add_string("]\n", S, !U)
        ),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_ground_pred_inst_info(output_debug, InstVarSet,
                Uniq, PredInstInfo, S, !U)
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", S, !U)
        ),
        add_string(Suffix, S, !U)
    ;
        Inst = inst_var(Var),
        mercury_format_var_vs(InstVarSet, print_name_only, Var, S, !U),
        add_string(Suffix, S, !U)
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        mercury_format_constrained_inst_vars(output_debug, InstVarSet,
            Vars, ConstrainedInst, S, !U),
        add_string(Suffix, S, !U)
    ;
        Inst = abstract_inst(Name, Args),
        mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr,
            Indent, yes, Suffix, user_inst(Name, Args), S, !U)
    ;
        Inst = defined_inst(InstName),
        mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr,
            Indent, yes, Suffix, InstName, S, !U)
    ;
        Inst = not_reached,
        add_string("not_reached", S, !U),
        add_string(Suffix, S, !U)
    ).

:- pred mercury_format_structured_bound_insts(output_lang::in,
    inst_varset::in, incl_addr::in, int::in, list(bound_inst)::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_structured_bound_insts(_, _, _, _, [], _, !U).
mercury_format_structured_bound_insts(Lang, InstVarSet, InclAddr, Indent0,
        [BoundInst | BoundInsts], S, !U) :-
    BoundInst = bound_functor(ConsId, Args),
    Indent1 = Indent0 + 1,
    Indent2 = Indent1 + 1,
    (
        Args = [],
        mercury_format_tabs(Indent1, S, !U),
        mercury_format_cons_id(Lang, needs_brackets, ConsId, S, !U),
        add_string("\n", S, !U)
    ;
        Args = [_ | _],
        mercury_format_tabs(Indent1, S, !U),
        mercury_format_cons_id(Lang, does_not_need_brackets, ConsId, S, !U),
        add_string("(\n", S, !U),
        mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr,
            Indent2, Args, S, !U),
        mercury_format_tabs(Indent1, S, !U),
        add_string(")\n", S, !U)
    ),
    (
        BoundInsts = []
    ;
        BoundInsts = [_ | _],
        mercury_format_tabs(Indent0, S, !U),
        add_string(";\n", S, !U),
        mercury_format_structured_bound_insts(Lang, InstVarSet,
            InclAddr, Indent0, BoundInsts, S, !U)
    ).

:- pred get_inst_addr(mer_inst::in, int::out) is det.

:- pragma foreign_proc("C",
    get_inst_addr(Inst::in, InstAddr::out),
    [will_not_call_mercury, promise_pure],
"
    InstAddr = Inst;
").

get_inst_addr(_, -1).

%---------------------------------------------------------------------------%

mercury_output_structured_unify_mode_list(Stream, Lang, InstVarSet, InclAddr,
        Indent, Insts, !IO) :-
    mercury_format_structured_unify_mode_list(Lang, InstVarSet, InclAddr,
        Indent, 1, Insts, Stream, !IO).

mercury_structured_unify_mode_list_to_string(Lang, InstVarSet, InclAddr,
        Indent, Insts) = String :-
    mercury_format_structured_unify_mode_list(Lang, InstVarSet, InclAddr,
        Indent, 1, Insts, unit, "", String).

:- pred mercury_format_structured_unify_mode_list(output_lang::in,
    inst_varset::in, incl_addr::in, int::in, int::in, list(unify_mode)::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_structured_unify_mode_list(_, _, _, _, _, [], _, !U).
mercury_format_structured_unify_mode_list(Lang, InstVarSet, InclAddr, Indent,
        ArgNum, [UnifyMode | UnifyModes], S, !U) :-
    mercury_format_tabs(Indent, S, !U),
    add_string("argument ", S, !U),
    add_int(ArgNum, S, !U),
    add_string(":\n", S, !U),
    mercury_format_structured_unify_mode(Lang, InstVarSet, InclAddr,
        Indent, UnifyMode, S, !U),
    mercury_format_structured_unify_mode_list(Lang, InstVarSet, InclAddr,
        Indent, ArgNum + 1, UnifyModes, S, !U).

%---------------------------------------------------------------------------%

mercury_output_structured_unify_mode(Stream, Lang, InstVarSet, InclAddr,
        Indent, Inst, !IO) :-
    mercury_format_structured_unify_mode(Lang, InstVarSet, InclAddr, Indent,
        Inst, Stream, !IO).

mercury_structured_unify_mode_to_string(Lang, InstVarSet, InclAddr,
        Indent, Inst) = String :-
    mercury_format_structured_unify_mode(Lang, InstVarSet, InclAddr, Indent,
        Inst, unit, "", String).

:- pred mercury_format_structured_unify_mode(output_lang::in, inst_varset::in,
    incl_addr::in, int::in, unify_mode::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_structured_unify_mode(Lang, InstVarSet, InclAddr, Indent,
        UnifyMode, S, !U) :-
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInit, LHSFinal, RHSInit, RHSFinal),
    get_inst_addr(LHSInit, LHSInitAddr),
    get_inst_addr(RHSInit, RHSInitAddr),
    get_inst_addr(LHSFinal, LHSFinalAddr),
    get_inst_addr(RHSFinal, RHSFinalAddr),

    mercury_format_tabs(Indent, S, !U),
    add_string("old lhs inst:\n", S, !U),
    mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
        "\n", LHSInit, S, !U),

    mercury_format_tabs(Indent, S, !U),
    ( if RHSInitAddr = LHSInitAddr then
        % We have printed the old lhs inst.
        add_string("old rhs inst: same as old lhs inst\n", S, !U)
    else
        add_string("old rhs inst:\n", S, !U),
        mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
            "\n", RHSInit, S, !U)
    ),

    mercury_format_tabs(Indent, S, !U),
    ( if LHSFinalAddr = LHSInitAddr then
        % We have printed the old lhs inst.
        add_string("new lhs inst: unchanged\n", S, !U)
    else if LHSFinalAddr = RHSInitAddr then
        % We have printed or described the old rhs inst.
        add_string("new lhs inst: changed to old rhs inst\n", S, !U)
    else
        add_string("new lhs inst:\n", S, !U),
        mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
            "\n", LHSFinal, S, !U)
    ),

    mercury_format_tabs(Indent, S, !U),
    ( if RHSFinalAddr = RHSInitAddr then
        % We have printed or described the old rhs inst.
        add_string("new rhs inst: unchanged\n", S, !U)
    else if RHSFinalAddr = LHSFinalAddr then
        % We have printed or described the new lhs inst.
        add_string("new rhs inst: changed to new lhs inst\n", S, !U)
    else
        add_string("new rhs inst:\n", S, !U),
        mercury_format_structured_inst(Lang, InstVarSet, InclAddr, Indent,
            "\n", RHSFinal, S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_structured_inst_name(output_lang::in, inst_varset::in,
    incl_addr::in, int::in, bool::in, string::in, inst_name::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr, Indent,
        FirstIndentPrinted, Suffix, InstName, S, !U) :-
    (
        FirstIndentPrinted = no,
        mercury_format_tabs(Indent, S, !U)
    ;
        FirstIndentPrinted = yes
    ),
    (
        InstName = user_inst(Name, Args),
        (
            Args = [],
            mercury_format_bracketed_sym_name(Name, S, !U)
        ;
            Args = [_ | _],
            mercury_format_sym_name(Name, S, !U),
            add_string("(\n", S, !U),
            mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr,
                Indent + 1, Args, S, !U),
            mercury_format_tabs(Indent, S, !U),
            add_string(")", S, !U)
        ),
        add_string(Suffix, S, !U)
    ;
        InstName = unify_inst(IsLive, Real, InstA, InstB),
        add_string("$unify(", S, !U),
        mercury_format_is_live_comma(IsLive, S, !U),
        mercury_format_real_comma(Real, S, !U),
        add_string("\n", S, !U),
        mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr,
            Indent + 1, [InstA, InstB], S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = merge_inst(InstA, InstB),
        add_string("$merge_inst(\n", S, !U),
        mercury_format_structured_inst_list(Lang, InstVarSet, InclAddr,
            Indent + 1, [InstA, InstB], S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = ground_inst(SubInstName, Uniq, IsLive, Real),
        add_string("$ground(", S, !U),
        mercury_format_is_live_comma(IsLive, S, !U),
        mercury_format_real_comma(Real, S, !U),
        mercury_format_uniqueness(Uniq, "shared", S, !U),
        add_string(",\n", S, !U),
        mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr,
            Indent + 1, no, "\n", SubInstName, S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = any_inst(SubInstName, Uniq, IsLive, Real),
        add_string("$any(", S, !U),
        mercury_format_is_live_comma(IsLive, S, !U),
        mercury_format_real_comma(Real, S, !U),
        mercury_format_uniqueness(Uniq, "shared", S, !U),
        add_string(",\n", S, !U),
        mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr,
            Indent + 1, no, "\n", SubInstName, S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = shared_inst(SubInstName),
        add_string("$shared_inst(\n", S, !U),
        mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr,
            Indent + 1, no, "\n", SubInstName, S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        add_string("$mostly_uniq_inst(\n", S, !U),
        mercury_format_structured_inst_name( Lang, InstVarSet, InclAddr,
            Indent + 1, no, "\n", SubInstName, S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = typed_ground(Uniqueness, Type),
        add_string("$typed_ground(", S, !U),
        mercury_format_uniqueness(Uniqueness, "shared", S, !U),
        add_string(", ", S, !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, print_name_only, Type, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ;
        InstName = typed_inst(Type, SubInstName),
        add_string("$typed_inst(", S, !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, print_name_only, Type, S, !U),
        add_string(",\n", S, !U),
        mercury_format_structured_inst_name(Lang, InstVarSet, InclAddr,
            Indent + 1, no, "\n", SubInstName, S, !U),
        mercury_format_tabs(Indent, S, !U),
        add_string(")", S, !U),
        add_string(Suffix, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_unify_mode_list(Stream, InstVarSet, UnifyModes, !IO) :-
    mercury_format_unify_mode_list(InstVarSet, UnifyModes, Stream, !IO).

mercury_unify_mode_list_to_string(InstVarSet, UnifyModes) = String :-
    mercury_format_unify_mode_list(InstVarSet, UnifyModes, unit, "", String).

:- pred mercury_format_unify_mode_list(inst_varset::in, list(unify_mode)::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_unify_mode_list(_InstVarSet, [], _S, !U).
mercury_format_unify_mode_list(InstVarSet, [Mode | Modes], S, !U) :-
    mercury_format_unify_mode(InstVarSet, Mode, S, !U),
    (
        Modes = []
    ;
        Modes = [_ | _],
        add_string(", ", S, !U),
        mercury_format_unify_mode_list(InstVarSet, Modes, S, !U)
    ).

mercury_output_unify_mode(Stream, InstVarSet, UnifyMode, !IO) :-
    mercury_format_unify_mode(InstVarSet, UnifyMode, Stream, !IO).

mercury_unify_mode_to_string(InstVarSet, UnifyMode) = String :-
    mercury_format_unify_mode(InstVarSet, UnifyMode, unit, "", String).

:- pred mercury_format_unify_mode(inst_varset::in, unify_mode::in,
    S::in, U::di, U::uo) is det <= output(S, U).

mercury_format_unify_mode(InstVarSet, UnifyMode, S, !U) :-
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInit, LHSFinal, RHSInit, RHSFinal),
    mercury_format_mode(output_debug, InstVarSet,
        from_to_mode(LHSInit, LHSFinal), S, !U),
    add_string(" = ", S, !U),
    mercury_format_mode(output_debug, InstVarSet,
        from_to_mode(RHSInit, RHSFinal), S, !U).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_mode.
%---------------------------------------------------------------------------%
