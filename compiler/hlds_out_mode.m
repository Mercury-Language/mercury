%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_out_mode.m.
% Main authors: conway, fjh.
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_mode.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred write_instmap(prog_varset::in, var_name_print::in, int::in,
    instmap::in, io::di, io::uo) is det.

:- pred write_var_inst_list(prog_varset::in, var_name_print::in, int::in,
    assoc_list(prog_var, mer_inst)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type incl_addr
    --->    do_not_incl_addr
    ;       do_incl_addr.

    % Output a list of insts in a format that makes them easy to read
    % but may not be valid Mercury.
    %
:- pred mercury_output_structured_inst_list(list(mer_inst)::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_inst_list_to_string(list(mer_inst), int,
    output_lang, incl_addr, inst_varset) = string.

    % Output an inst in a format that makes it easy to read
    % but may not be valid Mercury.
    % The `int' argument specifies the indentation level.
    % (These routines are used with `--debug-modes'.)
    %
:- pred mercury_output_structured_inst(mer_inst::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_inst_to_string(mer_inst, int,
    output_lang, incl_addr, inst_varset) = string.

%-----------------------------------------------------------------------------%

:- pred mercury_output_structured_unify_mode_list(list(unify_mode)::in,
    int::in, output_lang::in, incl_addr::in, inst_varset::in,
    io::di, io::uo) is det.
:- func mercury_structured_unify_mode_list_to_string(list(unify_mode),
    int, output_lang, incl_addr, inst_varset) = string.

:- pred mercury_output_structured_unify_mode(unify_mode::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_unify_mode_to_string(unify_mode, int,
    output_lang, incl_addr, inst_varset) = string.

%-----------------------------------------------------------------------------%

:- pred mercury_output_unify_mode_list(list(unify_mode)::in, inst_varset::in,
    io::di, io::uo) is det.
:- func mercury_unify_mode_list_to_string(list(unify_mode), inst_varset)
    = string.

:- pred mercury_output_unify_mode(unify_mode::in, inst_varset::in,
    io::di, io::uo) is det.
:- func mercury_unify_mode_to_string(unify_mode, inst_varset) = string.

:- implementation.

:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.

:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

write_instmap(VarSet, VarNamePrint, Indent, InstMap, !IO) :-
    ( if instmap_is_unreachable(InstMap) then
        io.write_string("unreachable", !IO)
    else
        instmap_to_assoc_list(InstMap, AssocList),
        write_var_inst_list(VarSet, VarNamePrint, Indent, AssocList, !IO)
    ).

write_var_inst_list(_, _, _, [], !IO).
write_var_inst_list(VarSet, VarNamePrint, Indent, [Var - Inst | VarsInsts],
        !IO) :-
    mercury_output_var(VarSet, VarNamePrint, Var, !IO),
    io.write_string(" -> ", !IO),
    varset.init(InstVarSet),
    mercury_output_inst(output_debug, InstVarSet, Inst, !IO),
    (
        VarsInsts = []
    ;
        VarsInsts = [_ | _],
        mercury_output_newline(Indent, !IO),
        io.write_string("%            ", !IO),
        write_var_inst_list(VarSet, VarNamePrint, Indent, VarsInsts, !IO)
    ).

%-----------------------------------------------------------------------------%

mercury_output_structured_inst_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, !IO) :-
    mercury_format_structured_inst_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, !IO).

mercury_structured_inst_list_to_string(Insts, Indent, Lang, InclAddr,
        InstVarSet) = String :-
    mercury_format_structured_inst_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_inst_list(list(mer_inst)::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_inst_list([], _, _, _, _, !U).
mercury_format_structured_inst_list([Inst | Insts], Indent, Lang, InclAddr,
        InstVarSet, !U) :-
    (
        Insts = [],
        Suffix = "\n"
    ;
        Insts = [_ | _],
        Suffix = ",\n"
    ),
    mercury_format_structured_inst(Suffix, Inst, Indent, Lang, InclAddr,
        InstVarSet, !U),
    mercury_format_structured_inst_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, !U).

%-----------------------------------------------------------------------------%

mercury_output_structured_inst(Inst, Indent, Lang, InclAddr, InstVarSet, !U) :-
    mercury_format_structured_inst("\n", Inst, Indent, Lang, InclAddr,
        InstVarSet, !U).

mercury_structured_inst_to_string(Inst, Indent, Lang, InclAddr, InstVarSet)
        = String :-
    mercury_format_structured_inst("\n", Inst, Indent, Lang, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_inst(string::in, mer_inst::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_inst(Suffix, Inst, Indent, Lang, InclAddr,
        InstVarSet, !U) :-
    mercury_format_tabs(Indent, !U),
    (
        InclAddr = do_not_incl_addr
    ;
        InclAddr = do_incl_addr,
        get_inst_addr(Inst, InstAddr),
        InstAddrStr = string.format("%x", [i(InstAddr)]),
        add_string(InstAddrStr, !U),
        add_string(": ", !U)
    ),
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_any_pred_inst_info(output_debug, InstVarSet,
                Uniq, PredInstInfo, !U)
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, !U)
        ),
        add_string(Suffix, !U)
    ;
        Inst = free,
        add_string("free", !U),
        add_string(Suffix, !U)
    ;
        Inst = free(_T),
        add_string("free(with some type)", !U),
        add_string(Suffix, !U)
    ;
        Inst = bound(Uniq, InstResults, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", !U),
        add_string("(\n", !U),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            InstResultsTerm =
                inst_test_results_to_term(term.context_init, InstResults),
            InstResultsStr = mercury_term_to_string(varset.init,
                print_num_only, InstResultsTerm),
            mercury_format_tabs(Indent + 1, !U),
            add_string(InstResultsStr, !U),
            add_string(",\n", !U)
        ),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            mercury_format_tabs(Indent + 1, !U),
            add_string("[\n", !U)
        ),
        mercury_format_structured_bound_insts(BoundInsts, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            mercury_format_tabs(Indent + 1, !U),
            add_string("]\n", !U)
        ),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_ground_pred_inst_info(output_debug, InstVarSet,
                Uniq, PredInstInfo, !U)
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", !U)
        ),
        add_string(Suffix, !U)
    ;
        Inst = inst_var(Var),
        mercury_format_var(InstVarSet, print_name_only, Var, !U),
        add_string(Suffix, !U)
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        mercury_format_constrained_inst_vars(output_debug, InstVarSet,
            Vars, ConstrainedInst, !U),
        add_string(Suffix, !U)
    ;
        Inst = abstract_inst(Name, Args),
        mercury_format_structured_inst_name(Suffix, user_inst(Name, Args),
            yes, Indent, Lang, InclAddr, InstVarSet, !U)
    ;
        Inst = defined_inst(InstName),
        mercury_format_structured_inst_name(Suffix, InstName,
            yes, Indent, Lang, InclAddr, InstVarSet, !U)
    ;
        Inst = not_reached,
        add_string("not_reached", !U),
        add_string(Suffix, !U)
    ).

:- pred mercury_format_structured_bound_insts(list(bound_inst)::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_bound_insts([], _, _, _, _, !U).
mercury_format_structured_bound_insts([BoundInst | BoundInsts],
        Indent0, Lang, InclAddr, InstVarSet, !U) :-
    BoundInst = bound_functor(ConsId, Args),
    Indent1 = Indent0 + 1,
    Indent2 = Indent1 + 1,
    (
        Args = [],
        mercury_format_tabs(Indent1, !U),
        mercury_format_cons_id(needs_brackets, ConsId, !U),
        add_string("\n", !U)
    ;
        Args = [_ | _],
        mercury_format_tabs(Indent1, !U),
        mercury_format_cons_id(does_not_need_brackets, ConsId, !U),
        add_string("(\n", !U),
        mercury_format_structured_inst_list(Args, Indent2,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent1, !U),
        add_string(")\n", !U)
    ),
    (
        BoundInsts = []
    ;
        BoundInsts = [_ | _],
        mercury_format_tabs(Indent0, !U),
        add_string(";\n", !U),
        mercury_format_structured_bound_insts(BoundInsts, Indent0,
            Lang, InclAddr, InstVarSet, !U)
    ).

:- pred get_inst_addr(mer_inst::in, int::out) is det.

:- pragma foreign_proc("C",
    get_inst_addr(Inst::in, InstAddr::out),
    [will_not_call_mercury, promise_pure],
"
    InstAddr = Inst;
").

get_inst_addr(_, -1).

%-----------------------------------------------------------------------------%

mercury_output_structured_unify_mode_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, !IO) :-
    mercury_format_structured_unify_mode_list(Insts, 1, Indent, Lang, InclAddr,
        InstVarSet, !IO).

mercury_structured_unify_mode_list_to_string(Insts, Indent, Lang, InclAddr,
        InstVarSet) = String :-
    mercury_format_structured_unify_mode_list(Insts, 1, Indent, Lang, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_unify_mode_list(list(unify_mode)::in,
    int::in, int::in, output_lang::in, incl_addr::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_structured_unify_mode_list([], _, _, _, _, _, !U).
mercury_format_structured_unify_mode_list([UnifyMode | UnifyModes], ArgNum,
        Indent, Lang, InclAddr, InstVarSet, !U) :-
    mercury_format_tabs(Indent, !U),
    add_string("argument ", !U),
    add_int(ArgNum, !U),
    add_string(":\n", !U),
    mercury_format_structured_unify_mode(UnifyMode, Indent,
        Lang, InclAddr, InstVarSet, !U),
    mercury_format_structured_unify_mode_list(UnifyModes, ArgNum +1, Indent,
        Lang, InclAddr, InstVarSet, !U).

%-----------------------------------------------------------------------------%

mercury_output_structured_unify_mode(Inst, Indent, Lang, InclAddr,
        InstVarSet, !IO) :-
    mercury_format_structured_unify_mode(Inst, Indent, Lang, InclAddr,
        InstVarSet, !IO).

mercury_structured_unify_mode_to_string(Inst, Indent, Lang, InclAddr,
        InstVarSet) = String :-
    mercury_format_structured_unify_mode(Inst, Indent, Lang, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_unify_mode(unify_mode::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_unify_mode(UnifyMode, Indent, Lang, InclAddr,
        InstVarSet, !U) :-
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInit, LHSFinal, RHSInit, RHSFinal),
    get_inst_addr(LHSInit, LHSInitAddr),
    get_inst_addr(RHSInit, RHSInitAddr),
    get_inst_addr(LHSFinal, LHSFinalAddr),
    get_inst_addr(RHSFinal, RHSFinalAddr),

    mercury_format_tabs(Indent, !U),
    add_string("old lhs inst:\n", !U),
    mercury_format_structured_inst("\n", LHSInit, Indent, Lang, InclAddr,
        InstVarSet, !U),

    mercury_format_tabs(Indent, !U),
    ( if RHSInitAddr = LHSInitAddr then
        % We have printed the old lhs inst.
        add_string("old rhs inst: same as old lhs inst\n", !U)
    else
        add_string("old rhs inst:\n", !U),
        mercury_format_structured_inst("\n", RHSInit, Indent, Lang, InclAddr,
            InstVarSet, !U)
    ),

    mercury_format_tabs(Indent, !U),
    ( if LHSFinalAddr = LHSInitAddr then
        % We have printed the old lhs inst.
        add_string("new lhs inst: unchanged\n", !U)
    else if LHSFinalAddr = RHSInitAddr then
        % We have printed or described the old rhs inst.
        add_string("new lhs inst: changed to old rhs inst\n", !U)
    else
        add_string("new lhs inst:\n", !U),
        mercury_format_structured_inst("\n", LHSFinal, Indent, Lang, InclAddr,
            InstVarSet, !U)
    ),

    mercury_format_tabs(Indent, !U),
    ( if RHSFinalAddr = RHSInitAddr then
        % We have printed or described the old rhs inst.
        add_string("new rhs inst: unchanged\n", !U)
    else if RHSFinalAddr = LHSFinalAddr then
        % We have printed or described the new lhs inst.
        add_string("new rhs inst: changed to new lhs inst\n", !U)
    else
        add_string("new rhs inst:\n", !U),
        mercury_format_structured_inst("\n", RHSFinal, Indent, Lang, InclAddr,
            InstVarSet, !U)
    ).

%-----------------------------------------------------------------------------%

:- pred mercury_format_structured_inst_name(string::in, inst_name::in,
    bool::in, int::in, output_lang::in, incl_addr::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_structured_inst_name(Suffix, InstName, FirstIndentPrinted,
        Indent, Lang, InclAddr, InstVarSet, !U) :-
    (
        FirstIndentPrinted = no,
        mercury_format_tabs(Indent, !U)
    ;
        FirstIndentPrinted = yes
    ),
    (
        InstName = user_inst(Name, Args),
        (
            Args = [],
            mercury_format_bracketed_sym_name(Name, !U)
        ;
            Args = [_ | _],
            mercury_format_sym_name(Name, !U),
            add_string("(\n", !U),
            mercury_format_structured_inst_list(Args, Indent + 1,
                Lang, InclAddr, InstVarSet, !U),
            mercury_format_tabs(Indent, !U),
            add_string(")", !U)
        ),
        add_string(Suffix, !U)
    ;
        InstName = unify_inst(IsLive, Real, InstA, InstB),
        add_string("$unify(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        add_string("\n", !U),
        mercury_format_structured_inst_list([InstA, InstB], Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = merge_inst(InstA, InstB),
        add_string("$merge_inst(\n", !U),
        mercury_format_structured_inst_list([InstA, InstB], Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = ground_inst(SubInstName, Uniq, IsLive, Real),
        add_string("$ground(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        mercury_format_uniqueness(Uniq, "shared", !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name("\n", SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = any_inst(SubInstName, Uniq, IsLive, Real),
        add_string("$any(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        mercury_format_uniqueness(Uniq, "shared", !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name("\n", SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = shared_inst(SubInstName),
        add_string("$shared_inst(\n", !U),
        mercury_format_structured_inst_name("\n", SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        add_string("$mostly_uniq_inst(\n", !U),
        mercury_format_structured_inst_name("\n", SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = typed_ground(Uniqueness, Type),
        add_string("$typed_ground(", !U),
        mercury_format_uniqueness(Uniqueness, "shared", !U),
        add_string(", ", !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, print_name_only, Type, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ;
        InstName = typed_inst(Type, SubInstName),
        add_string("$typed_inst(", !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, print_name_only, Type, !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name("\n", SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")", !U),
        add_string(Suffix, !U)
    ).

%-----------------------------------------------------------------------------%

mercury_output_unify_mode_list(UnifyModes, InstVarSet, !IO) :-
    mercury_format_unify_mode_list(UnifyModes, InstVarSet, !IO).

mercury_unify_mode_list_to_string(UnifyModes, InstVarSet) = String :-
    mercury_format_unify_mode_list(UnifyModes, InstVarSet, "", String).

:- pred mercury_format_unify_mode_list(list(unify_mode)::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_unify_mode_list([], _InstVarSet, !IO).
mercury_format_unify_mode_list([Mode | Modes], InstVarSet, !IO) :-
    mercury_format_unify_mode(Mode, InstVarSet, !IO),
    (
        Modes = [],
        true
    ;
        Modes = [_ | _],
        add_string(", ", !IO),
        mercury_format_unify_mode_list(Modes, InstVarSet, !IO)
    ).

mercury_output_unify_mode(UnifyMode, InstVarSet, !IO) :-
    mercury_format_unify_mode(UnifyMode, InstVarSet, !IO).

mercury_unify_mode_to_string(UnifyMode, InstVarSet) = String :-
    mercury_format_unify_mode(UnifyMode, InstVarSet, "", String).

:- pred mercury_format_unify_mode(unify_mode::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_unify_mode(UnifyMode, InstVarSet, !IO) :-
    UnifyMode = unify_modes_li_lf_ri_rf(LHSInit, LHSFinal, RHSInit, RHSFinal),
    mercury_format_mode(output_debug, InstVarSet,
        from_to_mode(LHSInit, LHSFinal), !IO),
    add_string(" = ", !IO),
    mercury_format_mode(output_debug, InstVarSet,
        from_to_mode(RHSInit, RHSFinal), !IO).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_mode.
%-----------------------------------------------------------------------------%
