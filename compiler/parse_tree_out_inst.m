%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module converts the parse tree structure representations
% of insts and modes back into Mercury source text.
%
% However, since mer_insts can represent not just insts that can occur
% in source code, but also insts created by the compiler, it must also
% be able to output the latter kind of inst as well. For these, we cannot
% print them as valid Mercury.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_inst.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred mercury_output_inst_list(output_lang::in, inst_varset::in,
    list(mer_inst)::in, io::di, io::uo) is det.
:- func mercury_inst_list_to_string(output_lang, inst_varset, list(mer_inst))
    = string.

%-----------------------------------------------------------------------------%

:- pred mercury_output_inst(output_lang::in, inst_varset::in, mer_inst::in,
    io::di, io::uo) is det.
:- func mercury_inst_to_string(output_lang, inst_varset, mer_inst) = string.
:- pred mercury_format_inst(output_lang::in, InstInfo::in, mer_inst::in,
    U::di, U::uo) is det <= (output(U), inst_info(InstInfo)).

%-----------------------------------------------------------------------------%

:- pred mercury_format_inst_name(output_lang::in, InstInfo::in, inst_name::in,
    U::di, U::uo) is det <= (output(U), inst_info(InstInfo)).

%-----------------------------------------------------------------------------%

:- pred mercury_format_constrained_inst_vars(output_lang::in, InstInfo::in,
    set(inst_var)::in, mer_inst::in, U::di, U::uo) is det
    <= (output(U), inst_info(InstInfo)).

%-----------------------------------------------------------------------------%

:- pred mercury_format_ground_pred_inst_info(output_lang::in, inst_varset::in,
    uniqueness::in, pred_inst_info::in, U::di, U::uo) is det <= output(U).

:- pred mercury_format_any_pred_inst_info(output_lang::in, inst_varset::in,
    uniqueness::in, pred_inst_info::in, U::di, U::uo) is det <= output(U).

%-----------------------------------------------------------------------------%

:- pred mercury_format_is_live_comma(is_live::in, U::di, U::uo) is det
    <= output(U).

:- pred mercury_format_real_comma(unify_is_real::in, U::di, U::uo) is det
    <= output(U).

:- pred mercury_format_uniqueness(uniqueness::in, string::in,
    U::di, U::uo) is det <= output(U).

:- pred mercury_format_any_uniqueness(uniqueness::in,
    U::di, U::uo) is det <= output(U).

%-----------------------------------------------------------------------------%

:- pred mercury_output_mode(output_lang::in, inst_varset::in, mer_mode::in,
    io::di, io::uo) is det.
:- func mercury_mode_to_string(output_lang, inst_varset, mer_mode) = string.
:- pred mercury_format_mode(output_lang::in, InstInfo::in, mer_mode::in,
    U::di, U::uo) is det <= (output(U), inst_info(InstInfo)).

:- pred mercury_output_mode_list(output_lang::in, inst_varset::in,
    list(mer_mode)::in, io::di, io::uo) is det.
:- func mercury_mode_list_to_string(output_lang, inst_varset, list(mer_mode))
    = string.
:- pred mercury_format_mode_list(output_lang::in, InstInfo::in,
    list(mer_mode)::in, U::di, U::uo) is det
    <= (output(U), inst_info(InstInfo)).

%-----------------------------------------------------------------------------%

% We use the following type class to share code between mercury_output_inst,
% which outputs inst in Mercury syntax, and mercury_output_expanded_inst,
% which is the same except that it expands any compiler-defined insts
% (except those which have already been encountered).
%
% (XXX Perhaps we should use the same sort of technique to also avoid
% code duplication with mercury_format_structured_inst.)

:- typeclass inst_info(InstInfo) where [
    (func instvarset(InstInfo) = inst_varset),
    (pred format_defined_inst(output_lang::in, InstInfo::in, inst_name::in,
        U::di, U::uo) is det <= output(U))
].

:- type simple_inst_info
    --->    simple_inst_info(sii_varset :: inst_varset).

:- instance inst_info(simple_inst_info).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module char.
:- import_module ops.
:- import_module require.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- instance inst_info(simple_inst_info) where [
    func(instvarset/1) is sii_varset,
    pred(format_defined_inst/5) is mercury_format_inst_name
].

%-----------------------------------------------------------------------------%

mercury_output_inst_list(Lang, InstVarSet, Insts, !IO) :-
    mercury_format_inst_list(Lang, simple_inst_info(InstVarSet), Insts, !IO).

mercury_inst_list_to_string(Lang, VarSet, Insts) = String :-
    mercury_format_inst_list(Lang, simple_inst_info(VarSet), Insts,
        "", String).

:- pred mercury_format_inst_list(output_lang::in, InstInfo::in,
    list(mer_inst)::in, U::di, U::uo) is det
    <= (output(U), inst_info(InstInfo)).

mercury_format_inst_list(_, _, [], !U).
mercury_format_inst_list(Lang, Info, [Inst | Insts], !U) :-
    mercury_format_inst(Lang, Info, Inst, !U),
    (
        Insts = []
    ;
        Insts = [_ | _],
        add_string(", ", !U),
        mercury_format_inst_list(Lang, Info, Insts, !U)
    ).

%-----------------------------------------------------------------------------%

mercury_output_inst(Lang, VarSet, Inst, !IO) :-
    mercury_format_inst(Lang, simple_inst_info(VarSet), Inst, !IO).

mercury_inst_to_string(Lang, VarSet, Inst) = String :-
    mercury_format_inst(Lang, simple_inst_info(VarSet), Inst, "", String).

mercury_format_inst(Lang, InstInfo, Inst, !U) :-
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_any_pred_inst_info(Lang, instvarset(InstInfo),
                Uniq, PredInstInfo, !U)
        ;
            HOInstInfo = none,
            mercury_format_any_uniqueness(Uniq, !U)
        )
    ;
        Inst = free,
        add_string("free", !U)
    ;
        Inst = free(_T),
        add_string("free(with some type)", !U)
    ;
        Inst = bound(Uniq, _, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", !U),
        add_string("(", !U),
        mercury_format_bound_insts(Lang, InstInfo, BoundInsts, !U),
        add_string(")", !U)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_ground_pred_inst_info(Lang, instvarset(InstInfo),
                Uniq, PredInstInfo, !U)
        ;
            HOInstInfo = none,
            mercury_format_uniqueness(Uniq, "ground", !U)
        )
    ;
        Inst = inst_var(Var),
        mercury_format_var(InstInfo ^ instvarset, print_name_only, Var, !U)
    ;
        Inst = constrained_inst_vars(Vars, CInst),
        mercury_format_constrained_inst_vars(Lang, InstInfo, Vars, CInst, !U)
    ;
        Inst = abstract_inst(Name, Args),
        mercury_format_inst_name(Lang, InstInfo, user_inst(Name, Args), !U)
    ;
        Inst = defined_inst(InstName),
        format_defined_inst(Lang, InstInfo, InstName, !U)
    ;
        Inst = not_reached,
        add_string("not_reached", !U)
    ).

:- pred mercury_format_bound_insts(output_lang::in,  InstInfo::in,
    list(bound_inst)::in, U::di, U::uo) is det
    <= (output(U), inst_info(InstInfo)).

mercury_format_bound_insts(_, _, [], !U).
mercury_format_bound_insts(Lang, InstInfo, [BoundInst | BoundInsts], !U) :-
    BoundInst = bound_functor(ConsId, Args),
    (
        Args = [],
        mercury_format_cons_id(needs_brackets, ConsId, !U)
    ;
        Args = [_ | _],
        mercury_format_cons_id(does_not_need_brackets, ConsId, !U),
        add_string("(", !U),
        mercury_format_inst_list(Lang, InstInfo, Args, !U),
        add_string(")", !U)
    ),
    (
        BoundInsts = []
    ;
        BoundInsts = [_ | _],
        add_string(" ; ", !U),
        mercury_format_bound_insts(Lang, InstInfo, BoundInsts, !U)
    ).

%-----------------------------------------------------------------------------%

mercury_format_inst_name(Lang, InstInfo, InstName, !U) :-
    (
        InstName = user_inst(Name, Args),
        (
            Args = [],
            mercury_format_bracketed_sym_name(Name, !U)
        ;
            Args = [_ | _],
            mercury_format_sym_name(Name, !U),
            add_string("(", !U),
            mercury_format_inst_list(Lang, InstInfo, Args, !U),
            add_string(")", !U)
        )
    ;
        InstName = unify_inst(IsLive, Real, InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "unify_inst")
        ;
            Lang = output_debug,
            add_string("$unify(", !U),
            mercury_format_is_live_comma(IsLive, !U),
            mercury_format_comma_real(Real, !U),
            mercury_format_inst_list(Lang, InstInfo, [InstA, InstB], !U),
            add_string(")", !U)
        )
    ;
        InstName = merge_inst(InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "merge_inst")
        ;
            Lang = output_debug,
            add_string("$merge_inst(", !U),
            mercury_format_inst_list(Lang, InstInfo, [InstA, InstB], !U),
            add_string(")", !U)
        )
    ;
        InstName = ground_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "ground_inst")
        ;
            Lang = output_debug,
            add_string("$ground(", !U),
            mercury_format_inst_name(Lang, InstInfo, SubInstName, !U),
            add_string(", ", !U),
            mercury_format_is_live_comma(IsLive, !U),
            mercury_format_uniqueness(Uniq, "shared", !U),
            mercury_format_comma_real(Real, !U),
            add_string(")", !U)
        )
    ;
        InstName = any_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "any_inst")
        ;
            Lang = output_debug,
            add_string("$any(", !U),
            mercury_format_inst_name(Lang, InstInfo, SubInstName, !U),
            add_string(", ", !U),
            mercury_format_is_live_comma(IsLive, !U),
            mercury_format_uniqueness(Uniq, "shared", !U),
            mercury_format_comma_real(Real, !U),
            add_string(")", !U)
        )
    ;
        InstName = shared_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "shared_inst")
        ;
            Lang = output_debug,
            add_string("$shared_inst(", !U),
            mercury_format_inst_name(Lang, InstInfo, SubInstName, !U),
            add_string(")", !U)
        )
    ;
        InstName = mostly_uniq_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "mostly_uniq_inst")
        ;
            Lang = output_debug,
            add_string("$mostly_uniq_inst(", !U),
            mercury_format_inst_name(Lang, InstInfo, SubInstName, !U),
            add_string(")", !U)
        )
    ;
        InstName = typed_ground(Uniqueness, Type),
        (
            Lang = output_mercury,
            unexpected($module, $pred, "typed_ground")
        ;
            Lang = output_debug,
            add_string("$typed_ground(", !U),
            mercury_format_uniqueness(Uniqueness, "shared", !U),
            add_string(", ", !U),
            varset.init(TypeVarSet),
            mercury_format_type(TypeVarSet, print_name_only, Type, !U),
            add_string(")", !U)
        )
    ;
        InstName = typed_inst(Type, SubInstName),
        (
            Lang = output_mercury,
            mercury_format_inst_name(Lang, InstInfo, SubInstName, !U)
        ;
            Lang = output_debug,
            add_string("$typed_inst(", !U),
            varset.init(TypeVarSet),
            mercury_format_type(TypeVarSet, print_name_only, Type, !U),
            add_string(", ", !U),
            mercury_format_inst_name(Lang, InstInfo, SubInstName, !U),
            add_string(")", !U)
        )
    ).

%-----------------------------------------------------------------------------%

mercury_format_constrained_inst_vars(Lang, InstInfo, !.Vars, Inst, !U) :-
    ( if set.remove_least(Var, !Vars) then
        add_string("(", !U),
        mercury_format_var(InstInfo ^ instvarset, print_name_only, Var, !U),
        add_string(" =< ", !U),
        mercury_format_constrained_inst_vars(Lang, InstInfo, !.Vars, Inst, !U),
        add_string(")", !U)
    else
        mercury_format_inst(Lang, InstInfo, Inst, !U)
    ).

%-----------------------------------------------------------------------------%

mercury_format_ground_pred_inst_info(Lang, VarSet, Uniq, PredInstInfo, !U) :-
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
    (
        Uniq = shared
    ;
        ( Uniq = unique
        ; Uniq = mostly_unique
        ; Uniq = clobbered
        ; Uniq = mostly_clobbered
        ),
        add_string("/* ", !U),
        mercury_format_uniqueness(Uniq, "ground", !U),
        add_string(" */", !U)
    ),
    (
        PredOrFunc = pf_predicate,
        (
            Modes = [],
            add_string("((pred) is ", !U),
            mercury_format_det(Det, !U),
            add_string(")", !U)
        ;
            Modes = [_ | _],
            add_string("(pred(", !U),
            mercury_format_mode_list(Lang, simple_inst_info(VarSet),
                Modes, !U),
            add_string(") is ", !U),
            mercury_format_det(Det, !U),
            add_string(")", !U)
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        (
            ArgModes = [],
            add_string("((func) = ", !U)
        ;
            ArgModes = [_ | _],
            add_string("(func(", !U),
            mercury_format_mode_list(Lang, simple_inst_info(VarSet),
                ArgModes, !U),
            add_string(") = ", !U)
        ),
        mercury_format_mode(Lang, simple_inst_info(VarSet), RetMode, !U),
        add_string(" is ", !U),
        mercury_format_det(Det, !U),
        add_string(")", !U)
    ),
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        add_string(" /* arg regs: [", !U),
        mercury_format_arg_reg_list(ArgRegs, !U),
        add_string("] */", !U)
    ;
        MaybeArgRegs = arg_reg_types_unset
    ).

mercury_format_any_pred_inst_info(Lang, VarSet, Uniq, PredInstInfo, !U) :-
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
    (
        Uniq = shared
    ;
        ( Uniq = unique
        ; Uniq = mostly_unique
        ; Uniq = clobbered
        ; Uniq = mostly_clobbered
        ),
        add_string("/* ", !U),
        mercury_format_uniqueness(Uniq, "any", !U),
        add_string(" */", !U)
    ),
    (
        PredOrFunc = pf_predicate,
        (
            Modes = [],
            add_string("(any_pred is ", !U),
            mercury_format_det(Det, !U),
            add_string(")", !U)
        ;
            Modes = [_ | _],
            add_string("(any_pred(", !U),
            mercury_format_mode_list(Lang, simple_inst_info(VarSet), Modes,
                !U),
            add_string(") is ", !U),
            mercury_format_det(Det, !U),
            add_string(")", !U)
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        (
            Modes = [],
            add_string("(any_func = ", !U)
        ;
            Modes = [_ | _],
            add_string("(any_func(", !U),
            mercury_format_mode_list(Lang, simple_inst_info(VarSet),
                ArgModes, !U),
            add_string(") = ", !U)
        ),
        mercury_format_mode(Lang, simple_inst_info(VarSet), RetMode, !U),
        add_string(" is ", !U),
        mercury_format_det(Det, !U),
        add_string(")", !U)
    ),
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        add_string(" /* arg regs: [", !U),
        mercury_format_arg_reg_list(ArgRegs, !U),
        add_string("] */", !U)
    ;
        MaybeArgRegs = arg_reg_types_unset
    ).

:- pred mercury_format_arg_reg_list(list(ho_arg_reg)::in, U::di, U::uo) is det
    <= output(U).

mercury_format_arg_reg_list([], !U).
mercury_format_arg_reg_list([Reg | Regs], !U) :-
    (
        Reg = ho_arg_reg_r,
        add_string("reg_r", !U)
    ;
        Reg = ho_arg_reg_f,
        add_string("reg_f", !U)
    ),
    (
        Regs = []
    ;
        Regs = [_ | _],
        add_string(", ", !U),
        mercury_format_arg_reg_list(Regs, !U)
    ).

%-----------------------------------------------------------------------------%

mercury_format_is_live_comma(IsLive, !U) :-
    (
        IsLive = is_live,
        add_string("live, ", !U)
    ;
        IsLive = is_dead,
        add_string("dead, ", !U)
    ).

mercury_format_real_comma(Real, !U) :-
    (
        Real = real_unify,
        add_string("real, ", !U)
    ;
        Real = fake_unify,
        add_string("fake, ", !U)
    ).

:- pred mercury_format_comma_real(unify_is_real::in, U::di, U::uo) is det
    <= output(U).

mercury_format_comma_real(Real, !U) :-
    (
        Real = real_unify,
        add_string(", real", !U)
    ;
        Real = fake_unify,
        add_string(", fake", !U)
    ).

mercury_format_uniqueness(shared, SharedString, !U) :-
    add_string(SharedString, !U).
mercury_format_uniqueness(unique, _, !U) :-
    add_string("unique", !U).
mercury_format_uniqueness(mostly_unique, _, !U) :-
    add_string("mostly_unique", !U).
mercury_format_uniqueness(clobbered, _, !U) :-
    add_string("clobbered", !U).
mercury_format_uniqueness(mostly_clobbered, _, !U) :-
    add_string("mostly_clobbered", !U).

mercury_format_any_uniqueness(shared, !U) :-
    add_string("any", !U).
mercury_format_any_uniqueness(unique, !U) :-
    add_string("unique_any", !U).
mercury_format_any_uniqueness(mostly_unique, !U) :-
    add_string("mostly_unique_any", !U).
mercury_format_any_uniqueness(clobbered, !U) :-
    add_string("clobbered_any", !U).
mercury_format_any_uniqueness(mostly_clobbered, !U) :-
    add_string("mostly_clobbered_any", !U).

%-----------------------------------------------------------------------------%

mercury_output_mode_list(Lang, InstVarSet, Modes, !IO) :-
    mercury_format_mode_list(Lang, simple_inst_info(InstVarSet), Modes, !IO).

mercury_mode_list_to_string(Lang, InstVarSet, Modes) = String :-
    mercury_format_mode_list(Lang, simple_inst_info(InstVarSet), Modes,
        "", String).

mercury_format_mode_list(_, _, [], !U).
mercury_format_mode_list(Lang, InstInfo, [Mode | Modes], !U) :-
    mercury_format_mode(Lang, InstInfo, Mode, !U),
    (
        Modes = []
    ;
        Modes = [_ | _],
        add_string(", ", !U),
        mercury_format_mode_list(Lang, InstInfo, Modes, !U)
    ).

mercury_output_mode(Lang, InstVarSet, Mode, !IO) :-
    mercury_format_mode(Lang, simple_inst_info(InstVarSet), Mode, !IO).

mercury_mode_to_string(Lang, InstVarSet, Mode) = String :-
    mercury_format_mode(Lang, simple_inst_info(InstVarSet), Mode, "", String).

mercury_format_mode(Lang, InstInfo, Mode, !U) :-
    (
        Mode = (InstA -> InstB),
        % Output higher-order pred and func modes in a nice format.
        ( if
            InstA = ground(_Uniq, higher_order(
                pred_inst_info(_PredOrFunc, _Modes, _, _Det))),
            InstB = InstA
        then
            mercury_format_inst(Lang, InstInfo, InstA, !U)
        else
            add_string("(", !U),
            mercury_format_inst(Lang, InstInfo, InstA, !U),
            add_string(" >> ", !U),
            mercury_format_inst(Lang, InstInfo, InstB, !U),
            add_string(")", !U)
        )
    ;
        Mode = user_defined_mode(Name, Args),
        (
            Args = [],
            mercury_format_bracketed_sym_name(Name, !U)
        ;
            Args = [_ | _],
            mercury_format_sym_name(Name, !U),
            add_string("(", !U),
            mercury_format_inst_list(Lang, InstInfo, Args, !U),
            add_string(")", !U)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_inst.
%-----------------------------------------------------------------------------%
