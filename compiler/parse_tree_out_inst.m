%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the parse tree structure representations
% of insts and modes back into Mercury source text.
%
% However, since mer_insts can represent not just insts that can occur
% in source code, but also insts created by the compiler, it must also
% be able to output the latter kind of inst as well. For these, we cannot
% print them as valid Mercury.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_inst.
:- interface.

:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- func mercury_inst_list_to_string(output_lang, inst_varset, list(mer_inst))
    = string.
:- pred mercury_output_inst_list(io.text_output_stream::in, output_lang::in,
    inst_varset::in, list(mer_inst)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- func mercury_inst_to_string(output_lang, inst_varset, mer_inst) = string.
:- pred mercury_output_inst(io.text_output_stream::in, output_lang::in,
    inst_varset::in, mer_inst::in, io::di, io::uo) is det.
:- pred mercury_format_inst(output_lang::in, inst_varset::in, mer_inst::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_inst_name(output_lang::in, inst_varset::in,
    inst_name::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_constrained_inst_vars(output_lang::in, inst_varset::in,
    set(inst_var)::in, mer_inst::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_ground_pred_inst_info(output_lang::in, inst_varset::in,
    uniqueness::in, pred_inst_info::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_format_any_pred_inst_info(output_lang::in, inst_varset::in,
    uniqueness::in, pred_inst_info::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_is_live_comma(is_live::in, S::in, U::di, U::uo)
    is det <= pt_output(S, U).

:- pred mercury_format_real_comma(unify_is_real::in, S::in, U::di, U::uo)
    is det <= pt_output(S, U).

:- func mercury_uniqueness_to_string(uniqueness, string) = string.
:- pred mercury_format_uniqueness(uniqueness::in, string::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_any_uniqueness_to_string(uniqueness) = string.
:- pred mercury_format_any_uniqueness(uniqueness::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- func mercury_mode_list_to_string(output_lang, inst_varset, list(mer_mode))
    = string.
:- pred mercury_output_mode_list(io.text_output_stream::in, output_lang::in,
    inst_varset::in, list(mer_mode)::in, io::di, io::uo) is det.
:- pred mercury_format_mode_list(output_lang::in, inst_varset::in,
    list(mer_mode)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

:- func mercury_mode_to_string(output_lang, inst_varset, mer_mode) = string.
:- pred mercury_output_mode(io.text_output_stream::in, output_lang::in,
    inst_varset::in, mer_mode::in, io::di, io::uo) is det.
:- pred mercury_format_mode(output_lang::in, inst_varset::in, mer_mode::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module require.
:- import_module string.
:- import_module string.builder.
:- import_module varset.

%---------------------------------------------------------------------------%

mercury_inst_list_to_string(Lang, InstVarSet, Insts) = Str :-
    State0 = string.builder.init,
    mercury_format_inst_list(Lang, InstVarSet, Insts, string.builder.handle,
        State0, State),
    Str = string.builder.to_string(State).

mercury_output_inst_list(Stream, Lang, InstVarSet, Insts, !IO) :-
    mercury_format_inst_list(Lang, InstVarSet, Insts, Stream, !IO).

:- pred mercury_format_inst_list(output_lang::in, inst_varset::in,
    list(mer_inst)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_inst_list(_, _, [], _S, !U).
mercury_format_inst_list(Lang, InstVarSet, [Inst | Insts], S, !U) :-
    mercury_format_inst(Lang, InstVarSet, Inst, S, !U),
    (
        Insts = []
    ;
        Insts = [_ | _],
        add_string(", ", S, !U),
        mercury_format_inst_list(Lang, InstVarSet, Insts, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_inst_to_string(Lang, InstVarSet, Inst) = Str :-
    State0 = string.builder.init,
    mercury_format_inst(Lang, InstVarSet, Inst, string.builder.handle,
        State0, State),
    Str = string.builder.to_string(State).

mercury_output_inst(Stream, Lang, InstVarSet, Inst, !IO) :-
    mercury_format_inst(Lang, InstVarSet, Inst, Stream, !IO).

mercury_format_inst(Lang, InstVarSet, Inst, S, !U) :-
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_any_pred_inst_info(Lang, InstVarSet,
                Uniq, PredInstInfo, S, !U)
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_any_uniqueness(Uniq, S, !U)
        )
    ;
        Inst = free,
        add_string("free", S, !U)
    ;
        Inst = free(_T),
        add_string("free(with some type)", S, !U)
    ;
        Inst = bound(Uniq, _, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", S, !U),
        add_string("(", S, !U),
        mercury_format_bound_insts(Lang, InstVarSet, BoundInsts, S, !U),
        add_string(")", S, !U)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_ground_pred_inst_info(Lang, InstVarSet,
                Uniq, PredInstInfo, S, !U)
        ;
            HOInstInfo = none_or_default_func,
            mercury_format_uniqueness(Uniq, "ground", S, !U)
        )
    ;
        Inst = inst_var(Var),
        mercury_format_var_vs(InstVarSet, print_name_only, Var, S, !U)
    ;
        Inst = constrained_inst_vars(Vars, CInst),
        mercury_format_constrained_inst_vars(Lang, InstVarSet, Vars, CInst,
            S, !U)
    ;
        Inst = defined_inst(InstName),
        mercury_format_inst_name(Lang, InstVarSet, InstName, S, !U)
    ;
        Inst = not_reached,
        add_string("not_reached", S, !U)
    ).

:- pred mercury_format_bound_insts(output_lang::in,  inst_varset::in,
    list(bound_inst)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_bound_insts(_, _, [], _S, !U).
mercury_format_bound_insts(Lang, InstVarSet, [BoundInst | BoundInsts],
        S, !U) :-
    BoundInst = bound_functor(ConsId, Args),
    (
        Args = [],
        mercury_format_cons_id(Lang, needs_brackets, ConsId, S, !U)
    ;
        Args = [_ | _],
        mercury_format_cons_id(Lang, does_not_need_brackets, ConsId, S, !U),
        add_string("(", S, !U),
        mercury_format_inst_list(Lang, InstVarSet, Args, S, !U),
        add_string(")", S, !U)
    ),
    (
        BoundInsts = []
    ;
        BoundInsts = [_ | _],
        add_string(" ; ", S, !U),
        mercury_format_bound_insts(Lang, InstVarSet, BoundInsts, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_inst_name(Lang, InstVarSet, InstName, S, !U) :-
    (
        InstName = user_inst(Name, Args),
        (
            Args = [],
            mercury_format_bracketed_sym_name(Name, S, !U)
        ;
            Args = [_ | _],
            mercury_format_sym_name(Name, S, !U),
            add_string("(", S, !U),
            mercury_format_inst_list(Lang, InstVarSet, Args, S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = unify_inst(IsLive, Real, InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($pred, "unify_inst")
        ;
            Lang = output_debug,
            add_string("$unify(", S, !U),
            mercury_format_is_live_comma(IsLive, S, !U),
            mercury_format_comma_real(Real, S, !U),
            mercury_format_inst_list(Lang, InstVarSet, [InstA, InstB], S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = merge_inst(InstA, InstB),
        (
            Lang = output_mercury,
            unexpected($pred, "merge_inst")
        ;
            Lang = output_debug,
            add_string("$merge_inst(", S, !U),
            mercury_format_inst_list(Lang, InstVarSet, [InstA, InstB], S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = ground_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($pred, "ground_inst")
        ;
            Lang = output_debug,
            add_string("$ground(", S, !U),
            mercury_format_inst_name(Lang, InstVarSet, SubInstName, S, !U),
            add_string(", ", S, !U),
            mercury_format_is_live_comma(IsLive, S, !U),
            mercury_format_uniqueness(Uniq, "shared", S, !U),
            mercury_format_comma_real(Real, S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = any_inst(SubInstName, Uniq, IsLive, Real),
        (
            Lang = output_mercury,
            unexpected($pred, "any_inst")
        ;
            Lang = output_debug,
            add_string("$any(", S, !U),
            mercury_format_inst_name(Lang, InstVarSet, SubInstName, S, !U),
            add_string(", ", S, !U),
            mercury_format_is_live_comma(IsLive, S, !U),
            mercury_format_uniqueness(Uniq, "shared", S, !U),
            mercury_format_comma_real(Real, S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = shared_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($pred, "shared_inst")
        ;
            Lang = output_debug,
            add_string("$shared_inst(", S, !U),
            mercury_format_inst_name(Lang, InstVarSet, SubInstName, S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = mostly_uniq_inst(SubInstName),
        (
            Lang = output_mercury,
            unexpected($pred, "mostly_uniq_inst")
        ;
            Lang = output_debug,
            add_string("$mostly_uniq_inst(", S, !U),
            mercury_format_inst_name(Lang, InstVarSet, SubInstName, S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = typed_ground(Uniqueness, Type),
        (
            Lang = output_mercury,
            unexpected($pred, "typed_ground")
        ;
            Lang = output_debug,
            add_string("$typed_ground(", S, !U),
            mercury_format_uniqueness(Uniqueness, "shared", S, !U),
            add_string(", ", S, !U),
            varset.init(TypeVarSet),
            mercury_format_type(TypeVarSet, print_name_only, Type, S, !U),
            add_string(")", S, !U)
        )
    ;
        InstName = typed_inst(Type, SubInstName),
        (
            Lang = output_mercury,
            mercury_format_inst_name(Lang, InstVarSet, SubInstName, S, !U)
        ;
            Lang = output_debug,
            add_string("$typed_inst(", S, !U),
            varset.init(TypeVarSet),
            mercury_format_type(TypeVarSet, print_name_only, Type, S, !U),
            add_string(", ", S, !U),
            mercury_format_inst_name(Lang, InstVarSet, SubInstName, S, !U),
            add_string(")", S, !U)
        )
    ).

%---------------------------------------------------------------------------%

mercury_format_constrained_inst_vars(Lang, InstVarSet, !.Vars, Inst, S, !U) :-
    ( if set.remove_least(Var, !Vars) then
        add_string("(", S, !U),
        mercury_format_var_vs(InstVarSet, print_name_only, Var, S, !U),
        add_string(" =< ", S, !U),
        mercury_format_constrained_inst_vars(Lang, InstVarSet, !.Vars, Inst,
            S, !U),
        add_string(")", S, !U)
    else
        mercury_format_inst(Lang, InstVarSet, Inst, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_ground_pred_inst_info(Lang, InstVarSet, Uniq, PredInstInfo,
        S, !U) :-
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
    (
        Uniq = shared
    ;
        ( Uniq = unique
        ; Uniq = mostly_unique
        ; Uniq = clobbered
        ; Uniq = mostly_clobbered
        ),
        add_string("/* ", S, !U),
        mercury_format_uniqueness(Uniq, "ground", S, !U),
        add_string(" */", S, !U)
    ),
    (
        PredOrFunc = pf_predicate,
        (
            Modes = [],
            add_string("((pred) is ", S, !U),
            add_string(mercury_det_to_string(Det), S, !U),
            add_string(")", S, !U)
        ;
            Modes = [_ | _],
            add_string("(pred(", S, !U),
            mercury_format_mode_list(Lang, InstVarSet, Modes, S, !U),
            add_string(") is ", S, !U),
            add_string(mercury_det_to_string(Det), S, !U),
            add_string(")", S, !U)
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        (
            ArgModes = [],
            add_string("((func) = ", S, !U)
        ;
            ArgModes = [_ | _],
            add_string("(func(", S, !U),
            mercury_format_mode_list(Lang, InstVarSet, ArgModes, S, !U),
            add_string(") = ", S, !U)
        ),
        mercury_format_mode(Lang, InstVarSet, RetMode, S, !U),
        add_string(" is ", S, !U),
        add_string(mercury_det_to_string(Det), S, !U),
        add_string(")", S, !U)
    ),
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        add_string(" /* arg regs: [", S, !U),
        mercury_format_arg_reg_list(ArgRegs, S, !U),
        add_string("] */", S, !U)
    ;
        MaybeArgRegs = arg_reg_types_unset
    ).

mercury_format_any_pred_inst_info(Lang, InstVarSet, Uniq, PredInstInfo,
        S, !U) :-
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, MaybeArgRegs, Det),
    (
        Uniq = shared
    ;
        ( Uniq = unique
        ; Uniq = mostly_unique
        ; Uniq = clobbered
        ; Uniq = mostly_clobbered
        ),
        add_string("/* ", S, !U),
        mercury_format_uniqueness(Uniq, "any", S, !U),
        add_string(" */", S, !U)
    ),
    (
        PredOrFunc = pf_predicate,
        (
            Modes = [],
            add_string("(any_pred is ", S, !U),
            add_string(mercury_det_to_string(Det), S, !U),
            add_string(")", S, !U)
        ;
            Modes = [_ | _],
            add_string("(any_pred(", S, !U),
            mercury_format_mode_list(Lang, InstVarSet, Modes, S, !U),
            add_string(") is ", S, !U),
            add_string(mercury_det_to_string(Det), S, !U),
            add_string(")", S, !U)
        )
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        (
            Modes = [],
            add_string("(any_func = ", S, !U)
        ;
            Modes = [_ | _],
            add_string("(any_func(", S, !U),
            mercury_format_mode_list(Lang, InstVarSet, ArgModes, S, !U),
            add_string(") = ", S, !U)
        ),
        mercury_format_mode(Lang, InstVarSet, RetMode, S, !U),
        add_string(" is ", S, !U),
        add_string(mercury_det_to_string(Det), S, !U),
        add_string(")", S, !U)
    ),
    (
        MaybeArgRegs = arg_reg_types(ArgRegs),
        add_string(" /* arg regs: [", S, !U),
        mercury_format_arg_reg_list(ArgRegs, S, !U),
        add_string("] */", S, !U)
    ;
        MaybeArgRegs = arg_reg_types_unset
    ).

:- pred mercury_format_arg_reg_list(list(ho_arg_reg)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_arg_reg_list([], _S, !U).
mercury_format_arg_reg_list([Reg | Regs], S, !U) :-
    (
        Reg = ho_arg_reg_r,
        add_string("reg_r", S, !U)
    ;
        Reg = ho_arg_reg_f,
        add_string("reg_f", S, !U)
    ),
    (
        Regs = []
    ;
        Regs = [_ | _],
        add_string(", ", S, !U),
        mercury_format_arg_reg_list(Regs, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_is_live_comma(IsLive, S, !U) :-
    (
        IsLive = is_live,
        add_string("live, ", S, !U)
    ;
        IsLive = is_dead,
        add_string("dead, ", S, !U)
    ).

mercury_format_real_comma(Real, S, !U) :-
    (
        Real = real_unify,
        add_string("real, ", S, !U)
    ;
        Real = fake_unify,
        add_string("fake, ", S, !U)
    ).

:- pred mercury_format_comma_real(unify_is_real::in, S::in, U::di, U::uo)
    is det <= pt_output(S, U).

mercury_format_comma_real(Real, S, !U) :-
    (
        Real = real_unify,
        add_string(", real", S, !U)
    ;
        Real = fake_unify,
        add_string(", fake", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_uniqueness_to_string(shared, SharedStr) = SharedStr.
mercury_uniqueness_to_string(unique, _) = "unique".
mercury_uniqueness_to_string(mostly_unique, _) = "mostly_unique".
mercury_uniqueness_to_string(clobbered, _) = "clobbered".
mercury_uniqueness_to_string(mostly_clobbered, _) = "mostly_clobbered".

mercury_format_uniqueness(Uniq, SharedStr, S, !U) :-
    add_string(mercury_uniqueness_to_string(Uniq, SharedStr), S, !U).

mercury_any_uniqueness_to_string(shared) = "any".
mercury_any_uniqueness_to_string(unique) = "unique_any".
mercury_any_uniqueness_to_string(mostly_unique) = "mostly_unique_any".
mercury_any_uniqueness_to_string(clobbered) = "clobbered_any".
mercury_any_uniqueness_to_string(mostly_clobbered) = "mostly_clobbered_any".

mercury_format_any_uniqueness(Uniq, S, !U) :-
    add_string(mercury_any_uniqueness_to_string(Uniq), S, !U).

%---------------------------------------------------------------------------%

mercury_mode_list_to_string(Lang, InstVarSet, Modes) = Str :-
    State0 = string.builder.init,
    mercury_format_mode_list(Lang, InstVarSet, Modes, string.builder.handle,
        State0, State),
    Str = string.builder.to_string(State).

mercury_output_mode_list(Stream, Lang, InstVarSet, Modes, !IO) :-
    mercury_format_mode_list(Lang, InstVarSet, Modes, Stream, !IO).

mercury_format_mode_list(_, _, [], _S, !U).
mercury_format_mode_list(Lang, InstVarSet, [Mode | Modes], S, !U) :-
    mercury_format_mode(Lang, InstVarSet, Mode, S, !U),
    (
        Modes = []
    ;
        Modes = [_ | _],
        add_string(", ", S, !U),
        mercury_format_mode_list(Lang, InstVarSet, Modes, S, !U)
    ).

%---------------------%

mercury_mode_to_string(Lang, InstVarSet, Mode) = Str :-
    State0 = string.builder.init,
    mercury_format_mode(Lang, InstVarSet, Mode, string.builder.handle,
        State0, State),
    Str = string.builder.to_string(State).

mercury_output_mode(Stream, Lang, InstVarSet, Mode, !IO) :-
    mercury_format_mode(Lang, InstVarSet, Mode, Stream, !IO).

mercury_format_mode(Lang, InstVarSet, Mode0, S, !U) :-
    (
        Mode0 = from_to_mode(FromInst0, ToInst0),
        % When generating output intended for humans, such as error
        % messages and HLDS dumps, we try to make the mode as easy to read
        % as possible. This involves transforming e.g. "free >> ground"
        % to simply "out".
        %
        % However, we cannot replace the expansions of in, out etc
        % with their names when we are generating actual Mercury code,
        % because if we did, we would make the actual *definitions*
        % of these modes in builtin.int circular.
        (
            Lang = output_mercury,
            mercury_format_from_to_mode(Lang, InstVarSet,
                FromInst0, ToInst0, S, !U)
        ;
            Lang = output_debug,
            ( if
                FromInst0 = ground(_Uniq, HOInstInfo),
                HOInstInfo = higher_order(pred_inst_info(_, _, _, _)),
                ToInst0 = FromInst0
            then
                % This special case would be recognized by insts_to_mode
                % as in(FromInst). However, in the case of higher order insts,
                % the compiler allows programmers to omit the in(...) wrapper.
                % Since we don't have to write the wrapper, we don't.
                mercury_format_inst(Lang, InstVarSet, FromInst0, S, !U)
            else
                insts_to_mode(FromInst0, ToInst0, Mode1),
                (
                    Mode1 = from_to_mode(FromInst1, ToInst1),
                    strip_module_names_from_inst(strip_builtin_module_name,
                        FromInst1, FromInst),
                    strip_module_names_from_inst(strip_builtin_module_name,
                        ToInst1, ToInst),
                    mercury_format_from_to_mode(Lang, InstVarSet,
                        FromInst, ToInst, S, !U)
                ;
                    Mode1 = user_defined_mode(SymName, ArgInsts),
                    mercury_format_user_defined_mode(Lang, InstVarSet,
                        SymName, ArgInsts, S, !U)
                )
            )
        )
    ;
        Mode0 = user_defined_mode(SymName, ArgInsts),
        mercury_format_user_defined_mode(Lang, InstVarSet,
            SymName, ArgInsts, S, !U)
    ).

:- pred mercury_format_from_to_mode(output_lang::in, inst_varset::in,
    mer_inst::in, mer_inst::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_from_to_mode(Lang, InstVarSet, FromInst, ToInst, S, !U) :-
    add_string("(", S, !U),
    mercury_format_inst(Lang, InstVarSet, FromInst, S, !U),
    add_string(" >> ", S, !U),
    mercury_format_inst(Lang, InstVarSet, ToInst, S, !U),
    add_string(")", S, !U).

:- pred mercury_format_user_defined_mode(output_lang::in, inst_varset::in,
    sym_name::in, list(mer_inst)::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_user_defined_mode(Lang, InstVarSet, SymName, ArgInsts, S, !U) :-
    % When generating output for a human, we
    %
    % 1 omit brackets from around sym_names, and
    % 2 omit module qualifiers from sym_names.
      %
    % The brackets are needed only to help the parser when trying to read in
    % the output sym_name, when that happens to be an operator. Humans can
    % parse the bracketless output even if the parser can't.
    %
    % It is extremely rare for two modules to define two modes with identical
    % names. Therefore in 99.9%+ of cases, the module qualifiers on mode names
    % are just clutter that humans would prefer not to have to read.
    (
        ArgInsts = [],
        (
            Lang = output_mercury,
            mercury_format_bracketed_sym_name(SymName, S, !U)
        ;
            Lang = output_debug,
            Name = unqualify_name(SymName),
            add_string(Name, S, !U)
        )
    ;
        ArgInsts = [_ | _],
        (
            Lang = output_mercury,
            mercury_format_sym_name(SymName, S, !U)
        ;
            Lang = output_debug,
            Name = unqualify_name(SymName),
            add_string(Name, S, !U)
        ),
        add_string("(", S, !U),
        mercury_format_inst_list(Lang, InstVarSet, ArgInsts, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_inst.
%---------------------------------------------------------------------------%
