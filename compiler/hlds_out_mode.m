%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
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
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- pred write_instmap(instmap::in, prog_varset::in, bool::in, int::in,
    io::di, io::uo) is det.

:- pred write_var_inst_list(assoc_list(prog_var, mer_inst)::in,
    prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Convert a mode or inst to a term representation.
    %
:- func mode_to_term(mer_mode) = prog_term.
:- func mode_to_term_with_context(term.context, mer_mode) = prog_term.
:- func inst_to_term(mer_inst) = prog_term.

%-----------------------------------------------------------------------------%

:- type incl_addr
    --->    do_not_incl_addr
    ;       do_incl_addr.

    % Output a list of insts in a format that makes them easy to read
    % but may not be valid Mercury.
    %
:- pred mercury_output_structured_inst_list(list(mer_inst)::in, int::in,
    incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_inst_list_to_string(list(mer_inst), int,
    incl_addr, inst_varset) = string.

    % Output an inst in a format that makes it easy to read
    % but may not be valid Mercury.
    % The `int' argument specifies the indentation level.
    % (These routines are used with `--debug-modes'.)
    %
:- pred mercury_output_structured_inst(mer_inst::in, int::in,
    incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_inst_to_string(mer_inst, int,
    incl_addr, inst_varset) = string.

%-----------------------------------------------------------------------------%

:- pred mercury_output_structured_uni_mode(uni_mode::in, int::in,
    incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_uni_mode_to_string(uni_mode, int, incl_addr,
    inst_varset) = string.

:- pred mercury_output_structured_uni_mode_list(list(uni_mode)::in, int::in,
    incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_uni_mode_list_to_string(list(uni_mode), int,
    incl_addr, inst_varset) = string.

%-----------------------------------------------------------------------------%

:- pred mercury_output_uni_mode(uni_mode::in, inst_varset::in,
    io::di, io::uo) is det.
:- func mercury_uni_mode_to_string(uni_mode, inst_varset) = string.

:- pred mercury_output_uni_mode_list(list(uni_mode)::in, inst_varset::in,
    io::di, io::uo) is det.
:- func mercury_uni_mode_list_to_string(list(uni_mode), inst_varset) = string.

%-----------------------------------------------------------------------------%

    % Output an inst in a format where all compiler-defined insts
    % have been expanded out; recursive insts have their self-referential
    % parts printed out as elipses ("...").
    % (These routines are used for outputting insts in mode errors.)
    %
:- pred mercury_output_expanded_inst(mer_inst::in, inst_varset::in,
    module_info::in, io::di, io::uo) is det.
:- func mercury_expanded_inst_to_string(mer_inst, inst_varset, module_info)
    = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module hlds.instmap.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

write_instmap(InstMap, VarSet, AppendVarNums, Indent, !IO) :-
    ( instmap_is_unreachable(InstMap) ->
        io.write_string("unreachable", !IO)
    ;
        instmap_to_assoc_list(InstMap, AssocList),
        write_var_inst_list(AssocList, VarSet, AppendVarNums, Indent, !IO)
    ).

write_var_inst_list([], _, _, _, !IO).
write_var_inst_list([Var - Inst | Rest], VarSet, AppendVarNums, Indent, !IO) :-
    mercury_output_var(VarSet, AppendVarNums, Var, !IO),
    io.write_string(" -> ", !IO),
    varset.init(InstVarSet),
    mercury_output_inst(Inst, InstVarSet, !IO),
    (
        Rest = []
    ;
        Rest = [_ | _],
        mercury_output_newline(Indent, !IO),
        io.write_string("%            ", !IO),
        write_var_inst_list(Rest, VarSet, AppendVarNums, Indent, !IO)
    ).

%-----------------------------------------------------------------------------%

mode_to_term(Mode) = mode_to_term_with_context(term.context_init, Mode).

mode_to_term_with_context(Context, Mode) = Term :-
    (
        Mode = (InstA -> InstB),
        (
            % Check for higher-order pred or func modes, and output them
            % in a nice format.
            InstA = ground(_Uniq, higher_order(_)),
            InstB = InstA
        ->
            Term = inst_to_term_with_context(InstA, Context)
        ;
            construct_qualified_term(unqualified(">>"),
                [inst_to_term_with_context(InstA, Context),
                inst_to_term_with_context(InstB, Context)],
                Context, Term)
        )
    ;
        Mode = user_defined_mode(Name, Args),
        construct_qualified_term(Name,
            list.map(map_inst_to_term(Context), Args),
            Context, Term)
    ).

:- func make_atom(string, prog_context) = prog_term.

make_atom(Name, Context) =
    term.functor(term.atom(Name), [], Context).

:- func map_inst_to_term(prog_context, mer_inst) = prog_term.

map_inst_to_term(Context, Inst) = inst_to_term_with_context(Inst, Context).

inst_to_term(Inst) = inst_to_term_with_context(Inst, term.context_init).

:- func inst_to_term_with_context(mer_inst, prog_context) = prog_term.

inst_to_term_with_context(Inst, Context) = Term :-
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            Term = any_pred_inst_info_to_term(Uniq, PredInstInfo, Context)
        ;
            HOInstInfo = none,
            Term = make_atom(any_inst_uniqueness(Uniq), Context)
        )
    ;
        Inst = free,
        Term = make_atom("free", Context)
    ;
        Inst = free(Type),
        unparse_type(Type, Term0),
        Term1 = term.coerce(Term0),
        Term = term.functor(term.atom("free"), [Term1], Context)
    ;
        Inst = bound(Uniq, BoundInsts),
        construct_qualified_term(
            unqualified(inst_uniqueness(Uniq, "bound")),
            [bound_insts_to_term(BoundInsts, Context)], Context, Term)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            Term = ground_pred_inst_info_to_term(Uniq, PredInstInfo, Context)
        ;
            HOInstInfo = none,
            Term = make_atom(inst_uniqueness(Uniq, "ground"), Context)
        )
    ;
        Inst = inst_var(Var),
        Term = term.coerce(term.variable(Var, context_init))
    ;
        Inst = constrained_inst_vars(Vars, SubInst),
        Term = set.fold(func(Var, VarTerm) =
                term.functor(term.atom("=<"),
                    [term.coerce(term.variable(Var, context_init)), VarTerm],
                    Context),
            Vars, inst_to_term_with_context(SubInst, Context))
    ;
        Inst = abstract_inst(Name, Args),
        Term = inst_name_to_term(user_inst(Name, Args), Context)
    ;
        Inst = defined_inst(InstName),
        Term = inst_name_to_term(InstName, Context)
    ;
        Inst = not_reached,
        Term = make_atom("not_reached", Context)
    ).

:- func ground_pred_inst_info_to_term(uniqueness, pred_inst_info, prog_context)
    = prog_term.

ground_pred_inst_info_to_term(_Uniq, PredInstInfo, Context) = Term :-
    % XXX we ignore Uniq
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, _, Det),
    (
        PredOrFunc = pf_predicate,
        construct_qualified_term(unqualified("pred"),
            list.map(mode_to_term_with_context(Context), Modes),
            Context, ModesTerm)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        construct_qualified_term(unqualified("func"),
            list.map(mode_to_term_with_context(Context), ArgModes),
            Context, ArgModesTerm),
        construct_qualified_term(unqualified("="),
            [ArgModesTerm, mode_to_term_with_context(Context, RetMode)],
            Context, ModesTerm)
    ),
    construct_qualified_term(unqualified("is"),
        [ModesTerm, det_to_term(Det, Context)], Context, Term).

:- func any_pred_inst_info_to_term(uniqueness, pred_inst_info, prog_context)
    = prog_term.

any_pred_inst_info_to_term(_Uniq, PredInstInfo, Context) = Term :-
    % XXX we ignore Uniq
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, _, Det),
    (
        PredOrFunc = pf_predicate,
        construct_qualified_term(unqualified("any_pred"),
            list.map(mode_to_term_with_context(Context), Modes),
            Context, ModesTerm)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        construct_qualified_term(unqualified("any_func"),
            list.map(mode_to_term_with_context(Context), ArgModes),
            Context, ArgModesTerm),
        construct_qualified_term(unqualified("="),
            [ArgModesTerm, mode_to_term_with_context(Context, RetMode)],
            Context, ModesTerm)
    ),
    construct_qualified_term(unqualified("is"),
        [ModesTerm, det_to_term(Det, Context)], Context, Term).

:- func inst_name_to_term(inst_name, prog_context) = prog_term.

inst_name_to_term(InstName, Context) = Term :-
    (
        InstName = user_inst(Name, Args),
        construct_qualified_term(Name,
            list.map(map_inst_to_term(Context), Args),
            Context, Term)
    ;
        InstName = merge_inst(InstA, InstB),
        construct_qualified_term(unqualified("$merge_inst"),
            list.map(map_inst_to_term(Context), [InstA, InstB]),
            Context, Term)
    ;
        InstName = shared_inst(SubInstName),
        construct_qualified_term(unqualified("$shared_inst"),
            [inst_name_to_term(SubInstName, Context)],
            Context, Term)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        construct_qualified_term(unqualified("$mostly_uniq_inst"),
            [inst_name_to_term(SubInstName, Context)],
            Context, Term)
    ;
        InstName = unify_inst(Liveness, InstA, InstB, Real),
        construct_qualified_term(unqualified("$unify"),
            [make_atom(is_live_to_str(Liveness), Context)] ++
            list.map(map_inst_to_term(Context), [InstA, InstB]) ++
            [make_atom(unify_is_real_to_str(Real), Context)],
            Context, Term)
    ;
        InstName = ground_inst(SubInstName, IsLive, Uniq, Real),
        construct_qualified_term(unqualified("$ground"),
            [inst_name_to_term(SubInstName, Context),
            make_atom(is_live_to_str(IsLive), Context),
            make_atom(inst_uniqueness(Uniq, "shared"), Context),
            make_atom(unify_is_real_to_str(Real), Context)],
            Context, Term)
    ;
        InstName = any_inst(SubInstName, IsLive, Uniq, Real),
        construct_qualified_term(unqualified("$any"),
            [inst_name_to_term(SubInstName, Context),
            make_atom(is_live_to_str(IsLive), Context),
            make_atom(inst_uniqueness(Uniq, "shared"), Context),
            make_atom(unify_is_real_to_str(Real), Context)],
            Context, Term)
    ;
        InstName = typed_ground(Uniq, Type),
        unparse_type(Type, Term0),
        construct_qualified_term(unqualified("$typed_ground"),
            [make_atom(inst_uniqueness(Uniq, "shared"), Context),
            term.coerce(Term0)],
            Context, Term)
    ;
        InstName = typed_inst(Type, SubInstName),
        unparse_type(Type, Term0),
        construct_qualified_term(unqualified("$typed_inst"),
            [term.coerce(Term0),
            inst_name_to_term(SubInstName, Context)],
            Context, Term)
    ).

:- func is_live_to_str(is_live) = string.

is_live_to_str(is_live) = "live".
is_live_to_str(is_dead) = "dead".

:- func unify_is_real_to_str(unify_is_real) = string.

unify_is_real_to_str(real_unify) = "real".
unify_is_real_to_str(fake_unify) = "fake".

:- func any_inst_uniqueness(uniqueness) = string.

any_inst_uniqueness(shared) = "any".
any_inst_uniqueness(unique) = "unique_any".
any_inst_uniqueness(mostly_unique) = "mostly_unique_any".
any_inst_uniqueness(clobbered) = "clobbered_any".
any_inst_uniqueness(mostly_clobbered) = "mostly_clobbered_any".

:- func inst_uniqueness(uniqueness, string) = string.

inst_uniqueness(shared, SharedName) = SharedName.
inst_uniqueness(unique, _) = "unique".
inst_uniqueness(mostly_unique, _) = "mostly_unique".
inst_uniqueness(clobbered, _) = "clobbered".
inst_uniqueness(mostly_clobbered, _) = "mostly_clobbered".

:- func bound_insts_to_term(list(bound_inst), prog_context) = prog_term.

bound_insts_to_term([], _) = _ :-
    unexpected($module, $pred, "bound_insts_to_term([])").
bound_insts_to_term([BoundInst | BoundInsts], Context) = Term :-
    BoundInst = bound_functor(ConsId, Args),
    ArgTerms = list.map(map_inst_to_term(Context), Args),
    ( cons_id_and_args_to_term(ConsId, ArgTerms, FirstTerm) ->
        (
            BoundInsts = [],
            Term = FirstTerm
        ;
            BoundInsts = [_ | _],
            construct_qualified_term(unqualified(";"),
                [FirstTerm, bound_insts_to_term(BoundInsts, Context)],
                Context, Term)
        )
    ;
        unexpected($module, $pred, "cons_id_and_args_to_term failed")
    ).

:- func det_to_term(determinism, prog_context) = prog_term.

det_to_term(Det, Context) = make_atom(det_to_string(Det), Context).

:- func det_to_string(determinism) = string.

det_to_string(detism_erroneous) = "erroneous".
det_to_string(detism_failure) = "failure".
det_to_string(detism_det) = "det".
det_to_string(detism_semi) = "semidet".
det_to_string(detism_cc_multi) = "cc_multi".
det_to_string(detism_cc_non) = "cc_nondet".
det_to_string(detism_multi) = "multi".
det_to_string(detism_non) = "nondet".

%-----------------------------------------------------------------------------%

mercury_output_structured_uni_mode_list(Insts, Indent, InclAddr, InstVarSet,
        !IO) :-
    mercury_format_structured_uni_mode_list(Insts, 1, Indent, InclAddr,
        InstVarSet, !IO).

mercury_structured_uni_mode_list_to_string(Insts, Indent, InclAddr, InstVarSet)
        = String :-
    mercury_format_structured_uni_mode_list(Insts, 1, Indent, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_uni_mode_list(list(uni_mode)::in, int::in,
    int::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_uni_mode_list([], _, _, _, _, !U).
mercury_format_structured_uni_mode_list([UniMode | UniModes], ArgNum, Indent,
        InclAddr, InstVarSet, !U) :-
    mercury_format_tabs(Indent, !U),
    add_string("argument ", !U),
    add_int(ArgNum, !U),
    add_string(":\n", !U),
    mercury_format_structured_uni_mode(UniMode, Indent,
        InclAddr, InstVarSet, !U),
    mercury_format_structured_uni_mode_list(UniModes, ArgNum +1, Indent,
        InclAddr, InstVarSet, !U).

%-----------------------------------------------------------------------------%

mercury_output_structured_uni_mode(Inst, Indent, InclAddr, InstVarSet,
        !IO) :-
    mercury_format_structured_uni_mode(Inst, Indent, InclAddr, InstVarSet,
        !IO).

mercury_structured_uni_mode_to_string(Inst, Indent, InclAddr, InstVarSet)
        = String :-
    mercury_format_structured_uni_mode(Inst, Indent, InclAddr, InstVarSet,
        "", String).

:- pred mercury_format_structured_uni_mode(uni_mode::in, int::in,
    incl_addr::in, inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_uni_mode(UniMode, Indent, InclAddr, InstVarSet,
        !U) :-
    UniMode = (InstA1 - InstB1 -> InstA2 - InstB2),
    get_inst_addr(InstA1, InstA1Addr),
    get_inst_addr(InstA2, InstA2Addr),
    get_inst_addr(InstB1, InstB1Addr),
    get_inst_addr(InstB2, InstB2Addr),

    mercury_format_tabs(Indent, !U),
    add_string("old lhs inst:\n", !U),
    mercury_format_structured_inst(InstA1, Indent, InclAddr, InstVarSet, !U),

    mercury_format_tabs(Indent, !U),
    ( InstB1Addr = InstA1Addr ->
        % We have printed the old lhs inst.
        add_string("old rhs inst: same as old lhs inst\n", !U)
    ;
        add_string("old rhs inst:\n", !U),
        mercury_format_structured_inst(InstB1, Indent, InclAddr,
            InstVarSet, !U)
    ),

    mercury_format_tabs(Indent, !U),
    ( InstA2Addr = InstA1Addr ->
        % We have printed the old lhs inst.
        add_string("new lhs inst: unchanged\n", !U)
    ; InstA2Addr = InstB1Addr ->
        % We have printed or described the old rhs inst.
        add_string("new lhs inst: changed to old rhs inst\n", !U)
    ;
        add_string("new lhs inst:\n", !U),
        mercury_format_structured_inst(InstA2, Indent, InclAddr,
            InstVarSet, !U)
    ),

    mercury_format_tabs(Indent, !U),
    ( InstB2Addr = InstB1Addr ->
        % We have printed or described the old rhs inst.
        add_string("new rhs inst: unchanged\n", !U)
    ; InstB2Addr = InstA2Addr ->
        % We have printed or described the new lhs inst.
        add_string("new rhs inst: changed to new lhs inst\n", !U)
    ;
        add_string("new rhs inst:\n", !U),
        mercury_format_structured_inst(InstB2, Indent, InclAddr,
            InstVarSet, !U)
    ).

%-----------------------------------------------------------------------------%

mercury_output_structured_inst_list(Insts, Indent, InclAddr, InstVarSet, !IO) :-
    mercury_format_structured_inst_list(Insts, Indent, InclAddr, InstVarSet,
        !IO).

mercury_structured_inst_list_to_string(Insts, Indent, InclAddr, InstVarSet)
        = String :-
    mercury_format_structured_inst_list(Insts, Indent, InclAddr, InstVarSet,
        "", String).

:- pred mercury_format_structured_inst_list(list(mer_inst)::in, int::in,
    incl_addr::in, inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_inst_list([], _, _, _, !U).
mercury_format_structured_inst_list([Inst | Insts], Indent, InclAddr,
        InstVarSet, !U) :-
    mercury_format_structured_inst(Inst, Indent, InclAddr, InstVarSet, !U),
    mercury_format_structured_inst_list(Insts, Indent, InclAddr,
        InstVarSet, !U).

%-----------------------------------------------------------------------------%

mercury_output_structured_inst(Inst, Indent, InclAddr, InstVarSet, !U) :-
    mercury_format_structured_inst(Inst, Indent, InclAddr, InstVarSet, !U).

mercury_structured_inst_to_string(Inst, Indent, InclAddr, InstVarSet)
        = String :-
    mercury_format_structured_inst(Inst, Indent, InclAddr, InstVarSet,
        "", String).

:- pred mercury_format_structured_inst(mer_inst::in, int::in, incl_addr::in,
    inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_inst(Inst, Indent, InclAddr, InstVarSet, !U) :-
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
            mercury_format_any_pred_inst_info(Uniq, PredInstInfo,
                InstVarSet, !U)
        ;
            HOInstInfo = none,
            mercury_format_any_uniqueness(Uniq, !U)
        ),
        add_string("\n", !U)
    ;
        Inst = free,
        add_string("free\n", !U)
    ;
        Inst = free(_T),
        add_string("free(with some type)\n", !U)
    ;
        Inst = bound(Uniq, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", !U),
        add_string("(\n", !U),
        mercury_format_structured_bound_insts(BoundInsts, Indent, InclAddr,
            InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            mercury_format_ground_pred_inst_info(Uniq, PredInstInfo,
                InstVarSet, !U)
        ;
            HOInstInfo = none,
            mercury_format_uniqueness(Uniq, "ground", !U)
        ),
        add_string("\n", !U)
    ;
        Inst = inst_var(Var),
        mercury_format_var(InstVarSet, no, Var, !U),
        add_string("\n", !U)
    ;
        Inst = constrained_inst_vars(Vars, ConstrainedInst),
        mercury_format_constrained_inst_vars(Vars, ConstrainedInst,
            simple_inst_info(InstVarSet), !U),
        add_string("\n", !U)
    ;
        Inst = abstract_inst(Name, Args),
        mercury_format_structured_inst_name(user_inst(Name, Args), 0,
            InclAddr, InstVarSet, !U)
    ;
        Inst = defined_inst(InstName),
        mercury_format_structured_inst_name(InstName, 0,
            InclAddr, InstVarSet, !U)
    ;
        Inst = not_reached,
        add_string("not_reached\n", !U)
    ).

:- pred mercury_format_structured_bound_insts(list(bound_inst)::in, int::in,
    incl_addr::in, inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_bound_insts([], _, _, _, !U).
mercury_format_structured_bound_insts([BoundInst | BoundInsts],
        Indent0, InclAddr, InstVarSet, !U) :-
    BoundInst = bound_functor(ConsId, Args),
    Indent1 = Indent0 + 1,
    Indent2 = Indent1 + 1,
    (
        Args = [],
        mercury_format_tabs(Indent1, !U),
        mercury_format_cons_id(ConsId, needs_brackets, !U),
        add_string("\n", !U)
    ;
        Args = [_ | _],
        mercury_format_tabs(Indent1, !U),
        mercury_format_cons_id(ConsId, does_not_need_brackets, !U),
        add_string("(\n", !U),
        mercury_format_structured_inst_list(Args, Indent2, InclAddr,
            InstVarSet, !U),
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
            InclAddr, InstVarSet, !U)
    ).

:- pred get_inst_addr(mer_inst::in, int::out) is det.

:- pragma foreign_proc("C",
    get_inst_addr(Inst::in, InstAddr::out),
    [will_not_call_mercury, promise_pure],
"
    InstAddr = Inst;
").

%-----------------------------------------------------------------------------%

:- pred mercury_format_structured_inst_name(inst_name::in, int::in,
    incl_addr::in, inst_varset::in, U::di, U::uo) is det <= output(U).

mercury_format_structured_inst_name(InstName, Indent, InclAddr, InstVarSet,
        !U) :-
    (
        InstName = user_inst(Name, Args),
        (
            Args = [],
            mercury_format_tabs(Indent, !U),
            mercury_format_bracketed_sym_name(Name, !U)
        ;
            Args = [_ | _],
            mercury_format_tabs(Indent, !U),
            mercury_format_sym_name(Name, !U),
            add_string("(\n", !U),
            mercury_format_structured_inst_list(Args, Indent + 1, InclAddr,
                InstVarSet, !U),
            mercury_format_tabs(Indent, !U),
            add_string(")\n", !U)
        )
    ;
        InstName = merge_inst(InstA, InstB),
        mercury_format_tabs(Indent, !U),
        add_string("$merge_inst(\n", !U),
        mercury_format_structured_inst_list([InstA, InstB], Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = shared_inst(SubInstName),
        add_string("$shared_inst(\n", !U),
        mercury_format_structured_inst_name(SubInstName, Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        mercury_format_tabs(Indent, !U),
        add_string("$mostly_uniq_inst(\n", !U),
        mercury_format_structured_inst_name(SubInstName, Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = unify_inst(IsLive, InstA, InstB, Real),
        mercury_format_tabs(Indent, !U),
        add_string("$unify(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        add_string("\n", !U),
        mercury_format_structured_inst_list([InstA, InstB], Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = ground_inst(SubInstName, IsLive, Uniq, Real),
        mercury_format_tabs(Indent, !U),
        add_string("$ground(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        mercury_format_uniqueness(Uniq, "shared", !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name(SubInstName, Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = any_inst(SubInstName, IsLive, Uniq, Real),
        mercury_format_tabs(Indent, !U),
        add_string("$any(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        mercury_format_uniqueness(Uniq, "shared", !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name(SubInstName, Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = typed_ground(Uniqueness, Type),
        mercury_format_tabs(Indent, !U),
        add_string("$typed_ground(", !U),
        mercury_format_uniqueness(Uniqueness, "shared", !U),
        add_string(", ", !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, no, Type, !U),
        add_string(")\n", !U)
    ;
        InstName = typed_inst(Type, SubInstName),
        mercury_format_tabs(Indent, !U),
        add_string("$typed_inst(", !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, no, Type, !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name(SubInstName, Indent + 1,
            InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ).

%-----------------------------------------------------------------------------%

mercury_output_uni_mode_list(UniModes, InstVarSet, !IO) :-
    mercury_format_uni_mode_list(UniModes, InstVarSet, !IO).

mercury_uni_mode_list_to_string(UniModes, InstVarSet) = String :-
    mercury_format_uni_mode_list(UniModes, InstVarSet, "", String).

:- pred mercury_format_uni_mode_list(list(uni_mode)::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_uni_mode_list([], _InstVarSet, !IO).
mercury_format_uni_mode_list([Mode | Modes], InstVarSet, !IO) :-
    mercury_format_uni_mode(Mode, InstVarSet, !IO),
    (
        Modes = [],
        true
    ;
        Modes = [_ | _],
        add_string(", ", !IO),
        mercury_format_uni_mode_list(Modes, InstVarSet, !IO)
    ).

mercury_output_uni_mode(UniMode, InstVarSet, !IO) :-
    mercury_format_uni_mode(UniMode, InstVarSet, !IO).

mercury_uni_mode_to_string(UniMode, InstVarSet) = String :-
    mercury_format_uni_mode(UniMode, InstVarSet, "", String).

:- pred mercury_format_uni_mode(uni_mode::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_uni_mode(UniMode, InstVarSet, !IO) :-
    UniMode = (InstA1 - InstB1 -> InstA2 - InstB2),
    mercury_format_mode((InstA1 -> InstA2), simple_inst_info(InstVarSet), !IO),
    add_string(" = ", !IO),
    mercury_format_mode((InstB1 -> InstB2), simple_inst_info(InstVarSet), !IO).

%-----------------------------------------------------------------------------%

mercury_output_expanded_inst(Inst, InstVarSet, ModuleInfo, !IO) :-
    set.init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(InstVarSet, ModuleInfo, Expansions), !IO).

mercury_expanded_inst_to_string(Inst, InstVarSet, ModuleInfo) = String :-
    set.init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(InstVarSet, ModuleInfo, Expansions), "", String).

:- pred mercury_format_expanded_defined_inst(inst_name::in,
    expanded_inst_info::in, U::di, U::uo) is det <= output(U).

mercury_format_expanded_defined_inst(InstName, ExpandedInstInfo, !S) :-
    ( set.member(InstName, ExpandedInstInfo ^ eii_expansions) ->
        add_string("...", !S)
    ; InstName = user_inst(_, _) ->
        % Don't expand user-defined insts, just output them as is
        % (we do expand any compiler-defined insts that occur
        % in the arguments of the user-defined inst, however).
        mercury_format_inst_name(InstName, ExpandedInstInfo, !S)
    ;
        inst_lookup(ExpandedInstInfo ^ eii_module_info, InstName, Inst),
        set.insert(InstName, ExpandedInstInfo ^ eii_expansions, Expansions),
        mercury_format_inst(Inst,
            ExpandedInstInfo ^ eii_expansions := Expansions, !S)
    ).

%-----------------------------------------------------------------------------%

:- instance inst_info(expanded_inst_info) where [
    func(instvarset/1) is eii_varset,
    pred(format_defined_inst/4) is mercury_format_expanded_defined_inst
].

:- type expanded_inst_info
    --->    expanded_inst_info(
                eii_varset      :: inst_varset,
                eii_module_info :: module_info,

                % The set of already-expanded insts; further occurrences
                % of these will be output as "...".
                eii_expansions  :: set(inst_name)
            ).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_mode.
%-----------------------------------------------------------------------------%
