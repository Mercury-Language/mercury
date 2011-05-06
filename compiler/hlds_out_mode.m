%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2011 The University of Melbourne.
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

:- import_module pair.
:- import_module require.
:- import_module set.
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
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
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
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
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
    unexpected(this_file, "bound_insts_to_term([])").
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
        unexpected(this_file,
            "bound_insts_to_term: cons_id_and_args_to_term failed")
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

mercury_output_uni_mode_list(UniModes, VarSet, !IO) :-
    mercury_format_uni_mode_list(UniModes, VarSet, !IO).

mercury_uni_mode_list_to_string(UniModes, VarSet) = String :-
    mercury_format_uni_mode_list(UniModes, VarSet, "", String).

:- pred mercury_format_uni_mode_list(list(uni_mode)::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_uni_mode_list([], _VarSet, !IO).
mercury_format_uni_mode_list([Mode | Modes], VarSet, !IO) :-
    mercury_format_uni_mode(Mode, VarSet, !IO),
    (
        Modes = [],
        true
    ;
        Modes = [_ | _],
        add_string(", ", !IO),
        mercury_format_uni_mode_list(Modes, VarSet, !IO)
    ).

mercury_output_uni_mode(UniMode, VarSet, !IO) :-
    mercury_format_uni_mode(UniMode, VarSet, !IO).

mercury_uni_mode_to_string(UniMode, VarSet) = String :-
    mercury_format_uni_mode(UniMode, VarSet, "", String).

:- pred mercury_format_uni_mode(uni_mode::in, inst_varset::in,
    U::di, U::uo) is det <= output(U).

mercury_format_uni_mode((InstA1 - InstB1 -> InstA2 - InstB2), VarSet, !IO) :-
    mercury_format_mode((InstA1 -> InstA2), simple_inst_info(VarSet), !IO),
    add_string(" = ", !IO),
    mercury_format_mode((InstB1 -> InstB2), simple_inst_info(VarSet), !IO).

%-----------------------------------------------------------------------------%

mercury_output_expanded_inst(Inst, VarSet, ModuleInfo, !IO) :-
    set.init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(VarSet, ModuleInfo, Expansions), !IO).

mercury_expanded_inst_to_string(Inst, VarSet, ModuleInfo) = String :-
    set.init(Expansions),
    mercury_format_inst(Inst,
        expanded_inst_info(VarSet, ModuleInfo, Expansions), "", String).

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
                eii_expansions  :: set(inst_name)
                                % the set of already-expanded insts;
                                % further occurrences of these will
                                % be output as "..."
            ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_out_mode.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_mode.
%-----------------------------------------------------------------------------%
