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
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.instmap.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- pred write_instmap(instmap::in, prog_varset::in, bool::in, int::in,
    io::di, io::uo) is det.

:- pred write_var_inst_list(assoc_list(prog_var, mer_inst)::in,
    prog_varset::in, bool::in, int::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Convert a mode or inst to a term representation.
    %
:- func mode_to_term(output_lang, mer_mode) = prog_term.
:- func mode_to_term_with_context(output_lang, prog_context, mer_mode)
    = prog_term.
:- func inst_to_term(output_lang, mer_inst) = prog_term.
:- func inst_name_to_term(output_lang, inst_name) = prog_term.

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

:- pred mercury_output_structured_uni_mode(uni_mode::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_uni_mode_to_string(uni_mode, int,
    output_lang, incl_addr, inst_varset) = string.

:- pred mercury_output_structured_uni_mode_list(list(uni_mode)::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, io::di, io::uo) is det.
:- func mercury_structured_uni_mode_list_to_string(list(uni_mode), int,
    output_lang, incl_addr, inst_varset) = string.

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
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%-----------------------------------------------------------------------------%

:- func make_atom(prog_context, string) = prog_term.

make_atom(Context, Name) =
    term.functor(term.atom(Name), [], Context).

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

mode_to_term(Lang, Mode) =
    mode_to_term_with_context(Lang, term.context_init, Mode).

mode_to_term_with_context(Lang, Context, Mode) = Term :-
    (
        Mode = (InstA -> InstB),
        (
            % Check for higher-order pred or func modes, and output them
            % in a nice format.
            InstA = ground(_Uniq, higher_order(_)),
            InstB = InstA
        ->
            Term = inst_to_term_with_context(Lang, Context, InstA)
        ;
            construct_qualified_term_with_context(unqualified(">>"),
                [inst_to_term_with_context(Lang, Context, InstA),
                inst_to_term_with_context(Lang, Context, InstB)],
                Context, Term)
        )
    ;
        Mode = user_defined_mode(Name, Args),
        construct_qualified_term_with_context(Name,
            list.map(inst_to_term_with_context(Lang, Context), Args),
            Context, Term)
    ).

inst_to_term(Lang, Inst) =
    inst_to_term_with_context(Lang, term.context_init, Inst).

:- func inst_to_term_with_context(output_lang, prog_context, mer_inst)
    = prog_term.

inst_to_term_with_context(Lang, Context, Inst) = Term :-
    (
        Inst = any(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            Term = any_pred_inst_info_to_term(Lang, Context, Uniq,
                PredInstInfo)
        ;
            HOInstInfo = none,
            Term = make_atom(Context, any_inst_uniqueness(Uniq))
        )
    ;
        Inst = free,
        Term = make_atom(Context, "free")
    ;
        Inst = free(Type),
        unparse_type(Type, Term0),
        Term1 = term.coerce(Term0),
        Term = term.functor(term.atom("free"), [Term1], Context)
    ;
        Inst = bound(Uniq, InstResults, BoundInsts),
        (
            Lang = output_mercury,
            ArgTerms = [bound_insts_to_term(Lang, Context, BoundInsts)]
        ;
            Lang = output_debug,
            ArgTerms =
                [inst_test_results_to_term(Context, InstResults),
                bound_insts_to_term(Lang, Context, BoundInsts)]
        ),
        construct_qualified_term_with_context(
            unqualified(inst_uniqueness(Uniq, "bound")),
            ArgTerms, Context, Term)
    ;
        Inst = ground(Uniq, HOInstInfo),
        (
            HOInstInfo = higher_order(PredInstInfo),
            Term = ground_pred_inst_info_to_term(Lang, Context, Uniq,
                PredInstInfo)
        ;
            HOInstInfo = none,
            Term = make_atom(Context, inst_uniqueness(Uniq, "ground"))
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
            Vars, inst_to_term_with_context(Lang, Context, SubInst))
    ;
        Inst = abstract_inst(Name, Args),
        Term = inst_name_to_term_with_context(Lang, Context,
            user_inst(Name, Args))
    ;
        Inst = defined_inst(InstName),
        Term = inst_name_to_term_with_context(Lang, Context, InstName)
    ;
        Inst = not_reached,
        Term = make_atom(Context, "not_reached")
    ).

:- func inst_test_results_to_term(prog_context, inst_test_results) = prog_term.

inst_test_results_to_term(Context, InstResults) = Term :-
    (
        InstResults = inst_test_results(GroundnessResult, AnyResult,
            InstNameResult, TypeResult),
        SubTerm1 = inst_result_groundness_to_term(Context, GroundnessResult),
        SubTerm2 = inst_result_contains_any_to_term(Context, AnyResult),
        SubTerm3 = inst_result_contains_instnames_to_term(Context,
            InstNameResult),
        SubTerm4 = inst_result_contains_types_to_term(Context, TypeResult),
        Term = term.functor(term.atom("results"),
            [SubTerm1, SubTerm2, SubTerm3, SubTerm4], Context)
    ;
        InstResults = inst_test_no_results,
        Term = term.functor(term.atom("no_results"), [], Context)
    ;
        InstResults = inst_test_results_fgtc,
        Term = term.functor(term.atom("fgtc"), [], Context)
    ).

:- func inst_result_groundness_to_term(prog_context, inst_result_groundness)
    = prog_term.

inst_result_groundness_to_term(Context, Groundness) = Term :-
    (
        Groundness = inst_result_is_not_ground,
        Term = term.functor(term.atom("is_not_ground"), [], Context)
    ;
        Groundness = inst_result_is_ground,
        Term = term.functor(term.atom("is_ground"), [], Context)
    ;
        Groundness = inst_result_groundness_unknown,
        Term = term.functor(term.atom("groundness_unknown"), [], Context)
    ).

:- func inst_result_contains_any_to_term(prog_context,
    inst_result_contains_any) = prog_term.

inst_result_contains_any_to_term(Context, ContainsAny) = Term :-
    (
        ContainsAny = inst_result_does_not_contain_any,
        Term = term.functor(term.atom("does_not_contain_any"), [], Context)
    ;
        ContainsAny = inst_result_does_contain_any,
        Term = term.functor(term.atom("does_contain_any"), [], Context)
    ;
        ContainsAny = inst_result_contains_any_unknown,
        Term = term.functor(term.atom("contains_any_unknown"), [], Context)
    ).

:- func inst_result_contains_instnames_to_term(prog_context,
    inst_result_contains_instnames) = prog_term.

inst_result_contains_instnames_to_term(Context, ContainsInstNames) = Term :-
    (
        ContainsInstNames = inst_result_contains_instnames_unknown,
        Term = term.functor(term.atom("contains_instnames_unknown"),
            [], Context)
    ;
        ContainsInstNames = inst_result_contains_instnames_known(InstNameSet),
        set.count(InstNameSet, NumInstNames),
        % Inst names can be pretty big, so we print only a count.
        % If necessary, we can later modify this code to actually print them.
        CountTerm = term.functor(term.integer(NumInstNames), [], Context),
        Term = term.functor(term.atom("contains_types_known"),
            [CountTerm], Context)
    ).

:- func inst_result_contains_types_to_term(prog_context,
    inst_result_contains_types) = prog_term.

inst_result_contains_types_to_term(Context, ContainsTypes) = Term :-
    (
        ContainsTypes = inst_result_contains_types_unknown,
        Term = term.functor(term.atom("contains_types_unknown"), [], Context)
    ;
        ContainsTypes = inst_result_contains_types_known(TypeCtorSet),
        set.to_sorted_list(TypeCtorSet, TypeCtors),
        TypeCtorTerms = list.map(type_ctor_to_term(Context), TypeCtors),
        Term = term.functor(term.atom("contains_types_known"),
            TypeCtorTerms, Context)
    ).

:- func type_ctor_to_term(prog_context, type_ctor) = prog_term.

type_ctor_to_term(Context, TypeCtor) = Term :-
    TypeCtor = type_ctor(SymName, Arity),
    string.format("%s/%d", [s(sym_name_to_string(SymName)), i(Arity)],
        ConsName),
    Term = term.functor(term.atom(ConsName), [], Context).

:- func ground_pred_inst_info_to_term(output_lang, prog_context, uniqueness,
    pred_inst_info) = prog_term.

ground_pred_inst_info_to_term(Lang, Context, _Uniq, PredInstInfo) = Term :-
    % XXX we ignore Uniq
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, _, Det),
    (
        PredOrFunc = pf_predicate,
        construct_qualified_term_with_context(unqualified("pred"),
            list.map(mode_to_term_with_context(Lang, Context), Modes),
            Context, ModesTerm)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        construct_qualified_term_with_context(unqualified("func"),
            list.map(mode_to_term_with_context(Lang, Context), ArgModes),
            Context, ArgModesTerm),
        construct_qualified_term_with_context(unqualified("="),
            [ArgModesTerm, mode_to_term_with_context(Lang, Context, RetMode)],
            Context, ModesTerm)
    ),
    construct_qualified_term_with_context(unqualified("is"),
        [ModesTerm, det_to_term(Context, Det)], Context, Term).

:- func any_pred_inst_info_to_term(output_lang, prog_context, uniqueness,
    pred_inst_info) = prog_term.

any_pred_inst_info_to_term(Lang, Context, _Uniq, PredInstInfo) = Term :-
    % XXX we ignore Uniq
    PredInstInfo = pred_inst_info(PredOrFunc, Modes, _, Det),
    (
        PredOrFunc = pf_predicate,
        construct_qualified_term_with_context(unqualified("any_pred"),
            list.map(mode_to_term_with_context(Lang, Context), Modes),
            Context, ModesTerm)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Modes, ArgModes, RetMode),
        construct_qualified_term_with_context(unqualified("any_func"),
            list.map(mode_to_term_with_context(Lang, Context), ArgModes),
            Context, ArgModesTerm),
        construct_qualified_term_with_context(unqualified("="),
            [ArgModesTerm, mode_to_term_with_context(Lang, Context, RetMode)],
            Context, ModesTerm)
    ),
    construct_qualified_term_with_context(unqualified("is"),
        [ModesTerm, det_to_term(Context, Det)], Context, Term).

inst_name_to_term(Lang, InstName) =
    inst_name_to_term_with_context(Lang, term.context_init, InstName).

:- func inst_name_to_term_with_context(output_lang, prog_context, inst_name)
    = prog_term.

inst_name_to_term_with_context(Lang, Context, InstName) = Term :-
    (
        InstName = user_inst(Name, Args),
        construct_qualified_term_with_context(Name,
            list.map(inst_to_term_with_context(Lang, Context), Args),
            Context, Term)
    ;
        InstName = merge_inst(InstA, InstB),
        construct_qualified_term_with_context(unqualified("$merge_inst"),
            list.map(inst_to_term_with_context(Lang, Context), [InstA, InstB]),
            Context, Term)
    ;
        InstName = shared_inst(SubInstName),
        construct_qualified_term_with_context(unqualified("$shared_inst"),
            [inst_name_to_term_with_context(Lang, Context, SubInstName)],
            Context, Term)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        construct_qualified_term_with_context(unqualified("$mostly_uniq_inst"),
            [inst_name_to_term_with_context(Lang, Context, SubInstName)],
            Context, Term)
    ;
        InstName = unify_inst(Liveness, InstA, InstB, Real),
        construct_qualified_term_with_context(unqualified("$unify"),
            [make_atom(Context, is_live_to_str(Liveness))] ++
            list.map(inst_to_term_with_context(Lang, Context),
                [InstA, InstB]) ++
            [make_atom(Context, unify_is_real_to_str(Real))],
            Context, Term)
    ;
        InstName = ground_inst(SubInstName, IsLive, Uniq, Real),
        construct_qualified_term_with_context(unqualified("$ground"),
            [inst_name_to_term_with_context(Lang, Context, SubInstName),
            make_atom(Context, is_live_to_str(IsLive)),
            make_atom(Context, inst_uniqueness(Uniq, "shared")),
            make_atom(Context, unify_is_real_to_str(Real))],
            Context, Term)
    ;
        InstName = any_inst(SubInstName, IsLive, Uniq, Real),
        construct_qualified_term_with_context(unqualified("$any"),
            [inst_name_to_term_with_context(Lang, Context, SubInstName),
            make_atom(Context, is_live_to_str(IsLive)),
            make_atom(Context, inst_uniqueness(Uniq, "shared")),
            make_atom(Context, unify_is_real_to_str(Real))],
            Context, Term)
    ;
        InstName = typed_ground(Uniq, Type),
        unparse_type(Type, Term0),
        construct_qualified_term_with_context(unqualified("$typed_ground"),
            [make_atom(Context, inst_uniqueness(Uniq, "shared")),
            term.coerce(Term0)],
            Context, Term)
    ;
        InstName = typed_inst(Type, SubInstName),
        unparse_type(Type, Term0),
        construct_qualified_term_with_context(unqualified("$typed_inst"),
            [term.coerce(Term0),
            inst_name_to_term_with_context(Lang, Context, SubInstName)],
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

:- func bound_insts_to_term(output_lang, prog_context, list(bound_inst))
    = prog_term.

bound_insts_to_term(_, Context, []) =
    % This shouldn't happen, but when it does, the problem is a LOT easier
    % to debug if there is a HLDS dump you can read.
    term.functor(term.atom("EMPTY_BOUND_INSTS"), [], Context).
bound_insts_to_term(Lang, Context, [BoundInst | BoundInsts]) =
    bound_insts_to_term_2(Lang, Context, BoundInst, BoundInsts).

:- func bound_insts_to_term_2(output_lang, prog_context,
    bound_inst, list(bound_inst)) = prog_term.

bound_insts_to_term_2(Lang, Context, BoundInst, BoundInsts) = Term :-
    BoundInst = bound_functor(ConsId, Args),
    ArgTerms = list.map(inst_to_term_with_context(Lang, Context), Args),
    cons_id_and_args_to_term_full(ConsId, ArgTerms, FirstTerm),
    (
        BoundInsts = [],
        Term = FirstTerm
    ;
        BoundInsts = [HeadBoundInst | TailBoundInsts],
        SecondTerm = bound_insts_to_term_2(Lang, Context,
            HeadBoundInst, TailBoundInsts),
        construct_qualified_term_with_context(unqualified(";"),
            [FirstTerm, SecondTerm], Context, Term)
    ).

:- pred cons_id_and_args_to_term_full(cons_id::in, list(prog_term)::in,
    prog_term::out) is det.

cons_id_and_args_to_term_full(ConsId, ArgTerms, Term) :-
    (
        ConsId = cons(SymName, _Arity, _TypeCtor),
        construct_qualified_term(SymName, ArgTerms, Term)
    ;
        ConsId = tuple_cons(_Arity),
        SymName = unqualified("{}"),
        construct_qualified_term(SymName, ArgTerms, Term)
    ;
        ConsId = closure_cons(_, _),
        term.context_init(Context),
        FunctorName = "closure_cons",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = int_const(Int),
        term.context_init(Context),
        Term = term.functor(term.integer(Int), [], Context)
    ;
        ConsId = float_const(Float),
        term.context_init(Context),
        Term = term.functor(term.float(Float), [], Context)
    ;
        ConsId = string_const(String),
        term.context_init(Context),
        Term = term.functor(term.string(String), [], Context)
    ;
        ConsId = char_const(Char),
        SymName = unqualified(string.from_char(Char)),
        construct_qualified_term(SymName, [], Term)
    ;
        ConsId = impl_defined_const(String),
        term.context_init(Context),
        FunctorName = "ImplDefinedConst: " ++ String,
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeCtorName, Arity),
        term.context_init(Context),
        string.format("TypeCtorInfo for %s.%s/%d",
            [s(sym_name_to_string(ModuleName)), s(TypeCtorName), i(Arity)],
            FunctorName),
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = base_typeclass_info_const(_, _, _, _),
        term.context_init(Context),
        FunctorName = "base_typeclass_info_const",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = type_info_cell_constructor(TypeCtor),
        TypeCtor = type_ctor(TypeCtorName, Arity),
        term.context_init(Context),
        string.format("type_info_cell_constructor for %s/%d",
            [s(sym_name_to_string(TypeCtorName)), i(Arity)], FunctorName),
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = typeclass_info_cell_constructor,
        term.context_init(Context),
        FunctorName = "typeclass_info_cell_constructor",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = type_info_const(TIConstNum),
        expect(unify(ArgTerms, []), $module, $pred,
            "type_info_const arity != 0"),
        term.context_init(Context),
        FunctorName = "type_info_const",
        Arg = term.functor(term.integer(TIConstNum), [], Context),
        Term = term.functor(term.string(FunctorName), [Arg], Context)
    ;
        ConsId = typeclass_info_const(TCIConstNum),
        expect(unify(ArgTerms, []), $module, $pred,
            "typeclass_info_const arity != 0"),
        term.context_init(Context),
        FunctorName = "typeclass_info_const",
        Arg = term.functor(term.integer(TCIConstNum), [], Context),
        Term = term.functor(term.string(FunctorName), [Arg], Context)
    ;
        ConsId = ground_term_const(TCIConstNum, SubConsId),
        expect(unify(ArgTerms, []), $module, $pred,
            "ground_term_const arity != 0"),
        cons_id_and_args_to_term_full(SubConsId, [], SubArg),
        term.context_init(Context),
        FunctorName = "ground_term_const",
        NumArg = term.functor(term.integer(TCIConstNum), [], Context),
        Term = term.functor(term.string(FunctorName), [NumArg, SubArg],
            Context)
    ;
        ConsId = tabling_info_const(_),
        term.context_init(Context),
        FunctorName = "tabling_info_const",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = table_io_entry_desc(_),
        term.context_init(Context),
        FunctorName = "table_io_entry_desc",
        Term = term.functor(term.string(FunctorName), [], Context)
    ;
        ConsId = deep_profiling_proc_layout(_),
        term.context_init(Context),
        FunctorName = "deep_profiling_proc_layout",
        Term = term.functor(term.string(FunctorName), [], Context)
    ).

:- func det_to_term(prog_context, determinism) = prog_term.

det_to_term(Context, Det) = make_atom(Context, det_to_string(Det)).

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

mercury_output_structured_uni_mode_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, !IO) :-
    mercury_format_structured_uni_mode_list(Insts, 1, Indent, Lang, InclAddr,
        InstVarSet, !IO).

mercury_structured_uni_mode_list_to_string(Insts, Indent, Lang, InclAddr,
        InstVarSet) = String :-
    mercury_format_structured_uni_mode_list(Insts, 1, Indent, Lang, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_uni_mode_list(list(uni_mode)::in, int::in,
    int::in, output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo)
    is det <= output(U).

mercury_format_structured_uni_mode_list([], _, _, _, _, _, !U).
mercury_format_structured_uni_mode_list([UniMode | UniModes], ArgNum, Indent,
        Lang, InclAddr, InstVarSet, !U) :-
    mercury_format_tabs(Indent, !U),
    add_string("argument ", !U),
    add_int(ArgNum, !U),
    add_string(":\n", !U),
    mercury_format_structured_uni_mode(UniMode, Indent,
        Lang, InclAddr, InstVarSet, !U),
    mercury_format_structured_uni_mode_list(UniModes, ArgNum +1, Indent,
        Lang, InclAddr, InstVarSet, !U).

%-----------------------------------------------------------------------------%

mercury_output_structured_uni_mode(Inst, Indent, Lang, InclAddr,
        InstVarSet, !IO) :-
    mercury_format_structured_uni_mode(Inst, Indent, Lang, InclAddr,
        InstVarSet, !IO).

mercury_structured_uni_mode_to_string(Inst, Indent, Lang, InclAddr,
        InstVarSet) = String :-
    mercury_format_structured_uni_mode(Inst, Indent, Lang, InclAddr,
        InstVarSet, "", String).

:- pred mercury_format_structured_uni_mode(uni_mode::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_uni_mode(UniMode, Indent, Lang, InclAddr,
        InstVarSet, !U) :-
    UniMode = (InstA1 - InstB1 -> InstA2 - InstB2),
    get_inst_addr(InstA1, InstA1Addr),
    get_inst_addr(InstA2, InstA2Addr),
    get_inst_addr(InstB1, InstB1Addr),
    get_inst_addr(InstB2, InstB2Addr),

    mercury_format_tabs(Indent, !U),
    add_string("old lhs inst:\n", !U),
    mercury_format_structured_inst(InstA1, Indent, Lang, InclAddr,
        InstVarSet, !U),

    mercury_format_tabs(Indent, !U),
    ( InstB1Addr = InstA1Addr ->
        % We have printed the old lhs inst.
        add_string("old rhs inst: same as old lhs inst\n", !U)
    ;
        add_string("old rhs inst:\n", !U),
        mercury_format_structured_inst(InstB1, Indent, Lang, InclAddr,
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
        mercury_format_structured_inst(InstA2, Indent, Lang, InclAddr,
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
        mercury_format_structured_inst(InstB2, Indent, Lang, InclAddr,
            InstVarSet, !U)
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
    mercury_format_structured_inst(Inst, Indent, Lang, InclAddr,
        InstVarSet, !U),
    mercury_format_structured_inst_list(Insts, Indent, Lang, InclAddr,
        InstVarSet, !U).

%-----------------------------------------------------------------------------%

mercury_output_structured_inst(Inst, Indent, Lang, InclAddr, InstVarSet, !U) :-
    mercury_format_structured_inst(Inst, Indent, Lang, InclAddr, InstVarSet,
        !U).

mercury_structured_inst_to_string(Inst, Indent, Lang, InclAddr, InstVarSet)
        = String :-
    mercury_format_structured_inst(Inst, Indent, Lang, InclAddr, InstVarSet,
        "", String).

:- pred mercury_format_structured_inst(mer_inst::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_inst(Inst, Indent, Lang, InclAddr, InstVarSet, !U) :-
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
        Inst = bound(Uniq, InstResults, BoundInsts),
        mercury_format_uniqueness(Uniq, "bound", !U),
        add_string("(\n", !U),
        (
            Lang = output_mercury
        ;
            Lang = output_debug,
            InstResultsTerm =
                inst_test_results_to_term(term.context_init, InstResults),
            InstResultsStr =
                mercury_term_to_string(varset.init, no, InstResultsTerm),
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
        mercury_format_structured_inst_name(user_inst(Name, Args), yes, Indent,
            Lang, InclAddr, InstVarSet, !U)
    ;
        Inst = defined_inst(InstName),
        mercury_format_structured_inst_name(InstName, yes, Indent,
            Lang, InclAddr, InstVarSet, !U)
    ;
        Inst = not_reached,
        add_string("not_reached\n", !U)
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

:- pred mercury_format_structured_inst_name(inst_name::in, bool::in, int::in,
    output_lang::in, incl_addr::in, inst_varset::in, U::di, U::uo) is det
    <= output(U).

mercury_format_structured_inst_name(InstName, FirstIndentPrinted, Indent,
        Lang, InclAddr, InstVarSet, !U) :-
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
            mercury_format_bracketed_sym_name(Name, !U),
            add_string("\n", !U)
        ;
            Args = [_ | _],
            mercury_format_sym_name(Name, !U),
            add_string("(\n", !U),
            mercury_format_structured_inst_list(Args, Indent + 1,
                Lang, InclAddr, InstVarSet, !U),
            mercury_format_tabs(Indent, !U),
            add_string(")\n", !U)
        )
    ;
        InstName = merge_inst(InstA, InstB),
        add_string("$merge_inst(\n", !U),
        mercury_format_structured_inst_list([InstA, InstB], Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = shared_inst(SubInstName),
        add_string("$shared_inst(\n", !U),
        mercury_format_structured_inst_name(SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = mostly_uniq_inst(SubInstName),
        add_string("$mostly_uniq_inst(\n", !U),
        mercury_format_structured_inst_name(SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = unify_inst(IsLive, InstA, InstB, Real),
        add_string("$unify(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        add_string("\n", !U),
        mercury_format_structured_inst_list([InstA, InstB], Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = ground_inst(SubInstName, IsLive, Uniq, Real),
        add_string("$ground(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        mercury_format_uniqueness(Uniq, "shared", !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name(SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = any_inst(SubInstName, IsLive, Uniq, Real),
        add_string("$any(", !U),
        mercury_format_is_live_comma(IsLive, !U),
        mercury_format_real_comma(Real, !U),
        mercury_format_uniqueness(Uniq, "shared", !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name(SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
        mercury_format_tabs(Indent, !U),
        add_string(")\n", !U)
    ;
        InstName = typed_ground(Uniqueness, Type),
        add_string("$typed_ground(", !U),
        mercury_format_uniqueness(Uniqueness, "shared", !U),
        add_string(", ", !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, no, Type, !U),
        add_string(")\n", !U)
    ;
        InstName = typed_inst(Type, SubInstName),
        add_string("$typed_inst(", !U),
        varset.init(TypeVarSet),
        mercury_format_type(TypeVarSet, no, Type, !U),
        add_string(",\n", !U),
        mercury_format_structured_inst_name(SubInstName, no, Indent + 1,
            Lang, InclAddr, InstVarSet, !U),
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
