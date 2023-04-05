%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2015, 2017-2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: du_type_layout.m.
% Main author: zs.
%
% The task of this module is decide the representation of each type.
% Once that is done, it will also invoke add_special_pred.m to declare
% and (if necessary) define the unify and compare predicates for each type.
%
% We decide representations in two passes.
%
% - The purpose of the first pass is to gather information about all types
%   that may be useful when those types occur as the arguments of function
%   symbols (whether in other types or in the same type).
%
%   However, some kinds of types are so simple that we never need such
%   information to decide their representations, and so we make those decisions
%   in the first pass. These simple types are:
%
%   - dummy types, types with one constructor of arity 0;
%   - enum types, types whose two or more constructors all have arity 0; and
%   - notag types, types with one constructor of arity 1 with no constraints.
%
%   The dummy and notag types have no function symbols with arguments
%   (and thus cannot have any existential constraints either).
%   For notag types, their representation is the same as the representation
%   of their single function symbol's argument type, whatever that happens
%   to be, and we need to record only the fact of the equivalence, not the
%   final representation type itself.
%
% - The second pass decides the representation of all other types.
%   For this, it uses two kinds of information gathered by the first pass:
%
%   - which types are representable in sub-word-size chunks of bits, and
%   - which types are guaranteed to be represented as a word-aligned pointers.
%
%   We use the first kind of information for packing arguments tightly
%   together, and we use the second kind to help decide which function symbols
%   we can apply the direct arg optimization to.
%
%---------------------------------------------------------------------------%
%
% Originally, I (zs) intended to run the du_type_layout pass
% after the whole semantic analysis phase of the compiler, i.e. after type,
% mode, and determinism checking and simplification. My reason for this,
% as discussed on m-dev in late october 2017, was to try to ensure that
% semantic analysis does not have access to, and thus cannot depend on,
% type representations, which are implementation-level details, and not
% a user-visible aspect of the program's semantics.
%
% I found out the hard way that this won't work, at least not without
% a significant amount of otherwise-unnecessary code duplication.
%
% The ultimate reason for this is that the code we want to generate
% for a type's unify and compare predicates depends on the type's
% representation. Consider a type such as
%
%     :- type t
%         --->    f(bool, bool, bool bool).
%
% One future packing optimization should allow us to represent values
% of this type as four bits in a register (no heap cell needed),
% If we can do that, then unifying two values X and Y of this type
% should be done by unsafe-casting both to int, and comparing the ints.
% If we cannot, then we should generate the usual code deconstructing
% both X and Y, and comparing the arguments (one by one or all together,
% depending on how well we can pack the heap cell's contents).
%
% Deciding type representations after semantic analysis would therefore
% require us to generate the unify and compare (and maybe index) predicates
% of each type late as well, but then (since the clauses we generate are NOT
% guaranteed to include all the right types, modes and other annotations)
% we would have to repeat semantic analysis just on these late-auto-generated
% predicates. This would not be too hard with some semantic analysis passes
% (e.g. determinism analysis) that are already effectively done
% predicate-by-predicate, but *would* require large amounts of new work
% for the polymorphism pass, which currently works on the whole module at once.
%
% To minimize this problem, we execute the du_type_layout pass directly after
% the make_hlds pass, i.e. before all the semantic analysis passes.
%
%---------------------------------------------------------------------------%
% XXX TYPE_REPN: Record contexts in the type table, and use them to replace
% all the term.context_inits in error messages below.
%---------------------------------------------------------------------------%

:- module hlds.du_type_layout.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

:- pred decide_type_repns(module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.add_foreign_enum.
:- import_module hlds.add_special_pred.
:- import_module hlds.hlds_data.
:- import_module hlds.passes_aux.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.      % undesirable dependency
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module pretty_printer.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.
:- import_module uint.
:- import_module uint32.
:- import_module uint8.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

decide_type_repns(!ModuleInfo, !Specs, !IO) :-
    module_info_get_type_repn_dec(!.ModuleInfo, TypeRepnDec),
    TypeRepnDec = type_repn_decision_data(_TypeRepns, _DirectArgMap,
        _ForeignEnums, ForeignExportEnums),

    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    get_all_type_ctor_defns(TypeTable0, TypeCtorsTypeDefns0),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, experiment2, Experiment2),
    (
        Experiment2 = no,
        decide_type_repns_old(!.ModuleInfo, TypeRepnDec, TypeTable0,
            TypeCtorsTypeDefns0, TypeCtorsTypeDefns, NoTagTypeMap, !Specs),
        set_all_type_ctor_defns(TypeCtorsTypeDefns,
            SortedTypeCtorsTypeDefns, TypeTable)
    ;
        Experiment2 = yes,
        decide_type_repns_old(!.ModuleInfo, TypeRepnDec, TypeTable0,
            TypeCtorsTypeDefns0, TypeCtorsTypeDefns, NoTagTypeMap, !Specs),
        set_all_type_ctor_defns(TypeCtorsTypeDefns,
            SortedTypeCtorsTypeDefns, TypeTable),
        module_info_get_name(!.ModuleInfo, ModuleName),
        decide_type_repns_new(Globals, ModuleName, TypeRepnDec,
            TypeCtorsTypeDefns0, TypeCtorsTypeDefnsB,
            NoTagTypeMapB, UnRepnTypeCtors, BadRepnTypeCtors, !Specs),
        set_all_type_ctor_defns(TypeCtorsTypeDefnsB,
            SortedTypeCtorsTypeDefnsB, _TypeTableB),
        get_debug_output_stream(!.ModuleInfo, DebugStream, !IO),
        compare_old_new_du(DebugStream, UnRepnTypeCtors, BadRepnTypeCtors,
            SortedTypeCtorsTypeDefns, SortedTypeCtorsTypeDefnsB, !IO),
        compare_old_new_notag(DebugStream, NoTagTypeMap, NoTagTypeMapB, !IO)
    ),

    module_info_set_type_table(TypeTable, !ModuleInfo),
    module_info_set_no_tag_types(NoTagTypeMap, !ModuleInfo),

    add_special_pred_decl_defns_for_types_maybe_lazily(
        SortedTypeCtorsTypeDefns, !ModuleInfo),

    list.foldl2(add_pragma_foreign_export_enum, ForeignExportEnums,
        !ModuleInfo, !Specs),

    maybe_show_type_repns(!.ModuleInfo, SortedTypeCtorsTypeDefns, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type repn_target
    --->    repn_target_c(c_repn_target)
    ;       repn_target_java
    ;       repn_target_csharp.

:- type c_repn_target
    --->    c_repn_target(word_size, c_repn_spf, c_repn_allow_direct_arg).

:- type c_repn_spf
    --->    c_no_single_prec_float
    ;       c_single_prec_float.

:- type c_repn_allow_direct_arg
    --->    c_do_not_allow_direct_arg
    ;       c_allow_direct_arg.

:- pred decide_type_repns_new(globals::in, module_name::in,
    type_repn_decision_data::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out,
    no_tag_type_table::out, set(type_ctor)::out, set(type_ctor)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_type_repns_new(Globals, ModuleName, TypeRepnDec, !TypeCtorsTypeDefns,
        NoTagTypeMap, UnRepnTypeCtors, BadRepnTypeCtors, !Specs) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.get_word_size(Globals, WordSize),
        globals.lookup_bool_option(Globals, single_prec_float, OptSPF),
        globals.lookup_bool_option(Globals, allow_direct_args, OptDA),
        ( OptSPF = no,  SPF = c_no_single_prec_float
        ; OptSPF = yes, SPF = c_single_prec_float
        ),
        ( OptDA = no,  DA = c_do_not_allow_direct_arg
        ; OptDA = yes, DA = c_allow_direct_arg
        ),
        RepnTarget = repn_target_c(c_repn_target(WordSize, SPF, DA))
    ;
        Target = target_java,
        RepnTarget = repn_target_java
    ;
        Target = target_csharp,
        RepnTarget = repn_target_csharp
    ),
    TypeRepnDec = type_repn_decision_data(TypeRepns,
        _DirectArgMap, _ForeignEnums, _ForeignExportEnums),
    list.map_foldl4(
        fill_in_non_sub_type_repn(Globals, ModuleName, RepnTarget, TypeRepns),
        !TypeCtorsTypeDefns, map.init, NoTagTypeMap,
        set.init, UnRepnTypeCtors, set.init, BadRepnTypeCtors, !Specs).
    % XXX TYPE_REPN fill_in_type_repn_sub

%---------------------------------------------------------------------------%

:- pred fill_in_non_sub_type_repn(globals::in, module_name::in,
    repn_target::in, type_ctor_repn_map::in,
    pair(type_ctor, hlds_type_defn)::in,
    pair(type_ctor, hlds_type_defn)::out,
    no_tag_type_table::in, no_tag_type_table::out,
    set(type_ctor)::in, set(type_ctor)::out,
    set(type_ctor)::in, set(type_ctor)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

fill_in_non_sub_type_repn(Globals, ModuleName, RepnTarget, TypeCtorRepnMap,
        TypeCtorTypeDefn0, TypeCtorTypeDefn,
        !NoTagTypeMap, !UnRepnTypeCtors, !BadRepnTypeCtors, !Specs) :-
    TypeCtorTypeDefn0 = TypeCtor - TypeDefn0,
    TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
    ( if map.search(TypeCtorRepnMap, TypeCtor, ItemTypeRepn) then
        ItemTypeRepn = item_type_repn_info(RepnSymName, RepnTypeParams,
            RepnInfo, RepnTVarSet, RepnContext, _RepnSeqNum),
        list.length(RepnTypeParams, NumRepnTypeParams),
        expect(unify(TypeCtorSymName, RepnSymName), $pred,
            "TypeCtorSymName != RepnSymName"),
        expect(unify(TypeCtorArity, NumRepnTypeParams), $pred,
            "TypeCtorArity != NumRepnTypeParams"),
        get_type_defn_body(TypeDefn0, Body0),
        (
            Body0 = hlds_du_type(BodyDu0),
            BodyDu0 = type_body_du(Ctors, MaybeSuperType, MaybeCanon,
                _, _MaybeForeignTypeBody),
            (
                (
                    RepnInfo = tcrepn_is_word_aligned_ptr,
                    RepnStr = "is_word_aligned_ptr"
                ;
                    RepnInfo = tcrepn_is_eqv_to(_),
                    RepnStr = "is_eqv_to"
                ;
                    RepnInfo = tcrepn_foreign(_),
                    RepnStr = "foreign"
                ),
                Pieces = [words("Error: unexpected"), words(RepnStr),
                    words("type_repn item for"), qual_type_ctor(TypeCtor),
                    suffix("."), nl],
                Spec = simplest_spec($pred, severity_error, phase_type_repn,
                    RepnContext, Pieces),
                !:Specs = [Spec | !.Specs],
                set.insert(TypeCtor, !BadRepnTypeCtors),
                % The presence of Spec should prevent TypeCtorTypeDefn
                % from being used.
                TypeCtorTypeDefn = TypeCtorTypeDefn0
            ;
                RepnInfo = tcrepn_du(DuRepnInfo),
                (
                    MaybeSuperType = subtype_of(_),
                    % There should NOT be an tcrepn_du ItemTypeRepn for
                    % a subtype, since subtypes take their representation
                    % information from their (ultimate) supertype.
                    Pieces = [words("Error: tcrepn_du type_repn item"),
                        words("for subtype"), qual_type_ctor(TypeCtor),
                        suffix("."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_type_repn, RepnContext, Pieces),
                    !:Specs = [Spec | !.Specs],
                    set.insert(TypeCtor, !BadRepnTypeCtors),
                    % The presence of Spec should prevent TypeCtorTypeDefn
                    % from being used.
                    TypeCtorTypeDefn = TypeCtorTypeDefn0
                ;
                    MaybeSuperType = not_a_subtype,
                    fill_in_non_sub_du_type_repn(Globals, ModuleName,
                        RepnTarget, TypeCtor, RepnTVarSet, RepnContext,
                        Ctors, MaybeCanon, DuRepnInfo, MaybeDuRepn),
                    % XXX TYPE_REPN Sanity check _MaybeForeignTypeBody
                    % against the MaybeDuRepn we construct, in case
                    % exactly one of the two represents a foreign type.
                    (
                        MaybeDuRepn = have_du_type_repn(DuRepn),
                        BodyDu = BodyDu0 ^ du_type_repn := yes(DuRepn),
                        Body = hlds_du_type(BodyDu),
                        set_type_defn_body(Body, TypeDefn0, TypeDefn),
                        TypeCtorTypeDefn = TypeCtor - TypeDefn,
                        ( if
                            DuRepn = du_type_repn(_, _, _, DuTypeKind, _),
                            DuTypeKind = du_type_kind_notag(CtorSymName,
                                RepnArgType, _MaybeArgName)
                        then
                            get_type_defn_tparams(TypeDefn0, TypeParams),
                            NoTagEntry = no_tag_type(TypeParams, CtorSymName,
                                RepnArgType),
                            map.det_insert(TypeCtor, NoTagEntry, !NoTagTypeMap)
                        else
                            true
                        )
                    ;
                        MaybeDuRepn = have_foreign_type_repn(ForeignTypeBody),
                        Body = hlds_foreign_type(ForeignTypeBody),
                        set_type_defn_body(Body, TypeDefn0, TypeDefn),
                        TypeCtorTypeDefn = TypeCtor - TypeDefn
                    ;
                        MaybeDuRepn = have_errors(CheckSpecs),
                        !:Specs = CheckSpecs ++ !.Specs,
                        % The presence of CheckSpecs should prevent
                        % TypeCtorTypeDefn from being used.
                        TypeCtorTypeDefn = TypeCtorTypeDefn0
                        % XXX TYPE_REPN Record TypeCtor's module as
                        % needing to have its .int file rebuilt.
                    )
                )
            ;
                RepnInfo = tcrepn_is_subtype_of(_),
                (
                    MaybeSuperType = subtype_of(_),
                    % XXX TYPE_REPN Record TypeCtor as a subtype
                    % needing a second pass.
                    TypeCtorTypeDefn = TypeCtorTypeDefn0
                ;
                    MaybeSuperType = not_a_subtype,
                    Pieces = [words("Error: tcrepn_is_subtype_of"),
                        words("type_repn item for"), qual_type_ctor(TypeCtor),
                        suffix(","), words("which is NOT a subtype."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_type_repn, RepnContext, Pieces),
                    !:Specs = [Spec | !.Specs],
                    set.insert(TypeCtor, !BadRepnTypeCtors),
                    % The presence of Spec should prevent TypeCtorTypeDefn
                    % from being used.
                    TypeCtorTypeDefn = TypeCtorTypeDefn0
                )
            )
        ;
            ( Body0 = hlds_eqv_type(_)
            ; Body0 = hlds_foreign_type(_)
            ; Body0 = hlds_abstract_type(_)
            ; Body0 = hlds_solver_type(_)
            ),
            TypeCtorTypeDefn = TypeCtorTypeDefn0
        )
    else
        TypeCtorTypeDefn = TypeCtor - TypeDefn0,
        get_type_defn_body(TypeDefn0, Body0),
        (
            ( Body0 = hlds_du_type(_)
            ; Body0 = hlds_eqv_type(_)
            ; Body0 = hlds_foreign_type(_)
            ),
            set.insert(TypeCtor, !UnRepnTypeCtors),
            trace [runtime(env("PRINT_UNDEF_TYPE_CTOR")), io(!IO)] (
                get_debug_output_stream(Globals, ModuleName, DebugStream, !IO),
                map.keys(TypeCtorRepnMap, TypeCtorRepnMapKeys),
                io.write_string(DebugStream, "NOREPN ", !IO),
                io.write_line(DebugStream, TypeCtor, !IO),
                io.write_line(DebugStream, Body0, !IO),
                list.foldl(io.write_line(DebugStream),
                    TypeCtorRepnMapKeys, !IO)
            )
        ;
            Body0 = hlds_abstract_type(DetailsAbstract0),
            (
                ( DetailsAbstract0 = abstract_type_general
                ; DetailsAbstract0 = abstract_type_fits_in_n_bits(_)
                ; DetailsAbstract0 = abstract_dummy_type
                ; DetailsAbstract0 = abstract_notag_type
                ),
                set.insert(TypeCtor, !UnRepnTypeCtors),
                trace [runtime(env("PRINT_UNDEF_TYPE_CTOR")), io(!IO)] (
                    get_debug_output_stream(Globals, ModuleName,
                        DebugStream, !IO),
                    io.write_string(DebugStream, "NOREPN ", !IO),
                    io.write_line(DebugStream, TypeCtor, !IO),
                    io.write_line(DebugStream, Body0, !IO)
                )
            ;
                DetailsAbstract0 = abstract_solver_type
                % Solver types, abstract or not, do not need a representation.
            ;
                DetailsAbstract0 = abstract_subtype(_),
                % XXX TYPE_REPN Not yet implemented.
                unexpected($pred, "abstract_subtype")
            )
        ;
            Body0 = hlds_solver_type(_)
            % Solver types, abstract or not, do not need a representation.
        )
    ).

%---------------------%

:- type maybe_du_type_repn
    --->    have_du_type_repn(du_type_repn)
    ;       have_foreign_type_repn(foreign_type_body)
    ;       have_errors(list(error_spec)).

%---------------------%

:- pred fill_in_non_sub_du_type_repn(globals::in, module_name::in,
    repn_target::in, type_ctor::in, tvarset::in, prog_context::in,
    one_or_more(constructor)::in, maybe_canonical::in, du_repn::in,
    maybe_du_type_repn::out) is det.

% XXX TYPE_REPN _Globals and _ModuleName are available in case
% we need get_debug_output_stream.
% XXX TYPE_REPN _RepnTVarSet is available in case
% we need it for formatting stuff for error messages.
fill_in_non_sub_du_type_repn(_Globals, _ModuleName, RepnTarget, TypeCtor,
        _RepnTVarSet, RepnContext, OoMCtors, MaybeCanon, DuRepnInfo,
        MaybeDuRepn) :-
    (
        (
            DuRepnInfo = dur_direct_dummy(DirectDummyRepn),
            check_and_record_du_direct_dummy(TypeCtor, RepnContext, OoMCtors,
                MaybeCanon, DirectDummyRepn, MercuryMaybeDuRepn),
            DirectDummyRepn = direct_dummy_repn(_RepnCtorName, MaybeCJCsEnum)
        ;
            DuRepnInfo = dur_enum(EnumRepn),
            check_and_record_du_enum(TypeCtor, RepnContext, OoMCtors,
                EnumRepn, MercuryMaybeDuRepn),
            EnumRepn = enum_repn(_RepnHeadCtorName, _RepnHeadTailCtorName,
                _RepnTailTailCtorNames, MaybeCJCsEnum)
        ),
        foreign_target_specific_repn(RepnTarget, MaybeCJCsEnum,
            ForeignLang, MaybeForeignEnum),
        (
            MaybeForeignEnum = yes(ForeignEnumRepn),
            (
                ForeignEnumRepn = enum_foreign_type(ForeignTypeRepn),
                ForeignTypeRepn =
                    foreign_type_repn(ForeignTypeName, Assertions),
                record_foreign_type_for_target(RepnTarget, ForeignTypeName,
                    MaybeCanon, Assertions, ForeignTypeBody),
                ForeignMaybeDuRepn = have_foreign_type_repn(ForeignTypeBody)
            ;
                ForeignEnumRepn = enum_foreign_enum(OoMForeignNames),
                Ctors = one_or_more_to_list(OoMCtors),
                ForeignNames = one_or_more_to_list(OoMForeignNames),
                record_foreign_enums(ForeignLang, ForeignNames, Ctors,
                    CtorRepns, map.init, CtorNameRepnMap, Lengths),
                (
                    Lengths = enum_list_lengths_match,
                    MaybeDirectArgFunctors = maybe.no,
                    DuRepn = du_type_repn(CtorRepns, CtorNameRepnMap,
                        no_cheaper_tag_test,
                        du_type_kind_foreign_enum(ForeignLang),
                        MaybeDirectArgFunctors),
                    ForeignMaybeDuRepn = have_du_type_repn(DuRepn)
                ;
                    Lengths = enum_list_lengths_do_not_match,
                    list.length(Ctors, NumCtors),
                    list.length(ForeignNames, NumForeignNames),
                    LangStr = foreign_language_string(ForeignLang),
                    Pieces = [words("Error in tcrepn_du type_repn item"),
                        words("for"), qual_type_ctor(TypeCtor), suffix(":"),
                        words("this enum type has"), int_fixed(NumCtors),
                        words("functors, but the"), words(LangStr),
                        words("foreign enum definition for it has"),
                        int_fixed(NumForeignNames), suffix("."), nl],
                    Spec = simplest_spec($pred, severity_error,
                        phase_type_repn, RepnContext, Pieces),
                    ForeignMaybeDuRepn = have_errors([Spec])
                )
            ),
            combine_mercury_foreign_du_repns(MercuryMaybeDuRepn,
                ForeignMaybeDuRepn, MaybeDuRepn)
        ;
            MaybeForeignEnum = no,
            MaybeDuRepn = MercuryMaybeDuRepn
        )
    ;
        DuRepnInfo = dur_notag(NoTagRepn),
        check_and_record_du_notag(TypeCtor, RepnContext, OoMCtors, MaybeCanon,
            NoTagRepn, MercuryMaybeDuRepn),
        NoTagRepn = notag_repn(_FunctorName, _ArgType, MaybeCJCs),
        foreign_target_specific_repn(RepnTarget, MaybeCJCs,
            _ForeignLang, MaybeForeign),
        (
            MaybeForeign = yes(ForeignTypeRepn),
            ForeignTypeRepn =
                foreign_type_repn(ForeignTypeName, Assertions),
            record_foreign_type_for_target(RepnTarget, ForeignTypeName,
                MaybeCanon, Assertions, ForeignTypeBody),
            ForeignMaybeDuRepn = have_foreign_type_repn(ForeignTypeBody),
            combine_mercury_foreign_du_repns(MercuryMaybeDuRepn,
                ForeignMaybeDuRepn, MaybeDuRepn)
        ;
            MaybeForeign = no,
            MaybeDuRepn = MercuryMaybeDuRepn
        )
    ;
        DuRepnInfo = dur_gen_only_functor(OnlyFunctorRepn),
        check_and_record_du_only_functor(RepnTarget, TypeCtor,
            RepnContext, OoMCtors, OnlyFunctorRepn, MaybeDuRepn)
    ;
        DuRepnInfo = dur_gen_more_functors(MoreFunctorsRepn),
        check_and_record_du_more_functors(RepnTarget, TypeCtor,
            RepnContext, OoMCtors, MoreFunctorsRepn, MaybeDuRepn)
    ).

:- pred combine_mercury_foreign_du_repns(maybe_du_type_repn::in,
    maybe_du_type_repn::in, maybe_du_type_repn::out) is det.

combine_mercury_foreign_du_repns(MerMaybeDuRepn, ForMaybeDuRepn,
        MaybeDuRepn) :-
    % Return error if EITHER the Mercury OR the foreign definition has errors.
    % If NEITHER has any errors, return the foreign definition, which overrides
    % the Mercury definition for the current target.
    ( if MerMaybeDuRepn = have_errors(MS) then MSpecs = MS else MSpecs = [] ),
    ( if ForMaybeDuRepn = have_errors(FS) then FSpecs = FS else FSpecs = [] ),
    MFSpecs = MSpecs ++ FSpecs,
    (
        MFSpecs = [_ | _],
        MaybeDuRepn = have_errors(MFSpecs)
    ;
        MFSpecs = [],
        MaybeDuRepn = ForMaybeDuRepn
    ).

%---------------------%

:- pred check_and_record_du_direct_dummy(type_ctor::in, prog_context::in,
    one_or_more(constructor)::in, maybe_canonical::in,
    direct_dummy_repn::in, maybe_du_type_repn::out) is det.

check_and_record_du_direct_dummy(TypeCtor, Context, OoMCtors, MaybeCanon,
        DirectDummyRepn, MaybeDuRepn) :-
    DirectDummyRepn = direct_dummy_repn(RepnCtorName, _MaybeCJCsEnum),
    ( if
        OoMCtors = one_or_more(Ctor, []),
        ctor_is_constant(Ctor, CtorName),
        CtorName = RepnCtorName
    then
        (
            MaybeCanon = noncanon(_),
            Pieces = [words("Error: the type representation item for"),
                qual_type_ctor(TypeCtor), words("says it is direct_dummy,"),
                words("but the type is noncanonical."), nl],
            Spec = simplest_spec($pred, severity_error, phase_type_repn,
                Context, Pieces),
            MaybeDuRepn = have_errors([Spec])
        ;
            MaybeCanon = canon,
            Ctor = ctor(Ordinal, _MaybeExistConstraints, CtorSymName,
                _CtorArgs, _NumCtorArgs, CtorContext),
            CtorTag = dummy_tag,
            CtorRepn = ctor_repn(Ordinal, no_exist_constraints, CtorSymName,
                CtorTag, [], 0, CtorContext),
            insert_ctor_repn_into_map(CtorRepn, map.init, CtorNameRepnMap),
            MaybeDirectArgFunctors = maybe.no,
            DuRepn = du_type_repn([CtorRepn], CtorNameRepnMap,
                no_cheaper_tag_test, du_type_kind_direct_dummy,
                MaybeDirectArgFunctors),
            MaybeDuRepn = have_du_type_repn(DuRepn)
        )
    else
        Pieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect: it says that"),
            words("the type has a single function symbol,"),
            words("a constant named"), quote(RepnCtorName), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_repn,
            Context, Pieces),
        MaybeDuRepn = have_errors([Spec])
    ).

%---------------------%

:- pred check_and_record_du_enum(type_ctor::in, prog_context::in,
    one_or_more(constructor)::in, enum_repn::in,
    maybe_du_type_repn::out) is det.

check_and_record_du_enum(TypeCtor, Context, Ctors, EnumRepn,
        MaybeDuRepn) :-
    EnumRepn = enum_repn(RepnHeadCtorName, RepnHeadTailCtorName,
        RepnTailTailCtorNames, _MaybeCJCs),
    ( if
        Ctors = one_or_more(HeadCtor, TailCtors),
        TailCtors = [HeadTailCtor | TailTailCtors],
        ctor_is_constant(HeadCtor, HeadCtorName),
        ctor_is_constant(HeadTailCtor, HeadTailCtorName),
        list.map(ctor_is_constant, TailTailCtors, TailTailCtorNames),
        HeadCtorName = RepnHeadCtorName,
        HeadTailCtorName = RepnHeadTailCtorName,
        TailTailCtorNames = RepnTailTailCtorNames
    then
        map.init(CtorNameRepnMap0),
        record_enum_repn(HeadCtor, HeadCtorRepn,
            0u32, _, CtorNameRepnMap0, CtorNameRepnMap1),
        record_enum_repn(HeadTailCtor, HeadTailCtorRepn,
            1u32, _, CtorNameRepnMap1, CtorNameRepnMap2),
        list.map_foldl2(record_enum_repn,
            TailTailCtors, TailTailCtorRepns,
            2u32, _, CtorNameRepnMap2, CtorNameRepnMap),
        CtorRepns = [HeadCtorRepn, HeadTailCtorRepn | TailTailCtorRepns],
        MaybeDirectArgFunctors = maybe.no,
        DuRepn = du_type_repn(CtorRepns, CtorNameRepnMap,
            no_cheaper_tag_test, du_type_kind_mercury_enum,
            MaybeDirectArgFunctors),
        MaybeDuRepn = have_du_type_repn(DuRepn)
    else
        RepnCtorNames = [RepnHeadCtorName, RepnHeadTailCtorName |
            RepnTailTailCtorNames],
        Pieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect: it says that"),
            words("the type is an enum, with function symbols named")] ++
            list_to_pieces(RepnCtorNames) ++ [suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_repn,
            Context, Pieces),
        MaybeDuRepn = have_errors([Spec])
    ).

:- pred record_enum_repn(constructor::in, constructor_repn::out,
    uint32::in, uint32::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

record_enum_repn(Ctor, CtorRepn, !ExpectedOrdinal, !CtorNameRepnMap) :-
    Ctor = ctor(Ordinal, _, SymName, _, _, Context),
    CtorTag = int_tag(int_tag_int(uint32.cast_to_int(Ordinal))),
    CtorRepn = ctor_repn(Ordinal, no_exist_constraints, SymName, CtorTag,
        [], 0, Context),
    expect(unify(Ordinal, !.ExpectedOrdinal), $pred,
        "Ordinal != !.ExpectedOrdinal"),
    !:ExpectedOrdinal = !.ExpectedOrdinal + 1u32,
    insert_ctor_repn_into_map(CtorRepn, !CtorNameRepnMap).

:- type enum_list_lengths
    --->    enum_list_lengths_do_not_match
    ;       enum_list_lengths_match.

:- pred record_foreign_enums(foreign_language::in, list(string)::in,
    list(constructor)::in, list(constructor_repn)::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out,
    enum_list_lengths::out) is det.

record_foreign_enums(_, [], [], [], !CtorNameRepnMap, enum_list_lengths_match).
record_foreign_enums(_, [], [_ | _], [], !CtorNameRepnMap,
        enum_list_lengths_do_not_match).
record_foreign_enums(_, [_ | _], [], [], !CtorNameRepnMap,
        enum_list_lengths_do_not_match).
record_foreign_enums(Lang, [ForeignName | ForeignNames], [Ctor | Ctors],
        [CtorRepn | CtorRepns], !CtorNameRepnMap, Lengths) :-
    % If the Ctor is no appropriate for an enum type, we will have detected
    % and reported this fact when we processed the Mercury representation.
    Ctor = ctor(Ordinal, _MaybeExistConstraints, CtorSymName,
        _CtorArgs, _NumCtorArgs, CtorContext),
    CtorTag = foreign_tag(Lang, ForeignName),
    CtorRepn = ctor_repn(Ordinal, no_exist_constraints, CtorSymName, CtorTag,
        [], 0, CtorContext),
    insert_ctor_repn_into_map(CtorRepn, !CtorNameRepnMap),
    record_foreign_enums(Lang, ForeignNames, Ctors, CtorRepns,
        !CtorNameRepnMap, Lengths).

%---------------------%

:- pred check_and_record_du_notag(type_ctor::in, prog_context::in,
    one_or_more(constructor)::in, maybe_canonical::in, notag_repn::in,
    maybe_du_type_repn::out) is det.

check_and_record_du_notag(TypeCtor, Context, Ctors, MaybeCanon,
        NoTagRepn, MaybeDuRepn) :-
    NoTagRepn = notag_repn(RepnCtorName, RepnArgType, _MaybeCJCs),
    ( if
        Ctors = one_or_more(Ctor, []),
        Ctor = ctor(Ordinal, no_exist_constraints, CtorSymName,
            [CtorArg], 1, CtorContext),
        CtorArg = ctor_arg(MaybeFieldName, _ArgType, ArgContext),
        unqualify_name(CtorSymName) = RepnCtorName
    then
        (
            MaybeCanon = noncanon(_),
            Pieces = [words("Error: the type representation item for"),
                qual_type_ctor(TypeCtor), words("says it is notag,"),
                words("but the type is noncanonical."), nl],
            Spec = simplest_spec($pred, severity_error, phase_type_repn,
                Context, Pieces),
            MaybeDuRepn = have_errors([Spec])
        ;
            MaybeCanon = canon,
            % XXX TYPE_REPN The apw_full is a *lie*
            % if RepnArgType is a 64 bit float on a 32 bit platform.
            % XXX TYPE_REPN Since the ArgPosWidth of the only argument
            % of a no_tag type should never be used, we should see whether
            % it would be practical to use a CtorArgRepn that does not have
            % this field.
            ArgPosWidth = apw_full(arg_only_offset(0), cell_offset(0)),
            CtorArgRepn = ctor_arg_repn(MaybeFieldName, RepnArgType,
                ArgPosWidth, ArgContext),
            CtorTag = no_tag,
            CtorRepn = ctor_repn(Ordinal, no_exist_constraints, CtorSymName,
                CtorTag, [CtorArgRepn], 1, CtorContext),
            insert_ctor_repn_into_map(CtorRepn, map.init, CtorNameRepnMap),
            MaybeDirectArgFunctors = maybe.no,
            (
                MaybeFieldName = no,
                MaybeArgName = no
            ;
                MaybeFieldName =
                    yes(ctor_field_name(ArgSymName, _FieldContext)),
                MaybeArgName = yes(unqualify_name(ArgSymName))
            ),
            DuTypeKind = du_type_kind_notag(CtorSymName, RepnArgType,
                MaybeArgName),
            DuRepn = du_type_repn([CtorRepn], CtorNameRepnMap,
                no_cheaper_tag_test, DuTypeKind, MaybeDirectArgFunctors),
            MaybeDuRepn = have_du_type_repn(DuRepn)
        )
    else
        Pieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect: it says that"),
            words("the type has a single function symbol named"),
            quote(RepnCtorName), words("with one argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_repn,
            Context, Pieces),
        MaybeDuRepn = have_errors([Spec])
    ).

%---------------------%

:- pred check_and_record_du_only_functor(repn_target::in, type_ctor::in,
    prog_context::in, one_or_more(constructor)::in,
    gen_du_only_functor_repn::in, maybe_du_type_repn::out) is det.

check_and_record_du_only_functor(RepnTarget, TypeCtor, Context,
        Ctors, OnlyFunctorRepn, MaybeDuRepn) :-
    OnlyFunctorRepn = gen_du_only_functor_repn(RepnCtorName, RepnArgTypes,
        CNonConstantRepns, _MaybeCJCs),
    list.length(RepnArgTypes, NumRepnArgTypes),
    ( if
        Ctors = one_or_more(Ctor, []),
        Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
            CtorArgs, CtorArity, CtorContext),
        unqualify_name(CtorSymName) = RepnCtorName,
        list.length(CtorArgs, NumArgs),
        NumArgs = NumRepnArgTypes
    then
        check_du_functor(TypeCtor, Ctor, RepnCtorName, NumRepnArgTypes, 0u32,
            [], CtorSpecs),
        (
            CtorSpecs = [_ | _],
            MaybeDuRepn = have_errors(CtorSpecs)
        ;
            CtorSpecs = [],
            (
                ( RepnTarget = repn_target_java
                ; RepnTarget = repn_target_csharp
                ),
                CtorTag = remote_args_tag(remote_args_only_functor),
                maybe_exist_constraints_num_extra_words(MaybeExistConstraints,
                    NumExtraWords),
                FirstAOWordNum = 0,
                FirstCellWordNum = NumExtraWords,
                record_high_level_data_ctor_args(FirstAOWordNum,
                    FirstCellWordNum, CtorArgs, CtorArgRepns)
            ;
                RepnTarget = repn_target_c(CRepnTarget),
                c_target_specific_repn(CRepnTarget, CNonConstantRepns,
                    NonConstantRepn),
                (
                    NonConstantRepn = oncr_local_cell(NCLocalRepn),
                    NCLocalRepn =
                        only_nonconstant_local_cell_repn(OoMLocalArgRepns),
                    LocalArgRepns = one_or_more_to_list(OoMLocalArgRepns),
                    CtorTag = local_args_tag(local_args_only_functor),
                    % These -2s mean that these arguments are not on the heap,
                    % but next to the ptag and any local sectag.
                    ArgOnlyOffset = arg_only_offset(-2),
                    CellOffset = cell_offset(-2),
                    record_local_ctor_args(ArgOnlyOffset, CellOffset,
                        not_seen_nondummy_arg, CtorArgs, RepnArgTypes,
                        LocalArgRepns, CtorArgRepns)
                ;
                    NonConstantRepn = oncr_remote_cell(NCRemoteRepn),
                    NCRemoteRepn =
                        only_nonconstant_remote_cell_repn(OoMRemoteArgRepns),
                    RemoteArgRepns = one_or_more_to_list(OoMRemoteArgRepns),
                    CtorTag = remote_args_tag(remote_args_only_functor),
                    record_remote_ctor_args(CtorArgs, RepnArgTypes,
                        RemoteArgRepns, CtorArgRepns)
                )
            ),
            CtorRepn = ctor_repn(Ordinal, no_exist_constraints, CtorSymName,
                CtorTag, CtorArgRepns, CtorArity, CtorContext),
            insert_ctor_repn_into_map(CtorRepn, map.init, CtorNameRepnMap),
            MaybeDirectArgFunctors = maybe.no,
            DuTypeKind = du_type_kind_general,
            DuRepn = du_type_repn([CtorRepn], CtorNameRepnMap,
                no_cheaper_tag_test, DuTypeKind, MaybeDirectArgFunctors),
            MaybeDuRepn = have_du_type_repn(DuRepn)
        )
    else
        Pieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect: it says that"),
            words("the type has a single function symbol named"),
            quote(RepnCtorName), words("with"),
            int_fixed(NumRepnArgTypes), words("arguments."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_repn,
            Context, Pieces),
        MaybeDuRepn = have_errors([Spec])
    ).

%---------------------%

:- pred check_and_record_du_more_functors(repn_target::in, type_ctor::in,
    prog_context::in, one_or_more(constructor)::in,
    gen_du_more_functors_repn::in, maybe_du_type_repn::out) is det.

check_and_record_du_more_functors(RepnTarget, TypeCtor, Context,
        OoMCtors, MoreFunctorsRepn, MaybeDuRepn) :-
    MoreFunctorsRepn = gen_du_more_functors_repn(HeadCtorMFRepn,
        HeadTailCtorMFRepn, TailTailCtorMFRepns, _MaybeCJCs),
    Ctors = one_or_more_to_list(OoMCtors),
    CtorMFRepns = [HeadCtorMFRepn, HeadTailCtorMFRepn | TailTailCtorMFRepns],
    ( if
        assoc_list.maybe_from_corresponding_lists(Ctors, CtorMFRepns,
            CtorPairs)
    then
        list.foldl2(check_gen_du_functor(TypeCtor), CtorPairs,
            0u32, _, [], CtorSpecs),
        (
            CtorSpecs = [_ | _],
            MaybeDuRepn = have_errors(CtorSpecs)
        ;
            CtorSpecs = [],
            (
                ( RepnTarget = repn_target_java
                ; RepnTarget = repn_target_csharp
                ),
                record_high_level_data_ctors(Ctors, CtorRepns),
                MaybeDirectArgFunctors = maybe.no
            ;
                RepnTarget = repn_target_c(CRepnTarget),
                record_low_level_data_ctors(CRepnTarget, CtorPairs,
                    CtorRepns, MaybeDirectArgFunctors)
            ),
            list.foldl(insert_ctor_repn_into_map, CtorRepns,
                map.init, CtorNameRepnMap),
            compute_cheaper_tag_test(TypeCtor, CtorRepns, CheaperTagTest),
            DuRepn = du_type_repn(CtorRepns, CtorNameRepnMap, CheaperTagTest,
                du_type_kind_general, MaybeDirectArgFunctors),
            MaybeDuRepn = have_du_type_repn(DuRepn)
        )
    else
        RepnCtorNames = list.map(gen_du_functor_repn_name_arity,
            [HeadCtorMFRepn, HeadTailCtorMFRepn | TailTailCtorMFRepns]),
        Pieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect: it says that"),
            words("the type is a non-enum du type,"),
            words("with function symbols named")] ++
            list_to_pieces(RepnCtorNames) ++ [suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_repn,
            Context, Pieces),
        MaybeDuRepn = have_errors([Spec])
    ).

%---------------------%

:- pred check_gen_du_functor(type_ctor::in,
    pair(constructor, gen_du_functor_repn)::in, uint32::in, uint32::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_gen_du_functor(TypeCtor, Ctor - GenDuFunctorRepn,
        !ExpectedOrdinal, !Specs) :-
    (
        GenDuFunctorRepn = gen_du_constant_functor_repn(RepnCtorName, _),
        RepnCtorArity = 0
    ;
        GenDuFunctorRepn = gen_du_nonconstant_functor_repn(RepnCtorName,
            RepnArgTypes0, _),
        list.length(RepnArgTypes0, RepnCtorArity)
    ),
    check_du_functor(TypeCtor, Ctor, RepnCtorName, RepnCtorArity,
        !.ExpectedOrdinal, !Specs),
    !:ExpectedOrdinal = !.ExpectedOrdinal + 1u32.

:- pred check_du_functor(type_ctor::in, constructor::in, string::in, int::in,
    uint32::in, list(error_spec)::in, list(error_spec)::out) is det.

check_du_functor(TypeCtor, Ctor, RepnCtorName, RepnCtorArity, ExpectedOrdinal,
        !Specs) :-
    Ctor = ctor(Ordinal, _MaybeExistConstraints, CtorSymName,
        _CtorArgs, CtorArity, CtorContext),
    CtorName = unqualify_name(CtorSymName),
    ( if
        CtorName = RepnCtorName,
        CtorArity = RepnCtorArity
    then
        true
    else
        CtorSNA = name_arity(CtorName, CtorArity),
        RepnCtorSNA0 = name_arity(RepnCtorName, RepnCtorArity),
        SNAPieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect: it says that the"),
            nth_fixed(uint32.cast_to_int(ExpectedOrdinal)),
            words("function symbol should be"),
            name_arity(RepnCtorSNA0), suffix(","),
            words("but actually it is"), name_arity(CtorSNA), suffix("."), nl],
        % XXX Context should be for Repn, not Ctor.
        SNASpec = simplest_spec($pred, severity_error, phase_type_repn,
            CtorContext, SNAPieces),
        !:Specs = [SNASpec | !.Specs]
    ),
    ( if Ordinal = ExpectedOrdinal then
        true
    else
        RepnCtorSNA1 = name_arity(RepnCtorName, RepnCtorArity),
        OrdinalPieces = [words("Error: the type representation item for"),
            qual_type_ctor(TypeCtor), words("is incorrect:"),
            words("it says that the ordinal of function symbol"),
            name_arity(RepnCtorSNA1), words("is"),
            int_fixed(uint32.cast_to_int(Ordinal)), suffix(","),
            words("but it should be"),
            int_fixed(uint32.cast_to_int(ExpectedOrdinal)), suffix("."), nl],
        % XXX Context should be for Repn, not Ctor.
        OrdinalSpec = simplest_spec($pred, severity_error, phase_type_repn,
            CtorContext, OrdinalPieces),
        !:Specs = [OrdinalSpec | !.Specs]
    ).

%---------------------%

:- pred record_high_level_data_ctors(list(constructor)::in,
    list(constructor_repn)::out) is det.

record_high_level_data_ctors([], []).
record_high_level_data_ctors([Ctor | Ctors], [CtorRepn | CtorRepns]) :-
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    OrdinalUint = uint32.cast_to_uint(Ordinal),
    CtorTag = remote_args_tag(remote_args_ctor(OrdinalUint)),
    maybe_exist_constraints_num_extra_words(MaybeExistConstraints,
        NumExtraWords),
    FirstAOWordNum = 0,
    FirstCellWordNum = NumExtraWords,
    record_high_level_data_ctor_args(FirstAOWordNum, FirstCellWordNum,
        CtorArgs, CtorArgRepns),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName, CtorTag,
        CtorArgRepns, CtorArity, CtorContext),
    record_high_level_data_ctors(Ctors, CtorRepns).

:- pred record_high_level_data_ctor_args(int::in, int::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out) is det.

record_high_level_data_ctor_args(_, _, [], []).
record_high_level_data_ctor_args(CurAOWordNum, CurCellWordNum,
        [CtorArg | CtorArgs], [CtorArgRepn | CtorArgRepns]) :-
    CtorArg = ctor_arg(MaybeFieldName, ArgType, ArgContext),
    ArgRepn = apw_full(arg_only_offset(CurAOWordNum),
        cell_offset(CurCellWordNum)),
    CtorArgRepn = ctor_arg_repn(MaybeFieldName, ArgType, ArgRepn, ArgContext),
    record_high_level_data_ctor_args(CurAOWordNum + 1, CurCellWordNum + 1,
        CtorArgs, CtorArgRepns).

%---------------------%

:- pred record_low_level_data_ctors(c_repn_target::in,
    assoc_list(constructor, gen_du_functor_repn)::in,
    list(constructor_repn)::out, maybe(list(sym_name_arity))::out) is det.

record_low_level_data_ctors(CRepnTarget, CtorsDuRepns, CtorRepns, no) :-
    % XXX TYPE_REPN Record NumLocalSectagBits and LocalMustMask in CRepnTarget.
    select_gen_du_repns_for_target(CRepnTarget, CtorsDuRepns,
        ConstantCtors, LocalCtors, DirectArgCtors, RemoteCtors),
    list.length(ConstantCtors, NumConstantCtors),
    list.length(LocalCtors, NumLocalCtors),
    num_bits_needed_for_n_things(NumConstantCtors + NumLocalCtors,
        NumLocalSectagBits),
    CRepnTarget = c_repn_target(WordSize, _, _),
    ( WordSize = word_size_32, NumPtagBits = 2
    ; WordSize = word_size_64, NumPtagBits = 3
    ),
    compute_sectag_bits(NumLocalSectagBits, LocalSectagBits),
    some [!CtorOrdRepnMap] (
        map.init(!:CtorOrdRepnMap),
        record_constant_ctors(NumPtagBits, LocalSectagBits,
            ConstantCtors, !CtorOrdRepnMap),
        record_local_ctors(NumPtagBits, LocalSectagBits,
            LocalCtors, !CtorOrdRepnMap),
        record_direct_arg_ctors(DirectArgCtors, !CtorOrdRepnMap),
        record_remote_ctors(RemoteCtors, !CtorOrdRepnMap),
        flatten_ctor_ord_repn_map(!.CtorOrdRepnMap, CtorsDuRepns, CtorRepns)
    ).

:- pred select_gen_du_repns_for_target(c_repn_target::in,
    assoc_list(constructor, gen_du_functor_repn)::in,
    list({constructor, constant_repn})::out,
    list({constructor, list(mer_type), more_nonconstant_local_cell_repn})::out,
    list({constructor, mer_type, ptag})::out,
    list({constructor, list(mer_type),
        more_nonconstant_remote_cell_repn})::out)
    is det.

select_gen_du_repns_for_target(_, [], [], [], [], []).
select_gen_du_repns_for_target(CRepnTarget, [Ctor - Repn | CtorRepns],
        !:ConstantCtors, !:LocalCtors, !:DirectArgCtors, !:RemoteCtors) :-
    select_gen_du_repns_for_target(CRepnTarget, CtorRepns,
        !:ConstantCtors, !:LocalCtors, !:DirectArgCtors, !:RemoteCtors),
    (
        Repn = gen_du_constant_functor_repn(_Name, CConstantRepns),
        c_target_specific_repn(CRepnTarget, CConstantRepns, ConstantRepn),
        !:ConstantCtors = [{Ctor, ConstantRepn} | !.ConstantCtors]
    ;
        Repn = gen_du_nonconstant_functor_repn(_Name, ArgTypes,
            CNonConstantRepns),
        c_target_specific_repn(CRepnTarget, CNonConstantRepns,
            NonConstantRepn),
        (
            NonConstantRepn = mncr_local_cell(LocalCellRepn),
            !:LocalCtors = [{Ctor, ArgTypes, LocalCellRepn} | !.LocalCtors]
        ;
            NonConstantRepn = mncr_remote_cell(RemoteCellRepn),
            !:RemoteCtors = [{Ctor, ArgTypes, RemoteCellRepn} | !.RemoteCtors]
        ;
            NonConstantRepn = mncr_direct_arg(Ptag),
            (
                ArgTypes = [],
                unexpected($pred, "mncr_direct_arg but no argtype")
            ;
                ArgTypes = [ArgType],
                !:DirectArgCtors = [{Ctor, ArgType, Ptag} | !.DirectArgCtors]
            ;
                ArgTypes = [_, _ | _],
                unexpected($pred, "mncr_direct_arg but more than one argtype")
            )
        )
    ).

%---------------------%

:- pred flatten_ctor_ord_repn_map(map(uint32, constructor_repn)::in,
    assoc_list(constructor, T)::in, list(constructor_repn)::out) is det.

flatten_ctor_ord_repn_map(_, [], []).
flatten_ctor_ord_repn_map(CtorOrdRepnMap, [Ctor - _| CtorPairs],
        [CtorRepn | CtorRepns]) :-
    Ctor = ctor(Ordinal, _, _, _, _, _),
    map.lookup(CtorOrdRepnMap, Ordinal, CtorRepn),
    flatten_ctor_ord_repn_map(CtorOrdRepnMap, CtorPairs, CtorRepns).

%---------------------%

:- pred record_constant_ctors(int::in, sectag_bits::in,
    list({constructor, constant_repn})::in,
    map(uint32, constructor_repn)::in, map(uint32, constructor_repn)::out)
    is det.

record_constant_ctors(_, _, [], !CtorOrdRepnMap).
record_constant_ctors(NumPtagBits, LocalSectagBits,
        [ConstantCtor | ConstantCtors], !CtorOrdRepnMap) :-
    ConstantCtor = {Ctor, ConstantRepn},
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    expect(unify(CtorArgs, []), $pred, "CtorArgs != []"),
    expect(unify(CtorArity, 0), $pred, "CtorArity != 0"),
    ConstantRepn = constant_repn(SectagValue, SectagWordOrSize),
    (
        SectagWordOrSize = lsectag_rest_of_word(RepnNumSectagBits),
        MustMask = lsectag_always_rest_of_word
    ;
        SectagWordOrSize = lsectag_part_of_word(RepnNumSectagBits),
        MustMask = lsectag_must_be_masked
    ),
    RepnSectagMask = uint.cast_from_int(
        (1 << uint8.cast_to_int(RepnNumSectagBits)) - 1),
    LocalSectagBits = sectag_bits(NumSectagBits, SectagMask),
    expect(unify(NumSectagBits, RepnNumSectagBits), $pred,
        "SectagBits != RepnNumSectagBits"),
    expect(unify(SectagMask, RepnSectagMask), $pred,
        "SectagMask != RepnSectagMask"),
    Ptag = ptag(0u8),
    % OR-ing in the ptag value, which is zero, would be a no-op.
    PrimSec = SectagValue << NumPtagBits,
    LocalSectag = local_sectag(SectagValue, PrimSec, LocalSectagBits),
    CtorTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorTag, [], 0, CtorContext),
    map.det_insert(Ordinal, CtorRepn, !CtorOrdRepnMap),
    record_constant_ctors(NumPtagBits, LocalSectagBits,
        ConstantCtors, !CtorOrdRepnMap).

:- pred record_local_ctors(int::in, sectag_bits::in,
    list({constructor, list(mer_type), more_nonconstant_local_cell_repn})::in,
    map(uint32, constructor_repn)::in, map(uint32, constructor_repn)::out)
    is det.

record_local_ctors(_, _, [], !CtorOrdRepnMap).
record_local_ctors(NumPtagBits, LocalSectagBits, [LocalCtor | LocalCtors],
        !CtorOrdRepnMap) :-
    LocalCtor = {Ctor, ArgTypes, LocalRepn},
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    expect_not(unify(CtorArgs, []), $pred, "CtorArgs = []"),
    expect(unify(list.length(CtorArgs), CtorArity), $pred, "bad CtorArity"),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
        "MaybeExistConstraints != no_exist_constraints"),
    LocalRepn = more_nonconstant_local_cell_repn(CellLocalSectag, OoMArgRepns),
    CellLocalSectag = cell_local_sectag(SectagValue, _SectagSize),
    % XXX TYPE_REPN _SectagSize should be recorded for more_functors,
    % not the ctor, so that (a) we could use it for the constants, and
    % (b) it would be recorded only once.
    Ptag = ptag(0u8),
    % OR-ing in the ptag value, which is zero, would be a no-op.
    PrimSec = SectagValue << NumPtagBits,
    LocalSectag = local_sectag(SectagValue, PrimSec, LocalSectagBits),
    CtorTag = local_args_tag(local_args_not_only_functor(Ptag, LocalSectag)),
    % These -2s mean that these arguments are not on the heap,
    % but next to the ptag and any local sectag.
    ArgOnlyOffset = arg_only_offset(-2),
    CellOffset = cell_offset(-2),
    ArgRepns = one_or_more_to_list(OoMArgRepns),
    record_local_ctor_args(ArgOnlyOffset, CellOffset, not_seen_nondummy_arg,
        CtorArgs, ArgTypes, ArgRepns, CtorArgRepns),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorTag, CtorArgRepns, CtorArity, CtorContext),
    map.det_insert(Ordinal, CtorRepn, !CtorOrdRepnMap),
    record_local_ctors(NumPtagBits, LocalSectagBits, LocalCtors,
        !CtorOrdRepnMap).

:- type maybe_seen_nondummy_arg
    --->    not_seen_nondummy_arg
    ;       seen_nondummy_arg.

:- pred record_local_ctor_args(arg_only_offset::in, cell_offset::in,
    maybe_seen_nondummy_arg::in, list(constructor_arg)::in,
    list(mer_type)::in, list(local_arg_repn)::in,
    list(constructor_arg_repn)::out) is det.

record_local_ctor_args(ArgOnlyOffset, CellOffset, SeenNonDummyArg0,
        CtorArgs, RepnArgTypes, ArgRepns, CtorArgRepns) :-
    ( if
        CtorArgs = [HeadCtorArg | TailCtorArgs],
        RepnArgTypes = [HeadRepnArgType | TailRepnArgTypes],
        ArgRepns = [HeadArgRepn | TailArgRepns]
    then
        HeadCtorArg = ctor_arg(MaybeFieldName, _ArgType, Context),
        (
            HeadArgRepn = local_partial(Shift, FillKindSize),
            ArgShift = uint.cast_to_int(Shift),
            fill_kind_size_to_kind_and_size(FillKindSize, FillKind,
                ArgNumBits),
            ArgMask = (1 << ArgNumBits) - 1,
            (
                SeenNonDummyArg0 = not_seen_nondummy_arg,
                ArgPosWidth = apw_partial_first(ArgOnlyOffset, CellOffset,
                    arg_shift(ArgShift), arg_num_bits(ArgNumBits),
                    arg_mask(ArgMask), FillKind)
            ;
                SeenNonDummyArg0 = seen_nondummy_arg,
                ArgPosWidth = apw_partial_shifted(ArgOnlyOffset, CellOffset,
                    arg_shift(ArgShift), arg_num_bits(ArgNumBits),
                    arg_mask(ArgMask), FillKind)
            ),
            SeenNonDummyArg = seen_nondummy_arg
        ;
            HeadArgRepn = local_none,
            (
                SeenNonDummyArg0 = not_seen_nondummy_arg,
                % XXX TYPE_REPN check that we are counting "seen"
                % from the correct end of the word.
                ArgPosWidth = apw_none_nowhere
            ;
                SeenNonDummyArg0 = seen_nondummy_arg,
                ArgPosWidth = apw_none_shifted(ArgOnlyOffset, CellOffset)
            ),
            SeenNonDummyArg = SeenNonDummyArg0
        ),
        HeadCtorArgRepn = ctor_arg_repn(MaybeFieldName, HeadRepnArgType,
            ArgPosWidth, Context),
        record_local_ctor_args(ArgOnlyOffset, CellOffset, SeenNonDummyArg,
            TailCtorArgs, TailRepnArgTypes, TailArgRepns, TailCtorArgRepns),
        CtorArgRepns = [HeadCtorArgRepn | TailCtorArgRepns]
    else if
        CtorArgs = [],
        RepnArgTypes = [],
        ArgRepns = []
    then
        CtorArgRepns = []
    else
        unexpected($pred, "length mismatch")
    ).

%---------------------%

:- pred record_direct_arg_ctors(list({constructor, mer_type, ptag})::in,
    map(uint32, constructor_repn)::in, map(uint32, constructor_repn)::out)
    is det.

record_direct_arg_ctors([], !CtorOrdRepnMap).
record_direct_arg_ctors([DirectArgCtor | DirectArgCtors], !CtorOrdRepnMap) :-
    DirectArgCtor = {Ctor, ArgType, Ptag},
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
        "direct_arg_tag but maybe_exist_constraints"),
    (
        CtorArgs = [],
        unexpected($pred, "CtorArgs = []")
    ;
        CtorArgs = [CtorArg]
    ;
        CtorArgs = [_, _ | _],
        unexpected($pred, "CtorArgs = [_, _ | _]")
    ),
    expect(unify(CtorArity, 1), $pred, "CtorArity != 1"),
    CtorTag = direct_arg_tag(Ptag),
    CtorArg = ctor_arg(MaybeFieldName, _Type, ArgContext),
    % The CtorArgRepn, and therefore the ArgPosWidth, will never be used
    % for direct_arg_tag functors. The CtorArgRepn we construct here is
    % designed to be the same as what the decide_type_repns_old constructs.
    ArgPosWidth = apw_full(arg_only_offset(0), cell_offset(0)),
    CtorArgRepn =
        ctor_arg_repn(MaybeFieldName, ArgType, ArgPosWidth, ArgContext),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName, CtorTag,
        [CtorArgRepn], CtorArity, CtorContext),
    map.det_insert(Ordinal, CtorRepn, !CtorOrdRepnMap),
    record_direct_arg_ctors(DirectArgCtors, !CtorOrdRepnMap).

%---------------------%

:- pred record_remote_ctors(
    list({constructor, list(mer_type), more_nonconstant_remote_cell_repn})::in,
    map(uint32, constructor_repn)::in, map(uint32, constructor_repn)::out)
    is det.

record_remote_ctors([], !CtorOrdRepnMap).
record_remote_ctors([RemoteCtor | RemoteCtors], !CtorOrdRepnMap) :-
    RemoteCtor = {Ctor, ArgTypes, Repn},
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    expect_not(unify(CtorArity, 0), $pred, "CtorArity = 0"),
    Repn = more_nonconstant_remote_cell_repn(Ptag, CellRemoteSectag,
        OoMRemoteArgRepns),
    (
        CellRemoteSectag = cell_remote_no_sectag,
        RemoteArgsTagInfo = remote_args_unshared(Ptag)
    ;
        CellRemoteSectag = cell_remote_sectag(SectagValue, SectagWordOrSize),
        (
            SectagWordOrSize = rsectag_full_word,
            RSectagSize = rsectag_word
        ;
            SectagWordOrSize = rsectag_part_of_word(SectagNumBits),
            Mask = (1u << uint8.cast_to_int(SectagNumBits)) - 1u,
            RSectagSize = rsectag_subword(sectag_bits(SectagNumBits, Mask))
        ),
        RemoteSectag = remote_sectag(SectagValue, RSectagSize),
        RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag)
    ),
    CtorTag = remote_args_tag(RemoteArgsTagInfo),
    RemoteArgRepns = one_or_more_to_list(OoMRemoteArgRepns),
    record_remote_ctor_args(CtorArgs, ArgTypes, RemoteArgRepns, CtorArgRepns),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName, CtorTag,
        CtorArgRepns, CtorArity, CtorContext),
    map.det_insert(Ordinal, CtorRepn, !CtorOrdRepnMap),
    record_remote_ctors(RemoteCtors, !CtorOrdRepnMap).

:- pred record_remote_ctor_args(list(constructor_arg)::in, list(mer_type)::in,
    list(remote_arg_repn)::in, list(constructor_arg_repn)::out) is det.

record_remote_ctor_args(CtorArgs, RepnArgTypes, ArgRepns, CtorArgRepns) :-
    ( if
        CtorArgs = [HeadCtorArg | TailCtorArgs],
        RepnArgTypes = [HeadRepnArgType | TailRepnArgTypes],
        ArgRepns = [HeadArgRepn | TailArgRepns]
    then
        HeadCtorArg = ctor_arg(MaybeFieldName, _ArgType, Context),
        (
            HeadArgRepn = remote_full(ArgOnlyOffset, CellOffset),
            ArgPosWidth = apw_full(ArgOnlyOffset, CellOffset)
        ;
            HeadArgRepn = remote_double(ArgOnlyOffset, CellOffset, DWKind),
            ArgPosWidth = apw_double(ArgOnlyOffset, CellOffset, DWKind)
        ;
            HeadArgRepn = remote_partial_first(ArgOnlyOffset, CellOffset,
                Shift, FillKindSize),
            ArgShift = uint8.cast_to_int(Shift),
            fill_kind_size_to_kind_and_size(FillKindSize, FillKind,
                ArgNumBits),
            ArgMask = (1 << ArgNumBits) - 1,
            ArgPosWidth = apw_partial_first(ArgOnlyOffset, CellOffset,
                arg_shift(ArgShift), arg_num_bits(ArgNumBits),
                arg_mask(ArgMask), FillKind)
        ;
            HeadArgRepn = remote_partial_shifted(ArgOnlyOffset, CellOffset,
                Shift, FillKindSize),
            ArgShift = uint8.cast_to_int(Shift),
            fill_kind_size_to_kind_and_size(FillKindSize, FillKind,
                ArgNumBits),
            ArgMask = (1 << ArgNumBits) - 1,
            ArgPosWidth = apw_partial_shifted(ArgOnlyOffset, CellOffset,
                arg_shift(ArgShift), arg_num_bits(ArgNumBits),
                arg_mask(ArgMask), FillKind)
        ;
            HeadArgRepn = remote_none_shifted(ArgOnlyOffset, CellOffset),
            ArgPosWidth = apw_none_shifted(ArgOnlyOffset, CellOffset)
        ;
            HeadArgRepn = remote_none_nowhere,
            ArgPosWidth = apw_none_nowhere
        ),
        HeadCtorArgRepn = ctor_arg_repn(MaybeFieldName, HeadRepnArgType,
            ArgPosWidth, Context),
        record_remote_ctor_args(TailCtorArgs, TailRepnArgTypes, TailArgRepns,
            TailCtorArgRepns),
        CtorArgRepns = [HeadCtorArgRepn | TailCtorArgRepns]
    else if
        CtorArgs = [],
        RepnArgTypes = [],
        ArgRepns = []
    then
        CtorArgRepns = []
    else
        unexpected($pred, "length mismatch")
    ).

%---------------------%

:- pred fill_kind_size_to_kind_and_size(fill_kind_size::in,
    fill_kind::out, int::out) is det.

fill_kind_size_to_kind_and_size(FillKindSize, FillKind, ArgNumBits) :-
    (
        FillKindSize = fk_enum(ArgNumBitsInt),
        ArgNumBits = uint.cast_to_int(ArgNumBitsInt),
        FillKind = fill_enum
    ;
        FillKindSize = fk_int8,
        ArgNumBits = 8,
        FillKind = fill_int8
    ;
        FillKindSize = fk_uint8,
        ArgNumBits = 8,
        FillKind = fill_uint8
    ;
        FillKindSize = fk_int16,
        ArgNumBits = 16,
        FillKind = fill_int16
    ;
        FillKindSize = fk_uint16,
        ArgNumBits = 16,
        FillKind = fill_uint16
    ;
        FillKindSize = fk_int32,
        ArgNumBits = 32,
        FillKind = fill_int32
    ;
        FillKindSize = fk_uint32,
        ArgNumBits = 32,
        FillKind = fill_uint32
    ;
        FillKindSize = fk_char21,
        ArgNumBits = 21,
        FillKind = fill_char21
    ).

%---------------------%

:- func gen_du_functor_repn_name_arity(gen_du_functor_repn) = string.

gen_du_functor_repn_name_arity(GenDuFunctorRepn) = Str :-
    (
        GenDuFunctorRepn = gen_du_constant_functor_repn(FunctorName, _CRepns),
        Str = string.format("%s/0", [s(FunctorName)])
    ;
        GenDuFunctorRepn = gen_du_nonconstant_functor_repn(FunctorName,
            ArgTypes, _CRepns),
        Str = string.format("%s/%d",
            [s(FunctorName), i(list.length(ArgTypes))])
    ).

%---------------------------------------------------------------------------%

:- pred foreign_target_specific_repn(repn_target::in, c_java_csharp(T)::in,
    foreign_language::out, T::out) is det.

foreign_target_specific_repn(RepnTarget, CJCs, Lang, Repn) :-
    (
        RepnTarget = repn_target_c(_CRepnTarget),
        Lang = lang_c,
        CJCs = c_java_csharp(Repn, _, _)
    ;
        RepnTarget = repn_target_java,
        Lang = lang_java,
        CJCs = c_java_csharp(_, Repn, _)
    ;
        RepnTarget = repn_target_csharp,
        Lang = lang_csharp,
        CJCs = c_java_csharp(_, _, Repn)
    ).

:- pred record_foreign_type_for_target(repn_target::in,
    string::in, maybe_canonical::in, foreign_type_assertions::in,
    foreign_type_body::out) is det.

record_foreign_type_for_target(RepnTarget, TypeName, MaybeCanon, Assertions,
        ForeignTypeBody) :-
    (
        RepnTarget = repn_target_c(_CRepnTarget),
        CType = c_type(TypeName),
        TypeDetailsForeign =
            type_details_foreign(CType, MaybeCanon, Assertions),
        ForeignTypeBody = foreign_type_body(yes(TypeDetailsForeign), no, no)
    ;
        RepnTarget = repn_target_java,
        JavaType = java_type(TypeName),
        TypeDetailsForeign =
            type_details_foreign(JavaType, MaybeCanon, Assertions),
        ForeignTypeBody = foreign_type_body(no, yes(TypeDetailsForeign), no)
    ;
        RepnTarget = repn_target_csharp,
        CsharpType = csharp_type(TypeName),
        TypeDetailsForeign =
            type_details_foreign(CsharpType, MaybeCanon, Assertions),
        ForeignTypeBody = foreign_type_body(no, no, yes(TypeDetailsForeign))
    ).

:- pred c_target_specific_repn(c_repn_target::in, c_repns(T)::in, T::out)
    is det.

c_target_specific_repn(CRepnTarget, CRepns, Repn) :-
    CRepnTarget = c_repn_target(WordSize, SPF, DA),
    (
        CRepns = c_repns_same(Repn)
    ;
        CRepns = c_repns_64_32(Repn64, Repn32),
        (
            WordSize = word_size_32,
            Repn = Repn32
        ;
            WordSize = word_size_64,
            Repn = Repn64
        )
    ;
        CRepns = c_repns_all(Repn64NoSPFNoDA, Repn64NoSPFDA,
            Repn32NoSPFNoDA, Repn32NoSPFDA, Repn32SPFNoDA, Repn32SPFDA),
        (
            WordSize = word_size_32,
            (
                SPF = c_no_single_prec_float,
                (
                    DA = c_do_not_allow_direct_arg,
                    Repn = Repn32NoSPFNoDA
                ;
                    DA = c_allow_direct_arg,
                    Repn = Repn32NoSPFDA
                )
            ;
                SPF = c_single_prec_float,
                (
                    DA = c_do_not_allow_direct_arg,
                    Repn = Repn32SPFNoDA
                ;
                    DA = c_allow_direct_arg,
                    Repn = Repn32SPFDA
                )
            )
        ;
            WordSize = word_size_64,
            (
                DA = c_do_not_allow_direct_arg,
                Repn = Repn64NoSPFNoDA
            ;
                DA = c_allow_direct_arg,
                Repn = Repn64NoSPFDA
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred compare_old_new_du(io.text_output_stream::in,
    set(type_ctor)::in, set(type_ctor)::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    io::di, io::uo) is det.

compare_old_new_du(Stream, UnRepnTypeCtors, BadRepnTypeCtors,
        OldSortedTypeCtorsTypeDefns, NewSortedTypeCtorsTypeDefns, !IO) :-
    % The two sorted assoc lists are sorted on the type_ctor's *base* name,
    % not on their fully qualified name, so we cannot use
    % map.from_*sorted*_assoc_list.
    map.from_assoc_list(OldSortedTypeCtorsTypeDefns, OldTypeTable),
    map.from_assoc_list(NewSortedTypeCtorsTypeDefns, NewTypeTable),
    map.keys_as_set(OldTypeTable, OldKeys),
    map.keys_as_set(NewTypeTable, NewKeys),
    set.union(OldKeys, NewKeys, Keys),
    set.foldl(
        compare_old_new_du_type_ctor(Stream, UnRepnTypeCtors, BadRepnTypeCtors,
            OldTypeTable, NewTypeTable),
        Keys, !IO).

:- pred compare_old_new_du_type_ctor(io.text_output_stream::in,
    set(type_ctor)::in, set(type_ctor)::in,
    map(type_ctor, hlds_type_defn)::in, map(type_ctor, hlds_type_defn)::in,
    type_ctor::in, io::di, io::uo) is det.

compare_old_new_du_type_ctor(Stream, UnRepnTypeCtors, BadRepnTypeCtors,
        OldTypeTable, NewTypeTable, TypeCtor, !IO) :-
    ( if
        map.search(OldTypeTable, TypeCtor, OldTypeCtorDefn),
        map.search(NewTypeTable, TypeCtor, NewTypeCtorDefn)
    then
        get_type_defn_body(OldTypeCtorDefn, OldBody),
        get_type_defn_body(NewTypeCtorDefn, NewBody),
        ( if
            OldBody = hlds_du_type(OldBodyDu),
            NewBody = hlds_du_type(NewBodyDu)
        then
            MaybeOldRepn = OldBodyDu ^ du_type_repn,
            MaybeNewRepn = NewBodyDu ^ du_type_repn,
            ( if set.member(TypeCtor, UnRepnTypeCtors) then
                UnRepnStr = " (no new repn)"
            else
                UnRepnStr = ""
            ),
            ( if set.member(TypeCtor, BadRepnTypeCtors) then
                BadRepnStr = " (bad new repn)"
            else
                BadRepnStr = ""
            ),
            Suffix = UnRepnStr ++ BadRepnStr,
            (
                MaybeOldRepn = no,
                MaybeNewRepn = no
            ;
                MaybeOldRepn = no,
                MaybeNewRepn = yes(_NewRepn),
                io.format(Stream,
                    "\nTYPE_CTOR_DEFN MISSING OLD REPN for %s%s\n",
                    [s(type_ctor_to_string(TypeCtor)), s(Suffix)], !IO)
            ;
                MaybeOldRepn = yes(_OldRepn),
                MaybeNewRepn = no,
                io.format(Stream,
                    "\nTYPE_CTOR_DEFN MISSING NEW REPN for %s%s\n",
                    [s(type_ctor_to_string(TypeCtor)), s(Suffix)], !IO)
            ;
                MaybeOldRepn = yes(OldRepn),
                MaybeNewRepn = yes(NewRepn),
                ( if OldRepn = NewRepn then
                    true
                else
                    io.format(Stream,
                        "\nTYPE_CTOR_DEFN REPN MISMATCH for %s:\n",
                        [s(type_ctor_to_string(TypeCtor))], !IO),
                    OldDoc = pretty_printer.format(OldTypeCtorDefn),
                    NewDoc = pretty_printer.format(NewTypeCtorDefn),
                    io.format(Stream, "old repn:\n", [], !IO),
                    pretty_printer.write_doc(Stream, OldDoc, !IO),
                    io.nl(Stream, !IO),
                    io.format(Stream, "new repn:%s\n", [s(Suffix)], !IO),
                    pretty_printer.write_doc(Stream, NewDoc, !IO),
                    io.nl(Stream, !IO)
                )
            )
        else
            ( if OldBody = hlds_du_type(_OldBodyDu) then
                io.format(Stream, "TYPE_CTOR_NAME_ONLY_DU_IN_OLD: %s\n",
                    [s(type_ctor_to_string(TypeCtor))], !IO)
            else if NewBody = hlds_du_type(_NewBodyDu) then
                io.format(Stream, "TYPE_CTOR_NAME_ONLY_DU_IN_NEW: %s\n",
                    [s(type_ctor_to_string(TypeCtor))], !IO)
            else
                true
            )
        )
    else
        % We get called only on TypeCtors that exist in at least one of
        % OldTypeTable and NewTypeTable. If TypeCtor is not in one,
        % it must be in the other.
        ( if map.search(OldTypeTable, TypeCtor, _OldTypeCtorDefn) then
            io.format(Stream, "TYPE_CTOR_NAME_ONLY_IN_OLD: %s\n",
                [s(type_ctor_to_string(TypeCtor))], !IO)
        else
            % If the absence of type representation info using the old
            % algorithm has not been a problem, then the presence of
            % such info with the new algorithm should not be a problem either.
            true
        )
    ).

:- pred compare_old_new_notag(io.text_output_stream::in,
    no_tag_type_table::in, no_tag_type_table::in, io::di, io::uo) is det.

compare_old_new_notag(Stream, OldNoTagTypeTable, NewNoTagTypeTable, !IO) :-
    map.keys_as_set(OldNoTagTypeTable, OldKeys),
    map.keys_as_set(NewNoTagTypeTable, NewKeys),
    set.union(OldKeys, NewKeys, Keys),
    set.foldl(
        compare_old_new_notag_type_ctor(Stream,
            OldNoTagTypeTable, NewNoTagTypeTable),
        Keys, !IO).

:- pred compare_old_new_notag_type_ctor(io.text_output_stream::in,
    no_tag_type_table::in, no_tag_type_table::in, type_ctor::in,
    io::di, io::uo) is det.

compare_old_new_notag_type_ctor(Stream, OldNoTagTypeTable, NewNoTagTypeTable,
        TypeCtor, !IO) :-
    ( if
        map.search(OldNoTagTypeTable, TypeCtor, OldEntry),
        map.search(NewNoTagTypeTable, TypeCtor, NewEntry)
    then
        OldEntry = no_tag_type(OldParams, OldCtor, OldArgType),
        NewEntry = no_tag_type(NewParams, NewCtor, NewArgType),
        ( if OldParams = NewParams then
            true
        else
            varset.init(DummyVarSetA),
            OldParamStrs = list.map(
                mercury_var_to_string_vs(DummyVarSetA, print_num_only),
                OldParams),
            NewParamStrs = list.map(
                mercury_var_to_string_vs(DummyVarSetA, print_num_only),
                NewParams),
            OldParamsStr = string.join_list(", ", OldParamStrs),
            NewParamsStr = string.join_list(", ", NewParamStrs),
            io.format(Stream, "NOTAG_TYPE_TABLE_PARAMS %s: <%s> vs <%s>\n",
                [s(type_ctor_to_string(TypeCtor)),
                s(OldParamsStr), s(NewParamsStr)], !IO)
        ),
        ( if OldCtor = NewCtor then
            true
        else
            io.format(Stream, "NOTAG_TYPE_TABLE_CTOR_NAME %s: <%s> vs <%s>\n",
                [s(type_ctor_to_string(TypeCtor)),
                s(sym_name_to_string(OldCtor)),
                s(sym_name_to_string(NewCtor))], !IO)
        ),
        ( if OldArgType = NewArgType then
            true
        else
            varset.init(DummyVarSetC),
            type_to_debug_string(DummyVarSetC, OldArgType, OldArgTypeStr),
            type_to_debug_string(DummyVarSetC, NewArgType, NewArgTypeStr),
            io.format(Stream, "NOTAG_TYPE_TABLE_ARG_TYPE %s: <%s> vs <%s>\n",
                [s(type_ctor_to_string(TypeCtor)),
                s(OldArgTypeStr), s(NewArgTypeStr)], !IO)
        )
    else
        % We get called only on TypeCtors that exist in at least one of
        % OldNoTagTypeTable and NewNoTagTypeTable. If TypeCtor is not in one,
        % it must be in the other.
        ( if map.search(OldNoTagTypeTable, TypeCtor, _) then
            PresentIn = "old"
        else
            PresentIn = "new"
        ),
        io.format(Stream, "NOTAG_TYPE_TABLE_MISMATCH: %s present only in %s\n",
            [s(type_ctor_to_string(TypeCtor)), s(PresentIn)], !IO)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred decide_type_repns_old(module_info::in,
    type_repn_decision_data::in, type_table::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out, no_tag_type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_type_repns_old(ModuleInfo, TypeRepnDec, TypeTable0,
        TypeCtorsTypeDefns0, TypeCtorsTypeDefns, NoTagTypeMap, !Specs) :-
    TypeRepnDec = type_repn_decision_data(_TypeRepns, DirectArgMap,
        ForeignEnums, _ForeignExportEnums),
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_name(ModuleInfo, ModuleName),
    setup_decide_du_params(Globals, ModuleName, DirectArgMap, Params),

    list.foldl2(add_pragma_foreign_enum(ModuleInfo), ForeignEnums,
        map.init, TypeCtorToForeignEnumMap, !Specs),

    % Pass 1.
    map.init(ComponentTypeMap0),
    map.init(NoTagTypeMap0),
    list.foldl5(
        decide_if_simple_du_type(ModuleInfo, Params,
            TypeCtorToForeignEnumMap),
        TypeCtorsTypeDefns0,
        [], NonSubTypeCtorsTypeDefns1,
        [], SubTypeCtorsTypeDefns1,
        ComponentTypeMap0, ComponentTypeMap1,
        NoTagTypeMap0, NoTagTypeMap1, !Specs),

    % Pass 1b.
    list.foldl2(decide_if_subtype_of_simple_du_type(TypeTable0),
        SubTypeCtorsTypeDefns1,
        ComponentTypeMap1, ComponentTypeMap,
        NoTagTypeMap1, NoTagTypeMap),

    % Pass 2.
    list.map_foldl(
        decide_if_complex_du_type(ModuleInfo, Params, ComponentTypeMap),
        NonSubTypeCtorsTypeDefns1, NonSubTypeCtorsTypeDefns2, !Specs),

    % Pass 2b.
    list.map_foldl(decide_if_subtype(TypeTable0, NonSubTypeCtorsTypeDefns2),
        SubTypeCtorsTypeDefns1, SubTypeCtorsTypeDefns2, !Specs),

    TypeCtorsTypeDefns = SubTypeCtorsTypeDefns2 ++ NonSubTypeCtorsTypeDefns2.

:- pred add_special_pred_decl_defns_for_types_maybe_lazily(
    assoc_list(type_ctor, hlds_type_defn)::in,
    module_info::in, module_info::out) is det.

add_special_pred_decl_defns_for_types_maybe_lazily([], !ModuleInfo).
add_special_pred_decl_defns_for_types_maybe_lazily(
        [TypeCtor - TypeDefn | TypeCtorsTypeDefns], !ModuleInfo) :-
    add_special_pred_decl_defns_for_type_maybe_lazily(
        TypeCtor, TypeDefn, !ModuleInfo),
    add_special_pred_decl_defns_for_types_maybe_lazily(
        TypeCtorsTypeDefns, !ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Pass 1.
%

:- type word_aligned_why
    --->    foreign_type_assertion
    ;       mercury_type_defn(hlds_type_defn).

:- type packable_kind
    --->    packable_dummy
    ;       packable_n_bits(int, fill_kind).
            % The number of bits should logically be a uint, not an int,
            % since an argument cannot take a negative number of bits.
            % However, it is better to represent it as an int until
            % the infrastructure for uints is better developed,
            % e.g. until we can do log operations on them, or print them,
            % without converting them to an int first.

:- type component_type_kind
    --->    packable(packable_kind)
    ;       is_word_aligned_ptr(word_aligned_why)
    ;       is_eqv_type(type_ctor).

:- type component_type_map == map(type_ctor, component_type_kind).

:- pred decide_if_simple_du_type(module_info::in, decide_du_params::in,
    type_ctor_to_foreign_enums_map::in,
    pair(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out,
    assoc_list(type_ctor, hlds_type_defn)::in,
    assoc_list(type_ctor, hlds_type_defn)::out,
    component_type_map::in, component_type_map::out,
    no_tag_type_table::in, no_tag_type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_if_simple_du_type(ModuleInfo, Params, TypeCtorToForeignEnumMap,
        TypeCtorTypeDefn0, !NonSubTypeCtorTypeDefns, !SubTypeCtorsTypeDefns,
        !ComponentTypeMap, !NoTagTypeMap, !Specs) :-
    TypeCtorTypeDefn0 = TypeCtor - TypeDefn0,
    get_type_defn_body(TypeDefn0, Body0),
    (
        Body0 = hlds_du_type(BodyDu0),
        BodyDu0 = type_body_du(OoMCtors, MaybeSuperType, MaybeCanonical,
            MaybeRepn0, MaybeForeign),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        expect(unify(MaybeRepn0, no), $pred, "MaybeRepn0 != no"),
        (
            MaybeSuperType = not_a_subtype,
            ( if
                map.search(TypeCtorToForeignEnumMap, TypeCtor, TCFE),
                TCFE = type_ctor_foreign_enums(_LangContextMap,
                    MaybeForeignEnumTagMap),
                MaybeForeignEnumTagMap = yes(ForeignEnumTagMap)
            then
                decide_simple_type_foreign_enum(ModuleInfo, Params,
                    TypeCtor, TypeDefn0, BodyDu0, OoMCtors, ForeignEnumTagMap,
                    TypeCtorTypeDefn, !Specs)
            else if
                ctors_are_all_constants([HeadCtor | TailCtors], _)
            then
                decide_simple_type_dummy_or_mercury_enum(ModuleInfo, Params,
                    TypeCtor, TypeDefn0, BodyDu0, OoMCtors, TypeCtorTypeDefn,
                    !ComponentTypeMap, !Specs)
            else if
                TailCtors = []
            then
                SingleCtor = HeadCtor,
                ( if
                    SingleCtor = ctor(_Ordinal, no_exist_constraints,
                        SingleCtorSymName, [SingleArg], 1, SingleCtorContext),
                    MaybeCanonical = canon,
                    Params ^ ddp_unboxed_no_tag_types =
                        use_unboxed_no_tag_types
                then
                    decide_simple_type_notag(ModuleInfo, Params,
                        TypeCtor, TypeDefn0, BodyDu0,
                        SingleCtorSymName, SingleArg, SingleCtorContext,
                        TypeCtorTypeDefn, !NoTagTypeMap, !Specs)
                else
                    add_du_if_single_ctor_is_word_aligned_ptr(Params, TypeCtor,
                        TypeDefn0, MaybeForeign, !ComponentTypeMap),

                    % Figure out the representation of these types
                    % in the second pass.
                    TypeCtorTypeDefn = TypeCtorTypeDefn0
                )
            else
                % Figure out the representation of these types
                % in the second pass.
                TypeCtorTypeDefn = TypeCtorTypeDefn0
            ),
            cons(TypeCtorTypeDefn, !NonSubTypeCtorTypeDefns)
        ;
            MaybeSuperType = subtype_of(_),
            % Figure out the representation of subtypes in later passes.
            cons(TypeCtorTypeDefn0, !SubTypeCtorsTypeDefns)
        )
    ;
        Body0 = hlds_foreign_type(ForeignType),
        add_foreign_if_word_aligned_ptr(ModuleInfo, Params, TypeCtor,
            ForeignType, !ComponentTypeMap, !Specs),

        % There are no questions of representation to figure out.
        cons(TypeCtorTypeDefn0, !NonSubTypeCtorTypeDefns)
    ;
        Body0 = hlds_abstract_type(AbstractDetails),
        (
            ( AbstractDetails = abstract_type_fits_in_n_bits(_)
            ; AbstractDetails = abstract_dummy_type
            ; AbstractDetails = abstract_notag_type
            ; AbstractDetails = abstract_type_general
            ; AbstractDetails = abstract_solver_type
            ),
            add_abstract_if_packable(TypeCtor, AbstractDetails,
                !ComponentTypeMap),
            cons(TypeCtorTypeDefn0, !NonSubTypeCtorTypeDefns)
        ;
            AbstractDetails = abstract_subtype(_),
            cons(TypeCtorTypeDefn0, !SubTypeCtorsTypeDefns)
        )
    ;
        % XXX TYPE_REPN Enter type equivalences into ComponentTypeMap.
        ( Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_solver_type(_)
        ),
        % There are no questions of representation to figure out.
        cons(TypeCtorTypeDefn0, !NonSubTypeCtorTypeDefns)
    ).

%---------------------%

:- pred decide_simple_type_foreign_enum(module_info::in, decide_du_params::in,
    type_ctor::in, hlds_type_defn::in, type_body_du::in,
    one_or_more(constructor)::in, {cons_id_to_tag_map, foreign_language}::in,
    pair(type_ctor, hlds_type_defn)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_simple_type_foreign_enum(_ModuleInfo, Params, TypeCtor, TypeDefn0,
        BodyDu0, OoMCtors, ForeignEnums, TypeCtorTypeDefn, !Specs) :-
    % XXX TYPE_REPN Should MaybeForeign = yes(...) be allowed?
    ForeignEnums = {ForeignEnumTagMap, Lang},
    DuKind = du_type_kind_foreign_enum(Lang),
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        DirectArgPieces = [words("Error:"), qual_type_ctor(TypeCtor),
            words("has both a"), pragma_decl("foreign_enum"),
            words("declaration and a direct_arg specification."), nl],
        % XXX Should have a non-dummy context.
        DirectArgSpec = simplest_spec($pred, severity_error, phase_type_check,
            dummy_context, DirectArgPieces),
        !:Specs = [DirectArgSpec | !.Specs]
    else
        true
    ),
    Ctors = one_or_more_to_list(OoMCtors),
    ( if ctors_are_all_constants(Ctors, _) then
        true
    else
        NonEnumArgPieces = [words("Error:"), qual_type_ctor(TypeCtor),
            words("has a"), pragma_decl("foreign_enum"), words("declaration,"),
            words("but it has function symbols whose arity is not zero."), nl],
        % XXX Should have the foreign_enum's context.
        NonEnumArgSpec = simplest_spec($pred, severity_error, phase_type_check,
            dummy_context, NonEnumArgPieces),
        !:Specs = [NonEnumArgSpec | !.Specs]
    ),
    list.map_foldl(add_repn_to_foreign_enum_ctor(TypeCtor, ForeignEnumTagMap),
        Ctors, CtorRepns, map.init, CtorRepnMap),
    MaybeCheaperTagTest = no_cheaper_tag_test,
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn(CtorRepns, CtorRepnMap, MaybeCheaperTagTest,
        DuKind, MaybeDirectArgFunctors),
    BodyDu = BodyDu0 ^ du_type_repn := yes(Repn),
    Body = hlds_du_type(BodyDu),
    set_type_defn_body(Body, TypeDefn0, TypeDefn),
    TypeCtorTypeDefn = TypeCtor - TypeDefn.

:- pred add_repn_to_foreign_enum_ctor(type_ctor::in, cons_id_to_tag_map::in,
    constructor::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

add_repn_to_foreign_enum_ctor(TypeCtor, ConsTagMap, Ctor, CtorRepn,
        !CtorRepnMap) :-
    Ctor = ctor(Ordinal, MaybeExistConstraints, SymName, Args, Arity,
        Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    map.lookup(ConsTagMap, ConsId, ConsTag),
    % All function symbols of a foreign enum type should have arity zero.
    % If any have a nonzero arity, our caller will generate an error message,
    % and won't proceed to code generation.
    ArgRepns = list.map(add_dummy_repn_to_ctor_arg, Args),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, SymName, ConsTag,
        ArgRepns, Arity, Context),
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap).

:- func add_dummy_repn_to_ctor_arg(constructor_arg) = constructor_arg_repn.

add_dummy_repn_to_ctor_arg(ConsArg) = ConsArgRepn :-
    ConsArg = ctor_arg(MaybeFieldName, Type, Context),
    DummyWidth = apw_full(arg_only_offset(-3), cell_offset(-3)),
    ConsArgRepn = ctor_arg_repn(MaybeFieldName, Type, DummyWidth, Context).

%---------------------%

:- pred decide_simple_type_dummy_or_mercury_enum(module_info::in,
    decide_du_params::in, type_ctor::in, hlds_type_defn::in,
    type_body_du::in, one_or_more(constructor)::in,
    pair(type_ctor, hlds_type_defn)::out,
    component_type_map::in, component_type_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_simple_type_dummy_or_mercury_enum(_ModuleInfo, Params,
        TypeCtor, TypeDefn0, BodyDu0, OoMCtors, TypeCtorTypeDefn,
        !ComponentTypeMap, !Specs) :-
    OoMCtors = one_or_more(HeadCtor, TailCtors),
    (
        TailCtors = [],
        SingleCtor = HeadCtor,
        DuTypeKind = du_type_kind_direct_dummy,
        SingleCtor = ctor(Ordinal, _MaybeExistConstraints,
            SingleCtorSymName, _Args, _SingleCtorArity, SingleCtorContext),
        SingleCtorTag = dummy_tag,
        SingleCtorRepn = ctor_repn(Ordinal, no_exist_constraints,
            SingleCtorSymName, SingleCtorTag, [], 0, SingleCtorContext),
        CtorRepns = [SingleCtorRepn],
        insert_ctor_repn_into_map(SingleCtorRepn, map.init, CtorRepnMap),
        ComponentKind = packable(packable_dummy)
    ;
        TailCtors = [_ | _],
        DuTypeKind = du_type_kind_mercury_enum,
        assign_tags_to_enum_constants([HeadCtor | TailCtors], CtorRepns,
            0, NextTag, map.init, CtorRepnMap),
        int.log2(NextTag, NumBits),
        ComponentKind = packable(packable_n_bits(NumBits, fill_enum))
    ),
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        Pieces = [words("Error: all the function symbols of"),
            qual_type_ctor(TypeCtor), words("have arity zero,"),
            words("yet it has a direct_arg specification."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_check,
            dummy_context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    MaybeCheaperTagTest = no_cheaper_tag_test,
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn(CtorRepns, CtorRepnMap, MaybeCheaperTagTest,
        DuTypeKind, MaybeDirectArgFunctors),
    BodyDu = BodyDu0 ^ du_type_repn := yes(Repn),
    Body = hlds_du_type(BodyDu),
    set_type_defn_body(Body, TypeDefn0, TypeDefn),
    TypeCtorTypeDefn = TypeCtor - TypeDefn,

    map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap).

:- pred assign_tags_to_enum_constants(
    list(constructor)::in, list(constructor_repn)::out, int::in, int::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

assign_tags_to_enum_constants([], [], !CurTag, !CtorRepnMap).
assign_tags_to_enum_constants([Ctor | Ctors], [CtorRepn | CtorRepns],
        !CurTag, !CtorRepnMap) :-
    Ctor = ctor(Ordinal, MaybeExistConstraints, SymName, Args, Arity, Context),
    expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
        "enum constant has existential constraints"),
    expect(unify(Args, []), $pred, "enum constant has arguments"),
    expect(unify(Arity, 0), $pred, "enum constant has nonzero arity"),
    CtorTag = int_tag(int_tag_int(!.CurTag)),
    CtorRepn = ctor_repn(Ordinal, no_exist_constraints, SymName, CtorTag,
        [], 0, Context),
    !:CurTag = !.CurTag + 1,
    insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap),
    assign_tags_to_enum_constants(Ctors, CtorRepns, !CurTag, !CtorRepnMap).

%---------------------%

:- pred decide_simple_type_notag(module_info::in, decide_du_params::in,
    type_ctor::in, hlds_type_defn::in, type_body_du::in,
    sym_name::in, constructor_arg::in, prog_context::in,
    pair(type_ctor, hlds_type_defn)::out,
    no_tag_type_table::in, no_tag_type_table::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_simple_type_notag(_ModuleInfo, Params, TypeCtor, TypeDefn0, BodyDu0,
        SingleCtorSymName, SingleArg, SingleCtorContext,
        TypeCtorTypeDefn, !NoTagTypeMap, !Specs) :-
    SingleCtorTag = no_tag,
    SingleArg = ctor_arg(MaybeSingleArgFieldName, SingleArgType,
        SingleArgContext),
    % XXX TYPE_REPN The apw_full is a *lie*
    % if the arg type is a 64 bit float on a 32 bit platform.
    SingleArgRepn = ctor_arg_repn(MaybeSingleArgFieldName, SingleArgType,
        apw_full(arg_only_offset(0), cell_offset(0)), SingleArgContext),
    SingleCtorRepn = ctor_repn(0u32, no_exist_constraints,
        SingleCtorSymName, SingleCtorTag, [SingleArgRepn], 1,
        SingleCtorContext),
    insert_ctor_repn_into_map(SingleCtorRepn, map.init, CtorRepnMap),

    MaybeCheaperTagTest = no_cheaper_tag_test,
    (
        MaybeSingleArgFieldName = no,
        MaybeSingleArgName = no
    ;
        MaybeSingleArgFieldName =
            yes(ctor_field_name(SingleArgSymName, _FieldContext)),
        MaybeSingleArgName = yes(unqualify_name(SingleArgSymName))
    ),
    DuTypeKind = du_type_kind_notag(SingleCtorSymName, SingleArgType,
        MaybeSingleArgName),
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        Pieces = [words("Error:"), qual_type_ctor(TypeCtor),
            words("is a no_tag type,"),
            words("yet it has a direct_arg specification."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_check,
            dummy_context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn([SingleCtorRepn], CtorRepnMap, MaybeCheaperTagTest,
        DuTypeKind, MaybeDirectArgFunctors),
    BodyDu = BodyDu0 ^ du_type_repn := yes(Repn),
    Body = hlds_du_type(BodyDu),
    set_type_defn_body(Body, TypeDefn0, TypeDefn),
    TypeCtorTypeDefn = TypeCtor - TypeDefn,

    get_type_defn_tparams(TypeDefn0, TypeParams),
    NoTagType = no_tag_type(TypeParams, SingleCtorSymName, SingleArgType),
    map.det_insert(TypeCtor, NoTagType, !NoTagTypeMap).

%---------------------%

:- pred add_du_if_single_ctor_is_word_aligned_ptr(decide_du_params::in,
    type_ctor::in, hlds_type_defn::in, maybe(foreign_type_body)::in,
    component_type_map::in, component_type_map::out) is det.

add_du_if_single_ctor_is_word_aligned_ptr(Params, TypeCtor, TypeDefn,
        MaybeForeign, !ComponentTypeMap) :-
    % Are we guaranteed to choose a word aligned pointer as the representation?
    ( if
        TypeCtor = type_ctor(_TypeCtorSymName, TypeCtorArity),

        % NOTE We could let the argument's type to have a set of type params
        % that is a subset of the type params of the containing type,
        % but that would require the runtime system to be able to handle
        % variables in the argument type, during unification and comparison
        % (mercury_unify_compare_body.h) during deconstruction
        % (mercury_ml_expand_body.h), during deep copying
        % (mercury_deep_copy_body.h), and maybe during some other
        % operations.
        TypeCtorArity = 0,

        % XXX TYPE_REPN Why this test? It is inherited from legacy code,
        % but the direct_arg optimization is not applicable to types
        % that have only a single constructor.
        DirectArgMap = Params ^ ddp_direct_arg_map,
        not map.search(DirectArgMap, TypeCtor, _DirectArgFunctors)
    then
        % XXX TYPE_REPN This test is only for backward compatibility.
        % The code we should use long term is the else arm.
        ( if
            MaybeForeign = yes(Foreign),
            Target = Params ^ ddp_target,
            is_foreign_type_body_for_target(Foreign, Target, Assertions)
        then
            ( if asserted_word_aligned_pointer(Assertions) then
                ComponentKind = is_word_aligned_ptr(foreign_type_assertion),
                map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
            else
                true
            )
        else
            ComponentKind = is_word_aligned_ptr(mercury_type_defn(TypeDefn)),
            map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
        )
    else
        true
    ).

%---------------------%

:- pred add_foreign_if_word_aligned_ptr(module_info::in, decide_du_params::in,
    type_ctor::in, foreign_type_body::in,
    component_type_map::in, component_type_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_if_word_aligned_ptr(ModuleInfo, Params, TypeCtor,
        ForeignType, !ComponentTypeMap, !Specs) :-
    DirectArgMap = Params ^ ddp_direct_arg_map,
    ( if map.search(DirectArgMap, TypeCtor, _DirectArgFunctors) then
        DirectArgPieces = [words("Error:"), qual_type_ctor(TypeCtor),
            words("has a foreign language representation on this backend,"),
            words("but it also has a direct_arg specification."), nl],
        DirectArgSpec = simplest_spec($pred, severity_error, phase_type_check,
            dummy_context, DirectArgPieces),
        !:Specs = [DirectArgSpec | !.Specs]
    else
        true
    ),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( if is_foreign_type_body_for_target(ForeignType, Target, Assertions) then
        ( if asserted_word_aligned_pointer(Assertions) then
            ComponentKind = is_word_aligned_ptr(foreign_type_assertion),
            map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
        else
            true
        )
    else
        unexpected($pred, "foreign type is not for this backend")
    ).

%---------------------%

:- pred add_abstract_if_packable(type_ctor::in, type_details_abstract::in,
    component_type_map::in, component_type_map::out) is det.

add_abstract_if_packable(TypeCtor, AbstractDetails, !ComponentTypeMap) :-
    (
        AbstractDetails = abstract_type_fits_in_n_bits(NumBits),
        % XXX TYPE_REPN We should get Fill from AbstractDetails.
        Fill = fill_enum,
        ComponentKind = packable(packable_n_bits(NumBits, Fill)),
        map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
    ;
        AbstractDetails = abstract_dummy_type,
        ComponentKind = packable(packable_dummy),
        map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
    ;
        AbstractDetails = abstract_notag_type,
        % XXX Enter information into NoTagTypeMap?
        % We do not yet generate "where type_is_abstract_notag_type" so I think
        % this is unreachable. The `where' block mechanism, along with this
        % module (du_type_layout.m) are intended to be replaced by the
        % `:- type_representation' mechanism anyway.
        sorry($pred, "abstract_notag_type")
    ;
        ( AbstractDetails = abstract_type_general
        ; AbstractDetails = abstract_subtype(_)
        ; AbstractDetails = abstract_solver_type
        )
    ).

%---------------------------------------------------------------------------%
%
% Pass 1b.
%

    % After deciding the representation of simple du types, transfer
    % information in component_type_map and no_tag_type_table to
    % any subtypes of those simple du types.
    %
:- pred decide_if_subtype_of_simple_du_type(type_table::in,
    pair(type_ctor, hlds_type_defn)::in,
    component_type_map::in, component_type_map::out,
    no_tag_type_table::in, no_tag_type_table::out) is det.

decide_if_subtype_of_simple_du_type(OldTypeTable, TypeCtorTypeDefn,
        !ComponentTypeMap, !NoTagTypeMap) :-
    TypeCtorTypeDefn = TypeCtor - TypeDefn,
    get_type_defn_body(TypeDefn, Body),
    (
        Body = hlds_du_type(BodyDu),
        BodyDu = type_body_du(Ctors, MaybeSuperType, _MaybeCanonical,
            MaybeRepn, _MaybeForeign),
        (
            MaybeSuperType = subtype_of(SuperType),
            expect(unify(MaybeRepn, no), $pred, "MaybeRepn != no"),
            ( if
                type_to_ctor(SuperType, SuperTypeCtor),
                get_base_type_ctor(OldTypeTable, SuperTypeCtor, BaseTypeCtor)
            then
                maybe_copy_component_kind_from_base(BaseTypeCtor, TypeCtor,
                    !ComponentTypeMap),
                get_type_defn_tparams(TypeDefn, TypeParams),
                maybe_copy_no_tag_type_from_base(BaseTypeCtor,
                    TypeCtor, TypeParams, yes(Ctors), !NoTagTypeMap)
            else
                unexpected($pred, "cannot get base type")
            )
        ;
            MaybeSuperType = not_a_subtype,
            unexpected($pred, "not subtype")
        )
    ;
        Body = hlds_abstract_type(AbstractDetails),
        (
            AbstractDetails = abstract_subtype(SuperTypeCtor),
            ( if
                get_base_type_ctor(OldTypeTable, SuperTypeCtor, BaseTypeCtor)
            then
                maybe_copy_component_kind_from_base(BaseTypeCtor, TypeCtor,
                    !ComponentTypeMap),
                get_type_defn_tparams(TypeDefn, TypeParams),
                maybe_copy_no_tag_type_from_base(BaseTypeCtor, TypeCtor,
                    TypeParams, no, !NoTagTypeMap)
            else
                % This is reachable if there was an error in the module that
                % defines the imported subtype. The subtype definition would
                % not have been checked when the interface file is created.
                true
            )
        ;
            ( AbstractDetails = abstract_type_general
            ; AbstractDetails = abstract_type_fits_in_n_bits(_)
            ; AbstractDetails = abstract_dummy_type
            ; AbstractDetails = abstract_notag_type
            ; AbstractDetails = abstract_solver_type
            ),
            unexpected($pred, "not subtype")
        )
    ;
        ( Body = hlds_foreign_type(_)
        ; Body = hlds_eqv_type(_)
        ; Body = hlds_solver_type(_)
        ),
        unexpected($pred, "not subtype")
    ).

:- pred maybe_copy_component_kind_from_base(type_ctor::in, type_ctor::in,
    component_type_map::in, component_type_map::out) is det.

maybe_copy_component_kind_from_base(BaseTypeCtor, TypeCtor,
        !ComponentTypeMap) :-
    ( if map.search(!.ComponentTypeMap, BaseTypeCtor, ComponentKind) then
        map.det_insert(TypeCtor, ComponentKind, !ComponentTypeMap)
    else
        true
    ).

:- pred maybe_copy_no_tag_type_from_base(type_ctor::in,
    type_ctor::in, list(type_param)::in, maybe(one_or_more(constructor))::in,
    no_tag_type_table::in, no_tag_type_table::out) is det.

maybe_copy_no_tag_type_from_base(BaseTypeCtor, TypeCtor, TypeParams0,
        MaybeCtors, !NoTagTypeMap) :-
    ( if map.search(!.NoTagTypeMap, BaseTypeCtor, BaseNoTagType) then
        BaseNoTagType = no_tag_type(BaseTypeParams, BaseCtorName, BaseArgType),
        (
            MaybeCtors = no,
            % A subtype must have the same constructor name as the base type,
            % but possibly a different module name.
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            det_sym_name_get_module_name(TypeCtorSymName, ModuleName),
            UnqualCtorName = unqualify_name(BaseCtorName),
            CtorName = qualified(ModuleName, UnqualCtorName),
            % We do not have the actual argument type in the abstract subtype
            % so just use the argument type from the base type.
            TypeParams = BaseTypeParams,
            ArgType = BaseArgType
        ;
            MaybeCtors = yes(Ctors),
            Ctors = one_or_more(Ctor, TailCtors),
            expect(unify(TailCtors, []), $pred,
                "subtype of notag type has multiple ctors"),
            Ctor = ctor(_Ordinal, _MaybeExist, CtorName, CtorArgs, _NumArgs,
                _Context),
            ( if CtorArgs = [CtorArgPrime] then
                CtorArg = CtorArgPrime
            else
                unexpected($pred,
                    "subtype of notag type has wrong number of ctor args")
            ),
            CtorArg = ctor_arg(_MaybeFieldName, ArgType, _ArgContext),
            TypeParams = TypeParams0
        ),
        NoTagType = no_tag_type(TypeParams, CtorName, ArgType),
        map.det_insert(TypeCtor, NoTagType, !NoTagTypeMap)
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Pass 2.
%

:- pred decide_if_complex_du_type(module_info::in, decide_du_params::in,
    component_type_map::in,
    pair(type_ctor, hlds_type_defn)::in, pair(type_ctor, hlds_type_defn)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_if_complex_du_type(ModuleInfo, Params, ComponentTypeMap,
        TypeCtorTypeDefn0, TypeCtorTypeDefn, !Specs) :-
    TypeCtorTypeDefn0 = TypeCtor - TypeDefn0,
    get_type_defn_body(TypeDefn0, Body0),
    (
        Body0 = hlds_du_type(BodyDu0),
        BodyDu0 = type_body_du(Ctors, MaybeSuperType, _MaybeCanonical,
            MaybeRepn0, _MaybeForeign),
        expect(unify(MaybeSuperType, not_a_subtype), $pred,
            "subtype not separated out"),
        (
            MaybeRepn0 = yes(_),
            % We have already decided this type's representation
            % in the first pass.
            TypeCtorTypeDefn = TypeCtorTypeDefn0
        ;
            MaybeRepn0 = no,
            decide_complex_du_type(ModuleInfo, Params, ComponentTypeMap,
                TypeCtor, TypeDefn0, one_or_more_to_list(Ctors), Repn, !Specs),
            BodyDu = BodyDu0 ^ du_type_repn := yes(Repn),
            Body = hlds_du_type(BodyDu),
            set_type_defn_body(Body, TypeDefn0, TypeDefn),
            TypeCtorTypeDefn = TypeCtor - TypeDefn
        )
    ;
        ( Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_abstract_type(_)
        ; Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_solver_type(_)
        ),
        % There are no questions of representation to figure out.
        TypeCtorTypeDefn = TypeCtorTypeDefn0
    ).

:- pred decide_complex_du_type(module_info::in, decide_du_params::in,
    component_type_map::in, type_ctor::in, hlds_type_defn::in,
    list(constructor)::in, du_type_repn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type(ModuleInfo, Params, ComponentTypeMap, TypeCtor,
        TypeDefn0, Ctors, Repn, !Specs) :-
    get_type_defn_status(TypeDefn0, TypeStatus),
    ( if Ctors = [SingleCtor] then
        decide_complex_du_type_single_ctor(ModuleInfo, Params,
            ComponentTypeMap, TypeStatus, SingleCtor, Repn, !Specs)
    else
        decide_complex_du_type_general(ModuleInfo, Params, ComponentTypeMap,
            TypeCtor, TypeStatus, Ctors, Repn, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_type_single_ctor(module_info::in,
    decide_du_params::in, component_type_map::in,
    type_status::in, constructor::in, du_type_repn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type_single_ctor(ModuleInfo, Params, ComponentTypeMap,
        TypeStatus, SingleCtor, Repn, !Specs) :-
    SingleCtor = ctor(Ordinal, MaybeExistConstraints, SingleCtorSymName,
        SingleCtorArgs, SingleCtorArity, SingleCtorContext),
    % Check whether we could pack SingleCtorArgs into a single word
    % with the (zero) primary tag.
    %
    % We want to keep the primary tag so that we can apply the direct arg
    % optimization to *another* type that has a functor whose only argument
    % is of this type.
    ( if
        MaybeExistConstraints = no_exist_constraints,
        Params ^ ddp_maybe_primary_tags = max_primary_tag(_, NumPtagBits),
        Params ^ ddp_allow_packing_local_sectags = yes,
        Limit = Params ^ ddp_arg_pack_bits - NumPtagBits,
        ctor_has_all_packable_args_within_limit(Params, ComponentTypeMap,
            SingleCtor, Limit, _NumBits)
    then
        SingleCtorTag = local_args_tag(local_args_only_functor),
        % This LocalSectag says the local sectag contains zero bits,
        % and its value is zero both without and with the primary tag
        % (which is therefore also zero).
        LocalSectag = local_sectag(0u, 0u, sectag_bits(0u8, 0u)),
        decide_complex_du_ctor_local_args(Params, ComponentTypeMap,
            LocalSectag, SingleCtorArgs, SingleCtorArgRepns)
    else
        SingleCtorTag = remote_args_tag(remote_args_only_functor),
        NumRemoteSectagBits = 0,
        decide_complex_du_ctor_remote_args(ModuleInfo, Params,
            ComponentTypeMap, TypeStatus, NumRemoteSectagBits, SingleCtorTag,
            MaybeExistConstraints, SingleCtorSymName, SingleCtorContext,
            SingleCtorArgs, SingleCtorArgRepns, _MaybeTagwordArgs, !Specs)
    ),
    SingleCtorRepn = ctor_repn(Ordinal, MaybeExistConstraints,
        SingleCtorSymName, SingleCtorTag,
        SingleCtorArgRepns, SingleCtorArity, SingleCtorContext),

    CtorRepnMap = map.singleton(unqualify_name(SingleCtorSymName),
        one_or_more(SingleCtorRepn, [])),
    DuTypeKind = du_type_kind_general,
    MaybeDirectArgFunctors = no,
    Repn = du_type_repn([SingleCtorRepn], CtorRepnMap, no_cheaper_tag_test,
        DuTypeKind, MaybeDirectArgFunctors).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_type_general(module_info::in, decide_du_params::in,
    component_type_map::in, type_ctor::in, type_status::in,
    list(constructor)::in, du_type_repn::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type_general(ModuleInfo, Params, ComponentTypeMap,
        TypeCtor, TypeStatus, Ctors, Repn, !Specs) :-
    Target = Params ^ ddp_target,
    UsesConstructors = target_uses_constructors(Target),
    MaybePrimaryTags = Params ^ ddp_maybe_primary_tags,
    (
        MaybePrimaryTags = no_primary_tags,
        (
            UsesConstructors = yes,
            % In this case, NumRemoteSectags is actually more like "NumDatas".
            assign_ctor_remote_args_tags(TypeCtor,
                Ctors, 0u, NumRemoteSectags, map.init, CtorTagMap)
        ;
            UsesConstructors = no,
            assign_unshared_then_shared_remote_args_tags(TypeCtor, 0u8, 0u8,
                Ctors, NumRemoteSectags, map.init, CtorTagMap)
        ),
        DirectArgFunctorNames = []
    ;
        MaybePrimaryTags = max_primary_tag(MaxPtag, NumPtagBits),
        expect(unify(UsesConstructors, no), $pred,
            "have ptags but UsesConstructors = yes"),
        MaxPtag = ptag(MaxPtagUint8),
        separate_out_constants(Ctors, Constants, Functors),
        MaybeDirectArgs = Params ^ ddp_maybe_direct_args,
        (
            MaybeDirectArgs = direct_args_enabled,
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            det_sym_name_get_module_name(TypeCtorSymName, TypeCtorModuleName),
            DirectArgMap = Params ^ ddp_direct_arg_map,
            ( if map.search(DirectArgMap, TypeCtor, DirectArgMapEntry) then
                AssertedDirectArgFunctors = DirectArgMapEntry
            else
                AssertedDirectArgFunctors = []
            ),
            TypeIsImported = type_status_is_imported(TypeStatus),
            TypeDefinedHere = type_status_defined_in_this_module(TypeStatus),
            list.filter(
                is_direct_arg_ctor(ComponentTypeMap, TypeCtorModuleName,
                    TypeStatus, TypeIsImported, TypeDefinedHere,
                    AssertedDirectArgFunctors),
                Functors, DirectArgFunctors, NonDirectArgFunctors),
            check_direct_arg_assertions(AssertedDirectArgFunctors,
                NonDirectArgFunctors, !Specs),
            DirectArgFunctorNames =
                list.map(constructor_to_sym_name_arity, DirectArgFunctors)
        ;
            MaybeDirectArgs = direct_args_disabled,
            DirectArgFunctors = [],
            DirectArgFunctorNames = [],
            NonDirectArgFunctors = Functors
        ),
        compute_local_packable_functors(Params, ComponentTypeMap, NumPtagBits,
            Constants, NonDirectArgFunctors,
            LocalPackedFunctors, NonLocalPackedFunctors,
            LocalSectagBits, MustMask),
        some [!CurPtagUint8, !CtorTagMap] (
            !:CurPtagUint8 = 0u8,
            map.init(!:CtorTagMap),
            ( if
                Constants = [],
                LocalPackedFunctors = []
            then
                true
            else
                LocalsPtag = ptag(!.CurPtagUint8),
                CurLocalSectag0 = 0u,
                assign_tags_to_constants(TypeCtor, LocalsPtag,
                    NumPtagBits, LocalSectagBits, MustMask,
                    CurLocalSectag0, CurLocalSectag1, Constants, !CtorTagMap),
                assign_tags_to_local_packed_functors(TypeCtor, LocalsPtag,
                    NumPtagBits, LocalSectagBits,
                    CurLocalSectag1, LocalPackedFunctors, !CtorTagMap),
                !:CurPtagUint8 = !.CurPtagUint8 + 1u8
            ),
            assign_tags_to_direct_arg_functors(TypeCtor, MaxPtagUint8,
                !CurPtagUint8, DirectArgFunctors, NonDirectArgFunctors,
                LeftOverDirectArgFunctors, !CtorTagMap),
            assign_unshared_then_shared_remote_args_tags(TypeCtor,
                MaxPtagUint8, !.CurPtagUint8,
                LeftOverDirectArgFunctors ++ NonLocalPackedFunctors,
                NumRemoteSectags, !CtorTagMap),
            CtorTagMap = !.CtorTagMap
        )
    ),
    ( if NumRemoteSectags = 0u then
        NumRemoteSectagBitsInt = 0,
        NumRemoteSectagBits = 0u8
    else
        NumRemoteSectagsInt = uint.cast_to_int(NumRemoteSectags),
        % XXX int.log2 should actually be uint.log2.
        int.log2(NumRemoteSectagsInt, NumRemoteSectagBitsInt),
        NumRemoteSectagBits = uint8.det_from_int(NumRemoteSectagBitsInt)
    ),
    list.map_foldl2(
        decide_complex_du_type_ctor(ModuleInfo, Params, ComponentTypeMap,
            TypeCtor, TypeStatus, CtorTagMap, NumRemoteSectagBitsInt),
        Ctors, CtorRepns0, no_tagword_args, MaybeTagwordArgs, !Specs),
    (
        MaybeTagwordArgs = no_tagword_args,
        CtorRepns = CtorRepns0
    ;
        MaybeTagwordArgs = some_tagword_args,
        % Some ctors with remote_args_tag(remote_args_shared(...)) tags
        % have arguments packed up against their sectags, so we have to set
        % the representation of *all* constructors with remote sectags
        % to use just NumRemoteSectagBits to represent the sectag,
        % *not* the whole word. We use this to signal the code generator
        % that computing the remote sectag requires masking off any bits
        % that are not part of the sectag. If we did not do this, then
        % bits belonging to the arguments packed next to the sectag
        % would end up in the value that we compute for the sectag.
        % Trying to use such a "contaminated" sectag e.g. as an index into
        % an array of MR_DuFunctorDescs would lead to incorrect behavior.
        SectagMask = (1u << NumRemoteSectagBitsInt) - 1u,
        SectagBits = sectag_bits(NumRemoteSectagBits, SectagMask),
        SectagSize = rsectag_subword(SectagBits),
        set_remote_args_sectag_size(SectagSize, CtorRepns0, CtorRepns)
    ),
    list.foldl(insert_ctor_repn_into_map, CtorRepns, map.init, CtorRepnMap),
    compute_cheaper_tag_test(TypeCtor, CtorRepns, CheaperTagTest),
    % The maybe() wrapper looks to be unnecessary, but we currently use it
    % to allow the representation of "where direct_arg is []" annotations
    % on types, such as in tests/invalid/where_direct_arg.m.
    (
        DirectArgFunctorNames = [],
        MaybeDirectArgFunctorNames = no
    ;
        DirectArgFunctorNames = [_ | _],
        MaybeDirectArgFunctorNames = yes(DirectArgFunctorNames)
    ),
    Repn = du_type_repn(CtorRepns, CtorRepnMap, CheaperTagTest,
        du_type_kind_general, MaybeDirectArgFunctorNames).

:- pred set_remote_args_sectag_size(rsectag_size::in,
    list(constructor_repn)::in, list(constructor_repn)::out) is det.

set_remote_args_sectag_size(_, [], []).
set_remote_args_sectag_size(SectagSize,
        [CtorRepn0 | CtorRepns0], [CtorRepn | CtorRepns]) :-
    ConsTag0 = CtorRepn0 ^ cr_tag,
    (
        ConsTag0 = remote_args_tag(RemoteArgsTagInfo0),
        (
            RemoteArgsTagInfo0 = remote_args_shared(Ptag, RemoteSectag0),
            RemoteSectag0 = remote_sectag(Sectag, _SectagSize0),
            RemoteSectag = remote_sectag(Sectag, SectagSize),
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            ConsTag = remote_args_tag(RemoteArgsTagInfo),
            CtorRepn = CtorRepn0 ^ cr_tag := ConsTag
        ;
            ( RemoteArgsTagInfo0 = remote_args_only_functor
            ; RemoteArgsTagInfo0 = remote_args_unshared(_)
            ; RemoteArgsTagInfo0 = remote_args_ctor(_)
            ),
            CtorRepn = CtorRepn0
        )
    ;
        ( ConsTag0 = int_tag(_)
        ; ConsTag0 = float_tag(_)
        ; ConsTag0 = string_tag(_)
        ; ConsTag0 = foreign_tag(_, _)
        ; ConsTag0 = dummy_tag
        ; ConsTag0 = shared_local_tag_no_args(_, _, _)
        ; ConsTag0 = ground_term_const_tag(_, _)
        ; ConsTag0 = type_info_const_tag(_)
        ; ConsTag0 = typeclass_info_const_tag(_)
        ; ConsTag0 = type_ctor_info_tag(_, _, _)
        ; ConsTag0 = base_typeclass_info_tag(_, _, _)
        ; ConsTag0 = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag0 = tabling_info_tag(_, _)
        ; ConsTag0 = table_io_entry_tag(_, _)
        ; ConsTag0 = local_args_tag(_)
        ; ConsTag0 = no_tag
        ; ConsTag0 = direct_arg_tag(_)
        ; ConsTag0 = closure_tag(_, _, _)
        ),
        CtorRepn = CtorRepn0
    ),
    set_remote_args_sectag_size(SectagSize, CtorRepns0, CtorRepns).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_type_ctor(module_info::in, decide_du_params::in,
    component_type_map::in, type_ctor::in, type_status::in,
    cons_id_to_tag_map::in, int::in, constructor::in, constructor_repn::out,
    maybe_tagword_args::in, maybe_tagword_args::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_type_ctor(ModuleInfo, Params, ComponentTypeMap,
        TypeCtor, TypeStatus, CtorTagMap, NumRemoteSectagBits,
        Ctor, CtorRepn, !MaybeTagwordArgs, !Specs) :-
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorSymName,
        CtorArgs, CtorArity, CtorContext),
    ConsId = cons(CtorSymName, CtorArity, TypeCtor),
    map.lookup(CtorTagMap, ConsId, CtorTag),
    (
        (
            CtorTag = remote_args_tag(_)
        ;
            CtorTag = no_tag,
            expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
                "no_exist_constraints but exist_constraints")
        ;
            CtorTag = direct_arg_tag(_),
            expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
                "direct_arg_tag but exist_constraints")
        ),
        decide_complex_du_ctor_remote_args(ModuleInfo, Params,
            ComponentTypeMap, TypeStatus, NumRemoteSectagBits, CtorTag,
            MaybeExistConstraints, CtorSymName, CtorContext,
            CtorArgs, CtorArgRepns, CtorMaybeTagwordArgs, !Specs),
        (
            CtorMaybeTagwordArgs = no_tagword_args
        ;
            CtorMaybeTagwordArgs = some_tagword_args,
            !:MaybeTagwordArgs = some_tagword_args
        )
    ;
        CtorTag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            % Our parent predicate decide_complex_du_type_general
            % should never assign this tag to a Ctor. Any ctor for which
            % this tag is the right tag should be handled by our parent's
            % sister predicate decide_complex_du_type_single_ctor.
            unexpected($pred, "local_args_only_functor")
        ;
            LocalArgsTagInfo = local_args_not_only_functor(_Ptag, LocalSectag)
        ),
        expect(unify(MaybeExistConstraints, no_exist_constraints), $pred,
            "shared_local_tag_with_args but exist_constraints"),
        decide_complex_du_ctor_local_args(Params, ComponentTypeMap,
            LocalSectag, CtorArgs, CtorArgRepns)
    ;
        ( CtorTag = dummy_tag
        ; CtorTag = int_tag(_)
        ; CtorTag = shared_local_tag_no_args(_, _, _)
        ),
        % Do nothing.
        expect(unify(CtorArgs, []), $pred, "enum or dummy type has args"),
        CtorArgRepns = []
    ;
        ( CtorTag = float_tag(_)
        ; CtorTag = string_tag(_)
        ; CtorTag = foreign_tag(_, _)
        ; CtorTag = type_ctor_info_tag(_, _, _)
        ; CtorTag = type_info_const_tag(_)
        ; CtorTag = typeclass_info_const_tag(_)
        ; CtorTag = base_typeclass_info_tag(_, _, _)
        ; CtorTag = closure_tag(_, _, _)
        ; CtorTag = deep_profiling_proc_layout_tag(_, _)
        ; CtorTag = table_io_entry_tag(_, _)
        ; CtorTag = tabling_info_tag(_, _)
        ; CtorTag = ground_term_const_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ),
    CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorSymName, CtorTag,
        CtorArgRepns, CtorArity, CtorContext).

%---------------------------------------------------------------------------%

:- inst sortof_remote_args_tag for cons_tag/0
    --->    remote_args_tag(ground)
    ;       no_tag
    ;       direct_arg_tag(ground).

:- type maybe_tagword_args
    --->    no_tagword_args
    ;       some_tagword_args.

:- pred decide_complex_du_ctor_remote_args(module_info::in,
    decide_du_params::in, component_type_map::in, type_status::in,
    int::in, cons_tag::in(sortof_remote_args_tag),
    maybe_cons_exist_constraints::in, sym_name::in, prog_context::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out,
    maybe_tagword_args::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_complex_du_ctor_remote_args(ModuleInfo, Params, ComponentTypeMap,
        TypeStatus, NumRemoteSectagBits, CtorTag, MaybeExistConstraints,
        CtorSymName, CtorContext, CtorArgs, CtorArgRepns, MaybeTagwordArgs,
        !Specs) :-
    maybe_exist_constraints_num_extra_words(MaybeExistConstraints,
        NumExtraArgWords),

    % There are two schemes for what a memory cell for a term may look like.
    % In the traditional scheme, the cell consists of, in order:
    %
    % - a word containing the remote secondary tag bits, if this ctor
    %   has a remote secondary tag;
    %
    % - zero or more extra arguments containing typeinfos and/or
    %   typeclass_infos added by polymorphism, if this ctor has one or
    %   more existential constraints (this number should be given by
    %   NumExtraArgWords), and
    %
    % - the words containing the ctor's arguments themselves.
    %
    % In a newer scheme that applies only to functors that have secondary tags
    % but do not have extra arguments containing typeinfos and/or
    % typeclass_infos, the cell consists of:
    %
    % - a word containing the remote secondary tag bits, and
    %   as long a subsequence of the initial subword-sized arguments
    %   as will fit, and
    %
    % - the words containing the rest of the ctor's arguments.
    %
    % The first arm of this switch and the else part of the second arm
    % handles the first case, while the then part of the second arm
    % handles the second case.
    (
        ( CtorTag = remote_args_tag(remote_args_only_functor)
        ; CtorTag = remote_args_tag(remote_args_unshared(_))
        ; CtorTag = remote_args_tag(remote_args_ctor(_))
        ; CtorTag = no_tag
        ; CtorTag = direct_arg_tag(_)
        ),
        MaybeTagwordArgs = no_tagword_args,
        TagwordCtorArgRepns = [],
        NonTagwordCtorArgs = CtorArgs,
        NumTagwords = 0
    ;
        CtorTag = remote_args_tag(remote_args_shared(_, _)),
        ( if
            % Is the optimization of packing args next to the remote sectag
            % allowed?
            Params ^ ddp_allow_packing_remote_sectags = yes,
            % Is there a remote sectag we can pack next to?
            NumRemoteSectagBits > 0,
            % We cannot put args next to the remote sectag if we have to put
            % extra typeinfos and/or typeclass_infos between them.
            NumExtraArgWords = 0,

            % If we pass all the above tests, try to find args to pack
            % next to the remote sectag.
            Limit = Params ^ ddp_arg_pack_bits - NumRemoteSectagBits,
            find_initial_args_packable_within_limit(Params, ComponentTypeMap,
                Limit, 0, _NumBits, CtorArgs,
                PackableCtorArgs, LeftOverCtorArgs),
            % Did we find any?
            PackableCtorArgs = [_ | _]
        then
            MaybeTagwordArgs = some_tagword_args,
            % The code of runtime/mercury_ml_expand_body.h has traditionally
            % set the argument vector to point to the first argument, i.e.
            % *past* the word containing the secondary tag (which used to fill
            % the whole of a word). Setting ArgOnlyOffset to -1 for arguments
            % packed next to the sectag allows such code to still work.
            ArgOnlyOffset = arg_only_offset(-1),
            CellOffset = cell_offset(0),
            decide_tagword_args(Params, ComponentTypeMap,
                ArgOnlyOffset, CellOffset, NumRemoteSectagBits,
                PackableCtorArgs, TagwordCtorArgRepns),
            NonTagwordCtorArgs = LeftOverCtorArgs
        else
            MaybeTagwordArgs = no_tagword_args,
            TagwordCtorArgRepns = [],
            NonTagwordCtorArgs = CtorArgs
        ),
        NumTagwords = 1
    ),

    FirstArgWordNum = 0,
    FirstCellWordNum = NumTagwords + NumExtraArgWords,
    decide_complex_du_ctor_remote_args_loop(ModuleInfo, Params,
        ComponentTypeMap, FirstArgWordNum, FirstCellWordNum,
        NonTagwordCtorArgs, NonTagwordCtorArgRepns),
    CtorArgRepns = TagwordCtorArgRepns ++ NonTagwordCtorArgRepns,

    MaybeInformPacking = Params ^ ddp_inform_suboptimal_pack,
    ( if
        MaybeInformPacking = inform_about_packing,
        type_status_defined_in_this_module(TypeStatus) = yes
    then
        (
            MaybeTagwordArgs = no_tagword_args,
            MaybeSectagAsPseudoArg = no
        ;
            MaybeTagwordArgs = some_tagword_args,
            MaybeSectagAsPseudoArg = yes(NumRemoteSectagBits)
        ),
        inform_about_any_suboptimal_packing(Params, CtorSymName, CtorContext,
            MaybeSectagAsPseudoArg, CtorArgRepns, !Specs)
    else
        true
    ).

:- func target_uses_constructors(compilation_target) = bool.

target_uses_constructors(target_c) = no.
target_uses_constructors(target_csharp) = yes.
target_uses_constructors(target_java) = yes.
% NOTE The information here is repeated in ml_target_uses_constructors in
% ml_type_gen.m; any changes here will require corresponding changes there.

:- pred decide_complex_du_ctor_remote_args_loop(module_info::in,
    decide_du_params::in, component_type_map::in, int::in, int::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out) is det.

decide_complex_du_ctor_remote_args_loop(_, _, _, _, _, [], []).
decide_complex_du_ctor_remote_args_loop(ModuleInfo, Params, ComponentTypeMap,
        CurAOWordNum, CurCellWordNum, [Arg | Args], ArgRepns) :-
    ArgOnlyOffset = arg_only_offset(CurAOWordNum),
    CellOffset = cell_offset(CurCellWordNum),
    NumAvailBits = Params ^ ddp_arg_pack_bits,
    NumUsedBits0 = 0,
    find_word_packable_args(Params, ComponentTypeMap,
        NumAvailBits, NumUsedBits0, NumUsedBits,
        [Arg | Args], ArgsPackables, LeftOverArgs),
    (
        ArgsPackables = [_ | _],
        NumPrefixBits = 0,
        decide_packed_arg_word_loop(treat_as_first_arg,
            ArgOnlyOffset, CellOffset, NumPrefixBits, _,
            ArgsPackables, WordArgRepns),
        ( if NumUsedBits > 0 then
            NextAOWordNum = CurAOWordNum + 1,
            NextCellWordNum = CurCellWordNum + 1
        else
            NextAOWordNum = CurAOWordNum,
            NextCellWordNum = CurCellWordNum
        ),
        decide_complex_du_ctor_remote_args_loop(ModuleInfo, Params,
            ComponentTypeMap, NextAOWordNum, NextCellWordNum,
            LeftOverArgs, TailArgRepns),
        ArgRepns = WordArgRepns ++ TailArgRepns
    ;
        ArgsPackables = [],
        Arg = ctor_arg(ArgName, ArgType, ArgContext),
        deref_eqv_types(ModuleInfo, ArgType, DerefArgType),
        ( if
            DerefArgType = builtin_type(BuiltinType),
            (
                BuiltinType = builtin_type_float,
                Params ^ ddp_double_word_floats = use_double_word_floats,
                DWKind = dw_float
            ;
                BuiltinType = builtin_type_int(int_type_int64),
                Params ^ ddp_double_word_int64s = use_double_word_int64s,
                % XXX ARG_PACK For bootstrapping.
                Params ^ ddp_allow_double_word_ints = yes,
                DWKind = dw_int64
            ;
                BuiltinType = builtin_type_int(int_type_uint64),
                Params ^ ddp_double_word_int64s = use_double_word_int64s,
                % XXX ARG_PACK For bootstrapping.
                Params ^ ddp_allow_double_word_ints = yes,
                DWKind = dw_uint64
            )
        then
            ArgPosWidth = apw_double(ArgOnlyOffset, CellOffset, DWKind),
            NextAOWordNum = CurAOWordNum + 2,
            NextCellWordNum = CurCellWordNum + 2
        else
            ArgPosWidth = apw_full(ArgOnlyOffset, CellOffset),
            NextAOWordNum = CurAOWordNum + 1,
            NextCellWordNum = CurCellWordNum + 1
        ),
        HeadArgRepn = ctor_arg_repn(ArgName, ArgType, ArgPosWidth, ArgContext),
        decide_complex_du_ctor_remote_args_loop(ModuleInfo, Params,
            ComponentTypeMap, NextAOWordNum, NextCellWordNum,
            Args, TailArgRepns),
        ArgRepns = [HeadArgRepn | TailArgRepns]
    ).

%---------------------------------------------------------------------------%

:- pred decide_complex_du_ctor_local_args(decide_du_params::in,
    component_type_map::in, local_sectag::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out) is det.

decide_complex_du_ctor_local_args(Params, ComponentTypeMap,
        LocalSectag, CtorArgs, CtorArgRepns) :-
    % A word representing a constructor with locally packed arguments contains,
    % in order:
    %
    % - the two or three bits containing the primary tag,
    %
    % - the zero or more bits containing the local secondary tag, and
    %
    % - one or more bit fields, one for each argument, each containing
    %   zero or more bits: zero bits for dummy type arguments, one or more bits
    %   for non-dummy type arguments.
    %
    % We have two rules governing how the bit fields of the arguments
    % are allocated.
    %
    % - The bit fields of arguments are allocated in a contigous region
    %   next to the primary and secondary tag bits.
    %
    % - Given two arguments i and j, if i < j, then argument i will be
    %   allocated a bit field containing *more* significant bits than
    %   argument j. This means that the argument next to the local secondary
    %   tag will be the *last* (non-dummy type) argument of the constructor,
    %   not the first.
    %
    % The reason for the second rule is this allows the automatically generated
    % comparison predicate for the type to compare any consecutive sequence of
    % two or more arguments to be compared at once, if they all compare
    % as unsigned.

    MaybePrimaryTags = Params ^ ddp_maybe_primary_tags,
    (
        MaybePrimaryTags = max_primary_tag(_, NumPtagBits)
    ;
        MaybePrimaryTags = no_primary_tags,
        % We ruled this out in compute_local_packable_functors.
        unexpected($pred, "MaybePrimaryTags = no_primary_tags")
    ),
    ArgOnlyOffset = arg_only_offset(-2),
    CellOffset = cell_offset(-2),
    LocalSectag = local_sectag(_, _, SectagBits),
    SectagBits = sectag_bits(NumSectagBits, _),
    NumPrimSectagBits = NumPtagBits + uint8.cast_to_int(NumSectagBits),
    decide_tagword_args(Params, ComponentTypeMap,
        ArgOnlyOffset, CellOffset, NumPrimSectagBits, CtorArgs, CtorArgRepns).

:- pred decide_tagword_args(decide_du_params::in, component_type_map::in,
    arg_only_offset::in, cell_offset::in, int::in,
    list(constructor_arg)::in, list(constructor_arg_repn)::out) is det.

decide_tagword_args(Params, ComponentTypeMap,
        ArgOnlyOffset, CellOffset, NumFixedBits, CtorArgs, CtorArgRepns) :-
    pair_args_with_packable(Params, ComponentTypeMap,
        CtorArgs, CtorArgsPackables),
    decide_packed_arg_word_loop(do_not_treat_as_first_arg,
        ArgOnlyOffset, CellOffset, NumFixedBits, _,
        CtorArgsPackables, CtorArgRepns).

:- type maybe_treat_as_first_arg
    --->    do_not_treat_as_first_arg
    ;       treat_as_first_arg.

    % Assign representations to the arguments packed together into a single
    % word at the offset given by ArgOnlyOffset and CellOffset.
    %
    % We assign bit fields to the packed arguments in reverse order,
    % putting the last argument next to the fixed bits (which occupy
    % the low-order NumPrefixBits bits of the word), then the next last,
    % and so on. We do this to put earlier arguments into more significant
    % bits than later arguments, which will let us use a single unsigned
    % comparison to compare any run of consecutive unsigned arguments.
    %
    % We assume that the ArgPackable list ends with a nondummy argument;
    % this is intended to be ensured by having our caller compute ArgPackable
    % by calling find_word_packable_args. We do *not* assume that ArgPackable
    % *starts* with a nondummy argument, but we assign the apw_none_nowhere
    % representation to any dummy in any such initial subsequence.
    %
:- pred decide_packed_arg_word_loop(maybe_treat_as_first_arg::in,
    arg_only_offset::in, cell_offset::in, int::in, int::out,
    assoc_list(constructor_arg, packable_kind)::in,
    list(constructor_arg_repn)::out) is det.

decide_packed_arg_word_loop(_, _, _, NumPrefixBits, NumPrefixBits,
        [], []).
decide_packed_arg_word_loop(TreatAsFirst, ArgOnlyOffset, CellOffset,
        NumPrefixBits, NextShift,
        [ArgPackable | ArgsPackables], [ArgRepn | ArgRepns]) :-
    ArgPackable = Arg - Packable,
    (
        Packable = packable_n_bits(NumArgBits, FillKind),
        decide_packed_arg_word_loop(do_not_treat_as_first_arg,
            ArgOnlyOffset, CellOffset, NumPrefixBits, CurShift,
            ArgsPackables, ArgRepns),
        ArgMask = (1 << NumArgBits) - 1,
        (
            TreatAsFirst = treat_as_first_arg,
            ArgPosWidth = apw_partial_first(ArgOnlyOffset, CellOffset,
                arg_shift(CurShift), arg_num_bits(NumArgBits),
                arg_mask(ArgMask), FillKind)
        ;
            TreatAsFirst = do_not_treat_as_first_arg,
            ArgPosWidth = apw_partial_shifted(ArgOnlyOffset, CellOffset,
                arg_shift(CurShift), arg_num_bits(NumArgBits),
                arg_mask(ArgMask), FillKind)
        ),
        NextShift = CurShift + NumArgBits
    ;
        Packable = packable_dummy,
        decide_packed_arg_word_loop(TreatAsFirst,
            ArgOnlyOffset, CellOffset, NumPrefixBits, CurShift,
            ArgsPackables, ArgRepns),
        (
            TreatAsFirst = treat_as_first_arg,
            ArgPosWidth = apw_none_nowhere
        ;
            TreatAsFirst = do_not_treat_as_first_arg,
            ArgPosWidth = apw_none_shifted(ArgOnlyOffset, CellOffset)
        ),
        NextShift = CurShift
    ),
    Arg = ctor_arg(ArgName, ArgType, ArgContext),
    ArgRepn = ctor_arg_repn(ArgName, ArgType, ArgPosWidth, ArgContext).

%---------------------------------------------------------------------------%

    % Find an initial subsequence of arguments that may all be packed
    % together into a word which has NumAvailBits available. Return these
    % arguments with their packability information, and return the remaining
    % arguments. Make sure that the last packable argument is not of a dummy
    % type; if the list of packable arguments ends with a run of one or more
    % arguments of dummy types, return these as part of the LeftOverArgs.
    %
:- pred find_word_packable_args(decide_du_params::in, component_type_map::in,
    int::in, int::in, int::out, list(constructor_arg)::in,
    assoc_list(constructor_arg, packable_kind)::out,
    list(constructor_arg)::out) is det.

find_word_packable_args(_, _, _, !NumUsedBits, [], [], []).
find_word_packable_args(Params, ComponentTypeMap, NumAvailBits, !NumUsedBits,
        [Arg | Args], ArgsPackables, LeftOverArgs) :-
    Arg = ctor_arg(_ArgName, ArgType, _ArgContext),
    InitNumUsedBits = !.NumUsedBits,
    ( if
        may_pack_arg_type(Params, ComponentTypeMap, ArgType, Packable),
        (
            Packable = packable_n_bits(NumArgBits, _FillKind),
            NextNumAvailBits = NumAvailBits - NumArgBits,
            !:NumUsedBits = !.NumUsedBits + NumArgBits
        ;
            Packable = packable_dummy,
            NextNumAvailBits = NumAvailBits
        ),
        NextNumAvailBits >= 0
    then
        find_word_packable_args(Params, ComponentTypeMap,
            NextNumAvailBits, !NumUsedBits,
            Args, TailArgsPackables, TailLeftOverArgs),
        % If we have allocated any bits at all to nondummy values,
        % do not let the ArgsPackables list end with one or more dummy args.
        % The recursive application of this principle ensures that
        % if ArgsPackables contains any nondummy args at all, then
        % the last element in ArgsPackables will be a non-dummy.
        ( if
            InitNumUsedBits > 0,
            TailArgsPackables = [],
            Packable = packable_dummy
        then
            ArgsPackables = [],
            LeftOverArgs = [Arg | Args]
        else
            HeadArgPackable = Arg - Packable,
            ArgsPackables = [HeadArgPackable | TailArgsPackables],
            LeftOverArgs = TailLeftOverArgs
        )
    else
        ArgsPackables = [],
        LeftOverArgs = [Arg | Args]
    ).

    % Pair each of the given args, which must be packable,
    % with its packability information.
    %
:- pred pair_args_with_packable(decide_du_params::in, component_type_map::in,
    list(constructor_arg)::in, assoc_list(constructor_arg, packable_kind)::out)
    is det.

pair_args_with_packable(_, _, [], []).
pair_args_with_packable(Params, ComponentTypeMap,
        [Arg | Args], [ArgPackable | ArgsPackables]) :-
    Arg = ctor_arg(_ArgName, ArgType, _ArgContext),
    ( if may_pack_arg_type(Params, ComponentTypeMap, ArgType, Packable) then
        ArgPackable = Arg - Packable
    else
        unexpected($pred, "not packable")
    ),
    pair_args_with_packable(Params, ComponentTypeMap, Args, ArgsPackables).

:- pred may_pack_arg_type(decide_du_params::in, component_type_map::in,
    mer_type::in, packable_kind::out) is semidet.

may_pack_arg_type(Params, ComponentTypeMap, ArgType, PackableKind) :-
    % XXX ARG_PACK Make this code dereference eqv types,
    % subject to all types involved having the same visibility.
    type_to_ctor(ArgType, ArgTypeCtor),

    ( if map.search(ComponentTypeMap, ArgTypeCtor, ComponentKind) then
        ComponentKind = packable(PackableKind),
        (
            PackableKind = packable_n_bits(NumArgBits, _FillKind),
            NumArgBits < Params ^ ddp_arg_pack_bits
        ;
            PackableKind = packable_dummy,
            % XXX ARG_PACK For bootstrapping.
            Params ^ ddp_allow_packing_dummies = yes
        )
    else
        ArgType = builtin_type(ArgBuiltinType),
        (
            ArgBuiltinType = builtin_type_int(ArgIntType),
            Params ^ ddp_allow_packing_ints = yes,
            (
                (
                    ArgIntType = int_type_int8,
                    NumArgBits = 8,
                    FillKind = fill_int8
                ;
                    ArgIntType = int_type_int16,
                    NumArgBits = 16,
                    FillKind = fill_int16
                ;
                    ArgIntType = int_type_int32,
                    NumArgBits = 32,
                    NumArgBits < Params ^ ddp_arg_pack_bits,
                    FillKind = fill_int32
                )
            ;
                (
                    ArgIntType = int_type_uint8,
                    NumArgBits = 8,
                    FillKind = fill_uint8
                ;
                    ArgIntType = int_type_uint16,
                    NumArgBits = 16,
                    FillKind = fill_uint16
                ;
                    ArgIntType = int_type_uint32,
                    NumArgBits = 32,
                    NumArgBits < Params ^ ddp_arg_pack_bits,
                    FillKind = fill_uint32
                )
            )
        ;
            ArgBuiltinType = builtin_type_char,
            Params ^ ddp_allow_packing_chars = yes,
            % Section 2.4 of the Unicode standard says that "The codespace
            % consists of the integers from 0 to 10FFFF", which means that
            % all Unicode characters fit into 21 bits.
            NumArgBits = 21,
            FillKind = fill_char21
        ),
        PackableKind = packable_n_bits(NumArgBits, FillKind)
    ).

%---------------------------------------------------------------------------%

:- pred assign_tags_to_constants(type_ctor::in, ptag::in, int::in,
    sectag_bits::in, lsectag_mask::in, uint::in, uint::out,
    list(constructor)::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_tags_to_constants(_, _, _, _, _, !CurSectag, [], !CtorTagMap).
assign_tags_to_constants(TypeCtor, Ptag, NumPtagBits, SectagBits, MustMask,
        !CurSectag, [Ctor | Ctors], !CtorTagMap) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, SymName, _Args, Arity,
        _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    Ptag = ptag(PtagUint8),
    PrimSec = (!.CurSectag << NumPtagBits) \/ uint8.cast_to_uint(PtagUint8),
    LocalSectag = local_sectag(!.CurSectag, PrimSec, SectagBits),
    ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, MustMask),
    map.det_insert(ConsId, ConsTag, !CtorTagMap),
    !:CurSectag = !.CurSectag + 1u,
    assign_tags_to_constants(TypeCtor, Ptag, NumPtagBits,
        SectagBits, MustMask, !CurSectag, Ctors, !CtorTagMap).

:- pred assign_tags_to_local_packed_functors(type_ctor::in, ptag::in, int::in,
    sectag_bits::in, uint::in, list(constructor)::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_tags_to_local_packed_functors(_, _, _, _, _, [], !CtorTagMap).
assign_tags_to_local_packed_functors(TypeCtor, Ptag, NumPtagBits, SectagBits,
        CurSectag, [Ctor | Ctors], !CtorTagMap) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, SymName, _Args, Arity,
        _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    Ptag = ptag(PtagUint8),
    PrimSec = (CurSectag << NumPtagBits) \/ uint8.cast_to_uint(PtagUint8),
    LocalSectag = local_sectag(CurSectag, PrimSec, SectagBits),
    ConsTag = local_args_tag(local_args_not_only_functor(Ptag, LocalSectag)),
    map.det_insert(ConsId, ConsTag, !CtorTagMap),
    assign_tags_to_local_packed_functors(TypeCtor, Ptag, NumPtagBits,
        SectagBits, CurSectag + 1u, Ctors, !CtorTagMap).

:- pred assign_tags_to_direct_arg_functors(type_ctor::in,
    uint8::in, uint8::in, uint8::out,
    list(constructor)::in, list(constructor)::in, list(constructor)::out,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_tags_to_direct_arg_functors(_, _, !CurPtag, [], _, [], !CtorTagMap).
assign_tags_to_direct_arg_functors(TypeCtor, MaxPtagUint8, !CurPtagUint8,
        [DirectArgCtor | DirectArgCtors], NonDirectArgCtors, LeftOverCtors,
        !CtorTagMap) :-
    DirectArgCtor = ctor(_Ordinal, _MaybeExistConstraints, Name, _Args, Arity,
        _Context),
    ConsId = cons(Name, Arity, TypeCtor),
    ( if
        % If we are about to run out of unshared tags, stop, and return
        % the leftovers.
        !.CurPtagUint8 = MaxPtagUint8,
        ( DirectArgCtors = [_ | _]
        ; NonDirectArgCtors = [_ | _]
        )
    then
        LeftOverCtors = [DirectArgCtor | DirectArgCtors]
    else
        ConsTag = direct_arg_tag(ptag(!.CurPtagUint8)),
        map.det_insert(ConsId, ConsTag, !CtorTagMap),
        !:CurPtagUint8 = !.CurPtagUint8 + 1u8,
        assign_tags_to_direct_arg_functors(TypeCtor, MaxPtagUint8,
            !CurPtagUint8, DirectArgCtors, NonDirectArgCtors, LeftOverCtors,
            !CtorTagMap)
    ).

:- pred assign_unshared_then_shared_remote_args_tags(type_ctor::in,
    uint8::in, uint8::in, list(constructor)::in,
    uint::out, cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_unshared_then_shared_remote_args_tags(_, _, _, [], 0u, !CtorTagMap).
assign_unshared_then_shared_remote_args_tags(TypeCtor, MaxPtagUint8,
        !.CurPtagUint8, [Ctor | Ctors], NumRemoteSectags, !CtorTagMap) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, Name, _Args, Arity,
        _Context),
    ConsId = cons(Name, Arity, TypeCtor),
    ( if
        % If we are about to run out of unshared tags, start assigning
        % shared remote tags instead.
        !.CurPtagUint8 = MaxPtagUint8,
        Ctors = [_ | _]
    then
        CurRemoteSectag0 = 0u,
        assign_shared_remote_args_tags(TypeCtor,
            ptag(!.CurPtagUint8), [Ctor | Ctors],
            CurRemoteSectag0, CurRemoteSectag, !CtorTagMap),
        % We assigned remote sec tags 0 .. CurRemoteSectag-1,
        % which is CurRemoteSectag sec tags.
        NumRemoteSectags = CurRemoteSectag
    else
        ConsTag = remote_args_tag(remote_args_unshared(ptag(!.CurPtagUint8))),
        map.det_insert(ConsId, ConsTag, !CtorTagMap),
        !:CurPtagUint8 = !.CurPtagUint8 + 1u8,
        assign_unshared_then_shared_remote_args_tags(TypeCtor, MaxPtagUint8,
            !.CurPtagUint8, Ctors, NumRemoteSectags, !CtorTagMap)
    ).

:- pred assign_shared_remote_args_tags(type_ctor::in, ptag::in,
    list(constructor)::in, uint::in, uint::out,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_shared_remote_args_tags(_, _, [], !CurRemoteSectag, !CtorTagMap).
assign_shared_remote_args_tags(TypeCtor, Ptag, [Ctor | Ctors],
        !CurRemoteSectag, !CtorTagMap) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, SymName, _Args, Arity,
        _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    % The rsectag_word part of the tag can be overridden later.
    RemoteSectag = remote_sectag(!.CurRemoteSectag, rsectag_word),
    ConsTag = remote_args_tag(remote_args_shared(Ptag, RemoteSectag)),
    map.det_insert(ConsId, ConsTag, !CtorTagMap),
    !:CurRemoteSectag = !.CurRemoteSectag + 1u,
    assign_shared_remote_args_tags(TypeCtor, Ptag, Ctors,
        !CurRemoteSectag, !CtorTagMap).

:- pred assign_ctor_remote_args_tags(type_ctor::in,
    list(constructor)::in, uint::in, uint::out,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out) is det.

assign_ctor_remote_args_tags(_, [], !CurData, !CtorTagMap).
assign_ctor_remote_args_tags(TypeCtor, [Ctor | Ctors],
        !CurData, !CtorTagMap) :-
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, SymName, _Args, Arity,
        _Context),
    ConsId = cons(SymName, Arity, TypeCtor),
    ConsTag = remote_args_tag(remote_args_ctor(!.CurData)),
    map.det_insert(ConsId, ConsTag, !CtorTagMap),
    !:CurData = !.CurData + 1u,
    assign_ctor_remote_args_tags(TypeCtor, Ctors, !CurData, !CtorTagMap).

%---------------------------------------------------------------------------%

    % compute_local_packable_functors(Params, ComponentTypeMap, NumPtagBits,
    %     Constants, Functors, PackedFunctors, NonPackedFunctors,
    %     SectagBits, MustMask):
    %
    % Given a list of a type's Constants and nonconstant Functors,
    % find out which (if any) of the Functors can be represented by packing
    % the values of their arguments next to the primary and secondary tags.
    % The secondary tag may occupy zero bits, if there are no constants
    % and just one functor that can be packed this way.
    %
:- pred compute_local_packable_functors(decide_du_params::in,
    component_type_map::in, int::in,
    list(constructor)::in, list(constructor)::in,
    list(constructor)::out, list(constructor)::out,
    sectag_bits::out, lsectag_mask::out) is det.

compute_local_packable_functors(Params, ComponentTypeMap, NumPtagBits,
        Constants, Functors, PackedFunctors, NonPackedFunctors,
        SectagBits, MustMask) :-
    list.length(Constants, NumConstants),
    ( if
        Params ^ ddp_maybe_primary_tags = max_primary_tag(_, _),
        Params ^ ddp_allow_packing_local_sectags = yes
    then
        separate_out_local_sectag_packable(Params, ComponentTypeMap, Functors,
            SizedPackableFunctors, NonPackableFunctors)
    else
        SizedPackableFunctors = [],
        NonPackableFunctors = Functors
    ),
    (
        SizedPackableFunctors = [],
        PackedFunctors = [],
        NonPackedFunctors = Functors,
        num_bits_needed_for_n_things(NumConstants, NumSectagBits),
        MustMask = lsectag_always_rest_of_word
    ;
        SizedPackableFunctors = [_ | _],
        list.sort(compare_sized_packable_functors,
            SizedPackableFunctors, SortedSizedPackableFunctors),
        list.det_last(SortedSizedPackableFunctors, MaxPackableBits - _),
        list.length(SizedPackableFunctors, NumPackable),
        NumArgPackBits = Params ^ ddp_arg_pack_bits,
        trace [io(!IO), compile_time(flag("du_type_layout"))] (
            some [TraceNumSectagBits] (
                get_debug_output_stream(Params ^ ddp_globals,
                    Params ^ ddp_module_name, DebugStream, !IO),
                io.write_string(DebugStream,
                    "\nsized packable functors:\n", !IO),
                list.foldl(
                    output_sized_packable_functor(DebugStream,
                        yes({Params, ComponentTypeMap})),
                    SizedPackableFunctors, !IO),
                num_bits_needed_for_n_things(NumConstants + NumPackable,
                    TraceNumSectagBits),
                io.write_string(DebugStream, "NumPtagBits: ", !IO),
                io.write_line(DebugStream, NumPtagBits, !IO),
                io.write_string(DebugStream, "TraceNumSectagBits: ", !IO),
                io.write_line(DebugStream, TraceNumSectagBits, !IO),
                io.write_string(DebugStream, "MaxPackableBits: ", !IO),
                io.write_line(DebugStream, MaxPackableBits, !IO),
                io.write_string(DebugStream, "NumArgPackBits: ", !IO),
                io.write_line(DebugStream, NumArgPackBits, !IO)
            )
        ),
        ( if
            num_bits_needed_for_n_things(NumConstants + NumPackable,
                NumSectagBits0),
            NumPtagBits + NumSectagBits0 + MaxPackableBits =< NumArgPackBits
        then
            % We can pack all of PackableFunctors with their local sectag.
            assoc_list.values(SizedPackableFunctors, PackedFunctors),
            NonPackedFunctors = NonPackableFunctors,
            NumSectagBits = NumSectagBits0
        else
            % We *cannot* pack all of PackableFunctors with their
            % local sectag, either because some require too many bits
            % on their own, or because there are too many such functors,
            % requiring too many sectag bits to distinguish them, or both.
            %
            % Our objective is to assign local sectags to as many
            % PackableFunctors as can. This requires giving preference
            % to PackableFunctors that take up the least number of bits
            % themselves. For example, on a 64 bit platform where
            % the primary tag is 3 bits, if we have one constant,
            % then we could pack that constant together with either
            %
            % - just one PackableFunctor that occupies 60 bits
            %   (since distinguishing one constant and two nonconstants
            %   would require two sectag bits, and 3 + 2 + 60 > 64), or
            %   (for example)
            %
            % - 15 PackableFunctors that each occupy up to 57 bits
            %   (because distinguishing them from the constant and each other
            %   requires 4 sectag bits, and 3 + 4 + 57 =< 64).
            %
            % This is why we try to pack nonconstant functors in increasing
            % order of how many bits they need for just the arguments.
            %
            % We start by computing the number of sectag bits needed
            % for just the constants, and seeing what nonconstant functors
            % we can pack using just that number of sectag bits. We then
            % try to see if we can pack in more function symbols with
            % one more sectag bit. We keep increasing the number of sectag bits
            % until we can pack in no more.

            num_bits_needed_for_n_things(NumConstants, NumSectagBits0),
            NumSectagValues0 = 1 << NumSectagBits0,
            TakeLimit0 = NumSectagValues0 - NumConstants,
            take_local_packable_functors_constant_sectag_bits(Params,
                NumArgPackBits, NumPtagBits, NumSectagBits0,
                TakeLimit0, 0, _NumTaken,
                SortedSizedPackableFunctors,
                [], RevSizedPackedFunctors0, SizedNonPackedFunctors0),
            % _NumTaken may be 0 because TakeLimit0 was 0.
            take_local_packable_functors_incr_sectag_bits(Params,
                NumArgPackBits, NumPtagBits, NumSectagBits0, NumSectagBits,
                RevSizedPackedFunctors0, SizedNonPackedFunctors0,
                RevSizedPackedFunctors, SizedNonPackedFunctors),
            assoc_list.values(RevSizedPackedFunctors, RevPackedFunctors),
            assoc_list.values(SizedNonPackedFunctors,
                NonPackedPackableFunctors),
            % The sorting operates on the first arguments first,
            % which is the functor's ordinal number.
            list.sort(RevPackedFunctors, PackedFunctors),
            list.sort(NonPackableFunctors ++ NonPackedPackableFunctors,
                NonPackedFunctors)
        ),
        (
            PackedFunctors = [],
            MustMask = lsectag_always_rest_of_word
        ;
            PackedFunctors = [_ | _],
            MustMask = lsectag_must_be_masked
        )
    ),
    compute_sectag_bits(NumSectagBits, SectagBits).

:- pred compare_sized_packable_functors(
    pair(int, constructor)::in, pair(int, constructor)::in,
    comparison_result::out) is det.

compare_sized_packable_functors(SizeA - CtorA, SizeB - CtorB, Result) :-
    compare(SizeResult, SizeA, SizeB),
    (
        ( SizeResult = (<)
        ; SizeResult = (>)
        ),
        Result = SizeResult
    ;
        SizeResult = (=),
        % Keep the sort stable.
        OrdinalA = CtorA ^ cons_ordinal,
        OrdinalB = CtorB ^ cons_ordinal,
        compare(Result, OrdinalA, OrdinalB)
    ).

:- pred take_local_packable_functors_incr_sectag_bits(decide_du_params::in,
    int::in, int::in, int::in, int::out,
    assoc_list(int, constructor)::in, assoc_list(int, constructor)::in,
    assoc_list(int, constructor)::out, assoc_list(int, constructor)::out)
    is det.

take_local_packable_functors_incr_sectag_bits(Params,
        NumArgPackBits, NumPtagBits, NumSectagBits0, NumSectagBits,
        RevPackedFunctors0, NonPackedFunctors0,
        RevPackedFunctors, NonPackedFunctors) :-
    trace [io(!IO), compile_time(flag("du_type_layout"))] (
        get_debug_output_stream(Params ^ ddp_globals, Params ^ ddp_module_name,
            DebugStream, !IO),
        io.write_string(DebugStream, "\nstart of incr_sectag_bits:\n", !IO),
        io.write_string(DebugStream, "RevPackedFunctors0:\n", !IO),
        list.foldl(output_sized_packable_functor(DebugStream, maybe.no),
            RevPackedFunctors0, !IO),
        io.write_string(DebugStream, "NumArgPackBits: ", !IO),
        io.write_line(DebugStream, NumArgPackBits, !IO),
        io.write_string(DebugStream, "NumPtagBits: ", !IO),
        io.write_line(DebugStream, NumPtagBits, !IO),
        io.write_string(DebugStream, "NumSectagBits0: ", !IO),
        io.write_line(DebugStream, NumSectagBits0, !IO)
    ),
    ( if
        (
            RevPackedFunctors0 = []
        ;
            RevPackedFunctors0 = [MaxPackableBits - _ | _],
            NumPtagBits + NumSectagBits0 + MaxPackableBits + 1
                =< NumArgPackBits
        )
    then
        NumSectagBits1 = NumSectagBits0 + 1,
        TakeLimit = (1 << NumSectagBits1) - (1 << NumSectagBits0),
        take_local_packable_functors_constant_sectag_bits(Params,
            NumArgPackBits, NumPtagBits, NumSectagBits1,
            TakeLimit, 0, NumTaken,
            NonPackedFunctors0,
            RevPackedFunctors0, RevPackedFunctors1, NonPackedFunctors1),
        ( if NumTaken > 0 then
            take_local_packable_functors_incr_sectag_bits(Params,
                NumArgPackBits, NumPtagBits, NumSectagBits1, NumSectagBits,
                RevPackedFunctors1, NonPackedFunctors1,
                RevPackedFunctors, NonPackedFunctors)
        else
            NumSectagBits = NumSectagBits0,
            RevPackedFunctors = RevPackedFunctors0,
            NonPackedFunctors = NonPackedFunctors0
        )
    else
        NumSectagBits = NumSectagBits0,
        RevPackedFunctors = RevPackedFunctors0,
        NonPackedFunctors = NonPackedFunctors0
    ).

:- pred take_local_packable_functors_constant_sectag_bits(decide_du_params::in,
    int::in, int::in, int::in, int::in, int::in, int::out,
    assoc_list(int, constructor)::in,
    assoc_list(int, constructor)::in, assoc_list(int, constructor)::out,
    assoc_list(int, constructor)::out) is det.

take_local_packable_functors_constant_sectag_bits(_, _, _, _, _, !NumTaken, [],
        !RevPackedFunctors, []).
take_local_packable_functors_constant_sectag_bits(Params, ArgPackBits,
        PtagBits, SectagBits, TakeLimit, !NumTaken,
        [PackableFunctor | PackableFunctors],
        !RevPackedFunctors, NonPackedFunctors) :-
    PackableFunctor = PackableBits - _Functor,
    trace [io(!IO), compile_time(flag("du_type_layout"))] (
        get_debug_output_stream(Params ^ ddp_globals, Params ^ ddp_module_name,
            DebugStream, !IO),
        io.write_string(DebugStream, "\nconstant_sectag_bits test:\n", !IO),
        io.write_string(DebugStream, "PackableFunctor: ", !IO),
        output_sized_packable_functor(DebugStream, maybe.no,
            PackableFunctor, !IO),
        io.write_string(DebugStream, "ArgPackBits: ", !IO),
        io.write_line(DebugStream, ArgPackBits, !IO),
        io.write_string(DebugStream, "PtagBits: ", !IO),
        io.write_line(DebugStream, PtagBits, !IO),
        io.write_string(DebugStream, "SectagBits: ", !IO),
        io.write_line(DebugStream, SectagBits, !IO),
        io.write_string(DebugStream, "TakeLimit: ", !IO),
        io.write_line(DebugStream, TakeLimit, !IO)
    ),
    ( if
        TakeLimit > 0,
        PtagBits + SectagBits + PackableBits =< ArgPackBits
    then
        trace [io(!IO), compile_time(flag("du_type_layout"))] (
            get_debug_output_stream(Params ^ ddp_globals,
                Params ^ ddp_module_name, DebugStream, !IO),
            io.write_string(DebugStream, "TAKEN\n", !IO)
        ),
        !:RevPackedFunctors = [PackableFunctor | !.RevPackedFunctors],
        !:NumTaken = !.NumTaken + 1,
        take_local_packable_functors_constant_sectag_bits(Params, ArgPackBits,
            PtagBits, SectagBits, TakeLimit - 1, !NumTaken, PackableFunctors,
            !RevPackedFunctors, NonPackedFunctors)
    else
        trace [io(!IO), compile_time(flag("du_type_layout"))] (
            get_debug_output_stream(Params ^ ddp_globals,
                Params ^ ddp_module_name, DebugStream, !IO),
            io.write_string(DebugStream, "NOT TAKEN\n", !IO)
        ),
        NonPackedFunctors = [PackableFunctor | PackableFunctors]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Pass 2b.
%

    % After deciding the representation of simple and complex du types,
    % use that to derive the representation of subtypes.
    %
:- pred decide_if_subtype(type_table::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    pair(type_ctor, hlds_type_defn)::in, pair(type_ctor, hlds_type_defn)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

decide_if_subtype(OldTypeTable, NonSubTypeCtorsTypeDefns,
        TypeCtorTypeDefn0, TypeCtorTypeDefn, !Specs) :-
    TypeCtorTypeDefn0 = TypeCtor - TypeDefn0,
    get_type_defn_body(TypeDefn0, Body0),
    (
        Body0 = hlds_du_type(BodyDu0),
        BodyDu0 = type_body_du(Ctors, MaybeSuperType, _MaybeCanonical,
            MaybeRepn0, _MaybeForeign),
        (
            MaybeSuperType = subtype_of(SuperType),
            expect(unify(MaybeRepn0, no), $pred,
                "type representation already decided for subtype"),
            ( if
                type_to_ctor(SuperType, SuperTypeCtor),
                get_base_type_ctor(OldTypeTable, SuperTypeCtor, BaseTypeCtor),
                search_du_type_repn(NonSubTypeCtorsTypeDefns,
                    BaseTypeCtor, BaseRepn)
            then
                make_subtype_type_repn(TypeCtor, Ctors, BaseRepn, Repn),
                BodyDu = BodyDu0 ^ du_type_repn := yes(Repn),
                Body = hlds_du_type(BodyDu),
                set_type_defn_body(Body, TypeDefn0, TypeDefn),
                TypeCtorTypeDefn = TypeCtor - TypeDefn
            else
                unexpected($pred, "missing base type representation")
            )
        ;
            MaybeSuperType = not_a_subtype,
            unexpected($pred, "not subtype")
        )
    ;
        Body0 = hlds_abstract_type(AbstractDetails),
        (
            AbstractDetails = abstract_subtype(_),
            TypeCtorTypeDefn = TypeCtorTypeDefn0
        ;
            ( AbstractDetails = abstract_type_fits_in_n_bits(_)
            ; AbstractDetails = abstract_dummy_type
            ; AbstractDetails = abstract_notag_type
            ; AbstractDetails = abstract_type_general
            ; AbstractDetails = abstract_solver_type
            ),
            unexpected($pred, "not subtype")
        )
    ;
        ( Body0 = hlds_foreign_type(_)
        ; Body0 = hlds_eqv_type(_)
        ; Body0 = hlds_solver_type(_)
        ),
        unexpected($pred, "not subtype")
    ).

:- pred search_du_type_repn(assoc_list(type_ctor, hlds_type_defn)::in,
    type_ctor::in, du_type_repn::out) is semidet.

search_du_type_repn(TypeDefns, TypeCtor, DuTypeRepn) :-
    assoc_list.search(TypeDefns, TypeCtor, TypeRepn),
    get_type_defn_body(TypeRepn, TypeBody),
    TypeBody = hlds_du_type(TypeBodyDu),
    TypeBodyDu ^ du_type_repn = yes(DuTypeRepn).

:- pred make_subtype_type_repn(type_ctor::in, one_or_more(constructor)::in,
    du_type_repn::in, du_type_repn::out) is det.

make_subtype_type_repn(TypeCtor, OoMCtors, BaseRepn, Repn) :-
    BaseRepn = du_type_repn(BaseCtorRepns, _BaseCtorRepnMap,
        _BaseCheaperTagTest, BaseDuTypeKind, MaybeBaseDirectArgFunctors),

    Ctors = one_or_more_to_list(OoMCtors),
    list.map_foldl(make_subtype_ctor_repn(BaseCtorRepns),
        Ctors, CtorRepns, map.init, CtorRepnMap),

    compute_cheaper_tag_test(TypeCtor, CtorRepns, CheaperTagTest),

    (
        BaseDuTypeKind = du_type_kind_mercury_enum,
        % The subtype must be an enum even if it has only one constructor as
        % the term may be upcast. This breaks an assumption for non-subtypes.
        DuTypeKind = du_type_kind_mercury_enum
    ;
        BaseDuTypeKind = du_type_kind_foreign_enum(_),
        unexpected($pred, "du_type_kind_foreign_enum")
    ;
        BaseDuTypeKind = du_type_kind_direct_dummy,
        DuTypeKind = du_type_kind_direct_dummy
    ;
        BaseDuTypeKind = du_type_kind_notag(SingleFunctorName, _BaseArgType,
            _MaybeBaseArgName),
        ( if
            CtorRepns = [SingleCtorRepn],
            SingleCtorRepn = ctor_repn(_Ordinal, _MaybeExist,
                SingleFunctorName, _ConsTag, [SingleArgRepn], 1, _Context)
        then
            SingleArgRepn = ctor_arg_repn(MaybeSingleArgFieldName,
                SingleArgType, _SingleArgPosWidth, _SingleArgContext),
            (
                MaybeSingleArgFieldName = no,
                MaybeSingleArgName = no
            ;
                MaybeSingleArgFieldName =
                    yes(ctor_field_name(SingleArgSymName, _FieldContext)),
                MaybeSingleArgName = yes(unqualify_name(SingleArgSymName))
            ),
            DuTypeKind = du_type_kind_notag(SingleFunctorName, SingleArgType,
                MaybeSingleArgName)
        else
            unexpected($pred, "wrong ctor for notag subtype")
        )
    ;
        BaseDuTypeKind = du_type_kind_general,
        DuTypeKind = du_type_kind_general
    ),

    (
        MaybeBaseDirectArgFunctors = no,
        MaybeDirectArgFunctors = no
    ;
        MaybeBaseDirectArgFunctors = yes(BaseDirectArgFunctors),
        list.filter(has_matching_constructor(Ctors),
            BaseDirectArgFunctors, DirectArgFunctors),
        MaybeDirectArgFunctors = yes(DirectArgFunctors)
    ),

    Repn = du_type_repn(CtorRepns, CtorRepnMap, CheaperTagTest, DuTypeKind,
        MaybeDirectArgFunctors).

:- pred make_subtype_ctor_repn(list(constructor_repn)::in,
    constructor::in, constructor_repn::out,
    ctor_name_to_repn_map::in, ctor_name_to_repn_map::out) is det.

make_subtype_ctor_repn(BaseCtorRepns, Ctor, CtorRepn, !CtorRepnMap) :-
    Ctor = ctor(Ordinal, MaybeExistConstraints, CtorName, CtorArgs, CtorArity,
        Context),
    UnqualCtorName = unqualify_name(CtorName),
    ( if
        search_ctor_repn_by_unqual_name(BaseCtorRepns, UnqualCtorName,
            CtorArity, BaseCtorRepn)
    then
        BaseCtorRepn = ctor_repn(_BaseOrdinal, _BaseMaybeExistConstraints,
            _BaseCtorName, BaseCtorTag, BaseCtorArgRepns, _BaseCtorArity,
            _BaseContext),
        CtorTag = BaseCtorTag,
        list.map_corresponding(make_subtype_constructor_arg_repn,
            CtorArgs, BaseCtorArgRepns, CtorArgRepns),
        CtorRepn = ctor_repn(Ordinal, MaybeExistConstraints, CtorName,
            CtorTag, CtorArgRepns, CtorArity, Context),
        insert_ctor_repn_into_map(CtorRepn, !CtorRepnMap)
    else
        unexpected($pred, "missing base ctor type repn")
    ).

:- pred search_ctor_repn_by_unqual_name(list(constructor_repn)::in,
    string::in, int::in, constructor_repn::out) is semidet.

search_ctor_repn_by_unqual_name([CtorRepn | CtorRepns], UnqualName, Arity,
        MatchingCtorRepn) :-
    ( if
        unqualify_name(CtorRepn ^ cr_name) = UnqualName,
        CtorRepn ^ cr_num_args = Arity
    then
        MatchingCtorRepn = CtorRepn
    else
        search_ctor_repn_by_unqual_name(CtorRepns, UnqualName, Arity,
            MatchingCtorRepn)
    ).

:- pred make_subtype_constructor_arg_repn(constructor_arg::in,
    constructor_arg_repn::in, constructor_arg_repn::out) is det.

make_subtype_constructor_arg_repn(CtorArg, BaseCtorArgRepn, CtorArgRepn) :-
    CtorArg = ctor_arg(MaybeFieldName, ArgType, Context),
    BaseCtorArgRepn = ctor_arg_repn(_MaybeBaseFieldName, _BaseArgType,
        ArgPosWidth, _BaseContext),
    CtorArgRepn = ctor_arg_repn(MaybeFieldName, ArgType, ArgPosWidth, Context).

:- pred has_matching_constructor(list(constructor)::in, sym_name_arity::in)
    is semidet.

has_matching_constructor(Ctors, SymNameArity) :-
    list.any_true(is_matching_constructor(SymNameArity), Ctors).

:- pred is_matching_constructor(sym_name_arity::in, constructor::in)
    is semidet.

is_matching_constructor(SymNameArity, Ctor) :-
    SymNameArity = sym_name_arity(SymName, Arity),
    Ctor = ctor(_Ordinal, _MaybeExist, SymName, _Args, Arity, _Context).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates.
% XXX TYPE_REPN Rationalise the order of the predicates from here onwards.
%

:- pred is_direct_arg_ctor(component_type_map::in, module_name::in,
    type_status::in, bool::in, bool::in,
    list(sym_name_arity)::in, constructor::in) is semidet.

is_direct_arg_ctor(ComponentTypeMap, TypeCtorModule, TypeStatus,
        TypeIsImported, TypeDefinedHere, AssertedDirectArgCtors, Ctor) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, ConsSymName, ConsArgs,
        ConsArity, _CtorContext),
    MaybeExistConstraints = no_exist_constraints,
    ConsArgs = [ConsArg],
    expect(unify(ConsArity, 1), $pred, "ConsArity != 1"),
    ConsArg = ctor_arg(_MaybeFieldName, ArgType, _ArgContext),
    type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeCtorArgTypes),

    ConsConsId = sym_name_arity(ConsSymName, ConsArity),
    ( if
        % Trust the `direct_arg' attribute of an imported type.
        TypeIsImported = yes,
        list.contains(AssertedDirectArgCtors, ConsConsId)
    then
        ArgCond = direct_arg_asserted
    else if
        % Tuples are always acceptable argument types as they are represented
        % by word-aligned vector pointers.
        % Strings are *not* always word-aligned (yet) so are not acceptable.
        type_ctor_is_tuple(ArgTypeCtor)
    then
        ArgCond = arg_type_is_word_aligned_pointer
    else
        map.search(ComponentTypeMap, ArgTypeCtor, ArgComponentKind),
        ArgComponentKind = is_word_aligned_ptr(WordAlignedWhy),
        (
            WordAlignedWhy = foreign_type_assertion,
            ArgCond = arg_type_is_word_aligned_pointer
        ;
            WordAlignedWhy = mercury_type_defn(ArgTypeDefn),
            % The argument type is not a foreign type.

            % XXX TYPE_REPN Should be able to delete this test, since it
            % duplicates one that was done when adding this entry to
            % ComponentTypeMap.
            ArgTypeCtorArgTypes = [],
            % XXX We could let this be a subset of the type params, but that
            % would require the runtime system to be able to handle variables
            % in the argument type, during unification and comparison
            % (mercury_unify_compare_body.h) during deconstruction
            % (mercury_ml_expand_body.h), during deep copying
            % (mercury_deep_copy_body.h), and maybe during some other
            % operations.

            get_type_defn_body(ArgTypeDefn, ArgTypeDefnBody),
            ArgTypeDefnBody = hlds_du_type(ArgTypeDefnBodyDu),
            ArgTypeDefnBodyDu = type_body_du(_ArgCtors, _ArgMaybeSuperType,
                _ArgMaybeUserEqComp, _ArgMaybeRepn, ArgMaybeForeign),

            ArgMaybeForeign = no,

            ( if
                TypeDefinedHere = yes,
                list.contains(AssertedDirectArgCtors, ConsConsId)
            then
                ArgCond = direct_arg_asserted
            else
                ArgTypeCtor = type_ctor(ArgTypeCtorSymName, _ArgTypeCtorArity),
                sym_name_get_module_name(ArgTypeCtorSymName,
                    ArgTypeCtorModule),
                ( if TypeCtorModule = ArgTypeCtorModule then
                    get_type_defn_status(ArgTypeDefn, ArgTypeStatus),
                    ArgCond = arg_type_defined_in_same_module(ArgTypeStatus)
                else
                    ArgCond = arg_type_defined_in_different_module
                )
            )
        )
    ),
    module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = yes.

:- type direct_arg_cond
    --->    direct_arg_asserted
            % The constructor being checked has a single argument, and a
            % `where direct_arg' attribute asserts that the direct arg
            % representation may be used for the constructor.

    ;       arg_type_is_word_aligned_pointer
            % The constructor being checked has a single argument, and either
            % the argument has a builtin type that is represented with a
            % word-aligned pointer, or the argument has a foreign type with the
            % `word_aligned_pointer' assertion.

    ;       arg_type_defined_in_same_module(type_status)
            % The constructor being checked has a single argument, and the
            % argument type is defined in the same module as the constructor.
            % The argument type has the given import status.

    ;       arg_type_defined_in_different_module.
            % The constructor being checked has a single argument, and the
            % argument type is defined in a different module from the
            % constructor.

    % When this predicate is called, we should have checked that
    % the constructor has a single argument, and the argument has a type
    % such that the direct arg functor representation may apply
    % to the constructor. We still need to check that other modules
    % would infer the same type representation for the same constructor,
    % given that they may not have the same knowledge of the constructor's type
    % or the argument type.
    %
    % TypeStatus is import status of the type of the constructor being checked.
    %
:- func module_visibilities_allow_direct_arg(type_status, direct_arg_cond)
    = bool.

module_visibilities_allow_direct_arg(TypeStatus, ArgCond) = AllowDirectArg :-
    % XXX STATUS
    TypeStatus = type_status(OldImportStatus),
    (
        % If the outer type _definition_ is not exported from this module,
        % then the direct arg representation may be used. In the absence of
        % intermodule optimisation, only this module can [de]construct values
        % of this type.
        ( OldImportStatus = status_local
        ; OldImportStatus = status_abstract_exported
        ),
        AllowDirectArg = yes
    ;
        % If the outer type is opt-exported, another module may opt-import this
        % type, but abstract-import the argument type. It could not then infer
        % if the direct arg representation is required for any functors of the
        % outer type. The problem is overcome by adding `where direct_arg'
        % attributes to the opt-exported type definition in .opt files,
        % which state the functors that require the direct arg representation.
        OldImportStatus = status_opt_exported,
        AllowDirectArg = yes
    ;
        % If the outer type is exported from this module, then the direct arg
        % representation may be used, so long as any importing modules will
        % infer the same thing.
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ArgTypeStatus = type_status(ArgOldTypeStatus),
            ( if
                (
                    OldImportStatus = status_exported,
                    ArgOldTypeStatus = status_exported
                ;
                    % If the wrapper type is exported to submodules only, then
                    % the only modules whose access to the argument type
                    % matters is those submodules. Each of these
                    % ArgOldTypeStatus values allows these submodules access
                    % to the argument type's definition. The fact that some
                    % of them also allow other modules access doesn't matter,
                    % because (due to their lack of visibility to the wrapper
                    % type) the question of the wrapper's type representation
                    % won't come up in them.
                    OldImportStatus = status_exported_to_submodules,
                    ( ArgOldTypeStatus = status_exported
                    ; ArgOldTypeStatus = status_exported_to_submodules
                    ; ArgOldTypeStatus = status_abstract_exported
                    )
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % The direct arg representation is required if the type of the
        % constructor being checked is imported, and:
        % - if a `where direct_arg' attribute says so
        % - if the argument value is a word-aligned pointer
        % - if the argument type is imported from the same module
        OldImportStatus = status_imported(TypeImportLocn),
        (
            ( ArgCond = direct_arg_asserted
            ; ArgCond = arg_type_is_word_aligned_pointer
            ),
            AllowDirectArg = yes
        ;
            ArgCond = arg_type_defined_in_same_module(ArgTypeStatus),
            ( if
                ArgTypeStatus = type_status(status_imported(ArgImportLocn)),
                % If the argument type is only exported by an ancestor to its
                % submodules (of which we are one), the outer type must also
                % only be exported to submodules. Otherwise submodules and
                % non-submodules would infer different things.
                (
                    ArgImportLocn =
                        import_locn_ancestor_int0_implementation
                =>
                    TypeImportLocn =
                        import_locn_ancestor_int0_implementation
                )
            then
                AllowDirectArg = yes
            else
                AllowDirectArg = no
            )
        ;
            ArgCond = arg_type_defined_in_different_module,
            AllowDirectArg = no
        )
    ;
        % If the outer type is opt-imported, there will always be a
        % `where direct_arg' attribute on the type definition which states
        % if the direct argument representation must be used.
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ),
        (
            ArgCond = direct_arg_asserted,
            AllowDirectArg = yes
        ;
            ( ArgCond = arg_type_is_word_aligned_pointer
            ; ArgCond = arg_type_defined_in_same_module(_)
            ; ArgCond = arg_type_defined_in_different_module
            ),
            AllowDirectArg = no
        )
    ;
        ( OldImportStatus = status_external(_)
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_pseudo_imported
        ),
        unexpected($pred, "inappropriate status for type")
    ).

:- pred is_foreign_type_body_for_target(foreign_type_body::in,
    compilation_target::in, foreign_type_assertions::out) is semidet.

is_foreign_type_body_for_target(ForeignType, Target, Assertions) :-
    require_complete_switch [Target]
    (
        Target = target_c,
        ForeignType ^ c = yes(type_details_foreign(_, _, Assertions))
    ;
        Target = target_java,
        ForeignType ^ java = yes(type_details_foreign(_, _, Assertions))
    ;
        Target = target_csharp,
        ForeignType ^ csharp = yes(type_details_foreign(_, _, Assertions))
    ).

%---------------------------------------------------------------------------%

    % check_direct_arg_assertions(AssertedDirectArgCtors, NonDirectArgCtors,
    %   !Specs):
    %
    % The caller should pass the list of constructors that have been determined
    % not to be direct_arg constructors. If any of these constructors
    % nevertheless appears in AssertedDirectArgCtors, generate an error message
    % for it.
    %
:- pred check_direct_arg_assertions(list(sym_name_arity)::in,
    list(constructor)::in, list(error_spec)::in, list(error_spec)::out) is det.

check_direct_arg_assertions(_AssertedDirectArgCtors, [], !Specs).
check_direct_arg_assertions(AssertedDirectArgCtors, [Ctor | Ctors], !Specs) :-
    Ctor = ctor(_, _, SymName, _Args, Arity, Context),
    SymNameArity = sym_name_arity(SymName, Arity),
    ( if list.contains(AssertedDirectArgCtors, SymNameArity) then
        Pieces = [words("Error:"),
            unqual_sym_name_arity(sym_name_arity(SymName, Arity)),
            words("cannot be represented as a direct pointer"),
            words("to its sole argument."), nl],
        Spec = simplest_spec($pred, severity_error, phase_type_check,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    check_direct_arg_assertions(AssertedDirectArgCtors, Ctors, !Specs).

:- func constructor_to_sym_name_arity(constructor) = sym_name_arity.

constructor_to_sym_name_arity(ctor(_, _, Name, _Args, Arity, _)) =
    sym_name_arity(Name, Arity).

%---------------------------------------------------------------------------%

    % For data types with exactly two alternatives, one of which is a constant,
    % we can test against the constant (negating the result of the test,
    % if needed), since a test against a constant is cheaper than a tag test.
    %
:- pred compute_cheaper_tag_test(type_ctor::in, list(constructor_repn)::in,
    maybe_cheaper_tag_test::out) is det.

compute_cheaper_tag_test(TypeCtor, CtorRepns, CheaperTagTest) :-
    ( if CtorRepns = [CtorRepnA, CtorRepnB] then
        CtorRepnA = ctor_repn(_OrdinalA, _MaybeExistA, CtorSymNameA, CtorTagA,
            _CtorArgsA, CtorArityA, _CtorContextA),
        CtorRepnB = ctor_repn(_OrdinalB, _MaybeExistB, CtorSymNameB, CtorTagB,
            _CtorArgsB, CtorArityB, _CtorContextB),
        ( if
            CtorArityB = 0,
            CtorArityA > 0
        then
            ConsIdA = cons(CtorSymNameA, CtorArityA, TypeCtor),
            ConsIdB = cons(CtorSymNameB, CtorArityB, TypeCtor),
            CheaperTagTest = cheaper_tag_test(ConsIdA, CtorTagA,
                ConsIdB, CtorTagB)
        else if
            CtorArityA = 0,
            CtorArityB > 0
        then
            ConsIdA = cons(CtorSymNameA, CtorArityA, TypeCtor),
            ConsIdB = cons(CtorSymNameB, CtorArityB, TypeCtor),
            CheaperTagTest = cheaper_tag_test(ConsIdB, CtorTagB,
                ConsIdA, CtorTagA)
        else
            CheaperTagTest = no_cheaper_tag_test
        )
    else
        CheaperTagTest = no_cheaper_tag_test
    ).

%---------------------------------------------------------------------------%

:- pred inform_about_any_suboptimal_packing(decide_du_params::in,
    sym_name::in, prog_context::in,
    maybe(int)::in, list(constructor_arg_repn)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

inform_about_any_suboptimal_packing(Params, CtorSymName, CtorContext,
        MaybeSectagAsPseudoArg, CtorArgRepns, !Specs) :-
    % Find the number of words we would need to store all the sub-word-sized
    % arguments in CtorArgRepns using the first-fit-decreasing algorithm
    % (see the wikipedia page on "Bin_packing_problem"). If this number,
    % NumSubWordBins, is smaller than ActualNumSubWords, then generate
    % an informational message giving SubWordBins as one possible better
    % packing order of the arguments than the one in the program.

    (
        MaybeSectagAsPseudoArg = no,
        SubWords0 = [],
        ActualNumSubWords0 = 0
    ;
        MaybeSectagAsPseudoArg = yes(NumRemoteSectagBits),
        SubWords0 = [sub_word(NumRemoteSectagBits, field_id_sectag)],
        ActualNumSubWords0 = 1
    ),
    record_subword_args_and_count_their_words(CtorArgRepns, 0,
        SubWords0, SubWords, ActualNumSubWords0, ActualNumSubWords),
    list.sort(SubWords, SortedSubWords),
    list.reverse(SortedSubWords, RevSortedSubWords),
    BinSize = Params ^ ddp_arg_pack_bits,
    list.foldl(insert_subword_into_first_fit_bin(BinSize), RevSortedSubWords,
        [], SubWordBins),
    list.length(SubWordBins, NumSubWordBins),

    ( if NumSubWordBins < ActualNumSubWords then
        list.length(CtorArgRepns, CtorArity),
        CtorSymNameArity = sym_name_arity(CtorSymName, CtorArity),
        StartPieces = [words("The arguments of the constructor"),
            unqual_sym_name_arity(CtorSymNameArity),
            words("could be packed more tightly."),
            words("Here is one arrangement for the arguments"),
            words("which take up less than one word each"),
            words("that would allow better packing."),
            words("(The position of the word sized arguments"),
            words("does not affect the effectiveness of the packing.)"), nl],
        EndPieces = [blank_line,
            words("This arrangement of the sub-word-sized arguments"),
            words("would take"), int_fixed(NumSubWordBins),
            words(choose_number(SubWordBins, "word", "words")), suffix(","),
            words("whereas their current arrangement takes"),
            int_fixed(ActualNumSubWords),
            words((if ActualNumSubWords = 1 then "word" else "words")),
            suffix("."), nl],
        list.map(describe_sub_word_bin, SubWordBins, SubWordBinPieceLists),
        Pieces = StartPieces ++ list.condense(SubWordBinPieceLists)
            ++ EndPieces,
        Spec = simplest_spec($pred, severity_informational, phase_type_check,
            CtorContext, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- type field_id
    --->    field_id_sectag
    ;       field_id_name(int, string)
    ;       field_id_ordinal(int).

:- type sub_word
    --->    sub_word(
                sub_word_num_bits   :: int,
                sub_word_id         :: field_id
            ).

:- pred record_subword_args_and_count_their_words(
    list(constructor_arg_repn)::in, int::in,
    list(sub_word)::in, list(sub_word)::out, int::in, int::out) is det.

record_subword_args_and_count_their_words([], _, !SubWords, !NumWords).
record_subword_args_and_count_their_words([ArgRepn | ArgRepns], CurArgNum,
        !SubWords, !NumWords) :-
    ArgRepn = ctor_arg_repn(MaybeFieldName, _Type, PosWidth, _Context),
    (
        ( PosWidth = apw_full(_, _)
        ; PosWidth = apw_double(_, _, _)
        ; PosWidth = apw_none_nowhere
        ; PosWidth = apw_none_shifted(_, _)
        )
    ;
        (
            PosWidth = apw_partial_first(_, _, _, ArgNumBits, _, _),
            !:NumWords = !.NumWords + 1
        ;
            PosWidth = apw_partial_shifted(_, _, _, ArgNumBits, _, _)
        ),
        ArgNumBits = arg_num_bits(NumBits),
        (
            MaybeFieldName = yes(ctor_field_name(SymName, _FieldContext)),
            Name = unqualify_name(SymName),
            Id = field_id_name(CurArgNum, Name)
        ;
            MaybeFieldName = no,
            Id = field_id_ordinal(CurArgNum)
        ),
        SubWord = sub_word(NumBits, Id),
        !:SubWords = [SubWord | !.SubWords]
    ),
    record_subword_args_and_count_their_words(ArgRepns, CurArgNum + 1,
        !SubWords, !NumWords).

:- type sub_word_bin
    --->    sub_word_bin(
                rev_sub_words_in_bin    :: list(sub_word),
                remaining_bits_in_bin   :: int
            ).

:- pred insert_subword_into_first_fit_bin(int::in, sub_word::in,
    list(sub_word_bin)::in, list(sub_word_bin)::out) is det.

insert_subword_into_first_fit_bin(BinSize, SubWord, Bins0, Bins) :-
    SubWord = sub_word(SubWordNumBits, _Id),
    (
        Bins0 = [],
        ( if SubWordNumBits < BinSize then
            Bins = [sub_word_bin([SubWord], BinSize - SubWordNumBits)]
        else
            unexpected($pred, "SubWordNumBits >= BinSize")
        )
    ;
        Bins0 = [HeadBin0 | TailBins0],
        HeadBin0 = sub_word_bin(RevSubWords0, RemainingBitsInBin0),
        ( if SubWordNumBits =< RemainingBitsInBin0 then
            RevSubWords = [SubWord | RevSubWords0],
            RemainingBitsInBin = RemainingBitsInBin0 - SubWordNumBits,
            HeadBin = sub_word_bin(RevSubWords, RemainingBitsInBin),
            Bins = [HeadBin | TailBins0]
        else
            insert_subword_into_first_fit_bin(BinSize, SubWord,
                TailBins0, TailBins),
            Bins = [HeadBin0 | TailBins]
        )
    ).

:- pred describe_sub_word_bin(sub_word_bin::in, list(format_piece)::out)
    is det.

describe_sub_word_bin(SubWordBin, Pieces) :-
    SubWordBin = sub_word_bin(RevSubWords, _RemainingBits),
    list.reverse(RevSubWords, SubWords),
    list.map_foldl(describe_sub_word, SubWords, SubWordPieceLists,
        0, TotalNumBits),
    list.condense(SubWordPieceLists, SubWordPieces),
    Pieces = [blank_line,
        words("One word containing the following arguments:"), nl]
        ++ SubWordPieces ++
        [words("These total"), int_fixed(TotalNumBits),
        words((if TotalNumBits = 1 then "bit." else "bits.")), nl].

:- pred describe_sub_word(sub_word::in, list(format_piece)::out,
    int::in, int::out) is det.

describe_sub_word(SubWord, Pieces, !TotalNumBits) :-
    SubWord = sub_word(SubWordNumBits, Id),
    !:TotalNumBits = !.TotalNumBits + SubWordNumBits,
    NumBitsStr = string.format("#bits = %d", [i(SubWordNumBits)]),
    (
        Id = field_id_sectag,
        Pieces = [words("- the distinguishing tag"),
            words("at the start of the memory cell,"), fixed(NumBitsStr), nl]
    ;
        Id = field_id_ordinal(ArgNum),
        Pieces = [words("- the current"), nth_fixed(ArgNum),
            words("argument,"), fixed(NumBitsStr), nl]
    ;
        Id = field_id_name(_ArgNum, ArgName),
        Pieces = [words("- the argument named"), quote(ArgName), suffix(","),
            fixed(NumBitsStr), nl]
    ).

%---------------------------------------------------------------------------%
%
% Auxiliary functions and predicates.
%

:- pred deref_eqv_types(module_info::in, mer_type::in, mer_type::out) is det.

deref_eqv_types(ModuleInfo, Type0, Type) :-
    ( if type_to_type_defn_body(ModuleInfo, Type0, TypeBody0) then
        ( if TypeBody0 = hlds_eqv_type(Type1) then
            % XXX Should we require that Type1 have the same visibility
            % as Type0? If it doesn't, then we this predicate may yield
            % a different final type when compiling different modules,
            % which means we can make different decisions about data
            % representations when compiling different modules.
            % However, we currently test only whether the final type
            % is float, and equivalences to float are always exported.
            % XXX This still leaves the possibility of t1 == t2, t2 == float,
            % which is a problem if *only the second* equivalence is exported.
            % XXX Something will need to change when we start caring whether
            % the dereferenced type is int64 or uint64.
            deref_eqv_types(ModuleInfo, Type1, Type)
        else
            Type = Type0
        )
    else
        Type = Type0
    ).

%---------------------%

:- pred separate_out_constants(list(constructor)::in,
    list(constructor)::out, list(constructor)::out) is det.

separate_out_constants([], [], []).
separate_out_constants([Ctor | Ctors], Constants, Functors) :-
    separate_out_constants(Ctors, ConstantsTail, FunctorsTail),
    ( if ctor_is_constant(Ctor, _) then
        Constants = [Ctor | ConstantsTail],
        Functors = FunctorsTail
    else
        Constants = ConstantsTail,
        Functors = [Ctor | FunctorsTail]
    ).

:- pred separate_out_local_sectag_packable(decide_du_params::in,
    component_type_map::in, list(constructor)::in,
    assoc_list(int, constructor)::out, list(constructor)::out) is det.

separate_out_local_sectag_packable(_, _, [], [], []).
separate_out_local_sectag_packable(Params, ComponentTypeMap,
        [Ctor | Ctors], Packable, NonPackable) :-
    separate_out_local_sectag_packable(Params, ComponentTypeMap,
        Ctors, PackableTail, NonPackableTail),
    ( if
        Limit = Params ^ ddp_arg_pack_bits,
        ctor_has_all_packable_args_within_limit(Params, ComponentTypeMap, Ctor,
            Limit, NumBits)
    then
        Packable = [NumBits - Ctor | PackableTail],
        NonPackable = NonPackableTail
    else
        Packable = PackableTail,
        NonPackable = [Ctor | NonPackableTail]
    ).

:- pred ctor_has_all_packable_args_within_limit(decide_du_params::in,
    component_type_map::in, constructor::in, int::in, int::out) is semidet.

ctor_has_all_packable_args_within_limit(Params, ComponentTypeMap, Ctor,
        Limit, NumBits) :-
    Args = Ctor ^ cons_args,
    Ctor ^ cons_maybe_exist = no_exist_constraints,
    find_initial_args_packable_within_limit(Params, ComponentTypeMap, Limit,
        0, NumBits, Args, _PackableArgs, LeftOverArgs),
    LeftOverArgs = [].

:- pred find_initial_args_packable_within_limit(decide_du_params::in,
    component_type_map::in, int::in, int::in, int::out,
    list(constructor_arg)::in,
    list(constructor_arg)::out, list(constructor_arg)::out) is det.

find_initial_args_packable_within_limit(_, _, _, !NumBits, [], [], []).
find_initial_args_packable_within_limit(Params, ComponentTypeMap, Limit,
        !NumBits, [Arg | Args], PackableArgs, LeftOverArgs) :-
    Arg = ctor_arg(_ArgName, ArgType, _ArgContext),
    ( if
        may_pack_arg_type(Params, ComponentTypeMap, ArgType, Packable),
        (
            Packable = packable_n_bits(ArgNumArgBits, _FillKind),
            !:NumBits = !.NumBits + ArgNumArgBits
        ;
            Packable = packable_dummy
        ),
        !.NumBits =< Limit
    then
        find_initial_args_packable_within_limit(Params, ComponentTypeMap,
            Limit, !NumBits, Args, PackableArgsTail, LeftOverArgs),
        PackableArgs = [Arg | PackableArgsTail]
    else
        PackableArgs = [],
        LeftOverArgs = [Arg | Args]
    ).

%---------------------------------------------------------------------------%

:- pred compute_sectag_bits(int::in, sectag_bits::out) is det.

compute_sectag_bits(NumBits, SectagBits) :-
    NumBitsMask = (1 << NumBits) - 1,
    ( if
        uint8.from_int(NumBits, NumBitsUint8),
        uint.from_int(NumBitsMask, NumBitsMaskUint)
    then
        SectagBits = sectag_bits(NumBitsUint8, NumBitsMaskUint)
    else
        unexpected($pred, "NumBitsNeeded does not fit in 8 bits")
    ).

:- pred num_bits_needed_for_n_things(int::in, int::out) is det.

num_bits_needed_for_n_things(NumSharers, NumBits) :-
    num_bits_needed_for_n_things_loop(NumSharers, 0, NumBits).

:- pred num_bits_needed_for_n_things_loop(int::in, int::in, int::out) is det.

num_bits_needed_for_n_things_loop(N, NumBits0, NumBits) :-
    ( if N =< (1 << NumBits0) then
        NumBits = NumBits0
    else
        num_bits_needed_for_n_things_loop(N, NumBits0 + 1, NumBits)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_show_type_repns(module_info::in,
    assoc_list(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

maybe_show_type_repns(ModuleInfo, TypeCtorsTypeDefns, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, show_local_type_repns,
        ShowLocalTypeRepns),
    (
        ShowLocalTypeRepns = no
        % If we are not showing the representations of local types,
        % we definitely aren't showing the representations of *all* types.
    ;
        ShowLocalTypeRepns = yes,
        globals.lookup_bool_option(Globals, show_all_type_repns,
            ShowAllTypeRepns),
        (
            ShowAllTypeRepns = no,
            ShowWhichTypes = show_locally_defined_types
        ;
            ShowAllTypeRepns = yes,
            ShowWhichTypes = show_all_visible_types
        ),
        globals.lookup_bool_option(Globals, show_developer_type_repns,
            ShowDeveloperTypeRepns),
        (
            ShowDeveloperTypeRepns = no,
            ForDevelopers = not_for_developers
        ;
            ShowDeveloperTypeRepns = yes,
            ForDevelopers = for_developers
        ),

        module_info_get_name(ModuleInfo, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".type_repns")),
            ModuleName, TypeRepnFileName, !IO),
        io.open_output(TypeRepnFileName, TypeRepnFileResult, !IO),
        (
            TypeRepnFileResult = ok(TypeRepnStream),
            compute_maybe_primary_tag_bits(Globals, MaybePrimaryTags),
            list.foldl(
                show_decisions_if_du_type(TypeRepnStream, MaybePrimaryTags,
                    ShowWhichTypes, ForDevelopers),
                TypeCtorsTypeDefns, !IO),
            io.close_output(TypeRepnStream, !IO)
        ;
            TypeRepnFileResult = error(_)
        )
    ).

:- pred show_decisions_if_du_type(io.text_output_stream::in,
    maybe_primary_tags::in, show_which_types::in, maybe_for_developers::in,
    pair(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

show_decisions_if_du_type(Stream, MaybePrimaryTags, ShowWhichTypes,
        ForDevelopers, TypeCtorTypeDefn, !IO) :-
    TypeCtorTypeDefn = TypeCtor - TypeDefn,
    (
        ShowWhichTypes = show_all_visible_types,
        ShowType = yes
    ;
        ShowWhichTypes = show_locally_defined_types,
        get_type_defn_status(TypeDefn, TypeStatus),
        ShowType = type_status_defined_in_this_module(TypeStatus)
    ),
    (
        ShowType = no
    ;
        ShowType = yes,
        get_type_defn_body(TypeDefn, Body),
        (
            ( Body = hlds_foreign_type(_)
            ; Body = hlds_abstract_type(_)
            ; Body = hlds_eqv_type(_)
            ; Body = hlds_solver_type(_)
            )
        ;
            Body = hlds_du_type(BodyDu),
            BodyDu = type_body_du(_Ctors, MaybeSuperType, _MaybeCanonical,
                MaybeRepn, _MaybeForeign),
            (
                MaybeRepn = no,
                unexpected($pred, "MaybeRepn = no")
            ;
                MaybeRepn = yes(Repn)
            ),
            Repn = du_type_repn(CtorRepns, _CtorRepnMap, _MaybeCheaperTagTest,
                DuTypeKind, _MaybeDirectArgFunctors),
            TypeCtorStr = type_ctor_to_string(TypeCtor),
            io.format(Stream, "\ntype constructor %s\n",
                [s(TypeCtorStr)], !IO),
            (
                MaybeSuperType = subtype_of(SuperType),
                type_to_ctor_det(SuperType, SuperTypeCtor),
                SuperTypeCtorStr = type_ctor_to_string(SuperTypeCtor),
                io.format(Stream, "subtype of %s\n",
                    [s(SuperTypeCtorStr)], !IO)
            ;
                MaybeSuperType = not_a_subtype
            ),
            (
                DuTypeKind = du_type_kind_direct_dummy,
                io.write_string(Stream, "dummy type\n", !IO)
            ;
                DuTypeKind = du_type_kind_mercury_enum,
                io.format(Stream,
                    "mercury enumeration type with %d function symbols\n",
                    [i(list.length(CtorRepns))], !IO)
            ;
                DuTypeKind = du_type_kind_foreign_enum(_),
                io.format(Stream,
                    "foreign enumeration type with %d function symbols\n",
                    [i(list.length(CtorRepns))], !IO)
            ;
                DuTypeKind = du_type_kind_notag(_, _, _),
                io.write_string(Stream, "notag type\n", !IO)
            ;
                DuTypeKind = du_type_kind_general,
                io.write_string(Stream,
                    "general discriminated union type\n", !IO),
                list.foldl(
                    show_decisions_for_ctor(Stream, MaybePrimaryTags,
                        ForDevelopers, TypeCtorStr),
                    CtorRepns, !IO)
            )
        )
    ).

:- pred show_decisions_for_ctor(io.text_output_stream::in,
    maybe_primary_tags::in, maybe_for_developers::in, string::in,
    constructor_repn::in, io::di, io::uo) is det.

show_decisions_for_ctor(Stream, MaybePrimaryTags, ForDevelopers, TypeCtorStr,
        CtorRepn, !IO) :-
    CtorRepn = ctor_repn(_Ordinal, MaybeExist, CtorSymName, ConsTag,
        CtorArgRepns, NumArgs, _Context),
    CtorName = unqualify_name(CtorSymName),
    CtorStr = string.format("%s/%d", [s(CtorName), i(NumArgs)]),
    (
        ForDevelopers = not_for_developers,
        io.format(Stream, "  %s: ", [s(CtorStr)], !IO)
    ;
        ForDevelopers = for_developers,
        io.format(Stream, "  CTOR %s %s: ", [s(TypeCtorStr), s(CtorStr)], !IO)
    ),
    (
        ConsTag = shared_local_tag_no_args(Ptag, LocalSectag, _Mask),
        ArgsLocn = args_local,
        LocalSectag = local_sectag(Sectag, _, SectagBits),
        PtagDesc = show_ptag(MaybePrimaryTags, Ptag),
        SectagDesc = show_sectag_bits(MaybePrimaryTags, ArgsLocn,
            Sectag, SectagBits),
        io.format(Stream, "no arguments, %s, %s\n",
            [s(PtagDesc), s(SectagDesc)], !IO),
        ShowArgs = yes,
        NumRemoteSectagWords = 0
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        ArgsLocn = args_local,
        (
            LocalArgsTagInfo = local_args_only_functor,
            io.write_string(Stream, "local arguments, only_functor", !IO)
        ;
            LocalArgsTagInfo = local_args_not_only_functor(Ptag, LocalSectag),
            LocalSectag = local_sectag(Sectag, _, SectagBits),
            PtagDesc = show_ptag(MaybePrimaryTags, Ptag),
            SectagDesc = show_sectag_bits(MaybePrimaryTags, ArgsLocn,
                Sectag, SectagBits),
            io.format(Stream, "local arguments, %s, %s\n",
                [s(PtagDesc), s(SectagDesc)], !IO)
        ),
        ShowArgs = yes,
        NumRemoteSectagWords = 0
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        ArgsLocn = args_remote,
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            io.write_string(Stream, "remote arguments, only_functor\n", !IO),
            ShowArgs = yes,
            NumRemoteSectagWords = 0
        ;
            RemoteArgsTagInfo = remote_args_unshared(Ptag),
            PtagDesc = show_ptag(MaybePrimaryTags, Ptag),
            io.format(Stream, "remote arguments, %s, no secondary tag\n",
                [s(PtagDesc)], !IO),
            ShowArgs = yes,
            NumRemoteSectagWords = 0
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            PtagDesc = show_ptag(MaybePrimaryTags, Ptag),
            RemoteSectag = remote_sectag(Sectag, SectagSize),
            (
                SectagSize = rsectag_word,
                SectagDesc = string.format(
                    "remote secondary tag %d (in heap cell word 0, all bits)",
                    [i(uint.cast_to_int(Sectag))])
            ;
                SectagSize = rsectag_subword(SectagBits),
                SectagDesc = show_sectag_bits(MaybePrimaryTags, ArgsLocn,
                    Sectag, SectagBits)
            ),
            io.format(Stream, "remote arguments, %s, %s\n",
                [s(PtagDesc), s(SectagDesc)], !IO),
            ShowArgs = yes,
            NumRemoteSectagWords = 1
        ;
            RemoteArgsTagInfo = remote_args_ctor(Data),
            io.format(Stream,
                "constructor identifier %d, " ++
                "every argument has its own field\n",
                [i(uint.cast_to_int(Data))], !IO),
            ShowArgs = no,
            NumRemoteSectagWords = 0
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        PtagDesc = show_ptag(MaybePrimaryTags, Ptag),
        io.format(Stream,
            "direct arg, %s, rest of word is pointer to argument\n",
            [s(PtagDesc)], !IO),
        ShowArgs = no,
        ArgsLocn = args_remote,
        NumRemoteSectagWords = 0
    ;
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = dummy_tag
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = no_tag
        ; ConsTag = closure_tag(_, _, _)
        ),
        unexpected($pred, "unexpected kind of tag for general du type")
    ),
    ( if
        ForDevelopers = not_for_developers,
        MaybeExist = exist_constraints(ConsExistConstraints)
    then
        expect(unify(ArgsLocn, args_remote), $pred,
            "exist_constraints but ArgsLocn != args_remote"),
        ConsExistConstraints = cons_exist_constraints(
            _ExistQTVars, Constraints,
            UnconstrainedExistQTVars, _ConstrainedExistQTVars),
        list.length(UnconstrainedExistQTVars, NumTypeInfos),
        list.length(Constraints, NumTypeClassInfos),
        TIStart = NumRemoteSectagWords,
        ( if NumTypeInfos = 0 then
            true
        else if NumTypeInfos = 1 then
            io.format(Stream,
                "    %s: type_info in heap cell word %d\n",
                [s(CtorStr), i(TIStart)], !IO)
        else
            io.format(Stream,
                "    %s: type_infos in heap cell words %d to %d\n",
                [s(CtorStr), i(TIStart), i(TIStart + NumTypeInfos - 1)], !IO)
        ),
        TCIStart = TIStart + NumTypeInfos,
        ( if NumTypeClassInfos = 0 then
            true
        else if NumTypeClassInfos = 1 then
            io.format(Stream,
                "    %s: typeclass_info in heap cell word %d\n",
                [s(CtorStr), i(TCIStart)], !IO)
        else
            io.format(Stream,
                "    %s: typeclass_infos in heap cell words %d to %d\n",
                [s(CtorStr), i(TCIStart), i(TCIStart + NumTypeClassInfos - 1)],
                !IO)
        )
    else
        true
    ),
    (
        ShowArgs = no
    ;
        ShowArgs = yes,
        show_decisions_for_ctor_args(Stream, ForDevelopers, TypeCtorStr,
            CtorStr, ArgsLocn, 1, CtorArgRepns, !IO)
    ).

:- type args_locn
    --->    args_local
    ;       args_remote.

:- pred show_decisions_for_ctor_args(io.text_output_stream::in,
    maybe_for_developers::in, string::in, string::in, args_locn::in,
    int::in, list(constructor_arg_repn)::in, io::di, io::uo) is det.

show_decisions_for_ctor_args(_, _, _, _, _, _, [], !IO).
show_decisions_for_ctor_args(Stream, ForDevelopers, TypeCtorStr, CtorStr,
        ArgsLocn, ArgNum, [CtorArgRepn | CtorArgRepns], !IO) :-
    (
        ForDevelopers = not_for_developers,
        io.format(Stream, "    arg %d: ",
            [i(ArgNum)], !IO)
    ;
        ForDevelopers = for_developers,
        io.format(Stream, "    CTOR_ARG %s %s arg %d: ",
            [s(TypeCtorStr), s(CtorStr), i(ArgNum)], !IO)
    ),
    CtorArgRepn = ctor_arg_repn(_, _, ArgPosWidth, _),
    (
        ArgPosWidth = apw_full(_, CellOffset),
        expect(unify(ArgsLocn, args_remote), $pred, "apw_full not remote"),
        CellOffset = cell_offset(CellOffsetInt),
        io.format(Stream, "heap cell word %d\n", [i(CellOffsetInt)], !IO)
    ;
        ArgPosWidth = apw_double(_, CellOffset, DWKind),
        expect(unify(ArgsLocn, args_remote), $pred, "apw_double not remote"),
        CellOffset = cell_offset(CellOffsetInt),
        io.format(Stream, "heap cell words %d and %d",
            [i(CellOffsetInt), i(CellOffsetInt + 1)], !IO),
        (
            ForDevelopers = not_for_developers,
            io.nl(Stream, !IO)
        ;
            ForDevelopers = for_developers,
            (
                DWKind = dw_float,
                io.write_string(Stream, ", float\n", !IO)
            ;
                DWKind = dw_int64,
                io.write_string(Stream, ", int64\n", !IO)
            ;
                DWKind = dw_uint64,
                io.write_string(Stream, ", uint64\n", !IO)
            )
        )
    ;
        (
            ArgPosWidth = apw_partial_first(_, CellOffset, Shift, NumBits,
                _, Fill),
            PosKind = "first"
        ;
            ArgPosWidth = apw_partial_shifted(_, CellOffset, Shift, NumBits,
                _, Fill),
            PosKind = "shifted"
        ),
        Shift = arg_shift(ShiftInt),
        NumBits = arg_num_bits(NumBitsInt),
        MinBitPos = ShiftInt,
        MaxBitPos = ShiftInt + NumBitsInt - 1,
        io.format(Stream, "%s, bits %d to %d",
            [s(arg_word_desc(ArgsLocn, CellOffset)),
            i(MinBitPos), i(MaxBitPos)], !IO),
        (
            ForDevelopers = not_for_developers,
            io.nl(Stream, !IO)
        ;
            ForDevelopers = for_developers,
            io.format(Stream,
                " (partial_%s, shift %d, num_bits %d, fill %s)\n",
                [s(PosKind), i(ShiftInt), i(NumBitsInt),
                s(fill_kind_to_str(Fill))], !IO)
        )
    ;
        ArgPosWidth = apw_none_shifted(_, CellOffset),
        io.format(Stream, "%s, no bits\n",
            [s(arg_word_desc(ArgsLocn, CellOffset))], !IO)
    ;
        ArgPosWidth = apw_none_nowhere,
        io.write_string(Stream, "no bits\n", !IO)
    ),
    show_decisions_for_ctor_args(Stream, ForDevelopers, TypeCtorStr, CtorStr,
        ArgsLocn, ArgNum + 1, CtorArgRepns, !IO).

:- func show_ptag(maybe_primary_tags, ptag) = string.

show_ptag(MaybePrimaryTags, Ptag) = Desc :-
    Ptag = ptag(PtagUint8),
    (
        MaybePrimaryTags = no_primary_tags,
        expect(unify(PtagUint8, 0u8), $pred,
            "nonzero ptag in the absence of ptags"),
        Desc = "no primary tag"
    ;
        MaybePrimaryTags = max_primary_tag(_, NumPtagBits),
        Desc = string.format("primary tag %d (in bits 0 to %d)",
            [i(uint8.cast_to_int(PtagUint8)), i(NumPtagBits - 1)])
    ).

:- func show_sectag_bits(maybe_primary_tags, args_locn, uint, sectag_bits)
    = string.

show_sectag_bits(MaybePrimaryTags, ArgsLocn, Sectag, SectagBits) = Desc :-
    SectagBits = sectag_bits(NumSectagBitsUint8, _),
    NumSectagBits = uint8.cast_to_int(NumSectagBitsUint8),
    ( if NumSectagBits = 0 then
        Desc = "no secondary tag"
    else
        SectagInt = uint.cast_to_int(Sectag),
        (
            ArgsLocn = args_local,
            (
                MaybePrimaryTags = no_primary_tags,
                unexpected($pred, "local sectag in the absence of ptags")
            ;
                MaybePrimaryTags = max_primary_tag(_, NumPtagBits)
            ),
            Desc = string.format(
                "local secondary tag %d (in bits %d to %d)",
                [i(SectagInt), i(NumPtagBits),
                i(NumPtagBits + NumSectagBits - 1)])
        ;
            ArgsLocn = args_remote,
            Desc = string.format(
                "remote secondary tag %d (in heap cell word 0, bits 0 to %d)",
                [i(SectagInt), i(NumSectagBits - 1)])
        )
    ).

:- func arg_word_desc(args_locn, cell_offset) = string.

arg_word_desc(ArgsLocn, CellOffset) = Desc :-
    CellOffset = cell_offset(CellOffsetInt),
    (
        ArgsLocn = args_local,
        expect(unify(CellOffsetInt, -2), $pred,
            "unexpected offset for local arg"),
        Desc = "local word"
    ;
        ArgsLocn = args_remote,
        expect(CellOffsetInt >= 0, $pred, "negative offset for remote arg"),
        Desc = string.format("heap cell word %d", [i(CellOffsetInt)])
    ).

:- func fill_kind_to_str(fill_kind) = string.

fill_kind_to_str(fill_enum) = "enum".
fill_kind_to_str(fill_int8) = "int8".
fill_kind_to_str(fill_int16) = "int16".
fill_kind_to_str(fill_int32) = "int32".
fill_kind_to_str(fill_uint8) = "uint8".
fill_kind_to_str(fill_uint16) = "uint16".
fill_kind_to_str(fill_uint32) = "uint32".
fill_kind_to_str(fill_char21) = "char21".

%---------------------------------------------------------------------------%

:- pred output_sized_packable_functor(io.text_output_stream::in,
    maybe({decide_du_params, component_type_map})::in,
    pair(int, constructor)::in, io::di, io::uo) is det.

output_sized_packable_functor(Stream, PrintArgSizes, SizedPackable, !IO) :-
    SizedPackable = NumBits - Constructor,
    Constructor = ctor(_Ordinal, _MaybeExist, Name, Args, NumArgs, _Context),
    io.format(Stream, "%2d: %s/%d",
        [i(NumBits), s(unqualify_name(Name)), i(NumArgs)], !IO),
    (
        PrintArgSizes = no,
        io.nl(Stream, !IO)
    ;
        PrintArgSizes = yes({Params, ComponentTypeMap}),
        io.write_string(Stream, "(", !IO),
        output_sized_packable_functor_args(Stream, Params, ComponentTypeMap,
            "", Args, !IO),
        io.write_string(Stream, ")\n", !IO)
    ).

:- pred output_sized_packable_functor_args(io.text_output_stream::in,
    decide_du_params::in, component_type_map::in, string::in,
    list(constructor_arg)::in, io::di, io::uo) is det.

output_sized_packable_functor_args(_, _, _, _, [], !IO).
output_sized_packable_functor_args(Stream, Params, ComponentTypeMap, Prefix,
        [Arg | Args], !IO) :-
    Arg = ctor_arg(_ArgName, ArgType, _ArgContext),
    ( if may_pack_arg_type(Params, ComponentTypeMap, ArgType, Packable) then
        (
            Packable = packable_n_bits(ArgNumArgBits, _FillKind)
        ;
            Packable = packable_dummy,
            ArgNumArgBits = 0
        ),
        io.format(Stream, "%s%d", [s(Prefix), i(ArgNumArgBits)], !IO)
    else
        unexpected($pred, "nonpackable")
    ),
    output_sized_packable_functor_args(Stream, Params, ComponentTypeMap, ", ",
        Args, !IO).

%---------------------------------------------------------------------------%

:- type maybe_double_word_floats
    --->    no_double_word_floats
    ;       use_double_word_floats.

    % This setting applies to uint64s as well.
:- type maybe_double_word_int64s
    --->    no_double_word_int64s
    ;       use_double_word_int64s.

:- type maybe_primary_tags
    --->    no_primary_tags
    ;       max_primary_tag(ptag, int).
            % The maximum ptag, and the number of bits it occupies.

:- type maybe_unboxed_no_tag_types
    --->    no_unboxed_no_tag_types
    ;       use_unboxed_no_tag_types.

:- type maybe_direct_args
    --->    direct_args_disabled
    ;       direct_args_enabled.

:- type maybe_inform_about_packing
    --->    do_not_inform_about_packing
    ;       inform_about_packing.

:- type show_which_types
    --->    show_locally_defined_types
    ;       show_all_visible_types.

:- type maybe_for_developers
    --->    not_for_developers
    ;       for_developers.

:- type maybe_show_type_repns
    --->    do_not_show_type_repns
    ;       show_type_repns(show_which_types, maybe_for_developers).

:- type decide_du_params
    --->    decide_du_params(
                % For constructing debug (and possibly other) output streams.
                ddp_globals                     :: globals,
                ddp_module_name                 :: module_name,

                ddp_arg_pack_bits               :: int,
                ddp_maybe_primary_tags          :: maybe_primary_tags,

                ddp_target                      :: compilation_target,

                ddp_double_word_floats          :: maybe_double_word_floats,
                ddp_double_word_int64s          :: maybe_double_word_int64s,
                ddp_unboxed_no_tag_types        :: maybe_unboxed_no_tag_types,

                ddp_inform_suboptimal_pack      :: maybe_inform_about_packing,

                % Only for bootstrapping.
                ddp_allow_double_word_ints      :: bool,
                ddp_allow_packing_ints          :: bool,
                ddp_allow_packing_chars         :: bool,
                ddp_allow_packing_dummies       :: bool,
                ddp_allow_packing_local_sectags :: bool,
                ddp_allow_packing_remote_sectags :: bool,
                ddp_allow_packing_mini_types    :: bool,

                % We use the direct_arg_map for two purposes:
                % - to optimize data representations, and
                % - to generate error messages for incorrect uses of
                %   "where direct_arg is" clauses.
                % We need the direct_arg_map for the first purpose
                % only when the direct_arg optimization is enabled,
                % but we use it for the second purpose even when it is
                % disabled.
                % XXX When we remove "where direct_arg is" clauses
                % from the language, the second purpose will go away.
                ddp_maybe_direct_args           :: maybe_direct_args,
                ddp_direct_arg_map              :: direct_arg_map
            ).

:- pred setup_decide_du_params(globals::in, module_name::in,
    direct_arg_map::in, decide_du_params::out) is det.

setup_decide_du_params(Globals, ModuleName, DirectArgMap, Params) :-
    % Compute Target.
    globals.get_target(Globals, Target),

    % Compute DoubleWordFloats and DoubleWordInt64s.
    globals.lookup_bool_option(Globals, allow_double_word_fields,
        AllowDoubleWords),
    (
        AllowDoubleWords = yes,
        globals.get_word_size(Globals, WordSize),
        (
            WordSize = word_size_32,
            globals.lookup_bool_option(Globals, single_prec_float,
                SinglePrecFloat),
            (
                SinglePrecFloat = no,
                DoubleWordFloats = use_double_word_floats
            ;
                SinglePrecFloat = yes,
                DoubleWordFloats = no_double_word_floats
            ),
            DoubleWordInt64s = use_double_word_int64s
        ;
            WordSize = word_size_64,
            DoubleWordInt64s = no_double_word_int64s,
            DoubleWordFloats = no_double_word_floats
        )
    ;
        AllowDoubleWords = no,
        DoubleWordFloats = no_double_word_floats,
        DoubleWordInt64s = no_double_word_int64s
    ),

    % Compute UnboxedNoTagTypes.
    globals.lookup_bool_option(Globals, unboxed_no_tag_types,
        UnboxedNoTagTypesBool),
    (
        UnboxedNoTagTypesBool = no,
        UnboxedNoTagTypes = no_unboxed_no_tag_types
    ;
        UnboxedNoTagTypesBool = yes,
        UnboxedNoTagTypes = use_unboxed_no_tag_types
    ),

    % Compute MaybePrimaryTags.
    compute_maybe_primary_tag_bits(Globals, MaybePrimaryTags),

    % Compute ArgPackBits.
    globals.lookup_int_option(Globals, arg_pack_bits, ArgPackBits),

    % Compute AllowDoubleWordInts, AllowPackingInts and AllowPackingDummies.
    globals.lookup_bool_option(Globals, allow_double_word_ints,
        AllowDoubleWordInts),
    globals.lookup_bool_option(Globals, allow_packing_ints,
        AllowPackingInts),
    globals.lookup_bool_option(Globals, allow_packing_chars,
        AllowPackingChars),
    globals.lookup_bool_option(Globals, allow_packing_dummies,
        AllowPackingDummies),
    globals.lookup_bool_option(Globals, allow_packing_local_sectags,
        AllowPackingLocalSegtags),
    globals.lookup_bool_option(Globals, allow_packing_remote_sectags,
        AllowPackingRemoteSegtags),
    globals.lookup_bool_option(Globals, allow_packing_mini_types,
        AllowPackingMiniTypes),

    % Compute MaybeDirectArgs.
    (
        Target = target_c,
        globals.lookup_bool_option(Globals, allow_direct_args,
            AllowDirectArgs),
        % We cannot use direct arg functors in term size grades.
        globals.lookup_bool_option(Globals, record_term_sizes_as_words,
            TermSizeWords),
        globals.lookup_bool_option(Globals, record_term_sizes_as_cells,
            TermSizeCells),
        ( if
            AllowDirectArgs = yes,
            TermSizeWords = no,
            TermSizeCells = no
        then
            MaybeDirectArgs = direct_args_enabled
        else
            MaybeDirectArgs = direct_args_disabled
        )
    ;
        ( Target = target_csharp
        ; Target = target_java
        ),
        % Direct arg functors have not (yet) been implemented on these targets.
        MaybeDirectArgs = direct_args_disabled,

        expect(unify(AllowDoubleWords, no), $pred,
            "AllowDoubleWords != no"),
        expect(unify(AllowPackingInts, no), $pred,
            "AllowPackingInts != no"),
        expect(unify(AllowPackingDummies, no), $pred,
            "AllowPackingDummies != no")
    ),

    % Compute MaybeInformPacking.
    globals.lookup_bool_option(Globals, inform_suboptimal_packing,
        InformPacking),
    (
        InformPacking = no,
        MaybeInformPacking = do_not_inform_about_packing
    ;
        InformPacking = yes,
        MaybeInformPacking = inform_about_packing
    ),

    Params = decide_du_params(Globals, ModuleName,
        ArgPackBits, MaybePrimaryTags, Target,
        DoubleWordFloats, DoubleWordInt64s,
        UnboxedNoTagTypes, MaybeInformPacking,
        AllowDoubleWordInts, AllowPackingInts, AllowPackingChars,
        AllowPackingDummies, AllowPackingLocalSegtags,
        AllowPackingRemoteSegtags, AllowPackingMiniTypes,
        MaybeDirectArgs, DirectArgMap).

:- pred compute_maybe_primary_tag_bits(globals::in, maybe_primary_tags::out)
    is det.

compute_maybe_primary_tag_bits(Globals, MaybePrimaryTags) :-
    globals.get_target(Globals, Target),
    (
        Target = target_c,
        globals.lookup_int_option(Globals, num_ptag_bits, NumPtagBits),
        % We require the use of two or three primary tags when targeting C.
        ( if NumPtagBits = 2 then
            MaybePrimaryTags = max_primary_tag(ptag(3u8), NumPtagBits)
        else if NumPtagBits = 3 then
            MaybePrimaryTags = max_primary_tag(ptag(7u8), NumPtagBits)
        else
            % handle_options.m should have generated an error if num_ptag_bits
            % is not 2 or 3, which should have meant that its caller in
            % mercury_compile_main.m stops execution before
            % du_type_layout.m is invoked.
            unexpected($pred, "target_c but NumPtagBits not 2 or 3")
        )
    ;
        ( Target = target_java
        ; Target = target_csharp
        ),
        MaybePrimaryTags = no_primary_tags
    ).

:- pred maybe_exist_constraints_num_extra_words(
    maybe_cons_exist_constraints::in, int::out) is det.

maybe_exist_constraints_num_extra_words(MaybeExistConstraints, ExtraWords) :-
    (
        MaybeExistConstraints = no_exist_constraints,
        ExtraWords = 0
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(_ExistQTVars, Constraints,
            UnconstrainedExistQTVars, _ConstrainedExistQTVars),
        list.length(UnconstrainedExistQTVars, NumTypeInfos),
        list.length(Constraints, NumTypeClassInfos),
        ExtraWords = NumTypeInfos + NumTypeClassInfos
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.du_type_layout.
%---------------------------------------------------------------------------%
