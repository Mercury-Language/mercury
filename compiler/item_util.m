%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: item_util.m.
%
% This module contains utility predicates for dealing with items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.item_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.

:- import_module list.
:- import_module map.
:- import_module set.

%---------------------------------------------------------------------------%
%
% Operations on foreign_import_module (fim) items.
%

:- func fim_item_to_spec(item_fim) = fim_spec.
:- func fim_spec_to_item(fim_spec) = item_fim.
:- func fim_module_lang_to_spec(module_name, foreign_language) = fim_spec.
:- func fim_module_lang_to_item(module_name, foreign_language) = item_fim.

:- pred add_implicit_fim_for_module(module_name::in, foreign_language::in,
    map(fim_spec, prog_context)::in, map(fim_spec, prog_context)::out) is det.

    % For what languages could this item need the import of foreign modules.
    %
:- func item_needs_foreign_imports(item) = list(foreign_language).

:- pred acc_needed_self_fim_langs_for_type_defn(item_type_defn_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.
:- pred acc_needed_self_fim_langs_for_foreign_proc(item_foreign_proc_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.
:- pred acc_needed_self_fim_langs_for_foreign_enum(item_foreign_enum_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.
:- pred acc_needed_self_fim_langs_for_impl_pragma(item_impl_pragma_info::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

%---------------------------------------------------------------------------%
%
% Describing items for error messages.
%

    % Return a description of one item, ...
    %
:- func item_desc_pieces(item) = list(format_piece).

    % ... and its plural version.
    %
:- func items_desc_pieces(item) = list(format_piece).

:- func decl_pragma_desc_pieces(item_decl_pragma_info) = list(format_piece).
:- func decl_marker_desc_pieces(item_decl_marker_info) = list(format_piece).
:- func impl_pragma_desc_pieces(item_impl_pragma_info) = list(format_piece).
:- func impl_marker_desc_pieces(item_impl_marker_info) = list(format_piece).
:- func gen_pragma_desc_pieces(item_generated_pragma_info)
    = list(format_piece).

%---------------------------------------------------------------------------%
%
% Projection operations.
%

:- func parse_tree_module_src_project_name(parse_tree_module_src)
    = module_name.

:- func item_include_module_name(item_include) = module_name.

:- func get_avail_context(item_avail) = prog_context.
:- func get_import_context(avail_import_info) = prog_context.
:- func get_use_context(avail_use_info) = prog_context.

:- func get_avail_module_name(item_avail) = module_name.
:- func get_import_module_name(avail_import_info) = module_name.
:- func get_use_module_name(avail_use_info) = module_name.

%---------------------------------------------------------------------------%
%
% Given a checked map for types, insts or modes, return the interface items
% and the implementation items they represent. (This will be a consistent
% subset of the set of the relevant kind of items in the module's code.)
% For types, return the set of foreign_enum items as well; these are all
% in the implementation section.
%

:- pred type_ctor_checked_map_get_src_defns(type_ctor_checked_map::in,
    list(item_type_defn_info)::out, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::out) is det.

:- pred inst_ctor_checked_map_get_src_defns(inst_ctor_checked_map::in,
    list(item_inst_defn_info)::out, list(item_inst_defn_info)::out) is det.

:- pred mode_ctor_checked_map_get_src_defns(mode_ctor_checked_map::in,
    list(item_mode_defn_info)::out, list(item_mode_defn_info)::out) is det.

%---------------------------------------------------------------------------%
%
% Wrapping up pieces of information with a dummy context
% and a dummy sequence number.
%

:- func wrap_include(module_name) = item_include.
:- func wrap_import_avail(module_name) = item_avail.
:- func wrap_use_avail(module_name) = item_avail.
:- func wrap_import(module_name) = avail_import_info.
:- func wrap_use(module_name) = avail_use_info.

:- func wrap_avail_import(avail_import_info) = item_avail.
:- func wrap_avail_use(avail_use_info) = item_avail.

:- func wrap_type_defn_item(item_type_defn_info) = item.
:- func wrap_inst_defn_item(item_inst_defn_info) = item.
:- func wrap_mode_defn_item(item_mode_defn_info) = item.
:- func wrap_typeclass_item(item_typeclass_info) = item.
:- func wrap_instance_item(item_instance_info) = item.
:- func wrap_pred_decl_item(item_pred_decl_info) = item.
:- func wrap_mode_decl_item(item_mode_decl_info) = item.
:- func wrap_foreign_enum_item(item_foreign_enum_info) = item.
:- func wrap_foreign_export_enum_item(item_foreign_export_enum_info) = item.
:- func wrap_clause(item_clause_info) = item.
:- func wrap_decl_pragma_item(item_decl_pragma_info) = item.
:- func wrap_impl_pragma_item(item_impl_pragma_info) = item.
:- func wrap_generated_pragma_item(item_generated_pragma_info) = item.
:- func wrap_promise_item(item_promise_info) = item.
:- func wrap_initialise_item(item_initialise_info) = item.
:- func wrap_finalise_item(item_finalise_info) = item.
:- func wrap_mutable_item(item_mutable_info) = item.
:- func wrap_type_repn_item(item_type_repn_info) = item.

%---------------------------------------------------------------------------%
%
% Converting specific forms of type definitions to the generic form.
%

:- func wrap_abstract_type_defn(item_type_defn_info_abstract)
    = item_type_defn_info.
:- func wrap_solver_type_defn(item_type_defn_info_solver)
    = item_type_defn_info.
:- func wrap_eqv_type_defn(item_type_defn_info_eqv)
    = item_type_defn_info.
:- func wrap_du_type_defn(item_type_defn_info_du)
    = item_type_defn_info.
:- func wrap_sub_type_defn(item_type_defn_info_sub)
    = item_type_defn_info.
:- func wrap_foreign_type_defn(item_type_defn_info_foreign)
    = item_type_defn_info.

:- func wrap_abstract_inst_defn(item_inst_defn_info_abstract)
    = item_inst_defn_info.
:- func wrap_eqv_inst_defn(item_inst_defn_info_eqv)
    = item_inst_defn_info.

:- func wrap_abstract_mode_defn(item_mode_defn_info_abstract)
    = item_mode_defn_info.
:- func wrap_eqv_mode_defn(item_mode_defn_info_eqv)
    = item_mode_defn_info.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.

:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

fim_item_to_spec(FIM) = FIMSpec :-
    FIM = item_fim(Lang, ModuleName, _, _),
    FIMSpec = fim_spec(Lang, ModuleName).

fim_spec_to_item(FIMSpec) = FIM :-
    FIMSpec = fim_spec(Lang, ModuleName),
    FIM = item_fim(Lang, ModuleName, dummy_context, item_no_seq_num).

fim_module_lang_to_spec(ModuleName, Lang) = fim_spec(Lang, ModuleName).

fim_module_lang_to_item(ModuleName, Lang) =
    item_fim(Lang, ModuleName, dummy_context, item_no_seq_num).

add_implicit_fim_for_module(ModuleName, Lang, !Map) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    ( if map.search(!.Map, FIMSpec, _) then
        true
    else
        map.det_insert(FIMSpec, dummy_context, !Map)
    ).

%---------------------------------------------------------------------------%

item_needs_foreign_imports(Item) = Langs :-
    (
        Item = item_mutable(_ItemMutable),
        % We can use all foreign languages.
        Langs = all_foreign_languages
    ;
        Item = item_type_defn(ItemTypeDefn),
        ( if
            ItemTypeDefn ^ td_ctor_defn =
                parse_tree_foreign_type(DetailsForeign),
            DetailsForeign = type_details_foreign(ForeignType, _, _)
        then
            Langs = [foreign_type_language(ForeignType)]
        else
            Langs = []
        )
    ;
        Item = item_foreign_proc(FPInfo),
        FPInfo = item_foreign_proc_info(Attrs, _, _, _, _, _, _, _, _),
        Langs = [get_foreign_language(Attrs)]
    ;
        Item = item_foreign_enum(FEInfo),
        FEInfo = item_foreign_enum_info(Lang, _, _, _, _),
        Langs = [Lang]
    ;
        Item = item_impl_pragma(ItemImplPragma),
        Langs = impl_pragma_needs_foreign_imports(ItemImplPragma)
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_decl_pragma(_)
        ; Item = item_decl_marker(_)
        ; Item = item_impl_marker(_)
        ; Item = item_generated_pragma(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        Langs = []
    ;
        Item = item_type_repn(_),
        % These should not occur in source files.
        unexpected($pred, "item_type_repn")
    ).

acc_needed_self_fim_langs_for_type_defn(ItemTypeDefn, !Langs) :-
    ( if
        ItemTypeDefn ^ td_ctor_defn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, _, _)
    then
        set.insert(foreign_type_language(ForeignType), !Langs)
    else
        true
    ).

acc_needed_self_fim_langs_for_foreign_proc(FPInfo, !Langs) :-
    FPInfo = item_foreign_proc_info(Attrs, _, _, _, _, _, _, _, _),
    set.insert(get_foreign_language(Attrs), !Langs).

acc_needed_self_fim_langs_for_foreign_enum(FEInfo, !Langs) :-
    FEInfo = item_foreign_enum_info(Lang, _, _, _, _),
    set.insert(Lang, !Langs).

acc_needed_self_fim_langs_for_impl_pragma(ItemImplPragma, !Langs) :-
    set.insert_list(impl_pragma_needs_foreign_imports(ItemImplPragma), !Langs).

:- func impl_pragma_needs_foreign_imports(item_impl_pragma_info)
    = list(foreign_language).

impl_pragma_needs_foreign_imports(ImplPragma) = Langs :-
    (
        (
            ImplPragma = impl_pragma_foreign_decl(FDInfo),
            FDInfo = impl_pragma_foreign_decl_info(Lang, _, _, _, _)
        ;
            ImplPragma = impl_pragma_foreign_code(FCInfo),
            FCInfo = impl_pragma_foreign_code_info(Lang, _, _, _)
        ;
            ImplPragma = impl_pragma_fproc_export(FPEInfo),
            FPEInfo = impl_pragma_fproc_export_info(_, Lang, _, _, _, _, _)
        ),
        Langs = [Lang]
    ;
        ( ImplPragma = impl_pragma_external_proc(_)
        ; ImplPragma = impl_pragma_tabled(_)
        ; ImplPragma = impl_pragma_fact_table(_)
        ; ImplPragma = impl_pragma_req_tail_rec(_)
        ; ImplPragma = impl_pragma_req_feature_set(_)
        ),
        Langs = []
    ).

%---------------------------------------------------------------------------%

item_desc_pieces(Item) = Pieces :-
    % If you change this code, see whether items_desc_to_pieces needs
    % updating as well.
    (
        Item = item_clause(_),
        Pieces = [words("clause")]
    ;
        Item = item_type_defn(_),
        Pieces = [words("type definition")]
    ;
        Item = item_inst_defn(_),
        Pieces = [words("inst definition")]
    ;
        Item = item_mode_defn(_),
        Pieces = [words("mode definition")]
    ;
        Item = item_pred_decl(ItemPredDecl),
        PorF = ItemPredDecl ^ pf_p_or_f,
        (
            PorF = pf_predicate,
            Pieces = [words("predicate declaration")]
        ;
            PorF = pf_function,
            Pieces = [words("function declaration")]
        )
    ;
        Item = item_mode_decl(_),
        Pieces = [words("mode declaration")]
    ;
        Item = item_foreign_proc(_),
        Pieces = [pragma_decl("foreign_proc"), words("declaration")]
    ;
        Item = item_foreign_enum(_),
        Pieces = [pragma_decl("foreign_enum"), words("declaration")]
    ;
        Item = item_foreign_export_enum(_),
        Pieces = [pragma_decl("foreign_export_enum"), words("declaration")]
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        Pieces = decl_pragma_desc_pieces(ItemDeclPragma)
    ;
        Item = item_decl_marker(ItemDeclMarker),
        Pieces = decl_marker_desc_pieces(ItemDeclMarker)
    ;
        Item = item_impl_pragma(ItemImplPragma),
        Pieces = impl_pragma_desc_pieces(ItemImplPragma)
    ;
        Item = item_impl_marker(ItemImplMarker),
        Pieces = impl_marker_desc_pieces(ItemImplMarker)
    ;
        Item = item_generated_pragma(ItemGenPragma),
        Pieces = gen_pragma_desc_pieces(ItemGenPragma)
    ;
        Item = item_promise(ItemPromise),
        PromiseType = ItemPromise ^ prom_type,
        (
            PromiseType = promise_type_exclusive,
            Pieces = [words("exclusivity promise")]
        ;
            PromiseType = promise_type_exhaustive,
            Pieces = [words("exhaustivity promise")]
        ;
            PromiseType = promise_type_exclusive_exhaustive,
            Pieces = [words("exclusivity and exhaustivity promise")]
        ;
            PromiseType = promise_type_true,
            Pieces = [words("promise")]
        )
    ;
        Item = item_typeclass(_),
        Pieces = [words("typeclass declaration")]
    ;
        Item = item_instance(_),
        Pieces = [words("instance declaration")]
    ;
        Item = item_initialise(_),
        Pieces = [decl("initialise"), words("declaration")]
    ;
        Item = item_finalise(_),
        Pieces = [decl("finalise"), words("declaration")]
    ;
        Item = item_mutable(_),
        Pieces = [decl("mutable"), words("declaration")]
    ;
        Item = item_type_repn(_),
        Pieces = [decl("type_repn"), words("declaration")]
    ).

items_desc_pieces(Item) = Pieces :-
    % At the moment, all the Pieces0 that item_desc_pieces can generate
    % can be made plural by adding a single final "s".
    Pieces0 = item_desc_pieces(Item),
    Pieces = Pieces0 ++ [suffix("s")].

decl_pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = decl_pragma_obsolete_pred(_),
        Pieces = [pragma_decl("obsolete"), words("declaration")]
    ;
        Pragma = decl_pragma_obsolete_proc(_),
        Pieces = [pragma_decl("obsolete_proc"), words("declaration")]
    ;
        Pragma = decl_pragma_format_call(_),
        Pieces = [pragma_decl("format_call"), words("declaration")]
    ;
        Pragma = decl_pragma_type_spec_constr(_),
        Pieces = [pragma_decl("type_spec_constrained_preds"),
            words("declaration")]
    ;
        Pragma = decl_pragma_type_spec(_),
        Pieces = [pragma_decl("type_spec"), words("declaration")]
    ;
        Pragma = decl_pragma_oisu(_),
        Pieces = [pragma_decl("oisu"), words("declaration")]
    ;
        Pragma = decl_pragma_termination(_),
        Pieces = [pragma_decl("termination_info"), words("declaration")]
    ;
        Pragma = decl_pragma_termination2(_),
        Pieces = [pragma_decl("termination2_info"), words("declaration")]
    ;
        Pragma = decl_pragma_struct_sharing(_),
        Pieces = [pragma_decl("structure_sharing"), words("declaration")]
    ;
        Pragma = decl_pragma_struct_reuse(_),
        Pieces = [pragma_decl("structure_reuse"), words("declaration")]
    ).

decl_marker_desc_pieces(Marker) = Pieces :-
    Marker = item_decl_marker_info(MarkerKind, _, _, _),
    (
        MarkerKind = dpmk_terminates,
        Pieces = [pragma_decl("terminates"), words("declaration")]
    ;
        MarkerKind = dpmk_does_not_terminate,
        Pieces = [pragma_decl("does_not_terminate"), words("declaration")]
    ;
        MarkerKind = dpmk_check_termination,
        Pieces = [pragma_decl("check_termination"), words("declaration")]
    ).

impl_pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = impl_pragma_foreign_code(_),
        Pieces = [pragma_decl("foreign_code"), words("declaration")]
    ;
        Pragma = impl_pragma_foreign_decl(_),
        Pieces = [pragma_decl("foreign_decl"), words("declaration")]
    ;
        Pragma = impl_pragma_fproc_export(_),
        Pieces = [pragma_decl("foreign_export"), words("declaration")]
    ;
        Pragma = impl_pragma_external_proc(External),
        External = impl_pragma_external_proc_info(PFNameArity, _, _, _),
        PFNameArity = pred_pf_name_arity(PorF, _, _),
        (
            PorF = pf_predicate,
            Pieces = [pragma_decl("external_pred"), words("declaration")]
        ;
            PorF = pf_function,
            Pieces = [pragma_decl("external_func"), words("declaration")]
        )
    ;
        Pragma = impl_pragma_req_tail_rec(_),
        Pieces = [pragma_decl("require_tail_recursion"), words("declaration")]
    ;
        Pragma = impl_pragma_fact_table(_),
        Pieces = [pragma_decl("fact_table"), words("declaration")]
    ;
        Pragma = impl_pragma_tabled(Tabled),
        Tabled = impl_pragma_tabled_info(TabledMethod, _, _, _, _),
        (
            TabledMethod = tabled_memo(_),
            Pieces = [pragma_decl("memo"), words("declaration")]
        ;
            TabledMethod = tabled_loop_check,
            Pieces = [pragma_decl("loop_check"), words("declaration")]
        ;
            TabledMethod = tabled_minimal(_),
            Pieces = [pragma_decl("minimal_model"), words("declaration")]
        ;
            TabledMethod = tabled_io(_, _),
            unexpected($pred, "eval_table_io")
        )
    ;
        Pragma = impl_pragma_req_feature_set(_),
        Pieces = [pragma_decl("require_feature_set"), words("declaration")]
    ).

impl_marker_desc_pieces(Marker) = Pieces :-
    Marker = item_impl_marker_info(MarkerKind, _, _, _),
    (
        MarkerKind = ipmk_inline,
        Pieces = [pragma_decl("inline"), words("declaration")]
    ;
        MarkerKind = ipmk_no_inline,
        Pieces = [pragma_decl("no_inline"), words("declaration")]
    ;
        MarkerKind = ipmk_consider_used,
        Pieces = [pragma_decl("consider_used"), words("declaration")]
    ;
        MarkerKind = ipmk_mode_check_clauses,
        Pieces = [pragma_decl("mode_check_clauses"), words("declaration")]
    ;
        MarkerKind = ipmk_no_detism_warning,
        Pieces = [pragma_decl("no_determinism_warning"), words("declaration")]
    ;
        MarkerKind = ipmk_promise_pure,
        Pieces = [pragma_decl("promise_pure"), words("declaration")]
    ;
        MarkerKind = ipmk_promise_semipure,
        Pieces = [pragma_decl("promise_semipure"), words("declaration")]
    ;
        MarkerKind = ipmk_promise_eqv_clauses,
        Pieces = [pragma_decl("promise_equivalent_clauses"),
            words("declaration")]
    ).

gen_pragma_desc_pieces(Pragma) = Pieces :-
    (
        Pragma = gen_pragma_unused_args(_),
        Pieces = [pragma_decl("unused_args"), words("declaration")]
    ;
        Pragma = gen_pragma_exceptions(_),
        Pieces = [pragma_decl("exceptions"), words("declaration")]
    ;
        Pragma = gen_pragma_trailing(_),
        Pieces = [pragma_decl("trailing_info"), words("declaration")]
    ;
        Pragma = gen_pragma_mm_tabling(_),
        Pieces = [pragma_decl("mm_tabling_info"), words("declaration")]
    ).

%---------------------------------------------------------------------------%

parse_tree_module_src_project_name(ParseTreeModuleSrc) =
    ParseTreeModuleSrc ^ ptms_module_name.

item_include_module_name(Incl) = ModuleName :-
    Incl = item_include(ModuleName, _Context, _SeqNum).

get_avail_context(avail_import(avail_import_info(_, Context, _))) = Context.
get_avail_context(avail_use(avail_use_info(_, Context, _))) = Context.

get_import_context(avail_import_info(_, Context, _)) = Context.

get_use_context(avail_use_info(_, Context, _)) = Context.

get_avail_module_name(ItemAvail) = ModuleName :-
    (
        ItemAvail = avail_import(AvailImportInfo),
        AvailImportInfo = avail_import_info(ModuleName, _, _)
    ;
        ItemAvail = avail_use(AvailUseInfo),
        AvailUseInfo = avail_use_info(ModuleName, _, _)
    ).

get_import_module_name(AvailImportInfo) = ModuleName :-
    AvailImportInfo = avail_import_info(ModuleName, _, _).

get_use_module_name(AvailUseInfo) = ModuleName :-
    AvailUseInfo = avail_use_info(ModuleName, _, _).

%---------------------------------------------------------------------------%

type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, ImpForeignEnums) :-
    map.values(TypeCtorCheckedMap, TypeCtorCheckedDefns),
    list.map3(type_ctor_checked_defn_get_src_defns, TypeCtorCheckedDefns,
        IntTypeDefnLists, ImpTypeDefnLists, ImpForeignEnumLists),
    list.condense(IntTypeDefnLists, IntTypeDefns),
    list.condense(ImpTypeDefnLists, ImpTypeDefns),
    list.condense(ImpForeignEnumLists, ImpForeignEnums).

inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, ImpInstDefns) :-
    map.values(InstCtorCheckedMap, InstCtorCheckedDefns),
    list.map2(inst_ctor_checked_defn_get_src_defns, InstCtorCheckedDefns,
        IntInstDefnLists, ImpInstDefnLists),
    list.condense(IntInstDefnLists, IntInstDefns),
    list.condense(ImpInstDefnLists, ImpInstDefns).

mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, ImpModeDefns) :-
    map.values(ModeCtorCheckedMap, ModeCtorCheckedDefns),
    list.map2(mode_ctor_checked_defn_get_src_defns, ModeCtorCheckedDefns,
        IntModeDefnLists, ImpModeDefnLists),
    list.condense(IntModeDefnLists, IntModeDefns),
    list.condense(ImpModeDefnLists, ImpModeDefns).

%---------------------%

:- pred type_ctor_checked_defn_get_src_defns(type_ctor_checked_defn::in,
    list(item_type_defn_info)::out, list(item_type_defn_info)::out,
    list(item_foreign_enum_info)::out) is det.

type_ctor_checked_defn_get_src_defns(CheckedDefn, IntDefns, ImpDefns,
        ImpForeignEnums) :-
    (
        CheckedDefn = checked_defn_solver(_, SrcDefnsSolver),
        SrcDefnsSolver = src_defns_solver(MaybeIntDefn, MaybeImpDefn),
        IntDefns = maybe_to_list(MaybeIntDefn),
        ImpDefns = maybe_to_list(MaybeImpDefn),
        ImpForeignEnums = []
    ;
        CheckedDefn = checked_defn_std(_, SrcDefnsStd),
        SrcDefnsStd = src_defns_std(IntDefns, ImpDefns, ImpForeignEnums)
    ).

:- pred inst_ctor_checked_defn_get_src_defns(inst_ctor_checked_defn::in,
    list(item_inst_defn_info)::out, list(item_inst_defn_info)::out) is det.

inst_ctor_checked_defn_get_src_defns(CheckedDefn, IntDefns, ImpDefns) :-
    CheckedDefn = checked_defn_inst(_, SrcDefns),
    SrcDefns = src_defns_inst(MaybeIntDefn, MaybeImpDefn),
    IntDefns = maybe_to_list(MaybeIntDefn),
    ImpDefns = maybe_to_list(MaybeImpDefn).

:- pred mode_ctor_checked_defn_get_src_defns(mode_ctor_checked_defn::in,
    list(item_mode_defn_info)::out, list(item_mode_defn_info)::out) is det.

mode_ctor_checked_defn_get_src_defns(CheckedDefn, IntDefns, ImpDefns) :-
    CheckedDefn = checked_defn_mode(_, SrcDefns),
    SrcDefns = src_defns_mode(MaybeIntDefn, MaybeImpDefn),
    IntDefns = maybe_to_list(MaybeIntDefn),
    ImpDefns = maybe_to_list(MaybeImpDefn).

    % XXX Should we move this to library/maybe.m?
:- func maybe_to_list(maybe(T)) = list(T).

maybe_to_list(no) = [].
maybe_to_list(yes(X)) = [X].

%---------------------------------------------------------------------------%

wrap_include(ModuleName) = Include :-
    Include = item_include(ModuleName, dummy_context, item_no_seq_num).

wrap_import_avail(ModuleName) = Avail :-
    ImportInfo = avail_import_info(ModuleName, dummy_context, item_no_seq_num),
    Avail = avail_import(ImportInfo).

wrap_use_avail(ModuleName) = Avail :-
    UseInfo = avail_use_info(ModuleName, dummy_context, item_no_seq_num),
    Avail = avail_use(UseInfo).

wrap_import(ModuleName) = ImportInfo :-
    ImportInfo = avail_import_info(ModuleName, dummy_context, item_no_seq_num).

wrap_use(ModuleName) = UseInfo :-
    UseInfo = avail_use_info(ModuleName, dummy_context, item_no_seq_num).

wrap_avail_import(AvailImportInfo) = avail_import(AvailImportInfo).
wrap_avail_use(AvailUseInfo) = avail_use(AvailUseInfo).

wrap_type_defn_item(X) = item_type_defn(X).
wrap_inst_defn_item(X) = item_inst_defn(X).
wrap_mode_defn_item(X) = item_mode_defn(X).
wrap_typeclass_item(X) = item_typeclass(X).
wrap_instance_item(X) = item_instance(X).
wrap_pred_decl_item(X) = item_pred_decl(X).
wrap_mode_decl_item(X) = item_mode_decl(X).
wrap_foreign_enum_item(X) = item_foreign_enum(X).
wrap_foreign_export_enum_item(X) = item_foreign_export_enum(X).
wrap_clause(X) = item_clause(X).
wrap_decl_pragma_item(X) = item_decl_pragma(X).
wrap_impl_pragma_item(X) = item_impl_pragma(X).
wrap_generated_pragma_item(X) = item_generated_pragma(X).
wrap_promise_item(X) = item_promise(X).
wrap_initialise_item(X) = item_initialise(X).
wrap_finalise_item(X) = item_finalise(X).
wrap_mutable_item(X) = item_mutable(X).
wrap_type_repn_item(X) = item_type_repn(X).

%---------------------------------------------------------------------------%

wrap_abstract_type_defn(AbstractDefnInfo) = TypeDefnInfo :-
    AbstractDefn = AbstractDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = AbstractDefnInfo ^ td_ctor_defn
        := parse_tree_abstract_type(AbstractDefn).

wrap_solver_type_defn(SolverDefnInfo) = TypeDefnInfo :-
    SolverDefn = SolverDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = SolverDefnInfo ^ td_ctor_defn
        := parse_tree_solver_type(SolverDefn).

wrap_eqv_type_defn(EqvDefnInfo) = TypeDefnInfo :-
    EqvDefn = EqvDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = EqvDefnInfo ^ td_ctor_defn
        := parse_tree_eqv_type(EqvDefn).

wrap_du_type_defn(DuDefnInfo) = TypeDefnInfo :-
    DuDefn = DuDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = DuDefnInfo ^ td_ctor_defn
        := parse_tree_du_type(DuDefn).

wrap_sub_type_defn(SubDefnInfo) = TypeDefnInfo :-
    SubDefn = SubDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = SubDefnInfo ^ td_ctor_defn
        := parse_tree_sub_type(SubDefn).

wrap_foreign_type_defn(ForeignDefnInfo) = TypeDefnInfo :-
    ForeignDefn = ForeignDefnInfo ^ td_ctor_defn,
    TypeDefnInfo = ForeignDefnInfo ^ td_ctor_defn
        := parse_tree_foreign_type(ForeignDefn).

%---------------------%

wrap_abstract_inst_defn(AbstractDefnInfo) = InstDefnInfo :-
    InstDefnInfo = AbstractDefnInfo ^ id_inst_defn := abstract_inst_defn.

wrap_eqv_inst_defn(EqvDefnInfo) = InstDefnInfo :-
    EqvDefn = EqvDefnInfo ^ id_inst_defn,
    InstDefnInfo = EqvDefnInfo ^ id_inst_defn
        := nonabstract_inst_defn(EqvDefn).

%---------------------%

wrap_abstract_mode_defn(AbstractDefnInfo) = ModeDefnInfo :-
    ModeDefnInfo = AbstractDefnInfo ^ md_mode_defn := abstract_mode_defn.

wrap_eqv_mode_defn(EqvDefnInfo) = ModeDefnInfo :-
    EqvDefn = EqvDefnInfo ^ md_mode_defn,
    ModeDefnInfo = EqvDefnInfo ^ md_mode_defn
        := nonabstract_mode_defn(EqvDefn).

%---------------------------------------------------------------------------%
:- end_module parse_tree.item_util.
%---------------------------------------------------------------------------%
