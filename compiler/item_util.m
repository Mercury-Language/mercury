%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: item_util.m.
%
% This module contains utility predicates for dealing with items.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.item_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

    % This is effectively a subtype of int_file_kind, which is defined in
    % prog_item.m.
:- type short_int_file_kind
    --->    sifk_int2   % the qualified short interface, for the .int2 file
    ;       sifk_int3.  % the unqualified short interface, for the .int3 file

%-----------------------------------------------------------------------------%

    % Could this item use items from imported modules.
    %
:- func item_needs_imports(item) = bool.

:- func item_needs_foreign_imports(item) = list(foreign_language).

:- func pragma_needs_foreign_imports(pragma_type) = list(foreign_language).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.

:- import_module require.

%-----------------------------------------------------------------------------%

item_needs_imports(Item) = NeedsImports :-
    % XXX This function is FAR too crude;
    % it should find out *what* imports we need.
    (
        Item = item_type_defn(ItemTypeDefn),
        ( if ItemTypeDefn ^ td_ctor_defn = parse_tree_abstract_type(_) then
            NeedsImports = no
        else
            NeedsImports = yes
        )
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_foreign_enum(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_pragma(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        NeedsImports = yes
    ;
        Item = item_type_repn(_),
        % These should not be generated yet.
        unexpected($pred, "item_type_repn")
    ).

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
        Item = item_foreign_enum(FEInfo),
        FEInfo = item_foreign_enum_info(Lang, _, _, _, _),
        Langs = [Lang]
    ;
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, _, _),
        Langs = pragma_needs_foreign_imports(Pragma)
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_foreign_export_enum(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_promise(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        Langs = []
    ;
        Item = item_type_repn(_),
        % These should not be generated yet.
        unexpected($pred, "item_type_repn")
    ).

pragma_needs_foreign_imports(Pragma) = Langs :-
    (
        (
            Pragma = pragma_foreign_decl(FDInfo),
            FDInfo = pragma_info_foreign_decl(Lang, _, _)
        ;
            Pragma = pragma_foreign_code(FCInfo),
            FCInfo = pragma_info_foreign_code(Lang, _)
        ;
            Pragma = pragma_foreign_proc_export(FPEInfo),
            FPEInfo = pragma_info_foreign_proc_export(Lang, _, _)
        ),
        Langs = [Lang]
    ;
        Pragma = pragma_foreign_proc(FPInfo),
        FPInfo = pragma_info_foreign_proc(Attrs, _, _, _, _, _, _),
        Langs = [get_foreign_language(Attrs)]
    ;
        ( Pragma = pragma_external_proc(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_consider_used(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_obsolete_pred(_)
        ; Pragma = pragma_obsolete_proc(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_require_tail_recursion(_)
        ; Pragma = pragma_oisu(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_fact_table(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_check_termination(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_require_feature_set(_)
        ),
        Langs = []
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.item_util.
%-----------------------------------------------------------------------------%
