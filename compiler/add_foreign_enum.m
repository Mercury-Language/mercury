%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module has two related but separate tasks.
%
% The task of the first half of this module is to check foreign_enum pragmas
% and gather from them the information that du_type_layout.m will use
% to help decide the representations of the types named in those pragmas.
%
% The task of the second half of this module is to check foreign_export_enum
% pragmas, and to record the information from the correct ones in the
% module_info for the code generator to use.
%
% The "third half" of the module consists of the predicates that generate
% the error messages for the two previous halves.
%
%---------------------------------------------------------------------------%

:- module hlds.add_foreign_enum.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type type_ctor_foreign_enums
    --->    type_ctor_foreign_enums(
                tcfe_lang_contexts      :: map(foreign_language, prog_context),
                tcfe_tag_values         :: maybe({cons_id_to_tag_map,
                                                foreign_language})
            ).

:- type type_ctor_to_foreign_enums_map
    == map(type_ctor, type_ctor_foreign_enums).

    % Check the given foreign_enum pragma for correctness.
    % If it is correct, update the given type_ctor_to_foreign_enums_map
    % with its information. If not, add the applicable error message(s)
    % to the list.
    %
:- pred add_pragma_foreign_enum(module_info::in, item_foreign_enum_info::in,
    type_ctor_to_foreign_enums_map::in, type_ctor_to_foreign_enums_map::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Check the given foreign_export_enum pragma for correctness.
    % If it is correct, record its information in the exported_enums field
    % of the module_info so that the code generator can include the
    % exported information in the target language file it emits
    % (when compiling for the applicable backend). If it is not correct,
    % add the applicable error message(s) to the list.
    %
:- pred add_pragma_foreign_export_enum(item_foreign_export_enum_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module hlds.make_hlds_error.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module cord.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%

add_pragma_foreign_enum(ModuleInfo, ItemForeignExportEnum,
        !TypeCtorForeignEnumMap, Specs0, Specs) :-
    ItemForeignExportEnum = item_foreign_enum_info(FEInfo, ItemMercuryStatus,
        Context, SeqNum),
    % XXX we shouldn't have to construct ItemPragmaInfo to do the
    % wrongly-in-interface test.
    ItemPragmaInfo = item_pragma_info(pragma_foreign_enum(FEInfo),
        item_origin_user, Context, SeqNum),
    report_if_pragma_is_wrongly_in_interface(ItemMercuryStatus, ItemPragmaInfo,
        Specs0, Specs1),
    item_mercury_status_to_type_status(ItemMercuryStatus, PragmaStatus),
    FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, ForeignTagValues),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    ContextPieces = [words("In"), pragma_decl("foreign_enum"),
        words("declaration for type"),
        qual_sym_name_and_arity(sym_name_arity(TypeSymName, TypeArity)),
        suffix(":"), nl],

    some [!ErrorSeveritiesPieces, !Specs] (
        !:ErrorSeveritiesPieces = [],
        !:Specs = [],
        report_if_builtin_type(TypeSymName, TypeArity, !ErrorSeveritiesPieces),

        module_info_get_type_table(ModuleInfo, TypeTable),
        ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
            get_type_defn_status(TypeDefn, TypeStatus),
            % Either both the type and the pragma are defined in this module,
            % or they are both imported. Any other combination is illegal.
            IsTypeLocal = type_status_defined_in_this_module(TypeStatus),
            ( if
                (
                    IsTypeLocal = yes,
                    ( PragmaStatus = type_status(status_local)
                    ; PragmaStatus =
                        type_status(status_exported_to_submodules)
                    )
                ;
                    IsTypeLocal = no,
                    type_status_is_imported(PragmaStatus) = yes
                )
            then
                true
            else if
                PragmaStatus = type_status(status_exported)
            then
                add_foreign_enum_pragma_in_interface_error(Context,
                    TypeSymName, TypeArity, !Specs)
            else
                NotThisModuleErrorPieces = [words("error: "),
                    qual_sym_name_and_arity(
                        sym_name_arity(TypeSymName, TypeArity)),
                    words("is not defined in this module.")],
                !:ErrorSeveritiesPieces =
                    [{severity_error, NotThisModuleErrorPieces} |
                        !.ErrorSeveritiesPieces]
            ),

            get_type_defn_body(TypeDefn, TypeBody),
            (
                (
                    TypeBody = hlds_eqv_type(_),
                    NonDu = "an equivalence type"
                ;
                    TypeBody = hlds_abstract_type(_),
                    NonDu = "an abstract type"
                ;
                    TypeBody = hlds_solver_type(_),
                    NonDu = "a solver type"
                ;
                    TypeBody = hlds_foreign_type(_),
                    NonDu = "a foreign type"
                ),
                report_not_enum_type(TypeSymName, TypeArity,
                    not_enum_non_du(NonDu), !ErrorSeveritiesPieces),
                list.foldl(
                    add_error_severity_pieces(Context, ContextPieces),
                    !.ErrorSeveritiesPieces, !Specs)
            ;
                TypeBody = hlds_du_type(Ctors, _MaybeUserEq, MaybeRepn,
                    _IsForeignType),
                expect(unify(MaybeRepn, no), $pred, "MaybeRepn != no"),

                list.map(constructor_to_cons_id(TypeCtor),
                    Ctors, ConsIds),
                find_enum_nonenum_cons_ids(ConsIds,
                    [], EnumSymNames, [], NonEnumConsIds),
                (
                    NonEnumConsIds = []
                ;
                    NonEnumConsIds = [_ | _],
                    report_not_enum_type(TypeSymName, TypeArity,
                        not_enum_du(NonEnumConsIds), !ErrorSeveritiesPieces)
                ),

                % Work out what language's foreign_enum pragma we should be
                % looking at for the current compilation target language.
                module_info_get_globals(ModuleInfo, Globals),
                globals.get_target(Globals, TargetLanguage),
                LangForForeignEnums =
                    target_lang_to_foreign_enum_lang(TargetLanguage),

                list.foldl(gather_ctor_name, Ctors,
                    set_tree234.init, CtorNameSet),
                build_foreign_enum_tag_map(Context, ContextPieces,
                    TypeSymName, CtorNameSet, ForeignTagValues,
                    MaybeForeignTagMap, !Specs),
                (
                    MaybeForeignTagMap = no,
                    MaybeTagValues = no
                ;
                    MaybeForeignTagMap = yes(ForeignTagMap),
                    list.foldl2(
                        make_foreign_tag(TypeCtor, Lang, ForeignTagMap),
                        EnumSymNames, map.init, TagValues0, [], UnmappedCtors),
                    MaybeTagValues = yes(TagValues0),
                    (
                        UnmappedCtors = []
                    ;
                        UnmappedCtors = [_ | _],
                        add_foreign_enum_unmapped_ctors_error(Context,
                            ContextPieces, UnmappedCtors, !Specs)
                    )
                ),

                list.foldl(
                    add_error_severity_pieces(Context, ContextPieces),
                    !.ErrorSeveritiesPieces, !Specs),

                ( if
                    MaybeTagValues = yes(TagValues),
                    Lang = LangForForeignEnums,
                    !.Specs = []
                then
                    MaybeTagValuesToSet = yes({TagValues, Lang})
                else
                    MaybeTagValuesToSet = no
                ),

                ( if map.search(!.TypeCtorForeignEnumMap, TypeCtor, TCFE0) then
                    TCFE0 = type_ctor_foreign_enums(LangContextMap0,
                        _OldMaybeTagValues),
                    ( if map.search(LangContextMap0, Lang, OldContext) then
                        maybe_add_duplicate_foreign_enum_error(
                            TypeSymName, TypeArity, Lang, PragmaStatus,
                            OldContext, Context, !Specs),
                        TCFE1 = TCFE0
                    else
                        map.det_insert(Lang, Context,
                            LangContextMap0, LangContextMap),
                        TCFE1 = TCFE0 ^ tcfe_lang_contexts := LangContextMap
                    ),
                    (
                        MaybeTagValuesToSet = no,
                        TCFE = TCFE1
                    ;
                        MaybeTagValuesToSet = yes(_),
                        TCFE = TCFE1 ^ tcfe_tag_values := MaybeTagValuesToSet
                    ),
                    map.det_update(TypeCtor, TCFE, !TypeCtorForeignEnumMap)
                else
                    LangContextMap = map.singleton(Lang, Context),
                    TCFE = type_ctor_foreign_enums(LangContextMap,
                        MaybeTagValuesToSet),
                    map.det_insert(TypeCtor, TCFE, !TypeCtorForeignEnumMap)
                )
            )
        else
            % This else-branch corresponds to an undefined type. We do not
            % issue an error message for it here, since module qualification
            % will have already done so.
            true
        ),
        Specs = !.Specs ++ Specs1
    ).

:- pred constructor_to_cons_id(type_ctor::in, constructor::in, cons_id::out)
    is det.

constructor_to_cons_id(TypeCtor, Ctor, ConsId) :-
    Ctor = ctor(_MaybeExistConstraints, SymName, _Args, Arity, _Ctxt),
    ConsId = cons(SymName, Arity, TypeCtor).

:- pred gather_ctor_name(constructor::in,
    set_tree234(string)::in, set_tree234(string)::out) is det.

gather_ctor_name(Constructor, !CtorNameSet) :-
    CtorSymName = Constructor ^ cons_name,
    Name = unqualify_name(CtorSymName),
    set_tree234.insert(Name, !CtorNameSet).

:- pred build_foreign_enum_tag_map(prog_context::in,
    list(format_component)::in, sym_name::in,
    set_tree234(string)::in, assoc_list(sym_name, string)::in,
    maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_foreign_enum_tag_map(Context, ContextPieces, TypeSymName,
        CtorNameSet, ForeignTagValues0, MaybeForeignTagMap, !Specs) :-
    (
        TypeSymName = qualified(TypeModuleName, _)
    ;
        TypeSymName = unqualified(_),
        unexpected($pred,
            "unqualified type name while processing foreign enum tags")
    ),
    list.map_foldl2(
        fixup_foreign_tag_val_qualification(CtorNameSet, TypeModuleName),
        ForeignTagValues0, ForeignTagValues,
        [], BadQualCtors, [], UnknownCtors),
    (
        UnknownCtors = []
    ;
        UnknownCtors = [_ | _],
        add_unknown_ctors_error(Context, ContextPieces, UnknownCtors, !Specs)
    ),
    (
        BadQualCtors = []
    ;
        BadQualCtors = [_ | _],
        add_bad_qual_ctors_error(Context, ContextPieces, BadQualCtors, !Specs)
    ),
    ( if bimap.from_assoc_list(ForeignTagValues, ForeignTagBiMap) then
        ForeignTagMap = ForeignTagBiMap ^ forward_map,
        MaybeForeignTagMap = yes(ForeignTagMap)
    else
        add_foreign_enum_bijection_error(Context, ContextPieces, !Specs),
        MaybeForeignTagMap = no
    ).

    % The constructor names we get from the parse tree may be unqualified
    % but the ones we match against in the HLDS are not. Module qualify them.
    %
    % XXX module_qual.m should really be doing this rather than add_pragma.m.
    %
:- pred fixup_foreign_tag_val_qualification(set_tree234(string)::in,
    module_name::in,
    pair(sym_name, string)::in, pair(sym_name, string)::out,
    list(sym_name)::in, list(sym_name)::out,
    list(sym_name)::in, list(sym_name)::out) is det.

fixup_foreign_tag_val_qualification(CtorNameSet, TypeModuleName, !NamesAndTags,
        !BadQualCtors, !UnknownCtors) :-
    !.NamesAndTags = CtorSymName0 - ForeignTag,
    (
        CtorSymName0 = unqualified(Name),
        CtorSymName = qualified(TypeModuleName, Name)
    ;
        CtorSymName0 = qualified(CtorModuleName, Name),
        ( if partial_sym_name_matches_full(CtorModuleName, TypeModuleName) then
            CtorSymName = qualified(TypeModuleName, Name)
        else
            !:BadQualCtors = [CtorSymName0 | !.BadQualCtors],
            CtorSymName = CtorSymName0
        )
    ),
    ( if set_tree234.contains(CtorNameSet, Name) then
        true
    else
        !:UnknownCtors = [CtorSymName0 | !.UnknownCtors]
    ),
    !:NamesAndTags = CtorSymName - ForeignTag.

    % For a given target language work out which language's foreign_enum
    % pragma we should be looking at.
    %
:- func target_lang_to_foreign_enum_lang(compilation_target)
    = foreign_language.

target_lang_to_foreign_enum_lang(target_c) = lang_c.
target_lang_to_foreign_enum_lang(target_csharp) = lang_csharp.
target_lang_to_foreign_enum_lang(target_java) = lang_java.
target_lang_to_foreign_enum_lang(target_erlang) = lang_erlang.

:- pred find_enum_nonenum_cons_ids(list(cons_id)::in,
    list(sym_name)::in, list(sym_name)::out,
    list(cons_id)::in, list(cons_id)::out) is det.

find_enum_nonenum_cons_ids([], !EnumSymNames, !NonEnumConsIds).
find_enum_nonenum_cons_ids([ConsId | ConsIds],
        !EnumSymNames, !NonEnumConsIds) :-
    ( if ConsId = cons(ConsSymName, 0, _) then
        !:EnumSymNames = [ConsSymName | !.EnumSymNames]
    else
        !:NonEnumConsIds = [ConsId | !.NonEnumConsIds]
    ),
    find_enum_nonenum_cons_ids(ConsIds, !EnumSymNames, !NonEnumConsIds).

:- pred make_foreign_tag(type_ctor::in, foreign_language::in,
    map(sym_name, string)::in, sym_name::in,
    cons_id_to_tag_map::in, cons_id_to_tag_map::out,
    list(sym_name)::in, list(sym_name)::out) is det.

make_foreign_tag(TypeCtor, ForeignLanguage, ForeignTagMap, ConsSymName,
        !ConsIdToTagMap, !UnmappedCtors) :-
    ( if map.search(ForeignTagMap, ConsSymName, ForeignTagValue) then
        ForeignTag = foreign_tag(ForeignLanguage, ForeignTagValue),
        ConsId = cons(ConsSymName, 0, TypeCtor),
        map.set(ConsId, ForeignTag, !ConsIdToTagMap)
    else
        !:UnmappedCtors = [ConsSymName | !.UnmappedCtors]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

add_pragma_foreign_export_enum(ItemForeignExportEnum, !ModuleInfo,
        Specs0, Specs) :-
    ItemForeignExportEnum = item_foreign_export_enum_info(FEEInfo,
        _ItemMercuryStatus, Context, _SeqNum),
    FEEInfo = pragma_info_foreign_export_enum(Lang, TypeCtor,
        Attributes, Overrides),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    ContextPieces = [words("In"), pragma_decl("foreign_export_enum"),
        words("declaration for type"),
        qual_sym_name_and_arity(sym_name_arity(TypeSymName, TypeArity)),
        suffix(":"), nl],
    some [!ErrorSeveritiesPieces, !Specs] (
        !:ErrorSeveritiesPieces = [],
        !:Specs = [],

        report_if_builtin_type(TypeSymName, TypeArity, !ErrorSeveritiesPieces),

        module_info_get_type_table(!.ModuleInfo, TypeTable),
        ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
            % If the type is in the type table (i.e. it is neither
            % a builtin type such as int nor a reference to an undefined type),
            % then TypeSymName should have been qualified by now.
            (
                TypeSymName = qualified(TypeModuleName, _TypeName)
            ;
                TypeSymName = unqualified(_),
                unexpected($pred,
                    "unqualified type name for foreign_export_enum")
            ),
            get_type_defn_body(TypeDefn, TypeBody),
            (
                (
                    TypeBody = hlds_eqv_type(_),
                    NonDu = "an equivalence type"
                ;
                    TypeBody = hlds_abstract_type(_),
                    NonDu = "an abstract type"
                ;
                    TypeBody = hlds_solver_type(_),
                    NonDu = "a solver type"
                ;
                    TypeBody = hlds_foreign_type(_),
                    NonDu = "a foreign type"
                ),
                report_not_enum_type(TypeSymName, TypeArity,
                    not_enum_non_du(NonDu), !ErrorSeveritiesPieces)
            ;
                % XXX How should we handle IsForeignType here?
                TypeBody = hlds_du_type(Ctors, _MaybeUserEq, MaybeRepn,
                    _IsForeignType),
                (
                    MaybeRepn = no,
                    unexpected($pred, "MaybeRepn = no")
                ;
                    MaybeRepn = yes(Repn)
                ),
                Repn = du_type_repn(_TagValues, CtorRepns, _ConsCtorMap,
                    _CheaperTagTest, DuTypeKind, _MaybeDirectArgCtors),
                list.map(constructor_to_cons_id(TypeCtor),
                    Ctors, ConsIds),
                find_enum_nonenum_cons_ids(ConsIds,
                    [], _EnumSymNames, [], NonEnumConsIds),
                (
                    ( DuTypeKind = du_type_kind_general
                    ; DuTypeKind = du_type_kind_notag(_, _, _)
                    ),
                    report_not_enum_type(TypeSymName, TypeArity,
                        not_enum_du(NonEnumConsIds), !ErrorSeveritiesPieces)
                ;
                    ( DuTypeKind = du_type_kind_mercury_enum
                    ; DuTypeKind = du_type_kind_foreign_enum(_)
                    ; DuTypeKind = du_type_kind_direct_dummy
                    )
                ),

                Attributes =
                    export_enum_attributes(MaybePrefix, MakeUpperCase),
                (
                    MaybePrefix = yes(Prefix)
                ;
                    MaybePrefix = no,
                    Prefix = ""
                ),
                build_export_enum_overrides_map(TypeModuleName, Context,
                    ContextPieces, CtorRepns, Overrides, OverrideBimap,
                    !Specs),
                OverrideMap = bimap.forward_map(OverrideBimap),
                build_export_enum_name_map(ContextPieces, Context, Lang,
                    Prefix, MakeUpperCase, OverrideMap, CtorRepns,
                    NameMap, !Specs),
                ( if
                    !.ErrorSeveritiesPieces = [],
                    !.Specs = []
                then
                    ExportedEnum = exported_enum_info(TypeCtor, CtorRepns,
                        Lang, NameMap, Context),
                    module_info_get_exported_enums(!.ModuleInfo,
                        ExportedEnums0),
                    ExportedEnums = [ExportedEnum | ExportedEnums0],
                    module_info_set_exported_enums(ExportedEnums,
                        !ModuleInfo)
                else
                    true
                )
            )
        else
            % This case corresponds to an undefined type. We do not issue
            % an error message for it here, since module qualification
            % will have already done so.
            true
        ),
        (
            !.ErrorSeveritiesPieces = [_ | _],
            list.foldl(add_error_severity_pieces(Context, ContextPieces),
                !.ErrorSeveritiesPieces, !Specs)
        ;
            !.ErrorSeveritiesPieces = []
        ),
        Specs = !.Specs ++ Specs0
    ).

:- pred build_export_enum_overrides_map(module_name::in, prog_context::in,
    list(format_component)::in, list(constructor_repn)::in,
    assoc_list(sym_name, string)::in, bimap(string, string)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_overrides_map(TypeModuleName, Context, ContextPieces,
        CtorRepns, Overrides, OverrideMap, !Specs) :-
    build_valid_ctor_names_set(CtorRepns, set_tree234.init, ValidCtorNames),

    SeenCtorNames0 = set_tree234.init,
    SeenForeignNames0 = set_tree234.init,
    BadQualCtorSymNamesCord0 = cord.init,
    InvalidCtorSymNamesCord0 = cord.init,
    RepeatedCtorNamesCord0 = cord.init,
    RepeatedForeignNamesCord0 = cord.init,
    build_export_enum_overrides_map_loop(TypeModuleName, ValidCtorNames,
        Overrides, bimap.init, OverrideMap,
        SeenCtorNames0, SeenForeignNames0,
        BadQualCtorSymNamesCord0, BadQualCtorSymNamesCord,
        InvalidCtorSymNamesCord0, InvalidCtorSymNamesCord,
        RepeatedCtorNamesCord0, RepeatedCtorNamesCord,
        RepeatedForeignNamesCord0, RepeatedForeignNamesCord),

    ( if cord.is_empty(BadQualCtorSymNamesCord) then
        true
    else
        add_bad_qual_ctors_error(Context, ContextPieces,
            cord.to_list(BadQualCtorSymNamesCord), !Specs)
    ),
    ( if cord.is_empty(InvalidCtorSymNamesCord) then
        true
    else
        add_unknown_ctors_error(Context, ContextPieces,
            cord.to_list(InvalidCtorSymNamesCord), !Specs)
    ),
    RepeatedCtorNames = cord.to_list(RepeatedCtorNamesCord),
    RepeatedForeignNames = cord.to_list(RepeatedForeignNamesCord),
    ( if
        RepeatedCtorNames = [],
        RepeatedForeignNames = []
    then
        true
    else
        MainPieces = ContextPieces ++
            [invis_order_default_start(3), words("error: "),
            words("the user-specified mapping between Mercury and"),
            words("foreign names does not form a bijection."), nl],
        (
            RepeatedCtorNames = [],
            CtorNamePieces = []
        ;
            RepeatedCtorNames = [_ | _],
            CtorNamePieces =
                [words("The following Mercury constructor"),
                words(choose_number(RepeatedCtorNames,
                    "name is", "names are")),
                words("repeated:"), nl_indent_delta(2)] ++
                list_to_quoted_pieces(RepeatedCtorNames) ++
                [suffix("."), nl_indent_delta(-2)]
        ),
        (
            RepeatedForeignNames = [],
            ForeignNamePieces = []
        ;
            RepeatedForeignNames = [_ | _],
            ForeignNamePieces =
                [words("The following foreign"),
                words(choose_number(RepeatedForeignNames,
                    "name is", "names are")),
                words("repeated:"), nl_indent_delta(2)] ++
                list_to_quoted_pieces(RepeatedForeignNames) ++
                [suffix("."), nl_indent_delta(-2)]
        ),
        Pieces = MainPieces ++ CtorNamePieces ++ ForeignNamePieces,
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_valid_ctor_names_set(list(constructor_repn)::in,
    set_tree234(string)::in, set_tree234(string)::out) is det.

build_valid_ctor_names_set([], !ValidNamesSet).
build_valid_ctor_names_set([CtorRepn | CtorRepns], !ValidNamesSet) :-
    CtorSymName = CtorRepn ^ cr_name,
    CtorName = unqualify_name(CtorSymName),
    set_tree234.insert(CtorName, !ValidNamesSet),
    build_valid_ctor_names_set(CtorRepns, !ValidNamesSet).

:- pred build_export_enum_overrides_map_loop(module_name::in,
    set_tree234(string)::in,
    assoc_list(sym_name, string)::in,
    bimap(string, string)::in, bimap(string, string)::out,
    set_tree234(string)::in, set_tree234(string)::in,
    cord(sym_name)::in, cord(sym_name)::out,
    cord(sym_name)::in, cord(sym_name)::out,
    cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out) is det.

build_export_enum_overrides_map_loop(_, _, [], !OverrideMap,
        _SeenCtorNames, _SeenForeignNames,
        !BadQualCtorSymNames, !InvalidCtorSymNames,
        !RepeatedCtorNames, !RepeatedForeignNames).
build_export_enum_overrides_map_loop(TypeModuleName, ValidCtorNames,
        [Override | Overrides], !OverrideMap,
        !.SeenCtorNames, !.SeenForeignNames,
        !BadQualCtorSymNames, !InvalidCtorSymNames,
        !RepeatedCtorNames, !RepeatedForeignNames) :-
    Override = CtorSymName - ForeignName,
    some [!OK] (
        !:OK = yes,
        (
            CtorSymName = qualified(CtorModuleName, CtorName),
            ( if CtorModuleName = TypeModuleName then
                true
            else
                !:BadQualCtorSymNames =
                    cord.snoc(!.BadQualCtorSymNames, CtorSymName),
                !:OK = no
            )
        ;
            CtorSymName = unqualified(CtorName)
        ),
        ( if set_tree234.contains(ValidCtorNames, CtorName) then
            true
        else
            !:InvalidCtorSymNames =
                cord.snoc(!.InvalidCtorSymNames, CtorSymName),
            !:OK = no
        ),
        ( if set_tree234.insert_new(CtorName, !SeenCtorNames) then
            true
        else
            !:RepeatedCtorNames = cord.snoc(!.RepeatedCtorNames, CtorName),
            !:OK = no
        ),
        ( if set_tree234.insert_new(ForeignName, !SeenForeignNames) then
            true
        else
            !:RepeatedForeignNames =
                cord.snoc(!.RepeatedForeignNames, ForeignName),
            !:OK = no
        ),
        (
            !.OK = yes,
            bimap.det_insert(CtorName, ForeignName, !OverrideMap)
        ;
            !.OK = no
        )
    ),
    build_export_enum_overrides_map_loop(TypeModuleName, ValidCtorNames,
        Overrides, !OverrideMap,
        !.SeenCtorNames, !.SeenForeignNames,
        !BadQualCtorSymNames, !InvalidCtorSymNames,
        !RepeatedCtorNames, !RepeatedForeignNames).

:- pred build_export_enum_name_map(list(format_component)::in,
    prog_context::in, foreign_language::in, string::in,
    uppercase_export_enum::in, map(string, string)::in,
    list(constructor_repn)::in, map(string, string)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_name_map(ContextPieces, Context, Lang, Prefix, MakeUpperCase,
        OverrideMap, CtorRepns, NameMap, !Specs) :-
    list.foldl2(
        add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, OverrideMap),
        CtorRepns, map.init, NameMap, cord.init, BadForeignNamesCord),

    BadForeignNames = cord.to_list(BadForeignNamesCord),
    (
        BadForeignNames = []
    ;
        BadForeignNames = [_ | _],
        (
            Lang = lang_c,
            LangName = "C"
        ;
            Lang = lang_java,
            LangName = "Java"
        ;
            ( Lang = lang_csharp
            ; Lang = lang_erlang
            ),
            % XXX The code of add_ctor_to_name_map is OK
            % with Lang = lang_csharp.
            sorry($pred, "foreign_export_enum pragma for unsupported language")
        ),
        AlwaysPieces = ContextPieces ++
            [words("error: some of the constructors of the type"),
            words("cannot be converted into valid identifiers for"),
            words(LangName), suffix("."), nl],
        MakeBFNPieces = (func(BadForeignName) = [quote(BadForeignName)]),
        BadForeignPiecesList = list.map(MakeBFNPieces, BadForeignNames),
        BadForeignPieces =
            component_list_to_line_pieces(BadForeignPiecesList, [suffix(".")]),
        VerbosePieces = [words("The problematic"),
            words(choose_number(BadForeignNames,
                "foreign name is:", "foreign names are:")),
            nl_indent_delta(2)] ++ BadForeignPieces,
        Msg = simple_msg(Context,
            [always(AlwaysPieces),
            verbose_only(verbose_always, VerbosePieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred add_ctor_to_name_map(foreign_language::in, string::in,
    uppercase_export_enum::in, map(string, string)::in, constructor_repn::in,
    map(string, string)::in, map(string, string)::out,
    cord(string)::in, cord(string)::out) is det.

add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, OverrideMap, CtorRepn,
        !NameMap, !BadForeignNames) :-
    CtorSymName = CtorRepn ^ cr_name,
    CtorName = unqualify_name(CtorSymName),

    % If the user specified a name for this constructor, then use that.
    ( if map.search(OverrideMap, CtorName, UserForeignName) then
        ForeignNameTail = UserForeignName
    else
        % Otherwise derive a name automatically from the constructor name.
        (
            MakeUpperCase = uppercase_export_enum,
            ForeignNameTail = string.to_upper(CtorName)
        ;
            MakeUpperCase = do_not_uppercase_export_enum,
            ForeignNameTail = CtorName
        )
    ),
    ForeignName = Prefix ++ ForeignNameTail,
    (
        ( Lang = lang_c
        ; Lang = lang_java
        ; Lang = lang_csharp
        ),
        IsValidForeignName = pred_to_bool(is_valid_c_identifier(ForeignName))
    ;
        Lang = lang_erlang,
        sorry($pred, "foreign_export_enum for Erlang")
    ),
    (
        IsValidForeignName = yes,
        map.det_insert(CtorName, ForeignName, !NameMap)
    ;
        IsValidForeignName = no,
        !:BadForeignNames = cord.snoc(!.BadForeignNames, ForeignName)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_foreign_enum_unmapped_ctors_error(prog_context::in,
    list(format_component)::in,
    list(sym_name)::in(non_empty_list),
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_enum_unmapped_ctors_error(Context, ContextPieces, Ctors0,
        !Specs) :-
    list.sort(Ctors0, Ctors),
    list.split_upto(10, Ctors, CtorsStart, CtorsEnd),
    DoOrDoes = choose_number(Ctors, "constructor does", "constructors do"),
    PrefixPieces = ContextPieces ++ [
        words("error: the following"), words(DoOrDoes),
        words("not have a foreign value:")
    ],
    (
        CtorsEnd = [],
        CtorsPieces =
            [nl_indent_delta(2)] ++
            unqual_ctors_to_line_pieces(Ctors, [suffix(".")]) ++
            [nl_indent_delta(-2)],
        CtorsComponent = always(CtorsPieces)
    ;
        CtorsEnd = [_ | _],
        list.length(CtorsEnd, NumEndCtors),
        NonVerboseCtorsPieces =
            [nl_indent_delta(2)] ++
            unqual_ctors_to_line_pieces(CtorsStart,
                [suffix(","), fixed("...")]) ++
            [nl_indent_delta(-2), words("and"),
            int_fixed(NumEndCtors), words("more."), nl],
        VerboseCtorsPieces =
            [nl_indent_delta(2)] ++
            unqual_ctors_to_line_pieces(Ctors, [suffix(".")]) ++
            [nl_indent_delta(-2)],
        CtorsComponent =
            verbose_and_nonverbose(VerboseCtorsPieces, NonVerboseCtorsPieces)
    ),
    Msg = simple_msg(Context,
        [always(PrefixPieces), CtorsComponent]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- func unqual_ctors_to_line_pieces(list(sym_name), list(format_component))
    = list(format_component).

unqual_ctors_to_line_pieces(Ctors, Final) = Pieces :-
    Components = list.map(unqual_ctor_to_format_component, Ctors),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func unqual_ctor_to_format_component(sym_name) = list(format_component).

unqual_ctor_to_format_component(SymName) = [unqual_sym_name(SymName)].

%---------------------------------------------------------------------------%

:- func qual_ctors_to_line_pieces(list(sym_name), list(format_component))
    = list(format_component).

qual_ctors_to_line_pieces(Ctors, Final) = Pieces :-
    Components = list.map(qual_ctor_to_format_component, Ctors),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func qual_ctor_to_format_component(sym_name) = list(format_component).

qual_ctor_to_format_component(SymName) = [qual_sym_name(SymName)].

%---------------------------------------------------------------------------%

:- pred add_unknown_ctors_error(prog_context::in, list(format_component)::in,
    list(sym_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

add_unknown_ctors_error(Context, ContextPieces, Ctors, !Specs) :-
    IsOrAre = choose_number(Ctors, "symbol is not a constructor",
        "symbols are not constructors"),
    ErrorPieces = [invis_order_default_start(1),
        words("error: the following"), words(IsOrAre),
        words("of the type:"), nl_indent_delta(2)] ++
        unqual_ctors_to_line_pieces(Ctors, [suffix(".")]),
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_bad_qual_ctors_error(prog_context::in, list(format_component)::in,
    list(sym_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

add_bad_qual_ctors_error(Context, ContextPieces, Ctors, !Specs) :-
    HasOrHave = choose_number(Ctors, "symbol has", "symbols have"),
    ErrorPieces = [invis_order_default_start(2),
        words("error: the following"),
        words(HasOrHave), words("a module qualification"),
        words("that is not compatible with the type definition:"),
        nl_indent_delta(2)] ++
        qual_ctors_to_line_pieces(Ctors, [suffix(".")]),
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_foreign_enum_bijection_error(prog_context::in,
    list(format_component)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_enum_bijection_error(Context, ContextPieces, !Specs) :-
    ErrorPieces = [words("error: "),
        words("the mapping between Mercury enumeration values and"),
        words("foreign values does not form a bijection."), nl],
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred add_foreign_enum_pragma_in_interface_error(prog_context::in,
    sym_name::in, arity::in, list(error_spec)::in, list(error_spec)::out)
    is det.

add_foreign_enum_pragma_in_interface_error(Context, TypeSymame, TypeArity,
        !Specs) :-
    ErrorPieces = [words("Error: "),
        pragma_decl("foreign_enum"), words("declaration for"),
        qual_sym_name_and_arity(sym_name_arity(TypeSymame, TypeArity)),
        words("in module interface."), nl],
    Msg = simple_msg(Context, [always(ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred maybe_add_duplicate_foreign_enum_error(sym_name::in, arity::in,
    foreign_language::in, type_status::in, prog_context::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_add_duplicate_foreign_enum_error(TypeSymame, TypeArity, Lang,
        PragmaStatus, OldContext, Context, !Specs) :-
    ( if PragmaStatus = type_status(status_opt_imported) then
        true
    else
        TypeSymNameArity = sym_name_arity(TypeSymame, TypeArity),
        LangStr = mercury_foreign_language_to_string(Lang),
        CurPieces = [words("Error: duplicate foreign_enum pragma"),
            words("for type constructor"),
            qual_sym_name_and_arity(TypeSymNameArity),
            words("and target language"), fixed(LangStr), suffix("."), nl],
        OldPieces = [words("The first foreign_enum pragma"),
            words("for"), qual_sym_name_and_arity(TypeSymNameArity),
            words("and"), fixed(LangStr), words("was here."), nl],
        CurMsg = simple_msg(Context, [always(CurPieces)]),
        OldMsg = simple_msg(OldContext, [always(OldPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
            [CurMsg, OldMsg]),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%

:- pred add_error_severity_pieces(prog_context::in, list(format_component)::in,
    {error_severity, list(format_component)}::in,
    list(error_spec)::in, list(error_spec)::out) is det.

add_error_severity_pieces(Context, ContextPieces, {Severity, ErrorPieces},
        !Specs) :-
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- pred report_if_builtin_type(sym_name::in, arity::in,
    list({error_severity, list(format_component)})::in,
    list({error_severity, list(format_component)})::out) is det.

report_if_builtin_type(TypeSymame, TypeArity, !ErrorSeveritiesPieces) :-
    ( if
        % Emit an error message for foreign_enum and foreign_export_enum
        % pragmas for the builtin atomic types.
        TypeArity = 0,
        is_builtin_type_sym_name(TypeSymame)
    then
        BuiltinErrorPieces = [words("error: "),
            unqual_sym_name_and_arity(sym_name_arity(TypeSymame, TypeArity)),
            words("is a builtin type."), nl],
        !:ErrorSeveritiesPieces = [{severity_error, BuiltinErrorPieces} |
            !.ErrorSeveritiesPieces]
    else
        true
    ).

:- type not_enum_info
    --->    not_enum_du(
                % The non-enum cons_ids.
                list(cons_id)
            )
    ;       not_enum_non_du(
                % What kind of non-du type is it?
                string
            ).

:- pred report_not_enum_type(sym_name::in, arity::in, not_enum_info::in,
    list({error_severity, list(format_component)})::in,
    list({error_severity, list(format_component)})::out) is det.

report_not_enum_type(TypeSymName, TypeArity, NotEnumInfo,
        !ErrorSeveritiesPieces) :-
    TypeSNA = sym_name_arity(TypeSymName, TypeArity),
    (
        NotEnumInfo = not_enum_non_du(TypeKind),
        ErrorPieces = [words("error: "),
            qual_sym_name_and_arity(TypeSNA),
            words("is not an enumeration type;"),
            words("it is"), words(TypeKind), suffix("."), nl],
        !:ErrorSeveritiesPieces =
            [{severity_error, ErrorPieces} | !.ErrorSeveritiesPieces]
    ;
        NotEnumInfo = not_enum_du(NonEnumConsIds),
        list.sort_and_remove_dups(NonEnumConsIds, SortedNonEnumConsIds),
        (
            SortedNonEnumConsIds = []
        ;
            SortedNonEnumConsIds = [_ | _],
            ConsIdPieces =
                component_list_to_pieces("and",
                    list.map(wrap_unqual_cons_id_and_maybe_arity,
                        SortedNonEnumConsIds)),
            ItHasThese = choose_number(SortedNonEnumConsIds,
                words("It has this non-zero arity constructor:"),
                words("It has these non-zero arity constructors:")),
            ErrorPieces = [words("error: "), qual_sym_name_and_arity(TypeSNA),
                words("is not an enumeration type."), nl, ItHasThese,
                nl_indent_delta(2)] ++ ConsIdPieces ++
                [suffix("."), nl_indent_delta(-2)],
            !:ErrorSeveritiesPieces =
                [{severity_error, ErrorPieces} | !.ErrorSeveritiesPieces]
        )
    ).

:- func wrap_unqual_cons_id_and_maybe_arity(cons_id) = format_component.

wrap_unqual_cons_id_and_maybe_arity(ConsId) =
    unqual_cons_id_and_maybe_arity(ConsId).

%---------------------------------------------------------------------------%
:- end_module hlds.add_foreign_enum.
%---------------------------------------------------------------------------%
