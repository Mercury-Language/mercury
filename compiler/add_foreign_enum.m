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
% The task of the first part of this module is to check foreign_enum pragmas
% and gather from them the information that du_type_layout.m will use
% to help decide the representations of the types named in those pragmas.
%
% The task of the second part of this module is to check foreign_export_enum
% pragmas, and to record the information from the correct ones in the
% module_info for the code generator to use.
%
% The third part of the module consists of some utility predicates
% used by both of the previous parts, many of which are predicates that
% generate error messages.
%
%---------------------------------------------------------------------------%

:- module hlds.add_foreign_enum.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bimap.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set_tree234.

%---------------------------------------------------------------------------%

:- type cons_id_to_tag_map == map(cons_id, cons_tag).

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

    % build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
    %   Overrides, !OverrideMap, !SeenCtorNames, !.SeenForeignNames,
    %   !BadQualCtorSymNames, !InvalidCtorSymNames,
    %   !RepeatedCtorNames, !RepeatedForeignNames):
    %
    % Exported to decide_type_repn.m.
    %
:- pred build_ctor_name_to_foreign_name_map_loop(module_name::in,
    set_tree234(string)::in, assoc_list(sym_name, string)::in,
    bimap(string, string)::in, bimap(string, string)::out,
    set_tree234(string)::in, set_tree234(string)::out, set_tree234(string)::in,
    cord(sym_name)::in, cord(sym_name)::out,
    cord(sym_name)::in, cord(sym_name)::out,
    cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module hlds.make_hlds_error.
:- import_module hlds.status.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 1: the implementation of foreign_enums.
%

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
    FEInfo = pragma_info_foreign_enum(Lang, TypeCtor,
        OoMMercuryForeignTagPairs),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    TypeSNA = sym_name_arity(TypeSymName, TypeArity),
    ContextPieces = [words("In"), pragma_decl("foreign_enum"),
        words("declaration for type"), qual_sym_name_and_arity(TypeSNA),
        suffix(":"), nl],

    some [!Specs] (
        !:Specs = [],
        report_if_builtin_type(Context, ContextPieces, TypeSymName, TypeArity,
            !Specs),

        module_info_get_type_table(ModuleInfo, TypeTable),
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
                NotThisModulePieces = ContextPieces ++
                    [words("error: "), qual_sym_name_and_arity(TypeSNA),
                    words("is not defined in this module."), nl],
                NotThisModuleMsg = simple_msg(Context,
                    [always(NotThisModulePieces)]),
                NotThisModuleSpec = error_spec(severity_error,
                    phase_parse_tree_to_hlds, [NotThisModuleMsg]),
                !:Specs = [NotThisModuleSpec | !.Specs]
            ),

            get_type_defn_body(TypeDefn, TypeBody),
            (
                ( TypeBody = hlds_eqv_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_)
                ; TypeBody = hlds_foreign_type(_)
                ),
                report_not_du_type(Context, ContextPieces,
                    TypeSymName, TypeArity, TypeBody, !Specs)
            ;
                TypeBody = hlds_du_type(Ctors, _MaybeUserEq, MaybeRepn,
                    _IsForeignType),
                expect(unify(MaybeRepn, no), $pred, "MaybeRepn != no"),

                MercuryForeignTagPairs =
                    one_or_more_to_list(OoMMercuryForeignTagPairs),
                build_mercury_foreign_map(TypeModuleName,
                    TypeSymName, TypeArity, for_foreign_enum,
                    Context, ContextPieces, one_or_more_to_list(Ctors),
                    MercuryForeignTagPairs, MercuryForeignTagBimap, !Specs),
                MercuryForeignTagNames =
                    bimap.to_assoc_list(MercuryForeignTagBimap),
                list.map(
                    map_cons_id_to_foreign_tag(TypeCtor, TypeModuleName,
                        Lang),
                    MercuryForeignTagNames, ConsIdForeignTags),
                % Converting each name to a cons_id would preserve the order,
                % but bimap.to_assoc_list does not guarantee an order,
                % and unlike map.m, bimap.m does not have to_sorted_assoc_list.
                map.from_assoc_list(ConsIdForeignTags, ConsIdToTagMap),

                % Work out what language's foreign_enum pragma we should be
                % looking at for the current compilation target language.
                module_info_get_globals(ModuleInfo, Globals),
                globals.get_target(Globals, TargetLanguage),
                LangForForeignEnums =
                    target_lang_to_foreign_enum_lang(TargetLanguage),

                ( if
                    Lang = LangForForeignEnums,
                    !.Specs = []
                then
                    MaybeTagValuesToSet = yes({ConsIdToTagMap, Lang})
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

    % For a given target language work out which language's foreign_enum
    % pragma we should be looking at.
    %
:- func target_lang_to_foreign_enum_lang(compilation_target)
    = foreign_language.

target_lang_to_foreign_enum_lang(target_c) = lang_c.
target_lang_to_foreign_enum_lang(target_csharp) = lang_csharp.
target_lang_to_foreign_enum_lang(target_java) = lang_java.
target_lang_to_foreign_enum_lang(target_erlang) = lang_erlang.

:- pred map_cons_id_to_foreign_tag(type_ctor::in, module_name::in,
    foreign_language::in,
    pair(string, string)::in, pair(cons_id, cons_tag)::out) is det.

map_cons_id_to_foreign_tag(TypeCtor, TypeModuleName, ForeignLanguage,
        CtorName - ForeignTagName, ConsId - ForeignTag) :-
    CtorSymName = qualified(TypeModuleName, CtorName),
    ConsId = cons(CtorSymName, 0, TypeCtor),
    ForeignTag = foreign_tag(ForeignLanguage, ForeignTagName).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 2: the implementation of foreign_export_enums.
%

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
    some [!Specs] (
        !:Specs = [],
        report_if_builtin_type(Context, ContextPieces, TypeSymName, TypeArity,
            !Specs),

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
                ( TypeBody = hlds_eqv_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_)
                ; TypeBody = hlds_foreign_type(_)
                ),
                report_not_du_type(Context, ContextPieces,
                    TypeSymName, TypeArity, TypeBody, !Specs)
            ;
                TypeBody = hlds_du_type(Ctors, _MaybeUserEq, MaybeRepn,
                    _IsForeignType),
                (
                    MaybeRepn = no,
                    unexpected($pred, "MaybeRepn = no")
                ;
                    MaybeRepn = yes(Repn),
                    CtorRepns = Repn ^ dur_ctor_repns
                ),

                build_mercury_foreign_map(TypeModuleName,
                    TypeSymName, TypeArity, for_foreign_export_enum,
                    Context, ContextPieces, one_or_more_to_list(Ctors),
                    Overrides, OverrideBimap, !Specs),
                OverrideMap = bimap.forward_map(OverrideBimap),

                Attributes =
                    export_enum_attributes(MaybePrefix, MakeUpperCase),
                (
                    MaybePrefix = yes(Prefix)
                ;
                    MaybePrefix = no,
                    Prefix = ""
                ),
                build_export_enum_name_map(ContextPieces, Context, Lang,
                    Prefix, MakeUpperCase, OverrideMap, CtorRepns,
                    NameMap, !Specs),
                (
                    !.Specs = [],
                    ExportedEnum = exported_enum_info(TypeCtor, CtorRepns,
                        Lang, NameMap, Context),
                    module_info_get_exported_enums(!.ModuleInfo,
                        ExportedEnums0),
                    ExportedEnums = [ExportedEnum | ExportedEnums0],
                    module_info_set_exported_enums(ExportedEnums,
                        !ModuleInfo)
                ;
                    !.Specs = [_ | _]
                )
            )
        else
            % This case corresponds to an undefined type. We do not issue
            % an error message for it here, since module qualification
            % will have already done so.
            true
        ),
        Specs = !.Specs ++ Specs0
    ).

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
%
% Part 3: utilities that help implement both foreign_enums and
% foreign_export_enums.
%

:- type for_fe_or_fee
    --->    for_foreign_enum
    ;       for_foreign_export_enum.

:- pred build_mercury_foreign_map(module_name::in,
    sym_name::in, arity::in, for_fe_or_fee::in, prog_context::in,
    list(format_component)::in, list(constructor)::in,
    assoc_list(sym_name, string)::in, bimap(string, string)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_mercury_foreign_map(TypeModuleName, TypeSymName, TypeArity,
        ForWhat, Context, ContextPieces, Ctors, Overrides, OverrideMap,
        !Specs) :-
    find_nonenum_ctors_build_valid_ctor_names(Ctors,
        set_tree234.init, ValidCtorNames, cord.init, NonEnumSNAsCord),

    ( if cord.is_empty(NonEnumSNAsCord) then
        true
    else
        NotEnumInfo = not_enum_du(cord.to_list(NonEnumSNAsCord)),
        report_not_enum_type(Context, ContextPieces, TypeSymName, TypeArity,
            NotEnumInfo, !Specs)
    ),

    SeenCtorNames0 = set_tree234.init,
    SeenForeignNames0 = set_tree234.init,
    BadQualCtorSymNamesCord0 = cord.init,
    InvalidCtorSymNamesCord0 = cord.init,
    RepeatedCtorNamesCord0 = cord.init,
    RepeatedForeignNamesCord0 = cord.init,
    build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
        Overrides, bimap.init, OverrideMap,
        SeenCtorNames0, SeenCtorNames, SeenForeignNames0,
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
        % How should we describe the contents of RepeatedForeignNames
        % in error messages: as "names" or "values"?
        %
        % (The variable is RepeatedForeignNames because
        % RepeatedForeignNamesOrValues would be too long.)
        (
            ForWhat = for_foreign_export_enum,
            % Foreign_export_enums specify name of the foreign lval
            % (variable name or macro name) to set to the representation
            % of the Mercury constant chosen by the Mercury compiler.
            NameOrValue = "name",
            NamesOrValues = "names"
        ;
            ForWhat = for_foreign_enum,
            % Foreign_enums tell the Mercury compiler what rval it should
            % use to represent the Mercury constant. The rval may be
            % the value of a variable, but it may also be a constant
            % (or possibly even a constant expression).
            NameOrValue = "value",
            NamesOrValues = "values"
        ),
        MainPieces = ContextPieces ++
            [invis_order_default_start(3), words("error: "),
            words("the specified mapping between"),
            words("the names of Mercury constructors"),
            words("and the corresponding foreign"), words(NamesOrValues),
            words("is inconsistent."), nl],
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
                    NameOrValue ++ " is", NamesOrValues ++ " are")),
                words("repeated:"), nl_indent_delta(2)] ++
                list_to_quoted_pieces(RepeatedForeignNames) ++
                [suffix("."), nl_indent_delta(-2)]
        ),
        Pieces = MainPieces ++ CtorNamePieces ++ ForeignNamePieces,
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ),
    (
        ForWhat = for_foreign_export_enum
    ;
        ForWhat = for_foreign_enum,
        set_tree234.difference(ValidCtorNames, SeenCtorNames, UnseenCtorNames),
        set_tree234.to_sorted_list(UnseenCtorNames, UnseenCtorNamesList),
        (
            UnseenCtorNamesList = []
        ;
            UnseenCtorNamesList = [_ | _],
            add_foreign_enum_unmapped_ctors_error(Context, ContextPieces,
                UnseenCtorNamesList, !Specs)
        )
    ).

:- pred find_nonenum_ctors_build_valid_ctor_names(list(constructor)::in,
    set_tree234(string)::in, set_tree234(string)::out,
    cord(sym_name_and_arity)::in, cord(sym_name_and_arity)::out) is det.

find_nonenum_ctors_build_valid_ctor_names([], !ValidNamesSet, !NonEnumSNAs).
find_nonenum_ctors_build_valid_ctor_names([Ctor | Ctors],
        !ValidNamesSet, !NonEnumSNAs) :-
    CtorSymName = Ctor ^ cons_name,
    CtorArity = Ctor ^ cons_num_args,
    ( if CtorArity = 0 then
        true
    else
        CtorSNA = sym_name_arity(CtorSymName, CtorArity),
        !:NonEnumSNAs = cord.snoc(!.NonEnumSNAs, CtorSNA)
    ),
    CtorName = unqualify_name(CtorSymName),
    set_tree234.insert(CtorName, !ValidNamesSet),
    find_nonenum_ctors_build_valid_ctor_names(Ctors,
        !ValidNamesSet, !NonEnumSNAs).

build_ctor_name_to_foreign_name_map_loop(_, _, [], !OverrideMap,
        !SeenCtorNames, _SeenForeignNames,
        !BadQualCtorSymNames, !InvalidCtorSymNames,
        !RepeatedCtorNames, !RepeatedForeignNames).
build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
        [Override | Overrides], !OverrideMap,
        !SeenCtorNames, !.SeenForeignNames,
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
    build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
        Overrides, !OverrideMap,
        !SeenCtorNames, !.SeenForeignNames,
        !BadQualCtorSymNames, !InvalidCtorSymNames,
        !RepeatedCtorNames, !RepeatedForeignNames).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_foreign_enum_unmapped_ctors_error(prog_context::in,
    list(format_component)::in,
    list(string)::in(non_empty_list),
    list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_enum_unmapped_ctors_error(Context, ContextPieces, CtorNames0,
        !Specs) :-
    list.sort(CtorNames0, CtorNames),
    list.split_upto(10, CtorNames, CtorsStart, CtorsEnd),
    DoOrDoes = choose_number(CtorNames, "constructor does", "constructors do"),
    PrefixPieces = ContextPieces ++ [
        words("error: the following"), words(DoOrDoes),
        words("not have a foreign value:")
    ],
    (
        CtorsEnd = [],
        CtorsPieces =
            [nl_indent_delta(2)] ++
            ctor_names_to_line_pieces(CtorNames, [suffix(".")]) ++
            [nl_indent_delta(-2)],
        CtorsComponent = always(CtorsPieces)
    ;
        CtorsEnd = [_ | _],
        list.length(CtorsEnd, NumEndCtors),
        NonVerboseCtorsPieces =
            [nl_indent_delta(2)] ++
            ctor_names_to_line_pieces(CtorsStart,
                [suffix(","), fixed("...")]) ++
            [nl_indent_delta(-2), words("and"),
            int_fixed(NumEndCtors), words("more."), nl],
        VerboseCtorsPieces =
            [nl_indent_delta(2)] ++
            ctor_names_to_line_pieces(CtorNames, [suffix(".")]) ++
            [nl_indent_delta(-2)],
        CtorsComponent =
            verbose_and_nonverbose(VerboseCtorsPieces, NonVerboseCtorsPieces)
    ),
    Msg = simple_msg(Context, [always(PrefixPieces), CtorsComponent]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%

:- func ctor_names_to_line_pieces(list(string), list(format_component))
    = list(format_component).

ctor_names_to_line_pieces(CtorNames, Final) = Pieces :-
    Components = list.map(ctor_name_to_format_component, CtorNames),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func ctor_name_to_format_component(string) = list(format_component).

ctor_name_to_format_component(CtorName) = [quote(CtorName)].

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

:- pred report_if_builtin_type(prog_context::in, list(format_component)::in,
    sym_name::in, arity::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_if_builtin_type(Context, ContextPieces, TypeSymame, TypeArity,
        !Specs) :-
    ( if
        % Emit an error message for foreign_enum and foreign_export_enum
        % pragmas for the builtin atomic types.
        TypeArity = 0,
        is_builtin_type_sym_name(TypeSymame)
    then
        Pieces = ContextPieces ++ [words("error: "),
            unqual_sym_name_and_arity(sym_name_arity(TypeSymame, TypeArity)),
            words("is a builtin type."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- type not_enum_info
    --->    not_enum_du(
                % The non-enum sym_names and their (nonzero) arities.
                list(sym_name_and_arity)
            )
    ;       not_enum_non_du(
                % What kind of non-du type is it?
                string
            ).

:- inst non_du_type_body for hlds_type_body/0
    --->    hlds_eqv_type(ground)
    ;       hlds_foreign_type(ground)
    ;       hlds_solver_type(ground)
    ;       hlds_abstract_type(ground).

:- pred report_not_du_type(prog_context::in, list(format_component)::in,
    sym_name::in, arity::in, hlds_type_body::in(non_du_type_body),
    list(error_spec)::in, list(error_spec)::out) is det.

report_not_du_type(Context, ContextPieces, TypeSymName, TypeArity, TypeBody,
        !Specs) :-
    (
        TypeBody = hlds_eqv_type(_),
        TypeKindDesc = "an equivalence type"
    ;
        TypeBody = hlds_abstract_type(_),
        TypeKindDesc = "an abstract type"
    ;
        TypeBody = hlds_solver_type(_),
        TypeKindDesc = "a solver type"
    ;
        TypeBody = hlds_foreign_type(_),
        TypeKindDesc = "a foreign type"
    ),
    report_not_enum_type(Context, ContextPieces, TypeSymName, TypeArity,
        not_enum_non_du(TypeKindDesc), !Specs).

:- pred report_not_enum_type(prog_context::in, list(format_component)::in,
    sym_name::in, arity::in, not_enum_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_not_enum_type(Context, ContextPieces, TypeSymName, TypeArity,
        NotEnumInfo, !Specs) :-
    TypeSNA = sym_name_arity(TypeSymName, TypeArity),
    (
        NotEnumInfo = not_enum_non_du(TypeKindDesc),
        Pieces = ContextPieces ++ [words("error: "),
            qual_sym_name_and_arity(TypeSNA),
            words("is not an enumeration type;"),
            words("it is"), words(TypeKindDesc), suffix("."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        NotEnumInfo = not_enum_du(NonEnumSNAs),
        % NOTE Sorting would put the non-constant constructors into
        % alphabetical order; without sorting, they are in declaration order.
        % Printing them in declaration order seems more useful to me -zs.

        % list.sort_and_remove_dups(NonEnumSNAs, OrderedNonEnumSNAs),
        OrderedNonEnumSNAs = NonEnumSNAs,
        (
            OrderedNonEnumSNAs = []
        ;
            OrderedNonEnumSNAs = [_ | _],
            SNAPieces =
                component_list_to_pieces("and",
                    list.map(func(SNA) = unqual_sym_name_and_arity(SNA),
                        OrderedNonEnumSNAs)),
            ItHasThese = choose_number(OrderedNonEnumSNAs,
                words("It has this non-zero arity constructor:"),
                words("It has these non-zero arity constructors:")),
            Pieces = ContextPieces ++ [words("error: "),
                qual_sym_name_and_arity(TypeSNA),
                words("is not an enumeration type."), nl,
                ItHasThese, nl_indent_delta(2)] ++ SNAPieces ++
                [suffix("."), nl_indent_delta(-2)],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.add_foreign_enum.
%---------------------------------------------------------------------------%
