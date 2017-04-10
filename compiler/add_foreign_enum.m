%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.add_pragma.add_foreign_enum.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred add_pragma_foreign_export_enum(pragma_info_foreign_export_enum::in,
    type_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred add_pragma_foreign_enum(pragma_info_foreign_enum::in,
    type_status::in, prog_context::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module assoc_list.
:- import_module bimap.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

add_pragma_foreign_export_enum(FEEInfo, _TypeStatus, Context,
        !ModuleInfo, !Specs) :-
    FEEInfo = pragma_info_foreign_export_enum(Lang, TypeCtor,
        Attributes, Overrides),
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable),
    ContextPieces = [words("In"), pragma_decl("foreign_export_enum"),
        words("declaration for type"),
        qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
        suffix(":"), nl],
    ( if
        % Emit an error message for foreign_export_enum pragmas for the
        % builtin atomic types.
        TypeArity = 0,
        is_builtin_type_sym_name(TypeName)
    then
        ErrorPieces = [words("error: "),
            unqual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
            words("is a builtin type."), nl],
        MaybeError = yes({severity_error, ErrorPieces})
    else
        ( if search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn) then
            get_type_defn_body(TypeDefn, TypeBody),
            (
                ( TypeBody = hlds_eqv_type(_)
                ; TypeBody = hlds_abstract_type(_)
                ; TypeBody = hlds_solver_type(_, _)
                ; TypeBody = hlds_foreign_type(_)
                ),
                ErrorPieces = [words("error: "),
                    qual_sym_name_and_arity(
                        sym_name_arity(TypeName, TypeArity)),
                    words("is not an enumeration type."), nl],
                MaybeError = yes({severity_error, ErrorPieces})
            ;
                % XXX How should we handle IsForeignType here?
                TypeBody = hlds_du_type(Ctors, _TagValues, _CheaperTagTest,
                    DuTypeKind, _MaybeUserEq, _MaybeDirectArgCtors,
                    _ReservedTag, _ReservedAddr, _IsForeignType),
                (
                    ( DuTypeKind = du_type_kind_mercury_enum
                    ; DuTypeKind = du_type_kind_foreign_enum(_)
                    ; DuTypeKind = du_type_kind_direct_dummy
                    ),
                    Attributes = export_enum_attributes(MaybePrefix,
                        MakeUpperCase),
                    (
                        MaybePrefix = yes(Prefix)
                    ;
                        MaybePrefix = no,
                        Prefix = ""
                    ),
                    build_export_enum_overrides_map(TypeName, Context,
                        ContextPieces, Overrides, MaybeOverridesMap, !Specs),
                    (
                        MaybeOverridesMap = yes(OverridesMap),
                        build_export_enum_name_map(ContextPieces, Lang,
                            TypeName, TypeArity, Context, Prefix,
                            MakeUpperCase, OverridesMap, Ctors, MaybeMapping,
                            !Specs),
                        (
                            MaybeMapping = yes(Mapping),
                            ExportedEnum = exported_enum_info(Lang, Context,
                                TypeCtor, Mapping),
                            module_info_get_exported_enums(!.ModuleInfo,
                                ExportedEnums0),
                            ExportedEnums = [ExportedEnum | ExportedEnums0],
                            module_info_set_exported_enums(ExportedEnums,
                                !ModuleInfo)
                        ;
                            MaybeMapping = no
                        )
                    ;
                        MaybeOverridesMap = no
                    ),
                    MaybeError = no
                ;
                    ( DuTypeKind = du_type_kind_general
                    ; DuTypeKind = du_type_kind_notag(_, _, _)
                    ),
                    % XXX Maybe we should add a verbose component
                    % that identifies the non-zero arity constructors.
                    ErrorPieces = [words("error: "),
                        qual_sym_name_and_arity(
                            sym_name_arity(TypeName, TypeArity)),
                        words("is not an enumeration type."),
                        words("It has one or more non-zero arity"),
                        words("constructors."), nl],
                    MaybeError = yes({severity_error, ErrorPieces})
                )
            )
        else
            % This case corresponds to an undefined type. We do not issue
            % an error message for it here, since module qualification
            % will have already done so.
            MaybeError = no
        )
    ),
    (
        MaybeError = no
    ;
        MaybeError = yes({Severity, MaybeErrorPieces}),
        Msg = simple_msg(Context, [always(ContextPieces ++ MaybeErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_export_enum_overrides_map(sym_name::in, prog_context::in,
    format_components::in, assoc_list(sym_name, string)::in,
    maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_overrides_map(TypeName, Context, ContextPieces,
        OverridesList0, MaybeOverridesMap, !Specs) :-
    (
        TypeName = qualified(ModuleName, _)
    ;
        TypeName = unqualified(_),
        unexpected($module, $pred,
            "unqualified type name while building override map")
    ),
    % Strip off module qualifiers that match those of the type being exported.
    % We leave those that do not match so that they can be reported as errors
    % later.
    StripQualifiers = (func(Name0) = Name :-
        (
            Name0 = qualified(ModuleQualifier, UnqualName),
            ( if ModuleQualifier = ModuleName then
                Name = unqualified(UnqualName)
            else
                Name = Name0
            )
        ;
            Name0 = unqualified(_),
            Name = Name0
        )
    ),
    OverridesList = assoc_list.map_keys_only(StripQualifiers,
        OverridesList0),
    ( if bimap.from_assoc_list(OverridesList, OverridesMap0) then
        OverridesMap = bimap.forward_map(OverridesMap0),
        MaybeOverridesMap = yes(OverridesMap)
    else
        MaybeOverridesMap = no,
        % XXX we should report exactly why it is not a bijective.
        ErrorPieces = [words("error: "),
            words("the user-specified mapping between Mercury and"),
            words("foreign names does not form a bijection.")],
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_export_enum_name_map(format_components::in, foreign_language::in,
    sym_name::in, arity::in, prog_context::in, string::in,
    uppercase_export_enum::in, map(sym_name, string)::in,
    list(constructor)::in, maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_export_enum_name_map(ContextPieces, Lang, TypeName, TypeArity, Context,
        Prefix, MakeUpperCase, Overrides0, Ctors, MaybeMapping, !Specs) :-
    (
        TypeName = qualified(TypeModuleQual, _)
    ;
        % The type name should have been module qualified by now.
        TypeName = unqualified(_),
        unexpected($module, $pred,
            "unqualified type name for foreign_export_enum")
    ),

    list.foldl3(
        add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, TypeModuleQual),
        Ctors, Overrides0, Overrides, map.init, NameMap, [], BadCtors),

    % Check for any remaining user-specified renamings that didn't match
    % the constructors of the type and report errors for them.

    ( if map.is_empty(Overrides) then
        (
            BadCtors = [],
            check_name_map_for_conflicts(Context, ContextPieces, NameMap,
                MaybeMapping, !Specs)
        ;
            BadCtors = [_ | _],
            (
                Lang = lang_c,
                What = "C identifiers."
            ;
                Lang = lang_java,
                What = "Java identifiers."
            ;
                ( Lang = lang_csharp
                ; Lang = lang_erlang
                ),
                sorry($module, $pred,
                    "foreign_export_enum pragma for unsupported language")
            ),
            BadCtorsErrorPieces = [
                words("error: not all the constructors of the type"),
                qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
                words("can be converted into valid " ++ What)
            ],
            list.sort(BadCtors, SortedBadCtors),
            BadCtorComponents = list.map((func(S) = [qual_sym_name(S)]),
                SortedBadCtors),
            BadCtorsList = component_list_to_line_pieces(
                BadCtorComponents, [nl]),
            BadCtorsVerboseErrorPieces = [words("The following"),
                words(choose_number(BadCtors, "constructor", "constructors")),
                words("cannot be converted:"), nl_indent_delta(2)]
                ++ BadCtorsList,
            BadCtorsMsg = simple_msg(Context,
                [always(ContextPieces ++ BadCtorsErrorPieces),
                verbose_only(verbose_always, BadCtorsVerboseErrorPieces)]),
            BadCtorsSpec = error_spec(severity_error,
                phase_parse_tree_to_hlds, [BadCtorsMsg]),
            !:Specs = [BadCtorsSpec | !.Specs],
            MaybeMapping = no
        )
    else
        InvalidRenamings = map.keys(Overrides),
        InvalidRenamingSymNamePieces =
            list.map((func(S) = [qual_sym_name(S)]), InvalidRenamings),
        InvalidRenamingPieces = [words("the following"),
            words(choose_number(InvalidRenamings,
                "constructor does", "constructors do")),
            words("not match any of the constructors of"),
            qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
            suffix(":"), nl] ++
            component_list_to_line_pieces(InvalidRenamingSymNamePieces,
                [suffix("."), nl]),
        InvalidRenamingMsg = simple_msg(Context,
            [always(ContextPieces ++ InvalidRenamingPieces)]),
        InvalidRenamingSpec = error_spec(severity_error,
            phase_parse_tree_to_hlds, [InvalidRenamingMsg]),
        !:Specs = [InvalidRenamingSpec | !.Specs],
        MaybeMapping = no
        % NOTE: in the presence of this error we do not report if constructors
        % could not be converted to names in the foreign language.
    ).

    % Check that the mapping from foreign names to Mercury names is not
    % one-to-many.
    %
:- pred check_name_map_for_conflicts(prog_context::in, format_components::in,
    map(sym_name, string)::in, maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_name_map_for_conflicts(Context, ContextPieces, NameMap, MaybeNameMap,
        !Specs) :-
    NamesAndForeignNames = map.to_assoc_list(NameMap),
    ( if bimap.from_assoc_list(NamesAndForeignNames, _) then
        MaybeNameMap = yes(NameMap)
    else
        MaybeNameMap = no,
        % XXX We should report exactly why it is not bijective.
        ErrorPieces = [words("error:"),
            words("the mapping between Mercury and foreign names"),
            words("does not form a bijection."), nl],
        Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred add_ctor_to_name_map(foreign_language::in,
    string::in, uppercase_export_enum::in, sym_name::in, constructor::in,
    map(sym_name, string)::in, map(sym_name, string)::out,
    map(sym_name, string)::in, map(sym_name, string)::out,
    list(sym_name)::in, list(sym_name)::out) is det.

add_ctor_to_name_map(Lang, Prefix, MakeUpperCase, _TypeModQual, Ctor,
        !Overrides, !NameMap, !BadCtors) :-
    CtorSymName = Ctor ^ cons_name,
    (
        % All of the constructor sym_names should be module qualified by now.
        % We unqualify them before inserting them into the mapping since
        % the code in export.m expects that to be done.

        CtorSymName = qualified(_, _),
        UnqualCtorName = unqualify_name(CtorSymName),
        UnqualSymName = unqualified(UnqualCtorName)
    ;
        CtorSymName = unqualified(_),
        unexpected($module, $pred, "unqualified constructor name")
    ),

    % If the user specified a name for this constructor then use that.
    ( if map.remove(UnqualSymName, UserForeignName, !Overrides) then
        ForeignNameTail = UserForeignName
    else
        % Otherwise try to derive a name automatically from the
        % constructor name.
        (
            MakeUpperCase = uppercase_export_enum,
            ForeignNameTail = string.to_upper(UnqualCtorName)
        ;
            MakeUpperCase = do_not_uppercase_export_enum,
            ForeignNameTail = UnqualCtorName
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
        sorry($module, $pred, "foreign_export_enum for Erlang")
    ),
    (
        IsValidForeignName = yes,
        map.det_insert(UnqualSymName, ForeignName, !NameMap)
    ;
        IsValidForeignName = no,
        list.cons(UnqualSymName, !BadCtors)
    ).

%-----------------------------------------------------------------------------%

add_pragma_foreign_enum(FEInfo, PragmaStatus, Context, !ModuleInfo, !Specs) :-
    FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, ForeignTagValues),
    TypeCtor = type_ctor(TypeName, TypeArity),
    module_info_get_type_table(!.ModuleInfo, TypeTable0),
    ContextPieces = [words("In"), pragma_decl("foreign_enum"),
        words("declaration for type"),
        qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
        suffix(":"), nl],
    ( if
        % Emit an error message for foreign_enum pragmas for the
        % builtin atomic types.
        TypeArity = 0,
        is_builtin_type_sym_name(TypeName)
    then
        ErrorPieces = [words("error: "),
            unqual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
            words("is a builtin type."), nl],
        MaybeError = yes({severity_error, ErrorPieces})
    else if
        search_type_ctor_defn(TypeTable0, TypeCtor, TypeDefn0)
    then
        get_type_defn_body(TypeDefn0, TypeBody0),
        (
            ( TypeBody0 = hlds_eqv_type(_)
            ; TypeBody0 = hlds_abstract_type(_)
            ; TypeBody0 = hlds_solver_type(_, _)
            ; TypeBody0 = hlds_foreign_type(_)
            ),
            ErrorPieces = [words("error: "),
                qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
                words("is not an enumeration type."), nl],
            MaybeError = yes({severity_error, ErrorPieces})
        ;
            TypeBody0 = hlds_du_type(Ctors, OldTagValues, CheaperTagTest,
                DuTypeKind0, MaybeUserEq, MaybeDirectArgCtors,
                ReservedTag, ReservedAddr, IsForeignType),
            % Work out what language's foreign_enum pragma we should be
            % looking at for the current compilation target language.
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, TargetLanguage),
            LangForForeignEnums =
                target_lang_to_foreign_enum_lang(TargetLanguage),
            (
                ( DuTypeKind0 = du_type_kind_direct_dummy
                ; DuTypeKind0 = du_type_kind_mercury_enum
                ),
                get_type_defn_status(TypeDefn0, TypeStatus),
                % Either both the type and the pragma are defined in this
                % module or they are both imported. Any other combination
                % is illegal.
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
                    % XXX We should also check that this type is not
                    % the subject of a reserved tag pragma.
                    DuTypeKind = du_type_kind_foreign_enum(Lang),
                    build_foreign_enum_tag_map(Context, ContextPieces,
                        TypeName, ForeignTagValues, MaybeForeignTagMap,
                        !Specs),
                    ( if
                        LangForForeignEnums = Lang,
                        MaybeForeignTagMap = yes(ForeignTagMap)
                    then
                        map.foldl2(make_foreign_tag(Lang, ForeignTagMap),
                            OldTagValues, map.init, TagValues, [],
                            UnmappedCtors),
                        (
                            UnmappedCtors = [],
                            TypeBody = hlds_du_type(Ctors, TagValues,
                                CheaperTagTest, DuTypeKind, MaybeUserEq,
                                MaybeDirectArgCtors, ReservedTag, ReservedAddr,
                                IsForeignType),
                            set_type_defn_body(TypeBody, TypeDefn0, TypeDefn),
                            replace_type_ctor_defn(TypeCtor, TypeDefn,
                                TypeTable0, TypeTable),
                            module_info_set_type_table(TypeTable, !ModuleInfo)
                        ;
                            UnmappedCtors = [_ | _],
                            add_foreign_enum_unmapped_ctors_error(Context,
                                ContextPieces, UnmappedCtors, !Specs)
                        )
                    else
                        % If there are no matching foreign_enum pragmas for
                        % this target language, then don't do anything.
                        true
                    ),
                    MaybeError = no
                else if
                    PragmaStatus = type_status(status_exported)
                then
                    add_foreign_enum_pragma_in_interface_error(Context,
                        TypeName, TypeArity, !Specs),
                    MaybeError = no
                else
                    ErrorPieces = [words("error: "),
                        qual_sym_name_and_arity(
                            sym_name_arity(TypeName, TypeArity)),
                        words("is not defined in this module.")],
                    MaybeError = yes({severity_error, ErrorPieces})
                )
            ;
                DuTypeKind0 = du_type_kind_foreign_enum(_),
                ( if
                    ( LangForForeignEnums \= Lang
                    ; PragmaStatus = type_status(status_opt_imported)
                    )
                then
                    MaybeError = no
                else
                    % XXX appending ContextPieces (as below) makes this
                    % read awkwardly -- also we should report the location
                    % of the duplicate foreign_enum pragmas.
                    ErrorPieces = [words("error: "),
                        qual_sym_name_and_arity(
                            sym_name_arity(TypeName, TypeArity)),
                        words("has multiple foreign_enum pragmas.")],
                    MaybeError = yes({severity_error, ErrorPieces})
                )
            ;
                ( DuTypeKind0 = du_type_kind_general
                ; DuTypeKind0 = du_type_kind_notag(_, _, _)
                ),
                ErrorPieces = [words("error: "),
                    qual_sym_name_and_arity(
                        sym_name_arity(TypeName, TypeArity)),
                    words("is not an enumeration type."), nl],
                MaybeError = yes({severity_error, ErrorPieces})
            )
        )
    else
        % This else-branch corresponds to an undefined type. We do not
        % issue an error message for it here, since module qualification
        % will have already done so.
        MaybeError = no
    ),
    (
        MaybeError = no
    ;
        MaybeError = yes({Severity, MaybeErrorPieces}),
        Msg = simple_msg(Context, [always(ContextPieces ++ MaybeErrorPieces)]),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred build_foreign_enum_tag_map(prog_context::in, format_components::in,
    sym_name::in, assoc_list(sym_name, string)::in,
    maybe(map(sym_name, string))::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_foreign_enum_tag_map(Context, ContextPieces, TypeName, ForeignTagValues0,
        MaybeForeignTagMap, !Specs) :-
    (
        TypeName = qualified(TypeModuleName, _)
    ;
        TypeName = unqualified(_),
        unexpected($module, $pred,
            "unqualified type name while processing foreign tags.")
    ),
    list.map_foldl(fixup_foreign_tag_val_qualification(TypeModuleName),
        ForeignTagValues0, ForeignTagValues1, [], BadCtors),
    (
        BadCtors = [],
        ( if bimap.from_assoc_list(ForeignTagValues1, ForeignTagValues) then
            ForeignTagMap = ForeignTagValues ^ forward_map,
            MaybeForeignTagMap = yes(ForeignTagMap)
        else
            add_foreign_enum_bijection_error(Context, ContextPieces, !Specs),
            MaybeForeignTagMap = no
        )
    ;
        BadCtors = [_ | _],
        MaybeForeignTagMap = no
    ).

    % The constructor names we get from the parse tree may be unqualified
    % but the ones we match against in the HLDS are not. Module qualify them.
    %
    % XXX module_qual.m should really be doing this rather than add_pragma.m.
    %
:- pred fixup_foreign_tag_val_qualification(module_name::in,
    pair(sym_name, string)::in, pair(sym_name, string)::out,
    list(sym_name)::in, list(sym_name)::out) is det.

fixup_foreign_tag_val_qualification(TypeModuleName, !NamesAndTags,
        !BadCtors) :-
    !.NamesAndTags = CtorSymName0 - ForeignTag,
    (
        CtorSymName0 = unqualified(Name),
        CtorSymName = qualified(TypeModuleName, Name)
    ;
        CtorSymName0 = qualified(CtorModuleName, Name),
        ( if partial_sym_name_matches_full(CtorModuleName, TypeModuleName) then
            CtorSymName = qualified(TypeModuleName, Name)
        else
            !:BadCtors = [CtorSymName0 | !.BadCtors],
            CtorSymName = CtorSymName0
        )
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

:- pred make_foreign_tag(foreign_language::in, map(sym_name, string)::in,
    cons_id::in, cons_tag::in,
    cons_tag_values::in, cons_tag_values::out,
    list(sym_name)::in, list(sym_name)::out) is det.

make_foreign_tag(ForeignLanguage, ForeignTagMap, ConsId, _, !ConsTagValues,
        !UnmappedCtors) :-
    ( if ConsId = cons(ConsSymName0, 0, _) then
        ConsSymName = ConsSymName0
    else
        unexpected($module, $pred, "non arity zero enumeration constant.")
    ),
    ( if map.search(ForeignTagMap, ConsSymName, ForeignTagValue) then
        ForeignTag = foreign_tag(ForeignLanguage, ForeignTagValue),
        map.set(ConsId, ForeignTag, !ConsTagValues)
    else
        !:UnmappedCtors = [ConsSymName | !.UnmappedCtors]
    ).

%-----------------------------------------------------------------------------%

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
    list.cons(Spec, !Specs).

:- func unqual_ctors_to_line_pieces(list(sym_name), list(format_component))
    = list(format_component).

unqual_ctors_to_line_pieces(Ctors, Final) = Pieces :-
    Components = list.map(unqual_ctor_to_format_component, Ctors),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func unqual_ctor_to_format_component(sym_name) = list(format_component).

unqual_ctor_to_format_component(SymName) = [unqual_sym_name(SymName)].

%-----------------------------------------------------------------------------%

:- pred add_foreign_enum_bijection_error(prog_context::in,
    format_components::in, list(error_spec)::in, list(error_spec)::out) is det.

add_foreign_enum_bijection_error(Context, ContextPieces, !Specs) :-
    ErrorPieces = [words("error: "),
        words("the mapping between Mercury enumeration values and"),
        words("foreign values does not form a bijection."), nl],
    Msg = simple_msg(Context, [always(ContextPieces ++ ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

%-----------------------------------------------------------------------------%

:- pred add_foreign_enum_pragma_in_interface_error(prog_context::in,
    sym_name::in, arity::in, list(error_spec)::in, list(error_spec)::out)
    is det.

add_foreign_enum_pragma_in_interface_error(Context, TypeName, TypeArity,
        !Specs) :-
    ErrorPieces = [words("Error: "),
        pragma_decl("foreign_enum"), words("declaration for"),
        qual_sym_name_and_arity(sym_name_arity(TypeName, TypeArity)),
        words("in module interface."), nl ],
    Msg = simple_msg(Context, [always(ErrorPieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    list.cons(Spec, !Specs).

%----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.add_pragma.add_foreign_enum.
%----------------------------------------------------------------------------%
