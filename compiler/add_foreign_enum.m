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
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module map.
:- import_module maybe.

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
:- pred add_pragma_foreign_enum(module_info::in,
    {item_mercury_status, item_foreign_enum_info}::in,
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
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign_enum.

:- import_module assoc_list.
:- import_module bimap.
:- import_module bool.
:- import_module cord.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set_tree234.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Part 1: the implementation of foreign_enums.
%

add_pragma_foreign_enum(ModuleInfo, ImsItem, !TypeCtorForeignEnumMap,
        Specs0, Specs) :-
    ImsItem = {ItemMercuryStatus, ItemForeignEnum},
    ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor,
        OoMMercuryForeignTagPairs, Context, _SeqNum),
    item_mercury_status_to_type_status(ItemMercuryStatus, PragmaStatus),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    TypeSNA = sym_name_arity(TypeSymName, TypeArity),
    ContextPieces = [words("In"), pragma_decl("foreign_enum"),
        words("declaration for type"), qual_type_ctor(TypeCtor),
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
                unexpected($pred, "unqualified type name for foreign_enum")
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
                % We should have filtered out foreign_enum pragmas
                % in the interface section when we constructed the parse tree;
                % this is just a sanity check.
                unexpected($pred, "foreign_enum in the interface section")
            else
                % As of 2019 Sep 29, this should not happen anymore,
                % since we now catch foreign_enum pragmas that refer
                % to types in other modules when parsing them.
                NotThisModulePieces = ContextPieces ++
                    [words("error: "), qual_sym_name_arity(TypeSNA),
                    words("is not defined in this module."), nl],
                NotThisModuleSpec = simplest_spec($pred, severity_error,
                    phase_parse_tree_to_hlds, Context, NotThisModulePieces),
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
                TypeBody = hlds_du_type(TypeBodyDu),
                TypeBodyDu = type_body_du(Ctors, MaybeSuperType, _MaybeUserEq,
                    MaybeRepn, _IsForeignType),
                expect(unify(MaybeSuperType, not_a_subtype), $pred,
                    "MaybeSuperType != no"),
                expect(unify(MaybeRepn, no), $pred,
                    "MaybeRepn != no"),

                MercuryForeignTagPairs =
                    one_or_more_to_list(OoMMercuryForeignTagPairs),
                build_mercury_foreign_map(TypeModuleName, TypeSymName,
                    TypeArity, for_foreign_enum, Context, ContextPieces,
                    one_or_more_to_list(Ctors),
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
        Specs = !.Specs ++ Specs0
    ).

    % For a given target language work out which language's foreign_enum
    % pragma we should be looking at.
    %
:- func target_lang_to_foreign_enum_lang(compilation_target)
    = foreign_language.

target_lang_to_foreign_enum_lang(target_c) = lang_c.
target_lang_to_foreign_enum_lang(target_csharp) = lang_csharp.
target_lang_to_foreign_enum_lang(target_java) = lang_java.

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
    ItemForeignExportEnum = item_foreign_export_enum_info(Lang, TypeCtor,
        Attributes, Overrides, Context, _SeqNum),
    TypeCtor = type_ctor(TypeSymName, TypeArity),
    ContextPieces = [words("In"), pragma_decl("foreign_export_enum"),
        words("declaration for type"), qual_type_ctor(TypeCtor),
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
                TypeBody = hlds_du_type(TypeBodyDu),
                TypeBodyDu = type_body_du(Ctors, MaybeSuperType, _MaybeUserEq,
                    MaybeRepn, _IsForeignType),
                expect(unify(MaybeSuperType, not_a_subtype), $pred,
                    "MaybeSuperType != no"),
                (
                    MaybeRepn = no,
                    unexpected($pred, "MaybeRepn = no")
                ;
                    MaybeRepn = yes(Repn),
                    CtorRepns = Repn ^ dur_ctor_repns
                ),

                build_mercury_foreign_map(TypeModuleName, TypeSymName,
                    TypeArity, for_foreign_export_enum, Context, ContextPieces,
                    one_or_more_to_list(Ctors),
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

:- pred build_export_enum_name_map(list(format_piece)::in,
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
            Lang = lang_csharp,
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
            component_list_to_line_pieces(BadForeignPiecesList,
                [suffix("."), nl]),
        VerbosePieces = [words("The problematic"),
            words(choose_number(BadForeignNames,
                "foreign name is:", "foreign names are:")),
            nl_indent_delta(2)] ++ BadForeignPieces,
        Msg = simple_msg(Context,
            [always(AlwaysPieces),
            verbose_only(verbose_always, VerbosePieces)]),
        Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
            [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- pred add_ctor_to_name_map(foreign_language::in, string::in,
    uppercase_export_enum::in, map(string, string)::in, constructor_repn::in,
    map(string, string)::in, map(string, string)::out,
    cord(string)::in, cord(string)::out) is det.

add_ctor_to_name_map(_Lang, Prefix, MakeUpperCase, OverrideMap, CtorRepn,
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
    IsValidForeignName = pred_to_bool(is_valid_c_identifier(ForeignName)),
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

:- pred build_mercury_foreign_map(module_name::in, sym_name::in, arity::in,
    for_fe_or_fee::in, prog_context::in, list(format_piece)::in,
    list(constructor)::in,
    assoc_list(sym_name, string)::in, bimap(string, string)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

build_mercury_foreign_map(TypeModuleName, TypeSymName, TypeArity, ForWhat,
        Context, ContextPieces, Ctors, Overrides, OverrideMap, !Specs) :-
    find_nonenum_ctors_build_valid_ctor_names(Ctors,
        set_tree234.init, ValidCtorNames, cord.init, NonEnumSNAsCord),
    ( if cord.is_empty(NonEnumSNAsCord) then
        true
    else
        NotEnumInfo = not_enum_du(cord.to_list(NonEnumSNAsCord)),
        report_not_enum_type(Context, ContextPieces, TypeSymName, TypeArity,
            NotEnumInfo, !Specs)
    ),
    build_ctor_name_to_foreign_name_map(ForWhat, Context, ContextPieces,
        TypeModuleName, ValidCtorNames, Overrides, OverrideMap, !Specs).

:- pred find_nonenum_ctors_build_valid_ctor_names(list(constructor)::in,
    set_tree234(string)::in, set_tree234(string)::out,
    cord(sym_name_arity)::in, cord(sym_name_arity)::out) is det.

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

%---------------------------------------------------------------------------%
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
            qual_sym_name_arity(TypeSymNameArity),
            words("and target language"), fixed(LangStr), suffix("."), nl],
        OldPieces = [words("The first foreign_enum pragma"),
            words("for"), qual_sym_name_arity(TypeSymNameArity),
            words("and"), fixed(LangStr), words("was here."), nl],
        CurMsg = simplest_msg(Context, CurPieces),
        OldMsg = simplest_msg(OldContext, OldPieces),
        Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds,
            [CurMsg, OldMsg]),
        !:Specs = [Spec | !.Specs]
    ).

%---------------------------------------------------------------------------%

:- pred report_if_builtin_type(prog_context::in, list(format_piece)::in,
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
            unqual_sym_name_arity(sym_name_arity(TypeSymame, TypeArity)),
            words("is a builtin type."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

:- type not_enum_info
    --->    not_enum_du(
                % The non-enum sym_names and their (nonzero) arities.
                list(sym_name_arity)
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

:- pred report_not_du_type(prog_context::in, list(format_piece)::in,
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

:- pred report_not_enum_type(prog_context::in, list(format_piece)::in,
    sym_name::in, arity::in, not_enum_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_not_enum_type(Context, ContextPieces, TypeSymName, TypeArity,
        NotEnumInfo, !Specs) :-
    TypeSNA = sym_name_arity(TypeSymName, TypeArity),
    (
        NotEnumInfo = not_enum_non_du(TypeKindDesc),
        Pieces = ContextPieces ++ [words("error: "),
            qual_sym_name_arity(TypeSNA),
            words("is not an enumeration type;"),
            words("it is"), words(TypeKindDesc), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
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
                    list.map(func(SNA) = unqual_sym_name_arity(SNA),
                        OrderedNonEnumSNAs)),
            ItHasThese = choose_number(OrderedNonEnumSNAs,
                words("It has this non-zero arity constructor:"),
                words("It has these non-zero arity constructors:")),
            Pieces = ContextPieces ++ [words("error: "),
                qual_sym_name_arity(TypeSNA),
                words("is not an enumeration type."), nl,
                ItHasThese, nl_indent_delta(2)] ++ SNAPieces ++
                [suffix("."), nl_indent_delta(-2)],
            Spec = simplest_spec($pred, severity_error,
                phase_parse_tree_to_hlds, Context, Pieces),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.add_foreign_enum.
%---------------------------------------------------------------------------%
