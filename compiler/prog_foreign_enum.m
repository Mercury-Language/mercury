%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks the correctness of Mercury name to foreign name mappings
% in foreign enum and foreign export enum declarations.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_foreign_enum.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bimap.
:- import_module list.
:- import_module set_tree234.

%---------------------------------------------------------------------------%

:- type for_fe_or_fee
    --->    for_foreign_enum
    ;       for_foreign_export_enum.

    % build_ctor_name_to_foreign_name_map_loop(ForWhat, Context, ContextPieces,
    %   TypeModuleName, ValidCtorNames, MercuryForeignAL, MercuryForeignBiMap,
    %   !Specs):
    %
    % Given MercuryForeignAL, a list of pairs of Mercury and foreign names,
    % check that all the Mercury names are correctly module qualified
    % (i.e. they are in module TypeModuleName), that they name a valid data
    % constructor (i.e. one in ValidCtorNames) and try to construct a bimap
    % (MercuryForeignBiMap) between those two sets of names, using the rules
    % appropriate for either foreign_enum or foreign_export_enum pragmas,
    % as selected by ForWhat.
    %
    % Generate an error message for each Mercury name that is not correctly
    % module qualified, for each name that is not a valid ctor, and for each
    % deviation of MercuryForeignAL from the relevant rules. Use ContextPieces
    % as the first half of each error message, and Context as their context.
    %
:- pred build_ctor_name_to_foreign_name_map(for_fe_or_fee::in,
    prog_context::in, list(format_component)::in, module_name::in,
    set_tree234(string)::in, assoc_list(sym_name, string)::in,
    bimap(string, string)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module cord.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

build_ctor_name_to_foreign_name_map(ForWhat, Context, ContextPieces,
        TypeModuleName, ValidCtorNames, MercuryForeignAL,
        MercuryForeignBiMap, !Specs) :-
    MercuryForeignBiMap0 = bimap.init,
    SeenCtorNames0 = set_tree234.init,
    SeenForeignNames0 = set_tree234.init,
    BadQualCtorSymNamesCord0 = cord.init,
    InvalidCtorSymNamesCord0 = cord.init,
    RepeatedCtorNamesCord0 = cord.init,
    RepeatedForeignNamesCord0 = cord.init,
    build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
        MercuryForeignAL, MercuryForeignBiMap0, MercuryForeignBiMap,
        SeenCtorNames0, SeenCtorNames, SeenForeignNames0,
        BadQualCtorSymNamesCord0, BadQualCtorSymNamesCord,
        InvalidCtorSymNamesCord0, InvalidCtorSymNamesCord,
        RepeatedCtorNamesCord0, RepeatedCtorNamesCord,
        RepeatedForeignNamesCord0, RepeatedForeignNamesCord),

    % Badly qualified data constructor names should have been caught by
    % parse_pragma.m, and should have prevented the construction
    % of the foreign_enum pragma, for at least one of our callers
    % (check_parse_tree_type_defns.m), but maybe not the other
    % (add_foreign_enum.m).
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
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
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

:- pred build_ctor_name_to_foreign_name_map_loop(module_name::in,
    set_tree234(string)::in, assoc_list(sym_name, string)::in,
    bimap(string, string)::in, bimap(string, string)::out,
    set_tree234(string)::in, set_tree234(string)::out, set_tree234(string)::in,
    cord(sym_name)::in, cord(sym_name)::out,
    cord(sym_name)::in, cord(sym_name)::out,
    cord(string)::in, cord(string)::out,
    cord(string)::in, cord(string)::out) is det.

build_ctor_name_to_foreign_name_map_loop(_, _, [], !MercuryForeignBiMap,
        !SeenCtorNames, _SeenForeignNames, !BadQualCtorSymNames,
        !InvalidCtorSymNames, !RepeatedCtorNames, !RepeatedForeignNames).
build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
        [MercuryForeign | MercuryForeignAL], !MercuryForeignBiMap,
        !SeenCtorNames, !.SeenForeignNames, !BadQualCtorSymNames,
        !InvalidCtorSymNames, !RepeatedCtorNames, !RepeatedForeignNames) :-
    MercuryForeign = CtorSymName - ForeignName,
    some [!OK] (
        !:OK = yes,
        (
            CtorSymName = qualified(CtorModuleName, CtorName),
            ( if CtorModuleName = TypeModuleName then
                true
            else
                cord.snoc(CtorSymName, !BadQualCtorSymNames),
                !:OK = no
            )
        ;
            CtorSymName = unqualified(CtorName)
        ),
        ( if set_tree234.contains(ValidCtorNames, CtorName) then
            true
        else
            cord.snoc(CtorSymName, !InvalidCtorSymNames),
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
            cord.snoc(ForeignName, !RepeatedForeignNames),
            !:OK = no
        ),
        (
            !.OK = yes,
            bimap.det_insert(CtorName, ForeignName, !MercuryForeignBiMap)
        ;
            !.OK = no
        )
    ),
    build_ctor_name_to_foreign_name_map_loop(TypeModuleName, ValidCtorNames,
        MercuryForeignAL, !MercuryForeignBiMap,
        !SeenCtorNames, !.SeenForeignNames, !BadQualCtorSymNames,
        !InvalidCtorSymNames, !RepeatedCtorNames, !RepeatedForeignNames).

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
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, ContextPieces ++ ErrorPieces),
    !:Specs = [Spec | !.Specs].

%-----%

:- func qual_ctors_to_line_pieces(list(sym_name), list(format_component))
    = list(format_component).

qual_ctors_to_line_pieces(Ctors, Final) = Pieces :-
    Components = list.map(qual_ctor_to_format_component, Ctors),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func qual_ctor_to_format_component(sym_name) = list(format_component).

qual_ctor_to_format_component(SymName) = [qual_sym_name(SymName)].

%---------------------------------------------------------------------------%

    % add_unknown_ctors_error(Context, ContextPieces, Ctors, !Specs):
    %
    % Given ContextPieces as the first half of an error message that
    % identifies a type, generate a complete error message about Ctors
    % not being constructor(s) of that type.
    %
:- pred add_unknown_ctors_error(prog_context::in, list(format_component)::in,
    list(sym_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

add_unknown_ctors_error(Context, ContextPieces, Ctors, !Specs) :-
    IsOrAre = choose_number(Ctors, "symbol is not a constructor",
        "symbols are not constructors"),
    ErrorPieces = [invis_order_default_start(1),
        words("error: the following"), words(IsOrAre),
        words("of the type:"), nl_indent_delta(2)] ++
        unqual_ctors_to_line_pieces(Ctors, [suffix("."), nl]),
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, ContextPieces ++ ErrorPieces),
    !:Specs = [Spec | !.Specs].

%------%

:- func unqual_ctors_to_line_pieces(list(sym_name), list(format_component))
    = list(format_component).

unqual_ctors_to_line_pieces(Ctors, Final) = Pieces :-
    Components = list.map(unqual_ctor_to_format_component, Ctors),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func unqual_ctor_to_format_component(sym_name) = list(format_component).

unqual_ctor_to_format_component(SymName) = [unqual_sym_name(SymName)].

%---------------------------------------------------------------------------%

    % add_foreign_enum_unmapped_ctors_error(Context, ContextPieces, CtorNames,
    %   !Specs):
    %
    % Given ContextPieces as the first half of an error message that
    % identifies a type, generate a complete error message about CtorNames
    % not being mapped to a foreign language value.
    %
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
    Spec = error_spec($pred, severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%------%

:- func ctor_names_to_line_pieces(list(string), list(format_component))
    = list(format_component).

ctor_names_to_line_pieces(CtorNames, Final) = Pieces :-
    Components = list.map(ctor_name_to_format_component, CtorNames),
    Pieces = component_list_to_line_pieces(Components, Final).

:- func ctor_name_to_format_component(string) = list(format_component).

ctor_name_to_format_component(CtorName) = [quote(CtorName)].

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_foreign_enum.
%---------------------------------------------------------------------------%
