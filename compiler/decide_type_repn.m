%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: decide_type_repn.m.
% Main author: zs.
%
% The task of this module is decide the representation of each type
% and to generate type_representation items recording those decisions
% for inclusion in automatically generated interface files.
%
% We decide the representation of simple types (direct dummy types,
% enum types, and notag types) when generating .int3 files.
% The intention is that we will decide the representation of
% all other types when generating .int/.int2 files.
%
% The task of this module differs from that of the (current) du_type_layout.m
% not just in that it does its work earlier, on the parse tree instead of
% on the HLDS, but also in that the information it puts into interface files
% must be grade-independent.
%
%---------------------------------------------------------------------------%

:- module parse_tree.decide_type_repn.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.

:- type unqual_type_ctor
    --->    unqual_type_ctor(string, arity).

    % Maps each type constructor in the module that has at least one
    % foreign_enum pragma for it to a list, each member of which says
    % "for this language, the functors of the type are implemented
    % by this string in that foreign language".
    %
:- type foreign_enum_map ==
    map(unqual_type_ctor,
        assoc_list(foreign_language, one_or_more(pair(sym_name, string)))).

    % decide_repns_for_simple_types(Globals,
    %   IntTypeDefns, ImpTypeDefns, ForeignEnumTypeCtors, TypeRepnItems):
    %
    % Given the type definitions in the two sections of a module,
    %
    % - figure out which type definitions define exported simple types,
    % - decide their representations, and
    % - generate items recording those decisions.
    %
:- pred decide_repns_for_simple_types(module_name::in,
    list(item_type_defn_info)::in, list(item_type_defn_info)::in,
    foreign_enum_map::in, list(item)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.add_foreign_enum. % XXX TYPE_REPN undesirable dependency

:- import_module bimap.
:- import_module cord.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

decide_repns_for_simple_types(ModuleName, IntTypeDefns, ImpTypeDefns,
        ForeignEnumTypeCtors, Items) :-
    list.foldl3(
        decide_repn_if_simple_du_type(ModuleName, ForeignEnumTypeCtors,
            si_imp),
        ImpTypeDefns, map.init, ImpTypeRepnMap0, map.init, ImpWordAlignedMap,
        [], ImpForeignTypeDefns),
    list.foldl(record_foreign_type,
        ImpForeignTypeDefns, ImpTypeRepnMap0, ImpTypeRepnMap),
    list.foldl3(
        decide_repn_if_simple_du_type(ModuleName, ForeignEnumTypeCtors,
            si_int(ImpTypeRepnMap, ImpWordAlignedMap)),
        IntTypeDefns, map.init, TypeRepnMap0, map.init, WordAlignedMap,
        [], IntForeignTypeDefns),
    list.foldl(record_foreign_type,
        IntForeignTypeDefns, TypeRepnMap0, TypeRepnMap),
    map.foldl(make_type_repn_item(ModuleName), TypeRepnMap,
        cord.init, ItemsCord0),
    map.foldl(maybe_make_word_aligned_item(ModuleName), WordAlignedMap,
        ItemsCord0, ItemsCord),
    Items = cord.list(ItemsCord).

%---------------------%

:- pred make_type_repn_item(module_name::in,
    unqual_type_ctor::in, du_or_foreign_repn::in,
    cord(item)::in, cord(item)::out) is det.

make_type_repn_item(ModuleName, UnqualTypeCtor, Repn, !Items) :-
    UnqualTypeCtor = unqual_type_ctor(TypeName, Arity),
    TypeCtorSymName = qualified(ModuleName, TypeName),
    varset.init(TVarSet0),
    varset.new_vars(Arity, TypeParams, TVarSet0, TVarSet),
    (
        Repn = dofr_du(DuRepn),
        TypeRepn = tcrepn_du(DuRepn)
    ;
        Repn = dofr_maybe_foreign(OoMLangRepns, MaybeDuRepn0),
        (
            MaybeDuRepn0 = no,
            MaybeDuRepn = no
        ;
            MaybeDuRepn0 = yes(DuRepn0),
            (
                DuRepn0 = dur_enum(EnumRepn0),
                OoMLangRepns = one_or_more(HeadLangRepn, TailLangsRepns),
                gather_used_foreign_languages(HeadLangRepn,
                    set.init, UsedLangs1),
                list.foldl(gather_used_foreign_languages, TailLangsRepns,
                    UsedLangs1, UsedLangs),
                EnumRepn0 = enum_repn(EnumFunctors, ForeignEnums0),
                list.filter(is_non_overridden_foreign_enum(UsedLangs),
                    ForeignEnums0, ForeignEnums),
                EnumRepn = enum_repn(EnumFunctors, ForeignEnums),
                DuRepn = dur_enum(EnumRepn),
                MaybeDuRepn = yes(DuRepn)
            ;
                ( DuRepn0 = dur_notag(_)
                ; DuRepn0 = dur_direct_dummy(_)
                ; DuRepn0 = dur_gen(_)
                ),
                MaybeDuRepn = MaybeDuRepn0
            )
        ),
        TypeRepn = tcrepn_maybe_foreign(OoMLangRepns, MaybeDuRepn)
    ),
    TypeRepnInfo = item_type_repn_info(TypeCtorSymName, TypeParams,
        TypeRepn, TVarSet, term.context_init, -1),
    Item = item_type_repn(TypeRepnInfo),
    !:Items = cord.snoc(!.Items, Item).

:- pred gather_used_foreign_languages(
    pair(foreign_language, foreign_type_repn)::in,
    set(foreign_language)::in, set(foreign_language)::out) is det.

gather_used_foreign_languages(Lang - _Repn, !UsedLangs) :-
    set.insert(Lang, !UsedLangs).

:- pred is_non_overridden_foreign_enum(set(foreign_language)::in,
    pair(foreign_language, one_or_more(string))::in) is semidet.

is_non_overridden_foreign_enum(OverriddenLangs, Lang - _ForeignEnums) :-
    not set.contains(OverriddenLangs, Lang).

%---------------------%

:- pred maybe_make_word_aligned_item(module_name::in,
    unqual_type_ctor::in, maybe_word_aligned::in,
    cord(item)::in, cord(item)::out) is det.

maybe_make_word_aligned_item(ModuleName, UnqualTypeCtor, WordAligned,
        !Items) :-
    (
        WordAligned = not_word_aligned
    ;
        WordAligned = word_aligned,
        UnqualTypeCtor = unqual_type_ctor(TypeName, Arity),
        TypeCtorSymName = qualified(ModuleName, TypeName),
        varset.init(TVarSet0),
        varset.new_vars(Arity, TypeParams, TVarSet0, TVarSet),
        TypeRepnInfo = item_type_repn_info(TypeCtorSymName, TypeParams,
            tcrepn_is_word_aligned_ptr, TVarSet, term.context_init, -1),
        Item = item_type_repn(TypeRepnInfo),
        !:Items = cord.snoc(!.Items, Item)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type du_or_foreign_repn
    --->    dofr_du(
                du_repn
            )
    ;       dofr_maybe_foreign(
                one_or_more(pair(foreign_language, foreign_type_repn)),
                maybe(du_repn)
            ).

:- type type_repn_map == map(unqual_type_ctor, du_or_foreign_repn).

:- type maybe_word_aligned
    --->    not_word_aligned
    ;       word_aligned.

:- type word_aligned_map == map(unqual_type_ctor, maybe_word_aligned).

:- type section_info
    --->    si_imp
    ;       si_int(type_repn_map, word_aligned_map).

:- pred decide_repn_if_simple_du_type(module_name::in, foreign_enum_map::in,
    section_info::in, item_type_defn_info::in,
    type_repn_map::in, type_repn_map::out,
    word_aligned_map::in, word_aligned_map::out,
    list(item_type_defn_info)::in, list(item_type_defn_info)::out) is det.

decide_repn_if_simple_du_type(ModuleName, ForeignEnumTypeCtors,
        SectionInfo, TypeDefnInfo, !TypeRepnMap, !WordAlignedMap,
        !DeferredForeignTypeDefns) :-
    TypeDefnInfo = item_type_defn_info(TypeCtorSymName, TypeParams,
        TypeDefn, _TVarSet, _Context, _SeqNum),
    (
        TypeCtorSymName = unqualified(_),
        unexpected($pred, "unqualified TypeCtorSymName")
    ;
        TypeCtorSymName = qualified(_TypeCtorModuleName, TypeCtorName)
        % _TypeCtorModuleName need not be the same as ModuleName.
        % For correctness, it is enough that it *matches* ModuleName,
        % in the sense of partial_sym_name_matches_full.
        % And if it does not match ModuleName, then the module has
        % a compile-time error, and therefore the .int3 file
        % we are contributing to here will have to be rebuilt
        % after that error is fixed.
    ),
    list.length(TypeParams, TypeCtorArity),
    UnqualTypeCtor = unqual_type_ctor(TypeCtorName, TypeCtorArity),
    (
        TypeDefn = parse_tree_du_type(DuDetails),
        % XXX TYPE_REPN We should delete the "where direct_arg is" clause
        % from the Mercury language before we switch over to using this
        % module for anything but testing purposes. If we do not, we will
        % have to handle _MaybeDirectArgs here.
        DuDetails = type_details_du(OoMCtors, MaybeCanonical,
            _MaybeDirectArgs),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        (
            TailCtors = [],
            % The type has exactly one data constructor.
            SingleCtor = HeadCtor,
            SingleCtor = ctor(_Ordinal, MaybeExistConstraints,
                SingleCtorSymName, Args, Arity, _SingleCtorContext),
            ( if
                ctor_is_constant(SingleCtor, SingleCtorName)
            then
                DirectDummyRepn = direct_dummy_repn(SingleCtorName),
                DuRepn = dur_direct_dummy(DirectDummyRepn),
                add_du_repn_to_type_map(UnqualTypeCtor, DuRepn, !TypeRepnMap),
                WordAligned = not_word_aligned
            else if
                MaybeExistConstraints = no_exist_constraints,
                Args = [_],
                Arity = 1,
                MaybeCanonical = canon
            then
                NotagRepn = notag_repn(unqualify_name(SingleCtorSymName)),
                DuRepn = dur_notag(NotagRepn),
                add_du_repn_to_type_map(UnqualTypeCtor, DuRepn, !TypeRepnMap),
                WordAligned = not_word_aligned
            else
                % NOTE We currently do not apply the direct arg optimization
                % to polymorphic argument types.
                % We could let the argument's type to have a set of type params
                % that is a subset of the type params of the containing type,
                % but that would require the runtime system to be able
                % to handle variables in the argument type, during unification
                % and comparison (mercury_unify_compare_body.h),
                % during deconstruction (mercury_ml_expand_body.h),
                % during deep copying (mercury_deep_copy_body.h), and maybe
                % during some other operations.
                ( if TypeCtorArity = 0 then
                    WordAligned = word_aligned
                else
                    WordAligned = not_word_aligned
                )
            )
        ;
            TailCtors = [_ | _],
            % The type has at least two data constructors.
            ( if
                ctor_is_constant(HeadCtor, HeadName),
                ctors_are_all_constants(TailCtors, TailNames)
            then
                ( if
                    map.search(ForeignEnumTypeCtors, UnqualTypeCtor,
                        ForeignTypeCtorEntry)
                then
                    Names = [HeadName | TailNames],
                    set_tree234.list_to_set(Names, NamesSet),
                    list.foldl(
                        build_foreign_langs_name_map(ModuleName,
                            Names, NamesSet),
                        ForeignTypeCtorEntry, map.init, ForeignLangsNamesMap),
                    map.to_assoc_list(ForeignLangsNamesMap, ForeignLangsNames)
                else
                    ForeignLangsNames = []
                ),
                OoMNames = one_or_more(HeadName, TailNames),
                EnumRepn = enum_repn(OoMNames, ForeignLangsNames),
                DuRepn = dur_enum(EnumRepn),
                add_du_repn_to_type_map(UnqualTypeCtor, DuRepn, !TypeRepnMap)
            else
                true
            ),
            WordAligned = not_word_aligned
        ),
        record_type_word_alignment(UnqualTypeCtor, WordAligned,
            !WordAlignedMap)
    ;
        TypeDefn = parse_tree_foreign_type(_ForeignType),
        !:DeferredForeignTypeDefns =
            [TypeDefnInfo | !.DeferredForeignTypeDefns]
    ;
        TypeDefn = parse_tree_abstract_type(_),
        (
            SectionInfo = si_imp
        ;
            SectionInfo = si_int(ImpTypeRepnMap, ImpWordAlignedMap),
            ( if map.search(ImpTypeRepnMap, UnqualTypeCtor, ImpRepn) then
                ( if map.search(!.TypeRepnMap, UnqualTypeCtor, _) then
                    % We have already found a definition of this type
                    % in the interface that let us decide its representation,
                    % so the type is defined at least twice. Keep the
                    % representation we derived from the (first) definition
                    % in the interface.
                    true
                else
                    map.det_insert(UnqualTypeCtor, ImpRepn, !TypeRepnMap)
                )
            else
                % Either the type is not defined in the implementation section,
                % or if there such a definition, it is not simple enough
                % to decide its representation in this pass.
                %
                % If there is a representation for this type in the interface
                % section either, we want to keep that.
                % If there is no representation for this type in the interface
                % section either, we want to keep that absence as well.
                true
            ),
            ( if map.search(ImpWordAlignedMap, UnqualTypeCtor, ImpWA) then
                record_type_word_alignment(UnqualTypeCtor, ImpWA,
                    !WordAlignedMap)
            else
                true
            )
        )
    ;
        ( TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        )
    ).

%---------------------%

:- pred build_foreign_langs_name_map(module_name::in,
    list(string)::in, set_tree234(string)::in,
    pair(foreign_language, one_or_more(pair(sym_name, string)))::in,
    map(foreign_language, one_or_more(string))::in,
    map(foreign_language, one_or_more(string))::out) is det.

build_foreign_langs_name_map(ModuleName, CtorNames, CtorNamesSet,
        Lang - OoMCtorSymNamesForeignNames, !ForeignLangsNamesMap) :-
    ( if map.search(!.ForeignLangsNamesMap, Lang, _) then
        % This is a foreign_enum for a type_ctor/foreign_language combination
        % that we have seen before. This is an error, and the programmer
        % will have to fix it before we can build a meaningful .int3 file.
        % To avoid confusing readers of the .int3 file we are building *now*,
        % we ignore such duplicates.
        true
    else
        CtorSymNamesForeignNames =
            one_or_more_to_list(OoMCtorSymNamesForeignNames),
        SeenCtorNamesSet0 = set_tree234.init,
        SeenForeignNamesSet0 = set_tree234.init,
        BadQualCtorSymNamesCord0 = cord.init,
        InvalidCtorSymNamesCord0 = cord.init,
        RepeatedCtorNamesCord0 = cord.init,
        RepeatedForeignNamesCord0 = cord.init,
        build_ctor_name_to_foreign_name_map_loop(ModuleName, CtorNamesSet,
            CtorSymNamesForeignNames, bimap.init, MercuryForeignBimap,
            SeenCtorNamesSet0, SeenCtorNamesSet, SeenForeignNamesSet0,
            BadQualCtorSymNamesCord0, BadQualCtorSymNamesCord,
            InvalidCtorSymNamesCord0, InvalidCtorSymNamesCord,
            RepeatedCtorNamesCord0, RepeatedCtorNamesCord,
            RepeatedForeignNamesCord0, RepeatedForeignNamesCord),
        ( if
            cord.is_empty(BadQualCtorSymNamesCord),
            cord.is_empty(InvalidCtorSymNamesCord),
            cord.is_empty(RepeatedCtorNamesCord),
            cord.is_empty(RepeatedForeignNamesCord),

            set_tree234.difference(CtorNamesSet, SeenCtorNamesSet,
                UnseenCtorNamesSet),
            set_tree234.is_non_empty(UnseenCtorNamesSet),

            list.map(bimap.forward_search(MercuryForeignBimap),
                CtorNames, ForeignNames),
            list_to_one_or_more(ForeignNames, OoMForeignNames)
        then
            map.det_insert(Lang, OoMForeignNames, !ForeignLangsNamesMap)
        else
            % The foreign_enum pragma has at least one error.
            % Don't include it in the .int3 file we are building.
            true
        )
    ).

%---------------------%

:- pred add_du_repn_to_type_map(unqual_type_ctor::in, du_repn::in,
    type_repn_map::in, type_repn_map::out) is det.

add_du_repn_to_type_map(UnqualTypeCtor, DuRepn, !TypeRepnMap) :-
    ( if map.search(!.TypeRepnMap, UnqualTypeCtor, OldTypeRepn) then
        (
            OldTypeRepn = dofr_du(_)
            % UnqualTypeCtor has more than one du definition in this module.
            % This is an error, and it will be detected and reported when
            % the module is compiled to target code. It does not matter
            % which du definition we put into the .int3 file, so we choose
            % the one we found first.
        ;
            OldTypeRepn =
                dofr_maybe_foreign(OldForeignLangRepns, OldMaybeDuRepn),
            (
                OldMaybeDuRepn = no,
                NewTypeRepn =
                    dofr_maybe_foreign(OldForeignLangRepns, yes(DuRepn)),
                map.det_update(UnqualTypeCtor, NewTypeRepn, !TypeRepnMap)
            ;
                OldMaybeDuRepn = yes(_)
                % This also means that that UnqualTypeCtor has more
                % than one definition in this module.
            )
        )
    else
        NewTypeRepn = dofr_du(DuRepn),
        map.det_insert(UnqualTypeCtor, NewTypeRepn, !TypeRepnMap)
    ).

%---------------------%

    % A type constructor *should* only have one Mercury definition
    % in a module. However, in practice it may have two or more.
    % This is an error the programmer will have to fix. We expect that
    % in the vast majority of the cases, the fix will be choosing
    % one of these definitions. In the hope (though not the expectation)
    % of minimizing the amount of recompilation that has to be done
    % after the fix, we record a type constructor as having guaranteed-
    % to-be-aligned values only if *all* the definitions provide
    % this guarantee.
    %
:- pred record_type_word_alignment(unqual_type_ctor::in,
    maybe_word_aligned::in,
    word_aligned_map::in, word_aligned_map::out) is det.

record_type_word_alignment(UnqualTypeCtor, WordAligned, !WordAlignedMap) :-
    ( if map.search(!.WordAlignedMap, UnqualTypeCtor, OldWordAligned) then
        (
            WordAligned = word_aligned,
            % Whether OldWordAligned is word_aligned or not_word_aligned,
            % we want to keep its value.
            true
        ;
            WordAligned = not_word_aligned,
            % Whether want to record that a type is word aligned
            % only if *all* its definitions are word aligned.
            (
                OldWordAligned = word_aligned,
                map.det_update(UnqualTypeCtor, WordAligned, !WordAlignedMap)
            ;
                OldWordAligned = not_word_aligned
                % No update needed; the map already has the right value.
            )
        )
    else
        map.det_insert(UnqualTypeCtor, WordAligned, !WordAlignedMap)
    ).

%---------------------%

:- pred record_foreign_type(item_type_defn_info::in,
    type_repn_map::in, type_repn_map::out) is det.

record_foreign_type(ForeignTypeDefnInfo, !TypeRepnMap) :-
    ForeignTypeDefnInfo = item_type_defn_info(TypeCtorSymName, TypeParams,
        ForeignTypeDefn, _TVarSet, _Context, _SeqNum),
    TypeCtorName = unqualify_name(TypeCtorSymName),
    UnqualTypeCtor = unqual_type_ctor(TypeCtorName, list.length(TypeParams)),
    (
        ForeignTypeDefn = parse_tree_foreign_type(ForeignDetails),
        ForeignDetails =
            type_details_foreign(LangType, _MaybeCanonical, Assertions),
        ( LangType = c(c_type(TypeName)), Lang = lang_c
        ; LangType = java(java_type(TypeName)), Lang = lang_java
        ; LangType = csharp(csharp_type(TypeName)), Lang = lang_csharp
        ; LangType = erlang(erlang_type), Lang = lang_erlang, TypeName = ""
        ),
        Repn = foreign_type_repn(TypeName, Assertions),
        LangRepn = Lang - Repn,
        ( if map.search(!.TypeRepnMap, UnqualTypeCtor, OldTypeRepn) then
            (
                OldTypeRepn = dofr_du(DuRepn),
                NewForeignLangRepns = one_or_more(LangRepn, []),
                NewTypeRepn =
                    dofr_maybe_foreign(NewForeignLangRepns, yes(DuRepn))
            ;
                OldTypeRepn =
                    dofr_maybe_foreign(OldForeignLangRepns, MaybeDuRepn),
                list.foldl(gather_used_foreign_languages,
                    one_or_more_to_list(OldForeignLangRepns),
                    set.init, OldLangs),
                ( if set.contains(OldLangs, Lang) then
                    % This is the second (or third, or ...) definition
                    % of this type constructor in this language.
                    % This is an error, so the .int3 file we are now
                    % generating will have to be rebuilt after the
                    % programmer fixes the error. To avoid confusing
                    % the readers of the .int3 file we are generating
                    % now, we ignore any duplicate definitions.
                    NewTypeRepn = OldTypeRepn
                else
                    NewForeignLangRepns =
                        one_or_more_cons(LangRepn, OldForeignLangRepns),
                    NewTypeRepn =
                        dofr_maybe_foreign(NewForeignLangRepns, MaybeDuRepn)
                )
            ),
            map.det_update(UnqualTypeCtor, NewTypeRepn, !TypeRepnMap)
        else
            OoMLangRepns = one_or_more(LangRepn, []),
            NewTypeRepn = dofr_maybe_foreign(OoMLangRepns, no),
            map.det_insert(UnqualTypeCtor, NewTypeRepn, !TypeRepnMap)
        )
    ;
        ( ForeignTypeDefn = parse_tree_du_type(_)
        ; ForeignTypeDefn = parse_tree_abstract_type(_)
        ; ForeignTypeDefn = parse_tree_eqv_type(_)
        ; ForeignTypeDefn = parse_tree_solver_type(_)
        ),
        unexpected($pred, "ForeignTypeDefnInfo not foreign")
    ).

%---------------------------------------------------------------------------%
%
% Auxiliary functions and predicates.
%

:- pred ctors_are_all_constants(list(constructor)::in, list(string)::out)
    is semidet.

ctors_are_all_constants([], []).
ctors_are_all_constants([Ctor | Ctors], [Name | Names]) :-
    ctor_is_constant(Ctor, Name),
    ctors_are_all_constants(Ctors, Names).

:- pred ctor_is_constant(constructor::in, string::out) is semidet.

ctor_is_constant(Ctor, Name) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity,
        _Context),
    MaybeExistConstraints = no_exist_constraints,
    Args = [],
    Arity = 0,
    Name = unqualify_name(SymName).

%---------------------------------------------------------------------------%
:- end_module parse_tree.decide_type_repn.
%---------------------------------------------------------------------------%
