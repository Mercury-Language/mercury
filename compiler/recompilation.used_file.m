%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001-2012 University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recompilation.usage.m.
% Main author: stayl.
%
% Write the file recording which imported items were used by a compilation.
%
%---------------------------------------------------------------------------%

:- module recompilation.used_file.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.module_imports.

:- import_module assoc_list.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

    % The resolved_used_items records the possible matches for a program item.
    % It is used by recompilation_check.m to work out whether a new item
    % could cause ambiguity with an item which was used during a compilation.
:- type resolved_used_items
    --->    resolved_used_items(
                rui_type_names      :: simple_item_set,
                rui_type_defns      :: simple_item_set,
                rui_insts           :: simple_item_set,
                rui_modes           :: simple_item_set,
                rui_typeclasses     :: simple_item_set,
                rui_functors        :: resolved_functor_set,
                rui_predicates      :: resolved_pred_or_func_set,
                rui_functions       :: resolved_pred_or_func_set
            ).

:- func init_resolved_used_items = resolved_used_items.

%---------------------%

:- type resolved_item_set(T) == map(string, resolved_item_list(T)).

    % The list is sorted on arity. This is useful because when determining
    % whether there is an ambiguity, we need to test a predicate or function
    % against all used functors with equal or lower arity.
:- type resolved_item_list(T) == assoc_list(arity, resolved_item_map(T)).

:- type resolved_item_map(T) == map(module_qualifier, T).

%---------------------%

    % A resolved_functor_set records all possible matches
    % for each functor application.
:- type resolved_functor_set == resolved_item_set(set(resolved_functor)).
:- type resolved_functor_map == resolved_item_map(set(resolved_functor)).

:- type resolved_functor
    --->    resolved_functor_pred_or_func(
                pred_id,
                module_name,
                pred_or_func,
                arity       % The actual arity of the predicate or function
            )
    ;       resolved_functor_constructor(
                item_name   % type_ctor
            )
    ;       resolved_functor_field(
                item_name,  % type_ctor
                item_name   % cons_id
            ).

%---------------------%

:- type resolved_pred_or_func_set ==
    resolved_item_set(set(pair(pred_id, module_name))).
:- type resolved_pred_or_func_map ==
    resolved_item_map(set(pair(pred_id, module_name))).

%---------------------------------------------------------------------------%

    % This type contains all the information that goes into a .used file.
    %
    % XXX RECOMP At the moment, its form is only distantly related to the form
    % in which this info ends up in the .used file. Future changes should
    % fix that, hopefully to the extent that you can just call io.write
    % (or equivalent) on it to create every part of the .used file after
    % the version number.
    %
:- type used_file_contents
    --->    used_file_contents(
                % The name of the module whose .used file this represents.
                ufc_module_name             :: module_name,

                % Is this module the top module in its source file?
                ufc_maybe_top_module        :: maybe_top_module,

                % XXX Document the remaining fields.
                ufc_module_timestamp_map    :: module_timestamp_map,
                ufc_mi_version_numbers_map  :: module_item_version_numbers_map,
                ufc_resolved_used_items     :: resolved_used_items,
                ufc_used_typeclasses        :: set(item_name),
                ufc_imported_items          :: imported_items,
                ufc_module_instances        :: map(module_name, set(item_name))
            ).

:- type imported_items == map(module_name, module_imported_items).

:- type module_imported_items
    --->    module_imported_items(
                mii_type_names      :: imported_item_set,
                mii_type_defns      :: imported_item_set,
                mii_insts           :: imported_item_set,
                mii_modes           :: imported_item_set,
                mii_typeclasses     :: imported_item_set,
                mii_functors        :: imported_item_set,
                mii_predicates      :: imported_item_set,
                mii_functions       :: imported_item_set
            ).

:- type imported_item_set == set(name_arity).

    % Changes which modify the format of the `.used' files will increment
    % this number. recompilation_check.m should recompile if the version number
    % is out of date.
    %
:- func used_file_version_number = int.

:- pred write_usage_file(module_info::in, used_file_contents::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_out.
:- import_module recompilation.version.

:- import_module bool.
:- import_module list.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

init_resolved_used_items =
    resolved_used_items(map.init, map.init,  map.init, map.init, map.init,
        map.init, map.init, map.init).

%---------------------------------------------------------------------------%

used_file_version_number = 2.

write_usage_file(ModuleInfo, UsedFileContents, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    % XXX We should output to progress stream and error stream,
    % not CurStream.
    io.output_stream(CurStream, !IO),
    maybe_write_string(CurStream, Verbose,
        "% Writing recompilation compilation dependency information\n", !IO),

    module_info_get_name(ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".used")), ModuleName, FileName, !IO),
    io.open_output(FileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        write_usage_file_to_stream(FileStream, UsedFileContents, !IO),
        io.close_output(FileStream, !IO)
    ;
        FileResult = error(IOError),
        io.error_message(IOError, IOErrorMessage),
        io.format(CurStream, "\nError opening `%s' for output: %s.\n",
            [s(FileName), s(IOErrorMessage)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred write_usage_file_to_stream(io.text_output_stream::in,
    used_file_contents::in, io::di, io::uo) is det.

write_usage_file_to_stream(Stream, UsedFileContents, !IO) :-
    UsedFileContents = used_file_contents(ThisModuleName,
        MaybeTopModule, TimestampMap, _ModuleItemVersionNumbersMap,
        ResolvedUsedItems, UsedClasses, ImportedItems, _ModuleInstances),

    io.write_int(Stream, used_file_version_number, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, module_item_version_numbers_version_number, !IO),
    io.write_string(Stream, ".\n\n", !IO),

    map.lookup(TimestampMap, ThisModuleName,
        module_timestamp(_, ThisModuleTimestamp, _)),
    io.write_string(Stream, "(", !IO),
    mercury_output_bracketed_sym_name(ThisModuleName, Stream, !IO),
    io.write_string(Stream, ", "".m"", ", !IO),
    write_version_number(Stream, ThisModuleTimestamp, !IO),
    io.write_string(Stream, ").\n\n", !IO),

    NestedSubModules = get_nested_children_list_of_top_module(MaybeTopModule),
    (
        NestedSubModules = [],
        io.write_string(Stream, "sub_modules.\n\n", !IO)
    ;
        NestedSubModules = [_ | _],
        io.write_string(Stream, "sub_modules(", !IO),
        write_out_list(mercury_output_bracketed_sym_name, ", ",
            NestedSubModules, Stream, !IO),
        io.write_string(Stream, ").\n\n", !IO)
    ),

    ( if ResolvedUsedItems = init_resolved_used_items then
        io.write_string(Stream, "used_items.\n", !IO)
    else
        io.write_string(Stream, "used_items(\n\t", !IO),
        some [!WriteComma] (
            !:WriteComma = no,
            write_simple_item_matches(Stream, type_abstract_item,
                ResolvedUsedItems, !WriteComma, !IO),
            write_simple_item_matches(Stream, type_body_item,
                ResolvedUsedItems, !WriteComma, !IO),
            write_simple_item_matches(Stream, inst_item,
                ResolvedUsedItems, !WriteComma, !IO),
            write_simple_item_matches(Stream, mode_item,
                ResolvedUsedItems, !WriteComma, !IO),
            write_simple_item_matches(Stream, typeclass_item,
                ResolvedUsedItems, !WriteComma, !IO),
            write_pred_or_func_matches(Stream, pf_predicate,
                ResolvedUsedItems, !WriteComma, !IO),
            write_pred_or_func_matches(Stream, pf_function,
                ResolvedUsedItems, !WriteComma, !IO),
            write_functor_matches(Stream, ResolvedUsedItems ^ rui_functors,
                !WriteComma, !IO),
            _ = !.WriteComma
        ),
        io.write_string(Stream, "\n).\n\n", !IO)
    ),

    ( if set.is_empty(UsedClasses) then
        io.write_string(Stream, "used_classes.\n", !IO)
    else
        io.write_string(Stream, "used_classes(", !IO),
        write_out_list(write_classname_and_arity, ", ",
            set.to_sorted_list(UsedClasses), Stream, !IO),
        io.write_string(Stream, ").\n", !IO)
    ),

    map.foldl(
        write_module_name_and_used_items(Stream, UsedFileContents),
        ImportedItems, !IO),
    % recompilation_check.m checks for this item when reading in the `.used'
    % file to make sure the earlier compilation wasn't interrupted in the
    % middle of writing the file.
    io.nl(Stream, !IO),
    io.write_string(Stream, "done.\n", !IO).

:- pred write_module_name_and_used_items(io.text_output_stream::in,
    used_file_contents::in,
    module_name::in, module_imported_items::in, io::di, io::uo) is det.

write_module_name_and_used_items(Stream, UsedFileContents,
        ModuleName, ModuleUsedItems, !IO) :-
    UsedFileContents = used_file_contents(_ThisModuleName,
        _MaybeTopModule, TimestampMap, ModuleItemVersionNumbersMap,
        _ResolvedUsedItems, _UsedClasses, _ImportedItems, ModuleInstances),

    io.nl(Stream, !IO),
    io.write_string(Stream, "(", !IO),
    mercury_output_bracketed_sym_name(ModuleName, Stream, !IO),
    io.write_string(Stream, ", """, !IO),
    map.lookup(TimestampMap, ModuleName,
        module_timestamp(FileKind, ModuleTimestamp, RecompNeedQual)),
    file_kind_to_extension(FileKind, ExtStr, _Ext),
    io.write_string(Stream, ExtStr, !IO),
    io.write_string(Stream, """, ", !IO),
    write_version_number(Stream, ModuleTimestamp, !IO),
    % This must be kept in sync with parse_module_timestamp in
    % recompilation.check.m.
    (
        RecompNeedQual = recomp_avail_src,
        io.write_string(Stream, ", src", !IO)
    ;
        RecompNeedQual = recomp_avail_int_use,
        % We used to output just ", used".
        io.write_string(Stream, ", int_used", !IO)
    ;
        RecompNeedQual = recomp_avail_imp_use,
        io.write_string(Stream, ", imp_used", !IO)
    ;
        RecompNeedQual = recomp_avail_int_import,
        % We used to output nothing.
        io.write_string(Stream, ", int_imported", !IO)
    ;
        RecompNeedQual = recomp_avail_imp_import,
        % We used to output nothing.
        io.write_string(Stream, ", imp_imported", !IO)
    ;
        RecompNeedQual = recomp_avail_int_use_imp_import,
        io.write_string(Stream, ", int_used_imp_imported", !IO)
    ),
    io.write_string(Stream, ")", !IO),
    ( if
        % XXX We don't yet record all uses of items from these modules
        % in polymorphism.m, etc.
        not any_mercury_builtin_module(ModuleName),
        map.search(ModuleItemVersionNumbersMap, ModuleName,
            ModuleItemVersionNumbers)
    then
        % Select out from the version numbers of all items in the imported
        % module the ones which are used.
        ModuleUsedItems = module_imported_items(UsedTypeNames, UsedTypeDefns,
            UsedInsts, UsedModes, UsedClasses, _UsedInstances,
            UsedPreds, UsedFuncs),
        ModuleItemVersionNumbers =
            module_item_version_numbers(TypeNameMap, TypeDefnMap,
                InstMap, ModeMap, ClassMap, InstanceMap, PredMap, FuncMap),
        map.select(TypeNameMap, UsedTypeNames, UsedTypeNameMap),
        map.select(TypeDefnMap, UsedTypeDefns, UsedTypeDefnMap),
        map.select(InstMap, UsedInsts, UsedInstMap),
        map.select(ModeMap, UsedModes, UsedModeMap),
        map.select(ClassMap, UsedClasses, UsedClassMap),
        ( if map.search(ModuleInstances, ModuleName, UsedInstances) then
            map.select(InstanceMap, UsedInstances, UsedInstanceMap)
        else
            map.init(UsedInstanceMap)
        ),
        map.select(PredMap, UsedPreds, UsedPredMap),
        map.select(FuncMap, UsedFuncs, UsedFuncMap),
        UsedModuleItemVersionNumbers =
            module_item_version_numbers(UsedTypeNameMap, UsedTypeDefnMap,
                UsedInstMap, UsedModeMap, UsedClassMap, UsedInstanceMap,
                UsedPredMap, UsedFuncMap),

        io.write_string(Stream, " => ", !IO),
        write_module_item_version_numbers(Stream,
            UsedModuleItemVersionNumbers, !IO),
        io.write_string(Stream, ".\n", !IO)
    else
        % If we don't have version numbers for a module, we just recompile
        % if the interface file's timestamp changes.
        io.write_string(Stream, ".\n", !IO)
    ).

:- pred write_classname_and_arity(item_name::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_classname_and_arity(item_name(ClassName, ClassArity), Stream, !IO) :-
    mercury_output_bracketed_sym_name(ClassName, Stream, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, ClassArity, !IO).

:- pred write_comma_if_needed(io.text_output_stream::in,
    bool::in, bool::out, io::di, io::uo) is det.

write_comma_if_needed(Stream, !WriteComma, !IO) :-
    (
        !.WriteComma = yes,
        io.write_string(Stream, ",\n\t", !IO)
    ;
        !.WriteComma = no
    ),
    !:WriteComma = yes.

:- pred write_simple_item_matches(io.text_output_stream::in,
    item_type::in(simple_item), resolved_used_items::in,
    bool::in, bool::out, io::di, io::uo) is det.

write_simple_item_matches(Stream, ItemType, UsedItems, !WriteComma, !IO) :-
    (
        ItemType = type_abstract_item,
        Ids = UsedItems ^ rui_type_names
    ;
        ItemType = type_body_item,
        Ids = UsedItems ^ rui_type_defns
    ;
        ItemType = inst_item,
        Ids = UsedItems ^ rui_insts
    ;
        ItemType = mode_item,
        Ids = UsedItems ^ rui_modes
    ;
        ItemType = typeclass_item,
        Ids = UsedItems ^ rui_typeclasses
    ),
    ( if map.is_empty(Ids) then
        true
    else
        write_comma_if_needed(Stream, !WriteComma, !IO),
        write_simple_item_matches_2(Stream, ItemType, Ids, !IO)
    ).

:- pred write_simple_item_matches_2(io.text_output_stream::in,
    item_type::in, simple_item_set::in, io::di, io::uo) is det.

write_simple_item_matches_2(Stream, ItemType, ItemSet, !IO) :-
    string_to_item_type(ItemTypeStr, ItemType),
    io.write_string(Stream, ItemTypeStr, !IO),
    io.write_string(Stream, "(\n\t\t", !IO),
    map.to_assoc_list(ItemSet, ItemList),
    write_out_list(write_simple_item_matches_3, ",\n\t\t", ItemList,
        Stream, !IO),
    io.write_string(Stream, "\n\t)", !IO).

:- pred write_simple_item_matches_3(
    pair(name_arity, map(module_qualifier, module_name))::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_simple_item_matches_3(NameArity - Matches, Stream, !IO) :-
    NameArity = name_arity(Name, Arity),
    mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
        unqualified(Name), Stream, !IO),
    io.write_string(Stream, "/", !IO),
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, " - (", !IO),
    map.to_assoc_list(Matches, MatchList),
    write_out_list(write_simple_item_matches_4, ", ", MatchList, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred write_simple_item_matches_4(pair(module_qualifier, module_name)::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_simple_item_matches_4(Qualifier - ModuleName, Stream, !IO) :-
    mercury_output_bracketed_sym_name(Qualifier, Stream, !IO),
    ( if Qualifier = ModuleName then
        true
    else
        io.write_string(Stream, " => ", !IO),
        mercury_output_bracketed_sym_name(ModuleName, Stream, !IO)
    ).

:- pred write_pred_or_func_matches(io.text_output_stream::in,
    pred_or_func::in, resolved_used_items::in,
    bool::in, bool::out, io::di, io::uo) is det.

write_pred_or_func_matches(Stream, PredOrFunc, UsedItems, !WriteComma, !IO) :-
    (
        PredOrFunc = pf_predicate,
        ItemType = predicate_item,
        ItemSet = UsedItems ^ rui_predicates
    ;
        PredOrFunc = pf_function,
        ItemType = function_item,
        ItemSet = UsedItems ^ rui_functions
    ),
    ( if map.is_empty(ItemSet) then
        true
    else
        write_comma_if_needed(Stream, !WriteComma, !IO),
        write_pred_or_func_matches_2(ItemType, ItemSet, Stream, !IO)
    ).

:- pred write_pred_or_func_matches_2(item_type::in,
    resolved_pred_or_func_set::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_pred_or_func_matches_2(ItemType, ItemSet, Stream, !IO) :-
    write_resolved_item_set(ItemType, ItemSet, write_pred_or_func_matches_3,
        Stream, !IO).

:- pred write_pred_or_func_matches_3(
    pair(sym_name, set(pair(pred_id, sym_name)))::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_pred_or_func_matches_3(Qualifier - PredIdModuleNames, Stream, !IO) :-
    ModuleNames = assoc_list.values(set.to_sorted_list(PredIdModuleNames)),
    mercury_output_bracketed_sym_name(Qualifier, Stream, !IO),
    ( if ModuleNames = [Qualifier] then
        true
    else
        io.write_string(Stream, " => (", !IO),
        write_out_list(mercury_output_bracketed_sym_name, ", ", ModuleNames,
            Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred write_functor_matches(io.text_output_stream::in,
    resolved_functor_set::in, bool::in, bool::out, io::di, io::uo) is det.

write_functor_matches(Stream, Ids, !WriteComma, !IO) :-
    ( if map.is_empty(Ids) then
        true
    else
        write_comma_if_needed(Stream, !WriteComma, !IO),
        write_resolved_item_set(functor_item, Ids, write_functor_matches_2,
            Stream, !IO)
    ).

:- pred write_functor_matches_2(pair(sym_name, set(resolved_functor))::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_functor_matches_2(Qualifier - MatchingCtors, Stream, !IO) :-
    mercury_output_bracketed_sym_name(Qualifier, Stream, !IO),
    io.write_string(Stream, " => (", !IO),
    write_out_list(write_resolved_functor, ", ",
        set.to_sorted_list(MatchingCtors), Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- type write_resolved_item(T) ==
    pred(pair(module_qualifier, T), io.text_output_stream, io, io).
:- inst write_resolved_item == (pred(in, in, di, uo) is det).

:- pred write_resolved_item_set(item_type::in, resolved_item_set(T)::in,
    write_resolved_item(T)::in(write_resolved_item),
    io.text_output_stream::in, io::di, io::uo) is det.

write_resolved_item_set(ItemType, ItemSet, WriteMatches, Stream, !IO) :-
    string_to_item_type(ItemTypeStr, ItemType),
    io.write_string(Stream, ItemTypeStr, !IO),
    io.write_string(Stream, "(\n\t\t", !IO),
    map.to_assoc_list(ItemSet, ItemList),
    write_out_list(write_resolved_item_set_2(WriteMatches), ",\n\t\t",
        ItemList, Stream, !IO),
    io.write_string(Stream, "\n\t)", !IO).

:- pred write_resolved_item_set_2(
    write_resolved_item(T)::in(write_resolved_item),
    pair(string, list(pair(int, map(sym_name, T))))::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_resolved_item_set_2(WriteMatches, Name - MatchesAL, Stream, !IO) :-
    mercury_output_bracketed_sym_name(unqualified(Name), Stream, !IO),
    io.write_string(Stream, " - (", !IO),
    write_out_list(write_resolved_item_set_3(WriteMatches), ",\n\t\t\t",
        MatchesAL, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred write_resolved_item_set_3(
    write_resolved_item(T)::in(write_resolved_item),
    pair(int, map(sym_name, T))::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_resolved_item_set_3(WriteMatches, Arity - Matches, Stream, !IO) :-
    io.write_int(Stream, Arity, !IO),
    io.write_string(Stream, " - (", !IO),
    map.to_assoc_list(Matches, MatchList),
    write_out_list(WriteMatches, ",\n\t\t\t\t", MatchList, Stream, !IO),
    io.write_string(Stream, ")", !IO).

:- pred write_resolved_functor(resolved_functor::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_resolved_functor(ResolvedFunctor, Stream, !IO) :-
    (
        ResolvedFunctor = resolved_functor_pred_or_func(_, ModuleName,
            PredOrFunc, Arity),
        io.write_string(Stream, pred_or_func_to_full_str(PredOrFunc), !IO),
        io.write_string(Stream, "(", !IO),
        mercury_output_bracketed_sym_name(ModuleName, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_int(Stream, Arity, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ResolvedFunctor = resolved_functor_constructor(ItemName),
        ItemName = item_name(TypeName, Arity),
        io.write_string(Stream, "ctor(", !IO),
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
            TypeName, Stream, !IO),
        io.write_string(Stream, "/", !IO),
        io.write_int(Stream, Arity, !IO),
        io.write_string(Stream, ")", !IO)
    ;
        ResolvedFunctor = resolved_functor_field(TypeItemName, ConsItemName),
        TypeItemName = item_name(TypeName, TypeArity),
        ConsItemName = item_name(ConsName, ConsArity),
        io.write_string(Stream, "field(", !IO),
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
            TypeName, Stream, !IO),
        io.write_string(Stream, "/", !IO),
        io.write_int(Stream, TypeArity, !IO),
        io.write_string(Stream, ", ", !IO),
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
            ConsName, Stream, !IO),
        io.write_string(Stream, "/", !IO),
        io.write_int(Stream, ConsArity, !IO),
        io.write_string(Stream, ")", !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module recompilation.used_file.
%---------------------------------------------------------------------------%
