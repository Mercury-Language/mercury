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
% Original author: stayl.
%
% Write out .used files, the file recording which imported items were used
% by a compilation, and read them back in.
%
% XXX At the moment, the two halves use different data structures.
% This should be fixed by standardizing on (a variant of) the data structure
% now used by read_used_file_for_module.
%
%---------------------------------------------------------------------------%

:- module recompilation.used_file.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_data.

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
    --->    resolved_functor_pred_or_func(pred_id, pred_or_func,
                module_name, pred_form_arity)
            % The actual arity of the predicate or function
    ;       resolved_functor_data_constructor(type_ctor)
    ;       resolved_functor_field_access_func(cons_ctor).

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
                ufc_used_typeclasses        :: set(recomp_item_name),
                ufc_imported_items          :: imported_items,
                ufc_module_instances        :: map(module_name,
                                                set(recomp_item_name))
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

:- type used_file_result(T)
    --->    used_file_ok(T)
    ;       used_file_error(used_file_error).

:- type used_file_error
    --->    uf_read_error(file_name, io.error)
            % The name of the unreadable file, and the OS's error code.
    ;       uf_syntax_error(prog_context, string)
            % The location of the error, and a short message about the nature
            % of the error.
    ;       uf_invalid_file_format(file_name)
            % The named file does not start with a valid version number
            % for a .used file.
    ;       uf_unreadable_used_items(list(error_spec)).
            % XXX document this

    % This type represents the part of the .used file *after*
    % the version numbers.
:- type used_file
    --->    used_file(
                % The timestamp of the source file of the module
                % whose .used file this is.
                module_timestamp,

                % If this list is non-empty, then this module is the top
                % module in its source file, and the list gives the names
                % of all of its nested submodules.
                %
                % If this list is empty, then *either*
                %
                % - this module is not the top module in its source file, or
                % - this module *is* the top module in its source file,
                %   but it has no nested submodules.
                %
                % You can't tell which of these is the case.
                list(module_name),

                % XXX document the meanings of these fields.
                resolved_used_items,
                list(recomp_item_name),
                list(recomp_used_module)
            ).

:- type recomp_used_module
    --->    recomp_used_module(
                module_name,
                module_timestamp,
                maybe(module_item_version_numbers)
            ).

:- pred read_used_file_for_module(globals::in, module_name::in,
    used_file_result(used_file)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_util.
:- import_module recompilation.version.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module mercury_term_lexer.
:- import_module mercury_term_parser.
:- import_module string.
:- import_module term_context.
:- import_module term_int.
:- import_module unit.

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

    % Term 1.
    io.write_string(Stream, used_file_version_numbers_to_string, !IO),

    % Term 2.
    map.lookup(TimestampMap, ThisModuleName,
        module_timestamp(_, ThisModuleTimestamp, _)),
    io.write_string(Stream,
        module_source_timestamp_to_string(ThisModuleName, ThisModuleTimestamp),
        !IO),

    % Term 3.
    io.write_string(Stream,
        nested_children_of_top_module_to_string(MaybeTopModule), !IO),

    % Term 4.
    io.write_string(Stream, used_items_to_string(ResolvedUsedItems), !IO),

    % Term 5.
    io.write_string(Stream, used_classes_to_string(UsedClasses), !IO),

    % Terms 6+.
    UsedFileStrs = list.map(
        module_name_and_used_items_to_string(UsedFileContents),
        map.to_sorted_assoc_list(ImportedItems)),
    io.write_strings(Stream, UsedFileStrs, !IO),
    % We check for this end-of-list marker when reading in the `.used' file,
    % to make sure the earlier compilation wasn't interrupted in the
    % middle of writing the file.
    io.write_string(Stream, "\ndone.\n", !IO).

%---------------------------------------------------------------------------%

:- func used_file_version_numbers_to_string = string.

used_file_version_numbers_to_string = Str :-
    string.format("%d,%d.\n\n",
        [i(used_file_version_number),
        i(module_item_version_numbers_version_number)], Str).

%---------------------------------------------------------------------------%

:- func module_source_timestamp_to_string(module_name, timestamp) = string.

module_source_timestamp_to_string(ModuleName, Timestamp) = Str :-
    string.format("(%s, "".m"", %s).\n\n",
        [s(mercury_bracketed_sym_name_to_string(ModuleName)),
        s(version_number_to_string(Timestamp))], Str).

%---------------------------------------------------------------------------%

:- func nested_children_of_top_module_to_string(maybe_top_module) = string.

nested_children_of_top_module_to_string(MaybeTopModule) = Str :-
    NestedSubModules = get_nested_children_list_of_top_module(MaybeTopModule),
    (
        NestedSubModules = [],
        Str = "sub_modules.\n\n"
    ;
        NestedSubModules = [_ | _],
        NestedSubModuleStrs =
            list.map(mercury_bracketed_sym_name_to_string, NestedSubModules),
        NestedSubModulesStr = string.join_list(", ", NestedSubModuleStrs),
        string.format("sub_modules(%s).\n\n", [s(NestedSubModulesStr)], Str)
    ).

%---------------------------------------------------------------------------%

:- func used_items_to_string(resolved_used_items) = string.

used_items_to_string(ResolvedUsedItems) = Str :-
    ResolvedUsedItems = resolved_used_items(UsedTypeNameSet, UsedTypeDefnSet,
        UsedInstSet, UsedModeSet, UsedClassSet, UsedFunctorSet,
        UsedPredSet, UsedFuncSet),
    MS1 = maybe_simple_item_matches_to_string(recomp_type_name,
        UsedTypeNameSet),
    MS2 = maybe_simple_item_matches_to_string(recomp_type_defn,
        UsedTypeDefnSet),
    MS3 = maybe_simple_item_matches_to_string(recomp_inst, UsedInstSet),
    MS4 = maybe_simple_item_matches_to_string(recomp_mode, UsedModeSet),
    MS5 = maybe_simple_item_matches_to_string(recomp_typeclass, UsedClassSet),
    MS6 = maybe_pred_or_func_matches_to_string(recomp_predicate, UsedPredSet),
    MS7 = maybe_pred_or_func_matches_to_string(recomp_function, UsedFuncSet),
    % XXX no item_type wrapper
    MS8 = maybe_functor_matches_to_string(UsedFunctorSet),
    MatchStrs = MS1 ++ MS2 ++ MS3 ++ MS4 ++ MS5 ++ MS6 ++ MS7 ++ MS8,
    (
        MatchStrs = [],
        Str = "used_items.\n"
    ;
        MatchStrs = [_ | _],
        MatchesStr = string.join_list(",\n\t", MatchStrs),
        string.format("used_items(\n" ++ "\t%s\n" ++ ").\n\n",
            [s(MatchesStr)], Str)
    ).

%---------------------%

:- func maybe_simple_item_matches_to_string(recomp_item_type, simple_item_set)
    = list(string).

maybe_simple_item_matches_to_string(ItemType, ItemSet) = Strs :-
    ( if map.is_empty(ItemSet) then
        Strs = []
    else
        string_to_recomp_item_type(ItemTypeStr, ItemType),
        map.to_assoc_list(ItemSet, ItemList),
        ItemStrs = list.map(simple_item_matches_to_string_3, ItemList),
        ItemsStr = string.join_list(",\n\t\t", ItemStrs),
        string.format("%s(\n" ++ "\t\t%s\n" ++ "\t)",
            [s(ItemTypeStr), s(ItemsStr)], Str),
        Strs = [Str]
    ).

:- func simple_item_matches_to_string_3(
    pair(name_arity, map(module_qualifier, module_name))) = string.

simple_item_matches_to_string_3(NameArity - Matches) = Str :-
    NameArity = name_arity(Name, Arity),
    map.to_assoc_list(Matches, MatchList),
    MatchStrs =
        list.map(simple_item_matches_to_string_4, MatchList),
    MatchesStr = string.join_list(", ", MatchStrs),
    NGT = next_to_graphic_token,
    string.format("%s/%d - (%s)",
        [s(mercury_bracketed_atom_to_string(NGT, Name)), i(Arity),
        s(MatchesStr)], Str).

:- func simple_item_matches_to_string_4(pair(module_qualifier, module_name))
    = string.

simple_item_matches_to_string_4(Qualifier - ModuleName) = Str :-
    QualifierStr = mercury_bracketed_sym_name_to_string(Qualifier),
    ( if Qualifier = ModuleName then
        Str = QualifierStr
    else
        ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
        string.format("%s => %s", [s(QualifierStr), s(ModuleNameStr)], Str)
    ).

%---------------------%

:- func maybe_pred_or_func_matches_to_string(recomp_item_type,
    resolved_pred_or_func_set) = list(string).

maybe_pred_or_func_matches_to_string(ItemType, ItemSet) = Strs :-
    ( if map.is_empty(ItemSet) then
        Strs = []
    else
        Str = resolved_item_set_to_string(pred_or_func_matches_to_string_3,
            ItemType, ItemSet),
        Strs = [Str]
    ).

:- func pred_or_func_matches_to_string_3(
    pair(sym_name, set(pair(pred_id, sym_name))))
    = string.

pred_or_func_matches_to_string_3(Qualifier - PredIdModuleNames) = Str :-
    ModuleNames = assoc_list.values(set.to_sorted_list(PredIdModuleNames)),
    QualifierStr = mercury_bracketed_sym_name_to_string(Qualifier),
    ( if ModuleNames = [Qualifier] then
        Str = QualifierStr
    else
        ModuleNameStrs =
            list.map(mercury_bracketed_sym_name_to_string, ModuleNames),
        ModuleNamesStr = string.join_list(", ", ModuleNameStrs),
        string.format("%s => (%s)", [s(QualifierStr), s(ModuleNamesStr)], Str)
    ).

%---------------------%

:- func maybe_functor_matches_to_string(resolved_functor_set) = list(string).

maybe_functor_matches_to_string(ItemSet) = Strs :-
    ( if map.is_empty(ItemSet) then
        Strs = []
    else
        MatchesStr = resolved_item_set_to_string(functor_matches_to_string_2,
            recomp_functor, ItemSet),
        Strs = [MatchesStr]
    ).

:- func functor_matches_to_string_2(pair(sym_name, set(resolved_functor)))
    = string.

functor_matches_to_string_2(Qualifier - MatchingCtors) = Str :-
    MatchingCtorList = set.to_sorted_list(MatchingCtors),
    MatchingCtorStrs = list.map(resolved_functor_to_string, MatchingCtorList),
    MatchingCtorsStr = string.join_list(", ", MatchingCtorStrs),
    string.format("%s => (%s)",
        [s(mercury_bracketed_sym_name_to_string(Qualifier)),
        s(MatchingCtorsStr)], Str).

:- func resolved_functor_to_string(resolved_functor) = string.

resolved_functor_to_string(ResolvedFunctor) = Str :-
    (
        ResolvedFunctor = resolved_functor_pred_or_func(_, PredOrFunc,
            ModuleName, pred_form_arity(Arity)),
        string.format("%s(%s, %i)",
            [s(pred_or_func_to_full_str(PredOrFunc)),
            s(mercury_bracketed_sym_name_to_string(ModuleName)),
            i(Arity)], Str)
    ;
        ResolvedFunctor = resolved_functor_data_constructor(TypeCtor),
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        NGT = next_to_graphic_token,
        string.format("ctor(%s/%i)",
            [s(mercury_bracketed_sym_name_to_string_ngt(NGT, TypeCtorSymName)),
            i(TypeCtorArity)], Str)
    ;
        ResolvedFunctor = resolved_functor_field_access_func(ConsCtor),
        ConsCtor = cons_ctor(ConsSymName, ConsArity, TypeCtor),
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        NGT = next_to_graphic_token,
        string.format("field(%s/%i, %s/%i)",
            [s(mercury_bracketed_sym_name_to_string_ngt(NGT, TypeCtorSymName)),
            i(TypeCtorArity),
            s(mercury_bracketed_sym_name_to_string_ngt(NGT, ConsSymName)),
            i(ConsArity)], Str)
    ).

%---------------------%

:- func resolved_item_set_to_string(
    (func(pair(module_qualifier, T)) = string),
    recomp_item_type, resolved_item_set(T)) = string.

resolved_item_set_to_string(MatchesToStr, ItemType, ItemSet) = Str :-
    string_to_recomp_item_type(ItemTypeStr, ItemType),
    map.to_assoc_list(ItemSet, ItemList),
    ItemStrs = list.map(resolved_item_set_to_string_2(MatchesToStr), ItemList),
    ItemsStr = string.join_list(",\n\t\t", ItemStrs),
    string.format("%s(\n" ++ "\t\t%s\n" ++ "\t)",
        [s(ItemTypeStr), s(ItemsStr)], Str).

:- func resolved_item_set_to_string_2(
    (func(pair(module_qualifier, T)) = string),
    pair(string, list(pair(int, map(sym_name, T))))) = string.

resolved_item_set_to_string_2(MatchesToStr, Name - MatchesAL) = Str :-
    NGT = not_next_to_graphic_token,
    NameStr = mercury_bracketed_atom_to_string(NGT, Name),
    MatchStrs = list.map(resolved_item_set_to_string_3(MatchesToStr),
        MatchesAL),
    MatchesStr = string.join_list(",\n\t\t\t", MatchStrs),
    string.format("%s - (%s)", [s(NameStr), s(MatchesStr)], Str).

:- func resolved_item_set_to_string_3(
    (func(pair(module_qualifier, T)) = string),
    pair(int, map(sym_name, T))) = string.

resolved_item_set_to_string_3(MatchesToStr, Arity - Matches) = Str :-
    map.to_assoc_list(Matches, MatchList),
    MatchStrs : list(string) = list.map(MatchesToStr, MatchList),
    MatchesStr = string.join_list(",\n\t\t\t\t", MatchStrs),
    string.format("%d - (%s)", [i(Arity), s(MatchesStr)], Str).

%---------------------------------------------------------------------------%

:- func used_classes_to_string(set(recomp_item_name)) = string.

used_classes_to_string(UsedClasses) = Str :-
    UsedClassList = set.to_sorted_list(UsedClasses),
    (
        UsedClassList = [],
        Str = "used_classes.\n"
    ;
        UsedClassList = [_ | _],
        UsedClassStrs =
            list.map(used_classname_and_arity_to_string, UsedClassList),
        UsedClassesStr = string.join_list(", ", UsedClassStrs),
        string.format("used_classes(%s).\n\n", [s(UsedClassesStr)], Str)
    ).

:- func used_classname_and_arity_to_string(recomp_item_name) = string.

used_classname_and_arity_to_string(ItemName) = Str :-
    ItemName = recomp_item_name(ClassName, ClassArity),
    ClassNameStr = mercury_bracketed_sym_name_to_string(ClassName),
    string.format("%s/%i", [s(ClassNameStr), i(ClassArity)], Str).

%---------------------------------------------------------------------------%

:- func module_name_and_used_items_to_string(used_file_contents,
    pair(module_name, module_imported_items)) = string.

module_name_and_used_items_to_string(UsedFileContents,
        ModuleName - ModuleUsedItems) = Str :-
    UsedFileContents = used_file_contents(_ThisModuleName,
        _MaybeTopModule, TimestampMap, ModuleItemVersionNumbersMap,
        _ResolvedUsedItems, _UsedClasses, _ImportedItems, ModuleInstances),
    map.lookup(TimestampMap, ModuleName,
        module_timestamp(FileKind, ModuleTimestamp, RecompNeedQual)),
    file_kind_to_extension(FileKind, ExtStr, _Ext),
    (
        RecompNeedQual = recomp_avail_src,
        ImportStr = "src"
    ;
        RecompNeedQual = recomp_avail_int_use,
        % We used to output just ", used".
        ImportStr = "int_used"
    ;
        RecompNeedQual = recomp_avail_imp_use,
        ImportStr = "imp_used"
    ;
        RecompNeedQual = recomp_avail_int_import,
        % We used to output nothing.
        ImportStr = "int_imported"
    ;
        RecompNeedQual = recomp_avail_imp_import,
        % We used to output nothing.
        ImportStr = "imp_imported"
    ;
        RecompNeedQual = recomp_avail_int_use_imp_import,
        ImportStr = "int_used_imp_imported"
    ),
    string.format("\n(%s, ""%s"", %s, %s)",
        [s(mercury_bracketed_sym_name_to_string(ModuleName)), s(ExtStr),
        s(version_number_to_string(ModuleTimestamp)), s(ImportStr)], HdrStr),
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
        VNsStr = module_item_version_numbers_to_string(
            UsedModuleItemVersionNumbers),
        string.format("%s => %s.\n", [s(HdrStr), s(VNsStr)], Str)
    else
        % If we don't have version numbers for a module, we just recompile
        % if the interface file's timestamp changes.
        string.format("%s.\n", [s(HdrStr)], Str)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

read_used_file_for_module(Globals, ModuleName, ReadUsedFileResult, !IO) :-
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(other_ext(".used")), ModuleName, UsedFileName, !IO),
    io.read_named_file_as_string(UsedFileName, MaybeUsedFileString, !IO),
    (
        MaybeUsedFileString = ok(UsedFileString),
        string.length(UsedFileString, MaxOffset),
        LineContext0 = line_context(1, 0),
        LinePosn0 = line_posn(0),
        read_and_parse_used_file(UsedFileName, UsedFileString, MaxOffset,
            LineContext0, LinePosn0, ReadUsedFileResult)
    ;
        MaybeUsedFileString = error(IOError),
        Error = uf_read_error(UsedFileName, IOError),
        ReadUsedFileResult = used_file_error(Error)
    ).

:- pred read_and_parse_used_file(string::in, string::in, int::in,
    line_context::in, line_posn::in, used_file_result(used_file)::out) is det.

read_and_parse_used_file(UsedFileName, UsedFileString, MaxOffset,
        !.LineContext, !.LinePosn, ParseUsedFile) :-
    % XXX
    % The contents of the *entire .used file* should be a simple large term
    % of a type that is designed to represent its entire contents, since
    % that would allow a single call to io.read to read it all in, allowing us
    % to dispense with pretty much all of the code handling syntax errors
    % in this module. (Since the contents of .used files are machine generated,
    % we do not need to strive to generate user-friendly error messages;
    % a simple indication of the error's presence and location will do.)
    %
    % That type, used_file, should depend *only* on public type definitions,
    % unlike e.g. the representation of sets, which is hidden behind
    % an abstraction barrier. This is because we don't want changes hidden by
    % those abstraction barriers to affect the file format.
    %
    % We could then handle transitions between file format versions either
    %
    % - by trying the parse the contents of the .used file first as a member
    %   of the new type, and if that failed, as a member of the old type, or
    %
    % - by having both formats represented by two different function symbols
    %   in the same type.
    %
    % Alternatively, the .used file could contain two terms, the version
    % number info, and everything else. We could then select the predicate
    % we use to read in everything else based on the version number.

    % Check that the format of the usage file is the current format.
    read_and_parse_used_file_version_number(UsedFileName, UsedFileString,
        MaxOffset, !LineContext, !LinePosn, ParseVersionNumber),
    (
        ParseVersionNumber = used_file_error(Error),
        ParseUsedFile = used_file_error(Error)
    ;
        ParseVersionNumber = used_file_ok(_Unit),

        % Find the timestamp of the module the last time it was compiled.
        read_and_parse_module_timestamp(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseTimestamp),
        % Find out whether this module has any inline submodules.
        read_and_parse_inline_submodules(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseInlineSubModules),
        % Parse the used items, which are used for checking for ambiguities
        % with new items.
        read_and_parse_used_items(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseUsedItems),
        read_and_parse_used_classes(UsedFileName, UsedFileString,
            MaxOffset, !LineContext, !LinePosn, ParseUsedClasses),
        read_and_parse_used_modules(UsedFileName, UsedFileString,
            MaxOffset, !.LineContext, _, !.LinePosn, _,
            cord.init, ParseUsedModules),
        ( if
            ParseTimestamp = used_file_ok(ModuleNameTimestamp),
            ParseInlineSubModules = used_file_ok(InlineSubModules),
            ParseUsedItems = used_file_ok(UsedItems),
            ParseUsedClasses = used_file_ok(UsedClasses),
            ParseUsedModules = used_file_ok(UsedModules)
        then
            ModuleNameTimestamp =
                module_name_and_timestamp(_ModuleName, ModuleTimestamp),
            UsedFile = used_file(ModuleTimestamp, InlineSubModules,
                UsedItems, set.to_sorted_list(UsedClasses), UsedModules),
            ParseUsedFile = used_file_ok(UsedFile)
        else
            Errors1 = project_error_reason(ParseTimestamp),
            Errors2 = project_error_reason(ParseInlineSubModules),
            Errors3 = project_error_reason(ParseUsedItems),
            Errors4 = project_error_reason(ParseUsedClasses),
            Errors5 = project_error_reason(ParseUsedModules),
            % Since at least one used_file_ok test in the condition has failed,
            % there must be at least one reason.
            % XXX We could report all the syntax errors we found,
            % not just the first.
            FirstError = list.det_head(Errors1 ++ Errors2 ++
                Errors3 ++ Errors4 ++ Errors5),
            ParseUsedFile = used_file_error(FirstError)
        )
    ).

:- func project_error_reason(used_file_result(T)) = list(used_file_error).

project_error_reason(used_file_ok(_)) = [].
project_error_reason(used_file_error(Error)) = [Error].

:- pred read_and_parse_used_file_version_number(string::in,
    string::in, int::in, line_context::in, line_context::out,
    line_posn::in, line_posn::out, used_file_result(unit)::out) is det.

read_and_parse_used_file_version_number(UsedFileName, UsedFileString,
        MaxOffset, !LineContext, !LinePosn, ParseTerm) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "usage file version number", ReadTerm),
    (
        ReadTerm = used_file_error(Error),
        ParseTerm = used_file_error(Error)
    ;
        ReadTerm = used_file_ok(Term),
        ( if
            % XXX ITEM_LIST This term should be more self-descriptive.
            % Instead of the current "2,1.", it should be something like
            % "mercury_smart_recomp_usage(usage_format(2), version_format(1))".
            % We could initially accept both formats when reading in,
            % while generating the new format only.
            Term = term.functor(term.atom(","), [SubTerm1, SubTerm2], _),
            term_int.decimal_term_to_int(SubTerm1, used_file_version_number),
            term_int.decimal_term_to_int(SubTerm2,
                module_item_version_numbers_version_number)
        then
            ParseTerm = used_file_ok(unit)
        else
            Error = uf_invalid_file_format(UsedFileName),
            ParseTerm = used_file_error(Error)
        )
    ).

%---------------------%

:- type module_name_and_timestamp
    --->    module_name_and_timestamp(module_name, module_timestamp).

:- pred read_and_parse_module_timestamp(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    used_file_result(module_name_and_timestamp)::out) is det.

read_and_parse_module_timestamp(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseTerm) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "module timestamp", ReadTerm),
    (
        ReadTerm = used_file_error(Error),
        ParseTerm = used_file_error(Error)
    ;
        ReadTerm = used_file_ok(Term),
        parse_module_timestamp(Term, ParseTerm)
    ).

:- pred parse_module_timestamp(term::in,
    used_file_result(module_name_and_timestamp)::out) is det.

parse_module_timestamp(Term, ParseTerm) :-
    conjunction_to_list(Term, Args),
    ( if
        Args = [ModuleNameTerm, SuffixTerm, TimestampTerm | MaybeOtherTerms],
        try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName),
        SuffixTerm = term.functor(term.string(SuffixStr), [], _),
        extension_to_file_kind(SuffixStr, FileKind),
        parse_timestamp_term(TimestampTerm, Timestamp),
        % This must be kept in sync with write_module_name_and_used_items
        % in recompilation.usage.m.
        (
            MaybeOtherTerms = [],
            RecompAvail = recomp_avail_int_import
        ;
            MaybeOtherTerms = [term.functor(term.atom(Other), [], _)],
            (
                Other = "src",
                RecompAvail = recomp_avail_src
            ;
                % XXX Does write_module_name_and_used_items
                % still generate "used"?
                ( Other = "used"
                ; Other = "int_used"
                ),
                RecompAvail = recomp_avail_int_use
            ;
                Other = "imp_used",
                RecompAvail = recomp_avail_imp_use
            ;
                Other = "int_imported",
                RecompAvail = recomp_avail_int_import
            ;
                Other = "imp_imported",
                RecompAvail = recomp_avail_imp_import
            ;
                Other = "int_used_imp_imported",
                RecompAvail = recomp_avail_int_use_imp_import
            )
        )
    then
        ModuleTimestamp = module_timestamp(FileKind, Timestamp, RecompAvail),
        NameTimestamp = module_name_and_timestamp(ModuleName, ModuleTimestamp),
        ParseTerm = used_file_ok(NameTimestamp)
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in module timestamp"),
        ParseTerm = used_file_error(Error)
    ).

%---------------------%

:- pred read_and_parse_inline_submodules(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    used_file_result(list(module_name))::out) is det.

read_and_parse_inline_submodules(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseSubModules) :-
    % Find out whether this module has any inline submodules.
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "inline submodules", ReadTerm),
    (
        ReadTerm = used_file_error(Error),
        ParseSubModules = used_file_error(Error)
    ;
        ReadTerm = used_file_ok(Term),
        ( if
            Term = term.functor(term.atom("sub_modules"), SubModuleTerms, _),
            list.map(try_parse_sym_name_and_no_args,
                SubModuleTerms, SubModules)
        then
            ParseSubModules = used_file_ok(SubModules)
        else
            Context = get_term_context(Term),
            Error = uf_syntax_error(Context, "error in sub_modules term"),
            ParseSubModules = used_file_error(Error)
        )
    ).

%---------------------%

:- pred read_and_parse_used_items(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    used_file_result(resolved_used_items)::out) is det.

read_and_parse_used_items(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseUsedItems) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "used items", ReadTerm),
    (
        ReadTerm = used_file_error(Error),
        ParseUsedItems = used_file_error(Error)
    ;
        ReadTerm = used_file_ok(Term),
        ( if
            Term = term.functor(term.atom("used_items"), UsedItemTerms, _)
        then
            UsedItems0 = init_resolved_used_items,
            ErrorsCord0 = cord.init,
            list.foldl2(parse_used_item_set, UsedItemTerms,
                UsedItems0, UsedItems, ErrorsCord0, ErrorsCord),
            Errors = cord.list(ErrorsCord),
            (
                Errors = [],
                ParseUsedItems = used_file_ok(UsedItems)
            ;
                Errors = [HeadError | _],
                ParseUsedItems = used_file_error(HeadError)
            )
        else
            Context = get_term_context(Term),
            Error = uf_syntax_error(Context, "error in used items"),
            ParseUsedItems = used_file_error(Error)
        )
    ).

:- pred parse_used_item_set(term::in,
    resolved_used_items::in, resolved_used_items::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_used_item_set(Term, !UsedItems, !Errors) :-
    ( if
        Term = term.functor(term.atom(ItemTypeStr), ItemTerms, _),
        string_to_recomp_item_type(ItemTypeStr, ItemType)
    then
        (
            ( ItemType = recomp_type_name
            ; ItemType = recomp_type_defn
            ; ItemType = recomp_inst
            ; ItemType = recomp_mode
            ; ItemType = recomp_typeclass
            ),
            list.foldl2(parse_simple_item, ItemTerms,
                map.init, SimpleItems, cord.init, ItemErrors),
            ( if cord.is_empty(ItemErrors) then
                (
                    ItemType = recomp_type_name,
                    !UsedItems ^ rui_type_names := SimpleItems
                ;
                    ItemType = recomp_type_defn,
                    !UsedItems ^ rui_type_defns := SimpleItems
                ;
                    ItemType = recomp_inst,
                    !UsedItems ^ rui_insts := SimpleItems
                ;
                    ItemType = recomp_mode,
                    !UsedItems ^ rui_modes := SimpleItems
                ;
                    ItemType = recomp_typeclass,
                    !UsedItems ^ rui_typeclasses := SimpleItems
                )
            else
                !:Errors = !.Errors ++ ItemErrors
            )
        ;
            ( ItemType = recomp_predicate
            ; ItemType = recomp_function
            ),
            list.foldl2(parse_pred_or_func_item, ItemTerms,
                map.init, PredOrFuncItems, cord.init, ItemErrors),
            ( if cord.is_empty(ItemErrors) then
                (
                    ItemType = recomp_predicate,
                    !UsedItems ^ rui_predicates := PredOrFuncItems
                ;
                    ItemType = recomp_function,
                    !UsedItems ^ rui_functions := PredOrFuncItems
                )
            else
                !:Errors = !.Errors ++ ItemErrors
            )
        ;
            ItemType = recomp_functor,
            list.foldl2(parse_functor_item, ItemTerms,
                map.init, CtorItems, cord.init, ItemErrors),
            ( if cord.is_empty(ItemErrors) then
                !UsedItems ^ rui_functors := CtorItems
            else
                !:Errors = !.Errors ++ ItemErrors
            )
        ;
            ( ItemType = recomp_mutable
            ; ItemType = recomp_foreign_proc
            ),
            Context = get_term_context(Term),
            Msg = "error in used items: unknown item type: " ++ ItemTypeStr,
            Error = uf_syntax_error(Context, Msg),
            !:Errors = cord.snoc(!.Errors, Error)
        )
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in used items"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

:- pred parse_simple_item(term::in, simple_item_set::in, simple_item_set::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_simple_item(Term, !Set, !Errors) :-
    ( if
        Term = term.functor(term.atom("-"), [NameArityTerm, MatchesTerm], _),
        parse_sym_name_and_arity(NameArityTerm, SymName, Arity)
    then
        Name = unqualify_name(SymName),
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.foldl2(parse_simple_item_match, MatchTermList,
            map.init, Matches, cord.init, TermErrors),
        ( if cord.is_empty(TermErrors) then
            map.det_insert(name_arity(Name, Arity), Matches, !Set)
        else
            !:Errors = !.Errors ++ TermErrors
        )
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in simple items"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

:- pred parse_simple_item_match(term::in,
    map(module_qualifier, module_name)::in,
    map(module_qualifier, module_name)::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_simple_item_match(Term, !ItemMap, !Errors) :-
    ( if
        % XXX This defaulty representation (the absence of an arrow
        % meaning there is no explicit qualifier term) is bad design.
        % There should *always* be an arrow (or some other fixed functor),
        % with an explicit representation of the qualifier being the same
        % as the module name.
        ( if
            Term = term.functor(term.atom("=>"),
                [QualifierTerm, ModuleNameTerm], _)
        then
            try_parse_sym_name_and_no_args(QualifierTerm, Qualifier),
            try_parse_sym_name_and_no_args(ModuleNameTerm, ModuleName)
        else
            try_parse_sym_name_and_no_args(Term, ModuleName),
            Qualifier = ModuleName
        )
    then
        map.det_insert(Qualifier, ModuleName, !ItemMap)
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in simple item match"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

:- pred parse_pred_or_func_item(term::in,
    resolved_pred_or_func_set::in, resolved_pred_or_func_set::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_pred_or_func_item(Term, !Set, !Errors) :-
    parse_resolved_item_set(parse_pred_or_func_item_match, Term,
        !Set, !Errors).

:- pred parse_pred_or_func_item_match(term::in,
    resolved_pred_or_func_map::in, resolved_pred_or_func_map::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_pred_or_func_item_match(Term, !Items, !Errors) :-
    InvPredId = invalid_pred_id,
    ( if
        ( if
            Term = term.functor(term.atom("=>"),
                [QualifierTerm, MatchesTerm], _)
        then
            try_parse_sym_name_and_no_args(QualifierTerm, Qualifier),
            conjunction_to_list(MatchesTerm, MatchesList),
            list.map(
                ( pred(MatchTerm::in, Match::out) is semidet :-
                    try_parse_sym_name_and_no_args(MatchTerm, MatchName),
                    Match = InvPredId - MatchName
                ),
                MatchesList, Matches)
        else
            try_parse_sym_name_and_no_args(Term, Qualifier),
            Matches = [InvPredId - Qualifier]
        )
    then
        map.det_insert(Qualifier, set.list_to_set(Matches), !Items)
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in pred or func match"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

:- pred parse_functor_item(term::in,
    resolved_functor_set::in, resolved_functor_set::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_functor_item(Term, !Set, !Errors) :-
    parse_resolved_item_set(parse_functor_matches, Term, !Set, !Errors).

:- pred parse_functor_matches(term::in,
    resolved_functor_map::in, resolved_functor_map::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_functor_matches(Term, !Map, !Errors) :-
    ( if
        Term = term.functor(term.atom("=>"), [QualifierTerm, MatchesTerm], _),
        try_parse_sym_name_and_no_args(QualifierTerm, Qualifier)
    then
        conjunction_to_list(MatchesTerm, MatchesTerms),
        list.foldl2(parse_resolved_functor, MatchesTerms,
            [], RevMatches, cord.init, TermErrors),
        ( if cord.is_empty(TermErrors) then
            list.reverse(RevMatches, Matches),
            map.det_insert(Qualifier, set.list_to_set(Matches), !Map)
        else
            !:Errors = !.Errors ++ TermErrors
        )
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in functor match"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

:- pred parse_resolved_functor(term::in,
    list(resolved_functor)::in, list(resolved_functor)::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_resolved_functor(Term, !RevCtors, !Errors) :-
    ( if
        Term = term.functor(term.atom(Atom), ArgTerms, Context),
        ( Atom = "predicate"
        ; Atom = "function"
        ; Atom = "ctor"
        ; Atom = "field"
        )
    then
        (
            ( Atom = "predicate", PredOrFunc = pf_predicate
            ; Atom = "function",  PredOrFunc = pf_function
            ),
            ( if
                ArgTerms = [ModuleTerm, ArityTerm],
                try_parse_sym_name_and_no_args(ModuleTerm, ModuleName),
                term_int.decimal_term_to_int(ArityTerm, Arity)
            then
                InvPredId = invalid_pred_id,
                Ctor = resolved_functor_pred_or_func(InvPredId, PredOrFunc,
                    ModuleName, pred_form_arity(Arity)),
                !:RevCtors = [Ctor | !.RevCtors]
            else
                Error = uf_syntax_error(Context, "error in functor match"),
                !:Errors = cord.snoc(!.Errors, Error)
            )
        ;
            Atom = "ctor",
            ( if
                ArgTerms = [NameArityTerm],
                parse_sym_name_and_arity(NameArityTerm,
                    TypeName, TypeArity)
            then
                TypeCtor = type_ctor(TypeName, TypeArity),
                Ctor = resolved_functor_data_constructor(TypeCtor),
                !:RevCtors = [Ctor | !.RevCtors]
            else
                Error = uf_syntax_error(Context, "error in functor match"),
                !:Errors = cord.snoc(!.Errors, Error)
            )
        ;
            Atom = "field",
            ( if
                ArgTerms = [TypeNameArityTerm, ConsNameArityTerm],
                parse_sym_name_and_arity(TypeNameArityTerm,
                    TypeName, TypeArity),
                parse_sym_name_and_arity(ConsNameArityTerm,
                    ConsName, ConsArity)
            then
                TypeCtor = type_ctor(TypeName, TypeArity),
                ConsCtor = cons_ctor(ConsName, ConsArity, TypeCtor),
                Ctor = resolved_functor_field_access_func(ConsCtor),
                !:RevCtors = [Ctor | !.RevCtors]
            else
                Error = uf_syntax_error(Context, "error in functor match"),
                !:Errors = cord.snoc(!.Errors, Error)
            )
        )
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in functor match"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

%---------------------%

:- type parse_resolved_item_matches(T) ==
    pred(term, resolved_item_map(T), resolved_item_map(T),
        cord(used_file_error), cord(used_file_error)).
:- inst parse_resolved_item_matches == (pred(in, in, out, in, out) is det).

:- pred parse_resolved_item_set(
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches),
    term::in, resolved_item_set(T)::in, resolved_item_set(T)::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_resolved_item_set(ParseMatches, Term, !Set, !Errors) :-
    ( if
        Term = term.functor(term.atom("-"), [NameTerm, MatchesTerm], _),
        NameTerm = term.functor(term.atom(Name), [], _)
    then
        conjunction_to_list(MatchesTerm, MatchTermList),
        list.foldl2(parse_resolved_item_arity_matches(ParseMatches),
            MatchTermList, [], RevMatches, !Errors),
        list.reverse(RevMatches, Matches),
        map.det_insert(Name, Matches, !Set)
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in resolved item matches"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

:- pred parse_resolved_item_arity_matches(
    parse_resolved_item_matches(T)::in(parse_resolved_item_matches), term::in,
    list(pair(arity, resolved_item_map(T)))::in,
    list(pair(arity, resolved_item_map(T)))::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_resolved_item_arity_matches(ParseMatches, Term,
        !RevArityMatchMaps, !Errors) :-
    ( if
        Term = term.functor(term.atom("-"), [ArityTerm, MatchesTerm], _),
        term_int.decimal_term_to_int(ArityTerm, Arity0),
        conjunction_to_list(MatchesTerm, MatchTermList)
    then
        Arity = Arity0,
        list.foldl2(
            ( pred(MatchTerm::in, Map0::in, Map::out,
                    Errors0::in, Errors::out) is det :-
                ParseMatches(MatchTerm, Map0, Map, Errors0, Errors)
            ),
            MatchTermList, map.init, MatchMap, cord.init, TermErrors),
        ( if cord.is_empty(TermErrors) then
            !:RevArityMatchMaps = [Arity - MatchMap | !.RevArityMatchMaps]
        else
            !:Errors = !.Errors ++ TermErrors
        )
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in resolved item matches"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

%---------------------%

:- pred read_and_parse_used_classes(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    used_file_result(set(recomp_item_name))::out) is det.

read_and_parse_used_classes(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, ParseUsedClasses) :-
    read_term_check_for_error_or_eof(UsedFileName, UsedFileString, MaxOffset,
        !LineContext, !LinePosn, "used classes", ReadTerm),
    (
        ReadTerm = used_file_error(Error),
        ParseUsedClasses = used_file_error(Error)
    ;
        ReadTerm = used_file_ok(Term),
        ( if
            Term = term.functor(term.atom("used_classes"), UsedClassTerms, _)
            % XXX The format of the .used file should put UsedClassTerms
            % into a list, to give the used_classes functor a fixed arity.
        then
            list.foldl2(parse_name_and_arity_item_add_to_set,
                UsedClassTerms, set.init, UsedClasses, cord.init, ErrorsCord),
            Errors = cord.list(ErrorsCord),
            (
                Errors = [],
                ParseUsedClasses = used_file_ok(UsedClasses)
            ;
                Errors = [HeadError | _],
                ParseUsedClasses = used_file_error(HeadError)
            )
        else
            Context = get_term_context(Term),
            Error = uf_syntax_error(Context, "error in used_typeclasses term"),
            ParseUsedClasses = used_file_error(Error)
        )
    ).

:- pred parse_name_and_arity_item_add_to_set(term::in,
    set(recomp_item_name)::in, set(recomp_item_name)::out,
    cord(used_file_error)::in, cord(used_file_error)::out) is det.

parse_name_and_arity_item_add_to_set(Term, !UsedClasses, !Errors) :-
    ( if parse_sym_name_and_arity(Term, ClassName, ClassArity) then
        UsedClass = recomp_item_name(ClassName, ClassArity),
        set.insert(UsedClass, !UsedClasses)
    else
        Context = get_term_context(Term),
        Error = uf_syntax_error(Context, "error in used_typeclasses term"),
        !:Errors = cord.snoc(!.Errors, Error)
    ).

%---------------------%

:- pred read_and_parse_used_modules(string::in, string::in, int::in,
    line_context::in, line_context::out, line_posn::in, line_posn::out,
    cord(recomp_used_module)::in,
    used_file_result(list(recomp_used_module))::out) is det.

read_and_parse_used_modules(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, !.UsedModulesCord, ParseUsedModules) :-
    read_term_check_for_error_or_eof(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, "used items list", ReadTerm),
    (
        ReadTerm = used_file_ok(Term),
        % There should always be an item `done.' at the end of the list
        % of modules to check. We use this to make sure that the writing
        % of the `.used' file was not interrupted.
        % XXX See the comment in read_and_parse_used_file for a simpler way
        % of doing that.
        ( if Term = term.functor(term.atom("done"), [], _) then
            % XXX We should check that we are at the end-of-file.
            ParseUsedModules = used_file_ok(cord.list(!.UsedModulesCord))
        else
            % XXX This defaulty representation (the absence of an arrow
            % meaning there is no used items term) is bad design.
            % There should *always* be an arrow (or some other fixed functor),
            % with an explicit representation of an empty set of used items.
            ( if
                Term = term.functor(term.atom("=>"),
                    [TimestampTerm0, UsedItemsTerm], _)
            then
                TimestampTerm = TimestampTerm0,
                parse_module_item_version_numbers(UsedItemsTerm,
                    MaybeUsedItems),
                (
                    MaybeUsedItems = ok1(VersionNumbers),
                    ParseVersionNumbers = used_file_ok(yes(VersionNumbers))
                ;
                    MaybeUsedItems = error1(Specs),
                    VNError = uf_unreadable_used_items(Specs),
                    ParseVersionNumbers = used_file_error(VNError)
                )
            else
                TimestampTerm = Term,
                ParseVersionNumbers = used_file_ok(no)
            ),
            parse_module_timestamp(TimestampTerm, ParseModuleTimestamp),
            (
                ParseModuleTimestamp = used_file_ok(ModuleNameTimestamp),
                ModuleNameTimestamp =
                    module_name_and_timestamp( ModuleName, ModuleTimestamp),
                (
                    ParseVersionNumbers = used_file_ok(MaybeVersionNumbers),
                    UsedModule = recomp_used_module(ModuleName,
                        ModuleTimestamp, MaybeVersionNumbers),
                    !:UsedModulesCord =
                        cord.snoc(!.UsedModulesCord, UsedModule),
                    read_and_parse_used_modules(FileName, FileString,
                        MaxOffset, !LineContext, !LinePosn, !.UsedModulesCord,
                        ParseUsedModules)
                ;
                    ParseVersionNumbers = used_file_error(Error),
                    ParseUsedModules = used_file_error(Error)
                )
            ;
                ParseModuleTimestamp = used_file_error(Error),
                ParseUsedModules = used_file_error(Error)
            )
        )
    ;
        ReadTerm = used_file_error(Error),
        ParseUsedModules = used_file_error(Error)
    ).

%---------------------------------------------------------------------------%

:- pred read_term_check_for_error_or_eof(string::in,
    string::in, int::in, line_context::in, line_context::out,
    line_posn::in, line_posn::out, string::in, used_file_result(term)::out)
    is det.

read_term_check_for_error_or_eof(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, ItemName, ReadTerm) :-
    mercury_term_parser.read_term_from_linestr(FileName, FileString, MaxOffset,
        !LineContext, !LinePosn, TermResult),
    (
        TermResult = term(_, Term),
        ReadTerm = used_file_ok(Term)
    ;
        TermResult = error(Message, _LineNumber),
        !.LineContext = line_context(LineNumber, _OffsetAtStartOfLine),
        Error = uf_syntax_error(context(FileString, LineNumber), Message),
        ReadTerm = used_file_error(Error)
    ;
        TermResult = eof,
        !.LineContext = line_context(LineNumber, _OffsetAtStartOfLine),
        Message = "unexpected end of file, expected " ++ ItemName ++ ".",
        Error = uf_syntax_error(context(FileName, LineNumber), Message),
        ReadTerm = used_file_error(Error)
    ).

%---------------------------------------------------------------------------%
:- end_module recompilation.used_file.
%---------------------------------------------------------------------------%
