%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2005 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: recompilation.usage.m.
% Main author: stayl.

% Write the file recording which imported items were used by a compilation.

%-----------------------------------------------------------------------------%

:- module recompilation__usage.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.

    % The resolved_used_items records the possible matches for a program item.
    % It is used by recompilation_check.m to work out whether a new item
    % could cause ambiguity with an item which was used during a compilation.
:- type resolved_used_items ==
    item_id_set(simple_item_set, resolved_pred_or_func_set,
        resolved_functor_set).

:- type resolved_pred_or_func_set ==
    resolved_item_set(set(pair(pred_id, module_name))).
:- type resolved_pred_or_func_map ==
    resolved_item_map(set(pair(pred_id, module_name))).

    % A resolved_functor_set records all possible matches
    % for each functor application.
:- type resolved_functor_set == resolved_item_set(set(resolved_functor)).
:- type resolved_functor_map == resolved_item_map(set(resolved_functor)).

:- type resolved_item_set(T) == map(string, resolved_item_list(T)).

    % The list is sorted on arity. This is useful because when determining
    % whether there is an ambiguity we need to test a predicate or function
    % against all used functors with equal or lower arity.
:- type resolved_item_list(T) == assoc_list(arity, resolved_item_map(T)).

:- type resolved_item_map(T) == map(module_qualifier, T).

:- type resolved_functor
    --->    pred_or_func(
                pred_id,
                module_name,
                pred_or_func,
                arity       % The actual arity of the predicate or function
            )
    ;       constructor(
                item_name   % type_ctor
            )
    ;       field(
                item_name,  % type_ctor
                item_name   % cons_id
            ).

:- pred write_usage_file(module_info::in,
    list(module_name)::in, maybe(module_timestamps)::in,
    io::di, io::uo) is det.

    % Changes which modify the format of the `.used' files will increment
    % this number. recompilation_check.m should recompile if the version number
    % is out of date.
    %
:- func usage_file_version_number = int.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module queue.
:- import_module std_util.
:- import_module string.
:- import_module svmap.

write_usage_file(ModuleInfo, NestedSubModules,
        MaybeTimestamps, !IO) :-
    module_info_get_maybe_recompilation_info(ModuleInfo,
        MaybeRecompInfo),
    (
        MaybeRecompInfo = yes(RecompInfo),
        MaybeTimestamps = yes(Timestamps)
    ->
        globals__io_lookup_bool_option(verbose, Verbose, !IO),
        maybe_write_string(Verbose,
            "% Writing recompilation compilation dependency information\n",
            !IO),

        module_info_get_name(ModuleInfo, ModuleName),
        module_name_to_file_name(ModuleName, ".used", yes, FileName, !IO),
        io__open_output(FileName, FileResult, !IO),
        (
            FileResult = ok(Stream0),
            io__set_output_stream(Stream0, OldStream, !IO),
            write_usage_file_2(ModuleInfo,
                NestedSubModules, RecompInfo, Timestamps, !IO),
            io__set_output_stream(OldStream, Stream, !IO),
            io__close_output(Stream, !IO)
        ;
            FileResult = error(IOError),
            io__error_message(IOError, IOErrorMessage),
            io__write_string("\nError opening `", !IO),
            io__write_string(FileName, !IO),
            io__write_string("'for output: ", !IO),
            io__write_string(IOErrorMessage, !IO),
            io__write_string(".\n", !IO),
            io__set_exit_status(1, !IO)
        )
    ;
        true
    ).

:- pred write_usage_file_2(module_info::in,
    list(module_name)::in, recompilation_info::in,
    module_timestamps::in, io::di, io::uo) is det.

write_usage_file_2(ModuleInfo, NestedSubModules,
        RecompInfo, Timestamps, !IO) :-
    io__write_int(usage_file_version_number, !IO),
    io__write_string(",", !IO),
    io__write_int(version_numbers_version_number, !IO),
    io__write_string(".\n\n", !IO),

    module_info_get_name(ModuleInfo, ThisModuleName),
    map__lookup(Timestamps, ThisModuleName,
        module_timestamp(_, ThisModuleTimestamp, _)),
    io__write_string("(", !IO),
    mercury_output_bracketed_sym_name(ThisModuleName, !IO),
    io__write_string(", "".m"", ", !IO),
    write_version_number(ThisModuleTimestamp, !IO),
    io__write_string(").\n\n", !IO),

    (
        NestedSubModules = [],
        io__write_string("sub_modules.\n\n", !IO)
    ;
        NestedSubModules = [_ | _],
        io__write_string("sub_modules(", !IO),
        io__write_list(NestedSubModules, ", ",
            mercury_output_bracketed_sym_name, !IO),
        io__write_string(").\n\n", !IO)
    ),

    UsedItems = RecompInfo ^ used_items,
    find_all_used_imported_items(ModuleInfo,
        UsedItems, RecompInfo ^ dependencies, ResolvedUsedItems,
        UsedClasses, ImportedItems, ModuleInstances),

    ( UsedItems = init_used_items ->
        io__write_string("used_items.\n", !IO)
    ;
        io__write_string("used_items(\n\t", !IO),
        some [!WriteComma] (
            !:WriteComma = no,
            write_simple_item_matches(type_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_simple_item_matches(type_body_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_simple_item_matches(mode_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_simple_item_matches(inst_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_simple_item_matches(typeclass_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_pred_or_func_matches(predicate_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_pred_or_func_matches(function_item, ResolvedUsedItems,
                !WriteComma, !IO),
            write_functor_matches(ResolvedUsedItems ^ functors,
                !WriteComma, !IO),
            _ = !.WriteComma
        ),
        io__write_string("\n).\n\n", !IO)
    ),

    ( set__empty(UsedClasses) ->
        io__write_string("used_classes.\n", !IO)
    ;
        io__write_string("used_classes(", !IO),
        io__write_list(set__to_sorted_list(UsedClasses), ", ",
            write_classname_and_arity, !IO),
        io__write_string(").\n", !IO)
    ),

    map__foldl(write_module_name_and_used_items(RecompInfo, Timestamps,
        ModuleInstances), ImportedItems, !IO),
    % recompilation_check.m checks for this item when reading in the `.used'
    % file to make sure the earlier compilation wasn't interrupted in the
    % middle of writing the file.
    io__nl(!IO),
    io__write_string("done.\n", !IO).

:- pred write_module_name_and_used_items(recompilation_info::in,
    module_timestamps::in, map(module_name, set(item_name))::in,
    module_name::in, item_id_set(set(pair(string, arity)))::in,
    io::di, io::uo) is det.

write_module_name_and_used_items(RecompInfo, Timestamps, ModuleInstances,
        ModuleName, ModuleUsedItems, !IO) :-
    io__nl(!IO),
    io__write_string("(", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io__write_string(", """, !IO),
    map__lookup(Timestamps, ModuleName,
        module_timestamp(Suffix, ModuleTimestamp, NeedQualifier)),
    io__write_string(Suffix, !IO),
    io__write_string(""", ", !IO),
    write_version_number(ModuleTimestamp, !IO),
    ( NeedQualifier = must_be_qualified ->
        io__write_string(", used)", !IO)
    ;
        io__write_string(")", !IO)
    ),
    (
        % XXX We don't yet record all uses of items from these modules
        % in polymorphism.m, etc.
        \+ any_mercury_builtin_module(ModuleName),
        map__search(RecompInfo ^ version_numbers, ModuleName, ModuleVersions)
    ->
        % Select out from the version numbers of all items in the imported
        % module the ones which are used.
        ModuleVersions = version_numbers(ModuleItemVersions,
            ModuleInstanceVersions),
        ModuleUsedItemVersions = map_ids(
            (func(ItemType, Ids0) = Ids :-
                ModuleItemNames = extract_ids(ModuleUsedItems, ItemType),
                map__select(Ids0, ModuleItemNames, Ids)
            ),
            ModuleItemVersions, map__init),
        (
            map__search(ModuleInstances, ModuleName, ModuleUsedInstances)
        ->
            map__select(ModuleInstanceVersions, ModuleUsedInstances,
                ModuleUsedInstanceVersions)
        ;
            map__init(ModuleUsedInstanceVersions)
        ),

        io__write_string(" => ", !IO),
        ModuleUsedVersionNumbers =
            version_numbers(ModuleUsedItemVersions,
                ModuleUsedInstanceVersions),
        write_version_numbers(ModuleUsedVersionNumbers, !IO),
        io__write_string(".\n", !IO)
    ;
        % If we don't have version numbers for a module we just recompile
        % if the interface file's timestamp changes.
        io__write_string(".\n", !IO)
    ).

:- pred write_classname_and_arity(pair(class_name, arity)::in,
    io::di, io::uo) is det.

write_classname_and_arity(ClassName - ClassArity, !IO) :-
    mercury_output_bracketed_sym_name(ClassName, !IO),
    io__write_string("/", !IO),
    io__write_int(ClassArity, !IO).

:- pred write_comma_if_needed(bool::in, bool::out, io::di, io::uo) is det.

write_comma_if_needed(!WriteComma, !IO) :-
    (
        !.WriteComma = yes,
        io__write_string(",\n\t", !IO)
    ;
        !.WriteComma = no
    ),
    !:WriteComma = yes.

:- pred write_simple_item_matches(item_type::in(simple_item),
    resolved_used_items::in, bool::in, bool::out, io::di, io::uo) is det.

write_simple_item_matches(ItemType, UsedItems, !WriteComma, !IO) :-
    Ids = extract_simple_item_set(UsedItems, ItemType),
    ( map__is_empty(Ids) ->
        true
    ;
        write_comma_if_needed(!WriteComma, !IO),
        write_simple_item_matches_2(ItemType, Ids, !IO)
    ).

:- pred write_simple_item_matches_2(item_type::in, simple_item_set::in,
    io::di, io::uo) is det.

write_simple_item_matches_2(ItemType, ItemSet, !IO) :-
    string_to_item_type(ItemTypeStr, ItemType),
    io__write_string(ItemTypeStr, !IO),
    io__write_string("(\n\t\t", !IO),
    map__to_assoc_list(ItemSet, ItemList),
    io__write_list(ItemList, ",\n\t\t", write_simple_item_matches_3, !IO),
    io__write_string("\n\t)", !IO).

:- pred write_simple_item_matches_3(
    pair(pair(string, arity), map(module_qualifier, module_name))::in,
    io::di, io::uo) is det.

write_simple_item_matches_3((Name - Arity) - Matches, !IO) :-
    mercury_output_bracketed_sym_name(unqualified(Name), next_to_graphic_token,
        !IO),
    io__write_string("/", !IO),
    io__write_int(Arity, !IO),
    io__write_string(" - (", !IO),
    map__to_assoc_list(Matches, MatchList),
    io__write_list(MatchList, ", ", write_simple_item_matches_4, !IO),
    io__write_string(")", !IO).

:- pred write_simple_item_matches_4(pair(module_qualifier, module_name)::in,
    io::di, io::uo) is det.

write_simple_item_matches_4(Qualifier - ModuleName, !IO) :-
    mercury_output_bracketed_sym_name(Qualifier, !IO),
    ( Qualifier = ModuleName ->
        true
    ;
        io__write_string(" => ", !IO),
        mercury_output_bracketed_sym_name(ModuleName, !IO)
    ).

:- pred write_pred_or_func_matches(item_type::in(pred_or_func),
    resolved_used_items::in, bool::in, bool::out,
    io::di, io::uo) is det.

write_pred_or_func_matches(ItemType, UsedItems, !WriteComma, !IO) :-
    Ids = extract_pred_or_func_set(UsedItems, ItemType),
    ( map__is_empty(Ids) ->
        true
    ;
        write_comma_if_needed(!WriteComma, !IO),
        write_pred_or_func_matches_2(ItemType, Ids, !IO)
    ).

:- pred write_pred_or_func_matches_2(item_type::in(pred_or_func),
    resolved_pred_or_func_set::in, io::di, io::uo) is det.

write_pred_or_func_matches_2(ItemType, ItemSet, !IO) :-
    write_resolved_item_set(ItemType, ItemSet,
        write_pred_or_func_matches_3, !IO).

:- pred write_pred_or_func_matches_3(
    pair(sym_name, set(pair(pred_id, sym_name)))::in,
    io::di, io::uo) is det.

write_pred_or_func_matches_3(Qualifier - PredIdModuleNames, !IO) :-
    ModuleNames = assoc_list__values(set__to_sorted_list(PredIdModuleNames)),
    mercury_output_bracketed_sym_name(Qualifier, !IO),
    ( ModuleNames = [Qualifier] ->
        true
    ;
        io__write_string(" => (", !IO),
        io__write_list(ModuleNames, ", ", mercury_output_bracketed_sym_name,
            !IO),
        io__write_string(")", !IO)
    ).

:- pred write_functor_matches(resolved_functor_set::in,
    bool::in, bool::out, io::di, io::uo) is det.

write_functor_matches(Ids, !WriteComma, !IO) :-
    ( map__is_empty(Ids) ->
        true
    ;
        write_comma_if_needed(!WriteComma, !IO),
        write_resolved_item_set(functor_item, Ids, write_functor_matches_2,
            !IO)
    ).

:- pred write_functor_matches_2(pair(sym_name, set(resolved_functor))::in,
    io::di, io::uo) is det.

write_functor_matches_2(Qualifier - MatchingCtors, !IO) :-
    mercury_output_bracketed_sym_name(Qualifier, !IO),
    io__write_string(" => (", !IO),
    io__write_list(set__to_sorted_list(MatchingCtors), ", ",
        write_resolved_functor, !IO),
    io__write_string(")", !IO).

:- type write_resolved_item(T) == pred(pair(module_qualifier, T), io, io).
:- inst write_resolved_item == (pred(in, di, uo) is det).

:- pred write_resolved_item_set(item_type::in, resolved_item_set(T)::in,
    write_resolved_item(T)::in(write_resolved_item),
    io::di, io::uo) is det.

write_resolved_item_set(ItemType, ItemSet, WriteMatches, !IO) :-
    string_to_item_type(ItemTypeStr, ItemType),
    io__write_string(ItemTypeStr, !IO),
    io__write_string("(\n\t\t", !IO),
    map__to_assoc_list(ItemSet, ItemList),
    io__write_list(ItemList, ",\n\t\t",
        write_resolved_item_set_2(WriteMatches), !IO),
    io__write_string("\n\t)", !IO).

:- pred write_resolved_item_set_2(
    write_resolved_item(T)::in(write_resolved_item),
    pair(string, list(pair(int, map(sym_name, T))))::in,
    io::di, io::uo) is det.

write_resolved_item_set_2(WriteMatches, Name - MatchesAL, !IO) :-
    mercury_output_bracketed_sym_name(unqualified(Name), !IO),
    io__write_string(" - (", !IO),
    io__write_list(MatchesAL, ",\n\t\t\t",
        write_resolved_item_set_3(WriteMatches), !IO),
    io__write_string(")", !IO).

:- pred write_resolved_item_set_3(
    write_resolved_item(T)::in(write_resolved_item),
    pair(int, map(sym_name, T))::in, io::di, io::uo) is det.

write_resolved_item_set_3(WriteMatches, Arity - Matches, !IO) :-
    io__write_int(Arity, !IO),
    io__write_string(" - (", !IO),
    map__to_assoc_list(Matches, MatchList),
    io__write_list(MatchList, ",\n\t\t\t\t", WriteMatches, !IO),
    io__write_string(")", !IO).

:- pred write_resolved_functor(resolved_functor::in, io::di, io::uo) is det.

write_resolved_functor(pred_or_func(_, ModuleName, PredOrFunc, Arity), !IO) :-
    io__write(PredOrFunc, !IO),
    io__write_string("(", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io__write_string(", ", !IO),
    io__write_int(Arity, !IO),
    io__write_string(")", !IO).
write_resolved_functor(constructor(TypeName - Arity), !IO) :-
    io__write_string("ctor(", !IO),
    mercury_output_bracketed_sym_name(TypeName, next_to_graphic_token, !IO),
    io__write_string("/", !IO),
    io__write_int(Arity, !IO),
    io__write_string(")", !IO).
write_resolved_functor(field(TypeName - TypeArity, ConsName - ConsArity),
        !IO) :-
    io__write_string("field(", !IO),
    mercury_output_bracketed_sym_name(TypeName, next_to_graphic_token, !IO),
    io__write_string("/", !IO),
    io__write_int(TypeArity, !IO),
    io__write_string(", ", !IO),
    mercury_output_bracketed_sym_name(ConsName, next_to_graphic_token, !IO),
    io__write_string("/", !IO),
    io__write_int(ConsArity, !IO),
    io__write_string(")", !IO).

usage_file_version_number = 2.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type recompilation_usage_info
    --->    recompilation_usage_info(
                module_info         :: module_info,
                item_queue          :: queue(item_id),
                imported_items      :: imported_items,
                module_instances    :: map(module_name, set(item_name)),
                                    % For each module, the used typeclasses for
                                    % which the module contains an instance.

                dependencies        :: map(item_id, set(item_id)),
                used_items          :: resolved_used_items,
                used_typeclasses    :: set(item_name)
            ).

:- type imported_items == map(module_name, module_imported_items).

    % The constructors set should always be empty -
    % constructors are never imported separately.
:- type module_imported_items == item_id_set(imported_item_set).

:- type imported_item_set == set(pair(string, arity)).

%-----------------------------------------------------------------------------%

:- pred visible_modules(module_info::in, module_name::out) is nondet.

visible_modules(ModuleInfo, VisibleModule) :-
    visible_module(VisibleModule, ModuleInfo),
    \+ module_info_get_name(ModuleInfo, VisibleModule).

:- pred insert_into_imported_items_map(module_name::in,
    imported_items::in, imported_items::out) is det.

insert_into_imported_items_map(VisibleModule, !ImportedItemsMap) :-
    ModuleItems = init_item_id_set(set__init),
    svmap__det_insert(VisibleModule, ModuleItems, !ImportedItemsMap).

    % Go over the set of imported items found to be used and
    % find the transitive closure of the imported items they use.
    %
:- pred find_all_used_imported_items(module_info::in,
    used_items::in, map(item_id, set(item_id))::in,
    resolved_used_items::out, set(item_name)::out, imported_items::out,
    map(module_name, set(item_name))::out) is det.

find_all_used_imported_items(ModuleInfo,
        UsedItems, Dependencies, ResolvedUsedItems, UsedTypeClasses,
        ImportedItems, ModuleInstances) :-
    % We need to make sure each visible module has an entry in the `.used'
    % file, even if nothing was used from it. This will cause
    % recompilation_check.m to check for new items causing ambiguity
    % when the interface of the module changes.
    map__init(ImportedItems0),
    promise_equivalent_solutions [ImportedItems1] (
        std_util__unsorted_aggregate(visible_modules(ModuleInfo),
            insert_into_imported_items_map, ImportedItems0, ImportedItems1)
    ),

    queue__init(ItemsToProcess0),
    map__init(ModuleUsedClasses),
    set__init(UsedClasses0),

    UsedItems = item_id_set(Types, TypeBodies, Modes, Insts, Classes,
        _, _, _, _),
    map__init(ResolvedCtors),
    map__init(ResolvedPreds),
    map__init(ResolvedFuncs),
    map__init(ResolvedMutables),
    ResolvedUsedItems0 = item_id_set(Types, TypeBodies, Modes, Insts,
        Classes, ResolvedCtors, ResolvedPreds, ResolvedFuncs,
        ResolvedMutables),

    Info0 = recompilation_usage_info(ModuleInfo, ItemsToProcess0,
        ImportedItems1, ModuleUsedClasses, Dependencies,
        ResolvedUsedItems0, UsedClasses0),

    find_all_used_imported_items_2(UsedItems,
        Info0, Info),

    ImportedItems = Info ^ imported_items,
    ModuleInstances = Info ^ module_instances,
    UsedTypeClasses = Info ^ used_typeclasses,
    ResolvedUsedItems = Info ^ used_items.

:- pred find_all_used_imported_items_2(used_items::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_all_used_imported_items_2(UsedItems, !Info) :-
    % Find items used by imported instances for local classes.
    ModuleInfo = !.Info ^ module_info,
    module_info_get_instance_table(ModuleInfo, Instances),
    map__foldl(find_items_used_by_instances, Instances, !Info),

    Predicates = UsedItems ^ predicates,
    find_items_used_by_preds(predicate, Predicates, !Info),

    Functions = UsedItems ^ functions,
    find_items_used_by_preds(function, Functions, !Info),

    Constructors = UsedItems ^ functors,
    find_items_used_by_functors(Constructors, !Info),

    Types = UsedItems ^ types,
    find_items_used_by_simple_item_set(type_item, Types, !Info),

    TypeBodies = UsedItems ^ type_bodies,
    find_items_used_by_simple_item_set(type_body_item, TypeBodies, !Info),

    Modes = UsedItems ^ modes,
    find_items_used_by_simple_item_set(mode_item, Modes, !Info),

    Classes = UsedItems ^ typeclasses,
    find_items_used_by_simple_item_set(typeclass_item, Classes, !Info),

    Insts = UsedItems ^ insts,
    find_items_used_by_simple_item_set(inst_item, Insts, !Info),

    process_imported_item_queue(!Info).

:- pred process_imported_item_queue(
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

process_imported_item_queue(!Info) :-
    Queue0 = !.Info ^ item_queue,
    !:Info = !.Info ^ item_queue := queue__init,
    process_imported_item_queue_2(Queue0, !Info),
    Queue = !.Info ^ item_queue,
    ( queue__is_empty(Queue) ->
        true
    ;
        process_imported_item_queue(!Info)
    ).

:- pred process_imported_item_queue_2(
    queue(item_id)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

process_imported_item_queue_2(Queue0, !Info) :-
    ( queue__get(Queue0, Item, Queue) ->
        Item = item_id(ItemType, ItemId),
        find_items_used_by_item(ItemType, ItemId, !Info),
        process_imported_item_queue_2(Queue, !Info)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred record_used_pred_or_func(pred_or_func::in,
    pair(sym_name, arity)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_used_pred_or_func(PredOrFunc, Id, !Info) :-
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    ItemSet0 = !.Info ^ used_items,
    IdSet0 = extract_pred_or_func_set(ItemSet0, ItemType),
    Id = SymName - Arity,
    record_resolved_item(SymName, Arity,
        do_record_used_pred_or_func(PredOrFunc),
        IdSet0, IdSet, !Info),
    ItemSet = update_pred_or_func_set(ItemSet0, ItemType, IdSet),
    !:Info = !.Info ^ used_items := ItemSet.

:- pred do_record_used_pred_or_func(pred_or_func::in,
    module_qualifier::in, sym_name::in, arity::in, bool::out,
    resolved_pred_or_func_map::in, resolved_pred_or_func_map::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

do_record_used_pred_or_func(PredOrFunc, ModuleQualifier,
        SymName, Arity, Recorded, !MatchingNames, !Info) :-
    ModuleInfo = !.Info ^ module_info,
    (
        module_info_get_predicate_table(ModuleInfo, PredTable),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        predicate_table_search_pf_sym_arity(PredTable,
            may_be_partially_qualified, PredOrFunc, SymName,
            OrigArity, MatchingPredIds)
    ->
        Recorded = yes,
        PredModules = set__list_to_set(list__map(
            (func(PredId) = PredId - PredModule :-
                module_info_pred_info(ModuleInfo, PredId, PredInfo),
                PredModule = pred_info_module(PredInfo)
            ),
            MatchingPredIds)),
        svmap__det_insert(ModuleQualifier, PredModules, !MatchingNames),
        unqualify_name(SymName, Name),
        set__fold(find_items_used_by_pred(PredOrFunc, Name - Arity),
            PredModules, !Info)
    ;
        Recorded = no
    ).

%-----------------------------------------------------------------------------%

:- pred record_used_functor(pair(sym_name, arity)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_used_functor(SymName - Arity, !Info) :-
    ItemSet0 = !.Info ^ used_items,
    IdSet0 = ItemSet0 ^ functors,
    record_resolved_item(SymName, Arity, do_record_used_functor,
        IdSet0, IdSet, !Info),
    ItemSet = ItemSet0 ^ functors := IdSet,
    !:Info = !.Info ^ used_items := ItemSet.

:- pred do_record_used_functor(module_qualifier::in,
    sym_name::in, arity::in, bool::out, resolved_functor_map::in,
    resolved_functor_map::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

do_record_used_functor(ModuleQualifier, SymName, Arity, Recorded,
        !ResolvedCtorMap, !Info) :-
    ModuleInfo = !.Info ^ module_info,

    find_matching_functors(ModuleInfo, SymName, Arity, MatchingCtors),
    unqualify_name(SymName, Name),
    set__fold(find_items_used_by_functor(Name, Arity), MatchingCtors, !Info),

    ( set__empty(MatchingCtors) ->
        Recorded = no
    ;
        Recorded = yes,
        svmap__det_insert(ModuleQualifier, MatchingCtors, !ResolvedCtorMap)
    ).

:- pred find_matching_functors(module_info::in,
    sym_name::in, arity::in, set(resolved_functor)::out) is det.

find_matching_functors(ModuleInfo, SymName, Arity, ResolvedConstructors) :-
    % Is it a constructor.
    module_info_get_cons_table(ModuleInfo, Ctors),
    ( map__search(Ctors, cons(SymName, Arity), ConsDefns0) ->
        ConsDefns1 = ConsDefns0
    ;
        ConsDefns1 = []
    ),
    (
        remove_new_prefix(SymName, SymNameMinusNew),
        map__search(Ctors, cons(SymNameMinusNew, Arity), ConsDefns2)
    ->
        ConsDefns = list__append(ConsDefns1, ConsDefns2)
    ;
        ConsDefns = ConsDefns1
    ),
    MatchingConstructors =
        list__map(
            (func(ConsDefn) = Ctor :-
                ConsDefn = hlds_cons_defn(_,_,_, TypeCtor, _),
                Ctor = constructor(TypeCtor)
            ),
            ConsDefns),

    % Is it a higher-order term or function call.
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    (
        predicate_table_search_sym(PredicateTable,
            may_be_partially_qualified, SymName, PredIds)
    ->
        MatchingPreds = list__filter_map(
            get_pred_or_func_ctors(ModuleInfo,
                SymName, Arity),
            PredIds)
    ;
        MatchingPreds = []
    ),

    % Is it a field access function.
    (
        is_field_access_function_name(ModuleInfo, SymName, Arity,
            _, FieldName),
        module_info_get_ctor_field_table(ModuleInfo, CtorFields),
        map__search(CtorFields, FieldName, FieldDefns)
    ->
        MatchingFields = list__map(
            (func(FieldDefn) = FieldCtor :-
                FieldDefn = hlds_ctor_field_defn(_, _, TypeCtor, ConsId, _),
                ( ConsId = cons(ConsName, ConsArity) ->
                    FieldCtor = field(TypeCtor, ConsName - ConsArity)
                ;
                    unexpected(this_file, "weird cons_id in hlds_field_defn")
                )
            ), FieldDefns)
    ;
        MatchingFields = []
    ),

    ResolvedConstructors = set__list_to_set(list__condense(
        [MatchingConstructors, MatchingPreds, MatchingFields])
    ).

:- func get_pred_or_func_ctors(module_info, sym_name,
    arity, pred_id) = resolved_functor is semidet.

get_pred_or_func_ctors(ModuleInfo, _SymName, Arity, PredId) = ResolvedCtor :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredArity = pred_info_orig_arity(PredInfo),
    pred_info_get_exist_quant_tvars(PredInfo, PredExistQVars),
    adjust_func_arity(PredOrFunc, OrigArity, PredArity),
    (
        PredOrFunc = predicate,
        OrigArity >= Arity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified predicate.
        PredExistQVars = []
    ;
        PredOrFunc = function,
        OrigArity >= Arity,
        % We don't support first-class polymorphism, so you can't take
        % the address of an existentially quantified function. You can however
        % call such a function, so long as you pass *all* the parameters.
        ( PredExistQVars = []
        ; OrigArity = Arity
        )
    ),
    ResolvedCtor = pred_or_func(PredId, PredModule, PredOrFunc, OrigArity).

%-----------------------------------------------------------------------------%

:- type record_resolved_item(T) ==
    pred(module_qualifier, sym_name, arity, bool,
        resolved_item_map(T), resolved_item_map(T),
        recompilation_usage_info, recompilation_usage_info).
:- inst record_resolved_item ==
    (pred(in, in, in, out, in, out, in, out) is det).

:- pred record_resolved_item(sym_name::in, arity::in,
    record_resolved_item(T)::in(record_resolved_item),
    resolved_item_set(T)::in, resolved_item_set(T)::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_resolved_item(SymName, Arity, RecordItem, !IdSet, !Info) :-
    unqualify_name(SymName, UnqualifiedName),
    ModuleQualifier = find_module_qualifier(SymName),
    ( map__search(!.IdSet, UnqualifiedName, MatchingNames0) ->
        MatchingNames1 = MatchingNames0
    ;
        MatchingNames1 = []
    ),
    record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem,
        Recorded, MatchingNames1, MatchingNames, !Info),
    (
        Recorded = yes,
        svmap__set(UnqualifiedName, MatchingNames, !IdSet)
    ;
        Recorded = no
    ).

:- pred record_resolved_item_2(module_qualifier::in, sym_name::in, arity::in,
    record_resolved_item(T)::in(record_resolved_item), bool::out,
    resolved_item_list(T)::in, resolved_item_list(T)::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_resolved_item_2(ModuleQualifier,
        SymName, Arity, RecordItem, Recorded, !List, !Info) :-
    !.List = [],
    map__init(Map0),
    record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem,
        Recorded, Map0, Map, !Info),
    (
        Recorded = yes,
        !:List = [Arity - Map]
    ;
        Recorded = no
    ).
record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem, Recorded,
        !List, !Info) :-
    !.List = [ThisArity - ArityMap0 | ListRest0],
    ( Arity < ThisArity ->
        map__init(NewArityMap0),
        record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem,
            Recorded, NewArityMap0, NewArityMap, !Info),
        (
            Recorded = yes,
            !:List = [Arity - NewArityMap | !.List]
        ;
            Recorded = no
        )
    ; Arity = ThisArity ->
        record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem,
            Recorded, ArityMap0, ArityMap, !Info),
        (
            Recorded = yes,
            !:List = [Arity - ArityMap | ListRest0]
        ;
            Recorded = no
        )
    ;
        record_resolved_item_2(ModuleQualifier, SymName, Arity, RecordItem,
            Recorded, ListRest0, ListRest, !Info),
        (
            Recorded = yes,
            !:List = [ThisArity - ArityMap0 | ListRest]
        ;
            Recorded = no
        )
    ).

:- pred record_resolved_item_3(module_qualifier::in, sym_name::in, arity::in,
    record_resolved_item(T)::in(record_resolved_item), bool::out,
    resolved_item_map(T)::in, resolved_item_map(T)::out,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_resolved_item_3(ModuleQualifier, SymName, Arity, RecordItem, Recorded,
        !ResolvedMap, !Info) :-
    ( map__contains(!.ResolvedMap, ModuleQualifier) ->
        Recorded = no
    ;
        RecordItem(ModuleQualifier, SymName, Arity, Recorded,
            !ResolvedMap, !Info)
    ).

%-----------------------------------------------------------------------------%

:- pred find_items_used_by_item(item_type::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_item(type_item, TypeCtor, !Info) :-
    ModuleInfo = !.Info ^ module_info,
    module_info_get_type_table(ModuleInfo, Types),
    map__lookup(Types, TypeCtor, TypeDefn),
    hlds_data__get_type_defn_body(TypeDefn, TypeBody),
    ( TypeBody = eqv_type(Type) ->
        % If we use an equivalence type we also use the type
        % it is equivalent to.
        find_items_used_by_type(Type, !Info)
    ;
        true
    ).
find_items_used_by_item(type_body_item, TypeCtor, !Info) :-
    ModuleInfo = !.Info ^ module_info,
    module_info_get_type_table(ModuleInfo, Types),
    map__lookup(Types, TypeCtor, TypeDefn),
    hlds_data__get_type_defn_body(TypeDefn, TypeBody),
    find_items_used_by_type_body(TypeBody, !Info).
find_items_used_by_item(mode_item, ModeId, !Info):-
    ModuleInfo = !.Info ^ module_info,
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefns),
    map__lookup(ModeDefns, ModeId, ModeDefn),
    find_items_used_by_mode_defn(ModeDefn, !Info).
find_items_used_by_item(inst_item, InstId, !Info):-
    ModuleInfo = !.Info ^ module_info,
    module_info_get_inst_table(ModuleInfo, Insts),
    inst_table_get_user_insts(Insts, UserInsts),
    user_inst_table_get_inst_defns(UserInsts, UserInstDefns),
    map__lookup(UserInstDefns, InstId, InstDefn),
    find_items_used_by_inst_defn(InstDefn, !Info).
find_items_used_by_item(typeclass_item, ClassItemId, !Info) :-
    ClassItemId = ClassName - ClassArity,
    ClassId = class_id(ClassName, ClassArity),
    ModuleInfo = !.Info ^ module_info,
    module_info_get_class_table(ModuleInfo, Classes),
    map__lookup(Classes, ClassId, ClassDefn),
    Constraints = ClassDefn ^ class_supers,
    ClassInterface = ClassDefn ^ class_interface,
    find_items_used_by_class_constraints(Constraints, !Info),
    (
        ClassInterface = abstract
    ;
        ClassInterface = concrete(Methods),
        list__foldl(find_items_used_by_class_method, Methods, !Info)
    ),
    module_info_get_instance_table(ModuleInfo, Instances),
    ( map__search(Instances, ClassId, InstanceDefns) ->
        list__foldl(find_items_used_by_instance(ClassItemId), InstanceDefns,
            !Info)
    ;
        true
    ).
find_items_used_by_item(predicate_item, ItemId, !Info) :-
    record_used_pred_or_func(predicate, ItemId, !Info).
find_items_used_by_item(function_item, ItemId, !Info) :-
    record_used_pred_or_func(function, ItemId, !Info).
find_items_used_by_item(functor_item, _, !Info) :-
    unexpected(this_file, "find_items_used_by_item: functor").
find_items_used_by_item(mutable_item, _MutableItemId, !Info).
    %
    % Mutables are expanded into other item types which track the
    % types, insts, preds, and funcs used.

:- pred find_items_used_by_instances(class_id::in,
    list(hlds_instance_defn)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_instances(ClassId, InstanceDefns, !Info) :-
    ClassId = class_id(Name, Arity),
    NameArity = Name - Arity,
    ( item_is_local(!.Info, NameArity) ->
        record_expanded_items_used_by_item(typeclass_item, NameArity, !Info),
        list__foldl(find_items_used_by_instance(NameArity), InstanceDefns,
            !Info)
    ;
        true
    ).

:- pred find_items_used_by_instance(item_name::in, hlds_instance_defn::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_instance(ClassId, Defn, !Info) :-
    Defn = hlds_instance_defn(InstanceModuleName, _, _, Constraints,
        ArgTypes, _, _, _, _),
    % XXX Handle interface (currently not needed because the interfaces
    % for imported instances are only needed with --intermodule-optimization,
    % which isn't handled here yet).
    ModuleInfo = !.Info ^ module_info,
    ( module_info_get_name(ModuleInfo, InstanceModuleName) ->
        true
    ;
        find_items_used_by_class_constraints(Constraints, !Info),
        find_items_used_by_types(ArgTypes, !Info),
        ModuleInstances0 = !.Info ^ module_instances,
        ( map__search(ModuleInstances0, InstanceModuleName, ClassIdsPrime) ->
            ClassIds1 = ClassIdsPrime
        ;
            set__init(ClassIds1)
        ),
        set__insert(ClassIds1, ClassId, ClassIds),
        map__set(ModuleInstances0, InstanceModuleName, ClassIds,
            ModuleInstances),
        !:Info = !.Info ^ module_instances := ModuleInstances
    ).

:- pred find_items_used_by_class_method(class_method::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_method(Method, !Info) :-
    Method = pred_or_func(_, _, _, _, _, ArgTypesAndModes, _, _, _, _, _,
        Constraints, _),
    find_items_used_by_class_context(Constraints, !Info),
    list__foldl(find_items_used_by_type_and_mode, ArgTypesAndModes, !Info).
find_items_used_by_class_method(Method, !Info) :-
    Method = pred_or_func_mode(_, _, _, Modes, _, _, _, _),
    find_items_used_by_modes(Modes, !Info).

:- pred find_items_used_by_type_and_mode(type_and_mode::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_and_mode(TypeAndMode, !Info) :-
    (
        TypeAndMode = type_only(Type)
    ;
        TypeAndMode = type_and_mode(Type, Mode),
        find_items_used_by_mode(Mode, !Info)
    ),
    find_items_used_by_type(Type, !Info).

:- pred find_items_used_by_type_body(hlds_type_body::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_body(TypeBody, !Info) :-
    Ctors = TypeBody ^ du_type_ctors,
    list__foldl(find_items_used_by_ctor, Ctors, !Info).
find_items_used_by_type_body(eqv_type(Type), !Info) :-
    find_items_used_by_type(Type, !Info).
find_items_used_by_type_body(abstract_type(_), !Info).
find_items_used_by_type_body(foreign_type(_), !Info).
    % rafe: XXX Should we trace the representation type?
find_items_used_by_type_body(solver_type(_, _), !Info).

:- pred find_items_used_by_ctor(constructor::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_ctor(Ctor, !Info) :-
    Ctor = ctor(_, Constraints, _, CtorArgs),
    find_items_used_by_class_constraints(Constraints, !Info),
    list__foldl(find_items_used_by_ctor_arg, CtorArgs, !Info).

:- pred find_items_used_by_ctor_arg(constructor_arg::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_ctor_arg(CtorArg, !Info) :-
    CtorArg = _ - ArgType,
    find_items_used_by_type(ArgType, !Info).

:- pred find_items_used_by_mode_defn(hlds_mode_defn::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_mode_defn(Defn, !Info) :-
    Defn = hlds_mode_defn(_, _, eqv_mode(Mode), _, _),
    find_items_used_by_mode(Mode, !Info).

:- pred find_items_used_by_inst_defn(hlds_inst_defn::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_inst_defn(Defn, !Info) :-
    Defn = hlds_inst_defn(_, _, InstBody, _, _),
    (
        InstBody = eqv_inst(Inst),
        find_items_used_by_inst(Inst, !Info)
    ;
        InstBody = abstract_inst
    ).

:- pred find_items_used_by_preds(pred_or_func::in, pred_or_func_set::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_preds(PredOrFunc, Set, !Info) :-
    map__foldl(find_items_used_by_preds_2(PredOrFunc), Set, !Info).

:- pred find_items_used_by_preds_2(pred_or_func::in,
    pair(string, arity)::in, map(module_qualifier, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_preds_2(PredOrFunc, Name - Arity, MatchingPredMap, !Info) :-
    map__foldl(find_items_used_by_preds_3(
        PredOrFunc, Name, Arity), MatchingPredMap, !Info).

:- pred find_items_used_by_preds_3(pred_or_func::in,
    string::in, arity::in, module_qualifier::in, module_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_preds_3(PredOrFunc, Name, Arity, ModuleQualifier, _,
        !Info) :-
    SymName = module_qualify_name(ModuleQualifier, Name),
    record_used_pred_or_func(PredOrFunc, SymName - Arity, !Info).

:- pred find_items_used_by_pred(pred_or_func::in,
    pair(string, arity)::in, pair(pred_id, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_pred(PredOrFunc, Name - Arity, PredId - PredModule,
        !Info) :-
    ItemType = pred_or_func_to_item_type(PredOrFunc),
    ModuleInfo = !.Info ^ module_info,
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    (
        ItemId = qualified(PredModule, Name) - Arity,
        (
            item_is_recorded_used(!.Info, ItemType, ItemId)
        ;
            item_is_local(!.Info, ItemId)
        )
    ->
        % We've already recorded the items used by this predicate.
        true
    ;
        % Items used by class methods are recorded when processing
        % the typeclass declaration. Make sure that is done.
        pred_info_get_markers(PredInfo, Markers),
        check_marker(Markers, class_method)
    ->
        % The typeclass for which the predicate is a method is the first
        % of the universal class constraints in the pred_info.
        pred_info_get_class_context(PredInfo, MethodClassContext),
        MethodClassContext = constraints(MethodUnivConstraints, _),
        (
            MethodUnivConstraints = [constraint(ClassName0, ClassArgs) | _]
        ->
            ClassName = ClassName0,
            ClassArity = list__length(ClassArgs)
        ;
            unexpected(this_file, "class method with no class constraints")
        ),
        maybe_record_item_to_process(typeclass_item, ClassName - ClassArity,
            !Info)
    ;
        NameArity = qualified(PredModule, Name) - Arity,
        record_expanded_items_used_by_item(ItemType, NameArity, !Info),
        record_imported_item(ItemType, NameArity, !Info),
        pred_info_arg_types(PredInfo, ArgTypes),
        find_items_used_by_types(ArgTypes, !Info),
        pred_info_procedures(PredInfo, Procs),
        map__foldl(find_items_used_by_proc_arg_modes, Procs, !Info),
        pred_info_get_class_context(PredInfo, ClassContext),
        find_items_used_by_class_context(ClassContext, !Info),

        % Record items used by `:- pragma type_spec' declarations.
        module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
        TypeSpecInfo = type_spec_info(_, _, _, PragmaMap),
        ( map__search(PragmaMap, PredId, TypeSpecPragmas) ->
            list__foldl(find_items_used_by_type_spec, TypeSpecPragmas, !Info)
        ;
            true
        )
    ).

:- pred find_items_used_by_proc_arg_modes(proc_id::in, proc_info::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_proc_arg_modes(_ProcId, ProcInfo, !Info) :-
    proc_info_argmodes(ProcInfo, ArgModes),
    find_items_used_by_modes(ArgModes, !Info).

:- pred find_items_used_by_type_spec(pragma_type::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type_spec(Pragma, !Info) :-
    ( Pragma = type_spec(_, _, _, _, MaybeModes, Subst, _, _) ->
        (
            MaybeModes = yes(Modes),
            find_items_used_by_modes(Modes, !Info)
        ;
            MaybeModes = no
        ),
        assoc_list__values(Subst, SubstTypes),
        find_items_used_by_types(SubstTypes, !Info)
    ;
        unexpected(this_file,
            "find_items_used_by_type_spec: unexpected pragma type")
    ).

:- pred find_items_used_by_functors(functor_set::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functors(Set, !Info) :-
    map__foldl(find_items_used_by_functors_2, Set, !Info).

:- pred find_items_used_by_functors_2(pair(string, arity)::in,
    map(module_qualifier, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functors_2(Name - Arity, MatchingCtorMap, !Info) :-
    map__foldl(find_items_used_by_functors_3(Name, Arity), MatchingCtorMap,
        !Info).

:- pred find_items_used_by_functors_3(string::in, arity::in,
    module_qualifier::in, module_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functors_3(Name, Arity, Qualifier, _, !Info) :-
    SymName = module_qualify_name(Qualifier, Name),
    record_used_functor(SymName - Arity, !Info).

:- pred find_items_used_by_functor(string::in, arity::in, resolved_functor::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_functor(Name, _Arity, ResolverFunctor, !Info) :-
    ResolverFunctor = pred_or_func(PredId, PredModule, PredOrFunc, PredArity),
    find_items_used_by_pred(PredOrFunc, Name - PredArity, PredId - PredModule,
        !Info).
find_items_used_by_functor(_, _, ResolverFunctor, !Info) :-
    ResolverFunctor = constructor(TypeCtor),
    maybe_record_item_to_process(type_body_item, TypeCtor, !Info).
find_items_used_by_functor(_, _, ResolverFunctor, !Info) :-
    ResolverFunctor = field(TypeCtor, _),
    maybe_record_item_to_process(type_body_item, TypeCtor, !Info).

:- pred find_items_used_by_simple_item_set(item_type::in, simple_item_set::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_simple_item_set(ItemType, Set, !Info) :-
    map__foldl(find_items_used_by_simple_item_set_2(ItemType), Set, !Info).

:- pred find_items_used_by_simple_item_set_2(item_type::in,
    pair(string, arity)::in, map(module_qualifier, module_name)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_simple_item_set_2(ItemType, Name - Arity, MatchingIdMap,
        !Info) :-
    map__foldl(find_items_used_by_simple_item_set_3(ItemType, Name, Arity),
        MatchingIdMap, !Info).

:- pred find_items_used_by_simple_item_set_3(item_type::in,
    string::in, arity::in, module_qualifier::in, module_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_simple_item_set_3(ItemType, Name, Arity, _, Module,
        !Info) :-
    maybe_record_item_to_process(ItemType, qualified(Module, Name) - Arity,
        !Info).

:- pred find_items_used_by_types(list(mer_type)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_types(Types, !Info) :-
    list__foldl(find_items_used_by_type, Types, !Info).

:- pred find_items_used_by_type(mer_type::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_type(Type, !Info) :-
    ( type_to_ctor_and_args(Type, TypeCtor, TypeArgs) ->
        (
            % Unqualified type-ids are builtin types.
            TypeCtor = qualified(_, _) - _,
            \+ type_ctor_is_higher_order(TypeCtor, _, _, _)
        ->
            maybe_record_item_to_process(type_item, TypeCtor, !Info)
        ;
            true
        ),
        find_items_used_by_types(TypeArgs, !Info)
    ;
        true
    ).

:- pred find_items_used_by_modes(list(mer_mode)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_modes(Modes, !Info) :-
    list__foldl(find_items_used_by_mode, Modes, !Info).

:- pred find_items_used_by_mode(mer_mode::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_mode((Inst1 -> Inst2), !Info) :-
    find_items_used_by_inst(Inst1, !Info),
    find_items_used_by_inst(Inst2, !Info).
find_items_used_by_mode(user_defined_mode(ModeName, ArgInsts), !Info) :-
    list__length(ArgInsts, ModeArity),
    maybe_record_item_to_process(mode_item, ModeName - ModeArity, !Info),
    find_items_used_by_insts(ArgInsts, !Info).

:- pred find_items_used_by_insts(list(mer_inst)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_insts(Modes, !Info) :-
    list__foldl(find_items_used_by_inst, Modes, !Info).

:- pred find_items_used_by_inst(mer_inst::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_inst(any(_), !Info).
find_items_used_by_inst(free, !Info).
find_items_used_by_inst(free(_), !Info).
find_items_used_by_inst(bound(_, BoundInsts), !Info) :-
    list__foldl(find_items_used_by_bound_inst, BoundInsts, !Info).
find_items_used_by_inst(ground(_, GroundInstInfo), !Info) :-
    (
        GroundInstInfo = higher_order(pred_inst_info(_, Modes, _)),
        find_items_used_by_modes(Modes, !Info)
    ;
        GroundInstInfo = none
    ).
find_items_used_by_inst(not_reached, !Info).
find_items_used_by_inst(inst_var(_), !Info).
find_items_used_by_inst(constrained_inst_vars(_, Inst), !Info) :-
    find_items_used_by_inst(Inst, !Info).
find_items_used_by_inst(defined_inst(InstName), !Info) :-
    find_items_used_by_inst_name(InstName, !Info).
find_items_used_by_inst(abstract_inst(Name, ArgInsts), !Info) :-
    list__length(ArgInsts, Arity),
    maybe_record_item_to_process(inst_item, Name - Arity, !Info),
    find_items_used_by_insts(ArgInsts, !Info).

:- pred find_items_used_by_bound_inst(bound_inst::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_bound_inst(BoundInst, !Info) :-
    BoundInst = functor(ConsId, ArgInsts),
    ( ConsId = cons(Name, Arity) ->
        record_used_functor(Name - Arity, !Info)
    ;
        true
    ),
    find_items_used_by_insts(ArgInsts, !Info).

:- pred find_items_used_by_inst_name(inst_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_inst_name(user_inst(Name, ArgInsts), !Info) :-
    list__length(ArgInsts, Arity),
    maybe_record_item_to_process(inst_item, Name - Arity, !Info),
    find_items_used_by_insts(ArgInsts, !Info).
find_items_used_by_inst_name(merge_inst(Inst1, Inst2), !Info) :-
    find_items_used_by_inst(Inst1, !Info),
    find_items_used_by_inst(Inst2, !Info).
find_items_used_by_inst_name(unify_inst(_, Inst1, Inst2, _), !Info) :-
    find_items_used_by_inst(Inst1, !Info),
    find_items_used_by_inst(Inst2, !Info).
find_items_used_by_inst_name(ground_inst(InstName, _, _, _), !Info) :-
    find_items_used_by_inst_name(InstName, !Info).
find_items_used_by_inst_name(any_inst(InstName, _, _, _), !Info) :-
    find_items_used_by_inst_name(InstName, !Info).
find_items_used_by_inst_name(shared_inst(InstName), !Info) :-
    find_items_used_by_inst_name(InstName, !Info).
find_items_used_by_inst_name(mostly_uniq_inst(InstName), !Info) :-
    find_items_used_by_inst_name(InstName, !Info).
find_items_used_by_inst_name(typed_ground(_, Type), !Info) :-
    find_items_used_by_type(Type, !Info).
find_items_used_by_inst_name(typed_inst(Type, InstName), !Info) :-
    find_items_used_by_type(Type, !Info),
    find_items_used_by_inst_name(InstName, !Info).

:- pred find_items_used_by_class_context(prog_constraints::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_context(constraints(Constraints1, Constraints2),
        !Info) :-
    find_items_used_by_class_constraints(Constraints1, !Info),
    find_items_used_by_class_constraints(Constraints2, !Info).

:- pred find_items_used_by_class_constraints(list(prog_constraint)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_constraints(Constraints, !Info) :-
    list__foldl(find_items_used_by_class_constraint, Constraints, !Info).

:- pred find_items_used_by_class_constraint(prog_constraint::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

find_items_used_by_class_constraint(constraint(ClassName, ArgTypes), !Info) :-
    ClassArity = list__length(ArgTypes),
    maybe_record_item_to_process(typeclass_item, ClassName - ClassArity,
        !Info),
    find_items_used_by_types(ArgTypes, !Info).

:- pred maybe_record_item_to_process(item_type::in, pair(sym_name, arity)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

maybe_record_item_to_process(ItemType, NameArity, !Info) :-
    ( ItemType = typeclass_item ->
        Classes0 = !.Info ^ used_typeclasses,
        set__insert(Classes0, NameArity, Classes),
        !:Info = !.Info ^ used_typeclasses := Classes
    ;
        true
    ),

    (
        item_is_recorded_used(!.Info, ItemType, NameArity)
    ->
        % This item has already been recorded.
        true
    ;
        item_is_local(!.Info, NameArity)
    ->
        % Ignore local items. The items used by them have already been recorded
        % by module_qual.m.
        true
    ;
        Queue0 = !.Info ^ item_queue,
        queue__put(Queue0, item_id(ItemType, NameArity), Queue),
        !:Info = !.Info ^ item_queue := Queue,

        record_imported_item(ItemType, NameArity, !Info),
        record_expanded_items_used_by_item(ItemType, NameArity, !Info)
    ).

:- pred item_is_recorded_used(recompilation_usage_info::in, item_type::in,
    pair(sym_name, arity)::in) is semidet.

item_is_recorded_used(Info, ItemType, NameArity) :-
    ImportedItems = Info ^ imported_items,
    NameArity = qualified(ModuleName, Name) - Arity,
    map__search(ImportedItems, ModuleName, ModuleIdSet),
    ModuleItemIdSet = extract_ids(ModuleIdSet, ItemType),
    set__member(Name - Arity, ModuleItemIdSet).

:- pred item_is_local(recompilation_usage_info::in,
    pair(sym_name, arity)::in) is semidet.

item_is_local(Info, NameArity) :-
    NameArity = qualified(ModuleName, _) - _,
    module_info_get_name(Info ^ module_info, ModuleName).

:- pred record_imported_item(item_type::in,
    pair(sym_name, arity)::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_imported_item(ItemType, SymName - Arity, !Info) :-
    ( SymName = qualified(Module0, Name0) ->
        Module = Module0,
        Name = Name0
    ;
        unexpected(this_file, "maybe_record_item_to_process: unqualified item")
    ),

    ImportedItems0 = !.Info ^ imported_items,
    ( map__search(ImportedItems0, Module, ModuleItems0) ->
        ModuleItems1 = ModuleItems0
    ;
        ModuleItems1 = init_item_id_set(set__init)
    ),
    ModuleItemIds0 = extract_ids(ModuleItems1, ItemType),
    set__insert(ModuleItemIds0, Name - Arity, ModuleItemIds),
    ModuleItems = update_ids(ModuleItems1, ItemType, ModuleItemIds),
    map__set(ImportedItems0, Module, ModuleItems, ImportedItems),
    !:Info = !.Info ^ imported_items := ImportedItems.

    % Uses of equivalence types have been expanded away by equiv_type.m.
    % equiv_type.m records which equivalence types were used by each
    % imported item.
:- pred record_expanded_items_used_by_item(item_type::in, item_name::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_expanded_items_used_by_item(ItemType, NameArity, !Info) :-
    Dependencies = !.Info ^ dependencies,
    (
        map__search(Dependencies, item_id(ItemType, NameArity), EquivTypes)
    ->
        list__foldl(record_expanded_items_used_by_item_2,
            set__to_sorted_list(EquivTypes), !Info)
    ;
        true
    ).

:- pred record_expanded_items_used_by_item_2(item_id::in,
    recompilation_usage_info::in, recompilation_usage_info::out) is det.

record_expanded_items_used_by_item_2(Item, !Info) :-
    Item = item_id(DepItemType, DepItemId),
    maybe_record_item_to_process(DepItemType, DepItemId, !Info).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "recompilation.usage.m".

%-----------------------------------------------------------------------------%
