%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: recompilation.usage.m
% Main author: stayl
%
% Write the file recording which imported items were used by a compilation.
%-----------------------------------------------------------------------------%
:- module recompilation__usage.

:- interface.

:- import_module hlds__hlds_module, hlds__hlds_pred, parse_tree__modules.
:- import_module parse_tree__prog_data.

:- import_module assoc_list, io, list, map, set, std_util.

	%
	% The resolved_used_items records the possible matches
	% for a program item. It is used by recompilation_check.m
	% to work out whether a new item could cause ambiguity with
	% an item which was used during a compilation.
	%
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

	% The list is sorted on arity.
	% This is useful because when determining whether
	% there is an ambiguity we need to test a predicate or
	% function against all used functors with equal or
	% lower arity.
:- type resolved_item_list(T) == assoc_list(arity, resolved_item_map(T)).

:- type resolved_item_map(T) == map(module_qualifier, T).

:- type resolved_functor
	--->	pred_or_func(
			pred_id,
			module_name,
			pred_or_func,
			arity		% The actual arity of the
					% predicate or function
		)
	;	constructor(
			item_name	% type_ctor
		)
	;	field(
			item_name,	% type_ctor
			item_name	% cons_id
		)
	.

:- pred recompilation__usage__write_usage_file(module_info::in,
		list(module_name)::in, maybe(module_timestamps)::in,
		io__state::di, io__state::uo) is det.

	% Changes which modify the format of the `.used' files will
	% increment this number. recompilation_check.m should recompile
	% if the version number is out of date.
:- func usage_file_version_number = int.

%-----------------------------------------------------------------------------%
:- implementation.

:- import_module hlds__hlds_data, hlds__hlds_pred, parse_tree__prog_util.
:- import_module check_hlds__type_util, (parse_tree__inst).
:- import_module hlds__hlds_out, parse_tree__mercury_to_mercury.
:- import_module hlds__passes_aux, parse_tree__prog_data.
:- import_module libs__globals, libs__options.
:- import_module recompilation__version, libs__timestamp.

:- import_module assoc_list, bool, int, require.
:- import_module queue, std_util, string.

recompilation__usage__write_usage_file(ModuleInfo, NestedSubModules,
		MaybeTimestamps) -->
	{ module_info_get_maybe_recompilation_info(ModuleInfo,
		MaybeRecompInfo) },
	(
		{ MaybeRecompInfo = yes(RecompInfo) },
		{ MaybeTimestamps = yes(Timestamps) }
	->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose,
	"% Writing recompilation compilation dependency information\n"),

		{ module_info_name(ModuleInfo, ModuleName) },
		module_name_to_file_name(ModuleName, ".used", yes, FileName),
		io__open_output(FileName, FileResult),
		(
			{ FileResult = ok(Stream0) },
			io__set_output_stream(Stream0, OldStream),
			recompilation__usage__write_usage_file_2(ModuleInfo,
				NestedSubModules, RecompInfo, Timestamps),
			io__set_output_stream(OldStream, Stream),
			io__close_output(Stream)
		;
			{ FileResult = error(IOError) },
			{ io__error_message(IOError, IOErrorMessage) },
			io__write_string("\nError opening `"),
			io__write_string(FileName),
			io__write_string("'for output: "),
			io__write_string(IOErrorMessage),
			io__write_string(".\n"),
			io__set_exit_status(1)
		)
	;
		[]
	).

:- pred recompilation__usage__write_usage_file_2(module_info::in,
		list(module_name)::in, recompilation_info::in,
		module_timestamps::in, io__state::di, io__state::uo) is det.

recompilation__usage__write_usage_file_2(ModuleInfo, NestedSubModules,
		RecompInfo, Timestamps) -->
	io__write_int(usage_file_version_number),
	io__write_string(","),
	io__write_int(version_numbers_version_number),
	io__write_string(".\n\n"),

	{ module_info_name(ModuleInfo, ThisModuleName) },
	{ map__lookup(Timestamps, ThisModuleName,
		module_timestamp(_, ThisModuleTimestamp, _)) },
	io__write_string("("),
	mercury_output_bracketed_sym_name(ThisModuleName),
	io__write_string(", "".m"", "),
	write_version_number(ThisModuleTimestamp),
	io__write_string(").\n\n"),

	( { NestedSubModules = [] } ->
		io__write_string("sub_modules.\n\n")
	;
		io__write_string("sub_modules("),
		io__write_list(NestedSubModules, ", ",
			mercury_output_bracketed_sym_name),
		io__write_string(").\n\n")
	),

	{ UsedItems = RecompInfo ^ used_items },
	{ recompilation__usage__find_all_used_imported_items(ModuleInfo,
		UsedItems, RecompInfo ^ dependencies, ResolvedUsedItems,
		UsedClasses, ImportedItems, ModuleInstances) },

	( { UsedItems = init_used_items } ->
		io__write_string("used_items.\n")
	;
		io__write_string("used_items(\n\t"),
		{ WriteComma0 = no },
		write_simple_item_matches((type), ResolvedUsedItems,
				WriteComma0, WriteComma1),
		write_simple_item_matches(type_body, ResolvedUsedItems,
				WriteComma1, WriteComma2),
		write_simple_item_matches((mode), ResolvedUsedItems,
				WriteComma2, WriteComma3),
		write_simple_item_matches((inst), ResolvedUsedItems,
				WriteComma3, WriteComma4),
		write_simple_item_matches((typeclass), ResolvedUsedItems,
				WriteComma4, WriteComma5),
		write_pred_or_func_matches((predicate), ResolvedUsedItems,
				WriteComma5, WriteComma6),
		write_pred_or_func_matches((function), ResolvedUsedItems,
				WriteComma6, WriteComma7),
		write_functor_matches(ResolvedUsedItems ^ functors,
				WriteComma7, _),
		io__write_string("\n).\n\n")
	),

	( { set__empty(UsedClasses) } ->
		io__write_string("used_classes.\n")
	;
		io__write_string("used_classes("),
		io__write_list(set__to_sorted_list(UsedClasses), ", ",
		    (pred((ClassName - ClassArity)::in, di, uo) is det -->
			mercury_output_bracketed_sym_name(ClassName),
			io__write_string("/"),
			io__write_int(ClassArity)
		    )),
		io__write_string(").\n")
	),

	map__foldl(
	    (pred(ModuleName::in, ModuleUsedItems::in, di, uo) is det -->	
		io__nl,
		io__write_string("("),
		mercury_output_bracketed_sym_name(ModuleName),
		io__write_string(", """),
		{ map__lookup(Timestamps, ModuleName,
			module_timestamp(Suffix, ModuleTimestamp,
				NeedQualifier)) },
		io__write_string(Suffix),
		io__write_string(""", "),
		write_version_number(ModuleTimestamp),
		( { NeedQualifier = must_be_qualified } ->
			io__write_string(", used)")
		;
			io__write_string(")")
		),
		(
			% XXX We don't yet record all uses of items
			% from these modules in polymorphism.m, etc.
			\+ { any_mercury_builtin_module(ModuleName) },
			{ map__search(RecompInfo ^ version_numbers,
				ModuleName, ModuleVersions) }
		->
			%
			% Select out from the version numbers of all items
			% in the imported module the ones which are used.
			%

			{ ModuleVersions = version_numbers(ModuleItemVersions,
						ModuleInstanceVersions) },
			{ ModuleUsedItemVersions = map_ids(
				(func(ItemType, Ids0) = Ids :-
					ModuleItemNames = extract_ids(
						ModuleUsedItems, ItemType),
					map__select(Ids0, ModuleItemNames, Ids)
				),
				ModuleItemVersions,
				map__init) },

			{
				map__search(ModuleInstances, ModuleName,
					ModuleUsedInstances)
			->
				map__select(ModuleInstanceVersions,
					ModuleUsedInstances,
					ModuleUsedInstanceVersions)
			;
				map__init(ModuleUsedInstanceVersions)
			},

			io__write_string(" => "),
			{ ModuleUsedVersionNumbers =
				version_numbers(ModuleUsedItemVersions,
						ModuleUsedInstanceVersions) },
			recompilation__version__write_version_numbers(
				ModuleUsedVersionNumbers),
			io__write_string(".\n")
	   	;
			% If we don't have version numbers for a module
			% we just recompile if the interface file's
			% timestamp changes.
			io__write_string(".\n")
		)
	    ), ImportedItems),

	%
	% recompilation_check.m checks for this item when reading
	% in the `.used' file to make sure the earlier compilation
	% wasn't interrupted in the middle of writing the file.
	%
	io__nl,
	io__write_string("done.\n").

:- pred write_simple_item_matches(item_type::in(simple_item),
		resolved_used_items::in, bool::in, bool::out,
		io__state::di, io__state::uo) is det.

write_simple_item_matches(ItemType, UsedItems, WriteComma0, WriteComma) -->
	{ Ids = extract_simple_item_set(UsedItems, ItemType) },
	( { map__is_empty(Ids) } ->
		{ WriteComma = WriteComma0 }
	;
		( { WriteComma0 = yes } ->
			io__write_string(",\n\t")
		;
			[]
		),
		{ WriteComma = yes },
		write_simple_item_matches_2(ItemType, Ids)
	).

:- pred write_simple_item_matches_2(item_type::in, simple_item_set::in,
		io__state::di, io__state::uo) is det.

write_simple_item_matches_2(ItemType, ItemSet) -->
	{ string_to_item_type(ItemTypeStr, ItemType) },
	io__write_string(ItemTypeStr),
	io__write_string("(\n\t\t"),
	{ map__to_assoc_list(ItemSet, ItemList) },
	io__write_list(ItemList, ",\n\t\t",
	    (pred(((Name - Arity) - Matches)::in, di, uo) is det -->
		mercury_output_bracketed_sym_name(unqualified(Name),
			next_to_graphic_token),
		io__write_string("/"),
		io__write_int(Arity),
		io__write_string(" - ("),
		{ map__to_assoc_list(Matches, MatchList) },
		io__write_list(MatchList, ", ",
		    (pred((Qualifier - ModuleName)::in, di, uo) is det -->
			mercury_output_bracketed_sym_name(Qualifier),
			( { Qualifier = ModuleName } ->
				[]
			;
				io__write_string(" => "),
				mercury_output_bracketed_sym_name(ModuleName)
		    	)
		    )
		),
		io__write_string(")")
	    )
	),
	io__write_string("\n\t)").

:- pred write_pred_or_func_matches(item_type::in(pred_or_func),
		resolved_used_items::in, bool::in, bool::out,
		io__state::di, io__state::uo) is det.

write_pred_or_func_matches(ItemType, UsedItems, WriteComma0, WriteComma) -->
	{ Ids = extract_pred_or_func_set(UsedItems, ItemType) },
	( { map__is_empty(Ids) } ->
		{ WriteComma = WriteComma0 }
	;
		( { WriteComma0 = yes } ->
			io__write_string(",\n\t")
		;
			[]
		),
		{ WriteComma = yes },
		write_pred_or_func_matches_2(ItemType, Ids)
	).

:- pred write_pred_or_func_matches_2(item_type::in(pred_or_func),
		resolved_pred_or_func_set::in,
		io__state::di, io__state::uo) is det.

write_pred_or_func_matches_2(ItemType, ItemSet) -->
	write_resolved_item_set(ItemType, ItemSet,
	    (pred((Qualifier - PredIdModuleNames)::in, di, uo) is det -->
		{ ModuleNames = assoc_list__values(set__to_sorted_list(
				PredIdModuleNames)) },
		mercury_output_bracketed_sym_name(Qualifier),
		( { ModuleNames = [Qualifier] } ->
			[]
		;
			io__write_string(" => ("),
			io__write_list(ModuleNames, ", ",
				mercury_output_bracketed_sym_name),
			io__write_string(")")
		)
	    )).

:- pred write_functor_matches(resolved_functor_set::in,
	bool::in, bool::out, io__state::di, io__state::uo) is det.

write_functor_matches(Ids, WriteComma0, WriteComma) -->
	( { map__is_empty(Ids) } ->
		{ WriteComma = WriteComma0 }
	;
		( { WriteComma0 = yes } ->
			io__write_string(",\n\t")
		;
			[]
		),
		{ WriteComma = yes },
		write_resolved_item_set(functor, Ids,
		    (pred((Qualifier - MatchingCtors)::in, di, uo) is det -->
			mercury_output_bracketed_sym_name(Qualifier),
			io__write_string(" => ("),
			io__write_list(
				set__to_sorted_list(MatchingCtors),
				", ", write_resolved_functor),
			io__write_string(")")
		    ))
	).

:- type write_resolved_item(T) ==
		pred(pair(module_qualifier, T), io__state, io__state).
:- inst write_resolved_item == (pred(in, di, uo) is det).

:- pred write_resolved_item_set(item_type::in, resolved_item_set(T)::in,
	write_resolved_item(T)::in(write_resolved_item),
	io__state::di, io__state::uo) is det.

write_resolved_item_set(ItemType, ItemSet, WriteMatches) -->
	{ string_to_item_type(ItemTypeStr, ItemType) },
	io__write_string(ItemTypeStr),
	io__write_string("(\n\t\t"),
	{ map__to_assoc_list(ItemSet, ItemList) },
	io__write_list(ItemList, ",\n\t\t",
	    (pred((Name - MatchesAL)::in, di, uo) is det -->
		mercury_output_bracketed_sym_name(unqualified(Name)),
		io__write_string(" - ("),
		io__write_list(MatchesAL, ",\n\t\t\t",
		    (pred((Arity - Matches)::in, di, uo) is det -->
			io__write_int(Arity),
			io__write_string(" - ("),
			{ map__to_assoc_list(Matches, MatchList) },
			io__write_list(MatchList, ",\n\t\t\t\t",
			    WriteMatches),
			io__write_string(")")
		    )),	
		io__write_string(")")
	    )),
	io__write_string("\n\t)").

:- pred write_resolved_functor(resolved_functor::in,
		io__state::di, io__state::uo) is det.

write_resolved_functor(pred_or_func(_, ModuleName, PredOrFunc, Arity)) -->
	io__write(PredOrFunc),
	io__write_string("("),
	mercury_output_bracketed_sym_name(ModuleName),
	io__write_string(", "),
	io__write_int(Arity),
	io__write_string(")").
write_resolved_functor(constructor(TypeName - Arity)) -->
	io__write_string("ctor("),
	mercury_output_bracketed_sym_name(TypeName, next_to_graphic_token),
	io__write_string("/"),
	io__write_int(Arity),
	io__write_string(")").
write_resolved_functor(
		field(TypeName - TypeArity, ConsName - ConsArity)) -->
	io__write_string("field("),
	mercury_output_bracketed_sym_name(TypeName, next_to_graphic_token),
	io__write_string("/"),
	io__write_int(TypeArity),
	io__write_string(", "),
	mercury_output_bracketed_sym_name(ConsName, next_to_graphic_token),
	io__write_string("/"),
	io__write_int(ConsArity),
	io__write_string(")").

usage_file_version_number = 2.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type recompilation_usage_info
	---> recompilation_usage_info(
		module_info :: module_info,
		item_queue :: queue(item_id),
		imported_items :: imported_items,
			% For each module, the used typeclasses for
			% which the module contains an instance.
		module_instances :: map(module_name, set(item_name)),
		dependencies :: map(item_id, set(item_id)),
		used_items :: resolved_used_items,
		used_typeclasses :: set(item_name)
	).

:- type imported_items == map(module_name, module_imported_items).

	% The constructors set should always be empty -
	% constructors are never imported separately.
:- type module_imported_items == item_id_set(imported_item_set).

:- type imported_item_set == set(pair(string, arity)).

%-----------------------------------------------------------------------------%

	%
	% Go over the set of imported items found to be used and
	% find the transitive closure of the imported items they use.
	%
:- pred recompilation__usage__find_all_used_imported_items(module_info::in,
	used_items::in, map(item_id, set(item_id))::in,
	resolved_used_items::out, set(item_name)::out, imported_items::out,
	map(module_name, set(item_name))::out) is det.

recompilation__usage__find_all_used_imported_items(ModuleInfo,
		UsedItems, Dependencies, ResolvedUsedItems, UsedTypeClasses,
		ImportedItems, ModuleInstances) :-

	%
	% We need to make sure each visible module has an entry in
	% the `.used' file, even if nothing was used from it.
	% This will cause recompilation_check.m to check for new items
	% causing ambiguity when the interface of the module changes.
	%
	map__init(ImportedItems0),
	ImportedItems2 = promise_only_solution(
	    (pred(ImportedItems1::out) is cc_multi :-
		std_util__unsorted_aggregate(
		    (pred(VisibleModule::out) is nondet :-
			visible_module(VisibleModule, ModuleInfo),
			\+ module_info_name(ModuleInfo, VisibleModule)
		    ),
		    (pred(VisibleModule::in, ImportedItemsMap0::in,
				ImportedItemsMap::out) is det :-
			ModuleItems = init_item_id_set(set__init),
			map__det_insert(ImportedItemsMap0, VisibleModule,
				ModuleItems, ImportedItemsMap)	
		    ),
		    ImportedItems0, ImportedItems1)
	    )),

	queue__init(ItemsToProcess0),
	map__init(ModuleUsedClasses),
	set__init(UsedClasses0),

	UsedItems = item_id_set(Types, TypeBodies, Modes, Insts, Classes,
			_, _, _),
	map__init(ResolvedCtors),
	map__init(ResolvedPreds),
	map__init(ResolvedFuncs),
	ResolvedUsedItems0 = item_id_set(Types, TypeBodies, Modes, Insts,
			Classes, ResolvedCtors, ResolvedPreds, ResolvedFuncs),

	Info0 = recompilation_usage_info(ModuleInfo, ItemsToProcess0,
		ImportedItems2, ModuleUsedClasses, Dependencies,
		ResolvedUsedItems0, UsedClasses0),

	recompilation__usage__find_all_used_imported_items_2(UsedItems,
		Info0, Info),

	ImportedItems = Info ^ imported_items,
	ModuleInstances = Info ^ module_instances,
	UsedTypeClasses = Info ^ used_typeclasses,
	ResolvedUsedItems = Info ^ used_items.

:- pred recompilation__usage__find_all_used_imported_items_2(used_items::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_all_used_imported_items_2(UsedItems) -->
		
	%
	% Find items used by imported instances for local classes.
	%
	ModuleInfo =^ module_info,
	{ module_info_instances(ModuleInfo, Instances) },
	map__foldl(
	    (pred(ClassId::in, InstanceDefns::in, in, out) is det -->
	    	{ ClassId = class_id(Name, Arity) },
		=(Info),
		{ NameArity = Name - Arity },
		( { item_is_local(Info, NameArity) } ->
		    recompilation__usage__record_expanded_items_used_by_item(
		    	(typeclass), NameArity),
		    list__foldl(
		    	recompilation__usage__find_items_used_by_instance(
				NameArity),
			InstanceDefns)
		;
			[]
		)
	    ), Instances),

	{ Predicates = UsedItems ^ predicates },
	recompilation__usage__find_items_used_by_preds(predicate, Predicates),

	{ Functions = UsedItems ^ functions },
	recompilation__usage__find_items_used_by_preds(function, Functions),
		
	{ Constructors = UsedItems ^ functors },
	recompilation__usage__find_items_used_by_functors(Constructors),

	{ Types = UsedItems ^ types },
	recompilation__usage__find_items_used_by_simple_item_set((type), Types),

	{ TypeBodies = UsedItems ^ type_bodies },
	recompilation__usage__find_items_used_by_simple_item_set((type_body),
		TypeBodies),

	{ Modes = UsedItems ^ modes },
	recompilation__usage__find_items_used_by_simple_item_set((mode), Modes),

	{ Classes = UsedItems ^ typeclasses },
	recompilation__usage__find_items_used_by_simple_item_set((typeclass),
		Classes),

	{ Insts = UsedItems ^ insts },
	recompilation__usage__find_items_used_by_simple_item_set((inst), Insts),

	recompilation__usage__process_imported_item_queue.

:- pred recompilation__usage__process_imported_item_queue(
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__process_imported_item_queue -->
	Queue0 =^ item_queue,
	^ item_queue := queue__init,
	recompilation__usage__process_imported_item_queue_2(Queue0),
	Queue =^ item_queue,
	( { queue__is_empty(Queue) } ->
		[]
	;
		recompilation__usage__process_imported_item_queue
	).

:- pred recompilation__usage__process_imported_item_queue_2(
		queue(item_id)::in, recompilation_usage_info::in,
		recompilation_usage_info::out) is det.

recompilation__usage__process_imported_item_queue_2(Queue0) -->
	( { queue__get(Queue0, Item, Queue) } ->
		{ Item = item_id(ItemType, ItemId) },
		recompilation__usage__find_items_used_by_item(ItemType, ItemId),
		recompilation__usage__process_imported_item_queue_2(Queue)
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred recompilation__usage__record_used_pred_or_func(pred_or_func::in,
	pair(sym_name, arity)::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__record_used_pred_or_func(PredOrFunc, Id) -->
	{ ItemType = pred_or_func_to_item_type(PredOrFunc) },
	ItemSet0 =^ used_items,
	{ IdSet0 = extract_pred_or_func_set(ItemSet0, ItemType) },
	{ Id = SymName - Arity },
	record_resolved_item(SymName, Arity,
		recompilation__usage__do_record_used_pred_or_func(PredOrFunc),
		IdSet0, IdSet),
	{ ItemSet = update_pred_or_func_set(ItemSet0, ItemType, IdSet) },
	^ used_items := ItemSet.

:- pred recompilation__usage__do_record_used_pred_or_func(pred_or_func::in,
	module_qualifier::in, sym_name::in, arity::in, bool::out,
	resolved_pred_or_func_map::in, resolved_pred_or_func_map::out,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__do_record_used_pred_or_func(PredOrFunc, ModuleQualifier,
		SymName, Arity, Recorded, MatchingNames0, MatchingNames) -->
	ModuleInfo =^ module_info,
	(
		{ module_info_get_predicate_table(ModuleInfo, PredTable) },
		{ adjust_func_arity(PredOrFunc, OrigArity, Arity) },
		{ predicate_table_search_pf_sym_arity(PredTable,
			PredOrFunc, SymName, OrigArity, MatchingPredIds) }
	->
		{ Recorded = yes },
		{ PredModules = set__list_to_set(list__map(
			(func(PredId) = PredId - PredModule :-
				module_info_pred_info(ModuleInfo,
					PredId, PredInfo),
				pred_info_module(PredInfo, PredModule)
			),
			MatchingPredIds)) },
		{ map__det_insert(MatchingNames0, ModuleQualifier,
			PredModules, MatchingNames) },
		{ unqualify_name(SymName, Name) },
		set__fold(
			recompilation__usage__find_items_used_by_pred(
		    		PredOrFunc, Name - Arity),
			PredModules)
	;
		{ Recorded = no },
		{ MatchingNames = MatchingNames0 }
	).

%-----------------------------------------------------------------------------%

:- pred recompilation__usage__record_used_functor(pair(sym_name, arity)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__record_used_functor(SymName - Arity) -->
	ItemSet0 =^ used_items,
	{ IdSet0 = ItemSet0 ^ functors },
	record_resolved_item(SymName, Arity,
		recompilation__usage__do_record_used_functor,
		IdSet0, IdSet),
	{ ItemSet = ItemSet0 ^ functors := IdSet },
	^ used_items := ItemSet.

:- pred recompilation__usage__do_record_used_functor(module_qualifier::in,
	sym_name::in, arity::in, bool::out, resolved_functor_map::in,
	resolved_functor_map::out, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__do_record_used_functor(ModuleQualifier, SymName, Arity,
		Recorded, ResolvedCtorMap0, ResolvedCtorMap) -->
	ModuleInfo =^ module_info,

	{ recompilation__usage__find_matching_functors(ModuleInfo,
		SymName, Arity, MatchingCtors) },
	{ unqualify_name(SymName, Name) },
	set__fold(
		recompilation__usage__find_items_used_by_functor(
			Name, Arity),
		MatchingCtors),
	
	{ set__empty(MatchingCtors) ->
		Recorded = no,
		ResolvedCtorMap = ResolvedCtorMap0
	;
		Recorded = yes,
		map__det_insert(ResolvedCtorMap0, ModuleQualifier,
			MatchingCtors, ResolvedCtorMap)
	}.

:- pred recompilation__usage__find_matching_functors(module_info::in,
	sym_name::in, arity::in, set(resolved_functor)::out) is det.

recompilation__usage__find_matching_functors(ModuleInfo, SymName, Arity,
		ResolvedConstructors) :-
	%
	% Is it a constructor.
	%
	module_info_ctors(ModuleInfo, Ctors),
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

	%
	% Is it a higher-order term or function call.
	%
	module_info_get_predicate_table(ModuleInfo, PredicateTable),
	( predicate_table_search_sym(PredicateTable, SymName, PredIds) ->
		MatchingPreds = list__filter_map(
			recompilation__usage__get_pred_or_func_ctors(ModuleInfo,
				SymName, Arity),
			PredIds)
	;
		MatchingPreds = []
	),

	%
	% Is it a field access function.
	%
	(
		is_field_access_function_name(ModuleInfo, SymName, Arity,
			_, FieldName),
		module_info_ctor_field_table(ModuleInfo, CtorFields),
		map__search(CtorFields, FieldName, FieldDefns)
	->
		MatchingFields = list__map(
			(func(FieldDefn) = FieldCtor :-
				FieldDefn = hlds_ctor_field_defn(_, _,
						TypeCtor, ConsId, _),
				( ConsId = cons(ConsName, ConsArity) ->
					FieldCtor = field(TypeCtor,
						ConsName - ConsArity)
				;	
					error(
					"weird cons_id in hlds_field_defn")
				)
			), FieldDefns)
	;
		MatchingFields = []
	),

	ResolvedConstructors = set__list_to_set(list__condense(
		[MatchingConstructors, MatchingPreds, MatchingFields])
	). 

:- func recompilation__usage__get_pred_or_func_ctors(module_info, sym_name,
		arity, pred_id) = resolved_functor is semidet.

recompilation__usage__get_pred_or_func_ctors(ModuleInfo, _SymName, Arity,
		PredId) = ResolvedCtor :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),	
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	pred_info_get_exist_quant_tvars(PredInfo, PredExistQVars),
	pred_info_arity(PredInfo, PredArity),
	adjust_func_arity(PredOrFunc, OrigArity, PredArity),
	(
		PredOrFunc = predicate,
		OrigArity >= Arity,
		% We don't support first-class polymorphism,
		% so you can't take the address of an existentially
		% quantified predicate.
		PredExistQVars = []
	;
		PredOrFunc = function,
		OrigArity >= Arity,
		% We don't support first-class polymorphism,
		% so you can't take the address of an existentially
		% quantified function.  You can however call such
		% a function, so long as you pass *all* the parameters.
		( PredExistQVars = []
		; OrigArity = Arity
		)
	),
	pred_info_module(PredInfo, PredModule),
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

record_resolved_item(SymName, Arity, RecordItem, IdSet0, IdSet) -->
	{ unqualify_name(SymName, UnqualifiedName) },
	{ ModuleQualifier = find_module_qualifier(SymName) },
	{ map__search(IdSet0, UnqualifiedName, MatchingNames0) ->
		MatchingNames1 = MatchingNames0
	;
		MatchingNames1 = []
	},
	recompilation__usage__record_resolved_item_2(ModuleQualifier, SymName,
		Arity, RecordItem, Recorded,
		MatchingNames1, MatchingNames),
	{ Recorded = yes ->
		map__set(IdSet0, UnqualifiedName, MatchingNames, IdSet)
	;
		IdSet = IdSet0
	}.

:- pred recompilation__usage__record_resolved_item_2(
	module_qualifier::in, sym_name::in, arity::in,
	record_resolved_item(T)::in(record_resolved_item),
	bool::out, resolved_item_list(T)::in, resolved_item_list(T)::out,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__record_resolved_item_2(ModuleQualifier,
		SymName, Arity, RecordItem, Recorded, [], List) -->
	{ map__init(Map0) },
	recompilation__usage__record_resolved_item_3(ModuleQualifier,
		SymName, Arity, RecordItem, Recorded, Map0, Map),
	{ Recorded = yes ->
		List = [Arity - Map]
	;
		List = []
	}.
recompilation__usage__record_resolved_item_2(ModuleQualifier,
		SymName, Arity, RecordItem, Recorded, List0, List) -->
	{ List0 = [ThisArity - ArityMap0 | ListRest0] },
	( { Arity < ThisArity } ->
		{ map__init(NewArityMap0) },
		recompilation__usage__record_resolved_item_3(ModuleQualifier,
			SymName, Arity, RecordItem, Recorded,
			NewArityMap0, NewArityMap),
		{ Recorded = yes ->
			List = [Arity - NewArityMap | List0]
		;
			List = List0
		}
	; { Arity = ThisArity } ->
		recompilation__usage__record_resolved_item_3(ModuleQualifier,
			SymName, Arity, RecordItem, Recorded,
			ArityMap0, ArityMap),
		{ Recorded = yes ->
			List = [Arity - ArityMap | ListRest0]
		;
			List = List0
		}
	;
		recompilation__usage__record_resolved_item_2(ModuleQualifier,
			SymName, Arity, RecordItem, Recorded,
			ListRest0, ListRest),
		{ Recorded = yes ->
			List = [ThisArity - ArityMap0 | ListRest]
		;
			List = List0
		}
	).

:- pred recompilation__usage__record_resolved_item_3(
	module_qualifier::in, sym_name::in, arity::in,
	record_resolved_item(T)::in(record_resolved_item), bool::out,
	resolved_item_map(T)::in, resolved_item_map(T)::out,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__record_resolved_item_3(ModuleQualifier, SymName, Arity,
		RecordItem, Recorded, ResolvedMap0, ResolvedMap) -->
	( { map__contains(ResolvedMap0, ModuleQualifier) } ->
		{ Recorded = no },
		{ ResolvedMap = ResolvedMap0 }
	;
		RecordItem(ModuleQualifier, SymName, Arity, Recorded,
			ResolvedMap0, ResolvedMap)
	).

%-----------------------------------------------------------------------------%

:- pred recompilation__usage__find_items_used_by_item(item_type::in,
	item_name::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_item((type), TypeCtor) -->
	ModuleInfo =^ module_info,
	{ module_info_types(ModuleInfo, Types) },
	{ map__lookup(Types, TypeCtor, TypeDefn) },
	{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
	( { TypeBody = eqv_type(Type) } ->
		% If we use an equivalence type we also use the
		% type it is equivalent to.
		recompilation__usage__find_items_used_by_type(Type)
	;
		[]
	).
recompilation__usage__find_items_used_by_item(type_body, TypeCtor) -->
	ModuleInfo =^ module_info,
	{ module_info_types(ModuleInfo, Types) },
	{ map__lookup(Types, TypeCtor, TypeDefn) },
	{ hlds_data__get_type_defn_body(TypeDefn, TypeBody) },
	recompilation__usage__find_items_used_by_type_body(TypeBody).
recompilation__usage__find_items_used_by_item((mode), ModeId) -->
	ModuleInfo =^ module_info,
	{ module_info_modes(ModuleInfo, Modes) },
	{ mode_table_get_mode_defns(Modes, ModeDefns) },
	{ map__lookup(ModeDefns, ModeId, ModeDefn) },
	recompilation__usage__find_items_used_by_mode_defn(ModeDefn).
recompilation__usage__find_items_used_by_item((inst), InstId) -->
	ModuleInfo =^ module_info,
	{ module_info_insts(ModuleInfo, Insts) },
	{ inst_table_get_user_insts(Insts, UserInsts) },
	{ user_inst_table_get_inst_defns(UserInsts, UserInstDefns) },
	{ map__lookup(UserInstDefns, InstId, InstDefn) },
	recompilation__usage__find_items_used_by_inst_defn(InstDefn).
recompilation__usage__find_items_used_by_item((typeclass), ClassItemId) -->
	{ ClassItemId = ClassName - ClassArity },
	{ ClassId = class_id(ClassName, ClassArity) },
	ModuleInfo =^ module_info,
	{ module_info_classes(ModuleInfo, Classes) },
	{ map__lookup(Classes, ClassId, ClassDefn) },
	{ ClassDefn = hlds_class_defn(_, Constraints, _, ClassInterface,
				_, _, _) },
	recompilation__usage__find_items_used_by_class_constraints(
		Constraints),
	(
		{ ClassInterface = abstract }
	;
		{ ClassInterface = concrete(Methods) },
		list__foldl(
			recompilation__usage__find_items_used_by_class_method,
			Methods)
	),
	{ module_info_instances(ModuleInfo, Instances) },
	( { map__search(Instances, ClassId, InstanceDefns) } ->
		list__foldl(
		    recompilation__usage__find_items_used_by_instance(
			ClassItemId), InstanceDefns)
	;
		[]
	).

recompilation__usage__find_items_used_by_item(predicate, ItemId) --> 
	recompilation__usage__record_used_pred_or_func(predicate, ItemId).
recompilation__usage__find_items_used_by_item(function, ItemId) -->
	recompilation__usage__record_used_pred_or_func(function, ItemId).
recompilation__usage__find_items_used_by_item(functor, _) -->
	{ error("recompilation__usage__find_items_used_by_item: functor") }.

:- pred recompilation__usage__find_items_used_by_instance(item_name::in,
	hlds_instance_defn::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_instance(ClassId,
		hlds_instance_defn(InstanceModuleName, _, _, Constraints,
			ArgTypes, _, _, _, _)) -->
	% XXX handle interface (currently not needed because
	% the interfaces for imported instances are only needed with
	% --intermodule-optimization, which isn't handled here yet)
	ModuleInfo =^ module_info,
	( 
		{ module_info_name(ModuleInfo, InstanceModuleName) }
	->
		[]
	;
		recompilation__usage__find_items_used_by_class_constraints(
			Constraints),
		recompilation__usage__find_items_used_by_types(ArgTypes),
		ModuleInstances0 =^ module_instances,
		{
			map__search(ModuleInstances0, InstanceModuleName,
				ClassIds0)
		->
			ClassIds1 = ClassIds0
		;
			set__init(ClassIds1)
		},
		{ set__insert(ClassIds1, ClassId, ClassIds) },
		{ map__set(ModuleInstances0, InstanceModuleName, ClassIds,
			ModuleInstances) },
		^ module_instances := ModuleInstances
	).

:- pred recompilation__usage__find_items_used_by_class_method(
	class_method::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_class_method(
		pred_or_func(_, _, _, _, _, ArgTypesAndModes, _,
			_, _, _, _, Constraints, _)) -->
	recompilation__usage__find_items_used_by_class_context(
		Constraints),
	list__foldl(
	    (pred(TypeAndMode::in, in, out) is det -->
		(
			{ TypeAndMode = type_only(Type) }
		;
			{ TypeAndMode = type_and_mode(Type, Mode) },
			recompilation__usage__find_items_used_by_mode(Mode)
		),	
		recompilation__usage__find_items_used_by_type(Type)
	    ), ArgTypesAndModes).
recompilation__usage__find_items_used_by_class_method(
		pred_or_func_mode(_, _, _, Modes, _, _, _, _)) -->
	recompilation__usage__find_items_used_by_modes(Modes).

:- pred recompilation__usage__find_items_used_by_type_body(hlds_type_body::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_type_body(
		du_type(Ctors, _, _, _, _)) -->
	list__foldl(
	    (pred(Ctor::in, in, out) is det -->
		{ Ctor = ctor(_, Constraints, _, CtorArgs) },
		recompilation__usage__find_items_used_by_class_constraints(
			Constraints),
		list__foldl(
		    (pred(CtorArg::in, in, out) is det -->
			{ CtorArg = _ - ArgType },
			recompilation__usage__find_items_used_by_type(ArgType)
		    ), CtorArgs)
	    ), Ctors).
recompilation__usage__find_items_used_by_type_body(eqv_type(Type)) -->
	recompilation__usage__find_items_used_by_type(Type).
recompilation__usage__find_items_used_by_type_body(abstract_type) --> [].
recompilation__usage__find_items_used_by_type_body(foreign_type(_)) --> [].

:- pred recompilation__usage__find_items_used_by_mode_defn(hlds_mode_defn::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_mode_defn(
		hlds_mode_defn(_, _, eqv_mode(Mode), _, _, _)) -->
	recompilation__usage__find_items_used_by_mode(Mode).

:- pred recompilation__usage__find_items_used_by_inst_defn(hlds_inst_defn::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_inst_defn(
		hlds_inst_defn(_, _, InstBody, _, _, _)) -->
	(
		{ InstBody = eqv_inst(Inst) },
		recompilation__usage__find_items_used_by_inst(Inst)
	;
		{ InstBody = abstract_inst }
	).

:- pred recompilation__usage__find_items_used_by_preds(pred_or_func::in,
		pred_or_func_set::in, recompilation_usage_info::in,
		recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_preds(PredOrFunc, Set) -->
	map__foldl(
	    (pred((Name - Arity)::in, MatchingPredMap::in, in, out) is det -->
		map__foldl(
		    (pred(ModuleQualifier::in, _::in, in, out) is det -->
			{ SymName = module_qualify_name(ModuleQualifier,
					Name) },
			recompilation__usage__record_used_pred_or_func(
				PredOrFunc, SymName - Arity)
		    ), MatchingPredMap)
	    ), Set).

:- pred recompilation__usage__find_items_used_by_pred(pred_or_func::in,
	pair(string, arity)::in, pair(pred_id, module_name)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_pred(PredOrFunc, Name - Arity,
		PredId - PredModule) -->
	=(Info0),
	{ ItemType = pred_or_func_to_item_type(PredOrFunc) },
	ModuleInfo =^ module_info,
	{ module_info_pred_info(ModuleInfo, PredId, PredInfo) },
	(
		{ ItemId = qualified(PredModule, Name) - Arity },
		{
			recompilation__usage__item_is_recorded_used(Info0,
				ItemType, ItemId)
		;
			recompilation__usage__item_is_local(Info0, ItemId)
		}
	->
		% We've already recorded the items used by this predicate.
		[]
	;
		%
		% Items used by class methods are recorded when processing
		% the typeclass declaration. Make sure that is done.
		%
		{ pred_info_get_markers(PredInfo, Markers) },
		{ check_marker(Markers, class_method) }
	->
		%
		% The typeclass for which the predicate is a method is the
		% first of the universal class constraints in the pred_info.
		%
		{ pred_info_get_class_context(PredInfo, MethodClassContext) },
		{ MethodClassContext = constraints(MethodUnivConstraints, _) },
		{
			MethodUnivConstraints =
				[constraint(ClassName0, ClassArgs) | _]
		->
			ClassName = ClassName0,
			ClassArity = list__length(ClassArgs)
		;
			error("class method with no class constraints")
		},
		recompilation__usage__maybe_record_item_to_process(
			typeclass, ClassName - ClassArity)
	;
		{ NameArity = qualified(PredModule, Name) - Arity },
		recompilation__usage__record_expanded_items_used_by_item(
			ItemType, NameArity),
		recompilation__usage__record_imported_item(ItemType, NameArity),
		{ pred_info_arg_types(PredInfo, ArgTypes) },
		recompilation__usage__find_items_used_by_types(ArgTypes), 
		{ pred_info_procedures(PredInfo, Procs) },
		map__foldl(
			(pred(_::in, ProcInfo::in, in, out) is det -->
				{ proc_info_argmodes(ProcInfo, ArgModes) },
				recompilation__usage__find_items_used_by_modes(
					ArgModes)
			), Procs),
		{ pred_info_get_class_context(PredInfo, ClassContext) },
		recompilation__usage__find_items_used_by_class_context(
			ClassContext),

		%
		% Record items used by `:- pragma type_spec' declarations.
		%
		{ module_info_type_spec_info(ModuleInfo, TypeSpecInfo) },
		{ TypeSpecInfo = type_spec_info(_, _, _, PragmaMap) },
		( { map__search(PragmaMap, PredId, TypeSpecPragmas) } ->
			list__foldl(
			    recompilation__usage__find_items_used_by_type_spec,
			    TypeSpecPragmas)
		;
			[]
		)
	).

:- pred recompilation__usage__find_items_used_by_type_spec(pragma_type::in, 
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_type_spec(Pragma) -->
	( { Pragma = type_spec(_, _, _, _, MaybeModes, Subst, _, _) } ->
		( { MaybeModes = yes(Modes) } ->
			recompilation__usage__find_items_used_by_modes(Modes)
		;
			[]
		),
		{ assoc_list__values(Subst, SubstTypes) },
		recompilation__usage__find_items_used_by_types(SubstTypes)
	;
		{ error(
"recompilation__usage__find_items_used_by_type_spec: unexpected pragma type") }
	).

:- pred recompilation__usage__find_items_used_by_functors(
	functor_set::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_functors(Set) -->
	map__foldl(
	    (pred((Name - Arity)::in, MatchingCtorMap::in, in, out) is det -->
		map__foldl(
		    (pred(Qualifier::in, _::in, in, out) is det -->
			 { SymName = module_qualify_name(Qualifier, Name) },
			 recompilation__usage__record_used_functor(
			 	SymName - Arity)
		    ), MatchingCtorMap)
	    ), Set).

:- pred recompilation__usage__find_items_used_by_functor(
	string::in, arity::in, resolved_functor::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_functor(Name, _Arity,
		pred_or_func(PredId, PredModule, PredOrFunc, PredArity)) -->
	recompilation__usage__find_items_used_by_pred(PredOrFunc,
		Name - PredArity, PredId - PredModule).
recompilation__usage__find_items_used_by_functor(_, _,
		constructor(TypeCtor)) -->
	recompilation__usage__maybe_record_item_to_process(type_body, TypeCtor).
recompilation__usage__find_items_used_by_functor(_, _, field(TypeCtor, _)) -->
	recompilation__usage__maybe_record_item_to_process(type_body, TypeCtor).

:- pred recompilation__usage__find_items_used_by_simple_item_set(
	item_type::in, simple_item_set::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_simple_item_set(ItemType, Set) -->
	map__foldl(
	    (pred((Name - Arity)::in, MatchingIdMap::in, in, out) is det -->
		map__foldl(
		    (pred(_::in, Module::in, in, out) is det -->
			    recompilation__usage__maybe_record_item_to_process(
				ItemType, qualified(Module, Name) - Arity)
		    ), MatchingIdMap)
	    ), Set).
	
:- pred recompilation__usage__find_items_used_by_types(list(type)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_types(Types) -->
	list__foldl(recompilation__usage__find_items_used_by_type, Types).

:- pred recompilation__usage__find_items_used_by_type((type)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_type(Type) -->
	(
		{ type_to_ctor_and_args(Type, TypeCtor, TypeArgs) }
	->
		(
			% Unqualified type-ids are builtin types.
			{ TypeCtor = qualified(_, _) - _ },
			\+ { type_ctor_is_higher_order(TypeCtor, _, _) }
		->
			recompilation__usage__maybe_record_item_to_process(
				(type), TypeCtor)
		;
			[]
		),
		recompilation__usage__find_items_used_by_types(TypeArgs)
	;
		[]
	).

:- pred recompilation__usage__find_items_used_by_modes(list(mode)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_modes(Modes) -->
	list__foldl(recompilation__usage__find_items_used_by_mode, Modes).

:- pred recompilation__usage__find_items_used_by_mode((mode)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_mode((Inst1 -> Inst2)) -->
	recompilation__usage__find_items_used_by_inst(Inst1),
	recompilation__usage__find_items_used_by_inst(Inst2).
recompilation__usage__find_items_used_by_mode(
		user_defined_mode(ModeName, ArgInsts)) -->
	{ list__length(ArgInsts, ModeArity) },
	recompilation__usage__maybe_record_item_to_process((mode),
		ModeName - ModeArity),
	recompilation__usage__find_items_used_by_insts(ArgInsts).

:- pred recompilation__usage__find_items_used_by_insts(list(inst)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_insts(Modes) -->
	list__foldl(recompilation__usage__find_items_used_by_inst, Modes).

:- pred recompilation__usage__find_items_used_by_inst((inst)::in,
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_inst(any(_)) --> [].
recompilation__usage__find_items_used_by_inst(free) --> [].
recompilation__usage__find_items_used_by_inst(free(_)) --> [].
recompilation__usage__find_items_used_by_inst(bound(_, BoundInsts)) -->
	list__foldl(
	    (pred(BoundInst::in, in, out) is det -->
		{ BoundInst = functor(ConsId, ArgInsts) },
		( { ConsId = cons(Name, Arity) } ->
			recompilation__usage__record_used_functor(
				Name - Arity)
		;
		[]
		),
		recompilation__usage__find_items_used_by_insts(ArgInsts)
	    ), BoundInsts).
recompilation__usage__find_items_used_by_inst(ground(_, GroundInstInfo)) -->
	(
		{ GroundInstInfo = higher_order(pred_inst_info(_, Modes, _)) },
		recompilation__usage__find_items_used_by_modes(Modes)
	;
		{ GroundInstInfo = none }
	).
recompilation__usage__find_items_used_by_inst(not_reached) --> [].
recompilation__usage__find_items_used_by_inst(inst_var(_)) --> [].
recompilation__usage__find_items_used_by_inst(constrained_inst_vars(_, Inst)) -->
	recompilation__usage__find_items_used_by_inst(Inst).
recompilation__usage__find_items_used_by_inst(defined_inst(InstName)) -->
	recompilation__usage__find_items_used_by_inst_name(InstName).
recompilation__usage__find_items_used_by_inst(
		abstract_inst(Name, ArgInsts)) -->
	{ list__length(ArgInsts, Arity) },
	recompilation__usage__maybe_record_item_to_process((inst),
		Name - Arity),
	recompilation__usage__find_items_used_by_insts(ArgInsts).

:- pred recompilation__usage__find_items_used_by_inst_name(inst_name::in, 
	recompilation_usage_info::in, recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_inst_name(
		user_inst(Name, ArgInsts)) -->
	{ list__length(ArgInsts, Arity) },
	recompilation__usage__maybe_record_item_to_process((inst),
		Name - Arity),
	recompilation__usage__find_items_used_by_insts(ArgInsts).
recompilation__usage__find_items_used_by_inst_name(
		merge_inst(Inst1, Inst2)) -->
	recompilation__usage__find_items_used_by_inst(Inst1),
	recompilation__usage__find_items_used_by_inst(Inst2).
recompilation__usage__find_items_used_by_inst_name(
		unify_inst(_, Inst1, Inst2, _)) -->
	recompilation__usage__find_items_used_by_inst(Inst1),
	recompilation__usage__find_items_used_by_inst(Inst2).
recompilation__usage__find_items_used_by_inst_name(
		ground_inst(InstName, _, _, _)) -->
	recompilation__usage__find_items_used_by_inst_name(InstName).
recompilation__usage__find_items_used_by_inst_name(
		any_inst(InstName, _, _, _)) -->
	recompilation__usage__find_items_used_by_inst_name(InstName).
recompilation__usage__find_items_used_by_inst_name(shared_inst(InstName)) -->
	recompilation__usage__find_items_used_by_inst_name(InstName).
recompilation__usage__find_items_used_by_inst_name(
		mostly_uniq_inst(InstName)) -->
	recompilation__usage__find_items_used_by_inst_name(InstName).
recompilation__usage__find_items_used_by_inst_name(typed_ground(_, Type)) -->
	recompilation__usage__find_items_used_by_type(Type).
recompilation__usage__find_items_used_by_inst_name(
		typed_inst(Type, InstName)) -->
	recompilation__usage__find_items_used_by_type(Type),
	recompilation__usage__find_items_used_by_inst_name(InstName).

:- pred recompilation__usage__find_items_used_by_class_context(
	class_constraints::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.
		
recompilation__usage__find_items_used_by_class_context(
		constraints(Constraints1, Constraints2)) -->
	recompilation__usage__find_items_used_by_class_constraints(
		Constraints1),
	recompilation__usage__find_items_used_by_class_constraints(
		Constraints2).

:- pred recompilation__usage__find_items_used_by_class_constraints(
	list(class_constraint)::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_class_constraints(Constraints) -->
	list__foldl(recompilation__usage__find_items_used_by_class_constraint,
		Constraints).

:- pred recompilation__usage__find_items_used_by_class_constraint(
	class_constraint::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__find_items_used_by_class_constraint(
		constraint(ClassName, ArgTypes)) -->
	{ ClassArity = list__length(ArgTypes) },
	recompilation__usage__maybe_record_item_to_process((typeclass),
		ClassName - ClassArity),
	recompilation__usage__find_items_used_by_types(ArgTypes).

:- pred recompilation__usage__maybe_record_item_to_process(item_type::in,
	pair(sym_name, arity)::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__maybe_record_item_to_process(ItemType, NameArity) -->
	( { ItemType = (typeclass) } ->
		Classes0 =^ used_typeclasses,
		{ set__insert(Classes0, NameArity, Classes) },
		^ used_typeclasses := Classes
	;
		[]
	),

	=(Info),
	( 
		{ item_is_recorded_used(Info, ItemType, NameArity) }
	->
		% This item has already been recorded.
		[]
	;
		{ item_is_local(Info, NameArity) }
	->
		% Ignore local items. The items used by them
		% have already been recorded by module_qual.m.
		[]
	;
		Queue0 =^ item_queue,
		{ queue__put(Queue0, item_id(ItemType, NameArity), Queue) },
		^ item_queue := Queue,

		recompilation__usage__record_imported_item(ItemType, NameArity),
		recompilation__usage__record_expanded_items_used_by_item(
			ItemType, NameArity)
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
	module_info_name(Info ^ module_info, ModuleName).

:- pred recompilation__usage__record_imported_item(item_type::in,
	pair(sym_name, arity)::in, recompilation_usage_info::in,
	recompilation_usage_info::out) is det.

recompilation__usage__record_imported_item(ItemType, SymName - Arity) -->
	{ SymName = qualified(Module0, Name0) ->
		Module = Module0,
		Name = Name0
	;
		error(
"recompilation__usage__maybe_record_item_to_process: unqualified item")
	},

	ImportedItems0 =^ imported_items,
	{ map__search(ImportedItems0, Module, ModuleItems0) ->
		ModuleItems1 = ModuleItems0
	;
		ModuleItems1 = init_item_id_set(set__init)
	},
	{ ModuleItemIds0 = extract_ids(ModuleItems1, ItemType) },
	{ set__insert(ModuleItemIds0, Name - Arity, ModuleItemIds) },
	{ ModuleItems = update_ids(ModuleItems1, ItemType, ModuleItemIds) },
	{ map__set(ImportedItems0, Module, ModuleItems, ImportedItems) },
	^ imported_items := ImportedItems.

	% Uses of equivalence types have been expanded away by equiv_type.m.
	% equiv_type.m records which equivalence types were used by each
	% imported item.
:- pred recompilation__usage__record_expanded_items_used_by_item(
		item_type::in, item_name::in, recompilation_usage_info::in,
		recompilation_usage_info::out) is det.

recompilation__usage__record_expanded_items_used_by_item(ItemType,
			NameArity) -->
	Dependencies =^ dependencies,
	(
		{ map__search(Dependencies, item_id(ItemType, NameArity),
			EquivTypes) }
	->
		list__foldl(
		    (pred(Item::in, in, out) is det -->
			{ Item = item_id(DepItemType, DepItemId) },
			recompilation__usage__maybe_record_item_to_process(
				DepItemType, DepItemId)
		    ), set__to_sorted_list(EquivTypes))
	;
		[]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
