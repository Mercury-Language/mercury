%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: recompilation_version.m.
% Main author: stayl.
% 
% Compute version numbers for program items in interface files.
% 
%-----------------------------------------------------------------------------%

:- module recompilation.version.
:- interface.

:- import_module libs.timestamp.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_io_util.

:- import_module io.
:- import_module maybe.
:- import_module term.

    % compute_version_numbers(SourceFileModTime, NewItems, MaybeOldItems,
    %   VersionNumbers).
    %
:- pred compute_version_numbers(timestamp::in, item_list::in,
    maybe(item_list)::in, version_numbers::out) is det.

:- pred write_version_numbers(version_numbers::in, io::di, io::uo) is det.

:- pred parse_version_numbers(term::in, maybe1(version_numbers)::out) is det.

    % The version number for the format of the version numbers
    % written to the interface files.
    %
:- func version_numbers_version_number = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_out.
:- import_module libs.compiler_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module varset.

compute_version_numbers(SourceFileTime, Items, MaybeOldItems,
        version_numbers(ItemVersionNumbers, InstanceVersionNumbers)) :-
    gather_items(section_implementation, Items, GatheredItems,
        InstanceItems),
    (
        MaybeOldItems = yes(OldItems0),
        OldItems0 = [FirstItem, VersionNumberItem | OldItems],
        FirstItem = item_module_defn(_, md_interface) - _,
        VersionNumberItem = item_module_defn(_,
            md_version_numbers(_, OldVersionNumbers)) - _
    ->
        OldVersionNumbers = version_numbers(OldItemVersionNumbers,
            OldInstanceVersionNumbers),
        gather_items(section_implementation, OldItems, GatheredOldItems,
            OldInstanceItems)
    ;
        % There were no old version numbers, so every item
        % gets the same timestamp as the source module.
        OldItemVersionNumbers = init_item_id_set(map.init),
        GatheredOldItems = init_item_id_set(map.init),
        map.init(OldInstanceItems),
        map.init(OldInstanceVersionNumbers)
    ),

    compute_item_version_numbers(SourceFileTime,
        GatheredItems, GatheredOldItems,
        OldItemVersionNumbers, ItemVersionNumbers),

    compute_instance_version_numbers(SourceFileTime,
        InstanceItems, OldInstanceItems,
        OldInstanceVersionNumbers, InstanceVersionNumbers).

:- pred compute_item_version_numbers(timestamp::in,
    gathered_items::in, gathered_items::in,
    item_version_numbers::in, item_version_numbers::out) is det.

compute_item_version_numbers(SourceFileTime,
        GatheredItems, GatheredOldItems,
        OldVersionNumbers, VersionNumbers) :-
    VersionNumbers = map_ids(compute_item_version_numbers_2(SourceFileTime,
        GatheredOldItems, OldVersionNumbers), GatheredItems, map.init).

:- func compute_item_version_numbers_2(timestamp, gathered_items,
    item_version_numbers, item_type,
    map(pair(string, arity), assoc_list(section, item_and_context)))
    = map(pair(string, arity), timestamp).

compute_item_version_numbers_2(SourceFileTime, GatheredOldItems,
        OldVersionNumbers, ItemType, Items0) =
    map.map_values(compute_item_version_numbers_3(SourceFileTime,
        GatheredOldItems, OldVersionNumbers, ItemType), Items0).

:- func compute_item_version_numbers_3(timestamp, gathered_items,
    item_version_numbers, item_type, pair(string, arity),
    assoc_list(section, item_and_context)) = timestamp.

compute_item_version_numbers_3(SourceFileTime, GatheredOldItems,
        OldVersionNumbers, ItemType, NameArity, Items) =
    (
        OldIds = extract_ids(GatheredOldItems, ItemType),
        map.search(OldIds, NameArity, OldItems),
        items_are_unchanged(OldItems, Items),
        map.search(extract_ids(OldVersionNumbers, ItemType), NameArity,
            OldVersionNumber)
    ->
        OldVersionNumber
    ;
        SourceFileTime
    ).

:- pred compute_instance_version_numbers(timestamp::in,
    instance_item_map::in, instance_item_map::in,
    instance_version_numbers::in, instance_version_numbers::out) is det.

compute_instance_version_numbers(SourceFileTime,
        InstanceItems, OldInstanceItems,
        OldInstanceVersionNumbers, InstanceVersionNumbers) :-
    InstanceVersionNumbers = map.map_values(
        (func(ClassId, Items) = VersionNumber :-
            (
                map.search(OldInstanceItems, ClassId, OldItems),
                items_are_unchanged(OldItems, Items),
                map.search(OldInstanceVersionNumbers, ClassId,
                    OldVersionNumber)
            ->
                VersionNumber = OldVersionNumber
            ;
                VersionNumber = SourceFileTime
            )
        ),
        InstanceItems
    ).

:- pred gather_items(section::in, item_list::in,
    gathered_items::out, instance_item_map::out) is det.

gather_items(Section, Items, GatheredItems, Instances) :-
    list.reverse(Items, RevItems),
    Info0 = gathered_item_info(init_item_id_set(map.init), [], [], map.init),
    list.foldl2(gather_items_2, RevItems, Section, _, Info0, Info1),

    % Items which could appear in _OtherItems (those which aren't gathered
    % into the list for another type of item) can't appear in the interface
    % section. Those other items (e.g. assertions) will need to be handled here
    % when smart recompilation is made to work with
    % `--intermodule-optimization'.
    Info1 = gathered_item_info(GatheredItems1, PragmaItems,
        _OtherItems, Instances),
    list.reverse(PragmaItems, RevPragmaItems),
    list.foldl(distribute_pragma_items, RevPragmaItems,
        GatheredItems1, GatheredItems).

:- pred distribute_pragma_items(
    {maybe_pred_or_func_id, item_and_context, section}::in,
    gathered_items::in, gathered_items::out) is det.

distribute_pragma_items({ItemId, ItemAndContext, Section}, !GatheredItems) :-
    ItemId = MaybePredOrFunc - SymName / Arity,
    ItemAndContext = Item - ItemContext,

    % For predicates defined using `with_type` annotations we don't know
    % the actual arity, so always we need to add entries for pragmas, even if
    % the pragma doesn't match any recorded predicate. For pragmas which don't
    % include enough information to work out whether they apply to a predicate
    % or a function this will result in an extra entry in the version numbers.
    % Pragmas in the interface aren't common so this won't be too much of
    % a problem.
    AddIfNotExisting = yes,
    ItemName = item_name(SymName, Arity),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        add_gathered_item(Item, item_id(ItemType, ItemName),
            ItemContext, Section, AddIfNotExisting, !GatheredItems)
    ;
        MaybePredOrFunc = no,
        add_gathered_item(Item, item_id(predicate_item, ItemName),
            ItemContext, Section, AddIfNotExisting, !GatheredItems),
        add_gathered_item(Item, item_id(function_item, ItemName),
            ItemContext, Section, AddIfNotExisting, !GatheredItems)
    ),

    % Pragmas can apply to typeclass methods.
    map.map_values(distribute_pragma_items_class_items(MaybePredOrFunc,
        SymName, Arity, ItemAndContext, Section),
        extract_ids(!.GatheredItems, typeclass_item), GatheredTypeClasses),
    !:GatheredItems = update_ids(!.GatheredItems, typeclass_item,
        GatheredTypeClasses).

:- pred distribute_pragma_items_class_items(maybe(pred_or_func)::in,
    sym_name::in, arity::in, item_and_context::in, section::in,
    pair(string, int)::in,
    assoc_list(section, item_and_context)::in,
    assoc_list(section, item_and_context)::out) is det.

distribute_pragma_items_class_items(MaybePredOrFunc, SymName, Arity,
        ItemAndContext, Section, _, !ClassItems) :-
    (
        % Does this pragma match any of the methods of this class.
        list.member(_ - ClassItem, !.ClassItems),
        ClassItem = item_typeclass(_, _, _, _, Interface, _) - _,
        Interface = class_interface_concrete(Methods),
        list.member(Method, Methods),
        Method = method_pred_or_func(_, _, _, MethodPredOrFunc, SymName,
            TypesAndModes, WithType, _, _, _, _, _, _),
        ( MaybePredOrFunc = yes(MethodPredOrFunc)
        ; MaybePredOrFunc = no
        ),
        (
            WithType = no,
            adjust_func_arity(MethodPredOrFunc, Arity,
                list.length(TypesAndModes))
        ;
            % We don't know the actual arity, so just match on the name
            % and pred_or_func.
            WithType = yes(_)
        )
    ->
        % XXX O(N^2), but shouldn't happen too often.
        !:ClassItems = !.ClassItems ++ [Section - ItemAndContext]
    ;
        true
    ).

:- type gathered_item_info
    --->    gathered_item_info(
                gathered_items  :: gathered_items,
                pragma_items    :: list({maybe_pred_or_func_id,
                                    item_and_context, section}),
                other_items     :: item_list,
                instances       :: instance_item_map
            ).

:- type instance_item_map ==
    map(item_name, assoc_list(section, item_and_context)).

    % The constructors set should always be empty.
:- type gathered_items == item_id_set(gathered_item_map).
:- type gathered_item_map == map(pair(string, arity),
    assoc_list(section, item_and_context)).

:- pred gather_items_2(item_and_context::in, section::in, section::out,
    gathered_item_info::in, gathered_item_info::out) is det.

gather_items_2(ItemAndContext, !Section, !Info) :-
    ItemAndContext = Item - ItemContext,
    (
        Item = item_module_defn(_, md_interface)
    ->
        !:Section = section_interface
    ;
        Item = item_module_defn(_, md_implementation)
    ->
        !:Section = section_implementation
    ;
        Item = item_type_defn(VarSet, Name, Args, Body, Cond)
    ->
        (
            Body = parse_tree_abstract_type(_),
            NameItem = Item,
            % The body of an abstract type can be recorded as used when
            % generating a call to the automatically generated unification
            % procedure.
            BodyItem = Item
        ;
            Body = parse_tree_du_type(_, _),
            NameItem = item_type_defn(VarSet, Name, Args,
                parse_tree_abstract_type(non_solver_type), Cond),
            BodyItem = Item
        ;
            Body = parse_tree_eqv_type(_),
            % When we use an equivalence type we always use the body.
            NameItem = Item,
            BodyItem = Item
        ;
            Body = parse_tree_solver_type(_, _),
            NameItem = Item,
            BodyItem = Item
        ;
            Body = parse_tree_foreign_type(_, _, _),
            NameItem = Item,
            BodyItem = Item
        ),
        TypeCtorItem = item_name(Name, list.length(Args)),
        GatheredItems0 = !.Info ^ gathered_items,
        add_gathered_item(NameItem, item_id(type_item, TypeCtorItem),
            ItemContext, !.Section, yes, GatheredItems0, GatheredItems1),
        add_gathered_item(BodyItem, item_id(type_body_item, TypeCtorItem),
            ItemContext, !.Section, yes, GatheredItems1, GatheredItems),
        !:Info = !.Info ^ gathered_items := GatheredItems
    ;
        Item = item_instance(_, ClassName, ClassArgs, _, _, _)
    ->
        Instances0 = !.Info ^ instances,
        ClassArity = list.length(ClassArgs),
        ClassItemName = item_name(ClassName, ClassArity),
        ( map.search(Instances0, ClassItemName, InstanceItemsPrime) ->
            InstanceItems = InstanceItemsPrime
        ;
            InstanceItems = []
        ),
        map.set(Instances0, ClassItemName,
            [!.Section - (Item - ItemContext) | InstanceItems], Instances),
        !:Info = !.Info ^ instances := Instances
    ;
        % For predicates or functions defined using `with_inst` annotations
        % the pred_or_func and arity here won't be correct, but equiv_type.m
        % will record the dependency on the version number with the `incorrect'
        % pred_or_func and arity, so this will work.
        Item = item_pred_or_func_mode(_, MaybePredOrFunc, SymName, Modes,
            WithInst, _, _),
        MaybePredOrFunc = no,
        WithInst = yes(_)
    ->
        GatheredItems0 = !.Info ^ gathered_items,
        ItemName = item_name(SymName, list.length(Modes)),
        add_gathered_item(Item, item_id(predicate_item, ItemName),
            ItemContext, !.Section, yes, GatheredItems0, GatheredItems1),
        add_gathered_item(Item, item_id(function_item, ItemName),
            ItemContext, !.Section, yes, GatheredItems1, GatheredItems),
        !:Info = !.Info ^ gathered_items := GatheredItems
    ;
        item_to_item_id(Item, ItemId)
    ->
        GatheredItems0 = !.Info ^ gathered_items,
        add_gathered_item(Item, ItemId, ItemContext, !.Section, yes,
            GatheredItems0, GatheredItems),
        !:Info = !.Info ^ gathered_items := GatheredItems
    ;
        Item = item_pragma(_, PragmaType),
        is_pred_pragma(PragmaType, yes(PredOrFuncId))
    ->
        PragmaItems = !.Info ^ pragma_items,
        !:Info = !.Info ^ pragma_items :=
            [{PredOrFuncId, ItemAndContext, !.Section} | PragmaItems]
    ;
        OtherItems = !.Info ^ other_items,
        !:Info = !.Info ^ other_items := [ItemAndContext | OtherItems]
    ).

:- pred add_gathered_item(item::in, item_id::in,
    prog_context::in, section::in, bool::in, gathered_items::in,
    gathered_items::out) is det.

add_gathered_item(Item, ItemId, ItemContext, Section, AddIfNotExisting,
        !GatheredItems) :-
    ItemId = item_id(ItemType, ItemName),
    ItemName = item_name(SymName, Arity),
    Name = unqualify_name(SymName),
    IdMap0 = extract_ids(!.GatheredItems, ItemType),
    NameArity = Name - Arity,
    ( map.search(IdMap0, NameArity, MatchingItems0) ->
        MatchingItems = MatchingItems0
    ;
        MatchingItems = []
    ),
    (
        MatchingItems = [],
        AddIfNotExisting = no
    ->
        true
    ;
        add_gathered_item_2(Item, ItemType, NameArity, ItemContext, Section,
            MatchingItems, !GatheredItems)
    ).

:- pred add_gathered_item_2(item::in, item_type::in,
    pair(string, arity)::in, prog_context::in, section::in,
    assoc_list(section, item_and_context)::in,
    gathered_items::in, gathered_items::out) is det.

add_gathered_item_2(Item, ItemType, NameArity, ItemContext, Section,
        MatchingItems0, !GatheredItems) :-
    % mercury_to_mercury.m splits combined pred and mode declarations.
    % That needs to be done here as well the item list read from the interface
    % file will match the item list generated here.
    (
        Item = item_pred_or_func(Origin, TVarSet, InstVarSet, ExistQVars,
            PredOrFunc, PredName, TypesAndModes, WithType, WithInst, Det,
            Cond, Purity, ClassContext),
        split_types_and_modes(TypesAndModes, Types, MaybeModes),
        MaybeModes = yes(Modes),
        ( Modes = [_ | _]
        ; WithInst = yes(_)
        )
    ->
        TypesWithoutModes = list.map((func(Type) = type_only(Type)), Types),
        varset.init(EmptyInstVarSet),
        PredOrFuncItem = item_pred_or_func(Origin, TVarSet, EmptyInstVarSet,
            ExistQVars, PredOrFunc, PredName, TypesWithoutModes, WithType,
            no, no, Cond, Purity, ClassContext),
        (
            WithInst = yes(_),
            % MaybePredOrFunc needs to be `no' here because when the item
            % is read from the interface file we won't know whether it is
            % a predicate or a function mode.
            MaybePredOrFunc = no
        ;
            WithInst = no,
            MaybePredOrFunc = yes(PredOrFunc)
        ),
        PredOrFuncModeItem = item_pred_or_func_mode(InstVarSet,
            MaybePredOrFunc, PredName, Modes, WithInst, Det, Cond),
        MatchingItems =
            [Section - (PredOrFuncItem - ItemContext),
            Section - (PredOrFuncModeItem - ItemContext)
            | MatchingItems0]
    ;
        Item ^ tc_class_methods = class_interface_concrete(Methods0)
    ->
        MethodsList = list.map(split_class_method_types_and_modes, Methods0),
        list.condense(MethodsList, Methods),
        TypeclassItem = Item ^ tc_class_methods
            := class_interface_concrete(Methods),
        MatchingItems = [Section - (TypeclassItem - ItemContext)
            | MatchingItems0]
    ;
        MatchingItems = [Section - (Item - ItemContext) | MatchingItems0]
    ),

    IdMap0 = extract_ids(!.GatheredItems, ItemType),
    map.set(IdMap0, NameArity, MatchingItems, IdMap),
    !:GatheredItems = update_ids(!.GatheredItems, ItemType, IdMap).

:- func split_class_method_types_and_modes(class_method) = class_methods.

split_class_method_types_and_modes(Method0) = Methods :-
    % Always strip the context from the item -- this is needed
    % so the items can be easily tested for equality.
    Method0 = method_pred_or_func(TVarSet, InstVarSet, ExistQVars, PredOrFunc,
        SymName, TypesAndModes, WithType, WithInst, MaybeDet, Cond,
        Purity, ClassContext, _),
    (
        split_types_and_modes(TypesAndModes, Types, MaybeModes),
        MaybeModes = yes(Modes),
        ( Modes = [_ | _]
        ; WithInst = yes(_)
        )
    ->
        TypesWithoutModes = list.map((func(Type) = type_only(Type)), Types),
        (
            WithInst = yes(_),
            % MaybePredOrFunc needs to be `no' here because when the item
            % is read from the interface file we won't know whether it is
            % a predicate or a function mode.
            MaybePredOrFunc = no
        ;
            WithInst = no,
            MaybePredOrFunc = yes(PredOrFunc)
        ),
        PredOrFuncModeItem = method_pred_or_func_mode(InstVarSet,
            MaybePredOrFunc, SymName, Modes, WithInst, MaybeDet, Cond,
            term.context_init),
        PredOrFuncModeItems = [PredOrFuncModeItem]
    ;
        TypesWithoutModes = TypesAndModes,
        PredOrFuncModeItems = []
    ),
    varset.init(EmptyInstVarSet),
    PredOrFuncItem = method_pred_or_func(TVarSet, EmptyInstVarSet, ExistQVars,
        PredOrFunc, SymName, TypesWithoutModes, WithType, no, no, Cond, Purity,
        ClassContext, term.context_init),
    Methods = [PredOrFuncItem | PredOrFuncModeItems].
split_class_method_types_and_modes(Method0) = [Method] :-
    % Always strip the context from the item -- this is needed
    % so the items can be easily tested for equality.
    Method0 = method_pred_or_func_mode(A, B, C, D, E, F, G, _),
    Method = method_pred_or_func_mode(A, B, C, D, E, F, G, term.context_init).

:- pred item_to_item_id(item::in, item_id::out) is semidet.

item_to_item_id(Item, ItemId) :-
    item_to_item_id_2(Item, yes(ItemId)).

:- pred item_to_item_id_2(item::in, maybe(item_id)::out) is det.

item_to_item_id_2(item_clause(_, _, _, _, _, _), no).
item_to_item_id_2(item_type_defn(_, Name, Params, _, _),
        yes(item_id(type_item, item_name(Name, Arity)))) :-
    list.length(Params, Arity).
item_to_item_id_2(item_inst_defn(_, Name, Params, _, _),
        yes(item_id(inst_item, item_name(Name, Arity)))) :-
    list.length(Params, Arity).
item_to_item_id_2(item_mode_defn(_, Name, Params, _, _),
        yes(item_id(mode_item, item_name(Name, Arity)))) :-
    list.length(Params, Arity).
item_to_item_id_2(item_module_defn(_, _), no).
item_to_item_id_2(Item, yes(item_id(ItemType, item_name(SymName, Arity)))) :-
    Item = item_pred_or_func(_, _, _, _, PredOrFunc, SymName, TypesAndModes,
        WithType, _, _, _, _, _),
    % For predicates or functions defined using `with_type` annotations
    % the arity here won't be correct, but equiv_type.m will record
    % the dependency on the version number with the `incorrect' arity,
    % so this will work.
    (
        WithType = no,
        adjust_func_arity(PredOrFunc, Arity, list.length(TypesAndModes))
    ;
        WithType = yes(_),
        Arity = list.length(TypesAndModes)
    ),
    ItemType = pred_or_func_to_item_type(PredOrFunc).

item_to_item_id_2(Item, ItemId) :-
    Item = item_pred_or_func_mode(_, MaybePredOrFunc, SymName, Modes, _, _, _),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        adjust_func_arity(PredOrFunc, Arity, list.length(Modes)),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        ItemId = yes(item_id(ItemType, item_name(SymName, Arity)))
    ;
        MaybePredOrFunc = no,
        % We need to handle these separately because a `:- mode' declaration
        % with a `with_inst` annotation could be for a predicate or a function.
        ItemId = no
    ).

    % We need to handle these separately because some pragmas
    % may affect a predicate and a function.
item_to_item_id_2(item_pragma(_, _), no).
item_to_item_id_2(item_promise(_, _, _, _), no).
item_to_item_id_2(Item,
        yes(item_id(typeclass_item, item_name(ClassName, ClassArity)))) :-
    Item = item_typeclass(_, _, ClassName, ClassVars, _, _),
    list.length(ClassVars, ClassArity).

    % Instances are handled separately (unlike other items, the module
    % qualifier on an instance declaration is the module containing
    % the class, not the module containing the instance).
item_to_item_id_2(item_instance(_, _, _, _, _, _), no).
item_to_item_id_2(item_initialise(_, _, _), no).
item_to_item_id_2(item_finalise(_, _, _), no).
item_to_item_id_2(item_mutable(_, _, _, _, _, _), no).
item_to_item_id_2(item_nothing(_), no).

:- type maybe_pred_or_func_id == pair(maybe(pred_or_func), sym_name_and_arity).

:- pred is_pred_pragma(pragma_type::in, maybe(maybe_pred_or_func_id)::out)
    is det.

is_pred_pragma(pragma_foreign_decl(_, _, _), no).
is_pred_pragma(pragma_foreign_import_module(_, _), no).
is_pred_pragma(pragma_foreign_code(_, _), no).
is_pred_pragma(pragma_foreign_proc(_, Name, PredOrFunc, Args, _, _, _),
        yes(yes(PredOrFunc) - Name / Arity)) :-
    adjust_func_arity(PredOrFunc, Arity, list.length(Args)).
is_pred_pragma(pragma_type_spec(Name, _, Arity, MaybePredOrFunc, _, _, _, _),
        yes(MaybePredOrFunc - Name / Arity)).
is_pred_pragma(pragma_inline(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_no_inline(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_obsolete(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_foreign_export(_, Name, PredOrFunc, Modes, _),
        yes(yes(PredOrFunc) - Name / Arity)) :-
    adjust_func_arity(PredOrFunc, Arity, list.length(Modes)).
    % Pragma import declarations are never used directly by Mercury code.
is_pred_pragma(pragma_import(_, _, _, _, _), no).
is_pred_pragma(pragma_source_file(_), no).
is_pred_pragma(pragma_unused_args(PredOrFunc, Name, Arity, _, _),
        yes(yes(PredOrFunc) - Name / Arity)).
is_pred_pragma(pragma_exceptions(PredOrFunc, Name, Arity, _, _),
        yes(yes(PredOrFunc) - Name / Arity)).
is_pred_pragma(pragma_trailing_info(PredOrFunc, Name, Arity, _, _),
		yes(yes(PredOrFunc) - Name / Arity)).
is_pred_pragma(pragma_mm_tabling_info(PredOrFunc, Name, Arity, _, _),
        yes(yes(PredOrFunc) - Name / Arity)).
is_pred_pragma(pragma_fact_table(Name, Arity, _), yes(no - Name / Arity)).
is_pred_pragma(pragma_reserve_tag(_TypeName, _TypeArity), no).
is_pred_pragma(pragma_tabled(_, Name, Arity, MaybePredOrFunc, _, _Attrs),
        yes(MaybePredOrFunc - Name / Arity)).
is_pred_pragma(pragma_promise_pure(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_promise_semipure(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_promise_equivalent_clauses(Name, Arity),
        yes(no - Name / Arity)).
is_pred_pragma(pragma_termination_info(PredOrFunc, Name, Modes, _, _),
        yes(yes(PredOrFunc) - Name / Arity)) :-
    adjust_func_arity(PredOrFunc, Arity, list.length(Modes)).  
is_pred_pragma(pragma_structure_sharing(PredOrFunc, Name, Modes, _, _, _),
        yes(yes(PredOrFunc) - Name / Arity)) :-
    adjust_func_arity(PredOrFunc, Arity, list.length(Modes)).  
is_pred_pragma(pragma_structure_reuse(PredOrFunc, Name, Modes, _, _, _),
        yes(yes(PredOrFunc) - Name / Arity)) :-
    adjust_func_arity(PredOrFunc, Arity, list.length(Modes)).  
is_pred_pragma(pragma_termination2_info(PredOrFunc, Name, Modes, _, _, _),
        yes(yes(PredOrFunc) - Name / Arity)) :-
    adjust_func_arity(PredOrFunc, Arity, list.length(Modes)).  
is_pred_pragma(pragma_terminates(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_does_not_terminate(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_check_termination(Name, Arity), yes(no - Name / Arity)).
is_pred_pragma(pragma_mode_check_clauses(Name, Arity), yes(no - Name / Arity)).

    % XXX This is a bit brittle (need to be careful with term.contexts).
    % For example, it won't work for clauses.
    % It will never succeed when it shouldn't, so it will never
    % cause a necessary recompilation to be missed.
:- pred items_are_unchanged(assoc_list(section, item_and_context)::in,
    assoc_list(section, item_and_context)::in) is semidet.

items_are_unchanged([], []).
items_are_unchanged([Section - (Item1 - _) | Items1],
        [Section - (Item2 - _) | Items2]) :-
    yes = item_is_unchanged(Item1, Item2),
    items_are_unchanged(Items1, Items2).

    % In most places here, we don't need to compare the varsets.
    % What matters is that the variable numbers in the arguments
    % and body are the same, the names are usually irrelevant.
    %
    % The only places where the names of variables affect the
    % compilation of the program are in explicit type qualifications
    % and `:- pragma type_spec' declarations. Explicit type
    % qualifications do not need to be considered here. This module
    % only deals with items in interface files (we don't yet write type
    % qualifications to `.opt' files). Variables in type qualifications
    % are only matched with the head type variables of the predicate
    % by make_hlds.m. For `:- pragma type_spec' declarations to work
    % we need to consider a predicate or function declaration to be
    % changed if the names of any of the type variables are changed.
    %
    % It's important not to compare the varsets for type and instance
    % declarations because the declarations we get here may be abstract
    % declarations produced from concrete declarations for use in an
    % interface file. The varsets may contain variables from the
    % discarded bodies which will not be present in the items read
    % in from the interface files for comparison.
    %
    % This code assumes that the variables in the head of a
    % type or instance declaration are added to the varset before
    % those from the body, so that the variable numbers in the head of
    % the declaration match those from an abstract declaration read
    % from an interface file.
    %
:- func item_is_unchanged(item, item) = bool.

item_is_unchanged(item_type_defn(_, Name, Args, Defn, Cond), Item2) =
    ( Item2 = item_type_defn(_, Name, Args, Defn, Cond) -> yes ; no ).
item_is_unchanged(item_mode_defn(_VarSet, Name, Args, Defn, Cond), Item2) =
    ( Item2 = item_mode_defn(_, Name, Args, Defn, Cond) -> yes ; no ).
item_is_unchanged(item_inst_defn(_VarSet, Name, Args, Defn, Cond), Item2) =
    ( Item2 = item_inst_defn(_, Name, Args, Defn, Cond) -> yes ; no ).
item_is_unchanged(item_module_defn(_VarSet, Defn), Item2) =
    ( Item2 = item_module_defn(_, Defn) -> yes ; no ).
item_is_unchanged(item_instance(Constraints, Name, Types, Body, _VarSet,
        Module), Item2) =
    ( Item2 = item_instance(Constraints, Name, Types, Body, _, Module) ->
        yes
    ;
        no
    ).

    % XXX Need to compare the goals properly in clauses and assertions.
    % That's not necessary at the moment because smart recompilation
    % doesn't work with inter-module optimization yet.
item_is_unchanged(item_clause(_, _VarSet, PorF, SymName, Args, Goal), Item2) =
        ( Item2 = item_clause(_, _, PorF, SymName, Args, Goal) -> yes ; no ).
item_is_unchanged(item_promise(PromiseType, Goal, _, UnivVars), Item2) =
        ( Item2 = item_promise(PromiseType, Goal, _, UnivVars) -> yes ; no ).

    % We do need to compare the variable names in `:- pragma type_spec'
    % declarations because the names of the variables are used
    % to find the corresponding variables in the predicate or
    % function type declaration.
item_is_unchanged(item_pragma(_, PragmaType1), Item2) = Result :-
    ( Item2 = item_pragma(_, PragmaType2) ->
        (
            PragmaType1 = pragma_type_spec(Name, SpecName, Arity,
                MaybePredOrFunc, MaybeModes, TypeSubst1, TVarSet1, _),
            PragmaType2 = pragma_type_spec(Name, SpecName, Arity,
                MaybePredOrFunc, MaybeModes, TypeSubst2, TVarSet2, _)
        ->
            assoc_list.keys_and_values(TypeSubst1, TVars1, Types1),
            assoc_list.keys_and_values(TypeSubst2, TVars2, Types2),
            % XXX kind inference:
            % we assume vars have kind `star'.
            KindMap = map.init,
            prog_type.var_list_to_type_list(KindMap, TVars1, TVarTypes1),
            prog_type.var_list_to_type_list(KindMap, TVars2, TVarTypes2),
            (
                type_list_is_unchanged(
                    TVarSet1, TVarTypes1 ++ Types1,
                    TVarSet2, TVarTypes2 ++ Types2,
                    _, _, _)
            ->
                Result = yes
            ;
                Result = no
            )
        ;
            Result = ( PragmaType1 = PragmaType2 -> yes ; no )
        )
    ;
        Result = no
    ).
item_is_unchanged(item_nothing(A), Item2) =
    ( Item2 = item_nothing(A) -> yes ; no ).
item_is_unchanged(item_initialise(O, A, B), Item2) =
    ( Item2 = item_initialise(O, A, B) -> yes ; no ).
item_is_unchanged(item_finalise(O, A, B), Item2) = 
    ( Item2 = item_finalise(O, A, B) -> yes ; no ).
item_is_unchanged(item_mutable(A, B, C, D, E, F), Item2) =
    ( Item2 = item_mutable(A, B, C, D, E, F) -> yes ; no ).

item_is_unchanged(Item1, Item2) = Result :-
    Item1 = item_pred_or_func(_, TVarSet1, _, ExistQVars1, PredOrFunc,
        Name, TypesAndModes1, WithType1, _,
        Det1, Cond, Purity, Constraints1),
    (
        Item2 = item_pred_or_func(_, TVarSet2, _, ExistQVars2,
            PredOrFunc, Name, TypesAndModes2, WithType2,
            _, Det2, Cond, Purity, Constraints2),

        % For predicates, ignore the determinism -- the modes and
        % determinism should have been split into a separate
        % declaration. This case can only happen if this was
        % not a combined predicate and mode declaration
        % (XXX We should warn about this somewhere).
        % For functions a determinism declaration but no modes
        % implies the default modes. The default modes are
        % added later by make_hlds.m, so they won't have been
        % split into a separate declaration here.
        (
            PredOrFunc = function,
            Det1 = Det2
        ;
            PredOrFunc = predicate
        ),

        pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
            TypesAndModes1, WithType1, Constraints1, TVarSet2,
            ExistQVars2, TypesAndModes2, WithType2, Constraints2)
    ->
        Result = yes
    ;
        Result = no
    ).

item_is_unchanged(Item1, Item2) = Result :-
    Item1 = item_pred_or_func_mode(InstVarSet1, PredOrFunc, Name, Modes1,
            WithInst1, Det, Cond),
    (
        Item2 = item_pred_or_func_mode(InstVarSet2, PredOrFunc,
            Name, Modes2, WithInst2, Det, Cond),
        pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
            InstVarSet2, Modes2, WithInst2)
    ->
        Result = yes
    ;
        Result = no
    ).

item_is_unchanged(Item1, Item2) = Result :-
    Item1 = item_typeclass(Constraints, FunDeps, Name, Vars, Interface1,
        _VarSet),
    (
        Item2 = item_typeclass(Constraints, FunDeps, Name, Vars, Interface2,
            _),
        class_interface_is_unchanged(Interface1, Interface2)
    ->
        Result = yes
    ;
        Result = no
    ).

    % Apply a substitution to the existq_tvars, types_and_modes, and
    % prog_constraints so that the type variables from both declarations
    % being checked are contained in the same tvarset, then check that
    % they are identical.
    %
    % We can't just assume that the varsets will be identical for
    % identical declarations because mercury_to_mercury.m splits
    % combined type and mode declarations into separate declarations.
    % When they are read back in the variable numbers will be different
    % because parser stores the type and inst variables for a combined
    % declaration in a single varset (it doesn't know which are which).
    %
:- pred pred_or_func_type_is_unchanged(tvarset::in, existq_tvars::in,
    list(type_and_mode)::in, maybe(mer_type)::in, prog_constraints::in,
    tvarset::in, existq_tvars::in, list(type_and_mode)::in,
    maybe(mer_type)::in, prog_constraints::in) is semidet.

pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1, TypesAndModes1,
        MaybeWithType1, Constraints1, TVarSet2, ExistQVars2,
        TypesAndModes2, MaybeWithType2, Constraints2) :-
    GetArgTypes =
        (func(TypeAndMode0) = Type :-
            (
                TypeAndMode0 = type_only(Type)
            ;
                % This should have been split out into a
                % separate mode declaration by gather_items.
                TypeAndMode0 = type_and_mode(_, _),
                unexpected(this_file,
                    "pred_or_func_type_matches: type_and_mode")
            )
        ),
    Types1 = list.map(GetArgTypes, TypesAndModes1),
    Types2 = list.map(GetArgTypes, TypesAndModes2),
    (
        MaybeWithType1 = yes(WithType1),
        MaybeWithType2 = yes(WithType2),
        AllTypes1 = [WithType1 | Types1],
        AllTypes2 = [WithType2 | Types2]
    ;
        MaybeWithType1 = no,
        MaybeWithType2 = no,
        AllTypes1 = Types1,
        AllTypes2 = Types2
    ),

    type_list_is_unchanged(TVarSet1, AllTypes1, TVarSet2, AllTypes2,
        _TVarSet, Renaming, Types2ToTypes1Subst),

    % Check that the existentially quantified variables are equivalent.
    %
    % XXX kind inference: we assume all tvars have kind `star'.

    map.init(KindMap2),
    apply_variable_renaming_to_tvar_kind_map(Renaming, KindMap2,
        RenamedKindMap2),
    apply_variable_renaming_to_tvar_list(Renaming, ExistQVars2,
        RenamedExistQVars2),
    apply_rec_subst_to_tvar_list(RenamedKindMap2, Types2ToTypes1Subst,
        RenamedExistQVars2, SubstExistQTypes2),
    ( prog_type.type_list_to_var_list(SubstExistQTypes2, SubstExistQVars2) ->
        ExistQVars1 = SubstExistQVars2
    ;
        unexpected(this_file, "pred_or_func_type_is_unchanged: non-var")
    ),

    % Check that the class constraints are identical.
    apply_variable_renaming_to_prog_constraints(Renaming,
        Constraints2, RenamedConstraints2),
    apply_rec_subst_to_prog_constraints(Types2ToTypes1Subst,
        RenamedConstraints2, SubstConstraints2),
    Constraints1 = SubstConstraints2.

:- pred type_list_is_unchanged(tvarset::in, list(mer_type)::in,
    tvarset::in, list(mer_type)::in, tvarset::out,
    tvar_renaming::out, tsubst::out) is semidet.

type_list_is_unchanged(TVarSet1, Types1, TVarSet2, Types2,
        TVarSet, Renaming, Types2ToTypes1Subst) :-
    tvarset_merge_renaming(TVarSet1, TVarSet2, TVarSet, Renaming),
    apply_variable_renaming_to_type_list(Renaming, Types2, SubstTypes2),

    % Check that the types are equivalent.
    type_list_subsumes(SubstTypes2, Types1, Types2ToTypes1Subst),
    type_list_subsumes(Types1, SubstTypes2, _),

    % Check that the corresponding variables have the same names. This is
    % necessary because `:- pragma type_spec' declarations depend on the names
    % of the variables, so for example if two variable names are swapped,
    % the same `:- pragma type_spec' declaration will cause a different
    % specialized version to be created.

    ( all [VarInItem1, VarInItem2]
        (
            map.member(Types2ToTypes1Subst, VarInItem2, SubstTerm),
            % Note that since the type comes from a substitution,
            % it will not contain a kind annotation.
            SubstTerm = type_variable(VarInItem1, _)
        )
    =>
        (
            varset.lookup_name(TVarSet, VarInItem1, VarName1),
            varset.lookup_name(TVarSet, VarInItem2, VarName2),
            (
                VarName1 = VarName2
            ;
                % Variables written to interface files are always named,
                % even if the variable in the source code was not, so we can't
                % just use varset.search_name to check whether the variables
                % are named.
                VarIsNotNamed =
                    (pred(VarName::in) is semidet :-
                        string.append("V_", VarNum, VarName),
                        string.to_int(VarNum, _)
                    ),
                VarIsNotNamed(VarName1),
                VarIsNotNamed(VarName2)
            )
        )
    ).

:- pred pred_or_func_mode_is_unchanged(inst_varset::in, list(mer_mode)::in,
    maybe(mer_inst)::in, inst_varset::in, list(mer_mode)::in,
    maybe(mer_inst)::in) is semidet.

pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, MaybeWithInst1,
        InstVarSet2, Modes2, MaybeWithInst2) :-
    varset.coerce(InstVarSet1, VarSet1),
    varset.coerce(InstVarSet2, VarSet2),

    % Apply the substitution to the modes so that the inst variables
    % from both declarations being checked are contained in the same
    % inst_varset, then check that they are identical.
    varset.merge_subst(VarSet1, VarSet2, _, InstSubst),

    % Treat modes as terms here to use term.list_subsumes, which does just
    % what we want here.
    ModeTerms1 = list.map(mode_to_term, Modes1),
    ModeTerms2 = list.map(mode_to_term, Modes2),
    (
        MaybeWithInst1 = yes(Inst1),
        MaybeWithInst2 = yes(Inst2),
        WithInstTerm1 = mode_to_term(free -> Inst1),
        WithInstTerm2 = mode_to_term(free -> Inst2),
        AllModeTerms1 = [WithInstTerm1 | ModeTerms1],
        AllModeTerms2 = [WithInstTerm2 | ModeTerms2]
    ;
        MaybeWithInst1 = no,
        MaybeWithInst2 = no,
        AllModeTerms1 = ModeTerms1,
        AllModeTerms2 = ModeTerms2
    ),

    term.apply_substitution_to_list(AllModeTerms2, InstSubst,
        SubstAllModeTerms2),
    term.list_subsumes(AllModeTerms1, SubstAllModeTerms2, _),
    term.list_subsumes(SubstAllModeTerms2, AllModeTerms1, _).

    % Combined typeclass method type and mode declarations are split as for
    % ordinary predicate declarations, so the varsets won't necessarily match
    % up if a typeclass declaration is read back from an interface file.
    %
:- pred class_interface_is_unchanged(class_interface::in,
    class_interface::in) is semidet.

class_interface_is_unchanged(Interface0, Interface) :-
    (
        Interface0 = class_interface_abstract,
        Interface = class_interface_abstract
    ;
        Interface0 = class_interface_concrete(Methods1),
        class_methods_are_unchanged(Methods1, Methods2),
        Interface = class_interface_concrete(Methods2)
    ).

:- pred class_methods_are_unchanged(class_methods::in,
    class_methods::in) is semidet.

class_methods_are_unchanged([], []).
class_methods_are_unchanged([Method1 | Methods1], [Method2 | Methods2]) :-
    (
        Method1 = method_pred_or_func(TVarSet1, _, ExistQVars1, PredOrFunc,
            Name, TypesAndModes1, WithType1, _,
            Detism, Cond, Purity, Constraints1, _),
        Method2 = method_pred_or_func(TVarSet2, _, ExistQVars2, PredOrFunc,
            Name, TypesAndModes2, WithType2, _,
            Detism, Cond, Purity, Constraints2, _),
        pred_or_func_type_is_unchanged(TVarSet1, ExistQVars1,
            TypesAndModes1, WithType1, Constraints1,
            TVarSet2, ExistQVars2, TypesAndModes2, WithType2,
            Constraints2)
    ;
        Method1 = method_pred_or_func_mode(InstVarSet1, PredOrFunc, Name,
            Modes1, WithInst1, Det, Cond, _),
        Method2 = method_pred_or_func_mode(InstVarSet2, PredOrFunc, Name,
            Modes2, WithInst2, Det, Cond, _),
        pred_or_func_mode_is_unchanged(InstVarSet1, Modes1, WithInst1,
            InstVarSet2, Modes2, WithInst2)
    ),
    class_methods_are_unchanged(Methods1, Methods2).

%-----------------------------------------------------------------------------%

write_version_numbers(AllVersionNumbers, !IO) :-
    AllVersionNumbers = version_numbers(VersionNumbers,
        InstanceVersionNumbers),
    VersionNumbersList = list.filter_map(
        (func(ItemType) = (ItemType - ItemVersions) is semidet :-
            ItemVersions = extract_ids(VersionNumbers, ItemType),
            \+ map.is_empty(ItemVersions)
        ),
        [type_item, type_body_item, mode_item, inst_item,
            predicate_item, function_item, typeclass_item]),
    io.write_string("{\n\t", !IO),
    io.write_list(VersionNumbersList, ",\n\t",
        write_item_type_and_versions, !IO),
    ( map.is_empty(InstanceVersionNumbers) ->
        true
    ;
        (
            VersionNumbersList = []
        ;
            VersionNumbersList = [_ | _],
            io.write_string(",\n\t", !IO)
        ),
        io.write_string("instance(", !IO),
        map.to_assoc_list(InstanceVersionNumbers, InstanceAL),
        io.write_list(InstanceAL, ",\n\n\t",
            write_symname_arity_version_number, !IO),
        io.write_string(")\n\t", !IO)
    ),
    io.write_string("\n}", !IO).

:- pred write_item_type_and_versions(
    pair(item_type, map(pair(string, int), version_number))::in,
    io::di, io::uo) is det.

write_item_type_and_versions(ItemType - ItemVersions, !IO) :-
    string_to_item_type(ItemTypeStr, ItemType),
    io.write_string(ItemTypeStr, !IO),
    io.write_string("(\n\t\t", !IO),
    map.to_assoc_list(ItemVersions, ItemVersionsList),
    io.write_list(ItemVersionsList, ",\n\t\t",
        write_name_arity_version_number, !IO),
    io.write_string("\n\t)", !IO).

:- pred write_name_arity_version_number(
    pair(pair(string, int), version_number)::in, io::di, io::uo) is det.

write_name_arity_version_number(NameArity - VersionNumber, !IO) :-
    NameArity = Name - Arity,
    mercury_output_bracketed_sym_name(unqualified(Name),
        next_to_graphic_token, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" - ", !IO),
    write_version_number(VersionNumber, !IO).

:- pred write_symname_arity_version_number(
    pair(item_name, version_number)::in, io::di, io::uo) is det.

write_symname_arity_version_number(ItemName - VersionNumber, !IO) :-
    ItemName = item_name(SymName, Arity),
    mercury_output_bracketed_sym_name(SymName, next_to_graphic_token, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" - ", !IO),
    write_version_number(VersionNumber, !IO).

%-----------------------------------------------------------------------------%

version_numbers_version_number = 1.

%-----------------------------------------------------------------------------%

parse_version_numbers(VersionNumbersTerm, Result) :-
    (
        VersionNumbersTerm = term.functor(term.atom("{}"),
            VersionNumbersTermList0, _)
    ->
        VersionNumbersTermList = VersionNumbersTermList0
    ;
        VersionNumbersTermList = [VersionNumbersTerm]
    ),
    map_parser(parse_item_type_version_numbers, VersionNumbersTermList,
        Result0),
    (
        Result0 = ok1(List),
        VersionNumbers0 = version_numbers(init_item_id_set(map.init),
            map.init),
        VersionNumbers = list.foldl(
            (func(VNResult, version_numbers(VNs0, Instances0)) =
                    version_numbers(VNs, Instances) :-
                (
                    VNResult = items(ItemType, ItemVNs),
                    VNs = update_ids(VNs0, ItemType, ItemVNs),
                    Instances = Instances0
                ;
                    VNResult = instances(Instances),
                    VNs = VNs0
                )
            ), List, VersionNumbers0),
        Result = ok1(VersionNumbers)
    ;
        Result0 = error1(Errors),
        Result = error1(Errors)
    ).

:- type item_version_numbers_result
    --->    items(item_type, version_number_map)
    ;       instances(instance_version_numbers).

:- pred parse_item_type_version_numbers(term::in,
    maybe1(item_version_numbers_result)::out) is det.

parse_item_type_version_numbers(Term, Result) :-
    (
        Term = term.functor(term.atom(ItemTypeStr), ItemsVNsTerms, _),
        string_to_item_type(ItemTypeStr, ItemType)
    ->
        ParseName =
            (pred(NameTerm::in, Name::out) is semidet :-
                NameTerm = term.functor(term.atom(Name), [], _)
            ),
        map_parser(parse_key_version_number(ParseName), ItemsVNsTerms,
            Result0),
        (
            Result0 = ok1(VNsAL),
            map.from_assoc_list(VNsAL, VNsMap),
            Result = ok1(items(ItemType, VNsMap))
        ;
            Result0 = error1(Errors),
            Result = error1(Errors)
        )
    ;
        Term = term.functor(term.atom("instance"),
            InstanceVNsTerms, _)
    ->
        ParseName =
            (pred(NameTerm::in, Name::out) is semidet :-
                sym_name_and_args(NameTerm, Name, [])
            ),
        map_parser(parse_item_version_number(ParseName), InstanceVNsTerms,
            Result1),
        (
            Result1 = ok1(VNsAL),
            map.from_assoc_list(VNsAL, VNsMap),
            Result = ok1(instances(VNsMap))
        ;
            Result1 = error1(Errors),
            Result = error1(Errors)
        )
    ;
        Result = error1(["invalid item type version numbers" - Term])
    ).

:- pred parse_key_version_number(
    pred(term, string)::(pred(in, out) is semidet), term::in,
    maybe1(pair(pair(string, arity), version_number))::out) is det.

parse_key_version_number(ParseName, Term, Result) :-
    (
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, Name),
        ArityTerm = term.functor(term.integer(Arity), _, _),
        VersionNumber = term_to_version_number(VersionNumberTerm)
    ->
        Result = ok1((Name - Arity) - VersionNumber)
    ;
        Result = error1(["error in item version number" - Term])
    ).

:- pred parse_item_version_number(
    pred(term, sym_name)::(pred(in, out) is semidet), term::in,
    maybe1(pair(item_name, version_number))::out) is det.

parse_item_version_number(ParseName, Term, Result) :-
    (
        Term = term.functor(term.atom("-"),
            [ItemNameArityTerm, VersionNumberTerm], _),
        ItemNameArityTerm = term.functor(term.atom("/"),
            [NameTerm, ArityTerm], _),
        ParseName(NameTerm, SymName),
        ArityTerm = term.functor(term.integer(Arity), _, _),
        VersionNumber = term_to_version_number(VersionNumberTerm)
    ->
        Result = ok1(item_name(SymName, Arity) - VersionNumber)
    ;
        Result = error1(["error in item version number" - Term])
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "recompilation.version.m".

%-----------------------------------------------------------------------------%
