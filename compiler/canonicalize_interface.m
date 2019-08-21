%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: canonicalize_interface.m.
% Main author: zs.
%
% The job of this module is to put the contents of an interface file into
% a canonical order. Without this, a semantically-null change such as
% reordering some declarations in a module's interface  would cause
% that module's interface files to change, which would then require
% the recompilation of all *other* modules that import those interface files.
%
%---------------------------------------------------------------------------%

:- module parse_tree.canonicalize_interface.
:- interface.

:- import_module parse_tree.prog_item.

%---------------------------------------------------------------------------%

    % Put the contents of an interface file, as represented by its parse tree,
    % into a sort of standard order. We want to ensure that if the set of
    % exported entities of a module does not change, the contents of the
    % module's automatically generated interface files shouldn't change either,
    % even if the programmer reorders those exported entities. This is because
    % any change in the interface file will require the recompilation of
    % all the modules that import that interface file.
    %
    % We should be able to just sort the includes, avails and items
    % in the parse tree. The includes and avails we *could* always sort,
    % but for a long time we could not sort the items, because the code that
    % added items to the parse tree did so in three passes, and the ordering
    % of the items that were processed in the same pass mattered. For example,
    % if the ":- mode" declaration of a predicate preceded the ":- pred"
    % declaration of that predicate, the compiler would generate an error
    % message. Now that we add each kind of item to the HLDS in a separate
    % pass, we *could* just sort the list of items, but keeping the previous
    % ordering does make interface files easier to understand for us humans.
    %
    % The order we generate puts items in this order:
    %
    % - All type definitions.
    %
    % - All inst definitions (may refer to refer to types).
    %
    % - All mode definitions (may refer to types and insts).
    %
    % - All pred and mode declarations in sym_name_and_arity order,
    %   and with all pred declarations for a given sym_name_and_arity preceding
    %   all mode declarations for that sym_name_and_arity. If there is a pred
    %   declaration for both a predicate and a function for the same
    %   sym_name_and_arity (which can happen, and does happen reasonably often)
    %   the resulting order is somewhat awkward, but since mode declarations
    %   contain only a maybe(pred_or_func), not a definite pred_or_func,
    %   we cannot easily do any better. We preserve the order of mode
    %   declarations for a given sym_name_and_arity, since these matter.
    %
    %   The pred and mode declarations may of course refer to types,
    %   typeclasses, insts and modes. The types, insts and modes were
    %   defined earlier; the typeclasses don't have to be, because the
    %   code that add predicate declarations to the HLDS only records
    %   the typeclass constraints without checking whether the named
    %   typeclasses have been added to the HLDS yet.
    %
    % - All promises, typeclass definitions, instance declarations,
    %   and declaration-like pragmas. These may refer to types
    %   (for e.g. type_spec pragmas), insts/modes (e.g. as part of procedure
    %   specifiers), and predicates and functions. We sort these, as the
    %   ordering between them does not matter.
    %
    % Note that while we *could* just sort the Avails, we do process them
    % a bit more, for two reasons: to remove duplicates (which sorting could
    % do as well), and to remove the use_module declarations for modules
    % that have an import_module declaration as well (which sorting could
    % *not* do).
    %
    % There is no need for any similar processing for Incls, because,
    % unlike importing or using a module more than once, including a submodule
    % more than once is an error,
    %
:- pred order_parse_tree_int_contents(parse_tree_int::in, parse_tree_int::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.prog_data.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

order_parse_tree_int_contents(ParseTreeInt0, ParseTreeInt) :-
    ParseTreeInt0 = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls0, ImpIncls0, IntAvails0, ImpAvails0,
        IntFIMs0, ImpFIMs0, IntItems0, ImpItems0),
    list.sort(IntIncls0, IntIncls),
    list.sort(ImpIncls0, ImpIncls),
    order_avails(IntAvails0, IntAvails),
    order_avails(ImpAvails0, ImpAvails),
    list.sort(IntFIMs0, IntFIMs),
    list.sort(ImpFIMs0, ImpFIMs),
    order_items(IntItems0, IntItems),
    (
        ImpItems0 = [],
        % Do not try to tear apart and then put back together
        % the empty list of items; there is no point.
        ImpItems = ImpItems0
    ;
        ImpItems0 = [_ | _],
        order_items(ImpItems0, ImpItems)
    ),
    ParseTreeInt = parse_tree_int(ModuleName, IntFileKind, ModuleNameContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems).

%---------------------------------------------------------------------------%

:- pred order_avails(list(item_avail)::in, list(item_avail)::out) is det.

order_avails(Avails, SortedAvails) :-
    build_avail_map(Avails, map.init, AvailMap),
    map.foldl(append_avail_entry, AvailMap, cord.init, SortedAvailCord),
    SortedAvails = cord.list(SortedAvailCord).

:- type avail_map == map(module_name, import_or_use).

:- pred build_avail_map(list(item_avail)::in, avail_map::in, avail_map::out)
    is det.

build_avail_map([], !AvailMap).
build_avail_map([Avail | Avails], !AvailMap) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, _Context, _SeqNum)),
        CurKind = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, _Context, _SeqNum)),
        CurKind = use_decl
    ),
    ( if map.search(!.AvailMap, ModuleName, OldKind) then
        ( if OldKind = use_decl, CurKind = import_decl then
            map.det_update(ModuleName, CurKind, !AvailMap)
        else
            true
        )
    else
        map.det_insert(ModuleName, CurKind, !AvailMap)
    ),
    build_avail_map(Avails, !AvailMap).

:- pred append_avail_entry(module_name::in, import_or_use::in,
    cord(item_avail)::in, cord(item_avail)::out) is det.

append_avail_entry(ModuleName, ImportOrUse, !AvailsCord) :-
    % The context and sequence number don't get written out, so their value
    % doesn't matter.
    Context = term.context_init,
    SeqNum = -1,
    (
        ImportOrUse = import_decl,
        Avail = avail_import(avail_import_info(ModuleName, Context, SeqNum))
    ;
        ImportOrUse = use_decl,
        Avail = avail_use(avail_use_info(ModuleName, Context, SeqNum))
    ),
    !:AvailsCord = cord.snoc(!.AvailsCord, Avail).

%---------------------------------------------------------------------------%

:- pred order_items(list(item)::in, list(item)::out) is det.

order_items(Items, OrderedItems) :-
    classify_items(Items,
        map.init, TypeDefnMap,
        map.init, InstDefnMap,
        map.init, ModeDefnMap,
        map.init, PredRelatedMap,
        set.init, SortableItems,
        cord.init, NonReorderableItemsCord),
    NonReorderableItems = cord.list(NonReorderableItemsCord),
    expect(unify(NonReorderableItems, []), $pred,
        "non-reorderable items in interface"),
    some [!OrderedItemsCord] (
        !:OrderedItemsCord = cord.init,
        map.foldl_values(append_sym_name_map_items, TypeDefnMap,
            !OrderedItemsCord),
        map.foldl_values(append_sym_name_map_items, InstDefnMap,
            !OrderedItemsCord),
        map.foldl_values(append_sym_name_map_items, ModeDefnMap,
            !OrderedItemsCord),
        map.foldl_values(append_pred_related_items, PredRelatedMap,
            !OrderedItemsCord),
        !:OrderedItemsCord = !.OrderedItemsCord ++
            cord.from_list(set.to_sorted_list(SortableItems)),
        OrderedItems = cord.list(!.OrderedItemsCord)
    ).

%---------------------------------------------------------------------------%

:- type sym_name_items_map == map(sym_name_and_arity, cord(item)).
:- type pred_related_items_map == map(sym_name, pred_related_items).

:- type are_arities_pfs_known
    --->    some_arities_pfs_are_unknown
    ;       all_arities_pfs_are_known.

:- type arity_pf
    --->    arity_pf(int, pred_or_func).

:- type pred_related_items
    --->    pred_related_items(
                prs_arities_pfs_known   :: are_arities_pfs_known,

                % The next two field contain redundant information;
                % we only use one. Which one that is depends on the
                % value of prs_arities_pfs_known.

                % If there is a pred_decl and/or mode_decl item for this
                % sym_name for which we don't know either its arity
                % or whether it applies to a predicate or a function
                % (due to their use of with_type and/or with_inst annotations),
                % then we print all the pred and mode declarations
                % for this sym_name in their original order. This field
                % contains them in that order.
                prs_all_items           :: cord(item),

                % If we know the arity and the pred_or_func for all the
                % pred_decl and mode_decl items for this sym_name, then
                % we can and do print the pred and mode declarations
                % for each arity/pf combination separately. This field
                % contains all the predicate and mode declarations
                % for which we know the arity and the pred_or_func.
                prs_arity_pf_items      :: map(arity_pf, arity_pf_items)
            ).

:- type arity_pf_items
    --->    arity_pf_items(
                apfi_pred_decl_items    :: cord(item),
                % There should be exactly one item_pred_decl for any
                % sym_name/arity/pred_or_func combination that has any
                % item_mode_decl, but using a cord simplifies the code.

                apfi_mode_decl_items    :: cord(item)
                % There may be any number of item_mode_decls for any
                % sym_name/arity/pred_or_func combination that has
                % an item_pred_decl, from zero on up.

                % We could have a third field here for pragmas related
                % to the predicate, for more "natural-looking" output.
            ).

%---------------------------------------------------------------------------%

    % Classify the given list of items into the different categories
    % needed by order_items.
    %
    % No interface file of any kind should ever include any of the kinds
    % of items that we put into NonReorderableItems, so we could delete
    % the !NonReorderableItemsCord argument pair. However, .opt files
    % *may* contain such items (such as clauses). So we keep that argument
    % pair to allow us to use this predicate to canonicalize the contents
    % of .opt (and .trans_opt) files in the future.
    %
:- pred classify_items(list(item)::in,
    sym_name_items_map::in, sym_name_items_map::out,
    sym_name_items_map::in, sym_name_items_map::out,
    sym_name_items_map::in, sym_name_items_map::out,
    pred_related_items_map::in, pred_related_items_map::out,
    set(item)::in, set(item)::out,
    cord(item)::in, cord(item)::out) is det.

classify_items([], !TypeDefnMap, !InstDefnMap, !ModeDefnMap,
        !PredRelatedMap, !SortableItems, !NonReorderableItemsCord).
classify_items([Item | Items], !TypeDefnMap, !InstDefnMap, !ModeDefnMap,
        !PredRelatedMap, !SortableItems, !NonReorderableItemsCord) :-
    (
        Item = item_type_defn(ItemTypeDefnInfo),
        ItemTypeDefnInfo = item_type_defn_info(SymName, Params, _, _, _, _),
        list.length(Params, Arity),
        SymNameAndArity = sym_name_arity(SymName, Arity),
        add_to_sym_name_items_map(SymNameAndArity, Item, !TypeDefnMap)
    ;
        Item = item_inst_defn(ItemInstDefnInfo),
        ItemInstDefnInfo = item_inst_defn_info(SymName, Params, _, _, _, _, _),
        list.length(Params, Arity),
        SymNameAndArity = sym_name_arity(SymName, Arity),
        add_to_sym_name_items_map(SymNameAndArity, Item, !InstDefnMap)
    ;
        Item = item_mode_defn(ItemModeDefnInfo),
        ItemModeDefnInfo = item_mode_defn_info(SymName, Params, _, _, _, _),
        list.length(Params, Arity),
        SymNameAndArity = sym_name_arity(SymName, Arity),
        add_to_sym_name_items_map(SymNameAndArity, Item, !ModeDefnMap)
    ;
        Item = item_pred_decl(ItemPredDeclInfo),
        ItemPredDeclInfo = item_pred_decl_info(SymName, PorF, Args,
            MaybeWithType, MaybeWithInst, _, _, _, _, _, _, _, _, _),
        ( if
            MaybeWithType = no,
            MaybeWithInst = no
        then
            list.length(Args, Arity),
            ArityPf = arity_pf(Arity, PorF),
            ( if map.search(!.PredRelatedMap, SymName, PredRelated0) then
                PredRelated0 =
                    pred_related_items(Known, AllItems0, ArityPfMap0),
                AllItems = cord.snoc(AllItems0, Item),
                ( if map.search(ArityPfMap0, ArityPf, ArityPfItems0) then
                    ArityPfItems0 = arity_pf_items(PredItems0, ModeItems),
                    PredItems = cord.snoc(PredItems0, Item),
                    ArityPfItems = arity_pf_items(PredItems, ModeItems),
                    map.det_update(ArityPf, ArityPfItems,
                        ArityPfMap0, ArityPfMap)
                else
                    PredItems = cord.singleton(Item),
                    ModeItems = cord.init,
                    ArityPfItems = arity_pf_items(PredItems, ModeItems),
                    map.det_insert(ArityPf, ArityPfItems,
                        ArityPfMap0, ArityPfMap)
                ),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_update(SymName, PredRelated, !PredRelatedMap)
            else
                Known = all_arities_pfs_are_known,
                AllItems = cord.singleton(Item),
                PredItems = cord.singleton(Item),
                ModeItems = cord.init,
                ArityPfItems = arity_pf_items(PredItems, ModeItems),
                ArityPfMap = map.singleton(ArityPf, ArityPfItems),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_insert(SymName, PredRelated, !PredRelatedMap)
            )
        else
            Known = some_arities_pfs_are_unknown,
            ( if map.search(!.PredRelatedMap, SymName, PredRelated0) then
                PredRelated0 =
                    pred_related_items(_Known0, AllItems0, ArityPfMap),
                AllItems = cord.snoc(AllItems0, Item),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_update(SymName, PredRelated, !PredRelatedMap)
            else
                AllItems = cord.singleton(Item),
                map.init(ArityPfMap),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_insert(SymName, PredRelated, !PredRelatedMap)
            )
        )
    ;
        Item = item_mode_decl(ItemModeDeclInfo),
        ItemModeDeclInfo = item_mode_decl_info(SymName, MaybePorF, Args,
            MaybeWithInst, _, _, _, _),
        ( if
            MaybePorF = yes(PorF),
            MaybeWithInst = no
        then
            list.length(Args, Arity),
            ArityPf = arity_pf(Arity, PorF),
            ( if map.search(!.PredRelatedMap, SymName, PredRelated0) then
                PredRelated0 =
                    pred_related_items(Known, AllItems0, ArityPfMap0),
                AllItems = cord.snoc(AllItems0, Item),
                ( if map.search(ArityPfMap0, ArityPf, ArityPfItems0) then
                    ArityPfItems0 = arity_pf_items(PredItems, ModeItems0),
                    ModeItems = cord.snoc(ModeItems0, Item),
                    ArityPfItems = arity_pf_items(PredItems, ModeItems),
                    map.det_update(ArityPf, ArityPfItems,
                        ArityPfMap0, ArityPfMap)
                else
                    PredItems = cord.init,
                    ModeItems = cord.singleton(Item),
                    ArityPfItems = arity_pf_items(PredItems, ModeItems),
                    map.det_insert(ArityPf, ArityPfItems,
                        ArityPfMap0, ArityPfMap)
                ),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_update(SymName, PredRelated, !PredRelatedMap)
            else
                Known = all_arities_pfs_are_known,
                AllItems = cord.singleton(Item),
                PredItems = cord.init,
                ModeItems = cord.singleton(Item),
                ArityPfItems = arity_pf_items(PredItems, ModeItems),
                ArityPfMap = map.singleton(ArityPf, ArityPfItems),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_insert(SymName, PredRelated, !PredRelatedMap)
            )
        else
            Known = some_arities_pfs_are_unknown,
            ( if map.search(!.PredRelatedMap, SymName, PredRelated0) then
                PredRelated0 =
                    pred_related_items(_Known0, AllItems0, ArityPfMap),
                AllItems = cord.snoc(AllItems0, Item),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_update(SymName, PredRelated, !PredRelatedMap)
            else
                AllItems = cord.singleton(Item),
                map.init(ArityPfMap),
                PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
                map.det_insert(SymName, PredRelated, !PredRelatedMap)
            )
        )
    ;
        Item = item_pragma(ItemPragmaInfo),
        ItemPragmaInfo = item_pragma_info(Pragma, _, _, _),
        (
            ( Pragma = pragma_foreign_proc_export(_)
            ; Pragma = pragma_foreign_export_enum(_)
            ; Pragma = pragma_foreign_enum(_)
            ; Pragma = pragma_external_proc(_)
            ; Pragma = pragma_type_spec(_)
            ; Pragma = pragma_inline(_)
            ; Pragma = pragma_no_inline(_)
            ; Pragma = pragma_consider_used(_)
            ; Pragma = pragma_unused_args(_)
            ; Pragma = pragma_exceptions(_)
            ; Pragma = pragma_trailing_info(_)
            ; Pragma = pragma_mm_tabling_info(_)
            ; Pragma = pragma_obsolete(_)
            ; Pragma = pragma_no_detism_warning(_)
            ; Pragma = pragma_tabled(_)
            ; Pragma = pragma_fact_table(_)
            ; Pragma = pragma_oisu(_)
            ; Pragma = pragma_promise_eqv_clauses(_)
            ; Pragma = pragma_promise_pure(_)
            ; Pragma = pragma_promise_semipure(_)
            ; Pragma = pragma_termination_info(_)
            ; Pragma = pragma_termination2_info(_)
            ; Pragma = pragma_terminates(_)
            ; Pragma = pragma_does_not_terminate(_)
            ; Pragma = pragma_check_termination(_)
            ; Pragma = pragma_mode_check_clauses(_)
            ; Pragma = pragma_structure_sharing(_)
            ; Pragma = pragma_structure_reuse(_)
            ; Pragma = pragma_require_feature_set(_)
            ; Pragma = pragma_require_tail_recursion(_)
            ),
            set.insert(Item, !SortableItems)
        ;
            ( Pragma = pragma_foreign_decl(_)
            ; Pragma = pragma_foreign_code(_)
            ; Pragma = pragma_foreign_proc(_)
            ),
            !:NonReorderableItemsCord =
                cord.snoc(!.NonReorderableItemsCord, Item)
        )
    ;
        ( Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_type_repn(_)
        ),
        set.insert(Item, !SortableItems)
    ;
        ( Item = item_clause(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ),
        !:NonReorderableItemsCord = cord.snoc(!.NonReorderableItemsCord, Item)
    ),
    classify_items(Items, !TypeDefnMap, !InstDefnMap, !ModeDefnMap,
        !PredRelatedMap, !SortableItems, !NonReorderableItemsCord).

:- pred add_to_sym_name_items_map(sym_name_and_arity::in, item::in,
    sym_name_items_map::in, sym_name_items_map::out) is det.

add_to_sym_name_items_map(SymNameAndArity, Item, !SymNameItemsMap) :-
    ( if map.search(!.SymNameItemsMap, SymNameAndArity, OldItems) then
        NewItems = cord.snoc(OldItems, Item),
        map.det_update(SymNameAndArity, NewItems, !SymNameItemsMap)
    else
        NewItems = cord.singleton(Item),
        map.det_insert(SymNameAndArity, NewItems, !SymNameItemsMap)
    ).

%---------------------------------------------------------------------------%

:- pred append_sym_name_map_items(cord(item)::in,
    cord(item)::in, cord(item)::out) is det.

append_sym_name_map_items(SymNameItemsCord, !ItemsCord) :-
    !:ItemsCord = !.ItemsCord ++ SymNameItemsCord.

:- pred append_pred_related_items(pred_related_items::in,
    cord(item)::in, cord(item)::out) is det.

append_pred_related_items(PredRelated, !ItemsCord) :-
    PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
    (
        Known = all_arities_pfs_are_known,
        % We know, for each pred and mode declaration, the actual arity
        % and pred_or_func of the "predicate" they apply to. This allows us
        % to order the declarations we output by
        %
        % - lower arities before higher arities,
        % - functions before predicates,
        % - "predicate" declarations (for functions as well as actual
        %   predicates) before their mode declarations.
        %
        % The first two criteria are enforced by the key type of the map,
        % and the third by append_arity_pf_items.
        %
        % Specifying this precise an order keeps the interface file unchanged
        % even if the order of the items involved changes in the source code.
        map.foldl_values(append_arity_pf_items, ArityPfMap, !ItemsCord)
    ;
        Known = some_arities_pfs_are_unknown,
        % We cannot do what we do above, because for at least one pred
        % or mode declaration, we do not know either its arity, or
        % whether it belongs to a function or to a predicate.
        % Reordering mode declarations for the same entity (predicate or
        % function) could change the meaning of the program.
        %
        % Reordering pred declarations cannot change the meaning of the
        % program (since each of those entities must have exactly one),
        % but treating them differently from mode declarations would
        % not be worthwhile, since predicate declarations with with_type
        % and/or with_inst annotations are very rare. (With_inst annotations
        % are possible on item_pred_decls that contain a combined predmode
        % declaration.)
        !:ItemsCord = !.ItemsCord ++ AllItems
    ).

:- pred append_arity_pf_items(arity_pf_items::in,
    cord(item)::in, cord(item)::out) is det.

append_arity_pf_items(ArityPfItems, !ItemsCord) :-
    ArityPfItems = arity_pf_items(PredDeclItems, ModeDeclItems),
    !:ItemsCord = !.ItemsCord ++ PredDeclItems ++ ModeDeclItems.

%---------------------------------------------------------------------------%
:- end_module parse_tree.canonicalize_interface.
%---------------------------------------------------------------------------%
