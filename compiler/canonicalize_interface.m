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
% The original job of this module was to put the contents of an interface file
% into a canonical order. Without this, a semantically-null change such as
% reordering some declarations in a module's interface  would cause
% that module's interface files to change, which would then require
% the recompilation of all *other* modules that import those interface files.
%
% Since we switched to representing the contents of interface files using
% file-kind-specific parse_tree_intNs instead of generic parse_tree_ints,
% in which the different kinds of items are already separated from each other,
% most of this task is now done by mercury_output_parse_tree_intN in
% parse_tree_out.m. The one part that remains to be done here is putting
% predicate and mode declarations into canonical order.
%
%---------------------------------------------------------------------------%

:- module parse_tree.canonicalize_interface.
:- interface.

:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

:- type pred_or_mode_decl_item
    --->    pomd_pred(item_pred_decl_info)
    ;       pomd_mode(item_mode_decl_info).

:- pred order_pred_and_mode_decls(
    list(item_pred_decl_info)::in, list(item_mode_decl_info)::in,
    list(pred_or_mode_decl_item)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module cord.
:- import_module map.
:- import_module maybe.
:- import_module term.

%---------------------------------------------------------------------------%

order_pred_and_mode_decls(PredDecls, ModeDecls, PredOrModeDecls) :-
    some [!PredRelatedMap] (
        map.init(!:PredRelatedMap),
        list.foldl(classify_item_pred_decl, PredDecls, !PredRelatedMap),
        list.foldl(classify_item_mode_decl, ModeDecls, !PredRelatedMap),
        map.foldl_values(append_pred_related, !.PredRelatedMap,
            cord.init, PredOrModeDeclsCord)
    ),
    PredOrModeDecls = cord.list(PredOrModeDeclsCord).

%---------------------------------------------------------------------------%

:- type sym_name_items_map == map(sym_name_arity, cord(item)).
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
                prs_all_items           :: cord(pred_or_mode_decl_item),

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
                apfi_pred_decl_items    :: cord(item_pred_decl_info),
                % There should be exactly one item_pred_decl for any
                % sym_name/arity/pred_or_func combination that has any
                % item_mode_decl, but using a cord simplifies the code.

                apfi_mode_decl_items    :: cord(item_mode_decl_info)
                % There may be any number of item_mode_decls for any
                % sym_name/arity/pred_or_func combination that has
                % an item_pred_decl, from zero on up.

                % We could have a third field here for pragmas related
                % to the predicate, for more "natural-looking" output.
            ).

%---------------------------------------------------------------------------%

:- pred classify_item_pred_decl(item_pred_decl_info::in,
    pred_related_items_map::in, pred_related_items_map::out) is det.

classify_item_pred_decl(ItemPredDeclInfo, !PredRelatedMap) :-
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
            AllItems = cord.snoc(AllItems0, pomd_pred(ItemPredDeclInfo)),
            ( if map.search(ArityPfMap0, ArityPf, ArityPfItems0) then
                ArityPfItems0 = arity_pf_items(PredItems0, ModeItems),
                PredItems = cord.snoc(PredItems0, ItemPredDeclInfo),
                ArityPfItems = arity_pf_items(PredItems, ModeItems),
                map.det_update(ArityPf, ArityPfItems,
                    ArityPfMap0, ArityPfMap)
            else
                PredItems = cord.singleton(ItemPredDeclInfo),
                ModeItems = cord.init,
                ArityPfItems = arity_pf_items(PredItems, ModeItems),
                map.det_insert(ArityPf, ArityPfItems,
                    ArityPfMap0, ArityPfMap)
            ),
            PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
            map.det_update(SymName, PredRelated, !PredRelatedMap)
        else
            Known = all_arities_pfs_are_known,
            AllItems = cord.singleton(pomd_pred(ItemPredDeclInfo)),
            PredItems = cord.singleton(ItemPredDeclInfo),
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
            AllItems = cord.snoc(AllItems0, pomd_pred(ItemPredDeclInfo)),
            PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
            map.det_update(SymName, PredRelated, !PredRelatedMap)
        else
            AllItems = cord.singleton(pomd_pred(ItemPredDeclInfo)),
            map.init(ArityPfMap),
            PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
            map.det_insert(SymName, PredRelated, !PredRelatedMap)
        )
    ).

:- pred classify_item_mode_decl(item_mode_decl_info::in,
    pred_related_items_map::in, pred_related_items_map::out) is det.

classify_item_mode_decl(ItemModeDeclInfo, !PredRelatedMap) :-
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
            AllItems = cord.snoc(AllItems0, pomd_mode(ItemModeDeclInfo)),
            ( if map.search(ArityPfMap0, ArityPf, ArityPfItems0) then
                ArityPfItems0 = arity_pf_items(PredItems, ModeItems0),
                ModeItems = cord.snoc(ModeItems0, ItemModeDeclInfo),
                ArityPfItems = arity_pf_items(PredItems, ModeItems),
                map.det_update(ArityPf, ArityPfItems,
                    ArityPfMap0, ArityPfMap)
            else
                PredItems = cord.init,
                ModeItems = cord.singleton(ItemModeDeclInfo),
                ArityPfItems = arity_pf_items(PredItems, ModeItems),
                map.det_insert(ArityPf, ArityPfItems,
                    ArityPfMap0, ArityPfMap)
            ),
            PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
            map.det_update(SymName, PredRelated, !PredRelatedMap)
        else
            Known = all_arities_pfs_are_known,
            AllItems = cord.singleton(pomd_mode(ItemModeDeclInfo)),
            PredItems = cord.init,
            ModeItems = cord.singleton(ItemModeDeclInfo),
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
            AllItems = cord.snoc(AllItems0, pomd_mode(ItemModeDeclInfo)),
            PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
            map.det_update(SymName, PredRelated, !PredRelatedMap)
        else
            AllItems = cord.singleton(pomd_mode(ItemModeDeclInfo)),
            map.init(ArityPfMap),
            PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
            map.det_insert(SymName, PredRelated, !PredRelatedMap)
        )
    ).

%---------------------------------------------------------------------------%

:- pred append_pred_related(pred_related_items::in,
    cord(pred_or_mode_decl_item)::in, cord(pred_or_mode_decl_item)::out)
    is det.

append_pred_related(PredRelated, !PredOrModeDeclsCord) :-
    PredRelated = pred_related_items(Known, AllItems, ArityPfMap),
    (
        Known = all_arities_pfs_are_known,
        map.foldl_values(append_arity_pf, ArityPfMap,
            !PredOrModeDeclsCord)
    ;
        Known = some_arities_pfs_are_unknown,
        !:PredOrModeDeclsCord = !.PredOrModeDeclsCord ++ AllItems
    ).

:- pred append_arity_pf(arity_pf_items::in,
    cord(pred_or_mode_decl_item)::in, cord(pred_or_mode_decl_item)::out)
    is det.

append_arity_pf(ArityPfItems, !PredOrModeDeclsCord) :-
    ArityPfItems = arity_pf_items(PredDecls, ModeDecls),
    WrapPred = (func(P) = pomd_pred(P)),
    WrapMode = (func(M) = pomd_mode(M)),
    !:PredOrModeDeclsCord = !.PredOrModeDeclsCord ++
        cord.map(WrapPred, PredDecls) ++
        cord.map(WrapMode, ModeDecls).

%---------------------------------------------------------------------------%
:- end_module parse_tree.canonicalize_interface.
%---------------------------------------------------------------------------%
