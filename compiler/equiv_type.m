%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: equiv_type.m.
% Main author: fjh.
% 
% This module contains a parse-tree to parse-tree transformation
% that expands equivalence types. It also expands away `with_type`
% and `with_inst` annotations on predicate and function type declarations.
% 
%-----------------------------------------------------------------------------%

:- module parse_tree.equiv_type.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % expand_eqv_types(ModuleName, Items0, Items,
    %   CircularTypes, EqvMap, MaybeRecompInfo0, MaybeRecompInfo).
    %
    % First it builds up a map from type_ctor to the equivalent type.
    % Then it traverses through the list of items, expanding all types.
    % This has the effect of eliminating all the equivalence types
    % from the source code.
    %
    % `with_type` and `with_inst` annotations on predicate and
    % function type declarations are also expaneded.
    %
    % Error messages are generated for any circular equivalence types
    % and invalid `with_type` and `with_inst` annotations.
    %
    % For items not defined in the current module, the items expanded
    % while processing each item are recorded in the recompilation_info,
    % for use by smart recompilation.
    %
:- pred expand_eqv_types(module_name::in,
    list(item_and_context)::in, list(item_and_context)::out,
    bool::out, eqv_map::out, used_modules::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    io::di, io::uo) is det.

    % Replace equivalence types in a given type.
    % The bool output is `yes' if anything changed.
    %
:- pred replace_in_type(eqv_map::in, mer_type::in, mer_type::out,
    bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_type_list(eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out,
    tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_prog_constraints(eqv_map::in,
    prog_constraints::in, prog_constraints::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_prog_constraint(eqv_map::in,
    prog_constraint::in, prog_constraint::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- pred replace_in_ctors(eqv_map::in,
    list(constructor)::in, list(constructor)::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out) is det.

:- type eqv_type_body
    --->    eqv_type_body(
                tvarset,
                list(type_param),
                mer_type
            ).

:- type eqv_map == map(type_ctor, eqv_type_body).

:- type equiv_type_info == maybe(expanded_item_set).
:- type expanded_item_set.

    % For smart recompilation we need to record which items were
    % expanded in each declaration.  Any items which depend on
    % that declaration also depend on the expanded items.
    %
:- pred maybe_record_expanded_items(module_name::in,
    sym_name::in, maybe(recompilation_info)::in,
    equiv_type_info::out) is det.

    % Record all the expanded items in the recompilation_info.
    %
:- pred finish_recording_expanded_items(item_id::in,
    equiv_type_info::in, maybe(recompilation_info)::in,
    maybe(recompilation_info)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module assoc_list.
:- import_module bool.
:- import_module pair.
:- import_module set.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type eqv_type_location
    --->    eqv_type_out_of_module
    ;       eqv_type_in_interface
    ;       eqv_type_in_implementation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % First we build up a mapping which records the equivalence type
    % definitions.  Then we go through the item list and replace them.
    %
expand_eqv_types(ModuleName,
        Items0, Items, Error, EqvMap, UsedModules, !Info, !IO) :-
    map.init(EqvMap0),
    map.init(EqvInstMap0),
    build_eqv_map(Items0, EqvMap0, EqvMap, EqvInstMap0, EqvInstMap),
    replace_in_item_list(ModuleName, eqv_type_out_of_module, Items0,
        EqvMap, EqvInstMap, [], RevItems, [], ErrorList, !Info,
        used_modules_init, UsedModules),
    list.reverse(RevItems, Items),
    (
        ErrorList = [],
        Error = no
    ;
        ErrorList = [_ | _],
        list.foldl(report_error, list.reverse(ErrorList), !IO),
        Error = yes,
        io.set_exit_status(1, !IO)
    ).

    % We need to expand equivalence insts in
    % `:- pred p `with_inst` i' declarations.
:- type eqv_inst_body
    --->    eqv_inst_body(
                inst_varset,
                list(inst_var),
                mer_inst
            ).

:- type eqv_inst_map == map(inst_id, eqv_inst_body).

:- type pred_or_func_decl_type
    --->    type_decl
    ;       mode_decl.

:- type eqv_error == pair(eqv_error_type, prog_context).

:- type eqv_error_type
    --->    circular_equivalence(item)
    ;       invalid_with_type(sym_name, pred_or_func)
    ;       invalid_with_inst(pred_or_func_decl_type,
                sym_name, maybe(pred_or_func))
    ;       non_matching_with_type_with_inst(sym_name, pred_or_func).

:- pred build_eqv_map(list(item_and_context)::in,
    eqv_map::in, eqv_map::out, eqv_inst_map::in, eqv_inst_map::out) is det.

build_eqv_map([], !EqvMap, !EqvInstMap).
build_eqv_map([Item - _Context | Items0], !EqvMap, !EqvInstMap) :-
    ( Item = item_module_defn(_, md_abstract_imported) ->
        skip_abstract_imported_items(Items0, Items)
    ; Item = item_type_defn(VarSet, Name, Args, parse_tree_eqv_type(Body), _) ->
        Items = Items0,
        list.length(Args, Arity),
        TypeCtor = type_ctor(Name, Arity),
        svmap.set(TypeCtor, eqv_type_body(VarSet, Args, Body), !EqvMap)
    ; Item = item_inst_defn(VarSet, Name, Args, eqv_inst(Body), _) ->
        Items = Items0,
        list.length(Args, Arity),
        InstId = inst_id(Name, Arity),
        svmap.set(InstId, eqv_inst_body(VarSet, Args, Body), !EqvInstMap)
    ;
        Items = Items0
    ),
    build_eqv_map(Items, !EqvMap, !EqvInstMap).

:- pred skip_abstract_imported_items(list(item_and_context)::in,
    list(item_and_context)::out) is det.

skip_abstract_imported_items([], []).
skip_abstract_imported_items([Item - _ | Items0], Items) :-
    (
        Item = item_module_defn(_, Defn),
        is_section_defn(Defn) = yes,
        Defn \= md_abstract_imported
    ->
        Items = Items0
    ;
        skip_abstract_imported_items(Items0, Items)
    ).

:- func is_section_defn(module_defn) = bool.

is_section_defn(md_module(_)) = yes.
is_section_defn(md_end_module(_)) = yes.
is_section_defn(md_interface) = yes.
is_section_defn(md_implementation) = yes.
is_section_defn(md_private_interface) = yes.
is_section_defn(md_imported(_)) = yes.
is_section_defn(md_used(_)) = yes.
is_section_defn(md_abstract_imported) = yes.
is_section_defn(md_opt_imported) = yes.
is_section_defn(md_transitively_imported) = yes.
is_section_defn(md_external(_, _)) = no.
is_section_defn(md_export(_)) = no.
is_section_defn(md_import(_)) = no.
is_section_defn(md_use(_)) = no.
is_section_defn(md_include_module(_)) = no.
is_section_defn(md_version_numbers(_, _)) = no.

    % The following predicate replace_in_item_list performs substitution
    % of equivalence types on a list of items. Similarly the replace_in_<foo>
    % predicates that follow perform substitution of equivalence types
    % on <foo>s.
    %
:- pred replace_in_item_list(module_name::in, eqv_type_location::in,
    list(item_and_context)::in, eqv_map::in, eqv_inst_map::in,
    list(item_and_context)::in, list(item_and_context)::out,
    list(eqv_error)::in, list(eqv_error)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out) is det.

replace_in_item_list(_, _, [], _, _, !Items, !Errors, !Info, !UsedModules).
replace_in_item_list(ModuleName, Location0, [ItemAndContext0 | Items0],
        EqvMap, EqvInstMap, !ReplItems, !Errors, !Info, !UsedModules) :-
    ItemAndContext0 = Item0 - Context,
    ( Item0 = item_module_defn(_, ModuleDefn) ->
        ( ModuleDefn = md_interface,
            Location = eqv_type_in_interface
        ;
            ( ModuleDefn = md_implementation
            ; ModuleDefn = md_private_interface
            ),
            Location = eqv_type_in_implementation
        ;

            ( ModuleDefn = md_imported(_)
            ; ModuleDefn = md_used(_)
            ; ModuleDefn = md_abstract_imported
            ; ModuleDefn = md_opt_imported
            ; ModuleDefn = md_transitively_imported
                % XXX I'm not sure what these two are so they may not signify
                % that we've finished processing the module.
            ; ModuleDefn = md_external(_, _)
            ; ModuleDefn = md_export(_)
            ),
            Location = eqv_type_out_of_module
        ; 
            ( ModuleDefn = md_module(_)
            ; ModuleDefn = md_end_module(_)
            ; ModuleDefn = md_import(_)
            ; ModuleDefn = md_use(_)
            ; ModuleDefn = md_include_module(_)
            ; ModuleDefn = md_version_numbers(_, _)
            ),
            Location = Location0
        )
    ;
        Location = Location0
    ),
    (
        replace_in_item(ModuleName, Location, Item0, Context, EqvMap,
            EqvInstMap, Item, NewErrors, !Info, !UsedModules)
    ->
        ItemAndContext = Item - Context,

        % Discard the item if there were any errors.
        (
            NewErrors = [],
            !:ReplItems = [ItemAndContext | !.ReplItems]
        ;
            NewErrors = [_ | _]
        ),
        !:Errors = NewErrors ++ !.Errors
    ;
        ItemAndContext = ItemAndContext0,
        !:ReplItems = [ItemAndContext | !.ReplItems]
    ),
    replace_in_item_list(ModuleName, Location, Items0, EqvMap, EqvInstMap,
        !ReplItems, !Errors, !Info, !UsedModules).

:- pred replace_in_item(module_name::in, eqv_type_location::in, item::in,
    prog_context::in, eqv_map::in, eqv_inst_map::in, item::out,
    list(eqv_error)::out,
    maybe(recompilation_info)::in, maybe(recompilation_info)::out,
    used_modules::in, used_modules::out) is semidet.

replace_in_item(ModuleName, Location,
        item_type_defn(VarSet0, Name, TArgs, TypeDefn0, Cond) @ Item,
        Context, EqvMap, _EqvInstMap,
        item_type_defn(VarSet, Name, TArgs, TypeDefn, Cond),
        Error, !Info, !UsedModules) :-
    list.length(TArgs, Arity),
    maybe_record_expanded_items(ModuleName, Name, !.Info, UsedTypeCtors0),
    replace_in_type_defn(Location, EqvMap, type_ctor(Name, Arity), TypeDefn0,
        TypeDefn, ContainsCirc, VarSet0, VarSet,
        UsedTypeCtors0, UsedTypeCtors, !UsedModules),
    (
        ContainsCirc = yes,
        Error = [circular_equivalence(Item) - Context]
    ;
        ContainsCirc = no,
        Error = []
    ),
    ItemId = item_id(type_body_item, item_name(Name, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !Info).

replace_in_item(ModuleName, Location,
        item_pred_or_func(Origin, TypeVarSet0, InstVarSet, ExistQVars,
            PredOrFunc, PredName, TypesAndModes0, MaybeWithType0,
            MaybeWithInst0, Det0, Cond, Purity, ClassContext0),
        Context, EqvMap, EqvInstMap,
        item_pred_or_func(Origin, TypeVarSet, InstVarSet, ExistQVars,
            PredOrFunc, PredName, TypesAndModes, MaybeWithType,
            MaybeWithInst, Det, Cond, Purity, ClassContext),
        Errors, !Info, !UsedModules) :-
    maybe_record_expanded_items(ModuleName, PredName, !.Info, ExpandedItems0),

    replace_in_pred_type(Location, PredName, PredOrFunc, Context, EqvMap,
        EqvInstMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        Det0, Det, ExpandedItems0, ExpandedItems, !UsedModules, Errors),

    ItemType = pred_or_func_to_item_type(PredOrFunc),
    list.length(TypesAndModes, Arity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !Info).

replace_in_item(ModuleName, Location,
        item_pred_or_func_mode(InstVarSet, MaybePredOrFunc0, PredName,
            Modes0, WithInst0, Det0, Cond),
        Context, _EqvMap, EqvInstMap,
        item_pred_or_func_mode(InstVarSet, MaybePredOrFunc, PredName,
            Modes, WithInst, Det, Cond),
        Errors, !Info, !UsedModules) :-
    maybe_record_expanded_items(ModuleName, PredName, !.Info, ExpandedItems0),

    replace_in_pred_mode(Location, PredName, length(Modes0), Context,
        mode_decl, EqvInstMap, MaybePredOrFunc0, MaybePredOrFunc,
        ExtraModes, WithInst0, WithInst, Det0, Det,
        ExpandedItems0, ExpandedItems, !UsedModules, Errors),
    (
        ExtraModes = [],
        Modes = Modes0
    ;
        ExtraModes = [_ | _],
        Modes = Modes0 ++ ExtraModes
    ),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        ItemType = pred_or_func_to_item_type(PredOrFunc),
        list.length(Modes, Arity),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        ItemId = item_id(ItemType, item_name(PredName, OrigArity)),
        finish_recording_expanded_items(ItemId, ExpandedItems, !Info)
    ;
        MaybePredOrFunc = no
    ).

replace_in_item(ModuleName, Location,
        item_typeclass(Constraints0, FunDeps, ClassName, Vars,
            ClassInterface0, VarSet0),
        _Context, EqvMap, EqvInstMap,
        item_typeclass(Constraints, FunDeps, ClassName, Vars,
            ClassInterface, VarSet),
        Errors, !Info, !UsedModules) :-
    list.length(Vars, Arity),
    maybe_record_expanded_items(ModuleName, ClassName, !.Info, ExpandedItems0),
    replace_in_prog_constraint_list(Location, EqvMap,
        Constraints0, Constraints, VarSet0, VarSet,
        ExpandedItems0, ExpandedItems1, !UsedModules),
    (
        ClassInterface0 = class_interface_abstract,
        ClassInterface = class_interface_abstract,
        ExpandedItems = ExpandedItems1,
        Errors = []
    ;
        ClassInterface0 = class_interface_concrete(Methods0),
        replace_in_class_interface(Location, Methods0, EqvMap, EqvInstMap,
            Methods, [], Errors, ExpandedItems1, ExpandedItems, !UsedModules),
        ClassInterface = class_interface_concrete(Methods)
    ),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !Info).

replace_in_item(ModuleName, Location,
        item_instance(Constraints0, ClassName, Ts0, InstanceBody, VarSet0,
            ModName),
        _Context, EqvMap, _EqvInstMap,
        item_instance(Constraints, ClassName, Ts, InstanceBody, VarSet,
            ModName),
        [], !Info, !UsedModules) :-
    (
        ( !.Info = no
        ; ModName = ModuleName
        )
    ->
        UsedTypeCtors0 = no
    ;
        UsedTypeCtors0 = yes(ModuleName - set.init)
    ),
    replace_in_prog_constraint_list(Location, EqvMap,
        Constraints0, Constraints, VarSet0, VarSet1,
        UsedTypeCtors0, UsedTypeCtors1, !UsedModules),
    replace_in_type_list_location_circ(Location, EqvMap, Ts0, Ts, _, _,
        VarSet1, VarSet, UsedTypeCtors1, UsedTypeCtors, !UsedModules),
    list.length(Ts0, Arity),
    ItemId = item_id(typeclass_item, item_name(ClassName, Arity)),
    finish_recording_expanded_items(ItemId, UsedTypeCtors, !Info).

replace_in_item(ModuleName, Location,
        item_pragma(Origin, pragma_type_spec(PredName, B, Arity, D, E,
            Subst0, VarSet0, ItemIds0)),
        _Context, EqvMap, _EqvInstMap,
        item_pragma(Origin, pragma_type_spec(PredName, B, Arity, D, E,
            Subst, VarSet, ItemIds)),
        [], !Info, !UsedModules) :-
    (
        ( !.Info = no
        ; PredName = qualified(ModuleName, _)
        )
    ->
        ExpandedItems0 = no
    ;
        ExpandedItems0 = yes(ModuleName - ItemIds0)
    ),
    replace_in_subst(Location, EqvMap, Subst0, Subst, VarSet0, VarSet,
        ExpandedItems0, ExpandedItems, !UsedModules),
    (
        ExpandedItems = no,
        ItemIds = ItemIds0
    ;
        ExpandedItems = yes(_ - ItemIds)
    ).

replace_in_item(ModuleName, Location,
        item_pragma(Origin, pragma_foreign_proc(Attrs0, PName, PredOrFunc, 
            ProcVars, ProcVarset, ProcInstVarset, ProcImpl)),
        _Context, EqvMap, _EqvInstMap,
        item_pragma(Origin, pragma_foreign_proc(Attrs, PName, PredOrFunc, 
            ProcVars, ProcVarset, ProcInstVarset, ProcImpl)),
        [], !Info, !UsedModules) :-
    some [!EquivTypeInfo] (
        maybe_record_expanded_items(ModuleName, PName,
            !.Info, !:EquivTypeInfo),
        UserSharing0 = get_user_annotated_sharing(Attrs0), 
        (   
            UserSharing0 = user_sharing(Sharing0, MaybeTypes0),
            MaybeTypes0 = yes(user_type_info(Types0, TVarset0))
        ->
            replace_in_type_list_location(Location,
                EqvMap, Types0, Types, _AnythingChanged, 
                TVarset0, TVarset, !EquivTypeInfo, !UsedModules),
            replace_in_structure_sharing_domain(Location,
                EqvMap, Sharing0, Sharing,
                TVarset0, !EquivTypeInfo, !UsedModules),
            MaybeTypes = yes(user_type_info(Types, TVarset)),
            UserSharing = user_sharing(Sharing, MaybeTypes),
            set_user_annotated_sharing(UserSharing, Attrs0, Attrs)
        ;
            Attrs = Attrs0
        ),
        ItemId = item_id(foreign_proc_item, item_name(PName, 
            list.length(ProcVars))),
        finish_recording_expanded_items(ItemId, !.EquivTypeInfo, !Info)
    ). 

replace_in_item(ModuleName, Location,
        item_mutable(MutName, Type0, InitValue, Inst0, Attrs, Varset),
        _Context, EqvMap, EqvInstMap,
        item_mutable(MutName, Type, InitValue, Inst, Attrs, Varset),
        [], !Info, !UsedModules) :-
    QualName = qualified(ModuleName, MutName),
    maybe_record_expanded_items(ModuleName, QualName, !.Info, ExpandedItems0),
    TVarSet0 = varset.init,
    replace_in_type_location(Location, EqvMap, Type0, Type, _TypeChanged,
        TVarSet0, _TVarSet, ExpandedItems0, ExpandedItems1, !UsedModules),
    replace_in_inst(Location, Inst0, EqvInstMap, Inst,
        ExpandedItems1, ExpandedItems, !UsedModules),
    ItemId = item_id(mutable_item, item_name(QualName, 0)),
    finish_recording_expanded_items(ItemId, ExpandedItems, !Info).

:- pred replace_in_type_defn(eqv_type_location::in, eqv_map::in, type_ctor::in,
    type_defn::in, type_defn::out, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is semidet.

replace_in_type_defn(Location, EqvMap, TypeCtor, parse_tree_eqv_type(TBody0),
        parse_tree_eqv_type(TBody), ContainsCirc,
        !VarSet, !Info, !UsedModules) :-
    replace_in_type_location_2(Location, EqvMap, [TypeCtor], TBody0, TBody,
        _, ContainsCirc, !VarSet, !Info, !UsedModules).

replace_in_type_defn(Location, EqvMap, _,
        parse_tree_du_type(TBody0, EqPred),
        parse_tree_du_type(TBody, EqPred), no, !VarSet, !Info, !UsedModules) :-
    replace_in_ctors_location(Location, EqvMap, TBody0, TBody, !VarSet, !Info,
        !UsedModules).

replace_in_type_defn(Location, EqvMap, TypeCtor,
        parse_tree_solver_type(SolverTypeDetails0, MaybeUserEqComp),
        parse_tree_solver_type(SolverTypeDetails,  MaybeUserEqComp),
        ContainsCirc, !VarSet, !Info, !UsedModules) :-
    SolverTypeDetails0 = solver_type_details(RepresentationType0, InitPred,
        GroundInst, AnyInst, MutableItems),
    replace_in_type_location_2(Location, EqvMap, [TypeCtor], 
        RepresentationType0, RepresentationType,
        _, ContainsCirc, !VarSet, !Info, !UsedModules),
    SolverTypeDetails = solver_type_details(RepresentationType, InitPred,
        GroundInst, AnyInst, MutableItems).

%-----------------------------------------------------------------------------%

replace_in_prog_constraints(EqvMap, Cs0, Cs, !VarSet, !Info) :-
    replace_in_prog_constraints_location(eqv_type_out_of_module, EqvMap,
        Cs0, Cs, !VarSet, !Info, used_modules_init, _).

:- pred replace_in_prog_constraints_location(eqv_type_location::in,
    eqv_map::in, prog_constraints::in, prog_constraints::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraints_location(Location, EqvMap, Cs0, Cs, !VarSet, !Info,
        !UsedModules) :-
    Cs0 = constraints(UnivCs0, ExistCs0),
    Cs = constraints(UnivCs, ExistCs),
    replace_in_prog_constraint_list(Location, EqvMap, UnivCs0, UnivCs,
        !VarSet, !Info, !UsedModules),
    replace_in_prog_constraint_list(Location, EqvMap, ExistCs0, ExistCs,
        !VarSet, !Info, !UsedModules).

:- pred replace_in_prog_constraint_list(eqv_type_location::in, eqv_map::in,
    list(prog_constraint)::in, list(prog_constraint)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_list(Location, EqvMap, !Cs, !VarSet, !Info,
        !UsedModules) :-
    list.map_foldl3(replace_in_prog_constraint_location(Location, EqvMap),
        !Cs, !VarSet, !Info, !UsedModules).

replace_in_prog_constraint(EqvMap, Constraint0, Constraint, !VarSet, !Info) :-
    replace_in_prog_constraint_location(eqv_type_out_of_module, EqvMap,
        Constraint0, Constraint, !VarSet, !Info, used_modules_init, _).

:- pred replace_in_prog_constraint_location(eqv_type_location::in, eqv_map::in,
    prog_constraint::in, prog_constraint::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_prog_constraint_location(Location, EqvMap, Constraint0, Constraint,
        !VarSet, !Info, !UsedModules) :-
    Constraint0 = constraint(ClassName, Ts0),
    replace_in_type_list_location_circ(Location, EqvMap, Ts0, Ts, _, _,
        !VarSet, !Info, !UsedModules),
    Constraint = constraint(ClassName, Ts).

%-----------------------------------------------------------------------------%

:- pred replace_in_class_interface(eqv_type_location::in, class_methods::in,
    eqv_map::in, eqv_inst_map::in, class_methods::out,
    list(eqv_error)::in, list(eqv_error)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_class_interface(Location, ClassInterface0, EqvMap, EqvInstMap,
        ClassInterface, !Errors, !Info, !UsedModules) :-
    list.map_foldl3(replace_in_class_method(Location, EqvMap, EqvInstMap),
        ClassInterface0, ClassInterface, !Errors, !Info, !UsedModules).

:- pred replace_in_class_method(eqv_type_location::in,
    eqv_map::in, eqv_inst_map::in, class_method::in, class_method::out,
    list(eqv_error)::in, list(eqv_error)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_class_method(Location, EqvMap, EqvInstMap,
        method_pred_or_func(TypeVarSet0, InstVarSet, ExistQVars, PredOrFunc,
            PredName, TypesAndModes0, WithType0, WithInst0,
            Det0, Cond, Purity, ClassContext0, Context),
        method_pred_or_func(TypeVarSet, InstVarSet, ExistQVars, PredOrFunc,
            PredName, TypesAndModes, WithType, WithInst,
            Det, Cond, Purity, ClassContext, Context),
        !Errors, !Info, !UsedModules) :-
    replace_in_pred_type(Location, PredName, PredOrFunc, Context, EqvMap,
        EqvInstMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, TypeVarSet0, TypeVarSet,
        WithType0, WithType, WithInst0, WithInst, Det0, Det,
        !Info, !UsedModules, NewErrors),
    !:Errors = NewErrors ++ !.Errors.

replace_in_class_method(Location, _, EqvInstMap,
        method_pred_or_func_mode(InstVarSet, MaybePredOrFunc0, PredName,
            Modes0, WithInst0, Det0, Cond, Context),
        method_pred_or_func_mode(InstVarSet, MaybePredOrFunc, PredName,
            Modes, WithInst, Det, Cond, Context),
        !Errors, !Info, !UsedModules) :-
    replace_in_pred_mode(Location, PredName, length(Modes0), Context,
        mode_decl, EqvInstMap, MaybePredOrFunc0, MaybePredOrFunc,
        ExtraModes, WithInst0, WithInst, Det0, Det, !Info, !UsedModules,
        NewErrors),
    (
        ExtraModes = [],
        Modes = Modes0
    ;
        ExtraModes = [_ | _],
        Modes = Modes0 ++ ExtraModes
    ),
    !:Errors = NewErrors ++ !.Errors.

%-----------------------------------------------------------------------------%

:- pred replace_in_subst(eqv_type_location::in, eqv_map::in,
    assoc_list(tvar, mer_type)::in, assoc_list(tvar, mer_type)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_subst(_Location, _EqvMap, [], [], !VarSet, !Info, !UsedModules).
replace_in_subst(Location, EqvMap, [Var - Type0 | Subst0],
        [Var - Type | Subst], !VarSet, !Info, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, !VarSet, !Info,
        !UsedModules),
    replace_in_subst(Location, EqvMap, Subst0, Subst, !VarSet, !Info,
        !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_ctors(EqvMap, !Ctors, !VarSet, !Info) :-
    replace_in_ctors_location(eqv_type_out_of_module, EqvMap, !Ctors, !VarSet,
        !Info, used_modules_init, _).

:- pred replace_in_ctors_location(eqv_type_location::in, eqv_map::in,
    list(constructor)::in, list(constructor)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctors_location(Location, EqvMap, !Ctors, !VarSet, !Info,
        !UsedModules) :-
    list.map_foldl3(replace_in_ctor(Location, EqvMap), !Ctors, !VarSet, !Info,
        !UsedModules).

:- pred replace_in_ctor(eqv_type_location::in, eqv_map::in,
    constructor::in, constructor::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor(Location, EqvMap,
        ctor(ExistQVars, Constraints0, TName, Targs0),
        ctor(ExistQVars, Constraints, TName, Targs),
        !VarSet, !Info, !UsedModules) :-
    replace_in_ctor_arg_list(Location,
        EqvMap, Targs0, Targs, _, !VarSet, !Info, !UsedModules),
    replace_in_prog_constraint_list(Location, EqvMap,
        Constraints0, Constraints, !VarSet, !Info, !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_type_list(EqvMap, !Ts, Changed, !VarSet, !Info) :-
    replace_in_type_list_location(eqv_type_out_of_module, EqvMap, !Ts, Changed,
        !VarSet, !Info, used_modules_init, _).

:- pred replace_in_type_list_location(eqv_type_location::in, eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out,
    tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location(Location, EqvMap, !Ts, Changed, !VarSet, !Info,
        !UsedModules) :-
    replace_in_type_list_location_circ_2(Location, EqvMap, [], !Ts, Changed,
        no, _, !VarSet, !Info, !UsedModules).

:- pred replace_in_type_list_location_circ(eqv_type_location::in, eqv_map::in,
    list(mer_type)::in, list(mer_type)::out, bool::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ(Location, EqvMap, !Ts,
        Changed, ContainsCirc, !VarSet, !Info, !UsedModules) :-
    replace_in_type_list_location_circ_2(Location, EqvMap, [], !Ts,
        Changed, no, ContainsCirc, !VarSet, !Info, !UsedModules).

:- pred replace_in_type_list_location_circ_2(eqv_type_location::in,
    eqv_map::in, list(type_ctor)::in, list(mer_type)::in, list(mer_type)::out,
    bool::out, bool::in, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_list_location_circ_2(_Location, _EqvMap, _Seen, [], [], no,
        !ContainsCirc, !VarSet, !Info, !UsedModules).
replace_in_type_list_location_circ_2(Location, EqvMap, Seen,
        List0 @ [T0 | Ts0], List, Changed, !Circ, !VarSet, !Info,
        !UsedModules) :-
    replace_in_type_location_2(Location, EqvMap, Seen, T0, T, Changed0,
        ContainsCirc, !VarSet, !Info, !UsedModules),
    !:Circ = ContainsCirc `or` !.Circ,
    replace_in_type_list_location_circ_2(Location, EqvMap, Seen, Ts0, Ts,
        Changed1, !Circ, !VarSet, !Info, !UsedModules),
    (
        ( Changed0 = yes
        ; Changed1 = yes
        )
    ->
        Changed = yes,
        List = [T | Ts]
    ;
        Changed = no,
        List = List0
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_ctor_arg_list(eqv_type_location::in, eqv_map::in,
    list(constructor_arg)::in, list(constructor_arg)::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list(Location,
        EqvMap, !As, ContainsCirc, !VarSet, !Info, !UsedModules) :-
    replace_in_ctor_arg_list_2(Location, EqvMap, [], !As, no, ContainsCirc,
        !VarSet, !Info, !UsedModules).

:- pred replace_in_ctor_arg_list_2(eqv_type_location::in,
    eqv_map::in, list(type_ctor)::in,
    list(constructor_arg)::in, list(constructor_arg)::out,
    bool::in, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_ctor_arg_list_2(_Location, _EqvMap, _Seen, [], [],
        !Circ, !VarSet, !Info, !UsedModules).
replace_in_ctor_arg_list_2(Location,
        EqvMap, Seen, [N - T0 | As0], [N - T | As],
        !Circ, !VarSet, !Info, !UsedModules) :-
    replace_in_type_location_2(Location, EqvMap, Seen, T0, T, _, ContainsCirc,
        !VarSet, !Info, !UsedModules),
    !:Circ = !.Circ `or` ContainsCirc,
    replace_in_ctor_arg_list_2(Location, EqvMap,
        Seen, As0, As, !Circ, !VarSet, !Info, !UsedModules).

%-----------------------------------------------------------------------------%

replace_in_type(EqvMap, Type0, Type, Changed, !VarSet, !Info) :-
    replace_in_type_location(eqv_type_out_of_module, EqvMap, Type0, Type,
        Changed, !VarSet, !Info, used_modules_init, _).

:- pred replace_in_type_location(eqv_type_location::in, eqv_map::in,
    mer_type::in, mer_type::out, bool::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_location(Location, EqvMap, Type0, Type, Changed, !VarSet,
        !Info, !UsedModules) :-
    replace_in_type_location_2(Location, EqvMap, [], Type0, Type, Changed, _,
        !VarSet, !Info, !UsedModules).

    % Replace all equivalence types in a given type, detecting
    % any circularities.
    %
:- pred replace_in_type_location_2(eqv_type_location::in, eqv_map::in,
    list(type_ctor)::in, mer_type::in, mer_type::out, bool::out, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_type_location_2(Location, EqvMap, TypeCtorsAlreadyExpanded,
        Type0, Type, Changed, Circ, !VarSet, !Info, !UsedModules) :-
    (
        Type0 = type_variable(Var, Kind),
        Type = type_variable(Var, Kind),
        Changed = no,
        Circ = no
    ;
        Type0 = defined_type(SymName, TArgs0, Kind),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, TArgs0, TArgs, ArgsChanged, no, Circ0,
            !VarSet, !Info, !UsedModules),
        Arity = list.length(TArgs),
        TypeCtor = type_ctor(SymName, Arity),
        replace_type_ctor(Location, EqvMap, TypeCtorsAlreadyExpanded,
            Type0, TypeCtor, TArgs, Kind, Type, ArgsChanged, Changed,
            Circ0, Circ, !VarSet, !Info, !UsedModules)
    ;
        Type0 = builtin_type(_),
        Type = Type0,
        Changed = no,
        Circ = no
    ;
        Type0 = higher_order_type(Args0, MaybeRet0, Purity, EvalMethod),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, ArgsChanged, no, ArgsCirc,
            !VarSet, !Info, !UsedModules),
        (
            MaybeRet0 = yes(Ret0),
            replace_in_type_location_2(Location, EqvMap,
                TypeCtorsAlreadyExpanded, Ret0, Ret, RetChanged, RetCirc,
                !VarSet, !Info, !UsedModules),
            MaybeRet = yes(Ret),
            Changed = bool.or(ArgsChanged, RetChanged),
            Circ = bool.or(ArgsCirc, RetCirc)
        ;
            MaybeRet0 = no,
            MaybeRet = no,
            Changed = ArgsChanged,
            Circ = ArgsCirc
        ),
        (
            Changed = yes,
            Type = higher_order_type(Args, MaybeRet, Purity, EvalMethod)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = tuple_type(Args0, Kind),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, no, Circ,
            !VarSet, !Info, !UsedModules),
        (
            Changed = yes,
            Type = tuple_type(Args, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = apply_n_type(Var, Args0, Kind),
        replace_in_type_list_location_circ_2(Location, EqvMap,
            TypeCtorsAlreadyExpanded, Args0, Args, Changed, no, Circ,
            !VarSet, !Info, !UsedModules),
        (
            Changed = yes,
            Type = apply_n_type(Var, Args, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ;
        Type0 = kinded_type(RawType0, Kind),
        replace_in_type_location_2(Location, EqvMap, TypeCtorsAlreadyExpanded,
            RawType0, RawType, Changed, Circ, !VarSet, !Info, !UsedModules),
        (
            Changed = yes,
            Type = kinded_type(RawType, Kind)
        ;
            Changed = no,
            Type = Type0
        )
    ).

:- pred replace_type_ctor(eqv_type_location::in, eqv_map::in,
    list(type_ctor)::in, mer_type::in, type_ctor::in, list(mer_type)::in,
    kind::in, mer_type::out, bool::in, bool::out, bool::in, bool::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_type_ctor(Location, EqvMap, TypeCtorsAlreadyExpanded, Type0,
        TypeCtor, TArgs, Kind, Type, !Changed, !Circ, !VarSet, !Info,
        !UsedModules) :-
    ( list.member(TypeCtor, TypeCtorsAlreadyExpanded) ->
        AlreadyExpanded = yes
    ;
        AlreadyExpanded = no
    ),
    (
        map.search(EqvMap, TypeCtor, eqv_type_body(EqvVarSet, Args0, Body0)),

        % Don't merge in the variable names from the type declaration to avoid
        % creating multiple variables with the same name so that
        % `varset.create_name_var_map' can be used on the resulting tvarset.
        % make_hlds uses `varset.create_name_var_map' to match up type
        % variables in `:- pragma type_spec' declarations and explicit type
        % qualifications with the type variables in the predicate's
        % declaration.

        tvarset_merge_renaming_without_names(!.VarSet, EqvVarSet, !:VarSet,
            Renaming),
        !.Circ = no,
        AlreadyExpanded = no
    ->
        type_ctor_used_modules(Location, TypeCtor, !UsedModules),

        !:Changed = yes,
        map.apply_to_list(Args0, Renaming, Args),
        apply_variable_renaming_to_type(Renaming, Body0, Body1),
        TypeCtorItem = type_ctor_to_item_name(TypeCtor),
        record_expanded_item(item_id(type_item, TypeCtorItem), !Info),
        map.from_corresponding_lists(Args, TArgs, Subst),
        apply_subst_to_type(Subst, Body1, Body),
        replace_in_type_location_2(Location, EqvMap,
            [TypeCtor | TypeCtorsAlreadyExpanded], Body,
            Type, _, !:Circ, !VarSet, !Info, !UsedModules)
    ;
        (
            !.Changed = yes,
            TypeCtor = type_ctor(SymName, _Arity),
            Type = defined_type(SymName, TArgs, Kind)
        ;
            !.Changed = no,
            Type = Type0
        ),
        bool.or(AlreadyExpanded, !Circ)
    ).

:- pred type_ctor_used_modules(eqv_type_location::in, type_ctor::in,
    used_modules::in, used_modules::out) is det.

type_ctor_used_modules(eqv_type_out_of_module, _, !UsedModules).
type_ctor_used_modules(eqv_type_in_interface, type_ctor(Name, _),
        !UsedModules) :-
    add_sym_name_module(visibility_public, Name, !UsedModules).
type_ctor_used_modules(eqv_type_in_implementation, type_ctor(Name, _),
        !UsedModules) :-
    add_sym_name_module(visibility_private, Name, !UsedModules).

:- pred replace_in_inst(eqv_type_location::in,
    mer_inst::in, eqv_inst_map::in, mer_inst::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst(Location, Inst0, EqvInstMap, Inst, !Info, !UsedModules) :-
    replace_in_inst_location(Location, Inst0, EqvInstMap, set.init, Inst,
        !Info, !UsedModules).

:- pred replace_in_inst_location(eqv_type_location::in, mer_inst::in,
    eqv_inst_map::in, set(inst_id)::in, mer_inst::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_inst_location(Location, Inst0, EqvInstMap, ExpandedInstIds, Inst,
        !Info, !UsedModules) :-
    % XXX Need to record the used modules
    ( Inst0 = defined_inst(user_inst(SymName, ArgInsts)) ->
        InstId = inst_id(SymName, length(ArgInsts)),
        (
            set.member(InstId, ExpandedInstIds)
        ->
            Inst = Inst0
        ;
            map.search(EqvInstMap, InstId, EqvInstBody),
            EqvInstBody = eqv_inst_body(_, EqvInstParams, EqvInst)
        ->
            inst_substitute_arg_list(EqvInstParams, ArgInsts, EqvInst, Inst1),
            InstIdItem = inst_id_to_item_name(InstId),
            record_expanded_item(item_id(inst_item, InstIdItem), !Info),
            replace_in_inst_location(Location, Inst1, EqvInstMap,
                set.insert(ExpandedInstIds, InstId), Inst, !Info, !UsedModules)
        ;
            Inst = Inst0
        )
    ;
        Inst = Inst0
    ).

%-----------------------------------------------------------------------------%

:- pred replace_in_pred_type(eqv_type_location::in, sym_name::in,
    pred_or_func::in, prog_context::in, eqv_map::in, eqv_inst_map::in,
    prog_constraints::in, prog_constraints::out,
    list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out,
    maybe(mer_type)::in, maybe(mer_type)::out,
    maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out,
    list(eqv_error)::out) is det.

replace_in_pred_type(Location, PredName, PredOrFunc, Context,
        EqvMap, EqvInstMap, ClassContext0, ClassContext,
        TypesAndModes0, TypesAndModes, !TypeVarSet,
        MaybeWithType0, MaybeWithType, MaybeWithInst0, MaybeWithInst,
        Det0, Det, !Info, !UsedModules, Errors) :-
    replace_in_prog_constraints_location(Location, EqvMap,
        ClassContext0, ClassContext, !TypeVarSet, !Info, !UsedModules),
    replace_in_tms(Location, EqvMap,
        TypesAndModes0, TypesAndModes1, !TypeVarSet, !Info, !UsedModules),
    (
        MaybeWithType0 = yes(WithType0),
        replace_in_type_location(Location, EqvMap, WithType0, WithType, _,
            !TypeVarSet, !Info, !UsedModules),
        (
            type_is_higher_order_details(WithType, _Purity, PredOrFunc,
                _EvalMethod, ExtraTypes0)
        ->
            ExtraTypes = ExtraTypes0,
            Errors0 = []
        ;
            ExtraTypes = [],
            Errors0 = [invalid_with_type(PredName, PredOrFunc) - Context]
        )
    ;
        MaybeWithType0 = no,
        ExtraTypes = [],
        Errors0 = []
    ),

    replace_in_pred_mode(Location, PredName, length(TypesAndModes0),
        Context, type_decl, EqvInstMap, yes(PredOrFunc), _, ExtraModes,
        MaybeWithInst0, _, Det0, Det, !Info, !UsedModules, ModeErrors),
    Errors1 = Errors0 ++ ModeErrors,

    ( Errors1 = [_ | _] ->
        Errors = Errors1,
        ExtraTypesAndModes = []
    ; ExtraModes = [] ->
        Errors = Errors1,
        ExtraTypesAndModes = list.map((func(Type) = type_only(Type)),
            ExtraTypes)
    ; length(ExtraTypes) `with_type` int = length(ExtraModes) ->
        Errors = Errors1,
        assoc_list.from_corresponding_lists(ExtraTypes, ExtraModes,
            ExtraTypesModes),
        ExtraTypesAndModes = list.map(
            (func(Type - Mode) = type_and_mode(Type, Mode)),
            ExtraTypesModes)
    ;
        Errors = [non_matching_with_type_with_inst(PredName, PredOrFunc)
            - Context | Errors1],
        ExtraTypesAndModes = []
    ),

    (
        Errors = [],
        MaybeWithType = no,
        MaybeWithInst = no
    ;
        Errors = [_ | _],
        % Leave the `with_type` and `with_inst` fields so that make_hlds knows
        % to discard this declaration.
        MaybeWithType = MaybeWithType0,
        MaybeWithInst = MaybeWithInst0
    ),

    (
        ExtraTypesAndModes = [],
        TypesAndModes = TypesAndModes1
    ;
        ExtraTypesAndModes = [_ | _],
        OrigItemId = item_id(pred_or_func_to_item_type(PredOrFunc),
            item_name(PredName, list.length(TypesAndModes0))),
        record_expanded_item(OrigItemId, !Info),
        TypesAndModes = TypesAndModes1 ++ ExtraTypesAndModes
    ).

:- pred replace_in_pred_mode(eqv_type_location::in, sym_name::in, arity::in,
    prog_context::in, pred_or_func_decl_type::in, eqv_inst_map::in,
    maybe(pred_or_func)::in, maybe(pred_or_func)::out,
    list(mer_mode)::out, maybe(mer_inst)::in, maybe(mer_inst)::out,
    maybe(determinism)::in, maybe(determinism)::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out,
    list(eqv_error)::out) is det.

replace_in_pred_mode(Location, PredName, OrigArity, Context, DeclType,
        EqvInstMap, MaybePredOrFunc0, MaybePredOrFunc, ExtraModes,
        MaybeWithInst0, MaybeWithInst, Det0, Det,
        !Info, !UsedModules, Errors) :-
    (
        MaybeWithInst0 = yes(WithInst0),
        replace_in_inst(Location, WithInst0, EqvInstMap, WithInst, !Info,
            !UsedModules),
        (
            WithInst = ground(_, higher_order(pred_inst_info(
                PredOrFunc, ExtraModes0, DetPrime))),
            ( MaybePredOrFunc0 = no
            ; MaybePredOrFunc0 = yes(PredOrFunc)
            )
        ->
            Det = yes(DetPrime),
            MaybeWithInst = no,
            MaybePredOrFunc = yes(PredOrFunc),
            Errors = [],
            ExtraModes = ExtraModes0,
            (
                MaybePredOrFunc0 = no,
                RecordedPredOrFunc = predicate
            ;
                MaybePredOrFunc0 = yes(RecordedPredOrFunc)
            ),
            OrigItemId = item_id(pred_or_func_to_item_type(RecordedPredOrFunc),
                item_name(PredName, OrigArity)),
            record_expanded_item(OrigItemId, !Info)
        ;
            ExtraModes = [],
            MaybePredOrFunc = MaybePredOrFunc0,
            % Leave the `with_inst` fields so that make_hlds
            % knows to discard this declaration.
            MaybeWithInst = MaybeWithInst0,
            Det = Det0,
            Errors = [invalid_with_inst(DeclType, PredName, MaybePredOrFunc0)
                - Context]
        )
    ;
        MaybeWithInst0 = no,
        MaybeWithInst = MaybeWithInst0,
        MaybePredOrFunc = MaybePredOrFunc0,
        Errors = [],
        Det = Det0,
        ExtraModes = []
    ).

:- pred replace_in_tms(eqv_type_location::in, eqv_map::in,
    list(type_and_mode)::in, list(type_and_mode)::out,
    tvarset::in, tvarset::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_tms(Location, EqvMap, !TMs, !VarSet, !Info, !UsedModules) :-
    list.map_foldl3(replace_in_tm(Location, EqvMap), !TMs, !VarSet, !Info,
        !UsedModules).

:- pred replace_in_tm(eqv_type_location::in, eqv_map::in,
    type_and_mode::in, type_and_mode::out, tvarset::in, tvarset::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_tm(Location, EqvMap, type_only(Type0),
        type_only(Type), !VarSet, !Info, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, !VarSet, !Info,
        !UsedModules).

replace_in_tm(Location, EqvMap, type_and_mode(Type0, Mode),
        type_and_mode(Type, Mode), !VarSet, !Info, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, !VarSet, !Info,
        !UsedModules).

%-----------------------------------------------------------------------------%
%
:- pred replace_in_structure_sharing_domain(eqv_type_location::in, eqv_map::in, 
    structure_sharing_domain::in, structure_sharing_domain::out, 
    tvarset::in, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_domain(_, _, X @ structure_sharing_bottom,
    X, _TVarset, !EquivTypeInfo, !UsedModules).
replace_in_structure_sharing_domain(_, _, X @ structure_sharing_top(_), 
    X, _TVarset, !EquivTypeInfo, !UsedModules).
replace_in_structure_sharing_domain(Location, EqvMap, 
        structure_sharing_real(SharingPairs0),
        structure_sharing_real(SharingPairs),
        TVarset, !EquivTypeInfo, !UsedModules) :- 
    list.map_foldl2(
        replace_in_structure_sharing_pair(Location, EqvMap, TVarset), 
        SharingPairs0, SharingPairs, !EquivTypeInfo, !UsedModules).

:- pred replace_in_structure_sharing_pair(eqv_type_location::in,
    eqv_map::in, tvarset::in, 
    structure_sharing_pair::in, structure_sharing_pair::out,
    equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_structure_sharing_pair(Location, EqvMap, TVarset, Data10 - Data20, 
        Data1 - Data2, !EquivTypeInfo, !UsedModules) :- 
    replace_in_datastruct(Location, EqvMap, TVarset, Data10, Data1,
        !EquivTypeInfo, !UsedModules),
    replace_in_datastruct(Location, EqvMap, TVarset, Data20, Data2,
        !EquivTypeInfo, !UsedModules).

:- pred replace_in_datastruct(eqv_type_location::in,
    eqv_map::in, tvarset::in, datastruct::in,
    datastruct::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_datastruct(Location, EqvMap, TVarset, Data0, Data, !EquivTypeInfo,
        !UsedModules) :- 
    Sel0 = Data0 ^ sc_selector,
    list.map_foldl2(replace_in_unit_selector(Location, EqvMap, TVarset),
        Sel0, Sel, !EquivTypeInfo, !UsedModules), 
    Data = Data0 ^ sc_selector := Sel.

:- pred replace_in_unit_selector(eqv_type_location::in,
    eqv_map::in, tvarset::in, unit_selector::in,
    unit_selector::out, equiv_type_info::in, equiv_type_info::out,
    used_modules::in, used_modules::out) is det.

replace_in_unit_selector(_, _, _, X @ termsel(_, _), X, !EquivTypeInfo, !UMs).
replace_in_unit_selector(Location, EqvMap, TVarset,
        typesel(Type0), typesel(Type), !EquivTypeInfo, !UsedModules) :-
    replace_in_type_location(Location, EqvMap, Type0, Type, _, TVarset, _,
        !EquivTypeInfo, !UsedModules).

%-----------------------------------------------------------------------------%

:- type expanded_item_set == pair(module_name, set(item_id)).

maybe_record_expanded_items(_, _, no, no).
maybe_record_expanded_items(ModuleName, SymName, yes(_), MaybeInfo) :-
    ( SymName = qualified(ModuleName, _) ->
        MaybeInfo = no
    ;
        MaybeInfo = yes(ModuleName - set.init)
    ).

:- pred record_expanded_item(item_id::in,
    equiv_type_info::in, equiv_type_info::out) is det.

record_expanded_item(Item, !Info) :-
    map_maybe(record_expanded_item_2(Item), !Info).

:- pred record_expanded_item_2(item_id::in,
    pair(module_name, set(item_id))::in,
    pair(module_name, set(item_id))::out) is det.

record_expanded_item_2(ItemId, ModuleName - Items0, ModuleName - Items) :-
    ItemId = item_id(_, ItemName),
    ( ItemName = item_name(qualified(ModuleName, _), _) ->
        % We don't need to record local types.
        Items = Items0
    ;
        Items = set.insert(Items0, ItemId)
    ).

finish_recording_expanded_items(_, no, no, no).
finish_recording_expanded_items(_, no, yes(Info), yes(Info)).
finish_recording_expanded_items(_, yes(_), no, _) :-
    unexpected(this_file, "finish_recording_expanded_items").
finish_recording_expanded_items(Item, yes(_ - ExpandedItems),
        yes(Info0), yes(Info)) :-
    recompilation.record_expanded_items(Item, ExpandedItems, Info0, Info).

%-----------------------------------------------------------------------------%

:- pred report_error(eqv_error::in, io::di, io::uo) is det.

report_error(circular_equivalence(Item) - Context, !IO) :-
    (
        Item = item_type_defn(_, SymName, Params, TypeDefn, _),
        TypeDefn = parse_tree_eqv_type(_)
    ->
        Pieces = [words("Error: circular equivalence type"),
            sym_name_and_arity(SymName / length(Params)), suffix(".")],
        write_error_pieces(Context, 0, Pieces, !IO)
    ;
        unexpected(this_file, "report_error: invalid item")
    ).
report_error(invalid_with_type(SymName, PredOrFunc) - Context, !IO) :-
    Pieces = [words("In type declaration for"),
        p_or_f(PredOrFunc), sym_name(SymName), suffix(":"), nl,
        words("error: expected higher order"), p_or_f(PredOrFunc),
        words("type after `with_type`.")],
    write_error_pieces(Context, 0, Pieces, !IO).
report_error(invalid_with_inst(DeclType, SymName, MaybePredOrFunc) - Context,
        !IO) :-
    ( DeclType = type_decl, DeclStr = "declaration"
    ; DeclType = mode_decl, DeclStr = "mode declaration"
    ),
    (
        MaybePredOrFunc = no,
        PredOrFuncPieces = []
    ;
        MaybePredOrFunc = yes(PredOrFunc),
        PredOrFuncPieces = [p_or_f(PredOrFunc)]
    ),
    Pieces = [words("In"), words(DeclStr), words("for")] ++
        PredOrFuncPieces ++ [sym_name(SymName), suffix(":"), nl,
        words("error: expected higher order ")] ++ PredOrFuncPieces ++
        [words("inst after `with_inst`.")],
    write_error_pieces(Context, 0, Pieces, !IO).
report_error(non_matching_with_type_with_inst(SymName, PredOrFunc) - Context,
        !IO) :-
    Pieces = [words("In type declaration for"),
        p_or_f(PredOrFunc), sym_name(SymName), suffix(":"), nl,
        words("error: the `with_type` and `with_inst`"),
        words("annotations are incompatible.")],
    write_error_pieces(Context, 0, Pieces, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "equiv_type.m".

%-----------------------------------------------------------------------------%
:- end_module equiv_type.
%-----------------------------------------------------------------------------%
