%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module creates the initial HLDS from the augmented parse tree
% of a module.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % do_parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo,
    %   TypeEqvMapMap, UsedModules, QualInfo, InvalidTypes, InvalidModes, HLDS,
    %   Specs):
    %
    % Given MQInfo (returned by module_qual.m) and TypeEqvMapMap and
    % UsedModules (both returned by equiv_type.m), convert AugCompUnit
    % to HLDS. Return any errors found in Specs.
    % Return InvalidTypes = yes if we found undefined types.
    % Return InvalidModes = yes if we found undefined or cyclic insts or modes.
    % QualInfo is an abstract type that is then passed back to
    % produce_instance_method_clauses (see below).
    %
:- pred do_parse_tree_to_hlds(aug_compilation_unit::in, globals::in,
    string::in, mq_info::in, type_eqv_map::in, used_modules::in,
    qual_info::out, found_invalid_type::out, found_invalid_inst_or_mode::out,
    module_info::out, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- include_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.

:- import_module hlds.add_pred.
:- import_module hlds.add_special_pred.
:- import_module hlds.default_func_mode.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_mode.
:- import_module hlds.make_hlds.add_mutable_aux_preds.
:- import_module hlds.make_hlds.add_pragma.
:- import_module hlds.make_hlds.add_solver.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_passes.make_hlds_separate_items.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.make_hlds_error.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_item_stats.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.

:- import_module bool.
:- import_module io.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

do_parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo0,
        TypeEqvMapMap, UsedModules, !:QualInfo,
        !:FoundInvalidType, !:FoundInvalidInstOrMode, !:ModuleInfo, !:Specs) :-
    mq_info_get_partial_qualifier_info(MQInfo0, PQInfo),
    module_info_init(AugCompUnit, DumpBaseFileName, Globals, PQInfo, no,
        !:ModuleInfo),
    module_info_set_used_modules(UsedModules, !ModuleInfo),

    % Optionally gather statistics about the items in the compilation unit.
    trace [compile_time(flag("item_stats")), io(!IO)] (
        % We *append* statistics to a file, rather than simply *writing*
        % them to a file, so that we can gather statistics from a sequence of
        % Mercury compiler invocations, such as those in a bootcheck.
        % The file should be created empty before the start of the sequence,
        % and should be appended to by one Mercury compiler invocation
        % at a time. (We don't do any locking, so if more than one invocation
        % tries to append to the file at the same time, the resulting output
        % will be malformed.)
        %
        % You may of course change the name of the file, if you wish.
        %
        % A statistics file resulting from one or more compiler invocations
        % may be analyzed using tools/item_stats.

        io.open_append("/tmp/ITEM_STATS", Res, !IO),
        (
            Res = error(_)
        ;
            Res = ok(Stream),
            gather_and_write_item_stats(Stream, AugCompUnit, !IO),
            io.close_output(Stream, !IO)
        )
    ),

    AugCompUnit = aug_compilation_unit(ModuleName, _ModuleNameContext,
        ModuleVersionNumbers, _SrcItemBlocks,
        _DirectIntItemBlocks, _IndirectIntItemBlocks,
        _OptItemBlocks, _IntForOptItemBlocks),

    % We used to add items to the HLDS in three passes.
    % Roughly,
    %
    % - pass 1 added type, inst and mode definitions and predicate
    %   declarations to the module,
    % - pass 3 added the definitions of predicates, including not just
    %   clauses but also clause-like pragmas such as foreign_procs
    %   to the module, while
    % - pass 2 did the tasks that had to be done before pass 3 started
    %   but which needed access to all the declarations added by pass 1,
    %   not just the ones processed so far.
    %
    % We now add items to the HLDS by item kind: first one kind of item,
    % then another, then another. The general order remains roughly the same
    % as before, but we now also impose an order *within* what used to be
    % a pass. For example, we add all type definitions before we add
    % any inst definitions, since inst definitions may now refer to type
    % constructors.
    %
    % The constraints of what we have to add before what are documented below.

    separate_items_in_aug_comp_unit(AugCompUnit, ItemAvailLists,
        ItemTypeDefnsAbstract, ItemTypeDefnsMercury, ItemTypeDefnsForeign,
        ItemInstDefns, ItemModeDefns, ItemPredDecls, ItemModeDecls,
        ItemPromises, ItemTypeclasses, ItemInstances,
        ItemInitialises, ItemFinalises, ItemMutables, ItemTypeRepns,
        ItemForeignEnums, ItemForeignExportEnums,
        ItemPragmas2, ItemPragmas3, ItemClauses),

    map.init(DirectArgMap),
    TypeRepnDec = type_repn_decision_data(ItemTypeRepns, DirectArgMap,
        ItemForeignEnums, ItemForeignExportEnums), 
    module_info_set_type_repn_dec(TypeRepnDec, !ModuleInfo),

    % The old pass 1.

    !:FoundInvalidType = did_not_find_invalid_type,
    !:FoundInvalidInstOrMode = did_not_find_invalid_inst_or_mode,
    !:Specs = [],

    % Record the import_module and use_module declarations.
    list.foldl(add_item_avail_list, ItemAvailLists, !ModuleInfo),

    % Record type definitions.
    %
    % We first add the abstract type definitions, then the nonabstract ones,
    % since this should simplify the processing of type constructors that
    % have both abstract and nonabstract definitions. Among the nonabstract
    % type definitions, we process the ones that give Mercury definitions
    % before the ones that give foreign language definitions, since the code
    % that adds foreign language definitions can give the right error message
    % only in that case.
    %
    % The definition of a solver type requires the declaration and the
    % definition of several auxiliary predicates, for representation changes
    % and maybe for initialization. When add_type_defn defines a solver type,
    % it returns a record of what auxiliary predicates that solver type needs.
    % We gather these records in SolverAuxPredInfos, and declare and define
    % the auxiliary predicates they call for at the same time as we declare
    % and define other predicates.
    %
    % We likewise gather the mutables implicit in solver type definitions
    % (which contain the constraint store) in SolverItemMutables, and
    % process them when we process standalone mutables.
    some [!SolverAuxPredInfos, !SolverItemMutables]
    (
        !:SolverAuxPredInfos = [],
        !:SolverItemMutables = [],
        % XXX TYPE_REPN
        % - group parse_tree_{du,eqv,solver}_type together in their type;
        % - have separate_items_in_aug_comp_unit return the definitions of
        %   (a) abstract, (b) du/eqv/solver and (c) foreign types in lists
        %   whose elements are of different, specialized types; and
        % - call specialized versions of add_type_defn in each foldl5 below.
        % XXX TYPE_REPN consider treating du/eqv/solver type defns separately
        list.foldl5(add_type_defn, ItemTypeDefnsAbstract,
            !ModuleInfo, !FoundInvalidType, !Specs,
            !SolverAuxPredInfos, !SolverItemMutables),
        list.foldl5(add_type_defn, ItemTypeDefnsMercury,
            !ModuleInfo, !FoundInvalidType, !Specs,
            !SolverAuxPredInfos, !SolverItemMutables),
        list.foldl5(add_type_defn, ItemTypeDefnsForeign,
            !ModuleInfo, !FoundInvalidType, !Specs,
            !SolverAuxPredInfos, !SolverItemMutables),
        SolverAuxPredInfos = !.SolverAuxPredInfos,
        SolverItemMutables = !.SolverItemMutables
    ),

    % We process inst definitions after all type definitions because
    % the processing of type-specific insts may require access to the
    % definition of any type constructor.
    list.foldl3(add_inst_defn, ItemInstDefns,
        !ModuleInfo, !FoundInvalidInstOrMode, !Specs),

    % Mode definitions may refer to user defined insts. We don't yet
    % exploit the availability of all inst definitions when we process
    % a mode definition, but in the future, we could.
    list.foldl3(add_mode_defn, ItemModeDefns,
        !ModuleInfo, !FoundInvalidInstOrMode, !Specs),

    % A predicate declaration defines the type of the arguments of a predicate,
    % and may give the modes of those arguments. We don't yet exploit
    % the availability of all type and mode definitions when we process
    % a pred declaration (e.g. for error checking), but in the future,
    % we could.
    list.foldl2(add_pred_decl, ItemPredDecls,
        !ModuleInfo, !Specs),

    % We need to process the mode declaration of a predicate
    % after we have seen the (type) declaration of the predicate.
    % In the past, this required the predicate declaration to precede
    % the mode declaration in the source code.
    list.foldl2(add_mode_decl, ItemModeDecls,
        !ModuleInfo, !Specs),
    list.foldl2(add_solver_type_aux_pred_decls, SolverAuxPredInfos,
        !ModuleInfo, !Specs),

    % Every mutable has its own set of access predicates. Declare them,
    % whether the mutable definition was a standalone item or part of
    % the definition of a solver type. We have to do this after we add
    % types and insts to the HLDS, so we can check the types and insts
    % in the mutable for validity.
    % XXX Currently, we check only the inst for validity.
    list.foldl2(add_aux_pred_decls_for_mutable_if_local, ItemMutables,
        !ModuleInfo, !Specs),
    list.foldl2(add_aux_pred_decls_for_mutable_if_local, SolverItemMutables,
        !ModuleInfo, !Specs),

    % Record typeclass definitions.
    list.foldl2(add_typeclass_defn, ItemTypeclasses,
        !ModuleInfo, !Specs),

    % The old pass 2.

    % Now that we have processed all mode declarations, we have a reliable
    % record of which declared predicates and functions have NO mode
    % declaration. For functions, this is not an error, since Mercury
    % specifies a implicit mode declaration for them. maybe_add_default_mode
    % adds that implicit mode declaration in such circumstances.
    % XXX It should also generate error messages for PREDICATES that have
    % no mode declaration.
    list.foldl(maybe_add_default_mode, ItemPredDecls,
        !ModuleInfo),

    % Record instance definitions.
    list.foldl2(add_instance_defn, ItemInstances,
        !ModuleInfo, !Specs),

    % Implement several kinds of pragmas, the ones in the subtype
    % defined by the pragma_pass_2 inst.
    %
    % We have to implement the pragmas that affect type representations
    % BEFORE we process the type table below, because if we don't, that
    % processing may operate on incomplete data.
    %
    % We have to to implement pragmas that record some information
    % for declared predicates (e.g. assertions that they terminate, or
    % promises about their purity) after we have processed their declarations.
    % (Users may have valid stylistic reasons for putting pragmas affecting
    % a predicate before the declaration of the predicate itself.)
    % With most of them, we don't *have to* process them yet, but we can,
    % and we do. An exception is pragma_external_proc, which we *do* have
    % to process before adding clauses, so that we can guarantee that we catch
    % attempts to add clauses for external predicates, and generate an
    % error message for each such attempt.
    % XXX PASS STRUCTURE Do we actually catch such attempts?
    %
    % We also add some kinds of pragmas that don't have to be added
    % at any specific time, such as pragma_foreign_decl, pragma_foreign_code
    % and pragma_require_feature_set.
    %
    % NOTE We loop over ItemPragmas2 with a bespoke predicate, not list.foldl2,
    % because list.foldl2 doesn't (yet) know how to preserve the subtype inst
    % of ItemPragmas2.
    add_pass_2_pragmas(ItemPragmas2,
        !ModuleInfo, !Specs),

    % Since all declared predicates are in now the HLDS, we could check
    % all type definitions that define type-specific unify and/or compare
    % predicates whether the predicate names they give refer to predicates
    % that (a) exist, and (b) have the right signature. However, we currently
    % don't do that. Any such errors are discovered when we type and mode
    % check that automatically created unify and compare predicates, whose
    % bodies call the user-specified predicate names.
    % XXX PASS STRUCTURE Maybe we *should* that check here, since doing so
    % would allow us to generate better error messages.
    (
        !.FoundInvalidType = did_not_find_invalid_type,
        % Add constructors for du types to the HLDS, and check that
        % Mercury types defined solely by foreign types have a definition
        % that works for the target backend.
        %
        % This must be done after adding all type definitions and all
        % `:- pragma foreign_type' declarations. If there were errors
        % in foreign type type declarations, doing this may cause
        % a compiler abort.
        module_info_get_type_table(!.ModuleInfo, TypeTable0),
        foldl3_over_type_ctor_defns(
            add_du_ctors_check_foreign_type_for_cur_backend,
            TypeTable0, !FoundInvalidType, !ModuleInfo, !Specs)
    ;
        !.FoundInvalidType = found_invalid_type
    ),

    % Add the special preds for the builtin types which don't have a
    % type declaration, hence no hlds_type_defn is generated for them.
    ( if
        ModuleName = mercury_public_builtin_module,
        compiler_generated_rtti_for_builtins(!.ModuleInfo)
    then
        list.foldl(add_builtin_type_ctor_special_preds_in_builtin_module,
            builtin_type_ctors_with_no_hlds_type_defn, !ModuleInfo)
    else
        true
    ),

    % Balance any data structures that need it.
    module_info_optimize(!ModuleInfo),

    % The old pass 3.
    init_qual_info(MQInfo0, TypeEqvMapMap, !:QualInfo),

    % Add clauses to their predicates.
    list.foldl3(add_clause, ItemClauses,
        !ModuleInfo, !QualInfo, !Specs),

    % Add Mercury-defined clauses to the auxiliary predicates
    % that implement solver types, mutables, and promises.
    list.foldl3(add_solver_type_aux_pred_defns_if_local,
        SolverAuxPredInfos,
        !ModuleInfo, !QualInfo, !Specs),
    list.foldl3(add_aux_pred_defns_for_mutable_if_local,
        ItemMutables,
        !ModuleInfo, !QualInfo, !Specs),
    list.foldl3(add_aux_pred_defns_for_mutable_if_local,
        SolverItemMutables,
        !ModuleInfo, !QualInfo, !Specs),
    list.foldl3(add_promise, ItemPromises,
        !ModuleInfo, !QualInfo, !Specs),

    % Check that the predicates listed in `:- initialise' and `:- finalise'
    % declarations exist and have the correct signature, introduce
    % pragma foreign_export declarations for them and record their exported
    % names in the module_info, so that we can generate code to call them
    % at initialisation/finalisation time.
    list.foldl2(add_initialise, ItemInitialises,
        !ModuleInfo, !Specs),
    list.foldl2(add_finalise, ItemFinalises,
        !ModuleInfo, !Specs),

    % Implement all the pragmas we haven't processed earlier.
    % These will be the ones in the subtype defined by the pragma_pass_3 inst.
    %
    % NOTE We loop over ItemPragmas3 with a bespoke predicate, not list.foldl3,
    % because list.foldl2 doesn't (yet) know how to preserve the subtype inst
    % of ItemPragmas3.
    add_pass_3_pragmas(ItemPragmas3,
        !ModuleInfo, !QualInfo, !Specs),

    % Check that the declarations for field extraction and update functions
    % are sensible, and generate error messages for the ones that aren't.
    % We can do this only after we have processed every predicate declaration,
    % as well as everything that affects either the type table or the
    % constructor table.
    %
    list.foldl(check_pred_if_field_access_function(!.ModuleInfo),
        ItemPredDecls, !Specs),

    map.foldl(add_version_numbers, ModuleVersionNumbers, !QualInfo),

    qual_info_get_mq_info(!.QualInfo, MQInfo),
    mq_info_get_found_undef_type(MQInfo, MQUndefType),
    mq_info_get_found_undef_inst(MQInfo, MQUndefInst),
    mq_info_get_found_undef_mode(MQInfo, MQUndefMode),
    mq_info_get_found_undef_typeclass(MQInfo, MQUndefTypeClass),
    ( if
        ( MQUndefType = found_undef_type
        ; MQUndefTypeClass = found_undef_typeclass
        )
    then
        !:FoundInvalidType = found_invalid_type
    else
        true
    ),
    ( if
        ( MQUndefInst = found_undef_inst
        ; MQUndefMode = found_undef_mode
        )
    then
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred add_builtin_type_ctor_special_preds_in_builtin_module(type_ctor::in,
    module_info::in, module_info::out) is det.

add_builtin_type_ctor_special_preds_in_builtin_module(TypeCtor, !ModuleInfo) :-
    varset.init(TVarSet),
    term.context_init(Context),
    % These predicates are local only in the public builtin module,
    % but we *get here* only if we are compiling the public builtin module.
    TypeStatus = type_status(status_local),
    construct_type(TypeCtor, [], Type),
    % You cannot construct clauses to unify or compare values of an abstract
    % type. The abstract body tells the callee to generate code for
    % a builtin type.
    Body = hlds_abstract_type(abstract_type_general),
    % XXX For some reason, when generating C# code, if we don't add
    % the special preds now, we don't add them ever, which can cause compiler
    % aborts when we try to generate the type's type_ctor_info structure
    % (since we need to put references to the type's unify and compare
    % predicates into that structure.)
    add_special_pred_decl_defns_for_type_eagerly(TVarSet,
        Type, TypeCtor, Body, TypeStatus, Context, !ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_item_avail_list(ims_item(list(item_avail))::in,
    module_info::in, module_info::out) is det.

add_item_avail_list(StatusItem, !ModuleInfo) :-
    StatusItem = ims_item(ItemMercuryStatus, Avails),
    list.foldl(add_item_avail(ItemMercuryStatus), Avails, !ModuleInfo).

:- pred add_item_avail(item_mercury_status::in, item_avail::in,
    module_info::in, module_info::out) is det.

add_item_avail(ItemMercuryStatus, Avail, !ModuleInfo) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = import_decl
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum)),
        ImportOrUse = use_decl
    ),
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_anywhere,
            Section = ms_interface
        ;
            ( ItemExport = item_export_nowhere
            ; ItemExport = item_export_only_submodules
            ),
            Section = ms_implementation
        ),
        module_add_avail_module_name(ModuleName, Section, ImportOrUse,
            yes(Context), !ModuleInfo)
    ;
        ItemMercuryStatus = item_defined_in_other_module(ItemImport),
        (
            ItemImport = item_import_int_concrete(ImportLocn),
            (
                ImportLocn = import_locn_ancestor_private_interface_proper,
                module_add_avail_module_name(ModuleName, ms_implementation,
                    ImportOrUse, no, !ModuleInfo),
                % Any import_module which comes from a private interface
                % must by definition be a module used by the parent module.
                module_info_add_parent_to_used_modules(ModuleName, !ModuleInfo)
            ;
                ( ImportLocn = import_locn_interface
                ; ImportLocn = import_locn_implementation
                ; ImportLocn = import_locn_import_by_ancestor
                ),
                module_add_indirectly_imported_module_name(ModuleName,
                    !ModuleInfo)
            )
        ;
            ( ItemImport = item_import_int_abstract
            ; ItemImport = item_import_opt_int
            ),
            module_add_indirectly_imported_module_name(ModuleName, !ModuleInfo)
        )
    ).

%---------------------------------------------------------------------------%

:- pred add_type_defn(sec_item(item_type_defn_info)::in,
    module_info::in, module_info::out,
    found_invalid_type::in, found_invalid_type::out,
    list(error_spec)::in, list(error_spec)::out,
    list(solver_aux_pred_info)::in, list(solver_aux_pred_info)::out,
    sec_list(item_mutable_info)::in, sec_list(item_mutable_info)::out) is det.

add_type_defn(SectionItem, !ModuleInfo, !FoundInvalidType, !Specs,
        !SolverAuxPredInfos, !SectionMutableItems) :-
    SectionItem = sec_item(SectionInfo, ItemTypeDefnInfo),
    SectionInfo = sec_info(ItemMercuryStatus, NeedQual),
    ItemTypeDefnInfo = item_type_defn_info(SymName, TypeParams, TypeDefn,
        TypeVarSet, Context, _SeqNum),
    (
        TypeDefn = parse_tree_solver_type(Detailssolver),
        Detailssolver =
            type_details_solver(SolverTypeDetails, _MaybeUserEqComp),
        % If this is a solver type, then we need to also declare and define
        % the compiler generated construction function and deconstruction
        % predicate for the special constrained data constructor.
        % do_parse_tree_to_hlds will declare and define these predicates
        % when it declares and defines other predicates, based on the
        % information we add to !SolverAuxPredInfos and !SectionMutableItems.
        %
        % Before switch detection, we turn calls to these functions/predicates
        % into ordinary constructions/deconstructions, but preserve the
        % corresponding impurity annotations.
        SolverAuxPredInfo = solver_aux_pred_info(SymName, TypeParams,
            TypeVarSet, SolverTypeDetails, Context,
            ItemMercuryStatus, NeedQual),
        !:SolverAuxPredInfos = [SolverAuxPredInfo | !.SolverAuxPredInfos],

        MutableItems = SolverTypeDetails ^ std_mutable_items,
        list.map(wrap_with_section_info(SectionInfo), MutableItems,
            TypeSectionMutableItems),
        !:SectionMutableItems =
            TypeSectionMutableItems ++ !.SectionMutableItems
    ;
        ( TypeDefn = parse_tree_du_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        )
    ),
    item_mercury_status_to_type_status(ItemMercuryStatus, TypeStatus),
    module_add_type_defn(TypeStatus, NeedQual, ItemTypeDefnInfo,
        !ModuleInfo, !FoundInvalidType, !Specs).

%---------------------------------------------------------------------------%

:- pred add_inst_defn(ims_item(item_inst_defn_info)::in,
    module_info::in, module_info::out,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_inst_defn(StatusItem, !ModuleInfo, !FoundInvalidInstOrMode, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemInstDefnInfo),
    item_mercury_status_to_inst_status(ItemMercuryStatus, InstStatus),
    % XXX PASS STRUCTURE We could get module_add_inst_defn to update
    % !FoundInvalidInstOrMode directly.
    module_add_inst_defn(ItemInstDefnInfo, InstStatus, FoundError,
        !ModuleInfo, !Specs),
    (
        FoundError = no
    ;
        FoundError = yes,
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    ).

%---------------------------------------------------------------------------%

:- pred add_mode_defn(ims_item(item_mode_defn_info)::in,
    module_info::in, module_info::out,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mode_defn(StatusItem, !ModuleInfo, !FoundInvalidInstOrMode, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemModeDefnInfo),
    item_mercury_status_to_mode_status(ItemMercuryStatus, ModeStatus),
    % XXX PASS STRUCTURE We could get module_add_mode_defn to update
    % !FoundInvalidInstOrMode directly.
    module_add_mode_defn(ItemModeDefnInfo, ModeStatus, FoundError,
        !ModuleInfo, !Specs),
    (
        FoundError = no
    ;
        FoundError = yes,
        !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
    ).

%---------------------------------------------------------------------------%

:- pred add_pred_decl(sec_item(item_pred_decl_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pred_decl(SectionItem, !ModuleInfo, !Specs) :-
    SectionItem = sec_item(SectionInfo, ItemPredDecl),
    SectionInfo = sec_info(ItemMercuryStatus, NeedQual),
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc, TypesAndModes,
        WithType, WithInst, MaybeDetism, Origin, TypeVarSet, InstVarSet,
        ExistQVars, Purity, ClassContext, Context, SeqNum),
    % Any WithType and WithInst annotations should have been expanded
    % and the type and/or inst put into TypesAndModes by equiv_type.m.
    expect(unify(WithType, no), $module, $pred, "WithType != no"),
    expect(unify(WithInst, no), $module, $pred, "WithInst != no"),

    PredName = unqualify_name(PredSymName),
    ( if PredName = "" then
        Pieces = [words("Error: you cannot declare a"),
            words(pred_or_func_to_full_str(PredOrFunc)),
            words("whose name is a variable."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        % If this predicate was added as a result of the mutable transformation
        % then mark this predicate as a mutable access pred. We do this so that
        % we can tell optimizations, like inlining, to treat it specially.
        init_markers(Markers0),
        (
            Origin = item_origin_compiler(CompilerAttrs),
            CompilerAttrs = item_compiler_attributes(_AllowExport, IsMutable),
            (
                IsMutable = is_mutable,
                add_marker(marker_mutable_access_pred, Markers0, Markers)
            ;
                IsMutable = is_not_mutable,
                Markers = Markers0
            )
        ;
            Origin = item_origin_user,
            Markers = Markers0
        ),
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        % XXX ITEM_LIST Fix this lie.
        PredOrigin = origin_user(PredSymName),
        module_add_pred_or_func(PredOrigin, Context, SeqNum,
            yes(ItemMercuryStatus), PredStatus, NeedQual,
            PredOrFunc, PredSymName, TypeVarSet, InstVarSet, ExistQVars,
            TypesAndModes, ClassContext, MaybeDetism, Purity, Markers, _,
            !ModuleInfo, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred add_mode_decl(ims_item(item_mode_decl_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_mode_decl(StatusItem, !ModuleInfo, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemModeDecl),
    ItemModeDecl = item_mode_decl_info(PredSymName, MaybePredOrFunc, Modes,
        _WithInst, MaybeDet, VarSet, Context, SeqNum),

    PredName = unqualify_name(PredSymName),
    ( if PredName = "" then
        Pieces = [words("Error: you cannot declare a mode"),
            words("for a predicate whose name is a variable."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        (
            MaybePredOrFunc = yes(PredOrFunc),
            item_mercury_status_to_pred_status(ItemMercuryStatus, ModeStatus),
            module_add_mode(Context, SeqNum,
                yes(ItemMercuryStatus), ModeStatus,
                PredOrFunc, PredSymName, VarSet, Modes, MaybeDet,
                is_not_a_class_method, _, !ModuleInfo, !Specs)
        ;
            MaybePredOrFunc = no,
            % equiv_type.m should have either set the pred_or_func
            % or removed the item from the parse tree.
            unexpected($module, $pred, "no pred_or_func on mode declaration")
        )
    ).

%---------------------------------------------------------------------------%

:- pred maybe_add_default_mode(sec_item(item_pred_decl_info)::in,
    module_info::in, module_info::out) is det.

maybe_add_default_mode(SectionItem, !ModuleInfo) :-
    SectionItem = sec_item(_SectionInfo, ItemPredDecl),
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc, TypesAndModes,
        _Origin, _TypeVarSet, _InstVarSet, _ExistQVars, _WithType, _WithInst,
        _MaybeDet, _Purity, _ClassContext, _Context, _SeqNum),

    % Add default modes for function declarations, if necessary.
    PredName = unqualify_name(PredSymName),
    ( if PredName = "" then
        % We didn't add the predicate declaration itself above,
        % so we cannot possibly add anything to it now.
        true
    else
        (
            PredOrFunc = pf_predicate
        ;
            PredOrFunc = pf_function,
            list.length(TypesAndModes, Arity),
            adjust_func_arity(pf_function, FuncArity, Arity),
            module_info_get_predicate_table(!.ModuleInfo, PredTable0),
            predicate_table_lookup_func_sym_arity(PredTable0,
                is_fully_qualified, PredSymName, FuncArity, PredIds),
            (
                PredIds = [_ | _],
                predicate_table_get_preds(PredTable0, Preds0),
                maybe_add_default_func_modes(PredIds, Preds0, Preds),
                predicate_table_set_preds(Preds, PredTable0, PredTable),
                module_info_set_predicate_table(PredTable, !ModuleInfo)
            ;
                PredIds = [],
                unexpected($module, $pred, "can't find func declaration")
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_clause(ims_item(item_clause_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_clause(StatusItem, !ModuleInfo, !QualInfo, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemClauseInfo),
    ItemClauseInfo = item_clause_info(PredSymName, PredOrFunc, Args, Origin,
        VarSet, MaybeBodyGoal, Context, SeqNum),

    PredName = unqualify_name(PredSymName),
    ( if PredName = "" then
        Pieces = [words("Error: you cannot define a clause for a"),
            words(pred_or_func_to_full_str(PredOrFunc)),
            words("whose name is a variable."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    else
        (
            ItemMercuryStatus = item_defined_in_this_module(ItemExport),
            (
                ItemExport = item_export_anywhere,
                (
                    Origin = item_origin_user,
                    list.length(Args, Arity),

                    % There is no point printing out the qualified name
                    % since that information is already in the context.
                    ClauseId = simple_call_id_to_string(PredOrFunc,
                        sym_name_arity(unqualified(PredName), Arity)),
                    error_is_exported(Context,
                        [words("clause for " ++ ClauseId)], !Specs)
                ;
                    Origin = item_origin_compiler(CompilerAttrs),
                    CompilerAttrs = item_compiler_attributes(AllowExport,
                        _IsMutable),
                    (
                        AllowExport = do_allow_export
                    ;
                        AllowExport = do_not_allow_export,
                        unexpected($module, $pred, "bad introduced clause")
                    )
                )
            ;
                ( ItemExport = item_export_nowhere
                ; ItemExport = item_export_only_submodules
                )
            )
        ;
            ItemMercuryStatus = item_defined_in_other_module(_)
            % Clauses defined in other modules are NOT an error; they can be
            % imported from optimization files.
        ),
        % At this stage we only need know that it is not a promise declaration.
        item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
        module_add_clause(VarSet, PredOrFunc, PredSymName, Args, MaybeBodyGoal,
            PredStatus, Context, yes(SeqNum), goal_type_none,
            !ModuleInfo, !QualInfo, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred add_promise(ims_item(item_promise_info)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_promise(StatusItem, !ModuleInfo, !QualInfo, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemPromiseInfo),
    ItemPromiseInfo = item_promise_info(PromiseType, Goal, VarSet, UnivVars,
        Context, _SeqNum),
    % If the outermost universally quantified variables are placed in the head
    % of the dummy predicate, the typechecker will avoid warning about unbound
    % type variables as this implicitly adds a universal quantification of the
    % type variables needed.
    term.var_list_to_term_list(UnivVars, HeadVars),
    (
        % Extra error checking for promise ex declarations.
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        check_promise_ex_decl(UnivVars, PromiseType, Goal, Context, !Specs)
    ;
        PromiseType = promise_type_true
    ),
    % Add as dummy predicate.
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context,
        PredStatus, !ModuleInfo, !QualInfo, !Specs).

:- pred add_promise_clause(promise_type::in, list(term(prog_var_type))::in,
    prog_varset::in, goal::in, prog_context::in, pred_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context, Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    term.context_line(Context, Line),
    term.context_file(Context, File),
    string.format("%s__%d__%s",
        [s(prog_out.promise_to_string(PromiseType)), i(Line), s(File)], Name),

    % Promise declarations are recorded as a predicate with a goal_type
    % of goal_type_promise(X), where X is of promise_type. This allows us
    % to leverage off all the other checks in the compiler that operate
    % on predicates.
    %
    % :- promise all [A,B,R] ( R = A + B <=> R = B + A ).
    %
    % becomes
    %
    % promise__lineno__filename(A, B, R) :-
    %   ( R = A + B <=> R = B + A ).
    %
    % The double underscores in the compiler-generated name would be turned
    % into module qualifications if the name were provided by the user,
    % guarding against accidental name clashes.

    module_info_get_name(!.ModuleInfo, ModuleName),
    module_add_clause(VarSet, pf_predicate, qualified(ModuleName, Name),
        HeadVars, ok1(Goal), Status, Context, no,
        goal_type_promise(PromiseType), !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_initialise(ims_item(item_initialise_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_initialise(StatusItem, !ModuleInfo, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemInitialise),
    ItemInitialise = item_initialise_info(SymName, Arity, Origin, Context,
        _SeqNum),
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_anywhere,
            error_is_exported(Context,
                [decl("initialise"), words("declaration")], !Specs)
        ;
            ( ItemExport = item_export_nowhere
            ; ItemExport = item_export_only_submodules
            )
        ),
        (
            Origin = item_origin_user,
            implement_initialise(SymName, Arity, Context, !ModuleInfo, !Specs)
        ;
            Origin = item_origin_compiler(_CompilerAttrs),
            unexpected($module, $pred, "bad introduced initialise declaration")
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
        % It is OK if the initialise is defined in a parent module,
        % and we get to see it (though it SHOULD be kept out of .int0 files),
        % but we should NOT implement it.
    ).

:- pred add_finalise(ims_item(item_finalise_info)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_finalise(StatusItem, !ModuleInfo, !Specs) :-
    StatusItem = ims_item(ItemMercuryStatus, ItemFinaliseInfo),
    ItemFinaliseInfo = item_finalise_info(SymName, Arity, Origin, Context,
        _SeqNum),
    (
        ItemMercuryStatus = item_defined_in_this_module(ItemExport),
        (
            ItemExport = item_export_anywhere,
            error_is_exported(Context,
                [decl("finalise"), words("declaration")], !Specs)
        ;
            ( ItemExport = item_export_nowhere
            ; ItemExport = item_export_only_submodules
            )
        ),
        (
            Origin = item_origin_user,
            implement_finalise(SymName, Arity, Context, !ModuleInfo, !Specs)
        ;
            Origin = item_origin_compiler(_),
            unexpected($module, $pred, "bad introduced finalise declaration")
        )
    ;
        ItemMercuryStatus = item_defined_in_other_module(_)
        % It is OK if the initialise is defined in a parent module,
        % and we get to see it (though it SHOULD be kept out of .int0 files),
        % but we should NOT implement it.
    ).

%---------------------%

:- pred implement_initialise(sym_name::in, arity::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

implement_initialise(SymName, Arity, Context, !ModuleInfo, !Specs) :-
    % To implement an `:- initialise initpred.' declaration for C backends,
    % we need to:
    %
    % (1) construct a new C function name, CName, to use to export initpred,
    % (2) add the export pragma that does this,
    % (3) record the initpred/cname pair in the ModuleInfo so that
    %     code generation can ensure CName is called during module
    %     initialisation.
    %
    % For the Erlang backend, we need to have the initpred recorded in the
    % ModuleInfo. This is implied by the handling for the C backends.

    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, Arity, PredIds),
    (
        PredIds = [],
        Pieces = [words("Error:"),
            qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("used in"), decl("initialise"), words("declaration"),
            words("does not have a corresponding"),
            decl("pred"), words("declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            pred_info_get_proc_table(PredInfo, ProcTable),
            ProcInfos = map.values(ProcTable),
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            ExportLang =
                target_lang_to_foreign_export_lang(CompilationTarget),
            ( if
                ArgTypes = [Arg1Type, Arg2Type],
                type_is_io_state(Arg1Type),
                type_is_io_state(Arg2Type),
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [di_mode, uo_mode],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_pure
            then
                module_info_new_user_init_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName,
                    [di_mode, uo_mode], pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                % ZZZ should be do_not_allow_export
                Attrs = item_compiler_attributes(do_allow_export,
                    is_not_mutable),
                Origin = item_origin_compiler(Attrs),
                add_pragma_foreign_proc_export(Origin, FPEInfo, Context,
                    !ModuleInfo, !Specs)
            else if
                ArgTypes = [],
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_impure
            then
                module_info_new_user_init_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName, [],
                    pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                % ZZZ should be do_not_allow_export
                Attrs = item_compiler_attributes(do_allow_export,
                    is_not_mutable),
                Origin = item_origin_compiler(Attrs),
                add_pragma_foreign_proc_export(Origin, FPEInfo, Context,
                    !ModuleInfo, !Specs)
            else
                Pieces = [words("Error:"),
                    qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
                    words("used in initialise declaration"),
                    words("has invalid signature."), nl],
                % TODO: provide verbose error information here.
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            TailPredIds = [_ | _],
            Pieces = [words("Error:"),
                qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
                words("used in initialise declaration"),
                words("matches multiple pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------%

:- pred implement_finalise(sym_name::in, arity::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

implement_finalise(SymName, Arity, Context, !ModuleInfo, !Specs) :-
    % To implement a `:- finalise finalpred.' declaration for C backends,
    % we need to:
    %
    % (1) construct a new C function name, CName, to use to export finalpred,
    % (2) add `:- pragma foreign_export("C", finalpred(di, uo), CName).',
    % (3) record the finalpred/cname pair in the ModuleInfo so that
    % code generation can ensure cname is called during module finalisation.
    %
    % For the Erlang backend, we need to have the finalpred recorded in the
    % ModuleInfo. This is implied by the handling for the C backends.

    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, Arity, PredIds),
    (
        PredIds = [],
        Pieces = [words("Error:"),
            qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
            words("used in"), decl("finalise"), words("declaration"),
            words("does not have a corresponding"),
            decl("pred"), words("declaration."), nl],
        Msg = simple_msg(Context, [always(Pieces)]),
        Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ;
        PredIds = [HeadPredId | TailPredIds],
        (
            TailPredIds = [],
            PredId = HeadPredId,
            module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
            pred_info_get_arg_types(PredInfo, ArgTypes),
            pred_info_get_proc_table(PredInfo, ProcTable),
            ProcInfos = map.values(ProcTable),
            module_info_get_globals(!.ModuleInfo, Globals),
            globals.get_target(Globals, CompilationTarget),
            ExportLang = target_lang_to_foreign_export_lang(CompilationTarget),
            ( if
                ArgTypes = [Arg1Type, Arg2Type],
                type_is_io_state(Arg1Type),
                type_is_io_state(Arg2Type),
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [di_mode, uo_mode],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det ; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_pure
            then
                module_info_new_user_final_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName,
                    [di_mode, uo_mode], pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                % ZZZ should be do_not_allow_export
                Attrs = item_compiler_attributes(do_allow_export,
                    is_not_mutable),
                PEOrigin = item_origin_compiler(Attrs),
                add_pragma_foreign_proc_export(PEOrigin, FPEInfo, Context,
                    !ModuleInfo, !Specs)
            else if
                ArgTypes = [],
                list.member(ProcInfo, ProcInfos),
                proc_info_get_maybe_declared_argmodes(ProcInfo,
                    MaybeHeadModes),
                MaybeHeadModes = yes(HeadModes),
                HeadModes = [],
                proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
                MaybeDetism = yes(Detism),
                ( Detism = detism_det; Detism = detism_cc_multi ),
                pred_info_get_purity(PredInfo, Purity),
                Purity = purity_impure
            then
                module_info_new_user_final_pred(SymName, Arity, CName,
                    !ModuleInfo),
                PredNameModesPF = pred_name_modes_pf(SymName,
                    [], pf_predicate),
                FPEInfo = pragma_info_foreign_proc_export(ExportLang,
                    PredNameModesPF, CName),
                % ZZZ should be do_not_allow_export
                Attrs = item_compiler_attributes(do_allow_export,
                    is_not_mutable),
                PEOrigin = item_origin_compiler(Attrs),
                add_pragma_foreign_proc_export(PEOrigin, FPEInfo, Context,
                    !ModuleInfo, !Specs)
            else
                Pieces = [words("Error:"),
                    qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
                    words("used in"), decl("finalise"),
                    words("declaration has invalid signature."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            TailPredIds = [_ | _],
            Pieces = [words("Error:"),
                qual_sym_name_and_arity(sym_name_arity(SymName, Arity)),
                words("used in"), decl("finalise"), words("declaration"),
                words("has multiple"), decl("pred"), words("declarations."),
                nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------------------------------------------------------------%

:- pred add_version_numbers(module_name::in, version_numbers::in,
    qual_info::in, qual_info::out) is det.

add_version_numbers(ModuleName, VersionNumbers, !QualInfo) :-
    % Record the version numbers for each imported module
    % if smart recompilation is enabled.
    apply_to_recompilation_info(
        update_module_version_numbers(ModuleName, VersionNumbers),
        !QualInfo).

:- pred update_module_version_numbers(module_name::in,
    recompilation.version_numbers::in,
    recompilation_info::in, recompilation_info::out) is det.

update_module_version_numbers(ModuleName, ModuleVersionNumbers, !RecompInfo) :-
    VersionNumbersMap0 = !.RecompInfo ^ recomp_version_numbers,
    map.set(ModuleName, ModuleVersionNumbers,
        VersionNumbersMap0, VersionNumbersMap),
    !RecompInfo ^ recomp_version_numbers := VersionNumbersMap.

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.
%---------------------------------------------------------------------------%
