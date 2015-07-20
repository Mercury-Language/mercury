%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% We create the initial HLDS from the parse tree in three passes.
% Here are the tasks of the passes, and some of the ordering constraints
% between their activities.
%
% pass 1:
%
%   Add the declarations one by one to the module, except for pragmas.
%
% pass 2:
%
%   Add pragmas one by one to the module, and add default modes
%   for functions with no mode declaration.
%
%   Adding type definitions needs to come after we have added the pred
%   declarations, since we need to have the pred_id for `index/2' and
%   `compare/3' when we add compiler-generated clauses for `compare/3'.
%   (And similarly for other compiler-generated predicates like that.)
%
%   Adding pragmas needs to come after we have added the
%   pred declarations, in order to allow the pragma declarations
%   for a predicate to syntactically precede the pred declaration.
%
%   Adding default modes for functions needs to come after we have
%   processed all the mode declarations, since otherwise we can't be sure
%   that there isn't a mode declaration for the function.
%
% pass 3:
%
%   Add the clauses and clause-like-pragmas one by one to the module.
%
%   Check that the declarations for field extraction and update functions
%   are sensible.
%
%   Check that predicates listed in `:- initialise' and `:- finalise'
%   declarations exist and have the correct signature, introduce
%   pragma foreign_export declarations for them and record their exported
%   names in the module_info, so that we can generate code to call them
%   at initialisation/finalisation time.
%
% XXX It would be nice to document all the other constraints as well.
% I (zs) am sure there are others.
% When all the relevant constrainst are documented, it should be possible
% to move away from the pass model overall. Instead of traversing all the
% items in each pass, the first traversal of the items could, besides doing
% what the first pass does now, could also build separate lists of items
% for each subsequent traversal.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.make_hlds_passes.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.make_hlds.qual_info.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.

:- import_module bool.
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

:- include_module hlds.make_hlds.make_hlds_passes.du_type_layout.

:- import_module check_hlds.clause_to_proc.
:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.add_clause.
:- import_module hlds.make_hlds.add_foreign_proc.
:- import_module hlds.make_hlds.add_mode.
:- import_module hlds.make_hlds.add_mutable_aux_preds.
:- import_module hlds.make_hlds.add_pragma.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.add_solver.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.add_type.
:- import_module hlds.make_hlds.make_hlds_error.
:- import_module hlds.make_hlds.make_hlds_passes.du_type_layout.
:- import_module hlds.make_hlds.make_hlds_warn.
:- import_module hlds.pred_table.
:- import_module hlds.special_pred.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module recompilation.

:- import_module map.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

do_parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo0,
        TypeEqvMapMap, UsedModules, QualInfo,
        FoundInvalidType, FoundInvalidInstOrMode, !:ModuleInfo, !:Specs) :-
    mq_info_get_partial_qualifier_info(MQInfo0, PQInfo),
    module_info_init(AugCompUnit, DumpBaseFileName, Globals, PQInfo, no,
        !:ModuleInfo),
    module_info_set_used_modules(UsedModules, !ModuleInfo),
    AugCompUnit = compilation_unit(Name, _, AugItemBlocks),
    add_block_decls_pass_1(AugItemBlocks,
        did_not_find_invalid_inst_or_mode, FoundInvalidInstOrMode1,
        !ModuleInfo, [], Pass1Specs),
    globals.lookup_bool_option(Globals, statistics, Statistics),
    trace [io(!IO)] (
        maybe_write_string(Statistics, "% Processed all items in pass 1\n",
            !IO),
        maybe_report_stats(Statistics, !IO)
    ),

    add_block_decls_pass_2(AugItemBlocks, !ModuleInfo, [], Pass2Specs),
    !:Specs = Pass1Specs ++ Pass2Specs,
    % XXX Using Pass2Specs = [_ | _] as the test for found_invalid_type
    % is both too tight and too loose. Too tight because some pass 2 errors
    % are not type errors, and too loose because invalid types are now found
    % in pass 1. We avoid the second problem by recording the presence of
    % an error in a type definition in the type definition itself, and then
    % reporting that error in pass 2, but a more direct solution (allowing
    % the processing of every relevant item during either pass 1 or pass 2
    % to just set the found_invalid_type flag) would be better.
    (
        Pass2Specs = [],
        some [!TypeTable] (
            % Figure out how arguments should be stored into fields
            % before constructors are added to the HLDS.
            module_info_get_type_table(!.ModuleInfo, !:TypeTable),
            foldl_over_type_ctor_defns(decide_du_type_layout(!.ModuleInfo),
                !.TypeTable, !TypeTable),
            module_info_set_type_table(!.TypeTable, !ModuleInfo),

            % Add constructors and special preds to the HLDS. This must be done
            % after adding all type and `:- pragma foreign_type' declarations.
            % If there were errors in foreign type type declarations, doing
            % this may cause a compiler abort.
            foldl3_over_type_ctor_defns(process_type_defn, !.TypeTable,
                did_not_find_invalid_type, FoundInvalidType1,
                !ModuleInfo, !Specs)
        )
    ;
        Pass2Specs = [_ | _],
        FoundInvalidType1 = found_invalid_type
    ),

    % Add the special preds for the builtin types which don't have a
    % type declaration, hence no hlds_type_defn is generated for them.
    (
        Name = mercury_public_builtin_module,
        compiler_generated_rtti_for_builtins(!.ModuleInfo)
    ->
        list.foldl(add_builtin_type_ctor_special_preds,
            builtin_type_ctors_with_no_hlds_type_defn, !ModuleInfo)
    ;
        true
    ),

    % Balance any data structures that need it.
    module_info_optimize(!ModuleInfo),
    trace [io(!IO)] (
        maybe_write_string(Statistics, "% Processed all items in pass 2\n",
            !IO),
        maybe_report_stats(Statistics, !IO)
    ),

    init_qual_info(MQInfo0, TypeEqvMapMap, QualInfo0),
    add_blocks_pass_3(AugItemBlocks,
        !ModuleInfo, QualInfo0, QualInfo, !Specs),
    trace [io(!IO)] (
        maybe_write_string(Statistics, "% Processed all items in pass 3\n",
            !IO)
    ),

    qual_info_get_mq_info(QualInfo, MQInfo),
    mq_info_get_type_error_flag(MQInfo, MQInvalidType),
    mq_info_get_mode_error_flag(MQInfo, MQInvalidInstOrMode),
    (
        FoundInvalidType1 = did_not_find_invalid_type,
        MQInvalidType = no
    ->
        FoundInvalidType = did_not_find_invalid_type
    ;
        FoundInvalidType = found_invalid_type
    ),
    (
        FoundInvalidInstOrMode1 = did_not_find_invalid_inst_or_mode,
        MQInvalidInstOrMode = no
    ->
        FoundInvalidInstOrMode = did_not_find_invalid_inst_or_mode
    ;
        FoundInvalidInstOrMode = found_invalid_inst_or_mode
    ).

%---------------------------------------------------------------------------%

:- pred add_builtin_type_ctor_special_preds(type_ctor::in,
    module_info::in, module_info::out) is det.

add_builtin_type_ctor_special_preds(TypeCtor, !ModuleInfo) :-
    varset.init(TVarSet),
    Body = hlds_abstract_type(abstract_type_general),
    term.context_init(Context),
    Status = status_local,
    construct_type(TypeCtor, [], Type),

    % XXX We call `eagerly_add_special_preds' instead of `add_special_preds'
    % to bypass a call to `special_pred_is_generated_lazily' which calls
    % `classify_type_ctor'. `classify_type_ctor' knows about unqualified
    % builtin types, but not the qualified types like `builtin.int'/0 from
    % `builtin_type_ctors_with_no_hlds_type_defn'. Eventually it tries to
    % look up the builtin type from the type definition table, and aborts as
    % it won't find it.
    %
    % The special preds for these types shouldn't be generated lazily anyway.

    eagerly_add_special_preds(TVarSet, Type, TypeCtor, Body, Context, Status,
        !ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_block_decls_pass_1(list(aug_item_block)::in,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_block_decls_pass_1([],
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs).
add_block_decls_pass_1([AugItemBlock | AugItemBlocks],
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs) :-
    AugItemBlock = item_block(Section, _, Items),
    aug_module_section_status(Section, MaybeStatus),
    (
        MaybeStatus = yes(Status),
        add_item_decls_pass_1(Status, Items,
            !FoundInvalidInstOrMode, !ModuleInfo, !Specs)
    ;
        MaybeStatus = no,
        expect(unify(Items, []), $module, $pred,
            "MaybeStatus = no but Items != []")
    ),
    add_block_decls_pass_1(AugItemBlocks,
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs).

:- pred add_item_decls_pass_1(item_status::in, list(item)::in,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_decls_pass_1(_, [],
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs).
add_item_decls_pass_1(Status, [Item | Items],
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs) :-
    add_item_decl_pass_1(Status, Item,
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs),
    add_item_decls_pass_1(Status, Items,
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs).

%---------------------%

:- pred add_block_decls_pass_2(list(aug_item_block)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_block_decls_pass_2([], !ModuleInfo, !Specs).
add_block_decls_pass_2([AugItemBlock | AugItemBlocks], !ModuleInfo, !Specs) :-
    AugItemBlock = item_block(Section, _, Items),
    aug_module_section_status(Section, MaybeStatus),
    (
        MaybeStatus = yes(Status),
        add_item_decls_pass_2(Status, Items, !ModuleInfo, !Specs)
    ;
        MaybeStatus = no,
        expect(unify(Items, []), $module, $pred,
            "MaybeStatus = no but Items != []")
    ),
    add_block_decls_pass_2(AugItemBlocks, !ModuleInfo, !Specs).

:- pred add_item_decls_pass_2(item_status::in, list(item)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_decls_pass_2(_, [], !ModuleInfo, !Specs).
add_item_decls_pass_2(Status, [Item | Items], !ModuleInfo, !Specs) :-
    add_item_decl_pass_2(Status, Item, !ModuleInfo, !Specs),
    add_item_decls_pass_2(Status, Items, !ModuleInfo, !Specs).

%---------------------%

:- pred add_blocks_pass_3(list(aug_item_block)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_blocks_pass_3([], !ModuleInfo, !QualInfo, !Specs).
add_blocks_pass_3([AugItemBlock | AugItemBlocks], !ModuleInfo,
        !QualInfo, !Specs) :-
    AugItemBlock = item_block(Section, _, Items),
    aug_module_section_status(Section, MaybeStatus),
    (
        MaybeStatus = yes(Status),
        Status = item_status(ImportStatus, NeedQualifier),
        qual_info_get_mq_info(!.QualInfo, MQInfo0),
        mq_info_set_need_qual_flag(NeedQualifier, MQInfo0, MQInfo),
        qual_info_set_mq_info(MQInfo, !QualInfo),
        add_items_pass_3(ImportStatus, Items, !ModuleInfo, !QualInfo, !Specs)
    ;
        MaybeStatus = no,
        expect(unify(Items, []), $module, $pred,
            "MaybeStatus = no but Items != []")
    ),
    add_blocks_pass_3(AugItemBlocks, !ModuleInfo, !QualInfo, !Specs).

:- pred add_items_pass_3(import_status::in, list(item)::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_items_pass_3(_, [], !ModuleInfo, !QualInfo, !Specs).
add_items_pass_3(Status, [Item | Items], !ModuleInfo, !QualInfo, !Specs) :-
    add_item_pass_3(Status, Item, !ModuleInfo, !QualInfo, !Specs),
    add_items_pass_3(Status, Items, !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % The bool records whether any cyclic insts or modes were detected.
    %
:- pred add_item_decl_pass_1(item_status::in, item::in,
    found_invalid_inst_or_mode::in, found_invalid_inst_or_mode::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_decl_pass_1(Status, Item,
        !FoundInvalidInstOrMode, !ModuleInfo, !Specs) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        add_pass_1_module_defn(ItemModuleDefn, Status, !ModuleInfo, !Specs)
    ;
        Item = item_type_defn(ItemTypeDefnInfo),
        add_pass_1_type_defn(ItemTypeDefnInfo, Status, !ModuleInfo, !Specs)
    ;
        (
            Item = item_inst_defn(ItemInstDefnInfo),
            module_add_inst_defn(ItemInstDefnInfo, FoundError,
                Status, !ModuleInfo, !Specs)
        ;
            Item = item_mode_defn(ItemModeDefnInfo),
            module_add_mode_defn(ItemModeDefnInfo, FoundError,
                Status, !ModuleInfo, !Specs)
        ),
        (
            FoundError = yes,
            !:FoundInvalidInstOrMode = found_invalid_inst_or_mode
        ;
            FoundError = no
        )
    ;
        Item = item_pred_decl(ItemPredDecl),
        add_pass_1_pred_decl(ItemPredDecl, Status, !ModuleInfo, !Specs)
    ;
        Item = item_mode_decl(ItemModeDecl),
        add_pass_1_mode_decl(ItemModeDecl, Status, !ModuleInfo, !Specs)
    ;
        Item = item_typeclass(ItemTypeClass),
        module_add_class_defn(ItemTypeClass, Status, !ModuleInfo, !Specs)
    ;
        Item = item_mutable(ItemMutable),
        add_pass_1_mutable(ItemMutable, Status, !ModuleInfo, !Specs)
    ;
        ( Item = item_clause(_)
        ; Item = item_pragma(_)
        ; Item = item_promise(_)
        ; Item = item_instance(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_nothing(_)
        )
        % These will be processed only in later passes.
        %
        % We don't want to add clauses or pragma foreign_procs before we add
        % the declarations of the predicates they implement.
        %
        % We don't want to add instance declarations before the typeclass
        % declaration it implements.
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_1_module_defn(item_module_defn_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_module_defn(ItemModuleDefn, Status, !ModuleInfo, !Specs) :-
    ItemModuleDefn = item_module_defn_info(ModuleDefn, Context, _SeqNum),
    (
        ( ModuleDefn = md_import(ModuleSpecifier)
        ; ModuleDefn = md_use(ModuleSpecifier)
        ),
        Status = item_status(ImportStatus, _),
        add_module_specifier(ModuleSpecifier, ImportStatus, !ModuleInfo)
    ;
        ModuleDefn = md_external(MaybeBackend, External),
        ( External = name_arity(Name, Arity) ->
            module_info_get_globals(!.ModuleInfo, Globals),
            CurrentBackend = lookup_current_backend(Globals),
            (
                (
                    MaybeBackend = no
                ;
                    MaybeBackend = yes(Backend),
                    Backend = CurrentBackend
                )
            ->
                module_mark_as_external(Name, Arity, Context, !ModuleInfo,
                    !Specs)
            ;
                true
            )
        ;
            Pieces = [words("Warning:"), quote("external"),
                words("declaration requires arity."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ;
        ( ModuleDefn = md_include_module(_)
        ; ModuleDefn = md_version_numbers(_, _)
        )
    ).

:- pred add_module_specifier(module_specifier::in, import_status::in,
    module_info::in, module_info::out) is det.

add_module_specifier(Specifier, IStat, !ModuleInfo) :-
    ( status_defined_in_this_module(IStat) = yes ->
        module_add_imported_module_specifier(IStat, Specifier, !ModuleInfo)
    ; IStat = status_imported(import_locn_ancestor_private_interface_proper) ->
        module_add_imported_module_specifier(IStat, Specifier, !ModuleInfo),

        % Any import_module which comes from a private interface
        % must by definition be a module used by the parent module.
        module_info_add_parent_to_used_modules(Specifier, !ModuleInfo)
    ;
        module_add_indirectly_imported_module_specifier(Specifier,
            !ModuleInfo)
    ).

:- pred module_mark_as_external(sym_name::in, int::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

module_mark_as_external(PredName, Arity, Context, !ModuleInfo, !Specs) :-
    % `external' declarations can only apply to things defined in this module,
    % since everything else is already external.
    module_info_get_predicate_table(!.ModuleInfo, PredicateTable0),
    predicate_table_lookup_sym_arity(PredicateTable0, is_fully_qualified,
        PredName, Arity, PredIds),
    (
        PredIds = [_ | _],
        module_mark_preds_as_external(PredIds, !ModuleInfo)
    ;
        PredIds = [],
        undefined_pred_or_func_error(PredName, Arity, Context,
            [decl("external"), words("declaration")], !Specs)
    ).

:- pred module_mark_preds_as_external(list(pred_id)::in,
    module_info::in, module_info::out) is det.

module_mark_preds_as_external([], !ModuleInfo).
module_mark_preds_as_external([PredId | PredIds], !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, Preds0),
    map.lookup(Preds0, PredId, PredInfo0),
    pred_info_mark_as_external(PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, Preds0, Preds),
    module_info_set_preds(Preds, !ModuleInfo),
    module_mark_preds_as_external(PredIds, !ModuleInfo).

%---------------------------------------------------------------------------%

:- pred add_pass_1_type_defn(item_type_defn_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_type_defn(ItemTypeDefnInfo, Status, !ModuleInfo, !Specs) :-
    % If this is a solver type then we need to also add the declarations
    % for the compiler generated construction function and deconstruction
    % predicate for the special constrained data constructor.
    %
    % In pass 3 we add the corresponding clauses.
    %
    % Before switch detection, we turn calls to these functions/predicates
    % into ordinary constructions/deconstructions, but preserve the
    % corresponding impurity annotations.
    ItemTypeDefnInfo = item_type_defn_info(SymName, TypeParams, TypeDefn,
        TypeVarSet, Context, _SeqNum),
    ( TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp) ->
        add_solver_type_aux_pred_decls(TypeVarSet, SymName, TypeParams,
            SolverTypeDetails, Context, Status, !ModuleInfo, !Specs),
        MutableItems = SolverTypeDetails ^ std_mutable_items,
        add_solver_type_mutable_items_pass_1(MutableItems, Status,
            !ModuleInfo, !Specs)
    ;
        true
    ),
    module_add_type_defn(TypeVarSet, SymName, TypeParams, TypeDefn, Context,
        Status, !ModuleInfo, !Specs).

:- pred add_solver_type_mutable_items_pass_1(list(item_mutable_info)::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_pass_1([], _, !ModuleInfo, !Specs).
add_solver_type_mutable_items_pass_1([MutableInfo | MutableInfos], Status,
        !ModuleInfo, !Specs) :-
    add_pass_1_mutable(MutableInfo, Status, !ModuleInfo, !Specs),
    add_solver_type_mutable_items_pass_1(MutableInfos, Status,
        !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_1_pred_decl(item_pred_decl_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_pred_decl(ItemPredDecl, Status, !ModuleInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(PredName, PredOrFunc, TypesAndModes,
        WithType, WithInst, MaybeDetism, Origin, TypeVarSet, InstVarSet,
        ExistQVars, Purity, ClassContext, Context, _SeqNum),
    % Any WithType and WithInst annotations should have been expanded
    % and the type and/or inst put into TypesAndModes by equiv_type.m.
    expect(unify(WithType, no), $module, $pred, "WithType != no"),
    expect(unify(WithInst, no), $module, $pred, "WithInst != no"),

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
    % XXX ITEM_LIST Fix this lie.
    PredOrigin = origin_user(PredName),
    module_add_pred_or_func(PredOrigin, TypeVarSet, InstVarSet, ExistQVars,
        PredOrFunc, PredName, TypesAndModes, MaybeDetism, Purity, ClassContext,
        Markers, Context, Status, _, !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_1_mode_decl(item_mode_decl_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_mode_decl(ItemModeDecl, Status, !ModuleInfo, !Specs) :-
    ItemModeDecl = item_mode_decl_info(PredName, MaybePredOrFunc, Modes,
        _WithInst, MaybeDet, VarSet, Context, _SeqNum),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        Status = item_status(ImportStatus, _),
        IsClassMethod = no,
        module_add_mode(VarSet, PredName, Modes, MaybeDet, ImportStatus,
            Context, PredOrFunc, IsClassMethod, _, !ModuleInfo, !Specs)
    ;
        MaybePredOrFunc = no,
        % equiv_type.m should have either set the pred_or_func
        % or removed the item from the list.
        unexpected($module, $pred, "no pred_or_func on mode declaration")
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_1_mutable(item_mutable_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_1_mutable(ItemMutable, Status, !ModuleInfo, !Specs) :-
    % We add the initialise decl and the foreign_decl on the second pass and
    % the foreign_proc clauses on the third pass.
    Status = item_status(ImportStatus, _),
    DefinedThisModule = status_defined_in_this_module(ImportStatus),
    (
        DefinedThisModule = yes,
        add_mutable_aux_pred_decls(ItemMutable, Status, !ModuleInfo, !Specs)
    ;
        DefinedThisModule = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_item_decl_pass_2(item_status::in, item::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_decl_pass_2(Status, Item, !ModuleInfo, !Specs) :-
    (
        Item = item_module_defn(_ItemModuleDefn)
    ;
        Item = item_type_defn(ItemTypeDefn),
        add_pass_2_type_defn(ItemTypeDefn, Status, !ModuleInfo, !Specs)
    ;
        Item = item_pred_decl(ItemPredDecl),
        add_pass_2_pred_decl(ItemPredDecl, Status, !ModuleInfo, !Specs)
    ;
        Item = item_pragma(ItemPragma),
        add_pass_2_pragma(ItemPragma, Status, !ModuleInfo, !Specs)
    ;
        Item = item_instance(ItemInstance),
        add_pass_2_instance(ItemInstance, Status, !ModuleInfo, !Specs)
    ;
        Item = item_initialise(ItemInitialise),
        add_pass_2_initialise(ItemInitialise, Status, !ModuleInfo, !Specs)
    ;
        Item = item_finalise(ItemFinalise),
        add_pass_2_finalise(ItemFinalise, Status, !ModuleInfo, !Specs)
    ;
        Item = item_mutable(ItemMutable),
        add_pass_2_mutable(ItemMutable, Status, !ModuleInfo, !Specs)
    ;
        ( Item = item_clause(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_nothing(_)
        )
        % Do nothing in pass 2 for these kinds of items.
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_2_type_defn(item_type_defn_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_type_defn(ItemTypeDefnInfo, Status, !ModuleInfo, !Specs) :-
    ItemTypeDefnInfo = item_type_defn_info(_SymName, _TypeParams, TypeDefn,
        _TypeVarSet, _Context, _SeqNum),
    ( TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp) ->
        MutableItems = SolverTypeDetails ^ std_mutable_items,
        add_solver_type_mutable_items_pass_2(MutableItems, Status,
            !ModuleInfo, !Specs)
    ;
        true
    ).

:- pred add_solver_type_mutable_items_pass_2(list(item_mutable_info)::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_pass_2([], _, !ModuleInfo, !Specs).
add_solver_type_mutable_items_pass_2([MutableInfo | MutableInfos], Status,
        !ModuleInfo, !Specs) :-
    add_pass_2_mutable(MutableInfo, Status, !ModuleInfo, !Specs),
    add_solver_type_mutable_items_pass_2(MutableInfos, Status,
        !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_2_pred_decl(item_pred_decl_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_pred_decl(ItemPredDecl, _Status, !ModuleInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc, TypesAndModes,
        _Origin, _TypeVarSet, _InstVarSet, _ExistQVars, _WithType, _WithInst,
        _MaybeDet, _Purity, _ClassContext, _Context, _SeqNum),
    % Add default modes for function declarations, if necessary.
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        list.length(TypesAndModes, Arity),
        adjust_func_arity(pf_function, FuncArity, Arity),
        module_info_get_predicate_table(!.ModuleInfo, PredTable0),
        predicate_table_lookup_func_sym_arity(PredTable0,
            is_fully_qualified, SymName, FuncArity, PredIds),
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
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_2_instance(item_instance_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_instance(ItemInstance, Status, !ModuleInfo, !Specs) :-
    ItemInstance = item_instance_info(Name, Types, OriginalTypes,
        Constraints, Body, VarSet, InstanceModuleName, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    (
        Body = instance_body_abstract,
        make_status_abstract(ImportStatus, BodyStatus)
    ;
        Body = instance_body_concrete(_),
        BodyStatus = ImportStatus
    ),
    module_add_instance_defn(InstanceModuleName, Constraints, Name,
        Types, OriginalTypes, Body, VarSet, BodyStatus, Context,
        !ModuleInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_2_initialise(item_initialise_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_initialise(ItemInitialise, Status, !ModuleInfo, !Specs) :-
    % These are processed properly during pass 3, we just do some
    % error checking at this point.
    ItemInitialise = item_initialise_info(_, _, Origin, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        (
            Origin = item_origin_user,
            error_is_exported(Context,
                [decl("initialise"), words("declaration")], !Specs)
        ;
            Origin = item_origin_compiler(CompilerAttrs),
            CompilerAttrs = item_compiler_attributes(_AllowExport, IsMutable),
            (
                % Ignore the error if this initialise declaration was
                % introduced because of a mutable declaration.
                IsMutable = is_mutable
            ;
                IsMutable = is_not_mutable,
                unexpected($module, $pred,
                    "bad introduced initialise declaration")
            )
        )
    ;
        true
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_2_finalise(item_finalise_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_finalise(ItemFinalise, Status, !ModuleInfo, !Specs) :-
    % There are processed properly during pass 3, we just do some error
    % checking at this point.
    ItemFinalise = item_finalise_info(_, _, Origin, Context, _SeqNum),
    Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        (
            Origin = item_origin_user,
            error_is_exported(Context,
                [decl("finalise"), words("declaration")], !Specs)
        ;
            % There are no source-to-source transformations that introduce
            % finalise declarations.
            Origin = item_origin_compiler(_),
            unexpected($module, $pred, "bad introduced finalise declaration")
        )
    ;
        true
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_2_mutable(item_mutable_info::in,
    item_status::in, module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_2_mutable(ItemMutable, Status, !ModuleInfo, !Specs) :-
    Status = item_status(ImportStatus, _),
    ( ImportStatus = status_exported ->
        ItemMutable = item_mutable_info(_Name, _Type, _InitTerm, _Inst,
            _MutAttrs, _VarSet, Context, _SeqNum),
        error_is_exported(Context,
            [decl("mutable"), words("declaration")], !Specs)
    ;
        true
    ),

    % ZZZ wrong place for this comment.
    % We don't implement the `mutable' declaration unless it is defined in
    % this module. If we did not have this check, we would duplicate
    % the definition of the global variable storing the mutable
    % in any submodules of the module that actually defined the mutable.

    DefinedThisModule = status_defined_in_this_module(ImportStatus),
    (
        DefinedThisModule = yes,
        do_mutable_checks(ItemMutable, Status, !ModuleInfo, !Specs)
    ;
        DefinedThisModule = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred add_item_pass_3(import_status::in, item::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_item_pass_3(Status, Item, !ModuleInfo, !QualInfo, !Specs) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        add_pass_3_module_defn(ItemModuleDefn, !QualInfo)
    ;
        Item = item_clause(ItemClause),
        add_pass_3_clause(ItemClause, Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        Item = item_type_defn(ItemTypeDefn),
        add_pass_3_type_defn(ItemTypeDefn, Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_pred_decl(ItemPredDecl),
        add_pass_3_pred_decl(ItemPredDecl, Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_pragma(ItemPragma),
        add_pass_3_pragma(ItemPragma, Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        Item = item_promise(ItemPromise),
        add_pass_3_promise(ItemPromise, Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        Item = item_initialise(ItemInitialise),
        add_pass_3_initialise(ItemInitialise, Status, !ModuleInfo, !QualInfo,
            !Specs)
    ;
        Item = item_finalise(ItemFinalise),
        add_pass_3_finalise(ItemFinalise, !ModuleInfo, !Specs)
    ;
        Item = item_mutable(ItemMutable),
        add_pass_3_mutable(ItemMutable, Status, !ModuleInfo, !QualInfo, !Specs)
    ;
        ( Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_mode_decl(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_nothing(_)
        )
        % Do nothing.
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_3_module_defn(item_module_defn_info::in,
    qual_info::in, qual_info::out) is det.

add_pass_3_module_defn(ItemModuleDefn, !QualInfo) :-
    ItemModuleDefn = item_module_defn_info(ModuleDefn, _Context, _SeqNum),
    ( ModuleDefn = md_version_numbers(ModuleName, ModuleVersionNumbers) ->
        % Record the version numbers for each imported module
        % if smart recompilation is enabled.
        apply_to_recompilation_info(
            update_module_version_numbers(ModuleName, ModuleVersionNumbers),
            !QualInfo)
    ;
        true
    ).

:- pred update_module_version_numbers(module_name::in,
    recompilation.version_numbers::in,
    recompilation_info::in, recompilation_info::out) is det.

update_module_version_numbers(ModuleName, ModuleVersionNumbers, !RecompInfo) :-
    VersionNumbersMap0 = !.RecompInfo ^ recomp_version_numbers,
    map.set(ModuleName, ModuleVersionNumbers,
        VersionNumbersMap0, VersionNumbersMap),
    !RecompInfo ^ recomp_version_numbers := VersionNumbersMap.

%---------------------------------------------------------------------------%

:- pred add_pass_3_clause(item_clause_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_clause(ItemClause, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemClause = item_clause_info(PredName, PredOrFunc, Args, Origin, VarSet,
        Body, Context, SeqNum),
    ( Status = status_exported ->
        (
            Origin = item_origin_user,
            list.length(Args, Arity),

            % There is no point printing out the qualified name since that
            % information is already in the context.
            UnqualifiedPredName = unqualify_name(PredName),
            ClauseId = simple_call_id_to_string(PredOrFunc,
                unqualified(UnqualifiedPredName) / Arity),
            error_is_exported(Context, [words("clause for " ++ ClauseId)],
                !Specs)
        ;
            Origin = item_origin_compiler(CompilerAttrs),
            CompilerAttrs = item_compiler_attributes(AllowExport, _IsMutable),
            (
                AllowExport = do_allow_export
            ;
                AllowExport = do_not_allow_export,
                unexpected($module, $pred, "bad introduced clause")
            )
        )
    ;
        true
    ),
    % At this stage we only need know that it's not a promise declaration.
    module_add_clause(VarSet, PredOrFunc, PredName, Args, Body, Status,
        Context, yes(SeqNum), goal_type_none, !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_3_type_defn(item_type_defn_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_type_defn(ItemTypeDefn, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemTypeDefn = item_type_defn_info(SymName, TypeParams, TypeDefn,
        _TypeVarSet, Context, _SeqNum),
    % If this is a solver type, then we need to also add the clauses for
    % the compiler generated inst cast predicate (the declaration for which
    % was added in pass 1). We should only add the clauses if this is the
    % module in which the solver type was defined though.
    (
        TypeDefn = parse_tree_solver_type(SolverTypeDetails, _MaybeUserEqComp),
        status_defined_in_this_module(Status) = yes
    ->
        add_solver_type_aux_pred_defns(SymName, TypeParams, SolverTypeDetails,
            Context, Status, !ModuleInfo, !QualInfo, !Specs),
        MutableItems = SolverTypeDetails ^ std_mutable_items,
        add_solver_type_mutable_items_clauses(MutableItems, Status,
            !ModuleInfo, !QualInfo, !Specs)
    ;
        true
    ).

:- pred add_solver_type_mutable_items_clauses(list(item_mutable_info)::in,
    import_status::in,
    module_info::in, module_info::out, qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_solver_type_mutable_items_clauses([], _Status,
        !ModuleInfo, !QualInfo, !Specs).
add_solver_type_mutable_items_clauses([MutableInfo | MutableInfos], Status,
        !ModuleInfo, !QualInfo, !Specs) :-
    add_pass_3_mutable(MutableInfo, Status, !ModuleInfo, !QualInfo, !Specs),
    add_solver_type_mutable_items_clauses(MutableInfos, Status,
        !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_3_pred_decl(item_pred_decl_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_pred_decl(ItemPredDecl, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc, TypesAndModes,
        _, _, _, _, _WithType, _WithInst, _, _, _, Context, _SeqNum),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        list.length(TypesAndModes, PredArity),
        adjust_func_arity(pf_function, FuncArity, PredArity),
        maybe_check_field_access_function(!.ModuleInfo, SymName, FuncArity,
            Status, Context, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_3_promise(item_promise_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_promise(ItemPromise, Status, !ModuleInfo, !QualInfo, !Specs) :-
    ItemPromise = item_promise_info(PromiseType, Goal, VarSet, UnivVars,
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
    add_promise_clause(PromiseType, HeadVars, VarSet, Goal, Context,
        Status, !ModuleInfo, !QualInfo, !Specs).

:- pred add_promise_clause(promise_type::in, list(term(prog_var_type))::in,
    prog_varset::in, goal::in, prog_context::in, import_status::in,
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
    % promise.lineno_filename(A, B, R) :-
    %   ( R = A + B <=> R = B + A ).

    module_info_get_name(!.ModuleInfo, ModuleName),
    module_add_clause(VarSet, pf_predicate, qualified(ModuleName, Name),
        HeadVars, Goal, Status, Context, no, goal_type_promise(PromiseType),
        !ModuleInfo, !QualInfo, !Specs).

%---------------------------------------------------------------------------%

:- pred add_pass_3_initialise(item_initialise_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_initialise(ItemInitialise, _Status, !ModuleInfo, !QualInfo,
        !Specs) :-
    ItemInitialise = item_initialise_info(SymName, Arity, Origin, Context,
        _SeqNum),
    (
        Origin = item_origin_user,
        add_pass_3_initialise_user(SymName, Arity, Context,
            !ModuleInfo, !Specs)
    ;
        Origin = item_origin_compiler(_CompilerAttrs),
        unexpected($module, $pred, "bad introduced initialise declaration")
    ).

:- pred add_pass_3_initialise_user(sym_name::in, arity::in, prog_context::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_initialise_user(SymName, Arity, Context, !ModuleInfo, !Specs) :-
    % To handle a `:- initialise initpred.' declaration for C backends,
    % we need to:
    % (1) construct a new C function name, CName, to use to export
    %     initpred,
    % (2) add the export pragma that does this
    % (3) record the initpred/cname pair in the ModuleInfo so that
    %     code generation can ensure cname is called during module
    %     initialisation.
    %
    % For the Erlang backend, we need to have the initpred recorded in the
    % ModuleInfo. This is implied by the handling for the C backends.

    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, Arity, PredIds),
    (
        PredIds = [],
        Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
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
            (
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
            ->
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
            ;
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
            ->
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
            ;
                Pieces = [words("Error:"),
                    sym_name_and_arity(SymName/Arity),
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
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in initialise declaration"),
                words("matches multiple pred declarations."), nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_3_finalise(item_finalise_info::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_finalise(ItemFinalise, !ModuleInfo, !Specs) :-
    % To handle a `:- finalise finalpred.' declaration for C backends,
    % we need to:
    % (1) construct a new C function name, CName, to use to export finalpred,
    % (2) add `:- pragma foreign_export("C", finalpred(di, uo), CName).',
    % (3) record the finalpred/cname pair in the ModuleInfo so that
    % code generation can ensure cname is called during module finalisation.
    %
    % For the Erlang backend, we need to have the finalpred recorded in the
    % ModuleInfo. This is implied by the handling for the C backends.

    ItemFinalise = item_finalise_info(SymName, Arity, Origin, Context,
        _SeqNum),
    (
        Origin = item_origin_compiler(_),
        unexpected($module, $pred, "bad introduced finalise declaration")
    ;
        Origin = item_origin_user
    ),
    module_info_get_predicate_table(!.ModuleInfo, PredTable),
    predicate_table_lookup_pred_sym_arity(PredTable,
        may_be_partially_qualified, SymName, Arity, PredIds),
    (
        PredIds = [],
        Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
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
            (
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
            ->
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
            ;
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
            ->
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
            ;
                Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                    words("used in"), decl("finalise"),
                    words("declaration has invalid signature."), nl],
                Msg = simple_msg(Context, [always(Pieces)]),
                Spec = error_spec(severity_error, phase_parse_tree_to_hlds,
                    [Msg]),
                !:Specs = [Spec | !.Specs]
            )
        ;
            TailPredIds = [_ | _],
            Pieces = [words("Error:"), sym_name_and_arity(SymName/Arity),
                words("used in"), decl("finalise"), words("declaration"),
                words("has multiple"), decl("pred"), words("declarations."),
                nl],
            Msg = simple_msg(Context, [always(Pieces)]),
            Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
            !:Specs = [Spec | !.Specs]
        )
    ).

%---------------------------------------------------------------------------%

:- pred add_pass_3_mutable(item_mutable_info::in,
    import_status::in, module_info::in, module_info::out,
    qual_info::in, qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

add_pass_3_mutable(ItemMutable, Status, !ModuleInfo, !QualInfo, !Specs) :-
    % The transformation here is documented in the comments at the
    % beginning of prog_mutable.m.

    DefinedThisModule = status_defined_in_this_module(Status),
    (
        DefinedThisModule = yes,
        add_mutable_aux_pred_defns(ItemMutable, Status, !ModuleInfo,
            !QualInfo, !Specs)
    ;
        DefinedThisModule = no
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.make_hlds_passes.
%---------------------------------------------------------------------------%
