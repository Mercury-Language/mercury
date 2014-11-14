%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: module_qual.m.
% Main authors: stayl, fjh.
%
% Module qualifies types, insts and modes within declaration items.
% The head of all declarations should be module qualified in prog_io.m.
% This module qualifies the bodies of the declarations. Checks for
% undefined types, insts and modes. Uses two passes over the item list,
% one to collect all type, mode and inst ids and a second to do the
% qualification and report errors. If the --warn-interface-imports
% option is set, warns about modules imported in the interface that do
% not need to be in the interface. The modes of lambda expressions are
% qualified in modes.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.module_qual.
:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module recompilation.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % module_qualify_items(Items0, Items, EventSpecMap0, EventSpecMap,
    %   Globals, ModuleName, MaybeFileName, EventSpecFileName, MQ_Info,
    %   UndefTypes, UndefModes, !Specs, !IO):
    %
    % Items is Items0 with all items module qualified as much as possible;
    % likewise for EventSpecMap0 and EventSpecMap.
    %
    % If MaybeFileName is `yes(FileName)', then report undefined types, insts
    % and modes in Items0. MaybeFileName should be `no' when module qualifying
    % the short interface.
    %
    % Errors in EventSpecMap0 will be reported as being for EventSpecFileName.
    %
:- pred module_qualify_items(list(item)::in, list(item)::out,
    event_spec_map::in, event_spec_map::out, globals::in,
    module_name::in, maybe(string)::in, string::in, mq_info::out,
    bool::out, bool::out, list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds to qualify the mode of a lambda expression.
    %
:- pred qualify_lambda_mode_list(list(mer_mode)::in,
    list(mer_mode)::out, prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds.m to qualify the modes in a
    % clause mode annotation.
    %
:- pred qualify_clause_mode_list(list(mer_mode)::in,
    list(mer_mode)::out, prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % This is called from make_hlds to qualify an explicit type qualification.
    %
:- pred qualify_type_qualification(mer_type::in, mer_type::out,
    prog_context::in, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % The type mq_info holds information needed for doing module qualification.
    %
:- type mq_info.

:- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_need_qual_flag(mq_info::in, need_qualifier::out) is det.
:- pred mq_info_get_partial_qualifier_info(mq_info::in,
    partial_qualifier_info::out) is det.
:- pred mq_info_get_recompilation_info(mq_info::in,
    maybe(recompilation_info)::out) is det.

:- pred mq_info_set_need_qual_flag(need_qualifier::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_recompilation_info(maybe(recompilation_info)::in,
    mq_info::in, mq_info::out) is det.

    % The type partial_qualifier_info holds info need for computing which
    % partial quantifiers are visible -- see get_partial_qualifiers/3.
    %
:- type partial_qualifier_info.

    % Suppose we are processing a definition which defines the symbol
    % foo.bar.baz.quux/1. Then we insert the following symbols
    % into the symbol table:
    %   - if the current value of the NeedQual flag at this point
    %       is `may_be_unqualified',
    %       i.e. module `foo.bar.baz' was imported
    %       then we insert the fully unqualified symbol quux/1;
    %   - if module `foo.bar.baz' occurs in the "imported" section,
    %       i.e. if module `foo.bar' was imported,
    %       then we insert the partially qualified symbol baz.quux/1;
    %   - if module `foo.bar' occurs in the "imported" section,
    %       i.e. if module `foo' was imported,
    %       then we insert the partially qualified symbol bar.baz.quux/1;
    %   - we always insert the fully qualified symbol foo.bar.baz.quux/1.
    %
    % The predicate `get_partial_qualifiers' returns all of the
    % partial qualifiers for which we need to insert definitions,
    % i.e. all the ones which are visible. For example,
    % given as input `foo.bar.baz', it returns a list containing
    %   (1) `baz', iff `foo.bar' is imported, and
    %   (2) `bar.baz', iff `foo' is imported.
    % Note that the caller will still need to handle the fully-qualified
    % and fully-unqualified versions separately.
    %
:- pred get_partial_qualifiers(module_name::in, partial_qualifier_info::in,
    list(module_name)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module term.

%-----------------------------------------------------------------------------%

module_qualify_items(Items0, Items, EventSpecMap0, EventSpecMap, Globals,
        ModuleName, MaybeFileName, EventSpecFileName, !:Info, UndefTypes,
        UndefModes, !Specs) :-
    (
        MaybeFileName = yes(_),
        ReportErrors = yes
    ;
        MaybeFileName = no,
        ReportErrors = no
    ),
    init_mq_info(Items0, Globals, ReportErrors, ModuleName, !:Info),
    collect_mq_info(Items0, !Info),
    do_module_qualify_items(Items0, Items, !Info, !Specs),
    map.to_assoc_list(EventSpecMap0, EventSpecList0),
    do_module_qualify_event_specs(EventSpecFileName,
        EventSpecList0, EventSpecList, !Info, !Specs),
    map.from_assoc_list(EventSpecList, EventSpecMap),
    mq_info_get_type_error_flag(!.Info, UndefTypes),
    mq_info_get_mode_error_flag(!.Info, UndefModes),
    (
        % Warn about any unused module imports in the interface.
        % There is a special case involving type class instances that
        % we need to handle here. Consider:
        %
        %   :- module foo.
        %   :- interface.
        %
        %   :- import_module bar.
        %   :- typeclass tc1(T) <= tc2(T).
        %   :- instance tc1(unit).
        %
        % where module bar exports the instance tc2(unit). We must import
        % the module bar in the interface of the module foo in order for
        % the superclass constraint on the instance tc1(unit) to be satisfied.
        % However, at this stage of compilation we do not know that the
        % instance tc2(unit) needs to be visible. (Knowing this would require
        % a more extensive analysis of type classes and instances to be done
        % in this module.)
        %
        % In order to prevent the import of the module bar being erroneously
        % reported as unused we make the conservative assumption that any
        % imported module that exports a type class instance is used in
        % the interface of the importing module, except if the importing
        % module itself exports _no_ type class instances.
        %
        MaybeFileName = yes(FileName),
        mq_info_get_unused_interface_modules(!.Info, UnusedImports0),
        mq_info_get_exported_instances_flag(!.Info, ModuleExportsInstances),
        (
            ModuleExportsInstances = yes,
            mq_info_get_imported_instance_modules(!.Info, InstanceImports),
            set.difference(UnusedImports0, InstanceImports, UnusedImports1)
        ;
            ModuleExportsInstances = no,
            UnusedImports1 = UnusedImports0
        ),
        set.to_sorted_list(UnusedImports1, UnusedImports),
        maybe_warn_unused_interface_imports(ModuleName, FileName,
            UnusedImports, !Specs)
    ;
        MaybeFileName = no
    ).

qualify_lambda_mode_list(Modes0, Modes, Context, !Info, !Specs) :-
    mq_info_set_error_context(mqec_lambda_expr - Context, !Info),
    qualify_mode_list(Modes0, Modes, !Info, !Specs).

qualify_clause_mode_list(Modes0, Modes, Context, !Info, !Specs) :-
    mq_info_set_error_context(mqec_clause_mode_annotation - Context, !Info),
    qualify_mode_list(Modes0, Modes, !Info, !Specs).

qualify_type_qualification(Type0, Type, Context, !Info, !Specs) :-
    mq_info_set_error_context(mqec_type_qual - Context, !Info),
    qualify_type(Type0, Type, !Info, !Specs).

:- type mq_info
    --->    mq_info(
                % Modules which have been imported or used, i.e. the ones
                % for which there was a `:- import_module' or `:- use_module'
                % declaration in this module.
                imported_modules            :: set(module_name),

                % Modules which have been imported or used in the interface.
                interface_visible_modules   :: set(module_name),

                % Sets of all modules, types, insts, modes, and typeclasses
                % visible in this module. Impl_types is the set of all types
                % visible from the implementation of the module.
                modules                     :: module_id_set,
                types                       :: type_id_set,
                impl_types                  :: type_id_set,
                insts                       :: inst_id_set,
                modes                       :: mode_id_set,
                classes                     :: class_id_set,

                % Modules imported in the interface that are not definitely
                % needed in the interface.
                unused_interface_modules    :: set(module_name),

                % Modules from which `:- instance' declarations have
                % been imported.
                imported_instance_modules   :: set(module_name),

                % Does this module export any type class instances?
                exported_instances_flag      :: bool,

                % Import status of the current item.
                import_status               :: mq_import_status,

                % The number of errors found.
                num_errors                  :: int,

                % Are there any undefined types or typeclasses.
                type_error_flag             :: bool,

                % Are there any undefined insts or modes.
                mode_error_flag             :: bool,

                % Do we want to report errors.
                report_error_flag           :: bool,

                % The context of the current item.
                error_context               :: error_context,

                % The name of the current module.
                this_module                 :: module_name,

                % Must uses of the current item be explicitly module qualified.
                need_qual_flag              :: need_qualifier,

                maybe_recompilation_info    :: maybe(recompilation_info)
            ).

:- type partial_qualifier_info
    --->    partial_qualifier_info(module_id_set).

mq_info_get_partial_qualifier_info(MQInfo, QualifierInfo) :-
    mq_info_get_modules(MQInfo, ModuleIdSet),
    QualifierInfo = partial_qualifier_info(ModuleIdSet).

    % We only need to keep track of what is exported and what isn't,
    % so we use a simpler data type here than hlds_pred.import_status.
    %
:- type mq_import_status
    --->    mq_status_exported
    ;       mq_status_local
    ;       mq_status_imported(import_locn)
    ;       mq_status_abstract_imported.

    % Pass over the item list collecting all defined module, type, mode and
    % inst ids, all module synonym definitions, and the names of all
    % modules imported in the interface.
    %
:- pred collect_mq_info(list(item)::in, mq_info::in, mq_info::out) is det.

collect_mq_info([], !Info).
collect_mq_info([Item | Items], !Info) :-
    (
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _SeqNum),
        ModuleDefn = md_transitively_imported
    ->
        % Don't process the transitively imported items (from `.int2' files).
        % They can't be used in the current module.
        true
    ;
        collect_mq_info_item(Item, !Info),
        collect_mq_info(Items, !Info)
    ).

:- pred collect_mq_info_item(item::in, mq_info::in, mq_info::out) is det.

collect_mq_info_item(Item, !Info) :-
    (
        Item = item_module_start(ItemModuleStart),
        ItemModuleStart = item_module_start_info(ModuleName, _, _),
        add_module_defn(ModuleName, !Info)
    ;
        Item = item_module_end(_)
    ;
        Item = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        process_module_defn(ModuleDefn, !Info)
    ;
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(_, SymName, Params, _, _, _, _),
        ( mq_info_get_import_status(!.Info, mq_status_abstract_imported) ->
            % This item is not visible in the current module.
            true
        ;
            list.length(Params, Arity),
            mq_info_get_types(!.Info, Types0),
            mq_info_get_impl_types(!.Info, ImplTypes0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity), Types0, Types),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity),
                ImplTypes0, ImplTypes),
            mq_info_set_types(Types, !Info),
            mq_info_set_impl_types(ImplTypes, !Info)
        )
    ;
        Item = item_inst_defn(ItemInstDefn),
        ItemInstDefn = item_inst_defn_info(_, SymName, Params, _, _, _, _),
        ( mq_info_get_import_status(!.Info, mq_status_abstract_imported) ->
            % This item is not visible in the current module.
            true
        ;
            list.length(Params, Arity),
            mq_info_get_insts(!.Info, Insts0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity), Insts0, Insts),
            mq_info_set_insts(Insts, !Info)
        )
    ;
        Item = item_mode_defn(ItemModeDefn),
        ItemModeDefn = item_mode_defn_info(_, SymName, Params, _, _, _, _),
        ( mq_info_get_import_status(!.Info, mq_status_abstract_imported) ->
            % This item is not visible in the current module.
            true
        ;
            list.length(Params, Arity),
            mq_info_get_modes(!.Info, Modes0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity), Modes0, Modes),
            mq_info_set_modes(Modes, !Info)
        )
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(_PromiseType, Goal, _ProgVarSet,
            _UnivVars, _Context, _SeqNum),
        process_assert(Goal, SymNames, Success),
        (
            Success = yes,
            list.foldl(collect_mq_info_qualified_symname, SymNames, !Info)
        ;
            % Any unqualified symbol in the promise might come from *any* of
            % the imported modules. There's no way for us to tell which ones.
            % So we conservatively assume that it uses all of them.
            Success = no,
            set.init(UnusedInterfaceModules),
            mq_info_set_unused_interface_modules(UnusedInterfaceModules, !Info)
        )
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(_, _, SymName, Params, _, _, _, _),
        ( mq_info_get_import_status(!.Info, mq_status_abstract_imported) ->
            % This item is not visible in the current module.
            true
        ;
            list.length(Params, Arity),
            mq_info_get_classes(!.Info, Classes0),
            mq_info_get_need_qual_flag(!.Info, NeedQualifier),
            id_set_insert(NeedQualifier, mq_id(SymName, Arity),
                Classes0, Classes),
            mq_info_set_classes(Classes, !Info)
        )
    ;
          Item = item_instance(ItemInstance),
          ( mq_info_get_import_status(!.Info, mq_status_imported(_)) ->
              InstanceModule = ItemInstance ^ ci_module_containing_instance,
              mq_info_get_imported_instance_modules(!.Info,
                ImportedInstanceModules0),
              set.insert(InstanceModule,
                ImportedInstanceModules0, ImportedInstanceModules),
              mq_info_set_imported_instance_modules(ImportedInstanceModules,
                !Info)
          ;
              true
          )
    ;
        ( Item = item_clause(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_pragma(_)
        ; Item = item_initialise(_)
        ; Item = item_finalise(_)
        ; Item = item_mutable(_)
        ; Item = item_nothing(_)
        )
        % Do nothing.
    ).

:- pred collect_mq_info_qualified_symname(sym_name::in,
    mq_info::in, mq_info::out) is det.

collect_mq_info_qualified_symname(SymName, !Info) :-
    (
        SymName = qualified(ModuleName, _),
        mq_info_set_module_used(ModuleName, !Info)
    ;
        SymName = unqualified(_),
        unexpected($module, $pred, "unqualified")
    ).

    % process_module_defn:
    %
    % - Update the import status.
    %
    % - For sub-module definitions (whether nested or separate,
    %   i.e. either `:- module foo.' or `:- include_module foo.'),
    %   add the module id to the module_id_set.
    %
    % - For import declarations (`:- import_module' or `:- use_module'),
    %   if we're currently in the interface section, then add the
    %   imported modules to the unused_interface_modules list.
    %
:- pred process_module_defn(module_defn::in, mq_info::in, mq_info::out) is det.

process_module_defn(md_include_module(ModuleNameList), !Info) :-
    list.foldl(add_module_defn, ModuleNameList, !Info).
process_module_defn(md_interface, !Info) :-
    mq_info_set_import_status(mq_status_exported, !Info).
process_module_defn(md_implementation_but_exported_to_submodules, !Info) :-
    mq_info_set_import_status(mq_status_local, !Info).
process_module_defn(md_implementation, !Info) :-
    mq_info_set_import_status(mq_status_local, !Info).
process_module_defn(md_imported(Locn), !Info) :-
    mq_info_set_import_status(mq_status_imported(Locn), !Info),
    mq_info_set_need_qual_flag(may_be_unqualified, !Info).
process_module_defn(md_used(Locn), !Info) :-
    mq_info_set_import_status(mq_status_imported(Locn), !Info),
    mq_info_set_need_qual_flag(must_be_qualified, !Info).
process_module_defn(md_opt_imported, !Info) :-
    mq_info_set_import_status(mq_status_imported(import_locn_implementation),
        !Info),
    mq_info_set_need_qual_flag(must_be_qualified, !Info).
process_module_defn(md_abstract_imported, !Info) :-
    mq_info_set_import_status(mq_status_abstract_imported, !Info),
    mq_info_set_need_qual_flag(must_be_qualified, !Info).
process_module_defn(md_transitively_imported, !Info) :-
    unexpected($module, $pred, "transitively_imported item").
process_module_defn(md_external(_, _), !Info).
process_module_defn(md_export(_), !Info).
process_module_defn(md_import(Imports), !Info) :-
    add_imports(Imports, !Info).
process_module_defn(md_use(Imports), !Info) :-
    add_imports(Imports, !Info).
process_module_defn(md_version_numbers(_, _), !Info).

:- pred add_module_defn(module_name::in, mq_info::in, mq_info::out) is det.

add_module_defn(ModuleName, !Info) :-
    mq_info_get_modules(!.Info, Modules0),
    mq_info_get_need_qual_flag(!.Info, NeedQualifier),
    Arity = 0,
    id_set_insert(NeedQualifier, mq_id(ModuleName, Arity), Modules0, Modules),
    mq_info_set_modules(Modules, !Info).

:- pred add_imports(list(module_specifier)::in, mq_info::in, mq_info::out)
    is det.

add_imports(Imports, !Info) :-
    mq_info_get_import_status(!.Info, Status),

    % Modules imported from the the proper private interface of ancestors of
    % the current module are treated as if they were directly imported
    % by the current module.
    (
        ( Status = mq_status_local
        ; Status = mq_status_exported
        ; Status = mq_status_imported(
            import_locn_ancestor_private_interface_proper)
        )
    ->
        mq_info_get_imported_modules(!.Info, Modules0),
        set.insert_list(Imports, Modules0, Modules),
        mq_info_set_imported_modules(Modules, !Info)
    ;
        true
    ),

    % We check that all modules imported in the interface are
    % used in the interface.
    (
        Status = mq_status_exported
    ->
        mq_info_get_unused_interface_modules(!.Info, UnusedIntModules0),
        set.insert_list(Imports, UnusedIntModules0, UnusedIntModules),
        mq_info_set_unused_interface_modules(UnusedIntModules, !Info)
    ;
        true
    ),

    % Only modules imported in the interface or in the private
    % interface of ancestor modules may be used in the interface.
    (
        ( Status = mq_status_exported
        ; Status = mq_status_imported(
            import_locn_ancestor_private_interface_proper)
        )
    ->
        mq_info_get_interface_visible_modules(!.Info, IntModules0),
        set.insert_list(Imports, IntModules0, IntModules),
        mq_info_set_interface_visible_modules(IntModules, !Info)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

    % process_assert(G, SNs, B)
    %
    % Scan the goal, G, building the list of qualified symbols, SNs.
    % If there exists a single unqualified symbol in G, the bool, B,
    % will be set to no.
    %
:- pred process_assert(goal::in, list(sym_name)::out, bool::out) is det.

process_assert(Goal, Symbols, Success) :-
    % AAA Some more stuff to do accumulator introduction on, it
    % would be better to rewrite using maybes and then to declare
    % the maybe_and predicate to be associative.
    % NB. accumulator introduction doesn't work on this case yet.
    %
    Goal = GoalExpr - _Context,
    (
        ( GoalExpr = conj_expr(GA, GB)
        ; GoalExpr = par_conj_expr(GA, GB)
        ; GoalExpr = disj_expr(GA, GB)
        ; GoalExpr = implies_expr(GA, GB)
        ; GoalExpr = equivalent_expr(GA, GB)
        ),
        process_assert(GA, SymbolsA, SuccessA),
        process_assert(GB, SymbolsB, SuccessB),
        Symbols = SymbolsA ++ SymbolsB,
        bool.and(SuccessA, SuccessB, Success)
    ;
        ( GoalExpr = true_expr
        ; GoalExpr = fail_expr
        ),
        Symbols = [],
        Success = yes
    ;
        ( GoalExpr = not_expr(G)
        ; GoalExpr = some_expr(_, G)
        ; GoalExpr = some_state_vars_expr(_, G)
        ; GoalExpr = all_expr(_, G)
        ; GoalExpr = all_state_vars_expr(_, G)
        ; GoalExpr = promise_purity_expr(_, G)
        ; GoalExpr = promise_equivalent_solutions_expr(_, _, _, _, G)
        ; GoalExpr = promise_equivalent_solution_sets_expr(_, _, _, _, G)
        ; GoalExpr = promise_equivalent_solution_arbitrary_expr(_, _, _, _, G)
        ; GoalExpr = require_detism_expr(_, G)
        ; GoalExpr = require_complete_switch_expr(_, G)
        ; GoalExpr = require_switch_arms_detism_expr(_, _, G)
        ; GoalExpr = trace_expr(_, _, _, _, G)
        ),
        process_assert(G, Symbols, Success)
    ;
        GoalExpr = try_expr(_, SubGoal, Then, MaybeElse, Catches,
            MaybeCatchAny),
        process_assert(SubGoal, SymbolsGoal, SuccessGoal),
        process_assert(Then, SymbolsThen, SuccessThen),
        maybe_process_assert(MaybeElse, SymbolsElse, SuccessElse),
        list.map2(process_assert_catch, Catches,
            SymbolsCatches, SuccessCatches),
        (
            MaybeCatchAny = yes(catch_any_expr(_, CatchAnyGoal)),
            process_assert(CatchAnyGoal, SymbolsCatchAny, SuccessCatchAny)
        ;
            MaybeCatchAny = no,
            SymbolsCatchAny = [],
            SuccessCatchAny = no
        ),
        SymbolsLists = [SymbolsGoal, SymbolsThen, SymbolsElse, SymbolsCatchAny
            | SymbolsCatches],
        list.condense(SymbolsLists, Symbols),
        SuccessLists = [SuccessGoal, SuccessThen, SuccessElse, SuccessCatchAny
            | SuccessCatches],
        bool.and_list(SuccessLists, Success)
    ;
        GoalExpr = atomic_expr(_, _, _, MainGoal, OrElseGoals),
        process_assert(MainGoal, SymbolsMainGoal, SuccessMainGoal),
        process_assert_list(OrElseGoals,
            SymbolsOrElseGoals, SuccessOrElseGoals),
        Symbols = SymbolsMainGoal ++ SymbolsOrElseGoals,
        bool.and(SuccessMainGoal, SuccessOrElseGoals, Success)
    ;
        GoalExpr = if_then_else_expr(_, _, GA, GB, GC),
        process_assert(GA, SymbolsA, SuccessA),
        process_assert(GB, SymbolsB, SuccessB),
        process_assert(GC, SymbolsC, SuccessC),
        Symbols = SymbolsA ++ SymbolsB ++ SymbolsC,
        bool.and(SuccessA, SuccessB, Success0),
        bool.and(Success0, SuccessC, Success)
    ;
        GoalExpr = event_expr(_Name, Args0),
        list.map(term.coerce, Args0, Args),
        ( term_qualified_symbols_list(Args, SymbolsPrime) ->
            Symbols = SymbolsPrime,
            Success = yes
        ;
            Symbols = [],
            Success = no
        )
    ;
        GoalExpr = call_expr(SymName, Args0, _Purity),
        (
            SymName = qualified(_, _),
            list.map(term.coerce, Args0, Args),
            ( term_qualified_symbols_list(Args, Symbols0) ->
                Symbols = [SymName | Symbols0],
                Success = yes
            ;
                Symbols = [],
                Success = no
            )
        ;
            SymName = unqualified(_),
            Symbols = [],
            Success = no
        )
    ;
        GoalExpr = unify_expr(LHS0, RHS0, _Purity),
        term.coerce(LHS0, LHS),
        term.coerce(RHS0, RHS),
        (
            term_qualified_symbols(LHS, SymbolsL),
            term_qualified_symbols(RHS, SymbolsR)
        ->
            list.append(SymbolsL, SymbolsR, Symbols),
            Success = yes
        ;
            Symbols = [],
            Success = no
        )
    ).

:- pred maybe_process_assert(maybe(goal)::in, list(sym_name)::out, bool::out)
    is det.

maybe_process_assert(no, [], yes).
maybe_process_assert(yes(Goal), Symbols, Success) :-
    process_assert(Goal, Symbols, Success).

:- pred process_assert_catch(catch_expr::in, list(sym_name)::out, bool::out)
    is det.

process_assert_catch(catch_expr(Pattern0, Goal), Symbols, Success) :-
    term.coerce(Pattern0, Pattern),
    (
        term_qualified_symbols(Pattern, SymbolsPattern),
        process_assert(Goal, SymbolsGoal, yes)
    ->
        list.append(SymbolsPattern, SymbolsGoal, Symbols),
        Success = yes
    ;
        Symbols = [],
        Success = no
    ).

    % process_assert(G, SNs, B)
    %
    % Performs process_assert on a list of goals.
    %
:- pred process_assert_list(list(goal)::in, list(sym_name)::out,
        bool::out) is det.

process_assert_list(ExprList, Symbols, Success) :-
    (
        ExprList = [],
        Symbols = [],
        Success = yes
    ;
        ExprList = [Expr | Rest],
        process_assert(Expr, SymbolsE, SuccessE),
        process_assert_list(Rest, SymbolsR, SuccessR),
        list.append(SymbolsE, SymbolsR, Symbols),
        bool.and(SuccessE, SuccessR, Success)
    ).

    % term_qualified_symbols(T, S)
    %
    % Given a term, T, return the list of all the sym_names, S, in the term.
    % The predicate fails if any sub-term of T is unqualified.
    %
:- pred term_qualified_symbols(term::in, list(sym_name)::out) is semidet.

term_qualified_symbols(Term, Symbols) :-
    ( try_parse_sym_name_and_args(Term, SymName, Args) ->
        SymName = qualified(_, _),
        term_qualified_symbols_list(Args, Symbols0),
        Symbols = [SymName | Symbols0]
    ;
        Symbols = []
    ).

:- pred term_qualified_symbols_list(list(term)::in, list(sym_name)::out)
    is semidet.

    % Yeah one more place where accumulators will be introduced!
term_qualified_symbols_list([], []).
term_qualified_symbols_list([Term | Terms], Symbols) :-
    term_qualified_symbols(Term, TermSymbols),
    term_qualified_symbols_list(Terms, Symbols0),
    list.append(Symbols0, TermSymbols, Symbols).

%-----------------------------------------------------------------------------%

    % Iterate over the item list module qualifying all declarations.
    % Stop when the :- imported or :- opt_imported pseudo-declaration
    % is reached, since imported declarations should already be
    % module qualified.
    %
:- pred do_module_qualify_items(list(item)::in, list(item)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_items([], [], !Info, !Specs).
do_module_qualify_items([Item0 | Items0], [Item | Items], !Info, !Specs) :-
    module_qualify_item(Item0, Item, Continue, !Info, !Specs),
    (
        Continue = yes,
        do_module_qualify_items(Items0, Items, !Info, !Specs)
    ;
        Continue = no,
        Items = Items0
    ).

    % Call predicates to qualify a single item.
    %
:- pred module_qualify_item(item::in, item::out, bool::out,
    mq_info::in, mq_info::out, list(error_spec)::in, list(error_spec)::out)
    is det.

module_qualify_item(Item0, Item, Continue, !Info, !Specs) :-
    (
        ( Item0 = item_module_start(_)
        ; Item0 = item_module_end(_)
        ; Item0 = item_clause(_)
        ; Item0 = item_initialise(_)
        ; Item0 = item_finalise(_)
        ; Item0 = item_promise(_)
        ; Item0 = item_nothing(_)
        ),
        Item = Item0,
        Continue = yes
    ;
        Item0 = item_module_defn(ItemModuleDefn),
        ItemModuleDefn = item_module_defn_info(ModuleDefn, _, _),
        update_import_status(ModuleDefn, !Info, Continue),
        Item = Item0
    ;
        Item0 = item_type_defn(ItemTypeDefn0),
        ItemTypeDefn0 = item_type_defn_info(TVarSet, SymName, Params,
            TypeDefn0, C, Context, SeqNum),
        list.length(Params, Arity),
        mq_info_set_error_context(mqec_type(mq_id(SymName, Arity)) - Context,
            !Info),
        qualify_type_defn(TypeDefn0, TypeDefn, !Info, !Specs),
        ItemTypeDefn = item_type_defn_info(TVarSet, SymName, Params,
            TypeDefn, C, Context, SeqNum),
        Item = item_type_defn(ItemTypeDefn),
        Continue = yes
    ;
        Item0 = item_inst_defn(ItemInstDefn0),
        ItemInstDefn0 = item_inst_defn_info(A, SymName, Params, InstDefn0, C,
            Context, SeqNum),
        list.length(Params, Arity),
        mq_info_set_error_context(mqec_inst(mq_id(SymName, Arity)) - Context,
            !Info),
        qualify_inst_defn(InstDefn0, InstDefn, !Info, !Specs),
        ItemInstDefn = item_inst_defn_info(A, SymName, Params, InstDefn, C,
            Context, SeqNum),
        Item = item_inst_defn(ItemInstDefn),
        Continue = yes
    ;
        Item0 = item_mode_defn(ItemModeDefn0),
        ItemModeDefn0 = item_mode_defn_info(A, SymName, Params, ModeDefn0, C,
            Context, SeqNum),
        list.length(Params, Arity),
        mq_info_set_error_context(mqec_mode(mq_id(SymName, Arity)) - Context,
            !Info),
        qualify_mode_defn(ModeDefn0, ModeDefn, !Info, !Specs),
        ItemModeDefn = item_mode_defn_info(A, SymName, Params, ModeDefn, C,
            Context, SeqNum),
        Item = item_mode_defn(ItemModeDefn),
        Continue = yes
    ;
        Item0 = item_pred_decl(ItemPredDecl0),
        ItemPredDecl0 = item_pred_decl_info(Origin, A, IVs, B, PredOrFunc,
            SymName, TypesAndModes0, WithType0, WithInst0, C, D, E,
            Constraints0, Context, SeqNum),
        list.length(TypesAndModes0, Arity),
        mq_info_set_error_context(
            mqec_pred_or_func(PredOrFunc, mq_id(SymName, Arity)) - Context,
            !Info),
        qualify_types_and_modes(TypesAndModes0, TypesAndModes, !Info, !Specs),
        qualify_prog_constraints(Constraints0, Constraints, !Info, !Specs),
        map_fold2_maybe(qualify_type, WithType0, WithType, !Info, !Specs),
        map_fold2_maybe(qualify_inst, WithInst0, WithInst, !Info, !Specs),
        ItemPredDecl = item_pred_decl_info(Origin, A, IVs, B, PredOrFunc,
            SymName, TypesAndModes, WithType, WithInst, C, D, E,
            Constraints, Context, SeqNum),
        Item = item_pred_decl(ItemPredDecl),
        Continue = yes
    ;
        Item0 = item_mode_decl(ItemModeDecl0),
        ItemModeDecl0 = item_mode_decl_info(A, PredOrFunc, SymName, Modes0,
            WithInst0, C, D, Context, SeqNum),
        list.length(Modes0, Arity),
        mq_info_set_error_context(
            mqec_pred_or_func_mode(PredOrFunc, mq_id(SymName, Arity))
                - Context,
            !Info),
        qualify_mode_list(Modes0, Modes, !Info, !Specs),
        map_fold2_maybe(qualify_inst, WithInst0, WithInst, !Info, !Specs),
        ItemModeDecl = item_mode_decl_info(A, PredOrFunc, SymName, Modes,
            WithInst, C, D, Context, SeqNum),
        Item = item_mode_decl(ItemModeDecl),
        Continue = yes
    ;
        Item0 = item_pragma(ItemPragma0),
        ItemPragma0 = item_pragma_info(Origin, Pragma0, Context, SeqNum),
        mq_info_set_error_context(mqec_pragma(Pragma0) - Context, !Info),
        qualify_pragma(Pragma0, Pragma, !Info, !Specs),
        ItemPragma = item_pragma_info(Origin, Pragma, Context, SeqNum),
        Item = item_pragma(ItemPragma),
        Continue = yes
    ;
        Item0 = item_typeclass(ItemTypeClass0),
        ItemTypeClass0 = item_typeclass_info(Constraints0, FunDeps,
            Name, Vars, Interface0, VarSet, Context, SeqNum),
        list.length(Vars, Arity),
        mq_info_set_error_context(mqec_class(mq_id(Name, Arity)) - Context,
            !Info),
        qualify_prog_constraint_list(Constraints0, Constraints, !Info, !Specs),
        (
            Interface0 = class_interface_abstract,
            Interface = class_interface_abstract
        ;
            Interface0 = class_interface_concrete(Methods0),
            qualify_class_interface(Methods0, Methods, !Info, !Specs),
            Interface = class_interface_concrete(Methods)
        ),
        ItemTypeClass = item_typeclass_info(Constraints, FunDeps,
            Name, Vars, Interface, VarSet, Context, SeqNum),
        Item = item_typeclass(ItemTypeClass),
        Continue = yes
    ;
        Item0 = item_instance(ItemInstance0),
        ItemInstance0 = item_instance_info(Constraints0, Name0,
            Types0, OriginalTypes0, Body0, VarSet, ModName, Context, SeqNum),
        list.length(Types0, Arity),
        Id = mq_id(Name0, Arity),
        mq_info_set_error_context(mqec_instance(Id) - Context, !Info),

        ( mq_info_get_import_status(!.Info, mq_status_exported) ->
            mq_info_set_exported_instances_flag(yes, !Info)
        ;
            true
        ),

        % We don't qualify the implementation yet, since that requires
        % us to resolve overloading.
        qualify_prog_constraint_list(Constraints0, Constraints, !Info, !Specs),
        qualify_class_name(Id, mq_id(Name, _), !Info, !Specs),
        % XXX We don't want to keep the errors from the expansion of both
        % forms of the instance types, since printing two error messages about
        % one instance definition that make apparently contradictory
        % assumptions about whether the instance types are equiv-type-expanded
        % or not wouldd be confusing. However, I (zs) cannot think of any
        % compelling reason right now for preferring the error messages
        % from one version of the types over the other.
        qualify_type_list(Types0, Types, !Info, !Specs),
        qualify_type_list(OriginalTypes0, OriginalTypes, !Info, !.Specs, _),
        qualify_instance_body(Name, Body0, Body),
        ItemInstance = item_instance_info(Constraints, Name,
            Types, OriginalTypes, Body, VarSet, ModName, Context, SeqNum),
        Item = item_instance(ItemInstance),
        Continue = yes
    ;
        Item0 = item_mutable(ItemMutable0),
        do_module_qualify_mutable(ItemMutable0, ItemMutable, !Info, !Specs),
        Item = item_mutable(ItemMutable),
        Continue = yes
    ).

:- pred do_module_qualify_mutable(
    item_mutable_info::in, item_mutable_info::out,
    mq_info::in, mq_info::out, list(error_spec)::in, list(error_spec)::out)
    is det.

do_module_qualify_mutable(ItemMutable0, ItemMutable, !Info, !Specs) :-
    ItemMutable0 = item_mutable_info(Name, Type0, InitTerm, Inst0,
        Attrs, Varset, Context, SeqNum),
    mq_info_set_error_context(mqec_mutable(Name) - Context, !Info),
    qualify_type(Type0, Type, !Info, !Specs),
    qualify_inst(Inst0, Inst, !Info, !Specs),
    ItemMutable = item_mutable_info(Name, Type, InitTerm, Inst,
        Attrs, Varset, Context, SeqNum).

:- pred do_module_qualify_event_specs(string::in,
    assoc_list(string, event_spec)::in, assoc_list(string, event_spec)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_event_specs(_, [], [], !Info, !Specs).
do_module_qualify_event_specs(FileName,
        [Name - Spec0 | NameSpecs0], [Name - Spec | NameSpecs],
        !Info, !Specs) :-
    do_module_qualify_event_spec(FileName, Spec0, Spec, !Info, !Specs),
    do_module_qualify_event_specs(FileName, NameSpecs0, NameSpecs,
        !Info, !Specs).

:- pred do_module_qualify_event_spec(string::in,
    event_spec::in, event_spec::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_event_spec(FileName, EventSpec0, EventSpec, !Info, !Specs) :-
    EventSpec0 = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs0, SynthAttrNumOrder),
    list.map_foldl2(
        do_module_qualify_event_attr(EventName, FileName, EventLineNumber),
        Attrs0, Attrs, !Info, !Specs),
    EventSpec = event_spec(EventNumber, EventName, EventLineNumber,
        Attrs, SynthAttrNumOrder).

:- pred do_module_qualify_event_attr(string::in, string::in, int::in,
    event_attribute::in, event_attribute::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

do_module_qualify_event_attr(EventName, FileName, LineNumber, Attr0, Attr,
        !Info, !Specs) :-
    Attr0 = event_attribute(AttrNum, AttrName, AttrType0, AttrMode0,
        MaybeSynthCall),
    MQErrorContext = mqec_event_spec_attr(EventName, AttrName),
    Context = context(FileName, LineNumber),
    mq_info_set_error_context(MQErrorContext - Context, !Info),
    qualify_type(AttrType0, AttrType, !Info, !Specs),
    qualify_mode(AttrMode0, AttrMode, !Info, !Specs),
    Attr = event_attribute(AttrNum, AttrName, AttrType, AttrMode,
        MaybeSynthCall).

:- pred update_import_status(module_defn::in, mq_info::in, mq_info::out,
    bool::out) is det.

update_import_status(md_opt_imported, !Info, no).
update_import_status(md_abstract_imported, !Info, yes) :-
    mq_info_set_import_status(mq_status_abstract_imported, !Info).
update_import_status(md_transitively_imported, !Info, no).
update_import_status(md_interface, !Info, yes) :-
    mq_info_set_import_status(mq_status_exported, !Info).
update_import_status(md_implementation, !Info, yes) :-
    mq_info_set_import_status(mq_status_local, !Info).
update_import_status(md_implementation_but_exported_to_submodules, !Info, yes)
        :-
    mq_info_set_import_status(mq_status_local, !Info).
update_import_status(md_imported(_), !Info, no).
update_import_status(md_used(_), !Info, no).
update_import_status(md_external(_, _), !Info, yes).
update_import_status(md_export(_), !Info, yes).
update_import_status(md_import(_), !Info, yes).
update_import_status(md_use(_), !Info, yes).
update_import_status(md_version_numbers(_, _), !Info, yes).
update_import_status(md_include_module(_), !Info, yes) :-
    % The sub-module might make use of *any* of the imported modules.
    % There's no way for us to tell which ones.
    % So we conservatively assume that it uses all of them.
    set.init(UnusedInterfaceModules),
    mq_info_set_unused_interface_modules(UnusedInterfaceModules, !Info).

    % Qualify the constructors or other types in a type definition.
    %
:- pred qualify_type_defn(type_defn::in, type_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_defn(
        parse_tree_du_type(Ctors0, MaybeUserEqComp0, MaybeDirectArgCtors0),
        parse_tree_du_type(Ctors, MaybeUserEqComp, MaybeDirectArgCtors),
        !Info, !Specs) :-
    qualify_constructors(Ctors0, Ctors, !Info, !Specs),
    % User-defined equality pred names will be converted into predicate calls
    % and then module-qualified after type analysis (during mode analysis).
    % That way they get full type overloading resolution, etc. Thus we don't
    % module-qualify them here.
    MaybeUserEqComp = MaybeUserEqComp0,
    MaybeDirectArgCtors = MaybeDirectArgCtors0.
qualify_type_defn(parse_tree_eqv_type(Type0), parse_tree_eqv_type(Type),
        !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs).
qualify_type_defn(parse_tree_abstract_type(_) @ Defn, Defn, !Info, !Specs).
qualify_type_defn(parse_tree_foreign_type(_, _, _) @ Defn, Defn, !Info,
        !Specs).
qualify_type_defn(parse_tree_solver_type(SolverTypeDetails0, MaybeUserEqComp),
        parse_tree_solver_type(SolverTypeDetails, MaybeUserEqComp),
        !Info, !Specs) :-
    SolverTypeDetails0 = solver_type_details(RepnType0, InitPred,
        GroundInst0, AnyInst0, MutableItems0),
    qualify_type(RepnType0, RepnType, !Info, !Specs),
    qualify_inst(GroundInst0, GroundInst, !Info, !Specs),
    qualify_inst(AnyInst0, AnyInst, !Info, !Specs),
    qualify_constraint_stores(MutableItems0, MutableItems, !Info, !Specs),
    SolverTypeDetails  = solver_type_details(RepnType, InitPred,
        GroundInst, AnyInst, MutableItems).

:- pred qualify_constraint_stores(
    list(item_mutable_info)::in, list(item_mutable_info)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constraint_stores([], [], !Info, !Specs).
qualify_constraint_stores([Mutable0 | Mutables0], [Mutable | Mutables],
        !Info, !Specs) :-
    do_module_qualify_mutable(Mutable0, Mutable, !Info, !Specs),
    qualify_constraint_stores(Mutables0, Mutables, !Info, !Specs).

:- pred qualify_constructors(list(constructor)::in, list(constructor)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructors([], [], !Info, !Specs).
qualify_constructors([Ctor0 | Ctors0], [Ctor | Ctors], !Info, !Specs) :-
    Ctor0 = ctor(ExistQVars, Constraints0, SymName, Args0, Ctxt),
    qualify_constructor_arg_list(Args0, Args, !Info, !Specs),
    qualify_constructors(Ctors0, Ctors, !Info, !Specs),
    qualify_prog_constraint_list(Constraints0, Constraints, !Info, !Specs),
    Ctor  = ctor(ExistQVars, Constraints, SymName, Args, Ctxt).

    % Qualify the inst parameters of an inst definition.
    %
:- pred qualify_inst_defn(inst_defn::in, inst_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_defn(eqv_inst(Inst0), eqv_inst(Inst), !Info, !Specs) :-
    qualify_inst(Inst0, Inst, !Info, !Specs).
qualify_inst_defn(abstract_inst, abstract_inst, !Info, !Specs).

    % Qualify the mode parameter of an equivalence mode definition.
    %
:- pred qualify_mode_defn(mode_defn::in, mode_defn::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode_defn(eqv_mode(Mode0), eqv_mode(Mode), !Info, !Specs) :-
    qualify_mode(Mode0, Mode, !Info, !Specs).

    % Qualify a list of items of the form Type::Mode, as in a
    % predicate declaration.
    %
:- pred qualify_types_and_modes(list(type_and_mode)::in,
    list(type_and_mode)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_types_and_modes([], [], !Info, !Specs).
qualify_types_and_modes([TypeAndMode0 | TypesAndModes0],
        [TypeAndMode | TypesAndModes], !Info, !Specs) :-
    qualify_type_and_mode(TypeAndMode0, TypeAndMode, !Info, !Specs),
    qualify_types_and_modes(TypesAndModes0, TypesAndModes, !Info, !Specs).

:- pred qualify_type_and_mode(type_and_mode::in, type_and_mode::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_and_mode(type_only(Type0), type_only(Type), !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs).

qualify_type_and_mode(type_and_mode(Type0, Mode0), type_and_mode(Type, Mode),
        !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs),
    qualify_mode(Mode0, Mode, !Info, !Specs).

:- pred qualify_mode_list(list(mer_mode)::in, list(mer_mode)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode_list([], [], !Info, !Specs).
qualify_mode_list([Mode0 | Modes0], [Mode | Modes], !Info, !Specs) :-
    qualify_mode(Mode0, Mode, !Info, !Specs),
    qualify_mode_list(Modes0, Modes, !Info, !Specs).

:- pred qualify_mode(mer_mode::in, mer_mode::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_mode((InstA0 -> InstB0), (InstA -> InstB), !Info, !Specs) :-
    qualify_inst(InstA0, InstA, !Info, !Specs),
    qualify_inst(InstB0, InstB, !Info, !Specs).

qualify_mode(user_defined_mode(SymName0, Insts0),
        user_defined_mode(SymName, Insts), !Info, !Specs) :-
    qualify_inst_list(Insts0, Insts, !Info, !Specs),
    list.length(Insts, Arity),
    mq_info_get_modes(!.Info, Modes),
    find_unique_match(mq_id(SymName0, Arity), mq_id(SymName, _),
        Modes, mode_id, !Info, !Specs).

:- pred qualify_inst_list(list(mer_inst)::in, list(mer_inst)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_list([], [], !Info, !Specs).
qualify_inst_list([Inst0 | Insts0], [Inst | Insts], !Info, !Specs) :-
    qualify_inst(Inst0, Inst, !Info, !Specs),
    qualify_inst_list(Insts0, Insts, !Info, !Specs).

    % Qualify a single inst.
    %
:- pred qualify_inst(mer_inst::in, mer_inst::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst(Inst0, Inst, !Info, !Specs) :-
    (
        Inst0 = any(Uniq, HOInstInfo0),
        qualify_ho_inst_info(HOInstInfo0, HOInstInfo, !Info, !Specs),
        Inst = any(Uniq, HOInstInfo)
    ;
        ( Inst0 = free
        ; Inst0 = not_reached
        ; Inst0 = inst_var(_)
        ),
        Inst = Inst0
    ;
        Inst0 = free(_),
        unexpected($module, $pred, "compiler generated inst not expected")
    ;
        Inst0 = bound(Uniq, InstResults0, BoundInsts0),
        (
            ( InstResults0 = inst_test_results_fgtc
            ; InstResults0 = inst_test_no_results
            )
        ;
            InstResults0 = inst_test_results(_, _, _, _),
            unexpected($module, $pred, "compiler generated inst not expected")
        ),
        qualify_bound_inst_list(BoundInsts0, BoundInsts, !Info, !Specs),
        Inst = bound(Uniq, InstResults0, BoundInsts)
    ;
        Inst0 = ground(Uniq, HOInstInfo0),
        qualify_ho_inst_info(HOInstInfo0, HOInstInfo, !Info, !Specs),
        Inst = ground(Uniq, HOInstInfo)
    ;
        Inst0 = constrained_inst_vars(Vars, SubInst0),
        qualify_inst(SubInst0, SubInst, !Info, !Specs),
        Inst = constrained_inst_vars(Vars, SubInst)
    ;
        Inst0 = defined_inst(InstName0),
        qualify_inst_name(InstName0, InstName, !Info, !Specs),
        Inst = defined_inst(InstName)
    ;
        Inst0 = abstract_inst(Name, Args0),
        qualify_inst_list(Args0, Args, !Info, !Specs),
        Inst = abstract_inst(Name, Args)
    ).

:- pred qualify_ho_inst_info(ho_inst_info::in, ho_inst_info::out,
    mq_info::in, mq_info::out, list(error_spec)::in, list(error_spec)::out)
    is det.

qualify_ho_inst_info(HOInstInfo0, HOInstInfo, !Info, !Specs) :-
    (
        HOInstInfo0 = higher_order(pred_inst_info(A, Modes0, MaybeArgRegs,
            Det)),
        qualify_mode_list(Modes0, Modes, !Info, !Specs),
        HOInstInfo = higher_order(pred_inst_info(A, Modes, MaybeArgRegs, Det))
    ;
        HOInstInfo0 = none,
        HOInstInfo = none
    ).

    % Find the unique inst_id that matches this inst, and qualify
    % the argument insts.
    %
:- pred qualify_inst_name(inst_name::in, inst_name::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_inst_name(user_inst(SymName0, Insts0), user_inst(SymName, Insts),
        !Info, !Specs) :-
    qualify_inst_list(Insts0, Insts, !Info, !Specs),
    (
        % Check for a variable inst constructor.
        SymName0 = unqualified("")
    ->
        mq_info_get_error_context(!.Info, ErrorContext),
        report_invalid_user_inst(SymName0, Insts, ErrorContext, !Specs),
        mq_info_set_error_flag(inst_id, !Info),
        SymName = SymName0
    ;
        list.length(Insts0, Arity),
        mq_info_get_insts(!.Info, InstIds),
        find_unique_match(mq_id(SymName0, Arity), mq_id(SymName, _),
            InstIds, inst_id, !Info, !Specs)
    ).
qualify_inst_name(merge_inst(_, _), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(unify_inst(_, _, _, _), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(ground_inst(_, _, _, _), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(any_inst(_, _, _, _), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(shared_inst(_), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(mostly_uniq_inst(_), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(typed_ground(_, _), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").
qualify_inst_name(typed_inst(_, _), _, !Info, !Specs) :-
    unexpected($module, $pred, "compiler generated inst unexpected").

    % Qualify an inst of the form bound(functor(...)).
    %
:- pred qualify_bound_inst_list(list(bound_inst)::in, list(bound_inst)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_bound_inst_list([], [], !Info, !Specs).
qualify_bound_inst_list([bound_functor(ConsId, Insts0) | BoundInsts0],
         [bound_functor(ConsId, Insts) | BoundInsts], !Info, !Specs) :-
    (
        ConsId = cons(Name, Arity, _),
        Id = item_name(Name, Arity),
        update_recompilation_info(
            recompilation.record_used_item(functor_item, Id, Id), !Info)
    ;
        ( ConsId = tuple_cons(_)
        ; ConsId = closure_cons(_, _)
        ; ConsId = int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = ground_term_const(_, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        )
    ),
    qualify_inst_list(Insts0, Insts, !Info, !Specs),
    qualify_bound_inst_list(BoundInsts0, BoundInsts, !Info, !Specs).

:- pred qualify_constructor_arg_list(list(constructor_arg)::in,
    list(constructor_arg)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_constructor_arg_list([], [], !Info, !Specs).
qualify_constructor_arg_list([Arg0 | Args0], [Arg | Args], !Info, !Specs) :-
    qualify_type(Arg0 ^ arg_type, Type, !Info, !Specs),
    Arg = Arg0 ^ arg_type := Type,
    qualify_constructor_arg_list(Args0, Args, !Info, !Specs).

:- pred qualify_type_list(list(mer_type)::in, list(mer_type)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_list([], [], !Info, !Specs).
qualify_type_list([Type0 | Types0], [Type | Types], !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs),
    qualify_type_list(Types0, Types, !Info, !Specs).

:- pred qualify_maybe_type(maybe(mer_type)::in, maybe(mer_type)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_maybe_type(no, no, !Info, !Specs).
qualify_maybe_type(yes(Type0), yes(Type), !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs).

    % Qualify a type and its argument types.
    %
:- pred qualify_type(mer_type::in, mer_type::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type(type_variable(Var, Kind), type_variable(Var, Kind), !Info,
        !Specs).
qualify_type(defined_type(SymName0, Args0, Kind),
        defined_type(SymName, Args, Kind), !Info, !Specs) :-
    Arity = list.length(Args0),
    TypeCtorId0 = mq_id(SymName0, Arity),
    mq_info_get_types(!.Info, Types),
    find_unique_match(TypeCtorId0, TypeCtorId, Types, type_id, !Info, !Specs),
    TypeCtorId = mq_id(SymName, _),
    qualify_type_list(Args0, Args, !Info, !Specs).
qualify_type(builtin_type(BuiltinType), builtin_type(BuiltinType), !Info,
        !Specs) :-
    % The types `int', `float', and `string' are builtin types, defined by
    % the compiler, but arguably they ought to be defined in int.m, float.m,
    % and string.m, and so if someone uses the type `int' in the interface,
    % then we don't want to warn about `import_module int' in the interface.
    % We don't do the same for `character', since the corresponding library
    % module `char' will be flagged as used in the interface if the type
    % `char' is used.
    (
        BuiltinType = builtin_type_int,
        mq_info_set_module_used(unqualified("int"), !Info)
    ;
        BuiltinType = builtin_type_float,
        mq_info_set_module_used(unqualified("float"), !Info)
    ;
        BuiltinType = builtin_type_string,
        mq_info_set_module_used(unqualified("string"), !Info)
    ;
        BuiltinType = builtin_type_char
    ).
qualify_type(higher_order_type(Args0, MaybeRet0, Purity, EvalMethod),
        higher_order_type(Args, MaybeRet, Purity, EvalMethod),
        !Info, !Specs) :-
    qualify_type_list(Args0, Args, !Info, !Specs),
    qualify_maybe_type(MaybeRet0, MaybeRet, !Info, !Specs).
qualify_type(tuple_type(Args0, Kind), tuple_type(Args, Kind), !Info, !Specs) :-
    qualify_type_list(Args0, Args, !Info, !Specs).
qualify_type(apply_n_type(Var, Args0, Kind), apply_n_type(Var, Args, Kind),
        !Info, !Specs) :-
    qualify_type_list(Args0, Args, !Info, !Specs).
qualify_type(kinded_type(Type0, Kind), kinded_type(Type, Kind),
        !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs).

:- pred qualify_type_ctor(type_ctor::in, type_ctor::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_ctor(!TypeCtor, !Info, !Specs) :-
    !.TypeCtor = type_ctor(SymName0, Arity),
    ( is_builtin_atomic_type(!.TypeCtor) ->
        SymName = SymName0
    ;
        TypeCtorId0 = mq_id(SymName0, Arity),
        mq_info_get_types(!.Info, Types),
        find_unique_match(TypeCtorId0, TypeCtorId, Types, type_id,
            !Info, !Specs),
        TypeCtorId = mq_id(SymName, _)
    ),
    !:TypeCtor = type_ctor(SymName, Arity).

:- pred qualify_pragma(pragma_type::in, pragma_type::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma(Pragma0, Pragma, !Info, !Specs) :-
    (
        ( Pragma0 = pragma_source_file(_)
        ; Pragma0 = pragma_foreign_decl(_)
        ; Pragma0 = pragma_foreign_code(_)
        ; Pragma0 = pragma_foreign_import_module(_)
        ; Pragma0 = pragma_inline(_)
        ; Pragma0 = pragma_no_inline(_)
        ; Pragma0 = pragma_obsolete(_)
        ; Pragma0 = pragma_no_detism_warning(_)
        ; Pragma0 = pragma_unused_args(_)
        ; Pragma0 = pragma_exceptions(_)
        ; Pragma0 = pragma_trailing_info(_)
        ; Pragma0 = pragma_mm_tabling_info(_)
        ; Pragma0 = pragma_fact_table(_)
        ; Pragma0 = pragma_promise_pure(_)
        ; Pragma0 = pragma_promise_semipure(_)
        ; Pragma0 = pragma_promise_eqv_clauses(_)
        ; Pragma0 = pragma_terminates(_)
        ; Pragma0 = pragma_does_not_terminate(_)
        ; Pragma0 = pragma_check_termination(_)
        ; Pragma0 = pragma_mode_check_clauses(_)
        ; Pragma0 = pragma_require_feature_set(_)
        ),
        Pragma = Pragma0
    ;
        Pragma0 = pragma_reserve_tag(_TypeCtor0),
        % XXX We should be module qualifying TypeCtor0 here,
        % not in add_pragma.m. However, the code in add_pragma.m
        % does generate better error messages than qualify_type_ctor does;
        % this implies we should fix qualify_type_ctor.
        Pragma = Pragma0
    ;
        Pragma0 = pragma_foreign_export_enum(FEEInfo0),
        FEEInfo0 = pragma_info_foreign_export_enum(Lang, TypeCtor0,
            Attributes, Overrides),
        qualify_type_ctor(TypeCtor0, TypeCtor, !Info, !Specs),
        FEEInfo = pragma_info_foreign_export_enum(Lang, TypeCtor,
            Attributes, Overrides),
        Pragma = pragma_foreign_export_enum(FEEInfo)
    ;
        Pragma0 = pragma_foreign_enum(FEInfo0),
        FEInfo0 = pragma_info_foreign_enum(Lang, TypeCtor0, Values),
        qualify_type_ctor(TypeCtor0, TypeCtor, !Info, !Specs),
        FEInfo = pragma_info_foreign_enum(Lang, TypeCtor, Values),
        Pragma = pragma_foreign_enum(FEInfo)
    ;
        Pragma0 = pragma_foreign_proc(FPInfo0),
        FPInfo0 = pragma_info_foreign_proc(Attrs0, Name, PredOrFunc,
            Vars0, Varset, InstVarset, Impl),
        qualify_pragma_vars(Vars0, Vars, !Info, !Specs),
        UserSharing0 = get_user_annotated_sharing(Attrs0),
        qualify_user_sharing(UserSharing0, UserSharing, !Info, !Specs),
        set_user_annotated_sharing(UserSharing, Attrs0, Attrs),
        FPInfo = pragma_info_foreign_proc(Attrs, Name, PredOrFunc,
            Vars, Varset, InstVarset, Impl),
        Pragma = pragma_foreign_proc(FPInfo)
    ;
        Pragma0 = pragma_oisu(OISUInfo0),
        OISUInfo0 = pragma_info_oisu(TypeCtor0, CreatorPreds,
            MutatorPreds, DestructorPreds),
        % XXX Preds
        qualify_type_ctor(TypeCtor0, TypeCtor, !Info, !Specs),
        OISUInfo = pragma_info_oisu(TypeCtor, CreatorPreds,
            MutatorPreds, DestructorPreds),
        Pragma = pragma_oisu(OISUInfo)
    ;
        Pragma0 = pragma_tabled(TabledInfo0),
        TabledInfo0 = pragma_info_tabled(EvalMethod, PredNameArityPF,
            MModes0, Attrs),
        (
            MModes0 = yes(Modes0),
            qualify_mode_list(Modes0, Modes, !Info, !Specs),
            MModes = yes(Modes)
        ;
            MModes0 = no,
            MModes = no
        ),
        TabledInfo = pragma_info_tabled(EvalMethod, PredNameArityPF,
            MModes, Attrs),
        Pragma = pragma_tabled(TabledInfo)
    ;
        Pragma0 = pragma_foreign_proc_export(FPEInfo0),
        FPEInfo0 = pragma_info_foreign_proc_export(Lang, PredNameModesPF0,
            CFunc),
        PredNameModesPF0 = pred_name_modes_pf(Name, Modes0, PredOrFunc),
        qualify_mode_list(Modes0, Modes, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(Name, Modes, PredOrFunc),
        FPEInfo = pragma_info_foreign_proc_export(Lang, PredNameModesPF,
            CFunc),
        Pragma = pragma_foreign_proc_export(FPEInfo)
    ;
        Pragma0 = pragma_type_spec(TypeSpecInfo0),
        TypeSpecInfo0 = pragma_info_type_spec(A, B, C, D, MaybeModes0,
            Subst0, G, H),
        (
            MaybeModes0 = yes(Modes0),
            qualify_mode_list(Modes0, Modes, !Info, !Specs),
            MaybeModes = yes(Modes)
        ;
            MaybeModes0 = no,
            MaybeModes = no
        ),
        qualify_type_spec_subst(Subst0, Subst, !Info, !Specs),
        TypeSpecInfo = pragma_info_type_spec(A, B, C, D, MaybeModes,
            Subst, G, H),
        Pragma = pragma_type_spec(TypeSpecInfo)
    ;
        Pragma0 = pragma_termination_info(TermInfo0),
        TermInfo0 = pragma_info_termination_info(PredNameModesPF0, Args, Term),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        qualify_mode_list(ModeList0, ModeList, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        TermInfo = pragma_info_termination_info(PredNameModesPF, Args, Term),
        Pragma = pragma_termination_info(TermInfo)
    ;
        Pragma0 = pragma_structure_sharing(SharingInfo0),
        SharingInfo0 = pragma_info_structure_sharing(PredNameModesPF0,
            Vars, Types, Sharing),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        qualify_mode_list(ModeList0, ModeList, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        SharingInfo = pragma_info_structure_sharing(PredNameModesPF,
            Vars, Types, Sharing),
        Pragma = pragma_structure_sharing(SharingInfo)
    ;
        Pragma0 = pragma_structure_reuse(ReuseInfo0),
        ReuseInfo0 = pragma_info_structure_reuse(PredNameModesPF0,
            Vars, Types, ReuseTuples),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        qualify_mode_list(ModeList0, ModeList, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
            Vars, Types, ReuseTuples),
        Pragma = pragma_structure_reuse(ReuseInfo)
    ;
        Pragma0 = pragma_termination2_info(Term2Info0),
        Term2Info0 = pragma_info_termination2_info(PredNameModesPF0,
            SuccessArgs, FailureArgs, Term),
        PredNameModesPF0 = pred_name_modes_pf(SymName, ModeList0, PredOrFunc),
        qualify_mode_list(ModeList0, ModeList, !Info, !Specs),
        PredNameModesPF = pred_name_modes_pf(SymName, ModeList, PredOrFunc),
        Term2Info = pragma_info_termination2_info(PredNameModesPF,
            SuccessArgs, FailureArgs, Term),
        Pragma = pragma_termination2_info(Term2Info)
    ).

:- pred qualify_pragma_vars(list(pragma_var)::in, list(pragma_var)::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_pragma_vars([], [], !Info, !Specs).
qualify_pragma_vars([PragmaVar0  | PragmaVars0], [PragmaVar | PragmaVars],
        !Info, !Specs) :-
    PragmaVar0 = pragma_var(Var, Name, Mode0, Box),
    qualify_mode(Mode0, Mode, !Info, !Specs),
    PragmaVar = pragma_var(Var, Name, Mode, Box),
    qualify_pragma_vars(PragmaVars0, PragmaVars, !Info, !Specs).

:- pred qualify_type_spec_subst(assoc_list(tvar, mer_type)::in,
    assoc_list(tvar, mer_type)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_type_spec_subst([], [], !Info, !Specs).
qualify_type_spec_subst([Var - Type0 |  Subst0], [Var - Type | Subst],
        !Info, !Specs) :-
    qualify_type(Type0, Type, !Info, !Specs),
    qualify_type_spec_subst(Subst0, Subst, !Info, !Specs).

:- pred qualify_prog_constraints(prog_constraints::in,
    prog_constraints::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraints(constraints(UnivCs0, ExistCs0),
        constraints(UnivCs, ExistCs), !Info, !Specs) :-
    qualify_prog_constraint_list(UnivCs0, UnivCs, !Info, !Specs),
    qualify_prog_constraint_list(ExistCs0, ExistCs, !Info, !Specs).

:- pred qualify_prog_constraint_list(list(prog_constraint)::in,
    list(prog_constraint)::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraint_list([], [], !Info, !Specs).
qualify_prog_constraint_list([C0 | C0s], [C | Cs], !Info, !Specs) :-
    qualify_prog_constraint(C0, C, !Info, !Specs),
    qualify_prog_constraint_list(C0s, Cs, !Info, !Specs).

:- pred qualify_prog_constraint(prog_constraint::in, prog_constraint::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_prog_constraint(constraint(ClassName0, Types0),
        constraint(ClassName, Types), !Info, !Specs) :-
    list.length(Types0, Arity),
    qualify_class_name(mq_id(ClassName0, Arity), mq_id(ClassName, _),
        !Info, !Specs),
    qualify_type_list(Types0, Types, !Info, !Specs).

:- pred qualify_class_name(mq_id::in, mq_id::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_name(Class0, Class, !Info, !Specs) :-
    mq_info_get_classes(!.Info, ClassIdSet),
    find_unique_match(Class0, Class, ClassIdSet, class_id, !Info, !Specs).

:- pred qualify_class_interface(class_methods::in, class_methods::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_interface([], [], !Info, !Specs).
qualify_class_interface([M0 | M0s], [M | Ms], !Info, !Specs) :-
    qualify_class_method(M0, M, !Info, !Specs),
    qualify_class_interface(M0s, Ms, !Info, !Specs).

:- pred qualify_class_method(class_method::in, class_method::out,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_class_method(Method0, Method, !Info, !Specs) :-
    % There is no need to qualify the method name, since that is
    % done when the item is parsed.
    (
        Method0 = method_pred_or_func(TypeVarset, InstVarset, ExistQVars,
            PredOrFunc, Name, TypesAndModes0, WithType0, WithInst0, MaybeDet,
            Cond, Purity, ClassContext0, Context),
        qualify_types_and_modes(TypesAndModes0, TypesAndModes, !Info, !Specs),
        qualify_prog_constraints(ClassContext0, ClassContext, !Info, !Specs),
        map_fold2_maybe(qualify_type, WithType0, WithType, !Info, !Specs),
        map_fold2_maybe(qualify_inst, WithInst0, WithInst, !Info, !Specs),
        Method = method_pred_or_func(TypeVarset, InstVarset, ExistQVars,
            PredOrFunc, Name, TypesAndModes, WithType, WithInst, MaybeDet,
            Cond, Purity, ClassContext, Context)
    ;
        Method0 = method_pred_or_func_mode(Varset, PredOrFunc, Name, Modes0,
            WithInst0, MaybeDet, Cond, Context),
        qualify_mode_list(Modes0, Modes, !Info, !Specs),
        map_fold2_maybe(qualify_inst, WithInst0, WithInst, !Info, !Specs),
        Method = method_pred_or_func_mode(Varset, PredOrFunc, Name, Modes,
            WithInst, MaybeDet, Cond, Context)
    ).

:- pred qualify_instance_body(sym_name::in,
    instance_body::in, instance_body::out) is det.

qualify_instance_body(ClassName, InstanceBody0, InstanceBody) :-
    (
        InstanceBody0 = instance_body_abstract,
        InstanceBody = instance_body_abstract
    ;
        InstanceBody0 = instance_body_concrete(M0s),
        (
            ClassName = unqualified(_),
            Ms = M0s
        ;
            ClassName = qualified(_, _),
            sym_name_get_module_name_default(ClassName, unqualified(""),
                Module),
            Qualify = (pred(M0::in, M::out) is det :-
                M0 = instance_method(A, Method0, C, D, E),
                add_module_qualifier(Module, Method0, Method),
                M = instance_method(A, Method, C, D, E)
            ),
            list.map(Qualify, M0s, Ms)
        ),
        InstanceBody = instance_body_concrete(Ms)
    ).

:- pred add_module_qualifier(sym_name::in, sym_name::in, sym_name::out) is det.

add_module_qualifier(DefaultModule, SymName0, SymName) :-
    (
        SymName0 = unqualified(Name),
        SymName = qualified(DefaultModule, Name)
    ;
        SymName0 = qualified(SymModule, SubSymName),
        ( match_sym_name(SymModule, DefaultModule) ->
            SymName = qualified(DefaultModule, SubSymName)
        ;
            % This case is an error. The user must have written something like
            %   :- instance foo.bar(some_type) where [
            %       pred(baz.p/1) is q
            %   ].
            % where the module qualifier on the pred or func in the instance
            % (baz) does not match the qualifier for the class name (foo).
            %
            % We don't report the error here, we just leave the original
            % module qualifier intact so that the error can be reported
            % later on.
            SymName = SymName0
        )
    ).

    % Find the unique match in the current name space for a given mq_id
    % from a list of ids. If none exists, either because no match
    % was found or multiple matches were found, report an error.
    % This predicate assumes that type_ids, inst_ids, mode_ids and
    % class_ids have the same representation.
    %
:- pred find_unique_match(mq_id::in, mq_id::out, id_set::in, id_type::in,
    mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

find_unique_match(Id0, Id, Ids, TypeOfId, !Info, !Specs) :-
    % Find all IDs which match the current id.
    Id0 = mq_id(SymName0, Arity),
    mq_info_get_modules(!.Info, Modules),
    id_set_search_sym_arity(Ids, SymName0, Arity, Modules, MatchingModules0),

    ( mq_info_get_import_status(!.Info, mq_status_exported) ->
        % Items in the interface may only refer to modules
        % imported in the interface.
        mq_info_get_interface_visible_modules(!.Info, InterfaceImports),
        list.filter(set.contains(InterfaceImports),
            MatchingModules0, MatchingModules)
    ;
        MatchingModules = MatchingModules0
    ),

    (
        MatchingModules = [],
        % No matches for this id.
        Id = Id0,
        ( mq_info_get_report_error_flag(!.Info, yes) ->
            report_undefined(MatchingModules0, !.Info, Id0, TypeOfId, !Specs),
            mq_info_set_error_flag(TypeOfId, !Info)
        ;
            true
        )
    ;
        MatchingModules = [Module],
        % A unique match for this ID.
        IdName = unqualify_name(SymName0),
        Id = mq_id(qualified(Module, IdName), Arity),
        mq_info_set_module_used(Module, !Info),
        ItemType = convert_simple_item_type(TypeOfId),
        ItemName0 = item_name(SymName0, Arity),
        ItemName = item_name(qualified(Module, IdName), Arity),
        update_recompilation_info(
            recompilation.record_used_item(ItemType, ItemName0, ItemName),
            !Info)
    ;
        MatchingModules = [_, _ | _],
        % There are multiple matches.
        Id = Id0,
        ( mq_info_get_report_error_flag(!.Info, yes) ->
            mq_info_get_error_context(!.Info, ErrorContext),
            report_ambiguous_match(ErrorContext, Id0, TypeOfId,
                MatchingModules, !Specs),
            mq_info_set_error_flag(TypeOfId, !Info)
        ;
            true
        )
    ).

:- pred update_recompilation_info(
    pred(recompilation_info, recompilation_info)::in(pred(in, out) is det),
    mq_info::in, mq_info::out) is det.

update_recompilation_info(Pred, !Info) :-
    mq_info_get_recompilation_info(!.Info, MaybeRecompInfo0),
    (
        MaybeRecompInfo0 = yes(RecompInfo0),
        Pred(RecompInfo0, RecompInfo),
        mq_info_set_recompilation_info(yes(RecompInfo), !Info)
    ;
        MaybeRecompInfo0 = no
    ).

:- func convert_simple_item_type(id_type) = item_type.

convert_simple_item_type(type_id) = type_abstract_item.
convert_simple_item_type(mode_id) = mode_item.
convert_simple_item_type(inst_id) = inst_item.
convert_simple_item_type(class_id) = typeclass_item.

:- pred qualify_user_sharing(user_annotated_sharing::in,
    user_annotated_sharing::out, mq_info::in, mq_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

qualify_user_sharing(!UserSharing, !Info, !Specs) :-
    (
        !.UserSharing = no_user_annotated_sharing
    ;
        !.UserSharing = user_sharing(Sharing, MaybeTypes0),
        (
            MaybeTypes0 = yes(user_type_info(Types0, TVarset)),
            qualify_type_list(Types0, Types, !Info, !Specs),
            MaybeTypes = yes(user_type_info(Types, TVarset)),
            !:UserSharing = user_sharing(Sharing, MaybeTypes)
        ;
            MaybeTypes0 = no
        )
    ).

%-----------------------------------------------------------------------------%

:- type id_type
    --->    type_id
    ;       mode_id
    ;       inst_id
    ;       class_id.

:- type error_context == pair(mq_error_context, prog_context).

:- type mq_id
    --->    mq_id(sym_name, int).

:- type mq_error_context
    --->    mqec_type(mq_id)
    ;       mqec_inst(mq_id)
    ;       mqec_mode(mq_id)
    ;       mqec_pred_or_func(pred_or_func, mq_id)
    ;       mqec_pred_or_func_mode(maybe(pred_or_func), mq_id)
    ;       mqec_pragma(pragma_type)
    ;       mqec_lambda_expr
    ;       mqec_clause_mode_annotation
    ;       mqec_type_qual
    ;       mqec_class(mq_id)
    ;       mqec_instance(mq_id)
    ;       mqec_mutable(string)
    ;       mqec_event_spec_attr(string, string). % event name, attr name

:- func id_to_sym_name_and_arity(mq_id) = sym_name_and_arity.

id_to_sym_name_and_arity(mq_id(SymName, Arity)) = SymName / Arity.

    % Report an undefined type, inst or mode.
    %
:- pred report_undefined(list(module_name)::in, mq_info::in, mq_id::in,
    id_type::in, list(error_spec)::in, list(error_spec)::out) is det.

report_undefined(MatchingModules, Info, Id, IdType, !Specs) :-
    mq_info_get_error_context(Info, ErrorContext - Context),
    id_type_to_string(IdType, IdStr),
    Pieces1 = [words("In")] ++ mq_error_context_to_pieces(ErrorContext) ++
        [suffix(":"), nl, words("error: undefined"), fixed(IdStr),
        sym_name_and_arity(id_to_sym_name_and_arity(Id)),
        suffix("."), nl],
    (
        % If it is a qualified symbol, then check whether the specified module
        % has been imported.

        Id = mq_id(qualified(ModuleName, _), _Arity),
        mq_info_get_imported_modules(Info, ImportedModules),
        \+ set.member(ModuleName, ImportedModules),
        \+ ModuleName = Info ^ this_module
    ->
        Pieces2 = [words("(The module"), sym_name(ModuleName),
            words("has not been imported.)"), nl]
    ;
        (
            MatchingModules = [_ | MatchingModulesTail],
            (
                MatchingModulesTail = [],
                ModuleWord = "module",
                HasWord = "has"
            ;
                MatchingModulesTail = [_ | _],
                ModuleWord = "modules",
                HasWord = "have"
            ),
            MatchingSymNames = list.map(wrap_module_name, MatchingModules),
            Pieces2 = [words("(The"), fixed(ModuleWord)] ++
                component_list_to_pieces(MatchingSymNames) ++
                [fixed(HasWord),
                    words("not been imported in the interface.)"), nl]
        ;
            MatchingModules = [],
            Pieces2 = []
        )
    ),
    Msg = simple_msg(Context, [always(Pieces1 ++ Pieces2)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

    % Report an error where a type, inst, mode or typeclass had
    % multiple possible matches.
    %
:- pred report_ambiguous_match(error_context::in, mq_id::in, id_type::in,
    list(module_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

report_ambiguous_match(ErrorContext - Context, Id, IdType, Modules, !Specs) :-
    id_type_to_string(IdType, IdStr),
    ModuleNames = list.map(wrap_module_name, Modules),
    MainPieces = [words("In")] ++ mq_error_context_to_pieces(ErrorContext) ++
        [words("ambiguity error: multiple possible matches for"),
        fixed(IdStr), wrap_id(Id), suffix("."), nl,
        words("The possible matches are in modules")] ++ ModuleNames ++
        [suffix("."), nl],
    VerbosePieces = [words("An explicit module qualifier"),
        words("may be necessary."), nl],
    Msg = simple_msg(Context,
        [always(MainPieces), verbose_only(VerbosePieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

    % Give a context for the current error message.
    %
:- func mq_error_context_to_pieces(mq_error_context) = list(format_component).

mq_error_context_to_pieces(mqec_type(Id)) =
    [words("definition of type"), wrap_id(Id)].
mq_error_context_to_pieces(mqec_mode(Id)) =
    [words("definition of mode"), wrap_id(Id)].
mq_error_context_to_pieces(mqec_inst(Id)) =
    [words("definition of inst"), wrap_id(Id)].
mq_error_context_to_pieces(mqec_pred_or_func(PredOrFunc, Id)) = Pieces :-
    Id = mq_id(SymName, OrigArity),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    Pieces = [words("definition of "),
        fixed(pred_or_func_to_full_str(PredOrFunc)),
        sym_name_and_arity(SymName / Arity)].
mq_error_context_to_pieces(mqec_pred_or_func_mode(MaybePredOrFunc, Id))
        = Pieces :-
    Id = mq_id(SymName, OrigArity),
    (
        MaybePredOrFunc = yes(PredOrFunc),
        adjust_func_arity(PredOrFunc, OrigArity, Arity),
        Pieces = [words("mode declaration for"),
            fixed(pred_or_func_to_full_str(PredOrFunc)),
            sym_name_and_arity(SymName / Arity)]
    ;
        MaybePredOrFunc = no,
        Pieces = [words("mode declaration for"),
            sym_name_and_arity(SymName / OrigArity)]
    ).
mq_error_context_to_pieces(mqec_lambda_expr) =
    [words("mode declaration for lambda expression")].
mq_error_context_to_pieces(mqec_clause_mode_annotation) =
    [words("clause mode annotation")].
mq_error_context_to_pieces(mqec_pragma(Pragma)) =
        [words("pragma"), words(PragmaName)] :-
    (
        Pragma = pragma_foreign_decl(_),
        PragmaName = "foreign_decl"
    ;
        Pragma = pragma_foreign_code(_),
        PragmaName = "foreign_code"
    ;
        Pragma = pragma_foreign_proc(_),
        PragmaName = "foreign_proc"
    ;
        Pragma = pragma_foreign_import_module(_),
        PragmaName = "foreign_import_module"
    ;
        Pragma = pragma_foreign_proc_export(_),
        PragmaName = "foreign_proc_export"
    ;
        Pragma = pragma_foreign_export_enum(_),
        PragmaName = "foreign_export_enum"
    ;
        Pragma = pragma_foreign_enum(_),
        PragmaName = "foreign_enum"
    ;
        Pragma = pragma_type_spec(_),
        PragmaName = "type_spec"
    ;
        Pragma = pragma_inline(_),
        PragmaName = "inline"
    ;
        Pragma = pragma_no_inline(_),
        PragmaName = "no_inline"
    ;
        Pragma = pragma_unused_args(_),
        PragmaName = "unused_args"
    ;
        Pragma = pragma_exceptions(_),
        PragmaName = "exceptions"
    ;
        Pragma = pragma_trailing_info(_),
        PragmaName = "trailing_info"
    ;
        Pragma = pragma_mm_tabling_info(_),
        PragmaName = "mm_tabling_info"
    ;
        Pragma = pragma_obsolete(_),
        PragmaName = "obsolete"
    ;
        Pragma = pragma_no_detism_warning(_),
        PragmaName = "no_detism_warning"
    ;
        Pragma = pragma_source_file(_),
        PragmaName = "source_file"
    ;
        Pragma = pragma_tabled(_),
        PragmaName = "tabled"
    ;
        Pragma = pragma_fact_table(_),
        PragmaName = "fact_table"
    ;
        Pragma = pragma_reserve_tag(_),
        PragmaName = "reserve_tag"
    ;
        Pragma = pragma_oisu(_),
        PragmaName = "oisu"
    ;
        Pragma = pragma_promise_eqv_clauses(_),
        PragmaName = "promise_equivalent_clauses"
    ;
        Pragma = pragma_promise_pure(_),
        PragmaName = "promise_pure"
    ;
        Pragma = pragma_promise_semipure(_),
        PragmaName = "promise_semipure"
    ;
        Pragma = pragma_termination_info(_),
        PragmaName = "termination_info"
    ;
        Pragma = pragma_termination2_info(_),
        PragmaName = "termination2_info"
    ;
        Pragma = pragma_terminates(_),
        PragmaName = "terminates"
    ;
        Pragma = pragma_does_not_terminate(_),
        PragmaName = "does_not_terminate"
    ;
        Pragma = pragma_check_termination(_),
        PragmaName = "check_termination"
    ;
        Pragma = pragma_mode_check_clauses(_),
        PragmaName = "mode_check_clauses"
    ;
        Pragma = pragma_structure_sharing(_),
        PragmaName = "structure_sharing"
    ;
        Pragma = pragma_structure_reuse(_),
        PragmaName = "structure_reuse"
    ;
        Pragma = pragma_require_feature_set(_),
        PragmaName = "require_feature_set"
    ).
mq_error_context_to_pieces(mqec_type_qual) =
    [words("explicit type qualification")].
mq_error_context_to_pieces(mqec_class(Id)) =
    [words("declaration of typeclass"), wrap_id(Id)].
mq_error_context_to_pieces(mqec_instance(Id)) =
    [words("declaration of instance of typeclass"), wrap_id(Id)].
mq_error_context_to_pieces(mqec_mutable(Name)) =
    [words("declaration for mutable "), quote(Name)].
mq_error_context_to_pieces(mqec_event_spec_attr(EventName, AttrName)) =
    [words("attribute"), quote(AttrName), words("for"), quote(EventName)].

:- pred id_type_to_string(id_type::in, string::out) is det.

id_type_to_string(type_id, "type").
id_type_to_string(mode_id, "mode").
id_type_to_string(inst_id, "inst").
id_type_to_string(class_id, "typeclass").

    % Warn about modules imported in the interface when they do not need to be.
    %
:- pred maybe_warn_unused_interface_imports(module_name::in, string::in,
    list(module_name)::in, list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_unused_interface_imports(ModuleName, FileName, UnusedImports,
        !Specs) :-
    (
        UnusedImports = []
    ;
        UnusedImports = [_ | _],
        term.context_init(FileName, 1, Context),
        UnusedSymNames = list.map(wrap_module_name, UnusedImports),
        Pieces = [words("In module"), sym_name(ModuleName), suffix(":"), nl,
            words("warning:"),
            words(choose_number(UnusedImports, "module", "modules"))] ++
            component_list_to_pieces(UnusedSymNames) ++
            [words(choose_number(UnusedImports, "is", "are")),
            words("imported in the interface,"),
            words("but"), words(choose_number(UnusedImports, "is", "are")),
            words("not used in the interface.")],
        Msg = simple_msg(Context,
            [option_is_set(warn_interface_imports, yes, [always(Pieces)])]),
        Severity = severity_conditional(warn_interface_imports, yes,
            severity_warning, no),
        Spec = error_spec(Severity, phase_parse_tree_to_hlds, [Msg]),
        !:Specs = [Spec | !.Specs]
    ).

:- func wrap_module_name(module_name) = format_component.

wrap_module_name(SymName) = sym_name(SymName).

:- func wrap_id(mq_id) = format_component.

wrap_id(mq_id(Name, Arity)) = sym_name_and_arity(Name / Arity).

    % Output an error message about an ill-formed user_inst.
    %
:- pred report_invalid_user_inst(sym_name::in, list(mer_inst)::in,
    error_context::in, list(error_spec)::in, list(error_spec)::out) is det.

report_invalid_user_inst(_SymName, _Insts, ErrorContext - Context, !Specs) :-
    ContextPieces = mq_error_context_to_pieces(ErrorContext),
    Pieces = [words("In")] ++ ContextPieces ++ [suffix(":"), nl,
        words("error: variable used as inst constructor."), nl],
    Msg = simple_msg(Context, [always(Pieces)]),
    Spec = error_spec(severity_error, phase_parse_tree_to_hlds, [Msg]),
    !:Specs = [Spec | !.Specs].

%-----------------------------------------------------------------------------%

    % is_builtin_atomic_type(TypeCtor):
    %
    % Succeeds iff 'TypeCtor' is the type_ctor of a builtin atomic type.
    %
:- pred is_builtin_atomic_type(type_ctor::in) is semidet.

is_builtin_atomic_type(type_ctor(unqualified("int"), 0)).
is_builtin_atomic_type(type_ctor(unqualified("float"), 0)).
is_builtin_atomic_type(type_ctor(unqualified("string"), 0)).
is_builtin_atomic_type(type_ctor(unqualified("character"), 0)).

%-----------------------------------------------------------------------------%
%
% Access and initialisation predicates.
%

:- pred init_mq_info(list(item)::in, globals::in, bool::in, module_name::in,
    mq_info::out) is det.

init_mq_info(Items, Globals, ReportErrors, ModuleName, Info) :-
    term.context_init(Context),
    ErrorContext = mqec_type(mq_id(unqualified(""), 0)) - Context,
    set.init(InterfaceModules0),
    set.init(InstanceModules),
    ExportedInstancesFlag = no,
    get_implicit_dependencies(Items, Globals, ImportDeps, UseDeps),
    set.list_to_set(ImportDeps `list.append` UseDeps, ImportedModules),

    % Ancestor modules are visible without being explicitly imported.
    set.insert_list([ModuleName | get_ancestors(ModuleName)],
        ImportedModules, InterfaceVisibleModules),

    id_set_init(Empty),
    globals.lookup_bool_option(Globals, smart_recompilation,
        SmartRecompilation),
    (
        SmartRecompilation = no,
        MaybeRecompInfo = no
    ;
        SmartRecompilation = yes,
        MaybeRecompInfo = yes(init_recompilation_info(ModuleName))
    ),
    Info = mq_info(ImportedModules, InterfaceVisibleModules,
        Empty, Empty, Empty, Empty, Empty, Empty,
        InterfaceModules0, InstanceModules, ExportedInstancesFlag,
        mq_status_local, 0, no, no, ReportErrors, ErrorContext, ModuleName,
        may_be_unqualified, MaybeRecompInfo).

:- pred mq_info_get_imported_modules(mq_info::in, set(module_name)::out)
    is det.
:- pred mq_info_get_interface_visible_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_modules(mq_info::in, module_id_set::out) is det.
:- pred mq_info_get_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_impl_types(mq_info::in, type_id_set::out) is det.
:- pred mq_info_get_insts(mq_info::in, inst_id_set::out) is det.
:- pred mq_info_get_modes(mq_info::in, mode_id_set::out) is det.
:- pred mq_info_get_classes(mq_info::in, class_id_set::out) is det.
:- pred mq_info_get_unused_interface_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_imported_instance_modules(mq_info::in,
    set(module_name)::out) is det.
:- pred mq_info_get_exported_instances_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_import_status(mq_info::in, mq_import_status::out) is det.
% :- pred mq_info_get_type_error_flag(mq_info::in, bool::out) is det.
% :- pred mq_info_get_mode_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_report_error_flag(mq_info::in, bool::out) is det.
:- pred mq_info_get_error_context(mq_info::in, error_context::out) is det.

mq_info_get_imported_modules(Info, Info ^ imported_modules).
mq_info_get_interface_visible_modules(Info, Info ^ interface_visible_modules).
mq_info_get_modules(Info, Info ^ modules).
mq_info_get_types(Info, Info ^ types).
mq_info_get_impl_types(Info, Info ^ impl_types).
mq_info_get_insts(Info, Info ^ insts).
mq_info_get_modes(Info, Info ^ modes).
mq_info_get_classes(Info, Info ^ classes).
mq_info_get_unused_interface_modules(Info, Info ^ unused_interface_modules).
mq_info_get_imported_instance_modules(Info, Info ^ imported_instance_modules).
mq_info_get_exported_instances_flag(Info, Info ^ exported_instances_flag).
mq_info_get_import_status(Info, Info ^ import_status).
mq_info_get_type_error_flag(Info, Info ^ type_error_flag).
mq_info_get_mode_error_flag(Info, Info ^ mode_error_flag).
mq_info_get_report_error_flag(Info, Info ^ report_error_flag).
mq_info_get_error_context(Info, Info ^ error_context).
mq_info_get_need_qual_flag(Info, Info ^ need_qual_flag).
mq_info_get_recompilation_info(Info, Info ^ maybe_recompilation_info).

:- pred mq_info_set_imported_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_interface_visible_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_modules(module_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_types(type_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_impl_types(type_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_insts(inst_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_modes(mode_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_classes(class_id_set::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_unused_interface_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_imported_instance_modules(set(module_name)::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_exported_instances_flag(bool::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_import_status(mq_import_status::in,
    mq_info::in, mq_info::out) is det.
:- pred mq_info_set_type_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_mode_error_flag(mq_info::in, mq_info::out) is det.
:- pred mq_info_set_error_context(error_context::in,
    mq_info::in, mq_info::out) is det.

mq_info_set_imported_modules(ImportedModules, Info,
    Info ^ imported_modules := ImportedModules).
mq_info_set_interface_visible_modules(ImportedModules, Info,
    Info ^ interface_visible_modules := ImportedModules).
mq_info_set_modules(Modules, Info, Info ^ modules := Modules).
mq_info_set_types(Types, Info, Info ^ types := Types).
mq_info_set_impl_types(Types, Info, Info ^ impl_types := Types).
mq_info_set_insts(Insts, Info, Info ^ insts := Insts).
mq_info_set_modes(Modes, Info, Info ^ modes := Modes).
mq_info_set_classes(Classes, Info, Info ^ classes := Classes).
mq_info_set_unused_interface_modules(Modules, Info,
    Info ^ unused_interface_modules := Modules).
mq_info_set_imported_instance_modules(Modules, Info,
    Info ^ imported_instance_modules := Modules).
mq_info_set_exported_instances_flag(Flag, Info,
    Info ^ exported_instances_flag := Flag).
mq_info_set_import_status(Status, Info, Info ^ import_status := Status).
mq_info_set_type_error_flag(Info, Info ^ type_error_flag := yes).
mq_info_set_mode_error_flag(Info, Info ^ mode_error_flag := yes).
mq_info_set_error_context(Context, Info, Info ^ error_context := Context).
mq_info_set_need_qual_flag(Flag, Info, Info ^ need_qual_flag := Flag).
mq_info_set_recompilation_info(RecompInfo, Info,
    Info ^ maybe_recompilation_info := RecompInfo).

:- pred mq_info_set_error_flag(id_type::in, mq_info::in, mq_info::out) is det.

mq_info_set_error_flag(IdType, !Info) :-
    mq_info_set_error_flag_2(IdType, !Info).

:- pred mq_info_set_error_flag_2(id_type::in, mq_info::in, mq_info::out)
    is det.

mq_info_set_error_flag_2(type_id, !Info) :-
    mq_info_set_type_error_flag(!Info).
mq_info_set_error_flag_2(mode_id, !Info) :-
    mq_info_set_mode_error_flag(!Info).
mq_info_set_error_flag_2(inst_id, !Info) :-
    mq_info_set_mode_error_flag(!Info).
mq_info_set_error_flag_2(class_id, !Info) :-
    mq_info_set_type_error_flag(!Info).

    % If the current item is in the interface, remove its module name
    % from the list of modules not used in the interface (and if the
    % module name is itself module-qualified, recursively mark its
    % parent module as used).
    %
:- pred mq_info_set_module_used(module_name::in, mq_info::in, mq_info::out)
    is det.

mq_info_set_module_used(Module, !Info) :-
    ( mq_info_get_import_status(!.Info, mq_status_exported) ->
        mq_info_get_unused_interface_modules(!.Info, Modules0),
        set.delete(Module, Modules0, Modules),
        mq_info_set_unused_interface_modules(Modules, !Info),
        (
            Module = qualified(ParentModule, _),
            mq_info_set_module_used(ParentModule, !Info)
        ;
            Module = unqualified(_)
        )
    ;
        true
    ).

%----------------------------------------------------------------------------%
% Define a type for representing sets of ids during module qualification
% to allow efficient retrieval of all the modules which define an id
% with a certain name and arity.

% The first set of module_names can be used without module qualifiers,
% items from the second set can only be used with module qualifiers.
% Items from modules imported with a :- use_module declaration and from `.opt'
% files should go into the second set.
:- type id_set == map(pair(string, arity), pair(set(module_name))).

:- type type_id_set == id_set.
:- type mode_id_set == id_set.
:- type inst_id_set == id_set.
:- type class_id_set == id_set.
    % Modules don't have an arity, but for simplicity we use the same
    % data structure here, assigning arity zero to all module names.
:- type module_id_set == id_set.

:- pred id_set_init(id_set::out) is det.

id_set_init(IdSet) :-
    map.init(IdSet).

    % Insert an mq_id into an id_set, aborting with an error if the
    % mq_id is not module qualified.
    %
:- pred id_set_insert(need_qualifier::in, mq_id::in, id_set::in, id_set::out)
    is det.

id_set_insert(_, mq_id(unqualified(_), _), _, _) :-
    unexpected($module, $pred, "unqualified id").
id_set_insert(NeedQualifier, mq_id(qualified(Module, Name), Arity), !IdSet) :-
    ( map.search(!.IdSet, Name - Arity, ImportModules0 - UseModules0) ->
        ImportModules1 = ImportModules0,
        UseModules1 = UseModules0
    ;
        set.init(ImportModules1),
        set.init(UseModules1)
    ),
    (
        NeedQualifier = must_be_qualified,
        set.insert(Module, UseModules1, UseModules),
        ImportModules = ImportModules1
    ;
        NeedQualifier = may_be_unqualified,
        set.insert(Module, ImportModules1, ImportModules),
        UseModules = UseModules1
    ),
    map.set(Name - Arity, ImportModules - UseModules, !IdSet).

:- pred id_set_search_sym_arity(id_set::in, sym_name::in, int::in,
    module_id_set::in, list(module_name)::out) is det.

id_set_search_sym_arity(IdSet, Sym, Arity, Modules, MatchingModules) :-
    UnqualName = unqualify_name(Sym),
    (
        map.search(IdSet, UnqualName - Arity, ImportModules - UseModules)
    ->
        (
            Sym = unqualified(_),
            set.to_sorted_list(ImportModules, MatchingModules)
        ;
            Sym = qualified(Module, _),
            %
            % First, compute the set of modules that this
            % module specifier could possibly refer to.
            %
            % Do a recursive search to find nested modules
            % that match the specified module name.
            %
            ModuleArity = 0,
            id_set_search_sym_arity(Modules, Module, ModuleArity,
                Modules, MatchingParentModules),
            UnqualModule = unqualify_name(Module),
            AppendModuleName = (pred(X::in, Y::out) is det :-
                Y = qualified(X, UnqualModule)
            ),
            list.map(AppendModuleName,
                MatchingParentModules,
                MatchingNestedModules),
            %
            % Add the specified module name itself, in case
            % it refers to a top-level (unnested) module name,
            % since top-level modules don't get inserted into
            % the module_id_set.
            %
            AllMatchingModules = [Module | MatchingNestedModules],
            %
            % Second, compute the set of modules that define this symbol.
            %
            set.union(ImportModules, UseModules, DefiningModules),
            %
            % Third, take the intersection of the sets computed in
            % the first two steps
            %
            FindMatch = (pred(MatchModule::out) is nondet :-
                list.member(MatchModule, AllMatchingModules),
                set.member(MatchModule, DefiningModules)
            ),
            solutions(FindMatch, MatchingModules)
        )
    ;
        MatchingModules = []
    ).

%-----------------------------------------------------------------------------%

get_partial_qualifiers(ModuleName, PartialQualInfo, PartialQualifiers) :-
    PartialQualInfo = partial_qualifier_info(ModuleIdSet),
    (
        ModuleName = unqualified(_),
        PartialQualifiers = []
    ;
        ModuleName = qualified(Parent, Child),
        get_partial_qualifiers_2(Parent, unqualified(Child),
            ModuleIdSet, [], PartialQualifiers)
    ).

:- pred get_partial_qualifiers_2(module_name::in, module_name::in,
    module_id_set::in, list(module_name)::in, list(module_name)::out)
    is det.

get_partial_qualifiers_2(ImplicitPart, ExplicitPart, ModuleIdSet,
        !Qualifiers) :-
    % If the ImplicitPart module was imported, rather than just being
    % used, then insert the ExplicitPart module into the list of
    % valid partial qualifiers.

    ( parent_module_is_imported(ImplicitPart, ExplicitPart, ModuleIdSet) ->
        !:Qualifiers = [ExplicitPart | !.Qualifiers]
    ;
        true
    ),
    % Recursively try to add the other possible partial qualifiers.
    (
        ImplicitPart = qualified(Parent, Child),
        NextImplicitPart = Parent,
        NextExplicitPart = add_outermost_qualifier(Child, ExplicitPart),
        get_partial_qualifiers_2(NextImplicitPart, NextExplicitPart,
            ModuleIdSet, !Qualifiers)
    ;
        ImplicitPart = unqualified(_)
    ).

    % Check whether the parent module was imported, given the name of a
    % child (or grandchild, etc.) module occurring in that parent module.
    %
:- pred parent_module_is_imported(module_name::in, module_name::in,
    module_id_set::in) is semidet.

parent_module_is_imported(ParentModule, ChildModule, ModuleIdSet) :-
    % Find the module name at the start of the ChildModule;
    % this sub-module will be a direct sub-module of ParentModule
    DirectSubModuleName = get_first_module_name(ChildModule),

    % Check that the ParentModule was imported.
    % We do this by looking up the definitions for the direct sub-module
    % and checking that the one in ParentModule came from an
    % imported module.
    Arity = 0,
    map.search(ModuleIdSet, DirectSubModuleName - Arity,
        ImportModules - _UseModules),
    set.member(ParentModule, ImportModules).

    % Given a module name, possibly module-qualified,
    % return the name of the first module in the qualifier list.
    % e.g. given `foo.bar.baz', this returns `foo',
    % and given just `baz', it returns `baz'.
    %
:- func get_first_module_name(module_name) = string.

get_first_module_name(unqualified(ModuleName)) = ModuleName.
get_first_module_name(qualified(Parent, _)) = get_first_module_name(Parent).

%----------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.
%----------------------------------------------------------------------------%
