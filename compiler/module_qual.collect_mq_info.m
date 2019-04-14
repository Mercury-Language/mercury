%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module implements the first pass of module_qual.m; it records
% what entities are available from which modules and with what permissions.
%

:- module parse_tree.module_qual.collect_mq_info.
:- interface.

:- import_module parse_tree.prog_item.

:- import_module list.

%---------------------------------------------------------------------------%

:- type mq_section
    --->    mq_section_exported
    ;       mq_section_local
    ;       mq_section_imported(import_locn)
    ;       mq_section_abstract_imported.

:- type section_mq_info(MS) == (pred(MS, mq_section, module_permissions)).
:- inst section_mq_info     == (pred(in, out, out) is det).

:- pred src_section_mq_info(src_module_section::in,
    mq_section::out, module_permissions::out) is det.

:- pred int_section_mq_info(int_module_section::in,
    mq_section::out, module_permissions::out) is det.

    % Pass over the item_block list collecting all defined module, type,
    % inst, mode and class ids, together with their permissions.
    %
:- pred collect_mq_info_in_item_blocks(
    section_mq_info(MS)::in(section_mq_info), list(item_block(MS))::in,
    mq_info::in, mq_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.module_qual.id_set.
:- import_module parse_tree.parse_sym_name.

src_section_mq_info(SrcSection, MQSection, Permissions) :-
    (
        SrcSection = sms_interface,
        MQSection = mq_section_exported,
        PermInInt = may_use_in_int(may_be_unqualified)
    ;
        ( SrcSection = sms_implementation
        ; SrcSection = sms_impl_but_exported_to_submodules
        ),
        MQSection = mq_section_local,
        PermInInt = may_not_use_in_int
    ),
    PermInImp = may_use_in_imp(may_be_unqualified),
    Permissions = module_permissions(PermInInt, PermInImp).

int_section_mq_info(IntSection, MQSection, Permissions) :-
    (
        IntSection = ims_imported_or_used(_ModuleName, _IntFileKind,
            Locn, ImportOrUse),
        MQSection = mq_section_imported(Locn),
        (
            (
                ImportOrUse = iou_imported,
                NeedQual = may_be_unqualified
            ;
                ImportOrUse = iou_used,
                NeedQual = must_be_qualified
            ),
            (
                % XXX Whether this module's interface can use an mq_id
                % that was imported by an ancestor should depend on whether
                % the ancestor imported that mq_id in its INTERFACE or not.
                % Since we don't know where that import was, this is a
                % conservative approximation.
                ( Locn = import_locn_interface
                ; Locn = import_locn_import_by_ancestor
                ; Locn = import_locn_ancestor_private_interface_proper
                ),
                PermInInt = may_use_in_int(NeedQual)
            ;
                Locn = import_locn_implementation,
                PermInInt = may_not_use_in_int
            ),
            PermInImp = may_use_in_imp(NeedQual)
        ;
            ImportOrUse = iou_used_and_imported,
            PermInInt = may_use_in_int(must_be_qualified),
            PermInImp = may_use_in_imp(may_be_unqualified)
        )
    ;
        IntSection = ims_abstract_imported(_ModuleName, _IntFileKind),
        MQSection = mq_section_abstract_imported,
        PermInInt = may_not_use_in_int,
        PermInImp = may_use_in_imp(must_be_qualified)
    ),
    Permissions = module_permissions(PermInInt, PermInImp).

%---------------------------------------------------------------------------%

collect_mq_info_in_item_blocks(_, [], !Info).
collect_mq_info_in_item_blocks(SectionInfo, [ItemBlock | ItemBlocks], !Info) :-
    ItemBlock = item_block(_, Section, Context, Incls, Avails, Items),
    SectionInfo(Section, MQSection, Permissions),

    trace [compile_time(flag("debug_collect_mq_info")), io(!IO)] (
        io.write_string("collect_mq_info_in_item_block:\n", !IO),
        io.write_string("    ", !IO),
        io.write(Section, !IO),
        io.nl(!IO),
        io.write_string("    ", !IO),
        io.write(Context, !IO),
        io.nl(!IO),
        io.write_string("    ", !IO),
        io.write(MQSection, !IO),
        io.nl(!IO),
        io.write_string("    ", !IO),
        io.write(Permissions, !IO),
        io.nl(!IO),
        io.nl(!IO)
    ),

    % The usual case is Incls = []; optimize for it.
    (
        Incls = []
    ;
        Incls = [_ | _],
        mq_info_get_modules(!.Info, Modules0),
        list.foldl(collect_mq_info_in_item_include(Permissions), Incls,
            Modules0, Modules),
        mq_info_set_modules(Modules, !Info)
    ),

    % The usual case is Avails = [_ | _]. Testing for that would be
    % unnecessary overhead in most cases.
    mq_info_get_imported_modules(!.Info, ImportedModules0),
    mq_info_get_as_yet_unused_interface_modules(!.Info, UnusedIntModules0),
    list.foldl2(collect_mq_info_in_item_avail(MQSection, Permissions), Avails,
        ImportedModules0, ImportedModules,
        UnusedIntModules0, UnusedIntModules),
    mq_info_set_imported_modules(ImportedModules, !Info),
    mq_info_set_as_yet_unused_interface_modules(UnusedIntModules, !Info),

    collect_mq_info_in_items(MQSection, Permissions, Items, !Info),
    collect_mq_info_in_item_blocks(SectionInfo, ItemBlocks, !Info).

    % For submodule definitions (whether nested or separate,
    % i.e. either `:- module foo.' or `:- include_module foo.'),
    % add the module id to the module_id_set.
    %
    % We don't actually handle nested submodules here. Nested submodules
    % were separated out and replaced with the internal representation of
    % the `:- include_module' declaration that would correspond to it
    % by the code that created the module's raw_compilation_unit, which
    % was later transformed into the aug_compilation_unit whose items
    % we process here.
    %
:- pred collect_mq_info_in_item_include(module_permissions::in,
    item_include::in, module_id_set::in, module_id_set::out) is det.

collect_mq_info_in_item_include(Permissions, Incl, !Modules) :-
    Incl = item_include(IncludedModuleName, _Context, _SeqNum),
    Arity = 0,
    id_set_insert(Permissions, mq_id(IncludedModuleName, Arity), !Modules).

    % For import declarations (`:- import_module' or `:- use_module'),
    % if we are currently in the interface section, then add the
    % imported modules to the as_yet_unused_interface_modules list.
    %
    % XXX ITEM_LIST Why do we base this decision on the status we get
    % from the mq_info, instead of directly on the current section's
    % section kind?
    %
:- pred collect_mq_info_in_item_avail(mq_section::in, module_permissions::in,
    item_avail::in, set(module_name)::in, set(module_name)::out,
    map(module_name, one_or_more(prog_context))::in,
    map(module_name, one_or_more(prog_context))::out) is det.

collect_mq_info_in_item_avail(MQSection, _Permissions, Avail,
        !ImportedModules, !UnusedIntModules) :-
    % Modules imported from the proper private interface of ancestors of
    % the current module are treated as if they were directly imported
    % by the current module.
    %
    % We check that all modules imported in the interface are used
    % in the interface.
    ( Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum))
    ; Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum))
    ),
    (
        ( MQSection = mq_section_local
        ; MQSection = mq_section_imported(
            import_locn_ancestor_private_interface_proper)
        ),
        set.insert(ModuleName, !ImportedModules)
    ;
        MQSection = mq_section_exported,
        set.insert(ModuleName, !ImportedModules),
        % Most of the time, ModuleName does not occur in !.UnusedIntModules.
        % We therefore try the insertion first, and only if the insertion
        % fails do we look up and update the existing entry (OldContexts)
        % that caused that failure.
        OnlyNewContexts = one_or_more(Context, []),
        ( if map.insert(ModuleName, OnlyNewContexts, !UnusedIntModules) then
            true
        else
            map.lookup(!.UnusedIntModules, ModuleName, OldContexts),
            OldContexts = one_or_more(OldHeadContext, OldTailContexts),
            NewContexts = one_or_more(Context,
                [OldHeadContext | OldTailContexts]),
            map.det_update(ModuleName, NewContexts, !UnusedIntModules)
        )
    ;
        ( MQSection = mq_section_imported(import_locn_interface)
        ; MQSection = mq_section_imported(import_locn_implementation)
        ; MQSection = mq_section_imported(import_locn_import_by_ancestor)
        ; MQSection = mq_section_abstract_imported
        )
    ).

    % Pass over the item list collecting all defined module, type, mode and
    % inst ids, all module synonym definitions, and the names of all
    % modules imported in the interface.
    %
:- pred collect_mq_info_in_items(mq_section::in, module_permissions::in,
    list(item)::in, mq_info::in, mq_info::out) is det.

collect_mq_info_in_items(_, _, [], !Info).
collect_mq_info_in_items(MQSection, Permissions, [Item | Items], !Info) :-
    collect_mq_info_in_item(MQSection, Permissions, Item, !Info),
    collect_mq_info_in_items(MQSection, Permissions, Items, !Info).

:- pred collect_mq_info_in_item(mq_section::in, module_permissions::in,
    item::in, mq_info::in, mq_info::out) is det.

collect_mq_info_in_item(MQSection, Permissions, Item, !Info) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        ItemTypeDefn = item_type_defn_info(SymName, Params, _, _, _, _),
        ( if MQSection = mq_section_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_types(!.Info, Types0),
            id_set_insert(Permissions, mq_id(SymName, Arity), Types0, Types),
            mq_info_set_types(Types, !Info)
        )
    ;
        Item = item_inst_defn(ItemInstDefn),
        ItemInstDefn = item_inst_defn_info(SymName, Params, _, _, _, _, _),
        ( if MQSection = mq_section_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_insts(!.Info, Insts0),
            id_set_insert(Permissions, mq_id(SymName, Arity), Insts0, Insts),
            mq_info_set_insts(Insts, !Info)
        )
    ;
        Item = item_mode_defn(ItemModeDefn),
        ItemModeDefn = item_mode_defn_info(SymName, Params, _, _, _, _),
        ( if MQSection = mq_section_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_modes(!.Info, Modes0),
            id_set_insert(Permissions, mq_id(SymName, Arity), Modes0, Modes),
            mq_info_set_modes(Modes, !Info)
        )
    ;
        Item = item_promise(ItemPromise),
        ItemPromise = item_promise_info(_PromiseType, Goal, _ProgVarSet,
            _UnivVars, _Context, _SeqNum),
        collect_used_modules_in_promise_goal(Goal,
            set.init, UsedModuleNames, no, FoundUnqual),
        (
            FoundUnqual = no,
            InInt = mq_section_to_in_interface(MQSection),
            set.fold(mq_info_set_module_used(InInt), UsedModuleNames, !Info)
        ;
            % Any unqualified symbol in the promise might come from *any* of
            % the imported modules. There is no way for us to tell which ones,
            % so we conservatively assume that it uses *all* of them.
            FoundUnqual = yes,
            map.init(UnusedModules),
            mq_info_set_as_yet_unused_interface_modules(UnusedModules, !Info)
        )
    ;
        Item = item_typeclass(ItemTypeClass),
        ItemTypeClass = item_typeclass_info(SymName, Params, _, _, _, _, _, _),
        ( if MQSection = mq_section_abstract_imported then
            % This item is not visible in the current module.
            true
        else
            list.length(Params, Arity),
            mq_info_get_classes(!.Info, Classes0),
            id_set_insert(Permissions, mq_id(SymName, Arity),
                Classes0, Classes),
            mq_info_set_classes(Classes, !Info)
        )
    ;
        Item = item_instance(ItemInstance),
        ( if MQSection = mq_section_imported(_) then
            InstanceModule = ItemInstance ^ ci_module_containing_instance,
            mq_info_get_imported_instance_modules(!.Info,
                ImportedInstanceModules0),
            set.insert(InstanceModule,
                ImportedInstanceModules0, ImportedInstanceModules),
            mq_info_set_imported_instance_modules(ImportedInstanceModules,
                !Info)
        else
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
        ; Item = item_foreign_import_module(_)
        ; Item = item_type_repn(_)
        )
        % Do nothing.
    ).

:- func mq_section_to_in_interface(mq_section) = mq_in_interface.

mq_section_to_in_interface(mq_section_exported) = mq_used_in_interface.
mq_section_to_in_interface(mq_section_local) = mq_not_used_in_interface.
mq_section_to_in_interface(mq_section_imported(_)) = mq_not_used_in_interface.
mq_section_to_in_interface(mq_section_abstract_imported) =
    mq_not_used_in_interface.

%---------------------%

    % Scan Goal. Add the set of module names found in the qualified symbols
    % in Goal to !UsedModuleNames. If there exists a single unqualified symbol,
    % in Goal, set !Success to no.
    %
:- pred collect_used_modules_in_promise_goal(goal::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_promise_goal(Goal, !UsedModuleNames, !FoundUnqual) :-
    (
        ( Goal = conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = par_conj_expr(_, SubGoalA, SubGoalB)
        ; Goal = disj_expr(_, SubGoalA, SubGoalB)
        ; Goal = implies_expr(_, SubGoalA, SubGoalB)
        ; Goal = equivalent_expr(_, SubGoalA, SubGoalB)
        ),
        collect_used_modules_in_promise_goal(SubGoalA,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(SubGoalB,
            !UsedModuleNames, !FoundUnqual)
    ;
        ( Goal = true_expr(_)
        ; Goal = fail_expr(_)
        )
    ;
        ( Goal = not_expr(_, SubGoal)
        ; Goal = quant_expr(_, _, _, _, SubGoal)
        ; Goal = promise_purity_expr(_, _, SubGoal)
        ; Goal = promise_equivalent_solutions_expr(_, _, _, _, _, SubGoal)
        ; Goal = promise_equivalent_solution_sets_expr(_, _, _, _, _, SubGoal)
        ; Goal = promise_equivalent_solution_arbitrary_expr(_, _, _, _, _,
            SubGoal)
        ; Goal = require_detism_expr(_, _, SubGoal)
        ; Goal = require_complete_switch_expr(_, _, SubGoal)
        ; Goal = require_switch_arms_detism_expr(_, _, _, SubGoal)
        ; Goal = trace_expr(_, _, _, _, _, SubGoal)
        ; Goal = disable_warnings_expr(_, _, _, SubGoal)
        ),
        collect_used_modules_in_promise_goal(SubGoal,
            !UsedModuleNames, !FoundUnqual)
    ;
        Goal = try_expr(_, _, SubGoal, ThenGoal, MaybeElseGoal, Catches,
            MaybeCatchAny),
        collect_used_modules_in_promise_goal(SubGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(ThenGoal,
            !UsedModuleNames, !FoundUnqual),
        (
            MaybeElseGoal = no
        ;
            MaybeElseGoal = yes(ElseGoal),
            collect_used_modules_in_promise_goal(ElseGoal,
                !UsedModuleNames, !FoundUnqual)
        ),
        list.foldl2(collect_used_modules_in_promise_catch, Catches,
            !UsedModuleNames, !FoundUnqual),
        (
            MaybeCatchAny = no
        ;
            MaybeCatchAny = yes(catch_any_expr(_, CatchAnyGoal)),
            collect_used_modules_in_promise_goal(CatchAnyGoal,
                !UsedModuleNames, !FoundUnqual)
        )
    ;
        Goal = atomic_expr(_, _, _, _, MainGoal, OrElseGoals),
        collect_used_modules_in_promise_goal(MainGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goals(OrElseGoals,
            !UsedModuleNames, !FoundUnqual)
    ;
        Goal = if_then_else_expr(_, _, _, CondGoal, ThenGoal, ElseGoal),
        collect_used_modules_in_promise_goal(CondGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(ThenGoal,
            !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_promise_goal(ElseGoal,
            !UsedModuleNames, !FoundUnqual)
    ;
        Goal = event_expr(_, _Name, ArgTerms0),
        list.map(term.coerce, ArgTerms0, ArgTerms),
        collect_used_modules_in_terms(ArgTerms, !UsedModuleNames, !FoundUnqual)
    ;
        Goal = call_expr(_, SymName, ArgTerms0, _Purity),
        (
            SymName = qualified(ModuleName, _),
            set.insert(ModuleName, !UsedModuleNames)
        ;
            SymName = unqualified(_),
            !:FoundUnqual = yes
        ),
        list.map(term.coerce, ArgTerms0, ArgTerms),
        collect_used_modules_in_terms(ArgTerms, !UsedModuleNames, !FoundUnqual)
    ;
        Goal = unify_expr(_, LHS0, RHS0, _Purity),
        term.coerce(LHS0, LHS),
        term.coerce(RHS0, RHS),
        collect_used_modules_in_term(LHS, !UsedModuleNames, !FoundUnqual),
        collect_used_modules_in_term(RHS, !UsedModuleNames, !FoundUnqual)
    ).

    % Performs collect_used_modules_in_promise_goal on a list of goals.
    %
:- pred collect_used_modules_in_promise_goals(list(goal)::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_promise_goals([], !UsedModuleNames, !FoundUnqual).
collect_used_modules_in_promise_goals([Goal | Goals],
        !UsedModuleNames, !FoundUnqual) :-
    collect_used_modules_in_promise_goal(Goal,
        !UsedModuleNames, !FoundUnqual),
    collect_used_modules_in_promise_goals(Goals,
        !UsedModuleNames, !FoundUnqual).

:- pred collect_used_modules_in_promise_catch(catch_expr::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_promise_catch(CatchExpr,
        !UsedModuleNames, !FoundUnqual) :-
    CatchExpr = catch_expr(Pattern0, Goal),
    term.coerce(Pattern0, Pattern),
    collect_used_modules_in_term(Pattern, !UsedModuleNames, !FoundUnqual),
    collect_used_modules_in_promise_goal(Goal, !UsedModuleNames, !FoundUnqual).

    % Add all the module names in qualified sym_names in Term to
    % !UsedModuleNames, and set !FoundUnqual to true if any of the sym_names
    % in Term is unqualified.
    %
:- pred collect_used_modules_in_term(term::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_term(Term, !UsedModuleNames, !FoundUnqual) :-
    ( if try_parse_sym_name_and_args(Term, SymName, ArgTerms) then
        (
            SymName = qualified(ModuleName, _),
            set.insert(ModuleName, !UsedModuleNames)
        ;
            SymName = unqualified(_),
            !:FoundUnqual = yes
        ),
        collect_used_modules_in_terms(ArgTerms, !UsedModuleNames, !FoundUnqual)
    else
        true
    ).

:- pred collect_used_modules_in_terms(list(term)::in,
    set(module_name)::in, set(module_name)::out, bool::in, bool::out) is det.

collect_used_modules_in_terms([], !UsedModuleNames, !FoundUnqual).
collect_used_modules_in_terms([Term | Terms],
        !UsedModuleNames, !FoundUnqual) :-
    collect_used_modules_in_term(Term, !UsedModuleNames, !FoundUnqual),
    collect_used_modules_in_terms(Terms, !UsedModuleNames, !FoundUnqual).

%---------------------------------------------------------------------------%
:- end_module parse_tree.module_qual.collect_mq_info.
%---------------------------------------------------------------------------%
